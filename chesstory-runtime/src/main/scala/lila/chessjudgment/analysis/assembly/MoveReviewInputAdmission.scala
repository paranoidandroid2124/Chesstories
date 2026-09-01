package lila.chessjudgment.analysis.assembly

import chess.Color
import chess.variant.Standard
import lila.chessjudgment.model.line.{
  AdmittedCandidateLine,
  AutomaticTerminal,
  CandidateLineEvaluation,
  CanonicalPositionHistory,
  DrawClaimAction,
  DrawClaimAvailability,
  DrawClaimRule,
  FivefoldRepetitionKnowledge,
  PositionRuleAssessment,
  PrincipalVariationEvidence,
  ThreefoldClaimKnowledge
}
import lila.chessjudgment.model.line.EngineLine
import lila.chessjudgment.model.judgment.{
  EvidenceRef,
  CanonicalLineReplay,
  AdmittedRootRankingPair,
  LineNodeRole,
  AdmittedReviewLine,
  AdmittedMoveReviewInput,
  PlayerFacingPositionAction
}

/** Development-only input; player-use jobs enter through the focused v6 preparation path. */
final case class RawMoveReviewInput(
    fen: String,
    playedMoveUci: String,
    variations: List[EngineLine],
    ply: Option[Int] = None,
    movePrefixUci: List[String] = Nil
)

/** The v6 player path owns one reviewed move and one engine-ranked reference. */
final class PreparedFocusedMoveReview private (
    val legalMoveIndex: Int,
    val playedMoveUci: String,
    val bestMoveUci: String,
    val playedRootRank: Option[Int],
    val preparedInput: AdmittedMoveReviewInput,
    val drawClaims: List[DrawClaimAction]
)

object PreparedFocusedMoveReview:
  private[assembly] def materialized(
      legalMoveIndex: Int,
      playedMoveUci: String,
      bestMoveUci: String,
      playedRootRank: Option[Int],
      preparedInput: AdmittedMoveReviewInput,
      drawClaims: List[DrawClaimAction]
  ): PreparedFocusedMoveReview =
    new PreparedFocusedMoveReview(
      legalMoveIndex,
      playedMoveUci,
      bestMoveUci,
      playedRootRank,
      preparedInput,
      drawClaims
    )

enum PreparedFocusedPositionCommentary:
  case PositionAction(action: PlayerFacingPositionAction)
  case MoveReview(review: PreparedFocusedMoveReview)

object MoveReviewInputAdmission:
  private final case class AdmittedEvaluation(
      evaluation: CandidateLineEvaluation,
      replay: CanonicalLineReplay,
      automaticTerminal: Option[AutomaticTerminal]
  )

  /**
   * Prepares the sole v6 player review. The unrestricted root bundle owns the
   * reference ranking; a focused comparison is admitted only when the played
   * move was outside that bundle.
   */
  def prepareFocusedMoveReview(
      positionHistory: CanonicalPositionHistory,
      playedMoveUci: String,
      resultingFen: String,
      rootRuleAssessment: PositionRuleAssessment,
      rootEvaluations: List[AdmittedCandidateLine],
      focusComparisonEvaluations: List[AdmittedCandidateLine]
  ): Option[PreparedFocusedPositionCommentary] =
    val rootPosition = positionHistory.currentPosition
    val legalMoves = positionHistory.currentLegalMoveUcis
    val playedMove = EvidenceRef.normalizeMove(playedMoveUci)
    val legalMoveIndex = legalMoves.indexOf(playedMove)
    val normalizedResultingFen = PrincipalVariationEvidence.normalizeFen(resultingFen)

    for
      _ <- Option.when(legalMoveIndex >= 0)(())
      _ <- Option.when(rootEvaluations.size == legalMoves.size.min(3))((): Unit)
      admittedRoot <- materializeAdmissions(positionHistory, rootEvaluations)
      rootMoves = admittedRoot.flatMap(_.evaluation.moves.headOption).map(EvidenceRef.normalizeMove)
      _ <- Option.when(
        rootMoves.size == admittedRoot.size &&
          rootMoves.distinct.size == rootMoves.size &&
          rootMoves.forall(legalMoves.contains) &&
          admittedRoot.forall(_.evaluation.comparisonReady)
      )((): Unit)
      rankedRoot = admittedRoot.zipWithIndex.sortBy { case (admitted, sourceIndex) =>
        (-admitted.evaluation.rankingScoreForMover(rootPosition.color), sourceIndex)
      }.map(_._1)
      best <- rankedRoot.headOption
      bestEvaluation = best.evaluation
      bestMove <- bestEvaluation.moves.headOption.map(EvidenceRef.normalizeMove)
      playedInRoot = rankedRoot.find(_.evaluation.moves.headOption.exists(EvidenceRef.sameMove(_, playedMove)))
      admittedComparison <- materializeAdmissions(positionHistory, focusComparisonEvaluations)
      _ <- Option.when(
        playedInRoot.nonEmpty == admittedComparison.isEmpty
      )((): Unit)
      comparisonMoves = admittedComparison.flatMap(_.evaluation.moves.headOption).map(EvidenceRef.normalizeMove)
      _ <- Option.when(
        admittedComparison.isEmpty ||
          (admittedComparison.size == 2 &&
            comparisonMoves.distinct.size == 2 &&
            comparisonMoves.toSet == Set(bestMove, playedMove) &&
            admittedComparison.forall(_.evaluation.comparisonReady))
      )((): Unit)
      focusedBest = admittedComparison.find(_.evaluation.moves.headOption.exists(EvidenceRef.sameMove(_, bestMove)))
      _ <- Option.when(
        admittedComparison.isEmpty ||
          focusedBest.exists(value => sameReferenceEvaluationPoint(best.evaluation, value.evaluation))
      )((): Unit)
      playedAdmission <- playedInRoot.orElse(
        admittedComparison.find(_.evaluation.moves.headOption.exists(EvidenceRef.sameMove(_, playedMove)))
      )
      playedRootReplay <- playedAdmission.replay.replaySteps.headOption
        .flatMap(step => playedAdmission.replay.subset(List(step)))
      playedHistory <- positionHistory.extendAdmitted(playedRootReplay.legalSteps).toOption
      _ <- Option.when(playedHistory.currentFen == normalizedResultingFen)(())
      drawClaims = focusedDrawClaims(playedMove, rootRuleAssessment, playedHistory)
      result <-
        if drawClaims.nonEmpty && bestEvaluation.winPercentForMover(rootPosition.color) < 50.0 then
          Some(
            PreparedFocusedPositionCommentary.PositionAction(
              PlayerFacingPositionAction.DominantDrawClaim(drawClaims)
            )
          )
        else if legalMoves.size == 1 then
          Some(
            PreparedFocusedPositionCommentary.PositionAction(
              PlayerFacingPositionAction.ForcedSingleMove(
                playedMove,
                rootPosition.color,
                bestEvaluation,
                drawClaims
              )
            )
          )
        else
          val reviewEvaluations =
            if admittedComparison.nonEmpty then
              val comparisonByMove = admittedComparison.map(value =>
                EvidenceRef.normalizeMove(value.evaluation.moves.head) -> value
              ).toMap
              // Root owns the reference evaluation and trajectory. The focused
              // best line has already certified the same evaluation point, so
              // only its newly covered played line is consumed here.
              List(best, comparisonByMove(playedMove)) ++
                rankedRoot.filterNot(value =>
                  Set(bestMove, playedMove)(EvidenceRef.normalizeMove(value.evaluation.moves.head))
                )
            else rankedRoot
          for
            prepared <- assembleAdmittedReview(
              positionHistory,
              playedMove,
              reviewEvaluations
            )
            referenceMove <- prepared.referenceLine.flatMap(_.rootMove)
            _ <- Option.when(EvidenceRef.sameMove(referenceMove, bestMove))((): Unit)
            admittedRootRanking = rankedRoot.map(admitted =>
              EvidenceRef.normalizeMove(admitted.evaluation.moves.head)
            )
            admittedRootRankingPair <- AdmittedRootRankingPair.fromAdmittedRanking(admittedRootRanking)
          yield
            PreparedFocusedPositionCommentary.MoveReview(
              PreparedFocusedMoveReview.materialized(
                legalMoveIndex,
                playedMove,
                bestMove,
                rankedRoot.indexWhere(_.evaluation.moves.headOption.exists(EvidenceRef.sameMove(_, playedMove))) match
                  case -1    => None
                  case index => Some(index + 1),
                prepared.copy(admittedRootRankingPair = Some(admittedRootRankingPair)),
                drawClaims
              )
            )
    yield result

  def admit(raw: RawMoveReviewInput): Option[AdmittedMoveReviewInput] =
    val beforeFen = PrincipalVariationEvidence.normalizeFen(raw.fen)
    val playedMove = EvidenceRef.normalizeMove(raw.playedMoveUci)
    val movePrefix = raw.movePrefixUci.map(EvidenceRef.normalizeMove).filter(_.nonEmpty)
    val positionHistory =
      if movePrefix.nonEmpty then CanonicalPositionHistory.from(Standard.initialFen.value, movePrefix, beforeFen)
      else CanonicalPositionHistory.from(beforeFen, Nil, beforeFen)
    for
      history <- positionHistory.toOption
      _ <- Option.when(raw.ply.forall(_ == history.currentPly))((): Unit)
      input <- admitPrepared(
        history,
        playedMove,
        raw.variations.map(line => CandidateLineEvaluation.EngineSearch(line))
      )
    yield input

  private def admitPrepared(
      history: CanonicalPositionHistory,
      playedMove: String,
      variations: List[CandidateLineEvaluation]
  ): Option[AdmittedMoveReviewInput] =
    admitEvaluations(history, variations).flatMap { admitted =>
      assembleAdmittedReview(
        history,
        playedMove,
        admitted
      )
    }

  private def assembleAdmittedReview(
      history: CanonicalPositionHistory,
      playedMove: String,
      variations: List[AdmittedEvaluation]
  ): Option[AdmittedMoveReviewInput] =
      val canonicalBeforeFen = history.currentFen
      val beforePly = history.currentPly
      val side = Some(history.currentPosition.color)
      val ranked = preferredRankedEvaluations(
        variations,
        side
      ).zipWithIndex
      val reference = ranked.headOption.map { case (admitted, index) =>
        AdmittedReviewLine(
          LineNodeRole.BestReference,
          index + 1,
          admitted.evaluation,
          admitted.replay
        )
      }
      val playedAdmission =
        ranked
          .find { case (admitted, _) =>
            admitted.evaluation.moves.headOption.exists(move => EvidenceRef.sameMove(move, playedMove))
          }
          .filter { case (admitted, _) =>
            admitted.evaluation.moves.size >= 2 || admitted.automaticTerminal.nonEmpty
          }
      val played =
        playedAdmission
          .filterNot { case (admitted, _) =>
            reference.flatMap(_.rootMove).exists(referenceMove =>
              admitted.evaluation.moves.headOption.exists(EvidenceRef.sameMove(referenceMove, _))
            )
          }
          .map { case (admitted, index) =>
            AdmittedReviewLine(
              LineNodeRole.Played,
              index + 1,
              admitted.evaluation,
              admitted.replay
            )
          }
      val alternatives =
        ranked
          .filterNot { case (_, index) =>
            reference.exists(_.rank == index + 1) || playedAdmission.exists(_._2 == index)
          }
          .map { case (admitted, index) =>
            AdmittedReviewLine(
              LineNodeRole.Alternative,
              index + 1,
              admitted.evaluation,
              admitted.replay
            )
          }
      val lines = reference.toList ++ played.toList ++ alternatives
      Option
        .when(lines.map(line => line.role -> line.rank).distinct.size == lines.size)(lines)
        .flatMap { exactLines =>
          playedAdmission.flatMap { case (admittedPlayed, _) =>
            reference.flatMap { _ =>
              for
                graphPlayedMove <- admittedPlayed.evaluation.moves.headOption.map(EvidenceRef.normalizeMove)
                graphAfterPlayed <- admittedPlayed.replay.replaySteps.headOption.map(_.fenAfter)
                afterReference = reference
                  .filterNot(_.rootMove.exists(EvidenceRef.sameMove(_, graphPlayedMove)))
                  .flatMap(_.replay.replaySteps.headOption)
                  .map(_.fenAfter)
              yield AdmittedMoveReviewInput(
                beforeFen = canonicalBeforeFen,
                playedMoveUci = graphPlayedMove,
                beforePly = beforePly,
                sideToMove = side,
                afterPlayedFen = graphAfterPlayed,
                afterReferenceFen = afterReference,
                lines = exactLines,
                admittedRootRankingPair = None,
                positionHistory = history
              )
            }
          }
        }

  private def admitEvaluations(
      history: CanonicalPositionHistory,
      evaluations: List[CandidateLineEvaluation]
  ): Option[List[AdmittedEvaluation]] =
    val admitted = evaluations.map(admitEvaluation(history, _))
    Option.when(admitted.forall(_.nonEmpty))(admitted.flatten)

  private def materializeAdmissions(
      history: CanonicalPositionHistory,
      admissions: List[AdmittedCandidateLine]
  ): Option[List[AdmittedEvaluation]] =
    val materialized = admissions.map(materializeAdmission(history, _))
    Option.when(materialized.forall(_.nonEmpty))(materialized.flatten)

  private def admitEvaluation(
      history: CanonicalPositionHistory,
      rawEvaluation: CandidateLineEvaluation,
      predecessorReplay: Option[CanonicalLineReplay] = None
  ): Option[AdmittedEvaluation] =
    val normalized = normalizedEvaluation(rawEvaluation)
    for
      _ <- Option.when(normalized.moves.nonEmpty)(())
      continuation <- history.admitToFirstAutomaticTerminal(normalized.moves).toOption
      evaluation <- normalized match
        case CandidateLineEvaluation.EngineSearch(line) =>
          Option.when(engineScoreConsistent(line, continuation.automaticTerminal))(
            continuation.automaticTerminal
              .map(CandidateLineEvaluation.ExactAutomaticTerminal(continuation.moves, _))
              .getOrElse(normalized)
          )
        case exact @ CandidateLineEvaluation.ExactAutomaticTerminal(moves, terminal) =>
          Option.when(
            continuation.moves == moves.map(EvidenceRef.normalizeMove) &&
              continuation.automaticTerminal.contains(terminal)
          )(exact)
      admission <- continuation.bind(evaluation)
      materialized <- materializeAdmission(history, admission, predecessorReplay)
    yield materialized

  private def materializeAdmission(
      history: CanonicalPositionHistory,
      admission: AdmittedCandidateLine,
      predecessorReplay: Option[CanonicalLineReplay] = None
  ): Option[AdmittedEvaluation] =
    for
      steps <- admission.continuationAfter(history)
      replay <- predecessorReplay match
        case Some(predecessor) => CanonicalLineReplay.continueFromHistory(predecessor, steps)
        case None              => CanonicalLineReplay.fromHistory(steps)
    yield AdmittedEvaluation(admission.evaluation, replay, admission.automaticTerminal)

  private def focusedDrawClaims(
      playedMove: String,
      rootRuleAssessment: PositionRuleAssessment,
      playedHistory: CanonicalPositionHistory
  ): List[DrawClaimAction] =
    val declaredClaims =
      PositionRuleAssessment.declaredDrawClaims(
        playedMove,
        rootRuleAssessment,
        PositionRuleAssessment.assess(playedHistory)
      )
    rootRuleAssessment match
      case PositionRuleAssessment.Nonterminal(
            FivefoldRepetitionKnowledge.CertifiedNonterminal,
            ThreefoldClaimKnowledge.Known(threefold),
            fiftyMove
          ) =>
        List(
          Option.when(threefold == DrawClaimAvailability.AvailableNow)(
            DrawClaimAction.AvailableNow(DrawClaimRule.ThreefoldRepetition)
          ),
          Option.when(fiftyMove == DrawClaimAvailability.AvailableNow)(
            DrawClaimAction.AvailableNow(DrawClaimRule.FiftyMoveRule)
          ),
          Option
            .when(
              threefold != DrawClaimAvailability.AvailableNow &&
                declaredClaims.exists(_.rule == DrawClaimRule.ThreefoldRepetition)
            )(
              DrawClaimAction.AvailableByDeclaredMoves(
                DrawClaimRule.ThreefoldRepetition,
                Set(playedMove)
              )
            ),
          Option
            .when(
              fiftyMove != DrawClaimAvailability.AvailableNow &&
                declaredClaims.exists(_.rule == DrawClaimRule.FiftyMoveRule)
            )(
              DrawClaimAction.AvailableByDeclaredMoves(
                DrawClaimRule.FiftyMoveRule,
                Set(playedMove)
              )
            )
        ).flatten
      case _ => Nil

  private def normalizedLine(line: EngineLine): EngineLine =
    line.copy(moves = line.moves.map(EvidenceRef.normalizeMove))

  private def normalizedEvaluation(evaluation: CandidateLineEvaluation): CandidateLineEvaluation =
    evaluation match
      case CandidateLineEvaluation.EngineSearch(line) =>
        CandidateLineEvaluation.EngineSearch(normalizedLine(line))
      case CandidateLineEvaluation.ExactAutomaticTerminal(moves, terminal) =>
        CandidateLineEvaluation.ExactAutomaticTerminal(moves.map(EvidenceRef.normalizeMove), terminal)

  /** The focused best result is a comparison calibration, not a second
    * reference owner. Different continuations are harmless only when they
    * certify the exact same score/mate or automatic-terminal outcome.
    */
  private def sameReferenceEvaluationPoint(
      root: CandidateLineEvaluation,
      focused: CandidateLineEvaluation
  ): Boolean =
    (root, focused) match
      case (CandidateLineEvaluation.EngineSearch(rootLine), CandidateLineEvaluation.EngineSearch(focusedLine)) =>
        rootLine.scoreCp == focusedLine.scoreCp && rootLine.mate == focusedLine.mate
      case (
            CandidateLineEvaluation.ExactAutomaticTerminal(_, rootTerminal),
            CandidateLineEvaluation.ExactAutomaticTerminal(_, focusedTerminal)
          ) =>
        rootTerminal == focusedTerminal
      case _ => false

  private def preferredRankedEvaluations(
      evaluations: List[AdmittedEvaluation],
      sideToMove: Option[Color]
  ): List[AdmittedEvaluation] =
    val byRootMove = evaluations.zipWithIndex
      .groupBy { case (admitted, _) =>
        admitted.evaluation.moves.headOption.map(EvidenceRef.normalizeMove).getOrElse("")
      }
    byRootMove.foreach { case (rootMove, entries) =>
      require(
        entries.size == 1,
        s"one admitted root move must have one evaluation owner: $rootMove"
      )
    }
    val selectedEvaluations = byRootMove
      .values
      .map(_.head)
      .toList
    val ordered = sideToMove match
      case Some(side) =>
        selectedEvaluations.sortBy { case (admitted, originalRank) =>
          (-admitted.evaluation.rankingScoreForMover(side), originalRank)
        }
      case None => selectedEvaluations.sortBy(_._2)
    ordered.map(_._1)

  private def automaticTerminal(history: CanonicalPositionHistory): Option[AutomaticTerminal] =
    PositionRuleAssessment.assess(history) match
      case PositionRuleAssessment.Terminal(terminal) => Some(terminal)
      case PositionRuleAssessment.Nonterminal(_, _, _) => None

  private def engineScoreConsistent(
      line: EngineLine,
      terminal: Option[AutomaticTerminal]
  ): Boolean =
    if line.mate.contains(0) then false
    else terminal match
      case Some(AutomaticTerminal.Checkmate(winner)) =>
        line.mate.exists(mate => (mate > 0) == winner.white)
      case Some(
            AutomaticTerminal.Stalemate |
            AutomaticTerminal.InsufficientMaterial |
            AutomaticTerminal.FivefoldRepetition |
            AutomaticTerminal.SeventyFiveMoveRule
          ) =>
        line.mate.isEmpty && line.scoreCp.abs <= 1
      case None => true
