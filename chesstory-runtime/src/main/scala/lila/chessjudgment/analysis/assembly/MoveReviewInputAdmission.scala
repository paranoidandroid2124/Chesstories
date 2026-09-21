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
  AdmittedLegalLine,
  AdmittedAssessmentCandidate,
  AdmittedMoveReviewInput,
  AdmittedEvaluationObservation,
  EvaluationSource,
  PlayerFacingPositionAction
}
import lila.chessjudgment.model.judgment.{ SuppliedCommentaryLine, CommentaryLineAdmission, CommentaryLineStop }

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
  /** Legal observations do not require an engine score or a comparison. */
  def admitIndependent(
      history: CanonicalPositionHistory,
      playedMoveUci: String,
      lines: List[List[String]] = Nil,
      suppliedLines: List[SuppliedCommentaryLine] = Nil
  ): Option[AdmittedMoveReviewInput] =
    val played = EvidenceRef.normalizeMove(playedMoveUci)
    def replay(moves: List[String]): Option[AdmittedLegalLine] =
      history.admitToFirstAutomaticTerminal(moves).toOption
        .flatMap(continuation => CanonicalLineReplay.fromHistory(
          continuation.resultingHistory.segmentReplaySteps.drop(history.segmentReplaySteps.size)
        ))
        .map(replay => AdmittedLegalLine(replay))
    replay(List(played)).map { root =>
      // Keys are exact historical move prefixes, not board positions. Each new
      // edge is legally replayed once across all supplied nested alternatives.
      val prefixes = scala.collection.mutable.Map(List.empty[String] -> (history, false))
      def admitPrefix(inputMoves: List[String], maximumPrefixes: Int = 600): (List[String], Option[CommentaryLineStop]) =
        var moves = List.empty[String]
        var stop = Option.empty[CommentaryLineStop]
        val iterator = inputMoves.iterator
        while iterator.hasNext && stop.isEmpty do
          val move = iterator.next()
          val (before, terminal) = prefixes(moves)
          if terminal then stop = Some(CommentaryLineStop.AutomaticTerminal)
          else
            val next = moves :+ move
            if !prefixes.contains(next) && prefixes.size > maximumPrefixes then
              stop = Some(CommentaryLineStop.InputLimit)
            else prefixes.get(next).orElse {
              before.admitToFirstAutomaticTerminal(List(move)).toOption.map { continuation =>
                val value = continuation.resultingHistory -> continuation.automaticTerminal.nonEmpty
                prefixes.update(next, value)
                value
              }
            } match
              case Some(_) => moves = next
              case None => stop = Some(CommentaryLineStop.InvalidContinuation)
        moves -> stop
      val admitted = suppliedLines.map { line =>
        val (moves, stop) = admitPrefix(line.moves)
        CommentaryLineAdmission(line, moves.size, stop)
      }
      val acceptedMoves = admitted.map(_.acceptedMoves) ++ lines.map(moves =>
        // Direct variations already have their own wire limits.
        admitPrefix(moves.map(EvidenceRef.normalizeMove), Int.MaxValue)._1)
      val acceptedReplays = acceptedMoves.filter(_.nonEmpty).distinct.flatMap { moves =>
        CanonicalLineReplay.fromHistory(prefixes(moves)._1.segmentReplaySteps.drop(history.segmentReplaySteps.size))
          .map(replay => AdmittedLegalLine(replay))
      }
      AdmittedMoveReviewInput(played, (root :: acceptedReplays).distinctBy(_.moves),
        Nil, None, history, suppliedLines = admitted)
    }

  /** Retains physical replay when the separately reported score cannot bind to it. */
  def retainLegalContinuations(
      input: AdmittedMoveReviewInput,
      continuations: List[lila.chessjudgment.model.line.CanonicalHistoryContinuation]
  ): AdmittedMoveReviewInput =
    val history = input.positionHistory
    // A child search already owns the entire exact history. Retain that legal
    // path through the same owner check, without replaying it or importing its score.
    val replays = continuations.flatMap(_.stepsAfter(history))
      .flatMap(CanonicalLineReplay.fromHistory).map(replay => AdmittedLegalLine(replay))
    input.copy(legalLines = AdmittedLegalLine.merge(input.legalLines ++ replays))

  /** Promote only requested probe paths. Their replay calculations retain the exact
    * history owner, while their evidence remains excluded from independent plan discovery.
    */
  def retainProbePaths(input: AdmittedMoveReviewInput,
      paths: List[OccurrenceResources.ProbePath], discoveryOnly: Boolean = true): Option[AdmittedMoveReviewInput] =
    Option.when(paths.forall(path => (path.rootHistory eq input.positionHistory) &&
      path.history.stepsAfter(input.positionHistory).nonEmpty)) {
      val promoted = paths.map(path => AdmittedLegalLine(path.replay).copy(discoveryOnly = discoveryOnly))
      input.copy(legalLines = AdmittedLegalLine.merge(input.legalLines ++ promoted))
    }

  def retainAdmittedLines(
      input: AdmittedMoveReviewInput,
      lines: List[AdmittedCandidateLine],
      source: EvaluationSource
  ): AdmittedMoveReviewInput =
    require(source != EvaluationSource.ResponseComparison, "conditional child scores cannot rank root moves")
    val admitted = lines.flatMap(materializeAdmission(input.positionHistory, _))
    input.copy(
      legalLines = AdmittedLegalLine.merge(input.legalLines ++ admitted.map(value => AdmittedLegalLine(value.replay))),
      evaluationObservations = (input.evaluationObservations ++ admitted.map(value =>
        AdmittedEvaluationObservation(value.evaluation, source))).distinct
    )

  private final case class AdmittedEvaluation(
      evaluation: CandidateLineEvaluation,
      replay: CanonicalLineReplay,
      automaticTerminal: Option[AutomaticTerminal]
  )

  /** Prepares the sole v6 player review. The root-search bundle owns the
    * assessment ranking; a focused comparison is admitted only when the
    * played move was outside that bundle. Every unique replay actually
    * produced by those two reports remains available as a LegalLine, while
    * the complete legal root move universe remains independently available.
    */
  def prepareFocusedMoveReview(
      positionHistory: CanonicalPositionHistory,
      playedMoveUci: String,
      resultingFen: String,
      rootRuleAssessment: PositionRuleAssessment,
      rootEvaluations: List[AdmittedCandidateLine],
      focusComparisonEvaluations: List[AdmittedCandidateLine],
      rankingSource: EvaluationSource = EvaluationSource.RootSearch
  ): Option[PreparedFocusedPositionCommentary] =
    val rootPosition = positionHistory.currentPosition
    val legalMoves = positionHistory.currentLegalMoveUcis
    val playedMove = EvidenceRef.normalizeMove(playedMoveUci)
    val legalMoveIndex = legalMoves.indexOf(playedMove)
    val normalizedResultingFen = PrincipalVariationEvidence.normalizeFen(resultingFen)

    for
      _ <- Option.when(rankingSource != EvaluationSource.ResponseComparison)(())
      _ <- Option.when(legalMoveIndex >= 0)(())
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
      playedAdmission <- playedInRoot.orElse(
        admittedComparison.find(_.evaluation.moves.headOption.exists(EvidenceRef.sameMove(_, playedMove)))
      )
      playedRootReplay <- playedAdmission.replay.replaySteps.headOption
        .flatMap(step => playedAdmission.replay.subset(List(step)))
      // Runtime reports reconstruct the exact history independently. Their
      // legal steps need not share scalachess Position equality with this owner.
      // Extend this owner and require the admitted PV head's exact result too.
      playedHistory <- positionHistory.extend(List(playedMove)).toOption
      _ <- Option.when(
        playedHistory.currentFen == playedRootReplay.replaySteps.head.fenAfter &&
          playedHistory.currentFen == normalizedResultingFen
      )(())
      (legalLineEvaluations, assessmentEvaluations) =
        if admittedComparison.nonEmpty then
          val comparisonByMove = admittedComparison.map(value =>
            EvidenceRef.normalizeMove(value.evaluation.moves.head) -> value
          ).toMap
          val focusedPlayed = comparisonByMove(playedMove)
          val focusedBest = comparisonByMove(bestMove)
          val rootWithoutBest = rankedRoot.filterNot(value =>
            EvidenceRef.sameMove(value.evaluation.moves.head, bestMove)
          )
          // Preserve both reports, but use the focused observations for the
          // two repeated moves in both assessment and draw-claim selection.
          (rankedRoot ++ admittedComparison, List(focusedBest, focusedPlayed) ++ rootWithoutBest)
        else (rankedRoot, rankedRoot)
      bestAvailableEvaluation = assessmentEvaluations
        .maxBy(_.evaluation.rankingScoreForMover(rootPosition.color)).evaluation
      drawClaims = focusedDrawClaims(playedMove, rootRuleAssessment, playedHistory)
      result <-
        // The investigation issuer compares a restricted pair. Its losing scores
        // cannot make claiming a draw preferable to unexamined root choices.
        if rankingSource != EvaluationSource.InvestigationComparison && drawClaims.nonEmpty &&
            bestAvailableEvaluation.winPercentForMover(rootPosition.color) < 50.0 then
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
          for
            prepared <- assembleAdmittedReview(
              positionHistory,
              playedMove,
              legalLineEvaluations,
              assessmentEvaluations,
              rankedRoot.map(value => AdmittedEvaluationObservation(value.evaluation, rankingSource)) ++
                admittedComparison.map(value => AdmittedEvaluationObservation(value.evaluation, EvaluationSource.FocusComparison)),
              preferredReference = Some(bestMove)
            )
            referenceMove <- prepared.referenceCandidate.flatMap(_.rootMove)
            _ <- Option.when(EvidenceRef.sameMove(referenceMove, bestMove))((): Unit)
            admittedRootRanking = rankedRoot.map(admitted =>
              EvidenceRef.normalizeMove(admitted.evaluation.moves.head)
            )
            admittedRootRankingPair <-
              if EvidenceRef.sameMove(bestMove, playedMove) then
                AdmittedRootRankingPair.fromAdmittedRanking(admittedRootRanking).map(Some(_))
              else Some(None)
          yield
            PreparedFocusedPositionCommentary.MoveReview(
              PreparedFocusedMoveReview.materialized(
                legalMoveIndex,
                playedMove,
                bestMove,
                rankedRoot.indexWhere(_.evaluation.moves.headOption.exists(EvidenceRef.sameMove(_, playedMove))) match
                  case -1    => None
                  case index => Some(index + 1),
                prepared.copy(admittedRootRankingPair = admittedRootRankingPair),
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
    val evaluations = variations.map(admitEvaluation(history, _))
    lazy val independent = admitIndependent(history, playedMove, variations.map(_.moves)).map(input =>
      input.copy(evaluationObservations = evaluations.flatten.map(value =>
        AdmittedEvaluationObservation(value.evaluation, EvaluationSource.DirectInput))))
    Option.when(evaluations.forall(_.nonEmpty))(evaluations.flatten).flatMap { admitted =>
      assembleAdmittedReview(
        history,
        playedMove,
        admitted,
        admitted,
        admitted.map(value => AdmittedEvaluationObservation(value.evaluation, EvaluationSource.DirectInput))
      )
    }.orElse(independent)

  private def assembleAdmittedReview(
      history: CanonicalPositionHistory,
      playedMove: String,
      legalLineEvaluations: List[AdmittedEvaluation],
      assessmentEvaluations: List[AdmittedEvaluation],
      observations: List[AdmittedEvaluationObservation],
      preferredReference: Option[String] = None
  ): Option[AdmittedMoveReviewInput] =
    val admittedLineRootMoves = legalLineEvaluations
      .flatMap(_.evaluation.moves.headOption)
      .map(EvidenceRef.normalizeMove)
    val assessmentRootMoves = assessmentEvaluations
      .flatMap(_.evaluation.moves.headOption)
      .map(EvidenceRef.normalizeMove)
    val assessmentReplaysOwned = assessmentEvaluations.forall { assessment =>
      legalLineEvaluations.exists(_.replay.replaySteps == assessment.replay.replaySteps)
    }
    if admittedLineRootMoves.size != legalLineEvaluations.size ||
        assessmentRootMoves.size != assessmentEvaluations.size ||
        !assessmentReplaysOwned
    then None
    else
      val ranked = preferredRankedEvaluations(
        assessmentEvaluations,
        history.currentPosition.color
      ).zipWithIndex
      val reference = preferredReference
        .flatMap(move => ranked.find(_._1.evaluation.moves.headOption.exists(EvidenceRef.sameMove(_, move))))
        .orElse(ranked.headOption).map { case (admitted, index) =>
        AdmittedAssessmentCandidate(
          role = LineNodeRole.BestReference,
          rank = index + 1,
          evaluation = admitted.evaluation
        )
      }
      val playedAssessment =
        ranked
          .find { case (admitted, _) =>
            admitted.evaluation.moves.headOption.exists(move => EvidenceRef.sameMove(move, playedMove))
          }
      val played =
        playedAssessment
          .filterNot { case (admitted, _) =>
            reference.flatMap(_.rootMove).exists(referenceMove =>
              admitted.evaluation.moves.headOption.exists(EvidenceRef.sameMove(referenceMove, _))
            )
          }
          .map { case (admitted, index) =>
            AdmittedAssessmentCandidate(
              role = LineNodeRole.Played,
              rank = index + 1,
              evaluation = admitted.evaluation
            )
          }
      val alternatives =
        ranked
          .filterNot { case (_, index) =>
            reference.exists(_.rank == index + 1) || playedAssessment.exists(_._2 == index)
          }
          .map { case (admitted, index) =>
            AdmittedAssessmentCandidate(
              role = LineNodeRole.Alternative,
              rank = index + 1,
              evaluation = admitted.evaluation
            )
          }
      val candidates = reference.toList ++ played.toList ++ alternatives
      val legalLines = legalLineEvaluations.distinctBy(_.replay.replaySteps).map { admitted =>
        AdmittedLegalLine(admitted.replay)
      }
      legalLines
        .find(line => EvidenceRef.sameMove(line.rootMove, playedMove))
        .map { playedLine =>
          AdmittedMoveReviewInput(
            playedMoveUci = playedLine.rootMove,
            legalLines = legalLines,
            assessmentCandidates = candidates,
            admittedRootRankingPair = Option.when(EvidenceRef.sameMove(playedMove, reference.flatMap(_.rootMove).getOrElse("")))(
              AdmittedRootRankingPair.fromAdmittedRanking(ranked.map(_._1.evaluation.moves.head))
            ).flatten,
            positionHistory = history,
            evaluationObservations = observations
          )
        }

  private def materializeAdmissions(
      history: CanonicalPositionHistory,
      admissions: List[AdmittedCandidateLine]
  ): Option[List[AdmittedEvaluation]] =
    val materialized = admissions.map(materializeAdmission(history, _))
    Option.when(materialized.forall(_.nonEmpty))(materialized.flatten)

  private def admitEvaluation(
      history: CanonicalPositionHistory,
      rawEvaluation: CandidateLineEvaluation
  ): Option[AdmittedEvaluation] =
    val normalized = normalizedEvaluation(rawEvaluation)
    for
      _ <- Option.when(normalized.moves.nonEmpty)(())
      continuation <- history.admitToFirstAutomaticTerminal(normalized.moves).toOption
      evaluation <- normalized match
        case CandidateLineEvaluation.EngineSearch(line) =>
          CandidateLineEvaluation.fromEngineLine(
            line.copy(moves = continuation.moves), continuation.automaticTerminal)
        case exact @ CandidateLineEvaluation.ExactAutomaticTerminal(moves, terminal) =>
          Option.when(
            continuation.moves == moves.map(EvidenceRef.normalizeMove) &&
              continuation.automaticTerminal.contains(terminal)
          )(exact)
      admission <- continuation.bind(evaluation)
      materialized <- materializeAdmission(history, admission)
    yield materialized

  private def materializeAdmission(
      history: CanonicalPositionHistory,
      admission: AdmittedCandidateLine
  ): Option[AdmittedEvaluation] =
    for
      steps <- admission.continuationAfter(history)
      replay <- CanonicalLineReplay.fromHistory(steps)
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

  private def preferredRankedEvaluations(
      evaluations: List[AdmittedEvaluation],
      sideToMove: Color
  ): List[AdmittedEvaluation] =
    val byRootMove = evaluations.zipWithIndex
      .groupBy { case (admitted, _) =>
        admitted.evaluation.moves.headOption.map(EvidenceRef.normalizeMove).getOrElse("")
      }
    // Direct inputs may contain several analyses of one root move. Select by
    // depth, then input order, while retaining every observation and path.
    val selectedEvaluations = byRootMove
      .values
      .map(_.minBy { case (admitted, index) =>
        (-admitted.evaluation.engineLine.map(_.depth).getOrElse(Int.MaxValue), index)
      })
      .toList
    val ordered = selectedEvaluations.sortBy { case (admitted, originalRank) =>
      (-admitted.evaluation.rankingScoreForMover(sideToMove), originalRank)
    }
    ordered.map(_._1)

  private def automaticTerminal(history: CanonicalPositionHistory): Option[AutomaticTerminal] =
    PositionRuleAssessment.assess(history) match
      case PositionRuleAssessment.Terminal(terminal) => Some(terminal)
      case PositionRuleAssessment.Nonterminal(_, _, _) => None
