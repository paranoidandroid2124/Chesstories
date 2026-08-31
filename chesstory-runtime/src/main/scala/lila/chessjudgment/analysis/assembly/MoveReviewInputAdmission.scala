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
  RankedCandidateLineEvaluation,
  ThreefoldClaimKnowledge
}
import lila.chessjudgment.model.{
  BranchReplyProbeBinding,
  ProbeContractValidator,
  ProbeRequest,
  ProbeResolution,
  ProbeResult
}
import lila.chessjudgment.model.line.EngineLine
import lila.chessjudgment.model.judgment.{
  EvidenceRef,
  CanonicalLineReplay,
  CandidateSetDescriptor,
  LineNodeRole,
  AdmittedReviewLine,
  AdmittedMoveReviewInput,
  AdmittedReviewBranchReply,
  PlayerFacingPositionAction
}

/** Development-only input; player-use jobs enter through the focused v6 preparation path. */
final case class RawMoveReviewInput(
    fen: String,
    playedMoveUci: String,
    variations: List[EngineLine],
    ply: Option[Int] = None,
    movePrefixUci: List[String] = Nil,
    probeResults: List[ProbeResult] = Nil
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

  private[assembly] final case class AdmittedProbeBranch(branch: AdmittedReviewBranchReply)
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
            rankedDescriptors = rankedRoot.zipWithIndex.map { case (admitted, index) =>
              RankedCandidateLineEvaluation(
                index + 1,
                EvidenceRef.normalizeMove(admitted.evaluation.moves.head),
                admitted.evaluation
              )
            }
            completeCandidateSet <- CandidateSetDescriptor.fromCompleteRanking(
              rootPosition.color,
              rankedDescriptors
            )
          yield
            PreparedFocusedPositionCommentary.MoveReview(
              PreparedFocusedMoveReview.materialized(
                legalMoveIndex,
                playedMove,
                bestMove,
                rankedRoot.indexWhere(_.evaluation.moves.headOption.exists(EvidenceRef.sameMove(_, playedMove))) match
                  case -1    => None
                  case index => Some(index + 1),
                prepared.copy(completeCandidateSet = Some(completeCandidateSet)),
                drawClaims
              )
            )
    yield result

  /** Adds already certified branches without replaying or revalidating them. */
  private[assembly] def withAdmittedProbeBranches(
      preparedInput: AdmittedMoveReviewInput,
      admissions: List[AdmittedProbeBranch]
  ): AdmittedMoveReviewInput =
    val branches = preparedInput.branchReplies ++ admissions.map(_.branch)
    require(
      branches.map(_.sourceProbeId).distinct.size == branches.size,
      "admitted probe branches must have unique source ids"
    )
    preparedInput.copy(branchReplies = branches)

  /** Test/development boundary for an exact issued request/result handoff. */
  private[assembly] def withAdmittedProbeResults(
      preparedInput: AdmittedMoveReviewInput,
      fulfilledProbeResults: List[(ProbeRequest, ProbeResult)]
  ): AdmittedMoveReviewInput =
    val admissions = fulfilledProbeResults.foldLeft(List.empty[AdmittedProbeBranch]) {
      case (accepted, (request, result)) =>
        val firstRank = nextBranchReplyRank(preparedInput, accepted)
        admitIssuedProbeResult(preparedInput, request, result, firstRank) match
          case Some(admission) => accepted :+ admission
          case None =>
            throw IllegalArgumentException(
              s"issued probe ${request.id} was rejected"
            )
    }
    withAdmittedProbeBranches(preparedInput, admissions)

  private[assembly] def nextBranchReplyRank(
      preparedInput: AdmittedMoveReviewInput,
      admissions: List[AdmittedProbeBranch]
  ): Int =
    (
      preparedInput.lines.map(_.rank) ++
        preparedInput.branchReplies.flatMap(_.lines.map(_.rank)) ++
        admissions.flatMap(_.branch.lines.map(_.rank))
    ).maxOption
      .getOrElse(0) + 1

  def admit(raw: RawMoveReviewInput): Option[AdmittedMoveReviewInput] =
    val beforeFen = PrincipalVariationEvidence.normalizeFen(raw.fen)
    val playedMove = EvidenceRef.normalizeMove(raw.playedMoveUci)
    val movePrefix = raw.movePrefixUci.map(EvidenceRef.normalizeMove).filter(_.nonEmpty)
    val positionHistory =
      if movePrefix.nonEmpty then CanonicalPositionHistory.from(Standard.initialFen.value, movePrefix, beforeFen)
      else CanonicalPositionHistory.from(beforeFen, Nil, beforeFen)
    for
      _ <- Option.when(raw.probeResults.isEmpty)(())
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
      val played =
        ranked
          .find { case (admitted, _) =>
            admitted.evaluation.moves.headOption.exists(move => EvidenceRef.sameMove(move, playedMove))
          }
          .filter { case (admitted, _) =>
            admitted.evaluation.moves.size >= 2 || admitted.automaticTerminal.nonEmpty
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
            reference.exists(_.rank == index + 1) || played.exists(_.rank == index + 1)
          }
          .map { case (admitted, index) =>
            AdmittedReviewLine(
              LineNodeRole.Alternative,
              index + 1,
              admitted.evaluation,
              admitted.replay
            )
          }
      val lines = (reference.toList ++ played.toList ++ alternatives).distinctBy(line => line.role -> line.rank)
      played.flatMap { playedLine =>
        reference.flatMap { _ =>
          val graphPlayedMove = playedLine.rootMove.getOrElse(playedMove)
          for
            graphAfterPlayed <- playedLine.replay.replaySteps.headOption.map(_.fenAfter)
            afterReference = reference.flatMap(_.replay.replaySteps.headOption).map(_.fenAfter)
          yield AdmittedMoveReviewInput(
            beforeFen = canonicalBeforeFen,
            playedMoveUci = graphPlayedMove,
            beforePly = beforePly,
            sideToMove = side,
            afterPlayedFen = graphAfterPlayed,
            afterReferenceFen = afterReference,
            lines = lines,
            completeCandidateSet = None,
            positionHistory = history,
            branchReplies = Nil
          )
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
        entries.map(_._1.evaluation).distinct.size == 1,
        s"conflicting evaluations for one root move: $rootMove"
      )
    }
    val selectedEvaluations = byRootMove
      .values
      .map { entries =>
        val earliestRank = entries.map(_._2).min
        entries.minBy(_._2)._1 -> earliestRank
      }
      .toList
    val ordered = sideToMove match
      case Some(side) =>
        selectedEvaluations.sortBy { case (admitted, originalRank) =>
          (-admitted.evaluation.rankingScoreForMover(side), originalRank)
        }
      case None => selectedEvaluations.sortBy(_._2)
    ordered.map(_._1)

  private enum ProbeCandidateLines:
    case Raw(override val evaluations: List[CandidateLineEvaluation])
    case Certified(admissions: List[AdmittedCandidateLine])

    def evaluations: List[CandidateLineEvaluation] = this match
      case Raw(values)       => values
      case Certified(values) => values.map(_.evaluation)

  private final case class ProbeAdmissionInput(
      request: ProbeRequest,
      result: ProbeResult,
      lines: ProbeCandidateLines
  )

  private final case class BranchReplySeed(
      sourceProbeId: String,
      probedMoveUci: String,
      replyMoveUci: String,
      branchFen: String,
      branchHistory: CanonicalPositionHistory,
      predecessorReplay: CanonicalLineReplay,
      lines: ProbeCandidateLines,
      certifiedHorizonPlyOffset: Int,
      requiredReplyCount: Int,
      depthFloor: Int
  )

  private[assembly] def admitIssuedProbeResult(
      preparedInput: AdmittedMoveReviewInput,
      request: ProbeRequest,
      result: ProbeResult,
      firstBranchReplyRank: Int
  ): Option[AdmittedProbeBranch] =
    val evaluations = result.resolution match
      case ProbeResolution.EngineSearch(values, _) => values
      case ProbeResolution.ExactAutomaticTerminal(evaluation) => List(evaluation)
    admitBranchReplies(
      List(ProbeAdmissionInput(request, result, ProbeCandidateLines.Raw(evaluations))),
      beforeFen = preparedInput.beforeFen,
      rootLines = preparedInput.lines.filterNot(_.role == LineNodeRole.BranchReply),
      firstBranchReplyRank = firstBranchReplyRank,
      positionHistory = preparedInput.positionHistory
    ) match
      case branch :: Nil => Some(AdmittedProbeBranch(branch))
      case _             => None

  /** Production handoff for lines whose history occurrence was already
    * admitted when the browser report was accepted. No UCI move is replayed.
    */
  def admitCertifiedProbeResult(
      preparedInput: AdmittedMoveReviewInput,
      request: ProbeRequest,
      result: ProbeResult,
      admissions: List[AdmittedCandidateLine]
  ): Option[AdmittedMoveReviewInput] =
    admitBranchReplies(
      List(ProbeAdmissionInput(request, result, ProbeCandidateLines.Certified(admissions))),
      beforeFen = preparedInput.beforeFen,
      rootLines = preparedInput.lines.filterNot(_.role == LineNodeRole.BranchReply),
      firstBranchReplyRank = nextBranchReplyRank(preparedInput, Nil),
      positionHistory = preparedInput.positionHistory
    ) match
      case branch :: Nil =>
        Some(withAdmittedProbeBranches(preparedInput, List(AdmittedProbeBranch(branch))))
      case _ => None

  private def admitBranchReplies(
      probes: List[ProbeAdmissionInput],
      beforeFen: String,
      rootLines: List[AdmittedReviewLine],
      firstBranchReplyRank: Int,
      positionHistory: CanonicalPositionHistory
  ): List[AdmittedReviewBranchReply] =
    val branches = List.newBuilder[AdmittedReviewBranchReply]
    var nextRank = firstBranchReplyRank
    probes.foreach { probe =>
      branchReplySeed(beforeFen, rootLines, positionHistory, probe) match
        case None => ()
        case Some(seed) =>
          val side = Some(seed.branchHistory.currentPosition.color)
          val depthFloor = seed.depthFloor
          val materializedLines = seed.lines match
            case ProbeCandidateLines.Raw(evaluations) =>
              evaluations.flatMap(admitEvaluation(seed.branchHistory, _, Some(seed.predecessorReplay)))
            case ProbeCandidateLines.Certified(admissions) =>
              admissions.flatMap(materializeAdmission(seed.branchHistory, _, Some(seed.predecessorReplay)))
          val legalLines = preferredRankedEvaluations(
            materializedLines,
            side
          )
          val requiredReplyCount = seed.requiredReplyCount
          val topLegalLines = legalLines.take(requiredReplyCount)
          val topLinesDepthReady = topLegalLines.forall {
            case AdmittedEvaluation(CandidateLineEvaluation.EngineSearch(line), _, _) =>
              line.depth >= depthFloor
            case AdmittedEvaluation(CandidateLineEvaluation.ExactAutomaticTerminal(_, _), _, _) =>
              true
          }
          val replyLineBound = topLegalLines match
            case exact :: Nil => exact.evaluation.moves.headOption.exists(EvidenceRef.sameMove(_, seed.replyMoveUci))
            case _            => false
          val horizonCovered = topLegalLines.forall(admitted =>
            admitted.replay.legalSteps.size >= seed.certifiedHorizonPlyOffset ||
              admitted.automaticTerminal.nonEmpty
          )
          if requiredReplyCount > 0 &&
            topLegalLines.size == requiredReplyCount &&
            topLinesDepthReady &&
            replyLineBound &&
            horizonCovered
          then
            val branchLines = topLegalLines.map { admitted =>
              val rank = nextRank
              nextRank += 1
              AdmittedReviewLine(
                role = LineNodeRole.BranchReply,
                rank = rank,
                evaluation = admitted.evaluation,
                replay = admitted.replay,
                predecessorReplay = Some(seed.predecessorReplay)
              )
            }
            branches += AdmittedReviewBranchReply(
              sourceProbeId = seed.sourceProbeId,
              probedMoveUci = seed.probedMoveUci,
              branchFen = seed.branchFen,
              branchPly = plyFromFen(seed.branchFen),
              certifiedHorizonPlyOffset = seed.certifiedHorizonPlyOffset,
              lines = branchLines
            )
    }
    branches.result()

  private def branchReplySeed(
      beforeFen: String,
      rootLines: List[AdmittedReviewLine],
      positionHistory: CanonicalPositionHistory,
      input: ProbeAdmissionInput
  ): Option[BranchReplySeed] =
    val request = input.request
    val probe = input.result
    val evaluations = probe.resolution match
      case ProbeResolution.EngineSearch(values, _) => values
      case ProbeResolution.ExactAutomaticTerminal(evaluation) => List(evaluation)
    val branchFen = Option(request.fen).map(PrincipalVariationEvidence.normalizeFen).filter(_.nonEmpty)
    val probedMove = Option(EvidenceRef.normalizeMove(request.candidateMove)).filter(_.nonEmpty)
    val replyMove = request.moves match
      case exact :: Nil => Option(EvidenceRef.normalizeMove(exact)).filter(_.nonEmpty)
      case _            => None
    val certifiedHorizonPlyOffset = request.horizon.flatMap(BranchReplyProbeBinding.horizonPlyOffset)
    val depthFloor = Option.when(request.depthFloor > 0)(request.depthFloor)
    val contract = ProbeContractValidator.validateAgainstRequest(request, probe)
    if !contract.isValid || input.lines.evaluations != evaluations then None
    else (branchFen, probedMove, replyMove, certifiedHorizonPlyOffset, depthFloor) match
      case (None, _, _, _, _) => None
      case (_, None, _, _, _) => None
      case (_, _, None, _, _) => None
      case (_, _, _, None, _) => None
      case (_, _, _, _, None) => None
      case (Some(fen), Some(move), Some(reply), Some(horizon), Some(floor)) =>
        val moveLines = rootLines
          .filter(_.rootMove.exists(EvidenceRef.sameMove(_, move)))
        val matchingMoveLines = moveLines.flatMap { line =>
          for
            first <- line.replay.replaySteps.headOption
            if PrincipalVariationEvidence.sameBoardState(first.fenBefore, beforeFen)
            if PrincipalVariationEvidence.sameBoardState(first.fenAfter, fen)
            predecessor <- line.replay.subset(List(first))
            branchHistory <- positionHistory.extendAdmitted(predecessor.legalSteps).toOption
            if PrincipalVariationEvidence.sameBoardState(branchHistory.currentFen, fen)
            (_, expectedHash) <- branchHash(
              beforeFen,
              line,
              move,
              reply,
              horizon
            )
            if request.variationHash == expectedHash
          yield (predecessor, branchHistory)
        }
        if moveLines.isEmpty then None
        else matchingMoveLines match
          case (predecessor, branchHistory) :: Nil =>
            Some(
              BranchReplySeed(
                sourceProbeId = request.id,
                probedMoveUci = move,
                replyMoveUci = reply,
                branchFen = branchHistory.currentFen,
                branchHistory = branchHistory,
                predecessorReplay = predecessor,
                lines = input.lines,
                certifiedHorizonPlyOffset = horizon,
                requiredReplyCount = 1,
                depthFloor = floor
              )
            )
          case Nil => None
          case _   => None

  private def branchHash(
      beforeFen: String,
      line: AdmittedReviewLine,
      probedMove: String,
      replyMove: String,
      certifiedHorizonPlyOffset: Int
  ): Option[(EngineLine, String)] =
    line.evaluation.engineLine.flatMap { engineLine =>
      Some(
        engineLine -> BranchReplyProbeBinding.variationHash(
          rootFen = beforeFen,
          role = line.role,
          rootMove = probedMove,
          whitePovEvalCp = engineLine.scoreCp,
          mate = engineLine.mate,
          depth = engineLine.depth,
          moves = engineLine.moves,
          replyMove = replyMove,
          certifiedHorizonPlyOffset = certifiedHorizonPlyOffset
        )
      )
    }

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

  private def plyFromFen(fen: String): Int =
    val parts = fen.split("\\s+")
    val fullMove = parts.lift(5).flatMap(_.toIntOption).getOrElse(1).max(1)
    val blackToMove = parts.lift(1).contains("b")
    (fullMove - 1) * 2 + Option.when(blackToMove)(1).getOrElse(0)
