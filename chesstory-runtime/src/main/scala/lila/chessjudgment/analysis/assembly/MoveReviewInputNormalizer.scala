package lila.chessjudgment.analysis.assembly

import chess.Color
import chess.variant.Standard
import play.api.libs.json.{ Json, OFormat }
import lila.chessjudgment.analysis.opening.{ OpeningRecognitionIndex, OpeningThemePriorIndex }
import lila.chessjudgment.model.line.{
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
  ProbeAdmissionDiagnostic,
  ProbeAdmissionStatus,
  ProbeContractValidator,
  ProbeKind,
  ProbeRequest,
  ProbeResolution,
  ProbeResult
}
import lila.chessjudgment.model.strategic.EngineLine
import lila.chessjudgment.model.judgment.{
  EvidenceRef,
  CanonicalLineReplay,
  CandidateSetDescriptor,
  LineNodeRole,
  NormalizedCandidateLine,
  NormalizedMoveReviewInput,
  NormalizedThreatBranch,
  OpeningContextEvidence,
  OpeningContextSignal,
  OpeningRecognition,
  PlayerFacingPositionAction
}

final case class RawOpeningContext(
    eco: Option[String] = None,
    name: Option[String] = None,
    family: Option[String] = None
)

object RawOpeningContext:
  given OFormat[RawOpeningContext] = Json.format[RawOpeningContext]

/** Development-only input; player-use jobs enter through the focused v6 preparation path. */
final case class RawMoveReviewInput(
    fen: String,
    playedMoveUci: String,
    variations: List[EngineLine],
    ply: Option[Int] = None,
    openingContext: Option[RawOpeningContext] = None,
    movePrefixUci: List[String] = Nil,
    probeResults: List[ProbeResult] = Nil
)

/** The v6 player path owns one reviewed move and one engine-ranked reference. */
final class PreparedFocusedMoveReview private (
    val legalMoveIndex: Int,
    val playedMoveUci: String,
    val bestMoveUci: String,
    val playedRootRank: Option[Int],
    val preparedInput: NormalizedMoveReviewInput,
    val drawClaims: List[DrawClaimAction]
)

object PreparedFocusedMoveReview:
  private[assembly] def materialized(
      legalMoveIndex: Int,
      playedMoveUci: String,
      bestMoveUci: String,
      playedRootRank: Option[Int],
      preparedInput: NormalizedMoveReviewInput,
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

object MoveReviewInputNormalizer:
  private final case class AdmittedEvaluation(
      evaluation: CandidateLineEvaluation,
      replay: CanonicalLineReplay,
      automaticTerminal: Option[AutomaticTerminal]
  )

  private[assembly] final case class AdmittedProbeBranch(
      request: ProbeRequest,
      result: ProbeResult,
      branch: NormalizedThreatBranch,
      diagnostic: ProbeAdmissionDiagnostic
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
      rootEvaluations: List[CandidateLineEvaluation],
      focusComparisonEvaluations: List[CandidateLineEvaluation]
  ): Option[PreparedFocusedPositionCommentary] =
    val rootPosition = positionHistory.currentPosition
    val legalMoves = positionHistory.currentLegalMoveUcis
    val playedMove = EvidenceRef.normalizeMove(playedMoveUci)
    val legalMoveIndex = legalMoves.indexOf(playedMove)
    val normalizedResultingFen = PrincipalVariationEvidence.normalizeFen(resultingFen)

    for
      _ <- Option.when(legalMoveIndex >= 0)(())
      _ <- Option.when(rootEvaluations.size == legalMoves.size.min(3))((): Unit)
      admittedRoot <- admitEvaluations(positionHistory, rootEvaluations)
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
      admittedComparison <- admitEvaluations(positionHistory, focusComparisonEvaluations)
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
              List(comparisonByMove(bestMove), comparisonByMove(playedMove)) ++
                rankedRoot.filterNot(value =>
                  Set(bestMove, playedMove)(EvidenceRef.normalizeMove(value.evaluation.moves.head))
                )
            else rankedRoot
          for
            prepared <- normalizeAdmittedPrepared(
              positionHistory,
              playedMove,
              reviewEvaluations,
              OpeningRecognitionIndex.default,
              OpeningThemePriorIndex.default
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
      preparedInput: NormalizedMoveReviewInput,
      admissions: List[AdmittedProbeBranch]
  ): NormalizedMoveReviewInput =
    val branches = preparedInput.threatBranches ++ admissions.map(_.branch)
    require(
      branches.map(_.sourceProbeId).distinct.size == branches.size,
      "admitted probe branches must have unique source ids"
    )
    preparedInput.copy(
      threatBranches = branches,
      probeDiagnostics = (
        preparedInput.probeDiagnostics ++ admissions.map(_.diagnostic)
      ).distinctBy(diagnostic => diagnostic.probeId -> diagnostic.reasonCodes)
    )

  /** Test/development boundary for an exact issued request/result handoff. */
  private[assembly] def withAdmittedProbeResults(
      preparedInput: NormalizedMoveReviewInput,
      fulfilledProbeResults: List[(ProbeRequest, ProbeResult)]
  ): NormalizedMoveReviewInput =
    val admissions = fulfilledProbeResults.foldLeft(List.empty[AdmittedProbeBranch]) {
      case (accepted, (request, result)) =>
        val firstRank = nextThreatRank(preparedInput, accepted)
        admitIssuedProbeResult(preparedInput, request, result, firstRank) match
          case Right(admission) => accepted :+ admission
          case Left(diagnostic) =>
            throw IllegalArgumentException(
              s"issued probe ${request.id} was rejected: ${diagnostic.reasonCodes.mkString(",")}"
            )
    }
    withAdmittedProbeBranches(preparedInput, admissions)

  private[assembly] def nextThreatRank(
      preparedInput: NormalizedMoveReviewInput,
      admissions: List[AdmittedProbeBranch]
  ): Int =
    (
      preparedInput.lines.map(_.rank) ++
        preparedInput.threatBranches.flatMap(_.lines.map(_.rank)) ++
        admissions.flatMap(_.branch.lines.map(_.rank))
    ).maxOption
      .getOrElse(0) + 1

  def normalize(
      raw: RawMoveReviewInput,
      recognitionIndex: OpeningRecognitionIndex = OpeningRecognitionIndex.default,
      themePriorIndex: OpeningThemePriorIndex = OpeningThemePriorIndex.default
  ): Option[NormalizedMoveReviewInput] =
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
      input <- normalizePrepared(
        history,
        playedMove,
        raw.variations.map(line => CandidateLineEvaluation.EngineSearch(line)),
        recognitionIndex,
        themePriorIndex
      )
    yield input

  private def normalizePrepared(
      history: CanonicalPositionHistory,
      playedMove: String,
      variations: List[CandidateLineEvaluation],
      recognitionIndex: OpeningRecognitionIndex,
      themePriorIndex: OpeningThemePriorIndex
  ): Option[NormalizedMoveReviewInput] =
    admitEvaluations(history, variations).flatMap { admitted =>
      normalizeAdmittedPrepared(
        history,
        playedMove,
        admitted,
        recognitionIndex,
        themePriorIndex
      )
    }

  private def normalizeAdmittedPrepared(
      history: CanonicalPositionHistory,
      playedMove: String,
      variations: List[AdmittedEvaluation],
      recognitionIndex: OpeningRecognitionIndex,
      themePriorIndex: OpeningThemePriorIndex
  ): Option[NormalizedMoveReviewInput] =
      val canonicalBeforeFen = history.currentFen
      val beforePly = history.currentPly
      val side = Some(history.currentPosition.color)
      val openingContext = openingContextFor(history, recognitionIndex, themePriorIndex)
      val ranked = preferredRankedEvaluations(
        variations,
        side
      ).zipWithIndex
      val reference = ranked.headOption.map { case (admitted, index) =>
        NormalizedCandidateLine(
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
            NormalizedCandidateLine(
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
            NormalizedCandidateLine(
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
          yield NormalizedMoveReviewInput(
            beforeFen = canonicalBeforeFen,
            playedMoveUci = graphPlayedMove,
            beforePly = beforePly,
            sideToMove = side,
            afterPlayedFen = graphAfterPlayed,
            afterReferenceFen = afterReference,
            lines = lines,
            completeCandidateSet = None,
            positionHistory = history,
            openingContext = openingContext,
            threatBranches = Nil,
            probeDiagnostics = Nil
          )
        }
      }

  private def admitEvaluations(
      history: CanonicalPositionHistory,
      evaluations: List[CandidateLineEvaluation]
  ): Option[List[AdmittedEvaluation]] =
    val admitted = evaluations.map(admitEvaluation(history, _))
    Option.when(admitted.forall(_.nonEmpty))(admitted.flatten)

  private def admitEvaluation(
      history: CanonicalPositionHistory,
      rawEvaluation: CandidateLineEvaluation,
      predecessorReplay: Option[CanonicalLineReplay] = None
  ): Option[AdmittedEvaluation] =
    val normalized = normalizedEvaluation(rawEvaluation)
    val prefixSize = history.segmentReplaySteps.size
    for
      _ <- Option.when(normalized.moves.nonEmpty)(())
      (evaluation, lineHistory) <- normalized match
        case CandidateLineEvaluation.EngineSearch(line) =>
          history.extend(line.moves).toOption match
            case Some(completeHistory) =>
              val terminal = automaticTerminal(completeHistory)
              Option.when(engineScoreConsistent(line, terminal))(
                terminal
                  .map(CandidateLineEvaluation.ExactAutomaticTerminal(line.moves, _))
                  .getOrElse(normalized) -> completeHistory
              )
            case None =>
              history
                .inspectAutomaticTerminalBoundary(line.moves)
                .toOption
                .flatten
                .filter { case (_, terminal) => engineScoreConsistent(line, Some(terminal)) }
                .flatMap { case (terminalMoves, terminal) =>
                  history.extend(terminalMoves).toOption.map(
                    CandidateLineEvaluation.ExactAutomaticTerminal(terminalMoves, terminal) -> _
                  )
                }
        case exact @ CandidateLineEvaluation.ExactAutomaticTerminal(moves, terminal) =>
          history
            .extend(moves)
            .toOption
            .filter(lineHistory => automaticTerminal(lineHistory).contains(terminal))
            .map(exact -> _)
      steps = lineHistory.segmentReplaySteps.drop(prefixSize)
      replay <- predecessorReplay match
        case Some(predecessor) => CanonicalLineReplay.continueFromHistory(predecessor, steps)
        case None              => CanonicalLineReplay.fromHistory(steps)
    yield AdmittedEvaluation(evaluation, replay, automaticTerminal(lineHistory))

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

  private def openingContextFor(
      positionHistory: CanonicalPositionHistory,
      recognitionIndex: OpeningRecognitionIndex,
      themePriorIndex: OpeningThemePriorIndex
  ): OpeningContextEvidence =
    val recognition = recognitionIndex.recognize(
      positionHistory.movePrefixUci,
      positionHistory.currentFen,
      positionHistory.currentPly
    )
    val identity = recognition.flatMap(_.bestIdentity)
    val themePriorSelection = themePriorIndex.priorSelectionFor(
      certifyingRecognitionLineage(recognition),
      identity.flatMap(_.family),
      identity.flatMap(_.name)
    )
    OpeningContextEvidence(
      identity = identity,
      signals = List(
        Option.when(recognition.nonEmpty)(OpeningContextSignal.RecognizedIdentity),
        Option.when(themePriorSelection.nonEmpty)(OpeningContextSignal.ThemePrior)
      ).flatten,
      recognition = recognition,
      themePriorSelection = themePriorSelection
    )

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

  private final case class ThreatBranchSeed(
      sourceProbeId: String,
      probedMoveUci: String,
      replyMoveUci: String,
      branchFen: String,
      branchHistory: CanonicalPositionHistory,
      predecessorReplay: CanonicalLineReplay,
      evaluations: List[CandidateLineEvaluation],
      variationHash: Option[String],
      certifiedHorizonPlyOffset: Int,
      requiredReplyCount: Int,
      depthFloor: Int
  )

  private final case class ThreatBranchNormalization(
      branches: List[NormalizedThreatBranch],
      diagnostics: List[ProbeAdmissionDiagnostic]
  )

  private[assembly] def admitIssuedProbeResult(
      preparedInput: NormalizedMoveReviewInput,
      request: ProbeRequest,
      result: ProbeResult,
      firstThreatRank: Int
  ): Either[ProbeAdmissionDiagnostic, AdmittedProbeBranch] =
    val normalized = normalizeThreatBranches(
      List(request -> result),
      beforeFen = preparedInput.beforeFen,
      rootLines = preparedInput.lines.filterNot(_.role == LineNodeRole.Threat),
      firstThreatRank = firstThreatRank,
      positionHistory = preparedInput.positionHistory
    )
    (normalized.branches, normalized.diagnostics) match
      case (branch :: Nil, diagnostic :: Nil) if diagnostic.status == ProbeAdmissionStatus.Admitted =>
        Right(AdmittedProbeBranch(request, result, branch, diagnostic))
      case (Nil, diagnostic :: Nil) => Left(diagnostic)
      case _ =>
        Left(
          probeDiagnostic(
            result,
            status = ProbeAdmissionStatus.Rejected,
            reasons = List("PROBE_ADMISSION_INCONSISTENT"),
            purpose = Some(request.kind),
            candidateMove = Some(request.candidateMove),
            branchFen = Some(request.fen),
            admittedLineCount = 0,
            legalLineCount = 0,
            scoredLineCount = 0,
            depthFloor = Some(request.depthFloor),
            horizon = request.horizon,
            variationHash = Some(request.variationHash)
          )
        )

  private def normalizeThreatBranches(
      probes: List[(ProbeRequest, ProbeResult)],
      beforeFen: String,
      rootLines: List[NormalizedCandidateLine],
      firstThreatRank: Int,
      positionHistory: CanonicalPositionHistory
  ): ThreatBranchNormalization =
    val branches = List.newBuilder[NormalizedThreatBranch]
    val diagnostics = List.newBuilder[ProbeAdmissionDiagnostic]
    var nextRank = firstThreatRank
    probes.foreach { case (request, probe) =>
      threatBranchSeed(beforeFen, rootLines, positionHistory, request, probe) match
        case Left(diagnostic) =>
          diagnostics += diagnostic
        case Right(seed) =>
          val side = Some(seed.branchHistory.currentPosition.color)
          val depthFloor = seed.depthFloor
          val legalLines = preferredRankedEvaluations(
            seed.evaluations.flatMap(admitEvaluation(seed.branchHistory, _, Some(seed.predecessorReplay))),
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
              NormalizedCandidateLine(
                role = LineNodeRole.Threat,
                rank = rank,
                evaluation = admitted.evaluation,
                replay = admitted.replay,
                predecessorReplay = Some(seed.predecessorReplay)
              )
            }
            branches += NormalizedThreatBranch(
              sourceProbeId = seed.sourceProbeId,
              probedMoveUci = seed.probedMoveUci,
              branchFen = seed.branchFen,
              branchPly = plyFromFen(seed.branchFen),
              certifiedHorizonPlyOffset = seed.certifiedHorizonPlyOffset,
              lines = branchLines
            )
            diagnostics += probeDiagnostic(
              probe,
              status = ProbeAdmissionStatus.Admitted,
              reasons = List("ADMITTED"),
              purpose = Some(ProbeKind.BranchReply),
              candidateMove = Some(seed.probedMoveUci),
              branchFen = Some(seed.branchFen),
              admittedLineCount = branchLines.size,
              legalLineCount = legalLines.size,
              scoredLineCount = seed.evaluations.count(_.moves.nonEmpty),
              depthFloor = Some(depthFloor),
              horizon = Some(BranchReplyProbeBinding.horizon(seed.certifiedHorizonPlyOffset)),
              variationHash = seed.variationHash
            )
          else
            val reason =
              if requiredReplyCount == 0 then "BRANCH_POSITION_TERMINAL"
              else if legalLines.size < requiredReplyCount then "INSUFFICIENT_LEGAL_REPLY_LINES"
              else if !replyLineBound then "REPLY_MOVE_UNBOUND"
              else if !horizonCovered then "HORIZON_COVERAGE_UNVERIFIED"
              else "REPLY_LINE_DEPTH_FLOOR_UNMET"
            diagnostics += probeDiagnostic(
              probe,
              status = ProbeAdmissionStatus.Rejected,
              reasons = List(reason),
              purpose = Some(ProbeKind.BranchReply),
              candidateMove = Some(seed.probedMoveUci),
              branchFen = Some(seed.branchFen),
              admittedLineCount = 0,
              legalLineCount = legalLines.size,
              scoredLineCount = seed.evaluations.count(_.moves.nonEmpty),
              depthFloor = Some(depthFloor),
              horizon = Some(BranchReplyProbeBinding.horizon(seed.certifiedHorizonPlyOffset)),
              variationHash = seed.variationHash
            )
    }
    ThreatBranchNormalization(branches.result(), diagnostics.result())

  private def threatBranchSeed(
      beforeFen: String,
      rootLines: List[NormalizedCandidateLine],
      positionHistory: CanonicalPositionHistory,
      request: ProbeRequest,
      probe: ProbeResult
  ): Either[ProbeAdmissionDiagnostic, ThreatBranchSeed] =
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
    val baseDiagnostic = (reasons: List[String]) =>
      probeDiagnostic(
        probe,
        status = ProbeAdmissionStatus.Rejected,
        reasons = reasons,
        purpose = Some(request.kind),
        candidateMove = probedMove,
        branchFen = branchFen,
        admittedLineCount = 0,
        legalLineCount = 0,
        scoredLineCount = evaluations.count(_.moves.nonEmpty),
        depthFloor = depthFloor,
        horizon = request.horizon,
        variationHash = Some(request.variationHash)
      )

    if !contract.isValid then
      Left(baseDiagnostic(("CONTRACT_INVALID" :: contract.reasonCodes).distinct))
    else (branchFen, probedMove, replyMove, certifiedHorizonPlyOffset, depthFloor) match
      case (None, _, _, _, _) => Left(baseDiagnostic(List("BRANCH_FEN_MISSING")))
      case (_, None, _, _, _) => Left(baseDiagnostic(List("PROBED_MOVE_MISSING")))
      case (_, _, None, _, _) => Left(baseDiagnostic(List("REPLY_MOVE_UNVERIFIED")))
      case (_, _, _, None, _) => Left(baseDiagnostic(List("HORIZON_UNVERIFIED")))
      case (_, _, _, _, None) => Left(baseDiagnostic(List("DEPTH_FLOOR_UNVERIFIED")))
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
        if moveLines.isEmpty then Left(baseDiagnostic(List("ROOT_LINE_UNBOUND")))
        else matchingMoveLines match
          case (predecessor, branchHistory) :: Nil =>
            Right(
              ThreatBranchSeed(
                sourceProbeId = request.id,
                probedMoveUci = move,
                replyMoveUci = reply,
                branchFen = branchHistory.currentFen,
                branchHistory = branchHistory,
                predecessorReplay = predecessor,
                evaluations = evaluations,
                variationHash = Some(request.variationHash),
                certifiedHorizonPlyOffset = horizon,
                requiredReplyCount = 1,
                depthFloor = floor
              )
            )
          case Nil => Left(baseDiagnostic(List("BRANCH_OR_VARIATION_UNBOUND")))
          case _   => Left(baseDiagnostic(List("ROOT_LINE_AMBIGUOUS")))

  private def branchHash(
      beforeFen: String,
      line: NormalizedCandidateLine,
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

  private def probeDiagnostic(
      probe: ProbeResult,
      status: ProbeAdmissionStatus,
      reasons: List[String],
      purpose: Option[ProbeKind],
      candidateMove: Option[String],
      branchFen: Option[String],
      admittedLineCount: Int,
      legalLineCount: Int,
      scoredLineCount: Int,
      depthFloor: Option[Int],
      horizon: Option[String],
      variationHash: Option[String]
  ): ProbeAdmissionDiagnostic =
    ProbeAdmissionDiagnostic(
      probeId = Option(probe.id).map(_.trim).filter(_.nonEmpty).getOrElse("probe-result"),
      status = status,
      reasonCodes = reasons.distinct,
      purpose = purpose,
      candidateMove = candidateMove,
      fen = branchFen,
      admittedLineCount = admittedLineCount,
      legalLineCount = legalLineCount,
      scoredLineCount = scoredLineCount,
      depthFloor = depthFloor,
      horizon = horizon,
      variationHash = variationHash
    )

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

  private def certifyingRecognitionLineage(recognition: Option[OpeningRecognition]): Option[String] =
    recognition.filter(certifyingRecognition).flatMap(_.lineage)

  private def certifyingRecognition(recognition: OpeningRecognition): Boolean =
    // A unique canonical position keeps the same opening identity across legal move-order transpositions.
    recognition.confidence >= CertifyingRecognitionMinConfidence &&
      recognition.candidates.size == 1

  private def plyFromFen(fen: String): Int =
    val parts = fen.split("\\s+")
    val fullMove = parts.lift(5).flatMap(_.toIntOption).getOrElse(1).max(1)
    val blackToMove = parts.lift(1).contains("b")
    (fullMove - 1) * 2 + Option.when(blackToMove)(1).getOrElse(0)

  private val CertifyingRecognitionMinConfidence: Double = 0.7
