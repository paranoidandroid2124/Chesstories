package lila.chessjudgment.analysis.assembly

import chess.Color
import chess.format.Fen
import chess.variant.Standard
import play.api.libs.json.{ Json, OFormat }
import lila.chessjudgment.model.evaluation.PerspectiveMath
import lila.chessjudgment.analysis.opening.{ OpeningRecognitionIndex, OpeningThemePriorIndex }
import lila.chessjudgment.model.line.{
  AutomaticTerminal,
  CandidateLineEvaluation,
  CanonicalPositionHistory,
  DeclaredDrawClaim,
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
  CounterResourceProbeBinding,
  ProbeAdmissionDiagnostic,
  ProbeAdmissionStatus,
  ProbeContractValidator,
  ProbePurpose,
  ProbeRequest,
  ProbeResolution,
  ProbeResult
}
import lila.chessjudgment.model.strategic.EngineLine
import lila.chessjudgment.model.judgment.{
  EvidenceRef,
  CandidateSetDescriptor,
  CompleteCandidateSet,
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

/** Development-only input; player-use jobs enter through the prepared inventory. */
final case class RawMoveReviewInput(
    fen: String,
    playedMoveUci: String,
    variations: List[EngineLine],
    ply: Option[Int] = None,
    openingContext: Option[RawOpeningContext] = None,
    movePrefixUci: List[String] = Nil,
    probeResults: List[ProbeResult] = Nil
)

/** A single legal root move prepared by the rank-once inventory owner. */
final class PreparedMoveReview private (
    val legalMoveIndex: Int,
    val playedMoveUci: String,
    val preparedInput: NormalizedMoveReviewInput
)

object PreparedMoveReview:
  private[assembly] def materialized(
      legalMoveIndex: Int,
      playedMoveUci: String,
      preparedInput: NormalizedMoveReviewInput
  ): PreparedMoveReview =
    new PreparedMoveReview(legalMoveIndex, playedMoveUci, preparedInput)

/**
 * Immutable result of validating every legal root move and selecting its one
 * canonical reference. Its constructor is private so only the normalizer can
 * establish the inventory's ranking and prepared records.
 */
final class PreparedMoveReviewInventory private (
    val reviewRecords: List[PreparedMoveReview],
    val drawClaims: List[DrawClaimAction]
)

object PreparedMoveReviewInventory:
  private[assembly] def materialized(
      reviewRecords: List[PreparedMoveReview],
      drawClaims: List[DrawClaimAction]
  ): PreparedMoveReviewInventory =
    new PreparedMoveReviewInventory(reviewRecords, drawClaims)

enum PreparedPositionCommentary:
  case PositionAction(action: PlayerFacingPositionAction)
  case MoveChoices(inventory: PreparedMoveReviewInventory)

object MoveReviewInputNormalizer:
  /** Validates root lines and ranks the supplied root-move inventory once. */
  def prepareMoveReviewInventory(
      positionHistory: CanonicalPositionHistory,
      orderedLegalMoves: List[String],
      rootRuleAssessment: PositionRuleAssessment,
      rootEvaluations: List[CandidateLineEvaluation],
      declaredDrawClaims: Set[DeclaredDrawClaim]
  ): Option[PreparedPositionCommentary] =
    val rootPosition = positionHistory.currentPosition
    val canonicalRootFen = positionHistory.currentFen
    val legalMoveKeys = orderedLegalMoves.map(normalizeUci)
    val canonicalLegalMoveKeys = rootPosition.legalMoves.map(_.toUci.uci)
    val normalizedEvaluations = rootEvaluations.map {
      case CandidateLineEvaluation.EngineSearch(line) =>
        CandidateLineEvaluation.EngineSearch(normalizedLine(line))
      case CandidateLineEvaluation.ExactAutomaticTerminal(moves, terminal) =>
        CandidateLineEvaluation.ExactAutomaticTerminal(moves.map(normalizeUci), terminal)
    }
    val rootMoveKeys = normalizedEvaluations.map(_.moves.headOption.map(normalizeUci).filter(_.nonEmpty))
    if rootMoveKeys.exists(_.isEmpty) then
      None
    else
      val rootMoveValues = rootMoveKeys.flatten
      val evaluationsByMove = normalizedEvaluations.groupBy(evaluation => normalizeUci(evaluation.moves.head))
      if legalMoveKeys.size != canonicalLegalMoveKeys.size ||
        legalMoveKeys.distinct.size != legalMoveKeys.size ||
        legalMoveKeys.toSet != canonicalLegalMoveKeys.toSet
      then
        None
      else if rootMoveValues.exists(move => !legalMoveKeys.contains(move)) then
        None
      else if evaluationsByMove.exists { case (_, entries) => entries.size != 1 } then
        None
      else if legalMoveKeys.exists(move => !evaluationsByMove.contains(move)) then
        None
      else
        def admittedLineHistory(moves: List[String]): Option[CanonicalPositionHistory] =
          moves.foldLeft(Option(positionHistory)) {
              case (Some(history), moveUci) =>
                PositionRuleAssessment.assess(history) match
                  case PositionRuleAssessment.Nonterminal(
                        FivefoldRepetitionKnowledge.CertifiedNonterminal,
                        _,
                        _
                      ) =>
                    history.extend(List(moveUci)).toOption
                  case PositionRuleAssessment.Terminal(_) |
                      PositionRuleAssessment.Nonterminal(
                        FivefoldRepetitionKnowledge.HistoryUnavailable,
                        _,
                        _
                      ) =>
                    None
              case _ =>
                None
          }
        val invalidEvaluation = normalizedEvaluations.exists {
          case CandidateLineEvaluation.EngineSearch(line) =>
            admittedLineHistory(line.moves) match
              case None => true
              case Some(lineHistory) =>
                PositionRuleAssessment.assess(lineHistory) match
                  case PositionRuleAssessment.Terminal(AutomaticTerminal.Checkmate(winner)) =>
                    !line.mate.exists(mate => (mate > 0) == winner.white)
                  case PositionRuleAssessment.Terminal(AutomaticTerminal.Stalemate) =>
                    line.mate.nonEmpty || line.scoreCp.abs > 1
                  case PositionRuleAssessment.Terminal(
                        AutomaticTerminal.InsufficientMaterial |
                          AutomaticTerminal.FivefoldRepetition |
                          AutomaticTerminal.SeventyFiveMoveRule
                      ) => false
                  case PositionRuleAssessment.Nonterminal(
                        FivefoldRepetitionKnowledge.HistoryUnavailable,
                        _,
                        _
                      ) => true
                  case PositionRuleAssessment.Nonterminal(
                        FivefoldRepetitionKnowledge.CertifiedNonterminal,
                        _,
                        _
                      ) => false
          case CandidateLineEvaluation.ExactAutomaticTerminal(moves, terminal) =>
            admittedLineHistory(moves)
              .map(PositionRuleAssessment.assess)
              .forall(_ != PositionRuleAssessment.Terminal(terminal))
        }
        if invalidEvaluation then None
        else
          val successorHistories = legalMoveKeys.map(moveUci => moveUci -> positionHistory.extend(List(moveUci)))
          if successorHistories.exists(_._2.isLeft) then
            None
          else
            val successorFensByMove = successorHistories.collect {
              case (moveUci, Right(history)) => moveUci -> history.currentFen
            }.toMap
            rootRuleAssessment match {
            case PositionRuleAssessment.Nonterminal(
                  FivefoldRepetitionKnowledge.CertifiedNonterminal,
                  threefoldClaim,
                  fiftyMoveClaim
                ) =>
              val rankedAssessments =
                legalMoveKeys
                  .zipWithIndex
                  .map { case (moveUci, legalMoveIndex) =>
                    (moveUci, legalMoveIndex, evaluationsByMove(moveUci).head)
                  }
                  .sortBy { case (_, legalMoveIndex, evaluation) =>
                    (-evaluation.rankingScoreForMover(rootPosition.color), legalMoveIndex)
                  }
                  .zipWithIndex
                  .map { case ((moveUci, _, evaluation), rankIndex) =>
                    RankedCandidateLineEvaluation(rankIndex + 1, moveUci, evaluation)
                  }
              val completeCandidateSet =
                CandidateSetDescriptor.fromCompleteRanking(rootPosition.color, rankedAssessments)
              val declaredMovesByRule = declaredDrawClaims.groupMap(_.rule)(claim => normalizeUci(claim.moveUci))
              val drawClaims = DrawClaimRule.values.toList.flatMap { rule =>
                val availableNow = rule match
                  case DrawClaimRule.ThreefoldRepetition =>
                    threefoldClaim == ThreefoldClaimKnowledge.Known(DrawClaimAvailability.AvailableNow)
                  case DrawClaimRule.FiftyMoveRule =>
                    fiftyMoveClaim == DrawClaimAvailability.AvailableNow
                if availableNow then Some(DrawClaimAction.AvailableNow(rule))
                else
                  declaredMovesByRule
                    .get(rule)
                    .filter(_.nonEmpty)
                    .map(moves => DrawClaimAction.AvailableByDeclaredMoves(rule, moves))
              }
              val drawClaimStrictlyDominates =
                drawClaims.nonEmpty &&
                  rankedAssessments.head.evaluation.winPercentForMover(rootPosition.color) < 50.0
              if !rankedAssessments.forall(_.evaluation.comparisonReady) then None
              else if drawClaimStrictlyDominates then
                Some(
                  PreparedPositionCommentary.PositionAction(
                    PlayerFacingPositionAction.DominantDrawClaim(drawClaims)
                  )
                )
              else if rankedAssessments.size == 1 then
                val forced = rankedAssessments.head
                Some(
                  PreparedPositionCommentary.PositionAction(
                    PlayerFacingPositionAction.ForcedSingleMove(
                      forced.rootMoveUci,
                      rootPosition.color,
                      forced.evaluation,
                      drawClaims
                    )
                  )
                )
              else
                completeCandidateSet.flatMap { candidateSet =>
                  val rankedByRootMove = rankedAssessments.map(item => item.rootMoveUci -> item).toMap
                  val best = rankedAssessments.head
                  val openingContext = openingContextFor(
                    positionHistory,
                    OpeningRecognitionIndex.default,
                    OpeningThemePriorIndex.default
                  )
                  val materializedRecords = orderedLegalMoves.zipWithIndex.map { case (legalMove, index) =>
                    val legalMoveKey = normalizeUci(legalMove)
                    val played = rankedByRootMove(legalMoveKey)
                    val alternative = rankedAssessments.find(item =>
                      item.rootMoveUci != best.rootMoveUci && item.rootMoveUci != played.rootMoveUci
                    )
                    for
                      afterPlayedFen <- successorFensByMove.get(legalMoveKey)
                      afterReferenceFen <- successorFensByMove.get(best.rootMoveUci)
                    yield
                      val rootLines = List(
                        NormalizedCandidateLine(LineNodeRole.BestReference, best.rank, best.evaluation),
                        NormalizedCandidateLine(LineNodeRole.Played, played.rank, played.evaluation)
                      ) ++ alternative.map(ranked =>
                        NormalizedCandidateLine(LineNodeRole.Alternative, ranked.rank, ranked.evaluation)
                      )
                      PreparedMoveReview.materialized(
                        legalMoveIndex = index,
                        playedMoveUci = legalMoveKey,
                        preparedInput = NormalizedMoveReviewInput(
                          beforeFen = canonicalRootFen,
                          playedMoveUci = legalMoveKey,
                          beforePly = positionHistory.currentPly,
                          sideToMove = Some(rootPosition.color),
                          afterPlayedFen = afterPlayedFen,
                          afterReferenceFen = Some(afterReferenceFen),
                          lines = rootLines,
                          completeCandidateSet = Some(candidateSet),
                          positionHistory = positionHistory,
                          openingContext = openingContext
                        )
                      )
                  }
                  Option.when(materializedRecords.forall(_.nonEmpty))(
                    PreparedPositionCommentary.MoveChoices(
                      PreparedMoveReviewInventory.materialized(
                        reviewRecords = materializedRecords.flatten,
                        drawClaims = drawClaims
                      )
                    )
                  )
                }
            case PositionRuleAssessment.Terminal(_) |
                PositionRuleAssessment.Nonterminal(
                  FivefoldRepetitionKnowledge.HistoryUnavailable,
                  _,
                  _
                ) =>
              None
            }

  /**
   * Adds branch-only evidence to an already prepared root comparison. This
   * deliberately does not revisit root ranking or root-line selection.
   */
  def withAdmittedProbeResults(
      preparedInput: NormalizedMoveReviewInput,
      fulfilledProbeResults: List[(ProbeRequest, ProbeResult)]
  ): NormalizedMoveReviewInput =
    val rootLines = preparedInput.lines.filterNot(_.role == LineNodeRole.Threat)
    val branchNormalization = normalizeThreatBranches(
      fulfilledProbeResults.map(_._2),
      beforeFen = preparedInput.beforeFen,
      rootLines = rootLines,
      firstThreatRank = rootLines.map(_.rank).maxOption.getOrElse(0) + 1,
      positionHistory = preparedInput.positionHistory
    )
    preparedInput.copy(
      threatBranches = branchNormalization.branches,
      probeDiagnostics = preparedInput.probeDiagnostics ++ branchNormalization.diagnostics
    )

  def normalize(
      raw: RawMoveReviewInput,
      recognitionIndex: OpeningRecognitionIndex = OpeningRecognitionIndex.default,
      themePriorIndex: OpeningThemePriorIndex = OpeningThemePriorIndex.default
  ): Option[NormalizedMoveReviewInput] =
    val beforeFen = normalizeFen(raw.fen)
    val playedMove = normalizeUci(raw.playedMoveUci)
    val movePrefix = raw.movePrefixUci.map(normalizeUci).filter(_.nonEmpty)
    val positionHistory =
      if movePrefix.nonEmpty then CanonicalPositionHistory.from(Standard.initialFen.value, movePrefix, beforeFen)
      else CanonicalPositionHistory.from(beforeFen, Nil, beforeFen)
    for
      history <- positionHistory.toOption
      _ <- Option.when(raw.ply.forall(_ == history.currentPly))(())
      afterPlayedHistory <- history.extend(List(playedMove)).toOption
      input <- {
      val canonicalBeforeFen = history.currentFen
      val afterPlayed = afterPlayedHistory.currentFen
      val beforePly = history.currentPly
      val side = Some(history.currentPosition.color)
      val openingContext = openingContextFor(history, recognitionIndex, themePriorIndex)
      val ranked = preferredRankedLines(canonicalBeforeFen, raw.variations.zipWithIndex.filter(_._1.moves.nonEmpty), side)
      val reference = ranked.headOption.map { case (line, index) =>
        NormalizedCandidateLine(
          LineNodeRole.BestReference,
          index + 1,
          CandidateLineEvaluation.EngineSearch(normalizedLine(line))
        )
      }
      val played =
        ranked
          .find { case (line, _) => line.moves.headOption.exists(move => sameRootMove(canonicalBeforeFen, move, playedMove)) }
          .map { case (line, index) =>
            NormalizedCandidateLine(
              LineNodeRole.Played,
              index + 1,
              CandidateLineEvaluation.EngineSearch(normalizedLine(line))
            )
          }
          .filter(line => line.evaluation.moves.size >= 2 || positionHasNoLegalReply(afterPlayed))
      val alternatives =
        ranked
          .filterNot { case (_, index) =>
            reference.exists(_.rank == index + 1) || played.exists(_.rank == index + 1)
          }
          .map { case (line, index) =>
            NormalizedCandidateLine(
              LineNodeRole.Alternative,
              index + 1,
              CandidateLineEvaluation.EngineSearch(normalizedLine(line))
            )
          }
      val lines = (reference.toList ++ played.toList ++ alternatives).distinctBy(line => line.role -> line.rank)
      played.flatMap { playedLine =>
        reference.flatMap { _ =>
          val graphPlayedMove = playedLine.rootMove.getOrElse(playedMove)
          for
            graphAfterPlayed <- history.extend(List(graphPlayedMove)).toOption.map(_.currentFen)
            afterReference = reference.flatMap(_.rootMove).flatMap(move => history.extend(List(move)).toOption.map(_.currentFen))
            threatBranchNormalization =
              normalizeThreatBranches(
                raw.probeResults,
                beforeFen = canonicalBeforeFen,
                rootLines = lines,
                firstThreatRank = lines.map(_.rank).maxOption.getOrElse(0) + 1,
                positionHistory = history
              )
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
            threatBranches = threatBranchNormalization.branches,
            probeDiagnostics = threatBranchNormalization.diagnostics
          )
        }
      }
      }
    yield input

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

  def normalizeUci(uci: String): String =
    PrincipalVariationEvidence.normalizeUci(uci)

  private def normalizedLine(line: EngineLine): EngineLine =
    line.copy(moves = line.moves.map(normalizeUci))

  private def preferredRankedLines(
      startFen: String,
      lines: List[(EngineLine, Int)],
      sideToMove: Option[Color],
      requireTerminalScoreConsistency: Boolean = true
  ): List[(EngineLine, Int)] =
    val selectedLines = lines
      .groupBy { case (line, _) => line.moves.headOption.map(rootMoveKey(startFen, _)).getOrElse("") }
      .values
      .flatMap { entries =>
        val earliestRank = entries.map(_._2).min
        entries
          .filter { case (line, _) =>
            legalLine(startFen, line) &&
              (!requireTerminalScoreConsistency || terminalScoreConsistent(startFen, line))
          }
          .maxByOption { case (line, index) =>
            (line.depth, line.moves.size, line.mate.fold(0)(mate => 1000 - mate.abs), -index)
          }
          .map((line, _) => line -> earliestRank)
      }
      .toList
    val ordered = sideToMove match
      case Some(side) =>
        selectedLines.sortBy { case (line, originalRank) =>
          (-scoreForMover(side, line), originalRank)
        }
      case None =>
        selectedLines.sortBy(_._2)
      ordered.map(_._1).zipWithIndex

  private def preferredRankedEvaluations(
      startFen: String,
      evaluations: List[CandidateLineEvaluation],
      sideToMove: Option[Color]
  ): List[CandidateLineEvaluation] =
    val selectedEvaluations = evaluations.zipWithIndex
      .groupBy { case (evaluation, _) =>
        evaluation.moves.headOption.map(rootMoveKey(startFen, _)).getOrElse("")
      }
      .values
      .flatMap { entries =>
        val earliestRank = entries.map(_._2).min
        entries
          .filter { case (evaluation, _) =>
            evaluation.moves.nonEmpty &&
              PrincipalVariationEvidence
                .legalMoveReplay(startFen, evaluation.moves.map(normalizeUci), startPly = 0)
                .nonEmpty
          }
          .maxByOption { case (evaluation, index) =>
            evaluation match
              case CandidateLineEvaluation.EngineSearch(line) =>
                (0, line.depth, line.moves.size, line.mate.fold(0)(mate => 1000 - mate.abs), -index)
              case CandidateLineEvaluation.ExactAutomaticTerminal(moves, _) =>
                (1, 0, moves.size, 0, -index)
          }
          .map((evaluation, _) => evaluation -> earliestRank)
      }
      .toList
    val ordered = sideToMove match
      case Some(side) =>
        selectedEvaluations.sortBy { case (evaluation, originalRank) =>
          (-evaluation.rankingScoreForMover(side), originalRank)
        }
      case None => selectedEvaluations.sortBy(_._2)
    ordered.map(_._1)

  private final case class ThreatBranchSeed(
      sourceProbeId: String,
      probedMoveUci: String,
      branchFen: String,
      purpose: ProbePurpose,
      evaluations: List[CandidateLineEvaluation],
      variationHash: Option[String],
      opponentResourceMove: Option[String],
      certifiedHorizonPlyOffset: Option[Int]
  )

  private final case class ThreatBranchNormalization(
      branches: List[NormalizedThreatBranch],
      diagnostics: List[ProbeAdmissionDiagnostic]
  )

  private def normalizeThreatBranches(
      probes: List[ProbeResult],
      beforeFen: String,
      rootLines: List[NormalizedCandidateLine],
      firstThreatRank: Int,
      positionHistory: CanonicalPositionHistory
  ): ThreatBranchNormalization =
    val branches = List.newBuilder[NormalizedThreatBranch]
    val diagnostics = List.newBuilder[ProbeAdmissionDiagnostic]
    var nextRank = firstThreatRank
    probes.foreach { probe =>
      threatBranchSeed(beforeFen, rootLines, probe) match
        case Left(diagnostic) =>
          diagnostics += diagnostic
        case Right(seed) =>
          val side = sideToMove(seed.branchFen)
          val depthFloor = BranchReplyProbeBinding.DepthFloor
          val ranked = preferredRankedEvaluations(seed.branchFen, seed.evaluations, side)
          val legalLines = ranked.filter(evaluation =>
            evaluation.moves.nonEmpty &&
              PrincipalVariationEvidence.legalMoveReplay(
                seed.branchFen,
                evaluation.moves.map(normalizeUci),
                startPly = 0
              ).nonEmpty
          )
          val requiredReplyCount =
            if seed.opponentResourceMove.nonEmpty then 1
            else BranchReplyProbeBinding.requiredReplyCount(seed.branchFen)
          val topLegalLines = legalLines.take(requiredReplyCount)
          val topLinesDepthReady = topLegalLines.forall {
            case CandidateLineEvaluation.EngineSearch(line) => line.depth >= depthFloor
            case CandidateLineEvaluation.ExactAutomaticTerminal(_, _) => true
          }
          val topLinesTerminalScoreConsistent = topLegalLines.forall {
            case CandidateLineEvaluation.EngineSearch(line) => terminalScoreConsistent(seed.branchFen, line)
            case CandidateLineEvaluation.ExactAutomaticTerminal(moves, terminal) =>
              positionHistory
                .extend(List(seed.probedMoveUci))
                .toOption
                .filter(_.currentFen == seed.branchFen)
                .flatMap(history => history.extend(moves).toOption)
                .map(PositionRuleAssessment.assess)
                .contains(PositionRuleAssessment.Terminal(terminal))
          }
          val opponentResourceLineBound = seed.opponentResourceMove.forall(resource =>
            topLegalLines.size == 1 && topLegalLines.head.moves.headOption.exists(EvidenceRef.sameMove(_, resource))
          )
          if requiredReplyCount > 0 &&
            topLegalLines.size == requiredReplyCount &&
            topLinesDepthReady &&
            topLinesTerminalScoreConsistent &&
            opponentResourceLineBound
          then
            val branchLines = topLegalLines.map { evaluation =>
              val rank = nextRank
              nextRank += 1
              NormalizedCandidateLine(
                role = LineNodeRole.Threat,
                rank = rank,
                evaluation = evaluation match
                  case CandidateLineEvaluation.EngineSearch(line) =>
                    CandidateLineEvaluation.EngineSearch(normalizedLine(line))
                  case CandidateLineEvaluation.ExactAutomaticTerminal(moves, terminal) =>
                    CandidateLineEvaluation.ExactAutomaticTerminal(moves, terminal)
              )
            }
            branches += NormalizedThreatBranch(
              sourceProbeId = seed.sourceProbeId,
              probedMoveUci = seed.probedMoveUci,
              branchFen = seed.branchFen,
              branchPly = plyFromFen(seed.branchFen),
              opponentResourceMove = seed.opponentResourceMove,
              certifiedHorizonPlyOffset = seed.certifiedHorizonPlyOffset,
              lines = branchLines
            )
            diagnostics += probeDiagnostic(
              probe,
              status = ProbeAdmissionStatus.Admitted,
              reasons = List("ADMITTED"),
              purpose = Some(seed.purpose),
              candidateMove = Some(seed.probedMoveUci),
              branchFen = Some(seed.branchFen),
              admittedLineCount = branchLines.size,
              legalLineCount = legalLines.size,
              scoredLineCount = seed.evaluations.count(_.moves.nonEmpty),
              depthFloor = Some(depthFloor),
              variationHash = seed.variationHash
            )
          else
            val reason =
              if requiredReplyCount == 0 then "BRANCH_POSITION_TERMINAL"
              else if legalLines.size < requiredReplyCount then "INSUFFICIENT_LEGAL_REPLY_LINES"
              else if !opponentResourceLineBound then "OPPONENT_RESOURCE_LINE_UNBOUND"
              else if !topLinesTerminalScoreConsistent then "REPLY_LINE_TERMINAL_SCORE_INCONSISTENT"
              else "REPLY_LINE_DEPTH_FLOOR_UNMET"
            diagnostics += probeDiagnostic(
              probe,
              status = ProbeAdmissionStatus.Rejected,
              reasons = List(reason),
              purpose = Some(seed.purpose),
              candidateMove = Some(seed.probedMoveUci),
              branchFen = Some(seed.branchFen),
              admittedLineCount = 0,
              legalLineCount = legalLines.size,
              scoredLineCount = seed.evaluations.count(_.moves.nonEmpty),
              depthFloor = Some(depthFloor),
              variationHash = seed.variationHash
            )
    }
    ThreatBranchNormalization(branches.result(), diagnostics.result())

  private def threatBranchSeed(
      beforeFen: String,
      rootLines: List[NormalizedCandidateLine],
      probe: ProbeResult
  ): Either[ProbeAdmissionDiagnostic, ThreatBranchSeed] =
    probe.purpose.filter(ProbePurpose.isBranchReply) match
      case None =>
        Left(
          probeDiagnostic(
            probe,
            status = ProbeAdmissionStatus.Ignored,
            reasons = List("UNSUPPORTED_BRANCH_PURPOSE"),
            purpose = probe.purpose,
            candidateMove = probe.probedMove.orElse(probe.candidateMove).map(normalizeUci).filter(_.nonEmpty),
            branchFen = probe.fen.map(normalizeFen).filter(_.nonEmpty),
            admittedLineCount = 0,
            legalLineCount = 0,
            scoredLineCount = 0,
            depthFloor = None,
            variationHash = probe.variationHash
          )
        )
      case Some(purpose) =>
        val branchFen = probe.fen.map(normalizeFen).filter(_.nonEmpty)
        val evaluations = probe.resolution match
          case ProbeResolution.EngineSearch(values, _) => values
          case ProbeResolution.ExactAutomaticTerminal(evaluation) => List(evaluation)
        val probedMove = probe.probedMove.orElse(probe.candidateMove).map(normalizeUci).filter(_.nonEmpty)
        val opponentResourceMove = probe.opponentResourceMove.map(normalizeUci).filter(_.nonEmpty)
        val rawHorizon = probe.horizon.filter(_.nonEmpty)
        val certifiedHorizonPlyOffset = rawHorizon.flatMap(BranchReplyProbeBinding.horizonPlyOffset)
        val horizonError =
          if opponentResourceMove.nonEmpty then Option.when(rawHorizon.nonEmpty)("HORIZON_UNEXPECTED")
          else if rawHorizon.isEmpty then Some("HORIZON_MISSING")
          else Option.when(certifiedHorizonPlyOffset.isEmpty)("HORIZON_INVALID")
        val declaresCounterResource = probe.objective.contains(CounterResourceProbeBinding.Objective)
        val counterResourceObjectiveMismatch =
          opponentResourceMove.nonEmpty && probe.objective.exists(_ != CounterResourceProbeBinding.Objective)
        val baseDiagnostic =
          (reasons: List[String]) =>
            probeDiagnostic(
              probe,
              status = ProbeAdmissionStatus.Rejected,
              reasons = reasons,
              purpose = Some(purpose),
              candidateMove = probedMove,
              branchFen = branchFen,
              admittedLineCount = 0,
              legalLineCount = 0,
              scoredLineCount = evaluations.count(_.moves.nonEmpty),
              depthFloor = Some(BranchReplyProbeBinding.DepthFloor),
              variationHash = probe.variationHash
            )
        if declaresCounterResource && opponentResourceMove.isEmpty then
          Left(baseDiagnostic(List("OPPONENT_RESOURCE_MISSING")))
        else if counterResourceObjectiveMismatch then
          Left(baseDiagnostic(List("OPPONENT_RESOURCE_OBJECTIVE_MISMATCH")))
        else if horizonError.nonEmpty then
          Left(baseDiagnostic(horizonError.toList))
        else (branchFen, probedMove) match
          case (None, _) =>
            Left(baseDiagnostic(List("BRANCH_FEN_MISSING")))
          case (_, None) =>
            Left(baseDiagnostic(List("PROBED_MOVE_MISSING")))
          case (Some(fen), Some(move)) =>
            PrincipalVariationEvidence.legalFenAfter(beforeFen, move) match
              case Some(expectedBranchFen) if expectedBranchFen == fen =>
                val matchingMoveLines =
                  rootLines
                    .filterNot(_.role == LineNodeRole.Threat)
                    .filter(line => line.rootMove.exists(rootMove => sameRootMove(beforeFen, rootMove, move)))
                if matchingMoveLines.isEmpty then
                  Left(baseDiagnostic(List("ROOT_LINE_UNBOUND")))
                else if probe.variationHash.forall(_.trim.isEmpty) then
                  Left(baseDiagnostic(List("VARIATION_HASH_MISSING")))
                else
                  matchingMoveLines.flatMap(line =>
                    branchHash(beforeFen, line, move, opponentResourceMove, certifiedHorizonPlyOffset)
                      .filter { case (_, hash) => probe.variationHash.contains(hash) }
                      .map { case (engineLine, hash) => engineLine -> hash }
                  ).headOption match
                    case None =>
                      Left(baseDiagnostic(List("VARIATION_HASH_MISMATCH")))
                    case Some((rootEngineLine, expectedHash)) =>
                      val contract = branchProbeContract(
                        probe,
                        purpose,
                        expectedBranchFen,
                        move,
                        rootEngineLine,
                        expectedHash,
                        opponentResourceMove,
                        certifiedHorizonPlyOffset
                      )
                      if contract.isValid then
                        Right(
                          ThreatBranchSeed(
                            sourceProbeId = probe.id,
                            probedMoveUci = move,
                            branchFen = expectedBranchFen,
                            purpose = purpose,
                            evaluations = evaluations,
                            variationHash = probe.variationHash,
                            opponentResourceMove = opponentResourceMove,
                            certifiedHorizonPlyOffset = certifiedHorizonPlyOffset
                          )
                        )
                      else
                        Left(baseDiagnostic(("CONTRACT_INVALID" :: contract.reasonCodes).distinct))
              case Some(_) =>
                Left(baseDiagnostic(List("BRANCH_FEN_MISMATCH")))
              case None =>
                Left(baseDiagnostic(List("PROBED_MOVE_ILLEGAL_FROM_ROOT")))

  private def branchHash(
      beforeFen: String,
      line: NormalizedCandidateLine,
      probedMove: String,
      opponentResourceMove: Option[String],
      certifiedHorizonPlyOffset: Option[Int]
  ): Option[(EngineLine, String)] =
    line.evaluation.engineLine.flatMap { engineLine =>
      val hash = opponentResourceMove match
        case Some(resourceMove) =>
          Some(
            CounterResourceProbeBinding.variationHash(
              rootFen = beforeFen,
              role = line.role,
              rootMove = probedMove,
              whitePovEvalCp = engineLine.scoreCp,
              mate = engineLine.mate,
              depth = engineLine.depth,
              moves = engineLine.moves,
              opponentResourceMove = resourceMove
            )
          )
        case None =>
          certifiedHorizonPlyOffset.map { horizonPlyOffset =>
            BranchReplyProbeBinding.variationHash(
              rootFen = beforeFen,
              role = line.role,
              rootMove = probedMove,
              whitePovEvalCp = engineLine.scoreCp,
              mate = engineLine.mate,
              depth = engineLine.depth,
              moves = engineLine.moves,
              certifiedHorizonPlyOffset = horizonPlyOffset
            )
          }
      hash.map(engineLine -> _)
    }

  private def branchProbeContract(
      probe: ProbeResult,
      purpose: ProbePurpose,
      branchFen: String,
      probedMove: String,
      rootEngineLine: EngineLine,
      expectedHash: String,
      opponentResourceMove: Option[String],
      certifiedHorizonPlyOffset: Option[Int]
  ): ProbeContractValidator.ValidationResult =
    val counterResource = opponentResourceMove.nonEmpty
    val request =
      ProbeRequest(
        id = probe.id,
        fen = branchFen,
        moves = opponentResourceMove.toList,
        depth = BranchReplyProbeBinding.Depth,
        purpose = Some(purpose),
        multiPv = Some(if counterResource then 1 else BranchReplyProbeBinding.requiredReplyCount(branchFen)),
        objective = Some(
          if counterResource then CounterResourceProbeBinding.Objective
          else BranchReplyProbeBinding.Objective
        ),
        requiredSignals =
          if counterResource then CounterResourceProbeBinding.RequiredSignals
          else BranchReplyProbeBinding.RequiredSignals,
        horizon = certifiedHorizonPlyOffset.map(BranchReplyProbeBinding.horizon),
        candidateMove = Some(probedMove),
        opponentResourceMove = opponentResourceMove,
        depthFloor = Some(BranchReplyProbeBinding.DepthFloor),
        variationHash = Some(expectedHash)
      )
    ProbeContractValidator.validateAgainstRequest(request, probe)

  private def probeDiagnostic(
      probe: ProbeResult,
      status: ProbeAdmissionStatus,
      reasons: List[String],
      purpose: Option[ProbePurpose],
      candidateMove: Option[String],
      branchFen: Option[String],
      admittedLineCount: Int,
      legalLineCount: Int,
      scoredLineCount: Int,
      depthFloor: Option[Int],
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
      horizon = probe.horizon,
      variationHash = variationHash
    )

  private def scoreForMover(side: Color, line: EngineLine): Double =
    val outcome = PerspectiveMath.winPercentForMover(side, line.scoreCp, line.mate)
    val mateTieBreak =
      PerspectiveMath
        .mateForMover(side, line.mate)
        .map(mate => math.signum(mate).toDouble / mate.abs.max(1).toDouble)
        .getOrElse(0.0)
    outcome + mateTieBreak

  private def legalLine(startFen: String, line: EngineLine): Boolean =
    line.moves.nonEmpty &&
      PrincipalVariationEvidence
        .legalMoveReplay(startFen, line.moves.map(normalizeUci), startPly = 0)
        .nonEmpty

  private def positionHasNoLegalReply(fen: String): Boolean =
    Fen.read(Standard, Fen.Full(fen)).exists(_.legalMoves.isEmpty)

  private def terminalScoreConsistent(startFen: String, line: EngineLine): Boolean =
    if line.mate.contains(0) then false
    else
      PrincipalVariationEvidence
        .legalReplay(startFen, line.moves.map(normalizeUci), 0)
        .flatMap(_.lastOption)
        .flatMap((_, move) => Fen.read(Standard, Fen.Full(move.fenAfter)))
        .forall { terminal =>
          if terminal.staleMate then line.mate.isEmpty && line.scoreCp.abs <= 1
          else if terminal.checkMate then
            line.mate.exists(mate =>
              val whiteWon = terminal.color.black
              (mate > 0) == whiteWon
            )
          else true
        }

  private def rootMoveKey(startFen: String, move: String): String =
    PrincipalVariationEvidence.legalFenAfter(startFen, normalizeUci(move)).getOrElse(normalizeUci(move))

  private def sameRootMove(startFen: String, left: String, right: String): Boolean =
    val normalizedLeft = normalizeUci(left)
    val normalizedRight = normalizeUci(right)
    normalizedLeft == normalizedRight ||
      (
        normalizedLeft.nonEmpty &&
          normalizedRight.nonEmpty &&
          PrincipalVariationEvidence.legalFenAfter(startFen, normalizedLeft)
            .exists(after => PrincipalVariationEvidence.legalFenAfter(startFen, normalizedRight).contains(after))
      )

  private def normalizeFen(fen: String): String =
    Option(fen).getOrElse("").trim.split("\\s+").filter(_.nonEmpty).mkString(" ")

  private def certifyingRecognitionLineage(recognition: Option[OpeningRecognition]): Option[String] =
    recognition.filter(certifyingRecognition).flatMap(_.lineage)

  private def certifyingRecognition(recognition: OpeningRecognition): Boolean =
    // A unique canonical position keeps the same opening identity across legal move-order transpositions.
    recognition.confidence >= CertifyingRecognitionMinConfidence &&
      recognition.candidates.size == 1

  private def sideToMove(fen: String): Option[Color] =
    fen.split("\\s+").lift(1).flatMap:
      case "w" => Some(Color.White)
      case "b" => Some(Color.Black)
      case _   => None

  private def plyFromFen(fen: String): Int =
    val parts = fen.split("\\s+")
    val fullMove = parts.lift(5).flatMap(_.toIntOption).getOrElse(1).max(1)
    val blackToMove = parts.lift(1).contains("b")
    (fullMove - 1) * 2 + Option.when(blackToMove)(1).getOrElse(0)

  private val CertifyingRecognitionMinConfidence: Double = 0.7
