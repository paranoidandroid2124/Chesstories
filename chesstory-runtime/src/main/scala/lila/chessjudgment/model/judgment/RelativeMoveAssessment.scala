package lila.chessjudgment.model.judgment

import chess.Color
import lila.chessjudgment.model.evaluation.{ JudgmentThresholds, PerspectiveMath, VerdictThresholdPolicy }
import lila.chessjudgment.model.line.CandidateLineEvaluation

enum MoveChoiceVerdict:
  case ImprovesOnReference
  case MatchesReference
  case PlayableLoss
  case Inaccuracy
  case Mistake
  case Blunder

  def isActionableLoss: Boolean =
    this match
      case Inaccuracy | Mistake | Blunder => true
      case _                              => false

final case class AdmittedRootRankingPair private[chessjudgment] (
    bestMoveUci: String,
    secondMoveUci: String
)

object AdmittedRootRankingPair:
  def fromAdmittedRanking(
      rootMovesByRank: List[String]
  ): Option[AdmittedRootRankingPair] =
    rootMovesByRank match
      case best :: second :: _ =>
        val bestMove = EvidenceRef.normalizeMove(best)
        val secondMove = EvidenceRef.normalizeMove(second)
        Option.when(
          bestMove.nonEmpty && secondMove.nonEmpty && !EvidenceRef.sameMove(bestMove, secondMove)
        )(
          AdmittedRootRankingPair(
            bestMoveUci = bestMove,
            secondMoveUci = secondMove
          )
        )
      case _ =>
        None

enum CandidateComparisonKind:
  case PlayedVsBest
  case BestVsSecond
  case PlayedVsAlternative
  case ReferenceVsAlternative

enum RelativeCauseSourceSide:
  case Reference
  case Candidate

object RelativeCauseSourceSide:
  def eventLine(
      sourceSide: RelativeCauseSourceSide,
      referenceLine: LineNodeRef,
      candidateLine: LineNodeRef
  ): LineNodeRef =
    sourceSide match
      case RelativeCauseSourceSide.Reference =>
        referenceLine
      case RelativeCauseSourceSide.Candidate =>
        candidateLine

enum VerdictConfidence:
  case EngineBacked
  case LegalReplayVerified
  case MixedVerified

  def evidenceConfidence: EvidenceConfidence = this match
    case EngineBacked        => EvidenceConfidence.EngineBacked
    case LegalReplayVerified => EvidenceConfidence.LegalReplayVerified
    case MixedVerified       => EvidenceConfidence.Mixed

object VerdictConfidence:
  private[judgment] def fromEvaluations(
      reference: CandidateLineEvaluation,
      candidate: CandidateLineEvaluation
  ): Option[VerdictConfidence] =
    (reference, candidate) match
      case (CandidateLineEvaluation.EngineSearch(referenceLine), CandidateLineEvaluation.EngineSearch(candidateLine)) =>
        Option.when(
          JudgmentThresholds.engineBackedByDepth(referenceLine.depth, referenceLine.mate) &&
            JudgmentThresholds.engineBackedByDepth(candidateLine.depth, candidateLine.mate)
        )(VerdictConfidence.EngineBacked)
      case (
            CandidateLineEvaluation.ExactAutomaticTerminal(_, _),
            CandidateLineEvaluation.ExactAutomaticTerminal(_, _)
          ) =>
        Some(VerdictConfidence.LegalReplayVerified)
      case (CandidateLineEvaluation.EngineSearch(line), CandidateLineEvaluation.ExactAutomaticTerminal(_, _)) =>
        Option.when(JudgmentThresholds.engineBackedByDepth(line.depth, line.mate))(
          VerdictConfidence.MixedVerified
        )
      case (CandidateLineEvaluation.ExactAutomaticTerminal(_, _), CandidateLineEvaluation.EngineSearch(line)) =>
        Option.when(JudgmentThresholds.engineBackedByDepth(line.depth, line.mate))(
          VerdictConfidence.MixedVerified
        )

enum CandidateComparisonDeltaDetail:
  case EngineEvaluation(rawCpLossForMoverValue: Int, mateDistanceLossForMoverValue: Option[Int])
  case OutcomeOnly

  def rawCpLossForMover: Option[Int] = this match
    case EngineEvaluation(value, _) => Some(value)
    case OutcomeOnly                => None

  def mateDistanceLossForMover: Option[Int] = this match
    case EngineEvaluation(_, value) => value
    case OutcomeOnly                => None

final case class EvalComparison private[judgment] (
    mover: Color,
    candidateWinPercentDeltaForMover: Double,
    verdict: MoveChoiceVerdict,
    detail: CandidateComparisonDeltaDetail
):
  def winPercentLossForMover: Double =
    (-candidateWinPercentDeltaForMover).max(0.0)

object EvalComparison:
  def fromLines(
      mover: Color,
      reference: CandidateLineNode,
      candidate: CandidateLineNode
  ): Option[EvalComparison] =
    VerdictConfidence.fromEvaluations(reference.evaluation, candidate.evaluation).map { _ =>
      val (candidateWinPercentDeltaForMover, detail) =
        (reference.evaluation, candidate.evaluation) match
          case (CandidateLineEvaluation.EngineSearch(referenceLine), CandidateLineEvaluation.EngineSearch(candidateLine)) =>
            val delta =
              PerspectiveMath.compareForMover(
                mover = mover,
                reference = PerspectiveMath.EvalPoint(referenceLine.scoreCp, referenceLine.mate),
                candidate = PerspectiveMath.EvalPoint(candidateLine.scoreCp, candidateLine.mate)
              )
            delta.candidateWinPercentDeltaForMover -> CandidateComparisonDeltaDetail.EngineEvaluation(
              delta.rawCpLossForMover,
              delta.mateDistanceLossForMover
            )
          case _ =>
            (
              candidate.evaluation.winPercentForMover(mover) - reference.evaluation.winPercentForMover(mover)
            ) -> CandidateComparisonDeltaDetail.OutcomeOnly
      val winPercentLossForMover = (-candidateWinPercentDeltaForMover).max(0.0)
      EvalComparison(
        mover = mover,
        candidateWinPercentDeltaForMover = candidateWinPercentDeltaForMover,
        verdict = VerdictThresholdPolicy.verdictFromWinPercent(
          candidateWinPercentDeltaForMover,
          winPercentLossForMover,
          detail
        ),
        detail = detail
      )
    }

case class CandidateComparisonFact(
    kind: CandidateComparisonKind,
    referenceLine: LineNodeRef,
    candidateLine: LineNodeRef,
    comparison: EvalComparison,
    verdictConfidence: VerdictConfidence
):
  def hasDistinctRootMoves: Boolean =
    !EvidenceRef.sameMove(referenceLine.rootMove, candidateLine.rootMove)

object CandidateComparisonFact:
  def fromLines(
      kind: CandidateComparisonKind,
      mover: Color,
      reference: CandidateLineNode,
      candidate: CandidateLineNode
  ): Option[CandidateComparisonFact] =
    for
      verdictConfidence <- VerdictConfidence.fromEvaluations(reference.evaluation, candidate.evaluation)
      comparison <- EvalComparison.fromLines(mover, reference, candidate)
    yield CandidateComparisonFact(
      kind = kind,
      referenceLine = reference.ref,
      candidateLine = candidate.ref,
      comparison = comparison,
      verdictConfidence = verdictConfidence
    )

/** Evidence-id-independent identity of one evaluated comparison relation.
  * Both C canonicalization and cross-comparison exposure reuse this key.
  */
final case class CandidateComparisonSemanticKey(
    kind: CandidateComparisonKind,
    mover: Color,
    referenceLine: SemanticLineKey,
    candidateLine: SemanticLineKey,
    candidateWinPercentDeltaForMover: Double,
    verdict: MoveChoiceVerdict,
    verdictConfidence: VerdictConfidence
):
  def stableKey: String =
    List(
      kind.toString,
      mover.toString,
      referenceLine.stableKey,
      candidateLine.stableKey,
      java.lang.Double.toHexString(candidateWinPercentDeltaForMover),
      verdict.toString,
      verdictConfidence.toString
    ).mkString("|")

object CandidateComparisonSemanticKey:
  def from(comparison: CandidateComparisonFact): CandidateComparisonSemanticKey =
    CandidateComparisonSemanticKey(
      kind = comparison.kind,
      mover = comparison.comparison.mover,
      referenceLine = SemanticLineKey.from(comparison.referenceLine),
      candidateLine = SemanticLineKey.from(comparison.candidateLine),
      candidateWinPercentDeltaForMover = comparison.comparison.candidateWinPercentDeltaForMover,
      verdict = comparison.comparison.verdict,
      verdictConfidence = comparison.verdictConfidence
    )

enum RelativeCauseKind:
  case MissedTacticalResource
  case MissedSquareRelease
  case WrongMoveOrder
  case PassedPawnProgress

case class RelativeCauseFact(
    kind: RelativeCauseKind,
    comparisonEvidence: EvidenceRef,
    sourceSide: RelativeCauseSourceSide,
    proofSources: List[EvidenceRef]
):
  require(proofSources.nonEmpty, "a relative Cause requires exact typed proof sources")
  require(
    proofSources.map(_.id).distinct.size == proofSources.size,
    "a relative Cause must not repeat or collide proof-source owners"
  )
  require(
    proofSources == proofSources.sortBy(_.id),
    "relative Cause proof sources must use canonical evidence-id order"
  )

/** Evidence-id-independent causal frame used only with the complete typed
  * proof-occurrence list when checking producer ownership.
  */
final case class RelativeCauseSemanticFrameKey(
    kind: RelativeCauseKind,
    comparison: CandidateComparisonSemanticKey,
    eventLine: SemanticLineKey,
    sourceSide: RelativeCauseSourceSide
):
  def stableKey: String =
    List(
      kind.toString,
      comparison.stableKey,
      eventLine.stableKey,
      sourceSide.toString
    ).mkString("|")

final case class RelativeCauseSemanticKey(
    frame: RelativeCauseSemanticFrameKey,
    exactProofOccurrences: List[RootOwnedEffectChannelOccurrenceFingerprint]
):
  require(exactProofOccurrences.nonEmpty, "a relative Cause semantic key requires an admitted proof occurrence")
  require(
    exactProofOccurrences.map(_.stableSortKey) == exactProofOccurrences.map(_.stableSortKey).sorted &&
      exactProofOccurrences.distinct.size == exactProofOccurrences.size,
    "relative Cause proof occurrences must be unique and canonically ordered"
  )

  def kind: RelativeCauseKind = frame.kind
  def comparisonKind: CandidateComparisonKind = frame.comparison.kind
  def mover: Color = frame.comparison.mover
  def referenceLine: SemanticLineKey = frame.comparison.referenceLine
  def candidateLine: SemanticLineKey = frame.comparison.candidateLine
  def eventLine: SemanticLineKey = frame.eventLine
  def sourceSide: RelativeCauseSourceSide = frame.sourceSide

  def stableKey: String =
    List(
      frame.stableKey,
      exactProofOccurrences.map(_.stableSortKey).mkString(",")
    ).mkString("|")

object RelativeCauseSemanticKey:
  def from(
      cause: RelativeCauseFact,
      causeEvidenceId: String,
      graph: TypedEvidenceGraph
  ): RelativeCauseSemanticKey =
    val comparison = graph.comparisonFor(cause).getOrElse(
      throw IllegalArgumentException(
        s"relative cause '${cause.comparisonEvidence.id}' is not bound to a candidate comparison"
      )
    )
    val binding = graph.requiredRelativeCauseBinding(cause)
    val admittedChannels = RelativeCauseConstructionAdmission.admittedDirectChannels(cause, graph)
    require(
      RelativeCauseConstructionAdmission.initiallyReadyWithChannels(cause, graph, admittedChannels),
      s"relative cause '$causeEvidenceId' has no complete admitted typed proof construction"
    )
    val exactProofOccurrences = admittedChannels.map(_.exactOccurrenceFingerprint)
    require(
      exactProofOccurrences.distinct.size == exactProofOccurrences.size,
      s"relative cause '$causeEvidenceId' repeats one exact proof occurrence"
    )
    val orderedProofOccurrences = exactProofOccurrences.sortBy(_.stableSortKey)
    val frame = RelativeCauseSemanticFrameKey(
      kind = cause.kind,
      comparison = CandidateComparisonSemanticKey.from(comparison),
      eventLine = SemanticLineKey.from(binding.eventLine),
      sourceSide = cause.sourceSide
    )
    RelativeCauseSemanticKey(
      frame = frame,
      exactProofOccurrences = orderedProofOccurrences
    )

case class RelativeCauseBinding(
    sourceSide: RelativeCauseSourceSide,
    eventLine: LineNodeRef,
    evidenceLines: List[LineNodeRef]
)

object RelativeCauseFact:
  def binding(
      cause: RelativeCauseFact,
      comparison: CandidateComparisonFact
  ): RelativeCauseBinding =
    binding(
      comparisonKind = comparison.kind,
      referenceLine = comparison.referenceLine,
      candidateLine = comparison.candidateLine,
      sourceSide = cause.sourceSide
    )

  def binding(
      comparisonKind: CandidateComparisonKind,
      referenceLine: LineNodeRef,
      candidateLine: LineNodeRef,
      sourceSide: RelativeCauseSourceSide
  ): RelativeCauseBinding =
    require(
      comparisonKind == CandidateComparisonKind.PlayedVsBest,
      "a relative Cause may bind only an exact PlayedVsBest comparison"
    )
    val eventLine = RelativeCauseSourceSide.eventLine(sourceSide, referenceLine, candidateLine)
    RelativeCauseBinding(
      sourceSide = sourceSide,
      eventLine = eventLine,
      evidenceLines = List(eventLine)
    )

case class RelativeMoveAssessment(
    played: MoveTransitionEdge,
    referenceTransition: Option[MoveTransitionEdge],
    reference: CandidateLineNode,
    candidate: CandidateLineNode,
    evidence: EvidenceRef,
    primaryComparisonEvidence: EvidenceRef,
    relatedComparisonEvidence: List[EvidenceRef] = Nil,
    relativeCauseEvidence: List[EvidenceRef] = Nil
):
  val confidence: EvidenceConfidence = evidence.confidence
