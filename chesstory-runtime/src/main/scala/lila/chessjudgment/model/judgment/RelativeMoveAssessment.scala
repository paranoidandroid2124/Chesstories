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
    referenceSelection: SemanticLineKey,
    candidateSelection: SemanticLineKey,
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
      referenceSelection = SemanticLineKey.from(reference),
      candidateSelection = SemanticLineKey.from(candidate),
      comparison = comparison,
      verdictConfidence = verdictConfidence
    )

/** Evidence-id-independent identity of one quantitative Assessment comparison. */
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
      referenceLine = comparison.referenceSelection,
      candidateLine = comparison.candidateSelection,
      candidateWinPercentDeltaForMover = comparison.comparison.candidateWinPercentDeltaForMover,
      verdict = comparison.comparison.verdict,
      verdictConfidence = comparison.verdictConfidence
    )

case class RelativeMoveAssessment(
    played: MoveTransitionEdge,
    referenceTransition: Option[MoveTransitionEdge],
    reference: CandidateLineNode,
    candidate: CandidateLineNode,
    evidence: EvidenceRef,
    primaryComparisonEvidence: EvidenceRef
):
  val confidence: EvidenceConfidence = evidence.confidence

  private[chessjudgment] def canonicalParents: Option[List[EvidenceRef]] =
    val lineParents =
      if reference == candidate then List(reference.lineEvidence)
      else List(reference.lineEvidence, candidate.lineEvidence)
    val parents =
      List(played.evidence) ++
        lineParents ++
        referenceTransition.toList.map(_.evidence) ++
        List(primaryComparisonEvidence)
    Option.when(parents.map(_.id).distinct.size == parents.size)(parents)
