package lila.chessjudgment.model.judgment

import chess.Color
import lila.chessjudgment.model.evaluation.{ JudgmentThresholds, PerspectiveMath, VerdictThresholdPolicy }
import lila.chessjudgment.model.line.{ CandidateLineEvaluation, RankedCandidateLineEvaluation }

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

enum CandidateSetType:
  case OnlyMove
  case NarrowChoice
  case StyleChoice

final case class CandidateSetDescriptor private[judgment] (
    candidateSetType: CandidateSetType
):
  def onlyMove: Boolean = candidateSetType == CandidateSetType.OnlyMove

final case class CompleteCandidateSet private[chessjudgment] (
    bestMoveUci: String,
    secondMoveUci: String,
    descriptor: CandidateSetDescriptor
)

object CandidateSetDescriptor:
  def fromCompleteRanking(
      mover: Color,
      ranked: List[RankedCandidateLineEvaluation]
  ): Option[CompleteCandidateSet] =
    val ordered = ranked.sortBy(_.rank)
    for
      best <- ordered.headOption
      second <- ordered.tail.headOption
    yield
      val topCandidates = ordered.take(3)
      val reliable =
        topCandidates.forall(_.evaluation.comparisonReady)
      val bestWinPercent = best.evaluation.winPercentForMover(mover)
      val gap = (bestWinPercent - second.evaluation.winPercentForMover(mover)).max(0.0)
      val spread =
        topCandidates
          .map(candidate => (bestWinPercent - candidate.evaluation.winPercentForMover(mover)).max(0.0))
          .max
      val candidateSetType =
        if reliable && gap >= JudgmentThresholds.ONLY_MOVE_GAP_WP then
          CandidateSetType.OnlyMove
        else if reliable && spread <= JudgmentThresholds.STYLE_CHOICE_SPREAD_WP then
          CandidateSetType.StyleChoice
        else CandidateSetType.NarrowChoice
      CompleteCandidateSet(
        bestMoveUci = EvidenceRef.normalizeMove(best.rootMoveUci),
        secondMoveUci = EvidenceRef.normalizeMove(second.rootMoveUci),
        descriptor = CandidateSetDescriptor(candidateSetType)
      )

enum CandidateComparisonKind:
  case PlayedVsBest
  case BestVsSecond
  case PlayedVsAlternative
  case ReferenceVsAlternative

enum RelativeCauseRole:
  case PrimaryPlayedCause
  case PlayedAlternativeContext
  case CandidateSetConstraint
  case AlternativeDiagnostic

  def defaultEventLine(referenceLine: LineNodeRef, candidateLine: LineNodeRef): LineNodeRef =
    this match
      case RelativeCauseRole.PrimaryPlayedCause | RelativeCauseRole.PlayedAlternativeContext =>
        candidateLine
      case RelativeCauseRole.CandidateSetConstraint =>
        referenceLine
      case RelativeCauseRole.AlternativeDiagnostic =>
        candidateLine

object RelativeCauseRole:
  def fromComparisonKind(kind: CandidateComparisonKind): RelativeCauseRole =
    kind match
      case CandidateComparisonKind.PlayedVsBest =>
        RelativeCauseRole.PrimaryPlayedCause
      case CandidateComparisonKind.PlayedVsAlternative =>
        RelativeCauseRole.PlayedAlternativeContext
      case CandidateComparisonKind.BestVsSecond =>
        RelativeCauseRole.CandidateSetConstraint
      case CandidateComparisonKind.ReferenceVsAlternative =>
        RelativeCauseRole.AlternativeDiagnostic

enum RelativeCauseSourceSide:
  case Reference
  case Candidate
  case Shared
  case Mixed

object RelativeCauseSourceSide:
  def eventLine(
      sourceSide: RelativeCauseSourceSide,
      role: RelativeCauseRole,
      referenceLine: LineNodeRef,
      candidateLine: LineNodeRef
  ): LineNodeRef =
    sourceSide match
      case RelativeCauseSourceSide.Reference =>
        referenceLine
      case RelativeCauseSourceSide.Candidate =>
        candidateLine
      case RelativeCauseSourceSide.Shared | RelativeCauseSourceSide.Mixed =>
        role.defaultEventLine(referenceLine, candidateLine)

  def fromSupportEvidence(
      referenceLine: LineNodeRef,
      candidateLine: LineNodeRef,
      supportEvidence: List[EvidenceRef]
  ): Option[RelativeCauseSourceSide] =
    if supportEvidence.isEmpty then
      Some(RelativeCauseSourceSide.Shared)
    else
      combine(
        supportEvidence.flatMap(ref => evidenceRefSourceSide(ref, referenceLine, candidateLine))
      )

  private def evidenceRefSourceSide(
      ref: EvidenceRef,
      referenceLine: LineNodeRef,
      candidateLine: LineNodeRef
  ): Option[RelativeCauseSourceSide] =
    ref.line match
      case Some(line) if line == referenceLine =>
        Some(RelativeCauseSourceSide.Reference)
      case Some(line) if line == candidateLine =>
        Some(RelativeCauseSourceSide.Candidate)
      case _ =>
        ref.scope match
          case EvidenceScope.BestLine | EvidenceScope.ReferenceTransition | EvidenceScope.AfterReferencePosition =>
            lineRoleSourceSide(LineNodeRole.BestReference, referenceLine, candidateLine)
          case EvidenceScope.PlayedLine | EvidenceScope.PlayedTransition | EvidenceScope.AfterPlayedPosition =>
            lineRoleSourceSide(LineNodeRole.Played, referenceLine, candidateLine)
          case EvidenceScope.CandidateLine | EvidenceScope.AlternativeTransition =>
            lineRoleSourceSide(LineNodeRole.Alternative, referenceLine, candidateLine)
          case EvidenceScope.BeforePosition | EvidenceScope.CurrentPosition =>
            Some(RelativeCauseSourceSide.Shared)
          case EvidenceScope.ThreatLine | EvidenceScope.Counterfactual =>
            None

  private def lineRoleSourceSide(
      role: LineNodeRole,
      referenceLine: LineNodeRef,
      candidateLine: LineNodeRef
  ): Option[RelativeCauseSourceSide] =
    if referenceLine.role == role then Some(RelativeCauseSourceSide.Reference)
    else if candidateLine.role == role then Some(RelativeCauseSourceSide.Candidate)
    else None

  private def combine(sides: List[RelativeCauseSourceSide]): Option[RelativeCauseSourceSide] =
    if sides.isEmpty then None
    else
      val concreteSides = sides.filter(side =>
        side == RelativeCauseSourceSide.Reference || side == RelativeCauseSourceSide.Candidate
      ).distinct
      val hasShared = sides.contains(RelativeCauseSourceSide.Shared)
      if concreteSides.size > 1 || sides.contains(RelativeCauseSourceSide.Mixed) || (concreteSides.nonEmpty && hasShared) then
        Some(RelativeCauseSourceSide.Mixed)
      else concreteSides.headOption.orElse(Option.when(hasShared)(RelativeCauseSourceSide.Shared))

enum RelativeCauseBindingTier:
  case Primary
  case Supporting
  case Context

enum CauseAttributionKind:
  case ReferenceCreatesResource
  case CandidateAllowsLiability
  case CandidateCreatesValue
  case SharedContext
  case ContextOnly
  case Unattributed

case class CauseAttribution(
    kind: CauseAttributionKind,
    rootMoveMatched: Boolean = false,
    directProofEligible: Boolean = false,
    reason: Option[String] = None
):
  def unattributed: Boolean =
    kind == CauseAttributionKind.Unattributed

object CauseAttribution:
  val unattributed: CauseAttribution =
    CauseAttribution(CauseAttributionKind.Unattributed, reason = Some("no-cause-attribution"))

object RelativeCauseBindingTier:
  def from(
      role: RelativeCauseRole,
      sourceSide: RelativeCauseSourceSide
  ): RelativeCauseBindingTier =
    role match
      case RelativeCauseRole.PrimaryPlayedCause
          if sourceSide == RelativeCauseSourceSide.Shared =>
        RelativeCauseBindingTier.Supporting
      case RelativeCauseRole.PrimaryPlayedCause =>
        RelativeCauseBindingTier.Primary
      case RelativeCauseRole.CandidateSetConstraint =>
        RelativeCauseBindingTier.Supporting
      case RelativeCauseRole.PlayedAlternativeContext | RelativeCauseRole.AlternativeDiagnostic =>
        RelativeCauseBindingTier.Context

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
      candidate: CandidateLineNode,
      candidateSetType: Option[CandidateSetType] = None
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
          detail,
          candidateSetType
        ),
        detail = detail
      )
    }

case class CandidateComparisonFact(
    kind: CandidateComparisonKind,
    referenceLine: LineNodeRef,
    candidateLine: LineNodeRef,
    comparison: EvalComparison,
    verdictConfidence: VerdictConfidence,
    candidateSet: Option[CandidateSetDescriptor] = None
):
  def hasDistinctRootMoves: Boolean =
    !EvidenceRef.sameMove(referenceLine.rootMove, candidateLine.rootMove)

  def candidateSetType: Option[CandidateSetType] =
    candidateSet.map(_.candidateSetType)

object CandidateComparisonFact:
  def fromLines(
      kind: CandidateComparisonKind,
      mover: Color,
      reference: CandidateLineNode,
      candidate: CandidateLineNode,
      candidateSet: Option[CandidateSetDescriptor] = None
  ): Option[CandidateComparisonFact] =
    for
      verdictConfidence <- VerdictConfidence.fromEvaluations(reference.evaluation, candidate.evaluation)
      comparison <- EvalComparison.fromLines(mover, reference, candidate, candidateSet.map(_.candidateSetType))
    yield CandidateComparisonFact(
      kind = kind,
      referenceLine = reference.ref,
      candidateLine = candidate.ref,
      comparison = comparison,
      verdictConfidence = verdictConfidence,
      candidateSet = candidateSet
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
    verdictConfidence: VerdictConfidence,
    candidateSetType: Option[CandidateSetType]
):
  def stableKey: String =
    List(
      kind.toString,
      mover.toString,
      referenceLine.stableKey,
      candidateLine.stableKey,
      java.lang.Double.toHexString(candidateWinPercentDeltaForMover),
      verdict.toString,
      verdictConfidence.toString,
      candidateSetType.map(_.toString).getOrElse("")
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
      verdictConfidence = comparison.verdictConfidence,
      candidateSetType = comparison.candidateSet.map(_.candidateSetType)
    )

enum RelativeCauseKind:
  case MissedTacticalResource
  case TacticalRefutationOfPlayed
  case CandidateTacticalLiability
  case RecaptureRecoveryWindow
  case WrongMoveOrder
  case TempoLoss
  case ConversionMiss
  case ConversionSecured
  case SacrificeCompensation
  case PlanImprovement
  case PlanContradiction
  case DefensiveResource
  case DrawResource
  case KingForcing
  case MaterialSwing

/** C owns an explicit, fail-closed selector for the direct effects it may
  * expose. Raw proof projection deliberately ignores this value so assembly
  * can resolve endpoint deltas before any Cause record is registered.
  */
enum DirectEffectAdmission:
  case Unresolved
  case Restricted(exactOccurrences: Set[RootOwnedEffectChannelOccurrenceFingerprint])

case class RelativeCauseFact(
    kind: RelativeCauseKind,
    comparisonEvidence: EvidenceRef,
    supportEvidence: List[EvidenceRef],
    sourceSide: RelativeCauseSourceSide,
    attribution: CauseAttribution = CauseAttribution.unattributed,
    proof: Option[RelativeCauseProof] = None,
    directEffectAdmission: DirectEffectAdmission = DirectEffectAdmission.Unresolved
):
  def hasOwnedTypedDepth(graph: TypedEvidenceGraph): Boolean =
    graph.relativeCauseHasOwnedTypedDepth(this)
  def strategicCauseKind: Boolean =
    RelativeCauseKind.strategicContrastBacked(kind)
  def hasOwnedAdmissibleLongTermProof(graph: TypedEvidenceGraph): Boolean =
    graph.relativeCauseHasOwnedAdmissibleLongTermProof(this)
  def hasOwnedTacticalProof(graph: TypedEvidenceGraph): Boolean =
    graph.relativeCauseHasOwnedTacticalProof(this)

object RelativeCauseKind:
  def requiresExactPlanResult(kind: RelativeCauseKind): Boolean =
    kind == RelativeCauseKind.PlanImprovement ||
      kind == RelativeCauseKind.PlanContradiction

  def sourceAttributionCompatible(
      kind: RelativeCauseKind,
      sourceSide: RelativeCauseSourceSide,
      attributionKind: CauseAttributionKind
  ): Boolean =
    val polarityCompatible =
      attributionKind match
        case CauseAttributionKind.ReferenceCreatesResource =>
          sourceSide == RelativeCauseSourceSide.Reference
        case CauseAttributionKind.CandidateCreatesValue | CauseAttributionKind.CandidateAllowsLiability =>
          sourceSide == RelativeCauseSourceSide.Candidate
        case CauseAttributionKind.SharedContext =>
          sourceSide == RelativeCauseSourceSide.Shared || sourceSide == RelativeCauseSourceSide.Mixed
        case CauseAttributionKind.ContextOnly | CauseAttributionKind.Unattributed =>
          false
    val kindCompatible =
      kind match
        case RelativeCauseKind.ConversionMiss =>
          sourceSide == RelativeCauseSourceSide.Candidate &&
            attributionKind == CauseAttributionKind.CandidateAllowsLiability
        case RelativeCauseKind.ConversionSecured =>
          (sourceSide == RelativeCauseSourceSide.Reference &&
            attributionKind == CauseAttributionKind.ReferenceCreatesResource) ||
            (sourceSide == RelativeCauseSourceSide.Candidate &&
              attributionKind == CauseAttributionKind.CandidateCreatesValue)
        case _ =>
          true
    polarityCompatible && kindCompatible

  def strategicContrastBacked(kind: RelativeCauseKind): Boolean =
    kind match
      case RelativeCauseKind.SacrificeCompensation | RelativeCauseKind.PlanImprovement |
          RelativeCauseKind.PlanContradiction =>
        true
      case _ =>
        false

  def strategicAxisCanProveCause(
      kind: RelativeCauseKind,
      axis: StrategicAxisDetail
  ): Boolean =
    kind match
      case RelativeCauseKind.SacrificeCompensation =>
        false
      case RelativeCauseKind.PlanImprovement =>
        axis.kind == StrategicAxisKind.PlanCoherence &&
          axis.polarity == StrategicAxisPolarity.Gain
      case RelativeCauseKind.PlanContradiction =>
        axis.kind == StrategicAxisKind.PlanCoherence &&
          axis.polarity == StrategicAxisPolarity.Concede
      case _ =>
        false

  def planCausalEventCanProveCause(
      kind: RelativeCauseKind,
      event: PlanCausalEventEvidence
  ): Boolean =
    kind match
      case RelativeCauseKind.PlanImprovement =>
        event.exactRobustPublicResultAssessments.nonEmpty
      case RelativeCauseKind.PlanContradiction =>
        event.exactRefutedPublicResultAssessments.nonEmpty
      case RelativeCauseKind.SacrificeCompensation =>
        event.exactRobustPublicResultAssessments.nonEmpty
      case _ =>
        false

  def acceptsLineConsequence(
      kind: RelativeCauseKind,
      consequenceKind: LineConsequenceKind
  ): Boolean =
    kind match
      case RelativeCauseKind.RecaptureRecoveryWindow =>
        consequenceKind == LineConsequenceKind.RecaptureSequence ||
          consequenceKind == LineConsequenceKind.RecoveryWindow
      case RelativeCauseKind.WrongMoveOrder =>
        false
      case RelativeCauseKind.TempoLoss =>
        consequenceKind == LineConsequenceKind.ImmediateReplyCheck
      case RelativeCauseKind.KingForcing =>
        consequenceKind == LineConsequenceKind.Mate
      case RelativeCauseKind.DrawResource =>
        consequenceKind == LineConsequenceKind.DrawResource
      case RelativeCauseKind.ConversionMiss | RelativeCauseKind.ConversionSecured =>
        consequenceKind == LineConsequenceKind.RecaptureSequence ||
          consequenceKind == LineConsequenceKind.RecoveryWindow ||
          consequenceKind == LineConsequenceKind.Promotion ||
          consequenceKind == LineConsequenceKind.PromotionRace
      case RelativeCauseKind.MaterialSwing | RelativeCauseKind.SacrificeCompensation =>
        consequenceKind == LineConsequenceKind.MaterialGain ||
          consequenceKind == LineConsequenceKind.MaterialLoss ||
          consequenceKind == LineConsequenceKind.Sacrifice
      case RelativeCauseKind.MissedTacticalResource | RelativeCauseKind.TacticalRefutationOfPlayed |
          RelativeCauseKind.CandidateTacticalLiability =>
        LineConsequenceKind.tacticalDriver(consequenceKind)
      case _ =>
        false

  def acceptsDirectLineConsequence(
      kind: RelativeCauseKind,
      payload: LineFactEvidence,
      rootMove: String,
      consequence: LineConsequence
  ): Boolean =
    acceptsLineConsequence(kind, consequence.kind) &&
      (
        consequence.kind match
          case LineConsequenceKind.RecaptureSequence | LineConsequenceKind.RecoveryWindow =>
            payload.rootIsRecapture(rootMove)
          case _ =>
            true
      )

  def structuralConsequences(
      kind: RelativeCauseKind,
      payload: StructuralDeltaEvidence
  ): List[TransitionConsequence] =
    import TransitionConsequenceKind.*
    kind match
      case RelativeCauseKind.SacrificeCompensation | RelativeCauseKind.PlanImprovement |
          RelativeCauseKind.PlanContradiction =>
        Nil
      case RelativeCauseKind.ConversionMiss =>
        payload.removedConsequences.filter(consequence =>
          consequence.kind == PassedPawnConcession
        )
      case RelativeCauseKind.ConversionSecured =>
        payload.establishedConsequences.filter(consequence =>
          consequence.kind == PassedPawnProgress
        )
      case _ =>
        Nil

/** Evidence-id-independent causal frame. Two Causes may be compared for
  * semantic channel subsumption only when every field in this frame agrees.
  */
final case class RelativeCauseSemanticFrameKey(
    kind: RelativeCauseKind,
    comparison: CandidateComparisonSemanticKey,
    eventLine: SemanticLineKey,
    role: RelativeCauseRole,
    sourceSide: RelativeCauseSourceSide,
    attributionKind: CauseAttributionKind
):
  def stableKey: String =
    List(
      kind.toString,
      comparison.stableKey,
      eventLine.stableKey,
      role.toString,
      sourceSide.toString,
      attributionKind.toString
    ).mkString("|")

final case class RelativeCauseSemanticKey(
    frame: RelativeCauseSemanticFrameKey,
    directCausalSignatures: List[String],
    unreadyCauseEvidenceId: Option[String]
):
  def kind: RelativeCauseKind = frame.kind
  def comparisonKind: CandidateComparisonKind = frame.comparison.kind
  def mover: Color = frame.comparison.mover
  def referenceLine: SemanticLineKey = frame.comparison.referenceLine
  def candidateLine: SemanticLineKey = frame.comparison.candidateLine
  def eventLine: SemanticLineKey = frame.eventLine
  def role: RelativeCauseRole = frame.role
  def sourceSide: RelativeCauseSourceSide = frame.sourceSide
  def attributionKind: CauseAttributionKind = frame.attributionKind

  def sentenceReady: Boolean =
    directCausalSignatures.nonEmpty && unreadyCauseEvidenceId.isEmpty

  def stableKey: String =
    List(
      frame.stableKey,
      directCausalSignatures.mkString(","),
      unreadyCauseEvidenceId.getOrElse("")
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
    val directChannels = RelativeCauseConstructionAdmission
      .admittedDirectChannels(cause, graph)
      .map(_.causalSignature)
      .distinct
      .sorted
    val frame = RelativeCauseSemanticFrameKey(
      kind = cause.kind,
      comparison = CandidateComparisonSemanticKey.from(comparison),
      eventLine = SemanticLineKey.from(binding.eventLine),
      role = binding.role,
      sourceSide = cause.sourceSide,
      attributionKind = cause.attribution.kind
    )
    RelativeCauseSemanticKey(
      frame = frame,
      directCausalSignatures = directChannels,
      unreadyCauseEvidenceId = Option.unless(
        RelativeCauseConstructionAdmission.initiallyReady(cause, graph)
      )(causeEvidenceId)
    )

case class RelativeCauseBinding(
    role: RelativeCauseRole,
    sourceSide: RelativeCauseSourceSide,
    eventLine: LineNodeRef,
    evidenceLines: List[LineNodeRef],
    bindingTier: RelativeCauseBindingTier
)

object RelativeCauseFact:
  def binding(
      cause: RelativeCauseFact,
      comparison: CandidateComparisonFact
  ): RelativeCauseBinding =
    binding(
      kind = cause.kind,
      comparisonKind = comparison.kind,
      referenceLine = comparison.referenceLine,
      candidateLine = comparison.candidateLine,
      supportEvidence = cause.supportEvidence,
      explicitSourceSide = Some(cause.sourceSide)
    )

  def binding(
      kind: RelativeCauseKind,
      comparisonKind: CandidateComparisonKind,
      referenceLine: LineNodeRef,
      candidateLine: LineNodeRef,
      supportEvidence: List[EvidenceRef],
      explicitSourceSide: Option[RelativeCauseSourceSide]
  ): RelativeCauseBinding =
    val role = RelativeCauseRole.fromComparisonKind(comparisonKind)
    val sourceSide =
      explicitSourceSide
        .orElse(semanticSourceSide(kind, referenceLine, candidateLine, supportEvidence))
        .orElse(RelativeCauseSourceSide.fromSupportEvidence(referenceLine, candidateLine, supportEvidence))
        .getOrElse(RelativeCauseSourceSide.Shared)
    val eventLine = RelativeCauseSourceSide.eventLine(sourceSide, role, referenceLine, candidateLine)
    RelativeCauseBinding(
      role = role,
      sourceSide = sourceSide,
      eventLine = eventLine,
      evidenceLines = evidenceLinesFor(referenceLine, candidateLine, supportEvidence, sourceSide),
      bindingTier = RelativeCauseBindingTier.from(role, sourceSide)
    )

  private def semanticSourceSide(
      kind: RelativeCauseKind,
      referenceLine: LineNodeRef,
      candidateLine: LineNodeRef,
      supportEvidence: List[EvidenceRef]
  ): Option[RelativeCauseSourceSide] =
    kind match
      case _ =>
        None

  private def supportReferencesLine(supportEvidence: List[EvidenceRef], line: LineNodeRef): Boolean =
    supportEvidence.exists(_.line.contains(line))

  private def evidenceLinesFor(
      referenceLine: LineNodeRef,
      candidateLine: LineNodeRef,
      supportEvidence: List[EvidenceRef],
      sourceSide: RelativeCauseSourceSide
  ): List[LineNodeRef] =
    val supportLines =
      supportEvidence.flatMap(_.line).filter(line => line == referenceLine || line == candidateLine)
    val sideLines =
      sourceSide match
        case RelativeCauseSourceSide.Reference =>
          List(referenceLine)
        case RelativeCauseSourceSide.Candidate =>
          List(candidateLine)
        case RelativeCauseSourceSide.Mixed | RelativeCauseSourceSide.Shared =>
          List(referenceLine, candidateLine)
    (supportLines ++ sideLines).distinct

enum RelativeCauseProofRole:
  case DirectProof
  case ContrastProof
  case ContextSupport

enum RelativeCauseProofStrength:
  case Primary
  case Supporting
  case WeakHint

case class RelativeCauseStrategicProofIdentity(
    axisKeys: List[String],
    mechanismKinds: List[StrategicMechanismKind],
    mechanismSourceIds: List[String],
    signalSourceIds: List[String]
):
  def isEmpty: Boolean =
    axisKeys.isEmpty && mechanismKinds.isEmpty && mechanismSourceIds.isEmpty && signalSourceIds.isEmpty

object RelativeCauseStrategicProofIdentity:
  val empty: RelativeCauseStrategicProofIdentity =
    RelativeCauseStrategicProofIdentity(Nil, Nil, Nil, Nil)

case class RelativeCauseProofSection(
    role: RelativeCauseProofRole,
    strength: RelativeCauseProofStrength,
    sourceRefs: List[EvidenceRef] = Nil
)

object RelativeCauseProofSection:
  def merge(
      role: RelativeCauseProofRole,
      strength: RelativeCauseProofStrength,
      sections: List[RelativeCauseProofSection]
  ): RelativeCauseProofSection =
    RelativeCauseProofSection(
      role = role,
      strength = strength,
      sourceRefs = sections.flatMap(_.sourceRefs).distinctBy(_.id)
    )

case class RelativeCauseProof(
    directProof: RelativeCauseProofSection = RelativeCauseProofSection(
      role = RelativeCauseProofRole.DirectProof,
      strength = RelativeCauseProofStrength.Primary
    ),
    contrastProof: RelativeCauseProofSection = RelativeCauseProofSection(
      role = RelativeCauseProofRole.ContrastProof,
      strength = RelativeCauseProofStrength.Supporting
    ),
    contextSupport: RelativeCauseProofSection = RelativeCauseProofSection(
      role = RelativeCauseProofRole.ContextSupport,
      strength = RelativeCauseProofStrength.WeakHint
    )
):
  def sections: List[RelativeCauseProofSection] =
    List(directProof, contrastProof, contextSupport)

object RelativeCauseProof:
  def merge(proofs: List[RelativeCauseProof]): RelativeCauseProof =
    RelativeCauseProof(
      directProof = RelativeCauseProofSection.merge(
        RelativeCauseProofRole.DirectProof,
        RelativeCauseProofStrength.Primary,
        proofs.map(_.directProof)
      ),
      contrastProof = RelativeCauseProofSection.merge(
        RelativeCauseProofRole.ContrastProof,
        RelativeCauseProofStrength.Supporting,
        proofs.map(_.contrastProof)
      ),
      contextSupport = RelativeCauseProofSection.merge(
        RelativeCauseProofRole.ContextSupport,
        RelativeCauseProofStrength.WeakHint,
        proofs.map(_.contextSupport)
      )
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
