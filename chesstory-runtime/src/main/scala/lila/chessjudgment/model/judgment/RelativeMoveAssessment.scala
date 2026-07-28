package lila.chessjudgment.model.judgment

import chess.Color
import lila.chessjudgment.model.evaluation.{ JudgmentThresholds, PerspectiveMath, VerdictThresholdPolicy }

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

object CandidateSetDescriptor:
  private[chessjudgment] def uniqueLines(lines: List[CandidateLineNode]): List[CandidateLineNode] =
    lines
      .filterNot(_.role == LineNodeRole.Threat)
      .sortBy(_.ref.rank)
      .groupBy(line => EvidenceRef.normalizeMove(line.ref.rootMove))
      .values
      .map(_.minBy(_.ref.rank))
      .toList
      .sortBy(_.ref.rank)

  def fromLines(
      mover: Color,
      lines: List[CandidateLineNode],
      reference: CandidateLineNode
  ): Option[CandidateSetDescriptor] =
    val ordered = uniqueLines(lines)
    ordered.find(line => !EvidenceRef.sameMove(line.ref.rootMove, reference.ref.rootMove)).map { secondLine =>
      val topCandidates = ordered.take(3)
      val reliable =
        topCandidates.size >= 2 &&
          topCandidates.forall(line => JudgmentThresholds.engineBackedByDepth(line.depth, line.mate))
      val gap =
        EvalComparison
          .fromLines(mover, reference, secondLine)
          .winPercentLossForMover
      val spread =
        topCandidates
          .map(line => EvalComparison.fromLines(mover, reference, line).winPercentLossForMover)
          .maxOption
          .getOrElse(gap)
      val candidateSetType =
        if topCandidates.size >= 3 && reliable && gap >= JudgmentThresholds.ONLY_MOVE_GAP_WP then
          CandidateSetType.OnlyMove
        else if reliable && spread <= JudgmentThresholds.STYLE_CHOICE_SPREAD_WP then
          CandidateSetType.StyleChoice
        else CandidateSetType.NarrowChoice
      CandidateSetDescriptor(candidateSetType)
    }

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
      kind: RelativeCauseKind,
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
      sourceSide: RelativeCauseSourceSide,
      kind: RelativeCauseKind
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

final case class EvalComparison private[judgment] (
    mover: Color,
    candidateWinPercentDeltaForMover: Double,
    verdict: MoveChoiceVerdict
):
  def winPercentLossForMover: Double =
    (-candidateWinPercentDeltaForMover).max(0.0)

object EvalComparison:
  def fromLines(
      mover: Color,
      reference: CandidateLineNode,
      candidate: CandidateLineNode,
      candidateSetType: Option[CandidateSetType] = None
  ): EvalComparison =
    val delta =
      PerspectiveMath.compareForMover(
        mover = mover,
        reference = PerspectiveMath.EvalPoint(reference.whitePovEvalCp, reference.mate),
        candidate = PerspectiveMath.EvalPoint(candidate.whitePovEvalCp, candidate.mate)
      )
    EvalComparison(
      mover = mover,
      candidateWinPercentDeltaForMover = delta.candidateWinPercentDeltaForMover,
      verdict = VerdictThresholdPolicy.verdictFromWinPercent(
        delta.candidateWinPercentDeltaForMover,
        delta.winPercentLossForMover,
        delta.rawCpLossForMover,
        delta.mateDistanceLossForMover,
        candidateSetType
      )
    )

case class CandidateComparisonFact(
    kind: CandidateComparisonKind,
    referenceLine: LineNodeRef,
    candidateLine: LineNodeRef,
    comparison: EvalComparison,
    candidateSet: Option[CandidateSetDescriptor] = None,
    defensiveRecaptureResource: Option[PlayedVsBestDefensiveRecaptureResource] = None
):
  def hasDistinctRootMoves: Boolean =
    !EvidenceRef.sameMove(referenceLine.rootMove, candidateLine.rootMove)

  def candidateSetType: Option[CandidateSetType] =
    candidateSet.map(_.candidateSetType)

  /** Reclassifies this exact comparison from its registered line nodes through
    * the sole central verdict policy.
    */
  def recomputedComparison(
      reference: CandidateLineNode,
      candidate: CandidateLineNode
  ): EvalComparison =
    EvalComparison.fromLines(
      mover = comparison.mover,
      reference = reference,
      candidate = candidate,
      candidateSetType = candidateSetType
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
    candidateSetType: Option[CandidateSetType],
    defensiveRecaptureResource: Option[PlayedVsBestDefensiveRecaptureResource]
):
  def stableKey: String =
    List(
      kind.toString,
      mover.toString,
      referenceLine.stableKey,
      candidateLine.stableKey,
      java.lang.Double.toHexString(candidateWinPercentDeltaForMover),
      verdict.toString,
      candidateSetType.map(_.toString).getOrElse(""),
      defensiveRecaptureResource.map(_.toString).getOrElse("")
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
      candidateSetType = comparison.candidateSet.map(_.candidateSetType),
      defensiveRecaptureResource = comparison.defensiveRecaptureResource
    )

enum RelativeCauseKind:
  case MissedTacticalResource
  case TacticalRefutationOfPlayed
  case CandidateTacticalLiability
  case WrongRecapturer
  case RecaptureRecoveryWindow
  case WrongMoveOrder
  case OnlyDefenseNecessity
  case TempoLoss
  case ConversionMiss
  case ConversionSecured
  case SacrificeCompensation
  case StructuralImprovement
  case TargetPressureGain
  case TargetPressureRelease
  case CenterControlGain
  case KingSafetyConcession
  case PawnWeaknessTarget
  case PawnBreakOpportunity
  case ActivityGain
  case ActivityLoss
  case OpponentRestriction
  case StrategicConcession
  case MissedStrategicImprovement
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
  case Restricted(causalSignatures: Set[String])

  def admittedCausalSignatures: Set[String] =
    this match
      case DirectEffectAdmission.Restricted(signatures) => signatures
      case _                                             => Set.empty

  def productionReady: Boolean =
    admittedCausalSignatures.nonEmpty

case class RelativeCauseFact(
    kind: RelativeCauseKind,
    comparisonEvidence: EvidenceRef,
    supportEvidence: List[EvidenceRef],
    sourceSide: RelativeCauseSourceSide,
    attribution: CauseAttribution = CauseAttribution.unattributed,
    proof: Option[RelativeCauseProof] = None,
    directEffectAdmission: DirectEffectAdmission = DirectEffectAdmission.Unresolved
):
  def hasRawTypedDepth(graph: TypedEvidenceGraph): Boolean =
    graph.relativeCauseHasRawTypedDepth(this)
  def hasOwnedTypedDepth(graph: TypedEvidenceGraph): Boolean =
    graph.relativeCauseHasOwnedTypedDepth(this)
  def strategicCauseKind: Boolean =
    RelativeCauseKind.strategicContrastBacked(kind)
  def hasStrategicContrastDepth(graph: TypedEvidenceGraph): Boolean =
    graph.relativeCauseHasStrategicContrastDepth(this)
  def hasOwnedStrategicContrastDepth(graph: TypedEvidenceGraph): Boolean =
    attribution.directProofEligible && hasStrategicContrastDepth(graph)
  def hasOwnedAdmissibleLongTermProof(graph: TypedEvidenceGraph): Boolean =
    graph.relativeCauseHasOwnedAdmissibleLongTermProof(this)
  def hasOwnedTacticalProof(graph: TypedEvidenceGraph): Boolean =
    graph.relativeCauseHasOwnedTacticalProof(this)
  def strategicProofIdentity(graph: TypedEvidenceGraph): RelativeCauseStrategicProofIdentity =
    graph.relativeCauseStrategicProofIdentity(this)

object RelativeCauseKind:
  def requiresExactPlanResult(kind: RelativeCauseKind): Boolean =
    kind == RelativeCauseKind.PlanImprovement ||
      kind == RelativeCauseKind.PlanContradiction

  def defaultAttributionKind(
      kind: RelativeCauseKind,
      sourceSide: Option[RelativeCauseSourceSide]
  ): CauseAttributionKind =
    sourceSide match
      case Some(RelativeCauseSourceSide.Reference) =>
        CauseAttributionKind.ReferenceCreatesResource
      case Some(RelativeCauseSourceSide.Candidate) =>
        kind match
          case RelativeCauseKind.RecaptureRecoveryWindow | RelativeCauseKind.ConversionSecured |
              RelativeCauseKind.SacrificeCompensation | RelativeCauseKind.StructuralImprovement |
              RelativeCauseKind.TargetPressureGain | RelativeCauseKind.CenterControlGain |
              RelativeCauseKind.PawnWeaknessTarget | RelativeCauseKind.PawnBreakOpportunity |
              RelativeCauseKind.ActivityGain | RelativeCauseKind.OpponentRestriction |
              RelativeCauseKind.PlanImprovement | RelativeCauseKind.DefensiveResource |
              RelativeCauseKind.DrawResource =>
            CauseAttributionKind.CandidateCreatesValue
          case RelativeCauseKind.MissedTacticalResource | RelativeCauseKind.TacticalRefutationOfPlayed |
              RelativeCauseKind.CandidateTacticalLiability | RelativeCauseKind.WrongRecapturer |
              RelativeCauseKind.WrongMoveOrder | RelativeCauseKind.OnlyDefenseNecessity |
              RelativeCauseKind.TempoLoss |
              RelativeCauseKind.ConversionMiss | RelativeCauseKind.TargetPressureRelease |
              RelativeCauseKind.KingSafetyConcession | RelativeCauseKind.ActivityLoss |
              RelativeCauseKind.StrategicConcession | RelativeCauseKind.MissedStrategicImprovement |
              RelativeCauseKind.PlanContradiction | RelativeCauseKind.KingForcing |
              RelativeCauseKind.MaterialSwing =>
            CauseAttributionKind.CandidateAllowsLiability
      case Some(RelativeCauseSourceSide.Shared) =>
        CauseAttributionKind.SharedContext
      case Some(RelativeCauseSourceSide.Mixed) | None =>
        kind match
          case RelativeCauseKind.OnlyDefenseNecessity |
              RelativeCauseKind.MissedTacticalResource | RelativeCauseKind.MissedStrategicImprovement |
              RelativeCauseKind.KingForcing =>
            CauseAttributionKind.ReferenceCreatesResource
          case RelativeCauseKind.WrongRecapturer | RelativeCauseKind.WrongMoveOrder |
              RelativeCauseKind.TempoLoss | RelativeCauseKind.TacticalRefutationOfPlayed |
              RelativeCauseKind.CandidateTacticalLiability | RelativeCauseKind.MaterialSwing |
              RelativeCauseKind.PlanContradiction | RelativeCauseKind.TargetPressureRelease |
              RelativeCauseKind.ActivityLoss | RelativeCauseKind.OpponentRestriction |
              RelativeCauseKind.StrategicConcession | RelativeCauseKind.KingSafetyConcession |
              RelativeCauseKind.ConversionMiss =>
            CauseAttributionKind.CandidateAllowsLiability
          case RelativeCauseKind.RecaptureRecoveryWindow | RelativeCauseKind.ConversionSecured |
              RelativeCauseKind.SacrificeCompensation | RelativeCauseKind.StructuralImprovement |
              RelativeCauseKind.TargetPressureGain | RelativeCauseKind.CenterControlGain |
              RelativeCauseKind.PawnWeaknessTarget | RelativeCauseKind.PawnBreakOpportunity |
              RelativeCauseKind.ActivityGain | RelativeCauseKind.PlanImprovement |
              RelativeCauseKind.DefensiveResource | RelativeCauseKind.DrawResource =>
            CauseAttributionKind.Unattributed

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
      case RelativeCauseKind.StructuralImprovement | RelativeCauseKind.TargetPressureGain |
          RelativeCauseKind.TargetPressureRelease |
          RelativeCauseKind.CenterControlGain | RelativeCauseKind.KingSafetyConcession |
          RelativeCauseKind.PawnWeaknessTarget | RelativeCauseKind.PawnBreakOpportunity |
          RelativeCauseKind.ActivityGain | RelativeCauseKind.ActivityLoss | RelativeCauseKind.OpponentRestriction |
          RelativeCauseKind.StrategicConcession | RelativeCauseKind.MissedStrategicImprovement |
          RelativeCauseKind.PlanImprovement | RelativeCauseKind.PlanContradiction =>
        true
      case _ =>
        false

  def strategicAxisCanProveCause(
      kind: RelativeCauseKind,
      axis: StrategicAxisDetail,
      sourceSide: RelativeCauseSourceSide
  ): Boolean =
    kind match
      case RelativeCauseKind.PlanImprovement =>
        axis.kind == StrategicAxisKind.PlanCoherence &&
          axis.polarity == StrategicAxisPolarity.Gain
      case RelativeCauseKind.PlanContradiction =>
        axis.kind == StrategicAxisKind.PlanCoherence &&
          axis.polarity == StrategicAxisPolarity.Concede
      case RelativeCauseKind.TargetPressureGain | RelativeCauseKind.PawnWeaknessTarget =>
        axis.kind == StrategicAxisKind.Target && axis.polarity == StrategicAxisPolarity.Gain
      case RelativeCauseKind.TargetPressureRelease =>
        axis.kind == StrategicAxisKind.Target &&
          Set(
            StrategicAxisPolarity.Release,
            StrategicAxisPolarity.Loss,
            StrategicAxisPolarity.Concede
          )(axis.polarity)
      case RelativeCauseKind.PawnBreakOpportunity =>
        axis.kind == StrategicAxisKind.PawnBreak &&
          Set(
            StrategicAxisPolarity.Support,
            StrategicAxisPolarity.Preserve,
            StrategicAxisPolarity.Release,
            StrategicAxisPolarity.Gain
          )(axis.polarity)
      case RelativeCauseKind.CenterControlGain =>
        axis.kind == StrategicAxisKind.SpaceCenter
      case RelativeCauseKind.ActivityGain =>
        axis.kind == StrategicAxisKind.Activity &&
          axis.polarity == StrategicAxisPolarity.Gain &&
          (sourceSide == RelativeCauseSourceSide.Reference || sourceSide == RelativeCauseSourceSide.Candidate)
      case RelativeCauseKind.ActivityLoss =>
        axis.kind == StrategicAxisKind.Activity &&
          sourceSide == RelativeCauseSourceSide.Candidate &&
          Set(
            StrategicAxisPolarity.Loss,
            StrategicAxisPolarity.Release,
            StrategicAxisPolarity.Concede
          )(axis.polarity)
      case RelativeCauseKind.OpponentRestriction =>
        axis.kind == StrategicAxisKind.Counterplay && axis.polarity == StrategicAxisPolarity.Restrain
      case RelativeCauseKind.KingSafetyConcession =>
        axis.kind == StrategicAxisKind.Counterplay && axis.polarity == StrategicAxisPolarity.Concede
      case RelativeCauseKind.StructuralImprovement | RelativeCauseKind.MissedStrategicImprovement |
          RelativeCauseKind.StrategicConcession =>
        true
      case _ =>
        false

  def planCausalEventCanProveCause(
      kind: RelativeCauseKind,
      event: PlanCausalEventEvidence
  ): Boolean =
    kind match
      case RelativeCauseKind.PlanImprovement =>
        event.exactRobustPublicResultAssessment.nonEmpty
      case RelativeCauseKind.PlanContradiction =>
        event.exactRefutedPublicResultAssessment.nonEmpty
      case RelativeCauseKind.OpponentRestriction =>
        (
          event.opponentResourceDeterrence.nonEmpty &&
            event.structuralConsequences.exists(consequence =>
              consequence.kind == TransitionConsequenceKind.OpponentMobilityRestriction &&
                consequence.subjects.exists(StructuralDeltaEvidence.validOpponentMobilityRestrictionSubject)
            )
        ) ||
          (
            DirectOpponentRestrictionProof.rootMoveDirectlyRestrictsOpponent(event) &&
              DirectOpponentRestrictionProof.exactRootPawnBlockadeConsequences(event).nonEmpty
          )
      case _ =>
        false

  def acceptsLineConsequence(
      kind: RelativeCauseKind,
      consequenceKind: LineConsequenceKind
  ): Boolean =
    kind match
      case RelativeCauseKind.WrongRecapturer =>
        consequenceKind == LineConsequenceKind.MaterialLoss
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
      case RelativeCauseKind.TargetPressureGain =>
        payload.consequencesOf(TransitionConsequenceKind.TargetPressureGain) ++
          payload.consequencesOf(KingSafetyPressure) ++
          payload.consequencesOf(KingRingPressureGain)
      case RelativeCauseKind.TargetPressureRelease =>
        payload.consequencesOf(TransitionConsequenceKind.TargetPressureRelease)
      case RelativeCauseKind.CenterControlGain =>
        payload.consequencesOf(TransitionConsequenceKind.CenterControlGain)
      case RelativeCauseKind.OpponentRestriction =>
        opponentRestrictionConsequences(payload) ++ counterBreakCarrierConsequences(payload)
      case RelativeCauseKind.KingSafetyConcession =>
        payload.consequencesOf(TransitionConsequenceKind.KingSafetyConcession) ++
          payload.consequencesOf(KingRingPressureConcession)
      case RelativeCauseKind.PawnWeaknessTarget =>
        payload.consequencesOf(WeakPawnTargetCreated) ++
          payload.consequencesOf(WeakSquareTargetCreated)
      case RelativeCauseKind.PawnBreakOpportunity =>
        payload.consequencesOf(PawnTensionGain) ++
          payload.consequencesOf(PawnTensionResolution)
      case RelativeCauseKind.ActivityGain =>
        payload.consequencesOf(DevelopmentLagReduced) ++
          payload.consequencesOf(DevelopmentPieceActivated) ++
          payload.consequencesOf(DevelopmentMobilityGain) ++
          payload.consequencesOf(DevelopmentCenterControlGain) ++
          payload.consequencesOf(DevelopmentSafePlacement) ++
          payload.consequencesOf(FileOccupationGain) ++
          payload.consequencesOf(MobilityGain) ++
          payload.consequencesOf(LineUnlockGain) ++
          payload.consequencesOf(FileAccessGain) ++
          payload.consequencesOf(BatteryPressureGain) ++
          payload.consequencesOf(OutpostGain)
      case RelativeCauseKind.ActivityLoss =>
        payload.consequencesOf(DevelopmentLagIncreased) ++
          payload.consequencesOf(DevelopmentPieceRetreated) ++
          payload.consequencesOf(DevelopmentMobilityLoss) ++
          payload.consequencesOf(DevelopmentCenterControlLoss) ++
          payload.consequencesOf(DevelopmentUnsafePlacement) ++
          payload.consequencesOf(MobilityLoss) ++
          payload.consequencesOf(FileAccessLoss) ++
          payload.consequencesOf(OutpostConcession)
      case RelativeCauseKind.StructuralImprovement | RelativeCauseKind.MissedStrategicImprovement =>
        payload.positiveConsequences.filter(consequence =>
          StructuralDeltaEvidence.isStructuralAnchorConsequence(consequence.kind)
        )
      case RelativeCauseKind.PlanImprovement | RelativeCauseKind.PlanContradiction =>
        Nil
      case RelativeCauseKind.ConversionMiss =>
        payload.negativeConsequences.filter(consequence =>
          consequence.kind == PromotionPressureConcession ||
            consequence.kind == PassedPawnConcession
        )
      case RelativeCauseKind.ConversionSecured =>
        payload.positiveConsequences.filter(consequence =>
          consequence.kind == PromotionPressureGain ||
            consequence.kind == PassedPawnProgress
        )
      case RelativeCauseKind.StrategicConcession =>
        payload.negativeConsequences.filter(consequence =>
          StructuralDeltaEvidence.isStrategicSupportConsequence(consequence.kind)
        )
      case _ =>
        Nil

  def structuralConsequences(
      kind: RelativeCauseKind,
      payload: StructuralDeltaEvidence,
      axis: Option[StrategicAxisDetail]
  ): List[TransitionConsequence] =
    import TransitionConsequenceKind.*
    kind match
      case RelativeCauseKind.PawnBreakOpportunity =>
        axis match
          case Some(detail)
              if detail.kind == StrategicAxisKind.PawnBreak &&
                detail.polarity == StrategicAxisPolarity.Release =>
            payload.consequencesOf(PawnTensionResolution)
          case Some(detail)
              if detail.kind == StrategicAxisKind.PawnBreak &&
                (
                  detail.polarity == StrategicAxisPolarity.Support ||
                    detail.polarity == StrategicAxisPolarity.Preserve ||
                    detail.polarity == StrategicAxisPolarity.Gain
                ) =>
            payload.consequencesOf(PawnTensionGain)
          case Some(_) =>
            Nil
          case None =>
            structuralConsequences(kind, payload)
      case RelativeCauseKind.OpponentRestriction =>
        opponentRestrictionConsequences(payload)
      case _ =>
        structuralConsequences(kind, payload)

  private def opponentRestrictionConsequences(payload: StructuralDeltaEvidence): List[TransitionConsequence] =
    payload.consequencesOf(TransitionConsequenceKind.OpponentMobilityRestriction).filter(consequence =>
      consequence.subjects.exists(StructuralDeltaEvidence.validOpponentMobilityRestrictionSubject)
    )

  private def counterBreakCarrierConsequences(payload: StructuralDeltaEvidence): List[TransitionConsequence] =
    payload.consequences.filter(consequence =>
      (
        consequence.kind == TransitionConsequenceKind.PawnTensionGain ||
          consequence.kind == TransitionConsequenceKind.PawnTensionResolution
      ) &&
        consequence.subjects.exists(subject =>
          val normalized = Option(subject).getOrElse("").trim.toLowerCase
          normalized.startsWith("break-file:") ||
            normalized.startsWith("created-tension:") ||
            normalized.startsWith("resolved-tension:")
        )
    )

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
        .orElse(RelativeCauseSourceSide.fromSupportEvidence(kind, referenceLine, candidateLine, supportEvidence))
        .getOrElse(defaultSourceSide(kind))
    val eventLine = RelativeCauseSourceSide.eventLine(sourceSide, role, referenceLine, candidateLine)
    RelativeCauseBinding(
      role = role,
      sourceSide = sourceSide,
      eventLine = eventLine,
      evidenceLines = evidenceLinesFor(referenceLine, candidateLine, supportEvidence, sourceSide),
      bindingTier = RelativeCauseBindingTier.from(role, sourceSide, kind)
    )

  def defaultSourceSide(kind: RelativeCauseKind): RelativeCauseSourceSide =
    RelativeCauseSourceSide.Shared

  private def semanticSourceSide(
      kind: RelativeCauseKind,
      referenceLine: LineNodeRef,
      candidateLine: LineNodeRef,
      supportEvidence: List[EvidenceRef]
  ): Option[RelativeCauseSourceSide] =
    kind match
      case RelativeCauseKind.WrongRecapturer
          if EvidenceRef.sameDestinationDifferentOrigin(referenceLine.rootMove, candidateLine.rootMove) &&
            supportReferencesLine(supportEvidence, referenceLine) &&
            supportReferencesLine(supportEvidence, candidateLine) =>
        Some(RelativeCauseSourceSide.Candidate)
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
