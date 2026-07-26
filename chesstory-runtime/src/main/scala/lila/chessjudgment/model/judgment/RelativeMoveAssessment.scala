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
      Some(
        if kind == RelativeCauseKind.OnlyMoveNecessity then RelativeCauseSourceSide.Reference
        else RelativeCauseSourceSide.Shared
      )
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

enum RelativeCauseImportance:
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
  def contextOnly: Boolean =
    kind == CauseAttributionKind.ContextOnly
  def unattributed: Boolean =
    kind == CauseAttributionKind.Unattributed
  def rootMismatch: Boolean =
    reason.contains("root-mismatch")

object CauseAttribution:
  val unattributed: CauseAttribution =
    CauseAttribution(CauseAttributionKind.Unattributed, reason = Some("no-cause-attribution"))

object RelativeCauseImportance:
  def from(
      role: RelativeCauseRole,
      sourceSide: RelativeCauseSourceSide,
      kind: RelativeCauseKind
  ): RelativeCauseImportance =
    role match
      case RelativeCauseRole.PrimaryPlayedCause
          if sourceSide == RelativeCauseSourceSide.Shared && kind != RelativeCauseKind.OnlyMoveNecessity =>
        RelativeCauseImportance.Supporting
      case RelativeCauseRole.PrimaryPlayedCause =>
        RelativeCauseImportance.Primary
      case RelativeCauseRole.CandidateSetConstraint if kind == RelativeCauseKind.OnlyMoveNecessity =>
        RelativeCauseImportance.Primary
      case RelativeCauseRole.CandidateSetConstraint =>
        RelativeCauseImportance.Supporting
      case RelativeCauseRole.PlayedAlternativeContext | RelativeCauseRole.AlternativeDiagnostic =>
        RelativeCauseImportance.Context

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
    candidateSet: Option[CandidateSetDescriptor] = None
):
  def hasDistinctRootMoves: Boolean =
    !EvidenceRef.sameMove(referenceLine.rootMove, candidateLine.rootMove)

enum RelativeCauseKind:
  case MissedTacticalResource
  case TacticalRefutationOfPlayed
  case CandidateTacticalLiability
  case WrongRecapturer
  case RecaptureRecoveryWindow
  case WrongMoveOrder
  case OnlyMoveNecessity
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

case class RelativeCauseFact(
    kind: RelativeCauseKind,
    comparisonEvidence: EvidenceRef,
    supportEvidence: List[EvidenceRef],
    sourceSide: RelativeCauseSourceSide,
    attribution: CauseAttribution = CauseAttribution.unattributed,
    proof: Option[RelativeCauseProof] = None
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
  def identityKey(graph: TypedEvidenceGraph): RelativeCauseIdentityKey =
    RelativeCauseIdentityKey.from(this, graph)

object RelativeCauseKind:
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

  def acceptsLineConsequence(
      kind: RelativeCauseKind,
      consequenceKind: LineConsequenceKind
  ): Boolean =
    kind match
      case RelativeCauseKind.WrongRecapturer | RelativeCauseKind.RecaptureRecoveryWindow =>
        consequenceKind == LineConsequenceKind.RecaptureSequence ||
          consequenceKind == LineConsequenceKind.RecoveryWindow
      case RelativeCauseKind.WrongMoveOrder | RelativeCauseKind.TempoLoss =>
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
      case RelativeCauseKind.StructuralImprovement | RelativeCauseKind.MissedStrategicImprovement |
          RelativeCauseKind.PlanImprovement | RelativeCauseKind.PlanContradiction =>
        payload.positiveConsequences.filter(consequence =>
          StructuralDeltaEvidence.isStructuralAnchorConsequence(consequence.kind)
        )
      case RelativeCauseKind.ConversionMiss | RelativeCauseKind.ConversionSecured =>
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

case class RelativeCauseIdentityKey(
    kind: RelativeCauseKind,
    comparisonKind: CandidateComparisonKind,
    referenceLine: LineNodeRef,
    candidateLine: LineNodeRef,
    eventLine: LineNodeRef,
    role: RelativeCauseRole,
    sourceSide: RelativeCauseSourceSide,
    importance: RelativeCauseImportance,
    comparisonEvidenceId: String,
    supportEvidenceIds: List[String],
    evidenceLineIds: List[String],
    proofDirectSourceIds: List[String],
    proofContrastSourceIds: List[String],
    proofContextSupportSourceIds: List[String],
    proofStrategicAxisKeys: List[String],
    proofStrategicMechanismKinds: List[StrategicMechanismKind],
    proofStrategicMechanismSourceIds: List[String],
    proofStrategicMechanismSignalSourceIds: List[String],
    proofDirectKinds: List[String],
    proofContrastKinds: List[String],
    proofContextSupportKinds: List[String],
    attributionKind: CauseAttributionKind,
    attributionRootMoveMatched: Boolean,
    attributionDirectProofEligible: Boolean
)

case class RelativeCauseBinding(
    role: RelativeCauseRole,
    sourceSide: RelativeCauseSourceSide,
    eventLine: LineNodeRef,
    evidenceLines: List[LineNodeRef],
    importance: RelativeCauseImportance
)

object RelativeCauseIdentityKey:
  def from(cause: RelativeCauseFact, graph: TypedEvidenceGraph): RelativeCauseIdentityKey =
    val comparison =
      graph.comparisonFor(cause).getOrElse(
        throw IllegalArgumentException(
          s"relative cause '${cause.comparisonEvidence.id}' is not bound to a candidate comparison"
        )
      )
    val binding = graph.requiredRelativeCauseBinding(cause)
    val strategicProof = graph.relativeCauseStrategicProofIdentity(cause)
    RelativeCauseIdentityKey(
      kind = cause.kind,
      comparisonKind = comparison.kind,
      referenceLine = comparison.referenceLine,
      candidateLine = comparison.candidateLine,
      eventLine = binding.eventLine,
      role = binding.role,
      sourceSide = cause.sourceSide,
      importance = binding.importance,
      comparisonEvidenceId = cause.comparisonEvidence.id,
      supportEvidenceIds = cause.supportEvidence.map(_.id).distinct.sorted,
      evidenceLineIds = binding.evidenceLines.map(_.id).distinct.sorted,
      proofDirectSourceIds = cause.proof.toList.flatMap(_.directProof.sourceRefs.map(_.id)).distinct.sorted,
      proofContrastSourceIds = cause.proof.toList.flatMap(_.contrastProof.sourceRefs.map(_.id)).distinct.sorted,
      proofContextSupportSourceIds = cause.proof.toList.flatMap(_.contextSupport.sourceRefs.map(_.id)).distinct.sorted,
      proofStrategicAxisKeys = strategicProof.axisKeys,
      proofStrategicMechanismKinds = strategicProof.mechanismKinds,
      proofStrategicMechanismSourceIds = strategicProof.mechanismSourceIds,
      proofStrategicMechanismSignalSourceIds = strategicProof.signalSourceIds,
      proofDirectKinds = cause.proof.toList.flatMap(proof => graph.relativeCauseProofKindLabels(cause, proof.directProof)).distinct.sorted,
      proofContrastKinds = cause.proof.toList.flatMap(proof => graph.relativeCauseProofKindLabels(cause, proof.contrastProof)).distinct.sorted,
      proofContextSupportKinds = cause.proof.toList.flatMap(proof => graph.relativeCauseProofKindLabels(cause, proof.contextSupport)).distinct.sorted,
      attributionKind = cause.attribution.kind,
      attributionRootMoveMatched = cause.attribution.rootMoveMatched,
      attributionDirectProofEligible = cause.attribution.directProofEligible
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
      importance = RelativeCauseImportance.from(role, sourceSide, kind)
    )

  def defaultSourceSide(kind: RelativeCauseKind): RelativeCauseSourceSide =
    kind match
      case RelativeCauseKind.OnlyMoveNecessity =>
        RelativeCauseSourceSide.Reference
      case _ =>
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
):
  def hasReferences: Boolean =
    sourceRefs.nonEmpty

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

  def proofSections: List[RelativeCauseProofSection] =
    sections.filter(_.hasReferences)
  def hasAnyEvidence: Boolean =
    sections.exists(_.hasReferences)
  def depthProof: RelativeCauseProof =
    RelativeCauseProof(
      directProof = directProof,
      contrastProof = contrastProof
    )

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
