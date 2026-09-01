package lila.chessjudgment.model.judgment

import lila.chessjudgment.analysis.position.PositionRelationExtractor
import lila.chessjudgment.model.line.PrincipalVariationEvidence

private[chessjudgment] enum VacatedGateEnablesUnrecapturableSliderCaptureBranchRole extends CausalBranchRole:
  case CounterfactualReference
  case ObservedPlayedSibling

  def stableKey: String =
    this match
      case CounterfactualReference => "counterfactual-reference"
      case ObservedPlayedSibling    => "observed-played-sibling"

private[chessjudgment] enum VacatedGateEnablesUnrecapturableSliderCapturePremiseRole extends CausalPremiseRole:
  case ReferenceRootSliderReach
  case ReferenceExploitCapture

  def stableKey: String =
    this match
      case ReferenceRootSliderReach => "reference-root-slider-reach"
      case ReferenceExploitCapture  => "reference-exploit-capture"

private[chessjudgment] enum VacatedGateEnablesUnrecapturableSliderCaptureAbsenceRole extends CausalAbsenceRole:
  case ReferenceImmediateRecaptureAbsent
  case PlayedExploitMoveAbsent
  case PlayedReplacementCaptureAbsent

  def stableKey: String =
    this match
      case ReferenceImmediateRecaptureAbsent => "reference-immediate-recapture-absent"
      case PlayedExploitMoveAbsent            => "played-exploit-move-absent"
      case PlayedReplacementCaptureAbsent     => "played-replacement-capture-absent"

private[chessjudgment] enum VacatedGateEnablesUnrecapturableSliderCaptureStateRole extends CausalStateRole:
  case ReferenceInterveningSliderReach
  case PlayedSliderOccupied
  case PlayedTargetOccupied
  case PlayedGateBlockerOccupied
  case PlayedBlockedSliderReach

  def stableKey: String =
    this match
      case ReferenceInterveningSliderReach => "reference-intervening-slider-reach"
      case PlayedSliderOccupied            => "played-slider-occupied"
      case PlayedTargetOccupied            => "played-target-occupied"
      case PlayedGateBlockerOccupied       => "played-gate-blocker-occupied"
      case PlayedBlockedSliderReach        => "played-blocked-slider-reach"

private[chessjudgment] object VacatedGateEnablesUnrecapturableSliderCaptureCausalAuthority:
  private final case class PropositionDescriptor(
      rootFen: String,
      enabler: RelationMoveTransitionWitness,
      slider: RelationColoredPieceWitness,
      gateBlocker: RelationColoredPieceWitness,
      exploit: RelationMoveTransitionWitness,
      capturedTarget: RelationColoredPieceWitness
  ) extends CausalSemanticDescriptor:
    require(
      enabler.side == slider.side &&
        gateBlocker == RelationColoredPieceWitness(enabler.from, enabler.beforeRole, enabler.side),
      "the reference root mover must be the exact vacated gate blocker"
    )
    require(
      exploit.side == slider.side && exploit.from == slider.square &&
        exploit.beforeRole == slider.role && exploit.afterRole == slider.role &&
        capturedTarget.side != slider.side && exploit.to == capturedTarget.square,
      "the later capture must be consumed by the same exact slider and target"
    )

    val contractKind = BoundedCausalContractKind.VacatedGateEnablesUnrecapturableSliderCapture
    val semanticParts: List[String] = List(
      enabler.stableKey,
      coloredPieceStableKey(slider),
      coloredPieceStableKey(gateBlocker),
      exploit.stableKey,
      coloredPieceStableKey(capturedTarget)
    )

  def proposition(
      rootFen: String,
      enabler: RelationMoveTransitionWitness,
      slider: RelationColoredPieceWitness,
      gateBlocker: RelationColoredPieceWitness,
      exploit: RelationMoveTransitionWitness,
      capturedTarget: RelationColoredPieceWitness
  ): CausalPropositionIdentity =
    CausalPropositionIdentity.from(
      PropositionDescriptor(rootFen, enabler, slider, gateBlocker, exploit, capturedTarget)
    )

  def coloredPieceStableKey(piece: RelationColoredPieceWitness): String =
    s"${piece.side.toString.toLowerCase}:${piece.role.name.toLowerCase}@${piece.square.key.toLowerCase}"

private[chessjudgment] sealed trait VacatedGateEnablesUnrecapturableSliderCaptureManifest
    extends BoundedCausalContractManifest:
  def referenceRootReach: CausalVerticalRelationPremiseUse
  def referenceExploit: CausalVerticalRelationPremiseUse
  def referenceNoRecapture: CausalClosedAbsenceBinding
  def playedMoveAbsent: CausalClosedAbsenceBinding
  def playedCaptureAbsent: CausalClosedAbsenceBinding
  def referencePersistence: List[CausalClosedStateBinding]
  def playedPositionStates: List[CausalClosedStateBinding]

  final def contractKind = BoundedCausalContractKind.VacatedGateEnablesUnrecapturableSliderCapture
  final def premiseUses = List(referenceRootReach, referenceExploit)
  final def absenceBindings = List(referenceNoRecapture, playedMoveAbsent, playedCaptureAbsent)
  final override def stateBindings = referencePersistence ++ playedPositionStates
  final def stableKey: String =
    List(
      contractKind.toString.toLowerCase,
      premiseUses.map(_.stableKey).mkString("[", ",", "]"),
      absenceBindings.map(_.stableKey).mkString("[", ",", "]"),
      stateBindings.map(_.stableKey).mkString("[", ",", "]")
    ).mkString("|")

private[chessjudgment] object VacatedGateEnablesUnrecapturableSliderCaptureManifest:
  private final case class Exact(
      referenceRootReach: CausalVerticalRelationPremiseUse,
      referenceExploit: CausalVerticalRelationPremiseUse,
      referenceNoRecapture: CausalClosedAbsenceBinding,
      playedMoveAbsent: CausalClosedAbsenceBinding,
      playedCaptureAbsent: CausalClosedAbsenceBinding,
      referencePersistence: List[CausalClosedStateBinding],
      playedPositionStates: List[CausalClosedStateBinding]
  ) extends VacatedGateEnablesUnrecapturableSliderCaptureManifest

  def exact(
      referenceRootReach: CausalVerticalRelationPremiseUse,
      referenceExploit: CausalVerticalRelationPremiseUse,
      referenceNoRecapture: CausalClosedAbsenceBinding,
      playedMoveAbsent: CausalClosedAbsenceBinding,
      playedCaptureAbsent: CausalClosedAbsenceBinding,
      referencePersistence: List[CausalClosedStateBinding],
      playedPositionStates: List[CausalClosedStateBinding]
  ): VacatedGateEnablesUnrecapturableSliderCaptureManifest =
    val referenceRole = VacatedGateEnablesUnrecapturableSliderCaptureBranchRole.CounterfactualReference
    val playedRole = VacatedGateEnablesUnrecapturableSliderCaptureBranchRole.ObservedPlayedSibling
    val exploitIndex = referenceExploit.stepIndex
    val playedPreExploitIndex = exploitIndex - 1
    require(
      referenceRootReach.role == VacatedGateEnablesUnrecapturableSliderCapturePremiseRole.ReferenceRootSliderReach &&
        referenceRootReach.contract == VerticalRelationContractKind.SliderReachDelta &&
        referenceRootReach.result.kind == RelationFactKind.SliderReachDelta &&
        referenceRootReach.branchRole == referenceRole && referenceRootReach.stepIndex == 0,
      "vacated-gate-enables-unrecapturable-slider-capture must consume the exact reference root slider delta"
    )
    require(
      exploitIndex >= 2 &&
        referenceExploit.role == VacatedGateEnablesUnrecapturableSliderCapturePremiseRole.ReferenceExploitCapture &&
        referenceExploit.contract == VerticalRelationContractKind.CaptureRecaptureInventory &&
        referenceExploit.result.kind == RelationFactKind.CaptureRecaptureInventory &&
        referenceExploit.branchRole == referenceRole &&
        referenceExploit.branchId == referenceRootReach.branchId,
      "vacated-gate-enables-unrecapturable-slider-capture must consume a later same-branch capture inventory"
    )
    require(
      referenceNoRecapture.role ==
        VacatedGateEnablesUnrecapturableSliderCaptureAbsenceRole.ReferenceImmediateRecaptureAbsent &&
        referenceNoRecapture.branchRole == referenceRole &&
        referenceNoRecapture.branchId == referenceRootReach.branchId &&
        referenceNoRecapture.afterStepIndex == exploitIndex,
      "the reference recapture absence must belong to the exploit destination"
    )
    require(
      List(playedMoveAbsent, playedCaptureAbsent).forall(binding =>
        binding.branchRole == playedRole && binding.branchId == playedMoveAbsent.branchId &&
          binding.afterStepIndex == playedPreExploitIndex
      ) &&
        playedMoveAbsent.role == VacatedGateEnablesUnrecapturableSliderCaptureAbsenceRole.PlayedExploitMoveAbsent &&
        playedCaptureAbsent.role == VacatedGateEnablesUnrecapturableSliderCaptureAbsenceRole.PlayedReplacementCaptureAbsent,
      "played sibling exploit absences must share the exact pre-exploit occurrence"
    )
    require(
      referencePersistence.map(_.afterStepIndex) == (1 until exploitIndex).toList &&
        referencePersistence.forall(binding =>
          binding.role == VacatedGateEnablesUnrecapturableSliderCaptureStateRole.ReferenceInterveningSliderReach &&
            binding.branchRole == referenceRole && binding.branchId == referenceRootReach.branchId
        ),
      "every intervening reference occurrence must retain its exact slider-reach state"
    )
    val playedRoles = playedPositionStates.map(_.role)
    require(
      playedPositionStates.nonEmpty && playedPositionStates.forall(binding =>
        binding.branchRole == playedRole && binding.branchId == playedMoveAbsent.branchId &&
          binding.afterStepIndex == playedPreExploitIndex
      ) &&
        playedRoles.count(_ == VacatedGateEnablesUnrecapturableSliderCaptureStateRole.PlayedSliderOccupied) == 1 &&
        playedRoles.count(_ == VacatedGateEnablesUnrecapturableSliderCaptureStateRole.PlayedTargetOccupied) == 1 &&
        playedRoles.count(_ == VacatedGateEnablesUnrecapturableSliderCaptureStateRole.PlayedBlockedSliderReach) == 1 &&
        playedRoles.count(_ == VacatedGateEnablesUnrecapturableSliderCaptureStateRole.PlayedGateBlockerOccupied) == 1,
      "played sibling state closure needs slider, target, the exact blocker, and blocked reach"
    )
    Exact(
      referenceRootReach,
      referenceExploit,
      referenceNoRecapture,
      playedMoveAbsent,
      playedCaptureAbsent,
      referencePersistence,
      playedPositionStates
    )

private[chessjudgment] final case class VacatedGateEnablesUnrecapturableSliderCaptureSemanticProof private[chessjudgment] (
    identity: CausalPropositionIdentity,
    enabler: RelationMoveTransitionWitness,
    slider: RelationColoredPieceWitness,
    gateBlocker: RelationColoredPieceWitness,
    exploit: RelationMoveTransitionWitness,
    capturedTarget: RelationColoredPieceWitness
):
  require(
    identity.contractKind == BoundedCausalContractKind.VacatedGateEnablesUnrecapturableSliderCapture,
    "a vacated-gate-enables-unrecapturable-slider-capture proof needs its exact contract"
  )
  require(
    gateBlocker == RelationColoredPieceWitness(enabler.from, enabler.beforeRole, enabler.side) &&
      enabler.side == slider.side && exploit.side == slider.side &&
      exploit.from == slider.square && exploit.beforeRole == slider.role && exploit.afterRole == slider.role &&
      exploit.to == capturedTarget.square && capturedTarget.side != slider.side,
    "a vacated-gate-enables-unrecapturable-slider-capture proof must retain gate, slider, and later capture identities"
  )

  def semanticId: String = identity.semanticId
  def rootBoardState: String = identity.rootPositionIdentity.value

private[chessjudgment] final case class VacatedGateEnablesUnrecapturableSliderCaptureOccurrence private[chessjudgment] (
    proofSet: BoundedCausalProofSet
):
  require(
    proofSet.proposition.contractKind == BoundedCausalContractKind.VacatedGateEnablesUnrecapturableSliderCapture,
    "a vacated-gate-enables-unrecapturable-slider-capture occurrence needs its exact contract"
  )
  require(
    proofSet.occurrence.branches.size == 2 &&
      proofSet.occurrence.branch(VacatedGateEnablesUnrecapturableSliderCaptureBranchRole.CounterfactualReference).exists(
        _.line.role == LineNodeRole.BestReference
      ) &&
      proofSet.occurrence.branch(VacatedGateEnablesUnrecapturableSliderCaptureBranchRole.ObservedPlayedSibling).exists(
        _.line.role == LineNodeRole.Played
      ),
    "a vacated-gate-enables-unrecapturable-slider-capture occurrence needs one BestReference and one Played branch"
  )
  require(referenceSteps.size >= 3 && playedSteps.size == referenceSteps.size - 1)

  private def exactBranch(role: VacatedGateEnablesUnrecapturableSliderCaptureBranchRole): CausalBranchOccurrence =
    proofSet.occurrence
      .branch(role)
      .getOrElse(
        throw IllegalStateException(
          s"a vacated-gate-enables-unrecapturable-slider-capture occurrence lost its $role branch"
        )
      )

  def semanticId: String = proofSet.proposition.semanticId
  def occurrenceId: String = proofSet.occurrence.occurrenceId
  def referenceBranch: CausalBranchOccurrence =
    exactBranch(VacatedGateEnablesUnrecapturableSliderCaptureBranchRole.CounterfactualReference)
  def playedBranch: CausalBranchOccurrence =
    exactBranch(VacatedGateEnablesUnrecapturableSliderCaptureBranchRole.ObservedPlayedSibling)
  def referenceLine: LineNodeRef = referenceBranch.line
  def playedLine: LineNodeRef = playedBranch.line
  def referenceSteps: List[LineReplayStep] = referenceBranch.replaySteps
  def playedSteps: List[LineReplayStep] = playedBranch.replaySteps
  def proofPaths: List[CausalProofPathOccurrence] = proofSet.paths
  def enablingStep: LineReplayStep = referenceSteps.head
  def exploitStep: LineReplayStep = referenceSteps.last
  def referenceInterveningSteps: List[LineReplayStep] = referenceSteps.slice(1, referenceSteps.size - 1)
  def playedPreExploitStep: LineReplayStep = playedSteps.last

private[chessjudgment] final case class VacatedGateEnablesUnrecapturableSliderCaptureDependencyManifest private[chessjudgment] (
    referenceLineRecord: EvidenceRecord,
    playedLineRecord: EvidenceRecord,
    demandRecord: EvidenceRecord,
    demandingComparison: CandidateComparisonFact,
    proofSet: BoundedCausalProofSet
) extends BoundedCausalDependencyManifest:
  val contractKind = BoundedCausalContractKind.VacatedGateEnablesUnrecapturableSliderCapture
  require(
    proofSet.proposition.contractKind == contractKind &&
      proofSet.paths.forall(_.manifest.contractKind == contractKind),
    "a dependency manifest may contain only vacated-gate-enables-unrecapturable-slider-capture paths"
  )

  val stableKey: String =
    List(
      BoundedCausalIdentity.evidenceRecordKey(referenceLineRecord),
      BoundedCausalIdentity.evidenceRecordKey(playedLineRecord),
      BoundedCausalIdentity.evidenceRecordKey(demandRecord),
      CandidateComparisonSemanticKey.from(demandingComparison).stableKey,
      contractKind.toString.toLowerCase,
      proofSet.proposition.semanticId,
      proofSet.occurrence.occurrenceId,
      proofSet.paths.map(_.pathOccurrenceId).mkString("[", ",", "]")
    ).mkString("|")

  def consumes(
      comparison: CandidateComparisonFact,
      referenceSource: EvidenceRecord,
      playedSource: EvidenceRecord,
      demandSource: EvidenceRecord
  ): Boolean =
    demandingComparison == comparison && referenceLineRecord == referenceSource &&
      playedLineRecord == playedSource && demandRecord == demandSource

private final case class DirectLineAccessBoundAbsence(
    role: VacatedGateEnablesUnrecapturableSliderCaptureAbsenceRole,
    authority: ClosedRelationAbsenceAuthority
):
  def occurrence: ReplayPositionOccurrence = authority.occurrence

private object DirectLineAccessBoundAbsence:
  def forQuery(
      role: VacatedGateEnablesUnrecapturableSliderCaptureAbsenceRole,
      issuerRecord: EvidenceRecord,
      occurrence: ReplayPositionOccurrence,
      query: PositionRelationExtractor.ClosedRelationAbsenceQuery
  ): Option[DirectLineAccessBoundAbsence] =
    ClosedRelationAbsenceAuthority
      .forQuery(issuerRecord, occurrence, query)
      .map(DirectLineAccessBoundAbsence(role, _))

private final case class DirectLineAccessBoundState(
    role: VacatedGateEnablesUnrecapturableSliderCaptureStateRole,
    authority: ClosedPositionStateAuthority
):
  def occurrence: ReplayPositionOccurrence = authority.occurrence
  def proof: PositionRelationExtractor.ClosedPositionStateProof = authority.proof

private object DirectLineAccessBoundState:
  def certified(
      role: VacatedGateEnablesUnrecapturableSliderCaptureStateRole,
      issuerRecord: EvidenceRecord,
      occurrence: ReplayPositionOccurrence,
      proof: PositionRelationExtractor.ClosedPositionStateProof
  ): Option[DirectLineAccessBoundState] =
    ClosedPositionStateAuthority
      .certified(issuerRecord, occurrence, proof)
      .map(DirectLineAccessBoundState(role, _))

/** Opaque certified family result and sole graph-record proof authority. */
private[chessjudgment] final class CertifiedVacatedGateEnablesUnrecapturableSliderCapture private (
    val semantic: VacatedGateEnablesUnrecapturableSliderCaptureSemanticProof,
    val occurrence: VacatedGateEnablesUnrecapturableSliderCaptureOccurrence,
    val dependency: BoundedCausalDependencyFingerprint,
    private val dependencyManifest: VacatedGateEnablesUnrecapturableSliderCaptureDependencyManifest,
    private val referenceLineRecord: EvidenceRecord,
    private val playedLineRecord: EvidenceRecord,
    private val demandRecord: EvidenceRecord,
    private val demandingComparison: CandidateComparisonFact,
    private val referenceReplay: CanonicalLineReplay,
    private val playedReplay: CanonicalLineReplay,
    private val trajectory: LineAccessTrajectory,
    private val referenceRootReachOccurrence: ReplayVerticalRelationOccurrence,
    private val referenceExploitOccurrence: ReplayVerticalRelationOccurrence,
    private val absenceAuthorities: List[DirectLineAccessBoundAbsence],
    private val stateAuthorities: List[DirectLineAccessBoundState]
):
  require(
    semantic.identity == occurrence.proofSet.proposition &&
      dependencyManifest.proofSet == occurrence.proofSet &&
      BoundedCausalDependencyFingerprint.from(dependencyManifest) == dependency,
    "a certified vacated-gate-enables-unrecapturable-slider-capture result needs one proposition and complete dependency manifest"
  )

  def parentSources: List[EvidenceRef] =
    List(referenceLineRecord.ref, playedLineRecord.ref, demandRecord.ref).sortBy(_.id)

  private[chessjudgment] def lowerIssuerRecords: List[EvidenceRecord] =
    List(referenceLineRecord, playedLineRecord, demandRecord)

  def consumesDependencies(
      comparison: CandidateComparisonFact,
      referenceSource: EvidenceRecord,
      playedSource: EvidenceRecord,
      demandSource: EvidenceRecord
  ): Boolean =
    dependencyManifest.consumes(comparison, referenceSource, playedSource, demandSource)

  def proves(record: EvidenceRecord, payload: VacatedGateEnablesUnrecapturableSliderCaptureEvidence): Boolean =
    record.ref.producer == EvidenceProducer.CausalProofProducer &&
      record.ref.layer == EvidenceLayer.CausalProof &&
      record.ref.confidence == EvidenceConfidence.LegalReplayVerified &&
      record.ref.position == referenceLineRecord.ref.position &&
      record.ref.line.contains(occurrence.referenceLine) &&
      record.ref.scope == occurrence.referenceLine.role.scope &&
      record.parents == parentSources && payload.semantic == semantic &&
      payload.occurrence == occurrence && payload.dependencyFingerprint == dependency.value &&
      payload.occurrenceProof.contains(this) && remainsCertified

  def remainsCertified: Boolean =
    CertifiedComparedLineAuthority.exactRecord(
      referenceLineRecord,
      occurrence.referenceLine,
      referenceReplay
    ) &&
      CertifiedComparedLineAuthority.exactRecord(
        playedLineRecord,
        occurrence.playedLine,
        playedReplay
      ) &&
      ActionablePlayedVsBestCausalProofDemand.acceptsRecord(
        demandRecord,
        demandingComparison,
        referenceLineRecord.ref.position,
        referenceLineRecord,
        playedLineRecord
      ) &&
      demandingComparison.referenceLine == occurrence.referenceLine &&
      demandingComparison.candidateLine == occurrence.playedLine &&
      referenceLineRecord.ref.position == playedLineRecord.ref.position &&
      referenceReplay.replaySteps.take(occurrence.referenceSteps.size) == occurrence.referenceSteps &&
      playedReplay.replaySteps.take(occurrence.playedSteps.size) == occurrence.playedSteps &&
      semanticIdentityRemainsCertified && lowerOccurrencesRemainCertified &&
      pathRemainsCertified && closureAuthoritiesRemainCertified

  private def semanticIdentityRemainsCertified: Boolean =
    VacatedGateEnablesUnrecapturableSliderCaptureCausalAuthority.proposition(
      semantic.rootBoardState,
      semantic.enabler,
      semantic.slider,
      semantic.gateBlocker,
      semantic.exploit,
      semantic.capturedTarget
    ) == semantic.identity

  private def lowerOccurrencesRemainCertified: Boolean =
    val rootRebound = referenceReplay
      .verticalRelationOccurrences(
        occurrence.enablingStep,
        List(VerticalRelationContractKind.SliderReachDelta)
      )
      .filter(exact => ReplayVerticalRelationOccurrenceBinding.from(exact) == trajectory.relationOccurrenceAuthority.binding)
    val exploitRebound = referenceReplay
      .verticalRelationOccurrences(
        occurrence.exploitStep,
        List(VerticalRelationContractKind.CaptureRecaptureInventory)
      )
      .filter(exact => exact.occurrenceId == referenceExploitOccurrence.occurrenceId)
    val exactCapture = exploitRebound match
      case exact :: Nil =>
        exact.relation.detail match
          case capture: RelationWitnessDetail.CaptureRecaptureInventory =>
            capture.mover == semantic.exploit && capture.captured == semantic.capturedTarget &&
              capture.legalRecaptures.isEmpty && exact == referenceExploitOccurrence
          case _ => false
      case _ => false
    rootRebound == List(referenceRootReachOccurrence) && exactCapture &&
      trajectory.enablingStep == occurrence.enablingStep &&
      trajectory.enabledStep == occurrence.exploitStep &&
      trajectory.interveningSteps == occurrence.referenceInterveningSteps &&
      trajectory.enabledFrom == semantic.slider.square &&
      trajectory.enabledTo == semantic.exploit.to &&
      trajectory.vacatedSquares == List(semantic.gateBlocker.square) &&
      trajectory.persistenceStates.forall(state =>
        state.issuerEvidenceId == referenceLineRecord.ref.id &&
          state.issuerLine == occurrence.referenceLine &&
          state.scope == occurrence.referenceLine.role.scope
      )

  private def pathRemainsCertified: Boolean =
    occurrence.proofPaths match
      case path :: Nil =>
        path.manifest.isInstanceOf[VacatedGateEnablesUnrecapturableSliderCaptureManifest] &&
          path.premiseUses.size == 2 && path.closedAbsenceUses.size == 3 &&
          path.closedStateUses.size == stateAuthorities.size &&
          path.premiseUses.head.result == trajectory.relationOccurrenceAuthority.result &&
          path.premiseUses(1).result == DerivedRelationResultKey.from(referenceExploitOccurrence.relation) &&
          path.closedAbsenceUses.map(_.binding.role) == absenceAuthorities.map(_.role) &&
          path.closedAbsenceUses.map(_.binding.authority) == absenceAuthorities.map(_.authority) &&
          path.closedStateUses.map(_.binding.role) == stateAuthorities.map(_.role) &&
          parentSources.map(_.id).distinct.size == 3 &&
          dependencyManifest.consumes(
            demandingComparison,
            referenceLineRecord,
            playedLineRecord,
            demandRecord
          )
      case _ => false

  private def closureAuthoritiesRemainCertified: Boolean =
    absenceAuthorities.forall(_.authority.remainsCertified) && stateAuthorities.forall(authority =>
      authority.occurrence.certifies(authority.proof, authority.proof.scope) &&
        authority.occurrence
          .closedState(authority.proof.query, authority.proof.scope)
          .exists(authority.occurrence.certifies(_, authority.proof.scope))
    )

private[chessjudgment] object CertifiedVacatedGateEnablesUnrecapturableSliderCapture:
  private[judgment] def from(
      semantic: VacatedGateEnablesUnrecapturableSliderCaptureSemanticProof,
      occurrence: VacatedGateEnablesUnrecapturableSliderCaptureOccurrence,
      dependency: BoundedCausalDependencyFingerprint,
      dependencyManifest: VacatedGateEnablesUnrecapturableSliderCaptureDependencyManifest,
      referenceLineRecord: EvidenceRecord,
      playedLineRecord: EvidenceRecord,
      demandRecord: EvidenceRecord,
      demandingComparison: CandidateComparisonFact,
      referenceReplay: CanonicalLineReplay,
      playedReplay: CanonicalLineReplay,
      trajectory: LineAccessTrajectory,
      referenceRootReachOccurrence: ReplayVerticalRelationOccurrence,
      referenceExploitOccurrence: ReplayVerticalRelationOccurrence,
      absenceAuthorities: List[DirectLineAccessBoundAbsence],
      stateAuthorities: List[DirectLineAccessBoundState]
  ): CertifiedVacatedGateEnablesUnrecapturableSliderCapture =
    val certificate = new CertifiedVacatedGateEnablesUnrecapturableSliderCapture(
      semantic,
      occurrence,
      dependency,
      dependencyManifest,
      referenceLineRecord,
      playedLineRecord,
      demandRecord,
      demandingComparison,
      referenceReplay,
      playedReplay,
      trajectory,
      referenceRootReachOccurrence,
      referenceExploitOccurrence,
      absenceAuthorities,
      stateAuthorities
    )
    require(
      certificate.remainsCertified,
      "a vacated-gate-enables-unrecapturable-slider-capture certificate must retain all lower and closure authorities"
    )
    certificate

/** One independent changed-dependency route selected by the shared demand
  * dispatcher. Multiple root reach occurrences remain separate seeds and are
  * each allowed to prove (or fail to prove) their own physical path.
  */
private[chessjudgment] final case class VacatedGateEnablesUnrecapturableSliderCaptureChangedSeed private[chessjudgment] (
    rootReachOccurrence: ReplayVerticalRelationOccurrence,
    referenceExploitOccurrence: ReplayVerticalRelationOccurrence,
    referenceExploitIndex: Int
):
  require(
      rootReachOccurrence.contract == VerticalRelationContractKind.SliderReachDelta &&
      referenceExploitOccurrence.contract == VerticalRelationContractKind.CaptureRecaptureInventory &&
      referenceExploitIndex >= 2,
    "a vacated-gate-enables-unrecapturable-slider-capture seed needs one exact root reach and later capture occurrence"
  )

  private[chessjudgment] def stableKey: String =
    List(
      f"$referenceExploitIndex%08d",
      rootReachOccurrence.occurrenceId,
      referenceExploitOccurrence.occurrenceId
    ).mkString("|")

private[chessjudgment] object VacatedGateEnablesUnrecapturableSliderCaptureProof:
  private final case class ExactInputs(
      rootBoard: String,
      trajectory: LineAccessTrajectory,
      rootReachOccurrence: ReplayVerticalRelationOccurrence,
      rootReach: RelationWitnessDetail.SliderReachDelta,
      exploitOccurrence: ReplayVerticalRelationOccurrence,
      exploitCapture: RelationWitnessDetail.CaptureRecaptureInventory,
      slider: RelationColoredPieceWitness,
      gateBlocker: RelationColoredPieceWitness,
      referenceBranch: CausalBranchOccurrence,
      playedBranch: CausalBranchOccurrence,
      referenceImmediateRecaptureAbsence: DirectLineAccessBoundAbsence,
      playedExploitMoveAbsence: DirectLineAccessBoundAbsence,
      playedReplacementCaptureAbsence: DirectLineAccessBoundAbsence,
      referenceStateAuthorities: List[DirectLineAccessBoundState],
      playedStateAuthorities: List[DirectLineAccessBoundState]
  )

  /** Selected-family entry point. The dispatcher supplies exact root reach and
    * later-capture occurrences activated by the canonical L1 changed ledger;
    * every semantic, state, absence, and sibling obligation is still closed
    * below without rediscovering those selected occurrences.
    */
  private[chessjudgment] def deriveChangedDependencies(
      referenceLine: LineNodeRef,
      playedLine: LineNodeRef,
      referenceLineRecord: EvidenceRecord,
      playedLineRecord: EvidenceRecord,
      demandRecord: EvidenceRecord,
      demandingComparison: CandidateComparisonFact,
      referenceReplay: CanonicalLineReplay,
      playedReplay: CanonicalLineReplay,
      changedSeeds: List[VacatedGateEnablesUnrecapturableSliderCaptureChangedSeed]
  ): List[CertifiedVacatedGateEnablesUnrecapturableSliderCapture] =
    val seedKeys = changedSeeds.map(_.stableKey)
    require(
      seedKeys == seedKeys.distinct.sorted &&
        changedSeeds.forall(seed => referenceReplay.replaySteps.indices.contains(seed.referenceExploitIndex)),
      "vacated-gate-enables-unrecapturable-slider-capture demand needs unique, canonical exact occurrence seeds"
    )
    val admitted =
      CertifiedComparedLineAuthority.exactRecord(
        referenceLineRecord,
        referenceLine,
        referenceReplay
      ) &&
        CertifiedComparedLineAuthority.exactRecord(
          playedLineRecord,
          playedLine,
          playedReplay
        ) &&
        ActionablePlayedVsBestCausalProofDemand.acceptsRecord(
          demandRecord,
          demandingComparison,
          referenceLineRecord.ref.position,
          referenceLineRecord,
          playedLineRecord
        ) &&
        demandingComparison.referenceLine == referenceLine &&
        demandingComparison.candidateLine == playedLine &&
        referenceLineRecord.ref.position == playedLineRecord.ref.position &&
        !EvidenceRef.sameMove(referenceLine.rootMove, playedLine.rootMove)
    if !admitted then Nil
    else
      val referenceSteps = referenceReplay.replaySteps
      val playedSteps = playedReplay.replaySteps
      val exactInputs = for
        enablingStep <- referenceSteps.headOption.toList
        rootBoard <- PrincipalVariationEvidence.semanticBoardStateFen(enablingStep.fenBefore).toList
        playedRoot <- playedSteps.headOption.toList.flatMap(step =>
          PrincipalVariationEvidence.semanticBoardStateFen(step.fenBefore).toList
        )
        if rootBoard == playedRoot
        changedSeed <- changedSeeds
        exploitIndex = changedSeed.referenceExploitIndex
        exploitStep = referenceSteps(exploitIndex)
        exploitOccurrence = changedSeed.referenceExploitOccurrence
        if exploitOccurrence.step == exploitStep
        exploitCapture <- captureDetail(exploitOccurrence).toList
        if exploitCapture.legalRecaptures.isEmpty
        if playedSteps.size >= exploitIndex
        referenceIntervening = referenceSteps.slice(1, exploitIndex)
        if sameContinuation(referenceIntervening, playedSteps.slice(1, exploitIndex))
        trajectory <- LineAccessTrajectory.fromChangedRootClearanceOccurrence(
          enablingStep,
          exploitStep,
          referenceIntervening,
          referenceReplay,
          _ => Some(referenceLineRecord),
          changedSeed.rootReachOccurrence
        ).toList
        rootReachOccurrence = changedSeed.rootReachOccurrence
        rootReach <- sliderReachDetail(rootReachOccurrence).toList
        slider = RelationColoredPieceWitness(
          trajectory.enabledFrom,
          trajectory.enabledPieceRole,
          trajectory.color
        )
        gateBlocker <- exactGateBlocker(rootReach, trajectory).toList
        if exploitCapture.mover.side == slider.side &&
          exploitCapture.mover.from == slider.square &&
          exploitCapture.mover.beforeRole == slider.role &&
          exploitCapture.mover.afterRole == slider.role &&
          exploitCapture.mover.to == trajectory.enabledTo &&
          exploitCapture.captured.square == trajectory.enabledTo &&
          exploitCapture.captured.side != slider.side
        playedPreStep = playedSteps(exploitIndex - 1)
        playedOccurrence <- playedReplay.positionAfter(playedPreStep).toList
        referenceAfterExploit <- referenceReplay.positionAfter(exploitStep).toList
        referenceNoRecaptureQuery = PositionRelationExtractor.ClosedRelationAbsenceQuery.LegalCaptureOf(
          exploitCapture.captured.side,
          exploitCapture.mover.to
        )
        playedMoveAbsentQuery = PositionRelationExtractor.ClosedRelationAbsenceQuery.LegalMoveFromTo(
          slider.side,
          slider.square,
          exploitCapture.mover.to
        )
        playedCaptureAbsentQuery = PositionRelationExtractor.ClosedRelationAbsenceQuery.LegalCaptureOf(
          slider.side,
          exploitCapture.mover.to
        )
        playedSliderState <- occupiedState(playedOccurrence, slider, playedLine.role.scope).toList
        playedTargetState <- occupiedState(
          playedOccurrence,
          exploitCapture.captured,
          playedLine.role.scope
        ).toList
        playedGateState <- occupiedState(playedOccurrence, gateBlocker, playedLine.role.scope).toList
        playedBlockedReach <- playedOccurrence.existingSliderReachState(
          slider.side,
          RelationPieceWitness(slider.square, slider.role),
          rootReach.direction,
          playedLine.role.scope
        ).toList
        if blockedByExactGateBeforeTarget(
          playedBlockedReach,
          exploitCapture.mover.to,
          gateBlocker
        )
        referenceBranch = CausalBranchOccurrence.certifiedCounterfactual(
          VacatedGateEnablesUnrecapturableSliderCaptureBranchRole.CounterfactualReference,
          referenceLine,
          referenceReplay,
          exploitIndex + 1
        )
        playedBranch = CausalBranchOccurrence.observedRootWithAnalyzedContinuation(
          VacatedGateEnablesUnrecapturableSliderCaptureBranchRole.ObservedPlayedSibling,
          playedLine,
          playedReplay,
          exploitIndex
        )
        referenceStateAuthorities <- certifiedStates(
          referenceLineRecord,
          trajectory.persistenceStates.map(state =>
            (
              VacatedGateEnablesUnrecapturableSliderCaptureStateRole.ReferenceInterveningSliderReach,
              state.occurrence,
              state.proof
            )
          )
        ).toList
        if referenceStateAuthorities.map(_.occurrence.step) == referenceIntervening
        playedStateAuthorities <- certifiedStates(
          playedLineRecord,
          List(
            (
              VacatedGateEnablesUnrecapturableSliderCaptureStateRole.PlayedSliderOccupied,
              playedOccurrence,
              playedSliderState
            ),
            (
              VacatedGateEnablesUnrecapturableSliderCaptureStateRole.PlayedTargetOccupied,
              playedOccurrence,
              playedTargetState
            ),
            (
              VacatedGateEnablesUnrecapturableSliderCaptureStateRole.PlayedGateBlockerOccupied,
              playedOccurrence,
              playedGateState
            ),
            (
              VacatedGateEnablesUnrecapturableSliderCaptureStateRole.PlayedBlockedSliderReach,
              playedOccurrence,
              playedBlockedReach
            )
          )
        ).toList
        referenceImmediateRecaptureAbsence <- DirectLineAccessBoundAbsence.forQuery(
          VacatedGateEnablesUnrecapturableSliderCaptureAbsenceRole.ReferenceImmediateRecaptureAbsent,
          referenceLineRecord,
          referenceAfterExploit,
          referenceNoRecaptureQuery
        ).toList
        playedExploitMoveAbsence <- DirectLineAccessBoundAbsence.forQuery(
          VacatedGateEnablesUnrecapturableSliderCaptureAbsenceRole.PlayedExploitMoveAbsent,
          playedLineRecord,
          playedOccurrence,
          playedMoveAbsentQuery
        ).toList
        playedReplacementCaptureAbsence <- DirectLineAccessBoundAbsence.forQuery(
          VacatedGateEnablesUnrecapturableSliderCaptureAbsenceRole.PlayedReplacementCaptureAbsent,
          playedLineRecord,
          playedOccurrence,
          playedCaptureAbsentQuery
        ).toList
      yield ExactInputs(
        rootBoard,
        trajectory,
        rootReachOccurrence,
        rootReach,
        exploitOccurrence,
        exploitCapture,
        slider,
        gateBlocker,
        referenceBranch,
        playedBranch,
        referenceImmediateRecaptureAbsence,
        playedExploitMoveAbsence,
        playedReplacementCaptureAbsence,
        referenceStateAuthorities,
        playedStateAuthorities
      )

      exactInputs.map(input =>
        certify(
          input,
          referenceLineRecord,
          playedLineRecord,
          demandRecord,
          demandingComparison,
          referenceReplay,
          playedReplay
        )
      )

  private def certify(
      inputs: ExactInputs,
      referenceLineRecord: EvidenceRecord,
      playedLineRecord: EvidenceRecord,
      demandRecord: EvidenceRecord,
      demandingComparison: CandidateComparisonFact,
      referenceReplay: CanonicalLineReplay,
      playedReplay: CanonicalLineReplay
  ): CertifiedVacatedGateEnablesUnrecapturableSliderCapture =
    val proposition = VacatedGateEnablesUnrecapturableSliderCaptureCausalAuthority.proposition(
      inputs.rootBoard,
      inputs.rootReach.mover,
      inputs.slider,
      inputs.gateBlocker,
      inputs.exploitCapture.mover,
      inputs.exploitCapture.captured
    )
    val causalOccurrence = CausalOccurrenceIdentity.from(
      proposition,
      List(inputs.referenceBranch, inputs.playedBranch)
    )
    val rootReachUse = CausalVerticalRelationPremiseUse.from(
      VacatedGateEnablesUnrecapturableSliderCapturePremiseRole.ReferenceRootSliderReach,
      inputs.trajectory.relationOccurrenceAuthority,
      inputs.referenceBranch,
      0
    )
    val exploitIndex = inputs.referenceBranch.replaySteps.size - 1
    val exploitUse = CausalVerticalRelationPremiseUse.from(
      VacatedGateEnablesUnrecapturableSliderCapturePremiseRole.ReferenceExploitCapture,
      RecordBoundVerticalRelationOccurrence.certified(referenceLineRecord, inputs.exploitOccurrence).getOrElse(
        throw IllegalArgumentException("direct line access lost its graph-owned exploit occurrence")
      ),
      inputs.referenceBranch,
      exploitIndex
    )
    val referenceImmediateRecaptureAbsence = CausalClosedAbsenceBinding.afterStep(
      inputs.referenceImmediateRecaptureAbsence.role,
      inputs.referenceImmediateRecaptureAbsence.authority,
      inputs.referenceBranch,
      exploitIndex
    )
    val playedExploitMoveAbsence = CausalClosedAbsenceBinding.afterStep(
      inputs.playedExploitMoveAbsence.role,
      inputs.playedExploitMoveAbsence.authority,
      inputs.playedBranch,
      exploitIndex - 1
    )
    val playedReplacementCaptureAbsence = CausalClosedAbsenceBinding.afterStep(
      inputs.playedReplacementCaptureAbsence.role,
      inputs.playedReplacementCaptureAbsence.authority,
      inputs.playedBranch,
      exploitIndex - 1
    )
    val referenceStateBindings = inputs.referenceStateAuthorities.map { authority =>
      CausalClosedStateBinding.afterStep(
        authority.role,
        authority.authority,
        inputs.referenceBranch,
        exactBranchStepIndex(inputs.referenceBranch, authority.occurrence.step)
      )
    }
    val playedStateBindings = inputs.playedStateAuthorities.map(authority =>
      CausalClosedStateBinding.afterStep(
        authority.role,
        authority.authority,
        inputs.playedBranch,
        exactBranchStepIndex(inputs.playedBranch, authority.occurrence.step)
      )
    )
    val manifest = VacatedGateEnablesUnrecapturableSliderCaptureManifest.exact(
      rootReachUse,
      exploitUse,
      referenceImmediateRecaptureAbsence,
      playedExploitMoveAbsence,
      playedReplacementCaptureAbsence,
      referenceStateBindings,
      playedStateBindings
    )
    val path = CausalProofPathOccurrence.from(proposition, manifest)
    val proofSet = BoundedCausalProofSet.from(
      proposition,
      causalOccurrence,
      List(path)
    )
    val semantic = VacatedGateEnablesUnrecapturableSliderCaptureSemanticProof(
      proposition,
      inputs.rootReach.mover,
      inputs.slider,
      inputs.gateBlocker,
      inputs.exploitCapture.mover,
      inputs.exploitCapture.captured
    )
    val occurrence = VacatedGateEnablesUnrecapturableSliderCaptureOccurrence(proofSet)
    val dependencyManifest = VacatedGateEnablesUnrecapturableSliderCaptureDependencyManifest(
      referenceLineRecord,
      playedLineRecord,
      demandRecord,
      demandingComparison,
      proofSet
    )
    CertifiedVacatedGateEnablesUnrecapturableSliderCapture.from(
      semantic,
      occurrence,
      BoundedCausalDependencyFingerprint.from(dependencyManifest),
      dependencyManifest,
      referenceLineRecord,
      playedLineRecord,
      demandRecord,
      demandingComparison,
      referenceReplay,
      playedReplay,
      inputs.trajectory,
      inputs.rootReachOccurrence,
      inputs.exploitOccurrence,
      List(
        inputs.referenceImmediateRecaptureAbsence,
        inputs.playedExploitMoveAbsence,
        inputs.playedReplacementCaptureAbsence
      ),
      inputs.referenceStateAuthorities ++ inputs.playedStateAuthorities
    )

  private def exactBranchStepIndex(
      branch: CausalBranchOccurrence,
      step: LineReplayStep
  ): Int =
    branch.replaySteps.zipWithIndex.collect { case (candidate, index) if candidate == step => index } match
      case exact :: Nil => exact
      case _ =>
        throw IllegalArgumentException(
          "a closed state must bind one exact retained causal branch step"
        )

  private def sameContinuation(
      reference: List[LineReplayStep],
      played: List[LineReplayStep]
  ): Boolean =
    reference.size == played.size && reference.zip(played).forall { case (left, right) =>
      EvidenceRef.sameMove(left.moveUci, right.moveUci)
    }

  private def certifiedStates(
      issuerRecord: EvidenceRecord,
      states: List[(
        VacatedGateEnablesUnrecapturableSliderCaptureStateRole,
        ReplayPositionOccurrence,
        PositionRelationExtractor.ClosedPositionStateProof
      )]
  ): Option[List[DirectLineAccessBoundState]] =
    val certified = states.flatMap { case (role, occurrence, proof) =>
      DirectLineAccessBoundState.certified(role, issuerRecord, occurrence, proof)
    }
    Option.when(certified.size == states.size)(certified)

  private def captureDetail(
      occurrence: ReplayVerticalRelationOccurrence
  ): Option[RelationWitnessDetail.CaptureRecaptureInventory] =
    occurrence.relation.detail match
      case exact: RelationWitnessDetail.CaptureRecaptureInventory => Some(exact)
      case _                                                       => None

  private def sliderReachDetail(
      occurrence: ReplayVerticalRelationOccurrence
  ): Option[RelationWitnessDetail.SliderReachDelta] =
    occurrence.relation.detail match
      case exact: RelationWitnessDetail.SliderReachDelta => Some(exact)
      case _                                              => None

  private def exactGateBlocker(
      reach: RelationWitnessDetail.SliderReachDelta,
      trajectory: LineAccessTrajectory
  ): Option[RelationColoredPieceWitness] =
    val expected = RelationColoredPieceWitness(
      reach.mover.from,
      reach.mover.beforeRole,
      reach.mover.side
    )
    Option.when(
      reach.side == trajectory.color && reach.mover.side == reach.side &&
        reach.sliderBefore.contains(
          RelationPieceWitness(trajectory.enabledFrom, trajectory.enabledPieceRole)
        ) &&
        reach.sliderAfter.contains(
          RelationPieceWitness(trajectory.enabledFrom, trajectory.enabledPieceRole)
        ) &&
        reach.before.flatMap(_.firstOccupant).contains(expected) &&
        trajectory.vacatedSquares == List(expected.square)
    )(expected)

  private def occupiedState(
      occurrence: ReplayPositionOccurrence,
      piece: RelationColoredPieceWitness,
      scope: EvidenceScope
  ): Option[PositionRelationExtractor.ClosedPositionStateProof] =
    occurrence.closedState(
      PositionRelationExtractor.ClosedPositionStateQuery.OccupiedBy(piece),
      scope
    )

  private def blockedByExactGateBeforeTarget(
      proof: PositionRelationExtractor.ClosedPositionStateProof,
      target: EvidenceSquare,
      gateBlocker: RelationColoredPieceWitness
  ): Boolean =
    proof.query match
      case PositionRelationExtractor.ClosedPositionStateQuery.SliderReach(_, _, _, Some(reach)) =>
        reach.firstOccupant.contains(gateBlocker) && !reach.segment.exists(_.square == target)
      case _ => false
