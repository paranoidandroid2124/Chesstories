package lila.chessjudgment.model.judgment

import lila.chessjudgment.analysis.position.PositionRelationExtractor
import lila.chessjudgment.model.line.PrincipalVariationEvidence

private[chessjudgment] enum VacatedGateBranchRole extends CausalBranchRole:
  case GateVacatedThenCapture
  case GateRetained

  def stableKey: String =
    this match
      case GateVacatedThenCapture => "gate-vacated-then-capture"
      case GateRetained           => "gate-retained"

private[chessjudgment] enum VacatedGateEnablesUnrecapturableSliderCapturePremiseRole extends CausalPremiseRole:
  case GateVacatingSliderReach
  case LaterSliderCapture

  def stableKey: String =
    this match
      case GateVacatingSliderReach => "gate-vacating-slider-reach"
      case LaterSliderCapture      => "later-slider-capture"

private[chessjudgment] enum VacatedGateEnablesUnrecapturableSliderCaptureAbsenceRole extends CausalAbsenceRole:
  case LaterCaptureImmediateRecaptureAbsent
  case RetainedGateExploitMoveAbsent
  case RetainedGateReplacementCaptureAbsent

  def stableKey: String =
    this match
      case LaterCaptureImmediateRecaptureAbsent => "later-capture-immediate-recapture-absent"
      case RetainedGateExploitMoveAbsent        => "retained-gate-exploit-move-absent"
      case RetainedGateReplacementCaptureAbsent => "retained-gate-replacement-capture-absent"

private[chessjudgment] enum VacatedGateEnablesUnrecapturableSliderCaptureStateRole extends CausalStateRole:
  case VacatedGateInterveningSliderReach
  case VacatedGateTargetPersistence
  case RetainedGateSliderPersistence
  case RetainedGateTargetPersistence
  case RetainedGateBlockerPersistence
  case RetainedGateBlockedSliderReach

  def stableKey: String =
    this match
      case VacatedGateInterveningSliderReach => "vacated-gate-intervening-slider-reach"
      case VacatedGateTargetPersistence      => "vacated-gate-target-persistence"
      case RetainedGateSliderPersistence     => "retained-gate-slider-persistence"
      case RetainedGateTargetPersistence     => "retained-gate-target-persistence"
      case RetainedGateBlockerPersistence    => "retained-gate-blocker-persistence"
      case RetainedGateBlockedSliderReach    => "retained-gate-blocked-slider-reach"

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
      "the gate-vacating root mover must be the exact vacated gate blocker"
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
      BoundedCausalIdentity.coloredPieceKey(slider),
      BoundedCausalIdentity.coloredPieceKey(gateBlocker),
      exploit.stableKey,
      BoundedCausalIdentity.coloredPieceKey(capturedTarget)
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

private[chessjudgment] sealed trait VacatedGateEnablesUnrecapturableSliderCaptureManifest
    extends BoundedCausalContractManifest:
  def vacatedGateRootReach: CausalVerticalRelationPremiseUse
  def vacatedGateExploit: CausalVerticalRelationPremiseUse
  def vacatedGateNoRecapture: CausalClosedAbsenceBinding
  def retainedGateMoveAbsent: CausalClosedAbsenceBinding
  def retainedGateCaptureAbsent: CausalClosedAbsenceBinding
  def vacatedGatePersistence: List[CausalClosedStateBinding]
  def retainedGatePositionStates: List[CausalClosedStateBinding]

  final def contractKind = BoundedCausalContractKind.VacatedGateEnablesUnrecapturableSliderCapture
  final def premiseUses = List(vacatedGateRootReach, vacatedGateExploit)
  final def absenceBindings = List(vacatedGateNoRecapture, retainedGateMoveAbsent, retainedGateCaptureAbsent)
  final override def stateBindings = vacatedGatePersistence ++ retainedGatePositionStates
  final def stableKey: String =
    List(
      contractKind.toString.toLowerCase,
      premiseUses.map(_.stableKey).mkString("[", ",", "]"),
      absenceBindings.map(_.stableKey).mkString("[", ",", "]"),
      stateBindings.map(_.stableKey).mkString("[", ",", "]")
    ).mkString("|")

private[chessjudgment] object VacatedGateEnablesUnrecapturableSliderCaptureManifest:
  private final case class Exact(
      vacatedGateRootReach: CausalVerticalRelationPremiseUse,
      vacatedGateExploit: CausalVerticalRelationPremiseUse,
      vacatedGateNoRecapture: CausalClosedAbsenceBinding,
      retainedGateMoveAbsent: CausalClosedAbsenceBinding,
      retainedGateCaptureAbsent: CausalClosedAbsenceBinding,
      vacatedGatePersistence: List[CausalClosedStateBinding],
      retainedGatePositionStates: List[CausalClosedStateBinding]
  ) extends VacatedGateEnablesUnrecapturableSliderCaptureManifest

  def exact(
      vacatedGateRootReach: CausalVerticalRelationPremiseUse,
      vacatedGateExploit: CausalVerticalRelationPremiseUse,
      vacatedGateNoRecapture: CausalClosedAbsenceBinding,
      retainedGateMoveAbsent: CausalClosedAbsenceBinding,
      retainedGateCaptureAbsent: CausalClosedAbsenceBinding,
      vacatedGatePersistence: List[CausalClosedStateBinding],
      retainedGatePositionStates: List[CausalClosedStateBinding]
  ): VacatedGateEnablesUnrecapturableSliderCaptureManifest =
    val vacatedRole = VacatedGateBranchRole.GateVacatedThenCapture
    val retainedRole = VacatedGateBranchRole.GateRetained
    val exploitIndex = vacatedGateExploit.stepIndex
    val retainedGatePreExploitIndex = exploitIndex - 1
    require(
      vacatedGateRootReach.role == VacatedGateEnablesUnrecapturableSliderCapturePremiseRole.GateVacatingSliderReach &&
        vacatedGateRootReach.contract == VerticalRelationContractKind.SliderReachDelta &&
        vacatedGateRootReach.result.kind == RelationFactKind.SliderReachDelta &&
        vacatedGateRootReach.branchRole == vacatedRole && vacatedGateRootReach.stepIndex == 0,
      "vacated-gate-enables-unrecapturable-slider-capture must consume the exact gate-vacating root slider delta"
    )
    require(
      exploitIndex >= 2 &&
        vacatedGateExploit.role == VacatedGateEnablesUnrecapturableSliderCapturePremiseRole.LaterSliderCapture &&
        vacatedGateExploit.contract == VerticalRelationContractKind.CaptureRecaptureInventory &&
        vacatedGateExploit.result.kind == RelationFactKind.CaptureRecaptureInventory &&
        vacatedGateExploit.branchRole == vacatedRole &&
        vacatedGateExploit.branchId == vacatedGateRootReach.branchId,
      "vacated-gate-enables-unrecapturable-slider-capture must consume a later same-branch capture inventory"
    )
    require(
      vacatedGateNoRecapture.role ==
        VacatedGateEnablesUnrecapturableSliderCaptureAbsenceRole.LaterCaptureImmediateRecaptureAbsent &&
        vacatedGateNoRecapture.branchRole == vacatedRole &&
        vacatedGateNoRecapture.branchId == vacatedGateRootReach.branchId &&
        vacatedGateNoRecapture.afterStepIndex == exploitIndex,
      "the vacated-gate recapture absence must belong to the exploit destination"
    )
    require(
      List(retainedGateMoveAbsent, retainedGateCaptureAbsent).forall(binding =>
        binding.branchRole == retainedRole && binding.branchId == retainedGateMoveAbsent.branchId &&
          binding.afterStepIndex == retainedGatePreExploitIndex
      ) &&
        retainedGateMoveAbsent.role == VacatedGateEnablesUnrecapturableSliderCaptureAbsenceRole.RetainedGateExploitMoveAbsent &&
        retainedGateCaptureAbsent.role == VacatedGateEnablesUnrecapturableSliderCaptureAbsenceRole.RetainedGateReplacementCaptureAbsent,
      "retained-gate sibling exploit absences must share the exact pre-exploit occurrence"
    )
    def indicesFor(
        bindings: List[CausalClosedStateBinding],
        role: VacatedGateEnablesUnrecapturableSliderCaptureStateRole
    ): List[Int] = bindings.filter(_.role == role).map(_.afterStepIndex)
    require(
      vacatedGatePersistence.forall(binding =>
        binding.branchRole == vacatedRole && binding.branchId == vacatedGateRootReach.branchId
      ) && indicesFor(
        vacatedGatePersistence,
        VacatedGateEnablesUnrecapturableSliderCaptureStateRole.VacatedGateInterveningSliderReach
      ) == (1 until exploitIndex).toList && indicesFor(
        vacatedGatePersistence,
        VacatedGateEnablesUnrecapturableSliderCaptureStateRole.VacatedGateTargetPersistence
      ) == (0 until exploitIndex).toList,
      "the gate-vacated path needs continuous slider reach and exact target persistence"
    )
    require(
      retainedGatePositionStates.forall(binding =>
        binding.branchRole == retainedRole && binding.branchId == retainedGateMoveAbsent.branchId
      ) && List(
        VacatedGateEnablesUnrecapturableSliderCaptureStateRole.RetainedGateSliderPersistence,
        VacatedGateEnablesUnrecapturableSliderCaptureStateRole.RetainedGateTargetPersistence,
        VacatedGateEnablesUnrecapturableSliderCaptureStateRole.RetainedGateBlockerPersistence
      ).forall(role => indicesFor(retainedGatePositionStates, role) == (0 until exploitIndex).toList) &&
        indicesFor(
          retainedGatePositionStates,
          VacatedGateEnablesUnrecapturableSliderCaptureStateRole.RetainedGateBlockedSliderReach
        ) == List(retainedGatePreExploitIndex),
      "the sibling needs continuous slider, target, blocker, and final blocked reach states"
    )
    Exact(
      vacatedGateRootReach,
      vacatedGateExploit,
      vacatedGateNoRecapture,
      retainedGateMoveAbsent,
      retainedGateCaptureAbsent,
      vacatedGatePersistence,
      retainedGatePositionStates
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
    proofSet: BoundedCausalProofSet,
    subjectOccurrence: ExplanationSubjectOccurrence
):
  require(
    proofSet.proposition.contractKind == BoundedCausalContractKind.VacatedGateEnablesUnrecapturableSliderCapture,
    "a vacated-gate-enables-unrecapturable-slider-capture occurrence needs its exact contract"
  )
  require(
    proofSet.occurrence.branches.size == 2 &&
      proofSet.occurrence.branch(VacatedGateBranchRole.GateVacatedThenCapture).nonEmpty &&
      proofSet.occurrence.branch(VacatedGateBranchRole.GateRetained).nonEmpty,
    "a vacated-gate occurrence needs its gate-vacated and gate-retained branches"
  )
  require(vacatedGateSteps.size >= 3 && retainedGateSteps.size == vacatedGateSteps.size - 1)
  require(ownsSubject, "the proof occurrence must retain its exact requested root occurrence")

  private def exactBranch(role: VacatedGateBranchRole): CausalBranchOccurrence =
    proofSet.occurrence
      .branch(role)
      .getOrElse(
        throw IllegalStateException(
          s"a vacated-gate-enables-unrecapturable-slider-capture occurrence lost its $role branch"
        )
      )

  def semanticId: String = proofSet.proposition.semanticId
  def occurrenceId: String = proofSet.occurrence.occurrenceId
  def vacatedGateBranch: CausalBranchOccurrence = exactBranch(VacatedGateBranchRole.GateVacatedThenCapture)
  def retainedGateBranch: CausalBranchOccurrence = exactBranch(VacatedGateBranchRole.GateRetained)
  def vacatedGateLine: LineNodeRef = vacatedGateBranch.line
  def retainedGateLine: LineNodeRef = retainedGateBranch.line
  def vacatedGateSteps: List[LineReplayStep] = vacatedGateBranch.replaySteps
  def retainedGateSteps: List[LineReplayStep] = retainedGateBranch.replaySteps
  def proofPaths: List[CausalProofPathOccurrence] = proofSet.paths
  def enablingStep: LineReplayStep = vacatedGateSteps.head
  def exploitStep: LineReplayStep = vacatedGateSteps.last
  def vacatedGateInterveningSteps: List[LineReplayStep] = vacatedGateSteps.slice(1, vacatedGateSteps.size - 1)
  def retainedGatePreExploitStep: LineReplayStep = retainedGateSteps.last

  private def ownsSubject: Boolean =
    List(vacatedGateBranch, retainedGateBranch).exists { branch =>
      branch.line.id == subjectOccurrence.line.id &&
      branch.lineOwnerEvidenceId == subjectOccurrence.lineOwnerEvidenceId &&
      branch.rootTransitionEvidenceId == subjectOccurrence.transitionEvidenceId &&
      branch.rootProvenance == subjectOccurrence.rootProvenance &&
      branch.steps.headOption.exists(root =>
        EvidenceRef.sameMove(root.step.moveUci, subjectOccurrence.moveUci) &&
          root.step.ply == subjectOccurrence.destination.ply &&
          PrincipalVariationEvidence.sameBoardState(root.step.fenBefore, subjectOccurrence.start.fen) &&
          PrincipalVariationEvidence.sameBoardState(root.step.fenAfter, subjectOccurrence.destination.fen)
      )
    }

private[chessjudgment] final case class VacatedGateEnablesUnrecapturableSliderCaptureDependencyManifest private[chessjudgment] (
    subject: CertifiedRootOccurrence,
    vacatedGate: CertifiedRootOccurrence,
    retainedGate: CertifiedRootOccurrence,
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
      "subject-root",
      BoundedCausalIdentity.evidenceRecordKey(subject.lineOwner),
      BoundedCausalIdentity.evidenceRecordKey(subject.transitionOwner),
      "vacated-gate-root",
      BoundedCausalIdentity.evidenceRecordKey(vacatedGate.lineOwner),
      BoundedCausalIdentity.evidenceRecordKey(vacatedGate.transitionOwner),
      "retained-gate-root",
      BoundedCausalIdentity.evidenceRecordKey(retainedGate.lineOwner),
      BoundedCausalIdentity.evidenceRecordKey(retainedGate.transitionOwner),
      contractKind.toString.toLowerCase,
      proofSet.proposition.semanticId,
      proofSet.occurrence.occurrenceId,
      proofSet.paths.map(_.pathOccurrenceId).mkString("[", ",", "]")
    ).mkString("|")

  def consumes(
      exactSubject: CertifiedRootOccurrence,
      exactVacatedGate: CertifiedRootOccurrence,
      exactRetainedGate: CertifiedRootOccurrence
  ): Boolean =
    subject == exactSubject && vacatedGate == exactVacatedGate && retainedGate == exactRetainedGate

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
    val subject: CertifiedRootOccurrence,
    private val vacatedGate: CertifiedRootOccurrence,
    private val retainedGate: CertifiedRootOccurrence,
    private val trajectory: LineAccessTrajectory,
    private val vacancyClosure: VacancySiblingClosure,
    private val vacatedGateRootReachOccurrence: ReplayVerticalRelationOccurrence,
    private val vacatedGateExploitOccurrence: ReplayVerticalRelationOccurrence,
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
    val sources = List(
      vacatedGate.lineOwner.ref,
      vacatedGate.transitionOwner.ref,
      retainedGate.lineOwner.ref,
      retainedGate.transitionOwner.ref
    ).sortBy(_.id)
    require(sources.map(_.id).distinct.size == sources.size, "the two roots need distinct line and transition owners")
    sources

  private[chessjudgment] def lowerIssuerRecords: List[EvidenceRecord] =
    List(vacatedGate.lineOwner, vacatedGate.transitionOwner, retainedGate.lineOwner, retainedGate.transitionOwner)

  def consumesDependencies(
      exactSubject: CertifiedRootOccurrence,
      exactVacatedGate: CertifiedRootOccurrence,
      exactRetainedGate: CertifiedRootOccurrence
  ): Boolean = dependencyManifest.consumes(exactSubject, exactVacatedGate, exactRetainedGate)

  def proves(record: EvidenceRecord, payload: VacatedGateEnablesUnrecapturableSliderCaptureEvidence): Boolean =
    record.ref.producer == EvidenceProducer.CausalProofProducer &&
      record.ref.layer == EvidenceLayer.CausalProof &&
      record.ref.confidence == EvidenceConfidence.LegalReplayVerified &&
      record.ref.position == subject.transition.from &&
      record.ref.line.contains(subject.line) &&
      record.ref.scope == subject.transitionOwner.ref.scope &&
      record.parents == parentSources && payload.semantic == semantic &&
      payload.occurrence == occurrence && payload.dependencyFingerprint == dependency.value &&
      payload.subjectOccurrence == subject.publicOccurrence &&
      payload.occurrenceProof == this && remainsCertified

  def remainsCertified: Boolean =
    subject.remainsCertified && vacatedGate.remainsCertified && retainedGate.remainsCertified &&
      (subject == vacatedGate || subject == retainedGate) &&
      occurrence.subjectOccurrence == subject.publicOccurrence &&
      occurrence.vacatedGateLine == vacatedGate.line && occurrence.retainedGateLine == retainedGate.line &&
      vacatedGate.replay.replaySteps.take(occurrence.vacatedGateSteps.size) == occurrence.vacatedGateSteps &&
      retainedGate.replay.replaySteps.take(occurrence.retainedGateSteps.size) == occurrence.retainedGateSteps &&
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
    val rootRebound = vacatedGate.replay
      .verticalRelationOccurrences(
        occurrence.enablingStep,
        List(VerticalRelationContractKind.SliderReachDelta)
      )
      .filter(exact => ReplayVerticalRelationOccurrenceBinding.from(exact) == trajectory.relationOccurrenceAuthority.binding)
    val exploitRebound = vacatedGate.replay
      .verticalRelationOccurrences(
        occurrence.exploitStep,
        List(VerticalRelationContractKind.CaptureRecaptureInventory)
      )
      .filter(exact => exact.occurrenceId == vacatedGateExploitOccurrence.occurrenceId)
    val exactCapture = exploitRebound match
      case exact :: Nil =>
        exact.relation.detail match
          case capture: RelationWitnessDetail.CaptureRecaptureInventory =>
            capture.mover == semantic.exploit && capture.captured == semantic.capturedTarget &&
              capture.legalRecaptures.isEmpty && exact == vacatedGateExploitOccurrence
          case _ => false
      case _ => false
    rootRebound == List(vacatedGateRootReachOccurrence) && exactCapture &&
      trajectory.enablingStep == occurrence.enablingStep &&
      trajectory.enabledStep == occurrence.exploitStep &&
      trajectory.interveningSteps == occurrence.vacatedGateInterveningSteps &&
      trajectory.enabledFrom == semantic.slider.square &&
      trajectory.enabledTo == semantic.exploit.to &&
      trajectory.vacatedSquares == List(semantic.gateBlocker.square) &&
      trajectory.persistenceStates.forall(state =>
        state.issuerEvidenceId == vacatedGate.lineOwner.ref.id &&
          state.issuerLine == occurrence.vacatedGateLine &&
          state.scope == EvidenceScope.LegalLine
      )

  private def pathRemainsCertified: Boolean =
    occurrence.proofPaths match
      case path :: Nil =>
        val stateBindings = path.closedStateUses.map(_.binding)
        val blockerStates = stateBindings.filter(
          _.role == VacatedGateEnablesUnrecapturableSliderCaptureStateRole.RetainedGateBlockerPersistence
        )
        val moverStates = stateBindings.filter(
          _.role == VacatedGateEnablesUnrecapturableSliderCaptureStateRole.RetainedGateSliderPersistence
        )
        val vacatedGateTargetStates = stateBindings.filter(
          _.role == VacatedGateEnablesUnrecapturableSliderCaptureStateRole.VacatedGateTargetPersistence
        )
        val retainedGateTargetStates = stateBindings.filter(
          _.role == VacatedGateEnablesUnrecapturableSliderCaptureStateRole.RetainedGateTargetPersistence
        )
        val moveAbsence = path.closedAbsenceUses.map(_.binding).filter(
          _.role == VacatedGateEnablesUnrecapturableSliderCaptureAbsenceRole.RetainedGateExploitMoveAbsent
        )
        path.manifest.isInstanceOf[VacatedGateEnablesUnrecapturableSliderCaptureManifest] &&
          path.premiseUses.size == 2 && path.closedAbsenceUses.size == 3 &&
          path.closedStateUses.size == stateAuthorities.size &&
          path.premiseUses.head.result == trajectory.relationOccurrenceAuthority.result &&
          path.premiseUses(1).result == DerivedRelationResultKey.from(vacatedGateExploitOccurrence.relation) &&
          path.closedAbsenceUses.map(_.binding.role) == absenceAuthorities.map(_.role) &&
          path.closedAbsenceUses.map(_.binding.authority) == absenceAuthorities.map(_.authority) &&
          path.closedStateUses.map(_.binding.role) == stateAuthorities.map(_.role) &&
          blockerStates.size == occurrence.vacatedGateSteps.size - 1 &&
          moverStates.size == occurrence.vacatedGateSteps.size - 1 &&
          vacatedGateTargetStates.size == occurrence.vacatedGateSteps.size - 1 &&
          retainedGateTargetStates.size == occurrence.vacatedGateSteps.size - 1 &&
          (vacatedGateTargetStates ++ retainedGateTargetStates).forall(_.query ==
            PositionRelationExtractor.ClosedPositionStateQuery.OccupiedBy(semantic.capturedTarget)) &&
          moveAbsence.size == 1 &&
          vacancyClosure.matches(
            semantic.enabler,
            occurrence.enablingStep.moveUci,
            semantic.gateBlocker,
            semantic.exploit,
            occurrence.exploitStep.moveUci,
            semantic.slider,
            occurrence.vacatedGateBranch,
            occurrence.retainedGateBranch,
            occurrence.vacatedGateSteps.size - 1,
            blockerStates,
            moverStates,
            moveAbsence.head
          ) &&
          parentSources.map(_.id).distinct.size == 4 &&
          dependencyManifest.consumes(subject, vacatedGate, retainedGate)
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
      subject: CertifiedRootOccurrence,
      vacatedGate: CertifiedRootOccurrence,
      retainedGate: CertifiedRootOccurrence,
      trajectory: LineAccessTrajectory,
      vacancyClosure: VacancySiblingClosure,
      vacatedGateRootReachOccurrence: ReplayVerticalRelationOccurrence,
      vacatedGateExploitOccurrence: ReplayVerticalRelationOccurrence,
      absenceAuthorities: List[DirectLineAccessBoundAbsence],
      stateAuthorities: List[DirectLineAccessBoundState]
  ): CertifiedVacatedGateEnablesUnrecapturableSliderCapture =
    val certificate = new CertifiedVacatedGateEnablesUnrecapturableSliderCapture(
      semantic,
      occurrence,
      dependency,
      dependencyManifest,
      subject,
      vacatedGate,
      retainedGate,
      trajectory,
      vacancyClosure,
      vacatedGateRootReachOccurrence,
      vacatedGateExploitOccurrence,
      absenceAuthorities,
      stateAuthorities
    )
    require(
      certificate.remainsCertified,
      "a vacated-gate-enables-unrecapturable-slider-capture certificate must retain all lower and closure authorities"
    )
    certificate

/** One independent changed-dependency route selected by the shared demand
  * dispatcher. Multiple root reach occurrences remain separate demands and are
  * each allowed to prove (or fail to prove) their own physical path.
  */
private[chessjudgment] final case class VacatedGateEnablesUnrecapturableSliderCaptureDemand private[chessjudgment] (
    rootReachOccurrence: ReplayVerticalRelationOccurrence,
    vacatedGateExploitOccurrence: ReplayVerticalRelationOccurrence,
    vacatedGateExploitIndex: Int
):
  require(
      rootReachOccurrence.contract == VerticalRelationContractKind.SliderReachDelta &&
      vacatedGateExploitOccurrence.contract == VerticalRelationContractKind.CaptureRecaptureInventory &&
      vacatedGateExploitIndex >= 2,
    "a vacated-gate-enables-unrecapturable-slider-capture demand needs one exact root reach and later capture occurrence"
  )

  private[chessjudgment] def stableKey: String =
    List(
      f"$vacatedGateExploitIndex%08d",
      rootReachOccurrence.occurrenceId,
      vacatedGateExploitOccurrence.occurrenceId
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
      vacatedGateCausalBranch: CausalBranchOccurrence,
      retainedGateCausalBranch: CausalBranchOccurrence,
      vacatedGateImmediateRecaptureAbsence: DirectLineAccessBoundAbsence,
      retainedGateExploitMoveAbsence: DirectLineAccessBoundAbsence,
      retainedGateReplacementCaptureAbsence: DirectLineAccessBoundAbsence,
      vacatedGateStateAuthorities: List[DirectLineAccessBoundState],
      retainedGateStateAuthorities: List[DirectLineAccessBoundState]
  )

  /** Selected-family entry point. The dispatcher supplies exact root reach and
    * later-capture occurrences activated by the canonical L1 changed ledger;
    * every semantic, state, absence, and sibling obligation is still closed
    * below without rediscovering those selected occurrences.
    */
  private[chessjudgment] def certifyDemanded(
      subject: CertifiedRootOccurrence,
      vacatedGate: CertifiedRootOccurrence,
      retainedGate: CertifiedRootOccurrence,
      demands: List[VacatedGateEnablesUnrecapturableSliderCaptureDemand]
  ): List[CertifiedVacatedGateEnablesUnrecapturableSliderCapture] =
    val demandKeys = demands.map(_.stableKey)
    require(
      demandKeys == demandKeys.distinct.sorted &&
        demands.forall(demand => vacatedGate.replay.replaySteps.indices.contains(demand.vacatedGateExploitIndex)),
      "vacated-gate-enables-unrecapturable-slider-capture needs unique canonical exact occurrence demands"
    )
    val admitted = subject.remainsCertified && vacatedGate.remainsCertified && retainedGate.remainsCertified &&
      subject.isObserved && (subject == vacatedGate || subject == retainedGate) &&
      vacatedGate.transition.from == retainedGate.transition.from &&
      !EvidenceRef.sameMove(vacatedGate.line.rootMove, retainedGate.line.rootMove)
    if !admitted then Nil
    else
      val vacatedGateSteps = vacatedGate.replay.replaySteps
      val retainedGateSteps = retainedGate.replay.replaySteps
      val exactInputs = for
        enablingStep <- vacatedGateSteps.headOption.toList
        rootBoard <- PrincipalVariationEvidence.semanticBoardStateFen(enablingStep.fenBefore).toList
        retainedGateRoot <- retainedGateSteps.headOption.toList.flatMap(step =>
          PrincipalVariationEvidence.semanticBoardStateFen(step.fenBefore).toList
        )
        if rootBoard == retainedGateRoot
        demand <- demands
        exploitIndex = demand.vacatedGateExploitIndex
        exploitStep = vacatedGateSteps(exploitIndex)
        exploitOccurrence = demand.vacatedGateExploitOccurrence
        if exploitOccurrence.step == exploitStep
        exploitCapture <- captureDetail(exploitOccurrence).toList
        if exploitCapture.legalRecaptures.isEmpty
        if retainedGateSteps.size >= exploitIndex
        vacatedGateIntervening = vacatedGateSteps.slice(1, exploitIndex)
        if sameContinuation(vacatedGateIntervening, retainedGateSteps.slice(1, exploitIndex))
        trajectory <- LineAccessTrajectory.fromChangedRootClearanceOccurrence(
          enablingStep,
          exploitStep,
          vacatedGateIntervening,
            vacatedGate.replay,
          _ => Some(vacatedGate.lineOwner),
          demand.rootReachOccurrence
        ).toList
        rootReachOccurrence = demand.rootReachOccurrence
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
        retainedGatePreStep = retainedGateSteps(exploitIndex - 1)
        retainedGateOccurrence <- retainedGate.replay.positionAfter(retainedGatePreStep).toList
        vacatedGateAfterExploit <- vacatedGate.replay.positionAfter(exploitStep).toList
        vacatedGateNoRecaptureQuery = PositionRelationExtractor.ClosedRelationAbsenceQuery.LegalCaptureOf(
          exploitCapture.captured.side,
          exploitCapture.mover.to
        )
        retainedGateMoveAbsentQuery = PositionRelationExtractor.ClosedRelationAbsenceQuery.LegalMoveFromTo(
          slider.side,
          slider.square,
          exploitCapture.mover.to
        )
        retainedGateCaptureAbsentQuery = PositionRelationExtractor.ClosedRelationAbsenceQuery.LegalCaptureOf(
          slider.side,
          exploitCapture.mover.to
        )
        vacatedGateTargetStates = vacatedGateSteps.take(exploitIndex).flatMap(step =>
          vacatedGate.replay.positionAfter(step).flatMap(occurrence =>
            occupiedState(
              occurrence,
              exploitCapture.captured,
              EvidenceScope.LegalLine
            ).map(proof =>
              (
                VacatedGateEnablesUnrecapturableSliderCaptureStateRole.VacatedGateTargetPersistence,
                occurrence,
                proof
              )
            )
          )
        )
        if vacatedGateTargetStates.size == exploitIndex
        retainedGatePrefixStates = retainedGateSteps.take(exploitIndex).flatMap(step =>
          retainedGate.replay.positionAfter(step).toList.flatMap(occurrence =>
            List(
              VacatedGateEnablesUnrecapturableSliderCaptureStateRole.RetainedGateSliderPersistence -> slider,
              VacatedGateEnablesUnrecapturableSliderCaptureStateRole.RetainedGateTargetPersistence ->
                exploitCapture.captured,
              VacatedGateEnablesUnrecapturableSliderCaptureStateRole.RetainedGateBlockerPersistence ->
                gateBlocker
            ).flatMap { case (role, piece) =>
              occupiedState(occurrence, piece, EvidenceScope.LegalLine).map(proof =>
                (role, occurrence, proof)
              )
            }
          )
        )
        if retainedGatePrefixStates.size == exploitIndex * 3
        retainedGateBlockedReach <- retainedGateOccurrence.existingSliderReachState(
          slider.side,
          RelationPieceWitness(slider.square, slider.role),
          rootReach.direction,
          EvidenceScope.LegalLine
        ).toList
        if blockedByExactGateBeforeTarget(
          retainedGateBlockedReach,
          exploitCapture.mover.to,
          gateBlocker
        )
        vacatedGateBranch = CausalBranchOccurrence.fromRootOccurrence(
          VacatedGateBranchRole.GateVacatedThenCapture,
          vacatedGate,
          exploitIndex + 1
        )
        retainedGateBranch = CausalBranchOccurrence.fromRootOccurrence(
          VacatedGateBranchRole.GateRetained,
          retainedGate,
          exploitIndex
        )
        vacatedGateStateAuthorities <- certifiedStates(
          vacatedGate.lineOwner,
          trajectory.persistenceStates.map(state =>
            (
              VacatedGateEnablesUnrecapturableSliderCaptureStateRole.VacatedGateInterveningSliderReach,
              state.occurrence,
              state.proof
            )
          ) ++ vacatedGateTargetStates
        ).toList
        if vacatedGateStateAuthorities.count(
          _.role == VacatedGateEnablesUnrecapturableSliderCaptureStateRole.VacatedGateInterveningSliderReach
        ) == vacatedGateIntervening.size
        retainedGateStateAuthorities <- certifiedStates(
          retainedGate.lineOwner,
          retainedGatePrefixStates ++ List(
            (
              VacatedGateEnablesUnrecapturableSliderCaptureStateRole.RetainedGateBlockedSliderReach,
              retainedGateOccurrence,
              retainedGateBlockedReach
            )
          )
        ).toList
        vacatedGateImmediateRecaptureAbsence <- DirectLineAccessBoundAbsence.forQuery(
          VacatedGateEnablesUnrecapturableSliderCaptureAbsenceRole.LaterCaptureImmediateRecaptureAbsent,
          vacatedGate.lineOwner,
          vacatedGateAfterExploit,
          vacatedGateNoRecaptureQuery
        ).toList
        retainedGateExploitMoveAbsence <- DirectLineAccessBoundAbsence.forQuery(
          VacatedGateEnablesUnrecapturableSliderCaptureAbsenceRole.RetainedGateExploitMoveAbsent,
          retainedGate.lineOwner,
          retainedGateOccurrence,
          retainedGateMoveAbsentQuery
        ).toList
        retainedGateReplacementCaptureAbsence <- DirectLineAccessBoundAbsence.forQuery(
          VacatedGateEnablesUnrecapturableSliderCaptureAbsenceRole.RetainedGateReplacementCaptureAbsent,
          retainedGate.lineOwner,
          retainedGateOccurrence,
          retainedGateCaptureAbsentQuery
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
        vacatedGateBranch,
        retainedGateBranch,
        vacatedGateImmediateRecaptureAbsence,
        retainedGateExploitMoveAbsence,
        retainedGateReplacementCaptureAbsence,
        vacatedGateStateAuthorities,
        retainedGateStateAuthorities
      )

      exactInputs.map(input =>
        certify(
          input,
          subject,
          vacatedGate,
          retainedGate
        )
      )

  private def certify(
      inputs: ExactInputs,
      subject: CertifiedRootOccurrence,
      vacatedGate: CertifiedRootOccurrence,
      retainedGate: CertifiedRootOccurrence
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
      List(inputs.vacatedGateCausalBranch, inputs.retainedGateCausalBranch)
    )
    val rootReachUse = CausalVerticalRelationPremiseUse.from(
      VacatedGateEnablesUnrecapturableSliderCapturePremiseRole.GateVacatingSliderReach,
      inputs.trajectory.relationOccurrenceAuthority,
      inputs.vacatedGateCausalBranch,
      0
    )
    val exploitIndex = inputs.vacatedGateCausalBranch.replaySteps.size - 1
    val exploitUse = CausalVerticalRelationPremiseUse.from(
      VacatedGateEnablesUnrecapturableSliderCapturePremiseRole.LaterSliderCapture,
      RecordBoundVerticalRelationOccurrence.certified(vacatedGate.lineOwner, inputs.exploitOccurrence).getOrElse(
        throw IllegalArgumentException("a vacated-gate proof lost its graph-owned exploit occurrence")
      ),
      inputs.vacatedGateCausalBranch,
      exploitIndex
    )
    val vacatedGateImmediateRecaptureAbsence = CausalClosedAbsenceBinding.afterStep(
      inputs.vacatedGateImmediateRecaptureAbsence.role,
      inputs.vacatedGateImmediateRecaptureAbsence.authority,
      inputs.vacatedGateCausalBranch,
      exploitIndex
    )
    val retainedGateExploitMoveAbsence = CausalClosedAbsenceBinding.afterStep(
      inputs.retainedGateExploitMoveAbsence.role,
      inputs.retainedGateExploitMoveAbsence.authority,
      inputs.retainedGateCausalBranch,
      exploitIndex - 1
    )
    val retainedGateReplacementCaptureAbsence = CausalClosedAbsenceBinding.afterStep(
      inputs.retainedGateReplacementCaptureAbsence.role,
      inputs.retainedGateReplacementCaptureAbsence.authority,
      inputs.retainedGateCausalBranch,
      exploitIndex - 1
    )
    val vacatedGateStateBindings = inputs.vacatedGateStateAuthorities.map { authority =>
      CausalClosedStateBinding.afterStep(
        authority.role,
        authority.authority,
        inputs.vacatedGateCausalBranch,
        exactBranchStepIndex(inputs.vacatedGateCausalBranch, authority.occurrence.step)
      )
    }
    val retainedGateStateBindings = inputs.retainedGateStateAuthorities.map(authority =>
      CausalClosedStateBinding.afterStep(
        authority.role,
        authority.authority,
        inputs.retainedGateCausalBranch,
        exactBranchStepIndex(inputs.retainedGateCausalBranch, authority.occurrence.step)
      )
    )
    def exactRetainedGateStates(
        role: VacatedGateEnablesUnrecapturableSliderCaptureStateRole
    ): List[CausalClosedStateBinding] =
      val exact = retainedGateStateBindings.filter(_.role == role).sortBy(_.afterStepIndex)
      require(
        exact.map(_.afterStepIndex) == (0 until exploitIndex).toList,
        s"a vacated-gate proof lost its continuous $role retained-gate states"
      )
      exact
    val vacancyClosure = VacancySiblingClosure.certified(
      inputs.rootReach.mover,
      inputs.vacatedGateCausalBranch.replaySteps.head.moveUci,
      inputs.gateBlocker,
      inputs.exploitCapture.mover,
      inputs.vacatedGateCausalBranch.replaySteps(exploitIndex).moveUci,
      inputs.slider,
      inputs.vacatedGateCausalBranch,
      inputs.retainedGateCausalBranch,
      exploitIndex,
      exactRetainedGateStates(
        VacatedGateEnablesUnrecapturableSliderCaptureStateRole.RetainedGateBlockerPersistence
      ),
      exactRetainedGateStates(
        VacatedGateEnablesUnrecapturableSliderCaptureStateRole.RetainedGateSliderPersistence
      ),
      retainedGateExploitMoveAbsence
    ).getOrElse(
      throw IllegalArgumentException("a vacated-gate proof failed the shared vacancy-move kernel")
    )
    val manifest = VacatedGateEnablesUnrecapturableSliderCaptureManifest.exact(
      rootReachUse,
      exploitUse,
      vacatedGateImmediateRecaptureAbsence,
      retainedGateExploitMoveAbsence,
      retainedGateReplacementCaptureAbsence,
      vacatedGateStateBindings,
      retainedGateStateBindings
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
    val occurrence = VacatedGateEnablesUnrecapturableSliderCaptureOccurrence(proofSet, subject.publicOccurrence)
    val dependencyManifest = VacatedGateEnablesUnrecapturableSliderCaptureDependencyManifest(
      subject,
      vacatedGate,
      retainedGate,
      proofSet
    )
    CertifiedVacatedGateEnablesUnrecapturableSliderCapture.from(
      semantic,
      occurrence,
      BoundedCausalDependencyFingerprint.from(dependencyManifest),
      dependencyManifest,
      subject,
      vacatedGate,
      retainedGate,
      inputs.trajectory,
      vacancyClosure,
      inputs.rootReachOccurrence,
      inputs.exploitOccurrence,
      List(
        inputs.vacatedGateImmediateRecaptureAbsence,
        inputs.retainedGateExploitMoveAbsence,
        inputs.retainedGateReplacementCaptureAbsence
      ),
      inputs.vacatedGateStateAuthorities ++ inputs.retainedGateStateAuthorities
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
      vacatedGate: List[LineReplayStep],
      retainedGate: List[LineReplayStep]
  ): Boolean =
    vacatedGate.size == retainedGate.size && vacatedGate.zip(retainedGate).forall { case (left, right) =>
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
