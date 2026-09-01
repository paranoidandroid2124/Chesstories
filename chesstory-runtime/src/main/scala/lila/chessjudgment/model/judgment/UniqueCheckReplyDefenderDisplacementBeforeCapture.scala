package lila.chessjudgment.model.judgment

import lila.chessjudgment.analysis.position.PositionRelationExtractor
import lila.chessjudgment.model.line.PrincipalVariationEvidence

private[chessjudgment] enum UniqueCheckReplyBranchRole extends CausalBranchRole:
  case DisplacementThenCapture
  case ImmediateCaptureWithDefender

  def stableKey: String =
    this match
      case DisplacementThenCapture       => "displacement-then-capture"
      case ImmediateCaptureWithDefender  => "immediate-capture-with-defender"

/** Sole family construction authority. It validates and emits the exact
  * semantic descriptor consumed by the common identity core.
  */
private[chessjudgment] object UniqueCheckReplyDefenderDisplacementBeforeCaptureCausalAuthority:
  private final case class PropositionDescriptor(
      rootFen: String,
      trigger: RelationMoveTransitionWitness,
      forcedReply: RelationLegalMoveResourceWitness,
      realizer: RelationMoveTransitionWitness,
      capturedTarget: RelationColoredPieceWitness,
      immediateDefense: RelationLegalMoveResourceWitness,
      disabledDefender: RelationColoredPieceWitness
  ) extends CausalSemanticDescriptor:
    val contractKind = BoundedCausalContractKind.UniqueCheckReplyDefenderDisplacementBeforeCapture
    val semanticParts: List[String] = List(
      trigger.stableKey,
      forcedReply.stableKey,
      realizer.stableKey,
      BoundedCausalIdentity.coloredPieceKey(capturedTarget),
      immediateDefense.stableKey,
      BoundedCausalIdentity.coloredPieceKey(disabledDefender)
    )
  def proposition(
      rootFen: String,
      trigger: RelationMoveTransitionWitness,
      forcedReply: RelationLegalMoveResourceWitness,
      realizer: RelationMoveTransitionWitness,
      capturedTarget: RelationColoredPieceWitness,
      immediateDefense: RelationLegalMoveResourceWitness,
      disabledDefender: RelationColoredPieceWitness
  ): CausalPropositionIdentity =
    CausalPropositionIdentity.from(
      PropositionDescriptor(
        rootFen,
        trigger,
        forcedReply,
        realizer,
        capturedTarget,
        immediateDefense,
        disabledDefender
      )
    )

private[chessjudgment] enum UniqueCheckReplyDefenderDisplacementBeforeCapturePremiseRole extends CausalPremiseRole:
  case DisplacementCheckResponse
  case DelayedCaptureRecapture
  case ImmediateCaptureRecapture

  def stableKey: String =
    this match
      case DisplacementCheckResponse => "displacement-check-response"
      case DelayedCaptureRecapture   => "delayed-capture-recapture"
      case ImmediateCaptureRecapture => "immediate-capture-recapture"

private[chessjudgment] enum UniqueCheckReplyDefenderDisplacementBeforeCaptureAbsenceRole extends CausalAbsenceRole:
  case DelayedCaptureRecaptureAbsent

  def stableKey: String = "delayed-capture-recapture-absent"

private[chessjudgment] enum UniqueCheckReplyDefenderDisplacementBeforeCaptureStateRole extends CausalStateRole:
  case DelayedCaptureActorPresent
  case DelayedTargetPresent

  def stableKey: String =
    this match
      case DelayedCaptureActorPresent => "delayed-capture-actor-present"
      case DelayedTargetPresent       => "delayed-target-present"

/** Complete named lower-premise manifest for this contract. The common core
  * accepts only typed manifests; this family is the sole authority for which
  * lower occurrences and absence boundary prove its proposition.
  */
private[chessjudgment] sealed trait UniqueCheckReplyDefenderDisplacementBeforeCaptureManifest
    extends BoundedCausalContractManifest:
  def delayedNoRecapture: CausalClosedAbsenceBinding
  def delayedPersistence: List[CausalClosedStateBinding]
  final def contractKind = BoundedCausalContractKind.UniqueCheckReplyDefenderDisplacementBeforeCapture
  final def absenceBindings = List(delayedNoRecapture)
  final override def stateBindings = delayedPersistence
  final def stableKey: String =
    List(
      contractKind.toString.toLowerCase,
      premiseUses.map(_.stableKey).mkString("[", ",", "]"),
      delayedNoRecapture.stableKey,
      delayedPersistence.map(_.stableKey).mkString("[", ",", "]")
    ).mkString("|")

private[chessjudgment] object UniqueCheckReplyDefenderDisplacementBeforeCaptureManifest:
  private final case class Exact(
      checkResponse: CausalVerticalRelationPremiseUse,
      delayedRecapture: CausalVerticalRelationPremiseUse,
      immediateRecapture: CausalVerticalRelationPremiseUse,
      delayedNoRecapture: CausalClosedAbsenceBinding,
      delayedPersistence: List[CausalClosedStateBinding]
  ) extends UniqueCheckReplyDefenderDisplacementBeforeCaptureManifest:
    val premiseUses = List(checkResponse, delayedRecapture, immediateRecapture)

  def exact(
      checkResponse: CausalVerticalRelationPremiseUse,
      delayedRecapture: CausalVerticalRelationPremiseUse,
      immediateRecapture: CausalVerticalRelationPremiseUse,
      delayedNoRecapture: CausalClosedAbsenceBinding,
      delayedCaptureActor: RelationColoredPieceWitness,
      delayedCaptureTarget: RelationColoredPieceWitness,
      delayedPersistence: List[CausalClosedStateBinding]
  ): UniqueCheckReplyDefenderDisplacementBeforeCaptureManifest =
    validateShared(checkResponse, delayedRecapture, immediateRecapture, delayedNoRecapture)
    validatePersistence(delayedRecapture, delayedCaptureActor, delayedCaptureTarget, delayedPersistence)
    Exact(checkResponse, delayedRecapture, immediateRecapture, delayedNoRecapture, delayedPersistence)

  private def validateShared(
      checkResponse: CausalVerticalRelationPremiseUse,
      delayedRecapture: CausalVerticalRelationPremiseUse,
      immediateRecapture: CausalVerticalRelationPremiseUse,
      delayedNoRecapture: CausalClosedAbsenceBinding
  ): Unit =
    require(
      checkResponse.role == UniqueCheckReplyDefenderDisplacementBeforeCapturePremiseRole.DisplacementCheckResponse &&
        checkResponse.contract == VerticalRelationContractKind.CreatedCheckResponseInventory &&
        checkResponse.result.kind == RelationFactKind.CreatedCheckResponseInventory &&
        checkResponse.branchRole == UniqueCheckReplyBranchRole.DisplacementThenCapture &&
        checkResponse.stepIndex == 0,
      "the first premise must be the exact displacement-branch root check-response inventory"
    )
    require(
      delayedRecapture.role == UniqueCheckReplyDefenderDisplacementBeforeCapturePremiseRole.DelayedCaptureRecapture &&
        delayedRecapture.contract == VerticalRelationContractKind.CaptureRecaptureInventory &&
        delayedRecapture.result.kind == RelationFactKind.CaptureRecaptureInventory &&
        delayedRecapture.branchRole == UniqueCheckReplyBranchRole.DisplacementThenCapture &&
        delayedRecapture.stepIndex == 2,
      "the second premise must be the delayed-capture recapture inventory"
    )
    require(
      immediateRecapture.role == UniqueCheckReplyDefenderDisplacementBeforeCapturePremiseRole.ImmediateCaptureRecapture &&
        immediateRecapture.contract == VerticalRelationContractKind.CaptureRecaptureInventory &&
        immediateRecapture.result.kind == RelationFactKind.CaptureRecaptureInventory &&
        immediateRecapture.branchRole == UniqueCheckReplyBranchRole.ImmediateCaptureWithDefender &&
        immediateRecapture.stepIndex == 0,
      "the third premise must be the immediate-capture recapture inventory"
    )
    require(
      delayedNoRecapture.role == UniqueCheckReplyDefenderDisplacementBeforeCaptureAbsenceRole.DelayedCaptureRecaptureAbsent &&
        delayedNoRecapture.branchRole == UniqueCheckReplyBranchRole.DisplacementThenCapture &&
        checkResponse.branchId == delayedRecapture.branchId &&
        delayedNoRecapture.branchId == delayedRecapture.branchId &&
        delayedNoRecapture.afterStepIndex == delayedRecapture.stepIndex,
      "the closed absence must belong to the delayed-capture occurrence"
    )

  private def validatePersistence(
      delayedRecapture: CausalVerticalRelationPremiseUse,
      delayedCaptureActor: RelationColoredPieceWitness,
      delayedCaptureTarget: RelationColoredPieceWitness,
      persistence: List[CausalClosedStateBinding]
  ): Unit =
    val expected = (0 until delayedRecapture.stepIndex).toList.flatMap(index =>
      List(
        (
          index,
          UniqueCheckReplyDefenderDisplacementBeforeCaptureStateRole.DelayedCaptureActorPresent,
          delayedCaptureActor
        ),
        (
          index,
          UniqueCheckReplyDefenderDisplacementBeforeCaptureStateRole.DelayedTargetPresent,
          delayedCaptureTarget
        )
      )
    )
    require(
      persistence.zip(expected).forall { case (binding, (index, role, piece)) =>
        binding.role == role && binding.branchRole == UniqueCheckReplyBranchRole.DisplacementThenCapture &&
          binding.branchId == delayedRecapture.branchId && binding.afterStepIndex == index &&
          binding.query == PositionRelationExtractor.ClosedPositionStateQuery.OccupiedBy(piece)
      } && persistence.size == expected.size,
      "the delayed capture actor and target need exact occupied states after every pre-capture step"
    )

/** Transposition-shared meaning. Lower result ids and proof paths do not live
  * here: the same proposition may be certified by several exact occurrences
  * and independent lower-proof routes.
  */
private[chessjudgment] final case class UniqueCheckReplyDefenderDisplacementBeforeCaptureSemanticProof private[chessjudgment] (
    identity: CausalPropositionIdentity,
    trigger: RelationMoveTransitionWitness,
    forcedReply: RelationLegalMoveResourceWitness,
    realizer: RelationMoveTransitionWitness,
    capturedTarget: RelationColoredPieceWitness,
    immediateDefense: RelationLegalMoveResourceWitness,
    disabledDefender: RelationColoredPieceWitness
):
  require(
    identity.contractKind == BoundedCausalContractKind.UniqueCheckReplyDefenderDisplacementBeforeCapture,
    "a unique-check-reply defender-displacement proof needs its exact causal proposition kind"
  )
  require(
    disabledDefender.side == immediateDefense.movement.side &&
      disabledDefender.role == immediateDefense.movement.beforeRole &&
      disabledDefender.square == immediateDefense.movement.from,
    "the unique-check-reply defender-displacement proof must retain the exact immediate-capture recapturer identity"
  )
  require(
    disabledDefender.side == forcedReply.movement.side &&
      disabledDefender.role == forcedReply.movement.beforeRole &&
      disabledDefender.square == forcedReply.movement.from,
    "the semantic proof must retain the exact displaced defender"
  )

  def semanticId: String = identity.semanticId
  def rootBoardState: String = identity.rootPositionIdentity.value

/** One exact displacement sequence and its immediate-capture sibling. Either
  * semantic branch may be the requested observed occurrence.
  */
private[chessjudgment] final case class UniqueCheckReplyDefenderDisplacementBeforeCaptureOccurrence private[chessjudgment] (
    proofSet: BoundedCausalProofSet,
    subjectOccurrence: ExplanationSubjectOccurrence,
    delayedCaptureIndex: Int
):
  require(
    proofSet.proposition.contractKind ==
      BoundedCausalContractKind.UniqueCheckReplyDefenderDisplacementBeforeCapture,
    "a unique-check-reply defender-displacement occurrence needs its exact causal contract"
  )
  require(
    proofSet.occurrence.branches.size == 2 &&
      proofSet.occurrence.branch(UniqueCheckReplyBranchRole.DisplacementThenCapture).nonEmpty &&
      proofSet.occurrence.branch(UniqueCheckReplyBranchRole.ImmediateCaptureWithDefender).nonEmpty,
    "this contract needs its exact displacement and immediate-capture branches"
  )
  require(
    displacementSteps.size == 3,
    "a unique-check-reply defender-displacement occurrence needs trigger, reply, and realizer"
  )
  require(immediateCaptureSteps.size == 2, "the immediate-capture branch needs capture and recapture")
  require(delayedCaptureIndex == 2, "the first proof family admits only its immediate delayed capture")
  require(ownsSubject, "the proof occurrence must retain its exact requested root occurrence")

  private def exactBranch(role: UniqueCheckReplyBranchRole): CausalBranchOccurrence =
    proofSet.occurrence
      .branch(role)
      .getOrElse(throw IllegalStateException(s"a causal occurrence lost its $role branch"))

  def semanticId: String = proofSet.proposition.semanticId
  def occurrenceId: String = proofSet.occurrence.occurrenceId
  def displacementBranch: CausalBranchOccurrence =
    exactBranch(UniqueCheckReplyBranchRole.DisplacementThenCapture)
  def immediateCaptureBranch: CausalBranchOccurrence =
    exactBranch(UniqueCheckReplyBranchRole.ImmediateCaptureWithDefender)
  def displacementLine: LineNodeRef = displacementBranch.line
  def immediateCaptureLine: LineNodeRef = immediateCaptureBranch.line
  def displacementSteps: List[LineReplayStep] = displacementBranch.replaySteps
  def immediateCaptureSteps: List[LineReplayStep] = immediateCaptureBranch.replaySteps
  def proofPaths: List[CausalProofPathOccurrence] = proofSet.paths
  def triggerStep: LineReplayStep = displacementSteps.head
  def forcedReplyStep: LineReplayStep = displacementSteps(1)
  def realizerStep: LineReplayStep = displacementSteps(delayedCaptureIndex)
  def immediateRealizerStep: LineReplayStep = immediateCaptureSteps.head
  def immediateReplyStep: LineReplayStep = immediateCaptureSteps(1)

  private def ownsSubject: Boolean =
    List(displacementBranch, immediateCaptureBranch).exists { branch =>
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

/** Exact dependency identity for the first contract. Lower replays retain
  * their own calculation ownership; this manifest names only the line
  * records, occurrence, and proof paths actually consumed by the result.
  */
private[chessjudgment] final case class UniqueCheckReplyDefenderDisplacementBeforeCaptureDependencyManifest private[chessjudgment] (
    subject: CertifiedRootOccurrence,
    displacement: CertifiedRootOccurrence,
    immediateCapture: CertifiedRootOccurrence,
    disabledDefender: RelationColoredPieceWitness,
    proofSet: BoundedCausalProofSet
) extends BoundedCausalDependencyManifest:
  val contractKind = BoundedCausalContractKind.UniqueCheckReplyDefenderDisplacementBeforeCapture
  require(
    proofSet.proposition.contractKind == contractKind &&
      proofSet.paths.forall(_.manifest.contractKind == contractKind),
    "a dependency manifest may contain only its exact typed causal proof paths"
  )

  val stableKey: String =
    List(
      "subject-root",
      BoundedCausalIdentity.evidenceRecordKey(subject.lineOwner),
      BoundedCausalIdentity.evidenceRecordKey(subject.transitionOwner),
      "displacement-root",
      BoundedCausalIdentity.evidenceRecordKey(displacement.lineOwner),
      BoundedCausalIdentity.evidenceRecordKey(displacement.transitionOwner),
      "immediate-capture-root",
      BoundedCausalIdentity.evidenceRecordKey(immediateCapture.lineOwner),
      BoundedCausalIdentity.evidenceRecordKey(immediateCapture.transitionOwner),
      BoundedCausalIdentity.coloredPieceKey(disabledDefender),
      proofSet.proposition.semanticId,
      proofSet.occurrence.occurrenceId,
      proofSet.paths.map(_.pathOccurrenceId).mkString("[", ",", "]")
    ).mkString("|")

  def consumes(
      exactSubject: CertifiedRootOccurrence,
      exactDisplacement: CertifiedRootOccurrence,
      exactImmediateCapture: CertifiedRootOccurrence
  ): Boolean =
    subject == exactSubject && displacement == exactDisplacement && immediateCapture == exactImmediateCapture

/** Opaque occurrence authority. It retains the admitted line replays and the
  * exact lower absence proof for every path use. No legal move, attack map,
  * ray, or pawn fact is rebuilt here.
  */
private[chessjudgment] final class UniqueCheckReplyDefenderDisplacementBeforeCaptureOccurrenceProof private[chessjudgment] (
    val semantic: UniqueCheckReplyDefenderDisplacementBeforeCaptureSemanticProof,
    val occurrence: UniqueCheckReplyDefenderDisplacementBeforeCaptureOccurrence,
    val dependency: BoundedCausalDependencyFingerprint,
    private val dependencyManifest: UniqueCheckReplyDefenderDisplacementBeforeCaptureDependencyManifest,
    val subject: CertifiedRootOccurrence,
    private val displacement: CertifiedRootOccurrence,
    private val immediateCapture: CertifiedRootOccurrence,
    private val absenceAuthorities: List[
      (CausalClosedAbsenceUse, ClosedRelationAbsenceAuthority)
    ],
    private val stateAuthorities: List[
      (CausalClosedStateUse, ClosedPositionStateAuthority)
    ]
):
  private val expectedAbsenceUses =
    occurrence.proofPaths.flatMap(_.closedAbsenceUses).sortBy(_.useId)
  private val expectedStateUses =
    occurrence.proofPaths.flatMap(_.closedStateUses).sortBy(_.useId)
  require(
    semantic.identity == occurrence.proofSet.proposition &&
      dependencyManifest.disabledDefender == semantic.disabledDefender &&
      dependencyManifest.proofSet == occurrence.proofSet &&
      BoundedCausalDependencyFingerprint.from(dependencyManifest) == dependency,
    "an L2 occurrence authority must retain one proposition and complete dependency manifest"
  )
  require(
    absenceAuthorities.map(_._1).sortBy(_.useId) == expectedAbsenceUses &&
      absenceAuthorities.map(_._1.useId).distinct.size == absenceAuthorities.size,
    "every closed absence use must retain one exact lower-inventory authority"
  )
  require(
    stateAuthorities.map(_._1).sortBy(_.useId) == expectedStateUses &&
      stateAuthorities.map(_._1.useId).distinct.size == stateAuthorities.size,
    "every closed state use must retain one exact lower-inventory authority"
  )

  def parentSources: List[EvidenceRef] =
    val sources = List(
      displacement.lineOwner.ref,
      displacement.transitionOwner.ref,
      immediateCapture.lineOwner.ref,
      immediateCapture.transitionOwner.ref
    ).sortBy(_.id)
    require(sources.map(_.id).distinct.size == sources.size, "the two roots need distinct line and transition owners")
    sources

  private[chessjudgment] def lowerIssuerRecords: List[EvidenceRecord] =
    List(
      displacement.lineOwner,
      displacement.transitionOwner,
      immediateCapture.lineOwner,
      immediateCapture.transitionOwner
    )

  def consumesDependencies(
      exactSubject: CertifiedRootOccurrence,
      exactDisplacement: CertifiedRootOccurrence,
      exactImmediateCapture: CertifiedRootOccurrence
  ): Boolean =
    dependencyManifest.consumes(exactSubject, exactDisplacement, exactImmediateCapture)

  def proves(record: EvidenceRecord, payload: UniqueCheckReplyDefenderDisplacementBeforeCaptureEvidence): Boolean =
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
    subject.remainsCertified && displacement.remainsCertified && immediateCapture.remainsCertified &&
      (subject == displacement || subject == immediateCapture) &&
      occurrence.subjectOccurrence == subject.publicOccurrence &&
      occurrence.displacementLine == displacement.line &&
      occurrence.immediateCaptureLine == immediateCapture.line &&
      displacement.replay.replaySteps.take(occurrence.displacementSteps.size) == occurrence.displacementSteps &&
      immediateCapture.replay.replaySteps.take(occurrence.immediateCaptureSteps.size) == occurrence.immediateCaptureSteps &&
      closedAbsencesRemainCertified && closedStatesRemainCertified

  private def ownerFor(role: CausalBranchRole): Option[CertifiedRootOccurrence] =
    role match
      case UniqueCheckReplyBranchRole.DisplacementThenCapture      => Some(displacement)
      case UniqueCheckReplyBranchRole.ImmediateCaptureWithDefender => Some(immediateCapture)
      case _                                                       => None

  private def closedAbsencesRemainCertified: Boolean =
    absenceAuthorities.forall { case (use, authority) =>
      val binding = use.binding
      ownerFor(binding.branchRole).exists { root =>
        binding.authority == authority && authority.issuerRecord == root.lineOwner &&
          authority.remainsCertified &&
          occurrence.proofSet.occurrence.branch(binding.branchRole).exists(branch =>
            branch.branchId == binding.branchId &&
              branch.stepAt(binding.afterStepIndex).exists(stepOccurrence =>
                stepOccurrence.line == authority.issuerLine && stepOccurrence.step == authority.step
              )
          )
      }
    }

  private def closedStatesRemainCertified: Boolean =
    val realizer = RelationColoredPieceWitness(
      semantic.realizer.from,
      semantic.realizer.beforeRole,
      semantic.realizer.side
    )
    stateAuthorities.size == occurrence.delayedCaptureIndex * 2 &&
      stateAuthorities.forall { case (use, authority) =>
        val binding = use.binding
        val exactPiece = binding.role match
          case UniqueCheckReplyDefenderDisplacementBeforeCaptureStateRole.DelayedCaptureActorPresent =>
            Some(realizer)
          case UniqueCheckReplyDefenderDisplacementBeforeCaptureStateRole.DelayedTargetPresent =>
            Some(semantic.capturedTarget)
          case _ => None
        exactPiece.exists(piece =>
          binding.branchRole == UniqueCheckReplyBranchRole.DisplacementThenCapture &&
            binding.branchId == occurrence.displacementBranch.branchId &&
            binding.afterStepIndex >= 0 && binding.afterStepIndex < occurrence.delayedCaptureIndex &&
            binding.query == PositionRelationExtractor.ClosedPositionStateQuery.OccupiedBy(piece) &&
            binding.authority == authority && authority.issuerRecord == displacement.lineOwner &&
            ClosedPositionStateAuthority
              .certified(displacement.lineOwner, authority.occurrence, authority.proof)
              .contains(authority) &&
            occurrence.displacementBranch.stepAt(binding.afterStepIndex).exists(stepOccurrence =>
              stepOccurrence.line == authority.issuerLine && stepOccurrence.step == authority.step
            )
        )
      }

private[chessjudgment] final case class CertifiedUniqueCheckReplyDefenderDisplacementBeforeCapture private[chessjudgment] (
    semantic: UniqueCheckReplyDefenderDisplacementBeforeCaptureSemanticProof,
    occurrence: UniqueCheckReplyDefenderDisplacementBeforeCaptureOccurrence,
    dependency: BoundedCausalDependencyFingerprint,
    proof: UniqueCheckReplyDefenderDisplacementBeforeCaptureOccurrenceProof
)

/** Exact changed L1 occurrences selected once by the shared demand dispatcher.
  * The family proof may close higher causal obligations around these owners,
  * but it must not rediscover their transitions from a boolean demand.
  */
private[chessjudgment] final case class UniqueCheckReplyDefenderDisplacementBeforeCaptureDemand private[chessjudgment] (
    checkOccurrence: ReplayVerticalRelationOccurrence,
    forcedReply: RelationCheckResponseWitness,
    delayedCaptureOccurrence: ReplayVerticalRelationOccurrence,
    immediateCaptureOccurrence: ReplayVerticalRelationOccurrence,
    immediateRecapture: RelationLegalMoveResourceWitness
):
  require(
    checkOccurrence.contract == VerticalRelationContractKind.CreatedCheckResponseInventory &&
      delayedCaptureOccurrence.contract == VerticalRelationContractKind.CaptureRecaptureInventory &&
      immediateCaptureOccurrence.contract == VerticalRelationContractKind.CaptureRecaptureInventory,
    "a unique-check-reply defender-displacement demand must retain its exact check and capture L1 occurrences"
  )

  private[chessjudgment] def stableKey: String =
    List(
      checkOccurrence.occurrenceId,
      forcedReply.stableKey,
      delayedCaptureOccurrence.occurrenceId,
      immediateCaptureOccurrence.occurrenceId,
      immediateRecapture.stableKey
    ).mkString("|")

private[chessjudgment] object UniqueCheckReplyDefenderDisplacementBeforeCaptureProof:

  private final case class PersistenceState(
      role: UniqueCheckReplyDefenderDisplacementBeforeCaptureStateRole,
      stepIndex: Int,
      authority: ClosedPositionStateAuthority
  )

  private final case class ImmediateInputs(
      rootBoard: String,
      check: RelationWitnessDetail.CreatedCheckResponseInventory,
      forcedReply: RelationLegalMoveResourceWitness,
      checkOccurrence: ReplayVerticalRelationOccurrence,
      delayedCapture: RelationWitnessDetail.CaptureRecaptureInventory,
      delayedCaptureOccurrence: ReplayVerticalRelationOccurrence,
      immediateCaptureOccurrence: ReplayVerticalRelationOccurrence,
      immediateDefense: RelationLegalMoveResourceWitness,
      displacementCausalBranch: CausalBranchOccurrence,
      immediateCausalBranch: CausalBranchOccurrence,
      absenceAuthority: ClosedRelationAbsenceAuthority,
      persistenceStates: List[PersistenceState]
  )

  def certifyDemanded(
      subject: CertifiedRootOccurrence,
      displacement: CertifiedRootOccurrence,
      immediateCapture: CertifiedRootOccurrence,
      demand: UniqueCheckReplyDefenderDisplacementBeforeCaptureDemand
  ): List[CertifiedUniqueCheckReplyDefenderDisplacementBeforeCapture] =
    val immediateMove = immediateCapture.replay.replaySteps.headOption.map(_.moveUci)
    displacement.replay.replaySteps
      .lift(2)
      .filter(step => immediateMove.exists(EvidenceRef.sameMove(_, step.moveUci)))
      .map(_ =>
        derive(
          subject,
          displacement,
          immediateCapture,
          delayedCaptureIndex = 2,
          demand = demand
        )
      )
      .getOrElse(Nil)

  def derive(
      subject: CertifiedRootOccurrence,
      displacement: CertifiedRootOccurrence,
      immediateCapture: CertifiedRootOccurrence,
      delayedCaptureIndex: Int,
      demand: UniqueCheckReplyDefenderDisplacementBeforeCaptureDemand
  ): List[CertifiedUniqueCheckReplyDefenderDisplacementBeforeCapture] =
    val displacementReplay = displacement.replay
    val immediateReplay = immediateCapture.replay
    val inputs = for
      _ <- Option.when(
        subject.remainsCertified && displacement.remainsCertified && immediateCapture.remainsCertified &&
          subject.isObserved && (subject == displacement || subject == immediateCapture)
      )((): Unit)
      if displacement.transition.from == immediateCapture.transition.from
      triggerStep <- displacementReplay.replaySteps.headOption
      forcedReplyStep <- displacementReplay.replaySteps.lift(1)
      realizerStep <- displacementReplay.replaySteps.lift(delayedCaptureIndex)
      immediateRealizerStep <- immediateReplay.replaySteps.headOption
      immediateReplyStep <- immediateReplay.replaySteps.lift(1)
      if delayedCaptureIndex == 2
      if EvidenceRef.sameMove(realizerStep.moveUci, immediateRealizerStep.moveUci)
      if !EvidenceRef.sameMove(triggerStep.moveUci, immediateRealizerStep.moveUci)
      rootBoard <- PrincipalVariationEvidence.semanticBoardStateFen(triggerStep.fenBefore)
      immediateRoot <- PrincipalVariationEvidence.semanticBoardStateFen(immediateRealizerStep.fenBefore)
      if rootBoard == immediateRoot
      checkOccurrence = demand.checkOccurrence
      if checkOccurrence.step == triggerStep
      check <- checkOccurrence.relation.detail match
        case value: RelationWitnessDetail.CreatedCheckResponseInventory => Some(value)
        case _                                                          => None
      response <- check.responses match
        case exact :: Nil if exact == demand.forcedReply => Some(exact)
        case _                                           => None
      if EvidenceRef.sameMove(response.resource.moveUci, forcedReplyStep.moveUci)
      if check.terminal == RelationCheckTerminalState.Ongoing
      delayedCaptureOccurrence = demand.delayedCaptureOccurrence
      if delayedCaptureOccurrence.step == realizerStep
      delayedCapture <- delayedCaptureOccurrence.relation.detail match
        case value: RelationWitnessDetail.CaptureRecaptureInventory => Some(value)
        case _                                                       => None
      immediateCaptureOccurrence = demand.immediateCaptureOccurrence
      if immediateCaptureOccurrence.step == immediateRealizerStep
      immediateCaptureInventory <- immediateCaptureOccurrence.relation.detail match
        case value: RelationWitnessDetail.CaptureRecaptureInventory => Some(value)
        case _                                                       => None
      defense = demand.immediateRecapture
      if EvidenceRef.sameMove(defense.moveUci, immediateReplyStep.moveUci)
      if sameRealizerAndTarget(delayedCapture, immediateCaptureInventory)
      persistenceStates <- delayedPersistenceStates(
        displacement.lineOwner,
        displacementReplay,
        delayedCaptureIndex,
        coloredPieceAtStart(delayedCapture.mover),
        delayedCapture.captured
      )
      if immediateCaptureInventory.legalRecaptures == List(defense)
      if delayedCapture.legalRecaptures.isEmpty
      absenceQuery = PositionRelationExtractor.ClosedRelationAbsenceQuery.LegalCaptureOf(
        delayedCapture.captured.side,
        delayedCapture.mover.to
      )
      absenceOccurrence <- displacementReplay.positionAfter(realizerStep)
      absenceAuthority <- ClosedRelationAbsenceAuthority.forQuery(
        displacement.lineOwner,
        absenceOccurrence,
        absenceQuery
      )
      if checkOccurrence.certifiedSourcePremiseIds.nonEmpty
      if delayedCaptureOccurrence.certifiedSourcePremiseIds.nonEmpty
      if immediateCaptureOccurrence.certifiedSourcePremiseIds.nonEmpty
      displacementCausalBranch = CausalBranchOccurrence.fromRootOccurrence(
        UniqueCheckReplyBranchRole.DisplacementThenCapture,
        displacement,
        delayedCaptureIndex + 1
      )
      immediateCausalBranch = CausalBranchOccurrence.fromRootOccurrence(
        UniqueCheckReplyBranchRole.ImmediateCaptureWithDefender,
        immediateCapture,
        2
      )
    yield ImmediateInputs(
      rootBoard,
      check,
      response.resource,
      checkOccurrence,
      delayedCapture,
      delayedCaptureOccurrence,
      immediateCaptureOccurrence,
      defense,
      displacementCausalBranch,
      immediateCausalBranch,
      absenceAuthority,
      persistenceStates
    )

    inputs.toList.flatMap { exact =>
      disabledDefender(exact).map { defender =>
        certify(
          exact,
          defender,
          subject,
          displacement,
          immediateCapture,
          delayedCaptureIndex
        )
      }
    }

  private def certify(
      inputs: ImmediateInputs,
      disabledDefender: RelationColoredPieceWitness,
      subject: CertifiedRootOccurrence,
      displacement: CertifiedRootOccurrence,
      immediateCapture: CertifiedRootOccurrence,
      delayedCaptureIndex: Int
  ): CertifiedUniqueCheckReplyDefenderDisplacementBeforeCapture =
    val proposition = UniqueCheckReplyDefenderDisplacementBeforeCaptureCausalAuthority.proposition(
      inputs.rootBoard,
      inputs.check.mover,
      inputs.forcedReply,
      inputs.delayedCapture.mover,
      inputs.delayedCapture.captured,
      inputs.immediateDefense,
      disabledDefender
    )
    val displacementBranch = inputs.displacementCausalBranch
    val immediateBranch = inputs.immediateCausalBranch
    val causalOccurrence = CausalOccurrenceIdentity.from(
      proposition,
      List(displacementBranch, immediateBranch)
    )
    val checkUse = CausalVerticalRelationPremiseUse.from(
      UniqueCheckReplyDefenderDisplacementBeforeCapturePremiseRole.DisplacementCheckResponse,
      RecordBoundVerticalRelationOccurrence.certified(displacement.lineOwner, inputs.checkOccurrence).getOrElse(
        throw IllegalArgumentException("forced reply lost its graph-owned check-response occurrence")
      ),
      displacementBranch,
      0
    )
    val delayedRecaptureUse = CausalVerticalRelationPremiseUse.from(
      UniqueCheckReplyDefenderDisplacementBeforeCapturePremiseRole.DelayedCaptureRecapture,
      RecordBoundVerticalRelationOccurrence.certified(displacement.lineOwner, inputs.delayedCaptureOccurrence).getOrElse(
        throw IllegalArgumentException("forced reply lost its graph-owned delayed capture occurrence")
      ),
      displacementBranch,
      delayedCaptureIndex
    )
    val immediateRecaptureUse = CausalVerticalRelationPremiseUse.from(
      UniqueCheckReplyDefenderDisplacementBeforeCapturePremiseRole.ImmediateCaptureRecapture,
      RecordBoundVerticalRelationOccurrence.certified(immediateCapture.lineOwner, inputs.immediateCaptureOccurrence).getOrElse(
        throw IllegalArgumentException("forced reply lost its graph-owned immediate capture occurrence")
      ),
      immediateBranch,
      0
    )
    val absenceBinding = CausalClosedAbsenceBinding.afterStep(
      UniqueCheckReplyDefenderDisplacementBeforeCaptureAbsenceRole.DelayedCaptureRecaptureAbsent,
      inputs.absenceAuthority,
      displacementBranch,
      delayedCaptureIndex
    )
    val persistenceBindings = inputs.persistenceStates.map(state =>
      CausalClosedStateBinding.afterStep(
        state.role,
        state.authority,
        displacementBranch,
        state.stepIndex
      )
    )
    val manifest = UniqueCheckReplyDefenderDisplacementBeforeCaptureManifest.exact(
      checkUse,
      delayedRecaptureUse,
      immediateRecaptureUse,
      absenceBinding,
      coloredPieceAtStart(inputs.delayedCapture.mover),
      inputs.delayedCapture.captured,
      persistenceBindings
    )
    val path = CausalProofPathOccurrence.from(proposition, manifest)
    val absenceUse = path.closedAbsenceUses match
      case exact :: Nil => exact
      case other =>
        throw IllegalStateException(
          s"a unique-check-reply defender-displacement path retained ${other.size} absence uses"
        )
    val proofSet = BoundedCausalProofSet.from(
      proposition,
      causalOccurrence,
      List(path)
    )
    val semantic = UniqueCheckReplyDefenderDisplacementBeforeCaptureSemanticProof(
      identity = proposition,
      trigger = inputs.check.mover,
      forcedReply = inputs.forcedReply,
      realizer = inputs.delayedCapture.mover,
      capturedTarget = inputs.delayedCapture.captured,
      immediateDefense = inputs.immediateDefense,
      disabledDefender = disabledDefender
    )
    val occurrence = UniqueCheckReplyDefenderDisplacementBeforeCaptureOccurrence(
      proofSet,
      subject.publicOccurrence,
      delayedCaptureIndex
    )
    val dependencyManifest = UniqueCheckReplyDefenderDisplacementBeforeCaptureDependencyManifest(
      subject,
      displacement,
      immediateCapture,
      semantic.disabledDefender,
      proofSet
    )
    val dependency = BoundedCausalDependencyFingerprint.from(dependencyManifest)
    val proof = new UniqueCheckReplyDefenderDisplacementBeforeCaptureOccurrenceProof(
      semantic,
      occurrence,
      dependency,
      dependencyManifest,
      subject,
      displacement,
      immediateCapture,
      List(absenceUse -> inputs.absenceAuthority),
      path.closedStateUses.map(use => use -> use.binding.authority)
    )
    CertifiedUniqueCheckReplyDefenderDisplacementBeforeCapture(semantic, occurrence, dependency, proof)

  private def disabledDefender(inputs: ImmediateInputs): Option[RelationColoredPieceWitness] =
    Option.when(
      sameInitialPiece(inputs.forcedReply.movement, inputs.immediateDefense.movement)
    )(coloredPieceAtStart(inputs.forcedReply.movement))

  private def sameInitialPiece(
      left: RelationMoveTransitionWitness,
      right: RelationMoveTransitionWitness
  ): Boolean =
    left.side == right.side && left.from == right.from && left.beforeRole == right.beforeRole

  private def coloredPieceAtStart(
      movement: RelationMoveTransitionWitness
  ): RelationColoredPieceWitness =
    RelationColoredPieceWitness(movement.from, movement.beforeRole, movement.side)

  private def sameRealizerAndTarget(
      delayed: RelationWitnessDetail.CaptureRecaptureInventory,
      immediate: RelationWitnessDetail.CaptureRecaptureInventory
  ): Boolean =
    delayed.mover == immediate.mover && delayed.captured == immediate.captured

  private def delayedPersistenceStates(
      displacementLineRecord: EvidenceRecord,
      replay: CanonicalLineReplay,
      delayedCaptureIndex: Int,
      realizer: RelationColoredPieceWitness,
      target: RelationColoredPieceWitness
  ): Option[List[PersistenceState]] =
    val expected = replay.replaySteps.take(delayedCaptureIndex).zipWithIndex.flatMap { case (step, index) =>
      List(
        UniqueCheckReplyDefenderDisplacementBeforeCaptureStateRole.DelayedCaptureActorPresent -> realizer,
        UniqueCheckReplyDefenderDisplacementBeforeCaptureStateRole.DelayedTargetPresent -> target
      ).flatMap { case (role, piece) =>
        ClosedPositionStateAuthority
          .atStep(displacementLineRecord, step)((occurrence, scope) =>
            occurrence.existingOccupantState(piece, scope)
          )
          .map(PersistenceState(role, index, _))
      }
    }
    Option.when(expected.size == delayedCaptureIndex * 2)(expected)
