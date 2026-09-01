package lila.chessjudgment.model.judgment

import lila.chessjudgment.analysis.position.PositionRelationExtractor
import lila.chessjudgment.model.line.PrincipalVariationEvidence

private[chessjudgment] enum SoleRecapturerRemovalBranchRole extends CausalBranchRole:
  case RemovalThenTargetCapture
  case ImmediateTargetCapture

  def stableKey: String =
    this match
      case RemovalThenTargetCapture => "removal-then-target-capture"
      case ImmediateTargetCapture   => "immediate-target-capture"

private[chessjudgment] enum SoleRecapturerRemovalBeforeTargetCapturePremiseRole extends CausalPremiseRole:
  case DefenderRemoval
  case PostRemovalTargetCaptureInventory
  case ImmediateTargetCaptureInventory

  def stableKey: String =
    this match
      case DefenderRemoval                    => "defender-removal"
      case PostRemovalTargetCaptureInventory => "post-removal-target-capture-inventory"
      case ImmediateTargetCaptureInventory   => "immediate-target-capture-inventory"

private[chessjudgment] enum SoleRecapturerRemovalBeforeTargetCaptureAbsenceRole extends CausalAbsenceRole:
  case PostRemovalReplacementRecaptureAbsent

  def stableKey: String = "post-removal-replacement-recapture-absent"

private[chessjudgment] enum SoleRecapturerRemovalBeforeTargetCaptureStateRole extends CausalStateRole:
  case PostRemovalExploitActorPresent
  case PostRemovalTargetPresent

  def stableKey: String =
    this match
      case PostRemovalExploitActorPresent => "post-removal-exploit-actor-present"
      case PostRemovalTargetPresent       => "post-removal-target-present"

/** Sole family authority for the semantic identity. Its private descriptor is
  * constructed only after exact L1 membership and absence have been checked.
  */
private[chessjudgment] object SoleRecapturerRemovalBeforeTargetCaptureCausalAuthority:
  private final case class PropositionDescriptor(
      rootFen: String,
      remover: RelationMoveTransitionWitness,
      removedDefender: RelationColoredPieceWitness,
      removalRecapture: RelationLegalMoveResourceWitness,
      exploit: RelationMoveTransitionWitness,
      capturedTarget: RelationColoredPieceWitness,
      immediateRecapture: RelationLegalMoveResourceWitness
  ) extends CausalSemanticDescriptor:
    require(
      remover.side != removedDefender.side && remover.to == removedDefender.square,
      "defender removal must retain the exact capturing movement and defender square"
    )
    require(
      removalRecapture.movement.side == removedDefender.side &&
        removalRecapture.movement.to == remover.to &&
        removalRecapture.capture.exists(capture =>
          capture.capturedSide == remover.side &&
            capture.capturedRole == remover.afterRole &&
            capture.capturedSquare == remover.to
        ),
      "the removal-branch reply must be the exact certified recapture of the remover"
    )
    require(
      exploit.side != removedDefender.side && capturedTarget.side == removedDefender.side &&
        exploit.to == capturedTarget.square,
      "the exploit must retain its exact mover and captured target"
    )
    require(
      sameInitialPiece(removedDefender, immediateRecapture.movement) &&
        immediateRecapture.movement.to == exploit.to &&
        immediateRecapture.capture.exists(capture =>
          capture.capturedSide == exploit.side &&
            capture.capturedRole == exploit.afterRole &&
            capture.capturedSquare == exploit.to
        ),
      "the immediate-capture reply must be the removed defender's exact recapture of the exploit"
    )

    val contractKind = BoundedCausalContractKind.SoleRecapturerRemovalBeforeTargetCapture
    val semanticParts: List[String] = List(
      remover.stableKey,
      BoundedCausalIdentity.coloredPieceKey(removedDefender),
      removalRecapture.stableKey,
      exploit.stableKey,
      BoundedCausalIdentity.coloredPieceKey(capturedTarget),
      immediateRecapture.stableKey
    )

  def proposition(
      rootFen: String,
      remover: RelationMoveTransitionWitness,
      removedDefender: RelationColoredPieceWitness,
      removalRecapture: RelationLegalMoveResourceWitness,
      exploit: RelationMoveTransitionWitness,
      capturedTarget: RelationColoredPieceWitness,
      immediateRecapture: RelationLegalMoveResourceWitness
  ): CausalPropositionIdentity =
    CausalPropositionIdentity.from(
      PropositionDescriptor(
        rootFen,
        remover,
        removedDefender,
        removalRecapture,
        exploit,
        capturedTarget,
        immediateRecapture
      )
    )

  private def sameInitialPiece(
      piece: RelationColoredPieceWitness,
      movement: RelationMoveTransitionWitness
  ): Boolean =
    piece.side == movement.side && piece.square == movement.from && piece.role == movement.beforeRole

private[chessjudgment] sealed trait SoleRecapturerRemovalBeforeTargetCaptureManifest
    extends BoundedCausalContractManifest:
  def postRemovalNoRecapture: CausalClosedAbsenceBinding
  def postRemovalPersistence: List[CausalClosedStateBinding]
  final def contractKind = BoundedCausalContractKind.SoleRecapturerRemovalBeforeTargetCapture
  final def absenceBindings = List(postRemovalNoRecapture)
  final override def stateBindings = postRemovalPersistence
  final def stableKey: String =
    List(
      contractKind.toString.toLowerCase,
      premiseUses.map(_.stableKey).mkString("[", ",", "]"),
      postRemovalNoRecapture.stableKey,
      postRemovalPersistence.map(_.stableKey).mkString("[", ",", "]")
    ).mkString("|")

private[chessjudgment] object SoleRecapturerRemovalBeforeTargetCaptureManifest:
  private final case class Exact(
      removalCapture: CausalVerticalRelationPremiseUse,
      postRemovalCapture: CausalVerticalRelationPremiseUse,
      immediateCapture: CausalVerticalRelationPremiseUse,
      postRemovalNoRecapture: CausalClosedAbsenceBinding,
      postRemovalPersistence: List[CausalClosedStateBinding]
  ) extends SoleRecapturerRemovalBeforeTargetCaptureManifest:
    val premiseUses = List(removalCapture, postRemovalCapture, immediateCapture)

  def exact(
      removalCapture: CausalVerticalRelationPremiseUse,
      postRemovalCapture: CausalVerticalRelationPremiseUse,
      immediateCapture: CausalVerticalRelationPremiseUse,
      postRemovalNoRecapture: CausalClosedAbsenceBinding,
      exploitActor: RelationColoredPieceWitness,
      capturedTarget: RelationColoredPieceWitness,
      postRemovalPersistence: List[CausalClosedStateBinding]
  ): SoleRecapturerRemovalBeforeTargetCaptureManifest =
    require(
      removalCapture.role == SoleRecapturerRemovalBeforeTargetCapturePremiseRole.DefenderRemoval &&
        removalCapture.contract == VerticalRelationContractKind.CaptureRecaptureInventory &&
        removalCapture.result.kind == RelationFactKind.CaptureRecaptureInventory &&
        removalCapture.branchRole == SoleRecapturerRemovalBranchRole.RemovalThenTargetCapture &&
        removalCapture.stepIndex == 0,
      "defender removal must consume the removal-branch root capture inventory"
    )
    require(
      postRemovalCapture.role == SoleRecapturerRemovalBeforeTargetCapturePremiseRole.PostRemovalTargetCaptureInventory &&
        postRemovalCapture.contract == VerticalRelationContractKind.CaptureRecaptureInventory &&
        postRemovalCapture.result.kind == RelationFactKind.CaptureRecaptureInventory &&
        postRemovalCapture.branchRole == SoleRecapturerRemovalBranchRole.RemovalThenTargetCapture &&
        postRemovalCapture.branchId == removalCapture.branchId &&
        postRemovalCapture.stepIndex == 2,
      "the realized exploit must consume the post-removal third-ply capture inventory"
    )
    require(
      immediateCapture.role == SoleRecapturerRemovalBeforeTargetCapturePremiseRole.ImmediateTargetCaptureInventory &&
        immediateCapture.contract == VerticalRelationContractKind.CaptureRecaptureInventory &&
        immediateCapture.result.kind == RelationFactKind.CaptureRecaptureInventory &&
        immediateCapture.branchRole == SoleRecapturerRemovalBranchRole.ImmediateTargetCapture &&
        immediateCapture.stepIndex == 0,
      "the failed exploit must consume the immediate-capture root inventory"
    )
    require(
      postRemovalNoRecapture.role == SoleRecapturerRemovalBeforeTargetCaptureAbsenceRole.PostRemovalReplacementRecaptureAbsent &&
        postRemovalNoRecapture.branchRole == SoleRecapturerRemovalBranchRole.RemovalThenTargetCapture &&
        postRemovalNoRecapture.branchId == postRemovalCapture.branchId &&
        postRemovalNoRecapture.afterStepIndex == postRemovalCapture.stepIndex,
      "the closed recapture absence must belong to the post-removal target-capture occurrence"
    )
    validatePersistence(postRemovalCapture, exploitActor, capturedTarget, postRemovalPersistence)
    Exact(removalCapture, postRemovalCapture, immediateCapture, postRemovalNoRecapture, postRemovalPersistence)

  private def validatePersistence(
      postRemovalCapture: CausalVerticalRelationPremiseUse,
      exploitActor: RelationColoredPieceWitness,
      capturedTarget: RelationColoredPieceWitness,
      persistence: List[CausalClosedStateBinding]
  ): Unit =
    val expected = (0 until postRemovalCapture.stepIndex).toList.flatMap(index =>
      List(
        (index, SoleRecapturerRemovalBeforeTargetCaptureStateRole.PostRemovalExploitActorPresent, exploitActor),
        (index, SoleRecapturerRemovalBeforeTargetCaptureStateRole.PostRemovalTargetPresent, capturedTarget)
      )
    )
    require(
      persistence.zip(expected).forall { case (binding, (index, role, piece)) =>
        binding.role == role && binding.branchRole == SoleRecapturerRemovalBranchRole.RemovalThenTargetCapture &&
          binding.branchId == postRemovalCapture.branchId && binding.afterStepIndex == index &&
          binding.query == PositionRelationExtractor.ClosedPositionStateQuery.OccupiedBy(piece)
      } && persistence.size == expected.size,
      "the post-removal exploit actor and target need exact occupied states after every pre-exploit step"
    )

private[chessjudgment] final case class SoleRecapturerRemovalBeforeTargetCaptureSemanticProof private[chessjudgment] (
    identity: CausalPropositionIdentity,
    remover: RelationMoveTransitionWitness,
    removedDefender: RelationColoredPieceWitness,
    removalRecapture: RelationLegalMoveResourceWitness,
    exploit: RelationMoveTransitionWitness,
    capturedTarget: RelationColoredPieceWitness,
    immediateRecapture: RelationLegalMoveResourceWitness
):
  require(
    identity.contractKind == BoundedCausalContractKind.SoleRecapturerRemovalBeforeTargetCapture,
    "a sole-recapturer-removal-before-target-capture proof needs its exact causal contract"
  )
  require(
    remover.side != removedDefender.side && remover.to == removedDefender.square &&
      removalRecapture.movement.side == removedDefender.side,
    "the semantic proof must retain the exact defender-removal sequence"
  )
  require(
    exploit.to == capturedTarget.square && capturedTarget.side == removedDefender.side &&
      removedDefender.side == immediateRecapture.movement.side &&
      removedDefender.role == immediateRecapture.movement.beforeRole &&
      removedDefender.square == immediateRecapture.movement.from &&
      immediateRecapture.movement.to == exploit.to,
    "the semantic proof must retain the exact lost recapture obligation"
  )

  def semanticId: String = identity.semanticId
  def rootBoardState: String = identity.rootPositionIdentity.value

private[chessjudgment] final case class SoleRecapturerRemovalBeforeTargetCaptureOccurrence private[chessjudgment] (
    proofSet: BoundedCausalProofSet,
    subjectOccurrence: ExplanationSubjectOccurrence
):
  require(
    proofSet.proposition.contractKind == BoundedCausalContractKind.SoleRecapturerRemovalBeforeTargetCapture,
    "a sole-recapturer-removal-before-target-capture occurrence needs its exact causal contract"
  )
  require(
    proofSet.occurrence.branches.size == 2 &&
      proofSet.occurrence.branch(SoleRecapturerRemovalBranchRole.RemovalThenTargetCapture).nonEmpty &&
      proofSet.occurrence.branch(SoleRecapturerRemovalBranchRole.ImmediateTargetCapture).nonEmpty,
    "a sole-recapturer occurrence needs its removal and immediate-capture branches"
  )
  require(removalSteps.size == 3, "the removal branch needs removal, reply, and target capture")
  require(immediateSteps.size == 2, "the immediate branch needs target capture and recapture")
  require(ownsSubject, "the proof occurrence must retain its exact requested root occurrence")

  private def exactBranch(role: SoleRecapturerRemovalBranchRole): CausalBranchOccurrence =
    proofSet.occurrence
      .branch(role)
      .getOrElse(
        throw IllegalStateException(
          s"a sole-recapturer-removal-before-target-capture occurrence lost its $role branch"
        )
      )

  def semanticId: String = proofSet.proposition.semanticId
  def occurrenceId: String = proofSet.occurrence.occurrenceId
  def removalBranch: CausalBranchOccurrence =
    exactBranch(SoleRecapturerRemovalBranchRole.RemovalThenTargetCapture)
  def immediateCaptureBranch: CausalBranchOccurrence =
    exactBranch(SoleRecapturerRemovalBranchRole.ImmediateTargetCapture)
  def removalLine: LineNodeRef = removalBranch.line
  def immediateCaptureLine: LineNodeRef = immediateCaptureBranch.line
  def removalSteps: List[LineReplayStep] = removalBranch.replaySteps
  def immediateSteps: List[LineReplayStep] = immediateCaptureBranch.replaySteps
  def proofPaths: List[CausalProofPathOccurrence] = proofSet.paths
  def removalStep: LineReplayStep = removalSteps.head
  def removalRecaptureStep: LineReplayStep = removalSteps(1)
  def postRemovalCaptureStep: LineReplayStep = removalSteps(2)
  def immediateCaptureStep: LineReplayStep = immediateSteps.head
  def immediateRecaptureStep: LineReplayStep = immediateSteps(1)

  private def ownsSubject: Boolean =
    List(removalBranch, immediateCaptureBranch).exists { branch =>
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

private[chessjudgment] final case class SoleRecapturerRemovalBeforeTargetCaptureDependencyManifest private[chessjudgment] (
    subject: CertifiedRootOccurrence,
    removal: CertifiedRootOccurrence,
    immediateCapture: CertifiedRootOccurrence,
    removedDefender: RelationColoredPieceWitness,
    proofSet: BoundedCausalProofSet
) extends BoundedCausalDependencyManifest:
  val contractKind = BoundedCausalContractKind.SoleRecapturerRemovalBeforeTargetCapture
  require(
    proofSet.proposition.contractKind == contractKind &&
      proofSet.paths.forall(_.manifest.contractKind == contractKind),
    "a dependency manifest may contain only sole-recapturer-removal-before-target-capture proof paths"
  )

  val stableKey: String =
    List(
      "subject-root",
      BoundedCausalIdentity.evidenceRecordKey(subject.lineOwner),
      BoundedCausalIdentity.evidenceRecordKey(subject.transitionOwner),
      "removal-root",
      BoundedCausalIdentity.evidenceRecordKey(removal.lineOwner),
      BoundedCausalIdentity.evidenceRecordKey(removal.transitionOwner),
      "immediate-capture-root",
      BoundedCausalIdentity.evidenceRecordKey(immediateCapture.lineOwner),
      BoundedCausalIdentity.evidenceRecordKey(immediateCapture.transitionOwner),
      BoundedCausalIdentity.coloredPieceKey(removedDefender),
      proofSet.proposition.semanticId,
      proofSet.occurrence.occurrenceId,
      proofSet.paths.map(_.pathOccurrenceId).mkString("[", ",", "]")
    ).mkString("|")

  def consumes(
      exactSubject: CertifiedRootOccurrence,
      exactRemoval: CertifiedRootOccurrence,
      exactImmediateCapture: CertifiedRootOccurrence
  ): Boolean =
    subject == exactSubject && removal == exactRemoval && immediateCapture == exactImmediateCapture

/** Opaque certified family result and sole graph-record proof authority. */
private[chessjudgment] final class CertifiedSoleRecapturerRemovalBeforeTargetCapture private (
    val semantic: SoleRecapturerRemovalBeforeTargetCaptureSemanticProof,
    val occurrence: SoleRecapturerRemovalBeforeTargetCaptureOccurrence,
    val dependency: BoundedCausalDependencyFingerprint,
    private val dependencyManifest: SoleRecapturerRemovalBeforeTargetCaptureDependencyManifest,
    val subject: CertifiedRootOccurrence,
    private val removal: CertifiedRootOccurrence,
    private val immediateCapture: CertifiedRootOccurrence,
    private val absenceUse: CausalClosedAbsenceUse,
    private val absenceAuthority: ClosedRelationAbsenceAuthority,
    private val stateAuthorities: List[
      (CausalClosedStateUse, ClosedPositionStateAuthority)
    ]
):
  require(
    semantic.identity == occurrence.proofSet.proposition &&
      dependencyManifest.removedDefender == semantic.removedDefender &&
      dependencyManifest.proofSet == occurrence.proofSet &&
      BoundedCausalDependencyFingerprint.from(dependencyManifest) == dependency,
    "a certified sole-recapturer-removal-before-target-capture result needs one proposition and complete dependency manifest"
  )
  require(
    occurrence.proofPaths.flatMap(_.closedAbsenceUses) == List(absenceUse),
    "the sole-recapturer path needs its exact closed absence use"
  )
  require(
    occurrence.proofPaths.flatMap(_.closedStateUses).sortBy(_.useId) ==
      stateAuthorities.map(_._1).sortBy(_.useId) &&
      stateAuthorities.map(_._1.useId).distinct.size == stateAuthorities.size,
    "the sole-recapturer path needs one authority for every exact closed state use"
  )

  def parentSources: List[EvidenceRef] =
    val sources = List(
      removal.lineOwner.ref,
      removal.transitionOwner.ref,
      immediateCapture.lineOwner.ref,
      immediateCapture.transitionOwner.ref
    ).sortBy(_.id)
    require(sources.map(_.id).distinct.size == sources.size, "the two roots need distinct line and transition owners")
    sources

  private[chessjudgment] def lowerIssuerRecords: List[EvidenceRecord] =
    List(removal.lineOwner, removal.transitionOwner, immediateCapture.lineOwner, immediateCapture.transitionOwner)

  def consumesDependencies(
      exactSubject: CertifiedRootOccurrence,
      exactRemoval: CertifiedRootOccurrence,
      exactImmediateCapture: CertifiedRootOccurrence
  ): Boolean = dependencyManifest.consumes(exactSubject, exactRemoval, exactImmediateCapture)

  def proves(record: EvidenceRecord, payload: SoleRecapturerRemovalBeforeTargetCaptureEvidence): Boolean =
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
    subject.remainsCertified && removal.remainsCertified && immediateCapture.remainsCertified &&
      (subject == removal || subject == immediateCapture) &&
      occurrence.subjectOccurrence == subject.publicOccurrence &&
      occurrence.removalLine == removal.line && occurrence.immediateCaptureLine == immediateCapture.line &&
      removal.replay.replaySteps.take(3) == occurrence.removalSteps &&
      immediateCapture.replay.replaySteps.take(2) == occurrence.immediateSteps &&
      semanticIdentityRemainsCertified && captureSemanticsRemainCertified &&
      manifestRemainsCertified && premisesRemainCertified && absenceRemainsCertified &&
      statesRemainCertified

  private def semanticIdentityRemainsCertified: Boolean =
    SoleRecapturerRemovalBeforeTargetCaptureCausalAuthority.proposition(
      semantic.rootBoardState,
      semantic.remover,
      semantic.removedDefender,
      semantic.removalRecapture,
      semantic.exploit,
      semantic.capturedTarget,
      semantic.immediateRecapture
    ) == semantic.identity

  private def captureSemanticsRemainCertified: Boolean =
    val rebound = for
      (removalOccurrence, removalInventory) <- uniqueCapture(removal.replay, occurrence.removalStep)
      (removalResult, removalRecapture) <- removal.replay.exactRecaptureMembership(
        occurrence.removalStep,
        occurrence.removalRecaptureStep
      )
      if DerivedRelationResultKey.from(removalOccurrence.relation) == removalResult
      if removalInventory.mover == semantic.remover && removalInventory.captured == semantic.removedDefender
      if removalInventory.legalRecaptures == List(semantic.removalRecapture)
      if removalRecapture == semantic.removalRecapture
      (postRemovalOccurrence, postRemovalCapture) <- uniqueCapture(
        removal.replay,
        occurrence.postRemovalCaptureStep
      )
      if postRemovalCapture.mover == semantic.exploit &&
        postRemovalCapture.captured == semantic.capturedTarget &&
        postRemovalCapture.legalRecaptures.isEmpty
      (immediateOccurrence, immediateCaptureInventory) <- uniqueCapture(
        immediateCapture.replay,
        occurrence.immediateCaptureStep
      )
      (immediateResult, immediateRecapture) <- immediateCapture.replay.exactRecaptureMembership(
        occurrence.immediateCaptureStep,
        occurrence.immediateRecaptureStep
      )
      if DerivedRelationResultKey.from(immediateOccurrence.relation) == immediateResult
      if immediateCaptureInventory.mover == semantic.exploit &&
        immediateCaptureInventory.captured == semantic.capturedTarget
      if immediateCaptureInventory.legalRecaptures == List(semantic.immediateRecapture)
      if immediateRecapture == semantic.immediateRecapture
      if absenceAuthority.query ==
        PositionRelationExtractor.ClosedRelationAbsenceQuery.LegalCaptureOf(
          semantic.removedDefender.side,
          semantic.exploit.to
        )
    yield postRemovalOccurrence.occurrenceId != removalOccurrence.occurrenceId
    rebound.contains(true)

  private def manifestRemainsCertified: Boolean =
    occurrence.proofPaths match
      case path :: Nil =>
        path.manifest.isInstanceOf[SoleRecapturerRemovalBeforeTargetCaptureManifest] &&
          (path.premiseUses match
            case removal :: postRemovalCapture :: immediateCaptureUse :: Nil =>
              removal.role == SoleRecapturerRemovalBeforeTargetCapturePremiseRole.DefenderRemoval &&
                removal.contract == VerticalRelationContractKind.CaptureRecaptureInventory &&
                removal.result.kind == RelationFactKind.CaptureRecaptureInventory &&
                removal.branchId == occurrence.removalBranch.branchId && removal.stepIndex == 0 &&
                postRemovalCapture.role ==
                  SoleRecapturerRemovalBeforeTargetCapturePremiseRole.PostRemovalTargetCaptureInventory &&
                postRemovalCapture.contract == VerticalRelationContractKind.CaptureRecaptureInventory &&
                postRemovalCapture.result.kind == RelationFactKind.CaptureRecaptureInventory &&
                postRemovalCapture.branchId == occurrence.removalBranch.branchId &&
                postRemovalCapture.stepIndex == 2 &&
                immediateCaptureUse.role ==
                  SoleRecapturerRemovalBeforeTargetCapturePremiseRole.ImmediateTargetCaptureInventory &&
                immediateCaptureUse.contract == VerticalRelationContractKind.CaptureRecaptureInventory &&
                immediateCaptureUse.result.kind == RelationFactKind.CaptureRecaptureInventory &&
                immediateCaptureUse.branchId == occurrence.immediateCaptureBranch.branchId &&
                immediateCaptureUse.stepIndex == 0 &&
                List(removal, postRemovalCapture, immediateCaptureUse).forall(_.sourcePremiseIds.nonEmpty)
            case _ => false) &&
          path.closedAbsenceUses == List(absenceUse) &&
          absenceUse.binding.role ==
            SoleRecapturerRemovalBeforeTargetCaptureAbsenceRole.PostRemovalReplacementRecaptureAbsent &&
          absenceUse.binding.branchId == occurrence.removalBranch.branchId &&
          absenceUse.binding.afterStepIndex == 2 &&
          parentSources.map(_.id).distinct.size == 4 &&
          dependencyManifest.consumes(subject, removal, immediateCapture)
      case _ => false

  private def uniqueCapture(
      replay: CanonicalLineReplay,
      step: LineReplayStep
  ): Option[(ReplayVerticalRelationOccurrence, RelationWitnessDetail.CaptureRecaptureInventory)] =
    replay.verticalRelationOccurrences(
      step,
      List(VerticalRelationContractKind.CaptureRecaptureInventory)
    ) match
      case occurrence :: Nil =>
        occurrence.relation.detail match
          case detail: RelationWitnessDetail.CaptureRecaptureInventory => Some(occurrence -> detail)
          case _                                                       => None
      case _ => None

  private def premisesRemainCertified: Boolean =
    occurrence.proofPaths.flatMap(_.premiseUses).forall { use =>
      val replayAndOwner = use.branchRole match
        case SoleRecapturerRemovalBranchRole.RemovalThenTargetCapture =>
          Some(removal.replay -> removal.lineOwner)
        case SoleRecapturerRemovalBranchRole.ImmediateTargetCapture =>
          Some(immediateCapture.replay -> immediateCapture.lineOwner)
        case _ => None
      replayAndOwner.exists { case (exactReplay, owner) =>
        occurrence.proofSet.occurrence.branch(use.branchRole).exists(branch =>
          branch.branchId == use.branchId && branch.stepAt(use.stepIndex).exists(stepOccurrence =>
            exactReplay
              .verticalRelationOccurrences(stepOccurrence.step, List(use.contract))
              .exists(issuer =>
                RecordBoundVerticalRelationOccurrence.certified(owner, issuer).exists(authority =>
                  authority.result == use.result &&
                    authority.causalSourcePremiseIds == use.sourcePremiseIds
                )
              )
          )
        )
      }
    }

  private def absenceRemainsCertified: Boolean =
    val binding = absenceUse.binding
    binding.branchRole == SoleRecapturerRemovalBranchRole.RemovalThenTargetCapture &&
      binding.authority == absenceAuthority && absenceAuthority.issuerRecord == removal.lineOwner &&
      absenceAuthority.remainsCertified &&
      occurrence.proofSet.occurrence.branch(binding.branchRole).exists(branch =>
        branch.branchId == binding.branchId &&
          branch.stepAt(binding.afterStepIndex).exists(stepOccurrence =>
            stepOccurrence.line == absenceAuthority.issuerLine && stepOccurrence.step == absenceAuthority.step
          )
      )

  private def statesRemainCertified: Boolean =
    val exploitActor = RelationColoredPieceWitness(
      semantic.exploit.from,
      semantic.exploit.beforeRole,
      semantic.exploit.side
    )
    val exploitIndex = occurrence.removalSteps.size - 1
    stateAuthorities.size == exploitIndex * 2 &&
      stateAuthorities.forall { case (use, authority) =>
        val binding = use.binding
        val exactPiece = binding.role match
          case SoleRecapturerRemovalBeforeTargetCaptureStateRole.PostRemovalExploitActorPresent =>
            Some(exploitActor)
          case SoleRecapturerRemovalBeforeTargetCaptureStateRole.PostRemovalTargetPresent =>
            Some(semantic.capturedTarget)
          case _ => None
        exactPiece.exists(piece =>
          binding.branchRole == SoleRecapturerRemovalBranchRole.RemovalThenTargetCapture &&
            binding.branchId == occurrence.removalBranch.branchId &&
            binding.afterStepIndex >= 0 && binding.afterStepIndex < exploitIndex &&
            binding.query == PositionRelationExtractor.ClosedPositionStateQuery.OccupiedBy(piece) &&
            binding.authority == authority && authority.issuerRecord == removal.lineOwner &&
            ClosedPositionStateAuthority
              .certified(removal.lineOwner, authority.occurrence, authority.proof)
              .contains(authority) &&
            occurrence.removalBranch.stepAt(binding.afterStepIndex).exists(stepOccurrence =>
              stepOccurrence.line == authority.issuerLine && stepOccurrence.step == authority.step
            )
        )
      }

private[chessjudgment] object CertifiedSoleRecapturerRemovalBeforeTargetCapture:
  private[judgment] def from(
      semantic: SoleRecapturerRemovalBeforeTargetCaptureSemanticProof,
      occurrence: SoleRecapturerRemovalBeforeTargetCaptureOccurrence,
      dependency: BoundedCausalDependencyFingerprint,
      dependencyManifest: SoleRecapturerRemovalBeforeTargetCaptureDependencyManifest,
      subject: CertifiedRootOccurrence,
      removal: CertifiedRootOccurrence,
      immediateCapture: CertifiedRootOccurrence,
      absenceUse: CausalClosedAbsenceUse,
      absenceAuthority: ClosedRelationAbsenceAuthority,
      stateAuthorities: List[(CausalClosedStateUse, ClosedPositionStateAuthority)]
  ): CertifiedSoleRecapturerRemovalBeforeTargetCapture =
    val certificate = new CertifiedSoleRecapturerRemovalBeforeTargetCapture(
      semantic,
      occurrence,
      dependency,
      dependencyManifest,
      subject,
      removal,
      immediateCapture,
      absenceUse,
      absenceAuthority,
      stateAuthorities
    )
    require(
      certificate.remainsCertified,
      "a sole-recapturer-removal-before-target-capture certificate must retain its exact semantic, lower, and absence authorities"
    )
    certificate

/** Exact changed L1 occurrences selected once by the shared demand dispatcher.
  * Defense proof construction consumes these occurrences directly; it does not
  * scan the canonical replay a second time to rediscover them.
  */
private[chessjudgment] final case class SoleRecapturerRemovalBeforeTargetCaptureDemand private[chessjudgment] (
    removalOccurrence: ReplayVerticalRelationOccurrence,
    removalRecapture: RelationLegalMoveResourceWitness,
    postRemovalCaptureOccurrence: ReplayVerticalRelationOccurrence,
    immediateCaptureOccurrence: ReplayVerticalRelationOccurrence,
    immediateRecapture: RelationLegalMoveResourceWitness
):
  require(
    List(removalOccurrence, postRemovalCaptureOccurrence, immediateCaptureOccurrence).forall(
      _.contract == VerticalRelationContractKind.CaptureRecaptureInventory
    ),
    "a sole-recapturer-removal-before-target-capture demand must retain exact capture L1 occurrences"
  )

  private[chessjudgment] def stableKey: String =
    List(
      removalOccurrence.occurrenceId,
      removalRecapture.stableKey,
      postRemovalCaptureOccurrence.occurrenceId,
      immediateCaptureOccurrence.occurrenceId,
      immediateRecapture.stableKey
    ).mkString("|")

private[chessjudgment] object SoleRecapturerRemovalBeforeTargetCaptureProof:
  private final case class PersistenceState(
      role: SoleRecapturerRemovalBeforeTargetCaptureStateRole,
      stepIndex: Int,
      authority: ClosedPositionStateAuthority
  )

  private final case class ExactInputs(
      rootBoard: String,
      rootCapture: RelationWitnessDetail.CaptureRecaptureInventory,
      removalRecapture: RelationLegalMoveResourceWitness,
      rootCaptureOccurrence: ReplayVerticalRelationOccurrence,
      postRemovalCapture: RelationWitnessDetail.CaptureRecaptureInventory,
      postRemovalCaptureOccurrence: ReplayVerticalRelationOccurrence,
      immediateCaptureOccurrence: ReplayVerticalRelationOccurrence,
      immediateRecapture: RelationLegalMoveResourceWitness,
      removalCausalBranch: CausalBranchOccurrence,
      immediateCausalBranch: CausalBranchOccurrence,
      absenceAuthority: ClosedRelationAbsenceAuthority,
      persistenceStates: List[PersistenceState]
  )

  def certifyDemanded(
      subject: CertifiedRootOccurrence,
      removal: CertifiedRootOccurrence,
      immediateCapture: CertifiedRootOccurrence,
      demand: SoleRecapturerRemovalBeforeTargetCaptureDemand
  ): List[CertifiedSoleRecapturerRemovalBeforeTargetCapture] =
    val removalReplay = removal.replay
    val immediateReplay = immediateCapture.replay
    val inputs = for
      _ <- Option.when(
        subject.remainsCertified && removal.remainsCertified && immediateCapture.remainsCertified &&
          subject.isObserved && (subject == removal || subject == immediateCapture)
      )((): Unit)
      if removal.transition.from == immediateCapture.transition.from
      removalStep <- removalReplay.replaySteps.headOption
      removalRecaptureStep <- removalReplay.replaySteps.lift(1)
      postRemovalCaptureStep <- removalReplay.replaySteps.lift(2)
      immediateCaptureStep <- immediateReplay.replaySteps.headOption
      immediateRecaptureStep <- immediateReplay.replaySteps.lift(1)
      if EvidenceRef.sameMove(postRemovalCaptureStep.moveUci, immediateCaptureStep.moveUci)
      if !EvidenceRef.sameMove(removalStep.moveUci, immediateCaptureStep.moveUci)
      rootBoard <- PrincipalVariationEvidence.semanticBoardStateFen(removalStep.fenBefore)
      immediateRoot <- PrincipalVariationEvidence.semanticBoardStateFen(immediateCaptureStep.fenBefore)
      if rootBoard == immediateRoot
      rootCaptureOccurrence = demand.removalOccurrence
      if rootCaptureOccurrence.step == removalStep
      rootCapture <- captureDetail(rootCaptureOccurrence)
      removalRecapture = demand.removalRecapture
      if EvidenceRef.sameMove(removalRecapture.moveUci, removalRecaptureStep.moveUci)
      if rootCapture.legalRecaptures == List(removalRecapture)
      if rootCapture.captured.square == rootCapture.mover.to
      postRemovalCaptureOccurrence = demand.postRemovalCaptureOccurrence
      if postRemovalCaptureOccurrence.step == postRemovalCaptureStep
      postRemovalCapture <- captureDetail(postRemovalCaptureOccurrence)
      immediateCaptureOccurrence = demand.immediateCaptureOccurrence
      if immediateCaptureOccurrence.step == immediateCaptureStep
      immediateCaptureInventory <- captureDetail(immediateCaptureOccurrence)
      immediateRecapture = demand.immediateRecapture
      if EvidenceRef.sameMove(immediateRecapture.moveUci, immediateRecaptureStep.moveUci)
      if immediateCaptureInventory.legalRecaptures == List(immediateRecapture)
      if sameExploit(postRemovalCapture, immediateCaptureInventory)
      persistenceStates <- postRemovalPersistenceStates(
        removal.lineOwner,
        removalReplay,
        coloredPieceAtStart(postRemovalCapture.mover),
        postRemovalCapture.captured
      )
      if sameInitialPiece(rootCapture.captured, immediateRecapture.movement)
      if postRemovalCapture.legalRecaptures.isEmpty
      absenceQuery = PositionRelationExtractor.ClosedRelationAbsenceQuery.LegalCaptureOf(
        rootCapture.captured.side,
        postRemovalCapture.mover.to
      )
      absenceOccurrence <- removalReplay.positionAfter(postRemovalCaptureStep)
      absenceAuthority <- ClosedRelationAbsenceAuthority.forQuery(
        removal.lineOwner,
        absenceOccurrence,
        absenceQuery
      )
      if rootCaptureOccurrence.certifiedSourcePremiseIds.nonEmpty
      if postRemovalCaptureOccurrence.certifiedSourcePremiseIds.nonEmpty
      if immediateCaptureOccurrence.certifiedSourcePremiseIds.nonEmpty
      removalCausalBranch = CausalBranchOccurrence.fromRootOccurrence(
        SoleRecapturerRemovalBranchRole.RemovalThenTargetCapture,
        removal,
        3
      )
      immediateCausalBranch = CausalBranchOccurrence.fromRootOccurrence(
        SoleRecapturerRemovalBranchRole.ImmediateTargetCapture,
        immediateCapture,
        2
      )
    yield ExactInputs(
      rootBoard,
      rootCapture,
      removalRecapture,
      rootCaptureOccurrence,
      postRemovalCapture,
      postRemovalCaptureOccurrence,
      immediateCaptureOccurrence,
      immediateRecapture,
      removalCausalBranch,
      immediateCausalBranch,
      absenceAuthority,
      persistenceStates
    )

    inputs.toList.map(exact =>
      certify(
        exact,
        subject,
        removal,
        immediateCapture
      )
    )

  private def certify(
      inputs: ExactInputs,
      subject: CertifiedRootOccurrence,
      removal: CertifiedRootOccurrence,
      immediateCapture: CertifiedRootOccurrence
  ): CertifiedSoleRecapturerRemovalBeforeTargetCapture =
    val proposition = SoleRecapturerRemovalBeforeTargetCaptureCausalAuthority.proposition(
      inputs.rootBoard,
      inputs.rootCapture.mover,
      inputs.rootCapture.captured,
      inputs.removalRecapture,
      inputs.postRemovalCapture.mover,
      inputs.postRemovalCapture.captured,
      inputs.immediateRecapture
    )
    val causalOccurrence = CausalOccurrenceIdentity.from(
      proposition,
      List(inputs.removalCausalBranch, inputs.immediateCausalBranch)
    )
    val removalCaptureUse = CausalVerticalRelationPremiseUse.from(
      SoleRecapturerRemovalBeforeTargetCapturePremiseRole.DefenderRemoval,
      RecordBoundVerticalRelationOccurrence.certified(removal.lineOwner, inputs.rootCaptureOccurrence).getOrElse(
        throw IllegalArgumentException("defense obligation lost its graph-owned defender-removal occurrence")
      ),
      inputs.removalCausalBranch,
      0
    )
    val postRemovalCaptureUse = CausalVerticalRelationPremiseUse.from(
      SoleRecapturerRemovalBeforeTargetCapturePremiseRole.PostRemovalTargetCaptureInventory,
      RecordBoundVerticalRelationOccurrence.certified(removal.lineOwner, inputs.postRemovalCaptureOccurrence).getOrElse(
        throw IllegalArgumentException("defense obligation lost its graph-owned post-removal capture occurrence")
      ),
      inputs.removalCausalBranch,
      2
    )
    val immediateCaptureUse = CausalVerticalRelationPremiseUse.from(
      SoleRecapturerRemovalBeforeTargetCapturePremiseRole.ImmediateTargetCaptureInventory,
      RecordBoundVerticalRelationOccurrence.certified(immediateCapture.lineOwner, inputs.immediateCaptureOccurrence).getOrElse(
        throw IllegalArgumentException("defense obligation lost its graph-owned immediate capture occurrence")
      ),
      inputs.immediateCausalBranch,
      0
    )
    val absenceBinding = CausalClosedAbsenceBinding.afterStep(
      SoleRecapturerRemovalBeforeTargetCaptureAbsenceRole.PostRemovalReplacementRecaptureAbsent,
      inputs.absenceAuthority,
      inputs.removalCausalBranch,
      2
    )
    val persistenceBindings = inputs.persistenceStates.map(state =>
      CausalClosedStateBinding.afterStep(
        state.role,
        state.authority,
        inputs.removalCausalBranch,
        state.stepIndex
      )
    )
    val manifest = SoleRecapturerRemovalBeforeTargetCaptureManifest.exact(
      removalCaptureUse,
      postRemovalCaptureUse,
      immediateCaptureUse,
      absenceBinding,
      coloredPieceAtStart(inputs.postRemovalCapture.mover),
      inputs.postRemovalCapture.captured,
      persistenceBindings
    )
    val path = CausalProofPathOccurrence.from(proposition, manifest)
    val absenceUse = path.closedAbsenceUses match
      case exact :: Nil => exact
      case other =>
        throw IllegalStateException(
          s"a sole-recapturer-removal-before-target-capture path retained ${other.size} absences"
        )
    val proofSet = BoundedCausalProofSet.from(
      proposition,
      causalOccurrence,
      List(path)
    )
    val semantic = SoleRecapturerRemovalBeforeTargetCaptureSemanticProof(
      proposition,
      inputs.rootCapture.mover,
      inputs.rootCapture.captured,
      inputs.removalRecapture,
      inputs.postRemovalCapture.mover,
      inputs.postRemovalCapture.captured,
      inputs.immediateRecapture
    )
    val occurrence = SoleRecapturerRemovalBeforeTargetCaptureOccurrence(proofSet, subject.publicOccurrence)
    val dependencyManifest = SoleRecapturerRemovalBeforeTargetCaptureDependencyManifest(
      subject,
      removal,
      immediateCapture,
      semantic.removedDefender,
      proofSet
    )
    val dependency = BoundedCausalDependencyFingerprint.from(dependencyManifest)
    CertifiedSoleRecapturerRemovalBeforeTargetCapture.from(
      semantic,
      occurrence,
      dependency,
      dependencyManifest,
      subject,
      removal,
      immediateCapture,
      absenceUse,
      inputs.absenceAuthority,
      path.closedStateUses.map(use => use -> use.binding.authority)
    )

  private def captureDetail(
      occurrence: ReplayVerticalRelationOccurrence
  ): Option[RelationWitnessDetail.CaptureRecaptureInventory] =
    occurrence.relation.detail match
      case exact: RelationWitnessDetail.CaptureRecaptureInventory => Some(exact)
      case _                                                       => None

  private def sameExploit(
      postRemoval: RelationWitnessDetail.CaptureRecaptureInventory,
      immediate: RelationWitnessDetail.CaptureRecaptureInventory
  ): Boolean =
    postRemoval.mover == immediate.mover && postRemoval.captured == immediate.captured

  private def sameInitialPiece(
      piece: RelationColoredPieceWitness,
      movement: RelationMoveTransitionWitness
  ): Boolean =
    piece.side == movement.side && piece.square == movement.from && piece.role == movement.beforeRole

  private def coloredPieceAtStart(
      movement: RelationMoveTransitionWitness
  ): RelationColoredPieceWitness =
    RelationColoredPieceWitness(movement.from, movement.beforeRole, movement.side)

  private def postRemovalPersistenceStates(
      removalLineRecord: EvidenceRecord,
      replay: CanonicalLineReplay,
      exploitActor: RelationColoredPieceWitness,
      target: RelationColoredPieceWitness
  ): Option[List[PersistenceState]] =
    val expected = replay.replaySteps.take(2).zipWithIndex.flatMap { case (step, index) =>
      List(
        SoleRecapturerRemovalBeforeTargetCaptureStateRole.PostRemovalExploitActorPresent -> exploitActor,
        SoleRecapturerRemovalBeforeTargetCaptureStateRole.PostRemovalTargetPresent -> target
      ).flatMap { case (role, piece) =>
        ClosedPositionStateAuthority
          .atStep(removalLineRecord, step)((occurrence, scope) =>
            occurrence.existingOccupantState(piece, scope)
          )
          .map(PersistenceState(role, index, _))
      }
    }
    Option.when(expected.size == 4)(expected)
