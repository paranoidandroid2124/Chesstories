package lila.chessjudgment.model.judgment

import lila.chessjudgment.analysis.position.PositionRelationExtractor
import lila.chessjudgment.model.line.PrincipalVariationEvidence

private[chessjudgment] enum SoleRecapturerRemovalBeforeTargetCapturePremiseRole extends CausalPremiseRole:
  case ReferenceDefenderRemoval
  case ReferenceLaterExploitInventory
  case PlayedImmediateExploitInventory

  def stableKey: String =
    this match
      case ReferenceDefenderRemoval          => "reference-defender-removal"
      case ReferenceLaterExploitInventory    => "reference-later-exploit-inventory"
      case PlayedImmediateExploitInventory   => "played-immediate-exploit-inventory"

private[chessjudgment] enum SoleRecapturerRemovalBeforeTargetCaptureAbsenceRole extends CausalAbsenceRole:
  case ReferenceReplacementRecaptureAbsent

  def stableKey: String = "reference-replacement-recapture-absent"

private[chessjudgment] enum SoleRecapturerRemovalBeforeTargetCaptureStateRole extends CausalStateRole:
  case ReferenceExploitActorPresent
  case ReferenceTargetPresent

  def stableKey: String =
    this match
      case ReferenceExploitActorPresent => "reference-exploit-actor-present"
      case ReferenceTargetPresent       => "reference-target-present"

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
      playedRecapture: RelationLegalMoveResourceWitness
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
      "the reference reply must be the exact certified recapture of the remover"
    )
    require(
      exploit.side != removedDefender.side && capturedTarget.side == removedDefender.side &&
        exploit.to == capturedTarget.square,
      "the exploit must retain its exact mover and captured target"
    )
    require(
      sameInitialPiece(removedDefender, playedRecapture.movement) &&
        playedRecapture.movement.to == exploit.to &&
        playedRecapture.capture.exists(capture =>
          capture.capturedSide == exploit.side &&
            capture.capturedRole == exploit.afterRole &&
            capture.capturedSquare == exploit.to
        ),
      "the played reply must be the removed defender's exact recapture of the exploit"
    )

    val contractKind = BoundedCausalContractKind.SoleRecapturerRemovalBeforeTargetCapture
    val semanticParts: List[String] = List(
      remover.stableKey,
      BoundedCausalIdentity.coloredPieceKey(removedDefender),
      removalRecapture.stableKey,
      exploit.stableKey,
      BoundedCausalIdentity.coloredPieceKey(capturedTarget),
      playedRecapture.stableKey
    )

  def proposition(
      rootFen: String,
      remover: RelationMoveTransitionWitness,
      removedDefender: RelationColoredPieceWitness,
      removalRecapture: RelationLegalMoveResourceWitness,
      exploit: RelationMoveTransitionWitness,
      capturedTarget: RelationColoredPieceWitness,
      playedRecapture: RelationLegalMoveResourceWitness
  ): CausalPropositionIdentity =
    CausalPropositionIdentity.from(
      PropositionDescriptor(
        rootFen,
        remover,
        removedDefender,
        removalRecapture,
        exploit,
        capturedTarget,
        playedRecapture
      )
    )

  private def sameInitialPiece(
      piece: RelationColoredPieceWitness,
      movement: RelationMoveTransitionWitness
  ): Boolean =
    piece.side == movement.side && piece.square == movement.from && piece.role == movement.beforeRole

private[chessjudgment] sealed trait SoleRecapturerRemovalBeforeTargetCaptureManifest
    extends BoundedCausalContractManifest:
  def referenceNoRecapture: CausalClosedAbsenceBinding
  def referencePersistence: List[CausalClosedStateBinding]
  final def contractKind = BoundedCausalContractKind.SoleRecapturerRemovalBeforeTargetCapture
  final def absenceBindings = List(referenceNoRecapture)
  final override def stateBindings = referencePersistence
  final def stableKey: String =
    List(
      contractKind.toString.toLowerCase,
      premiseUses.map(_.stableKey).mkString("[", ",", "]"),
      referenceNoRecapture.stableKey,
      referencePersistence.map(_.stableKey).mkString("[", ",", "]")
    ).mkString("|")

private[chessjudgment] object SoleRecapturerRemovalBeforeTargetCaptureManifest:
  private final case class Exact(
      referenceRemoval: CausalVerticalRelationPremiseUse,
      referenceExploit: CausalVerticalRelationPremiseUse,
      playedExploit: CausalVerticalRelationPremiseUse,
      referenceNoRecapture: CausalClosedAbsenceBinding,
      referencePersistence: List[CausalClosedStateBinding]
  ) extends SoleRecapturerRemovalBeforeTargetCaptureManifest:
    val premiseUses = List(referenceRemoval, referenceExploit, playedExploit)

  def exact(
      referenceRemoval: CausalVerticalRelationPremiseUse,
      referenceExploit: CausalVerticalRelationPremiseUse,
      playedExploit: CausalVerticalRelationPremiseUse,
      referenceNoRecapture: CausalClosedAbsenceBinding,
      exploitActor: RelationColoredPieceWitness,
      capturedTarget: RelationColoredPieceWitness,
      referencePersistence: List[CausalClosedStateBinding]
  ): SoleRecapturerRemovalBeforeTargetCaptureManifest =
    require(
      referenceRemoval.role == SoleRecapturerRemovalBeforeTargetCapturePremiseRole.ReferenceDefenderRemoval &&
        referenceRemoval.contract == VerticalRelationContractKind.CaptureRecaptureInventory &&
        referenceRemoval.result.kind == RelationFactKind.CaptureRecaptureInventory &&
        referenceRemoval.branchRole == ComparedLineBranchRole.CounterfactualReference &&
        referenceRemoval.stepIndex == 0,
      "defender removal must consume the reference root capture inventory"
    )
    require(
      referenceExploit.role == SoleRecapturerRemovalBeforeTargetCapturePremiseRole.ReferenceLaterExploitInventory &&
        referenceExploit.contract == VerticalRelationContractKind.CaptureRecaptureInventory &&
        referenceExploit.result.kind == RelationFactKind.CaptureRecaptureInventory &&
        referenceExploit.branchRole == ComparedLineBranchRole.CounterfactualReference &&
        referenceExploit.branchId == referenceRemoval.branchId &&
        referenceExploit.stepIndex == 2,
      "the realized exploit must consume the reference third-ply capture inventory"
    )
    require(
      playedExploit.role == SoleRecapturerRemovalBeforeTargetCapturePremiseRole.PlayedImmediateExploitInventory &&
        playedExploit.contract == VerticalRelationContractKind.CaptureRecaptureInventory &&
        playedExploit.result.kind == RelationFactKind.CaptureRecaptureInventory &&
        playedExploit.branchRole == ComparedLineBranchRole.PlayedRootAnalysisContinuation &&
        playedExploit.stepIndex == 0,
      "the failed exploit must consume the played root capture inventory"
    )
    require(
      referenceNoRecapture.role == SoleRecapturerRemovalBeforeTargetCaptureAbsenceRole.ReferenceReplacementRecaptureAbsent &&
        referenceNoRecapture.branchRole == ComparedLineBranchRole.CounterfactualReference &&
        referenceNoRecapture.branchId == referenceExploit.branchId &&
        referenceNoRecapture.afterStepIndex == referenceExploit.stepIndex,
      "the closed recapture absence must belong to the reference exploit occurrence"
    )
    validatePersistence(referenceExploit, exploitActor, capturedTarget, referencePersistence)
    Exact(referenceRemoval, referenceExploit, playedExploit, referenceNoRecapture, referencePersistence)

  private def validatePersistence(
      referenceExploit: CausalVerticalRelationPremiseUse,
      exploitActor: RelationColoredPieceWitness,
      capturedTarget: RelationColoredPieceWitness,
      persistence: List[CausalClosedStateBinding]
  ): Unit =
    val expected = (0 until referenceExploit.stepIndex).toList.flatMap(index =>
      List(
        (index, SoleRecapturerRemovalBeforeTargetCaptureStateRole.ReferenceExploitActorPresent, exploitActor),
        (index, SoleRecapturerRemovalBeforeTargetCaptureStateRole.ReferenceTargetPresent, capturedTarget)
      )
    )
    require(
      persistence.zip(expected).forall { case (binding, (index, role, piece)) =>
        binding.role == role && binding.branchRole == ComparedLineBranchRole.CounterfactualReference &&
          binding.branchId == referenceExploit.branchId && binding.afterStepIndex == index &&
          binding.query == PositionRelationExtractor.ClosedPositionStateQuery.OccupiedBy(piece)
      } && persistence.size == expected.size,
      "the reference exploit actor and target need exact occupied states after every pre-exploit step"
    )

private[chessjudgment] final case class SoleRecapturerRemovalBeforeTargetCaptureSemanticProof private[chessjudgment] (
    identity: CausalPropositionIdentity,
    remover: RelationMoveTransitionWitness,
    removedDefender: RelationColoredPieceWitness,
    removalRecapture: RelationLegalMoveResourceWitness,
    exploit: RelationMoveTransitionWitness,
    capturedTarget: RelationColoredPieceWitness,
    playedRecapture: RelationLegalMoveResourceWitness
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
      removedDefender.side == playedRecapture.movement.side &&
      removedDefender.role == playedRecapture.movement.beforeRole &&
      removedDefender.square == playedRecapture.movement.from &&
      playedRecapture.movement.to == exploit.to,
    "the semantic proof must retain the exact lost recapture obligation"
  )

  def semanticId: String = identity.semanticId
  def rootBoardState: String = identity.rootPositionIdentity.value

private[chessjudgment] final case class SoleRecapturerRemovalBeforeTargetCaptureOccurrence private[chessjudgment] (
    proofSet: BoundedCausalProofSet
):
  require(
    proofSet.proposition.contractKind == BoundedCausalContractKind.SoleRecapturerRemovalBeforeTargetCapture,
    "a sole-recapturer-removal-before-target-capture occurrence needs its exact causal contract"
  )
  require(
    proofSet.occurrence.branches.size == 2 &&
      proofSet.occurrence.branch(ComparedLineBranchRole.CounterfactualReference).exists(
        _.line.role == LineNodeRole.BestReference
      ) &&
      proofSet.occurrence.branch(ComparedLineBranchRole.PlayedRootAnalysisContinuation).exists(
        _.line.role == LineNodeRole.Played
      ),
    "a sole-recapturer-removal-before-target-capture occurrence needs one BestReference and one played-root analysis continuation"
  )
  require(referenceSteps.size == 3, "the reference occurrence needs removal, reply, and exploit")
  require(playedSteps.size == 2, "the played-root analysis continuation needs exploit and recapture")

  private def exactBranch(role: ComparedLineBranchRole): CausalBranchOccurrence =
    proofSet.occurrence
      .branch(role)
      .getOrElse(
        throw IllegalStateException(
          s"a sole-recapturer-removal-before-target-capture occurrence lost its $role branch"
        )
      )

  def semanticId: String = proofSet.proposition.semanticId
  def occurrenceId: String = proofSet.occurrence.occurrenceId
  def referenceBranch: CausalBranchOccurrence = exactBranch(ComparedLineBranchRole.CounterfactualReference)
  def playedBranch: CausalBranchOccurrence =
    exactBranch(ComparedLineBranchRole.PlayedRootAnalysisContinuation)
  def referenceLine: LineNodeRef = referenceBranch.line
  def playedLine: LineNodeRef = playedBranch.line
  def referenceSteps: List[LineReplayStep] = referenceBranch.replaySteps
  def playedSteps: List[LineReplayStep] = playedBranch.replaySteps
  def proofPaths: List[CausalProofPathOccurrence] = proofSet.paths
  def removalStep: LineReplayStep = referenceSteps.head
  def removalRecaptureStep: LineReplayStep = referenceSteps(1)
  def referenceExploitStep: LineReplayStep = referenceSteps(2)
  def playedExploitStep: LineReplayStep = playedSteps.head
  def playedRecaptureStep: LineReplayStep = playedSteps(1)

private[chessjudgment] final case class SoleRecapturerRemovalBeforeTargetCaptureDependencyManifest private[chessjudgment] (
    referenceLineRecord: EvidenceRecord,
    playedLineRecord: EvidenceRecord,
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
      BoundedCausalIdentity.evidenceRecordKey(referenceLineRecord),
      BoundedCausalIdentity.evidenceRecordKey(playedLineRecord),
      BoundedCausalIdentity.coloredPieceKey(removedDefender),
      proofSet.proposition.semanticId,
      proofSet.occurrence.occurrenceId,
      proofSet.paths.map(_.pathOccurrenceId).mkString("[", ",", "]")
    ).mkString("|")

  def consumes(referenceSource: EvidenceRecord, playedSource: EvidenceRecord): Boolean =
    referenceLineRecord == referenceSource && playedLineRecord == playedSource

/** Opaque certified family result and sole graph-record proof authority. */
private[chessjudgment] final class CertifiedSoleRecapturerRemovalBeforeTargetCapture private (
    val semantic: SoleRecapturerRemovalBeforeTargetCaptureSemanticProof,
    val occurrence: SoleRecapturerRemovalBeforeTargetCaptureOccurrence,
    val dependency: BoundedCausalDependencyFingerprint,
    private val dependencyManifest: SoleRecapturerRemovalBeforeTargetCaptureDependencyManifest,
    private val referenceLineRecord: EvidenceRecord,
    private val playedLineRecord: EvidenceRecord,
    private val referenceReplay: CanonicalLineReplay,
    private val playedReplay: CanonicalLineReplay,
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
    List(referenceLineRecord.ref, playedLineRecord.ref).sortBy(_.id)

  private[chessjudgment] def lowerIssuerRecords: List[EvidenceRecord] =
    List(referenceLineRecord, playedLineRecord)

  def consumesDependencies(referenceSource: EvidenceRecord, playedSource: EvidenceRecord): Boolean =
    dependencyManifest.consumes(referenceSource, playedSource)

  def proves(record: EvidenceRecord, payload: SoleRecapturerRemovalBeforeTargetCaptureEvidence): Boolean =
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
      ) && referenceLineRecord.ref.position == playedLineRecord.ref.position &&
      referenceReplay.replaySteps.take(3) == occurrence.referenceSteps &&
      playedReplay.replaySteps.take(2) == occurrence.playedSteps &&
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
      semantic.playedRecapture
    ) == semantic.identity

  private def captureSemanticsRemainCertified: Boolean =
    val rebound = for
      (removalOccurrence, removal) <- uniqueCapture(referenceReplay, occurrence.removalStep)
      (removalResult, removalRecapture) <- referenceReplay.exactRecaptureMembership(
        occurrence.removalStep,
        occurrence.removalRecaptureStep
      )
      if DerivedRelationResultKey.from(removalOccurrence.relation) == removalResult
      if removal.mover == semantic.remover && removal.captured == semantic.removedDefender
      if removal.legalRecaptures == List(semantic.removalRecapture)
      if removalRecapture == semantic.removalRecapture
      (referenceOccurrence, referenceExploit) <- uniqueCapture(
        referenceReplay,
        occurrence.referenceExploitStep
      )
      if referenceExploit.mover == semantic.exploit &&
        referenceExploit.captured == semantic.capturedTarget &&
        referenceExploit.legalRecaptures.isEmpty
      (playedOccurrence, playedExploit) <- uniqueCapture(playedReplay, occurrence.playedExploitStep)
      (playedResult, playedRecapture) <- playedReplay.exactRecaptureMembership(
        occurrence.playedExploitStep,
        occurrence.playedRecaptureStep
      )
      if DerivedRelationResultKey.from(playedOccurrence.relation) == playedResult
      if playedExploit.mover == semantic.exploit && playedExploit.captured == semantic.capturedTarget
      if playedExploit.legalRecaptures == List(semantic.playedRecapture)
      if playedRecapture == semantic.playedRecapture
      if absenceAuthority.query ==
        PositionRelationExtractor.ClosedRelationAbsenceQuery.LegalCaptureOf(
          semantic.removedDefender.side,
          semantic.exploit.to
        )
    yield referenceOccurrence.occurrenceId != removalOccurrence.occurrenceId
    rebound.contains(true)

  private def manifestRemainsCertified: Boolean =
    occurrence.proofPaths match
      case path :: Nil =>
        path.manifest.isInstanceOf[SoleRecapturerRemovalBeforeTargetCaptureManifest] &&
          (path.premiseUses match
            case removal :: referenceExploit :: playedExploit :: Nil =>
              removal.role == SoleRecapturerRemovalBeforeTargetCapturePremiseRole.ReferenceDefenderRemoval &&
                removal.contract == VerticalRelationContractKind.CaptureRecaptureInventory &&
                removal.result.kind == RelationFactKind.CaptureRecaptureInventory &&
                removal.branchId == occurrence.referenceBranch.branchId && removal.stepIndex == 0 &&
                referenceExploit.role ==
                  SoleRecapturerRemovalBeforeTargetCapturePremiseRole.ReferenceLaterExploitInventory &&
                referenceExploit.contract == VerticalRelationContractKind.CaptureRecaptureInventory &&
                referenceExploit.result.kind == RelationFactKind.CaptureRecaptureInventory &&
                referenceExploit.branchId == occurrence.referenceBranch.branchId &&
                referenceExploit.stepIndex == 2 &&
                playedExploit.role ==
                  SoleRecapturerRemovalBeforeTargetCapturePremiseRole.PlayedImmediateExploitInventory &&
                playedExploit.contract == VerticalRelationContractKind.CaptureRecaptureInventory &&
                playedExploit.result.kind == RelationFactKind.CaptureRecaptureInventory &&
                playedExploit.branchId == occurrence.playedBranch.branchId && playedExploit.stepIndex == 0 &&
                List(removal, referenceExploit, playedExploit).forall(_.sourcePremiseIds.nonEmpty)
            case _ => false) &&
          path.closedAbsenceUses == List(absenceUse) &&
          absenceUse.binding.role ==
            SoleRecapturerRemovalBeforeTargetCaptureAbsenceRole.ReferenceReplacementRecaptureAbsent &&
          absenceUse.binding.branchId == occurrence.referenceBranch.branchId &&
          absenceUse.binding.afterStepIndex == 2 &&
          parentSources.map(_.id).distinct.size == 2 &&
          dependencyManifest.consumes(referenceLineRecord, playedLineRecord)
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
      val replayAndOwner = Option.when(
        use.branchRole == ComparedLineBranchRole.CounterfactualReference
      )(referenceReplay -> referenceLineRecord).orElse(
        Option.when(use.branchRole == ComparedLineBranchRole.PlayedRootAnalysisContinuation)(
          playedReplay -> playedLineRecord
        )
      )
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
    binding.branchRole == ComparedLineBranchRole.CounterfactualReference &&
      binding.authority == absenceAuthority && absenceAuthority.issuerRecord == referenceLineRecord &&
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
    val exploitIndex = occurrence.referenceSteps.size - 1
    stateAuthorities.size == exploitIndex * 2 &&
      stateAuthorities.forall { case (use, authority) =>
        val binding = use.binding
        val exactPiece = binding.role match
          case SoleRecapturerRemovalBeforeTargetCaptureStateRole.ReferenceExploitActorPresent =>
            Some(exploitActor)
          case SoleRecapturerRemovalBeforeTargetCaptureStateRole.ReferenceTargetPresent =>
            Some(semantic.capturedTarget)
          case _ => None
        exactPiece.exists(piece =>
          binding.branchRole == ComparedLineBranchRole.CounterfactualReference &&
            binding.branchId == occurrence.referenceBranch.branchId &&
            binding.afterStepIndex >= 0 && binding.afterStepIndex < exploitIndex &&
            binding.query == PositionRelationExtractor.ClosedPositionStateQuery.OccupiedBy(piece) &&
            binding.authority == authority && authority.issuerRecord == referenceLineRecord &&
            ClosedPositionStateAuthority
              .certified(referenceLineRecord, authority.occurrence, authority.proof)
              .contains(authority) &&
            occurrence.referenceBranch.stepAt(binding.afterStepIndex).exists(stepOccurrence =>
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
      referenceLineRecord: EvidenceRecord,
      playedLineRecord: EvidenceRecord,
      referenceReplay: CanonicalLineReplay,
      playedReplay: CanonicalLineReplay,
      absenceUse: CausalClosedAbsenceUse,
      absenceAuthority: ClosedRelationAbsenceAuthority,
      stateAuthorities: List[(CausalClosedStateUse, ClosedPositionStateAuthority)]
  ): CertifiedSoleRecapturerRemovalBeforeTargetCapture =
    val certificate = new CertifiedSoleRecapturerRemovalBeforeTargetCapture(
      semantic,
      occurrence,
      dependency,
      dependencyManifest,
      referenceLineRecord,
      playedLineRecord,
      referenceReplay,
      playedReplay,
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
private[chessjudgment] final case class SoleRecapturerRemovalBeforeTargetCaptureChangedSeed private[chessjudgment] (
    removalOccurrence: ReplayVerticalRelationOccurrence,
    removalRecapture: RelationLegalMoveResourceWitness,
    referenceExploitOccurrence: ReplayVerticalRelationOccurrence,
    playedExploitOccurrence: ReplayVerticalRelationOccurrence,
    playedRecapture: RelationLegalMoveResourceWitness
):
  require(
    List(removalOccurrence, referenceExploitOccurrence, playedExploitOccurrence).forall(
      _.contract == VerticalRelationContractKind.CaptureRecaptureInventory
    ),
    "a sole-recapturer-removal-before-target-capture seed must retain exact capture L1 occurrences"
  )

  private[chessjudgment] def stableKey: String =
    List(
      removalOccurrence.occurrenceId,
      removalRecapture.stableKey,
      referenceExploitOccurrence.occurrenceId,
      playedExploitOccurrence.occurrenceId,
      playedRecapture.stableKey
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
      referenceExploit: RelationWitnessDetail.CaptureRecaptureInventory,
      referenceExploitOccurrence: ReplayVerticalRelationOccurrence,
      playedExploitOccurrence: ReplayVerticalRelationOccurrence,
      playedRecapture: RelationLegalMoveResourceWitness,
      referenceBranch: CausalBranchOccurrence,
      playedBranch: CausalBranchOccurrence,
      absenceAuthority: ClosedRelationAbsenceAuthority,
      persistenceStates: List[PersistenceState]
  )

  def deriveImmediate(
      referenceLine: LineNodeRef,
      playedLine: LineNodeRef,
      referenceLineRecord: EvidenceRecord,
      playedLineRecord: EvidenceRecord,
      referenceReplay: CanonicalLineReplay,
      playedReplay: CanonicalLineReplay,
      changedSeed: SoleRecapturerRemovalBeforeTargetCaptureChangedSeed
  ): List[CertifiedSoleRecapturerRemovalBeforeTargetCapture] =
    val inputs = for
      _ <- Option.when(
        CertifiedComparedLineAuthority.exactRecord(
          referenceLineRecord,
          referenceLine,
          referenceReplay
        )
      )((): Unit)
      if CertifiedComparedLineAuthority.exactRecord(
        playedLineRecord,
        playedLine,
        playedReplay
      )
      if referenceLineRecord.ref.position == playedLineRecord.ref.position
      removalStep <- referenceReplay.replaySteps.headOption
      removalRecaptureStep <- referenceReplay.replaySteps.lift(1)
      referenceExploitStep <- referenceReplay.replaySteps.lift(2)
      playedExploitStep <- playedReplay.replaySteps.headOption
      playedRecaptureStep <- playedReplay.replaySteps.lift(1)
      if EvidenceRef.sameMove(referenceExploitStep.moveUci, playedExploitStep.moveUci)
      if !EvidenceRef.sameMove(removalStep.moveUci, playedExploitStep.moveUci)
      rootBoard <- PrincipalVariationEvidence.semanticBoardStateFen(removalStep.fenBefore)
      playedRoot <- PrincipalVariationEvidence.semanticBoardStateFen(playedExploitStep.fenBefore)
      if rootBoard == playedRoot
      rootCaptureOccurrence = changedSeed.removalOccurrence
      if rootCaptureOccurrence.step == removalStep
      rootCapture <- captureDetail(rootCaptureOccurrence)
      removalRecapture = changedSeed.removalRecapture
      if EvidenceRef.sameMove(removalRecapture.moveUci, removalRecaptureStep.moveUci)
      if rootCapture.legalRecaptures == List(removalRecapture)
      if rootCapture.captured.square == rootCapture.mover.to
      referenceExploitOccurrence = changedSeed.referenceExploitOccurrence
      if referenceExploitOccurrence.step == referenceExploitStep
      referenceExploit <- captureDetail(referenceExploitOccurrence)
      playedExploitOccurrence = changedSeed.playedExploitOccurrence
      if playedExploitOccurrence.step == playedExploitStep
      playedExploit <- captureDetail(playedExploitOccurrence)
      playedRecapture = changedSeed.playedRecapture
      if EvidenceRef.sameMove(playedRecapture.moveUci, playedRecaptureStep.moveUci)
      if playedExploit.legalRecaptures == List(playedRecapture)
      if sameExploit(referenceExploit, playedExploit)
      persistenceStates <- referencePersistenceStates(
        referenceLineRecord,
        referenceReplay,
        coloredPieceAtStart(referenceExploit.mover),
        referenceExploit.captured
      )
      if sameInitialPiece(rootCapture.captured, playedRecapture.movement)
      if referenceExploit.legalRecaptures.isEmpty
      absenceQuery = PositionRelationExtractor.ClosedRelationAbsenceQuery.LegalCaptureOf(
        rootCapture.captured.side,
        referenceExploit.mover.to
      )
      absenceOccurrence <- referenceReplay.positionAfter(referenceExploitStep)
      absenceAuthority <- ClosedRelationAbsenceAuthority.forQuery(
        referenceLineRecord,
        absenceOccurrence,
        absenceQuery
      )
      if rootCaptureOccurrence.certifiedSourcePremiseIds.nonEmpty
      if referenceExploitOccurrence.certifiedSourcePremiseIds.nonEmpty
      if playedExploitOccurrence.certifiedSourcePremiseIds.nonEmpty
      referenceBranch = CausalBranchOccurrence.certifiedCounterfactual(
        ComparedLineBranchRole.CounterfactualReference,
        referenceLine,
        referenceReplay,
        3
      )
      playedBranch = CausalBranchOccurrence.observedRootWithAnalyzedContinuation(
        ComparedLineBranchRole.PlayedRootAnalysisContinuation,
        playedLine,
        playedReplay,
        2
      )
    yield ExactInputs(
      rootBoard,
      rootCapture,
      removalRecapture,
      rootCaptureOccurrence,
      referenceExploit,
      referenceExploitOccurrence,
      playedExploitOccurrence,
      playedRecapture,
      referenceBranch,
      playedBranch,
      absenceAuthority,
      persistenceStates
    )

    inputs.toList.map(exact =>
      certify(
        exact,
        referenceLineRecord,
        playedLineRecord,
        referenceReplay,
        playedReplay
      )
    )

  private def certify(
      inputs: ExactInputs,
      referenceLineRecord: EvidenceRecord,
      playedLineRecord: EvidenceRecord,
      referenceReplay: CanonicalLineReplay,
      playedReplay: CanonicalLineReplay
  ): CertifiedSoleRecapturerRemovalBeforeTargetCapture =
    val proposition = SoleRecapturerRemovalBeforeTargetCaptureCausalAuthority.proposition(
      inputs.rootBoard,
      inputs.rootCapture.mover,
      inputs.rootCapture.captured,
      inputs.removalRecapture,
      inputs.referenceExploit.mover,
      inputs.referenceExploit.captured,
      inputs.playedRecapture
    )
    val causalOccurrence = CausalOccurrenceIdentity.from(
      proposition,
      List(inputs.referenceBranch, inputs.playedBranch)
    )
    val referenceRemovalUse = CausalVerticalRelationPremiseUse.from(
      SoleRecapturerRemovalBeforeTargetCapturePremiseRole.ReferenceDefenderRemoval,
      RecordBoundVerticalRelationOccurrence.certified(referenceLineRecord, inputs.rootCaptureOccurrence).getOrElse(
        throw IllegalArgumentException("defense obligation lost its graph-owned defender-removal occurrence")
      ),
      inputs.referenceBranch,
      0
    )
    val referenceExploitUse = CausalVerticalRelationPremiseUse.from(
      SoleRecapturerRemovalBeforeTargetCapturePremiseRole.ReferenceLaterExploitInventory,
      RecordBoundVerticalRelationOccurrence.certified(referenceLineRecord, inputs.referenceExploitOccurrence).getOrElse(
        throw IllegalArgumentException("defense obligation lost its graph-owned reference exploit occurrence")
      ),
      inputs.referenceBranch,
      2
    )
    val playedExploitUse = CausalVerticalRelationPremiseUse.from(
      SoleRecapturerRemovalBeforeTargetCapturePremiseRole.PlayedImmediateExploitInventory,
      RecordBoundVerticalRelationOccurrence.certified(playedLineRecord, inputs.playedExploitOccurrence).getOrElse(
        throw IllegalArgumentException("defense obligation lost its graph-owned played exploit occurrence")
      ),
      inputs.playedBranch,
      0
    )
    val absenceBinding = CausalClosedAbsenceBinding.afterStep(
      SoleRecapturerRemovalBeforeTargetCaptureAbsenceRole.ReferenceReplacementRecaptureAbsent,
      inputs.absenceAuthority,
      inputs.referenceBranch,
      2
    )
    val persistenceBindings = inputs.persistenceStates.map(state =>
      CausalClosedStateBinding.afterStep(
        state.role,
        state.authority,
        inputs.referenceBranch,
        state.stepIndex
      )
    )
    val manifest = SoleRecapturerRemovalBeforeTargetCaptureManifest.exact(
      referenceRemovalUse,
      referenceExploitUse,
      playedExploitUse,
      absenceBinding,
      coloredPieceAtStart(inputs.referenceExploit.mover),
      inputs.referenceExploit.captured,
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
      inputs.referenceExploit.mover,
      inputs.referenceExploit.captured,
      inputs.playedRecapture
    )
    val occurrence = SoleRecapturerRemovalBeforeTargetCaptureOccurrence(proofSet)
    val dependencyManifest = SoleRecapturerRemovalBeforeTargetCaptureDependencyManifest(
      referenceLineRecord,
      playedLineRecord,
      semantic.removedDefender,
      proofSet
    )
    val dependency = BoundedCausalDependencyFingerprint.from(dependencyManifest)
    CertifiedSoleRecapturerRemovalBeforeTargetCapture.from(
      semantic,
      occurrence,
      dependency,
      dependencyManifest,
      referenceLineRecord,
      playedLineRecord,
      referenceReplay,
      playedReplay,
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
      reference: RelationWitnessDetail.CaptureRecaptureInventory,
      played: RelationWitnessDetail.CaptureRecaptureInventory
  ): Boolean =
    reference.mover == played.mover && reference.captured == played.captured

  private def sameInitialPiece(
      piece: RelationColoredPieceWitness,
      movement: RelationMoveTransitionWitness
  ): Boolean =
    piece.side == movement.side && piece.square == movement.from && piece.role == movement.beforeRole

  private def coloredPieceAtStart(
      movement: RelationMoveTransitionWitness
  ): RelationColoredPieceWitness =
    RelationColoredPieceWitness(movement.from, movement.beforeRole, movement.side)

  private def referencePersistenceStates(
      referenceLineRecord: EvidenceRecord,
      replay: CanonicalLineReplay,
      exploitActor: RelationColoredPieceWitness,
      target: RelationColoredPieceWitness
  ): Option[List[PersistenceState]] =
    val expected = replay.replaySteps.take(2).zipWithIndex.flatMap { case (step, index) =>
      List(
        SoleRecapturerRemovalBeforeTargetCaptureStateRole.ReferenceExploitActorPresent -> exploitActor,
        SoleRecapturerRemovalBeforeTargetCaptureStateRole.ReferenceTargetPresent -> target
      ).flatMap { case (role, piece) =>
        ClosedPositionStateAuthority
          .atStep(referenceLineRecord, step)((occurrence, scope) =>
            occurrence.existingOccupantState(piece, scope)
          )
          .map(PersistenceState(role, index, _))
      }
    }
    Option.when(expected.size == 4)(expected)
