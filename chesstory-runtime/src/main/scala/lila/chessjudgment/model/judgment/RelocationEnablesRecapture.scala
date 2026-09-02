package lila.chessjudgment.model.judgment

import lila.chessjudgment.analysis.position.PositionRelationExtractor
import lila.chessjudgment.model.line.PrincipalVariationEvidence

private[chessjudgment] enum RelocationEnablesRecaptureBranchRole extends CausalBranchRole:
  case RelocatedResponder
  case RetainedResponder

  def stableKey: String = this match
    case RelocatedResponder => "relocated-responder"
    case RetainedResponder  => "retained-responder"

private[chessjudgment] enum RelocationEnablesRecapturePremiseRole extends CausalPremiseRole:
  case RelocatedRecaptureInventory
  case RetainedRecaptureInventory
  case RelocatedTargetCapture
  case RelocatedResponderRecapture
  case RetainedTargetCapture
  case RetainedOtherRecapture

  def stableKey: String = this match
    case RelocatedRecaptureInventory => "relocated-recapture-inventory"
    case RetainedRecaptureInventory  => "retained-recapture-inventory"
    case RelocatedTargetCapture      => "relocated-target-capture"
    case RelocatedResponderRecapture => "relocated-responder-recapture"
    case RetainedTargetCapture       => "retained-target-capture"
    case RetainedOtherRecapture      => "retained-other-recapture"

private[chessjudgment] enum RelocationEnablesRecaptureContinuityRole
    extends CausalPremiseRole,
      CausalStateRole:
  case RelocatedBranchTarget
  case RetainedBranchTarget
  case RelocatedResponder
  case RetainedResponder
  case RelocatedBranchAttacker
  case RetainedBranchAttacker

  def stableKey: String = this match
    case RelocatedBranchTarget   => "relocated-branch-target-continuity"
    case RetainedBranchTarget    => "retained-branch-target-continuity"
    case RelocatedResponder      => "relocated-responder-continuity"
    case RetainedResponder       => "retained-responder-continuity"
    case RelocatedBranchAttacker => "relocated-branch-attacker-continuity"
    case RetainedBranchAttacker  => "retained-branch-attacker-continuity"

private[chessjudgment] enum RelocationEnablesRecaptureAbsenceRole extends CausalAbsenceRole:
  case RetainedSeedRecaptureAbsent
  def stableKey: String = "retained-seed-recapture-absent"

private[chessjudgment] object RelocationEnablesRecaptureCausalAuthority:
  private final case class PropositionDescriptor(
      rootFen: String,
      relocation: RelationMoveTransitionWitness,
      targetCapture: RelationMoveTransitionWitness,
      attackerAtCommonRoot: RelationColoredPieceWitness,
      attackerAtCapture: RelationColoredPieceWitness,
      targetAtCommonRoot: RelationColoredPieceWitness,
      capturedTarget: RelationColoredPieceWitness,
      recaptureSquare: EvidenceSquare,
      relocatedResponderRecapture: RelationMoveTransitionWitness,
      retainedOtherRecapture: RelationMoveTransitionWitness
  ) extends CausalSemanticDescriptor:
    private val staging = RelationColoredPieceWitness(
      relocation.to,
      relocation.afterRole,
      relocation.side
    )
    require(
      RelationColoredPieceWitness(targetCapture.from, targetCapture.beforeRole, targetCapture.side) ==
        attackerAtCapture && attackerAtCommonRoot.side == targetCapture.side &&
        targetCapture.to == recaptureSquare && targetCapture.side != capturedTarget.side &&
        targetAtCommonRoot.side == capturedTarget.side && relocation.side == capturedTarget.side &&
        RelationColoredPieceWitness(
          relocatedResponderRecapture.from,
          relocatedResponderRecapture.beforeRole,
          relocatedResponderRecapture.side
        ) == staging && relocatedResponderRecapture.to == recaptureSquare &&
        retainedOtherRecapture.side == capturedTarget.side && retainedOtherRecapture.to == recaptureSquare,
      "relocation recapture semantics need exact attacker, target, and responder identities"
    )

    val contractKind = BoundedCausalContractKind.RelocationEnablesRecapture
    val semanticParts: List[String] = List(
      relocation.stableKey,
      targetCapture.stableKey,
      BoundedCausalIdentity.coloredPieceKey(attackerAtCommonRoot),
      BoundedCausalIdentity.coloredPieceKey(attackerAtCapture),
      BoundedCausalIdentity.coloredPieceKey(targetAtCommonRoot),
      BoundedCausalIdentity.coloredPieceKey(capturedTarget),
      recaptureSquare.key.toLowerCase,
      relocatedResponderRecapture.stableKey,
      retainedOtherRecapture.stableKey
    )

  def proposition(
      rootFen: String,
      relocation: RelationMoveTransitionWitness,
      targetCapture: RelationMoveTransitionWitness,
      attackerAtCommonRoot: RelationColoredPieceWitness,
      attackerAtCapture: RelationColoredPieceWitness,
      targetAtCommonRoot: RelationColoredPieceWitness,
      capturedTarget: RelationColoredPieceWitness,
      recaptureSquare: EvidenceSquare,
      relocatedResponderRecapture: RelationMoveTransitionWitness,
      retainedOtherRecapture: RelationMoveTransitionWitness
  ): CausalPropositionIdentity =
    CausalPropositionIdentity.from(PropositionDescriptor(
      rootFen,
      relocation,
      targetCapture,
      attackerAtCommonRoot,
      attackerAtCapture,
      targetAtCommonRoot,
      capturedTarget,
      recaptureSquare,
      relocatedResponderRecapture,
      retainedOtherRecapture
    ))

private[chessjudgment] sealed trait RelocationEnablesRecaptureManifest
    extends BoundedCausalContractManifest:
  def captureInventories: List[CausalVerticalRelationPremiseUse]
  def continuitySteps: List[CausalObjectContinuityStepUse]
  def endpointMoves: List[CausalLegalMovePremiseUse]
  def seedAbsence: CausalClosedAbsenceBinding
  def retainedStates: List[CausalClosedStateBinding]

  final def contractKind = BoundedCausalContractKind.RelocationEnablesRecapture
  final def premiseUses = captureInventories
  final override def supplementalPremiseUses = continuitySteps ::: endpointMoves
  final def absenceBindings = List(seedAbsence)
  final override def stateBindings = retainedStates
  final def stableKey: String = List(
    contractKind.semanticNamespace,
    captureInventories.map(_.stableKey).mkString("[", ",", "]"),
    continuitySteps.map(_.stableKey).mkString("[", ",", "]"),
    endpointMoves.map(_.stableKey).mkString("[", ",", "]"),
    seedAbsence.stableKey,
    retainedStates.map(_.stableKey).mkString("[", ",", "]")
  ).mkString("|")

private[chessjudgment] object RelocationEnablesRecaptureManifest:
  private final case class Exact(
      captureInventories: List[CausalVerticalRelationPremiseUse],
      continuitySteps: List[CausalObjectContinuityStepUse],
      endpointMoves: List[CausalLegalMovePremiseUse],
      seedAbsence: CausalClosedAbsenceBinding,
      retainedStates: List[CausalClosedStateBinding]
  ) extends RelocationEnablesRecaptureManifest

  def exact(
      relocatedInventory: CausalVerticalRelationPremiseUse,
      retainedInventory: CausalVerticalRelationPremiseUse,
      relocatedTarget: List[CausalObjectContinuityStepUse],
      retainedTarget: List[CausalObjectContinuityStepUse],
      relocatedResponder: List[CausalObjectContinuityStepUse],
      retainedResponder: List[CausalObjectContinuityStepUse],
      relocatedAttacker: List[CausalObjectContinuityStepUse],
      retainedAttacker: List[CausalObjectContinuityStepUse],
      relocatedCapture: CausalLegalMovePremiseUse,
      relocatedResponderRecapture: CausalLegalMovePremiseUse,
      retainedCapture: CausalLegalMovePremiseUse,
      retainedOtherRecapture: CausalLegalMovePremiseUse,
      recaptureSquare: EvidenceSquare,
      responderSeed: RelationColoredPieceWitness,
      seedAbsence: CausalClosedAbsenceBinding,
      retainedStates: List[CausalClosedStateBinding]
  ): RelocationEnablesRecaptureManifest =
    val relocatedRole = RelocationEnablesRecaptureBranchRole.RelocatedResponder
    val retainedRole = RelocationEnablesRecaptureBranchRole.RetainedResponder
    require(
      relocatedInventory.role == RelocationEnablesRecapturePremiseRole.RelocatedRecaptureInventory &&
        relocatedInventory.branchRole == relocatedRole &&
        relocatedInventory.stepIndex == relocatedCapture.stepIndex &&
        retainedInventory.role == RelocationEnablesRecapturePremiseRole.RetainedRecaptureInventory &&
        retainedInventory.branchRole == retainedRole &&
        retainedInventory.stepIndex == retainedCapture.stepIndex,
      "recapture inventories must bind the exact two capture occurrences"
    )

    exactContinuity(
      relocatedTarget,
      RelocationEnablesRecaptureContinuityRole.RelocatedBranchTarget,
      relocatedRole,
      relocatedCapture.stepIndex
    )
    exactContinuity(
      retainedTarget,
      RelocationEnablesRecaptureContinuityRole.RetainedBranchTarget,
      retainedRole,
      retainedCapture.stepIndex
    )
    exactContinuity(
      relocatedResponder,
      RelocationEnablesRecaptureContinuityRole.RelocatedResponder,
      relocatedRole,
      relocatedResponderRecapture.stepIndex
    )
    exactContinuity(
      retainedResponder,
      RelocationEnablesRecaptureContinuityRole.RetainedResponder,
      retainedRole,
      retainedOtherRecapture.stepIndex
    )
    exactContinuity(
      relocatedAttacker,
      RelocationEnablesRecaptureContinuityRole.RelocatedBranchAttacker,
      relocatedRole,
      relocatedCapture.stepIndex
    )
    exactContinuity(
      retainedAttacker,
      RelocationEnablesRecaptureContinuityRole.RetainedBranchAttacker,
      retainedRole,
      retainedCapture.stepIndex
    )
    require(
      relocatedResponder.count(_.selectedTransition.nonEmpty) == 1 &&
        retainedResponder.forall(_.selectedTransition.isEmpty),
      "the relocated-responder branch needs exactly one tracked-responder relocation before its capture"
    )

    require(
      legalRole(relocatedCapture, RelocationEnablesRecapturePremiseRole.RelocatedTargetCapture, relocatedRole) &&
        legalRole(
          relocatedResponderRecapture,
          RelocationEnablesRecapturePremiseRole.RelocatedResponderRecapture,
          relocatedRole
        ) && relocatedResponderRecapture.stepIndex == relocatedCapture.stepIndex + 1 &&
        legalRole(retainedCapture, RelocationEnablesRecapturePremiseRole.RetainedTargetCapture, retainedRole) &&
        legalRole(
          retainedOtherRecapture,
          RelocationEnablesRecapturePremiseRole.RetainedOtherRecapture,
          retainedRole
        ) && retainedOtherRecapture.stepIndex == retainedCapture.stepIndex + 1,
      "each target capture must be followed immediately by its exact certified recapture"
    )

    require(
      seedAbsence.role == RelocationEnablesRecaptureAbsenceRole.RetainedSeedRecaptureAbsent &&
        seedAbsence.branchRole == retainedRole && seedAbsence.branchId == retainedCapture.branchId &&
        seedAbsence.afterStepIndex == retainedCapture.stepIndex &&
        seedAbsence.authority.query == PositionRelationExtractor.ClosedRelationAbsenceQuery.LegalMoveFromTo(
          responderSeed.side,
          responderSeed.square,
          recaptureSquare
        ),
      "the retained branch must close the tracked seed-to-recapture-square move absence"
    )

    val continuity =
      relocatedTarget ::: retainedTarget ::: relocatedResponder ::: retainedResponder :::
        relocatedAttacker ::: retainedAttacker
    val retained = continuity.filter(_.transitionKind == ObjectContinuityStepKind.Retained)
    require(
      continuity.map(_.stableKey).distinct.size == continuity.size &&
        retainedStates.map(_.stableKey).distinct.size == retainedStates.size &&
        retainedStates.size == retained.size && retained.zip(retainedStates).forall { case (use, binding) =>
            binding.role == use.role && binding.branchId == use.branchId &&
              binding.branchRole == use.branchRole && binding.afterStepIndex == use.stepIndex &&
              binding.query == PositionRelationExtractor.ClosedPositionStateQuery.OccupiedBy(use.after)
        },
      "every retained continuity step must expose its exact occupied-state certificate once"
    )

    Exact(
      List(relocatedInventory, retainedInventory),
      continuity,
      List(relocatedCapture, relocatedResponderRecapture, retainedCapture, retainedOtherRecapture),
      seedAbsence,
      retainedStates
    )

  private def exactContinuity(
      uses: List[CausalObjectContinuityStepUse],
      role: RelocationEnablesRecaptureContinuityRole,
      branch: RelocationEnablesRecaptureBranchRole,
      untilExclusive: Int
  ): Unit =
    require(
      uses.map(_.stepIndex) == (0 until untilExclusive).toList &&
        uses.forall(use => use.role == role && use.branchRole == branch),
      "object continuity must retain every ordered branch occurrence"
    )

  private def legalRole(
      use: CausalLegalMovePremiseUse,
      role: RelocationEnablesRecapturePremiseRole,
      branch: RelocationEnablesRecaptureBranchRole
  ): Boolean = use.role == role && use.branchRole == branch

private[chessjudgment] final case class RelocationEnablesRecaptureSemanticProof private[chessjudgment] (
    identity: CausalPropositionIdentity,
    relocation: RelationMoveTransitionWitness,
    targetCapture: RelationMoveTransitionWitness,
    attackerAtCommonRoot: RelationColoredPieceWitness,
    attackerAtCapture: RelationColoredPieceWitness,
    targetAtCommonRoot: RelationColoredPieceWitness,
    capturedTarget: RelationColoredPieceWitness,
    recaptureSquare: EvidenceSquare,
    relocatedResponderRecapture: RelationMoveTransitionWitness,
    retainedOtherRecapture: RelationMoveTransitionWitness
):
  require(identity.contractKind == BoundedCausalContractKind.RelocationEnablesRecapture)
  def semanticId: String = identity.semanticId
  def rootBoardState: String = identity.rootPositionIdentity.value
  def trackedResponderAtSeed: RelationColoredPieceWitness =
    RelationColoredPieceWitness(relocation.from, relocation.beforeRole, relocation.side)
  def trackedResponderAtStaging: RelationColoredPieceWitness =
    RelationColoredPieceWitness(relocation.to, relocation.afterRole, relocation.side)
  def otherRecapturer: RelationColoredPieceWitness =
    RelationColoredPieceWitness(
      retainedOtherRecapture.from,
      retainedOtherRecapture.beforeRole,
      retainedOtherRecapture.side
    )

private[chessjudgment] final case class RelocationEnablesRecaptureOccurrence private[chessjudgment] (
    proofSet: BoundedCausalProofSet
):
  require(proofSet.proposition.contractKind == BoundedCausalContractKind.RelocationEnablesRecapture)
  require(
    proofSet.occurrence.branches.size == 2 &&
      proofSet.occurrence.branch(RelocationEnablesRecaptureBranchRole.RelocatedResponder).nonEmpty &&
      proofSet.occurrence.branch(RelocationEnablesRecaptureBranchRole.RetainedResponder).nonEmpty
  )
  require(
    proofSet.paths.size == 1,
    "one relocation-recapture occurrence has one role-unique conjunctive lower proof path"
  )
  private val manifest = proofSet.paths.head.manifest match
    case exact: RelocationEnablesRecaptureManifest => exact
    case _ => throw IllegalArgumentException("a relocation-recapture occurrence needs its exact typed manifest")

  private def endpointStepIndex(role: RelocationEnablesRecapturePremiseRole): Int =
    manifest.endpointMoves.collect { case use if use.role == role => use.stepIndex } match
      case exact :: Nil => exact
      case _ => throw IllegalArgumentException("a relocation-recapture occurrence lost one exact endpoint")

  val relocationStepIndex: Int = manifest.continuitySteps.collect {
    case use
        if use.role == RelocationEnablesRecaptureContinuityRole.RelocatedResponder &&
          use.selectedTransition.nonEmpty => use.stepIndex
  } match
    case exact :: Nil => exact
    case _ => throw IllegalArgumentException("a relocation-recapture occurrence lost its exact relocation")
  val relocatedCaptureStepIndex: Int =
    endpointStepIndex(RelocationEnablesRecapturePremiseRole.RelocatedTargetCapture)
  val retainedCaptureStepIndex: Int =
    endpointStepIndex(RelocationEnablesRecapturePremiseRole.RetainedTargetCapture)
  require(relocationStepIndex < relocatedCaptureStepIndex)

  private def branch(role: RelocationEnablesRecaptureBranchRole): CausalBranchOccurrence =
    proofSet.occurrence.branch(role).get
  def semanticId: String = proofSet.proposition.semanticId
  def occurrenceId: String = proofSet.occurrence.occurrenceId
  def relocatedBranch: CausalBranchOccurrence = branch(RelocationEnablesRecaptureBranchRole.RelocatedResponder)
  def retainedBranch: CausalBranchOccurrence = branch(RelocationEnablesRecaptureBranchRole.RetainedResponder)
  def relocatedLine = relocatedBranch.line
  def retainedLine = retainedBranch.line
  def relocatedRecaptureStepIndex: Int = relocatedCaptureStepIndex + 1
  def retainedRecaptureStepIndex: Int = retainedCaptureStepIndex + 1
  def proofPaths: List[CausalProofPathOccurrence] = proofSet.paths

private[chessjudgment] final case class RelocationEnablesRecaptureDependencyManifest private[chessjudgment] (
    subject: CertifiedRootOccurrence,
    relocated: CertifiedRootOccurrence,
    retained: CertifiedRootOccurrence,
    continuityKeys: List[String],
    proofSet: BoundedCausalProofSet
) extends BoundedCausalDependencyManifest:
  val contractKind = BoundedCausalContractKind.RelocationEnablesRecapture
  require(
    continuityKeys.size == 6 && continuityKeys.forall(_.nonEmpty) &&
      continuityKeys.distinct.size == continuityKeys.size,
    "relocation recapture needs all six exact object-continuity dependencies"
  )
  val stableKey: String = List(
    BoundedCausalIdentity.evidenceRecordKey(subject.lineOwner),
    BoundedCausalIdentity.evidenceRecordKey(subject.transitionOwner),
    BoundedCausalIdentity.evidenceRecordKey(relocated.lineOwner),
    BoundedCausalIdentity.evidenceRecordKey(relocated.transitionOwner),
    BoundedCausalIdentity.evidenceRecordKey(retained.lineOwner),
    BoundedCausalIdentity.evidenceRecordKey(retained.transitionOwner),
    continuityKeys.mkString("continuities[", ",", "]"),
    proofSet.proposition.semanticId,
    proofSet.occurrence.occurrenceId,
    proofSet.paths.map(_.pathOccurrenceId).mkString("paths[", ",", "]")
  ).mkString("|")

private[chessjudgment] final case class RelocationEnablesRecaptureDemand private[chessjudgment] (
    relocatedCapture: ReplayLegalMoveOccurrence,
    relocatedInventory: ReplayVerticalRelationOccurrence,
    relocatedResponderRecaptureResource: RelationLegalMoveResourceWitness,
    relocatedCaptureStepIndex: Int,
    relocatedResponderRecapture: ReplayLegalMoveOccurrence,
    retainedCapture: ReplayLegalMoveOccurrence,
    retainedInventory: ReplayVerticalRelationOccurrence,
    retainedOtherRecaptureResource: RelationLegalMoveResourceWitness,
    retainedCaptureStepIndex: Int,
    retainedOtherRecapture: ReplayLegalMoveOccurrence
):
  def stableKey: String = List(
    relocatedCapture.occurrenceId,
    relocatedInventory.occurrenceId,
    relocatedResponderRecaptureResource.stableKey,
    relocatedCaptureStepIndex.toString,
    relocatedResponderRecapture.occurrenceId,
    retainedCapture.occurrenceId,
    retainedInventory.occurrenceId,
    retainedOtherRecaptureResource.stableKey,
    retainedCaptureStepIndex.toString,
    retainedOtherRecapture.occurrenceId
  ).mkString("|")

private[chessjudgment] object RelocationEnablesRecaptureDemand:
  private[chessjudgment] final case class CaptureWithReply(
      capture: ReplayLegalMoveOccurrence,
      inventory: ReplayVerticalRelationOccurrence,
      resource: RelationLegalMoveResourceWitness,
      captureIndex: Int,
      recapture: ReplayLegalMoveOccurrence
  )

  private[chessjudgment] def fromCatalog(
      relocated: List[CaptureWithReply],
      retained: List[CaptureWithReply]
  ): List[RelocationEnablesRecaptureDemand] =
    val exact = for
      relocatedReply <- relocated
      retainedReply <- retained
      if ReplayLegalMoveOccurrence.sameMove(relocatedReply.capture, retainedReply.capture)
    yield RelocationEnablesRecaptureDemand(
      relocatedReply.capture,
      relocatedReply.inventory,
      relocatedReply.resource,
      relocatedReply.captureIndex,
      relocatedReply.recapture,
      retainedReply.capture,
      retainedReply.inventory,
      retainedReply.resource,
      retainedReply.captureIndex,
      retainedReply.recapture
    )
    val ordered = exact.sortBy(_.stableKey)
    require(
      ordered.map(_.stableKey).distinct.size == ordered.size,
      "one pair of admitted replays cannot issue duplicate relocation-recapture demands"
    )
    ordered

  private[chessjudgment] def capturesWithImmediateReply(
      replay: CanonicalLineReplay
  ): List[CaptureWithReply] =
    replay.transitionOccurrencesFor(replay.replaySteps).toList.flatMap(transitions =>
      transitions.zip(transitions.drop(1)).zipWithIndex.flatMap {
        case ((captureTransition, recaptureTransition), index) =>
          val capture = captureTransition.legalMoveOccurrence
          val recapture = recaptureTransition.legalMoveOccurrence
          for
            _ <- Option.when(capture.movement.capture.nonEmpty)(()).toList
            (inventory, resource) <- replay
              .exactRecaptureOccurrenceMembership(captureTransition, recaptureTransition)
              .toList
          yield CaptureWithReply(capture, inventory, resource, index, recapture)
      }
    )

/** Family-private binding of the canonical replay's exact immediate-recapture
  * membership. It retains the one issued L1 inventory occurrence and selected
  * resource without introducing another recapture predicate.
  */
private final case class RelocationRecaptureAuthority(
    capture: RecordBoundLegalMoveOccurrence,
    recapture: RecordBoundLegalMoveOccurrence,
    inventory: RecordBoundVerticalRelationOccurrence,
    resource: RelationLegalMoveResourceWitness
):
  require(
    capture.issuerRecord == recapture.issuerRecord &&
      capture.issuerRecord == inventory.issuerRecord,
    "a relocation-recapture binding needs one exact LegalLine owner"
  )
  def remainsCertified: Boolean =
    RelocationRecaptureAuthority.certified(capture, recapture).contains(this)

private object RelocationRecaptureAuthority:
  def certified(
      capture: RecordBoundLegalMoveOccurrence,
      recapture: RecordBoundLegalMoveOccurrence
  ): Option[RelocationRecaptureAuthority] =
    for
      _ <- Option.when(capture.issuerRecord == recapture.issuerRecord)(())
      lineRecord <- CertifiedLineReplayRecord.from(capture.issuerRecord)
      exactCapture <- RecordBoundLegalMoveOccurrence.certifiedBy(lineRecord, capture.occurrence)
      if exactCapture == capture
      exactRecapture <- RecordBoundLegalMoveOccurrence.certifiedBy(lineRecord, recapture.occurrence)
      if exactRecapture == recapture
      (occurrence, resource) <- lineRecord.replay
        .exactRecaptureOccurrenceMembership(capture.step, recapture.step)
      inventory <- RecordBoundVerticalRelationOccurrence.certified(capture.issuerRecord, occurrence)
    yield RelocationRecaptureAuthority(capture, recapture, inventory, resource)

private[chessjudgment] object RelocationEnablesRecaptureProof:
  private final case class ExactInputs(
      rootBoard: String,
      subject: CertifiedRootOccurrence,
      relocated: CertifiedRootOccurrence,
      retained: CertifiedRootOccurrence,
      relocatedBranch: CausalBranchOccurrence,
      retainedBranch: CausalBranchOccurrence,
      relocatedCapture: RecordBoundLegalMoveOccurrence,
      relocatedResponderRecapture: RecordBoundLegalMoveOccurrence,
      retainedCapture: RecordBoundLegalMoveOccurrence,
      retainedOtherRecapture: RecordBoundLegalMoveOccurrence,
      capturedTarget: RelationColoredPieceWitness,
      relocatedRecaptureAuthority: RelocationRecaptureAuthority,
      retainedRecaptureAuthority: RelocationRecaptureAuthority,
      relocatedTarget: RecordBoundObjectContinuity,
      retainedTarget: RecordBoundObjectContinuity,
      relocatedResponder: RecordBoundObjectContinuity,
      retainedResponder: RecordBoundObjectContinuity,
      relocatedAttacker: RecordBoundObjectContinuity,
      retainedAttacker: RecordBoundObjectContinuity,
      relocation: RecordBoundObjectContinuityStep,
      seedAbsence: ClosedRelationAbsenceAuthority,
      demand: RelocationEnablesRecaptureDemand
  )

  def certifyDemanded(
      subject: CertifiedRootOccurrence,
      relocated: CertifiedRootOccurrence,
      retained: CertifiedRootOccurrence,
      demands: List[RelocationEnablesRecaptureDemand]
  ): List[CertifiedRelocationEnablesRecapture] =
    val exact = demands.flatMap(demand => exactInputs(subject, relocated, retained, demand).map(certify))
    val keys = exact.map(value => value.semantic.semanticId -> value.occurrence.occurrenceId)
    require(keys.distinct.size == keys.size, "one exact relocation-recapture proof may be produced only once")
    exact.sortBy(value => value.semantic.semanticId -> value.occurrence.occurrenceId)

  private def exactInputs(
      subject: CertifiedRootOccurrence,
      relocated: CertifiedRootOccurrence,
      retained: CertifiedRootOccurrence,
      demand: RelocationEnablesRecaptureDemand
  ): Option[ExactInputs] =
    for
      _ <- Option.when(
        subject.remainsCertified && relocated.remainsCertified && retained.remainsCertified && subject.isObserved &&
          (subject == relocated || subject == retained) && relocated.line != retained.line &&
          relocated.transition.from.ply == retained.transition.from.ply &&
          PrincipalVariationEvidence.sameBoardState(relocated.transition.from.fen, retained.transition.from.fen)
      )((): Unit)
      rootBoard <- relocated.replay.replaySteps.headOption.flatMap(step =>
        PrincipalVariationEvidence.semanticBoardStateFen(step.fenBefore)
      )
      retainedBoard <- retained.replay.replaySteps.headOption.flatMap(step =>
        PrincipalVariationEvidence.semanticBoardStateFen(step.fenBefore)
      )
      if rootBoard == retainedBoard
      relocatedCapture <- RecordBoundLegalMoveOccurrence.atIndex(
        relocated.lineOwner,
        demand.relocatedCaptureStepIndex,
        demand.relocatedCapture
      )
      relocatedResponderRecapture <- RecordBoundLegalMoveOccurrence.atIndex(
        relocated.lineOwner,
        demand.relocatedCaptureStepIndex + 1,
        demand.relocatedResponderRecapture
      )
      retainedCapture <- RecordBoundLegalMoveOccurrence.atIndex(
        retained.lineOwner,
        demand.retainedCaptureStepIndex,
        demand.retainedCapture
      )
      retainedOtherRecapture <- RecordBoundLegalMoveOccurrence.atIndex(
        retained.lineOwner,
        demand.retainedCaptureStepIndex + 1,
        demand.retainedOtherRecapture
      )
      if RecordBoundLegalMoveOccurrence.sameMove(relocatedCapture, retainedCapture)
      capturedTarget <- RecordBoundLegalMoveOccurrence.capturedTarget(relocatedCapture)
      if RecordBoundLegalMoveOccurrence.capturedTarget(retainedCapture).contains(capturedTarget)
      relocatedRecaptureAuthority <- RelocationRecaptureAuthority.certified(relocatedCapture, relocatedResponderRecapture)
      if relocatedRecaptureAuthority.inventory.occurrence == demand.relocatedInventory &&
        relocatedRecaptureAuthority.resource == demand.relocatedResponderRecaptureResource
      retainedRecaptureAuthority <- RelocationRecaptureAuthority.certified(retainedCapture, retainedOtherRecapture)
      if retainedRecaptureAuthority.inventory.occurrence == demand.retainedInventory &&
        retainedRecaptureAuthority.resource == demand.retainedOtherRecaptureResource
      relocatedBranch = CausalBranchOccurrence.fromRootOccurrence(
        RelocationEnablesRecaptureBranchRole.RelocatedResponder,
        relocated,
        demand.relocatedCaptureStepIndex + 2
      )
      retainedBranch = CausalBranchOccurrence.fromRootOccurrence(
        RelocationEnablesRecaptureBranchRole.RetainedResponder,
        retained,
        demand.retainedCaptureStepIndex + 2
      )
      attackerAtCapture = RelationColoredPieceWitness(
        relocatedCapture.movement.from,
        relocatedCapture.movement.beforeRole,
        relocatedCapture.movement.side
      )
      if RelationColoredPieceWitness(
        retainedCapture.movement.from,
        retainedCapture.movement.beforeRole,
        retainedCapture.movement.side
      ) == attackerAtCapture
      relocatedContinuities <- RecordBoundObjectContinuity.endingAt(
        relocated.lineOwner,
        relocatedBranch,
        List(
          demand.relocatedCaptureStepIndex -> capturedTarget,
          demand.relocatedCaptureStepIndex -> attackerAtCapture,
          (demand.relocatedCaptureStepIndex + 1) -> RelationColoredPieceWitness(
            relocatedResponderRecapture.movement.from,
            relocatedResponderRecapture.movement.beforeRole,
            relocatedResponderRecapture.movement.side
          )
        )
      )
      (relocatedTarget, relocatedAttacker, relocatedResponder) <- relocatedContinuities match
        case List(target, attacker, responder) => Some((target, attacker, responder))
        case _                                 => None
      relocation <- relocatedResponder.movementSteps match
        case exact :: Nil if exact.stepIndex < demand.relocatedCaptureStepIndex => Some(exact)
        case _ => None
      relocationMove <- relocation.selectedTransition
      responderSeed = relocatedResponder.rootPiece
      retainedContinuities <- RecordBoundObjectContinuity.endingAt(
        retained.lineOwner,
        retainedBranch,
        List(
          demand.retainedCaptureStepIndex -> capturedTarget,
          demand.retainedCaptureStepIndex -> attackerAtCapture,
          (demand.retainedCaptureStepIndex + 1) -> responderSeed
        )
      )
      (retainedTarget, retainedAttacker, retainedResponder) <- retainedContinuities match
        case List(target, attacker, responder) => Some((target, attacker, responder))
        case _                                 => None
      if relocatedTarget.rootPiece == retainedTarget.rootPiece
      if relocatedAttacker.rootPiece == retainedAttacker.rootPiece
      if retainedResponder.rootPiece == responderSeed &&
        retainedResponder.endpointPiece == responderSeed && retainedResponder.movementSteps.isEmpty
      if RelationColoredPieceWitness(
        retainedOtherRecapture.movement.from,
        retainedOtherRecapture.movement.beforeRole,
        retainedOtherRecapture.movement.side
      ) != responderSeed
      if RelationColoredPieceWitness(
        relocatedResponderRecapture.movement.from,
        relocatedResponderRecapture.movement.beforeRole,
        relocatedResponderRecapture.movement.side
      ) == RelationColoredPieceWitness(
        relocationMove.to,
        relocationMove.afterRole,
        relocationMove.side
      )
      absenceOccurrence <- retained.replay.positionAfter(retainedCapture.step)
      seedAbsence <- ClosedRelationAbsenceAuthority.forQuery(
        retained.lineOwner,
        absenceOccurrence,
        PositionRelationExtractor.ClosedRelationAbsenceQuery.LegalMoveFromTo(
          responderSeed.side,
          responderSeed.square,
          retainedCapture.movement.to
        )
      )
    yield ExactInputs(
      rootBoard,
      subject,
      relocated,
      retained,
      relocatedBranch,
      retainedBranch,
      relocatedCapture,
      relocatedResponderRecapture,
      retainedCapture,
      retainedOtherRecapture,
      capturedTarget,
      relocatedRecaptureAuthority,
      retainedRecaptureAuthority,
      relocatedTarget,
      retainedTarget,
      relocatedResponder,
      retainedResponder,
      relocatedAttacker,
      retainedAttacker,
      relocation,
      seedAbsence,
      demand
    )

  private def certify(input: ExactInputs): CertifiedRelocationEnablesRecapture =
    val relocationMove = input.relocation.selectedTransition.get
    val continuities: List[(
      RelocationEnablesRecaptureContinuityRole,
      CausalBranchOccurrence,
      RecordBoundObjectContinuity
    )] = List(
      (
        RelocationEnablesRecaptureContinuityRole.RelocatedBranchTarget,
        input.relocatedBranch,
        input.relocatedTarget
      ),
      (
        RelocationEnablesRecaptureContinuityRole.RetainedBranchTarget,
        input.retainedBranch,
        input.retainedTarget
      ),
      (
        RelocationEnablesRecaptureContinuityRole.RelocatedResponder,
        input.relocatedBranch,
        input.relocatedResponder
      ),
      (
        RelocationEnablesRecaptureContinuityRole.RetainedResponder,
        input.retainedBranch,
        input.retainedResponder
      ),
      (
        RelocationEnablesRecaptureContinuityRole.RelocatedBranchAttacker,
        input.relocatedBranch,
        input.relocatedAttacker
      ),
      (
        RelocationEnablesRecaptureContinuityRole.RetainedBranchAttacker,
        input.retainedBranch,
        input.retainedAttacker
      )
    )
    val proposition = RelocationEnablesRecaptureCausalAuthority.proposition(
      input.rootBoard,
      relocationMove,
      input.relocatedCapture.movement,
      input.relocatedAttacker.rootPiece,
      input.relocatedAttacker.endpointPiece,
      input.relocatedTarget.rootPiece,
      input.capturedTarget,
      input.relocatedCapture.movement.to,
      input.relocatedResponderRecapture.movement,
      input.retainedOtherRecapture.movement
    )
    val causalOccurrence = CausalOccurrenceIdentity.from(
      proposition,
      List(input.relocatedBranch, input.retainedBranch)
    )
    val relationAuthorities = List(
      CausalVerticalRelationPremiseUse.from(
        RelocationEnablesRecapturePremiseRole.RelocatedRecaptureInventory,
        input.relocatedRecaptureAuthority.inventory,
        input.relocatedBranch,
        input.demand.relocatedCaptureStepIndex
      ) -> input.relocatedRecaptureAuthority.inventory,
      CausalVerticalRelationPremiseUse.from(
        RelocationEnablesRecapturePremiseRole.RetainedRecaptureInventory,
        input.retainedRecaptureAuthority.inventory,
        input.retainedBranch,
        input.demand.retainedCaptureStepIndex
      ) -> input.retainedRecaptureAuthority.inventory
    )
    val continuityAuthorities = continuities.flatMap { case (role, _, authority) =>
      CausalObjectContinuityStepUse.from(role, authority).zip(authority.steps)
    }
    val stateBindings = continuities.flatMap { case (role, _, authority) =>
      CausalClosedStateBinding.fromContinuity(role, authority).map(binding => binding -> binding.authority)
    }
    def continuityUses(role: RelocationEnablesRecaptureContinuityRole): List[CausalObjectContinuityStepUse] =
      continuityAuthorities.collect { case (use, _) if use.role == role => use }
    val endpointAuthorities = List(
      CausalLegalMovePremiseUse.from(
        RelocationEnablesRecapturePremiseRole.RelocatedTargetCapture,
        input.relocatedCapture,
        input.relocatedBranch,
        input.demand.relocatedCaptureStepIndex
      ) -> input.relocatedCapture,
      CausalLegalMovePremiseUse.from(
        RelocationEnablesRecapturePremiseRole.RelocatedResponderRecapture,
        input.relocatedResponderRecapture,
        input.relocatedBranch,
        input.demand.relocatedCaptureStepIndex + 1
      ) -> input.relocatedResponderRecapture,
      CausalLegalMovePremiseUse.from(
        RelocationEnablesRecapturePremiseRole.RetainedTargetCapture,
        input.retainedCapture,
        input.retainedBranch,
        input.demand.retainedCaptureStepIndex
      ) -> input.retainedCapture,
      CausalLegalMovePremiseUse.from(
        RelocationEnablesRecapturePremiseRole.RetainedOtherRecapture,
        input.retainedOtherRecapture,
        input.retainedBranch,
        input.demand.retainedCaptureStepIndex + 1
      ) -> input.retainedOtherRecapture
    )
    val absenceBinding = CausalClosedAbsenceBinding.afterStep(
      RelocationEnablesRecaptureAbsenceRole.RetainedSeedRecaptureAbsent,
      input.seedAbsence,
      input.retainedBranch,
      input.demand.retainedCaptureStepIndex
    )
    val manifest = RelocationEnablesRecaptureManifest.exact(
      relationAuthorities(0)._1,
      relationAuthorities(1)._1,
      continuityUses(RelocationEnablesRecaptureContinuityRole.RelocatedBranchTarget),
      continuityUses(RelocationEnablesRecaptureContinuityRole.RetainedBranchTarget),
      continuityUses(RelocationEnablesRecaptureContinuityRole.RelocatedResponder),
      continuityUses(RelocationEnablesRecaptureContinuityRole.RetainedResponder),
      continuityUses(RelocationEnablesRecaptureContinuityRole.RelocatedBranchAttacker),
      continuityUses(RelocationEnablesRecaptureContinuityRole.RetainedBranchAttacker),
      endpointAuthorities(0)._1,
      endpointAuthorities(1)._1,
      endpointAuthorities(2)._1,
      endpointAuthorities(3)._1,
      input.relocatedCapture.movement.to,
      input.relocatedResponder.rootPiece,
      absenceBinding,
      stateBindings.map(_._1)
    )
    val path = CausalProofPathOccurrence.from(proposition, manifest)
    val proofSet = BoundedCausalProofSet.from(proposition, causalOccurrence, List(path))
    val semantic = RelocationEnablesRecaptureSemanticProof(
      proposition,
      relocationMove,
      input.relocatedCapture.movement,
      input.relocatedAttacker.rootPiece,
      input.relocatedAttacker.endpointPiece,
      input.relocatedTarget.rootPiece,
      input.capturedTarget,
      input.relocatedCapture.movement.to,
      input.relocatedResponderRecapture.movement,
      input.retainedOtherRecapture.movement
    )
    val occurrence = RelocationEnablesRecaptureOccurrence(proofSet)
    val dependencyManifest = RelocationEnablesRecaptureDependencyManifest(
      input.subject,
      input.relocated,
      input.retained,
      continuities.map { case (role, branch, authority) =>
        s"${role.stableKey}|${branch.branchId}|${authority.stableKey}"
      },
      proofSet
    )
    CertifiedRelocationEnablesRecapture.from(
      semantic,
      occurrence,
      BoundedCausalDependencyFingerprint.from(dependencyManifest),
      dependencyManifest,
      input.subject,
      input.relocated,
      input.retained,
      continuities,
      input.relocatedRecaptureAuthority,
      input.retainedRecaptureAuthority,
      relationAuthorities,
      continuityAuthorities,
      endpointAuthorities,
      List(path.closedAbsenceUses.head -> input.seedAbsence),
      path.closedStateUses.zip(stateBindings.map(_._2))
    )

private[chessjudgment] final class CertifiedRelocationEnablesRecapture private (
    val semantic: RelocationEnablesRecaptureSemanticProof,
    val occurrence: RelocationEnablesRecaptureOccurrence,
    val dependency: BoundedCausalDependencyFingerprint,
    private val dependencyManifest: RelocationEnablesRecaptureDependencyManifest,
    val subject: CertifiedRootOccurrence,
    private val relocated: CertifiedRootOccurrence,
    private val retained: CertifiedRootOccurrence,
    private val continuities: List[(
      RelocationEnablesRecaptureContinuityRole,
      CausalBranchOccurrence,
      RecordBoundObjectContinuity
    )],
    private val relocatedRecaptureAuthority: RelocationRecaptureAuthority,
    private val retainedRecaptureAuthority: RelocationRecaptureAuthority,
    private val relationAuthorities: List[(CausalVerticalRelationPremiseUse, RecordBoundVerticalRelationOccurrence)],
    private val continuityAuthorities: List[(CausalObjectContinuityStepUse, RecordBoundObjectContinuityStep)],
    private val endpointAuthorities: List[(CausalLegalMovePremiseUse, RecordBoundLegalMoveOccurrence)],
    private val absenceAuthorities: List[(CausalClosedAbsenceUse, ClosedRelationAbsenceAuthority)],
    private val stateAuthorities: List[(CausalClosedStateUse, ClosedPositionStateAuthority)]
):
  require(
    semantic.identity == occurrence.proofSet.proposition && dependencyManifest.proofSet == occurrence.proofSet &&
      dependencyManifest.continuityKeys == continuityKeys &&
      BoundedCausalDependencyFingerprint.from(dependencyManifest) == dependency
  )
  require(
    occurrence.proofPaths.flatMap(_.premiseUses) == relationAuthorities.map(_._1) &&
      occurrence.proofPaths.flatMap(_.manifest.supplementalPremiseUses) ==
        continuityAuthorities.map(_._1) ::: endpointAuthorities.map(_._1) &&
      occurrence.proofPaths.flatMap(_.closedAbsenceUses) == absenceAuthorities.map(_._1) &&
      occurrence.proofPaths.flatMap(_.closedStateUses) == stateAuthorities.map(_._1)
  )

  def parentSources: List[EvidenceRef] =
    val sources = List(
      relocated.lineOwner.ref,
      relocated.transitionOwner.ref,
      retained.lineOwner.ref,
      retained.transitionOwner.ref
    ).sortBy(_.id)
    require(sources.map(_.id).distinct.size == sources.size)
    sources

  private[chessjudgment] def lowerIssuerRecords: List[EvidenceRecord] =
    List(relocated.lineOwner, relocated.transitionOwner, retained.lineOwner, retained.transitionOwner)

  def proves(record: EvidenceRecord, payload: RelocationEnablesRecaptureEvidence): Boolean =
    record.ref.producer == EvidenceProducer.CausalProofProducer &&
      record.ref.layer == EvidenceLayer.CausalProof &&
      record.ref.confidence == EvidenceConfidence.LegalReplayVerified &&
      record.ref.position == subject.transition.from && record.ref.line.contains(subject.line) &&
      record.ref.scope == subject.transitionOwner.ref.scope && record.parents == parentSources &&
      payload.semantic == semantic && payload.occurrence == occurrence &&
      payload.dependencyFingerprint == dependency.value &&
      payload.subjectOccurrence == subject.publicOccurrence && payload.occurrenceProof == this &&
      remainsCertified

  def remainsCertified: Boolean =
    subject.remainsCertified && relocated.remainsCertified && retained.remainsCertified &&
      subject.isObserved && (subject == relocated || subject == retained) &&
      continuitiesRemainCertified &&
      relocatedRecaptureAuthority.remainsCertified && retainedRecaptureAuthority.remainsCertified &&
      relationUsesRemainCertified && continuityUsesRemainCertified && endpointUsesRemainCertified &&
      absencesRemainCertified && statesRemainCertified && semanticRemainsCertified &&
      BoundedCausalDependencyFingerprint.from(dependencyManifest) == dependency

  private def continuitiesRemainCertified: Boolean =
    val relocatedContinuities = continuities.collect {
      case (_, branch, authority) if branch == occurrence.relocatedBranch => authority
    }
    val retainedContinuities = continuities.collect {
      case (_, branch, authority) if branch == occurrence.retainedBranch => authority
    }
    relocatedContinuities.size == 3 && retainedContinuities.size == 3 &&
      RecordBoundObjectContinuity.allRemainCertified(
        relocated.lineOwner,
        occurrence.relocatedBranch,
        relocatedContinuities
      ) && RecordBoundObjectContinuity.allRemainCertified(
        retained.lineOwner,
        occurrence.retainedBranch,
        retainedContinuities
      )

  private def relationUsesRemainCertified: Boolean = relationAuthorities.forall { case (use, authority) =>
    ownerFor(use.branchRole).exists { case (_, branch) =>
      authority.remainsCertified && branch.branchId == use.branchId &&
        CausalVerticalRelationPremiseUse.from(use.role, authority, branch, use.stepIndex) == use
    }
  }

  private def continuityUsesRemainCertified: Boolean =
    continuityAuthorities == continuities.flatMap { case (role, _, authority) =>
      CausalObjectContinuityStepUse.from(role, authority).zip(authority.steps)
    }

  private def endpointUsesRemainCertified: Boolean = endpointAuthorities.forall { case (use, authority) =>
    ownerFor(use.branchRole).exists { case (_, branch) =>
      authority.remainsCertified && CausalLegalMovePremiseUse.from(use.role, authority, branch, use.stepIndex) == use
    }
  }

  private def absencesRemainCertified: Boolean = absenceAuthorities match
    case List((use, authority)) =>
      val binding = use.binding
      authority.issuerRecord == retained.lineOwner && authority.remainsCertified && binding.authority == authority &&
        occurrence.retainedBranch.stepAt(binding.afterStepIndex).exists(step =>
          step.step == authority.step && step.line == authority.issuerLine
        )
    case _ => false

  private def statesRemainCertified: Boolean =
    stateAuthorities == occurrence.proofPaths.flatMap(_.closedStateUses).zip(
      continuities.flatMap { case (_, _, authority) =>
        authority.retainedSteps.flatMap(_.retainedState)
      }
    ) && stateAuthorities.map(_._1.binding) == continuities.flatMap { case (role, _, authority) =>
      CausalClosedStateBinding.fromContinuity(role, authority)
    }

  private def semanticRemainsCertified: Boolean =
    val relocatedTarget = exactContinuity(RelocationEnablesRecaptureContinuityRole.RelocatedBranchTarget)
    val retainedTarget = exactContinuity(RelocationEnablesRecaptureContinuityRole.RetainedBranchTarget)
    val relocatedResponder = exactContinuity(RelocationEnablesRecaptureContinuityRole.RelocatedResponder)
    val retainedResponder = exactContinuity(RelocationEnablesRecaptureContinuityRole.RetainedResponder)
    val relocatedAttacker = exactContinuity(RelocationEnablesRecaptureContinuityRole.RelocatedBranchAttacker)
    val retainedAttacker = exactContinuity(RelocationEnablesRecaptureContinuityRole.RetainedBranchAttacker)
    val relocation = relocatedResponder.movementSteps
    val relocatedCaptures = exactEndpoint(RelocationEnablesRecapturePremiseRole.RelocatedTargetCapture)
    val retainedCaptures = exactEndpoint(RelocationEnablesRecapturePremiseRole.RetainedTargetCapture)
    val relocatedResponderRecaptures = exactEndpoint(RelocationEnablesRecapturePremiseRole.RelocatedResponderRecapture)
    val retainedOtherRecaptures = exactEndpoint(RelocationEnablesRecapturePremiseRole.RetainedOtherRecapture)
    (relocation, relocatedCaptures, retainedCaptures, relocatedResponderRecaptures, retainedOtherRecaptures) match
      case (exact :: Nil, Some(relocatedCapture), Some(retainedCapture), Some(relocatedResponderRecapture),
            Some(retainedOtherRecapture)) =>
        val relocationMove = exact.selectedTransition.get
        relocatedTarget.rootPiece == retainedTarget.rootPiece &&
          relocatedAttacker.rootPiece == retainedAttacker.rootPiece &&
          relocatedResponder.rootPiece == retainedResponder.rootPiece &&
          retainedResponder.movementSteps.isEmpty &&
          relocatedCapture.movement == semantic.targetCapture &&
          retainedCapture.movement == semantic.targetCapture &&
          RecordBoundLegalMoveOccurrence.capturedTarget(relocatedCapture).contains(semantic.capturedTarget) &&
          RecordBoundLegalMoveOccurrence.capturedTarget(retainedCapture).contains(semantic.capturedTarget) &&
          RecordBoundLegalMoveOccurrence.sameMove(relocatedCapture, retainedCapture) &&
          relocatedResponderRecapture.movement == semantic.relocatedResponderRecapture &&
          retainedOtherRecapture.movement == semantic.retainedOtherRecapture &&
          semantic.relocation == relocationMove &&
          semantic.attackerAtCommonRoot == relocatedAttacker.rootPiece &&
          semantic.attackerAtCapture == relocatedAttacker.endpointPiece &&
          semantic.targetAtCommonRoot == relocatedTarget.rootPiece &&
          RelocationEnablesRecaptureCausalAuthority.proposition(
            semantic.rootBoardState,
            semantic.relocation,
            semantic.targetCapture,
            semantic.attackerAtCommonRoot,
            semantic.attackerAtCapture,
            semantic.targetAtCommonRoot,
            semantic.capturedTarget,
            semantic.recaptureSquare,
            semantic.relocatedResponderRecapture,
            semantic.retainedOtherRecapture
          ) == semantic.identity
      case _ => false

  private def exactContinuity(
      role: RelocationEnablesRecaptureContinuityRole
  ): RecordBoundObjectContinuity = continuities.collect {
    case (exactRole, _, authority) if exactRole == role => authority
  } match
    case exact :: Nil => exact
    case _ => throw IllegalStateException("a certified relocation proof lost one exact continuity")

  private def continuityKeys: List[String] = continuities.map {
    case (role, branch, authority) =>
      s"${role.stableKey}|${branch.branchId}|${authority.stableKey}"
  }

  private def exactEndpoint(
      role: RelocationEnablesRecapturePremiseRole
  ): Option[RecordBoundLegalMoveOccurrence] =
    endpointAuthorities.collect { case (use, authority) if use.role == role => authority } match
      case exact :: Nil => Some(exact)
      case _            => None

  private def ownerFor(
      role: CausalBranchRole
  ): Option[(CertifiedRootOccurrence, CausalBranchOccurrence)] = role match
    case RelocationEnablesRecaptureBranchRole.RelocatedResponder =>
      Some(relocated -> occurrence.relocatedBranch)
    case RelocationEnablesRecaptureBranchRole.RetainedResponder =>
      Some(retained -> occurrence.retainedBranch)
    case _ => None

private[chessjudgment] object CertifiedRelocationEnablesRecapture:
  def from(
      semantic: RelocationEnablesRecaptureSemanticProof,
      occurrence: RelocationEnablesRecaptureOccurrence,
      dependency: BoundedCausalDependencyFingerprint,
      dependencyManifest: RelocationEnablesRecaptureDependencyManifest,
      subject: CertifiedRootOccurrence,
      relocated: CertifiedRootOccurrence,
      retained: CertifiedRootOccurrence,
      continuities: List[(
        RelocationEnablesRecaptureContinuityRole,
        CausalBranchOccurrence,
        RecordBoundObjectContinuity
      )],
      relocatedRecaptureAuthority: RelocationRecaptureAuthority,
      retainedRecaptureAuthority: RelocationRecaptureAuthority,
      relationAuthorities: List[(CausalVerticalRelationPremiseUse, RecordBoundVerticalRelationOccurrence)],
      continuityAuthorities: List[(CausalObjectContinuityStepUse, RecordBoundObjectContinuityStep)],
      endpointAuthorities: List[(CausalLegalMovePremiseUse, RecordBoundLegalMoveOccurrence)],
      absenceAuthorities: List[(CausalClosedAbsenceUse, ClosedRelationAbsenceAuthority)],
      stateAuthorities: List[(CausalClosedStateUse, ClosedPositionStateAuthority)]
  ): CertifiedRelocationEnablesRecapture =
    val exact = new CertifiedRelocationEnablesRecapture(
      semantic,
      occurrence,
      dependency,
      dependencyManifest,
      subject,
      relocated,
      retained,
      continuities,
      relocatedRecaptureAuthority,
      retainedRecaptureAuthority,
      relationAuthorities,
      continuityAuthorities,
      endpointAuthorities,
      absenceAuthorities,
      stateAuthorities
    )
    require(exact.remainsCertified)
    exact
