package lila.chessjudgment.model.judgment

import lila.chessjudgment.analysis.position.PositionRelationExtractor
import lila.chessjudgment.model.line.PrincipalVariationEvidence

private[chessjudgment] enum CaptureExclusionBranchRole extends CausalBranchRole:
  case VacatingThenDeferred
  case ImmediateDeferredCapture

  def stableKey: String =
    this match
      case VacatingThenDeferred     => "vacating-then-deferred"
      case ImmediateDeferredCapture => "immediate-deferred-capture"

private[chessjudgment] enum CaptureExclusionMoveOrderPremiseRole extends CausalPremiseRole:
  case VacatingMove
  case ImmediateDeferredMove
  case ImmediateCaptureReply
  case LaterDeferredMove

  def stableKey: String =
    this match
      case VacatingMove          => "vacating-move"
      case ImmediateDeferredMove => "immediate-deferred-move"
      case ImmediateCaptureReply => "immediate-capture-reply"
      case LaterDeferredMove     => "later-deferred-move"

private[chessjudgment] enum CaptureExclusionMoveOrderAbsenceRole extends CausalAbsenceRole:
  case CaptureReplyAbsent

  def stableKey: String = "capture-reply-absent"

private[chessjudgment] enum CaptureExclusionMoveOrderStateRole extends CausalStateRole:
  case VacatedTarget
  case ReplyActor
  case DeferredActor

  def stableKey: String =
    this match
      case VacatedTarget => "vacated-target"
      case ReplyActor    => "reply-actor"
      case DeferredActor => "deferred-actor"

private[chessjudgment] object CaptureExclusionMoveOrderCausalAuthority:
  private final case class PropositionDescriptor(
      rootFen: String,
      vacatingMove: RelationMoveTransitionWitness,
      deferredMove: RelationMoveTransitionWitness,
      captureReply: RelationMoveTransitionWitness,
      capturedTarget: RelationColoredPieceWitness
  ) extends CausalSemanticDescriptor:
    require(
      vacatingMove.side == deferredMove.side && captureReply.side != deferredMove.side,
      "capture-exclusion move order needs opposing root move and reply sides"
    )
    require(
      vacatingMove.from == capturedTarget.square &&
        vacatingMove.side == capturedTarget.side &&
        vacatingMove.beforeRole == capturedTarget.role &&
        captureReply.to == capturedTarget.square &&
        captureReply.side != capturedTarget.side,
      "the vacating root must remove the immediate reply's exact captured target"
    )

    val contractKind = BoundedCausalContractKind.CaptureExclusionMoveOrder
    val semanticParts: List[String] = List(
      vacatingMove.stableKey,
      deferredMove.stableKey,
      captureReply.stableKey,
      BoundedCausalIdentity.coloredPieceKey(capturedTarget)
    )

  def proposition(
      rootFen: String,
      vacatingMove: RelationMoveTransitionWitness,
      deferredMove: RelationMoveTransitionWitness,
      captureReply: RelationMoveTransitionWitness,
      capturedTarget: RelationColoredPieceWitness
  ): CausalPropositionIdentity =
    CausalPropositionIdentity.from(
      PropositionDescriptor(rootFen, vacatingMove, deferredMove, captureReply, capturedTarget)
    )

private[chessjudgment] sealed trait CaptureExclusionMoveOrderManifest
    extends BoundedCausalContractManifest:
  def legalMoves: List[CausalLegalMovePremiseUse]
  def replyAbsences: List[CausalClosedAbsenceBinding]
  def retainedStates: List[CausalClosedStateBinding]
  final def contractKind = BoundedCausalContractKind.CaptureExclusionMoveOrder
  final def premiseUses = Nil
  final override def supplementalPremiseUses = legalMoves
  final def absenceBindings = replyAbsences
  final override def stateBindings = retainedStates
  final def stableKey: String =
    List(
      contractKind.semanticNamespace,
      legalMoves.map(_.stableKey).mkString("[", ",", "]"),
      replyAbsences.map(_.stableKey).mkString("[", ",", "]"),
      retainedStates.map(_.stableKey).mkString("[", ",", "]")
    ).mkString("|")

private[chessjudgment] object CaptureExclusionMoveOrderManifest:
  private final case class Exact(
      legalMoves: List[CausalLegalMovePremiseUse],
      replyAbsences: List[CausalClosedAbsenceBinding],
      retainedStates: List[CausalClosedStateBinding]
  ) extends CaptureExclusionMoveOrderManifest

  def exact(
      vacatingMove: CausalLegalMovePremiseUse,
      immediateDeferredMove: CausalLegalMovePremiseUse,
      immediateCaptureReply: CausalLegalMovePremiseUse,
      laterDeferredMove: CausalLegalMovePremiseUse,
      capturedTarget: RelationColoredPieceWitness,
      replyAbsences: List[CausalClosedAbsenceBinding],
      retainedStates: List[CausalClosedStateBinding]
  ): CaptureExclusionMoveOrderManifest =
    val laterDeferredIndex = laterDeferredMove.stepIndex
    require(
      vacatingMove.role == CaptureExclusionMoveOrderPremiseRole.VacatingMove &&
        vacatingMove.branchRole == CaptureExclusionBranchRole.VacatingThenDeferred &&
        vacatingMove.stepIndex == 0,
      "the vacating move must be the exact vacating-branch root occurrence"
    )
    require(
      immediateDeferredMove.role == CaptureExclusionMoveOrderPremiseRole.ImmediateDeferredMove &&
        immediateDeferredMove.branchRole == CaptureExclusionBranchRole.ImmediateDeferredCapture &&
        immediateDeferredMove.stepIndex == 0 &&
        immediateCaptureReply.role == CaptureExclusionMoveOrderPremiseRole.ImmediateCaptureReply &&
        immediateCaptureReply.branchRole == CaptureExclusionBranchRole.ImmediateDeferredCapture &&
        immediateCaptureReply.branchId == immediateDeferredMove.branchId &&
        immediateCaptureReply.stepIndex == 1,
      "the immediate branch must retain the deferred move and its exact capture reply"
    )
    require(
      laterDeferredMove.role == CaptureExclusionMoveOrderPremiseRole.LaterDeferredMove &&
        laterDeferredMove.branchRole == CaptureExclusionBranchRole.VacatingThenDeferred &&
        laterDeferredMove.branchId == vacatingMove.branchId &&
        laterDeferredIndex >= 2 && laterDeferredIndex % 2 == 0,
      "the identical deferred move must be a later same-side vacating-branch occurrence"
    )
    require(
      sameMove(immediateDeferredMove, laterDeferredMove),
      "immediate and later deferred moves must retain identical movement, capture, and mode"
    )
    val replyCapture = immediateCaptureReply.capture.getOrElse(
      throw IllegalArgumentException("capture-exclusion move order needs an exact capture reply")
    )
    require(
      replyCapture.capturedSquare == immediateCaptureReply.movement.to &&
        replyCapture.capturedSquare == capturedTarget.square &&
        replyCapture.capturedSide == capturedTarget.side &&
        replyCapture.capturedRole == capturedTarget.role &&
        vacatingMove.movement.from == capturedTarget.square &&
        vacatingMove.movement.side == capturedTarget.side &&
        vacatingMove.movement.beforeRole == capturedTarget.role &&
        vacatingMove.movement.side == immediateDeferredMove.movement.side &&
        immediateCaptureReply.movement.side != immediateDeferredMove.movement.side,
      "the vacating root must remove the ordinary captured target of the immediate reply"
    )

    val replyQuery = PositionRelationExtractor.ClosedRelationAbsenceQuery.LegalMoveFromTo(
      immediateCaptureReply.movement.side,
      immediateCaptureReply.movement.from,
      immediateCaptureReply.movement.to
    )
    val expectedAbsenceIndices = List(0, laterDeferredIndex)
    require(
      replyAbsences.map(_.afterStepIndex) == expectedAbsenceIndices &&
        replyAbsences.forall(binding =>
          binding.role == CaptureExclusionMoveOrderAbsenceRole.CaptureReplyAbsent &&
            binding.branchRole == CaptureExclusionBranchRole.VacatingThenDeferred &&
            binding.branchId == vacatingMove.branchId && binding.authority.query == replyQuery
        ),
      "the exact capture reply must be closed at both vacating-branch causal endpoints"
    )

    val replyActor = coloredOrigin(immediateCaptureReply.movement)
    val deferredActor = coloredOrigin(immediateDeferredMove.movement)
    val expectedStates = (0 to laterDeferredIndex).toList.flatMap { index =>
      List(
        (
          CaptureExclusionMoveOrderStateRole.VacatedTarget,
          CaptureExclusionBranchRole.VacatingThenDeferred,
          vacatingMove.branchId,
          index,
          PositionRelationExtractor.ClosedPositionStateQuery.Vacant(capturedTarget.square)
        ),
        (
          CaptureExclusionMoveOrderStateRole.ReplyActor,
          CaptureExclusionBranchRole.VacatingThenDeferred,
          vacatingMove.branchId,
          index,
          PositionRelationExtractor.ClosedPositionStateQuery.OccupiedBy(replyActor)
        )
      ) ++ Option.when(index < laterDeferredIndex)(
        (
          CaptureExclusionMoveOrderStateRole.DeferredActor,
          CaptureExclusionBranchRole.VacatingThenDeferred,
          vacatingMove.branchId,
          index,
          PositionRelationExtractor.ClosedPositionStateQuery.OccupiedBy(deferredActor)
        )
      )
    }
    require(
      retainedStates.size == expectedStates.size &&
        retainedStates.zip(expectedStates).forall { case (binding, expected) =>
          val (role, branchRole, branchId, index, query) = expected
          binding.role == role && binding.branchRole == branchRole && binding.branchId == branchId &&
            binding.afterStepIndex == index && binding.query == query
        },
      "capture-exclusion move order needs the complete target and actor state interval"
    )
    Exact(
      List(vacatingMove, immediateDeferredMove, immediateCaptureReply, laterDeferredMove),
      replyAbsences,
      retainedStates
    )

  private def sameMove(
      left: CausalLegalMovePremiseUse,
      right: CausalLegalMovePremiseUse
  ): Boolean =
    left.movement == right.movement &&
      EvidenceRef.sameMove(left.moveUci, right.moveUci) &&
      left.capture == right.capture && left.movementMode == right.movementMode &&
      left.legalMoveSemanticId == right.legalMoveSemanticId

  private def coloredOrigin(
      movement: RelationMoveTransitionWitness
  ): RelationColoredPieceWitness =
    RelationColoredPieceWitness(movement.from, movement.beforeRole, movement.side)

private[chessjudgment] final case class CaptureExclusionMoveOrderSemanticProof private[chessjudgment] (
    identity: CausalPropositionIdentity,
    vacatingMove: RelationMoveTransitionWitness,
    deferredMove: RelationMoveTransitionWitness,
    captureReply: RelationMoveTransitionWitness,
    capturedTarget: RelationColoredPieceWitness
):
  require(identity.contractKind == BoundedCausalContractKind.CaptureExclusionMoveOrder)
  require(
    vacatingMove.from == capturedTarget.square && vacatingMove.side == capturedTarget.side &&
      vacatingMove.beforeRole == capturedTarget.role && captureReply.to == capturedTarget.square &&
      captureReply.side != capturedTarget.side && deferredMove.side == capturedTarget.side,
    "capture-exclusion semantics must retain the exact vacated capture target"
  )

  def semanticId: String = identity.semanticId
  def rootBoardState: String = identity.rootPositionIdentity.value

private[chessjudgment] final case class CaptureExclusionMoveOrderOccurrence private[chessjudgment] (
    proofSet: BoundedCausalProofSet
):
  require(proofSet.proposition.contractKind == BoundedCausalContractKind.CaptureExclusionMoveOrder)
  require(
    proofSet.occurrence.branches.size == 2 &&
      proofSet.occurrence.branch(CaptureExclusionBranchRole.VacatingThenDeferred).nonEmpty &&
      proofSet.occurrence.branch(CaptureExclusionBranchRole.ImmediateDeferredCapture).nonEmpty,
    "capture exclusion needs its two family-specific sibling branches"
  )
  require(laterDeferredStepIndex >= 2 && laterDeferredStepIndex % 2 == 0)
  require(immediateSteps.size == 2)

  private def branch(role: CaptureExclusionBranchRole): CausalBranchOccurrence =
    proofSet.occurrence.branch(role).getOrElse(
      throw IllegalStateException(s"capture exclusion lost its $role branch")
    )

  def semanticId: String = proofSet.proposition.semanticId
  def occurrenceId: String = proofSet.occurrence.occurrenceId
  def vacatingBranch: CausalBranchOccurrence = branch(CaptureExclusionBranchRole.VacatingThenDeferred)
  def immediateBranch: CausalBranchOccurrence = branch(CaptureExclusionBranchRole.ImmediateDeferredCapture)
  def vacatingLine: LineNodeRef = vacatingBranch.line
  def immediateLine: LineNodeRef = immediateBranch.line
  def vacatingSteps: List[LineReplayStep] = vacatingBranch.replaySteps
  def immediateSteps: List[LineReplayStep] = immediateBranch.replaySteps
  def laterDeferredStepIndex: Int = vacatingSteps.size - 1
  def proofPaths: List[CausalProofPathOccurrence] = proofSet.paths
  def vacatingStep: LineReplayStep = vacatingSteps.head
  def laterDeferredStep: LineReplayStep = vacatingSteps(laterDeferredStepIndex)
  def immediateDeferredStep: LineReplayStep = immediateSteps.head
  def immediateReplyStep: LineReplayStep = immediateSteps(1)

private[chessjudgment] final case class CaptureExclusionMoveOrderDependencyManifest private[chessjudgment] (
    subject: CertifiedRootOccurrence,
    vacating: CertifiedRootOccurrence,
    immediate: CertifiedRootOccurrence,
    proofSet: BoundedCausalProofSet
) extends BoundedCausalDependencyManifest:
  val contractKind = BoundedCausalContractKind.CaptureExclusionMoveOrder
  require(
    proofSet.proposition.contractKind == contractKind &&
      proofSet.paths.forall(_.manifest.contractKind == contractKind)
  )

  val stableKey: String =
    List(
      "subject-root",
      BoundedCausalIdentity.evidenceRecordKey(subject.lineOwner),
      BoundedCausalIdentity.evidenceRecordKey(subject.transitionOwner),
      "vacating-root",
      BoundedCausalIdentity.evidenceRecordKey(vacating.lineOwner),
      BoundedCausalIdentity.evidenceRecordKey(vacating.transitionOwner),
      "immediate-root",
      BoundedCausalIdentity.evidenceRecordKey(immediate.lineOwner),
      BoundedCausalIdentity.evidenceRecordKey(immediate.transitionOwner),
      proofSet.proposition.semanticId,
      proofSet.occurrence.occurrenceId,
      proofSet.paths.map(_.pathOccurrenceId).sorted.mkString("paths[", ",", "]")
    ).mkString("|")

  def consumes(
      exactSubject: CertifiedRootOccurrence,
      exactVacating: CertifiedRootOccurrence,
      exactImmediate: CertifiedRootOccurrence
  ): Boolean =
    subject == exactSubject && vacating == exactVacating && immediate == exactImmediate

private[chessjudgment] final class CertifiedCaptureExclusionMoveOrder private (
    val semantic: CaptureExclusionMoveOrderSemanticProof,
    val occurrence: CaptureExclusionMoveOrderOccurrence,
    val dependency: BoundedCausalDependencyFingerprint,
    private val dependencyManifest: CaptureExclusionMoveOrderDependencyManifest,
    val subject: CertifiedRootOccurrence,
    private val vacating: CertifiedRootOccurrence,
    private val immediate: CertifiedRootOccurrence,
    private val legalMoveAuthorities: List[(CausalLegalMovePremiseUse, RecordBoundLegalMoveOccurrence)],
    private val absenceAuthorities: List[(CausalClosedAbsenceUse, ClosedRelationAbsenceAuthority)],
    private val stateAuthorities: List[(CausalClosedStateUse, ClosedPositionStateAuthority)]
):
  require(
    semantic.identity == occurrence.proofSet.proposition &&
      dependencyManifest.proofSet == occurrence.proofSet &&
      BoundedCausalDependencyFingerprint.from(dependencyManifest) == dependency
  )
  require(
    occurrence.proofPaths.flatMap(_.manifest.supplementalPremiseUses) == legalMoveAuthorities.map(_._1) &&
      occurrence.proofPaths.flatMap(_.closedAbsenceUses) == absenceAuthorities.map(_._1) &&
      occurrence.proofPaths.flatMap(_.closedStateUses) == stateAuthorities.map(_._1),
    "capture-exclusion certificate must own every exact lower use"
  )

  def parentSources: List[EvidenceRef] =
    val sources = List(
      vacating.lineOwner.ref,
      vacating.transitionOwner.ref,
      immediate.lineOwner.ref,
      immediate.transitionOwner.ref
    ).sortBy(_.id)
    require(
      sources.map(_.id).distinct.size == sources.size,
      "capture exclusion needs four distinct line/transition parent owners"
    )
    sources

  private[chessjudgment] def lowerIssuerRecords: List[EvidenceRecord] =
    List(
      vacating.lineOwner,
      vacating.transitionOwner,
      immediate.lineOwner,
      immediate.transitionOwner
    )

  def consumesDependencies(
      exactSubject: CertifiedRootOccurrence,
      exactVacating: CertifiedRootOccurrence,
      exactImmediate: CertifiedRootOccurrence
  ): Boolean =
    dependencyManifest.consumes(exactSubject, exactVacating, exactImmediate)

  def proves(record: EvidenceRecord, payload: CaptureExclusionMoveOrderEvidence): Boolean =
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
    subject.remainsCertified && vacating.remainsCertified && immediate.remainsCertified &&
      (subject == vacating || subject == immediate) &&
      occurrence.vacatingLine == vacating.line &&
      occurrence.immediateLine == immediate.line &&
      vacating.replay.replaySteps.take(occurrence.laterDeferredStepIndex + 1) == occurrence.vacatingSteps &&
      immediate.replay.replaySteps.take(2) == occurrence.immediateSteps &&
      CaptureExclusionMoveOrderCausalAuthority.proposition(
        semantic.rootBoardState,
        semantic.vacatingMove,
        semantic.deferredMove,
        semantic.captureReply,
        semantic.capturedTarget
      ) == semantic.identity &&
      legalMovesRemainCertified && absencesRemainCertified && statesRemainCertified &&
      BoundedCausalDependencyFingerprint.from(dependencyManifest) == dependency

  private def legalMovesRemainCertified: Boolean =
    legalMoveAuthorities.forall { case (use, authority) =>
      ownerFor(use.branchRole).exists { case (record, replay) =>
        replay.legalMoveOccurrence(authority.step)
          .flatMap(RecordBoundLegalMoveOccurrence.certified(record, _))
          .contains(authority) &&
          occurrence.proofSet.occurrence.branch(use.branchRole).exists(branch =>
            branch.branchId == use.branchId && branch.stepAt(use.stepIndex).exists(_.step == authority.step)
          ) &&
          CausalLegalMovePremiseUse.from(
            use.role,
            authority,
            occurrence.proofSet.occurrence.branch(use.branchRole).get,
            use.stepIndex
          ) == use
      }
    }

  private def absencesRemainCertified: Boolean =
    absenceAuthorities.forall { case (use, authority) =>
      val binding = use.binding
      authority.issuerRecord == vacating.lineOwner && authority.remainsCertified &&
        binding.authority == authority &&
        occurrence.vacatingBranch.stepAt(binding.afterStepIndex).exists(step =>
          step.step == authority.step && step.line == authority.issuerLine
        )
    }

  private def statesRemainCertified: Boolean =
    stateAuthorities.forall { case (use, authority) =>
      val binding = use.binding
      ownerFor(binding.branchRole).exists { case (record, _) =>
        binding.authority == authority && authority.issuerRecord == record &&
          ClosedPositionStateAuthority
            .certified(record, authority.occurrence, authority.proof)
            .contains(authority) &&
          occurrence.proofSet.occurrence.branch(binding.branchRole).exists(branch =>
            branch.branchId == binding.branchId && branch.stepAt(binding.afterStepIndex).exists(step =>
              step.step == authority.step && step.line == authority.issuerLine
            )
          )
      }
    }

  private def ownerFor(
      role: CausalBranchRole
  ): Option[(EvidenceRecord, CanonicalLineReplay)] =
    role match
      case CaptureExclusionBranchRole.VacatingThenDeferred =>
        Some(vacating.lineOwner -> vacating.replay)
      case CaptureExclusionBranchRole.ImmediateDeferredCapture =>
        Some(immediate.lineOwner -> immediate.replay)
      case _ => None

private[chessjudgment] object CertifiedCaptureExclusionMoveOrder:
  def from(
      semantic: CaptureExclusionMoveOrderSemanticProof,
      occurrence: CaptureExclusionMoveOrderOccurrence,
      dependency: BoundedCausalDependencyFingerprint,
      dependencyManifest: CaptureExclusionMoveOrderDependencyManifest,
      subject: CertifiedRootOccurrence,
      vacating: CertifiedRootOccurrence,
      immediate: CertifiedRootOccurrence,
      legalMoveAuthorities: List[(CausalLegalMovePremiseUse, RecordBoundLegalMoveOccurrence)],
      absenceAuthorities: List[(CausalClosedAbsenceUse, ClosedRelationAbsenceAuthority)],
      stateAuthorities: List[(CausalClosedStateUse, ClosedPositionStateAuthority)]
  ): CertifiedCaptureExclusionMoveOrder =
    val exact = new CertifiedCaptureExclusionMoveOrder(
      semantic,
      occurrence,
      dependency,
      dependencyManifest,
      subject,
      vacating,
      immediate,
      legalMoveAuthorities,
      absenceAuthorities,
      stateAuthorities
    )
    require(exact.remainsCertified, "capture-exclusion move order must retain every lower authority")
    exact

private[chessjudgment] final case class CaptureExclusionMoveOrderDemand private[chessjudgment] (
    vacatingMove: ReplayLegalMoveOccurrence,
    immediateDeferredMove: ReplayLegalMoveOccurrence,
    immediateCaptureReply: ReplayLegalMoveOccurrence,
    laterDeferredMove: ReplayLegalMoveOccurrence,
    laterDeferredStepIndex: Int
):
  require(laterDeferredStepIndex >= 2 && laterDeferredStepIndex % 2 == 0)
  require(CaptureExclusionMoveOrderDemand.sameMove(immediateDeferredMove, laterDeferredMove))

  def stableKey: String =
    List(
      vacatingMove.occurrenceId,
      immediateDeferredMove.occurrenceId,
      immediateCaptureReply.occurrenceId,
      laterDeferredMove.occurrenceId,
      laterDeferredStepIndex.toString
    ).mkString("|")

private[chessjudgment] object CaptureExclusionMoveOrderDemand:
  def sameMove(left: ReplayLegalMoveOccurrence, right: ReplayLegalMoveOccurrence): Boolean =
    ReplayLegalMoveOccurrence.sameMove(left, right)

private[chessjudgment] object CaptureExclusionMoveOrderProof:
  private final case class StateAuthority(
      role: CaptureExclusionMoveOrderStateRole,
      branch: CausalBranchOccurrence,
      stepIndex: Int,
      authority: ClosedPositionStateAuthority
  )

  def certifyDemanded(
      subject: CertifiedRootOccurrence,
      vacating: CertifiedRootOccurrence,
      immediate: CertifiedRootOccurrence,
      demands: List[CaptureExclusionMoveOrderDemand]
  ): List[CertifiedCaptureExclusionMoveOrder] =
    demands.flatMap(demand =>
      derive(
        subject,
        vacating,
        immediate,
        demand
      )
    )

  private def derive(
      subject: CertifiedRootOccurrence,
      vacating: CertifiedRootOccurrence,
      immediate: CertifiedRootOccurrence,
      demand: CaptureExclusionMoveOrderDemand
  ): Option[CertifiedCaptureExclusionMoveOrder] =
    val vacatingLineRecord = vacating.lineOwner
    val immediateLineRecord = immediate.lineOwner
    val vacatingReplay = vacating.replay
    val immediateReplay = immediate.replay
    for
      _ <- Option.when(
        subject.remainsCertified && vacating.remainsCertified && immediate.remainsCertified &&
          (subject == vacating || subject == immediate) &&
          vacating.line != immediate.line &&
          vacating.transition.from.ply == immediate.transition.from.ply &&
          PrincipalVariationEvidence.sameBoardState(
            vacating.transition.from.fen,
            immediate.transition.from.fen
          )
      )((): Unit)
      vacatingRoot <- vacatingReplay.replaySteps.headOption
      immediateRoot <- immediateReplay.replaySteps.headOption
      immediateReplyStep <- immediateReplay.replaySteps.lift(1)
      laterDeferredStep <- vacatingReplay.replaySteps.lift(demand.laterDeferredStepIndex)
      if demand.laterDeferredStepIndex >= 2 && demand.laterDeferredStepIndex % 2 == 0
      if demand.vacatingMove.step == vacatingRoot &&
        demand.immediateDeferredMove.step == immediateRoot &&
        demand.immediateCaptureReply.step == immediateReplyStep &&
        demand.laterDeferredMove.step == laterDeferredStep
      rootBoard <- PrincipalVariationEvidence.semanticBoardStateFen(vacatingRoot.fenBefore)
      immediateBoard <- PrincipalVariationEvidence.semanticBoardStateFen(immediateRoot.fenBefore)
      if rootBoard == immediateBoard
      vacatingMove <- RecordBoundLegalMoveOccurrence.atIndex(vacatingLineRecord, 0, demand.vacatingMove)
      immediateDeferredMove <- RecordBoundLegalMoveOccurrence.atIndex(
        immediateLineRecord,
        0,
        demand.immediateDeferredMove
      )
      immediateCaptureReply <- RecordBoundLegalMoveOccurrence.atIndex(
        immediateLineRecord,
        1,
        demand.immediateCaptureReply
      )
      laterDeferredMove <- RecordBoundLegalMoveOccurrence.atIndex(
        vacatingLineRecord,
        demand.laterDeferredStepIndex,
        demand.laterDeferredMove
      )
      if sameMove(immediateDeferredMove, laterDeferredMove)
      replyCapture <- immediateCaptureReply.capture
      if replyCapture.capturedSquare == immediateCaptureReply.movement.to
      capturedTarget = RelationColoredPieceWitness(
        replyCapture.capturedSquare,
        replyCapture.capturedRole,
        replyCapture.capturedSide
      )
      if vacatingMove.movement.from == capturedTarget.square &&
        vacatingMove.movement.side == capturedTarget.side &&
        vacatingMove.movement.beforeRole == capturedTarget.role &&
        vacatingMove.movement.side == immediateDeferredMove.movement.side &&
        immediateCaptureReply.movement.side != immediateDeferredMove.movement.side
      vacatingBranch = CausalBranchOccurrence.fromRootOccurrence(
        CaptureExclusionBranchRole.VacatingThenDeferred,
        vacating,
        demand.laterDeferredStepIndex + 1
      )
      immediateBranch = CausalBranchOccurrence.fromRootOccurrence(
        CaptureExclusionBranchRole.ImmediateDeferredCapture,
        immediate,
        2
      )
      absenceAuthorities <- vacatingBranchAbsences(
        vacatingLineRecord,
        vacatingReplay,
        immediateCaptureReply,
        demand.laterDeferredStepIndex
      )
      stateAuthorities <- retainedStates(
        vacatingLineRecord,
        vacatingReplay,
        vacatingBranch,
        immediateDeferredMove,
        immediateCaptureReply,
        capturedTarget,
        demand.laterDeferredStepIndex
      )
    yield certify(
      rootBoard,
      vacatingMove,
      immediateDeferredMove,
      immediateCaptureReply,
      laterDeferredMove,
      capturedTarget,
      vacatingBranch,
      immediateBranch,
      absenceAuthorities,
      stateAuthorities,
      subject,
      vacating,
      immediate,
      demand.laterDeferredStepIndex
    )

  private def certify(
      rootBoard: String,
      vacatingMove: RecordBoundLegalMoveOccurrence,
      immediateDeferredMove: RecordBoundLegalMoveOccurrence,
      immediateCaptureReply: RecordBoundLegalMoveOccurrence,
      laterDeferredMove: RecordBoundLegalMoveOccurrence,
      capturedTarget: RelationColoredPieceWitness,
      vacatingBranch: CausalBranchOccurrence,
      immediateBranch: CausalBranchOccurrence,
      absenceAuthorities: List[(Int, ClosedRelationAbsenceAuthority)],
      stateAuthorities: List[StateAuthority],
      subject: CertifiedRootOccurrence,
      vacating: CertifiedRootOccurrence,
      immediate: CertifiedRootOccurrence,
      deferredIndex: Int
  ): CertifiedCaptureExclusionMoveOrder =
    val proposition = CaptureExclusionMoveOrderCausalAuthority.proposition(
      rootBoard,
      vacatingMove.movement,
      immediateDeferredMove.movement,
      immediateCaptureReply.movement,
      capturedTarget
    )
    val causalOccurrence = CausalOccurrenceIdentity.from(
      proposition,
      List(vacatingBranch, immediateBranch)
    )
    val legalUses = List(
      CausalLegalMovePremiseUse.from(
        CaptureExclusionMoveOrderPremiseRole.VacatingMove,
        vacatingMove,
        vacatingBranch,
        0
      ),
      CausalLegalMovePremiseUse.from(
        CaptureExclusionMoveOrderPremiseRole.ImmediateDeferredMove,
        immediateDeferredMove,
        immediateBranch,
        0
      ),
      CausalLegalMovePremiseUse.from(
        CaptureExclusionMoveOrderPremiseRole.ImmediateCaptureReply,
        immediateCaptureReply,
        immediateBranch,
        1
      ),
      CausalLegalMovePremiseUse.from(
        CaptureExclusionMoveOrderPremiseRole.LaterDeferredMove,
        laterDeferredMove,
        vacatingBranch,
        deferredIndex
      )
    )
    val absenceBindings = absenceAuthorities.map { case (index, authority) =>
      CausalClosedAbsenceBinding.afterStep(
        CaptureExclusionMoveOrderAbsenceRole.CaptureReplyAbsent,
        authority,
        vacatingBranch,
        index
      )
    }
    val stateBindings = stateAuthorities.map(state =>
      CausalClosedStateBinding.afterStep(state.role, state.authority, state.branch, state.stepIndex)
    )
    val manifest = CaptureExclusionMoveOrderManifest.exact(
      legalUses(0),
      legalUses(1),
      legalUses(2),
      legalUses(3),
      capturedTarget,
      absenceBindings,
      stateBindings
    )
    val path = CausalProofPathOccurrence.from(proposition, manifest)
    val proofSet = BoundedCausalProofSet.from(proposition, causalOccurrence, List(path))
    val semantic = CaptureExclusionMoveOrderSemanticProof(
      proposition,
      vacatingMove.movement,
      immediateDeferredMove.movement,
      immediateCaptureReply.movement,
      capturedTarget
    )
    val occurrence = CaptureExclusionMoveOrderOccurrence(proofSet)
    val dependencyManifest = CaptureExclusionMoveOrderDependencyManifest(
      subject,
      vacating,
      immediate,
      proofSet
    )
    val dependency = BoundedCausalDependencyFingerprint.from(dependencyManifest)
    CertifiedCaptureExclusionMoveOrder.from(
      semantic,
      occurrence,
      dependency,
      dependencyManifest,
      subject,
      vacating,
      immediate,
      legalUses.zip(
        List(vacatingMove, immediateDeferredMove, immediateCaptureReply, laterDeferredMove)
      ),
      path.closedAbsenceUses.zip(absenceAuthorities.map(_._2)),
      path.closedStateUses.zip(stateAuthorities.map(_.authority))
    )

  private def vacatingBranchAbsences(
      lineRecord: EvidenceRecord,
      replay: CanonicalLineReplay,
      reply: RecordBoundLegalMoveOccurrence,
      deferredIndex: Int
  ): Option[List[(Int, ClosedRelationAbsenceAuthority)]] =
    val query = PositionRelationExtractor.ClosedRelationAbsenceQuery.LegalMoveFromTo(
      reply.movement.side,
      reply.movement.from,
      reply.movement.to
    )
    traverse(List(0, deferredIndex))(index =>
      for
        step <- replay.replaySteps.lift(index)
        occurrence <- replay.positionAfter(step)
        authority <- ClosedRelationAbsenceAuthority.forQuery(lineRecord, occurrence, query)
      yield index -> authority
    )

  private def retainedStates(
      vacatingLineRecord: EvidenceRecord,
      vacatingReplay: CanonicalLineReplay,
      vacatingBranch: CausalBranchOccurrence,
      deferred: RecordBoundLegalMoveOccurrence,
      reply: RecordBoundLegalMoveOccurrence,
      target: RelationColoredPieceWitness,
      deferredIndex: Int
  ): Option[List[StateAuthority]] =
    val replyActor = coloredOrigin(reply.movement)
    val deferredActor = coloredOrigin(deferred.movement)
    val vacatingStates = (0 to deferredIndex).toList.flatMap { index =>
      val exact = List(
        stateAt(
          vacatingLineRecord,
          vacatingReplay,
          vacatingBranch,
          index,
          CaptureExclusionMoveOrderStateRole.VacatedTarget,
          (occurrence, scope) => occurrence.existingVacancyState(target.square, scope)
        ),
        stateAt(
          vacatingLineRecord,
          vacatingReplay,
          vacatingBranch,
          index,
          CaptureExclusionMoveOrderStateRole.ReplyActor,
          (occurrence, scope) => occurrence.existingOccupantState(replyActor, scope)
        )
      )
      exact ++ Option.when(index < deferredIndex)(
        stateAt(
          vacatingLineRecord,
          vacatingReplay,
          vacatingBranch,
          index,
          CaptureExclusionMoveOrderStateRole.DeferredActor,
          (occurrence, scope) => occurrence.existingOccupantState(deferredActor, scope)
        )
      )
    }
    traverse(vacatingStates)(identity)

  private def stateAt(
      lineRecord: EvidenceRecord,
      replay: CanonicalLineReplay,
      branch: CausalBranchOccurrence,
      index: Int,
      role: CaptureExclusionMoveOrderStateRole,
      select: (ReplayPositionOccurrence, EvidenceScope) =>
        Option[PositionRelationExtractor.ClosedPositionStateProof]
  ): Option[StateAuthority] =
    for
      step <- replay.replaySteps.lift(index)
      authority <- ClosedPositionStateAuthority.atStep(lineRecord, step)(select)
    yield StateAuthority(role, branch, index, authority)

  private def sameMove(
      left: RecordBoundLegalMoveOccurrence,
      right: RecordBoundLegalMoveOccurrence
  ): Boolean =
    RecordBoundLegalMoveOccurrence.sameMove(left, right)

  private def coloredOrigin(
      movement: RelationMoveTransitionWitness
  ): RelationColoredPieceWitness =
    RelationColoredPieceWitness(movement.from, movement.beforeRole, movement.side)

  private def traverse[A, B](values: List[A])(select: A => Option[B]): Option[List[B]] =
    values.foldRight(Option(List.empty[B])) { (value, accumulated) =>
      for
        exact <- select(value)
        tail <- accumulated
      yield exact :: tail
    }
