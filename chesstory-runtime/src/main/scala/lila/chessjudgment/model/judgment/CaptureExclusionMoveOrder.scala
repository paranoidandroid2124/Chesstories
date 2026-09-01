package lila.chessjudgment.model.judgment

import lila.chessjudgment.analysis.position.PositionRelationExtractor
import lila.chessjudgment.model.line.PrincipalVariationEvidence

private[chessjudgment] enum CaptureExclusionMoveOrderPremiseRole extends CausalPremiseRole:
  case ReferenceVacatingMove
  case PlayedDeferredMove
  case PlayedCaptureReply
  case ReferenceDeferredMove

  def stableKey: String =
    this match
      case ReferenceVacatingMove  => "reference-vacating-move"
      case PlayedDeferredMove     => "played-deferred-move"
      case PlayedCaptureReply     => "played-capture-reply"
      case ReferenceDeferredMove  => "reference-deferred-move"

private[chessjudgment] enum CaptureExclusionMoveOrderAbsenceRole extends CausalAbsenceRole:
  case ReferenceCaptureReplyAbsent

  def stableKey: String = "reference-capture-reply-absent"

private[chessjudgment] enum CaptureExclusionMoveOrderStateRole extends CausalStateRole:
  case ReferenceVacatedTarget
  case ReferenceReplyActor
  case ReferenceDeferredActor

  def stableKey: String =
    this match
      case ReferenceVacatedTarget  => "reference-vacated-target"
      case ReferenceReplyActor     => "reference-reply-actor"
      case ReferenceDeferredActor  => "reference-deferred-actor"

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
      "the reference root must vacate the played reply's exact captured target"
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
      contractKind.toString.toLowerCase,
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
      referenceVacating: CausalLegalMovePremiseUse,
      playedDeferred: CausalLegalMovePremiseUse,
      playedReply: CausalLegalMovePremiseUse,
      referenceDeferred: CausalLegalMovePremiseUse,
      capturedTarget: RelationColoredPieceWitness,
      replyAbsences: List[CausalClosedAbsenceBinding],
      retainedStates: List[CausalClosedStateBinding]
  ): CaptureExclusionMoveOrderManifest =
    val deferredIndex = referenceDeferred.stepIndex
    require(
      referenceVacating.role == CaptureExclusionMoveOrderPremiseRole.ReferenceVacatingMove &&
        referenceVacating.branchRole == ComparedLineBranchRole.CounterfactualReference &&
        referenceVacating.stepIndex == 0,
      "the vacating move must be the exact reference root occurrence"
    )
    require(
      playedDeferred.role == CaptureExclusionMoveOrderPremiseRole.PlayedDeferredMove &&
        playedDeferred.branchRole == ComparedLineBranchRole.PlayedRootAnalysisContinuation &&
        playedDeferred.stepIndex == 0 &&
        playedReply.role == CaptureExclusionMoveOrderPremiseRole.PlayedCaptureReply &&
        playedReply.branchRole == ComparedLineBranchRole.PlayedRootAnalysisContinuation &&
        playedReply.branchId == playedDeferred.branchId && playedReply.stepIndex == 1,
      "the played branch must retain the deferred move and its immediate capture reply"
    )
    require(
      referenceDeferred.role == CaptureExclusionMoveOrderPremiseRole.ReferenceDeferredMove &&
        referenceDeferred.branchRole == ComparedLineBranchRole.CounterfactualReference &&
        referenceDeferred.branchId == referenceVacating.branchId &&
        deferredIndex >= 2 && deferredIndex % 2 == 0,
      "the identical deferred move must be a later same-side reference occurrence"
    )
    require(
      sameMove(playedDeferred, referenceDeferred),
      "played and reference deferred moves must retain identical movement, capture, and mode"
    )
    val replyCapture = playedReply.capture.getOrElse(
      throw IllegalArgumentException("capture-exclusion move order needs an exact capture reply")
    )
    require(
      replyCapture.capturedSquare == playedReply.movement.to &&
        replyCapture.capturedSquare == capturedTarget.square &&
        replyCapture.capturedSide == capturedTarget.side &&
        replyCapture.capturedRole == capturedTarget.role &&
        referenceVacating.movement.from == capturedTarget.square &&
        referenceVacating.movement.side == capturedTarget.side &&
        referenceVacating.movement.beforeRole == capturedTarget.role &&
        referenceVacating.movement.side == playedDeferred.movement.side &&
        playedReply.movement.side != playedDeferred.movement.side,
      "the reference root must remove the ordinary captured target of the played reply"
    )

    val replyQuery = PositionRelationExtractor.ClosedRelationAbsenceQuery.LegalMoveFromTo(
      playedReply.movement.side,
      playedReply.movement.from,
      playedReply.movement.to
    )
    val expectedAbsenceIndices = List(0, deferredIndex)
    require(
      replyAbsences.map(_.afterStepIndex) == expectedAbsenceIndices &&
        replyAbsences.forall(binding =>
          binding.role == CaptureExclusionMoveOrderAbsenceRole.ReferenceCaptureReplyAbsent &&
            binding.branchRole == ComparedLineBranchRole.CounterfactualReference &&
            binding.branchId == referenceVacating.branchId && binding.authority.query == replyQuery
        ),
      "the exact capture reply must be closed at both causal reference endpoints"
    )

    val replyActor = coloredOrigin(playedReply.movement)
    val deferredActor = coloredOrigin(playedDeferred.movement)
    val expectedStates = (0 to deferredIndex).toList.flatMap { index =>
      List(
        (
          CaptureExclusionMoveOrderStateRole.ReferenceVacatedTarget,
          ComparedLineBranchRole.CounterfactualReference,
          referenceVacating.branchId,
          index,
          PositionRelationExtractor.ClosedPositionStateQuery.Vacant(capturedTarget.square)
        ),
        (
          CaptureExclusionMoveOrderStateRole.ReferenceReplyActor,
          ComparedLineBranchRole.CounterfactualReference,
          referenceVacating.branchId,
          index,
          PositionRelationExtractor.ClosedPositionStateQuery.OccupiedBy(replyActor)
        )
      ) ++ Option.when(index < deferredIndex)(
        (
          CaptureExclusionMoveOrderStateRole.ReferenceDeferredActor,
          ComparedLineBranchRole.CounterfactualReference,
          referenceVacating.branchId,
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
      List(referenceVacating, playedDeferred, playedReply, referenceDeferred),
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
      proofSet.occurrence.branch(ComparedLineBranchRole.CounterfactualReference).exists(
        _.line.role == LineNodeRole.BestReference
      ) &&
      proofSet.occurrence.branch(ComparedLineBranchRole.PlayedRootAnalysisContinuation).exists(
        _.line.role == LineNodeRole.Played
      ),
    "capture-exclusion move order needs one reference and one played occurrence"
  )
  require(referenceDeferredStepIndex >= 2 && referenceDeferredStepIndex % 2 == 0)
  require(playedSteps.size == 2)

  private def branch(role: ComparedLineBranchRole): CausalBranchOccurrence =
    proofSet.occurrence.branch(role).getOrElse(
      throw IllegalStateException(s"capture-exclusion move order lost its $role branch")
    )

  def semanticId: String = proofSet.proposition.semanticId
  def occurrenceId: String = proofSet.occurrence.occurrenceId
  def referenceBranch: CausalBranchOccurrence = branch(ComparedLineBranchRole.CounterfactualReference)
  def playedBranch: CausalBranchOccurrence = branch(ComparedLineBranchRole.PlayedRootAnalysisContinuation)
  def referenceLine: LineNodeRef = referenceBranch.line
  def playedLine: LineNodeRef = playedBranch.line
  def referenceSteps: List[LineReplayStep] = referenceBranch.replaySteps
  def playedSteps: List[LineReplayStep] = playedBranch.replaySteps
  def referenceDeferredStepIndex: Int = referenceSteps.size - 1
  def proofPaths: List[CausalProofPathOccurrence] = proofSet.paths
  def vacatingStep: LineReplayStep = referenceSteps.head
  def referenceDeferredStep: LineReplayStep = referenceSteps(referenceDeferredStepIndex)
  def playedDeferredStep: LineReplayStep = playedSteps.head
  def playedReplyStep: LineReplayStep = playedSteps(1)

private[chessjudgment] final case class CaptureExclusionMoveOrderDependencyManifest private[chessjudgment] (
    referenceLineRecord: EvidenceRecord,
    playedLineRecord: EvidenceRecord,
    proofSet: BoundedCausalProofSet
) extends BoundedCausalDependencyManifest:
  val contractKind = BoundedCausalContractKind.CaptureExclusionMoveOrder
  require(
    proofSet.proposition.contractKind == contractKind &&
      proofSet.paths.forall(_.manifest.contractKind == contractKind)
  )

  val stableKey: String =
    List(
      BoundedCausalIdentity.evidenceRecordKey(referenceLineRecord),
      BoundedCausalIdentity.evidenceRecordKey(playedLineRecord),
      proofSet.proposition.semanticId,
      proofSet.occurrence.occurrenceId,
      proofSet.paths.map(_.pathOccurrenceId).sorted.mkString("paths[", ",", "]")
    ).mkString("|")

  def consumes(referenceSource: EvidenceRecord, playedSource: EvidenceRecord): Boolean =
    referenceLineRecord == referenceSource && playedLineRecord == playedSource

private[chessjudgment] final class CertifiedCaptureExclusionMoveOrder private (
    val semantic: CaptureExclusionMoveOrderSemanticProof,
    val occurrence: CaptureExclusionMoveOrderOccurrence,
    val dependency: BoundedCausalDependencyFingerprint,
    private val dependencyManifest: CaptureExclusionMoveOrderDependencyManifest,
    private val referenceLineRecord: EvidenceRecord,
    private val playedLineRecord: EvidenceRecord,
    private val referenceReplay: CanonicalLineReplay,
    private val playedReplay: CanonicalLineReplay,
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
    List(referenceLineRecord.ref, playedLineRecord.ref).sortBy(_.id)

  private[chessjudgment] def lowerIssuerRecords: List[EvidenceRecord] =
    List(referenceLineRecord, playedLineRecord)

  def consumesDependencies(referenceSource: EvidenceRecord, playedSource: EvidenceRecord): Boolean =
    dependencyManifest.consumes(referenceSource, playedSource)

  def proves(record: EvidenceRecord, payload: CaptureExclusionMoveOrderEvidence): Boolean =
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
      referenceLineRecord.ref.position == playedLineRecord.ref.position &&
      referenceReplay.replaySteps.take(occurrence.referenceDeferredStepIndex + 1) == occurrence.referenceSteps &&
      playedReplay.replaySteps.take(2) == occurrence.playedSteps &&
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
      authority.issuerRecord == referenceLineRecord && authority.remainsCertified &&
        binding.authority == authority &&
        occurrence.referenceBranch.stepAt(binding.afterStepIndex).exists(step =>
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
      case ComparedLineBranchRole.CounterfactualReference => Some(referenceLineRecord -> referenceReplay)
      case ComparedLineBranchRole.PlayedRootAnalysisContinuation => Some(playedLineRecord -> playedReplay)
      case _ => None

private[chessjudgment] object CertifiedCaptureExclusionMoveOrder:
  def from(
      semantic: CaptureExclusionMoveOrderSemanticProof,
      occurrence: CaptureExclusionMoveOrderOccurrence,
      dependency: BoundedCausalDependencyFingerprint,
      dependencyManifest: CaptureExclusionMoveOrderDependencyManifest,
      referenceLineRecord: EvidenceRecord,
      playedLineRecord: EvidenceRecord,
      referenceReplay: CanonicalLineReplay,
      playedReplay: CanonicalLineReplay,
      legalMoveAuthorities: List[(CausalLegalMovePremiseUse, RecordBoundLegalMoveOccurrence)],
      absenceAuthorities: List[(CausalClosedAbsenceUse, ClosedRelationAbsenceAuthority)],
      stateAuthorities: List[(CausalClosedStateUse, ClosedPositionStateAuthority)]
  ): CertifiedCaptureExclusionMoveOrder =
    val exact = new CertifiedCaptureExclusionMoveOrder(
      semantic,
      occurrence,
      dependency,
      dependencyManifest,
      referenceLineRecord,
      playedLineRecord,
      referenceReplay,
      playedReplay,
      legalMoveAuthorities,
      absenceAuthorities,
      stateAuthorities
    )
    require(exact.remainsCertified, "capture-exclusion move order must retain every lower authority")
    exact

private[chessjudgment] final case class CaptureExclusionMoveOrderChangedSeed private[chessjudgment] (
    referenceVacating: ReplayLegalMoveOccurrence,
    playedDeferred: ReplayLegalMoveOccurrence,
    playedReply: ReplayLegalMoveOccurrence,
    referenceDeferred: ReplayLegalMoveOccurrence,
    referenceDeferredStepIndex: Int
):
  require(referenceDeferredStepIndex >= 2 && referenceDeferredStepIndex % 2 == 0)
  require(CaptureExclusionMoveOrderChangedSeed.sameMove(playedDeferred, referenceDeferred))

  def stableKey: String =
    List(
      referenceVacating.occurrenceId,
      playedDeferred.occurrenceId,
      playedReply.occurrenceId,
      referenceDeferred.occurrenceId,
      referenceDeferredStepIndex.toString
    ).mkString("|")

private[chessjudgment] object CaptureExclusionMoveOrderChangedSeed:
  def sameMove(left: ReplayLegalMoveOccurrence, right: ReplayLegalMoveOccurrence): Boolean =
    left.movement.witness == right.movement.witness &&
      EvidenceRef.sameMove(left.movement.moveUci, right.movement.moveUci) &&
      left.movement.capture == right.movement.capture && left.movement.mode == right.movement.mode

private[chessjudgment] object CaptureExclusionMoveOrderProof:
  private final case class StateAuthority(
      role: CaptureExclusionMoveOrderStateRole,
      branch: CausalBranchOccurrence,
      stepIndex: Int,
      authority: ClosedPositionStateAuthority
  )

  def deriveChangedDependencies(
      referenceLine: LineNodeRef,
      playedLine: LineNodeRef,
      referenceLineRecord: EvidenceRecord,
      playedLineRecord: EvidenceRecord,
      referenceReplay: CanonicalLineReplay,
      playedReplay: CanonicalLineReplay,
      seeds: List[CaptureExclusionMoveOrderChangedSeed]
  ): List[CertifiedCaptureExclusionMoveOrder] =
    seeds.flatMap(seed =>
      derive(
        referenceLine,
        playedLine,
        referenceLineRecord,
        playedLineRecord,
        referenceReplay,
        playedReplay,
        seed
      )
    )

  private def derive(
      referenceLine: LineNodeRef,
      playedLine: LineNodeRef,
      referenceLineRecord: EvidenceRecord,
      playedLineRecord: EvidenceRecord,
      referenceReplay: CanonicalLineReplay,
      playedReplay: CanonicalLineReplay,
      seed: CaptureExclusionMoveOrderChangedSeed
  ): Option[CertifiedCaptureExclusionMoveOrder] =
    for
      _ <- Option.when(
        CertifiedComparedLineAuthority.exactRecord(referenceLineRecord, referenceLine, referenceReplay) &&
          CertifiedComparedLineAuthority.exactRecord(playedLineRecord, playedLine, playedReplay) &&
          referenceLineRecord.ref.position == playedLineRecord.ref.position
      )((): Unit)
      referenceRoot <- referenceReplay.replaySteps.headOption
      playedRoot <- playedReplay.replaySteps.headOption
      playedReplyStep <- playedReplay.replaySteps.lift(1)
      referenceDeferredStep <- referenceReplay.replaySteps.lift(seed.referenceDeferredStepIndex)
      if seed.referenceDeferredStepIndex >= 2 && seed.referenceDeferredStepIndex % 2 == 0
      if seed.referenceVacating.step == referenceRoot && seed.playedDeferred.step == playedRoot &&
        seed.playedReply.step == playedReplyStep && seed.referenceDeferred.step == referenceDeferredStep
      rootBoard <- PrincipalVariationEvidence.semanticBoardStateFen(referenceRoot.fenBefore)
      playedBoard <- PrincipalVariationEvidence.semanticBoardStateFen(playedRoot.fenBefore)
      if rootBoard == playedBoard
      referenceVacating <- RecordBoundLegalMoveOccurrence.certified(
        referenceLineRecord,
        seed.referenceVacating
      )
      playedDeferred <- RecordBoundLegalMoveOccurrence.certified(playedLineRecord, seed.playedDeferred)
      playedReply <- RecordBoundLegalMoveOccurrence.certified(playedLineRecord, seed.playedReply)
      referenceDeferred <- RecordBoundLegalMoveOccurrence.certified(
        referenceLineRecord,
        seed.referenceDeferred
      )
      if sameMove(playedDeferred, referenceDeferred)
      replyCapture <- playedReply.capture
      if replyCapture.capturedSquare == playedReply.movement.to
      capturedTarget = RelationColoredPieceWitness(
        replyCapture.capturedSquare,
        replyCapture.capturedRole,
        replyCapture.capturedSide
      )
      if referenceVacating.movement.from == capturedTarget.square &&
        referenceVacating.movement.side == capturedTarget.side &&
        referenceVacating.movement.beforeRole == capturedTarget.role &&
        referenceVacating.movement.side == playedDeferred.movement.side &&
        playedReply.movement.side != playedDeferred.movement.side
      referenceBranch = CausalBranchOccurrence.certifiedCounterfactual(
        ComparedLineBranchRole.CounterfactualReference,
        referenceLine,
        referenceReplay,
        seed.referenceDeferredStepIndex + 1
      )
      playedBranch = CausalBranchOccurrence.observedRootWithAnalyzedContinuation(
        ComparedLineBranchRole.PlayedRootAnalysisContinuation,
        playedLine,
        playedReplay,
        2
      )
      absenceAuthorities <- referenceAbsences(
        referenceLineRecord,
        referenceReplay,
        playedReply,
        seed.referenceDeferredStepIndex
      )
      stateAuthorities <- retainedStates(
        referenceLineRecord,
        referenceReplay,
        referenceBranch,
        playedDeferred,
        playedReply,
        capturedTarget,
        seed.referenceDeferredStepIndex
      )
    yield certify(
      rootBoard,
      referenceVacating,
      playedDeferred,
      playedReply,
      referenceDeferred,
      capturedTarget,
      referenceBranch,
      playedBranch,
      absenceAuthorities,
      stateAuthorities,
      referenceLineRecord,
      playedLineRecord,
      referenceReplay,
      playedReplay,
      seed.referenceDeferredStepIndex
    )

  private def certify(
      rootBoard: String,
      referenceVacating: RecordBoundLegalMoveOccurrence,
      playedDeferred: RecordBoundLegalMoveOccurrence,
      playedReply: RecordBoundLegalMoveOccurrence,
      referenceDeferred: RecordBoundLegalMoveOccurrence,
      capturedTarget: RelationColoredPieceWitness,
      referenceBranch: CausalBranchOccurrence,
      playedBranch: CausalBranchOccurrence,
      absenceAuthorities: List[(Int, ClosedRelationAbsenceAuthority)],
      stateAuthorities: List[StateAuthority],
      referenceLineRecord: EvidenceRecord,
      playedLineRecord: EvidenceRecord,
      referenceReplay: CanonicalLineReplay,
      playedReplay: CanonicalLineReplay,
      deferredIndex: Int
  ): CertifiedCaptureExclusionMoveOrder =
    val proposition = CaptureExclusionMoveOrderCausalAuthority.proposition(
      rootBoard,
      referenceVacating.movement,
      playedDeferred.movement,
      playedReply.movement,
      capturedTarget
    )
    val causalOccurrence = CausalOccurrenceIdentity.from(
      proposition,
      List(referenceBranch, playedBranch)
    )
    val legalUses = List(
      CausalLegalMovePremiseUse.from(
        CaptureExclusionMoveOrderPremiseRole.ReferenceVacatingMove,
        referenceVacating,
        referenceBranch,
        0
      ),
      CausalLegalMovePremiseUse.from(
        CaptureExclusionMoveOrderPremiseRole.PlayedDeferredMove,
        playedDeferred,
        playedBranch,
        0
      ),
      CausalLegalMovePremiseUse.from(
        CaptureExclusionMoveOrderPremiseRole.PlayedCaptureReply,
        playedReply,
        playedBranch,
        1
      ),
      CausalLegalMovePremiseUse.from(
        CaptureExclusionMoveOrderPremiseRole.ReferenceDeferredMove,
        referenceDeferred,
        referenceBranch,
        deferredIndex
      )
    )
    val absenceBindings = absenceAuthorities.map { case (index, authority) =>
      CausalClosedAbsenceBinding.afterStep(
        CaptureExclusionMoveOrderAbsenceRole.ReferenceCaptureReplyAbsent,
        authority,
        referenceBranch,
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
    val proofSet = BoundedCausalProofSet.from(
      proposition,
      causalOccurrence,
      List(path)
    )
    val semantic = CaptureExclusionMoveOrderSemanticProof(
      proposition,
      referenceVacating.movement,
      playedDeferred.movement,
      playedReply.movement,
      capturedTarget
    )
    val occurrence = CaptureExclusionMoveOrderOccurrence(proofSet)
    val dependencyManifest = CaptureExclusionMoveOrderDependencyManifest(
      referenceLineRecord,
      playedLineRecord,
      proofSet
    )
    val dependency = BoundedCausalDependencyFingerprint.from(dependencyManifest)
    CertifiedCaptureExclusionMoveOrder.from(
      semantic,
      occurrence,
      dependency,
      dependencyManifest,
      referenceLineRecord,
      playedLineRecord,
      referenceReplay,
      playedReplay,
      legalUses.zip(List(referenceVacating, playedDeferred, playedReply, referenceDeferred)),
      path.closedAbsenceUses.zip(absenceAuthorities.map(_._2)),
      path.closedStateUses.zip(stateAuthorities.map(_.authority))
    )

  private def referenceAbsences(
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
      referenceLineRecord: EvidenceRecord,
      referenceReplay: CanonicalLineReplay,
      referenceBranch: CausalBranchOccurrence,
      deferred: RecordBoundLegalMoveOccurrence,
      reply: RecordBoundLegalMoveOccurrence,
      target: RelationColoredPieceWitness,
      deferredIndex: Int
  ): Option[List[StateAuthority]] =
    val replyActor = coloredOrigin(reply.movement)
    val deferredActor = coloredOrigin(deferred.movement)
    val referenceStates = (0 to deferredIndex).toList.flatMap { index =>
      val exact = List(
        stateAt(
          referenceLineRecord,
          referenceReplay,
          referenceBranch,
          index,
          CaptureExclusionMoveOrderStateRole.ReferenceVacatedTarget,
          (occurrence, scope) => occurrence.existingVacancyState(target.square, scope)
        ),
        stateAt(
          referenceLineRecord,
          referenceReplay,
          referenceBranch,
          index,
          CaptureExclusionMoveOrderStateRole.ReferenceReplyActor,
          (occurrence, scope) => occurrence.existingOccupantState(replyActor, scope)
        )
      )
      exact ++ Option.when(index < deferredIndex)(
        stateAt(
          referenceLineRecord,
          referenceReplay,
          referenceBranch,
          index,
          CaptureExclusionMoveOrderStateRole.ReferenceDeferredActor,
          (occurrence, scope) => occurrence.existingOccupantState(deferredActor, scope)
        )
      )
    }
    traverse(referenceStates)(identity)

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
    left.movement == right.movement && EvidenceRef.sameMove(left.moveUci, right.moveUci) &&
      left.capture == right.capture && left.movementMode == right.movementMode

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
