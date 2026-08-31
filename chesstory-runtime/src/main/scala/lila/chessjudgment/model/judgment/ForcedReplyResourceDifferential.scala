package lila.chessjudgment.model.judgment

import lila.chessjudgment.analysis.position.PositionRelationExtractor
import lila.chessjudgment.model.line.PrincipalVariationEvidence

/** Sole family construction authority. It validates and emits the exact
  * semantic descriptor consumed by the common identity core.
  */
private[chessjudgment] object ForcedReplyResourceCausalAuthority:
  private final case class PropositionDescriptor(
      rootFen: String,
      mechanism: ForcedReplyResourceMechanism,
      trigger: RelationMoveTransitionWitness,
      forcedReply: RelationLegalMoveResourceWitness,
      realizer: RelationMoveTransitionWitness,
      capturedTarget: RelationColoredPieceWitness,
      playedDefense: RelationLegalMoveResourceWitness,
      disabledDefender: RelationColoredPieceWitness
  ) extends CausalSemanticDescriptor:
    require(mechanism.stableKey.nonEmpty, "a causal proposition needs its exact mechanism")
    val contractKind = BoundedCausalContractKind.ImmediateForcedReplyResourceDifferential
    val semanticParts: List[String] = List(
      mechanism.stableKey,
      trigger.stableKey,
      forcedReply.stableKey,
      realizer.stableKey,
      coloredPieceStableKey(capturedTarget),
      playedDefense.stableKey,
      coloredPieceStableKey(disabledDefender)
    )

  def proposition(
      rootFen: String,
      mechanism: ForcedReplyResourceMechanism,
      trigger: RelationMoveTransitionWitness,
      forcedReply: RelationLegalMoveResourceWitness,
      realizer: RelationMoveTransitionWitness,
      capturedTarget: RelationColoredPieceWitness,
      playedDefense: RelationLegalMoveResourceWitness,
      disabledDefender: RelationColoredPieceWitness
  ): CausalPropositionIdentity =
    CausalPropositionIdentity.from(
      PropositionDescriptor(
        rootFen,
        mechanism,
        trigger,
        forcedReply,
        realizer,
        capturedTarget,
        playedDefense,
        disabledDefender
      )
    )

  def coloredPieceStableKey(piece: RelationColoredPieceWitness): String =
    s"${piece.side.toString.toLowerCase}:${piece.role.name.toLowerCase}@${piece.square.key.toLowerCase}"

private[chessjudgment] enum ForcedReplyResourceBranchRole extends CausalBranchRole:
  case CounterfactualReference
  case ObservedPlayedRoot

  def stableKey: String =
    this match
      case CounterfactualReference => "counterfactual-reference"
      case ObservedPlayedRoot       => "observed-played-root"

private[chessjudgment] enum ForcedReplyResourcePremiseRole extends CausalPremiseRole:
  case CreatedCheckResponse
  case ReferenceCaptureRecapture
  case PlayedCaptureRecapture

  def stableKey: String =
    this match
      case CreatedCheckResponse       => "created-check-response"
      case ReferenceCaptureRecapture  => "reference-capture-recapture"
      case PlayedCaptureRecapture     => "played-capture-recapture"

private[chessjudgment] enum ForcedReplyResourceAbsenceRole extends CausalAbsenceRole:
  case ReferenceRecaptureAbsent

  def stableKey: String = "reference-recapture-absent"

/** The exact causal mechanism proved by this deliberately narrow contract.
  * It is part of semantic and dependency identity, not a serializer label.
  */
private[chessjudgment] enum ForcedReplyResourceMechanism:
  case ForcedDisplacement

  def stableKey: String = "forced-displacement"

  def wireCode: String = "forced_displacement"

/** Complete named lower-premise manifest for this contract. The common core
  * accepts only typed manifests; this family is the sole authority for which
  * lower occurrences and absence boundary prove its proposition.
  */
private[chessjudgment] sealed trait ForcedReplyResourceManifest
    extends BoundedCausalContractManifest:
  def mechanism: ForcedReplyResourceMechanism
  def referenceNoRecapture: CausalClosedAbsenceBinding
  final def contractKind = BoundedCausalContractKind.ImmediateForcedReplyResourceDifferential
  final def absenceBindings = List(referenceNoRecapture)
  final def stableKey: String =
    List(
      contractKind.toString.toLowerCase,
      premiseUses.map(_.stableKey).mkString("[", ",", "]"),
      referenceNoRecapture.stableKey
    ).mkString("|")

private[chessjudgment] object ForcedReplyResourceManifest:
  private final case class Displacement(
      checkResponse: CausalVerticalRelationPremiseUse,
      referenceRecapture: CausalVerticalRelationPremiseUse,
      playedRecapture: CausalVerticalRelationPremiseUse,
      referenceNoRecapture: CausalClosedAbsenceBinding
  ) extends ForcedReplyResourceManifest:
    val mechanism = ForcedReplyResourceMechanism.ForcedDisplacement
    val premiseUses = List(checkResponse, referenceRecapture, playedRecapture)

  def displacement(
      checkResponse: CausalVerticalRelationPremiseUse,
      referenceRecapture: CausalVerticalRelationPremiseUse,
      playedRecapture: CausalVerticalRelationPremiseUse,
      referenceNoRecapture: CausalClosedAbsenceBinding
  ): ForcedReplyResourceManifest =
    validateShared(checkResponse, referenceRecapture, playedRecapture, referenceNoRecapture)
    Displacement(checkResponse, referenceRecapture, playedRecapture, referenceNoRecapture)

  private def validateShared(
      checkResponse: CausalVerticalRelationPremiseUse,
      referenceRecapture: CausalVerticalRelationPremiseUse,
      playedRecapture: CausalVerticalRelationPremiseUse,
      referenceNoRecapture: CausalClosedAbsenceBinding
  ): Unit =
    require(
      checkResponse.role == ForcedReplyResourcePremiseRole.CreatedCheckResponse &&
        checkResponse.contract == VerticalRelationContractKind.CreatedCheckResponseInventory &&
        checkResponse.result.kind == RelationFactKind.CreatedCheckResponseInventory &&
        checkResponse.branchRole == ForcedReplyResourceBranchRole.CounterfactualReference &&
        checkResponse.stepIndex == 0,
      "the first premise must be the exact counterfactual root check-response inventory"
    )
    require(
      referenceRecapture.role == ForcedReplyResourcePremiseRole.ReferenceCaptureRecapture &&
        referenceRecapture.contract == VerticalRelationContractKind.CaptureRecaptureInventory &&
        referenceRecapture.result.kind == RelationFactKind.CaptureRecaptureInventory &&
        referenceRecapture.branchRole == ForcedReplyResourceBranchRole.CounterfactualReference &&
        referenceRecapture.stepIndex == 2,
      "the second premise must be the counterfactual realizer recapture inventory"
    )
    require(
      playedRecapture.role == ForcedReplyResourcePremiseRole.PlayedCaptureRecapture &&
        playedRecapture.contract == VerticalRelationContractKind.CaptureRecaptureInventory &&
        playedRecapture.result.kind == RelationFactKind.CaptureRecaptureInventory &&
        playedRecapture.branchRole == ForcedReplyResourceBranchRole.ObservedPlayedRoot &&
        playedRecapture.stepIndex == 0,
      "the third premise must be the observed played-root recapture inventory"
    )
    require(
      referenceNoRecapture.role == ForcedReplyResourceAbsenceRole.ReferenceRecaptureAbsent &&
        referenceNoRecapture.branchRole == ForcedReplyResourceBranchRole.CounterfactualReference &&
        checkResponse.branchId == referenceRecapture.branchId &&
        referenceNoRecapture.branchId == referenceRecapture.branchId &&
        referenceNoRecapture.afterStepIndex == referenceRecapture.stepIndex,
      "the closed absence must belong to the reference realizer occurrence"
    )

/** Transposition-shared meaning. Lower result ids and proof paths do not live
  * here: the same proposition may be certified by several exact occurrences
  * and independent lower-proof routes.
  */
private[chessjudgment] final case class ForcedReplyResourceSemanticProof private[chessjudgment] (
    identity: CausalPropositionIdentity,
    mechanism: ForcedReplyResourceMechanism,
    trigger: RelationMoveTransitionWitness,
    forcedReply: RelationLegalMoveResourceWitness,
    realizer: RelationMoveTransitionWitness,
    capturedTarget: RelationColoredPieceWitness,
    playedDefense: RelationLegalMoveResourceWitness,
    disabledDefender: RelationColoredPieceWitness
):
  require(
    identity.contractKind == BoundedCausalContractKind.ImmediateForcedReplyResourceDifferential,
    "a forced-reply proof needs its exact causal proposition kind"
  )
  require(
    disabledDefender.side == playedDefense.movement.side &&
      disabledDefender.role == playedDefense.movement.beforeRole &&
      disabledDefender.square == playedDefense.movement.from,
    "the resource differential must retain the exact played-branch recapturer identity"
  )
  require(
    mechanism == ForcedReplyResourceMechanism.ForcedDisplacement &&
      disabledDefender.side == forcedReply.movement.side &&
      disabledDefender.role == forcedReply.movement.beforeRole &&
      disabledDefender.square == forcedReply.movement.from,
    "the semantic proof must retain its exact forced-displacement mechanism"
  )

  def semanticId: String = identity.semanticId
  def rootBoardState: String = identity.rootPositionIdentity.value

/** One exact counterfactual-reference / observed-played-root occurrence. Each
  * branch owns its ordered provenance, while every independent proof path is
  * retained in the common proof set.
  */
private[chessjudgment] final case class ForcedReplyResourceOccurrence private[chessjudgment] (
    proofSet: BoundedCausalProofSet,
    referenceRealizerIndex: Int
):
  require(
    proofSet.proposition.contractKind ==
      BoundedCausalContractKind.ImmediateForcedReplyResourceDifferential,
    "a forced-reply occurrence needs its exact causal contract"
  )
  require(
    proofSet.occurrence.branches.size == 2 &&
      proofSet.occurrence.branch(ForcedReplyResourceBranchRole.CounterfactualReference).exists(
        _.line.role == LineNodeRole.BestReference
      ) &&
      proofSet.occurrence.branch(ForcedReplyResourceBranchRole.ObservedPlayedRoot).exists(
        _.line.role == LineNodeRole.Played
      ),
    "this contract needs one exact BestReference and observed Played branch"
  )
  require(referenceSteps.size == 3, "a forced-reply occurrence needs trigger, reply, and realizer")
  require(playedSteps.size == 2, "the immediate played occurrence needs realizer and reply")
  require(referenceRealizerIndex == 2, "the first proof family admits only its immediate realizer")

  private def exactBranch(role: ForcedReplyResourceBranchRole): CausalBranchOccurrence =
    proofSet.occurrence
      .branch(role)
      .getOrElse(throw IllegalStateException(s"a causal occurrence lost its $role branch"))

  def semanticId: String = proofSet.proposition.semanticId
  def occurrenceId: String = proofSet.occurrence.occurrenceId
  def referenceBranch: CausalBranchOccurrence =
    exactBranch(ForcedReplyResourceBranchRole.CounterfactualReference)
  def playedBranch: CausalBranchOccurrence =
    exactBranch(ForcedReplyResourceBranchRole.ObservedPlayedRoot)
  def referenceLine: LineNodeRef = referenceBranch.line
  def playedLine: LineNodeRef = playedBranch.line
  def referenceSteps: List[LineReplayStep] = referenceBranch.replaySteps
  def playedSteps: List[LineReplayStep] = playedBranch.replaySteps
  def proofPaths: List[CausalProofPathOccurrence] = proofSet.paths
  def triggerStep: LineReplayStep = referenceSteps.head
  def forcedReplyStep: LineReplayStep = referenceSteps(1)
  def realizerStep: LineReplayStep = referenceSteps(referenceRealizerIndex)
  def playedRealizerStep: LineReplayStep = playedSteps.head
  def playedReplyStep: LineReplayStep = playedSteps(1)

/** Exact invalidation input for the first contract. This is the only L2 cache
  * key owner: lower replays retain their own calculation caches, while this
  * manifest names only the records, comparison, occurrence, and proof paths
  * actually consumed by the result.
  */
private[chessjudgment] final case class ForcedReplyResourceDependencyManifest private[chessjudgment] (
    referenceLineRecord: EvidenceRecord,
    playedLineRecord: EvidenceRecord,
    demandRecord: EvidenceRecord,
    demandingComparison: CandidateComparisonFact,
    mechanism: ForcedReplyResourceMechanism,
    disabledDefender: RelationColoredPieceWitness,
    proofSet: BoundedCausalProofSet
) extends BoundedCausalDependencyManifest:
  val contractKind = BoundedCausalContractKind.ImmediateForcedReplyResourceDifferential
  require(
    proofSet.proposition.contractKind == contractKind &&
      proofSet.paths.forall(_.manifest.contractKind == contractKind),
    "a dependency manifest may contain only its exact typed causal proof paths"
  )

  val stableKey: String =
    List(
      BoundedCausalIdentity.evidenceRecordKey(referenceLineRecord),
      BoundedCausalIdentity.evidenceRecordKey(playedLineRecord),
      BoundedCausalIdentity.evidenceRecordKey(demandRecord),
      CandidateComparisonSemanticKey.from(demandingComparison).stableKey,
      java.lang.Double.toHexString(demandingComparison.comparison.winPercentLossForMover),
      mechanism.stableKey,
      ForcedReplyResourceCausalAuthority.coloredPieceStableKey(disabledDefender),
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

/** Opaque occurrence authority. It retains the admitted line replays and the
  * exact lower absence proof for every path use. No legal move, attack map,
  * ray, or pawn fact is rebuilt here.
  */
private[chessjudgment] final class ForcedReplyResourceOccurrenceProof private[chessjudgment] (
    val semantic: ForcedReplyResourceSemanticProof,
    val occurrence: ForcedReplyResourceOccurrence,
    val dependency: BoundedCausalDependencyFingerprint,
    private val dependencyManifest: ForcedReplyResourceDependencyManifest,
    private val referenceLineRecord: EvidenceRecord,
    private val playedLineRecord: EvidenceRecord,
    private val demandRecord: EvidenceRecord,
    private val demandingComparison: CandidateComparisonFact,
    private val referenceReplay: CanonicalLineReplay,
    private val playedReplay: CanonicalLineReplay,
    private val absenceAuthorities: List[
      (CausalClosedAbsenceUse, PositionRelationExtractor.ClosedRelationAbsenceProof)
    ]
):
  private val expectedAbsenceUses =
    occurrence.proofPaths.flatMap(_.closedAbsenceUses).sortBy(_.useId)
  require(
    semantic.identity == occurrence.proofSet.proposition &&
      dependencyManifest.mechanism == semantic.mechanism &&
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

  def parentSources: List[EvidenceRef] =
    List(referenceLineRecord.ref, playedLineRecord.ref, demandRecord.ref).sortBy(_.id)

  def consumesDependencies(
      comparison: CandidateComparisonFact,
      referenceSource: EvidenceRecord,
      playedSource: EvidenceRecord,
      demandSource: EvidenceRecord
  ): Boolean =
    dependencyManifest.consumes(comparison, referenceSource, playedSource, demandSource)

  def proves(record: EvidenceRecord, payload: ForcedReplyResourceDifferentialEvidence): Boolean =
    record.ref.producer == EvidenceProducer.CausalProofProducer &&
      record.ref.layer == EvidenceLayer.CausalProof &&
      record.ref.confidence == EvidenceConfidence.LegalReplayVerified &&
      record.ref.position == referenceLineRecord.ref.position &&
      record.ref.line.contains(occurrence.referenceLine) &&
      record.ref.scope == occurrence.referenceLine.role.scope &&
      record.parents == parentSources && payload.semantic == semantic &&
      payload.occurrence == occurrence && payload.dependencyFingerprint == dependency.value &&
      payload.occurrenceProof.contains(this) &&
      ForcedReplyResourceProof.exactLineRecord(
        referenceLineRecord,
        occurrence.referenceLine,
        referenceReplay
      ) &&
      ForcedReplyResourceProof.exactLineRecord(
        playedLineRecord,
        occurrence.playedLine,
        playedReplay
      ) &&
      ForcedReplyResourceProof.exactDemandRecord(
        demandRecord,
        demandingComparison,
        referenceLineRecord.ref.position,
        referenceLineRecord,
        playedLineRecord
      ) && referenceLineRecord.ref.position == playedLineRecord.ref.position &&
      demandRecord.ref.position == referenceLineRecord.ref.position &&
      referenceReplay.replaySteps.take(occurrence.referenceSteps.size) == occurrence.referenceSteps &&
      playedReplay.replaySteps.take(occurrence.playedSteps.size) == occurrence.playedSteps &&
      closedAbsencesRemainCertified

  private def closedAbsencesRemainCertified: Boolean =
    absenceAuthorities.forall { case (use, authority) =>
      val binding = use.binding
      val (exactReplay, issuerRecord) = binding.branchRole match
        case ForcedReplyResourceBranchRole.CounterfactualReference =>
          referenceReplay -> referenceLineRecord
        case ForcedReplyResourceBranchRole.ObservedPlayedRoot =>
          playedReplay -> playedLineRecord
        case _ =>
          throw IllegalStateException("a forced-reply absence used a foreign branch role")
      val certified = for
        _ <- Option.when(binding.issuerEvidenceId == issuerRecord.ref.id)((): Unit)
        branch <- occurrence.proofSet.occurrence.branch(binding.branchRole)
        if branch.branchId == binding.branchId
        stepOccurrence <- branch.stepAt(binding.afterStepIndex)
        if stepOccurrence.step.ply == binding.position.ply
        if PrincipalVariationEvidence.sameBoardState(
          stepOccurrence.step.fenAfter,
          binding.position.fen
        )
        afterAnalysis <- exactReplay.analysisAfter(stepOccurrence.step)
        issuerOccurrence <- exactReplay
          .verticalRelationOccurrences(
            stepOccurrence.step,
            List(VerticalRelationContractKind.CaptureRecaptureInventory)
          )
          .find(_.occurrenceId == binding.issuerOccurrenceId)
        reboundCapability <- issuerOccurrence.absenceCapability(
          ClosedRelationAbsencePremise(
            RelationSnapshotOccurrence.After,
            authority.query
          )
        )
        reboundAuthority <- reboundCapability.bind(binding.position, binding.scope)
        semanticBoard <- PrincipalVariationEvidence.semanticBoardStateFen(authority.position.fen)
      yield
        authority.query.stableKey == binding.queryKey && authority.position == binding.position &&
          authority.scope == binding.scope &&
          BoundedCausalIdentity.digest(
            List("closed-relation-absence:v1", semanticBoard, authority.query.stableKey)
          ) == binding.semanticProofId &&
          authority.sameOwner(binding.position, binding.scope, afterAnalysis.relationInventory) &&
          reboundAuthority.sameOwner(binding.position, binding.scope, afterAnalysis.relationInventory)
      certified.getOrElse(false)
    }

private[chessjudgment] final case class CertifiedForcedReplyResourceDifferential private[chessjudgment] (
    semantic: ForcedReplyResourceSemanticProof,
    occurrence: ForcedReplyResourceOccurrence,
    dependency: BoundedCausalDependencyFingerprint,
    proof: ForcedReplyResourceOccurrenceProof
)

private[chessjudgment] object ForcedReplyResourceProof:

  private final case class ImmediateInputs(
      rootBoard: String,
      check: RelationWitnessDetail.CreatedCheckResponseInventory,
      forcedReply: RelationLegalMoveResourceWitness,
      checkOccurrence: ReplayVerticalRelationOccurrence,
      referenceCapture: RelationWitnessDetail.CaptureRecaptureInventory,
      referenceCaptureOccurrence: ReplayVerticalRelationOccurrence,
      playedCaptureOccurrence: ReplayVerticalRelationOccurrence,
      playedDefense: RelationLegalMoveResourceWitness,
      referenceBranch: CausalBranchOccurrence,
      playedBranch: CausalBranchOccurrence,
      boundAbsence: PositionRelationExtractor.ClosedRelationAbsenceProof
  )

  private final case class MechanismBinding(
      mechanism: ForcedReplyResourceMechanism,
      disabledDefender: RelationColoredPieceWitness
  )

  def deriveImmediate(
      referenceLine: LineNodeRef,
      playedLine: LineNodeRef,
      referenceLineRecord: EvidenceRecord,
      playedLineRecord: EvidenceRecord,
      demandRecord: EvidenceRecord,
      demandingComparison: CandidateComparisonFact,
      referenceReplay: CanonicalLineReplay,
      playedReplay: CanonicalLineReplay
  ): List[CertifiedForcedReplyResourceDifferential] =
    val playedMove = playedReplay.replaySteps.headOption.map(_.moveUci)
    referenceReplay.replaySteps
      .lift(2)
      .filter(step => playedMove.exists(EvidenceRef.sameMove(_, step.moveUci)))
      .map(_ =>
        derive(
          referenceLine,
          playedLine,
          referenceLineRecord,
          playedLineRecord,
          demandRecord,
          demandingComparison,
          referenceReplay,
          playedReplay,
          referenceRealizerIndex = 2
        )
      )
      .getOrElse(Nil)

  def derive(
      referenceLine: LineNodeRef,
      playedLine: LineNodeRef,
      referenceLineRecord: EvidenceRecord,
      playedLineRecord: EvidenceRecord,
      demandRecord: EvidenceRecord,
      demandingComparison: CandidateComparisonFact,
      referenceReplay: CanonicalLineReplay,
      playedReplay: CanonicalLineReplay,
      referenceRealizerIndex: Int
  ): List[CertifiedForcedReplyResourceDifferential] =
    val inputs = for
      _ <- Option.when(exactLineRecord(referenceLineRecord, referenceLine, referenceReplay))((): Unit)
      if exactLineRecord(playedLineRecord, playedLine, playedReplay)
      if exactDemandRecord(
        demandRecord,
        demandingComparison,
        referenceLineRecord.ref.position,
        referenceLineRecord,
        playedLineRecord
      )
      if demandingComparison.referenceLine == referenceLine
      if demandingComparison.candidateLine == playedLine
      triggerStep <- referenceReplay.replaySteps.headOption
      forcedReplyStep <- referenceReplay.replaySteps.lift(1)
      realizerStep <- referenceReplay.replaySteps.lift(referenceRealizerIndex)
      playedRealizerStep <- playedReplay.replaySteps.headOption
      playedReplyStep <- playedReplay.replaySteps.lift(1)
      if referenceRealizerIndex == 2
      if EvidenceRef.sameMove(realizerStep.moveUci, playedRealizerStep.moveUci)
      if !EvidenceRef.sameMove(triggerStep.moveUci, playedRealizerStep.moveUci)
      rootBoard <- PrincipalVariationEvidence.semanticBoardStateFen(triggerStep.fenBefore)
      playedRoot <- PrincipalVariationEvidence.semanticBoardStateFen(playedRealizerStep.fenBefore)
      if rootBoard == playedRoot
      (checkResult, certifiedResponse) <- referenceReplay.exactCheckResponseMembership(
        triggerStep,
        forcedReplyStep
      )
      checkOccurrence <- uniqueVertical(
        referenceReplay,
        triggerStep,
        VerticalRelationContractKind.CreatedCheckResponseInventory
      )
      if DerivedRelationResultKey.from(checkOccurrence.relation) == checkResult
      check <- checkOccurrence.relation.detail match
        case value: RelationWitnessDetail.CreatedCheckResponseInventory => Some(value)
        case _                                                          => None
      response <- check.responses match
        case exact :: Nil if exact == certifiedResponse => Some(exact)
        case _                                           => None
      if check.terminal == RelationCheckTerminalState.Ongoing
      referenceCaptureOccurrence <- uniqueVertical(
        referenceReplay,
        realizerStep,
        VerticalRelationContractKind.CaptureRecaptureInventory
      )
      referenceCapture <- referenceCaptureOccurrence.relation.detail match
        case value: RelationWitnessDetail.CaptureRecaptureInventory => Some(value)
        case _                                                       => None
      playedCaptureOccurrence <- uniqueVertical(
        playedReplay,
        playedRealizerStep,
        VerticalRelationContractKind.CaptureRecaptureInventory
      )
      playedCapture <- playedCaptureOccurrence.relation.detail match
        case value: RelationWitnessDetail.CaptureRecaptureInventory => Some(value)
        case _                                                       => None
      (playedResult, defense) <- playedReplay.exactRecaptureMembership(
        playedRealizerStep,
        playedReplyStep
      )
      if DerivedRelationResultKey.from(playedCaptureOccurrence.relation) == playedResult
      if sameRealizerAndTarget(referenceCapture, playedCapture)
      if actorAndTargetUnchangedBeforeRealizer(
        referenceReplay,
        referenceRealizerIndex,
        referenceCapture
      )
      if playedCapture.legalRecaptures.contains(defense)
      if referenceCapture.legalRecaptures.isEmpty
      absenceQuery = PositionRelationExtractor.ClosedRelationAbsenceQuery.LegalCaptureOf(
        referenceCapture.captured.side,
        referenceCapture.mover.to
      )
      absencePremise = ClosedRelationAbsencePremise(
        RelationSnapshotOccurrence.After,
        absenceQuery
      )
      absenceCapability <- referenceCaptureOccurrence.absenceCapability(absencePremise)
      afterPosition <- referenceReplay.after(realizerStep)
      absencePosition = PositionNodeRef(
        realizerStep.fenAfter,
        realizerStep.ply,
        Some(afterPosition.color)
      )
      boundAbsence <- absenceCapability.bind(
        absencePosition,
        referenceLine.role.scope
      )
      if checkOccurrence.certifiedSourcePremiseIds.nonEmpty
      if referenceCaptureOccurrence.certifiedSourcePremiseIds.nonEmpty
      if playedCaptureOccurrence.certifiedSourcePremiseIds.nonEmpty
      referenceBranch = CausalBranchOccurrence.certifiedCounterfactual(
        ForcedReplyResourceBranchRole.CounterfactualReference,
        referenceLine,
        referenceReplay,
        referenceRealizerIndex + 1
      )
      playedBranch = CausalBranchOccurrence.observedRootWithAnalyzedContinuation(
        ForcedReplyResourceBranchRole.ObservedPlayedRoot,
        playedLine,
        playedReplay,
        2
      )
    yield ImmediateInputs(
      rootBoard,
      check,
      response.resource,
      checkOccurrence,
      referenceCapture,
      referenceCaptureOccurrence,
      playedCaptureOccurrence,
      defense,
      referenceBranch,
      playedBranch,
      boundAbsence
    )

    inputs.toList.flatMap { exact =>
      mechanismBindings(exact).map { binding =>
        certify(
          exact,
          binding,
          referenceLineRecord,
          playedLineRecord,
          demandRecord,
          demandingComparison,
          referenceReplay,
          playedReplay,
          referenceRealizerIndex
        )
      }
    }

  private def certify(
      inputs: ImmediateInputs,
      binding: MechanismBinding,
      referenceLineRecord: EvidenceRecord,
      playedLineRecord: EvidenceRecord,
      demandRecord: EvidenceRecord,
      demandingComparison: CandidateComparisonFact,
      referenceReplay: CanonicalLineReplay,
      playedReplay: CanonicalLineReplay,
      referenceRealizerIndex: Int
  ): CertifiedForcedReplyResourceDifferential =
    val proposition = ForcedReplyResourceCausalAuthority.proposition(
      inputs.rootBoard,
      binding.mechanism,
      inputs.check.mover,
      inputs.forcedReply,
      inputs.referenceCapture.mover,
      inputs.referenceCapture.captured,
      inputs.playedDefense,
      binding.disabledDefender
    )
    val referenceBranch = inputs.referenceBranch
    val playedBranch = inputs.playedBranch
    val causalOccurrence = CausalOccurrenceIdentity.from(
      proposition,
      List(referenceBranch, playedBranch)
    )
    val checkUse = CausalVerticalRelationPremiseUse.from(
      ForcedReplyResourcePremiseRole.CreatedCheckResponse,
      inputs.checkOccurrence,
      referenceBranch,
      0
    )
    val referenceRecaptureUse = CausalVerticalRelationPremiseUse.from(
      ForcedReplyResourcePremiseRole.ReferenceCaptureRecapture,
      inputs.referenceCaptureOccurrence,
      referenceBranch,
      referenceRealizerIndex
    )
    val playedRecaptureUse = CausalVerticalRelationPremiseUse.from(
      ForcedReplyResourcePremiseRole.PlayedCaptureRecapture,
      inputs.playedCaptureOccurrence,
      playedBranch,
      0
    )
    val absenceBinding = CausalClosedAbsenceBinding.afterStep(
      ForcedReplyResourceAbsenceRole.ReferenceRecaptureAbsent,
      inputs.boundAbsence,
      referenceBranch,
      referenceRealizerIndex,
      referenceLineRecord,
      inputs.referenceCaptureOccurrence
    )
    val manifest = ForcedReplyResourceManifest.displacement(
      checkUse,
      referenceRecaptureUse,
      playedRecaptureUse,
      absenceBinding
    )
    val path = CausalProofPathOccurrence.from(proposition, manifest)
    val absenceUse = path.closedAbsenceUses match
      case exact :: Nil => exact
      case other =>
        throw IllegalStateException(s"a forced-reply path retained ${other.size} absence uses")
    val proofSet = BoundedCausalProofSet.from(
      proposition,
      causalOccurrence,
      List(path)
    )
    val semantic = ForcedReplyResourceSemanticProof(
      identity = proposition,
      mechanism = binding.mechanism,
      trigger = inputs.check.mover,
      forcedReply = inputs.forcedReply,
      realizer = inputs.referenceCapture.mover,
      capturedTarget = inputs.referenceCapture.captured,
      playedDefense = inputs.playedDefense,
      disabledDefender = binding.disabledDefender
    )
    val occurrence = ForcedReplyResourceOccurrence(proofSet, referenceRealizerIndex)
    val dependencyManifest = ForcedReplyResourceDependencyManifest(
      referenceLineRecord,
      playedLineRecord,
      demandRecord,
      demandingComparison,
      semantic.mechanism,
      semantic.disabledDefender,
      proofSet
    )
    val dependency = BoundedCausalDependencyFingerprint.from(dependencyManifest)
    val proof = new ForcedReplyResourceOccurrenceProof(
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
      List(absenceUse -> inputs.boundAbsence)
    )
    CertifiedForcedReplyResourceDifferential(semantic, occurrence, dependency, proof)

  private def mechanismBindings(inputs: ImmediateInputs): List[MechanismBinding] =
    Option.when(
      sameInitialPiece(inputs.forcedReply.movement, inputs.playedDefense.movement)
    )(
      MechanismBinding(
        ForcedReplyResourceMechanism.ForcedDisplacement,
        coloredPieceAtStart(inputs.forcedReply.movement)
      )
    ).toList

  private def uniqueVertical(
      replay: CanonicalLineReplay,
      step: LineReplayStep,
      contract: VerticalRelationContractKind
  ): Option[ReplayVerticalRelationOccurrence] =
    replay.verticalRelationOccurrences(step, List(contract)) match
      case exact :: Nil => Some(exact)
      case _            => None

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
      reference: RelationWitnessDetail.CaptureRecaptureInventory,
      played: RelationWitnessDetail.CaptureRecaptureInventory
  ): Boolean =
    reference.mover == played.mover && reference.captured == played.captured

  private def actorAndTargetUnchangedBeforeRealizer(
      replay: CanonicalLineReplay,
      referenceRealizerIndex: Int,
      capture: RelationWitnessDetail.CaptureRecaptureInventory
  ): Boolean =
    val protectedSquares = Set(
      capture.mover.from.key.toLowerCase,
      capture.captured.square.key.toLowerCase
    )
    replay.replaySteps.take(referenceRealizerIndex).forall(step =>
      replay.transition(step).exists(transition =>
        transition.boardFootprint.changedSquareSet.forall(square =>
          !protectedSquares(square.key.toLowerCase)
        )
      )
    )

  private[chessjudgment] def exactLineRecord(
      source: EvidenceRecord,
      line: LineNodeRef,
      replay: CanonicalLineReplay
  ): Boolean =
    source match
      case EvidenceRecord(ref, payload: LineFactEvidence, _) =>
        ref.producer == EvidenceProducer.LegalLineProducer && ref.layer == EvidenceLayer.Line &&
          ref.confidence == EvidenceConfidence.LegalReplayVerified && ref.line.contains(line) &&
          ref.scope == line.role.scope && payload.line == line && payload.certifiedReplay.contains(replay)
      case _ => false

  private[chessjudgment] def exactDemandRecord(
      source: EvidenceRecord,
      comparison: CandidateComparisonFact,
      root: PositionNodeRef,
      referenceSource: EvidenceRecord,
      playedSource: EvidenceRecord
  ): Boolean =
    WrongMoveOrderCausalProofDemand.acceptsRecord(
      source,
      comparison,
      root,
      referenceSource,
      playedSource
    )
