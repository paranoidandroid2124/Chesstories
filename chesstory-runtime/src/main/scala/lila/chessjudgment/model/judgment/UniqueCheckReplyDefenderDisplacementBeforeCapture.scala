package lila.chessjudgment.model.judgment

import lila.chessjudgment.analysis.position.PositionRelationExtractor
import lila.chessjudgment.model.line.PrincipalVariationEvidence

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
      playedDefense: RelationLegalMoveResourceWitness,
      disabledDefender: RelationColoredPieceWitness
  ) extends CausalSemanticDescriptor:
    val contractKind = BoundedCausalContractKind.UniqueCheckReplyDefenderDisplacementBeforeCapture
    val semanticParts: List[String] = List(
      trigger.stableKey,
      forcedReply.stableKey,
      realizer.stableKey,
      coloredPieceStableKey(capturedTarget),
      playedDefense.stableKey,
      coloredPieceStableKey(disabledDefender)
    )

  def proposition(
      rootFen: String,
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

private[chessjudgment] enum UniqueCheckReplyDefenderDisplacementBeforeCaptureBranchRole extends CausalBranchRole:
  case CounterfactualReference
  case ObservedPlayedRoot

  def stableKey: String =
    this match
      case CounterfactualReference => "counterfactual-reference"
      case ObservedPlayedRoot       => "observed-played-root"

private[chessjudgment] enum UniqueCheckReplyDefenderDisplacementBeforeCapturePremiseRole extends CausalPremiseRole:
  case CreatedCheckResponse
  case ReferenceCaptureRecapture
  case PlayedCaptureRecapture

  def stableKey: String =
    this match
      case CreatedCheckResponse       => "created-check-response"
      case ReferenceCaptureRecapture  => "reference-capture-recapture"
      case PlayedCaptureRecapture     => "played-capture-recapture"

private[chessjudgment] enum UniqueCheckReplyDefenderDisplacementBeforeCaptureAbsenceRole extends CausalAbsenceRole:
  case ReferenceRecaptureAbsent

  def stableKey: String = "reference-recapture-absent"

/** Complete named lower-premise manifest for this contract. The common core
  * accepts only typed manifests; this family is the sole authority for which
  * lower occurrences and absence boundary prove its proposition.
  */
private[chessjudgment] sealed trait UniqueCheckReplyDefenderDisplacementBeforeCaptureManifest
    extends BoundedCausalContractManifest:
  def referenceNoRecapture: CausalClosedAbsenceBinding
  final def contractKind = BoundedCausalContractKind.UniqueCheckReplyDefenderDisplacementBeforeCapture
  final def absenceBindings = List(referenceNoRecapture)
  final def stableKey: String =
    List(
      contractKind.toString.toLowerCase,
      premiseUses.map(_.stableKey).mkString("[", ",", "]"),
      referenceNoRecapture.stableKey
    ).mkString("|")

private[chessjudgment] object UniqueCheckReplyDefenderDisplacementBeforeCaptureManifest:
  private final case class Exact(
      checkResponse: CausalVerticalRelationPremiseUse,
      referenceRecapture: CausalVerticalRelationPremiseUse,
      playedRecapture: CausalVerticalRelationPremiseUse,
      referenceNoRecapture: CausalClosedAbsenceBinding
  ) extends UniqueCheckReplyDefenderDisplacementBeforeCaptureManifest:
    val premiseUses = List(checkResponse, referenceRecapture, playedRecapture)

  def exact(
      checkResponse: CausalVerticalRelationPremiseUse,
      referenceRecapture: CausalVerticalRelationPremiseUse,
      playedRecapture: CausalVerticalRelationPremiseUse,
      referenceNoRecapture: CausalClosedAbsenceBinding
  ): UniqueCheckReplyDefenderDisplacementBeforeCaptureManifest =
    validateShared(checkResponse, referenceRecapture, playedRecapture, referenceNoRecapture)
    Exact(checkResponse, referenceRecapture, playedRecapture, referenceNoRecapture)

  private def validateShared(
      checkResponse: CausalVerticalRelationPremiseUse,
      referenceRecapture: CausalVerticalRelationPremiseUse,
      playedRecapture: CausalVerticalRelationPremiseUse,
      referenceNoRecapture: CausalClosedAbsenceBinding
  ): Unit =
    require(
      checkResponse.role == UniqueCheckReplyDefenderDisplacementBeforeCapturePremiseRole.CreatedCheckResponse &&
        checkResponse.contract == VerticalRelationContractKind.CreatedCheckResponseInventory &&
        checkResponse.result.kind == RelationFactKind.CreatedCheckResponseInventory &&
        checkResponse.branchRole == UniqueCheckReplyDefenderDisplacementBeforeCaptureBranchRole.CounterfactualReference &&
        checkResponse.stepIndex == 0,
      "the first premise must be the exact counterfactual root check-response inventory"
    )
    require(
      referenceRecapture.role == UniqueCheckReplyDefenderDisplacementBeforeCapturePremiseRole.ReferenceCaptureRecapture &&
        referenceRecapture.contract == VerticalRelationContractKind.CaptureRecaptureInventory &&
        referenceRecapture.result.kind == RelationFactKind.CaptureRecaptureInventory &&
        referenceRecapture.branchRole == UniqueCheckReplyDefenderDisplacementBeforeCaptureBranchRole.CounterfactualReference &&
        referenceRecapture.stepIndex == 2,
      "the second premise must be the counterfactual realizer recapture inventory"
    )
    require(
      playedRecapture.role == UniqueCheckReplyDefenderDisplacementBeforeCapturePremiseRole.PlayedCaptureRecapture &&
        playedRecapture.contract == VerticalRelationContractKind.CaptureRecaptureInventory &&
        playedRecapture.result.kind == RelationFactKind.CaptureRecaptureInventory &&
        playedRecapture.branchRole == UniqueCheckReplyDefenderDisplacementBeforeCaptureBranchRole.ObservedPlayedRoot &&
        playedRecapture.stepIndex == 0,
      "the third premise must be the observed played-root recapture inventory"
    )
    require(
      referenceNoRecapture.role == UniqueCheckReplyDefenderDisplacementBeforeCaptureAbsenceRole.ReferenceRecaptureAbsent &&
        referenceNoRecapture.branchRole == UniqueCheckReplyDefenderDisplacementBeforeCaptureBranchRole.CounterfactualReference &&
        checkResponse.branchId == referenceRecapture.branchId &&
        referenceNoRecapture.branchId == referenceRecapture.branchId &&
        referenceNoRecapture.afterStepIndex == referenceRecapture.stepIndex,
      "the closed absence must belong to the reference realizer occurrence"
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
    playedDefense: RelationLegalMoveResourceWitness,
    disabledDefender: RelationColoredPieceWitness
):
  require(
    identity.contractKind == BoundedCausalContractKind.UniqueCheckReplyDefenderDisplacementBeforeCapture,
    "a unique-check-reply defender-displacement proof needs its exact causal proposition kind"
  )
  require(
    disabledDefender.side == playedDefense.movement.side &&
      disabledDefender.role == playedDefense.movement.beforeRole &&
      disabledDefender.square == playedDefense.movement.from,
    "the unique-check-reply defender-displacement proof must retain the exact played-branch recapturer identity"
  )
  require(
    disabledDefender.side == forcedReply.movement.side &&
      disabledDefender.role == forcedReply.movement.beforeRole &&
      disabledDefender.square == forcedReply.movement.from,
    "the semantic proof must retain the exact displaced defender"
  )

  def semanticId: String = identity.semanticId
  def rootBoardState: String = identity.rootPositionIdentity.value

/** One exact counterfactual-reference / observed-played-root occurrence. Each
  * branch owns its ordered provenance, while every independent proof path is
  * retained in the common proof set.
  */
private[chessjudgment] final case class UniqueCheckReplyDefenderDisplacementBeforeCaptureOccurrence private[chessjudgment] (
    proofSet: BoundedCausalProofSet,
    referenceRealizerIndex: Int
):
  require(
    proofSet.proposition.contractKind ==
      BoundedCausalContractKind.UniqueCheckReplyDefenderDisplacementBeforeCapture,
    "a unique-check-reply defender-displacement occurrence needs its exact causal contract"
  )
  require(
    proofSet.occurrence.branches.size == 2 &&
      proofSet.occurrence.branch(UniqueCheckReplyDefenderDisplacementBeforeCaptureBranchRole.CounterfactualReference).exists(
        _.line.role == LineNodeRole.BestReference
      ) &&
      proofSet.occurrence.branch(UniqueCheckReplyDefenderDisplacementBeforeCaptureBranchRole.ObservedPlayedRoot).exists(
        _.line.role == LineNodeRole.Played
      ),
    "this contract needs one exact BestReference and observed Played branch"
  )
  require(
    referenceSteps.size == 3,
    "a unique-check-reply defender-displacement occurrence needs trigger, reply, and realizer"
  )
  require(playedSteps.size == 2, "the immediate played occurrence needs realizer and reply")
  require(referenceRealizerIndex == 2, "the first proof family admits only its immediate realizer")

  private def exactBranch(role: UniqueCheckReplyDefenderDisplacementBeforeCaptureBranchRole): CausalBranchOccurrence =
    proofSet.occurrence
      .branch(role)
      .getOrElse(throw IllegalStateException(s"a causal occurrence lost its $role branch"))

  def semanticId: String = proofSet.proposition.semanticId
  def occurrenceId: String = proofSet.occurrence.occurrenceId
  def referenceBranch: CausalBranchOccurrence =
    exactBranch(UniqueCheckReplyDefenderDisplacementBeforeCaptureBranchRole.CounterfactualReference)
  def playedBranch: CausalBranchOccurrence =
    exactBranch(UniqueCheckReplyDefenderDisplacementBeforeCaptureBranchRole.ObservedPlayedRoot)
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
private[chessjudgment] final case class UniqueCheckReplyDefenderDisplacementBeforeCaptureDependencyManifest private[chessjudgment] (
    referenceLineRecord: EvidenceRecord,
    playedLineRecord: EvidenceRecord,
    demandRecord: EvidenceRecord,
    demandingComparison: CandidateComparisonFact,
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
      BoundedCausalIdentity.evidenceRecordKey(referenceLineRecord),
      BoundedCausalIdentity.evidenceRecordKey(playedLineRecord),
      BoundedCausalIdentity.evidenceRecordKey(demandRecord),
      CandidateComparisonSemanticKey.from(demandingComparison).stableKey,
      UniqueCheckReplyDefenderDisplacementBeforeCaptureCausalAuthority.coloredPieceStableKey(disabledDefender),
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
private[chessjudgment] final class UniqueCheckReplyDefenderDisplacementBeforeCaptureOccurrenceProof private[chessjudgment] (
    val semantic: UniqueCheckReplyDefenderDisplacementBeforeCaptureSemanticProof,
    val occurrence: UniqueCheckReplyDefenderDisplacementBeforeCaptureOccurrence,
    val dependency: BoundedCausalDependencyFingerprint,
    private val dependencyManifest: UniqueCheckReplyDefenderDisplacementBeforeCaptureDependencyManifest,
    private val referenceLineRecord: EvidenceRecord,
    private val playedLineRecord: EvidenceRecord,
    private val demandRecord: EvidenceRecord,
    private val demandingComparison: CandidateComparisonFact,
    private val referenceReplay: CanonicalLineReplay,
    private val playedReplay: CanonicalLineReplay,
    private val absenceAuthorities: List[
      (CausalClosedAbsenceUse, ClosedRelationAbsenceAuthority)
    ]
):
  private val expectedAbsenceUses =
    occurrence.proofPaths.flatMap(_.closedAbsenceUses).sortBy(_.useId)
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

  def proves(record: EvidenceRecord, payload: UniqueCheckReplyDefenderDisplacementBeforeCaptureEvidence): Boolean =
    record.ref.producer == EvidenceProducer.CausalProofProducer &&
      record.ref.layer == EvidenceLayer.CausalProof &&
      record.ref.confidence == EvidenceConfidence.LegalReplayVerified &&
      record.ref.position == referenceLineRecord.ref.position &&
      record.ref.line.contains(occurrence.referenceLine) &&
      record.ref.scope == occurrence.referenceLine.role.scope &&
      record.parents == parentSources && payload.semantic == semantic &&
      payload.occurrence == occurrence && payload.dependencyFingerprint == dependency.value &&
      payload.occurrenceProof.contains(this) &&
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
      UniqueCheckReplyDefenderDisplacementBeforeCaptureProof.exactDemandRecord(
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
      val issuerRecord = binding.branchRole match
        case UniqueCheckReplyDefenderDisplacementBeforeCaptureBranchRole.CounterfactualReference =>
          referenceLineRecord
        case UniqueCheckReplyDefenderDisplacementBeforeCaptureBranchRole.ObservedPlayedRoot =>
          playedLineRecord
        case _ =>
          throw IllegalStateException(
            "a unique-check-reply defender-displacement absence used a foreign branch role"
          )
      binding.authority == authority && authority.issuerRecord == issuerRecord &&
        authority.remainsCertified &&
        occurrence.proofSet.occurrence.branch(binding.branchRole).exists(branch =>
          branch.branchId == binding.branchId &&
            branch.stepAt(binding.afterStepIndex).exists(stepOccurrence =>
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
private[chessjudgment] final case class UniqueCheckReplyDefenderDisplacementBeforeCaptureChangedSeed private[chessjudgment] (
    checkOccurrence: ReplayVerticalRelationOccurrence,
    forcedReply: RelationCheckResponseWitness,
    referenceExploitOccurrence: ReplayVerticalRelationOccurrence,
    playedExploitOccurrence: ReplayVerticalRelationOccurrence,
    playedRecapture: RelationLegalMoveResourceWitness
):
  require(
    checkOccurrence.contract == VerticalRelationContractKind.CreatedCheckResponseInventory &&
      referenceExploitOccurrence.contract == VerticalRelationContractKind.CaptureRecaptureInventory &&
      playedExploitOccurrence.contract == VerticalRelationContractKind.CaptureRecaptureInventory,
    "a unique-check-reply defender-displacement seed must retain its exact check and capture L1 occurrences"
  )

  private[chessjudgment] def stableKey: String =
    List(
      checkOccurrence.occurrenceId,
      forcedReply.stableKey,
      referenceExploitOccurrence.occurrenceId,
      playedExploitOccurrence.occurrenceId,
      playedRecapture.stableKey
    ).mkString("|")

private[chessjudgment] object UniqueCheckReplyDefenderDisplacementBeforeCaptureProof:

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
      absenceAuthority: ClosedRelationAbsenceAuthority
  )

  def deriveImmediate(
      referenceLine: LineNodeRef,
      playedLine: LineNodeRef,
      referenceLineRecord: EvidenceRecord,
      playedLineRecord: EvidenceRecord,
      demandRecord: EvidenceRecord,
      demandingComparison: CandidateComparisonFact,
      referenceReplay: CanonicalLineReplay,
      playedReplay: CanonicalLineReplay,
      changedSeed: UniqueCheckReplyDefenderDisplacementBeforeCaptureChangedSeed
  ): List[CertifiedUniqueCheckReplyDefenderDisplacementBeforeCapture] =
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
          referenceRealizerIndex = 2,
          changedSeed = changedSeed
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
      referenceRealizerIndex: Int,
      changedSeed: UniqueCheckReplyDefenderDisplacementBeforeCaptureChangedSeed
  ): List[CertifiedUniqueCheckReplyDefenderDisplacementBeforeCapture] =
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
      checkOccurrence = changedSeed.checkOccurrence
      if checkOccurrence.step == triggerStep
      check <- checkOccurrence.relation.detail match
        case value: RelationWitnessDetail.CreatedCheckResponseInventory => Some(value)
        case _                                                          => None
      response <- check.responses match
        case exact :: Nil if exact == changedSeed.forcedReply => Some(exact)
        case _                                           => None
      if EvidenceRef.sameMove(response.resource.moveUci, forcedReplyStep.moveUci)
      if check.terminal == RelationCheckTerminalState.Ongoing
      referenceCaptureOccurrence = changedSeed.referenceExploitOccurrence
      if referenceCaptureOccurrence.step == realizerStep
      referenceCapture <- referenceCaptureOccurrence.relation.detail match
        case value: RelationWitnessDetail.CaptureRecaptureInventory => Some(value)
        case _                                                       => None
      playedCaptureOccurrence = changedSeed.playedExploitOccurrence
      if playedCaptureOccurrence.step == playedRealizerStep
      playedCapture <- playedCaptureOccurrence.relation.detail match
        case value: RelationWitnessDetail.CaptureRecaptureInventory => Some(value)
        case _                                                       => None
      defense = changedSeed.playedRecapture
      if EvidenceRef.sameMove(defense.moveUci, playedReplyStep.moveUci)
      if sameRealizerAndTarget(referenceCapture, playedCapture)
      if actorAndTargetUnchangedBeforeRealizer(
        referenceReplay,
        referenceRealizerIndex,
        referenceCapture
      )
      if playedCapture.legalRecaptures == List(defense)
      if referenceCapture.legalRecaptures.isEmpty
      absenceQuery = PositionRelationExtractor.ClosedRelationAbsenceQuery.LegalCaptureOf(
        referenceCapture.captured.side,
        referenceCapture.mover.to
      )
      absenceOccurrence <- referenceReplay.positionAfter(realizerStep)
      absenceAuthority <- ClosedRelationAbsenceAuthority.forQuery(
        referenceLineRecord,
        absenceOccurrence,
        absenceQuery
      )
      if checkOccurrence.certifiedSourcePremiseIds.nonEmpty
      if referenceCaptureOccurrence.certifiedSourcePremiseIds.nonEmpty
      if playedCaptureOccurrence.certifiedSourcePremiseIds.nonEmpty
      referenceBranch = CausalBranchOccurrence.certifiedCounterfactual(
        UniqueCheckReplyDefenderDisplacementBeforeCaptureBranchRole.CounterfactualReference,
        referenceLine,
        referenceReplay,
        referenceRealizerIndex + 1
      )
      playedBranch = CausalBranchOccurrence.observedRootWithAnalyzedContinuation(
        UniqueCheckReplyDefenderDisplacementBeforeCaptureBranchRole.ObservedPlayedRoot,
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
      absenceAuthority
    )

    inputs.toList.flatMap { exact =>
      disabledDefender(exact).map { defender =>
        certify(
          exact,
          defender,
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
      disabledDefender: RelationColoredPieceWitness,
      referenceLineRecord: EvidenceRecord,
      playedLineRecord: EvidenceRecord,
      demandRecord: EvidenceRecord,
      demandingComparison: CandidateComparisonFact,
      referenceReplay: CanonicalLineReplay,
      playedReplay: CanonicalLineReplay,
      referenceRealizerIndex: Int
  ): CertifiedUniqueCheckReplyDefenderDisplacementBeforeCapture =
    val proposition = UniqueCheckReplyDefenderDisplacementBeforeCaptureCausalAuthority.proposition(
      inputs.rootBoard,
      inputs.check.mover,
      inputs.forcedReply,
      inputs.referenceCapture.mover,
      inputs.referenceCapture.captured,
      inputs.playedDefense,
      disabledDefender
    )
    val referenceBranch = inputs.referenceBranch
    val playedBranch = inputs.playedBranch
    val causalOccurrence = CausalOccurrenceIdentity.from(
      proposition,
      List(referenceBranch, playedBranch)
    )
    val checkUse = CausalVerticalRelationPremiseUse.from(
      UniqueCheckReplyDefenderDisplacementBeforeCapturePremiseRole.CreatedCheckResponse,
      RecordBoundVerticalRelationOccurrence.certified(referenceLineRecord, inputs.checkOccurrence).getOrElse(
        throw IllegalArgumentException("forced reply lost its graph-owned check-response occurrence")
      ),
      referenceBranch,
      0
    )
    val referenceRecaptureUse = CausalVerticalRelationPremiseUse.from(
      UniqueCheckReplyDefenderDisplacementBeforeCapturePremiseRole.ReferenceCaptureRecapture,
      RecordBoundVerticalRelationOccurrence.certified(referenceLineRecord, inputs.referenceCaptureOccurrence).getOrElse(
        throw IllegalArgumentException("forced reply lost its graph-owned reference capture occurrence")
      ),
      referenceBranch,
      referenceRealizerIndex
    )
    val playedRecaptureUse = CausalVerticalRelationPremiseUse.from(
      UniqueCheckReplyDefenderDisplacementBeforeCapturePremiseRole.PlayedCaptureRecapture,
      RecordBoundVerticalRelationOccurrence.certified(playedLineRecord, inputs.playedCaptureOccurrence).getOrElse(
        throw IllegalArgumentException("forced reply lost its graph-owned played capture occurrence")
      ),
      playedBranch,
      0
    )
    val absenceBinding = CausalClosedAbsenceBinding.afterStep(
      UniqueCheckReplyDefenderDisplacementBeforeCaptureAbsenceRole.ReferenceRecaptureAbsent,
      inputs.absenceAuthority,
      referenceBranch,
      referenceRealizerIndex
    )
    val manifest = UniqueCheckReplyDefenderDisplacementBeforeCaptureManifest.exact(
      checkUse,
      referenceRecaptureUse,
      playedRecaptureUse,
      absenceBinding
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
      realizer = inputs.referenceCapture.mover,
      capturedTarget = inputs.referenceCapture.captured,
      playedDefense = inputs.playedDefense,
      disabledDefender = disabledDefender
    )
    val occurrence = UniqueCheckReplyDefenderDisplacementBeforeCaptureOccurrence(proofSet, referenceRealizerIndex)
    val dependencyManifest = UniqueCheckReplyDefenderDisplacementBeforeCaptureDependencyManifest(
      referenceLineRecord,
      playedLineRecord,
      demandRecord,
      demandingComparison,
      semantic.disabledDefender,
      proofSet
    )
    val dependency = BoundedCausalDependencyFingerprint.from(dependencyManifest)
    val proof = new UniqueCheckReplyDefenderDisplacementBeforeCaptureOccurrenceProof(
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
      List(absenceUse -> inputs.absenceAuthority)
    )
    CertifiedUniqueCheckReplyDefenderDisplacementBeforeCapture(semantic, occurrence, dependency, proof)

  private def disabledDefender(inputs: ImmediateInputs): Option[RelationColoredPieceWitness] =
    Option.when(
      sameInitialPiece(inputs.forcedReply.movement, inputs.playedDefense.movement)
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

  private[chessjudgment] def exactDemandRecord(
      source: EvidenceRecord,
      comparison: CandidateComparisonFact,
      root: PositionNodeRef,
      referenceSource: EvidenceRecord,
      playedSource: EvidenceRecord
  ): Boolean =
    ActionablePlayedVsBestCausalProofDemand.acceptsRecord(
      source,
      comparison,
      root,
      referenceSource,
      playedSource
    )
