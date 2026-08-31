package lila.chessjudgment.model.judgment

import lila.chessjudgment.analysis.position.PositionRelationExtractor
import lila.chessjudgment.model.line.PrincipalVariationEvidence

private[chessjudgment] enum DefenseObligationChangeMechanism:
  case SoleRecapturerRemoval

  def stableKey: String = "sole-recapturer-removal"
  def wireCode: String = "sole_recapturer_removal"

private[chessjudgment] enum DefenseObligationChangeBranchRole extends CausalBranchRole:
  case CounterfactualReference
  case ObservedPlayedRoot

  def stableKey: String =
    this match
      case CounterfactualReference => "counterfactual-reference"
      case ObservedPlayedRoot       => "observed-played-root"

private[chessjudgment] enum DefenseObligationChangePremiseRole extends CausalPremiseRole:
  case ReferenceDefenderRemoval
  case ReferenceLaterExploitInventory
  case PlayedImmediateExploitInventory

  def stableKey: String =
    this match
      case ReferenceDefenderRemoval          => "reference-defender-removal"
      case ReferenceLaterExploitInventory    => "reference-later-exploit-inventory"
      case PlayedImmediateExploitInventory   => "played-immediate-exploit-inventory"

private[chessjudgment] enum DefenseObligationChangeAbsenceRole extends CausalAbsenceRole:
  case ReferenceReplacementRecaptureAbsent

  def stableKey: String = "reference-replacement-recapture-absent"

/** Sole family authority for the semantic identity. Its private descriptor is
  * constructed only after exact L1 membership and absence have been checked.
  */
private[chessjudgment] object DefenseObligationChangeCausalAuthority:
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

    val contractKind = BoundedCausalContractKind.DefenseObligationChange
    val semanticParts: List[String] = List(
      DefenseObligationChangeMechanism.SoleRecapturerRemoval.stableKey,
      remover.stableKey,
      coloredPieceStableKey(removedDefender),
      removalRecapture.stableKey,
      exploit.stableKey,
      coloredPieceStableKey(capturedTarget),
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

  def coloredPieceStableKey(piece: RelationColoredPieceWitness): String =
    s"${piece.side.toString.toLowerCase}:${piece.role.name.toLowerCase}@${piece.square.key.toLowerCase}"

  private def sameInitialPiece(
      piece: RelationColoredPieceWitness,
      movement: RelationMoveTransitionWitness
  ): Boolean =
    piece.side == movement.side && piece.square == movement.from && piece.role == movement.beforeRole

private[chessjudgment] sealed trait DefenseObligationChangeManifest
    extends BoundedCausalContractManifest:
  def referenceNoRecapture: CausalClosedAbsenceBinding
  final def contractKind = BoundedCausalContractKind.DefenseObligationChange
  final def absenceBindings = List(referenceNoRecapture)
  final def stableKey: String =
    List(
      contractKind.toString.toLowerCase,
      DefenseObligationChangeMechanism.SoleRecapturerRemoval.stableKey,
      premiseUses.map(_.stableKey).mkString("[", ",", "]"),
      referenceNoRecapture.stableKey
    ).mkString("|")

private[chessjudgment] object DefenseObligationChangeManifest:
  private final case class SoleRecapturerRemoval(
      referenceRemoval: CausalVerticalRelationPremiseUse,
      referenceExploit: CausalVerticalRelationPremiseUse,
      playedExploit: CausalVerticalRelationPremiseUse,
      referenceNoRecapture: CausalClosedAbsenceBinding
  ) extends DefenseObligationChangeManifest:
    val premiseUses = List(referenceRemoval, referenceExploit, playedExploit)

  def soleRecapturerRemoval(
      referenceRemoval: CausalVerticalRelationPremiseUse,
      referenceExploit: CausalVerticalRelationPremiseUse,
      playedExploit: CausalVerticalRelationPremiseUse,
      referenceNoRecapture: CausalClosedAbsenceBinding
  ): DefenseObligationChangeManifest =
    require(
      referenceRemoval.role == DefenseObligationChangePremiseRole.ReferenceDefenderRemoval &&
        referenceRemoval.contract == VerticalRelationContractKind.CaptureRecaptureInventory &&
        referenceRemoval.result.kind == RelationFactKind.CaptureRecaptureInventory &&
        referenceRemoval.branchRole == DefenseObligationChangeBranchRole.CounterfactualReference &&
        referenceRemoval.stepIndex == 0,
      "defender removal must consume the reference root capture inventory"
    )
    require(
      referenceExploit.role == DefenseObligationChangePremiseRole.ReferenceLaterExploitInventory &&
        referenceExploit.contract == VerticalRelationContractKind.CaptureRecaptureInventory &&
        referenceExploit.result.kind == RelationFactKind.CaptureRecaptureInventory &&
        referenceExploit.branchRole == DefenseObligationChangeBranchRole.CounterfactualReference &&
        referenceExploit.branchId == referenceRemoval.branchId &&
        referenceExploit.stepIndex == 2,
      "the realized exploit must consume the reference third-ply capture inventory"
    )
    require(
      playedExploit.role == DefenseObligationChangePremiseRole.PlayedImmediateExploitInventory &&
        playedExploit.contract == VerticalRelationContractKind.CaptureRecaptureInventory &&
        playedExploit.result.kind == RelationFactKind.CaptureRecaptureInventory &&
        playedExploit.branchRole == DefenseObligationChangeBranchRole.ObservedPlayedRoot &&
        playedExploit.stepIndex == 0,
      "the failed exploit must consume the played root capture inventory"
    )
    require(
      referenceNoRecapture.role == DefenseObligationChangeAbsenceRole.ReferenceReplacementRecaptureAbsent &&
        referenceNoRecapture.branchRole == DefenseObligationChangeBranchRole.CounterfactualReference &&
        referenceNoRecapture.branchId == referenceExploit.branchId &&
        referenceNoRecapture.afterStepIndex == referenceExploit.stepIndex,
      "the closed recapture absence must belong to the reference exploit occurrence"
    )
    SoleRecapturerRemoval(referenceRemoval, referenceExploit, playedExploit, referenceNoRecapture)

private[chessjudgment] final case class DefenseObligationChangeSemanticProof private[chessjudgment] (
    identity: CausalPropositionIdentity,
    mechanism: DefenseObligationChangeMechanism,
    remover: RelationMoveTransitionWitness,
    removedDefender: RelationColoredPieceWitness,
    removalRecapture: RelationLegalMoveResourceWitness,
    exploit: RelationMoveTransitionWitness,
    capturedTarget: RelationColoredPieceWitness,
    playedRecapture: RelationLegalMoveResourceWitness
):
  require(
    identity.contractKind == BoundedCausalContractKind.DefenseObligationChange &&
      mechanism == DefenseObligationChangeMechanism.SoleRecapturerRemoval,
    "a defense-obligation proof needs its exact causal contract and mechanism"
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

private[chessjudgment] final case class DefenseObligationChangeOccurrence private[chessjudgment] (
    proofSet: BoundedCausalProofSet
):
  require(
    proofSet.proposition.contractKind == BoundedCausalContractKind.DefenseObligationChange,
    "a defense-obligation occurrence needs its exact causal contract"
  )
  require(
    proofSet.occurrence.branches.size == 2 &&
      proofSet.occurrence.branch(DefenseObligationChangeBranchRole.CounterfactualReference).exists(
        _.line.role == LineNodeRole.BestReference
      ) &&
      proofSet.occurrence.branch(DefenseObligationChangeBranchRole.ObservedPlayedRoot).exists(
        _.line.role == LineNodeRole.Played
      ),
    "a defense-obligation occurrence needs one BestReference and one Played branch"
  )
  require(referenceSteps.size == 3, "the reference occurrence needs removal, reply, and exploit")
  require(playedSteps.size == 2, "the played occurrence needs exploit and recapture")

  private def exactBranch(role: DefenseObligationChangeBranchRole): CausalBranchOccurrence =
    proofSet.occurrence
      .branch(role)
      .getOrElse(throw IllegalStateException(s"a defense-obligation occurrence lost its $role branch"))

  def semanticId: String = proofSet.proposition.semanticId
  def occurrenceId: String = proofSet.occurrence.occurrenceId
  def referenceBranch: CausalBranchOccurrence = exactBranch(DefenseObligationChangeBranchRole.CounterfactualReference)
  def playedBranch: CausalBranchOccurrence = exactBranch(DefenseObligationChangeBranchRole.ObservedPlayedRoot)
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

private[chessjudgment] final case class DefenseObligationChangeDependencyManifest private[chessjudgment] (
    referenceLineRecord: EvidenceRecord,
    playedLineRecord: EvidenceRecord,
    demandRecord: EvidenceRecord,
    demandingComparison: CandidateComparisonFact,
    removedDefender: RelationColoredPieceWitness,
    proofSet: BoundedCausalProofSet
) extends BoundedCausalDependencyManifest:
  val contractKind = BoundedCausalContractKind.DefenseObligationChange
  require(
    proofSet.proposition.contractKind == contractKind &&
      proofSet.paths.forall(_.manifest.contractKind == contractKind),
    "a dependency manifest may contain only defense-obligation proof paths"
  )

  val stableKey: String =
    List(
      BoundedCausalIdentity.evidenceRecordKey(referenceLineRecord),
      BoundedCausalIdentity.evidenceRecordKey(playedLineRecord),
      BoundedCausalIdentity.evidenceRecordKey(demandRecord),
      CandidateComparisonSemanticKey.from(demandingComparison).stableKey,
      java.lang.Double.toHexString(demandingComparison.comparison.winPercentLossForMover),
      DefenseObligationChangeMechanism.SoleRecapturerRemoval.stableKey,
      DefenseObligationChangeCausalAuthority.coloredPieceStableKey(removedDefender),
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

/** Opaque certified family result and sole graph-record proof authority. */
private[chessjudgment] final class CertifiedDefenseObligationChange private (
    val semantic: DefenseObligationChangeSemanticProof,
    val occurrence: DefenseObligationChangeOccurrence,
    val dependency: BoundedCausalDependencyFingerprint,
    private val dependencyManifest: DefenseObligationChangeDependencyManifest,
    private val referenceLineRecord: EvidenceRecord,
    private val playedLineRecord: EvidenceRecord,
    private val demandRecord: EvidenceRecord,
    private val demandingComparison: CandidateComparisonFact,
    private val referenceReplay: CanonicalLineReplay,
    private val playedReplay: CanonicalLineReplay,
    private val absenceUse: CausalClosedAbsenceUse,
    private val absenceAuthority: PositionRelationExtractor.ClosedRelationAbsenceProof
):
  require(
    semantic.identity == occurrence.proofSet.proposition &&
      dependencyManifest.removedDefender == semantic.removedDefender &&
      dependencyManifest.proofSet == occurrence.proofSet &&
      BoundedCausalDependencyFingerprint.from(dependencyManifest) == dependency,
    "a certified defense-obligation result needs one proposition and complete dependency manifest"
  )
  require(
    occurrence.proofPaths.flatMap(_.closedAbsenceUses) == List(absenceUse),
    "the sole-recapturer path needs its exact closed absence use"
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

  def proves(record: EvidenceRecord, payload: DefenseObligationChangeEvidence): Boolean =
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
    DefenseObligationChangeProof.exactLineRecord(
      referenceLineRecord,
      occurrence.referenceLine,
      referenceReplay
    ) &&
      DefenseObligationChangeProof.exactLineRecord(
        playedLineRecord,
        occurrence.playedLine,
        playedReplay
      ) &&
      DefenseObligationChangeProof.exactDemandRecord(
        demandRecord,
        demandingComparison,
        referenceLineRecord.ref.position,
        referenceLineRecord,
        playedLineRecord
      ) &&
      referenceLineRecord.ref.position == playedLineRecord.ref.position &&
      demandRecord.ref.position == referenceLineRecord.ref.position &&
      referenceReplay.replaySteps.take(3) == occurrence.referenceSteps &&
      playedReplay.replaySteps.take(2) == occurrence.playedSteps &&
      semanticIdentityRemainsCertified && captureSemanticsRemainCertified &&
      manifestRemainsCertified && premisesRemainCertified && absenceRemainsCertified

  private def semanticIdentityRemainsCertified: Boolean =
    semantic.mechanism == DefenseObligationChangeMechanism.SoleRecapturerRemoval &&
      DefenseObligationChangeCausalAuthority.proposition(
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
        path.manifest.isInstanceOf[DefenseObligationChangeManifest] &&
          (path.premiseUses match
            case removal :: referenceExploit :: playedExploit :: Nil =>
              removal.role == DefenseObligationChangePremiseRole.ReferenceDefenderRemoval &&
                removal.contract == VerticalRelationContractKind.CaptureRecaptureInventory &&
                removal.result.kind == RelationFactKind.CaptureRecaptureInventory &&
                removal.branchId == occurrence.referenceBranch.branchId && removal.stepIndex == 0 &&
                referenceExploit.role ==
                  DefenseObligationChangePremiseRole.ReferenceLaterExploitInventory &&
                referenceExploit.contract == VerticalRelationContractKind.CaptureRecaptureInventory &&
                referenceExploit.result.kind == RelationFactKind.CaptureRecaptureInventory &&
                referenceExploit.branchId == occurrence.referenceBranch.branchId &&
                referenceExploit.stepIndex == 2 &&
                playedExploit.role ==
                  DefenseObligationChangePremiseRole.PlayedImmediateExploitInventory &&
                playedExploit.contract == VerticalRelationContractKind.CaptureRecaptureInventory &&
                playedExploit.result.kind == RelationFactKind.CaptureRecaptureInventory &&
                playedExploit.branchId == occurrence.playedBranch.branchId && playedExploit.stepIndex == 0 &&
                List(removal, referenceExploit, playedExploit).forall(_.sourcePremiseIds.nonEmpty)
            case _ => false) &&
          path.closedAbsenceUses == List(absenceUse) &&
          absenceUse.binding.role ==
            DefenseObligationChangeAbsenceRole.ReferenceReplacementRecaptureAbsent &&
          absenceUse.binding.branchId == occurrence.referenceBranch.branchId &&
          absenceUse.binding.afterStepIndex == 2 &&
          parentSources.map(_.id).distinct.size == 3 &&
          dependencyManifest.consumes(
            demandingComparison,
            referenceLineRecord,
            playedLineRecord,
            demandRecord
          )
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
      val replay = Option.when(
        use.branchRole == DefenseObligationChangeBranchRole.CounterfactualReference
      )(referenceReplay).orElse(
        Option.when(use.branchRole == DefenseObligationChangeBranchRole.ObservedPlayedRoot)(playedReplay)
      )
      replay.exists(exactReplay =>
        occurrence.proofSet.occurrence.branch(use.branchRole).exists(branch =>
          branch.branchId == use.branchId && branch.stepAt(use.stepIndex).exists(stepOccurrence =>
            exactReplay
              .verticalRelationOccurrences(stepOccurrence.step, List(use.contract))
              .exists(issuer =>
                DerivedRelationResultKey.from(issuer.relation) == use.result &&
                  issuer.certifiedSourcePremiseIds == use.sourcePremiseIds
              )
          )
        )
      )
    }

  private def absenceRemainsCertified: Boolean =
    val binding = absenceUse.binding
    val certified = for
      _ <- Option.when(
        binding.branchRole == DefenseObligationChangeBranchRole.CounterfactualReference &&
          binding.issuerEvidenceId == referenceLineRecord.ref.id
      )(())
      branch <- occurrence.proofSet.occurrence.branch(binding.branchRole)
      if branch.branchId == binding.branchId
      stepOccurrence <- branch.stepAt(binding.afterStepIndex)
      if stepOccurrence.step.ply == binding.position.ply
      if PrincipalVariationEvidence.sameBoardState(stepOccurrence.step.fenAfter, binding.position.fen)
      issuerOccurrence <- referenceReplay.positionAfter(stepOccurrence.step)
      if issuerOccurrence.occurrenceId == binding.issuerOccurrenceId
      reboundAuthority <- issuerOccurrence.closedAbsence(absenceAuthority.query, binding.scope)
      semanticBoard <- PrincipalVariationEvidence.semanticBoardStateFen(absenceAuthority.position.fen)
    yield
      absenceAuthority.query.stableKey == binding.queryKey &&
        absenceAuthority.position == binding.position && absenceAuthority.scope == binding.scope &&
        BoundedCausalIdentity.digest(
          List("closed-relation-absence:v1", semanticBoard, absenceAuthority.query.stableKey)
        ) == binding.semanticProofId &&
        issuerOccurrence.certifies(absenceAuthority, binding.scope) &&
        issuerOccurrence.certifies(reboundAuthority, binding.scope)
    certified.getOrElse(false)

private[chessjudgment] object CertifiedDefenseObligationChange:
  private[judgment] def from(
      semantic: DefenseObligationChangeSemanticProof,
      occurrence: DefenseObligationChangeOccurrence,
      dependency: BoundedCausalDependencyFingerprint,
      dependencyManifest: DefenseObligationChangeDependencyManifest,
      referenceLineRecord: EvidenceRecord,
      playedLineRecord: EvidenceRecord,
      demandRecord: EvidenceRecord,
      demandingComparison: CandidateComparisonFact,
      referenceReplay: CanonicalLineReplay,
      playedReplay: CanonicalLineReplay,
      absenceUse: CausalClosedAbsenceUse,
      absenceAuthority: PositionRelationExtractor.ClosedRelationAbsenceProof
  ): CertifiedDefenseObligationChange =
    val certificate = new CertifiedDefenseObligationChange(
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
      absenceUse,
      absenceAuthority
    )
    require(
      certificate.remainsCertified,
      "a defense-obligation certificate must retain its exact semantic, lower, and absence authorities"
    )
    certificate

private[chessjudgment] object DefenseObligationChangeProof:
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
      absenceOccurrence: ReplayPositionOccurrence,
      boundAbsence: PositionRelationExtractor.ClosedRelationAbsenceProof
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
  ): List[CertifiedDefenseObligationChange] =
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
      rootCaptureOccurrence <- uniqueCaptureOccurrence(referenceReplay, removalStep)
      rootCapture <- captureDetail(rootCaptureOccurrence)
      (rootCaptureResult, removalRecapture) <- referenceReplay.exactRecaptureMembership(
        removalStep,
        removalRecaptureStep
      )
      if DerivedRelationResultKey.from(rootCaptureOccurrence.relation) == rootCaptureResult
      if rootCapture.legalRecaptures == List(removalRecapture)
      if rootCapture.captured.square == rootCapture.mover.to
      referenceExploitOccurrence <- uniqueCaptureOccurrence(referenceReplay, referenceExploitStep)
      referenceExploit <- captureDetail(referenceExploitOccurrence)
      playedExploitOccurrence <- uniqueCaptureOccurrence(playedReplay, playedExploitStep)
      playedExploit <- captureDetail(playedExploitOccurrence)
      (playedExploitResult, playedRecapture) <- playedReplay.exactRecaptureMembership(
        playedExploitStep,
        playedRecaptureStep
      )
      if DerivedRelationResultKey.from(playedExploitOccurrence.relation) == playedExploitResult
      if playedExploit.legalRecaptures == List(playedRecapture)
      if sameExploit(referenceExploit, playedExploit)
      if actorAndTargetUnchangedBeforeExploit(referenceReplay, referenceExploit)
      if sameInitialPiece(rootCapture.captured, playedRecapture.movement)
      if referenceExploit.legalRecaptures.isEmpty
      absenceQuery = PositionRelationExtractor.ClosedRelationAbsenceQuery.LegalCaptureOf(
        rootCapture.captured.side,
        referenceExploit.mover.to
      )
      absenceOccurrence <- referenceReplay.positionAfter(referenceExploitStep)
      boundAbsence <- absenceOccurrence.closedAbsence(absenceQuery, referenceLine.role.scope)
      if rootCaptureOccurrence.certifiedSourcePremiseIds.nonEmpty
      if referenceExploitOccurrence.certifiedSourcePremiseIds.nonEmpty
      if playedExploitOccurrence.certifiedSourcePremiseIds.nonEmpty
      referenceBranch = CausalBranchOccurrence.certifiedCounterfactual(
        DefenseObligationChangeBranchRole.CounterfactualReference,
        referenceLine,
        referenceReplay,
        3
      )
      playedBranch = CausalBranchOccurrence.observedRootWithAnalyzedContinuation(
        DefenseObligationChangeBranchRole.ObservedPlayedRoot,
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
      absenceOccurrence,
      boundAbsence
    )

    inputs.toList.map(exact =>
      certify(
        exact,
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
  ): CertifiedDefenseObligationChange =
    val proposition = DefenseObligationChangeCausalAuthority.proposition(
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
      DefenseObligationChangePremiseRole.ReferenceDefenderRemoval,
      inputs.rootCaptureOccurrence,
      inputs.referenceBranch,
      0
    )
    val referenceExploitUse = CausalVerticalRelationPremiseUse.from(
      DefenseObligationChangePremiseRole.ReferenceLaterExploitInventory,
      inputs.referenceExploitOccurrence,
      inputs.referenceBranch,
      2
    )
    val playedExploitUse = CausalVerticalRelationPremiseUse.from(
      DefenseObligationChangePremiseRole.PlayedImmediateExploitInventory,
      inputs.playedExploitOccurrence,
      inputs.playedBranch,
      0
    )
    val absenceBinding = CausalClosedAbsenceBinding.afterStep(
      DefenseObligationChangeAbsenceRole.ReferenceReplacementRecaptureAbsent,
      inputs.boundAbsence,
      inputs.referenceBranch,
      2,
      referenceLineRecord,
      inputs.absenceOccurrence
    )
    val manifest = DefenseObligationChangeManifest.soleRecapturerRemoval(
      referenceRemovalUse,
      referenceExploitUse,
      playedExploitUse,
      absenceBinding
    )
    val path = CausalProofPathOccurrence.from(proposition, manifest)
    val absenceUse = path.closedAbsenceUses match
      case exact :: Nil => exact
      case other => throw IllegalStateException(s"a defense-obligation path retained ${other.size} absences")
    val proofSet = BoundedCausalProofSet.from(
      proposition,
      causalOccurrence,
      List(path)
    )
    val semantic = DefenseObligationChangeSemanticProof(
      proposition,
      DefenseObligationChangeMechanism.SoleRecapturerRemoval,
      inputs.rootCapture.mover,
      inputs.rootCapture.captured,
      inputs.removalRecapture,
      inputs.referenceExploit.mover,
      inputs.referenceExploit.captured,
      inputs.playedRecapture
    )
    val occurrence = DefenseObligationChangeOccurrence(proofSet)
    val dependencyManifest = DefenseObligationChangeDependencyManifest(
      referenceLineRecord,
      playedLineRecord,
      demandRecord,
      demandingComparison,
      semantic.removedDefender,
      proofSet
    )
    val dependency = BoundedCausalDependencyFingerprint.from(dependencyManifest)
    CertifiedDefenseObligationChange.from(
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
      absenceUse,
      inputs.boundAbsence
    )

  private def uniqueCaptureOccurrence(
      replay: CanonicalLineReplay,
      step: LineReplayStep
  ): Option[ReplayVerticalRelationOccurrence] =
    replay.verticalRelationOccurrences(
      step,
      List(VerticalRelationContractKind.CaptureRecaptureInventory)
    ) match
      case exact :: Nil => Some(exact)
      case _            => None

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

  private def actorAndTargetUnchangedBeforeExploit(
      replay: CanonicalLineReplay,
      capture: RelationWitnessDetail.CaptureRecaptureInventory
  ): Boolean =
    val protectedSquares = Set(
      capture.mover.from.key.toLowerCase,
      capture.captured.square.key.toLowerCase
    )
    replay.replaySteps.take(2).forall(step =>
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
