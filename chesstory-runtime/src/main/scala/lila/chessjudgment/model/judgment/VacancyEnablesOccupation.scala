package lila.chessjudgment.model.judgment

import lila.chessjudgment.analysis.position.PositionRelationExtractor
import lila.chessjudgment.model.line.PrincipalVariationEvidence

private[chessjudgment] enum VacancyOccupationPremiseRole extends CausalPremiseRole:
  case ReferenceReleaseMove
  case ReferenceOccupationMove

  def stableKey: String =
    this match
      case ReferenceReleaseMove    => "reference-release-move"
      case ReferenceOccupationMove => "reference-occupation-move"

private[chessjudgment] enum VacancyOccupationAbsenceRole extends CausalAbsenceRole:
  case PlayedOccupationMoveAbsent

  def stableKey: String = "played-occupation-move-absent"

private[chessjudgment] enum VacancyOccupationStateRole extends CausalStateRole:
  case ReferenceVacancy
  case ReferenceOccupation
  case PlayedBlocker
  case PlayedOccupier

  def stableKey: String =
    this match
      case ReferenceVacancy    => "reference-vacancy"
      case ReferenceOccupation => "reference-occupation"
      case PlayedBlocker        => "played-blocker"
      case PlayedOccupier       => "played-occupier"

private[chessjudgment] object VacancyOccupationCausalAuthority:
  private final case class Descriptor(
      rootFen: String,
      release: RelationMoveTransitionWitness,
      blocker: RelationColoredPieceWitness,
      occupation: RelationMoveTransitionWitness,
      occupier: RelationColoredPieceWitness
  ) extends CausalSemanticDescriptor:
    require(
      blocker == RelationColoredPieceWitness(release.from, release.beforeRole, release.side),
      "a vacancy proposition needs the exact released blocker"
    )
    require(
      occupation.side == release.side && occupation.to == release.from &&
        occupier == RelationColoredPieceWitness(
          occupation.from,
          occupation.beforeRole,
          occupation.side
        ),
      "an occupation proposition needs an exact same-side movement into the released square"
    )

    val contractKind = BoundedCausalContractKind.VacancyEnablesOccupation
    val semanticParts: List[String] = List(
      release.stableKey,
      BoundedCausalIdentity.coloredPieceKey(blocker),
      occupation.stableKey,
      BoundedCausalIdentity.coloredPieceKey(occupier)
    )

  def proposition(
      rootFen: String,
      release: RelationMoveTransitionWitness,
      blocker: RelationColoredPieceWitness,
      occupation: RelationMoveTransitionWitness,
      occupier: RelationColoredPieceWitness
  ): CausalPropositionIdentity =
    CausalPropositionIdentity.from(
      Descriptor(rootFen, release, blocker, occupation, occupier)
    )

private[chessjudgment] sealed trait VacancyOccupationManifest
    extends BoundedCausalContractManifest:
  def releaseMove: CausalLegalMovePremiseUse
  def occupationMove: CausalLegalMovePremiseUse
  def playedMoveAbsent: CausalClosedAbsenceBinding
  def referenceVacancies: List[CausalClosedStateBinding]
  def referenceOccupation: CausalClosedStateBinding
  def playedBlocker: CausalClosedStateBinding
  def playedOccupier: CausalClosedStateBinding

  final val contractKind = BoundedCausalContractKind.VacancyEnablesOccupation
  final val premiseUses: List[CausalVerticalRelationPremiseUse] = Nil
  final val absenceBindings = List(playedMoveAbsent)
  final override val stateBindings =
    referenceVacancies ++ List(referenceOccupation, playedBlocker, playedOccupier)
  final override val supplementalPremiseUses: List[CausalSupplementalPremiseUse] =
    List(releaseMove, occupationMove)
  final def stableKey: String =
    List(
      contractKind.toString.toLowerCase,
      supplementalPremiseUses.map(_.stableKey).mkString("[", ",", "]"),
      absenceBindings.map(_.stableKey).mkString("[", ",", "]"),
      stateBindings.map(_.stableKey).mkString("[", ",", "]")
    ).mkString("|")

private[chessjudgment] object VacancyOccupationManifest:
  private final case class Exact(
      releaseMove: CausalLegalMovePremiseUse,
      occupationMove: CausalLegalMovePremiseUse,
      playedMoveAbsent: CausalClosedAbsenceBinding,
      referenceVacancies: List[CausalClosedStateBinding],
      referenceOccupation: CausalClosedStateBinding,
      playedBlocker: CausalClosedStateBinding,
      playedOccupier: CausalClosedStateBinding
  ) extends VacancyOccupationManifest

  def exact(
      releaseMove: CausalLegalMovePremiseUse,
      occupationMove: CausalLegalMovePremiseUse,
      playedMoveAbsent: CausalClosedAbsenceBinding,
      referenceVacancies: List[CausalClosedStateBinding],
      referenceOccupation: CausalClosedStateBinding,
      playedBlocker: CausalClosedStateBinding,
      playedOccupier: CausalClosedStateBinding
  ): VacancyOccupationManifest =
    val occupationIndex = occupationMove.stepIndex
    val referenceRole = ComparedLineBranchRole.CounterfactualReference
    val playedRole = ComparedLineBranchRole.PlayedRootAnalysisContinuation
    require(
      releaseMove.role == VacancyOccupationPremiseRole.ReferenceReleaseMove &&
        releaseMove.branchRole == referenceRole && releaseMove.stepIndex == 0,
      "a vacancy proof needs the exact reference root release movement"
    )
    require(
      occupationIndex >= 2 &&
        occupationMove.role == VacancyOccupationPremiseRole.ReferenceOccupationMove &&
        occupationMove.branchRole == referenceRole &&
        occupationMove.branchId == releaseMove.branchId &&
        occupationMove.capture.isEmpty &&
        occupationMove.movement.side == releaseMove.movement.side &&
        occupationMove.movement.to == releaseMove.movement.from,
      "a vacancy proof needs a later non-capture occupation of the exact released square"
    )
    require(
      playedMoveAbsent.role == VacancyOccupationAbsenceRole.PlayedOccupationMoveAbsent &&
        playedMoveAbsent.branchRole == playedRole &&
        playedMoveAbsent.afterStepIndex == occupationIndex - 1,
      "a vacancy proof needs the Played sibling's exact pre-occupation move absence"
    )
    require(
      referenceVacancies.size == occupationIndex &&
        referenceVacancies.zipWithIndex.forall { case (binding, stepIndex) =>
          binding.role == VacancyOccupationStateRole.ReferenceVacancy &&
          binding.branchRole == referenceRole && binding.branchId == releaseMove.branchId &&
          binding.afterStepIndex == stepIndex &&
          binding.query == PositionRelationExtractor.ClosedPositionStateQuery.Vacant(
            releaseMove.movement.from
          )
        },
      "a vacancy proof needs the released square vacant after every pre-occupation occurrence"
    )
    require(
      referenceOccupation.role == VacancyOccupationStateRole.ReferenceOccupation &&
        referenceOccupation.branchRole == referenceRole &&
        referenceOccupation.branchId == releaseMove.branchId &&
        referenceOccupation.afterStepIndex == occupationIndex,
      "a vacancy proof needs the exact reference post-occupation state"
    )
    require(
      List(playedBlocker, playedOccupier).forall(binding =>
        binding.branchRole == playedRole && binding.branchId == playedMoveAbsent.branchId &&
          binding.afterStepIndex == occupationIndex - 1
      ) && playedBlocker.role == VacancyOccupationStateRole.PlayedBlocker &&
        playedOccupier.role == VacancyOccupationStateRole.PlayedOccupier,
      "a vacancy proof needs exact Played blocker and occupier states"
    )
    Exact(
      releaseMove,
      occupationMove,
      playedMoveAbsent,
      referenceVacancies,
      referenceOccupation,
      playedBlocker,
      playedOccupier
    )

private[chessjudgment] final case class VacancyEnablesOccupationSemanticProof private[chessjudgment] (
    identity: CausalPropositionIdentity,
    release: RelationMoveTransitionWitness,
    blocker: RelationColoredPieceWitness,
    occupation: RelationMoveTransitionWitness,
    occupier: RelationColoredPieceWitness
):
  require(identity.contractKind == BoundedCausalContractKind.VacancyEnablesOccupation)
  require(
    blocker == RelationColoredPieceWitness(release.from, release.beforeRole, release.side) &&
      occupation.side == release.side && occupation.to == release.from &&
      occupier == RelationColoredPieceWitness(
        occupation.from,
        occupation.beforeRole,
        occupation.side
      ),
    "a vacancy semantic proof must retain exact releaser, square, and occupier identities"
  )

  def semanticId: String = identity.semanticId
  def rootBoardState: String = identity.rootPositionIdentity.value

private[chessjudgment] final case class VacancyEnablesOccupationOccurrence private[chessjudgment] (
    proofSet: BoundedCausalProofSet
):
  require(proofSet.proposition.contractKind == BoundedCausalContractKind.VacancyEnablesOccupation)
  require(
    proofSet.occurrence.branches.size == 2 &&
      proofSet.occurrence.branch(ComparedLineBranchRole.CounterfactualReference).exists(
        _.line.role == LineNodeRole.BestReference
      ) && proofSet.occurrence.branch(ComparedLineBranchRole.PlayedRootAnalysisContinuation).exists(
        _.line.role == LineNodeRole.Played
      ),
    "a vacancy occurrence needs one BestReference and one Played branch"
  )
  require(referenceSteps.size >= 3 && playedSteps.size == referenceSteps.size - 1)

  private def exactBranch(role: ComparedLineBranchRole): CausalBranchOccurrence =
    proofSet.occurrence.branch(role).getOrElse(
      throw IllegalStateException(s"a vacancy occurrence lost its $role branch")
    )

  def semanticId: String = proofSet.proposition.semanticId
  def occurrenceId: String = proofSet.occurrence.occurrenceId
  def referenceBranch: CausalBranchOccurrence =
    exactBranch(ComparedLineBranchRole.CounterfactualReference)
  def playedBranch: CausalBranchOccurrence =
    exactBranch(ComparedLineBranchRole.PlayedRootAnalysisContinuation)
  def referenceLine: LineNodeRef = referenceBranch.line
  def playedLine: LineNodeRef = playedBranch.line
  def referenceSteps: List[LineReplayStep] = referenceBranch.replaySteps
  def playedSteps: List[LineReplayStep] = playedBranch.replaySteps
  def releaseStep: LineReplayStep = referenceSteps.head
  def occupationStep: LineReplayStep = referenceSteps.last
  def occupationStepIndex: Int = referenceSteps.size - 1
  def playedPreOccupationStep: LineReplayStep = playedSteps.last
  def proofPaths: List[CausalProofPathOccurrence] = proofSet.paths

private[chessjudgment] final case class VacancyOccupationDependencyManifest private[chessjudgment] (
    referenceLineRecord: EvidenceRecord,
    playedLineRecord: EvidenceRecord,
    proofSet: BoundedCausalProofSet
) extends BoundedCausalDependencyManifest:
  val contractKind = BoundedCausalContractKind.VacancyEnablesOccupation
  require(
    proofSet.proposition.contractKind == contractKind &&
      proofSet.paths.forall(_.manifest.contractKind == contractKind),
    "a vacancy dependency may contain only its exact typed proof paths"
  )

  val stableKey: String =
    List(
      BoundedCausalIdentity.evidenceRecordKey(referenceLineRecord),
      BoundedCausalIdentity.evidenceRecordKey(playedLineRecord),
      contractKind.toString.toLowerCase,
      proofSet.proposition.semanticId,
      proofSet.occurrence.occurrenceId,
      proofSet.paths.map(_.pathOccurrenceId).mkString("[", ",", "]")
    ).mkString("|")

  def consumes(reference: EvidenceRecord, played: EvidenceRecord): Boolean =
    referenceLineRecord == reference && playedLineRecord == played

private[chessjudgment] final class CertifiedVacancyEnablesOccupation private[chessjudgment] (
    val semantic: VacancyEnablesOccupationSemanticProof,
    val occurrence: VacancyEnablesOccupationOccurrence,
    val dependency: BoundedCausalDependencyFingerprint,
    private val dependencyManifest: VacancyOccupationDependencyManifest,
    private val referenceLineRecord: EvidenceRecord,
    private val playedLineRecord: EvidenceRecord,
    private val referenceReplay: CanonicalLineReplay,
    private val playedReplay: CanonicalLineReplay,
    private val releaseAuthority: RecordBoundLegalMoveOccurrence,
    private val occupationAuthority: RecordBoundLegalMoveOccurrence,
    private val vacancyClosure: VacancySiblingClosure,
    private val absenceAuthority: ClosedRelationAbsenceAuthority,
    private val stateAuthorities: List[ClosedPositionStateAuthority]
):
  require(
    semantic.identity == occurrence.proofSet.proposition &&
      dependencyManifest.proofSet == occurrence.proofSet &&
      BoundedCausalDependencyFingerprint.from(dependencyManifest) == dependency,
    "a certified vacancy result needs one proposition and complete dependency manifest"
  )

  def parentSources: List[EvidenceRef] =
    List(referenceLineRecord.ref, playedLineRecord.ref).sortBy(_.id)

  private[chessjudgment] def lowerIssuerRecords: List[EvidenceRecord] =
    List(referenceLineRecord, playedLineRecord)

  def consumesDependencies(reference: EvidenceRecord, played: EvidenceRecord): Boolean =
    dependencyManifest.consumes(reference, played)

  def proves(record: EvidenceRecord, payload: VacancyEnablesOccupationEvidence): Boolean =
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
    ) && CertifiedComparedLineAuthority.exactRecord(
      playedLineRecord,
      occurrence.playedLine,
      playedReplay
    ) && referenceLineRecord.ref.position == playedLineRecord.ref.position &&
      referenceReplay.replaySteps.take(occurrence.referenceSteps.size) == occurrence.referenceSteps &&
      playedReplay.replaySteps.take(occurrence.playedSteps.size) == occurrence.playedSteps &&
      lowerMovesRemainCertified && semanticRemainsCertified && pathRemainsCertified &&
      closuresRemainCertified

  private def lowerMovesRemainCertified: Boolean =
    referenceReplay.legalMoveOccurrence(occurrence.releaseStep)
      .flatMap(RecordBoundLegalMoveOccurrence.certified(referenceLineRecord, _))
      .contains(releaseAuthority) &&
      referenceReplay.legalMoveOccurrence(occurrence.occupationStep)
        .flatMap(RecordBoundLegalMoveOccurrence.certified(referenceLineRecord, _))
        .contains(occupationAuthority)

  private def semanticRemainsCertified: Boolean =
    VacancyOccupationCausalAuthority.proposition(
      semantic.rootBoardState,
      semantic.release,
      semantic.blocker,
      semantic.occupation,
      semantic.occupier
    ) == semantic.identity

  private def pathRemainsCertified: Boolean =
    occurrence.proofPaths match
      case path :: Nil =>
        path.manifest match
          case manifest: VacancyOccupationManifest =>
            val expectedRelease = CausalLegalMovePremiseUse.from(
              VacancyOccupationPremiseRole.ReferenceReleaseMove,
              releaseAuthority,
              occurrence.referenceBranch,
              0
            )
            val expectedOccupation = CausalLegalMovePremiseUse.from(
              VacancyOccupationPremiseRole.ReferenceOccupationMove,
              occupationAuthority,
              occurrence.referenceBranch,
              occurrence.occupationStepIndex
            )
            manifest.releaseMove == expectedRelease &&
              manifest.occupationMove == expectedOccupation &&
              path.closedAbsenceUses.map(_.binding.authority) == List(absenceAuthority) &&
              path.closedStateUses.map(_.binding.authority) == stateAuthorities &&
              vacancyClosure.matches(
                semantic.release,
                occurrence.releaseStep.moveUci,
                semantic.blocker,
                semantic.occupation,
                occurrence.occupationStep.moveUci,
                semantic.occupier,
                occurrence.referenceBranch,
                occurrence.playedBranch,
                occurrence.occupationStepIndex,
                manifest.playedBlocker,
                manifest.playedOccupier,
                manifest.playedMoveAbsent
              ) && dependencyManifest.consumes(referenceLineRecord, playedLineRecord)
          case _ => false
      case _ => false

  private def closuresRemainCertified: Boolean =
    absenceAuthority.remainsCertified && stateAuthorities.forall(authority =>
      authority.occurrence.certifies(authority.proof, authority.scope) &&
        authority.occurrence.closedState(authority.query, authority.scope)
          .exists(authority.occurrence.certifies(_, authority.scope))
    )

private[chessjudgment] final case class VacancyEnablesOccupationChangedSeed private[chessjudgment] (
    releaseOccurrence: ReplayLegalMoveOccurrence,
    occupationOccurrence: ReplayLegalMoveOccurrence,
    occupationStepIndex: Int
):
  require(occupationStepIndex >= 2, "a vacancy seed needs a later occupation occurrence")

  private[chessjudgment] def stableKey: String =
    List(
      f"$occupationStepIndex%08d",
      releaseOccurrence.occurrenceId,
      occupationOccurrence.occurrenceId
    ).mkString("|")

private[chessjudgment] object VacancyEnablesOccupationProof:
  private final case class ExactInputs(
      rootBoard: String,
      release: RecordBoundLegalMoveOccurrence,
      occupation: RecordBoundLegalMoveOccurrence,
      blocker: RelationColoredPieceWitness,
      occupier: RelationColoredPieceWitness,
      referenceBranch: CausalBranchOccurrence,
      playedBranch: CausalBranchOccurrence,
      playedAbsenceAuthority: ClosedRelationAbsenceAuthority,
      referenceVacancyAuthorities: List[ClosedPositionStateAuthority],
      referenceOccupationAuthority: ClosedPositionStateAuthority,
      playedBlockerAuthority: ClosedPositionStateAuthority,
      playedOccupierAuthority: ClosedPositionStateAuthority
  )

  private[chessjudgment] def deriveChangedDependencies(
      referenceLine: LineNodeRef,
      playedLine: LineNodeRef,
      referenceLineRecord: EvidenceRecord,
      playedLineRecord: EvidenceRecord,
      referenceReplay: CanonicalLineReplay,
      playedReplay: CanonicalLineReplay,
      changedSeeds: List[VacancyEnablesOccupationChangedSeed]
  ): List[CertifiedVacancyEnablesOccupation] =
    val seedKeys = changedSeeds.map(_.stableKey)
    require(
      seedKeys == seedKeys.distinct.sorted,
      "vacancy demand needs unique canonical exact occurrence seeds"
    )
    val admitted =
      CertifiedComparedLineAuthority.exactRecord(
        referenceLineRecord,
        referenceLine,
        referenceReplay
      ) && CertifiedComparedLineAuthority.exactRecord(
        playedLineRecord,
        playedLine,
        playedReplay
      ) && referenceLineRecord.ref.position == playedLineRecord.ref.position &&
        !EvidenceRef.sameMove(referenceLine.rootMove, playedLine.rootMove)
    if !admitted then Nil
    else
      val referenceSteps = referenceReplay.replaySteps
      val playedSteps = playedReplay.replaySteps
      val vacancyPrefix = referenceSteps.headOption
        .flatMap(referenceReplay.legalMoveOccurrence)
        .map(root => continuousVacancyPrefix(referenceLineRecord, referenceSteps, root.movement.from))
        .getOrElse(Nil)
      val exactInputs = for
        releaseStep <- referenceSteps.headOption.toList
        rootBoard <- PrincipalVariationEvidence.semanticBoardStateFen(releaseStep.fenBefore).toList
        playedRoot <- playedSteps.headOption.toList
        if PrincipalVariationEvidence.sameBoardState(releaseStep.fenBefore, playedRoot.fenBefore)
        seed <- changedSeeds
        occupationStep <- referenceSteps.lift(seed.occupationStepIndex).toList
        if seed.releaseOccurrence.step == releaseStep &&
          seed.occupationOccurrence.step == occupationStep
        releaseOccurrence <- referenceReplay.legalMoveOccurrence(releaseStep).toList
        occupationOccurrence <- referenceReplay.legalMoveOccurrence(occupationStep).toList
        if releaseOccurrence == seed.releaseOccurrence &&
          occupationOccurrence == seed.occupationOccurrence
        release <- RecordBoundLegalMoveOccurrence.certified(
          referenceLineRecord,
          releaseOccurrence
        ).toList
        occupation <- RecordBoundLegalMoveOccurrence.certified(
          referenceLineRecord,
          occupationOccurrence
        ).toList
        if occupation.capture.isEmpty && occupation.movement.side == release.movement.side &&
          occupation.movement.to == release.movement.from
        if playedSteps.size >= seed.occupationStepIndex
        playedPreStep = playedSteps(seed.occupationStepIndex - 1)
        playedOccurrence <- playedReplay.positionAfter(playedPreStep).toList
        blocker = RelationColoredPieceWitness(
          release.movement.from,
          release.movement.beforeRole,
          release.movement.side
        )
        occupier = RelationColoredPieceWitness(
          occupation.movement.from,
          occupation.movement.beforeRole,
          occupation.movement.side
        )
        occupiedAfter = RelationColoredPieceWitness(
          occupation.movement.to,
          occupation.movement.afterRole,
          occupation.movement.side
        )
        referenceBranch = CausalBranchOccurrence.certifiedCounterfactual(
          ComparedLineBranchRole.CounterfactualReference,
          referenceLine,
          referenceReplay,
          seed.occupationStepIndex + 1
        )
        playedBranch = CausalBranchOccurrence.observedRootWithAnalyzedContinuation(
          ComparedLineBranchRole.PlayedRootAnalysisContinuation,
          playedLine,
          playedReplay,
          seed.occupationStepIndex
        )
        referenceOccupationAuthority <- occupiedAuthority(
          referenceLineRecord,
          occupationStep,
          occupiedAfter
        ).toList
        playedBlockerAuthority <- occupiedAuthority(
          playedLineRecord,
          playedPreStep,
          blocker
        ).toList
        playedOccupierAuthority <- occupiedAuthority(
          playedLineRecord,
          playedPreStep,
          occupier
        ).toList
        playedAbsenceAuthority <- ClosedRelationAbsenceAuthority.forQuery(
          playedLineRecord,
          playedOccurrence,
          PositionRelationExtractor.ClosedRelationAbsenceQuery.LegalMoveFromTo(
            occupation.movement.side,
            occupation.movement.from,
            occupation.movement.to
          )
        ).toList
        if vacancyPrefix.size >= seed.occupationStepIndex
        referenceVacancyAuthorities = vacancyPrefix.take(seed.occupationStepIndex)
      yield ExactInputs(
        rootBoard,
        release,
        occupation,
        blocker,
        occupier,
        referenceBranch,
        playedBranch,
        playedAbsenceAuthority,
        referenceVacancyAuthorities,
        referenceOccupationAuthority,
        playedBlockerAuthority,
        playedOccupierAuthority
      )
      exactInputs.map(input =>
        certify(input, referenceLineRecord, playedLineRecord, referenceReplay, playedReplay)
      )

  private def certify(
      inputs: ExactInputs,
      referenceLineRecord: EvidenceRecord,
      playedLineRecord: EvidenceRecord,
      referenceReplay: CanonicalLineReplay,
      playedReplay: CanonicalLineReplay
  ): CertifiedVacancyEnablesOccupation =
    val occupationIndex = inputs.referenceBranch.replaySteps.size - 1
    val releaseUse = CausalLegalMovePremiseUse.from(
      VacancyOccupationPremiseRole.ReferenceReleaseMove,
      inputs.release,
      inputs.referenceBranch,
      0
    )
    val occupationUse = CausalLegalMovePremiseUse.from(
      VacancyOccupationPremiseRole.ReferenceOccupationMove,
      inputs.occupation,
      inputs.referenceBranch,
      occupationIndex
    )
    val playedMoveAbsent = CausalClosedAbsenceBinding.afterStep(
      VacancyOccupationAbsenceRole.PlayedOccupationMoveAbsent,
      inputs.playedAbsenceAuthority,
      inputs.playedBranch,
      occupationIndex - 1
    )
    val referenceVacancies = inputs.referenceVacancyAuthorities.zipWithIndex.map {
      case (authority, stepIndex) =>
        CausalClosedStateBinding.afterStep(
          VacancyOccupationStateRole.ReferenceVacancy,
          authority,
          inputs.referenceBranch,
          stepIndex
        )
    }
    val referenceOccupation = CausalClosedStateBinding.afterStep(
      VacancyOccupationStateRole.ReferenceOccupation,
      inputs.referenceOccupationAuthority,
      inputs.referenceBranch,
      occupationIndex
    )
    val playedBlocker = CausalClosedStateBinding.afterStep(
      VacancyOccupationStateRole.PlayedBlocker,
      inputs.playedBlockerAuthority,
      inputs.playedBranch,
      occupationIndex - 1
    )
    val playedOccupier = CausalClosedStateBinding.afterStep(
      VacancyOccupationStateRole.PlayedOccupier,
      inputs.playedOccupierAuthority,
      inputs.playedBranch,
      occupationIndex - 1
    )
    val vacancyClosure = VacancySiblingClosure.certified(
      inputs.release.movement,
      inputs.release.moveUci,
      inputs.blocker,
      inputs.occupation.movement,
      inputs.occupation.moveUci,
      inputs.occupier,
      inputs.referenceBranch,
      inputs.playedBranch,
      occupationIndex,
      playedBlocker,
      playedOccupier,
      playedMoveAbsent
    ).getOrElse(
      throw IllegalArgumentException("occupation failed the shared vacancy-move kernel")
    )
    val proposition = VacancyOccupationCausalAuthority.proposition(
      inputs.rootBoard,
      inputs.release.movement,
      inputs.blocker,
      inputs.occupation.movement,
      inputs.occupier
    )
    val manifest = VacancyOccupationManifest.exact(
      releaseUse,
      occupationUse,
      playedMoveAbsent,
      referenceVacancies,
      referenceOccupation,
      playedBlocker,
      playedOccupier
    )
    val causalOccurrence = CausalOccurrenceIdentity.from(
      proposition,
      List(inputs.referenceBranch, inputs.playedBranch)
    )
    val proofSet = BoundedCausalProofSet.from(
      proposition,
      causalOccurrence,
      List(CausalProofPathOccurrence.from(proposition, manifest))
    )
    val semantic = VacancyEnablesOccupationSemanticProof(
      proposition,
      inputs.release.movement,
      inputs.blocker,
      inputs.occupation.movement,
      inputs.occupier
    )
    val occurrence = VacancyEnablesOccupationOccurrence(proofSet)
    val dependencyManifest = VacancyOccupationDependencyManifest(
      referenceLineRecord,
      playedLineRecord,
      proofSet
    )
    new CertifiedVacancyEnablesOccupation(
      semantic,
      occurrence,
      BoundedCausalDependencyFingerprint.from(dependencyManifest),
      dependencyManifest,
      referenceLineRecord,
      playedLineRecord,
      referenceReplay,
      playedReplay,
      inputs.release,
      inputs.occupation,
      vacancyClosure,
      inputs.playedAbsenceAuthority,
      inputs.referenceVacancyAuthorities ++ List(
        inputs.referenceOccupationAuthority,
        inputs.playedBlockerAuthority,
        inputs.playedOccupierAuthority
      )
    )

  private def occupiedAuthority(
      issuerRecord: EvidenceRecord,
      step: LineReplayStep,
      piece: RelationColoredPieceWitness
  ): Option[ClosedPositionStateAuthority] =
    ClosedPositionStateAuthority.atStep(issuerRecord, step)((occurrence, scope) =>
      occurrence.existingOccupantState(piece, scope)
    )

  private def continuousVacancyPrefix(
      issuerRecord: EvidenceRecord,
      steps: List[LineReplayStep],
      releasedSquare: EvidenceSquare
  ): List[ClosedPositionStateAuthority] =
    steps
      .foldLeft(List.empty[ClosedPositionStateAuthority] -> true) {
        case ((reversePrefix, true), step) =>
          ClosedPositionStateAuthority.atStep(issuerRecord, step)((occurrence, scope) =>
            occurrence.existingVacancyState(releasedSquare, scope)
          ) match
            case Some(authority) => (authority :: reversePrefix) -> true
            case None            => reversePrefix -> false
        case (inactive, _) => inactive
      }
      ._1
      .reverse
