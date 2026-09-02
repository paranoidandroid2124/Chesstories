package lila.chessjudgment.model.judgment

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import lila.chessjudgment.analysis.position.PositionRelationExtractor
import lila.chessjudgment.model.line.PrincipalVariationEvidence

private[chessjudgment] enum BoundedCausalContractKind:
  case UniqueCheckReplyDefenderDisplacementBeforeCapture
  case SoleRecapturerRemovalBeforeTargetCapture
  case VacatedGateEnablesUnrecapturableSliderCapture
  case SquareReleaseRoute
  case CaptureExclusionMoveOrder
  case RelocationEnablesRecapture
  case PassedPawnProgressRealizedAfterOnlyLegalReply

  def semanticNamespace: String =
    this match
      case UniqueCheckReplyDefenderDisplacementBeforeCapture =>
        "causal-proposition:unique-check-reply-defender-displacement-before-capture:v2"
      case SoleRecapturerRemovalBeforeTargetCapture =>
        "causal-proposition:sole-recapturer-removal-before-target-capture:v1"
      case VacatedGateEnablesUnrecapturableSliderCapture =>
        "causal-proposition:vacated-gate-enables-unrecapturable-slider-capture:v1"
      case SquareReleaseRoute =>
        "causal-proposition:square-release-route:v1"
      case CaptureExclusionMoveOrder =>
        "causal-proposition:capture-exclusion-move-order:v1"
      case RelocationEnablesRecapture =>
        "causal-proposition:relocation-enables-recapture:v1"
      case PassedPawnProgressRealizedAfterOnlyLegalReply =>
        "causal-proposition:passed-pawn-progress-realized-after-only-legal-reply:v1"

private[chessjudgment] final case class SemanticPositionIdentity private (value: String)

private[chessjudgment] object SemanticPositionIdentity:
  def fromFen(fen: String): SemanticPositionIdentity =
    SemanticPositionIdentity(
      PrincipalVariationEvidence
        .semanticBoardStateFen(fen)
        .getOrElse(throw IllegalArgumentException("a causal proposition needs a valid root board"))
    )

/** Package-internal family extension capability. It carries semantic parts
  * only after a private family constructor has validated the exact lower
  * witnesses; the descriptor itself is not chess-proof authority.
  */
private[chessjudgment] trait CausalSemanticDescriptor:
  def contractKind: BoundedCausalContractKind
  def rootFen: String
  def semanticParts: List[String]

/** Shared transposition identity. Concrete proof paths and line occurrences
  * are deliberately excluded so independently reached proofs retain one
  * proposition without losing their routes.
  */
private[chessjudgment] final case class CausalPropositionIdentity private (
    semanticId: String,
    contractKind: BoundedCausalContractKind,
    rootPositionIdentity: SemanticPositionIdentity
):
  require(semanticId.matches("[0-9a-f]{64}"), "a causal proposition needs a canonical id")

private[chessjudgment] object CausalPropositionIdentity:
  private[judgment] def from(descriptor: CausalSemanticDescriptor): CausalPropositionIdentity =
    require(descriptor.semanticParts.nonEmpty, "a causal proposition needs exact semantic parts")
    val rootPositionIdentity = SemanticPositionIdentity.fromFen(descriptor.rootFen)
    val semanticId = BoundedCausalIdentity.digest(
      descriptor.contractKind.semanticNamespace :: rootPositionIdentity.value :: descriptor.semanticParts
    )
    CausalPropositionIdentity(semanticId, descriptor.contractKind, rootPositionIdentity)

private[chessjudgment] trait CausalBranchRole:
  def stableKey: String

enum CausalRootProvenance:
  case CounterfactualAnalyzedRoot
  case ObservedGameRoot

private[chessjudgment] enum CausalStepProvenance:
  case ObservedGameMove
  case CertifiedAnalysisMove

private[chessjudgment] final case class CausalStepOccurrence private[judgment] (
    index: Int,
    step: LineReplayStep,
    line: LineNodeRef,
    provenance: CausalStepProvenance
):
  require(index >= 0, "a causal step needs a non-negative branch index")

/** Exact ordered path identity. An observed branch records only its
  * history-owned root move as observed; every continuation move remains
  * certified analysis.
  */
private[chessjudgment] final case class CausalBranchOccurrence private (
    branchId: String,
    role: CausalBranchRole,
    rootProvenance: CausalRootProvenance,
    line: LineNodeRef,
    lineOwnerEvidenceId: String,
    rootTransitionEvidenceId: String,
    steps: List[CausalStepOccurrence],
    rootPositionIdentity: SemanticPositionIdentity
):
  require(branchId.matches("[0-9a-f]{64}"), "a causal branch needs a canonical id")
  require(
    lineOwnerEvidenceId.nonEmpty && rootTransitionEvidenceId.nonEmpty &&
      lineOwnerEvidenceId != rootTransitionEvidenceId,
    "a causal branch needs distinct exact LegalLine and root-transition owners"
  )
  require(steps.nonEmpty, "a causal branch needs an exact root step")
  require(steps.map(_.index) == steps.indices.toList, "causal steps must preserve their exact order")
  require(
    steps.head.line == line && EvidenceRef.sameMove(line.rootMove, steps.head.step.moveUci),
    "a causal branch must start at its registered line root"
  )
  require(
    steps.zip(steps.drop(1)).forall { case (before, after) =>
      after.step.ply == before.step.ply + 1 &&
        PrincipalVariationEvidence.normalizeFen(before.step.fenAfter) ==
          PrincipalVariationEvidence.normalizeFen(after.step.fenBefore)
    },
    "every retained causal occurrence must be one continuous adjacent replay"
  )
  require(
    (rootProvenance, steps.map(_.provenance)) match
      case (
            CausalRootProvenance.CounterfactualAnalyzedRoot,
            provenances
          ) => provenances.forall(_ == CausalStepProvenance.CertifiedAnalysisMove)
      case (
            CausalRootProvenance.ObservedGameRoot,
            CausalStepProvenance.ObservedGameMove :: tail
          ) => tail.forall(_ == CausalStepProvenance.CertifiedAnalysisMove)
      case _ => false,
    "branch provenance must distinguish the observed root from analyzed continuation"
  )

  def replaySteps: List[LineReplayStep] = steps.map(_.step)
  def stepAt(index: Int): Option[CausalStepOccurrence] = steps.lift(index)

private[chessjudgment] object CausalBranchOccurrence:
  def fromRootOccurrence(
      role: CausalBranchRole,
      root: CertifiedRootOccurrence,
      retainedStepCount: Int
  ): CausalBranchOccurrence =
    val steps = retainedSteps(root.replay, retainedStepCount)
    require(
      root.line == root.lineOwner.ref.line.getOrElse(
        throw IllegalArgumentException("a certified root occurrence lost its LegalLine owner")
      ) && steps.head == root.rootStep,
      "a causal branch must retain its certified graph-owned root occurrence"
    )
    fromCertifiedOccurrences(
      role,
      root.rootProvenance,
      root.line,
      root.lineOwner.ref.id,
      root.transitionOwner.ref.id,
      steps.zipWithIndex.map { case (step, index) =>
        new CausalStepOccurrence(
          index,
          step,
          root.line,
          if root.isObserved && index == 0 then CausalStepProvenance.ObservedGameMove
          else CausalStepProvenance.CertifiedAnalysisMove
        )
      }
    )

  private[judgment] def fromCertifiedOccurrences(
      role: CausalBranchRole,
      rootProvenance: CausalRootProvenance,
      line: LineNodeRef,
      lineOwnerEvidenceId: String,
      rootTransitionEvidenceId: String,
      steps: List[CausalStepOccurrence]
  ): CausalBranchOccurrence =
    val rootPositionIdentity = SemanticPositionIdentity.fromFen(steps.headOption.map(_.step.fenBefore).getOrElse(""))
    val branchId = BoundedCausalIdentity.digest(
      List(
        "causal-branch-occurrence:v2",
        role.stableKey,
        rootProvenance.toString.toLowerCase,
        BoundedCausalIdentity.lineKey(line),
        lineOwnerEvidenceId,
        rootTransitionEvidenceId,
        steps
          .map(step =>
            List(
              step.provenance.toString.toLowerCase,
              BoundedCausalIdentity.lineKey(step.line),
              BoundedCausalIdentity.stepKey(step.step)
            ).mkString(":")
          )
          .mkString("[", ",", "]")
      )
    )
    CausalBranchOccurrence(
      branchId,
      role,
      rootProvenance,
      line,
      lineOwnerEvidenceId,
      rootTransitionEvidenceId,
      steps,
      rootPositionIdentity
    )

  private def retainedSteps(
      replay: CanonicalLineReplay,
      retainedStepCount: Int
  ): List[LineReplayStep] =
    require(retainedStepCount > 0, "a causal branch must retain its root step")
    val steps = replay.replaySteps.take(retainedStepCount)
    require(
      steps.size == retainedStepCount && steps.forall(replay.legalStep(_).nonEmpty),
      "a causal branch may retain only exact steps from its certified replay"
    )
    steps

private[chessjudgment] trait CausalPremiseRole:
  def stableKey: String

private[chessjudgment] trait CausalSupplementalPremiseUse:
  def branchIds: Set[String]
  def stableKey: String

private[chessjudgment] final case class CausalVerticalRelationPremiseUse private (
    role: CausalPremiseRole,
    contract: VerticalRelationContractKind,
    result: DerivedRelationResultKey,
    issuerEvidenceId: String,
    issuerOccurrenceId: String,
    sourcePremiseIds: List[String],
    branchId: String,
    branchRole: CausalBranchRole,
    stepIndex: Int
):
  require(issuerEvidenceId.nonEmpty, "a causal relation premise needs its exact LegalLine issuer")
  require(
    issuerOccurrenceId.matches("[0-9a-f]{64}"),
    "a causal relation premise needs its exact replay occurrence"
  )
  require(sourcePremiseIds.nonEmpty, "a causal relation premise needs its exact lower sources")
  require(
    sourcePremiseIds == sourcePremiseIds.sorted && sourcePremiseIds.distinct.size == sourcePremiseIds.size,
    "lower premise ids must be unique and canonical within one named use"
  )
  require(stepIndex >= 0, "a relation premise needs an exact branch step")

  def stableKey: String =
    List(
      role.stableKey,
      VerticalRelationContractKind.id(contract),
      result.stableKey,
      issuerEvidenceId,
      issuerOccurrenceId,
      sourcePremiseIds.mkString("[", ",", "]"),
      branchId,
      branchRole.stableKey,
      stepIndex.toString
    ).mkString("|")

private[chessjudgment] object CausalVerticalRelationPremiseUse:
  def from(
      role: CausalPremiseRole,
      authority: RecordBoundVerticalRelationOccurrence,
      branch: CausalBranchOccurrence,
      stepIndex: Int
  ): CausalVerticalRelationPremiseUse =
    val stepOccurrence = branch.stepAt(stepIndex)
    require(
      stepOccurrence.exists(step =>
        step.step == authority.step && step.line == authority.issuerLine &&
          authority.scope == EvidenceScope.LegalLine
      ),
      "a relation premise must bind its exact graph-owned replay occurrence"
    )
    val sourcePremiseIds = authority.causalSourcePremiseIds
    CausalVerticalRelationPremiseUse(
      role,
      authority.contract,
      authority.result,
      authority.issuerEvidenceId,
      authority.occurrenceId,
      sourcePremiseIds,
      branch.branchId,
      branch.role,
      stepIndex
    )

private[chessjudgment] trait CausalAbsenceRole:
  def stableKey: String

private[chessjudgment] trait CausalStateRole:
  def stableKey: String

private[chessjudgment] trait CausalSupplementalClosureBinding:
  def branchIds: Set[String]
  def stableKey: String

/** Semantic absence id plus one exact branch use. The same semantic absence
  * may occur in several paths or steps; uniqueness never collapses by proof id.
  */
private[chessjudgment] final case class CausalClosedAbsenceBinding private (
    role: CausalAbsenceRole,
    authority: ClosedRelationAbsenceAuthority,
    branchId: String,
    branchRole: CausalBranchRole,
    afterStepIndex: Int
):
  def semanticProofId: String = authority.semanticProofId
  def issuerEvidenceId: String = authority.issuerEvidenceId
  def issuerOccurrenceId: String = authority.occurrence.occurrenceId
  def queryKey: String = authority.queryKey
  def position: PositionNodeRef = authority.proof.position
  def scope: EvidenceScope = authority.scope
  require(semanticProofId.matches("[0-9a-f]{64}"), "a closed absence needs its semantic id")
  require(issuerEvidenceId.nonEmpty, "a closed absence needs its exact LegalLine issuer")
  require(issuerOccurrenceId.matches("[0-9a-f]{64}"), "a closed absence needs its exact replay occurrence")
  require(queryKey.nonEmpty, "a closed absence needs its exact inventory query")

  def stableKey: String =
    List(
      role.stableKey,
      semanticProofId,
      issuerEvidenceId,
      issuerOccurrenceId,
      queryKey,
      PrincipalVariationEvidence.normalizeFen(position.fen),
      position.ply.toString,
      branchId,
      branchRole.stableKey,
      s"after:$afterStepIndex"
    ).mkString("|")

private[chessjudgment] object CausalClosedAbsenceBinding:
  def afterStep(
      role: CausalAbsenceRole,
      authority: ClosedRelationAbsenceAuthority,
      branch: CausalBranchOccurrence,
      stepIndex: Int
  ): CausalClosedAbsenceBinding =
    val retainedOccurrence = branch.stepAt(stepIndex)
      .getOrElse(throw IllegalArgumentException("a closed absence must bind a retained branch step"))
    val step = retainedOccurrence.step
    require(
      authority.step == step && step.ply == authority.proof.position.ply &&
        PrincipalVariationEvidence.sameBoardState(step.fenAfter, authority.proof.position.fen),
      "a closed absence position must be the exact after occurrence of its branch step"
    )
    require(
      authority.issuerLine == retainedOccurrence.line &&
        authority.scope == EvidenceScope.LegalLine,
      "a closed absence must retain its exact graph-owned branch inventory authority"
    )
    CausalClosedAbsenceBinding(
      role,
      authority,
      branch.branchId,
      branch.role,
      stepIndex
    )

/** Certified payload of one LegalLine record. Final graph membership remains
  * the EvidenceGraph's authority and is intentionally not claimed here.
  * Positive-state and L1 occurrence authorities share this validation instead
  * of rebuilding line ownership independently.
  */
private[chessjudgment] final case class CertifiedLineReplayRecord private (
    issuerRecord: EvidenceRecord,
    replay: CanonicalLineReplay
):
  val ref: EvidenceRef = issuerRecord.ref
  val line: LineNodeRef = ref.line.get
  val scope: EvidenceScope = ref.scope

private[chessjudgment] object CertifiedLineReplayRecord:
  def from(issuerRecord: EvidenceRecord): Option[CertifiedLineReplayRecord] =
    issuerRecord match
      case EvidenceRecord(ref, line: LineFactEvidence, _)
          if ref.producer == EvidenceProducer.LegalLineProducer &&
            ref.layer == EvidenceLayer.Line &&
            ref.confidence == EvidenceConfidence.LegalReplayVerified &&
            ref.line.contains(line.line) && ref.scope == EvidenceScope.LegalLine =>
        line.certifiedReplay.map(CertifiedLineReplayRecord(issuerRecord, _))
      case _ => None

  def exact(
      source: EvidenceRecord,
      line: LineNodeRef,
      replay: CanonicalLineReplay
  ): Boolean =
    from(source).exists(authority =>
      authority.line == line && authority.replay == replay
    )

/** Sole record-bound authority for one exact absence issued by a closed
  * LegalLine position inventory. L2 families consume this opaque authority;
  * none may reconstruct record, occurrence, or absence ownership themselves.
  */
private[chessjudgment] final case class ClosedRelationAbsenceAuthority private (
    lineRecord: CertifiedLineReplayRecord,
    occurrence: ReplayPositionOccurrence,
    proof: PositionRelationExtractor.ClosedRelationAbsenceProof
):
  val step: LineReplayStep = occurrence.step
  val issuerRecord: EvidenceRecord = lineRecord.issuerRecord
  val issuerEvidenceId: String = lineRecord.ref.id
  val issuerLine: LineNodeRef = lineRecord.line
  val scope: EvidenceScope = lineRecord.scope
  val semanticProofId: String = ClosedRelationAbsenceProofIdentity.semanticId(proof)
  val query: PositionRelationExtractor.ClosedRelationAbsenceQuery = proof.query
  def queryKey: String = query.stableKey
  def remainsCertified: Boolean =
    ClosedRelationAbsenceAuthority.certified(issuerRecord, occurrence, proof).contains(this)

private[chessjudgment] object ClosedRelationAbsenceAuthority:
  def forQuery(
      issuerRecord: EvidenceRecord,
      occurrence: ReplayPositionOccurrence,
      query: PositionRelationExtractor.ClosedRelationAbsenceQuery
  ): Option[ClosedRelationAbsenceAuthority] =
    for
      lineRecord <- CertifiedLineReplayRecord.from(issuerRecord)
      exactOccurrence <- lineRecord.replay.positionAfter(occurrence.step)
      if exactOccurrence.occurrenceId == occurrence.occurrenceId && exactOccurrence.sameOwner(occurrence)
      proof <- exactOccurrence.closedAbsence(query, lineRecord.scope)
      if occurrence.certifies(proof, lineRecord.scope)
    yield ClosedRelationAbsenceAuthority(lineRecord, exactOccurrence, proof)

  def certified(
      issuerRecord: EvidenceRecord,
      occurrence: ReplayPositionOccurrence,
      proof: PositionRelationExtractor.ClosedRelationAbsenceProof
  ): Option[ClosedRelationAbsenceAuthority] =
    for
      lineRecord <- CertifiedLineReplayRecord.from(issuerRecord)
      exactOccurrence <- lineRecord.replay.positionAfter(occurrence.step)
      if proof.scope == lineRecord.scope
      if exactOccurrence.occurrenceId == occurrence.occurrenceId && exactOccurrence.sameOwner(occurrence)
      rebound <- exactOccurrence.closedAbsence(proof.query, lineRecord.scope)
      if rebound.query == proof.query && rebound.position == proof.position && rebound.scope == proof.scope
      if exactOccurrence.certifies(proof, lineRecord.scope) && occurrence.certifies(proof, lineRecord.scope)
      if exactOccurrence.certifies(rebound, lineRecord.scope) && occurrence.certifies(rebound, lineRecord.scope)
    yield ClosedRelationAbsenceAuthority(lineRecord, exactOccurrence, proof)

/** One exact L1 occurrence bound to the full certified LegalLine record that
  * produced it. EvidenceGraph later proves that record is the canonical graph
  * owner; copied ids alone are never accepted.
  */
private[chessjudgment] final case class RecordBoundVerticalRelationOccurrence private (
    lineRecord: CertifiedLineReplayRecord,
    occurrence: ReplayVerticalRelationOccurrence
):
  val issuerRecord: EvidenceRecord = lineRecord.issuerRecord
  val issuerEvidenceId: String = lineRecord.ref.id
  val issuerLine: LineNodeRef = lineRecord.line
  val scope: EvidenceScope = lineRecord.scope
  val step: LineReplayStep = occurrence.step
  val contract: VerticalRelationContractKind = occurrence.contract
  val result: DerivedRelationResultKey = DerivedRelationResultKey.from(occurrence.relation)
  val occurrenceId: String = occurrence.occurrenceId
  val certifiedSourcePremiseIds: List[String] = occurrence.certifiedSourcePremiseIds
  val binding: ReplayVerticalRelationOccurrenceBinding = ReplayVerticalRelationOccurrenceBinding.from(occurrence)
  def remainsCertified: Boolean =
    RecordBoundVerticalRelationOccurrence.certified(issuerRecord, occurrence).contains(this)
  def causalSourcePremiseIds: List[String] =
    val ids = issuerEvidenceId :: occurrenceId :: certifiedSourcePremiseIds
    require(
      ids.distinct.size == ids.size,
      "an L2 relation premise cannot collapse distinct issuer, occurrence, or lower-premise owners"
    )
    ids.sorted
  def stableKey: String =
    List(
      BoundedCausalIdentity.evidenceRecordKey(issuerRecord),
      BoundedCausalIdentity.lineKey(issuerLine),
      binding.stableKey
    ).mkString("|")

private[chessjudgment] object RecordBoundVerticalRelationOccurrence:
  def certified(
      issuerRecord: EvidenceRecord,
      occurrence: ReplayVerticalRelationOccurrence
  ): Option[RecordBoundVerticalRelationOccurrence] =
    for
      lineRecord <- CertifiedLineReplayRecord.from(issuerRecord)
      rebound <- lineRecord.replay.verticalRelationOccurrences(occurrence.step, List(occurrence.contract))
        .find(_ == occurrence)
    yield RecordBoundVerticalRelationOccurrence(lineRecord, rebound)

/** One replay-owned legal movement bound to its sole certified LegalLine
  * record. It exposes the already-issued movement occurrence to L2 without
  * parsing the move or asking a second legality source.
  */
private[chessjudgment] final case class RecordBoundLegalMoveOccurrence private (
    lineRecord: CertifiedLineReplayRecord,
    occurrence: ReplayLegalMoveOccurrence,
    private[chessjudgment] val positionAfterOccurrence: ReplayPositionOccurrence
):
  val issuerRecord: EvidenceRecord = lineRecord.issuerRecord
  val issuerEvidenceId: String = lineRecord.ref.id
  val issuerLine: LineNodeRef = lineRecord.line
  val scope: EvidenceScope = lineRecord.scope
  val step: LineReplayStep = occurrence.step
  val movement: RelationMoveTransitionWitness = occurrence.movement.witness
  val moveUci: String = occurrence.movement.moveUci
  val capture: Option[RelationLegalCaptureWitness] = occurrence.movement.capture
  val movementMode: PositionRelationExtractor.ClosedLegalMovementMode = occurrence.movement.mode
  val legalMoveSemanticId: String = occurrence.movement.fact.semanticId
  val transitionFootprintId: String = occurrence.footprintId
  val occurrenceId: String = occurrence.occurrenceId

  def remainsCertified: Boolean =
    RecordBoundLegalMoveOccurrence.certified(issuerRecord, occurrence).contains(this)

  def causalSourcePremiseIds: List[String] =
    val ids = issuerEvidenceId :: occurrenceId :: occurrence.certifiedSourcePremiseIds
    require(
      ids.distinct.size == ids.size,
      "an L2 legal-move premise cannot collapse issuer, occurrence, or lower fact owners"
    )
    ids.sorted

  def leavesUntouched(piece: RelationColoredPieceWitness): Boolean =
    ReplayLegalMoveOccurrence.leavesUntouched(occurrence, piece)

private[chessjudgment] object RecordBoundLegalMoveOccurrence:
  private[chessjudgment] def certifiedBy(
      lineRecord: CertifiedLineReplayRecord,
      occurrence: ReplayLegalMoveOccurrence
  ): Option[RecordBoundLegalMoveOccurrence] =
    lineRecord.replay.transition(occurrence.step)
      .filter(_.legalMoveOccurrence == occurrence)
      .map(transition => RecordBoundLegalMoveOccurrence(
        lineRecord,
        occurrence,
        transition.positionAfterOccurrence
      ))

  private[chessjudgment] def exactSequence(
      lineRecord: CertifiedLineReplayRecord,
      steps: List[LineReplayStep]
  ): Option[List[RecordBoundLegalMoveOccurrence]] =
    lineRecord.replay.transitionOccurrencesFor(steps).map(_.map(transition =>
      RecordBoundLegalMoveOccurrence(
        lineRecord,
        transition.legalMoveOccurrence,
        transition.positionAfterOccurrence
      )
    ))

  def certified(
      issuerRecord: EvidenceRecord,
      occurrence: ReplayLegalMoveOccurrence
  ): Option[RecordBoundLegalMoveOccurrence] =
    for
      lineRecord <- CertifiedLineReplayRecord.from(issuerRecord)
      rebound <- certifiedBy(lineRecord, occurrence)
    yield rebound

  def atIndex(
      issuerRecord: EvidenceRecord,
      stepIndex: Int,
      expected: ReplayLegalMoveOccurrence
  ): Option[RecordBoundLegalMoveOccurrence] =
    for
      lineRecord <- CertifiedLineReplayRecord.from(issuerRecord)
      step <- lineRecord.replay.replaySteps.lift(stepIndex)
      transition <- lineRecord.replay.transition(step)
      if transition.legalMoveOccurrence == expected
    yield RecordBoundLegalMoveOccurrence(
      lineRecord,
      expected,
      transition.positionAfterOccurrence
    )

  def sameMove(
      left: RecordBoundLegalMoveOccurrence,
      right: RecordBoundLegalMoveOccurrence
  ): Boolean = ReplayLegalMoveOccurrence.sameMove(left.occurrence, right.occurrence)

  def capturedTarget(
      occurrence: RecordBoundLegalMoveOccurrence
  ): Option[RelationColoredPieceWitness] = ReplayLegalMoveOccurrence.capturedTarget(occurrence.occurrence)

/** One exact same-object trajectory edge issued by `LineObjectTrajectory` and
  * rebound to the selected footprint transitions at both endpoints. Every
  * intervening legal occurrence and OccupiedBy state remains path-owned.
  */
private[chessjudgment] final case class RecordBoundObjectTrajectory private (
    lineRecord: CertifiedLineReplayRecord,
    trajectory: LineObjectTrajectory,
    rootMovement: RecordBoundLegalMoveOccurrence,
    futureMovement: RecordBoundLegalMoveOccurrence,
    interveningMovements: List[RecordBoundLegalMoveOccurrence]
):
  require(
    rootMovement.issuerRecord == lineRecord.issuerRecord &&
      futureMovement.issuerRecord == lineRecord.issuerRecord &&
      rootMovement.occurrence == trajectory.rootLegalOccurrence &&
      futureMovement.occurrence == trajectory.futureLegalOccurrence &&
      ReplayLegalMoveOccurrence.ownsTransition(rootMovement.occurrence, trajectory.rootTransition) &&
      ReplayLegalMoveOccurrence.ownsTransition(futureMovement.occurrence, trajectory.futureTransition),
    "an object trajectory needs exact graph-owned selected endpoint transitions"
  )
  private val retainedPiece = RelationColoredPieceWitness(
    trajectory.rootTransition.to,
    trajectory.rootTransition.afterRole,
    trajectory.rootTransition.side
  )
  private val orderedMovements = rootMovement :: interveningMovements ::: List(futureMovement)
  require(
    RelationColoredPieceWitness(
      trajectory.futureTransition.from,
      trajectory.futureTransition.beforeRole,
      trajectory.futureTransition.side
    ) == retainedPiece &&
      interveningMovements.map(_.occurrence) == trajectory.interveningLegalOccurrences &&
      interveningMovements.forall(move =>
        move.issuerRecord == lineRecord.issuerRecord && move.leavesUntouched(retainedPiece)
      ) && trajectory.persistenceStates.forall(state =>
        state.issuerRecord == lineRecord.issuerRecord && (state.query match
          case PositionRelationExtractor.ClosedPositionStateQuery.OccupiedBy(piece) => piece == retainedPiece
          case _ => false)
      ),
    "an object trajectory must retain one physical mover through every untouched occurrence"
  )

  def persistenceStates: List[ClosedPositionStateAuthority] = trajectory.persistenceStates
  def plyOffset: Int = trajectory.plyOffset

  def stableKey: String = List(
    BoundedCausalIdentity.evidenceRecordKey(lineRecord.issuerRecord),
    rootMovement.occurrenceId,
    rootMovement.transitionFootprintId,
    trajectory.rootTransition.stableKey,
    futureMovement.occurrenceId,
    futureMovement.transitionFootprintId,
    trajectory.futureTransition.stableKey,
    interveningMovements.map(move =>
      List(move.occurrenceId, move.transitionFootprintId).mkString(":")
    ).mkString("[", ",", "]"),
    persistenceStates.map(state =>
      List(state.issuerEvidenceId, state.occurrence.occurrenceId, state.semanticProofId).mkString(":")
    ).mkString("[", ",", "]")
  ).mkString("|")

  def remainsCertified: Boolean =
    CertifiedLineReplayRecord.from(lineRecord.issuerRecord).contains(lineRecord) &&
      RecordBoundLegalMoveOccurrence.exactSequence(lineRecord, orderedMovements.map(_.step)).contains(orderedMovements) &&
      ReplayLegalMoveOccurrence.ownsTransition(rootMovement.occurrence, trajectory.rootTransition) &&
      ReplayLegalMoveOccurrence.ownsTransition(futureMovement.occurrence, trajectory.futureTransition) &&
      interveningMovements.forall(_.leavesUntouched(retainedPiece)) &&
      interveningMovements.zip(trajectory.persistenceStates).forall { case (move, state) =>
        ClosedPositionStateAuthority
          .certifiedBy(lineRecord, move.positionAfterOccurrence, state.occurrence, state.proof)
          .contains(state)
      }

private[chessjudgment] object RecordBoundObjectTrajectory:
  def firstAfter(
      issuerRecord: EvidenceRecord,
      rootStep: LineReplayStep,
      selectedRootTransition: RelationMoveTransitionWitness,
      continuation: List[LineReplayStep]
  ): Option[RecordBoundObjectTrajectory] =
    CertifiedLineReplayRecord.from(issuerRecord).flatMap { lineRecord =>
      val exact = LineObjectTrajectory
        .findAll(
          rootStep,
          continuation,
          lineRecord.replay,
          _ => Some(lineRecord),
          Some(selectedRootTransition)
        )
        .flatMap(fromTrajectory(lineRecord, _))
      require(
        exact.map(_.stableKey).distinct.size == exact.size,
        "one replay cannot issue duplicate object trajectories"
      )
      exact match
        case value :: Nil => Some(value)
        case Nil          => None
        case _ => throw IllegalStateException("one selected mover acquired multiple first future movements")
    }

  private def fromTrajectory(
      lineRecord: CertifiedLineReplayRecord,
      trajectory: LineObjectTrajectory
  ): Option[RecordBoundObjectTrajectory] =
    val expectedOccurrences =
      trajectory.rootLegalOccurrence :: trajectory.interveningLegalOccurrences ::: List(trajectory.futureLegalOccurrence)
    RecordBoundLegalMoveOccurrence
      .exactSequence(lineRecord, expectedOccurrences.map(_.step))
      .filter(_.map(_.occurrence) == expectedOccurrences)
      .filter(_ => trajectory.persistenceStates.forall(_.issuerRecord == lineRecord.issuerRecord))
      .map { movements =>
        RecordBoundObjectTrajectory(
          lineRecord,
          trajectory,
          movements.head,
          movements.last,
          movements.slice(1, movements.size - 1)
        )
      }

private[chessjudgment] enum ObjectContinuityStepKind:
  case Retained
  case Primary
  case Secondary
  def stableKey: String = toString.toLowerCase

/** Complete common-root-to-endpoint identity of one physical object through a
  * bounded replay prefix. The branch contributes every footprint occurrence;
  * no UCI parsing, board lookup, radius, or horizon is introduced here.
  */
private[chessjudgment] final case class RecordBoundObjectContinuity private (
    issuerRecord: EvidenceRecord,
    branch: CausalBranchOccurrence,
    issued: LineObjectContinuity
):
  val steps: List[RecordBoundObjectContinuityStep] = issued.steps
  val untilExclusive: Int = issued.steps.size
  val rootPiece: RelationColoredPieceWitness = issued.rootPiece
  val endpointPiece: RelationColoredPieceWitness = issued.endpointPiece
  private val branchPrefix = branch.steps.take(untilExclusive)
  require(
    branchPrefix.size == steps.size && steps.zip(branchPrefix).forall { case (step, occurrence) =>
      step.stepIndex == occurrence.index && step.legalOccurrence.issuerRecord == issuerRecord &&
        occurrence.step == step.legalOccurrence.step && occurrence.line == step.legalOccurrence.issuerLine
    },
    "an object continuity must bind one ordered prefix of its exact causal branch"
  )
  def movementSteps: List[RecordBoundObjectContinuityStep] = steps.filter(_.selectedTransition.nonEmpty)
  def retainedSteps: List[RecordBoundObjectContinuityStep] = steps.filter(_.retainedState.nonEmpty)
  def stableKey: String = List(
    BoundedCausalIdentity.evidenceRecordKey(issuerRecord),
    branch.branchId,
    untilExclusive.toString,
    BoundedCausalIdentity.coloredPieceKey(rootPiece),
    BoundedCausalIdentity.coloredPieceKey(endpointPiece),
    steps.map(_.stableKey).mkString("[", ",", "]")
  ).mkString("|")

  private def remainsCertifiedBy(reboundPrefix: List[RecordBoundLegalMoveOccurrence]): Boolean =
    val rebound = reboundPrefix.take(untilExclusive)
    rebound.size == steps.size && steps.zip(rebound).forall { case (step, exact) =>
      step.remainsCertifiedBy(exact)
    }

private[chessjudgment] object RecordBoundObjectContinuity:
  def endingAt(
      issuerRecord: EvidenceRecord,
      branch: CausalBranchOccurrence,
      endpoints: List[(Int, RelationColoredPieceWitness)]
  ): Option[List[RecordBoundObjectContinuity]] =
    val maxUntilExclusive = endpoints.map(_._1).maxOption.getOrElse(0)
    val indicesAreValid = endpoints.nonEmpty && endpoints.forall { case (untilExclusive, _) =>
      untilExclusive >= 0 && untilExclusive <= branch.steps.size
    }
    Option
      .when(indicesAreValid)(())
      .flatMap(_ => reboundPrefix(issuerRecord, branch, maxUntilExclusive))
      .flatMap { exactPrefix =>
        val continuities = LineObjectTrajectory
          .continuitiesEndingAt(exactPrefix, endpoints)
          .map(_.map(RecordBoundObjectContinuity(issuerRecord, branch, _)))
        Option.when(continuities.forall(_.nonEmpty))(continuities.flatten)
      }

  def allRemainCertified(
      issuerRecord: EvidenceRecord,
      branch: CausalBranchOccurrence,
      continuities: List[RecordBoundObjectContinuity]
  ): Boolean =
    val maxUntilExclusive = continuities.map(_.untilExclusive).maxOption.getOrElse(0)
    continuities.nonEmpty && reboundPrefix(issuerRecord, branch, maxUntilExclusive).exists { exactPrefix =>
      continuities.forall(continuity =>
        continuity.issuerRecord == issuerRecord && continuity.branch == branch &&
          continuity.remainsCertifiedBy(exactPrefix)
      )
    }

  private def reboundPrefix(
      issuerRecord: EvidenceRecord,
      branch: CausalBranchOccurrence,
      untilExclusive: Int
  ): Option[List[RecordBoundLegalMoveOccurrence]] =
    CertifiedLineReplayRecord.from(issuerRecord).flatMap { lineRecord =>
      val valid = branch.line == lineRecord.line && branch.lineOwnerEvidenceId == lineRecord.ref.id &&
        untilExclusive >= 0 && untilExclusive <= branch.steps.size &&
        branch.replaySteps.take(untilExclusive) == lineRecord.replay.replaySteps.take(untilExclusive)
      Option.when(valid)(()).flatMap(_ =>
        RecordBoundLegalMoveOccurrence.exactSequence(
          lineRecord,
          branch.replaySteps.take(untilExclusive)
        )
      )
    }


/** One exact use of a replay-owned legal movement in a causal proof path. */
private[chessjudgment] final case class CausalLegalMovePremiseUse private (
    role: CausalPremiseRole,
    movement: RelationMoveTransitionWitness,
    moveUci: String,
    capture: Option[RelationLegalCaptureWitness],
    movementMode: PositionRelationExtractor.ClosedLegalMovementMode,
    legalMoveSemanticId: String,
    issuerEvidenceId: String,
    issuerOccurrenceId: String,
    sourcePremiseIds: List[String],
    branchId: String,
    branchRole: CausalBranchRole,
    stepIndex: Int
) extends CausalSupplementalPremiseUse:
  require(EvidenceRef.normalizeMove(moveUci).nonEmpty, "a causal legal-move use needs an exact move")
  require(
    legalMoveSemanticId.matches("[0-9a-f]{64}"),
    "a causal legal-move use needs its exact lower semantic fact"
  )
  require(issuerEvidenceId.nonEmpty, "a causal legal-move use needs its exact LegalLine issuer")
  require(issuerOccurrenceId.matches("[0-9a-f]{64}"), "a causal legal-move use needs its replay occurrence")
  require(
    sourcePremiseIds.nonEmpty && sourcePremiseIds == sourcePremiseIds.distinct.sorted,
    "a causal legal-move use needs canonical lower premise ids"
  )
  require(stepIndex >= 0, "a causal legal-move use needs an exact branch step")

  val branchIds: Set[String] = Set(branchId)

  def stableKey: String =
    List(
      role.stableKey,
      movement.stableKey,
      EvidenceRef.normalizeMove(moveUci),
      BoundedCausalIdentity.legalCaptureKey(capture),
      movementMode.toString.toLowerCase,
      legalMoveSemanticId,
      issuerEvidenceId,
      issuerOccurrenceId,
      sourcePremiseIds.mkString("[", ",", "]"),
      branchId,
      branchRole.stableKey,
      stepIndex.toString
    ).mkString("|")

private[chessjudgment] object CausalLegalMovePremiseUse:
  def from(
      role: CausalPremiseRole,
      authority: RecordBoundLegalMoveOccurrence,
      branch: CausalBranchOccurrence,
      stepIndex: Int
  ): CausalLegalMovePremiseUse =
    val retained = branch.stepAt(stepIndex)
    require(
      retained.exists(step =>
        step.step == authority.step && step.line == authority.issuerLine &&
          authority.scope == EvidenceScope.LegalLine
      ),
      "a legal-move premise must bind its exact graph-owned replay occurrence"
    )
    CausalLegalMovePremiseUse(
      role,
      authority.movement,
      authority.moveUci,
      authority.capture,
      authority.movementMode,
      authority.legalMoveSemanticId,
      authority.issuerEvidenceId,
      authority.occurrenceId,
      authority.causalSourcePremiseIds,
      branch.branchId,
      branch.role,
      stepIndex
    )

/** Exact public-path use of one physical-object continuity step. The overall
  * legal move remains distinct from the selected primary/secondary footprint
  * transition, so castling cannot be misreported as a rook UCI move.
  */
private[chessjudgment] final case class CausalObjectContinuityStepUse private (
    role: CausalPremiseRole,
    transitionKind: ObjectContinuityStepKind,
    overallMoveUci: String,
    before: RelationColoredPieceWitness,
    after: RelationColoredPieceWitness,
    selectedTransition: Option[RelationMoveTransitionWitness],
    legalMoveSemanticId: String,
    transitionFootprintId: String,
    issuerEvidenceId: String,
    issuerOccurrenceId: String,
    sourcePremiseIds: List[String],
    branchId: String,
    branchRole: CausalBranchRole,
    stepIndex: Int
) extends CausalSupplementalPremiseUse:
  require(EvidenceRef.normalizeMove(overallMoveUci).nonEmpty)
  require(legalMoveSemanticId.matches("[0-9a-f]{64}"))
  require(transitionFootprintId.matches("[0-9a-f]{64}"))
  require(issuerEvidenceId.nonEmpty && issuerOccurrenceId.matches("[0-9a-f]{64}"))
  require(sourcePremiseIds.nonEmpty && sourcePremiseIds == sourcePremiseIds.distinct.sorted)
  require(stepIndex >= 0)
  require(
    (transitionKind, selectedTransition) match
      case (ObjectContinuityStepKind.Retained, None) => before == after
      case (ObjectContinuityStepKind.Primary | ObjectContinuityStepKind.Secondary, Some(move)) =>
        before == RelationColoredPieceWitness(move.from, move.beforeRole, move.side) &&
          after == RelationColoredPieceWitness(move.to, move.afterRole, move.side)
      case _ => false,
    "an object-continuity use must retain its exact selected transition kind"
  )
  val branchIds: Set[String] = Set(branchId)
  def stableKey: String = List(
    role.stableKey,
    transitionKind.stableKey,
    EvidenceRef.normalizeMove(overallMoveUci),
    BoundedCausalIdentity.coloredPieceKey(before),
    BoundedCausalIdentity.coloredPieceKey(after),
    selectedTransition.map(_.stableKey).getOrElse("retained"),
    legalMoveSemanticId,
    transitionFootprintId,
    issuerEvidenceId,
    issuerOccurrenceId,
    sourcePremiseIds.mkString("[", ",", "]"),
    branchId,
    branchRole.stableKey,
    stepIndex.toString
  ).mkString("|")

private[chessjudgment] object CausalObjectContinuityStepUse:
  def from(
      role: CausalPremiseRole,
      authority: RecordBoundObjectContinuity
  ): List[CausalObjectContinuityStepUse] =
    val branch = authority.branch
    authority.steps.map { step =>
      val sourcePremiseIds =
        (step.legalOccurrence.causalSourcePremiseIds :+
          s"transition-footprint:${step.legalOccurrence.transitionFootprintId}").sorted
      require(
        sourcePremiseIds.distinct.size == sourcePremiseIds.size,
        "an object-continuity use cannot collapse its legal and footprint owners"
      )
      CausalObjectContinuityStepUse(
        role,
        step.transitionKind,
        step.legalOccurrence.moveUci,
        step.before,
        step.after,
        step.selectedTransition,
        step.legalOccurrence.legalMoveSemanticId,
        step.legalOccurrence.transitionFootprintId,
        step.legalOccurrence.issuerEvidenceId,
        step.legalOccurrence.occurrenceId,
        sourcePremiseIds,
        branch.branchId,
        branch.role,
        step.stepIndex
      )
    }

/** Sole record-bound authority that one certified LegalLine replay occurrence
  * issued one exact positive-state proof. EvidenceGraph separately certifies
  * canonical graph ownership of the retained record.
  */
private[chessjudgment] final case class ClosedPositionStateAuthority private (
    lineRecord: CertifiedLineReplayRecord,
    occurrence: ReplayPositionOccurrence,
    proof: PositionRelationExtractor.ClosedPositionStateProof
):
  val step: LineReplayStep = occurrence.step
  val issuerRecord: EvidenceRecord = lineRecord.issuerRecord
  val issuerEvidenceId: String = lineRecord.ref.id
  val issuerLine: LineNodeRef = lineRecord.line
  val scope: EvidenceScope = lineRecord.scope
  val semanticProofId: String = ClosedPositionStateProofIdentity.semanticId(proof)
  val query: PositionRelationExtractor.ClosedPositionStateQuery = proof.query
  def queryKey: String = query.stableKey

private[chessjudgment] object ClosedPositionStateAuthority:
  def occurrenceAfter(
      issuerRecord: EvidenceRecord,
      step: LineReplayStep
  ): Option[ReplayPositionOccurrence] =
    CertifiedLineReplayRecord.from(issuerRecord).flatMap(_.replay.positionAfter(step))

  def atStep(
      issuerRecord: EvidenceRecord,
      step: LineReplayStep
  )(
      select: (ReplayPositionOccurrence, EvidenceScope) =>
        Option[PositionRelationExtractor.ClosedPositionStateProof]
  ): Option[ClosedPositionStateAuthority] =
    for
      lineRecord <- CertifiedLineReplayRecord.from(issuerRecord)
      occurrence <- lineRecord.replay.positionAfter(step)
      authority <- atOccurrenceBy(lineRecord, occurrence)(select)
    yield authority

  private[chessjudgment] def atOccurrenceBy(
      lineRecord: CertifiedLineReplayRecord,
      occurrence: ReplayPositionOccurrence
  )(
      select: (ReplayPositionOccurrence, EvidenceScope) =>
        Option[PositionRelationExtractor.ClosedPositionStateProof]
  ): Option[ClosedPositionStateAuthority] =
    for
      proof <- select(occurrence, lineRecord.scope)
      if occurrence.certifies(proof, lineRecord.scope)
    yield ClosedPositionStateAuthority(lineRecord, occurrence, proof)

  def certified(
      issuerRecord: EvidenceRecord,
      occurrence: ReplayPositionOccurrence,
      proof: PositionRelationExtractor.ClosedPositionStateProof
  ): Option[ClosedPositionStateAuthority] =
    for
      lineRecord <- CertifiedLineReplayRecord.from(issuerRecord)
      authority <- certifiedBy(lineRecord, occurrence, proof)
    yield authority

  private[chessjudgment] def certifiedBy(
      lineRecord: CertifiedLineReplayRecord,
      occurrence: ReplayPositionOccurrence,
      proof: PositionRelationExtractor.ClosedPositionStateProof
  ): Option[ClosedPositionStateAuthority] =
    for
      exactOccurrence <- lineRecord.replay.positionAfter(occurrence.step)
      authority <- certifiedBy(lineRecord, exactOccurrence, occurrence, proof)
    yield authority

  private[chessjudgment] def certifiedBy(
      lineRecord: CertifiedLineReplayRecord,
      exactOccurrence: ReplayPositionOccurrence,
      occurrence: ReplayPositionOccurrence,
      proof: PositionRelationExtractor.ClosedPositionStateProof
  ): Option[ClosedPositionStateAuthority] =
    for
      _ <- Option.when(proof.scope == lineRecord.scope)(())
      if exactOccurrence.occurrenceId == occurrence.occurrenceId && exactOccurrence.sameOwner(occurrence)
      if exactOccurrence.certifies(proof, lineRecord.scope) && occurrence.certifies(proof, lineRecord.scope)
    yield ClosedPositionStateAuthority(lineRecord, occurrence, proof)

/** Semantic positive-state id plus one exact branch use. The proof remains
  * owned by the replay position inventory that certified the full typed
  * query; this binding only adds LegalLine and causal-path coordinates.
  */
private[chessjudgment] final case class CausalClosedStateBinding private (
    role: CausalStateRole,
    authority: ClosedPositionStateAuthority,
    branchId: String,
    branchRole: CausalBranchRole,
    afterStepIndex: Int
):
  def semanticProofId: String = authority.semanticProofId
  def issuerEvidenceId: String = authority.issuerEvidenceId
  def issuerOccurrenceId: String = authority.occurrence.occurrenceId
  def query: PositionRelationExtractor.ClosedPositionStateQuery = authority.query
  def queryKey: String = authority.queryKey
  def position: PositionNodeRef = authority.proof.position
  def scope: EvidenceScope = authority.scope
  require(semanticProofId.matches("[0-9a-f]{64}"), "a closed state needs its semantic id")
  require(issuerEvidenceId.nonEmpty, "a closed state needs its exact LegalLine issuer")
  require(issuerOccurrenceId.matches("[0-9a-f]{64}"), "a closed state needs its exact replay occurrence")
  require(queryKey.nonEmpty, "a closed state needs its exact inventory query")

  def stableKey: String =
    List(
      role.stableKey,
      semanticProofId,
      issuerEvidenceId,
      issuerOccurrenceId,
      queryKey,
      PrincipalVariationEvidence.normalizeFen(position.fen),
      position.ply.toString,
      branchId,
      branchRole.stableKey,
      s"after:$afterStepIndex"
    ).mkString("|")

/** Single semantic identity authority for an exact positive state certified
  * by one closed position inventory. Causal families and other exact L2
  * consumers may bind different occurrence paths to this shared meaning.
  */
private[chessjudgment] object ClosedPositionStateProofIdentity:
  def semanticId(
      proof: PositionRelationExtractor.ClosedPositionStateProof
  ): String =
    val semanticBoard = PrincipalVariationEvidence
      .semanticBoardStateFen(proof.position.fen)
      .getOrElse(throw IllegalArgumentException("a closed state needs a semantic board state"))
    BoundedCausalIdentity.digest(
      List("closed-position-state:v1", semanticBoard, proof.query.stableKey)
    )

/** Single semantic identity authority for an exact absence certified by one
  * closed relation inventory. Families validate the rebound lower proof and
  * compare this issued identity instead of reproducing its digest recipe.
  */
private[chessjudgment] object ClosedRelationAbsenceProofIdentity:
  def semanticId(
      proof: PositionRelationExtractor.ClosedRelationAbsenceProof
  ): String =
    val semanticBoard = PrincipalVariationEvidence
      .semanticBoardStateFen(proof.position.fen)
      .getOrElse(throw IllegalArgumentException("a closed absence needs a semantic board state"))
    BoundedCausalIdentity.digest(
      List("closed-relation-absence:v1", semanticBoard, proof.query.stableKey)
    )

private[chessjudgment] object CausalClosedStateBinding:
  def fromContinuity(
      role: CausalStateRole,
      authority: RecordBoundObjectContinuity
  ): List[CausalClosedStateBinding] =
    authority.retainedSteps.map { step =>
      val state = step.retainedState.getOrElse(
        throw IllegalStateException("a retained object-continuity step lost its occupied-state authority")
      )
      CausalClosedStateBinding(
        role,
        state,
        authority.branch.branchId,
        authority.branch.role,
        step.stepIndex
      )
    }

  def afterStep(
      role: CausalStateRole,
      authority: ClosedPositionStateAuthority,
      branch: CausalBranchOccurrence,
      stepIndex: Int
  ): CausalClosedStateBinding =
    val retainedOccurrence = branch.stepAt(stepIndex)
      .getOrElse(throw IllegalArgumentException("a closed state must bind a retained branch step"))
    val step = retainedOccurrence.step
    require(
      authority.step == step && step.ply == authority.proof.position.ply &&
        PrincipalVariationEvidence.sameBoardState(step.fenAfter, authority.proof.position.fen),
      "a closed state position must be the exact after occurrence of its branch step"
    )
    require(
      authority.issuerLine == retainedOccurrence.line && authority.scope == EvidenceScope.LegalLine,
      "a closed state must retain its exact branch inventory scope"
    )
    CausalClosedStateBinding(
      role,
      authority,
      branch.branchId,
      branch.role,
      stepIndex
    )

private[chessjudgment] trait BoundedCausalContractManifest:
  def contractKind: BoundedCausalContractKind
  def premiseUses: List[CausalVerticalRelationPremiseUse]
  def absenceBindings: List[CausalClosedAbsenceBinding]
  def stateBindings: List[CausalClosedStateBinding] = Nil
  def supplementalPremiseUses: List[CausalSupplementalPremiseUse] = Nil
  def supplementalClosureBindings: List[CausalSupplementalClosureBinding] = Nil
  def stableKey: String

private[chessjudgment] final case class CausalClosedAbsenceUse private[judgment] (
    useId: String,
    pathOccurrenceId: String,
    binding: CausalClosedAbsenceBinding
):
  require(useId.matches("[0-9a-f]{64}"), "a closed absence use needs a canonical id")

private[chessjudgment] final case class CausalClosedStateUse private[judgment] (
    useId: String,
    pathOccurrenceId: String,
    binding: CausalClosedStateBinding
):
  require(useId.matches("[0-9a-f]{64}"), "a closed state use needs a canonical id")

private[chessjudgment] final case class CausalSupplementalClosureUse private[judgment] (
    useId: String,
    pathOccurrenceId: String,
    binding: CausalSupplementalClosureBinding
):
  require(useId.matches("[0-9a-f]{64}"), "a causal closure use needs a canonical id")

private[chessjudgment] final case class CausalProofPathOccurrence private (
    pathOccurrenceId: String,
    propositionId: String,
    manifest: BoundedCausalContractManifest,
    closedAbsenceUses: List[CausalClosedAbsenceUse],
    closedStateUses: List[CausalClosedStateUse],
    supplementalClosureUses: List[CausalSupplementalClosureUse]
):
  require(pathOccurrenceId.matches("[0-9a-f]{64}"), "a causal proof path needs a canonical id")
  require(
    closedAbsenceUses.forall(_.pathOccurrenceId == pathOccurrenceId) &&
      closedAbsenceUses.map(_.useId).distinct.size == closedAbsenceUses.size,
    "closed absence uses must retain their exact proof-path ownership"
  )
  require(
    closedStateUses.forall(_.pathOccurrenceId == pathOccurrenceId) &&
      closedStateUses.map(_.useId).distinct.size == closedStateUses.size,
    "closed state uses must retain their exact proof-path ownership"
  )
  require(
    supplementalClosureUses.forall(_.pathOccurrenceId == pathOccurrenceId) &&
      supplementalClosureUses.map(_.useId).distinct.size == supplementalClosureUses.size,
    "causal closure uses must retain their exact proof-path ownership"
  )

  def premiseUses: List[CausalVerticalRelationPremiseUse] = manifest.premiseUses

private[chessjudgment] object CausalProofPathOccurrence:
  def from(
      proposition: CausalPropositionIdentity,
      manifest: BoundedCausalContractManifest
  ): CausalProofPathOccurrence =
    require(
      proposition.contractKind == manifest.contractKind,
      "a causal proof path must use the proposition's exact contract"
    )
    val completeManifestKey = List(
      manifest.stableKey,
      manifest.premiseUses.map(_.stableKey).mkString("premises[", ",", "]"),
      manifest.absenceBindings.map(_.stableKey).mkString("absences[", ",", "]"),
      manifest.stateBindings.map(_.stableKey).mkString("states[", ",", "]"),
      manifest.supplementalPremiseUses.map(_.stableKey).mkString("supplemental-premises[", ",", "]"),
      manifest.supplementalClosureBindings.map(_.stableKey).mkString("supplemental-closures[", ",", "]")
    ).mkString("|")
    val pathId = BoundedCausalIdentity.digest(
      List("causal-proof-path-occurrence:v2", proposition.semanticId, completeManifestKey)
    )
    val uses = manifest.absenceBindings.map { binding =>
      new CausalClosedAbsenceUse(
        useId = BoundedCausalIdentity.digest(
          List("causal-closed-absence-use:v1", pathId, binding.stableKey)
        ),
        pathOccurrenceId = pathId,
        binding = binding
      )
    }
    val supplementalUses = manifest.supplementalClosureBindings.map { binding =>
      new CausalSupplementalClosureUse(
        useId = BoundedCausalIdentity.digest(
          List("causal-supplemental-closure-use:v1", pathId, binding.stableKey)
        ),
        pathOccurrenceId = pathId,
        binding = binding
      )
    }
    val stateUses = manifest.stateBindings.map { binding =>
      new CausalClosedStateUse(
        useId = BoundedCausalIdentity.digest(
          List("causal-closed-state-use:v1", pathId, binding.stableKey)
        ),
        pathOccurrenceId = pathId,
        binding = binding
      )
    }
    CausalProofPathOccurrence(pathId, proposition.semanticId, manifest, uses, stateUses, supplementalUses)

private[chessjudgment] final case class CausalOccurrenceIdentity private (
    occurrenceId: String,
    propositionId: String,
    branches: List[CausalBranchOccurrence]
):
  require(occurrenceId.matches("[0-9a-f]{64}"), "a causal occurrence needs a canonical id")

  def branch(role: CausalBranchRole): Option[CausalBranchOccurrence] =
    branches.filter(_.role == role) match
      case exact :: Nil => Some(exact)
      case _            => None

private[chessjudgment] object CausalOccurrenceIdentity:
  def from(
      proposition: CausalPropositionIdentity,
      branches: List[CausalBranchOccurrence]
  ): CausalOccurrenceIdentity =
    require(
      branches.nonEmpty && branches.map(_.branchId).distinct.size == branches.size,
      "a causal occurrence needs non-empty uniquely owned branches"
    )
    require(
      branches.forall(_.rootPositionIdentity == proposition.rootPositionIdentity),
      "all causal branches must share the proposition's semantic root board"
    )
    val canonical = branches.sortBy(_.branchId)
    val occurrenceId = BoundedCausalIdentity.digest(
      List(
        "causal-occurrence:v1",
        proposition.semanticId,
        canonical.map(_.branchId).mkString("[", ",", "]")
      )
    )
    CausalOccurrenceIdentity(occurrenceId, proposition.semanticId, canonical)

/** One occurrence can retain several independent proof paths. Exact duplicate
  * path ids are rejected; no defensive winner selection is performed.
  */
private[chessjudgment] final case class BoundedCausalProofSet private (
    proposition: CausalPropositionIdentity,
    occurrence: CausalOccurrenceIdentity,
    paths: List[CausalProofPathOccurrence]
):
  require(paths.nonEmpty, "a causal occurrence needs at least one proof path")
  require(
    paths == paths.sortBy(_.pathOccurrenceId) &&
      paths.map(_.pathOccurrenceId).distinct.size == paths.size,
    "independent causal proof paths must be preserved once in canonical order"
  )

private[chessjudgment] object BoundedCausalProofSet:
  def from(
      proposition: CausalPropositionIdentity,
      occurrence: CausalOccurrenceIdentity,
      paths: List[CausalProofPathOccurrence]
  ): BoundedCausalProofSet =
    require(occurrence.propositionId == proposition.semanticId)
    require(paths.forall(_.propositionId == proposition.semanticId))
    require(paths.map(_.pathOccurrenceId).distinct.size == paths.size)
    val branchIds = occurrence.branches.map(_.branchId).toSet
    require(
      paths.flatMap(_.premiseUses).forall(use => branchIds(use.branchId)) &&
        paths.flatMap(_.manifest.supplementalPremiseUses).forall(use => use.branchIds.subsetOf(branchIds)) &&
        paths.flatMap(_.closedAbsenceUses).forall(use => branchIds(use.binding.branchId)) &&
        paths.flatMap(_.closedStateUses).forall(use => branchIds(use.binding.branchId)) &&
        paths.flatMap(_.supplementalClosureUses).forall(use => use.binding.branchIds.subsetOf(branchIds)),
      "every premise and absence use must belong to a retained branch"
    )
    BoundedCausalProofSet(proposition, occurrence, paths.sortBy(_.pathOccurrenceId))

/** Read-only wire projection for common branches, typed lower premises, and
  * closed inventory coordinates. Unsupported supplemental premise kinds fail
  * closed instead of disappearing from a family projection.
  */
final case class BoundedCausalPublicStep private[chessjudgment] (
    index: Int,
    provenance: String,
    ply: Int,
    moveUci: String,
    fenBefore: String,
    fenAfter: String
)

final case class BoundedCausalPublicBranch private[chessjudgment] (
    branchId: String,
    lineId: String,
    lineOwnerEvidenceId: String,
    rootTransitionEvidenceId: String,
    rootMove: String,
    role: String,
    rootProvenance: String,
    steps: List[BoundedCausalPublicStep]
)

sealed trait BoundedCausalPublicPremise:
  def role: String
  def issuerEvidenceId: String
  def issuerOccurrenceId: String
  def sourcePremiseIds: List[String]
  def branchId: String
  def branchRole: String
  def stepIndex: Int

final case class BoundedCausalPublicPremiseUse private[chessjudgment] (
    role: String,
    contract: String,
    resultId: String,
    issuerEvidenceId: String,
    issuerOccurrenceId: String,
    sourcePremiseIds: List[String],
    branchId: String,
    branchRole: String,
    stepIndex: Int
) extends BoundedCausalPublicPremise

final case class BoundedCausalPublicLegalMoveUse private[chessjudgment] (
    role: String,
    moveUci: String,
    side: String,
    from: String,
    to: String,
    beforePiece: String,
    afterPiece: String,
    capturedSquare: Option[String],
    capturedPiece: Option[String],
    capturedSide: Option[String],
    movementMode: String,
    legalMoveSemanticId: String,
    issuerEvidenceId: String,
    issuerOccurrenceId: String,
    sourcePremiseIds: List[String],
    branchId: String,
    branchRole: String,
    stepIndex: Int
) extends BoundedCausalPublicPremise:
  require(
    List(capturedSquare, capturedPiece, capturedSide).forall(_.isDefined) ||
      List(capturedSquare, capturedPiece, capturedSide).forall(_.isEmpty),
    "a public legal-move use must retain a complete optional capture identity"
  )

final case class BoundedCausalPublicObjectContinuityUse private[chessjudgment] (
    role: String,
    transitionKind: String,
    overallMoveUci: String,
    side: String,
    beforeSquare: String,
    beforePiece: String,
    afterSquare: String,
    afterPiece: String,
    selectedTransition: Option[RelationMoveTransitionWitness],
    legalMoveSemanticId: String,
    transitionFootprintId: String,
    issuerEvidenceId: String,
    issuerOccurrenceId: String,
    sourcePremiseIds: List[String],
    branchId: String,
    branchRole: String,
    stepIndex: Int
) extends BoundedCausalPublicPremise

final case class BoundedCausalPublicClosedAbsenceUse private[chessjudgment] (
    useId: String,
    role: String,
    semanticProofId: String,
    issuerEvidenceId: String,
    issuerOccurrenceId: String,
    query: String,
    branchId: String,
    branchRole: String,
    afterStepIndex: Int,
    position: PositionNodeRef,
    scope: EvidenceScope
)

final case class BoundedCausalPublicClosedStateUse private[chessjudgment] (
    useId: String,
    role: String,
    semanticProofId: String,
    issuerEvidenceId: String,
    issuerOccurrenceId: String,
    query: String,
    branchId: String,
    branchRole: String,
    afterStepIndex: Int,
    position: PositionNodeRef,
    scope: EvidenceScope
)

final case class BoundedCausalPublicProofPath private[chessjudgment] (
    pathOccurrenceId: String,
    premises: List[BoundedCausalPublicPremise],
    closedAbsenceUses: List[BoundedCausalPublicClosedAbsenceUse],
    closedStateUses: List[BoundedCausalPublicClosedStateUse]
)

private[chessjudgment] object BoundedCausalPublicProjection:
  def branch(branch: CausalBranchOccurrence): BoundedCausalPublicBranch =
    BoundedCausalPublicBranch(
      branch.branchId,
      branch.line.id,
      branch.lineOwnerEvidenceId,
      branch.rootTransitionEvidenceId,
      EvidenceRef.normalizeMove(branch.line.rootMove),
      branch.role.stableKey,
      branch.rootProvenance.toString,
      branch.steps.map(step =>
        BoundedCausalPublicStep(
          step.index,
          step.provenance.toString,
          step.step.ply,
          step.step.moveUci,
          step.step.fenBefore,
          step.step.fenAfter
        )
      )
    )

  def legalMoveUse(use: CausalLegalMovePremiseUse): BoundedCausalPublicLegalMoveUse =
    BoundedCausalPublicLegalMoveUse(
      use.role.stableKey,
      EvidenceRef.normalizeMove(use.moveUci),
      use.movement.side.toString.toLowerCase,
      use.movement.from.key.toLowerCase,
      use.movement.to.key.toLowerCase,
      use.movement.beforeRole.name.toLowerCase,
      use.movement.afterRole.name.toLowerCase,
      use.capture.map(_.capturedSquare.key.toLowerCase),
      use.capture.map(_.capturedRole.name.toLowerCase),
      use.capture.map(_.capturedSide.toString.toLowerCase),
      use.movementMode.toString,
      use.legalMoveSemanticId,
      use.issuerEvidenceId,
      use.issuerOccurrenceId,
      use.sourcePremiseIds,
      use.branchId,
      use.branchRole.stableKey,
      use.stepIndex
    )

  def objectContinuityUse(
      use: CausalObjectContinuityStepUse
  ): BoundedCausalPublicObjectContinuityUse =
    BoundedCausalPublicObjectContinuityUse(
      use.role.stableKey,
      use.transitionKind.stableKey,
      EvidenceRef.normalizeMove(use.overallMoveUci),
      use.before.side.toString.toLowerCase,
      use.before.square.key.toLowerCase,
      use.before.role.name.toLowerCase,
      use.after.square.key.toLowerCase,
      use.after.role.name.toLowerCase,
      use.selectedTransition,
      use.legalMoveSemanticId,
      use.transitionFootprintId,
      use.issuerEvidenceId,
      use.issuerOccurrenceId,
      use.sourcePremiseIds,
      use.branchId,
      use.branchRole.stableKey,
      use.stepIndex
    )

  private def verticalRelationUse(
      premise: CausalVerticalRelationPremiseUse
  ): BoundedCausalPublicPremiseUse =
    BoundedCausalPublicPremiseUse(
      premise.role.stableKey,
      VerticalRelationContractKind.id(premise.contract),
      premise.result.stableKey,
      premise.issuerEvidenceId,
      premise.issuerOccurrenceId,
      premise.sourcePremiseIds,
      premise.branchId,
      premise.branchRole.stableKey,
      premise.stepIndex
    )

  def closedAbsenceUse(use: CausalClosedAbsenceUse): BoundedCausalPublicClosedAbsenceUse =
    val binding = use.binding
    BoundedCausalPublicClosedAbsenceUse(
      use.useId,
      binding.role.stableKey,
      binding.semanticProofId,
      binding.issuerEvidenceId,
      binding.issuerOccurrenceId,
      binding.queryKey,
      binding.branchId,
      binding.branchRole.stableKey,
      binding.afterStepIndex,
      binding.position,
      binding.scope
    )

  def closedStateUse(use: CausalClosedStateUse): BoundedCausalPublicClosedStateUse =
    val binding = use.binding
    BoundedCausalPublicClosedStateUse(
      use.useId,
      binding.role.stableKey,
      binding.semanticProofId,
      binding.issuerEvidenceId,
      binding.issuerOccurrenceId,
      binding.queryKey,
      binding.branchId,
      binding.branchRole.stableKey,
      binding.afterStepIndex,
      binding.position,
      binding.scope
    )

  def verticalRelationPaths(
      paths: List[CausalProofPathOccurrence]
  ): List[BoundedCausalPublicProofPath] =
    require(
      paths.forall(path =>
        path.manifest.supplementalPremiseUses.isEmpty && path.supplementalClosureUses.isEmpty
      ),
      "the common public causal projection cannot omit family-specific supplemental proof uses"
    )
    paths.map(path =>
      BoundedCausalPublicProofPath(
        path.pathOccurrenceId,
        path.premiseUses.map(verticalRelationUse),
        path.closedAbsenceUses.map(closedAbsenceUse),
        path.closedStateUses.map(closedStateUse)
      )
    )

  def legalMovePaths(
      paths: List[CausalProofPathOccurrence]
  ): List[BoundedCausalPublicProofPath] =
    require(
      paths.forall(path =>
        path.premiseUses.isEmpty && path.supplementalClosureUses.isEmpty &&
          path.manifest.supplementalPremiseUses.nonEmpty &&
          path.manifest.supplementalPremiseUses.forall(_.isInstanceOf[CausalLegalMovePremiseUse])
      ),
      "the legal-move public projection accepts only exact legal-move premise paths"
    )
    paths.map(path =>
      BoundedCausalPublicProofPath(
        path.pathOccurrenceId,
        path.manifest.supplementalPremiseUses.map {
          case exact: CausalLegalMovePremiseUse => legalMoveUse(exact)
          case _ =>
            throw IllegalStateException("a legal-move proof path lost its typed lower premise")
        },
        path.closedAbsenceUses.map(closedAbsenceUse),
        path.closedStateUses.map(closedStateUse)
      )
    )

  /** Projects a path that deliberately combines closed vertical L1 results
    * with replay-owned legal movements. Family code still owns the semantic
    * relationship between those exact uses; this common projection only
    * preserves every typed premise on the wire.
    */
  def mixedPaths(
      paths: List[CausalProofPathOccurrence]
  ): List[BoundedCausalPublicProofPath] =
    require(
      paths.forall(path =>
        path.supplementalClosureUses.isEmpty &&
          path.manifest.supplementalPremiseUses.forall(use =>
            use.isInstanceOf[CausalLegalMovePremiseUse] ||
              use.isInstanceOf[CausalObjectContinuityStepUse]
          )
      ),
      "the mixed public causal projection accepts only vertical, legal-move, and object-continuity premises"
    )
    paths.map(path =>
      BoundedCausalPublicProofPath(
        path.pathOccurrenceId,
        path.premiseUses.map(verticalRelationUse) ++ path.manifest.supplementalPremiseUses.map {
          case exact: CausalLegalMovePremiseUse => legalMoveUse(exact)
          case exact: CausalObjectContinuityStepUse => objectContinuityUse(exact)
          case _ =>
            throw IllegalStateException("a mixed proof path lost its typed supplemental premise")
        },
        path.closedAbsenceUses.map(closedAbsenceUse),
        path.closedStateUses.map(closedStateUse)
      )
    )

private[chessjudgment] trait BoundedCausalDependencyManifest:
  def contractKind: BoundedCausalContractKind
  def stableKey: String

private[chessjudgment] final case class BoundedCausalDependencyFingerprint private (
    value: String
):
  require(value.matches("[0-9a-f]{64}"), "a causal proof needs a complete dependency fingerprint")

private[chessjudgment] object BoundedCausalDependencyFingerprint:
  def from(manifest: BoundedCausalDependencyManifest): BoundedCausalDependencyFingerprint =
    BoundedCausalDependencyFingerprint(
      BoundedCausalIdentity.digest(
        List(
          "bounded-causal-dependency:v1",
          manifest.contractKind.semanticNamespace,
          manifest.stableKey
        )
      )
    )

private[chessjudgment] object BoundedCausalIdentity:
  def digest(parts: Iterable[String]): String =
    val exact = parts.iterator.map(value => s"${value.length}:$value").mkString("|")
    MessageDigest
      .getInstance("SHA-256")
      .digest(exact.getBytes(StandardCharsets.UTF_8))
      .iterator
      .map(byte => f"${byte & 0xff}%02x")
      .mkString

  def lineKey(line: LineNodeRef): String =
    List(
      line.id,
      EvidenceRef.normalizeMove(line.rootMove)
    ).mkString(":")

  def stepKey(step: LineReplayStep): String =
    List(
      step.ply.toString,
      EvidenceRef.normalizeMove(step.moveUci),
      PrincipalVariationEvidence.normalizeFen(step.fenBefore),
      PrincipalVariationEvidence.normalizeFen(step.fenAfter)
    ).mkString(":")

  def coloredPieceKey(piece: RelationColoredPieceWitness): String =
    s"${piece.side.toString.toLowerCase}:${piece.role.name.toLowerCase}@${piece.square.key.toLowerCase}"

  def legalCaptureKey(capture: Option[RelationLegalCaptureWitness]): String =
    capture.map(exact =>
      s"${exact.capturedSide.toString.toLowerCase}:${exact.capturedRole.name.toLowerCase}@${exact.capturedSquare.key.toLowerCase}"
    ).getOrElse("noncapture")

  def evidenceRecordKey(record: EvidenceRecord): String =
    record.payload match
      case payload: LineFactEvidence =>
        val line = record.ref.line.getOrElse(
          throw IllegalArgumentException("a causal LegalLine dependency needs its line occurrence")
        )
        val replay = payload.canonicalReplay.getOrElse(
          throw IllegalArgumentException("a causal LegalLine dependency needs its canonical replay")
        )
        List(
          "legal-line-owner:v2",
          PrincipalVariationEvidence.normalizeFen(record.ref.position.fen),
          record.ref.position.ply.toString,
          EvidenceRef.normalizeMove(line.rootMove),
          replay.replaySteps.map(stepKey).mkString("[", ",", "]")
        ).mkString("|")
      case MoveTransitionEvidence(moveUci, from, to, _) =>
        List(
          "move-transition-owner:v2",
          EvidenceRef.normalizeMove(moveUci),
          PrincipalVariationEvidence.normalizeFen(from.fen),
          from.ply.toString,
          PrincipalVariationEvidence.normalizeFen(to.fen),
          to.ply.toString
        ).mkString("|")
      case _ =>
        val ref = record.ref
        List(
          ref.id,
          ref.producer.toString,
          ref.layer.toString,
          PrincipalVariationEvidence.normalizeFen(ref.position.fen),
          ref.position.ply.toString,
          ref.line.map(lineKey).getOrElse(""),
          ref.confidence.toString,
          record.parents.map(_.id).sorted.mkString("[", ",", "]")
        ).mkString("|")
