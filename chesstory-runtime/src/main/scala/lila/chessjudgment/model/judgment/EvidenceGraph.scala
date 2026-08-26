package lila.chessjudgment.model.judgment

import chess.*
import chess.format.Fen
import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import scala.collection.immutable.VectorMap
import lila.chessjudgment.model.evaluation.{ JudgmentThresholds, PerspectiveMath }
import lila.chessjudgment.model.line.{ LegalReplayStep, PrincipalVariationEvidence }
import lila.chessjudgment.analysis.position.{ PositionAnalysis, PositionRelationExtractor }
import lila.chessjudgment.model.position.{ BoardGeometry, BoardTransitionFootprint, PositionFeatures }
import lila.chessjudgment.model.{
  BranchReplyProbeBinding,
  Plan,
  PlanEventIdentity,
  PlanEventOccurrence,
  PlanPositionOccurrence,
  PlanSequencePathOccurrence,
  PlanSequenceSummary,
  TransitionType
}
import lila.chessjudgment.model.strategic.PlanContinuity
import lila.chessjudgment.model.strategic.PlanTaxonomy.{ PlanKind, PlanTheme }

final case class EvidenceSquare(key: String)
final case class EvidenceFile(key: String)
final case class EvidencePieceRole(name: String)

enum EvidenceSemanticAnchorKind:
  case StrategicMechanism
  case StrategicAxis
  case Plan
  case Relation
  case OpeningAnchor
  case OpeningSupported
  case OpeningObserved
  case CandidateComparison
  case PlanCausalEvent
  case PlanTransition
  case LineEvent
  case LineConsequence
  case StructuralDelta

final case class EvidenceSemanticAnchor(
    kind: EvidenceSemanticAnchorKind,
    values: List[String]
):
  def stableKey: String =
    (kind.toString :: values).mkString(":")

object EvidenceSemanticAnchor:
  def of(kind: EvidenceSemanticAnchorKind, values: String*): EvidenceSemanticAnchor =
    EvidenceSemanticAnchor(kind, values.toList)

enum EvidenceObjectKind:
  case Move
  case Piece
  case Side
  case Square
  case File
  case Pawn
  case PlanSubject
  case Relation
  case Line
  case Mechanism
  case Consequence

final case class ConcreteChessObject(
    kind: EvidenceObjectKind,
    key: String
):
  def signaturePart: String =
    s"$kind:${key.trim.toLowerCase}"

/** A typed, primitive change owned by one direct Cause proof channel.
  *
  * This deliberately describes what happened on the Cause's own event line.
  * R separately translates it into the change experienced by the played move.
  */
enum DirectCausalChange:
  case Occurred
  case Prevented
  case Maintained
  case Lost
  case Refuted
  case Missed

/** Exact typed proof owned by one root-causal effect. A strategic wrapper may
  * refine the primitive's meaning, but it cannot replace the primitive proof.
  */
enum RootOwnedEffectProof:
  case LineEpisode(
      source: EvidenceRef,
      line: LineFactEvidence,
      episode: RootOwnedCausalEpisode
  )
  case RootLineEvent(
      source: EvidenceRef,
      line: LineFactEvidence,
      event: LineMoveEvent
  )
  case RootRelation(
      source: EvidenceRef,
      relation: RelationFactEvidence
  )
  case PlanResult(
      source: EvidenceRef,
      event: PlanCausalEventEvidence,
      assessment: PlanCausalResultAssessment,
      selectedInducedResponse: Option[PlanCausalResponse] = None
  )
  case PlanRestriction(
      source: EvidenceRef,
      event: PlanCausalEventEvidence,
      consequence: TransitionConsequence,
      deterrence: OpponentResourceDeterrenceProof
  )
  case DefensiveRecaptureResource(
      source: EvidenceRef,
      comparison: CandidateComparisonFact,
      resource: PlayedVsBestDefensiveRecaptureResource
  )
  case StrategicAxis(
      primitive: RootOwnedEffectProof,
      axis: StrategicAxisDetail,
      comparisonOutcome: Option[StrategicAxisComparisonOutcome]
  )

  /** Evidence record that owns the primitive effect, beneath any strategic wrapper. */
  final def primitiveSource: EvidenceRef =
    this match
      case RootOwnedEffectProof.LineEpisode(source, _, _)                 => source
      case RootOwnedEffectProof.RootLineEvent(source, _, _)               => source
      case RootOwnedEffectProof.RootRelation(source, _)                    => source
      case RootOwnedEffectProof.PlanResult(source, _, _, _)                => source
      case RootOwnedEffectProof.PlanRestriction(source, _, _, _)           => source
      case RootOwnedEffectProof.DefensiveRecaptureResource(source, _, _)   => source
      case RootOwnedEffectProof.StrategicAxis(primitive, _, _)             => primitive.primitiveSource

  /** Exact non-wrapper proof occurrence. Strategic meaning may refine this
    * occurrence, but it cannot change which chess event and route proved it.
    */
  final def primitiveProof: RootOwnedEffectProof =
    this match
      case RootOwnedEffectProof.StrategicAxis(primitive, _, _) => primitive.primitiveProof
      case primitive                                           => primitive

/** Primitive family of the exact effect owned by one public Cause channel.
  * This is deliberately independent of evidence ids and carrier wrappers.
  */
enum RootOwnedEffectPrimitiveKind:
  case Unspecified
  case LineEpisode
  case RootLineEvent
  case RootRelation
  case PlanResult
  case PlanRestriction
  case DefensiveRecaptureResource

/** Full strategic refinement identity. `label` is semantic data (for example
  * a plan id), not presentation text, so it must participate in equality.
  */
final case class RootOwnedStrategicAxisIdentity(
    kind: StrategicAxisKind,
    polarity: StrategicAxisPolarity,
    label: String,
    comparisonOutcome: Option[StrategicAxisComparisonOutcome]
):
  def stableKey: String =
    List(
      kind.toString.toLowerCase,
      polarity.toString.toLowerCase,
      label,
      comparisonOutcome.map(_.toString.toLowerCase).getOrElse("none")
    ).mkString(":")

/** Comparison-safe identity of a root-owned effect. Importance and fallback
  * dominance may compare effects only when this complete scope is equal.
  */
final case class RootOwnedEffectIdentity(
    primitiveKind: RootOwnedEffectPrimitiveKind,
    targetSignatures: List[String],
    planIds: List[String],
    strategicAxes: List[RootOwnedStrategicAxisIdentity],
    planResult: Option[PlanResultSemanticIdentity] = None
):
  def stableKey: String =
    List(
      primitiveKind.toString.toLowerCase,
      targetSignatures.mkString("[", ",", "]"),
      Option.unless(planResult.nonEmpty)(planIds).getOrElse(Nil).mkString("[", ",", "]"),
      strategicAxes.map(_.stableKey).mkString("[", ",", "]"),
      planResult.map(_.stableKey).getOrElse("none")
    ).mkString("|")

object RootOwnedEffectIdentity:
  val unscoped: RootOwnedEffectIdentity =
    RootOwnedEffectIdentity(RootOwnedEffectPrimitiveKind.Unspecified, Nil, Nil, Nil)

/** A typed magnitude is diagnostic only until its complete effect identity is
  * known. Different magnitude cases are never interchangeable.
  */
enum DirectCauseImportanceMeasure:
  case MateArrival(plyOffset: Int)
  case MaterialOutcome(durableNetCp: Int, onsetPlyOffset: Int)
  case StructuralStrength(units: Int)

enum DirectEffectMagnitudeKnowledge:
  case NotApplicable
  case Exact(measure: DirectCauseImportanceMeasure)
  case ExpectedButMissing
  case Ambiguous

  def comparisonReady: Boolean =
    this match
      case DirectEffectMagnitudeKnowledge.NotApplicable | _: DirectEffectMagnitudeKnowledge.Exact => true
      case DirectEffectMagnitudeKnowledge.ExpectedButMissing | DirectEffectMagnitudeKnowledge.Ambiguous => false

final case class RootOwnedEffectDescriptor(
    identity: RootOwnedEffectIdentity,
    magnitude: DirectEffectMagnitudeKnowledge,
    materialEventSalience: Option[RootOwnedMaterialEventSalience] = None
):
  require(
    magnitude match
      case DirectEffectMagnitudeKnowledge.Exact(
            DirectCauseImportanceMeasure.MaterialOutcome(durableNetCp, onsetPlyOffset)
          ) =>
        durableNetCp > 0 && materialEventSalience.exists(_.plyOffset == onsetPlyOffset)
      case _ => materialEventSalience.isEmpty,
    "material event salience must belong to one exact positive durable outcome"
  )

  def exactMagnitude: Option[DirectCauseImportanceMeasure] =
    magnitude match
      case DirectEffectMagnitudeKnowledge.Exact(measure) => Some(measure)
      case _                                             => None

  private[chessjudgment] def stableKey: String =
    val magnitudeKey = magnitude match
      case DirectEffectMagnitudeKnowledge.NotApplicable => "not-applicable"
      case DirectEffectMagnitudeKnowledge.ExpectedButMissing => "expected-but-missing"
      case DirectEffectMagnitudeKnowledge.Ambiguous => "ambiguous"
      case DirectEffectMagnitudeKnowledge.Exact(measure) =>
        measure match
          case DirectCauseImportanceMeasure.MateArrival(plyOffset) =>
            s"mate-arrival:$plyOffset"
          case DirectCauseImportanceMeasure.MaterialOutcome(durableNetCp, onsetPlyOffset) =>
            s"material-outcome:$durableNetCp:$onsetPlyOffset"
          case DirectCauseImportanceMeasure.StructuralStrength(units) =>
            s"structural-strength:$units"
    List(
      identity.stableKey,
      magnitudeKey,
      materialEventSalience.map(_.stableKey).getOrElse("none")
    ).map(RootOwnedEffectDescriptor.atom).mkString

  /** Taxonomy labels remain available to endpoint enumeration, but R compares
    * exact PlanResults by their owned source/result/route identity.
    */
  private[judgment] def semanticAgreementDescriptor: RootOwnedEffectDescriptor =
    if identity.planResult.nonEmpty then copy(identity = identity.copy(planIds = Nil))
    else this

object RootOwnedEffectDescriptor:
  private def atom(value: String): String = s"${value.length}:$value"

final case class ComparisonEndpointMoverIdentity(side: Color)

/** Whose value is at stake in one endpoint fact. This is derived from the
  * primitive beneficiary/perspective, never from the Cause label alone.
  */
enum RootOwnedEffectStake:
  case ActorValue
  case ActorLiability

/** Comparison scope for one root-owned endpoint observation. Move UCI,
  * concrete actor identity, destination, line identity, evidence ids, and
  * support ids are deliberately absent: ownership is validated on the public
  * channel, while this key compares outcomes for the reviewed mover.
  * Stake remains present: an opponent benefit is not the mover's value even
  * when the remaining chess objects happen to be equal.
  */
final case class ComparisonEndpointEffectScopeKey(
    rootBoardState: String,
    mover: ComparisonEndpointMoverIdentity,
    targetSignatures: List[String],
    mechanismSignatures: List[String],
    consequenceSignatures: List[String],
    horizon: Option[String],
    directChange: DirectCausalChange,
    effectIdentity: RootOwnedEffectIdentity,
    stake: RootOwnedEffectStake
)

enum ComparisonEndpointEffectMagnitude:
  case Exact(measure: DirectCauseImportanceMeasure)
  case QualitativePresence

final case class ComparisonEndpointEffectObservation(
    scope: ComparisonEndpointEffectScopeKey,
    magnitude: ComparisonEndpointEffectMagnitude
)

/** Cause-neutral F-stage witness for one exact root-owned endpoint effect.
  * Admission booleans and Cause-specific change labels are deliberately
  * absent. A strategic wrapper remains a distinct proof because it changes
  * the effect descriptor; tactical carriers may resolve to the bare primitive.
  */
final case class ComparisonEndpointEvidenceWitness(
    sourceSide: RelativeCauseSourceSide,
    line: LineNodeRef,
    binding: EvidenceObjectBinding,
    rootOwnedProof: RootOwnedEffectProof,
    proofSegment: DirectCauseProofSegment,
    effectDescriptor: RootOwnedEffectDescriptor,
    carrierAncestorSourceIds: List[String],
    observation: Option[ComparisonEndpointEffectObservation]
):
  def primitiveProofSource: EvidenceRef = rootOwnedProof.primitiveSource
  def carrier: EvidenceRef = binding.source
  def provenance: List[EvidenceRef] = binding.provenance

final case class ComparisonEndpointEvidenceSideSnapshot(
    sourceSide: RelativeCauseSourceSide,
    line: LineNodeRef,
    witnesses: List[ComparisonEndpointEvidenceWitness]
):
  /** Completeness belongs to one bare primitive family on this exact F
    * endpoint; strategic refinements retain their separate inventory.
    */
  private[chessjudgment] def barePrimitiveInventory(
      kind: RootOwnedEffectPrimitiveKind
  ): ComparisonEndpointEffectInventory =
    EvidenceObjectBinding
      .completeEndpointObservations(
        witnesses
          .filter(witness =>
            witness.effectDescriptor.identity.primitiveKind == kind &&
              witness.effectDescriptor.identity.strategicAxes.isEmpty
          )
          .map(_.observation)
      )
      .map(ComparisonEndpointEffectInventory.Complete.apply)
      .getOrElse(ComparisonEndpointEffectInventory.Incomplete)

final case class ComparisonEndpointEvidenceSnapshot(
    comparisonEvidence: EvidenceRef,
    comparison: CandidateComparisonFact,
    reference: ComparisonEndpointEvidenceSideSnapshot,
    candidate: ComparisonEndpointEvidenceSideSnapshot
):
  def forSide(
      sourceSide: RelativeCauseSourceSide
  ): Option[ComparisonEndpointEvidenceSideSnapshot] =
    sourceSide match
      case RelativeCauseSourceSide.Reference => Some(reference)
      case RelativeCauseSourceSide.Candidate => Some(candidate)
      case RelativeCauseSourceSide.Shared | RelativeCauseSourceSide.Mixed => None

enum ComparisonEndpointEffectInventory:
  case Complete(observations: Set[ComparisonEndpointEffectObservation])
  case Incomplete

final case class ComparisonEndpointLineEffectInventories(
    material: ComparisonEndpointEffectInventory,
    mate: ComparisonEndpointEffectInventory,
    qualitative: ComparisonEndpointEffectInventory
)

object ComparisonEndpointLineEffectInventories:
  val incomplete: ComparisonEndpointLineEffectInventories =
    ComparisonEndpointLineEffectInventories(
      ComparisonEndpointEffectInventory.Incomplete,
      ComparisonEndpointEffectInventory.Incomplete,
      ComparisonEndpointEffectInventory.Incomplete
    )

enum DirectCauseProofStepRole:
  case RootAction
  case CausalLink
  case TerminalEvent

/** How the final exposed move is connected to the effect certified by the
  * owning `RootOwnedEffectProof`. This is intentionally a proof-shape label,
  * not a second consequence or Cause-kind model.
  */
enum DirectCauseProofTerminalRelation:
  case ProducesLineConsequence
  case IsRootLineEvent
  case InstantiatesRelation
  case RealizesPlanResult
  case RestrictsOpponentResource
  case CreatesDefensiveRecaptureResource

final case class DirectCauseProofStep(
    plyOffset: Int,
    moveUci: String,
    role: DirectCauseProofStepRole,
    planEventOccurrence: Option[PlanEventIdentity] = None
):
  require(plyOffset >= 0, "a Cause proof step needs a root-relative ply offset")
  require(moveUci.nonEmpty, "a Cause proof step needs an exact move")
  require(
    planEventOccurrence.forall(event =>
      EvidenceRef.sameMove(event.rootMove, moveUci) &&
        event.actorFrom.contains(EvidenceRef.normalizeMove(moveUci).take(2)) &&
        event.actorTo.contains(EvidenceRef.normalizeMove(moveUci).slice(2, 4))
    ),
    "a Cause proof step's plan-event occurrence must identify the same exact move"
  )

  private[judgment] def stableKey: String =
    List(
      plyOffset.toString,
      EvidenceRef.normalizeMove(moveUci),
      role.toString.toLowerCase,
      planEventOccurrence.map(_.stableKey).getOrElse("none")
    ).map(DirectCauseProofSegment.atom).mkString

/** A compact sentence-ready view of moves owned by one direct proof. Missing
  * or unsafe step extraction yields no segment; it never makes the Cause itself
  * disappear. The terminal relation applies to the final ordered step.
  */
final case class DirectCauseProofSegment(
    terminalRelation: DirectCauseProofTerminalRelation,
    steps: List[DirectCauseProofStep],
    planDependencies: List[PlanCausalEventDependency] = Nil
):
  require(steps.nonEmpty, "a Cause proof segment needs at least one owned step")
  require(
    steps.map(_.plyOffset) == steps.map(_.plyOffset).distinct.sorted,
    "Cause proof steps must have unique increasing root-relative offsets"
  )
  require(steps.head.plyOffset == 0, "a Cause proof segment must begin at the root action")
  require(
    steps.head.role == DirectCauseProofStepRole.RootAction,
    "a Cause proof segment must identify its root action"
  )
  require(
    planDependencies.isEmpty ||
      (planDependencies.forall(_.planConnectionProven) &&
        planDependencies.sliding(2).forall {
          case List(left, right) => left.to == right.from
          case _                 => true
        }),
    "a Cause proof segment's plan dependency occurrences must form one exact proven path"
  )

  private[judgment] def stableKey: String =
    List(
      terminalRelation.toString.toLowerCase,
      steps.map(_.stableKey).mkString,
      planDependencies.map(_.stableKey).mkString
    ).map(DirectCauseProofSegment.atom).mkString

object DirectCauseProofSegment:
  private val UciMove = "^[a-h][1-8][a-h][1-8][qrbn]?$".r
  private[judgment] def atom(value: String): String = s"${value.length}:$value"

  def from(proof: RootOwnedEffectProof): Option[DirectCauseProofSegment] =
    allFrom(proof) match
      case exact :: Nil => Some(exact)
      case _            => None

  def allFrom(proof: RootOwnedEffectProof): List[DirectCauseProofSegment] =
    proof match
      case RootOwnedEffectProof.LineEpisode(_, line, episode) =>
        lineEpisode(line, episode).toList
      case RootOwnedEffectProof.RootLineEvent(_, line, event) =>
        (for
          move <- exactMove(event.moveUci)
          if event.plyOffset == 0
          if EvidenceRef.sameMove(move, line.line.rootMove)
        yield rootOnly(DirectCauseProofTerminalRelation.IsRootLineEvent, move)).toList
      case RootOwnedEffectProof.RootRelation(source, relation) =>
        (for
          line <- source.line
          move <- exactMove(line.rootMove)
          if relation.mentionsLineMove(move)
        yield rootOnly(DirectCauseProofTerminalRelation.InstantiatesRelation, move)).toList
      case RootOwnedEffectProof.PlanResult(_, event, assessment, selectedInducedResponse) =>
        planResults(event, assessment, selectedInducedResponse)
      case RootOwnedEffectProof.PlanRestriction(_, event, _, _) =>
        exactMove(event.rootTransition.moveUci)
          .map(rootOnly(DirectCauseProofTerminalRelation.RestrictsOpponentResource, _))
          .toList
      case RootOwnedEffectProof.DefensiveRecaptureResource(_, _, resource) =>
        exactOrderedMoves(
          DirectCauseProofTerminalRelation.CreatesDefensiveRecaptureResource,
          resource.referenceProofMoves
        ).toList
      case RootOwnedEffectProof.StrategicAxis(primitive, _, _) =>
        allFrom(primitive)

  private def exactOrderedMoves(
      terminalRelation: DirectCauseProofTerminalRelation,
      moves: List[String]
  ): Option[DirectCauseProofSegment] =
    val exact = moves.flatMap(exactMove)
    Option.when(exact.size == moves.size && exact.nonEmpty)(
      DirectCauseProofSegment(
        terminalRelation,
        exact.zipWithIndex.map { case (move, index) =>
          DirectCauseProofStep(
            index,
            move,
            if index == 0 then DirectCauseProofStepRole.RootAction
            else if index == exact.size - 1 then DirectCauseProofStepRole.TerminalEvent
            else DirectCauseProofStepRole.CausalLink
          )
        }
      )
    )

  private def lineEpisode(
      line: LineFactEvidence,
      episode: RootOwnedCausalEpisode
  ): Option[DirectCauseProofSegment] =
    val replayPrefix = line.lineReplaySteps.take(episode.eventPlyOffset + 1)
    val rootPly = replayPrefix.headOption.map(_.ply)
    val exactReplay = replayPrefix.flatMap(step =>
      for
        root <- rootPly
        move <- exactMove(step.moveUci)
      yield (step.ply - root) -> move
    )
    val exactChain = episode.chainMoves.flatMap(exactMove)
    val replayMoves = exactReplay.map(_._2)
    Option
      .when(
        episode.line == line.line &&
          episode.eventPlyOffset >= 0 &&
          replayPrefix.size == episode.eventPlyOffset + 1 &&
          exactReplay.size == replayPrefix.size &&
          exactChain.size == episode.chainMoves.size &&
          exactReplay.size == exactChain.size &&
          exactReplay.map(_._1) == (0 to episode.eventPlyOffset).toList &&
          replayMoves.zip(exactChain).forall((left, right) => EvidenceRef.sameMove(left, right)) &&
          replayMoves.headOption.exists(EvidenceRef.sameMove(_, episode.actor.moveUci)) &&
          episode.consequence.eventMove.forall(move => replayMoves.lastOption.exists(EvidenceRef.sameMove(_, move)))
      )(
        DirectCauseProofSegment(
          DirectCauseProofTerminalRelation.ProducesLineConsequence,
          causalSteps(exactReplay)
        )
      )

  private def planResults(
      event: PlanCausalEventEvidence,
      assessment: PlanCausalResultAssessment,
      selectedInducedResponse: Option[PlanCausalResponse]
  ): List[DirectCauseProofSegment] =
    val dependencyPath = assessment.causalPath
    val path = event.causalEpisode.root :: dependencyPath.map(_.to)
    val rootPly = event.causalEpisode.root.step.ply
    val extractedPath = path.flatMap(node =>
      exactMove(node.moveUci).map(move =>
        (node.step.ply - rootPly, move, Some(node.identity))
      )
    )
    val exactSelectedResponse = selectedInducedResponse.flatMap { response =>
      val rawEligibleResponses = event.causalEpisode.responses.filter(candidate =>
        candidate.trigger == event.causalEpisode.root &&
          candidate.proven &&
          candidate.plyOffset == 1 &&
          PrincipalVariationEvidence.sameBoardState(
            event.causalEpisode.root.step.fenAfter,
            candidate.step.fenBefore
          )
      )
      val plyOffset = response.step.ply - rootPly
      for
        move <- exactMove(response.step.moveUci)
        if rawEligibleResponses == List(response)
        if response.trigger == event.causalEpisode.root
        if response.proven
        if response.plyOffset == 1 && plyOffset == 1
        if assessment.sourcePlyOffset == 2
        if assessment.sourceEvent.step.ply == response.step.ply + 1
        if PrincipalVariationEvidence.sameBoardState(
          response.step.fenAfter,
          assessment.sourceEvent.step.fenBefore
        )
      yield (plyOffset, move, None)
    }
    val responseIdentityReady = selectedInducedResponse.isEmpty || exactSelectedResponse.nonEmpty
    val extracted = (extractedPath ++ exactSelectedResponse).sortBy(_._1)
    Option.when(
      dependencyPath.nonEmpty &&
        path.nonEmpty &&
        extractedPath.size == path.size &&
        responseIdentityReady &&
        path.head == event.causalEpisode.root &&
        path.last == assessment.sourceEvent &&
        assessment.sourcePlyOffset == assessment.sourceEvent.step.ply - rootPly &&
        extracted.map(_._1) == extracted.map(_._1).distinct.sorted &&
        extracted.headOption.exists(_._1 == 0)
    )(
      DirectCauseProofSegment(
        DirectCauseProofTerminalRelation.RealizesPlanResult,
        extracted.zipWithIndex.map { case ((offset, move, planEvent), index) =>
          DirectCauseProofStep(
            offset,
            move,
            stepRole(index, extracted.size),
            planEventOccurrence = planEvent
          )
        },
        planDependencies = dependencyPath
      )
    ).toList

  private def causalSteps(moves: List[(Int, String)]): List[DirectCauseProofStep] =
    moves.zipWithIndex.map { case ((offset, move), index) =>
      DirectCauseProofStep(offset, move, stepRole(index, moves.size))
    }

  private def stepRole(index: Int, size: Int): DirectCauseProofStepRole =
    if index == 0 then DirectCauseProofStepRole.RootAction
    else if index == size - 1 then DirectCauseProofStepRole.TerminalEvent
    else DirectCauseProofStepRole.CausalLink

  private def rootOnly(
      relation: DirectCauseProofTerminalRelation,
      move: String
  ): DirectCauseProofSegment =
    DirectCauseProofSegment(
      relation,
      List(DirectCauseProofStep(0, move, DirectCauseProofStepRole.RootAction))
    )

  private def exactMove(move: String): Option[String] =
    val normalized = EvidenceRef.normalizeMove(move)
    Option.when(UciMove.matches(normalized))(normalized)

/** One sentence-ready causal channel. `binding.source` is the public carrier;
  * an exact wrapped primitive is retained in `binding.provenance`.
  */
final case class DirectCauseChannel(
    binding: EvidenceObjectBinding,
    directChange: DirectCausalChange,
    /** Exact primitive identity retained when a typed wrapper enriches the
      * sentence-ready binding. Not exposed as the public causal identity.
    */
    primitiveCausalSignature: Option[String] = None,
    rootOwnedProof: Option[RootOwnedEffectProof] = None,
    proofSegmentOccurrence: Option[DirectCauseProofSegment] = None,
    /** The public channel is preserved when equivalent carriers disagree about
      * their owned descriptor, but that channel may not enter importance or
      * fallback dominance until the disagreement is resolved upstream.
      */
    importanceDescriptorAmbiguous: Boolean = false,
    proofSegmentAmbiguous: Boolean = false
):
  /** Representation-independent semantic identity. Evidence/support ids,
    * proof ids, witnesses, and carrier provenance are intentionally excluded.
    * Length-prefixed tuple fields make role boundaries collision-safe.
    */
  def causalSignature: String =
    DirectCauseChannel.causalSignature(binding, directChange)

  /** Cross-comparison explanatory novelty intentionally ignores which move
    * carried the same resource. It is not a Cause identity or dominance key.
    */
  def explanatoryNoveltySignature: String =
    DirectCauseChannel.explanatoryNoveltySignature(binding, directChange)

  /** Same primitive event beneath any typed wrapper enrichment. This is used
    * only for bare-leaf shadowing; dominance uses the full public signature.
    */
  def primitiveSignature: String =
    primitiveCausalSignature.getOrElse(causalSignature)

  /** Public proof compression is a read-only view of this channel's exact
    * owned proof. It cannot be reconstructed from binding witnesses.
    */
  def proofSegment: Option[DirectCauseProofSegment] =
    Option.unless(proofSegmentAmbiguous)(
      proofSegmentOccurrence.orElse(rootOwnedProof.flatMap(DirectCauseProofSegment.from))
    ).flatten

  def rootOwnedEffectDescriptor: Option[RootOwnedEffectDescriptor] =
    rootOwnedProof.map { proof =>
      val descriptor = RootOwnedEffectDescriptorPolicy.describe(binding, proof)
      if importanceDescriptorAmbiguous then
        descriptor.copy(
          magnitude = DirectEffectMagnitudeKnowledge.Ambiguous,
          materialEventSalience = None
        )
      else descriptor
    }

  /** Exact truth occurrence beneath carrier ids and wrapper provenance. This
    * is the shared deduplication key for direct-channel production and Cause
    * record canonicalization.
    */
  private[chessjudgment] def exactOccurrenceFingerprint:
      RootOwnedEffectChannelOccurrenceFingerprint =
    RootOwnedEffectChannelOccurrenceFingerprint(
      causalSignature = causalSignature,
      primitiveSignature = primitiveSignature,
      descriptor = rootOwnedEffectDescriptor,
      descriptorAmbiguous = importanceDescriptorAmbiguous,
      rootOwnedProof = rootOwnedProof,
      proofSegment = proofSegmentOccurrence.orElse(rootOwnedProof.flatMap(DirectCauseProofSegment.from)),
      proofSegmentAmbiguous = proofSegmentAmbiguous,
      proofRole = binding.proofRole,
      unprovedSource = Option.when(rootOwnedProof.isEmpty)(binding.source)
    )

/** `DirectCauseChannel` remains the one stored/public causal representation;
  * this alias names its stronger root-owned role without introducing a second
  * actor/target/mechanism/consequence authority.
  */
type RootOwnedEffect = DirectCauseChannel

final case class RootOwnedEffectChannelOccurrenceFingerprint(
    causalSignature: String,
    primitiveSignature: String,
    descriptor: Option[RootOwnedEffectDescriptor],
    descriptorAmbiguous: Boolean,
    rootOwnedProof: Option[RootOwnedEffectProof],
    proofSegment: Option[DirectCauseProofSegment],
    proofSegmentAmbiguous: Boolean,
    proofRole: Option[RelativeCauseProofRole],
    unprovedSource: Option[EvidenceRef]
):
  /** Deterministic ordering only. Equality of this case class remains the
    * occurrence identity; the ordering key never substitutes for it.
    */
  private[judgment] def stableSortKey: String =
    val proofKind = rootOwnedProof.map {
      case _: RootOwnedEffectProof.LineEpisode                => "line-episode"
      case _: RootOwnedEffectProof.RootLineEvent              => "root-line-event"
      case _: RootOwnedEffectProof.RootRelation               => "root-relation"
      case _: RootOwnedEffectProof.PlanResult                 => "plan-result"
      case _: RootOwnedEffectProof.PlanRestriction            => "plan-restriction"
      case _: RootOwnedEffectProof.DefensiveRecaptureResource => "defensive-recapture-resource"
      case _: RootOwnedEffectProof.StrategicAxis              => "strategic-axis"
    }.getOrElse("unproved")
    List(
      causalSignature,
      primitiveSignature,
      descriptor.map(_.stableKey).getOrElse("none"),
      descriptorAmbiguous.toString,
      proofKind,
      rootOwnedProof.map(_.primitiveSource.id).getOrElse("none"),
      proofSegment.map(_.stableKey).getOrElse("none"),
      proofSegmentAmbiguous.toString,
      proofRole.map(_.toString.toLowerCase).getOrElse("none"),
      unprovedSource.map(_.id).getOrElse("none")
    ).map(RootOwnedEffectChannelOccurrenceFingerprint.atom).mkString

  private[judgment] def stablePublicId(ownerKey: String): String =
    MessageDigest
      .getInstance("SHA-256")
      .digest(
        RootOwnedEffectChannelOccurrenceFingerprint
          .atom(ownerKey)
          .concat(stableSortKey)
          .getBytes(StandardCharsets.UTF_8)
      )
      .map(byte => f"${byte & 0xff}%02x")
      .mkString

object RootOwnedEffectChannelOccurrenceFingerprint:
  private[judgment] def atom(value: String): String = s"${value.length}:$value"

object DirectCauseChannel:
  private def atom(value: String): String =
    val normalized = Option(value).getOrElse("").trim.toLowerCase
    s"${normalized.length}:$normalized"

  private def tuple(label: String, values: List[String]): String =
    s"${atom(label)}[${values.distinct.sorted.map(atom).mkString}]"

  private def causalSignature(
      binding: EvidenceObjectBinding,
      directChange: DirectCausalChange
  ): String =
    List(
      tuple("actor", binding.actor.map(_.signaturePart)),
      tuple("target", binding.target.map(_.signaturePart)),
      tuple("mechanism", binding.mechanism.map(_.signaturePart)),
      tuple("consequence", binding.consequence.map(_.signaturePart)),
      tuple(
        "line",
        binding.line.toList.map(line =>
          List(
            line.role.toString,
            EvidenceRef.normalizeMove(line.rootMove),
            line.rank.toString
          ).map(atom).mkString
        )
      ),
      tuple("horizon", binding.horizon.toList),
      tuple("change", List(directChange.toString))
    ).mkString

  private def explanatoryNoveltySignature(
      binding: EvidenceObjectBinding,
      directChange: DirectCausalChange
  ): String =
    List(
      tuple("target", binding.target.map(_.signaturePart)),
      tuple("mechanism", binding.mechanism.map(_.signaturePart)),
      tuple("consequence", binding.consequence.map(_.signaturePart)),
      tuple("horizon", binding.horizon.toList),
      tuple("change", List(directChange.toString))
    ).mkString

/** Evidence-id-independent truth carried by one canonical direct channel.
  *
  * This is the shared agreement boundary for C record merging and R
  * cross-comparison representative selection. Evidence ids, support counts,
  * comparison rank, and serialization carriers are deliberately absent.
  */
final case class RootOwnedEffectChannelTruthFingerprint(
    causalSignature: String,
    explanatoryNoveltySignature: String,
    descriptor: Option[RootOwnedEffectDescriptor],
    descriptorAmbiguous: Boolean,
    proofSegment: Option[DirectCauseProofSegment],
    proofSegmentAmbiguous: Boolean
):
  private[judgment] def causalAgreementKey: RootOwnedEffectCausalAgreement =
    RootOwnedEffectCausalAgreement(
      causalSignature = causalSignature,
      explanatoryNoveltySignature = explanatoryNoveltySignature,
      descriptor = descriptor,
      descriptorAmbiguous = descriptorAmbiguous
    )

  private[judgment] def explanatoryAgreementKey: RootOwnedEffectExplanatoryAgreement =
    RootOwnedEffectExplanatoryAgreement(
      explanatoryNoveltySignature = explanatoryNoveltySignature,
      descriptor = descriptor,
      descriptorAmbiguous = descriptorAmbiguous
    )

private[judgment] final case class RootOwnedEffectCausalAgreement(
    causalSignature: String,
    explanatoryNoveltySignature: String,
    descriptor: Option[RootOwnedEffectDescriptor],
    descriptorAmbiguous: Boolean
)

private[judgment] final case class RootOwnedEffectExplanatoryAgreement(
    explanatoryNoveltySignature: String,
    descriptor: Option[RootOwnedEffectDescriptor],
    descriptorAmbiguous: Boolean
)

final case class RootOwnedEffectTruthView(
    channels: List[RootOwnedEffectChannelTruthFingerprint]
):
  def causalSignatures: List[String] =
    channels.map(_.causalSignature).distinct.sorted


  private[judgment] def agreesCausallyWith(other: RootOwnedEffectTruthView): Boolean =
    descriptorInternallyUnambiguous &&
      other.descriptorInternallyUnambiguous &&
      channels.map(_.causalAgreementKey).toSet == other.channels.map(_.causalAgreementKey).toSet

  /** Cross-line novelty is certifiable only with a complete, internally
    * unambiguous descriptor for every channel. Proof-segment contents are
    * intentionally excluded: different moves can realize the same effect.
    */
  private[judgment] def certifiedExplanatoryAgreement:
      Option[Set[RootOwnedEffectExplanatoryAgreement]] =
    Option.when(
      channels.nonEmpty &&
        channels.forall(channel => !channel.descriptorAmbiguous) &&
        channels.forall(_.descriptor.exists(_.magnitude.comparisonReady))
    )(channels.map(_.explanatoryAgreementKey).toSet)

  private def descriptorInternallyUnambiguous: Boolean =
    channels.forall(channel => !channel.descriptorAmbiguous)

object RootOwnedEffectTruthView:
  def from(channels: List[DirectCauseChannel]): RootOwnedEffectTruthView =
    val canonical = EvidenceObjectBinding.canonicalCauseChannels(channels)
    RootOwnedEffectTruthView(
      canonical.map(channel =>
        RootOwnedEffectChannelTruthFingerprint(
          causalSignature = channel.causalSignature,
          explanatoryNoveltySignature = channel.explanatoryNoveltySignature,
          descriptor = channel.rootOwnedEffectDescriptor.map(_.semanticAgreementDescriptor),
          descriptorAmbiguous = channel.importanceDescriptorAmbiguous,
          proofSegment = channel.proofSegment,
          proofSegmentAmbiguous = channel.proofSegmentAmbiguous
        )
      )
    )

final case class EvidenceObjectBinding(
    source: EvidenceRef,
    actor: List[ConcreteChessObject] = Nil,
    target: List[ConcreteChessObject] = Nil,
    mechanism: List[ConcreteChessObject] = Nil,
    consequence: List[ConcreteChessObject] = Nil,
    witness: List[ConcreteChessObject] = Nil,
    line: Option[LineNodeRef] = None,
    horizon: Option[String] = None,
    proofRole: Option[RelativeCauseProofRole] = None,
    provenance: List[EvidenceRef] = Nil,
    lineOccurrence: Option[LineReplayStep] = None,
    planDependencies: List[PlanCausalEventDependency] = Nil,
    planGoalProof: Option[PlanCausalGoalProof] = None
):
  def hasConcreteObject: Boolean =
    target.nonEmpty ||
      (actor.nonEmpty && (mechanism.nonEmpty || consequence.nonEmpty))
  def specificTargetMechanismReady: Boolean =
    actor.nonEmpty &&
      target.exists(EvidenceObjectBinding.specificSurfaceTargetObject) &&
      mechanism.nonEmpty &&
      consequence.nonEmpty &&
      proofRole.forall(_ != RelativeCauseProofRole.ContextSupport)
  def signature: String =
    val parts = List(
      "actor" -> actor,
      "target" -> target,
      "mechanism" -> mechanism,
      "consequence" -> consequence,
      "witness" -> witness
    ).flatMap { case (role, objects) =>
      objects.distinctBy(_.signaturePart).sortBy(_.signaturePart).map(obj => s"$role=${obj.signaturePart}")
    }
    val linePart = line.map(line => s"line=${line.id}").toList
    val horizonPart = horizon.map(horizon => s"horizon=${horizon.trim.toLowerCase}").toList
    val proofPart = proofRole.map(role => s"proof=$role").toList
    val occurrencePart = lineOccurrence.map { step =>
      val values = List(
        step.ply.toString,
        EvidenceRef.normalizeMove(step.moveUci),
        step.fenBefore.trim,
        step.fenAfter.trim
      )
      s"occurrence=${values.map(value => s"${value.length}:$value").mkString}"
    }.toList
    val dependencyPart = planDependencies.map(dependency => s"dependency=${dependency.stableKey}")
    val planGoalProofPart = planGoalProof.map(proof => s"plan-goal-proof=${proof.stableKey}").toList
    (parts ++ linePart ++ horizonPart ++ proofPart ++ occurrencePart ++ dependencyPart ++ planGoalProofPart)
      .mkString("|")

  private[chessjudgment] def occurrenceSignature: String =
    def atom(value: String): String = s"${value.length}:$value"
    val provenanceIds = provenance.map(_.id).distinct.sorted
    s"$signature|source=${atom(source.id)}|provenance=${provenanceIds.map(atom).mkString("[", ",", "]")}"

/** Single semantic authority for the exact effect scope and measurable
  * magnitude carried by a root-owned proof. It deliberately ignores evidence
  * ids while retaining causal targets, plan identity, and complete strategic
  * axis identity.
  */
object EvidenceObjectBinding:
  private val ConcretePieceRoleKeys = Set("pawn", "knight", "bishop", "rook", "queen", "king")


  def fromClaim(claim: JudgmentClaim, graph: TypedEvidenceGraph): List[EvidenceObjectBinding] =
    val causeRecords =
      claim.evidence.flatMap(ref =>
        graph.record(ref).filter(graph.proofEligible).collect {
          case record @ EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) =>
            record -> cause
        }
      )
    if causeRecords.isEmpty then fromEvidenceRefs(graph, claim.evidence)
    else
      val causeOwnedEvidenceIds =
        causeRecords.flatMap { case (record, cause) =>
          record.ref.id ::
            (cause.supportEvidence ++
              cause.proof.toList.flatMap(_.sections.flatMap(_.sourceRefs))).map(_.id)
        }.toSet
      (
        causeRecords.flatMap { case (_, cause) => fromRelativeCauseForProjection(cause, graph) } ++
          fromEvidenceRefs(
            graph,
            claim.evidence.filterNot(ref => causeOwnedEvidenceIds.contains(ref.id))
          )
      ).distinctBy(_.signature)

  def fromEvidenceRefs(graph: TypedEvidenceGraph, refs: List[EvidenceRef]): List[EvidenceObjectBinding] =
    fromEvidenceRefs(graph, refs, Set.empty).distinctBy(_.signature)

  private def fromEvidenceRefs(
      graph: TypedEvidenceGraph,
      refs: List[EvidenceRef],
      visited: Set[String]
  ): List[EvidenceObjectBinding] =
    refs
      .flatMap(ref => graph.byId.get(ref.id))
      .filter(graph.proofEligible)
      .flatMap(record => fromRecord(record, graph, visited))
      .distinctBy(_.occurrenceSignature)

  def fromRelativeCause(cause: RelativeCauseFact, graph: TypedEvidenceGraph): List[EvidenceObjectBinding] =
    fromRelativeCause(cause, graph, Set.empty)

  /** Public Cause projection keeps each actor, target, mechanism, and
    * consequence on one root-owned causal channel. It never fills a missing
    * role from a sibling record or from the Cause label itself. Every public
    * channel uses the legally replayed event-root mover as its actor; later
    * participants and opposing threat actors remain targets or witnesses.
    */
  def fromRelativeCauseForProjection(
      cause: RelativeCauseFact,
      graph: TypedEvidenceGraph
  ): List[EvidenceObjectBinding] =
    RelativeCauseConstructionAdmission.admittedDirectChannels(cause, graph).map(_.binding)

  /** Canonical raw typed Cause channels. This preserves diagnostically useful
    * sparse channels; it is not authority to cross the public boundary.
    */
  def directCauseChannelsForProjection(
      cause: RelativeCauseFact,
      graph: TypedEvidenceGraph
  ): List[DirectCauseChannel] =
    canonicalCauseChannels(
      cause.proof.toList.flatMap(proof =>
        projectionChannelsFromProofSectionForCause(cause, proof.directProof, graph)
      )
    )

  /** C-stage raw ownership builder. It keeps only the Cause's own admissible
    * direct proof channels, but does not authorize them for public use;
    * `RelativeCauseConstructionAdmission` owns that final admission step.
    */
  private[chessjudgment] def rawDirectSentenceChannelsForProjection(
      cause: RelativeCauseFact,
      graph: TypedEvidenceGraph
  ): List[DirectCauseChannel] =
    val directSourceIds = cause.proof.toList.flatMap(_.directProof.sourceRefs.map(_.id)).toSet
    canonicalCauseChannels(
      directCauseChannelsForProjection(cause, graph)
        .filter { channel =>
          val objectBinding = channel.binding
          RootOwnedEffectPolicy.admits(cause, graph, channel) &&
            directSourceIds(objectBinding.source.id) &&
            objectBinding.proofRole.contains(RelativeCauseProofRole.DirectProof)
        }
    )

  /** Cause-neutral endpoint inventory for one complete exact line. It derives
    * all sentence-eligible root episodes, root-local events, and endgame
    * horizons from F records, independently of Cause draft generation.
    */
  private[chessjudgment] def comparisonEndpointLineObservations(
      source: EvidenceRef,
      payload: LineFactEvidence,
      rootPosition: PositionNodeRef
  ): ComparisonEndpointLineEffectInventories =
    comparisonEndpointLineProjections(source, payload, rootPosition).fold(
      ComparisonEndpointLineEffectInventories.incomplete
    ) { projected =>
      ComparisonEndpointLineEffectInventories(
        material = completeEndpointInventory(
          projected.collect {
            case item if item.family == ComparisonEndpointLineFamily.Material => item.observation
          }
        ),
        mate = completeEndpointInventory(
          projected.collect {
            case item if item.family == ComparisonEndpointLineFamily.Mate => item.observation
          }
        ),
        qualitative = completeEndpointInventory(
          projected.collect {
            case item if item.family == ComparisonEndpointLineFamily.Qualitative => item.observation
          }
        )
      )
    }

  private enum ComparisonEndpointLineFamily:
    case Material
    case Mate
    case Qualitative

  private final case class ComparisonEndpointLineProjection(
      family: ComparisonEndpointLineFamily,
      binding: EvidenceObjectBinding,
      proof: RootOwnedEffectProof,
      observation: Option[ComparisonEndpointEffectObservation]
  )

  /** Single eligibility authority shared by endpoint magnitude inventory and
    * neutral witness enumeration.
    */
  private def comparisonEndpointLineProjections(
      source: EvidenceRef,
      payload: LineFactEvidence,
      rootPosition: PositionNodeRef
  ): Option[List[ComparisonEndpointLineProjection]] =
    val rootEpisodes = payload
      .rootOwnedCausalEpisodes(payload.line.rootMove)
      .filter(episode =>
        Set(
          LineConsequenceKind.ImmediateReplyCheck,
          LineConsequenceKind.Mate,
          LineConsequenceKind.DrawResource,
          LineConsequenceKind.MaterialGain,
          LineConsequenceKind.MaterialLoss,
          LineConsequenceKind.RecaptureSequence,
          LineConsequenceKind.RecoveryWindow,
          LineConsequenceKind.Promotion,
          LineConsequenceKind.PromotionRace
        )(episode.consequence.kind)
      )
    val episodeProjected = rootEpisodes.map { episode =>
        val proof = RootOwnedEffectProof.LineEpisode(source, payload, episode)
        val binding = rootOwnedEpisodeBinding(source, payload, episode)
        val family = episode.consequence.kind match
          case LineConsequenceKind.MaterialGain | LineConsequenceKind.MaterialLoss =>
            ComparisonEndpointLineFamily.Material
          case LineConsequenceKind.Mate => ComparisonEndpointLineFamily.Mate
          case _                        => ComparisonEndpointLineFamily.Qualitative
        ComparisonEndpointLineProjection(
          family,
          binding,
          proof,
          ComparisonEndpointEffectObservationPolicy.fromLineEpisode(
            rootPosition = rootPosition,
            eventLine = payload.line,
            binding = binding,
            proof = proof,
            episode = episode
          )
        )
      }
    val actor = RootCausalActor.fromLineFact(payload, payload.line.rootMove)
    val eventProjected = actor.toList.flatMap { rootActor =>
      payload
        .eventsForRootMove(payload.line.rootMove)
        .filter(event =>
          Set(
            LineEventKind.Capture,
            LineEventKind.Recapture,
            LineEventKind.Check,
            LineEventKind.Mate,
            LineEventKind.Promotion,
            LineEventKind.Tempo,
            LineEventKind.DefenderMove
          )(event.kind)
        )
        .filterNot(event =>
          rootEpisodes.exists(episode =>
            strictRootLineEventSubsumedByEpisode(
              episodeSource = source,
              episodeLine = payload,
              episode = episode,
              rootEventSource = source,
              rootEventLine = payload,
              event = event,
              rootEventActor = rootActor
            )
          )
        )
        .map { event =>
          val binding = rootLocalEventBinding(source, payload, rootActor, event)
          val proof = RootOwnedEffectProof.RootLineEvent(source, payload, event)
          ComparisonEndpointLineProjection(
            if event.kind == LineEventKind.Mate then ComparisonEndpointLineFamily.Mate
            else ComparisonEndpointLineFamily.Qualitative,
            binding,
            proof,
            Option
              .when(binding.specificTargetMechanismReady)(())
              .flatMap { _ =>
                ComparisonEndpointEffectObservationPolicy.fromRootLineEvent(
                  rootPosition,
                  payload.line,
                  binding,
                  proof,
                  event
                )
              }
          )
        }
    }
    actor.map(_ => episodeProjected ++ eventProjected)

  /** Enumerates neutral F-stage witnesses for one comparison endpoint. The
    * supplied records are the assembler's exact endpoint neighborhood; this
    * method never broadens ownership by scanning an unrelated graph branch.
    */
  private[chessjudgment] def comparisonEndpointEvidenceWitnesses(
      sourceSide: RelativeCauseSourceSide,
      eventLine: LineNodeRef,
      rootPosition: PositionNodeRef,
      comparisonEvidence: EvidenceRef,
      comparison: CandidateComparisonFact,
      endpointRecords: List[EvidenceRecord],
      involvedRecords: List[EvidenceRecord],
      graph: TypedEvidenceGraph
  ): List[ComparisonEndpointEvidenceWitness] =
    val actor = graph.certifiedRootActorFor(eventLine)

    def observationFromOwnedProof(
        binding: EvidenceObjectBinding,
        proof: RootOwnedEffectProof,
        change: DirectCausalChange
    ): Option[ComparisonEndpointEffectObservation] =
      for
        rootActor <- actor
        stake <- RootOwnedEffectPolicy.effectStake(proof, rootActor)
        observation <- ComparisonEndpointEffectObservationPolicy.fromOwnedProof(
          rootPosition,
          binding,
          proof,
          change,
          stake,
          rootActor
        )
      yield observation

    def witness(
        binding: EvidenceObjectBinding,
        proof: RootOwnedEffectProof,
        observation: Option[ComparisonEndpointEffectObservation],
        selectedSegment: Option[DirectCauseProofSegment] = None
    ): List[ComparisonEndpointEvidenceWitness] =
      val availableSegments = DirectCauseProofSegment.allFrom(proof)
      val segments = selectedSegment match
        case Some(selected) if availableSegments.contains(selected) => List(selected)
        case Some(_)                                                => Nil
        case None                                                   => availableSegments
      for
        line <- binding.line.toList
        if SemanticLineKey.same(line, eventLine)
        if binding.actor.nonEmpty && binding.target.nonEmpty &&
          binding.mechanism.nonEmpty && binding.consequence.nonEmpty
        segment <- segments
      yield ComparisonEndpointEvidenceWitness(
        sourceSide = sourceSide,
        line = eventLine,
        binding = binding.copy(line = Some(eventLine), proofRole = None),
        rootOwnedProof = proof,
        proofSegment = segment,
        effectDescriptor = RootOwnedEffectDescriptorPolicy.describe(binding, proof),
        carrierAncestorSourceIds = graph
          .record(binding.source)
          .toList
          .flatMap(graph.parentClosure)
          .map(_.ref.id)
          .distinct
          .sorted,
        observation = observation
      )

    val primitiveWitnesses = endpointRecords.flatMap {
      case EvidenceRecord(ref, payload: LineFactEvidence, _)
          if payload.line == eventLine &&
            RootOwnedEffectPolicy.sameCausalRootOccurrence(ref.position, rootPosition) =>
        comparisonEndpointLineProjections(ref, payload, rootPosition)
          .toList
          .flatten
          .flatMap(item => witness(item.binding, item.proof, item.observation))

      case EvidenceRecord(ref, payload: RelationFactEvidence, _)
          if RootOwnedEffectPolicy.relationRecordOwnsEventRoot(graph, ref, payload, eventLine) =>
        actor.toList.flatMap { exactActor =>
          val proof = RootOwnedEffectProof.RootRelation(ref, payload)
          val binding = rootRelationBinding(ref, payload, exactActor, eventLine)
          witness(binding, proof, observationFromOwnedProof(binding, proof, DirectCausalChange.Occurred))
        }

      case EvidenceRecord(ref, payload: PlanCausalEventEvidence, _)
          if actor.exists(rootActor =>
            RootOwnedEffectPolicy.planEventOwnsRoot(ref, payload, eventLine, rootActor.color)
          ) =>
        actor.toList.flatMap { exactActor =>
          val resultWitnesses = (
            payload.exactRobustPublicResultAssessments ++
              payload.exactRefutedPublicResultAssessments
          ).distinct.flatMap { assessment =>
            val proof = RootOwnedEffectProof.PlanResult(ref, payload, assessment)
            planAssessmentRouteBindings(ref, payload, exactActor, assessment, eventLine, proof).flatMap {
              case (binding, segment) =>
                witness(
                  binding,
                  proof,
                  ComparisonEndpointEffectObservationPolicy.fromExactPlanResult(
                    rootPosition,
                    eventLine,
                    ref,
                    payload,
                    assessment,
                    graph
                  ),
                  selectedSegment = Some(segment)
                )
            }
          }
          val inducedResponseMoveOrderWitnesses =
            ComparisonEndpointEffectObservationPolicy
              .exactInducedResponseMoveOrder(comparison, sourceSide, ref, payload, graph)
              .flatMap { case (assessment, response) =>
                inducedResponseMoveOrderBindings(
                  comparison,
                  sourceSide,
                  ref,
                  payload,
                  exactActor,
                  assessment,
                  response,
                  eventLine
                ).flatMap { case (binding, segment) =>
                  val proof = RootOwnedEffectProof.PlanResult(
                    ref,
                    payload,
                    assessment,
                    selectedInducedResponse = Some(response)
                  )
                  witness(
                    binding,
                    proof,
                    ComparisonEndpointEffectObservationPolicy.fromExactPlanResult(
                      rootPosition,
                      eventLine,
                      ref,
                      payload,
                      assessment,
                      graph,
                      Some(binding),
                      selectedInducedResponse = Some(response)
                    ),
                    selectedSegment = Some(segment)
                  )
                }
              }
          val restrictionWitnesses = RootOwnedEffectPolicy
            .planRestrictionProofs(ref, payload)
            .flatMap { case (consequence, proof) =>
              val binding = planDirectConsequenceBinding(
                ref,
                payload,
                exactActor,
                consequence,
                eventLine
              )
              witness(
                binding,
                proof,
                observationFromOwnedProof(binding, proof, DirectCausalChange.Prevented)
              )
            }
          resultWitnesses ++ inducedResponseMoveOrderWitnesses ++
            restrictionWitnesses
        }

      case _ => Nil
    }

    val defensiveWitnesses =
      if sourceSide != RelativeCauseSourceSide.Reference || comparisonEvidence.id.isEmpty then Nil
      else
        for
          _ <- Option.when(
            comparison.kind == CandidateComparisonKind.PlayedVsBest &&
              comparison.comparison.verdict.isActionableLoss &&
              comparison.referenceLine == eventLine
          )(()).toList
          resource <- comparison.defensiveRecaptureResource.toList
          comparisonRecord <- graph.record(comparisonEvidence).toList
          candidateLineRecord <- comparisonRecord.parents.flatMap(graph.record).collect {
            case EvidenceRecord(lineRef, line: LineFactEvidence, _)
                if line.line == comparison.candidateLine => lineRef -> line
          } match
            case exact :: Nil => List(exact)
            case _            => Nil
          referenceLine <- comparisonRecord.parents.flatMap(graph.record).collect {
            case EvidenceRecord(_, line: LineFactEvidence, _)
                if line.line == comparison.referenceLine => line
          } match
            case exact :: Nil => List(exact)
            case _            => Nil
          (candidateLineRef, candidateLine) = candidateLineRecord
          if PlayedVsBestDefensiveRecaptureResource.proves(
            comparison,
            rootPosition,
            candidateLine,
            referenceLine,
            resource
          )
          exactActor <- actor.toList
          proof = RootOwnedEffectProof.DefensiveRecaptureResource(
            comparisonEvidence,
            comparison,
            resource
          )
          binding = defensiveRecaptureBinding(
            comparisonEvidence,
            comparison,
            resource,
            candidateLineRef,
            exactActor
          )
          exact <- witness(
            binding,
            proof,
            observationFromOwnedProof(binding, proof, DirectCausalChange.Occurred)
          ).toList
        yield exact

    val baseWitnesses = primitiveWitnesses ++ defensiveWitnesses
    val baseBySource = baseWitnesses.groupBy(_.binding.source.id)
    val involvedById = involvedRecords.map(record => record.ref.id -> record).toMap

    def witnessesThroughCarrier(
        source: EvidenceRef,
        visited: Set[String]
    ): List[ComparisonEndpointEvidenceWitness] =
      if visited(source.id) then Nil
      else
        val direct = baseBySource.getOrElse(source.id, Nil)
        val carried = involvedById.get(source.id).toList.flatMap {
          case EvidenceRecord(carrier, payload: TacticalMechanismEvidence, _)
              if RootOwnedEffectPolicy.tacticalCarrierOwnsEventRoot(graph, carrier, payload, eventLine) =>
            actor.toList.flatMap { exactActor =>
              payload.signals.flatMap { signal =>
                signal.source.toList.flatMap { primitiveSource =>
                  graph.record(primitiveSource).toList
                    .filter(record => tacticalSignalMatchesPayload(signal, record.payload))
                    .flatMap(_ => witnessesThroughCarrier(primitiveSource, visited + source.id))
                    .flatMap { primitive =>
                      val binding = primitive.binding.copy(
                        source = carrier,
                        provenance = (primitive.binding.source :: primitive.binding.provenance)
                          .distinctBy(_.id),
                        actor = rootActorObjects(exactActor),
                        line = Some(eventLine)
                      )
                      witness(binding, primitive.rootOwnedProof, primitive.observation)
                    }
                }
              }
            }
          case EvidenceRecord(carrier, payload: StrategicMechanismEvidence, _)
              if carrier.line.contains(eventLine) =>
            actor.toList.flatMap { exactActor =>
              payload.signals.flatMap(signal =>
                signal.axis.toList.flatMap { axis =>
                  witnessesThroughCarrier(signal.source, visited + source.id).flatMap { primitive =>
                    RootOwnedEffectPolicy
                      .strategicProof(
                        primitive.rootOwnedProof,
                        axis,
                        None,
                        signal.planResultAssessment
                          .map(StrategicAxisPlanResultBinding(signal.source, _))
                          .toList
                      )
                      .toList
                      .flatMap { proof =>
                        val binding = strategicAxisBinding(
                          carrier,
                          primitive.binding,
                          exactActor,
                          eventLine,
                          axis,
                          None
                        )
                        witness(
                          binding,
                          proof,
                          ComparisonEndpointEffectObservationPolicy.fromStrategicAxis(
                            rootPosition,
                            exactActor,
                            axis
                          )
                        )
                      }
                  }
                }
              )
            }
          case EvidenceRecord(carrier, payload: StrategicMechanismContrastEvidence, _)
              if payload.comparisonKind == comparison.kind &&
                payload.referenceLine == comparison.referenceLine &&
                payload.candidateLine == comparison.candidateLine =>
            actor.toList.flatMap { exactActor =>
              payload.sustainedActionableComparisonsFor(sourceSide).flatMap { axisComparison =>
                graph.strategicComparisonSourceRefs(axisComparison, sourceSide).flatMap { primitiveSource =>
                  witnessesThroughCarrier(primitiveSource, visited + source.id).flatMap { primitive =>
                    RootOwnedEffectPolicy
                      .strategicProof(
                        primitive.rootOwnedProof,
                        axisComparison.axis,
                        Some(axisComparison.outcome),
                        axisComparison.planResultsFor(sourceSide)
                      )
                      .toList
                      .flatMap { proof =>
                        val binding = strategicAxisBinding(
                          carrier,
                          primitive.binding,
                          exactActor,
                          eventLine,
                          axisComparison.axis,
                          Some(axisComparison.outcome)
                        )
                        witness(
                          binding,
                          proof,
                          ComparisonEndpointEffectObservationPolicy.fromStrategicAxis(
                            rootPosition,
                            exactActor,
                            axisComparison.axis
                          )
                        )
                      }
                  }
                }
              }
            }
          case _ => Nil
        }
        direct ++ carried

    val carrierWitnesses = involvedRecords.flatMap(record =>
      witnessesThroughCarrier(record.ref, Set.empty)
    )
    (baseWitnesses ++ carrierWitnesses).distinct.sortBy(witness =>
      (
        witness.rootOwnedProof.toString,
        witness.binding.signature,
        witness.primitiveProofSource.id
      )
    )

  /** Reconstruct the primitive endpoint observation from the exact owned
    * proof. Cause-specific tactical/strategic carrier refinements stay on the
    * public channel and cannot split the comparison inventory.
    */
  private[chessjudgment] def comparisonEndpointPrimitiveObservation(
      cause: RelativeCauseFact,
      channel: DirectCauseChannel,
      graph: TypedEvidenceGraph
  ): Option[ComparisonEndpointEffectObservation] =
    for
      proof <- channel.rootOwnedProof
      eventLine <- channel.binding.line
      if RootOwnedEffectPolicy.admits(cause, graph, channel)
      observation <- proof match
        case exact @ RootOwnedEffectProof.LineEpisode(source, payload, episode) =>
          ComparisonEndpointEffectObservationPolicy.fromLineEpisode(
            rootPosition = cause.comparisonEvidence.position,
            eventLine = eventLine,
            binding = rootOwnedEpisodeBinding(source, payload, episode),
            proof = exact,
            episode = episode
          )
        case exact @ RootOwnedEffectProof.RootLineEvent(source, payload, event) =>
          RootCausalActor
            .fromLineFact(payload, eventLine.rootMove)
            .flatMap(actor =>
              ComparisonEndpointEffectObservationPolicy.fromRootLineEvent(
                rootPosition = cause.comparisonEvidence.position,
                eventLine = eventLine,
                binding = rootLocalEventBinding(source, payload, actor, event),
                proof = exact,
                event = event
              )
            )
        case exact @ RootOwnedEffectProof.DefensiveRecaptureResource(_, _, _) =>
          graph.certifiedRootActorFor(eventLine).flatMap(actor =>
            ComparisonEndpointEffectObservationPolicy.fromOwnedProof(
              rootPosition = cause.comparisonEvidence.position,
              binding = channel.binding,
              proof = exact,
              directChange = DirectCausalChange.Occurred,
              stake = RootOwnedEffectStake.ActorValue,
              actor = actor
            )
          )
        case proof =>
          RootOwnedEffectPolicy.exactPlanResultPrimitive(proof).flatMap {
            case (source, event, assessment, selectedInducedResponse) =>
              ComparisonEndpointEffectObservationPolicy.fromExactPlanResult(
                rootPosition = cause.comparisonEvidence.position,
                eventLine = eventLine,
                source = source,
                event = event,
                assessment = assessment,
                graph = graph,
                exactBinding = Option.when(cause.kind == RelativeCauseKind.WrongMoveOrder)(
                  channel.binding
                ),
                selectedInducedResponse = selectedInducedResponse
              )
          }
    yield observation

  private[chessjudgment] def completeEndpointObservations(
      projected: List[Option[ComparisonEndpointEffectObservation]]
  ): Option[Set[ComparisonEndpointEffectObservation]] =
    val successful = projected.flatten
    val oneMagnitudePerScope = successful
      .groupBy(_.scope)
      .forall(_._2.map(_.magnitude).distinct.size == 1)
    Option.when(
      successful.size == projected.size &&
        oneMagnitudePerScope
    )(successful.toSet)

  private def completeEndpointInventory(
      projected: List[Option[ComparisonEndpointEffectObservation]]
  ): ComparisonEndpointEffectInventory =
    completeEndpointObservations(projected)
      .map(ComparisonEndpointEffectInventory.Complete.apply)
      .getOrElse(ComparisonEndpointEffectInventory.Incomplete)

  private[judgment] def canonicalCauseChannels(channels: List[DirectCauseChannel]): List[DirectCauseChannel] =
    val canonicalOriginalGroups = channels
      .groupBy(_.exactOccurrenceFingerprint)
      .toList
      .map { case (_, equivalents) => canonicalCauseChannelGroup(equivalents) }
    val barePrimitivesByOccurrence = canonicalOriginalGroups
      .filter(channel => channel.binding.provenance.isEmpty && channel.primitiveCausalSignature.isEmpty)
      .groupBy(primitiveOccurrenceKey)
    val ambiguityPropagatedToWrappers = canonicalOriginalGroups.map { channel =>
      Option
        .when(channel.binding.provenance.nonEmpty)(
          barePrimitivesByOccurrence.get(primitiveOccurrenceKey(channel)).flatMap {
            case primitive :: Nil => Some(primitive)
            case _                => None
          }
        )
        .flatten
        .fold(channel)(primitive =>
          channel.copy(
            importanceDescriptorAmbiguous =
              channel.importanceDescriptorAmbiguous || primitive.importanceDescriptorAmbiguous,
            proofSegmentAmbiguous = channel.proofSegmentAmbiguous || primitive.proofSegmentAmbiguous
          )
        )
    }
    val rootEventRedundancyCollapsed = collapseSubsumedRootLineEvents(
      ambiguityPropagatedToWrappers
    )
    val wrappedPrimitiveOccurrences = rootEventRedundancyCollapsed.flatMap(channel =>
      Option.when(
        channel.binding.provenance.nonEmpty && !nonComparativePlanResultWrapper(channel)
      )(primitiveOccurrenceKey(channel))
    ).toSet
    rootEventRedundancyCollapsed
      .filter(channel =>
        channel.binding.provenance.nonEmpty || !wrappedPrimitiveOccurrences(primitiveOccurrenceKey(channel))
      )
      .sortBy(channel =>
        (
          channel.causalSignature,
          channel.rootOwnedProof.map(_.primitiveSource.id).getOrElse(channel.binding.source.id),
          proofSegmentSortKey(channel.proofSegmentOccurrence.orElse(channel.proofSegment)),
          channel.binding.source.id
        )
      )

  private final case class PrimitiveOccurrenceKey(
      primitiveSignature: String,
      primitiveProof: Option[RootOwnedEffectProof],
      proofSegment: Option[DirectCauseProofSegment],
      proofRole: Option[RelativeCauseProofRole],
      unprovedSource: Option[EvidenceRef]
  )

  private def primitiveOccurrenceKey(channel: DirectCauseChannel): PrimitiveOccurrenceKey =
    PrimitiveOccurrenceKey(
      primitiveSignature = channel.primitiveSignature,
      primitiveProof = channel.rootOwnedProof.map(_.primitiveProof),
      proofSegment = channel.proofSegmentOccurrence.orElse(channel.proofSegment),
      proofRole = channel.binding.proofRole,
      unprovedSource = Option.when(channel.rootOwnedProof.isEmpty)(channel.binding.source)
    )

  private def proofSegmentSortKey(segment: Option[DirectCauseProofSegment]): String =
    segment.map(_.stableKey).getOrElse("")

  /** A local plan-axis refinement without a comparison outcome cannot replace
    * the exact PlanResult primitive in typed endpoint inventory. Keep both;
    * an outcome-bearing strategic wrapper remains independently comparable.
    */
  private def nonComparativePlanResultWrapper(channel: DirectCauseChannel): Boolean =
    channel.rootOwnedProof.exists {
      case RootOwnedEffectProof.StrategicAxis(_: RootOwnedEffectProof.PlanResult, _, None) => true
      case _                                                                              => false
    }

  /** A root-local event is a qualitative view of the same effect when an
    * exact root-owned episode proves the event and its resulting outcome at
    * the same replay step. Collapse only that bare primitive view. Delayed
    * outcomes, recaptures/recovery, other results, and enriched mechanisms
    * remain independent public channels.
    */
  private def collapseSubsumedRootLineEvents(
      channels: List[DirectCauseChannel]
  ): List[DirectCauseChannel] =
    val episodes = channels.flatMap(exactBareLineEpisode)
    channels.filterNot(channel =>
      exactBareRootLineEvent(channel).exists { rootEvent =>
        episodes.exists(episode => lineEpisodeSubsumesRootEvent(episode, rootEvent))
      }
    )

  private final case class ExactRootLineEventChannel(
      channel: DirectCauseChannel,
      source: EvidenceRef,
      line: LineFactEvidence,
      event: LineMoveEvent,
      actor: RootCausalActor
  )

  private final case class ExactLineEpisodeChannel(
      channel: DirectCauseChannel,
      source: EvidenceRef,
      line: LineFactEvidence,
      episode: RootOwnedCausalEpisode
  )

  private def exactBareRootLineEvent(
      channel: DirectCauseChannel
  ): Option[ExactRootLineEventChannel] =
    channel.rootOwnedProof.flatMap {
      case RootOwnedEffectProof.RootLineEvent(source, line, event)
          if bareUnambiguousPrimitive(channel) &&
            Set(LineEventKind.Capture, LineEventKind.Mate, LineEventKind.Promotion)(event.kind) =>
        for
          actor <- RootCausalActor.fromLineFact(line, line.line.rootMove)
          if sourceOwnsExactRootReplay(source, line)
          if line.eventsForRootMove(line.line.rootMove).contains(event)
          if event.side.forall(_ == actor.color)
          if exactPrimitiveBinding(
            channel.binding,
            rootLocalEventBinding(source, line, actor, event)
          )
        yield ExactRootLineEventChannel(channel, source, line, event, actor)
      case _ => None
    }

  private def exactBareLineEpisode(
      channel: DirectCauseChannel
  ): Option[ExactLineEpisodeChannel] =
    channel.rootOwnedProof.flatMap {
      case RootOwnedEffectProof.LineEpisode(source, line, episode)
          if bareUnambiguousPrimitive(channel) &&
            Set(
              LineConsequenceKind.MaterialGain,
              LineConsequenceKind.Mate,
              LineConsequenceKind.Promotion
            )(episode.consequence.kind) =>
        Option.when(
          sourceOwnsExactRootReplay(source, line) &&
            line.rootOwnedCausalEpisodes(line.line.rootMove).contains(episode) &&
            exactPrimitiveBinding(
              channel.binding,
              rootOwnedEpisodeBinding(source, line, episode)
            )
        )(ExactLineEpisodeChannel(channel, source, line, episode))
      case _ => None
    }

  private def bareUnambiguousPrimitive(channel: DirectCauseChannel): Boolean =
    channel.binding.provenance.isEmpty &&
      channel.primitiveCausalSignature.isEmpty &&
      !channel.importanceDescriptorAmbiguous &&
      !channel.proofSegmentAmbiguous

  private def lineEpisodeSubsumesRootEvent(
      episode: ExactLineEpisodeChannel,
      rootEvent: ExactRootLineEventChannel
  ): Boolean =
    episode.channel.directChange == rootEvent.channel.directChange &&
      episode.channel.binding.proofRole == rootEvent.channel.binding.proofRole &&
      strictRootLineEventSubsumedByEpisode(
        episodeSource = episode.source,
        episodeLine = episode.line,
        episode = episode.episode,
        rootEventSource = rootEvent.source,
        rootEventLine = rootEvent.line,
        event = rootEvent.event,
        rootEventActor = rootEvent.actor
      )

  /** Shared strict containment policy for raw endpoint inventory and public
    * Cause channels. It removes only a root-local qualitative view whose exact
    * replay, actor, target, and typed outcome are already owned by one root
    * episode. Any disagreement leaves both facts visible and therefore keeps
    * downstream admission fail-closed.
    */
  private def strictRootLineEventSubsumedByEpisode(
      episodeSource: EvidenceRef,
      episodeLine: LineFactEvidence,
      episode: RootOwnedCausalEpisode,
      rootEventSource: EvidenceRef,
      rootEventLine: LineFactEvidence,
      event: LineMoveEvent,
      rootEventActor: RootCausalActor
  ): Boolean =
    val episodeBinding = rootOwnedEpisodeBinding(episodeSource, episodeLine, episode)
    val rootEventBinding = rootLocalEventBinding(
      rootEventSource,
      rootEventLine,
      rootEventActor,
      event
    )
    sourceOwnsExactRootReplay(episodeSource, episodeLine) &&
      sourceOwnsExactRootReplay(rootEventSource, rootEventLine) &&
      episodeLine.rootOwnedCausalEpisodes(episodeLine.line.rootMove).contains(episode) &&
      rootEventLine.eventsForRootMove(rootEventLine.line.rootMove).contains(event) &&
      event.side.forall(_ == rootEventActor.color) &&
      episode.actor == rootEventActor &&
      episode.consequence.beneficiary.contains(rootEventActor.color) &&
      SemanticLineKey.same(episodeLine.line, rootEventLine.line) &&
      SemanticLineKey.same(episode.line, rootEventLine.line) &&
      PrincipalVariationEvidence.sameBoardState(
        episodeSource.position.fen,
        rootEventSource.position.fen
      ) &&
      episode.eventPlyOffset == event.plyOffset &&
      event.plyOffset == 0 &&
      episode.consequence.rootMove.exists(
        EvidenceRef.sameMove(_, rootEventLine.line.rootMove)
      ) &&
      episode.consequence.eventMove.exists(EvidenceRef.sameMove(_, event.moveUci)) &&
      sameExactReplayEvent(episodeLine, episode, rootEventLine, event) &&
      sameConcreteObjects(episodeBinding.actor, rootEventBinding.actor) &&
      sameConcreteObjects(episodeBinding.target, rootEventBinding.target) &&
      rootEventBinding.target.nonEmpty &&
      directLineEffectIncludes(
        episodeLine,
        episode,
        rootEventLine,
        event,
        rootEventActor
      )

  private def directLineEffectIncludes(
      episodeLine: LineFactEvidence,
      episode: RootOwnedCausalEpisode,
      rootEventLine: LineFactEvidence,
      event: LineMoveEvent,
      rootEventActor: RootCausalActor
  ): Boolean =
    (event.kind, episode.consequence.kind) match
      case (LineEventKind.Capture, LineConsequenceKind.MaterialGain) =>
        (for
          eventCapture <- exactEventCapture(rootEventLine, event)
          episodeCapture <- episodeLine.uniqueMaterialCaptureFor(episode)
        yield
          !eventCapture.recapture &&
            !episodeCapture.recapture &&
            eventCapture.valueCp > 0 &&
            eventCapture.side == rootEventActor.color &&
            episodeCapture.side == rootEventActor.color &&
            eventCapture == episodeCapture
        ).contains(true)
      case (LineEventKind.Mate, LineConsequenceKind.Mate) =>
        true
      case (LineEventKind.Promotion, LineConsequenceKind.Promotion) =>
        true
      case _ =>
        false

  private def exactEventCapture(
      line: LineFactEvidence,
      event: LineMoveEvent
  ): Option[LineMaterialCapture] =
    line.uniqueMaterialCaptureAt(event.plyOffset, event.moveUci)

  private def sameExactReplayEvent(
      episodeLine: LineFactEvidence,
      episode: RootOwnedCausalEpisode,
      eventLine: LineFactEvidence,
      event: LineMoveEvent
  ): Boolean =
    (for
      episodeStep <- episodeLine.lineReplaySteps.lift(episode.eventPlyOffset)
      eventStep <- eventLine.lineReplaySteps.lift(event.plyOffset)
    yield
      EvidenceRef.sameMove(episodeStep.moveUci, eventStep.moveUci) &&
        EvidenceRef.sameMove(episodeStep.moveUci, event.moveUci) &&
        PrincipalVariationEvidence.sameBoardState(
          episodeStep.fenBefore,
          eventStep.fenBefore
        ) &&
        PrincipalVariationEvidence.sameBoardState(
          episodeStep.fenAfter,
          eventStep.fenAfter
        )
    ).contains(true)

  private def sourceOwnsExactRootReplay(
      source: EvidenceRef,
      line: LineFactEvidence
  ): Boolean =
    source.line.exists(SemanticLineKey.same(_, line.line)) &&
      line.lineReplaySteps.headOption.exists(step =>
        EvidenceRef.sameMove(step.moveUci, line.line.rootMove) &&
          PrincipalVariationEvidence.sameBoardState(source.position.fen, step.fenBefore)
      )

  private def exactPrimitiveBinding(
      actual: EvidenceObjectBinding,
      expected: EvidenceObjectBinding
  ): Boolean =
    sameConcreteObjects(actual.actor, expected.actor) &&
      sameConcreteObjects(actual.target, expected.target) &&
      actual.target.nonEmpty &&
      sameConcreteObjects(actual.mechanism, expected.mechanism) &&
      sameConcreteObjects(actual.consequence, expected.consequence) &&
      actual.line.exists(line => expected.line.exists(SemanticLineKey.same(line, _))) &&
      actual.horizon.map(_.trim.toLowerCase) == expected.horizon.map(_.trim.toLowerCase)

  private def sameConcreteObjects(
      left: List[ConcreteChessObject],
      right: List[ConcreteChessObject]
  ): Boolean =
    left.map(_.signaturePart).toSet == right.map(_.signaturePart).toSet

  /** Descriptor and proof-segment agreement is established before bare
    * primitives are shadowed by semantic wrappers. Evidence ids may choose a
    * carrier only after any semantic disagreement has been made fail-closed.
    */
  private def canonicalCauseChannelGroup(
      equivalents: List[DirectCauseChannel]
  ): DirectCauseChannel =
    val descriptorVariants = equivalents.map(_.rootOwnedEffectDescriptor).distinct
    val descriptorAmbiguous =
      equivalents.exists(_.importanceDescriptorAmbiguous) || descriptorVariants.size != 1
    val segmentAmbiguous =
      equivalents.exists(channel =>
        channel.proofSegmentAmbiguous || channel.rootOwnedProof.exists { proof =>
          val available = DirectCauseProofSegment.allFrom(proof)
          channel.proofSegmentOccurrence match
            case Some(selected) => !available.contains(selected)
            case None           => available.size > 1
        }
      )
    equivalents
      .minBy(channel =>
        (
          if channel.binding.provenance.nonEmpty then 0 else 1,
          -channel.binding.provenance.map(_.id).distinct.size,
          channel.binding.source.id,
          channel.binding.provenance.map(_.id).distinct.sorted.mkString("\u0000"),
          channel.binding.signature
        )
      )
      .copy(
        importanceDescriptorAmbiguous = descriptorAmbiguous,
        proofSegmentAmbiguous = segmentAmbiguous
      )

  private def projectionChannelsFromProofSectionForCause(
      cause: RelativeCauseFact,
      section: RelativeCauseProofSection,
      graph: TypedEvidenceGraph
  ): List[DirectCauseChannel] =
    canonicalCauseChannels(
      section.sourceRefs
        .flatMap(ref =>
          graph.record(ref).toList.flatMap {
            case EvidenceRecord(_, payload: LineFactEvidence, _) =>
              fromLineFactForCauseProjection(ref, payload, cause, section, graph)
            case EvidenceRecord(_, payload: RelationFactEvidence, _) =>
              fromRelationForCauseProjection(ref, payload, cause, graph)
            case EvidenceRecord(_, payload: TacticalMechanismEvidence, _) =>
              fromTacticalMechanismForCauseProjection(
                ref,
                payload,
                cause,
                section,
                graph,
                Set(ref.id)
              )
            case EvidenceRecord(_, payload: PlanCausalEventEvidence, _) =>
              fromPlanCausalEventForCauseProjection(ref, payload, cause, graph)
            case EvidenceRecord(_, CandidateComparisonEvidence(payload), _) =>
              fromCandidateComparisonForCauseProjection(ref, payload, cause, graph)
            case EvidenceRecord(_, payload: StrategicMechanismEvidence, _) =>
              fromStrategicMechanismForCauseProjection(
                ref,
                payload,
                cause,
                section,
                graph,
                Set(ref.id)
              )
            case EvidenceRecord(_, payload: StrategicMechanismContrastEvidence, _) =>
              fromStrategicContrastForCauseProjection(
                ref,
                payload,
                cause,
                section,
                graph,
                Set(ref.id)
              )
            case _ =>
              Nil
          }
        )
        .map(channel => channel.copy(binding = channel.binding.copy(proofRole = Some(section.role))))
    )

  private def fromLineFactForCauseProjection(
      ref: EvidenceRef,
      payload: LineFactEvidence,
      cause: RelativeCauseFact,
      section: RelativeCauseProofSection,
      graph: TypedEvidenceGraph
  ): List[DirectCauseChannel] =
    val binding = graph.requiredRelativeCauseBinding(cause)
    if !ref.line.contains(binding.eventLine) || payload.line != binding.eventLine then Nil
    else
      val rootMove = binding.eventLine.rootMove
      val episodeBindings = graph
        .relativeCauseRootOwnedCausalEpisodes(cause, section)
        .flatMap {
          case (source, episode) if source.id == ref.id =>
            RootOwnedEffectPolicy
              .certify(
                cause,
                graph,
                rootOwnedEpisodeBinding(ref, payload, episode),
                RootOwnedEffectProof.LineEpisode(ref, payload, episode)
              )
              .toList
          case _ => Nil
        }
      val rootActor = RootCausalActor.fromLineFact(payload, rootMove)
      val rootEventBindings = rootActor.toList.flatMap(actor =>
        graph
          .relativeCauseOwnedLineEvents(cause, section)
          .flatMap {
            case (source, event)
                if source.id == ref.id &&
                  event.side.forall(_ == actor.color) =>
              RootOwnedEffectPolicy
                .certify(
                  cause,
                  graph,
                  rootLocalEventBinding(ref, payload, actor, event),
                  RootOwnedEffectProof.RootLineEvent(ref, payload, event)
                )
            case _ => Nil
          }
      )
      canonicalCauseChannels(episodeBindings ++ rootEventBindings)

  private def fromCandidateComparisonForCauseProjection(
      ref: EvidenceRef,
      comparison: CandidateComparisonFact,
      cause: RelativeCauseFact,
      graph: TypedEvidenceGraph
  ): List[DirectCauseChannel] =
    (for
      registered <- graph.comparisonFor(cause)
      if registered == comparison
      if cause.kind == RelativeCauseKind.DefensiveResource
      if cause.sourceSide == RelativeCauseSourceSide.Reference
      if cause.attribution.kind == CauseAttributionKind.ReferenceCreatesResource
      resource <- comparison.defensiveRecaptureResource
      comparisonRecord <- graph.record(ref)
      candidateLineRef <- comparisonRecord.parents.flatMap(graph.record).collect {
        case EvidenceRecord(lineRef, line: LineFactEvidence, _)
            if line.line == comparison.candidateLine => lineRef
      } match
        case exact :: Nil => Some(exact)
        case _            => None
      actor <- graph.certifiedRootActorFor(comparison.referenceLine)
      binding = defensiveRecaptureBinding(
        ref,
        comparison,
        resource,
        candidateLineRef,
        actor
      )
      proof = RootOwnedEffectProof.DefensiveRecaptureResource(ref, comparison, resource)
    yield RootOwnedEffectPolicy.certify(cause, graph, binding, proof)).toList.flatten

  private def defensiveRecaptureBinding(
      source: EvidenceRef,
      comparison: CandidateComparisonFact,
      resource: PlayedVsBestDefensiveRecaptureResource,
      candidateLineRef: EvidenceRef,
      actor: RootCausalActor
  ): EvidenceObjectBinding =
    EvidenceObjectBinding(
      source = source,
      actor = rootActorObjects(actor),
      target = (
        objectOf(EvidenceObjectKind.Square, resource.target.key) ++
          objectOf(EvidenceObjectKind.Piece, resource.removedOccupantRole.name)
      ).distinctBy(_.signaturePart),
      mechanism = List(
        "CreatesLegalAlternativeRecapture",
        "PreservesOtherRecapturer"
      ).flatMap(objectOf(EvidenceObjectKind.Mechanism, _)),
      consequence = objectOf(
        EvidenceObjectKind.Consequence,
        "LegalAlternativeRecaptureResourceCreated"
      ),
      witness = (
        resource.referenceProofMoves.flatMap(objectOf(EvidenceObjectKind.Move, _)) ++
          List(
            resource.referenceRecapturerRole,
            resource.preservedAlternativeRecapturerRole
          ).flatMap(role => objectOf(EvidenceObjectKind.Piece, role.name)) ++
          lineObject(comparison.referenceLine)
      ).distinctBy(_.signaturePart),
      line = Some(comparison.referenceLine),
      horizon = Some("ply:2"),
      provenance = List(candidateLineRef)
    )

  private final case class CauseProjectionRoot(
      binding: RelativeCauseBinding,
      actor: RootCausalActor
  )

  private def causeProjectionRoot(
      ref: EvidenceRef,
      cause: RelativeCauseFact,
      graph: TypedEvidenceGraph,
      requireRefLine: Boolean = true
  ): Option[CauseProjectionRoot] =
    for
      binding <- graph.relativeCauseBinding(cause)
      if !requireRefLine || ref.line.contains(binding.eventLine)
      if RootOwnedEffectPolicy.sameCausalRootOccurrence(
        ref.position,
        cause.comparisonEvidence.position
      )
      actor <- graph.certifiedRootActorFor(binding.eventLine)
    yield CauseProjectionRoot(binding, actor)

  private def fromRelationForCauseProjection(
      ref: EvidenceRef,
      payload: RelationFactEvidence,
      cause: RelativeCauseFact,
      graph: TypedEvidenceGraph
  ): List[DirectCauseChannel] =
    causeProjectionRoot(ref, cause, graph).toList.flatMap { root =>
      RootOwnedEffectPolicy.certify(
        cause,
        graph,
        rootRelationBinding(ref, payload, root.actor, root.binding.eventLine),
        RootOwnedEffectProof.RootRelation(ref, payload)
      )
    }

  private def rootRelationBinding(
      source: EvidenceRef,
      payload: RelationFactEvidence,
      actor: RootCausalActor,
      eventLine: LineNodeRef
  ): EvidenceObjectBinding =
    val targetParticipants = payload.participants.filter(participant =>
      Set(
        RelationParticipantRole.Target,
        RelationParticipantRole.Supported,
        RelationParticipantRole.King,
        RelationParticipantRole.Blocker
      )(participant.participantRole)
    )
    EvidenceObjectBinding(
      source = source,
      actor = rootActorObjects(actor),
      target = (
        payload.targetSquares.flatMap(square => objectOf(EvidenceObjectKind.Square, square.key)) ++
          targetParticipants.flatMap(participantObjects)
      ).distinctBy(_.signaturePart),
      mechanism = objectOf(EvidenceObjectKind.Relation, payload.kind.toString) ++
        objectOf(EvidenceObjectKind.Mechanism, payload.detail.detailName),
      consequence = objectOf(EvidenceObjectKind.Consequence, payload.kind.toString),
      witness = (
        payload.participants.flatMap(participantObjects) ++
          payload.focusSquares.flatMap(square => objectOf(EvidenceObjectKind.Square, square.key)) ++
          payload.lineMoves.flatMap(move => objectOf(EvidenceObjectKind.Move, move)) ++
          lineObject(eventLine)
      ).distinctBy(_.signaturePart),
      line = Some(eventLine)
    )

  private def fromPlanCausalEventForCauseProjection(
      ref: EvidenceRef,
      payload: PlanCausalEventEvidence,
      cause: RelativeCauseFact,
      graph: TypedEvidenceGraph
  ): List[DirectCauseChannel] =
    canonicalCauseChannels(
      causeProjectionRoot(ref, cause, graph).toList.flatMap { root =>
        val eventLine = root.binding.eventLine
        cause.kind match
          case RelativeCauseKind.PlanImprovement | RelativeCauseKind.PlanContradiction =>
            RootOwnedEffectPolicy.planResultProofs(cause, ref, payload).flatMap {
              case (exact, proof) =>
                planAssessmentRouteBindings(ref, payload, root.actor, exact, eventLine, proof).flatMap {
                  case (binding, segment) =>
                    RootOwnedEffectPolicy.certify(
                      cause,
                      graph,
                      binding,
                      proof,
                      selectedProofSegment = Some(segment)
                    )
                }
            }
          case RelativeCauseKind.WrongMoveOrder =>
            graph.comparisonFor(cause).toList.flatMap { comparison =>
              ComparisonEndpointEffectObservationPolicy
                .exactInducedResponseMoveOrder(
                  comparison,
                  cause.sourceSide,
                  ref,
                  payload,
                  graph
                )
                .flatMap { case (exact, response) =>
                  inducedResponseMoveOrderBindings(
                    comparison,
                    cause.sourceSide,
                    ref,
                    payload,
                    root.actor,
                    exact,
                    response,
                    eventLine
                  ).flatMap { case (binding, segment) =>
                    RootOwnedEffectPolicy.certify(
                      cause,
                      graph,
                      binding,
                      RootOwnedEffectProof.PlanResult(
                        ref,
                        payload,
                        exact,
                        selectedInducedResponse = Some(response)
                      ),
                      selectedProofSegment = Some(segment)
                    )
                  }
                }
            }
          case RelativeCauseKind.OpponentRestriction =>
            val deterrenceChannels = RootOwnedEffectPolicy.planRestrictionProofs(ref, payload).flatMap {
              case (consequence, proof) =>
                RootOwnedEffectPolicy
                  .certify(
                    cause,
                    graph,
                    planDirectConsequenceBinding(ref, payload, root.actor, consequence, eventLine),
                    proof
                  )
                  .toList
            }
            deterrenceChannels
          case _ =>
            Nil
      }
    )

  private[chessjudgment] def planAssessmentRouteBindings(
      ref: EvidenceRef,
      payload: PlanCausalEventEvidence,
      actor: RootCausalActor,
      assessment: PlanCausalResultAssessment,
      eventLine: LineNodeRef,
      proof: RootOwnedEffectProof
  ): List[(EvidenceObjectBinding, DirectCauseProofSegment)] =
    DirectCauseProofSegment.allFrom(proof).map { segment =>
      planAssessmentBinding(
        ref,
        payload,
        actor,
        assessment,
        eventLine,
        segment.planDependencies
      ) -> segment
    }

  private def planAssessmentBinding(
      ref: EvidenceRef,
      payload: PlanCausalEventEvidence,
      actor: RootCausalActor,
      assessment: PlanCausalResultAssessment,
      eventLine: LineNodeRef,
      dependencies: List[PlanCausalEventDependency]
  ): EvidenceObjectBinding =
    val sourceEvent = assessment.sourceEvent
    val targets = (
      assessment.consequence.goalSubjectFacts.flatMap(subjectObject) ++
        PlanCausalEpisode
          .consequenceTargetSquares(assessment.consequence)
          .flatMap(square => objectOf(EvidenceObjectKind.Square, square.key))
    ).distinctBy(_.signaturePart)
    EvidenceObjectBinding(
      source = ref,
      actor = rootActorObjects(actor),
      target = targets,
      mechanism = (
        dependencies.flatMap(dependency => objectOf(EvidenceObjectKind.Mechanism, dependency.kind.toString)) ++
          objectOf(EvidenceObjectKind.Mechanism, assessment.goalProof.mechanism.toString) ++
          objectOf(EvidenceObjectKind.Mechanism, assessment.consequence.kind.toString)
      ).distinctBy(_.signaturePart),
      consequence = objectOf(
        EvidenceObjectKind.Consequence,
        s"${assessment.consequence.anchorKey}:${assessment.robustness}"
      ),
      witness = (
        objectOf(EvidenceObjectKind.PlanSubject, payload.planId.id) ++
          objectOf(EvidenceObjectKind.Move, sourceEvent.moveUci) ++
          sourceEvent.identity.actorRole.toList.flatMap(role => objectOf(EvidenceObjectKind.Piece, role)) ++
          assessment.realizedObservations.flatMap(observation => lineObject(observation.line)) ++
          assessment.realizedObservations.flatMap(_.realizationMoves).flatMap(objectOf(EvidenceObjectKind.Move, _)) ++
          dependencies.flatMap(_.proofSquares).flatMap(square => objectOf(EvidenceObjectKind.Square, square.key)) ++
          dependencies.flatMap(_.proofPieceRoles).flatMap(role => objectOf(EvidenceObjectKind.Piece, role.name)) ++
          lineObject(eventLine)
      ).distinctBy(_.signaturePart),
      line = Some(eventLine),
      horizon = Some(s"ply:${assessment.sourcePlyOffset}"),
      planDependencies = dependencies,
      planGoalProof = Some(assessment.goalProof)
    )

  /** Comparison-specific rendering of an already selected F-stage plan
    * result. It preserves the exact induced responder as the target while the
    * underlying dependency and consequence remain the PlanResult primitive.
    */
  private[chessjudgment] def inducedResponseMoveOrderBindings(
      comparison: CandidateComparisonFact,
      sourceSide: RelativeCauseSourceSide,
      ref: EvidenceRef,
      payload: PlanCausalEventEvidence,
      actor: RootCausalActor,
      assessment: PlanCausalResultAssessment,
      response: PlanCausalResponse,
      eventLine: LineNodeRef
  ): List[(EvidenceObjectBinding, DirectCauseProofSegment)] =
    val endpointLines = sourceSide match
      case RelativeCauseSourceSide.Reference =>
        Some(comparison.referenceLine -> comparison.candidateLine)
      case RelativeCauseSourceSide.Candidate =>
        Some(comparison.candidateLine -> comparison.referenceLine)
      case RelativeCauseSourceSide.Shared | RelativeCauseSourceSide.Mixed =>
        None
    val proof = RootOwnedEffectProof.PlanResult(
      ref,
      payload,
      assessment,
      selectedInducedResponse = Some(response)
    )
    for
      (sourceLine, delayedLine) <- endpointLines.toList
      if SemanticLineKey.same(sourceLine, eventLine)
      responseLegal <- response.certifiedLegalStep.toList
      responseActor <- RootCausalActor.fromLegalStep(response.step.moveUci, responseLegal).toList
      if responseActor.color == !actor.color
      (base, segment) <- planAssessmentRouteBindings(ref, payload, actor, assessment, eventLine, proof)
    yield
      base.copy(
        target = rootActorObjects(responseActor),
        mechanism = base.mechanism,
        consequence = (
          base.consequence ++ objectOf(EvidenceObjectKind.Move, delayedLine.rootMove)
        ).distinctBy(_.signaturePart),
        witness = (
          base.witness ++
            lineObject(sourceLine) ++
            lineObject(delayedLine) ++
            objectOf(EvidenceObjectKind.Move, payload.rootMove) ++
            objectOf(EvidenceObjectKind.Move, response.step.moveUci) ++
            objectOf(EvidenceObjectKind.Move, assessment.sourceEvent.moveUci)
        ).distinctBy(_.signaturePart)
      ) -> segment

  private def planDirectConsequenceBinding(
      ref: EvidenceRef,
      payload: PlanCausalEventEvidence,
      actor: RootCausalActor,
      consequence: TransitionConsequence,
      eventLine: LineNodeRef
  ): EvidenceObjectBinding =
    EvidenceObjectBinding(
      source = ref,
      actor = rootActorObjects(actor),
      target = (
        objectOf(EvidenceObjectKind.PlanSubject, payload.planId.id) ++
          consequence.goalSubjectFacts.flatMap(subjectObject) ++
          structuralOpponentRestrictionTargetObjects(consequence)
      ).distinctBy(_.signaturePart),
      mechanism = objectOf(EvidenceObjectKind.Mechanism, consequence.kind.toString),
      consequence = objectOf(EvidenceObjectKind.Consequence, consequence.anchorKey),
      witness = (
        consequence.witnessSubjectFacts.flatMap(subjectObject) ++
          lineObject(eventLine)
      ).distinctBy(_.signaturePart),
      line = Some(eventLine)
    )

  private def fromTacticalMechanismForCauseProjection(
      ref: EvidenceRef,
      payload: TacticalMechanismEvidence,
      cause: RelativeCauseFact,
      section: RelativeCauseProofSection,
      graph: TypedEvidenceGraph,
      visited: Set[String]
  ): List[DirectCauseChannel] =
    canonicalCauseChannels(
      causeProjectionRoot(ref, cause, graph).toList.flatMap { root =>
        val eventLine = root.binding.eventLine
        val wrapperRootOwned =
          RootOwnedEffectPolicy.tacticalCarrierOwnsEventRoot(graph, ref, payload, eventLine)
        if !wrapperRootOwned then Nil
        else
          payload.signals.flatMap { signal =>
            signal.source.toList.flatMap { source =>
              graph.record(source).toList
                .filter(record => tacticalSignalMatchesPayload(signal, record.payload))
                .flatMap(_ =>
                  primitiveCauseProjectionBindings(
                    source,
                    cause,
                    section,
                    graph,
                    visited
                  ).flatMap(channel =>
                    carryRootOwnedBinding(
                      cause = cause,
                      graph = graph,
                      carrier = ref,
                      actor = root.actor,
                      channel = channel,
                      line = eventLine
                    )
                  )
                )
            }
          }
      }
    )

  private def tacticalSignalMatchesPayload(
      signal: TacticalMechanismSignal,
      payload: EvidencePayload
  ): Boolean =
    signal.kind match
      case TacticalMechanismSignalKind.Relation => payload.isInstanceOf[RelationFactEvidence]
      case TacticalMechanismSignalKind.LineConsequence | TacticalMechanismSignalKind.LineEvent =>
        payload.isInstanceOf[LineFactEvidence]
      case TacticalMechanismSignalKind.MateBranch =>
        payload.isInstanceOf[LineFactEvidence]

  private def fromStrategicMechanismForCauseProjection(
      ref: EvidenceRef,
      payload: StrategicMechanismEvidence,
      cause: RelativeCauseFact,
      section: RelativeCauseProofSection,
      graph: TypedEvidenceGraph,
      visited: Set[String]
  ): List[DirectCauseChannel] =
    if RelativeCauseKind.requiresExactPlanResult(cause.kind) then Nil
    else
      canonicalCauseChannels(
        causeProjectionRoot(ref, cause, graph).toList.flatMap { root =>
          payload.signals.flatMap { signal =>
            signal.axis.toList
              .flatMap { axis =>
                primitiveCauseProjectionBindings(
                  signal.source,
                  cause,
                  section,
                  graph,
                  visited
                ).flatMap(channel =>
                  carryRootOwnedBinding(
                    cause = cause,
                    graph = graph,
                    carrier = ref,
                    actor = root.actor,
                    channel = channel,
                    line = root.binding.eventLine,
                    semanticMechanism = objectOf(EvidenceObjectKind.Mechanism, axis.kind.toString),
                    semanticConsequence = objectOf(EvidenceObjectKind.Consequence, axis.stableKey),
                    strategicAxis = Some(axis),
                    authorizedPlanResults = signal.planResultAssessment
                      .map(StrategicAxisPlanResultBinding(signal.source, _))
                      .toList
                  )
                )
              }
          }
        }
      )

  private def fromStrategicContrastForCauseProjection(
      ref: EvidenceRef,
      payload: StrategicMechanismContrastEvidence,
      cause: RelativeCauseFact,
      section: RelativeCauseProofSection,
      graph: TypedEvidenceGraph,
      visited: Set[String]
  ): List[DirectCauseChannel] =
    if RelativeCauseKind.requiresExactPlanResult(cause.kind) then Nil
    else
      canonicalCauseChannels(
        causeProjectionRoot(ref, cause, graph, requireRefLine = false).toList.flatMap { root =>
          val comparisonMatches = graph.comparisonFor(cause).exists(fact =>
            fact.kind == payload.comparisonKind &&
              fact.referenceLine == payload.referenceLine &&
              fact.candidateLine == payload.candidateLine
          )
          val sourceLineMatches = cause.sourceSide match
            case RelativeCauseSourceSide.Reference => root.binding.eventLine == payload.referenceLine
            case RelativeCauseSourceSide.Candidate => root.binding.eventLine == payload.candidateLine
            case RelativeCauseSourceSide.Shared | RelativeCauseSourceSide.Mixed => false
          if !comparisonMatches || !sourceLineMatches then Nil
          else
            graph
              .relativeCauseStrategicAxisComparisons(cause, section)
              .collect { case (source, comparison) if source.id == ref.id => comparison }
              .flatMap { comparison =>
                graph.strategicComparisonSourceRefs(comparison, cause.sourceSide).flatMap { source =>
                  primitiveCauseProjectionBindings(
                    source,
                    cause,
                    section,
                    graph,
                    visited
                  ).flatMap(channel =>
                    carryRootOwnedBinding(
                      cause = cause,
                      graph = graph,
                      carrier = ref,
                      actor = root.actor,
                      channel = channel,
                      line = root.binding.eventLine,
                      semanticMechanism = objectOf(EvidenceObjectKind.Mechanism, comparison.axis.kind.toString),
                      semanticConsequence = objectOf(EvidenceObjectKind.Consequence, comparison.axis.stableKey) ++
                        objectOf(EvidenceObjectKind.Consequence, comparison.outcome.toString),
                      strategicAxis = Some(comparison.axis),
                      strategicOutcome = Some(comparison.outcome),
                      authorizedPlanResults = comparison.planResultsFor(cause.sourceSide)
                    )
                  )
                }
              }
        }
      )

  private def primitiveCauseProjectionBindings(
      ref: EvidenceRef,
      cause: RelativeCauseFact,
      section: RelativeCauseProofSection,
      graph: TypedEvidenceGraph,
      visited: Set[String]
  ): List[DirectCauseChannel] =
    if visited(ref.id) then Nil
    else
      graph.record(ref).toList.flatMap {
        case EvidenceRecord(_, payload: LineFactEvidence, _) =>
          val ownedSection = section.copy(sourceRefs = List(ref))
          fromLineFactForCauseProjection(ref, payload, cause, ownedSection, graph)
        case EvidenceRecord(_, payload: RelationFactEvidence, _) =>
          fromRelationForCauseProjection(ref, payload, cause, graph)
        case EvidenceRecord(_, payload: PlanCausalEventEvidence, _) =>
          fromPlanCausalEventForCauseProjection(ref, payload, cause, graph)
        case EvidenceRecord(_, payload: TacticalMechanismEvidence, _) =>
          fromTacticalMechanismForCauseProjection(
            ref,
            payload,
            cause,
            section,
            graph,
            visited + ref.id
          )
        case EvidenceRecord(_, payload: StrategicMechanismEvidence, _) =>
          fromStrategicMechanismForCauseProjection(
            ref,
            payload,
            cause,
            section,
            graph,
            visited + ref.id
          )
        case _ =>
          Nil
      }

  private def carryRootOwnedBinding(
      cause: RelativeCauseFact,
      graph: TypedEvidenceGraph,
      carrier: EvidenceRef,
      actor: RootCausalActor,
      channel: DirectCauseChannel,
      line: LineNodeRef,
      semanticMechanism: List[ConcreteChessObject] = Nil,
      semanticConsequence: List[ConcreteChessObject] = Nil,
      strategicAxis: Option[StrategicAxisDetail] = None,
      strategicOutcome: Option[StrategicAxisComparisonOutcome] = None,
      authorizedPlanResults: List[StrategicAxisPlanResultBinding] = Nil
  ): List[DirectCauseChannel] =
    val binding = channel.binding
    val actorSignature = rootActorObjects(actor).map(_.signaturePart).toSet
    val bindingActorSignature = binding.actor.map(_.signaturePart).toSet
    for
      primitiveProof <- channel.rootOwnedProof.toList
      if actorSignature.subsetOf(bindingActorSignature)
      proof <- (strategicAxis match
        case Some(axis) =>
          RootOwnedEffectPolicy.strategicProof(
            primitiveProof,
            axis,
            strategicOutcome,
            authorizedPlanResults
          )
        case None       => Some(primitiveProof)
      ).toList
      carried <- RootOwnedEffectPolicy.certify(
        cause,
        graph,
        strategicAxis.fold(
          binding.copy(
            source = carrier,
            provenance = (binding.source :: binding.provenance).distinctBy(_.id),
            actor = rootActorObjects(actor),
            mechanism = (binding.mechanism ++ semanticMechanism).distinctBy(_.signaturePart),
            consequence = (binding.consequence ++ semanticConsequence).distinctBy(_.signaturePart),
            witness = binding.witness,
            line = Some(line),
            horizon = binding.horizon
          )
        )(axis =>
          strategicAxisBinding(
            carrier,
            binding,
            actor,
            line,
            axis,
            strategicOutcome
          )
        ),
        proof,
        channel.primitiveCausalSignature.orElse(Some(channel.causalSignature)),
        selectedProofSegment = channel.proofSegmentOccurrence.orElse(channel.proofSegment)
      )
    yield carried.copy(
      importanceDescriptorAmbiguous = channel.importanceDescriptorAmbiguous,
      proofSegmentAmbiguous = channel.proofSegmentAmbiguous
    )

  private def strategicAxisBinding(
      carrier: EvidenceRef,
      primitive: EvidenceObjectBinding,
      actor: RootCausalActor,
      line: LineNodeRef,
      axis: StrategicAxisDetail,
      outcome: Option[StrategicAxisComparisonOutcome]
  ): EvidenceObjectBinding =
    primitive.copy(
      source = carrier,
      provenance = (primitive.source :: primitive.provenance).distinctBy(_.id),
      actor = rootActorObjects(actor),
      mechanism = (
        primitive.mechanism ++ objectOf(EvidenceObjectKind.Mechanism, axis.kind.toString)
      ).distinctBy(_.signaturePart),
      consequence = (
        primitive.consequence ++
          objectOf(EvidenceObjectKind.Consequence, axis.stableKey) ++
          outcome.toList.flatMap(value =>
            objectOf(EvidenceObjectKind.Consequence, value.toString)
          )
      ).distinctBy(_.signaturePart),
      witness = primitive.witness,
      line = Some(line),
      horizon = primitive.horizon
    )

  private def rootOwnedEpisodeBinding(
      source: EvidenceRef,
      payload: LineFactEvidence,
      episode: RootOwnedCausalEpisode
  ): EvidenceObjectBinding =
    val target = episodeTargetObjects(payload, episode)
    EvidenceObjectBinding(
      source = source,
      actor = rootActorObjects(episode.actor),
      target = target,
      mechanism = episode.links.flatMap(link => objectOf(EvidenceObjectKind.Mechanism, link.kind.toString)),
      consequence = objectOf(EvidenceObjectKind.Consequence, episode.consequence.kind.toString),
      witness = episode.chainMoves.flatMap(move => objectOf(EvidenceObjectKind.Move, move)) ++
        episode.links.flatMap(link => objectOf(EvidenceObjectKind.Square, link.anchor.key)) ++
        lineObject(episode.line),
      line = Some(episode.line),
      horizon = Some(s"ply:${episode.eventPlyOffset}")
    )

  private def episodeTargetObjects(
      payload: LineFactEvidence,
      episode: RootOwnedCausalEpisode
  ): List[ConcreteChessObject] =
    val square = objectOf(EvidenceObjectKind.Square, episode.target.key)
    val role = episode.consequence.kind match
      case LineConsequenceKind.MaterialGain | LineConsequenceKind.MaterialLoss |
          LineConsequenceKind.RecaptureSequence | LineConsequenceKind.RecoveryWindow =>
        payload.lineReplaySteps
          .lift(episode.eventPlyOffset)
          .flatMap(step => payload.uniqueMaterialCaptureAt(episode.eventPlyOffset, step.moveUci))
          .toList
          .flatMap(capture => roleObject(Some(capture.capturedRole)))
      case LineConsequenceKind.ImmediateReplyCheck | LineConsequenceKind.Mate |
          LineConsequenceKind.DrawResource =>
        roleObject(Some(EvidencePieceRole(King.name)))
      case LineConsequenceKind.Promotion | LineConsequenceKind.PromotionRace =>
        payload.lineEvents
          .find(event =>
            event.plyOffset == episode.eventPlyOffset &&
              event.kind == LineEventKind.Promotion &&
              episode.chainMoves.lastOption.exists(EvidenceRef.sameMove(_, event.moveUci))
          )
          .toList
          .flatMap(event => roleObject(event.targetRole))
      case LineConsequenceKind.Sacrifice =>
        episode.consequence.sacrificeOccurrence.toList
          .flatMap(occurrence => roleObject(Some(occurrence.acceptance.capturedRole)))
      case LineConsequenceKind.ForcedTheme =>
        Nil
    (square ++ role).distinctBy(_.signaturePart)

  private def rootLocalEventBinding(
      source: EvidenceRef,
      payload: LineFactEvidence,
      actor: RootCausalActor,
      event: LineMoveEvent
  ): EvidenceObjectBinding =
    EvidenceObjectBinding(
      source = source,
      actor = rootActorObjects(actor),
      target = rootLocalEventTargets(payload, event),
      mechanism = objectOf(EvidenceObjectKind.Mechanism, event.kind.toString),
      consequence = objectOf(EvidenceObjectKind.Consequence, event.kind.toString),
      witness = objectOf(EvidenceObjectKind.Move, event.moveUci) ++
        squareObject(event.square) ++
        roleObject(event.targetRole) ++
        lineObject(payload.line),
      line = Some(payload.line),
      horizon = Some(s"ply:${event.plyOffset}")
    )

  private def rootLocalEventTargets(
      payload: LineFactEvidence,
      event: LineMoveEvent
  ): List[ConcreteChessObject] =
    event.kind match
      case LineEventKind.Check | LineEventKind.Mate =>
        squareObject(event.square) ++ roleObject(event.targetRole.filter(_.name.equalsIgnoreCase(King.name)))
      case LineEventKind.Capture | LineEventKind.Recapture =>
        payload.materialCaptures
          .find(capture =>
            capture.plyOffset == event.plyOffset && EvidenceRef.sameMove(capture.moveUci, event.moveUci)
          )
          .toList
          .flatMap(capture =>
            squareObject(Some(capture.square)) ++ roleObject(Some(capture.capturedRole))
          )
      case LineEventKind.Promotion =>
        squareObject(event.square) ++ roleObject(event.targetRole)
      case LineEventKind.Tempo | LineEventKind.DefenderMove =>
        squareObject(event.square) ++ roleObject(event.targetRole)
      case _ =>
        Nil

  private def rootActorObjects(actor: RootCausalActor): List[ConcreteChessObject] =
    (
      objectOf(EvidenceObjectKind.Move, actor.moveUci) ++
        objectOf(EvidenceObjectKind.Side, colorKey(actor.color)) ++
        objectOf(EvidenceObjectKind.Piece, actor.role.name) ++
        objectOf(EvidenceObjectKind.Square, actor.from.key) ++
        objectOf(EvidenceObjectKind.Square, actor.to.key)
    ).distinctBy(_.signaturePart)

  private def fromRelativeCause(
      cause: RelativeCauseFact,
      graph: TypedEvidenceGraph,
      visited: Set[String]
  ): List[EvidenceObjectBinding] =
    val proofBindings =
      cause.proof.toList.flatMap { proof =>
        bindingsFromProofSectionForCause(cause, proof.directProof, graph, visited) ++
          bindingsFromProofSectionForCause(cause, proof.contrastProof, graph, visited) ++
          bindingsFromProofSectionForCause(cause, proof.contextSupport, graph, visited)
      }
    val proofSourceIds =
      cause.proof.toList.flatMap(_.sections.flatMap(_.sourceRefs)).map(_.id).toSet
    val residualSupport =
      cause.supportEvidence.filterNot(ref =>
        proofSourceIds.contains(ref.id) ||
          (
            cause.strategicCauseKind &&
              graph.record(ref).exists(_.payload.isInstanceOf[StrategicMechanismContrastEvidence])
          )
      )
    val supportBindings =
      fromEvidenceRefs(graph, residualSupport, visited)
        .map(_.copy(proofRole = Some(RelativeCauseProofRole.ContextSupport)))
    (proofBindings ++ supportBindings).distinctBy(_.occurrenceSignature)

  def objectSignatures(bindings: List[EvidenceObjectBinding]): List[String] =
    bindings.filter(_.hasConcreteObject).map(_.signature).distinct.sorted

  private[chessjudgment] def specificSurfaceTargetObject(obj: ConcreteChessObject): Boolean =
    obj.kind match
      case EvidenceObjectKind.Square | EvidenceObjectKind.File | EvidenceObjectKind.Pawn | EvidenceObjectKind.Piece =>
        true
      case _ =>
        false

  private[chessjudgment] def goalTargetObjectGroups(
      consequence: TransitionConsequence
  ): List[Set[ConcreteChessObject]] =
    consequence.goalSubjectFacts.flatMap { subject =>
      val group = subjectObject(subject)
        .filter(obj => specificSurfaceTargetObject(obj) && obj.kind != EvidenceObjectKind.PlanSubject)
        .toSet
      val pieceRolesValid = group.collect {
        case obj if obj.kind == EvidenceObjectKind.Piece => obj.key.toLowerCase
      }.forall(ConcretePieceRoleKeys)
      Option.when(group.nonEmpty && pieceRolesValid)(group)
    }.distinct

  private[chessjudgment] def goalTargetObjects(
      consequence: TransitionConsequence
  ): Set[ConcreteChessObject] =
    goalTargetObjectGroups(consequence).flatten.toSet

  def specificTargetMechanismReady(bindings: List[EvidenceObjectBinding]): Boolean =
    bindings.exists(_.specificTargetMechanismReady)

  def hasConcreteObject(bindings: List[EvidenceObjectBinding]): Boolean =
    bindings.exists(_.hasConcreteObject)



  private def bindingsFromProofSectionForCause(
      cause: RelativeCauseFact,
      section: RelativeCauseProofSection,
      graph: TypedEvidenceGraph,
      visited: Set[String]
  ): List[EvidenceObjectBinding] =
    val contrastIds = graph.relativeCauseStrategicContrasts(section).map(_._1.id).toSet
    val ordinaryBindings =
      fromEvidenceRefs(
        graph,
        section.sourceRefs.filterNot(ref => contrastIds.contains(ref.id)),
        visited
      )
    val admittedContrastBindings =
      graph.relativeCauseStrategicAxisComparisons(cause, section).flatMap { case (contrastRef, comparison) =>
        graph.record(contrastRef).toList.flatMap {
          case EvidenceRecord(_, payload: StrategicMechanismContrastEvidence, _) =>
            fromStrategicContrastComparison(
              payload,
              comparison,
              graph.strategicComparisonSourceRefs(comparison, cause.sourceSide),
              graph,
              visited + contrastRef.id
            )
          case _ =>
            Nil
        }
      }
    (ordinaryBindings ++ admittedContrastBindings)
      .map(_.copy(proofRole = Some(section.role)))
      .distinctBy(_.occurrenceSignature)

  private def fromStrategicContrastComparison(
      payload: StrategicMechanismContrastEvidence,
      comparison: StrategicAxisComparison,
      sourceRefs: List[EvidenceRef],
      graph: TypedEvidenceGraph,
      visited: Set[String]
  ): List[EvidenceObjectBinding] =
    sourceRefs.flatMap(source =>
      graph.byId
        .get(source.id)
        .toList
        .flatMap(sourceRecord =>
          fromRecord(sourceRecord, graph, visited).map(binding =>
            binding.copy(
              mechanism = (
                binding.mechanism ++ objectOf(EvidenceObjectKind.Mechanism, comparison.axis.kind.toString)
              ).distinctBy(_.signaturePart),
              consequence = (
                binding.consequence ++ objectOf(EvidenceObjectKind.Consequence, comparison.outcome.toString)
              ).distinctBy(_.signaturePart),
              witness = (
                binding.witness ++ lineObject(payload.referenceLine) ++ lineObject(payload.candidateLine)
              ).distinctBy(_.signaturePart),
              horizon = Some(payload.sustainability.horizon.toString)
            )
          )
        )
    )

  private def fromRecord(
      record: EvidenceRecord,
      graph: TypedEvidenceGraph,
      visited: Set[String]
  ): List[EvidenceObjectBinding] =
    if visited.contains(record.ref.id) || !graph.proofEligible(record) then Nil
    else
      val nextVisited = visited + record.ref.id
      record.payload match
        case payload: LineFactEvidence =>
          fromLineFact(record.ref, payload)
        case payload: RelationFactEvidence =>
          Option
            .when(graph.relationProofEligible(record))(fromRelation(record.ref, payload))
            .toList
        case payload: PlanCausalEventEvidence =>
          fromPlanCausalEvent(record.ref, payload)
        case PlanTransitionEvidence(proof) =>
          val transition = proof.summary
          val routePieces = proof.causalDependencies.flatMap(_.proof.proofPieceRoles)
          val routeSquares = proof.causalDependencies.flatMap(_.proof.proofSquares)
          val routeMoves = proof.causalDependencies.flatMap(dependency =>
            dependency.from.event.rootMove ::
              dependency.interveningSteps.map(_.moveUci) :::
              List(dependency.to.event.rootMove)
          )
          transition.currentEvent.toList.map(current =>
            EvidenceObjectBinding(
              source = record.ref,
              actor = (
                transition.previousEvent.toList.flatMap(previous =>
                  moveObjects(previous.rootMove) ++
                    previous.actorRole.toList.flatMap(objectOf(EvidenceObjectKind.Piece, _))
                ) ++ routePieces.flatMap(role => objectOf(EvidenceObjectKind.Piece, role.name))
              ).distinctBy(_.signaturePart),
              target = (
                objectOf(EvidenceObjectKind.PlanSubject, current.goalKey) ++
                  current.targets.flatMap(planIdentityTargetObject) ++
                  routeSquares.flatMap(square => objectOf(EvidenceObjectKind.Square, square.key))
              ).distinctBy(_.signaturePart),
              mechanism = (
                objectOf(EvidenceObjectKind.Mechanism, "plan-transition") ++
                  proof.causalDependencies.flatMap(dependency =>
                    objectOf(EvidenceObjectKind.Mechanism, dependency.dependencyKind.toString) ++
                      objectOf(EvidenceObjectKind.Mechanism, dependency.proof.kind)
                  )
              ).distinctBy(_.signaturePart),
              consequence = objectOf(EvidenceObjectKind.Consequence, transition.transitionType.toString),
              witness = (
                transition.continuity.toList.flatMap(_.supportingMoves).flatMap(move => objectOf(EvidenceObjectKind.Move, move)) ++
                  objectOf(EvidenceObjectKind.Move, current.rootMove) ++
                  routeMoves.flatMap(move => objectOf(EvidenceObjectKind.Move, move))
              ).distinctBy(_.signaturePart),
              line = record.ref.line,
              horizon = transition.continuity.map(continuity => s"${continuity.consecutivePlies}-ply")
            )
          )
        case payload: StructuralDeltaEvidence =>
          fromStructuralDelta(record.ref, payload)
        case payload: TacticalMechanismEvidence =>
          val sourceBindings =
            payload.signals.flatMap(signal =>
              signal.source.toList.flatMap(source =>
                graph.byId.get(source.id).toList.flatMap(sourceRecord =>
                  fromRecord(sourceRecord, graph, nextVisited).map(binding =>
                    binding.copy(
                      mechanism = (
                        binding.mechanism ++ objectOf(EvidenceObjectKind.Mechanism, payload.kind.toString)
                      ).distinctBy(_.signaturePart),
                      consequence = (
                        binding.consequence ++ objectOf(EvidenceObjectKind.Consequence, payload.kind.toString)
                      ).distinctBy(_.signaturePart),
                      line = binding.line.orElse(payload.line),
                      horizon = binding.horizon.orElse(payload.line.map(_.role.toString))
                    )
                  )
                )
              )
            )
          if sourceBindings.nonEmpty then sourceBindings.distinctBy(_.occurrenceSignature)
          else List(fromTacticalMechanism(record.ref, payload))
        case payload: StrategicMechanismEvidence =>
          payload.signals.flatMap(signal =>
            graph.byId
              .get(signal.source.id)
              .toList
              .flatMap(source =>
                fromRecord(source, graph, nextVisited).map(binding =>
                  binding.copy(
                    mechanism = (binding.mechanism ++ objectOf(EvidenceObjectKind.Mechanism, payload.kind.toString)).distinctBy(
                      _.signaturePart
                    ),
                    consequence = (
                      binding.consequence ++ signal.axis.toList.flatMap(axis =>
                        objectOf(EvidenceObjectKind.Consequence, axis.stableKey)
                      )
                    ).distinctBy(_.signaturePart),
                    horizon = binding.horizon.orElse(signal.axis.map(_.kind.toString))
                  )
                )
              )
          )
        case payload: StrategicMechanismContrastEvidence =>
          payload.axisComparisons.flatMap(axisComparison =>
            fromStrategicContrastComparison(
              payload,
              axisComparison,
              axisComparison.sources,
              graph,
              nextVisited
            )
          )
        case RelativeCauseFactEvidence(cause) =>
          fromRelativeCause(cause, graph, nextVisited)
        case _ =>
          Nil

  private def fromLineFact(ref: EvidenceRef, payload: LineFactEvidence): List[EvidenceObjectBinding] =
    val rootBindings =
      payload.rootMove.toList.map { moveUci =>
        val move = normalize(moveUci)
        EvidenceObjectBinding(
          source = ref,
          actor = moveObjects(move),
          target = moveTargetSquare(move),
          mechanism = objectOf(EvidenceObjectKind.Mechanism, "LineRootMove"),
          consequence = objectOf(EvidenceObjectKind.Consequence, "LineRootMove"),
          witness = objectOf(EvidenceObjectKind.Move, move) ++ moveTargetSquare(move) ++ lineObject(payload.line),
          line = Some(payload.line)
        )
      }
    val replayBindings =
      payload.lineReplaySteps.drop(1).map { step =>
        val move = normalize(step.moveUci)
        EvidenceObjectBinding(
          source = ref,
          actor = moveObjects(move),
          target = moveTargetSquare(move),
          mechanism = objectOf(EvidenceObjectKind.Mechanism, "LineContinuation"),
          consequence = objectOf(EvidenceObjectKind.Consequence, "LineContinuation"),
          witness = objectOf(EvidenceObjectKind.Move, move) ++ moveTargetSquare(move) ++ lineObject(payload.line),
          line = Some(payload.line),
          lineOccurrence = Some(step)
        )
      }
    val eventBindings =
      payload.lineEvents.map { event =>
        val move = normalize(event.moveUci)
        EvidenceObjectBinding(
          source = ref,
          actor = moveObjects(move) ++
            event.side.toList.flatMap(color => objectOf(EvidenceObjectKind.Side, colorKey(color))) ++
            roleObject(event.pieceRole),
          target = {
            val squareTarget = squareObject(event.square)
            (if squareTarget.nonEmpty then squareTarget else moveTargetSquare(move)) ++ roleObject(event.targetRole)
          },
          mechanism = objectOf(EvidenceObjectKind.Mechanism, event.kind.toString),
          consequence = objectOf(EvidenceObjectKind.Consequence, event.kind.toString),
          witness = objectOf(EvidenceObjectKind.Move, move) ++ moveTargetSquare(move) ++ lineObject(payload.line),
          line = Some(payload.line),
          horizon = Some(s"ply:${event.plyOffset}")
        )
      }
    val consequenceBindings =
      payload.proofSignalConsequences.map { consequence =>
        val lineMoves = consequence.lineMoves.map(normalize)
        val ownedSacrificeOccurrence = consequence.sacrificeOccurrence.filter(_ =>
          consequence.kind == LineConsequenceKind.Sacrifice &&
            consequence.rootMove.exists(root => payload.rootMove.exists(EvidenceRef.sameMove(_, root)))
        )
        val eventMove = ownedSacrificeOccurrence
          .map(_.acceptance.moveUci)
          .orElse(consequence.eventMove)
          .orElse(lineMoves.headOption)
          .map(normalize)
        val consequenceMoves = (lineMoves ++ eventMove.toList).distinct
        val sacrificeCaptureTargets =
          ownedSacrificeOccurrence.toList.flatMap { occurrence =>
            val capture = occurrence.acceptance
            squareObject(Some(capture.square)) ++
              roleObject(Some(capture.capturedRole)) ++
              objectOf(EvidenceObjectKind.PlanSubject, s"material-sacrifice:${capture.square.key}")
          }
        val lineMoveWitness =
          consequenceMoves.flatMap(move => objectOf(EvidenceObjectKind.Move, move) ++ moveTargetSquare(move))
        val lineEventWitness =
          payload.lineEvents
            .filter(event => consequenceMoves.contains(normalize(event.moveUci)))
            .flatMap(event => squareObject(event.square) ++ roleObject(event.pieceRole) ++ roleObject(event.targetRole))
        EvidenceObjectBinding(
          source = ref,
          actor = eventMove.toList.flatMap(moveObjects),
          target = eventMove.toList.flatMap(moveTargetSquare) ++ sacrificeCaptureTargets,
          mechanism = objectOf(EvidenceObjectKind.Mechanism, consequence.kind.toString),
          consequence = objectOf(EvidenceObjectKind.Consequence, consequence.kind.toString),
          witness = lineMoveWitness ++ lineEventWitness ++ lineObject(payload.line),
          line = Some(payload.line)
        )
      }
    val materialCaptureBindings =
      payload.materialCaptures.map { capture =>
        val move = normalize(capture.moveUci)
        val rootMove = payload.rootMove.map(normalize)
        val rootMoveCapture = capture.plyOffset == 0 || rootMove.contains(move)
        val prefix = if capture.recapture then "material-recapture" else "material-capture"
        val materialIdentityTargets =
          if rootMoveCapture then
            objectOf(EvidenceObjectKind.PlanSubject, s"$prefix:${capture.square.key}") ++
              Option
                .when(payload.materialSacrificeCapture(capture))(s"material-sacrifice:${capture.square.key}")
                .toList
                .flatMap(objectOf(EvidenceObjectKind.PlanSubject, _))
          else Nil
        EvidenceObjectBinding(
          source = ref,
          actor = moveObjects(move) ++
            roleObject(Some(capture.attackerRole)) ++
            objectOf(EvidenceObjectKind.Side, colorKey(capture.side)),
          target = squareObject(Some(capture.square)) ++ roleObject(Some(capture.capturedRole)) ++ materialIdentityTargets,
          mechanism = objectOf(
            EvidenceObjectKind.Mechanism,
            if capture.recapture then "MaterialRecapture" else "MaterialCapture"
          ),
          consequence = objectOf(EvidenceObjectKind.Consequence, "MaterialCapture"),
          witness = objectOf(EvidenceObjectKind.Move, move) ++ squareObject(Some(capture.square)) ++ lineObject(payload.line),
          line = Some(payload.line),
          horizon = Some(s"ply:${capture.plyOffset}")
        )
      }
    (rootBindings ++ replayBindings ++ eventBindings ++ consequenceBindings ++ materialCaptureBindings).distinctBy(
      _.signature
    )

  private def fromRelation(ref: EvidenceRef, payload: RelationFactEvidence): EvidenceObjectBinding =
    val actorParticipants =
      payload.participants.filter(participant =>
        participant.participantRole == RelationParticipantRole.Controller ||
          participant.participantRole == RelationParticipantRole.Attacker ||
          participant.participantRole == RelationParticipantRole.Mover ||
          participant.participantRole == RelationParticipantRole.Beneficiary
      )
    val targetParticipants =
      payload.participants.filter(participant =>
        participant.participantRole == RelationParticipantRole.Target ||
          participant.participantRole == RelationParticipantRole.Supported ||
          participant.participantRole == RelationParticipantRole.King ||
          participant.participantRole == RelationParticipantRole.Blocker
      )
    val relationActors =
      payload.detail match
        case RelationWitnessDetail.GeometricControl(side, _, _, _, _) =>
          objectOf(EvidenceObjectKind.Side, colorKey(side))
        case RelationWitnessDetail.LegalMove(side, _, _, _, _, _) =>
          objectOf(EvidenceObjectKind.Side, colorKey(side))
        case RelationWitnessDetail.GeometricControlSetDelta(mover, controllingSide, _, _, _, _, _, _, _, _) =>
          objectOf(EvidenceObjectKind.Side, colorKey(mover.side)) ++
            objectOf(EvidenceObjectKind.Side, colorKey(controllingSide))
        case RelationWitnessDetail.GeometricSupporterCapture(mover, _, _, _, _, _) =>
          objectOf(EvidenceObjectKind.Side, colorKey(mover.side))
        case RelationWitnessDetail.GeometricSupportDelta(mover, supportedSide, _, _, _, _, _, _, _, _, _) =>
          objectOf(EvidenceObjectKind.Side, colorKey(mover.side)) ++
            objectOf(EvidenceObjectKind.Side, colorKey(supportedSide))
        case RelationWitnessDetail.SliderControlInterference(mover, controllerSide, _, _, _, _, _) =>
          objectOf(EvidenceObjectKind.Side, colorKey(mover.side)) ++
            objectOf(EvidenceObjectKind.Side, colorKey(controllerSide))
        case RelationWitnessDetail.GeometricLineControlAfterBlockerRemoval(mover, controllerSide, _, _, _, _, _, _, _, _, _, _) =>
          objectOf(EvidenceObjectKind.Side, colorKey(mover.side)) ++
            objectOf(EvidenceObjectKind.Side, colorKey(controllerSide))
        case RelationWitnessDetail.CheckingEnemyControlBundle(mover, _, _, _) =>
          objectOf(EvidenceObjectKind.Side, colorKey(mover.side))
        case RelationWitnessDetail.DoubleCheck(mover, _, _, _) =>
          objectOf(EvidenceObjectKind.Side, colorKey(mover.side))
        case RelationWitnessDetail.PawnFileGroup(side, _, _) =>
          objectOf(EvidenceObjectKind.Side, colorKey(side))
        case RelationWitnessDetail.PawnTension(_, _) =>
          Nil
        case RelationWitnessDetail.PawnFrontOccupancy(side, _, _, _) =>
          objectOf(EvidenceObjectKind.Side, colorKey(side))
        case RelationWitnessDetail.PawnPassage(side, _, _) =>
          objectOf(EvidenceObjectKind.Side, colorKey(side))
        case RelationWitnessDetail.MajorPieceFileOccupancy(side, _, _, _) =>
          objectOf(EvidenceObjectKind.Side, colorKey(side))
        case RelationWitnessDetail.RayBarrier(side, _, _, _, _) =>
          objectOf(EvidenceObjectKind.Side, colorKey(side))
    val relationTargets =
      payload.detail match
        case RelationWitnessDetail.GeometricControl(side, _, _, _, target) =>
          target match
            case RelationControlTarget.Friendly(_) => objectOf(EvidenceObjectKind.Side, colorKey(side))
            case RelationControlTarget.Enemy(_)    => objectOf(EvidenceObjectKind.Side, colorKey(!side))
            case RelationControlTarget.Empty       => Nil
        case RelationWitnessDetail.LegalMove(_, _, _, _, _, capture) =>
          capture.toList.flatMap(value => objectOf(EvidenceObjectKind.Side, colorKey(value.capturedSide)))
        case RelationWitnessDetail.GeometricControlSetDelta(_, controllingSide, _, beforeTarget, afterTarget, _, _, _, _, _) =>
          if List(beforeTarget, afterTarget).exists {
              case RelationControlTarget.Enemy(_) => true
              case _                              => false
            }
          then objectOf(EvidenceObjectKind.Side, colorKey(!controllingSide))
          else if List(beforeTarget, afterTarget).exists {
              case RelationControlTarget.Friendly(_) => true
              case _                                 => false
            }
          then objectOf(EvidenceObjectKind.Side, colorKey(controllingSide))
          else Nil
        case RelationWitnessDetail.GeometricSupporterCapture(mover, _, _, _, _, _) =>
          objectOf(EvidenceObjectKind.Side, colorKey(!mover.side))
        case RelationWitnessDetail.GeometricSupportDelta(_, supportedSide, _, _, _, _, _, _, _, _, _) =>
          objectOf(EvidenceObjectKind.Side, colorKey(supportedSide))
        case RelationWitnessDetail.SliderControlInterference(_, controllerSide, _, _, _, target, _) =>
          target match
            case RelationControlTarget.Friendly(_) => objectOf(EvidenceObjectKind.Side, colorKey(controllerSide))
            case RelationControlTarget.Enemy(_)    => objectOf(EvidenceObjectKind.Side, colorKey(!controllerSide))
            case RelationControlTarget.Empty       => Nil
        case RelationWitnessDetail.GeometricLineControlAfterBlockerRemoval(_, controllerSide, _, _, _, _, _, _, target, _, _, _) =>
          target match
            case RelationControlTarget.Friendly(_) => objectOf(EvidenceObjectKind.Side, colorKey(controllerSide))
            case RelationControlTarget.Enemy(_)    => objectOf(EvidenceObjectKind.Side, colorKey(!controllerSide))
            case RelationControlTarget.Empty       => Nil
        case RelationWitnessDetail.CheckingEnemyControlBundle(mover, _, _, _) =>
          objectOf(EvidenceObjectKind.Side, colorKey(!mover.side))
        case RelationWitnessDetail.DoubleCheck(mover, _, _, _) =>
          objectOf(EvidenceObjectKind.Side, colorKey(!mover.side))
        case RelationWitnessDetail.PawnFileGroup(_, file, _) =>
          objectOf(EvidenceObjectKind.File, file.key)
        case RelationWitnessDetail.PawnTension(_, _) =>
          objectOf(EvidenceObjectKind.Relation, "opposing-pawn-contact")
        case RelationWitnessDetail.PawnFrontOccupancy(_, _, _, occupant) =>
          occupant.toList.flatMap(value => objectOf(EvidenceObjectKind.Side, colorKey(value.side)))
        case RelationWitnessDetail.PawnPassage(side, _, blockers) =>
          Option.when(blockers.nonEmpty)(objectOf(EvidenceObjectKind.Side, colorKey(!side))).getOrElse(Nil)
        case RelationWitnessDetail.MajorPieceFileOccupancy(_, file, _, _) =>
          objectOf(EvidenceObjectKind.File, file.key)
        case RelationWitnessDetail.RayBarrier(side, _, _, occupants, _) =>
          val exactTargets = payload.targetSquares.toSet
          Option.when(occupants.exists(piece => piece.side != side && exactTargets(piece.square)))(
            objectOf(EvidenceObjectKind.Side, colorKey(!side))
          ).getOrElse(Nil)
    EvidenceObjectBinding(
      source = ref,
      actor = (relationActors ++ actorParticipants.flatMap(participantObjects)).distinctBy(_.signaturePart),
      target = (
        relationTargets ++
          payload.targetSquares.flatMap(square => objectOf(EvidenceObjectKind.Square, square.key)) ++
          targetParticipants.flatMap(participantObjects)
      ).distinctBy(_.signaturePart),
      mechanism = objectOf(EvidenceObjectKind.Relation, payload.kind.toString) ++
        objectOf(EvidenceObjectKind.Mechanism, payload.detail.detailName),
      consequence = objectOf(EvidenceObjectKind.Consequence, payload.kind.toString),
      witness = payload.lineMoves.flatMap(move => objectOf(EvidenceObjectKind.Move, move)) ++
        payload.focusSquares.flatMap(square => objectOf(EvidenceObjectKind.Square, square.key)),
      line = ref.line
    )

  private def fromPlanCausalEvent(ref: EvidenceRef, payload: PlanCausalEventEvidence): List[EvidenceObjectBinding] =
    val planTarget = objectOf(EvidenceObjectKind.PlanSubject, payload.planId.id)
    val rootActor =
      moveObjects(payload.rootMove) ++
        payload.identity.actorRole.toList.flatMap(objectOf(EvidenceObjectKind.Piece, _)) ++
        objectOf(EvidenceObjectKind.Side, colorKey(payload.perspective))
    val rootDestination =
      moveTargetSquare(payload.rootMove) ++
        Option(normalize(payload.rootMove).slice(2, 3)).filter(_.matches("[a-h]")).toList.flatMap(
          objectOf(EvidenceObjectKind.File, _)
        )
    val rootWitness = objectOf(EvidenceObjectKind.Move, payload.rootMove) ++ lineObject(payload.rootLine)
    val directBindings = payload.directGoalConsequences.map { consequence =>
      val explicitTargets = consequence.targetSubjectFacts.nonEmpty
      EvidenceObjectBinding(
        source = ref,
        actor = rootActor,
        target = (
          planTarget ++
            Option.unless(explicitTargets)(rootDestination).getOrElse(Nil) ++
            consequence.goalSubjectFacts.flatMap(subjectObject)
        ).distinctBy(_.signaturePart),
        mechanism = objectOf(EvidenceObjectKind.Mechanism, consequence.kind.toString),
        consequence = objectOf(EvidenceObjectKind.Consequence, consequence.anchorKey),
        witness = (rootWitness ++ consequence.witnessSubjectFacts.flatMap(subjectObject)).distinctBy(_.signaturePart),
        line = Some(payload.rootLine)
      )
    }
    val rootDependencyBindings = payload.rootEnablingDependencies.map { dependency =>
      val dependencySquares = dependency.proofSquares.flatMap(square => objectOf(EvidenceObjectKind.Square, square.key))
      val dependencyPieces = dependency.proofPieceRoles.flatMap(role => objectOf(EvidenceObjectKind.Piece, role.name))
      EvidenceObjectBinding(
        source = ref,
        actor = rootActor,
        target = (planTarget ++ dependencySquares).distinctBy(_.signaturePart),
        mechanism = objectOf(EvidenceObjectKind.Mechanism, dependency.kind.toString),
        consequence = objectOf(EvidenceObjectKind.Consequence, dependency.kind.toString),
        witness = (
          rootWitness ++
            dependencySquares ++
            dependencyPieces
        ).distinctBy(_.signaturePart),
        line = Some(payload.rootLine)
      )
    }
    val historyDependencyBindings = payload.historyEnablingDependencies.map { dependency =>
      val dependencySquares = dependency.proofSquares.flatMap(square => objectOf(EvidenceObjectKind.Square, square.key))
      val dependencyPieces = dependency.proofPieceRoles.flatMap(role => objectOf(EvidenceObjectKind.Piece, role.name))
      EvidenceObjectBinding(
        source = ref,
        actor = (
          rootActor ++ dependency.from.identity.actorRole.toList.flatMap(objectOf(EvidenceObjectKind.Piece, _))
        ).distinctBy(_.signaturePart),
        target = (planTarget ++ dependencySquares).distinctBy(_.signaturePart),
        mechanism = objectOf(EvidenceObjectKind.Mechanism, dependency.kind.toString),
        consequence = objectOf(EvidenceObjectKind.Consequence, dependency.kind.toString),
        witness = (
          rootWitness ++
            objectOf(EvidenceObjectKind.Move, dependency.from.moveUci) ++
            dependencySquares ++
            dependencyPieces
        ).distinctBy(_.signaturePart),
        line = Some(payload.rootLine),
        horizon = Some(s"ply:${dependency.plyOffset}")
      )
    }
    val resultBindings = payload.positiveGoalResultAssessments.map { assessment =>
      val sourceEvent = assessment.sourceEvent
      val enablingDependencies = assessment.causalPath
      val resultTargets = (
        assessment.consequence.goalSubjectFacts.flatMap(subjectObject) ++
          PlanCausalEpisode
            .consequenceTargetSquares(assessment.consequence)
            .flatMap(square =>
              objectOf(EvidenceObjectKind.Square, square.key) ++ objectOf(EvidenceObjectKind.File, square.key.take(1))
            )
      ).distinctBy(_.signaturePart)
      val dependencyWitnesses = (
        enablingDependencies.flatMap(_.proofSquares).flatMap(square =>
          objectOf(EvidenceObjectKind.Square, square.key)
        ) ++
          enablingDependencies.flatMap(_.proofPieceRoles).flatMap(role =>
            objectOf(EvidenceObjectKind.Piece, role.name)
          )
      ).distinctBy(_.signaturePart)
      EvidenceObjectBinding(
        source = ref,
        actor = (
          rootActor ++
            sourceEvent.identity.actorRole.toList.flatMap(objectOf(EvidenceObjectKind.Piece, _))
        ).distinctBy(_.signaturePart),
        target = (planTarget ++ resultTargets).distinctBy(_.signaturePart),
        mechanism = (
          enablingDependencies.flatMap(dependency => objectOf(EvidenceObjectKind.Mechanism, dependency.kind.toString)) ++
            objectOf(EvidenceObjectKind.Mechanism, assessment.goalProof.mechanism.toString) ++
            objectOf(EvidenceObjectKind.Mechanism, assessment.consequence.kind.toString)
        ).distinctBy(_.signaturePart),
        consequence = (
          objectOf(EvidenceObjectKind.Consequence, assessment.consequence.anchorKey) ++
            objectOf(EvidenceObjectKind.Consequence, assessment.robustness.toString)
        ).distinctBy(_.signaturePart),
        witness = (
          rootWitness ++
            objectOf(EvidenceObjectKind.Move, sourceEvent.moveUci) ++
            assessment.realizedObservations.flatMap(observation => lineObject(observation.line)) ++
            assessment.realizedObservations.flatMap(_.realizationMoves).flatMap(objectOf(EvidenceObjectKind.Move, _)) ++
            resultTargets ++
            assessment.consequence.witnessSubjectFacts.flatMap(subjectObject) ++
            dependencyWitnesses
        ).distinctBy(_.signaturePart),
        line = Some(payload.rootLine),
        horizon = Some(s"ply:${assessment.sourcePlyOffset}"),
        planDependencies = enablingDependencies,
        planGoalProof = Some(assessment.goalProof)
      )
    }
    (directBindings ++ rootDependencyBindings ++ historyDependencyBindings ++ resultBindings)
      .distinctBy(_.occurrenceSignature)

  private def fromStructuralDelta(ref: EvidenceRef, payload: StructuralDeltaEvidence): List[EvidenceObjectBinding] =
    val actor =
      moveObjects(payload.moveUci) ++
        payload.transition.actorRole.toList.flatMap(role => objectOf(EvidenceObjectKind.Piece, role.name)) ++
        objectOf(EvidenceObjectKind.Side, colorKey(payload.perspective))
    val signalBindings =
      payload.signals.map { signal =>
        EvidenceObjectBinding(
          source = ref,
          actor = actor.distinctBy(_.signaturePart),
          target = signal.subjectFacts.flatMap(subjectObject),
          mechanism = objectOf(EvidenceObjectKind.Mechanism, signal.kind.toString),
          consequence = Nil,
          witness = objectOf(EvidenceObjectKind.Move, payload.moveUci) ++ payload.line.toList.flatMap(lineObject),
          line = payload.line
        )
      }
    val consequenceBindings =
      payload.consequences.flatMap(consequence =>
        structuralConsequenceBindings(ref, payload, actor, consequence)
      )
    val relationChangeBindings =
      payload.relationChanges.map { change =>
        EvidenceObjectBinding(
          source = ref,
          actor = actor.distinctBy(_.signaturePart),
          target = (
            change.targetSquares.flatMap(square => objectOf(EvidenceObjectKind.Square, square.key)) ++
              change.files.flatMap(file => objectOf(EvidenceObjectKind.File, file.key))
          ).distinctBy(_.signaturePart),
          mechanism = objectOf(EvidenceObjectKind.Relation, change.kind.toString),
          consequence = objectOf(
            EvidenceObjectKind.Consequence,
            s"relation-${change.direction.toString.toLowerCase}:${RelationFactKind.id(change.kind)}"
          ),
          witness = (
            objectOf(EvidenceObjectKind.Move, payload.moveUci) ++
              payload.line.toList.flatMap(lineObject) ++
              change.participants.flatMap(participantObjects) ++
              change.dependencySquares.flatMap(square => objectOf(EvidenceObjectKind.Square, square.key)) ++
              change.proofKeys.flatMap(relationProofKeyObjects)
          ).distinctBy(_.signaturePart),
          line = payload.line,
          provenance = List(change.source)
        )
      }
    (signalBindings ++ consequenceBindings).distinctBy(_.occurrenceSignature) ++ relationChangeBindings

  private def structuralConsequenceBindings(
      ref: EvidenceRef,
      payload: StructuralDeltaEvidence,
      actor: List[ConcreteChessObject],
      consequence: TransitionConsequence
  ): List[EvidenceObjectBinding] =
    val allBindings = (consequence.subjectBindings ++ consequence.targetBindings).distinct
    val groups =
      if TransitionConsequenceRelationProof.relationBacked(consequence.kind) then
        allBindings
          .groupBy(binding => binding.relationKeys.map(_.stableKey).sorted.mkString("|"))
          .toList
          .sortBy(_._1)
          .map(_._2)
      else List(allBindings)
    val changesByKey = payload.relationChanges.groupMap(_.key)(identity)
    val resolved = groups.map { bindings =>
      val keys = bindings.flatMap(_.relationKeys).distinct.sortBy(_.stableKey)
      val sources = keys.flatMap(key => changesByKey.getOrElse(key, Nil).map(_.source)).distinctBy(_.id)
      val exactResolution =
        if TransitionConsequenceRelationProof.relationBacked(consequence.kind) then
          bindings.nonEmpty &&
            bindings.forall(_.relationKeys.nonEmpty) &&
            keys.forall(key => changesByKey.get(key).exists(_.size == 1)) &&
            sources.size == keys.size
        else bindings.forall(_.relationKeys.isEmpty)
      Option.when(exactResolution) {
        val bindingSet = bindings.toSet
        val goalSubjects = consequence.goalSubjectBindings.filter(bindingSet).map(_.subject)
        val witnessSubjects = consequence.witnessSubjectBindings.filter(bindingSet).map(_.subject)
        EvidenceObjectBinding(
          source = ref,
          actor = actor.distinctBy(_.signaturePart),
          target = (
            goalSubjects.flatMap(subjectObject) ++
              structuralOpponentRestrictionTargetObjects(consequence)
          ).distinctBy(_.signaturePart),
          mechanism = objectOf(EvidenceObjectKind.Mechanism, consequence.kind.toString),
          consequence = objectOf(EvidenceObjectKind.Consequence, consequence.anchorKey),
          witness = (
            objectOf(EvidenceObjectKind.Move, payload.moveUci) ++
              payload.line.toList.flatMap(lineObject) ++
              witnessSubjects.flatMap(subjectObject)
          ).distinctBy(_.signaturePart),
          line = payload.line,
          provenance = sources
        )
      }
    }
    if resolved.forall(_.nonEmpty) then resolved.flatten else Nil

  private def structuralOpponentRestrictionTargetObjects(
      consequence: TransitionConsequence
  ): List[ConcreteChessObject] =
    Option
      .when(consequence.kind == TransitionConsequenceKind.OpponentMobilityRestriction)(
        consequence.subjectFacts
          .filter(StructuralDeltaEvidence.validOpponentMobilityRestrictionSubject)
          .flatMap(StructuralDeltaEvidence.restrictedOpponentEntry)
          .flatMap { case (piece, from, to) =>
            objectOf(EvidenceObjectKind.Piece, piece.name) ++
              objectOf(EvidenceObjectKind.Square, from.key) ++
              objectOf(EvidenceObjectKind.Square, to.key)
          }
          .distinctBy(_.signaturePart)
      )
      .getOrElse(Nil)

  private def fromTacticalMechanism(ref: EvidenceRef, payload: TacticalMechanismEvidence): EvidenceObjectBinding =
    EvidenceObjectBinding(
      source = ref,
      actor = payload.moveUci.toList.flatMap(moveObjects),
      target = Nil,
      mechanism = objectOf(EvidenceObjectKind.Mechanism, payload.kind.toString) ++
        payload.signals.flatMap(signal => objectOf(EvidenceObjectKind.Mechanism, signal.label)),
      consequence = objectOf(EvidenceObjectKind.Consequence, payload.kind.toString),
      witness = payload.line.toList.flatMap(lineObject),
      line = payload.line
    )

  private def participantObjects(participant: RelationParticipant): List[ConcreteChessObject] =
    objectOf(EvidenceObjectKind.Square, participant.square.key) ++
      participant.role.toList.flatMap(role => objectOf(EvidenceObjectKind.Piece, role.name)) ++
      objectOf(EvidenceObjectKind.Mechanism, participant.participantRole.toString)

  private def relationProofKeyObjects(key: RelationProofKey): List[ConcreteChessObject] =
    key match
      case RelationProofKey.ChangedSquare(square) =>
        objectOf(EvidenceObjectKind.Square, square.key)
      case RelationProofKey.MovedPiece(side, beforeRole, afterRole, from, to) =>
        objectOf(EvidenceObjectKind.Side, colorKey(side)) ++
          objectOf(EvidenceObjectKind.Piece, beforeRole.name) ++
          objectOf(EvidenceObjectKind.Piece, afterRole.name) ++
          objectOf(EvidenceObjectKind.Square, from.key) ++
          objectOf(EvidenceObjectKind.Square, to.key)

  private def moveObjects(move: String): List[ConcreteChessObject] =
    val normalized = normalize(move)
    objectOf(EvidenceObjectKind.Move, normalized) ++ moveSourceSquare(normalized)

  private def moveSourceSquare(move: String): List[ConcreteChessObject] =
    if move.length >= 2 then objectOf(EvidenceObjectKind.Square, move.take(2)) else Nil

  private def moveTargetSquare(move: String): List[ConcreteChessObject] =
    if move.length >= 4 then objectOf(EvidenceObjectKind.Square, move.slice(2, 4)) else Nil

  private def squareObject(square: Option[EvidenceSquare]): List[ConcreteChessObject] =
    square.toList.flatMap(square => objectOf(EvidenceObjectKind.Square, square.key))

  private def roleObject(role: Option[EvidencePieceRole]): List[ConcreteChessObject] =
    role.toList.flatMap(role => objectOf(EvidenceObjectKind.Piece, role.name))

  private def lineObject(line: LineNodeRef): List[ConcreteChessObject] =
    objectOf(EvidenceObjectKind.Line, line.id) ++ objectOf(EvidenceObjectKind.Move, line.rootMove)

  private def subjectObject(subject: StructuralSubject): List[ConcreteChessObject] =
    val identityObject = subject.identityKey.toList.flatMap(objectOf(EvidenceObjectKind.PlanSubject, _))
    val values = subject match
      case StructuralSubject.OpenFile(file) =>
        objectOf(EvidenceObjectKind.File, file.key)
      case StructuralSubject.SemiOpenFile(file) =>
        objectOf(EvidenceObjectKind.File, file.key)
      case StructuralSubject.BreakFile(file) =>
        objectOf(EvidenceObjectKind.File, file.key)
      case StructuralSubject.FileOccupation(file, square, role) =>
        objectOf(EvidenceObjectKind.File, file.key) ++
          objectOf(EvidenceObjectKind.Square, square.key) ++
          objectOf(EvidenceObjectKind.Piece, role.name)
      case StructuralSubject.PieceAt(role, square) =>
        objectOf(EvidenceObjectKind.Piece, role.name) ++ objectOf(EvidenceObjectKind.Square, square.key)
      case StructuralSubject.PawnTensionCreated(from, to) =>
        objectOf(EvidenceObjectKind.Piece, "pawn") ++
          objectOf(EvidenceObjectKind.Square, from.key) ++ objectOf(EvidenceObjectKind.Square, to.key)
      case StructuralSubject.PawnTensionResolved(from, to) =>
        objectOf(EvidenceObjectKind.Piece, "pawn") ++
          objectOf(EvidenceObjectKind.Square, from.key) ++ objectOf(EvidenceObjectKind.Square, to.key)
      case StructuralSubject.Battery(detail) =>
        List(detail.attackerRole, detail.occupants.head.role).flatMap(role => objectOf(EvidenceObjectKind.Piece, role.name)) ++
          objectOf(EvidenceObjectKind.Square, detail.attackerSquare.key) ++
          objectOf(EvidenceObjectKind.Square, detail.occupants.head.square.key)
      case StructuralSubject.PassedPawnCreated(_, square) =>
        objectOf(EvidenceObjectKind.Piece, "pawn") ++ objectOf(EvidenceObjectKind.Square, square.key)
      case StructuralSubject.PassedPawnLost(_, square) =>
        objectOf(EvidenceObjectKind.Piece, "pawn") ++ objectOf(EvidenceObjectKind.Square, square.key)
      case StructuralSubject.PassedPawnAdvanced(_, from, to, _) =>
        objectOf(EvidenceObjectKind.Piece, "pawn") ++
          objectOf(EvidenceObjectKind.Square, from.key) ++ objectOf(EvidenceObjectKind.Square, to.key)
      case StructuralSubject.PassedStatusCreated(_, from, to, _) =>
        objectOf(EvidenceObjectKind.Piece, "pawn") ++
          objectOf(EvidenceObjectKind.Square, from.key) ++ objectOf(EvidenceObjectKind.Square, to.key)
      case StructuralSubject.PassedPawnPromoted(_, from, to) =>
        objectOf(EvidenceObjectKind.Piece, "pawn") ++
          objectOf(EvidenceObjectKind.Square, from.key) ++ objectOf(EvidenceObjectKind.Square, to.key)
      case StructuralSubject.OpponentResourceDeterred(role, from, to) =>
        objectOf(EvidenceObjectKind.Piece, role.name) ++
          objectOf(EvidenceObjectKind.Square, from.key) ++ objectOf(EvidenceObjectKind.Square, to.key)
    (identityObject ++ values).distinctBy(_.signaturePart)

  private def planIdentityTargetObject(raw: String): List[ConcreteChessObject] =
    val normalized = normalize(raw)
    normalized match
      case value if value.matches("square:[a-h][1-8]") =>
        objectOf(EvidenceObjectKind.Square, value.stripPrefix("square:"))
      case value if value.matches("file:[a-h]") =>
        objectOf(EvidenceObjectKind.File, value.stripPrefix("file:"))
      case value =>
        objectOf(EvidenceObjectKind.PlanSubject, value)

  private def objectOf(kind: EvidenceObjectKind, raw: String): List[ConcreteChessObject] =
    val key = normalize(raw)
    Option.when(key.nonEmpty)(ConcreteChessObject(kind, key)).toList

  private def normalize(raw: String): String =
    Option(raw).getOrElse("").trim.toLowerCase

  private def colorKey(color: Color): String =
    if color.white then "white" else "black"

enum RelationParticipantRole:
  case Controller
  case Supported
  case Attacker
  case Target
  case Blocker
  case Beneficiary
  case King
  case Mover
  case Other

final case class RelationParticipant(
    square: EvidenceSquare,
    role: Option[EvidencePieceRole],
    participantRole: RelationParticipantRole
)

enum RelationAxisSignal:
  case File
  case Rank
  case Diagonal

/** Exact chess classification of one canonical slider barrier. The
  * classification is derived from the ray occupants; it never owns a second
  * board fact.
  */
enum RelationRayPattern:
  case Ordinary
  case XRay
  case Battery
  case Pin
  case Skewer

object RelationRayPattern:
  private[chessjudgment] def classify(
      owner: Color,
      occupants: List[RelationColoredPieceWitness],
      axis: RelationAxisSignal
  ): RelationRayPattern =
    require(occupants.nonEmpty, "a ray pattern needs its first occupied barrier")
    val barrier = occupants.head
    val rear = occupants.lift(1)
    val barrierIsKing = barrier.role.name.equalsIgnoreCase(King.name)
    rear match
      case Some(target) if barrier.side != owner && target.side == barrier.side &&
          target.role.name.equalsIgnoreCase(King.name) && !barrierIsKing =>
        RelationRayPattern.Pin
      case Some(target) if barrier.side != owner && target.side == barrier.side &&
          barrierIsKing && !target.role.name.equalsIgnoreCase(King.name) =>
        RelationRayPattern.Skewer
      case Some(target) if barrier.side != owner && target.side == barrier.side =>
        RelationRayPattern.XRay
      case _ if barrier.side == owner && sliderSupports(barrier.role, axis) =>
        RelationRayPattern.Battery
      case _ =>
        RelationRayPattern.Ordinary

  def id(pattern: RelationRayPattern): String =
    pattern match
      case Ordinary => "ray_barrier"
      case XRay     => "xray"
      case Battery  => "battery"
      case Pin      => "pin"
      case Skewer   => "skewer"

  private def sliderSupports(role: EvidencePieceRole, axis: RelationAxisSignal): Boolean =
    role.name.equalsIgnoreCase(Queen.name) ||
      role.name.equalsIgnoreCase(Rook.name) && axis != RelationAxisSignal.Diagonal ||
      role.name.equalsIgnoreCase(Bishop.name) && axis == RelationAxisSignal.Diagonal

final case class RelationPieceWitness(
    square: EvidenceSquare,
    role: EvidencePieceRole
)

/** One exact geometric-control edge onto an occupied enemy square. The side
  * is owned by the enclosing relation so a bundle cannot silently mix both
  * players' controls.
  */
final case class RelationEnemyControlWitness(
    controllerSquare: EvidenceSquare,
    controllerRole: EvidencePieceRole,
    targetSquare: EvidenceSquare,
    targetRole: EvidencePieceRole
)

final case class RelationColoredPieceWitness(
    square: EvidenceSquare,
    role: EvidencePieceRole,
    side: Color
)

/** Exact before/after identity of a piece changed by the admitted root move.
  * This is a projection of `BoardPieceTransition`, not a second move parser.
  */
final case class RelationMoveTransitionWitness private[chessjudgment] (
    side: Color,
    from: EvidenceSquare,
    to: EvidenceSquare,
    beforeRole: EvidencePieceRole,
    afterRole: EvidencePieceRole
)

enum RelationControlTarget:
  case Empty
  case Friendly(role: EvidencePieceRole)
  case Enemy(role: EvidencePieceRole)

  def pieceRole: Option[EvidencePieceRole] =
    this match
      case Empty          => None
      case Friendly(role) => Some(role)
      case Enemy(role)    => Some(role)

final case class RelationLegalCaptureWitness(
    capturedSquare: EvidenceSquare,
    capturedRole: EvidencePieceRole,
    capturedSide: Color
)

enum RelationPremiseOccurrence:
  case Before
  case After
  case Removed
  case Established

final case class RelationCombinationPremise private[chessjudgment] (
    occurrence: RelationPremiseOccurrence,
    kind: RelationFactKind,
    semanticId: String
):
  require(semanticId.matches("[0-9a-f]{64}"), "a relation-combination premise needs a canonical semantic id")

  private[judgment] def stableKey: String =
    s"${occurrence.toString.toLowerCase}:${RelationFactKind.id(kind)}:$semanticId"

final case class RelationCombinationProof private[chessjudgment] (
    contract: RelationCombinationContractKind,
    premises: List[RelationCombinationPremise],
    private[chessjudgment] val proofKeys: List[RelationProofKey]
):
  require(premises.size >= 2, "a combined relation needs at least two canonical premises")
  require(premises.distinct.size == premises.size, "a combined relation cannot repeat a premise")
  require(
    premises == premises.sortBy(_.stableKey),
    "relation-combination premises must use canonical order"
  )
  require(proofKeys.nonEmpty, "a combined relation needs an exact transition proof key")
  require(proofKeys.distinct.size == proofKeys.size, "a combined relation cannot repeat a proof key")
  require(
    proofKeys == proofKeys.sortBy(_.stableKey),
    "relation-combination proof keys must use canonical order"
  )

  private[judgment] def stableKey: String =
    val premiseKey = premises.map(_.stableKey).mkString("[", ",", "]")
    val proofKey = proofKeys.map(_.stableKey).mkString("[", ",", "]")
    s"contract:${RelationCombinationContractKind.id(contract)}:premises:$premiseKey:proof-keys:$proofKey"

object RelationCombinationProof:
  private[chessjudgment] def from(
      contract: RelationCombinationContractKind,
      premises: List[(RelationPremiseOccurrence, RelationFactEvidence)],
      proofKeys: List[RelationProofKey]
  ): RelationCombinationProof =
    require(
      premises.forall { case (_, relation) =>
        relation.isPositionRelation && (relation.origin match
          case RelationEvidenceOrigin.PositionSnapshot(_) => true
          case _                                           => false)
      },
      "a combined relation may consume only certified canonical position facts"
    )
    val exact = premises.map { case (occurrence, relation) =>
      RelationCombinationPremise(occurrence, relation.kind, relation.semanticId)
    }
    require(exact.distinct.size == exact.size, "a combined relation cannot consume one premise twice")
    RelationCombinationProof(
      contract,
      exact.sortBy(_.stableKey),
      proofKeys.sortBy(_.stableKey)
    )

enum RelationBlockerRemovalMode:
  case Moved
  case Captured

enum RelationWitnessDetail:
  case GeometricControl(
      side: Color,
      attackerSquare: EvidenceSquare,
      attackerRole: EvidencePieceRole,
      targetSquare: EvidenceSquare,
      target: RelationControlTarget
  )
  case LegalMove(
      side: Color,
      moverSquare: EvidenceSquare,
      moverRole: EvidencePieceRole,
      destinationSquare: EvidenceSquare,
      moveUci: String,
      capture: Option[RelationLegalCaptureWitness]
  )
  case GeometricControlSetDelta(
      mover: RelationMoveTransitionWitness,
      controllingSide: Color,
      targetSquare: EvidenceSquare,
      beforeTarget: RelationControlTarget,
      afterTarget: RelationControlTarget,
      beforeControllers: List[RelationPieceWitness],
      afterControllers: List[RelationPieceWitness],
      removedControllers: List[RelationPieceWitness],
      establishedControllers: List[RelationPieceWitness],
      proof: RelationCombinationProof
  )
  case GeometricSupporterCapture(
      capturer: RelationMoveTransitionWitness,
      supporterSquare: EvidenceSquare,
      supporterRole: EvidencePieceRole,
      supportedSquare: EvidenceSquare,
      supportedRole: EvidencePieceRole,
      proof: RelationCombinationProof
  )
  case GeometricSupportDelta(
      mover: RelationMoveTransitionWitness,
      supportedSide: Color,
      supportedBeforeSquare: EvidenceSquare,
      supportedBeforeRole: EvidencePieceRole,
      supportedAfterSquare: EvidenceSquare,
      supportedAfterRole: EvidencePieceRole,
      beforeSupporters: List[RelationPieceWitness],
      afterSupporters: List[RelationPieceWitness],
      removedSupporters: List[RelationPieceWitness],
      establishedSupporters: List[RelationPieceWitness],
      proof: RelationCombinationProof
  )
  case SliderControlInterference(
      interposer: RelationMoveTransitionWitness,
      controllerSide: Color,
      controllerSquare: EvidenceSquare,
      controllerRole: EvidencePieceRole,
      targetSquare: EvidenceSquare,
      target: RelationControlTarget,
      proof: RelationCombinationProof
  )
  case GeometricLineControlAfterBlockerRemoval(
      mover: RelationMoveTransitionWitness,
      controllerSide: Color,
      controllerBeforeSquare: EvidenceSquare,
      controllerAfterSquare: EvidenceSquare,
      controllerRole: EvidencePieceRole,
      blockerSquare: EvidenceSquare,
      blockerRole: EvidencePieceRole,
      targetSquare: EvidenceSquare,
      target: RelationControlTarget,
      barrierPattern: RelationRayPattern,
      removalMode: RelationBlockerRemovalMode,
      proof: RelationCombinationProof
  )
  case CheckingEnemyControlBundle(
      mover: RelationMoveTransitionWitness,
      kingControls: List[RelationEnemyControlWitness],
      otherEnemyControls: List[RelationEnemyControlWitness],
      proof: RelationCombinationProof
  )
  case PawnFileGroup(side: Color, file: EvidenceFile, pawns: List[EvidenceSquare])
  case PawnTension(whitePawnSquare: EvidenceSquare, blackPawnSquare: EvidenceSquare)
  case PawnFrontOccupancy(
      side: Color,
      pawnSquare: EvidenceSquare,
      frontSquare: Option[EvidenceSquare],
      occupant: Option[RelationColoredPieceWitness]
  )
  case PawnPassage(side: Color, pawnSquare: EvidenceSquare, blockerSquares: List[EvidenceSquare])
  case MajorPieceFileOccupancy(
      side: Color,
      file: EvidenceFile,
      occupants: List[RelationPieceWitness],
      open: Boolean
  )
  case DoubleCheck(
      mover: RelationMoveTransitionWitness,
      kingSquare: EvidenceSquare,
      checkers: List[RelationPieceWitness],
      proof: RelationCombinationProof
  )
  case RayBarrier(
      side: Color,
      attackerSquare: EvidenceSquare,
      attackerRole: EvidencePieceRole,
      occupants: List[RelationColoredPieceWitness],
      axis: RelationAxisSignal
  )

  def detailName: String =
    this match
      case ray: RayBarrier =>
        RelationRayPattern.id(RelationRayProjection.pattern(ray))
      case _ => RelationFactKind.id(RelationWitnessDetail.factKind(this))

/** Named ray semantics are a projection of the complete ordered barrier, not
  * a second board fact. Distant occupants remain in the source RayBarrier but
  * do not make an unchanged pin, skewer, x-ray, or battery newly established.
  */
private[chessjudgment] final case class RelationNamedRayProjection(
    side: Color,
    attackerSquare: EvidenceSquare,
    attackerRole: EvidencePieceRole,
    barrier: RelationColoredPieceWitness,
    immediateTarget: Option[RelationColoredPieceWitness],
    axis: RelationAxisSignal,
    pattern: RelationRayPattern
)

private[chessjudgment] final case class RelationBatteryFormationProjection(
    side: Color,
    attackerSquare: EvidenceSquare,
    attackerRole: EvidencePieceRole,
    supportingSlider: RelationColoredPieceWitness,
    axis: RelationAxisSignal
)

private[chessjudgment] object RelationRayProjection:
  def pattern(detail: RelationWitnessDetail.RayBarrier): RelationRayPattern =
    RelationRayPattern.classify(detail.side, detail.occupants, detail.axis)

  def immediateTarget(
      detail: RelationWitnessDetail.RayBarrier
  ): Option[RelationColoredPieceWitness] =
    val rear = detail.occupants.lift(1)
    pattern(detail) match
      case RelationRayPattern.Pin | RelationRayPattern.XRay | RelationRayPattern.Skewer =>
        rear
      case RelationRayPattern.Battery =>
        rear.filter(_.side != detail.side)
      case RelationRayPattern.Ordinary =>
        None

  def named(
      detail: RelationWitnessDetail.RayBarrier
  ): Option[RelationNamedRayProjection] =
    val exactPattern = pattern(detail)
    Option.when(exactPattern != RelationRayPattern.Ordinary)(
      RelationNamedRayProjection(
        side = detail.side,
        attackerSquare = detail.attackerSquare,
        attackerRole = detail.attackerRole,
        barrier = detail.occupants.head,
        immediateTarget = immediateTarget(detail),
        axis = detail.axis,
        pattern = exactPattern
      )
    )

  def batteryFormation(
      detail: RelationWitnessDetail.RayBarrier
  ): Option[RelationBatteryFormationProjection] =
    Option.when(pattern(detail) == RelationRayPattern.Battery)(
      RelationBatteryFormationProjection(
        side = detail.side,
        attackerSquare = detail.attackerSquare,
        attackerRole = detail.attackerRole,
        supportingSlider = detail.occupants.head,
        axis = detail.axis
      )
    )

object RelationWitnessDetail:
  private def atom(value: String): String =
    val normalized = Option(value).getOrElse("").trim.toLowerCase
    s"${normalized.length}:$normalized"

  private def tuple(label: String, values: List[String]): String =
    atom(label) + values.map(atom).mkString

  private def values(items: Iterable[String]): String =
    items.toList.sorted.map(atom).mkString

  private def sequence(items: Iterable[String]): String =
    items.iterator.map(atom).mkString

  private def square(value: EvidenceSquare): String = value.key
  private def role(value: EvidencePieceRole): String = value.name
  private def side(value: Color): String = value.toString
  private def boardFile(value: EvidenceFile): String = value.key
  private def move(value: String): String = EvidenceRef.normalizeMove(value)
  private def piece(value: RelationPieceWitness): String =
    tuple("piece", List(square(value.square), role(value.role)))
  private def enemyControl(value: RelationEnemyControlWitness): String =
    tuple(
      "enemy-control",
      List(
        square(value.controllerSquare),
        role(value.controllerRole),
        square(value.targetSquare),
        role(value.targetRole)
      )
    )
  private def coloredPiece(value: RelationColoredPieceWitness): String =
    tuple("colored-piece", List(square(value.square), role(value.role), side(value.side)))
  private def moveTransition(value: RelationMoveTransitionWitness): String =
    tuple(
      "move-transition",
      List(
        side(value.side),
        square(value.from),
        square(value.to),
        role(value.beforeRole),
        role(value.afterRole)
      )
    )
  private def controlTarget(value: RelationControlTarget): String =
    value match
      case RelationControlTarget.Empty => tuple("empty", Nil)
      case RelationControlTarget.Friendly(roleValue) => tuple("friendly", List(role(roleValue)))
      case RelationControlTarget.Enemy(roleValue) => tuple("enemy", List(role(roleValue)))
  private def legalCapture(value: RelationLegalCaptureWitness): String =
    tuple(
      "legal-capture",
      List(
        square(value.capturedSquare),
        role(value.capturedRole),
        side(value.capturedSide)
      )
    )
  private def optional[A](value: Option[A])(encode: A => String): String =
    value.map(item => tuple("some", List(encode(item)))).getOrElse(tuple("none", Nil))

  /** Injective, representation-independent identity of one typed relation
    * detail. Case-class `toString` is intentionally excluded from graph ids,
    * ordering, and public mechanism identity.
    */
  def stableKey(detail: RelationWitnessDetail): String =
    val fields = detail match
      case GeometricControl(owner, attacker, attackerRole, target, targetState) =>
        List(side(owner), square(attacker), role(attackerRole), square(target), controlTarget(targetState))
      case LegalMove(owner, mover, moverRole, destination, moveUci, captured) =>
        List(
          side(owner),
          square(mover),
          role(moverRole),
          square(destination),
          move(moveUci),
          optional(captured)(legalCapture)
        )
      case GeometricControlSetDelta(
            mover,
            controllingSide,
            target,
            beforeTarget,
            afterTarget,
            beforeControllers,
            afterControllers,
            removedControllers,
            establishedControllers,
            proof
          ) =>
        require(
          beforeControllers.nonEmpty || afterControllers.nonEmpty,
          "a geometric control-set delta needs one exact controller occurrence"
        )
        List(
          moveTransition(mover),
          side(controllingSide),
          square(target),
          controlTarget(beforeTarget),
          controlTarget(afterTarget),
          values(beforeControllers.map(piece)),
          values(afterControllers.map(piece)),
          values(removedControllers.map(piece)),
          values(establishedControllers.map(piece)),
          proof.stableKey
        )
      case GeometricSupporterCapture(
            capturer,
            supporter,
            supporterRole,
            supported,
            supportedRole,
            proof
        ) =>
        List(
          moveTransition(capturer),
          square(supporter),
          role(supporterRole),
          square(supported),
          role(supportedRole),
          proof.stableKey
        )
      case GeometricSupportDelta(
            mover,
            supportedSide,
            supportedBefore,
            supportedBeforeRole,
            supportedAfter,
            supportedAfterRole,
            beforeSupporters,
            afterSupporters,
            removedSupporters,
            establishedSupporters,
            proof
          ) =>
        require(
          removedSupporters.nonEmpty || establishedSupporters.nonEmpty,
          "a geometric support delta needs at least one changed supporter"
        )
        List(
          moveTransition(mover),
          side(supportedSide),
          square(supportedBefore),
          role(supportedBeforeRole),
          square(supportedAfter),
          role(supportedAfterRole),
          values(beforeSupporters.map(piece)),
          values(afterSupporters.map(piece)),
          values(removedSupporters.map(piece)),
          values(establishedSupporters.map(piece)),
          proof.stableKey
        )
      case SliderControlInterference(
            interposer,
            controllerSide,
            controller,
            controllerRole,
            target,
            targetState,
            proof
        ) =>
        List(
          moveTransition(interposer),
          side(controllerSide),
          square(controller),
          role(controllerRole),
          square(target),
          controlTarget(targetState),
          proof.stableKey
        )
      case GeometricLineControlAfterBlockerRemoval(
            mover,
            controllerSide,
            controllerBefore,
            controllerAfter,
            controllerRole,
            blocker,
            blockerRole,
            target,
            targetState,
            barrierPattern,
            removalMode,
            proof
        ) =>
        List(
          moveTransition(mover),
          side(controllerSide),
          square(controllerBefore),
          square(controllerAfter),
          role(controllerRole),
          square(blocker),
          role(blockerRole),
          square(target),
          controlTarget(targetState),
          RelationRayPattern.id(barrierPattern),
          removalMode.toString,
          proof.stableKey
        )
      case CheckingEnemyControlBundle(mover, kingControls, otherEnemyControls, proof) =>
        List(
          moveTransition(mover),
          values(kingControls.map(enemyControl)),
          values(otherEnemyControls.map(enemyControl)),
          proof.stableKey
        )
      case PawnFileGroup(owner, file, pawns) =>
        List(side(owner), boardFile(file), values(pawns.map(square)))
      case PawnTension(whitePawn, blackPawn) =>
        List(square(whitePawn), square(blackPawn))
      case PawnFrontOccupancy(owner, pawn, front, occupant) =>
        List(side(owner), square(pawn), optional(front)(square), optional(occupant)(coloredPiece))
      case PawnPassage(owner, pawn, blockers) =>
        List(side(owner), square(pawn), values(blockers.map(square)))
      case MajorPieceFileOccupancy(owner, file, occupants, open) =>
        List(side(owner), boardFile(file), values(occupants.map(piece)), open.toString)
      case DoubleCheck(mover, king, checkers, proof) =>
        List(
          moveTransition(mover),
          square(king),
          values(checkers.map(piece)),
          proof.stableKey
        )
      case RayBarrier(owner, attacker, attackerRole, occupants, axis) =>
        require(occupants.nonEmpty, "a ray barrier needs its ordered occupied topology")
        List(
          side(owner),
          square(attacker),
          role(attackerRole),
          sequence(occupants.map(coloredPiece)),
          axis.toString
        )
    tuple(RelationFactKind.id(factKind(detail)), fields)

  def stableOccurrenceKey(detail: RelationWitnessDetail, lineMoves: List[String]): String =
    tuple(
      "relation-occurrence",
      List(stableKey(detail), sequence(lineMoves.map(move)))
    )

  def factKind(detail: RelationWitnessDetail): RelationFactKind =
    detail match
      case _: GeometricControl     => RelationFactKind.GeometricControl
      case _: LegalMove            => RelationFactKind.LegalMove
      case _: GeometricControlSetDelta => RelationFactKind.GeometricControlSetDelta
      case _: GeometricSupporterCapture => RelationFactKind.GeometricSupporterCapture
      case _: GeometricSupportDelta => RelationFactKind.GeometricSupportDelta
      case _: SliderControlInterference         => RelationFactKind.SliderControlInterference
      case _: GeometricLineControlAfterBlockerRemoval =>
        RelationFactKind.GeometricLineControlAfterBlockerRemoval
      case _: CheckingEnemyControlBundle => RelationFactKind.CheckingEnemyControlBundle
      case _: PawnFileGroup        => RelationFactKind.PawnFileGroup
      case _: PawnTension          => RelationFactKind.PawnTension
      case _: PawnFrontOccupancy   => RelationFactKind.PawnFrontOccupancy
      case _: PawnPassage          => RelationFactKind.PawnPassage
      case _: MajorPieceFileOccupancy => RelationFactKind.MajorPieceFileOccupancy
      case _: DoubleCheck          => RelationFactKind.DoubleCheck
      case _: RayBarrier           => RelationFactKind.RayBarrier

  private[chessjudgment] def validCombinationProof(detail: RelationWitnessDetail): Boolean =
    combinationProof(detail).exists(proof =>
      RelationCombinationContractKind.forDetail(detail).contains(proof.contract)
    )

  private[chessjudgment] def combinationProof(
      detail: RelationWitnessDetail
  ): Option[RelationCombinationProof] =
    detail match
      case GeometricSupporterCapture(_, _, _, _, _, proof) => Some(proof)
      case GeometricControlSetDelta(_, _, _, _, _, _, _, _, _, proof) => Some(proof)
      case GeometricSupportDelta(_, _, _, _, _, _, _, _, _, _, proof) => Some(proof)
      case SliderControlInterference(_, _, _, _, _, _, proof) => Some(proof)
      case GeometricLineControlAfterBlockerRemoval(_, _, _, _, _, _, _, _, _, _, _, proof) => Some(proof)
      case CheckingEnemyControlBundle(_, _, _, proof) => Some(proof)
      case DoubleCheck(_, _, _, proof) => Some(proof)
      case _ => None

  def combinationPremises(detail: RelationWitnessDetail): List[RelationCombinationPremise] =
    combinationProof(detail).toList.flatMap(_.premises)

  def focusSquares(detail: RelationWitnessDetail): List[EvidenceSquare] =
    val squares =
      detail match
        case GeometricControl(_, attackerSquare, _, targetSquare, _) =>
          List(attackerSquare, targetSquare)
        case LegalMove(_, moverSquare, _, destinationSquare, _, capture) =>
          List(moverSquare, destinationSquare) ++ capture.map(_.capturedSquare)
        case GeometricControlSetDelta(
              mover,
              _,
              target,
              _,
              _,
              beforeControllers,
              afterControllers,
              removedControllers,
              establishedControllers,
              _
            ) =>
          mover.from :: mover.to :: target ::
            (beforeControllers ++ afterControllers ++ removedControllers ++ establishedControllers).map(_.square)
        case GeometricSupporterCapture(mover, defender, _, beneficiary, _, _) =>
          List(mover.from, mover.to, defender, beneficiary)
        case GeometricSupportDelta(
              mover,
              _,
              supportedBefore,
              _,
              supportedAfter,
              _,
              beforeSupporters,
              afterSupporters,
              removedSupporters,
              establishedSupporters,
              _
            ) =>
          mover.from :: mover.to :: supportedBefore :: supportedAfter ::
            (beforeSupporters ++ afterSupporters ++ removedSupporters ++ establishedSupporters).map(_.square)
        case SliderControlInterference(mover, _, controller, _, target, _, _) =>
          List(mover.from, mover.to, controller, target)
        case GeometricLineControlAfterBlockerRemoval(mover, _, controllerBefore, controllerAfter, _, blocker, _, target, _, _, _, _) =>
          List(mover.from, mover.to, controllerBefore, controllerAfter, blocker, target)
        case CheckingEnemyControlBundle(mover, kingControls, otherEnemyControls, _) =>
          mover.from :: mover.to :: (kingControls ++ otherEnemyControls).flatMap(control =>
            List(control.controllerSquare, control.targetSquare)
          )
        case PawnFileGroup(_, _, pawns) =>
          pawns
        case PawnTension(whitePawnSquare, blackPawnSquare) =>
          List(whitePawnSquare, blackPawnSquare)
        case PawnFrontOccupancy(_, pawnSquare, frontSquare, occupant) =>
          pawnSquare :: (frontSquare.toList ++ occupant.map(_.square))
        case PawnPassage(_, pawnSquare, blockerSquares) =>
          pawnSquare :: blockerSquares
        case MajorPieceFileOccupancy(_, _, occupants, _) =>
          occupants.map(_.square)
        case DoubleCheck(mover, kingSquare, checkers, _) =>
          mover.from :: mover.to :: kingSquare :: checkers.map(_.square)
        case RayBarrier(_, attackerSquare, _, occupants, _) =>
          attackerSquare :: occupants.map(_.square)
    squares.distinct

  def targetSquares(detail: RelationWitnessDetail): List[EvidenceSquare] =
    val squares = detail match
      case GeometricControl(_, _, _, targetSquare, _) =>
        List(targetSquare)
      case LegalMove(_, _, _, destinationSquare, _, capture) =>
        capture.map(_.capturedSquare).getOrElse(destinationSquare) :: Nil
      case GeometricControlSetDelta(_, _, target, _, _, _, _, _, _, _) =>
        List(target)
      case GeometricSupporterCapture(_, defender, _, beneficiary, _, _) =>
        List(defender, beneficiary)
      case GeometricSupportDelta(_, _, _, _, supportedAfter, _, _, _, _, _, _) =>
        List(supportedAfter)
      case SliderControlInterference(mover, _, _, _, target, _, _) =>
        List(mover.to, target)
      case GeometricLineControlAfterBlockerRemoval(_, _, _, _, _, _, _, target, _, _, _, _) =>
        List(target)
      case CheckingEnemyControlBundle(_, kingControls, otherEnemyControls, _) =>
        (kingControls ++ otherEnemyControls).map(_.targetSquare)
      case PawnFileGroup(_, _, pawns) =>
        pawns
      case PawnTension(whitePawnSquare, blackPawnSquare) =>
        List(whitePawnSquare, blackPawnSquare)
      case PawnFrontOccupancy(_, pawnSquare, frontSquare, _) =>
        frontSquare.toList match
          case Nil     => List(pawnSquare)
          case squares => squares
      case PawnPassage(_, pawnSquare, _) =>
        List(pawnSquare)
      case MajorPieceFileOccupancy(_, _, occupants, _) =>
        occupants.map(_.square)
      case DoubleCheck(_, kingSquare, _, _) =>
        List(kingSquare)
      case ray: RayBarrier =>
        RelationRayProjection.immediateTarget(ray).map(_.square).toList
    squares.distinct

  def files(detail: RelationWitnessDetail): List[EvidenceFile] =
    detail match
      case MajorPieceFileOccupancy(_, file, _, _) => List(file)
      case PawnFileGroup(_, file, _)                => List(file)
      case _                                       => Nil

  def participants(detail: RelationWitnessDetail): List[RelationParticipant] =
    val values =
      detail match
        case GeometricControl(_, attackerSquare, attackerRole, targetSquare, targetState) =>
          List(
            part(
              attackerSquare,
              RelationParticipantRole.Controller,
              Some(attackerRole)
            ),
            part(
              targetSquare,
              targetState match
                case RelationControlTarget.Friendly(_) => RelationParticipantRole.Supported
                case _                                 => RelationParticipantRole.Target,
              targetState.pieceRole
            )
          )
        case LegalMove(_, moverSquare, moverRole, destinationSquare, _, capture) =>
          part(moverSquare, RelationParticipantRole.Mover, Some(moverRole)) ::
            capture
              .map(value => part(value.capturedSquare, RelationParticipantRole.Target, Some(value.capturedRole)))
              .orElse(Some(part(destinationSquare, RelationParticipantRole.Target)))
              .toList
        case GeometricControlSetDelta(
              mover,
              _,
              target,
              beforeTarget,
              afterTarget,
              beforeControllers,
              afterControllers,
              removedControllers,
              establishedControllers,
              _
            ) =>
          List(
            part(mover.from, RelationParticipantRole.Mover, Some(mover.beforeRole)),
            part(mover.to, RelationParticipantRole.Mover, Some(mover.afterRole)),
            part(
              target,
              (beforeTarget, afterTarget) match
                case (RelationControlTarget.Friendly(_), _) | (_, RelationControlTarget.Friendly(_)) =>
                  RelationParticipantRole.Supported
                case _ => RelationParticipantRole.Target,
              afterTarget.pieceRole.orElse(beforeTarget.pieceRole)
            )
          ) ++ (beforeControllers ++ afterControllers ++ removedControllers ++ establishedControllers).map(value =>
            part(value.square, RelationParticipantRole.Controller, Some(value.role))
          )
        case GeometricSupporterCapture(mover, defender, defenderRole, beneficiary, beneficiaryRole, _) =>
          List(
            part(mover.from, RelationParticipantRole.Mover, Some(mover.beforeRole)),
            part(mover.to, RelationParticipantRole.Mover, Some(mover.afterRole)),
            part(defender, RelationParticipantRole.Controller, Some(defenderRole)),
            part(beneficiary, RelationParticipantRole.Supported, Some(beneficiaryRole))
          )
        case GeometricSupportDelta(
              mover,
              _,
              supportedBefore,
              supportedBeforeRole,
              supportedAfter,
              supportedAfterRole,
              beforeSupporters,
              afterSupporters,
              removedSupporters,
              establishedSupporters,
              _
            ) =>
          List(
            part(mover.from, RelationParticipantRole.Mover, Some(mover.beforeRole)),
            part(mover.to, RelationParticipantRole.Mover, Some(mover.afterRole)),
            part(supportedBefore, RelationParticipantRole.Supported, Some(supportedBeforeRole)),
            part(supportedAfter, RelationParticipantRole.Supported, Some(supportedAfterRole))
          ) ++ beforeSupporters.map(value =>
            part(value.square, RelationParticipantRole.Controller, Some(value.role))
          ) ++ afterSupporters.map(value =>
            part(value.square, RelationParticipantRole.Controller, Some(value.role))
          ) ++ removedSupporters.map(value =>
            part(value.square, RelationParticipantRole.Controller, Some(value.role))
          ) ++ establishedSupporters.map(value =>
            part(value.square, RelationParticipantRole.Controller, Some(value.role))
          )
        case SliderControlInterference(mover, _, controller, controllerRole, target, targetState, _) =>
          List(
            part(mover.from, RelationParticipantRole.Mover, Some(mover.beforeRole)),
            part(mover.to, RelationParticipantRole.Blocker, Some(mover.afterRole)),
            part(controller, RelationParticipantRole.Controller, Some(controllerRole)),
            part(
              target,
              targetState match
                case RelationControlTarget.Friendly(_) => RelationParticipantRole.Supported
                case _                                 => RelationParticipantRole.Target,
              targetState.pieceRole
            )
          )
        case GeometricLineControlAfterBlockerRemoval(mover, _, controllerBefore, controllerAfter, controllerRole, blocker, blockerRole, target, targetState, _, _, _) =>
          List(
            part(mover.from, RelationParticipantRole.Mover, Some(mover.beforeRole)),
            part(mover.to, RelationParticipantRole.Mover, Some(mover.afterRole)),
            part(controllerBefore, RelationParticipantRole.Controller, Some(controllerRole)),
            part(controllerAfter, RelationParticipantRole.Controller, Some(controllerRole)),
            part(blocker, RelationParticipantRole.Blocker, Some(blockerRole)),
            part(
              target,
              targetState match
                case RelationControlTarget.Friendly(_) => RelationParticipantRole.Supported
                case _                                 => RelationParticipantRole.Target,
              targetState.pieceRole
            )
          )
        case CheckingEnemyControlBundle(mover, kingControls, otherEnemyControls, _) =>
          List(
            part(mover.from, RelationParticipantRole.Mover, Some(mover.beforeRole)),
            part(mover.to, RelationParticipantRole.Mover, Some(mover.afterRole))
          ) ++ kingControls.flatMap(control =>
            List(
              part(control.controllerSquare, RelationParticipantRole.Attacker, Some(control.controllerRole)),
              part(control.targetSquare, RelationParticipantRole.King, Some(control.targetRole))
            )
          ) ++ otherEnemyControls.flatMap(control =>
            List(
              part(control.controllerSquare, RelationParticipantRole.Attacker, Some(control.controllerRole)),
              part(control.targetSquare, RelationParticipantRole.Target, Some(control.targetRole))
            )
          )
        case PawnFileGroup(_, _, pawns) =>
          pawns.map(part(_, RelationParticipantRole.Other, Some(EvidencePieceRole("pawn"))))
        case PawnTension(whitePawnSquare, blackPawnSquare) =>
          List(
            part(whitePawnSquare, RelationParticipantRole.Attacker, Some(EvidencePieceRole("pawn"))),
            part(blackPawnSquare, RelationParticipantRole.Attacker, Some(EvidencePieceRole("pawn")))
          )
        case PawnFrontOccupancy(_, pawnSquare, frontSquare, occupant) =>
          part(pawnSquare, RelationParticipantRole.Other, Some(EvidencePieceRole("pawn"))) ::
            occupant.map(value =>
              part(value.square, RelationParticipantRole.Blocker, Some(value.role))
            ).toList ++
            frontSquare.filterNot(square => occupant.exists(_.square == square))
              .map(part(_, RelationParticipantRole.Other)).toList
        case PawnPassage(_, pawnSquare, blockerSquares) =>
          part(pawnSquare, RelationParticipantRole.Beneficiary, Some(EvidencePieceRole("pawn"))) ::
            blockerSquares.map(part(_, RelationParticipantRole.Blocker, Some(EvidencePieceRole("pawn"))))
        case MajorPieceFileOccupancy(_, _, occupants, _) =>
          occupants.map(value => part(value.square, RelationParticipantRole.Beneficiary, Some(value.role)))
        case DoubleCheck(mover, kingSquare, checkers, _) =>
          List(
            part(mover.from, RelationParticipantRole.Mover, Some(mover.beforeRole)),
            part(mover.to, RelationParticipantRole.Mover, Some(mover.afterRole)),
            part(kingSquare, RelationParticipantRole.King, Some(EvidencePieceRole(King.name)))
          ) ++ checkers.map(checker =>
            part(checker.square, RelationParticipantRole.Attacker, Some(checker.role))
          )
        case ray @ RayBarrier(_, attackerSquare, attackerRole, occupants, _) =>
          val pattern = RelationRayProjection.pattern(ray)
          val barrier = occupants.head
          val exactTarget = targetSquares(detail).toSet
          val barrierParticipantRole =
            if pattern == RelationRayPattern.Battery then RelationParticipantRole.Attacker
            else if barrier.role.name.equalsIgnoreCase(King.name) then RelationParticipantRole.King
            else RelationParticipantRole.Blocker
          List(
            part(attackerSquare, RelationParticipantRole.Attacker, Some(attackerRole)),
            part(barrier.square, barrierParticipantRole, Some(barrier.role))
          ) ++ occupants.drop(1).zipWithIndex.map { case (piece, rearIndex) =>
            val participantRole =
              if rearIndex == 0 && exactTarget(piece.square) then
                if piece.role.name.equalsIgnoreCase(King.name) then RelationParticipantRole.King
                else RelationParticipantRole.Target
              else RelationParticipantRole.Other
            part(
              piece.square,
              participantRole,
              Some(piece.role)
            )
          }
    values.distinct

  private def part(
      square: EvidenceSquare,
      participantRole: RelationParticipantRole,
      role: Option[EvidencePieceRole] = None
  ): RelationParticipant =
    RelationParticipant(square = square, role = role, participantRole = participantRole)

enum RelationFactKind:
  case GeometricControl
  case LegalMove
  case GeometricControlSetDelta
  case GeometricSupporterCapture
  case GeometricSupportDelta
  case SliderControlInterference
  case GeometricLineControlAfterBlockerRemoval
  case CheckingEnemyControlBundle
  case PawnFileGroup
  case PawnTension
  case PawnFrontOccupancy
  case PawnPassage
  case MajorPieceFileOccupancy
  case DoubleCheck
  case RayBarrier

object RelationFactKind:
  private val byId: Map[String, RelationFactKind] =
    RelationFactKind.values.iterator.map(kind => id(kind) -> kind).toMap

  def fromId(raw: String): Option[RelationFactKind] =
    byId.get(Option(raw).getOrElse("").trim.toLowerCase)
  def id(kind: RelationFactKind): String =
    kind match
      case GeometricControl      => "geometric_control"
      case LegalMove             => "legal_move"
      case GeometricControlSetDelta => "geometric_control_set_delta"
      case GeometricSupporterCapture => "geometric_supporter_capture"
      case GeometricSupportDelta => "geometric_support_delta"
      case SliderControlInterference          => "slider_control_interference"
      case GeometricLineControlAfterBlockerRemoval => "geometric_line_control_after_blocker_removal"
      case CheckingEnemyControlBundle => "checking_enemy_control_bundle"
      case PawnFileGroup         => "pawn_file_group"
      case PawnTension           => "pawn_tension"
      case PawnFrontOccupancy    => "pawn_front_occupancy"
      case PawnPassage           => "pawn_passage"
      case MajorPieceFileOccupancy => "major_piece_file_occupancy"
      case DoubleCheck           => "double_check"
      case RayBarrier            => "ray_barrier"

sealed trait EvidencePayload

final case class PositionFeatureEvidence(features: PositionFeatures) extends EvidencePayload

enum StrategicMechanismKind:
  case TargetPressure
  case CenterControl
  case KingSafety
  case Activity
  case PawnStructure
  case PlanPressure
  case Compensation
  case OpeningAlignment

enum StrategicMechanismSignalKind:
  case PlanPressure
  case OpeningAnchor
  case OpeningApplicability

enum StrategicAxisKind:
  case Counterplay
  case PlanCoherence

enum StrategicAxisPolarity:
  case Gain
  case Concede
  case Restrain
  case Support

enum StrategicSustainabilityHorizon:
  case Immediate
  case ShortPv
  case MediumPv
  case LongPv
  case Unknown

enum StrategicAxisComparisonOutcome:
  case ReferenceOnly
  case CandidateOnly
  case SharedSustained
  case CandidateConcession

final case class StrategicAxisDetail(
    kind: StrategicAxisKind,
    polarity: StrategicAxisPolarity,
    label: String
):
  def stableKey: String =
    s"$kind:$polarity:$label"

final case class StrategicMechanismSignal(
    kind: StrategicMechanismSignalKind,
    label: String,
    source: EvidenceRef,
    axis: Option[StrategicAxisDetail] = None,
    planResultAssessment: Option[PlanCausalResultAssessment] = None
):
  def sourceLayer: EvidenceLayer = source.layer
  def axisKey: Option[String] =
    axis.map(_.stableKey)

/** Exact PlanResult occurrence carried by one strategic axis. Keeping the
  * producer ref and assessment together prevents a later source/result
  * Cartesian product when several plan events or result routes share an axis.
  */
final case class StrategicAxisPlanResultBinding(
    source: EvidenceRef,
    assessment: PlanCausalResultAssessment
)

final case class StrategicMechanismEvidence(
    kind: StrategicMechanismKind,
    signals: List[StrategicMechanismSignal],
    semanticAnchors: List[EvidenceSemanticAnchor],
    private[chessjudgment] val assemblyProof: Option[StrategicMechanismAssemblyProof] = None
) extends EvidencePayload:
  private[chessjudgment] def exactAssemblyCertified(record: EvidenceRecord): Boolean =
    assemblyProof.exists(_.proves(record, this))
  def signalKinds: Set[StrategicMechanismSignalKind] =
    signals.map(_.kind).toSet
  def hasSignals: Boolean =
    signals.nonEmpty
  def hasProofSource: Boolean =
    hasResolvedPlanEvent || signalKinds.exists(kind =>
      kind == StrategicMechanismSignalKind.OpeningAnchor
    )
  def hasResolvedPlanEvent: Boolean =
    signals.exists(signal =>
      signal.kind == StrategicMechanismSignalKind.PlanPressure &&
        signal.sourceLayer == EvidenceLayer.PlanCausalEvent &&
        signal.axis.exists(_.kind == StrategicAxisKind.PlanCoherence)
    )
  def canAnchorStrategicClaim: Boolean =
    hasProofSource &&
      kind != StrategicMechanismKind.OpeningAlignment &&
      (kind match
        case StrategicMechanismKind.PlanPressure =>
          hasResolvedPlanEvent
        case _ =>
          true
      )
  def canAnchorPawnStructureClaim: Boolean =
    kind == StrategicMechanismKind.PawnStructure && hasSignals
  def canAnchorOpeningClaim: Boolean =
    kind == StrategicMechanismKind.OpeningAlignment &&
      signalKinds.contains(StrategicMechanismSignalKind.OpeningApplicability)
  def canAnchorPlanClaim: Boolean =
    kind == StrategicMechanismKind.PlanPressure &&
      hasResolvedPlanEvent
  def canSupportStrategicCause: Boolean =
    canAnchorStrategicClaim || canAnchorPawnStructureClaim
  def hasOpeningAnchorSignal: Boolean =
    signalKinds.contains(StrategicMechanismSignalKind.OpeningAnchor)
  def axisDetails: List[StrategicAxisDetail] =
    signals.flatMap(_.axis).distinctBy(_.stableKey)
  def hasStrategicAxis: Boolean =
    axisDetails.nonEmpty
  def hasPassedPawnResourceSignal: Boolean =
    kind == StrategicMechanismKind.PawnStructure &&
      canAnchorPawnStructureClaim &&
      hasAnySignalLabel(Set("passed-pawn-progress", "promotion-pressure-gain"))
  private def hasAnySignalLabel(labels: Set[String]): Boolean =
    signals.exists(signal => labels.contains(signal.label))
  def semanticGroupingAnchors: List[EvidenceSemanticAnchor] =
    (semanticAnchors ++ signals.flatMap(_.axis).map(axis => EvidenceSemanticAnchor.of(EvidenceSemanticAnchorKind.StrategicAxis, axis.stableKey)))
      .distinctBy(_.stableKey)

private[chessjudgment] final case class StrategicMechanismAssemblyProof private (
    ref: EvidenceRef,
    parents: List[EvidenceRef],
    kind: StrategicMechanismKind,
    signals: List[StrategicMechanismSignal],
    semanticAnchors: List[EvidenceSemanticAnchor]
):
  def proves(record: EvidenceRecord, payload: StrategicMechanismEvidence): Boolean =
    record.ref == ref &&
      record.parents == parents &&
      payload.kind == kind &&
      payload.signals == signals &&
      payload.semanticAnchors == semanticAnchors

private[chessjudgment] object StrategicMechanismAssemblyProof:
  def from(
      ref: EvidenceRef,
      parents: List[EvidenceRef],
      payload: StrategicMechanismEvidence
  ): StrategicMechanismAssemblyProof =
    StrategicMechanismAssemblyProof(
      ref,
      parents,
      payload.kind,
      payload.signals,
      payload.semanticAnchors
    )

final case class StrategicAxisComparison(
    axis: StrategicAxisDetail,
    outcome: StrategicAxisComparisonOutcome,
    referenceSources: List[EvidenceRef],
    candidateSources: List[EvidenceRef],
    referencePlanResults: List[StrategicAxisPlanResultBinding] = Nil,
    candidatePlanResults: List[StrategicAxisPlanResultBinding] = Nil
):
  def axisKey: String =
    axis.stableKey
  def sources: List[EvidenceRef] =
    (referenceSources ++ candidateSources).distinctBy(_.id)
  def sourcesFor(sourceSide: RelativeCauseSourceSide): List[EvidenceRef] =
    sourceSide match
      case RelativeCauseSourceSide.Reference => referenceSources
      case RelativeCauseSourceSide.Candidate => candidateSources
      case RelativeCauseSourceSide.Shared | RelativeCauseSourceSide.Mixed => sources
  def planResultsFor(
      sourceSide: RelativeCauseSourceSide
  ): List[StrategicAxisPlanResultBinding] =
    sourceSide match
      case RelativeCauseSourceSide.Reference => referencePlanResults
      case RelativeCauseSourceSide.Candidate => candidatePlanResults
      case RelativeCauseSourceSide.Shared | RelativeCauseSourceSide.Mixed =>
        (referencePlanResults ++ candidatePlanResults).distinct
  def hasContrast: Boolean =
    outcome == StrategicAxisComparisonOutcome.ReferenceOnly ||
      outcome == StrategicAxisComparisonOutcome.CandidateOnly ||
      outcome == StrategicAxisComparisonOutcome.CandidateConcession
  def candidateNegative: Boolean =
    (candidateSources.nonEmpty || outcome == StrategicAxisComparisonOutcome.CandidateConcession) &&
      (
        axis.polarity == StrategicAxisPolarity.Concede ||
          outcome == StrategicAxisComparisonOutcome.CandidateConcession
      )
  def referenceLead: Boolean =
    outcome == StrategicAxisComparisonOutcome.ReferenceOnly
  def candidateLead: Boolean =
    outcome == StrategicAxisComparisonOutcome.CandidateOnly ||
      outcome == StrategicAxisComparisonOutcome.CandidateConcession

final case class StrategicPlanComparison(
    referencePlanIds: List[String],
    candidatePlanIds: List[String]
):
  def hasPlanDelta: Boolean =
    referencePlanIds.sorted != candidatePlanIds.sorted

final case class StrategicSustainabilityAssessment(
    horizon: StrategicSustainabilityHorizon,
    lineMaintained: Boolean,
    pvMaintained: Boolean,
    referencePlyCount: Int,
    candidatePlyCount: Int,
    sustainedAxisKeys: List[String] = Nil
):
  def sustains(axisKey: String): Boolean =
    sustainedAxisKeys.contains(axisKey)
  def hasSustainedPv: Boolean =
    pvMaintained &&
      sustainedAxisKeys.nonEmpty &&
      (horizon == StrategicSustainabilityHorizon.ShortPv ||
        horizon == StrategicSustainabilityHorizon.MediumPv ||
        horizon == StrategicSustainabilityHorizon.LongPv)

final case class StrategicContrastSupport(
    directSources: List[EvidenceRef],
    contrastSources: List[EvidenceRef],
    contextSources: List[EvidenceRef]
):
  def all: List[EvidenceRef] =
    (directSources ++ contrastSources ++ contextSources).distinctBy(_.id)

final case class StrategicMechanismContrastEvidence(
    comparisonKind: CandidateComparisonKind,
    referenceLine: LineNodeRef,
    candidateLine: LineNodeRef,
    axisComparisons: List[StrategicAxisComparison],
    planComparison: Option[StrategicPlanComparison],
    sustainability: StrategicSustainabilityAssessment,
    support: StrategicContrastSupport,
    private[chessjudgment] val assemblyProof: Option[StrategicMechanismContrastAssemblyProof] = None
) extends EvidencePayload:
  private[chessjudgment] def exactAssemblyCertified(record: EvidenceRecord): Boolean =
    assemblyProof.exists(_.proves(record, this))
  def actionableComparisons: List[StrategicAxisComparison] =
    axisComparisons.filter(_.hasContrast)
  def sustainedActionableComparisons: List[StrategicAxisComparison] =
    actionableComparisons.filter(comparison => sustainability.sustains(comparison.axisKey))
  def sustainedActionableComparisonsFor(
      sourceSide: RelativeCauseSourceSide
  ): List[StrategicAxisComparison] =
    sustainedActionableComparisons.filter(_.sourcesFor(sourceSide).nonEmpty)
  def sustainedCauseComparisons(
      kind: RelativeCauseKind,
      sourceSide: RelativeCauseSourceSide
  ): List[StrategicAxisComparison] =
    val proving = sustainedActionableComparisonsFor(sourceSide).filter(comparison =>
      RelativeCauseKind.strategicAxisCanProveCause(kind, comparison.axis, sourceSide) &&
        (
          !RelativeCauseKind.requiresExactPlanResult(kind) ||
            comparison.planResultsFor(sourceSide).nonEmpty
        )
    )
    if kind == RelativeCauseKind.SacrificeCompensation then
      proving.filter(comparison =>
        sustainability.hasSustainedPv &&
          (sourceSide match
            case RelativeCauseSourceSide.Reference => comparison.referenceLead
            case RelativeCauseSourceSide.Candidate => comparison.candidateLead
            case RelativeCauseSourceSide.Shared | RelativeCauseSourceSide.Mixed => false)
      )
    else proving
  def hasActionableContrast: Boolean =
    actionableComparisons.nonEmpty || planComparison.exists(_.hasPlanDelta)
  def hasSustainedActionableContrast: Boolean =
    sustainedActionableComparisons.nonEmpty
  def sourceRefs: List[EvidenceRef] =
    support.all
  def axisKeys: List[String] =
    axisComparisons.map(_.axisKey).distinct.sorted

private[chessjudgment] final case class StrategicMechanismContrastAssemblyProof private (
    ref: EvidenceRef,
    parents: List[EvidenceRef],
    comparisonKind: CandidateComparisonKind,
    referenceLine: LineNodeRef,
    candidateLine: LineNodeRef,
    axisComparisons: List[StrategicAxisComparison],
    planComparison: Option[StrategicPlanComparison],
    sustainability: StrategicSustainabilityAssessment,
    support: StrategicContrastSupport
):
  def proves(record: EvidenceRecord, payload: StrategicMechanismContrastEvidence): Boolean =
    record.ref == ref &&
      record.parents == parents &&
      payload.comparisonKind == comparisonKind &&
      payload.referenceLine == referenceLine &&
      payload.candidateLine == candidateLine &&
      payload.axisComparisons == axisComparisons &&
      payload.planComparison == planComparison &&
      payload.sustainability == sustainability &&
      payload.support == support

private[chessjudgment] object StrategicMechanismContrastAssemblyProof:
  private[chessjudgment] def from(
      ref: EvidenceRef,
      parents: List[EvidenceRef],
      payload: StrategicMechanismContrastEvidence
  ): StrategicMechanismContrastAssemblyProof =
    StrategicMechanismContrastAssemblyProof(
      ref,
      parents,
      payload.comparisonKind,
      payload.referenceLine,
      payload.candidateLine,
      payload.axisComparisons,
      payload.planComparison,
      payload.sustainability,
      payload.support
    )

object StrategicMechanismContrastEvidence:
  private[chessjudgment] def currentMovePlanCoherenceAxis(axis: StrategicAxisDetail): Boolean =
    axis.kind == StrategicAxisKind.PlanCoherence &&
      axis.polarity == StrategicAxisPolarity.Gain

object StrategicMechanismEvidence:
  def rawStrategicSourceLayer(layer: EvidenceLayer): Boolean =
    layer match
      case EvidenceLayer.PlanCausalEvent | EvidenceLayer.FeatureAnchor |
          EvidenceLayer.ApplicabilityAssessment | EvidenceLayer.OpeningContext =>
        true
      case _ =>
        false

  def openingClaimSupported(records: List[EvidenceRecord]): Boolean =
    val mechanisms = records.collect { case EvidenceRecord(_, payload: StrategicMechanismEvidence, _) => payload }
    mechanisms.exists(_.canAnchorOpeningClaim)

  def sourceMechanisms(record: EvidenceRecord): List[(StrategicMechanismKind, StrategicMechanismSignal)] =
    record.payload match
      case _: StructuralDeltaEvidence =>
        Nil
      case payload: PlanCausalEventEvidence =>
        val axes = planCausalAxes(payload).flatMap { case (axis, assessment) =>
          concreteAxis(record, Some(axis)).map(_ -> assessment)
        }.distinct
        val admittedAxes = if axes.nonEmpty then axes.map((axis, assessment) => Some(axis) -> assessment)
          else List(None -> None)
        admittedAxes.map { case (axis, assessment) =>
          StrategicMechanismKind.PlanPressure ->
            signal(
              StrategicMechanismSignalKind.PlanPressure,
              payload.planId.id,
              record.ref,
              axis,
              assessment
            )
        }
      case FeatureAnchorEvidence(anchor) =>
        val mechanism =
          anchor.theme match
            case OpeningTheme.CenterControl    => StrategicMechanismKind.CenterControl
            case OpeningTheme.Development      => StrategicMechanismKind.Activity
            case OpeningTheme.PawnStructure    => StrategicMechanismKind.PawnStructure
            case OpeningTheme.GambitInitiative => StrategicMechanismKind.Compensation
            case OpeningTheme.KingSafety       => StrategicMechanismKind.KingSafety
            case OpeningTheme.PlanPressure     => StrategicMechanismKind.TargetPressure
        List(
          mechanism -> signal(
            StrategicMechanismSignalKind.OpeningAnchor,
            s"${anchor.theme}:${anchor.signal}",
            record.ref
          )
        )
      case ApplicabilityAssessmentEvidence(assessment) if assessment.canCertifyOpeningClaim =>
        List(
          StrategicMechanismKind.OpeningAlignment -> signal(
            StrategicMechanismSignalKind.OpeningApplicability,
            assessment.supportedThemes.map(_.toString).sorted.mkString(","),
            record.ref
          )
        )
      case _ =>
        Nil

  private def planCausalAxes(
      event: PlanCausalEventEvidence
  ): List[(StrategicAxisDetail, Option[PlanCausalResultAssessment])] =
    val directProof =
      event.structuralConsequences.nonEmpty ||
        event.rootEnablingDependencies.nonEmpty
    def planAxis(
        polarity: StrategicAxisPolarity,
        assessment: Option[PlanCausalResultAssessment]
    ): (StrategicAxisDetail, Option[PlanCausalResultAssessment]) =
      StrategicAxisDetail(StrategicAxisKind.PlanCoherence, polarity, event.planId.id) -> assessment
    val directAxes = Option.when(directProof)(planAxis(StrategicAxisPolarity.Support, None)).toList
    val robustAxes = event.exactRobustPublicResultAssessments.map(assessment =>
      planAxis(StrategicAxisPolarity.Gain, Some(assessment))
    )
    val refutedAxes = event.exactRefutedPublicResultAssessments.map(assessment =>
      planAxis(StrategicAxisPolarity.Concede, Some(assessment))
    )
    val conditionalAxes = event.positiveCausalResultAssessments
      .filter(_.robustness == PlanCausalRobustness.Conditional)
      .map(assessment => planAxis(StrategicAxisPolarity.Support, Some(assessment)))
    val planAxes = (directAxes ++ robustAxes ++ refutedAxes ++ conditionalAxes).distinct
    val opponentResourceAxes =
      Option
        .when(
          event.opponentResourceDeterrence.nonEmpty &&
            event.structuralConsequences.exists(consequence =>
              consequence.kind == TransitionConsequenceKind.OpponentMobilityRestriction &&
                consequence.subjectFacts.exists(StructuralDeltaEvidence.validOpponentMobilityRestrictionSubject)
            )
        )(
          StrategicAxisDetail(
            StrategicAxisKind.Counterplay,
            StrategicAxisPolarity.Restrain,
            "opponent-resource-deterrence"
          )
        )
        .toList
        .map(_ -> None)
    (planAxes ++ opponentResourceAxes).distinct

  def sourceSemanticAnchors(record: EvidenceRecord): List[EvidenceSemanticAnchor] =
    record.payload match
      case FeatureAnchorEvidence(anchor) =>
        List(EvidenceSemanticAnchor.of(EvidenceSemanticAnchorKind.OpeningAnchor, anchor.theme.toString, anchor.signal.toString))
      case ApplicabilityAssessmentEvidence(assessment) =>
        assessment.supportedThemes.map(theme => EvidenceSemanticAnchor.of(EvidenceSemanticAnchorKind.OpeningSupported, theme.toString)) ++
          assessment.observedThemes.map(theme => EvidenceSemanticAnchor.of(EvidenceSemanticAnchorKind.OpeningObserved, theme.toString))
      case payload: PlanCausalEventEvidence =>
        payload.semanticGroupingAnchors
      case PlanTransitionEvidence(proof) =>
        val transition = proof.summary
        transition.currentEvent.map(current =>
          EvidenceSemanticAnchor.of(
            EvidenceSemanticAnchorKind.PlanTransition,
            (transition.previousEvent.toList.map(_.goalKey) ++ List(current.goalKey) ++
              transition.continuity.toList.map(continuity => s"${continuity.consecutivePlies}-ply") ++
              proof.causalDependencies.map(dependency =>
                s"route:${dependency.dependencyKind}:${dependency.proof.kind}"
              ).distinct.sorted)*
          )
        ).toList
      case payload: StructuralDeltaEvidence =>
        (
          payload.signalAnchors.map(anchor => EvidenceSemanticAnchor.of(EvidenceSemanticAnchorKind.StructuralDelta, s"signal:$anchor")) ++
            payload.consequenceAnchors.map(anchor => EvidenceSemanticAnchor.of(EvidenceSemanticAnchorKind.StructuralDelta, s"consequence:$anchor"))
        ).distinct
      case _ =>
        Nil

  private def signal(
      kind: StrategicMechanismSignalKind,
      label: String,
      source: EvidenceRef,
      axis: Option[StrategicAxisDetail] = None,
      planResultAssessment: Option[PlanCausalResultAssessment] = None
  ): StrategicMechanismSignal =
    StrategicMechanismSignal(kind, label, source, axis, planResultAssessment)

  private def concreteAxis(record: EvidenceRecord, axis: Option[StrategicAxisDetail]): Option[StrategicAxisDetail] =
    axis.filter(_ => sourceHasAxisSubject(record))

  private def sourceHasAxisSubject(record: EvidenceRecord): Boolean =
    record.payload match
      case payload: PlanCausalEventEvidence =>
        payload.identity.actorRole.nonEmpty &&
          (
            payload.identity.actorFrom.zip(payload.identity.actorTo).exists { case (from, to) =>
              EvidenceRef.sameMove(s"$from$to", payload.rootMove)
            } ||
            payload.identity.targets.nonEmpty ||
              payload.structuralConsequences.exists(_.subjectFacts.nonEmpty) ||
              payload.episode.exists(_.planSequenceProven)
          )
      case FeatureAnchorEvidence(_) | ApplicabilityAssessmentEvidence(_) =>
        false
      case _ =>
        false

enum OpeningFamily:
  case A
  case B
  case C
  case D
  case E

object OpeningFamily:
  def fromEco(raw: String): Option[OpeningFamily] =
    Option(raw)
      .map(_.trim.toUpperCase)
      .flatMap(_.headOption)
      .flatMap(ch => fromRaw(ch.toString))

  def fromRaw(raw: String): Option[OpeningFamily] =
    Option(raw).map(_.trim.toUpperCase).collect:
      case "A" => OpeningFamily.A
      case "B" => OpeningFamily.B
      case "C" => OpeningFamily.C
      case "D" => OpeningFamily.D
      case "E" => OpeningFamily.E

enum OpeningContextSignal:
  case RecognizedIdentity
  case ThemePrior

enum OpeningTheme:
  case CenterControl
  case Development
  case PawnStructure
  case GambitInitiative
  case KingSafety
  case PlanPressure

enum OpeningThemePriorMatchSource:
  case ExactLineage
  case LineageAlias
  case NameHint
  case FamilyFallback

  def openingSpecific: Boolean =
    this != OpeningThemePriorMatchSource.FamilyFallback

  def canCertifyOpeningClaim: Boolean =
    this match
      case OpeningThemePriorMatchSource.ExactLineage | OpeningThemePriorMatchSource.LineageAlias =>
        true
      case OpeningThemePriorMatchSource.NameHint | OpeningThemePriorMatchSource.FamilyFallback =>
        false

enum FeatureAnchorSignal:
  case PawnStructureObserved
  case PawnTensionObserved
  case StructuralDeltaObserved

final case class FeatureAnchor(
    theme: OpeningTheme,
    signal: FeatureAnchorSignal,
    sourceLayer: EvidenceLayer
)

enum FeatureApplicability:
  case OpeningRelevant
  case ObservedOnly

enum ApplicabilityStatus:
  case InternalOnly
  case Supported
  case PartiallySupported
  case Unverified
  case Ambiguous

final case class ApplicabilityAssessment(
    applicability: FeatureApplicability,
    status: ApplicabilityStatus,
    observedThemes: List[OpeningTheme],
    supportedThemes: List[OpeningTheme],
    unverifiedPriorThemes: List[OpeningTheme],
    observedOnlyThemes: List[OpeningTheme],
    priorMatchSources: List[OpeningThemePriorMatchSource] = Nil
):
  def hasInternalAnchorAlignment: Boolean =
    applicability == FeatureApplicability.OpeningRelevant &&
      observedThemes.nonEmpty &&
      supportedThemes.nonEmpty &&
      (status == ApplicabilityStatus.Supported || status == ApplicabilityStatus.PartiallySupported)

  def hasCertifyingPriorEvidence: Boolean =
    priorMatchSources.exists(_.canCertifyOpeningClaim)

  def canCertifyOpeningClaim: Boolean =
    hasInternalAnchorAlignment && hasCertifyingPriorEvidence

final case class OpeningIdentity(
    eco: Option[String],
    name: Option[String],
    family: Option[OpeningFamily]
)

final case class OpeningCandidate(
    identity: OpeningIdentity,
    lineage: Option[String],
    frequency: Int,
    sampleCount: Int,
    confidence: Double
)

enum OpeningRecognitionMatchKind:
  case ExactPrefixAndPosition
  case PositionTransposition

final case class OpeningRecognition(
    movePrefixHash: String,
    positionKey: String,
    matchedBy: OpeningRecognitionMatchKind,
    candidates: List[OpeningCandidate],
    matchedPly: Int,
    frequency: Int,
    sampleCount: Int,
    confidence: Double
):
  def bestCandidate: Option[OpeningCandidate] =
    candidates.headOption

  def bestIdentity: Option[OpeningIdentity] =
    bestCandidate.map(_.identity)

  def lineage: Option[String] =
    bestCandidate.flatMap(_.lineage)

final case class OpeningThemePrior(
    lineage: Option[String],
    family: Option[OpeningFamily],
    themes: List[OpeningTheme],
    typicalPawnStructures: List[String],
    centerBreaks: List[String],
    developmentPriorities: List[String],
    gambitCompensation: Boolean,
    strategicPlanPriors: List[String]
)

final case class OpeningThemePriorSelection(
    prior: OpeningThemePrior,
    matchSource: OpeningThemePriorMatchSource,
    requestedLineage: Option[String],
    canonicalLineage: Option[String]
):
  def openingSpecific: Boolean =
    matchSource.openingSpecific

  def canCertifyOpeningClaim: Boolean =
    matchSource.canCertifyOpeningClaim

final case class OpeningContextEvidence(
    identity: Option[OpeningIdentity],
    signals: List[OpeningContextSignal],
    recognition: Option[OpeningRecognition] = None,
    themePriorSelection: Option[OpeningThemePriorSelection] = None
) extends EvidencePayload

final case class FeatureAnchorEvidence(
    anchor: FeatureAnchor
) extends EvidencePayload

final case class ApplicabilityAssessmentEvidence(
    assessment: ApplicabilityAssessment
) extends EvidencePayload

final case class ForcedLineThemeEvidence(
    id: String,
    lineMoves: List[String]
)

final case class LineReplayStep(
    ply: Int,
    moveUci: String,
    fenBefore: String,
    fenAfter: String
)

final case class LineObjectTrajectory private (
    rootStep: LineReplayStep,
    futureStep: LineReplayStep,
    pieceRole: EvidencePieceRole,
    color: Color,
    rootFrom: EvidenceSquare,
    rootTo: EvidenceSquare,
    futureFrom: EvidenceSquare,
    futureTo: EvidenceSquare,
    plyOffset: Int
)

object LineObjectTrajectory:
  private[chessjudgment] def find(
      rootStep: LineReplayStep,
      continuation: List[LineReplayStep],
      maxPlyOffset: Int,
      replay: CanonicalLineReplay
  ): Option[LineObjectTrajectory] =
    val rootMove = EvidenceRef.normalizeMove(rootStep.moveUci)
    for
      rootFrom <- Square.fromKey(rootMove.take(2))
      rootTo <- Square.fromKey(rootMove.slice(2, 4))
      before <- replay.before(rootStep)
      piece <- before.board.pieceAt(rootFrom)
      if replay.after(rootStep).exists(_.board.pieceAt(rootTo).contains(piece))
      (futureStep, index) <- continuation.zipWithIndex
        .take(maxPlyOffset.max(0))
        .takeWhile { case (step, _) =>
          replay.before(step).exists(_.board.pieceAt(rootTo).contains(piece))
        }
        .find { case (step, _) =>
          Square.fromKey(EvidenceRef.normalizeMove(step.moveUci).take(2)).contains(rootTo)
        }
      futureTo <- Square.fromKey(EvidenceRef.normalizeMove(futureStep.moveUci).slice(2, 4))
      if futureTo != rootFrom
      if replay.after(futureStep).exists(position =>
          position.board.pieceAt(futureTo).exists(afterPiece =>
            afterPiece.color == piece.color &&
              (afterPiece.role == piece.role || rootMove.length == 5 || EvidenceRef.normalizeMove(futureStep.moveUci).length == 5)
          )
        )
    yield LineObjectTrajectory(
      rootStep = rootStep,
      futureStep = futureStep,
      pieceRole = EvidencePieceRole(piece.role.toString),
      color = piece.color,
      rootFrom = EvidenceSquare(rootFrom.key),
      rootTo = EvidenceSquare(rootTo.key),
      futureFrom = EvidenceSquare(rootTo.key),
      futureTo = EvidenceSquare(futureTo.key),
      plyOffset = index + 1
    )

final case class LineAccessTrajectory private (
    enablingStep: LineReplayStep,
    enabledStep: LineReplayStep,
    interveningSteps: List[LineReplayStep],
    enabledPieceRole: EvidencePieceRole,
    color: Color,
    vacatedSquare: EvidenceSquare,
    enabledFrom: EvidenceSquare,
    enabledTo: EvidenceSquare,
    plyOffset: Int
)

object LineAccessTrajectory:
  /** Exact root-to-effect access used by causal episodes. Merely placing a
    * slider behind a blocker does not cause the blocker's later move or capture.
    */
  private[chessjudgment] def findRootClearanceBeforeUse(
      enablingStep: LineReplayStep,
      enabledStep: LineReplayStep,
      interveningSteps: List[LineReplayStep],
      replay: CanonicalLineReplay
  ): Option[LineAccessTrajectory] =
    val enablingMove = EvidenceRef.normalizeMove(enablingStep.moveUci)
    val enabledMove = EvidenceRef.normalizeMove(enabledStep.moveUci)
    for
      vacated <- Square.fromKey(enablingMove.take(2))
      enabledFrom <- Square.fromKey(enabledMove.take(2))
      enabledTo <- Square.fromKey(enabledMove.slice(2, 4))
      beforeEnabling <- replay.before(enablingStep)
      afterEnabling <- replay.after(enablingStep)
      enablingPiece <- beforeEnabling.board.pieceAt(vacated)
      enabledPiece <- beforeEnabling.board.pieceAt(enabledFrom)
      if enablingPiece.color == enabledPiece.color
      if afterEnabling.board.pieceAt(vacated).isEmpty
      if afterEnabling.board.pieceAt(enabledFrom).contains(enabledPiece)
      enabledPath = BoardGeometry.movementPath(enabledPiece, enabledFrom, enabledTo).map(_.key)
      if enabledTo == vacated || enabledPath.contains(vacated.key)
      if pathRemainsClear((enabledPath :+ vacated.key).distinct, enablingStep, enabledStep, interveningSteps, replay)
      if interveningSteps.forall(step =>
        replay.before(step).flatMap(_.board.pieceAt(enabledFrom)).contains(enabledPiece) &&
          replay.after(step).flatMap(_.board.pieceAt(enabledFrom)).contains(enabledPiece)
      )
      if replay.before(enabledStep).flatMap(_.board.pieceAt(enabledFrom)).contains(enabledPiece)
      if replay.after(enabledStep).flatMap(_.board.pieceAt(enabledTo)).exists(piece =>
        piece.color == enabledPiece.color &&
          (piece.role == enabledPiece.role || enabledMove.length == 5)
      )
    yield LineAccessTrajectory(
      enablingStep = enablingStep,
      enabledStep = enabledStep,
      interveningSteps = interveningSteps,
      enabledPieceRole = EvidencePieceRole(enabledPiece.role.toString),
      color = enabledPiece.color,
      vacatedSquare = EvidenceSquare(vacated.key),
      enabledFrom = EvidenceSquare(enabledFrom.key),
      enabledTo = EvidenceSquare(enabledTo.key),
      plyOffset = interveningSteps.size + 1
    )

  private def pathRemainsClear(
      path: List[String],
      enablingStep: LineReplayStep,
      enabledStep: LineReplayStep,
      interveningSteps: List[LineReplayStep],
      replay: CanonicalLineReplay
  ): Boolean =
    val observed =
      replay.after(enablingStep).toList ++
        interveningSteps.flatMap(step => List(replay.before(step), replay.after(step)).flatten) ++
        replay.before(enabledStep).toList
    observed.nonEmpty && observed.forall(position =>
      path.forall(square => Square.fromKey(square).forall(position.board.pieceAt(_).isEmpty))
    )

enum PawnBreakFollowUpKind:
  case ReleasedPassedPawn

sealed trait PlanResponseContinuationTrajectory:
  def triggerStep: LineReplayStep
  def replyStep: LineReplayStep
  def followUpStep: LineReplayStep
  def interveningSteps: List[LineReplayStep]
  def replyFrom: EvidenceSquare
  def replyTo: EvidenceSquare
  def followUpFrom: EvidenceSquare
  def followUpTo: EvidenceSquare
  def plyOffset: Int
  def involvedRoles: List[EvidencePieceRole]

final case class PawnBreakFollowUpTrajectory private (
    breakStep: LineReplayStep,
    replyStep: LineReplayStep,
    followUpStep: LineReplayStep,
    interveningSteps: List[LineReplayStep],
    kind: PawnBreakFollowUpKind,
    color: Color,
    replyFrom: EvidenceSquare,
    replyTo: EvidenceSquare,
    followUpFrom: EvidenceSquare,
    followUpTo: EvidenceSquare,
    releasedPassedPawn: EvidenceSquare,
    private[chessjudgment] val releaseWitness: ReplayRelationChangeWitness,
    plyOffset: Int
) extends PlanResponseContinuationTrajectory:
  def triggerStep: LineReplayStep = breakStep
  def involvedRoles: List[EvidencePieceRole] = List(EvidencePieceRole(Pawn.toString))

object PawnBreakFollowUpTrajectory:
  private[chessjudgment] def find(
      breakStep: LineReplayStep,
      replyStep: LineReplayStep,
      followUpStep: LineReplayStep,
      interveningSteps: List[LineReplayStep],
      replay: CanonicalLineReplay
  ): Option[PawnBreakFollowUpTrajectory] =
    val breakMove = EvidenceRef.normalizeMove(breakStep.moveUci)
    val replyMove = EvidenceRef.normalizeMove(replyStep.moveUci)
    val followUpMove = EvidenceRef.normalizeMove(followUpStep.moveUci)
    for
      _ <- Option.when(List(breakStep, replyStep, followUpStep).forall(_.ply > 0))(())
      breakFrom <- Square.fromKey(breakMove.take(2))
      breakTo <- Square.fromKey(breakMove.slice(2, 4))
      replyFrom <- Square.fromKey(replyMove.take(2))
      replyTo <- Square.fromKey(replyMove.slice(2, 4))
      followUpFrom <- Square.fromKey(followUpMove.take(2))
      followUpTo <- Square.fromKey(followUpMove.slice(2, 4))
      beforeBreakAnalysis <- replay.analysisBefore(breakStep)
      afterBreakAnalysis <- replay.analysisAfter(breakStep)
      beforeReplyAnalysis <- replay.analysisBefore(replyStep)
      afterReplyAnalysis <- replay.analysisAfter(replyStep)
      beforeFollowUpAnalysis <- replay.analysisBefore(followUpStep)
      afterFollowUpAnalysis <- replay.analysisAfter(followUpStep)
      beforeBreak = beforeBreakAnalysis.position
      afterBreak = afterBreakAnalysis.position
      beforeReply = beforeReplyAnalysis.position
      afterReply = afterReplyAnalysis.position
      beforeFollowUp = beforeFollowUpAnalysis.position
      afterFollowUp = afterFollowUpAnalysis.position
      breakPawn <- beforeBreak.board.pieceAt(breakFrom)
      if breakPawn.role == Pawn
      if afterBreak.board.pieceAt(breakTo).contains(breakPawn)
      if beforeReply.board.pieceAt(breakTo).contains(breakPawn)
      replyPawn <- beforeReply.board.pieceAt(replyFrom)
      if replyPawn.role == Pawn && replyPawn.color == !breakPawn.color
      if replyTo == breakTo
      if afterReply.board.pieceAt(replyTo).contains(replyPawn)
      followUpPawn <- beforeFollowUp.board.pieceAt(followUpFrom)
      if followUpPawn.role == Pawn && followUpPawn.color == breakPawn.color
      if afterFollowUp.board.pieceAt(followUpTo).exists(piece =>
        piece.color == breakPawn.color && (piece.role == Pawn || followUpMove.length == 5)
      )
      if interveningSteps.headOption.contains(replyStep)
      if followUpStep.ply - breakStep.ply == interveningSteps.size + 1
      releaseChange <- replay.transition(replyStep).toList
        .flatMap(_.relationDelta.established)
        .find(_.detail match
          case RelationWitnessDetail.PawnPassage(owner, pawn, blockers) =>
            owner == breakPawn.color &&
              pawn.key.equalsIgnoreCase(followUpFrom.key) &&
              blockers.isEmpty
          case _ => false
        )
      releaseWitness <- ReplayRelationChangeWitness.certify(replay, replyStep, releaseChange)
      remainingPassageAnalyses = interveningSteps.drop(1).flatMap(replay.analysisBefore)
      if remainingPassageAnalyses.size == interveningSteps.drop(1).size
      passageAnalyses = afterReplyAnalysis :: remainingPassageAnalyses ::: List(beforeFollowUpAnalysis)
      if passageAnalyses.forall(_.boardRelations.exists(relation =>
        relation.kind == releaseChange.kind &&
          relation.semanticId == releaseChange.semanticId &&
          relation.detail == releaseChange.detail
      ))
      if interveningSteps.drop(1).forall(step =>
        replay.before(step).flatMap(_.board.pieceAt(followUpFrom)).contains(followUpPawn)
      )
    yield PawnBreakFollowUpTrajectory(
      breakStep = breakStep,
      replyStep = replyStep,
      followUpStep = followUpStep,
      interveningSteps = interveningSteps,
      kind = PawnBreakFollowUpKind.ReleasedPassedPawn,
      color = breakPawn.color,
      replyFrom = EvidenceSquare(replyFrom.key),
      replyTo = EvidenceSquare(replyTo.key),
      followUpFrom = EvidenceSquare(followUpFrom.key),
      followUpTo = EvidenceSquare(followUpTo.key),
      releasedPassedPawn = EvidenceSquare(followUpFrom.key),
      releaseWitness = releaseWitness,
      plyOffset = followUpStep.ply - breakStep.ply
    )
final case class CaptureResponseFollowUpTrajectory private (
    triggerStep: LineReplayStep,
    replyStep: LineReplayStep,
    followUpStep: LineReplayStep,
    interveningSteps: List[LineReplayStep],
    triggerRole: EvidencePieceRole,
    responderRole: EvidencePieceRole,
    followUpRole: EvidencePieceRole,
    replyFrom: EvidenceSquare,
    replyTo: EvidenceSquare,
    followUpFrom: EvidenceSquare,
    followUpTo: EvidenceSquare,
    plyOffset: Int
) extends PlanResponseContinuationTrajectory:
  def involvedRoles: List[EvidencePieceRole] = List(triggerRole, responderRole, followUpRole).distinct

object CaptureResponseFollowUpTrajectory:
  private[chessjudgment] def find(
      triggerStep: LineReplayStep,
      replyStep: LineReplayStep,
      followUpStep: LineReplayStep,
      interveningSteps: List[LineReplayStep],
      replay: CanonicalLineReplay
  ): Option[CaptureResponseFollowUpTrajectory] =
    val triggerMove = EvidenceRef.normalizeMove(triggerStep.moveUci)
    val replyMove = EvidenceRef.normalizeMove(replyStep.moveUci)
    val followUpMove = EvidenceRef.normalizeMove(followUpStep.moveUci)
    for
      triggerFrom <- Square.fromKey(triggerMove.take(2))
      triggerTo <- Square.fromKey(triggerMove.slice(2, 4))
      replyFrom <- Square.fromKey(replyMove.take(2))
      replyTo <- Square.fromKey(replyMove.slice(2, 4))
      followUpFrom <- Square.fromKey(followUpMove.take(2))
      followUpTo <- Square.fromKey(followUpMove.slice(2, 4))
      beforeTrigger <- replay.before(triggerStep)
      afterTrigger <- replay.after(triggerStep)
      beforeReply <- replay.before(replyStep)
      afterReply <- replay.after(replyStep)
      beforeFollowUp <- replay.before(followUpStep)
      afterFollowUp <- replay.after(followUpStep)
      triggerActor <- beforeTrigger.board.pieceAt(triggerFrom)
      if afterTrigger.board.pieceAt(triggerTo).contains(triggerActor)
      if beforeReply.board.pieceAt(triggerTo).contains(triggerActor)
      responder <- beforeReply.board.pieceAt(replyFrom)
      if responder.color == !triggerActor.color && replyTo == triggerTo
      if afterReply.board.pieceAt(replyTo).contains(responder)
      follower <- beforeFollowUp.board.pieceAt(followUpFrom)
      if follower.color == triggerActor.color && followUpTo == replyTo
      if beforeFollowUp.board.pieceAt(replyTo).contains(responder)
      if afterFollowUp.board.pieceAt(followUpTo).exists(_.color == triggerActor.color)
      if interveningSteps == List(replyStep)
      if followUpStep.ply - triggerStep.ply == 2
    yield CaptureResponseFollowUpTrajectory(
      triggerStep = triggerStep,
      replyStep = replyStep,
      followUpStep = followUpStep,
      interveningSteps = interveningSteps,
      triggerRole = EvidencePieceRole(triggerActor.role.toString),
      responderRole = EvidencePieceRole(responder.role.toString),
      followUpRole = EvidencePieceRole(follower.role.toString),
      replyFrom = EvidenceSquare(replyFrom.key),
      replyTo = EvidenceSquare(replyTo.key),
      followUpFrom = EvidenceSquare(followUpFrom.key),
      followUpTo = EvidenceSquare(followUpTo.key),
      plyOffset = 2
    )

final case class CheckResponseFollowUpTrajectory private (
    triggerStep: LineReplayStep,
    replyStep: LineReplayStep,
    followUpStep: LineReplayStep,
    interveningSteps: List[LineReplayStep],
    triggerRole: EvidencePieceRole,
    responderRole: EvidencePieceRole,
    followUpRole: EvidencePieceRole,
    replyFrom: EvidenceSquare,
    replyTo: EvidenceSquare,
    followUpFrom: EvidenceSquare,
    followUpTo: EvidenceSquare,
    plyOffset: Int
) extends PlanResponseContinuationTrajectory:
  def involvedRoles: List[EvidencePieceRole] = List(triggerRole, responderRole, followUpRole).distinct

object CheckResponseFollowUpTrajectory:
  private[chessjudgment] def find(
      triggerStep: LineReplayStep,
      replyStep: LineReplayStep,
      followUpStep: LineReplayStep,
      interveningSteps: List[LineReplayStep],
      replay: CanonicalLineReplay
  ): Option[CheckResponseFollowUpTrajectory] =
    val triggerMove = EvidenceRef.normalizeMove(triggerStep.moveUci)
    val replyMove = EvidenceRef.normalizeMove(replyStep.moveUci)
    val followUpMove = EvidenceRef.normalizeMove(followUpStep.moveUci)
    for
      triggerFrom <- Square.fromKey(triggerMove.take(2))
      triggerTo <- Square.fromKey(triggerMove.slice(2, 4))
      replyFrom <- Square.fromKey(replyMove.take(2))
      replyTo <- Square.fromKey(replyMove.slice(2, 4))
      followUpFrom <- Square.fromKey(followUpMove.take(2))
      followUpTo <- Square.fromKey(followUpMove.slice(2, 4))
      beforeTrigger <- replay.before(triggerStep)
      afterTrigger <- replay.after(triggerStep)
      beforeReply <- replay.before(replyStep)
      afterReply <- replay.after(replyStep)
      beforeFollowUp <- replay.before(followUpStep)
      afterFollowUp <- replay.after(followUpStep)
      triggerActor <- beforeTrigger.board.pieceAt(triggerFrom)
      if afterTrigger.board.pieceAt(triggerTo).contains(triggerActor)
      if afterTrigger.check.yes && beforeReply.check.yes
      responder <- beforeReply.board.pieceAt(replyFrom)
      if responder.color == !triggerActor.color
      if afterReply.board.pieceAt(replyTo).contains(responder)
      if !afterReply.check.yes
      follower <- beforeFollowUp.board.pieceAt(followUpFrom)
      if follower.color == triggerActor.color
      if afterFollowUp.board.pieceAt(followUpTo).exists(_.color == triggerActor.color)
      if interveningSteps == List(replyStep)
      if followUpStep.ply - triggerStep.ply == 2
    yield CheckResponseFollowUpTrajectory(
      triggerStep = triggerStep,
      replyStep = replyStep,
      followUpStep = followUpStep,
      interveningSteps = interveningSteps,
      triggerRole = EvidencePieceRole(triggerActor.role.toString),
      responderRole = EvidencePieceRole(responder.role.toString),
      followUpRole = EvidencePieceRole(follower.role.toString),
      replyFrom = EvidenceSquare(replyFrom.key),
      replyTo = EvidenceSquare(replyTo.key),
      followUpFrom = EvidenceSquare(followUpFrom.key),
      followUpTo = EvidenceSquare(followUpTo.key),
      plyOffset = 2
    )

  /** Root-owned check-response seed: the checking actor itself must survive
    * the forced reply and perform the follow-up. Cross-piece continuations can
    * still be useful plan diagnostics, but need a separate reply-enabled
    * geometry proof before they can own a public causal episode.
    */
  private[chessjudgment] def findRootActorContinuation(
      triggerStep: LineReplayStep,
      replyStep: LineReplayStep,
      followUpStep: LineReplayStep,
      interveningSteps: List[LineReplayStep],
      replay: CanonicalLineReplay
  ): Option[CheckResponseFollowUpTrajectory] =
    find(triggerStep, replyStep, followUpStep, interveningSteps, replay)
      .filter(rootActorContinues(_, replay))

  private def rootActorContinues(
      trajectory: CheckResponseFollowUpTrajectory,
      replay: CanonicalLineReplay
  ): Boolean =
    (for
      triggerTo <- Square.fromKey(EvidenceRef.normalizeMove(trajectory.triggerStep.moveUci).slice(2, 4))
      followUpFrom <- Square.fromKey(EvidenceRef.normalizeMove(trajectory.followUpStep.moveUci).take(2))
      followUpTo <- Square.fromKey(EvidenceRef.normalizeMove(trajectory.followUpStep.moveUci).slice(2, 4))
      afterTrigger <- replay.after(trajectory.triggerStep)
      beforeFollowUp <- replay.before(trajectory.followUpStep)
      afterFollowUp <- replay.after(trajectory.followUpStep)
      triggerActor <- afterTrigger.board.pieceAt(triggerTo)
      if followUpFrom == triggerTo
      if beforeFollowUp.board.pieceAt(followUpFrom).contains(triggerActor)
      if afterFollowUp.board.pieceAt(followUpTo).exists(piece =>
        piece.color == triggerActor.color &&
          (piece.role == triggerActor.role || trajectory.followUpStep.moveUci.length == 5)
      )
    yield true).getOrElse(false)

final case class ExchangeConversionTrajectory private (
    planRootStep: LineReplayStep,
    triggerStep: LineReplayStep,
    replyStep: LineReplayStep,
    followUpStep: LineReplayStep,
    interveningSteps: List[LineReplayStep],
    exchangeCapture: LineMaterialCapture,
    exchangeRecapture: LineMaterialCapture,
    checkPrefix: Option[CheckResponseFollowUpTrajectory],
    exchangeActorRoute: Option[LineObjectTrajectory],
    convertingPawnAtPhaseBoundary: EvidenceSquare,
    materialSummary: LineMaterialSummary
) extends PlanResponseContinuationTrajectory:
  private def moveSquares(step: LineReplayStep): (EvidenceSquare, EvidenceSquare) =
    val move = EvidenceRef.normalizeMove(step.moveUci)
    EvidenceSquare(move.take(2)) -> EvidenceSquare(move.slice(2, 4))
  def replyFrom: EvidenceSquare = moveSquares(replyStep)._1
  def replyTo: EvidenceSquare = moveSquares(replyStep)._2
  def followUpFrom: EvidenceSquare = moveSquares(followUpStep)._1
  def followUpTo: EvidenceSquare = moveSquares(followUpStep)._2
  def plyOffset: Int = followUpStep.ply - triggerStep.ply
  def involvedRoles: List[EvidencePieceRole] =
    List(exchangeCapture.attackerRole, EvidencePieceRole(Pawn.toString)).distinct

object ExchangeConversionTrajectory:
  private val MaterialRoles = List(Knight, Bishop, Rook, Queen)
  private[chessjudgment] def find(
      planRootStep: LineReplayStep,
      exchangeStep: LineReplayStep,
      replyStep: LineReplayStep,
      promotionStep: LineReplayStep,
      interveningSteps: List[LineReplayStep],
      planPrefix: List[LineReplayStep],
      materialSummary: LineMaterialSummary,
      replay: CanonicalLineReplay
  ): Option[ExchangeConversionTrajectory] =
    val exchangeOffset = exchangeStep.ply - planRootStep.ply
    val replyOffset = replyStep.ply - planRootStep.ply
    val rootIsExchange = exchangeStep == planRootStep
    val prefixProof =
      if rootIsExchange && planPrefix.isEmpty then Some(None -> None)
      else if exchangeOffset == 2 && planPrefix.size == 1 then
        for
          check <- CheckResponseFollowUpTrajectory
            .find(planRootStep, planPrefix.head, exchangeStep, planPrefix, replay)
          route <- LineObjectTrajectory
            .find(planRootStep, planPrefix :+ exchangeStep, maxPlyOffset = 2, replay)
            .filter(_.futureStep == exchangeStep)
        yield Some(check) -> Some(route)
      else None
    for
      (checkPrefix, actorRoute) <- prefixProof
      if materialSummary.materialWindowComplete
      pair <- materialSummary.lastPieceExchangePairAt(exchangeOffset, replyOffset)
      (exchangeCapture, exchangeRecapture) = pair
      if replyOffset == exchangeOffset + 1
      if interveningSteps.headOption.contains(replyStep)
      if promotionStep.ply - exchangeStep.ply == interveningSteps.size + 1
      if actualLastPieceExchange(exchangeStep, replyStep, exchangeCapture, exchangeRecapture, replay)
      phaseBoundaryPawn <- convertingPawn(
        replyStep,
        promotionStep,
        interveningSteps.drop(1),
        exchangeCapture.side,
        replay
      )
      chain =
        (if rootIsExchange then List(planRootStep)
         else planRootStep :: (planPrefix :+ exchangeStep)) ++
          interveningSteps :+ promotionStep
      if continuous(chain)
    yield ExchangeConversionTrajectory(
      planRootStep = planRootStep,
      triggerStep = exchangeStep,
      replyStep = replyStep,
      followUpStep = promotionStep,
      interveningSteps = interveningSteps,
      exchangeCapture = exchangeCapture,
      exchangeRecapture = exchangeRecapture,
      checkPrefix = checkPrefix,
      exchangeActorRoute = actorRoute,
      convertingPawnAtPhaseBoundary = EvidenceSquare(phaseBoundaryPawn.key),
      materialSummary = materialSummary
    )

  private def actualLastPieceExchange(
      exchangeStep: LineReplayStep,
      replyStep: LineReplayStep,
      exchangeCapture: LineMaterialCapture,
      exchangeRecapture: LineMaterialCapture,
      replay: CanonicalLineReplay
  ): Boolean =
    val exchangeMove = EvidenceRef.normalizeMove(exchangeStep.moveUci)
    val replyMove = EvidenceRef.normalizeMove(replyStep.moveUci)
    (for
      exchangeFrom <- Square.fromKey(exchangeMove.take(2))
      exchangeTo <- Square.fromKey(exchangeMove.slice(2, 4))
      replyFrom <- Square.fromKey(replyMove.take(2))
      replyTo <- Square.fromKey(replyMove.slice(2, 4))
      beforeExchange <- replay.before(exchangeStep)
      afterExchange <- replay.after(exchangeStep)
      beforeReply <- replay.before(replyStep)
      afterReply <- replay.after(replyStep)
      actor <- beforeExchange.board.pieceAt(exchangeFrom)
      peer <- beforeExchange.board.pieceAt(exchangeTo)
      responder <- beforeReply.board.pieceAt(replyFrom)
    yield
      actor.color == exchangeCapture.side &&
        actor.role == peer.role &&
        !Set(Pawn, King)(actor.role) &&
        peer.color == !actor.color &&
        afterExchange.board.pieceAt(exchangeTo).contains(actor) &&
        PrincipalVariationEvidence.sameBoardState(exchangeStep.fenAfter, replyStep.fenBefore) &&
        replyTo == exchangeTo &&
        responder.color == peer.color &&
        afterReply.board.pieceAt(replyTo).contains(responder) &&
        nonPawnMaterialCount(beforeExchange.board) == 2 &&
        beforeExchange.board.byPiece(actor.color, actor.role).count == 1 &&
        beforeExchange.board.byPiece(peer.color, peer.role).count == 1 &&
        nonPawnMaterialCount(afterReply.board) == 0 &&
        captureMatches(exchangeCapture, exchangeStep, actor, peer, exchangeTo) &&
        captureMatches(exchangeRecapture, replyStep, responder, actor, replyTo)
    ).contains(true)

  private def convertingPawn(
      replyStep: LineReplayStep,
      promotionStep: LineReplayStep,
      betweenReplyAndPromotion: List[LineReplayStep],
      color: Color,
      replay: CanonicalLineReplay
  ): Option[Square] =
    val promotionMove = EvidenceRef.normalizeMove(promotionStep.moveUci)
    for
      promotionFrom <- Square.fromKey(promotionMove.take(2))
      promotionTo <- Square.fromKey(promotionMove.slice(2, 4))
      if promotionMove.length == 5
      beforePromotion <- replay.before(promotionStep)
      afterPromotion <- replay.after(promotionStep)
      if beforePromotion.board.pieceAt(promotionFrom).contains(Piece(color, Pawn))
      promoted <- afterPromotion.board.pieceAt(promotionTo)
      if promoted.color == color && promoted.role != Pawn && promoted.role != King
      boundarySquare <- betweenReplyAndPromotion.reverse.foldLeft(Option(promotionFrom)) {
        case (None, _) => None
        case (Some(current), step) =>
          val move = EvidenceRef.normalizeMove(step.moveUci)
          for
            from <- Square.fromKey(move.take(2))
            to <- Square.fromKey(move.slice(2, 4))
            before <- replay.before(step)
            after <- replay.after(step)
            previous <-
              if to == current &&
                  before.board.pieceAt(from).contains(Piece(color, Pawn)) &&
                  after.board.pieceAt(to).contains(Piece(color, Pawn))
              then Some(from)
              else if
                before.board.pieceAt(current).contains(Piece(color, Pawn)) &&
                  after.board.pieceAt(current).contains(Piece(color, Pawn))
              then Some(current)
              else None
          yield previous
      }
      boundary <- replay.after(replyStep)
      if boundary.board.pieceAt(boundarySquare).contains(Piece(color, Pawn))
    yield boundarySquare

  private def captureMatches(
      capture: LineMaterialCapture,
      step: LineReplayStep,
      attacker: Piece,
      captured: Piece,
      square: Square
  ): Boolean =
    EvidenceRef.sameMove(capture.moveUci, step.moveUci) &&
      capture.side == attacker.color &&
      capture.attackerRole.name.equalsIgnoreCase(attacker.role.toString) &&
      capture.capturedRole.name.equalsIgnoreCase(captured.role.toString) &&
      capture.square.key.equalsIgnoreCase(square.key)

  private def nonPawnMaterialCount(board: Board): Int =
    List(Color.White, Color.Black).map(color => MaterialRoles.map(role => board.byPiece(color, role).count).sum).sum

  private def continuous(steps: List[LineReplayStep]): Boolean =
    steps.zip(steps.drop(1)).forall((left, right) =>
      PrincipalVariationEvidence.sameBoardState(left.fenAfter, right.fenBefore)
    )

enum LineEventKind:
  case Capture
  case Recapture
  case DefenderMove
  case Castling
  case Check
  case Mate
  case Tempo
  case Stalemate
  case Promotion
  case ForcedTheme

final case class LineMoveEvent(
    kind: LineEventKind,
    moveUci: String,
    plyOffset: Int,
    side: Option[Color] = None,
    pieceRole: Option[EvidencePieceRole] = None,
    targetRole: Option[EvidencePieceRole] = None,
    square: Option[EvidenceSquare] = None
)

enum LineConsequenceKind:
  case ForcedTheme
  case ImmediateReplyCheck
  case Mate
  case DrawResource
  case MaterialGain
  case MaterialLoss
  case RecaptureSequence
  case RecoveryWindow
  case Sacrifice
  case Promotion
  case PromotionRace

object LineConsequenceKind:


  def tacticalDriver(kind: LineConsequenceKind): Boolean =
    kind match
      case LineConsequenceKind.MaterialGain | LineConsequenceKind.MaterialLoss |
          LineConsequenceKind.RecaptureSequence | LineConsequenceKind.RecoveryWindow |
          LineConsequenceKind.ImmediateReplyCheck | LineConsequenceKind.Mate |
          LineConsequenceKind.DrawResource | LineConsequenceKind.Promotion | LineConsequenceKind.PromotionRace =>
        true
      case LineConsequenceKind.ForcedTheme | LineConsequenceKind.Sacrifice =>
        false

enum LineMaterialOutcomeSignal:
  case MoverCapture
  case OpponentCapture
  case PromotionGain
  case PromotionLoss
  case UnrecoveredPawnGain
  case UnrecoveredPawnLoss
  case RecoveryWindow

final case class LineConsequence(
    kind: LineConsequenceKind,
    lineMoves: List[String],
    proofSignal: Boolean,
    eventMove: Option[String] = None,
    rootMove: Option[String] = None,
    rootSide: Option[Color] = None,
    beneficiary: Option[Color] = None,
    sacrificeOccurrence: Option[LineSacrificeOccurrence] = None,
    materialOutcome: Option[RootOwnedMaterialOutcome] = None
):
  def rootMoveMatched(rootMove: String): Boolean =
    this.rootMove.exists(move => EvidenceRef.sameMove(move, rootMove))

  /** Exact stationary projection. Cardinality is zero or one because every
    * sacrifice consequence owns one occurrence.
    */
  def stationarySacrificeCaptures: List[LineMaterialCapture] =
    sacrificeOccurrence.filter(_.stationary).map(_.acceptance).toList

final case class LineConsequenceProfile(
    proofSignalKinds: List[LineConsequenceKind],
    hasConcreteProofSignal: Boolean,
    hasConversionConsequence: Boolean,
    hasMaterialResult: Boolean,
    hasRecaptureRecovery: Boolean,
    hasSacrifice: Boolean,
    hasPromotionRace: Boolean,
    hasMate: Boolean,
    hasDrawResource: Boolean
)
final case class LineMaterialOutcomeProfile(
    gainSignals: Set[LineMaterialOutcomeSignal],
    lossSignals: Set[LineMaterialOutcomeSignal]
):
  def merge(other: LineMaterialOutcomeProfile): LineMaterialOutcomeProfile =
    LineMaterialOutcomeProfile(
      gainSignals = gainSignals ++ other.gainSignals,
      lossSignals = lossSignals ++ other.lossSignals
    )



object LineMaterialOutcomeProfile:
  val empty: LineMaterialOutcomeProfile =
    LineMaterialOutcomeProfile(Set.empty, Set.empty)

final case class LineMaterialCapture(
    moveUci: String,
    plyOffset: Int,
    side: Color,
    attackerRole: EvidencePieceRole,
    capturedRole: EvidencePieceRole,
    square: EvidenceSquare,
    valueCp: Int,
    recapture: Boolean
)

/** One exact replay occurrence at which a piece was placed on the square on
  * which it was later accepted. `None` on [[LineSacrificeOccurrence.offer]]
  * means that the accepted piece already occupied the square at the root.
  */
final case class LineSacrificeOffer(
    moveUci: String,
    plyOffset: Int
):
  require(moveUci.nonEmpty, "a sacrifice offer needs an exact move")
  require(plyOffset >= 0, "a sacrifice offer needs a root-relative ply offset")

/** Exact identity of one accepted material offer in the canonical replay.
  * The acceptance capture is never shared or inferred across occurrences.
  */
final case class LineSacrificeOccurrence private (
    offer: Option[LineSacrificeOffer],
    acceptance: LineMaterialCapture
):
  require(acceptance.plyOffset >= 0, "a sacrifice acceptance needs a root-relative ply offset")
  require(
    offer.forall(_.plyOffset < acceptance.plyOffset),
    "a moved sacrifice offer must precede its acceptance"
  )

  def target: EvidenceSquare = acceptance.square
  def stationary: Boolean = offer.isEmpty

object LineSacrificeOccurrence:
  private final case class TrackedOffer(
      piece: Piece,
      offer: Option[LineSacrificeOffer]
  )

  private[chessjudgment] def acceptanceKey(
      occurrence: LineSacrificeOccurrence
  ): (Int, String) =
    LineMaterialSummary.captureOccurrenceKey(occurrence.acceptance)

  /** Builds an occurrence by following the occupant of the acceptance square
    * through the admitted replay. This is an occupancy transition, not a
    * nearest/first candidate choice: every move touching the square replaces
    * the tracked state in replay order.
    */
  private[chessjudgment] def fromCanonicalReplay(
      replay: CanonicalLineReplay,
      offeredSide: Color,
      acceptance: LineMaterialCapture
  ): Option[LineSacrificeOccurrence] =
    for
      square <- Square.fromKey(acceptance.square.key)
      acceptanceReplay <- replay.replaySteps.lift(acceptance.plyOffset)
      acceptanceStep <- replay.legalSteps.lift(acceptance.plyOffset)
      if EvidenceRef.sameMove(acceptanceReplay.moveUci, acceptance.moveUci)
      if EvidenceRef.sameMove(acceptanceStep.uci, acceptance.moveUci)
      if acceptanceStep.move.dest == square && acceptanceStep.move.captures
      if acceptance.side == acceptanceStep.move.piece.color
      if acceptanceStep.move.piece.role.toString.equalsIgnoreCase(acceptance.attackerRole.name)
      if acceptance.side != offeredSide
      initialPosition <- replay.legalSteps.headOption.map(_.before)
      initial = initialPosition.board.pieceAt(square).map(piece => TrackedOffer(piece, None))
      tracked = replay.legalSteps
        .zip(replay.replaySteps)
        .take(acceptance.plyOffset)
        .zipWithIndex
        .foldLeft(initial) { case (current, ((legal, declared), plyOffset)) =>
          if legal.move.dest == square then
            Some(TrackedOffer(
              legal.move.piece,
              Some(LineSacrificeOffer(EvidenceRef.normalizeMove(declared.moveUci), plyOffset))
            ))
          else if legal.move.orig == square then None
          else current.filter(tracked => legal.after.board.pieceAt(square).contains(tracked.piece))
        }
      offered <- tracked
      if offered.piece.color == offeredSide
      if offered.piece.role.toString.equalsIgnoreCase(acceptance.capturedRole.name)
      if acceptanceStep.before.board.pieceAt(square).contains(offered.piece)
      if acceptanceStep.capturedRole.contains(offered.piece.role)
      if acceptanceStep.after.board.pieceAt(square).exists(_.color == acceptance.side)
      if offered.offer.forall(offer =>
        EvidenceRef.normalizeMove(offer.moveUci).slice(2, 4).equalsIgnoreCase(square.key)
      )
    yield LineSacrificeOccurrence(offered.offer, LineMaterialSummary.normalizedCapture(acceptance))

/** Concrete capture salience is sentence-facing event identity, not the
  * magnitude of the material result that survives the exchange sequence.
  */
final case class RootOwnedMaterialEventSalience(
    moveUci: String,
    plyOffset: Int,
    capturedRole: EvidencePieceRole,
    square: EvidenceSquare,
    targetValueCp: Int
):
  private[judgment] def stableKey: String =
    List(
      EvidenceRef.normalizeMove(moveUci),
      plyOffset.toString,
      capturedRole.name.toLowerCase,
      square.key.toLowerCase,
      targetValueCp.toString
    ).mkString(":")
  require(moveUci.nonEmpty, "a material event needs an exact move")
  require(plyOffset >= 0, "a material event needs a root-relative ply offset")
  require(targetValueCp > 0, "a material event target needs positive value")

  private[judgment] def matches(capture: LineMaterialCapture): Boolean =
    EvidenceRef.sameMove(moveUci, capture.moveUci) &&
      plyOffset == capture.plyOffset &&
      capturedRole == capture.capturedRole &&
      square == capture.square &&
      targetValueCp == capture.valueCp

object RootOwnedMaterialEventSalience:
  private[chessjudgment] def from(capture: LineMaterialCapture): Option[RootOwnedMaterialEventSalience] =
    Option.when(capture.valueCp > 0)(
      RootOwnedMaterialEventSalience(
        moveUci = EvidenceRef.normalizeMove(capture.moveUci),
        plyOffset = capture.plyOffset,
        capturedRole = capture.capturedRole,
        square = capture.square,
        targetValueCp = capture.valueCp
      )
    )

/** Exact positive material consequence retained for this one root-owned
  * event. The amount is beneficiary-normalized and excludes later gains that
  * never belonged to the event's lasting floor.
  */
final case class RootOwnedMaterialOutcome(
    event: RootOwnedMaterialEventSalience,
    beneficiary: Color,
    durableNetCp: Int
):
  require(durableNetCp > 0, "a durable material outcome needs positive beneficiary value")

final case class LineMaterialSummary(
    sideToMove: Color,
    captures: List[LineMaterialCapture],
    netCaptureCpForMover: Int,
    maxGainCpForMover: Int,
    maxLossCpForMover: Int,
    hasRecaptureChain: Boolean,
    hasRecoveryWindow: Boolean,
    promotionGainCpForMover: Int,
    materialWindowComplete: Boolean
):

  private lazy val exactCaptureInventory: Option[List[LineMaterialCapture]] =
    LineMaterialSummary.exactCaptureInventory(captures)

  private[chessjudgment] def exactCaptures: Option[List[LineMaterialCapture]] =
    exactCaptureInventory

  private[chessjudgment] def exactCaptureAt(
      plyOffset: Int,
      moveUci: String
  ): Option[LineMaterialCapture] =
    exactCaptureInventory.flatMap { inventory =>
      inventory.filter(capture =>
        capture.plyOffset == plyOffset && EvidenceRef.sameMove(capture.moveUci, moveUci)
      ) match
        case capture :: Nil => Some(capture)
        case _              => None
    }

  def capturesByMover: List[LineMaterialCapture] =
    captures.filter(_.side == sideToMove)

  def capturesByOpponent: List[LineMaterialCapture] =
    captures.filter(_.side != sideToMove)

  def nonPawnCapturesByMover: List[LineMaterialCapture] =
    capturesByMover.filter(capture => proofSignalCapturedRole(capture.capturedRole))

  def nonPawnCapturesByOpponent: List[LineMaterialCapture] =
    capturesByOpponent.filter(capture => proofSignalCapturedRole(capture.capturedRole))

  def pawnCapturesByMover: List[LineMaterialCapture] =
    capturesByMover.filter(capture => pawnCapturedRole(capture.capturedRole))

  def pawnCapturesByOpponent: List[LineMaterialCapture] =
    capturesByOpponent.filter(capture => pawnCapturedRole(capture.capturedRole))

  def hasPromotionGainForMover: Boolean =
    promotionGainCpForMover > 0

  def hasPromotionLossForMover: Boolean =
    promotionGainCpForMover < 0

  def hasResolvedMaterialSequence: Boolean =
    materialWindowComplete && (hasRecaptureChain || hasRecoveryWindow)

  private[chessjudgment] def durableRecoveryCaptureForMover: Option[LineMaterialCapture] =
    val orderedCaptures = captures.sortBy(_.plyOffset)
    val runningBalances = orderedCaptures
      .scanLeft(0)((balance, capture) =>
        balance + (if capture.side == sideToMove then capture.valueCp else -capture.valueCp)
      )
      .tail
    Option
      .when(
        materialWindowComplete &&
          hasRecoveryWindow &&
          promotionGainCpForMover == 0 &&
          runningBalances.lastOption.getOrElse(0) == netCaptureCpForMover
      )(
        orderedCaptures.zip(runningBalances).zipWithIndex.collectFirst {
          case ((capture, balance), index)
              if capture.side == sideToMove &&
                runningBalances.take(index).exists(_ < 0) &&
                balance >= 0 &&
                runningBalances.drop(index).forall(_ >= 0) =>
            capture
        }
      )
      .flatten

  def hasProofSignalMaterialGain: Boolean =
    materialWindowComplete && netCaptureCpForMover > 0 && (nonPawnCapturesByMover.nonEmpty || hasPromotionGainForMover)

  def hasProofSignalMaterialLoss: Boolean =
    materialWindowComplete && netCaptureCpForMover < 0 && (nonPawnCapturesByOpponent.nonEmpty || hasPromotionLossForMover)

  def hasUnrecoveredPawnGainForMover: Boolean =
    materialWindowComplete && netCaptureCpForMover > 0 && pawnCapturesByMover.nonEmpty

  def hasUnrecoveredPawnLossForMover: Boolean =
    materialWindowComplete && netCaptureCpForMover < 0 && pawnCapturesByOpponent.nonEmpty

  def hasProofSignalMaterialEvent: Boolean =
    hasProofSignalMaterialGain ||
      hasProofSignalMaterialLoss ||
      hasUnrecoveredPawnGainForMover ||
      hasUnrecoveredPawnLossForMover ||
      hasResolvedMaterialSequence

  private[chessjudgment] def sacrificeResponsesFor(
      capture: LineMaterialCapture
  ): List[LineMaterialCapture] =
    exactCaptureInventory.toList
      .flatten
      .filter(response => LineMaterialSummary.materialSacrificePair(capture, response))

  def lastPieceExchangePairAt(
      exchangePlyOffset: Int,
      recapturePlyOffset: Int
  ): Option[(LineMaterialCapture, LineMaterialCapture)] =
    for
      exchange <- captures.find(_.plyOffset == exchangePlyOffset)
      recapture <- captures.find(_.plyOffset == recapturePlyOffset)
      if recapturePlyOffset == exchangePlyOffset + 1
      if exchange.side == sideToMove
      if exchange.attackerRole == exchange.capturedRole
      if !Set("pawn", "king")(exchange.attackerRole.name.trim.toLowerCase)
      if recapture.recapture
      if recapture.side != exchange.side
      if recapture.square == exchange.square
      if recapture.capturedRole == exchange.attackerRole
    yield exchange -> recapture

  private def proofSignalCapturedRole(role: EvidencePieceRole): Boolean =
    val normalized = role.name.trim.toLowerCase
    normalized.nonEmpty && normalized != "pawn" && normalized != "king"

  private def pawnCapturedRole(role: EvidencePieceRole): Boolean =
    role.name.trim.equalsIgnoreCase("pawn")

object LineMaterialSummary:
  private[chessjudgment] def normalizedCapture(capture: LineMaterialCapture): LineMaterialCapture =
    capture.copy(moveUci = EvidenceRef.normalizeMove(capture.moveUci))

  private[chessjudgment] def captureOccurrenceKey(
      capture: LineMaterialCapture
  ): (Int, String) =
    capture.plyOffset -> EvidenceRef.normalizeMove(capture.moveUci)

  /** Exact duplicate rows collapse to one occurrence. Two different payloads
    * claiming the same replay ply and move invalidate the inventory.
    */
  private[chessjudgment] def exactCaptureInventory(
      captures: List[LineMaterialCapture]
  ): Option[List[LineMaterialCapture]] =
    val groups = captures.map(normalizedCapture).groupBy(captureOccurrenceKey)
    val resolved = groups.toList.map { case (_, sameOccurrence) =>
      sameOccurrence.distinct match
        case capture :: Nil => Some(capture)
        case _              => None
    }
    Option.when(resolved.forall(_.nonEmpty))(
      resolved.flatten.sortBy(capture => (
        capture.plyOffset,
        EvidenceRef.normalizeMove(capture.moveUci),
        capture.square.key.toLowerCase
      ))
    )

  private[chessjudgment] def materialSacrificePair(
      offer: LineMaterialCapture,
      response: LineMaterialCapture
  ): Boolean =
    !offer.recapture &&
      response.recapture &&
      response.plyOffset > offer.plyOffset &&
      response.square == offer.square &&
      response.side != offer.side &&
      response.capturedRole == offer.attackerRole &&
      materialValue(offer.attackerRole) > materialValue(offer.capturedRole)

  private def materialValue(role: EvidencePieceRole): Int =
    role.name.toLowerCase match
      case "queen"             => 9
      case "rook"              => 5
      case "bishop" | "knight" => 3
      case "pawn"              => 1
      case "king"              => 100
      case _                    => 0

enum RootCausalLinkKind:
  case ImmediateRootAction
  case RootActorContinuation
  case ContinuousLineAccess
  case ForcedCaptureResponse
  case ForcedCheckResponse
  case RootActorCaptured
  case MaterialActorContinuation
  case MaterialCaptureResponse

final case class RootCausalActor(
    moveUci: String,
    role: EvidencePieceRole,
    color: Color,
    from: EvidenceSquare,
    to: EvidenceSquare
)

object RootCausalActor:
  private[chessjudgment] def fromLegalStep(
      rootMoveUci: String,
      step: LegalReplayStep
  ): Option[RootCausalActor] =
    val normalizedRoot = EvidenceRef.normalizeMove(rootMoveUci)
    Option.when(
      EvidenceRef.sameMove(step.uci, normalizedRoot) &&
        step.before.color == step.move.piece.color
    )(
      RootCausalActor(
        moveUci = normalizedRoot,
        role = EvidencePieceRole(step.move.piece.role.name),
        color = step.move.piece.color,
        from = EvidenceSquare(step.move.orig.key),
        to = EvidenceSquare(step.move.dest.key)
      )
    )

  private[chessjudgment] def fromNode(node: PlanCausalEventNode): Option[RootCausalActor] =
    node.certifiedLegalStep.flatMap(fromLegalStep(node.moveUci, _))

  private[chessjudgment] def fromPlanEvent(
      event: PlanCausalEventEvidence
  ): Option[RootCausalActor] =
    Option.when(event.rootTransitionIsCertified)(event.causalEpisode.root)
      .flatMap(fromNode)

  private[chessjudgment] def fromLineFact(
      line: LineFactEvidence,
      rootMoveUci: String
  ): Option[RootCausalActor] =
    line.certifiedRootActor(EvidenceRef.normalizeMove(rootMoveUci))

final case class RootCausalLink(
    kind: RootCausalLinkKind,
    causeMove: String,
    effectMove: String,
    anchor: EvidenceSquare
)

final case class RootOwnedCausalEpisode private (
    line: LineNodeRef,
    actor: RootCausalActor,
    target: EvidenceSquare,
    links: List[RootCausalLink],
    consequence: LineConsequence,
    eventPlyOffset: Int,
    chainMoves: List[String]
):
  require(links.nonEmpty, "a root-owned causal episode needs a verified causal link")
  require(chainMoves.nonEmpty, "a root-owned causal episode needs a replay chain")

  def forcingTacticalResource(lineFacts: LineFactEvidence): Boolean =
    RootOwnedEffectPolicy.admitsLineEpisode(lineFacts, this) &&
      consequence.proofSignal &&
      consequence.beneficiary.contains(actor.color) &&
      LineConsequenceKind.tacticalDriver(consequence.kind) &&
      consequence.kind != LineConsequenceKind.MaterialLoss

private[chessjudgment] final case class LineReplayDerivedFacts(
    rootActorSurvivesReply: Option[Boolean],
    rootActorSurvivesLine: Option[Boolean],
    rootActor: Option[RootCausalActor],
    rootOwnedEpisodes: List[RootOwnedCausalEpisode],
    replayCertified: Boolean
)

private[chessjudgment] object LineReplayDerivedFacts:
  val empty: LineReplayDerivedFacts =
    LineReplayDerivedFacts(None, None, None, Nil, replayCertified = false)

  def from(replay: CanonicalLineReplay): LineReplayDerivedFacts =
    LineReplayDerivedFacts(
      rootActorSurvivesReply = rootActorSurvivesThrough(replay, 2),
      rootActorSurvivesLine = rootActorSurvivesThrough(replay, replay.replaySteps.size),
      rootActor = replay.legalSteps.headOption.map { step =>
        RootCausalActor(
          moveUci = step.uci,
          role = EvidencePieceRole(step.move.piece.role.toString),
          color = step.move.piece.color,
          from = EvidenceSquare(step.move.orig.key),
          to = EvidenceSquare(step.move.dest.key)
        )
      },
      rootOwnedEpisodes = Nil,
      replayCertified = true
    )

  def withRootOwnedEpisodes(
      derived: LineReplayDerivedFacts,
      episodes: List[RootOwnedCausalEpisode]
  ): LineReplayDerivedFacts =
    derived.copy(rootOwnedEpisodes = episodes.distinct)

  private def rootActorSurvivesThrough(
      replay: CanonicalLineReplay,
      plies: Int
  ): Option[Boolean] =
    replay.legalSteps.headOption.map { rootStep =>
      val actorColor = rootStep.move.piece.color
      replay.legalSteps.take(plies.max(1)).foldLeft(Option(rootStep.move.orig)) { (square, step) =>
        square.flatMap { current =>
          val next = if step.move.orig == current then step.move.dest else current
          Option.when(step.after.board.pieceAt(next).exists(_.color == actorColor))(next)
        }
      }.nonEmpty
    }

final case class LineFactEvidence private[chessjudgment] (
    line: LineNodeRef,
    private val forcedTheme: Option[ForcedLineThemeEvidence] = None,
    private val material: Option[LineMaterialSummary] = None,
    private val replay: List[LineReplayStep] = Nil,
    private val events: List[LineMoveEvent] = Nil,
    private val consequences: List[LineConsequence] = Nil,
    private val replayDerived: LineReplayDerivedFacts = LineReplayDerivedFacts.empty,
    private[chessjudgment] val canonicalReplay: Option[CanonicalLineReplay] = None,
    private[chessjudgment] val canonicalPredecessorReplay: Option[CanonicalLineReplay] = None
) extends EvidencePayload:
  def rootMove: Option[String] =
    replay.headOption.map(_.moveUci)
  def reply: Option[String] =
    replay.lift(1).map(_.moveUci)
  def continuation: List[String] =
    replay.drop(2).map(_.moveUci)
  def lineReplaySteps: List[LineReplayStep] =
    replay
  def lineReplayMoves: List[String] =
    replay.map(_.moveUci)
  def lineReplayContinuationMoves: List[String] =
    lineReplayMoves.drop(1)
  def rootActorSurvivesReply: Option[Boolean] =
    replayDerived.rootActorSurvivesReply
  def rootActorSurvivesLine: Option[Boolean] =
    replayDerived.rootActorSurvivesLine
  private[chessjudgment] def certifiedRootActor(rootMoveUci: String): Option[RootCausalActor] =
    replayDerived.rootActor.filter(actor => EvidenceRef.sameMove(actor.moveUci, rootMoveUci))
  private[chessjudgment] def certifiedReplay: Option[CanonicalLineReplay] =
    canonicalReplay.filter(value => replayDerived.replayCertified && value.matches(replay))
  private[chessjudgment] def replayIsCertified: Boolean = certifiedReplay.nonEmpty
  def materialCaptures: List[LineMaterialCapture] =
    material.flatMap(_.exactCaptures).getOrElse(Nil)
  private lazy val materialCapturesByOccurrence: Map[(Int, String), LineMaterialCapture] =
    materialCaptures.map(capture => LineMaterialSummary.captureOccurrenceKey(capture) -> capture).toMap
  private[chessjudgment] def uniqueMaterialCaptureAt(
      plyOffset: Int,
      moveUci: String
  ): Option[LineMaterialCapture] =
    materialCapturesByOccurrence.get(plyOffset -> EvidenceRef.normalizeMove(moveUci))
  private[chessjudgment] def uniqueMaterialCaptureFor(
      episode: RootOwnedCausalEpisode
  ): Option[LineMaterialCapture] =
    for
      replayStep <- replay.lift(episode.eventPlyOffset)
      chainMove <- episode.chainMoves.lastOption
      if EvidenceRef.sameMove(chainMove, replayStep.moveUci)
      capture <- uniqueMaterialCaptureAt(episode.eventPlyOffset, replayStep.moveUci)
    yield capture
  private[chessjudgment] def durableRecoveryCaptureForMover: Option[LineMaterialCapture] =
    material.flatMap(_.durableRecoveryCaptureForMover)
  def rootMaterialCapture(rootMoveUci: String): Option[LineMaterialCapture] =
    uniqueMaterialCaptureAt(0, rootMoveUci)
  def rootIsRecapture(rootMoveUci: String): Boolean =
    rootMaterialCapture(rootMoveUci).exists(_.recapture)
  def rootIsCaptureSacrifice(rootMoveUci: String): Boolean =
    rootMaterialCapture(rootMoveUci).nonEmpty &&
      sacrificeOccurrencesForRootMove(rootMoveUci).exists(_.offer.exists(offer =>
        offer.plyOffset == 0 && EvidenceRef.sameMove(offer.moveUci, rootMoveUci)
      ))
  def rootCaptureSacrificeResponses(rootMoveUci: String): List[LineMaterialCapture] =
    sacrificeOccurrencesForRootMove(rootMoveUci).collect {
      case occurrence
          if occurrence.offer.exists(offer =>
            offer.plyOffset == 0 && EvidenceRef.sameMove(offer.moveUci, rootMoveUci)
          ) =>
        occurrence.acceptance
    }
  def materialGainCapturesFor(side: Color): List[LineMaterialCapture] =
    val lastingGainMoves = consequences.collect {
      case consequence
          if Set(LineConsequenceKind.MaterialGain, LineConsequenceKind.MaterialLoss)(consequence.kind) &&
            consequence.beneficiary.contains(side) =>
        consequence.eventMove
    }.flatten
    materialCaptures.filter(capture =>
      capture.side == side && lastingGainMoves.exists(EvidenceRef.sameMove(_, capture.moveUci))
    ).distinct
  def opponentResourcePunishmentCapturesFor(side: Color): List[LineMaterialCapture] =
    val immediatePawnPunishments = materialCaptures.filter(capture =>
      capture.side == side &&
        capture.plyOffset == 1 &&
        !capture.recapture &&
        capture.capturedRole.name.equalsIgnoreCase("pawn")
    )
    (materialGainCapturesFor(side) ++ immediatePawnPunishments).distinct
  def materialSacrificeCapture(capture: LineMaterialCapture): Boolean =
    rootMove.exists(root =>
      sacrificeOccurrencesForRootMove(root).exists { occurrence =>
        occurrence.acceptance == LineMaterialSummary.normalizedCapture(capture) ||
          occurrence.offer.exists(offer =>
            offer.plyOffset == capture.plyOffset && EvidenceRef.sameMove(offer.moveUci, capture.moveUci)
          )
      }
    )
  def lineEvents: List[LineMoveEvent] =
    events
  def lineConsequences: List[LineConsequence] =
    consequences
  def rootOwnedCausalEpisodes(rootMoveUci: String): List[RootOwnedCausalEpisode] =
    val stored = replayDerived.rootOwnedEpisodes.filter(episode =>
      EvidenceRef.sameMove(episode.actor.moveUci, rootMoveUci)
    )
    if replayIsCertified then stored else Nil
  def rootOwnedCausalConsequences(rootMoveUci: String): List[LineConsequence] =
    rootOwnedCausalEpisodes(rootMoveUci).map(_.consequence).distinct
  def lineReplayCount: Int =
    replay.size

  def lineEventsOf(kind: LineEventKind): List[LineMoveEvent] =
    events.filter(_.kind == kind)
  def hasLineEvent(kind: LineEventKind): Boolean =
    lineEventsOf(kind).nonEmpty
  def eventsForRootMove(rootMoveUci: String): List[LineMoveEvent] =
    val normalizedRoot = EvidenceRef.normalizeMove(rootMoveUci)
    events.filter(event =>
      event.plyOffset == 0 && EvidenceRef.normalizeMove(event.moveUci) == normalizedRoot
    )
  def hasRootCaptureEvent(rootMoveUci: String): Boolean =
    eventsForRootMove(rootMoveUci).exists(event =>
      event.kind == LineEventKind.Capture || event.kind == LineEventKind.Recapture
    )
  def materialNetCaptureCpForMover: Option[Int] =
    material.map(_.netCaptureCpForMover)
  def hasMaterialRecaptureChain: Boolean =
    material.exists(_.hasRecaptureChain)
  def hasMaterialRecoveryWindow: Boolean =
    material.exists(_.hasRecoveryWindow)
  def hasCompleteMaterialWindow: Boolean =
    material.forall(_.materialWindowComplete)
  def hasProofSignalMaterialEvent: Boolean =
    material.exists(_.hasProofSignalMaterialEvent)
  def hasSacrificeMaterialEvent: Boolean =
    rootMove.exists(root => sacrificeOccurrencesForRootMove(root).nonEmpty)
  def proofSignalConsequences: List[LineConsequence] =
    consequences.filter(_.proofSignal)
  private[chessjudgment] def proofConsequenceCandidatesForRootMove(
      rootMoveUci: String
  ): List[LineConsequence] =
    (
      proofSignalConsequences ++
        immediateReplyCheckLiabilitiesForRootMove(rootMoveUci)
    ).distinct
  private[chessjudgment] def ownedLineConsequences(
      eventLine: LineNodeRef,
      rootMoveUci: String,
      expectedRootSide: Color,
      expectedBeneficiary: Color,
      sourceLabels: Set[String] = Set.empty
  ): List[LineConsequence] =
    if line != eventLine || !rootMove.exists(EvidenceRef.sameMove(_, rootMoveUci)) then Nil
    else
      rootOwnedCausalConsequences(rootMoveUci).filter { consequence =>
        val sourceOwnsKind = sourceLabels.isEmpty || sourceLabels.contains(consequence.kind.toString)
        sourceOwnsKind &&
          consequence.rootSide.contains(expectedRootSide) &&
          consequence.beneficiary.contains(expectedBeneficiary)
      }
  def consequencesForRootMove(rootMoveUci: String): List[LineConsequence] =
    proofSignalConsequences.filter(_.rootMoveMatched(rootMoveUci))
  def immediateReplyCheckLiabilitiesForRootMove(rootMoveUci: String): List[LineConsequence] =
    consequences.filter(consequence =>
        consequence.kind == LineConsequenceKind.ImmediateReplyCheck &&
        consequence.rootMoveMatched(rootMoveUci) &&
        consequence.rootSide.nonEmpty &&
        (consequence.lineMoves match
          case root :: replyMove :: _ =>
            EvidenceRef.sameMove(root, rootMoveUci) &&
              reply.exists(EvidenceRef.sameMove(_, replyMove))
          case _ =>
            false)
    )
  def sacrificeSquaresForRootMove(rootMoveUci: String): List[EvidenceSquare] =
    sacrificeOccurrencesForRootMove(rootMoveUci).map(_.target).distinct
  def sacrificeOccurrencesForRootMove(rootMoveUci: String): List[LineSacrificeOccurrence] =
    val grouped = consequencesForRootMove(rootMoveUci)
      .filter(_.kind == LineConsequenceKind.Sacrifice)
      .flatMap(_.sacrificeOccurrence)
      .groupBy(LineSacrificeOccurrence.acceptanceKey)
    val resolved = grouped.toList.map { case (_, sameAcceptance) =>
      sameAcceptance.distinct match
        case occurrence :: Nil => Some(occurrence)
        case _                 => None
    }
    Option
      .when(resolved.forall(_.nonEmpty))(
        resolved.flatten.sortBy(occurrence => (
          occurrence.acceptance.plyOffset,
          EvidenceRef.normalizeMove(occurrence.acceptance.moveUci),
          occurrence.acceptance.square.key.toLowerCase
        ))
      )
      .getOrElse(Nil)
  def principalSacrificeCostSequenceForRootMove(rootMoveUci: String): List[LineMaterialCapture] =
    sacrificeOccurrencesForRootMove(rootMoveUci).map(_.acceptance)
  def hasProofSignalConsequence: Boolean =
    proofSignalConsequences.nonEmpty
  def proofSignalConsequenceKinds: List[LineConsequenceKind] =
    proofSignalConsequences.map(_.kind)
  def hasProofSignalConsequence(kind: LineConsequenceKind): Boolean =
    proofSignalConsequenceKinds.contains(kind)
  def consequenceProfile: LineConsequenceProfile =
    val kinds = proofSignalConsequenceKinds
    LineConsequenceProfile(
      proofSignalKinds = kinds,
      hasConcreteProofSignal = kinds.nonEmpty,
      hasConversionConsequence = kinds.exists {
        case LineConsequenceKind.RecaptureSequence | LineConsequenceKind.RecoveryWindow |
            LineConsequenceKind.MaterialGain | LineConsequenceKind.MaterialLoss |
            LineConsequenceKind.Sacrifice =>
          true
        case _ =>
          false
      },
      hasMaterialResult = kinds.exists {
        case LineConsequenceKind.MaterialGain | LineConsequenceKind.MaterialLoss |
            LineConsequenceKind.Sacrifice | LineConsequenceKind.Promotion | LineConsequenceKind.PromotionRace =>
          true
        case _ =>
          false
      },
      hasRecaptureRecovery = kinds.exists(kind =>
        kind == LineConsequenceKind.RecaptureSequence || kind == LineConsequenceKind.RecoveryWindow
      ),
      hasSacrifice = kinds.contains(LineConsequenceKind.Sacrifice),
      hasPromotionRace = kinds.exists(kind =>
        kind == LineConsequenceKind.Promotion || kind == LineConsequenceKind.PromotionRace
      ),
      hasMate = kinds.contains(LineConsequenceKind.Mate),
      hasDrawResource = kinds.contains(LineConsequenceKind.DrawResource)
    )
  def hasConversionConsequence: Boolean =
    consequenceProfile.hasConversionConsequence
  def hasMaterialConsequence: Boolean =
    consequenceProfile.hasMaterialResult
  def hasRecaptureRecoveryConsequence: Boolean =
    consequenceProfile.hasRecaptureRecovery
  def semanticGroupingAnchors: List[EvidenceSemanticAnchor] =
    Option
      .when(hasLineEvent(LineEventKind.Castling))(
        EvidenceSemanticAnchor.of(EvidenceSemanticAnchorKind.LineEvent, LineEventKind.Castling.toString)
      )
      .toList ++
      proofSignalConsequenceKinds.map(kind =>
        EvidenceSemanticAnchor.of(EvidenceSemanticAnchorKind.LineConsequence, kind.toString)
      )

  def materialOutcomeProfile: LineMaterialOutcomeProfile =
    val consequenceGainSignals =
      consequences.collect {
        case LineConsequence(LineConsequenceKind.MaterialGain, _, true, _, _, _, _, _, _) =>
          LineMaterialOutcomeSignal.MoverCapture
        case LineConsequence(LineConsequenceKind.MaterialGain, _, false, _, _, _, _, _, _) =>
          LineMaterialOutcomeSignal.UnrecoveredPawnGain
        case LineConsequence(LineConsequenceKind.RecoveryWindow, _, true, _, _, _, _, _, _) =>
          LineMaterialOutcomeSignal.RecoveryWindow
      }.toSet
    val consequenceLossSignals =
      consequences.collect {
        case LineConsequence(LineConsequenceKind.MaterialLoss, _, true, _, _, _, _, _, _) =>
          LineMaterialOutcomeSignal.OpponentCapture
        case LineConsequence(LineConsequenceKind.MaterialLoss, _, false, _, _, _, _, _, _) =>
          LineMaterialOutcomeSignal.UnrecoveredPawnLoss
      }.toSet
    val materialGainSignals =
      material
        .map(summary =>
          Set(
            Option.when(summary.netCaptureCpForMover > 0 && summary.nonPawnCapturesByMover.nonEmpty)(LineMaterialOutcomeSignal.MoverCapture),
            Option.when(summary.netCaptureCpForMover > 0 && summary.hasPromotionGainForMover)(LineMaterialOutcomeSignal.PromotionGain),
            Option.when(summary.hasUnrecoveredPawnGainForMover)(LineMaterialOutcomeSignal.UnrecoveredPawnGain),
            Option.when(summary.hasRecoveryWindow)(LineMaterialOutcomeSignal.RecoveryWindow)
          ).flatten
        )
        .getOrElse(Set.empty)
    val materialLossSignals =
      material
        .map(summary =>
          Set(
            Option.when(summary.netCaptureCpForMover < 0 && summary.nonPawnCapturesByOpponent.nonEmpty)(LineMaterialOutcomeSignal.OpponentCapture),
            Option.when(summary.netCaptureCpForMover < 0 && summary.hasPromotionLossForMover)(LineMaterialOutcomeSignal.PromotionLoss),
            Option.when(summary.hasUnrecoveredPawnLossForMover)(LineMaterialOutcomeSignal.UnrecoveredPawnLoss)
          ).flatten
        )
        .getOrElse(Set.empty)
    LineMaterialOutcomeProfile(
      gainSignals = consequenceGainSignals ++ materialGainSignals,
      lossSignals = consequenceLossSignals ++ materialLossSignals
    )

object RootOwnedCausalEpisode:
  private final case class TrackedActor(square: Square, role: EvidencePieceRole)

  def from(line: LineFactEvidence, rootMoveUci: String): List[RootOwnedCausalEpisode] =
    line.certifiedReplay.toList
      .flatMap(from(line, rootMoveUci, _))

  private[chessjudgment] def from(
      line: LineFactEvidence,
      rootMoveUci: String,
      replay: CanonicalLineReplay
  ): List[RootOwnedCausalEpisode] =
    val steps = line.lineReplaySteps
    val normalizedRoot = EvidenceRef.normalizeMove(rootMoveUci)
    val rootContext =
      for
        rootStep <- steps.headOption
        actor <- line.certifiedRootActor(normalizedRoot).orElse(RootCausalActor.fromLineFact(line, normalizedRoot))
      yield rootStep -> actor

    rootContext.toList.flatMap { case (rootStep, actor) =>
      val candidates =
        (line.lineConsequences ++ line.immediateReplyCheckLiabilitiesForRootMove(rootMoveUci)).distinct
      candidates.flatMap { consequence =>
        eventPlyOffsets(line, consequence).flatMap { eventPlyOffset =>
          steps.lift(eventPlyOffset).toList.flatMap { eventStep =>
            for
              target <- consequenceTarget(line, consequence, eventStep, eventPlyOffset, replay)
              if actualConsequenceAt(line, consequence, eventStep, eventPlyOffset, actor.color, replay)
              links <- causalLinks(line, rootStep, actor, consequence, eventStep, eventPlyOffset, replay)
                .filter(_.nonEmpty)
            yield RootOwnedCausalEpisode(
              line = line.line,
              actor = actor,
              target = target,
              links = links,
              consequence = consequence,
              eventPlyOffset = eventPlyOffset,
              chainMoves = steps.take(eventPlyOffset + 1).map(_.moveUci)
            )
          }
        }
      }.distinct
    }

  private def eventPlyOffsets(
      line: LineFactEvidence,
      consequence: LineConsequence
  ): List[Int] =
    consequence.sacrificeOccurrence match
      case Some(occurrence) if consequence.kind == LineConsequenceKind.Sacrifice =>
        val acceptance = occurrence.acceptance
        line.lineReplaySteps.lift(acceptance.plyOffset).toList.collect {
          case step if EvidenceRef.sameMove(step.moveUci, acceptance.moveUci) => acceptance.plyOffset
        }
      case _ =>
        val moves = line.lineReplayMoves
        val explicitMove = consequence.eventMove.map(EvidenceRef.normalizeMove)
        val proofTailMove = consequence.lineMoves.lastOption.map(EvidenceRef.normalizeMove)
        val selectedMove = explicitMove.orElse(proofTailMove)
        val materialOffsets = selectedMove.toList.flatMap(move =>
          line.materialCaptures.collect {
            case capture if EvidenceRef.sameMove(capture.moveUci, move) => capture.plyOffset
          }
        )
        val replayOffsets = selectedMove.toList.flatMap(move =>
          moves.zipWithIndex.collect { case (candidate, index) if EvidenceRef.sameMove(candidate, move) => index }
        )
        (materialOffsets ++ replayOffsets).filter(index => index >= 0 && index < moves.size).distinct.sorted

  private def consequenceTarget(
      line: LineFactEvidence,
      consequence: LineConsequence,
      eventStep: LineReplayStep,
      eventPlyOffset: Int,
      replay: CanonicalLineReplay
  ): Option[EvidenceSquare] =
    val move = EvidenceRef.normalizeMove(eventStep.moveUci)
    consequence.kind match
      case LineConsequenceKind.MaterialGain | LineConsequenceKind.MaterialLoss |
          LineConsequenceKind.RecaptureSequence | LineConsequenceKind.RecoveryWindow =>
        line.uniqueMaterialCaptureAt(eventPlyOffset, move).map(_.square)
      case LineConsequenceKind.ImmediateReplyCheck =>
        verifiedKingTarget(eventStep, replay, _.check.yes)
      case LineConsequenceKind.Mate =>
        verifiedKingTarget(eventStep, replay, _.checkMate)
      case LineConsequenceKind.DrawResource =>
        verifiedKingTarget(eventStep, replay, _.staleMate)
      case LineConsequenceKind.Promotion | LineConsequenceKind.PromotionRace =>
        Option
          .when(move.length == 5)(EvidenceSquare(move.slice(2, 4)))
      case LineConsequenceKind.Sacrifice =>
        consequence.sacrificeOccurrence.map(_.target)
      case LineConsequenceKind.ForcedTheme =>
        None

  private def verifiedKingTarget(
      step: LineReplayStep,
      replay: CanonicalLineReplay,
      predicate: chess.Position => Boolean
  ): Option[EvidenceSquare] =
    replay.after(step)
      .filter(predicate)
      .flatMap(position => position.board.kingPosOf(position.color))
      .map(square => EvidenceSquare(square.key))

  private def actualConsequenceAt(
      line: LineFactEvidence,
      consequence: LineConsequence,
      eventStep: LineReplayStep,
      eventPlyOffset: Int,
      rootColor: Color,
      replay: CanonicalLineReplay
  ): Boolean =
    val move = EvidenceRef.normalizeMove(eventStep.moveUci)
    val mover =
      for
        square <- Square.fromKey(move.take(2))
        before <- replay.before(eventStep)
        piece <- before.board.pieceAt(square)
      yield piece
    val after = replay.after(eventStep)
    val capture = line.uniqueMaterialCaptureAt(eventPlyOffset, move)
    consequence.kind match
      case LineConsequenceKind.MaterialGain =>
        capture.exists(captured =>
          consequence.beneficiary.contains(captured.side) && captured.side == rootColor
        )
      case LineConsequenceKind.MaterialLoss =>
        capture.exists(captured =>
          consequence.beneficiary.contains(captured.side) && captured.side != rootColor
        )
      case LineConsequenceKind.RecaptureSequence =>
        capture.exists(actual =>
          actual.recapture &&
            consequence.eventMove.exists(EvidenceRef.sameMove(_, actual.moveUci)) &&
            consequence.rootSide.contains(rootColor) &&
            consequence.beneficiary.contains(actual.side)
        )
      case LineConsequenceKind.RecoveryWindow =>
        (line.durableRecoveryCaptureForMover, capture) match
          case (Some(expected), Some(actual)) =>
            expected == actual &&
              expected.side == rootColor &&
              consequence.eventMove.exists(EvidenceRef.sameMove(_, expected.moveUci)) &&
              consequence.rootSide.contains(rootColor) &&
              consequence.beneficiary.contains(rootColor)
          case _ =>
            false
      case LineConsequenceKind.ImmediateReplyCheck =>
        eventPlyOffset == 1 && mover.exists(_.color != rootColor) && after.exists(_.check.yes)
      case LineConsequenceKind.Mate =>
        mover.exists(piece => consequence.beneficiary.contains(piece.color)) && after.exists(_.checkMate)
      case LineConsequenceKind.DrawResource =>
        after.exists(_.staleMate)
      case LineConsequenceKind.Promotion | LineConsequenceKind.PromotionRace =>
        move.length == 5 && mover.exists(piece => consequence.beneficiary.contains(piece.color))
      case LineConsequenceKind.Sacrifice =>
        consequence.sacrificeOccurrence.exists(occurrence =>
          occurrence.acceptance.plyOffset == eventPlyOffset &&
            capture.contains(occurrence.acceptance) &&
            consequence.rootSide.contains(rootColor) &&
            occurrence.acceptance.side != rootColor
        )
      case LineConsequenceKind.ForcedTheme =>
        false

  private def causalLinks(
      line: LineFactEvidence,
      rootStep: LineReplayStep,
      actor: RootCausalActor,
      consequence: LineConsequence,
      eventStep: LineReplayStep,
      eventPlyOffset: Int,
      replay: CanonicalLineReplay
  ): Option[List[RootCausalLink]] =
    val rootMove = EvidenceRef.normalizeMove(rootStep.moveUci)
    val eventMove = EvidenceRef.normalizeMove(eventStep.moveUci)
    val eventAnchor = EvidenceSquare(eventMove.slice(2, 4))
    if eventPlyOffset == 0 then
      Some(List(RootCausalLink(
        RootCausalLinkKind.ImmediateRootAction,
        rootMove,
        eventMove,
        eventAnchor
      )))
    else
      val actorAction =
        trackedActorBefore(line, actor, eventPlyOffset, replay).filter(tracked =>
          EvidenceRef.sameMove(tracked.square.key, eventMove.take(2)) &&
            Square.fromKey(eventMove.slice(2, 4)).exists(destination =>
              replay.after(eventStep)
                .flatMap(_.board.pieceAt(destination))
                .exists(_.color == actor.color)
            )
        ).map(_ => RootCausalLink(
          RootCausalLinkKind.RootActorContinuation,
          rootMove,
          eventMove,
          eventAnchor
        ))
      val lineAccess =
        continuousLineAccessSeedLink(line, eventPlyOffset, replay)
      val forcedCaptureResponse =
        forcedCaptureResponseLink(line, rootStep, eventStep, eventPlyOffset, replay)
      val forcedCheckResponse =
        forcedCheckResponseLink(line, rootStep, eventStep, eventPlyOffset, replay)
      val actorCaptured =
        Option.when(consequence.kind == LineConsequenceKind.MaterialLoss)(
          rootActorCapturedSeedLink(line, actor, eventPlyOffset, replay)
        ).flatten
      val materialSequence =
        materialSequenceLinks(line, rootStep, actor, consequence, eventPlyOffset, replay)
      (List(actorAction, lineAccess, forcedCaptureResponse, forcedCheckResponse, actorCaptured).flatten ++
        materialSequence) match
        case Nil   => None
        case links => Some(links.distinct)

  private def forcedCaptureResponseLink(
      line: LineFactEvidence,
      rootStep: LineReplayStep,
      eventStep: LineReplayStep,
      eventPlyOffset: Int,
      replay: CanonicalLineReplay
  ): Option[RootCausalLink] =
    val rootMove = EvidenceRef.normalizeMove(rootStep.moveUci)
    val eventMove = EvidenceRef.normalizeMove(eventStep.moveUci)
    if eventPlyOffset != 2 || !line.materialCaptures.exists(capture =>
        capture.plyOffset == 0 && EvidenceRef.sameMove(capture.moveUci, rootMove)
      )
    then None
    else
      for
        reply <- line.lineReplaySteps.lift(1)
        trajectory <- CaptureResponseFollowUpTrajectory.find(rootStep, reply, eventStep, List(reply), replay)
      yield RootCausalLink(
        RootCausalLinkKind.ForcedCaptureResponse,
        rootMove,
        eventMove,
        trajectory.replyTo
      )

  private def forcedCheckResponseLink(
      line: LineFactEvidence,
      rootStep: LineReplayStep,
      eventStep: LineReplayStep,
      eventPlyOffset: Int,
      replay: CanonicalLineReplay
  ): Option[RootCausalLink] =
    val rootMove = EvidenceRef.normalizeMove(rootStep.moveUci)
    val eventMove = EvidenceRef.normalizeMove(eventStep.moveUci)
    if eventPlyOffset != 2 then None
    else
      for
        reply <- line.lineReplaySteps.lift(1)
        trajectory <- CheckResponseFollowUpTrajectory
          .findRootActorContinuation(rootStep, reply, eventStep, List(reply), replay)
      yield RootCausalLink(
        RootCausalLinkKind.ForcedCheckResponse,
        rootMove,
        eventMove,
        trajectory.replyTo
      )

  private def continuousLineAccessSeedLink(
      line: LineFactEvidence,
      eventPlyOffset: Int,
      replay: CanonicalLineReplay
  ): Option[RootCausalLink] =
    for
      rootStep <- line.lineReplaySteps.headOption
      if EvidenceRef.sameMove(rootStep.moveUci, line.line.rootMove)
      eventStep <- line.lineReplaySteps.lift(eventPlyOffset)
      trajectory <- LineAccessTrajectory.findRootClearanceBeforeUse(
        rootStep,
        eventStep,
        line.lineReplaySteps.slice(1, eventPlyOffset),
        replay
      )
    yield RootCausalLink(
      RootCausalLinkKind.ContinuousLineAccess,
      EvidenceRef.normalizeMove(rootStep.moveUci),
      EvidenceRef.normalizeMove(eventStep.moveUci),
      trajectory.vacatedSquare
    )

  private def rootActorCapturedSeedLink(
      line: LineFactEvidence,
      actor: RootCausalActor,
      eventPlyOffset: Int,
      replay: CanonicalLineReplay
  ): Option[RootCausalLink] =
    for
      rootStep <- line.lineReplaySteps.headOption
      if EvidenceRef.sameMove(rootStep.moveUci, line.line.rootMove)
      if EvidenceRef.sameMove(actor.moveUci, line.line.rootMove)
      eventStep <- line.lineReplaySteps.lift(eventPlyOffset)
      tracked <- trackedActorBefore(line, actor, eventPlyOffset, replay)
      capture <- line.uniqueMaterialCaptureAt(eventPlyOffset, eventStep.moveUci)
      if capture.side != actor.color
      if capture.square.key.equalsIgnoreCase(tracked.square.key)
      if capture.capturedRole.name.equalsIgnoreCase(tracked.role.name)
      immediateReplyOwnsLoss =
        eventPlyOffset == 1 &&
          line.lineReplaySteps.lift(1).contains(eventStep)
      rootSacrificeOwnsLoss =
        line.rootCaptureSacrificeResponses(line.line.rootMove).contains(capture)
      if immediateReplyOwnsLoss || rootSacrificeOwnsLoss
    yield RootCausalLink(
      RootCausalLinkKind.RootActorCaptured,
      EvidenceRef.normalizeMove(rootStep.moveUci),
      EvidenceRef.normalizeMove(eventStep.moveUci),
      EvidenceSquare(tracked.square.key)
    )

  private def materialSequenceLinks(
      line: LineFactEvidence,
      rootStep: LineReplayStep,
      actor: RootCausalActor,
      consequence: LineConsequence,
      eventPlyOffset: Int,
      replay: CanonicalLineReplay
  ): List[RootCausalLink] =
    val consequenceMoves = consequence.lineMoves.map(EvidenceRef.normalizeMove).toSet
    val capturesByPly = line.materialCaptures
      .filter(capture =>
        capture.plyOffset > 0 &&
          capture.plyOffset <= eventPlyOffset &&
          consequenceMoves.exists(EvidenceRef.sameMove(_, capture.moveUci))
      )
      .map(capture => capture.plyOffset -> capture)
      .toMap
    val capturePlies = capturesByPly.keys.toList.sorted
    var paths = capturePlies.flatMap { ply =>
      rootMaterialSeedLink(line, rootStep, actor, ply, replay).map(link => ply -> List(link))
    }.toMap
    capturePlies.foreach { toPly =>
      if !paths.contains(toPly) then
        val predecessor = paths.keys.toList.sorted.reverse.collectFirst(Function.unlift { fromPly =>
          materialContinuationLink(line, capturesByPly(fromPly), capturesByPly(toPly), replay).map(link =>
            paths(fromPly) :+ link
          )
        })
        predecessor.foreach(path => paths = paths.updated(toPly, path))
    }
    paths.getOrElse(eventPlyOffset, Nil)

  private def rootMaterialSeedLink(
      line: LineFactEvidence,
      rootStep: LineReplayStep,
      actor: RootCausalActor,
      eventPlyOffset: Int,
      replay: CanonicalLineReplay
  ): Option[RootCausalLink] =
    line.lineReplaySteps.lift(eventPlyOffset).flatMap { eventStep =>
      val rootMove = EvidenceRef.normalizeMove(rootStep.moveUci)
      val eventMove = EvidenceRef.normalizeMove(eventStep.moveUci)
      val anchor = EvidenceSquare(eventMove.slice(2, 4))
      val actorAction =
        trackedActorBefore(line, actor, eventPlyOffset, replay)
          .filter(tracked => EvidenceRef.sameMove(tracked.square.key, eventMove.take(2)))
          .map(_ => RootCausalLink(
            RootCausalLinkKind.RootActorContinuation,
            rootMove,
            eventMove,
            anchor
          ))
      val lineAccess =
        continuousLineAccessSeedLink(line, eventPlyOffset, replay)
      forcedCaptureResponseLink(line, rootStep, eventStep, eventPlyOffset, replay)
        .orElse(forcedCheckResponseLink(line, rootStep, eventStep, eventPlyOffset, replay))
        .orElse(rootActorCapturedSeedLink(line, actor, eventPlyOffset, replay))
        .orElse(lineAccess)
        .orElse(actorAction)
    }

  private def materialContinuationLink(
      line: LineFactEvidence,
      fromCapture: LineMaterialCapture,
      toCapture: LineMaterialCapture,
      replay: CanonicalLineReplay
  ): Option[RootCausalLink] =
    for
      fromStep <- line.lineReplaySteps.lift(fromCapture.plyOffset)
      toStep <- line.lineReplaySteps.lift(toCapture.plyOffset)
      if toCapture.plyOffset > fromCapture.plyOffset
      link <-
        val continuation = line.lineReplaySteps.slice(fromCapture.plyOffset + 1, toCapture.plyOffset + 1)
        LineObjectTrajectory
          .find(fromStep, continuation, toCapture.plyOffset - fromCapture.plyOffset, replay)
          .filter(_.futureStep == toStep)
          .map(trajectory => RootCausalLink(
            RootCausalLinkKind.MaterialActorContinuation,
            fromStep.moveUci,
            toStep.moveUci,
            trajectory.rootTo
          ))
          .orElse(
            materialActorCaptured(line, fromCapture, toCapture, replay).map(anchor => RootCausalLink(
              RootCausalLinkKind.MaterialCaptureResponse,
              fromStep.moveUci,
              toStep.moveUci,
              anchor
            ))
          )
    yield link

  private def materialActorCaptured(
      line: LineFactEvidence,
      fromCapture: LineMaterialCapture,
      toCapture: LineMaterialCapture,
      replay: CanonicalLineReplay
  ): Option[EvidenceSquare] =
    val fromMove = EvidenceRef.normalizeMove(fromCapture.moveUci)
    val destination = fromMove.slice(2, 4)
    val intervening = line.lineReplaySteps.slice(fromCapture.plyOffset + 1, toCapture.plyOffset)
    Square.fromKey(destination).flatMap { destinationSquare =>
      val endpointVerified =
        for
          fromStep <- line.lineReplaySteps.lift(fromCapture.plyOffset)
          toStep <- line.lineReplaySteps.lift(toCapture.plyOffset)
          afterFrom <- replay.after(fromStep)
          beforeTo <- replay.before(toStep)
          afterTo <- replay.after(toStep)
          toDestination <- Square.fromKey(EvidenceRef.normalizeMove(toStep.moveUci).slice(2, 4))
          if toDestination == destinationSquare
          offeredActor <- afterFrom.board.pieceAt(destinationSquare)
          if offeredActor.color == fromCapture.side
          if offeredActor.role.toString.equalsIgnoreCase(fromCapture.attackerRole.name)
          if beforeTo.board.pieceAt(destinationSquare).contains(offeredActor)
          capturingActor <- afterTo.board.pieceAt(destinationSquare)
          if capturingActor.color == toCapture.side
        yield true
      val actorPersists = intervening.forall(step =>
        replay.before(step)
          .flatMap(_.board.pieceAt(destinationSquare))
          .exists(piece =>
            piece.color == fromCapture.side &&
              piece.role.toString.equalsIgnoreCase(fromCapture.attackerRole.name)
          ) &&
          replay.after(step)
            .flatMap(_.board.pieceAt(destinationSquare))
            .exists(piece =>
              piece.color == fromCapture.side &&
                piece.role.toString.equalsIgnoreCase(fromCapture.attackerRole.name)
            )
      )
      Option.when(
        endpointVerified.contains(true) &&
          actorPersists &&
          toCapture.side != fromCapture.side &&
          toCapture.square.key.equalsIgnoreCase(destination) &&
          toCapture.capturedRole.name.equalsIgnoreCase(fromCapture.attackerRole.name)
      )(EvidenceSquare(destination))
    }

  private def trackedActorBefore(
      line: LineFactEvidence,
      actor: RootCausalActor,
      eventPlyOffset: Int,
      replay: CanonicalLineReplay
  ): Option[TrackedActor] =
    val initial = for
      square <- Square.fromKey(actor.to.key)
      rootAfter <- line.lineReplaySteps.headOption.flatMap(replay.after)
      piece <- rootAfter.board.pieceAt(square)
      if piece.color == actor.color
    yield TrackedActor(square, EvidencePieceRole(piece.role.toString))
    line.lineReplaySteps.slice(1, eventPlyOffset).foldLeft(initial) { (tracked, step) =>
      tracked.flatMap { current =>
        val move = EvidenceRef.normalizeMove(step.moveUci)
        for
          from <- Square.fromKey(move.take(2))
          to <- Square.fromKey(move.slice(2, 4))
          before <- replay.before(step)
          after <- replay.after(step)
          pieceBefore <- before.board.pieceAt(current.square)
          if pieceBefore.color == actor.color
          if pieceBefore.role.toString.equalsIgnoreCase(current.role.name)
          next <-
            if from == current.square then
              after.board.pieceAt(to).filter(_.color == actor.color).map(piece =>
                TrackedActor(to, EvidencePieceRole(piece.role.toString))
              )
            else if to == current.square then None
            else
              after.board.pieceAt(current.square)
                .filter(piece =>
                  piece.color == actor.color && piece.role.toString.equalsIgnoreCase(current.role.name)
                )
                .map(_ => current)
        yield next
      }
    }

object LineFactEvidence:
  private[chessjudgment] def fromCertifiedReplay(
      line: LineNodeRef,
      replay: CanonicalLineReplay,
      forcedTheme: Option[ForcedLineThemeEvidence] = None,
      material: Option[LineMaterialSummary] = None,
      events: List[LineMoveEvent] = Nil,
      consequences: List[LineConsequence] = Nil,
      predecessorReplay: Option[CanonicalLineReplay] = None
  ): LineFactEvidence =
    require(
      replay.replaySteps.headOption.exists(step => EvidenceRef.sameMove(step.moveUci, line.rootMove)),
      "certified line evidence must begin with its declared root move"
    )
    val derived = LineReplayDerivedFacts.from(replay)
    val provisional = LineFactEvidence(
      line = line,
      forcedTheme = forcedTheme,
      material = material,
      replay = replay.replaySteps,
      events = events,
      consequences = consequences,
      replayDerived = derived,
      canonicalReplay = Some(replay),
      canonicalPredecessorReplay = predecessorReplay
    )
    val episodes = RootOwnedCausalEpisode
      .from(provisional, line.rootMove, replay)
      .filter(RootOwnedEffectPolicy.admitsLineEpisode(provisional, _))
    provisional.copy(
      replayDerived = LineReplayDerivedFacts.withRootOwnedEpisodes(derived, episodes)
    )

  def fromRecords(records: List[EvidenceRecord]): List[LineFactEvidence] =
    records.collect { case EvidenceRecord(_, payload: LineFactEvidence, _) => payload }

  def materialOutcomeProfile(records: List[EvidenceRecord]): LineMaterialOutcomeProfile =
    fromRecords(records).map(_.materialOutcomeProfile).foldLeft(LineMaterialOutcomeProfile.empty)(_.merge(_))





  def hasMaterialRecaptureChain(records: List[EvidenceRecord]): Boolean =
    fromRecords(records).exists(_.hasMaterialRecaptureChain)

  def hasMaterialRecoveryWindow(records: List[EvidenceRecord]): Boolean =
    fromRecords(records).exists(_.hasMaterialRecoveryWindow)


final case class CandidateLineEvaluationEvidence(
    line: LineNodeRef,
    evaluation: lila.chessjudgment.model.line.CandidateLineEvaluation
) extends EvidencePayload


final case class MoveTransitionEvidence(
    moveUci: String,
    from: PositionNodeRef,
    to: PositionNodeRef
) extends EvidencePayload

enum StructuralSignalPolarity:
  case Gain
  case Loss
  case Neutral

enum StructuralSignalKind:
  case PawnTensionCreated
  case PawnTensionResolved
  case PassedPawnCreated
  case PassedPawnAdvanced
  case BatteryCreated

/** Typed chess objects carried by structural signals and consequences.
  * `label` is a one-way presentation projection; no proof code may recover
  * chess semantics by parsing it.
  */
enum StructuralSubject:
  case OpenFile(file: EvidenceFile)
  case SemiOpenFile(file: EvidenceFile)
  case BreakFile(file: EvidenceFile)
  case FileOccupation(file: EvidenceFile, square: EvidenceSquare, role: EvidencePieceRole)
  case PieceAt(role: EvidencePieceRole, square: EvidenceSquare)
  case PawnTensionCreated(from: EvidenceSquare, to: EvidenceSquare)
  case PawnTensionResolved(from: EvidenceSquare, to: EvidenceSquare)
  case Battery(detail: RelationWitnessDetail.RayBarrier)
  case PassedPawnCreated(side: Color, square: EvidenceSquare)
  case PassedPawnLost(side: Color, square: EvidenceSquare)
  case PassedPawnAdvanced(side: Color, from: EvidenceSquare, to: EvidenceSquare, relativeRank: Int)
  case PassedStatusCreated(side: Color, from: EvidenceSquare, to: EvidenceSquare, relativeRank: Int)
  case PassedPawnPromoted(side: Color, from: EvidenceSquare, to: EvidenceSquare)
  case OpponentResourceDeterred(role: EvidencePieceRole, from: EvidenceSquare, to: EvidenceSquare)

  def stableKey: String = StructuralSubject.stableKey(this)

  def label: String =
    this match
      case OpenFile(file) => s"open-file:${file.key.toLowerCase}"
      case SemiOpenFile(file) => s"semi-open-file:${file.key.toLowerCase}"
      case BreakFile(file) => s"break-file:${file.key.toLowerCase}"
      case FileOccupation(file, square, _) =>
        s"${file.key.toLowerCase}:${square.key.toLowerCase}"
      case PieceAt(role, square) =>
        s"${role.name.toLowerCase}:${square.key.toLowerCase}"
      case PawnTensionCreated(from, to) =>
        s"created-tension:${from.key.toLowerCase}-${to.key.toLowerCase}"
      case PawnTensionResolved(from, to) =>
        s"resolved-tension:${from.key.toLowerCase}-${to.key.toLowerCase}"
      case Battery(detail) =>
        val endpoints = List(
          detail.attackerSquare.key.toLowerCase -> detail.attackerRole.name.toLowerCase,
          detail.occupants.head.square.key.toLowerCase -> detail.occupants.head.role.name.toLowerCase
        ).sortBy(_._1)
        s"battery:${detail.axis.toString.toLowerCase}:${endpoints.map(_._1).mkString("-")}:${endpoints.map(_._2).mkString("-")}"
      case PassedPawnCreated(_, square) =>
        s"passed-pawn-created:${square.key.toLowerCase}"
      case PassedPawnLost(_, square) =>
        s"passed-pawn-lost:${square.key.toLowerCase}"
      case PassedPawnAdvanced(_, from, to, rank) =>
        s"passed-pawn-advanced:${from.key.toLowerCase}-${to.key.toLowerCase}:rank-$rank"
      case PassedStatusCreated(_, from, to, rank) =>
        s"passed-status-created:${from.key.toLowerCase}-${to.key.toLowerCase}:rank-$rank"
      case PassedPawnPromoted(_, from, to) =>
        s"passed-pawn-promoted:${from.key.toLowerCase}-${to.key.toLowerCase}"
      case OpponentResourceDeterred(role, from, to) =>
        s"${role.name.toLowerCase}:${from.key.toLowerCase}-${to.key.toLowerCase}:resource-deterred"

  def semanticSquares: List[EvidenceSquare] =
    this match
      case OpenFile(_) | SemiOpenFile(_) | BreakFile(_) => Nil
      case FileOccupation(_, square, _) => List(square)
      case PieceAt(_, square) => List(square)
      case PawnTensionCreated(from, to) => List(from, to)
      case PawnTensionResolved(from, to) => List(from, to)
      case Battery(detail) =>
        detail.attackerSquare :: detail.occupants.map(_.square)
      case PassedPawnCreated(_, square) => List(square)
      case PassedPawnLost(_, square) => List(square)
      case PassedPawnAdvanced(_, from, to, _) => List(from, to)
      case PassedStatusCreated(_, from, to, _) => List(from, to)
      case PassedPawnPromoted(_, from, to) => List(from, to)
      case OpponentResourceDeterred(_, from, to) => List(from, to)

  def targetSquares: List[EvidenceSquare] =
    this match
      case OpenFile(_) | SemiOpenFile(_) | BreakFile(_) => Nil
      case FileOccupation(_, square, _) => List(square)
      case PieceAt(_, square) => List(square)
      case PawnTensionCreated(from, to) => List(from, to)
      case PawnTensionResolved(from, to) => List(from, to)
      case Battery(detail) =>
        RelationRayProjection.immediateTarget(detail).map(_.square).toList
      case PassedPawnCreated(_, square) => List(square)
      case PassedPawnLost(_, square) => List(square)
      case PassedPawnAdvanced(_, _, to, _) => List(to)
      case PassedStatusCreated(_, _, to, _) => List(to)
      case PassedPawnPromoted(_, _, to) => List(to)
      case OpponentResourceDeterred(_, _, to) => List(to)

  def identityKey: Option[String] =
    this match
      case _: PawnTensionCreated | _: PawnTensionResolved |
          _: PassedPawnCreated | _: PassedPawnLost | _: PassedPawnAdvanced |
          _: PassedStatusCreated | _: PassedPawnPromoted =>
        Some(label)
      case _ => None

object StructuralSubject:
  private def atom(value: String): String =
    val normalized = Option(value).getOrElse("").trim.toLowerCase
    s"${normalized.length}:$normalized"

  private def key(kind: String, values: String*): String =
    atom(kind) + values.iterator.map(atom).mkString

  def stableKey(subject: StructuralSubject): String =
    subject match
      case OpenFile(file) => key("open-file", file.key)
      case SemiOpenFile(file) => key("semi-open-file", file.key)
      case BreakFile(file) => key("break-file", file.key)
      case FileOccupation(file, square, role) => key("file-occupation", file.key, square.key, role.name)
      case PieceAt(role, square) => key("piece-at", role.name, square.key)
      case PawnTensionCreated(from, to) => key("pawn-tension-created", from.key, to.key)
      case PawnTensionResolved(from, to) => key("pawn-tension-resolved", from.key, to.key)
      case Battery(detail) => key("battery", RelationWitnessDetail.stableKey(detail))
      case PassedPawnCreated(side, square) => key("passed-pawn-created", side.toString, square.key)
      case PassedPawnLost(side, square) => key("passed-pawn-lost", side.toString, square.key)
      case PassedPawnAdvanced(side, from, to, rank) =>
        key("passed-pawn-advanced", side.toString, from.key, to.key, rank.toString)
      case PassedStatusCreated(side, from, to, rank) =>
        key("passed-status-created", side.toString, from.key, to.key, rank.toString)
      case PassedPawnPromoted(side, from, to) =>
        key("passed-pawn-promoted", side.toString, from.key, to.key)
      case OpponentResourceDeterred(role, from, to) =>
        key("opponent-resource-deterred", role.name, from.key, to.key)

final case class StructuralSignal(
    kind: StructuralSignalKind,
    magnitude: Int,
    subjectFacts: List[StructuralSubject] = Nil
):
  def subjects: List[String] = subjectFacts.map(_.label)
  def anchorKey: String =
    kind.toString

enum TransitionConsequenceKind:
  case OpenFileEstablished
  case SemiOpenFileEstablished
  case FileOccupationEstablished
  case PawnTensionCreated
  case PawnTensionResolution
  case PassedPawnProgress
  case PassedPawnConcession
  case BatteryFormation
  case OpponentMobilityRestriction

object TransitionConsequenceKind:
  private val RootActorBound = Set(
    FileOccupationEstablished,
    BatteryFormation,
    PassedPawnProgress
  )

  private val EstablishedStates = Set(
    OpenFileEstablished,
    SemiOpenFileEstablished,
    FileOccupationEstablished,
    PawnTensionCreated,
    PassedPawnProgress,
    BatteryFormation,
    OpponentMobilityRestriction
  )

  private val RemovedStates = Set(
    PawnTensionResolution,
    PassedPawnConcession
  )

  def requiresRootActorSurvival(kind: TransitionConsequenceKind): Boolean =
    RootActorBound(kind)

  /** Direction of the observed state change, not an evaluation of the move. */
  def observedPolarity(kind: TransitionConsequenceKind): StructuralSignalPolarity =
    kind match
      case PassedPawnConcession =>
        StructuralSignalPolarity.Loss
      case OpenFileEstablished | SemiOpenFileEstablished | FileOccupationEstablished |
          PawnTensionCreated | PawnTensionResolution | BatteryFormation =>
        StructuralSignalPolarity.Neutral
      case PassedPawnProgress | OpponentMobilityRestriction =>
        StructuralSignalPolarity.Gain

  def establishesState(kind: TransitionConsequenceKind): Boolean = EstablishedStates(kind)
  def removesState(kind: TransitionConsequenceKind): Boolean = RemovedStates(kind)

enum TransitionConsequenceCategory:
  case PawnStructure
  case PawnStructureDelta
  case PieceActivity

final case class StructuralSubjectBinding private[chessjudgment] (
    subject: StructuralSubject,
    relationKeys: List[RelationChangeKey]
):
  require(relationKeys.distinct.size == relationKeys.size, "duplicate structural-subject relation keys")
  def stableKey: String =
    s"${subject.stableKey}:relations:${relationKeys.map(_.stableKey).mkString("[", ",", "]")}"

object StructuralSubjectBinding:
  private[chessjudgment] def unbound(subject: StructuralSubject): StructuralSubjectBinding =
    StructuralSubjectBinding(subject, Nil)

  private[chessjudgment] def fromRelations(
      subject: StructuralSubject,
      relationKeys: List[RelationChangeKey]
  ): StructuralSubjectBinding =
    require(relationKeys.nonEmpty, "a relation-derived structural subject requires exact relation keys")
    StructuralSubjectBinding(subject, relationKeys.distinct.sortBy(_.stableKey))

final case class TransitionConsequence private[chessjudgment] (
    kind: TransitionConsequenceKind,
    strength: Int,
    subjectBindings: List[StructuralSubjectBinding] = Nil,
    targetBindings: List[StructuralSubjectBinding] = Nil
):
  require(subjectBindings.distinct.size == subjectBindings.size, "duplicate structural consequence subject bindings")
  require(targetBindings.distinct.size == targetBindings.size, "duplicate structural consequence target bindings")
  def subjectFacts: List[StructuralSubject] = subjectBindings.map(_.subject)
  def targetSubjectFacts: List[StructuralSubject] = targetBindings.map(_.subject)
  private[chessjudgment] def relationKeys: List[RelationChangeKey] =
    (subjectBindings ++ targetBindings).flatMap(_.relationKeys).distinct.sortBy(_.stableKey)
  require(subjectFacts.distinct.size == subjectFacts.size, "duplicate structural consequence subjects")
  require(targetSubjectFacts.distinct.size == targetSubjectFacts.size, "duplicate structural consequence targets")

  def subjects: List[String] = subjectFacts.map(_.label)
  def targetSubjects: List[String] = targetSubjectFacts.map(_.label)
  def polarity: StructuralSignalPolarity =
    TransitionConsequenceKind.observedPolarity(kind)
  def establishesState: Boolean =
    TransitionConsequenceKind.establishesState(kind)
  def removesState: Boolean =
    TransitionConsequenceKind.removesState(kind)
  def anchorKey: String =
    s"$kind:$polarity"
  private[chessjudgment] def stableKey: String =
    PlanCausalProofKey.product(
      "transition-consequence",
      List(
        kind.toString.toLowerCase,
        strength.toString,
        PlanCausalProofKey.sequence(subjectBindings.map(_.stableKey).distinct.sorted),
        PlanCausalProofKey.sequence(targetBindings.map(_.stableKey).distinct.sorted)
      )
    )
  private[chessjudgment] def goalSubjectFacts: List[StructuralSubject] =
    if targetSubjectFacts.nonEmpty then targetSubjectFacts else subjectFacts
  private[chessjudgment] def witnessSubjectFacts: List[StructuralSubject] =
    if targetSubjectFacts.isEmpty then Nil else subjectFacts.filterNot(targetSubjectFacts.toSet)
  def goalSubjects: List[String] =
    goalSubjectFacts.map(_.label)
  def witnessSubjects: List[String] =
    witnessSubjectFacts.map(_.label)
  private[chessjudgment] def goalSubjectBindings: List[StructuralSubjectBinding] =
    if targetBindings.nonEmpty then targetBindings else subjectBindings
  private[chessjudgment] def witnessSubjectBindings: List[StructuralSubjectBinding] =
    if targetBindings.isEmpty then Nil else subjectBindings.filterNot(targetBindings.toSet)

private[chessjudgment] object TransitionConsequenceRelationProof:
  private val RelationBackedKinds = Set(
    TransitionConsequenceKind.OpenFileEstablished,
    TransitionConsequenceKind.SemiOpenFileEstablished,
    TransitionConsequenceKind.FileOccupationEstablished,
    TransitionConsequenceKind.PawnTensionCreated,
    TransitionConsequenceKind.PawnTensionResolution,
    TransitionConsequenceKind.PassedPawnProgress,
    TransitionConsequenceKind.PassedPawnConcession,
    TransitionConsequenceKind.BatteryFormation
  )

  def relationBacked(kind: TransitionConsequenceKind): Boolean =
    RelationBackedKinds(kind)

  def provesSemantic(
      consequences: List[TransitionConsequence],
      changes: List[RelationSemanticChange]
  ): Boolean =
    relationKeysResolve(consequences, changes.map(_.key))

  def provesCanonical(
      consequences: List[TransitionConsequence],
      changes: List[CanonicalRelationChange],
      transition: StructuralTransitionBinding
  ): Boolean =
    relationKeysResolve(consequences, changes.map(_.key)) &&
      changes.forall { change =>
        val owner = change.direction match
          case RelationChangeDirection.Removed     => transition.from
          case RelationChangeDirection.Established => transition.to
        change.source.position == owner
      }

  private def relationKeysResolve(
      consequences: List[TransitionConsequence],
      availableKeys: List[RelationChangeKey]
  ): Boolean =
    val available = availableKeys.toSet
    available.size == availableKeys.size && consequences.forall { consequence =>
      val bindings = consequence.subjectBindings ++ consequence.targetBindings
      val allKeysResolve = bindings.flatMap(_.relationKeys).forall(available)
      if relationBacked(consequence.kind) then
        bindings.nonEmpty && bindings.forall(_.relationKeys.nonEmpty) && allKeysResolve
      else
        bindings.forall(_.relationKeys.isEmpty)
    }

final case class StructuralTransitionBinding(
    moveUci: String,
    role: TransitionEdgeRole,
    from: PositionNodeRef,
    to: PositionNodeRef,
    line: Option[LineNodeRef],
    perspective: Color,
    actorRole: Option[EvidencePieceRole] = None
)

enum RelationEvidenceOrigin:
  case Unverified
  case PositionSnapshot(semanticFen: String)
  case LegalReplay(
      semanticStartFen: String,
      semanticAfterFen: String,
      proofMoves: List[String]
  )

private[chessjudgment] final class ClosedRelationOutputBinding private[judgment] (
    val result: EvidenceRef,
    val relation: RelationFactEvidence,
    val sources: List[EvidenceRef]
):
  require(sources.nonEmpty, "a replay-derived relation output needs exact canonical sources")
  require(sources.map(_.id).distinct.size == sources.size, "a relation output cannot repeat one source")

private[judgment] object ClosedRelationOutputBinding:
  def certified(
      result: EvidenceRef,
      relation: RelationFactEvidence,
      sources: List[EvidenceRef]
  ): ClosedRelationOutputBinding =
    new ClosedRelationOutputBinding(result, relation, sources.sortBy(_.id))

/** Persistent occurrence owner for one already-evaluated closed relation
  * production. Empty contract result lists are evidence that the contract was
  * evaluated at this transition, not synthetic negative chess facts.
  */
final class ClosedRelationOccurrenceEvidence private[judgment] (
    val edge: MoveTransitionEdge,
    val lineOwner: Option[LineNodeRef],
    val lineEvidence: Option[EvidenceRef],
    private[chessjudgment] val closedResults: VectorMap[RelationCombinationContractKind, List[EvidenceRef]],
    private val outputsByEvidenceId: Map[String, ClosedRelationOutputBinding]
) extends EvidencePayload:
  val scope: EvidenceScope = edge.role.scope

  private[chessjudgment] def outputFor(result: EvidenceRef): Option[ClosedRelationOutputBinding] =
    outputsByEvidenceId.get(result.id).filter(_.result == result)

  private[chessjudgment] def outputs: List[ClosedRelationOutputBinding] =
    outputsByEvidenceId.values.toList.sortBy(_.result.id)

private[judgment] object ClosedRelationOccurrenceEvidence:
  def certified(
      edge: MoveTransitionEdge,
      lineOwner: Option[LineNodeRef],
      lineEvidence: Option[EvidenceRef],
      closedResults: VectorMap[RelationCombinationContractKind, List[EvidenceRef]],
      outputs: List[ClosedRelationOutputBinding]
  ): ClosedRelationOccurrenceEvidence =
    require(
      edge.evidence.producer == EvidenceProducer.MoveTransitionProducer &&
        edge.evidence.layer == EvidenceLayer.MoveTransition &&
        edge.evidence.position == edge.from &&
        edge.evidence.line.isEmpty &&
        edge.evidence.scope == edge.role.scope &&
        edge.evidence.confidence == EvidenceConfidence.LegalReplayVerified,
      "a closed relation occurrence must be owned by its exact admitted transition authority"
    )
    require(
      lineOwner.isDefined == lineEvidence.isDefined,
      "a closed relation occurrence must preserve its exact optional line owner"
    )
    require(
      lineOwner.forall(line =>
        lineEvidence.exists(ref =>
          ref.producer == EvidenceProducer.LegalLineProducer &&
            ref.layer == EvidenceLayer.Line &&
            ref.position == edge.from &&
            ref.line.contains(line) &&
            ref.scope == line.role.scope &&
            ref.confidence == EvidenceConfidence.LegalReplayVerified &&
            line.role == edge.role.lineRole &&
            EvidenceRef.sameMove(line.rootMove, edge.moveUci)
        )
      ),
      lineOwner.zip(lineEvidence).headOption
        .map { case (line, ref) =>
          s"closed relation line authority mismatch: producer=${ref.producer}, layer=${ref.layer}, " +
            s"position=${ref.position == edge.from}, scope=${ref.scope}/${line.role.scope}, " +
            s"confidence=${ref.confidence}, role=${line.role}/${edge.role.lineRole}, " +
            s"move=${line.rootMove}/${edge.moveUci}"
        }
        .getOrElse("a closed relation occurrence line must be owned by its exact line evidence")
    )
    require(
      closedResults.keySet == RelationCombinationContractKind.values.toSet,
      "a closed relation occurrence must preserve every registered contract, including empty results"
    )
    require(outputs.map(_.result.id).distinct.size == outputs.size, "a relation occurrence cannot repeat an output")
    require(
      outputs.forall(output =>
        output.result.producer == EvidenceProducer.RelationProducer &&
          output.result.layer == EvidenceLayer.Relation &&
          output.result.position == edge.from &&
          output.result.line == lineOwner &&
          output.result.scope == edge.role.scope &&
          output.result.confidence == EvidenceConfidence.LegalReplayVerified
      ),
      "every relation output must belong to the exact transition occurrence"
    )
    val outputByRef = outputs.map(output => output.result -> output).toMap
    val closedOutputRefs = closedResults.valuesIterator.flatten.toList
    require(
      closedOutputRefs.distinct.size == closedOutputRefs.size && closedOutputRefs.forall(outputByRef.contains),
      "closed contract results must name unique materialized relation outputs"
    )
    val combinedOutputs = outputs.filter(output =>
      RelationCombinationContractKind.forDetail(output.relation.detail).nonEmpty
    )
    val combinedOutputIds = combinedOutputs.iterator.map(_.result.id).toSet
    require(
      combinedOutputs.map(_.result).toSet == closedOutputRefs.toSet &&
        combinedOutputs.forall(output =>
          RelationCombinationContractKind.forDetail(output.relation.detail).exists(contract =>
            closedResults(contract).contains(output.result)
          ) && output.sources.size == output.relation.combinationPremises.size
        ),
      "closed contract ledgers must exactly own every combined relation output"
    )
    require(
      outputs.filterNot(output => combinedOutputIds(output.result.id)).forall(output =>
        output.relation.detail.isInstanceOf[RelationWitnessDetail.RayBarrier] &&
          output.relation.hasLineProof && output.sources.size == 1
      ),
      "non-combination relation outputs must be exact root-after ray projections"
    )
    new ClosedRelationOccurrenceEvidence(
      edge,
      lineOwner,
      lineEvidence,
      closedResults.view.mapValues(_.sortBy(_.id)).to(VectorMap),
      outputs.map(output => output.result.id -> output).toMap
    )

  def record(id: String, payload: ClosedRelationOccurrenceEvidence): EvidenceRecord =
    EvidenceRecord(
      ref = EvidenceRef(
        id = id,
        producer = EvidenceProducer.RelationProducer,
        layer = EvidenceLayer.Relation,
        position = payload.edge.from,
        line = payload.lineOwner,
        scope = payload.scope,
        confidence = EvidenceConfidence.LegalReplayVerified
      ),
      payload = payload,
      parents = payload.edge.evidence :: payload.lineEvidence.toList
    )

private[chessjudgment] enum RelationDependencyKey:
  case AttackOrigin(square: Square)
  case AttackTarget(square: Square)
  case LegalMoveInventory
  case SliderOrigin(square: Square)
  case PawnIdentity(side: Color, square: Square)
  case PawnFrontCell(side: Color, square: Square)
  case PawnFile(side: Color, file: File)
  case FileAccess(side: Color, file: File)

private[chessjudgment] final case class RelationDependencyFootprint(
    keys: Set[RelationDependencyKey],
    squares: List[EvidenceSquare]
):
  require(keys.nonEmpty, "a board relation dependency footprint requires typed invalidation keys")
  require(squares.nonEmpty, "a board relation dependency footprint requires exact board cells")

private[chessjudgment] object RelationDependencyFootprint:
  import RelationDependencyKey.*

  def forBoardRelation(detail: RelationWitnessDetail): Option[RelationDependencyFootprint] =
    val keys = detail match
      case RelationWitnessDetail.GeometricControl(_, attacker, _, target, _) =>
        Set[RelationDependencyKey](AttackOrigin(boardSquare(attacker)), AttackTarget(boardSquare(target)))
      case _: RelationWitnessDetail.LegalMove =>
        Set[RelationDependencyKey](LegalMoveInventory)
      case RelationWitnessDetail.RayBarrier(_, attacker, _, _, _) =>
        Set[RelationDependencyKey](SliderOrigin(boardSquare(attacker)))
      case RelationWitnessDetail.PawnFileGroup(side, file, _) =>
        Set[RelationDependencyKey](PawnFile(side, boardFile(file)))
      case RelationWitnessDetail.PawnTension(whitePawn, blackPawn) =>
        Set[RelationDependencyKey](
          PawnIdentity(Color.White, boardSquare(whitePawn)),
          PawnIdentity(Color.Black, boardSquare(blackPawn))
        )
      case RelationWitnessDetail.PawnFrontOccupancy(side, pawn, front, _) =>
        Set[RelationDependencyKey](PawnIdentity(side, boardSquare(pawn))) ++
          front.map(value => PawnFrontCell(side, boardSquare(value)))
      case RelationWitnessDetail.PawnPassage(side, pawn, _) =>
        val origin = boardSquare(pawn)
        val passage = (0 to 7).filter(rank =>
          if side.white then rank > origin.rank.value else rank < origin.rank.value
        ).flatMap(rank =>
          (-1 to 1).flatMap(fileOffset => Square.at(origin.file.value + fileOffset, rank))
        ).map(target => PawnIdentity(!side, target)).toSet
        passage + PawnIdentity(side, origin)
      case RelationWitnessDetail.MajorPieceFileOccupancy(side, file, _, _) =>
        Set[RelationDependencyKey](FileAccess(side, boardFile(file)))
      case _: RelationWitnessDetail.DoubleCheck |
          _: RelationWitnessDetail.GeometricControlSetDelta |
          _: RelationWitnessDetail.GeometricSupporterCapture | _: RelationWitnessDetail.GeometricSupportDelta |
          _: RelationWitnessDetail.SliderControlInterference |
          _: RelationWitnessDetail.GeometricLineControlAfterBlockerRemoval |
          _: RelationWitnessDetail.CheckingEnemyControlBundle =>
        Set.empty[RelationDependencyKey]
    Option.when(keys.nonEmpty) {
      val spans = detail match
        case RelationWitnessDetail.GeometricControl(_, attacker, _, target, _) => lineSpan(attacker, target)
        case RelationWitnessDetail.RayBarrier(_, attacker, _, occupants, _) =>
          raySpanToEdge(attacker, occupants.head.square)
        case RelationWitnessDetail.PawnPassage(side, pawn, _) => pawnPassageSpan(side, pawn)
        case RelationWitnessDetail.PawnFileGroup(_, file, _) => fileSpan(file)
        case RelationWitnessDetail.MajorPieceFileOccupancy(_, file, _, _) => fileSpan(file)
        case _ => Nil
      val squares = (
        RelationWitnessDetail.focusSquares(detail) ++
          RelationWitnessDetail.targetSquares(detail) ++
          RelationWitnessDetail.participants(detail).map(_.square) ++
          spans
      ).distinct.sortBy(_.key)
      RelationDependencyFootprint(keys, squares)
    }

  private def boardSquare(value: EvidenceSquare): Square =
    Square.fromKey(value.key).getOrElse(
      throw IllegalArgumentException(s"invalid canonical relation square '${value.key}'")
    )

  private def boardFile(value: EvidenceFile): File =
    value.key.toLowerCase.headOption.flatMap(File.fromChar).getOrElse(
      throw IllegalArgumentException(s"invalid canonical relation file '${value.key}'")
    )

  private def lineSpan(from: EvidenceSquare, to: EvidenceSquare): List[EvidenceSquare] =
    val origin = boardSquare(from)
    val target = boardSquare(to)
    BoardGeometry.lineSpan(origin, target)
      .map(square => EvidenceSquare(square.key))

  private def raySpanToEdge(from: EvidenceSquare, through: EvidenceSquare): List[EvidenceSquare] =
    BoardGeometry
      .raySpanToEdge(boardSquare(from), boardSquare(through))
      .map(square => EvidenceSquare(square.key))

  private def pawnPassageSpan(side: Color, pawn: EvidenceSquare): List[EvidenceSquare] =
    val origin = boardSquare(pawn)
    val ranks = (0 to 7).filter(rank =>
      if side.white then rank > origin.rank.value else rank < origin.rank.value
    )
    (-1 to 1).toList.flatMap(fileOffset =>
      ranks.flatMap(rank => Square.at(origin.file.value + fileOffset, rank))
    ).map(square => EvidenceSquare(square.key))

  private def fileSpan(file: EvidenceFile): List[EvidenceSquare] =
    val exactFile = boardFile(file)
    Rank.all.map(rank => EvidenceSquare(Square(exactFile, rank).key))

/** Immutable semantic core of one relation fact. Position/line occurrence
  * authority is deliberately kept out of this object so unchanged facts can
  * be shared by incremental position snapshots without rehashing them.
  */
private[chessjudgment] final class CanonicalRelationFact private (
    val kind: RelationFactKind,
    val detail: RelationWitnessDetail,
    val lineMoves: List[String],
    val semanticId: String,
    val dependencyFootprint: Option[RelationDependencyFootprint]
)

private[chessjudgment] object CanonicalRelationFact:
  def from(
      detail: RelationWitnessDetail,
      lineMoves: List[String]
  ): CanonicalRelationFact =
    val normalizedLineMoves = normalizeMoves(lineMoves)
    val raw = RelationWitnessDetail.stableOccurrenceKey(detail, normalizedLineMoves)
    val semanticId = MessageDigest
      .getInstance("SHA-256")
      .digest(raw.getBytes(StandardCharsets.UTF_8))
      .map(byte => f"${byte & 0xff}%02x")
      .mkString
    new CanonicalRelationFact(
      kind = RelationWitnessDetail.factKind(detail),
      detail = detail,
      lineMoves = normalizedLineMoves,
      semanticId = semanticId,
      dependencyFootprint = RelationDependencyFootprint.forBoardRelation(detail)
    )

  def normalizeMoves(moves: List[String]): List[String] =
    val normalized = moves.map(EvidenceRef.normalizeMove)
    require(
      normalized.forall(_.nonEmpty),
      "a canonical relation occurrence cannot discard an invalid move token"
    )
    normalized

final class RelationFactEvidence private (
    private[chessjudgment] val canonicalFact: CanonicalRelationFact,
    val origin: RelationEvidenceOrigin,
    private[chessjudgment] val rootTransitionFootprint: Option[BoardTransitionFootprint]
) extends EvidencePayload:
  def kind: RelationFactKind = canonicalFact.kind
  def detail: RelationWitnessDetail = canonicalFact.detail
  def lineMoves: List[String] = canonicalFact.lineMoves
  def focusSquares: List[EvidenceSquare] =
    RelationWitnessDetail.focusSquares(detail)
  def targetSquares: List[EvidenceSquare] =
    RelationWitnessDetail.targetSquares(detail)
  def participants: List[RelationParticipant] =
    RelationWitnessDetail.participants(detail)
  def combinationPremises: List[RelationCombinationPremise] =
    RelationWitnessDetail.combinationPremises(detail)
  def files: List[EvidenceFile] =
    RelationWitnessDetail.files(detail)
  def hasLineProof: Boolean =
    lineMoves.nonEmpty
  def lineProofCount: Int =
    lineMoves.size
  def hasConcreteWitness: Boolean =
    participants.nonEmpty || focusSquares.nonEmpty || files.nonEmpty || lineMoves.nonEmpty
  def semanticId: String = canonicalFact.semanticId
  def isPositionRelation: Boolean =
    detail match
      case _: RelationWitnessDetail.MajorPieceFileOccupancy |
          _: RelationWitnessDetail.GeometricControl | _: RelationWitnessDetail.LegalMove |
          _: RelationWitnessDetail.PawnFileGroup |
          _: RelationWitnessDetail.PawnTension | _: RelationWitnessDetail.PawnFrontOccupancy |
          _: RelationWitnessDetail.PawnPassage | _: RelationWitnessDetail.RayBarrier =>
        true
      case _ =>
        false
  def targetHintSquares: List[EvidenceSquare] =
    (targetSquares ++ focusSquares).distinct
  def semanticGroupingAnchors: List[EvidenceSemanticAnchor] =
    val participantKeys = participants.map { participant =>
      val role = participant.role.map(value => s":${value.name.toLowerCase}").getOrElse("")
      s"${participant.participantRole}$role@${participant.square.key.toLowerCase}"
    }
    val squareKeys = focusSquares.map(square => s"square:${square.key.toLowerCase}")
    val fileKeys = files.map(file => s"file:${file.key.toLowerCase}")
    List(
      EvidenceSemanticAnchor.of(
        EvidenceSemanticAnchorKind.Relation,
        (RelationFactKind.id(kind) :: (participantKeys ++ squareKeys ++ fileKeys).distinct.sorted)*
      )
    )
  def mentionsLineMove(moveUci: String): Boolean =
    lineMoves.exists(EvidenceRef.sameMove(_, moveUci))
  def rootGeometryConnected(moveUci: String): Boolean =
    (lineMoves, rootTransitionFootprint) match
      case (rootMove :: Nil, Some(footprint)) if EvidenceRef.sameMove(rootMove, moveUci) =>
        rootTransitionConnected(footprint)
      case _ => false

  private[chessjudgment] def rootTransitionConnected(
      footprint: BoardTransitionFootprint
  ): Boolean =
    def moved(
        side: Color,
        from: EvidenceSquare,
        to: EvidenceSquare,
        beforeRole: Option[EvidencePieceRole] = None,
        afterRole: Option[EvidencePieceRole] = None
    ): Boolean =
      footprint.pieceTransitions.exists(transition =>
        transition.side == side &&
          transition.from.key.equalsIgnoreCase(from.key) &&
          transition.to.key.equalsIgnoreCase(to.key) &&
          beforeRole.forall(role => transition.beforeRole.name.equalsIgnoreCase(role.name)) &&
          afterRole.forall(role => transition.afterRole.name.equalsIgnoreCase(role.name))
      )

    detail match
      case RelationWitnessDetail.GeometricSupporterCapture(mover, _, _, _, _, _) =>
        moved(mover.side, mover.from, mover.to, Some(mover.beforeRole), Some(mover.afterRole))
      case RelationWitnessDetail.GeometricControlSetDelta(mover, _, _, _, _, _, _, _, _, _) =>
        moved(mover.side, mover.from, mover.to, Some(mover.beforeRole), Some(mover.afterRole))
      case RelationWitnessDetail.GeometricSupportDelta(mover, _, _, _, _, _, _, _, _, _, _) =>
        moved(mover.side, mover.from, mover.to, Some(mover.beforeRole), Some(mover.afterRole))
      case RelationWitnessDetail.SliderControlInterference(mover, _, _, _, _, _, _) =>
        moved(mover.side, mover.from, mover.to, Some(mover.beforeRole), Some(mover.afterRole))
      case RelationWitnessDetail.GeometricLineControlAfterBlockerRemoval(mover, _, _, _, _, _, _, _, _, _, _, _) =>
        moved(mover.side, mover.from, mover.to, Some(mover.beforeRole), Some(mover.afterRole))
      case RelationWitnessDetail.CheckingEnemyControlBundle(mover, _, _, _) =>
        moved(mover.side, mover.from, mover.to, Some(mover.beforeRole), Some(mover.afterRole))
      case RelationWitnessDetail.DoubleCheck(mover, _, _, _) =>
        moved(mover.side, mover.from, mover.to, Some(mover.beforeRole), Some(mover.afterRole))
      case _ =>
        val changed = footprint.changedSquares.map(_.key.toLowerCase).toSet
        canonicalFact.dependencyFootprint.exists(_.squares.exists(square => changed(square.key.toLowerCase)))
  override def equals(other: Any): Boolean =
    other match
      case that: RelationFactEvidence =>
        detail == that.detail && lineMoves == that.lineMoves && origin == that.origin &&
          rootTransitionFootprint == that.rootTransitionFootprint
      case _ =>
        false
  override def hashCode: Int =
    31 * (31 * (31 * detail.hashCode + lineMoves.hashCode) + origin.hashCode) + rootTransitionFootprint.hashCode

object RelationFactEvidence:
  private def fenSideToMove(fen: String): Option[Color] =
    Option(fen).getOrElse("").trim.split("\\s+").lift(1).flatMap {
      case "w" => Some(Color.White)
      case "b" => Some(Color.Black)
      case _   => None
    }

  private def exactOccurrenceSide(position: PositionNodeRef, fen: String): Boolean =
    fenSideToMove(fen).exists(position.sideToMove.contains)

  private[judgment] def verified(ref: EvidenceRef, payload: RelationFactEvidence): Boolean =
    ref.producer == EvidenceProducer.RelationProducer &&
      ref.layer == EvidenceLayer.Relation &&
      payload.hasConcreteWitness &&
      ((ref.confidence, payload.origin) match
        case (
              EvidenceConfidence.BoardDerived,
              RelationEvidenceOrigin.PositionSnapshot(semanticFen)
            ) =>
          ref.line.isEmpty &&
            payload.isPositionRelation &&
            !payload.hasLineProof &&
            exactOccurrenceSide(ref.position, semanticFen) &&
            PrincipalVariationEvidence.sameBoardState(ref.position.fen, semanticFen)
        case (
              EvidenceConfidence.LegalReplayVerified,
              RelationEvidenceOrigin.LegalReplay(
                semanticStartFen,
                semanticAfterFen,
                proofMoves
              )
            ) =>
          payload.hasLineProof &&
            exactOccurrenceSide(ref.position, semanticStartFen) &&
            PrincipalVariationEvidence.sameBoardState(ref.position.fen, semanticStartFen) &&
            semanticAfterFen.trim.nonEmpty &&
            payload.lineMoves == proofMoves
        case _ =>
          false
      )

  private[chessjudgment] def from(
      detail: RelationWitnessDetail,
      lineMoves: List[String]
  ): RelationFactEvidence =
    new RelationFactEvidence(
      canonicalFact = CanonicalRelationFact.from(detail, lineMoves),
      origin = RelationEvidenceOrigin.Unverified,
      rootTransitionFootprint = None
    )

  private[chessjudgment] def certifiedFromCanonicalPositionFacts(
      facts: List[CanonicalRelationFact],
      position: Position
  ): List[RelationFactEvidence] =
    val writtenFen = Fen.write(position).value
    val semanticFen = PrincipalVariationEvidence
      .semanticBoardStateFen(writtenFen)
      .getOrElse(writtenFen)
    val origin = RelationEvidenceOrigin.PositionSnapshot(semanticFen)
    facts.map(fact => new RelationFactEvidence(fact, origin, None))

  /** The tactical detector is the sole producer of replay-derived relation
    * details. Raw callers cannot turn an arbitrary label into legal-replay
    * authority by attaching a matching move list.
    */
  private[chessjudgment] def certifiedTacticalBatch(
      unverified: List[RelationFactEvidence],
      transition: CanonicalReplayTransition
  ): Option[List[RelationFactEvidence]] =
    for
      certification <- legalReplayCertification(transition)
      if unverified.forall(relation =>
        relation.origin == RelationEvidenceOrigin.Unverified &&
          tacticalReplayDetail(relation.detail) &&
          relation.rootTransitionConnected(transition.boardFootprint)
      )
    yield unverified.map(relation => certification.certify(relation.detail))

  private[chessjudgment] def certifiedRootAfterProjections(
      sources: List[RelationFactEvidence],
      transition: CanonicalReplayTransition
  ): Option[List[RelationFactEvidence]] =
    for
      certification <- legalReplayCertification(transition)
      if sources.forall(source =>
        source.isPositionRelation &&
          source.rootTransitionConnected(transition.boardFootprint) &&
          (source.origin match
            case RelationEvidenceOrigin.PositionSnapshot(snapshotFen) =>
              PrincipalVariationEvidence.sameBoardState(snapshotFen, certification.semanticAfterFen)
            case _ => false)
      )
    yield sources.map(source => certification.certify(source.detail))

  private def tacticalReplayDetail(detail: RelationWitnessDetail): Boolean =
    detail match
      case combined @ (_: RelationWitnessDetail.DoubleCheck | _: RelationWitnessDetail.GeometricSupporterCapture |
          _: RelationWitnessDetail.GeometricControlSetDelta | _: RelationWitnessDetail.GeometricSupportDelta |
          _: RelationWitnessDetail.SliderControlInterference |
          _: RelationWitnessDetail.GeometricLineControlAfterBlockerRemoval |
          _: RelationWitnessDetail.CheckingEnemyControlBundle) =>
        RelationWitnessDetail.validCombinationProof(combined)
      case _ =>
        false

  private final case class LegalReplayCertification(
      semanticStartFen: String,
      semanticAfterFen: String,
      proofMoves: List[String],
      footprint: BoardTransitionFootprint
  ):
    private val origin = RelationEvidenceOrigin.LegalReplay(
      semanticStartFen,
      semanticAfterFen,
      proofMoves
    )

    def certify(detail: RelationWitnessDetail): RelationFactEvidence =
      new RelationFactEvidence(
        canonicalFact = CanonicalRelationFact.from(detail, proofMoves),
        origin = origin,
        rootTransitionFootprint = Some(footprint)
      )

  private def legalReplayCertification(
      transition: CanonicalReplayTransition
  ): Option[LegalReplayCertification] =
    val first = transition.legal
    for
      semanticFen <- PrincipalVariationEvidence.semanticBoardStateFen(Fen.write(first.before).value)
      semanticAfterFen <- PrincipalVariationEvidence.semanticBoardStateFen(Fen.write(first.after).value)
      proofMove = EvidenceRef.normalizeMove(first.uci)
      if proofMove.nonEmpty
    yield LegalReplayCertification(
      semanticFen,
      semanticAfterFen,
      List(proofMove),
      transition.boardFootprint
    )

  private[chessjudgment] def record(
      id: String,
      payload: RelationFactEvidence,
      position: PositionNodeRef,
      line: Option[LineNodeRef],
      scope: EvidenceScope,
      confidence: EvidenceConfidence,
      parents: List[EvidenceRef] = Nil
  ): EvidenceRecord =
    EvidenceRecord(
      ref = EvidenceRef(
        id = id,
        producer = EvidenceProducer.RelationProducer,
        layer = EvidenceLayer.Relation,
        position = position,
        line = line,
        scope = scope,
        confidence = confidence
      ),
      payload = payload,
      parents = parents
    )

enum TacticalMechanismKind:
  case KingForcing
  case MaterialGain
  case RecaptureChoice
  case Tempo
  case Refutation
  case DrawResource
  case PawnPromotion
  case DefensiveResource

object TacticalMechanismKind:
  def fromRelation(relation: RelationFactEvidence): Option[TacticalMechanismKind] =
    relation.detail match
      case _: RelationWitnessDetail.DoubleCheck | _: RelationWitnessDetail.CheckingEnemyControlBundle =>
        Some(TacticalMechanismKind.KingForcing)
      case RelationWitnessDetail.GeometricControlSetDelta(
            mover,
            controllingSide,
            _,
            _,
            RelationControlTarget.Enemy(role),
            _,
            _,
            _,
            establishedControllers,
            _
          ) if controllingSide == mover.side && role.name.equalsIgnoreCase(King.name) && establishedControllers.nonEmpty =>
        Some(TacticalMechanismKind.KingForcing)
      case RelationWitnessDetail.GeometricLineControlAfterBlockerRemoval(
            mover,
            controllerSide,
            _,
            _,
            _,
            _,
            _,
            _,
            RelationControlTarget.Enemy(role),
            _,
            _,
            _
          ) if controllerSide == mover.side && role.name.equalsIgnoreCase(King.name) =>
        Some(TacticalMechanismKind.KingForcing)
      case _ => None

  def fromLineConsequence(kind: LineConsequenceKind, rootRecapture: Boolean): List[TacticalMechanismKind] =
    kind match
      case LineConsequenceKind.MaterialGain =>
        List(TacticalMechanismKind.MaterialGain)
      case LineConsequenceKind.MaterialLoss =>
        Nil
      case LineConsequenceKind.RecaptureSequence | LineConsequenceKind.RecoveryWindow =>
        Option.when(rootRecapture)(TacticalMechanismKind.RecaptureChoice).toList
      case LineConsequenceKind.ImmediateReplyCheck =>
        List(TacticalMechanismKind.Tempo)
      case LineConsequenceKind.Mate =>
        List(TacticalMechanismKind.KingForcing)
      case LineConsequenceKind.DrawResource =>
        List(TacticalMechanismKind.DrawResource)
      case LineConsequenceKind.Promotion | LineConsequenceKind.PromotionRace =>
        List(TacticalMechanismKind.PawnPromotion)
      case LineConsequenceKind.ForcedTheme | LineConsequenceKind.Sacrifice =>
        Nil

  def relativeCauseKind(
      kind: TacticalMechanismKind,
      badLoss: Boolean,
      playedCandidate: Boolean
  ): RelativeCauseKind =
    kind match
      case TacticalMechanismKind.KingForcing =>
        RelativeCauseKind.KingForcing
      case TacticalMechanismKind.RecaptureChoice =>
        if badLoss then
          if playedCandidate then RelativeCauseKind.TacticalRefutationOfPlayed
          else RelativeCauseKind.CandidateTacticalLiability
        else RelativeCauseKind.RecaptureRecoveryWindow
      case TacticalMechanismKind.Tempo =>
        if badLoss then RelativeCauseKind.TempoLoss else RelativeCauseKind.WrongMoveOrder
      case TacticalMechanismKind.DrawResource =>
        RelativeCauseKind.DrawResource
      case TacticalMechanismKind.DefensiveResource =>
        RelativeCauseKind.DefensiveResource
      case TacticalMechanismKind.MaterialGain | TacticalMechanismKind.Refutation |
          TacticalMechanismKind.PawnPromotion =>
        if badLoss then
          if playedCandidate then RelativeCauseKind.TacticalRefutationOfPlayed
          else RelativeCauseKind.CandidateTacticalLiability
        else RelativeCauseKind.MissedTacticalResource

enum TacticalMechanismSignalKind:
  case Relation
  case LineConsequence
  case LineEvent
  case MateBranch

final case class TacticalMechanismSignal(
    kind: TacticalMechanismSignalKind,
    label: String,
    sourceLayer: EvidenceLayer,
    source: Option[EvidenceRef] = None,
    relationKind: Option[RelationFactKind] = None
)

final case class TacticalMechanismEvidence(
    kind: TacticalMechanismKind,
    moveUci: Option[String],
    line: Option[LineNodeRef],
    signals: List[TacticalMechanismSignal]
) extends EvidencePayload:
  def signalKinds: Set[TacticalMechanismSignalKind] =
    signals.map(_.kind).toSet
  def hasLineProof: Boolean =
    signalKinds.exists(kind =>
      kind == TacticalMechanismSignalKind.LineConsequence ||
        kind == TacticalMechanismSignalKind.LineEvent ||
        kind == TacticalMechanismSignalKind.MateBranch
    )
  private[chessjudgment] def lineConsequenceSourceLabelsByEvidenceId: Map[String, Set[String]] =
    signals.collect {
      case signal
          if signal.kind == TacticalMechanismSignalKind.LineConsequence &&
            signal.source.exists(_.layer == EvidenceLayer.Line) =>
        signal.source.get.id -> signal.label
    }.groupMap(_._1)(_._2).view.mapValues(_.toSet).toMap
  def hasConcreteProof: Boolean =
    signals.nonEmpty && hasLineProof
  def hasEngineOrForcingProof: Boolean =
    signalKinds.exists(kind =>
      kind == TacticalMechanismSignalKind.MateBranch ||
        kind == TacticalMechanismSignalKind.LineConsequence
    ) || (
      kind == TacticalMechanismKind.KingForcing &&
        signalKinds.contains(TacticalMechanismSignalKind.Relation) &&
        signalKinds.contains(TacticalMechanismSignalKind.LineEvent)
    )
  def tactical: Boolean =
    kind != TacticalMechanismKind.DefensiveResource &&
      kind != TacticalMechanismKind.DrawResource
  def defensive: Boolean =
    kind == TacticalMechanismKind.DefensiveResource ||
      kind == TacticalMechanismKind.DrawResource
  def canAnchorTacticalClaim: Boolean =
    tactical && hasConcreteProof && hasEngineOrForcingProof
  def canAnchorDefensiveClaim: Boolean =
    defensive && hasLineProof

/** Exact vertical bridge from a closed relation occurrence to the certified
  * root line event that gives the relation commentary authority. Merely
  * sharing a move or a line is insufficient.
  */
private[chessjudgment] final case class TacticalRelationLineContract private (
    relationNode: CanonicalRelationNode,
    lineRecord: EvidenceRecord,
    event: LineMoveEvent
):
  val kind: TacticalMechanismKind = TacticalMechanismKind.KingForcing
  val signals: List[TacticalMechanismSignal] =
    List(
      TacticalMechanismSignal(
        TacticalMechanismSignalKind.Relation,
        relationNode.relation.detail.detailName,
        EvidenceLayer.Relation,
        Some(relationNode.ref),
        Some(relationNode.relation.kind)
      ),
      TacticalMechanismSignal(
        TacticalMechanismSignalKind.LineEvent,
        event.kind.toString,
        EvidenceLayer.Line,
        Some(lineRecord.ref)
      )
    )

private[chessjudgment] object TacticalRelationLineContract:
  def bindings(
      graph: TypedEvidenceGraph,
      node: CanonicalRelationNode,
      rootMove: String
  ): List[TacticalRelationLineContract] =
    val normalizedRoot = EvidenceRef.normalizeMove(rootMove)
    val carrier = node.record.parents.flatMap(parent => graph.byId.get(parent.id).filter(_.ref == parent)).collect {
      case record @ EvidenceRecord(_, occurrence: ClosedRelationOccurrenceEvidence, _)
          if occurrence.outputFor(node.ref).exists(binding => binding.relation == node.relation) =>
        record -> occurrence
    } match
      case exact :: Nil => Some(exact)
      case _            => None

    (for
      (carrierRecord, occurrence) <- carrier.toList
      if carrierRecord.ref.position == node.ref.position
      if EvidenceRef.sameMove(occurrence.edge.moveUci, normalizedRoot)
      (mover, kingSquare) <- forcingKingTarget(node.relation).toList
      lineRef <- occurrence.lineEvidence.toList
      lineRecord <- graph.byId.get(lineRef.id).toList
      lineFact <- lineRecord.payload match
        case value: LineFactEvidence => List(value)
        case _                       => Nil
      if lineRecord.ref == lineRef
      if lineRecord.ref.producer == EvidenceProducer.LegalLineProducer
      if lineRecord.ref.layer == EvidenceLayer.Line
      if lineRecord.ref.confidence == EvidenceConfidence.LegalReplayVerified
      if lineRecord.ref.position == occurrence.edge.from
      if lineRecord.ref.line == occurrence.lineOwner && occurrence.lineOwner.contains(lineFact.line)
      if lineFact.replayIsCertified && lineFact.rootMove.exists(EvidenceRef.sameMove(_, normalizedRoot))
      event <- List(LineEventKind.Check, LineEventKind.Mate).flatMap(lineFact.lineEventsOf)
      if event.plyOffset == 0 && EvidenceRef.sameMove(event.moveUci, normalizedRoot)
      if event.side.contains(mover.side)
      if event.targetRole.exists(_.name.equalsIgnoreCase(King.name))
      if event.square.contains(kingSquare)
    yield TacticalRelationLineContract(node, lineRecord, event))
      .sortBy(binding => binding.event.kind.toString)

  def certifies(
      graph: TypedEvidenceGraph,
      payload: TacticalMechanismEvidence
  ): Boolean =
    payload.signals.collect {
      case signal if signal.kind == TacticalMechanismSignalKind.Relation => signal.source
    }.flatten match
      case relationRef :: Nil =>
        graph.relationGraph.byEvidenceId.get(relationRef.id).filter(_.ref == relationRef).exists { node =>
          payload.moveUci.exists(rootMove =>
            bindings(graph, node, rootMove).exists(binding =>
              binding.kind == payload.kind && binding.signals == payload.signals
            )
          )
        }
      case _ => false

  private def forcingKingTarget(
      relation: RelationFactEvidence
  ): Option[(RelationMoveTransitionWitness, EvidenceSquare)] =
    relation.detail match
      case RelationWitnessDetail.DoubleCheck(mover, kingSquare, checkers, _)
          if checkers.distinct.size >= 2 =>
        Some(mover -> kingSquare)
      case RelationWitnessDetail.CheckingEnemyControlBundle(mover, kingControls, otherEnemyControls, _)
          if kingControls.nonEmpty && otherEnemyControls.nonEmpty =>
        kingControls
          .filter(_.targetRole.name.equalsIgnoreCase(King.name))
          .map(_.targetSquare)
          .distinct match
          case kingSquare :: Nil => Some(mover -> kingSquare)
          case _                 => None
      case RelationWitnessDetail.GeometricControlSetDelta(
            mover,
            controllingSide,
            kingSquare,
            _,
            RelationControlTarget.Enemy(role),
            _,
            _,
            _,
            establishedControllers,
            _
          ) if controllingSide == mover.side && role.name.equalsIgnoreCase(King.name) && establishedControllers.nonEmpty =>
        Some(mover -> kingSquare)
      case RelationWitnessDetail.GeometricLineControlAfterBlockerRemoval(
            mover,
            controllerSide,
            _,
            _,
            _,
            _,
            _,
            kingSquare,
            RelationControlTarget.Enemy(role),
            _,
            _,
            _
          ) if controllerSide == mover.side && role.name.equalsIgnoreCase(King.name) =>
        Some(mover -> kingSquare)
      case _ => None

final case class StructuralDeltaEvidence(
    transition: StructuralTransitionBinding,
    signals: List[StructuralSignal],
    consequences: List[TransitionConsequence],
    private[chessjudgment] val relationChanges: List[CanonicalRelationChange],
    private[chessjudgment] val canonicalTransitionProof: Option[CanonicalTransitionProof],
    private[chessjudgment] val canonicalDeltaProof: Option[CanonicalTransitionDeltaProof]
) extends EvidencePayload:
  import TransitionConsequenceKind.*

  private lazy val certifiedTransitionProof: Option[CanonicalTransitionProof] =
    canonicalTransitionProof.filter(_.proves(transition))

  private[chessjudgment] lazy val exactOutputInventoryCertified: Boolean =
    canonicalDeltaProof.exists(_.proves(this))

  def moveUci: String = transition.moveUci
  def role: TransitionEdgeRole = transition.role
  def from: PositionNodeRef = transition.from
  def to: PositionNodeRef = transition.to
  def line: Option[LineNodeRef] = transition.line
  def perspective: Color = transition.perspective
  def hasSignals: Boolean = signals.nonEmpty
  def hasConsequences: Boolean = consequences.nonEmpty
  private[chessjudgment] def hasRelationChanges: Boolean = relationChanges.nonEmpty
  def hasTypedOutput: Boolean = hasSignals || hasConsequences || hasRelationChanges
  def signalAnchors: List[String] = signals.map(_.anchorKey).distinct
  def consequenceAnchors: List[String] = consequences.map(_.anchorKey).distinct
  def consequencesOf(kind: TransitionConsequenceKind): List[TransitionConsequence] = consequences.filter(_.kind == kind)
  def hasConsequence(kind: TransitionConsequenceKind): Boolean = consequences.exists(_.kind == kind)
  def hasAnyConsequence(kinds: Set[TransitionConsequenceKind]): Boolean =
    consequences.exists(consequence => kinds.contains(consequence.kind))
  def hasConsequenceCategory(category: TransitionConsequenceCategory): Boolean =
    consequences.exists(consequence => StructuralDeltaEvidence.hasConsequenceCategory(consequence.kind, category))
  def establishedConsequences: List[TransitionConsequence] =
    consequences.filter(_.establishesState)
  def removedConsequences: List[TransitionConsequence] =
    consequences.filter(_.removesState)
  private[chessjudgment] def transitionIsCertified: Boolean =
    certifiedTransitionProof.nonEmpty
  private[chessjudgment] def certifiedRootStep: Option[LegalReplayStep] =
    certifiedTransitionProof.map(_.rootStep)
  private[chessjudgment] def certifiedRootResponseCount(maximum: Int): Option[Int] =
    certifiedTransitionProof.map(_.legalResponseCount(maximum))
object StructuralDeltaEvidence:
  import TransitionConsequenceKind.*
  import TransitionConsequenceCategory.*

  def hasConsequenceCategory(kind: TransitionConsequenceKind, category: TransitionConsequenceCategory): Boolean =
    consequenceCategories.getOrElse(kind, Set.empty).contains(category)

  private[chessjudgment] def validOpponentMobilityRestrictionSubject(subject: StructuralSubject): Boolean =
    subject.isInstanceOf[StructuralSubject.OpponentResourceDeterred]

  private[chessjudgment] def restrictedOpponentEntry(
      subject: StructuralSubject
  ): Option[(EvidencePieceRole, EvidenceSquare, EvidenceSquare)] =
    subject match
      case StructuralSubject.OpponentResourceDeterred(role, from, to) => Some((role, from, to))
      case _                                                          => None
  private lazy val consequenceCategories: Map[TransitionConsequenceKind, Set[TransitionConsequenceCategory]] =
    Map(
      OpenFileEstablished -> Set(PawnStructure, PawnStructureDelta),
      SemiOpenFileEstablished -> Set(PawnStructure, PawnStructureDelta),
      FileOccupationEstablished -> Set(PieceActivity),
      PawnTensionCreated -> Set(PawnStructure, PawnStructureDelta),
      PawnTensionResolution -> Set(PawnStructureDelta),
      PassedPawnProgress -> Set(PawnStructure, PawnStructureDelta),
      PassedPawnConcession -> Set(PawnStructure, PawnStructureDelta),
      BatteryFormation -> Set(PieceActivity),
      OpponentMobilityRestriction -> Set.empty
    )

final case class PlanTransitionEvidence(
    proof: PlanSequenceProof
) extends EvidencePayload

enum PlanCausalDependencyKind:
  case ObjectStatePrecondition
  case LineAccessPrecondition
  case ResponseContinuationPrecondition

enum PlanCausalDependencyProof:
  case ObjectState(trajectory: LineObjectTrajectory)
  case LineAccess(trajectory: LineAccessTrajectory)
  case ResponseContinuation(trajectory: PlanResponseContinuationTrajectory)

final case class PlanCausalEventNode(
    identity: PlanEventIdentity,
    step: LineReplayStep,
    perspective: Color,
    structuralConsequences: List[TransitionConsequence],
    private[chessjudgment] val canonicalStep: Option[LegalReplayStep] = None
):
  def moveUci: String = EvidenceRef.normalizeMove(step.moveUci)
  private val moveOrigin = moveUci.take(2)
  private val moveDestination = moveUci.slice(2, 4)
  require(
    moveUci.length >= 4 &&
      EvidenceRef.sameMove(identity.rootMove, moveUci) &&
      identity.actorFrom.contains(moveOrigin) &&
      identity.actorTo.contains(moveDestination),
    "plan-causal event identity must be derived from the event replay step"
  )
  private[chessjudgment] def certifiedLegalStep: Option[LegalReplayStep] =
    canonicalStep.filter(legal =>
      legal.ply == step.ply &&
        EvidenceRef.sameMove(legal.uci, step.moveUci) &&
        PrincipalVariationEvidence.sameBoardState(Fen.write(legal.before).value, step.fenBefore) &&
        PrincipalVariationEvidence.sameBoardState(Fen.write(legal.after).value, step.fenAfter) &&
        legal.move.piece.color == perspective
    )

final case class PlanCausalEventDependency(
    from: PlanCausalEventNode,
    to: PlanCausalEventNode,
    kind: PlanCausalDependencyKind,
    proof: PlanCausalDependencyProof,
    plyOffset: Int
):
  private[chessjudgment] def stableKey: String =
    PlanCausalDependencyOccurrenceIdentity.from(this).stableKey

  def planConnectionProven: Boolean =
    from.step.ply < to.step.ply &&
      plyOffset == to.step.ply - from.step.ply &&
      ((kind, proof) match
        case (PlanCausalDependencyKind.ObjectStatePrecondition, PlanCausalDependencyProof.ObjectState(trajectory)) =>
          trajectory.rootStep == from.step &&
            trajectory.futureStep == to.step &&
            trajectory.plyOffset == plyOffset
        case (PlanCausalDependencyKind.LineAccessPrecondition, PlanCausalDependencyProof.LineAccess(trajectory)) =>
          trajectory.enablingStep == from.step &&
            trajectory.enabledStep == to.step &&
            trajectory.plyOffset == plyOffset
        case (
              PlanCausalDependencyKind.ResponseContinuationPrecondition,
              PlanCausalDependencyProof.ResponseContinuation(trajectory)
            ) =>
          trajectory.triggerStep == from.step &&
            trajectory.followUpStep == to.step &&
            trajectory.plyOffset == plyOffset
        case _ =>
          false
      )
  def enablesContinuation: Boolean =
    planConnectionProven &&
      (kind == PlanCausalDependencyKind.ObjectStatePrecondition ||
        kind == PlanCausalDependencyKind.LineAccessPrecondition ||
        kind == PlanCausalDependencyKind.ResponseContinuationPrecondition)
  def proofSquares: List[EvidenceSquare] =
    proof match
      case PlanCausalDependencyProof.ObjectState(trajectory) =>
        List(trajectory.rootTo)
      case PlanCausalDependencyProof.LineAccess(trajectory) =>
        List(trajectory.vacatedSquare)
      case PlanCausalDependencyProof.ResponseContinuation(trajectory) =>
        List(
          trajectory.replyFrom,
          trajectory.replyTo,
          trajectory.followUpFrom,
          trajectory.followUpTo
        ) ++ (trajectory match
          case pawn: PawnBreakFollowUpTrajectory => List(pawn.releasedPassedPawn)
          case exchange: ExchangeConversionTrajectory =>
            List(exchange.convertingPawnAtPhaseBoundary)
          case _ => Nil)
  def proofPieceRoles: List[EvidencePieceRole] =
    proof match
      case PlanCausalDependencyProof.ObjectState(trajectory) =>
        List(trajectory.pieceRole)
      case PlanCausalDependencyProof.LineAccess(trajectory) =>
        List(trajectory.enabledPieceRole)
      case PlanCausalDependencyProof.ResponseContinuation(trajectory) =>
        trajectory.involvedRoles
  def preparedPawnAdvanceFile: Option[String] =
    val pawnAdvanceSquares = proof match
      case PlanCausalDependencyProof.LineAccess(trajectory)
          if trajectory.enabledPieceRole.name.equalsIgnoreCase(Pawn.toString) =>
        Some(trajectory.enabledFrom -> trajectory.enabledTo)
      case _ => None
    pawnAdvanceSquares.collect {
      case (from, to) if from.key.headOption == to.key.headOption => from.key.take(1)
    }

private[chessjudgment] final case class PlanCausalResponseProof private (
    triggerStep: LineReplayStep,
    responseStep: LineReplayStep,
    capturesPlanPiece: Boolean,
    movesPressuredPiece: Boolean,
    answersCheck: Boolean,
    legalResponseStep: LegalReplayStep,
    beforeResponseAnalysis: PositionAnalysis,
    afterResponseAnalysis: PositionAnalysis
):
  def proves(trigger: PlanCausalEventNode, step: LineReplayStep, plyOffset: Int): Boolean =
    trigger.step == triggerStep &&
      step == responseStep &&
      step.ply > trigger.step.ply &&
      plyOffset == step.ply - trigger.step.ply &&
      beforeResponseAnalysis.features.plyCount == step.ply - 1 &&
      afterResponseAnalysis.features.plyCount == step.ply &&
      PrincipalVariationEvidence.sameBoardState(beforeResponseAnalysis.features.fen, step.fenBefore) &&
      PrincipalVariationEvidence.sameBoardState(afterResponseAnalysis.features.fen, step.fenAfter) &&
      (capturesPlanPiece || movesPressuredPiece || answersCheck)

private[chessjudgment] object PlanCausalResponseProof:
  def from(
      trigger: PlanCausalEventNode,
      response: LineReplayStep,
      legalResponse: LegalReplayStep,
      beforeResponseAnalysis: PositionAnalysis,
      afterResponseAnalysis: PositionAnalysis
  ): Option[PlanCausalResponseProof] =
    val responseMove = EvidenceRef.normalizeMove(response.moveUci)
    for
      triggerLegal <- trigger.certifiedLegalStep
      planSquare <- Square.fromKey(trigger.moveUci.slice(2, 4))
      responseFrom <- Square.fromKey(responseMove.take(2))
      responseTo <- Square.fromKey(responseMove.slice(2, 4))
      planPiece <- triggerLegal.after.board.pieceAt(planSquare)
      if planPiece.color == trigger.perspective
      if legalResponse.ply == response.ply
      if EvidenceRef.sameMove(legalResponse.uci, response.moveUci)
      if PrincipalVariationEvidence.sameBoardState(Fen.write(legalResponse.before).value, response.fenBefore)
      if PrincipalVariationEvidence.sameBoardState(Fen.write(legalResponse.after).value, response.fenAfter)
      if legalResponse.before.color == !trigger.perspective
      if legalResponse.before.board.pieceAt(planSquare).contains(planPiece)
      responder <- legalResponse.before.board.pieceAt(responseFrom)
      if responder.color == !trigger.perspective
      if legalResponse.after.board.pieceAt(responseTo).contains(responder)
      if beforeResponseAnalysis.features.plyCount == legalResponse.ply - 1
      if afterResponseAnalysis.features.plyCount == legalResponse.ply
      if PrincipalVariationEvidence.sameBoardState(
        beforeResponseAnalysis.features.fen,
        Fen.write(legalResponse.before).value
      )
      if PrincipalVariationEvidence.sameBoardState(
        afterResponseAnalysis.features.fen,
        Fen.write(legalResponse.after).value
      )
      pressureOrigins = pressureOriginSquares(trigger, triggerLegal.after)
      captures = responseTo == planSquare && legalResponse.after.board.pieceAt(planSquare).contains(responder)
      movesPressured = pressureOrigins(responseFrom.key.toLowerCase)
      answers =
        response.ply == trigger.step.ply + 1 &&
          PrincipalVariationEvidence.sameBoardState(trigger.step.fenAfter, response.fenBefore) &&
          legalResponse.before.check.yes &&
          responder.role == King &&
          !legalResponse.after.check.yes
      proof = PlanCausalResponseProof(
        trigger.step,
        response,
        captures,
        movesPressured,
        answers,
        legalResponse,
        beforeResponseAnalysis,
        afterResponseAnalysis
      )
      if proof.proves(trigger, response, response.ply - trigger.step.ply)
    yield proof

  private def pressureOriginSquares(
      trigger: PlanCausalEventNode,
      afterTrigger: Position
  ): Set[String] =
    val tensionPawns = trigger.structuralConsequences
      .filter(_.kind == TransitionConsequenceKind.PawnTensionCreated)
      .flatMap(PlanCausalEpisode.consequenceSquares)
      .filter(square =>
        Square.fromKey(square.key).flatMap(afterTrigger.board.pieceAt).exists(piece =>
          piece.color == !trigger.perspective && piece.role == Pawn
        )
      )
      .map(_.key.toLowerCase)
    PlanCausalEpisode.pressureTargetSquares(trigger) ++ tensionPawns

final case class PlanCausalResponse private[chessjudgment] (
    trigger: PlanCausalEventNode,
    step: LineReplayStep,
    plyOffset: Int,
    structuralConsequences: List[TransitionConsequence] = Nil,
    private[chessjudgment] val certificate: Option[PlanCausalResponseProof] = None
):
  def capturesPlanPiece: Boolean =
    certificate.exists(_.capturesPlanPiece)
  def movesPressuredPiece: Boolean =
    certificate.exists(_.movesPressuredPiece)
  def answersCheck: Boolean =
    certificate.exists(_.answersCheck)
  def proven: Boolean =
    certificate.exists(_.proves(trigger, step, plyOffset))
  private[chessjudgment] def certifiedLegalStep: Option[LegalReplayStep] =
    certificate.filter(_.proves(trigger, step, plyOffset)).map(_.legalResponseStep)

object PlanCausalResponse:
  private[chessjudgment] def certified(
      trigger: PlanCausalEventNode,
      step: LineReplayStep,
      legalStep: LegalReplayStep,
      beforeAnalysis: PositionAnalysis,
      afterAnalysis: PositionAnalysis
  ): Option[PlanCausalResponse] =
    PlanCausalResponseProof.from(trigger, step, legalStep, beforeAnalysis, afterAnalysis).map(proof =>
      PlanCausalResponse(
        trigger = trigger,
        step = step,
        plyOffset = step.ply - trigger.step.ply,
        certificate = Some(proof)
      )
    )

  private[chessjudgment] def planPiecePresent(
      trigger: PlanCausalEventNode,
      step: LineReplayStep,
      legalStep: Option[LegalReplayStep]
  ): Boolean =
    (for
      triggerLegal <- trigger.certifiedLegalStep
      responseLegal <- legalStep
      planSquare <- Square.fromKey(trigger.moveUci.slice(2, 4))
      actor <- triggerLegal.after.board.pieceAt(planSquare)
      if responseLegal.ply == step.ply
      if EvidenceRef.sameMove(responseLegal.uci, step.moveUci)
      if PrincipalVariationEvidence.sameBoardState(Fen.write(responseLegal.before).value, step.fenBefore)
      if responseLegal.before.board.pieceAt(planSquare).contains(actor)
    yield true).contains(true)

enum PlanCausalGoalMechanism:
  case RookFileOccupation
  case PawnTensionCreation
  case PawnTensionResolution
  case PawnPassedStatusProgress
  case PassedPawnConversion
  case PassedPawnManufacture
  case SeventhRankInvasion
  case ObjectStatePieceRoute
  case LineAccessRookFileOccupation
  case LineAccessRookBattery
  case LineAccessPieceRoute
  case ReleasedPassedPawnContinuation

final case class PlanCausalGoalFunctionIdentity(
    goalKind: PlanKind,
    mechanism: PlanCausalGoalMechanism,
    supportingDependency: Option[PlanCausalDependencyFunctionIdentity]
):
  def stableKey: String =
    PlanCausalProofKey.product(
      "plan-goal-function",
      List(
        goalKind.id,
        mechanism.toString.toLowerCase,
        PlanCausalProofKey.optional(supportingDependency.map(_.stableKey))
      )
    )

/** Typed authority that one exact transition, and optionally one exact edge,
  * establishes a named plan goal.  The companion is the only producer.
  */
final case class PlanCausalGoalProof private (
    goalKind: PlanKind,
    sourceTransition: StructuralTransitionBinding,
    consequence: TransitionConsequence,
    mechanism: PlanCausalGoalMechanism,
    supportingDependency: Option[PlanCausalEventDependency]
):
  def goalTheme: PlanTheme = goalKind.theme

  private[chessjudgment] def binds(
      sourceEvent: PlanCausalEventNode,
      exactConsequence: TransitionConsequence,
      causalPath: List[PlanCausalEventDependency]
  ): Boolean =
    consequence == exactConsequence &&
      EvidenceRef.sameMove(sourceTransition.moveUci, sourceEvent.moveUci) &&
      sourceTransition.from.ply == sourceEvent.step.ply - 1 &&
      sourceTransition.to.ply == sourceEvent.step.ply &&
      PrincipalVariationEvidence.sameBoardState(sourceTransition.from.fen, sourceEvent.step.fenBefore) &&
      PrincipalVariationEvidence.sameBoardState(sourceTransition.to.fen, sourceEvent.step.fenAfter) &&
      sourceTransition.perspective == sourceEvent.perspective &&
      sourceTransition.actorRole.map(_.name.toLowerCase) == sourceEvent.identity.actorRole.map(_.toLowerCase) &&
      supportingDependency.forall(causalPath.contains)

  private[chessjudgment] def functionIdentity(
      root: PlanCausalEventNode
  ): PlanCausalGoalFunctionIdentity =
    PlanCausalGoalFunctionIdentity(
      goalKind,
      mechanism,
      supportingDependency.map(PlanCausalDependencyFunctionIdentity.from(root, _))
    )

  private[chessjudgment] def withSupportingDependency(
      dependency: Option[PlanCausalEventDependency]
  ): PlanCausalGoalProof =
    copy(supportingDependency = dependency)

  private[chessjudgment] def stableKey: String =
    PlanCausalProofKey.product(
      "plan-goal-proof",
      List(
        goalKind.id,
        mechanism.toString.toLowerCase,
        sourceTransition.moveUci,
        sourceTransition.from.ply.toString,
        PrincipalVariationEvidence.normalizeFen(sourceTransition.from.fen),
        sourceTransition.to.ply.toString,
        PrincipalVariationEvidence.normalizeFen(sourceTransition.to.fen),
        consequence.stableKey,
        PlanCausalProofKey.optional(supportingDependency.map(_.stableKey))
      )
    )

/** One result occurrence remains attached to the exact causal path and typed
  * goal mechanism that produced it.  It is never reconstructed as a later
  * path/result Cartesian product.
  */
final case class PlanCausalResultRoute private (
    sourceEvent: PlanCausalEventNode,
    consequence: TransitionConsequence,
    causalPath: List[PlanCausalEventDependency],
    goalProof: PlanCausalGoalProof
):
  private[chessjudgment] def stableKey: String =
    PlanCausalProofKey.product(
      "plan-result-route",
      List(
        PlanEventOccurrence.from(
          sourceEvent.identity,
          sourceEvent.moveUci,
          sourceEvent.step.ply,
          sourceEvent.step.fenBefore,
          sourceEvent.step.fenAfter
        ).stableKey,
        consequence.stableKey,
        PlanCausalProofKey.sequence(causalPath.map(_.stableKey)),
        goalProof.stableKey
      )
    )

  private[chessjudgment] def withMappedEvents(
      resolve: PlanCausalEventNode => PlanCausalEventNode
  ): PlanCausalResultRoute =
    val remappedPath = causalPath.map(dependency =>
      dependency.copy(from = resolve(dependency.from), to = resolve(dependency.to))
    )
    val remappedSupport = goalProof.supportingDependency.flatMap(original =>
      causalPath.zip(remappedPath).collectFirst { case (before, after) if before == original => after }
    )
    PlanCausalResultRoute(
      sourceEvent = resolve(sourceEvent),
      consequence = consequence,
      causalPath = remappedPath,
      goalProof = goalProof.withSupportingDependency(remappedSupport)
    )

object PlanCausalResultRoute:
  private[chessjudgment] def certified(
      sourceEvent: PlanCausalEventNode,
      consequence: TransitionConsequence,
      causalPath: List[PlanCausalEventDependency],
      goalProof: PlanCausalGoalProof
  ): Option[PlanCausalResultRoute] =
    val connected = causalPath.zip(causalPath.drop(1)).forall { case (current, next) =>
      current.to == next.from
    }
    Option.when(
      causalPath.nonEmpty &&
        causalPath.distinct.size == causalPath.size &&
        causalPath.forall(_.enablesContinuation) &&
        causalPath.last.to == sourceEvent &&
        connected &&
        consequence.establishesState &&
        consequence.strength > 0 &&
        goalProof.binds(sourceEvent, consequence, causalPath)
    )(
      PlanCausalResultRoute(sourceEvent, consequence, causalPath, goalProof)
    )

final case class PlanCausalEpisode(
    root: PlanCausalEventNode,
    continuations: List[PlanCausalEventNode],
    dependencies: List[PlanCausalEventDependency],
    responses: List[PlanCausalResponse],
    antecedents: List[PlanCausalEventNode] = Nil,
    resultRoutes: List[PlanCausalResultRoute] = Nil
):
  require(resultRoutes.distinct.size == resultRoutes.size, "duplicate exact plan-result routes")
  require(
    resultRoutes.forall(route =>
      route.causalPath.headOption.exists(_.from == root) &&
        continuations.contains(route.sourceEvent) &&
        route.causalPath.forall(dependencies.contains)
    ),
    "plan-result routes must belong to this episode and begin at its root"
  )
  def withRootIdentity(identity: PlanEventIdentity): PlanCausalEpisode =
    if root.identity == identity then this
    else
      val previousRoot = root
      val resolvedRoot = root.copy(identity = identity)
      def resolve(event: PlanCausalEventNode): PlanCausalEventNode =
        if event == previousRoot then resolvedRoot else event
      copy(
        root = resolvedRoot,
        continuations = continuations.map(resolve),
        dependencies = dependencies.map(dependency =>
          dependency.copy(from = resolve(dependency.from), to = resolve(dependency.to))
        ),
        responses = responses.map(response => response.copy(trigger = resolve(response.trigger))),
        antecedents = antecedents.map(resolve),
        resultRoutes = resultRoutes.map(_.withMappedEvents(resolve))
      )
  lazy val antecedentSteps: List[PlanCausalEventNode] =
    antecedents.distinct.filter(_.step.ply < root.step.ply).sortBy(event => (event.step.ply, event.moveUci))
  lazy val planSteps: List[PlanCausalEventNode] =
    (root :: continuations).distinct.sortBy(event => (event.step.ply, event.moveUci))
  lazy val chronologicalSteps: List[PlanCausalEventNode] =
    (antecedentSteps ++ planSteps).distinct.sortBy(event => (event.step.ply, event.moveUci))
  def events: List[PlanCausalEventNode] = planSteps
  private lazy val futureDependencies: List[PlanCausalEventDependency] =
    dependencies.filter(dependency => planSteps.contains(dependency.from) && planSteps.contains(dependency.to))
  lazy val historyDependencies: List[PlanCausalEventDependency] =
    dependencies.filter(dependency =>
      chronologicalSteps.contains(dependency.from) &&
        chronologicalSteps.contains(dependency.to) &&
        dependency.from.step.ply < root.step.ply &&
        dependency.to.step.ply <= root.step.ply
    )
  def planSequenceProven: Boolean =
    continuations.nonEmpty &&
      futureDependencies.nonEmpty &&
      futureDependencies.forall(_.planConnectionProven) &&
      responses.forall(response => planSteps.contains(response.trigger) && response.proven) &&
      futureConnectedToRoot
  lazy val historicalDependencyPathsToRoot: List[List[PlanCausalEventDependency]] =
    val provenDependencies = historyDependencies.filter(_.enablesContinuation)
    @annotation.tailrec
    def reverseReachable(reached: Set[PlanCausalEventNode]): Set[PlanCausalEventNode] =
      val next = reached ++ provenDependencies.collect {
        case dependency if reached(dependency.to) => dependency.from
      }
      if next == reached then reached else reverseReachable(next)
    val relevantNodes = reverseReachable(Set(root))
    val relevantDependencies = provenDependencies
      .filter(dependency => relevantNodes(dependency.from) && relevantNodes(dependency.to))
      .distinct
      .sortBy(_.stableKey)
    val incomingNodes = relevantDependencies.map(_.to).toSet
    val origins = relevantDependencies
      .map(_.from)
      .distinct
      .filterNot(incomingNodes)
      .sortBy(event => (event.step.ply, event.moveUci))
    val outgoing = relevantDependencies
      .groupMap(_.from)(identity)
      .view
      .mapValues(_.sortBy(_.stableKey))
      .toMap
    val memo = scala.collection.mutable.Map.empty[PlanCausalEventNode, List[List[PlanCausalEventDependency]]]
    def suffixes(current: PlanCausalEventNode): List[List[PlanCausalEventDependency]] =
      memo.getOrElseUpdate(
        current,
        if current == root then List(Nil)
        else
          outgoing.getOrElse(current, Nil).flatMap(dependency =>
            suffixes(dependency.to).map(dependency :: _)
          )
      )
    origins
      .flatMap(suffixes)
      .filter(path => path.nonEmpty && path.last.to == root)
      .distinct
      .sortBy(_.map(_.stableKey).mkString)
  def historySequenceProven: Boolean = historicalDependencyPathsToRoot.nonEmpty
  def historicalCompletionProven: Boolean =
    historySequenceProven && PlanCausalEpisode.resultConsequences(root).exists(consequence =>
      consequence.establishesState && consequence.strength > 0
    )
  lazy val rootEnabledSteps: List[PlanCausalEventNode] =
    @annotation.tailrec
    def expand(enabled: Set[PlanCausalEventNode]): Set[PlanCausalEventNode] =
      val next = enabled ++ dependencies.collect {
        case dependency if enabled(dependency.from) && dependency.enablesContinuation => dependency.to
      }
      if next == enabled then enabled else expand(next)
    planSteps.filter(expand(Set(root)))
  def rootEnablesContinuation: Boolean = rootEnabledSteps.exists(_ != root)
  def continuationsEnabledByRoot: List[PlanCausalEventNode] = rootEnabledSteps.filterNot(_ == root)
  def causalEpisodeProven: Boolean = planSequenceProven || historySequenceProven
  def requiredPlyOffset: Int =
    resultRoutes.map(_.sourceEvent.step.ply - root.step.ply).maxOption.getOrElse(0).max(0)
  def completionProven: Boolean =
    resultRoutes.exists(_.sourceEvent != root)
  def enablingPathsTo(destination: PlanCausalEventNode): List[List[PlanCausalEventNode]] =
    enablingDependencyPathsTo(destination)
      .map(path => root :: path.map(_.to))
      .distinct

  /** Every returned path owns its exact dependency occurrences. Parallel
    * chess explanations between the same event nodes therefore remain
    * distinct instead of collapsing into one node-only path.
    */
  def enablingDependencyPathsTo(
      destination: PlanCausalEventNode
  ): List[List[PlanCausalEventDependency]] =
    val relevant = enablingDependenciesTo(destination)
    val outgoing = relevant
      .groupMap(_.from)(identity)
      .view
      .mapValues(_.distinct.sortBy(_.stableKey))
      .toMap
    val memo = scala.collection.mutable.Map.empty[PlanCausalEventNode, List[List[PlanCausalEventDependency]]]
    def suffixes(current: PlanCausalEventNode): List[List[PlanCausalEventDependency]] =
      memo.getOrElseUpdate(
        current,
        if current == destination then List(Nil)
        else
          outgoing.getOrElse(current, Nil).flatMap(dependency =>
            suffixes(dependency.to).map(dependency :: _)
          )
      )
    Option
      .when(destination == root || relevant.nonEmpty)(suffixes(root))
      .getOrElse(Nil)
      .distinct

  def enablingDependenciesTo(destination: PlanCausalEventNode): List[PlanCausalEventDependency] =
    @annotation.tailrec
    def reverseReachable(reached: Set[PlanCausalEventNode]): Set[PlanCausalEventNode] =
      val next = reached ++ futureDependencies.collect {
        case dependency if dependency.enablesContinuation && reached(dependency.to) => dependency.from
      }
      if next == reached then reached else reverseReachable(next)
    val relevantNodes = rootEnabledSteps.toSet.intersect(reverseReachable(Set(destination)))
    futureDependencies
      .filter(dependency =>
        dependency.enablesContinuation && relevantNodes(dependency.from) && relevantNodes(dependency.to)
      )
      .sortBy(_.stableKey)

  def enablingAncestorOf(
      ancestor: PlanCausalEventNode,
      destination: PlanCausalEventNode
  ): Boolean =
    ancestor == destination || enablingDependenciesTo(destination).exists(dependency =>
      dependency.from == ancestor || dependency.to == ancestor
    )

  private def futureConnectedToRoot: Boolean =
    @annotation.tailrec
    def expand(connected: Set[PlanCausalEventNode]): Set[PlanCausalEventNode] =
      val next = connected ++ futureDependencies.flatMap { dependency =>
        Option.when(dependency.enablesContinuation && connected(dependency.from))(dependency.to)
      }
      if next == connected then connected else expand(next)
    planSteps.toSet.subsetOf(expand(Set(root)))

final case class OpponentResourceComparison(
    rootMove: String,
    sourceProbeId: String,
    resourceLine: LineNodeRef
)

private[chessjudgment] final case class OpponentResourceDeterrenceCertificate private[chessjudgment] (
    sourceProbeId: String,
    resourceLine: LineNodeRef,
    comparisons: List[OpponentResourceComparison],
    materialGainPlyOffset: Int,
    rootLine: LineNodeRef,
    rootTransition: StructuralTransitionBinding,
    resourceReplay: CanonicalLineReplay,
    resourceSequence: List[LineReplayStep],
    materialGain: LineMaterialCapture,
    consequence: TransitionConsequence,
    rootImprovementWinPercent: Double,
    comparisonContrastWinPercent: Double,
    materialCounterplayPreventionProven: Boolean
):
  def ownsProof(
      expectedSourceProbeId: String,
      expectedResourceLine: LineNodeRef,
      expectedComparisons: List[OpponentResourceComparison],
      expectedMaterialGainPlyOffset: Int
  ): Boolean =
    sourceProbeId == expectedSourceProbeId &&
      resourceLine == expectedResourceLine &&
      comparisons == expectedComparisons &&
      materialGainPlyOffset == expectedMaterialGainPlyOffset

  def binds(
      expectedSourceProbeId: String,
      expectedResourceLine: LineNodeRef,
      expectedComparisons: List[OpponentResourceComparison],
      expectedMaterialGainPlyOffset: Int,
      expectedRootLine: LineNodeRef,
      expectedTransition: StructuralTransitionBinding,
      episode: PlanCausalEpisode
  ): Boolean =
    ownsProof(
      expectedSourceProbeId,
      expectedResourceLine,
      expectedComparisons,
      expectedMaterialGainPlyOffset
    ) &&
      rootLine == expectedRootLine &&
      rootTransition == expectedTransition &&
      episode.rootEnablesContinuation &&
      episode.continuationsEnabledByRoot.exists(event =>
        EvidenceRef.sameMove(event.moveUci, materialGain.moveUci)
      )

final case class OpponentResourceDeterrenceProof private[chessjudgment] (
    sourceProbeId: String,
    resourceLine: LineNodeRef,
    comparisons: List[OpponentResourceComparison],
    materialGainPlyOffset: Int,
    private[chessjudgment] val certificate: Option[OpponentResourceDeterrenceCertificate] = None
):
  require(
    materialGainPlyOffset >= 1,
    "opponent-resource proof material result must follow the resource move"
  )

  private def canonicalLine(
      ref: LineNodeRef,
      lines: List[CandidateLineNode]
  ): Option[CandidateLineNode] =
    lines.filter(_.ref == ref) match
      case line :: Nil => Some(line)
      case _           => None

  private def canonicalEvaluatedLine(
      ref: LineNodeRef,
      lines: List[CandidateLineNode],
    graph: TypedEvidenceGraph
  ): Option[CandidateLineNode] =
    for
      line <- canonicalLine(ref, lines)
      evalRecord <- graph.uniqueCandidateEvaluationRecordFor(ref)
      eval <- evalRecord.payload match
        case payload: CandidateLineEvaluationEvidence => Some(payload)
        case _                                        => None
      if evalRecord.ref.producer == EvidenceProducer.EngineEvalProducer
      if evalRecord.ref.layer == EvidenceLayer.Eval
      if evalRecord.ref.position == line.evidence.position
      if evalRecord.ref.line.contains(ref)
      if evalRecord.ref.scope == line.role.scope
      if evalRecord.parents == List(line.evidence)
      if eval.evaluation == line.evaluation
    yield line

  private def canonicalLineEvidence(
      ref: LineNodeRef,
      lines: List[CandidateLineNode],
      graph: TypedEvidenceGraph
  ): Option[(CandidateLineNode, LineFactEvidence)] =
    for
      line <- canonicalEvaluatedLine(ref, lines, graph)
      facts <- graph.certifiedLineFactFor(ref)
      record <- graph.record(line.evidence)
      if record.payload == facts
      if record.ref == line.evidence
      if record.ref.producer == EvidenceProducer.LegalLineProducer
      if record.ref.layer == EvidenceLayer.Line
      if record.ref.position == line.evidence.position
      if record.ref.line.contains(ref)
      if record.ref.scope == line.role.scope
      if facts.line == ref
      if facts.lineReplayMoves.map(EvidenceRef.normalizeMove) ==
        line.evaluation.moves.map(EvidenceRef.normalizeMove)
    yield line -> facts

  private def canonicalResourceEvidence(
      lines: List[CandidateLineNode],
      graph: TypedEvidenceGraph
  ): Option[(CandidateLineNode, LineFactEvidence)] =
    canonicalLineEvidence(resourceLine, lines, graph)

  private def canonicalWitness(
      perspective: Color,
      lines: List[CandidateLineNode],
      graph: TypedEvidenceGraph
  ): Option[(CanonicalLineReplay, List[LineReplayStep], LineMaterialCapture)] =
    for
      (_, facts) <- canonicalResourceEvidence(lines, graph)
      replay <- facts.certifiedReplay
      materialGain <- facts
        .opponentResourcePunishmentCapturesFor(perspective)
        .filter(_.plyOffset == materialGainPlyOffset) match
        case gain :: Nil => Some(gain)
        case _           => None
      resourceSequence = facts.lineReplaySteps.take(materialGainPlyOffset + 1)
      if resourceSequence.size == materialGainPlyOffset + 1
      if resourceSequence.size >= 2
      if replay.replaySteps.take(resourceSequence.size) == resourceSequence
    yield (replay, resourceSequence, materialGain)

  private[chessjudgment] def certifiedFor(
      rootLine: LineNodeRef,
      rootTransition: StructuralTransitionBinding,
      episode: PlanCausalEpisode
  ): Option[OpponentResourceDeterrenceCertificate] =
    certificate.filter(_.binds(
      sourceProbeId,
      resourceLine,
      comparisons,
      materialGainPlyOffset,
      rootLine,
      rootTransition,
      episode
    ))

  private def ownCertificate: Option[OpponentResourceDeterrenceCertificate] =
    certificate.filter(_.ownsProof(
      sourceProbeId,
      resourceLine,
      comparisons,
      materialGainPlyOffset
    ))

  private[chessjudgment] def resourceReplaySequence
      : Option[(CanonicalLineReplay, List[LineReplayStep])] =
    ownCertificate.map(certified => certified.resourceReplay -> certified.resourceSequence)

  def materialGain: Option[LineMaterialCapture] =
    ownCertificate.map(_.materialGain)

  def resourceMove: Option[String] =
    ownCertificate.flatMap(_.resourceSequence.headOption.map(_.moveUci))

  def consequence: Option[TransitionConsequence] =
    ownCertificate.map(_.consequence)

  private[chessjudgment] def certifiedComparisonMetrics: Option[(Double, Double)] =
    ownCertificate.map(certified =>
      certified.rootImprovementWinPercent -> certified.comparisonContrastWinPercent
    )

  private def canonicalEvaluationMetrics(
      rootLine: LineNodeRef,
      perspective: Color,
      lines: List[CandidateLineNode],
      graph: TypedEvidenceGraph
  ): Option[(Double, Double)] =
    val resolvedComparisons =
      comparisons.map(comparison => canonicalEvaluatedLine(comparison.resourceLine, lines, graph))
    (for
      baseline <- canonicalEvaluatedLine(rootLine, lines, graph)
      resource <- canonicalEvaluatedLine(resourceLine, lines, graph)
      comparisonLines <- Option.when(resolvedComparisons.forall(_.nonEmpty))(resolvedComparisons.flatten)
      if comparisonLines.nonEmpty
      baselineEngine <- baseline.evaluation.engineLine
      resourceEngine <- resource.evaluation.engineLine
      comparisonEngines <- Option.when(comparisonLines.forall(_.evaluation.engineLine.nonEmpty))(
        comparisonLines.flatMap(_.evaluation.engineLine)
      )
    yield
      val resourceOutcome =
        PerspectiveMath.winPercentForMover(perspective, resourceEngine.scoreCp, resourceEngine.mate)
      val rootImprovement =
        resourceOutcome - PerspectiveMath.winPercentForMover(
          perspective,
          baselineEngine.scoreCp,
          baselineEngine.mate
        )
      val bestComparisonContrast = comparisonEngines.map(line =>
        resourceOutcome - PerspectiveMath.winPercentForMover(
          perspective,
          line.scoreCp,
          line.mate
        )
      ).max
      rootImprovement -> bestComparisonContrast
    )

  private def structurallyProven(
      rootLine: LineNodeRef,
      rootTransition: StructuralTransitionBinding,
      episode: PlanCausalEpisode,
      lines: List[CandidateLineNode],
      graph: TypedEvidenceGraph,
      witness: (CanonicalLineReplay, List[LineReplayStep], LineMaterialCapture)
  ): Boolean =
    val (resourceReplay, resourceSequence, materialGain) = witness
    locally {
      val resourceStep = resourceSequence.head
      val responseStep = resourceSequence(1)
      val materialResultStep = resourceSequence.last
      def lineStartsAt(
          ref: LineNodeRef,
          expectedFen: String
      ): Boolean =
        canonicalLineEvidence(ref, lines, graph).exists { case (line, facts) =>
          facts.certifiedReplay.exists(_.replaySteps.headOption.exists(step =>
            PrincipalVariationEvidence.sameBoardState(line.evidence.position.fen, expectedFen) &&
              PrincipalVariationEvidence.sameBoardState(step.fenBefore, expectedFen) &&
              EvidenceRef.sameMove(step.moveUci, ref.rootMove)
          ))
        }
      def predecessorOwnsBranch(
          ref: LineNodeRef,
          rootMove: String,
          expectedBefore: String,
          expectedAfter: String
      ): Boolean =
        canonicalLineEvidence(ref, lines, graph).exists { case (_, facts) =>
          facts.canonicalPredecessorReplay.exists { predecessor =>
            predecessor.replaySteps match
              case step :: Nil =>
                EvidenceRef.sameMove(step.moveUci, rootMove) &&
                  PrincipalVariationEvidence.sameBoardState(step.fenBefore, expectedBefore) &&
                  PrincipalVariationEvidence.sameBoardState(step.fenAfter, expectedAfter)
              case _ => false
          }
        }
      val resource = EvidenceRef.normalizeMove(resourceStep.moveUci)
      val exactResourceMove =
        resource.length >= 4 &&
          (for
            from <- Square.fromKey(resource.take(2))
            to <- Square.fromKey(resource.slice(2, 4))
            legal <- resourceReplay.legalStep(resourceStep)
            actor <- legal.before.board.pieceAt(from)
            arrived <- legal.after.board.pieceAt(to)
          yield
            actor.color == !rootTransition.perspective &&
              legal.before.color == actor.color &&
              arrived.color == actor.color
          ).contains(true)
      val exactMaterialResult =
        EvidenceRef.sameMove(materialGain.moveUci, materialResultStep.moveUci) &&
          materialGain.side == rootTransition.perspective &&
          materialGain.square.key.equalsIgnoreCase(EvidenceRef.normalizeMove(materialResultStep.moveUci).slice(2, 4))
      val captureEnabledByRoot = episode.continuationsEnabledByRoot.exists(event =>
        EvidenceRef.sameMove(event.moveUci, materialGain.moveUci)
      )
      val exactObservedSequence =
        resourceSequence.sliding(2).forall {
          case List(previous, next) =>
            previous.ply + 1 == next.ply &&
              PrincipalVariationEvidence.sameBoardState(previous.fenAfter, next.fenBefore)
          case _ => true
        }
      val comparisonLinesBound =
        comparisons.nonEmpty &&
          comparisons.forall(comparison =>
            comparison.sourceProbeId.trim.nonEmpty &&
              comparison.resourceLine.role == LineNodeRole.Threat &&
              !EvidenceRef.sameMove(comparison.rootMove, rootLine.rootMove) &&
              EvidenceRef.sameMove(comparison.resourceLine.rootMove, resource) &&
              canonicalLineEvidence(comparison.resourceLine, lines, graph).exists { case (line, _) =>
                predecessorOwnsBranch(
                  comparison.resourceLine,
                  comparison.rootMove,
                  rootTransition.from.fen,
                  line.evidence.position.fen
                ) && lineStartsAt(comparison.resourceLine, line.evidence.position.fen)
              }
          )
      rootTransition.line.contains(rootLine) &&
        lineStartsAt(rootLine, rootTransition.from.fen) &&
        lineStartsAt(resourceLine, rootTransition.to.fen) &&
        predecessorOwnsBranch(
          resourceLine,
          rootLine.rootMove,
          rootTransition.from.fen,
          rootTransition.to.fen
        ) &&
        resourceLine.role == LineNodeRole.Threat &&
        sourceProbeId.trim.nonEmpty &&
        comparisonLinesBound &&
        PrincipalVariationEvidence.sameBoardState(resourceStep.fenBefore, rootTransition.to.fen) &&
        responseStep.ply == resourceStep.ply + 1 &&
        materialResultStep.ply >= responseStep.ply &&
        exactResourceMove &&
        exactMaterialResult &&
        exactObservedSequence &&
        captureEnabledByRoot &&
        episode.rootEnablesContinuation
    }

  private[chessjudgment] def certify(
      rootLine: LineNodeRef,
      rootTransition: StructuralTransitionBinding,
      episode: PlanCausalEpisode,
      lines: List[CandidateLineNode],
      graph: TypedEvidenceGraph
  ): Option[OpponentResourceDeterrenceProof] =
    for
      _ <- Option.when(certificate.isEmpty)(())
      witness @ (resourceReplay, resourceSequence, materialGain) <-
        canonicalWitness(rootTransition.perspective, lines, graph)
      if structurallyProven(rootLine, rootTransition, episode, lines, graph, witness)
      (rootImprovement, comparisonContrast) <-
        canonicalEvaluationMetrics(rootLine, rootTransition.perspective, lines, graph)
      if rootImprovement >= JudgmentThresholds.SIGNIFICANT_THREAT_WP
      if comparisonContrast >= JudgmentThresholds.SIGNIFICANT_THREAT_WP
      resourceStep <- resourceSequence.headOption
      legalResource <- resourceReplay.legalStep(resourceStep)
      if legalResource.move.piece.color == !rootTransition.perspective
      resourceConsequence = OpponentResourceDeterrenceProof.consequence(
        resourceStep.moveUci,
        legalResource.move.piece.role.toString.toLowerCase
      )
      preventsMaterialCounterplay =
        resourceSequence.lift(1) == resourceSequence.lastOption &&
          rootImprovement >= JudgmentThresholds.MATERIAL_THREAT_WP &&
          comparisonContrast >= JudgmentThresholds.MATERIAL_THREAT_WP
      certified = OpponentResourceDeterrenceCertificate(
        sourceProbeId = sourceProbeId,
        resourceLine = resourceLine,
        comparisons = comparisons,
        materialGainPlyOffset = materialGainPlyOffset,
        rootLine = rootLine,
        rootTransition = rootTransition,
        resourceReplay = resourceReplay,
        resourceSequence = resourceSequence,
        materialGain = materialGain,
        consequence = resourceConsequence,
        rootImprovementWinPercent = rootImprovement,
        comparisonContrastWinPercent = comparisonContrast,
        materialCounterplayPreventionProven = preventsMaterialCounterplay
      )
    yield copy(certificate = Some(certified))

object OpponentResourceDeterrenceProof:
  def consequence(resourceMove: String, resourceRole: String): TransitionConsequence =
    val normalized = EvidenceRef.normalizeMove(resourceMove)
    val role = Option(resourceRole).getOrElse("").trim.toLowerCase
    require(
      Set("pawn", "knight", "bishop", "rook", "queen", "king")(role),
      "resource deterrence requires an exact piece role"
    )
    require(normalized.matches("[a-h][1-8][a-h][1-8][qrbn]?"), "resource deterrence requires an exact legal move shape")
    TransitionConsequence(
      kind = TransitionConsequenceKind.OpponentMobilityRestriction,
      strength = 1,
      subjectBindings = List(
        StructuralSubjectBinding.unbound(
          StructuralSubject.OpponentResourceDeterred(
            EvidencePieceRole(role),
            EvidenceSquare(normalized.take(2)),
            EvidenceSquare(normalized.slice(2, 4))
          )
        )
      )
    )

object PlanCausalEpisode:
  private val PressureKinds = Set(
    TransitionConsequenceKind.BatteryFormation,
    TransitionConsequenceKind.OpponentMobilityRestriction
  )

  private val MeansOnlyResultKinds = Set.empty[TransitionConsequenceKind]

  def resultConsequences(event: PlanCausalEventNode): List[TransitionConsequence] =
    event.structuralConsequences
      .filter(_.subjectFacts.nonEmpty)
      .distinct

  def consequenceSquares(consequence: TransitionConsequence): List[EvidenceSquare] =
    consequence.subjectFacts
      .flatMap(_.semanticSquares)
      .distinct

  def goalTargetSubjects(consequence: TransitionConsequence): List[String] =
    consequence.goalSubjectBindings.map(_.stableKey)

  def consequenceTargetSquares(
      consequence: TransitionConsequence
  ): List[EvidenceSquare] =
    consequence.goalSubjectFacts.flatMap(_.targetSquares).distinct

  private[chessjudgment] def meansOnlyResultKind(kind: TransitionConsequenceKind): Boolean =
    MeansOnlyResultKinds(kind)

  def pressureTargetSquares(event: PlanCausalEventNode): Set[String] =
    event.structuralConsequences
      .filter(consequence => PressureKinds(consequence.kind))
      .flatMap(consequenceSquares)
      .map(_.key.toLowerCase)
      .toSet

  def triggerMoveCapturesPiece(trigger: PlanCausalEventNode): Boolean =
    trigger.certifiedLegalStep.exists(_.capturedRole.nonEmpty)

enum PlanCausalBranchOutcome:
  case Realized
  case Deferred
  case Diverted
  case Refuted

enum PlanCausalRealizationMatch:
  case ExactMove
  case EquivalentFunction

final case class PlanCausalRealization(
    observedRoot: PlanCausalEventNode,
    resultRoute: PlanCausalResultRoute,
    matchKind: PlanCausalRealizationMatch
):
  def event: PlanCausalEventNode = resultRoute.sourceEvent
  def moveUci: String = EvidenceRef.normalizeMove(event.moveUci)
  def plyOffset: Int = event.step.ply - observedRoot.step.ply

enum PlanCausalTerminalOutcome:
  case Victory
  case Defeat
  case Draw

enum PlanCausalRobustness:
  case Untested
  case Deferred
  case Superseded
  case Refuted
  case Conditional
  case Robust

final case class PlanCausalBranchWitness(
    sourceProbeId: String,
    line: LineNodeRef,
    observedEpisode: Option[PlanCausalEpisode],
    certifiedHorizonPlyOffset: Int,
    observedThroughPlyOffset: Int,
    terminalOutcome: Option[PlanCausalTerminalOutcome],
    terminalPlyOffset: Option[Int],
    terminalStep: Option[LineReplayStep],
    private[chessjudgment] val canonicalReplay: Option[CanonicalLineReplay] = None
)

private object PlanCausalProofKey:
  def product(kind: String, values: Iterable[String]): String =
    sequence(kind :: values.toList)

  def sequence(values: Iterable[String]): String =
    values.iterator.map(value => s"${value.length}:$value").mkString

  def optional(value: Option[String]): String =
    value match
      case Some(exact) => product("some", List(exact))
      case None        => product("none", Nil)

/** Branch-neutral chess function proved by one exact causal dependency.
  * Replay steps and FENs remain on the occurrence proof; this projection drops
  * only intervening occurrence identity so independently replayed reply
  * branches can test whether they preserve the same causal function.
  */
sealed trait PlanCausalDependencyFunctionProof:
  def kind: String
  def proofSquares: List[EvidenceSquare]
  def proofPieceRoles: List[EvidencePieceRole]
  protected def identityParts: List[String]
  final def stableKey: String =
    PlanCausalProofKey.product(kind, identityParts)

object PlanCausalDependencyFunctionProof:
  final case class ObjectState(
      pieceRole: EvidencePieceRole,
      color: Color,
      rootFrom: EvidenceSquare,
      rootTo: EvidenceSquare,
      futureFrom: EvidenceSquare,
      futureTo: EvidenceSquare
  ) extends PlanCausalDependencyFunctionProof:
    val kind = "object-state"
    val proofSquares = List(rootFrom, rootTo, futureFrom, futureTo)
    val proofPieceRoles = List(pieceRole)
    protected val identityParts = List(
      pieceRole.name.toLowerCase,
      color.toString.toLowerCase,
      rootFrom.key.toLowerCase,
      rootTo.key.toLowerCase,
      futureFrom.key.toLowerCase,
      futureTo.key.toLowerCase
    )

  final case class LineAccess(
      enabledPieceRole: EvidencePieceRole,
      color: Color,
      vacatedSquare: EvidenceSquare,
      enabledFrom: EvidenceSquare,
      enabledTo: EvidenceSquare
  ) extends PlanCausalDependencyFunctionProof:
    val kind = "line-access"
    val proofSquares = List(vacatedSquare, enabledFrom, enabledTo)
    val proofPieceRoles = List(enabledPieceRole)
    protected val identityParts = List(
      enabledPieceRole.name.toLowerCase,
      color.toString.toLowerCase,
      vacatedSquare.key.toLowerCase,
      enabledFrom.key.toLowerCase,
      enabledTo.key.toLowerCase
    )

  sealed trait ResponseContinuation extends PlanCausalDependencyFunctionProof:
    def replyMoveUci: String
    def replyFrom: EvidenceSquare
    def replyTo: EvidenceSquare
    def followUpFrom: EvidenceSquare
    def followUpTo: EvidenceSquare
    final def proofSquares: List[EvidenceSquare] =
      List(replyFrom, replyTo, followUpFrom, followUpTo) ++ resultSquares
    protected def resultSquares: List[EvidenceSquare]

  final case class PawnBreakFollowUp(
      followUpKind: PawnBreakFollowUpKind,
      color: Color,
      replyMoveUci: String,
      replyFrom: EvidenceSquare,
      replyTo: EvidenceSquare,
      followUpFrom: EvidenceSquare,
      followUpTo: EvidenceSquare,
      releasedPassedPawn: EvidenceSquare,
      releaseRelationKey: String
  ) extends ResponseContinuation:
    val kind = "response-continuation:pawn-break-follow-up"
    val proofPieceRoles = List(EvidencePieceRole(Pawn.toString))
    protected val resultSquares = List(releasedPassedPawn)
    protected val identityParts = List(
      followUpKind.toString.toLowerCase,
      color.toString.toLowerCase,
      EvidenceRef.normalizeMove(replyMoveUci),
      replyFrom.key.toLowerCase,
      replyTo.key.toLowerCase,
      followUpFrom.key.toLowerCase,
      followUpTo.key.toLowerCase,
      releasedPassedPawn.key.toLowerCase,
      releaseRelationKey
    )

  final case class CaptureFollowUp(
      triggerRole: EvidencePieceRole,
      responderRole: EvidencePieceRole,
      followUpRole: EvidencePieceRole,
      replyMoveUci: String,
      replyFrom: EvidenceSquare,
      replyTo: EvidenceSquare,
      followUpFrom: EvidenceSquare,
      followUpTo: EvidenceSquare
  ) extends ResponseContinuation:
    val kind = "response-continuation:capture-follow-up"
    val proofPieceRoles = List(triggerRole, responderRole, followUpRole).distinct
    protected val resultSquares = Nil
    protected val identityParts = List(
      triggerRole.name.toLowerCase,
      responderRole.name.toLowerCase,
      followUpRole.name.toLowerCase,
      EvidenceRef.normalizeMove(replyMoveUci),
      replyFrom.key.toLowerCase,
      replyTo.key.toLowerCase,
      followUpFrom.key.toLowerCase,
      followUpTo.key.toLowerCase
    )

  final case class CheckFollowUp(
      triggerRole: EvidencePieceRole,
      responderRole: EvidencePieceRole,
      followUpRole: EvidencePieceRole,
      replyMoveUci: String,
      replyFrom: EvidenceSquare,
      replyTo: EvidenceSquare,
      followUpFrom: EvidenceSquare,
      followUpTo: EvidenceSquare
  ) extends ResponseContinuation:
    val kind = "response-continuation:check-follow-up"
    val proofPieceRoles = List(triggerRole, responderRole, followUpRole).distinct
    protected val resultSquares = Nil
    protected val identityParts = List(
      triggerRole.name.toLowerCase,
      responderRole.name.toLowerCase,
      followUpRole.name.toLowerCase,
      EvidenceRef.normalizeMove(replyMoveUci),
      replyFrom.key.toLowerCase,
      replyTo.key.toLowerCase,
      followUpFrom.key.toLowerCase,
      followUpTo.key.toLowerCase
    )

  final case class ExchangeConversion(
      involvedRoles: List[EvidencePieceRole],
      replyMoveUci: String,
      replyFrom: EvidenceSquare,
      replyTo: EvidenceSquare,
      followUpFrom: EvidenceSquare,
      followUpTo: EvidenceSquare,
      convertingPawnAtPhaseBoundary: EvidenceSquare
  ) extends ResponseContinuation:
    val kind = "response-continuation:exchange-conversion"
    val proofPieceRoles = involvedRoles.distinct
    protected val resultSquares = List(convertingPawnAtPhaseBoundary)
    protected val identityParts = List(
      PlanCausalProofKey.sequence(involvedRoles.map(_.name.toLowerCase).distinct.sorted),
      EvidenceRef.normalizeMove(replyMoveUci),
      replyFrom.key.toLowerCase,
      replyTo.key.toLowerCase,
      followUpFrom.key.toLowerCase,
      followUpTo.key.toLowerCase,
      convertingPawnAtPhaseBoundary.key.toLowerCase
    )

  def from(dependency: PlanCausalEventDependency): PlanCausalDependencyFunctionProof =
    dependency.proof match
      case PlanCausalDependencyProof.ObjectState(trajectory) =>
        ObjectState(
          trajectory.pieceRole,
          trajectory.color,
          trajectory.rootFrom,
          trajectory.rootTo,
          trajectory.futureFrom,
          trajectory.futureTo
        )
      case PlanCausalDependencyProof.LineAccess(trajectory) =>
        LineAccess(
          trajectory.enabledPieceRole,
          trajectory.color,
          trajectory.vacatedSquare,
          trajectory.enabledFrom,
          trajectory.enabledTo
        )
      case PlanCausalDependencyProof.ResponseContinuation(trajectory: PawnBreakFollowUpTrajectory) =>
        PawnBreakFollowUp(
          trajectory.kind,
          trajectory.color,
          trajectory.replyStep.moveUci,
          trajectory.replyFrom,
          trajectory.replyTo,
          trajectory.followUpFrom,
          trajectory.followUpTo,
          trajectory.releasedPassedPawn,
          trajectory.releaseWitness.changeKey.stableKey
        )
      case PlanCausalDependencyProof.ResponseContinuation(trajectory: CaptureResponseFollowUpTrajectory) =>
        CaptureFollowUp(
          trajectory.triggerRole,
          trajectory.responderRole,
          trajectory.followUpRole,
          trajectory.replyStep.moveUci,
          trajectory.replyFrom,
          trajectory.replyTo,
          trajectory.followUpFrom,
          trajectory.followUpTo
        )
      case PlanCausalDependencyProof.ResponseContinuation(trajectory: CheckResponseFollowUpTrajectory) =>
        CheckFollowUp(
          trajectory.triggerRole,
          trajectory.responderRole,
          trajectory.followUpRole,
          trajectory.replyStep.moveUci,
          trajectory.replyFrom,
          trajectory.replyTo,
          trajectory.followUpFrom,
          trajectory.followUpTo
        )
      case PlanCausalDependencyProof.ResponseContinuation(trajectory: ExchangeConversionTrajectory) =>
        ExchangeConversion(
          trajectory.involvedRoles,
          trajectory.replyStep.moveUci,
          trajectory.replyFrom,
          trajectory.replyTo,
          trajectory.followUpFrom,
          trajectory.followUpTo,
          trajectory.convertingPawnAtPhaseBoundary
        )

final case class PlanCausalDependencyFunctionIdentity(
    fromMoveUci: String,
    fromPlyOffset: Int,
    toMoveUci: String,
    toPlyOffset: Int,
    dependencyKind: PlanCausalDependencyKind,
    proof: PlanCausalDependencyFunctionProof,
    plyOffset: Int
):
  def proofKind: String = proof.kind
  def proofSquares: List[String] = proof.proofSquares.map(_.key.toLowerCase).distinct.sorted
  def proofPieceRoles: List[String] = proof.proofPieceRoles.map(_.name.toLowerCase).distinct.sorted
  def stableKey: String =
    PlanCausalProofKey.product(
      "causal-dependency-function",
      List(
        fromMoveUci,
        fromPlyOffset.toString,
        toMoveUci,
        toPlyOffset.toString,
        dependencyKind.toString.toLowerCase,
        proof.stableKey,
        plyOffset.toString
      )
    )

object PlanCausalDependencyFunctionIdentity:
  def from(
      root: PlanCausalEventNode,
      dependency: PlanCausalEventDependency
  ): PlanCausalDependencyFunctionIdentity =
    PlanCausalDependencyFunctionIdentity(
      fromMoveUci = EvidenceRef.normalizeMove(dependency.from.moveUci),
      fromPlyOffset = dependency.from.step.ply - root.step.ply,
      toMoveUci = EvidenceRef.normalizeMove(dependency.to.moveUci),
      toPlyOffset = dependency.to.step.ply - root.step.ply,
      dependencyKind = dependency.kind,
      proof = PlanCausalDependencyFunctionProof.from(dependency),
      plyOffset = dependency.plyOffset
    )

final case class PlanCausalLineStepOccurrence private (
    moveUci: String,
    before: PlanPositionOccurrence,
    after: PlanPositionOccurrence
):
  def stableKey: String =
    PlanCausalProofKey.product(
      "causal-line-step-occurrence",
      List(moveUci, before.stableKey, after.stableKey)
    )

object PlanCausalLineStepOccurrence:
  def from(step: LineReplayStep): PlanCausalLineStepOccurrence =
    require(step.ply > 0, "causal line step occurrence requires a positive result ply")
    PlanCausalLineStepOccurrence(
      moveUci = EvidenceRef.normalizeMove(step.moveUci),
      before = PlanPositionOccurrence.from(step.fenBefore, step.ply - 1),
      after = PlanPositionOccurrence.from(step.fenAfter, step.ply)
    )

/** Exact occurrence of one causal edge. The semantic function remains typed,
  * while FEN/ply identity for both endpoints and every intervening replay step
  * prevents parallel routes from collapsing into a node-only sequence.
  */
final case class PlanCausalDependencyOccurrenceIdentity private (
    from: PlanEventOccurrence,
    to: PlanEventOccurrence,
    dependencyKind: PlanCausalDependencyKind,
    proof: PlanCausalDependencyFunctionProof,
    interveningSteps: List[PlanCausalLineStepOccurrence],
    plyOffset: Int
):
  def stableKey: String =
    PlanCausalProofKey.product(
      "causal-dependency-occurrence",
      List(
        from.stableKey,
        to.stableKey,
        dependencyKind.toString.toLowerCase,
        proof.stableKey,
        PlanCausalProofKey.sequence(interveningSteps.map(_.stableKey)),
        plyOffset.toString
      )
    )

object PlanCausalDependencyOccurrenceIdentity:
  def from(dependency: PlanCausalEventDependency): PlanCausalDependencyOccurrenceIdentity =
    require(dependency.planConnectionProven, "causal dependency occurrence requires an exact proven connection")
    val intervening = dependency.proof match
      case PlanCausalDependencyProof.ObjectState(_) => Nil
      case PlanCausalDependencyProof.LineAccess(trajectory) => trajectory.interveningSteps
      case PlanCausalDependencyProof.ResponseContinuation(trajectory) => trajectory.interveningSteps
    PlanCausalDependencyOccurrenceIdentity(
      from = eventOccurrence(dependency.from),
      to = eventOccurrence(dependency.to),
      dependencyKind = dependency.kind,
      proof = PlanCausalDependencyFunctionProof.from(dependency),
      interveningSteps = intervening.map(PlanCausalLineStepOccurrence.from),
      plyOffset = dependency.plyOffset
    )

  private def eventOccurrence(event: PlanCausalEventNode): PlanEventOccurrence =
    PlanEventOccurrence.from(
      event = event.identity,
      moveUci = event.moveUci,
      ply = event.step.ply,
      fenBefore = event.step.fenBefore,
      fenAfter = event.step.fenAfter
    )

final case class PlanSequenceProof private (
    summary: PlanSequenceSummary,
    causalDependencies: List[PlanCausalDependencyOccurrenceIdentity]
):
  def stableKey: String =
    val continuityKey = summary.continuity
      .map(continuity =>
        PlanCausalProofKey.product(
          "continuity",
          List(
            PlanCausalProofKey.optional(continuity.startingEvent.map(_.stableKey)),
            continuity.consecutivePlies.toString,
            continuity.startingPly.toString,
            PlanCausalProofKey.sequence(continuity.supportingMoves),
            PlanCausalProofKey.sequence(continuity.supportingEvents.map(_.stableKey)),
            continuity.completionProven.toString
          )
        )
      )
      .getOrElse(PlanCausalProofKey.product("no-continuity", Nil))
    PlanCausalProofKey.product(
      "plan-sequence-proof",
      List(
        summary.transitionType.toString.toLowerCase,
        summary.exactPathOccurrence.stableKey,
        PlanCausalProofKey.optional(summary.primaryPlanId.map(_.id)),
        PlanCausalProofKey.optional(summary.previousPlanId.map(_.id)),
        continuityKey,
        PlanCausalProofKey.optional(summary.previousEvent.map(_.stableKey)),
        PlanCausalProofKey.optional(summary.currentEvent.map(_.stableKey)),
        PlanCausalProofKey.sequence(causalDependencies.map(_.stableKey))
      )
    )

object PlanSequenceProof:
  def from(
      summary: PlanSequenceSummary,
      dependencies: List[PlanCausalEventDependency]
  ): PlanSequenceProof =
    val exactDependencies = dependencies
      .map(PlanCausalDependencyOccurrenceIdentity.from)
      .distinct
    require(exactDependencies.nonEmpty, "plan sequence proof requires at least one causal dependency")
    val dependencyEvents = exactDependencies.head.from :: exactDependencies.map(_.to)
    require(
      exactDependencies.zip(exactDependencies.drop(1)).forall { case (current, next) =>
        current.to == next.from
      },
      "plan sequence proof dependencies must form one ordered causal path"
    )
    require(
      dependencyEvents == summary.exactPathOccurrence.events,
      "plan sequence proof dependencies must exactly own its event path"
    )
    PlanSequenceProof(summary, exactDependencies)

object PlanCausalFunctionalMatch:
  def functionallyEquivalent(
      expected: List[TransitionConsequence],
      observed: List[TransitionConsequence]
  ): Boolean =
    val expectedPositive = expected.filter(consequence => consequence.establishesState && consequence.strength > 0)
    val observedPositive = observed.filter(consequence => consequence.establishesState && consequence.strength > 0)
    @annotation.tailrec
    def consume(
        remainingExpected: List[TransitionConsequence],
        remainingObserved: List[TransitionConsequence]
    ): Boolean =
      remainingExpected match
        case Nil => true
        case expectedHead :: expectedTail =>
          remainingObserved.zipWithIndex.find { case (observedValue, _) =>
            consequenceCompatible(expectedHead, observedValue)
          } match
            case Some((_, index)) =>
              consume(expectedTail, remainingObserved.patch(index, Nil, 1))
            case None => false
    expectedPositive.nonEmpty && consume(expectedPositive, observedPositive)

  def causallyEquivalent(
      expectedRoot: PlanCausalEventNode,
      expectedRoute: PlanCausalResultRoute,
      observedRoot: PlanCausalEventNode,
      observedRoute: PlanCausalResultRoute
  ): Boolean =
    expectedRoute.causalPath.forall(_.planConnectionProven) &&
      observedRoute.causalPath.forall(_.planConnectionProven) &&
      functionPath(expectedRoot, expectedRoute.causalPath) ==
        functionPath(observedRoot, observedRoute.causalPath) &&
      expectedRoute.goalProof.functionIdentity(expectedRoot) ==
        observedRoute.goalProof.functionIdentity(observedRoot) &&
      functionallyEquivalent(List(expectedRoute.consequence), List(observedRoute.consequence))

  private def targetObjectsCompatible(
      expected: TransitionConsequence,
      observed: TransitionConsequence
  ): Boolean =
    val left = EvidenceObjectBinding.goalTargetObjectGroups(expected).toSet
    val right = EvidenceObjectBinding.goalTargetObjectGroups(observed).toSet
    left.nonEmpty && left == right

  private def consequenceCompatible(
      expected: TransitionConsequence,
      observed: TransitionConsequence
  ): Boolean =
    expected.kind == observed.kind &&
      expected.polarity == observed.polarity &&
      observed.strength >= expected.strength &&
      targetObjectsCompatible(expected, observed)

  private def functionPath(
      root: PlanCausalEventNode,
      dependencies: List[PlanCausalEventDependency]
  ): List[PlanCausalDependencyFunctionIdentity] =
    dependencies.map(PlanCausalDependencyFunctionIdentity.from(root, _))

final case class PlanCausalResultObservation(
    line: LineNodeRef,
    replyMove: String,
    outcome: PlanCausalBranchOutcome,
    realizations: List[PlanCausalRealization],
    observedThroughPlyOffset: Int,
    terminalOutcome: Option[PlanCausalTerminalOutcome],
    terminalPlyOffset: Option[Int],
    terminalStep: Option[LineReplayStep],
    private[chessjudgment] val canonicalReplay: Option[CanonicalLineReplay] = None
):
  def realizationMoves: List[String] = realizations.map(_.moveUci).distinct
  def realizationPlyOffsets: List[Int] =
    realizations.map(_.plyOffset).distinct.sorted

final case class PlanCausalResultAssessment(
    resultRoute: PlanCausalResultRoute,
    sourcePlyOffset: Int,
    observations: List[PlanCausalResultObservation],
    robustness: PlanCausalRobustness
):
  def sourceEvent: PlanCausalEventNode = resultRoute.sourceEvent
  def consequence: TransitionConsequence = resultRoute.consequence
  def causalPath: List[PlanCausalEventDependency] = resultRoute.causalPath
  def goalProof: PlanCausalGoalProof = resultRoute.goalProof
  def positiveProofReady: Boolean =
    robustness == PlanCausalRobustness.Robust || robustness == PlanCausalRobustness.Conditional
  def realizedObservations: List[PlanCausalResultObservation] =
    observations.filter(_.outcome == PlanCausalBranchOutcome.Realized)

final case class PlanResultSourceOccurrence(
    moveUci: String,
    plyOffset: Int,
    actorRole: Option[String] = None
):
  def stableKey: String =
    List(moveUci, plyOffset.toString, actorRole.map(_.toLowerCase).getOrElse("unknown")).mkString("@")

final case class PlanResultBranchRealizationIdentity(
    moveUci: String,
    matchKind: PlanCausalRealizationMatch,
    plyOffset: Int,
    causalRoute: List[PlanCausalDependencyFunctionIdentity],
    goalFunction: PlanCausalGoalFunctionIdentity
):
  def stableKey: String =
    PlanCausalProofKey.product(
      "plan-result-branch-realization",
      List(
        moveUci,
        matchKind.toString.toLowerCase,
        plyOffset.toString,
        PlanCausalProofKey.sequence(causalRoute.map(_.stableKey)),
        goalFunction.stableKey
      )
    )

final case class PlanResultBranchIdentity(
    replyMoveUci: String,
    outcome: PlanCausalBranchOutcome,
    observedThroughPlyOffset: Int,
    realizations: List[PlanResultBranchRealizationIdentity],
    terminalOutcome: Option[PlanCausalTerminalOutcome],
    terminalPlyOffset: Option[Int],
    terminalMoveUci: Option[String]
):
  def stableKey: String =
    List(
      replyMoveUci,
      outcome.toString.toLowerCase,
      observedThroughPlyOffset.toString,
      realizations.map(_.stableKey).mkString("[", ",", "]"),
      terminalOutcome.map(_.toString.toLowerCase).getOrElse("none"),
      terminalPlyOffset.map(_.toString).getOrElse("none"),
      terminalMoveUci.getOrElse("none")
    ).mkString(":")

/** Plan taxonomy is annotation. This is the exact result occurrence and
  * causal route that may participate in player-facing semantic equality.
  */
final case class PlanResultSemanticIdentity(
    root: PlanResultSourceOccurrence,
    source: PlanResultSourceOccurrence,
    selectedInducedResponse: Option[PlanResultSourceOccurrence],
    consequenceKind: TransitionConsequenceKind,
    polarity: StructuralSignalPolarity,
    goalTargetSubjects: List[String],
    strength: Int,
    robustness: PlanCausalRobustness,
    branches: List[PlanResultBranchIdentity],
    causalRoute: List[PlanCausalDependencyFunctionIdentity],
    goalFunction: PlanCausalGoalFunctionIdentity
):
  def stableKey: String =
    (List(root.stableKey, source.stableKey) ++
      selectedInducedResponse.toList.map(response => s"induced-response:${response.stableKey}") ++ List(
      consequenceKind.toString.toLowerCase,
      polarity.toString.toLowerCase,
      goalTargetSubjects.mkString("[", ",", "]"),
      strength.toString,
      robustness.toString.toLowerCase,
      branches.map(_.stableKey).mkString("[", ",", "]"),
      causalRoute.map(_.stableKey).mkString("[", ",", "]"),
      goalFunction.stableKey
    )).mkString("|")

object PlanResultSemanticIdentity:
  def from(
      event: PlanCausalEventEvidence,
      assessment: PlanCausalResultAssessment,
      selectedInducedResponse: Option[PlanCausalResponse]
  ): PlanResultSemanticIdentity =
    PlanResultSemanticIdentity(
      root = PlanResultSourceOccurrence(
        EvidenceRef.normalizeMove(event.causalEpisode.root.moveUci),
        0,
        event.causalEpisode.root.identity.actorRole
      ),
      source = PlanResultSourceOccurrence(
        EvidenceRef.normalizeMove(assessment.sourceEvent.moveUci),
        assessment.sourcePlyOffset,
        assessment.sourceEvent.identity.actorRole
      ),
      selectedInducedResponse = selectedInducedResponse.map(response =>
        PlanResultSourceOccurrence(
          EvidenceRef.normalizeMove(response.step.moveUci),
          response.step.ply - event.causalEpisode.root.step.ply,
          response.certifiedLegalStep.map(_.move.piece.role.name)
        )
      ),
      consequenceKind = assessment.consequence.kind,
      polarity = assessment.consequence.polarity,
      goalTargetSubjects = normalizedGoalTargetSubjects(
        assessment.consequence.goalSubjectBindings.map(_.stableKey)
      ),
      strength = assessment.consequence.strength,
      robustness = assessment.robustness,
      branches = assessmentBranches(assessment.observations),
      causalRoute = assessment.causalPath.map(
        PlanCausalDependencyFunctionIdentity.from(event.causalEpisode.root, _)
      ),
      goalFunction = assessment.goalProof.functionIdentity(event.causalEpisode.root)
    )

  private def normalizedGoalTargetSubjects(subjects: List[String]): List[String] =
    subjects.map(normalize).filter(_.nonEmpty).distinct.sorted

  private def assessmentBranches(
      observations: List[PlanCausalResultObservation]
  ): List[PlanResultBranchIdentity] =
    observations.map { observation =>
      PlanResultBranchIdentity(
        replyMoveUci = EvidenceRef.normalizeMove(observation.replyMove),
        outcome = observation.outcome,
        observedThroughPlyOffset = observation.observedThroughPlyOffset,
        realizations = observation.realizations.map(realization =>
          PlanResultBranchRealizationIdentity(
            moveUci = realization.moveUci,
            matchKind = realization.matchKind,
            plyOffset = realization.plyOffset,
            causalRoute = realization.resultRoute.causalPath.map(
              PlanCausalDependencyFunctionIdentity.from(realization.observedRoot, _)
            ),
            goalFunction = realization.resultRoute.goalProof.functionIdentity(realization.observedRoot)
          )
        ).distinct.sortBy(_.stableKey),
        terminalOutcome = observation.terminalOutcome,
        terminalPlyOffset = observation.terminalPlyOffset,
        terminalMoveUci = observation.terminalStep
          .map(step => EvidenceRef.normalizeMove(step.moveUci))
          .filter(_.nonEmpty)
      )
    }.distinct.sortBy(_.stableKey)

  private def normalize(value: String): String =
    Option(value).getOrElse("").trim.toLowerCase

object PlanCausalResultAssessment:
  def fromRoute(
      episode: PlanCausalEpisode,
      resultRoute: PlanCausalResultRoute,
      witnesses: List[PlanCausalBranchWitness],
      branchSetComplete: Boolean
  ): PlanCausalResultAssessment =
    require(
      episode.resultRoutes.contains(resultRoute),
      "plan result assessment requires a route owned by its episode"
    )
    val sourcePlyOffset = resultRoute.sourceEvent.step.ply - episode.root.step.ply
    val observations = witnesses.map(witness =>
      observation(episode, resultRoute, sourcePlyOffset, witness)
    )
    val robustness =
      if observations.isEmpty then PlanCausalRobustness.Untested
      else if !branchSetComplete || observations.exists(_.outcome == PlanCausalBranchOutcome.Deferred) then
        PlanCausalRobustness.Deferred
      else if observations.forall(_.outcome == PlanCausalBranchOutcome.Realized) then PlanCausalRobustness.Robust
      else if observations.exists(_.outcome == PlanCausalBranchOutcome.Realized) then PlanCausalRobustness.Conditional
      else if observations.forall(observation =>
        observation.outcome == PlanCausalBranchOutcome.Diverted &&
          observation.terminalOutcome.exists(_ != PlanCausalTerminalOutcome.Defeat)
      ) then PlanCausalRobustness.Superseded
      else PlanCausalRobustness.Refuted
    PlanCausalResultAssessment(resultRoute, sourcePlyOffset, observations, robustness)

  private def observation(
      episode: PlanCausalEpisode,
      resultRoute: PlanCausalResultRoute,
      sourcePlyOffset: Int,
      witness: PlanCausalBranchWitness
  ): PlanCausalResultObservation =
    val realizations = witness.observedEpisode.toList.flatMap { observedEpisode =>
      observedEpisode.resultRoutes.flatMap { candidateRoute =>
        val candidate = candidateRoute.sourceEvent
        val offset = candidate.step.ply - observedEpisode.root.step.ply
        Option
          .when(
            offset <= witness.observedThroughPlyOffset &&
              PlanCausalFunctionalMatch.causallyEquivalent(
                episode.root,
                resultRoute,
                observedEpisode.root,
                candidateRoute
              )
          )(
            PlanCausalRealization(
              observedEpisode.root,
              candidateRoute,
              if EvidenceRef.sameMove(resultRoute.sourceEvent.moveUci, candidate.moveUci) &&
                  offset == sourcePlyOffset
              then PlanCausalRealizationMatch.ExactMove
              else PlanCausalRealizationMatch.EquivalentFunction
            )
          )
          .toList
      }
    }.distinct.sortBy(realization =>
      (
        if realization.matchKind == PlanCausalRealizationMatch.ExactMove then 0 else 1,
        realization.event.step.ply,
        realization.moveUci,
        exactEventOrderKey(realization.event)
      )
    )
    val terminalBeforeDeadline =
      witness.terminalOutcome.filter(_ => witness.terminalPlyOffset.exists(_ <= witness.observedThroughPlyOffset))
    val outcome =
      if realizations.nonEmpty then PlanCausalBranchOutcome.Realized
      else
        terminalBeforeDeadline match
          case Some(PlanCausalTerminalOutcome.Defeat) => PlanCausalBranchOutcome.Refuted
          case Some(_)                                => PlanCausalBranchOutcome.Diverted
          case None if witness.observedThroughPlyOffset < sourcePlyOffset => PlanCausalBranchOutcome.Deferred
          case None if witness.observedEpisode.exists(observedEpisode =>
              observedEpisode.continuationsEnabledByRoot.exists(event =>
                event.step.ply - observedEpisode.root.step.ply <= witness.observedThroughPlyOffset
              )
            ) =>
            PlanCausalBranchOutcome.Diverted
          case None => PlanCausalBranchOutcome.Refuted
    PlanCausalResultObservation(
      line = witness.line,
      replyMove = witness.line.rootMove,
      outcome = outcome,
      realizations = realizations,
      observedThroughPlyOffset = witness.observedThroughPlyOffset,
      terminalOutcome = terminalBeforeDeadline,
      terminalPlyOffset = witness.terminalPlyOffset.filter(_ <= witness.observedThroughPlyOffset),
      terminalStep = Option.when(terminalBeforeDeadline.nonEmpty)(witness.terminalStep).flatten,
      canonicalReplay = witness.canonicalReplay
    )

  private def exactEventOrderKey(event: PlanCausalEventNode): String =
    val consequenceKeys = event.structuralConsequences.map { consequence =>
      List(
        consequence.kind.toString,
        consequence.strength.toString,
        consequence.subjectBindings.map(_.stableKey).mkString("[", ",", "]"),
        consequence.targetBindings.map(_.stableKey).mkString("[", ",", "]")
      ).mkString(":")
    }.sorted
    List(
      event.identity.stableKey,
      event.moveUci,
      event.step.ply.toString,
      PrincipalVariationEvidence.normalizeFen(event.step.fenBefore),
      PrincipalVariationEvidence.normalizeFen(event.step.fenAfter),
      event.perspective.toString,
      consequenceKeys.mkString("[", ",", "]")
    ).mkString("|")

final case class PlanCausalEventEvidence(
    rootTransition: StructuralTransitionBinding,
    causalEpisode: PlanCausalEpisode,
    branchWitnesses: List[PlanCausalBranchWitness],
    opponentResourceDeterrence: Option[OpponentResourceDeterrenceProof] = None,
    continuationSourceLine: Option[LineNodeRef] = None,
    private[chessjudgment] val canonicalRootTransitionProof: Option[CanonicalTransitionProof] = None
) extends EvidencePayload:
  require(rootTransition.line.nonEmpty, "plan-causal root transition must reference its canonical line")
  require(
    EvidenceRef.sameMove(causalEpisode.root.moveUci, rootTransition.moveUci) &&
      causalEpisode.root.step.ply == rootTransition.to.ply &&
      PrincipalVariationEvidence.sameBoardState(causalEpisode.root.step.fenBefore, rootTransition.from.fen) &&
      PrincipalVariationEvidence.sameBoardState(causalEpisode.root.step.fenAfter, rootTransition.to.fen) &&
      causalEpisode.root.perspective == rootTransition.perspective,
    "plan-causal episode root must match its authoritative root transition"
  )

  def episode: Option[PlanCausalEpisode] =
    Option.when(causalEpisode.causalEpisodeProven)(causalEpisode)
  def identity: PlanEventIdentity = causalEpisode.root.identity
  def planId: PlanKind = identity.kind
  def rootLine: LineNodeRef = rootTransition.line.get
  def structuralConsequences: List[TransitionConsequence] = causalEpisode.root.structuralConsequences
  def rootMove: String = rootTransition.moveUci
  def perspective: Color = rootTransition.perspective
  private[chessjudgment] def rootTransitionIsCertified: Boolean =
    canonicalRootTransitionProof.exists(_.proves(rootTransition))
  def opponentResourceDeterrenceProofReady: Boolean =
    rootTransitionIsCertified && opponentResourceDeterrence.exists(proof =>
      episode.exists(proof.certifiedFor(rootLine, rootTransition, _).nonEmpty)
    )
  def materialCounterplayPreventionProofReady: Boolean =
    rootTransitionIsCertified && opponentResourceDeterrence.exists(proof =>
      episode.exists(causalEpisode =>
        proof
          .certifiedFor(rootLine, rootTransition, causalEpisode)
          .exists(_.materialCounterplayPreventionProven)
      )
    )
  def resultAdvancesGoal(consequence: TransitionConsequence): Boolean =
    PlanCausalGoalProof.proves(identity, rootTransition, consequence)
  def directGoalConsequences: List[TransitionConsequence] =
    structuralConsequences.filter(resultAdvancesGoal)
  def observedGoalResultRoutes: List[PlanCausalResultRoute] =
    episode.toList.flatMap(_.resultRoutes).distinct.sortBy(_.stableKey)
  def rootEnablingDependencies: List[PlanCausalEventDependency] =
    episode.toList.flatMap(causalEpisode =>
      causalEpisode.dependencies.filter(dependency => dependency.from == causalEpisode.root && dependency.enablesContinuation)
    )
  def historyEnablingDependencies: List[PlanCausalEventDependency] =
    episode.toList.flatMap(_.historyDependencies.filter(_.enablesContinuation))
  def counterfactualContinuationProven: Boolean = episode.exists(_.rootEnablesContinuation)
  def requiredHorizonPlyOffset: Int =
    episode
      .toList
      .flatMap(causalEpisode =>
        observedGoalResultRoutes.map { route =>
          route.sourceEvent.step.ply - causalEpisode.root.step.ply
        }
      )
      .maxOption
      .getOrElse(0)
      .max(0)
  lazy val expectedReplyCount: Int =
    canonicalRootTransitionProof
      .filter(_.proves(rootTransition))
      .map(_.legalResponseCount(BranchReplyProbeBinding.ReplyMultiPv))
      .getOrElse(0)
  lazy val provenForwardDependencyPaths: List[List[PlanCausalEventDependency]] =
    Option
      .when(branchWitnesses.nonEmpty && episodePublicProofReady)(
        positiveGoalResultAssessments.map(_.causalPath)
      )
      .getOrElse(Nil)
      .filter(_.nonEmpty)
      .distinct
      .sortBy(_.map(_.stableKey).mkString)
  lazy val planSequenceProofs: List[PlanSequenceProof] =
    val historicalProofs = for
      causalEpisode <- episode.toList
      if causalEpisode.historySequenceProven
      dependencyPath <- causalEpisode.historicalDependencyPathsToRoot
      sequence = dependencyPath.head.from :: dependencyPath.map(_.to)
      antecedents = sequence.dropRight(1)
      continuity <- PlanContinuity.fromAntecedents(
        antecedents.map(event => event.identity -> event.step.ply),
        currentPly = causalEpisode.root.step.ply,
        completionProven = causalEpisode.historicalCompletionProven
      )
      summary = PlanSequenceSummary(
        transitionType = continuity.episodeTransitionType,
        exactPathOccurrence = planSequencePathOccurrence(sequence),
        primaryPlanId = Some(planId),
        previousPlanId = Some(planId),
        continuity = Some(continuity),
        previousEvent = antecedents.lastOption.map(_.identity),
        currentEvent = Some(identity)
      )
    yield PlanSequenceProof.from(summary, dependencyPath)
    val forwardProofs = for
      causalEpisode <- episode.toList
      dependencyPath <- provenForwardDependencyPaths
      sequence = causalEpisode.root :: dependencyPath.map(_.to)
      continuity <- PlanContinuity.fromEvents(
        sequence.map(event => event.identity -> event.step.ply),
        completionProven = causalEpisode.completionProven
      )
      summary = PlanSequenceSummary(
        transitionType = TransitionType.Opening,
        exactPathOccurrence = planSequencePathOccurrence(sequence),
        primaryPlanId = Some(planId),
        previousPlanId = None,
        continuity = Some(continuity),
        previousEvent = None,
        currentEvent = Some(identity)
      )
    yield PlanSequenceProof.from(summary, dependencyPath)
    (historicalProofs ++ forwardProofs)
      .distinct
      .sortBy(_.stableKey)
  private def planSequencePathOccurrence(
      sequence: List[PlanCausalEventNode]
  ): PlanSequencePathOccurrence =
    PlanSequencePathOccurrence.from(
      sequence.map(event =>
        PlanEventOccurrence.from(
          event = event.identity,
          moveUci = event.moveUci,
          ply = event.step.ply,
          fenBefore = event.step.fenBefore,
          fenAfter = event.step.fenAfter
        )
      )
    )
  lazy val causalResultAssessments: List[PlanCausalResultAssessment] =
    episode.toList.flatMap { causalEpisode =>
      causalEpisode.resultRoutes.map { resultRoute =>
        PlanCausalResultAssessment.fromRoute(
          causalEpisode,
          resultRoute,
          branchWitnesses,
          branchSetComplete
        )
      }
    }.distinct
  def positiveCausalResultAssessments: List[PlanCausalResultAssessment] =
    causalResultAssessments.filter(_.positiveProofReady)
  def positiveGoalResultAssessments: List[PlanCausalResultAssessment] =
    positiveCausalResultAssessments
  def resolvedCausalResultAssessments: List[PlanCausalResultAssessment] =
    causalResultAssessments.filterNot(assessment =>
      assessment.robustness == PlanCausalRobustness.Untested ||
        assessment.robustness == PlanCausalRobustness.Deferred
    )
  def resolvedGoalResultAssessments: List[PlanCausalResultAssessment] =
    resolvedCausalResultAssessments
  def goalDependencyProofReady: Boolean =
    identity.goalTheme == PlanTheme.PawnBreakPreparation &&
      rootEnablingDependencies.exists(_.preparedPawnAdvanceFile.nonEmpty) &&
      (continuationSourceLine.isEmpty || branchCoverageComplete)
  /** Every exact result authorized for an affirmative public plan Cause.
    * Sibling results retain independent robustness and proof identity.
    */
  def exactRobustPublicResultAssessments: List[PlanCausalResultAssessment] =
    positiveGoalResultAssessments
      .filter(_.robustness == PlanCausalRobustness.Robust)
      .sortBy(publicResultAssessmentSortKey)
  /** Every exact result authorized for a refuted public plan Cause. A failed
    * route cannot refute a result that another exact route still realizes.
    * Unrelated sibling results retain independent status.
    */
  def exactRefutedPublicResultAssessments: List[PlanCausalResultAssessment] =
    resolvedGoalResultAssessments
      .filter(_.robustness == PlanCausalRobustness.Refuted)
      .sortBy(publicResultAssessmentSortKey)
  private def publicResultAssessmentSortKey(
      assessment: PlanCausalResultAssessment
  ): (Int, Int, String, String, String) =
    (
      assessment.sourcePlyOffset,
      -assessment.consequence.strength,
      assessment.consequence.kind.toString,
      assessment.consequence.subjectBindings.map(_.stableKey).sorted.mkString(":"),
      assessment.causalPath.map(_.stableKey).mkString
    )
  def branchSetComplete: Boolean =
    expectedReplyCount > 0 &&
      branchWitnesses.size == expectedReplyCount &&
      branchWitnesses.map(_.line).distinct.size == branchWitnesses.size &&
      branchWitnesses.map(_.line.rootMove).distinct.size == branchWitnesses.size &&
      branchWitnesses.map(_.sourceProbeId).distinct.size == 1 &&
      branchWitnesses.map(_.certifiedHorizonPlyOffset).distinct.size == 1
  def branchCoverageComplete: Boolean =
    branchSetComplete &&
      causalResultAssessments.nonEmpty &&
      causalResultAssessments.forall(assessment =>
        assessment.robustness != PlanCausalRobustness.Untested &&
          assessment.robustness != PlanCausalRobustness.Deferred
      )
  def episodePublicProofReady: Boolean =
    counterfactualContinuationProven &&
      branchCoverageComplete &&
      positiveGoalResultAssessments.nonEmpty
  def semanticGroupingAnchors: List[EvidenceSemanticAnchor] =
    List(
      EvidenceSemanticAnchor.of(EvidenceSemanticAnchorKind.Plan, planId.id),
      EvidenceSemanticAnchor.of(
        EvidenceSemanticAnchorKind.PlanCausalEvent,
        identity.goalKey,
        s"root:$rootMove",
        s"actor:${identity.actorRole.getOrElse("unknown")}",
        s"targets:${identity.targets.mkString(",")}",
        s"results:${identity.results.mkString(",")}"
      )
    )

object PlanCausalGoalProof:
  def certify(
      identity: PlanEventIdentity,
      transition: StructuralTransitionBinding,
      consequence: TransitionConsequence
  ): Option[PlanCausalGoalProof] =
    certify(identity.goalTheme, Some(identity.kind), transition, consequence, identity.actorRole)

  def certify(
      goalTheme: PlanTheme,
      goalKind: Option[PlanKind],
      transition: StructuralTransitionBinding,
      consequence: TransitionConsequence,
      certifiedActorRole: Option[String] = None
  ): Option[PlanCausalGoalProof] =
    for
      kind <- goalKind.filter(_.theme == goalTheme)
      mechanism <- directMechanism(kind, transition, consequence, certifiedActorRole)
    yield PlanCausalGoalProof(kind, transition, consequence, mechanism, None)

  private[chessjudgment] def certifyDependency(
      plan: Plan,
      sourceTransition: StructuralTransitionBinding,
      dependency: PlanCausalEventDependency,
      consequence: TransitionConsequence
  ): Option[PlanCausalGoalProof] =
    dependencyMechanism(plan, dependency, consequence).map(mechanism =>
      PlanCausalGoalProof(plan.kind, sourceTransition, consequence, mechanism, Some(dependency))
    )

  def proves(
      identity: PlanEventIdentity,
      transition: StructuralTransitionBinding,
      consequence: TransitionConsequence
  ): Boolean =
    certify(identity, transition, consequence).nonEmpty

  def proves(
      goalTheme: PlanTheme,
      goalKind: Option[PlanKind],
      transition: StructuralTransitionBinding,
      consequence: TransitionConsequence,
      certifiedActorRole: Option[String] = None
  ): Boolean =
    certify(goalTheme, goalKind, transition, consequence, certifiedActorRole).nonEmpty

  private def directMechanism(
      kind: PlanKind,
      transition: StructuralTransitionBinding,
      consequence: TransitionConsequence,
      certifiedActorRole: Option[String]
  ): Option[PlanCausalGoalMechanism] =
    import TransitionConsequenceKind.*
    kind.theme match
      case PlanTheme.OpeningPrinciples =>
        None
      case PlanTheme.RestrictionProphylaxis =>
        None
      case PlanTheme.PieceRedeployment =>
        kind match
          case PlanKind.RookFileTransfer =>
            Option.when(
              transitionActorIs(transition, Rook, certifiedActorRole) &&
              consequence.kind == FileOccupationEstablished &&
              consequence.goalSubjectFacts.exists {
                case StructuralSubject.FileOccupation(file, square, _) =>
                  val destination = EvidenceRef.normalizeMove(transition.moveUci).slice(2, 4)
                  square.key.equalsIgnoreCase(destination) && file.key.equalsIgnoreCase(destination.take(1))
                case _ => false
              }
            )(PlanCausalGoalMechanism.RookFileOccupation)
          case _ =>
            None
      case PlanTheme.PawnBreakPreparation =>
        Option
          .when(
            kind == PlanKind.PawnAdvancePreparation &&
              pawnAdvanceResult(transition, consequence, certifiedActorRole)
          )(pawnAdvanceMechanism(consequence.kind))
          .flatten
      case PlanTheme.AdvantageTransformation =>
        kind match
          case PlanKind.PasserConversion =>
            Option.when(
              consequence.kind == PassedPawnProgress &&
              transitionActorIs(transition, Pawn, certifiedActorRole) &&
              consequence.goalSubjectFacts.exists(
                passedPawnConversionBy(_, transition.moveUci, transition.perspective)
              )
            )(PlanCausalGoalMechanism.PassedPawnConversion)
          case PlanKind.PassedPawnManufacture =>
            Option.when(
              consequence.kind == PassedPawnProgress &&
              transitionActorIs(transition, Pawn, certifiedActorRole) &&
              consequence.goalSubjectFacts.exists(
                passedPawnManufacture(_, transition.moveUci, transition.perspective)
              )
            )(PlanCausalGoalMechanism.PassedPawnManufacture)
          case PlanKind.InvasionTransition =>
            Option.when(seventhRankInvasionResult(transition, consequence, certifiedActorRole))(
              PlanCausalGoalMechanism.SeventhRankInvasion
            )
          case _ =>
            None

  private def pawnAdvanceMechanism(
      kind: TransitionConsequenceKind
  ): Option[PlanCausalGoalMechanism] =
    kind match
      case TransitionConsequenceKind.PawnTensionCreated =>
        Some(PlanCausalGoalMechanism.PawnTensionCreation)
      case TransitionConsequenceKind.PawnTensionResolution =>
        Some(PlanCausalGoalMechanism.PawnTensionResolution)
      case TransitionConsequenceKind.PassedPawnProgress =>
        Some(PlanCausalGoalMechanism.PawnPassedStatusProgress)
      case _ => None

  private def seventhRankInvasionResult(
      transition: StructuralTransitionBinding,
      consequence: TransitionConsequence,
      certifiedActorRole: Option[String]
  ): Boolean =
    val destination = EvidenceRef.normalizeMove(transition.moveUci).slice(2, 4)
    consequence.kind == TransitionConsequenceKind.FileOccupationEstablished &&
      transitionActorIs(transition, Rook, certifiedActorRole) &&
      Square.fromKey(destination).exists(square =>
        (if transition.perspective.white then square.rank.value + 1 else 8 - square.rank.value) == 7
      ) &&
      consequence.goalSubjectFacts.exists {
        case StructuralSubject.FileOccupation(_, square, _) => square.key.equalsIgnoreCase(destination)
        case _                                              => false
      }

  private def dependencyMechanism(
      plan: Plan,
      dependency: PlanCausalEventDependency,
      consequence: TransitionConsequence
  ): Option[PlanCausalGoalMechanism] =
    import TransitionConsequenceKind.*
    if consequence.kind == OpponentMobilityRestriction then None
    else dependency.proof match
      case PlanCausalDependencyProof.ObjectState(trajectory) =>
        val sameMovedPiece =
          dependency.to.identity.actorRole.exists(_.equalsIgnoreCase(trajectory.pieceRole.name)) &&
            EvidenceRef.sameMove(dependency.to.moveUci, trajectory.futureStep.moveUci)
        Option.when(
          plan.theme == PlanTheme.PieceRedeployment &&
            sameMovedPiece &&
            !PlanCausalEpisode.triggerMoveCapturesPiece(dependency.to) &&
            movedPieceRouteMechanism(dependency.to, consequence).nonEmpty
        )(PlanCausalGoalMechanism.ObjectStatePieceRoute)
      case PlanCausalDependencyProof.LineAccess(trajectory)
          if trajectory.enabledPieceRole.name.equalsIgnoreCase(_root_.chess.Rook.toString) &&
            dependency.from.identity.kind == PlanKind.RookFileTransfer =>
        val mechanism = consequence.kind match
          case FileOccupationEstablished => Some(PlanCausalGoalMechanism.LineAccessRookFileOccupation)
          case BatteryFormation     => Some(PlanCausalGoalMechanism.LineAccessRookBattery)
          case _                    => None
        mechanism.filter(_ =>
          consequence.establishesState &&
            consequence.strength > 0 &&
            resultBoundToFutureMove(
              dependency.to,
              trajectory.enabledPieceRole,
              trajectory.enabledFrom,
              trajectory.enabledTo,
              consequence
            )
        )
      case PlanCausalDependencyProof.LineAccess(trajectory)
          if plan.theme == PlanTheme.PieceRedeployment =>
        Option.when(
          dependency.to.identity.actorRole.exists(_.equalsIgnoreCase(trajectory.enabledPieceRole.name)) &&
            EvidenceRef.sameMove(dependency.to.moveUci, trajectory.enabledStep.moveUci) &&
            !PlanCausalEpisode.triggerMoveCapturesPiece(dependency.to) &&
            movedPieceRouteMechanism(dependency.to, consequence).nonEmpty
        )(PlanCausalGoalMechanism.LineAccessPieceRoute)
      case PlanCausalDependencyProof.ResponseContinuation(pawn: PawnBreakFollowUpTrajectory)
          if Set(PlanTheme.PawnBreakPreparation, PlanTheme.AdvantageTransformation)(plan.theme) =>
        val resultSquares = PlanCausalEpisode.consequenceSquares(consequence).map(_.key.toLowerCase).toSet
        Option.when(
          pawn.kind == PawnBreakFollowUpKind.ReleasedPassedPawn &&
            consequence.kind == PassedPawnProgress &&
            (resultSquares(pawn.releasedPassedPawn.key.toLowerCase) ||
              resultSquares(pawn.followUpFrom.key.toLowerCase) ||
              resultSquares(pawn.followUpTo.key.toLowerCase))
        )(PlanCausalGoalMechanism.ReleasedPassedPawnContinuation)
      case _ => None

  def movedPieceCreatesRouteResult(
      sourceEvent: PlanCausalEventNode,
      consequence: TransitionConsequence
  ): Boolean =
    movedPieceRouteMechanism(sourceEvent, consequence).nonEmpty

  private def movedPieceRouteMechanism(
      sourceEvent: PlanCausalEventNode,
      consequence: TransitionConsequence
  ): Option[PlanCausalGoalMechanism] =
    import TransitionConsequenceKind.*
    val move = EvidenceRef.normalizeMove(sourceEvent.moveUci)
    val destination = move.slice(2, 4)
    val actorRole = sourceEvent.identity.actorRole.map(_.toLowerCase)
    val movedPieceCreatedBattery =
      consequence.subjectFacts.exists {
        case StructuralSubject.Battery(detail) =>
          actorRole.exists(role =>
            List(detail.attackerRole, detail.occupants.head.role).exists(_.name.equalsIgnoreCase(role))
          ) && List(detail.attackerSquare, detail.occupants.head.square).exists(_.key.equalsIgnoreCase(destination))
        case _ => false
      }
    Option.when(
      actorRole.exists(role => !role.equalsIgnoreCase(_root_.chess.Pawn.toString)) &&
        consequence.establishesState &&
        consequence.strength > 0
    )(
      consequence.kind match
        case BatteryFormation =>
          Option.when(movedPieceCreatedBattery)(PlanCausalGoalMechanism.LineAccessRookBattery)
        case FileOccupationEstablished =>
          Option.when(
            consequence.goalSubjectFacts.exists {
              case StructuralSubject.FileOccupation(_, square, _) => square.key.equalsIgnoreCase(destination)
              case _                                              => false
            }
          )(PlanCausalGoalMechanism.LineAccessRookFileOccupation)
        case _ =>
          None
    ).flatten

  private def resultBoundToFutureMove(
      event: PlanCausalEventNode,
      role: EvidencePieceRole,
      from: EvidenceSquare,
      to: EvidenceSquare,
      consequence: TransitionConsequence
  ): Boolean =
    val move = EvidenceRef.normalizeMove(event.moveUci)
    val actorMatches =
      event.identity.actorRole.exists(_.equalsIgnoreCase(role.name)) &&
        move.take(2).equalsIgnoreCase(from.key) &&
        move.slice(2, 4).equalsIgnoreCase(to.key)
    val targets = EvidenceObjectBinding.goalTargetObjects(consequence)
    val routeTarget = targets.exists(target =>
      target.kind == EvidenceObjectKind.Square && target.key.equalsIgnoreCase(to.key) ||
        target.kind == EvidenceObjectKind.File && target.key.equalsIgnoreCase(to.key.take(1))
    )
    val batteryActor = consequence.subjectFacts.exists {
      case StructuralSubject.Battery(detail) =>
        List(detail.attackerSquare, detail.occupants.head.square).exists(_.key.equalsIgnoreCase(to.key)) &&
          List(detail.attackerRole, detail.occupants.head.role).exists(_.name.equalsIgnoreCase(role.name))
      case _ => false
    }
    actorMatches && (routeTarget || batteryActor)

  private def transitionActorIs(
      transition: StructuralTransitionBinding,
      role: Role,
      certifiedActorRole: Option[String]
  ): Boolean =
    transition.actorRole.map(_.name).orElse(certifiedActorRole)
      .exists(_.equalsIgnoreCase(role.name))

  /** General pawn-continuation result.  It binds the actual pawn transition
    * to a typed board delta and deliberately knows nothing about named board
    * zones such as "centre" or "wing".
    */
  private def pawnAdvanceResult(
      transition: StructuralTransitionBinding,
      consequence: TransitionConsequence,
      certifiedActorRole: Option[String]
  ): Boolean =
    val move = EvidenceRef.normalizeMove(transition.moveUci)
    (for
      from <- Square.fromKey(move.take(2))
      to <- Square.fromKey(move.slice(2, 4))
      if transitionActorIs(transition, Pawn, certifiedActorRole)
      if BoardGeometry.isOneOrInitialTwoStepPawnAdvance(from, to, transition.perspective)
      if consequence.subjectFacts.exists(subject =>
        pawnAdvanceSubjectMatches(consequence.kind, subject, move, from.key, to.key, transition.perspective)
      )
    yield true).contains(true)

  private def pawnAdvanceSubjectMatches(
      kind: TransitionConsequenceKind,
      subject: StructuralSubject,
      move: String,
      from: String,
      to: String,
      side: Color
  ): Boolean =
    kind match
      case TransitionConsequenceKind.OpenFileEstablished | TransitionConsequenceKind.SemiOpenFileEstablished =>
        subject match
          case StructuralSubject.OpenFile(file)     => file.key.equalsIgnoreCase(to.take(1))
          case StructuralSubject.SemiOpenFile(file) => file.key.equalsIgnoreCase(to.take(1))
          case _                                    => false
      case TransitionConsequenceKind.PawnTensionCreated | TransitionConsequenceKind.PawnTensionResolution =>
        subject match
          case StructuralSubject.PawnTensionCreated(attacker, _) => attacker.key.equalsIgnoreCase(to)
          case StructuralSubject.PawnTensionResolved(attacker, _) => attacker.key.equalsIgnoreCase(to)
          case _ => false
      case TransitionConsequenceKind.PassedPawnProgress =>
        pawnAdvancePassedPawnResult(subject, from, to, side)
      case _ =>
        false

  private def passedPawnConversionBy(subject: StructuralSubject, moveUci: String, side: Color): Boolean =
    val move = EvidenceRef.normalizeMove(moveUci)
    subject match
      case StructuralSubject.PassedPawnAdvanced(owner, from, to, rank) =>
        owner == side && ordinaryMoveMatches(from, to, move) && relativeRank(to, side) == rank
      case StructuralSubject.PassedPawnPromoted(owner, from, to) =>
        owner == side && promotedMoveMatches(from, to, move) && relativeRank(to, side) == 8
      case _ => false

  private def passedPawnManufacture(subject: StructuralSubject, moveUci: String, side: Color): Boolean =
    val move = EvidenceRef.normalizeMove(moveUci)
    subject match
      case StructuralSubject.PassedStatusCreated(owner, from, to, rank) =>
        owner == side && ordinaryMoveMatches(from, to, move) && relativeRank(to, side) == rank
      case StructuralSubject.PassedPawnCreated(owner, square) =>
        owner == side && move.length >= 4 && square.key.equalsIgnoreCase(move.slice(2, 4))
      case _ => false

  private def pawnAdvancePassedPawnResult(
      subject: StructuralSubject,
      from: String,
      to: String,
      side: Color
  ): Boolean =
    subject match
      case StructuralSubject.PassedPawnAdvanced(owner, routeFrom, routeTo, rank) =>
        owner == side && routeFrom.key.equalsIgnoreCase(from) && routeTo.key.equalsIgnoreCase(to) &&
          relativeRank(routeTo, side) == rank
      case StructuralSubject.PassedStatusCreated(owner, routeFrom, routeTo, rank) =>
        owner == side && routeFrom.key.equalsIgnoreCase(from) && routeTo.key.equalsIgnoreCase(to) &&
          relativeRank(routeTo, side) == rank
      case StructuralSubject.PassedPawnPromoted(owner, routeFrom, routeTo) =>
        owner == side && routeFrom.key.equalsIgnoreCase(from) && routeTo.key.equalsIgnoreCase(to) &&
          relativeRank(routeTo, side) == 8
      case StructuralSubject.PassedPawnCreated(owner, square) =>
        owner == side && square.key.equalsIgnoreCase(to)
      case _ => false

  private def ordinaryMoveMatches(from: EvidenceSquare, to: EvidenceSquare, move: String): Boolean =
    move.length == 4 && from.key.equalsIgnoreCase(move.take(2)) && to.key.equalsIgnoreCase(move.slice(2, 4))

  private def promotedMoveMatches(from: EvidenceSquare, to: EvidenceSquare, move: String): Boolean =
    move.length == 5 && from.key.equalsIgnoreCase(move.take(2)) && to.key.equalsIgnoreCase(move.slice(2, 4)) &&
      "qrbn".contains(move.last)

  private def relativeRank(square: EvidenceSquare, side: Color): Int =
    Square.fromKey(square.key)
      .map(value => if side.white then value.rank.value + 1 else 8 - value.rank.value)
      .getOrElse(0)

final case class CandidateComparisonEvidence(
    comparison: CandidateComparisonFact
) extends EvidencePayload

final case class RelativeAssessmentEvidence(
    assessment: RelativeMoveAssessment
) extends EvidencePayload

final case class RelativeCauseFactEvidence(
    cause: RelativeCauseFact
) extends EvidencePayload

final case class EvidenceRecord(
    ref: EvidenceRef,
    payload: EvidencePayload,
    parents: List[EvidenceRef] = Nil
):
  def payloadLineRefs: List[LineNodeRef] =
    payload match
      case lineFact: LineFactEvidence =>
        List(lineFact.line)
      case payload: PlanCausalEventEvidence =>
        payload.rootLine :: (
          payload.branchWitnesses.map(_.line) ++
            payload.opponentResourceDeterrence.toList.flatMap(proof =>
              proof.resourceLine :: proof.comparisons.map(_.resourceLine)
            )
        )
      case CandidateLineEvaluationEvidence(payloadLine, _) =>
        List(payloadLine)
      case CandidateComparisonEvidence(fact) =>
        List(fact.referenceLine, fact.candidateLine)
      case RelativeAssessmentEvidence(assessment) =>
        List(assessment.reference.ref, assessment.candidate.ref)
      case RelativeCauseFactEvidence(_) =>
        ref.line.toList
      case payload: TacticalMechanismEvidence =>
        payload.line.toList
      case _: StrategicMechanismEvidence =>
        ref.line.toList
      case payload: StrategicMechanismContrastEvidence =>
        List(payload.referenceLine, payload.candidateLine)
      case _ =>
        Nil
  def referencesLine(line: LineNodeRef): Boolean =
    ref.line.contains(line) || payloadLineRefs.contains(line)
  def carriesLinePayload(line: LineNodeRef, layer: EvidenceLayer): Boolean =
    ref.layer == layer && ref.line.contains(line) && payloadLineRefs.contains(line)
  def mentionsMove(moveUci: String): Boolean =
    payload match
      case MoveTransitionEvidence(move, _, _) =>
        EvidenceRef.sameMove(move, moveUci)
      case relation: RelationFactEvidence =>
        relation.mentionsLineMove(moveUci) || ref.scope == EvidenceScope.PlayedTransition
      case mechanism: TacticalMechanismEvidence =>
        mechanism.moveUci.exists(EvidenceRef.sameMove(_, moveUci)) ||
          mechanism.line.exists(line => EvidenceRef.sameMove(line.rootMove, moveUci)) ||
          ref.scope == EvidenceScope.PlayedTransition
      case _ =>
        false
  def hasRootCaptureEvent(rootMove: String): Boolean =
    payload match
      case payload: LineFactEvidence =>
        payload.hasRootCaptureEvent(rootMove)
      case _ =>
        false

object EvidenceRecord:
  def rootCaptureRecords(records: List[EvidenceRecord], rootMove: String): List[EvidenceRecord] =
    records.filter(_.hasRootCaptureEvent(rootMove))

  def hasRootCaptureEvent(records: List[EvidenceRecord], rootMove: String): Boolean =
    rootCaptureRecords(records, rootMove).nonEmpty

final case class CanonicalRelationNode private[judgment] (
    record: EvidenceRecord,
    relation: RelationFactEvidence
):
  def ref: EvidenceRef = record.ref
  def participants: List[RelationParticipant] = relation.participants
  def semanticKey: String =
    val replayOccurrence = record.parents.find(parent =>
      parent.producer == EvidenceProducer.RelationProducer &&
        parent.layer == EvidenceLayer.Relation &&
        parent.confidence == EvidenceConfidence.LegalReplayVerified
    ).map(_.id)
    List(
      CanonicalRelationGraph.positionOccurrenceKey(ref.position),
      replayOccurrence
        .orElse(ref.line.map(SemanticLineKey.from).map(_.stableKey))
        .getOrElse("position"),
      ref.scope.toString,
      relation.semanticId
    ).mkString(":")

private[chessjudgment] final case class CanonicalPositionRelationSnapshot private[judgment] (
    position: PositionNodeRef,
    scope: EvidenceScope,
    private[judgment] val nodesBySemanticId: scala.collection.immutable.VectorMap[String, CanonicalRelationNode],
    private[chessjudgment] val inventory: PositionRelationExtractor.PositionRelationInventoryCertificate
):
  def nodes: List[CanonicalRelationNode] = nodesBySemanticId.valuesIterator.toList

  private[chessjudgment] def nodeFor(
      relation: RelationFactEvidence
  ): Option[CanonicalRelationNode] =
    nodesBySemanticId.get(relation.semanticId).filter(_.relation == relation)

  private[chessjudgment] def occupantAt(
      square: EvidenceSquare
  ): Option[RelationColoredPieceWitness] =
    inventory.occupantAt(square)

  private[chessjudgment] def sideToMove: Color = inventory.sideToMove

  private[chessjudgment] def proveAbsence(
      query: PositionRelationExtractor.ClosedRelationAbsenceQuery
  ): Option[PositionRelationExtractor.ClosedRelationAbsenceProof] =
    inventory.proveAbsence(query, position, scope)

final class CanonicalRelationGraph private (
    private val nodeInventory: Vector[CanonicalRelationNode],
    private val evidenceIndex: Map[String, CanonicalRelationNode],
    private val semanticIndex: Map[String, CanonicalRelationNode],
    private val occurrenceIndex: Map[String, Vector[CanonicalRelationNode]],
    private val positionRelationIndex: Map[
      (String, EvidenceScope),
      scala.collection.immutable.VectorMap[String, CanonicalRelationNode]
    ],
    private val squareIndex: Map[EvidenceSquare, Vector[CanonicalRelationNode]]
):
  lazy val nodes: List[CanonicalRelationNode] = nodeInventory.toList

  val byEvidenceId: Map[String, CanonicalRelationNode] = evidenceIndex

  def at(position: PositionNodeRef): List[CanonicalRelationNode] =
    occurrenceIndex
      .getOrElse(CanonicalRelationGraph.occurrenceKey(position), Vector.empty)
      .toList

  def positionRelationsAt(position: PositionNodeRef): List[CanonicalRelationNode] =
    EvidenceScope.values.toList.flatMap(scope =>
      positionRelationIndex
        .get(CanonicalRelationGraph.occurrenceKey(position) -> scope)
        .toList
        .flatMap(_.valuesIterator)
    )

  def positionRelationsAt(
      position: PositionNodeRef,
      scope: EvidenceScope
  ): List[CanonicalRelationNode] =
    positionRelationIndex
      .get(CanonicalRelationGraph.occurrenceKey(position) -> scope)
      .toList
      .flatMap(_.valuesIterator)

  private[chessjudgment] def closedPositionRelationSnapshot(
      position: PositionNodeRef,
      scope: EvidenceScope,
      inventory: PositionRelationExtractor.PositionRelationInventoryCertificate
  ): CanonicalPositionRelationSnapshot =
    val nodes = positionRelationIndex.getOrElse(
      CanonicalRelationGraph.occurrenceKey(position) -> scope,
      scala.collection.immutable.VectorMap.empty
    )
    require(
      inventory.certifies(position, nodes.iterator.map { case (semanticId, node) => semanticId -> node.relation }),
      s"position '${position.id.getOrElse(position.fen)}' does not contain its exact closed relation inventory"
    )
    CanonicalPositionRelationSnapshot(position, scope, nodes, inventory)

  def nodesByEvidenceIds(ids: Set[String]): List[CanonicalRelationNode] =
    ids.iterator.flatMap(byEvidenceId.get).toList.sortBy(_.ref.id)

  def contains(ref: EvidenceRef, relation: RelationFactEvidence): Boolean =
    byEvidenceId
      .get(ref.id)
      .exists(node => node.ref == ref && node.relation == relation)

  def containsRecord(record: EvidenceRecord): Boolean =
    byEvidenceId
      .get(record.ref.id)
      .exists(_.record == record)

  def touching(square: EvidenceSquare): List[CanonicalRelationNode] =
    squareIndex.getOrElse(square, Vector.empty).toList

  /** Add only newly admitted relation occurrences. Existing graph nodes and
    * indexes are persistent inputs; they are never filtered, regrouped, or
    * revalidated on the incremental path.
    */
  private[judgment] def addAll(
      incoming: IterableOnce[EvidenceRecord]
  ): CanonicalRelationGraph =
    CanonicalRelationGraph.append(this, incoming.iterator.toVector)

object CanonicalRelationGraph:
  private[judgment] def canonicalNode(record: EvidenceRecord): Option[CanonicalRelationNode] =
    record match
      case canonical @ EvidenceRecord(ref, relation: RelationFactEvidence, _)
          if ref.layer == EvidenceLayer.Relation && ref.producer == EvidenceProducer.RelationProducer =>
        Some(CanonicalRelationNode(canonical, relation))
      case _ => None

  private[judgment] def relationShaped(record: EvidenceRecord): Boolean =
    record.payload match
      case _: ClosedRelationOccurrenceEvidence => false
      case _ =>
        record.ref.layer == EvidenceLayer.Relation ||
          record.ref.producer == EvidenceProducer.RelationProducer ||
          record.payload.isInstanceOf[RelationFactEvidence]

  private def ownsClosedPositionSnapshot(node: CanonicalRelationNode): Boolean =
    node.ref.line.isEmpty && node.ref.confidence == EvidenceConfidence.BoardDerived &&
      (node.relation.origin match
        case RelationEvidenceOrigin.PositionSnapshot(_) => true
        case _                                           => false)

  private[judgment] def positionOccurrenceKey(position: PositionNodeRef): String =
    val occurrenceFen = PrincipalVariationEvidence.normalizeFen(position.fen)
    position.id match
      case Some(id) => s"node:$id:$occurrenceFen:${position.ply}"
      case None     => s"anonymous:$occurrenceFen:${position.ply}"

  private[judgment] def occurrenceKey(position: PositionNodeRef): String =
    positionOccurrenceKey(position)

  private[judgment] val empty =
    new CanonicalRelationGraph(
      nodeInventory = Vector.empty,
      evidenceIndex = Map.empty,
      semanticIndex = Map.empty,
      occurrenceIndex = Map.empty,
      positionRelationIndex = Map.empty,
      squareIndex = Map.empty
    )

  private def append(
      graph: CanonicalRelationGraph,
      incoming: Vector[EvidenceRecord]
  ): CanonicalRelationGraph =
    val relationLayerRecords = incoming.filter(relationShaped)
    val invalid = relationLayerRecords.filter(record => canonicalNode(record).isEmpty)
    require(invalid.isEmpty, "Relation layer accepts only canonical RelationProducer records")

    var nodeInventory = graph.nodeInventory
    var evidenceIndex = graph.evidenceIndex
    var semanticIndex = graph.semanticIndex
    var occurrenceIndex = graph.occurrenceIndex
    var positionRelationIndex = graph.positionRelationIndex
    var squareIndex = graph.squareIndex
    var added = 0

    relationLayerRecords.foreach { record =>
      val node = canonicalNode(record).getOrElse(
        throw IllegalArgumentException("canonical relation validation changed during graph insertion")
      )
      require(
        RelationFactEvidence.verified(node.ref, node.relation),
        s"unverified relation cannot enter the canonical graph: ${node.ref.id}"
      )
      evidenceIndex.get(node.ref.id) match
        case Some(existing) if existing == node => ()
        case Some(_) =>
          throw IllegalArgumentException(
            s"canonical relation evidence id collision: ${node.ref.id}"
          )
        case None =>
          semanticIndex.get(node.semanticKey) match
            case Some(_) =>
              throw IllegalArgumentException(
                s"duplicate canonical relation identity: ${node.semanticKey}"
              )
            case None =>
              nodeInventory = nodeInventory :+ node
              evidenceIndex = evidenceIndex.updated(node.ref.id, node)
              semanticIndex = semanticIndex.updated(node.semanticKey, node)
              val nodeOccurrenceKey = occurrenceKey(node.ref.position)
              occurrenceIndex = occurrenceIndex.updated(
                nodeOccurrenceKey,
                occurrenceIndex.getOrElse(nodeOccurrenceKey, Vector.empty) :+ node
              )
              if ownsClosedPositionSnapshot(node) && node.relation.isPositionRelation then
                val positionKey = nodeOccurrenceKey -> node.ref.scope
                val snapshot = positionRelationIndex.getOrElse(
                  positionKey,
                  scala.collection.immutable.VectorMap.empty
                )
                positionRelationIndex = positionRelationIndex.updated(
                  positionKey,
                  snapshot.updated(node.relation.semanticId, node)
                )
              node.participants.map(_.square).distinct.foreach { square =>
                squareIndex = squareIndex.updated(
                  square,
                  squareIndex.getOrElse(square, Vector.empty) :+ node
                )
              }
              added += 1
    }

    if added == 0 then graph
    else
      new CanonicalRelationGraph(
        nodeInventory,
        evidenceIndex,
        semanticIndex,
        occurrenceIndex,
        positionRelationIndex,
        squareIndex
      )

final class TypedEvidenceGraph private (
    private val recordIndex: scala.collection.immutable.VectorMap[String, EvidenceRecord],
    val relationGraph: CanonicalRelationGraph
):
  lazy val records: List[EvidenceRecord] = recordIndex.valuesIterator.toList

  val byId: Map[String, EvidenceRecord] = recordIndex

  private lazy val recordsByPosition: Map[PositionNodeRef, List[EvidenceRecord]] =
    records.groupBy(_.ref.position)

  private lazy val recordsByLine: Map[LineNodeRef, List[EvidenceRecord]] =
    records.flatMap(record => record.ref.line.map(_ -> record)).groupMap(_._1)(_._2)

  private lazy val lineFactsByLine: Map[LineNodeRef, List[LineFactEvidence]] =
    records
      .collect { case EvidenceRecord(_, payload: LineFactEvidence, _) => payload }
      .groupBy(_.line)

  private lazy val candidateEvaluationRecordsByLine
      : Map[LineNodeRef, List[EvidenceRecord]] =
    records
      .collect {
        case record @ EvidenceRecord(_, payload: CandidateLineEvaluationEvidence, _) =>
          payload.line -> record
      }
      .groupMap(_._1)(_._2)

  private lazy val structuralDeltasByLine: Map[LineNodeRef, List[StructuralDeltaEvidence]] =
    records
      .collect {
        case EvidenceRecord(ref, payload: StructuralDeltaEvidence, _) =>
          payload.line.filter(ref.line.contains).map(_ -> payload)
      }
      .flatten
      .groupMap(_._1)(_._2)

  private[chessjudgment] def certifiedLineFactFor(
      line: LineNodeRef
  ): Option[LineFactEvidence] =
    lineFactsByLine.getOrElse(line, Nil) match
      case exact :: Nil if exact.replayIsCertified => Some(exact)
      case _                                     => None

  private[chessjudgment] def certifiedRootActorFor(
      line: LineNodeRef
  ): Option[RootCausalActor] =
    certifiedLineFactFor(line).flatMap(RootCausalActor.fromLineFact(_, line.rootMove))

  private[chessjudgment] def uniqueCandidateEvaluationRecordFor(
      line: LineNodeRef
  ): Option[EvidenceRecord] =
    candidateEvaluationRecordsByLine.getOrElse(line, Nil) match
      case exact :: Nil => Some(exact)
      case _            => None

  private[chessjudgment] def certifiedRootResponseCountFor(
      line: LineNodeRef,
      maximum: Int
  ): Option[Int] =
    structuralDeltasByLine.getOrElse(line, Nil) match
      case exact :: Nil
          if exact.line.contains(line) &&
            EvidenceRef.sameMove(exact.moveUci, line.rootMove) =>
        exact.certifiedRootResponseCount(maximum)
      case _ => None

  def record(ref: EvidenceRef): Option[EvidenceRecord] =
    byId.get(ref.id).filter(_.ref == ref)

  /** Re-materializes only line nodes whose legal line record and canonical evaluation
    * are unambiguous in this graph. Consumers still validate the record
    * producer, parent and scope identities; ambiguity deliberately yields no
    * candidate so proof checks fail closed.
    */
  private[chessjudgment] lazy val canonicalCandidateLinesFromEvidence: List[CandidateLineNode] =
    records.collect { case record @ EvidenceRecord(_, payload: LineFactEvidence, _) =>
      record -> payload
    }.flatMap { case (lineRecord, facts) =>
      records.collect {
        case record @ EvidenceRecord(_, eval: CandidateLineEvaluationEvidence, _) if eval.line == facts.line =>
          record -> eval
      } match
        case (_, eval) :: Nil =>
          List(CandidateLineNode(
            ref = facts.line,
            evaluation = eval.evaluation,
            evidence = lineRecord.ref
          ))
        case _ =>
          Nil
    }

  def recordsFor(position: PositionNodeRef): List[EvidenceRecord] =
    recordsByPosition.getOrElse(position, Nil)

  def recordsFor(line: LineNodeRef): List[EvidenceRecord] =
    recordsByLine.getOrElse(line, Nil)

  private[chessjudgment] def ownedLineConsequences(
      fact: CandidateComparisonFact,
      sourceSide: RelativeCauseSourceSide,
      attributionKind: CauseAttributionKind,
      sourceLabelsByEvidenceId: Map[String, Set[String]] = Map.empty
  ): List[(EvidenceRef, LineConsequence)] =
    TypedEvidenceGraph.ownedLineConsequences(
      records,
      fact,
      sourceSide,
      attributionKind,
      sourceLabelsByEvidenceId
    )

  def relativeCauseProofRecords(section: RelativeCauseProofSection): List[EvidenceRecord] =
    section.sourceRefs.flatMap(ref => record(ref)).distinctBy(_.ref.id)

  def relativeCauseLineConsequences(
      kind: RelativeCauseKind,
      section: RelativeCauseProofSection
  ): List[(EvidenceRef, LineConsequence)] =
    relativeCauseProofRecords(section).flatMap {
      case EvidenceRecord(ref, payload: LineFactEvidence, _) =>
        val consequences = ref.line.toList.flatMap(line =>
          payload.proofConsequenceCandidatesForRootMove(line.rootMove)
        )
        consequences
          .filter(consequence => RelativeCauseKind.acceptsLineConsequence(kind, consequence.kind))
          .map(consequence => ref -> consequence)
      case _ =>
        Nil
    }.distinct

  def relativeCauseOwnedLineEvents(
      cause: RelativeCauseFact,
      section: RelativeCauseProofSection
  ): List[(EvidenceRef, LineMoveEvent)] =
    val sourceIds = relativeCauseProofRecords(section).map(_.ref.id).toSet
    relativeCauseBinding(cause).toList.flatMap { binding =>
      relativeCauseProofRecords(section).flatMap {
        case EvidenceRecord(ref, payload: LineFactEvidence, _)
            if sourceIds(ref.id) && ref.line.contains(binding.eventLine) && payload.line == binding.eventLine =>
          payload.eventsForRootMove(binding.eventLine.rootMove).map(ref -> _)
        case _ =>
          Nil
      }
    }.distinct

  def relativeCauseOwnedLineConsequences(
      cause: RelativeCauseFact,
      section: RelativeCauseProofSection
  ): List[(EvidenceRef, LineConsequence)] =
    val sourceIds = relativeCauseProofRecords(section).map(_.ref.id).toSet
    (for
      fact <- comparisonFor(cause).toList
      binding <- relativeCauseBinding(cause).toList
      (ref, consequence) <- ownedLineConsequences(fact, cause.sourceSide, cause.attribution.kind)
      if sourceIds(ref.id)
      payload <- record(ref).toList.collect { case EvidenceRecord(_, line: LineFactEvidence, _) => line }
      if ref.line.contains(binding.eventLine) && payload.line == binding.eventLine
      if RelativeCauseKind.acceptsDirectLineConsequence(
        cause.kind,
        payload,
        binding.eventLine.rootMove,
        consequence
      )
    yield ref -> consequence).distinct

  def relativeCauseRootOwnedCausalEpisodes(
      cause: RelativeCauseFact,
      section: RelativeCauseProofSection
  ): List[(EvidenceRef, RootOwnedCausalEpisode)] =
    val ownedConsequences = relativeCauseOwnedLineConsequences(cause, section).toSet
    relativeCauseBinding(cause).toList.flatMap { binding =>
      relativeCauseProofRecords(section).flatMap {
        case EvidenceRecord(ref, payload: LineFactEvidence, _)
            if ref.line.contains(binding.eventLine) && payload.line == binding.eventLine =>
          payload
            .rootOwnedCausalEpisodes(binding.eventLine.rootMove)
            .filter(episode => ownedConsequences(ref -> episode.consequence))
            .map(ref -> _)
        case _ =>
          Nil
      }
    }.distinct

  def relativeCauseRelations(section: RelativeCauseProofSection): List[(EvidenceRef, RelationFactEvidence)] =
    val proofIds = relativeCauseProofRecords(section).map(_.ref.id).toSet
    proofEligibleRelationNodesByEvidenceIds(proofIds)
      .map(node => node.ref -> node.relation)
      .distinct

  def relativeCauseTacticalMechanisms(
      section: RelativeCauseProofSection
  ): List[(EvidenceRef, TacticalMechanismEvidence)] =
    relativeCauseProofRecords(section).collect {
      case EvidenceRecord(ref, payload: TacticalMechanismEvidence, _) if payload.hasConcreteProof =>
        ref -> payload
    }.distinct

  def relativeCauseStrategicMechanisms(
      section: RelativeCauseProofSection
  ): List[(EvidenceRef, StrategicMechanismEvidence)] =
    relativeCauseProofRecords(section).collect {
      case record @ EvidenceRecord(ref, payload: StrategicMechanismEvidence, _)
          if proofEligible(record) && payload.canSupportStrategicCause =>
        ref -> payload
    }.distinct

  def relativeCauseStrategicContrasts(
      section: RelativeCauseProofSection
  ): List[(EvidenceRef, StrategicMechanismContrastEvidence)] =
    relativeCauseProofRecords(section).collect {
      case EvidenceRecord(ref, payload: StrategicMechanismContrastEvidence, _)
          if payload.axisComparisons.exists(_.hasContrast) =>
        ref -> payload
    }.distinct

  private def relativeCauseStrategicSignalsForCause(
      cause: RelativeCauseFact,
      payload: StrategicMechanismEvidence
  ): List[StrategicMechanismSignal] =
    if !cause.strategicCauseKind then payload.signals
    else
      payload.signals.filter(signal =>
        signal.axis.exists(axis =>
          RelativeCauseKind.strategicAxisCanProveCause(cause.kind, axis, cause.sourceSide)
        )
      )

  private def relativeCauseStrategicComparisonsForCause(
      cause: RelativeCauseFact,
      payload: StrategicMechanismContrastEvidence
  ): List[StrategicAxisComparison] =
    if cause.strategicCauseKind then
      payload.sustainedCauseComparisons(cause.kind, cause.sourceSide)
    else payload.sustainedActionableComparisonsFor(cause.sourceSide)

  def relativeCauseStrategicAxisComparisons(
      cause: RelativeCauseFact,
      section: RelativeCauseProofSection
  ): List[(EvidenceRef, StrategicAxisComparison)] =
    relativeCauseStrategicContrasts(section).flatMap { case (ref, payload) =>
      relativeCauseStrategicComparisonsForCause(cause, payload).map(ref -> _)
    }.distinct

  private def strategicAxisLeafSourceRefs(
      refs: List[EvidenceRef],
      axisFilter: StrategicAxisDetail => Boolean,
      visited: Set[String] = Set.empty
  ): List[EvidenceRef] =
    refs.flatMap { ref =>
      if visited(ref.id) then Nil
      else
        record(ref) match
          case Some(EvidenceRecord(_, mechanism: StrategicMechanismEvidence, _)) =>
            mechanism.signals
              .filter(signal => signal.axis.exists(axisFilter))
              .flatMap(signal => strategicAxisLeafSourceRefs(List(signal.source), axisFilter, visited + ref.id))
          case Some(_) =>
            List(ref)
          case None =>
            Nil
    }.distinctBy(_.id)

  private[chessjudgment] def strategicAxisLeafSourceRefs(
      comparison: StrategicAxisComparison
  ): List[EvidenceRef] =
    strategicAxisLeafSourceRefs(
      comparison.sources,
      axis => axis.stableKey == comparison.axis.stableKey
    )

  private[chessjudgment] def strategicComparisonSourceRefs(
      comparison: StrategicAxisComparison,
      sourceSide: RelativeCauseSourceSide
  ): List[EvidenceRef] =
    strategicAxisLeafSourceRefs(
      comparison.sourcesFor(sourceSide),
      axis => axis.stableKey == comparison.axis.stableKey
    )

  def strategicComparisonPlanEventRefs(
      comparison: StrategicAxisComparison,
      sourceSide: RelativeCauseSourceSide,
      eventLine: LineNodeRef
  ): List[EvidenceRef] =
    strategicComparisonSourceRefs(comparison, sourceSide).filter(ref =>
      ref.layer == EvidenceLayer.PlanCausalEvent &&
        ref.line.contains(eventLine) &&
        record(ref).exists {
          case eventRecord @ EvidenceRecord(_, event: PlanCausalEventEvidence, _) =>
            proofEligible(eventRecord) &&
              event.rootLine == eventLine && EvidenceRef.sameMove(event.rootMove, eventLine.rootMove)
          case _ =>
            false
        }
    )

  def relativeCauseDefensiveRecaptureResources(
      kind: RelativeCauseKind,
      section: RelativeCauseProofSection
  ): List[(EvidenceRef, CandidateComparisonFact, PlayedVsBestDefensiveRecaptureResource)] =
    Option
      .when(kind == RelativeCauseKind.DefensiveResource)(
        relativeCauseProofRecords(section).flatMap {
          case EvidenceRecord(ref, CandidateComparisonEvidence(comparison), _) =>
            comparison.defensiveRecaptureResource.map(resource => (ref, comparison, resource)).toList
          case _ => Nil
        }.distinct
      )
      .getOrElse(Nil)

  def relativeCauseProofSectionHasConcreteProof(
      kind: RelativeCauseKind,
      section: RelativeCauseProofSection
  ): Boolean =
    val selectedMoveOrderPlanResult =
      section.role == RelativeCauseProofRole.DirectProof &&
        kind == RelativeCauseKind.WrongMoveOrder &&
        relativeCauseProofRecords(section).exists {
          case EvidenceRecord(_, event: PlanCausalEventEvidence, _) =>
            event.exactRobustPublicResultAssessments.nonEmpty
          case _ =>
            false
        }
    relativeCauseLineConsequences(kind, section).nonEmpty ||
      relativeCauseRelations(section).nonEmpty ||
      relativeCauseTacticalMechanisms(section).nonEmpty ||
      relativeCauseStrategicMechanisms(section).nonEmpty ||
      relativeCauseStrategicContrasts(section).nonEmpty ||
      relativeCauseDefensiveRecaptureResources(kind, section).nonEmpty ||
      selectedMoveOrderPlanResult

  def relativeCauseProofHasRawTypedDepth(kind: RelativeCauseKind, proof: RelativeCauseProof): Boolean =
    relativeCauseProofHasRawDirectProof(kind, proof) ||
      relativeCauseProofSectionHasConcreteProof(kind, proof.contrastProof)

  def relativeCauseProofHasRawDirectProof(kind: RelativeCauseKind, proof: RelativeCauseProof): Boolean =
    relativeCauseProofSectionHasConcreteProof(kind, proof.directProof)


  def relativeCauseProofHasRawContextSupport(proof: RelativeCauseProof): Boolean =
    relativeCauseProofRecords(proof.contextSupport).nonEmpty



  def relativeCauseHasOwnedTypedDepth(cause: RelativeCauseFact): Boolean =
    cause.attribution.directProofEligible &&
      cause.proof.exists(proof => relativeCauseProofSectionHasConcreteProof(cause.kind, proof.directProof))

  def relativeCauseHasOwnedAdmissibleLongTermProof(cause: RelativeCauseFact): Boolean =
    cause.attribution.directProofEligible &&
      cause.strategicCauseKind &&
      cause.proof.exists(proof =>
        relativeCauseStrategicAxisComparisons(cause, proof.directProof).nonEmpty ||
          EvidenceObjectBinding
            .rawDirectSentenceChannelsForProjection(cause, this)
            .exists(relativeCauseChannelHasOwnedStrategicAuthority(cause, _))
      )

  /** A strategic Cause may expose a channel only when that exact strategic
    * wrapper owns its long-term proof in the Cause's direct section. Another
    * admitted or filtered-out axis cannot lend its comparison authority.
    */
  def relativeCauseStrategicChannelHasOwnedLongTermProof(
      cause: RelativeCauseFact,
      channel: DirectCauseChannel
  ): Boolean =
    relativeCauseChannelHasOwnedStrategicAuthority(cause, channel)

  /** One central strategic authority boundary is shared by C admission and all
    * later public-readiness checks. A strategic Cause requires either an exact
    * selected plan result or its own sustained-axis comparison.
    */
  private def relativeCauseChannelHasOwnedStrategicAuthority(
      cause: RelativeCauseFact,
      channel: DirectCauseChannel
  ): Boolean =
    val directSection = cause.proof.map(_.directProof)
    val eventLine = relativeCauseBinding(cause).map(_.eventLine)
    val exactPlanResultAuthority =
      (directSection, eventLine, channel.rootOwnedProof.flatMap(RootOwnedEffectPolicy.exactPlanResultPrimitive)) match
        case (Some(section), Some(line), Some((source, event, assessment, selectedInducedResponse))) =>
          cause.attribution.directProofEligible &&
            cause.strategicCauseKind &&
            selectedInducedResponse.isEmpty &&
            channel.binding.line.contains(line) &&
            section.sourceRefs.exists(_.id == source.id) &&
            channel.binding.source == source &&
            RootOwnedEffectPolicy
              .planResultProofs(cause, source, event)
              .exists(_._1 == assessment) &&
            RootOwnedEffectPolicy.admits(cause, this, channel)
        case _ => false
    val sustainedAxisAuthority =
      (directSection, eventLine, channel.rootOwnedProof) match
        case (
              Some(section),
              Some(line),
              Some(RootOwnedEffectProof.StrategicAxis(primitive, axis, outcome))
            )
            if cause.attribution.directProofEligible &&
              cause.strategicCauseKind &&
              channel.binding.line.contains(line) &&
              RelativeCauseKind.strategicAxisCanProveCause(cause.kind, axis, cause.sourceSide) =>
          val primitiveSource = primitive.primitiveSource
          relativeCauseStrategicAxisComparisons(cause, section).exists {
            case (carrier, comparison) =>
              carrier.id == channel.binding.source.id &&
                comparison.axis.stableKey == axis.stableKey &&
                outcome.contains(comparison.outcome) &&
                comparison.sourcesFor(cause.sourceSide).nonEmpty &&
                strategicComparisonSourceRefs(comparison, cause.sourceSide).exists(
                  _.id == primitiveSource.id
                ) &&
                RootOwnedEffectPolicy.strategicPrimitiveAuthorized(
                  primitive,
                  axis,
                  comparison.planResultsFor(cause.sourceSide)
                )
          }
        case _ =>
          false
    exactPlanResultAuthority || sustainedAxisAuthority

  def relativeCauseHasOwnedTacticalProof(cause: RelativeCauseFact): Boolean =
    cause.attribution.directProofEligible &&
      cause.proof.exists(proof =>
        relativeCauseTacticalMechanisms(proof.directProof).nonEmpty ||
          relativeCauseRelations(proof.directProof).exists { case (_, payload) =>
            payload.hasConcreteWitness && payload.hasLineProof
          } ||
          relativeCauseOwnedLineConsequences(cause, proof.directProof)
            .exists { case (_, consequence) => LineConsequenceKind.tacticalDriver(consequence.kind) }
      )

  def relativeCauseStrategicProofIdentity(
      cause: RelativeCauseFact
  ): RelativeCauseStrategicProofIdentity =
    cause.proof match
      case None =>
        RelativeCauseStrategicProofIdentity.empty
      case Some(proof) =>
        val mechanisms =
          proof.sections
            .flatMap(relativeCauseStrategicMechanisms)
            .flatMap { case (ref, payload) =>
              val signals = relativeCauseStrategicSignalsForCause(cause, payload)
              Option.when(signals.nonEmpty)((ref, payload, signals))
            }
            .distinctBy(_._1.id)
        val contrasts =
          proof.sections
            .flatMap(relativeCauseStrategicContrasts)
            .flatMap { case (ref, payload) =>
              val comparisons = relativeCauseStrategicComparisonsForCause(cause, payload)
              Option.when(comparisons.nonEmpty)((ref, payload, comparisons))
            }
            .distinctBy(_._1.id)
        RelativeCauseStrategicProofIdentity(
          axisKeys = (
            mechanisms.flatMap(_._3.flatMap(_.axisKey)) ++
              contrasts.flatMap(_._3.map(_.axisKey))
          ).distinct.sorted,
          mechanismKinds = mechanisms.map(_._2.kind).distinct.sortBy(_.toString),
          mechanismSourceIds = (mechanisms.map(_._1.id) ++ contrasts.map(_._1.id)).distinct.sorted,
          signalSourceIds = (
            mechanisms.flatMap(_._3.map(_.source.id)) ++
              contrasts.flatMap(_._3.flatMap(_.sourcesFor(cause.sourceSide).map(_.id)))
          ).distinct.sorted
        )

  def candidateComparisonRecord(ref: EvidenceRef): Option[EvidenceRecord] =
    record(ref)
      .filter(_.payload.isInstanceOf[CandidateComparisonEvidence])

  def candidateComparison(ref: EvidenceRef): Option[CandidateComparisonFact] =
    candidateComparisonRecord(ref).collect {
      case EvidenceRecord(_, CandidateComparisonEvidence(fact), _) => fact
    }

  def comparisonFor(assessment: RelativeMoveAssessment): Option[CandidateComparisonFact] =
    candidateComparison(assessment.primaryComparisonEvidence)

  def comparisonFor(cause: RelativeCauseFact): Option[CandidateComparisonFact] =
    candidateComparison(cause.comparisonEvidence)

  def relativeCauseBinding(cause: RelativeCauseFact): Option[RelativeCauseBinding] =
    comparisonFor(cause).map(comparison => RelativeCauseFact.binding(cause, comparison))

  def requiredRelativeCauseBinding(cause: RelativeCauseFact): RelativeCauseBinding =
    relativeCauseBinding(cause).getOrElse(
      throw IllegalArgumentException(
        s"relative cause '${cause.comparisonEvidence.id}' is not bound to a candidate comparison"
      )
    )

  def candidateSetFor(fact: CandidateComparisonFact): Option[CandidateSetDescriptor] =
    fact.candidateSet.orElse {
      if fact.kind != CandidateComparisonKind.PlayedVsBest then None
      else
        val requesterParents =
          records.collect {
            case EvidenceRecord(_, CandidateComparisonEvidence(candidateFact), parents)
                if candidateFact == fact =>
              parents.map(_.id)
          }.flatten.toSet
        val owners =
          records.collect {
            case EvidenceRecord(ref, CandidateComparisonEvidence(candidateFact), _)
                if requesterParents(ref.id) && candidateFact.candidateSet.nonEmpty =>
              candidateFact.candidateSet.get
          }
        owners match
          case descriptor :: Nil => Some(descriptor)
          case Nil                => None
          case _ =>
            throw IllegalStateException(
              s"multiple candidate-set owners for comparison '${fact.referenceLine.id}->${fact.candidateLine.id}'"
            )
    }

  def candidateSetFor(assessment: RelativeMoveAssessment): Option[CandidateSetDescriptor] =
    comparisonFor(assessment).flatMap(candidateSetFor)

  def parentClosure(record: EvidenceRecord): List[EvidenceRecord] =
    def loop(refs: List[EvidenceRef], seen: Set[String]): List[EvidenceRecord] =
      refs.flatMap { ref =>
        if seen.contains(ref.id) then Nil
        else byId.get(ref.id).toList.flatMap(parent => parent :: loop(parent.parents, seen + ref.id))
      }
    loop(record.parents, Set.empty).distinctBy(_.ref.id)

  private lazy val proofEligibilityById: Map[String, Boolean] =
    val resolved = scala.collection.mutable.Map.empty[String, Boolean]
    def resolve(record: EvidenceRecord, visiting: Set[String]): Boolean =
      resolved.get(record.ref.id) match
        case Some(eligible) => eligible
        case None =>
          val eligible =
            !visiting(record.ref.id) &&
              intrinsicallyProofEligible(record) &&
              record.parents.forall(parent =>
                byId.get(parent.id).exists(parentRecord =>
                  parentRecord.ref == parent && resolve(parentRecord, visiting + record.ref.id)
                )
              )
          resolved.update(record.ref.id, eligible)
          eligible
    records.map(record => record.ref.id -> resolve(record, Set.empty)).toMap

  def proofEligible(record: EvidenceRecord): Boolean =
    byId.get(record.ref.id).contains(record) &&
      proofEligibilityById.getOrElse(record.ref.id, false)

  /** Canonical relation membership records what was produced. Proof consumers
    * must additionally require that every exact parent and combination premise
    * resolves in this graph.
    */
  private[chessjudgment] def relationProofEligible(record: EvidenceRecord): Boolean =
    relationGraph.containsRecord(record) && proofEligible(record)

  private[chessjudgment] def proofEligibleRelationNodesByEvidenceIds(
      ids: Set[String]
  ): List[CanonicalRelationNode] =
    relationGraph
      .nodesByEvidenceIds(ids)
      .filter(node => relationProofEligible(node.record))

  private def intrinsicallyProofEligible(record: EvidenceRecord): Boolean =
    (record.ref.confidence != EvidenceConfidence.Mixed || record.parents.nonEmpty) &&
      (record.payload match
        case PositionFeatureEvidence(features) =>
          exactAuthority(record, EvidenceProducer.PositionFeatureProducer, EvidenceLayer.PositionFeature) &&
            record.ref.confidence == EvidenceConfidence.BoardDerived &&
            record.ref.line.isEmpty &&
            PrincipalVariationEvidence.sameBoardState(features.fen, record.ref.position.fen) &&
            features.plyCount == record.ref.position.ply &&
            record.ref.position.sideToMove.forall(_ == features.sideToMove)
        case payload: StrategicMechanismEvidence =>
          exactAuthority(record, EvidenceProducer.StrategicMechanismProducer, EvidenceLayer.StrategicMechanism) &&
            payload.hasSignals && payload.semanticAnchors.nonEmpty && payload.exactAssemblyCertified(record)
        case payload: StrategicMechanismContrastEvidence =>
          exactAuthority(record, EvidenceProducer.StrategicMechanismProducer, EvidenceLayer.StrategicMechanism) &&
            payload.hasActionableContrast && payload.exactAssemblyCertified(record)
        case _: OpeningContextEvidence =>
          exactAuthority(record, EvidenceProducer.OpeningContextProducer, EvidenceLayer.OpeningContext)
        case _: FeatureAnchorEvidence =>
          exactAuthority(record, EvidenceProducer.FeatureAnchorProducer, EvidenceLayer.FeatureAnchor)
        case _: ApplicabilityAssessmentEvidence =>
          exactAuthority(
            record,
            EvidenceProducer.ApplicabilityAssessmentProducer,
            EvidenceLayer.ApplicabilityAssessment
          )
        case payload: LineFactEvidence =>
          exactAuthority(record, EvidenceProducer.LegalLineProducer, EvidenceLayer.Line) &&
            record.ref.confidence == EvidenceConfidence.LegalReplayVerified &&
            record.ref.line.contains(payload.line) && record.ref.scope == payload.line.role.scope &&
            payload.replayIsCertified && payload.rootMove.exists(EvidenceRef.sameMove(_, payload.line.rootMove)) &&
            payload.canonicalReplay.exists(_.replaySteps.headOption.exists(step =>
              PrincipalVariationEvidence.sameBoardState(step.fenBefore, record.ref.position.fen)
            ))
        case CandidateLineEvaluationEvidence(line, evaluation) =>
          record.ref.layer == EvidenceLayer.Eval && record.ref.line.contains(line) &&
            record.ref.scope == line.role.scope && (evaluation match
              case lila.chessjudgment.model.line.CandidateLineEvaluation.EngineSearch(_) =>
                record.ref.producer == EvidenceProducer.EngineEvalProducer &&
                  record.ref.confidence == EvidenceConfidence.EngineBacked
              case lila.chessjudgment.model.line.CandidateLineEvaluation.ExactAutomaticTerminal(_, _) =>
                record.ref.producer == EvidenceProducer.LegalLineProducer &&
                  record.ref.confidence == EvidenceConfidence.LegalReplayVerified)
        case MoveTransitionEvidence(moveUci, from, to) =>
          exactAuthority(record, EvidenceProducer.MoveTransitionProducer, EvidenceLayer.MoveTransition) &&
            record.ref.confidence == EvidenceConfidence.LegalReplayVerified &&
            record.ref.line.isEmpty && record.ref.position == from && from != to &&
            EvidenceRef.normalizeMove(moveUci).nonEmpty
        case payload: StructuralDeltaEvidence =>
          structuralDeltaAncestryVerified(record, payload) &&
            payload.exactOutputInventoryCertified
        case payload: RelationFactEvidence =>
          relationOccurrenceAncestryVerified(record, payload)
        case payload: ClosedRelationOccurrenceEvidence =>
          closedRelationOccurrenceAncestryVerified(record, payload)
        case payload: TacticalMechanismEvidence =>
          tacticalMechanismAncestryVerified(record, payload)
        case _: PlanTransitionEvidence =>
          exactAuthority(record, EvidenceProducer.PlanTransitionProducer, EvidenceLayer.PlanTransition)
        case payload: PlanCausalEventEvidence =>
          exactAuthority(record, EvidenceProducer.PlanCausalEventProducer, EvidenceLayer.PlanCausalEvent) &&
            payload.rootTransitionIsCertified && record.ref.position == payload.rootTransition.from &&
            record.ref.line.contains(payload.rootLine) && record.ref.scope == payload.rootTransition.role.scope
        case CandidateComparisonEvidence(comparison) =>
          exactAuthority(record, EvidenceProducer.RelativeMoveProducer, EvidenceLayer.CandidateComparison) &&
            record.ref.line.contains(comparison.candidateLine)
        case RelativeAssessmentEvidence(assessment) =>
          exactAuthority(record, EvidenceProducer.RelativeMoveProducer, EvidenceLayer.RelativeAssessment) &&
            record.ref == assessment.evidence && record.ref.line.contains(assessment.candidate.ref)
        case RelativeCauseFactEvidence(cause) =>
          exactAuthority(record, EvidenceProducer.RelativeMoveProducer, EvidenceLayer.RelativeCause) &&
            record.parents.exists(_ == cause.comparisonEvidence) &&
            cause.supportEvidence.forall(record.parents.contains)
      )

  private def exactAuthority(
      record: EvidenceRecord,
      producer: EvidenceProducer,
      layer: EvidenceLayer
  ): Boolean =
    record.ref.producer == producer && record.ref.layer == layer

  private def tacticalMechanismAncestryVerified(
      record: EvidenceRecord,
      payload: TacticalMechanismEvidence
  ): Boolean =
    val directParents = record.parents.flatMap(parent => byId.get(parent.id).filter(_.ref == parent))
    val sourceRefs = payload.signals.flatMap(_.source)
    val sourceRecords = sourceRefs.flatMap(source => byId.get(source.id).filter(_.ref == source))
    val transitionParents = directParents.filter(_.payload.isInstanceOf[MoveTransitionEvidence])
    val exactTransitionParents = transitionParents.forall {
      case EvidenceRecord(ref, MoveTransitionEvidence(moveUci, from, _), _) =>
        ref.producer == EvidenceProducer.MoveTransitionProducer &&
          ref.layer == EvidenceLayer.MoveTransition &&
          ref.confidence == EvidenceConfidence.LegalReplayVerified &&
          ref.position == record.ref.position && from == record.ref.position &&
          payload.moveUci.exists(EvidenceRef.sameMove(_, moveUci))
      case _ => false
    }
    val expectedParents = (sourceRefs ++ transitionParents.map(_.ref)).sortBy(_.id)
    val exactSources =
      payload.signals.nonEmpty && sourceRefs.size == payload.signals.size &&
        sourceRefs.map(_.id).distinct.size == sourceRefs.size &&
        sourceRecords.size == sourceRefs.size &&
        sourceRecords.forall(source =>
          source.ref.position == record.ref.position && (source.payload match
            case _: RelationFactEvidence => true
            case _                       => source.ref.scope == record.ref.scope)
        ) &&
        payload.signals.forall(signal => tacticalSignalVerified(payload, signal))
    val exactRelationContract =
      if payload.signals.exists(_.kind == TacticalMechanismSignalKind.Relation) then
        TacticalRelationLineContract.certifies(this, payload)
      else true
    val exactConfidence =
      if payload.signalKinds.contains(TacticalMechanismSignalKind.MateBranch) then
        record.ref.confidence == EvidenceConfidence.EngineBacked
      else record.ref.confidence == EvidenceConfidence.LegalReplayVerified

    exactAuthority(record, EvidenceProducer.TacticalMechanismProducer, EvidenceLayer.TacticalMechanism) &&
      payload.hasConcreteProof && record.ref.line == payload.line &&
      record.parents.map(_.id).distinct.size == record.parents.size &&
      payload.moveUci.exists(move =>
        EvidenceRef.normalizeMove(move).nonEmpty &&
          payload.line.forall(line => EvidenceRef.sameMove(line.rootMove, move))
      ) &&
      record.parents.sortBy(_.id) == expectedParents &&
      exactTransitionParents && exactSources && exactRelationContract && exactConfidence

  private def tacticalSignalVerified(
      mechanism: TacticalMechanismEvidence,
      signal: TacticalMechanismSignal
  ): Boolean =
    signal.source.exists(source =>
      source.layer == signal.sourceLayer && byId.get(source.id).exists {
        case relationRecord @ EvidenceRecord(ref, relation: RelationFactEvidence, _) =>
          signal.kind == TacticalMechanismSignalKind.Relation &&
            signal.sourceLayer == EvidenceLayer.Relation &&
            signal.relationKind.contains(relation.kind) &&
            signal.label == relation.detail.detailName &&
            relationGraph.containsRecord(relationRecord) &&
            mechanism.moveUci.exists(relation.mentionsLineMove) &&
            TacticalMechanismKind.fromRelation(relation).contains(mechanism.kind) &&
            ref == source
        case EvidenceRecord(ref, line: LineFactEvidence, _) =>
          ref == source && signal.sourceLayer == EvidenceLayer.Line &&
            mechanism.moveUci.exists(rootMove =>
              line.rootMove.exists(EvidenceRef.sameMove(_, rootMove)) &&
                (signal.kind match
                  case TacticalMechanismSignalKind.LineConsequence =>
                    val consequences =
                      (line.rootOwnedCausalConsequences(rootMove) ++
                        line.immediateReplyCheckLiabilitiesForRootMove(rootMove)).distinct
                    consequences.exists(consequence =>
                      signal.label == consequence.kind.toString &&
                        (
                          TacticalMechanismKind
                            .fromLineConsequence(consequence.kind, line.rootIsRecapture(rootMove))
                            .contains(mechanism.kind) ||
                            (mechanism.kind == TacticalMechanismKind.DefensiveResource &&
                              Set(LineConsequenceKind.RecaptureSequence, LineConsequenceKind.RecoveryWindow)(
                                consequence.kind
                              ) &&
                              line.lineEventsOf(LineEventKind.DefenderMove).exists(event =>
                                event.plyOffset == 0 && EvidenceRef.sameMove(event.moveUci, rootMove)
                              ))
                        )
                    )
                  case TacticalMechanismSignalKind.LineEvent =>
                    LineEventKind.values.find(_.toString == signal.label).exists(kind =>
                      line.lineEventsOf(kind).exists(event =>
                        event.plyOffset == 0 && EvidenceRef.sameMove(event.moveUci, rootMove) &&
                          (mechanism.kind != TacticalMechanismKind.DefensiveResource ||
                            (event.kind == LineEventKind.DefenderMove &&
                              event.targetRole.exists(_.name.equalsIgnoreCase(King.name))))
                      )
                    )
                  case _ => false)
            )
        case EvidenceRecord(
              ref,
              CandidateLineEvaluationEvidence(
                _,
                lila.chessjudgment.model.line.CandidateLineEvaluation.EngineSearch(line)
              ),
              _
            ) =>
          ref == source && signal.kind == TacticalMechanismSignalKind.MateBranch &&
            signal.sourceLayer == EvidenceLayer.Eval && signal.relationKind.isEmpty &&
            mechanism.kind == TacticalMechanismKind.KingForcing &&
            line.mate.exists(mate => signal.label == mate.toString) &&
            mechanism.moveUci.exists(rootMove => line.moves.headOption.exists(EvidenceRef.sameMove(_, rootMove)))
        case _ => false
      }
    )

  private def relationOccurrenceAncestryVerified(
      record: EvidenceRecord,
      payload: RelationFactEvidence
  ): Boolean =
    payload.origin match
      case RelationEvidenceOrigin.PositionSnapshot(_) =>
        record.parents.isEmpty
      case _: RelationEvidenceOrigin.LegalReplay =>
        val directParents = record.parents.flatMap(parent => byId.get(parent.id).filter(_.ref == parent))
        val carrierParents = directParents.collect {
          case exact @ EvidenceRecord(_, occurrence: ClosedRelationOccurrenceEvidence, _) => exact -> occurrence
        }
        carrierParents match
          case (carrierRecord, occurrence) :: Nil =>
            occurrence.outputFor(record.ref).exists { binding =>
              val expectedParents = carrierRecord.ref :: binding.sources
              binding.relation == payload &&
                record.ref.position == occurrence.edge.from &&
                record.ref.line == occurrence.lineOwner &&
                record.ref.scope == occurrence.scope &&
                record.parents.size == expectedParents.size &&
                record.parents.toSet == expectedParents.toSet &&
                binding.sources.forall(exactCanonicalRelationSource)
            }
          case _ => false
      case RelationEvidenceOrigin.Unverified => false

  private def closedRelationOccurrenceAncestryVerified(
      record: EvidenceRecord,
      payload: ClosedRelationOccurrenceEvidence
  ): Boolean =
    val expectedParents = payload.edge.evidence :: payload.lineEvidence.toList
    val exactTransition = byId.get(payload.edge.evidence.id).exists {
      case EvidenceRecord(ref, MoveTransitionEvidence(moveUci, from, to), parents) =>
        ref == payload.edge.evidence &&
          ref.producer == EvidenceProducer.MoveTransitionProducer &&
          ref.layer == EvidenceLayer.MoveTransition &&
          ref.position == payload.edge.from &&
          ref.line.isEmpty &&
          ref.scope == payload.edge.role.scope &&
          ref.confidence == EvidenceConfidence.LegalReplayVerified &&
          parents.isEmpty &&
          from == payload.edge.from && to == payload.edge.to &&
          EvidenceRef.sameMove(moveUci, payload.edge.moveUci)
      case _ => false
    }
    val exactLine = (payload.lineOwner, payload.lineEvidence) match
      case (Some(line), Some(lineRef)) =>
        byId.get(lineRef.id).exists {
          case EvidenceRecord(ref, lineFact: LineFactEvidence, _) =>
            ref == lineRef &&
              ref.producer == EvidenceProducer.LegalLineProducer &&
              ref.layer == EvidenceLayer.Line &&
              ref.position == payload.edge.from &&
              ref.line.contains(line) &&
              ref.scope == line.role.scope &&
              ref.confidence == EvidenceConfidence.LegalReplayVerified &&
              line.role == payload.edge.role.lineRole &&
              lineFact.line == line &&
              lineFact.canonicalReplay.exists(_.replaySteps.headOption.exists(step =>
                step.ply == payload.edge.to.ply &&
                  EvidenceRef.sameMove(step.moveUci, payload.edge.moveUci) &&
                  PrincipalVariationEvidence.sameBoardState(step.fenBefore, payload.edge.from.fen) &&
                  PrincipalVariationEvidence.sameBoardState(step.fenAfter, payload.edge.to.fen)
              ))
          case _ => false
        }
      case (None, None) => true
      case _            => false
    val exactOutputs = payload.outputs.forall(binding =>
      byId.get(binding.result.id).exists {
        case EvidenceRecord(ref, relation: RelationFactEvidence, parents) =>
          ref == binding.result && relation == binding.relation &&
            parents.size == binding.sources.size + 1 &&
            parents.toSet == (record.ref :: binding.sources).toSet &&
            binding.sources.forall(exactCanonicalRelationSource)
        case _ => false
      }
    )
    record.ref.producer == EvidenceProducer.RelationProducer &&
      record.ref.layer == EvidenceLayer.Relation &&
      record.ref.confidence == EvidenceConfidence.LegalReplayVerified &&
      record.ref.position == payload.edge.from &&
      record.ref.line == payload.lineOwner &&
      record.ref.scope == payload.scope &&
      record.parents.size == expectedParents.size &&
      record.parents.toSet == expectedParents.toSet &&
      exactTransition && exactLine && exactOutputs

  private def exactCanonicalRelationSource(source: EvidenceRef): Boolean =
    byId.get(source.id).exists(sourceRecord =>
      sourceRecord.ref == source && relationGraph.containsRecord(sourceRecord)
    )

  private def structuralDeltaAncestryVerified(
      record: EvidenceRecord,
      payload: StructuralDeltaEvidence
  ): Boolean =
    val directParents = record.parents.flatMap(parent => byId.get(parent.id).filter(_.ref == parent))
    val directParentsById = directParents.map(parent => parent.ref.id -> parent).toMap
    val transitionParent = directParents.exists {
      case EvidenceRecord(ref, MoveTransitionEvidence(moveUci, from, to), _) =>
        ref.producer == EvidenceProducer.MoveTransitionProducer &&
          ref.layer == EvidenceLayer.MoveTransition &&
          ref.confidence == EvidenceConfidence.LegalReplayVerified &&
          EvidenceRef.sameMove(moveUci, payload.moveUci) &&
          from == payload.from &&
          to == payload.to
      case _ =>
        false
    }
    def positionParent(position: PositionNodeRef): Boolean =
      directParents.exists {
        case EvidenceRecord(ref, PositionFeatureEvidence(features), _) =>
          ref.producer == EvidenceProducer.PositionFeatureProducer &&
            ref.layer == EvidenceLayer.PositionFeature &&
            ref.confidence == EvidenceConfidence.BoardDerived &&
            ref.position == position &&
            PrincipalVariationEvidence.sameBoardState(features.fen, position.fen) &&
            features.plyCount == position.ply &&
            position.sideToMove.forall(_ == features.sideToMove)
        case _ =>
          false
      }
    record.ref.producer == EvidenceProducer.StructuralDeltaProducer &&
      record.ref.layer == EvidenceLayer.StructuralDelta &&
      record.ref.confidence == EvidenceConfidence.BoardDerived &&
      record.ref.position == payload.from &&
      transitionParent &&
      positionParent(payload.from) &&
      positionParent(payload.to) &&
      payload.relationChanges.forall(change =>
        directParentsById.get(change.source.id).contains(change.sourceNode.record) &&
          relationGraph.contains(change.source, change.relation)
      )

  def add(record: EvidenceRecord): TypedEvidenceGraph =
    addAll(List(record))

  def addAll(incoming: IterableOnce[EvidenceRecord]): TypedEvidenceGraph =
    val additions = incoming.iterator.toList
    if additions.isEmpty then this
    else
      var merged = recordIndex
      var changed = false
      var relationsChanged = false
      additions.foreach { record =>
        merged.get(record.ref.id) match
          case None =>
            merged = merged.updated(record.ref.id, record)
            changed = true
            relationsChanged ||= CanonicalRelationGraph.relationShaped(record)
          case Some(existing) if existing == record => ()
          case Some(_) =>
            throw IllegalArgumentException(
              s"evidence id collision for '${record.ref.id}': existing record differs from the attempted addition"
            )
      }
      if !changed then this
      else
        val updatedRelationGraph =
          if relationsChanged then relationGraph.addAll(additions)
          else relationGraph
        new TypedEvidenceGraph(merged, updatedRelationGraph)

object TypedEvidenceGraph:
  private[chessjudgment] def ownedLineConsequences(
      records: List[EvidenceRecord],
      fact: CandidateComparisonFact,
      sourceSide: RelativeCauseSourceSide,
      attributionKind: CauseAttributionKind,
      sourceLabelsByEvidenceId: Map[String, Set[String]] = Map.empty
  ): List[(EvidenceRef, LineConsequence)] =
    val ownedLines =
      sourceSide match
        case RelativeCauseSourceSide.Reference => List(fact.referenceLine)
        case RelativeCauseSourceSide.Candidate => List(fact.candidateLine)
        case RelativeCauseSourceSide.Shared | RelativeCauseSourceSide.Mixed =>
          List(fact.referenceLine, fact.candidateLine)
    val expectedBeneficiary =
      attributionKind match
        case CauseAttributionKind.ReferenceCreatesResource | CauseAttributionKind.CandidateCreatesValue =>
          Some(fact.comparison.mover)
        case CauseAttributionKind.CandidateAllowsLiability =>
          Some(!fact.comparison.mover)
        case CauseAttributionKind.SharedContext | CauseAttributionKind.ContextOnly |
            CauseAttributionKind.Unattributed =>
          None
    expectedBeneficiary.toList.flatMap(beneficiary =>
      records.collect {
        case EvidenceRecord(ref, payload: LineFactEvidence, _)
            if ref.line.exists(ownedLines.contains) &&
              (sourceLabelsByEvidenceId.isEmpty || sourceLabelsByEvidenceId.contains(ref.id)) =>
          val eventLine = ref.line.get
          payload
            .ownedLineConsequences(
              eventLine,
              eventLine.rootMove,
              fact.comparison.mover,
              beneficiary,
              sourceLabelsByEvidenceId.getOrElse(ref.id, Set.empty)
            )
            .map(ref -> _)
      }.flatten
    ).distinct

  val empty: TypedEvidenceGraph =
    val records = scala.collection.immutable.VectorMap.empty[String, EvidenceRecord]
    new TypedEvidenceGraph(records, CanonicalRelationGraph.empty)
