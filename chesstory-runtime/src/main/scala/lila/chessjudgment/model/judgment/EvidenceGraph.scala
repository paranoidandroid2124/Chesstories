package lila.chessjudgment.model.judgment

import chess.*
import chess.format.Fen
import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import scala.collection.immutable.VectorMap
import lila.chessjudgment.model.evaluation.{ JudgmentThresholds, PerspectiveMath }
import lila.chessjudgment.model.line.{ LegalReplayStep, PrincipalVariationEvidence }
import lila.chessjudgment.analysis.position.{ PositionAnalysis, PositionRelationExtractor }
import lila.chessjudgment.model.position.{
  BoardGeometry,
  BoardPieceTransition,
  BoardTransitionFootprint,
  PositionFeatures
}
import lila.chessjudgment.model.{
  Plan,
  PlanActorOccurrence,
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

object EvidenceFile:
  private[chessjudgment] def contains(
      file: EvidenceFile,
      square: EvidenceSquare
  ): Boolean =
    val boardFile = file.key.trim.toLowerCase match
      case exact if exact.length == 1 => File.fromChar(exact.head)
      case _                          => None
    boardFile.exists(expected => Square.fromKey(square.key.trim.toLowerCase).exists(_.file == expected))

object EvidencePieceRole:
  /** UCI promotion is a protocol mapping, not the first letter of a role name:
    * knight is encoded as `n`.
    */
  def promotionSuffix(
      before: EvidencePieceRole,
      after: EvidencePieceRole
  ): String =
    if before.name.equalsIgnoreCase(after.name) then ""
    else
      after.name.toLowerCase match
        case "queen"  => "q"
        case "rook"   => "r"
        case "bishop" => "b"
        case "knight" => "n"
        case role      => throw IllegalArgumentException(s"unsupported UCI promotion role '$role'")

enum EvidenceSemanticAnchorKind:
  case StrategicMechanism
  case StrategicAxis
  case Plan
  case Relation
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
      EvidenceRef.sameMove(event.rootMove, moveUci)
    ),
    "a Cause proof step's plan-event occurrence must identify the same move occurrence"
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
    rootActor: RootCausalActor,
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
      rootActor = rootActor,
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
    rootActor: RootCausalActor,
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
      case _: RootOwnedEffectProof.StrategicAxis              => "strategic-axis"
    }.getOrElse("unproved")
    List(
      causalSignature,
      primitiveSignature,
      rootActor.moveUci,
      rootActor.color.toString.toLowerCase,
      rootActor.role.name.toLowerCase,
      rootActor.from.key.toLowerCase,
      rootActor.to.key.toLowerCase,
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
            LineEventKind.CheckEvasion
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
          resultWitnesses ++ inducedResponseMoveOrderWitnesses
        }

      case _ => Nil
    }

    val baseWitnesses = primitiveWitnesses
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
            case EvidenceRecord(_, CandidateComparisonEvidence(_), _) =>
              Nil
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
          objectOf(EvidenceObjectKind.Piece, sourceEvent.identity.actor.beforeRole) ++
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
      case LineEventKind.Tempo | LineEventKind.CheckEvasion =>
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
                  planActorObjects(previous.rootMove, previous.actor)
                ) ++ routePieces.flatMap(role => objectOf(EvidenceObjectKind.Piece, role.name))
              ).distinctBy(_.signaturePart),
              target = (
                objectOf(EvidenceObjectKind.PlanSubject, current.goalKey) ++
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
          else List(fromTacticalMechanism(record.ref, payload, graph))
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
      for
        moveUci <- payload.rootMove.toList
        actor <- exactLineActorAt(payload, 0, moveUci).toList
      yield
        val move = EvidenceRef.normalizeMove(moveUci)
        EvidenceObjectBinding(
          source = ref,
          actor = rootActorObjects(actor),
          target = actorTargetSquare(actor),
          mechanism = objectOf(EvidenceObjectKind.Mechanism, "LineRootMove"),
          consequence = objectOf(EvidenceObjectKind.Consequence, "LineRootMove"),
          witness = objectOf(EvidenceObjectKind.Move, move) ++ actorTargetSquare(actor) ++ lineObject(payload.line),
          line = Some(payload.line)
        )
    val replayBindings =
      payload.lineReplaySteps.drop(1).zipWithIndex.flatMap { case (step, index) =>
        exactLineActorAt(payload, index + 1, step.moveUci).map { actor =>
          val move = EvidenceRef.normalizeMove(step.moveUci)
          EvidenceObjectBinding(
            source = ref,
            actor = rootActorObjects(actor),
            target = actorTargetSquare(actor),
            mechanism = objectOf(EvidenceObjectKind.Mechanism, "LineContinuation"),
            consequence = objectOf(EvidenceObjectKind.Consequence, "LineContinuation"),
            witness = objectOf(EvidenceObjectKind.Move, move) ++ actorTargetSquare(actor) ++ lineObject(payload.line),
            line = Some(payload.line),
            lineOccurrence = Some(step)
          )
        }
      }
    val eventBindings =
      payload.lineEvents.flatMap { event =>
        exactLineActorAt(payload, event.plyOffset, event.moveUci).map { actor =>
          val move = EvidenceRef.normalizeMove(event.moveUci)
          EvidenceObjectBinding(
            source = ref,
            actor = rootActorObjects(actor),
            target = {
              val squareTarget = squareObject(event.square)
              (if squareTarget.nonEmpty then squareTarget else actorTargetSquare(actor)) ++ roleObject(event.targetRole)
            },
            mechanism = objectOf(EvidenceObjectKind.Mechanism, event.kind.toString),
            consequence = objectOf(EvidenceObjectKind.Consequence, event.kind.toString),
            witness = objectOf(EvidenceObjectKind.Move, move) ++ actorTargetSquare(actor) ++ lineObject(payload.line),
            line = Some(payload.line),
            horizon = Some(s"ply:${event.plyOffset}")
          )
        }
      }
    val consequenceBindings =
      payload.proofSignalConsequences.flatMap { consequence =>
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
        val eventActor = ownedSacrificeOccurrence
          .flatMap(occurrence =>
            exactLineActorAt(
              payload,
              occurrence.acceptance.plyOffset,
              occurrence.acceptance.moveUci
            )
          )
          .orElse(eventMove.flatMap(uniqueLineActorFor(payload, _)))
        val sacrificeCaptureTargets =
          ownedSacrificeOccurrence.toList.flatMap { occurrence =>
            val capture = occurrence.acceptance
            squareObject(Some(capture.square)) ++
              roleObject(Some(capture.capturedRole)) ++
              objectOf(EvidenceObjectKind.PlanSubject, s"material-sacrifice:${capture.square.key}")
          }
        val lineMoveWitness =
          consequenceMoves.flatMap(move =>
            exactLineActorsFor(payload, move).flatMap(actor =>
              objectOf(EvidenceObjectKind.Move, move) ++ actorTargetSquare(actor)
            )
          )
        val lineEventWitness =
          payload.lineEvents
            .filter(event => consequenceMoves.contains(normalize(event.moveUci)))
            .flatMap(event => squareObject(event.square) ++ roleObject(event.pieceRole) ++ roleObject(event.targetRole))
        eventActor.toList.map(actor =>
          EvidenceObjectBinding(
            source = ref,
            actor = rootActorObjects(actor),
            target = actorTargetSquare(actor) ++ sacrificeCaptureTargets,
            mechanism = objectOf(EvidenceObjectKind.Mechanism, consequence.kind.toString),
            consequence = objectOf(EvidenceObjectKind.Consequence, consequence.kind.toString),
            witness = lineMoveWitness ++ lineEventWitness ++ lineObject(payload.line),
            line = Some(payload.line)
          )
        )
      }
    val materialCaptureBindings =
      payload.materialCaptures.flatMap { capture =>
        exactLineActorAt(payload, capture.plyOffset, capture.moveUci).map { actor =>
          val move = EvidenceRef.normalizeMove(capture.moveUci)
          val rootMove = payload.rootMove.map(normalize)
          val rootMoveCapture = capture.plyOffset == 0 && rootMove.contains(move)
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
            actor = rootActorObjects(actor),
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
        case RelationWitnessDetail.SliderLineInterruption(mover, controllerSide, _, _, _, _, _) =>
          objectOf(EvidenceObjectKind.Side, colorKey(mover.side)) ++
            objectOf(EvidenceObjectKind.Side, colorKey(controllerSide))
        case RelationWitnessDetail.GeometricLineControlAfterBlockerRemoval(mover, controllerSide, _, _, _, _, _, _, _, _, _) =>
          objectOf(EvidenceObjectKind.Side, colorKey(mover.side)) ++
            objectOf(EvidenceObjectKind.Side, colorKey(controllerSide))
        case RelationWitnessDetail.NamedRayTransition(mover, owner, _, _, _, _, _, _, _, _) =>
          objectOf(EvidenceObjectKind.Side, colorKey(mover.side)) ++
            objectOf(EvidenceObjectKind.Side, colorKey(owner))
        case RelationWitnessDetail.GeometricSupportCausalTransition(mover, supportedSide, _, _, _, _, _, _, _, _, _) =>
          objectOf(EvidenceObjectKind.Side, colorKey(mover.side)) ++
            objectOf(EvidenceObjectKind.Side, colorKey(supportedSide))
        case RelationWitnessDetail.CaptureRecaptureInventory(mover, captured, _, _, _) =>
          objectOf(EvidenceObjectKind.Side, colorKey(mover.side)) ++
            objectOf(EvidenceObjectKind.Side, colorKey(captured.side))
        case RelationWitnessDetail.CreatedCheckResponseInventory(mover, checkedSide, _, _, _, _, _, _) =>
          objectOf(EvidenceObjectKind.Side, colorKey(mover.side)) ++
            objectOf(EvidenceObjectKind.Side, colorKey(checkedSide))
        case RelationWitnessDetail.RootCheckResponse(mover, respondingSide, _, _, _, _) =>
          objectOf(EvidenceObjectKind.Side, colorKey(mover.side)) ++
            objectOf(EvidenceObjectKind.Side, colorKey(respondingSide))
        case RelationWitnessDetail.MovementAffordanceLegalRestriction(mover, restrictedSide, _, _, _, _, _) =>
          objectOf(EvidenceObjectKind.Side, colorKey(mover.side)) ++
            objectOf(EvidenceObjectKind.Side, colorKey(restrictedSide))
        case RelationWitnessDetail.AbsolutePinMovementRestriction(mover, restrictedSide, _, _, _, _, _, _, _, _) =>
          objectOf(EvidenceObjectKind.Side, colorKey(mover.side)) ++
            objectOf(EvidenceObjectKind.Side, colorKey(restrictedSide))
        case RelationWitnessDetail.SliderReachDelta(mover, owner, _, _, _, _, _, _) =>
          objectOf(EvidenceObjectKind.Side, colorKey(mover.side)) ++
            objectOf(EvidenceObjectKind.Side, colorKey(owner))
        case RelationWitnessDetail.SharedGeometricSupportOfEnemyControlledTargets(_, controllingSide, supportedSide, _, _, _) =>
          objectOf(EvidenceObjectKind.Side, colorKey(controllingSide)) ++
            objectOf(EvidenceObjectKind.Side, colorKey(supportedSide))
        case RelationWitnessDetail.GeometricMultiTargetContact(mover, controllingSide, _, _, _, _, _) =>
          objectOf(EvidenceObjectKind.Side, colorKey(mover.side)) ++
            objectOf(EvidenceObjectKind.Side, colorKey(controllingSide))
        case RelationWitnessDetail.GeometricEnemyContactWithoutFriendlySupport(mover, controllingSide, _, _, _, _) =>
          objectOf(EvidenceObjectKind.Side, colorKey(mover.side)) ++
            objectOf(EvidenceObjectKind.Side, colorKey(controllingSide))
        case RelationWitnessDetail.PawnTopologyTransition(mover, before, after, _, _) =>
          objectOf(EvidenceObjectKind.Side, colorKey(mover.side)) ++
            (before.orElse(after).toList.flatMap(value => objectOf(EvidenceObjectKind.Side, colorKey(value.side))))
        case RelationWitnessDetail.PawnOccupiedFilePartitionTransition(mover, owner, _, _, _) =>
          objectOf(EvidenceObjectKind.Side, colorKey(mover.side)) ++
            objectOf(EvidenceObjectKind.Side, colorKey(owner))
        case RelationWitnessDetail.MajorPiecePawnFileCorridorTransition(mover, owner, _, _, _, _, _, _, _, _, _) =>
          objectOf(EvidenceObjectKind.Side, colorKey(mover.side)) ++
            objectOf(EvidenceObjectKind.Side, colorKey(owner))
        case RelationWitnessDetail.CastlingRightRemoved(mover, owner, _, _) =>
          objectOf(EvidenceObjectKind.Side, colorKey(mover.side)) ++
            objectOf(EvidenceObjectKind.Side, colorKey(owner))
        case RelationWitnessDetail.StalemateTransition(mover, stalledSide, _, _) =>
          objectOf(EvidenceObjectKind.Side, colorKey(mover.side)) ++
            objectOf(EvidenceObjectKind.Side, colorKey(stalledSide))
        case RelationWitnessDetail.PawnFileGroup(side, _, _) =>
          objectOf(EvidenceObjectKind.Side, colorKey(side))
        case RelationWitnessDetail.PawnFrontOccupancy(side, _, _, _) =>
          objectOf(EvidenceObjectKind.Side, colorKey(side))
        case RelationWitnessDetail.PawnAdvanceAffordance(side, _, _, _) =>
          objectOf(EvidenceObjectKind.Side, colorKey(side))
        case RelationWitnessDetail.PawnPassage(side, _, _) =>
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
        case RelationWitnessDetail.SliderLineInterruption(_, controllerSide, _, _, _, targets, _) =>
          targets.flatMap(_.target match
            case RelationControlTarget.Friendly(_) => objectOf(EvidenceObjectKind.Side, colorKey(controllerSide))
            case RelationControlTarget.Enemy(_)    => objectOf(EvidenceObjectKind.Side, colorKey(!controllerSide))
            case RelationControlTarget.Empty       => Nil
          )
        case RelationWitnessDetail.GeometricLineControlAfterBlockerRemoval(_, controllerSide, _, _, _, _, _, opened, _, _, _) =>
          opened.flatMap(_.target match
            case RelationControlTarget.Friendly(_) => objectOf(EvidenceObjectKind.Side, colorKey(controllerSide))
            case RelationControlTarget.Enemy(_)    => objectOf(EvidenceObjectKind.Side, colorKey(!controllerSide))
            case RelationControlTarget.Empty       => Nil
          )
        case RelationWitnessDetail.NamedRayTransition(_, owner, _, _, barrier, target, _, _, _, _) =>
          Option.when(barrier.side != owner || target.exists(_.side != owner))(
            objectOf(EvidenceObjectKind.Side, colorKey(!owner))
          ).getOrElse(Nil)
        case RelationWitnessDetail.GeometricSupportCausalTransition(_, supportedSide, _, _, _, _, _, _, _, _, _) =>
          objectOf(EvidenceObjectKind.Side, colorKey(supportedSide))
        case RelationWitnessDetail.CaptureRecaptureInventory(_, captured, _, _, _) =>
          objectOf(EvidenceObjectKind.Side, colorKey(captured.side))
        case RelationWitnessDetail.CreatedCheckResponseInventory(_, checkedSide, _, _, _, _, _, _) =>
          objectOf(EvidenceObjectKind.Side, colorKey(checkedSide))
        case RelationWitnessDetail.RootCheckResponse(_, respondingSide, _, _, _, _) =>
          objectOf(EvidenceObjectKind.Side, colorKey(respondingSide))
        case RelationWitnessDetail.MovementAffordanceLegalRestriction(_, restrictedSide, _, _, _, _, _) =>
          objectOf(EvidenceObjectKind.Side, colorKey(restrictedSide))
        case RelationWitnessDetail.AbsolutePinMovementRestriction(_, restrictedSide, _, _, _, _, _, _, _, _) =>
          objectOf(EvidenceObjectKind.Side, colorKey(restrictedSide))
        case RelationWitnessDetail.SliderReachDelta(_, owner, _, _, _, _, _, _) =>
          objectOf(EvidenceObjectKind.Side, colorKey(owner))
        case RelationWitnessDetail.SharedGeometricSupportOfEnemyControlledTargets(_, _, supportedSide, _, _, _) =>
          objectOf(EvidenceObjectKind.Side, colorKey(supportedSide))
        case RelationWitnessDetail.GeometricMultiTargetContact(_, controllingSide, _, _, _, _, _) =>
          objectOf(EvidenceObjectKind.Side, colorKey(!controllingSide))
        case RelationWitnessDetail.GeometricEnemyContactWithoutFriendlySupport(_, controllingSide, _, _, _, _) =>
          objectOf(EvidenceObjectKind.Side, colorKey(!controllingSide))
        case RelationWitnessDetail.PawnTopologyTransition(_, before, after, _, _) =>
          before.orElse(after).toList.flatMap(value => objectOf(EvidenceObjectKind.Side, colorKey(value.side)))
        case RelationWitnessDetail.PawnOccupiedFilePartitionTransition(_, owner, _, _, _) =>
          objectOf(EvidenceObjectKind.Side, colorKey(owner))
        case RelationWitnessDetail.MajorPiecePawnFileCorridorTransition(_, owner, file, _, _, _, _, _, _, _, _) =>
          objectOf(EvidenceObjectKind.Side, colorKey(owner)) ++
            objectOf(EvidenceObjectKind.File, file.key)
        case RelationWitnessDetail.CastlingRightRemoved(_, owner, _, _) =>
          objectOf(EvidenceObjectKind.Side, colorKey(owner))
        case RelationWitnessDetail.StalemateTransition(_, stalledSide, _, _) =>
          objectOf(EvidenceObjectKind.Side, colorKey(stalledSide))
        case RelationWitnessDetail.PawnFileGroup(_, file, _) =>
          objectOf(EvidenceObjectKind.File, file.key)
        case RelationWitnessDetail.PawnFrontOccupancy(_, _, _, occupant) =>
          occupant.toList.flatMap(value => objectOf(EvidenceObjectKind.Side, colorKey(value.side)))
        case RelationWitnessDetail.PawnAdvanceAffordance(_, _, _, _) =>
          Nil
        case RelationWitnessDetail.PawnPassage(side, _, opposingPawns) =>
          Option.when(opposingPawns.nonEmpty)(objectOf(EvidenceObjectKind.Side, colorKey(!side))).getOrElse(Nil)
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
    val rootActor = planActorObjects(payload.rootMove, payload.identity.actor)
    val rootDestination = objectOf(EvidenceObjectKind.Square, payload.identity.actor.to)
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
          rootActor ++ objectOf(EvidenceObjectKind.Piece, dependency.from.identity.actor.beforeRole)
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
            .flatMap(square => objectOf(EvidenceObjectKind.Square, square.key))
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
            objectOf(EvidenceObjectKind.Piece, sourceEvent.identity.actor.beforeRole)
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
    val actor = payload.certifiedRootMovement.toList.flatMap(movementActorObjects)
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
          .groupBy(binding =>
            (
              binding.relationKeys.map(_.stableKey) ++
                binding.derivedRelationKeys.map(_.stableKey)
            ).sorted.mkString("|")
          )
          .toList
          .sortBy(_._1)
          .map(_._2)
      else List(allBindings)
    val changesByKey = payload.relationChanges.groupMap(_.key)(identity)
    val derivedSourcesByKey = payload.derivedRelationSources.groupMap(_.key)(_.source)
    val resolved = groups.map { bindings =>
      val keys = bindings.flatMap(_.relationKeys).distinct.sortBy(_.stableKey)
      val derivedKeys = bindings.flatMap(_.derivedRelationKeys).distinct.sortBy(_.stableKey)
      val sources = (
        keys.flatMap(key => changesByKey.getOrElse(key, Nil).map(_.source)) ++
          derivedKeys.flatMap(key => derivedSourcesByKey.getOrElse(key, Nil))
      ).distinctBy(_.id)
      val exactResolution =
        if TransitionConsequenceRelationProof.relationBacked(consequence.kind) then
          bindings.nonEmpty &&
            bindings.forall(binding => binding.relationKeys.nonEmpty || binding.derivedRelationKeys.nonEmpty) &&
            keys.forall(key => changesByKey.get(key).exists(_.size == 1)) &&
            derivedKeys.forall(key => derivedSourcesByKey.get(key).exists(_.size == 1)) &&
            sources.size == keys.size + derivedKeys.size
        else bindings.forall(binding => binding.relationKeys.isEmpty && binding.derivedRelationKeys.isEmpty)
      Option.when(exactResolution) {
        val bindingSet = bindings.toSet
        val goalSubjects = consequence.goalSubjectBindings.filter(bindingSet).map(_.subject)
        val witnessSubjects = consequence.witnessSubjectBindings.filter(bindingSet).map(_.subject)
        EvidenceObjectBinding(
          source = ref,
          actor = actor.distinctBy(_.signaturePart),
          target = goalSubjects.flatMap(subjectObject).distinctBy(_.signaturePart),
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

  private def fromTacticalMechanism(
      ref: EvidenceRef,
      payload: TacticalMechanismEvidence,
      graph: TypedEvidenceGraph
  ): EvidenceObjectBinding =
    val actor = for
      line <- payload.line.toList
      move <- payload.moveUci.toList
      exact <- graph.certifiedRootActorFor(line).toList
      if EvidenceRef.sameMove(exact.moveUci, move)
      obj <- rootActorObjects(exact)
    yield obj
    EvidenceObjectBinding(
      source = ref,
      actor = actor.distinctBy(_.signaturePart),
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

  private def planActorObjects(
      moveUci: String,
      actor: PlanActorOccurrence
  ): List[ConcreteChessObject] =
    (
      objectOf(EvidenceObjectKind.Move, EvidenceRef.normalizeMove(moveUci)) ++
        objectOf(EvidenceObjectKind.Side, colorKey(actor.side)) ++
        objectOf(EvidenceObjectKind.Piece, actor.beforeRole) ++
        objectOf(EvidenceObjectKind.Square, actor.from) ++
        objectOf(EvidenceObjectKind.Square, actor.to)
    ).distinctBy(_.signaturePart)

  private def movementActorObjects(movement: CanonicalRootLegalMove): List[ConcreteChessObject] =
    (
      objectOf(EvidenceObjectKind.Move, movement.moveUci) ++
        objectOf(EvidenceObjectKind.Side, colorKey(movement.side)) ++
        objectOf(EvidenceObjectKind.Piece, movement.beforeRole.name) ++
        objectOf(EvidenceObjectKind.Square, movement.from.key) ++
        objectOf(EvidenceObjectKind.Square, movement.to.key)
    ).distinctBy(_.signaturePart)

  private def exactLineActorAt(
      payload: LineFactEvidence,
      plyOffset: Int,
      moveUci: String
  ): Option[RootCausalActor] =
    for
      replay <- payload.certifiedReplay
      step <- payload.lineReplaySteps.lift(plyOffset)
      if EvidenceRef.sameMove(step.moveUci, moveUci)
      legal <- replay.legalStep(step)
      actor <- RootCausalActor.fromLegalStep(step.moveUci, legal)
    yield actor

  private def exactLineActorsFor(
      payload: LineFactEvidence,
      moveUci: String
  ): List[RootCausalActor] =
    payload.lineReplaySteps.zipWithIndex.flatMap { case (step, plyOffset) =>
      Option
        .when(EvidenceRef.sameMove(step.moveUci, moveUci))(
          exactLineActorAt(payload, plyOffset, step.moveUci)
        )
        .flatten
    }

  private def uniqueLineActorFor(
      payload: LineFactEvidence,
      moveUci: String
  ): Option[RootCausalActor] =
    exactLineActorsFor(payload, moveUci).distinct match
      case exact :: Nil => Some(exact)
      case _            => None

  private def actorTargetSquare(actor: RootCausalActor): List[ConcreteChessObject] =
    objectOf(EvidenceObjectKind.Square, actor.to.key)

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
      case StructuralSubject.SemiOpenFile(side, file) =>
        objectOf(EvidenceObjectKind.Side, colorKey(side)) ++ objectOf(EvidenceObjectKind.File, file.key)
      case StructuralSubject.PieceAt(side, role, square) =>
        objectOf(EvidenceObjectKind.Side, colorKey(side)) ++
          objectOf(EvidenceObjectKind.Piece, role.name) ++ objectOf(EvidenceObjectKind.Square, square.key)
      case StructuralSubject.GeometricControlSetChange(
            _,
            controllingSide,
            targetSquare,
            _,
            _,
            beforeControllers,
            afterControllers,
            removedControllers,
            establishedControllers
          ) =>
        objectOf(EvidenceObjectKind.Side, colorKey(controllingSide)) ++
          objectOf(EvidenceObjectKind.Square, targetSquare.key) ++
          (beforeControllers ++ afterControllers ++ removedControllers ++ establishedControllers).flatMap(piece =>
            objectOf(EvidenceObjectKind.Piece, piece.role.name) ++
              objectOf(EvidenceObjectKind.Square, piece.square.key)
          )
      case StructuralSubject.SliderReachChange(side, sliderBefore, sliderAfter, _, gained, lost) =>
        objectOf(EvidenceObjectKind.Side, colorKey(side)) ++
          (sliderBefore.toList ++ sliderAfter.toList).flatMap(slider =>
            objectOf(EvidenceObjectKind.Piece, slider.role.name) ++
              objectOf(EvidenceObjectKind.Square, slider.square.key)
          ) ++ (gained ++ lost).flatMap(reach =>
            objectOf(EvidenceObjectKind.Square, reach.square.key) ++
              roleObject(reach.target.pieceRole)
          )
      case StructuralSubject.PawnTensionCreated(side, from, to) =>
        objectOf(EvidenceObjectKind.Side, colorKey(side)) ++ objectOf(EvidenceObjectKind.Piece, "pawn") ++
          objectOf(EvidenceObjectKind.Square, from.key) ++ objectOf(EvidenceObjectKind.Square, to.key)
      case StructuralSubject.PawnTensionResolved(side, from, to) =>
        objectOf(EvidenceObjectKind.Side, colorKey(side)) ++ objectOf(EvidenceObjectKind.Piece, "pawn") ++
          objectOf(EvidenceObjectKind.Square, from.key) ++ objectOf(EvidenceObjectKind.Square, to.key)
      case StructuralSubject.Battery(formation) =>
        objectOf(EvidenceObjectKind.Side, colorKey(formation.side)) ++
          List(formation.firstSlider, formation.secondSlider).flatMap(slider =>
            objectOf(EvidenceObjectKind.Piece, slider.role.name) ++
            objectOf(EvidenceObjectKind.Square, slider.square.key)
          )
      case StructuralSubject.PassedPawnCreated(side, square) =>
        objectOf(EvidenceObjectKind.Side, colorKey(side)) ++ objectOf(EvidenceObjectKind.Piece, "pawn") ++
          objectOf(EvidenceObjectKind.Square, square.key)
      case StructuralSubject.PassedPawnLost(side, square) =>
        objectOf(EvidenceObjectKind.Side, colorKey(side)) ++ objectOf(EvidenceObjectKind.Piece, "pawn") ++
          objectOf(EvidenceObjectKind.Square, square.key)
      case StructuralSubject.PassedPawnAdvanced(side, from, to, _) =>
        objectOf(EvidenceObjectKind.Side, colorKey(side)) ++ objectOf(EvidenceObjectKind.Piece, "pawn") ++
          objectOf(EvidenceObjectKind.Square, from.key) ++ objectOf(EvidenceObjectKind.Square, to.key)
      case StructuralSubject.PassedStatusCreated(side, from, to, _) =>
        objectOf(EvidenceObjectKind.Side, colorKey(side)) ++ objectOf(EvidenceObjectKind.Piece, "pawn") ++
          objectOf(EvidenceObjectKind.Square, from.key) ++ objectOf(EvidenceObjectKind.Square, to.key)
      case StructuralSubject.PassedPawnPromoted(side, from, to) =>
        objectOf(EvidenceObjectKind.Side, colorKey(side)) ++ objectOf(EvidenceObjectKind.Piece, "pawn") ++
          objectOf(EvidenceObjectKind.Square, from.key) ++ objectOf(EvidenceObjectKind.Square, to.key)
    (identityObject ++ values).distinctBy(_.signaturePart)

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

final case class RelationRayDirection(fileStep: Int, rankStep: Int):
  require(
    (-1 to 1).contains(fileStep) && (-1 to 1).contains(rankStep) &&
      (fileStep != 0 || rankStep != 0),
    "a ray direction needs one non-zero unit step"
  )
  require(
    fileStep == 0 || rankStep == 0 || fileStep.abs == rankStep.abs,
    "a ray direction must be orthogonal or diagonal"
  )

  def axis: RelationAxisSignal =
    if fileStep == 0 then RelationAxisSignal.File
    else if rankStep == 0 then RelationAxisSignal.Rank
    else RelationAxisSignal.Diagonal

  private[chessjudgment] def stableKey: String = s"$fileStep:$rankStep"

/** Exact chess classification of one canonical slider barrier. The
  * classification is derived from the ray occupants; it never owns a second
  * board fact.
  */
enum RelationRayPattern:
  case Ordinary
  case XRay
  case Battery
  case AbsoluteKingPin
  case KingSkewer

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
        RelationRayPattern.AbsoluteKingPin
      case Some(target) if barrier.side != owner && target.side == barrier.side &&
          barrierIsKing && !target.role.name.equalsIgnoreCase(King.name) =>
        RelationRayPattern.KingSkewer
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
      case AbsoluteKingPin => "absolute_king_pin"
      case KingSkewer      => "king_skewer"

  private[judgment] def sliderSupports(role: EvidencePieceRole, axis: RelationAxisSignal): Boolean =
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
):
  private[chessjudgment] def stableKey: String =
    s"${side.toString.toLowerCase}:${beforeRole.name.toLowerCase}>${afterRole.name.toLowerCase}:${from.key.toLowerCase}-${to.key.toLowerCase}"

object RelationMoveTransitionWitness:
  private[chessjudgment] def from(
      movement: BoardPieceTransition
  ): RelationMoveTransitionWitness =
    RelationMoveTransitionWitness(
      movement.side,
      EvidenceSquare(movement.from.key),
      EvidenceSquare(movement.to.key),
      EvidencePieceRole(movement.beforeRole.name),
      EvidencePieceRole(movement.afterRole.name)
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

/** One square in the distance-ordered controlled segment established after a
  * barrier disappears. The enclosing relation owns the controller and side.
  */
final case class RelationControlReachWitness(
    square: EvidenceSquare,
    target: RelationControlTarget
):
  private[chessjudgment] def stableKey: String =
    val targetKey = target match
      case RelationControlTarget.Empty          => "empty"
      case RelationControlTarget.Friendly(role) => s"friendly:${role.name.toLowerCase}"
      case RelationControlTarget.Enemy(role)    => s"enemy:${role.name.toLowerCase}"
    s"${square.key.toLowerCase}:$targetKey"

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
  case SliderLineInterruption(
      interposer: RelationMoveTransitionWitness,
      controllerSide: Color,
      controllerBeforeSquare: EvidenceSquare,
      controllerAfterSquare: EvidenceSquare,
      controllerRole: EvidencePieceRole,
      interruptedTargets: List[RelationControlReachWitness],
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
      openedTargets: List[RelationControlReachWitness],
      barrierPattern: RelationRayPattern,
      removalMode: RelationBlockerRemovalMode,
      proof: RelationCombinationProof
  )
  case NamedRayTransition(
      mover: RelationMoveTransitionWitness,
      side: Color,
      attackerSquare: EvidenceSquare,
      attackerRole: EvidencePieceRole,
      barrier: RelationColoredPieceWitness,
      immediateTarget: Option[RelationColoredPieceWitness],
      axis: RelationAxisSignal,
      pattern: RelationRayPattern,
      direction: RelationChangeDirection,
      proof: RelationCombinationProof
  )
  case GeometricSupportCausalTransition(
      mover: RelationMoveTransitionWitness,
      supportedSide: Color,
      supportedBeforeSquare: EvidenceSquare,
      supportedBeforeRole: EvidencePieceRole,
      supportedAfterSquare: EvidenceSquare,
      supportedAfterRole: EvidencePieceRole,
      beforeSupporters: List[RelationPieceWitness],
      afterSupporters: List[RelationPieceWitness],
      removals: List[RelationSupportRemovalWitness],
      establishments: List[RelationSupportEstablishmentWitness],
      proof: VerticalRelationDerivationProof
  )
  case CaptureRecaptureInventory(
      mover: RelationMoveTransitionWitness,
      captured: RelationColoredPieceWitness,
      geometricRecapturers: List[RelationPieceWitness],
      legalRecaptures: List[RelationLegalMoveResourceWitness],
      proof: VerticalRelationDerivationProof
  )
  case CreatedCheckResponseInventory(
      mover: RelationMoveTransitionWitness,
      checkedSide: Color,
      kingSquare: EvidenceSquare,
      checkers: List[RelationPieceWitness],
      responses: List[RelationCheckResponseWitness],
      controlledKingDestinations: List[RelationControlledKingDestinationWitness],
      terminal: RelationCheckTerminalState,
      proof: VerticalRelationDerivationProof
  )
  case RootCheckResponse(
      mover: RelationMoveTransitionWitness,
      respondingSide: Color,
      kingSquare: EvidenceSquare,
      checkers: List[RelationPieceWitness],
      response: RelationCheckResponseWitness,
      proof: VerticalRelationDerivationProof
  )
  case MovementAffordanceLegalRestriction(
      mover: RelationMoveTransitionWitness,
      restrictedSide: Color,
      piece: RelationPieceWitness,
      movementResources: List[RelationMovementResourceWitness],
      legalResources: List[RelationLegalMoveResourceWitness],
      unavailableResources: List[RelationMovementResourceWitness],
      proof: VerticalRelationDerivationProof
  )
  case AbsolutePinMovementRestriction(
      mover: RelationMoveTransitionWitness,
      restrictedSide: Color,
      pinner: RelationPieceWitness,
      pinned: RelationPieceWitness,
      kingSquare: EvidenceSquare,
      axis: RelationAxisSignal,
      movementResources: List[RelationMovementResourceWitness],
      legalResources: List[RelationLegalMoveResourceWitness],
      newlyPinForbiddenResources: List[RelationMovementResourceWitness],
      proof: VerticalRelationDerivationProof
  )
  case SliderReachDelta(
      mover: RelationMoveTransitionWitness,
      side: Color,
      sliderBefore: Option[RelationPieceWitness],
      sliderAfter: Option[RelationPieceWitness],
      direction: RelationRayDirection,
      before: Option[RelationSliderReachWitness],
      after: Option[RelationSliderReachWitness],
      proof: VerticalRelationDerivationProof
  )
  case GeometricMultiTargetContact(
      mover: RelationMoveTransitionWitness,
      controllingSide: Color,
      controllerBefore: RelationPieceWitness,
      controllerAfter: RelationPieceWitness,
      newlyEstablishedTargets: List[RelationPieceWitness],
      maintainedTargets: List[RelationPieceWitness],
      proof: VerticalRelationDerivationProof
  )
  case GeometricEnemyContactWithoutFriendlySupport(
      mover: RelationMoveTransitionWitness,
      controllingSide: Color,
      controllerBefore: RelationPieceWitness,
      controllerAfter: RelationPieceWitness,
      target: RelationPieceWitness,
      proof: VerticalRelationDerivationProof
  )
  case SharedGeometricSupportOfEnemyControlledTargets(
      mover: RelationMoveTransitionWitness,
      controllingSide: Color,
      supportedSide: Color,
      sharedSupporter: RelationPieceWitness,
      targets: List[RelationSharedGeometricSupportTargetWitness],
      proof: VerticalRelationDerivationProof
  )
  case PawnTopologyTransition(
      mover: RelationMoveTransitionWitness,
      before: Option[RelationPawnTopologyStateWitness],
      after: Option[RelationPawnTopologyStateWitness],
      changedFacets: List[RelationPawnTopologyFacet],
      proof: VerticalRelationDerivationProof
  )
  case PawnOccupiedFilePartitionTransition(
      mover: RelationMoveTransitionWitness,
      side: Color,
      before: List[RelationPawnOccupiedFileRunWitness],
      after: List[RelationPawnOccupiedFileRunWitness],
      proof: VerticalRelationDerivationProof
  )
  case MajorPiecePawnFileCorridorTransition(
      mover: RelationMoveTransitionWitness,
      side: Color,
      file: EvidenceFile,
      sliderBefore: Option[RelationPieceWitness],
      sliderAfter: Option[RelationPieceWitness],
      occurrenceChange: Option[RelationSliderOccurrenceChange],
      beforePawnFile: RelationPawnFileStateWitness,
      afterPawnFile: RelationPawnFileStateWitness,
      beforeCorridor: Option[RelationFileCorridorWitness],
      afterCorridor: Option[RelationFileCorridorWitness],
      proof: VerticalRelationDerivationProof
  )
  case CastlingRightRemoved(
      mover: RelationMoveTransitionWitness,
      side: Color,
      flank: RelationCastlingFlank,
      proof: VerticalRelationDerivationProof
  )
  case StalemateTransition(
      mover: RelationMoveTransitionWitness,
      stalledSide: Color,
      kingSquare: EvidenceSquare,
      proof: VerticalRelationDerivationProof
  )
  case PawnFileGroup(side: Color, file: EvidenceFile, pawns: List[EvidenceSquare])
  case PawnFrontOccupancy(
      side: Color,
      pawnSquare: EvidenceSquare,
      frontSquare: Option[EvidenceSquare],
      occupant: Option[RelationColoredPieceWitness]
  )
  case PawnAdvanceAffordance(
      side: Color,
      pawnSquare: EvidenceSquare,
      destinationSquare: EvidenceSquare,
      traversedSquares: List[EvidenceSquare]
  )
  case PawnPassage(side: Color, pawnSquare: EvidenceSquare, opposingPawnSquares: List[EvidenceSquare])
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
      case NamedRayTransition(_, _, _, _, _, _, _, pattern, _, _) =>
        RelationRayPattern.id(pattern)
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

final case class RelationBatteryFormationWitness(
    side: Color,
    firstSlider: RelationColoredPieceWitness,
    secondSlider: RelationColoredPieceWitness,
    axis: RelationAxisSignal
):
  require(firstSlider.side == side && secondSlider.side == side, "a battery cannot mix sides")
  require(firstSlider.square != secondSlider.square, "a battery needs two distinct sliders")
  require(
    RelationRayPattern.sliderSupports(firstSlider.role, axis) &&
      RelationRayPattern.sliderSupports(secondSlider.role, axis),
    "a battery needs two sliders compatible with its exact axis"
  )
  require(
    s"${firstSlider.square.key.toLowerCase}:${firstSlider.role.name.toLowerCase}".compareTo(
      s"${secondSlider.square.key.toLowerCase}:${secondSlider.role.name.toLowerCase}"
    ) < 0,
    "a battery formation must use canonical slider order"
  )

private[chessjudgment] object RelationRayProjection:
  def pattern(detail: RelationWitnessDetail.RayBarrier): RelationRayPattern =
    RelationRayPattern.classify(detail.side, detail.occupants, detail.axis)

  def immediateTarget(
      detail: RelationWitnessDetail.RayBarrier
  ): Option[RelationColoredPieceWitness] =
    val rear = detail.occupants.lift(1)
    pattern(detail) match
      case RelationRayPattern.AbsoluteKingPin | RelationRayPattern.XRay | RelationRayPattern.KingSkewer =>
        rear
      case RelationRayPattern.Battery =>
        rear.filter(_.side != detail.side)
      case RelationRayPattern.Ordinary =>
        None

  def liesStrictlyBetweenAttackerAnd(
      detail: RelationWitnessDetail.RayBarrier,
      target: EvidenceSquare,
      candidate: EvidenceSquare
  ): Boolean =
    require(
      detail.occupants.exists(_.square == target),
      "a ray corridor target must belong to its canonical ordered occupants"
    )
    liesStrictlyBetween(detail.attackerSquare, target, candidate)

  def liesStrictlyBetween(
      attacker: EvidenceSquare,
      target: EvidenceSquare,
      candidate: EvidenceSquare
  ): Boolean =
    BoardGeometry.liesStrictlyBetween(
      boardSquare(attacker),
      boardSquare(target),
      boardSquare(candidate)
    )

  private def boardSquare(square: EvidenceSquare): Square =
    Square.fromKey(square.key).getOrElse(
      throw IllegalArgumentException(s"invalid canonical ray square '${square.key}'")
    )

  def named(
      detail: RelationWitnessDetail.RayBarrier
  ): Option[RelationNamedRayProjection] =
    val exactPattern = pattern(detail)
    Option.when(exactPattern != RelationRayPattern.Ordinary) {
      if exactPattern == RelationRayPattern.Battery then
        val sliders = List(
          RelationColoredPieceWitness(detail.attackerSquare, detail.attackerRole, detail.side),
          detail.occupants.head
        ).sortBy(piece => piece.square.key.toLowerCase -> piece.role.name.toLowerCase)
        RelationNamedRayProjection(
          side = detail.side,
          attackerSquare = sliders.head.square,
          attackerRole = sliders.head.role,
          barrier = sliders.last,
          immediateTarget = None,
          axis = detail.axis,
          pattern = exactPattern
        )
      else
        RelationNamedRayProjection(
          side = detail.side,
          attackerSquare = detail.attackerSquare,
          attackerRole = detail.attackerRole,
          barrier = detail.occupants.head,
          immediateTarget = immediateTarget(detail),
          axis = detail.axis,
          pattern = exactPattern
        )
    }

  def batteryFormation(
      detail: RelationWitnessDetail.RayBarrier
  ): Option[RelationBatteryFormationWitness] =
    named(detail).collect {
      case projection if projection.pattern == RelationRayPattern.Battery =>
        RelationBatteryFormationWitness(
          side = projection.side,
          firstSlider = RelationColoredPieceWitness(
            projection.attackerSquare,
            projection.attackerRole,
            projection.side
          ),
          secondSlider = projection.barrier,
          axis = projection.axis
        )
    }

  def batteryFormation(
      detail: RelationWitnessDetail.NamedRayTransition
  ): Option[RelationBatteryFormationWitness] =
    Option.when(detail.pattern == RelationRayPattern.Battery) {
      val sliders = List(
        RelationColoredPieceWitness(detail.attackerSquare, detail.attackerRole, detail.side),
        detail.barrier
      ).sortBy(piece => piece.square.key.toLowerCase -> piece.role.name.toLowerCase)
      RelationBatteryFormationWitness(
        side = detail.side,
        firstSlider = sliders.head,
        secondSlider = sliders.last,
        axis = detail.axis
      )
    }

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
  private def controlReach(value: RelationControlReachWitness): String =
    tuple("control-reach", List(square(value.square), controlTarget(value.target)))
  private def legalCapture(value: RelationLegalCaptureWitness): String =
    tuple(
      "legal-capture",
      List(
        square(value.capturedSquare),
        role(value.capturedRole),
        side(value.capturedSide)
      )
    )
  private def supportRemoval(value: RelationSupportRemovalWitness): String =
    tuple("support-removal", List(piece(value.supporter), sequence(value.causes.map(_.stableKey))))
  private def supportEstablishment(value: RelationSupportEstablishmentWitness): String =
    tuple("support-establishment", List(piece(value.supporter), sequence(value.causes.map(_.stableKey))))
  private def movementResource(value: RelationMovementResourceWitness): String =
    tuple(
      "movement-resource",
      List(square(value.destination), controlTarget(value.target), value.mode.stableKey)
    )
  private def sliderReach(value: RelationSliderReachWitness): String =
    tuple(
      "slider-reach",
      List(
        sequence(value.segment.map(controlReach)),
        optional(value.firstOccupant)(coloredPiece)
      )
    )
  private def pawnConnection(value: RelationPawnConnectionWitness): String =
    tuple(
      "pawn-connection",
      List(square(value.peer), value.kind.stableKey, value.direction.stableKey)
    )
  private def pawnTopologyState(value: RelationPawnTopologyStateWitness): String =
    tuple(
      "pawn-topology-state",
      List(
        side(value.side),
        square(value.square),
        value.doubled.toString,
        value.isolated.toString,
        value.passed.toString,
        value.geometricallyProtectedPasser.toString,
        optional(value.frontSquare)(square),
        optional(value.frontOccupant)(coloredPiece),
        sequence(value.componentPawns.map(square)),
        sequence(value.connections.map(pawnConnection)),
        sequence(value.enemyPawnContacts.map(square))
      )
    )
  private def pawnTopologySquares(value: RelationPawnTopologyStateWitness): List[EvidenceSquare] =
    (value.square :: value.frontSquare.toList ++ value.frontOccupant.map(_.square) ++
      value.componentPawns ++ value.connections.map(_.peer) ++ value.enemyPawnContacts).distinct
  private def pawnOccupiedFileRun(value: RelationPawnOccupiedFileRunWitness): String =
    tuple(
      "pawn-occupied-file-run",
      List(sequence(value.files.map(boardFile)), sequence(value.pawns.map(square)))
    )
  private def changedPawnOccupiedFileRuns(
      before: List[RelationPawnOccupiedFileRunWitness],
      after: List[RelationPawnOccupiedFileRunWitness]
  ): List[RelationPawnOccupiedFileRunWitness] =
    def fileKey(run: RelationPawnOccupiedFileRunWitness): String =
      run.files.map(_.key.toLowerCase).mkString
    val beforeKeys = before.map(fileKey).toSet
    val afterKeys = after.map(fileKey).toSet
    before.filterNot(run => afterKeys(fileKey(run))) ++
      after.filterNot(run => beforeKeys(fileKey(run)))
  private def optional[A](value: Option[A])(encode: A => String): String =
    value.map(item => tuple("some", List(encode(item)))).getOrElse(tuple("none", Nil))

  /** Injective, representation-independent identity of one typed relation
    * detail. Case-class `toString` is intentionally excluded from graph ids,
    * ordering, and public mechanism identity.
    */
  private def encodedKey(detail: RelationWitnessDetail, includeDerivation: Boolean): String =
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
          values(establishedControllers.map(piece))
        ) ++ Option.when(includeDerivation)(proof.stableKey).toList
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
          role(supportedRole)
        ) ++ Option.when(includeDerivation)(proof.stableKey).toList
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
          values(establishedSupporters.map(piece))
        ) ++ Option.when(includeDerivation)(proof.stableKey).toList
      case SliderLineInterruption(
            interposer,
            controllerSide,
            controllerBefore,
            controllerAfter,
            controllerRole,
            interruptedTargets,
            proof
        ) =>
        require(interruptedTargets.nonEmpty, "a line interruption needs a non-empty affected corridor")
        require(
          interruptedTargets.map(_.square).distinct.size == interruptedTargets.size,
          "a line interruption cannot repeat an affected square"
        )
        List(
          moveTransition(interposer),
          side(controllerSide),
          square(controllerBefore),
          square(controllerAfter),
          role(controllerRole),
          sequence(interruptedTargets.map(controlReach))
        ) ++ Option.when(includeDerivation)(proof.stableKey).toList
      case GeometricLineControlAfterBlockerRemoval(
            mover,
            controllerSide,
            controllerBefore,
            controllerAfter,
            controllerRole,
            blocker,
            blockerRole,
            openedTargets,
            barrierPattern,
            removalMode,
            proof
        ) =>
        require(openedTargets.nonEmpty, "a line opening needs a non-empty controlled corridor")
        require(
          openedTargets.map(_.square).distinct.size == openedTargets.size,
          "a line-opening corridor cannot repeat a controlled square"
        )
        List(
          moveTransition(mover),
          side(controllerSide),
          square(controllerBefore),
          square(controllerAfter),
          role(controllerRole),
          square(blocker),
          role(blockerRole),
          sequence(openedTargets.map(controlReach)),
          RelationRayPattern.id(barrierPattern),
          removalMode.toString
        ) ++ Option.when(includeDerivation)(proof.stableKey).toList
      case NamedRayTransition(
            mover,
            owner,
            attacker,
            attackerRole,
            barrier,
            immediateTarget,
            axis,
            pattern,
            direction,
            proof
          ) =>
        require(pattern != RelationRayPattern.Ordinary, "a named-ray transition needs a named pattern")
        require(
          pattern == RelationRayPattern.Battery || immediateTarget.nonEmpty,
          "a pin, skewer, or x-ray transition needs its exact rear target"
        )
        List(
          moveTransition(mover),
          side(owner),
          square(attacker),
          role(attackerRole),
          coloredPiece(barrier),
          immediateTarget.map(coloredPiece).getOrElse("none"),
          axis.toString,
          RelationRayPattern.id(pattern),
          direction.toString
        ) ++ Option.when(includeDerivation)(proof.stableKey).toList
      case GeometricSupportCausalTransition(
            mover,
            supportedSide,
            supportedBeforeSquare,
            supportedBeforeRole,
            supportedAfterSquare,
            supportedAfterRole,
            beforeSupporters,
            afterSupporters,
            removals,
            establishments,
            proof
          ) =>
        def canonicalSupporters(values: List[RelationPieceWitness]): Boolean =
          values.distinct.size == values.size &&
            values == values.sortBy(value => value.square.key -> value.role.name)
        require(
          removals.nonEmpty || establishments.nonEmpty,
          "a causal support transition needs one changed support relation"
        )
        require(
          canonicalSupporters(beforeSupporters) && canonicalSupporters(afterSupporters) &&
            removals.map(_.supporter).distinct.size == removals.size &&
            removals == removals.sortBy(value => value.supporter.square.key -> value.supporter.role.name) &&
            removals.forall(removal => beforeSupporters.contains(removal.supporter)) &&
            establishments.map(_.supporter).distinct.size == establishments.size &&
            establishments == establishments.sortBy(value => value.supporter.square.key -> value.supporter.role.name) &&
            establishments.forall(establishment => afterSupporters.contains(establishment.supporter)),
          "a causal support transition must retain canonical closed support sets and exact changed members"
        )
        List(
          moveTransition(mover),
          side(supportedSide),
          square(supportedBeforeSquare),
          role(supportedBeforeRole),
          square(supportedAfterSquare),
          role(supportedAfterRole),
          values(beforeSupporters.map(piece)),
          values(afterSupporters.map(piece)),
          values(removals.map(supportRemoval)),
          values(establishments.map(supportEstablishment))
        ) ++ Option.when(includeDerivation)(proof.stableKey).toList
      case CaptureRecaptureInventory(mover, captured, geometricRecapturers, legalRecaptures, proof) =>
        require(
          geometricRecapturers.distinct.size == geometricRecapturers.size &&
            geometricRecapturers == geometricRecapturers.sortBy(value => value.square.key -> value.role.name),
          "geometric recapturers must be unique and canonically ordered"
        )
        require(
          legalRecaptures.distinct.size == legalRecaptures.size &&
            legalRecaptures == legalRecaptures.sortBy(_.stableKey),
          "legal recaptures must be unique and canonically ordered"
        )
        List(
          moveTransition(mover),
          coloredPiece(captured),
          values(geometricRecapturers.map(piece)),
          values(legalRecaptures.map(_.stableKey))
        ) ++ Option.when(includeDerivation)(proof.stableKey).toList
      case CreatedCheckResponseInventory(
            mover,
            checkedSide,
            kingSquare,
            checkers,
            responses,
            controlledKingDestinations,
            terminal,
            proof
          ) =>
        require(
          checkers.nonEmpty && checkers.distinct.size == checkers.size &&
            checkers == checkers.sortBy(value => value.square.key -> value.role.name),
          "a check response inventory needs unique canonical checkers"
        )
        require(
          responses.distinct.size == responses.size && responses == responses.sortBy(_.stableKey),
          "check responses must be unique and canonically ordered"
        )
        require(
          controlledKingDestinations.distinct.size == controlledKingDestinations.size &&
            controlledKingDestinations.map(_.resource.destination).distinct.size == controlledKingDestinations.size &&
            controlledKingDestinations == controlledKingDestinations.sortBy(_.stableKey),
          "controlled king destinations must be unique and canonically ordered"
        )
        require(
          (terminal == RelationCheckTerminalState.Checkmate) == responses.isEmpty,
          "checkmate is exactly a checked position without legal responses"
        )
        List(
          moveTransition(mover),
          side(checkedSide),
          square(kingSquare),
          values(checkers.map(piece)),
          values(responses.map(_.stableKey)),
          values(controlledKingDestinations.map(_.stableKey)),
          terminal.toString.toLowerCase
        ) ++ Option.when(includeDerivation)(proof.stableKey).toList
      case RootCheckResponse(mover, respondingSide, kingSquare, checkers, response, proof) =>
        require(
          respondingSide == mover.side && checkers.nonEmpty &&
            checkers.distinct.size == checkers.size &&
            checkers == checkers.sortBy(value => value.square.key -> value.role.name) &&
            response.resource.movement == mover,
          "a root check response needs the exact mover, king, and canonical checker set"
        )
        List(
          moveTransition(mover),
          side(respondingSide),
          square(kingSquare),
          values(checkers.map(piece)),
          response.stableKey
        ) ++ Option.when(includeDerivation)(proof.stableKey).toList
      case MovementAffordanceLegalRestriction(
            mover,
            restrictedSide,
            restrictedPiece,
            movementResources,
            legalResources,
            unavailableResources,
            proof
          ) =>
        val movementDestinations = movementResources.map(_.destination).toSet
        val legalDestinations = legalResources.map(_.movement.to).toSet
        require(restrictedSide != mover.side, "an after-position legal restriction belongs to the reply side")
        require(
          movementResources.nonEmpty && movementResources.distinct.size == movementResources.size &&
            movementResources == movementResources.sortBy(_.stableKey) &&
            legalResources.distinct.size == legalResources.size && legalResources == legalResources.sortBy(_.stableKey) &&
            legalResources.forall(resource =>
              resource.movement.side == restrictedSide && resource.movement.from == restrictedPiece.square &&
                movementDestinations(resource.movement.to)
            ) &&
            unavailableResources.nonEmpty && unavailableResources.distinct.size == unavailableResources.size &&
            unavailableResources == unavailableResources.sortBy(_.stableKey) &&
            unavailableResources.forall(movementResources.contains) &&
            unavailableResources.forall(resource => !legalDestinations(resource.destination)),
          "a movement-affordance legal restriction needs canonical movement, legal, and unavailable resources"
        )
        List(
          moveTransition(mover),
          side(restrictedSide),
          piece(restrictedPiece),
          sequence(movementResources.map(movementResource)),
          sequence(legalResources.map(_.stableKey)),
          sequence(unavailableResources.map(movementResource))
        ) ++ Option.when(includeDerivation)(proof.stableKey).toList
      case AbsolutePinMovementRestriction(
            mover,
            restrictedSide,
            pinner,
            pinned,
            kingSquare,
            axis,
            movementResources,
            legalResources,
            newlyPinForbiddenResources,
            proof
          ) =>
        require(pinner.square != pinned.square, "an absolute pin needs distinct pinner and pinned piece")
        require(pinned.square != kingSquare, "an absolute pin needs the pinned piece in front of its king")
        require(restrictedSide != mover.side, "an after-position pin restriction belongs to the reply side")
        require(
          movementResources.nonEmpty && movementResources.distinct.size == movementResources.size &&
            movementResources == movementResources.sortBy(_.stableKey) &&
            legalResources.distinct.size == legalResources.size && legalResources == legalResources.sortBy(_.stableKey) &&
            legalResources.forall(resource =>
              resource.movement.side == restrictedSide && resource.movement.from == pinned.square &&
                movementResources.exists(_.destination == resource.movement.to)
            ) &&
            newlyPinForbiddenResources.nonEmpty &&
            newlyPinForbiddenResources.distinct.size == newlyPinForbiddenResources.size &&
            newlyPinForbiddenResources == newlyPinForbiddenResources.sortBy(_.stableKey) &&
            newlyPinForbiddenResources.forall(movementResources.contains) &&
            newlyPinForbiddenResources.forall(forbidden =>
              legalResources.forall(_.movement.to != forbidden.destination)
            ),
          "an absolute pin restriction needs canonical movement, legal, and newly forbidden resources"
        )
        List(
          moveTransition(mover),
          side(restrictedSide),
          piece(pinner),
          piece(pinned),
          square(kingSquare),
          axis.toString,
          sequence(movementResources.map(movementResource)),
          sequence(legalResources.map(_.stableKey)),
          sequence(newlyPinForbiddenResources.map(movementResource))
        ) ++ Option.when(includeDerivation)(proof.stableKey).toList
      case SliderReachDelta(mover, owner, sliderBefore, sliderAfter, direction, before, after, proof) =>
        require(sliderBefore.nonEmpty || sliderAfter.nonEmpty, "a slider-reach delta needs one slider occurrence")
        require(before != after, "a slider-reach delta must change its exact reachable segment")
        require(
          (sliderBefore.toList ++ sliderAfter.toList).forall(piece =>
            List(Bishop.name, Rook.name, Queen.name).exists(_.equalsIgnoreCase(piece.role.name))
          ),
          "a slider-reach delta accepts only bishop, rook, or queen occurrences"
        )
        List(
          moveTransition(mover),
          side(owner),
          optional(sliderBefore)(piece),
          optional(sliderAfter)(piece),
          direction.stableKey,
          optional(before)(sliderReach),
          optional(after)(sliderReach)
        ) ++ Option.when(includeDerivation)(proof.stableKey).toList
      case GeometricMultiTargetContact(
            mover,
            controllingSide,
            controllerBefore,
            controllerAfter,
            newlyEstablishedTargets,
            maintainedTargets,
            proof
          ) =>
        val targets = newlyEstablishedTargets ++ maintainedTargets
        require(controllingSide == mover.side, "multi-target control must belong to the root mover's side")
        require(
          newlyEstablishedTargets.nonEmpty && targets.size >= 2 && targets.distinct.size == targets.size,
          "multi-target control needs one new and at least two total enemy contacts"
        )
        List(
          moveTransition(mover),
          side(controllingSide),
          piece(controllerBefore),
          piece(controllerAfter),
          values(newlyEstablishedTargets.map(piece)),
          values(maintainedTargets.map(piece))
        ) ++ Option.when(includeDerivation)(proof.stableKey).toList
      case GeometricEnemyContactWithoutFriendlySupport(
            mover,
            controllingSide,
            controllerBefore,
            controllerAfter,
            target,
            proof
          ) =>
        require(controllingSide == mover.side, "a new enemy contact must belong to the root mover's side")
        List(
          moveTransition(mover),
          side(controllingSide),
          piece(controllerBefore),
          piece(controllerAfter),
          piece(target)
        ) ++ Option.when(includeDerivation)(proof.stableKey).toList
      case SharedGeometricSupportOfEnemyControlledTargets(mover, controllingSide, supportedSide, sharedSupporter, targets, proof) =>
        require(controllingSide == mover.side && supportedSide != controllingSide, "enemy control and friendly support must join opposing sides")
        require(
          targets.size >= 2 && targets.distinct.size == targets.size &&
            targets == targets.sortBy(_.stableKey) && targets.forall(_.friendlySupporters.contains(sharedSupporter)),
          "shared geometric support needs two canonical enemy-controlled targets with the same friendly supporter"
        )
        List(
          moveTransition(mover),
          side(controllingSide),
          side(supportedSide),
          piece(sharedSupporter),
          values(targets.map(_.stableKey))
        ) ++ Option.when(includeDerivation)(proof.stableKey).toList
      case PawnTopologyTransition(mover, before, after, changedFacets, proof) =>
        require(before.nonEmpty, "a pawn topology transition needs its before occurrence")
        require(
          changedFacets.nonEmpty && changedFacets == RelationPawnTopologyFacet.changed(before, after),
          "pawn topology facets must be the exact canonical state difference"
        )
        require(
          proof.provesPawnTopologyFacets(before, after, changedFacets),
          "every pawn topology facet must retain its exact closed L0 proof domain"
        )
        List(
          moveTransition(mover),
          optional(before)(pawnTopologyState),
          optional(after)(pawnTopologyState),
          sequence(changedFacets.map(_.stableKey))
        ) ++ Option.when(includeDerivation)(proof.stableKey).toList
      case PawnOccupiedFilePartitionTransition(mover, owner, before, after, proof) =>
        require(
          RelationPawnOccupiedFileRunWitness.canonicalPartition(before) &&
            RelationPawnOccupiedFileRunWitness.canonicalPartition(after),
          "an occupied-file partition transition needs canonical before and after partitions"
        )
        require(
          before.map(_.files) != after.map(_.files),
          "an occupied-file partition transition must change its maximal file runs"
        )
        List(
          moveTransition(mover),
          side(owner),
          sequence(before.map(pawnOccupiedFileRun)),
          sequence(after.map(pawnOccupiedFileRun))
        ) ++ Option.when(includeDerivation)(proof.stableKey).toList
      case MajorPiecePawnFileCorridorTransition(
            mover,
            owner,
            file,
            sliderBefore,
            sliderAfter,
            occurrenceChange,
            beforePawnFile,
            afterPawnFile,
            beforeCorridor,
            afterCorridor,
            proof
          ) =>
        val pieces = sliderBefore.toList ++ sliderAfter.toList
        def onFile(value: RelationPieceWitness): Boolean =
          EvidenceFile.contains(file, value.square)
        require(
          pieces.nonEmpty && pieces.forall(piece =>
            piece.role.name.equalsIgnoreCase(Rook.name) || piece.role.name.equalsIgnoreCase(Queen.name)
          ),
          "a pawn-file corridor transition needs an exact rook or queen occurrence"
        )
        require(
          beforePawnFile.file == file && afterPawnFile.file == file &&
            sliderBefore.exists(onFile) == beforeCorridor.nonEmpty &&
            sliderAfter.exists(onFile) == afterCorridor.nonEmpty,
          "a pawn-file corridor must bind each two-ray state to the slider occurrence on its file"
        )
        require(
          beforeCorridor.nonEmpty && beforePawnFile.opennessFor(owner).ownPawnAbsent ||
            afterCorridor.nonEmpty && afterPawnFile.opennessFor(owner).ownPawnAbsent,
          "a pawn-file corridor transition needs one usable exact two-direction corridor"
        )
        require(
          occurrenceChange.isEmpty == (sliderBefore == sliderAfter),
          "a changed slider occurrence needs one exact movement, promotion, or capture cause"
        )
        occurrenceChange.foreach {
          case RelationSliderOccurrenceChange.PieceTransition(movement) =>
            require(
              movement.side == owner && sliderAfter.exists(after =>
                movement.to == after.square && movement.afterRole == after.role
              ) && (sliderBefore match
                case Some(before) =>
                  movement.from == before.square && movement.beforeRole == before.role
                case None => movement.beforeRole.name.equalsIgnoreCase(Pawn.name)),
              "a slider occurrence transition must bind its exact before and after identities"
            )
          case RelationSliderOccurrenceChange.Captured(piece) =>
            require(
              piece.side == owner && sliderBefore.contains(RelationPieceWitness(piece.square, piece.role)) &&
                sliderAfter.isEmpty,
              "a captured slider cause must bind the exact vanished occurrence"
            )
        }
        require(
          sliderBefore != sliderAfter || beforeCorridor != afterCorridor ||
            beforePawnFile.opennessFor(owner) != afterPawnFile.opennessFor(owner),
          "a pawn-file corridor transition must change its occurrence, owner-relative openness, or two-ray state"
        )
        List(
          moveTransition(mover),
          side(owner),
          boardFile(file),
          optional(sliderBefore)(piece),
          optional(sliderAfter)(piece),
          optional(occurrenceChange)(_.stableKey),
          beforePawnFile.stableKey,
          afterPawnFile.stableKey,
          optional(beforeCorridor)(_.stableKey),
          optional(afterCorridor)(_.stableKey)
        ) ++ Option.when(includeDerivation)(proof.stableKey).toList
      case CastlingRightRemoved(mover, owner, flank, proof) =>
        List(
          moveTransition(mover),
          side(owner),
          flank.stableKey
        ) ++ Option.when(includeDerivation)(proof.stableKey).toList
      case StalemateTransition(mover, stalledSide, kingSquare, proof) =>
        require(stalledSide != mover.side, "the after-position side to move must be the stalemated side")
        List(
          moveTransition(mover),
          side(stalledSide),
          square(kingSquare)
        ) ++ Option.when(includeDerivation)(proof.stableKey).toList
      case PawnFileGroup(owner, file, pawns) =>
        List(side(owner), boardFile(file), values(pawns.map(square)))
      case PawnFrontOccupancy(owner, pawn, front, occupant) =>
        List(side(owner), square(pawn), optional(front)(square), optional(occupant)(coloredPiece))
      case PawnAdvanceAffordance(owner, pawn, destination, traversed) =>
        require(
          traversed.nonEmpty && traversed.size <= 2 && traversed.distinct.size == traversed.size &&
            traversed.last == destination,
          "a pawn advance affordance needs its exact one- or two-cell empty corridor"
        )
        List(side(owner), square(pawn), square(destination), sequence(traversed.map(square)))
      case PawnPassage(owner, pawn, opposingPawns) =>
        List(side(owner), square(pawn), values(opposingPawns.map(square)))
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

  private[chessjudgment] def createdCheckResponse(
      detail: RelationWitnessDetail
  ): Option[CreatedCheckResponseInventory] =
    detail match
      case value: CreatedCheckResponseInventory => Some(value)
      case _                                    => None

  /** Exact derivation identity. Different valid proofs of the same assertion
    * remain different graph paths.
    */
  def stableKey(detail: RelationWitnessDetail): String =
    encodedKey(detail, includeDerivation = true)

  /** Chess assertion identity without its proof route. This is for grouping,
    * never for deleting a second derivation.
    */
  def assertionStableKey(detail: RelationWitnessDetail): String =
    encodedKey(detail, includeDerivation = false)

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
      case _: SliderLineInterruption         => RelationFactKind.SliderLineInterruption
      case _: GeometricLineControlAfterBlockerRemoval =>
        RelationFactKind.GeometricLineControlAfterBlockerRemoval
      case _: NamedRayTransition   => RelationFactKind.NamedRayTransition
      case _: GeometricSupportCausalTransition => RelationFactKind.GeometricSupportCausalTransition
      case _: CaptureRecaptureInventory => RelationFactKind.CaptureRecaptureInventory
      case _: CreatedCheckResponseInventory => RelationFactKind.CreatedCheckResponseInventory
      case _: RootCheckResponse => RelationFactKind.RootCheckResponse
      case _: MovementAffordanceLegalRestriction => RelationFactKind.MovementAffordanceLegalRestriction
      case _: AbsolutePinMovementRestriction => RelationFactKind.AbsolutePinMovementRestriction
      case _: SliderReachDelta => RelationFactKind.SliderReachDelta
      case _: GeometricMultiTargetContact => RelationFactKind.GeometricMultiTargetContact
      case _: GeometricEnemyContactWithoutFriendlySupport =>
        RelationFactKind.GeometricEnemyContactWithoutFriendlySupport
      case _: SharedGeometricSupportOfEnemyControlledTargets => RelationFactKind.SharedGeometricSupportOfEnemyControlledTargets
      case _: PawnTopologyTransition => RelationFactKind.PawnTopologyTransition
      case _: PawnOccupiedFilePartitionTransition => RelationFactKind.PawnOccupiedFilePartitionTransition
      case _: MajorPiecePawnFileCorridorTransition => RelationFactKind.MajorPiecePawnFileCorridorTransition
      case _: CastlingRightRemoved => RelationFactKind.CastlingRightRemoved
      case _: StalemateTransition => RelationFactKind.StalemateTransition
      case _: PawnFileGroup        => RelationFactKind.PawnFileGroup
      case _: PawnFrontOccupancy   => RelationFactKind.PawnFrontOccupancy
      case _: PawnAdvanceAffordance => RelationFactKind.PawnAdvanceAffordance
      case _: PawnPassage          => RelationFactKind.PawnPassage
      case _: RayBarrier           => RelationFactKind.RayBarrier

  private[chessjudgment] def proofStage(detail: RelationWitnessDetail): RelationProofStage =
    detail match
      case _: GeometricControl | _: LegalMove | _: PawnFileGroup |
          _: PawnFrontOccupancy | _: PawnAdvanceAffordance | _: PawnPassage |
          _: RayBarrier =>
        RelationProofStage.PositionFact
      case _: GeometricControlSetDelta | _: GeometricSupporterCapture | _: GeometricSupportDelta |
          _: SliderLineInterruption | _: GeometricLineControlAfterBlockerRemoval | _: NamedRayTransition =>
        RelationProofStage.TransitionFact
      case vertical =>
        VerticalRelationContractKind.forDetail(vertical)
          .map(VerticalRelationContractKind.outputStage)
          .getOrElse(
            throw IllegalArgumentException(
              s"relation '${vertical.detailName}' has no registered proof stage"
            )
          )

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
      case SliderLineInterruption(_, _, _, _, _, _, proof) => Some(proof)
      case NamedRayTransition(_, _, _, _, _, _, _, _, _, proof) => Some(proof)
      case GeometricLineControlAfterBlockerRemoval(
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            proof
          ) =>
        Some(proof)
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
        case SliderLineInterruption(mover, _, controllerBefore, controllerAfter, _, targets, _) =>
          List(mover.from, mover.to, controllerBefore, controllerAfter) ++ targets.map(_.square)
        case GeometricLineControlAfterBlockerRemoval(mover, _, controllerBefore, controllerAfter, _, blocker, _, opened, _, _, _) =>
          List(mover.from, mover.to, controllerBefore, controllerAfter, blocker) ++ opened.map(_.square)
        case NamedRayTransition(mover, _, attacker, _, barrier, immediateTarget, _, _, _, _) =>
          List(mover.from, mover.to, attacker, barrier.square) ++ immediateTarget.map(_.square)
        case GeometricSupportCausalTransition(
              mover,
              _,
              supportedBefore,
              _,
              supportedAfter,
              _,
              beforeSupporters,
              afterSupporters,
              removals,
              establishments,
              _
            ) =>
          List(mover.from, mover.to, supportedBefore, supportedAfter) ++
            (beforeSupporters ++ afterSupporters ++ removals.map(_.supporter) ++
              establishments.map(_.supporter)).map(_.square)
        case CaptureRecaptureInventory(mover, captured, geometricRecapturers, legalRecaptures, _) =>
          List(mover.from, mover.to, captured.square) ++ geometricRecapturers.map(_.square) ++
            legalRecaptures.flatMap(resource => List(resource.movement.from, resource.movement.to))
        case CreatedCheckResponseInventory(
              mover,
              _,
              kingSquare,
              checkers,
              responses,
              controlledKingDestinations,
              _,
              _
            ) =>
          List(mover.from, mover.to, kingSquare) ++ checkers.map(_.square) ++
            responses.flatMap(response => List(response.resource.movement.from, response.resource.movement.to)) ++
            controlledKingDestinations.flatMap(destination =>
              destination.resource.destination :: destination.controllers.map(_.square)
            )
        case RootCheckResponse(mover, _, kingSquare, checkers, response, _) =>
          List(mover.from, mover.to, kingSquare) ++ checkers.map(_.square) ++
            List(response.resource.movement.from, response.resource.movement.to)
        case MovementAffordanceLegalRestriction(mover, _, restrictedPiece, geometric, legal, unavailable, _) =>
          List(mover.from, mover.to, restrictedPiece.square) ++ geometric.map(_.destination) ++
            legal.flatMap(resource => List(resource.movement.from, resource.movement.to)) ++
            unavailable.map(_.destination)
        case AbsolutePinMovementRestriction(mover, _, pinner, pinned, kingSquare, _, geometric, legal, forbidden, _) =>
          List(mover.from, mover.to, pinner.square, pinned.square, kingSquare) ++
            geometric.map(_.destination) ++ legal.flatMap(resource => List(resource.movement.from, resource.movement.to)) ++
            forbidden.map(_.destination)
        case SliderReachDelta(mover, _, sliderBefore, sliderAfter, _, before, after, _) =>
          List(mover.from, mover.to) ++ sliderBefore.map(_.square) ++ sliderAfter.map(_.square) ++
            before.toList.flatMap(reach => reach.segment.map(_.square) ++ reach.firstOccupant.map(_.square)) ++
            after.toList.flatMap(reach => reach.segment.map(_.square) ++ reach.firstOccupant.map(_.square))
        case GeometricMultiTargetContact(mover, _, controllerBefore, controllerAfter, newlyEstablished, maintained, _) =>
          List(mover.from, mover.to, controllerBefore.square, controllerAfter.square) ++
            (newlyEstablished ++ maintained).map(_.square)
        case GeometricEnemyContactWithoutFriendlySupport(mover, _, controllerBefore, controllerAfter, target, _) =>
          List(mover.from, mover.to, controllerBefore.square, controllerAfter.square, target.square)
        case SharedGeometricSupportOfEnemyControlledTargets(mover, _, _, sharedSupporter, targets, _) =>
          List(mover.from, mover.to, sharedSupporter.square) ++ targets.flatMap(target =>
            target.target.square :: (target.enemyControllers ++ target.friendlySupporters).map(_.square)
          )
        case PawnTopologyTransition(mover, before, after, _, _) =>
          List(mover.from, mover.to) ++ before.toList.flatMap(pawnTopologySquares) ++
            after.toList.flatMap(pawnTopologySquares)
        case PawnOccupiedFilePartitionTransition(mover, _, before, after, _) =>
          List(mover.from, mover.to) ++ changedPawnOccupiedFileRuns(before, after).flatMap(_.pawns)
        case MajorPiecePawnFileCorridorTransition(
              mover,
              _,
              _,
              sliderBefore,
              sliderAfter,
              _,
              beforePawnFile,
              afterPawnFile,
              beforeCorridor,
              afterCorridor,
              _
            ) =>
          def corridorSquares(corridor: Option[RelationFileCorridorWitness]): List[EvidenceSquare] =
            corridor.toList.flatMap(_.rays.flatMap(_.reach.toList.flatMap(reach =>
              reach.segment.map(_.square) ++ reach.firstOccupant.map(_.square)
            )))
          List(mover.from, mover.to) ++ sliderBefore.map(_.square) ++ sliderAfter.map(_.square) ++
            beforePawnFile.whitePawns ++ beforePawnFile.blackPawns ++
            afterPawnFile.whitePawns ++ afterPawnFile.blackPawns ++
            corridorSquares(beforeCorridor) ++ corridorSquares(afterCorridor)
        case CastlingRightRemoved(mover, _, _, _) =>
          List(mover.from, mover.to)
        case StalemateTransition(mover, _, kingSquare, _) =>
          List(mover.from, mover.to, kingSquare)
        case PawnFileGroup(_, _, pawns) =>
          pawns
        case PawnFrontOccupancy(_, pawnSquare, frontSquare, occupant) =>
          pawnSquare :: (frontSquare.toList ++ occupant.map(_.square))
        case PawnAdvanceAffordance(_, pawnSquare, _, traversedSquares) =>
          pawnSquare :: traversedSquares
        case PawnPassage(_, pawnSquare, opposingPawnSquares) =>
          pawnSquare :: opposingPawnSquares
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
      case SliderLineInterruption(mover, _, _, _, _, targets, _) =>
        mover.to :: targets.map(_.square)
      case GeometricLineControlAfterBlockerRemoval(_, _, _, _, _, _, _, opened, _, _, _) =>
        opened.map(_.square)
      case NamedRayTransition(_, _, _, _, barrier, immediateTarget, _, _, _, _) =>
        immediateTarget.map(_.square).getOrElse(barrier.square) :: Nil
      case GeometricSupportCausalTransition(_, _, _, _, supportedAfter, _, _, _, _, _, _) =>
        List(supportedAfter)
      case CaptureRecaptureInventory(mover, _, _, _, _) =>
        List(mover.to)
      case CreatedCheckResponseInventory(_, _, kingSquare, _, _, _, _, _) =>
        List(kingSquare)
      case RootCheckResponse(_, _, kingSquare, _, _, _) =>
        List(kingSquare)
      case MovementAffordanceLegalRestriction(_, _, restrictedPiece, _, _, unavailable, _) =>
        restrictedPiece.square :: unavailable.map(_.destination)
      case AbsolutePinMovementRestriction(_, _, _, pinned, _, _, _, _, forbidden, _) =>
        pinned.square :: forbidden.map(_.destination)
      case SliderReachDelta(_, _, _, _, _, _, after, _) =>
        after.toList.flatMap(_.segment.map(_.square))
      case GeometricMultiTargetContact(_, _, _, _, newlyEstablished, maintained, _) =>
        (newlyEstablished ++ maintained).map(_.square)
      case GeometricEnemyContactWithoutFriendlySupport(_, _, _, _, target, _) =>
        List(target.square)
      case SharedGeometricSupportOfEnemyControlledTargets(_, _, _, _, targets, _) =>
        targets.map(_.target.square)
      case PawnTopologyTransition(_, before, after, _, _) =>
        after.orElse(before).toList.map(_.square)
      case PawnOccupiedFilePartitionTransition(_, _, before, after, _) =>
        changedPawnOccupiedFileRuns(before, after).flatMap(_.pawns)
      case MajorPiecePawnFileCorridorTransition(_, _, _, sliderBefore, sliderAfter, _, _, _, _, _, _) =>
        sliderAfter.orElse(sliderBefore).toList.map(_.square)
      case CastlingRightRemoved(_, _, _, _) =>
        Nil
      case StalemateTransition(_, _, kingSquare, _) =>
        List(kingSquare)
      case PawnFileGroup(_, _, pawns) =>
        pawns
      case PawnFrontOccupancy(_, pawnSquare, frontSquare, _) =>
        frontSquare.toList match
          case Nil     => List(pawnSquare)
          case squares => squares
      case PawnAdvanceAffordance(_, _, destinationSquare, _) =>
        List(destinationSquare)
      case PawnPassage(_, pawnSquare, _) =>
        List(pawnSquare)
      case ray: RayBarrier =>
        RelationRayProjection.immediateTarget(ray).map(_.square).toList
    squares.distinct

  def files(detail: RelationWitnessDetail): List[EvidenceFile] =
    detail match
      case PawnOccupiedFilePartitionTransition(_, _, before, after, _) =>
        changedPawnOccupiedFileRuns(before, after).flatMap(_.files).distinct.sortBy(_.key)
      case MajorPiecePawnFileCorridorTransition(_, _, file, _, _, _, _, _, _, _, _) => List(file)
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
        case SliderLineInterruption(mover, _, controllerBefore, controllerAfter, controllerRole, targets, _) =>
          List(
            part(mover.from, RelationParticipantRole.Mover, Some(mover.beforeRole)),
            part(mover.to, RelationParticipantRole.Blocker, Some(mover.afterRole)),
            part(controllerBefore, RelationParticipantRole.Controller, Some(controllerRole)),
            part(controllerAfter, RelationParticipantRole.Controller, Some(controllerRole))
          ) ++ targets.map(target =>
            part(
              target.square,
              target.target match
                case RelationControlTarget.Friendly(_) => RelationParticipantRole.Supported
                case _                                 => RelationParticipantRole.Target,
              target.target.pieceRole
            )
          )
        case GeometricLineControlAfterBlockerRemoval(mover, _, controllerBefore, controllerAfter, controllerRole, blocker, blockerRole, opened, _, _, _) =>
          List(
            part(mover.from, RelationParticipantRole.Mover, Some(mover.beforeRole)),
            part(mover.to, RelationParticipantRole.Mover, Some(mover.afterRole)),
            part(controllerBefore, RelationParticipantRole.Controller, Some(controllerRole)),
            part(controllerAfter, RelationParticipantRole.Controller, Some(controllerRole)),
            part(blocker, RelationParticipantRole.Blocker, Some(blockerRole))
          ) ++ opened.map(reach =>
            part(
              reach.square,
              reach.target match
                case RelationControlTarget.Friendly(_) => RelationParticipantRole.Supported
                case _                                 => RelationParticipantRole.Target,
              reach.target.pieceRole
            )
          )
        case NamedRayTransition(mover, owner, attacker, attackerRole, barrier, immediateTarget, _, pattern, _, _) =>
          val barrierRole =
            if pattern == RelationRayPattern.Battery then RelationParticipantRole.Attacker
            else if barrier.role.name.equalsIgnoreCase(King.name) then RelationParticipantRole.King
            else RelationParticipantRole.Blocker
          List(
            part(mover.from, RelationParticipantRole.Mover, Some(mover.beforeRole)),
            part(mover.to, RelationParticipantRole.Mover, Some(mover.afterRole)),
            part(attacker, RelationParticipantRole.Attacker, Some(attackerRole)),
            part(barrier.square, barrierRole, Some(barrier.role))
          ) ++ immediateTarget.map(target =>
            part(
              target.square,
              if target.role.name.equalsIgnoreCase(King.name) then RelationParticipantRole.King
              else if target.side == owner then RelationParticipantRole.Supported
              else RelationParticipantRole.Target,
              Some(target.role)
            )
          )
        case GeometricSupportCausalTransition(
              mover,
              _,
              supportedBefore,
              supportedBeforeRole,
              supportedAfter,
              supportedAfterRole,
              beforeSupporters,
              afterSupporters,
              removals,
              establishments,
              _
            ) =>
          List(
            part(mover.from, RelationParticipantRole.Mover, Some(mover.beforeRole)),
            part(mover.to, RelationParticipantRole.Mover, Some(mover.afterRole)),
            part(supportedBefore, RelationParticipantRole.Supported, Some(supportedBeforeRole)),
            part(supportedAfter, RelationParticipantRole.Supported, Some(supportedAfterRole))
          ) ++ (beforeSupporters ++ afterSupporters ++ establishments.map(_.supporter)).map(supporter =>
            part(supporter.square, RelationParticipantRole.Controller, Some(supporter.role))
          ) ++ removals.map(removal =>
            part(
              removal.supporter.square,
              RelationParticipantRole.Controller,
              Some(removal.supporter.role)
            )
          )
        case CaptureRecaptureInventory(mover, captured, geometricRecapturers, legalRecaptures, _) =>
          List(
            part(mover.from, RelationParticipantRole.Mover, Some(mover.beforeRole)),
            part(mover.to, RelationParticipantRole.Mover, Some(mover.afterRole)),
            part(captured.square, RelationParticipantRole.Target, Some(captured.role))
          ) ++ geometricRecapturers.map(value =>
            part(value.square, RelationParticipantRole.Controller, Some(value.role))
          ) ++ legalRecaptures.map(value =>
            part(value.movement.from, RelationParticipantRole.Controller, Some(value.movement.beforeRole))
          )
        case CreatedCheckResponseInventory(
              mover,
              _,
              kingSquare,
              checkers,
              responses,
              controlledKingDestinations,
              _,
              _
            ) =>
          List(
            part(mover.from, RelationParticipantRole.Mover, Some(mover.beforeRole)),
            part(mover.to, RelationParticipantRole.Mover, Some(mover.afterRole)),
            part(kingSquare, RelationParticipantRole.King, Some(EvidencePieceRole(King.name)))
          ) ++ checkers.map(value =>
            part(value.square, RelationParticipantRole.Attacker, Some(value.role))
          ) ++ responses.map(value =>
            part(value.resource.movement.from, RelationParticipantRole.Controller, Some(value.resource.movement.beforeRole))
          ) ++ controlledKingDestinations.flatMap(destination =>
            destination.controllers.map(controller =>
              part(controller.square, RelationParticipantRole.Controller, Some(controller.role))
            )
          )
        case RootCheckResponse(mover, _, kingSquare, checkers, response, _) =>
          List(
            part(mover.from, RelationParticipantRole.Mover, Some(mover.beforeRole)),
            part(mover.to, RelationParticipantRole.Mover, Some(mover.afterRole)),
            part(kingSquare, RelationParticipantRole.King, Some(EvidencePieceRole(King.name))),
            part(
              response.resource.movement.from,
              RelationParticipantRole.Controller,
              Some(response.resource.movement.beforeRole)
            )
          ) ++ checkers.map(value =>
            part(value.square, RelationParticipantRole.Attacker, Some(value.role))
          )
        case MovementAffordanceLegalRestriction(mover, _, restrictedPiece, _, _, unavailable, _) =>
          List(
            part(mover.from, RelationParticipantRole.Mover, Some(mover.beforeRole)),
            part(mover.to, RelationParticipantRole.Mover, Some(mover.afterRole)),
            part(restrictedPiece.square, RelationParticipantRole.Controller, Some(restrictedPiece.role))
          ) ++ unavailable.map(resource =>
            part(resource.destination, RelationParticipantRole.Target, resource.target.pieceRole)
          )
        case AbsolutePinMovementRestriction(mover, _, pinner, pinned, kingSquare, _, _, _, forbidden, _) =>
          List(
            part(mover.from, RelationParticipantRole.Mover, Some(mover.beforeRole)),
            part(mover.to, RelationParticipantRole.Mover, Some(mover.afterRole)),
            part(pinner.square, RelationParticipantRole.Attacker, Some(pinner.role)),
            part(pinned.square, RelationParticipantRole.Blocker, Some(pinned.role)),
            part(kingSquare, RelationParticipantRole.King, Some(EvidencePieceRole(King.name)))
          ) ++ forbidden.map(resource =>
            part(resource.destination, RelationParticipantRole.Target, resource.target.pieceRole)
          )
        case SliderReachDelta(mover, _, sliderBefore, sliderAfter, _, before, after, _) =>
          List(
            part(mover.from, RelationParticipantRole.Mover, Some(mover.beforeRole)),
            part(mover.to, RelationParticipantRole.Mover, Some(mover.afterRole))
          ) ++ sliderBefore.map(value =>
            part(value.square, RelationParticipantRole.Controller, Some(value.role))
          ) ++ sliderAfter.map(value =>
            part(value.square, RelationParticipantRole.Controller, Some(value.role))
          ) ++ (before.toList ++ after.toList).flatMap(reach =>
            reach.segment.map(control =>
              part(control.square, RelationParticipantRole.Target, control.target.pieceRole)
            )
          )
        case GeometricMultiTargetContact(mover, _, controllerBefore, controllerAfter, newlyEstablished, maintained, _) =>
          List(
            part(mover.from, RelationParticipantRole.Mover, Some(mover.beforeRole)),
            part(mover.to, RelationParticipantRole.Mover, Some(mover.afterRole)),
            part(controllerBefore.square, RelationParticipantRole.Controller, Some(controllerBefore.role)),
            part(controllerAfter.square, RelationParticipantRole.Controller, Some(controllerAfter.role))
          ) ++ (newlyEstablished ++ maintained).map(target =>
            part(target.square, RelationParticipantRole.Target, Some(target.role))
          )
        case GeometricEnemyContactWithoutFriendlySupport(mover, _, controllerBefore, controllerAfter, target, _) =>
          List(
            part(mover.from, RelationParticipantRole.Mover, Some(mover.beforeRole)),
            part(mover.to, RelationParticipantRole.Mover, Some(mover.afterRole)),
            part(controllerBefore.square, RelationParticipantRole.Controller, Some(controllerBefore.role)),
            part(controllerAfter.square, RelationParticipantRole.Controller, Some(controllerAfter.role)),
            part(target.square, RelationParticipantRole.Target, Some(target.role))
          )
        case SharedGeometricSupportOfEnemyControlledTargets(mover, _, _, sharedSupporter, targets, _) =>
          List(
            part(mover.from, RelationParticipantRole.Mover, Some(mover.beforeRole)),
            part(mover.to, RelationParticipantRole.Mover, Some(mover.afterRole)),
            part(sharedSupporter.square, RelationParticipantRole.Controller, Some(sharedSupporter.role))
          ) ++ targets.flatMap(target =>
            part(target.target.square, RelationParticipantRole.Supported, Some(target.target.role)) ::
              target.enemyControllers.map(value => part(value.square, RelationParticipantRole.Controller, Some(value.role))) ++
              target.friendlySupporters.map(value => part(value.square, RelationParticipantRole.Controller, Some(value.role)))
          )
        case PawnTopologyTransition(mover, before, after, _, _) =>
          val states = before.toList ++ after.toList
          List(
            part(mover.from, RelationParticipantRole.Mover, Some(mover.beforeRole)),
            part(mover.to, RelationParticipantRole.Mover, Some(mover.afterRole))
          ) ++ states.flatMap(state =>
            part(state.square, RelationParticipantRole.Beneficiary, Some(EvidencePieceRole("pawn"))) ::
              state.frontOccupant.map(value =>
                part(value.square, RelationParticipantRole.Blocker, Some(value.role))
              ).toList ++
              state.connections.map(value =>
                part(
                  value.peer,
                  value.direction match
                    case RelationPawnConnectionDirection.Supports => RelationParticipantRole.Supported
                    case RelationPawnConnectionDirection.SupportedBy => RelationParticipantRole.Controller
                    case RelationPawnConnectionDirection.Peer => RelationParticipantRole.Other,
                  Some(EvidencePieceRole("pawn"))
                )
              ) ++ state.enemyPawnContacts.map(value =>
                part(value, RelationParticipantRole.Target, Some(EvidencePieceRole("pawn")))
              )
          )
        case PawnOccupiedFilePartitionTransition(mover, _, before, after, _) =>
          val moverSquares = Set(mover.from, mover.to)
          List(
            part(mover.from, RelationParticipantRole.Mover, Some(mover.beforeRole)),
            part(mover.to, RelationParticipantRole.Mover, Some(mover.afterRole))
          ) ++ changedPawnOccupiedFileRuns(before, after).flatMap(_.pawns).filterNot(moverSquares).map(square =>
            part(square, RelationParticipantRole.Other, Some(EvidencePieceRole(Pawn.name)))
          )
        case MajorPiecePawnFileCorridorTransition(mover, _, _, sliderBefore, sliderAfter, _, _, _, _, _, _) =>
          List(
            part(mover.from, RelationParticipantRole.Mover, Some(mover.beforeRole)),
            part(mover.to, RelationParticipantRole.Mover, Some(mover.afterRole))
          ) ++ sliderBefore.map(piece =>
            part(piece.square, RelationParticipantRole.Beneficiary, Some(piece.role))
          ) ++ sliderAfter.map(piece =>
            part(piece.square, RelationParticipantRole.Beneficiary, Some(piece.role))
          )
        case CastlingRightRemoved(mover, _, _, _) =>
          List(
            part(mover.from, RelationParticipantRole.Mover, Some(mover.beforeRole)),
            part(mover.to, RelationParticipantRole.Mover, Some(mover.afterRole))
          )
        case StalemateTransition(mover, _, kingSquare, _) =>
          List(
            part(mover.from, RelationParticipantRole.Mover, Some(mover.beforeRole)),
            part(mover.to, RelationParticipantRole.Mover, Some(mover.afterRole)),
            part(kingSquare, RelationParticipantRole.King, Some(EvidencePieceRole(King.name)))
          )
        case PawnFileGroup(_, _, pawns) =>
          pawns.map(part(_, RelationParticipantRole.Other, Some(EvidencePieceRole("pawn"))))
        case PawnFrontOccupancy(_, pawnSquare, frontSquare, occupant) =>
          part(pawnSquare, RelationParticipantRole.Other, Some(EvidencePieceRole("pawn"))) ::
            occupant.map(value =>
              part(value.square, RelationParticipantRole.Blocker, Some(value.role))
            ).toList ++
            frontSquare.filterNot(square => occupant.exists(_.square == square))
              .map(part(_, RelationParticipantRole.Other)).toList
        case PawnAdvanceAffordance(_, pawnSquare, _, _) =>
          List(part(pawnSquare, RelationParticipantRole.Other, Some(EvidencePieceRole("pawn"))))
        case PawnPassage(_, pawnSquare, opposingPawnSquares) =>
          part(pawnSquare, RelationParticipantRole.Beneficiary, Some(EvidencePieceRole("pawn"))) ::
            opposingPawnSquares.map(part(_, RelationParticipantRole.Target, Some(EvidencePieceRole("pawn"))))
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
  case SliderLineInterruption
  case GeometricLineControlAfterBlockerRemoval
  case NamedRayTransition
  case GeometricSupportCausalTransition
  case CaptureRecaptureInventory
  case CreatedCheckResponseInventory
  case RootCheckResponse
  case MovementAffordanceLegalRestriction
  case AbsolutePinMovementRestriction
  case SliderReachDelta
  case GeometricMultiTargetContact
  case GeometricEnemyContactWithoutFriendlySupport
  case SharedGeometricSupportOfEnemyControlledTargets
  case PawnTopologyTransition
  case PawnOccupiedFilePartitionTransition
  case MajorPiecePawnFileCorridorTransition
  case CastlingRightRemoved
  case StalemateTransition
  case PawnFileGroup
  case PawnFrontOccupancy
  case PawnAdvanceAffordance
  case PawnPassage
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
      case SliderLineInterruption          => "slider_line_interruption"
      case GeometricLineControlAfterBlockerRemoval => "geometric_line_control_after_blocker_removal"
      case NamedRayTransition   => "named_ray_transition"
      case GeometricSupportCausalTransition => "geometric_support_causal_transition"
      case CaptureRecaptureInventory => "capture_recapture_inventory"
      case CreatedCheckResponseInventory => "created_check_response_inventory"
      case RootCheckResponse => "root_check_response"
      case MovementAffordanceLegalRestriction => "movement_affordance_legal_restriction"
      case AbsolutePinMovementRestriction => "absolute_pin_movement_restriction"
      case SliderReachDelta => "slider_reach_delta"
      case GeometricMultiTargetContact => "geometric_multi_target_contact"
      case GeometricEnemyContactWithoutFriendlySupport =>
        "geometric_enemy_contact_without_friendly_support"
      case SharedGeometricSupportOfEnemyControlledTargets => "shared_geometric_support_of_enemy_controlled_targets"
      case PawnTopologyTransition => "pawn_topology_transition"
      case PawnOccupiedFilePartitionTransition => "pawn_occupied_file_partition_transition"
      case MajorPiecePawnFileCorridorTransition => "major_piece_pawn_file_corridor_transition"
      case CastlingRightRemoved => "castling_right_removed"
      case StalemateTransition => "stalemate_transition"
      case PawnFileGroup         => "pawn_file_group"
      case PawnFrontOccupancy    => "pawn_front_occupancy"
      case PawnAdvanceAffordance => "pawn_advance_affordance"
      case PawnPassage           => "pawn_passage"
      case RayBarrier            => "ray_barrier"

sealed trait EvidencePayload

final case class PositionFeatureEvidence(features: PositionFeatures) extends EvidencePayload

enum StrategicMechanismKind:
  case PlanPressure
  case OpeningAlignment

enum StrategicMechanismSignalKind:
  case PlanPressure
  case OpeningApplicability

enum StrategicAxisKind:
  case PlanCoherence

enum StrategicAxisPolarity:
  case Gain
  case Concede
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
    hasResolvedPlanEvent
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
  def canAnchorOpeningClaim: Boolean =
    kind == StrategicMechanismKind.OpeningAlignment &&
      signalKinds.contains(StrategicMechanismSignalKind.OpeningApplicability)
  def canAnchorPlanClaim: Boolean =
    kind == StrategicMechanismKind.PlanPressure &&
      hasResolvedPlanEvent
  def canSupportStrategicCause: Boolean =
    canAnchorStrategicClaim
  def axisDetails: List[StrategicAxisDetail] =
    signals.flatMap(_.axis).distinctBy(_.stableKey)
  def hasStrategicAxis: Boolean =
    axisDetails.nonEmpty
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
      RelativeCauseKind.strategicAxisCanProveCause(kind, comparison.axis) &&
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
    planAxes

  def sourceSemanticAnchors(record: EvidenceRecord): List[EvidenceSemanticAnchor] =
    record.payload match
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
        payload.causalEpisode.root.certifiedLegalStep.nonEmpty &&
          (payload.structuralConsequences.exists(_.subjectFacts.nonEmpty) ||
            payload.episode.exists(_.planSequenceProven))
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
    rootBeforeRole: EvidencePieceRole,
    pieceRole: EvidencePieceRole,
    futureAfterRole: EvidencePieceRole,
    color: Color,
    rootFrom: EvidenceSquare,
    rootTo: EvidenceSquare,
    futureFrom: EvidenceSquare,
    futureTo: EvidenceSquare,
    plyOffset: Int
)

object LineObjectTrajectory:
  private[chessjudgment] def findAll(
      rootStep: LineReplayStep,
      continuation: List[LineReplayStep],
      replay: CanonicalLineReplay
  ): List[LineObjectTrajectory] =
    val exactContinuation =
      replay.replaySteps.indexOf(rootStep) match
        case rootIndex if rootIndex >= 0 &&
            replay.replaySteps.slice(rootIndex + 1, rootIndex + 1 + continuation.size) == continuation =>
          continuation
        case _ => Nil
    replay.transition(rootStep).toList.flatMap { rootTransition =>
      rootTransition.boardFootprint.pieceTransitions.flatMap { rootMovement =>
        firstFutureMovement(rootMovement, exactContinuation, replay).map { case (futureStep, futureMovement, offset) =>
          LineObjectTrajectory(
            rootStep = rootStep,
            futureStep = futureStep,
            rootBeforeRole = EvidencePieceRole(rootMovement.beforeRole.name),
            pieceRole = EvidencePieceRole(rootMovement.afterRole.name),
            futureAfterRole = EvidencePieceRole(futureMovement.afterRole.name),
            color = rootMovement.side,
            rootFrom = EvidenceSquare(rootMovement.from.key),
            rootTo = EvidenceSquare(rootMovement.to.key),
            futureFrom = EvidenceSquare(futureMovement.from.key),
            futureTo = EvidenceSquare(futureMovement.to.key),
            plyOffset = offset
          )
        }
      }
    }.sortBy(trajectory =>
      (
        trajectory.rootFrom.key,
        trajectory.rootTo.key,
        trajectory.futureStep.ply,
        trajectory.futureFrom.key,
        trajectory.futureTo.key
      )
    )

  private def firstFutureMovement(
      tracked: BoardPieceTransition,
      continuation: List[LineReplayStep],
      replay: CanonicalLineReplay
  ): Option[(LineReplayStep, BoardPieceTransition, Int)] =
    def loop(remaining: List[LineReplayStep], offset: Int): Option[(LineReplayStep, BoardPieceTransition, Int)] =
      remaining match
        case Nil => None
        case step :: tail =>
          replay.transition(step).flatMap { transition =>
            transition.boardFootprint.pieceTransitions.filter(movement =>
              movement.side == tracked.side && movement.beforeRole == tracked.afterRole &&
                movement.from == tracked.to
            ) match
              case exact :: Nil => Some((step, exact, offset))
              case Nil if !transition.boardFootprint.changedSquareSet(tracked.to) =>
                loop(tail, offset + 1)
              case _ => None
          }
    loop(continuation, 1)

final case class LineAccessTrajectory private (
    enablingStep: LineReplayStep,
    enabledStep: LineReplayStep,
    interveningSteps: List[LineReplayStep],
    enabledPieceRole: EvidencePieceRole,
    color: Color,
    vacatedSquares: List[EvidenceSquare],
    enabledFrom: EvidenceSquare,
    enabledTo: EvidenceSquare,
    plyOffset: Int,
    private[chessjudgment] val accessRelationKey: DerivedRelationResultKey
):
  require(vacatedSquares.nonEmpty, "a line-access trajectory needs an exact vacated gate")
  def vacatedSquare: EvidenceSquare = vacatedSquares.head

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
    val candidates =
      for
        enablingIndex <- replay.replaySteps.indexOf(enablingStep) :: Nil
        enabledIndex <- replay.replaySteps.indexOf(enabledStep) :: Nil
        if enablingIndex >= 0 && enabledIndex > enablingIndex
        if replay.replaySteps.slice(enablingIndex + 1, enabledIndex) == interveningSteps
        enablingTransition <- replay.transition(enablingStep).toList
        enabledLegal <- replay.legalStep(enabledStep).toList
        enabledPiece = RelationPieceWitness(
          EvidenceSquare(enabledLegal.move.orig.key),
          EvidencePieceRole(enabledLegal.move.piece.role.name)
        )
        occurrence <- replay.verticalRelationOccurrences(
          enablingStep,
          List(VerticalRelationContractKind.SliderReachDelta)
        )
        detail <- occurrence.relation.detail match
          case exact: RelationWitnessDetail.SliderReachDelta => List(exact)
          case _ => Nil
        if detail.side == enabledLegal.move.piece.color && detail.mover.side == detail.side
        if detail.sliderBefore.contains(enabledPiece) && detail.sliderAfter.contains(enabledPiece)
        afterReach <- detail.after.toList
        destination = EvidenceSquare(enabledLegal.move.dest.key)
        destinationIndex = afterReach.segment.indexWhere(_.square == destination)
        if destinationIndex >= 0 && movementAvailable(afterReach.segment(destinationIndex).target)
        if !detail.before.exists(_.segment.exists(reach =>
          reach.square == destination && movementAvailable(reach.target)
        ))
        openedSegment = afterReach.segment.take(destinationIndex + 1).map(_.square)
        vacatedSet = enablingTransition.boardFootprint.cellChanges.collect {
          case change
              if change.before.nonEmpty && change.after.isEmpty &&
                openedSegment.contains(EvidenceSquare(change.square.key)) =>
            EvidenceSquare(change.square.key)
        }.toSet
        vacated = openedSegment.filter(vacatedSet)
        if vacated.nonEmpty
        if interveningSteps.forall(step =>
          replay.analysisAfter(step).exists(analysis =>
            accessAvailable(
              analysis.relationInventory,
              detail.side,
              enabledPiece,
              destination
            )
          )
        )
      yield LineAccessTrajectory(
        enablingStep = enablingStep,
        enabledStep = enabledStep,
        interveningSteps = interveningSteps,
        enabledPieceRole = enabledPiece.role,
        color = detail.side,
        vacatedSquares = vacated,
        enabledFrom = enabledPiece.square,
        enabledTo = destination,
        plyOffset = enabledIndex - enablingIndex,
        accessRelationKey = DerivedRelationResultKey.from(occurrence.relation)
      )

    candidates match
      case exact :: Nil => Some(exact)
      case _            => None

  private def movementAvailable(target: RelationControlTarget): Boolean =
    target match
      case RelationControlTarget.Friendly(_) => false
      case RelationControlTarget.Empty | RelationControlTarget.Enemy(_) => true

  private def accessAvailable(
      inventory: PositionRelationExtractor.PositionRelationInventoryCertificate,
      side: Color,
      piece: RelationPieceWitness,
      destination: EvidenceSquare
  ): Boolean =
    inventory.occupantAt(piece.square).exists(occupant =>
      occupant.side == side && occupant.role == piece.role
    ) && inventory.controlsFrom(piece.square).exists(control =>
      control.side == side && control.controller == piece &&
        control.targetSquare == destination && movementAvailable(control.target)
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
    for
      _ <- Option.when(List(breakStep, replyStep, followUpStep).forall(_.ply > 0))(())
      breakLegal <- replay.legalStep(breakStep)
      replyLegal <- replay.legalStep(replyStep)
      followUpLegal <- replay.legalStep(followUpStep)
      if breakLegal.move.piece.role == Pawn
      if replyLegal.move.piece.role == Pawn && replyLegal.move.piece.color == !breakLegal.move.piece.color
      if replyLegal.move.dest == breakLegal.move.dest &&
        replyLegal.move.capture.contains(breakLegal.move.dest) && replyLegal.capturedRole.contains(Pawn)
      if followUpLegal.move.piece.role == Pawn &&
        followUpLegal.move.piece.color == breakLegal.move.piece.color
      afterReplyAnalysis <- replay.analysisAfter(replyStep)
      if interveningSteps.headOption.contains(replyStep)
      if followUpStep.ply - breakStep.ply == interveningSteps.size + 1
      releaseChange <- replay.transition(replyStep).toList
        .flatMap(_.relationDelta.established)
        .find(_.detail match
          case RelationWitnessDetail.PawnPassage(owner, pawn, opposingPawns) =>
            owner == breakLegal.move.piece.color &&
              pawn.key.equalsIgnoreCase(followUpLegal.move.orig.key) &&
              opposingPawns.isEmpty
          case _ => false
        )
      releaseWitness <- ReplayRelationChangeWitness.certify(replay, replyStep, releaseChange)
      laterPassageAnalyses = interveningSteps.drop(1).flatMap(replay.analysisAfter)
      if laterPassageAnalyses.size == interveningSteps.drop(1).size
      if (afterReplyAnalysis :: laterPassageAnalyses).forall(analysis =>
        analysis.relationInventory.relationBySemanticId(releaseChange.semanticId).exists(relation =>
          relation.kind == releaseChange.kind && relation.detail == releaseChange.detail
        )
      )
    yield PawnBreakFollowUpTrajectory(
      breakStep = breakStep,
      replyStep = replyStep,
      followUpStep = followUpStep,
      interveningSteps = interveningSteps,
      kind = PawnBreakFollowUpKind.ReleasedPassedPawn,
      color = breakLegal.move.piece.color,
      replyFrom = EvidenceSquare(replyLegal.move.orig.key),
      replyTo = EvidenceSquare(replyLegal.move.dest.key),
      followUpFrom = EvidenceSquare(followUpLegal.move.orig.key),
      followUpTo = EvidenceSquare(followUpLegal.move.dest.key),
      releasedPassedPawn = EvidenceSquare(followUpLegal.move.orig.key),
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
    plyOffset: Int,
    private[chessjudgment] val recaptureInventoryKey: DerivedRelationResultKey
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
    val candidates = for
      triggerLegal <- replay.legalStep(triggerStep).toList
      replyLegal <- replay.legalStep(replyStep).toList
      followUpLegal <- replay.legalStep(followUpStep).toList
      if interveningSteps == List(replyStep) && followUpStep.ply - triggerStep.ply == 2
      if replyLegal.move.piece.color == !triggerLegal.move.piece.color
      if followUpLegal.move.piece.color == triggerLegal.move.piece.color
      occurrence <- replay.verticalRelationOccurrences(
        replyStep,
        List(VerticalRelationContractKind.CaptureRecaptureInventory)
      )
      detail <- occurrence.relation.detail match
        case exact: RelationWitnessDetail.CaptureRecaptureInventory => List(exact)
        case _ => Nil
      if legalMovementMatches(detail.mover, replyLegal)
      if detail.captured.side == triggerLegal.move.piece.color &&
        detail.captured.role.name.equalsIgnoreCase(triggerLegal.move.piece.role.name) &&
        detail.captured.square.key.equalsIgnoreCase(triggerLegal.move.dest.key)
      recapture <- detail.legalRecaptures.filter(resource =>
        EvidenceRef.sameMove(resource.moveUci, followUpStep.moveUci) &&
          legalMovementMatches(resource.movement, followUpLegal) &&
          resource.capture.exists(capture =>
            capture.capturedSide == replyLegal.move.piece.color &&
              capture.capturedRole.name.equalsIgnoreCase(replyLegal.move.piece.role.name) &&
              capture.capturedSquare.key.equalsIgnoreCase(replyLegal.move.dest.key)
          )
      )
    yield CaptureResponseFollowUpTrajectory(
      triggerStep = triggerStep,
      replyStep = replyStep,
      followUpStep = followUpStep,
      interveningSteps = interveningSteps,
      triggerRole = EvidencePieceRole(triggerLegal.move.piece.role.name),
      responderRole = EvidencePieceRole(replyLegal.move.piece.role.name),
      followUpRole = EvidencePieceRole(followUpLegal.move.piece.role.name),
      replyFrom = EvidenceSquare(replyLegal.move.orig.key),
      replyTo = EvidenceSquare(replyLegal.move.dest.key),
      followUpFrom = EvidenceSquare(followUpLegal.move.orig.key),
      followUpTo = EvidenceSquare(followUpLegal.move.dest.key),
      plyOffset = 2,
      recaptureInventoryKey = DerivedRelationResultKey.from(occurrence.relation)
    )
    candidates match
      case exact :: Nil => Some(exact)
      case _            => None

  private[chessjudgment] def legalMovementMatches(
      movement: RelationMoveTransitionWitness,
      legal: LegalReplayStep
  ): Boolean =
    movement.side == legal.move.piece.color &&
      movement.from.key.equalsIgnoreCase(legal.move.orig.key) &&
      movement.to.key.equalsIgnoreCase(legal.move.dest.key) &&
      movement.beforeRole.name.equalsIgnoreCase(legal.move.piece.role.name)

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
    plyOffset: Int,
    private[chessjudgment] val checkInventoryKey: DerivedRelationResultKey
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
    val candidates = for
      triggerLegal <- replay.legalStep(triggerStep).toList
      replyLegal <- replay.legalStep(replyStep).toList
      followUpLegal <- replay.legalStep(followUpStep).toList
      if interveningSteps == List(replyStep) && followUpStep.ply - triggerStep.ply == 2
      if replyLegal.move.piece.color == !triggerLegal.move.piece.color
      if followUpLegal.move.piece.color == triggerLegal.move.piece.color
      occurrence <- replay.verticalRelationOccurrences(
        triggerStep,
        List(VerticalRelationContractKind.CreatedCheckResponseInventory)
      )
      detail <- occurrence.relation.detail match
        case exact: RelationWitnessDetail.CreatedCheckResponseInventory => List(exact)
        case _ => Nil
      if CaptureResponseFollowUpTrajectory.legalMovementMatches(detail.mover, triggerLegal)
      if detail.checkedSide == replyLegal.move.piece.color &&
        detail.terminal == RelationCheckTerminalState.Ongoing
      response <- detail.responses.filter(resource =>
        EvidenceRef.sameMove(resource.resource.moveUci, replyStep.moveUci) &&
          CaptureResponseFollowUpTrajectory.legalMovementMatches(resource.resource.movement, replyLegal)
      )
    yield CheckResponseFollowUpTrajectory(
      triggerStep = triggerStep,
      replyStep = replyStep,
      followUpStep = followUpStep,
      interveningSteps = interveningSteps,
      triggerRole = EvidencePieceRole(triggerLegal.move.piece.role.name),
      responderRole = EvidencePieceRole(replyLegal.move.piece.role.name),
      followUpRole = EvidencePieceRole(followUpLegal.move.piece.role.name),
      replyFrom = EvidenceSquare(replyLegal.move.orig.key),
      replyTo = EvidenceSquare(replyLegal.move.dest.key),
      followUpFrom = EvidenceSquare(followUpLegal.move.orig.key),
      followUpTo = EvidenceSquare(followUpLegal.move.dest.key),
      plyOffset = 2,
      checkInventoryKey = DerivedRelationResultKey.from(occurrence.relation)
    )
    candidates match
      case exact :: Nil => Some(exact)
      case _            => None

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
    replay.legalStep(trajectory.triggerStep).exists(trigger =>
      LineObjectTrajectory
        .findAll(
          trajectory.triggerStep,
          trajectory.interveningSteps :+ trajectory.followUpStep,
          replay
        )
        .exists(route =>
          route.futureStep == trajectory.followUpStep &&
            route.rootFrom.key.equalsIgnoreCase(trigger.move.orig.key)
        )
    )

enum LineEventKind:
  case Capture
  case Recapture
  case CheckEvasion
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

enum LineMaterialRecaptureStatus:
  case Proven(inventory: DerivedRelationResultKey)
  case Excluded
  case Unknown

final case class LineMaterialCapture private[chessjudgment] (
    moveUci: String,
    plyOffset: Int,
    side: Color,
    attackerRole: EvidencePieceRole,
    capturedRole: EvidencePieceRole,
    square: EvidenceSquare,
    valueCp: Int,
    recaptureStatus: LineMaterialRecaptureStatus
):
  def recapture: Boolean = recaptureStatus.isInstanceOf[LineMaterialRecaptureStatus.Proven]
  private[chessjudgment] def recaptureExcluded: Boolean =
    recaptureStatus == LineMaterialRecaptureStatus.Excluded

private[chessjudgment] final case class ObservedLineMaterialEvent(
    moveUci: String,
    plyOffset: Int,
    movement: RelationMoveTransitionWitness,
    legalMoveSemanticId: String,
    capture: Option[LineMaterialCapture],
    promotionGainCp: Int
):
  require(plyOffset >= 0, "an observed material event needs a root-relative ply")
  require(legalMoveSemanticId.matches("[0-9a-f]{64}"), "a material event needs its canonical legal-move fact")
  require(
    EvidenceRef.sameMove(
      moveUci,
      s"${movement.from.key}${movement.to.key}${EvidencePieceRole.promotionSuffix(movement.beforeRole, movement.afterRole)}"
    ),
    "an observed material event must retain its canonical movement"
  )
  require(
    capture.forall(value =>
      EvidenceRef.sameMove(value.moveUci, moveUci) && value.plyOffset == plyOffset &&
        value.side == movement.side && value.attackerRole == movement.beforeRole
    ),
    "an observed capture must belong to the same canonical move occurrence"
  )
  require(promotionGainCp >= 0, "a promotion material delta cannot be negative")

enum ClosedLineMaterialTerminal:
  case Checkmate
  case Stalemate

final case class ClosedLineMaterialOutcome(
    terminal: ClosedLineMaterialTerminal,
    terminalPlyOffset: Int
):
  require(terminalPlyOffset >= 0, "a closed material outcome needs an exact terminal ply")

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
      piece: RelationColoredPieceWitness,
      offer: Option[LineSacrificeOffer]
  )

  private[chessjudgment] def acceptanceKey(
      occurrence: LineSacrificeOccurrence
  ): (Int, String) =
    LineMaterialSummary.captureOccurrenceKey(occurrence.acceptance)

  /** Builds an occurrence by following the acceptance square through the
    * canonical move footprints. Every legal move is interpreted once by the
    * replay owner; this consumer never reopens its boards.
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
      acceptanceTransition <- replay.transition(acceptanceReplay)
      if EvidenceRef.sameMove(acceptanceReplay.moveUci, acceptance.moveUci)
      if EvidenceRef.sameMove(acceptanceStep.uci, acceptance.moveUci)
      if acceptanceTransition.relationDelta.rootMove.capture.exists(capture =>
        capture.capturedSquare == acceptance.square &&
          capture.capturedRole == acceptance.capturedRole
      )
      if acceptance.side == acceptanceStep.move.piece.color
      if acceptanceStep.move.piece.role.toString.equalsIgnoreCase(acceptance.attackerRole.name)
      if acceptance.side != offeredSide
      initialInventory <- replay.replaySteps.headOption.flatMap(replay.analysisBefore).map(_.relationInventory)
      initial = initialInventory.occupantAt(EvidenceSquare(square.key)).map(piece => TrackedOffer(piece, None))
      tracked = replay.replaySteps
        .take(acceptance.plyOffset)
        .zipWithIndex
        .foldLeft(initial) { case (current, (declared, plyOffset)) =>
          replay.transition(declared) match
            case None => None
            case Some(transition) =>
              transition.boardFootprint.pieceTransitions.find(_.to == square) match
                case Some(arrival) =>
                  Some(TrackedOffer(
                    RelationColoredPieceWitness(
                      square = EvidenceSquare(arrival.to.key),
                      role = EvidencePieceRole(arrival.afterRole.name),
                      side = arrival.side
                    ),
                    Some(LineSacrificeOffer(EvidenceRef.normalizeMove(declared.moveUci), plyOffset))
                  ))
                case None if transition.boardFootprint.changedSquareSet(square) => None
                case None => current
        }
      offered <- tracked
      if offered.piece.side == offeredSide
      if offered.piece.role == acceptance.capturedRole
      if acceptanceStep.capturedRole.exists(role => offered.piece.role.name.equalsIgnoreCase(role.name))
      if acceptanceTransition.boardFootprint.cellChanges.exists(change =>
        change.square == square &&
          change.before.exists(piece =>
            piece.color == offered.piece.side && piece.role.name.equalsIgnoreCase(offered.piece.role.name)
          ) &&
          !change.after.exists(_.color == offered.piece.side)
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


final case class LineMaterialSummary private[chessjudgment] (
    sideToMove: Color,
    events: List[ObservedLineMaterialEvent],
    closedOutcome: Option[ClosedLineMaterialOutcome]
):
  require(events.nonEmpty, "a material summary needs at least one observed material event")
  require(
    events.map(_.plyOffset) == events.map(_.plyOffset).distinct.sorted,
    "material events must be unique and ordered by replay ply"
  )
  require(
    closedOutcome.forall(_.terminalPlyOffset >= events.last.plyOffset),
    "a closed material terminal cannot precede an observed material event"
  )

  val captures: List[LineMaterialCapture] = events.flatMap(_.capture)

  private lazy val exactCaptureInventory: Option[List[LineMaterialCapture]] =
    LineMaterialSummary.exactCaptureInventory(captures)

  private lazy val signedDeltasForMover: List[Int] =
    events.map { event =>
      val captureDelta = event.capture.map(_.valueCp).getOrElse(0)
      val materialDelta = captureDelta + event.promotionGainCp
      if event.movement.side == sideToMove then materialDelta else -materialDelta
    }

  private lazy val runningBalancesForMover: List[Int] =
    signedDeltasForMover.scanLeft(0)(_ + _).tail

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

  def observedNetCpForMover: Int = runningBalancesForMover.lastOption.getOrElse(0)
  def observedMaxGainCpForMover: Int = (0 :: runningBalancesForMover).max
  def observedMaxLossCpForMover: Int = (0 :: runningBalancesForMover).min
  def closedNetCpForMover: Option[Int] = closedOutcome.map(_ => observedNetCpForMover)
  def isClosed: Boolean = closedOutcome.nonEmpty

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

  def promotionGainCpForMover: Int =
    events.map(event =>
      if event.movement.side == sideToMove then event.promotionGainCp else -event.promotionGainCp
    ).sum

  def hasPromotionGainForMover: Boolean = promotionGainCpForMover > 0
  def hasPromotionLossForMover: Boolean = promotionGainCpForMover < 0
  def hasRecaptureChain: Boolean = captures.exists(_.recapture)

  def hasObservedRecovery: Boolean =
    runningBalancesForMover.zipWithIndex.exists { case (balance, index) =>
      balance >= 0 && runningBalancesForMover.take(index).exists(_ < 0)
    }

  def hasClosedRecovery: Boolean =
    isClosed && hasObservedRecovery && observedNetCpForMover >= 0

  def hasResolvedMaterialSequence: Boolean =
    hasRecaptureChain || hasObservedRecovery

  private[chessjudgment] def durableRecoveryCaptureForMover: Option[LineMaterialCapture] =
    Option.when(hasClosedRecovery)(
      events.zip(runningBalancesForMover).zipWithIndex.collectFirst {
        case ((event, balance), index)
            if event.movement.side == sideToMove &&
              event.capture.nonEmpty &&
              runningBalancesForMover.take(index).exists(_ < 0) &&
              balance >= 0 &&
              runningBalancesForMover.drop(index).forall(_ >= 0) =>
          event.capture.get
      }
    ).flatten

  def hasProofSignalMaterialGain: Boolean =
    closedNetCpForMover.exists(_ > 0) && (nonPawnCapturesByMover.nonEmpty || hasPromotionGainForMover)

  def hasProofSignalMaterialLoss: Boolean =
    closedNetCpForMover.exists(_ < 0) && (nonPawnCapturesByOpponent.nonEmpty || hasPromotionLossForMover)

  def hasUnrecoveredPawnGainForMover: Boolean =
    closedNetCpForMover.exists(_ > 0) && pawnCapturesByMover.nonEmpty

  def hasUnrecoveredPawnLossForMover: Boolean =
    closedNetCpForMover.exists(_ < 0) && pawnCapturesByOpponent.nonEmpty

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
    offer.recaptureExcluded &&
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
      val initial = Option(rootStep.move.orig -> rootStep.move.piece.role)
      replay.replaySteps.take(plies.max(1)).foldLeft(initial) { (tracked, step) =>
        tracked.flatMap { case (square, role) =>
          replay.transition(step).flatMap { transition =>
            transition.boardFootprint.pieceTransitions.find(movement =>
              movement.side == actorColor && movement.from == square && movement.beforeRole == role
            ) match
              case Some(movement) => Some(movement.to -> movement.afterRole)
              case None if transition.boardFootprint.changedSquareSet(square) => None
              case None => Some(square -> role)
          }
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
  def materialClosedNetCpForMover: Option[Int] =
    material.flatMap(_.closedNetCpForMover)
  def hasMaterialRecaptureChain: Boolean =
    material.exists(_.hasRecaptureChain)
  def hasClosedMaterialRecovery: Boolean =
    material.exists(_.hasClosedRecovery)
  def hasClosedMaterialOutcome: Boolean =
    material.exists(_.isClosed)
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
            Option.when(summary.closedNetCpForMover.exists(_ > 0) && summary.nonPawnCapturesByMover.nonEmpty)(LineMaterialOutcomeSignal.MoverCapture),
            Option.when(summary.closedNetCpForMover.exists(_ > 0) && summary.hasPromotionGainForMover)(LineMaterialOutcomeSignal.PromotionGain),
            Option.when(summary.hasUnrecoveredPawnGainForMover)(LineMaterialOutcomeSignal.UnrecoveredPawnGain),
            Option.when(summary.hasClosedRecovery)(LineMaterialOutcomeSignal.RecoveryWindow)
          ).flatten
        )
        .getOrElse(Set.empty)
    val materialLossSignals =
      material
        .map(summary =>
          Set(
            Option.when(summary.closedNetCpForMover.exists(_ < 0) && summary.nonPawnCapturesByOpponent.nonEmpty)(LineMaterialOutcomeSignal.OpponentCapture),
            Option.when(summary.closedNetCpForMover.exists(_ < 0) && summary.hasPromotionLossForMover)(LineMaterialOutcomeSignal.PromotionLoss),
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
        actor <- line.certifiedRootActor(normalizedRoot)
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
        verifiedKingTarget(eventStep, replay, VerifiedKingState.InCheck)
      case LineConsequenceKind.Mate =>
        verifiedKingTarget(eventStep, replay, VerifiedKingState.Checkmate)
      case LineConsequenceKind.DrawResource =>
        verifiedKingTarget(eventStep, replay, VerifiedKingState.Stalemate)
      case LineConsequenceKind.Promotion | LineConsequenceKind.PromotionRace =>
        replay.transition(eventStep).flatMap { transition =>
          val movement = transition.relationDelta.rootMove
          Option.when(
            movement.beforeRole.name.equalsIgnoreCase(Pawn.toString) &&
              !movement.afterRole.name.equalsIgnoreCase(Pawn.toString)
          )(movement.to)
        }
      case LineConsequenceKind.Sacrifice =>
        consequence.sacrificeOccurrence.map(_.target)
      case LineConsequenceKind.ForcedTheme =>
        None

  private enum VerifiedKingState:
    case InCheck
    case Checkmate
    case Stalemate

  private def verifiedKingTarget(
      step: LineReplayStep,
      replay: CanonicalLineReplay,
      expected: VerifiedKingState
  ): Option[EvidenceSquare] =
    replay.analysisAfter(step).flatMap { analysis =>
      val inventory = analysis.relationInventory
      val state = inventory.stateView
      val matches = expected match
        case VerifiedKingState.InCheck => state.inCheck(state.sideToMove)
        case VerifiedKingState.Checkmate =>
          inventory.kingTerminalState == PositionRelationExtractor.ClosedKingTerminalState.Checkmate
        case VerifiedKingState.Stalemate =>
          inventory.kingTerminalState == PositionRelationExtractor.ClosedKingTerminalState.Stalemate
      Option.when(matches)(state.kingSquare(state.sideToMove)).flatten
    }

  private def actualConsequenceAt(
      line: LineFactEvidence,
      consequence: LineConsequence,
      eventStep: LineReplayStep,
      eventPlyOffset: Int,
      rootColor: Color,
      replay: CanonicalLineReplay
  ): Boolean =
    val move = EvidenceRef.normalizeMove(eventStep.moveUci)
    val eventTransition = replay.transition(eventStep)
    val mover = eventTransition.map(_.legal.move.piece)
    val afterInventory = eventTransition.map(_.afterAnalysis.relationInventory)
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
        eventPlyOffset == 1 && mover.exists(_.color != rootColor) && afterInventory.exists { inventory =>
          val state = inventory.stateView
          state.inCheck(state.sideToMove)
        }
      case LineConsequenceKind.Mate =>
        mover.exists(piece => consequence.beneficiary.contains(piece.color)) && afterInventory.exists(
          _.kingTerminalState == PositionRelationExtractor.ClosedKingTerminalState.Checkmate
        )
      case LineConsequenceKind.DrawResource =>
        afterInventory.exists(
          _.kingTerminalState == PositionRelationExtractor.ClosedKingTerminalState.Stalemate
        )
      case LineConsequenceKind.Promotion | LineConsequenceKind.PromotionRace =>
        eventTransition.exists(transition =>
          consequence.beneficiary.contains(transition.legal.move.piece.color) &&
            transition.boardFootprint.pieceTransitions.exists(movement =>
              movement.side == transition.legal.move.piece.color &&
                movement.from == transition.legal.move.orig &&
                movement.to == transition.legal.move.dest &&
                movement.beforeRole == Pawn && movement.afterRole != Pawn
            )
        )
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
    replay.transition(eventStep).flatMap { eventTransition =>
      val movement = eventTransition.relationDelta.rootMove
      val eventAnchor = movement.to
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
            movement.side == actor.color &&
              movement.from == tracked.square &&
              movement.beforeRole.name.equalsIgnoreCase(tracked.role.name)
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
    }

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
    var paths = Map.empty[Int, List[List[RootCausalLink]]]
    capturePlies.foreach { toPly =>
      val directPaths = rootMaterialSeedLinks(line, rootStep, actor, toPly, replay).map(List(_))
      val continuedPaths = paths.toList
        .filter(_._1 < toPly)
        .sortBy(_._1)
        .flatMap { case (fromPly, fromPaths) =>
          materialContinuationLinks(line, capturesByPly(fromPly), capturesByPly(toPly), replay)
            .flatMap(link => fromPaths.map(_ :+ link))
        }
      val exactPaths = (directPaths ++ continuedPaths).distinct
      if exactPaths.nonEmpty then paths = paths.updated(toPly, exactPaths)
    }
    paths.getOrElse(eventPlyOffset, Nil).flatten.distinct

  private def rootMaterialSeedLinks(
      line: LineFactEvidence,
      rootStep: LineReplayStep,
      actor: RootCausalActor,
      eventPlyOffset: Int,
      replay: CanonicalLineReplay
  ): List[RootCausalLink] =
    line.lineReplaySteps.lift(eventPlyOffset).toList.flatMap { eventStep =>
      val rootMove = EvidenceRef.normalizeMove(rootStep.moveUci)
      val eventMove = EvidenceRef.normalizeMove(eventStep.moveUci)
      replay.transition(eventStep).toList.flatMap { transition =>
        val movement = transition.relationDelta.rootMove
        val actorAction =
          trackedActorBefore(line, actor, eventPlyOffset, replay)
            .filter(tracked =>
              movement.side == actor.color &&
                movement.from == tracked.square &&
                movement.beforeRole.name.equalsIgnoreCase(tracked.role.name)
            )
            .map(_ => RootCausalLink(
              RootCausalLinkKind.RootActorContinuation,
              rootMove,
              eventMove,
              movement.to
            ))
        List(
          forcedCaptureResponseLink(line, rootStep, eventStep, eventPlyOffset, replay),
          forcedCheckResponseLink(line, rootStep, eventStep, eventPlyOffset, replay),
          rootActorCapturedSeedLink(line, actor, eventPlyOffset, replay),
          continuousLineAccessSeedLink(line, eventPlyOffset, replay),
          actorAction
        ).flatten.distinct
      }
    }

  private def materialContinuationLinks(
      line: LineFactEvidence,
      fromCapture: LineMaterialCapture,
      toCapture: LineMaterialCapture,
      replay: CanonicalLineReplay
  ): List[RootCausalLink] =
    (for
      fromStep <- line.lineReplaySteps.lift(fromCapture.plyOffset)
      toStep <- line.lineReplaySteps.lift(toCapture.plyOffset)
      if toCapture.plyOffset > fromCapture.plyOffset
    yield
      val continuation = line.lineReplaySteps.slice(fromCapture.plyOffset + 1, toCapture.plyOffset + 1)
      val actorContinuation = LineObjectTrajectory
          .findAll(fromStep, continuation, replay)
          .find(_.futureStep == toStep)
          .map(trajectory => RootCausalLink(
            RootCausalLinkKind.MaterialActorContinuation,
            fromStep.moveUci,
            toStep.moveUci,
            trajectory.rootTo
          ))
      val actorCaptured = materialActorCaptured(line, fromCapture, toCapture, replay).map(anchor => RootCausalLink(
        RootCausalLinkKind.MaterialCaptureResponse,
        fromStep.moveUci,
        toStep.moveUci,
        anchor
      ))
      List(actorContinuation, actorCaptured).flatten
    ).toList.flatten.distinct

  private def materialActorCaptured(
      line: LineFactEvidence,
      fromCapture: LineMaterialCapture,
      toCapture: LineMaterialCapture,
      replay: CanonicalLineReplay
  ): Option[EvidenceSquare] =
    for
      fromStep <- line.lineReplaySteps.lift(fromCapture.plyOffset)
      toStep <- line.lineReplaySteps.lift(toCapture.plyOffset)
      fromTransition <- replay.transition(fromStep)
      toTransition <- replay.transition(toStep)
      actorMovement <- fromTransition.boardFootprint.pieceTransitions.find(movement =>
        movement.side == fromCapture.side &&
          movement.beforeRole.name.equalsIgnoreCase(fromCapture.attackerRole.name) &&
          movement.to.key.equalsIgnoreCase(fromCapture.square.key)
      )
      tracked = line.lineReplaySteps
        .slice(fromCapture.plyOffset + 1, toCapture.plyOffset)
        .foldLeft(Option(actorMovement.to -> actorMovement.afterRole)) { (current, step) =>
          current.flatMap { case (square, role) =>
            replay.transition(step).flatMap { transition =>
              transition.boardFootprint.pieceTransitions.find(movement =>
                movement.side == fromCapture.side && movement.from == square && movement.beforeRole == role
              ) match
                case Some(_) => None
                case None if transition.boardFootprint.changedSquareSet(square) => None
                case None => Some(square -> role)
            }
          }
        }
      (square, role) <- tracked
      if toCapture.side != fromCapture.side &&
        toCapture.square.key.equalsIgnoreCase(square.key) &&
        toCapture.capturedRole.name.equalsIgnoreCase(role.name) &&
        toTransition.legal.capturedRole.contains(role)
      if toTransition.boardFootprint.cellChanges.exists(change =>
        change.square == square &&
          change.before.contains(Piece(fromCapture.side, role)) &&
          !change.after.contains(Piece(fromCapture.side, role))
      )
    yield EvidenceSquare(square.key)

  private def trackedActorBefore(
      line: LineFactEvidence,
      actor: RootCausalActor,
      eventPlyOffset: Int,
      replay: CanonicalLineReplay
  ): Option[TrackedActor] =
    val initial = for
      rootStep <- line.lineReplaySteps.headOption
      transition <- replay.transition(rootStep)
      movement <- transition.boardFootprint.pieceTransitions.find(movement =>
        movement.side == actor.color &&
          movement.from.key.equalsIgnoreCase(actor.from.key) &&
          movement.to.key.equalsIgnoreCase(actor.to.key) &&
          movement.beforeRole.name.equalsIgnoreCase(actor.role.name)
      )
    yield TrackedActor(movement.to, EvidencePieceRole(movement.afterRole.name))
    line.lineReplaySteps.slice(1, eventPlyOffset).foldLeft(initial) { (tracked, step) =>
      tracked.flatMap { current =>
        replay.transition(step).flatMap { transition =>
          transition.boardFootprint.pieceTransitions.find(movement =>
            movement.side == actor.color &&
              movement.from == current.square &&
              movement.beforeRole.name.equalsIgnoreCase(current.role.name)
          ) match
            case Some(movement) => Some(TrackedActor(movement.to, EvidencePieceRole(movement.afterRole.name)))
            case None if transition.boardFootprint.changedSquareSet(current.square) => None
            case None => Some(current)
        }
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

  def hasClosedMaterialRecovery(records: List[EvidenceRecord]): Boolean =
    fromRecords(records).exists(_.hasClosedMaterialRecovery)


final case class CandidateLineEvaluationEvidence(
    line: LineNodeRef,
    evaluation: lila.chessjudgment.model.line.CandidateLineEvaluation
) extends EvidencePayload


final case class MoveTransitionEvidence private[chessjudgment] (
    moveUci: String,
    from: PositionNodeRef,
    to: PositionNodeRef,
    private[chessjudgment] val canonicalTransitionProof: Option[CanonicalTransitionProof]
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
  case SemiOpenFile(side: Color, file: EvidenceFile)
  case PieceAt(side: Color, role: EvidencePieceRole, square: EvidenceSquare)
  case GeometricControlSetChange(
      mover: RelationMoveTransitionWitness,
      controllingSide: Color,
      targetSquare: EvidenceSquare,
      beforeTarget: RelationControlTarget,
      afterTarget: RelationControlTarget,
      beforeControllers: List[RelationPieceWitness],
      afterControllers: List[RelationPieceWitness],
      removedControllers: List[RelationPieceWitness],
      establishedControllers: List[RelationPieceWitness]
  )
  case SliderReachChange(
      side: Color,
      sliderBefore: Option[RelationPieceWitness],
      sliderAfter: Option[RelationPieceWitness],
      direction: RelationRayDirection,
      gained: List[RelationControlReachWitness],
      lost: List[RelationControlReachWitness]
  )
  case PawnTensionCreated(side: Color, from: EvidenceSquare, to: EvidenceSquare)
  case PawnTensionResolved(side: Color, from: EvidenceSquare, to: EvidenceSquare)
  case Battery(formation: RelationBatteryFormationWitness)
  case PassedPawnCreated(side: Color, square: EvidenceSquare)
  case PassedPawnLost(side: Color, square: EvidenceSquare)
  case PassedPawnAdvanced(side: Color, from: EvidenceSquare, to: EvidenceSquare, relativeRank: Int)
  case PassedStatusCreated(side: Color, from: EvidenceSquare, to: EvidenceSquare, relativeRank: Int)
  case PassedPawnPromoted(side: Color, from: EvidenceSquare, to: EvidenceSquare)

  def stableKey: String = StructuralSubject.stableKey(this)

  def label: String =
    this match
      case OpenFile(file) => s"open-file:${file.key.toLowerCase}"
      case SemiOpenFile(side, file) => s"semi-open-file:${side.toString.toLowerCase}:${file.key.toLowerCase}"
      case PieceAt(side, role, square) =>
        s"piece-at:${side.toString.toLowerCase}:${role.name.toLowerCase}:${square.key.toLowerCase}"
      case GeometricControlSetChange(
            mover,
            controllingSide,
            targetSquare,
            beforeTarget,
            afterTarget,
            beforeControllers,
            afterControllers,
            removedControllers,
            establishedControllers
          ) =>
        s"geometric-control-set-change:${mover.stableKey}:${controllingSide.toString.toLowerCase}:${targetSquare.key.toLowerCase}:" +
          s"${StructuralSubject.controlTargetKey(beforeTarget)}>${StructuralSubject.controlTargetKey(afterTarget)}:" +
          s"before:${StructuralSubject.pieceKeys(beforeControllers)}:after:${StructuralSubject.pieceKeys(afterControllers)}:" +
          s"removed:${StructuralSubject.pieceKeys(removedControllers)}:established:${StructuralSubject.pieceKeys(establishedControllers)}"
      case SliderReachChange(side, sliderBefore, sliderAfter, direction, gained, lost) =>
        s"slider-reach-change:${side.toString.toLowerCase}:${StructuralSubject.sliderKey(sliderBefore)}-${StructuralSubject.sliderKey(sliderAfter)}:${direction.stableKey}:gained:${gained.map(_.stableKey).mkString(",")}:lost:${lost.map(_.stableKey).mkString(",")}"
      case PawnTensionCreated(side, from, to) =>
        s"created-tension:${side.toString.toLowerCase}:${from.key.toLowerCase}-${to.key.toLowerCase}"
      case PawnTensionResolved(side, from, to) =>
        s"resolved-tension:${side.toString.toLowerCase}:${from.key.toLowerCase}-${to.key.toLowerCase}"
      case Battery(formation) =>
        val sliders = List(formation.firstSlider, formation.secondSlider)
        s"battery:${formation.side.toString.toLowerCase}:${formation.axis.toString.toLowerCase}:${sliders.map(_.square.key.toLowerCase).mkString("-")}:${sliders.map(_.role.name.toLowerCase).mkString("-")}"
      case PassedPawnCreated(side, square) =>
        s"passed-pawn-created:${side.toString.toLowerCase}:${square.key.toLowerCase}"
      case PassedPawnLost(side, square) =>
        s"passed-pawn-lost:${side.toString.toLowerCase}:${square.key.toLowerCase}"
      case PassedPawnAdvanced(side, from, to, rank) =>
        s"passed-pawn-advanced:${side.toString.toLowerCase}:${from.key.toLowerCase}-${to.key.toLowerCase}:rank-$rank"
      case PassedStatusCreated(side, from, to, rank) =>
        s"passed-status-created:${side.toString.toLowerCase}:${from.key.toLowerCase}-${to.key.toLowerCase}:rank-$rank"
      case PassedPawnPromoted(side, from, to) =>
        s"passed-pawn-promoted:${side.toString.toLowerCase}:${from.key.toLowerCase}-${to.key.toLowerCase}"

  def semanticSquares: List[EvidenceSquare] =
    this match
      case OpenFile(_) | SemiOpenFile(_, _) => Nil
      case PieceAt(_, _, square) => List(square)
      case GeometricControlSetChange(mover, _, target, _, _, before, after, removed, established) =>
        (List(mover.from, mover.to, target) ++ (before ++ after ++ removed ++ established).map(_.square)).distinct
      case SliderReachChange(_, sliderBefore, sliderAfter, _, gained, lost) =>
        (sliderBefore.toList.map(_.square) ++ sliderAfter.toList.map(_.square) ++
          (gained ++ lost).map(_.square)).distinct
      case PawnTensionCreated(_, from, to) => List(from, to)
      case PawnTensionResolved(_, from, to) => List(from, to)
      case Battery(formation) =>
        List(formation.firstSlider.square, formation.secondSlider.square)
      case PassedPawnCreated(_, square) => List(square)
      case PassedPawnLost(_, square) => List(square)
      case PassedPawnAdvanced(_, from, to, _) => List(from, to)
      case PassedStatusCreated(_, from, to, _) => List(from, to)
      case PassedPawnPromoted(_, from, to) => List(from, to)

  def targetSquares: List[EvidenceSquare] =
    this match
      case OpenFile(_) | SemiOpenFile(_, _) => Nil
      case PieceAt(_, _, square) => List(square)
      case GeometricControlSetChange(_, _, target, _, _, _, _, _, _) => List(target)
      case SliderReachChange(_, _, _, _, gained, lost) => (gained ++ lost).map(_.square).distinct
      case PawnTensionCreated(_, from, to) => List(from, to)
      case PawnTensionResolved(_, from, to) => List(from, to)
      case Battery(_) => Nil
      case PassedPawnCreated(_, square) => List(square)
      case PassedPawnLost(_, square) => List(square)
      case PassedPawnAdvanced(_, _, to, _) => List(to)
      case PassedStatusCreated(_, _, to, _) => List(to)
      case PassedPawnPromoted(_, _, to) => List(to)

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

  private def sliderKey(slider: Option[RelationPieceWitness]): String =
    slider.fold("absent")(piece => s"${piece.role.name.toLowerCase}@${piece.square.key.toLowerCase}")

  private def pieceKey(piece: RelationPieceWitness): String =
    s"${piece.role.name.toLowerCase}@${piece.square.key.toLowerCase}"

  private def pieceKeys(pieces: List[RelationPieceWitness]): String =
    pieces.map(pieceKey).mkString(",")

  private def controlTargetKey(target: RelationControlTarget): String =
    target match
      case RelationControlTarget.Empty          => "empty"
      case RelationControlTarget.Friendly(role) => s"friendly:${role.name.toLowerCase}"
      case RelationControlTarget.Enemy(role)    => s"enemy:${role.name.toLowerCase}"

  private[chessjudgment] def fromGeometricControlSetDelta(
      relation: RelationFactEvidence
  ): Option[StructuralSubject] =
    if relation.kind != RelationFactKind.GeometricControlSetDelta then None
    else
      relation.detail match
        case RelationWitnessDetail.GeometricControlSetDelta(
              mover,
              controllingSide,
              targetSquare,
              beforeTarget,
              afterTarget,
              beforeControllers,
              afterControllers,
              removedControllers,
              establishedControllers,
              _
            ) =>
          Some(
            GeometricControlSetChange(
              mover,
              controllingSide,
              targetSquare,
              beforeTarget,
              afterTarget,
              beforeControllers,
              afterControllers,
              removedControllers,
              establishedControllers
            )
          )
        case _ => None

  private[chessjudgment] def fromSliderReachDelta(relation: RelationFactEvidence): Option[StructuralSubject] =
    if relation.kind != RelationFactKind.SliderReachDelta then None
    else
      relation.detail match
        case RelationWitnessDetail.SliderReachDelta(
              _,
              side,
              sliderBefore,
              sliderAfter,
              direction,
              before,
              after,
              _
            ) =>
          val beforeSegment = before.toList.flatMap(_.segment)
          val afterSegment = after.toList.flatMap(_.segment)
          val beforeSet = beforeSegment.toSet
          val afterSet = afterSegment.toSet
          val gained = afterSegment.filterNot(beforeSet)
          val lost = beforeSegment.filterNot(afterSet)
          Option.when(gained.nonEmpty || lost.nonEmpty)(
            SliderReachChange(side, sliderBefore, sliderAfter, direction, gained, lost)
          )
        case _ => None

  def stableKey(subject: StructuralSubject): String =
    subject match
      case OpenFile(file) => key("open-file", file.key)
      case SemiOpenFile(side, file) => key("semi-open-file", side.toString, file.key)
      case PieceAt(side, role, square) => key("piece-at", side.toString, role.name, square.key)
      case GeometricControlSetChange(
            mover,
            controllingSide,
            targetSquare,
            beforeTarget,
            afterTarget,
            beforeControllers,
            afterControllers,
            removedControllers,
            establishedControllers
          ) =>
        key(
          "geometric-control-set-change",
          mover.stableKey,
          controllingSide.toString,
          targetSquare.key,
          controlTargetKey(beforeTarget),
          controlTargetKey(afterTarget),
          pieceKeys(beforeControllers),
          pieceKeys(afterControllers),
          pieceKeys(removedControllers),
          pieceKeys(establishedControllers)
        )
      case SliderReachChange(side, sliderBefore, sliderAfter, direction, gained, lost) =>
        key(
          "slider-reach-change",
          side.toString,
          sliderKey(sliderBefore),
          sliderKey(sliderAfter),
          direction.stableKey,
          gained.map(_.stableKey).mkString(","),
          lost.map(_.stableKey).mkString(",")
        )
      case PawnTensionCreated(side, from, to) => key("pawn-tension-created", side.toString, from.key, to.key)
      case PawnTensionResolved(side, from, to) => key("pawn-tension-resolved", side.toString, from.key, to.key)
      case Battery(formation) =>
        key(
          "battery",
          formation.side.toString,
          formation.axis.toString,
          formation.firstSlider.square.key,
          formation.firstSlider.role.name,
          formation.secondSlider.square.key,
          formation.secondSlider.role.name
        )
      case PassedPawnCreated(side, square) => key("passed-pawn-created", side.toString, square.key)
      case PassedPawnLost(side, square) => key("passed-pawn-lost", side.toString, square.key)
      case PassedPawnAdvanced(side, from, to, rank) =>
        key("passed-pawn-advanced", side.toString, from.key, to.key, rank.toString)
      case PassedStatusCreated(side, from, to, rank) =>
        key("passed-status-created", side.toString, from.key, to.key, rank.toString)
      case PassedPawnPromoted(side, from, to) =>
        key("passed-pawn-promoted", side.toString, from.key, to.key)

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
  case GeometricControlSetChanged
  case PawnTensionCreated
  case PawnTensionResolution
  case PassedPawnProgress
  case PassedPawnConcession
  case BatteryFormation
  case SliderReachChanged

object TransitionConsequenceKind:
  private val RootActorBound = Set(
    BatteryFormation,
    PassedPawnProgress
  )

  private val EstablishedStates = Set(
    OpenFileEstablished,
    SemiOpenFileEstablished,
    PawnTensionCreated,
    PassedPawnProgress,
    BatteryFormation
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
      case OpenFileEstablished | SemiOpenFileEstablished | GeometricControlSetChanged |
          PawnTensionCreated | PawnTensionResolution | BatteryFormation | SliderReachChanged =>
        StructuralSignalPolarity.Neutral
      case PassedPawnProgress =>
        StructuralSignalPolarity.Gain

  def establishesState(kind: TransitionConsequenceKind): Boolean = EstablishedStates(kind)
  def removesState(kind: TransitionConsequenceKind): Boolean = RemovedStates(kind)

enum TransitionConsequenceCategory:
  case PawnStructure
  case PawnStructureDelta

private[chessjudgment] final case class DerivedRelationResultKey(
    kind: RelationFactKind,
    semanticId: String
):
  require(semanticId.matches("[0-9a-f]{64}"), "a derived relation key needs a canonical semantic id")
  def stableKey: String = s"${RelationFactKind.id(kind)}:$semanticId"

private[chessjudgment] object DerivedRelationResultKey:
  def from(relation: RelationFactEvidence): DerivedRelationResultKey =
    require(
      relation.proofStage match
        case RelationProofStage.TransitionFact | RelationProofStage.Vertical(_) => true
        case _                                                                 => false,
      "a structural derived source must be a certified transition or vertical result"
    )
    DerivedRelationResultKey(relation.kind, relation.semanticId)

private[chessjudgment] final case class StructuralDerivedRelationSource(
    key: DerivedRelationResultKey,
    source: EvidenceRef
)

final case class StructuralProofProvenance(
    proofKey: String,
    evidenceId: String
)

final case class StructuralSubjectBinding private[chessjudgment] (
    subject: StructuralSubject,
    relationKeys: List[RelationChangeKey],
    derivedRelationKeys: List[DerivedRelationResultKey]
):
  require(relationKeys.distinct.size == relationKeys.size, "duplicate structural-subject relation keys")
  require(
    derivedRelationKeys.distinct.size == derivedRelationKeys.size,
    "duplicate structural-subject derived relation keys"
  )
  def stableKey: String =
    s"${subject.stableKey}:relations:${relationKeys.map(_.stableKey).mkString("[", ",", "]")}:derived:${derivedRelationKeys.map(_.stableKey).mkString("[", ",", "]")}"

object StructuralSubjectBinding:
  private[chessjudgment] def unbound(subject: StructuralSubject): StructuralSubjectBinding =
    StructuralSubjectBinding(subject, Nil, Nil)

  private[chessjudgment] def fromRelations(
      subject: StructuralSubject,
      relationKeys: List[RelationChangeKey]
  ): StructuralSubjectBinding =
    require(relationKeys.nonEmpty, "a relation-derived structural subject requires exact relation keys")
    require(
      relationKeys.distinct.size == relationKeys.size,
      "a relation-derived structural subject cannot hide duplicate proof keys"
    )
    StructuralSubjectBinding(subject, relationKeys.sortBy(_.stableKey), Nil)

  private[chessjudgment] def fromDerivedRelations(
      subject: StructuralSubject,
      relationKeys: List[DerivedRelationResultKey]
  ): StructuralSubjectBinding =
    require(relationKeys.nonEmpty, "a derived structural subject requires exact result keys")
    require(
      relationKeys.distinct.size == relationKeys.size,
      "a derived structural subject cannot hide duplicate result keys"
    )
    StructuralSubjectBinding(subject, Nil, relationKeys.sortBy(_.stableKey))

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
  private[chessjudgment] def derivedRelationKeys: List[DerivedRelationResultKey] =
    (subjectBindings ++ targetBindings).flatMap(_.derivedRelationKeys).distinct.sortBy(_.stableKey)
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
  def proofKey: String = stableKey
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
    TransitionConsequenceKind.GeometricControlSetChanged,
    TransitionConsequenceKind.PawnTensionCreated,
    TransitionConsequenceKind.PawnTensionResolution,
    TransitionConsequenceKind.PassedPawnProgress,
    TransitionConsequenceKind.PassedPawnConcession,
    TransitionConsequenceKind.BatteryFormation,
    TransitionConsequenceKind.SliderReachChanged
  )

  def relationBacked(kind: TransitionConsequenceKind): Boolean =
    RelationBackedKinds(kind)

  def provesSemantic(
      consequences: List[TransitionConsequence],
      delta: RelationSemanticDelta,
      derivedRelations: List[RelationFactEvidence]
  ): Boolean =
    relationSemanticsResolve(
      consequences,
      delta.changes.map(change => RelationChangeProofView(change.direction, change.key, change.detail)),
      derivedRelations,
      delta
    )

  def provesCanonical(
      consequences: List[TransitionConsequence],
      changes: List[CanonicalRelationChange],
      transition: StructuralTransitionBinding,
      semanticDelta: RelationSemanticDelta,
      derivedRelations: List[RelationFactEvidence]
  ): Boolean =
    changes.map(_.key).toSet == semanticDelta.changes.map(_.key).toSet &&
      changes.forall { change =>
        val owner = change.direction match
          case RelationChangeDirection.Removed     => transition.from
          case RelationChangeDirection.Established => transition.to
        change.source.position == owner
      } && relationSemanticsResolve(
        consequences,
        changes.map(change => RelationChangeProofView(change.direction, change.key, change.detail)),
        derivedRelations,
        semanticDelta
      )

  private final case class RelationChangeProofView(
      direction: RelationChangeDirection,
      key: RelationChangeKey,
      detail: RelationWitnessDetail
  )

  private def relationSemanticsResolve(
      consequences: List[TransitionConsequence],
      changes: List[RelationChangeProofView],
      derivedRelations: List[RelationFactEvidence],
      delta: RelationSemanticDelta
  ): Boolean =
    val byKey = changes.map(change => change.key -> change).toMap
    val derivedByKey = derivedRelations.map(relation => DerivedRelationResultKey.from(relation) -> relation).toMap
    byKey.size == changes.size && derivedByKey.size == derivedRelations.size && consequences.forall { consequence =>
      val bindings = consequence.subjectBindings ++ consequence.targetBindings
      if relationBacked(consequence.kind) then
        bindings.nonEmpty && bindings.forall { binding =>
          (binding.relationKeys.nonEmpty || binding.derivedRelationKeys.nonEmpty) &&
            binding.relationKeys.flatMap(byKey.get).size == binding.relationKeys.size &&
            binding.derivedRelationKeys.flatMap(derivedByKey.get).size == binding.derivedRelationKeys.size &&
            provesBinding(
              consequence.kind,
              binding.subject,
              binding.relationKeys.map(byKey),
              binding.derivedRelationKeys.map(derivedByKey),
              delta
            )
        }
      else
        bindings.forall(binding => binding.relationKeys.isEmpty && binding.derivedRelationKeys.isEmpty)
    }

  private def provesBinding(
      consequenceKind: TransitionConsequenceKind,
      subject: StructuralSubject,
      changes: List[RelationChangeProofView],
      derivedRelations: List[RelationFactEvidence],
      delta: RelationSemanticDelta
  ): Boolean =
    def passageStates(
        side: Color
    ): List[(RelationChangeDirection, EvidenceSquare, List[EvidenceSquare])] =
      changes.flatMap(change => change.detail match
        case RelationWitnessDetail.PawnPassage(owner, pawn, opposingPawns) if owner == side =>
          List((change.direction, pawn, opposingPawns))
        case _ => Nil
      )
    def exactPassageTransition(
        side: Color,
        from: EvidenceSquare,
        to: EvidenceSquare,
        beforePassed: Boolean,
        afterPassed: Boolean
    ): Boolean =
      val states = passageStates(side)
      states.size == changes.size && states.count {
        case (RelationChangeDirection.Removed, pawn, opposing) =>
          pawn == from && opposing.isEmpty == beforePassed
        case _ => false
      } == 1 && states.count {
        case (RelationChangeDirection.Established, pawn, opposing) =>
          pawn == to && opposing.isEmpty == afterPassed
        case _ => false
      } == 1
    def rootPawnTransition(side: Color, from: EvidenceSquare, to: EvidenceSquare): Boolean =
      val root = delta.rootMove
      root.side == side && root.from == from && root.to == to &&
        root.beforeRole.name.equalsIgnoreCase(Pawn.name)
    def exactFile(file: EvidenceFile): Option[File] =
      file.key.toLowerCase.headOption.flatMap(File.fromChar)
    def afterPawnFile(file: EvidenceFile): Option[PositionRelationExtractor.ClosedPawnFile] =
      exactFile(file).flatMap(exact =>
        delta.afterInventory.pawnFiles(Set(exact)).find(_.file == file)
      )
    def exactPawnTension(
        direction: RelationChangeDirection,
        side: Color,
        from: EvidenceSquare,
        to: EvidenceSquare
    ): Boolean =
      changes.size == 1 && changes.head.direction == direction && (changes.head.detail match
        case RelationWitnessDetail.GeometricControl(
              Color.White,
              whitePawn,
              whiteRole,
              blackPawn,
              RelationControlTarget.Enemy(blackRole)
            ) =>
          whiteRole.name.equalsIgnoreCase(Pawn.name) && blackRole.name.equalsIgnoreCase(Pawn.name) &&
            (if side.white then whitePawn == from && blackPawn == to
             else blackPawn == from && whitePawn == to)
        case _ => false)

    val acceptsDerivedRelations =
      consequenceKind == TransitionConsequenceKind.GeometricControlSetChanged ||
        consequenceKind == TransitionConsequenceKind.SliderReachChanged
    if !acceptsDerivedRelations && derivedRelations.nonEmpty then false
    else (consequenceKind, subject) match
      case (TransitionConsequenceKind.OpenFileEstablished, StructuralSubject.OpenFile(file)) =>
        changes.nonEmpty && changes.forall(change => change.detail match
          case RelationWitnessDetail.PawnFileGroup(_, exact, _) => exact == file
          case _                                                => false
        ) && afterPawnFile(file).exists(_.isOpen)
      case (
            TransitionConsequenceKind.SemiOpenFileEstablished,
            StructuralSubject.SemiOpenFile(side, file)
          ) =>
        changes.nonEmpty && changes.forall(change => change.detail match
          case RelationWitnessDetail.PawnFileGroup(_, exact, _) => exact == file
          case _                                                => false
        ) && afterPawnFile(file).exists(_.semiOpenFor(side))
      case (
            TransitionConsequenceKind.GeometricControlSetChanged,
            exact: StructuralSubject.GeometricControlSetChange
          ) =>
        changes.isEmpty && derivedRelations.nonEmpty &&
          derivedRelations.forall(StructuralSubject.fromGeometricControlSetDelta(_).contains(exact))
      case (
            TransitionConsequenceKind.SliderReachChanged,
            exact: StructuralSubject.SliderReachChange
          ) =>
        changes.isEmpty && derivedRelations.nonEmpty &&
          derivedRelations.forall(StructuralSubject.fromSliderReachDelta(_).contains(exact))
      case (
            TransitionConsequenceKind.PawnTensionCreated,
            StructuralSubject.PawnTensionCreated(side, from, to)
          ) =>
        exactPawnTension(RelationChangeDirection.Established, side, from, to)
      case (
            TransitionConsequenceKind.PawnTensionResolution,
            StructuralSubject.PawnTensionResolved(side, from, to)
          ) =>
        exactPawnTension(RelationChangeDirection.Removed, side, from, to)
      case (
            TransitionConsequenceKind.PassedPawnProgress,
            StructuralSubject.PassedPawnCreated(side, square)
          ) =>
        exactPassageTransition(side, square, square, beforePassed = false, afterPassed = true)
      case (
            TransitionConsequenceKind.PassedPawnConcession,
            StructuralSubject.PassedPawnLost(side, square)
          ) =>
        passageStates(side) match
          case List(
                (RelationChangeDirection.Removed, from, beforeOpposing),
                (RelationChangeDirection.Established, to, afterOpposing)
              ) =>
            to == square && beforeOpposing.isEmpty && afterOpposing.nonEmpty &&
              (from == to || rootPawnTransition(side, from, to))
          case List(
                (RelationChangeDirection.Established, to, afterOpposing),
                (RelationChangeDirection.Removed, from, beforeOpposing)
              ) =>
            to == square && beforeOpposing.isEmpty && afterOpposing.nonEmpty &&
              (from == to || rootPawnTransition(side, from, to))
          case _ => false
      case (
            TransitionConsequenceKind.PassedPawnProgress,
            StructuralSubject.PassedPawnAdvanced(side, from, to, _)
          ) =>
        rootPawnTransition(side, from, to) &&
          exactPassageTransition(side, from, to, beforePassed = true, afterPassed = true)
      case (
            TransitionConsequenceKind.PassedPawnProgress,
            StructuralSubject.PassedStatusCreated(side, from, to, _)
          ) =>
        rootPawnTransition(side, from, to) &&
          exactPassageTransition(side, from, to, beforePassed = false, afterPassed = true)
      case (
            TransitionConsequenceKind.PassedPawnProgress,
            StructuralSubject.PassedPawnPromoted(side, from, to)
          ) =>
        val root = delta.rootMove
        rootPawnTransition(side, from, to) && !root.afterRole.name.equalsIgnoreCase(Pawn.name) &&
          passageStates(side) == List((RelationChangeDirection.Removed, from, Nil))
      case (
            TransitionConsequenceKind.BatteryFormation,
            StructuralSubject.Battery(formation)
          ) =>
        changes.nonEmpty && changes.forall(change =>
          change.direction == RelationChangeDirection.Established && (change.detail match
            case ray: RelationWitnessDetail.RayBarrier =>
              RelationRayProjection.batteryFormation(ray).contains(formation)
            case _ => false)
        )
      case (
            TransitionConsequenceKind.BatteryFormation,
            StructuralSubject.PieceAt(side, role, square)
          ) =>
        changes.nonEmpty && changes.forall(change =>
          change.direction == RelationChangeDirection.Established && (change.detail match
            case ray: RelationWitnessDetail.RayBarrier =>
              ray.occupants.contains(RelationColoredPieceWitness(square, role, side))
            case _ => false)
        )
      case _ => false

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

private[chessjudgment] final case class RelationReplayCertificationOwner(
    semanticStartFen: String,
    semanticAfterFen: String,
    proofMoves: List[String],
    footprint: BoardTransitionFootprint
)

private[chessjudgment] final class ClosedRelationOutputBinding private[judgment] (
    val result: EvidenceRef,
    val relation: RelationFactEvidence,
    val sources: List[EvidenceRef],
    private[chessjudgment] val absenceProofs: List[BoundClosedRelationAbsence],
    private[chessjudgment] val stateProofs: List[BoundClosedPositionState]
):
  private val rootAndClosedAbsenceOnly =
    VerticalRelationContracts.proofOf(relation.detail).exists(proof =>
      proof.sourcePremises.nonEmpty &&
        proof.sourcePremises.forall(_.source == VerticalRelationPremiseSource.RootTransition) &&
        (proof.absences.nonEmpty || proof.states.nonEmpty) &&
        absenceProofs.map(_.premise) == proof.absences &&
        stateProofs.map(_.premise) == proof.states
    )
  require(
    sources.nonEmpty || rootAndClosedAbsenceOnly,
    "a replay-derived relation output needs a positive source or an exact root-and-absence proof"
  )
  require(sources.map(_.id).distinct.size == sources.size, "a relation output cannot repeat one source")

private[judgment] object ClosedRelationOutputBinding:
  def certified(
      result: EvidenceRef,
      relation: RelationFactEvidence,
      sources: List[EvidenceRef],
      absenceProofs: List[BoundClosedRelationAbsence],
      stateProofs: List[BoundClosedPositionState]
  ): ClosedRelationOutputBinding =
    new ClosedRelationOutputBinding(
      result,
      relation,
      sources.sortBy(_.id),
      absenceProofs.sortBy(_.premise.stableKey),
      stateProofs.sortBy(_.premise.stableKey)
    )

/** Persistent occurrence owner for one already-evaluated closed relation
  * production. Contract closure remains in the semantic inventory; the
  * occurrence persists only actual outputs and their exact source bindings.
  */
final class ClosedRelationOccurrenceEvidence private (
    val edge: MoveTransitionEdge,
    val lineOwner: Option[LineNodeRef],
    val lineEvidence: Option[EvidenceRef],
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
    new ClosedRelationOccurrenceEvidence(
      edge,
      lineOwner,
      lineEvidence,
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
  case RayFirstOccupant(square: Square)
  case PawnIdentity(side: Color, square: Square)
  case PawnFrontCell(side: Color, square: Square)
  case PawnFile(side: Color, file: File)

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
      case RelationWitnessDetail.RayBarrier(_, attacker, _, occupants, _) =>
        Set[RelationDependencyKey](SliderOrigin(boardSquare(attacker))) ++
          occupants.headOption.map(occupant => RayFirstOccupant(boardSquare(occupant.square)))
      case RelationWitnessDetail.PawnFileGroup(side, file, _) =>
        Set[RelationDependencyKey](PawnFile(side, boardFile(file)))
      case RelationWitnessDetail.PawnFrontOccupancy(side, pawn, front, _) =>
        Set[RelationDependencyKey](PawnIdentity(side, boardSquare(pawn))) ++
          front.map(value => PawnFrontCell(side, boardSquare(value)))
      case RelationWitnessDetail.PawnAdvanceAffordance(side, pawn, _, traversed) =>
        Set[RelationDependencyKey](PawnIdentity(side, boardSquare(pawn))) ++
          traversed.map(value => PawnFrontCell(side, boardSquare(value)))
      case RelationWitnessDetail.PawnPassage(side, pawn, _) =>
        val origin = boardSquare(pawn)
        val passage = (0 to 7).filter(rank =>
          if side.white then rank > origin.rank.value else rank < origin.rank.value
        ).flatMap(rank =>
          (-1 to 1).flatMap(fileOffset => Square.at(origin.file.value + fileOffset, rank))
        ).map(target => PawnIdentity(!side, target)).toSet
        passage + PawnIdentity(side, origin)
      case _: RelationWitnessDetail.GeometricSupportCausalTransition |
      _: RelationWitnessDetail.CaptureRecaptureInventory |
      _: RelationWitnessDetail.CreatedCheckResponseInventory |
          _: RelationWitnessDetail.RootCheckResponse |
          _: RelationWitnessDetail.MovementAffordanceLegalRestriction |
          _: RelationWitnessDetail.AbsolutePinMovementRestriction |
          _: RelationWitnessDetail.SliderReachDelta |
          _: RelationWitnessDetail.SharedGeometricSupportOfEnemyControlledTargets |
          _: RelationWitnessDetail.GeometricMultiTargetContact |
          _: RelationWitnessDetail.GeometricEnemyContactWithoutFriendlySupport |
          _: RelationWitnessDetail.PawnTopologyTransition |
          _: RelationWitnessDetail.PawnOccupiedFilePartitionTransition |
          _: RelationWitnessDetail.MajorPiecePawnFileCorridorTransition |
          _: RelationWitnessDetail.CastlingRightRemoved |
          _: RelationWitnessDetail.StalemateTransition |
          _: RelationWitnessDetail.GeometricControlSetDelta |
          _: RelationWitnessDetail.GeometricSupporterCapture | _: RelationWitnessDetail.GeometricSupportDelta |
          _: RelationWitnessDetail.SliderLineInterruption |
          _: RelationWitnessDetail.GeometricLineControlAfterBlockerRemoval |
          _: RelationWitnessDetail.NamedRayTransition =>
        Set.empty[RelationDependencyKey]
    Option.when(keys.nonEmpty) {
      val spans = detail match
        case RelationWitnessDetail.GeometricControl(_, attacker, _, target, _) => lineSpan(attacker, target)
        case RelationWitnessDetail.RayBarrier(_, attacker, _, occupants, _) =>
          raySpanToEdge(attacker, occupants.head.square)
        case RelationWitnessDetail.PawnPassage(side, pawn, _) => pawnPassageSpan(side, pawn)
        case RelationWitnessDetail.PawnFileGroup(_, file, _) => fileSpan(file)
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
    val assertionId: String,
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
    val assertionRaw = RelationWitnessDetail.assertionStableKey(detail)
    val digest = MessageDigest.getInstance("SHA-256")
    val assertionId = digest
      .digest(assertionRaw.getBytes(StandardCharsets.UTF_8))
      .map(byte => f"${byte & 0xff}%02x")
      .mkString
    val semanticId = MessageDigest
      .getInstance("SHA-256")
      .digest(raw.getBytes(StandardCharsets.UTF_8))
      .map(byte => f"${byte & 0xff}%02x")
      .mkString
    new CanonicalRelationFact(
      kind = RelationWitnessDetail.factKind(detail),
      detail = detail,
      lineMoves = normalizedLineMoves,
      assertionId = assertionId,
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
    participants.nonEmpty || focusSquares.nonEmpty || files.nonEmpty
  def semanticId: String = canonicalFact.semanticId
  def assertionId: String = canonicalFact.assertionId
  private[chessjudgment] def proofStage: RelationProofStage =
    RelationWitnessDetail.proofStage(detail)
  def isPositionRelation: Boolean =
    proofStage == RelationProofStage.PositionFact
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
      case RelationWitnessDetail.SliderLineInterruption(mover, _, _, _, _, _, _) =>
        moved(mover.side, mover.from, mover.to, Some(mover.beforeRole), Some(mover.afterRole))
      case RelationWitnessDetail.GeometricLineControlAfterBlockerRemoval(mover, _, _, _, _, _, _, _, _, _, _) =>
        moved(mover.side, mover.from, mover.to, Some(mover.beforeRole), Some(mover.afterRole))
      case RelationWitnessDetail.NamedRayTransition(mover, _, _, _, _, _, _, _, _, _) =>
        moved(mover.side, mover.from, mover.to, Some(mover.beforeRole), Some(mover.afterRole))
      case RelationWitnessDetail.GeometricSupportCausalTransition(mover, _, _, _, _, _, _, _, _, _, _) =>
        moved(mover.side, mover.from, mover.to, Some(mover.beforeRole), Some(mover.afterRole))
      case RelationWitnessDetail.CaptureRecaptureInventory(mover, _, _, _, _) =>
        moved(mover.side, mover.from, mover.to, Some(mover.beforeRole), Some(mover.afterRole))
      case RelationWitnessDetail.CreatedCheckResponseInventory(mover, _, _, _, _, _, _, _) =>
        moved(mover.side, mover.from, mover.to, Some(mover.beforeRole), Some(mover.afterRole))
      case RelationWitnessDetail.RootCheckResponse(mover, _, _, _, _, _) =>
        moved(mover.side, mover.from, mover.to, Some(mover.beforeRole), Some(mover.afterRole))
      case RelationWitnessDetail.MovementAffordanceLegalRestriction(mover, _, _, _, _, _, _) =>
        moved(mover.side, mover.from, mover.to, Some(mover.beforeRole), Some(mover.afterRole))
      case RelationWitnessDetail.AbsolutePinMovementRestriction(mover, _, _, _, _, _, _, _, _, _) =>
        moved(mover.side, mover.from, mover.to, Some(mover.beforeRole), Some(mover.afterRole))
      case RelationWitnessDetail.SliderReachDelta(mover, _, _, _, _, _, _, _) =>
        moved(mover.side, mover.from, mover.to, Some(mover.beforeRole), Some(mover.afterRole))
      case RelationWitnessDetail.GeometricMultiTargetContact(mover, _, _, _, _, _, _) =>
        moved(mover.side, mover.from, mover.to, Some(mover.beforeRole), Some(mover.afterRole))
      case RelationWitnessDetail.GeometricEnemyContactWithoutFriendlySupport(mover, _, _, _, _, _) =>
        moved(mover.side, mover.from, mover.to, Some(mover.beforeRole), Some(mover.afterRole))
      case RelationWitnessDetail.SharedGeometricSupportOfEnemyControlledTargets(mover, _, _, _, _, _) =>
        moved(mover.side, mover.from, mover.to, Some(mover.beforeRole), Some(mover.afterRole))
      case RelationWitnessDetail.PawnTopologyTransition(mover, _, _, _, _) =>
        moved(mover.side, mover.from, mover.to, Some(mover.beforeRole), Some(mover.afterRole))
      case RelationWitnessDetail.PawnOccupiedFilePartitionTransition(mover, _, _, _, _) =>
        moved(mover.side, mover.from, mover.to, Some(mover.beforeRole), Some(mover.afterRole))
      case RelationWitnessDetail.MajorPiecePawnFileCorridorTransition(mover, _, _, _, _, _, _, _, _, _, _) =>
        moved(mover.side, mover.from, mover.to, Some(mover.beforeRole), Some(mover.afterRole))
      case RelationWitnessDetail.CastlingRightRemoved(mover, _, _, _) =>
        moved(mover.side, mover.from, mover.to, Some(mover.beforeRole), Some(mover.afterRole))
      case RelationWitnessDetail.StalemateTransition(mover, _, _, _) =>
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
            ref.position.sideToMove.nonEmpty &&
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
            ref.position.sideToMove.nonEmpty &&
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

  private[chessjudgment] def certifiedFromCanonicalPositionFact(
      fact: CanonicalRelationFact,
      semanticFen: String
  ): RelationFactEvidence =
    require(semanticFen.trim.nonEmpty, "a canonical position fact needs its exact semantic FEN")
    new RelationFactEvidence(fact, RelationEvidenceOrigin.PositionSnapshot(semanticFen), None)

  /** The closed transition inventory is the sole producer of replay-derived relation
    * details. Raw callers cannot turn an arbitrary label into legal-replay
    * authority by attaching a matching move list.
    */
  private[chessjudgment] def certifiedTransitionContractBatch(
      batch: TransitionRelationContractBatch,
      delta: RelationSemanticDelta,
      legal: LegalReplayStep,
      footprint: BoardTransitionFootprint
  ): Option[ClosedRelationCombinationResults] =
    for
      certification <- legalReplayCertification(legal, footprint)
      if batch.owns(delta)
    yield batch.results.certifiedBy(certification.owner, certification.certify)

  private[chessjudgment] def certifiedVerticalBatch(
      unverified: ClosedVerticalRelationResults,
      lowerResults: ClosedRelationCombinationResults,
      delta: RelationSemanticDelta,
      legal: LegalReplayStep,
      footprint: BoardTransitionFootprint
  ): Option[ClosedVerticalRelationResults] =
    for
      certification <- legalReplayCertification(legal, footprint)
      if unverified.owns(lowerResults, delta)
      if lowerResults.certifiedFor(certification.owner)
    yield unverified.certifiedBy(certification.certify)

  private final case class LegalReplayCertification(
      semanticStartFen: String,
      semanticAfterFen: String,
      proofMoves: List[String],
      footprint: BoardTransitionFootprint
  ):
    val owner: RelationReplayCertificationOwner =
      RelationReplayCertificationOwner(
        semanticStartFen,
        semanticAfterFen,
        proofMoves,
        footprint
      )

    private val origin = RelationEvidenceOrigin.LegalReplay(
      semanticStartFen,
      semanticAfterFen,
      proofMoves
    )

    def owns(relation: RelationFactEvidence): Boolean =
      relation.origin == origin && relation.lineMoves == proofMoves &&
        relation.rootTransitionFootprint.contains(footprint)

    def certify(relation: RelationFactEvidence): RelationFactEvidence =
      require(
        relation.origin == RelationEvidenceOrigin.Unverified && relation.lineMoves == proofMoves &&
          relation.rootTransitionConnected(footprint),
        "legal-replay certification can only attach authority to its exact closed result"
      )
      new RelationFactEvidence(
        canonicalFact = relation.canonicalFact,
        origin = origin,
        rootTransitionFootprint = Some(footprint)
      )

  private def legalReplayCertification(
      first: LegalReplayStep,
      footprint: BoardTransitionFootprint
  ): Option[LegalReplayCertification] =
    for
      semanticFen <- PrincipalVariationEvidence.semanticBoardStateFen(Fen.write(first.before).value)
      semanticAfterFen <- PrincipalVariationEvidence.semanticBoardStateFen(Fen.write(first.after).value)
      proofMove = EvidenceRef.normalizeMove(first.uci)
      if proofMove.nonEmpty
    yield LegalReplayCertification(
      semanticFen,
      semanticAfterFen,
      List(proofMove),
      footprint
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
    RelationWitnessDetail
      .createdCheckResponse(relation.detail)
      .map(_ => TacticalMechanismKind.KingForcing)

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

/** The exact already-selected bridge between one closed relation occurrence
  * and one root line event. Verification follows these references directly;
  * it must not run the relation/line join a second time.
  */
private[chessjudgment] final case class TacticalRelationLineProof(
    relation: EvidenceRef,
    occurrence: EvidenceRef,
    line: EvidenceRef,
    rootMove: String,
    event: LineMoveEvent
):
  require(EvidenceRef.normalizeMove(rootMove).nonEmpty, "a relation/line proof requires one normalized root move")
  require(event.plyOffset == 0, "a relation/line proof may only bind the root line event")

  def expectedSignals(relationFact: RelationFactEvidence): List[TacticalMechanismSignal] =
    List(
      TacticalMechanismSignal(
        TacticalMechanismSignalKind.Relation,
        relationFact.detail.detailName,
        EvidenceLayer.Relation,
        Some(relation),
        Some(relationFact.kind)
      ),
      TacticalMechanismSignal(
        TacticalMechanismSignalKind.LineEvent,
        event.kind.toString,
        EvidenceLayer.Line,
        Some(line)
      )
    )

final case class TacticalMechanismEvidence(
    kind: TacticalMechanismKind,
    moveUci: Option[String],
    line: Option[LineNodeRef],
    signals: List[TacticalMechanismSignal],
    relationLineProof: Option[TacticalRelationLineProof] = None
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
    occurrenceRecord: EvidenceRecord,
    lineRecord: EvidenceRecord,
    rootMove: String,
    event: LineMoveEvent
):
  val kind: TacticalMechanismKind = TacticalMechanismKind.KingForcing
  val proof: TacticalRelationLineProof =
    TacticalRelationLineProof(
      relationNode.ref,
      occurrenceRecord.ref,
      lineRecord.ref,
      rootMove,
      event
    )
  val signals: List[TacticalMechanismSignal] = proof.expectedSignals(relationNode.relation)

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
      check <- RelationWitnessDetail.createdCheckResponse(node.relation.detail).toList
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
      eventKind = check.terminal match
        case RelationCheckTerminalState.Ongoing   => LineEventKind.Check
        case RelationCheckTerminalState.Checkmate => LineEventKind.Mate
      event <- lineFact.lineEventsOf(eventKind)
      if event.plyOffset == 0 && EvidenceRef.sameMove(event.moveUci, normalizedRoot)
      if event.side.contains(check.mover.side)
      if event.targetRole.exists(_.name.equalsIgnoreCase(King.name))
      if event.square.contains(check.kingSquare)
    yield TacticalRelationLineContract(node, carrierRecord, lineRecord, normalizedRoot, event))
      .sortBy(binding => binding.event.kind.toString)

  def certifiesProof(
      graph: TypedEvidenceGraph,
      payload: TacticalMechanismEvidence,
      proof: TacticalRelationLineProof
  ): Boolean =
    val normalizedRoot = EvidenceRef.normalizeMove(proof.rootMove)
    (for
      relationRecord <- graph.byId.get(proof.relation.id).filter(_.ref == proof.relation)
      node <- graph.relationGraph.byEvidenceId.get(proof.relation.id).filter(_.ref == proof.relation)
      if relationRecord == node.record
      if relationRecord.parents.contains(proof.occurrence)
      occurrenceRecord <- graph.byId.get(proof.occurrence.id).filter(_.ref == proof.occurrence)
      occurrence <- occurrenceRecord.payload match
        case value: ClosedRelationOccurrenceEvidence => Some(value)
        case _                                       => None
      if occurrence.outputFor(proof.relation).exists(_.relation == node.relation)
      if occurrenceRecord.ref.position == node.ref.position
      if EvidenceRef.sameMove(occurrence.edge.moveUci, normalizedRoot)
      if occurrence.lineEvidence.contains(proof.line)
      lineRecord <- graph.byId.get(proof.line.id).filter(_.ref == proof.line)
      lineFact <- lineRecord.payload match
        case value: LineFactEvidence => Some(value)
        case _                       => None
      if lineRecord.ref.producer == EvidenceProducer.LegalLineProducer
      if lineRecord.ref.layer == EvidenceLayer.Line
      if lineRecord.ref.confidence == EvidenceConfidence.LegalReplayVerified
      if lineRecord.ref.position == occurrence.edge.from
      if lineRecord.ref.line == occurrence.lineOwner && occurrence.lineOwner.contains(lineFact.line)
      if lineFact.replayIsCertified && lineFact.rootMove.exists(EvidenceRef.sameMove(_, normalizedRoot))
      if lineFact.lineEventsOf(proof.event.kind).contains(proof.event)
      check <- RelationWitnessDetail.createdCheckResponse(node.relation.detail)
      expectedEventKind = check.terminal match
        case RelationCheckTerminalState.Ongoing   => LineEventKind.Check
        case RelationCheckTerminalState.Checkmate => LineEventKind.Mate
      if proof.event.kind == expectedEventKind
      if proof.event.plyOffset == 0 && EvidenceRef.sameMove(proof.event.moveUci, normalizedRoot)
      if proof.event.side.contains(check.mover.side)
      if proof.event.targetRole.exists(_.name.equalsIgnoreCase(King.name))
      if proof.event.square.contains(check.kingSquare)
      if payload.kind == TacticalMechanismKind.KingForcing
      if payload.moveUci.exists(EvidenceRef.sameMove(_, normalizedRoot))
      if payload.line == occurrence.lineOwner
      if payload.signals == proof.expectedSignals(node.relation)
    yield ()).isDefined

final case class StructuralDeltaEvidence(
    transition: StructuralTransitionBinding,
    signals: List[StructuralSignal],
    consequences: List[TransitionConsequence],
    private[chessjudgment] val relationChanges: List[CanonicalRelationChange],
    private[chessjudgment] val derivedRelationSources: List[StructuralDerivedRelationSource],
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
  def provenanceFor(binding: StructuralSubjectBinding): List[StructuralProofProvenance] =
    val relationProofs = binding.relationKeys.map { key =>
      relationChanges.filter(_.key == key) match
        case exact :: Nil => StructuralProofProvenance(key.stableKey, exact.source.id)
        case matches =>
          throw IllegalStateException(
            s"structural relation key '${key.stableKey}' resolved to ${matches.size} evidence sources"
          )
    }
    val derivedProofs = binding.derivedRelationKeys.map { key =>
      derivedRelationSources.filter(_.key == key) match
        case exact :: Nil => StructuralProofProvenance(key.stableKey, exact.source.id)
        case matches =>
          throw IllegalStateException(
            s"structural derived key '${key.stableKey}' resolved to ${matches.size} evidence sources"
          )
    }
    (relationProofs ++ derivedProofs).sortBy(proof => proof.proofKey -> proof.evidenceId)
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
  private[chessjudgment] def certifiedRootMovement: Option[CanonicalRootLegalMove] =
    certifiedTransitionProof.map(_.rootMovement)
  private[chessjudgment] def certifiedRootResponseMoves: Option[List[String]] =
    certifiedTransitionProof.map(_.legalResponseMoves)
object StructuralDeltaEvidence:
  import TransitionConsequenceKind.*
  import TransitionConsequenceCategory.*

  def hasConsequenceCategory(kind: TransitionConsequenceKind, category: TransitionConsequenceCategory): Boolean =
    consequenceCategories.getOrElse(kind, Set.empty).contains(category)

  private lazy val consequenceCategories: Map[TransitionConsequenceKind, Set[TransitionConsequenceCategory]] =
    Map(
      OpenFileEstablished -> Set(PawnStructure, PawnStructureDelta),
      SemiOpenFileEstablished -> Set(PawnStructure, PawnStructureDelta),
      PawnTensionCreated -> Set(PawnStructure, PawnStructureDelta),
      PawnTensionResolution -> Set(PawnStructureDelta),
      PassedPawnProgress -> Set(PawnStructure, PawnStructureDelta),
      PassedPawnConcession -> Set(PawnStructure, PawnStructureDelta)
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
    private[chessjudgment] val canonicalStep: Option[LegalReplayStep] = None,
    private[chessjudgment] val canonicalMovement: Option[CanonicalRootLegalMove] = None
):
  def moveUci: String = EvidenceRef.normalizeMove(step.moveUci)
  require(
    moveUci.nonEmpty && EvidenceRef.sameMove(identity.rootMove, moveUci),
    "plan-causal event identity must reference the event replay step"
  )

  private def identityMatches(movement: CanonicalRootLegalMove): Boolean =
    EvidenceRef.sameMove(movement.moveUci, moveUci) &&
      identity.actor.side == movement.side &&
      identity.actor.beforeRole.equalsIgnoreCase(movement.beforeRole.name) &&
      identity.actor.afterRole.equalsIgnoreCase(movement.afterRole.name) &&
      identity.actor.from.equalsIgnoreCase(movement.from.key) &&
      identity.actor.to.equalsIgnoreCase(movement.to.key) &&
      identity.actor.legalMoveSemanticId == movement.fact.semanticId

  private def movementMatchesLegal(
      movement: CanonicalRootLegalMove,
      legal: LegalReplayStep
  ): Boolean =
    EvidenceRef.sameMove(movement.moveUci, legal.uci) &&
      movement.side == legal.move.piece.color &&
      movement.from.key.equalsIgnoreCase(legal.move.orig.key) &&
      movement.to.key.equalsIgnoreCase(legal.move.dest.key) &&
      movement.beforeRole.name.equalsIgnoreCase(legal.move.piece.role.name)

  private[chessjudgment] def certifiedLegalStep: Option[LegalReplayStep] =
    for
      legal <- canonicalStep
      movement <- canonicalMovement
      if identityMatches(movement)
      if movementMatchesLegal(movement, legal)
      if legal.ply == step.ply
      if EvidenceRef.sameMove(legal.uci, step.moveUci)
      if PrincipalVariationEvidence.sameBoardState(Fen.write(legal.before).value, step.fenBefore)
      if PrincipalVariationEvidence.sameBoardState(Fen.write(legal.after).value, step.fenAfter)
      if legal.move.piece.color == perspective
    yield legal

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
    from.certifiedLegalStep.nonEmpty &&
      to.certifiedLegalStep.nonEmpty &&
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
          case _ => Nil)
  def proofPieceRoles: List[EvidencePieceRole] =
    proof match
      case PlanCausalDependencyProof.ObjectState(trajectory) =>
        List(trajectory.rootBeforeRole, trajectory.pieceRole, trajectory.futureAfterRole).distinct
      case PlanCausalDependencyProof.LineAccess(trajectory) =>
        List(trajectory.enabledPieceRole)
      case PlanCausalDependencyProof.ResponseContinuation(trajectory) =>
        trajectory.involvedRoles
private[chessjudgment] enum PlanCausalResponseReason:
  case RootActorCaptured
  case CheckAnswered(mode: RelationCheckResponseMode)

private[chessjudgment] final case class PlanCausalResponseProof private (
    triggerStep: LineReplayStep,
    responseStep: LineReplayStep,
    reasons: List[PlanCausalResponseReason],
    legalResponseStep: LegalReplayStep,
    responseMovement: CanonicalRootLegalMove,
    responseFootprint: BoardTransitionFootprint,
    checkInventoryKey: Option[DerivedRelationResultKey]
):
  require(reasons.nonEmpty && reasons.distinct.size == reasons.size, "a causal response needs exact unique reasons")

  def proves(trigger: PlanCausalEventNode, step: LineReplayStep, plyOffset: Int): Boolean =
    trigger.step == triggerStep &&
      step == responseStep &&
      step.ply > trigger.step.ply &&
      plyOffset == step.ply - trigger.step.ply &&
      legalResponseStep.ply == step.ply &&
      EvidenceRef.sameMove(legalResponseStep.uci, step.moveUci) &&
      PrincipalVariationEvidence.sameBoardState(Fen.write(legalResponseStep.before).value, step.fenBefore) &&
      PrincipalVariationEvidence.sameBoardState(Fen.write(legalResponseStep.after).value, step.fenAfter) &&
      EvidenceRef.sameMove(responseMovement.moveUci, legalResponseStep.uci) &&
      responseMovement.side == legalResponseStep.move.piece.color &&
      responseMovement.from.key.equalsIgnoreCase(legalResponseStep.move.orig.key) &&
      responseMovement.to.key.equalsIgnoreCase(legalResponseStep.move.dest.key) &&
      responseMovement.beforeRole.name.equalsIgnoreCase(legalResponseStep.move.piece.role.name) &&
      responseFootprint.pieceTransitions.exists(movement =>
        movement.side == legalResponseStep.move.piece.color &&
          movement.from == legalResponseStep.move.orig &&
          movement.to == legalResponseStep.move.dest
      ) &&
      reasons.nonEmpty

private[chessjudgment] object PlanCausalResponseProof:
  def from(
      trigger: PlanCausalEventNode,
      response: LineReplayStep,
      triggerTransition: CanonicalReplayTransition,
      responseTransition: CanonicalReplayTransition
  ): Option[PlanCausalResponseProof] =
    for
      triggerLegal <- trigger.certifiedLegalStep
      if triggerTransition.declared == trigger.step && triggerTransition.legal == triggerLegal
      if responseTransition.declared == response
      legalResponse = responseTransition.legal
      if legalResponse.move.piece.color == !trigger.perspective
      rootMovement <- triggerTransition.boardFootprint.pieceTransitions.find(movement =>
        movement.side == trigger.perspective &&
          movement.from == triggerLegal.move.orig &&
          movement.to == triggerLegal.move.dest &&
          movement.beforeRole == triggerLegal.move.piece.role
      )
      captureReason = Option.when(
        legalResponse.move.captures &&
          legalResponse.capturedRole.contains(rootMovement.afterRole) &&
          responseTransition.boardFootprint.cellChanges.exists(change =>
            change.square == rootMovement.to &&
              change.before.contains(Piece(trigger.perspective, rootMovement.afterRole)) &&
              !change.after.contains(Piece(trigger.perspective, rootMovement.afterRole))
          )
      )(PlanCausalResponseReason.RootActorCaptured)
      checkWitnesses = exactCheckResponse(triggerTransition, responseTransition)
      reasons = captureReason.toList ++ checkWitnesses.toList.flatMap(_._2.modes.map(PlanCausalResponseReason.CheckAnswered.apply))
      if reasons.nonEmpty
      proof = PlanCausalResponseProof(
        trigger.step,
        response,
        reasons.distinct,
        legalResponse,
        responseTransition.relationDelta.rootMove,
        responseTransition.boardFootprint,
        checkWitnesses.map(_._1)
      )
      if proof.proves(trigger, response, response.ply - trigger.step.ply)
    yield proof

  private def exactCheckResponse(
      triggerTransition: CanonicalReplayTransition,
      responseTransition: CanonicalReplayTransition
  ): Option[(DerivedRelationResultKey, RelationCheckResponseWitness)] =
    val legalResponse = responseTransition.legal
    val matches = triggerTransition
      .verticalRelationsFor(VerticalRelationContractKind.CreatedCheckResponseInventory)
      .flatMap(relation =>
        RelationWitnessDetail.createdCheckResponse(relation.detail).toList.flatMap(detail =>
          detail.responses
            .filter(response =>
              detail.checkedSide == legalResponse.move.piece.color &&
                detail.terminal == RelationCheckTerminalState.Ongoing &&
                EvidenceRef.sameMove(response.resource.moveUci, legalResponse.uci) &&
                CaptureResponseFollowUpTrajectory.legalMovementMatches(response.resource.movement, legalResponse)
            )
            .map(response => DerivedRelationResultKey.from(relation) -> response)
        )
      )
    matches match
      case exact :: Nil => Some(exact)
      case _            => None

final case class PlanCausalResponse private[chessjudgment] (
    trigger: PlanCausalEventNode,
    step: LineReplayStep,
    plyOffset: Int,
    structuralConsequences: List[TransitionConsequence] = Nil,
    private[chessjudgment] val certificate: Option[PlanCausalResponseProof] = None
):
  def capturesPlanPiece: Boolean =
    certificate.exists(_.reasons.contains(PlanCausalResponseReason.RootActorCaptured))
  def answersCheck: Boolean =
    certificate.exists(_.reasons.exists {
      case PlanCausalResponseReason.CheckAnswered(_) => true
      case _                                          => false
    })
  def proven: Boolean =
    certificate.exists(_.proves(trigger, step, plyOffset))
  private[chessjudgment] def certifiedLegalStep: Option[LegalReplayStep] =
    certificate.filter(_.proves(trigger, step, plyOffset)).map(_.legalResponseStep)
  private[chessjudgment] def certifiedMovement: Option[CanonicalRootLegalMove] =
    certificate.filter(_.proves(trigger, step, plyOffset)).map(_.responseMovement)

object PlanCausalResponse:
  private[chessjudgment] def certified(
      trigger: PlanCausalEventNode,
      step: LineReplayStep,
      triggerTransition: CanonicalReplayTransition,
      responseTransition: CanonicalReplayTransition
  ): Option[PlanCausalResponse] =
    PlanCausalResponseProof.from(trigger, step, triggerTransition, responseTransition).map(proof =>
      PlanCausalResponse(
        trigger = trigger,
        step = step,
        plyOffset = step.ply - trigger.step.ply,
        certificate = Some(proof)
      )
    )

enum PlanCausalGoalMechanism:
  case PassedPawnConversion
  case PassedPawnManufacture
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
      sourceEvent.certifiedLegalStep.nonEmpty &&
      sourceTransition.actorRole.exists(_.name.equalsIgnoreCase(sourceEvent.identity.actor.beforeRole)) &&
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


object PlanCausalEpisode:
  private val PressureKinds = Set(
    TransitionConsequenceKind.BatteryFormation
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
      rootBeforeRole: EvidencePieceRole,
      pieceRole: EvidencePieceRole,
      futureAfterRole: EvidencePieceRole,
      color: Color,
      rootFrom: EvidenceSquare,
      rootTo: EvidenceSquare,
      futureFrom: EvidenceSquare,
      futureTo: EvidenceSquare
  ) extends PlanCausalDependencyFunctionProof:
    val kind = "object-state"
    val proofSquares = List(rootFrom, rootTo, futureFrom, futureTo)
    val proofPieceRoles = List(rootBeforeRole, pieceRole, futureAfterRole).distinct
    protected val identityParts = List(
      rootBeforeRole.name.toLowerCase,
      pieceRole.name.toLowerCase,
      futureAfterRole.name.toLowerCase,
      color.toString.toLowerCase,
      rootFrom.key.toLowerCase,
      rootTo.key.toLowerCase,
      futureFrom.key.toLowerCase,
      futureTo.key.toLowerCase
    )

  final case class LineAccess(
      enabledPieceRole: EvidencePieceRole,
      color: Color,
      vacatedSquares: List[EvidenceSquare],
      enabledFrom: EvidenceSquare,
      enabledTo: EvidenceSquare,
      accessRelationKey: String
  ) extends PlanCausalDependencyFunctionProof:
    require(vacatedSquares.nonEmpty, "a line-access function needs an exact vacated gate")
    val kind = "line-access"
    val proofSquares = vacatedSquares ++ List(enabledFrom, enabledTo)
    val proofPieceRoles = List(enabledPieceRole)
    protected val identityParts = List(
      enabledPieceRole.name.toLowerCase,
      color.toString.toLowerCase,
      PlanCausalProofKey.sequence(vacatedSquares.map(_.key.toLowerCase)),
      enabledFrom.key.toLowerCase,
      enabledTo.key.toLowerCase,
      accessRelationKey
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
      followUpTo: EvidenceSquare,
      recaptureInventoryKey: String
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
      followUpTo.key.toLowerCase,
      recaptureInventoryKey
    )

  final case class CheckFollowUp(
      triggerRole: EvidencePieceRole,
      responderRole: EvidencePieceRole,
      followUpRole: EvidencePieceRole,
      replyMoveUci: String,
      replyFrom: EvidenceSquare,
      replyTo: EvidenceSquare,
      followUpFrom: EvidenceSquare,
      followUpTo: EvidenceSquare,
      checkInventoryKey: String
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
      followUpTo.key.toLowerCase,
      checkInventoryKey
    )

  def from(dependency: PlanCausalEventDependency): PlanCausalDependencyFunctionProof =
    dependency.proof match
      case PlanCausalDependencyProof.ObjectState(trajectory) =>
        ObjectState(
          trajectory.rootBeforeRole,
          trajectory.pieceRole,
          trajectory.futureAfterRole,
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
          trajectory.vacatedSquares,
          trajectory.enabledFrom,
          trajectory.enabledTo,
          trajectory.accessRelationKey.stableKey
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
          trajectory.followUpTo,
          trajectory.recaptureInventoryKey.stableKey
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
          trajectory.followUpTo,
          trajectory.checkInventoryKey.stableKey
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

final case class PlanResultSourceOccurrence private[chessjudgment] (
    moveUci: String,
    plyOffset: Int,
    actor: PlanActorOccurrence
):
  def stableKey: String =
    List(EvidenceRef.normalizeMove(moveUci), plyOffset.toString, actor.stableKey).mkString("@")

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
        event.causalEpisode.root.identity.actor
      ),
      source = PlanResultSourceOccurrence(
        EvidenceRef.normalizeMove(assessment.sourceEvent.moveUci),
        assessment.sourcePlyOffset,
        assessment.sourceEvent.identity.actor
      ),
      selectedInducedResponse = selectedInducedResponse.flatMap(response =>
        response.certifiedMovement.map(movement =>
          PlanResultSourceOccurrence(
            EvidenceRef.normalizeMove(response.step.moveUci),
            response.step.ply - event.causalEpisode.root.step.ply,
            PlanActorOccurrence.certified(
              movement.side,
              movement.beforeRole.name,
              movement.afterRole.name,
              movement.from.key,
              movement.to.key,
              movement.fact.semanticId
            )
          )
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
    directGoalProofs: List[PlanCausalGoalProof],
    branchWitnesses: List[PlanCausalBranchWitness],
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
  require(
    directGoalProofs.distinct.size == directGoalProofs.size &&
      directGoalProofs.forall(proof =>
        proof.goalKind == causalEpisode.root.identity.kind &&
          proof.sourceTransition == rootTransition &&
          causalEpisode.root.structuralConsequences.contains(proof.consequence)
      ),
    "direct plan goals must retain their exact structural proof instead of being reconstructed from a label"
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
  def resultAdvancesGoal(consequence: TransitionConsequence): Boolean =
    directGoalProofs.exists(_.consequence == consequence)
  def directGoalConsequences: List[TransitionConsequence] =
    directGoalProofs.map(_.consequence).distinct
  def observedGoalResultRoutes: List[PlanCausalResultRoute] =
    episode.toList.flatMap(_.resultRoutes).distinct.sortBy(_.stableKey)
  def rootEnablingDependencies: List[PlanCausalEventDependency] =
    episode.toList.flatMap(causalEpisode =>
      causalEpisode.dependencies.filter(dependency => dependency.from == causalEpisode.root && dependency.enablesContinuation)
    )
  def historyEnablingDependencies: List[PlanCausalEventDependency] =
    episode.toList.flatMap(_.historyDependencies.filter(_.enablesContinuation))
  def observedRootEnablesContinuation: Boolean = episode.exists(_.rootEnablesContinuation)
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
  lazy val totalLegalReplyCount: Int =
    canonicalRootTransitionProof
      .filter(_.proves(rootTransition))
      .map(_.legalResponseCount)
      .getOrElse(0)
  private lazy val certifiedBranchResponseSteps: Option[List[LineReplayStep]] =
    val steps = branchWitnesses.flatMap(witness =>
      witness.canonicalReplay
        .flatMap(_.replaySteps.headOption)
        .filter(step => EvidenceRef.sameMove(step.moveUci, witness.line.rootMove))
    )
    Option.when(steps.size == branchWitnesses.size)(steps)
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
    totalLegalReplyCount > 0 &&
      canonicalRootTransitionProof
        .filter(_.proves(rootTransition))
        .exists(proof => certifiedBranchResponseSteps.exists(proof.certifiesCompleteLegalResponseSet)) &&
      branchWitnesses.map(_.line).distinct.size == branchWitnesses.size &&
      branchWitnesses.map(_.line.rootMove).distinct.size == branchWitnesses.size &&
      branchWitnesses.map(_.sourceProbeId).distinct.size == branchWitnesses.size &&
      branchWitnesses.map(_.certifiedHorizonPlyOffset).distinct.size == 1
  def branchCoverageComplete: Boolean =
    branchSetComplete &&
      causalResultAssessments.nonEmpty &&
      causalResultAssessments.forall(assessment =>
        assessment.robustness != PlanCausalRobustness.Untested &&
          assessment.robustness != PlanCausalRobustness.Deferred
      )
  def episodePublicProofReady: Boolean =
    observedRootEnablesContinuation &&
      branchCoverageComplete &&
      positiveGoalResultAssessments.nonEmpty
  def semanticGroupingAnchors: List[EvidenceSemanticAnchor] =
    List(
      EvidenceSemanticAnchor.of(EvidenceSemanticAnchorKind.Plan, planId.id),
      EvidenceSemanticAnchor.of(
        EvidenceSemanticAnchorKind.PlanCausalEvent,
        identity.goalKey,
        s"root:$rootMove",
        s"actor:${identity.actor.beforeRole}",
        s"proofs:${directGoalProofs.map(_.stableKey).sorted.mkString(",")}"
      )
    )

object PlanCausalGoalProof:
  private final case class ExactPlanActor(
      side: Color,
      beforeRole: String,
      afterRole: String,
      from: EvidenceSquare,
      to: EvidenceSquare
  )

  private object ExactPlanActor:
    def from(movement: CanonicalRootLegalMove): ExactPlanActor =
      ExactPlanActor(
        movement.side,
        movement.beforeRole.name,
        movement.afterRole.name,
        movement.from,
        movement.to
      )

    def from(actor: PlanActorOccurrence): ExactPlanActor =
      ExactPlanActor(
        actor.side,
        actor.beforeRole,
        actor.afterRole,
        EvidenceSquare(actor.from),
        EvidenceSquare(actor.to)
      )

  /** Closed projection from an exact structural result to the presentation
    * goals it can actually prove.  This replaces enum-wide plan guessing: a
    * new label cannot become a candidate until a concrete consequence family
    * and mechanism admit it here.
    */
  private[chessjudgment] def directCandidates(
      transition: StructuralTransitionBinding,
      consequence: TransitionConsequence,
      movement: CanonicalRootLegalMove
  ): List[PlanCausalGoalProof] =
    import TransitionConsequenceKind.*
    val eligibleKinds = consequence.kind match
      case PassedPawnProgress =>
        List(PlanKind.PasserConversion, PlanKind.PassedPawnManufacture)
      case _ => Nil
    eligibleKinds.flatMap(kind =>
      certify(kind.theme, Some(kind), transition, consequence, movement)
    )

  def certify(
      identity: PlanEventIdentity,
      transition: StructuralTransitionBinding,
      consequence: TransitionConsequence
  ): Option[PlanCausalGoalProof] =
    certifyExact(
      identity.goalTheme,
      Some(identity.kind),
      transition,
      consequence,
      ExactPlanActor.from(identity.actor)
    )

  def certify(
      goalTheme: PlanTheme,
      goalKind: Option[PlanKind],
      transition: StructuralTransitionBinding,
      consequence: TransitionConsequence,
      movement: CanonicalRootLegalMove
  ): Option[PlanCausalGoalProof] =
    certifyExact(goalTheme, goalKind, transition, consequence, ExactPlanActor.from(movement))

  private def certifyExact(
      goalTheme: PlanTheme,
      goalKind: Option[PlanKind],
      transition: StructuralTransitionBinding,
      consequence: TransitionConsequence,
      actor: ExactPlanActor
  ): Option[PlanCausalGoalProof] =
    for
      kind <- goalKind.filter(_.theme == goalTheme)
      mechanism <- directMechanism(kind, transition, consequence, actor)
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
      movement: CanonicalRootLegalMove
  ): Boolean =
    certify(goalTheme, goalKind, transition, consequence, movement).nonEmpty

  private def directMechanism(
      kind: PlanKind,
      transition: StructuralTransitionBinding,
      consequence: TransitionConsequence,
      actor: ExactPlanActor
  ): Option[PlanCausalGoalMechanism] =
    import TransitionConsequenceKind.*
    kind match
      case PlanKind.PasserConversion =>
        Option.when(
          consequence.kind == PassedPawnProgress &&
          transitionActorIs(transition, Pawn, actor) &&
          consequence.goalSubjectFacts.exists(
            passedPawnConversionBy(_, actor)
          )
        )(PlanCausalGoalMechanism.PassedPawnConversion)
      case PlanKind.PassedPawnManufacture =>
        Option.when(
          consequence.kind == PassedPawnProgress &&
          consequence.goalSubjectFacts.exists(
            passedPawnManufacture(_, actor)
          )
        )(PlanCausalGoalMechanism.PassedPawnManufacture)

  private def dependencyMechanism(
      plan: Plan,
      dependency: PlanCausalEventDependency,
      consequence: TransitionConsequence
  ): Option[PlanCausalGoalMechanism] =
    import TransitionConsequenceKind.*
    dependency.proof match
      case PlanCausalDependencyProof.ResponseContinuation(pawn: PawnBreakFollowUpTrajectory)
          if plan.theme == PlanTheme.AdvantageTransformation =>
        val resultSquares = PlanCausalEpisode.consequenceSquares(consequence).map(_.key.toLowerCase).toSet
        Option.when(
          pawn.kind == PawnBreakFollowUpKind.ReleasedPassedPawn &&
            consequence.kind == PassedPawnProgress &&
            (resultSquares(pawn.releasedPassedPawn.key.toLowerCase) ||
              resultSquares(pawn.followUpFrom.key.toLowerCase) ||
              resultSquares(pawn.followUpTo.key.toLowerCase))
        )(PlanCausalGoalMechanism.ReleasedPassedPawnContinuation)
      case _ => None

  private def transitionActorIs(
      transition: StructuralTransitionBinding,
      role: Role,
      actor: ExactPlanActor
  ): Boolean =
    actor.side == transition.perspective &&
      actor.beforeRole.equalsIgnoreCase(role.name) &&
      transition.actorRole.exists(_.name.equalsIgnoreCase(actor.beforeRole))

  private def passedPawnConversionBy(subject: StructuralSubject, actor: ExactPlanActor): Boolean =
    subject match
      case StructuralSubject.PassedPawnAdvanced(owner, from, to, rank) =>
        owner == actor.side &&
          actor.beforeRole.equalsIgnoreCase(Pawn.name) &&
          actor.afterRole.equalsIgnoreCase(Pawn.name) &&
          from == actor.from && to == actor.to && relativeRank(to, actor.side) == rank
      case StructuralSubject.PassedPawnPromoted(owner, from, to) =>
        owner == actor.side &&
          actor.beforeRole.equalsIgnoreCase(Pawn.name) &&
          !actor.afterRole.equalsIgnoreCase(Pawn.name) &&
          from == actor.from && to == actor.to && relativeRank(to, actor.side) == 8
      case _ => false

  private def passedPawnManufacture(subject: StructuralSubject, actor: ExactPlanActor): Boolean =
    subject match
      case StructuralSubject.PassedStatusCreated(owner, from, to, rank) =>
        owner == actor.side && from == actor.from && to == actor.to && relativeRank(to, actor.side) == rank
      case StructuralSubject.PassedPawnCreated(owner, _) =>
        owner == actor.side
      case _ => false

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
        payload.rootLine :: payload.branchWitnesses.map(_.line)
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
      case MoveTransitionEvidence(move, _, _, _) =>
        EvidenceRef.sameMove(move, moveUci)
      case relation: RelationFactEvidence =>
        relation.mentionsLineMove(moveUci)
      case mechanism: TacticalMechanismEvidence =>
        mechanism.moveUci.exists(EvidenceRef.sameMove(_, moveUci)) ||
          mechanism.line.exists(line => EvidenceRef.sameMove(line.rootMove, moveUci))
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
    relation: RelationFactEvidence,
    occurrenceOwner: Option[EvidenceRef]
):
  def ref: EvidenceRef = record.ref
  def participants: List[RelationParticipant] = relation.participants
  def semanticKey: String =
    List(
      CanonicalRelationGraph.positionOccurrenceKey(ref.position),
      occurrenceOwner.map(_.id)
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

  private[chessjudgment] def bindAbsence(
      certificate: PositionRelationExtractor.ClosedRelationAbsenceCertificate
  ): Option[PositionRelationExtractor.ClosedRelationAbsenceProof] =
    inventory.bindAbsence(certificate, position, scope)

  private[chessjudgment] def bindState(
      certificate: PositionRelationExtractor.ClosedPositionStateCertificate
  ): Option[PositionRelationExtractor.ClosedPositionStateProof] =
    inventory.bindState(certificate, position, scope)

final class CanonicalRelationGraph private (
    private val nodeInventory: Vector[CanonicalRelationNode],
    private val evidenceIndex: Map[String, CanonicalRelationNode],
    private val semanticIndex: Map[String, CanonicalRelationNode],
    private val positionRelationIndex: Map[
      (String, EvidenceScope),
      scala.collection.immutable.VectorMap[String, CanonicalRelationNode]
    ]
):
  lazy val nodes: List[CanonicalRelationNode] = nodeInventory.toList

  val byEvidenceId: Map[String, CanonicalRelationNode] = evidenceIndex

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

  private[chessjudgment] def positionRelationsBySemanticId(
      position: PositionNodeRef,
      semanticId: String
  ): List[CanonicalRelationNode] =
    EvidenceScope.values.toList.flatMap(scope =>
      positionRelationIndex
        .get(CanonicalRelationGraph.occurrenceKey(position) -> scope)
        .flatMap(_.get(semanticId))
    )

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
        val owner = relation.origin match
          case _: RelationEvidenceOrigin.LegalReplay =>
            canonical.parents.headOption.filter(parent =>
              parent.producer == EvidenceProducer.RelationProducer &&
                parent.layer == EvidenceLayer.Relation &&
                parent.confidence == EvidenceConfidence.LegalReplayVerified
            ).map(Some(_))
          case _ => Some(None)
        owner.map(exactOwner => CanonicalRelationNode(canonical, relation, exactOwner))
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
      positionRelationIndex = Map.empty
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
    var positionRelationIndex = graph.positionRelationIndex
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
              added += 1
    }

    if added == 0 then graph
    else
      new CanonicalRelationGraph(
        nodeInventory,
        evidenceIndex,
        semanticIndex,
        positionRelationIndex
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

  private[chessjudgment] def certifiedRootResponseMovesFor(
      line: LineNodeRef
  ): Option[List[String]] =
    structuralDeltasByLine.getOrElse(line, Nil) match
      case exact :: Nil
          if exact.line.contains(line) &&
            EvidenceRef.sameMove(exact.moveUci, line.rootMove) =>
        exact.certifiedRootResponseMoves
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
          RelativeCauseKind.strategicAxisCanProveCause(cause.kind, axis)
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
              RelativeCauseKind.strategicAxisCanProveCause(cause.kind, axis) =>
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
        case payload @ MoveTransitionEvidence(moveUci, from, to, _) =>
          exactAuthority(record, EvidenceProducer.MoveTransitionProducer, EvidenceLayer.MoveTransition) &&
            record.ref.confidence == EvidenceConfidence.LegalReplayVerified &&
            record.ref.line.isEmpty && record.ref.position == from && from != to &&
            EvidenceRef.normalizeMove(moveUci).nonEmpty &&
            payload.canonicalTransitionProof.exists(_.provesMove(moveUci, from, to, record.ref.scope))
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
      case EvidenceRecord(ref, MoveTransitionEvidence(moveUci, from, _, _), _) =>
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
    val hasRelationSignal = payload.signals.exists(_.kind == TacticalMechanismSignalKind.Relation)
    val exactRelationContract =
      (payload.relationLineProof, hasRelationSignal) match
        case (Some(proof), true) => TacticalRelationLineContract.certifiesProof(this, payload, proof)
        case (None, false)       => true
        case _                   => false
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
                              line.rootIsRecapture(rootMove))
                        )
                    )
                  case TacticalMechanismSignalKind.LineEvent =>
                    LineEventKind.values.find(_.toString == signal.label).exists(kind =>
                      line.lineEventsOf(kind).exists(event =>
                        event.plyOffset == 0 && EvidenceRef.sameMove(event.moveUci, rootMove) &&
                          (mechanism.kind != TacticalMechanismKind.DefensiveResource ||
                            (event.kind == LineEventKind.CheckEvasion &&
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
                exactClosedRelationBinding(binding, occurrence)
            }
          case _ => false
      case RelationEvidenceOrigin.Unverified => false

  private def closedRelationOccurrenceAncestryVerified(
      record: EvidenceRecord,
      payload: ClosedRelationOccurrenceEvidence
  ): Boolean =
    val expectedParents = payload.edge.evidence :: payload.lineEvidence.toList
    val exactTransition = byId.get(payload.edge.evidence.id).exists {
      case EvidenceRecord(ref, MoveTransitionEvidence(moveUci, from, to, _), parents) =>
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
            parents.toSet == (record.ref :: binding.sources).toSet
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

  /** Verifies the already-bound source identities without reproducing any
    * board relation or vertical derivation search.
    */
  private def exactClosedRelationBinding(
      binding: ClosedRelationOutputBinding,
      occurrence: ClosedRelationOccurrenceEvidence
  ): Boolean =
    val sourceRecords = binding.sources.flatMap(source =>
      byId.get(source.id).filter(record => record.ref == source && exactCanonicalRelationSource(source))
    )
    val sourceRelations = sourceRecords.collect {
      case record @ EvidenceRecord(_, relation: RelationFactEvidence, _) => record -> relation
    }
    val exactSources =
      sourceRecords.size == binding.sources.size && sourceRelations.size == sourceRecords.size

    final case class CombinationSourceKey(
        position: PositionNodeRef,
        kind: RelationFactKind,
        semanticId: String,
        stage: RelationProofStage
    )

    final case class VerticalSourceKey(
        position: PositionNodeRef,
        kind: RelationFactKind,
        assertionId: String,
        semanticId: String,
        stage: RelationProofStage
    )

    val combinationSourceEntries = sourceRelations.map { case value @ (record, relation) =>
      CombinationSourceKey(record.ref.position, relation.kind, relation.semanticId, relation.proofStage) -> value
    }
    val verticalSourceEntries = sourceRelations.map { case value @ (record, relation) =>
      VerticalSourceKey(
        record.ref.position,
        relation.kind,
        relation.assertionId,
        relation.semanticId,
        relation.proofStage
      ) -> value
    }
    val uniqueSourceKeys =
      combinationSourceEntries.map(_._1).distinct.size == combinationSourceEntries.size &&
        verticalSourceEntries.map(_._1).distinct.size == verticalSourceEntries.size
    val combinationSources = combinationSourceEntries.toMap
    val verticalSources = verticalSourceEntries.toMap

    def positionFor(premise: RelationCombinationPremise): PositionNodeRef =
      premise.occurrence match
        case RelationPremiseOccurrence.Before | RelationPremiseOccurrence.Removed => occurrence.edge.from
        case RelationPremiseOccurrence.After | RelationPremiseOccurrence.Established => occurrence.edge.to

    def verticalSourceMatches(
        record: EvidenceRecord,
        relation: RelationFactEvidence,
        premise: VerticalRelationPremise
    ): Boolean =
      val exactIdentity =
        relation.kind == premise.kind && relation.assertionId == premise.assertionId &&
          relation.semanticId == premise.semanticId
      exactIdentity && (premise.source match
        case VerticalRelationPremiseSource.Position(
              RelationPremiseOccurrence.Before | RelationPremiseOccurrence.Removed
            ) =>
          relation.proofStage == premise.stage && record.ref.position == occurrence.edge.from &&
            relation.isPositionRelation && !relation.hasLineProof
        case VerticalRelationPremiseSource.Position(
              RelationPremiseOccurrence.After | RelationPremiseOccurrence.Established
            ) =>
          relation.proofStage == premise.stage && record.ref.position == occurrence.edge.to &&
            relation.isPositionRelation && !relation.hasLineProof
        case VerticalRelationPremiseSource.RootTransition => false
        case VerticalRelationPremiseSource.Derived =>
          relation.proofStage == premise.stage && record.ref.position == occurrence.edge.from &&
            RelationProofStage.rank(relation.proofStage) >=
              RelationProofStage.rank(RelationProofStage.TransitionFact) &&
            occurrence.outputFor(record.ref).exists(_.relation == relation))

    def verticalSourceFor(
        premise: VerticalRelationPremise
    ): Option[(EvidenceRecord, RelationFactEvidence)] =
      val position = premise.source match
        case VerticalRelationPremiseSource.Position(
              RelationPremiseOccurrence.Before | RelationPremiseOccurrence.Removed
            ) => occurrence.edge.from
        case VerticalRelationPremiseSource.Position(
              RelationPremiseOccurrence.After | RelationPremiseOccurrence.Established
            ) => occurrence.edge.to
        case VerticalRelationPremiseSource.RootTransition => occurrence.edge.from
        case VerticalRelationPremiseSource.Derived => occurrence.edge.from
      verticalSources.get(
        VerticalSourceKey(
          position,
          premise.kind,
          premise.assertionId,
          premise.semanticId,
          premise.stage
        )
      )

    def rootTransitionMatches(premise: VerticalRelationPremise): Boolean =
      premise.source == VerticalRelationPremiseSource.RootTransition &&
        premise.stage == RelationProofStage.TransitionFact &&
        relationGraph.positionRelationsBySemanticId(
          occurrence.edge.from,
          premise.semanticId
        ).exists(node =>
          val relation = node.relation
          relation.kind == premise.kind && relation.assertionId == premise.assertionId &&
            relation.semanticId == premise.semanticId && relation.isPositionRelation &&
            !relation.hasLineProof && (relation.detail match
              case RelationWitnessDetail.LegalMove(_, _, _, _, moveUci, _) =>
                EvidenceRef.sameMove(moveUci, occurrence.edge.moveUci)
              case _ => false)
        )

    exactSources && uniqueSourceKeys && (binding.relation.proofStage match
      case RelationProofStage.TransitionFact =>
        RelationWitnessDetail.combinationProof(binding.relation.detail).exists { proof =>
          binding.absenceProofs.isEmpty && binding.stateProofs.isEmpty &&
            sourceRelations.size == proof.premises.size &&
            proof.premises.forall(premise =>
              combinationSources.contains(
                CombinationSourceKey(
                  positionFor(premise),
                  premise.kind,
                  premise.semanticId,
                  RelationProofStage.PositionFact
                )
              )
            )
        }
      case RelationProofStage.Vertical(_) =>
        VerticalRelationContracts.proofOf(binding.relation.detail).exists { proof =>
          val rootPremises = proof.sourcePremises.filter(
            _.source == VerticalRelationPremiseSource.RootTransition
          )
          val positionAndDerivedPremises = proof.sourcePremises.filterNot(
            _.source == VerticalRelationPremiseSource.RootTransition
          )
          val expectedSourceKeys = positionAndDerivedPremises.flatMap(premise =>
            verticalSourceFor(premise).map { case (record, relation) =>
              VerticalSourceKey(
                record.ref.position,
                relation.kind,
                relation.assertionId,
                relation.semanticId,
                relation.proofStage
              )
            }
          ).toSet
          val hasExactTransitionAnchor = proof.sourcePremises.exists(premise =>
            VerticalRelationPremiseSource.transitionAnchored(premise.source, premise.stage)
          )
          proof.proves(binding.relation.detail) &&
            verticalSourceEntries.map(_._1).toSet == expectedSourceKeys &&
            hasExactTransitionAnchor && rootPremises.forall(rootTransitionMatches) &&
            positionAndDerivedPremises.forall(premise =>
              verticalSourceFor(premise).exists { case (record, relation) =>
                verticalSourceMatches(record, relation, premise) &&
                  RelationProofStage.rank(relation.proofStage) < RelationProofStage.rank(binding.relation.proofStage)
              }
            ) &&
            binding.absenceProofs.map(_.premise) == proof.absences &&
            binding.absenceProofs.forall(_.ownedBy(occurrence.edge)) &&
            binding.stateProofs.map(_.premise) == proof.states &&
            binding.stateProofs.forall(_.ownedBy(occurrence.edge))
        }
      case RelationProofStage.PositionFact => false)

  private def structuralDeltaAncestryVerified(
      record: EvidenceRecord,
      payload: StructuralDeltaEvidence
  ): Boolean =
    val directParents = record.parents.flatMap(parent => byId.get(parent.id).filter(_.ref == parent))
    val directParentsById = directParents.map(parent => parent.ref.id -> parent).toMap
    val transitionParent = directParents.exists {
      case EvidenceRecord(ref, MoveTransitionEvidence(moveUci, from, to, _), _) =>
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
    val expectedDerivedKeys = payload.consequences.flatMap(_.derivedRelationKeys).toSet
    val exactDerivedSources =
      payload.derivedRelationSources.map(_.key).toSet == expectedDerivedKeys &&
        payload.derivedRelationSources.forall { source =>
          directParentsById.get(source.source.id).exists {
            case EvidenceRecord(ref, relation: RelationFactEvidence, parents) =>
              ref == source.source && (relation.proofStage match
                case RelationProofStage.TransitionFact | RelationProofStage.Vertical(_) =>
                  DerivedRelationResultKey.from(relation) == source.key
                case _ => false) &&
                relation.mentionsLineMove(payload.moveUci) && relationGraph.contains(ref, relation) &&
                parents.exists(parent => byId.get(parent.id).exists {
                  case EvidenceRecord(_, occurrence: ClosedRelationOccurrenceEvidence, _) =>
                    occurrence.edge.from == payload.from && occurrence.edge.to == payload.to &&
                      EvidenceRef.sameMove(occurrence.edge.moveUci, payload.moveUci) &&
                      occurrence.outputFor(ref).exists(_.relation == relation)
                  case _ => false
                })
            case _ => false
          }
        }
    record.ref.producer == EvidenceProducer.StructuralDeltaProducer &&
      record.ref.layer == EvidenceLayer.StructuralDelta &&
      record.ref.confidence == EvidenceConfidence.BoardDerived &&
      record.ref.position == payload.from &&
      transitionParent &&
      positionParent(payload.from) &&
      positionParent(payload.to) &&
      exactDerivedSources &&
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
