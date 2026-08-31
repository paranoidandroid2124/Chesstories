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
  BoardPieceTransition,
  BoardTransitionFootprint,
  PositionOccurrenceState
}
import lila.chessjudgment.model.{
  PassedPawnResultActorOccurrence,
  PassedPawnResultEventIdentity,
  PassedPawnResultEventOccurrence,
  PassedPawnResultPositionOccurrence
}
import lila.chessjudgment.model.PassedPawnResultKind

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
  case PassedPawnResultKind
  case Relation
  case CandidateComparison
  case PassedPawnResultEvent
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
  case PassedPawnSubject
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
  case Maintained
  case Lost

/** Exact typed proof owned by one root-causal effect. */
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
  case ForcedReplyResourceDifferential(
      source: EvidenceRef,
      result: ForcedReplyResourceDifferentialEvidence
  )
  case PassedPawnResult(
      source: EvidenceRef,
      result: PassedPawnResultProofEvidence
  )

  /** Evidence record that owns the effect. */
  final def primitiveSource: EvidenceRef =
    this match
      case RootOwnedEffectProof.LineEpisode(source, _, _)                 => source
      case RootOwnedEffectProof.RootLineEvent(source, _, _)               => source
      case RootOwnedEffectProof.RootRelation(source, _)                    => source
      case RootOwnedEffectProof.ForcedReplyResourceDifferential(source, _) => source
      case RootOwnedEffectProof.PassedPawnResult(source, _)               => source

/** Primitive family of the exact effect owned by one public Cause channel.
  * This is deliberately independent of evidence ids and carrier wrappers.
  */
enum RootOwnedEffectPrimitiveKind:
  case Unspecified
  case LineEpisode
  case RootLineEvent
  case RootRelation
  case CausalResourceDifferential
  case PassedPawnResult

/** Comparison-safe identity of a root-owned effect. Importance and fallback
  * dominance may compare effects only when this complete scope is equal.
  */
final case class RootOwnedEffectIdentity(
    primitiveKind: RootOwnedEffectPrimitiveKind,
    targetSignatures: List[String],
    passedPawnResultKindIds: List[String],
    passedPawnResult: Option[PassedPawnResultSemanticIdentity] = None,
    causalProofId: Option[String] = None
):
  def stableKey: String =
    List(
      primitiveKind.toString.toLowerCase,
      targetSignatures.mkString("[", ",", "]"),
      Option.unless(passedPawnResult.nonEmpty)(passedPawnResultKindIds).getOrElse(Nil).mkString("[", ",", "]"),
      passedPawnResult.map(_.stableKey).getOrElse("none"),
      causalProofId.getOrElse("none")
    ).mkString("|")

object RootOwnedEffectIdentity:
  val unscoped: RootOwnedEffectIdentity =
    RootOwnedEffectIdentity(RootOwnedEffectPrimitiveKind.Unspecified, Nil, Nil)

/** A typed magnitude is diagnostic only until its complete effect identity is
  * known. Different magnitude cases are never interchangeable.
  */
enum DirectCauseImportanceMeasure:
  case MateArrival(plyOffset: Int)
  case MaterialOutcome(durableNetCp: Int, onsetPlyOffset: Int)

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
    List(
      identity.stableKey,
      magnitudeKey,
      materialEventSalience.map(_.stableKey).getOrElse("none")
    ).map(RootOwnedEffectDescriptor.atom).mkString

  /** Taxonomy labels remain available to endpoint enumeration, but R compares
    * exact PassedPawnResults by their owned source/result/route identity.
    */
  private[judgment] def semanticAgreementDescriptor: RootOwnedEffectDescriptor =
    if identity.passedPawnResult.nonEmpty then copy(identity = identity.copy(passedPawnResultKindIds = Nil))
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
  * absent. A typed PassedPawnResult carrier remains a distinct proof because it
  * changes the effect descriptor; tactical carriers may resolve to the bare
  * primitive.
  */
final case class ComparisonEndpointEvidenceWitness(
    sourceSide: RelativeCauseSourceSide,
    line: LineNodeRef,
    binding: EvidenceObjectBinding,
    rootOwnedProof: RootOwnedEffectProof,
    proofSegment: Option[DirectCauseProofSegment],
    effectDescriptor: RootOwnedEffectDescriptor,
    carrierAncestorSourceIds: List[String],
    observation: Option[ComparisonEndpointEffectObservation]
):
  def primitiveProofSource: EvidenceRef = rootOwnedProof.primitiveSource
  def carrier: EvidenceRef = binding.source
  def provenance: List[EvidenceRef] = binding.provenance

private[chessjudgment] final case class ComparisonEndpointEvidenceSideSnapshot(
    sourceSide: RelativeCauseSourceSide,
    line: LineNodeRef,
    witnesses: List[ComparisonEndpointEvidenceWitness],
    lineInventories: Map[ComparisonEndpointLineProofFamily, ComparisonEndpointEffectInventory],
    passedPawnResultInventory: Option[PassedPawnResultEndpointInventory]
)

private[chessjudgment] final case class ComparisonEndpointEvidenceSnapshot(
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

private[chessjudgment] final case class PassedPawnResultEndpointInventory(
    observations: Set[ComparisonEndpointEffectObservation],
    enumeratedPassedPawnResultKindIds: Set[String],
    incompleteResultIdentities: Set[PassedPawnResultSemanticIdentity],
    extractionReady: Boolean
):
  def uniqueObservationFor(
      scope: ComparisonEndpointEffectScopeKey
  ): Option[Option[ComparisonEndpointEffectObservation]] =
    (scope.effectIdentity.passedPawnResultKindIds.distinct, scope.effectIdentity.passedPawnResult) match
      case (passedPawnResultKind :: Nil, Some(identity))
          if extractionReady && enumeratedPassedPawnResultKindIds(passedPawnResultKind) && !incompleteResultIdentities(identity) =>
        ComparisonEndpointEffectObservationPolicy.uniqueObservationFor(
          ComparisonEndpointEffectInventory.Complete(observations),
          scope
        )
      case _ =>
        None

private[chessjudgment] enum ComparisonEndpointLineProofFamily:
  case LineEpisodeMaterial
  case LineEpisodeMate
  case LineEpisodeQualitative
  case RootLineEventMate
  case RootLineEventQualitative

private[chessjudgment] object ComparisonEndpointLineProofFamily:
  def fromChannel(channel: DirectCauseChannel): Option[ComparisonEndpointLineProofFamily] =
    for
      proof <- channel.rootOwnedProof
      descriptor <- channel.rootOwnedEffectDescriptor
      family <- from(proof, descriptor)
    yield family

  def from(
      proof: RootOwnedEffectProof,
      descriptor: RootOwnedEffectDescriptor
  ): Option[ComparisonEndpointLineProofFamily] =
    proof match
      case RootOwnedEffectProof.LineEpisode(_, _, _) =>
        descriptor.magnitude match
          case DirectEffectMagnitudeKnowledge.Exact(
                DirectCauseImportanceMeasure.MaterialOutcome(_, _)
              ) => Some(ComparisonEndpointLineProofFamily.LineEpisodeMaterial)
          case DirectEffectMagnitudeKnowledge.Exact(DirectCauseImportanceMeasure.MateArrival(_)) =>
            Some(ComparisonEndpointLineProofFamily.LineEpisodeMate)
          case DirectEffectMagnitudeKnowledge.NotApplicable =>
            Some(ComparisonEndpointLineProofFamily.LineEpisodeQualitative)
          case DirectEffectMagnitudeKnowledge.Exact(_) |
              DirectEffectMagnitudeKnowledge.ExpectedButMissing |
              DirectEffectMagnitudeKnowledge.Ambiguous => None
      case RootOwnedEffectProof.RootLineEvent(_, _, event) =>
        Some(
          if event.kind == LineEventKind.Mate then
            ComparisonEndpointLineProofFamily.RootLineEventMate
          else ComparisonEndpointLineProofFamily.RootLineEventQualitative
        )
      case _ => None

private[chessjudgment] enum ComparisonEndpointWitnessDemandMode:
  case MembershipOnly
  case DifferentialClosure

private[chessjudgment] enum ComparisonEndpointWitnessFamily:
  case Line(family: ComparisonEndpointLineProofFamily)
  case Primitive(kind: RootOwnedEffectPrimitiveKind)

private[chessjudgment] final case class ComparisonEndpointWitnessDemandEntry private (
    sourceSide: RelativeCauseSourceSide,
    mode: ComparisonEndpointWitnessDemandMode,
    family: ComparisonEndpointWitnessFamily,
    carrierSource: EvidenceRef,
    exactOccurrence: RootOwnedEffectChannelOccurrenceFingerprint
):
  private[chessjudgment] def stableSortKey: String =
    List(
      sourceSide.toString,
      mode.toString,
      family.toString,
      carrierSource.id,
      exactOccurrence.stableSortKey
    ).mkString("|")

private[chessjudgment] object ComparisonEndpointWitnessDemandEntry:
  def fromChannel(
      sourceSide: RelativeCauseSourceSide,
      mode: ComparisonEndpointWitnessDemandMode,
      channel: DirectCauseChannel
  ): Option[ComparisonEndpointWitnessDemandEntry] =
    for
      _ <- Option.when(
        sourceSide == RelativeCauseSourceSide.Reference ||
          sourceSide == RelativeCauseSourceSide.Candidate
      )(())
      proof <- channel.rootOwnedProof
      descriptor <- channel.rootOwnedEffectDescriptor
      family <- ComparisonEndpointLineProofFamily
        .from(proof, descriptor)
        .map(ComparisonEndpointWitnessFamily.Line.apply)
        .orElse(
          Option
            .unless(descriptor.identity.primitiveKind == RootOwnedEffectPrimitiveKind.Unspecified)(
              ComparisonEndpointWitnessFamily.Primitive(descriptor.identity.primitiveKind)
            )
        )
    yield ComparisonEndpointWitnessDemandEntry(
      sourceSide,
      mode,
      family,
      channel.binding.source,
      channel.exactOccurrenceFingerprint
    )

/** Exact family manifest issued from active typed Cause channels. It limits
  * neutral endpoint projection to the lower proof families actually consumed
  * by this comparison; carrier ids retain wrapper occurrence ownership.
  */
private[chessjudgment] final case class ComparisonEndpointWitnessDemand private (
    entries: List[ComparisonEndpointWitnessDemandEntry]
):
  def nonEmpty: Boolean = entries.nonEmpty

  def entriesForEndpoint(
      side: RelativeCauseSourceSide
  ): List[ComparisonEndpointWitnessDemandEntry] =
    entries.filter(entry =>
      entry.sourceSide == side || entry.mode == ComparisonEndpointWitnessDemandMode.DifferentialClosure
    )

  def hasClosureForEndpoint(side: RelativeCauseSourceSide): Boolean =
    entriesForEndpoint(side).exists(_.mode == ComparisonEndpointWitnessDemandMode.DifferentialClosure)

  def sourceEntries(
      side: RelativeCauseSourceSide
  ): List[ComparisonEndpointWitnessDemandEntry] =
    entries.filter(_.sourceSide == side)

  def closes(family: ComparisonEndpointWitnessFamily): Boolean =
    entries.exists(entry =>
      entry.mode == ComparisonEndpointWitnessDemandMode.DifferentialClosure && entry.family == family
    )

  def lineFamiliesForEndpoint(
      side: RelativeCauseSourceSide
  ): Set[ComparisonEndpointLineProofFamily] =
    entriesForEndpoint(side).flatMap {
      case ComparisonEndpointWitnessDemandEntry(_, _, ComparisonEndpointWitnessFamily.Line(family), _, _) =>
        Some(family)
      case _ => None
    }.toSet

  def requiresForEndpoint(
      side: RelativeCauseSourceSide,
      kind: RootOwnedEffectPrimitiveKind
  ): Boolean =
    entriesForEndpoint(side).exists(
      _.family == ComparisonEndpointWitnessFamily.Primitive(kind)
    )

  def demandedProofs(
      side: RelativeCauseSourceSide,
      family: ComparisonEndpointWitnessFamily
  ): Set[RootOwnedEffectProof] =
    sourceEntries(side)
      .filter(_.family == family)
      .flatMap(_.exactOccurrence.rootOwnedProof)
      .toSet

  def carrierSourceIds(side: RelativeCauseSourceSide): Set[String] =
    sourceEntries(side).map(_.carrierSource.id).toSet

  def demandedSourceIds: Set[String] =
    entries.flatMap(sourceIdsFor).toSet

  def demandedSourceIds(side: RelativeCauseSourceSide): Set[String] =
    sourceEntries(side).flatMap(sourceIdsFor).toSet

  private def sourceIdsFor(entry: ComparisonEndpointWitnessDemandEntry): List[String] =
    entry.carrierSource.id :: entry.exactOccurrence.rootOwnedProof.toList.flatMap {
      case RootOwnedEffectProof.PassedPawnResult(source, result) =>
        List(source.id, result.eventSource.id)
      case proof =>
        List(proof.primitiveSource.id)
    }

private[chessjudgment] object ComparisonEndpointWitnessDemand:
  def fromEntries(
      entries: List[ComparisonEndpointWitnessDemandEntry]
  ): ComparisonEndpointWitnessDemand =
    ComparisonEndpointWitnessDemand(entries.distinct.sortBy(_.stableSortKey))

private[chessjudgment] final case class ComparisonEndpointPassedPawnResultClosureInput(
    lineRefIds: Set[String],
    structuralRefIds: Set[String],
    extractionReady: Boolean
)

private[chessjudgment] final case class ComparisonEndpointEvidenceProjection(
    witnesses: List[ComparisonEndpointEvidenceWitness],
    lineInventories: Map[ComparisonEndpointLineProofFamily, ComparisonEndpointEffectInventory],
    passedPawnResultInventory: Option[PassedPawnResultEndpointInventory]
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
  case RealizesPassedPawnResult

final case class DirectCauseProofStep(
    plyOffset: Int,
    moveUci: String,
    role: DirectCauseProofStepRole,
    passedPawnResultEventOccurrence: Option[PassedPawnResultEventIdentity] = None
):
  require(plyOffset >= 0, "a Cause proof step needs a root-relative ply offset")
  require(moveUci.nonEmpty, "a Cause proof step needs an exact move")
  require(
    passedPawnResultEventOccurrence.forall(event =>
      EvidenceRef.sameMove(event.rootMove, moveUci)
    ),
    "a Cause proof step's passed-pawn-result event occurrence must identify the same move occurrence"
  )

  private[judgment] def stableKey: String =
    List(
      plyOffset.toString,
      EvidenceRef.normalizeMove(moveUci),
      role.toString.toLowerCase,
      passedPawnResultEventOccurrence.map(_.stableKey).getOrElse("none")
    ).map(DirectCauseProofSegment.atom).mkString

/** A compact sentence-ready view of moves owned by one direct proof. Missing
  * or unsafe step extraction yields no segment; it never makes the Cause itself
  * disappear. The terminal relation applies to the final ordered step.
  */
final case class DirectCauseProofSegment(
    terminalRelation: DirectCauseProofTerminalRelation,
    steps: List[DirectCauseProofStep],
    passedPawnResultDependencies: List[PassedPawnResultDependency] = Nil
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
    passedPawnResultDependencies.isEmpty ||
      (passedPawnResultDependencies.forall(_.causalConnectionProven) &&
        passedPawnResultDependencies.sliding(2).forall {
          case List(left, right) => left.to == right.from
          case _                 => true
        }),
    "a Cause proof segment's passed-pawn-result dependency occurrences must form one exact proven path"
  )

  private[judgment] def stableKey: String =
    List(
      terminalRelation.toString.toLowerCase,
      steps.map(_.stableKey).mkString,
      passedPawnResultDependencies.map(_.stableKey).mkString
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
      case RootOwnedEffectProof.ForcedReplyResourceDifferential(_, _) =>
        Nil
      case RootOwnedEffectProof.PassedPawnResult(_, result) =>
        passedPawnResults(result)

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
    val eventOccurrence = episode.eventOccurrence
    val replayPrefix = line.lineReplaySteps.take(eventOccurrence.plyOffset + 1)
    val rootPly = replayPrefix.headOption.map(_.ply)
    val exactReplay = replayPrefix.flatMap(step =>
      for
        root <- rootPly
        move <- exactMove(step.moveUci)
      yield (step.ply - root) -> move
    )
    val exactChain = episode.chain.flatMap(occurrence =>
      exactMove(occurrence.moveUci).map(occurrence.plyOffset -> _)
    )
    Option
      .when(
        episode.line == line.line &&
          replayPrefix.size == eventOccurrence.plyOffset + 1 &&
          exactReplay.size == replayPrefix.size &&
          exactChain.size == episode.chain.size &&
          exactReplay.size == exactChain.size &&
          exactReplay.map(_._1) == (0 to eventOccurrence.plyOffset).toList &&
          exactReplay.zip(exactChain).forall { case ((leftPly, leftMove), (rightPly, rightMove)) =>
            leftPly == rightPly && EvidenceRef.sameMove(leftMove, rightMove)
          } &&
          exactReplay.headOption.exists((_, move) => EvidenceRef.sameMove(move, episode.actor.moveUci)) &&
          exactReplay.lastOption.exists((ply, move) => episode.consequence.eventMatches(ply, move))
      )(
        DirectCauseProofSegment(
          DirectCauseProofTerminalRelation.ProducesLineConsequence,
          causalSteps(exactReplay)
        )
      )

  private def passedPawnResults(
      result: PassedPawnResultProofEvidence
  ): List[DirectCauseProofSegment] =
    val event = result.event
    val assessment = result.assessment
    val dependencyPath = assessment.causalPath
    val path = event.causalEpisode.root :: dependencyPath.map(_.to)
    val rootPly = event.causalEpisode.root.step.ply
    val extractedPath = path.flatMap(node =>
      exactMove(node.moveUci).map(move =>
        (node.step.ply - rootPly, move, Some(node.identity))
      )
    )
    val extracted = extractedPath.sortBy(_._1)
    Option.when(
      dependencyPath.nonEmpty &&
        path.nonEmpty &&
        extractedPath.size == path.size &&
          path.head == event.causalEpisode.root &&
        path.last == assessment.sourceEvent &&
        assessment.sourcePlyOffset == assessment.sourceEvent.step.ply - rootPly &&
        extracted.map(_._1) == extracted.map(_._1).distinct.sorted &&
        extracted.headOption.exists(_._1 == 0)
    )(
      DirectCauseProofSegment(
        DirectCauseProofTerminalRelation.RealizesPassedPawnResult,
        extracted.zipWithIndex.map { case ((offset, move, resultEvent), index) =>
          DirectCauseProofStep(
            offset,
            move,
            stepRole(index, extracted.size),
            passedPawnResultEventOccurrence = resultEvent
          )
        },
        passedPawnResultDependencies = dependencyPath
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
      case _: RootOwnedEffectProof.ForcedReplyResourceDifferential => "forced-reply-resource-differential"
      case _: RootOwnedEffectProof.PassedPawnResult                 => "passed-pawn-result"
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

/** Evidence-id-independent agreement carried by one exposure-selected direct channel.
  *
  * This is the shared agreement boundary for C record merging and R
  * cross-comparison representative selection. Evidence ids, support counts,
  * comparison rank, and serialization carriers are deliberately absent.
  */
final case class RootOwnedEffectChannelAgreementFingerprint(
    causalSignature: String,
    explanatoryNoveltySignature: String,
    descriptor: Option[RootOwnedEffectDescriptor],
    descriptorAmbiguous: Boolean
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

final case class RootOwnedEffectAgreementView(
    channels: List[RootOwnedEffectChannelAgreementFingerprint]
):
  private[judgment] def agreesCausallyWith(other: RootOwnedEffectAgreementView): Boolean =
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

object RootOwnedEffectAgreementView:
  def from(channels: List[DirectCauseChannel]): RootOwnedEffectAgreementView =
    val projected = EvidenceObjectBinding.projectCauseChannelsForExposure(channels)
    RootOwnedEffectAgreementView(
      projected.map(channel =>
        RootOwnedEffectChannelAgreementFingerprint(
          causalSignature = channel.causalSignature,
          explanatoryNoveltySignature = channel.explanatoryNoveltySignature,
          descriptor = channel.rootOwnedEffectDescriptor.map(_.semanticAgreementDescriptor),
          descriptorAmbiguous = channel.importanceDescriptorAmbiguous
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
    passedPawnResultDependencies: List[PassedPawnResultDependency] = Nil,
    passedPawnResultProof: Option[PassedPawnResultTransitionProof] = None
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
    val dependencyPart = passedPawnResultDependencies.map(dependency => s"dependency=${dependency.stableKey}")
    val passedPawnResultProofPart = passedPawnResultProof.map(proof => s"passed-pawn-result-transition-proof=${proof.stableKey}").toList
    (parts ++ linePart ++ horizonPart ++ proofPart ++ occurrencePart ++ dependencyPart ++ passedPawnResultProofPart)
      .mkString("|")

  private[chessjudgment] def occurrenceSignature: String =
    def atom(value: String): String = s"${value.length}:$value"
    val provenanceIds = provenance.map(_.id).distinct.sorted
    s"$signature|source=${atom(source.id)}|provenance=${provenanceIds.map(atom).mkString("[", ",", "]")}"

/** Single semantic authority for the exact effect scope and measurable
  * magnitude carried by a root-owned proof. It deliberately ignores evidence
  * ids while retaining causal targets, passed-pawn-result identity, and complete
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
    projectCauseChannelsForExposure(
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
    projectCauseChannelsForExposure(
      directCauseChannelsForProjection(cause, graph)
        .filter { channel =>
          val objectBinding = channel.binding
          RootOwnedEffectPolicy.admits(cause, graph, channel) &&
            directSourceIds(objectBinding.source.id) &&
            objectBinding.proofRole.contains(RelativeCauseProofRole.DirectProof)
        }
    )

  private final case class ComparisonEndpointLineProjection(
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
      rootPosition: PositionNodeRef,
      family: ComparisonEndpointLineProofFamily,
      acceptedProofs: Option[Set[RootOwnedEffectProof]] = None
  ): Option[List[ComparisonEndpointLineProjection]] =
    def accepted(proof: RootOwnedEffectProof): Boolean =
      acceptedProofs.forall(_(proof))

    val rootEpisodes = payload
      .rootOwnedCausalEpisodes(payload.line.rootMove)
      .filter(episode => lineEpisodeFamily(episode.consequence.kind).contains(family))
    val episodeProjected = rootEpisodes.flatMap { episode =>
        val proof = RootOwnedEffectProof.LineEpisode(source, payload, episode)
        val binding = rootOwnedEpisodeBinding(source, payload, episode)
        Option.when(accepted(proof))(
          ComparisonEndpointLineProjection(
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
        )
      }
    val actor = RootCausalActor.fromLineFact(payload, payload.line.rootMove)
    val eventProjected = actor.toList.flatMap { rootActor =>
      payload
        .eventsForRootMove(payload.line.rootMove)
        .filter(event => rootLineEventFamily(event.kind).contains(family))
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
        .flatMap { event =>
          val binding = rootLocalEventBinding(source, payload, rootActor, event)
          val proof = RootOwnedEffectProof.RootLineEvent(source, payload, event)
          Option.when(accepted(proof))(
            ComparisonEndpointLineProjection(
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
          )
        }
    }
    actor.map(_ => episodeProjected ++ eventProjected)

  private def lineEpisodeFamily(
      kind: LineConsequenceKind
  ): Option[ComparisonEndpointLineProofFamily] =
    kind match
      case LineConsequenceKind.MaterialGain | LineConsequenceKind.MaterialLoss =>
        Some(ComparisonEndpointLineProofFamily.LineEpisodeMaterial)
      case LineConsequenceKind.Mate =>
        Some(ComparisonEndpointLineProofFamily.LineEpisodeMate)
      case LineConsequenceKind.ImmediateReplyCheck | LineConsequenceKind.DrawResource |
          LineConsequenceKind.RecaptureSequence | LineConsequenceKind.RecoveryWindow |
          LineConsequenceKind.Promotion =>
        Some(ComparisonEndpointLineProofFamily.LineEpisodeQualitative)
      case _ => None

  private def rootLineEventFamily(
      kind: LineEventKind
  ): Option[ComparisonEndpointLineProofFamily] =
    kind match
      case LineEventKind.Mate =>
        Some(ComparisonEndpointLineProofFamily.RootLineEventMate)
      case LineEventKind.Capture | LineEventKind.Recapture | LineEventKind.Check |
          LineEventKind.Promotion | LineEventKind.CheckEvasion =>
        Some(ComparisonEndpointLineProofFamily.RootLineEventQualitative)
      case _ => None

  /** Enumerates neutral F-stage witnesses for one comparison endpoint. The
    * supplied records are the assembler's exact endpoint neighborhood; this
    * method never broadens ownership by scanning an unrelated graph branch.
    */
  private[chessjudgment] def comparisonEndpointEvidenceProjection(
      sourceSide: RelativeCauseSourceSide,
      eventLine: LineNodeRef,
      rootPosition: PositionNodeRef,
      endpointRecords: List[EvidenceRecord],
      involvedRecords: List[EvidenceRecord],
      demand: ComparisonEndpointWitnessDemand,
      passedPawnResultClosureInput: Option[ComparisonEndpointPassedPawnResultClosureInput],
      graph: TypedEvidenceGraph
  ): ComparisonEndpointEvidenceProjection =
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
        case Some(selected) if availableSegments.contains(selected) => List(Some(selected))
        case Some(_)                                                => Nil
        case None if availableSegments.nonEmpty                     => availableSegments.map(Some(_))
        case None                                                   => List(None)
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

    val lineFamilies = demand.lineFamiliesForEndpoint(sourceSide)
    val lineProjectionByFamily = endpointRecords.collectFirst {
      case EvidenceRecord(ref, payload: LineFactEvidence, _)
          if payload.line == eventLine &&
            RootOwnedEffectPolicy.sameCausalRootOccurrence(ref.position, rootPosition) =>
        lineFamilies.toList.sortBy(_.toString).map { family =>
          val demandFamily = ComparisonEndpointWitnessFamily.Line(family)
          val acceptedProofs =
            Option.unless(demand.closes(demandFamily))(
              demand.demandedProofs(sourceSide, demandFamily)
            )
          family -> comparisonEndpointLineProjections(
            ref,
            payload,
            rootPosition,
            family,
            acceptedProofs
          )
        }.toMap
    }.getOrElse(lineFamilies.map(_ -> None).toMap)
    val lineWitnesses = lineProjectionByFamily.values.toList.flatMap(_.toList.flatten).flatMap(item =>
      witness(item.binding, item.proof, item.observation)
    )
    val lineInventories = lineProjectionByFamily.collect {
      case (family, projected) if demand.closes(ComparisonEndpointWitnessFamily.Line(family)) =>
        family -> projected
          .map(items => completeEndpointInventory(items.map(_.observation)))
          .getOrElse(ComparisonEndpointEffectInventory.Incomplete)
    }

    val relationFamily = ComparisonEndpointWitnessFamily.Primitive(
      RootOwnedEffectPrimitiveKind.RootRelation
    )
    val demandedRelations = demand.demandedProofs(sourceSide, relationFamily)
    val relationWitnesses = endpointRecords.flatMap {
      case EvidenceRecord(ref, payload: RelationFactEvidence, _)
          if demand.requiresForEndpoint(sourceSide, RootOwnedEffectPrimitiveKind.RootRelation) &&
            RootOwnedEffectPolicy.relationRecordOwnsEventRoot(graph, ref, payload, eventLine) =>
        actor.toList.flatMap { exactActor =>
          val proof = RootOwnedEffectProof.RootRelation(ref, payload)
          val binding = rootRelationBinding(ref, payload, exactActor, eventLine)
          Option
            .when(demand.closes(relationFamily) || demandedRelations(proof))(
              witness(
                binding,
                proof,
                observationFromOwnedProof(binding, proof, DirectCausalChange.Occurred)
              )
            )
            .toList
            .flatten
        }
      case _ => Nil
    }

    val causalFamily = ComparisonEndpointWitnessFamily.Primitive(
      RootOwnedEffectPrimitiveKind.CausalResourceDifferential
    )
    val demandedCausalProofs = demand.demandedProofs(sourceSide, causalFamily)
    val causalWitnesses = endpointRecords.flatMap {
      case EvidenceRecord(ref, payload: ForcedReplyResourceDifferentialEvidence, _)
          if demand.requiresForEndpoint(
            sourceSide,
            RootOwnedEffectPrimitiveKind.CausalResourceDifferential
          ) &&
            payload.occurrence.referenceLine == eventLine &&
            sourceSide == RelativeCauseSourceSide.Reference =>
        actor.toList.flatMap { exactActor =>
          val proof = RootOwnedEffectProof.ForcedReplyResourceDifferential(ref, payload)
          val binding = forcedReplyResourceBinding(ref, payload, exactActor, eventLine)
          Option
            .when(demand.closes(causalFamily) || demandedCausalProofs(proof))(
              witness(
                binding,
                proof,
                observationFromOwnedProof(binding, proof, DirectCausalChange.Occurred)
              )
            )
            .toList
            .flatten
        }
      case _ => Nil
    }

    val passedPawnResultFamily = ComparisonEndpointWitnessFamily.Primitive(
      RootOwnedEffectPrimitiveKind.PassedPawnResult
    )
    val closesPassedPawnResults = demand.closes(passedPawnResultFamily)
    val demandedPassedPawnResultProofs = demand.demandedProofs(sourceSide, passedPawnResultFamily)
    val passedPawnResultEventProbes = endpointRecords.collect {
      case record @ EvidenceRecord(ref, payload: PassedPawnResultEventEvidence, _)
          if demand.requiresForEndpoint(sourceSide, RootOwnedEffectPrimitiveKind.PassedPawnResult) &&
            RootOwnedEffectPolicy.sameCausalRootOccurrence(ref.position, rootPosition) &&
            ref.line.contains(eventLine) &&
            payload.rootLine == eventLine =>
        record -> payload
    }
    val passedPawnResultProofRecords = (endpointRecords ++ involvedRecords).distinctBy(_.ref.id).collect {
      case record @ EvidenceRecord(ref, result: PassedPawnResultProofEvidence, _)
          if demand.requiresForEndpoint(sourceSide, RootOwnedEffectPrimitiveKind.PassedPawnResult) &&
            RootOwnedEffectPolicy.sameCausalRootOccurrence(ref.position, rootPosition) &&
            ref.line.contains(eventLine) &&
            result.rootLine == eventLine &&
            graph.proofEligible(record) =>
        record -> result
    }
    val projectedPassedPawnResultRecords = passedPawnResultEventProbes.map { case (eventRecord, event) =>
      val carrierReady = passedPawnResultClosureInput.exists(input =>
        input.extractionReady &&
          graph.proofEligible(eventRecord) &&
          eventRecord.parents.exists(parent => input.structuralRefIds(parent.id)) &&
          eventRecord.parents.exists(parent => input.lineRefIds(parent.id))
      )
      val exactResults = passedPawnResultProofRecords.filter { case (_, result) =>
        result.eventSource == eventRecord.ref &&
          result.event == event &&
          RelativeCauseKind.passedPawnResultProofCanProveCause(RelativeCauseKind.PassedPawnResult, result)
      }
      val selectedResults = exactResults.filter { case (record, result) =>
        closesPassedPawnResults || demandedPassedPawnResultProofs(
          RootOwnedEffectProof.PassedPawnResult(record.ref, result)
        )
      }
      val projected = selectedResults.map { case (record, result) =>
        val ref = record.ref
        val proof = RootOwnedEffectProof.PassedPawnResult(ref, result)
        val observation = ComparisonEndpointEffectObservationPolicy.fromExactPassedPawnResult(
          rootPosition,
          eventLine,
          ref,
          result,
          graph
        )
        val witnesses = actor.toList
          .filter(rootActor => RootOwnedEffectPolicy.passedPawnResultOwnsEventRoot(
            graph,
            ref,
            result,
            eventLine,
            rootActor
          ))
          .flatMap(exactActor =>
            passedPawnResultRouteBindings(
              ref,
              result.event,
              exactActor,
              result.assessment,
              eventLine,
              proof
            ).flatMap {
              case (binding, segment) =>
                witness(
                  binding.copy(provenance = result.proofParentSources),
                  proof,
                  observation,
                  selectedSegment = Some(segment)
                )
            }
          )
        (result.semanticIdentity, observation, witnesses)
      }
      val allResultIdentities = event.causalResultAssessments
        .map(assessment => PassedPawnResultSemanticIdentity.from(event, assessment))
        .toSet
      val successfulIdentities = projected.collect {
        case (identity, Some(_), witnesses) if witnesses.nonEmpty => identity
      }.toSet
      val incomplete =
        if !closesPassedPawnResults then Set.empty[PassedPawnResultSemanticIdentity]
        else if carrierReady then allResultIdentities -- successfulIdentities
        else allResultIdentities
      (
        event.passedPawnResultKind.id,
        if carrierReady then projected.flatMap(_._2).toSet
        else Set.empty[ComparisonEndpointEffectObservation],
        incomplete,
        projected.flatMap(_._3),
        carrierReady
      )
    }
    val passedPawnResultWitnesses = projectedPassedPawnResultRecords.flatMap(_._4)
    val passedPawnResultInventory = Option.when(closesPassedPawnResults) {
      val allSuccessful = projectedPassedPawnResultRecords.flatMap(_._2).toSet
      val conflictingScopes = allSuccessful
        .groupBy(_.scope)
        .collect { case (scope, values) if values.map(_.magnitude).size != 1 => scope }
        .toSet
      PassedPawnResultEndpointInventory(
        observations = allSuccessful.filterNot(observation => conflictingScopes(observation.scope)),
        enumeratedPassedPawnResultKindIds = projectedPassedPawnResultRecords.collect {
          case (passedPawnResultKind, _, _, _, true) => passedPawnResultKind
        }.toSet,
        incompleteResultIdentities = projectedPassedPawnResultRecords.flatMap(_._3).toSet ++
          conflictingScopes.flatMap(_.effectIdentity.passedPawnResult),
        extractionReady = passedPawnResultClosureInput.exists(_.extractionReady)
      )
    }

    val primitiveWitnesses = lineWitnesses ++ relationWitnesses ++ causalWitnesses ++ passedPawnResultWitnesses

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
          case _ => Nil
        }
        direct ++ carried

    val carrierWitnesses = demand.carrierSourceIds(sourceSide).toList.sorted.flatMap(sourceId =>
      involvedById.get(sourceId).toList.flatMap(record =>
        witnessesThroughCarrier(record.ref, Set.empty)
      )
    )
    ComparisonEndpointEvidenceProjection(
      witnesses = (baseWitnesses ++ carrierWitnesses).distinct.sortBy(witness =>
        (
          witness.rootOwnedProof.toString,
          witness.binding.signature,
          witness.primitiveProofSource.id
        )
      ),
      lineInventories = lineInventories,
      passedPawnResultInventory = passedPawnResultInventory
    )

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

  private[judgment] def projectCauseChannelsForExposure(channels: List[DirectCauseChannel]): List[DirectCauseChannel] =
    val selectedOccurrenceChannels = channels
      .groupBy(_.exactOccurrenceFingerprint)
      .toList
      .map { case (_, equivalents) => selectExposureChannelForExactOccurrence(equivalents) }
    val barePrimitivesByOccurrence = selectedOccurrenceChannels
      .filter(channel => channel.binding.provenance.isEmpty && channel.primitiveCausalSignature.isEmpty)
      .groupBy(primitiveOccurrenceKey)
    val ambiguityPropagatedToWrappers = selectedOccurrenceChannels.map { channel =>
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
      Option.when(channel.binding.provenance.nonEmpty)(primitiveOccurrenceKey(channel))
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
      primitiveProof = channel.rootOwnedProof,
      proofSegment = channel.proofSegmentOccurrence.orElse(channel.proofSegment),
      proofRole = channel.binding.proofRole,
      unprovedSource = Option.when(channel.rootOwnedProof.isEmpty)(channel.binding.source)
    )

  private def proofSegmentSortKey(segment: Option[DirectCauseProofSegment]): String =
    segment.map(_.stableKey).getOrElse("")

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
      episode.eventOccurrence.plyOffset == event.plyOffset &&
      event.plyOffset == 0 &&
      episode.consequence.rootMove.exists(
        EvidenceRef.sameMove(_, rootEventLine.line.rootMove)
      ) &&
      episode.consequence.eventMatches(event.plyOffset, event.moveUci) &&
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
      episodeStep <- episodeLine.lineReplaySteps.lift(episode.eventOccurrence.plyOffset)
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
  private def selectExposureChannelForExactOccurrence(
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
    projectCauseChannelsForExposure(
      section.sourceRefs
        .flatMap(ref =>
          graph.record(ref).toList.flatMap {
            case EvidenceRecord(_, payload: LineFactEvidence, _) =>
              fromLineFactForCauseProjection(ref, payload, cause, section, graph)
            case EvidenceRecord(_, payload: RelationFactEvidence, _) =>
              fromRelationForCauseProjection(ref, payload, cause, graph)
            case EvidenceRecord(_, payload: ForcedReplyResourceDifferentialEvidence, _) =>
              fromForcedReplyResourceForCauseProjection(ref, payload, cause, graph)
            case EvidenceRecord(_, payload: TacticalMechanismEvidence, _) =>
              fromTacticalMechanismForCauseProjection(
                ref,
                payload,
                cause,
                section,
                graph,
                Set(ref.id)
              )
            case EvidenceRecord(_, payload: PassedPawnResultProofEvidence, _) =>
              fromPassedPawnResultCausalProofForCauseProjection(ref, payload, cause, graph)
            case EvidenceRecord(_, _: PassedPawnResultEventEvidence, _) =>
              Nil
            case EvidenceRecord(_, CandidateComparisonEvidence(_), _) =>
              Nil
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
      projectCauseChannelsForExposure(episodeBindings ++ rootEventBindings)


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

  private def forcedReplyResourceBinding(
      source: EvidenceRef,
      payload: ForcedReplyResourceDifferentialEvidence,
      actor: RootCausalActor,
      eventLine: LineNodeRef
  ): EvidenceObjectBinding =
    val semantic = payload.semantic
    val occurrence = payload.occurrence
    val pathWitnesses = occurrence.proofPaths.flatMap(path =>
      objectOf(EvidenceObjectKind.Relation, s"proof-path:${path.pathOccurrenceId}") ++
        path.premiseUses.flatMap(use =>
          objectOf(
            EvidenceObjectKind.Relation,
            s"proof-path:${path.pathOccurrenceId}:premise-use:${use.stableKey}"
          )
        ) ++
        path.closedAbsenceUses.flatMap(use =>
          objectOf(
            EvidenceObjectKind.Relation,
            s"proof-path:${path.pathOccurrenceId}:closed-absence-use:${use.useId}"
          )
        )
    )
    EvidenceObjectBinding(
      source = source,
      provenance = payload.proofParentSources,
      proofRole = Some(RelativeCauseProofRole.DirectProof),
      actor = rootActorObjects(actor),
      target = (
        objectOf(EvidenceObjectKind.Square, semantic.capturedTarget.square.key) ++
          objectOf(EvidenceObjectKind.Piece, semantic.capturedTarget.role.name) ++
          objectOf(EvidenceObjectKind.Square, semantic.playedDefense.movement.from.key)
      ).distinctBy(_.signaturePart),
      mechanism = (
        objectOf(EvidenceObjectKind.Mechanism, "forced-reply-resource-differential") ++
          objectOf(EvidenceObjectKind.Relation, RelationFactKind.CreatedCheckResponseInventory.toString) ++
          objectOf(EvidenceObjectKind.Relation, RelationFactKind.CaptureRecaptureInventory.toString)
      ).distinctBy(_.signaturePart),
      consequence = (
        objectOf(EvidenceObjectKind.Consequence, "reference-closed-recapture-absence") ++
          objectOf(EvidenceObjectKind.Consequence, "played-legal-recapture")
      ).distinctBy(_.signaturePart),
      witness = (
        pathWitnesses ++
          objectOf(EvidenceObjectKind.Piece, semantic.forcedReply.movement.beforeRole.name) ++
          objectOf(EvidenceObjectKind.Square, semantic.forcedReply.movement.from.key) ++
          objectOf(EvidenceObjectKind.Square, semantic.forcedReply.movement.to.key) ++
          objectOf(EvidenceObjectKind.Line, s"counterfactual-reference:${occurrence.referenceLine.id}") ++
          objectOf(EvidenceObjectKind.Line, s"actual-played:${occurrence.playedLine.id}") ++
          objectOf(EvidenceObjectKind.Relation, s"semantic-proof:${semantic.semanticId}") ++
          objectOf(EvidenceObjectKind.Relation, s"occurrence:${occurrence.occurrenceId}")
      ).distinctBy(_.signaturePart),
      line = Some(eventLine),
      horizon = Some(s"ply:${occurrence.realizerStep.ply - occurrence.triggerStep.ply}")
    )

  private def fromForcedReplyResourceForCauseProjection(
      ref: EvidenceRef,
      payload: ForcedReplyResourceDifferentialEvidence,
      cause: RelativeCauseFact,
      graph: TypedEvidenceGraph
  ): List[DirectCauseChannel] =
    causeProjectionRoot(ref, cause, graph).toList.flatMap { root =>
      val comparisonMatches = graph.comparisonFor(cause).exists(comparison =>
        cause.kind == RelativeCauseKind.WrongMoveOrder &&
          cause.sourceSide == RelativeCauseSourceSide.Reference &&
          payload.occurrence.referenceLine == comparison.referenceLine &&
          payload.occurrence.playedLine == comparison.candidateLine &&
          root.binding.eventLine == comparison.referenceLine
      )
      if !comparisonMatches then Nil
      else
        RootOwnedEffectPolicy.certify(
          cause,
          graph,
          forcedReplyResourceBinding(ref, payload, root.actor, root.binding.eventLine),
          RootOwnedEffectProof.ForcedReplyResourceDifferential(ref, payload)
        )
    }

  private def fromPassedPawnResultCausalProofForCauseProjection(
      ref: EvidenceRef,
      result: PassedPawnResultProofEvidence,
      cause: RelativeCauseFact,
      graph: TypedEvidenceGraph
  ): List[DirectCauseChannel] =
    projectCauseChannelsForExposure(
      causeProjectionRoot(ref, cause, graph).toList.flatMap { root =>
        val eventLine = root.binding.eventLine
        cause.kind match
          case RelativeCauseKind.PassedPawnResult =>
            RootOwnedEffectPolicy.passedPawnResultProofs(cause, ref, result).flatMap {
              case (exact, proof) =>
                passedPawnResultRouteBindings(ref, result.event, root.actor, exact, eventLine, proof).flatMap {
                  case (binding, segment) =>
                    RootOwnedEffectPolicy.certify(
                      cause,
                      graph,
                      binding.copy(provenance = result.proofParentSources),
                      proof,
                      selectedProofSegment = Some(segment)
                    )
                }
            }
          case _ =>
            Nil
      }
    )

  private[chessjudgment] def passedPawnResultRouteBindings(
      ref: EvidenceRef,
      payload: PassedPawnResultEventEvidence,
      actor: RootCausalActor,
      assessment: PassedPawnResultReplyAssessment,
      eventLine: LineNodeRef,
      proof: RootOwnedEffectProof
  ): List[(EvidenceObjectBinding, DirectCauseProofSegment)] =
    DirectCauseProofSegment.allFrom(proof).map { segment =>
      passedPawnResultBinding(
        ref,
        payload,
        actor,
        assessment,
        eventLine,
        segment.passedPawnResultDependencies
      ) -> segment
    }

  private def passedPawnResultBinding(
      ref: EvidenceRef,
      payload: PassedPawnResultEventEvidence,
      actor: RootCausalActor,
      assessment: PassedPawnResultReplyAssessment,
      eventLine: LineNodeRef,
      dependencies: List[PassedPawnResultDependency]
  ): EvidenceObjectBinding =
    val sourceEvent = assessment.sourceEvent
    val targets = (
      assessment.consequence.resultSubjectFacts.flatMap(subjectObject) ++
        PassedPawnResultEpisode
          .consequenceTargetSquares(assessment.consequence)
          .flatMap(square => objectOf(EvidenceObjectKind.Square, square.key))
    ).distinctBy(_.signaturePart)
    EvidenceObjectBinding(
      source = ref,
      actor = rootActorObjects(actor),
      target = targets,
      mechanism = (
        dependencies.flatMap(dependency => objectOf(EvidenceObjectKind.Mechanism, dependency.kind.toString)) ++
          objectOf(EvidenceObjectKind.Mechanism, assessment.resultProof.mechanism.toString) ++
          objectOf(EvidenceObjectKind.Mechanism, assessment.consequence.kind.toString)
      ).distinctBy(_.signaturePart),
      consequence = objectOf(
        EvidenceObjectKind.Consequence,
        s"${assessment.consequence.anchorKey}:${assessment.robustness}"
      ),
      witness = (
        objectOf(EvidenceObjectKind.PassedPawnSubject, payload.passedPawnResultKind.id) ++
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
      passedPawnResultDependencies = dependencies,
      passedPawnResultProof = Some(assessment.resultProof)
    )

  private def fromTacticalMechanismForCauseProjection(
      ref: EvidenceRef,
      payload: TacticalMechanismEvidence,
      cause: RelativeCauseFact,
      section: RelativeCauseProofSection,
      graph: TypedEvidenceGraph,
      visited: Set[String]
  ): List[DirectCauseChannel] =
    projectCauseChannelsForExposure(
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
        case EvidenceRecord(_, payload: ForcedReplyResourceDifferentialEvidence, _) =>
          fromForcedReplyResourceForCauseProjection(ref, payload, cause, graph)
        case EvidenceRecord(_, payload: PassedPawnResultProofEvidence, _) =>
          fromPassedPawnResultCausalProofForCauseProjection(ref, payload, cause, graph)
        case EvidenceRecord(_, _: PassedPawnResultEventEvidence, _) =>
          Nil
        case EvidenceRecord(_, payload: TacticalMechanismEvidence, _) =>
          fromTacticalMechanismForCauseProjection(
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
      semanticConsequence: List[ConcreteChessObject] = Nil
  ): List[DirectCauseChannel] =
    val binding = channel.binding
    val actorSignature = rootActorObjects(actor).map(_.signaturePart).toSet
    val bindingActorSignature = binding.actor.map(_.signaturePart).toSet
    for
      proof <- channel.rootOwnedProof.toList
      if actorSignature.subsetOf(bindingActorSignature)
      carried <- RootOwnedEffectPolicy.certify(
        cause,
        graph,
        binding.copy(
          source = carrier,
          provenance = (binding.source :: binding.provenance).distinctBy(_.id),
          actor = rootActorObjects(actor),
          mechanism = (binding.mechanism ++ semanticMechanism).distinctBy(_.signaturePart),
          consequence = (binding.consequence ++ semanticConsequence).distinctBy(_.signaturePart),
          witness = binding.witness,
          line = Some(line),
          horizon = binding.horizon
        ),
        proof,
        channel.primitiveCausalSignature.orElse(Some(channel.causalSignature)),
        selectedProofSegment = channel.proofSegmentOccurrence.orElse(channel.proofSegment)
      )
    yield carried.copy(
      importanceDescriptorAmbiguous = channel.importanceDescriptorAmbiguous,
      proofSegmentAmbiguous = channel.proofSegmentAmbiguous
    )

  private def rootOwnedEpisodeBinding(
      source: EvidenceRef,
      payload: LineFactEvidence,
      episode: RootOwnedCausalEpisode
  ): EvidenceObjectBinding =
    val target = episodeTargetObjects(payload, episode)
    val event = episode.eventOccurrence
    EvidenceObjectBinding(
      source = source,
      actor = rootActorObjects(episode.actor),
      target = target,
      mechanism = episode.links.flatMap(link => objectOf(EvidenceObjectKind.Mechanism, link.kind.toString)),
      consequence = objectOf(EvidenceObjectKind.Consequence, episode.consequence.kind.toString),
      witness = episode.chain.flatMap(occurrence => objectOf(EvidenceObjectKind.Move, occurrence.moveUci)) ++
        episode.links.flatMap(link => objectOf(EvidenceObjectKind.Square, link.anchor.key)) ++
        lineObject(episode.line),
      line = Some(episode.line),
      horizon = Some(s"ply:${event.plyOffset}"),
      lineOccurrence = payload.lineReplaySteps.lift(event.plyOffset)
    )

  private def episodeTargetObjects(
      payload: LineFactEvidence,
      episode: RootOwnedCausalEpisode
  ): List[ConcreteChessObject] =
    val eventOccurrence = episode.eventOccurrence
    val square = objectOf(EvidenceObjectKind.Square, episode.target.key)
    val role = episode.consequence.kind match
      case LineConsequenceKind.MaterialGain | LineConsequenceKind.MaterialLoss |
          LineConsequenceKind.RecaptureSequence | LineConsequenceKind.RecoveryWindow =>
        payload.lineReplaySteps
          .lift(eventOccurrence.plyOffset)
          .filter(step => eventOccurrence.sameOccurrence(eventOccurrence.plyOffset, step.moveUci))
          .flatMap(step => payload.uniqueMaterialCaptureAt(eventOccurrence.plyOffset, step.moveUci))
          .toList
          .flatMap(capture => roleObject(Some(capture.capturedRole)))
      case LineConsequenceKind.ImmediateReplyCheck | LineConsequenceKind.Mate |
          LineConsequenceKind.DrawResource =>
        roleObject(Some(EvidencePieceRole(King.name)))
      case LineConsequenceKind.Promotion =>
        payload.lineEvents
          .find(event =>
            event.plyOffset == eventOccurrence.plyOffset &&
              event.kind == LineEventKind.Promotion &&
              eventOccurrence.sameOccurrence(event.plyOffset, event.moveUci)
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
      case LineEventKind.CheckEvasion =>
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
      cause.supportEvidence.filterNot(ref => proofSourceIds.contains(ref.id))
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

  private[chessjudgment] def resultTargetObjectGroups(
      consequence: TransitionConsequence
  ): List[Set[ConcreteChessObject]] =
    consequence.resultSubjectFacts.flatMap { subject =>
      val group = subjectObject(subject)
        .filter(obj => specificSurfaceTargetObject(obj) && obj.kind != EvidenceObjectKind.PassedPawnSubject)
        .toSet
      val pieceRolesValid = group.collect {
        case obj if obj.kind == EvidenceObjectKind.Piece => obj.key.toLowerCase
      }.forall(ConcretePieceRoleKeys)
      Option.when(group.nonEmpty && pieceRolesValid)(group)
    }.distinct

  private[chessjudgment] def resultTargetObjects(
      consequence: TransitionConsequence
  ): Set[ConcreteChessObject] =
    resultTargetObjectGroups(consequence).flatten.toSet

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
    fromEvidenceRefs(graph, section.sourceRefs, visited)
      .map(_.copy(proofRole = Some(section.role)))
      .distinctBy(_.occurrenceSignature)

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
        case payload: ForcedReplyResourceDifferentialEvidence =>
          val trigger = payload.semantic.trigger
          val actor = RootCausalActor(
            payload.occurrence.triggerStep.moveUci,
            trigger.beforeRole,
            trigger.side,
            trigger.from,
            trigger.to
          )
          List(forcedReplyResourceBinding(record.ref, payload, actor, payload.occurrence.referenceLine))
        case _: PassedPawnResultEventEvidence =>
          Nil
        case payload: PassedPawnResultProofEvidence =>
          RootCausalActor.fromPassedPawnResultEvent(payload.event).toList.flatMap { actor =>
            val proof = RootOwnedEffectProof.PassedPawnResult(record.ref, payload)
            passedPawnResultRouteBindings(
              record.ref,
              payload.event,
              actor,
              payload.assessment,
              payload.rootLine,
              proof
            ).map { case (binding, _) =>
              binding.copy(provenance = payload.proofParentSources)
            }
          }
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
                      horizon = binding.horizon
                    )
                  )
                )
              )
            )
          if sourceBindings.nonEmpty then sourceBindings.distinctBy(_.occurrenceSignature)
          else List(fromTacticalMechanism(record.ref, payload, graph))
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
      payload.directCauseProjectionEligibleConsequences.flatMap { consequence =>
        val ownedSacrificeOccurrence = consequence.sacrificeOccurrence.filter(_ =>
          consequence.kind == LineConsequenceKind.Sacrifice &&
            consequence.rootMove.exists(root => payload.rootMove.exists(EvidenceRef.sameMove(_, root)))
        )
        val eventActor = consequence.eventOccurrence.flatMap(occurrence =>
          exactLineActorAt(payload, occurrence.plyOffset, occurrence.moveUci)
        )
        val sacrificeCaptureTargets =
          ownedSacrificeOccurrence.toList.flatMap { occurrence =>
            val capture = occurrence.acceptance
            squareObject(Some(capture.square)) ++
              roleObject(Some(capture.capturedRole)) ++
              objectOf(EvidenceObjectKind.PassedPawnSubject, s"material-sacrifice:${capture.square.key}")
          }
        val lineMoveWitness =
          consequence.proofOccurrences.flatMap(occurrence =>
            exactLineActorAt(payload, occurrence.plyOffset, occurrence.moveUci).toList.flatMap(actor =>
              objectOf(EvidenceObjectKind.Move, occurrence.moveUci) ++ actorTargetSquare(actor)
            )
          )
        val lineEventWitness =
          payload.lineEvents
            .filter(event => consequence.hasProofOccurrence(event.plyOffset, event.moveUci))
            .flatMap(event => squareObject(event.square) ++ roleObject(event.pieceRole) ++ roleObject(event.targetRole))
        (for
          occurrence <- consequence.eventOccurrence.toList
          actor <- eventActor.toList
          step <- payload.lineReplaySteps.lift(occurrence.plyOffset).toList
          if occurrence.sameOccurrence(occurrence.plyOffset, step.moveUci)
        yield
          EvidenceObjectBinding(
            source = ref,
            actor = rootActorObjects(actor),
            target = actorTargetSquare(actor) ++ sacrificeCaptureTargets,
            mechanism = objectOf(EvidenceObjectKind.Mechanism, consequence.kind.toString),
            consequence = objectOf(EvidenceObjectKind.Consequence, consequence.kind.toString),
            witness = lineMoveWitness ++ lineEventWitness ++ lineObject(payload.line),
            line = Some(payload.line),
            horizon = Some(s"ply:${occurrence.plyOffset}"),
            lineOccurrence = Some(step)
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
              objectOf(EvidenceObjectKind.PassedPawnSubject, s"$prefix:${capture.square.key}") ++
                Option
                  .when(payload.materialSacrificeCapture(capture))(s"material-sacrifice:${capture.square.key}")
                  .toList
                  .flatMap(objectOf(EvidenceObjectKind.PassedPawnSubject, _))
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
        case RelationWitnessDetail.GeometricControl(side, _, _, _) =>
          objectOf(EvidenceObjectKind.Side, colorKey(side))
        case RelationWitnessDetail.LegalMove(side, _, _, _, _, _) =>
          objectOf(EvidenceObjectKind.Side, colorKey(side))
        case RelationWitnessDetail.GeometricControlSetDelta(mover, controllingSide, _, _, _, _, _, _, _, _) =>
          objectOf(EvidenceObjectKind.Side, colorKey(mover.side)) ++
            objectOf(EvidenceObjectKind.Side, colorKey(controllingSide))
        case RelationWitnessDetail.CaptureRecaptureInventory(mover, captured, _, _, _, _) =>
          objectOf(EvidenceObjectKind.Side, colorKey(mover.side)) ++
            objectOf(EvidenceObjectKind.Side, colorKey(captured.side))
        case RelationWitnessDetail.CreatedCheckResponseInventory(mover, checkedSide, _, _, _, _, _, _) =>
          objectOf(EvidenceObjectKind.Side, colorKey(mover.side)) ++
            objectOf(EvidenceObjectKind.Side, colorKey(checkedSide))
        case RelationWitnessDetail.RootCheckResponse(mover, respondingSide, _, _, _, _) =>
          objectOf(EvidenceObjectKind.Side, colorKey(mover.side)) ++
            objectOf(EvidenceObjectKind.Side, colorKey(respondingSide))
        case RelationWitnessDetail.SliderReachDelta(mover, owner, _, _, _, _, _, _) =>
          objectOf(EvidenceObjectKind.Side, colorKey(mover.side)) ++
            objectOf(EvidenceObjectKind.Side, colorKey(owner))
        case RelationWitnessDetail.PawnTopologyTransition(mover, before, after, _, _) =>
          objectOf(EvidenceObjectKind.Side, colorKey(mover.side)) ++
            (before.orElse(after).toList.flatMap(value => objectOf(EvidenceObjectKind.Side, colorKey(value.side))))
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
        case RelationWitnessDetail.GeometricControl(_, _, _, _) =>
          Nil
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
        case RelationWitnessDetail.CaptureRecaptureInventory(_, captured, _, _, _, _) =>
          objectOf(EvidenceObjectKind.Side, colorKey(captured.side))
        case RelationWitnessDetail.CreatedCheckResponseInventory(_, checkedSide, _, _, _, _, _, _) =>
          objectOf(EvidenceObjectKind.Side, colorKey(checkedSide))
        case RelationWitnessDetail.RootCheckResponse(_, respondingSide, _, _, _, _) =>
          objectOf(EvidenceObjectKind.Side, colorKey(respondingSide))
        case RelationWitnessDetail.SliderReachDelta(_, owner, _, _, _, _, _, _) =>
          objectOf(EvidenceObjectKind.Side, colorKey(owner))
        case RelationWitnessDetail.PawnTopologyTransition(_, before, after, _, _) =>
          before.orElse(after).toList.flatMap(value => objectOf(EvidenceObjectKind.Side, colorKey(value.side)))
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

  private def fromStructuralDelta(ref: EvidenceRef, payload: StructuralDeltaEvidence): List[EvidenceObjectBinding] =
    val actor = payload.certifiedRootMovement.toList.flatMap(movementActorObjects)
    payload.consequences
      .flatMap(consequence => structuralConsequenceBindings(ref, payload, actor, consequence))
      .distinctBy(_.occurrenceSignature)

  private def structuralConsequenceBindings(
      ref: EvidenceRef,
      payload: StructuralDeltaEvidence,
      actor: List[ConcreteChessObject],
      consequence: TransitionConsequence
  ): List[EvidenceObjectBinding] =
    val allBindings = (consequence.subjectBindings ++ consequence.targetBindings).distinct
    val groups =
      allBindings
        .groupBy(binding => binding.relationKeys.map(_.stableKey).sorted.mkString("|"))
        .toList
        .sortBy(_._1)
        .map(_._2)
    val changesByKey = payload.relationChanges.groupMap(_.key)(identity)
    val resolved = groups.map { bindings =>
      val keys = bindings.flatMap(_.relationKeys).distinct.sortBy(_.stableKey)
      val sources = keys.flatMap(key => changesByKey.getOrElse(key, Nil).map(_.source)).distinctBy(_.id)
      val exactResolution =
        bindings.nonEmpty &&
          bindings.forall(_.relationKeys.nonEmpty) &&
          keys.forall(key => changesByKey.get(key).exists(_.size == 1)) &&
          sources.size == keys.size
      Option.when(exactResolution) {
        val bindingSet = bindings.toSet
        val resultSubjects = consequence.resultSubjectBindings.filter(bindingSet).map(_.subject)
        val witnessSubjects = consequence.witnessSubjectBindings.filter(bindingSet).map(_.subject)
        EvidenceObjectBinding(
          source = ref,
          actor = actor.distinctBy(_.signaturePart),
          target = resultSubjects.flatMap(subjectObject).distinctBy(_.signaturePart),
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
    }.distinct
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

  private def actorTargetSquare(actor: RootCausalActor): List[ConcreteChessObject] =
    objectOf(EvidenceObjectKind.Square, actor.to.key)

  private def squareObject(square: Option[EvidenceSquare]): List[ConcreteChessObject] =
    square.toList.flatMap(square => objectOf(EvidenceObjectKind.Square, square.key))

  private def roleObject(role: Option[EvidencePieceRole]): List[ConcreteChessObject] =
    role.toList.flatMap(role => objectOf(EvidenceObjectKind.Piece, role.name))

  private def lineObject(line: LineNodeRef): List[ConcreteChessObject] =
    objectOf(EvidenceObjectKind.Line, line.id) ++ objectOf(EvidenceObjectKind.Move, line.rootMove)

  private def subjectObject(subject: StructuralSubject): List[ConcreteChessObject] =
    val identityObject = subject.identityKey.toList.flatMap(objectOf(EvidenceObjectKind.PassedPawnSubject, _))
    val values = subject match
      case StructuralSubject.OpenFile(file) =>
        objectOf(EvidenceObjectKind.File, file.key)
      case StructuralSubject.SemiOpenFile(side, file) =>
        objectOf(EvidenceObjectKind.Side, colorKey(side)) ++ objectOf(EvidenceObjectKind.File, file.key)
      case StructuralSubject.PieceAt(side, role, square) =>
        objectOf(EvidenceObjectKind.Side, colorKey(side)) ++
          objectOf(EvidenceObjectKind.Piece, role.name) ++ objectOf(EvidenceObjectKind.Square, square.key)
      case StructuralSubject.PawnTensionCreated(side, from, to) =>
        objectOf(EvidenceObjectKind.Side, colorKey(side)) ++ objectOf(EvidenceObjectKind.Piece, "pawn") ++
          objectOf(EvidenceObjectKind.Square, from.key) ++ objectOf(EvidenceObjectKind.Square, to.key)
      case StructuralSubject.PawnTensionResolved(side, from, to) =>
        objectOf(EvidenceObjectKind.Side, colorKey(side)) ++ objectOf(EvidenceObjectKind.Piece, "pawn") ++
          objectOf(EvidenceObjectKind.Square, from.key) ++ objectOf(EvidenceObjectKind.Square, to.key)
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

final case class RelationRayGeometry(
    direction: RelationRayDirection,
    squares: List[EvidenceSquare]
):
  require(squares.nonEmpty, "a relation ray needs its exact non-empty board segment")
  require(squares.distinct.size == squares.size, "a relation ray cannot repeat a board square")

  def axis: RelationAxisSignal = direction.axis

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

enum RelationWitnessDetail:
  case GeometricControl(
      side: Color,
      attackerSquare: EvidenceSquare,
      attackerRole: EvidencePieceRole,
      targetSquare: EvidenceSquare
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
  case CaptureRecaptureInventory(
      mover: RelationMoveTransitionWitness,
      captured: RelationColoredPieceWitness,
      geometricRecapturers: List[RelationPieceWitness],
      legalRecaptures: List[RelationLegalMoveResourceWitness],
      restrictedRecaptures: List[RelationRestrictedResourceWitness],
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
  case PawnTopologyTransition(
      mover: RelationMoveTransitionWitness,
      before: Option[RelationPawnTopologyStateWitness],
      after: Option[RelationPawnTopologyStateWitness],
      changedFacets: List[RelationPawnTopologyFacet],
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
      geometry: RelationRayGeometry
  )

  def detailName: String =
    RelationFactKind.id(RelationWitnessDetail.factKind(this))

private[chessjudgment] object RelationRayProjection:
  /** Geometry alone does not certify a pin. L1 consumers combine this exact
    * ray shape with legal-resource absence and post-move king exposure.
    */
  def isAbsoluteKingPinGeometry(detail: RelationWitnessDetail.RayBarrier): Boolean =
    detail.occupants.headOption.exists { barrier =>
      detail.occupants.lift(1).exists { rear =>
        barrier.side != detail.side && rear.side == barrier.side &&
          rear.role.name.equalsIgnoreCase(King.name) &&
          !barrier.role.name.equalsIgnoreCase(King.name)
      }
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
  private def optional[A](value: Option[A])(encode: A => String): String =
    value.map(item => tuple("some", List(encode(item)))).getOrElse(tuple("none", Nil))

  /** Injective, representation-independent identity of one typed relation
    * detail. Case-class `toString` is intentionally excluded from graph ids,
    * ordering, and public mechanism identity.
    */
  private def encodedKey(detail: RelationWitnessDetail, includeDerivation: Boolean): String =
    val fields = detail match
      case GeometricControl(owner, attacker, attackerRole, target) =>
        List(side(owner), square(attacker), role(attackerRole), square(target))
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
      case CaptureRecaptureInventory(
            mover,
            captured,
            geometricRecapturers,
            legalRecaptures,
            restrictedRecaptures,
            proof
          ) =>
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
        require(
          restrictedRecaptures.distinct.size == restrictedRecaptures.size &&
            restrictedRecaptures == restrictedRecaptures.sortBy(_.stableKey) &&
            restrictedRecaptures.forall(restriction =>
              geometricRecapturers.contains(restriction.piece) &&
                restriction.resource.destination == mover.to &&
                restriction.resource.target == RelationControlTarget.Enemy(mover.afterRole) &&
                restriction.resource.mode == RelationMovementResourceMode.ControlledDestination &&
                legalRecaptures.forall(resource =>
                  resource.movement.from != restriction.piece.square ||
                    resource.movement.to != restriction.resource.destination
                )
            ),
          "restricted recaptures must be canonical, geometric, and absent from legal recaptures"
        )
        List(
          moveTransition(mover),
          coloredPiece(captured),
          values(geometricRecapturers.map(piece)),
          values(legalRecaptures.map(_.stableKey)),
          values(restrictedRecaptures.map(_.stableKey))
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
      case RayBarrier(owner, attacker, attackerRole, occupants, geometry) =>
        List(
          side(owner),
          square(attacker),
          role(attackerRole),
          sequence(occupants.map(coloredPiece)),
          geometry.direction.stableKey,
          sequence(geometry.squares.map(square))
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
      case _: CaptureRecaptureInventory => RelationFactKind.CaptureRecaptureInventory
      case _: CreatedCheckResponseInventory => RelationFactKind.CreatedCheckResponseInventory
      case _: RootCheckResponse => RelationFactKind.RootCheckResponse
      case _: SliderReachDelta => RelationFactKind.SliderReachDelta
      case _: PawnTopologyTransition => RelationFactKind.PawnTopologyTransition
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
      case _: GeometricControlSetDelta =>
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
      case GeometricControlSetDelta(_, _, _, _, _, _, _, _, _, proof) => Some(proof)
      case _ => None

  def combinationPremises(detail: RelationWitnessDetail): List[RelationCombinationPremise] =
    combinationProof(detail).toList.flatMap(_.premises)

  def focusSquares(detail: RelationWitnessDetail): List[EvidenceSquare] =
    val squares =
      detail match
        case GeometricControl(_, attackerSquare, _, targetSquare) =>
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
        case CaptureRecaptureInventory(mover, captured, geometricRecapturers, legalRecaptures, restrictions, _) =>
          List(mover.from, mover.to, captured.square) ++ geometricRecapturers.map(_.square) ++
            legalRecaptures.flatMap(resource => List(resource.movement.from, resource.movement.to)) ++
            restrictions.flatMap(restriction =>
              List(restriction.piece.square, restriction.resource.destination, restriction.kingSquare) ++
                restriction.postMoveControllers.map(_.square) ++
                restriction.absolutePinPaths.flatMap(path =>
                  List(path.pinner.square, path.pinned.square, path.kingSquare) ++ path.geometry.squares
                )
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
          List(mover.from, mover.to, kingSquare) ++ checkers.map(_.square) ++
            responses.flatMap(response => List(response.resource.movement.from, response.resource.movement.to)) ++
            controlledKingDestinations.flatMap(destination =>
              destination.resource.destination :: destination.controllers.map(_.square)
            )
        case RootCheckResponse(mover, _, kingSquare, checkers, response, _) =>
          List(mover.from, mover.to, kingSquare) ++ checkers.map(_.square) ++
            List(response.resource.movement.from, response.resource.movement.to)
        case SliderReachDelta(mover, _, sliderBefore, sliderAfter, _, before, after, _) =>
          List(mover.from, mover.to) ++ sliderBefore.map(_.square) ++ sliderAfter.map(_.square) ++
            before.toList.flatMap(reach => reach.segment.map(_.square) ++ reach.firstOccupant.map(_.square)) ++
            after.toList.flatMap(reach => reach.segment.map(_.square) ++ reach.firstOccupant.map(_.square))
        case PawnTopologyTransition(mover, before, after, _, _) =>
          List(mover.from, mover.to) ++ before.toList.flatMap(pawnTopologySquares) ++
            after.toList.flatMap(pawnTopologySquares)
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
      case GeometricControl(_, _, _, targetSquare) =>
        List(targetSquare)
      case LegalMove(_, _, _, destinationSquare, _, capture) =>
        capture.map(_.capturedSquare).getOrElse(destinationSquare) :: Nil
      case GeometricControlSetDelta(_, _, target, _, _, _, _, _, _, _) =>
        List(target)
      case CaptureRecaptureInventory(mover, _, _, _, _, _) =>
        List(mover.to)
      case CreatedCheckResponseInventory(_, _, kingSquare, _, _, _, _, _) =>
        List(kingSquare)
      case RootCheckResponse(_, _, kingSquare, _, _, _) =>
        List(kingSquare)
      case SliderReachDelta(_, _, _, _, _, _, after, _) =>
        after.toList.flatMap(_.segment.map(_.square))
      case PawnTopologyTransition(_, before, after, _, _) =>
        after.orElse(before).toList.map(_.square)
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
        ray.occupants.headOption.map(_.square).toList
    squares.distinct

  def files(detail: RelationWitnessDetail): List[EvidenceFile] =
    detail match
      case PawnFileGroup(_, file, _)                => List(file)
      case _                                       => Nil

  def participants(detail: RelationWitnessDetail): List[RelationParticipant] =
    val values =
      detail match
        case GeometricControl(_, attackerSquare, attackerRole, targetSquare) =>
          List(
            part(
              attackerSquare,
              RelationParticipantRole.Controller,
              Some(attackerRole)
            ),
            part(
              targetSquare,
              RelationParticipantRole.Target
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
        case CaptureRecaptureInventory(mover, captured, geometricRecapturers, legalRecaptures, restrictions, _) =>
          List(
            part(mover.from, RelationParticipantRole.Mover, Some(mover.beforeRole)),
            part(mover.to, RelationParticipantRole.Mover, Some(mover.afterRole)),
            part(captured.square, RelationParticipantRole.Target, Some(captured.role))
          ) ++ geometricRecapturers.map(value =>
            part(value.square, RelationParticipantRole.Controller, Some(value.role))
          ) ++ legalRecaptures.map(value =>
            part(value.movement.from, RelationParticipantRole.Controller, Some(value.movement.beforeRole))
          ) ++ restrictions.flatMap(restriction =>
            part(restriction.piece.square, RelationParticipantRole.Controller, Some(restriction.piece.role)) ::
              part(restriction.kingSquare, RelationParticipantRole.King, Some(EvidencePieceRole(King.name))) ::
              restriction.postMoveControllers.map(value =>
                part(value.square, RelationParticipantRole.Attacker, Some(value.role))
              )
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
          val exactTarget = targetSquares(detail).toSet
          part(attackerSquare, RelationParticipantRole.Attacker, Some(attackerRole)) ::
            occupants.headOption.toList.flatMap { barrier =>
              val barrierParticipantRole =
                if barrier.side == ray.side then RelationParticipantRole.Supported
                else if barrier.role.name.equalsIgnoreCase(King.name) then RelationParticipantRole.King
                else RelationParticipantRole.Blocker
              part(barrier.square, barrierParticipantRole, Some(barrier.role)) ::
                occupants.drop(1).zipWithIndex.map { case (piece, rearIndex) =>
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
  case CaptureRecaptureInventory
  case CreatedCheckResponseInventory
  case RootCheckResponse
  case SliderReachDelta
  case PawnTopologyTransition
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
      case CaptureRecaptureInventory => "capture_recapture_inventory"
      case CreatedCheckResponseInventory => "created_check_response_inventory"
      case RootCheckResponse => "root_check_response"
      case SliderReachDelta => "slider_reach_delta"
      case PawnTopologyTransition => "pawn_topology_transition"
      case StalemateTransition => "stalemate_transition"
      case PawnFileGroup         => "pawn_file_group"
      case PawnFrontOccupancy    => "pawn_front_occupancy"
      case PawnAdvanceAffordance => "pawn_advance_affordance"
      case PawnPassage           => "pawn_passage"
      case RayBarrier            => "ray_barrier"

sealed trait EvidencePayload

private[chessjudgment] final case class PositionOccurrenceEvidence(
    occurrence: PositionOccurrenceState
) extends EvidencePayload

/** First result in the common bounded L2 family. It is deliberately
  * package-private: the only public projection is the existing WrongMoveOrder
  * explanation that consumes this exact actual/counterfactual proof.
  */
private[chessjudgment] final case class ForcedReplyResourceDifferentialEvidence private[chessjudgment] (
    semantic: ForcedReplyResourceSemanticProof,
    occurrence: ForcedReplyResourceOccurrence,
    dependencyFingerprint: String,
    private[chessjudgment] val occurrenceProof: Option[ForcedReplyResourceOccurrenceProof] = None
) extends EvidencePayload:
  require(
    occurrence.semanticId == semantic.semanticId,
    "an L2 occurrence must retain its exact semantic proof"
  )
  require(
    dependencyFingerprint.matches("[0-9a-f]{64}"),
    "an L2 result needs its complete dependency fingerprint"
  )

  def semanticId: String = semantic.semanticId
  def occurrenceId: String = occurrence.occurrenceId
  def dependencyId: String = dependencyFingerprint
  def triggerMechanism: String = semantic.mechanism.wireCode
  def referenceLine: LineNodeRef = occurrence.referenceLine
  def playedLine: LineNodeRef = occurrence.playedLine
  def referenceSteps: List[LineReplayStep] = occurrence.referenceSteps
  def playedSteps: List[LineReplayStep] = occurrence.playedSteps
  def referenceBranch: CausalBranchOccurrence = occurrence.referenceBranch
  def playedBranch: CausalBranchOccurrence = occurrence.playedBranch
  def proofPaths: List[CausalProofPathOccurrence] = occurrence.proofPaths
  def hasCompleteProofPaths: Boolean =
    proofPaths.nonEmpty && proofPaths.forall(path =>
      path.premiseUses.nonEmpty && path.closedAbsenceUses.nonEmpty
    )
  def triggerMovement: RelationMoveTransitionWitness = semantic.trigger
  def forcedReplyResource: RelationLegalMoveResourceWitness = semantic.forcedReply
  def realizerMovement: RelationMoveTransitionWitness = semantic.realizer
  def capturedTarget: RelationColoredPieceWitness = semantic.capturedTarget
  def playedDefenseResource: RelationLegalMoveResourceWitness = semantic.playedDefense
  def disabledDefender: RelationColoredPieceWitness = semantic.disabledDefender
  def realizingMove: String = occurrence.realizerStep.moveUci
  def playedDefenseMove: String = occurrence.playedReplyStep.moveUci

  private[chessjudgment] def exactOccurrenceCertified(record: EvidenceRecord): Boolean =
    occurrenceProof.exists(_.proves(record, this))

  private[chessjudgment] def proofParentSources: List[EvidenceRef] =
    occurrenceProof.toList.flatMap(_.parentSources)
  private[chessjudgment] def consumesDependencies(
      fact: CandidateComparisonFact,
      referenceSource: EvidenceRecord,
      playedSource: EvidenceRecord,
      demandSource: EvidenceRecord
  ): Boolean =
    occurrenceProof.exists(
      _.consumesDependencies(fact, referenceSource, playedSource, demandSource)
    )
  private[chessjudgment] def exactReuseIdentity: String =
    List(
      semantic.identity.contractKind.toString.toLowerCase,
      semantic.mechanism.stableKey,
      semanticId,
      occurrenceId,
      dependencyId,
      proofPaths.map(_.pathOccurrenceId).mkString("[", ",", "]")
    ).mkString("|")

/** Read-only public occurrence projection of one certified passed-pawn-result link. */
final case class PassedPawnResultPublicLink(
    kind: String,
    fromStepKey: String,
    toStepKey: String,
    occurrenceLinkKey: String
)

/** Read-only public occurrence projection of one exact replay step. */
final case class PassedPawnResultPublicStep(
    index: Int,
    stepKey: String,
    ply: Int,
    moveUci: String,
    fenBefore: String,
    fenAfter: String,
    lineId: String,
    lineRole: String,
    lineRank: Int,
    lineRootMove: String,
    provenance: String,
    incomingLink: Option[PassedPawnResultPublicLink]
)

/** One occurrence path. Transposed semantics may be shared, while this exact
  * branch and its provenance remain visible and are never de-duplicated.
  */
final case class PassedPawnResultPublicBranch(
    branchId: String,
    role: String,
    replyMove: Option[String],
    sourceProbeId: Option[String],
    lineId: String,
    lineRole: String,
    lineRank: Int,
    lineRootMove: String,
    rootProvenance: String,
    steps: List[PassedPawnResultPublicStep]
):
  require(
    role match
      case "expected_result_route" => replyMove.isEmpty && sourceProbeId.isEmpty
      case "legal_reply"           => replyMove.nonEmpty && sourceProbeId.nonEmpty
      case _                       => false,
    "a public passed-pawn result branch needs one exact certified branch role"
  )

/** One exact lower-premise occurrence use retained by an independent path. */
final case class PassedPawnResultPublicPremise(
    role: String,
    lowerKind: String,
    lowerSemanticKey: String,
    sourcePremiseIds: List[String],
    branchId: String,
    branchRole: String,
    relatedBranchIds: List[String],
    fromStepIndex: Int,
    toStepIndex: Int
)

/** Public projection of one independent proof path. */
final case class PassedPawnResultPublicProofPath(
    pathOccurrenceId: String,
    replyBranchId: String,
    realizationActor: PassedPawnResultActorOccurrence,
    realizationMove: String,
    realizationPly: Int,
    realizationMatchKind: PassedPawnResultMatch,
    premises: List[PassedPawnResultPublicPremise],
    closureUseIds: List[String]
)

/** Closed legal-reply inventory issued by the exact lower StructuralDelta authority. */
final case class PassedPawnResultPublicClosedReplyInventory(
    issuerEvidenceId: String,
    coverageEvidenceId: String,
    rootAfterFen: String,
    rootAfterPly: Int,
    scope: String,
    legalReplyMoves: List[String],
    branchByReply: List[(String, String)],
    certifiedHorizonPlyOffset: Int
)

/** Publicly consumed L2 result for the only currently closed passed-pawn result family.
  * The passed-pawn result label remains annotation; the certified proposition is that the
  * exact passed-pawn result is causally realized under every legal root reply.
  */
final case class PassedPawnResultProofEvidence private[chessjudgment] (
    eventSource: EvidenceRef,
    comparisonDemand: EvidenceRef,
    event: PassedPawnResultEventEvidence,
    assessment: PassedPawnResultReplyAssessment,
    semanticIdentity: PassedPawnResultSemanticIdentity,
    private[chessjudgment] val proofSet: BoundedCausalProofSet,
    private[chessjudgment] val closedReplyInventory: CausalClosedReplyInventoryBinding,
    dependencyFingerprint: String,
    lowerPremiseIds: List[String],
    private[chessjudgment] val occurrenceProof: Option[PassedPawnResultOccurrenceProof] = None
) extends EvidencePayload:
  require(dependencyFingerprint.matches("[0-9a-f]{64}"), "a passed-pawn result needs a complete dependency key")
  require(lowerPremiseIds.nonEmpty && lowerPremiseIds == lowerPremiseIds.distinct.sorted)

  def contract: String = "passed_pawn_result_under_closed_replies"
  def semanticId: String = proofSet.proposition.semanticId
  def occurrenceId: String = proofSet.occurrence.occurrenceId
  def rootLine: LineNodeRef = event.rootLine
  def rootMove: String = EvidenceRef.normalizeMove(event.rootMove)
  def rootPly: Int = sourceBranch.steps.head.step.ply
  def realizingMove: String = EvidenceRef.normalizeMove(assessment.sourceEvent.moveUci)
  def realizingPly: Int = assessment.sourceEvent.step.ply
  def resultPlyOffset: Int = assessment.sourcePlyOffset
  def perspective: chess.Color = event.perspective
  def consequenceKind: TransitionConsequenceKind = assessment.consequence.kind
  def resultTargetSubjects: List[String] = semanticIdentity.resultTargetSubjects
  def rootActor: PassedPawnResultActorOccurrence = event.identity.actor
  def resultActor: PassedPawnResultActorOccurrence = assessment.sourceEvent.identity.actor
  def sourceBranch: CausalBranchOccurrence =
    exactBranch(PassedPawnResultBranchRole.ExpectedResultRoute)
  def replyBranches: List[CausalBranchOccurrence] =
    proofSet.occurrence.branches.filterNot(_.role == PassedPawnResultBranchRole.ExpectedResultRoute)
  def proofPaths: List[CausalProofPathOccurrence] = proofSet.paths
  def legalReplyMoves: List[String] = closedReplyInventory.legalReplyMoves
  def certifiedHorizonPlyOffset: Int = closedReplyInventory.certifiedHorizonPlyOffset

  /** Public views are flattened only from the sealed occurrence proof. They
    * perform no move generation, relation extraction, or fallback analysis.
    */
  def publicBranches: List[PassedPawnResultPublicBranch] =
    proofSet.occurrence.branches.map(publicBranch)

  def publicProofPaths: List[PassedPawnResultPublicProofPath] =
    proofSet.paths.map { path =>
      val manifest = path.manifest match
        case exact: PassedPawnResultPathManifest => exact
        case _ =>
          throw IllegalStateException("a public passed-pawn-result path lost its sealed manifest")
      PassedPawnResultPublicProofPath(
        pathOccurrenceId = path.pathOccurrenceId,
        replyBranchId = manifest.replyBranchId,
        realizationActor = manifest.realization.event.identity.actor,
        realizationMove = manifest.realization.moveUci,
        realizationPly = manifest.realization.event.step.ply,
        realizationMatchKind = manifest.realization.matchKind,
        premises = manifest.supplementalPremiseUses.map {
          case premise: CausalTypedPremiseUse => publicPremise(premise)
          case _ =>
            throw IllegalStateException("a public passed-pawn-result path lost a typed lower premise")
        },
        closureUseIds = path.supplementalClosureUses.map(_.useId)
      )
    }

  def publicClosedReplyInventory: PassedPawnResultPublicClosedReplyInventory =
    PassedPawnResultPublicClosedReplyInventory(
      issuerEvidenceId = closedReplyInventory.issuerEvidenceId,
      coverageEvidenceId = closedReplyInventory.coverageEvidenceId,
      rootAfterFen = closedReplyInventory.rootAfter.fen,
      rootAfterPly = closedReplyInventory.rootAfter.ply,
      scope = snakeCode(closedReplyInventory.scope.toString),
      legalReplyMoves = closedReplyInventory.legalReplyMoves,
      branchByReply = closedReplyInventory.branchByReply,
      certifiedHorizonPlyOffset = closedReplyInventory.certifiedHorizonPlyOffset
    )

  def hasCompleteProofPaths: Boolean =
    assessment.robustness == PassedPawnResultReplyCoverage.AllLegalRepliesRealize &&
      assessment.observations.nonEmpty && assessment.observations.forall(_.outcome == PassedPawnResultBranchOutcome.Realized) &&
      proofPaths.nonEmpty &&
      proofPaths.collect {
        case path if path.manifest.isInstanceOf[PassedPawnResultPathManifest] =>
          path.manifest.asInstanceOf[PassedPawnResultPathManifest].replyBranchId
      }.toSet == replyBranches.map(_.branchId).toSet &&
      closedReplyInventory.branchIds == replyBranches.map(_.branchId).toSet

  private def exactBranch(role: PassedPawnResultBranchRole): CausalBranchOccurrence =
    proofSet.occurrence.branch(role).getOrElse(
      throw IllegalStateException(s"a passed-pawn result lost its '${role.stableKey}' occurrence")
    )

  private[chessjudgment] def exactOccurrenceCertified(record: EvidenceRecord): Boolean =
    occurrenceProof.exists(_.proves(record, this))

  private[chessjudgment] def consumesExactDependencies(
      source: EvidenceRecord,
      comparison: EvidenceRecord,
      inventory: EvidenceRecord,
      resultAssessment: PassedPawnResultReplyAssessment
  ): Boolean =
    occurrenceProof.exists(_.consumesExactDependencies(source, comparison, inventory, resultAssessment))

  private[chessjudgment] def proofParentSources: List[EvidenceRef] =
    occurrenceProof.toList.flatMap(_.parentSources)

  private def publicBranch(branch: CausalBranchOccurrence): PassedPawnResultPublicBranch =
    val (role, replyMove, sourceProbeId) = branch.role match
      case PassedPawnResultBranchRole.ExpectedResultRoute =>
        ("expected_result_route", None, None)
      case PassedPawnResultBranchRole.LegalReply(move, probe) =>
        ("legal_reply", Some(EvidenceRef.normalizeMove(move)), Some(probe))
      case _ =>
        throw IllegalStateException("a public passed-pawn result contains an unsupported branch role")
    PassedPawnResultPublicBranch(
      branchId = branch.branchId,
      role = role,
      replyMove = replyMove,
      sourceProbeId = sourceProbeId,
      lineId = branch.line.id,
      lineRole = snakeCode(branch.line.role.toString),
      lineRank = branch.line.rank,
      lineRootMove = EvidenceRef.normalizeMove(branch.line.rootMove),
      rootProvenance = branch.rootProvenance match
        case CausalRootProvenance.CounterfactualAnalyzedRoot => "counterfactual_analyzed_root"
        case CausalRootProvenance.ObservedGameRoot            => "observed_game_root",
      steps = branch.steps.map { occurrence =>
        val step = occurrence.step
        PassedPawnResultPublicStep(
          index = occurrence.index,
          stepKey = BoundedCausalIdentity.stepKey(step),
          ply = step.ply,
          moveUci = EvidenceRef.normalizeMove(step.moveUci),
          fenBefore = step.fenBefore,
          fenAfter = step.fenAfter,
          lineId = occurrence.line.id,
          lineRole = snakeCode(occurrence.line.role.toString),
          lineRank = occurrence.line.rank,
          lineRootMove = EvidenceRef.normalizeMove(occurrence.line.rootMove),
          provenance = occurrence.provenance match
            case CausalStepProvenance.ObservedGameMove       => "observed_game_move"
            case CausalStepProvenance.CertifiedAnalysisMove => "certified_analysis_move",
          incomingLink = occurrence.linkFromPrevious.map { link =>
            PassedPawnResultPublicLink(
              kind = link.kind match
                case CausalOccurrenceLinkKind.AdjacentLegalReplay => "adjacent_legal_replay"
                case CausalOccurrenceLinkKind.CertifiedCausalDependency => "certified_causal_dependency",
              fromStepKey = link.fromStepKey,
              toStepKey = link.toStepKey,
              occurrenceLinkKey = link.lowerProofKey
            )
          }
        )
      }
    )

  private def publicPremise(premise: CausalTypedPremiseUse): PassedPawnResultPublicPremise =
    PassedPawnResultPublicPremise(
      role = passedPawnResultPremiseRoleCode(premise.role),
      lowerKind = premise.lowerKind,
      lowerSemanticKey = premise.lowerSemanticKey,
      sourcePremiseIds = premise.sourcePremiseIds,
      branchId = premise.branchId,
      branchRole = passedPawnResultBranchRoleCode(premise.branchRole),
      relatedBranchIds = premise.relatedBranchIds,
      fromStepIndex = premise.fromStepIndex,
      toStepIndex = premise.toStepIndex
    )

  private def passedPawnResultPremiseRoleCode(role: CausalPremiseRole): String =
    role match
      case PassedPawnResultPremiseRole.ComparisonDemand         => "comparison_demand"
      case PassedPawnResultPremiseRole.ExpectedDependency(_)    => "expected_dependency"
      case PassedPawnResultPremiseRole.ExpectedResult           => "expected_result"
      case PassedPawnResultPremiseRole.ObservedDependency(_, _) => "observed_dependency"
      case PassedPawnResultPremiseRole.ObservedResult(_)        => "observed_result"
      case PassedPawnResultPremiseRole.FunctionalMatch(_)       => "functional_match"
      case _ =>
        throw IllegalStateException("a public passed-pawn result contains an unsupported premise role")

  private def passedPawnResultBranchRoleCode(role: CausalBranchRole): String =
    role match
      case PassedPawnResultBranchRole.ExpectedResultRoute => "expected_result_route"
      case PassedPawnResultBranchRole.LegalReply(_, _)    => "legal_reply"
      case _ =>
        throw IllegalStateException("a public passed-pawn result contains an unsupported premise branch")

  private def snakeCode(value: String): String =
    value
      .replaceAll("([a-z0-9])([A-Z])", "$1_$2")
      .replace('-', '_')
      .toLowerCase

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

sealed trait CausalResponseContinuationTrajectory:
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
  private[chessjudgment] def relationOccurrenceBinding: ReplayVerticalRelationOccurrenceBinding

final case class PawnBreakFollowUpTrajectory private (
    breakStep: LineReplayStep,
    replyStep: LineReplayStep,
    followUpStep: LineReplayStep,
    interveningSteps: List[LineReplayStep],
    color: Color,
    replyFrom: EvidenceSquare,
    replyTo: EvidenceSquare,
    followUpFrom: EvidenceSquare,
    followUpTo: EvidenceSquare,
    releasedPassedPawn: EvidenceSquare,
    private[chessjudgment] val relationOccurrenceBinding: ReplayVerticalRelationOccurrenceBinding,
    plyOffset: Int
) extends CausalResponseContinuationTrajectory:
  require(
    relationOccurrenceBinding.step == replyStep &&
      relationOccurrenceBinding.contract == VerticalRelationContractKind.PawnTopologyTransition &&
      relationOccurrenceBinding.result.kind == RelationFactKind.PawnTopologyTransition,
    "a released-passer continuation needs its exact L1 pawn-topology transition"
  )
  private[chessjudgment] def pawnTopologyTransitionKey: DerivedRelationResultKey =
    relationOccurrenceBinding.result
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
    val candidates = for
      _ <- Option.when(List(breakStep, replyStep, followUpStep).forall(_.ply > 0))(()).toList
      breakLegal <- replay.legalStep(breakStep).toList
      replyLegal <- replay.legalStep(replyStep).toList
      followUpLegal <- replay.legalStep(followUpStep).toList
      if breakLegal.move.piece.role == Pawn
      if replyLegal.move.piece.role == Pawn && replyLegal.move.piece.color == !breakLegal.move.piece.color
      if replyLegal.move.dest == breakLegal.move.dest &&
        replyLegal.move.capture.contains(breakLegal.move.dest) && replyLegal.capturedRole.contains(Pawn)
      if followUpLegal.move.piece.role == Pawn &&
        followUpLegal.move.piece.color == breakLegal.move.piece.color
      if interveningSteps.headOption.contains(replyStep)
      if followUpStep.ply - breakStep.ply == interveningSteps.size + 1
      occurrence <- replay.verticalRelationOccurrences(
        replyStep,
        List(VerticalRelationContractKind.PawnTopologyTransition)
      )
      detail <- occurrence.relation.detail match
        case exact: RelationWitnessDetail.PawnTopologyTransition => List(exact)
        case _ => Nil
      before <- detail.before.toList
      after <- detail.after.toList
      releasedPassedPawn = EvidenceSquare(followUpLegal.move.orig.key)
      if samePawn(before, breakLegal.move.piece.color, releasedPassedPawn) && !before.passed
      if samePawn(after, breakLegal.move.piece.color, releasedPassedPawn) && after.passed
      if detail.changedFacets.contains(RelationPawnTopologyFacet.Passed)
      if interveningSteps.drop(1).forall(step =>
        preservesReleasedPasser(replay, step, breakLegal.move.piece.color, releasedPassedPawn)
      )
    yield PawnBreakFollowUpTrajectory(
      breakStep = breakStep,
      replyStep = replyStep,
      followUpStep = followUpStep,
      interveningSteps = interveningSteps,
      color = breakLegal.move.piece.color,
      replyFrom = EvidenceSquare(replyLegal.move.orig.key),
      replyTo = EvidenceSquare(replyLegal.move.dest.key),
      followUpFrom = EvidenceSquare(followUpLegal.move.orig.key),
      followUpTo = EvidenceSquare(followUpLegal.move.dest.key),
      releasedPassedPawn = releasedPassedPawn,
      relationOccurrenceBinding = ReplayVerticalRelationOccurrenceBinding.from(occurrence),
      plyOffset = followUpStep.ply - breakStep.ply
    )

    candidates match
      case exact :: Nil => Some(exact)
      case _            => None

  private def preservesReleasedPasser(
      replay: CanonicalLineReplay,
      step: LineReplayStep,
      side: Color,
      square: EvidenceSquare
  ): Boolean =
    replay.transition(step).exists { _ =>
      val matching = replay.verticalRelationOccurrences(
        step,
        List(VerticalRelationContractKind.PawnTopologyTransition)
      ).flatMap { occurrence =>
        occurrence.relation.detail match
          case exact: RelationWitnessDetail.PawnTopologyTransition
              if exact.before.exists(samePawn(_, side, square)) =>
            List(exact)
          case _ => Nil
      }
      matching match
        case Nil => true
        case exact :: Nil => exact.after.exists(state => samePawn(state, side, square) && state.passed)
        case _ => false
    }

  private def samePawn(
      state: RelationPawnTopologyStateWitness,
      side: Color,
      square: EvidenceSquare
  ): Boolean =
    state.side == side && state.square.key.equalsIgnoreCase(square.key)

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
    private[chessjudgment] val relationOccurrenceBinding: ReplayVerticalRelationOccurrenceBinding
) extends CausalResponseContinuationTrajectory:
  require(
    relationOccurrenceBinding.step == replyStep &&
      relationOccurrenceBinding.contract == VerticalRelationContractKind.CaptureRecaptureInventory &&
      relationOccurrenceBinding.result.kind == RelationFactKind.CaptureRecaptureInventory,
    "a capture-response continuation needs its exact L1 recapture inventory occurrence"
  )
  private[chessjudgment] def recaptureInventoryKey: DerivedRelationResultKey =
    relationOccurrenceBinding.result
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
      if replyLegal.move.captures &&
        replyLegal.capturedRole.exists(_.name.equalsIgnoreCase(triggerLegal.move.piece.role.name)) &&
        replyLegal.move.dest == triggerLegal.move.dest
      (recaptureOccurrence, _) <- replay.exactRecaptureOccurrenceMembership(replyStep, followUpStep).toList
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
      relationOccurrenceBinding = ReplayVerticalRelationOccurrenceBinding.from(recaptureOccurrence)
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

enum LineEventKind:
  case Capture
  case Recapture
  case CheckEvasion
  case Castling
  case Check
  case Mate
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

object LineConsequenceKind:


  def tacticalDriver(kind: LineConsequenceKind): Boolean =
    kind match
      case LineConsequenceKind.MaterialGain | LineConsequenceKind.MaterialLoss |
          LineConsequenceKind.RecaptureSequence | LineConsequenceKind.RecoveryWindow |
          LineConsequenceKind.ImmediateReplyCheck | LineConsequenceKind.Mate |
          LineConsequenceKind.DrawResource | LineConsequenceKind.Promotion =>
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

final case class LineMoveOccurrence(
    moveUci: String,
    plyOffset: Int
):
  require(
    moveUci.matches("^[a-h][1-8][a-h][1-8][qrbn]?$") &&
      moveUci == EvidenceRef.normalizeMove(moveUci),
    "a line move occurrence needs one canonical UCI move"
  )
  require(plyOffset >= 0, "a line move occurrence needs a root-relative ply")

  def stableKey: String = s"$plyOffset:$moveUci"

  def sameOccurrence(otherPlyOffset: Int, otherMoveUci: String): Boolean =
    plyOffset == otherPlyOffset && EvidenceRef.sameMove(moveUci, otherMoveUci)

final case class LineConsequence(
    kind: LineConsequenceKind,
    proofOccurrences: List[LineMoveOccurrence],
    directCauseProjectionEligible: Boolean,
    eventOccurrence: Option[LineMoveOccurrence] = None,
    rootMove: Option[String] = None,
    rootSide: Option[Color] = None,
    beneficiary: Option[Color] = None,
    sacrificeOccurrence: Option[LineSacrificeOccurrence] = None,
    materialOutcome: Option[RootOwnedMaterialOutcome] = None
):
  require(
    proofOccurrences.map(_.stableKey).distinct.size == proofOccurrences.size &&
      proofOccurrences.map(_.plyOffset).distinct.size == proofOccurrences.size &&
      proofOccurrences.map(_.plyOffset) == proofOccurrences.map(_.plyOffset).sorted,
    "line consequence proof occurrences must be exact, unique, and ordered"
  )
  require(
    eventOccurrence.forall(event =>
      proofOccurrences.exists(_.sameOccurrence(event.plyOffset, event.moveUci))
    ),
    "a line consequence event must belong to its exact proof occurrence path"
  )
  require(
    !directCauseProjectionEligible || eventOccurrence.nonEmpty,
    "a direct line consequence needs an exact event occurrence"
  )
  require(
    !directCauseProjectionEligible || eventOccurrence.exists(event =>
      proofOccurrences.lastOption.exists(_.sameOccurrence(event.plyOffset, event.moveUci))
    ),
    "a direct line consequence event must terminate its proof occurrence path"
  )

  def hasProofOccurrence(plyOffset: Int, moveUci: String): Boolean =
    proofOccurrences.exists(_.sameOccurrence(plyOffset, moveUci))

  def eventMatches(plyOffset: Int, moveUci: String): Boolean =
    eventOccurrence.exists(_.sameOccurrence(plyOffset, moveUci))

  def rootMoveMatched(rootMove: String): Boolean =
    this.rootMove.exists(move => EvidenceRef.sameMove(move, rootMove))

  /** Exact stationary projection. Cardinality is zero or one because every
    * sacrifice consequence owns one occurrence.
    */
  def stationarySacrificeCaptures: List[LineMaterialCapture] =
    sacrificeOccurrence.filter(_.stationary).map(_.acceptance).toList

final case class LineConsequenceProfile(
    directCauseProjectionEligibleKinds: List[LineConsequenceKind],
    hasDirectCauseProjectionEligibleConsequence: Boolean,
    hasMaterialResult: Boolean,
    hasRecaptureRecovery: Boolean,
    hasSacrifice: Boolean,
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
  require(
    events.forall(_.capture.forall(
      _.recaptureStatus != LineMaterialRecaptureStatus.Unknown
    )),
    "a material summary cannot publish an unresolved recapture occurrence"
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
    capturesByMover.filter(capture => directCauseProjectionEligibleCapturedRole(capture.capturedRole))

  def nonPawnCapturesByOpponent: List[LineMaterialCapture] =
    capturesByOpponent.filter(capture => directCauseProjectionEligibleCapturedRole(capture.capturedRole))

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

  def hasDirectCauseProjectionEligibleMaterialGain: Boolean =
    closedNetCpForMover.exists(_ > 0) && (nonPawnCapturesByMover.nonEmpty || hasPromotionGainForMover)

  def hasDirectCauseProjectionEligibleMaterialLoss: Boolean =
    closedNetCpForMover.exists(_ < 0) && (nonPawnCapturesByOpponent.nonEmpty || hasPromotionLossForMover)

  def hasUnrecoveredPawnGainForMover: Boolean =
    closedNetCpForMover.exists(_ > 0) && pawnCapturesByMover.nonEmpty

  def hasUnrecoveredPawnLossForMover: Boolean =
    closedNetCpForMover.exists(_ < 0) && pawnCapturesByOpponent.nonEmpty

  def hasDirectCauseProjectionEligibleMaterialEvent: Boolean =
    hasDirectCauseProjectionEligibleMaterialGain ||
      hasDirectCauseProjectionEligibleMaterialLoss ||
      hasUnrecoveredPawnGainForMover ||
      hasUnrecoveredPawnLossForMover ||
      hasResolvedMaterialSequence

  private[chessjudgment] def sacrificeResponsesFor(
      capture: LineMaterialCapture
  ): List[LineMaterialCapture] =
    exactCaptureInventory.toList
      .flatten
      .filter(response => LineMaterialSummary.materialSacrificePair(capture, response))

  private def directCauseProjectionEligibleCapturedRole(role: EvidencePieceRole): Boolean =
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

  private[chessjudgment] def fromNode(node: PassedPawnResultEventNode): Option[RootCausalActor] =
    node.certifiedLegalStep.flatMap(fromLegalStep(node.moveUci, _))

  private[chessjudgment] def fromPassedPawnResultEvent(
      event: PassedPawnResultEventEvidence
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
    cause: LineMoveOccurrence,
    effect: LineMoveOccurrence,
    anchor: EvidenceSquare
):
  require(
    cause.plyOffset <= effect.plyOffset,
    "a root causal link cannot point backward in its line"
  )
  require(
    kind == RootCausalLinkKind.ImmediateRootAction || cause.plyOffset < effect.plyOffset,
    "only an immediate root action may link one occurrence to itself"
  )

final case class RootOwnedCausalEpisode private (
    line: LineNodeRef,
    actor: RootCausalActor,
    target: EvidenceSquare,
    links: List[RootCausalLink],
    consequence: LineConsequence,
    chain: List[LineMoveOccurrence]
):
  require(links.nonEmpty, "a root-owned causal episode needs a verified causal link")
  require(chain.nonEmpty, "a root-owned causal episode needs a replay chain")
  require(
    chain.map(_.plyOffset) == chain.indices.toList,
    "a root-owned causal episode needs a contiguous root-relative replay chain"
  )
  require(
    consequence.eventOccurrence.exists(event =>
      chain.lastOption.exists(_.sameOccurrence(event.plyOffset, event.moveUci))
    ),
    "a root-owned causal episode must terminate at its consequence occurrence"
  )
  require(
    links.forall(link =>
      chain.exists(_.sameOccurrence(link.cause.plyOffset, link.cause.moveUci)) &&
        chain.exists(_.sameOccurrence(link.effect.plyOffset, link.effect.moveUci))
    ),
    "a root causal link must connect exact occurrences in its episode chain"
  )

  def eventOccurrence: LineMoveOccurrence = chain.last

  def forcingTacticalResource(lineFacts: LineFactEvidence): Boolean =
    RootOwnedEffectPolicy.admitsLineEpisode(lineFacts, this) &&
      consequence.directCauseProjectionEligible &&
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
          role = EvidencePieceRole(step.move.piece.role.name),
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
    val event = episode.eventOccurrence
    for
      replayStep <- replay.lift(event.plyOffset)
      if event.sameOccurrence(event.plyOffset, replayStep.moveUci)
      capture <- uniqueMaterialCaptureAt(event.plyOffset, replayStep.moveUci)
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
    val lastingGainOccurrences = consequences.collect {
      case consequence
          if Set(LineConsequenceKind.MaterialGain, LineConsequenceKind.MaterialLoss)(consequence.kind) &&
            consequence.beneficiary.contains(side) =>
        consequence.eventOccurrence
    }.flatten
    lastingGainOccurrences.flatMap(occurrence =>
      uniqueMaterialCaptureAt(occurrence.plyOffset, occurrence.moveUci).filter(_.side == side)
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
  def hasDirectCauseProjectionEligibleMaterialEvent: Boolean =
    material.exists(_.hasDirectCauseProjectionEligibleMaterialEvent)
  def hasSacrificeMaterialEvent: Boolean =
    rootMove.exists(root => sacrificeOccurrencesForRootMove(root).nonEmpty)
  def directCauseProjectionEligibleConsequences: List[LineConsequence] =
    consequences.filter(_.directCauseProjectionEligible)
  private[chessjudgment] def proofConsequenceCandidatesForRootMove(
      rootMoveUci: String
  ): List[LineConsequence] =
    (
      directCauseProjectionEligibleConsequences ++
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
    directCauseProjectionEligibleConsequences.filter(_.rootMoveMatched(rootMoveUci))
  def immediateReplyCheckLiabilitiesForRootMove(rootMoveUci: String): List[LineConsequence] =
    consequences.filter(consequence =>
        consequence.kind == LineConsequenceKind.ImmediateReplyCheck &&
        consequence.rootMoveMatched(rootMoveUci) &&
        consequence.rootSide.nonEmpty &&
        (consequence.proofOccurrences match
          case root :: replyOccurrence :: _ =>
            root.sameOccurrence(0, rootMoveUci) &&
              replyOccurrence.plyOffset == 1 &&
              reply.exists(EvidenceRef.sameMove(_, replyOccurrence.moveUci))
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
  def hasDirectCauseProjectionEligibleConsequence: Boolean =
    directCauseProjectionEligibleConsequences.nonEmpty
  def directCauseProjectionEligibleConsequenceKinds: List[LineConsequenceKind] =
    directCauseProjectionEligibleConsequences.map(_.kind)
  def hasDirectCauseProjectionEligibleConsequence(kind: LineConsequenceKind): Boolean =
    directCauseProjectionEligibleConsequenceKinds.contains(kind)
  def consequenceProfile: LineConsequenceProfile =
    val kinds = directCauseProjectionEligibleConsequenceKinds
    LineConsequenceProfile(
      directCauseProjectionEligibleKinds = kinds,
      hasDirectCauseProjectionEligibleConsequence = kinds.nonEmpty,
      hasMaterialResult = kinds.exists {
        case LineConsequenceKind.MaterialGain | LineConsequenceKind.MaterialLoss |
            LineConsequenceKind.Sacrifice | LineConsequenceKind.Promotion =>
          true
        case _ =>
          false
      },
      hasRecaptureRecovery = kinds.exists(kind =>
        kind == LineConsequenceKind.RecaptureSequence || kind == LineConsequenceKind.RecoveryWindow
      ),
      hasSacrifice = kinds.contains(LineConsequenceKind.Sacrifice),
      hasMate = kinds.contains(LineConsequenceKind.Mate),
      hasDrawResource = kinds.contains(LineConsequenceKind.DrawResource)
    )
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
      directCauseProjectionEligibleConsequenceKinds.map(kind =>
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
              chain = steps.take(eventPlyOffset + 1).zipWithIndex.map { case (step, plyOffset) =>
                LineMoveOccurrence(EvidenceRef.normalizeMove(step.moveUci), plyOffset)
              }
            )
          }
        }
      }.distinct
    }

  private def eventPlyOffsets(
      line: LineFactEvidence,
      consequence: LineConsequence
  ): List[Int] =
    consequence.eventOccurrence.toList.flatMap(occurrence =>
      line.lineReplaySteps.lift(occurrence.plyOffset).toList.collect {
        case step if occurrence.sameOccurrence(occurrence.plyOffset, step.moveUci) => occurrence.plyOffset
      }
    )

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
      case LineConsequenceKind.Promotion =>
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
    val mover = eventTransition.map(_.relationDelta.rootMove)
    val afterInventory = eventTransition.map(_.afterAnalysis.relationInventory)
    val capture = line.uniqueMaterialCaptureAt(eventPlyOffset, move)
    consequence.kind match
      case LineConsequenceKind.MaterialGain =>
        capture.exists(captured =>
          consequence.eventMatches(eventPlyOffset, captured.moveUci) &&
            consequence.beneficiary.contains(captured.side) && captured.side == rootColor
        )
      case LineConsequenceKind.MaterialLoss =>
        capture.exists(captured =>
          consequence.eventMatches(eventPlyOffset, captured.moveUci) &&
            consequence.beneficiary.contains(captured.side) && captured.side != rootColor
        )
      case LineConsequenceKind.RecaptureSequence =>
        capture.exists(actual =>
          actual.recapture &&
            consequence.eventMatches(eventPlyOffset, actual.moveUci) &&
            consequence.rootSide.contains(rootColor) &&
            consequence.beneficiary.contains(actual.side)
        )
      case LineConsequenceKind.RecoveryWindow =>
        (line.durableRecoveryCaptureForMover, capture) match
          case (Some(expected), Some(actual)) =>
            expected == actual &&
              expected.side == rootColor &&
              consequence.eventMatches(eventPlyOffset, expected.moveUci) &&
              consequence.rootSide.contains(rootColor) &&
              consequence.beneficiary.contains(rootColor)
          case _ =>
            false
      case LineConsequenceKind.ImmediateReplyCheck =>
        eventPlyOffset == 1 && mover.exists(_.side != rootColor) && afterInventory.exists { inventory =>
          val state = inventory.stateView
          state.inCheck(state.sideToMove)
        }
      case LineConsequenceKind.Mate =>
        mover.exists(movement => consequence.beneficiary.contains(movement.side)) && afterInventory.exists(
          _.kingTerminalState == PositionRelationExtractor.ClosedKingTerminalState.Checkmate
        )
      case LineConsequenceKind.DrawResource =>
        afterInventory.exists(
          _.kingTerminalState == PositionRelationExtractor.ClosedKingTerminalState.Stalemate
        )
      case LineConsequenceKind.Promotion =>
        consequence.eventMatches(eventPlyOffset, eventStep.moveUci) && eventTransition.exists(transition =>
          val movement = transition.relationDelta.rootMove
          consequence.beneficiary.contains(movement.side) &&
            movement.beforeRole.name.equalsIgnoreCase(Pawn.name) &&
            !movement.afterRole.name.equalsIgnoreCase(Pawn.name)
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
          LineMoveOccurrence(rootMove, 0),
          LineMoveOccurrence(eventMove, eventPlyOffset),
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
            LineMoveOccurrence(rootMove, 0),
            LineMoveOccurrence(eventMove, eventPlyOffset),
            eventAnchor
          ))
        val lineAccess =
          continuousLineAccessSeedLink(line, eventPlyOffset, replay)
        val forcedCaptureResponse =
          forcedCaptureResponseLink(line, rootStep, eventStep, eventPlyOffset, replay)
        val actorCaptured =
          Option.when(consequence.kind == LineConsequenceKind.MaterialLoss)(
            rootActorCapturedSeedLink(line, actor, eventPlyOffset, replay)
          ).flatten
        val materialSequence =
          materialSequenceLinks(line, rootStep, actor, consequence, eventPlyOffset, replay)
        (List(actorAction, lineAccess, forcedCaptureResponse, actorCaptured).flatten ++
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
        LineMoveOccurrence(rootMove, 0),
        LineMoveOccurrence(eventMove, eventPlyOffset),
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
      LineMoveOccurrence(EvidenceRef.normalizeMove(rootStep.moveUci), 0),
      LineMoveOccurrence(EvidenceRef.normalizeMove(eventStep.moveUci), eventPlyOffset),
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
      LineMoveOccurrence(EvidenceRef.normalizeMove(rootStep.moveUci), 0),
      LineMoveOccurrence(EvidenceRef.normalizeMove(eventStep.moveUci), eventPlyOffset),
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
    val consequenceOccurrences = consequence.proofOccurrences.map(occurrence =>
      occurrence.plyOffset -> occurrence.moveUci
    ).toSet
    val capturesByPly = line.materialCaptures
      .filter(capture =>
        capture.plyOffset > 0 &&
          capture.plyOffset <= eventPlyOffset &&
          consequenceOccurrences.contains(
            capture.plyOffset -> EvidenceRef.normalizeMove(capture.moveUci)
          )
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
              LineMoveOccurrence(rootMove, 0),
              LineMoveOccurrence(eventMove, eventPlyOffset),
              movement.to
            ))
        List(
          forcedCaptureResponseLink(line, rootStep, eventStep, eventPlyOffset, replay),
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
            LineMoveOccurrence(EvidenceRef.normalizeMove(fromStep.moveUci), fromCapture.plyOffset),
            LineMoveOccurrence(EvidenceRef.normalizeMove(toStep.moveUci), toCapture.plyOffset),
            trajectory.rootTo
          ))
      val actorCaptured = materialActorCaptured(line, fromCapture, toCapture, replay).map(anchor => RootCausalLink(
        RootCausalLinkKind.MaterialCaptureResponse,
        LineMoveOccurrence(EvidenceRef.normalizeMove(fromStep.moveUci), fromCapture.plyOffset),
        LineMoveOccurrence(EvidenceRef.normalizeMove(toStep.moveUci), toCapture.plyOffset),
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
    def ownsOccurrence(occurrence: LineMoveOccurrence): Boolean =
      replay.replaySteps
        .lift(occurrence.plyOffset)
        .exists(step => occurrence.sameOccurrence(occurrence.plyOffset, step.moveUci))
    require(
      consequences.forall(consequence =>
        consequence.proofOccurrences.forall(ownsOccurrence) &&
          consequence.eventOccurrence.forall(ownsOccurrence)
      ),
      "a certified line consequence must retain only occurrences from its canonical replay"
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

/** Typed chess objects carried by structural consequences.
  * `label` is a one-way presentation projection; no proof code may recover
  * chess semantics by parsing it.
  */
enum StructuralSubject:
  case OpenFile(file: EvidenceFile)
  case SemiOpenFile(side: Color, file: EvidenceFile)
  case PieceAt(side: Color, role: EvidencePieceRole, square: EvidenceSquare)
  case PawnTensionCreated(side: Color, from: EvidenceSquare, to: EvidenceSquare)
  case PawnTensionResolved(side: Color, from: EvidenceSquare, to: EvidenceSquare)
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
      case PawnTensionCreated(side, from, to) =>
        s"created-tension:${side.toString.toLowerCase}:${from.key.toLowerCase}-${to.key.toLowerCase}"
      case PawnTensionResolved(side, from, to) =>
        s"resolved-tension:${side.toString.toLowerCase}:${from.key.toLowerCase}-${to.key.toLowerCase}"
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
      case PawnTensionCreated(_, from, to) => List(from, to)
      case PawnTensionResolved(_, from, to) => List(from, to)
      case PassedPawnCreated(_, square) => List(square)
      case PassedPawnLost(_, square) => List(square)
      case PassedPawnAdvanced(_, from, to, _) => List(from, to)
      case PassedStatusCreated(_, from, to, _) => List(from, to)
      case PassedPawnPromoted(_, from, to) => List(from, to)

  def targetSquares: List[EvidenceSquare] =
    this match
      case OpenFile(_) | SemiOpenFile(_, _) => Nil
      case PieceAt(_, _, square) => List(square)
      case PawnTensionCreated(_, from, to) => List(from, to)
      case PawnTensionResolved(_, from, to) => List(from, to)
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

  def stableKey(subject: StructuralSubject): String =
    subject match
      case OpenFile(file) => key("open-file", file.key)
      case SemiOpenFile(side, file) => key("semi-open-file", side.toString, file.key)
      case PieceAt(side, role, square) => key("piece-at", side.toString, role.name, square.key)
      case PawnTensionCreated(side, from, to) => key("pawn-tension-created", side.toString, from.key, to.key)
      case PawnTensionResolved(side, from, to) => key("pawn-tension-resolved", side.toString, from.key, to.key)
      case PassedPawnCreated(side, square) => key("passed-pawn-created", side.toString, square.key)
      case PassedPawnLost(side, square) => key("passed-pawn-lost", side.toString, square.key)
      case PassedPawnAdvanced(side, from, to, rank) =>
        key("passed-pawn-advanced", side.toString, from.key, to.key, rank.toString)
      case PassedStatusCreated(side, from, to, rank) =>
        key("passed-status-created", side.toString, from.key, to.key, rank.toString)
      case PassedPawnPromoted(side, from, to) =>
        key("passed-pawn-promoted", side.toString, from.key, to.key)

enum TransitionConsequenceKind:
  case OpenFileEstablished
  case SemiOpenFileEstablished
  case PawnTensionCreated
  case PawnTensionResolution
  case PassedPawnProgress
  case PassedPawnStatusRemoved

object TransitionConsequenceKind:
  private val RootActorBound = Set(PassedPawnProgress)

  private val EstablishedStates = Set(
    OpenFileEstablished,
    SemiOpenFileEstablished,
    PawnTensionCreated,
    PassedPawnProgress
  )

  private val RemovedStates = Set(
    PawnTensionResolution,
    PassedPawnStatusRemoved
  )

  def requiresRootActorSurvival(kind: TransitionConsequenceKind): Boolean =
    RootActorBound(kind)

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
      "a derived result key must identify a certified transition or vertical result"
    )
    DerivedRelationResultKey(relation.kind, relation.semanticId)

private[chessjudgment] final case class TransitionResultPremiseSource(
    key: DerivedRelationResultKey,
    source: EvidenceRef
)

final case class StructuralSubjectBinding private[chessjudgment] (
    subject: StructuralSubject,
    relationKeys: List[RelationChangeKey]
):
  require(relationKeys.distinct.size == relationKeys.size, "duplicate structural-subject relation keys")
  def stableKey: String =
    s"${subject.stableKey}:relations:${relationKeys.map(_.stableKey).mkString("[", ",", "]")}"

object StructuralSubjectBinding:
  private[chessjudgment] def fromRelations(
      subject: StructuralSubject,
      relationKeys: List[RelationChangeKey]
  ): StructuralSubjectBinding =
    require(relationKeys.nonEmpty, "a relation-derived structural subject requires exact relation keys")
    require(
      relationKeys.distinct.size == relationKeys.size,
      "a relation-derived structural subject cannot hide duplicate proof keys"
    )
    StructuralSubjectBinding(subject, relationKeys.sortBy(_.stableKey))

final case class TransitionConsequence private[chessjudgment] (
    kind: TransitionConsequenceKind,
    subjectBindings: List[StructuralSubjectBinding] = Nil,
    targetBindings: List[StructuralSubjectBinding] = Nil,
    private[chessjudgment] val resultPremiseKeys: List[DerivedRelationResultKey] = Nil
):
  require(subjectBindings.nonEmpty, "a transition consequence needs at least one exact subject")
  require(subjectBindings.distinct.size == subjectBindings.size, "duplicate structural consequence subject bindings")
  require(targetBindings.distinct.size == targetBindings.size, "duplicate structural consequence target bindings")
  require(
    resultPremiseKeys == resultPremiseKeys.distinct.sortBy(_.stableKey),
    "transition-result premise keys must be unique and canonically ordered"
  )
  def subjectFacts: List[StructuralSubject] = subjectBindings.map(_.subject)
  def targetSubjectFacts: List[StructuralSubject] = targetBindings.map(_.subject)
  private[chessjudgment] def relationKeys: List[RelationChangeKey] =
    (subjectBindings ++ targetBindings).flatMap(_.relationKeys).distinct.sortBy(_.stableKey)
  require(subjectFacts.distinct.size == subjectFacts.size, "duplicate structural consequence subjects")
  require(targetSubjectFacts.distinct.size == targetSubjectFacts.size, "duplicate structural consequence targets")

  def establishesState: Boolean =
    TransitionConsequenceKind.establishesState(kind)
  def removesState: Boolean =
    TransitionConsequenceKind.removesState(kind)
  def anchorKey: String =
    kind.toString
  private[chessjudgment] def stableKey: String =
    PassedPawnResultProofKey.product(
      "transition-consequence",
      List(
        kind.toString.toLowerCase,
        PassedPawnResultProofKey.sequence(subjectBindings.map(_.stableKey).distinct.sorted),
        PassedPawnResultProofKey.sequence(targetBindings.map(_.stableKey).distinct.sorted),
        PassedPawnResultProofKey.sequence(resultPremiseKeys.map(_.stableKey))
      )
    )
  private[chessjudgment] def resultSubjectFacts: List[StructuralSubject] =
    if targetSubjectFacts.nonEmpty then targetSubjectFacts else subjectFacts
  private[chessjudgment] def witnessSubjectFacts: List[StructuralSubject] =
    if targetSubjectFacts.isEmpty then Nil else subjectFacts.filterNot(targetSubjectFacts.toSet)
  private[chessjudgment] def resultSubjectBindings: List[StructuralSubjectBinding] =
    if targetBindings.nonEmpty then targetBindings else subjectBindings
  private[chessjudgment] def witnessSubjectBindings: List[StructuralSubjectBinding] =
    if targetBindings.isEmpty then Nil else subjectBindings.filterNot(targetBindings.toSet)

private[chessjudgment] object TransitionConsequenceBindingProof:
  def provesCanonical(
      consequences: List[TransitionConsequence],
      changes: List[CanonicalRelationChange],
      transition: StructuralTransitionBinding,
      canonicalDelta: CanonicalRelationDelta
  ): Boolean =
    val bindings = consequences.flatMap(consequence =>
      consequence.subjectBindings ++ consequence.targetBindings
    )
    val expectedKeys = bindings.flatMap(_.relationKeys).toSet
    val canonicalByKey = canonicalDelta.changes.map(change => change.key -> change).toMap
    consequences.nonEmpty &&
      consequences.distinct.size == consequences.size &&
      bindings.nonEmpty &&
      bindings.forall(_.relationKeys.nonEmpty) &&
      changes.map(_.key).distinct.size == changes.size &&
      changes.map(_.key).toSet == expectedKeys &&
      expectedKeys.subsetOf(canonicalByKey.keySet) &&
      changes.forall { change =>
        val owner = change.direction match
          case RelationChangeDirection.Removed     => transition.from
          case RelationChangeDirection.Established => transition.to
        change.source.position == owner &&
          canonicalByKey.get(change.key).contains(change)
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
  def fromPosition(
      detail: RelationWitnessDetail,
      dependencyFootprint: RelationDependencyFootprint
  ): CanonicalRelationFact =
    require(
      RelationWitnessDetail.proofStage(detail) == RelationProofStage.PositionFact,
      "only an L0 producer may attach a board dependency footprint"
    )
    build(detail, Nil, Some(dependencyFootprint))

  def from(
      detail: RelationWitnessDetail,
      lineMoves: List[String]
  ): CanonicalRelationFact =
    require(
      RelationWitnessDetail.proofStage(detail) != RelationProofStage.PositionFact,
      "L0 facts must come from their producer with an exact dependency footprint"
    )
    build(detail, lineMoves, None)

  private def build(
      detail: RelationWitnessDetail,
      lineMoves: List[String],
      dependencyFootprint: Option[RelationDependencyFootprint]
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
      dependencyFootprint = dependencyFootprint
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
      case RelationWitnessDetail.GeometricControlSetDelta(mover, _, _, _, _, _, _, _, _, _) =>
        moved(mover.side, mover.from, mover.to, Some(mover.beforeRole), Some(mover.afterRole))
      case RelationWitnessDetail.CaptureRecaptureInventory(mover, _, _, _, _, _) =>
        moved(mover.side, mover.from, mover.to, Some(mover.beforeRole), Some(mover.afterRole))
      case RelationWitnessDetail.CreatedCheckResponseInventory(mover, _, _, _, _, _, _, _) =>
        moved(mover.side, mover.from, mover.to, Some(mover.beforeRole), Some(mover.afterRole))
      case RelationWitnessDetail.RootCheckResponse(mover, _, _, _, _, _) =>
        moved(mover.side, mover.from, mover.to, Some(mover.beforeRole), Some(mover.afterRole))
      case RelationWitnessDetail.SliderReachDelta(mover, _, _, _, _, _, _, _) =>
        moved(mover.side, mover.from, mover.to, Some(mover.beforeRole), Some(mover.afterRole))
      case RelationWitnessDetail.PawnTopologyTransition(mover, _, _, _, _) =>
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
        Nil
      case LineConsequenceKind.Mate =>
        List(TacticalMechanismKind.KingForcing)
      case LineConsequenceKind.DrawResource =>
        List(TacticalMechanismKind.DrawResource)
      case LineConsequenceKind.Promotion =>
        List(TacticalMechanismKind.PawnPromotion)
      case LineConsequenceKind.ForcedTheme | LineConsequenceKind.Sacrifice =>
        Nil

  def relativeCauseKind(
      kind: TacticalMechanismKind,
      badLoss: Boolean,
      playedCandidate: Boolean
  ): Option[RelativeCauseKind] =
    kind match
      case TacticalMechanismKind.KingForcing =>
        Some(RelativeCauseKind.KingForcing)
      case TacticalMechanismKind.RecaptureChoice =>
        Some(
          if badLoss then
            if playedCandidate then RelativeCauseKind.TacticalRefutationOfPlayed
            else RelativeCauseKind.CandidateTacticalLiability
          else RelativeCauseKind.RecaptureRecoveryWindow
        )
      case TacticalMechanismKind.DrawResource =>
        Some(RelativeCauseKind.DrawResource)
      case TacticalMechanismKind.DefensiveResource =>
        None
      case TacticalMechanismKind.MaterialGain | TacticalMechanismKind.PawnPromotion =>
        Some(
          if badLoss then
            if playedCandidate then RelativeCauseKind.TacticalRefutationOfPlayed
            else RelativeCauseKind.CandidateTacticalLiability
          else RelativeCauseKind.MissedTacticalResource
        )

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
    consequences: List[TransitionConsequence],
    private[chessjudgment] val relationChanges: List[CanonicalRelationChange],
    private[chessjudgment] val resultPremiseSources: List[TransitionResultPremiseSource],
    private[chessjudgment] val canonicalTransitionProof: Option[CanonicalTransitionProof],
    private[chessjudgment] val canonicalDeltaProof: Option[CanonicalTransitionDeltaProof],
    private[chessjudgment] val replayStructuralOccurrence: Option[ReplayStructuralOccurrence] = None
) extends EvidencePayload:
  import TransitionConsequenceKind.*

  private lazy val certifiedTransitionProof: Option[CanonicalTransitionProof] =
    canonicalTransitionProof.filter(_.proves(transition))

  private[chessjudgment] lazy val canonicalOutputShapeCertified: Boolean =
    canonicalDeltaProof.exists(_.proves(this)) && replayStructuralOccurrence.exists(occurrence =>
      occurrence.step.ply == transition.to.ply &&
        EvidenceRef.sameMove(occurrence.step.moveUci, transition.moveUci) &&
        PrincipalVariationEvidence.sameBoardState(occurrence.step.fenBefore, transition.from.fen) &&
        PrincipalVariationEvidence.sameBoardState(occurrence.step.fenAfter, transition.to.fen) &&
        occurrence.consequences == consequences &&
        occurrence.resultPremiseOccurrences.map(_.result) == resultPremiseSources.map(_.key)
    )

  def moveUci: String = transition.moveUci
  def role: TransitionEdgeRole = transition.role
  def from: PositionNodeRef = transition.from
  def to: PositionNodeRef = transition.to
  def line: Option[LineNodeRef] = transition.line
  def perspective: Color = transition.perspective
  def consequenceAnchors: List[String] = consequences.map(_.anchorKey).distinct
  def hasConsequenceCategory(category: TransitionConsequenceCategory): Boolean =
    consequences.exists(consequence => StructuralDeltaEvidence.hasConsequenceCategory(consequence.kind, category))
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
      PassedPawnStatusRemoved -> Set(PawnStructure, PawnStructureDelta)
    )

enum PassedPawnResultDependencyKind:
  case ObjectStatePrecondition
  case LineAccessPrecondition
  case ResponseContinuationPrecondition

enum PassedPawnResultDependencyProof:
  case ObjectState(trajectory: LineObjectTrajectory)
  case LineAccess(trajectory: LineAccessTrajectory)
  case ResponseContinuation(trajectory: CausalResponseContinuationTrajectory)

final case class PassedPawnResultEventNode private[chessjudgment] (
    identity: PassedPawnResultEventIdentity,
    step: LineReplayStep,
    perspective: Color,
    structuralConsequences: List[TransitionConsequence],
    private[chessjudgment] val lineOccurrenceOwner: EvidenceRef,
    private[chessjudgment] val structuralOccurrence: ReplayStructuralOccurrence,
    private[chessjudgment] val structuralTransition: StructuralTransitionBinding,
    private[chessjudgment] val canonicalStep: Option[LegalReplayStep] = None,
    private[chessjudgment] val canonicalMovement: Option[CanonicalRootLegalMove] = None
):
  def moveUci: String = EvidenceRef.normalizeMove(step.moveUci)
  require(
    moveUci.nonEmpty && EvidenceRef.sameMove(identity.rootMove, moveUci),
    "passed-pawn-result-causal event identity must reference the event replay step"
  )
  require(
    lineOccurrenceOwner.producer == EvidenceProducer.LegalLineProducer &&
      lineOccurrenceOwner.layer == EvidenceLayer.Line &&
      lineOccurrenceOwner.line == structuralTransition.line &&
      lineOccurrenceOwner.line.exists(line => lineOccurrenceOwner.scope == line.role.scope) &&
      EvidenceRef.sameMove(structuralTransition.moveUci, step.moveUci) &&
      structuralTransition.from.ply == step.ply - 1 &&
      structuralTransition.to.ply == step.ply &&
      PrincipalVariationEvidence.sameBoardState(structuralTransition.from.fen, step.fenBefore) &&
      PrincipalVariationEvidence.sameBoardState(structuralTransition.to.fen, step.fenAfter) &&
      structuralTransition.perspective == perspective &&
      structuralOccurrence.step == step &&
      structuralConsequences.distinct.size == structuralConsequences.size &&
      structuralConsequences.forall(structuralOccurrence.consequences.contains),
    "passed-pawn-result-causal event must retain its exact replay-owned structural occurrence and line owner"
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

final case class PassedPawnResultDependency(
    from: PassedPawnResultEventNode,
    to: PassedPawnResultEventNode,
    kind: PassedPawnResultDependencyKind,
    proof: PassedPawnResultDependencyProof,
    plyOffset: Int
):
  private[chessjudgment] def stableKey: String =
    PassedPawnResultDependencyOccurrenceIdentity.from(this).stableKey

  private[chessjudgment] def relationOccurrenceBindings: List[ReplayVerticalRelationOccurrenceBinding] =
    proof match
      case PassedPawnResultDependencyProof.ResponseContinuation(trajectory) =>
        List(trajectory.relationOccurrenceBinding)
      case _ => Nil

  def causalConnectionProven: Boolean =
    from.certifiedLegalStep.nonEmpty &&
      to.certifiedLegalStep.nonEmpty &&
      from.step.ply < to.step.ply &&
      plyOffset == to.step.ply - from.step.ply &&
      ((kind, proof) match
        case (PassedPawnResultDependencyKind.ObjectStatePrecondition, PassedPawnResultDependencyProof.ObjectState(trajectory)) =>
          trajectory.rootStep == from.step &&
            trajectory.futureStep == to.step &&
            trajectory.plyOffset == plyOffset
        case (PassedPawnResultDependencyKind.LineAccessPrecondition, PassedPawnResultDependencyProof.LineAccess(trajectory)) =>
          trajectory.enablingStep == from.step &&
            trajectory.enabledStep == to.step &&
            trajectory.plyOffset == plyOffset
        case (
              PassedPawnResultDependencyKind.ResponseContinuationPrecondition,
              PassedPawnResultDependencyProof.ResponseContinuation(trajectory)
            ) =>
          trajectory.triggerStep == from.step &&
            trajectory.followUpStep == to.step &&
            trajectory.plyOffset == plyOffset
        case _ =>
          false
      )
  def enablesContinuation: Boolean =
    causalConnectionProven &&
      (kind == PassedPawnResultDependencyKind.ObjectStatePrecondition ||
        kind == PassedPawnResultDependencyKind.LineAccessPrecondition ||
        kind == PassedPawnResultDependencyKind.ResponseContinuationPrecondition)
  def proofSquares: List[EvidenceSquare] =
    proof match
      case PassedPawnResultDependencyProof.ObjectState(trajectory) =>
        List(trajectory.rootTo)
      case PassedPawnResultDependencyProof.LineAccess(trajectory) =>
        List(trajectory.vacatedSquare)
      case PassedPawnResultDependencyProof.ResponseContinuation(trajectory) =>
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
      case PassedPawnResultDependencyProof.ObjectState(trajectory) =>
        List(trajectory.rootBeforeRole, trajectory.pieceRole, trajectory.futureAfterRole).distinct
      case PassedPawnResultDependencyProof.LineAccess(trajectory) =>
        List(trajectory.enabledPieceRole)
      case PassedPawnResultDependencyProof.ResponseContinuation(trajectory) =>
        trajectory.involvedRoles
private[chessjudgment] enum PassedPawnResultReplyReason:
  case RootActorCaptured
  case CheckAnswered(mode: RelationCheckResponseMode)

private[chessjudgment] final case class PassedPawnResultReplyProof private (
    triggerStep: LineReplayStep,
    responseStep: LineReplayStep,
    reasons: List[PassedPawnResultReplyReason],
    legalResponseStep: LegalReplayStep,
    responseMovement: CanonicalRootLegalMove,
    responseFootprint: BoardTransitionFootprint,
    checkInventoryKey: Option[DerivedRelationResultKey]
):
  require(reasons.nonEmpty && reasons.distinct.size == reasons.size, "a causal response needs exact unique reasons")

  def proves(trigger: PassedPawnResultEventNode, step: LineReplayStep, plyOffset: Int): Boolean =
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

private[chessjudgment] object PassedPawnResultReplyProof:
  def from(
      trigger: PassedPawnResultEventNode,
      response: LineReplayStep,
      triggerTransition: CanonicalReplayTransition,
      responseTransition: CanonicalReplayTransition,
      replay: CanonicalLineReplay
  ): Option[PassedPawnResultReplyProof] =
    for
      triggerLegal <- trigger.certifiedLegalStep
      if triggerTransition.declared == trigger.step && triggerTransition.legal == triggerLegal
      if responseTransition.declared == response
      if replay.transition(trigger.step).contains(triggerTransition)
      if replay.transition(response).contains(responseTransition)
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
      )(PassedPawnResultReplyReason.RootActorCaptured)
      checkWitnesses = replay.exactCheckResponseMembership(trigger.step, response)
      reasons = captureReason.toList ++ checkWitnesses.toList.flatMap(_._2.modes.map(PassedPawnResultReplyReason.CheckAnswered.apply))
      if reasons.nonEmpty
      proof = PassedPawnResultReplyProof(
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

final case class PassedPawnResultReply private[chessjudgment] (
    trigger: PassedPawnResultEventNode,
    step: LineReplayStep,
    plyOffset: Int,
    structuralConsequences: List[TransitionConsequence] = Nil,
    private[chessjudgment] val certificate: Option[PassedPawnResultReplyProof] = None
):
  def capturesRootActor: Boolean =
    certificate.exists(_.reasons.contains(PassedPawnResultReplyReason.RootActorCaptured))
  def answersCheck: Boolean =
    certificate.exists(_.reasons.exists {
      case PassedPawnResultReplyReason.CheckAnswered(_) => true
      case _                                          => false
    })
  def proven: Boolean =
    certificate.exists(_.proves(trigger, step, plyOffset))
  private[chessjudgment] def certifiedLegalStep: Option[LegalReplayStep] =
    certificate.filter(_.proves(trigger, step, plyOffset)).map(_.legalResponseStep)
  private[chessjudgment] def certifiedMovement: Option[CanonicalRootLegalMove] =
    certificate.filter(_.proves(trigger, step, plyOffset)).map(_.responseMovement)

object PassedPawnResultReply:
  private[chessjudgment] def certified(
      trigger: PassedPawnResultEventNode,
      step: LineReplayStep,
      triggerTransition: CanonicalReplayTransition,
      responseTransition: CanonicalReplayTransition,
      replay: CanonicalLineReplay
  ): Option[PassedPawnResultReply] =
    PassedPawnResultReplyProof.from(trigger, step, triggerTransition, responseTransition, replay).map(proof =>
      PassedPawnResultReply(
        trigger = trigger,
        step = step,
        plyOffset = step.ply - trigger.step.ply,
        certificate = Some(proof)
      )
    )

enum PassedPawnResultMechanism:
  case AdvanceOrPromotion
  case Creation
  case ReleasedPassedPawnContinuation

final case class PassedPawnResultFunctionIdentity(
    resultKind: PassedPawnResultKind,
    mechanism: PassedPawnResultMechanism,
    supportingDependency: Option[CausalDependencyFunctionIdentity]
):
  def stableKey: String =
    PassedPawnResultProofKey.product(
      "passed-pawn-result-function",
      List(
        resultKind.id,
        mechanism.toString.toLowerCase,
        PassedPawnResultProofKey.optional(supportingDependency.map(_.stableKey))
      )
    )

/** Typed authority that one exact transition, and optionally one exact edge,
  * establishes a passed-pawn result. The companion is the only producer.
  */
final case class PassedPawnResultTransitionProof private (
    resultKind: PassedPawnResultKind,
    sourceLineOccurrenceOwner: EvidenceRef,
    sourceOccurrenceId: String,
    sourcePremiseKeys: List[String],
    sourceTransition: StructuralTransitionBinding,
    consequence: TransitionConsequence,
    mechanism: PassedPawnResultMechanism,
    supportingDependency: Option[PassedPawnResultDependency]
):
  private[chessjudgment] def binds(
      sourceEvent: PassedPawnResultEventNode,
      exactConsequence: TransitionConsequence,
      causalPath: List[PassedPawnResultDependency]
  ): Boolean =
    consequence == exactConsequence &&
      sourceEvent.lineOccurrenceOwner == sourceLineOccurrenceOwner &&
      sourceEvent.structuralOccurrence.occurrenceId == sourceOccurrenceId &&
      sourceEvent.structuralOccurrence.sourcePremiseKeys == sourcePremiseKeys &&
      sourceEvent.structuralTransition == sourceTransition &&
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
      root: PassedPawnResultEventNode
  ): PassedPawnResultFunctionIdentity =
    PassedPawnResultFunctionIdentity(
      resultKind,
      mechanism,
      supportingDependency.map(CausalDependencyFunctionIdentity.from(root, _))
    )

  private[chessjudgment] def withSupportingDependency(
      dependency: Option[PassedPawnResultDependency]
  ): PassedPawnResultTransitionProof =
    copy(supportingDependency = dependency)

  private[chessjudgment] def stableKey: String =
    PassedPawnResultProofKey.product(
      "passed-pawn-result-transition-proof",
      List(
        resultKind.id,
        sourceLineOccurrenceOwner.id,
        sourceOccurrenceId,
        sourcePremiseKeys.mkString("[", ",", "]"),
        mechanism.toString.toLowerCase,
        sourceTransition.moveUci,
        sourceTransition.from.ply.toString,
        PrincipalVariationEvidence.normalizeFen(sourceTransition.from.fen),
        sourceTransition.to.ply.toString,
        PrincipalVariationEvidence.normalizeFen(sourceTransition.to.fen),
        consequence.stableKey,
        PassedPawnResultProofKey.optional(supportingDependency.map(_.stableKey))
      )
    )

/** One result occurrence remains attached to the exact causal path and typed
  * result mechanism that produced it.  It is never reconstructed as a later
  * path/result Cartesian product.
  */
final case class PassedPawnResultRoute private (
    sourceEvent: PassedPawnResultEventNode,
    consequence: TransitionConsequence,
    causalPath: List[PassedPawnResultDependency],
    resultProof: PassedPawnResultTransitionProof
):
  private[chessjudgment] def stableKey: String =
    PassedPawnResultProofKey.product(
      "passed-pawn-result-route",
      List(
        PassedPawnResultEventOccurrence.from(
          sourceEvent.identity,
          sourceEvent.moveUci,
          sourceEvent.step.ply,
          sourceEvent.step.fenBefore,
          sourceEvent.step.fenAfter
        ).stableKey,
        consequence.stableKey,
        PassedPawnResultProofKey.sequence(causalPath.map(_.stableKey)),
        resultProof.stableKey
      )
    )

  private[chessjudgment] def withMappedEvents(
      resolve: PassedPawnResultEventNode => PassedPawnResultEventNode
  ): PassedPawnResultRoute =
    val remappedPath = causalPath.map(dependency =>
      dependency.copy(from = resolve(dependency.from), to = resolve(dependency.to))
    )
    val remappedSupport = resultProof.supportingDependency.flatMap(original =>
      causalPath.zip(remappedPath).collectFirst { case (before, after) if before == original => after }
    )
    PassedPawnResultRoute(
      sourceEvent = resolve(sourceEvent),
      consequence = consequence,
      causalPath = remappedPath,
      resultProof = resultProof.withSupportingDependency(remappedSupport)
    )

object PassedPawnResultRoute:
  private[chessjudgment] def certified(
      sourceEvent: PassedPawnResultEventNode,
      consequence: TransitionConsequence,
      causalPath: List[PassedPawnResultDependency],
      resultProof: PassedPawnResultTransitionProof
  ): Option[PassedPawnResultRoute] =
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
        resultProof.binds(sourceEvent, consequence, causalPath)
    )(
      PassedPawnResultRoute(sourceEvent, consequence, causalPath, resultProof)
    )

final case class PassedPawnResultEpisode(
    root: PassedPawnResultEventNode,
    continuations: List[PassedPawnResultEventNode],
    dependencies: List[PassedPawnResultDependency],
    responses: List[PassedPawnResultReply],
    resultRoutes: List[PassedPawnResultRoute] = Nil
):
  require(!continuations.contains(root), "the passed-pawn-result root cannot also be a continuation")
  require(
    continuations.distinct.size == continuations.size,
    "duplicate exact passed-pawn-result continuations"
  )
  require(
    dependencies.distinct.size == dependencies.size,
    "duplicate exact passed-pawn-result dependencies"
  )
  require(
    responses.distinct.size == responses.size,
    "duplicate exact passed-pawn-result responses"
  )
  require(resultRoutes.distinct.size == resultRoutes.size, "duplicate exact passed-pawn-result routes")
  require(
    resultRoutes.forall(route =>
      route.causalPath.headOption.exists(_.from == root) &&
        continuations.contains(route.sourceEvent) &&
        route.causalPath.forall(dependencies.contains)
    ),
    "passed-pawn-result routes must belong to this episode and begin at its root"
  )
  def withRootIdentity(identity: PassedPawnResultEventIdentity): PassedPawnResultEpisode =
    if root.identity == identity then this
    else
      val previousRoot = root
      val resolvedRoot = root.copy(identity = identity)
      def resolve(event: PassedPawnResultEventNode): PassedPawnResultEventNode =
        if event == previousRoot then resolvedRoot else event
      copy(
        root = resolvedRoot,
        continuations = continuations.map(resolve),
        dependencies = dependencies.map(dependency =>
          dependency.copy(from = resolve(dependency.from), to = resolve(dependency.to))
        ),
        responses = responses.map(response => response.copy(trigger = resolve(response.trigger))),
        resultRoutes = resultRoutes.map(_.withMappedEvents(resolve))
      )
  lazy val resultSteps: List[PassedPawnResultEventNode] =
    (root :: continuations).sortBy(event => (event.step.ply, event.moveUci))
  def events: List[PassedPawnResultEventNode] = resultSteps
  private lazy val futureDependencies: List[PassedPawnResultDependency] =
    dependencies.filter(dependency => resultSteps.contains(dependency.from) && resultSteps.contains(dependency.to))
  def causalEpisodeProven: Boolean =
    continuations.nonEmpty &&
      futureDependencies.nonEmpty &&
      futureDependencies.forall(_.causalConnectionProven) &&
      responses.forall(response => resultSteps.contains(response.trigger) && response.proven) &&
      futureConnectedToRoot
  lazy val rootEnabledSteps: List[PassedPawnResultEventNode] =
    @annotation.tailrec
    def expand(enabled: Set[PassedPawnResultEventNode]): Set[PassedPawnResultEventNode] =
      val next = enabled ++ dependencies.collect {
        case dependency if enabled(dependency.from) && dependency.enablesContinuation => dependency.to
      }
      if next == enabled then enabled else expand(next)
    resultSteps.filter(expand(Set(root)))
  def rootEnablesContinuation: Boolean = rootEnabledSteps.exists(_ != root)
  def continuationsEnabledByRoot: List[PassedPawnResultEventNode] = rootEnabledSteps.filterNot(_ == root)
  def requiredPlyOffset: Int =
    resultRoutes.map(_.sourceEvent.step.ply - root.step.ply).maxOption.getOrElse(0).max(0)

  /** Every returned path owns its exact dependency occurrences. Parallel
    * chess explanations between the same event nodes therefore remain
    * distinct instead of collapsing into one node-only path.
    */
  def enablingDependencyPathsTo(
      destination: PassedPawnResultEventNode
  ): List[List[PassedPawnResultDependency]] =
    val relevant = enablingDependenciesTo(destination)
    val outgoing = relevant
      .groupMap(_.from)(identity)
      .view
      .mapValues(_.sortBy(_.stableKey))
      .toMap
    val memo = scala.collection.mutable.Map.empty[PassedPawnResultEventNode, List[List[PassedPawnResultDependency]]]
    def suffixes(current: PassedPawnResultEventNode): List[List[PassedPawnResultDependency]] =
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

  def enablingDependenciesTo(destination: PassedPawnResultEventNode): List[PassedPawnResultDependency] =
    @annotation.tailrec
    def reverseReachable(reached: Set[PassedPawnResultEventNode]): Set[PassedPawnResultEventNode] =
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
      ancestor: PassedPawnResultEventNode,
      destination: PassedPawnResultEventNode
  ): Boolean =
    ancestor == destination || enablingDependenciesTo(destination).exists(dependency =>
      dependency.from == ancestor || dependency.to == ancestor
    )

  private def futureConnectedToRoot: Boolean =
    @annotation.tailrec
    def expand(connected: Set[PassedPawnResultEventNode]): Set[PassedPawnResultEventNode] =
      val next = connected ++ futureDependencies.flatMap { dependency =>
        Option.when(dependency.enablesContinuation && connected(dependency.from))(dependency.to)
      }
      if next == connected then connected else expand(next)
    resultSteps.toSet.subsetOf(expand(Set(root)))


object PassedPawnResultEpisode:
  private val MeansOnlyResultKinds = Set.empty[TransitionConsequenceKind]

  def resultConsequences(event: PassedPawnResultEventNode): List[TransitionConsequence] =
    event.structuralConsequences
      .filter(_.subjectFacts.nonEmpty)
      .distinct

  def consequenceSquares(consequence: TransitionConsequence): List[EvidenceSquare] =
    consequence.subjectFacts
      .flatMap(_.semanticSquares)
      .distinct

  def resultTargetSubjects(consequence: TransitionConsequence): List[String] =
    consequence.resultSubjectBindings.map(_.stableKey)

  def consequenceTargetSquares(
      consequence: TransitionConsequence
  ): List[EvidenceSquare] =
    consequence.resultSubjectFacts.flatMap(_.targetSquares).distinct

  private[chessjudgment] def meansOnlyResultKind(kind: TransitionConsequenceKind): Boolean =
    MeansOnlyResultKinds(kind)

  def triggerMoveCapturesPiece(trigger: PassedPawnResultEventNode): Boolean =
    trigger.certifiedLegalStep.exists(_.capturedRole.nonEmpty)

enum PassedPawnResultBranchOutcome:
  case Realized
  case Deferred
  case Diverted
  case Refuted

enum PassedPawnResultMatch:
  case ExactMove
  case EquivalentFunction

final case class PassedPawnResultRealization(
    observedRoot: PassedPawnResultEventNode,
    resultRoute: PassedPawnResultRoute,
    matchKind: PassedPawnResultMatch
):
  def event: PassedPawnResultEventNode = resultRoute.sourceEvent
  def moveUci: String = EvidenceRef.normalizeMove(event.moveUci)
  def plyOffset: Int = event.step.ply - observedRoot.step.ply

enum PassedPawnResultTerminalOutcome:
  case Victory
  case Defeat
  case Draw

enum PassedPawnResultReplyCoverage:
  case NoReplyWitnesses
  case IncompleteReplyCoverage
  case AllRepliesDiverted
  case SomeRepliesRealize
  case AllLegalRepliesRealize

final case class PassedPawnReplyBranchWitness(
    sourceProbeId: String,
    line: LineNodeRef,
    observedEpisode: Option[PassedPawnResultEpisode],
    certifiedHorizonPlyOffset: Int,
    observedThroughPlyOffset: Int,
    terminalOutcome: Option[PassedPawnResultTerminalOutcome],
    terminalPlyOffset: Option[Int],
    terminalStep: Option[LineReplayStep],
    private[chessjudgment] val canonicalReplay: Option[CanonicalLineReplay] = None
)

private object PassedPawnResultProofKey:
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
sealed trait CausalDependencyFunctionProof:
  def kind: String
  def proofSquares: List[EvidenceSquare]
  def proofPieceRoles: List[EvidencePieceRole]
  protected def identityParts: List[String]
  final def stableKey: String =
    PassedPawnResultProofKey.product(kind, identityParts)

object CausalDependencyFunctionProof:
  final case class ObjectState(
      rootBeforeRole: EvidencePieceRole,
      pieceRole: EvidencePieceRole,
      futureAfterRole: EvidencePieceRole,
      color: Color,
      rootFrom: EvidenceSquare,
      rootTo: EvidenceSquare,
      futureFrom: EvidenceSquare,
      futureTo: EvidenceSquare
  ) extends CausalDependencyFunctionProof:
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
  ) extends CausalDependencyFunctionProof:
    require(vacatedSquares.nonEmpty, "a line-access function needs an exact vacated gate")
    val kind = "line-access"
    val proofSquares = vacatedSquares ++ List(enabledFrom, enabledTo)
    val proofPieceRoles = List(enabledPieceRole)
    protected val identityParts = List(
      enabledPieceRole.name.toLowerCase,
      color.toString.toLowerCase,
      PassedPawnResultProofKey.sequence(vacatedSquares.map(_.key.toLowerCase)),
      enabledFrom.key.toLowerCase,
      enabledTo.key.toLowerCase,
      accessRelationKey
    )

  sealed trait ResponseContinuation extends CausalDependencyFunctionProof:
    def replyMoveUci: String
    def replyFrom: EvidenceSquare
    def replyTo: EvidenceSquare
    def followUpFrom: EvidenceSquare
    def followUpTo: EvidenceSquare
    final def proofSquares: List[EvidenceSquare] =
      List(replyFrom, replyTo, followUpFrom, followUpTo) ++ resultSquares
    protected def resultSquares: List[EvidenceSquare]

  final case class PawnBreakFollowUp(
      color: Color,
      replyMoveUci: String,
      replyFrom: EvidenceSquare,
      replyTo: EvidenceSquare,
      followUpFrom: EvidenceSquare,
      followUpTo: EvidenceSquare,
      releasedPassedPawn: EvidenceSquare,
      pawnTopologyTransitionKey: String
  ) extends ResponseContinuation:
    require(
      pawnTopologyTransitionKey.matches(
        s"${RelationFactKind.id(RelationFactKind.PawnTopologyTransition)}:[0-9a-f]{64}"
      ),
      "a released-passer function needs an L1 pawn-topology transition key"
    )
    val kind = "response-continuation:pawn-break-follow-up"
    val proofPieceRoles = List(EvidencePieceRole(Pawn.toString))
    protected val resultSquares = List(releasedPassedPawn)
    protected val identityParts = List(
      color.toString.toLowerCase,
      EvidenceRef.normalizeMove(replyMoveUci),
      replyFrom.key.toLowerCase,
      replyTo.key.toLowerCase,
      followUpFrom.key.toLowerCase,
      followUpTo.key.toLowerCase,
      releasedPassedPawn.key.toLowerCase,
      pawnTopologyTransitionKey
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

  def from(dependency: PassedPawnResultDependency): CausalDependencyFunctionProof =
    dependency.proof match
      case PassedPawnResultDependencyProof.ObjectState(trajectory) =>
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
      case PassedPawnResultDependencyProof.LineAccess(trajectory) =>
        LineAccess(
          trajectory.enabledPieceRole,
          trajectory.color,
          trajectory.vacatedSquares,
          trajectory.enabledFrom,
          trajectory.enabledTo,
          trajectory.accessRelationKey.stableKey
        )
      case PassedPawnResultDependencyProof.ResponseContinuation(trajectory: PawnBreakFollowUpTrajectory) =>
        PawnBreakFollowUp(
          trajectory.color,
          trajectory.replyStep.moveUci,
          trajectory.replyFrom,
          trajectory.replyTo,
          trajectory.followUpFrom,
          trajectory.followUpTo,
          trajectory.releasedPassedPawn,
          trajectory.pawnTopologyTransitionKey.stableKey
        )
      case PassedPawnResultDependencyProof.ResponseContinuation(trajectory: CaptureResponseFollowUpTrajectory) =>
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

final case class CausalDependencyFunctionIdentity(
    fromMoveUci: String,
    fromPlyOffset: Int,
    toMoveUci: String,
    toPlyOffset: Int,
    dependencyKind: PassedPawnResultDependencyKind,
    proof: CausalDependencyFunctionProof,
    plyOffset: Int
):
  def proofKind: String = proof.kind
  def proofSquares: List[String] = proof.proofSquares.map(_.key.toLowerCase).distinct.sorted
  def proofPieceRoles: List[String] = proof.proofPieceRoles.map(_.name.toLowerCase).distinct.sorted
  def stableKey: String =
    PassedPawnResultProofKey.product(
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

object CausalDependencyFunctionIdentity:
  def from(
      root: PassedPawnResultEventNode,
      dependency: PassedPawnResultDependency
  ): CausalDependencyFunctionIdentity =
    CausalDependencyFunctionIdentity(
      fromMoveUci = EvidenceRef.normalizeMove(dependency.from.moveUci),
      fromPlyOffset = dependency.from.step.ply - root.step.ply,
      toMoveUci = EvidenceRef.normalizeMove(dependency.to.moveUci),
      toPlyOffset = dependency.to.step.ply - root.step.ply,
      dependencyKind = dependency.kind,
      proof = CausalDependencyFunctionProof.from(dependency),
      plyOffset = dependency.plyOffset
    )

final case class PassedPawnResultLineStepOccurrence private (
    moveUci: String,
    before: PassedPawnResultPositionOccurrence,
    after: PassedPawnResultPositionOccurrence
):
  def stableKey: String =
    PassedPawnResultProofKey.product(
      "causal-line-step-occurrence",
      List(moveUci, before.stableKey, after.stableKey)
    )

object PassedPawnResultLineStepOccurrence:
  def from(step: LineReplayStep): PassedPawnResultLineStepOccurrence =
    require(step.ply > 0, "causal line step occurrence requires a positive result ply")
    PassedPawnResultLineStepOccurrence(
      moveUci = EvidenceRef.normalizeMove(step.moveUci),
      before = PassedPawnResultPositionOccurrence.from(step.fenBefore, step.ply - 1),
      after = PassedPawnResultPositionOccurrence.from(step.fenAfter, step.ply)
    )

/** Exact occurrence of one causal edge. The semantic function remains typed,
  * while FEN/ply identity for both endpoints and every intervening replay step
  * prevents parallel routes from collapsing into a node-only sequence.
  */
final case class PassedPawnResultDependencyOccurrenceIdentity private (
    from: PassedPawnResultEventOccurrence,
    to: PassedPawnResultEventOccurrence,
    dependencyKind: PassedPawnResultDependencyKind,
    proof: CausalDependencyFunctionProof,
    interveningSteps: List[PassedPawnResultLineStepOccurrence],
    relationOccurrences: List[ReplayVerticalRelationOccurrenceBinding],
    plyOffset: Int
):
  def stableKey: String =
    PassedPawnResultProofKey.product(
      "causal-dependency-occurrence",
      List(
        from.stableKey,
        to.stableKey,
        dependencyKind.toString.toLowerCase,
        proof.stableKey,
        PassedPawnResultProofKey.sequence(interveningSteps.map(_.stableKey)),
        PassedPawnResultProofKey.sequence(relationOccurrences.map(_.stableKey)),
        plyOffset.toString
      )
    )

object PassedPawnResultDependencyOccurrenceIdentity:
  def from(dependency: PassedPawnResultDependency): PassedPawnResultDependencyOccurrenceIdentity =
    require(dependency.causalConnectionProven, "causal dependency occurrence requires an exact proven connection")
    val intervening = dependency.proof match
      case PassedPawnResultDependencyProof.ObjectState(_) => Nil
      case PassedPawnResultDependencyProof.LineAccess(trajectory) => trajectory.interveningSteps
      case PassedPawnResultDependencyProof.ResponseContinuation(trajectory) => trajectory.interveningSteps
    PassedPawnResultDependencyOccurrenceIdentity(
      from = eventOccurrence(dependency.from),
      to = eventOccurrence(dependency.to),
      dependencyKind = dependency.kind,
      proof = CausalDependencyFunctionProof.from(dependency),
      interveningSteps = intervening.map(PassedPawnResultLineStepOccurrence.from),
      relationOccurrences = dependency.relationOccurrenceBindings.sortBy(_.stableKey),
      plyOffset = dependency.plyOffset
    )

  private def eventOccurrence(event: PassedPawnResultEventNode): PassedPawnResultEventOccurrence =
    PassedPawnResultEventOccurrence.from(
      event = event.identity,
      moveUci = event.moveUci,
      ply = event.step.ply,
      fenBefore = event.step.fenBefore,
      fenAfter = event.step.fenAfter
    )

object PassedPawnResultFunctionalMatch:
  def functionallyEquivalent(
      expected: List[TransitionConsequence],
      observed: List[TransitionConsequence]
  ): Boolean =
    val expectedPositive = expected.filter(_.establishesState)
    val observedPositive = observed.filter(_.establishesState)
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
      expectedRoot: PassedPawnResultEventNode,
      expectedRoute: PassedPawnResultRoute,
      observedRoot: PassedPawnResultEventNode,
      observedRoute: PassedPawnResultRoute
  ): Boolean =
    expectedRoute.causalPath.forall(_.causalConnectionProven) &&
      observedRoute.causalPath.forall(_.causalConnectionProven) &&
      functionPath(expectedRoot, expectedRoute.causalPath) ==
        functionPath(observedRoot, observedRoute.causalPath) &&
      expectedRoute.resultProof.functionIdentity(expectedRoot) ==
        observedRoute.resultProof.functionIdentity(observedRoot) &&
      functionallyEquivalent(List(expectedRoute.consequence), List(observedRoute.consequence))

  private def targetObjectsCompatible(
      expected: TransitionConsequence,
      observed: TransitionConsequence
  ): Boolean =
    val left = EvidenceObjectBinding.resultTargetObjectGroups(expected).toSet
    val right = EvidenceObjectBinding.resultTargetObjectGroups(observed).toSet
    left.nonEmpty && left == right

  private def consequenceCompatible(
      expected: TransitionConsequence,
      observed: TransitionConsequence
  ): Boolean =
    expected.kind == observed.kind &&
      targetObjectsCompatible(expected, observed)

  private def functionPath(
      root: PassedPawnResultEventNode,
      dependencies: List[PassedPawnResultDependency]
  ): List[CausalDependencyFunctionIdentity] =
    dependencies.map(CausalDependencyFunctionIdentity.from(root, _))

final case class PassedPawnResultBranchObservation(
    line: LineNodeRef,
    replyMove: String,
    outcome: PassedPawnResultBranchOutcome,
    realizations: List[PassedPawnResultRealization],
    observedThroughPlyOffset: Int,
    terminalOutcome: Option[PassedPawnResultTerminalOutcome],
    terminalPlyOffset: Option[Int],
    terminalStep: Option[LineReplayStep],
    private[chessjudgment] val canonicalReplay: Option[CanonicalLineReplay] = None
):
  def realizationMoves: List[String] = realizations.map(_.moveUci).distinct
  def realizationPlyOffsets: List[Int] =
    realizations.map(_.plyOffset).distinct.sorted

final case class PassedPawnResultReplyAssessment(
    resultRoute: PassedPawnResultRoute,
    sourcePlyOffset: Int,
    observations: List[PassedPawnResultBranchObservation],
    robustness: PassedPawnResultReplyCoverage
):
  def sourceEvent: PassedPawnResultEventNode = resultRoute.sourceEvent
  def consequence: TransitionConsequence = resultRoute.consequence
  def causalPath: List[PassedPawnResultDependency] = resultRoute.causalPath
  def resultProof: PassedPawnResultTransitionProof = resultRoute.resultProof
  def positiveProofReady: Boolean =
    robustness == PassedPawnResultReplyCoverage.AllLegalRepliesRealize || robustness == PassedPawnResultReplyCoverage.SomeRepliesRealize
  def realizedObservations: List[PassedPawnResultBranchObservation] =
    observations.filter(_.outcome == PassedPawnResultBranchOutcome.Realized)

final case class PassedPawnResultSourceOccurrence private[chessjudgment] (
    moveUci: String,
    plyOffset: Int,
    actor: PassedPawnResultActorOccurrence
):
  def stableKey: String =
    List(EvidenceRef.normalizeMove(moveUci), plyOffset.toString, actor.stableKey).mkString("@")

final case class PassedPawnResultBranchRealizationIdentity(
    moveUci: String,
    matchKind: PassedPawnResultMatch,
    plyOffset: Int,
    causalRoute: List[CausalDependencyFunctionIdentity],
    resultFunction: PassedPawnResultFunctionIdentity
):
  def stableKey: String =
    PassedPawnResultProofKey.product(
      "passed-pawn-result-branch-realization",
      List(
        moveUci,
        matchKind.toString.toLowerCase,
        plyOffset.toString,
        PassedPawnResultProofKey.sequence(causalRoute.map(_.stableKey)),
        resultFunction.stableKey
      )
    )

final case class PassedPawnResultBranchIdentity(
    replyMoveUci: String,
    outcome: PassedPawnResultBranchOutcome,
    observedThroughPlyOffset: Int,
    realizations: List[PassedPawnResultBranchRealizationIdentity],
    terminalOutcome: Option[PassedPawnResultTerminalOutcome],
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

/** passed-pawn result taxonomy is annotation. This is the exact result occurrence and
  * causal route that may participate in player-facing semantic equality.
  */
final case class PassedPawnResultSemanticIdentity(
    root: PassedPawnResultSourceOccurrence,
    source: PassedPawnResultSourceOccurrence,
    consequenceKind: TransitionConsequenceKind,
    resultTargetSubjects: List[String],
    robustness: PassedPawnResultReplyCoverage,
    branches: List[PassedPawnResultBranchIdentity],
    causalRoute: List[CausalDependencyFunctionIdentity],
    resultFunction: PassedPawnResultFunctionIdentity
):
  def stableKey: String =
    (List(root.stableKey, source.stableKey) ++ List(
      consequenceKind.toString.toLowerCase,
      resultTargetSubjects.mkString("[", ",", "]"),
      robustness.toString.toLowerCase,
      branches.map(_.stableKey).mkString("[", ",", "]"),
      causalRoute.map(_.stableKey).mkString("[", ",", "]"),
      resultFunction.stableKey
    )).mkString("|")

object PassedPawnResultSemanticIdentity:
  def from(
      event: PassedPawnResultEventEvidence,
      assessment: PassedPawnResultReplyAssessment
  ): PassedPawnResultSemanticIdentity =
    PassedPawnResultSemanticIdentity(
      root = PassedPawnResultSourceOccurrence(
        EvidenceRef.normalizeMove(event.causalEpisode.root.moveUci),
        0,
        event.causalEpisode.root.identity.actor
      ),
      source = PassedPawnResultSourceOccurrence(
        EvidenceRef.normalizeMove(assessment.sourceEvent.moveUci),
        assessment.sourcePlyOffset,
        assessment.sourceEvent.identity.actor
      ),
      consequenceKind = assessment.consequence.kind,
      resultTargetSubjects = normalizedResultTargetSubjects(
        assessment.consequence.resultSubjectBindings.map(_.stableKey)
      ),
      robustness = assessment.robustness,
      branches = assessmentBranches(assessment.observations),
      causalRoute = assessment.causalPath.map(
        CausalDependencyFunctionIdentity.from(event.causalEpisode.root, _)
      ),
      resultFunction = assessment.resultProof.functionIdentity(event.causalEpisode.root)
    )

  private def normalizedResultTargetSubjects(subjects: List[String]): List[String] =
    subjects.map(normalize).filter(_.nonEmpty).distinct.sorted

  private def assessmentBranches(
      observations: List[PassedPawnResultBranchObservation]
  ): List[PassedPawnResultBranchIdentity] =
    observations.map { observation =>
      PassedPawnResultBranchIdentity(
        replyMoveUci = EvidenceRef.normalizeMove(observation.replyMove),
        outcome = observation.outcome,
        observedThroughPlyOffset = observation.observedThroughPlyOffset,
        realizations = observation.realizations.map(realization =>
          PassedPawnResultBranchRealizationIdentity(
            moveUci = realization.moveUci,
            matchKind = realization.matchKind,
            plyOffset = realization.plyOffset,
            causalRoute = realization.resultRoute.causalPath.map(
              CausalDependencyFunctionIdentity.from(realization.observedRoot, _)
            ),
            resultFunction = realization.resultRoute.resultProof.functionIdentity(realization.observedRoot)
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

object PassedPawnResultReplyAssessment:
  def fromRoute(
      episode: PassedPawnResultEpisode,
      resultRoute: PassedPawnResultRoute,
      witnesses: List[PassedPawnReplyBranchWitness],
      branchSetComplete: Boolean
  ): PassedPawnResultReplyAssessment =
    require(
      episode.resultRoutes.contains(resultRoute),
      "a passed-pawn-result assessment requires a route owned by its episode"
    )
    val sourcePlyOffset = resultRoute.sourceEvent.step.ply - episode.root.step.ply
    val observations = witnesses.map(witness =>
      observation(episode, resultRoute, sourcePlyOffset, witness)
    )
    val robustness =
      if observations.isEmpty then PassedPawnResultReplyCoverage.NoReplyWitnesses
      else if !branchSetComplete || observations.exists(_.outcome == PassedPawnResultBranchOutcome.Deferred) then
        PassedPawnResultReplyCoverage.IncompleteReplyCoverage
      else if observations.forall(_.outcome == PassedPawnResultBranchOutcome.Realized) then PassedPawnResultReplyCoverage.AllLegalRepliesRealize
      else if observations.exists(_.outcome == PassedPawnResultBranchOutcome.Realized) then PassedPawnResultReplyCoverage.SomeRepliesRealize
      else if observations.forall(observation =>
        observation.outcome == PassedPawnResultBranchOutcome.Diverted &&
          observation.terminalOutcome.exists(_ != PassedPawnResultTerminalOutcome.Defeat)
      ) then PassedPawnResultReplyCoverage.AllRepliesDiverted
      else PassedPawnResultReplyCoverage.IncompleteReplyCoverage
    PassedPawnResultReplyAssessment(resultRoute, sourcePlyOffset, observations, robustness)

  private def observation(
      episode: PassedPawnResultEpisode,
      resultRoute: PassedPawnResultRoute,
      sourcePlyOffset: Int,
      witness: PassedPawnReplyBranchWitness
  ): PassedPawnResultBranchObservation =
    val realizations = witness.observedEpisode.toList.flatMap { observedEpisode =>
      observedEpisode.resultRoutes.flatMap { candidateRoute =>
        val candidate = candidateRoute.sourceEvent
        val offset = candidate.step.ply - observedEpisode.root.step.ply
        Option
          .when(
            offset <= witness.observedThroughPlyOffset &&
              PassedPawnResultFunctionalMatch.causallyEquivalent(
                episode.root,
                resultRoute,
                observedEpisode.root,
                candidateRoute
              )
          )(
            PassedPawnResultRealization(
              observedEpisode.root,
              candidateRoute,
              if EvidenceRef.sameMove(resultRoute.sourceEvent.moveUci, candidate.moveUci) &&
                  offset == sourcePlyOffset
              then PassedPawnResultMatch.ExactMove
              else PassedPawnResultMatch.EquivalentFunction
            )
          )
          .toList
      }
    }.distinct.sortBy(realization =>
      (
        if realization.matchKind == PassedPawnResultMatch.ExactMove then 0 else 1,
        realization.event.step.ply,
        realization.moveUci,
        exactEventOrderKey(realization.event)
      )
    )
    val terminalBeforeDeadline =
      witness.terminalOutcome.filter(_ => witness.terminalPlyOffset.exists(_ <= witness.observedThroughPlyOffset))
    val outcome =
      if realizations.nonEmpty then PassedPawnResultBranchOutcome.Realized
      else
        terminalBeforeDeadline match
          case Some(PassedPawnResultTerminalOutcome.Defeat) => PassedPawnResultBranchOutcome.Refuted
          case Some(_)                                => PassedPawnResultBranchOutcome.Diverted
          case None if witness.observedThroughPlyOffset < sourcePlyOffset => PassedPawnResultBranchOutcome.Deferred
          case None if witness.observedEpisode.exists(observedEpisode =>
              observedEpisode.continuationsEnabledByRoot.exists(event =>
                event.step.ply - observedEpisode.root.step.ply <= witness.observedThroughPlyOffset
              )
            ) =>
            PassedPawnResultBranchOutcome.Diverted
          case None => PassedPawnResultBranchOutcome.Deferred
    PassedPawnResultBranchObservation(
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

  private def exactEventOrderKey(event: PassedPawnResultEventNode): String =
    val consequenceKeys = event.structuralConsequences.map { consequence =>
      List(
        consequence.kind.toString,
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

final case class PassedPawnResultEventEvidence(
    rootTransition: StructuralTransitionBinding,
    causalEpisode: PassedPawnResultEpisode,
    directResultProofs: List[PassedPawnResultTransitionProof],
    branchWitnesses: List[PassedPawnReplyBranchWitness],
    continuationSourceLine: Option[LineNodeRef] = None,
    private[chessjudgment] val canonicalRootTransitionProof: Option[CanonicalTransitionProof] = None
) extends EvidencePayload:
  require(rootTransition.line.nonEmpty, "passed-pawn-result-causal root transition must reference its canonical line")
  require(
    EvidenceRef.sameMove(causalEpisode.root.moveUci, rootTransition.moveUci) &&
      causalEpisode.root.step.ply == rootTransition.to.ply &&
      PrincipalVariationEvidence.sameBoardState(causalEpisode.root.step.fenBefore, rootTransition.from.fen) &&
      PrincipalVariationEvidence.sameBoardState(causalEpisode.root.step.fenAfter, rootTransition.to.fen) &&
      causalEpisode.root.perspective == rootTransition.perspective,
    "passed-pawn-result-causal episode root must match its authoritative root transition"
  )
  require(
    directResultProofs.distinct.size == directResultProofs.size &&
      directResultProofs.forall(proof =>
        proof.resultKind == causalEpisode.root.identity.kind &&
          proof.sourceLineOccurrenceOwner == causalEpisode.root.lineOccurrenceOwner &&
          proof.sourceOccurrenceId == causalEpisode.root.structuralOccurrence.occurrenceId &&
          proof.sourcePremiseKeys == causalEpisode.root.structuralOccurrence.sourcePremiseKeys &&
          proof.sourceTransition == rootTransition &&
          causalEpisode.root.structuralConsequences.contains(proof.consequence)
      ),
    "direct passed-pawn results must retain their exact structural proof instead of being reconstructed from a label"
  )

  def episode: Option[PassedPawnResultEpisode] =
    Option.when(causalEpisode.causalEpisodeProven)(causalEpisode)
  def identity: PassedPawnResultEventIdentity = causalEpisode.root.identity
  def passedPawnResultKind: PassedPawnResultKind = identity.kind
  def rootLine: LineNodeRef = rootTransition.line.get
  def structuralConsequences: List[TransitionConsequence] = causalEpisode.root.structuralConsequences
  def rootMove: String = rootTransition.moveUci
  def perspective: Color = rootTransition.perspective
  private[chessjudgment] def rootTransitionIsCertified: Boolean =
    canonicalRootTransitionProof.exists(_.proves(rootTransition))
  def provesDirectResult(consequence: TransitionConsequence): Boolean =
    directResultProofs.exists(_.consequence == consequence)
  def directResultConsequences: List[TransitionConsequence] =
    directResultProofs.map(_.consequence).distinct
  def observedResultRoutes: List[PassedPawnResultRoute] =
    episode.toList.flatMap(_.resultRoutes).distinct.sortBy(_.stableKey)
  def rootEnablingDependencies: List[PassedPawnResultDependency] =
    episode.toList.flatMap(causalEpisode =>
      causalEpisode.dependencies.filter(dependency => dependency.from == causalEpisode.root && dependency.enablesContinuation)
    )
  def observedRootEnablesContinuation: Boolean = episode.exists(_.rootEnablesContinuation)
  def requiredHorizonPlyOffset: Int =
    episode
      .toList
      .flatMap(causalEpisode =>
        observedResultRoutes.map { route =>
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
  lazy val causalResultAssessments: List[PassedPawnResultReplyAssessment] =
    episode.toList.flatMap { causalEpisode =>
      causalEpisode.resultRoutes.map { resultRoute =>
        PassedPawnResultReplyAssessment.fromRoute(
          causalEpisode,
          resultRoute,
          branchWitnesses,
          branchSetComplete
        )
      }
    }.distinct
  def positiveCausalResultAssessments: List[PassedPawnResultReplyAssessment] =
    causalResultAssessments.filter(_.positiveProofReady)
  def realizedResultAssessments: List[PassedPawnResultReplyAssessment] =
    positiveCausalResultAssessments
  def resolvedCausalResultAssessments: List[PassedPawnResultReplyAssessment] =
    causalResultAssessments.filterNot(assessment =>
      assessment.robustness == PassedPawnResultReplyCoverage.NoReplyWitnesses ||
        assessment.robustness == PassedPawnResultReplyCoverage.IncompleteReplyCoverage
    )
  def resolvedResultAssessments: List[PassedPawnResultReplyAssessment] =
    resolvedCausalResultAssessments
  /** Every exact result authorized for an affirmative public passed-pawn-result Cause.
    * Sibling results retain independent robustness and proof identity.
    */
  def exactRobustPublicResultAssessments: List[PassedPawnResultReplyAssessment] =
    realizedResultAssessments
      .filter(_.robustness == PassedPawnResultReplyCoverage.AllLegalRepliesRealize)
      .sortBy(publicResultAssessmentSortKey)
  private def publicResultAssessmentSortKey(
      assessment: PassedPawnResultReplyAssessment
  ): (Int, String, String, String) =
    (
      assessment.sourcePlyOffset,
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
        assessment.robustness != PassedPawnResultReplyCoverage.NoReplyWitnesses &&
          assessment.robustness != PassedPawnResultReplyCoverage.IncompleteReplyCoverage
      )
  def episodePublicProofReady: Boolean =
    observedRootEnablesContinuation &&
      branchCoverageComplete &&
      realizedResultAssessments.nonEmpty
  def semanticGroupingAnchors: List[EvidenceSemanticAnchor] =
    List(
      EvidenceSemanticAnchor.of(EvidenceSemanticAnchorKind.PassedPawnResultKind, passedPawnResultKind.id),
      EvidenceSemanticAnchor.of(
        EvidenceSemanticAnchorKind.PassedPawnResultEvent,
        identity.resultKindKey,
        s"root:$rootMove",
        s"actor:${identity.actor.beforeRole}",
        s"proofs:${directResultProofs.map(_.stableKey).sorted.mkString(",")}"
      )
    )

  private[chessjudgment] def lineOccurrenceOwners: List[EvidenceRef] =
    (
      causalEpisode.resultSteps ++
        branchWitnesses.flatMap(_.observedEpisode.toList.flatMap(_.resultSteps))
    ).map(_.lineOccurrenceOwner).distinctBy(_.id).sortBy(_.id)

  private[chessjudgment] def retainsLineOccurrenceOwners(record: EvidenceRecord): Boolean =
    record.payload == this && lineOccurrenceOwners.nonEmpty &&
      lineOccurrenceOwners.forall(owner => record.parents.contains(owner))

object PassedPawnResultTransitionProof:
  private final case class ExactPassedPawnResultActor(
      side: Color,
      beforeRole: String,
      afterRole: String,
      from: EvidenceSquare,
      to: EvidenceSquare
  )

  private object ExactPassedPawnResultActor:
    def from(movement: CanonicalRootLegalMove): ExactPassedPawnResultActor =
      ExactPassedPawnResultActor(
        movement.side,
        movement.beforeRole.name,
        movement.afterRole.name,
        movement.from,
        movement.to
      )

    def from(actor: PassedPawnResultActorOccurrence): ExactPassedPawnResultActor =
      ExactPassedPawnResultActor(
        actor.side,
        actor.beforeRole,
        actor.afterRole,
        EvidenceSquare(actor.from),
        EvidenceSquare(actor.to)
      )

  /** Closed projection from an exact structural result to the presentation
    * results it can actually prove. This replaces enum-wide idea guessing: a
    * new label cannot become a candidate until a concrete consequence family
    * and mechanism admit it here.
    */
  private[chessjudgment] def directCandidates(
      sourceLineOccurrenceOwner: EvidenceRef,
      sourceOccurrence: ReplayStructuralOccurrence,
      transition: StructuralTransitionBinding,
      consequence: TransitionConsequence,
      movement: CanonicalRootLegalMove
  ): List[PassedPawnResultTransitionProof] =
    import TransitionConsequenceKind.*
    val eligibleKinds = consequence.kind match
      case PassedPawnProgress =>
        List(PassedPawnResultKind.AdvanceOrPromote, PassedPawnResultKind.Creation)
      case _ => Nil
    eligibleKinds.flatMap(kind =>
      certify(
        kind,
        sourceLineOccurrenceOwner,
        sourceOccurrence,
        transition,
        consequence,
        movement
      )
    )

  def certify(
      identity: PassedPawnResultEventIdentity,
      sourceLineOccurrenceOwner: EvidenceRef,
      sourceOccurrence: ReplayStructuralOccurrence,
      transition: StructuralTransitionBinding,
      consequence: TransitionConsequence
  ): Option[PassedPawnResultTransitionProof] =
    certifyExact(
      identity.kind,
      sourceLineOccurrenceOwner,
      sourceOccurrence,
      transition,
      consequence,
      ExactPassedPawnResultActor.from(identity.actor)
    )

  def certify(
      resultKind: PassedPawnResultKind,
      sourceLineOccurrenceOwner: EvidenceRef,
      sourceOccurrence: ReplayStructuralOccurrence,
      transition: StructuralTransitionBinding,
      consequence: TransitionConsequence,
      movement: CanonicalRootLegalMove
  ): Option[PassedPawnResultTransitionProof] =
    certifyExact(
      resultKind,
      sourceLineOccurrenceOwner,
      sourceOccurrence,
      transition,
      consequence,
      ExactPassedPawnResultActor.from(movement)
    )

  private def certifyExact(
      resultKind: PassedPawnResultKind,
      sourceLineOccurrenceOwner: EvidenceRef,
      sourceOccurrence: ReplayStructuralOccurrence,
      transition: StructuralTransitionBinding,
      consequence: TransitionConsequence,
      actor: ExactPassedPawnResultActor
  ): Option[PassedPawnResultTransitionProof] =
    Option
      .when(
        sourceOccurrence.step.ply == transition.to.ply &&
          EvidenceRef.sameMove(sourceOccurrence.step.moveUci, transition.moveUci) &&
          sourceOccurrence.consequences.contains(consequence)
      )(
        ()
      )
      .flatMap(_ => directMechanism(resultKind, transition, consequence, actor))
      .map(mechanism =>
        PassedPawnResultTransitionProof(
          resultKind,
          sourceLineOccurrenceOwner,
          sourceOccurrence.occurrenceId,
          sourceOccurrence.sourcePremiseKeys,
          transition,
          consequence,
          mechanism,
          None
        )
      )

  private[chessjudgment] def certifyDependency(
      resultKind: PassedPawnResultKind,
      sourceLineOccurrenceOwner: EvidenceRef,
      sourceOccurrence: ReplayStructuralOccurrence,
      sourceTransition: StructuralTransitionBinding,
      dependency: PassedPawnResultDependency,
      consequence: TransitionConsequence
  ): Option[PassedPawnResultTransitionProof] =
    Option.when(
      sourceOccurrence.step.ply == sourceTransition.to.ply &&
        EvidenceRef.sameMove(sourceOccurrence.step.moveUci, sourceTransition.moveUci) &&
        sourceOccurrence.consequences.contains(consequence)
    )(()).flatMap(_ => dependencyMechanism(dependency, consequence)).map(mechanism =>
      PassedPawnResultTransitionProof(
        resultKind,
        sourceLineOccurrenceOwner,
        sourceOccurrence.occurrenceId,
        sourceOccurrence.sourcePremiseKeys,
        sourceTransition,
        consequence,
        mechanism,
        Some(dependency)
      )
    )

  def proves(
      identity: PassedPawnResultEventIdentity,
      sourceLineOccurrenceOwner: EvidenceRef,
      sourceOccurrence: ReplayStructuralOccurrence,
      transition: StructuralTransitionBinding,
      consequence: TransitionConsequence
  ): Boolean =
    certify(identity, sourceLineOccurrenceOwner, sourceOccurrence, transition, consequence).nonEmpty

  def proves(
      resultKind: PassedPawnResultKind,
      sourceLineOccurrenceOwner: EvidenceRef,
      sourceOccurrence: ReplayStructuralOccurrence,
      transition: StructuralTransitionBinding,
      consequence: TransitionConsequence,
      movement: CanonicalRootLegalMove
  ): Boolean =
    certify(
      resultKind,
      sourceLineOccurrenceOwner,
      sourceOccurrence,
      transition,
      consequence,
      movement
    ).nonEmpty

  private def directMechanism(
      kind: PassedPawnResultKind,
      transition: StructuralTransitionBinding,
      consequence: TransitionConsequence,
      actor: ExactPassedPawnResultActor
  ): Option[PassedPawnResultMechanism] =
    import TransitionConsequenceKind.*
    kind match
      case PassedPawnResultKind.AdvanceOrPromote =>
        Option.when(
          consequence.kind == PassedPawnProgress &&
          transitionActorIs(transition, Pawn, actor) &&
          consequence.resultSubjectFacts.exists(
            passedPawnConversionBy(_, actor)
          )
        )(PassedPawnResultMechanism.AdvanceOrPromotion)
      case PassedPawnResultKind.Creation =>
        Option.when(
          consequence.kind == PassedPawnProgress &&
          consequence.resultSubjectFacts.exists(
            passedPawnManufacture(_, actor)
          )
        )(PassedPawnResultMechanism.Creation)

  private def dependencyMechanism(
      dependency: PassedPawnResultDependency,
      consequence: TransitionConsequence
  ): Option[PassedPawnResultMechanism] =
    import TransitionConsequenceKind.*
    dependency.proof match
      case PassedPawnResultDependencyProof.ResponseContinuation(pawn: PawnBreakFollowUpTrajectory) =>
        val resultSquares = PassedPawnResultEpisode.consequenceSquares(consequence).map(_.key.toLowerCase).toSet
        Option.when(
          consequence.kind == PassedPawnProgress &&
            (resultSquares(pawn.releasedPassedPawn.key.toLowerCase) ||
              resultSquares(pawn.followUpFrom.key.toLowerCase) ||
              resultSquares(pawn.followUpTo.key.toLowerCase))
        )(PassedPawnResultMechanism.ReleasedPassedPawnContinuation)
      case _ => None

  private def transitionActorIs(
      transition: StructuralTransitionBinding,
      role: Role,
      actor: ExactPassedPawnResultActor
  ): Boolean =
    actor.side == transition.perspective &&
      actor.beforeRole.equalsIgnoreCase(role.name) &&
      transition.actorRole.exists(_.name.equalsIgnoreCase(actor.beforeRole))

  private def passedPawnConversionBy(subject: StructuralSubject, actor: ExactPassedPawnResultActor): Boolean =
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

  private def passedPawnManufacture(subject: StructuralSubject, actor: ExactPassedPawnResultActor): Boolean =
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
      case payload: PassedPawnResultEventEvidence =>
        payload.rootLine :: payload.branchWitnesses.map(_.line)
      case payload: PassedPawnResultProofEvidence =>
        List(payload.rootLine)
      case payload: ForcedReplyResourceDifferentialEvidence =>
        List(payload.occurrence.referenceLine, payload.occurrence.playedLine)
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

  private lazy val structuralDeltasByLine: Map[LineNodeRef, List[StructuralDeltaEvidence]] =
    records
      .collect {
        case EvidenceRecord(ref, payload: StructuralDeltaEvidence, _) =>
          payload.line.filter(ref.line.contains).map(_ -> payload)
      }
      .flatten
      .groupMap(_._1)(_._2)

  private[chessjudgment] def uniqueProofEligibleLineFactRecordFor(
      line: LineNodeRef
  ): Option[(EvidenceRecord, LineFactEvidence)] =
    recordsFor(line).collect {
      case record @ EvidenceRecord(_, payload: LineFactEvidence, _)
          if payload.line == line && proofEligible(record) =>
        record -> payload
    } match
      case exact :: Nil => Some(exact)
      case _            => None

  private[chessjudgment] def uniqueReplayCertifiedLineFactFor(
      line: LineNodeRef
  ): Option[LineFactEvidence] =
    lineFactsByLine.getOrElse(line, Nil) match
      case exact :: Nil if exact.replayIsCertified => Some(exact)
      case _                                        => None

  private[chessjudgment] def certifiedRootActorFor(
      line: LineNodeRef
  ): Option[RootCausalActor] =
    uniqueReplayCertifiedLineFactFor(line).flatMap(RootCausalActor.fromLineFact(_, line.rootMove))

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
    val ownedLines =
      sourceSide match
        case RelativeCauseSourceSide.Reference => List(fact.referenceLine)
        case RelativeCauseSourceSide.Candidate => List(fact.candidateLine)
        case RelativeCauseSourceSide.Shared | RelativeCauseSourceSide.Mixed =>
          List(fact.referenceLine, fact.candidateLine)
    val indexedLineRecords = ownedLines.flatMap(line =>
      recordsFor(line).filter(_.ref.line.contains(line))
    )
    TypedEvidenceGraph.ownedLineConsequences(
      indexedLineRecords,
      fact,
      sourceSide,
      attributionKind,
      sourceLabelsByEvidenceId
    )

  def relativeCauseProofRecords(section: RelativeCauseProofSection): List[EvidenceRecord] =
    section.sourceRefs.flatMap(ref => record(ref))

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
    }

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
    }

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
    yield ref -> consequence)

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

  def relativeCausePassedPawnResults(
      kind: RelativeCauseKind,
      section: RelativeCauseProofSection
  ): List[(EvidenceRef, PassedPawnResultProofEvidence)] =
    relativeCauseProofRecords(section).collect {
      case record @ EvidenceRecord(ref, result: PassedPawnResultProofEvidence, _)
          if proofEligible(record) && RelativeCauseKind.passedPawnResultProofCanProveCause(kind, result) =>
        ref -> result
    }.distinct

  def relativeCauseProofSectionHasConcreteProof(
      kind: RelativeCauseKind,
      section: RelativeCauseProofSection
  ): Boolean =
    val selectedMoveOrderCausalProof =
      section.role == RelativeCauseProofRole.DirectProof &&
        kind == RelativeCauseKind.WrongMoveOrder &&
        relativeCauseProofRecords(section).exists {
          case record @ EvidenceRecord(_, result: ForcedReplyResourceDifferentialEvidence, _) =>
            result.hasCompleteProofPaths && proofEligible(record)
          case _ =>
            false
        }
    relativeCauseLineConsequences(kind, section).nonEmpty ||
      relativeCauseRelations(section).nonEmpty ||
      relativeCauseTacticalMechanisms(section).nonEmpty ||
      relativeCausePassedPawnResults(kind, section).nonEmpty ||
      selectedMoveOrderCausalProof

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

  def relativeCauseHasOwnedPassedPawnResultProof(cause: RelativeCauseFact): Boolean =
    cause.attribution.directProofEligible &&
      cause.passedPawnResultCauseKind &&
      cause.proof.exists(proof =>
        EvidenceObjectBinding
          .rawDirectSentenceChannelsForProjection(cause, this)
          .exists(relativeCauseChannelHasOwnedPassedPawnResultAuthority(cause, _))
      )

  /** A passed-pawn-result Cause may expose a channel only when that exact selected result
    * owns its branch-bound proof in the Cause's direct section.
    */
  def relativeCauseChannelHasOwnedPassedPawnResultProof(
      cause: RelativeCauseFact,
      channel: DirectCauseChannel
  ): Boolean =
    relativeCauseChannelHasOwnedPassedPawnResultAuthority(cause, channel)

  /** One passed-pawn-result authority boundary is shared by C admission and later
    * public-readiness checks. Only an exact selected result is admitted.
    */
  private def relativeCauseChannelHasOwnedPassedPawnResultAuthority(
      cause: RelativeCauseFact,
      channel: DirectCauseChannel
  ): Boolean =
    val directSection = cause.proof.map(_.directProof)
    val eventLine = relativeCauseBinding(cause).map(_.eventLine)
    val exactPassedPawnResultAuthority =
      (directSection, eventLine, channel.rootOwnedProof.flatMap(RootOwnedEffectPolicy.exactPassedPawnResultPrimitive)) match
        case (Some(section), Some(line), Some((source, result))) =>
          cause.attribution.directProofEligible &&
            cause.passedPawnResultCauseKind &&
            channel.binding.line.contains(line) &&
            section.sourceRefs.exists(_.id == source.id) &&
            channel.binding.source == source &&
            RootOwnedEffectPolicy
              .passedPawnResultProofs(cause, source, result)
              .exists(_._1 == result.assessment) &&
            RootOwnedEffectPolicy.admits(cause, this, channel)
        case _ => false
    exactPassedPawnResultAuthority

  def relativeCauseHasOwnedTacticalProof(cause: RelativeCauseFact): Boolean =
    cause.attribution.directProofEligible &&
      cause.proof.exists(proof =>
        (cause.kind == RelativeCauseKind.WrongMoveOrder &&
          relativeCauseProofRecords(proof.directProof).exists {
            case record @ EvidenceRecord(_, _: ForcedReplyResourceDifferentialEvidence, _) =>
              proofEligible(record)
            case _ => false
          }) ||
          relativeCauseTacticalMechanisms(proof.directProof).nonEmpty ||
          relativeCauseRelations(proof.directProof).exists { case (_, payload) =>
            payload.hasConcreteWitness && payload.hasLineProof
          } ||
          relativeCauseOwnedLineConsequences(cause, proof.directProof)
            .exists { case (_, consequence) => LineConsequenceKind.tacticalDriver(consequence.kind) }
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

  def parentClosure(origin: EvidenceRecord): List[EvidenceRecord] =
    val visited = scala.collection.mutable.Set.empty[String]
    val ordered = scala.collection.mutable.ListBuffer.empty[EvidenceRecord]
    def visit(refs: List[EvidenceRef]): Unit =
      refs.foreach { ref =>
        record(ref).foreach { parent =>
          if visited.add(parent.ref.id) then
            ordered += parent
            visit(parent.parents)
        }
      }
    visit(origin.parents)
    ordered.toList

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
        case PositionOccurrenceEvidence(occurrence) =>
          exactAuthority(record, EvidenceProducer.PositionOccurrenceProducer, EvidenceLayer.PositionOccurrence) &&
            record.ref.confidence == EvidenceConfidence.BoardDerived &&
            record.ref.line.isEmpty &&
            PrincipalVariationEvidence.sameBoardState(occurrence.fen, record.ref.position.fen) &&
            occurrence.plyCount == record.ref.position.ply &&
            record.ref.position.sideToMove.forall(_ == occurrence.sideToMove)
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
            payload.canonicalOutputShapeCertified
        case payload: ForcedReplyResourceDifferentialEvidence =>
          exactAuthority(record, EvidenceProducer.CausalProofProducer, EvidenceLayer.CausalProof) &&
            payload.exactOccurrenceCertified(record)
        case payload: PassedPawnResultProofEvidence =>
          exactAuthority(record, EvidenceProducer.CausalProofProducer, EvidenceLayer.CausalProof) &&
            record.ref.confidence == EvidenceConfidence.LegalReplayVerified &&
            payload.exactOccurrenceCertified(record)
        case payload: RelationFactEvidence =>
          relationOccurrenceAncestryVerified(record, payload)
        case payload: ClosedRelationOccurrenceEvidence =>
          closedRelationOccurrenceAncestryVerified(record, payload)
        case payload: TacticalMechanismEvidence =>
          tacticalMechanismAncestryVerified(record, payload)
        case payload: PassedPawnResultEventEvidence =>
          exactAuthority(record, EvidenceProducer.PassedPawnResultEventProducer, EvidenceLayer.PassedPawnResultEvent) &&
            payload.rootTransitionIsCertified && record.ref.position == payload.rootTransition.from &&
            record.ref.line.contains(payload.rootLine) && record.ref.scope == payload.rootTransition.role.scope &&
            payload.retainsLineOccurrenceOwners(record) && payload.lineOccurrenceOwners.forall(owner =>
              byId.get(owner.id).exists(_.ref == owner)
            )
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
    val transitionParents = directParents.collect {
      case record @ EvidenceRecord(ref, MoveTransitionEvidence(moveUci, from, to, _), _)
          if ref.producer == EvidenceProducer.MoveTransitionProducer &&
            ref.layer == EvidenceLayer.MoveTransition &&
            ref.confidence == EvidenceConfidence.LegalReplayVerified &&
            EvidenceRef.sameMove(moveUci, payload.moveUci) &&
            from == payload.from &&
            to == payload.to =>
        record
    }
    val transitionParent = transitionParents match
      case exact :: Nil => Some(exact)
      case _            => None
    def positionParent(position: PositionNodeRef): Boolean =
      directParents.exists {
        case EvidenceRecord(ref, PositionOccurrenceEvidence(occurrence), _) =>
          ref.producer == EvidenceProducer.PositionOccurrenceProducer &&
            ref.layer == EvidenceLayer.PositionOccurrence &&
            ref.confidence == EvidenceConfidence.BoardDerived &&
            ref.position == position &&
            PrincipalVariationEvidence.sameBoardState(occurrence.fen, position.fen) &&
            occurrence.plyCount == position.ply &&
            position.sideToMove.forall(_ == occurrence.sideToMove)
        case _ =>
          false
      }
    val expectedResultKeys = payload.consequences.flatMap(_.resultPremiseKeys).toSet
    val exactResultPremiseSources =
      payload.resultPremiseSources.map(_.key).distinct.size == payload.resultPremiseSources.size &&
        payload.resultPremiseSources.map(_.source).distinct.size == payload.resultPremiseSources.size &&
        payload.resultPremiseSources.map(_.key).toSet == expectedResultKeys &&
        payload.resultPremiseSources.forall { premise =>
          directParentsById.get(premise.source.id).exists {
            case EvidenceRecord(ref, relation: RelationFactEvidence, parents) =>
              ref == premise.source &&
                ref.line == payload.line &&
                relation.proofStage != RelationProofStage.PositionFact &&
                DerivedRelationResultKey.from(relation) == premise.key &&
                relation.mentionsLineMove(payload.moveUci) &&
                relationGraph.contains(ref, relation) &&
                parents.exists(parent => byId.get(parent.id).exists {
                  case EvidenceRecord(occurrenceRef, occurrence: ClosedRelationOccurrenceEvidence, _) =>
                    occurrenceRef == parent && occurrenceRef.line == payload.line &&
                      occurrence.lineOwner == payload.line &&
                      transitionParent.exists(exact => occurrence.edge.evidence == exact.ref) &&
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
      transitionParent.nonEmpty &&
      positionParent(payload.from) &&
      positionParent(payload.to) &&
      exactResultPremiseSources &&
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
    )

  val empty: TypedEvidenceGraph =
    val records = scala.collection.immutable.VectorMap.empty[String, EvidenceRecord]
    new TypedEvidenceGraph(records, CanonicalRelationGraph.empty)
