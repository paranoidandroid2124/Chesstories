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

/** Exact typed proof owned by one root-causal effect. */
enum RootOwnedEffectProof:
  case UniqueCheckReplyDefenderDisplacementBeforeCapture(
      source: EvidenceRef,
      result: UniqueCheckReplyDefenderDisplacementBeforeCaptureEvidence
  )
  case SoleRecapturerRemovalBeforeTargetCapture(
      source: EvidenceRef,
      result: SoleRecapturerRemovalBeforeTargetCaptureEvidence
  )
  case VacatedGateEnablesUnrecapturableSliderCapture(
      source: EvidenceRef,
      result: VacatedGateEnablesUnrecapturableSliderCaptureEvidence
  )
  case SquareReleaseRoute(
      source: EvidenceRef,
      result: SquareReleaseRouteEvidence
  )
  case PassedPawnProgressRealizedAfterOnlyLegalReply(
      source: EvidenceRef,
      result: PassedPawnProgressRealizedAfterOnlyLegalReplyProofEvidence
  )

  /** Evidence record that owns the effect. */
  final def primitiveSource: EvidenceRef =
    this match
      case RootOwnedEffectProof.UniqueCheckReplyDefenderDisplacementBeforeCapture(source, _) => source
      case RootOwnedEffectProof.SoleRecapturerRemovalBeforeTargetCapture(source, _)         => source
      case RootOwnedEffectProof.VacatedGateEnablesUnrecapturableSliderCapture(source, _)     => source
      case RootOwnedEffectProof.SquareReleaseRoute(source, _)                          => source
      case RootOwnedEffectProof.PassedPawnProgressRealizedAfterOnlyLegalReply(source, _)               => source

  final def provenance: List[EvidenceRef] =
    this match
      case RootOwnedEffectProof.UniqueCheckReplyDefenderDisplacementBeforeCapture(_, result) => result.proofParentSources
      case RootOwnedEffectProof.SoleRecapturerRemovalBeforeTargetCapture(_, result)         => result.proofParentSources
      case RootOwnedEffectProof.VacatedGateEnablesUnrecapturableSliderCapture(_, result)     => result.proofParentSources
      case RootOwnedEffectProof.SquareReleaseRoute(_, result)                          => result.proofParentSources
      case RootOwnedEffectProof.PassedPawnProgressRealizedAfterOnlyLegalReply(_, result)                => result.proofParentSources

  final def eventLine: LineNodeRef =
    this match
      case RootOwnedEffectProof.UniqueCheckReplyDefenderDisplacementBeforeCapture(_, result) => result.occurrence.referenceLine
      case RootOwnedEffectProof.SoleRecapturerRemovalBeforeTargetCapture(_, result)         => result.occurrence.referenceLine
      case RootOwnedEffectProof.VacatedGateEnablesUnrecapturableSliderCapture(_, result)     => result.occurrence.referenceLine
      case RootOwnedEffectProof.SquareReleaseRoute(_, result)                          => result.occurrence.referenceLine
      case RootOwnedEffectProof.PassedPawnProgressRealizedAfterOnlyLegalReply(_, result)                => result.rootLine

  /** Compact producer-issued identity for one exact proof occurrence. */
  final def occurrenceIdentity: RootOwnedEffectOccurrenceIdentity =
    this match
      case RootOwnedEffectProof.UniqueCheckReplyDefenderDisplacementBeforeCapture(source, result) =>
        RootOwnedEffectOccurrenceIdentity(
          family = "unique-check-reply-defender-displacement-before-capture",
          sourceEvidenceId = source.id,
          semanticId = result.semanticId,
          occurrenceId = result.occurrenceId,
          dependencyFingerprint = result.dependencyId,
          proofPathOccurrenceIds = result.publicProofPaths.map(_.pathOccurrenceId).sorted
        )
      case RootOwnedEffectProof.SoleRecapturerRemovalBeforeTargetCapture(source, result) =>
        RootOwnedEffectOccurrenceIdentity(
          family = "sole-recapturer-removal-before-target-capture",
          sourceEvidenceId = source.id,
          semanticId = result.semanticId,
          occurrenceId = result.occurrenceId,
          dependencyFingerprint = result.dependencyId,
          proofPathOccurrenceIds = result.publicProofPaths.map(_.pathOccurrenceId).sorted
        )
      case RootOwnedEffectProof.VacatedGateEnablesUnrecapturableSliderCapture(source, result) =>
        RootOwnedEffectOccurrenceIdentity(
          family = "vacated-gate-enables-unrecapturable-slider-capture",
          sourceEvidenceId = source.id,
          semanticId = result.semanticId,
          occurrenceId = result.occurrenceId,
          dependencyFingerprint = result.dependencyId,
          proofPathOccurrenceIds = result.publicProofPaths.map(_.pathOccurrenceId).sorted
        )
      case RootOwnedEffectProof.SquareReleaseRoute(source, result) =>
        RootOwnedEffectOccurrenceIdentity(
          family = "square-release-route",
          sourceEvidenceId = source.id,
          semanticId = result.semanticId,
          occurrenceId = result.occurrenceId,
          dependencyFingerprint = result.dependencyId,
          proofPathOccurrenceIds = result.publicProofPaths.map(_.pathOccurrenceId).sorted
        )
      case RootOwnedEffectProof.PassedPawnProgressRealizedAfterOnlyLegalReply(source, result) =>
        RootOwnedEffectOccurrenceIdentity(
          family = "passed-pawn-progress-realized-after-only-legal-reply",
          sourceEvidenceId = source.id,
          semanticId = result.semanticId,
          occurrenceId = result.occurrenceId,
          dependencyFingerprint = result.dependencyFingerprint,
          proofPathOccurrenceIds = result.publicProofPaths.map(_.pathOccurrenceId).sorted
        )

final case class RootOwnedEffectOccurrenceIdentity(
    family: String,
    sourceEvidenceId: String,
    semanticId: String,
    occurrenceId: String,
    dependencyFingerprint: String,
    proofPathOccurrenceIds: List[String]
):
  require(family.nonEmpty, "an exact L2 proof occurrence requires a family")
  require(sourceEvidenceId.nonEmpty, "an exact L2 proof occurrence requires a source owner")
  require(semanticId.nonEmpty, "an exact L2 proof occurrence requires a semantic id")
  require(occurrenceId.nonEmpty, "an exact L2 proof occurrence requires an occurrence id")
  require(dependencyFingerprint.nonEmpty, "an exact L2 proof occurrence requires a dependency fingerprint")
  require(
    proofPathOccurrenceIds.nonEmpty && proofPathOccurrenceIds.distinct.size == proofPathOccurrenceIds.size,
    "an exact L2 proof occurrence requires distinct owned proof paths"
  )
  require(
    proofPathOccurrenceIds == proofPathOccurrenceIds.sorted,
    "an exact L2 proof occurrence requires canonical proof-path order"
  )

  private[judgment] def stableKey: String =
    List(
      family,
      sourceEvidenceId,
      semanticId,
      occurrenceId,
      dependencyFingerprint,
      proofPathOccurrenceIds.mkString("[", ",", "]")
    ).map(RootOwnedEffectChannelOccurrenceFingerprint.atom).mkString

  /** Exact chess occurrence independent of the evidence owner id. Used only
    * to reject a second producer for the same typed truth.
    */
  private[judgment] def producerIndependentKey: String =
    List(
      family,
      semanticId,
      occurrenceId,
      dependencyFingerprint,
      proofPathOccurrenceIds.mkString("[", ",", "]")
    ).map(RootOwnedEffectChannelOccurrenceFingerprint.atom).mkString

/** One exact typed proof occurrence prepared for an upper Cause consumer. */
final case class DirectCauseChannel(
    rootOwnedProof: RootOwnedEffectProof
):
  def source: EvidenceRef = rootOwnedProof.primitiveSource
  def provenance: List[EvidenceRef] = rootOwnedProof.provenance
  def eventLine: LineNodeRef = rootOwnedProof.eventLine
  private[chessjudgment] def familyMetadata: DirectCauseFamilyMetadata =
    RootOwnedEffectPolicy.familyMetadata(rootOwnedProof)

  /** The typed producer's proposition and occurrence identity. */
  def typedProofIdentity: RootOwnedEffectOccurrenceIdentity =
    rootOwnedProof.occurrenceIdentity

  /** Exact truth occurrence beneath carrier ids and wrapper provenance. This
    * is the shared deduplication key for direct-channel production and Cause
    * record canonicalization.
    */
  private[chessjudgment] def exactOccurrenceFingerprint:
      RootOwnedEffectChannelOccurrenceFingerprint =
    RootOwnedEffectChannelOccurrenceFingerprint(
      proofOccurrence = typedProofIdentity
    )

/** `DirectCauseChannel` remains the one stored/public causal representation;
  * this alias names its stronger root-owned role without introducing a second
  * actor/target/mechanism/consequence authority.
  */
type RootOwnedEffect = DirectCauseChannel

final case class RootOwnedEffectChannelOccurrenceFingerprint(
    proofOccurrence: RootOwnedEffectOccurrenceIdentity
):
  /** Deterministic ordering only. Equality of this case class remains the
    * occurrence identity; the ordering key never substitutes for it.
    */
  private[judgment] def stableSortKey: String =
    RootOwnedEffectChannelOccurrenceFingerprint.atom(proofOccurrence.stableKey)

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
  private[chessjudgment] def fromTypedProofRecord(
      record: EvidenceRecord
  ): Option[DirectCauseChannel] =
    record match
      case EvidenceRecord(ref, payload: UniqueCheckReplyDefenderDisplacementBeforeCaptureEvidence, _) =>
        Some(DirectCauseChannel(RootOwnedEffectProof.UniqueCheckReplyDefenderDisplacementBeforeCapture(ref, payload)))
      case EvidenceRecord(ref, payload: SoleRecapturerRemovalBeforeTargetCaptureEvidence, _) =>
        Some(DirectCauseChannel(RootOwnedEffectProof.SoleRecapturerRemovalBeforeTargetCapture(ref, payload)))
      case EvidenceRecord(ref, payload: VacatedGateEnablesUnrecapturableSliderCaptureEvidence, _) =>
        Some(DirectCauseChannel(RootOwnedEffectProof.VacatedGateEnablesUnrecapturableSliderCapture(ref, payload)))
      case EvidenceRecord(ref, payload: SquareReleaseRouteEvidence, _) =>
        Some(DirectCauseChannel(RootOwnedEffectProof.SquareReleaseRoute(ref, payload)))
      case EvidenceRecord(ref, payload: PassedPawnProgressRealizedAfterOnlyLegalReplyProofEvidence, _) =>
        Some(DirectCauseChannel(RootOwnedEffectProof.PassedPawnProgressRealizedAfterOnlyLegalReply(ref, payload)))
      case _ => None

  /** Build only channels whose typed proof producer owns this exact Cause.
    * Missing or mismatched proof sources are not adapted into a channel.
    */
  private[chessjudgment] def certifiedForCause(
      cause: RelativeCauseFact,
      graph: TypedEvidenceGraph
  ): List[DirectCauseChannel] =
    validateAndOrder(
      graph.relativeCauseBinding(cause).toList.flatMap { binding =>
        cause.proofSources.flatMap(ref =>
          graph
            .record(ref)
            .flatMap(fromTypedProofRecord)
            .toList
            .flatMap(RootOwnedEffectPolicy.certify(cause, graph, binding, _))
        )
      }
    )

  private[judgment] def validateAndOrder(
      channels: List[DirectCauseChannel]
  ): List[DirectCauseChannel] =
    channels.foreach { channel =>
      require(
        channel.source == channel.rootOwnedProof.primitiveSource,
        "an exact L2 channel carrier must be its typed proof owner"
      )
    }
    val duplicateOccurrences = channels
      .groupMap(_.typedProofIdentity.producerIndependentKey)(_.source.id)
      .collect { case (occurrence, owners) if owners.size > 1 => occurrence -> owners.sorted }
      .toList
      .sortBy(_._1)
    require(
      duplicateOccurrences.isEmpty,
      s"one exact L2 channel occurrence has multiple producers: ${duplicateOccurrences.flatMap(_._2).mkString(",")}"
    )
    channels.sortBy(_.exactOccurrenceFingerprint.stableSortKey)

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

/** Private cache ownership for one deterministic L2 family result set.
  *
  * Every produced owner carries the complete set of dependency fingerprints
  * produced from the same exact demand.  A later assembly can therefore tell
  * a full cache hit from a partially populated graph without deriving the
  * family again.  This is deliberately not a public proof contract.
  */
private[chessjudgment] final case class ExactCausalProofResultSet private (
    dependencyIds: List[String]
):
  require(dependencyIds.nonEmpty, "an exact causal result set cannot be empty")
  require(
    dependencyIds == dependencyIds.sorted && dependencyIds.distinct.size == dependencyIds.size,
    "an exact causal result set must retain one sorted entry per dependency"
  )
  require(
    dependencyIds.forall(_.matches("[0-9a-f]{64}")),
    "an exact causal result set may contain only complete dependency fingerprints"
  )

  def contains(dependencyId: String): Boolean = dependencyIds.contains(dependencyId)

  def exactlyOwns(currentDependencyIds: Iterable[String]): Boolean =
    currentDependencyIds.toList.sorted == dependencyIds

private[chessjudgment] object ExactCausalProofResultSet:
  def from(dependencyIds: List[String]): ExactCausalProofResultSet =
    require(
      dependencyIds.distinct.size == dependencyIds.size,
      "one exact causal dependency may occur only once in its result set"
    )
    ExactCausalProofResultSet(dependencyIds.sorted)

/** First result in the common bounded L2 family. It is deliberately
  * package-private: the only public projection is the existing WrongMoveOrder
  * explanation that consumes this exact actual/counterfactual proof.
  */
private[chessjudgment] final case class UniqueCheckReplyDefenderDisplacementBeforeCaptureEvidence private[chessjudgment] (
    semantic: UniqueCheckReplyDefenderDisplacementBeforeCaptureSemanticProof,
    occurrence: UniqueCheckReplyDefenderDisplacementBeforeCaptureOccurrence,
    dependencyFingerprint: String,
    private[chessjudgment] val resultSet: ExactCausalProofResultSet,
    private[chessjudgment] val occurrenceProof: Option[UniqueCheckReplyDefenderDisplacementBeforeCaptureOccurrenceProof] = None
) extends EvidencePayload:
  require(
    occurrence.semanticId == semantic.semanticId,
    "an L2 occurrence must retain its exact semantic proof"
  )
  require(
    dependencyFingerprint.matches("[0-9a-f]{64}"),
    "an L2 result needs its complete dependency fingerprint"
  )
  require(
    resultSet.contains(dependencyFingerprint),
    "an L2 result must belong to its demand's complete result set"
  )

  def semanticId: String = semantic.semanticId
  def occurrenceId: String = occurrence.occurrenceId
  def dependencyId: String = dependencyFingerprint
  def referenceLine: LineNodeRef = occurrence.referenceLine
  def playedLine: LineNodeRef = occurrence.playedLine
  def referenceSteps: List[LineReplayStep] = occurrence.referenceSteps
  def playedSteps: List[LineReplayStep] = occurrence.playedSteps
  def referenceBranch: CausalBranchOccurrence = occurrence.referenceBranch
  def playedBranch: CausalBranchOccurrence = occurrence.playedBranch
  def proofPaths: List[CausalProofPathOccurrence] = occurrence.proofPaths
  def publicReferenceBranch: BoundedCausalPublicBranch =
    BoundedCausalPublicProjection.branch(occurrence.referenceBranch)
  def publicPlayedBranch: BoundedCausalPublicBranch =
    BoundedCausalPublicProjection.branch(occurrence.playedBranch)
  def publicProofPaths: List[BoundedCausalPublicProofPath] =
    BoundedCausalPublicProjection.verticalRelationPaths(occurrence.proofPaths)
  def hasCompleteProofPaths: Boolean =
    proofPaths.nonEmpty && proofPaths.forall(path =>
      path.premiseUses.nonEmpty && path.closedAbsenceUses.nonEmpty
    )
  def triggerMovement: RelationMoveTransitionWitness = semantic.trigger
  def uniqueCheckReplyDefenderDisplacementBeforeCapture: RelationLegalMoveResourceWitness = semantic.forcedReply
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

  private[chessjudgment] def lowerRecordsAreCanonical(
      byId: Map[String, EvidenceRecord]
  ): Boolean =
    occurrenceProof.exists(_.lowerIssuerRecords.forall(record => byId.get(record.ref.id).contains(record)))
  private[chessjudgment] def consumesDependencies(
      referenceSource: EvidenceRecord,
      playedSource: EvidenceRecord
  ): Boolean =
    occurrenceProof.exists(_.consumesDependencies(referenceSource, playedSource))
/** Exact occurrence of one defense obligation changing between the retained
  * BestReference and Played branches. The mechanism remains family-specific;
  * this payload does not claim general overload or defense collapse.
  */
private[chessjudgment] final case class SoleRecapturerRemovalBeforeTargetCaptureEvidence private[chessjudgment] (
    semantic: SoleRecapturerRemovalBeforeTargetCaptureSemanticProof,
    occurrence: SoleRecapturerRemovalBeforeTargetCaptureOccurrence,
    dependencyFingerprint: String,
    private[chessjudgment] val resultSet: ExactCausalProofResultSet,
    private[chessjudgment] val occurrenceProof: Option[CertifiedSoleRecapturerRemovalBeforeTargetCapture] = None
) extends EvidencePayload:
  require(
    occurrence.semanticId == semantic.semanticId,
    "a sole-recapturer-removal-before-target-capture occurrence must retain its exact semantic proof"
  )
  require(
    dependencyFingerprint.matches("[0-9a-f]{64}"),
    "a sole-recapturer-removal-before-target-capture result needs its complete dependency fingerprint"
  )
  require(
    resultSet.contains(dependencyFingerprint),
    "a sole-recapturer-removal-before-target-capture result must belong to its demand's complete result set"
  )

  def semanticId: String = semantic.semanticId
  def occurrenceId: String = occurrence.occurrenceId
  def dependencyId: String = dependencyFingerprint
  def referenceLine: LineNodeRef = occurrence.referenceLine
  def playedLine: LineNodeRef = occurrence.playedLine
  def referenceSteps: List[LineReplayStep] = occurrence.referenceSteps
  def playedSteps: List[LineReplayStep] = occurrence.playedSteps
  def referenceBranch: CausalBranchOccurrence = occurrence.referenceBranch
  def playedBranch: CausalBranchOccurrence = occurrence.playedBranch
  def proofPaths: List[CausalProofPathOccurrence] = occurrence.proofPaths
  def publicReferenceBranch: BoundedCausalPublicBranch =
    BoundedCausalPublicProjection.branch(occurrence.referenceBranch)
  def publicPlayedBranch: BoundedCausalPublicBranch =
    BoundedCausalPublicProjection.branch(occurrence.playedBranch)
  def publicProofPaths: List[BoundedCausalPublicProofPath] =
    BoundedCausalPublicProjection.verticalRelationPaths(occurrence.proofPaths)
  def hasCompleteProofPaths: Boolean =
    proofPaths.nonEmpty && proofPaths.forall(path =>
      path.premiseUses.size == 3 && path.closedAbsenceUses.size == 1
    )
  def remover: RelationMoveTransitionWitness = semantic.remover
  def removedDefender: RelationColoredPieceWitness = semantic.removedDefender
  def removalRecapture: RelationLegalMoveResourceWitness = semantic.removalRecapture
  def laterExploit: RelationMoveTransitionWitness = semantic.exploit
  def capturedTarget: RelationColoredPieceWitness = semantic.capturedTarget
  def playedSoleRecapture: RelationLegalMoveResourceWitness = semantic.playedRecapture
  def laterExploitMove: String = occurrence.referenceExploitStep.moveUci
  def playedSoleRecaptureMove: String = occurrence.playedRecaptureStep.moveUci

  private[chessjudgment] def exactOccurrenceCertified(record: EvidenceRecord): Boolean =
    occurrenceProof.exists(_.proves(record, this))

  private[chessjudgment] def proofParentSources: List[EvidenceRef] =
    occurrenceProof.toList.flatMap(_.parentSources)

  private[chessjudgment] def lowerRecordsAreCanonical(
      byId: Map[String, EvidenceRecord]
  ): Boolean =
    occurrenceProof.exists(_.lowerIssuerRecords.forall(record => byId.get(record.ref.id).contains(record)))

  private[chessjudgment] def consumesDependencies(
      referenceSource: EvidenceRecord,
      playedSource: EvidenceRecord
  ): Boolean =
    occurrenceProof.exists(_.consumesDependencies(referenceSource, playedSource))

/** Exact capture-only gate release: the reference root vacates a certified
  * gate, the same slider later consumes the opened line, and the Played
  * sibling retains the exact blocker and lacks that legal capture resource.
  */
private[chessjudgment] final case class VacatedGateEnablesUnrecapturableSliderCaptureEvidence private[chessjudgment] (
    semantic: VacatedGateEnablesUnrecapturableSliderCaptureSemanticProof,
    occurrence: VacatedGateEnablesUnrecapturableSliderCaptureOccurrence,
    dependencyFingerprint: String,
    private[chessjudgment] val resultSet: ExactCausalProofResultSet,
    private[chessjudgment] val occurrenceProof: Option[CertifiedVacatedGateEnablesUnrecapturableSliderCapture] = None
) extends EvidencePayload:
  require(
    occurrence.semanticId == semantic.semanticId,
    "a vacated-gate-enables-unrecapturable-slider-capture occurrence must retain its semantic proof"
  )
  require(
    dependencyFingerprint.matches("[0-9a-f]{64}"),
    "a vacated-gate-enables-unrecapturable-slider-capture result needs its complete dependency fingerprint"
  )
  require(
    resultSet.contains(dependencyFingerprint),
    "a direct line-access result must belong to its demand's complete result set"
  )

  def semanticId: String = semantic.semanticId
  def occurrenceId: String = occurrence.occurrenceId
  def dependencyId: String = dependencyFingerprint
  def referenceLine: LineNodeRef = occurrence.referenceLine
  def playedLine: LineNodeRef = occurrence.playedLine
  def referenceSteps: List[LineReplayStep] = occurrence.referenceSteps
  def playedSteps: List[LineReplayStep] = occurrence.playedSteps
  def referenceBranch: CausalBranchOccurrence = occurrence.referenceBranch
  def playedBranch: CausalBranchOccurrence = occurrence.playedBranch
  def proofPaths: List[CausalProofPathOccurrence] = occurrence.proofPaths
  def publicReferenceBranch: BoundedCausalPublicBranch =
    BoundedCausalPublicProjection.branch(occurrence.referenceBranch)
  def publicPlayedBranch: BoundedCausalPublicBranch =
    BoundedCausalPublicProjection.branch(occurrence.playedBranch)
  def publicProofPaths: List[BoundedCausalPublicProofPath] =
    BoundedCausalPublicProjection.verticalRelationPaths(occurrence.proofPaths)
  def hasCompleteProofPaths: Boolean =
    proofPaths.nonEmpty && proofPaths.forall(path =>
      path.premiseUses.size == 2 && path.closedAbsenceUses.size == 3 &&
        path.closedStateUses.nonEmpty
    )
  def enabler: RelationMoveTransitionWitness = semantic.enabler
  def slider: RelationColoredPieceWitness = semantic.slider
  def gateBlocker: RelationColoredPieceWitness = semantic.gateBlocker
  def exploit: RelationMoveTransitionWitness = semantic.exploit
  def capturedTarget: RelationColoredPieceWitness = semantic.capturedTarget
  def exploitMove: String = occurrence.exploitStep.moveUci

  private[chessjudgment] def exactOccurrenceCertified(record: EvidenceRecord): Boolean =
    occurrenceProof.exists(_.proves(record, this))

  private[chessjudgment] def proofParentSources: List[EvidenceRef] =
    occurrenceProof.toList.flatMap(_.parentSources)

  private[chessjudgment] def lowerRecordsAreCanonical(
      byId: Map[String, EvidenceRecord]
  ): Boolean =
    occurrenceProof.exists(_.lowerIssuerRecords.forall(record => byId.get(record.ref.id).contains(record)))

  private[chessjudgment] def consumesDependencies(
      referenceSource: EvidenceRecord,
      playedSource: EvidenceRecord
  ): Boolean =
    occurrenceProof.exists(_.consumesDependencies(referenceSource, playedSource))

/** Derivation-free public detail for the exact route terminal. Lower result
  * ids and sources remain in each public proof path.
  */
enum SquareReleaseRoutePublicTerminal:
  case Occupation
  case Capture(
      assertionId: String,
      captured: RelationColoredPieceWitness,
      geometricRecapturers: List[RelationPieceWitness],
      legalRecaptures: List[RelationLegalMoveResourceWitness],
      restrictedRecaptures: List[RelationRestrictedResourceWitness]
  )
  case CreatedCheck(
      assertionId: String,
      checkedSide: Color,
      kingSquare: EvidenceSquare,
      checkers: List[RelationPieceWitness],
      responses: List[RelationCheckResponseWitness],
      controlledKingDestinations: List[RelationControlledKingDestinationWitness],
      terminal: RelationCheckTerminalState
  )

object SquareReleaseRoutePublicTerminal:
  private[chessjudgment] def from(
      terminal: SquareReleaseRouteTerminal
  ): SquareReleaseRoutePublicTerminal =
    terminal match
      case SquareReleaseRouteTerminal.Occupation => Occupation
      case SquareReleaseRouteTerminal.Capture(
            assertionId,
            _,
            captured,
            geometricRecapturers,
            legalRecaptures,
            restrictedRecaptures
          ) =>
        Capture(
          assertionId,
          captured,
          geometricRecapturers,
          legalRecaptures,
          restrictedRecaptures
        )
      case SquareReleaseRouteTerminal.CreatedCheck(
            assertionId,
            _,
            checkedSide,
            kingSquare,
            checkers,
            responses,
            controlledKingDestinations,
            terminal
          ) =>
        CreatedCheck(
          assertionId,
          checkedSide,
          kingSquare,
          checkers,
          responses,
          controlledKingDestinations,
          terminal
        )

/** Exact bounded square release consumed by an ordered same-object route.
  * The Played sibling is closed immediately before the first route leg; a
  * multi-leg reference occurrence additionally retains its exact L1 terminal
  * and actual reply when that terminal is ongoing.
  */
private[chessjudgment] final case class SquareReleaseRouteEvidence private[chessjudgment] (
    semantic: SquareReleaseRouteSemanticProof,
    occurrence: SquareReleaseRouteOccurrence,
    dependencyFingerprint: String,
    private[chessjudgment] val resultSet: ExactCausalProofResultSet,
    private[chessjudgment] val occurrenceProof: Option[CertifiedSquareReleaseRoute] = None
) extends EvidencePayload:
  require(occurrence.semanticId == semantic.semanticId)
  require(
    dependencyFingerprint.matches("[0-9a-f]{64}"),
    "a square-release-route result needs its complete dependency fingerprint"
  )
  require(
    resultSet.contains(dependencyFingerprint),
    "a square-release-route result must belong to its demand's complete result set"
  )

  def semanticId: String = semantic.semanticId
  def occurrenceId: String = occurrence.occurrenceId
  def dependencyId: String = dependencyFingerprint
  def referenceLine: LineNodeRef = occurrence.referenceLine
  def playedLine: LineNodeRef = occurrence.playedLine
  def referenceSteps: List[LineReplayStep] = occurrence.referenceSteps
  def playedSteps: List[LineReplayStep] = occurrence.playedSteps
  def referenceBranch: CausalBranchOccurrence = occurrence.referenceBranch
  def playedBranch: CausalBranchOccurrence = occurrence.playedBranch
  def proofPaths: List[CausalProofPathOccurrence] = occurrence.proofPaths
  def publicReferenceBranch: BoundedCausalPublicBranch =
    BoundedCausalPublicProjection.branch(referenceBranch)
  def publicPlayedBranch: BoundedCausalPublicBranch =
    BoundedCausalPublicProjection.branch(playedBranch)
  def publicProofPaths: List[BoundedCausalPublicProofPath] =
    BoundedCausalPublicProjection.mixedPaths(proofPaths)
  def hasCompleteProofPaths: Boolean =
    val persistenceCount = occurrence.routeStepIndices.zip(occurrence.routeStepIndices.drop(1))
      .map { case (before, after) => after - before - 1 }
      .sum
    val expectedStates =
      occurrence.firstRouteStepIndex + occurrence.routeStepIndices.size + persistenceCount +
        occurrence.firstRouteStepIndex * 2
    proofPaths.nonEmpty && proofPaths.forall(path =>
      path.manifest.supplementalPremiseUses.size ==
        1 + occurrence.routeStepIndices.size + occurrence.terminalReplyStepIndex.size &&
        path.manifest.supplementalPremiseUses.forall(_.isInstanceOf[CausalLegalMovePremiseUse]) &&
        path.premiseUses.size == (semantic.terminal match
          case SquareReleaseRouteTerminal.Occupation => 0
          case _                                     => 1) &&
        path.closedAbsenceUses.size == 1 && path.closedStateUses.size == expectedStates
    )
  def releaser: RelationMoveTransitionWitness = semantic.release
  def releasedBlocker: RelationColoredPieceWitness = semantic.blocker
  def routePiece: RelationColoredPieceWitness = semantic.routePiece
  def route: List[RelationMoveTransitionWitness] = semantic.route
  def routeMoves: List[String] = occurrence.routeSteps.map(_.moveUci)
  def routeStepIndices: List[Int] = occurrence.routeStepIndices
  def terminalStepIndex: Int = occurrence.terminalStepIndex
  def terminalReplyMove: Option[String] = occurrence.terminalReplyStep.map(_.moveUci)
  def publicTerminal: SquareReleaseRoutePublicTerminal =
    SquareReleaseRoutePublicTerminal.from(semantic.terminal)

  private[chessjudgment] def exactOccurrenceCertified(record: EvidenceRecord): Boolean =
    occurrenceProof.exists(_.proves(record, this))

  private[chessjudgment] def proofParentSources: List[EvidenceRef] =
    occurrenceProof.toList.flatMap(_.parentSources)

  private[chessjudgment] def lowerRecordsAreCanonical(
      byId: Map[String, EvidenceRecord]
  ): Boolean =
    occurrenceProof.exists(_.lowerIssuerRecords.forall(record => byId.get(record.ref.id).contains(record)))

  private[chessjudgment] def consumesDependencies(
      referenceSource: EvidenceRecord,
      playedSource: EvidenceRecord
  ): Boolean =
    occurrenceProof.exists(_.consumesDependencies(referenceSource, playedSource))

/** Read-only public occurrence projection of one exact replay step. */
final case class PassedPawnProgressPublicStep(
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
    provenance: String
)

/** One occurrence path. Transposed semantics may be shared, while this exact
  * branch and its provenance remain visible and are never de-duplicated.
  */
final case class PassedPawnProgressPublicBranch(
    branchId: String,
    role: String,
    replyMove: String,
    sourceOccurrenceId: String,
    lineId: String,
    lineRole: String,
    lineRank: Int,
    lineRootMove: String,
    rootProvenance: String,
    steps: List[PassedPawnProgressPublicStep]
):
  require(
    role == "played_root_analysis_continuation" && EvidenceRef.normalizeMove(replyMove).nonEmpty &&
      sourceOccurrenceId.nonEmpty,
    "a public passed-pawn result branch needs its exact analysis-continuation identity"
  )

/** One exact lower-premise occurrence use retained by an independent path. */
final case class PassedPawnProgressPublicSquareWitness(
    role: String,
    square: String
):
  require(role.nonEmpty && square.matches("[a-h][1-8]"), "a public dependency square needs an exact labeled coordinate")

final case class PassedPawnProgressPublicPieceWitness(
    role: String,
    side: String,
    piece: String
):
  require(
    role.nonEmpty && Set("white", "black")(side) &&
      Set("pawn", "knight", "bishop", "rook", "queen", "king")(piece),
    "a public dependency piece needs an exact labeled side and role"
  )

final case class PassedPawnProgressPublicRelationIssuer(
    contract: String,
    relationKind: String,
    resultKey: String,
    occurrenceId: String,
    stepKey: String,
    sourcePremiseIds: List[String],
    issuerEvidenceId: String,
    line: LineNodeRef,
    scope: EvidenceScope
):
  require(contract.nonEmpty && relationKind.nonEmpty, "a public relation issuer needs its exact contract and result kind")
  require(
    contract == relationKind &&
      resultKey.matches(s"${java.util.regex.Pattern.quote(contract)}:[0-9a-f]{64}") &&
      occurrenceId.matches("[0-9a-f]{64}") && stepKey.nonEmpty && issuerEvidenceId.nonEmpty &&
        scope == line.role.scope,
    "a public relation issuer needs exact result and occurrence identities"
  )
  require(
    sourcePremiseIds.nonEmpty && sourcePremiseIds == sourcePremiseIds.distinct.sorted,
    "a public relation issuer needs canonical lower premise owners"
  )

sealed trait PassedPawnProgressPublicPositionStateWitness:
  def kind: String
  def side: String
  def square: String

object PassedPawnProgressPublicPositionStateWitness:
  private val sides = Set("white", "black")
  private val pieces = Set("pawn", "knight", "bishop", "rook", "queen", "king")
  private val sliders = Set("bishop", "rook", "queen")

  private def validSquare(square: String): Boolean = square.matches("[a-h][1-8]")

  final case class OccupiedBy(side: String, square: String, piece: String)
      extends PassedPawnProgressPublicPositionStateWitness:
    val kind = "occupied_by"
    require(
      sides(side) && validSquare(square) && pieces(piece),
      "a public occupant state needs an exact side, square, and piece"
    )

  final case class SliderReachSegment(square: String, target: String, occupantPiece: Option[String]):
    require(
      (target == "empty" && occupantPiece.isEmpty) ||
        (Set("friendly", "enemy")(target) && occupantPiece.exists(pieces)),
      "a public slider segment needs an exact target occupancy"
    )
    require(validSquare(square), "a public slider segment needs an exact board square")

  final case class SliderReach(
      side: String,
      square: String,
      piece: String,
      fileStep: Int,
      rankStep: Int,
      segment: List[SliderReachSegment]
  ) extends PassedPawnProgressPublicPositionStateWitness:
    val kind = "slider_reach"
    require(
      sides(side) && validSquare(square) && sliders(piece) &&
        (-1 to 1).contains(fileStep) && (-1 to 1).contains(rankStep) &&
        (fileStep != 0 || rankStep != 0),
      "a public slider reach needs its typed wire fields"
    )

  final case class PawnTopology(side: String, square: String, passed: Boolean)
      extends PassedPawnProgressPublicPositionStateWitness:
    val kind = "pawn_topology"
    require(
      sides(side) && validSquare(square),
      "a public pawn-topology witness needs its typed wire fields"
    )

final case class PassedPawnProgressPublicPositionStateIssuer(
    state: PassedPawnProgressPublicPositionStateWitness,
    semanticProofId: String,
    issuerEvidenceId: String,
    issuerOccurrenceId: String,
    stepKey: String,
    ply: Int,
    moveUci: String,
    fenBefore: String,
    fenAfter: String,
    line: LineNodeRef,
    scope: EvidenceScope
):
  require(semanticProofId.matches("[0-9a-f]{64}"), "a public position-state issuer needs its semantic id")
  require(
    issuerEvidenceId.nonEmpty && issuerOccurrenceId.matches("[0-9a-f]{64}") && stepKey.nonEmpty &&
      ply > 0 && EvidenceRef.normalizeMove(moveUci).nonEmpty && fenBefore.nonEmpty && fenAfter.nonEmpty,
    "a public position-state issuer needs exact LegalLine and replay occurrence ownership"
  )
  require(scope == line.role.scope, "a public position-state issuer scope must match its exact line owner")

final case class PassedPawnProgressPublicDependencyProof(
    dependencyKind: String,
    proofKind: String,
    squares: List[PassedPawnProgressPublicSquareWitness],
    pieces: List[PassedPawnProgressPublicPieceWitness],
    relationIssuers: List[PassedPawnProgressPublicRelationIssuer],
    positionStateIssuers: List[PassedPawnProgressPublicPositionStateIssuer]
):
  require(squares.distinct.size == squares.size, "a public dependency proof cannot duplicate a square witness")
  require(pieces.distinct.size == pieces.size, "a public dependency proof cannot duplicate a piece witness")
  require(
    relationIssuers.distinct.size == relationIssuers.size &&
      positionStateIssuers.distinct.size == positionStateIssuers.size,
    "a public dependency proof cannot duplicate issuer occurrences"
  )

final case class PassedPawnProgressPublicPremise(
    role: String,
    lowerKind: String,
    lowerSemanticKey: String,
    sourcePremiseIds: List[String],
    branchId: String,
    branchRole: String,
    fromStepIndex: Int,
    toStepIndex: Int,
    dependencyProof: Option[PassedPawnProgressPublicDependencyProof] = None
)

/** Public projection of one independent proof path. */
final case class PassedPawnProgressPublicProofPath(
    pathOccurrenceId: String,
    analysisContinuationBranchId: String,
    realizationActor: PassedPawnResultActorOccurrence,
    realizationMove: String,
    realizationPly: Int,
    premises: List[PassedPawnProgressPublicPremise],
    closureUseIds: List[String]
)

/** Closed legal-reply inventory issued by the exact lower StructuralDelta authority. */
final case class PassedPawnProgressPublicClosedReplyInventory(
    issuerEvidenceId: String,
    rootAfterFen: String,
    rootAfterPly: Int,
    scope: String,
    legalReplyMove: String,
    analysisContinuationBranchId: String
)

/** Publicly consumed L2 result for passed-pawn progress realized after the only legal reply.
  * The lower passed-pawn result event remains a premise; the certified proposition is the
  * narrower ordered realization retained by this exact proof.
  */
final case class PassedPawnProgressRealizedAfterOnlyLegalReplyProofEvidence private[chessjudgment] (
    eventSource: EvidenceRef,
    event: PassedPawnResultEventEvidence,
    resultRoutes: List[PassedPawnResultRoute],
    semanticIdentity: PassedPawnProgressSemanticIdentity,
    private[chessjudgment] val proofSet: BoundedCausalProofSet,
    private[chessjudgment] val closedReplyInventory: PassedPawnProgressClosedReplyInventoryBinding,
    dependencyFingerprint: String,
    lowerPremiseIds: List[String],
    private[chessjudgment] val occurrenceProof: Option[PassedPawnProgressOccurrenceProof] = None
) extends EvidencePayload:
  require(dependencyFingerprint.matches("[0-9a-f]{64}"), "a passed-pawn result needs a complete dependency key")
  require(lowerPremiseIds.nonEmpty && lowerPremiseIds == lowerPremiseIds.distinct.sorted)
  require(
    resultRoutes.nonEmpty && resultRoutes == resultRoutes.sortBy(_.stableKey) &&
      resultRoutes.distinct.size == resultRoutes.size &&
      resultRoutes.forall(route => event.exactOnlyReplyResultRoutes.contains(route)) &&
      resultRoutes.map(PassedPawnProgressSemanticIdentity.from(event, _)).distinct == List(semanticIdentity),
    "a passed-pawn result needs every independent route for one exact certified analysis result occurrence"
  )

  def contract: String = "passed_pawn_progress_realized_after_only_legal_reply"
  def semanticId: String = proofSet.proposition.semanticId
  def occurrenceId: String = proofSet.occurrence.occurrenceId
  def rootLine: LineNodeRef = event.rootLine
  def rootMove: String = EvidenceRef.normalizeMove(event.rootMove)
  def rootPly: Int = analysisContinuationBranch.steps.head.step.ply
  def realizingMove: String = EvidenceRef.normalizeMove(resultRoute.sourceEvent.moveUci)
  def realizingPly: Int = resultRoute.sourceEvent.step.ply
  def resultPlyOffset: Int = realizingPly - rootPly
  def perspective: chess.Color = event.perspective
  def consequenceKind: TransitionConsequenceKind = resultRoute.consequence.kind
  def resultTargetSubjects: List[String] = semanticIdentity.resultTargetSubjects
  def rootActor: PassedPawnResultActorOccurrence = event.identity.actor
  def resultActor: PassedPawnResultActorOccurrence = resultRoute.sourceEvent.identity.actor
  def analysisContinuationBranch: CausalBranchOccurrence =
    proofSet.occurrence.branches match
      case exact :: Nil
          if exact.role.isInstanceOf[PassedPawnProgressBranchRole.PlayedRootAnalysisContinuation] => exact
      case _ =>
        throw IllegalStateException("a passed-pawn result lost its single analysis-continuation occurrence")
  def proofPaths: List[CausalProofPathOccurrence] = proofSet.paths
  def legalReplyMove: String = closedReplyInventory.legalReplyMove

  /** Public views are flattened only from the sealed occurrence proof. They
    * perform no move generation or relation extraction.
    */
  def publicBranches: List[PassedPawnProgressPublicBranch] =
    proofSet.occurrence.branches.map(publicBranch)

  def publicProofPaths: List[PassedPawnProgressPublicProofPath] =
    proofSet.paths.map { path =>
      val manifest = path.manifest match
        case exact: PassedPawnProgressPathManifest => exact
        case _ =>
          throw IllegalStateException(
            "a public passed-pawn-progress-realized-after-only-legal-reply path lost its sealed manifest"
          )
      PassedPawnProgressPublicProofPath(
        pathOccurrenceId = path.pathOccurrenceId,
        analysisContinuationBranchId = manifest.analysisContinuationBranchId,
        realizationActor = manifest.resultRoute.sourceEvent.identity.actor,
        realizationMove = EvidenceRef.normalizeMove(manifest.resultRoute.sourceEvent.moveUci),
        realizationPly = manifest.resultRoute.sourceEvent.step.ply,
        premises = manifest.supplementalPremiseUses.map {
          case premise: PassedPawnProgressPremiseUse => publicPremise(premise)
          case _ =>
            throw IllegalStateException(
              "a public passed-pawn-progress-realized-after-only-legal-reply path lost a typed lower premise"
            )
        },
        closureUseIds = path.supplementalClosureUses.map(_.useId)
      )
    }

  def publicClosedReplyInventory: PassedPawnProgressPublicClosedReplyInventory =
    PassedPawnProgressPublicClosedReplyInventory(
      issuerEvidenceId = closedReplyInventory.issuerEvidenceId,
      rootAfterFen = closedReplyInventory.rootAfter.fen,
      rootAfterPly = closedReplyInventory.rootAfter.ply,
      scope = snakeCode(closedReplyInventory.scope.toString),
      legalReplyMove = closedReplyInventory.legalReplyMove,
      analysisContinuationBranchId = closedReplyInventory.analysisContinuationBranchId
    )

  def hasCompleteProofPaths: Boolean =
    proofSet.occurrence.branches == List(analysisContinuationBranch) && proofPaths.nonEmpty &&
      proofPaths.collect {
        case path if path.manifest.isInstanceOf[PassedPawnProgressPathManifest] =>
          path.manifest.asInstanceOf[PassedPawnProgressPathManifest].resultRoute.stableKey
      }.toSet == resultRoutes.map(_.stableKey).toSet &&
      proofPaths.forall {
        case path if path.manifest.isInstanceOf[PassedPawnProgressPathManifest] =>
          path.manifest.asInstanceOf[PassedPawnProgressPathManifest].analysisContinuationBranchId ==
            analysisContinuationBranch.branchId
        case _ => false
      } && closedReplyInventory.branchIds == Set(analysisContinuationBranch.branchId)

  private def resultRoute: PassedPawnResultRoute = resultRoutes.head

  private[chessjudgment] def exactOccurrenceCertified(record: EvidenceRecord): Boolean =
    occurrenceProof.exists(_.proves(record, this))

  private[chessjudgment] def consumesExactDependencies(
      source: EvidenceRecord,
      inventory: EvidenceRecord,
      routes: List[PassedPawnResultRoute]
  ): Boolean =
    occurrenceProof.exists(_.consumesExactDependencies(source, inventory, routes))

  private[chessjudgment] def proofParentSources: List[EvidenceRef] =
    occurrenceProof.toList.flatMap(_.parentSources)

  private[chessjudgment] def lowerRecordsAreCanonical(
      byId: Map[String, EvidenceRecord]
  ): Boolean =
    occurrenceProof.exists(_.lowerIssuerRecords.forall(record => byId.get(record.ref.id).contains(record)))

  private def publicBranch(branch: CausalBranchOccurrence): PassedPawnProgressPublicBranch =
    val (replyMove, sourceOccurrenceId) = branch.role match
      case PassedPawnProgressBranchRole.PlayedRootAnalysisContinuation(move, occurrence) =>
        EvidenceRef.normalizeMove(move) -> occurrence
      case _ =>
        throw IllegalStateException("a public passed-pawn result contains an unsupported branch role")
    PassedPawnProgressPublicBranch(
      branchId = branch.branchId,
      role = "played_root_analysis_continuation",
      replyMove = replyMove,
      sourceOccurrenceId = sourceOccurrenceId,
      lineId = branch.line.id,
      lineRole = snakeCode(branch.line.role.toString),
      lineRank = branch.line.rank,
      lineRootMove = EvidenceRef.normalizeMove(branch.line.rootMove),
      rootProvenance = branch.rootProvenance match
        case CausalRootProvenance.CounterfactualAnalyzedRoot => "counterfactual_analyzed_root"
        case CausalRootProvenance.ObservedGameRoot            => "observed_game_root",
      steps = branch.steps.map { occurrence =>
        val step = occurrence.step
        PassedPawnProgressPublicStep(
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
            case CausalStepProvenance.CertifiedAnalysisMove => "certified_analysis_move"
        )
      }
    )

  private def publicPremise(premise: PassedPawnProgressPremiseUse): PassedPawnProgressPublicPremise =
    PassedPawnProgressPublicPremise(
      role = passedPawnResultPremiseRoleCode(premise.role),
      lowerKind = premise.lowerKind,
      lowerSemanticKey = premise.lowerSemanticKey,
      sourcePremiseIds = premise.sourcePremiseIds,
      branchId = premise.branchId,
      branchRole = passedPawnResultBranchRoleCode(premise.branchRole),
      fromStepIndex = premise.fromStepIndex,
      toStepIndex = premise.toStepIndex,
      dependencyProof = premise.dependencyWitness.map(_.publicProof)
    )

  private def passedPawnResultPremiseRoleCode(role: CausalPremiseRole): String =
    role match
      case PassedPawnProgressPremiseRole.Dependency(_) => "dependency"
      case PassedPawnProgressPremiseRole.Result        => "result"
      case _ =>
        throw IllegalStateException("a public passed-pawn result contains an unsupported premise role")

  private def passedPawnResultBranchRoleCode(role: CausalBranchRole): String =
    role match
      case PassedPawnProgressBranchRole.PlayedRootAnalysisContinuation(_, _) =>
        "played_root_analysis_continuation"
      case _ =>
        throw IllegalStateException("a public passed-pawn result contains an unsupported premise branch")

  private def snakeCode(value: String): String =
    value
      .replaceAll("([a-z0-9])([A-Z])", "$1_$2")
      .replace('-', '_')
      .toLowerCase

final case class LineReplayStep(
    ply: Int,
    moveUci: String,
    fenBefore: String,
    fenAfter: String
)

final case class LineObjectTrajectory private (
    rootStep: LineReplayStep,
    futureStep: LineReplayStep,
    interveningSteps: List[LineReplayStep],
    rootBeforeRole: EvidencePieceRole,
    pieceRole: EvidencePieceRole,
    futureAfterRole: EvidencePieceRole,
    color: Color,
    rootFrom: EvidenceSquare,
    rootTo: EvidenceSquare,
    futureFrom: EvidenceSquare,
    futureTo: EvidenceSquare,
    plyOffset: Int,
    private[chessjudgment] val persistenceStates: List[ClosedPositionStateAuthority]
):
  require(
    persistenceStates.map(_.step) == interveningSteps,
    "an object trajectory must retain one exact occupant state for every intervening occurrence"
  )

object LineObjectTrajectory:
  private[chessjudgment] def findAll(
      rootStep: LineReplayStep,
      continuation: List[LineReplayStep],
      replay: CanonicalLineReplay,
      ownerAt: LineReplayStep => Option[EvidenceRecord]
  ): List[LineObjectTrajectory] =
    val exactContinuation =
      replay.replaySteps.indexOf(rootStep) match
        case rootIndex if rootIndex >= 0 &&
            replay.replaySteps.slice(rootIndex + 1, rootIndex + 1 + continuation.size) == continuation =>
          continuation
        case _ => Nil
    replay.transition(rootStep).toList.flatMap { rootTransition =>
      rootTransition.boardFootprint.pieceTransitions.flatMap { rootMovement =>
        firstFutureMovement(rootMovement, exactContinuation, replay).flatMap { case (futureStep, futureMovement, offset) =>
          val intervening = exactContinuation.take(offset - 1)
          val tracked = RelationColoredPieceWitness(
            EvidenceSquare(rootMovement.to.key),
            EvidencePieceRole(rootMovement.afterRole.name),
            rootMovement.side
          )
          val persistence = intervening.flatMap(step =>
            ownerAt(step).flatMap(owner =>
              ClosedPositionStateAuthority.atStep(owner, step)((occurrence, scope) =>
                occurrence.existingOccupantState(tracked, scope)
              )
            )
          )
          Option.when(persistence.size == intervening.size)(
            LineObjectTrajectory(
              rootStep = rootStep,
              futureStep = futureStep,
              interveningSteps = intervening,
              rootBeforeRole = EvidencePieceRole(rootMovement.beforeRole.name),
              pieceRole = EvidencePieceRole(rootMovement.afterRole.name),
              futureAfterRole = EvidencePieceRole(futureMovement.afterRole.name),
              color = rootMovement.side,
              rootFrom = EvidenceSquare(rootMovement.from.key),
              rootTo = EvidenceSquare(rootMovement.to.key),
              futureFrom = EvidenceSquare(futureMovement.from.key),
              futureTo = EvidenceSquare(futureMovement.to.key),
              plyOffset = offset,
              persistenceStates = persistence
            )
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

/** One exact positive-state premise as used by the passed-pawn causal graph.
  * Semantic state meaning is shared across transpositions; LegalLine and
  * replay-position ids preserve the concrete occurrence path.
  */
private[chessjudgment] final case class PassedPawnResultPositionStateOccurrence private (
    semanticProofId: String,
    issuerEvidenceId: String,
    issuerOccurrenceId: String,
    query: PositionRelationExtractor.ClosedPositionStateQuery,
    step: LineReplayStep,
    issuerLine: LineNodeRef,
    scope: EvidenceScope
):
  require(semanticProofId.matches("[0-9a-f]{64}"), "a dependency state needs its semantic id")
  require(issuerEvidenceId.nonEmpty, "a dependency state needs its LegalLine owner")
  require(issuerOccurrenceId.matches("[0-9a-f]{64}"), "a dependency state needs its replay occurrence")
  def queryKey: String = query.stableKey

  def semanticKey: String = s"$semanticProofId|$queryKey"

  def stableKey: String =
    List(
      semanticKey,
      issuerEvidenceId,
      issuerOccurrenceId,
      BoundedCausalIdentity.stepKey(step),
      BoundedCausalIdentity.lineKey(issuerLine),
      scope.toString.toLowerCase
    ).mkString("|")

private[chessjudgment] object PassedPawnResultPositionStateOccurrence:
  def from(
      state: ClosedPositionStateAuthority
  ): PassedPawnResultPositionStateOccurrence =
    require(
      state.occurrence.step == state.step &&
        state.occurrence.certifies(state.proof, state.scope),
      "a dependency state must come from the issuer's exact closed replay position"
    )
    PassedPawnResultPositionStateOccurrence(
      semanticProofId = state.semanticProofId,
      issuerEvidenceId = state.issuerEvidenceId,
      issuerOccurrenceId = state.occurrence.occurrenceId,
      query = state.proof.query,
      step = state.step,
      issuerLine = state.issuerLine,
      scope = state.scope
    )

  def from(
      binding: CausalClosedStateBinding
  ): PassedPawnResultPositionStateOccurrence =
    val state = binding.authority
    require(
      binding.afterStepIndex >= 0 && state.occurrence.step == state.step &&
        state.occurrence.certifies(state.proof, state.scope),
      "a dependency state use must retain its exact closed replay position"
    )
    PassedPawnResultPositionStateOccurrence(
      semanticProofId = binding.semanticProofId,
      issuerEvidenceId = binding.issuerEvidenceId,
      issuerOccurrenceId = binding.issuerOccurrenceId,
      query = binding.query,
      step = state.step,
      issuerLine = state.issuerLine,
      scope = binding.scope
    )

private[chessjudgment] object LineAccessPersistenceStateAuthority:
  def atStep(
      step: LineReplayStep,
      owner: EvidenceRecord,
      side: Color,
      slider: RelationPieceWitness,
      direction: RelationRayDirection,
      destination: EvidenceSquare
  ): Option[ClosedPositionStateAuthority] =
    ClosedPositionStateAuthority.atStep(owner, step)((occurrence, scope) =>
      occurrence.existingSliderReachState(side, slider, direction, scope).filter { proof =>
        proof.query match
          case PositionRelationExtractor.ClosedPositionStateQuery.SliderReach(
                exactSide,
                exactSlider,
                exactDirection,
                Some(exactReach)
              ) if exactSide == side && exactSlider == slider && exactDirection == direction =>
            exactReach.segment.exists(control =>
              control.square == destination && LineAccessTrajectory.movementAvailable(control.target)
            )
          case _ => false
      }
    )

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
    private[chessjudgment] val relationOccurrenceAuthority: RecordBoundVerticalRelationOccurrence,
    private[chessjudgment] val persistenceStates: List[ClosedPositionStateAuthority]
):
  require(vacatedSquares.nonEmpty, "a line-access trajectory needs an exact vacated gate")
  require(
    relationOccurrenceAuthority.step == enablingStep &&
      relationOccurrenceAuthority.contract == VerticalRelationContractKind.SliderReachDelta &&
      relationOccurrenceAuthority.result.kind == RelationFactKind.SliderReachDelta,
    "a line-access trajectory needs its exact L1 slider-reach occurrence"
  )
  require(
    persistenceStates.map(_.step) == interveningSteps,
    "a line-access trajectory must retain one exact positive-state proof for every intervening occurrence"
  )
  def vacatedSquare: EvidenceSquare = vacatedSquares.head
  private[chessjudgment] def accessRelationKey: DerivedRelationResultKey =
    relationOccurrenceAuthority.result

object LineAccessTrajectory:
  /** Exact root-to-effect access used by causal episodes. Merely placing a
    * slider behind a blocker does not cause the blocker's later move or capture.
    */
  private[chessjudgment] def findAllRootClearancesBeforeUse(
      enablingStep: LineReplayStep,
      enabledStep: LineReplayStep,
      interveningSteps: List[LineReplayStep],
      replay: CanonicalLineReplay,
      ownerAt: LineReplayStep => Option[EvidenceRecord]
  ): List[LineAccessTrajectory] =
    val candidates = replay
      .verticalRelationOccurrences(
        enablingStep,
        List(VerticalRelationContractKind.SliderReachDelta)
      )
      .flatMap(occurrence =>
        fromChangedRootClearanceOccurrence(
          enablingStep,
          enabledStep,
          interveningSteps,
          replay,
          ownerAt,
          occurrence
        ).toList
      )

    require(
      candidates.size <= 1,
      "one enabled slider move cannot have multiple exact L1 clearance producers"
    )
    candidates

  /** Seed-specific trajectory closure for L2 demand dispatch. The caller owns
    * selection of the changed L1 occurrence, so this path never scans all root
    * reach results and never collapses independent occurrences to one.
    */
  private[chessjudgment] def fromChangedRootClearanceOccurrence(
      enablingStep: LineReplayStep,
      enabledStep: LineReplayStep,
      interveningSteps: List[LineReplayStep],
      replay: CanonicalLineReplay,
      ownerAt: LineReplayStep => Option[EvidenceRecord],
      occurrence: ReplayVerticalRelationOccurrence
  ): Option[LineAccessTrajectory] =
    for
      enablingIndex <- Some(replay.replaySteps.indexOf(enablingStep))
      enabledIndex <- Some(replay.replaySteps.indexOf(enabledStep))
      if enablingIndex >= 0 && enabledIndex > enablingIndex
      if replay.replaySteps.slice(enablingIndex + 1, enabledIndex) == interveningSteps
      if occurrence.step == enablingStep &&
        occurrence.contract == VerticalRelationContractKind.SliderReachDelta
      enablingTransition <- replay.transition(enablingStep)
      enabledLegal <- replay.legalStep(enabledStep)
      enabledPiece = RelationPieceWitness(
        EvidenceSquare(enabledLegal.move.orig.key),
        EvidencePieceRole(enabledLegal.move.piece.role.name)
      )
      relationAuthority <- ownerAt(enablingStep)
        .flatMap(RecordBoundVerticalRelationOccurrence.certified(_, occurrence))
      detail <- occurrence.relation.detail match
        case exact: RelationWitnessDetail.SliderReachDelta => Some(exact)
        case _                                             => None
      if detail.side == enabledLegal.move.piece.color && detail.mover.side == detail.side
      if detail.sliderBefore.contains(enabledPiece) && detail.sliderAfter.contains(enabledPiece)
      afterReach <- detail.after
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
      persistenceStates = interveningSteps.flatMap(step =>
        ownerAt(step).flatMap(owner =>
          LineAccessPersistenceStateAuthority.atStep(
            step,
            owner,
            detail.side,
            enabledPiece,
            detail.direction,
            destination
          )
        )
      )
      if persistenceStates.size == interveningSteps.size
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
      relationOccurrenceAuthority = relationAuthority,
      persistenceStates = persistenceStates
    )

  private[chessjudgment] def findRootClearanceBeforeUse(
      enablingStep: LineReplayStep,
      enabledStep: LineReplayStep,
      interveningSteps: List[LineReplayStep],
      replay: CanonicalLineReplay,
      ownerAt: LineReplayStep => Option[EvidenceRecord]
  ): Option[LineAccessTrajectory] =
    findAllRootClearancesBeforeUse(
      enablingStep,
      enabledStep,
      interveningSteps,
      replay,
      ownerAt
    ).headOption

  private[judgment] def movementAvailable(target: RelationControlTarget): Boolean =
    target match
      case RelationControlTarget.Friendly(_) => false
      case RelationControlTarget.Empty | RelationControlTarget.Enemy(_) => true

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
  private[chessjudgment] def relationOccurrenceAuthority: RecordBoundVerticalRelationOccurrence

private[chessjudgment] object PawnTopologyPersistenceStateAuthority:
  def atStep(
      step: LineReplayStep,
      owner: EvidenceRecord,
      side: Color,
      square: EvidenceSquare
  ): Option[ClosedPositionStateAuthority] =
    ClosedPositionStateAuthority.atStep(owner, step)((occurrence, scope) =>
      occurrence.existingPawnTopologyState(side, square, passed = true, scope)
    )

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
    private[chessjudgment] val relationOccurrenceAuthority: RecordBoundVerticalRelationOccurrence,
    plyOffset: Int,
    private[chessjudgment] val persistenceStates: List[ClosedPositionStateAuthority]
) extends CausalResponseContinuationTrajectory:
  require(
    relationOccurrenceAuthority.step == replyStep &&
      relationOccurrenceAuthority.contract == VerticalRelationContractKind.PawnTopologyTransition &&
      relationOccurrenceAuthority.result.kind == RelationFactKind.PawnTopologyTransition,
    "a released-passer continuation needs its exact L1 pawn-topology transition"
  )
  require(
    persistenceStates.map(_.step) == interveningSteps,
    "a released-passer continuation must retain one exact passed-pawn state for every intervening occurrence"
  )
  private[chessjudgment] def pawnTopologyTransitionKey: DerivedRelationResultKey =
    relationOccurrenceAuthority.result
  def triggerStep: LineReplayStep = breakStep
  def involvedRoles: List[EvidencePieceRole] = List(EvidencePieceRole(Pawn.toString))

object PawnBreakFollowUpTrajectory:
  private[chessjudgment] def find(
      breakStep: LineReplayStep,
      replyStep: LineReplayStep,
      followUpStep: LineReplayStep,
      interveningSteps: List[LineReplayStep],
      replay: CanonicalLineReplay,
      ownerAt: LineReplayStep => Option[EvidenceRecord]
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
      relationAuthority <- ownerAt(replyStep)
        .flatMap(RecordBoundVerticalRelationOccurrence.certified(_, occurrence))
        .toList
      detail <- occurrence.relation.detail match
        case exact: RelationWitnessDetail.PawnTopologyTransition => List(exact)
        case _ => Nil
      before <- detail.before.toList
      after <- detail.after.toList
      releasedPassedPawn = EvidenceSquare(followUpLegal.move.orig.key)
      if samePawn(before, breakLegal.move.piece.color, releasedPassedPawn) && !before.passed
      if samePawn(after, breakLegal.move.piece.color, releasedPassedPawn) && after.passed
      if detail.changedFacets.contains(RelationPawnTopologyFacet.Passed)
      persistenceStates = interveningSteps.flatMap(step =>
        for
          owner <- ownerAt(step)
          state <- PawnTopologyPersistenceStateAuthority.atStep(
            step,
            owner,
            breakLegal.move.piece.color,
            releasedPassedPawn
          )
        yield state
      )
      if persistenceStates.size == interveningSteps.size
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
      relationOccurrenceAuthority = relationAuthority,
      plyOffset = followUpStep.ply - breakStep.ply,
      persistenceStates = persistenceStates
    )

    candidates match
      case exact :: Nil => Some(exact)
      case _            => None

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
    private[chessjudgment] val relationOccurrenceAuthority: RecordBoundVerticalRelationOccurrence
) extends CausalResponseContinuationTrajectory:
  require(
    relationOccurrenceAuthority.step == replyStep &&
      relationOccurrenceAuthority.contract == VerticalRelationContractKind.CaptureRecaptureInventory &&
      relationOccurrenceAuthority.result.kind == RelationFactKind.CaptureRecaptureInventory,
    "a capture-response continuation needs its exact L1 recapture inventory occurrence"
  )
  private[chessjudgment] def recaptureInventoryKey: DerivedRelationResultKey =
    relationOccurrenceAuthority.result
  def involvedRoles: List[EvidencePieceRole] = List(triggerRole, responderRole, followUpRole).distinct

object CaptureResponseFollowUpTrajectory:
  private[chessjudgment] def find(
      triggerStep: LineReplayStep,
      replyStep: LineReplayStep,
      followUpStep: LineReplayStep,
      interveningSteps: List[LineReplayStep],
      replay: CanonicalLineReplay,
      ownerAt: LineReplayStep => Option[EvidenceRecord]
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
      relationAuthority <- ownerAt(replyStep)
        .flatMap(RecordBoundVerticalRelationOccurrence.certified(_, recaptureOccurrence))
        .toList
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
      relationOccurrenceAuthority = relationAuthority
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
  case ImmediateReplyCheck
  case Mate
  case DrawResource
  case MaterialGain
  case MaterialLoss
  case RecaptureSequence
  case RecoveryWindow
  case Promotion

object LineConsequenceKind:


  def tacticalDriver(kind: LineConsequenceKind): Boolean =
    kind match
      case LineConsequenceKind.MaterialGain | LineConsequenceKind.MaterialLoss |
          LineConsequenceKind.RecaptureSequence | LineConsequenceKind.RecoveryWindow |
          LineConsequenceKind.ImmediateReplyCheck | LineConsequenceKind.Mate |
          LineConsequenceKind.DrawResource | LineConsequenceKind.Promotion =>
        true

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
    eventOccurrence: Option[LineMoveOccurrence] = None,
    rootMove: Option[String] = None,
    rootSide: Option[Color] = None,
    beneficiary: Option[Color] = None
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
  def hasProofOccurrence(plyOffset: Int, moveUci: String): Boolean =
    proofOccurrences.exists(_.sameOccurrence(plyOffset, moveUci))

  def eventMatches(plyOffset: Int, moveUci: String): Boolean =
    eventOccurrence.exists(_.sameOccurrence(plyOffset, moveUci))

  def rootMoveMatched(rootMove: String): Boolean =
    this.rootMove.exists(move => EvidenceRef.sameMove(move, rootMove))

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

  def observedNetCpForMover: Int = runningBalancesForMover.lastOption.getOrElse(0)
  def observedMaxGainCpForMover: Int = (0 :: runningBalancesForMover).max
  def observedMaxLossCpForMover: Int = (0 :: runningBalancesForMover).min
  def closedNetCpForMover: Option[Int] = closedOutcome.map(_ => observedNetCpForMover)
  def isClosed: Boolean = closedOutcome.nonEmpty

  def capturesByMover: List[LineMaterialCapture] =
    captures.filter(_.side == sideToMove)

  def capturesByOpponent: List[LineMaterialCapture] =
    captures.filter(_.side != sideToMove)

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

private[chessjudgment] final case class LineReplayDerivedFacts(
    rootActorSurvivesReply: Option[Boolean],
    rootActorSurvivesLine: Option[Boolean],
    rootActor: Option[RootCausalActor],
    replayCertified: Boolean
)

private[chessjudgment] object LineReplayDerivedFacts:
  val empty: LineReplayDerivedFacts =
    LineReplayDerivedFacts(None, None, None, replayCertified = false)

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
      replayCertified = true
    )

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
  private lazy val verifiedCanonicalReplay: Option[CanonicalLineReplay] =
    canonicalReplay.filter(value => replayDerived.replayCertified && value.matches(replay))
  private[chessjudgment] def certifiedReplay: Option[CanonicalLineReplay] =
    verifiedCanonicalReplay
  private[chessjudgment] def replayIsCertified: Boolean = certifiedReplay.nonEmpty
  def materialCaptures: List[LineMaterialCapture] =
    material.flatMap(_.exactCaptures).getOrElse(Nil)
  def lineEvents: List[LineMoveEvent] =
    events
  def lineConsequences: List[LineConsequence] =
    consequences
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
  def semanticGroupingAnchors: List[EvidenceSemanticAnchor] =
    Option
      .when(hasLineEvent(LineEventKind.Castling))(
        EvidenceSemanticAnchor.of(EvidenceSemanticAnchorKind.LineEvent, LineEventKind.Castling.toString)
      )
      .toList ++
      consequences.map(_.kind).distinct.map(kind =>
        EvidenceSemanticAnchor.of(EvidenceSemanticAnchorKind.LineConsequence, kind.toString)
      )


object LineFactEvidence:
  private[chessjudgment] def fromCertifiedReplay(
      line: LineNodeRef,
      replay: CanonicalLineReplay,
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
    LineFactEvidence(
      line = line,
      material = material,
      replay = replay.replaySteps,
      events = events,
      consequences = consequences,
      replayDerived = derived,
      canonicalReplay = Some(replay),
      canonicalPredecessorReplay = predecessorReplay
    )

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
    PassedPawnProgressRealizedAfterOnlyLegalReplyProofKey.product(
      "transition-consequence",
      List(
        kind.toString.toLowerCase,
        PassedPawnProgressRealizedAfterOnlyLegalReplyProofKey.sequence(subjectBindings.map(_.stableKey).distinct.sorted),
        PassedPawnProgressRealizedAfterOnlyLegalReplyProofKey.sequence(targetBindings.map(_.stableKey).distinct.sorted),
        PassedPawnProgressRealizedAfterOnlyLegalReplyProofKey.sequence(resultPremiseKeys.map(_.stableKey))
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
    val exactEmptyInventory = consequences.isEmpty && bindings.isEmpty && changes.isEmpty
    val exactPositiveInventory = consequences.nonEmpty &&
      bindings.nonEmpty && bindings.forall(_.relationKeys.nonEmpty)
    (exactEmptyInventory || exactPositiveInventory) &&
      consequences.distinct.size == consequences.size &&
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

  /** An empty consequence list is a certified closed result, not missing
    * evidence. The structural producer emits one carrier for every admitted
    * transition so upper layers can distinguish exact absence from an
    * uncomputed inventory without replaying any board fact.
    */

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
    private[chessjudgment] val lineOccurrenceRecord: EvidenceRecord,
    private[chessjudgment] val structuralOccurrence: ReplayStructuralOccurrence,
    private[chessjudgment] val structuralTransition: StructuralTransitionBinding,
    private[chessjudgment] val canonicalStep: Option[LegalReplayStep] = None,
    private[chessjudgment] val canonicalMovement: Option[CanonicalRootLegalMove] = None
):
  private[chessjudgment] def lineOccurrenceOwner: EvidenceRef = lineOccurrenceRecord.ref
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
  require(
    ClosedPositionStateAuthority.occurrenceAfter(lineOccurrenceRecord, step).nonEmpty,
    "a passed-pawn event record must own the event's exact certified LegalLine replay occurrence"
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

  private[chessjudgment] def interveningSteps: List[LineReplayStep] =
    proof match
      case PassedPawnResultDependencyProof.ObjectState(trajectory) => trajectory.interveningSteps
      case PassedPawnResultDependencyProof.LineAccess(trajectory) => trajectory.interveningSteps
      case PassedPawnResultDependencyProof.ResponseContinuation(trajectory) => trajectory.interveningSteps

  private[chessjudgment] def relationOccurrenceBindings: List[ReplayVerticalRelationOccurrenceBinding] =
    proof match
      case PassedPawnResultDependencyProof.LineAccess(trajectory) =>
        List(trajectory.relationOccurrenceAuthority.binding)
      case PassedPawnResultDependencyProof.ResponseContinuation(trajectory) =>
        List(trajectory.relationOccurrenceAuthority.binding)
      case _ => Nil

  private[chessjudgment] def relationOccurrenceAuthorities: List[RecordBoundVerticalRelationOccurrence] =
    proof match
      case PassedPawnResultDependencyProof.LineAccess(trajectory) =>
        List(trajectory.relationOccurrenceAuthority)
      case PassedPawnResultDependencyProof.ResponseContinuation(trajectory) =>
        List(trajectory.relationOccurrenceAuthority)
      case _ => Nil

  private[chessjudgment] def positionStateAuthorities: List[ClosedPositionStateAuthority] =
    proof match
      case PassedPawnResultDependencyProof.ObjectState(trajectory) =>
        trajectory.persistenceStates
      case PassedPawnResultDependencyProof.LineAccess(trajectory) =>
        trajectory.persistenceStates
      case PassedPawnResultDependencyProof.ResponseContinuation(trajectory: PawnBreakFollowUpTrajectory) =>
        trajectory.persistenceStates
      case _ => Nil

  private[chessjudgment] def positionStateOccurrences: List[PassedPawnResultPositionStateOccurrence] =
    positionStateAuthorities.map(PassedPawnResultPositionStateOccurrence.from)

  private[chessjudgment] def lowerIssuerRecords: List[EvidenceRecord] =
    relationOccurrenceAuthorities.map(_.issuerRecord) ++ positionStateAuthorities.map(_.issuerRecord)

  private def lowerAuthoritiesOwnRoute: Boolean =
    to.lineOccurrenceOwner.line.exists { consumingLine =>
      val consumingScope = to.lineOccurrenceOwner.scope
      val requiredStateSteps = proof match
        case PassedPawnResultDependencyProof.ObjectState(_) | PassedPawnResultDependencyProof.LineAccess(_) =>
          interveningSteps
        case PassedPawnResultDependencyProof.ResponseContinuation(_: PawnBreakFollowUpTrajectory) =>
          interveningSteps
        case _ => Nil
      val authoritySteps =
        (positionStateAuthorities.map(_.step) ++ relationOccurrenceAuthorities.map(_.step)).toSet
      consumingScope == consumingLine.role.scope &&
      positionStateAuthorities.map(_.step) == requiredStateSteps &&
      interveningSteps.forall(authoritySteps) &&
      positionStateAuthorities.forall(authority =>
        authority.issuerLine == consumingLine && authority.scope == consumingScope
      ) && relationOccurrenceAuthorities.forall(authority =>
        (authority.step == from.step || interveningSteps.contains(authority.step)) &&
          authority.issuerLine == consumingLine && authority.scope == consumingScope
      )
    }

  def causalConnectionProven: Boolean =
    from.certifiedLegalStep.nonEmpty &&
      to.certifiedLegalStep.nonEmpty &&
      from.step.ply < to.step.ply &&
      plyOffset == to.step.ply - from.step.ply &&
      lowerAuthoritiesOwnRoute &&
      ((kind, proof) match
        case (PassedPawnResultDependencyKind.ObjectStatePrecondition, PassedPawnResultDependencyProof.ObjectState(trajectory)) =>
          trajectory.rootStep == from.step &&
            trajectory.futureStep == to.step &&
            trajectory.plyOffset == plyOffset
        case (PassedPawnResultDependencyKind.LineAccessPrecondition, PassedPawnResultDependencyProof.LineAccess(trajectory)) =>
          trajectory.enablingStep == from.step &&
            trajectory.enabledStep == to.step &&
            from.lineOccurrenceOwner == to.lineOccurrenceOwner &&
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
    PassedPawnProgressRealizedAfterOnlyLegalReplyProofKey.product(
      "passed-pawn-result-function",
      List(
        resultKind.id,
        mechanism.toString.toLowerCase,
        PassedPawnProgressRealizedAfterOnlyLegalReplyProofKey.optional(supportingDependency.map(_.stableKey))
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
    PassedPawnProgressRealizedAfterOnlyLegalReplyProofKey.product(
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
        PassedPawnProgressRealizedAfterOnlyLegalReplyProofKey.optional(supportingDependency.map(_.stableKey))
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
    PassedPawnProgressRealizedAfterOnlyLegalReplyProofKey.product(
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
        PassedPawnProgressRealizedAfterOnlyLegalReplyProofKey.sequence(causalPath.map(_.stableKey)),
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

private object PassedPawnProgressRealizedAfterOnlyLegalReplyProofKey:
  def product(kind: String, values: Iterable[String]): String =
    sequence(kind :: values.toList)

  def sequence(values: Iterable[String]): String =
    values.iterator.map(value => s"${value.length}:$value").mkString

  def optional(value: Option[String]): String =
    value match
      case Some(exact) => product("some", List(exact))
      case None        => product("none", Nil)

/** Chess function proved by one exact causal dependency. Replay steps and
  * FENs remain on the occurrence proof; this projection is used only for the
  * semantic identity of the graph-owned dependency.
  */
sealed trait CausalDependencyFunctionProof:
  def kind: String
  def proofSquares: List[EvidenceSquare]
  def proofPieceRoles: List[EvidencePieceRole]
  protected def identityParts: List[String]
  final def stableKey: String =
    PassedPawnProgressRealizedAfterOnlyLegalReplyProofKey.product(kind, identityParts)

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
      PassedPawnProgressRealizedAfterOnlyLegalReplyProofKey.sequence(vacatedSquares.map(_.key.toLowerCase)),
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

/** Exact lower-proof witness retained by dependency premises. It projects
  * only the certified dependency and its replay-owned L1 occurrences; it has
  * no board, move-generation, or relation-extraction capability.
  */
private[chessjudgment] final case class PassedPawnResultDependencyPremiseWitness private (
    dependencyOccurrenceKey: String,
    dependencyKind: PassedPawnResultDependencyKind,
    functionProof: CausalDependencyFunctionProof,
    fromSide: Color,
    toSide: Color,
    relationOccurrences: List[RecordBoundVerticalRelationOccurrence],
    positionStateOccurrences: List[PassedPawnResultPositionStateOccurrence]
):
  require(dependencyOccurrenceKey.nonEmpty, "a dependency witness needs its exact occurrence identity")
  require(fromSide == toSide, "a passed-pawn dependency must retain one exact acting side")
  require(
    relationOccurrences == relationOccurrences.sortBy(_.stableKey),
    "dependency relation issuers must retain canonical occurrence order"
  )
  require(
    (dependencyKind, functionProof, relationOccurrences, positionStateOccurrences) match
      case (
            PassedPawnResultDependencyKind.ObjectStatePrecondition,
            proof: CausalDependencyFunctionProof.ObjectState,
            Nil,
            states
          ) =>
        proof.color == fromSide && states.forall(objectStateMatches(_, proof))
      case (
            PassedPawnResultDependencyKind.LineAccessPrecondition,
            proof: CausalDependencyFunctionProof.LineAccess,
            List(issuer),
            states
          ) =>
          proof.color == fromSide &&
          issuer.contract == VerticalRelationContractKind.SliderReachDelta &&
          issuer.result.kind == RelationFactKind.SliderReachDelta &&
          issuer.result.stableKey == proof.accessRelationKey &&
          states.forall(lineAccessStateMatches(_, proof))
      case (
            PassedPawnResultDependencyKind.ResponseContinuationPrecondition,
            proof: CausalDependencyFunctionProof.PawnBreakFollowUp,
            List(issuer),
            states
          ) =>
        proof.color == fromSide &&
          issuer.contract == VerticalRelationContractKind.PawnTopologyTransition &&
          issuer.result.kind == RelationFactKind.PawnTopologyTransition &&
          issuer.result.stableKey == proof.pawnTopologyTransitionKey &&
          states.forall(pawnTopologyStateMatches(_, proof))
      case (
            PassedPawnResultDependencyKind.ResponseContinuationPrecondition,
            proof: CausalDependencyFunctionProof.CaptureFollowUp,
            List(issuer),
            Nil
          ) =>
        issuer.contract == VerticalRelationContractKind.CaptureRecaptureInventory &&
          issuer.result.kind == RelationFactKind.CaptureRecaptureInventory &&
          issuer.result.stableKey == proof.recaptureInventoryKey
      case _ => false,
    "a dependency witness needs the exact proof-specific L1 issuer inventory"
  )

  private[chessjudgment] def requiredSourcePremiseIds: List[String] =
    (
      relationOccurrences.flatMap(issuer =>
        issuer.issuerEvidenceId :: issuer.occurrenceId :: issuer.certifiedSourcePremiseIds
      ) ++
        positionStateOccurrences.flatMap(state =>
          List(state.issuerEvidenceId, state.issuerOccurrenceId)
        )
    ).distinct.sorted

  private def objectStateMatches(
      state: PassedPawnResultPositionStateOccurrence,
      proof: CausalDependencyFunctionProof.ObjectState
  ): Boolean =
    state.query match
      case PositionRelationExtractor.ClosedPositionStateQuery.OccupiedBy(piece) =>
        piece == RelationColoredPieceWitness(proof.rootTo, proof.pieceRole, proof.color)
      case _ => false

  private def lineAccessStateMatches(
      state: PassedPawnResultPositionStateOccurrence,
      proof: CausalDependencyFunctionProof.LineAccess
  ): Boolean =
    state.query match
      case PositionRelationExtractor.ClosedPositionStateQuery.SliderReach(
            side,
            slider,
            _,
            Some(reach)
          ) =>
        side == proof.color &&
          slider == RelationPieceWitness(proof.enabledFrom, proof.enabledPieceRole) &&
          reach.segment.exists(control =>
            control.square == proof.enabledTo && LineAccessTrajectory.movementAvailable(control.target)
          )
      case _ => false

  private def pawnTopologyStateMatches(
      state: PassedPawnResultPositionStateOccurrence,
      proof: CausalDependencyFunctionProof.PawnBreakFollowUp
  ): Boolean =
    state.query match
      case PositionRelationExtractor.ClosedPositionStateQuery.PawnTopology(pawn) =>
        pawn.side == proof.color && pawn.square == proof.releasedPassedPawn && pawn.passed
      case _ => false

  private[chessjudgment] def publicProof: PassedPawnProgressPublicDependencyProof =
    val (proofKind, squares, pieces) = functionProof match
      case proof: CausalDependencyFunctionProof.ObjectState =>
        (
          "object_state",
          List(
            square("root_from", proof.rootFrom),
            square("root_to", proof.rootTo),
            square("future_from", proof.futureFrom),
            square("future_to", proof.futureTo)
          ),
          List(
            piece("root_before", proof.color, proof.rootBeforeRole),
            piece("tracked", proof.color, proof.pieceRole),
            piece("future_after", proof.color, proof.futureAfterRole)
          )
        )
      case proof: CausalDependencyFunctionProof.LineAccess =>
        (
          "line_access",
          proof.vacatedSquares.map(square("vacated_gate", _)) ++
            List(square("enabled_from", proof.enabledFrom), square("enabled_to", proof.enabledTo)),
          List(piece("enabled_piece", proof.color, proof.enabledPieceRole))
        )
      case proof: CausalDependencyFunctionProof.PawnBreakFollowUp =>
        (
          "pawn_break_follow_up",
          List(
            square("reply_from", proof.replyFrom),
            square("reply_to", proof.replyTo),
            square("follow_up_from", proof.followUpFrom),
            square("follow_up_to", proof.followUpTo),
            square("released_passed_pawn", proof.releasedPassedPawn)
          ),
          List(
            piece("trigger_pawn", proof.color, EvidencePieceRole(Pawn.name)),
            piece("responder_pawn", !proof.color, EvidencePieceRole(Pawn.name)),
            piece("follow_up_pawn", proof.color, EvidencePieceRole(Pawn.name))
          )
        )
      case proof: CausalDependencyFunctionProof.CaptureFollowUp =>
        (
          "capture_follow_up",
          List(
            square("reply_from", proof.replyFrom),
            square("reply_to", proof.replyTo),
            square("follow_up_from", proof.followUpFrom),
            square("follow_up_to", proof.followUpTo)
          ),
          List(
            piece("trigger_piece", fromSide, proof.triggerRole),
            piece("responder_piece", !fromSide, proof.responderRole),
            piece("follow_up_piece", toSide, proof.followUpRole)
          )
        )
    PassedPawnProgressPublicDependencyProof(
      dependencyKind = dependencyKind match
        case PassedPawnResultDependencyKind.ObjectStatePrecondition => "object_state_precondition"
        case PassedPawnResultDependencyKind.LineAccessPrecondition  => "line_access_precondition"
        case PassedPawnResultDependencyKind.ResponseContinuationPrecondition =>
          "response_continuation_precondition",
      proofKind = proofKind,
      squares = squares,
      pieces = pieces,
      relationIssuers = relationOccurrences.map(issuer =>
        PassedPawnProgressPublicRelationIssuer(
          contract = VerticalRelationContractKind.id(issuer.contract),
          relationKind = RelationFactKind.id(issuer.result.kind),
          resultKey = issuer.result.stableKey,
          occurrenceId = issuer.occurrenceId,
          stepKey = BoundedCausalIdentity.stepKey(issuer.step),
          sourcePremiseIds = issuer.certifiedSourcePremiseIds,
          issuerEvidenceId = issuer.issuerEvidenceId,
          line = issuer.issuerLine,
          scope = issuer.scope
        )
      ),
      positionStateIssuers = positionStateOccurrences.map(state =>
        PassedPawnProgressPublicPositionStateIssuer(
          state = publicPositionState(state.query),
          semanticProofId = state.semanticProofId,
          issuerEvidenceId = state.issuerEvidenceId,
          issuerOccurrenceId = state.issuerOccurrenceId,
          stepKey = BoundedCausalIdentity.stepKey(state.step),
          ply = state.step.ply,
          moveUci = EvidenceRef.normalizeMove(state.step.moveUci),
          fenBefore = state.step.fenBefore,
          fenAfter = state.step.fenAfter,
          line = state.issuerLine,
          scope = state.scope
        )
      )
    )

  private def publicPositionState(
      query: PositionRelationExtractor.ClosedPositionStateQuery
  ): PassedPawnProgressPublicPositionStateWitness =
    import PassedPawnProgressPublicPositionStateWitness.*
    query match
      case PositionRelationExtractor.ClosedPositionStateQuery.OccupiedBy(piece) =>
        OccupiedBy(piece.side.toString.toLowerCase, piece.square.key.toLowerCase, piece.role.name.toLowerCase)
      case PositionRelationExtractor.ClosedPositionStateQuery.SliderReach(side, slider, direction, Some(reach)) =>
        SliderReach(
          side.toString.toLowerCase,
          slider.square.key.toLowerCase,
          slider.role.name.toLowerCase,
          direction.fileStep,
          direction.rankStep,
          reach.segment.map(control =>
            control.target match
              case RelationControlTarget.Empty =>
                SliderReachSegment(control.square.key.toLowerCase, "empty", None)
              case RelationControlTarget.Friendly(role) =>
                SliderReachSegment(control.square.key.toLowerCase, "friendly", Some(role.name.toLowerCase))
              case RelationControlTarget.Enemy(role) =>
                SliderReachSegment(control.square.key.toLowerCase, "enemy", Some(role.name.toLowerCase))
          )
        )
      case PositionRelationExtractor.ClosedPositionStateQuery.PawnTopology(pawn) =>
        PawnTopology(pawn.side.toString.toLowerCase, pawn.square.key.toLowerCase, pawn.passed)
      case _ =>
        throw IllegalStateException("a passed-pawn dependency exposed an unsupported closed position state")

  private def square(role: String, value: EvidenceSquare): PassedPawnProgressPublicSquareWitness =
    PassedPawnProgressPublicSquareWitness(role, value.key.toLowerCase)

  private def piece(
      role: String,
      side: Color,
      value: EvidencePieceRole
  ): PassedPawnProgressPublicPieceWitness =
    PassedPawnProgressPublicPieceWitness(role, side.toString.toLowerCase, value.name.toLowerCase)

private[chessjudgment] object PassedPawnResultDependencyPremiseWitness:
  def from(
      dependency: PassedPawnResultDependency,
      stateBindings: List[CausalClosedStateBinding]
  ): PassedPawnResultDependencyPremiseWitness =
    require(dependency.causalConnectionProven, "a dependency premise witness requires an exact proven occurrence")
    val retainedSteps = dependency.proof match
      case PassedPawnResultDependencyProof.LineAccess(trajectory) =>
        dependency.from.step :: (trajectory.interveningSteps :+ dependency.to.step)
      case PassedPawnResultDependencyProof.ResponseContinuation(trajectory) =>
        dependency.from.step :: (trajectory.interveningSteps :+ dependency.to.step)
      case _ => List(dependency.from.step, dependency.to.step)
    require(
      dependency.relationOccurrenceAuthorities.forall(authority => retainedSteps.contains(authority.step)),
      "a dependency premise witness must retain every exact L1 occurrence on its certified route"
    )
    val expectedStateSteps = dependency.proof match
      case PassedPawnResultDependencyProof.ObjectState(trajectory) => trajectory.interveningSteps
      case PassedPawnResultDependencyProof.LineAccess(trajectory) => trajectory.interveningSteps
      case PassedPawnResultDependencyProof.ResponseContinuation(trajectory: PawnBreakFollowUpTrajectory) =>
        trajectory.interveningSteps
      case _                                                       => Nil
    require(
      stateBindings.map(_.authority) == dependency.positionStateAuthorities,
      "a dependency premise witness must use its exact manifest-bound state authorities"
    )
    val positionStateOccurrences = stateBindings.map(PassedPawnResultPositionStateOccurrence.from)
    require(
      positionStateOccurrences.map(_.step) == expectedStateSteps,
      "a stateful dependency must retain every ordered intervening position-state occurrence"
    )
    PassedPawnResultDependencyPremiseWitness(
      dependencyOccurrenceKey = dependency.stableKey,
      dependencyKind = dependency.kind,
      functionProof = CausalDependencyFunctionProof.from(dependency),
      fromSide = dependency.from.perspective,
      toSide = dependency.to.perspective,
      relationOccurrences = dependency.relationOccurrenceAuthorities.sortBy(_.stableKey),
      positionStateOccurrences = positionStateOccurrences
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
    PassedPawnProgressRealizedAfterOnlyLegalReplyProofKey.product(
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
    PassedPawnProgressRealizedAfterOnlyLegalReplyProofKey.product(
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
    positionStateOccurrences: List[PassedPawnResultPositionStateOccurrence],
    plyOffset: Int
):
  def stableKey: String =
    PassedPawnProgressRealizedAfterOnlyLegalReplyProofKey.product(
      "causal-dependency-occurrence",
      List(
        from.stableKey,
        to.stableKey,
        dependencyKind.toString.toLowerCase,
        proof.stableKey,
        PassedPawnProgressRealizedAfterOnlyLegalReplyProofKey.sequence(interveningSteps.map(_.stableKey)),
        PassedPawnProgressRealizedAfterOnlyLegalReplyProofKey.sequence(relationOccurrences.map(_.stableKey)),
        PassedPawnProgressRealizedAfterOnlyLegalReplyProofKey.sequence(positionStateOccurrences.map(_.stableKey)),
        plyOffset.toString
      )
    )

object PassedPawnResultDependencyOccurrenceIdentity:
  def from(dependency: PassedPawnResultDependency): PassedPawnResultDependencyOccurrenceIdentity =
    require(dependency.causalConnectionProven, "causal dependency occurrence requires an exact proven connection")
    val intervening = dependency.proof match
      case PassedPawnResultDependencyProof.ObjectState(trajectory) => trajectory.interveningSteps
      case PassedPawnResultDependencyProof.LineAccess(trajectory) => trajectory.interveningSteps
      case PassedPawnResultDependencyProof.ResponseContinuation(trajectory) => trajectory.interveningSteps
    PassedPawnResultDependencyOccurrenceIdentity(
      from = eventOccurrence(dependency.from),
      to = eventOccurrence(dependency.to),
      dependencyKind = dependency.kind,
      proof = CausalDependencyFunctionProof.from(dependency),
      interveningSteps = intervening.map(PassedPawnResultLineStepOccurrence.from),
      relationOccurrences = dependency.relationOccurrenceBindings.sortBy(_.stableKey),
      positionStateOccurrences = dependency.positionStateOccurrences,
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

final case class PassedPawnResultSourceOccurrence private[chessjudgment] (
    moveUci: String,
    plyOffset: Int,
    actor: PassedPawnResultActorOccurrence
):
  def stableKey: String =
    List(EvidenceRef.normalizeMove(moveUci), plyOffset.toString, actor.stableKey).mkString("@")

/** passed-pawn result taxonomy is annotation. This is the exact result occurrence and
  * function that may be shared by multiple exact dependency paths. Branch
  * occurrence identity remains on the bounded proof instead of being folded
  * into this transposition-safe semantic key.
  */
final case class PassedPawnProgressSemanticIdentity(
    root: PassedPawnResultSourceOccurrence,
    source: PassedPawnResultSourceOccurrence,
    consequenceKind: TransitionConsequenceKind,
    resultTargetSubjects: List[String],
    resultFunction: PassedPawnResultFunctionIdentity
):
  def stableKey: String =
    (List(root.stableKey, source.stableKey) ++ List(
      consequenceKind.toString.toLowerCase,
      resultTargetSubjects.mkString("[", ",", "]"),
      resultFunction.stableKey
    )).mkString("|")

object PassedPawnProgressSemanticIdentity:
  def from(
      event: PassedPawnResultEventEvidence,
      route: PassedPawnResultRoute
  ): PassedPawnProgressSemanticIdentity =
    require(
      event.causalEpisode.resultRoutes.contains(route),
      "a passed-pawn semantic identity needs an exact route owned by its event"
    )
    val sourcePlyOffset = route.sourceEvent.step.ply - event.causalEpisode.root.step.ply
    PassedPawnProgressSemanticIdentity(
      root = PassedPawnResultSourceOccurrence(
        EvidenceRef.normalizeMove(event.causalEpisode.root.moveUci),
        0,
        event.causalEpisode.root.identity.actor
      ),
      source = PassedPawnResultSourceOccurrence(
        EvidenceRef.normalizeMove(route.sourceEvent.moveUci),
        sourcePlyOffset,
        route.sourceEvent.identity.actor
      ),
      consequenceKind = route.consequence.kind,
      resultTargetSubjects = normalizedResultTargetSubjects(
        route.consequence.resultSubjectBindings.map(_.stableKey)
      ),
      resultFunction = route.resultProof.functionIdentity(event.causalEpisode.root)
    )

  private def normalizedResultTargetSubjects(subjects: List[String]): List[String] =
    subjects.map(normalize).filter(_.nonEmpty).distinct.sorted

  private def normalize(value: String): String =
    Option(value).getOrElse("").trim.toLowerCase

final case class PassedPawnResultEventEvidence(
    rootTransition: StructuralTransitionBinding,
    causalEpisode: PassedPawnResultEpisode,
    directResultProofs: List[PassedPawnResultTransitionProof],
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
  require(
    causalEpisode.causalEpisodeProven && causalEpisode.resultRoutes.nonEmpty,
    "a passed-pawn result event needs a proven causal episode"
  )

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
  def exactResultRoutes: List[PassedPawnResultRoute] =
    causalEpisode.resultRoutes.sortBy(_.stableKey)
  def rootEnablingDependencies: List[PassedPawnResultDependency] =
    causalEpisode.dependencies.filter(dependency => dependency.from == causalEpisode.root && dependency.enablesContinuation)
  def observedRootEnablesContinuation: Boolean = causalEpisode.rootEnablesContinuation
  private lazy val certifiedOnlyReplyStep: Option[LineReplayStep] =
    exactResultRoutes.flatMap(analysisContinuationSteps(_).lift(1)).distinct match
      case exact :: Nil => Some(exact)
      case _            => None

  def onlyLegalReplyMove: Option[String] =
    certifiedOnlyReplyStep.map(step => EvidenceRef.normalizeMove(step.moveUci))

  /** Every exact result authorized for an affirmative public passed-pawn-result Cause.
    * Sibling results retain independent semantic identity, while multiple
    * dependency paths to the same occurrence remain available to one L2 proof.
    */
  def exactOnlyReplyResultRoutes: List[PassedPawnResultRoute] =
    Option.when(onlyLegalReplyRealized)(exactResultRoutes).toList.flatten

  def onlyLegalReplyClosed: Boolean =
    canonicalRootTransitionProof
      .filter(_.proves(rootTransition))
      .exists(proof =>
        proof.legalResponseCount == 1 &&
          certifiedOnlyReplyStep.exists(step => proof.certifiesCompleteLegalResponseSet(List(step)))
      )
  def onlyLegalReplyRealized: Boolean =
    onlyLegalReplyClosed && exactResultRoutes.nonEmpty
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

  private[chessjudgment] def lineOccurrenceRecords: List[EvidenceRecord] =
    (
      causalEpisode.resultSteps.map(_.lineOccurrenceRecord) ++
        causalEpisode.dependencies.flatMap(_.lowerIssuerRecords)
    ).sortBy(_.ref.id)

  private[chessjudgment] def lineOccurrenceOwners: List[EvidenceRef] =
    lineOccurrenceRecords.map(_.ref).distinctBy(_.id)

  private[chessjudgment] def retainsLineOccurrenceOwners(record: EvidenceRecord): Boolean =
    record.payload == this && lineOccurrenceOwners.nonEmpty &&
      lineOccurrenceOwners.forall(owner => record.parents.contains(owner))

  private def analysisContinuationSteps(route: PassedPawnResultRoute): List[LineReplayStep] =
    causalEpisode.root.step :: route.causalPath.flatMap(dependency =>
      dependency.interveningSteps :+ dependency.to.step
    )

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
        List(payload.rootLine)
      case payload: PassedPawnProgressRealizedAfterOnlyLegalReplyProofEvidence =>
        List(payload.rootLine)
      case payload: UniqueCheckReplyDefenderDisplacementBeforeCaptureEvidence =>
        List(payload.occurrence.referenceLine, payload.occurrence.playedLine)
      case payload: SoleRecapturerRemovalBeforeTargetCaptureEvidence =>
        List(payload.occurrence.referenceLine, payload.occurrence.playedLine)
      case payload: VacatedGateEnablesUnrecapturableSliderCaptureEvidence =>
        List(payload.occurrence.referenceLine, payload.occurrence.playedLine)
      case payload: SquareReleaseRouteEvidence =>
        List(payload.occurrence.referenceLine, payload.occurrence.playedLine)
      case CandidateLineEvaluationEvidence(payloadLine, _) =>
        List(payloadLine)
      case CandidateComparisonEvidence(fact) =>
        List(fact.referenceLine, fact.candidateLine)
      case RelativeAssessmentEvidence(assessment) =>
        List(assessment.reference.ref, assessment.candidate.ref)
      case RelativeCauseFactEvidence(_) =>
        ref.line.toList
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

  def record(ref: EvidenceRef): Option[EvidenceRecord] =
    byId.get(ref.id).filter(_.ref == ref)

  def recordsFor(position: PositionNodeRef): List[EvidenceRecord] =
    recordsByPosition.getOrElse(position, Nil)

  def recordsFor(line: LineNodeRef): List[EvidenceRecord] =
    recordsByLine.getOrElse(line, Nil)

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

  /** Graph-owned, demand-driven proof-eligibility memo.
    *
    * One consumer query resolves only that record and its exact parent
    * ancestry. Unrelated graph records remain untouched until a consumer asks
    * for them. The graph is immutable, so a record id completely identifies a
    * cache entry for the lifetime of this graph instance; `addAll` creates a
    * new graph with a fresh memo.
    */
  private val proofEligibilityById = scala.collection.mutable.Map.empty[String, Boolean]
  private val proofEligibilityLock = new Object

  private def resolveProofEligibility(
      record: EvidenceRecord,
      visiting: Set[String]
  ): Boolean =
    proofEligibilityById.get(record.ref.id) match
      case Some(eligible) => eligible
      case None =>
        val eligible =
          !visiting(record.ref.id) &&
            intrinsicallyProofEligible(record) &&
            record.parents.forall(parent =>
              byId.get(parent.id).exists(parentRecord =>
                parentRecord.ref == parent &&
                  resolveProofEligibility(parentRecord, visiting + record.ref.id)
              )
            )
        proofEligibilityById.update(record.ref.id, eligible)
        eligible

  def proofEligible(record: EvidenceRecord): Boolean =
    byId.get(record.ref.id).contains(record) &&
      proofEligibilityLock.synchronized {
        resolveProofEligibility(record, Set.empty)
      }

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
        case payload: UniqueCheckReplyDefenderDisplacementBeforeCaptureEvidence =>
          exactAuthority(record, EvidenceProducer.CausalProofProducer, EvidenceLayer.CausalProof) &&
            payload.exactOccurrenceCertified(record) && payload.lowerRecordsAreCanonical(byId)
        case payload: SoleRecapturerRemovalBeforeTargetCaptureEvidence =>
          exactAuthority(record, EvidenceProducer.CausalProofProducer, EvidenceLayer.CausalProof) &&
            payload.exactOccurrenceCertified(record) && payload.lowerRecordsAreCanonical(byId)
        case payload: VacatedGateEnablesUnrecapturableSliderCaptureEvidence =>
          exactAuthority(record, EvidenceProducer.CausalProofProducer, EvidenceLayer.CausalProof) &&
            payload.exactOccurrenceCertified(record) && payload.lowerRecordsAreCanonical(byId)
        case payload: SquareReleaseRouteEvidence =>
          exactAuthority(record, EvidenceProducer.CausalProofProducer, EvidenceLayer.CausalProof) &&
            payload.exactOccurrenceCertified(record) && payload.lowerRecordsAreCanonical(byId)
        case payload: PassedPawnProgressRealizedAfterOnlyLegalReplyProofEvidence =>
          exactAuthority(record, EvidenceProducer.CausalProofProducer, EvidenceLayer.CausalProof) &&
            record.ref.confidence == EvidenceConfidence.LegalReplayVerified &&
            payload.exactOccurrenceCertified(record) && payload.lowerRecordsAreCanonical(byId)
        case payload: RelationFactEvidence =>
          relationOccurrenceAncestryVerified(record, payload)
        case payload: ClosedRelationOccurrenceEvidence =>
          closedRelationOccurrenceAncestryVerified(record, payload)
        case payload: PassedPawnResultEventEvidence =>
          exactAuthority(record, EvidenceProducer.PassedPawnResultEventProducer, EvidenceLayer.PassedPawnResultEvent) &&
            payload.rootTransitionIsCertified && record.ref.position == payload.rootTransition.from &&
            record.ref.line.contains(payload.rootLine) && record.ref.scope == payload.rootTransition.role.scope &&
            payload.retainsLineOccurrenceOwners(record) && payload.lineOccurrenceRecords.forall(owner =>
              byId.get(owner.ref.id).contains(owner)
            )
        case CandidateComparisonEvidence(comparison) =>
          candidateComparisonAncestryVerified(record, comparison)
        case RelativeAssessmentEvidence(assessment) =>
          exactAuthority(record, EvidenceProducer.RelativeMoveProducer, EvidenceLayer.RelativeAssessment) &&
            record.ref == assessment.evidence && record.ref.line.contains(assessment.candidate.ref)
        case RelativeCauseFactEvidence(cause) =>
          exactAuthority(record, EvidenceProducer.RelativeMoveProducer, EvidenceLayer.RelativeCause) &&
            record.parents.map(_.id).distinct.size == record.parents.size &&
            record.parents.toSet == (cause.comparisonEvidence :: cause.proofSources).toSet &&
            record.parents.size == cause.proofSources.size + 1
      )

  private def exactAuthority(
      record: EvidenceRecord,
      producer: EvidenceProducer,
      layer: EvidenceLayer
  ): Boolean =
    record.ref.producer == producer && record.ref.layer == layer

  private def candidateComparisonAncestryVerified(
      record: EvidenceRecord,
      comparison: CandidateComparisonFact
  ): Boolean =
    val directParents = record.parents.flatMap(parent => byId.get(parent.id).filter(_.ref == parent))
    def exactLineSource(line: LineNodeRef): Option[EvidenceRecord] =
      directParents.collect {
        case source @ EvidenceRecord(ref, payload: LineFactEvidence, _)
            if ref.position == record.ref.position && ref.line.contains(line) &&
              ref.scope == line.role.scope && payload.line == line =>
          source
      } match
        case source :: Nil => Some(source)
        case _             => None
    def exactEvaluation(
        line: LineNodeRef,
        lineSource: EvidenceRecord
    ): Option[lila.chessjudgment.model.line.CandidateLineEvaluation] =
      directParents.collect {
        case EvidenceRecord(
              ref,
              CandidateLineEvaluationEvidence(payloadLine, evaluation),
              parents
            )
            if ref.position == record.ref.position && ref.line.contains(line) &&
              ref.scope == line.role.scope && payloadLine == line &&
              parents == List(lineSource.ref) =>
          evaluation
      } match
        case evaluation :: Nil => Some(evaluation)
        case _                 => None
    val canonicalInputs = for
      referenceSource <- exactLineSource(comparison.referenceLine)
      candidateSource <- exactLineSource(comparison.candidateLine)
      referenceEvaluation <- exactEvaluation(comparison.referenceLine, referenceSource)
      candidateEvaluation <- exactEvaluation(comparison.candidateLine, candidateSource)
    yield (
      referenceSource,
      candidateSource,
      CandidateLineNode(comparison.referenceLine, referenceEvaluation, referenceSource.ref),
      CandidateLineNode(comparison.candidateLine, candidateEvaluation, candidateSource.ref)
    )
    directParents.size == record.parents.size && canonicalInputs.exists {
      case (referenceSource, candidateSource, reference, candidate) =>
        CandidateComparisonRecordAuthority.exactShape(
          record,
          comparison,
          record.ref.position,
          referenceSource,
          candidateSource
        ) && CandidateComparisonFact
          .fromLines(
            comparison.kind,
            comparison.comparison.mover,
            reference,
            candidate
          )
          .contains(comparison)
    }

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
  val empty: TypedEvidenceGraph =
    val records = scala.collection.immutable.VectorMap.empty[String, EvidenceRecord]
    new TypedEvidenceGraph(records, CanonicalRelationGraph.empty)
