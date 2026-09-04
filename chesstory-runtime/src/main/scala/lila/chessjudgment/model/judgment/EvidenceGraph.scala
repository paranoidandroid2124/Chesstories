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
  BoardMoveEffect,
  BoardPieceTransition,
  BoardTransitionFootprint,
  PositionOccurrenceState
}

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
  case Relation
  case LineEvent
  case LineConsequence

final case class EvidenceSemanticAnchor(
    kind: EvidenceSemanticAnchorKind,
    values: List[String]
):
  def stableKey: String =
    (kind.toString :: values).mkString(":")

object EvidenceSemanticAnchor:
  def of(kind: EvidenceSemanticAnchorKind, values: String*): EvidenceSemanticAnchor =
    EvidenceSemanticAnchor(kind, values.toList)

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

  private[judgment] def stableKey(kind: RelationFactKind): String =
    val premiseKey = premises.map(_.stableKey).mkString("[", ",", "]")
    val proofKey = proofKeys.map(_.stableKey).mkString("[", ",", "]")
    s"contract:${RelationFactKind.id(kind)}:premises:$premiseKey:proof-keys:$proofKey"

object RelationCombinationProof:
  private[chessjudgment] def from(
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
      controlledKingDestinations: List[KingDestinationControlWitness],
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
        ) ++ Option.when(includeDerivation)(proof.stableKey(RelationFactKind.GeometricControlSetDelta)).toList
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
        ) ++ Option
          .when(includeDerivation)(proof.stableKey(RelationFactKind.CaptureRecaptureInventory))
          .toList
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
        ) ++ Option
          .when(includeDerivation)(proof.stableKey(RelationFactKind.CreatedCheckResponseInventory))
          .toList
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
        ) ++ Option.when(includeDerivation)(
          proof.stableKey(RelationFactKind.RootCheckResponse)
        ).toList
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
        ) ++ Option.when(includeDerivation)(
          proof.stableKey(RelationFactKind.PawnTopologyTransition)
        ).toList
      case StalemateTransition(mover, stalledSide, kingSquare, proof) =>
        require(stalledSide != mover.side, "the after-position side to move must be the stalemated side")
        List(
          moveTransition(mover),
          side(stalledSide),
          square(kingSquare)
        ) ++ Option.when(includeDerivation)(
          proof.stableKey(RelationFactKind.StalemateTransition)
        ).toList
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
        VerticalRelationContracts.proofOf(vertical)
          .map(_ => RelationProofStage.DerivedFact)
          .getOrElse(
            throw IllegalArgumentException(
              s"relation '${vertical.detailName}' has no derivation proof"
            )
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

final case class LineReplayStep(
    ply: Int,
    moveUci: String,
    fenBefore: String,
    fenAfter: String
)

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
    runningBalancesForMover.dropWhile(_ >= 0).drop(1).exists(_ >= 0)

  def hasClosedRecovery: Boolean =
    isClosed && hasObservedRecovery && observedNetCpForMover >= 0

  def hasResolvedMaterialSequence: Boolean =
    hasRecaptureChain || hasObservedRecovery

  private[chessjudgment] def durableRecoveryCaptureForMover: Option[LineMaterialCapture] =
    Option.when(hasClosedRecovery)(
      events
        .drop(runningBalancesForMover.lastIndexWhere(_ < 0) + 1)
        .collectFirst {
          case event
              if event.movement.side == sideToMove && event.capture.nonEmpty =>
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

final case class LineFactEvidence private[chessjudgment] (
    line: LineNodeRef,
    private val material: Option[LineMaterialSummary] = None,
    private val replay: List[LineReplayStep] = Nil,
    private val events: List[LineMoveEvent] = Nil,
    private val consequences: List[LineConsequence] = Nil,
    private[chessjudgment] val canonicalReplay: Option[CanonicalLineReplay] = None
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
  private lazy val verifiedCanonicalReplay: Option[CanonicalLineReplay] =
    canonicalReplay.filter(_.matches(replay))
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
      consequences: List[LineConsequence] = Nil
  ): LineFactEvidence =
    require(
      replay.replaySteps.headOption.exists(step => EvidenceRef.sameMove(step.moveUci, line.rootMove)),
      "certified line evidence must begin with its declared root move"
    )
    val replayByOffset = replay.replaySteps.toVector
    def ownsOccurrence(occurrence: LineMoveOccurrence): Boolean =
      replayByOffset
        .lift(occurrence.plyOffset)
        .exists(step => occurrence.sameOccurrence(occurrence.plyOffset, step.moveUci))
    require(
      consequences.forall(consequence =>
        consequence.proofOccurrences.forall(ownsOccurrence) &&
          consequence.eventOccurrence.forall(ownsOccurrence)
      ),
      "a certified line consequence must retain only occurrences from its canonical replay"
    )
    LineFactEvidence(
      line = line,
      material = material,
      replay = replay.replaySteps,
      events = events,
      consequences = consequences,
      canonicalReplay = Some(replay)
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
) extends EvidencePayload:
  private[chessjudgment] def movement: Option[RelationMoveTransitionWitness] =
    canonicalTransitionProof.map(_.rootMovement.witness)

  private[chessjudgment] def moveEffect: Option[BoardMoveEffect] =
    canonicalTransitionProof.map(_.moveEffect)

/** Typed chess objects carried by structural consequences.
  * `label` is a one-way presentation projection; no proof code may recover
  * chess semantics by parsing it.
  */
enum StructuralSubject:
  case OpenFile(file: EvidenceFile)
  case SemiOpenFile(side: Color, file: EvidenceFile)
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
        case RelationProofStage.TransitionFact | RelationProofStage.DerivedFact => true
        case _                                                                  => false,
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
    def sequence(values: Iterable[String]): String =
      values.iterator.map(value => s"${value.length}:$value").mkString
    sequence(
      List(
        "transition-consequence",
        kind.toString.toLowerCase,
        sequence(subjectBindings.map(_.stableKey).distinct.sorted),
        sequence(targetBindings.map(_.stableKey).distinct.sorted),
        sequence(resultPremiseKeys.map(_.stableKey))
      )
    )
  private[chessjudgment] def resultSubjectFacts: List[StructuralSubject] =
    if targetSubjectFacts.nonEmpty then targetSubjectFacts else subjectFacts

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
  * production. Proof closure remains in the semantic inventory; the
  * occurrence persists only actual outputs and their exact source bindings.
  */
final class ClosedRelationOccurrenceEvidence private (
    val edge: MoveTransitionEdge,
    val lineOwner: LineNodeRef,
    val lineEvidence: EvidenceRef,
    private val outputsByEvidenceId: Map[String, ClosedRelationOutputBinding]
) extends EvidencePayload:
  val scope: EvidenceScope = EvidenceScope.LegalLine

  private[chessjudgment] def outputFor(result: EvidenceRef): Option[ClosedRelationOutputBinding] =
    outputsByEvidenceId.get(result.id).filter(_.result == result)

private[judgment] object ClosedRelationOccurrenceEvidence:
  def certified(
      edge: MoveTransitionEdge,
      lineOwner: LineNodeRef,
      lineEvidence: EvidenceRef,
      outputs: List[ClosedRelationOutputBinding]
  ): ClosedRelationOccurrenceEvidence =
    val edgeLineOwnership = edge.role match
      case TransitionEdgeRole.LineContinuation =>
        edge.evidence.line.contains(lineOwner)
      case _ => edge.evidence.line.isEmpty
    require(
      edge.evidence.producer == EvidenceProducer.MoveTransitionProducer &&
        edge.evidence.layer == EvidenceLayer.MoveTransition &&
        edge.evidence.position == edge.from &&
        edgeLineOwnership &&
        edge.evidence.scope == edge.role.scope &&
        edge.evidence.confidence == EvidenceConfidence.LegalReplayVerified,
      "a closed relation occurrence must be owned by its exact admitted transition authority"
    )
    require(
      lineEvidence.producer == EvidenceProducer.LegalLineProducer &&
        lineEvidence.layer == EvidenceLayer.Line &&
        lineEvidence.line.contains(lineOwner) &&
        lineEvidence.scope == EvidenceScope.LegalLine &&
        lineEvidence.confidence == EvidenceConfidence.LegalReplayVerified,
      "a closed relation occurrence must be owned by its exact line evidence"
    )
    require(
      outputs.forall(output =>
        output.result.producer == EvidenceProducer.RelationProducer &&
          output.result.layer == EvidenceLayer.Relation &&
          output.result.position == edge.from &&
          output.result.line.contains(lineOwner) &&
          output.result.scope == EvidenceScope.LegalLine &&
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
        line = Some(payload.lineOwner),
        scope = payload.scope,
        confidence = EvidenceConfidence.LegalReplayVerified
      ),
      payload = payload,
      parents = List(payload.edge.evidence, payload.lineEvidence)
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
  private[chessjudgment] def certifiedControlSetDeltas(
      unverified: ControlSetDeltaResults,
      delta: RelationSemanticDelta,
      legal: LegalReplayStep,
      footprint: BoardTransitionFootprint
  ): Option[ControlSetDeltaResults] =
    for
      certification <- legalReplayCertification(legal, footprint)
      if unverified.owns(delta)
    yield unverified.certifiedBy(certification.owner, certification.certify)

  private[chessjudgment] def certifiedVerticalBatch(
      unverified: ClosedVerticalRelationResults,
      lowerResults: ControlSetDeltaResults,
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
        occurrence.resultPremiseOccurrences.map(_.resultKey) == resultPremiseSources.map(_.key)
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

final case class CandidateComparisonEvidence(
    comparison: CandidateComparisonFact
) extends EvidencePayload

final case class RelativeAssessmentEvidence(
    assessment: RelativeMoveAssessment
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
      case CandidateLineEvaluationEvidence(payloadLine, _) =>
        List(payloadLine)
      case CandidateComparisonEvidence(fact) =>
        List(fact.referenceLine, fact.candidateLine)
      case RelativeAssessmentEvidence(assessment) =>
        List(assessment.reference.ref, assessment.candidate.ref)
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
        .orElse(ref.line.map(EvidenceIdentity.lineKey))
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

  private def intrinsicallyProofEligible(record: EvidenceRecord): Boolean =
    (record.ref.confidence != EvidenceConfidence.Mixed || record.parents.nonEmpty) &&
      (record.payload match
        case PositionOccurrenceEvidence(occurrence) =>
          exactAuthority(record, EvidenceProducer.PositionOccurrenceProducer, EvidenceLayer.PositionOccurrence) &&
            record.ref.confidence == EvidenceConfidence.BoardDerived &&
            PrincipalVariationEvidence.sameBoardState(occurrence.fen, record.ref.position.fen) &&
            occurrence.plyCount == record.ref.position.ply &&
            record.ref.position.sideToMove.forall(_ == occurrence.sideToMove) &&
            exactPositionOccurrenceOwner(record)
        case payload: LineFactEvidence =>
          exactAuthority(record, EvidenceProducer.LegalLineProducer, EvidenceLayer.Line) &&
            record.ref.confidence == EvidenceConfidence.LegalReplayVerified &&
            record.ref.line.contains(payload.line) && record.ref.scope == EvidenceScope.LegalLine &&
            payload.replayIsCertified && payload.rootMove.exists(EvidenceRef.sameMove(_, payload.line.rootMove)) &&
            payload.canonicalReplay.exists(_.replaySteps.headOption.exists(step =>
              PrincipalVariationEvidence.sameBoardState(step.fenBefore, record.ref.position.fen)
            ))
        case CandidateLineEvaluationEvidence(line, evaluation) =>
          record.ref.layer == EvidenceLayer.Eval && record.ref.line.contains(line) &&
            Set(EvidenceScope.BestLine, EvidenceScope.PlayedLine, EvidenceScope.CandidateLine)(record.ref.scope) &&
            (evaluation match
              case lila.chessjudgment.model.line.CandidateLineEvaluation.EngineSearch(_) =>
                record.ref.producer == EvidenceProducer.EngineEvalProducer &&
                  record.ref.confidence == EvidenceConfidence.EngineBacked
              case lila.chessjudgment.model.line.CandidateLineEvaluation.ExactAutomaticTerminal(_, _) =>
                record.ref.producer == EvidenceProducer.LegalLineProducer &&
                  record.ref.confidence == EvidenceConfidence.LegalReplayVerified)
        case payload @ MoveTransitionEvidence(moveUci, from, to, _) =>
          exactAuthority(record, EvidenceProducer.MoveTransitionProducer, EvidenceLayer.MoveTransition) &&
            record.ref.confidence == EvidenceConfidence.LegalReplayVerified &&
            record.ref.position == from && from != to &&
            EvidenceRef.normalizeMove(moveUci).nonEmpty &&
            payload.canonicalTransitionProof.exists(
              _.provesMove(moveUci, from, to, record.ref.scope, record.ref.line)
            ) && exactMoveTransitionOwner(record, moveUci, from, to)
        case payload: StructuralDeltaEvidence =>
          structuralDeltaAncestryVerified(record, payload) &&
            payload.canonicalOutputShapeCertified
        case payload: RelationFactEvidence =>
          relationOccurrenceAncestryVerified(record, payload)
        case payload: ClosedRelationOccurrenceEvidence =>
          closedRelationAncestryVerified(record, payload)
        case CandidateComparisonEvidence(comparison) =>
          candidateComparisonAncestryVerified(record, comparison)
        case RelativeAssessmentEvidence(assessment) =>
          relativeAssessmentAncestryVerified(record, assessment)
      )

  private def exactPositionOccurrenceOwner(record: EvidenceRecord): Boolean =
    record.ref.line match
      case None => record.ref.scope != EvidenceScope.LegalLine && record.parents.isEmpty
      case Some(line) =>
        record.ref.scope == EvidenceScope.LegalLine && (record.parents match
          case parent :: Nil =>
            byId.get(parent.id).exists {
              case EvidenceRecord(ref, MoveTransitionEvidence(_, _, to, _), _) =>
                ref == parent && ref.producer == EvidenceProducer.MoveTransitionProducer &&
                  ref.layer == EvidenceLayer.MoveTransition && ref.line.contains(line) &&
                  ref.scope == EvidenceScope.LegalLine && to == record.ref.position
              case _ => false
            }
          case _ => false)

  private def exactMoveTransitionOwner(
      record: EvidenceRecord,
      moveUci: String,
      from: PositionNodeRef,
      to: PositionNodeRef
  ): Boolean =
    record.ref.line match
      case None => record.ref.scope != EvidenceScope.LegalLine && record.parents.isEmpty
      case Some(line) =>
        record.ref.scope == EvidenceScope.LegalLine &&
          certifiedLineParent(record, line).exists(replay =>
            replay.transitionIndex(moveUci, from, to).exists(_ > 0)
          )

  private def certifiedLineParent(
      record: EvidenceRecord,
      line: LineNodeRef
  ): Option[CanonicalLineReplay] =
    record.parents match
      case parent :: Nil =>
        byId
          .get(parent.id)
          .filter(_.ref == parent)
          .flatMap(certifiedLineReplay)
          .collect { case (`line`, replay) => replay }
      case _ => None

  private def certifiedLineReplay(
      record: EvidenceRecord
  ): Option[(LineNodeRef, CanonicalLineReplay)] =
    record match
      case EvidenceRecord(ref, payload: LineFactEvidence, _)
          if ref.producer == EvidenceProducer.LegalLineProducer &&
            ref.layer == EvidenceLayer.Line &&
            ref.confidence == EvidenceConfidence.LegalReplayVerified &&
            ref.line.contains(payload.line) && ref.scope == EvidenceScope.LegalLine =>
        payload.certifiedReplay.map(payload.line -> _)
      case _ => None

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
              ref.scope == EvidenceScope.LegalLine && payload.line == line =>
          source
      } match
        case source :: Nil => Some(source)
        case _             => None
    def exactEvaluation(
        line: LineNodeRef,
        selection: SemanticLineKey,
        lineSource: EvidenceRecord
    ): Option[lila.chessjudgment.model.line.CandidateLineEvaluation] =
      directParents.collect {
        case EvidenceRecord(
              ref,
              CandidateLineEvaluationEvidence(payloadLine, evaluation),
              parents
            )
            if ref.position == record.ref.position && ref.line.contains(line) &&
              ref.scope == selection.role.scope && payloadLine == line &&
              parents == List(lineSource.ref) =>
          evaluation
      } match
        case evaluation :: Nil => Some(evaluation)
        case _                 => None
    val canonicalInputs = for
      referenceSource <- exactLineSource(comparison.referenceLine)
      candidateSource <- exactLineSource(comparison.candidateLine)
      referenceEvaluation <- exactEvaluation(comparison.referenceLine, comparison.referenceSelection, referenceSource)
      candidateEvaluation <- exactEvaluation(comparison.candidateLine, comparison.candidateSelection, candidateSource)
    yield (
      referenceSource,
      candidateSource,
      CandidateLineNode(
        comparison.referenceLine,
        comparison.referenceSelection.role,
        comparison.referenceSelection.rank,
        referenceEvaluation,
        referenceSource.ref
      ),
      CandidateLineNode(
        comparison.candidateLine,
        comparison.candidateSelection.role,
        comparison.candidateSelection.rank,
        candidateEvaluation,
        candidateSource.ref
      )
    )
    record.ref.position.sideToMove.contains(comparison.comparison.mover) &&
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

  private def relativeAssessmentAncestryVerified(
      record: EvidenceRecord,
      assessment: RelativeMoveAssessment
  ): Boolean =
    val directParents = record.parents.flatMap(parent => byId.get(parent.id).filter(_.ref == parent))
    def parent(ref: EvidenceRef): Option[EvidenceRecord] =
      directParents.find(_.ref == ref)
    def exactTransition(edge: MoveTransitionEdge): Boolean =
      parent(edge.evidence).exists(edge.matches)
    def endpointBound(line: CandidateLineNode, comparison: EvidenceRecord): Boolean =
      val evaluations = comparison.parents
        .flatMap(parent => byId.get(parent.id).filter(_.ref == parent))
        .collect {
          case EvidenceRecord(_, CandidateLineEvaluationEvidence(ref, evaluation), _) if ref == line.ref =>
            evaluation
        }
      comparison.parents.contains(line.lineEvidence) && evaluations == List(line.evaluation)
    def comparisonBound(source: EvidenceRecord, fact: CandidateComparisonFact): Boolean =
      val referenceBound =
        assessment.reference.role == LineNodeRole.BestReference &&
          fact.referenceLine == assessment.reference.ref &&
          fact.referenceSelection == SemanticLineKey.from(assessment.reference) &&
          endpointBound(assessment.reference, source)
      val playedBound =
        assessment.played.role == TransitionEdgeRole.Played &&
          assessment.played.from == record.ref.position &&
          EvidenceRef.sameMove(assessment.played.moveUci, assessment.candidate.ref.rootMove)
      referenceBound && playedBound && fact.hasDistinctRootMoves && (fact.kind match
        case CandidateComparisonKind.PlayedVsBest =>
          assessment.candidate.role == LineNodeRole.Played &&
            fact.candidateLine == assessment.candidate.ref &&
            fact.candidateSelection == SemanticLineKey.from(assessment.candidate) &&
            endpointBound(assessment.candidate, source) &&
            assessment.referenceTransition.exists(edge =>
              edge.role == TransitionEdgeRole.Alternative &&
                edge.from == assessment.played.from &&
                EvidenceRef.sameMove(edge.moveUci, assessment.reference.ref.rootMove)
            )
        case CandidateComparisonKind.BestVsSecond =>
          assessment.candidate == assessment.reference &&
            assessment.referenceTransition.isEmpty &&
            EvidenceRef.sameMove(assessment.played.moveUci, assessment.reference.ref.rootMove) &&
            fact.candidateSelection.role == LineNodeRole.Alternative &&
            fact.candidateSelection.rank == 2)
    val comparison = parent(assessment.primaryComparisonEvidence).collect {
      case source @ EvidenceRecord(_, CandidateComparisonEvidence(fact), _) => source -> fact
    }
    exactAuthority(record, EvidenceProducer.RelativeMoveProducer, EvidenceLayer.RelativeAssessment) &&
      record.ref == assessment.evidence &&
      record.ref.position == assessment.played.from &&
      record.ref.line.contains(assessment.candidate.ref) &&
      record.ref.scope == EvidenceScope.Counterfactual &&
      assessment.canonicalParents.contains(record.parents) &&
      directParents.size == record.parents.size &&
      exactTransition(assessment.played) &&
      assessment.referenceTransition.forall(exactTransition) &&
      comparison.exists { case (source, fact) =>
        record.ref.confidence == source.ref.confidence &&
          comparisonBound(source, fact)
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
                record.ref.line.contains(occurrence.lineOwner) &&
                record.ref.scope == occurrence.scope &&
                record.parents.size == expectedParents.size &&
                record.parents.toSet == expectedParents.toSet &&
                exactClosedRelationBinding(binding, occurrence)
            }
          case _ => false
      case RelationEvidenceOrigin.Unverified => false

  private def closedRelationAncestryVerified(
      record: EvidenceRecord,
      payload: ClosedRelationOccurrenceEvidence
  ): Boolean =
    val expectedParents = List(payload.edge.evidence, payload.lineEvidence)
    val exactTransition = byId.get(payload.edge.evidence.id).exists {
      case exact @ EvidenceRecord(ref, MoveTransitionEvidence(moveUci, from, to, _), parents) =>
        ref == payload.edge.evidence && intrinsicallyProofEligible(exact) &&
          parents == payload.edge.evidence.line.map(_ => payload.lineEvidence).toList &&
          from == payload.edge.from && to == payload.edge.to &&
          EvidenceRef.sameMove(moveUci, payload.edge.moveUci)
      case _ => false
    }
    val exactLine = byId
      .get(payload.lineEvidence.id)
      .filter(_.ref == payload.lineEvidence)
      .flatMap(certifiedLineReplay)
      .filter(_._1 == payload.lineOwner)
      .exists { case (_, replay) =>
        val occurrenceIndex = replay.transitionIndex(
          payload.edge.moveUci,
          payload.edge.from,
          payload.edge.to
        )
        payload.edge.role match
          case TransitionEdgeRole.LineContinuation =>
            payload.edge.evidence.line.contains(payload.lineOwner) &&
              occurrenceIndex.exists(_ > 0)
          case _ =>
            payload.edge.evidence.line.isEmpty && payload.lineEvidence.position == payload.edge.from &&
              EvidenceRef.sameMove(payload.lineOwner.rootMove, payload.edge.moveUci) &&
              occurrenceIndex.contains(0)
      }
    record.ref.producer == EvidenceProducer.RelationProducer &&
      record.ref.layer == EvidenceLayer.Relation &&
      record.ref.confidence == EvidenceConfidence.LegalReplayVerified &&
      record.ref.position == payload.edge.from &&
      record.ref.line.contains(payload.lineOwner) &&
      record.ref.scope == payload.scope &&
      record.parents.size == expectedParents.size &&
      record.parents.toSet == expectedParents.toSet &&
      exactTransition && exactLine

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
      case RelationProofStage.DerivedFact =>
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
    val lineParents = directParents.flatMap(certifiedLineReplay)
    val exactLineOwner = payload.line match
      case Some(line) =>
        lineParents match
          case (`line`, replay) :: Nil =>
            val occurrenceIndex = replay.transitionIndex(
              payload.moveUci,
              payload.from,
              payload.to
            )
            payload.role match
              case TransitionEdgeRole.LineContinuation =>
                transitionParent.exists(_.ref.line.contains(line)) &&
                  occurrenceIndex.exists(_ > 0)
              case _ =>
                transitionParent.exists(_.ref.line.isEmpty) &&
                  EvidenceRef.sameMove(line.rootMove, payload.moveUci) &&
                  occurrenceIndex.contains(0)
          case _ => false
      case None => lineParents.isEmpty && transitionParent.exists(_.ref.line.isEmpty)
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
                      payload.line.contains(occurrence.lineOwner) &&
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
      record.ref.line == payload.line &&
      record.ref.scope == payload.role.scope &&
      transitionParent.nonEmpty &&
      exactLineOwner &&
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
