package lila.chessjudgment.model

import chess.{ Square, Role, Color, File }
import lila.chessjudgment.model.strategic.{
  EndgameOppositionType,
  RookEndgameGeometry,
  RookEndgamePattern as RookEndgamePatternKind,
  RuleOfSquareStatus,
  TheoreticalOutcomeHint
}

/**
 * Fact Scope: Defines the temporal or logical context of a fact.
 */
sealed trait FactScope
object FactScope {
  case object Now extends FactScope
}

/**
 * Fact: A verified, structured piece of chess evidence.
 * Facts are low-level board evidence.
 */
sealed trait Fact {
  def scope: FactScope
  def participants: List[Square]
  def squareFocus: Fact.SquareFocus = Fact.squareFocus(this)
}

object Fact {

  final case class SquareFocus(
      subjectSquares: List[Square] = Nil,
      targetSquares: List[Square] = Nil,
      attackerSquares: List[Square] = Nil,
      defenderSquares: List[Square] = Nil,
      relatedSquares: List[Square] = Nil,
      tacticalHintSquares: List[Square] = Nil,
      vulnerableMaterialSquares: List[Square] = Nil
  )

  def squareFocus(fact: Fact): SquareFocus =
    def unique(squares: List[Square]) = squares.distinct
    fact match
      case HangingPiece(_, square, _, attackers, defenders, _) =>
        SquareFocus(
          subjectSquares = List(square),
          targetSquares = List(square),
          attackerSquares = unique(attackers),
          defenderSquares = unique(defenders),
          relatedSquares = List(square),
          tacticalHintSquares = List(square),
          vulnerableMaterialSquares = List(square)
        )
      case TargetPiece(_, square, _, attackers, defenders, _) =>
        SquareFocus(
          subjectSquares = List(square),
          targetSquares = List(square),
          attackerSquares = unique(attackers),
          defenderSquares = unique(defenders),
          relatedSquares = List(square),
          tacticalHintSquares = List(square),
          vulnerableMaterialSquares = List(square)
        )
      case Pin(_, attacker, _, pinned, _, behind, _, _, _) =>
        val relationSquares = List(pinned, behind)
        SquareFocus(
          subjectSquares = List(pinned),
          targetSquares = List(behind),
          attackerSquares = List(attacker),
          relatedSquares = relationSquares,
          tacticalHintSquares = relationSquares
        )
      case Skewer(_, attacker, _, front, _, back, _, _) =>
        val relationSquares = List(front, back)
        SquareFocus(
          subjectSquares = List(front),
          targetSquares = List(back),
          attackerSquares = List(attacker),
          relatedSquares = relationSquares,
          tacticalHintSquares = relationSquares
        )
      case Fork(_, attacker, _, targets, _) =>
        val targetSquares = targets.map(_._1)
        SquareFocus(
          subjectSquares = List(attacker),
          targetSquares = targetSquares,
          attackerSquares = List(attacker),
          relatedSquares = targetSquares,
          tacticalHintSquares = targetSquares
        )
      case XRay(_, attacker, _, blocker, _, target, _, _) =>
        val relationSquares = List(attacker, blocker, target)
        SquareFocus(
          subjectSquares = List(attacker),
          targetSquares = List(target),
          attackerSquares = List(attacker),
          relatedSquares = relationSquares,
          tacticalHintSquares = relationSquares
        )
      case Battery(_, front, _, back, _, _, _, target, _) =>
        val relationSquares = (List(front, back) ++ target).distinct
        SquareFocus(
          subjectSquares = List(front, back),
          targetSquares = target.toList,
          attackerSquares = List(front, back),
          relatedSquares = relationSquares,
          tacticalHintSquares = relationSquares
        )
      case FileControl(_, _, _, _) | SpaceAdvantage(_, _, _) =>
        SquareFocus()
      case WeakSquare(square, _, _, _) =>
        SquareFocus(
          subjectSquares = List(square),
          targetSquares = List(square),
          relatedSquares = List(square),
          tacticalHintSquares = List(square)
        )
      case Outpost(square, _, _) =>
        SquareFocus(
          subjectSquares = List(square),
          targetSquares = List(square),
          relatedSquares = List(square)
        )
      case ActivatesPiece(_, from, to, _, _) =>
        SquareFocus(
          subjectSquares = List(to),
          targetSquares = List(to),
          relatedSquares = List(from, to)
        )
      case KingActivity(_, square, _, _, _) =>
        SquareFocus(subjectSquares = List(square), relatedSquares = List(square))
      case Opposition(_, king, enemyKing, _, _, _, _) =>
        SquareFocus(subjectSquares = List(king), targetSquares = List(enemyKing), relatedSquares = List(king, enemyKing))
      case RuleOfSquare(defenderKing, targetPawn, promotionSquare, _, _) =>
        SquareFocus(
          subjectSquares = List(defenderKing),
          targetSquares = List(targetPawn, promotionSquare),
          relatedSquares = List(defenderKing, targetPawn, promotionSquare)
        )
      case PawnPromotion(square, _, _) =>
        SquareFocus(subjectSquares = List(square), targetSquares = List(square), relatedSquares = List(square))
      case StalemateThreat(king, _) =>
        SquareFocus(subjectSquares = List(king), targetSquares = List(king), relatedSquares = List(king))
      case DoubleCheck(checkingSquares, _) =>
        SquareFocus(attackerSquares = unique(checkingSquares), relatedSquares = unique(checkingSquares), tacticalHintSquares = unique(checkingSquares))
      case RookEndgamePattern(_, _, _, anchorSquares, geometry) =>
        val related = if anchorSquares.nonEmpty then anchorSquares else geometry.toList.flatMap(_.anchorSquares)
        SquareFocus(relatedSquares = unique(related))
      case EndgameOutcome(_, _, _) | Zugzwang(_, _) =>
        SquareFocus()

  enum WeakSquareReason:
    case NoPawnDefense
    case StructuralHole
    case Unknown

  enum RayAxis:
    case File
    case Rank
    case Diagonal

  // --- Tactical Facts ---

  /** A piece that is attacked more than it is defended, or undefended. */
  case class HangingPiece(
      owner: Color,
      square: Square,
      role: Role,
      attackers: List[Square],
      defenders: List[Square],
      scope: FactScope
  ) extends Fact {
    def participants = square :: (attackers ++ defenders).distinct
  }

  /** A piece on a square that is a tactical target (even if not hanging). */
  case class TargetPiece(
      owner: Color,
      square: Square,
      role: Role,
      attackers: List[Square],
      defenders: List[Square],
      scope: FactScope
  ) extends Fact {
    def participants = square :: (attackers ++ defenders).distinct
  }

  /** A piece (pinned) that cannot move without exposing a more valuable piece (behind). */
  case class Pin(
      attackerColor: Color,
      attacker: Square,
      attackerRole: Role,
      pinned: Square,
      pinnedRole: Role,
      behind: Square,
      behindRole: Role,
      isAbsolute: Boolean,
      scope: FactScope
  ) extends Fact {
    def participants = List(attacker, pinned, behind)
  }

  /** A piece (front) that must move, exposing another piece (back) to capture. */
  case class Skewer(
      attackerColor: Color,
      attacker: Square,
      attackerRole: Role,
      front: Square,
      frontRole: Role,
      back: Square,
      backRole: Role,
      scope: FactScope
  ) extends Fact {
    def participants = List(attacker, front, back)
  }

  /** A single piece (attacker) that attacks multiple targets simultaneously. */
  case class Fork(
      attackerColor: Color,
      attacker: Square,
      attackerRole: Role,
      targets: List[(Square, Role)],
      scope: FactScope
  ) extends Fact {
    def participants = attacker :: targets.map(_._1)
  }

  case class XRay(
      attackerColor: Color,
      attacker: Square,
      attackerRole: Role,
      blocker: Square,
      blockerRole: Role,
      target: Square,
      targetRole: Role,
      scope: FactScope
  ) extends Fact {
    def participants = List(attacker, blocker, target)
  }

  case class Battery(
      attackerColor: Color,
      front: Square,
      frontRole: Role,
      back: Square,
      backRole: Role,
      axis: RayAxis,
      scope: FactScope,
      target: Option[Square] = None,
      targetRole: Option[Role] = None
  ) extends Fact {
    def participants = (List(front, back) ++ target).distinct
  }

  case class FileControl(
      file: File,
      color: Color,
      open: Boolean,
      scope: FactScope
  ) extends Fact {
    def participants = Nil
  }

  case class SpaceAdvantage(
      color: Color,
      pawnDelta: Int,
      scope: FactScope
  ) extends Fact {
    def participants = Nil
  }

  // --- Structural / Strategic Facts ---

  /** A square that is difficult to defend and can be exploited. */
  case class WeakSquare(
      square: Square,
      color: Color,
      reason: WeakSquareReason,
      scope: FactScope
  ) extends Fact {
    def participants = List(square)
  }

  /** A strong outpost for a piece. */
  case class Outpost(
      square: Square,
      role: Role,
      scope: FactScope
  ) extends Fact {
    def participants = List(square)
  }

  // --- Intent Facts ---

  /** Verifiable intent: A piece was moved to a square with a specific effect. */
  case class ActivatesPiece(
      role: Role,
      from: Square,
      to: Square,
      openedRay: Boolean = false,
      scope: FactScope
  ) extends Fact {
    def participants = List(from, to)
  }

  // --- Endgame Facts ---

  /** Metrics for king activity in the endgame. */
  case class KingActivity(
      color: Color,
      square: Square,
      mobility: Int,
      proximityToCenter: Int,
      scope: FactScope
  ) extends Fact {
    def participants = List(square)
  }

  /** Relationship between kings in the endgame. */
  case class Opposition(
      color: Color,
      king: Square,
      enemyKing: Square,
      distance: Int, // 1 for direct, 3/5 for distant
      isDirect: Boolean,
      oppositionType: EndgameOppositionType = EndgameOppositionType.Direct,
      scope: FactScope
  ) extends Fact {
    def participants = List(king, enemyKing)
  }

  /** Rule of the square state against a passed pawn race. */
  case class RuleOfSquare(
      defenderKing: Square,
      targetPawn: Square,
      promotionSquare: Square,
      status: RuleOfSquareStatus,
      scope: FactScope
  ) extends Fact {
    def participants = List(defenderKing, targetPawn, promotionSquare)
  }

  /** Canonical rook endgame pattern detected by the oracle. */
  case class RookEndgamePattern(
      pattern: RookEndgamePatternKind,
      scope: FactScope,
      primaryPattern: Option[String] = None,
      anchorSquares: List[Square] = Nil,
      geometry: Option[RookEndgameGeometry] = None
  ) extends Fact {
    def participants = if anchorSquares.nonEmpty then anchorSquares else geometry.toList.flatMap(_.anchorSquares)
  }

  /** Coarse theoretical outcome hint from deterministic endgame rules. */
  case class EndgameOutcome(
      outcome: TheoreticalOutcomeHint,
      confidence: Double,
      scope: FactScope
  ) extends Fact {
    def participants = Nil
  }

  /** Zugzwang - any move worsens the position. */
  case class Zugzwang(
      color: Color,
      scope: FactScope
  ) extends Fact {
    def participants = Nil
  }

  /** Pawn promotion or push towards it. */
  case class PawnPromotion(
      square: Square,
      promotedTo: Option[Role], // None if it's a push of a passed pawn
      scope: FactScope
  ) extends Fact {
    def participants = List(square)
  }

  /** Stalemate threat in a desperate position. */
  case class StalemateThreat(
      king: Square,
      scope: FactScope
  ) extends Fact {
    def participants = List(king)
  }

  /** Double check - two pieces checking at once. */
  case class DoubleCheck(
      checkingSquares: List[Square],
      scope: FactScope
  ) extends Fact {
    def participants = checkingSquares
  }
}
