package lila.chessjudgment.model.strategic

import chess.{ Color, Rank, Role, Square }
import lila.chessjudgment.model.{ PlanEventIdentity, TransitionType }

case class PieceActivity(
    piece: Role,
    square: Square,
    mobilityScore: Double, // 0.0 (Trapped) to 1.0 (Optimal)
    isTrapped: Boolean, // No safe moves
    isBadBishop: Boolean, // Blocked by own pawns
    keyRoutes: List[Square], // Path to relevant area (e.g. Qh5 -> h7)
    coordinationLinks: List[Square], // Squares defended/attacked in tandem with friendly pieces
    directionalTargets: List[Square] = Nil, // Empty strategic squares worth working toward, but not yet route-quality
    concreteTargets: List[Square] = Nil // Enemy-occupied tactical or exchange squares, not redeployment routes
)

case class Compensation(
    investedMaterial: Int, // CP value sacrificed
    returnVector: Map[String, Double], // { "Time" -> 0.8, "Space" -> 0.5, "Attack" -> 0.9 }
    expiryPly: Option[Int] // How long does this compensation lasts
)

// Endgame Features
enum EndgameOppositionType:
  case Direct, Distant, Diagonal, None

enum RuleOfSquareStatus:
  case Holds, Fails, NA

enum RookEndgamePattern:
  case RookBehindPassedPawn, KingCutOff, TarraschDefenseActive, PassiveRookDefense, None

enum TheoreticalOutcomeHint:
  case Win, Draw, Unclear

case class RookEndgameGeometry(
    techniqueSide: Color,
    strongSide: Color,
    defendingSide: Color,
    passedPawn: Square,
    promotionSquare: Square,
    attackingKing: Option[Square] = None,
    defendingKing: Option[Square] = None,
    attackingRook: Option[Square] = None,
    defendingRook: Option[Square] = None,
    barrierRank: Option[Rank] = None
):
  def techniqueKing: Option[Square] =
    if techniqueSide == strongSide then attackingKing
    else if techniqueSide == defendingSide then defendingKing
    else None

  def anchorSquares: List[Square] =
    (
      techniqueKing.toList ++
        attackingKing.toList ++
        defendingKing.toList ++
        attackingRook.toList ++
        defendingRook.toList ++
        List(passedPawn, promotionSquare)
    ).distinct

case class EndgameFeature(
    hasOpposition: Boolean,
    isZugzwang: Boolean,
    keySquaresControlled: List[Square],
    oppositionType: EndgameOppositionType = EndgameOppositionType.None,
    zugzwangLikelihood: Double = 0.0,
    ruleOfSquare: RuleOfSquareStatus = RuleOfSquareStatus.NA,
    kingActivityDelta: Int = 0,
    rookEndgamePattern: RookEndgamePattern = RookEndgamePattern.None,
    theoreticalOutcomeHint: TheoreticalOutcomeHint = TheoreticalOutcomeHint.Unclear,
    confidence: Double = 0.0,
    primaryPattern: Option[String] = None,
    rookEndgameAnchorSquares: List[Square] = Nil,
    rookEndgameGeometry: Option[RookEndgameGeometry] = None
)

// (VariationLine moved to Variation.scala)

// StructureTag and PlanTag have been removed as they were obsolete dead code.

enum PositionalTag:
  case Outpost(square: Square, color: Color)
  case OpenFile(file: chess.File, color: Color)
  case WeakSquare(square: Square, color: Color)
  case LoosePiece(square: Square, role: Role, color: Color)
  case WeakBackRank(color: Color)
  case BishopPairAdvantage(color: Color)
  case BadBishop(color: Color)
  case GoodBishop(color: Color)
  // New additions
  case RookOnSeventh(color: Color)
  case StrongKnight(square: Square, color: Color)
  case SpaceAdvantage(color: Color)
  case OppositeColorBishops
  case KingStuckCenter(color: Color)
  case ConnectedRooks(color: Color)
  case DoubledRooks(file: chess.File, color: Color)
  case ColorComplexWeakness(color: Color, squareColor: String, squares: List[Square])  // "light" or "dark"
  case PawnMajority(color: Color, flank: String, count: Int)  // "queenside" or "kingside"
  case MinorityAttack(color: Color, flank: String)
  // case QueenActivity(color: Color)
  // case QueenManeuver(color: Color)
  case MateNet(color: Color)
  // case PerpetualCheck(color: Color)
  case RemovingTheDefender(target: Role, color: Color)
  case Initiative(color: Color)

case class PlanContinuity(
  startingEvent: Option[PlanEventIdentity],
  consecutivePlies: Int,
  startingPly: Int,
  supportingMoves: List[String] = Nil,
  supportingEvents: List[PlanEventIdentity] = Nil,
  completionProven: Boolean = false
):
  def episodeTransitionType: TransitionType =
    if supportingEvents.isEmpty then TransitionType.Opening
    else if completionProven then TransitionType.Completion
    else TransitionType.Continuation

object PlanContinuity:
  import play.api.libs.json.*
  import lila.chessjudgment.model.PlanEventIdentity.given
  given Reads[PlanContinuity] = Reads { js =>
    for
      startingEvent <- (js \ "startingEvent").validateOpt[PlanEventIdentity]
      consecutivePlies <- (js \ "consecutivePlies").validate[Int]
      startingPly <- (js \ "startingPly").validate[Int]
      supportingMoves <- (js \ "supportingMoves").validateOpt[List[String]]
      supportingEvents <- (js \ "supportingEvents").validateOpt[List[PlanEventIdentity]]
      completionProven <- (js \ "completionProven").validateOpt[Boolean]
    yield PlanContinuity(
      startingEvent = startingEvent,
      consecutivePlies = consecutivePlies,
      startingPly = startingPly,
      supportingMoves = supportingMoves.getOrElse(Nil),
      supportingEvents = supportingEvents.getOrElse(Nil),
      completionProven = completionProven.getOrElse(false)
    )
  }
  given Writes[PlanContinuity] = Writes { c =>
    Json.obj(
      "startingEvent" -> c.startingEvent,
      "consecutivePlies" -> c.consecutivePlies,
      "startingPly" -> c.startingPly,
      "supportingMoves" -> c.supportingMoves,
      "supportingEvents" -> c.supportingEvents,
      "completionProven" -> c.completionProven
    )
  }

  def fromEvents(
      events: List[(PlanEventIdentity, Int)],
      completionProven: Boolean
  ): Option[PlanContinuity] =
    events.sortBy(_._2) match
      case Nil | _ :: Nil => None
      case ordered =>
        val supporting = ordered.drop(1)
        Some(
          PlanContinuity(
            startingEvent = Some(ordered.head._1),
            consecutivePlies = (ordered.last._2 - ordered.head._2 + 1).max(1),
            startingPly = ordered.head._2,
            supportingMoves = supporting.map(_._1.rootMove),
            supportingEvents = supporting.map(_._1),
            completionProven = completionProven
          )
        )

  def fromAntecedents(
      events: List[(PlanEventIdentity, Int)],
      currentPly: Int,
      completionProven: Boolean
  ): Option[PlanContinuity] =
    events.sortBy(_._2) match
      case Nil => None
      case ordered if ordered.last._2 >= currentPly => None
      case ordered =>
        Some(
          PlanContinuity(
            startingEvent = Some(ordered.head._1),
            consecutivePlies = (currentPly - ordered.head._2 + 1).max(1),
            startingPly = ordered.head._2,
            supportingMoves = ordered.map(_._1.rootMove),
            supportingEvents = ordered.map(_._1),
            completionProven = completionProven
          )
        )
