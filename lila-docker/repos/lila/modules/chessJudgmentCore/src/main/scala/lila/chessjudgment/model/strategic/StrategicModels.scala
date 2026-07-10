package lila.chessjudgment.model.strategic

import chess.{ Color, Rank, Role, Square }

case class PreventedPlan(
    planId: String, // e.g. "StopCheck", "PreventFork", "DenyBreak"
    deniedSquares: List[Square], // Squares opponent stopped controlling/occupying
    breakNeutralized: Option[String], // e.g. "f5"
    mobilityDelta: Int, // Change in opponent's safe mobility
    counterplayScoreDrop: Int, // Drop in opponent's positional eval
    preventedThreatType: Option[String] = None, // "Check", "Fork", "Mate", "Material"
    deniedResourceClass: Option[String] = None, // "break" | "entry_square" | "forcing_threat" | "piece_activity"
    deniedEntryScope: Option[String] = None, // "single_square" | "file" | "sector"
    breakNeutralizationStrength: Option[Int] = None, // 0-100
    defensiveSufficiency: Option[Int] = None, // 0-100
    sourceScope: lila.chessjudgment.model.FactScope = lila.chessjudgment.model.FactScope.Now,
    sourceLine: Option[VariationLine] = None
)

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
    triangulationAvailable: Boolean = false,
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
  planId: Option[String],
  consecutivePlies: Int,
  startingPly: Int,
  supportingMoves: List[String] = Nil
)
object PlanContinuity:
  import play.api.libs.json.*
  given Reads[PlanContinuity] = Reads { js =>
    for
      planId <- (js \ "planId").validateOpt[String]
      consecutivePlies <- (js \ "consecutivePlies").validate[Int]
      startingPly <- (js \ "startingPly").validate[Int]
      supportingMoves <- (js \ "supportingMoves").validateOpt[List[String]]
    yield PlanContinuity(
      planId = planId,
      consecutivePlies = consecutivePlies,
      startingPly = startingPly,
      supportingMoves = supportingMoves.getOrElse(Nil)
    )
  }
  given Writes[PlanContinuity] = Writes { c =>
    Json.obj(
      "planId" -> c.planId,
      "consecutivePlies" -> c.consecutivePlies,
      "startingPly" -> c.startingPly,
      "supportingMoves" -> c.supportingMoves
    )
  }
