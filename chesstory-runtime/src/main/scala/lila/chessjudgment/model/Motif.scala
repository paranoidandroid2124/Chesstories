package lila.chessjudgment.model

import chess.*

/**
 * Atomic high-level observation from a move or position state.
 * Normalized to be color-agnostic where applicable using relative coordinates.
 * 
 * Categories:
 * - Pawn Motifs: PawnAdvance, PawnBreak, PawnPromotion, PassedPawnPush
 * - Piece Motifs: RookLift, Fianchetto, Outpost, Centralization, PieceLift
 * - King Motifs: KingStep, Castling
 * - Tactical Motifs: Check, Capture, Pin, Fork, Skewer, DiscoveredAttack, DoubleCheck
 * - Structural Motifs: DoubledPieces, Battery, IsolatedPawn, BackwardPawn, PassedPawn
 * - Endgame Motifs: Opposition, Zugzwang, Fortress
 */
sealed trait Motif:
  def plyIndex: Int
  def color: Color
  def move: Option[String] // UCI anchor if applicable
  def category: MotifCategory

enum MotifCategory:
  case Pawn, Piece, King, Tactical, Structural, Endgame, Positional

object Motif:

  /**
   * Normalizes rank based on side: 
   * White Rank 1-8 stays 1-8. 
   * Black Rank 1-8 becomes 8-1.
   */
  def relativeRank(rank: Int, color: Color): Int =
    if (color.white) rank else 9 - rank

  /** Simple pawn advance (non-capture) */
  case class PawnAdvance(
      file: File,
      fromRank: Int,
      toRank: Int,
      color: Color,
      plyIndex: Int,
      move: Option[String]
  ) extends Motif:
    val category = MotifCategory.Pawn
    def relativeFrom: Int = relativeRank(fromRank, color)
    def relativeTo: Int = relativeRank(toRank, color)
    def isDoubleStep: Boolean = (relativeTo - relativeFrom).abs == 2

  /** Pawn break - pawn capture that opens a file or challenges pawn structure */
  case class PawnBreak(
      file: File,
      targetFile: File,
      color: Color,
      plyIndex: Int,
      move: Option[String]
  ) extends Motif:
    val category = MotifCategory.Pawn

  /** Pawn promotion */
  case class PawnPromotion(
      file: File,
      promotedTo: Role,
      color: Color,
      plyIndex: Int,
      move: Option[String]
  ) extends Motif:
    val category = MotifCategory.Pawn
    def isUnderpromotion: Boolean = promotedTo != Queen

  /** Pushing a passed pawn toward promotion */
  case class PassedPawnPush(
      file: File,
      toRank: Int,
      color: Color,
      plyIndex: Int,
      move: Option[String]
  ) extends Motif:
    val category = MotifCategory.Pawn
    def relativeTo: Int = relativeRank(toRank, color)

  /** Rook lift from back rank to 3rd/4th rank for attack */
  case class RookLift(
      file: File,
      fromRank: Int,
      toRank: Int,
      color: Color,
      plyIndex: Int,
      move: Option[String]
  ) extends Motif:
    val category = MotifCategory.Piece
    def relativeFrom: Int = relativeRank(fromRank, color)
    def relativeTo: Int = relativeRank(toRank, color)

  /** Bishop fianchetto (Bg2/Bb2 for White, Bg7/Bb7 for Black) */
  case class Fianchetto(
      side: FianchettoSide,
      color: Color,
      plyIndex: Int,
      move: Option[String]
  ) extends Motif:
    val category = MotifCategory.Piece

  enum FianchettoSide:
    case Kingside, Queenside

  /** Piece placed on a strong outpost (typically a knight on e5/d5) */
  case class Outpost(
      piece: Role,
      square: Square,
      color: Color,
      plyIndex: Int,
      move: Option[String]
  ) extends Motif:
    val category = MotifCategory.Piece

  /** Piece moving to central squares (d4, d5, e4, e5) */
  case class Centralization(
      piece: Role,
      square: Square,
      color: Color,
      plyIndex: Int,
      move: Option[String]
  ) extends Motif:
    val category = MotifCategory.Piece

  /** Generic piece lift (knight or bishop moving up the board for attack) */
  case class PieceLift(
      piece: Role,
      fromRank: Int,
      toRank: Int,
      color: Color,
      plyIndex: Int,
      move: Option[String]
  ) extends Motif:
    val category = MotifCategory.Piece

  /** King movement with strategic purpose */
  case class KingStep(
      stepType: KingStepType,
      color: Color,
      plyIndex: Int,
      move: Option[String]
  ) extends Motif:
    val category = MotifCategory.King

  enum KingStepType:
    case OffBackRank    // King leaves safety (risky or endgame activation)
    case ToCorner       // King retreats to corner for safety
    case Activation     // King marches toward center in endgame
    case Evasion        // King escapes from check or threat
    case Other

  /** Castling (short or long) */
  case class Castling(
      side: CastlingSide,
      color: Color,
      plyIndex: Int,
      move: Option[String]
  ) extends Motif:
    val category = MotifCategory.King

  enum CastlingSide:
    case Kingside, Queenside

  /** Check */
  case class Check(
      piece: Role,
      targetSquare: Square,
      checkType: CheckType,
      color: Color,      // Color of the checking side
      plyIndex: Int,
      move: Option[String]
  ) extends Motif:
    val category = MotifCategory.Tactical

  enum CheckType:
    case Normal, Discovered, Double, Mate, Smothered

  /** Capture (with sacrifice detection and ROI for exchange sacrifices) */
  case class Capture(
      piece: Role,
      captured: Role,
      square: Square,
      captureType: CaptureType,
      color: Color,      // Color of the capturing side
      plyIndex: Int,
      move: Option[String],
      sacrificeROI: Option[SacrificeROI] = None  // Exchange sacrifice compensation
  ) extends Motif:
    val category = MotifCategory.Tactical

  /** Sacrifice ROI - material sacrifice compensation */
  case class SacrificeROI(
      reasons: List[SacrificeReason],
      expectedValue: Int      // Estimated CP compensation
  )

  enum SacrificeReason:
    case OpenFile
    case KingExposed
    case BishopPair
    case PieceActivity
    case PassedPawn

  enum CaptureType:
    case Normal, Sacrifice, Exchange, Recapture, Winning, ExchangeSacrifice  // Added ExchangeSacrifice

  /** Zwischenzug - intermediate move instead of expected recapture */
  case class Zwischenzug(
      intermediateMove: String,
      threatType: ZwischenzugThreat,
      expectedRecaptureSquare: Square,
      color: Color,      // Color of the side playing the intermediate move
      plyIndex: Int,
      move: Option[String]
  ) extends Motif:
    val category = MotifCategory.Tactical

  enum ZwischenzugThreat:
    case Mate
    case Check
    case Fork
    case WinningCapture

  /** Pin - piece attacks through an enemy piece to a more valuable piece behind */
  case class Pin(
      pinningPiece: Role,
      pinnedPiece: Role,
      targetBehind: Role, // Usually King or Queen
      color: Color,
      plyIndex: Int,
      move: Option[String],
      pinningSq: Option[Square] = None,
      pinnedSq: Option[Square] = None,
      behindSq: Option[Square] = None
  ) extends Motif:
    val category = MotifCategory.Tactical
    def isAbsolutePin: Boolean = targetBehind == King

  /** Fork - piece attacks two or more pieces simultaneously */
  case class Fork(
      attackingPiece: Role,
      targets: List[Role],
      square: Square,
      targetSquares: List[Square], // Specific target squares
      color: Color,
      plyIndex: Int,
      move: Option[String]
  ) extends Motif:
    val category = MotifCategory.Tactical
    def isRoyalFork: Boolean = targets.contains(King) && targets.contains(Queen)
    def isDoubleAttack: Boolean = targets.size >= 2
    
  /** Domination - piece restricts an enemy piece's mobility significantly */
  case class Domination(
      dominatingPiece: Role,
      dominatedPiece: Role,
      square: Square,
      color: Color,
      plyIndex: Int,
      move: Option[String]
  ) extends Motif:
    val category = MotifCategory.Positional

  /** Maneuver - quiet move improving piece location */
  case class Maneuver(
      piece: Role,
      purpose: ManeuverPurpose,
      color: Color,
      plyIndex: Int,
      move: Option[String]
  ) extends Motif:
    val category = MotifCategory.Positional

  enum ManeuverPurpose:
    case Rerouting
    case ImprovingScope

  /** Skewer - attack through a valuable piece to a less valuable piece behind */
  case class Skewer(
      attackingPiece: Role,
      frontPiece: Role,
      backPiece: Role,
      color: Color,
      plyIndex: Int,
      move: Option[String],
      attackingSq: Option[Square] = None,
      frontSq: Option[Square] = None,
      backSq: Option[Square] = None
  ) extends Motif:
    val category = MotifCategory.Tactical

  /** Discovered attack - moving one piece reveals an attack from another */
  case class DiscoveredAttack(
      movingPiece: Role,
      attackingPiece: Role,
      target: Role,
      color: Color,
      plyIndex: Int,
      move: Option[String],
      movingSq: Option[Square] = None,
      attackingSq: Option[Square] = None,
      targetSq: Option[Square] = None
  ) extends Motif:
    val category = MotifCategory.Tactical
    def isDiscoveredCheck: Boolean = target == King

  /** Removing the Defender - capturing a piece that defended a critical target */
  case class RemovingTheDefender(
      attacker: Role,
      victim: Role,
      protectedTarget: Role,
      square: Square,
      color: Color,
      plyIndex: Int,
      move: Option[String]
  ) extends Motif:
    val category = MotifCategory.Tactical

  /** Deflection - forcing a piece away from a critical defensive duty */
  case class Deflection(
      piece: Role,
      fromSquare: Square,
      color: Color,
      plyIndex: Int,
      move: Option[String]
  ) extends Motif:
    val category = MotifCategory.Tactical

  /** Decoy - luring a piece to a vulnerable square */
  case class Decoy(
      piece: Role,
      toSquare: Square,
      color: Color,
      plyIndex: Int,
      move: Option[String]
  ) extends Motif:
    val category = MotifCategory.Tactical

  /** XRay - attacking a target through another piece */
  case class XRay(
      piece: Role,
      target: Role,
      square: Square,
      color: Color,
      plyIndex: Int,
      move: Option[String]
  ) extends Motif:
    val category = MotifCategory.Tactical

  /** Overloading - a piece is defending too many things at once */
  case class Overloading(
      overloadedPiece: Role,
      overloadedSquare: Square,
      duties: List[Square], // Squares the piece is trying to defend
      color: Color,
      plyIndex: Int,
      move: Option[String]
  ) extends Motif:
    val category = MotifCategory.Tactical
    def dutyCount: Int = duties.size

  /** Double Check - both the moving piece and revealed piece give check */
  case class DoubleCheck(
      movingPiece: Role,
      revealedPiece: Role,
      color: Color,
      plyIndex: Int,
      move: Option[String]
  ) extends Motif:
    val category = MotifCategory.Tactical

  /** Back Rank Mate threat or execution */
  case class BackRankMate(
      mateType: BackRankMateType,
      attackingPiece: Role,
      color: Color,
      plyIndex: Int,
      move: Option[String]
  ) extends Motif:
    val category = MotifCategory.Tactical

  enum BackRankMateType:
    case Threat, Execution

  /** Trapped Piece - a piece has no safe squares to escape */
  case class TrappedPiece(
      trappedRole: Role,
      trappedSquare: Square,
      color: Color, // Color of the trapped piece (opponent's color)
      plyIndex: Int,
      move: Option[String]
  ) extends Motif:
    val category = MotifCategory.Tactical
    def isValuableTrap: Boolean = trappedRole == Queen || trappedRole == Rook

  /** Mate Net - king trapped in a web of threats */
  case class MateNet(
      kingSquare: Square,
      attackers: List[Role],
      color: Color, // Attacking side
      plyIndex: Int,
      move: Option[String]
  ) extends Motif:
    val category = MotifCategory.Tactical

  /*
  case class PerpetualCheck(
      color: Color, // Side giving checks
      plyIndex: Int,
      move: Option[String]
  ) extends Motif:
    val category = MotifCategory.Tactical
  */

  /** Initiative - seizing/maintaining pressure */
  case class Initiative(
      color: Color,
      score: Int, // Heuristic strength
      plyIndex: Int,
      move: Option[String]
  ) extends Motif:
    val category = MotifCategory.Positional

  /** Interference - blocking the coordination between enemy pieces */
  case class Interference(
      interferingPiece: Role,
      interferingSquare: Square,
      blockedPiece1: Role,
      blockedPiece2: Role,
      color: Color,
      plyIndex: Int,
      move: Option[String]
  ) extends Motif:
    val category = MotifCategory.Tactical

  /** Clearance - moving a piece to clear a line/square for another piece */
  case class Clearance(
      clearingPiece: Role,
      clearingFrom: Square,
      clearedLine: ClearanceType,
      beneficiary: Role,      // The piece that benefits from the cleared line
      color: Color,
      plyIndex: Int,
      move: Option[String]
  ) extends Motif:
    val category = MotifCategory.Tactical

  enum ClearanceType:
    case File, Rank, Diagonal, Square

  /** Doubled pieces on a file or rank */
  case class DoubledPieces(
      role: Role,
      file: File,
      color: Color,
      plyIndex: Int,
      move: Option[String] = None
  ) extends Motif:
    val category = MotifCategory.Structural

  /** Battery - two pieces aligned on same line (e.g., Q+B on diagonal) */
  case class Battery(
      front: Role,
      back: Role,
      axis: BatteryAxis,
      color: Color,
      plyIndex: Int,
      move: Option[String] = None,
      frontSq: Option[Square] = None,
      backSq: Option[Square] = None,
      targetSq: Option[Square] = None,
      targetRole: Option[Role] = None
  ) extends Motif:
    val category = MotifCategory.Structural

  enum BatteryAxis:
    case File, Rank, Diagonal

  /** Isolated pawn (no friendly pawns on adjacent files) */
  case class IsolatedPawn(
      file: File,
      rank: Int,
      color: Color,
      plyIndex: Int,
      move: Option[String] = None
  ) extends Motif:
    val category = MotifCategory.Structural

  /** Backward pawn (cannot be supported by other pawns) */
  case class BackwardPawn(
      file: File,
      rank: Int,
      color: Color,
      plyIndex: Int,
      move: Option[String] = None
  ) extends Motif:
    val category = MotifCategory.Structural

  /** Passed pawn (no enemy pawns can block or capture it) */
  case class PassedPawn(
      file: File,
      rank: Int,
      color: Color,
      isProtected: Boolean,
      plyIndex: Int,
      move: Option[String] = None
  ) extends Motif:
    val category = MotifCategory.Structural

  /** Doubled pawns (two pawns on same file) */
  case class DoubledPawns(
      file: File,
      color: Color,
      plyIndex: Int,
      move: Option[String] = None
  ) extends Motif:
    val category = MotifCategory.Structural

  /** Pawn chain */
  case class PawnChain(
      baseFile: File,
      tipFile: File,
      color: Color,
      plyIndex: Int,
      move: Option[String] = None
  ) extends Motif:
    val category = MotifCategory.Structural

  /** Opposition - kings face each other with odd number of squares between */
  case class Opposition(
      opponentKingSquare: Square,
      ownKingSquare: Square,
      oppType: OppositionType,
      color: Color,
      plyIndex: Int,
      move: Option[String] = None
  ) extends Motif:
    val category = MotifCategory.Endgame

  enum OppositionType:
    case Direct, Distant, Diagonal

  case class Zugzwang(
      color: Color, // Side in zugzwang
      plyIndex: Int,
      move: Option[String] = None
  ) extends Motif:
    val category = MotifCategory.Endgame

  /*
  case class Fortress(
      color: Color, // Defending side
      plyIndex: Int,
      move: Option[String] = None
  ) extends Motif:
    val category = MotifCategory.Endgame
  */

  case class StalemateThreat(
      color: Color,
      plyIndex: Int,
      move: Option[String] = None
  ) extends Motif:
    val category = MotifCategory.Endgame

  /** Open file control - rook/queen on an open file */
  case class OpenFileControl(
      file: File,
      color: Color,
      plyIndex: Int,
      move: Option[String] = None
  ) extends Motif:
    val category = MotifCategory.Positional

  /** Semi-open file control - rook/queen on a file with only enemy pawns */
  case class SemiOpenFileControl(
      file: File,
      color: Color,
      plyIndex: Int,
      move: Option[String] = None
  ) extends Motif:
    val category = MotifCategory.Positional

  /** Seventh Rank Invasion - rook on technical 7th rank pressure */
  case class SeventhRankInvasion(
      color: Color,
      plyIndex: Int,
      move: Option[String] = None
  ) extends Motif:
    val category = MotifCategory.Positional

  /** Rook Behind Passed Pawn - Tarrasch rule */
  case class RookBehindPassedPawn(
      file: File,
      color: Color,
      plyIndex: Int,
      move: Option[String] = None
  ) extends Motif:
    val category = MotifCategory.Positional

  /** King Cut-Off - Rook/Queen restricting enemy king in endgame */
  case class KingCutOff(
      axis: KingCutOffAxis,
      coordinate: Int,
      color: Color,
      plyIndex: Int,
      move: Option[String] = None
  ) extends Motif:
    val category = MotifCategory.Positional

  enum KingCutOffAxis:
    case Rank
    case File

  /** Weak back rank - king trapped on back rank */
  case class WeakBackRank(
      color: Color,
      plyIndex: Int,
      move: Option[String] = None
  ) extends Motif:
    val category = MotifCategory.Positional

  /** Space advantage - significant central pawn presence */
  case class SpaceAdvantage(
      color: Color,
      pawnDelta: Int,
      plyIndex: Int,
      move: Option[String] = None
  ) extends Motif:
    val category = MotifCategory.Positional

  /** Knight vs Bishop Imbalance */
  case class KnightVsBishop(
      color: Color, // sideWithKnight
      isKnightBetter: Boolean, // heuristic
      plyIndex: Int,
      move: Option[String] = None
  ) extends Motif:
    val category = MotifCategory.Positional

  /** Blockade - Piece stopping a passed pawn */
  case class Blockade(
      piece: Role,
      square: Square,
      pawnSquare: Square,
      color: Color,
      plyIndex: Int,
      move: Option[String] = None
  ) extends Motif:
    val category = MotifCategory.Positional

  /** Smothered Mate - Knight mate against suffocated King */
  case class SmotheredMate(
      color: Color,
      kingSquare: Square,
      plyIndex: Int,
      move: Option[String]
  ) extends Motif:
    val category = MotifCategory.Tactical
