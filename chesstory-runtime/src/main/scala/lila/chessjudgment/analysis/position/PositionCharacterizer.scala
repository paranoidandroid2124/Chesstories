package lila.chessjudgment.analysis.position

import _root_.chess.*
import lila.chessjudgment.analysis.evaluation.{ JudgmentThresholds, PerspectiveMath }
import lila.chessjudgment.model.*

/**
 * Characterizes the nature of the position using board feature inputs.
 * Acts as an position nature assessment, consuming insights from PositionFeatures and Eval.
 */
object PositionCharacterizer:

  /**
   * Characterizes the position using aggregated context.
   * 
   * @param pos     The raw board state.
   * @param features  Board strategic features (e.g. King Safety, Pawn Structure).
   * @param whitePovEvalCp Engine Evaluation (centipawns) from white's perspective.
   * @param material  Typed material imbalance features.
   */
  def characterize(
    pos: Position,
    features: List[lila.chessjudgment.model.strategic.PositionalTag],
    whitePovEvalCp: Option[Int],
    material: MaterialImbalanceFeatures
  ): PositionNature =
    val tension = calculateTension(pos)
    val fluidity = calculateFluidity(pos.board)
    val kingSafetyPenalty = calculateKingSafetyPenalty(features)
    val isImbalanced = material.isImbalanced
    
    // Adjusted Tension: Raw attacks + King Danger
    val adjTension = Math.min(1.0, tension + kingSafetyPenalty)
    
    val decisiveWinPercentEdge =
      whitePovEvalCp
        .map(cp => PerspectiveMath.absoluteWhiteWinPercentEdge(cp))
        .getOrElse(0.0)
    val isDecided = decisiveWinPercentEdge >= JudgmentThresholds.DECISIVE_EDGE_WP
    
    val natureType = 
      if (isDecided) 
        if (fluidity > 0.5) NatureType.Dynamic else NatureType.Static
      else if (adjTension > 0.6 || (isImbalanced && adjTension > 0.4)) NatureType.Chaos
      else if (adjTension > 0.3 || fluidity > 0.5) NatureType.Dynamic
      else if (fluidity < 0.2 && adjTension < 0.2) NatureType.Static
      else NatureType.Transition

    PositionNature(
      natureType = natureType,
      tension = adjTension,
      stability = calculateStability(adjTension, fluidity, isDecided)
    )

  /**
   * Tension is based on the number of active attacks between pieces of opposite colors.
   */
  private def calculateTension(pos: Position): Double =
    val board = pos.board
    val occupied = board.occupied
    
    var attackCount = 0
    board.foreach { (color, role, sq) =>
      val targets: Bitboard = role match
        case Pawn   => sq.pawnAttacks(color)
        case Knight => sq.knightAttacks
        case Bishop => sq.bishopAttacks(occupied)
        case Rook   => sq.rookAttacks(occupied)
        case Queen  => sq.queenAttacks(occupied)
        case King   => sq.kingAttacks
      
      attackCount += (targets & board.byColor(!color)).count
    }

    val pieceCount = board.nbPieces
    if (pieceCount == 0) 0.0
    else Math.min(1.0, attackCount.toDouble / (pieceCount.toDouble * 1.5))

  /**
   * Fluidity is based on open pawn structures.
   */
  private def calculateFluidity(board: Board): Double =
    val pawns = board.pawns
    val totalPawns = pawns.count
    if (totalPawns == 0) return 1.0
    
    val centerFiles = List(File.C, File.D, File.E, File.F)
    val openFilesCount = centerFiles.count(f => (pawns & Bitboard.file(f)).isEmpty)
    
    openFilesCount.toDouble / centerFiles.size
    
  private def calculateKingSafetyPenalty(features: List[lila.chessjudgment.model.strategic.PositionalTag]): Double =
    // Boost tension if King is exposed or under attack
    features.find {
      case lila.chessjudgment.model.strategic.PositionalTag.MateNet(_) => true
      case lila.chessjudgment.model.strategic.PositionalTag.KingStuckCenter(_) => true
      case lila.chessjudgment.model.strategic.PositionalTag.WeakBackRank(_) => true
      case _ => false 
    } match
      case Some(_) => 0.3
      case None => 0.0
      
  private def calculateStability(tension: Double, fluidity: Double, isDecided: Boolean): Double =
    if (isDecided) 0.9 // Game is settled, high stability
    else 1.0 - (tension * 0.6 + fluidity * 0.4)
