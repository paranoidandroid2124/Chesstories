package lila.chessjudgment.model.strategic

import play.api.libs.json.*

/**
 * Parsed PV Move with coordinate-level information.
 * This is the "first-class" representation of a move in the PV.
 */
case class PvMove(
    uci: String,                    // "d1b3"
    from: String,                   // "d1"
    to: String,                     // "b3"
    piece: String,                  // "Q" (uppercase = white, lowercase = black in standard)
    isCapture: Boolean,
    capturedPiece: Option[String],
    givesCheck: Boolean
)

object PvMove:
  given Reads[PvMove] = Json.reads[PvMove]
  given Writes[PvMove] = Json.writes[PvMove]

/**
 * Represents a variation line from engine analysis.
 */
case class VariationLine(
    moves: List[String],            // Raw UCI moves
    scoreCp: Int,
    mate: Option[Int] = None,
    depth: Int = 0,
    resultingFen: Option[String] = None,
    parsedMoves: List[PvMove] = Nil // Parsed moves with metadata
):
  /** Get our move (index 0) */
  def ourMove: Option[PvMove] = parsedMoves.headOption
  
  /** Get opponent's reply (index 1) */
  def theirReply: Option[PvMove] = parsedMoves.lift(1)

object VariationLine:
  given Reads[VariationLine] = Json.reads[VariationLine]
  given Writes[VariationLine] = Json.writes[VariationLine]
