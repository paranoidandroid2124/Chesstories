package lila.chessjudgment.model.line

import play.api.libs.json.*

/**
 * Engine transport variation admitted before review assembly.
 *
 * `scoreCp` and `mate` are always from White's perspective. Consumers that reason
 * from the side-to-move's perspective must use `PerspectiveMath`
 * instead of materializing another line type with copied moves and metadata.
 */
case class EngineLine(
    moves: List[String],            // Raw UCI moves
    scoreCp: Int,
    mate: Option[Int] = None,
    depth: Int = 0
)

object EngineLine:
  given Reads[EngineLine] = Json.reads[EngineLine]
  given Writes[EngineLine] = Json.writes[EngineLine]
