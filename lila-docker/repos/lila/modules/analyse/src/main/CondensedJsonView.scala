package lila.analyse

import play.api.libs.json.*
import lila.tree.Node
import lila.core.game.Pov

object CondensedJsonView:

  def apply(
      pov: Pov,
      root: Node,
      pref: lila.core.pref.Pref
  ): JsObject =
    Json.obj(
      "treeParts" -> Json.arr(Node.defaultNodeJsonWriter.writes(root)),
      "orientation" -> pov.color.name,
      "userAnalysis" -> true,
      "game" -> Json.obj(
        "id" -> "synthetic",
        "variant" -> Json.obj(
          "key" -> pov.game.variant.key.value,
          "name" -> pov.game.variant.name
        ),
        "player" -> pov.color.name,
        "status" -> Json.obj("id" -> 20, "name" -> "started"),
        "fen" -> chess.format.Fen.write(pov.game.chess).value,
        "turns" -> pov.game.chess.ply.value,
        "source" -> "ai",
        "speed" -> "correspondence",
        "perf" -> "bullet",
        "rated" -> false
      ),
      "player" -> Json.obj("color" -> pov.color.name),
      "opponent" -> Json.obj("color" -> (!pov.color).name),
      "pref" -> Json.obj(
        // AnalysePref (ui/analyse/src/interfaces.ts)
        "coords" -> pref.coords,
        "highlight" -> pref.highlight,
        "destination" -> pref.destination,
        "rookCastle" -> (pref.rookCastle != 0),
        "animationDuration" -> pref.animationMillis,
        "moveEvent" -> pref.moveEvent
      ),
      "takebackable" -> false,
      "moretimeable" -> false
    )
