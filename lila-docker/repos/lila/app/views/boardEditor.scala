package views

import chess.format.Fen
import play.api.libs.json.Json

import lila.app.UiEnv.{ * }
import lila.ui.PageModule

object boardEditor:

  def apply(fen: Option[Fen.Full])(using ctx: Context): Page =
    Page("Board editor")
      .css("editor")
      .flag(_.zoom)
      .js:
        PageModule(
          "editor",
          Json.obj(
            "baseUrl" -> routes.Editor.index.url,
            "fen" -> fen.map(_.value),
            "animation" -> Json.obj("duration" -> ctx.pref.animationMillis),
            "embed" -> false,
            "options" -> Json.obj("coordinates" -> true)
          )
        )
      .wrap: _ =>
        main(cls := "page-small")(
          div(id := "board-editor")
        )
