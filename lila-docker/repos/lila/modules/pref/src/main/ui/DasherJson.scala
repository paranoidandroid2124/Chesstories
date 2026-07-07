package lila.pref
package ui

import play.api.libs.json.*

import lila.common.Json.given
import lila.core.perm.Granter
import lila.ui.Context

object DasherJson:

  def apply(pref: Pref, gallery: Option[JsValue])(using ctx: Context): JsObject =
    Json.obj(
      "user" -> ctx.me.map(_.light),
      "sound" -> Json.obj(
        "list" -> SoundSet.list.map { set =>
          s"${set.key} ${set.name}"
        }
      ),
      "background" -> Json
        .obj(
          "current" -> Pref.Bg.asString.get(pref.bg),
          "image" -> pref.bgImgOrDefault
        )
        .add("gallery", gallery),
      "board" -> Json.obj(
        "d2" -> Json.obj(
          "current" -> pref.currentTheme.name,
          "list" -> Theme.all
        )
      ),
      "piece" -> Json.obj(
        "d2" -> Json.obj(
          "current" -> pref.currentPieceSet.name,
          "list" -> PieceSet.all
        )
      ),
      "coach" -> Granter.opt(_.Coach)(using ctx.me)
    )
