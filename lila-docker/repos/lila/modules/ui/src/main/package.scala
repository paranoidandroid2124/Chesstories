package lila.ui

import play.api.libs.json.JsValue
import scalatags.Text.all.Frag

import lila.core.data.SafeJsonStr

// Import core lilaism extensions including .some
export lila.core.lilaism.Lilaism.{ *, given }

case class PageModule(name: String, data: JsValue | SafeJsonStr)
case class Esm(key: String, init: WithNonce[Frag] = _ => ScalatagsExtensions.emptyFrag)
type EsmList = List[Option[Esm]]

final class AnalyseEndpoints(
    val explorer: String,
    val tablebase: String
)
