package lila.pref

import monocle.syntax.all.*
import play.api.mvc.RequestHeader

object RequestPref:

  import Pref.default

  private def queryBg(req: RequestHeader): Option[Int] =
    queryParam(req.queryString, "bg")
      .flatMap(Pref.Bg.fromString.get)

  private def applyBg(pref: Pref, bg: Int): Pref =
    val updated = pref.copy(bg = bg)
    if updated.bg == Pref.Bg.DARKBOARD then
      updated.copy(bg = Pref.Bg.DARK).focus(_.board.brightness).replace(60)
    else updated

  def queryParamOverride(req: RequestHeader)(pref: Pref): Pref =
    queryBg(req).fold(pref)(applyBg(pref, _))

  def fromRequest(req: RequestHeader): Pref =
    val qs = req.queryString
    if qs.isEmpty then default
    else
      def param(name: String): Option[String] = queryParam(qs, name)
      val pref = default.copy(
        bg = queryBg(req) | default.bg,
        theme = param("theme") | default.theme,
        theme3d = param("theme3d") | default.theme3d,
        pieceSet = param("pieceSet") | default.pieceSet,
        pieceSet3d = param("pieceSet3d") | default.pieceSet3d,
        soundSet = param("soundSet") | default.soundSet,
        bgImg = param("bgImg"),
        is3d = param("is3d").has("true"),
        board = default.board.copy(
          opacity = param("boardOpacity").flatMap(_.toIntOption) | default.board.opacity,
          brightness = param("boardBrightness").flatMap(_.toIntOption) | default.board.brightness,
          hue = param("boardHue").flatMap(_.toIntOption) | default.board.hue
        )
      )
      applyBg(pref, pref.bg)

  private def queryParam(queryString: Map[String, Seq[String]], name: String): Option[String] =
    queryString
      .get(name)
      .flatMap(_.headOption)
      .filter: v =>
        v.nonEmpty && v != "auto"
