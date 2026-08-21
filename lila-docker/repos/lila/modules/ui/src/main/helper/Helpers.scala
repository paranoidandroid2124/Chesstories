package lila.ui

import play.api.i18n.Lang
import java.time.Month
import java.time.ZoneOffset
import java.time.format.DateTimeFormatter
import java.time.format.TextStyle
import scala.annotation.unused
import scalatags.Text.all.*

trait Helpers:
  private val timeDatetime = attr("datetime")
  private val absoluteDateFormatter =
    DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm 'UTC'").withZone(ZoneOffset.UTC)
  private def classNames(values: Option[String]*): String = values.flatten.mkString(" ")

  def iconTag(icon: Icon): Tag = i(attr("data-icon") := icon.value)
  def iconTag(icon: Icon, text: Frag): Tag = i(attr("data-icon") := icon.value, cls := "text")(text)
  def showMonth(m: Month)(using lang: Lang): String =
    m.getDisplayName(TextStyle.FULL, lang.locale)

  private def timeagoTag(d: java.time.Instant, preload: Boolean): Tag =
    tag("time")(
      cls := classNames(Some("timeago"), preload.option("timeago--preload")),
      timeDatetime := d.toString
    )(showDate(d))

  def chessgroundMini(fen: String | chess.format.BoardFen, color: chess.Color, lastMove: Option[chess.format.Uci]): Tag => Tag = t =>
    t(
      cls := "mini-board mini-board--init pv-line",
      attr("data-fen") := fen.toString,
      attr("data-color") := color.name,
      attr("data-lastmove") := lastMove.map(_.uci).getOrElse(""),
      attr("data-state") := s"${fen.toString},${color.name},${lastMove.map(_.uci).getOrElse("")}"
    )
  def userIdLink(
      userId: Option[lila.core.userId.UserId],
      withOnline: Boolean = true,
      klass: Option[String] = None
  ): Tag =
    userId.fold[Tag](span(cls := "user-link user-link--anonymous")("Anonymous")): userId =>
      val baseClasses = userClass(userId, withOnline).split(' ').filter(_.nonEmpty).toList
      val classes = (baseClasses ++ klass.toList).mkString(" ")
      a(cls := classes, href := s"/@/${userId.value}")(userId.value)
  def aiNameFrag(level: Int): Frag = frag(s"AI level $level")
  def routeUrl(call: play.api.mvc.Call): String = call.url

  def submitButton(mods: Modifier*): Tag = button(tpe := "submit", mods)
  def postForm(mods: Modifier*): Tag = form(method := "post", mods)
  def copyMeInput(v: String): Tag = input(cls := "copyable", value := v, readonly)

  def momentFromNow(d: java.time.Instant): Frag = timeagoTag(d, preload = false)
  def titleNameOrId(userId: lila.core.userId.UserId): Frag = userIdLink(Some(userId), withOnline = false)
  def pagerNext(pager: scalalib.paginator.Paginator[?], url: Int => String): Frag =
    if pager.currentPage < pager.nbPages then
      val nextPage = pager.currentPage + 1
      div(cls := "pager", attr("aria-label") := s"Pagination, page ${pager.currentPage} of ${pager.nbPages}")(
        span(cls := "pager__status")(s"Page ${pager.currentPage} of ${pager.nbPages}"),
        a(cls := "button button-empty pager__next", href := url(nextPage), rel := "next")("Load more")
      )
    else frag()
  def addQueryParam(url: String, name: String, value: String): String =
    val separator = if (url.contains("?")) "&" else "?"
    s"$url$separator$name=$value"

  def chessgroundBoard: Tag = div(cls := "cg-wrap")(raw(""))

  private def playerNameTag(p: lila.core.game.Player): Tag =
    p.aiLevel match
      case Some(level) => span(cls := "player-link player-link--ai")(aiNameFrag(level))
      case None =>
        p.userId match
          case Some(id) => userIdLink(Some(id), withOnline = false)
          case None =>
            p.name.fold(span(cls := "player-link player-link--anonymous")("Anonymous")): name =>
              span(cls := "player-link")(name.value)

  def playerText(p: lila.core.game.Player): Tag = playerNameTag(p)
  def userClass(@unused id: lila.core.userId.UserId, withOnline: Boolean = false): String =
    classNames(Some("user-link"), withOnline.option("user-link--online"))
  def lineIcon: Tag = span(cls := "line-icon")
  def showDate(d: java.time.Instant): String = absoluteDateFormatter.format(d)

  object form3:
    def submit(text: Frag, icon: Option[Icon] = None): Tag =
      button(tpe := "submit", cls := "button")(icon.fold(frag())(iconTag), text)

object Helpers extends Helpers
