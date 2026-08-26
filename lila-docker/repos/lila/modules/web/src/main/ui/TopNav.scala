package lila.web
package ui

import lila.ui.Helpers
import lila.ui.ScalatagsTemplate.{ *, given }
import ShellPrimitives.*

case class TopNav(helpers: Helpers):
  def apply(using ctx: lila.ui.PageContext): Frag =
    def isOn(prefix: String) = ctx.req.path == prefix || ctx.req.path.startsWith(prefix + "/")

    def item(
        hrefUrl: String,
        label: String,
        active: Boolean,
        titleText: Option[String] = None,
        mobileOnly: Boolean = false
    ) =
      li(cls := List("mobile-only" -> mobileOnly))(
        a(
          href := hrefUrl,
          cls := List(
            navLinkClass -> true,
            "is-active" -> active
          ),
          aria("current") := Option.when(active)("page"),
          title := titleText.getOrElse(label),
          aria.label := titleText.getOrElse(label)
        )(label)
      )

    st.nav(id := navId, aria.label := "Primary")(
      ul(cls := navMainClass)(
        item("/", "Home", ctx.req.path == "/", mobileOnly = true),
        item("/analysis", "Review", isOn("/analysis"), Some("Review a game move by move")),
        item("/study", "Study", isOn("/study")),
        item("/import", "Import Games", isOn("/import"), Some("Import recent public games")),
        item("/help", "Help", isOn("/help")),
        item("/journal", "Journal", isOn("/journal"))
      )
    )
