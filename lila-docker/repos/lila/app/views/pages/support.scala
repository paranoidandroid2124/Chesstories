package views.pages

import lila.app.UiEnv.*
import lila.ui.Page
import scala.annotation.unused

object support:
  def apply(
      patreon: Option[String],
      githubSponsors: Option[String],
      buyMeACoffee: Option[String]
  )(using @unused ctx: Context): Page =
    val links = List(
      "Patreon" -> patreon,
      "GitHub Sponsors" -> githubSponsors,
      "Buy Me a Coffee" -> buyMeACoffee
    ).collect { case (label, Some(url)) => label -> url }

    Page("Support Chesstory")
      .css("legal")
      .wrap: _ =>
        main(cls := "legal-page")(
          div(cls := "legal-container")(
            st.article(cls := "legal-content")(
              header(cls := "legal-header")(
                h1("Support Chesstory"),
                p(cls := "legal-meta")("Core board study stays open. Deeper review paths still follow current usage limits.")
              ),
              st.section(cls := "legal-section")(
                h2("Help improve Chesstory"),
                p("Tell us about a move explanation that helped, a confusing interaction, or a problem you ran into. Your feedback helps us choose what to improve next."),
                div(cls := "legal-actions")(
                  a(
                    href := routes.BetaFeedback.formPage(
                      surface = "general",
                      feature = "support_page",
                      entrypoint = "support_page",
                      returnTo = routes.Main.support.url,
                      notify = false
                    ).url,
                    cls := "button"
                  )("Share beta feedback")
                )
              ),
              links.nonEmpty.option(st.section(cls := "legal-section")(
                h2("Support the project"),
                p("Your support helps with hosting and maintenance."),
                ul(links.map { (label, url) => li(a(href := url, target := "_blank", rel := "noopener noreferrer")(label)) })
              )),
              details(cls := "legal-section")(
                summary("Current access and future plans"),
                p("There is no active paid subscription. Access follows the current beta limits."),
                a(href := routes.BetaFeedback.formPage(surface = "general", feature = "paid_plan_waitlist", entrypoint = "support_waitlist", returnTo = routes.Main.support.url, notify = true).url, cls := "legal-link")("Join paid-plan waitlist"),
                ul(
                  li("Anonymous full-game review is currently limited to 1 Game Chronicle request per day per IP."),
                  li("Signed-in free access is currently limited by current beta quota controls."),
                  li("Additional burst or fair-use controls may apply on higher-access beta tiers."),
                  li("Support does not bypass safety, abuse-prevention, or quota controls.")
                )
              ),
              footer(cls := "legal-footer")(
                a(href := homeUrl, cls := "legal-link")("Back to Home")
              )
            )
          )
        )
