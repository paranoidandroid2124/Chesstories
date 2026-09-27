package views.pages

import lila.app.UiEnv.*
import lila.ui.{ Esm, Page }
import scala.annotation.unused

object landing:

  def apply()(using @unused ctx: Context): Page =
    Page("Chesstory - Move-by-move chess explanations")
      .css("landing")
      .js(Esm("site.reviewExample"))
      .wrap: _ =>
        main(cls := "landing")(
          st.section(cls := "landing__intro")(
            div(cls := "landing__grid")(
              div(cls := "landing__copy")(
                span(cls := "landing__eyebrow")("A little clarity. Every move."),
                h1("Understand why the game changed"),
                p(
                  "See what your move changed, explore the alternative, and take one useful idea into your next game."
                ),
                div(cls := "landing__actions")(
                  a(href := routes.UserAnalysis.index.url, cls := "button landing__primary")(
                    "Review a game"
                  ),
                  a(href := "#review-example", cls := "landing__secondary")("Try the example ↓")
                )
              ),
              ReviewExample.widget
            )
          ),
          st.section(cls := "landing__routes", aria.label := "What the review explains")(
            h2("What the review explains"),
            ol(
              route(
                "01",
                "Played vs reference",
                "Compare the move on the board with a concrete alternative."
              ),
              route(
                "02",
                "Reason for the difference",
                "See what each move changes and why the order matters."
              ),
              route("03", "Proof you can replay", "Follow the supporting line and keep the reusable lesson.")
            )
          )
        )

  private def route(index: String, title: String, body: String): Frag =
    li(cls := "landing__route")(
      span(cls := "landing__route-index")(index),
      div(
        strong(title),
        span(body)
      )
    )
