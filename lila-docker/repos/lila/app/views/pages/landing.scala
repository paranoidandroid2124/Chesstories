package views.pages

import lila.app.UiEnv.*
import lila.ui.Page
import scala.annotation.unused

object landing:

  def apply()(using @unused ctx: Context): Page =
    Page("Chesstory - Move-by-move chess explanations")
      .css("landing")
      .wrap: _ =>
        main(cls := "landing")(
          st.section(cls := "landing__intro")(
            div(cls := "landing__grid")(
              div(cls := "landing__copy")(
                h1("Understand why the game changed"),
                p(
                  "Review it move by move. Compare what was played with the reference line, follow the reason " +
                    "for the move order, and keep the lesson for the next similar position."
                ),
                div(cls := "landing__actions")(
                  a(href := routes.UserAnalysis.index.url, cls := "button landing__primary")(
                    "Review move by move"
                  ),
                  a(href := routes.Main.examples.url, cls := "landing__secondary")("See an explained example")
                )
              ),
              st.aside(cls := "landing__position", aria.label := "Explained position preview")(
                board,
                div(cls := "landing__position-meta")(
                  span("Move under review"),
                  strong(ReviewExample.move),
                  p(s"White to move • ${ReviewExample.opening}")
                ),
                div(cls := "landing__line")(
                  span("Why this sequence"),
                  p(ReviewExample.explanation),
                  code(s"Reference: ${ReviewExample.line}")
                )
              )
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

  private def board: Frag =
    chessgroundMini(ReviewExample.fen, chess.White, None)(
      div(cls := "landing-board", role := "img", aria.label := "Italian Game position after 5...O-O")
    )

  private def route(index: String, title: String, body: String): Frag =
    li(cls := "landing__route")(
      span(cls := "landing__route-index")(index),
      div(
        strong(title),
        span(body)
      )
    )
