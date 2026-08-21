package views.pages

import lila.app.UiEnv.*
import lila.ui.Page
import scala.annotation.unused

object landing:

  private val heroFen = "r1bq1rk1/pppp1ppp/2n2n2/2b1p3/2B1P3/3P1N2/PPP2PPP/RNBQ1RK1 w - - 4 6"

  def prelaunch()(using @unused ctx: Context): Page =
    Page("Chesstory - Chess review on the board")
      .css("landing")
      .wrap: _ =>
        main(cls := "landing")(
          st.section(cls := "landing__intro")(
            div(cls := "landing__grid")(
              div(cls := "landing__copy")(
                h1("Review a game on the board"),
                p(
                  "Load a game, keep the board and engine line in view, and work from the move that changed it. " +
                    "Save the parts worth returning to as a study."
                ),
                div(cls := "landing__actions")(
                  a(href := routes.UserAnalysis.index.url, cls := "button landing__primary")(
                    "Analyze a game"
                  ),
                  a(href := routes.Main.examples.url, cls := "landing__secondary")("View examples")
                )
              ),
              st.aside(cls := "landing__position", aria.label := "Current position preview")(
                board,
                div(cls := "landing__position-meta")(
                  span("Current move"),
                  strong("6. h3"),
                  p("White to move • Italian Game")
                ),
                div(cls := "landing__line")(
                  span("Reference"),
                  code("6. c3 d6 7. d4")
                )
              )
            )
          ),
          st.section(cls := "landing__routes", aria.label := "Review workflow")(
            h2("Choose a starting point"),
            ol(
              route("01", "Paste a PGN", "Open a full game directly on the analysis board."),
              route("02", "Import a public game", "Choose one recent game from Lichess or Chess.com."),
              route("03", "Continue a study", "Return to a saved chapter, variation, or note.")
            )
          ),
          st.section(cls := "landing__facts", aria.label := "Product facts")(
            div(cls := "landing__fact")(
              strong("Board first"),
              span("Moves, candidate lines, and explanations stay beside the board.")
            ),
            div(cls := "landing__fact")(
              strong("Engine visible"),
              span("Evaluation and principal variations remain visible.")
            ),
            div(cls := "landing__fact")(
              strong("Study when useful"),
              span("Save a line to reopen or share it.")
            )
          )
        )

  private def board: Frag =
    chessgroundMini(heroFen, chess.White, None)(
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
