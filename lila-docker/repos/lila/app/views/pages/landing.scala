package views.pages

import lila.app.UiEnv.*
import lila.ui.Page
import scala.annotation.unused

object landing:

  private val heroPieces = List(
    "wK" -> "g1",
    "wQ" -> "d1",
    "wR" -> "a1",
    "wR" -> "f1",
    "wB" -> "c1",
    "wB" -> "c4",
    "wN" -> "b1",
    "wN" -> "f3",
    "wP" -> "a2",
    "wP" -> "b2",
    "wP" -> "c2",
    "wP" -> "e4",
    "wP" -> "d3",
    "wP" -> "f2",
    "wP" -> "g2",
    "wP" -> "h2",
    "bK" -> "g8",
    "bQ" -> "d8",
    "bR" -> "a8",
    "bR" -> "f8",
    "bB" -> "c8",
    "bB" -> "c5",
    "bN" -> "c6",
    "bN" -> "f6",
    "bP" -> "a7",
    "bP" -> "b7",
    "bP" -> "c7",
    "bP" -> "d7",
    "bP" -> "e5",
    "bP" -> "f7",
    "bP" -> "g7",
    "bP" -> "h7"
  )

  def apply()(using @unused ctx: Context): Page =
    Page("Chesstory - Chess analysis that explains the plan")
      .css("landing")
      .flag(_.noHeader)
      .wrap: _ =>
        main(cls := "landing-pro")(
          landingHeader,
          hero,
          story,
          studyPayoff,
          landingFooter
        )

  private def landingHeader(using Context) =
    header(cls := "landing-header")(
      div(cls := "landing-container landing-header__inner")(
        a(href := routes.Main.landing.url, cls := "logo")("Chesstory"),
        st.nav(cls := "section-nav", aria.label := "Landing sections")(
          a(href := "#story", cls := "btn-text")("Story"),
          a(href := "#study", cls := "btn-text")("Study")
        ),
        div(cls := "auth-nav")(
          a(href := routes.Auth.login.url, cls := "btn-text")("Log in"),
          a(href := routes.UserAnalysis.index.url, cls := "btn-secondary landing-header-cta")("Analyze a game")
        )
      )
    )

  private def hero(using Context) =
    st.section(cls := "landing-hero")(
      div(cls := "landing-container landing-hero__grid")(
        div(cls := "landing-hero__copy")(
          p(cls := "eyebrow")("For players who want the plan, not only the number"),
          h1("Chess analysis that explains what the position is asking"),
          p(cls := "hero-summary")(
            "Paste a PGN or import a public game. Chesstory keeps Stockfish as evidence, " +
              "then turns the key position into human strategic explanation and a shareable review study."
          ),
          div(cls := "hero-cta")(
            a(href := routes.UserAnalysis.index.url, cls := "btn-primary")("Analyze a game"),
            a(href := routes.Importer.importGame.url, cls := "btn-secondary")("Import public game")
          ),
          p(cls := "hero-disclosure")("No playing lobby. No guesswork. One game, explained deeply.")
        ),
        div(cls := "landing-hero__visual")(
          div(cls := "motion-demo")(
            div(cls := "motion-demo__pgn")(
              span(cls := "motion-demo__label")("PGN"),
              code("1. e4 e5 2. Nf3 Nc6 3. Bc4 Bc5 4. O-O Nf6 5. d3 O-O")
            ),
            board("motion-demo__board"),
            div(cls := "motion-demo__engine")(
              span(cls := "motion-demo__label")("Stockfish"),
              strong("+0.37 depth 22"),
              engineLine("c3", "+0.37", true),
              engineLine("Nbd2", "+0.25", false),
              engineLine("a4", "+0.22", false)
            ),
            div(cls := "motion-demo__explain")(
              span(cls := "motion-demo__label")("Chesstory"),
              strong("White is choosing how to make d4 real."),
              p("h3 is playable at +0.20, but c3 first ties the next move to the central break the structure is asking for.")
            )
          )
        )
      )
    )

  private def story(using Context) =
    st.section(id := "story", cls := "landing-story")(
      div(cls := "landing-container")(
        div(cls := "section-heading section-heading--center")(
          p(cls := "section-kicker")("From moves to meaning"),
          h2("Watch a game turn into an explanation you can study"),
          p(
            "Raw game text becomes a board you can inspect. Stockfish shows the evidence. " +
              "Chesstory turns the key position into a plan, then keeps it as a review study."
          )
        )
      ),
      div(cls := "landing-cinema")(
        div(cls := "landing-container landing-cinema__grid")(
          div(cls := "cinema-stage", attr("aria-hidden") := "true")(
            div(cls := "cinema-stage__shell")(
              div(cls := "cinema-layer cinema-layer--pgn")(
                span(cls := "cinema-kicker")("Raw game"),
                code("[White \"You\"] [Black \"Opponent\"] 1. e4 e5 2. Nf3 Nc6 3. Bc4 Bc5 4. O-O Nf6 5. d3 O-O")
              ),
              board("cinema-layer cinema-layer--board"),
              div(cls := "cinema-layer cinema-layer--engine")(
                span(cls := "cinema-kicker")("Engine evidence"),
                strong("+0.37 / depth 22"),
                engineLine("c3", "+0.37", true),
                engineLine("Nbd2", "+0.25", false),
                engineLine("a4", "+0.22", false)
              ),
              div(cls := "cinema-layer cinema-layer--brief")(
                span(cls := "cinema-kicker")("Chesstory explanation"),
                explanationRow("Position question", "Can White prepare d4 without giving Black easy counterplay?"),
                explanationRow("Engine verdict", "Stockfish likes c3 best. h3 was checked separately at +0.20, about 0.17 lower."),
                explanationRow("Current move effect", "h3 prevents Bg4, but it spends a tempo on a problem Black has not forced yet."),
                explanationRow("Human idea", "When the plan is d4, prefer quiet moves that make the pawn break stronger.")
              ),
              div(cls := "cinema-layer cinema-layer--study")(
                span(cls := "cinema-kicker")("Review study"),
                strong("Your game vs opponent review"),
                p("Opening to middlegame transition"),
                span(cls := "study-link-pill")("/study/7kP2aQb/2rNf9xL")
              )
            )
          ),
          ol(cls := "cinema-steps")(
            storyStep(
              "01",
              "Paste or import a game",
              "Start from a PGN, a Lichess game, or a Chess.com public game. The story begins with the real moves.",
              "Raw input enters the analysis board."
            ),
            storyStep(
              "02",
              "Let the position appear",
              "The board, move list, and current position become the shared reference point for every explanation.",
              "The game is now something you can inspect."
            ),
            storyStep(
              "03",
              "Keep the engine as evidence",
              "Stockfish still matters. Here the verified WASM line is c3 at +0.37, then Nbd2 at +0.25 and a4 at +0.22.",
              "The numbers stay visible as evidence, not as the whole answer."
            ),
            storyStep(
              "04",
              "Recover the strategic logic",
              "Chesstory separates the move effect, best-move difference, human idea, and supporting line.",
              "The position starts to speak in chess language."
            ),
            storyStep(
              "05",
              "Save the explanation",
              "Turn a serious review into a study with sections, playable lines, notes, and a shareable link.",
              "A game becomes a review artifact."
            )
          )
        )
      )
    )

  private def studyPayoff(using Context) =
    st.section(id := "study", cls := "landing-section landing-study-payoff")(
      div(cls := "landing-container landing-study-payoff__grid")(
        div(cls := "study-payoff-copy")(
          p(cls := "section-kicker")("The final artifact"),
          h2("A review study is the page you can return to and share"),
          p(
            "Keep the board, candidate lines, and explanation together under a clear title, " +
              "then reopen it later or share it with someone who wants to see the reasoning."
          ),
          div(cls := "landing-actions")(
            a(href := routes.UserAnalysis.index.url, cls := "btn-primary")("Analyze a game"),
            a(href := routes.Importer.importGame.url, cls := "btn-secondary")("Import public game")
          )
        ),
        div(cls := "study-artifact")(
          div(cls := "study-artifact__cover")(
            span("Review study"),
            strong("Opening to middlegame transition"),
            p("Your game vs opponent")
          ),
          div(cls := "study-artifact__body")(
            explanationRow("Position question", "Can White prepare d4 without losing time?"),
            explanationRow("Best vs played", "c3 supports the break; h3 answers a pin that was not urgent yet."),
            explanationRow("Lesson", "Before making a useful move, ask whether it advances the plan the structure wants.")
          )
        )
      )
    )

  private def landingFooter(using Context) =
    footer(cls := "landing-footer")(
      div(cls := "landing-container footer-links")(
        span("2026 Chesstory"),
        span(
          a(href := routes.Main.privacy.url)("Privacy"),
          " / ",
          a(href := routes.Main.terms.url)("Terms"),
          " / ",
          a(href := routes.Main.contact.url)("Contact"),
          " / ",
          a(href := routes.Main.source.url)("Open Source"),
          " / ",
          a(href := "#cookie-consent", cls := "js-cookie-consent-open")("Cookie settings")
        )
      )
    )

  private def board(extraCls: String): Frag =
    val pieces = heroPieces.map { case (piece, square) =>
      span(cls := s"landing-piece landing-piece--$piece landing-piece--$square")()
    }
    div(cls := s"landing-board $extraCls")(
      (div(cls := "landing-board__grid") :: pieces)*
    )

  private def engineLine(move: String, eval: String, active: Boolean): Frag =
    div(cls := s"engine-line${if active then " is-active" else ""}")(
      span(move),
      strong(eval)
    )

  private def explanationRow(label: String, body: String): Frag =
    div(cls := "explain-row")(
      span(label),
      p(body)
    )

  private def storyStep(index: String, title: String, copy: String, output: String): Frag =
    li(cls := "cinema-step")(
      span(cls := "step-index")(index),
      h3(title),
      p(copy),
      div(cls := "step-output")(output)
    )
