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
          resources,
          landingFooter
        )

  def prelaunch()(using @unused ctx: Context): Page =
    Page("Chesstory - Launch updates")
      .css("landing")
      .flag(_.noHeader)
      .wrap: _ =>
        main(cls := "landing-pro prelaunch-page")(
          prelaunchHeader,
          prelaunchHero,
          prelaunchModes,
          prelaunchTransformation,
          prelaunchSignal,
          prelaunchFooter
        )

  private def landingHeader(using @unused ctx: Context) =
    header(cls := "landing-header")(
      div(cls := "landing-container landing-header__inner")(
        a(href := routes.Main.landing.url, cls := "logo")("Chesstory"),
        st.nav(cls := "section-nav", aria.label := "Landing sections")(
          a(href := "#story", cls := "btn-text")("Story"),
          a(href := "#study", cls := "btn-text")("Study"),
          a(href := "#resources", cls := "btn-text")("Resources")
        ),
        div(cls := "auth-nav")(
          a(href := routes.Auth.login.url, cls := "btn-text")("Log in"),
          a(href := routes.UserAnalysis.index.url, cls := "btn-secondary landing-header-cta")("Analyze a game")
        )
      )
    )

  private def prelaunchHeader(using @unused ctx: Context) =
    header(cls := "landing-header prelaunch-header")(
      div(cls := "landing-container landing-header__inner")(
        a(href := routes.Main.prelaunch.url, cls := "logo")("Chesstory"),
        div(cls := "auth-nav")(
          a(href := "#launch-notice", cls := "btn-secondary landing-header-cta")("Get updates")
        )
      )
    )

  private def prelaunchHero(using @unused ctx: Context) =
    st.section(cls := "landing-hero prelaunch-hero")(
      div(cls := "landing-container landing-hero__grid")(
        div(cls := "landing-hero__copy")(
          p(cls := "eyebrow")("Prelaunch waitlist"),
          h1("Review your chess games like a story, not a spreadsheet"),
          p(cls := "hero-summary")(
            "Chesstory is not open yet. We are building a review page that keeps the engine visible, then explains the turning point, the missed idea, and the lesson you can remember next game."
          ),
          prelaunchSignupForm("prelaunch_hero"),
          p(cls := "hero-disclosure")("No account, no billing, no fake beta queue. One email field for launch updates.")
        ),
        div(cls := "landing-hero__visual")(
          prelaunchMockup("motion-demo", "Plan")
        )
      )
    )

  private def prelaunchModes(using @unused ctx: Context) =
    st.section(id := "preview", cls := "landing-section prelaunch-modes")(
      div(cls := "landing-container")(
        div(cls := "section-heading section-heading--center")(
          p(cls := "section-kicker")("Tiny product feel"),
          h2("Pick a review moment. The page changes like the product should."),
          p("This is a lightweight mockup, not a working analyzer. It shows the kind of explanation Chesstory should generate once the product opens.")
        ),
        div(cls := "prelaunch-mode-demo")(
          input(tpe := "radio", id := "mode-blunder", name := "prelaunch-mode", checked := true),
          input(tpe := "radio", id := "mode-plan", name := "prelaunch-mode"),
          input(tpe := "radio", id := "mode-endgame", name := "prelaunch-mode"),
          div(cls := "prelaunch-mode-tabs", aria.label := "Sample review moments")(
            label(cls := "prelaunch-mode-tab", attr("for") := "mode-blunder")("Blunder"),
            label(cls := "prelaunch-mode-tab", attr("for") := "mode-plan")("Plan"),
            label(cls := "prelaunch-mode-tab", attr("for") := "mode-endgame")("Endgame")
          ),
          div(cls := "prelaunch-mode-panels")(
            prelaunchModePanel(
              "mode-blunder",
              "The losing move was not random",
              "Bxh7+ looked forcing, but the defender on f6 was still covering h7. Chesstory should point to the missing removal idea before showing the engine line.",
              "Turning point",
              "Move 17 changed the game because the attack started before the defender was overloaded.",
              "Bxh7+",
              "-1.80"
            ),
            prelaunchModePanel(
              "mode-plan",
              "The engine move has a human reason",
              "c3 is not just +0.37. It prepares d4, gives the bishop a safer retreat, and makes Black answer the center instead of inventing counterplay.",
              "Plan",
              "The position is asking White to make d4 real, not to spend a tempo on a harmless pin.",
              "c3",
              "+0.37"
            ),
            prelaunchModePanel(
              "mode-endgame",
              "The endgame lesson should survive",
              "Kf2 keeps the opposition close enough to support the outside passer. The useful memory is not the tablebase result; it is which king race matters.",
              "Lesson",
              "Before pushing the passer, bring the king close enough that the opponent cannot win both races.",
              "Kf2",
              "+0.62"
            )
          )
        )
      )
    )

  private def prelaunchTransformation(using @unused ctx: Context) =
    st.section(id := "why", cls := "landing-section prelaunch-transform")(
      div(cls := "landing-container landing-study-payoff__grid")(
        div(cls := "study-payoff-copy")(
          p(cls := "section-kicker")("Before / after"),
          h2("From engine output to a review you can actually use"),
          p("The marketing job is simple: show the transformation before asking for trust. Chesstory should turn one confusing position into a short, inspectable study note.")
        ),
        div(cls := "prelaunch-compare", aria.label := "Engine-only review compared with Chesstory review")(
          div(cls := "prelaunch-compare__panel prelaunch-compare__panel--before")(
            span(cls := "motion-demo__label")("Before"),
            strong("Stockfish: c3 +0.37"),
            p("A number, a move, and a line. Useful, but easy to forget when the same structure appears again.")
          ),
          div(cls := "prelaunch-compare__panel prelaunch-compare__panel--after")(
            span(cls := "motion-demo__label")("After"),
            strong("Plan: make d4 real"),
            p("The review names the position question, explains the move effect, and keeps the engine evidence beside the lesson.")
          )
        )
      )
    )

  private def prelaunchSignal(using @unused ctx: Context) =
    st.section(id := "launch-notice-section", cls := "landing-section landing-resources prelaunch-promise")(
      div(cls := "landing-container")(
        div(cls := "section-heading section-heading--center")(
          p(cls := "section-kicker")("What your email means"),
          h2("We are measuring demand before pretending this is launched"),
          p(
            "The page has one real action: leave an email if this review style sounds worth hearing about. Everything else is a small preview of the direction."
          )
        ),
        div(cls := "landing-resource-grid")(
          prelaunchCard("Specific outcome", "A good signup page should say what changes for the player: a game becomes a memorable review."),
          prelaunchCard("Small honest demo", "The mockup shows the desired product feel without claiming that analysis is live today."),
          prelaunchCard("Email only", "We keep the form short because extra fields can wait until there is a real beta to segment.")
        )
      )
    )

  private def hero(using @unused ctx: Context) =
    st.section(cls := "landing-hero")(
      div(cls := "landing-container landing-hero__grid")(
        div(cls := "landing-hero__copy")(
          p(cls := "eyebrow")("For players who want the plan, not only the number"),
          h1("Chess analysis that explains what the position is asking"),
          p(cls := "hero-summary")(
            "Paste a PGN or import a public game. Chesstory keeps the Stockfish line visible, " +
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

  private def story(using @unused ctx: Context) =
    st.section(id := "story", cls := "landing-story")(
      div(cls := "landing-container")(
        div(cls := "section-heading section-heading--center")(
          p(cls := "section-kicker")("From moves to meaning"),
          h2("Watch a game turn into an explanation you can study"),
          p(
            "Your game score becomes a board you can inspect. Stockfish shows the reference line. " +
              "Chesstory turns the key position into a plan, then keeps it as a review study."
          )
        )
      ),
      div(cls := "landing-cinema")(
        div(cls := "landing-container landing-cinema__grid")(
          div(cls := "cinema-stage", attr("aria-hidden") := "true")(
            div(cls := "cinema-stage__shell")(
              div(cls := "cinema-layer cinema-layer--pgn")(
                span(cls := "cinema-kicker")("Game score"),
                code("[White \"You\"] [Black \"Opponent\"] 1. e4 e5 2. Nf3 Nc6 3. Bc4 Bc5 4. O-O Nf6 5. d3 O-O")
              ),
              board("cinema-layer cinema-layer--board"),
              div(cls := "cinema-layer cinema-layer--engine")(
                span(cls := "cinema-kicker")("Stockfish line"),
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
              "Your moves appear on the analysis board."
            ),
            storyStep(
              "02",
              "Let the position appear",
              "The board, move list, and current position become the shared reference point for every explanation.",
              "The game is now something you can inspect."
            ),
            storyStep(
              "03",
              "Keep Stockfish visible",
              "Stockfish prefers c3 at +0.37, then Nbd2 at +0.25 and a4 at +0.22.",
              "The numbers stay visible as the benchmark, not as the whole answer."
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
              "The review becomes a study you can reopen and share."
            )
          )
        )
      )
    )

  private def studyPayoff(using @unused ctx: Context) =
    st.section(id := "study", cls := "landing-section landing-study-payoff")(
      div(cls := "landing-container landing-study-payoff__grid")(
        div(cls := "study-payoff-copy")(
          p(cls := "section-kicker")("The study you keep"),
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

  private def resources(using @unused ctx: Context) =
    st.section(id := "resources", cls := "landing-section landing-resources")(
      div(cls := "landing-container")(
        div(cls := "section-heading section-heading--center")(
          p(cls := "section-kicker")("How Chesstory works"),
          h2("Learn what Chesstory is building before you trust it with a game"),
          p(
            "The beta should not feel like a black box. Read the help guide, review the privacy details, follow the journal, and see where the roadmap is headed."
          )
        ),
        div(cls := "landing-resource-grid")(
          resourceCard("Help", "Start a review, import games, understand the Stockfish line, and create a review study.", routes.Main.help.url),
          resourceCard("Examples", "See realistic sample reviews before trying your own game.", routes.Main.examples.url),
          resourceCard("Journal", "Follow chess-study ideas and new Chesstory features as the beta grows.", routes.Main.journal.url),
          resourceCard("Beta Access / Pricing", "Understand the current free beta and planned paid options before subscriptions exist.", routes.Main.pricing.url),
          resourceCard("Trust", "Review privacy, source, and reliability details before public launch.", routes.Main.trust.url),
          resourceCard("Roadmap", "See Now, Next, and Later without date promises that the beta cannot honestly make yet.", routes.Main.roadmap.url)
        )
      )
    )

  private def landingFooter(using @unused ctx: Context) =
    footer(cls := "landing-footer")(
      div(cls := "landing-container footer-links")(
        span("2026 Chesstory"),
        span(
          a(href := routes.Main.help.url)("Help"),
          " / ",
          a(href := routes.Main.examples.url)("Examples"),
          " / ",
          a(href := routes.Main.journal.url)("Journal"),
          " / ",
          a(href := routes.Main.pricing.url)("Beta Access / Pricing"),
          " / ",
          a(href := routes.Main.trust.url)("Trust"),
          " / ",
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

  private def prelaunchFooter(using @unused ctx: Context) =
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
          a(href := "#cookie-consent", cls := "js-cookie-consent-open")("Cookie settings")
        )
      )
    )

  private def prelaunchSignupForm(entrypoint: String)(using ctx: Context): Frag =
    st.form(id := "launch-notice", cls := "prelaunch-form", method := "post", action := routes.BetaFeedback.submitForm.url)(
      input(tpe := "hidden", name := "surface", value := "prelaunch"),
      input(tpe := "hidden", name := "feature", value := "launch_notice"),
      input(tpe := "hidden", name := "entrypoint", value := entrypoint),
      input(tpe := "hidden", name := "willingness", value := "maybe"),
      input(tpe := "hidden", name := "notify", value := "true"),
      input(tpe := "hidden", name := "returnTo", value := s"${routes.Main.prelaunch.url}#launch-notice"),
      ctx.req.flash.get("success").map(msg =>
        p(cls := "prelaunch-form__success")(msg)
      ),
      div(cls := "prelaunch-form__row")(
        input(
          cls := "prelaunch-form__input",
          tpe := "email",
          name := "email",
          placeholder := "you@example.com",
          autocomplete := "email",
          required := true
        ),
        button(cls := "btn-primary prelaunch-form__button", tpe := "submit")("Notify me")
      ),
      p(cls := "prelaunch-form__note")("We will store this email for Chesstory launch updates. You can ask us to remove it later.")
    )

  private def prelaunchMockup(extraCls: String, tag: String): Frag =
    div(cls := s"motion-demo prelaunch-visual $extraCls")(
      div(cls := "motion-demo__pgn")(
        span(cls := "motion-demo__label")("Sample game moment"),
        code("1. e4 e5 2. Nf3 Nc6 3. Bc4 Bc5 4. O-O Nf6 5. d3 O-O")
      ),
      board("motion-demo__board"),
      div(cls := "motion-demo__engine")(
        span(cls := "motion-demo__label")("Stockfish"),
        strong("+0.37 depth 22"),
        engineLine("c3", "+0.37", true),
        engineLine("Nbd2", "+0.25", false),
        engineLine("h3", "+0.20", false)
      ),
      div(cls := "motion-demo__explain")(
        span(cls := "motion-demo__label")(s"Chesstory $tag"),
        strong("White is choosing how to make d4 real."),
        p("The review should connect the engine's best move to the human plan: prepare the central break before answering a threat that is not urgent.")
      )
    )

  private def prelaunchModePanel(
      id: String,
      title: String,
      copy: String,
      label: String,
      output: String,
      move: String,
      eval: String
  ): Frag =
    div(cls := s"prelaunch-mode-panel prelaunch-mode-panel--$id")(
      div(cls := "prelaunch-mode-copy")(
        span(cls := "motion-demo__label")(label),
        h3(title),
        p(copy),
        div(cls := "step-output")(output)
      ),
      div(cls := "prelaunch-mode-board")(
        board("prelaunch-mode-board__board"),
        div(cls := "prelaunch-mode-engine")(
          span(cls := "motion-demo__label")("Engine"),
          engineLine(move, eval, true),
          engineLine("candidate line", "depth 22", false)
        )
      )
    )

  private def prelaunchCard(title: String, copy: String): Frag =
    div(cls := "landing-resource-card prelaunch-card")(
      span(cls := "landing-resource-card__label")("Prelaunch"),
      strong(title),
      p(copy)
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

  private def resourceCard(title: String, copy: String, hrefUrl: String): Frag =
    a(href := hrefUrl, cls := "landing-resource-card")(
      span(cls := "landing-resource-card__label")("Resource"),
      strong(title),
      p(copy)
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
