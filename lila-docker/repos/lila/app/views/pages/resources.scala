package views.pages

import lila.app.UiEnv.*
import lila.ui.{ Esm, Page }
import scala.annotation.unused

private object resourcePageBits:
  def shell(pageTitle: String, heading: String, meta: String)(body: Frag*)(using @unused ctx: Context): Page =
    Page(pageTitle)
      .css("legal")
      .wrap: _ =>
        main(cls := "legal-page legal-page--resource")(
          div(cls := "legal-container")(
            header(cls := "legal-header")(
              h1(heading),
              p(cls := "legal-meta")(meta)
            ),
            div(cls := "legal-content")(body*)
          )
        )

  def linkRows(items: List[(String, String, String)]): Frag =
    div(cls := "legal-link-rows")(
      items.map { (title, copy, url) =>
        a(href := url, cls := "legal-link-row")(
          strong(title),
          span(copy)
        )
      }*
    )

  def actionRow(actions: List[(String, String, String)]): Frag =
    div(cls := "legal-actions")(
      actions.map { (label, url, klass) =>
        a(href := url, cls := klass)(label)
      }*
    )

  def factRows(items: List[(String, Frag)]): Frag =
    dl(cls := "legal-facts")(
      items.map { (label, value) =>
        div(
          dt(label),
          dd(value)
        )
      }*
    )

object help:
  private val questions = List(
    ("What can I import?", "Paste a PGN, choose a PGN file, or browse a public Lichess or Chess.com account.", "import pgn file public account 가져오기 기보 파일 공개 계정 리체스 체스닷컴"),
    ("What does Stockfish do here?", "Evaluation, depth, and candidate lines stay beside the board as evidence for the move you are reviewing.", "engine analysis evaluation depth candidate stockfish 엔진 분석 평가 깊이 후보 스톡피시"),
    ("What is a review study?", "A study keeps the board, move tree, notes, and saved sections together so you can reopen or share a specific chapter.", "study review save share notes chapter 스터디 복기 저장 공유 메모 챕터"),
    ("Can I use Chesstory without signing in?", "You can open the core board flow without an account. Sign in to manage Studies you explicitly save; ordinary analysis and imports are not retained as history.", "account login sign in anonymous free 계정 로그인 회원가입 비회원 무료"),
    ("Can I delete my data?", "Use the contact page for account, study, or privacy requests. The privacy policy describes the current handling of chess material.", "data delete privacy personal information 데이터 삭제 개인정보 문의")
  )

  def apply(query: Option[String] = None)(using @unused ctx: Context): Page =
    val term = query.map(_.trim).filter(_.nonEmpty)
    val matched = term.fold(questions): needle =>
      val words = needle.toLowerCase.split("\\s+").filter(_.nonEmpty)
      questions.filter { case (title, body, keywords) =>
        val searchable = s"$title $body $keywords".toLowerCase
        words.forall(searchable.contains)
      }
    val matchedItems: List[Frag] = matched.map { case (title, body, _) =>
      details(if term.isDefined && matched.size == 1 then attr("open") := "open" else emptyFrag)(
        summary(title),
        p(body)
      )
    }
    resourcePageBits.shell(
      "Help - Chesstory",
      "Help",
      "Guidance for importing games, reviewing positions, studies, and account data."
    )(
      st.form(cls := "legal-search", role := "search", method := "get", action := routes.Main.help.url)(
        label(`for` := "help-query")("Search help"),
        div(
          input(
            id := "help-query",
            tpe := "search",
            name := "q",
            value := term | "",
            placeholder := "Import, engine, study, privacy"
          ),
          button(tpe := "submit", cls := "button button-thin")("Search")
        )
      ),
      st.section(cls := "legal-section legal-section--questions")(
        h2(term.fold("Popular questions")(value => s"${matched.size} matching questions for “$value”")),
        div(cls := "legal-questions")(
          if matched.nonEmpty then frag(matchedItems*)
          else p("No help topic matched that search. Try “import”, “engine”, “study”, or “privacy”.")
        )
      ),
      st.section(cls := "legal-section legal-section--tasks")(
        h2("Start a task"),
        resourcePageBits.linkRows(
          List(
            (
              "Review a game",
              "Paste a PGN and work through why key positions changed.",
              routes.UserAnalysis.index.url
            ),
            (
              "Import a game",
              "Paste PGN, choose a file, or browse a public account.",
              routes.Importer.importGame.url
            ),
            ("Browse studies", "Open public studies or return to your saved work.", "/study")
          )
        )
      ),
      st.section(cls := "legal-section")(
        h2("Contact, service, and policy"),
        resourcePageBits.factRows(
          List(
            "Contact" -> frag(a(href := routes.Main.contact.url)("Ask a question or request help.")),
            "Service" -> frag(
              "Found a problem? ",
              a(href := routes.Main.contact.url)("Let us know"),
              " so we can look into it."
            ),
            "Policies" -> frag(
              a(href := routes.Main.privacy.url)("Privacy"),
              " • ",
              a(href := routes.Main.terms.url)("Terms"),
              " • ",
              a(href := routes.Main.trust.url)("Trust")
            )
          )
        )
      )
    )

object pricing:
  def apply()(using @unused ctx: Context): Page =
    resourcePageBits.shell(
      "Beta Access / Pricing - Chesstory",
      "Beta access and pricing",
      "Chesstory is open beta. There is no active paid subscription or billing flow."
    )(
      st.section(cls := "legal-section legal-access-card")(
        span(cls := "legal-access-card__label")("AVAILABLE NOW"),
        h2("Open beta"),
        p(
          "The beta is for reviewing one game at a time: board, engine evidence, and saved study work when you are signed in."
        ),
        ul(
          li("Explore a position without an account."),
          li("Import a PGN or find a public game."),
          li("Save your work in a study when signed in.")
        ),
        resourcePageBits.actionRow(List(("Start a review", routes.UserAnalysis.index.url, "button")))
      )
    )

object trust:
  def apply(contactEmail: Option[String])(using @unused ctx: Context): Page =
    resourcePageBits.shell(
      "Trust - Chesstory",
      "Trust",
      "Facts about the engine, judgment path, chess data, and beta limits."
    )(
      st.section(cls := "legal-section")(
        h2("What the product uses"),
        resourcePageBits.factRows(
          List(
            "Engine reference" -> frag(
              "Browser-side Stockfish components provide evaluation and principal variations. The product shows depth and lines; it does not currently expose a fixed engine build identifier."
            ),
            "Judgment path" -> frag(
              "Current move facts and judgment are produced by the Chesstory engine and judgment path. This route does not use an external AI-model provider."
            ),
            "Review inputs" -> frag(
              "Current FEN, played move, engine variations and evaluations, move prefix, opening context, and bounded probe results when available."
            ),
            "External model processing" -> frag(
              "The current explanation route does not send review data to an external AI-model provider."
            ),
            "Your deletion request" -> frag(
              "Use ",
              a(href := routes.Main.contact.url)("contact"),
              " for account, study, or privacy requests; see the ",
              a(href := routes.Main.privacy.url)("privacy policy"),
              " for the current scope."
            ),
            "Limitations" -> frag(
              "Engine output and current judgment facts are study aids, not a coach, tournament arbiter, or guarantee of a correct human plan."
            )
          )
        )
      ),
      st.section(cls := "legal-section")(
        h2("Service and changes"),
        resourcePageBits.factRows(
          List(
            "Service readiness" -> frag(
              "A health endpoint is available. A public monitored status page is not published yet."
            ),
            "Changes" -> frag(
              a(href := routes.Main.journal.url)("Read product changes in the journal."),
              br,
              a(href := routes.Main.roadmap.url)("See planned work on the roadmap.")
            ),
            "Source" -> frag(
              "Chesstory includes AGPL-licensed chess server software; see ",
              a(href := routes.Main.source.url)("open source"),
              " for source-availability details."
            ),
            "Contact" -> frag(
              "Questions about privacy, security, or source code? ",
              a(href := routes.Main.contact.url)("Send us a request"),
              contactEmail
                .map(email => frag(" or ", a(href := s"mailto:$email")(email)))
                .getOrElse(emptyFrag),
              "."
            )
          )
        )
      )
    )

private[pages] object ReviewExample:
  val fen = "r1bq1rk1/pppp1ppp/2n2n2/2b1p3/2B1P3/3P1N2/PPP2PPP/RNBQ1RK1 w - - 4 6"
  val move = "6. h3"
  val opening = "Italian Game"
  val line = "6. c3 d6 7. d4"
  val explanation =
    "6. h3 prevents ...Bg4, but it does not prepare the central break. " +
      "6. c3 supports d4 immediately; after 6...d6, 7. d4 challenges e5 with the break prepared."
  val lesson = "Prepare the support for a central break before playing the break itself."

  def widget: Frag =
    st.aside(id := "review-example", cls := "review-example", attr("data-fen") := fen, aria.label := "Interactive Italian Game example")(
      div(cls := "review-example__head")(span("TRY A POSITION"), strong(opening)),
      div(cls := "review-example__tabs", role := "group", aria.label := "Compare moves")(
        button(tpe := "button", attr("data-line") := "played", attr("aria-pressed") := "true")("Played · 6. h3"),
        button(tpe := "button", attr("data-line") := "reference", attr("aria-pressed") := "false")("Alternative · 6. c3")
      ),
      div(cls := "review-example__board cg-wrap", role := "img", aria.label := "Replay board for the selected example line"),
      div(cls := "review-example__controls")(
        button(tpe := "button", attr("data-step") := "prev", aria.label := "Previous example move")("←"),
        div(strong(cls := "review-example__move", attr("aria-live") := "polite")("Starting position"), span(cls := "review-example__progress")("0 / 1")),
        button(tpe := "button", attr("data-step") := "next", aria.label := "Next example move")("→")
      ),
      p(cls := "review-example__hint")("Choose a move, then use the arrows to compare."),
      p(cls := "review-example__caption", attr("aria-live") := "polite")(explanation)
    )

object examples:

  def apply()(using @unused ctx: Context): Page =
    resourcePageBits.shell(
      "Explained example - Chesstory",
      "A move sequence, explained",
      "See how Chesstory connects the move under review, a reference line, and a reusable lesson."
    )(
      st.section(cls := "legal-section")(
        h2(s"${ReviewExample.opening}: prepare the central break"),
        div(cls := "legal-example-list")(
          st.article(cls := "legal-example")(
            ReviewExample.widget,
            div(cls := "legal-example__copy")(
              h3("Why 6. c3 comes before 7. d4"),
              dl(
                div(dt("Move under review"), dd(ReviewExample.move)),
                div(dt("Reference line"), dd(ReviewExample.line))
              ),
              p(ReviewExample.explanation),
              p(strong("Lesson: "), ReviewExample.lesson),
              resourcePageBits.actionRow(
                List(("Review your own game", routes.UserAnalysis.index.url, "button"))
              )
            )
          )
        )
      )
    ).js(Esm("site.reviewExample"))

object roadmap:
  def apply()(using @unused ctx: Context): Page =
    resourcePageBits.shell(
      "Roadmap - Chesstory",
      "Roadmap",
      "A beta roadmap without date promises."
    )(
      st.section(cls := "legal-section legal-roadmap")(
        h2("Now"),
        ul(
          li("One-game review flow."),
          li("PGN paste and public game import."),
          li("Stockfish evidence beside plain-language explanation."),
          li("Review studies for saving and sharing work.")
        )
      ),
      st.section(cls := "legal-section legal-roadmap")(
        h2("Next"),
        ul(
          li("Clearer import, help, and study-sharing flows."),
          li("Additional public example reviews."),
          li("Iterate on explanation quality with beta feedback and review evidence.")
        )
      ),
      st.section(cls := "legal-section legal-roadmap")(
        h2("Later"),
        ul(
          li("Player-level recurring weakness summaries."),
          li("Paid plans after limits and billing terms are ready."),
          li("A monitored public status page.")
        )
      )
    )
