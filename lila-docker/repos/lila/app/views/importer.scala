package views

import controllers.Importer.{ GameSummary, providerChessCom, providerLichess }
import lila.app.UiEnv.*
import lila.ui.{ Esm, Page }
import scala.annotation.unused

object importer:

  private val filePreReadLimitBytes = 200000
  private val inlinePgnCharacterLimit = controllers.AnalysePgnPipeline.maxInlinePgnChars

  def index(
      error: Option[String] = None,
      provider: String = providerLichess,
      username: String = "",
      draft: Option[String] = None
  )(using @unused ctx: Context): Page =
    val pageError = error
    Page("Recent Games - Chesstory")
      .css("auth")
      .js(Esm("site.importer"))
      .wrap: _ =>
        main(cls := "import-page")(
          div(cls := "import-shell")(
            header(cls := "import-heading")(
              h1("Review a game"),
              p("Paste a PGN or choose a file, then review the game move by move.")
            ),
            pageError.map(msg => div(cls := "import-message import-message--error", role := "alert")(msg)),
            importWorkspace(draft),
            accountLookup(provider, username)
          )
        )

  def gameList(
      provider: String,
      username: String,
      games: List[(String, GameSummary)],
      notice: Option[String] = None
  )(using @unused ctx: Context): Page =
    Page("Recent Games - Chesstory")
      .css("auth")
      .wrap: _ =>
        main(cls := "import-page")(
          div(cls := "import-shell")(
            header(cls := "import-heading")(
              h1(s"@$username"),
              p(
                providerShortLabel(provider),
                " • Choose one public game to review move by move. Save a study only after the review is useful."
              )
            ),
            notice
              .map(msg => div(cls := "import-message", role := "status", attr("aria-live") := "polite")(msg)),
            st.section(cls := "import-results", aria.label := "Recent public games")(
              div(cls := "import-results__head")(
                h2("Recent public games"),
                span(s"${games.size} shown")
              ),
              games.nonEmpty
                .option(
                  div(cls := "import-results__list")(
                    games.map { case (selectionId, game) => gameRow(selectionId, game) }*
                  )
                )
            ),
            accountLookup(provider, username, compact = true),
            div(cls := "auth-links")(
              a(href := routes.Importer.importGame.url)("New import"),
              a(href := routes.UserAnalysis.index.url)("Review another game")
            )
          )
        )

  private def importWorkspace(draft: Option[String]): Frag =
    st.section(cls := "import-workspace", aria.label := "Import a game")(
      form(
        id := "import-workspace",
        cls := "import-workspace__form",
        method := "post",
        action := routes.Importer.sendGame.url
      )(
        div(cls := "form-group import-workspace__game-text")(
          label(`for` := "import-pgn")("PGN"),
          textarea(
            id := "import-pgn",
            name := "pgn",
            rows := 7,
            placeholder := "Paste a complete game score",
            attr("data-import-byte-limit") := filePreReadLimitBytes.toString,
            attr("data-import-character-limit") := inlinePgnCharacterLimit.toString,
            attr("aria-describedby") := "import-workspace-status",
            required
          )(draft.getOrElse("")),
          label(cls := "import-file", `for` := "import-file")(
            span("PGN file"),
            input(
              id := "import-file",
              tpe := "file",
              accept := ".pgn,.txt,text/plain",
              attr("aria-describedby") := "import-workspace-status"
            )
          )
        ),
        div(cls := "import-workspace__submit")(
          div(
            p(
              "Files above 200,000 bytes are rejected before reading. After loading, submitted PGN must be 200,000 characters or fewer. A selected file stays in this browser until it is copied into this field."
            ),
            p(
              id := "import-workspace-status",
              cls := "import-workspace__status",
              role := "status",
              attr("aria-live") := "polite"
            )
          ),
          button(id := "import-submit", cls := "button", tpe := "submit")("Review this game")
        )
      )
    )

  private def accountLookup(provider: String, username: String, compact: Boolean = false): Frag =
    st.section(
      id := "import-account",
      cls := s"import-account${if compact then " import-account--compact" else ""}"
    )(
      div(cls := "import-account__head")(
        h2(if compact then "Find another player" else "Browse a public account"),
        p("Choose Lichess or Chess.com, then select one recent public game.")
      ),
      form(cls := "auth-form import-account__form", method := "post", action := routes.Importer.sendGame.url)(
        div(cls := "form-group")(
          label(`for` := s"import-provider${if compact then "-compact" else ""}")("Game site"),
          st.select(id := s"import-provider${if compact then "-compact" else ""}", name := "provider")(
            option(
              value := providerLichess,
              if provider == providerLichess then selected := true else emptyFrag
            )("Lichess"),
            option(
              value := providerChessCom,
              if provider == providerChessCom then selected := true else emptyFrag
            )("Chess.com")
          )
        ),
        div(cls := "form-group")(
          label(`for` := s"import-username${if compact then "-compact" else ""}")("Username"),
          input(
            id := s"import-username${if compact then "-compact" else ""}",
            tpe := "text",
            name := "username",
            value := username,
            placeholder := "e.g. DrNykterstein",
            autocomplete := "username",
            required
          )
        ),
        button(cls := "button", tpe := "submit")("Show recent games")
      )
    )

  private def gameRow(selectionId: String, game: GameSummary): Frag =
    div(cls := "import-game")(
      div(cls := "import-game__head")(
        strong(cls := "import-game__title")(s"${game.white} vs ${game.black}")
      ),
      div(cls := "import-game__meta")(
        providerShortLabel(game.provider),
        span("•"),
        game.result,
        span("•"),
        s"${game.playedAt} UTC",
        span("•"),
        game.speed,
        span("•"),
        game.gameId
      ),
      div(cls := "import-game__actions")(
        form(method := "post", action := routes.Importer.sendGame.url, style := "margin: 0;")(
          input(tpe := "hidden", name := "gameSelection", value := selectionId),
          button(tpe := "submit", cls := "button button-thin")("Open review board")
        ),
        game.sourceUrl.map(url =>
          a(href := url, target := "_blank", rel := "noopener noreferrer", cls := "import-game__source")(
            "Original game"
          )
        )
      )
    )

  private def providerShortLabel(provider: String): String =
    provider.trim.toLowerCase match
      case `providerChessCom` => "Chess.com"
      case _ => "Lichess"
