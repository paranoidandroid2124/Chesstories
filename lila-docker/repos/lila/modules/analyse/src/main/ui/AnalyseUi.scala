package lila.analyse
package ui

import chess.variant.*
import chess.format.{ Uci, Fen }
import play.api.libs.json.*

import lila.ui.*
import lila.ui.ScalatagsTemplate.{ *, given }

final class AnalyseUi(helpers: Helpers)(endpoints: AnalyseEndpoints):
  import helpers.*

  def miniSpan(fen: Fen.Board, color: Color = chess.White, lastMove: Option[Uci] = None) =
    chessgroundMini(fen, color, lastMove)(span)

  def explorerAndCevalConfig(using ctx: Context) =
    Json.obj(
      "explorer" -> Json.obj(
        "endpoint" -> endpoints.explorer,
        "tablebaseEndpoint" -> endpoints.tablebase,
        "showRatings" -> ctx.pref.showRatings
      ),
      "externalEngineEndpoint" -> endpoints.externalEngine
    )

  def userAnalysis(
      data: JsObject,
      pov: Pov,
      chess960PositionNum: Option[Int] = None,
      inlinePgn: Option[String] = None,
      importHistory: Option[JsObject] = None
  )(using ctx: Context): Page =
    Page("Analysis")
      .css("analyse.workspace")
      .csp(bits.cspExternalEngine.compose(_.withExternalAnalysisApis))
      .js:
        bits.analyseModule(
          "userAnalysis",
          Json
            .obj(
              "data" -> data
            )
            .add("inlinePgn", inlinePgn)
            .add("importHistory", importHistory) ++
            explorerAndCevalConfig
        )
      .graph(
        title = "Chess analysis board",
        url = lila.ui.Url(routeUrl(routes.UserAnalysis.index)),
        description = "Analyse chess positions and variations on an interactive chess board"
      )
      .flag(_.zoom):
        main(
          cls := List(
            "analyse" -> true
          )
        )(
          pov.game.synthetic.option(
            st.aside(cls := "analyse__side")(
              lila.ui.bits.mselect(
                "analyse-variant",
                span(cls := "text", dataIcon := Icon.CrownElite)(
                  pov.game.variant.name
                ),
                List(Standard, Chess960)
                  .map: v =>
                    a(
                      dataIcon := Icon.CrownElite,
                      cls := (pov.game.variant == v).option("current"),
                      href := routes.UserAnalysis.parseArg(v.key.value)
                    )(v.name)
              ),
              pov.game.variant.chess960.option(chess960selector(chess960PositionNum))
            )
          ),
          div(cls := "analyse__board main-board")(chessgroundBoard),
          div(cls := "analyse__tools"),
          div(cls := "analyse__controls")
        )

  private def chess960selector(num: Option[Int]) =
    div(cls := "jump-960")(
      num.map(pos => label(`for` := "chess960-position")(s"Chess960 position $pos")),
      br,
      form(
        cls := "control-960",
        method := "GET",
        action := routes.UserAnalysis.parseArg("chess960")
      )(
        input(
          id := "chess960-position",
          `type` := "number",
          name := "position",
          min := 0,
          max := 959,
          value := num
        ),
        form3.submit("Load position", icon = none)
      )
    )

  def titleOf(pov: Pov) =
    s"${playerText(pov.game.whitePlayer)} vs ${playerText(pov.game.blackPlayer)}"

  object bits:

    val dataPanel = attr("data-panel")

    def page(title: String): Page =
      Page(title)
        .flag(_.zoom)
        .flag(_.noRobots)
        .csp:
          cspExternalEngine.compose(_.withPeer.withInlineIconFont.withChessDbCn)

    def cspExternalEngine: Update[ContentSecurityPolicy] =
      _.withWebAssembly.withExternalEngine(endpoints.externalEngine)

    def analyseModule(mode: "userAnalysis" | "replay", json: JsObject) =
      PageModule("analyse.user", Json.obj("mode" -> mode, "cfg" -> json))

    val embedUserAnalysisBody = div(id := "main-wrap", cls := "is2d")(
      main(cls := "analyse")(
        div(cls := "analyse__board main-board")(chessgroundBoard),
        div(cls := "analyse__tools"),
        div(cls := "analyse__controls")
      )
    )
