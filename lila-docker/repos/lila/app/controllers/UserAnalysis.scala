package controllers

import lila.app.*
import play.api.mvc.*

final class UserAnalysis(
    env: Env
) extends LilaController(env):

  def index = Open:
    renderPage()

  def help = Open:
    fuccess(Ok.snip(views.analyse.ui.keyboardHelp))

  def parseArg(arg: String) = Open:
    val (key, fenStr) = arg.indexOf('/') match
      case -1 => (arg, None)
      case i => (arg.take(i), Some(arg.drop(i + 1)))

    val variantOpt =
      chess.variant
        .Variant(chess.variant.Variant.LilaKey(key))
        .filter: variant =>
          variant.standard || variant.chess960

    variantOpt.fold(
      Redirect(routes.UserAnalysis.index).toFuccess
    ): variant =>
      val rawFen: Option[chess.format.Fen.Full] = fenStr
        .map(_.replace("_", " "))
        .flatMap(lila.common.String.decodeUriPath)
        .filter(_.trim.nonEmpty)
        .map(chess.format.Fen.Full.clean)
      val fen = rawFen.filter(fen => chess.format.Fen.read(variant, fen).isDefined)
      val positionQuery =
        Option.when(variant.chess960 && rawFen.isEmpty)(ctx.req.getQueryString("position")).flatten

      if rawFen.isDefined && fen.isEmpty then
        Redirect(routes.UserAnalysis.parseArg(variant.key.value)).toFuccess
      else if positionQuery.isDefined then
        Chess960Analysis
          .canonicalArgForPosition(positionQuery)
          .fold(Redirect(routes.UserAnalysis.parseArg(variant.key.value)).toFuccess): canonicalArg =>
            Redirect(routes.UserAnalysis.parseArg(canonicalArg)).toFuccess
      else renderPage(variant = variant, fen = fen)

  def embed = Anon:
    val payload = AnalysePgnPipeline.buildPayload()
    fuccess:
      InEmbedContext:
        Ok.snip(
          views.analyse.embed.userAnalysis(
            payload.data,
            inlinePgn = payload.inlinePgn
          )
        )

  private def renderPage(
      variant: chess.variant.Variant = chess.variant.Standard,
      fen: Option[chess.format.Fen.Full] = None,
      inlinePgn: Option[String] = None
  )(using ctx: Context): Fu[Result] =
    Ok.page(AnalysePgnPipeline.page(variant = variant, fen = fen, inlinePgn = inlinePgn)).map: result =>
      if ctx.isAuth || inlinePgn.nonEmpty then result.hasPersonalData else result
