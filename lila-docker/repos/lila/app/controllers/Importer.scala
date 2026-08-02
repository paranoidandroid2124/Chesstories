package controllers

import lila.app.*
import lila.analyse.ImportHistory
import play.api.libs.json.*
import play.api.libs.ws.StandaloneWSClient
import play.api.libs.ws.DefaultBodyReadables.*
import play.api.mvc.Result

import java.net.URI
import java.nio.charset.StandardCharsets
import java.time.{ Instant, ZoneOffset }
import java.time.format.DateTimeFormatter
import java.util.Base64
import scala.concurrent.duration.*
import scala.util.control.NonFatal

object Importer:
  case class GameCard(
      provider: String,
      gameId: String,
      playedAt: String,
      white: String,
      black: String,
      result: String,
      speed: String,
      sourceUrl: Option[String],
      pgn64: String
  )

  private val chessComArchiveHost = "api.chess.com"
  private val chessComArchivePath =
    "^/pub/player/[A-Za-z0-9][A-Za-z0-9_-]{0,29}/games/[0-9]{4}/(?:0[1-9]|1[0-2])$".r

  private[controllers] def chessComArchiveFetchUrls(archiveListBody: String): List[String] =
    (Json.parse(archiveListBody) \ "archives")
      .asOpt[List[String]]
      .getOrElse(Nil)
      .flatMap(approvedChessComArchiveUrl)

  private def approvedChessComArchiveUrl(raw: String): Option[String] =
    Option(raw).filter(_.nonEmpty).flatMap: value =>
      try
        val uri = URI.create(value)
        val rawPath = Option(uri.getRawPath)
        Option.when(
          Option(uri.getScheme).exists(_.equalsIgnoreCase("https")) &&
            Option(uri.getHost).exists(_.equalsIgnoreCase(chessComArchiveHost)) &&
            (uri.getPort == -1 || uri.getPort == 443) &&
            uri.getRawUserInfo == null &&
            uri.getRawQuery == null &&
            uri.getRawFragment == null &&
            rawPath.exists(path => chessComArchivePath.pattern.matcher(path).matches)
        )(uri.toString)
      catch
        case NonFatal(_) => None

final class Importer(
    env: Env,
    ws: StandaloneWSClient
) extends LilaController(env):
  import Importer.GameCard

  private val logger = lila.log("importer")
  private val usernamePattern = "^[A-Za-z0-9][A-Za-z0-9_-]{1,29}$".r
  private val requestTimeout = 12.seconds
  private val recentGameTarget = 40
  private val chessComArchiveScanLimit = 8
  private val utcDateFmt = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm").withZone(ZoneOffset.UTC)
  private def configured(path: String): Option[String] =
    env.config.getOptional[String](path).map(_.trim).filter(_.nonEmpty)
  private val lichessImportApiBase =
    configured("external.import.lichess.api_base")
      .orElse(sys.env.get("LICHESS_IMPORT_API_BASE").map(_.trim).filter(_.nonEmpty))
      .getOrElse("https://lichess.org")
      .stripSuffix("/")
  private val lichessWebBase =
    configured("external.import.lichess.web_base")
      .orElse(sys.env.get("LICHESS_WEB_BASE").map(_.trim).filter(_.nonEmpty))
      .getOrElse("https://lichess.org")
      .stripSuffix("/")
  private val chessComApiBase =
    configured("external.import.chesscom.api_base")
      .orElse(sys.env.get("CHESSCOM_API_BASE").map(_.trim).filter(_.nonEmpty))
      .getOrElse("https://api.chess.com")
      .stripSuffix("/")

  def importGame = Open:
    def queryParam(name: String): Option[String] =
      req.queryString.get(name).flatMap(_.headOption).map(_.trim).filter(_.nonEmpty)
    val provider =
      queryParam("provider").map(_.toLowerCase).filter(ImportHistory.providers).getOrElse(ImportHistory.providerLichess)
    val username = queryParam("username").getOrElse("")
    Ok.async(importIndexPage(provider = provider, username = username)).map: result =>
      if ctx.isAuth then result.hasPersonalData else result

  // Handles two cases:
  // 1) provider + username => redirect to fetched game list
  // 2) pgn64 => render analysis for selected game
  def sendGame = OpenBodyOf(parse.formUrlEncoded): (ctx: BodyContext[Map[String, Seq[String]]]) ?=>
    val form = ctx.body.body

    def first(name: String): Option[String] =
      form.get(name).flatMap(_.headOption).map(_.trim).filter(_.nonEmpty)

    first("pgn64").flatMap(decodePgn64) match
      case Some(pgn) =>
        renderAnalysisWithInlinePgn(pgn, submittedAnalysisSource(first))
      case None =>
        val provider = first("provider").map(_.toLowerCase)
        val username = first("username")
        (provider, username) match
          case (Some(ImportHistory.providerLichess), Some(user)) =>
            Redirect(routes.Importer.importFromLichess(user)).toFuccess
          case (Some(ImportHistory.providerChessCom), Some(user)) =>
            Redirect(routes.Importer.importFromChessCom(user)).toFuccess
          case _ =>
            BadRequest.async(importIndexPage(error = Some("Please choose provider and enter a valid username."))).map: result =>
              if ctx.isAuth then result.hasPersonalData else result

  def apiSendGame = Open:
    Redirect(routes.Importer.importGame).toFuccess

  def importFromLichess(username: String) = Open:
    normalizedUsername(username).fold[Fu[Result]](
      BadRequest.async(importIndexPage(error = Some("Invalid Lichess username."))).map: result =>
        if ctx.isAuth then result.hasPersonalData else result
    ) { normalized =>
      for
        _ <- recordAccountSearch(ImportHistory.providerLichess, normalized)
        games <- fetchRecentLichessGames(normalized)
        page <- gameListPage(
          provider = ImportHistory.providerLichess,
          username = normalized,
          games = games,
          notice = Option.when(games.isEmpty)("No public games found for this Lichess user.")
        )
        res <- Ok.page(page)
      yield if ctx.isAuth then res.hasPersonalData else res
    }

  def importFromChessCom(username: String) = Open:
    normalizedUsername(username).fold[Fu[Result]](
      BadRequest.async(importIndexPage(error = Some("Invalid Chess.com username."))).map: result =>
        if ctx.isAuth then result.hasPersonalData else result
    ) { normalized =>
      for
        _ <- recordAccountSearch(ImportHistory.providerChessCom, normalized)
        games <- fetchRecentChessComGames(normalized)
        page <- gameListPage(
          provider = ImportHistory.providerChessCom,
          username = normalized,
          games = games,
          notice = Option.when(games.isEmpty)("No public games found for this Chess.com user.")
        )
        res <- Ok.page(page)
      yield if ctx.isAuth then res.hasPersonalData else res
    }

  private def normalizedUsername(raw: String): Option[String] =
    Option(raw).map(_.trim).filter(_.nonEmpty).flatMap(usernamePattern.findFirstIn)

  private def renderAnalysisWithInlinePgn(
      rawPgn: String,
      source: ImportHistory.AnalysisSource
  )(using ctx: Context): Fu[Result] =
    AnalysePgnPipeline.normalizedInlinePgn(rawPgn).fold[Fu[Result]](
      BadRequest("Empty PGN payload from upstream provider").toFuccess
    ): inlinePgn =>
      ctx.me.fold[Fu[Result]](
        Ok.page(AnalysePgnPipeline.page(inlinePgn = Some(inlinePgn))).map(_.hasPersonalData)
      ): me =>
        env.analyse.importHistory
          .recordAnalysis(me.userId, inlinePgn, source)
          .map: entry =>
            Redirect(routes.UserAnalysis.imported(entry._id))
          .recoverWith { case NonFatal(err) =>
            logger.warn(s"import analysis history save failed err=${err.getMessage}")
            Ok.page(AnalysePgnPipeline.page(inlinePgn = Some(inlinePgn))).map(_.hasPersonalData)
          }

  private def submittedAnalysisSource(
      first: String => Option[String]
  ): ImportHistory.AnalysisSource =
    ImportHistory.AnalysisSource(
      sourceType = first("sourceType").getOrElse(ImportHistory.sourceManual),
      provider = first("sourceProvider").orElse(first("provider")),
      username = first("sourceUsername").orElse(first("username")),
      externalGameId = first("sourceGameId"),
      sourceUrl = first("sourceUrl").flatMap(trustedSourceUrl),
      white = first("sourceWhite"),
      black = first("sourceBlack"),
      result = first("sourceResult"),
      speed = first("sourceSpeed"),
      playedAtLabel = first("sourcePlayedAt")
    )

  private def importIndexPage(
      error: Option[String] = None,
      provider: String = ImportHistory.providerLichess,
      username: String = ""
  )(using ctx: Context): Fu[lila.ui.Page] =
    importPageSummary.map: summary =>
      views.importer.index(
        error = error,
        provider = provider,
        username = username,
        recentAccounts = summary.accounts,
        recentAnalyses = summary.analyses
      )

  private def gameListPage(
      provider: String,
      username: String,
      games: List[GameCard],
      notice: Option[String]
  )(using ctx: Context): Fu[lila.ui.Page] =
    importPageSummary.map: summary =>
      views.importer.gameList(
        provider = provider,
        username = username,
        games = games,
        notice = notice,
        recentAccounts = summary.accounts,
        recentAnalyses = summary.analyses
      )

  private def importPageSummary(using ctx: Context): Fu[ImportHistory.RecentSummary] =
    ctx.me.fold(fuccess(ImportHistory.RecentSummary.empty)): me =>
      env.analyse.importHistory.recentSummary(me.userId)

  private def recordAccountSearch(provider: String, username: String)(using ctx: Context): Funit =
    ctx.me.fold(funit): me =>
      env.analyse.importHistory.recordAccountSearch(me.userId, provider, username)

  private def fetchRecentLichessGames(username: String): Fu[List[GameCard]] =
    val url = s"$lichessImportApiBase/api/games/user/$username?max=$recentGameTarget&pgnInJson=true"
    ws.url(url)
      .withHttpHeaders("Accept" -> "application/x-ndjson")
      .withRequestTimeout(requestTimeout)
      .get()
      .map { res =>
        if res.status == 200 then
          Option(res.body[String])
            .toList
            .flatMap(_.split('\n').toList)
            .map(_.trim)
            .filter(_.nonEmpty)
            .flatMap(parseLichessNdjsonLine)
            .take(recentGameTarget)
        else
          logger.warn(s"lichess import list failed username=$username status=${res.status}")
          Nil
      }
      .recover { case NonFatal(err) =>
        logger.warn(s"lichess import list exception username=$username err=${err.getMessage}")
        Nil
      }

  private def parseLichessNdjsonLine(line: String): Option[GameCard] =
    try
      val js = Json.parse(line)
      val pgn = (js \ "pgn").asOpt[String].map(_.trim).filter(_.nonEmpty)
      pgn.map { value =>
        val gameId = (js \ "id").asOpt[String].getOrElse(s"lichess-${Math.abs(value.hashCode)}")
        val white = lichessPlayerName(js, "white")
        val black = lichessPlayerName(js, "black")
        val winner = (js \ "winner").asOpt[String]
        val result = winner match
          case Some("white") => "1-0"
          case Some("black") => "0-1"
          case _             => "1/2-1/2"
        val playedAt = formatEpochMs((js \ "lastMoveAt").asOpt[Long].orElse((js \ "createdAt").asOpt[Long]))
        val speed = (js \ "speed").asOpt[String].orElse((js \ "perf").asOpt[String]).getOrElse("-")
        val sourceUrl = Some(s"$lichessWebBase/$gameId")
        GameCard(
          provider = ImportHistory.providerLichess,
          gameId = gameId,
          playedAt = playedAt,
          white = white,
          black = black,
          result = result,
          speed = speed,
          sourceUrl = sourceUrl,
          pgn64 = encodePgn64(value)
        )
      }
    catch
      case NonFatal(_) => None

  private def lichessPlayerName(js: JsValue, side: String): String =
    (js \ "players" \ side \ "user" \ "name")
      .asOpt[String]
      .orElse((js \ "players" \ side \ "name").asOpt[String])
      .getOrElse(side.capitalize)

  private def fetchRecentChessComGames(username: String): Fu[List[GameCard]] =
    val archivesUrl = s"$chessComApiBase/pub/player/${username.toLowerCase}/games/archives"
    ws.url(archivesUrl)
      .withRequestTimeout(requestTimeout)
      .get()
      .flatMap { res =>
        if res.status == 200 then
          val archives =
            Importer
              .chessComArchiveFetchUrls(res.body[String])
              .reverse
              .take(chessComArchiveScanLimit)
          collectChessComGames(archives, acc = Nil)
        else
          logger.warn(s"chesscom archives failed username=$username status=${res.status}")
          fuccess(Nil)
      }
      .recover { case NonFatal(err) =>
        logger.warn(s"chesscom archives exception username=$username err=${err.getMessage}")
        Nil
      }

  private def collectChessComGames(
      archives: List[String],
      acc: List[GameCard]
  ): Fu[List[GameCard]] =
    if acc.size >= recentGameTarget || archives.isEmpty then
      fuccess(acc.take(recentGameTarget))
    else
      val head = archives.head
      val tail = archives.tail
      ws.url(head)
        .withFollowRedirects(false)
        .withRequestTimeout(requestTimeout)
        .get()
        .flatMap { res =>
          val merged =
            if res.status == 200 then acc ++ parseChessComArchive(res.body[String])
            else acc
          collectChessComGames(tail, merged.take(recentGameTarget))
        }
        .recoverWith { case NonFatal(_) =>
          collectChessComGames(tail, acc)
        }

  private def parseChessComArchive(jsonText: String): List[GameCard] =
    val games = (Json.parse(jsonText) \ "games").asOpt[List[JsObject]].getOrElse(Nil)
    games
      .sortBy(g => (g \ "end_time").asOpt[Long].getOrElse(0L))
      .reverse
      .flatMap(parseChessComGame)

  private def parseChessComGame(game: JsObject): Option[GameCard] =
    val pgn = (game \ "pgn").asOpt[String].map(_.trim).filter(_.nonEmpty)
    pgn.map { value =>
      val sourceUrl = (game \ "url").asOpt[String].flatMap(trustedSourceUrl)
      val gameId = sourceUrl.flatMap(_.split('/').lastOption).filter(_.nonEmpty).getOrElse(s"chesscom-${Math.abs(value.hashCode)}")
      val white = (game \ "white" \ "username").asOpt[String].getOrElse("White")
      val black = (game \ "black" \ "username").asOpt[String].getOrElse("Black")
      val result = chessComResult(game)
      val speed = (game \ "time_class").asOpt[String].getOrElse("-")
      val playedAt = formatEpochMs((game \ "end_time").asOpt[Long].map(_ * 1000L))
      GameCard(
        provider = ImportHistory.providerChessCom,
        gameId = gameId,
        playedAt = playedAt,
        white = white,
        black = black,
        result = result,
        speed = speed,
        sourceUrl = sourceUrl,
        pgn64 = encodePgn64(value)
      )
    }

  private def chessComResult(game: JsObject): String =
    val w = (game \ "white" \ "result").asOpt[String].getOrElse("").toLowerCase
    val b = (game \ "black" \ "result").asOpt[String].getOrElse("").toLowerCase
    if w == "win" then "1-0"
    else if b == "win" then "0-1"
    else "1/2-1/2"

  private def formatEpochMs(millisOpt: Option[Long]): String =
    millisOpt
      .map(ms => utcDateFmt.format(Instant.ofEpochMilli(ms)))
      .getOrElse("-")

  private def encodePgn64(pgn: String): String =
    Base64.getUrlEncoder.withoutPadding().encodeToString(pgn.getBytes(StandardCharsets.UTF_8))

  private def decodePgn64(encoded: String): Option[String] =
    val normalized = Option(encoded).map(_.trim).filter(_.nonEmpty)
    normalized.flatMap { raw =>
      val padded =
        raw.length % 4 match
          case 0 => raw
          case mod => raw + ("=" * (4 - mod))
      try
        val text = new String(Base64.getUrlDecoder.decode(padded), StandardCharsets.UTF_8).trim
        Option(text).filter(_.nonEmpty)
      catch
        case NonFatal(_) => None
    }

  private def trustedSourceUrl(raw: String): Option[String] =
    try
      val uri = URI.create(raw.trim)
      val host = Option(uri.getHost).map(_.toLowerCase)
      val scheme = Option(uri.getScheme).map(_.toLowerCase)
      Option.when(
        scheme.contains("https") &&
          host.exists(h => h == "lichess.org" || h.endsWith(".lichess.org") || h == "chess.com" || h.endsWith(".chess.com"))
      )(uri.toString)
    catch
      case NonFatal(_) => None
