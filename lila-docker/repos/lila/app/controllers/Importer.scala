package controllers

import lila.app.*
import play.api.libs.json.*
import play.api.libs.ws.StandaloneWSClient
import play.api.libs.ws.DefaultBodyReadables.*
import play.api.mvc.Result

import java.net.URI
import java.nio.ByteBuffer
import java.nio.charset.{ CodingErrorAction, StandardCharsets }
import java.time.{ Instant, ZoneOffset }
import java.time.format.DateTimeFormatter
import java.util.Base64
import scala.concurrent.duration.*
import scala.util.control.NonFatal

object Importer:
  val providerLichess = "lichess"
  val providerChessCom = "chesscom"
  val providers = Set(providerLichess, providerChessCom)

  enum SubmittedPgn:
    case Inline(value: String)
    case InvalidEncoded

  enum GameSelectionSubmission:
    case Absent
    case Invalid
    case Selected(pgn: String)

  case class GameSummary(
      provider: String,
      gameId: String,
      playedAt: String,
      white: String,
      black: String,
      result: String,
      speed: String,
      sourceUrl: Option[String],
      pgn: String
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
    Option(raw)
      .filter(_.nonEmpty)
      .flatMap: value =>
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
        catch case NonFatal(_) => None

  private[controllers] def submittedPgn(form: Map[String, Seq[String]]): Option[SubmittedPgn] =
    def first(name: String): Option[String] =
      form.get(name).flatMap(_.headOption).map(_.trim).filter(_.nonEmpty)
    if form.contains("pgn64") then
      Some(decodePgn64(first("pgn64").getOrElse("")) match
        case Some(pgn) => SubmittedPgn.Inline(pgn)
        case None => SubmittedPgn.InvalidEncoded)
    else first("pgn").map(SubmittedPgn.Inline.apply)

  private[controllers] def gameSelectionSubmission(
      form: Map[String, Seq[String]],
      lookup: String => Option[(String, GameSummary)]
  ): GameSelectionSubmission =
    form.get("gameSelection") match
      case None => GameSelectionSubmission.Absent
      case Some(values) =>
        values.headOption
          .map(_.trim)
          .filter(_.nonEmpty)
          .flatMap(lookup)
          .fold[GameSelectionSubmission](GameSelectionSubmission.Invalid): (_, game) =>
            GameSelectionSubmission.Selected(game.pgn)

  private[controllers] def decodePgn64(encoded: String): Option[String] =
    val normalized = Option(encoded).map(_.trim).filter(_.nonEmpty)
    normalized.flatMap { raw =>
      val padded =
        raw.length % 4 match
          case 0 => raw
          case mod => raw + ("=" * (4 - mod))
      try
        val text =
          StandardCharsets.UTF_8
            .newDecoder()
            .onMalformedInput(CodingErrorAction.REPORT)
            .onUnmappableCharacter(CodingErrorAction.REPORT)
            .decode(ByteBuffer.wrap(Base64.getUrlDecoder.decode(padded)))
            .toString
            .trim
        Option(text).filter(_.nonEmpty)
      catch case NonFatal(_) => None
    }

final class Importer(
    env: Env,
    ws: StandaloneWSClient
) extends LilaController(env):
  import Importer.GameSummary

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
  private val gameSelections =
    env.memo.cacheApi.notLoadingSync[String, (String, GameSummary)](128, "importer.gameSelection"):
      _.maximumSize(2048).expireAfterWrite(10.minutes).build()

  def importGame = Open:
    def queryParam(name: String): Option[String] =
      req.queryString.get(name).flatMap(_.headOption).map(_.trim).filter(_.nonEmpty)
    val provider =
      queryParam("provider")
        .map(_.toLowerCase)
        .filter(Importer.providers)
        .getOrElse(Importer.providerLichess)
    val username = queryParam("username").getOrElse("")
    Ok.async(importIndexPage(provider = provider, username = username))
      .map: result =>
        if ctx.isAuth then result.hasPersonalData else result

  // Handles two cases:
  // 1) provider + username => redirect to fetched game list
  // 2) game selection, pgn64 or pgn => render analysis for an inline game
  def sendGame = OpenBodyOf(parse.formUrlEncoded): (ctx: BodyContext[Map[String, Seq[String]]]) ?=>
    val form = ctx.body.body

    Importer.gameSelectionSubmission(
      form,
      id =>
        val selected = gameSelections.getIfPresent(id)
        gameSelections.invalidate(id)
        selected
    ) match
      case Importer.GameSelectionSubmission.Selected(pgn) =>
        renderAnalysisWithInlinePgn(pgn)
      case Importer.GameSelectionSubmission.Invalid =>
        BadRequest
          .async(importIndexPage(error = Some("This game selection expired. Reload the game list and try again.")))
          .map: result =>
            if ctx.isAuth then result.hasPersonalData else result
      case Importer.GameSelectionSubmission.Absent =>
        Importer.submittedPgn(form) match
          case Some(Importer.SubmittedPgn.Inline(pgn)) =>
            renderAnalysisWithInlinePgn(pgn)
          case Some(Importer.SubmittedPgn.InvalidEncoded) =>
            BadRequest
              .async(importIndexPage(error = Some("The encoded game could not be read.")))
              .map: result =>
                if ctx.isAuth then result.hasPersonalData else result
          case None =>
            def first(name: String): Option[String] =
              form.get(name).flatMap(_.headOption).map(_.trim).filter(_.nonEmpty)
            val provider = first("provider").map(_.toLowerCase)
            val username = first("username")
            (provider, username) match
              case (Some(Importer.providerLichess), Some(user)) =>
                Redirect(routes.Importer.importFromLichess(user)).toFuccess
              case (Some(Importer.providerChessCom), Some(user)) =>
                Redirect(routes.Importer.importFromChessCom(user)).toFuccess
              case _ =>
                BadRequest
                  .async(importIndexPage(error = Some("Please choose provider and enter a valid username.")))
                  .map: result =>
                    if ctx.isAuth then result.hasPersonalData else result

  def apiSendGame = Open:
    Redirect(routes.Importer.importGame).toFuccess

  def importFromLichess(username: String) = Open:
    normalizedUsername(username).fold[Fu[Result]](
      BadRequest
        .async(importIndexPage(error = Some("Invalid Lichess username.")))
        .map: result =>
          if ctx.isAuth then result.hasPersonalData else result
    ) { normalized =>
      for
        games <- fetchRecentLichessGames(normalized)
        page <- gameListPage(
          provider = Importer.providerLichess,
          username = normalized,
          games = games,
          notice = Option.when(games.isEmpty)("No public games found for this Lichess user.")
        )
        res <- Ok.page(page)
      yield if ctx.isAuth then res.hasPersonalData else res
    }

  def importFromChessCom(username: String) = Open:
    normalizedUsername(username).fold[Fu[Result]](
      BadRequest
        .async(importIndexPage(error = Some("Invalid Chess.com username.")))
        .map: result =>
          if ctx.isAuth then result.hasPersonalData else result
    ) { normalized =>
      for
        games <- fetchRecentChessComGames(normalized)
        page <- gameListPage(
          provider = Importer.providerChessCom,
          username = normalized,
          games = games,
          notice = Option.when(games.isEmpty)("No public games found for this Chess.com user.")
        )
        res <- Ok.page(page)
      yield if ctx.isAuth then res.hasPersonalData else res
    }

  private def normalizedUsername(raw: String): Option[String] =
    Option(raw).map(_.trim).filter(_.nonEmpty).flatMap(usernamePattern.findFirstIn)

  private def renderAnalysisWithInlinePgn(rawPgn: String)(using ctx: Context): Fu[Result] =
    AnalysePgnPipeline
      .validatedInlinePgn(rawPgn)
      .fold[Fu[Result]](
        BadRequest
          .async(
            importIndexPage(
              error = Some(
                "Enter a complete replayable PGN: 200,000 characters or fewer, with up to 600 moves across the full PGN tree."
              ),
              draft = AnalysePgnPipeline.inlinePgnDraft(rawPgn)
            )
          )
          .map: result =>
            if ctx.isAuth then result.hasPersonalData else result
      ): inlinePgn =>
        Ok.page(AnalysePgnPipeline.page(inlinePgn = Some(inlinePgn))).map(_.hasPersonalData)

  private def importIndexPage(
      error: Option[String] = None,
      provider: String = Importer.providerLichess,
      username: String = "",
      draft: Option[String] = None
  )(using ctx: Context): Fu[lila.ui.Page] =
    fuccess:
      views.importer.index(
        error = error,
        provider = provider,
        username = username,
        draft = draft
      )

  private def gameListPage(
      provider: String,
      username: String,
      games: List[GameSummary],
      notice: Option[String]
  )(using ctx: Context): Fu[lila.ui.Page] =
    fuccess:
      val selectedGames = games.map: game =>
        val selectionId = scalalib.SecureRandom.nextString(24)
        gameSelections.put(selectionId, username -> game)
        selectionId -> game
      views.importer.gameList(
        provider = provider,
        username = username,
        games = selectedGames,
        notice = notice
      )

  private def fetchRecentLichessGames(username: String): Fu[List[GameSummary]] =
    val url = s"$lichessImportApiBase/api/games/user/$username?max=$recentGameTarget&pgnInJson=true"
    ws.url(url)
      .withHttpHeaders("Accept" -> "application/x-ndjson")
      .withRequestTimeout(requestTimeout)
      .get()
      .map { res =>
        if res.status == 200 then
          Option(res.body[String]).toList
            .flatMap(_.split('\n').toList)
            .map(_.trim)
            .filter(_.nonEmpty)
            .flatMap(parseLichessNdjsonLine)
            .take(recentGameTarget)
        else
          logger.warn(s"lichess import list failed status=${res.status}")
          Nil
      }
      .recover { case NonFatal(err) =>
        logger.warn(s"lichess import list exception=${err.getClass.getSimpleName}")
        Nil
      }

  private def parseLichessNdjsonLine(line: String): Option[GameSummary] =
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
          case _ => "1/2-1/2"
        val playedAt = formatEpochMs((js \ "lastMoveAt").asOpt[Long].orElse((js \ "createdAt").asOpt[Long]))
        val speed = (js \ "speed").asOpt[String].orElse((js \ "perf").asOpt[String]).getOrElse("-")
        val sourceUrl = Some(s"$lichessWebBase/$gameId")
        GameSummary(
          provider = Importer.providerLichess,
          gameId = gameId,
          playedAt = playedAt,
          white = white,
          black = black,
          result = result,
          speed = speed,
          sourceUrl = sourceUrl,
          pgn = value
        )
      }
    catch case NonFatal(_) => None

  private def lichessPlayerName(js: JsValue, side: String): String =
    (js \ "players" \ side \ "user" \ "name")
      .asOpt[String]
      .orElse((js \ "players" \ side \ "name").asOpt[String])
      .getOrElse(side.capitalize)

  private def fetchRecentChessComGames(username: String): Fu[List[GameSummary]] =
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
          logger.warn(s"chesscom archives failed status=${res.status}")
          fuccess(Nil)
      }
      .recover { case NonFatal(err) =>
        logger.warn(s"chesscom archives exception=${err.getClass.getSimpleName}")
        Nil
      }

  private def collectChessComGames(
      archives: List[String],
      acc: List[GameSummary]
  ): Fu[List[GameSummary]] =
    if acc.size >= recentGameTarget || archives.isEmpty then fuccess(acc.take(recentGameTarget))
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

  private def parseChessComArchive(jsonText: String): List[GameSummary] =
    val games = (Json.parse(jsonText) \ "games").asOpt[List[JsObject]].getOrElse(Nil)
    games
      .sortBy(g => (g \ "end_time").asOpt[Long].getOrElse(0L))
      .reverse
      .flatMap(parseChessComGame)

  private def parseChessComGame(game: JsObject): Option[GameSummary] =
    val pgn = (game \ "pgn").asOpt[String].map(_.trim).filter(_.nonEmpty)
    pgn.map { value =>
      val sourceUrl = (game \ "url").asOpt[String].flatMap(trustedSourceUrl)
      val gameId = sourceUrl
        .flatMap(_.split('/').lastOption)
        .filter(_.nonEmpty)
        .getOrElse(s"chesscom-${Math.abs(value.hashCode)}")
      val white = (game \ "white" \ "username").asOpt[String].getOrElse("White")
      val black = (game \ "black" \ "username").asOpt[String].getOrElse("Black")
      val result = chessComResult(game)
      val speed = (game \ "time_class").asOpt[String].getOrElse("-")
      val playedAt = formatEpochMs((game \ "end_time").asOpt[Long].map(_ * 1000L))
      GameSummary(
        provider = Importer.providerChessCom,
        gameId = gameId,
        playedAt = playedAt,
        white = white,
        black = black,
        result = result,
        speed = speed,
        sourceUrl = sourceUrl,
        pgn = value
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

  private def trustedSourceUrl(raw: String): Option[String] =
    try
      val uri = URI.create(raw.trim)
      val host = Option(uri.getHost).map(_.toLowerCase)
      val scheme = Option(uri.getScheme).map(_.toLowerCase)
      Option.when(
        scheme.contains("https") &&
          host.exists(h =>
            h == "lichess.org" || h.endsWith(".lichess.org") || h == "chess.com" || h.endsWith(".chess.com")
          )
      )(uri.toString)
    catch case NonFatal(_) => None
