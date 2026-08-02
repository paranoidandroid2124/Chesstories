package controllers
import play.api.mvc.*
import play.api.libs.json.Json
import scala.util.control.NonFatal

import lila.app.*
import lila.core.study.StudyOrder
import lila.core.config.CollName
import lila.db.dsl.*
import lila.web.{ StaticContent, WebForms }

final class Main(
    env: Env,
    assetsC: ExternalAssets
) extends LilaController(env):

  private val journalContent = JournalContent(env.getFile, env.memo.markdown)

  private def supportLink(key: String): Option[String] =
    env.config
      .getOptional[String](key)
      .map(_.trim)
      .filter(_.nonEmpty)

  private def publicContactEmail: Option[String] =
    Option(env.net.email.value).map(_.trim).filter(_.nonEmpty)

  def landing = Open:
    if ctx.isAuth then Redirect(routes.Main.home).toFuccess
    else Redirect(routes.Main.prelaunch).toFuccess

  def prelaunch = Open:
    Ok.page(views.pages.landing.prelaunch())

  def home = Auth { ctx ?=> me ?=>
    for
      summary <- env.analyse.importHistory.recentSummary(me.userId)
      studyPager <- env.study.pager.mine(StudyOrder.updated, page = 1)(using me)
      recentStudies = studyPager.currentPageResults.take(3).toList
      continueCard =
        summary.analyses.headOption
          .map(Main.HomeContinueCard.Analysis.apply)
          .orElse(recentStudies.headOption.map(Main.HomeContinueCard.Study.apply))
          .getOrElse(Main.HomeContinueCard.Starter)
      data = Main.HomePageData(
        continueCard = continueCard,
        quickActions = List(
          Main.HomeQuickAction(
            label = "PGN",
            title = "Start a deep review",
            copy = "Paste one game and review the key positions on the board.",
            href = routes.UserAnalysis.index.url
          ),
          Main.HomeQuickAction(
            label = "Recent games",
            title = "Bring a public game",
            copy = "Find recent Lichess or Chess.com games and choose one to review.",
            href = routes.Importer.importGame.url
          )
        ),
        recentAnalyses = summary.analyses.take(4),
        recentAccounts = summary.accounts.take(4),
        recentStudies = recentStudies
      )
      result <- Ok.page(views.pages.home(data))
    yield result.hasPersonalData
  }

  def support = Open:
    Ok.page(
      views.pages.support(
        patreon = supportLink("support.links.patreon"),
        githubSponsors = supportLink("support.links.githubSponsors"),
        buyMeACoffee = supportLink("support.links.buyMeACoffee")
      )
    )

  def plan = Open:
    Redirect(routes.Main.support, MOVED_PERMANENTLY).toFuccess

  def help = Open:
    Ok.page(views.pages.help())

  def pricing = Open:
    Ok.page(views.pages.pricing())

  def trust = Open:
    Ok.page(views.pages.trust(publicContactEmail))

  def examples = Open:
    Ok.page(views.pages.examples())

  def roadmap = Open:
    Ok.page(views.pages.roadmap())

  def privacy = Open:
    Ok.page(views.pages.privacy(publicContactEmail))

  def terms = Open:
    Ok.page(views.pages.terms(publicContactEmail))

  def source = Open:
    Ok.page(views.pages.openSource())

  def journal = Open:
    Ok.page(views.pages.journal(journalContent.all, None))

  def journalPost(slug: String) = Open:
    journalContent.bySlug(slug) match
      case Some(post) => Ok.page(views.pages.journal(journalContent.all, Some(post)))
      case None       => NotFound.page(views.site.message.notFound(Some("Journal post not found.")))

  def toggleBlindMode = OpenBody:
    bindForm(WebForms.blind)(
      _ => BadRequest,
      (enable, redirect) =>
        Redirect(redirect).withCookies:
          lila.web.WebConfig.blindCookie.make(env.security.lilaCookie)(enable != "0")
    )

  def handlerNotFound(msg: Option[String]) =
    fuccess(NotFound(msg.getOrElse("Not Found")))

  def robots = Anon:
    Ok:
      if env.net.crawlable && req.domain == env.net.domain.value && env.mode.isProd
      then StaticContent.robotsTxt
      else "User-agent: *\nDisallow: /"

  def manifest = Anon:
    JsonOk:
      StaticContent.manifest(env.net)

  def contact = Open:
    Ok.page(views.pages.contact(publicContactEmail))

  def livez = Anon:
    Ok("ok").toFuccess

  def healthz = Anon:
    healthChecks.map: checks =>
      val ready = Main.isReady(checks)
      (if ready then Ok else ServiceUnavailable)(
        Json.obj(
          "status" -> (if ready then "ready" else "unready"),
          "checks" -> checks.map(_.json)
        )
      )

  def devAsset(@scala.annotation.unused v: String, path: String, file: String) = assetsC.at(path, file)

  private def healthChecks =
    val requiredInProd = env.mode.isProd
    for
      mongoReady <- mongoHealth
      bindingStatuses <- env.openBetaBindings.snapshot
    yield
      List(
        Main.HealthCheck(
          name = "mongo",
          ok = mongoReady,
          required = true,
          detail = if mongoReady then "query_ok" else "query_failed"
        ),
        Main.mailerCheck(env.config, env.mailer.mailer.canSend, required = requiredInProd),
      ) ++ bindingStatuses.flatMap(Main.bindingHealthCheck(_, requiredInProd))

  private def mongoHealth =
    env.mongo
      .mainDb(CollName("announce"))
      .find($empty)
      .one[Bdoc]
      .map(_ => true)
      .recover { case NonFatal(_) => false }

object Main:

  sealed trait HomeContinueCard
  object HomeContinueCard:
    final case class Analysis(entry: lila.analyse.ImportHistory.Analysis) extends HomeContinueCard
    final case class Study(entry: lila.study.Study.WithChaptersAndLiked) extends HomeContinueCard
    case object Starter extends HomeContinueCard

  final case class HomeQuickAction(
      label: String,
      title: String,
      copy: String,
      href: String
  )

  final case class HomePageData(
      continueCard: HomeContinueCard,
      quickActions: List[HomeQuickAction],
      recentAnalyses: List[lila.analyse.ImportHistory.Analysis],
      recentAccounts: List[lila.analyse.ImportHistory.Account],
      recentStudies: List[lila.study.Study.WithChaptersAndLiked]
  )

  final case class HealthCheck(
      name: String,
      ok: Boolean,
      required: Boolean,
      detail: String
  ):
    def json =
      Json.obj(
        "name" -> name,
        "ok" -> ok,
        "required" -> required
      )

  def isReady(checks: Iterable[HealthCheck]): Boolean =
    checks.forall(check => !check.required || check.ok)

  def mailerCheck(config: play.api.Configuration, canSend: Boolean, required: Boolean): HealthCheck =
    val mock = config.getOptional[Boolean]("mailer.primary.mock").getOrElse(true)
    val missing =
      List(
        "host" -> config.getOptional[String]("mailer.primary.host"),
        "user" -> config.getOptional[String]("mailer.primary.user"),
        "password" -> config.getOptional[String]("mailer.primary.password"),
        "sender" -> config.getOptional[String]("mailer.primary.sender")
      ).collect:
        case (label, value) if value.forall(_.trim.isEmpty) => label
    val ok = !mock && canSend && missing.isEmpty
    val detail =
      if mock then "mock_enabled"
      else if !canSend then "disabled_by_live_setting"
      else if missing.nonEmpty then s"missing:${missing.mkString(",")}"
      else "configured"
    HealthCheck("mailer", ok = ok, required = required, detail = detail)

  def bindingHealthCheck(
      status: OpenBetaBindingStatus,
      requiredInProd: Boolean
  ): Option[HealthCheck] =
    Option.unless(status.spec.readinessClass == "none"):
      HealthCheck(
        name = healthCheckName(status.spec.env),
        ok = status.readyOk,
        required = requiredInProd && status.readinessRequired,
        detail = status.detail
      )

  private def healthCheckName(env: String) =
    env match
      case "REDIS_URI" => "redis"
      case "EXPLORER_API_BASE" => "explorer"
      case "TABLEBASE_API_BASE" => "tablebase"
      case "LICHESS_IMPORT_API_BASE" => "lichess_import_api"
      case "LICHESS_WEB_BASE" => "lichess_web"
      case "CHESSCOM_API_BASE" => "chesscom_api"
      case other => other.toLowerCase
