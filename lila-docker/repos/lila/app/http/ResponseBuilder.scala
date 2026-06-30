package lila.app
package http
import alleycats.Zero
import play.api.http.*
import play.api.libs.json.*
import play.api.mvc.*

import lila.common.HTTPRequest
import lila.core.net.ApiVersion
import lila.ui.{ Page, Snippet }

trait ResponseBuilder(using Executor)
    extends lila.web.ResponseBuilder
    with lila.web.CtrlExtensions
    with RequestContext
    with CtrlPage:

  val keyPages = KeyPages(env)
  export env.net.{ baseUrl, routeUrl }

  given (using Context): Zero[Fu[Result]] = Zero(notFound)

  def Found[A](a: Fu[Option[A]])(f: A => Fu[Result])(using Context): Fu[Result] =
    a.flatMap(_.fold(notFound)(f))

  def FoundEmbed[A](a: Fu[Option[A]])(f: A => Fu[Result])(using @scala.annotation.unused ctx: EmbedContext): Fu[Result] =
    a.flatMap(_.fold(notFoundEmbed)(f))

  def Found[A](a: Option[A])(f: A => Fu[Result])(using Context): Fu[Result] =
    a.fold(notFound)(f)

  def FoundOk[A, B: Writeable](fua: Fu[Option[A]])(op: A => Fu[B])(using Context): Fu[Result] =
    Found(fua): a =>
      op(a).dmap(Ok(_))

  def FoundPage[A](fua: Fu[Option[A]])(op: A => Fu[Page])(using Context): Fu[Result] =
    Found(fua): a =>
      Ok.async(op(a))

  def FoundSnip[A](fua: Fu[Option[A]])(op: A => Fu[Snippet])(using Context): Fu[Result] =
    Found(fua): a =>
      Ok.snipAsync(op(a).map(_.frag))

  extension [A](fua: Fu[Option[A]])
    def orNotFound(f: A => Fu[Result])(using Context): Fu[Result] =
      fua.flatMap { _.fold(notFound)(f) }
  extension [A](fua: Fu[Boolean])
    def elseNotFound(f: => Fu[Result])(using Context): Fu[Result] =
      fua.flatMap { if _ then f else notFound }

  def WithEnabledUserId(name: UserStr)(op: UserId => Fu[Result])(using Context) =
    name.validateId
      .so(userId => env.user.repo.isEnabled(userId).map(_.option(userId)))
      .flatMap(_.fold(notFound("No such user".some))(op))

  def rateLimited(using Context): Fu[Result] = rateLimited(rateLimitedMsg)
  def rateLimited(msg: String = rateLimitedMsg)(using ctx: Context): Fu[Result] = negotiate(
    html =
      if HTTPRequest.isSynchronousHttp(ctx.req)
      then TooManyRequests.page(views.site.message.rateLimited(msg.some))
      else TooManyRequests(msg).toFuccess,
    json = TooManyRequests(jsonError(msg))
  )

  def negotiateApi(html: => Fu[Result], api: ApiVersion => Fu[Result])(using ctx: Context): Fu[Result] =
    negotiate(html, api(ApiVersion.mobile).dmap(_.withHeaders(VARY -> "Accept").as(JSON)))

  def negotiate(html: => Fu[Result], json: => Fu[Result])(using ctx: Context): Fu[Result] =
    if HTTPRequest.acceptsJson(ctx.req) || ctx.isTokenAuth
    then json.dmap(_.withHeaders(VARY -> "Accept").as(JSON))
    else html.dmap(_.withHeaders(VARY -> "Accept"))

  def negotiateJson(result: => Fu[Result])(using Context): Fu[Result] =
    negotiate(
      notFound("This endpoint only returns JSON, add the header `Accept: application/json`".some),
      result
    )

  def notFound(using ctx: Context): Fu[Result] = notFound(none)
  def notFound(msg: Option[String])(using ctx: Context): Fu[Result] =
    if ctx.isTokenAuth || HTTPRequest.acceptsJson(ctx.req) || HTTPRequest.acceptsNdJson(ctx.req)
    then msg.fold(notFoundJson())(notFoundJson)
    else if HTTPRequest.isSynchronousHttp(ctx.req)
    then keyPages.notFound(msg)
    else msg.fold(notFoundText())(notFoundText)

  def notFoundEmbed: Fu[Result] = notFoundEmbed(none)
  def notFoundEmbed(msg: Option[String]): Fu[Result] = fuccess(keyPages.notFoundEmbed(msg))

  def authenticationFailed(using ctx: Context): Fu[Result] =
    negotiate(
      html = Redirect(routes.Auth.login.url),
      json = Unauthorized(jsonError("Login required"))
    )

  def authorizationFailed(using ctx: Context): Fu[Result] =
    if HTTPRequest.isSynchronousHttp(ctx.req)
    then Forbidden.page(views.site.message.authFailed)
    else
      fuccess:
        render:
          case Accepts.Json() => forbiddenJson()
          case _ => forbiddenText()

  def serverError(msg: String)(using ctx: Context): Fu[Result] =
    negotiate(
      InternalServerError.page(views.site.message.serverError(msg)),
      InternalServerError(jsonError(msg))
    )
