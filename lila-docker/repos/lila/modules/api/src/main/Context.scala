package lila.api

import play.api.mvc.{ Request, RequestHeader }
import lila.common.HTTPRequest
import lila.core.net.IpAddress
import lila.pref.Pref
import lila.ui.Nonce

/* Who is logged in, and how */
final class LoginContext(
    val me: Option[Me],
    val impersonatedBy: Option[lila.core.userId.ModId]
):
  export me.{ isDefined as isAuth, isEmpty as isAnon }
  def user: Option[User] = Me.raw(me)
  def userId: Option[UserId] = user.map(_.id)
  def username: Option[UserName] = user.map(_.username)
  def isAppealUser = me.exists(_.enabled.no)
  def kid = lila.core.user.KidMode(false)
  def isTokenAuth = false

object LoginContext:
  val anon = LoginContext(none, none)

/* Data available in every HTTP request */
class Context(
    val req: RequestHeader,
    val loginContext: LoginContext,
    val pref: Pref
) extends lila.ui.Context:
  export loginContext.*
  def ip: IpAddress = HTTPRequest.ipAddress(req)
  lazy val mobileApiVersion: Option[lila.core.net.ApiVersion] = None
  lazy val blind = false
  def updatePref(f: Update[Pref]) = new Context(req, loginContext, f(pref))

object Context:
  export lila.api.{ Context, BodyContext, LoginContext, PageContext, EmbedContext }
  given (using ctx: Context): Option[Me] = ctx.me
  given (using ctx: Context): Option[MyId] = ctx.myId
  given (using page: PageContext): Context = page.ctx
  given (using embed: EmbedContext): Context = embed.ctx

  import lila.pref.RequestPref
  def minimal(req: RequestHeader) =
    Context(req, LoginContext.anon, RequestPref.fromRequest(req))
  def minimalBody[A](req: Request[A]) =
    BodyContext(req, LoginContext.anon, RequestPref.fromRequest(req))

final class BodyContext[A](
    val body: Request[A],
    userContext: LoginContext,
    pref: Pref
) extends Context(body, userContext, pref)

/* Data necessary to render the site shell */
case class PageData(
    nonce: Option[Nonce],
    error: Boolean = false
)

object PageData:
  def anon(nonce: Option[Nonce]) = PageData(nonce)
  def error(nonce: Option[Nonce]) = anon(nonce).copy(error = true)

final class PageContext(val ctx: Context, val data: PageData) extends lila.ui.PageContext:
  export ctx.*
  export data.*

final class EmbedContext(val ctx: Context, val bg: String, val nonce: Nonce):
  export ctx.*
  def boardClass = ctx.pref.realTheme.name
  def pieceSet = ctx.pref.realPieceSet

object EmbedContext:
  def apply(ctx: Context): EmbedContext = new EmbedContext(
    ctx,
    bg = ctx.req.queryString.get("bg").flatMap(_.headOption).filterNot("auto".==) | "system",
    nonce = Nonce.random
  )
