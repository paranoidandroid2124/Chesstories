package lila.ui

import play.api.mvc.RequestHeader

import lila.core.net.IpAddress
import lila.core.pref.Pref

/* Data available in every HTTP request */
trait Context:
  val req: RequestHeader
  def isAuth: Boolean
  def isTokenAuth: Boolean
  def me: Option[Me]
  def user: Option[User]
  def userId: Option[UserId]
  def pref: Pref
  def ip: IpAddress
  def blind: Boolean

  def is[U: UserIdOf](u: U): Boolean = me.exists(_.is(u))
  def isnt[U: UserIdOf](u: U): Boolean = !is(u)
  def myId: Option[MyId] = me.map(_.myId)

import lila.core.user.Me
object Context:
  given ctxMe(using ctx: Context): Option[Me] = ctx.me

/* Data necessary to render the site shell */
trait PageContext extends Context:
  val me: Option[Me]
  val impersonatedBy: Option[lila.core.userId.ModId]
  def nonce: Option[Nonce]
  def error: Boolean
