package lila.core
package security

import play.api.data.Mapping
import play.api.mvc.RequestHeader

import lila.core.email.EmailAddress
import lila.core.net.{ ApiVersion }
import lila.core.user.{ User }
import lila.core.userId.{ UserId, UserName }

case class GarbageCollect(userId: UserId)
case class CloseAccount(userId: UserId)
case class ReopenAccount(user: User)
case class AskAreRelated(users: PairOf[UserId], promise: Promise[Boolean])

case class ClearPassword(value: String) extends AnyVal:
  override def toString = "ClearPassword(****)"

case class HashedPassword(bytes: Array[Byte])

trait Authenticator:
  def passEnc(p: ClearPassword): HashedPassword
  def setPassword(id: UserId, p: ClearPassword): Funit

trait SignupForm:
  val emailField: Mapping[EmailAddress]
  val username: Mapping[UserName]

case class UserSignup(
    user: User,
    email: EmailAddress,
    req: RequestHeader,
    apiVersion: Option[ApiVersion]
)

trait SecurityApi

trait SpamApi:
  def detect(text: String): Boolean
  def replace(text: String): String

case class IsProxy(name: Option[String]):
  def yes: Boolean = name.isDefined
  def no: Boolean = name.isEmpty
  def isEmpty: Boolean = name.isEmpty
  def isTor: Boolean = name.contains("Tor")
  def isFloodish: Boolean = name.isDefined
  def isVpn: Boolean = name.exists(n => n.contains("VPN") || n.contains("Proxy"))

object IsProxy:
  val empty = IsProxy(None)
  val torIdx = "Tor"
  val tor = IsProxy(Some(torIdx))
  def apply(name: String): IsProxy = IsProxy(if (name.isEmpty) None else Some(name))

trait Ip2ProxyApi:
  def ofReq(req: RequestHeader): Fu[IsProxy]
  def ofIp(ip: lila.core.net.IpAddress): Fu[IsProxy]
  def keepProxies(ips: Seq[lila.core.net.IpAddress]): Fu[Map[lila.core.net.IpAddress, String]]

trait UserTrustApi:
  def get(id: UserId): Fu[Int]

trait Hcaptcha:
  def verify(response: String, ip: Option[lila.core.net.IpAddress]): Fu[Boolean]

trait HcaptchaForm[A]:
  def form: play.api.data.Form[A]
  def enabled: Boolean = false
  def apply(key: String): play.api.data.Field = form(key)

trait FloodApi:
  def allowRequest(req: RequestHeader): Fu[Boolean]
  def allowMessage(source: FloodSource, text: String): Boolean

opaque type FloodSource = String
object FloodSource extends TotalWrapper[FloodSource, String]:
  def apply(ip: lila.core.net.IpAddress): FloodSource = ip.value.asInstanceOf[FloodSource]
  def apply(userId: UserId): FloodSource = userId.value.asInstanceOf[FloodSource]
