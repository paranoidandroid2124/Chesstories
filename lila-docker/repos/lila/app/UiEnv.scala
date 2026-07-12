package lila.app

import lila.ui.*
import lila.web.ui.*

object UiEnv
    extends ScalatagsTemplate
    with SecurityHelper
    with Helpers
    with AssetHelper
    with AssetFullHelper:

  trait IsOnline:
    def exec(userId: UserId): Boolean

  export lila.core.lilaism.Lilaism.{ *, given }
  export lila.core.id.ImageId
  export lila.common.extensions.*
  export lila.common.String.html.richText
  export lila.ui.{ Page, Nonce, OpenGraph, PageModule, EsmList, Icon }
  def esmPage(name: String): EsmList = List(lila.ui.Esm(name).some)
  export lila.api.Context.{ ctxToTranslate as _, *, given }
  
  override val siteName = "Chesstory"

  private var envVar: Option[Env] = None
  def setEnv(e: Env) = envVar = Some(e)
  def env: Env = envVar.get

  def netConfig = env.net

  given lila.core.config.NetDomain = env.net.domain
  given (using ctx: PageContext): Option[Nonce] = ctx.nonce

  // helpers dependencies
  override def assetBaseUrl = netConfig.assetBaseUrl.value
  override def netBaseUrl = netConfig.baseUrl.value
  override def assetVersion: String = lila.core.net.AssetVersion.current.value
  override def safeJsonValue(v: play.api.libs.json.JsValue) = lila.common.String.html.safeJsonValue(v)
  override def preload(url: String, as: String, crossOrigin: Boolean, tpe: Option[String]) = 
    super[AssetFullHelper].preload(url, as, crossOrigin, tpe)
  override def embedJsUnsafe(code: String)(nonce: Option[Nonce]) = 
    super[AssetFullHelper].embedJsUnsafe(code)(nonce)
  override def iconTag(icon: Icon): Tag = super[Helpers].iconTag(icon)
  override def iconTag(icon: Icon, text: Frag): Tag = super[Helpers].iconTag(icon, text)

  def routeUrl = netConfig.routeUrl
  def homeUrl(using ctx: Context): String =
    if ctx.isAuth then "/home" else "/"
  def isOnline: IsOnline = new IsOnline:
    def exec(userId: UserId) = false
  def lightUserSync = env.user.lightUserSync
  def manifest = env.web.manifest
  def analyseEndpoints = env.web.analyseEndpoints

  def helpers: Helpers = this
  def assetHelper: AssetFullHelper = this
