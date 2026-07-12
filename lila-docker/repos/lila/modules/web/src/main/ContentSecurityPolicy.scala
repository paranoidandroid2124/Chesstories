package lila.web

import lila.core.config.AssetDomain

object ContentSecurityPolicy:
  lazy val default = lila.ui.ContentSecurityPolicy(
    defaultSrc = List("'none'"),
    connectSrc = Nil,
    styleSrc = Nil,
    frameSrc = Nil,
    workerSrc = Nil,
    imgSrc = Nil,
    mediaSrc = Nil,
    scriptSrc = Nil,
    fontSrc = Nil,
    baseUri = List("'none'"),
    objectSrc = List("'none'"),
    formAction = List("'self'")
  )

  def page(assetDomain: AssetDomain, connectSrcs: List[String]) =
    lila.ui.ContentSecurityPolicy(
      defaultSrc = List("'self'", assetDomain.value),
      connectSrc = "'self'" :: "blob:" :: "data:" :: connectSrcs,
      styleSrc = List("'self'", "'unsafe-inline'", assetDomain.value),
      frameSrc = List(
        "'self'",
        assetDomain.value,
        "www.youtube.com",
        "www.youtube-nocookie.com",
        "player.vimeo.com"
      ),
      workerSrc = List("'self'", assetDomain.value, "blob:"),
      imgSrc = List("'self'", "blob:", "data:", "*"),
      mediaSrc = List("'self'", "blob:", assetDomain.value),
      scriptSrc = List("'self'", "blob:", assetDomain.value),
      fontSrc = List("'self'", assetDomain.value),
      baseUri = List("'none'"),
      objectSrc = List("'none'"),
      formAction = List("'self'")
    )

  def embed(assetDomain: AssetDomain) =
    lila.ui.ContentSecurityPolicy(
      defaultSrc = List("'self'", assetDomain.value),
      connectSrc = List("'self'", "blob:", "data:", assetDomain.value),
      styleSrc = List("'self'", "'unsafe-inline'", assetDomain.value),
      frameSrc = Nil,
      workerSrc = Nil,
      imgSrc = List("'self'", "blob:", "data:", "*"),
      mediaSrc = Nil,
      scriptSrc = List("'self'", assetDomain.value),
      fontSrc = List("'self'", assetDomain.value),
      baseUri = List("'none'"),
      objectSrc = List("'none'"),
      formAction = List("'self'")
    )

  def render(csp: lila.ui.ContentSecurityPolicy): String =
    import csp.*
    List(
      "default-src " -> defaultSrc,
      "connect-src " -> connectSrc,
      "style-src " -> styleSrc,
      "frame-src " -> frameSrc,
      "worker-src " -> workerSrc,
      "img-src " -> imgSrc,
      "media-src " -> mediaSrc,
      "script-src " -> scriptSrc,
      "font-src " -> fontSrc,
      "base-uri " -> baseUri,
      "object-src " -> objectSrc,
      "form-action " -> formAction
    ).collect {
      case (directive, sources) if sources.nonEmpty =>
        sources.mkString(directive, " ", ";")
    }.mkString(" ")
