package lila.web

import play.api.libs.json.Json

import lila.common.Json.given
import lila.core.config.NetConfig

object StaticContent:

  val robotsTxt = """User-agent: *
Allow: /
Disallow: /game/export/
Disallow: /games/export/
Disallow: /api/
Disallow: /opening/config/
"""

  def manifest(net: NetConfig) =
    Json.obj(
      "name" -> net.siteName,
      "short_name" -> net.siteName,
      "start_url" -> "/",
      "display" -> "standalone",
      "background_color" -> "#161A25",
      "theme_color" -> "#161A25",
      "description" -> "AI chess analysis, simplified.",
      "icons" -> List(32, 64, 128, 192, 256, 512).map: size =>
        Json.obj(
          "src" -> s"//${net.assetDomain}/assets/logo/chesstory-favicon-$size.png",
          "sizes" -> s"${size}x$size",
          "type" -> "image/png"
        ),
      "related_applications" -> Json.arr()
    )
