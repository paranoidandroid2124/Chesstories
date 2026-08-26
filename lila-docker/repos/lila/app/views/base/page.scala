package views.base

import scalalib.StringUtils.escapeHtmlRaw

import lila.app.UiEnv.{ *, given }
import lila.common.String.html.safeJsonValue
import lila.ui.{ RenderedPage, PageFlags }
import lila.web.ui.ShellPrimitives

object page:

  val pieceSetImages = lila.web.ui.PieceSetImages(assetHelper)

  val ui = lila.web.ui.layout(helpers, assetHelper)
  import ui.*

  private val topnav = lila.web.ui.TopNav(helpers)

  private def metaThemeColor(using ctx: Context): Frag =
    raw(s"""<meta name="theme-color" content="${ctx.pref.themeColor}">""")

  private def boardPreload(using ctx: Context) = frag(
    imagePreload(assetUrl(s"images/board/${ctx.pref.currentTheme.file}"))
  )

  def boardStyle(zoomable: Boolean)(using ctx: lila.api.Context) =
    s"---board-opacity:${ctx.pref.board.opacity};" +
      s"---board-brightness:${ctx.pref.board.brightness};" +
      s"---board-hue:${ctx.pref.board.hue};" +
      zoomable.so(s"---zoom:$pageZoom;")

  def apply(p: Page)(using ctx: lila.api.PageContext): RenderedPage =
    import ctx.pref
    val allModules: EsmList = p.modules ++
      p.pageModule.fold(Nil)(module => esmPage(module.name))
    val pageFrag = frag(
      doctype,
      page.ui.htmlTag(
        (ctx.impersonatedBy.isEmpty && !ctx.blind)
          .option(cls := ctx.pref.themeColorClass),
        topComment,
        head(
          charset,
          viewport,
          metaCsp(p.csp.map(_(defaultCsp))),
          metaThemeColor,
          st.headTitle:
            val prodTitle = p.fullTitle | s"${p.title} • $siteName"
            if env.mode.isProd then prodTitle
            else s"${ctx.me.so(_.username.value + " ")} $prodTitle"
          ,
          cssTag("lib.theme.all"),
          cssTag("site"),
          ctx.blind.option(cssTag("bits.blind")),
          p.cssKeys.map(cssTag),
          meta(
            content := p.openGraph.fold(
              "Chesstory explains why a chess position changed, move by move, with the engine evidence beside it."
            )(o => o.description),
            name := "description"
          ),
          link(rel := "mask-icon", href := staticAssetUrl("logo/chesstory.svg"), attr("color") := "black"),
          favicons,
          (p.flags(PageFlags.noRobots) || !netConfig.crawlable).option:
            raw("""<meta content="noindex, nofollow" name="robots">""")
          ,
          noTranslate,
          p.openGraph.map(lila.web.ui.openGraph),
          p.atomLinkTag,
          (pref.bg == lila.pref.Pref.Bg.TRANSPARENT).option(pref.bgImgOrDefault).map { loc =>
            val url =
              if loc.startsWith("/assets/") then assetUrl(loc.drop(8))
              else escapeHtmlRaw(loc).replace("&amp;", "&")
            raw(s"""<style id="bg-data">html.transp::before{background-image:url("$url");}</style>""")
          },
          fontsPreload,
          boardPreload,
          manifests,
          p.withHrefLangs.map(hrefLangs),
          sitePreload(allModules),
          chesstoryFontFaceCss,
          pieceSetImages.load(ctx.pref.currentPieceSet.name),
          (ctx.pref.bg === lila.pref.Pref.Bg.SYSTEM || ctx.impersonatedBy.isDefined)
            .so(systemThemeScript(ctx.nonce))
        ).pipe(p.transformHead),
        st.body(
          cls := {
            val baseClass = s"${pref.currentBg} coords-${pref.coordsClass}"
            List(
              baseClass -> true,
              "simple-board" -> pref.simpleBoard,
              "piece-letter" -> pref.pieceNotationIsLetter,
              "blind-mode" -> ctx.blind,
              "kid" -> ctx.kid.yes,
              "mobile" -> lila.common.HTTPRequest.isMobileBrowser(ctx.req),
              "no-rating" -> !pref.showRatings,
              "zen" -> pref.isZen
            )
          },
          dataUser := ctx.userId,
          dataUsername := ctx.username,
          dataSoundSet := pref.currentSoundSet.toString,
          attr("data-socket-domains") := netConfig.socketDomains.mkString(","),
          dataAssetUrl := netConfig.assetBaseUrl.value,
          dataAssetVersion := assetVersion,
          dataNonce := ctx.nonce,
          dataTheme := pref.currentBg,
          dataBoard := pref.currentTheme.name,
          dataPieceSet := pref.currentPieceSet.name,
          attr("data-shell-header-id") := ShellPrimitives.headerId,
          attr("data-shell-nav-id") := ShellPrimitives.navId,
          attr("data-shell-nav-toggle-id") := ShellPrimitives.navToggleId,
          dataAnnounce := env.announceApi.current.map(a => safeJsonValue(a.json).value),
          attr("data-brand-v3-shell") := "1",
          attr("data-brand-v3-analysis-surface") := "1",
          attr("data-brand-explorer-proxy") := "1",
          style := boardStyle(p.flags(PageFlags.zoom))
        )(
          a(cls := "skip-link", href := "#main-wrap")("Skip to main content"),
          Option.unless(p.flags(PageFlags.noHeader)):
            ui.siteHeader(
              isAppealUser = ctx.isAppealUser,
              error = ctx.data.error,
              topnav = topnav(using ctx)
            )
          ,
          div(
            id := "main-wrap",
            tabindex := -1,
            cls := List(
              "full-screen-force" -> p.flags(PageFlags.fullScreen),
              "is2d" -> true
            )
          )(p.transform(p.body)),
          Option.when(!p.flags(PageFlags.noHeader) && !p.flags(PageFlags.fullScreen))(ui.siteFooter),
          bottomHtml,
          ctx.nonce.map(inlineJs(_, allModules)),
          modulesInit(allModules, ctx.nonce),
          p.pageModule.map { mod =>
            frag(
              jsonScript(mod.data),
              ctx.nonce.map(n =>
                embedJsUnsafe(s"site.load.then(() => site.asset.loadEsmPage('${mod.name}'))")(n.some)
              )
            )
          }
        )
      )
    )
    RenderedPage(pageFrag.render)
