package lila.app
package http

import play.api.mvc.*

import lila.common.HTTPRequest

final class PageCache(cacheApi: lila.memo.CacheApi):

  private val cache = cacheApi.notLoading[String, Result](16, "pageCache"):
    _.expireAfterWrite(1.seconds).buildAsync()

  def apply(compute: () => Fu[Result])(using ctx: Context): Fu[Result] =
    if ctx.isAnon && defaultPrefs(ctx.req) then
      cache.getFuture(cacheKey(ctx), _ => compute())
    else compute()

  private def cacheKey(ctx: Context) =
    HTTPRequest.actionName(ctx.req)

  private def defaultPrefs(req: RequestHeader) =
    lila.pref.RequestPref.fromRequest(req) == lila.pref.Pref.default
