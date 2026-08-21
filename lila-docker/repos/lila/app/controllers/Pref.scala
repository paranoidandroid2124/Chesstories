package controllers

import java.net.URLEncoder
import lila.app.*

object Pref:
  private[controllers] val canonicalCateg = "display"

  private[controllers] def canonicalCategory(categSlug: String): String =
    Option(categSlug)
      .map(_.trim.toLowerCase)
      .filter(_ == canonicalCateg)
      .getOrElse(canonicalCateg)

final class Pref(
    env: Env
) extends LilaController(env):

  def form(categSlug: String) = Open:
    val canonicalCateg = Pref.canonicalCategory(categSlug)
    if categSlug != canonicalCateg then Redirect(routes.Pref.form(canonicalCateg)).toFuccess
    else
      ctx.me.fold(
        {
          val referrer = routes.Pref.form(canonicalCateg).url
          val encoded = URLEncoder.encode(referrer, "UTF-8")
          fuccess(Redirect(s"${routes.Auth.login.url}?referrer=$encoded"))
        }
      ) { me =>
        env.pref.api.get(me).flatMap: pref =>
          Ok.page(views.account.pref(me, env.security.forms.prefOf(pref)))
      }

  def formApply = AuthBody { ctx ?=> me ?=>
    implicit val _ = ctx
    bindForm(env.security.forms.prefOf(ctx.pref))(
      err =>
        negotiate(
          BadRequest.page(views.account.pref(me, err)),
          BadRequest(errorsAsJson(err))
      ),
      pref =>
        env.pref.api.setPref(me, pref) >>
          negotiate(
            fuccess(
              Redirect(s"${routes.Pref.form(Pref.canonicalCateg).url}?notice=preferences-updated")
            ),
            fuccess(NoContent)
          )
    )
  }

  def set(name: String) = Open { ctx ?=>
    get("v").fold(fuccess(BadRequest)): v =>
      name match
        case "zoom" =>
          v.toIntOption
            .filter(z => z >= 0 && z <= 100)
            .fold(fuccess(BadRequest))(_ => fuccess(Ok))

        case "bg" =>
          lila.pref.Pref.Bg.fromString
            .get(v)
            .fold(fuccess(BadRequest)): _ =>
              ctx.me.fold(fuccess(Ok)) { me =>
                env.pref.api
                  .set(me, name, v)
                  .inject(Ok)
                  .recover { case _: Exception => BadRequest }
              }

        case _ =>
          ctx.me.fold(fuccess(Unauthorized)): me =>
            env.pref.api
              .set(me, name, v)
              .inject(Ok)
              .recover { case _: Exception => BadRequest }
  }

  def options = Auth { ctx ?=> me ?=>
    env.pref.api.get(me).map: pref =>
      Ok(lila.pref.ui.DasherJson(pref, none)(using ctx)).as(JSON)
  }
