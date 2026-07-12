package controllers

import play.api.mvc.*
import lila.app.*

final class User(
    override val env: Env
) extends LilaController(env):

  def show(username: UserStr) = Open:
    Ok(s"User profile for ${username.value} - features simplified").toFuccess

  def myself = Auth { _ ?=> me ?=>
    Redirect(routes.User.show(me.username))
  }
