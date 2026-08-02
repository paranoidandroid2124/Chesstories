package controllers

import play.api.data.*
import play.api.data.Forms.*
import play.api.libs.json.*
import play.api.mvc.{ DiscardingCookie, Result }

import lila.app.*
import lila.core.email.EmailAddress
import lila.core.security.ClearPassword
import lila.core.user.UserDelete
import lila.security.PasswordAndToken

final class Account(
    env: Env
) extends LilaController(env):

  private val forms = new lila.user.UserForm

  private case class PasswordChangeData(
      currentPassword: Option[String],
      newPassword: String,
      confirmPassword: String
  )

  private val emailChangeForm = Form(
    single("email" -> env.security.forms.signup.emailField)
  )

  private val passwordChangeForm = Form(
    mapping(
      "currentPassword" -> optional(nonEmptyText),
      "newPassword" -> nonEmptyText(minLength = 4, maxLength = 999),
      "confirmPassword" -> nonEmptyText(minLength = 4, maxLength = 999)
    )(PasswordChangeData.apply)(data =>
      Some((data.currentPassword, data.newPassword, data.confirmPassword))
    )
      .verifying("New passwords do not match.", data => data.newPassword == data.confirmPassword)
  )

  private val deleteRequestForm = Form(
    single("confirm" -> nonEmptyText.verifying("Type DELETE to confirm.", _.trim == "DELETE"))
  )

  private def securityPage(
      me: UserModel,
      emailForm: Form[EmailAddress] = emailChangeForm,
      passwordForm: Form[PasswordChangeData] = passwordChangeForm
  )(render: lila.ui.Page => Fu[Result])(using Context): Fu[Result] =
    env.security.authenticator.hasPassword(me.id).flatMap: hasPassword =>
      render(views.account.security(me, hasPassword, emailForm, passwordForm))

  private def deletePage(
      me: UserModel,
      pendingRequest: Option[UserDelete],
      form: Form[String]
  )(render: lila.ui.Page => Fu[Result])(using Context): Fu[Result] =
    render(views.account.delete(me, pendingRequest, form))

  def profile = Auth { ctx ?=> me ?=>
    val form = forms.username(me)
    Ok.page(views.account.profile(me, form))
  }

  def profileApply = AuthBody { ctx ?=> me ?=>
    bindForm(forms.username(me))(
      err => BadRequest.page(views.account.profile(me, err)),
      name =>
        if name.id == me.username.id then
          env.user.api.updateUsername(me.id, name) inject
            Redirect(routes.Account.profile).flashing("success" -> "Username updated")
        else
          fuccess(BadRequest("Username mismatch"))
    )
  }

  def info = Auth { ctx ?=> me ?=>
    negotiateJson:
      Ok(
        env.user.jsonView.full(me) ++ Json.obj(
            "prefs" -> Json.obj(),
            "nowPlaying" -> JsArray(),
            "online" -> true
          )
          .add("kid" -> false)
          .add("troll" -> me.marks.troll)
          .add("announce" -> env.announceApi.current.map(_.json))
      ).hasPersonalData
  }

  def nowPlaying = Auth { _ ?=> _ ?=>
    negotiateJson(JsonOk(Json.obj("nowPlaying" -> JsArray())))
  }

  def apiMe = Auth { _ ?=> me ?=>
    JsonOk(env.user.jsonView.full(me))
  }

  def apiNowPlaying = Auth { _ ?=> _ ?=>
    JsonOk(Json.obj("nowPlaying" -> JsArray()))
  }

  def dasher = Auth { _ ?=> me ?=>
    negotiateJson:
      Ok:
        lila.common.Json.lightUser.write(me.light) ++ Json.obj(
            "prefs" -> Json.obj()
        )
  }

  def close = Auth { ctx ?=> me ?=>
    Ok.page(views.account.close(me))
  }

  def closeConfirm = Auth { ctx ?=> me ?=>
    val closeF = env.user.api.disable(me.id) >> env.security.sessionStore.closeAllSessionsOf(me.id)
    closeF.inject(
      Redirect(routes.Main.landing)
        .flashing("success" -> "Account closed. Contact support from the same email address if you need it reopened.")
        .discardingCookies(DiscardingCookie(env.security.api.sessionIdKey))
    )
  }

  def delete = Auth { ctx ?=> me ?=>
    env.user.deleteRequestRepo.byId(me.id).flatMap: pendingRequest =>
      deletePage(me, pendingRequest, deleteRequestForm)(Ok.page)
  }

  def deleteConfirm = AuthBody { ctx ?=> me ?=>
    bindForm(deleteRequestForm)(
      err =>
        env.user.deleteRequestRepo.byId(me.id).flatMap: pendingRequest =>
          deletePage(me, pendingRequest, err)(BadRequest.page),
      _ =>
        val requestF =
          env.user.deleteRequestRepo.schedule(me.id) >>
            env.user.api.disable(me.id) >>
            env.security.sessionStore.closeAllSessionsOf(me.id)
        requestF.inject(
          Redirect(routes.Main.landing)
            .flashing("success" -> "Deletion request submitted. Your account has been closed and queued for manual erasure.")
            .discardingCookies(DiscardingCookie(env.security.api.sessionIdKey))
        )
    )
  }

  def username = profile
  def usernameApply = profileApply

  def passwd = Auth { ctx ?=> me ?=>
    securityPage(me)(Ok.page)
  }

  def passwdApply = AuthBody { ctx ?=> me ?=>
    bindForm(passwordChangeForm)(
      err => securityPage(me, passwordForm = err)(BadRequest.page),
      data =>
        env.security.authenticator.hasPassword(me.id).flatMap: hasPassword =>
          val verifyCurrentPassword =
            if !hasPassword then fuccess(true)
            else
              data.currentPassword.fold(fuccess(false)): currentPassword =>
                env.security.authenticator
                  .authenticateById(me.id, PasswordAndToken(ClearPassword(currentPassword), None))
                  .map(_.isDefined)

          verifyCurrentPassword.flatMap:
            case false =>
              val withError =
                if hasPassword && data.currentPassword.isEmpty then
                  passwordChangeForm.fill(data).withError("currentPassword", "Enter your current password.")
                else
                  passwordChangeForm.fill(data).withError("currentPassword", "Current password is incorrect.")
              securityPage(me, passwordForm = withError)(BadRequest.page)
            case true if data.currentPassword.contains(data.newPassword) =>
              securityPage(
                me,
                passwordForm = passwordChangeForm.fill(data).withError("newPassword", "Choose a different password.")
              )(BadRequest.page)
            case true =>
              val newPassword = ClearPassword(data.newPassword)
              env.security.pwnedApi.isPwned(newPassword).flatMap:
                case pwned if pwned.yes =>
                  securityPage(
                    me,
                    passwordForm = passwordChangeForm.fill(data).withError(
                      "newPassword",
                      "This password is known in data breaches. Choose another one."
                    )
                  )(BadRequest.page)
                case _ =>
                  env.security.authenticator
                    .setPassword(me.id, newPassword)
                    .inject(
                      Redirect(routes.Account.passwd).flashing(
                        "success" ->
                          (if hasPassword then "Password updated." else "Password set. You can now log in with it.")
                      )
                    )
    )
  }

  def emailApply = AuthBody { ctx ?=> me ?=>
    bindForm(emailChangeForm)(
      err => securityPage(me, emailForm = err)(BadRequest.page),
      newEmail =>
        val normalized = EmailAddress(newEmail.normalize.value)
        if me.email.exists(existing => EmailAddress(existing.normalize.value) == normalized) then
          securityPage(
            me,
            emailForm = emailChangeForm.fill(normalized).withError("email", "You are already using this email.")
          )(BadRequest.page)
        else
          val tooMany =
            securityPage(
              me,
              emailForm =
                emailChangeForm.fill(normalized).withGlobalError("Too many email change attempts. Try again later.")
            )(BadRequest.page)
          limit.signup(ctx.ip, tooMany):
            env.user.repo.byEmail(normalized).flatMap:
              case Some(existing) if existing.id != me.id =>
                securityPage(
                  me,
                  emailForm = emailChangeForm.fill(normalized).withError("email", "Email already in use.")
                )(BadRequest.page)
              case _ =>
                val token = env.security.emailChangeToken.generate(me.id, normalized, 30.minutes)
                val url = s"${env.baseUrl}${routes.Account.emailConfirm(token).url}"
                env.mailer.automaticEmail.emailChangeConfirm(me, normalized, url).inject(
                  Redirect(routes.Account.passwd)
                    .flashing("success" -> s"Confirmation email sent to ${normalized.conceal}.")
                )
    )
  }

  private def withEmailChange(token: String)(
      f: (UserModel, EmailAddress) => Fu[Result]
  )(using Context): Fu[Result] =
    env.security.emailChangeToken.read(token).fold(
      Ok.page(views.auth.loginError("Invalid or expired email change link."))
    ): (userId, email) =>
      env.user.repo.byId(userId).flatMap:
        case None =>
          Ok.page(views.auth.loginError("Invalid or expired email change link."))
        case Some(user) =>
          env.user.repo.byEmail(email).flatMap:
            case Some(existing) if existing.id != userId =>
              Ok.page(views.auth.loginError("This email address is already in use."))
            case _ =>
              f(user, email)

  def emailConfirm(token: String) = Open:
    withEmailChange(token): (_, _) =>
      Ok.page(views.auth.confirmEmail(routes.Account.emailConfirmApply(token).url))

  def emailConfirmApply(token: String) = OpenBody:
    withEmailChange(token): (user, email) =>
      env.user.repo.updateEmail(user.id, email).inject:
        val redirect =
          if ctx.me.exists(_.id == user.id) then routes.Account.passwd else routes.Auth.login
        Redirect(redirect).flashing("success" -> "Email address updated.")
