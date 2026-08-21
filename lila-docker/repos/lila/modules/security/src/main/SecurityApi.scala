package lila.security

import lila.common.*
import lila.core.lilaism.Core.*
import lila.core.id.SessionId
import lila.core.email.EmailAddress
import lila.core.userId.UserStr
import lila.core.security.ClearPassword

final class SecurityApi(
    userRepo: lila.user.UserRepo,
    sessionStore: SessionStore,
    authenticator: Authenticator
)(using Executor):

  val sessionIdKey = "sid"

  def reqSessionId(req: play.api.mvc.RequestHeader): Option[SessionId] =
    req.cookies.get(sessionIdKey).map(_.value).map(SessionId.apply)

  def saveAuthentication(userId: lila.core.userId.UserId): Fu[SessionId] =
    val sid = SessionId(java.util.UUID.randomUUID.toString)
    sessionStore.save(sid, userId) >> Future.successful(sid)

  case class AuthSuccess(user: lila.core.user.User)

  def authenticate(
      login: String,
      pass: String
  ): Fu[Option[AuthSuccess]] =
    val trimmed = login.trim
    val password = ClearPassword(pass)
    val withPassword = PasswordAndToken(password, None)

    val authUser: Fu[Option[lila.core.user.User]] =
      EmailAddress.from(trimmed) match
        case Some(address) =>
          authenticator.authenticateByEmail(address.normalize, withPassword)
        case None =>
          UserStr
            .read(trimmed)
            .fold[Fu[Option[lila.core.user.User]]](fuccess(None))(u => authenticator.authenticateById(u.id, withPassword))

    authUser.map:
      case Some(user) if user.enabled.yes =>
        AuthSuccess(user).some
      case _ =>
        None

  def logout(sid: SessionId): Funit =
    sessionStore.delete(sid)

  def restoreUser(req: play.api.mvc.RequestHeader): Fu[Option[lila.core.user.User]] =
    reqSessionId(req).fold(fuccess(None)): sid =>
      sessionStore
        .authInfo(sid)
        .flatMap:
          case Some(info) => userRepo.byId(info.userId)
          case None       => fuccess(None)
