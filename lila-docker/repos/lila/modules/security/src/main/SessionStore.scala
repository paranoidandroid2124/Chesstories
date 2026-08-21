package lila.security

import reactivemongo.api.bson.*

import scala.concurrent.blocking

import lila.core.userId.UserId
import lila.core.lilaism.Core.*
import lila.core.id.SessionId
import lila.db.dsl.{ *, given }
import java.time.Instant
import scala.concurrent.duration.*

case class AuthInfo(userId: UserId)

final class SessionStore(val coll: Coll, cacheApi: lila.memo.CacheApi)(using executor: Executor):

  private val authCache = cacheApi[SessionId, Option[AuthInfo]](65_536, "security.session.info"):
    _.expireAfterAccess(5.minutes).buildAsyncFuture[SessionId, Option[AuthInfo]]: id =>
      coll
        .find($doc("_id" -> id, "up" -> $ne(false)), authInfoProjection.some)
        .one[Bdoc]
        .map:
          _.flatMap: doc =>
            if doc.getAsOpt[Instant]("date").forall(_.isBefore(nowInstant.minusHours(12))) then
              coll.updateFieldUnchecked($id(id), "date", nowInstant)
            doc.getAsOpt[UserId]("user").map(AuthInfo.apply)

  def authInfo(sessionId: SessionId) = authCache.get(sessionId)

  private val authInfoProjection = $doc("user" -> true, "date" -> true, "_id" -> false)
  private def uncache(sessionId: SessionId) =
    blocking { blockingUncache(sessionId) }
  private def uncacheAllOf(userId: UserId): Funit =
    coll.distinctEasy[SessionId, Seq]("_id", $doc("user" -> userId)).map { ids =>
      blocking:
        ids.foreach(blockingUncache)
    }

  private def blockingUncache(sessionId: SessionId) =
    authCache.underlying.synchronous.invalidate(sessionId)

  private[security] def save(
      sessionId: SessionId,
      userId: UserId
  ): Funit =
    coll.insert
      .one:
        $doc(
          "_id" -> sessionId,
          "user" -> userId,
          "date" -> nowInstant
        )
      .void

  def delete(sessionId: SessionId): Funit =
    for _ <- coll.delete.one($id(sessionId))
    yield uncache(sessionId)

  def closeAllSessionsOf(userId: UserId): Funit =
    coll.delete.one($doc("user" -> userId)).void >> uncacheAllOf(userId)
