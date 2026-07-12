package lila.core
package misc

import lila.core.id.GameId
import lila.core.userId.*
import lila.core.user.Me

trait AtInstant[A]:
  def apply(a: A): Instant
  extension (a: A) inline def atInstant: Instant = apply(a)
object AtInstant:
  given atInstantOrdering: [A: AtInstant] => Ordering[A] = Ordering.by[A, Instant](_.atInstant)

package lpv:
  import _root_.chess.format.pgn.PgnStr
  enum LpvEmbed:
    case PublicPgn(pgn: PgnStr)
    case PrivateStudy
  type LinkRender = (String, String) => Option[scalatags.Text.Frag]
  enum Lpv:
    case AllPgnsFromText(text: String, max: Max, promise: Promise[Map[String, LpvEmbed]])
    case LinkRenderFromText(text: String, promise: Promise[LinkRender])

package mailer:
  case class CorrespondenceOpponent(
      opponentId: Option[UserId],
      remainingTime: Option[java.time.Duration],
      gameId: GameId
  )
  case class CorrespondenceOpponents(userId: UserId, opponents: List[CorrespondenceOpponent])
