package lila.chessjudgment.model

import chess.Color
final case class PassedPawnResultActorOccurrence private (
    side: Color,
    beforeRole: String,
    afterRole: String,
    from: String,
    to: String,
    legalMoveSemanticId: String
):
  require(beforeRole.nonEmpty && afterRole.nonEmpty, "a passed-pawn result actor needs exact before/after roles")
  require(from.matches("[a-h][1-8]") && to.matches("[a-h][1-8]"), "a passed-pawn result actor needs exact board squares")
  require(legalMoveSemanticId.matches("[0-9a-f]{64}"), "a passed-pawn result actor needs its canonical legal-move relation")

  def stableKey: String =
    List(
      side.toString.toLowerCase,
      beforeRole,
      afterRole,
      from,
      to,
      legalMoveSemanticId
    ).mkString("|")

object PassedPawnResultActorOccurrence:
  private[chessjudgment] def certified(
      side: Color,
      beforeRole: String,
      afterRole: String,
      from: String,
      to: String,
      legalMoveSemanticId: String
  ): PassedPawnResultActorOccurrence =
    PassedPawnResultActorOccurrence(
      side,
      normalize(beforeRole),
      normalize(afterRole),
      normalize(from),
      normalize(to),
      normalize(legalMoveSemanticId)
    )

  private def normalize(raw: String): String =
    Option(raw).getOrElse("").trim.toLowerCase

case class PassedPawnResultEventIdentity(
    rootMove: String,
    kind: PassedPawnResultKind,
    actor: PassedPawnResultActorOccurrence
):
  def resultKindKey: String = kind.id
  def stableKey: String =
    List(
      rootMove,
      resultKindKey,
      actor.stableKey
    ).mkString("|")

object PassedPawnResultEventIdentity:
  private[chessjudgment] def fromCanonical(
      rootMove: String,
      kind: PassedPawnResultKind,
      actor: PassedPawnResultActorOccurrence
  ): PassedPawnResultEventIdentity =
    val move = Option(rootMove).getOrElse("").trim.toLowerCase
    require(move.matches("[a-h][1-8][a-h][1-8][qrbn]?"), "a passed-pawn result event needs one exact legal move id")
    PassedPawnResultEventIdentity(move, kind, actor)

final case class PassedPawnResultPositionOccurrence private (
    fen: String,
    ply: Int
):
  def stableKey: String =
    PassedPawnResultOccurrenceKey.product("position", List(fen, ply.toString))

object PassedPawnResultPositionOccurrence:
  def from(fen: String, ply: Int): PassedPawnResultPositionOccurrence =
    val exactFen = Option(fen).getOrElse("").trim
    require(exactFen.nonEmpty, "a passed-pawn result position occurrence requires a FEN")
    require(ply >= 0, "a passed-pawn result position occurrence requires a non-negative ply")
    PassedPawnResultPositionOccurrence(exactFen, ply)

final case class PassedPawnResultEventOccurrence private (
    event: PassedPawnResultEventIdentity,
    before: PassedPawnResultPositionOccurrence,
    after: PassedPawnResultPositionOccurrence
):
  def stableKey: String =
    val eventKey = PassedPawnResultOccurrenceKey.product(
      "event",
      List(
        event.rootMove,
        event.kind.id,
        event.actor.stableKey
      )
    )
    PassedPawnResultOccurrenceKey.product("event-occurrence", List(eventKey, before.stableKey, after.stableKey))

object PassedPawnResultEventOccurrence:
  def from(
      event: PassedPawnResultEventIdentity,
      moveUci: String,
      ply: Int,
      fenBefore: String,
      fenAfter: String
  ): PassedPawnResultEventOccurrence =
    val exactMove = Option(moveUci).getOrElse("").trim.toLowerCase
    require(
      exactMove.nonEmpty && event.rootMove == exactMove,
      "passed-pawn result event occurrence move must match its event identity"
    )
    require(ply > 0, "a passed-pawn result event occurrence requires a positive result ply")
    PassedPawnResultEventOccurrence(
      event = event,
      before = PassedPawnResultPositionOccurrence.from(fenBefore, ply - 1),
      after = PassedPawnResultPositionOccurrence.from(fenAfter, ply)
    )

private object PassedPawnResultOccurrenceKey:
  def product(kind: String, values: Iterable[String]): String =
    sequenceParts(kind :: values.toList)

  private def sequenceParts(values: Iterable[String]): String =
    values.iterator.map(value => s"${value.length}:$value").mkString("|")
