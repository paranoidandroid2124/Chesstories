package lila.chessjudgment.model

import chess.Color
import lila.chessjudgment.model.strategic.PlanTaxonomy.{ PlanKind, PlanTheme }
import lila.chessjudgment.model.strategic.PlanContinuity
import play.api.libs.json.*

final case class PlanActorOccurrence private (
    side: Color,
    beforeRole: String,
    afterRole: String,
    from: String,
    to: String,
    legalMoveSemanticId: String
):
  require(beforeRole.nonEmpty && afterRole.nonEmpty, "a plan actor needs exact before/after roles")
  require(from.matches("[a-h][1-8]") && to.matches("[a-h][1-8]"), "a plan actor needs exact board squares")
  require(legalMoveSemanticId.matches("[0-9a-f]{64}"), "a plan actor needs its canonical legal-move relation")

  def stableKey: String =
    List(
      side.toString.toLowerCase,
      beforeRole,
      afterRole,
      from,
      to,
      legalMoveSemanticId
    ).mkString("|")

object PlanActorOccurrence:
  private[chessjudgment] def certified(
      side: Color,
      beforeRole: String,
      afterRole: String,
      from: String,
      to: String,
      legalMoveSemanticId: String
  ): PlanActorOccurrence =
    PlanActorOccurrence(
      side,
      normalize(beforeRole),
      normalize(afterRole),
      normalize(from),
      normalize(to),
      normalize(legalMoveSemanticId)
    )

  private def normalize(raw: String): String =
    Option(raw).getOrElse("").trim.toLowerCase

case class PlanEventIdentity(
    rootMove: String,
    kind: PlanKind,
    actor: PlanActorOccurrence
):
  def goalTheme: PlanTheme = kind.theme
  def goalKey: String = kind.id
  def stableKey: String =
    List(
      rootMove,
      goalKey,
      actor.stableKey
    ).mkString("|")

object PlanEventIdentity:
  private[chessjudgment] def fromCanonical(
      rootMove: String,
      kind: PlanKind,
      actor: PlanActorOccurrence
  ): PlanEventIdentity =
    val move = Option(rootMove).getOrElse("").trim.toLowerCase
    require(move.matches("[a-h][1-8][a-h][1-8][qrbn]?"), "a plan event needs one exact legal move id")
    PlanEventIdentity(move, kind, actor)

  given Reads[PlanEventIdentity] = Reads { js =>
    for
      rootMove <- (js \ "rootMove").validate[String]
      rawKind <- (js \ "goalKind").validate[String]
      kind <- PlanKind.fromId(rawKind).map(JsSuccess(_)).getOrElse(JsError("invalid goalKind"))
      rawTheme <- (js \ "goalTheme").validateOpt[String]
      _ <- rawTheme match
        case None => JsSuccess(())
        case Some(raw) =>
          PlanTheme
            .fromId(raw)
            .filter(_ == kind.theme)
            .map(_ => JsSuccess(()))
            .getOrElse(JsError("goalTheme conflicts with goalKind"))
      rawSide <- (js \ "actorSide").validate[String]
      actorSide <- rawSide.trim.toLowerCase match
        case "white" => JsSuccess(Color.White)
        case "black" => JsSuccess(Color.Black)
        case _       => JsError("invalid actorSide")
      actorRole <- (js \ "actorRole").validate[String]
      actorAfterRole <- (js \ "actorAfterRole").validate[String]
      actorFrom <- (js \ "actorFrom").validate[String]
      actorTo <- (js \ "actorTo").validate[String]
      legalMoveSemanticId <- (js \ "actorLegalMoveSemanticId").validate[String]
    yield PlanEventIdentity(
      rootMove.trim.toLowerCase,
      kind,
      PlanActorOccurrence.certified(
        actorSide,
        actorRole,
        actorAfterRole,
        actorFrom,
        actorTo,
        legalMoveSemanticId
      )
    )
  }

  given Writes[PlanEventIdentity] = Writes { event =>
    Json.obj(
      "rootMove" -> event.rootMove,
      "goalTheme" -> event.goalTheme.id,
      "goalKind" -> event.kind.id,
      "actorSide" -> event.actor.side.toString.toLowerCase,
      "actorRole" -> event.actor.beforeRole,
      "actorAfterRole" -> event.actor.afterRole,
      "actorFrom" -> event.actor.from,
      "actorTo" -> event.actor.to,
      "actorLegalMoveSemanticId" -> event.actor.legalMoveSemanticId
    )
  }

final case class PlanPositionOccurrence private (
    fen: String,
    ply: Int
):
  def stableKey: String =
    PlanExactOccurrenceKey.product("position", List(fen, ply.toString))

object PlanPositionOccurrence:
  def from(fen: String, ply: Int): PlanPositionOccurrence =
    val exactFen = Option(fen).getOrElse("").trim
    require(exactFen.nonEmpty, "plan position occurrence requires a FEN")
    require(ply >= 0, "plan position occurrence requires a non-negative ply")
    PlanPositionOccurrence(exactFen, ply)

final case class PlanEventOccurrence private (
    event: PlanEventIdentity,
    before: PlanPositionOccurrence,
    after: PlanPositionOccurrence
):
  def stableKey: String =
    val eventKey = PlanExactOccurrenceKey.product(
      "event",
      List(
        event.rootMove,
        event.kind.id,
        event.actor.stableKey
      )
    )
    PlanExactOccurrenceKey.product("event-occurrence", List(eventKey, before.stableKey, after.stableKey))

object PlanEventOccurrence:
  def from(
      event: PlanEventIdentity,
      moveUci: String,
      ply: Int,
      fenBefore: String,
      fenAfter: String
  ): PlanEventOccurrence =
    val exactMove = Option(moveUci).getOrElse("").trim.toLowerCase
    require(
      exactMove.nonEmpty && event.rootMove == exactMove,
      "plan event occurrence move must match its event identity"
    )
    require(ply > 0, "plan event occurrence requires a positive result ply")
    PlanEventOccurrence(
      event = event,
      before = PlanPositionOccurrence.from(fenBefore, ply - 1),
      after = PlanPositionOccurrence.from(fenAfter, ply)
    )

final case class PlanSequencePathOccurrence private (
    events: List[PlanEventOccurrence]
):
  def stableKey: String =
    PlanExactOccurrenceKey.product(
      "plan-sequence-path",
      List(PlanExactOccurrenceKey.sequence(events.map(_.stableKey)))
    )

object PlanSequencePathOccurrence:
  def from(events: List[PlanEventOccurrence]): PlanSequencePathOccurrence =
    require(events.nonEmpty, "plan sequence path occurrence requires at least one event")
    val eventPlies = events.map(_.after.ply)
    require(
      eventPlies == eventPlies.sorted && eventPlies.distinct.size == eventPlies.size,
      "plan sequence path occurrences must be strictly ordered by ply"
    )
    PlanSequencePathOccurrence(events)

private object PlanExactOccurrenceKey:
  def optional(value: Option[String]): String =
    value match
      case Some(exact) => product("some", List(exact))
      case None        => product("none", Nil)

  def sequence(values: Iterable[String]): String =
    product("sequence", values.toList)

  def product(kind: String, values: Iterable[String]): String =
    sequenceParts(kind :: values.toList)

  private def sequenceParts(values: Iterable[String]): String =
    values.iterator.map(value => s"${value.length}:$value").mkString("|")

case class PlanSequenceSummary(
  transitionType: TransitionType,
  exactPathOccurrence: PlanSequencePathOccurrence,
  primaryPlanId: Option[PlanKind] = None,
  previousPlanId: Option[PlanKind] = None,
  continuity: Option[PlanContinuity] = None,
  previousEvent: Option[PlanEventIdentity] = None,
  currentEvent: Option[PlanEventIdentity] = None
)

enum TransitionType:
  case Continuation   // Same plan persists
  case Completion     // A prior event's concrete route or target is realized
  case ForcedPivot    // Threat forces abandonment
  case Opening        // First move of sequence
