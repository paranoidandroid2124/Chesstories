package lila.chessjudgment.model

import lila.chessjudgment.model.strategic.PlanTaxonomy.{ PlanKind, PlanTheme }
import lila.chessjudgment.model.strategic.PlanContinuity
import play.api.libs.json.*

case class PlanEventIdentity(
    rootMove: String,
    kind: PlanKind,
    actorRole: Option[String],
    actorFrom: Option[String],
    actorTo: Option[String],
    targets: List[String],
    results: List[String]
):
  def goalTheme: PlanTheme = kind.theme
  def goalKey: String = kind.id
  def stableKey: String =
    List(
      rootMove,
      goalKey,
      actorRole.getOrElse(""),
      actorFrom.getOrElse(""),
      actorTo.getOrElse(""),
      targets.distinct.sorted.mkString(","),
      results.distinct.sorted.mkString(",")
    ).mkString("|")

object PlanEventIdentity:
  def from(
      rootMove: String,
      kind: PlanKind,
      actorRole: Option[String],
      targets: List[String],
      results: List[String]
  ): PlanEventIdentity =
    val move = Option(rootMove).getOrElse("").trim.toLowerCase
    PlanEventIdentity(
      rootMove = move,
      kind = kind,
      actorRole = actorRole.map(_.trim.toLowerCase).filter(_.nonEmpty),
      actorFrom = Option.when(move.length >= 4)(move.take(2)),
      actorTo = Option.when(move.length >= 4)(move.slice(2, 4)),
      targets = targets.map(_.trim.toLowerCase).filter(_.nonEmpty).distinct.sorted,
      results = results.map(_.trim.toLowerCase).filter(_.nonEmpty).distinct.sorted
    )

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
      actorRole <- (js \ "actorRole").validateOpt[String]
      actorFrom <- (js \ "actorFrom").validateOpt[String]
      actorTo <- (js \ "actorTo").validateOpt[String]
      targets <- (js \ "targets").validateOpt[List[String]]
      results <- (js \ "results").validateOpt[List[String]]
    yield PlanEventIdentity(
      rootMove,
      kind,
      actorRole,
      actorFrom,
      actorTo,
      targets.getOrElse(Nil),
      results.getOrElse(Nil)
    )
  }

  given Writes[PlanEventIdentity] = Writes { event =>
    Json.obj(
      "rootMove" -> event.rootMove,
      "goalTheme" -> event.goalTheme.id,
      "goalKind" -> event.kind.id,
      "actorRole" -> event.actorRole,
      "actorFrom" -> event.actorFrom,
      "actorTo" -> event.actorTo,
      "targets" -> event.targets,
      "results" -> event.results
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
        PlanExactOccurrenceKey.optional(event.actorRole),
        PlanExactOccurrenceKey.optional(event.actorFrom),
        PlanExactOccurrenceKey.optional(event.actorTo),
        PlanExactOccurrenceKey.sequence(event.targets.distinct.sorted),
        PlanExactOccurrenceKey.sequence(event.results.distinct.sorted)
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
