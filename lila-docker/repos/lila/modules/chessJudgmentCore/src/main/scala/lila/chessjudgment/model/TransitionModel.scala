package lila.chessjudgment.model

import lila.chessjudgment.model.strategic.PlanTaxonomy.{ PlanKind, PlanTheme }
import lila.chessjudgment.model.strategic.PlanContinuity
import play.api.libs.json.*

case class PlanEventIdentity(
    rootMove: String,
    goalTheme: PlanTheme,
    goalKind: Option[PlanKind],
    actorRole: Option[String],
    actorFrom: Option[String],
    actorTo: Option[String],
    targets: List[String],
    results: List[String]
):
  def goalKey: String = goalKind.map(_.id).getOrElse(goalTheme.id)
  def stableKey: String =
    List(
      goalKey,
      actorRole.getOrElse(""),
      actorFrom.getOrElse(""),
      actorTo.getOrElse(""),
      targets.distinct.sorted.mkString(","),
      results.distinct.sorted.mkString(",")
    ).mkString("|")

  def sameGoal(other: PlanEventIdentity): Boolean =
    (goalKind, other.goalKind) match
      case (Some(left), Some(right)) => left == right
      case _                         => goalTheme != PlanTheme.Unknown && goalTheme == other.goalTheme

  def actorContinuesInto(other: PlanEventIdentity): Boolean =
    actorRole.nonEmpty && actorRole == other.actorRole && actorTo.nonEmpty && actorTo == other.actorFrom

  def targetEnables(other: PlanEventIdentity): Boolean =
    other.actorFrom.exists(from => targets.contains(s"square:$from"))

  def sharesTarget(other: PlanEventIdentity): Boolean =
    targets.toSet.intersect(other.targets.toSet).nonEmpty

  def continuesInto(other: PlanEventIdentity): Boolean =
    sameGoal(other) && (actorContinuesInto(other) || sharesTarget(other))

  def completedBy(other: PlanEventIdentity): Boolean =
    sameGoal(other) &&
      (actorContinuesInto(other) || targetEnables(other)) &&
      other.results.toSet.diff(results.toSet).nonEmpty

object PlanEventIdentity:
  def from(
      rootMove: String,
      support: List[PlanSupport],
      actorRole: Option[String],
      targets: List[String],
      results: List[String]
  ): PlanEventIdentity =
    val goalKind = support.collectFirst { case PlanSupport.Subplan(kind) => kind }
    val goalTheme =
      support
        .collectFirst { case PlanSupport.Theme(theme) => theme }
        .orElse(goalKind.map(_.theme))
        .getOrElse(PlanTheme.Unknown)
    val move = Option(rootMove).getOrElse("").trim.toLowerCase
    PlanEventIdentity(
      rootMove = move,
      goalTheme = goalTheme,
      goalKind = goalKind,
      actorRole = actorRole.map(_.trim.toLowerCase).filter(_.nonEmpty),
      actorFrom = Option.when(move.length >= 4)(move.take(2)),
      actorTo = Option.when(move.length >= 4)(move.slice(2, 4)),
      targets = targets.map(_.trim.toLowerCase).filter(_.nonEmpty).distinct.sorted,
      results = results.map(_.trim.toLowerCase).filter(_.nonEmpty).distinct.sorted
    )

  given Reads[PlanEventIdentity] = Reads { js =>
    for
      rootMove <- (js \ "rootMove").validate[String]
      rawTheme <- (js \ "goalTheme").validate[String]
      goalTheme <- PlanTheme.fromId(rawTheme).map(JsSuccess(_)).getOrElse(JsError("invalid goalTheme"))
      rawKind <- (js \ "goalKind").validateOpt[String]
      goalKind <- rawKind match
        case None      => JsSuccess(None)
        case Some(raw) => PlanKind.fromId(raw).map(kind => JsSuccess(Some(kind))).getOrElse(JsError("invalid goalKind"))
      actorRole <- (js \ "actorRole").validateOpt[String]
      actorFrom <- (js \ "actorFrom").validateOpt[String]
      actorTo <- (js \ "actorTo").validateOpt[String]
      targets <- (js \ "targets").validateOpt[List[String]]
      results <- (js \ "results").validateOpt[List[String]]
    yield PlanEventIdentity(
      rootMove,
      goalTheme,
      goalKind,
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
      "goalKind" -> event.goalKind.map(_.id),
      "actorRole" -> event.actorRole,
      "actorFrom" -> event.actorFrom,
      "actorTo" -> event.actorTo,
      "targets" -> event.targets,
      "results" -> event.results
    )
  }

case class PlanSequenceSummary(
  transitionType: TransitionType,
  primaryPlanId: Option[PlanId] = None,
  previousPlanId: Option[PlanId] = None,
  continuity: Option[PlanContinuity] = None,
  previousEvent: Option[PlanEventIdentity] = None,
  currentEvent: Option[PlanEventIdentity] = None
)

enum TransitionType:
  case Continuation   // Same plan persists
  case Completion     // A prior event's concrete route or target is realized
  case NaturalShift   // Phase change → new plan
  case ForcedPivot    // Threat forces abandonment
  case Opportunistic  // Unexpected opportunity
  case Opening        // First move of sequence
