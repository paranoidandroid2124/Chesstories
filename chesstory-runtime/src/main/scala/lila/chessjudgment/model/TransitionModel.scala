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
  def goalKind: Option[PlanKind] = Some(kind)
  def goalTheme: PlanTheme = kind.theme
  def goalKey: String = kind.id
  def stableKey: String =
    List(
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
      support: List[PlanSupport],
      actorRole: Option[String],
      targets: List[String],
      results: List[String]
  ): PlanEventIdentity =
    val kind = support
      .collectFirst { case PlanSupport.Subplan(value) => value }
      .getOrElse(throw new IllegalArgumentException("plan event identity requires canonical plan kind"))
    require(
      support.collect { case PlanSupport.Theme(theme) => theme }.forall(_ == kind.theme),
      "plan event theme must be derived from plan kind"
    )
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

case class PlanSequenceSummary(
  transitionType: TransitionType,
  primaryPlanId: Option[PlanKind] = None,
  previousPlanId: Option[PlanKind] = None,
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
