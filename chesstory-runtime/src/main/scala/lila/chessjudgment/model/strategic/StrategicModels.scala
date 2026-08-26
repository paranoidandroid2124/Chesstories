package lila.chessjudgment.model.strategic

import lila.chessjudgment.model.{ PlanEventIdentity, TransitionType }

case class PlanContinuity(
  startingEvent: Option[PlanEventIdentity],
  consecutivePlies: Int,
  startingPly: Int,
  supportingMoves: List[String] = Nil,
  supportingEvents: List[PlanEventIdentity] = Nil,
  completionProven: Boolean = false
):
  def episodeTransitionType: TransitionType =
    if supportingEvents.isEmpty then TransitionType.Opening
    else if completionProven then TransitionType.Completion
    else TransitionType.Continuation

object PlanContinuity:
  import play.api.libs.json.*
  import lila.chessjudgment.model.PlanEventIdentity.given
  given Reads[PlanContinuity] = Reads { js =>
    for
      startingEvent <- (js \ "startingEvent").validateOpt[PlanEventIdentity]
      consecutivePlies <- (js \ "consecutivePlies").validate[Int]
      startingPly <- (js \ "startingPly").validate[Int]
      supportingMoves <- (js \ "supportingMoves").validateOpt[List[String]]
      supportingEvents <- (js \ "supportingEvents").validateOpt[List[PlanEventIdentity]]
      completionProven <- (js \ "completionProven").validateOpt[Boolean]
    yield PlanContinuity(
      startingEvent = startingEvent,
      consecutivePlies = consecutivePlies,
      startingPly = startingPly,
      supportingMoves = supportingMoves.getOrElse(Nil),
      supportingEvents = supportingEvents.getOrElse(Nil),
      completionProven = completionProven.getOrElse(false)
    )
  }
  given Writes[PlanContinuity] = Writes { c =>
    Json.obj(
      "startingEvent" -> c.startingEvent,
      "consecutivePlies" -> c.consecutivePlies,
      "startingPly" -> c.startingPly,
      "supportingMoves" -> c.supportingMoves,
      "supportingEvents" -> c.supportingEvents,
      "completionProven" -> c.completionProven
    )
  }

  def fromEvents(
      events: List[(PlanEventIdentity, Int)],
      completionProven: Boolean
  ): Option[PlanContinuity] =
    events.sortBy(_._2) match
      case Nil | _ :: Nil => None
      case ordered =>
        val supporting = ordered.drop(1)
        Some(
          PlanContinuity(
            startingEvent = Some(ordered.head._1),
            consecutivePlies = (ordered.last._2 - ordered.head._2 + 1).max(1),
            startingPly = ordered.head._2,
            supportingMoves = supporting.map(_._1.rootMove),
            supportingEvents = supporting.map(_._1),
            completionProven = completionProven
          )
        )

  def fromAntecedents(
      events: List[(PlanEventIdentity, Int)],
      currentPly: Int,
      completionProven: Boolean
  ): Option[PlanContinuity] =
    events.sortBy(_._2) match
      case Nil => None
      case ordered if ordered.last._2 >= currentPly => None
      case ordered =>
        Some(
          PlanContinuity(
            startingEvent = Some(ordered.head._1),
            consecutivePlies = (currentPly - ordered.head._2 + 1).max(1),
            startingPly = ordered.head._2,
            supportingMoves = ordered.map(_._1.rootMove),
            supportingEvents = ordered.map(_._1),
            completionProven = completionProven
          )
        )
