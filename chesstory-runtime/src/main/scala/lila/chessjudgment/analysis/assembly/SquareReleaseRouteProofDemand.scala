package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.model.judgment.*

private[assembly] final case class SquareReleaseRouteProofDemand private (
    subject: CertifiedRootOccurrence,
    releasedSquareBranch: CertifiedRootOccurrence,
    retainedSquareBranch: CertifiedRootOccurrence,
    lowers: List[SquareReleaseRouteDemand]
):
  require(subject == releasedSquareBranch || subject == retainedSquareBranch)
  require(lowers.nonEmpty)

  def stableKey: String = List(
    subject.transitionOwner.ref.id,
    releasedSquareBranch.lineOwner.ref.id,
    releasedSquareBranch.transitionOwner.ref.id,
    retainedSquareBranch.lineOwner.ref.id,
    retainedSquareBranch.transitionOwner.ref.id,
    lowers.map(_.stableKey).mkString("[", ",", "]")
  ).mkString("|")

private[assembly] object SquareReleaseRouteProofDemand:
  def from(demand: OccurrenceExplanationDemand): List[SquareReleaseRouteProofDemand] =
    val exact = for
      (released, retained) <- demand.subjectSiblingOrientations
      lowers = lowerDemands(released, retained)
      if lowers.nonEmpty
    yield SquareReleaseRouteProofDemand(demand.subject, released, retained, lowers)
    canonical(exact, _.stableKey, "square-release-route")

  private def lowerDemands(
      released: CertifiedRootOccurrence,
      retained: CertifiedRootOccurrence
  ): List[SquareReleaseRouteDemand] =
    val releasedSteps = released.replay.replaySteps
    val retainedSteps = retained.replay.replaySteps
    val occupations = releasedSteps.headOption.toList.flatMap(rootStep =>
      released.replay.legalMoveOccurrence(rootStep).toList.flatMap(rootOccurrence =>
        releasedSteps.indices.drop(2).filter(_ <= retainedSteps.size).toList.flatMap(index =>
          releasedSteps.lift(index).toList.flatMap(step =>
            released.replay.legalMoveOccurrence(step).toList.collect {
              case firstRouteLeg
                  if firstRouteLeg.movement.capture.isEmpty &&
                    firstRouteLeg.movement.side == rootOccurrence.movement.side &&
                    firstRouteLeg.movement.to == rootOccurrence.movement.from =>
                SquareReleaseRouteDemand.occupation(rootOccurrence, firstRouteLeg, index)
            }
          )
        )
      )
    )
    val terminals = occupations.flatMap(firstTerminalRouteDemands(released, _))
    canonical(occupations ++ terminals, _.stableKey, "square-release-route lower")

  private def firstTerminalRouteDemands(
      released: CertifiedRootOccurrence,
      occupation: SquareReleaseRouteDemand
  ): List[SquareReleaseRouteDemand] =
    val replay = released.replay
    val steps = replay.replaySteps

    def loop(
        route: List[ReplayLegalMoveOccurrence],
        indices: List[Int],
        remaining: List[LineReplayStep]
    ): List[SquareReleaseRouteDemand] =
      RecordBoundObjectTrajectory
        .firstAfter(released.lineOwner, route.last.step, route.last.movement.witness, remaining)
        .toList.flatMap { trajectory =>
          val next = trajectory.futureMovement.occurrence
          val nextIndex = indices.last + trajectory.plyOffset
          if nextIndex <= indices.last then Nil
          else
            val extendedRoute = route :+ next
            val extendedIndices = indices :+ nextIndex
            val terminals = replay.verticalRelationOccurrences(
              next.step,
              List(
                VerticalRelationContractKind.CaptureRecaptureInventory,
                VerticalRelationContractKind.CreatedCheckResponseInventory
              )
            ).flatMap(terminal =>
              exactTerminalReply(replay, nextIndex, terminal).map(reply =>
                SquareReleaseRouteDemand.terminal(
                  occupation.releaseOccurrence,
                  extendedRoute,
                  extendedIndices,
                  terminal,
                  reply
                )
              )
            )
            if terminals.nonEmpty then terminals
            else loop(extendedRoute, extendedIndices, remaining.drop(trajectory.plyOffset))
        }

    loop(
      occupation.routeOccurrences,
      occupation.routeStepIndices,
      steps.drop(occupation.routeStepIndices.last + 1)
    )

  private def exactTerminalReply(
      replay: CanonicalLineReplay,
      terminalIndex: Int,
      terminal: ReplayVerticalRelationOccurrence
  ): Option[Option[ReplayLegalMoveOccurrence]] =
    terminal.relation.detail match
      case _: RelationWitnessDetail.CaptureRecaptureInventory =>
        replay.replaySteps.lift(terminalIndex + 1).flatMap(replay.legalMoveOccurrence).map(Some(_))
      case RelationWitnessDetail.CreatedCheckResponseInventory(
            _, _, _, _, _, _, RelationCheckTerminalState.Checkmate, _
          ) =>
        Some(None)
      case _: RelationWitnessDetail.CreatedCheckResponseInventory =>
        for
          replyStep <- replay.replaySteps.lift(terminalIndex + 1)
          reply <- replay.legalMoveOccurrence(replyStep)
          membership <- replay.exactCheckResponseOccurrenceMembership(terminal.step, replyStep)
          if membership._1 == terminal
        yield Some(reply)
      case _ => None

  private def canonical[A](values: List[A], key: A => String, label: String): List[A] =
    val sorted = values.sortBy(key)
    require(
      sorted.map(key).distinct.size == sorted.size,
      s"one exact $label occurrence demand may be dispatched only once"
    )
    sorted
