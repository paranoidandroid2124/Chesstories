package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.model.judgment.*

/** Family-private occurrence demands. They share only the graph-certified root
  * inventory and requested subject. Each family orients branches from its own
  * exact lower occurrences; selection role, rank, verdict, and comparison
  * identity never participate.
  */
private[assembly] final case class UniqueCheckReplyProofDemand private (
    subject: CertifiedRootOccurrence,
    displacementBranch: CertifiedRootOccurrence,
    immediateCaptureBranch: CertifiedRootOccurrence,
    lower: UniqueCheckReplyDefenderDisplacementBeforeCaptureDemand
):
  require(subject == displacementBranch || subject == immediateCaptureBranch)

  def stableKey: String = List(
    subject.transitionOwner.ref.id,
    displacementBranch.lineOwner.ref.id,
    displacementBranch.transitionOwner.ref.id,
    immediateCaptureBranch.lineOwner.ref.id,
    immediateCaptureBranch.transitionOwner.ref.id,
    lower.stableKey
  ).mkString("|")

private[assembly] object UniqueCheckReplyProofDemand:
  def from(demand: OccurrenceExplanationDemand): List[UniqueCheckReplyProofDemand] =
    val exact = for
      (displacement, immediate) <- demand.subjectSiblingOrientations
      lower <- lowerDemand(displacement.replay, immediate.replay).toList
    yield UniqueCheckReplyProofDemand(demand.subject, displacement, immediate, lower)
    canonical(exact, _.stableKey, "unique-check-reply")

  private def lowerDemand(
      displacement: CanonicalLineReplay,
      immediate: CanonicalLineReplay
  ): Option[UniqueCheckReplyDefenderDisplacementBeforeCaptureDemand] =
    val displacementSteps = displacement.replaySteps
    val immediateSteps = immediate.replaySteps
    for
      delayedCapture <- displacementSteps.lift(2)
      immediateCapture <- immediateSteps.headOption
      if EvidenceRef.sameMove(delayedCapture.moveUci, immediateCapture.moveUci)
      _ <- immediateSteps.lift(1)
      if activates(displacement, 0, VerticalRelationContractKind.CreatedCheckResponseInventory)
      if activates(displacement, 2, VerticalRelationContractKind.CaptureRecaptureInventory)
      if activates(immediate, 0, VerticalRelationContractKind.CaptureRecaptureInventory)
      checkMembership <- for
        trigger <- displacementSteps.headOption
        reply <- displacementSteps.lift(1)
        exact <- displacement.exactCheckResponseOccurrenceMembership(trigger, reply)
      yield exact
      captureOccurrence <- exactVerticalOccurrence(
        displacement,
        delayedCapture,
        VerticalRelationContractKind.CaptureRecaptureInventory
      )
      immediateMembership <- for
        reply <- immediateSteps.lift(1)
        exact <- immediate.exactRecaptureOccurrenceMembership(immediateCapture, reply)
      yield exact
    yield UniqueCheckReplyDefenderDisplacementBeforeCaptureDemand(
      checkMembership._1,
      checkMembership._2,
      captureOccurrence,
      immediateMembership._1,
      immediateMembership._2
    )

private[assembly] final case class SoleRecapturerRemovalProofDemand private (
    subject: CertifiedRootOccurrence,
    removalBranch: CertifiedRootOccurrence,
    immediateCaptureBranch: CertifiedRootOccurrence,
    lower: SoleRecapturerRemovalBeforeTargetCaptureDemand
):
  require(subject == removalBranch || subject == immediateCaptureBranch)

  def stableKey: String = List(
    subject.transitionOwner.ref.id,
    removalBranch.lineOwner.ref.id,
    removalBranch.transitionOwner.ref.id,
    immediateCaptureBranch.lineOwner.ref.id,
    immediateCaptureBranch.transitionOwner.ref.id,
    lower.stableKey
  ).mkString("|")

private[assembly] object SoleRecapturerRemovalProofDemand:
  def from(demand: OccurrenceExplanationDemand): List[SoleRecapturerRemovalProofDemand] =
    val exact = for
      (removal, immediate) <- demand.subjectSiblingOrientations
      lower <- lowerDemand(removal.replay, immediate.replay).toList
    yield SoleRecapturerRemovalProofDemand(demand.subject, removal, immediate, lower)
    canonical(exact, _.stableKey, "sole-recapturer-removal")

  private def lowerDemand(
      removal: CanonicalLineReplay,
      immediate: CanonicalLineReplay
  ): Option[SoleRecapturerRemovalBeforeTargetCaptureDemand] =
    val removalSteps = removal.replaySteps
    val immediateSteps = immediate.replaySteps
    for
      delayedCapture <- removalSteps.lift(2)
      immediateCapture <- immediateSteps.headOption
      if EvidenceRef.sameMove(delayedCapture.moveUci, immediateCapture.moveUci)
      _ <- immediateSteps.lift(1)
      if activates(removal, 0, VerticalRelationContractKind.CaptureRecaptureInventory)
      if activates(removal, 2, VerticalRelationContractKind.CaptureRecaptureInventory)
      if activates(immediate, 0, VerticalRelationContractKind.CaptureRecaptureInventory)
      removalMembership <- for
        root <- removalSteps.headOption
        reply <- removalSteps.lift(1)
        exact <- removal.exactRecaptureOccurrenceMembership(root, reply)
      yield exact
      delayedOccurrence <- exactVerticalOccurrence(
        removal,
        delayedCapture,
        VerticalRelationContractKind.CaptureRecaptureInventory
      )
      immediateMembership <- for
        reply <- immediateSteps.lift(1)
        exact <- immediate.exactRecaptureOccurrenceMembership(immediateCapture, reply)
      yield exact
    yield SoleRecapturerRemovalBeforeTargetCaptureDemand(
      removalMembership._1,
      removalMembership._2,
      delayedOccurrence,
      immediateMembership._1,
      immediateMembership._2
    )

private[assembly] final case class VacatedGateCaptureProofDemand private (
    subject: CertifiedRootOccurrence,
    vacatedGateBranch: CertifiedRootOccurrence,
    retainedGateBranch: CertifiedRootOccurrence,
    lowers: List[VacatedGateEnablesUnrecapturableSliderCaptureDemand]
):
  require(subject == vacatedGateBranch || subject == retainedGateBranch)
  require(lowers.nonEmpty)

  def stableKey: String = List(
    subject.transitionOwner.ref.id,
    vacatedGateBranch.lineOwner.ref.id,
    vacatedGateBranch.transitionOwner.ref.id,
    retainedGateBranch.lineOwner.ref.id,
    retainedGateBranch.transitionOwner.ref.id,
    lowers.map(_.stableKey).mkString("[", ",", "]")
  ).mkString("|")

private[assembly] object VacatedGateCaptureProofDemand:
  def from(demand: OccurrenceExplanationDemand): List[VacatedGateCaptureProofDemand] =
    val exact = for
      (vacated, retained) <- demand.subjectSiblingOrientations
      lowers = lowerDemands(vacated.replay, retained.replay)
      if lowers.nonEmpty
    yield VacatedGateCaptureProofDemand(demand.subject, vacated, retained, lowers)
    canonical(exact, _.stableKey, "vacated-gate-capture")

  private def lowerDemands(
      vacated: CanonicalLineReplay,
      retained: CanonicalLineReplay
  ): List[VacatedGateEnablesUnrecapturableSliderCaptureDemand] =
    val vacatedSteps = vacated.replaySteps
    val retainedSteps = retained.replaySteps
    if !activates(vacated, 0, VerticalRelationContractKind.SliderReachDelta) then Nil
    else
      val reachOccurrences = vacatedSteps.headOption.toList.flatMap(step =>
        vacated.verticalRelationOccurrences(step, List(VerticalRelationContractKind.SliderReachDelta))
      )
      val exploitIndices = vacatedSteps.indices.drop(2).filter(index =>
        retainedSteps.size >= index &&
          activates(vacated, index, VerticalRelationContractKind.CaptureRecaptureInventory)
      ).toList
      val exact = for
        exploitIndex <- exploitIndices
        exploitStep <- vacatedSteps.lift(exploitIndex).toList
        exploit <- exactVerticalOccurrence(
          vacated,
          exploitStep,
          VerticalRelationContractKind.CaptureRecaptureInventory
        ).toList
        reach <- reachOccurrences
      yield VacatedGateEnablesUnrecapturableSliderCaptureDemand(reach, exploit, exploitIndex)
      canonical(exact, _.stableKey, "vacated-gate lower")

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

private def activates(
    replay: CanonicalLineReplay,
    stepIndex: Int,
    contract: VerticalRelationContractKind
): Boolean =
  replay.replaySteps.lift(stepIndex).exists(replay.activatesVerticalRelation(_, contract))

private def exactVerticalOccurrence(
    replay: CanonicalLineReplay,
    step: LineReplayStep,
    contract: VerticalRelationContractKind
): Option[ReplayVerticalRelationOccurrence] =
  replay.verticalRelationOccurrences(step, List(contract)) match
    case exact :: Nil => Some(exact)
    case _            => None

private def canonical[A](values: List[A], key: A => String, label: String): List[A] =
  val sorted = values.sortBy(key)
  require(
    sorted.map(key).distinct.size == sorted.size,
    s"one exact $label occurrence demand may be dispatched only once"
  )
  sorted
