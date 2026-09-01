package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.model.judgment.*

/** One graph-owned PlayedVsBest demand and its two admitted line occurrences.
  * Family producers share this lookup; none may independently rediscover the
  * comparison, line owners, or canonical replays.
  */
private[assembly] final case class ExactPlayedVsBestCausalInput private (
    comparison: CandidateComparisonFact,
    demandSource: EvidenceRecord,
    referenceSource: EvidenceRecord,
    playedSource: EvidenceRecord,
    referenceReplay: CanonicalLineReplay,
    playedReplay: CanonicalLineReplay
)

private[assembly] object ExactPlayedVsBestCausalInput:

  def from(
      context: JudgmentAssemblyContext,
      demandSource: EvidenceRecord
  ): Option[ExactPlayedVsBestCausalInput] =
    for
      registeredDemand <- context.evidenceGraph.record(demandSource.ref)
      if registeredDemand == demandSource && context.evidenceGraph.proofEligible(registeredDemand)
      comparison <- registeredDemand.payload match
        case CandidateComparisonEvidence(exact) => Some(exact)
        case _                                  => None
      if ActionablePlayedVsBestCausalProofDemand.accepts(comparison)
      referenceLine <- context.line(LineNodeRole.BestReference)
      playedLine <- context.line(LineNodeRole.Played)
      if referenceLine.ref == comparison.referenceLine
      if playedLine.ref == comparison.candidateLine
      if !EvidenceRef.sameMove(referenceLine.ref.rootMove, playedLine.ref.rootMove)
      referenceOwner <- context.evidenceGraph
        .uniqueProofEligibleLineFactRecordFor(referenceLine.ref)
        .map(_._1)
      playedOwner <- context.evidenceGraph
        .uniqueProofEligibleLineFactRecordFor(playedLine.ref)
        .map(_._1)
      referenceReplay <- certifiedReplay(referenceOwner)
      playedReplay <- certifiedReplay(playedOwner)
      if CertifiedComparedLineAuthority.exactRecord(
        referenceOwner,
        comparison.referenceLine,
        referenceReplay
      )
      if CertifiedComparedLineAuthority.exactRecord(
        playedOwner,
        comparison.candidateLine,
        playedReplay
      )
      if ActionablePlayedVsBestCausalProofDemand.acceptsRecord(
        registeredDemand,
        comparison,
        referenceOwner.ref.position,
        referenceOwner,
        playedOwner
      )
    yield ExactPlayedVsBestCausalInput(
      comparison,
      registeredDemand,
      referenceOwner,
      playedOwner,
      referenceReplay,
      playedReplay
    )

  private def certifiedReplay(source: EvidenceRecord): Option[CanonicalLineReplay] =
    source.payload match
      case exact: LineFactEvidence => exact.certifiedReplay
      case _                       => None

/** Exact lower-contract predispatch. Cheap changed keys decide which closed L1
  * occurrences are materialized; those occurrences and their exact membership
  * witnesses are then retained as family seeds. The family authority still
  * closes identity, absence, sibling, and later-consumption obligations, but it
  * never rediscovers a selected transition from a boolean or integer address.
  */
private[assembly] final case class RelationCausalProofDemand private (
    input: ExactPlayedVsBestCausalInput,
    uniqueCheckReplyDefenderDisplacementBeforeCaptureSeed: Option[UniqueCheckReplyDefenderDisplacementBeforeCaptureChangedSeed],
    soleRecapturerRemovalBeforeTargetCaptureSeed: Option[SoleRecapturerRemovalBeforeTargetCaptureChangedSeed],
    vacatedGateEnablesUnrecapturableSliderCaptureSeeds: List[VacatedGateEnablesUnrecapturableSliderCaptureChangedSeed],
    squareReleaseRouteSeeds: List[SquareReleaseRouteChangedSeed]
)

private[assembly] object RelationCausalProofDemand:

  def from(input: ExactPlayedVsBestCausalInput): RelationCausalProofDemand =
    val reference = input.referenceReplay
    val played = input.playedReplay
    val referenceSteps = reference.replaySteps
    val playedSteps = played.replaySteps
    val commonImmediateExploitShape =
      referenceSteps.lift(2).exists(referenceExploit =>
        playedSteps.headOption.exists(playedExploit =>
          EvidenceRef.sameMove(referenceExploit.moveUci, playedExploit.moveUci)
        )
      ) && playedSteps.lift(1).nonEmpty
    lazy val referenceRootCheckChanged =
      activates(reference, 0, VerticalRelationContractKind.CreatedCheckResponseInventory)
    lazy val referenceRootCaptureChanged =
      activates(reference, 0, VerticalRelationContractKind.CaptureRecaptureInventory)
    lazy val referenceImmediateExploitCaptureChanged =
      activates(reference, 2, VerticalRelationContractKind.CaptureRecaptureInventory)
    lazy val playedImmediateExploitCaptureChanged =
      activates(played, 0, VerticalRelationContractKind.CaptureRecaptureInventory)

    val forcedChanged =
      commonImmediateExploitShape &&
        referenceRootCheckChanged &&
        referenceImmediateExploitCaptureChanged &&
        playedImmediateExploitCaptureChanged

    val defenseChanged =
      commonImmediateExploitShape &&
        referenceRootCaptureChanged &&
        referenceImmediateExploitCaptureChanged &&
        playedImmediateExploitCaptureChanged

    val directExploitIndices =
      Option
        .when(activates(reference, 0, VerticalRelationContractKind.SliderReachDelta))(
          referenceSteps.indices.drop(2).filter(index =>
            playedSteps.size >= index &&
              activates(reference, index, VerticalRelationContractKind.CaptureRecaptureInventory)
          ).toList
        )
        .getOrElse(Nil)

    val referenceCaptureIndices =
      (directExploitIndices ++ Option.when(
        (forcedChanged || defenseChanged) && !directExploitIndices.contains(2)
      )(2)).sorted
    val referenceCaptureOccurrences = referenceCaptureIndices.map(index =>
      index -> referenceSteps.lift(index).flatMap(step =>
        exactVerticalOccurrence(
          reference,
          step,
          VerticalRelationContractKind.CaptureRecaptureInventory
        )
      )
    ).toMap
    lazy val playedRecaptureMembership =
      Option
        .when(forcedChanged || defenseChanged)(
          for
            exploit <- playedSteps.headOption
            reply <- playedSteps.lift(1)
            exact <- played.exactRecaptureOccurrenceMembership(exploit, reply)
          yield exact
        )
        .flatten
    lazy val referenceCheckMembership =
      Option
        .when(forcedChanged)(
          for
            trigger <- referenceSteps.headOption
            reply <- referenceSteps.lift(1)
            exact <- reference.exactCheckResponseOccurrenceMembership(trigger, reply)
          yield exact
        )
        .flatten
    lazy val referenceRemovalMembership =
      Option
        .when(defenseChanged)(
          for
            removal <- referenceSteps.headOption
            reply <- referenceSteps.lift(1)
            exact <- reference.exactRecaptureOccurrenceMembership(removal, reply)
          yield exact
        )
        .flatten

    val forcedSeed = for
      (checkOccurrence, forcedReply) <- referenceCheckMembership
      referenceExploitOccurrence <- referenceCaptureOccurrences.get(2).flatten
      (playedExploitOccurrence, playedRecapture) <- playedRecaptureMembership
    yield UniqueCheckReplyDefenderDisplacementBeforeCaptureChangedSeed(
      checkOccurrence,
      forcedReply,
      referenceExploitOccurrence,
      playedExploitOccurrence,
      playedRecapture
    )

    val defenseSeed = for
      (removalOccurrence, removalRecapture) <- referenceRemovalMembership
      referenceExploitOccurrence <- referenceCaptureOccurrences.get(2).flatten
      (playedExploitOccurrence, playedRecapture) <- playedRecaptureMembership
    yield SoleRecapturerRemovalBeforeTargetCaptureChangedSeed(
      removalOccurrence,
      removalRecapture,
      referenceExploitOccurrence,
      playedExploitOccurrence,
      playedRecapture
    )

    val rootReachOccurrences =
      Option
        .when(directExploitIndices.nonEmpty)(
          referenceSteps.headOption.toList.flatMap(step =>
            reference.verticalRelationOccurrences(
              step,
              List(VerticalRelationContractKind.SliderReachDelta)
            )
          )
        )
        .getOrElse(Nil)
    val directSeeds = for
      exploitIndex <- directExploitIndices
      exploitOccurrence <- referenceCaptureOccurrences.get(exploitIndex).flatten.toList
      rootReachOccurrence <- rootReachOccurrences
    yield VacatedGateEnablesUnrecapturableSliderCaptureChangedSeed(
      rootReachOccurrence,
      exploitOccurrence,
      exploitIndex
    )
    val directSeedKeys = directSeeds.map(_.stableKey)
    require(
      directSeedKeys.distinct.size == directSeedKeys.size,
      "one direct line-access changed occurrence route may be dispatched only once"
    )

    val occupationSeeds = referenceSteps.headOption.toList.flatMap(rootStep =>
      reference.legalMoveOccurrence(rootStep).toList.flatMap(rootOccurrence =>
        referenceSteps.indices.drop(2).filter(_ <= playedSteps.size).toList.flatMap(index =>
          referenceSteps.lift(index).toList.flatMap(step =>
            reference.legalMoveOccurrence(step).toList.collect {
              case firstRouteLeg
                  if firstRouteLeg.movement.capture.isEmpty &&
                    firstRouteLeg.movement.side == rootOccurrence.movement.side &&
                    firstRouteLeg.movement.to == rootOccurrence.movement.from =>
                SquareReleaseRouteChangedSeed.occupation(rootOccurrence, firstRouteLeg, index)
            }
          )
        )
      )
    )
    val terminalRouteSeeds = occupationSeeds.flatMap(seed =>
      firstTerminalRouteSeeds(input, seed)
    )
    val squareReleaseRouteSeeds = (occupationSeeds ++ terminalRouteSeeds).sortBy(_.stableKey)
    require(
      squareReleaseRouteSeeds.map(_.stableKey).distinct.size == squareReleaseRouteSeeds.size,
      "one exact square-release route occurrence may be dispatched only once"
    )

    RelationCausalProofDemand(
      input,
      forcedSeed,
      defenseSeed,
      directSeeds.sortBy(_.stableKey),
      squareReleaseRouteSeeds
    )

  /** Follows the replay-owned same object until the first later leg that owns
    * a closed capture or created-check result. There is no horizon or score
    * filter: the admitted LegalLine continuation is the complete boundary.
    */
  private def firstTerminalRouteSeeds(
      input: ExactPlayedVsBestCausalInput,
      occupation: SquareReleaseRouteChangedSeed
  ): List[SquareReleaseRouteChangedSeed] =
    val replay = input.referenceReplay
    val steps = replay.replaySteps

    def loop(
        route: List[ReplayLegalMoveOccurrence],
        indices: List[Int]
    ): List[SquareReleaseRouteChangedSeed] =
      RecordBoundObjectTrajectory.firstAfter(input.referenceSource, route.last.step).toList.flatMap {
        trajectory =>
          val next = trajectory.futureMovement.occurrence
          val nextIndex = steps.indexOf(next.step)
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
                SquareReleaseRouteChangedSeed.terminal(
                  occupation.releaseOccurrence,
                  extendedRoute,
                  extendedIndices,
                  terminal,
                  reply
                )
              )
            )
            if terminals.nonEmpty then terminals else loop(extendedRoute, extendedIndices)
      }

    loop(occupation.routeOccurrences, occupation.routeStepIndices)

  private def exactTerminalReply(
      replay: CanonicalLineReplay,
      terminalIndex: Int,
      terminal: ReplayVerticalRelationOccurrence
  ): Option[Option[ReplayLegalMoveOccurrence]] =
    terminal.relation.detail match
      case _: RelationWitnessDetail.CaptureRecaptureInventory =>
        replay.replaySteps.lift(terminalIndex + 1)
          .flatMap(replay.legalMoveOccurrence)
          .map(Some(_))
      case RelationWitnessDetail.CreatedCheckResponseInventory(
            _,
            _,
            _,
            _,
            _,
            _,
            RelationCheckTerminalState.Checkmate,
            _
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
