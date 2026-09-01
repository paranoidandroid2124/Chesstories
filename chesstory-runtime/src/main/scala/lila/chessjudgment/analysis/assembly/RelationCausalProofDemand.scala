package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.model.judgment.*

/** One graph-owned PlayedVsBest demand and its two admitted line occurrences.
  * Family certifiers share this lookup; none may independently rediscover the
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

/** Exact lower-contract predispatch. Changed L1 activations cheaply gate the
  * families that require them. Route and move-order demands instead enumerate
  * the complete admitted replays without a score, horizon, top-N, or radius
  * cutoff. Every demand retains its exact occurrence witnesses; family
  * authority still closes identity, absence, sibling, and later consumption.
  */
private[assembly] final case class RelationCausalProofDemand private (
    input: ExactPlayedVsBestCausalInput,
    uniqueCheckReplyDefenderDisplacementBeforeCaptureDemand: Option[UniqueCheckReplyDefenderDisplacementBeforeCaptureDemand],
    soleRecapturerRemovalBeforeTargetCaptureDemand: Option[SoleRecapturerRemovalBeforeTargetCaptureDemand],
    vacatedGateEnablesUnrecapturableSliderCaptureDemands: List[VacatedGateEnablesUnrecapturableSliderCaptureDemand],
    squareReleaseRouteDemands: List[SquareReleaseRouteDemand],
    captureExclusionMoveOrderDemands: List[CaptureExclusionMoveOrderDemand]
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

    val forcedDemand = for
      (checkOccurrence, forcedReply) <- referenceCheckMembership
      referenceExploitOccurrence <- referenceCaptureOccurrences.get(2).flatten
      (playedExploitOccurrence, playedRecapture) <- playedRecaptureMembership
    yield UniqueCheckReplyDefenderDisplacementBeforeCaptureDemand(
      checkOccurrence,
      forcedReply,
      referenceExploitOccurrence,
      playedExploitOccurrence,
      playedRecapture
    )

    val defenseDemand = for
      (removalOccurrence, removalRecapture) <- referenceRemovalMembership
      referenceExploitOccurrence <- referenceCaptureOccurrences.get(2).flatten
      (playedExploitOccurrence, playedRecapture) <- playedRecaptureMembership
    yield SoleRecapturerRemovalBeforeTargetCaptureDemand(
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
    val directDemands = for
      exploitIndex <- directExploitIndices
      exploitOccurrence <- referenceCaptureOccurrences.get(exploitIndex).flatten.toList
      rootReachOccurrence <- rootReachOccurrences
    yield VacatedGateEnablesUnrecapturableSliderCaptureDemand(
      rootReachOccurrence,
      exploitOccurrence,
      exploitIndex
    )
    val directDemandKeys = directDemands.map(_.stableKey)
    require(
      directDemandKeys.distinct.size == directDemandKeys.size,
      "one direct line-access occurrence demand may be dispatched only once"
    )

    val occupationDemands = referenceSteps.headOption.toList.flatMap(rootStep =>
      reference.legalMoveOccurrence(rootStep).toList.flatMap(rootOccurrence =>
        referenceSteps.indices.drop(2).filter(_ <= playedSteps.size).toList.flatMap(index =>
          referenceSteps.lift(index).toList.flatMap(step =>
            reference.legalMoveOccurrence(step).toList.collect {
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
    val terminalRouteDemands = occupationDemands.flatMap(demand =>
      firstTerminalRouteDemands(input, demand)
    )
    val squareReleaseRouteDemands = (occupationDemands ++ terminalRouteDemands).sortBy(_.stableKey)
    require(
      squareReleaseRouteDemands.map(_.stableKey).distinct.size == squareReleaseRouteDemands.size,
      "one exact square-release route occurrence may be dispatched only once"
    )

    val captureExclusionMoveOrderDemands =
      (for
        referenceRootStep <- referenceSteps.headOption.toList
        playedRootStep <- playedSteps.headOption.toList
        playedReplyStep <- playedSteps.lift(1).toList
        referenceRoot <- reference.legalMoveOccurrence(referenceRootStep).toList
        playedRoot <- played.legalMoveOccurrence(playedRootStep).toList
        playedReply <- played.legalMoveOccurrence(playedReplyStep).toList
        replyCapture <- playedReply.movement.capture.toList
        if replyCapture.capturedSquare == playedReply.movement.to
        if referenceRoot.movement.witness.from == replyCapture.capturedSquare &&
          referenceRoot.movement.witness.side == replyCapture.capturedSide &&
          referenceRoot.movement.witness.beforeRole == replyCapture.capturedRole &&
          referenceRoot.movement.witness.side == playedRoot.movement.witness.side &&
          playedReply.movement.witness.side != playedRoot.movement.witness.side
        deferredIndex <- referenceSteps.indices.drop(2).filter(_ % 2 == 0).toList
        deferredStep <- referenceSteps.lift(deferredIndex).toList
        referenceDeferred <- reference.legalMoveOccurrence(deferredStep).toList
        if CaptureExclusionMoveOrderDemand.sameMove(playedRoot, referenceDeferred)
      yield CaptureExclusionMoveOrderDemand(
        referenceRoot,
        playedRoot,
        playedReply,
        referenceDeferred,
        deferredIndex
      )).sortBy(_.stableKey)
    require(
      captureExclusionMoveOrderDemands.map(_.stableKey).distinct.size == captureExclusionMoveOrderDemands.size,
      "one exact capture-exclusion move-order occurrence may be dispatched only once"
    )

    RelationCausalProofDemand(
      input,
      forcedDemand,
      defenseDemand,
      directDemands.sortBy(_.stableKey),
      squareReleaseRouteDemands,
      captureExclusionMoveOrderDemands
    )

  /** Follows the replay-owned same object until the first later leg that owns
    * a closed capture or created-check result. There is no horizon or score
    * filter: the admitted LegalLine continuation is the complete boundary.
    */
  private def firstTerminalRouteDemands(
      input: ExactPlayedVsBestCausalInput,
      occupation: SquareReleaseRouteDemand
  ): List[SquareReleaseRouteDemand] =
    val replay = input.referenceReplay
    val steps = replay.replaySteps

    def loop(
        route: List[ReplayLegalMoveOccurrence],
        indices: List[Int]
    ): List[SquareReleaseRouteDemand] =
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
                SquareReleaseRouteDemand.terminal(
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
