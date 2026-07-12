package lila.chessjudgment.analysis.assembly

import chess.Color
import lila.chessjudgment.model.{
  Plan,
  PlanEventIdentity,
  PlanId,
  PlanMatch,
  PlanSupport,
  ProbeResult,
  TransitionType
}
import lila.chessjudgment.model.strategic.VariationLine
import lila.chessjudgment.model.strategic.PlanTaxonomy.{ PlanKind, PlanTheme }
import lila.chessjudgment.model.judgment.*

class ChessIdeaAssemblerTest extends munit.FunSuite:

  test("assembled plan pressure and pawn structure evidence stays acyclic"):
    val graph = EvidenceFactAssembler
      .assemble(
        RawMoveReviewInput(
          fen = chess.variant.Standard.initialFen.value,
          playedMoveUci = "d2d4",
          variations = List(
            VariationLine(List("e2e4", "e7e5", "g1f3"), scoreCp = 30, depth = 16),
            VariationLine(List("d2d4", "d7d5", "g1f3"), scoreCp = 20, depth = 16)
          ),
          currentEvalCp = Some(20),
          ply = Some(1)
        )
      )
      .getOrElse(fail("expected assembled evidence graph"))
      .context
      .evidenceGraph

    assert(graph.records.exists(_.payload.isInstanceOf[PlanPressureEvidence]))
    assert(graph.records.exists(_.payload.isInstanceOf[PawnStructureFactEvidence]))
    val playedPressure = graph.records.collectFirst {
      case EvidenceRecord(ref, payload: PlanPressureEvidence, _)
          if ref.line.exists(_.role == LineNodeRole.Played) => payload
    }.getOrElse(fail("expected played plan pressure"))
    assert(!playedPressure.activePlanIds(Some("d2d4")).contains(PlanId.OpeningDevelopment))
    assert(!graph.records.exists {
      case EvidenceRecord(_, mechanism: StrategicMechanismEvidence, _) =>
        mechanism.signals.exists(signal =>
          signal.kind == StrategicMechanismSignalKind.PlanPressure &&
            signal.source.layer == EvidenceLayer.PlanPressure
        )
      case _ => false
    })
    assertEquals(
      graph.records.filter(record => graph.parentClosure(record).exists(_.ref.id == record.ref.id)).map(_.ref.id),
      Nil
    )

  test("castling activity keeps its root line without borrowing plan pressure"):
    val graph = EvidenceFactAssembler
      .assemble(
        RawMoveReviewInput(
          fen = "rnb1k2r/pp2ppbp/5np1/q1Pp4/2P2B2/2N1P3/PP3PPP/2RQKBNR b Kkq - 2 7",
          playedMoveUci = "e8h8",
          variations = List(
            VariationLine(
              List("e8g8", "c4d5", "b8d7", "f1c4", "d7c5", "a2a3"),
              scoreCp = 0,
              depth = 12
            )
          ),
          currentEvalCp = Some(0),
          ply = Some(13)
        )
      )
      .getOrElse(fail("expected castling evidence graph"))
      .context
      .evidenceGraph
    val activity = graph.records.collectFirst {
      case record @ EvidenceRecord(ref, mechanism: StrategicMechanismEvidence, _)
          if ref.line.exists(_.role == LineNodeRole.Played) &&
            mechanism.kind == StrategicMechanismKind.Activity &&
            mechanism.signals.exists(_.label == "current-move-route") =>
        record -> mechanism
    }.getOrElse(fail("expected line-owned castling activity"))

    assert(activity._2.canAnchorStrategicIdea)
    assert(!activity._1.parents.exists(_.layer == EvidenceLayer.PlanPressure))
    assert(activity._1.parents.exists(parent =>
      graph.byId.get(parent.id).exists {
        case EvidenceRecord(ref, motif: MoveMotifEvidence, _) =>
          ref.line.exists(_.role == LineNodeRole.Played) && motif.motif.isInstanceOf[lila.chessjudgment.model.Motif.Castling]
        case _ => false
      }
    ))

  test("distinct same-root plans can own only their typed event results"):
    val result = MoveReviewJudgmentOrchestrator
      .build(
        RawMoveReviewInput(
          fen = "rnbqkb1r/pp1ppppp/5n2/2pP4/2P5/8/PP2PPPP/RNBQKBNR b KQkq - 0 3",
          playedMoveUci = "b7b5",
          variations = List(
            VariationLine(
              List("b7b5", "c4b5", "a7a6", "b5a6", "g7g6", "b1c3", "f8g7", "e2e4", "e8g8", "g1f3", "d7d6", "f1e2"),
              scoreCp = 0,
              depth = 12
            )
          ),
          currentEvalCp = Some(0),
          ply = Some(5),
          openingContext = Some(RawOpeningContext(name = Some("Benko Gambit / queenside file pressure"))),
          movePrefixUci = List("d2d4", "g8f6", "c2c4", "c7c5", "d4d5")
        )
      )
      .getOrElse(fail("expected judgment result"))
    assertEquals(result.packet.probeRequests, Nil)
    val view = result.packet.moveJudgmentView.getOrElse(fail("expected move judgment view"))
    val playedPressure = result.packet.evidenceGraph.records.collectFirst {
      case EvidenceRecord(ref, payload: PlanPressureEvidence, _)
          if ref.line.exists(line => line.role == LineNodeRole.Played && EvidenceRef.sameMove(line.rootMove, "b7b5")) =>
        payload
    }.getOrElse(fail("expected played plan pressure"))
    assert(playedPressure.rootBackedPlans(Some("b7b5")).size > 1)
    val (eventRecord, event) = result.packet.evidenceGraph.records.collectFirst {
      case record @ EvidenceRecord(_, payload: PlanCausalEventEvidence, _)
          if payload.rootLine.role == LineNodeRole.Played && EvidenceRef.sameMove(payload.rootMove, "b7b5") =>
        record -> payload
    }.getOrElse(fail("expected internal causal event"))
    assertEquals(event.planId, PlanId.QueensideAttack)
    assertEquals(eventRecord.ref.confidence, EvidenceConfidence.Mixed)
    assert(eventRecord.parents.exists(parent =>
      result.packet.evidenceGraph.byId.get(parent.id).exists {
        case EvidenceRecord(_, motif: MoveMotifEvidence, _) =>
          motif.isRootEvent && EvidenceRef.sameMove(motif.rootMove, event.rootMove)
        case _ =>
          false
      }
    ))
    val structural = eventRecord.parents.flatMap(parent => result.packet.evidenceGraph.byId.get(parent.id)).collectFirst {
      case EvidenceRecord(_, payload: StructuralDeltaEvidence, _) => payload
    }.getOrElse(fail("expected structural parent"))
    val positiveStructural = structural.consequences.filter(consequence => consequence.positive && consequence.strength > 0)
    assert(event.structuralConsequences.forall(_.subjects.nonEmpty), event.structuralConsequences)
    assert(event.structuralConsequences.size < positiveStructural.size, event.structuralConsequences -> positiveStructural)
    assert(result.packet.evidenceGraph.records.exists {
      case EvidenceRecord(_, mechanism: StrategicMechanismEvidence, _) =>
        mechanism.signals.exists(_.source.id == eventRecord.ref.id)
      case _ => false
    })
    assert(result.packet.evidenceGraph.records.exists {
      case EvidenceRecord(_, _: PlanTransitionEvidence, parents) =>
        parents.map(_.id) == List(eventRecord.ref.id)
      case _ => false
    })
    assert(view.moveMeaningClaims.exists(claim =>
      claim.moveUci == "b7b5" &&
        claim.positiveFunctionalProofEvidenceIds.exists(id =>
          result.packet.evidenceGraph.byId.get(id).exists {
            case EvidenceRecord(ref, _: PlanCausalEventEvidence, _) =>
              ref.confidence != EvidenceConfidence.Heuristic
            case _ => false
          }
        )
    ), view.moveMeaningClaims)

  test("principal plan event exports only its owned public carriers"):
    val result = MoveReviewJudgmentOrchestrator
      .build(
        RawMoveReviewInput(
          fen = "rnbqkb1r/p2ppppp/5n2/2pP4/2p1P3/8/PP1N1PPP/R1BQKBNR b KQkq - 0 5",
          playedMoveUci = "e7e6",
          variations = List(
            VariationLine(
              List("e7e6", "f1c4", "e6d5", "e4d5", "d7d6", "g1f3", "g7g6", "e1g1", "f8g7", "f1e1"),
              scoreCp = 0,
              depth = 12
            )
          ),
          currentEvalCp = Some(0),
          ply = Some(9),
          openingContext = Some(RawOpeningContext(name = Some("Benko Gambit / ...bxc4 and ...e6 counterplay"))),
          movePrefixUci = List("d2d4", "g8f6", "c2c4", "c7c5", "d4d5", "b7b5", "b1d2", "b5c4", "e2e4")
        )
      )
      .getOrElse(fail("expected judgment result"))
    val claims =
      result.packet.moveJudgmentView.toList.flatMap(_.moveMeaningClaims).filter(_.principalPlanEvent.nonEmpty)
    val publicSurfaces = result.packet.moveJudgmentView.toList
      .flatMap(MoveMeaningSurface.publicSurfaces)
      .filter(_.principalPlanEvent.nonEmpty)
    val playedEventRecord = result.packet.evidenceGraph.records
      .collectFirst {
        case record @ EvidenceRecord(ref, event: PlanCausalEventEvidence, _)
            if ref.line.exists(
              _.role == LineNodeRole.Played
            ) && event.planId == PlanId.PawnBreakPreparation =>
          record -> event
      }
      .getOrElse(fail("expected played causal event"))
    val transitionRecord = result.packet.evidenceGraph.records
      .collectFirst {
        case record @ EvidenceRecord(_, PlanTransitionEvidence(summary), _)
            if summary == playedEventRecord._2.planSequenceSummary =>
          record -> summary
      }
      .getOrElse(fail("expected event-owned plan transition"))
    val planMechanism = result.packet.evidenceGraph.records.collectFirst {
      case EvidenceRecord(_, mechanism: StrategicMechanismEvidence, _)
          if mechanism.hasOwnedPlanEvent &&
            mechanism.signals.exists(_.source.id == playedEventRecord._1.ref.id) =>
        mechanism
    }.getOrElse(fail("expected event-owned plan mechanism"))

    assert(claims.nonEmpty)
    assert(publicSurfaces.nonEmpty)
    assert(!playedEventRecord._1.parents.exists(_.layer == EvidenceLayer.PlanPressure))
    assertEquals(transitionRecord._2, playedEventRecord._2.planSequenceSummary)
    assertEquals(transitionRecord._1.parents.map(_.id), List(playedEventRecord._1.ref.id))
    assert(planMechanism.canAnchorPlanIdea)
    assert(!planMechanism.signals.exists(_.source.layer == EvidenceLayer.PlanPressure))
    assert(
      planMechanism.semanticAnchors.exists(
        _.stableKey == s"PlanPressure:${PlanId.PawnBreakPreparation}"
      )
    )
    assert(publicSurfaces.forall(_.evidence.positiveFunctionalProofIds.nonEmpty), publicSurfaces)
    assert(
      result.packet.moveJudgmentView.exists(view =>
        MoveMeaningSurface.publicPayloadJson(view).toString.contains("positive_functional_proof_ids")
      )
    )
    claims.foreach { claim =>
      val event = claim.principalPlanEvent.getOrElse(fail("expected principal plan event"))
      assertEquals(claim.principalPlanId, Some(PlanId.PawnBreakPreparation))
      assertEquals(event.rootMove, "e7e6")
      assertEquals(event.results.map(_.kind), List(TransitionConsequenceKind.PawnTensionGain))
      assert(claim.boardCarriers.exists(carrier => carrier.kind == "Move" && carrier.value == "e7e6"))
      assertEquals(claim.boardCarriers.filter(_.kind == "File").map(_.value).distinct, List("e"))
      assertEquals(claim.boardCarriers.filter(_.kind == "Piece").map(_.value).distinct, List("pawn"))
      assert(!claim.boardCarriers.exists(_.semanticRole.contains("counter_break_file")), claim.boardCarriers)
      assert(!claim.boardCarriers.exists(carrier => carrier.kind == "Square" && Set("c4", "d8", "f8")(carrier.value)), claim.boardCarriers)
    }

  test("specific weak-pawn event outranks generic activity without borrowing plan authority"):
    val result = MoveReviewJudgmentOrchestrator
      .build(weakPawnInput())
      .getOrElse(fail("expected judgment result"))
    val events = result.packet.evidenceGraph.records.collect {
      case record @ EvidenceRecord(ref, event: PlanCausalEventEvidence, _)
          if ref.line.exists(_.role == LineNodeRole.Played) && EvidenceRef.sameMove(event.rootMove, "c6a5") =>
        record -> event
    }
    val weakPawnEvent =
      events.find(_._2.planId == PlanId.WeakPawnAttack).getOrElse(fail("expected c4 plan event"))
    val activityEvent =
      events.find(_._2.planId == PlanId.PieceActivation).getOrElse(fail("expected activity alternative"))
    val publicClaims =
      result.packet.moveJudgmentView.toList.flatMap(_.moveMeaningClaims).filter(_.publicSurfaceAdmitted)

    assertEquals(weakPawnEvent._1.ref.confidence, EvidenceConfidence.Mixed)
    assertEquals(activityEvent._1.ref.confidence, EvidenceConfidence.Heuristic)
    assertEquals(
      weakPawnEvent._2.structuralConsequences.map(_.kind),
      List(TransitionConsequenceKind.TargetPressureGain)
    )
    assertEquals(weakPawnEvent._2.structuralConsequences.flatMap(_.subjects), List("c4"))
    assert(
      publicClaims.exists(claim =>
        claim.principalPlanId.contains(PlanId.WeakPawnAttack) &&
          claim.positiveFunctionalProofEvidenceIds.contains(weakPawnEvent._1.ref.id) &&
          claim.targetSquares.contains("c4")
      ),
      publicClaims
    )
    assert(
      !publicClaims.exists(claim =>
        claim.meaningKind == "TargetPressure" &&
          claim.positiveFunctionalProofEvidenceIds.isEmpty &&
          claim.sourceEvidenceIds.exists(_.contains("structural-delta:played:c6a5"))
      ),
      publicClaims
    )

  test("line trajectory preserves the moved knight across future plies"):
    val graph = EvidenceFactAssembler
      .assemble(
        RawMoveReviewInput(
          fen = chess.variant.Standard.initialFen.value,
          playedMoveUci = "g1f3",
          variations = List(VariationLine(List("g1f3", "g8f6", "f3e5"), scoreCp = 0, depth = 12)),
          currentEvalCp = Some(0),
          ply = Some(1)
        )
      )
      .getOrElse(fail("expected assembled evidence graph"))
      .context
      .evidenceGraph
    val trajectory = graph.records.collectFirst {
      case EvidenceRecord(ref, payload: LineFactEvidence, _)
          if ref.line.exists(_.role == LineNodeRole.Played) =>
        payload.futureRootObjectMove()
    }.flatten.getOrElse(fail("expected future knight trajectory"))

    assertEquals(trajectory.rootStep.moveUci, "g1f3")
    assertEquals(trajectory.futureStep.moveUci, "f3e5")
    assertEquals(trajectory.pieceRole.name.toLowerCase, "knight")
    assertEquals(trajectory.rootFrom.key -> trajectory.rootTo.key, "g1" -> "f3")
    assertEquals(trajectory.futureFrom.key -> trajectory.futureTo.key, "f3" -> "e5")
    assertEquals(trajectory.plyOffset, 2)

  test("plan event owns a multi-actor episode only through typed line and target dependencies"):
    val result = MoveReviewJudgmentOrchestrator
      .build(nimzoB6Input())
      .getOrElse(fail("expected judgment result"))
    assert(result.isValid, result.validation.issues)
    val event = result.packet.evidenceGraph.records.collectFirst {
      case EvidenceRecord(_, payload: PlanCausalEventEvidence, _)
          if payload.planId == PlanId.WeakPawnAttack && EvidenceRef.sameMove(payload.rootMove, "b7b6") =>
        payload
    }.getOrElse(fail("expected weak-pawn causal event"))
    val episode = event.episode.getOrElse(fail("expected typed plan episode"))

    assert(episode.dependencyProven, episode)
    assertEquals(episode.events.map(_.moveUci), List("b7b6", "c6a5", "c8a6"))
    assert(episode.dependencies.exists(dependency =>
      dependency.kind == PlanCausalDependencyKind.LineAccessPrecondition &&
        EvidenceRef.sameMove(dependency.from.moveUci, "b7b6") &&
        EvidenceRef.sameMove(dependency.to.moveUci, "c8a6")
    ), episode.dependencies)
    assert(episode.dependencies.exists(dependency =>
      dependency.kind == PlanCausalDependencyKind.SharedTargetCoordination &&
        EvidenceRef.sameMove(dependency.from.moveUci, "c6a5") &&
        EvidenceRef.sameMove(dependency.to.moveUci, "c8a6") &&
        dependency.proof == PlanCausalDependencyProof.SharedTarget(List(EvidenceSquare("c4")))
    ), episode.dependencies)
    assertEquals(episode.rootOwnedFutureEvents.map(_.moveUci), List("c8a6"))
    assertEquals(episode.futureMove, Some("c8a6"))
    val transition = result.packet.evidenceGraph.records.collectFirst {
      case EvidenceRecord(_, PlanTransitionEvidence(summary), _)
          if summary.primaryPlanId.contains(PlanId.WeakPawnAttack) =>
        summary
    }.getOrElse(fail("expected episode transition"))
    assertEquals(transition.transitionType, TransitionType.Completion)
    assertEquals(transition.previousPlanId, Some(PlanId.WeakPawnAttack))
    assertEquals(transition.previousEvent.map(_.rootMove), Some("c6a5"))
    assertEquals(transition.currentEvent.map(_.rootMove), Some("c8a6"))
    assertEquals(transition.continuity.flatMap(_.principalEvent).map(_.rootMove), Some("b7b6"))
    assertEquals(transition.continuity.toList.flatMap(_.supportingMoves), List("b7b6", "c6a5"))
    assertEquals(transition.continuity.map(_.consecutivePlies), Some(5))
    assertEquals(result.packet.probeRequests.flatMap(_.candidateMove), List("b7b6"))

  test("flank episode preserves the inducing move opponent concession and same-actor maintenance"):
    val result = MoveReviewJudgmentOrchestrator
      .build(gameChangerH4Input())
      .getOrElse(fail("expected judgment result"))
    assert(result.isValid, result.validation.issues)
    val event = result.packet.evidenceGraph.records.collectFirst {
      case EvidenceRecord(_, payload: PlanCausalEventEvidence, _)
          if payload.planId == PlanId.KingsideAttack && EvidenceRef.sameMove(payload.rootMove, "h2h4") =>
        payload
    }.getOrElse(fail("expected kingside causal event"))
    val episode = event.episode.getOrElse(fail("expected flank episode"))

    assert(episode.dependencyProven, episode)
    assertEquals(episode.events.map(_.moveUci), List("h2h4", "f3g5", "g5h3"))
    assert(episode.dependencies.exists(dependency =>
      dependency.kind == PlanCausalDependencyKind.FlankAdvanceCoordination &&
        EvidenceRef.sameMove(dependency.from.moveUci, "h2h4") &&
        EvidenceRef.sameMove(dependency.to.moveUci, "f3g5")
    ), episode.dependencies)
    assert(episode.dependencies.exists(dependency =>
      dependency.kind == PlanCausalDependencyKind.ObjectStatePrecondition &&
        EvidenceRef.sameMove(dependency.from.moveUci, "f3g5") &&
        EvidenceRef.sameMove(dependency.to.moveUci, "g5h3")
    ), episode.dependencies)
    assert(episode.responses.exists(response =>
      EvidenceRef.sameMove(response.trigger.moveUci, "f3g5") &&
        EvidenceRef.sameMove(response.step.moveUci, "h7h6") &&
        response.target == EvidenceSquare("h7")
    ), episode.responses)

  test("flank episode exports multi-actor means and the induced reply after robust probing"):
    val result = MoveReviewJudgmentOrchestrator
      .build(withReplyProbeFor(gameChangerH4Input(), "h2h4", List(
        VariationLine(List("a8c8", "f3g5", "h7h6", "g5h3"), 0, depth = 16),
        VariationLine(List("d8d7", "f3g5", "h7h6", "g5h3"), 0, depth = 16),
        VariationLine(List("g7g6", "f3g5", "h7h6", "g5h3"), 0, depth = 16)
      )))
      .getOrElse(fail("expected judgment result"))
    val event = causalEvent(result, "h2h4", "f3g5")
    val proof = PlanCausalPublicProof.from(event).getOrElse(fail("expected public flank episode"))

    assert(result.isValid, result.validation.issues)
    assertEquals(event.robustness, PlanCausalRobustness.Robust)
    assertEquals(proof.dependencyKind, PlanCausalDependencyKind.FlankAdvanceCoordination)
    assertEquals(proof.sequence.map(_.move), List("h2h4", "f3g5", "g5h3"))
    assert(proof.sequence.find(step => EvidenceRef.sameMove(step.move, "f3g5")).exists(
      _.dependencyKinds.contains(PlanCausalDependencyKind.FlankAdvanceCoordination)
    ))
    assert(proof.inducedResponses.exists(response =>
      EvidenceRef.sameMove(response.triggerMove, "f3g5") &&
        EvidenceRef.sameMove(response.move, "h7h6") &&
        response.targetSquare == "h7"
    ), proof.inducedResponses)

  test("principal plan event keeps an actor destination in means without inventing a target"):
    val root = PositionNodeRef("8/8/2N5/8/8/8/8/4K2k w - - 0 1", 1, Some(Color.White))
    val after = PositionNodeRef("8/8/8/N7/8/8/8/4K2k b - - 1 1", 2, Some(Color.Black))
    val line = LineNodeRef("played-c6a5", "c6a5", 1, LineNodeRole.Played)
    val event = PlanCausalEventEvidence(
      planId = PlanId.WeakPawnAttack,
      identity = PlanEventIdentity(
        rootMove = "c6a5",
        goalTheme = PlanTheme.WeaknessFixation,
        goalKind = Some(PlanKind.StaticWeaknessFixation),
        actorRole = Some("knight"),
        actorFrom = Some("c6"),
        actorTo = Some("a5"),
        targets = Nil,
        results = List("kind:mobilitygain")
      ),
      rootLine = line,
      rootTransition = StructuralTransitionBinding(
        moveUci = "c6a5",
        role = TransitionEdgeRole.Played,
        from = root,
        to = after,
        line = Some(line),
        perspective = Color.White
      ),
      structuralConsequences = List(
        TransitionConsequence(TransitionConsequenceKind.MobilityGain, StructuralSignalPolarity.Gain, 1)
      ),
      developmentChoices = Nil,
      branchWitnesses = Nil
    )

    val proof = PlanEventPublicProof.from(event)
    assertEquals(proof.actorTo, Some("a5"))
    assertEquals(proof.targets, Nil)

  test("line trajectory does not transfer identity to a same-role replacement"):
    val graph = EvidenceFactAssembler
      .assemble(
        RawMoveReviewInput(
          fen = "4k3/8/8/8/6b1/8/3N4/4K1N1 w - - 0 1",
          playedMoveUci = "g1f3",
          variations = List(
            VariationLine(List("g1f3", "g4f3", "d2f3", "e8e7", "f3e5"), scoreCp = 0, depth = 12)
          ),
          currentEvalCp = Some(0),
          ply = Some(1)
        )
      )
      .getOrElse(fail("expected assembled evidence graph"))
      .context
      .evidenceGraph
    val trajectory = graph.records.collectFirst {
      case EvidenceRecord(ref, payload: LineFactEvidence, _)
          if ref.line.exists(_.role == LineNodeRole.Played) =>
        payload.futureRootObjectMove(payload.lineReplayCount)
    }.flatten

    assertEquals(trajectory, None)

  test("future plan event stays internal until reply robustness is observed"):
    val result = MoveReviewJudgmentOrchestrator
      .build(doknjasInput())
      .getOrElse(fail("expected judgment result"))
    val eventRecord = result.packet.evidenceGraph.records.collectFirst {
      case record @ EvidenceRecord(_, payload: PlanCausalEventEvidence, _)
          if EvidenceRef.sameMove(payload.rootMove, "b7b5") && payload.episode.exists(
            _.events.exists(event => EvidenceRef.sameMove(event.moveUci, "b5b4"))
          ) =>
        record -> payload
    }.getOrElse(fail("expected causal event"))

    assert(eventRecord._2.counterfactualDependencyProven)
    assertEquals(eventRecord._2.robustness, PlanCausalRobustness.Untested)
    assert(!eventRecord._2.episodePublicProofReady)
    val publicEvent = PlanEventPublicProof.from(eventRecord._2)
    assert(publicEvent.results.forall(_.stage == "direct"), publicEvent.results)
    assertEquals(publicEvent.responses, Nil)
    assertEquals(result.packet.probeRequests.flatMap(_.candidateMove), List("b7b5"))
    assert(!EvidenceObjectBinding.fromEvidenceRefs(result.packet.evidenceGraph, List(eventRecord._1.ref)).exists(binding =>
      binding.witness.exists(obj => obj.kind == EvidenceObjectKind.Move && obj.key == "b5b4")
    ))

  test("reply probe certifies future trajectory under event-owned plan authority"):
    val input = doknjasComparisonInput()
    val result = MoveReviewJudgmentOrchestrator
      .build(withReplyProbe(input, List(
        VariationLine(List("b2b4", "a5b4", "f4d2", "b4b2", "c1c2", "b2a3", "f1e2", "b5b4"), 0, depth = 16),
        VariationLine(List("f1e2", "b5b4"), 0, depth = 16),
        VariationLine(List("h2h3", "b5b4"), 0, depth = 16)
      )))
      .getOrElse(fail("expected judgment result"))
    val event = causalEvent(result, "b7b5", "b5b4")

    assert(result.isValid, result.validation.issues)
    assertEquals(event.branchWitnesses.size, 3)
    assert(event.branchWitnesses.forall(_.outcome == PlanCausalBranchOutcome.Realized), event.branchWitnesses)
    assert(event.branchWitnesses.forall(_.realizationMatch.contains(PlanCausalRealizationMatch.ExactMove)), event.branchWitnesses)
    assertEquals(event.robustness, PlanCausalRobustness.Robust)
    assert(event.episodePublicProofReady)
    val planSignals = planCoherenceSignals(result, "b7b5")
    assertEquals(planSignals.map(_.source.layer).distinct, List(EvidenceLayer.PlanCausalEvent))
    assertEquals(planSignals.flatMap(_.axis.map(_.polarity)).distinct, List(StrategicAxisPolarity.Gain))
    assert(planRelativeCauses(result, "b7b5").contains(RelativeCauseKind.PlanImprovement))
    val publicEvent = PlanEventPublicProof.from(event)
    val causalProof = publicEvent.futureCausality.getOrElse(fail("expected public causal episode"))
    assertEquals(causalProof.futureMove, "b5b4")
    assertEquals(causalProof.dependencyKind, PlanCausalDependencyKind.ObjectStatePrecondition)
    assertEquals(causalProof.sequence.filter(_.rootOwned).map(_.move), List("b7b5", "b5b4"))
    assert(causalProof.sequence.exists(step => !step.rootOwned && step.actorRole.contains("queen")))
    assertEquals(publicEvent.results, causalProof.representativeResult.toList)
    assertEquals(publicEvent.results.map(_.stage), List("future"))
    assertEquals(result.packet.probeRequests, Nil)
    val view = result.packet.moveJudgmentView.getOrElse(fail("expected move judgment view"))
    val eventRecord = result.packet.evidenceGraph.records.collectFirst {
      case record @ EvidenceRecord(_, payload: PlanCausalEventEvidence, _)
          if payload.episode.exists(_.events.exists(event => EvidenceRef.sameMove(event.moveUci, "b5b4"))) => record
    }.getOrElse(fail("expected internal causal event"))
    assertEquals(eventRecord.ref.confidence, EvidenceConfidence.Mixed)
    val episodeBinding = EvidenceObjectBinding
      .fromEvidenceRefs(result.packet.evidenceGraph, List(eventRecord.ref))
      .find(binding => binding.horizon.contains(s"ply:${causalProof.plyOffset}"))
      .getOrElse(fail("expected representative episode binding"))
    assert(episodeBinding.witness.exists(obj => obj.kind == EvidenceObjectKind.Move && obj.key == causalProof.futureMove))
    assert(episodeBinding.target.exists(obj => obj.kind == EvidenceObjectKind.Square && obj.key == causalProof.targetSquare))
    assert(view.moveMeaningClaims.exists(claim =>
      claim.moveUci == "b7b5" &&
        claim.positiveFunctionalProofEvidenceIds.exists(id =>
          result.packet.evidenceGraph.byId.get(id).exists {
            case EvidenceRecord(ref, _: PlanCausalEventEvidence, _) =>
              ref.confidence != EvidenceConfidence.Heuristic
            case _ => false
          }
        )
    ), view.moveMeaningClaims)
    val publicJson = MoveMeaningSurface.publicPayloadJson(view)
    assert((publicJson \\ "future_move").exists(_.asOpt[String].contains("b5b4")), publicJson)
    val planEventJson = (publicJson \\ "principal_plan_event").collectFirst {
      case value: play.api.libs.json.JsObject
          if (value \ "means" \ "future_move").asOpt[String].contains("b5b4") =>
        value
    }.getOrElse(fail("expected public principal plan event"))
    val sequenceMoves = (planEventJson \ "means" \ "sequence")
      .asOpt[List[play.api.libs.json.JsObject]]
      .getOrElse(Nil)
      .flatMap(step => (step \ "move").asOpt[String])
    assert(sequenceMoves.contains("b7b5") && sequenceMoves.contains("b5b4"), sequenceMoves)
    assertEquals(
      (planEventJson \ "results").asOpt[List[play.api.libs.json.JsObject]].getOrElse(Nil).size,
      1
    )
    val semanticCodes = (publicJson \\ "move_semantics")
      .flatMap(_.asOpt[List[play.api.libs.json.JsObject]].getOrElse(Nil))
      .flatMap(semantic => (semantic \ "idea" \ "code").asOpt[String])
    val planIndex = semanticCodes.indexOf("plan_continuity")
    val specificIndex = semanticCodes.indexWhere(_ != "plan_continuity")
    assert(specificIndex >= 0 && (planIndex < 0 || specificIndex < planIndex), semanticCodes)

  test("future realization accepts a different move only when typed function and subject agree"):
    val expected = TransitionConsequence(
      TransitionConsequenceKind.TargetPressureGain,
      StructuralSignalPolarity.Gain,
      2,
      List("weak-pawn:c4")
    )
    val equivalent = expected.copy(strength = 1)
    val differentTarget = expected.copy(subjects = List("weak-pawn:f4"))

    assertEquals(
      PlanCausalFunctionalMatch.classify("b5b4", List(expected), "b5a4", List(equivalent)),
      Some(PlanCausalRealizationMatch.EquivalentFunction)
    )
    assertEquals(
      PlanCausalFunctionalMatch.classify("b5b4", List(expected), "b5a4", List(differentTarget)),
      None
    )
    assertEquals(
      PlanCausalFunctionalMatch.classify("b5b4", List(expected.copy(subjects = Nil)), "b5a4", List(equivalent)),
      None
    )

  test("plan event results must support the typed plan goal"):
    val weaknessPlan = PlanMatch(
      plan = Plan.WeakPawnAttack(Color.White),
      score = 0.8,
      evidence = Nil,
      support = List(
        PlanSupport.Theme(PlanTheme.WeaknessFixation),
        PlanSupport.Subplan(PlanKind.StaticWeaknessFixation)
      )
    )
    val mobility = TransitionConsequence(
      TransitionConsequenceKind.MobilityGain,
      StructuralSignalPolarity.Gain,
      2,
      List("bishop:c8-a6:mobility+2")
    )
    val pressure = TransitionConsequence(
      TransitionConsequenceKind.TargetPressureGain,
      StructuralSignalPolarity.Gain,
      2,
      List("weak-pawn:c5")
    )

    assert(!PlanCausalEventProof.consequenceSupportsPlan(weaknessPlan, mobility))
    assert(PlanCausalEventProof.consequenceSupportsPlan(weaknessPlan, pressure))
    assert(!PlanCausalEventProof.developmentSupportsPlan(weaknessPlan))

    val before = PositionNodeRef(
      "r1bq1bk1/p1n3r1/p2p2n1/3Pp1pp/1N2Pp2/2N2P2/1P2BBPP/R2Q1R1K w - - 2 21",
      40,
      Some(Color.White)
    )
    val targetBundle = TransitionConsequence(
      TransitionConsequenceKind.TargetPressureGain,
      StructuralSignalPolarity.Gain,
      6,
      List("a7", "d8", "e5")
    )
    val owned = PlanCausalEventProof
      .ownedConsequence(
        weaknessPlan,
        targetBundle,
        StructuralTransitionBinding(
          moveUci = "b4c6",
          role = TransitionEdgeRole.Played,
          from = before,
          to = before.copy(ply = 41, sideToMove = Some(Color.Black)),
          line = Some(LineNodeRef("played-b4c6", "b4c6", 1, LineNodeRole.Played)),
          perspective = Color.White
        )
      )
      .getOrElse(fail("expected owned weak-pawn targets"))

    assertEquals(owned.subjects, List("a7", "e5"))
    assertEquals(owned.strength, 2)

  test("reply probe preserves conditionality instead of treating an unfinished branch as refutation"):
    val input = doknjasComparisonInput()
    val result = MoveReviewJudgmentOrchestrator
      .build(withReplyProbe(input, List(
        VariationLine(List("f1e2", "b5b4"), 0, depth = 16),
        VariationLine(List("h2h3", "a7a6"), 0, depth = 16),
        VariationLine(List("g1h3", "e7e6"), 0, depth = 16)
      )))
      .getOrElse(fail("expected judgment result"))
    val event = causalEvent(result, "b7b5", "b5b4")

    assert(result.isValid, result.validation.issues)
    assertEquals(event.branchWitnesses.count(_.outcome == PlanCausalBranchOutcome.Realized), 1)
    assertEquals(event.branchWitnesses.count(_.outcome == PlanCausalBranchOutcome.Deferred), 2)
    assertEquals(event.robustness, PlanCausalRobustness.Conditional)
    assert(event.episodePublicProofReady)
    assertEquals(
      planCoherenceSignals(result, "b7b5").flatMap(_.axis.map(_.polarity)).distinct,
      List(StrategicAxisPolarity.Support)
    )

  test("reply probe refutes an unrealized episode after its representative horizon"):
    val input = doknjasComparisonInput()
    val result = MoveReviewJudgmentOrchestrator
      .build(withReplyProbe(input, List(
        VariationLine(List("f1e2", "a5a3", "b2b3", "a3a4", "a2a3", "a4a5", "a3a4", "a5a6"), 0, depth = 16),
        VariationLine(List("h2h3", "a5a3", "b2b3", "a3a4", "a2a3", "a4a5", "a3a4", "a5a6"), 0, depth = 16),
        VariationLine(List("g1h3", "a5a3", "b2b3", "a3a4", "a2a3", "a4a5", "a3a4", "a5a6"), 0, depth = 16)
      )))
      .getOrElse(fail("expected judgment result"))
    val event = causalEvent(result, "b7b5", "b5b4")

    assert(result.isValid, result.validation.issues)
    assertEquals(event.branchWitnesses.map(_.outcome).distinct, List(PlanCausalBranchOutcome.Refuted))
    assertEquals(event.robustness, PlanCausalRobustness.Refuted)
    assert(!event.episodePublicProofReady)
    assert(PlanEventPublicProof.from(event).results.forall(_.stage == "direct"))
    val planSignals = planCoherenceSignals(result, "b7b5")
    assertEquals(planSignals.map(_.source.layer).distinct, List(EvidenceLayer.PlanCausalEvent))
    assertEquals(planSignals.flatMap(_.axis.map(_.polarity)).distinct, List(StrategicAxisPolarity.Concede))
    assert(planRelativeCauses(result, "b7b5").contains(RelativeCauseKind.PlanContradiction))

  test("validator rejects a causal event whose branch outcome was re-labeled downstream"):
    val result = MoveReviewJudgmentOrchestrator
      .build(withReplyProbe(doknjasInput(), List(
        VariationLine(List("b2b4", "a5b4", "f4d2", "b4b2", "c1c2", "b2a3", "f1e2", "b5b4"), 0, depth = 16),
        VariationLine(List("f1e2", "b5b4"), 0, depth = 16),
        VariationLine(List("h2h3", "b5b4"), 0, depth = 16)
      )))
      .getOrElse(fail("expected judgment result"))
    val causalRecord = result.packet.evidenceGraph.records.collectFirst {
      case record @ EvidenceRecord(_, payload: PlanCausalEventEvidence, _)
          if payload.episode.exists(_.events.exists(event => EvidenceRef.sameMove(event.moveUci, "b5b4"))) =>
        record -> payload
    }.getOrElse(fail("expected causal record"))
    val tamperedPayload = causalRecord._2.copy(
      branchWitnesses = causalRecord._2.branchWitnesses match
        case head :: tail => head.copy(outcome = PlanCausalBranchOutcome.Refuted) :: tail
        case Nil          => fail("expected branch witnesses")
    )
    val tamperedGraph = TypedEvidenceGraph(result.packet.evidenceGraph.records.map { record =>
      if record.ref.id == causalRecord._1.ref.id then causalRecord._1.copy(payload = tamperedPayload)
      else record
    })
    val validation = JudgmentPacketValidator.validate(result.packet.copy(evidenceGraph = tamperedGraph))

    assert(validation.issues.exists(_.kind == JudgmentPacketValidationIssueKind.InvalidPlanCausalBranchProof), validation.issues)

  test("validator rejects a causal event whose principal identity was re-labeled downstream"):
    val result = MoveReviewJudgmentOrchestrator
      .build(doknjasInput())
      .getOrElse(fail("expected judgment result"))
    val causalRecord = result.packet.evidenceGraph.records.collectFirst {
      case record @ EvidenceRecord(_, payload: PlanCausalEventEvidence, _)
          if EvidenceRef.sameMove(payload.rootMove, "b7b5") =>
        record -> payload
    }.getOrElse(fail("expected causal record"))
    val tamperedGraph = TypedEvidenceGraph(result.packet.evidenceGraph.records.map { record =>
      if record.ref.id == causalRecord._1.ref.id then
        causalRecord._1.copy(payload = causalRecord._2.copy(identity = causalRecord._2.identity.copy(targets = List("square:a1"))))
      else record
    })
    val validation = JudgmentPacketValidator.validate(result.packet.copy(evidenceGraph = tamperedGraph))

    assert(validation.issues.exists(_.kind == JudgmentPacketValidationIssueKind.MismatchedPlanCausalEventBinding), validation.issues)

  test("plan transition cannot borrow a plan-pressure parent as a second authority"):
    val result = MoveReviewJudgmentOrchestrator
      .build(weakPawnInput())
      .getOrElse(fail("expected judgment result"))
    val transitionRecord = result.packet.evidenceGraph.records
      .collectFirst {
        case record @ EvidenceRecord(_, PlanTransitionEvidence(summary), _)
            if summary.primaryPlanId.contains(PlanId.WeakPawnAttack) =>
          record
      }
      .getOrElse(fail("expected weak-pawn transition"))
    val pressureRef = result.packet.evidenceGraph.records
      .collectFirst {
        case EvidenceRecord(ref, _: PlanPressureEvidence, _) if ref.line == transitionRecord.ref.line =>
          ref
      }
      .getOrElse(fail("expected matching plan pressure"))
    val tamperedGraph = TypedEvidenceGraph(result.packet.evidenceGraph.records.map { record =>
      if record.ref.id == transitionRecord.ref.id then
        record.copy(parents = (record.parents :+ pressureRef).distinctBy(_.id))
      else record
    })
    val validation = JudgmentPacketValidator.validate(result.packet.copy(evidenceGraph = tamperedGraph))

    assert(
      validation.issues.exists(_.kind == JudgmentPacketValidationIssueKind.InvalidPlanTransitionOwnership),
      validation.issues
    )

  test("the same authoritative plan event cannot be duplicated on a root line"):
    val result = MoveReviewJudgmentOrchestrator
      .build(weakPawnInput())
      .getOrElse(fail("expected judgment result"))
    val eventRecord = result.packet.evidenceGraph.records
      .collectFirst {
        case record @ EvidenceRecord(ref, _: PlanCausalEventEvidence, _)
            if ref.line.exists(
              _.role == LineNodeRole.Played
            ) && ref.confidence != EvidenceConfidence.Heuristic =>
          record
      }
      .getOrElse(fail("expected authoritative plan event"))
    val duplicate = eventRecord.copy(ref = eventRecord.ref.copy(id = s"${eventRecord.ref.id}:duplicate"))
    val validation = JudgmentPacketValidator.validate(
      result.packet.copy(evidenceGraph = TypedEvidenceGraph(result.packet.evidenceGraph.records :+ duplicate))
    )

    assert(
      validation.issues.exists(
        _.kind == JudgmentPacketValidationIssueKind.DuplicateAuthoritativePlanCausalEvent
      ),
      validation.issues
    )

  test("structural transition proof can seed a strategic relative-cause idea"):
    val root = PositionNodeRef("8/8/8/8/8/8/3P4/8 w - - 0 1", 1, Some(Color.White), Some("root"))
    val afterPlayed = PositionNodeRef("8/8/8/8/3P4/8/8/8 b - - 0 1", 2, Some(Color.Black), Some("after-played"))
    val playedLine = LineNodeRef("played-line", "d2d4", 2, LineNodeRole.Played)
    val referenceLine = LineNodeRef("reference-line", "g1f3", 1, LineNodeRole.BestReference)

    val structuralRef = evidenceRef(
      id = "structural-delta:played:d2d4",
      producer = EvidenceProducer.StructuralDeltaProducer,
      layer = EvidenceLayer.StructuralDelta,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.PlayedTransition
    )
    val transition = StructuralTransitionBinding(
      moveUci = "d2d4",
      role = TransitionEdgeRole.Played,
      from = root,
      to = afterPlayed,
      line = Some(playedLine),
      perspective = Color.White
    )
    val consequence = TransitionConsequence(
      kind = TransitionConsequenceKind.CenterControlGain,
      polarity = StructuralSignalPolarity.Gain,
      strength = 2
    )
    val cause = RelativeCauseFact(
      kind = RelativeCauseKind.StructuralImprovement,
      comparisonKind = CandidateComparisonKind.PlayedVsBest,
      referenceLine = referenceLine,
      candidateLine = playedLine,
      verdict = MoveChoiceVerdict.Inaccuracy,
      winPercentLossForMover = 4.0,
      candidateWinPercentDeltaForMover = -4.0,
      supportEvidence = List(structuralRef),
      evidenceLines = List(playedLine),
      role = RelativeCauseRole.PrimaryPlayedCause,
      eventLine = playedLine,
      sourceSide = RelativeCauseSourceSide.Candidate,
      importance = RelativeCauseImportance.Primary,
      attribution = CauseAttribution(
        kind = CauseAttributionKind.CandidateCreatesValue,
        ownedEvidence = List(structuralRef),
        rootMoveMatched = true,
        directProofEligible = true
      )
    )(
      proof = Some(
        RelativeCauseProof(
          directProof = RelativeCauseProofSection(
            role = RelativeCauseProofRole.DirectProof,
            strength = RelativeCauseProofStrength.Primary,
            transitionConsequences = List(TransitionConsequenceProof(structuralRef, transition, consequence))
          )
        )
      )
    )
    val causeRef = evidenceRef(
      id = "relative-cause:played-best:structural-improvement",
      producer = EvidenceProducer.RelativeMoveProducer,
      layer = EvidenceLayer.RelativeCause,
      position = root,
      line = Some(playedLine),
      scope = EvidenceScope.Counterfactual
    )
    val graph = TypedEvidenceGraph(
      List(
        EvidenceRecord(
          structuralRef,
          StructuralDeltaEvidence(
            transition = transition,
            signals = Nil,
            consequences = List(consequence)
          )
        ),
        EvidenceRecord(causeRef, RelativeCauseFactEvidence(cause), parents = List(structuralRef))
      )
    )

    val assembly = ChessIdeaAssembler.enrich(
      RelativeAssessmentAssembly(
        input = normalizedInput(root, afterPlayed),
        context = JudgmentAssemblyContext.empty(graph)
      )
    )

    val relativeCauseIdeas = assembly.context.ideas.filter(_.evidence.exists(_.id == causeRef.id))
    assertEquals(relativeCauseIdeas.map(_.ref.family), List(ChessIdeaFamily.Strategic))

  test("principal mobility event compresses its route without turning the destination into a target"):
    val result = MoveReviewJudgmentOrchestrator
      .build(marDelPlataKnightRouteInput())
      .getOrElse(fail("expected judgment result"))
    val event = result.packet.evidenceGraph.records.collectFirst {
      case EvidenceRecord(_, payload: PlanCausalEventEvidence, _)
          if payload.planId == PlanId.PieceActivation && EvidenceRef.sameMove(payload.rootMove, "d3b4") =>
        payload
    }.getOrElse(fail("expected piece-activation event"))
    val proof = PlanEventPublicProof.from(event)
    val surfaces = result.packet.moveJudgmentView.toList.flatMap(MoveMeaningSurface.publicSurfaces)
    val route = surfaces.filter(surface => surface.subject == "played_move" && surface.ideaType == "piece_route") match
      case value :: Nil => value
      case values       => fail(s"expected one compressed route, got $values")

    assert(result.isValid, result.validation.issues)
    assertEquals(proof.actorFrom -> proof.actorTo, Some("d3") -> Some("b4"))
    assertEquals(proof.targets, Nil)
    assertEquals(proof.results.map(_.kind), List(TransitionConsequenceKind.MobilityGain))
    assertEquals(route.principalPlanId, Some(PlanId.PieceActivation))
    assertEquals(route.idea.label, "knight maneuver to b4")
    assertEquals(route.target.squares, Nil)
    assert(!surfaces.exists(surface => surface.principalPlanId.contains(PlanId.PieceActivation) && surface.ideaType == "target_pressure"))
    assert(surfaces.exists(surface => surface.principalPlanId.contains(PlanId.WeakPawnAttack) && surface.target.squares == List("a6")))

  test("opening development does not probe a later capture as the same plan episode"):
    val result = MoveReviewJudgmentOrchestrator
      .build(morphyBishopDevelopmentInput())
      .getOrElse(fail("expected judgment result"))
    val event = result.packet.evidenceGraph.records.collectFirst {
      case EvidenceRecord(_, payload: PlanCausalEventEvidence, _)
          if payload.planId == PlanId.OpeningDevelopment && EvidenceRef.sameMove(payload.rootMove, "c8g4") =>
        payload
    }.getOrElse(fail("expected opening-development event"))
    val proof = PlanEventPublicProof.from(event)
    val surfaces = result.packet.moveJudgmentView.toList.flatMap(MoveMeaningSurface.publicSurfaces)
    val route = surfaces.filter(surface => surface.subject == "played_move" && surface.principalPlanId.contains(PlanId.OpeningDevelopment)) match
      case value :: Nil => value
      case values       => fail(s"expected one owned development route, got $values")

    assert(result.isValid, result.validation.issues)
    assertEquals(proof.targets, Nil)
    assertEquals(proof.results.map(_.kind), List(TransitionConsequenceKind.DevelopmentMobilityGain))
    assertEquals(proof.futureCausality, None)
    assert(!result.packet.probeRequests.exists(_.candidateMove.exists(EvidenceRef.sameMove(_, "c8g4"))))
    assertEquals(route.ideaType, "piece_route")
    assertEquals(route.idea.label, "bishop development to g4")
    assertEquals(route.target.squares, Nil)

  test("owned weak-pawn event absorbs the same-target surface fallback"):
    val result = MoveReviewJudgmentOrchestrator
      .build(benoniC4FixationInput())
      .getOrElse(fail("expected judgment result"))
    val surfaces = result.packet.moveJudgmentView.toList
      .flatMap(MoveMeaningSurface.publicSurfaces)
      .filter(_.subject == "played_move")
    val weakB2 = surfaces.filter(surface => surface.ideaType == "target_pressure" && surface.target.squares == List("b2"))

    assert(result.isValid, result.validation.issues)
    assertEquals(weakB2.size, 1)
    assertEquals(weakB2.head.principalPlanId, Some(PlanId.WeakPawnAttack))
    assertEquals(weakB2.head.idea.label, "weak b2 pawn")
    assert(!weakB2.head.target.squares.contains("c4"))

  private def normalizedInput(
      root: PositionNodeRef,
      afterPlayed: PositionNodeRef
  ): NormalizedMoveReviewInput =
    NormalizedMoveReviewInput(
      beforeFen = root.fen,
      playedMoveUci = "d2d4",
      beforePly = root.ply,
      sideToMove = Some(Color.White),
      afterPlayedFen = afterPlayed.fen,
      afterReferenceFen = None,
      lines = Nil,
      currentWhitePovEvalCp = 0,
      opening = None
    )

  private def doknjasInput(probeResults: List[ProbeResult] = Nil): RawMoveReviewInput =
    RawMoveReviewInput(
      fen = "r1b2rk1/pp2ppbp/5np1/q1nP4/4PB2/2N2P2/PP4PP/2RQKBNR b K - 0 10",
      playedMoveUci = "b7b5",
      variations = List(
        VariationLine(
          List("b7b5", "b2b4", "a5b4", "f4d2", "b4b2", "c1c2", "b2a3", "f1e2", "b5b4", "c3b5", "a3a5", "c2c5"),
          scoreCp = 0,
          depth = 16
        )
      ),
      currentEvalCp = Some(0),
      ply = Some(19),
      openingContext = Some(RawOpeningContext(name = Some("Grunfeld / b5-b4 counterplay versus center"))),
      movePrefixUci = List(
        "d2d4", "g8f6", "c2c4", "g7g6", "b1c3", "d7d5", "c1f4", "f8g7", "e2e3", "c7c5",
        "d4c5", "d8a5", "a1c1", "e8h8", "c4d5", "b8d7", "f2f3", "d7c5", "e3e4"
      ),
      probeResults = probeResults
    )

  private def doknjasComparisonInput(): RawMoveReviewInput =
    val base = doknjasInput()
    base.copy(variations =
      VariationLine(List("e7e6", "h2h3"), scoreCp = 0, depth = 16) :: base.variations
    )

  private def weakPawnInput(): RawMoveReviewInput =
    RawMoveReviewInput(
      fen = "r1bqk2r/p4ppp/1pnppn2/2p5/2PPP3/P1PBBP2/6PP/R2QK1NR b KQkq - 1 9",
      playedMoveUci = "c6a5",
      variations = List(
        VariationLine(
          List(
            "c6a5",
            "g1h3",
            "c8a6",
            "d1e2",
            "d8d7",
            "e4e5",
            "d6e5",
            "d4e5",
            "f6g8",
            "e1g1",
            "g8e7",
            "a1d1"
          ),
          scoreCp = 0,
          depth = 12
        )
      ),
      currentEvalCp = Some(0),
      ply = Some(17),
      openingContext = Some(RawOpeningContext(name = Some("Nimzo-Indian Saemisch / c4-pawn pressure"))),
      movePrefixUci = List(
        "d2d4",
        "g8f6",
        "c2c4",
        "e7e6",
        "b1c3",
        "f8b4",
        "a2a3",
        "b4c3",
        "b2c3",
        "c7c5",
        "f2f3",
        "b8c6",
        "e2e4",
        "d7d6",
        "c1e3",
        "b7b6",
        "f1d3"
      )
    )

  private def nimzoB6Input(): RawMoveReviewInput =
    RawMoveReviewInput(
      fen = "r1bqk2r/pp3ppp/2nppn2/2p5/2PPP3/P1P1BP2/6PP/R2QKBNR b KQkq - 1 8",
      playedMoveUci = "b7b6",
      variations = List(
        VariationLine(
          List(
            "b7b6",
            "f1d3",
            "c6a5",
            "g1h3",
            "c8a6",
            "d1e2",
            "d8d7",
            "e4e5",
            "d6e5",
            "d4e5",
            "f6g8",
            "e1g1"
          ),
          scoreCp = 0,
          depth = 12
        )
      ),
      currentEvalCp = Some(0),
      ply = Some(15),
      openingContext = Some(RawOpeningContext(name = Some("Nimzo-Indian Saemisch / c4-pawn pressure"))),
      movePrefixUci = List(
        "d2d4",
        "g8f6",
        "c2c4",
        "e7e6",
        "b1c3",
        "f8b4",
        "a2a3",
        "b4c3",
        "b2c3",
        "c7c5",
        "f2f3",
        "b8c6",
        "e2e4",
        "d7d6",
        "c1e3"
      )
    )

  private def gameChangerH4Input(): RawMoveReviewInput =
    RawMoveReviewInput(
      fen = "r2q1rk1/p3bppp/bpn1p3/3pP3/3P1B2/2P2NP1/P4PBP/R2QR1K1 w - - 3 15",
      playedMoveUci = "h2h4",
      variations = List(
        VariationLine(
          List("h2h4", "a8c8", "e1e3", "c8c7", "f3g5", "h7h6", "g5h3"),
          scoreCp = 0,
          depth = 12
        )
      ),
      currentEvalCp = Some(0),
      ply = Some(28),
      openingContext = Some(RawOpeningContext(name = Some("AlphaZero / h4 and Ng5-h3 kingside concession extraction"))),
      movePrefixUci = List(
        "d2d4", "g8f6", "g1f3", "e7e6", "c2c4", "b7b6", "g2g3", "c8b7", "f1g2", "f8b4",
        "c1d2", "b4e7", "b1c3", "c7c6", "d2f4", "e8h8", "e2e4", "d7d5", "e4e5", "f6e4",
        "c4d5", "c6d5", "e1h1", "e4c3", "b2c3", "b7a6", "f1e1", "b8c6"
      )
    )

  private def marDelPlataKnightRouteInput(): RawMoveReviewInput =
    RawMoveReviewInput(
      fen = "r1bqnbk1/p5r1/p2p2n1/3Pp1pp/4Pp2/2NN1P2/1P2BBPP/R2Q1R1K w - - 0 20",
      playedMoveUci = "d3b4",
      variations = List(VariationLine(List("d3b4", "e8c7", "b4c6", "d8f6"), scoreCp = 0, depth = 12)),
      currentEvalCp = Some(0),
      ply = Some(38),
      openingContext = Some(RawOpeningContext(name = Some("Kings Indian Mar del Plata / opposite-wing breakthrough race"))),
      movePrefixUci = List(
        "d2d4", "g8f6", "c2c4", "g7g6", "b1c3", "f8g7", "e2e4", "d7d6", "g1f3", "e8h8",
        "f1e2", "e7e5", "e1h1", "b8c6", "d4d5", "c6e7", "f3e1", "f6e8", "c1e3", "f7f5",
        "f2f3", "f5f4", "e3f2", "g6g5", "c4c5", "e7g6", "a2a4", "f8f7", "e1d3", "g7f8",
        "a4a5", "f7g7", "a5a6", "b7a6", "c5d6", "c7d6", "g1h1", "h7h5"
      )
    )

  private def morphyBishopDevelopmentInput(): RawMoveReviewInput =
    RawMoveReviewInput(
      fen = "rnbqr1k1/ppp2ppp/5n2/3P4/5P2/2PP4/P1PBB1PP/R2QK1NR b KQ - 2 10",
      playedMoveUci = "c8g4",
      variations = List(VariationLine(
        List("c8g4", "c3c4", "c7c6", "d5c6", "b8c6", "e1f1", "e8e2", "g1e2", "c6d4", "d1b1", "g4e2", "f1f2"),
        scoreCp = 0,
        depth = 12
      )),
      currentEvalCp = Some(0),
      ply = Some(19),
      openingContext = Some(RawOpeningContext(name = Some("Kings Gambit / positional pawn sacrifice to open files"))),
      movePrefixUci = List(
        "e2e4", "e7e5", "f2f4", "d7d5", "e4d5", "e5e4", "b1c3", "g8f6", "d2d3", "f8b4",
        "c1d2", "e4e3", "d2e3", "e8h8", "e3d2", "b4c3", "b2c3", "f8e8", "f1e2"
      )
    )

  private def benoniC4FixationInput(): RawMoveReviewInput =
    RawMoveReviewInput(
      fen = "r2qr1k1/1p1n1pbp/p2p1np1/2pP4/P3PP2/2N2Q2/1P1N2PP/R1B2RK1 b - - 2 14",
      playedMoveUci = "c5c4",
      variations = List(VariationLine(
        List("c5c4", "d2c4", "a8c8", "c4d6", "d8b6", "g1h1", "b6d6", "e4e5", "d7e5", "f4e5", "d6e5", "c1f4"),
        scoreCp = 0,
        depth = 12
      )),
      currentEvalCp = Some(0),
      ply = Some(27),
      openingContext = Some(RawOpeningContext(name = Some("Modern Benoni / ...c4 pawn sacrifice"))),
      movePrefixUci = List(
        "d2d4", "g8f6", "c2c4", "c7c5", "d4d5", "e7e6", "b1c3", "e6d5", "c4d5", "d7d6",
        "e2e4", "g7g6", "g1f3", "f8g7", "f1e2", "e8h8", "e1h1", "a7a6", "a2a4", "c8g4",
        "f3d2", "g4e2", "d1e2", "b8d7", "f2f4", "f8e8", "e2f3"
      )
    )

  private def withReplyProbe(input: RawMoveReviewInput, replyLines: List[VariationLine]): RawMoveReviewInput =
    withReplyProbeFor(input, "b7b5", replyLines)

  private def withReplyProbeFor(
      input: RawMoveReviewInput,
      candidateMove: String,
      replyLines: List[VariationLine]
  ): RawMoveReviewInput =
    val request = MoveReviewJudgmentOrchestrator
      .build(input)
      .getOrElse(fail("expected initial probe request"))
      .packet
      .probeRequests
      .find(_.candidateMove.exists(EvidenceRef.sameMove(_, candidateMove)))
      .getOrElse(fail(s"expected $candidateMove reply probe request"))
    input.copy(probeResults = List(
      ProbeResult(
        id = request.id,
        fen = Some(request.fen),
        evalCp = 0,
        replyLines = Some(replyLines),
        deltaVsBaseline = 0,
        purpose = request.purpose,
        probedMove = request.candidateMove,
        depth = Some(request.depth),
        objective = request.objective,
        requiredSignals = request.requiredSignals,
        candidateMove = request.candidateMove,
        depthFloor = request.depthFloor,
        variationHash = request.variationHash
      )
    ))

  private def causalEvent(
      result: MoveReviewJudgmentResult,
      rootMove: String,
      futureMove: String
  ): PlanCausalEventEvidence =
    result.packet.evidenceGraph.records.collectFirst {
      case EvidenceRecord(_, payload: PlanCausalEventEvidence, _)
          if EvidenceRef.sameMove(payload.rootMove, rootMove) && payload.episode.exists(
            _.events.exists(event => EvidenceRef.sameMove(event.moveUci, futureMove))
          ) =>
        payload
    }.getOrElse(fail("expected causal event"))

  private def planCoherenceSignals(
      result: MoveReviewJudgmentResult,
      rootMove: String
  ): List[StrategicMechanismSignal] =
    result.packet.evidenceGraph.records.collect {
      case EvidenceRecord(ref, payload: StrategicMechanismEvidence, _)
          if ref.line.exists(line => EvidenceRef.sameMove(line.rootMove, rootMove)) &&
            payload.kind == StrategicMechanismKind.PlanPressure =>
        payload.signals.filter(_.axis.exists(_.kind == StrategicAxisKind.PlanCoherence))
    }.flatten

  private def planRelativeCauses(
      result: MoveReviewJudgmentResult,
      rootMove: String
  ): List[RelativeCauseKind] =
    result.packet.evidenceGraph.records.collect {
      case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _)
          if EvidenceRef.sameMove(cause.candidateLine.rootMove, rootMove) =>
        cause.kind
    }.distinct

  private def evidenceRef(
      id: String,
      producer: EvidenceProducer,
      layer: EvidenceLayer,
      position: PositionNodeRef,
      line: Option[LineNodeRef],
      scope: EvidenceScope
  ): EvidenceRef =
    EvidenceRef(
      id = id,
      producer = producer,
      layer = layer,
      position = position,
      line = line,
      scope = scope,
      confidence = EvidenceConfidence.EngineBacked
    )
