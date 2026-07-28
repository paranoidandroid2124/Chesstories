package lila.chessjudgment.analysis.assembly

import chess.{ Knight, White }
import lila.chessjudgment.model.{ ActivePlans, Plan, PlanEventIdentity, PlanMatch }
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.PrincipalVariationEvidence
import lila.chessjudgment.model.strategic.{ EngineLine, PlanTaxonomy }
import PlanTaxonomy.PlanKind

class PlanResultEndpointInventoryTest extends munit.FunSuite:

  private final case class InventoryFixture(
      root: PositionNodeRef,
      line: LineNodeRef,
      lineRecord: EvidenceRecord,
      structuralRecord: EvidenceRecord,
      pressureRecord: EvidenceRecord,
      robustEvent: EvidenceRecord,
      refutedEvent: EvidenceRecord
  ):
    def baseRecords: List[EvidenceRecord] =
      List(lineRecord, structuralRecord, pressureRecord)

  test("plan-result inventory accepts only exact post-root pressure scopes for comparison roles"):
    val expected = List(
      LineNodeRole.Played -> EvidenceScope.AfterPlayedPosition,
      LineNodeRole.BestReference -> EvidenceScope.AfterReferencePosition,
      LineNodeRole.Alternative -> EvidenceScope.AlternativeTransition
    )

    expected.foreach { case (role, scope) =>
      val fixture = inventoryFixture(role)
      assertEquals(RelativeAssessmentAssembler.exactPostRootPlanPressureScope(role), Some(scope))
      val exact = inventory(fixture, fixture.baseRecords)
      assert(exact.extractionReady)
      assert(exact.enumeratedPlanIds(PlanKind.WorstPieceImprovement.id))

      val lineScopePressure = fixture.pressureRecord.copy(
        ref = fixture.pressureRecord.ref.copy(
          id = s"${fixture.pressureRecord.ref.id}-line-scope",
          scope = fixture.line.role.scope
        )
      )
      val wrongScope = inventory(
        fixture,
        List(fixture.lineRecord, fixture.structuralRecord, lineScopePressure)
      )
      assert(!wrongScope.extractionReady)

      val wrongPlyPressure = fixture.pressureRecord.copy(
        ref = fixture.pressureRecord.ref.copy(
          id = s"${fixture.pressureRecord.ref.id}-wrong-ply",
          position = fixture.pressureRecord.ref.position.copy(ply = fixture.root.ply + 2)
        )
      )
      val wrongPly = inventory(
        fixture,
        List(fixture.lineRecord, fixture.structuralRecord, wrongPlyPressure)
      )
      assert(!wrongPly.extractionReady)
    }

    val threat = inventoryFixture(LineNodeRole.Threat)
    assertEquals(
      RelativeAssessmentAssembler.exactPostRootPlanPressureScope(LineNodeRole.Threat),
      None
    )
    assert(!inventory(threat, threat.baseRecords).extractionReady)

  test("enumerated plan absence is complete empty while non-enumeration stays incomplete"):
    val fixture = inventoryFixture(LineNodeRole.Played)
    val observed = inventory(fixture, fixture.baseRecords :+ fixture.robustEvent)
      .observations
      .headOption
      .getOrElse(fail("expected an exact robust plan-result observation"))

    val noEvent = inventory(fixture, fixture.baseRecords)
    assertEquals(noEvent.uniqueObservationFor(observed.scope), Some(None))

    val unenumeratedScope = observed.scope.copy(
      effectIdentity = observed.scope.effectIdentity.copy(
        planIds = List(PlanKind.FlankClamp.id)
      )
    )
    assertEquals(noEvent.uniqueObservationFor(unenumeratedScope), None)

  test("heuristic same-plan events make that enumerated plan incomplete"):
    val fixture = inventoryFixture(LineNodeRole.Played)
    val exactInventory = inventory(fixture, fixture.baseRecords :+ fixture.robustEvent)
    val exactScope = exactInventory.observations.headOption
      .getOrElse(fail("expected an exact robust plan-result observation"))
      .scope
    val heuristic = fixture.robustEvent.copy(
      ref = fixture.robustEvent.ref.copy(
        id = s"${fixture.robustEvent.ref.id}-heuristic",
        confidence = EvidenceConfidence.Heuristic
      )
    )
    val result = inventory(fixture, fixture.baseRecords :+ heuristic)

    assert(result.extractionReady)
    assert(result.enumeratedPlanIds(PlanKind.WorstPieceImprovement.id))
    assert(result.incompletePlanIds(PlanKind.WorstPieceImprovement.id))
    assertEquals(result.uniqueObservationFor(exactScope), None)

  test("semantic duplicate carriers and mixed exact siblings are id-order independent"):
    val fixture = inventoryFixture(LineNodeRole.Played)
    val duplicateLine = fixture.lineRecord.copy(
      ref = fixture.lineRecord.ref.copy(id = s"${fixture.lineRecord.ref.id}-duplicate")
    )
    val duplicateStructural = fixture.structuralRecord.copy(
      ref = fixture.structuralRecord.ref.copy(id = s"${fixture.structuralRecord.ref.id}-duplicate")
    )
    val duplicatePressure = fixture.pressureRecord.copy(
      ref = fixture.pressureRecord.ref.copy(id = s"${fixture.pressureRecord.ref.id}-duplicate"),
      parents = List(duplicateStructural.ref)
    )
    def reparent(record: EvidenceRecord): EvidenceRecord =
      record.copy(
        parents = List(duplicateLine.ref, duplicateStructural.ref, duplicatePressure.ref)
      )
    val records = List(
      fixture.lineRecord,
      duplicateLine,
      fixture.structuralRecord,
      duplicateStructural,
      fixture.pressureRecord,
      duplicatePressure,
      reparent(fixture.robustEvent),
      reparent(fixture.refutedEvent)
    )
    val forward = inventory(fixture, records)
    val reverse = inventory(fixture, records.reverse)

    assertEquals(forward.observations, reverse.observations)
    assertEquals(
      forward.observations.map(_.scope.directChange),
      Set(DirectCausalChange.Occurred, DirectCausalChange.Refuted)
    )
    assertEquals(forward.incompletePlanIds, Set.empty[String])

  test("plan-result differential admits only exact source against a complete weaker counterpart"):
    val fixture = inventoryFixture(LineNodeRole.Played)
    val source = inventory(fixture, fixture.baseRecords :+ fixture.refutedEvent)
    val sourceObservation = source.observations.headOption
      .getOrElse(fail("expected an exact refuted source observation"))
    val completeEmpty = inventory(fixture, fixture.baseRecords)
    assert(RelativeAssessmentAssembler.PlanResultDifferentialPolicy.admitted(
      source,
      completeEmpty,
      sourceObservation
    ))

    assert(!RelativeAssessmentAssembler.PlanResultDifferentialPolicy.admitted(
      source,
      source,
      sourceObservation
    ))

    val nonEnumerated = RelativeAssessmentAssembler.PlanResultEndpointInventory(
      observations = Set.empty,
      enumeratedPlanIds = Set.empty,
      incompletePlanIds = Set.empty,
      extractionReady = true
    )
    assert(!RelativeAssessmentAssembler.PlanResultDifferentialPolicy.admitted(
      source,
      nonEnumerated,
      sourceObservation
    ))

    val heuristicEvent = fixture.refutedEvent.copy(
      ref = fixture.refutedEvent.ref.copy(
        id = s"${fixture.refutedEvent.ref.id}-heuristic-counterpart",
        confidence = EvidenceConfidence.Heuristic
      )
    )
    val unresolved = inventory(fixture, fixture.baseRecords :+ heuristicEvent)
    assert(!RelativeAssessmentAssembler.PlanResultDifferentialPolicy.admitted(
      source,
      unresolved,
      sourceObservation
    ))

  test("a PlanResult wrapper cannot bypass an incomplete plan counterpart through strategic inventory"):
    val fixture = inventoryFixture(LineNodeRole.Played)
    val source = inventory(fixture, fixture.baseRecords :+ fixture.robustEvent)
    val event = fixture.robustEvent.payload match
      case payload: PlanCausalEventEvidence => payload
      case _                                => fail("expected plan event")
    val assessment = event.exactRobustPublicResultAssessment.getOrElse(
      fail("expected exact robust plan result")
    )
    val primitive = RootOwnedEffectProof.PlanResult(fixture.robustEvent.ref, event, assessment)
    val wrapper = DirectCauseChannel(
      binding = EvidenceObjectBinding(fixture.robustEvent.ref),
      directChange = DirectCausalChange.Occurred,
      rootOwnedProof = Some(RootOwnedEffectProof.StrategicAxis(
        primitive,
        StrategicAxisDetail(
          StrategicAxisKind.PlanCoherence,
          StrategicAxisPolarity.Gain,
          event.planId.id
        ),
        None
      ))
    )
    val incompleteCounterpart = RelativeAssessmentAssembler.PlanResultEndpointInventory(
      observations = Set.empty,
      enumeratedPlanIds = Set(event.planId.id),
      incompletePlanIds = Set(event.planId.id),
      extractionReady = true
    )
    val cause = RelativeCauseFact(
      RelativeCauseKind.PlanImprovement,
      fixture.robustEvent.ref,
      List(fixture.robustEvent.ref),
      RelativeCauseSourceSide.Candidate
    )

    assertEquals(
      wrapper.rootOwnedEffectDescriptor.map(_.identity.primitiveKind),
      Some(RootOwnedEffectPrimitiveKind.PlanResult)
    )
    assertEquals(
      RootOwnedEffectPolicy.exactPlanResultPrimitive(wrapper.rootOwnedProof.get),
      None
    )
    assert(!RelativeAssessmentAssembler.PlanResultDifferentialPolicy.admittedChannel(
      source,
      incompleteCounterpart,
      cause,
      wrapper,
      TypedEvidenceGraph.empty
    ))

  test("PVB retains an exact played plan refutation without counterpart plan enumeration"):
    val fixture = retainedLiabilityFixture()
    val counterpart = RelativeAssessmentAssembler.planResultEndpointInventory(
      fixture.graph,
      fixture.comparison.referenceLine,
      White,
      fixture.root
    )

    assert(fixture.comparison.comparison.verdict.isActionableLoss)
    assert(!counterpart.enumeratedPlanIds(fixture.event.planId.id))
    assert(
      ComparisonEndpointEffectObservationPolicy.retainedPlayedLiabilityReady(
        fixture.cause,
        fixture.channel,
        fixture.comparison,
        fixture.graph
      )
    )

  test("retained played plan liability rejects every broader comparison or proof authority"):
    val valid = retainedLiabilityFixture()
    def retained(fixture: RetainedLiabilityFixture): Boolean =
      ComparisonEndpointEffectObservationPolicy.retainedPlayedLiabilityReady(
        fixture.cause,
        fixture.channel,
        fixture.comparison,
        fixture.graph
      )

    val playedVsAlternative = retainedLiabilityFixture(
      comparisonKind = CandidateComparisonKind.PlayedVsAlternative
    )
    val nonActionable = retainedLiabilityFixture(actionableLoss = false)
    val positive = retainedLiabilityFixture(robust = true)
    val heuristic = retainedLiabilityFixture(confidence = EvidenceConfidence.Heuristic)
    val primitive = valid.channel.rootOwnedProof.getOrElse(fail("expected exact plan proof"))
    val wrapper = valid.channel.copy(rootOwnedProof = Some(RootOwnedEffectProof.StrategicAxis(
      primitive,
      StrategicAxisDetail(
        StrategicAxisKind.PlanCoherence,
        StrategicAxisPolarity.Concede,
        valid.event.planId.id
      ),
      None
    )))

    assert(!retained(playedVsAlternative))
    assert(!retained(nonActionable))
    assert(!retained(positive))
    assert(!retained(heuristic))
    assert(!ComparisonEndpointEffectObservationPolicy.retainedPlayedLiabilityReady(
      valid.cause,
      wrapper,
      valid.comparison,
      valid.graph
    ))
    assert(!ComparisonEndpointEffectObservationPolicy.retainedPlayedLiabilityReady(
      valid.cause.copy(sourceSide = RelativeCauseSourceSide.Reference),
      valid.channel,
      valid.comparison,
      valid.graph
    ))
    assert(!ComparisonEndpointEffectObservationPolicy.retainedPlayedLiabilityReady(
      valid.cause.copy(attribution = valid.cause.attribution.copy(
        kind = CauseAttributionKind.CandidateCreatesValue
      )),
      valid.channel,
      valid.comparison,
      valid.graph
    ))
    assert(!ComparisonEndpointEffectObservationPolicy.retainedPlayedLiabilityReady(
      valid.cause,
      valid.channel.copy(importanceDescriptorAmbiguous = true),
      valid.comparison,
      valid.graph
    ))
    assert(ComparisonEndpointEffectObservationPolicy.retainedPlayedLiabilityReady(
      valid.cause,
      valid.channel.copy(proofSegmentAmbiguous = true),
      valid.comparison,
      valid.graph
    ))

  private final case class RetainedLiabilityFixture(
      root: PositionNodeRef,
      cause: RelativeCauseFact,
      comparison: CandidateComparisonFact,
      graph: TypedEvidenceGraph,
      channel: DirectCauseChannel,
      event: PlanCausalEventEvidence
  )

  private def retainedLiabilityFixture(
      comparisonKind: CandidateComparisonKind = CandidateComparisonKind.PlayedVsBest,
      actionableLoss: Boolean = true,
      robust: Boolean = false,
      confidence: EvidenceConfidence = EvidenceConfidence.EngineBacked
  ): RetainedLiabilityFixture =
    val inventory = inventoryFixture(LineNodeRole.Played)
    val baseEventRecord = if robust then inventory.robustEvent else inventory.refutedEvent
    val eventRecord = baseEventRecord.copy(
      ref = baseEventRecord.ref.copy(confidence = confidence)
    )
    val event = eventRecord.payload match
      case payload: PlanCausalEventEvidence => payload
      case _                                => fail("expected plan event")
    val referenceLine = LineNodeRef(
      "retained-plan-reference",
      "g1f3",
      20,
      LineNodeRole.BestReference
    )
    val comparison = CandidateComparisonFact(
      comparisonKind,
      referenceLine,
      inventory.line,
      EvalComparison.fromLines(
        White,
        CandidateLineNode(
          referenceLine,
          EngineLine(List(referenceLine.rootMove), 300, depth = 18),
          evidenceRef(
            "retained-plan-reference-eval",
            EvidenceProducer.EngineEvalProducer,
            EvidenceLayer.Eval,
            inventory.root,
            referenceLine,
            EvidenceScope.BestLine,
            EvidenceConfidence.EngineBacked
          )
        ),
        CandidateLineNode(
          inventory.line,
          EngineLine(
            List(inventory.line.rootMove),
            if actionableLoss then -300 else 300,
            depth = 18
          ),
          evidenceRef(
            "retained-plan-candidate-eval",
            EvidenceProducer.EngineEvalProducer,
            EvidenceLayer.Eval,
            inventory.root,
            inventory.line,
            EvidenceScope.PlayedLine,
            EvidenceConfidence.EngineBacked
          )
        )
      )
    )
    val comparisonRef = EvidenceRef(
      "retained-plan-comparison",
      EvidenceProducer.RelativeMoveProducer,
      EvidenceLayer.CandidateComparison,
      inventory.root,
      Some(inventory.line),
      EvidenceScope.Counterfactual,
      EvidenceConfidence.EngineBacked
    )
    val cause = RelativeCauseFact(
      kind = if robust then RelativeCauseKind.PlanImprovement else RelativeCauseKind.PlanContradiction,
      comparisonEvidence = comparisonRef,
      supportEvidence = List(eventRecord.ref),
      sourceSide = RelativeCauseSourceSide.Candidate,
      attribution = CauseAttribution(
        if robust then CauseAttributionKind.CandidateCreatesValue
        else CauseAttributionKind.CandidateAllowsLiability,
        rootMoveMatched = true,
        directProofEligible = true
      ),
      proof = Some(RelativeCauseProof(
        directProof = RelativeCauseProofSection(
          RelativeCauseProofRole.DirectProof,
          RelativeCauseProofStrength.Primary,
          List(eventRecord.ref)
        )
      ))
    )
    val graph = (
      inventory.baseRecords ++
        List(
          eventRecord,
          EvidenceRecord(comparisonRef, CandidateComparisonEvidence(comparison))
        )
    ).foldLeft(TypedEvidenceGraph.empty)((current, record) => current.add(record))
    val channel = EvidenceObjectBinding
      .rawDirectSentenceChannelsForProjection(cause, graph)
      .headOption
      .getOrElse(fail("expected exact candidate plan-result channel"))

    RetainedLiabilityFixture(inventory.root, cause, comparison, graph, channel, event)

  private def inventory(
      fixture: InventoryFixture,
      records: List[EvidenceRecord]
  ): RelativeAssessmentAssembler.PlanResultEndpointInventory =
    val graph = records.foldLeft(TypedEvidenceGraph.empty)((current, record) => current.add(record))
    RelativeAssessmentAssembler.planResultEndpointInventory(
      graph,
      fixture.line,
      White,
      fixture.root
    )

  private def inventoryFixture(role: LineNodeRole): InventoryFixture =
    val id = role.toString.toLowerCase
    val fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    val rootStep = legalStep(fen, "b1c3", 1)
    val replyStep = legalStep(rootStep.fenAfter, "a7a6", 2)
    val resultStep = legalStep(replyStep.fenAfter, "c3d5", 3)
    val root = PositionNodeRef(fen, 0, Some(White))
    val afterRoot = PositionNodeRef(rootStep.fenAfter, 1, Some(!White))
    val line = LineNodeRef(s"plan-inventory-$id", rootStep.moveUci, 1, role)
    val transitionRole = role match
      case LineNodeRole.Played        => TransitionEdgeRole.Played
      case LineNodeRole.BestReference => TransitionEdgeRole.Reference
      case LineNodeRole.Alternative   => TransitionEdgeRole.Alternative
      case LineNodeRole.Threat        => TransitionEdgeRole.Threat
    val transition = StructuralTransitionBinding(
      moveUci = rootStep.moveUci,
      role = transitionRole,
      from = root,
      to = afterRoot,
      line = Some(line),
      perspective = White
    )
    val lineRef = evidenceRef(
      s"plan-inventory-$id-line",
      EvidenceProducer.LegalLineProducer,
      EvidenceLayer.Line,
      root,
      line,
      line.role.scope
    )
    val lineRecord = EvidenceRecord(
      lineRef,
      LineFactEvidence(line, replay = List(rootStep, replyStep, resultStep))
    )
    val structuralRef = evidenceRef(
      s"plan-inventory-$id-structural",
      EvidenceProducer.StructuralDeltaProducer,
      EvidenceLayer.StructuralDelta,
      root,
      line,
      transitionRole.scope
    )
    val structuralRecord = EvidenceRecord(
      structuralRef,
      StructuralDeltaEvidence(transition, signals = Nil, consequences = Nil),
      parents = List(lineRef)
    )
    val pressureScope = RelativeAssessmentAssembler
      .exactPostRootPlanPressureScope(role)
      .getOrElse(EvidenceScope.ThreatLine)
    val pressureRef = evidenceRef(
      s"plan-inventory-$id-pressure",
      EvidenceProducer.PlanPressureProducer,
      EvidenceLayer.PlanPressure,
      afterRoot,
      line,
      pressureScope
    )
    val pressureRecord = EvidenceRecord(
      pressureRef,
      PlanPressureEvidence(
        ActivePlans(List(PlanMatch(
          plan = Plan(PlanKind.WorstPieceImprovement, White),
          score = 1.0,
          evidence = Nil
        ))),
        alignment = None
      ),
      parents = List(structuralRef)
    )

    def eventRecord(robust: Boolean): EvidenceRecord =
      val consequence = TransitionConsequence(
        TransitionConsequenceKind.MobilityGain,
        StructuralSignalPolarity.Gain,
        strength = 2,
        subjects = List("knight:c3-d5")
      )
      val rootNode = PlanCausalEventNode(
        PlanEventIdentity(
          rootStep.moveUci,
          PlanKind.WorstPieceImprovement,
          Some(Knight.name),
          Some("b1"),
          Some("c3"),
          Nil,
          Nil
        ),
        rootStep,
        White,
        Nil,
        Nil
      )
      val resultNode = PlanCausalEventNode(
        PlanEventIdentity(
          resultStep.moveUci,
          PlanKind.WorstPieceImprovement,
          Some(Knight.name),
          Some("c3"),
          Some("d5"),
          List("knight:c3-d5"),
          List("mobility-gain")
        ),
        resultStep,
        White,
        List(consequence),
        Nil
      )
      val trajectory = LineObjectTrajectory
        .find(rootStep, List(replyStep, resultStep), maxPlyOffset = 2)
        .getOrElse(fail("expected the root knight trajectory"))
      val episode = PlanCausalEpisode(
        rootNode,
        List(resultNode),
        List(PlanCausalEventDependency(
          rootNode,
          resultNode,
          PlanCausalDependencyKind.ObjectStatePrecondition,
          PlanCausalDependencyProof.ObjectState(trajectory),
          plyOffset = 2
        )),
        Nil
      )
      val witnesses = List("a7a6", "a7a5", "b7b6").zipWithIndex.map { case (move, index) =>
        PlanCausalBranchWitness(
          sourceProbeId = s"plan-inventory-$id-probe",
          line = LineNodeRef(s"plan-inventory-$id-reply-$index", move, index + 1, LineNodeRole.Threat),
          outcome = if robust then PlanCausalBranchOutcome.Realized else PlanCausalBranchOutcome.Refuted,
          observedEpisode = Option.when(robust)(episode),
          observedConsequences = Option.when(robust)(List(consequence)).getOrElse(Nil),
          realizationMatch = Option.when(robust)(PlanCausalRealizationMatch.ExactMove),
          realizationMove = Option.when(robust)(resultNode.moveUci),
          requiredPlyOffset = 2,
          certifiedHorizonPlyOffset = 2,
          observedPlyOffset = 2,
          observedThroughPlyOffset = 2,
          terminalOutcome = Option.unless(robust)(PlanCausalTerminalOutcome.Defeat),
          terminalPlyOffset = Option.unless(robust)(2),
          terminalStep = None
        )
      }
      val suffix = if robust then "robust" else "refuted"
      EvidenceRecord(
        evidenceRef(
          s"plan-inventory-$id-$suffix-event",
          EvidenceProducer.PlanCausalEventProducer,
          EvidenceLayer.PlanCausalEvent,
          root,
          line,
          transitionRole.scope,
          EvidenceConfidence.EngineBacked
        ),
        PlanCausalEventEvidence(transition, episode, witnesses),
        parents = List(lineRef, structuralRef, pressureRef)
      )

    InventoryFixture(
      root,
      line,
      lineRecord,
      structuralRecord,
      pressureRecord,
      eventRecord(robust = true),
      eventRecord(robust = false)
    )

  private def evidenceRef(
      id: String,
      producer: EvidenceProducer,
      layer: EvidenceLayer,
      position: PositionNodeRef,
      line: LineNodeRef,
      scope: EvidenceScope,
      confidence: EvidenceConfidence = EvidenceConfidence.LegalReplayVerified
  ): EvidenceRef =
    EvidenceRef(id, producer, layer, position, Some(line), scope, confidence)

  private def legalStep(fen: String, move: String, ply: Int): LineReplayStep =
    LineReplayStep(
      ply,
      move,
      fen,
      PrincipalVariationEvidence
        .legalFenAfter(fen, move)
        .getOrElse(fail(s"expected legal move $move"))
    )
