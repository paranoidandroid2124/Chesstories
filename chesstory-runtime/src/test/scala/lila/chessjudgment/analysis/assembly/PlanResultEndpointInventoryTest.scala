package lila.chessjudgment.analysis.assembly

import chess.White
import lila.chessjudgment.analysis.policy.{ ClaimAdmissionStatus, ClaimTruthPolicy }
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

  test("plan-result differential admits only exact source against a complete weaker counterpart"):
    val fixture = inventoryFixture(LineNodeRole.Played)
    val source = inventory(fixture, fixture.baseRecords :+ fixture.refutedEvent)
    val sourceObservation = source.observations.headOption
      .getOrElse(fail("expected an exact refuted source observation"))
    val completeEmpty = inventory(fixture, fixture.baseRecords)
    assert(RelativeAssessmentAssembler.PlanResultDifferentialPolicy.admitted(
      source,
      completeEmpty,
      fixture.refutedEvent.ref,
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
      fixture.refutedEvent.ref,
      sourceObservation
    ))

  test("move-order differential compares endpoint-neutral PlanResults while preserving exact ownership"):
    val reference = distinctRootMoveOrderFixture(LineNodeRole.BestReference, strength = 3)

    def endpointFacts(
        candidate: InventoryFixture,
        candidateEvent: EvidenceRecord
    ): (
        JudgmentAssemblyContext,
        CandidateComparisonFact,
        RelativeAssessmentAssembler.PlanResultEndpointInventoryPair
    ) =
      val facts = RelativeAssessmentAssembler.enrichFacts(
        comparisonContext(
          reference,
          candidate,
          referenceEvents = List(reference.robustEvent),
          candidateEvents = List(candidateEvent)
        )
      )
      val comparison = facts.evidenceGraph.records.collect {
        case EvidenceRecord(_, CandidateComparisonEvidence(comparison), _)
            if comparison.kind == CandidateComparisonKind.PlayedVsBest => comparison
      } match
        case exact :: Nil => exact
        case _            => fail("expected one PlayedVsBest comparison")
      val inventories = RelativeAssessmentAssembler.planResultEndpointInventories(
        facts.evidenceGraph,
        comparison,
        reference.root
      )
      (facts, comparison, inventories)

    def sourceObservation(
        inventories: RelativeAssessmentAssembler.PlanResultEndpointInventoryPair
    ): ComparisonEndpointEffectObservation =
      inventories.reference
        .exactInducedResponseObservationFor(reference.robustEvent.ref)
        .getOrElse(fail("expected source-keyed reference move-order observation"))

    def candidateObservation(
        inventories: RelativeAssessmentAssembler.PlanResultEndpointInventoryPair
    ): ComparisonEndpointEffectObservation =
      inventories.candidate.observations.toList match
        case exact :: Nil => exact
        case _            => fail("expected one ordinary candidate PlanResult")

    def differential(
        inventories: RelativeAssessmentAssembler.PlanResultEndpointInventoryPair
    ): Boolean =
      RelativeAssessmentAssembler.PlanResultDifferentialPolicy.admitted(
        inventories.reference,
        inventories.candidate,
        reference.robustEvent.ref,
        sourceObservation(inventories)
      )

    val equalCandidate = distinctRootMoveOrderFixture(LineNodeRole.Played, strength = 3)
    val (equalFacts, equalComparison, equalInventories) =
      endpointFacts(equalCandidate, equalCandidate.robustEvent)
    val exactSource = sourceObservation(equalInventories)
    val exactCounterpart = candidateObservation(equalInventories)
    val sourceIdentity = exactSource.scope.effectIdentity.planResult
      .getOrElse(fail("expected exact source PlanResult identity"))
    val counterpartIdentity = exactCounterpart.scope.effectIdentity.planResult
      .getOrElse(fail("expected exact counterpart PlanResult identity"))
    assertNotEquals(exactSource.scope, exactCounterpart.scope)
    assertEquals(sourceIdentity.source.moveUci, "e4e5")
    assertEquals(counterpartIdentity.source.moveUci, "d5d6")
    assertEquals(
      sourceIdentity.causalRoute.map(route => route.fromMoveUci -> route.toMoveUci),
      List("f2f4" -> "e4e5")
    )
    assertEquals(
      counterpartIdentity.causalRoute.map(route => route.fromMoveUci -> route.toMoveUci),
      List("e4e5" -> "d5d6")
    )
    assert(exactSource.scope.consequenceSignatures.contains("Move:e4e5"))
    assert(!exactCounterpart.scope.consequenceSignatures.exists(_.startsWith("Move:")))
    val sourceKey = ComparisonEndpointEffectObservationPolicy
      .planResultComparisonKey(exactSource)
      .getOrElse(fail("expected source comparison key"))
    assertEquals(
      ComparisonEndpointEffectObservationPolicy.planResultComparisonKey(exactCounterpart),
      Some(sourceKey)
    )
    assert(!differential(equalInventories))
    val ordinarySource = equalInventories.reference.observations.filterNot(_ == exactSource).toList match
      case exact :: Nil => exact
      case _            => fail("expected one ordinary source PlanResult")
    assert(!RelativeAssessmentAssembler.PlanResultDifferentialPolicy.admitted(
      equalInventories.reference,
      equalInventories.candidate,
      reference.robustEvent.ref,
      ordinarySource
    ))

    val weakerCandidate = distinctRootMoveOrderFixture(LineNodeRole.Played, strength = 2)
    val (weakerFacts, _, weakerInventories) =
      endpointFacts(weakerCandidate, weakerCandidate.robustEvent)
    assertEquals(
      ComparisonEndpointEffectObservationPolicy
        .planResultComparisonKey(candidateObservation(weakerInventories)),
      Some(sourceKey)
    )
    assert(differential(weakerInventories))

    val strongerCandidate = distinctRootMoveOrderFixture(LineNodeRole.Played, strength = 4)
    val (_, _, strongerInventories) =
      endpointFacts(strongerCandidate, strongerCandidate.robustEvent)
    val strongerSource = sourceObservation(strongerInventories)
    val strongerCounterpart = candidateObservation(strongerInventories)
    assert(
      List(strongerInventories.reference, strongerInventories.candidate)
        .forall(inventory => inventory.extractionReady && inventory.incompletePlanIds.isEmpty)
    )
    assertEquals(
      ComparisonEndpointEffectObservationPolicy.planResultComparisonKey(strongerCounterpart),
      ComparisonEndpointEffectObservationPolicy.planResultComparisonKey(strongerSource)
    )
    assertEquals(
      ComparisonEndpointEffectObservationPolicy.compareMagnitude(
        strongerSource.magnitude,
        strongerCounterpart.magnitude
      ),
      ComparisonEndpointEffectObservationPolicy.MagnitudeRelation.RightStrictlyStronger
    )
    assert(!differential(strongerInventories))

    val incompleteGraph = equalFacts.evidenceGraph.records
      .filterNot(_.ref.id == equalCandidate.pressureRecord.ref.id)
      .foldLeft(TypedEvidenceGraph.empty)((graph, record) => graph.add(record))
    val incompleteInventories = RelativeAssessmentAssembler.planResultEndpointInventories(
      incompleteGraph,
      equalComparison,
      reference.root
    )
    assert(!incompleteInventories.candidate.extractionReady)
    assert(!differential(incompleteInventories))

    val unrelatedCandidate = distinctRootMoveOrderFixture(
      LineNodeRole.Played,
      strength = 3,
      consequenceKind = TransitionConsequenceKind.CenterControlGain
    )
    val (unrelatedFacts, _, unrelatedInventories) =
      endpointFacts(unrelatedCandidate, unrelatedCandidate.robustEvent)
    assert(
      List(unrelatedInventories.reference, unrelatedInventories.candidate)
        .forall(inventory => inventory.extractionReady && inventory.incompletePlanIds.isEmpty)
    )
    assertNotEquals(
      ComparisonEndpointEffectObservationPolicy
        .planResultComparisonKey(candidateObservation(unrelatedInventories)),
      Some(sourceKey)
    )
    val sourcePlanId = exactSource.scope.effectIdentity.planIds match
      case exact :: Nil => exact
      case _            => fail("expected one source plan id")
    assert(unrelatedInventories.candidate.enumeratedPlanIds(sourcePlanId))
    assertEquals(
      unrelatedInventories.candidate.uniqueComparisonMagnitudeFor(sourceKey, sourcePlanId),
      Some(None)
    )
    assert(differential(unrelatedInventories))

    val fieldBoundaryMismatches = List(
      exactCounterpart.copy(scope = exactCounterpart.scope.copy(directChange = DirectCausalChange.Lost)),
      exactCounterpart.copy(scope = exactCounterpart.scope.copy(stake = RootOwnedEffectStake.ActorLiability))
    )
    assert(fieldBoundaryMismatches.forall(observation =>
      ComparisonEndpointEffectObservationPolicy.planResultComparisonKey(observation) != Some(sourceKey)
    ))

    val production = RelativeAssessmentAssembler.enrichCauses(weakerFacts)
    val (causeRef, cause) = production.evidenceGraph.records.collect {
      case EvidenceRecord(ref, RelativeCauseFactEvidence(cause), _)
          if cause.kind == RelativeCauseKind.WrongMoveOrder => ref -> cause
    } match
      case exact :: Nil => exact
      case _            => fail("expected one stronger-source WrongMoveOrder")
    val admittedChannel = RelativeCauseConstructionAdmission
      .admittedDirectChannels(cause, production.evidenceGraph)
      .find(_.binding.source == reference.robustEvent.ref)
      .getOrElse(fail("expected admitted exact reference carrier"))
    assertEquals(
      admittedChannel.rootOwnedEffectDescriptor.map(_.identity),
      Some(sourceObservation(weakerInventories).scope.effectIdentity)
    )
    val exactProof = admittedChannel.rootOwnedProof.getOrElse(fail("expected exact PlanResult proof"))
    exactProof match
      case RootOwnedEffectProof.PlanResult(source, event, assessment) =>
        assertEquals(source, reference.robustEvent.ref)
        assertEquals(event, reference.robustEvent.payload)
        assertEquals(assessment.consequence.strength, 3)
      case _ => fail("expected PlanResult primitive")
    assertEquals(admittedChannel.proofSegment, DirectCauseProofSegment.from(exactProof))
    assert(admittedChannel.binding.consequence.exists(_.signaturePart == "Move:e4e5"))

    val snapshotWitness = RelativeAssessmentAssembler
      .comparisonEndpointEvidenceSnapshots(weakerFacts)
      .flatMap(_.reference.witnesses)
      .find(witness =>
        witness.primitiveProofSource == reference.robustEvent.ref &&
          witness.observation.contains(sourceObservation(weakerInventories))
    )
      .getOrElse(fail("expected exact snapshot witness"))
    assertEquals(snapshotWitness.effectDescriptor.identity, exactSource.scope.effectIdentity)
    assertEquals(admittedChannel.rootOwnedEffectDescriptor, Some(snapshotWitness.effectDescriptor))
    assertEquals(
      admittedChannel.binding,
      snapshotWitness.binding.copy(proofRole = Some(RelativeCauseProofRole.DirectProof))
    )
    assertEquals(snapshotWitness.rootOwnedProof, exactProof)

    val claim = JudgmentClaimAssembler
      .propose(production)
      .find(claim => claim.family == ClaimFamily.Tactical && claim.evidence.contains(causeRef))
      .getOrElse(fail("expected tactical WMO claim"))
    assertEquals(
      ClaimTruthPolicy.evaluate(claim, production.evidenceGraph).status,
      ClaimAdmissionStatus.Certified
    )
    assert(!RelativeAssessmentAssembler.enrichCauses(equalFacts).evidenceGraph.records.exists {
      case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) =>
        cause.kind == RelativeCauseKind.WrongMoveOrder
      case _ => false
    })
    val unrelatedProduction = RelativeAssessmentAssembler.enrichCauses(unrelatedFacts)
    assert(unrelatedProduction.evidenceGraph.records.exists {
      case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _)
          if cause.kind == RelativeCauseKind.WrongMoveOrder =>
        RelativeCauseConstructionAdmission
          .admittedDirectChannels(cause, unrelatedProduction.evidenceGraph)
          .nonEmpty
      case _ => false
    })

    val equalMutationGraph = production.evidenceGraph.records
      .map(record =>
        if record.ref.id == weakerCandidate.robustEvent.ref.id then equalCandidate.robustEvent
        else record
      )
      .foldLeft(TypedEvidenceGraph.empty)((graph, record) => graph.add(record))
    assert(RelativeCauseConstructionAdmission.admittedDirectChannels(cause, equalMutationGraph).nonEmpty)
    assertEquals(
      ClaimTruthPolicy.evaluate(claim, equalMutationGraph).status,
      ClaimAdmissionStatus.Deferred
    )

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
    val nonPvb = retainedLiabilityFixture(
      comparisonKind = CandidateComparisonKind.PlayedVsAlternative
    )
    assert(!ComparisonEndpointEffectObservationPolicy.retainedPlayedLiabilityReady(
      nonPvb.cause,
      nonPvb.channel,
      nonPvb.comparison,
      nonPvb.graph
    ))

  test("comparison inventory exposes one exact induced-response move-order witness"):
    val fixture = inventoryFixture(
      LineNodeRole.BestReference,
      inducedMoveOrder = true,
      additionalPlanKinds = List(PlanKind.OpeningDevelopment)
    )
    val eventRecord = fixture.robustEvent
    val event = eventRecord.payload match
      case payload: PlanCausalEventEvidence => payload
      case _                                => fail("expected plan event")
    val candidateLine = LineNodeRef(
      "induced-response-candidate",
      "e2e4",
      2,
      LineNodeRole.Played
    )
    val comparison = CandidateComparisonFact(
      CandidateComparisonKind.PlayedVsBest,
      fixture.line,
      candidateLine,
      EvalComparison.fromLines(
        White,
        CandidateLineNode(
          fixture.line,
          EngineLine(List(fixture.line.rootMove), 300, depth = 18),
          fixture.lineRecord.ref
        ),
        CandidateLineNode(
          candidateLine,
          EngineLine(List(candidateLine.rootMove), -300, depth = 18),
          fixture.lineRecord.ref.copy(
            id = "induced-response-candidate-eval",
            line = Some(candidateLine),
            scope = EvidenceScope.PlayedLine
          )
        )
      )
    )
    val comparisonRef = fixture.lineRecord.ref.copy(
      id = "induced-response-comparison",
      producer = EvidenceProducer.RelativeMoveProducer,
      layer = EvidenceLayer.CandidateComparison,
      line = Some(candidateLine),
      scope = EvidenceScope.Counterfactual
    )
    val comparisonRecord = EvidenceRecord(
      comparisonRef,
      CandidateComparisonEvidence(comparison)
    )
    val records = fixture.baseRecords ++ List(eventRecord, comparisonRecord)
    val graph = records.foldLeft(TypedEvidenceGraph.empty)((current, record) => current.add(record))

    val selected = ComparisonEndpointEffectObservationPolicy
      .exactInducedResponseMoveOrder(
        comparison,
        RelativeCauseSourceSide.Reference,
        eventRecord.ref,
        event
      )
      .getOrElse(fail("expected exact induced response"))
    assertEquals(selected._2.step.moveUci, "d5d4")

    val endpointInventory = RelativeAssessmentAssembler.planResultEndpointInventory(
      graph,
      fixture.line,
      White,
      fixture.root,
      Some(comparison),
      Some(RelativeCauseSourceSide.Reference)
    )
    assert(endpointInventory.observations.exists(observation =>
      observation.scope.targetSignatures.contains("Move:d5d4") &&
        observation.scope.consequenceSignatures.contains("Move:e2e4")
    ))

    val witnesses = EvidenceObjectBinding.comparisonEndpointEvidenceWitnesses(
      RelativeCauseSourceSide.Reference,
      fixture.line,
      fixture.root,
      comparisonRef,
      comparison,
      fixture.baseRecords :+ eventRecord,
      records,
      graph
    )
    val snapshot = ComparisonEndpointEvidenceSnapshot(
      comparisonRef,
      comparison,
      ComparisonEndpointEvidenceSideSnapshot(
        RelativeCauseSourceSide.Reference,
        fixture.line,
        witnesses
      ),
      ComparisonEndpointEvidenceSideSnapshot(
        RelativeCauseSourceSide.Candidate,
        candidateLine,
        Nil
      )
    )
    val moveOrderRecords =
      ComparisonEndpointEffectObservationPolicy.exactInducedResponseMoveOrderRecords(
        snapshot,
        RelativeCauseSourceSide.Reference,
        graph
      )
    assertEquals(moveOrderRecords.map(_.ref.id), List(eventRecord.ref.id))
    val profile = RelativeCauseSignalProfile.from(
      comparison,
      referenceRecords = fixture.baseRecords,
      candidateRecords = Nil,
      sharedRecords = Nil,
      referenceEndpointMoveOrderRecords = moveOrderRecords
    )
    val draft = RelativeCauseDraftPlanner
      .drafts(profile, comparisonRecord)
      .find(_.kind == RelativeCauseKind.WrongMoveOrder)
      .getOrElse(fail("expected a move-order draft"))
    assertEquals(draft.support.map(_.ref.id), List(eventRecord.ref.id))
    assertEquals(draft.sourceSide, Some(RelativeCauseSourceSide.Reference))
    assertEquals(draft.attributionKind, CauseAttributionKind.ReferenceCreatesResource)

    val noResponse = event.copy(
      causalEpisode = event.causalEpisode.copy(responses = Nil)
    )
    assertEquals(
      ComparisonEndpointEffectObservationPolicy.exactInducedResponseMoveOrder(
        comparison,
        RelativeCauseSourceSide.Reference,
        eventRecord.ref,
        noResponse
      ),
      None
    )
    val differentEndpoint = comparison.copy(
      candidateLine = candidateLine.copy(rootMove = "e2e3")
    )
    assertEquals(
      ComparisonEndpointEffectObservationPolicy.exactInducedResponseMoveOrder(
        differentEndpoint,
        RelativeCauseSourceSide.Reference,
        eventRecord.ref,
        event
      ),
      None
    )
    val alternateResponseStep =
      legalStep(event.causalEpisode.root.step.fenAfter, "b5b4", 2)
    val ambiguous = event.copy(
      causalEpisode = event.causalEpisode.copy(
        responses = event.causalEpisode.responses :+
          PlanCausalResponse(
            event.causalEpisode.root,
            alternateResponseStep,
            plyOffset = 1
          )
      )
    )
    assertEquals(
      ComparisonEndpointEffectObservationPolicy.exactInducedResponseMoveOrder(
        comparison,
        RelativeCauseSourceSide.Reference,
        eventRecord.ref,
        ambiguous
      ),
      None
    )

    val candidateFixture = inventoryFixture(
      LineNodeRole.Played,
      inducedMoveOrder = true,
      rootMove = "e2e4"
    )
    val sourceContext = comparisonContext(
      fixture,
      candidateFixture,
      referenceEvents = List(eventRecord)
    )
    val sourceGraph = sourceContext.evidenceGraph
    val cContext = RelativeAssessmentAssembler.enrichCauses(
      RelativeAssessmentAssembler.enrichFacts(sourceContext)
    )
    val (causeRef, exactCause) = cContext.evidenceGraph.records.collect {
      case EvidenceRecord(ref, RelativeCauseFactEvidence(cause), _)
          if cause.kind == RelativeCauseKind.WrongMoveOrder => ref -> cause
    } match
      case exact :: Nil => exact
      case _            => fail("expected one C-generated exact WrongMoveOrder")
    assert(exactCause.directEffectAdmission.productionReady)
    val exactClaim = JudgmentClaimAssembler
      .propose(cContext)
      .find(claim => claim.family == ClaimFamily.Tactical && claim.evidence.contains(causeRef))
      .getOrElse(fail("expected Jp tactical host for C-generated WrongMoveOrder"))
    assertEquals(ClaimTruthPolicy.evaluate(exactClaim, cContext.evidenceGraph).status, ClaimAdmissionStatus.Certified)

    val incompleteCandidateGraph = cContext.evidenceGraph.records
      .filterNot(_.ref.id == candidateFixture.pressureRecord.ref.id)
      .foldLeft(TypedEvidenceGraph.empty)((current, record) => current.add(record))
    assert(RelativeCauseConstructionAdmission.admittedDirectChannels(exactCause, incompleteCandidateGraph).nonEmpty)
    assertEquals(
      ClaimTruthPolicy.evaluate(exactClaim, incompleteCandidateGraph).status,
      ClaimAdmissionStatus.Deferred
    )

    val genericPlanOnly = exactClaim.copy(
      id = s"${exactClaim.id}-generic-plan-only",
      subject = ClaimSubject.PlayedMove,
      primaryLine = Some(candidateFixture.line),
      subjectMove = Some(candidateFixture.line.rootMove),
      evidence = List(candidateFixture.robustEvent.ref, exactCause.comparisonEvidence),
      scope = EvidenceScope.PlayedLine
    )
    assertEquals(
      ClaimTruthPolicy.evaluate(genericPlanOnly, cContext.evidenceGraph.add(candidateFixture.robustEvent)).status,
      ClaimAdmissionStatus.Deferred
    )

    val openingDevelopmentRecord = inventoryFixture(
      LineNodeRole.BestReference,
      inducedMoveOrder = true,
      planKind = PlanKind.OpeningDevelopment,
      resultConsequenceKind = TransitionConsequenceKind.DevelopmentMobilityGain,
      resultLabel = "development-mobility-gain",
      eventRecordIdSuffix = "-opening-development"
    ).robustEvent
    val multiSourceContext = RelativeAssessmentAssembler.enrichCauses(
      RelativeAssessmentAssembler.enrichFacts(
        sourceContext.copy(evidenceGraph = sourceGraph.add(openingDevelopmentRecord))
      )
    )
    val (multiSourceCauseRef, multiSourceCause) = multiSourceContext.evidenceGraph.records.collect {
      case EvidenceRecord(ref, RelativeCauseFactEvidence(cause), _)
          if cause.kind == RelativeCauseKind.WrongMoveOrder => ref -> cause
    } match
      case exact :: Nil => exact
      case _            => fail("expected one two-source C-generated WrongMoveOrder")
    assertEquals(
      multiSourceCause.proof.map(_.directProof.sourceRefs.map(_.id).toSet),
      Some(Set(eventRecord.ref.id, openingDevelopmentRecord.ref.id))
    )
    assertEquals(
      RelativeCauseConstructionAdmission
        .admittedDirectChannels(multiSourceCause, multiSourceContext.evidenceGraph)
        .map(_.binding.source.id)
        .toSet,
      Set(eventRecord.ref.id)
    )
    val multiSourceClaim = JudgmentClaimAssembler
      .propose(multiSourceContext)
      .find(claim => claim.family == ClaimFamily.Tactical && claim.evidence.contains(multiSourceCauseRef))
      .getOrElse(fail("expected Jp tactical host for two-source WrongMoveOrder"))
    assertEquals(
      ClaimTruthPolicy.evaluate(multiSourceClaim, multiSourceContext.evidenceGraph).status,
      ClaimAdmissionStatus.Certified
    )

    def withAddedDirectSource(source: EvidenceRef): RelativeCauseFact =
      exactCause.copy(
        supportEvidence = exactCause.supportEvidence :+ source,
        proof = exactCause.proof.map(proof =>
          proof.copy(directProof = proof.directProof.copy(
            sourceRefs = proof.directProof.sourceRefs :+ source
          ))
        )
      )
    val missingSource = eventRecord.ref.copy(id = s"${eventRecord.ref.id}-missing")
    val malformedCauses = List(
      ("missing-direct-source", withAddedDirectSource(missingSource), None),
      (
        "foreign-direct-source",
        withAddedDirectSource(candidateFixture.robustEvent.ref),
        Some(candidateFixture.robustEvent)
      ),
      ("duplicate-direct-source", exactCause.copy(
        proof = exactCause.proof.map(proof =>
          proof.copy(directProof = proof.directProof.copy(
            sourceRefs = proof.directProof.sourceRefs ++ proof.directProof.sourceRefs.headOption
          ))
        )
      ), None)
    )
    malformedCauses.foreach { case (suffix, malformedCause, foreignRecord) =>
      val malformedCauseRecord = cContext.evidenceGraph
        .record(causeRef)
        .getOrElse(fail("expected C-generated WrongMoveOrder record"))
        .copy(
          ref = causeRef.copy(id = s"${causeRef.id}-$suffix"),
          payload = RelativeCauseFactEvidence(malformedCause)
        )
      val malformedGraph = foreignRecord
        .fold(cContext.evidenceGraph)(record => cContext.evidenceGraph.add(record))
        .add(malformedCauseRecord)
      assert(RelativeCauseConstructionAdmission.admittedDirectChannels(malformedCause, malformedGraph).nonEmpty)
      val malformedClaim = exactClaim.copy(
        id = s"${exactClaim.id}-$suffix",
        evidence = exactClaim.evidence.map(ref =>
          if ref == causeRef then malformedCauseRecord.ref else ref
        )
      )
      assertEquals(
        ClaimTruthPolicy.evaluate(malformedClaim, malformedGraph).status,
        ClaimAdmissionStatus.Deferred
      )
    }

  private final case class RetainedLiabilityFixture(
      root: PositionNodeRef,
      cause: RelativeCauseFact,
      comparison: CandidateComparisonFact,
      graph: TypedEvidenceGraph,
      channel: DirectCauseChannel,
      event: PlanCausalEventEvidence
  )

  private def retainedLiabilityFixture(
      comparisonKind: CandidateComparisonKind = CandidateComparisonKind.PlayedVsBest
  ): RetainedLiabilityFixture =
    val inventory = inventoryFixture(LineNodeRole.Played)
    val eventRecord = inventory.refutedEvent
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
            -300,
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
      kind = RelativeCauseKind.PlanContradiction,
      comparisonEvidence = comparisonRef,
      supportEvidence = List(eventRecord.ref),
      sourceSide = RelativeCauseSourceSide.Candidate,
      attribution = CauseAttribution(
        CauseAttributionKind.CandidateAllowsLiability,
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

  private def comparisonContext(
      reference: InventoryFixture,
      candidate: InventoryFixture,
      referenceEvents: List[EvidenceRecord] = Nil,
      candidateEvents: List[EvidenceRecord] = Nil
  ): JudgmentAssemblyContext =
    assertEquals(candidate.root, reference.root)
    def afterRoot(fixture: InventoryFixture): PositionNodeRef =
      fixture.structuralRecord.payload match
        case StructuralDeltaEvidence(transition, _, _, _) => transition.to
        case _                                            => fail("expected structural transition")
    def lineMoves(fixture: InventoryFixture): List[String] =
      fixture.lineRecord.payload match
        case line: LineFactEvidence => line.lineReplayMoves
        case _                      => fail("expected legal line")
    def transitionEvidence(id: String, scope: EvidenceScope): EvidenceRef =
      EvidenceRef(
        id,
        EvidenceProducer.MoveTransitionProducer,
        EvidenceLayer.MoveTransition,
        reference.root,
        None,
        scope,
        EvidenceConfidence.LegalReplayVerified
      )

    val referenceAfter = afterRoot(reference)
    val candidateAfter = afterRoot(candidate)
    val referenceNode = CandidateLineNode(
      reference.line,
      EngineLine(lineMoves(reference), 300, depth = 18),
      reference.lineRecord.ref
    )
    val candidateNode = CandidateLineNode(
      candidate.line,
      EngineLine(lineMoves(candidate), -300, depth = 18),
      candidate.lineRecord.ref
    )
    val input = NormalizedMoveReviewInput(
      beforeFen = reference.root.fen,
      playedMoveUci = candidate.line.rootMove,
      beforePly = reference.root.ply,
      sideToMove = Some(White),
      afterPlayedFen = candidateAfter.fen,
      afterReferenceFen = Some(referenceAfter.fen),
      lines = List(
        NormalizedCandidateLine(LineNodeRole.BestReference, reference.line.rank, referenceNode.line),
        NormalizedCandidateLine(LineNodeRole.Played, candidate.line.rank, candidateNode.line)
      ),
      opening = None
    )
    val graph = (
      reference.baseRecords ++ referenceEvents ++
        candidate.baseRecords ++ candidateEvents
    ).foldLeft(TypedEvidenceGraph.empty)((current, record) => current.add(record))
    JudgmentAssemblyContext(
      input = input,
      positions = List(
        PositionNode(PositionNodeRole.Before, reference.root),
        PositionNode(PositionNodeRole.AfterPlayed, candidateAfter),
        PositionNode(PositionNodeRole.AfterReference, referenceAfter)
      ),
      lines = List(referenceNode, candidateNode),
      transitions = List(
        MoveTransitionEdge(
          TransitionEdgeRole.Played,
          reference.root,
          candidate.line.rootMove,
          candidateAfter,
          transitionEvidence(
            s"${candidate.line.id}-transition",
            EvidenceScope.PlayedTransition
          )
        ),
        MoveTransitionEdge(
          TransitionEdgeRole.Reference,
          reference.root,
          reference.line.rootMove,
          referenceAfter,
          transitionEvidence(
            s"${reference.line.id}-transition",
            EvidenceScope.ReferenceTransition
          )
        )
      ),
      evidenceGraph = graph,
      claims = Nil
    )

  private val DistinctRootMoveOrderFen =
    "4k3/8/6p1/3P4/4P3/8/5P2/4K3 w - - 0 1"

  private def distinctRootMoveOrderFixture(
      role: LineNodeRole,
      strength: Int,
      consequenceKind: TransitionConsequenceKind = TransitionConsequenceKind.SpaceGain
  ): InventoryFixture =
    val (rootMove, resultMove, induced) = role match
      case LineNodeRole.BestReference => ("f2f4", "e4e5", true)
      case LineNodeRole.Played        => ("e4e5", "d5d6", false)
      case unsupported                => fail(s"unsupported move-order endpoint $unsupported")
    inventoryFixture(
      role,
      inducedMoveOrder = induced,
      rootMove = rootMove,
      planKind = PlanKind.CentralSpaceBind,
      resultConsequenceKind = consequenceKind,
      resultLabel = consequenceKind.toString,
      rootFen = Some(DistinctRootMoveOrderFen),
      replyMove = Some("g6g5"),
      resultMove = Some(resultMove),
      resultStrength = strength,
      resultGoalTargets = List("square:d6"),
      dependencyKind = Some(PlanCausalDependencyKind.PawnAdvanceSupport)
    )

  private def inventoryFixture(
      role: LineNodeRole,
      inducedMoveOrder: Boolean = false,
      rootMove: String = "b1c3",
      planKind: PlanKind = PlanKind.WorstPieceImprovement,
      additionalPlanKinds: List[PlanKind] = Nil,
      resultConsequenceKind: TransitionConsequenceKind = TransitionConsequenceKind.MobilityGain,
      resultLabel: String = "mobility-gain",
      eventRecordIdSuffix: String = "",
      rootFen: Option[String] = None,
      replyMove: Option[String] = None,
      resultMove: Option[String] = None,
      resultStrength: Int = 2,
      resultGoalTargets: List[String] = Nil,
      dependencyKind: Option[PlanCausalDependencyKind] = None
  ): InventoryFixture =
    val id = role.toString.toLowerCase
    val fen = rootFen.getOrElse(
      if inducedMoveOrder then "4k3/8/8/1p1p4/8/8/4P3/1N2K3 w - - 0 1"
      else "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    )
    val rootStep = legalStep(fen, rootMove, 1)
    val replyStep = legalStep(
      rootStep.fenAfter,
      replyMove.getOrElse(if inducedMoveOrder then "d5d4" else "a7a6"),
      2
    )
    val resultStep = legalStep(
      replyStep.fenAfter,
      resultMove.getOrElse(
        if inducedMoveOrder && rootMove == "b1c3" then "e2e4"
        else if rootMove == "e2e4" then "e4e5"
        else "c3d5"
      ),
      3
    )
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
        ActivePlans((planKind :: additionalPlanKinds).distinct.map(kind =>
          PlanMatch(
            plan = Plan(kind, White),
            score = 1.0,
            evidence = Nil
          )
        )),
        alignment = None
      ),
      parents = List(structuralRef)
    )

    def eventRecord(robust: Boolean): EvidenceRecord =
      def actorFor(step: LineReplayStep): RootCausalActor =
        RootCausalActor
          .fromPosition(PositionNodeRef(step.fenBefore, step.ply - 1), step.moveUci)
          .getOrElse(fail(s"expected legal actor for ${step.moveUci}"))
      val rootActor = actorFor(rootStep)
      val resultActor = actorFor(resultStep)
      val resultSubject =
        s"${resultActor.role.name.toLowerCase}:${resultActor.from.key}-${resultActor.to.key}"
      val consequence = TransitionConsequence(
        resultConsequenceKind,
        StructuralSignalPolarity.Gain,
        strength = resultStrength,
        subjects = List(resultSubject),
        targetSubjects = resultGoalTargets
      )
      val rootNode = PlanCausalEventNode(
        PlanEventIdentity(
          rootStep.moveUci,
          planKind,
          Some(rootActor.role.name),
          Some(rootStep.moveUci.take(2)),
          Some(rootStep.moveUci.slice(2, 4)),
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
          planKind,
          Some(resultActor.role.name),
          Some(resultStep.moveUci.take(2)),
          Some(resultStep.moveUci.slice(2, 4)),
          List(resultSubject),
          List(resultLabel)
        ),
        resultStep,
        White,
        List(consequence),
        Nil
      )
      val dependency = dependencyKind.getOrElse(
        if inducedMoveOrder && rootMove == "b1c3" then
          PlanCausalDependencyKind.PawnAdvanceSupport
        else PlanCausalDependencyKind.ObjectStatePrecondition
      ) match
        case PlanCausalDependencyKind.PawnAdvanceSupport =>
          val trajectory = PawnAdvanceSupportTrajectory
            .find(rootStep, resultStep, List(replyStep))
            .getOrElse(fail("expected exact pawn-advance support"))
          PlanCausalEventDependency(
            rootNode,
            resultNode,
            PlanCausalDependencyKind.PawnAdvanceSupport,
            PlanCausalDependencyProof.PawnAdvanceSupport(trajectory),
            plyOffset = 2
          )
        case PlanCausalDependencyKind.ObjectStatePrecondition =>
          val trajectory = LineObjectTrajectory
            .find(rootStep, List(replyStep, resultStep), maxPlyOffset = 2)
            .getOrElse(fail("expected the root knight trajectory"))
          PlanCausalEventDependency(
            rootNode,
            resultNode,
            PlanCausalDependencyKind.ObjectStatePrecondition,
            PlanCausalDependencyProof.ObjectState(trajectory),
            plyOffset = 2
          )
        case unsupported =>
          fail(s"unsupported inventory dependency $unsupported")
      val episode = PlanCausalEpisode(
        rootNode,
        List(resultNode),
        List(dependency),
        Option
          .when(inducedMoveOrder)(
            PlanCausalResponse(rootNode, replyStep, plyOffset = 1)
          )
          .toList
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
          s"plan-inventory-$id-$suffix-event$eventRecordIdSuffix",
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
