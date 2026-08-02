package lila.chessjudgment.analysis.assembly

import chess.{ Black, Queen, White }
import lila.chessjudgment.analysis.policy.{ ClaimAdmissionStatus, ClaimTruthPolicy }
import lila.chessjudgment.model.Motif
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.PrincipalVariationEvidence
import lila.chessjudgment.model.strategic.EngineLine

class RelativeCauseGenerationOwnershipTest extends munit.FunSuite:

  private val queenFen = "4k3/8/8/8/8/8/8/3QK3 w - - 0 1"
  private val queenPosition = PositionNodeRef(queenFen, 0, Some(White))
  private val queenReference = LineNodeRef("queen-reference", "d1h5", 1, LineNodeRole.BestReference)
  private val queenCandidate = LineNodeRef("queen-played", "d1d2", 2, LineNodeRole.Played)
  private val queenFact = comparison(queenPosition, queenReference, queenCandidate)
  private val queenBinding = binding(queenReference)

  test("exact direct pawn blockades remain endpoint-owned resources"):
    val raw = RawMoveReviewInput(
      fen = "8/5ppp/8/5P1P/2k3P1/2p5/5P2/2K5 b - - 0 1",
      playedMoveUci = "h7h6",
      variations = List(
        EngineLine(List("f7f6", "h5h6", "g7h6", "c1d1", "c4d5", "d1c2", "d5e4", "g4g5", "h6g5", "c2c3", "g5g4", "c3c2", "e4f4", "c2c3"), -534, depth = 20),
        EngineLine(List("h7h6", "f5f6", "g7f6", "f2f4", "c4d4", "g4g5", "h6g5", "f4g5", "f6f5", "h5h6", "f5f4", "h6h7", "f4f3", "h7h8q", "d4e3", "h8h5", "c3c2", "c1c2", "e3f4", "h5h4", "f4f5"), 682, depth = 20)
      )
    )
    val execution = MoveReviewJudgmentOrchestrator
      .execute(raw, JudgmentBoundaryIntervention.identity)
      .getOrElse(fail("expected a complete cached-line execution"))
    val graph = execution.c.evidenceGraph
    val rootPlanContent = JudgmentClaimAssembler.propose(execution.c).find { claim =>
      claim.content.collect { case JudgmentClaimContent.StrategicMechanism(carrier) =>
        carrier.position == execution.c.playedTransition.map(_.from).getOrElse(carrier.position) &&
          graph.record(carrier).exists {
            case EvidenceRecord(_, mechanism: StrategicMechanismEvidence, _) =>
              mechanism.signals.exists(signal => graph.record(signal.source).exists(_.payload.isInstanceOf[PlanCausalEventEvidence]))
            case _ => false
          }
      }.getOrElse(false)
    }.getOrElse(fail("expected a root PlanCausalEvent strategic content claim"))
    assertEquals(ClaimTruthPolicy.evaluate(rootPlanContent, execution.c).status, ClaimAdmissionStatus.Certified)
    val causes = graph.records.collect {
      case EvidenceRecord(ref, RelativeCauseFactEvidence(cause), _) => ref -> cause
    }
    val snapshotsByComparisonId = RelativeAssessmentAssembler
      .comparisonEndpointEvidenceSnapshots(execution.f)
      .map(snapshot => snapshot.comparisonEvidence.id -> snapshot)
      .toMap
    val packet = execution.packet.getOrElse(fail("expected a disposition-closed packet"))
    val ledger = execution.r.causeDispositionLedger
    assertEquals(packet.causeDispositionLedger, ledger)
    assertEquals(
      ledger.byCauseEvidenceId.keySet,
      causes.map(_._1.id).toSet
    )
    assertEquals(
      ledger.selectedCauseEvidenceIds,
      execution.r.causeExposureResolution.selectedCauseEvidenceIds
    )
    val incompleteLedger = ledger.copy(dispositions = ledger.dispositions.drop(1))
    assertEquals(
      EvidenceBackedJudgmentPacket.fromAssembly(
        execution.context,
        packet.probeRequests,
        execution.r.playerFacingClaimDecisions,
        execution.r.onlyMoveConstraintResolutions,
        execution.r.causeExposureResolution,
        incompleteLedger,
        execution.r.selectedContentClaimIds
      ),
      None
    )
    val selectedDisposition = ledger.dispositions
      .find(_.status == CauseDispositionStatus.Selected)
      .getOrElse(fail("expected a selected Cause disposition"))
    val inconsistentLedger = ledger.copy(
      dispositions = ledger.dispositions.map { disposition =>
        if disposition.causeEvidence.id == selectedDisposition.causeEvidence.id then
          disposition.copy(
            status = CauseDispositionStatus.Diagnostic,
            reason = CauseDispositionReason.DiagnosticComparison,
            selectedOwnerClaimId = None
          )
        else disposition
      }
    )
    assertEquals(
      EvidenceBackedJudgmentPacket.fromAssembly(
        execution.context,
        packet.probeRequests,
        execution.r.playerFacingClaimDecisions,
        execution.r.onlyMoveConstraintResolutions,
        execution.r.causeExposureResolution,
        inconsistentLedger,
        execution.r.selectedContentClaimIds
      ),
      None
    )
    def blockadeCause(
        side: RelativeCauseSourceSide,
        attribution: CauseAttributionKind,
        restrictedFrom: String,
        restrictedTo: String
    ): (EvidenceRef, RelativeCauseFact, List[DirectCauseChannel]) =
      val (ref, cause) = causes.find { case (_, candidate) =>
        candidate.kind == RelativeCauseKind.OpponentRestriction &&
          candidate.sourceSide == side &&
          candidate.attribution.kind == attribution
      }.getOrElse(fail(s"missing $side direct pawn-blockade Cause"))
      val channels = RelativeCauseConstructionAdmission.admittedDirectChannels(cause, graph)
      assert(channels.nonEmpty)
      assert(cause.directEffectAdmission.productionReady)
      assertEquals(graph.comparisonFor(cause).map(_.kind), Some(CandidateComparisonKind.PlayedVsBest))
      val snapshot = snapshotsByComparisonId.getOrElse(
        cause.comparisonEvidence.id,
        fail(s"missing neutral snapshot for ${cause.comparisonEvidence.id}")
      )
      assert(
        channels.exists(channel =>
          ComparisonEndpointEffectObservationPolicy
            .uniqueNeutralWitnessFor(snapshot, cause.sourceSide, channel, graph)
            .nonEmpty
        )
      )
      channels.foreach(channel =>
        val targets = channel.binding.target.map(_.signaturePart).toSet
        assertEquals(channel.directChange, DirectCausalChange.Prevented)
        assert(targets(s"${EvidenceObjectKind.Piece}:pawn"))
        assert(targets(s"${EvidenceObjectKind.Square}:$restrictedFrom"))
        assert(targets(s"${EvidenceObjectKind.Square}:$restrictedTo"))
        val proof = channel.rootOwnedProof.getOrElse(fail("expected an owned structural proof"))
        val segment = DirectCauseProofSegment.from(proof).getOrElse(fail("expected an exact root proof segment"))
        assertEquals(segment.steps.map(_.moveUci), List(channel.binding.line.get.rootMove))
        def structuralPrimitive(value: RootOwnedEffectProof): Option[(StructuralDeltaEvidence, TransitionConsequence)] =
          value match
            case RootOwnedEffectProof.StructuralTransition(_, delta, consequence) => Some(delta -> consequence)
            case RootOwnedEffectProof.StrategicAxis(primitive, _, _)               => structuralPrimitive(primitive)
            case _                                                                 => None
        val (delta, consequence) = structuralPrimitive(proof).getOrElse(fail("expected a structural primitive"))
        assertEquals(delta.moveUci, channel.binding.line.get.rootMove)
        assert(
          StructuralDeltaEvidence
            .exactRootOccupiedPawnAdvanceRestrictions(delta, consequence)
            .contains(s"pawn:$restrictedFrom-$restrictedTo:advance-restricted")
        )
      )
      val expectedAxisLabel = s"direct-pawn-advance-block:$restrictedFrom-$restrictedTo"
      val (carriedChannel, carrier, signal) = channels.flatMap { channel =>
        graph.record(channel.binding.source).toList.collect {
          case EvidenceRecord(_, mechanism: StrategicMechanismEvidence, _) =>
            mechanism.signals
              .find(_.axis.exists(axis =>
                axis.kind == StrategicAxisKind.Counterplay &&
                  axis.polarity == StrategicAxisPolarity.Restrain &&
                  axis.label == expectedAxisLabel
              ))
              .map(signal => (channel, mechanism, signal))
        }.flatten
      }.headOption.getOrElse(fail("expected a direct PlanPressure carrier"))
      val eventRecord = graph.record(signal.source).collect {
        case record @ EvidenceRecord(_, event: PlanCausalEventEvidence, _) => record -> event
      }.getOrElse(fail("expected the carrier to own an exact plan event"))
      val (eventEvidence, event) = eventRecord
      val primitives = DirectOpponentRestrictionProof
        .exactRootPawnBlockadePrimitives(eventEvidence.ref, event, graph)
      assert(primitives.nonEmpty)
      val (structuralRef, _, lineRef, line, _) = primitives.head
      val directParentIds = eventEvidence.parents.map(_.id).toSet
      assert(directParentIds(structuralRef.id))
      assert(directParentIds(lineRef.id))
      assertEquals(line.rootActorSurvivesReply, Some(true))
      val provenanceIds = carriedChannel.binding.provenance.map(_.id).toSet
      assert(provenanceIds(eventEvidence.ref.id))
      assert(provenanceIds(structuralRef.id))
      assert(provenanceIds(lineRef.id))
      assert(carrier.signals.exists(_.source.id == eventEvidence.ref.id))
      (ref, cause, channels)

    val (_, referenceCause, referenceChannels) = blockadeCause(
      RelativeCauseSourceSide.Reference,
      CauseAttributionKind.ReferenceCreatesResource,
      restrictedFrom = "f5",
      restrictedTo = "f6"
    )
    val (_, candidateCause, candidateChannels) = blockadeCause(
      RelativeCauseSourceSide.Candidate,
      CauseAttributionKind.CandidateCreatesValue,
      restrictedFrom = "h5",
      restrictedTo = "h6"
    )
    assert(referenceChannels.forall(_.binding.line.exists(_.rootMove == "f7f6")))
    assert(candidateChannels.forall(_.binding.line.exists(_.rootMove == "h7h6")))
    assert(!causes.exists { case (_, cause) =>
      cause.kind == RelativeCauseKind.OpponentRestriction &&
        cause.attribution.kind == CauseAttributionKind.CandidateAllowsLiability
    })
    assertEquals(referenceCause.sourceSide, RelativeCauseSourceSide.Reference)
    assertEquals(candidateCause.sourceSide, RelativeCauseSourceSide.Candidate)

  test("direct pawn restriction authority rejects illegal and non-surviving blockades"):
    def restrictionDelta(fen: String, move: String, line: LineNodeRef): (StructuralDeltaEvidence, TransitionConsequence) =
      val afterFen = PrincipalVariationEvidence
        .legalFenAfter(fen, move)
        .getOrElse(fail(s"expected legal root move $move"))
      val consequence = TransitionConsequence(
        kind = TransitionConsequenceKind.OpponentMobilityRestriction,
        polarity = StructuralSignalPolarity.Gain,
        strength = 2,
        subjects = List("pawn:f5-f6:advance-restricted")
      )
      StructuralDeltaEvidence(
        transition = StructuralTransitionBinding(
          moveUci = move,
          role = TransitionEdgeRole.Reference,
          from = PositionNodeRef(fen, 0, Some(Black)),
          to = PositionNodeRef(afterFen, 1, Some(White)),
          line = Some(line),
          perspective = Black
        ),
        signals = Nil,
        consequences = List(consequence)
      ) -> consequence

    val pinnedFen = "2k5/5p2/8/r4P1K/8/8/8/8 b - - 0 1"
    val pinnedLine = LineNodeRef("pinned-pawn-reference", "f7f6", 1, LineNodeRole.BestReference)
    val (pinnedDelta, pinnedConsequence) = restrictionDelta(pinnedFen, "f7f6", pinnedLine)
    assertEquals(
      StructuralDeltaEvidence.exactRootOccupiedPawnAdvanceRestrictions(pinnedDelta, pinnedConsequence),
      Nil
    )
    assert(!DirectOpponentRestrictionProof.directRestrictionSurvivesReply(pinnedLine, pinnedDelta, None))
    assert(!StrategicMechanismEvidence
      .structuralAxesForConsequence(pinnedDelta, pinnedConsequence)
      .exists(_.label.startsWith("direct-pawn-advance-block:")))

    val capturedFen = "2k5/5p2/8/4PP2/8/8/8/2K5 b - - 0 1"
    val capturedLine = LineNodeRef("captured-blocker-reference", "f7f6", 1, LineNodeRole.BestReference)
    val (capturedDelta, capturedConsequence) = restrictionDelta(capturedFen, "f7f6", capturedLine)
    val afterRoot = capturedDelta.transition.to.fen
    val afterReply = PrincipalVariationEvidence
      .legalFenAfter(afterRoot, "e5f6")
      .getOrElse(fail("expected the reply to capture the blocker"))
    val capturedLineFact = LineFactEvidence(
      line = capturedLine,
      replay = List(
        LineReplayStep(1, "f7f6", capturedFen, afterRoot),
        LineReplayStep(2, "e5f6", afterRoot, afterReply)
      )
    )
    assert(
      StructuralDeltaEvidence
        .exactRootOccupiedPawnAdvanceRestrictions(capturedDelta, capturedConsequence)
        .nonEmpty
    )
    assertEquals(capturedLineFact.rootActorSurvivesReply, Some(false))
    assert(!DirectOpponentRestrictionProof
      .directRestrictionSurvivesReply(capturedLine, capturedDelta, Some(capturedLineFact)))

  test("C retains a truthful specific cause and its generic companion on the same channel"):
    val explicitRelation = mateRelation("draft-mate-relation", queenReference)
    val mateEval = evalRecord("draft-mate-eval", queenPosition, queenReference)
    val mechanism = kingForcingMechanism(
      "draft-king-forcing",
      queenPosition,
      queenReference,
      mateEval.ref,
      explicitRelation.ref
    )
    val profile = RelativeCauseSignalProfile.from(
      fact = queenFact,
      referenceRecords = List(explicitRelation, mateEval, mechanism),
      candidateRecords = Nil,
      sharedRecords = Nil
    )
    val comparisonRecord =
      ExplicitCauseAdmissionTestSupport.comparisonRecord("draft-comparison", queenPosition, queenFact)
    val drafts = RelativeCauseDraftPlanner.drafts(profile, comparisonRecord)
    val retained = drafts.filter(draft =>
      Set(RelativeCauseKind.KingForcing, RelativeCauseKind.MissedTacticalResource)(draft.kind) &&
        draft.sourceSide.contains(RelativeCauseSourceSide.Reference) &&
        draft.attributionKind == CauseAttributionKind.ReferenceCreatesResource
    )

    assert(retained.exists(_.kind == RelativeCauseKind.KingForcing))
    assert(retained.exists(_.kind == RelativeCauseKind.MissedTacticalResource))

  test("direct Relation proof comes only from support closure or an explicit owned signal"):
    val explicitRelation = mateRelation("explicit-relation", queenReference)
    val siblingRelation = mateRelation("unrelated-sibling-relation", queenReference)
    val mateEval = evalRecord("explicit-mate-eval", queenPosition, queenReference)
    val mechanism = kingForcingMechanism(
      "explicit-carrier",
      queenPosition,
      queenReference,
      mateEval.ref,
      explicitRelation.ref
    )
    val graph = graphOf(mechanism, mateEval, explicitRelation, siblingRelation)
    val ids = directIds(
      graph,
      queenFact,
      RelativeCauseKind.KingForcing,
      queenBinding,
      List(mechanism)
    )

    assert(ids(mechanism.ref.id))
    assert(ids(explicitRelation.ref.id))
    assert(!ids(siblingRelation.ref.id))

  test("Relation direct ownership requires exact event line, root move, and root geometry"):
    val exact = hangingRelation(
      "exact-relation",
      queenReference,
      List(queenReference.rootMove),
      attackerSquare = "h5",
      targetSquare = "e8"
    )
    val siblingLine = queenReference.copy(id = "sibling-line", rank = 9)
    val wrongLine = hangingRelation(
      "wrong-line-relation",
      siblingLine,
      List(queenReference.rootMove),
      attackerSquare = "h5",
      targetSquare = "e8"
    )
    val laterOnly = hangingRelation(
      "later-only-relation",
      queenReference,
      List("e8e7", "e7e6"),
      attackerSquare = "h5",
      targetSquare = "e8"
    )
    val unrelatedGeometry = hangingRelation(
      "unrelated-geometry-relation",
      queenReference,
      List(queenReference.rootMove),
      attackerSquare = "a8",
      targetSquare = "e8"
    )
    val graph = graphOf(exact, wrongLine, laterOnly, unrelatedGeometry)

    assertEquals(
      directIds(graph, queenFact, RelativeCauseKind.MissedTacticalResource, queenBinding, List(exact)),
      Set(exact.ref.id)
    )
    List(wrongLine, laterOnly, unrelatedGeometry).foreach(record =>
      assertEquals(
        directIds(
          graph,
          queenFact,
          RelativeCauseKind.MissedTacticalResource,
          queenBinding,
          List(record)
        ),
        Set.empty[String]
      )
    )

  test("Threat direct ownership distinguishes exact defense from a root-created mate threat"):
    val defenseFen = "4k3/8/8/8/7q/8/6P1/4K3 w - - 0 1"
    val defensePosition = PositionNodeRef(defenseFen, 0, Some(White))
    val defenseLine = LineNodeRef("defense-reference", "g2g3", 1, LineNodeRole.BestReference)
    val defenseCandidate = LineNodeRef("defense-played", "e1f1", 2, LineNodeRole.Played)
    val defenseFact = comparison(defensePosition, defenseLine, defenseCandidate)
    val defenseBinding = binding(defenseLine)
    val exactDefense = threatRecord(
      "exact-defense",
      defensePosition,
      defenseLine,
      threatActor = Black,
      motifMove = "d8h4",
      motifPly = 0,
      bestDefense = Some(defenseLine.rootMove),
      defenseCount = 1
    )
    val wrongDefense = exactDefense.copy(
      ref = exactDefense.ref.copy(id = "wrong-defense"),
      payload = threatPayload(
        threatActor = Black,
        motifMove = "d8h4",
        motifPly = 0,
        bestDefense = Some("e1f1"),
        defenseCount = 1
      )
    )
    val wrongSide = exactDefense.copy(
      ref = exactDefense.ref.copy(id = "wrong-side-defense"),
      payload = threatPayload(
        threatActor = White,
        motifMove = defenseLine.rootMove,
        motifPly = 0,
        bestDefense = Some(defenseLine.rootMove),
        defenseCount = 1
      )
    )
    val defenseGraph = graphOf(exactDefense, wrongDefense, wrongSide)

    assertEquals(
      directIds(
        defenseGraph,
        defenseFact,
        RelativeCauseKind.OnlyDefenseNecessity,
        defenseBinding,
        List(exactDefense)
      ),
      Set(exactDefense.ref.id)
    )
    List(wrongDefense, wrongSide).foreach(record =>
      assertEquals(
        directIds(
          defenseGraph,
          defenseFact,
          RelativeCauseKind.OnlyDefenseNecessity,
          defenseBinding,
          List(record)
        ),
        Set.empty[String]
      )
    )
    val exactBestDefense = exactDefense.copy(
      ref = exactDefense.ref.copy(id = "exact-best-defense"),
      payload = threatPayload(
        threatActor = Black,
        motifMove = "d8h4",
        motifPly = 0,
        bestDefense = Some(defenseLine.rootMove),
        defenseCount = 2
      )
    )
    val wrongBestDefense = exactBestDefense.copy(
      ref = exactBestDefense.ref.copy(id = "wrong-best-defense"),
      payload = threatPayload(
        threatActor = Black,
        motifMove = "d8h4",
        motifPly = 0,
        bestDefense = Some("e1f1"),
        defenseCount = 2
      )
    )
    val bestDefenseGraph = graphOf(exactBestDefense, wrongBestDefense, wrongSide)
    assertEquals(
      directIds(
        bestDefenseGraph,
        defenseFact,
        RelativeCauseKind.DefensiveResource,
        defenseBinding,
        List(exactBestDefense)
      ),
      Set(exactBestDefense.ref.id)
    )
    List(wrongBestDefense, wrongSide).foreach(record =>
      assertEquals(
        directIds(
          bestDefenseGraph,
          defenseFact,
          RelativeCauseKind.DefensiveResource,
          defenseBinding,
          List(record)
        ),
        Set.empty[String]
      )
    )

    val rootThreat = threatRecord(
      "root-created-mate",
      queenPosition,
      queenReference,
      threatActor = White,
      motifMove = queenReference.rootMove,
      motifPly = 0,
      bestDefense = None,
      defenseCount = 0
    )
    val laterThreat = rootThreat.copy(
      ref = rootThreat.ref.copy(id = "later-mate"),
      payload = threatPayload(
        threatActor = White,
        motifMove = queenReference.rootMove,
        motifPly = 1,
        bestDefense = None,
        defenseCount = 0
      )
    )
    val wrongMoveThreat = rootThreat.copy(
      ref = rootThreat.ref.copy(id = "wrong-move-mate"),
      payload = threatPayload(
        threatActor = White,
        motifMove = "d1d2",
        motifPly = 0,
        bestDefense = None,
        defenseCount = 0
      )
    )
    val threatGraph = graphOf(rootThreat, laterThreat, wrongMoveThreat)
    assertEquals(
      directIds(
        threatGraph,
        queenFact,
        RelativeCauseKind.KingForcing,
        queenBinding,
        List(rootThreat)
      ),
      Set(rootThreat.ref.id)
    )
    List(laterThreat, wrongMoveThreat).foreach(record =>
      assertEquals(
        directIds(
          threatGraph,
          queenFact,
          RelativeCauseKind.KingForcing,
          queenBinding,
          List(record)
        ),
        Set.empty[String]
      )
    )
  test("Tactical mechanism requires its own exact root move and an owned primitive source"):
    val explicitRelation = mateRelation("mechanism-relation", queenReference)
    val mateEval = evalRecord("mechanism-mate-eval", queenPosition, queenReference)
    val exact = kingForcingMechanism(
      "exact-mechanism",
      queenPosition,
      queenReference,
      mateEval.ref,
      explicitRelation.ref
    )
    val wrongMove = exact.copy(
      ref = exact.ref.copy(id = "wrong-root-mechanism"),
      payload = exact.payload match
        case mechanism: TacticalMechanismEvidence => mechanism.copy(moveUci = Some("d1d2"))
        case _                                    => fail("expected tactical mechanism")
    )
    val orphanSource = mateEval.ref.copy(id = "missing-mate-source")
    val noOwnedPrimitive = exact.copy(
      ref = exact.ref.copy(id = "unowned-source-mechanism"),
      payload = TacticalMechanismEvidence(
        TacticalMechanismKind.KingForcing,
        Some(queenReference.rootMove),
        Some(queenReference),
        List(TacticalMechanismSignal(
          TacticalMechanismSignalKind.MateBranch,
          "unowned-mate-branch",
          EvidenceLayer.Eval,
          Some(orphanSource)
        ))
      )
    )
    val graph = graphOf(exact, wrongMove, noOwnedPrimitive, mateEval, explicitRelation)

    assert(directIds(graph, queenFact, RelativeCauseKind.KingForcing, queenBinding, List(exact))(exact.ref.id))
    List(wrongMove, noOwnedPrimitive).foreach(record =>
      assertEquals(
        directIds(graph, queenFact, RelativeCauseKind.KingForcing, queenBinding, List(record)),
        Set.empty[String]
      )
    )

  test("PlanContradiction rejects a positive structural delta without an owned refuted plan event"):
    val candidateAfter = PositionNodeRef(
      PrincipalVariationEvidence
        .legalFenAfter(queenPosition.fen, queenCandidate.rootMove)
        .getOrElse(fail("expected legal candidate root move")),
      1,
      Some(Black)
    )
    val positivePayload = StructuralDeltaEvidence(
      StructuralTransitionBinding(
        queenCandidate.rootMove,
        TransitionEdgeRole.Played,
        queenPosition,
        candidateAfter,
        Some(queenCandidate),
        White
      ),
      signals = Nil,
      consequences = List(TransitionConsequence(
        TransitionConsequenceKind.TargetPressureGain,
        StructuralSignalPolarity.Gain,
        strength = 3,
        subjects = List("king:e8")
      ))
    )
    val positiveStructural = EvidenceRecord(
      evidenceRef(
        "positive-structural-plan-label",
        EvidenceProducer.StructuralDeltaProducer,
        EvidenceLayer.StructuralDelta,
        queenPosition,
        queenCandidate
      ),
      positivePayload
    )
    val candidateBinding = RelativeCauseBinding(
      RelativeCauseRole.PrimaryPlayedCause,
      RelativeCauseSourceSide.Candidate,
      queenCandidate,
      List(queenCandidate),
      RelativeCauseBindingTier.Primary
    )

    assertEquals(
      RelativeCauseKind.structuralConsequences(
        RelativeCauseKind.PlanContradiction,
        positivePayload
      ),
      Nil
    )
    assertEquals(
      directIds(
        graphOf(positiveStructural),
        queenFact,
        RelativeCauseKind.PlanContradiction,
        candidateBinding,
        List(positiveStructural),
        CauseAttributionKind.CandidateAllowsLiability
      ),
      Set.empty[String]
    )

  private def directIds(
      graph: TypedEvidenceGraph,
      fact: CandidateComparisonFact,
      kind: RelativeCauseKind,
      causeBinding: RelativeCauseBinding,
      support: List[EvidenceRecord],
      attributionKind: CauseAttributionKind = CauseAttributionKind.ReferenceCreatesResource
  ): Set[String] =
    RelativeAssessmentAssembler
      .ownedCauseDirectProofRecords(
        graph,
        fact,
        kind,
        causeBinding,
        attributionKind,
        support
      )
      .map(_.ref.id)
      .toSet

  private def comparison(
      position: PositionNodeRef,
      reference: LineNodeRef,
      candidate: LineNodeRef
  ): CandidateComparisonFact =
    CandidateComparisonFact(
      CandidateComparisonKind.PlayedVsBest,
      reference,
      candidate,
      EvalComparison.fromLines(
        White,
        CandidateLineNode(
          reference,
          EngineLine(List(reference.rootMove), scoreCp = 500, depth = 20),
          evidenceRef(
            s"${reference.id}-eval",
            EvidenceProducer.EngineEvalProducer,
            EvidenceLayer.Eval,
            position,
            reference,
            EvidenceConfidence.EngineBacked
          )
        ),
        CandidateLineNode(
          candidate,
          EngineLine(List(candidate.rootMove), scoreCp = -500, depth = 20),
          evidenceRef(
            s"${candidate.id}-eval",
            EvidenceProducer.EngineEvalProducer,
            EvidenceLayer.Eval,
            position,
            candidate,
            EvidenceConfidence.EngineBacked
          )
        )
      )
    )

  private def binding(line: LineNodeRef): RelativeCauseBinding =
    RelativeCauseBinding(
      RelativeCauseRole.PrimaryPlayedCause,
      RelativeCauseSourceSide.Reference,
      line,
      List(line),
      RelativeCauseBindingTier.Primary
    )

  private def mateRelation(id: String, line: LineNodeRef): EvidenceRecord =
    val detail = RelationWitnessDetail.MatePattern(
      "mate_net",
      EvidenceSquare("e8"),
      List(EvidenceSquare("h5")),
      line.rootMove,
      Some("root-mate-net")
    )
    EvidenceRecord(
      evidenceRef(
        id,
        EvidenceProducer.TacticalRelationProducer,
        EvidenceLayer.Relation,
        queenPosition,
        line
      ),
      RelationFactEvidence.from(detail, List(line.rootMove)).getOrElse(fail("expected mate relation"))
    )

  private def hangingRelation(
      id: String,
      line: LineNodeRef,
      lineMoves: List[String],
      attackerSquare: String,
      targetSquare: String
  ): EvidenceRecord =
    val detail = RelationWitnessDetail.HangingPiece(
      EvidenceSquare(attackerSquare),
      EvidenceSquare(targetSquare),
      EvidencePieceRole(Queen.name),
      EvidencePieceRole("king")
    )
    EvidenceRecord(
      evidenceRef(
        id,
        EvidenceProducer.TacticalRelationProducer,
        EvidenceLayer.Relation,
        queenPosition,
        line
      ),
      RelationFactEvidence.from(detail, lineMoves).getOrElse(fail("expected hanging relation"))
    )

  private def evalRecord(
      id: String,
      position: PositionNodeRef,
      line: LineNodeRef
  ): EvidenceRecord =
    EvidenceRecord(
      evidenceRef(
        id,
        EvidenceProducer.EngineEvalProducer,
        EvidenceLayer.Eval,
        position,
        line,
        EvidenceConfidence.EngineBacked
      ),
      EvalFactEvidence(line, whitePovEvalCp = 900, mate = Some(1), depth = 20)
    )

  private def kingForcingMechanism(
      id: String,
      position: PositionNodeRef,
      line: LineNodeRef,
      evalSource: EvidenceRef,
      relationSource: EvidenceRef
  ): EvidenceRecord =
    EvidenceRecord(
      evidenceRef(
        id,
        EvidenceProducer.TacticalMechanismProducer,
        EvidenceLayer.TacticalMechanism,
        position,
        line
      ),
      TacticalMechanismEvidence(
        TacticalMechanismKind.KingForcing,
        Some(line.rootMove),
        Some(line),
        List(
          TacticalMechanismSignal(
            TacticalMechanismSignalKind.MateBranch,
            "mate-branch",
            EvidenceLayer.Eval,
            Some(evalSource)
          ),
          TacticalMechanismSignal(
            TacticalMechanismSignalKind.Relation,
            "mate-net",
            EvidenceLayer.Relation,
            Some(relationSource),
            Some(RelationFactKind.MateNet)
          )
        )
      )
    )

  private def threatRecord(
      id: String,
      position: PositionNodeRef,
      line: LineNodeRef,
      threatActor: chess.Color,
      motifMove: String,
      motifPly: Int,
      bestDefense: Option[String],
      defenseCount: Int
  ): EvidenceRecord =
    EvidenceRecord(
      evidenceRef(
        id,
        EvidenceProducer.ThreatPressureProducer,
        EvidenceLayer.ThreatPressure,
        position,
        line
      ),
      threatPayload(threatActor, motifMove, motifPly, bestDefense, defenseCount)
    )

  private def threatPayload(
      threatActor: chess.Color,
      motifMove: String,
      motifPly: Int,
      bestDefense: Option[String],
      defenseCount: Int
  ): ThreatEpisodeEvidence =
    val target = chess.Square.fromKey("e8").getOrElse(fail("expected e8"))
    val threat = Threat(
      threatActor,
      ThreatKind.Mate,
      turnsToImpact = 1,
      motifs = List(Motif.Check(
        Queen,
        target,
        Motif.CheckType.Mate,
        threatActor,
        motifPly,
        Some(motifMove)
      )),
      attackSquares = List("e8"),
      targetPieces = List("king"),
      bestDefense = bestDefense,
      defenseCount = defenseCount
    )
    ThreatEpisodeEvidence(ThreatEpisode.fromThreat(threat, 0))

  private def evidenceRef(
      id: String,
      producer: EvidenceProducer,
      layer: EvidenceLayer,
      position: PositionNodeRef,
      line: LineNodeRef,
      confidence: EvidenceConfidence = EvidenceConfidence.LegalReplayVerified
  ): EvidenceRef =
    EvidenceRef(id, producer, layer, position, Some(line), line.role.scope, confidence)

  private def graphOf(records: EvidenceRecord*): TypedEvidenceGraph =
    records.foldLeft(TypedEvidenceGraph.empty)((graph, record) => graph.add(record))
