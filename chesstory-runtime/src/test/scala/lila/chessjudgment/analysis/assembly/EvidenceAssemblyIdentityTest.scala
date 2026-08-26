package lila.chessjudgment.analysis.assembly

import chess.{ Black, White }
import lila.chessjudgment.analysis.policy.{ ClaimAdmissionDecision, ClaimAdmissionStatus }
import lila.chessjudgment.model.ProbeObjective
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.{ CandidateLineEvaluation, CanonicalPositionHistory, PrincipalVariationEvidence }
import lila.chessjudgment.model.strategic.EngineLine

class EvidenceAssemblyIdentityTest extends munit.FunSuite:

  private val fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
  private val position = PositionNodeRef(fen, 0, Some(White), Some("identity-root"))
  private val line = LineNodeRef("identity-line", "e2e4", 1, LineNodeRole.Played)

  test("same tactical kind on different proof carriers remains separate"):
    val first = mateEvaluation("mate-route-a", mate = 2)
    val second = mateEvaluation("mate-route-b", mate = 3)
    def assembled(records: List[EvidenceRecord]): List[EvidenceRecord] =
      val context = JudgmentAssemblyContext(
        input = input,
        positions = Nil,
        lines = List(CandidateLineNode(line, candidateEvaluation(first), first.ref)),
        transitions = Nil,
        evidenceGraph = TypedEvidenceGraph.empty.addAll(records),
        claims = Nil
      )
      EvidenceFactAssembler.enrich(context).evidenceGraph.records.collect {
        case record @ EvidenceRecord(_, payload: TacticalMechanismEvidence, _)
            if payload.kind == TacticalMechanismKind.KingForcing =>
          record
      }

    val mechanisms = assembled(List(first, second))

    assertEquals(mechanisms.size, 2)
    assertEquals(assembled(List(second, first)).map(_.ref.id), mechanisms.map(_.ref.id))
    assertEquals(
      mechanisms.map(_.parents.map(_.id).toSet).toSet,
      Set(Set(first.ref.id), Set(second.ref.id))
    )
    assert(mechanisms.forall {
      case EvidenceRecord(_, payload: TacticalMechanismEvidence, _) => payload.signals.size == 1
      case _                                                        => false
    })

  test("same strategic kind preserves exact anchors and carrier routes"):
    val assembled = strategicAssembly()
    val mechanisms = strategicMechanisms(assembled)

    assertEquals(mechanisms.size, 3)
    assertEquals(
      strategicMechanisms(strategicAssembly(reverseSources = true)).map(_.ref.id),
      mechanisms.map(_.ref.id)
    )
    assertEquals(
      mechanisms.map(_.parents.map(_.id).toSet).toSet,
      Set(Set("center-route-a"), Set("center-route-b"), Set("center-route-c"))
    )
    val byParent = mechanisms.map(record => record.parents.head.id -> record).toMap
    val anchorsA = strategicPayload(byParent("center-route-a")).semanticGroupingAnchors.map(_.stableKey).toSet
    val anchorsB = strategicPayload(byParent("center-route-b")).semanticGroupingAnchors.map(_.stableKey).toSet
    val anchorsC = strategicPayload(byParent("center-route-c")).semanticGroupingAnchors.map(_.stableKey).toSet
    assertNotEquals(anchorsA, anchorsB)
    assertEquals(anchorsA, anchorsC)

  test("claim canonicalization removes only an exact duplicate"):
    val assembled = strategicAssembly()
    val byParent = strategicMechanisms(assembled).map(record => record.parents.head.id -> record).toMap
    val routeA = strategicClaim("claim-route-a", byParent("center-route-a"), EvidenceConfidence.BoardDerived)
    val exactDuplicate = routeA.copy(id = "claim-route-a-duplicate")
    val routeC = strategicClaim("claim-route-c", byParent("center-route-c"), EvidenceConfidence.BoardDerived)
    val distinctConfidence = strategicClaim("claim-route-a-mixed", byParent("center-route-a"), EvidenceConfidence.Mixed)
    val decisions = List(routeA, exactDuplicate, routeC, distinctConfidence).map(certified)
    val result = ClaimDeduplicator.deduplicateDetailed(decisions, assembled.evidenceGraph)
    val reversed = ClaimDeduplicator.deduplicateDetailed(decisions.reverse, assembled.evidenceGraph)

    assertEquals(result.decisions.size, 3)
    assertEquals(result.trace.map(_.originalClaimId), List(exactDuplicate.id))
    assertEquals(reversed.decisions.map(_.claim.id).toSet, result.decisions.map(_.claim.id).toSet)
    assertEquals(
      reversed.trace.map(trace => trace.originalClaimId -> trace.keptClaimId).toSet,
      result.trace.map(trace => trace.originalClaimId -> trace.keptClaimId).toSet
    )
    assert(result.decisions.forall(_.claim.evidence.size == 1))
    assertEquals(
      result.decisions.map(decision => decision.claim.evidence.head.id -> decision.claim.confidence).toSet,
      Set(
        byParent("center-route-a").ref.id -> EvidenceConfidence.BoardDerived,
        byParent("center-route-c").ref.id -> EvidenceConfidence.BoardDerived,
        byParent("center-route-a").ref.id -> EvidenceConfidence.Mixed
      )
    )

  test("line and transition identities include their exact board occurrence"):
    val afterE4 = PrincipalVariationEvidence
      .legalFenAfter(fen, "e2e4")
      .getOrElse(fail("expected e4"))
    val afterD4 = PrincipalVariationEvidence
      .legalFenAfter(fen, "d2d4")
      .getOrElse(fail("expected d4"))
    val rootA = PositionNodeRef(afterE4, 1, Some(Black), Some("branch-root-a"))
    val rootB = PositionNodeRef(afterD4, 1, Some(Black), Some("branch-root-b"))
    val move = "g8f6"
    def normalized(root: PositionNodeRef): NormalizedCandidateLine =
      val history = CanonicalPositionHistory
        .from(root.fen, Nil, root.fen)
        .getOrElse(fail("expected a branch history"))
      val extended = history.extend(List(move)).getOrElse(fail("expected a legal branch line"))
      val replay = CanonicalLineReplay
        .fromHistory(extended.segmentReplaySteps.drop(history.segmentReplaySteps.size))
        .getOrElse(fail("expected a canonical branch replay"))
      NormalizedCandidateLine(
        LineNodeRole.Threat,
        rank = 1,
        CandidateLineEvaluation.EngineSearch(EngineLine(List(move), scoreCp = 0, depth = 18)),
        replay
      )
    val allocator = JudgmentProvenanceAllocator.forInput(input)
    val lineA = normalized(rootA)
    val lineB = normalized(rootB)
    val lineKeyA = allocator.lineOccurrenceKey(lineA, rootA)
    val lineKeyB = allocator.lineOccurrenceKey(lineB, rootB)
    val refA = allocator.lineRef(lineA, lineKeyA)
    val refB = allocator.lineRef(lineB, lineKeyB)

    assertNotEquals(lineKeyA, lineKeyB)
    assertNotEquals(refA, refB)
    assertEquals(
      allocator.lineRef(lineA, allocator.lineOccurrenceKey(lineA, rootA)),
      refA
    )

    val toA = PositionNodeRef(
      PrincipalVariationEvidence.legalFenAfter(rootA.fen, move).getOrElse(fail("expected Nf6 after e4")),
      2,
      Some(White),
      Some("branch-to-a")
    )
    val toB = PositionNodeRef(
      PrincipalVariationEvidence.legalFenAfter(rootB.fen, move).getOrElse(fail("expected Nf6 after d4")),
      2,
      Some(White),
      Some("branch-to-b")
    )
    assertNotEquals(
      allocator.transitionOccurrenceKey(TransitionEdgeRole.Threat, rootA, move, toA),
      allocator.transitionOccurrenceKey(TransitionEdgeRole.Threat, rootB, move, toB)
    )

  test("a combined relation is sealed by one exact transition occurrence"):
    val assembled = EvidenceFactAssembler
      .assemble(
        RawMoveReviewInput(
          fen = "4k3/8/r7/8/8/8/4B3/4R1K1 w - - 0 1",
          playedMoveUci = "e2d3",
          variations = List(
            EngineLine(List("e2b5"), scoreCp = 100, depth = 20),
            EngineLine(List("e2d3", "e8f7"), scoreCp = 0, depth = 20)
          )
        )
      )
      .getOrElse(fail("expected the double-check review to assemble"))
    val doubleChecks = assembled.evidenceGraph.records.collect {
      case record @ EvidenceRecord(_, relation: RelationFactEvidence, _)
          if relation.kind == RelationFactKind.DoubleCheck => record
    }

    assert(doubleChecks.nonEmpty)
    doubleChecks.foreach { record =>
      val carriers = record.parents.flatMap(assembled.evidenceGraph.record).collect {
        case EvidenceRecord(_, occurrence: ClosedRelationOccurrenceEvidence, _) => occurrence
      }
      assertEquals(carriers.size, 1)
      assert(carriers.head.closedResults(RelationCombinationContractKind.DoubleCheck).contains(record.ref))
      assert(assembled.evidenceGraph.proofEligible(record))
    }
    val doubleCheckIds = doubleChecks.map(_.ref.id).toSet
    val verticalMechanisms = assembled.evidenceGraph.records.collect {
      case record @ EvidenceRecord(_, payload: TacticalMechanismEvidence, _)
          if payload.kind == TacticalMechanismKind.KingForcing &&
            payload.signals.exists(signal =>
              signal.kind == TacticalMechanismSignalKind.Relation &&
                signal.source.exists(source => doubleCheckIds(source.id))
            ) && payload.signalKinds.contains(TacticalMechanismSignalKind.LineEvent) =>
        record
    }
    assertEquals(verticalMechanisms.size, doubleChecks.size)
    assert(verticalMechanisms.forall(assembled.evidenceGraph.proofEligible))

  test("threat relation production uses each exact branch occurrence owner"):
    val rootFen = "4k3/8/8/2b5/8/2N5/P7/4K3 w - - 0 1"
    val rootPly = 0
    val relationMove = "c5b4"
    def replay(startFen: String, moves: List[String], startPly: Int): CanonicalLineReplay =
      PrincipalVariationEvidence
        .legalMoveReplay(startFen, moves, startPly)
        .flatMap(CanonicalLineReplay.fromLegalReplay)
        .getOrElse(fail(s"expected legal replay: $startFen ${moves.mkString(" ")}"))
    def branch(sourceProbeId: String, probedMove: String): NormalizedThreatBranch =
      val predecessor = replay(rootFen, List(probedMove), startPly = rootPly)
      val branchFen = predecessor.replaySteps.last.fenAfter
      val continuation = replay(branchFen, List(relationMove), startPly = rootPly + 1)
      NormalizedThreatBranch(
        sourceProbeId = sourceProbeId,
        objective = ProbeObjective.BranchReplyMultiPv,
        probedMoveUci = probedMove,
        branchFen = branchFen,
        branchPly = rootPly + 1,
        opponentResourceMove = None,
        certifiedHorizonPlyOffset = Some(1),
        lines = List(
          NormalizedCandidateLine(
            role = LineNodeRole.Threat,
            rank = 1,
            evaluation = CandidateLineEvaluation.EngineSearch(
              EngineLine(List(relationMove), scoreCp = 0, depth = 20)
            ),
            replay = continuation,
            predecessorReplay = Some(predecessor)
          )
        )
      )

    val branches = List(branch("branch-a", "a2a3"), branch("branch-b", "a2a4"))
    val playedReplay = replay(rootFen, List(branches.head.probedMoveUci), startPly = rootPly)
    val playedLine = NormalizedCandidateLine(
      role = LineNodeRole.Played,
      rank = 1,
      evaluation = CandidateLineEvaluation.EngineSearch(
        EngineLine(List(branches.head.probedMoveUci), scoreCp = 0, depth = 20)
      ),
      replay = playedReplay
    )
    val history = CanonicalPositionHistory
      .from(rootFen, Nil, rootFen)
      .fold(error => fail(error.toString), identity)
    val reviewInput = NormalizedMoveReviewInput(
      beforeFen = rootFen,
      playedMoveUci = branches.head.probedMoveUci,
      beforePly = rootPly,
      sideToMove = Some(White),
      afterPlayedFen = branches.head.branchFen,
      afterReferenceFen = None,
      lines = List(playedLine),
      completeCandidateSet = None,
      positionHistory = history,
      openingContext = OpeningContextEvidence(None, Nil),
      threatBranches = branches
    )
    val assembled = NodeLineTransitionAssembler
      .assemble(reviewInput)
      .getOrElse(fail("expected both threat branches to assemble"))
    val threatLines = assembled.lines.filter(_.role == LineNodeRole.Threat)
    assertEquals(
      threatLines.map(line => (line.role, line.ref.rank, line.ref.rootMove)).toSet,
      Set((LineNodeRole.Threat, 1, relationMove))
    )
    val expectedPositionByLine = threatLines.map { line =>
      val owner = assembled
        .threatLineOwner(line.ref)
        .getOrElse(fail(s"expected exact owner for ${line.ref.id}"))
      line.ref -> owner.branchPosition
    }.toMap
    assertEquals(expectedPositionByLine.values.toSet.size, 2)
    val afterPositionByLine = threatLines.map { line =>
      line.ref -> assembled
        .lineRootOccurrence(line.ref)
        .getOrElse(fail(s"expected root occurrence for ${line.ref.id}"))
        .destination
    }.toMap

    val enriched = EvidenceFactAssembler.enrich(assembled)
    val closedOccurrences = enriched.evidenceGraph.records.collect {
      case EvidenceRecord(_, occurrence: ClosedRelationOccurrenceEvidence, _) => occurrence
    }
    assertEquals(
      closedOccurrences.map(_.edge.evidence.id).toSet,
      assembled.transitions.map(_.evidence.id).toSet
    )
    assert(closedOccurrences.forall(
      _.closedResults.keySet == RelationCombinationContractKind.values.toSet
    ))
    val relationRecords = enriched.evidenceGraph
      .records
      .collect {
        case record @ EvidenceRecord(ref, relation: RelationFactEvidence, _)
            if ref.line.exists(expectedPositionByLine.contains) && (relation.detail match
              case RelationWitnessDetail.RayBarrier(owner, _, _, occupants, axis) =>
                RelationRayPattern.classify(owner, occupants, axis) == RelationRayPattern.Pin
              case _ => false
            ) =>
          record -> relation
      }
    assertEquals(relationRecords.size, 2)
    assertEquals(
      relationRecords.map { case (record, _) => record.ref.line.get -> record.ref.position }.toMap,
      expectedPositionByLine
    )
    assertEquals(relationRecords.map(_._2.semanticId).distinct.size, 1)
    assertEquals(relationRecords.map(_._1.ref.id).distinct.size, 2)
    relationRecords.foreach { case (record, relation) =>
      val afterPosition = afterPositionByLine(record.ref.line.get)
      val source = enriched.evidenceGraph.relationGraph
        .positionRelationsAt(afterPosition)
        .filter(_.relation.detail == relation.detail)
      assertEquals(source.size, 1)
      assert(
        record.parents.contains(source.head.ref),
        "a line-owned named ray must retain its exact closed after-position source"
      )
      assert(enriched.evidenceGraph.proofEligible(record))
    }

  private def strategicAssembly(reverseSources: Boolean = false): JudgmentAssemblyContext =
    val sourceRecords = List(
      featureAnchor("center-route-a", FeatureAnchorSignal.PawnTensionObserved),
      featureAnchor("center-route-b", FeatureAnchorSignal.StructuralDeltaObserved),
      featureAnchor("center-route-c", FeatureAnchorSignal.PawnTensionObserved)
    )
    val sources = if reverseSources then sourceRecords.reverse else sourceRecords
    EvidenceFactAssembler.enrich(
      JudgmentAssemblyContext(
        input = input,
        positions = Nil,
        lines = Nil,
        transitions = Nil,
        evidenceGraph = TypedEvidenceGraph.empty.addAll(sources),
        claims = Nil
      )
    )

  private def strategicMechanisms(context: JudgmentAssemblyContext): List[EvidenceRecord] =
    context.evidenceGraph.records.collect {
      case record @ EvidenceRecord(_, payload: StrategicMechanismEvidence, _)
          if payload.kind == StrategicMechanismKind.CenterControl =>
        record
    }

  private def strategicPayload(record: EvidenceRecord): StrategicMechanismEvidence =
    record.payload match
      case payload: StrategicMechanismEvidence => payload
      case _                                    => fail("expected strategic mechanism")

  private def strategicClaim(
      id: String,
      carrier: EvidenceRecord,
      confidence: EvidenceConfidence
  ): JudgmentClaim =
    JudgmentClaim(
      id = id,
      family = ClaimFamily.Strategic,
      subject = ClaimSubject.Position,
      primaryPosition = position,
      primaryLine = None,
      subjectMove = None,
      evidence = List(carrier.ref),
      scope = EvidenceScope.BeforePosition,
      confidence = confidence
    )

  private def certified(claim: JudgmentClaim): ClaimAdmissionDecision =
    ClaimAdmissionDecision(
      claim = claim,
      status = ClaimAdmissionStatus.Certified,
      presentLayers = claim.evidence.map(_.layer).toSet,
      missingLayerGroups = Nil,
      missingEvidence = Nil
    )

  private def featureAnchor(id: String, signal: FeatureAnchorSignal): EvidenceRecord =
    EvidenceRecord(
      EvidenceRef(
        id = id,
        producer = EvidenceProducer.FeatureAnchorProducer,
        layer = EvidenceLayer.FeatureAnchor,
        position = position,
        line = None,
        scope = EvidenceScope.BeforePosition,
        confidence = EvidenceConfidence.BoardDerived
      ),
      FeatureAnchorEvidence(
        FeatureAnchor(
          theme = OpeningTheme.CenterControl,
          signal = signal,
          sourceLayer = EvidenceLayer.Relation
        )
      )
    )

  private def mateEvaluation(id: String, mate: Int): EvidenceRecord =
    val evaluation = CandidateLineEvaluation.EngineSearch(
      EngineLine(List(line.rootMove), scoreCp = 900, mate = Some(mate), depth = 20)
    )
    EvidenceRecord(
      EvidenceRef(
        id = id,
        producer = EvidenceProducer.EngineEvalProducer,
        layer = EvidenceLayer.Eval,
        position = position,
        line = Some(line),
        scope = line.role.scope,
        confidence = EvidenceConfidence.EngineBacked
      ),
      CandidateLineEvaluationEvidence(line, evaluation)
    )

  private def candidateEvaluation(record: EvidenceRecord): CandidateLineEvaluation =
    record.payload match
      case CandidateLineEvaluationEvidence(_, evaluation) => evaluation
      case _                                               => fail("expected candidate evaluation")

  private def input: NormalizedMoveReviewInput =
    val after = PrincipalVariationEvidence
      .legalFenAfter(fen, line.rootMove)
      .getOrElse(fail("expected legal root move"))
    val history = CanonicalPositionHistory
      .from(fen, Nil, fen)
      .fold(error => fail(error.toString), identity)
    NormalizedMoveReviewInput(
      beforeFen = fen,
      playedMoveUci = line.rootMove,
      beforePly = 0,
      sideToMove = Some(White),
      afterPlayedFen = after,
      afterReferenceFen = None,
      lines = Nil,
      completeCandidateSet = None,
      positionHistory = history,
      openingContext = OpeningContextEvidence(None, Nil)
    )
