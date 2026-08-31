package lila.chessjudgment.analysis.assembly

import chess.{ Black, White }
import lila.chessjudgment.analysis.policy.{ ClaimAdmissionDecision, ClaimAdmissionStatus }
import lila.chessjudgment.analysis.position.PositionRelationExtractor
import lila.chessjudgment.analysis.relation.ClosedRelationEvidence
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.{ CandidateLineEvaluation, CanonicalPositionHistory, PrincipalVariationEvidence }
import lila.chessjudgment.model.line.EngineLine

class EvidenceAssemblyIdentityTest extends munit.FunSuite:

  private val fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
  private val position = PositionNodeRef(fen, 0, Some(White), Some("identity-root"))
  private val line = LineNodeRef("identity-line", "e2e4", 1, LineNodeRole.Played)

  private def hasCreatedDoubleCheck(relation: RelationFactEvidence): Boolean =
    relation.detail match
      case RelationWitnessDetail.CreatedCheckResponseInventory(_, _, _, checkers, _, _, _, _) =>
        checkers.size >= 2
      case _ => false

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
    def normalized(root: PositionNodeRef): AdmittedReviewLine =
      val history = CanonicalPositionHistory
        .from(root.fen, Nil, root.fen)
        .getOrElse(fail("expected a branch history"))
      val extended = history.extend(List(move)).getOrElse(fail("expected a legal branch line"))
      val replay = CanonicalLineReplay
        .fromHistory(extended.segmentReplaySteps.drop(history.segmentReplaySteps.size))
        .getOrElse(fail("expected a canonical branch replay"))
      AdmittedReviewLine(
        LineNodeRole.BranchReply,
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
      allocator.transitionOccurrenceKey(TransitionEdgeRole.BranchReply, rootA, move, toA),
      allocator.transitionOccurrenceKey(TransitionEdgeRole.BranchReply, rootB, move, toB)
    )

  test("fresh double-check control stays on one transition occurrence and feeds king forcing"):
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
    val freshChecks = assembled.evidenceGraph.records.collect {
      case record @ EvidenceRecord(_, relation: RelationFactEvidence, _)
          if hasCreatedDoubleCheck(relation) => record
    }

    assert(freshChecks.nonEmpty)
    freshChecks.foreach { record =>
      val carriers = record.parents.flatMap(assembled.evidenceGraph.record).collect {
        case EvidenceRecord(_, occurrence: ClosedRelationOccurrenceEvidence, _) => occurrence
      }
      assertEquals(carriers.size, 1)
      assert(assembled.evidenceGraph.proofEligible(record))
    }
    val responseInventories = assembled.evidenceGraph.records.collect {
      case record @ EvidenceRecord(_, relation: RelationFactEvidence, _)
          if relation.kind == RelationFactKind.CreatedCheckResponseInventory => record
    }
    assert(responseInventories.nonEmpty)
    assert(responseInventories.forall(assembled.evidenceGraph.proofEligible))
    val freshCheckIds = freshChecks.map(_.ref.id).toSet
    val mechanisms = assembled.evidenceGraph.records.collect {
      case record @ EvidenceRecord(_, payload: TacticalMechanismEvidence, _)
          if payload.kind == TacticalMechanismKind.KingForcing &&
            payload.signals.exists(signal =>
              signal.kind == TacticalMechanismSignalKind.Relation &&
                signal.source.exists(source => freshCheckIds(source.id))
            ) && payload.signalKinds.contains(TacticalMechanismSignalKind.LineEvent) =>
        record
    }
    assertEquals(mechanisms.size, freshChecks.size)
    assert(mechanisms.forall { record =>
      val proof = record.payload
        .asInstanceOf[TacticalMechanismEvidence]
        .relationLineProof
        .getOrElse(fail("expected the exact relation/occurrence/line bridge"))
      record.parents.contains(proof.relation) && record.parents.contains(proof.line) &&
        assembled.evidenceGraph
          .record(proof.relation)
          .exists(_.parents.contains(proof.occurrence)) &&
        assembled.evidenceGraph.proofEligible(record)
    })

  test("a root check response binds the transition carrier and chosen legal response once"):
    val assembled = EvidenceFactAssembler
      .assemble(
        RawMoveReviewInput(
          fen = "k3r3/8/8/8/8/3B4/8/4K3 w - - 0 1",
          playedMoveUci = "d3e2",
          variations = List(
            EngineLine(List("e1d2"), scoreCp = 20, depth = 20),
            EngineLine(List("d3e2", "e8e2"), scoreCp = 0, depth = 20)
          )
        )
      )
      .getOrElse(fail("expected the checked root response to assemble"))
    val response = assembled.evidenceGraph.records.collectFirst {
      case record @ EvidenceRecord(_, relation: RelationFactEvidence, _)
          if relation.kind == RelationFactKind.RootCheckResponse => record
    }.getOrElse(fail("expected the root response relation"))
    val carriers = response.parents.flatMap(assembled.evidenceGraph.record).collect {
      case EvidenceRecord(_, occurrence: ClosedRelationOccurrenceEvidence, _) => occurrence
    }

    assertEquals(carriers.size, 1)
    assertEquals(response.parents.distinct.size, response.parents.size)
    assert(assembled.evidenceGraph.proofEligible(response))

  test("a root-and-absence stalemate proof materializes without a fabricated positive source"):
    val assembled = EvidenceFactAssembler
      .assemble(
        RawMoveReviewInput(
          fen = "k7/2Q5/2K5/8/8/8/8/8 w - - 0 1",
          playedMoveUci = "c7b6",
          variations = List(
            EngineLine(List("c7b6"), scoreCp = 0, depth = 20)
          )
        )
      )
      .getOrElse(fail("expected the stalemate transition to assemble"))
    val stalemate = assembled.evidenceGraph.records.collectFirst {
      case record @ EvidenceRecord(_, relation: RelationFactEvidence, _)
          if relation.kind == RelationFactKind.StalemateTransition => record
    }.getOrElse(fail("expected the closed stalemate relation"))
    val parents = stalemate.parents.flatMap(assembled.evidenceGraph.record)

    assertEquals(parents.count(_.payload.isInstanceOf[ClosedRelationOccurrenceEvidence]), 1)
    assertEquals(parents.count(_.payload.isInstanceOf[RelationFactEvidence]), 0)
    assert(assembled.evidenceGraph.proofEligible(stalemate))

  test("transposed boards share double-check control semantics but retain both exact occurrences"):
    val rootFen = "4k3/6pp/8/8/8/8/PP2B3/4R1K1 w - - 0 1"
    def endFen(moves: List[String]): String =
      PrincipalVariationEvidence
        .legalMoveReplay(rootFen, moves, startPly = 0)
        .flatMap(CanonicalLineReplay.fromLegalReplay)
        .flatMap(_.replaySteps.lastOption.map(_.fenAfter))
        .getOrElse(fail(s"expected legal transposition path: ${moves.mkString(" ")}"))
    val transposedA = endFen(List("a2a3", "h7h6", "b2b3", "g7g6"))
    val transposedB = endFen(List("b2b3", "g7g6", "a2a3", "h7h6"))
    assert(PrincipalVariationEvidence.sameBoardState(transposedA, transposedB))

    val replay = PrincipalVariationEvidence
      .legalMoveReplay(transposedA, List("e2b5"), startPly = 4)
      .flatMap(CanonicalLineReplay.fromLegalReplay)
      .getOrElse(fail("expected the transposed double-check transition"))
    val transition = replay.onlyTransition.getOrElse(fail("expected one transition"))
    val production = ClosedRelationEvidence.relationProduction(replay, "e2b5")
    def bind(path: String): CanonicalRelationTransitionInventory =
      val beforeRef = PositionNodeRef(transposedA, 4, Some(White), Some(s"$path-before"))
      val afterFen = chess.format.Fen.write(transition.legal.after).value
      val afterRef = PositionNodeRef(afterFen, 5, Some(Black), Some(s"$path-after"))
      def snapshot(
          analysis: lila.chessjudgment.analysis.position.PositionAnalysis,
          position: PositionNodeRef
      ): CanonicalPositionRelationSnapshot =
        TypedEvidenceGraph.empty
          .addAll(PositionRelationExtractor.records(
            analysis.boardRelations,
            position,
            EvidenceScope.CurrentPosition,
            relation => s"$path:${position.ply}:${relation.semanticId}"
          ))
          .relationGraph
          .closedPositionRelationSnapshot(position, EvidenceScope.CurrentPosition, analysis.relationInventory)
      val before = snapshot(transition.beforeAnalysis, beforeRef)
      val after = snapshot(transition.afterAnalysis, afterRef)
      val delta = CanonicalRelationDelta.bind(transition.declared, transition.relationDelta, before, after)
      production.bindClosedOutput(before, after, delta)
    val boundA = bind("transposition-a")
    val boundB = bind("transposition-b")
    val doubleA = boundA.relations.filter(hasCreatedDoubleCheck)
    val doubleB = boundB.relations.filter(hasCreatedDoubleCheck)

    assertEquals(doubleA.size, 1)
    assertEquals(doubleB.size, 1)
    assertEquals(doubleA.head.assertionId, doubleB.head.assertionId)
    assertEquals(doubleA.head.semanticId, doubleB.head.semanticId)
    assertNotEquals(boundA.before.position, boundB.before.position)
    assert(PrincipalVariationEvidence.sameBoardState(boundA.before.position.fen, boundB.before.position.fen))
    assertNotEquals(boundA.after.position, boundB.after.position)

  test("branch-reply relation production uses each exact branch occurrence owner"):
    val rootFen = "4k3/8/8/2b5/8/2N5/P7/4K3 w - - 0 1"
    val rootPly = 0
    val relationMove = "c5b4"
    def replay(startFen: String, moves: List[String], startPly: Int): CanonicalLineReplay =
      PrincipalVariationEvidence
        .legalMoveReplay(startFen, moves, startPly)
        .flatMap(CanonicalLineReplay.fromLegalReplay)
        .getOrElse(fail(s"expected legal replay: $startFen ${moves.mkString(" ")}"))
    def branch(sourceProbeId: String, probedMove: String): AdmittedReviewBranchReply =
      val predecessor = replay(rootFen, List(probedMove), startPly = rootPly)
      val branchFen = predecessor.replaySteps.last.fenAfter
      val continuation = replay(branchFen, List(relationMove), startPly = rootPly + 1)
      AdmittedReviewBranchReply(
        sourceProbeId = sourceProbeId,
        probedMoveUci = probedMove,
        branchFen = branchFen,
        branchPly = rootPly + 1,
        certifiedHorizonPlyOffset = 1,
        lines = List(
          AdmittedReviewLine(
            role = LineNodeRole.BranchReply,
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
    val playedLine = AdmittedReviewLine(
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
    val reviewInput = AdmittedMoveReviewInput(
      beforeFen = rootFen,
      playedMoveUci = branches.head.probedMoveUci,
      beforePly = rootPly,
      sideToMove = Some(White),
      afterPlayedFen = branches.head.branchFen,
      afterReferenceFen = None,
      lines = List(playedLine),
      completeCandidateSet = None,
      positionHistory = history,
      branchReplies = branches
    )
    val assembled = NodeLineTransitionAssembler
      .assemble(reviewInput)
      .getOrElse(fail("expected both branch replies to assemble"))
    val branchReplyLines = assembled.lines.filter(_.role == LineNodeRole.BranchReply)
    assertEquals(
      branchReplyLines.map(line => (line.role, line.ref.rank, line.ref.rootMove)).toSet,
      Set((LineNodeRole.BranchReply, 1, relationMove))
    )
    val expectedPositionByLine = branchReplyLines.map { line =>
      val owner = assembled
        .branchReplyLineOwner(line.ref)
        .getOrElse(fail(s"expected exact owner for ${line.ref.id}"))
      line.ref -> owner.branchPosition
    }.toMap
    assertEquals(expectedPositionByLine.values.toSet.size, 2)
    val afterPositionByLine = branchReplyLines.map { line =>
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
    val relationRecords = enriched.evidenceGraph
      .records
      .collect {
        case record @ EvidenceRecord(ref, relation: RelationFactEvidence, _)
            if ref.line.exists(expectedPositionByLine.contains) && (relation.detail match
              case RelationWitnessDetail.NamedRayTransition(_, _, _, _, _, _, _, pattern, direction, _) =>
                pattern == RelationRayPattern.AbsoluteKingPin && direction == RelationChangeDirection.Established
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
        .filter(node => (node.relation.detail, relation.detail) match
          case (
                ray: RelationWitnessDetail.RayBarrier,
                RelationWitnessDetail.NamedRayTransition(_, side, attacker, role, barrier, target, axis, pattern, _, _)
              ) =>
            RelationRayProjection.named(ray).exists(projection =>
              projection.side == side && projection.attackerSquare == attacker &&
                projection.attackerRole == role && projection.barrier == barrier &&
                projection.immediateTarget == target && projection.axis == axis &&
                projection.pattern == pattern
            )
          case _ => false
        )
      assertEquals(source.size, 1)
      assert(
        record.parents.contains(source.head.ref),
        "a line-owned named ray must retain its exact closed after-position source"
      )
      assert(enriched.evidenceGraph.proofEligible(record))
    }

  test("a branch-reply carrier cannot become a defensive claim without a line reference"):
    val carrier = EvidenceRecord(
      EvidenceRef(
        id = "branch-reply-defensive-carrier",
        producer = EvidenceProducer.TacticalMechanismProducer,
        layer = EvidenceLayer.TacticalMechanism,
        position = position,
        line = None,
        scope = EvidenceScope.BranchReplyLine,
        confidence = EvidenceConfidence.LegalReplayVerified
      ),
      TacticalMechanismEvidence(
        kind = TacticalMechanismKind.DefensiveResource,
        moveUci = Some("e2e4"),
        line = None,
        signals = List(
          TacticalMechanismSignal(
            kind = TacticalMechanismSignalKind.LineEvent,
            label = "branch-reply-carrier",
            sourceLayer = EvidenceLayer.Line
          )
        )
      )
    )
    val context = JudgmentAssemblyContext(
      input = input,
      positions = Nil,
      lines = Nil,
      transitions = Nil,
      evidenceGraph = TypedEvidenceGraph.empty.add(carrier),
      claims = Nil
    )

    assertEquals(JudgmentClaimAssembler.propose(context), Nil)

  test("a defensive resource remains owned by its exact line rather than inventing a threat subject"):
    val carrier = EvidenceRecord(
      EvidenceRef(
        id = "played-line-defensive-carrier",
        producer = EvidenceProducer.TacticalMechanismProducer,
        layer = EvidenceLayer.TacticalMechanism,
        position = position,
        line = Some(line),
        scope = EvidenceScope.PlayedLine,
        confidence = EvidenceConfidence.LegalReplayVerified
      ),
      TacticalMechanismEvidence(
        kind = TacticalMechanismKind.DefensiveResource,
        moveUci = Some(line.rootMove),
        line = Some(line),
        signals = List(
          TacticalMechanismSignal(
            kind = TacticalMechanismSignalKind.LineEvent,
            label = "exact-played-defense",
            sourceLayer = EvidenceLayer.Line
          )
        )
      )
    )
    val context = JudgmentAssemblyContext(
      input = input,
      positions = Nil,
      lines = Nil,
      transitions = Nil,
      evidenceGraph = TypedEvidenceGraph.empty.add(carrier),
      claims = Nil
    )

    val claims = JudgmentClaimAssembler.propose(context)
    assertEquals(claims.size, 1)
    assertEquals(claims.head.family, ClaimFamily.Defensive)
    assertEquals(claims.head.subject, ClaimSubject.PlayedMove)
    assertEquals(claims.head.primaryLine, Some(line))

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

  private def input: AdmittedMoveReviewInput =
    val after = PrincipalVariationEvidence
      .legalFenAfter(fen, line.rootMove)
      .getOrElse(fail("expected legal root move"))
    val history = CanonicalPositionHistory
      .from(fen, Nil, fen)
      .fold(error => fail(error.toString), identity)
    AdmittedMoveReviewInput(
      beforeFen = fen,
      playedMoveUci = line.rootMove,
      beforePly = 0,
      sideToMove = Some(White),
      afterPlayedFen = after,
      afterReferenceFen = None,
      lines = Nil,
      completeCandidateSet = None,
      positionHistory = history,
    )
