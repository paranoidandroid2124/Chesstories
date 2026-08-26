package lila.chessjudgment.analysis.assembly

import chess.{ Black, White }
import lila.chessjudgment.analysis.position.{ PositionAnalyzer, PositionRelationExtractor }
import lila.chessjudgment.analysis.structure.StructuralDeltaAnalyzer
import lila.chessjudgment.analysis.tactical.TacticalRelationEvidence
import lila.chessjudgment.analysis.transition.TransitionFactNormalizer
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.{ CanonicalPositionHistory, PrincipalVariationEvidence }
import lila.chessjudgment.model.strategic.EngineLine

class RelativeCauseGenerationOwnershipTest extends munit.FunSuite:

  private val queenFen = "4k3/8/8/8/8/8/4B3/4R1K1 w - - 0 1"
  private val queenPosition = PositionNodeRef(queenFen, 0, Some(White))
  private val queenReference = LineNodeRef("queen-reference", "e2b5", 1, LineNodeRole.BestReference)
  private val queenCandidate = LineNodeRef("queen-played", "e2d3", 2, LineNodeRole.Played)
  private val queenFact = comparison(queenPosition, queenReference, queenCandidate)
  private val queenBinding = binding(queenReference)

  test("hypothetical-turn pawn blockades do not enter v6 authority"):
    val raw = RawMoveReviewInput(
      fen = "8/5ppp/8/5P1P/2k3P1/2p5/5P2/2K5 b - - 0 1",
      playedMoveUci = "h7h6",
      variations = List(
        EngineLine(List("f7f6", "h5h6", "g7h6", "c1d1", "c4d5", "d1c2", "d5e4", "g4g5", "h6g5", "c2c3", "g5g4", "c3c2", "e4f4", "c2c3"), -534, depth = 20),
        EngineLine(List("h7h6", "f5f6", "g7f6", "f2f4", "c4d4", "g4g5", "h6g5", "f4g5", "f6f5", "h5h6", "f5f4", "h6h7", "f4f3", "h7h8q", "d4e3", "h8h5", "c3c2", "c1c2", "e3f4", "h5h4", "f4f5"), 682, depth = 20)
      )
    )
    val f = EvidenceFactAssembler
      .assemble(raw)
      .map(RelativeAssessmentAssembler.enrichFacts)
      .getOrElse(fail("expected a complete cached-line fact assembly"))
    val c = RelativeAssessmentAssembler.enrichCauses(f)
    val graph = c.evidenceGraph
    assert(!graph.records.exists {
      case EvidenceRecord(_, delta: StructuralDeltaEvidence, _) =>
        delta.consequencesOf(TransitionConsequenceKind.OpponentMobilityRestriction).nonEmpty
      case _ => false
    })
    assert(!graph.records.exists {
      case EvidenceRecord(_, event: PlanCausalEventEvidence, _) =>
        event.planId == lila.chessjudgment.model.strategic.PlanTaxonomy.PlanKind.ProphylaxisRestraint
      case _ => false
    })
    assert(!graph.records.exists {
      case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) =>
        cause.kind == RelativeCauseKind.OpponentRestriction
      case _ => false
    })

  test("direct pawn restriction authority rejects illegal and non-surviving blockades"):
    def restrictionDelta(
        fen: String,
        move: String,
        line: LineNodeRef
    ): (StructuralDeltaEvidence, Option[TransitionConsequence]) =
      val replay = certifiedReplay(fen, List(move))
      val replayTransition = replay.onlyTransition.getOrElse(fail("expected one replay transition"))
      val afterFen = replayTransition.declared.fenAfter
      val from = PositionNodeRef(fen, 0, Some(Black))
      val to = PositionNodeRef(afterFen, 1, Some(White))
      val beforeRelations = canonicalRelations(replayTransition.beforeAnalysis, from)
      val afterRelations = canonicalRelations(replayTransition.afterAnalysis, to)
      val delta = StructuralDeltaAnalyzer.bind(
        transition = replayTransition,
        canonicalRelations = CanonicalRelationDelta.bind(
          replayTransition.declared,
          replayTransition.relationDelta,
          beforeRelations,
          afterRelations
        )
      )
      val transitionRef = EvidenceRef(
        id = s"restriction-transition:$move:${line.id}",
        producer = EvidenceProducer.MoveTransitionProducer,
        layer = EvidenceLayer.MoveTransition,
        position = from,
        line = Some(line),
        scope = EvidenceScope.ReferenceTransition,
        confidence = EvidenceConfidence.LegalReplayVerified
      )

      val transition = MoveTransitionEdge(
        role = TransitionEdgeRole.Reference,
        from = from,
        moveUci = move,
        to = to,
        evidence = transitionRef
      )
      val structural = TransitionFactNormalizer
        .fromStructuralDelta(
          id = s"restriction-structural:$move:${line.id}",
          delta = delta,
          transition = transition,
          replay = replay,
          line = Some(line),
          perspective = Black,
          parents = Nil
        )
        .payload match
          case value: StructuralDeltaEvidence => value
          case _                              => fail("expected normalized structural evidence")
      val consequence = structural
        .consequencesOf(TransitionConsequenceKind.OpponentMobilityRestriction)
        .headOption
      structural -> consequence

    val pinnedFen = "2k5/5p2/8/r4P1K/8/8/8/8 b - - 0 1"
    val pinnedLine = LineNodeRef("pinned-pawn-reference", "f7f6", 1, LineNodeRole.BestReference)
    val (pinnedDelta, pinnedConsequence) = restrictionDelta(pinnedFen, "f7f6", pinnedLine)
    assertEquals(pinnedConsequence, None)
    assertEquals(pinnedDelta.consequencesOf(TransitionConsequenceKind.OpponentMobilityRestriction), Nil)

    val capturedFen = "2k5/5p2/8/4PP2/8/8/8/2K5 b - - 0 1"
    val capturedLine = LineNodeRef("captured-blocker-reference", "f7f6", 1, LineNodeRole.BestReference)
    val (capturedDelta, capturedConsequenceOption) = restrictionDelta(capturedFen, "f7f6", capturedLine)
    assertEquals(capturedConsequenceOption, None)
    val capturedReplay = certifiedReplay(capturedFen, List("f7f6", "e5f6"))
    val capturedLineFact = LineFactEvidence.fromCertifiedReplay(
      line = capturedLine,
      replay = capturedReplay
    )
    assertEquals(capturedLineFact.rootActorSurvivesReply, Some(false))
    assertEquals(capturedDelta.consequencesOf(TransitionConsequenceKind.OpponentMobilityRestriction), Nil)

  test("Tactical mechanism requires its own exact root move and an owned primitive source"):
    val explicitRelation = forcingRelation("mechanism-relation", queenReference)
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

  test("PlanContradiction rejects a structural delta without an owned refuted plan event"):
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
        TransitionConsequenceKind.FileOccupationEstablished,
        strength = 3,
        subjectBindings = List(
          StructuralSubjectBinding.unbound(
            StructuralSubject.FileOccupation(
              EvidenceFile("h"),
              EvidenceSquare("h5"),
              EvidencePieceRole("queen")
            )
          )
        )
      )),
      relationChanges = Nil,
      canonicalTransitionProof = None,
      canonicalDeltaProof = None
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
          lila.chessjudgment.model.line.CandidateLineEvaluation.EngineSearch(EngineLine(List(reference.rootMove), scoreCp = 500, depth = 20)),
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
          lila.chessjudgment.model.line.CandidateLineEvaluation.EngineSearch(EngineLine(List(candidate.rootMove), scoreCp = -500, depth = 20)),
          evidenceRef(
            s"${candidate.id}-eval",
            EvidenceProducer.EngineEvalProducer,
            EvidenceLayer.Eval,
            position,
            candidate,
            EvidenceConfidence.EngineBacked
          )
        )
      ).get,
      VerdictConfidence.EngineBacked
    )

  private def binding(line: LineNodeRef): RelativeCauseBinding =
    RelativeCauseBinding(
      RelativeCauseRole.PrimaryPlayedCause,
      RelativeCauseSourceSide.Reference,
      line,
      List(line),
      RelativeCauseBindingTier.Primary
    )

  private def forcingRelation(id: String, line: LineNodeRef): EvidenceRecord =
    val legalReplay = PrincipalVariationEvidence
      .legalMoveReplay(queenFen, List(line.rootMove), startPly = 0)
      .getOrElse(fail("expected legal forcing replay"))
    val replay = CanonicalLineReplay
      .fromLegalReplay(legalReplay)
      .getOrElse(fail("expected canonical forcing replay"))
    val certified = TacticalRelationEvidence
      .relationProduction(replay, line.rootMove)
      .relations
      .find(_.kind == RelationFactKind.DoubleCheck)
      .getOrElse(fail("expected exact double-check relation"))
    EvidenceRecord(
      evidenceRef(
        id,
        EvidenceProducer.RelationProducer,
        EvidenceLayer.Relation,
        queenPosition,
        line
      ),
      certified
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
      CandidateLineEvaluationEvidence(
        line,
        lila.chessjudgment.model.line.CandidateLineEvaluation.EngineSearch(
          EngineLine(List(line.rootMove), scoreCp = 900, mate = Some(1), depth = 20)
        )
      )
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
            "double-check",
            EvidenceLayer.Relation,
            Some(relationSource),
            Some(RelationFactKind.DoubleCheck)
          )
        )
      )
    )

  private def evidenceRef(
      id: String,
      producer: EvidenceProducer,
      layer: EvidenceLayer,
      position: PositionNodeRef,
      line: LineNodeRef,
      confidence: EvidenceConfidence = EvidenceConfidence.LegalReplayVerified
  ): EvidenceRef =
    EvidenceRef(id, producer, layer, position, Some(line), line.role.scope, confidence)

  private def certifiedReplay(fen: String, moves: List[String]): CanonicalLineReplay =
    (for
      history <- CanonicalPositionHistory.from(fen, Nil, fen).toOption
      extended <- history.extend(moves).toOption
      replay <- CanonicalLineReplay.fromHistory(
        extended.segmentReplaySteps.drop(history.segmentReplaySteps.size)
      )
      rebased <- replay.rebased(1)
    yield rebased).getOrElse(
      fail(s"expected one certified replay for ${moves.mkString(" ")}")
    )

  private def graphOf(records: EvidenceRecord*): TypedEvidenceGraph =
    records.foldLeft(TypedEvidenceGraph.empty)((graph, record) => graph.add(record))

  private def canonicalRelations(
      analysis: lila.chessjudgment.analysis.position.PositionAnalysis,
      position: PositionNodeRef
  ): CanonicalPositionRelationSnapshot =
    TypedEvidenceGraph.empty
      .addAll(
        PositionRelationExtractor.records(
          analysis.boardRelations,
          position,
          EvidenceScope.CurrentPosition,
          relation => s"restriction:${position.ply}:${relation.semanticId}"
        )
      )
      .relationGraph
      .closedPositionRelationSnapshot(
        position,
        EvidenceScope.CurrentPosition,
        analysis.relationInventory
      )
