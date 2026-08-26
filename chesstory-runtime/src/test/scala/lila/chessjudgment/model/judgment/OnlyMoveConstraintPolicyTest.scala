package lila.chessjudgment.model.judgment

import chess.{ King, Queen, White }
import lila.chessjudgment.model.line.CanonicalPositionHistory
import lila.chessjudgment.model.strategic.EngineLine


class OnlyMoveConstraintPolicyTest extends munit.FunSuite:

  private val mateFen = "7k/8/5KQ1/8/8/8/P7/8 w - - 0 1"

  private def playedMoves(fixture: ConstraintFixture): Set[String] =
    fixture.fact.kind match
      case CandidateComparisonKind.PlayedVsBest => Set(fixture.candidate.rootMove)
      case CandidateComparisonKind.BestVsSecond => Set(fixture.reference.rootMove)
      case _                                     => Set.empty

  test("only-move constraint qualifies an exact line-backed mate cause"):
    val fixture = constraintFixture()
    val exactLine = mateLineRecord("exact-line", fixture.reference)
    val siblingLine = fixture.reference.copy(id = "sibling", role = LineNodeRole.Threat)
    val siblingProofRecord = mateLineRecord("sibling-line", siblingLine)

    val exact = causeRecord(
      id = "exact-cause",
      fixture = fixture,
      kind = RelativeCauseKind.KingForcing,
      sourceSide = RelativeCauseSourceSide.Reference,
      attribution = ownedReferenceAttribution,
      direct = exactLine.ref
    )
    val wrongSource = causeRecord(
      id = "wrong-source-cause",
      fixture = fixture,
      kind = RelativeCauseKind.KingForcing,
      sourceSide = RelativeCauseSourceSide.Candidate,
      attribution = CauseAttribution(
        CauseAttributionKind.CandidateCreatesValue,
        rootMoveMatched = true,
        directProofEligible = true
      ),
      direct = exactLine.ref
    )
    val wrongAttribution = causeRecord(
      id = "wrong-attribution-cause",
      fixture = fixture,
      kind = RelativeCauseKind.KingForcing,
      sourceSide = RelativeCauseSourceSide.Reference,
      attribution = CauseAttribution(
        CauseAttributionKind.ContextOnly,
        rootMoveMatched = true,
        directProofEligible = false
      ),
      direct = exactLine.ref
    )
    val siblingProof = causeRecord(
      id = "sibling-proof-cause",
      fixture = fixture,
      kind = RelativeCauseKind.KingForcing,
      sourceSide = RelativeCauseSourceSide.Reference,
      attribution = ownedReferenceAttribution,
      direct = siblingProofRecord.ref
    )

    val otherFixture = constraintFixture(
      comparisonId = "other-comparison",
      candidate = fixture.candidate.copy(id = "other-candidate", rootMove = "b2b3")
    )
    val otherComparisonCause = causeRecord(
      id = "other-comparison-cause",
      fixture = otherFixture,
      kind = RelativeCauseKind.KingForcing,
      sourceSide = RelativeCauseSourceSide.Reference,
      attribution = ownedReferenceAttribution,
      direct = exactLine.ref
    )

    val graph = graphOf(
      fixture.comparisonRecord,
      otherFixture.comparisonRecord,
      exactLine,
      siblingProofRecord,
      exact,
      wrongSource,
      wrongAttribution,
      siblingProof,
      otherComparisonCause
    )

    val rows = List(
      "same comparison/reference/source/attribution with owned objects" ->
        OnlyMoveConstraintPolicy.qualifier(exact.ref, cause(exact, graph), graph, playedMoves(fixture)).nonEmpty -> true,
      "different source" ->
        OnlyMoveConstraintPolicy.qualifier(wrongSource.ref, cause(wrongSource, graph), graph, playedMoves(fixture)).nonEmpty -> false,
      "different attribution" ->
        OnlyMoveConstraintPolicy.qualifier(wrongAttribution.ref, cause(wrongAttribution, graph), graph, playedMoves(fixture)).nonEmpty -> false,
      "sibling line direct proof" ->
        OnlyMoveConstraintPolicy.qualifier(siblingProof.ref, cause(siblingProof, graph), graph, playedMoves(fixture)).nonEmpty -> false,
      "different comparison" ->
        OnlyMoveConstraintPolicy
          .resolve(
            fixture.comparisonRecord.ref,
            List(cause(otherComparisonCause, graph) -> otherComparisonCause.ref),
            graph,
            playedMoves(fixture)
          )
          .exists(_.qualifiers.nonEmpty) -> false
    )

    rows.foreach { case ((label, actual), expected) =>
      assertEquals(actual, expected, label)
    }

    val qualifier = OnlyMoveConstraintPolicy
      .qualifier(exact.ref, cause(exact, graph), graph, playedMoves(fixture))
      .getOrElse(fail("expected exact concrete cause qualifier"))
    assertEquals(qualifier.causeEvidence.id, exact.ref.id)
    assertEquals(qualifier.comparisonEvidence.id, fixture.comparisonRecord.ref.id)
    assertEquals(qualifier.relation, OnlyMoveMechanismRelation.SameChannelAssociation)
    assertEquals(qualifier.licensesCausalBecause, false)
    assertEquals(
      EvidenceObjectBinding
        .fromRelativeCauseForProjection(cause(exact, graph), graph)
        .filter(_.specificTargetMechanismReady)
        .map(_.source.id),
      List(exactLine.ref.id)
    )

  test("comparison-only uniqueness remains diagnostic without a concrete cause"):
    val fixture = constraintFixture()
    val graph = graphOf(fixture.comparisonRecord)

    val resolution = OnlyMoveConstraintPolicy
      .resolve(
        fixture.comparisonRecord.ref,
        Nil,
        graph,
        playedMoves(fixture)
      )
      .getOrElse(fail("expected diagnostic only-move resolution"))

    assertEquals(resolution.disposition, OnlyMoveConstraintDisposition.DiagnosticOnly)
    assertEquals(resolution.qualifiers, Nil)

  test("exact line-backed cause survives while descendant evidence cannot be borrowed"):
    val fixture = constraintFixture()
    val exactLine = mateLineRecord("mate-line", fixture.reference)
    val exactCause = causeRecord(
      id = "line-backed-cause",
      fixture = fixture,
      kind = RelativeCauseKind.KingForcing,
      sourceSide = RelativeCauseSourceSide.Reference,
      attribution = ownedReferenceAttribution,
      direct = exactLine.ref
    )
    val descendant = EvidenceRecord(
      ref = evidenceRef(
        id = "descendant-eval",
        producer = EvidenceProducer.EngineEvalProducer,
        layer = EvidenceLayer.Eval,
        line = Some(fixture.candidate),
        confidence = EvidenceConfidence.EngineBacked
      ),
      payload = CandidateLineEvaluationEvidence(
        fixture.candidate,
        lila.chessjudgment.model.line.CandidateLineEvaluation.EngineSearch(
          EngineLine(List(fixture.candidate.rootMove), 0, None, 18)
        )
      ),
      parents = List(exactCause.ref)
    )
    val graph = graphOf(fixture.comparisonRecord, exactLine, exactCause, descendant)
    val lineCause = cause(exactCause, graph)

    assertEquals(ClaimFamily.fromCause(RelativeCauseKind.KingForcing), ClaimFamily.Tactical)
    assert(lineCause.hasOwnedTypedDepth(graph))
    assert(
      EvidenceObjectBinding.specificTargetMechanismReady(
        EvidenceObjectBinding.fromRelativeCauseForProjection(lineCause, graph)
      )
    )
    val diagnostic = OnlyMoveConstraintPolicy.resolveAll(
      graph,
      Nil,
      playedMoves(fixture)
    )
    assertEquals(diagnostic.map(_.disposition), List(OnlyMoveConstraintDisposition.DiagnosticOnly))
    assertEquals(diagnostic.flatMap(_.qualifiers), Nil)
    val selected = OnlyMoveConstraintPolicy.resolveAll(
      graph,
      List(lineCause -> exactCause.ref),
      playedMoves(fixture)
    )
    assertEquals(
      selected.flatMap(_.qualifiers).map(_.causeEvidence.id),
      List(exactCause.ref.id)
    )
    assertEquals(selected.map(_.disposition), List(OnlyMoveConstraintDisposition.ConcreteCauseQualifier))

  private final case class ConstraintFixture(
      reference: LineNodeRef,
      candidate: LineNodeRef,
      fact: CandidateComparisonFact,
      comparisonRecord: EvidenceRecord
  )

  private def constraintFixture(
      comparisonId: String = "played-vs-best",
      reference: LineNodeRef = LineNodeRef("best", "g6g7", 1, LineNodeRole.BestReference),
      candidate: LineNodeRef = LineNodeRef("played", "a2a3", 2, LineNodeRole.Played),
      kind: CandidateComparisonKind = CandidateComparisonKind.PlayedVsBest
  ): ConstraintFixture =
    val fact = CandidateComparisonFact(
      kind = kind,
      referenceLine = reference,
      candidateLine = candidate,
      comparison = EvalComparison(
        White,
        -30.0,
        MoveChoiceVerdict.Blunder,
        CandidateComparisonDeltaDetail.OutcomeOnly
      ),
      verdictConfidence = VerdictConfidence.EngineBacked,
      candidateSet = Some(CandidateSetDescriptor(CandidateSetType.OnlyMove))
    )
    val ref = evidenceRef(
      id = comparisonId,
      producer = EvidenceProducer.RelativeMoveProducer,
      layer = EvidenceLayer.CandidateComparison,
      line = Some(candidate),
      confidence = EvidenceConfidence.EngineBacked
    )
    ConstraintFixture(reference, candidate, fact, EvidenceRecord(ref, CandidateComparisonEvidence(fact)))

  private def causeRecord(
      id: String,
      fixture: ConstraintFixture,
      kind: RelativeCauseKind,
      sourceSide: RelativeCauseSourceSide,
      attribution: CauseAttribution,
      direct: EvidenceRef
  ): EvidenceRecord =
    val cause = RelativeCauseFact(
      kind = kind,
      comparisonEvidence = fixture.comparisonRecord.ref,
      supportEvidence = List(direct),
      sourceSide = sourceSide,
      attribution = attribution,
      proof = Some(RelativeCauseProof(
        directProof = RelativeCauseProofSection(
          RelativeCauseProofRole.DirectProof,
          RelativeCauseProofStrength.Primary,
          List(direct)
        )
      ))
    )
    val eventLine =
      if sourceSide == RelativeCauseSourceSide.Candidate then fixture.candidate else fixture.reference
    EvidenceRecord(
      ref = evidenceRef(
        id = id,
        producer = EvidenceProducer.RelativeMoveProducer,
        layer = EvidenceLayer.RelativeCause,
        line = Some(eventLine),
        confidence = EvidenceConfidence.EngineBacked
      ),
      payload = RelativeCauseFactEvidence(cause),
      parents = List(fixture.comparisonRecord.ref, direct).distinctBy(_.id)
    )

  private def mateLineRecord(id: String, line: LineNodeRef): EvidenceRecord =
    val history = CanonicalPositionHistory
      .from(mateFen, Nil, mateFen)
      .getOrElse(fail("expected a canonical mate root"))
    val extended = history
      .extend(List(line.rootMove))
      .getOrElse(fail("expected legal mating move"))
    val replay = CanonicalLineReplay
      .fromHistory(extended.segmentReplaySteps.drop(history.segmentReplaySteps.size))
      .getOrElse(fail("expected one certified mating replay"))
    EvidenceRecord(
      ref = evidenceRef(
        id = id,
        producer = EvidenceProducer.LegalLineProducer,
        layer = EvidenceLayer.Line,
        line = Some(line),
        confidence = EvidenceConfidence.LegalReplayVerified
      ),
      payload = LineFactEvidence.fromCertifiedReplay(
        line = line,
        replay = replay,
        events = List(
          LineMoveEvent(
            kind = LineEventKind.Mate,
            moveUci = line.rootMove,
            plyOffset = 0,
            side = Some(White),
            pieceRole = Some(EvidencePieceRole(Queen.name)),
            targetRole = Some(EvidencePieceRole(King.name)),
            square = Some(EvidenceSquare("h8"))
          )
        ),
        consequences = List(
          LineConsequence(
            kind = LineConsequenceKind.Mate,
            lineMoves = List(line.rootMove),
            proofSignal = true,
            eventMove = Some(line.rootMove),
            rootMove = Some(line.rootMove),
            rootSide = Some(White),
            beneficiary = Some(White)
          )
        )
      )
    )

  private def evidenceRef(
      id: String,
      producer: EvidenceProducer,
      layer: EvidenceLayer,
      line: Option[LineNodeRef],
      confidence: EvidenceConfidence
  ): EvidenceRef =
    EvidenceRef(
      id = id,
      producer = producer,
      layer = layer,
      position = PositionNodeRef(mateFen, 0, Some(White)),
      line = line,
      scope = line.map(_.role.scope).getOrElse(EvidenceScope.Counterfactual),
      confidence = confidence
    )

  private def ownedReferenceAttribution: CauseAttribution =
    CauseAttribution(
      CauseAttributionKind.ReferenceCreatesResource,
      rootMoveMatched = true,
      directProofEligible = true
    )

  private def cause(
      record: EvidenceRecord,
      graph: TypedEvidenceGraph
  ): RelativeCauseFact =
    ExplicitCauseAdmissionTestSupport
      .registeredCause(record.ref, graph)
      .getOrElse(fail(s"expected registered Cause ${record.ref.id}"))

  private def graphOf(records: EvidenceRecord*): TypedEvidenceGraph =
    ExplicitCauseAdmissionTestSupport.graph(records.toList)
