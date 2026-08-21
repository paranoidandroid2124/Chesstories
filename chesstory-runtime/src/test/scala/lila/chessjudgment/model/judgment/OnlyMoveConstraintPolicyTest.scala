package lila.chessjudgment.model.judgment

import chess.{ Black, Queen, Square, White }
import lila.chessjudgment.model.Motif
import lila.chessjudgment.model.strategic.EngineLine


class OnlyMoveConstraintPolicyTest extends munit.FunSuite:

  private def playedMoves(fixture: ConstraintFixture): Set[String] =
    fixture.fact.kind match
      case CandidateComparisonKind.PlayedVsBest => Set(fixture.candidate.rootMove)
      case CandidateComparisonKind.BestVsSecond => Set(fixture.reference.rootMove)
      case _                                     => Set.empty

  test("only-move constraint qualification truth table"):
    val fixture = constraintFixture()
    val exactThreat = threatRecord("exact-threat", fixture.reference, fixture.reference.rootMove, "e1")
    val siblingLine = fixture.reference.copy(id = "sibling", role = LineNodeRole.Threat)
    val siblingThreat = threatRecord("sibling-threat", siblingLine, fixture.reference.rootMove, "h4")

    val exact = causeRecord(
      id = "exact-cause",
      fixture = fixture,
      kind = RelativeCauseKind.OnlyDefenseNecessity,
      sourceSide = RelativeCauseSourceSide.Reference,
      attribution = ownedReferenceAttribution,
      direct = exactThreat.ref
    )
    val wrongSource = causeRecord(
      id = "wrong-source-cause",
      fixture = fixture,
      kind = RelativeCauseKind.OnlyDefenseNecessity,
      sourceSide = RelativeCauseSourceSide.Candidate,
      attribution = CauseAttribution(
        CauseAttributionKind.CandidateCreatesValue,
        rootMoveMatched = true,
        directProofEligible = true
      ),
      direct = exactThreat.ref
    )
    val wrongAttribution = causeRecord(
      id = "wrong-attribution-cause",
      fixture = fixture,
      kind = RelativeCauseKind.OnlyDefenseNecessity,
      sourceSide = RelativeCauseSourceSide.Reference,
      attribution = CauseAttribution(
        CauseAttributionKind.ContextOnly,
        rootMoveMatched = true,
        directProofEligible = false
      ),
      direct = exactThreat.ref
    )
    val siblingProof = causeRecord(
      id = "sibling-proof-cause",
      fixture = fixture,
      kind = RelativeCauseKind.OnlyDefenseNecessity,
      sourceSide = RelativeCauseSourceSide.Reference,
      attribution = ownedReferenceAttribution,
      direct = siblingThreat.ref
    )

    val otherFixture = constraintFixture(
      comparisonId = "other-comparison",
      candidate = fixture.candidate.copy(id = "other-candidate", rootMove = "b2b3")
    )
    val otherComparisonCause = causeRecord(
      id = "other-comparison-cause",
      fixture = otherFixture,
      kind = RelativeCauseKind.OnlyDefenseNecessity,
      sourceSide = RelativeCauseSourceSide.Reference,
      attribution = ownedReferenceAttribution,
      direct = exactThreat.ref
    )

    val graph = graphOf(
      fixture.comparisonRecord,
      otherFixture.comparisonRecord,
      exactThreat,
      siblingThreat,
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
    assert(
      EvidenceObjectBinding
        .fromRelativeCauseForProjection(cause(exact, graph), graph)
        .filter(_.specificTargetMechanismReady)
        .forall(_.source.id == exactThreat.ref.id)
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

  test("exact only-defense survives while sibling and descendant evidence cannot be borrowed"):
    val fixture = constraintFixture()
    val exactThreat = threatRecord("defense-threat", fixture.reference, fixture.reference.rootMove, "e1")
    val exactDefense = causeRecord(
      id = "only-defense",
      fixture = fixture,
      kind = RelativeCauseKind.OnlyDefenseNecessity,
      sourceSide = RelativeCauseSourceSide.Reference,
      attribution = ownedReferenceAttribution,
      direct = exactThreat.ref
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
      parents = List(exactDefense.ref)
    )
    val graph = graphOf(fixture.comparisonRecord, exactThreat, exactDefense, descendant)
    val defenseCause = cause(exactDefense, graph)

    assertEquals(ClaimFamily.fromCause(RelativeCauseKind.OnlyDefenseNecessity), Some(ClaimFamily.Defensive))
    assert(defenseCause.hasOwnedTypedDepth(graph))
    assert(
      EvidenceObjectBinding.specificTargetMechanismReady(
        EvidenceObjectBinding.fromRelativeCauseForProjection(defenseCause, graph)
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
      List(defenseCause -> exactDefense.ref),
      playedMoves(fixture)
    )
    assertEquals(
      selected.flatMap(_.qualifiers).map(_.causeEvidence.id),
      List(exactDefense.ref.id)
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
      reference: LineNodeRef = LineNodeRef("best", "d1h5", 1, LineNodeRole.BestReference),
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

  private def threatRecord(
      id: String,
      line: LineNodeRef,
      onlyDefense: String,
      targetSquare: String
  ): EvidenceRecord =
    val square = Square.fromKey(targetSquare).getOrElse(fail(s"expected square $targetSquare"))
    val threat = Threat(
      threatActor = Black,
      kind = ThreatKind.Mate,
      turnsToImpact = 1,
      motifs = List(Motif.Check(Queen, square, Motif.CheckType.Normal, Black, 0, Some("d8h4"))),
      attackSquares = List(targetSquare),
      targetPieces = List("King"),
      bestDefense = Some(onlyDefense),
      defenseCount = 1
    )
    EvidenceRecord(
      ref = evidenceRef(
        id = id,
        producer = EvidenceProducer.ThreatPressureProducer,
        layer = EvidenceLayer.ThreatPressure,
        line = Some(line),
        confidence = EvidenceConfidence.LegalReplayVerified
      ),
      payload = ThreatEpisodeEvidence(ThreatEpisode.fromThreat(threat, 0))
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
      position = PositionNodeRef("4k3/8/8/8/8/8/8/3QK3 w - - 0 1", 0, Some(White)),
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
