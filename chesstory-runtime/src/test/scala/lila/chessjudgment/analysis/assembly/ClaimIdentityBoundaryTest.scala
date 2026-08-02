package lila.chessjudgment.analysis.assembly

import chess.{ Knight, Queen, Rook, White }
import lila.chessjudgment.analysis.policy.{ ClaimAdmissionDecision, ClaimAdmissionStatus, ClaimTruthPolicy }
import lila.chessjudgment.model.PlanEventIdentity
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.PrincipalVariationEvidence
import lila.chessjudgment.model.strategic.EngineLine
import lila.chessjudgment.model.strategic.PlanTaxonomy.PlanKind

class ClaimIdentityBoundaryTest extends munit.FunSuite:

  private val position = PositionNodeRef(
    "1r2k3/8/8/8/8/8/4Q3/4K3 w - - 0 1",
    0,
    Some(White)
  )
  private val reference = LineNodeRef("reference", "e2b5", 1, LineNodeRole.BestReference)
  private val played = LineNodeRef("played", "e2e3", 9, LineNodeRole.Played)

  test("exact PlanEvent Causes own one Plan host without contrast or context evidence"):
    assertEquals(ClaimFamily.fromCause(RelativeCauseKind.PlanImprovement), Some(ClaimFamily.Plan))
    assertEquals(ClaimFamily.fromCause(RelativeCauseKind.PlanContradiction), Some(ClaimFamily.Plan))
    List(
      (
        exactPlanCauseFixture("exact-plan-host", RelativeCauseKind.PlanImprovement),
        RelativeCauseKind.PlanImprovement,
        RelativeCauseSourceSide.Reference,
        CauseAttributionKind.ReferenceCreatesResource
      ),
      (
        exactPlanCauseFixture("exact-refuted-plan-host", RelativeCauseKind.PlanContradiction),
        RelativeCauseKind.PlanContradiction,
        RelativeCauseSourceSide.Candidate,
        CauseAttributionKind.CandidateAllowsLiability
      )
    ).foreach { case (fixture, kind, sourceSide, attribution) =>
      assertExactPlanCauseHost(fixture, kind, sourceSide, attribution)
    }

  test("exact F content reaches Ja, R, and packet through generic played endpoints"):
    val bestVsSecond = exactContentExecution(
      "e2e4",
      List(
        EngineLine(List("e2e4", "e7e5", "g1f3"), 30, depth = 18),
        EngineLine(List("d2d4", "d7d5", "g1f3"), 20, depth = 18),
        EngineLine(List("c2c4", "e7e5", "g1f3"), 10, depth = 18)
      )
    )
    List(
      exactContentExecution() -> CandidateComparisonKind.PlayedVsBest,
      bestVsSecond -> CandidateComparisonKind.BestVsSecond
    ).foreach { case (execution, expectedKind) =>
      val candidate = candidateContentClaim(execution.jp)
      assertEquals(candidateContent(candidate).comparison.kind, expectedKind)
      assertEquals(admissionStatus(execution, candidate), Some(ClaimAdmissionStatus.Certified))
      assertEquals(
        execution.r.selectedContentClaimIds.toSet,
        certifiedContentClaimIds(execution.ja.decisions)
      )
      assertEquals(execution.packet.map(_.selectedContentClaimIds), Some(execution.r.selectedContentClaimIds))
    }

    val execution = exactContentExecution()
    assertEquals(admissionStatus(execution, strategicContentClaim(execution.jp)), Some(ClaimAdmissionStatus.Certified))

  test("content certification fails closed for forged, borrowed, or ambiguous identities"):
    val execution = exactContentExecution()
    val graph = execution.c.evidenceGraph
    val candidate = candidateContentClaim(execution.jp)
    val candidateContentValue = candidateContent(candidate)
    val comparison = graph.record(candidateContentValue.carrierRef).collect {
      case EvidenceRecord(_, CandidateComparisonEvidence(fact), _) => fact
    }.getOrElse(fail("expected comparison carrier"))
    val originalComparisonRecord = graph.record(candidateContentValue.carrierRef)
      .getOrElse(fail("missing comparison record"))
    val alternate = List(comparison.referenceLine, comparison.candidateLine)
      .find(line => !EvidenceRef.sameMove(line.rootMove, candidate.subjectMove.getOrElse("")))
      .getOrElse(fail("expected non-played endpoint"))
    val rewrittenComparison = comparison.copy(
      referenceLine = alternate,
      candidateLine = LineNodeRef("forged-candidate-endpoint", "c2c4", 3, LineNodeRole.Alternative)
    )
    val graphWithRewrittenComparison = graphOf(graph.records.map {
      case record if record.ref == candidateContentValue.carrierRef =>
        originalComparisonRecord.copy(payload = CandidateComparisonEvidence(rewrittenComparison))
      case record => record
    })
    val strategic = strategicContentClaim(execution.jp)
    val strategicCarrier = strategic.content.collect {
      case JudgmentClaimContent.StrategicMechanism(ref) => ref
    }.getOrElse(fail("expected strategic carrier"))
    val wrongTypeCarrier = graph.records.collectFirst {
      case EvidenceRecord(ref, _: LineFactEvidence, _) => ref
    }.getOrElse(fail("expected line carrier"))
    val foreignCarrier = candidateContentValue.carrierRef.copy(confidence = EvidenceConfidence.Heuristic)
    val missingCarrier = candidateContentValue.carrierRef.copy(id = "missing-comparison-carrier")
    List(
      (
        "played endpoint rewrite",
        candidate.copy(subjectMove = Some(alternate.rootMove), primaryLine = Some(alternate)),
        graph,
        ClaimAdmissionStatus.Deferred
      ),
      (
        "coordinated comparison rewrite",
        candidate.copy(
          subjectMove = Some(alternate.rootMove),
          primaryLine = Some(alternate),
          content = Some(JudgmentClaimContent.CandidateComparison(
            candidateContentValue.carrierRef,
            CandidateComparisonSemanticKey.from(rewrittenComparison)
          ))
        ),
        graphWithRewrittenComparison,
        ClaimAdmissionStatus.Deferred
      ),
      (
        "foreign exact ref",
        candidate.copy(
          evidence = candidate.evidence.map(ref => if ref == candidateContentValue.carrierRef then foreignCarrier else ref),
          content = Some(JudgmentClaimContent.CandidateComparison(foreignCarrier, candidateContentValue.comparison))
        ),
        graph,
        ClaimAdmissionStatus.Rejected
      ),
      (
        "missing carrier",
        candidate.copy(
          evidence = candidate.evidence.map(ref => if ref == candidateContentValue.carrierRef then missingCarrier else ref),
          content = Some(JudgmentClaimContent.CandidateComparison(missingCarrier, candidateContentValue.comparison))
        ),
        graph,
        ClaimAdmissionStatus.Rejected
      ),
      (
        "wrong carrier payload",
        candidate.copy(
          content = Some(JudgmentClaimContent.CandidateComparison(wrongTypeCarrier, candidateContentValue.comparison))
        ),
        graph,
        ClaimAdmissionStatus.Deferred
      )
    ).foreach { case (name, claim, candidateGraph, expected) =>
      assertEquals(ClaimTruthPolicy.evaluate(claim, candidateGraph).status, expected, name)
    }

    assert(graph.records.exists(record => record.ref.position == strategicCarrier.position && record.ref.layer == EvidenceLayer.Board))
    val lineOrEval = strategic.evidence.collectFirst {
      case ref if graph.record(ref).exists(_.payload.isInstanceOf[EvalFactEvidence]) => ref
    }.getOrElse(fail("expected exact Eval ref"))
    val incomplete = graphOf(graph.records.filterNot(_.ref == lineOrEval))
    val proposed = JudgmentClaimAssembler.propose(execution.c.copy(evidenceGraph = incomplete))
      .filter(_.content.contains(JudgmentClaimContent.StrategicMechanism(strategicCarrier)))
    assertEquals(proposed.size, 1)
    assert(!proposed.head.evidence.exists(ref => ref.layer == EvidenceLayer.Board || ref.layer == EvidenceLayer.OpeningContext))
    assertEquals(ClaimTruthPolicy.evaluate(proposed.head, incomplete).status, ClaimAdmissionStatus.Deferred)

    val duplicateCarrier = candidateContentValue.carrierRef.copy(id = "same-comparison-different-carrier")
    val graphWithDuplicate = graph.add(originalComparisonRecord.copy(ref = duplicateCarrier))
    val distinctCarrierClaim = candidate.copy(
      id = "same-comparison-different-carrier",
      evidence = List(duplicateCarrier),
      content = Some(JudgmentClaimContent.CandidateComparison(duplicateCarrier, candidateContentValue.comparison))
    )
    val deduplicated = ClaimDeduplicator.deduplicateDetailed(
      List(
        ClaimTruthPolicy.evaluate(candidate, graphWithDuplicate),
        ClaimTruthPolicy.evaluate(distinctCarrierClaim, graphWithDuplicate)
      ),
      graphWithDuplicate
    )

    assertEquals(deduplicated.decisions.map(_.claim.content).toSet, Set(candidate.content, distinctCarrierClaim.content))
    assertEquals(ClaimArbitrator.contentClaimIds(deduplicated.decisions), Nil)

  private val exactContentVariations = List(
    EngineLine(List("e2e4", "e7e5", "g1f3"), 30, depth = 18),
    EngineLine(List("d2d4", "d7d5", "g1f3"), 20, depth = 18)
  )

  private def exactContentExecution(
      playedMove: String = "d2d4",
      variations: List[EngineLine] = exactContentVariations
  ) =
    MoveReviewJudgmentOrchestrator
      .execute(
        RawMoveReviewInput(
          fen = chess.variant.Standard.initialFen.value,
          playedMoveUci = playedMove,
          variations = variations,
          ply = Some(0)
        ),
        JudgmentBoundaryIntervention.identity
      )
      .getOrElse(fail("expected closed core execution"))

  private def candidateContentClaim(claims: List[JudgmentClaim]): JudgmentClaim =
    claims.find { claim =>
      claim.family == ClaimFamily.Evaluation &&
      claim.subject == ClaimSubject.PlayedMove &&
      claim.content.exists {
        case JudgmentClaimContent.CandidateComparison(_, _) => true
        case _                                              => false
      }
    }.getOrElse(fail("expected exact comparison content proposal"))

  private def admissionStatus(
      execution: JudgmentBoundaryExecution,
      claim: JudgmentClaim
  ): Option[ClaimAdmissionStatus] =
    execution.ja.decisions.find(_.claim.id == claim.id).map(_.status)

  private def certifiedContentClaimIds(
      decisions: List[ClaimAdmissionDecision]
  ): Set[String] =
    decisions.collect {
      case ClaimAdmissionDecision(claim, ClaimAdmissionStatus.Certified, _, _, _) =>
        claim.content.map(_ => claim.id)
    }.flatten.toSet

  private def candidateContent(claim: JudgmentClaim) =
    claim.content.collect {
      case content @ JudgmentClaimContent.CandidateComparison(_, _) => content
    }.getOrElse(fail("expected comparison content identity"))

  private def strategicContentClaim(claims: List[JudgmentClaim]): JudgmentClaim =
    claims.find(_.content.exists {
      case JudgmentClaimContent.StrategicMechanism(_) => true
      case _                                                   => false
    }).getOrElse(fail("expected exact strategic wrapper content proposal"))

  private def assertExactPlanCauseHost(
      fixture: ExactPlanCauseFixture,
      expectedKind: RelativeCauseKind,
      expectedSourceSide: RelativeCauseSourceSide,
      expectedAttribution: CauseAttributionKind
  ): Unit =
    val graph = graphOf(fixture.records)
    val registeredCause = graph.record(fixture.causeRef).collect {
      case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) => cause
    }.getOrElse(fail("expected registered exact Plan Cause"))
    val event = graph.record(fixture.eventRef).collect {
      case EvidenceRecord(_, event: PlanCausalEventEvidence, _) => event
    }.getOrElse(fail("expected exact PlanEvent"))
    val context = JudgmentAssemblyContext(
      input = fixture.input,
      positions = Nil,
      lines = Nil,
      transitions = Nil,
      evidenceGraph = graph,
      claims = Nil
    )

    assertEquals(registeredCause.kind, expectedKind)
    assertEquals(registeredCause.sourceSide, expectedSourceSide)
    assertEquals(registeredCause.attribution.kind, expectedAttribution)
    if expectedKind == RelativeCauseKind.PlanImprovement then
      assert(event.exactRobustPublicResultAssessment.nonEmpty)
    else
      assert(event.exactRefutedPublicResultAssessment.nonEmpty)
    assert(PlayerFacingCauseReadinessPolicy.ready(registeredCause, fixture.causeRef, graph))

    val hosted = JudgmentClaimAssembler
      .propose(context)
      .filter(_.evidence.exists(_.id == fixture.causeRef.id))
    assertEquals(hosted.size, 1)
    val claim = hosted.head
    assertEquals(claim.family, ClaimFamily.Plan)
    assertEquals(claim.evidence.map(_.id), List(fixture.causeRef.id, fixture.eventRef.id))
    assert(!claim.evidence.exists(_.id == fixture.contrastRef.id))
    assert(!claim.evidence.exists(_.id == fixture.contextRef.id))

    val admission = ClaimCandidateGraphAssembler.fromClaims(hosted, context.evidenceGraph)
    assertEquals(admission.decisions.map(_.status), List(ClaimAdmissionStatus.Certified))
    assertEquals(
      PlayerFacingCauseReadinessPolicy.collect(claim, graph).map(_._2.id),
      List(fixture.causeRef.id)
    )

  test("MaterialSwing proposes exactly one Material family even with tactical conversion-shaped proof"):
    val proof = hangingRelation("material-proof")
    val fixture = causeFixture(
      id = "material",
      kind = RelativeCauseKind.MaterialSwing,
      directProof = proof,
      contrastProof = None
    )
    val graph = graphOf(fixture.records)
    val context = JudgmentAssemblyContext(
      input = normalizedInput,
      positions = Nil,
      lines = Nil,
      transitions = Nil,
      evidenceGraph = graph,
      claims = Nil
    )
    val families = JudgmentClaimAssembler
      .propose(context)
      .filter(_.evidence.exists(_.id == fixture.causeRef.id))
      .map(_.family)

    assertEquals(families, List(ClaimFamily.Material))

  test("MaterialSwing cannot certify a Tactical claim through its owned relation proof"):
    val proof = hangingRelation("material-tactical-proof")
    val fixture = causeFixture(
      id = "material-tactical",
      kind = RelativeCauseKind.MaterialSwing,
      directProof = proof,
      contrastProof = None
    )
    val graph = graphOf(fixture.records)
    val tactical = JudgmentClaim(
      id = "material-tactical-host",
      family = ClaimFamily.Tactical,
      subject = ClaimSubject.ReferenceMove,
      primaryPosition = position,
      primaryLine = Some(reference),
      subjectMove = Some(reference.rootMove),
      evidence = List(fixture.causeRef, proof.ref, fixture.comparisonRef),
      scope = EvidenceScope.BestLine,
      confidence = EvidenceConfidence.EngineBacked
    )

    assertEquals(ClaimTruthPolicy.evaluate(tactical, graph).status, ClaimAdmissionStatus.Deferred)

    val independentRelationClaim = tactical.copy(
      id = "independent-relation-tactical",
      evidence = List(proof.ref, fixture.comparisonRef)
    )
    assertEquals(
      ClaimTruthPolicy.evaluate(independentRelationClaim, graph).status,
      ClaimAdmissionStatus.Certified
    )

  test("a ready non-material Cause keeps its owned tactical-proof path"):
    val proof = mateRelation("strategic-owned-tactical-proof")
    val fixture = causeFixture(
      id = "strategic-owned-tactical",
      kind = RelativeCauseKind.KingForcing,
      directProof = proof,
      contrastProof = None
    )
    val graph = graphOf(fixture.records)
    val tactical = JudgmentClaim(
      id = "strategic-owned-tactical-host",
      family = ClaimFamily.Tactical,
      subject = ClaimSubject.ReferenceMove,
      primaryPosition = position,
      primaryLine = Some(reference),
      subjectMove = Some(reference.rootMove),
      evidence = List(fixture.causeRef, proof.ref, fixture.comparisonRef),
      scope = EvidenceScope.BestLine,
      confidence = EvidenceConfidence.EngineBacked
    )

    assertEquals(
      PlayerFacingCauseReadinessPolicy.collect(tactical, graph).map(_._2.id),
      List(fixture.causeRef.id)
    )
    assertEquals(ClaimTruthPolicy.evaluate(tactical, graph).status, ClaimAdmissionStatus.Certified)

  test("Ja cannot certify a Cause host with contrast proof standing in for empty direct proof"):
    val contrast = mateRelation("contrast-only-proof")
    val base = causeFixture(
      id = "contrast-only",
      kind = RelativeCauseKind.KingForcing,
      directProof = contrast,
      contrastProof = None
    )
    val records = base.records.map {
      case record @ EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) =>
        record.copy(
          payload = RelativeCauseFactEvidence(
            cause.copy(
              proof = Some(RelativeCauseProof(
                directProof = RelativeCauseProofSection(
                  RelativeCauseProofRole.DirectProof,
                  RelativeCauseProofStrength.Primary,
                  Nil
                ),
                contrastProof = RelativeCauseProofSection(
                  RelativeCauseProofRole.ContrastProof,
                  RelativeCauseProofStrength.Supporting,
                  List(contrast.ref)
                )
              ))
            )
          ),
          parents = List(base.comparisonRef, contrast.ref)
        )
      case record =>
        record
    }
    val graph = graphOf(records)
    val tactical = JudgmentClaim(
      id = "contrast-only-host",
      family = ClaimFamily.Tactical,
      subject = ClaimSubject.ReferenceMove,
      primaryPosition = position,
      primaryLine = Some(reference),
      subjectMove = Some(reference.rootMove),
      evidence = List(base.causeRef, contrast.ref, base.comparisonRef),
      scope = EvidenceScope.BestLine,
      confidence = EvidenceConfidence.EngineBacked
    )

    assertEquals(PlayerFacingCauseReadinessPolicy.collect(tactical, graph), Nil)
    assertEquals(ClaimTruthPolicy.evaluate(tactical, graph).status, ClaimAdmissionStatus.Deferred)

  test("claim dedup keeps a ready direct Cause when semantic duplicate IDs collide"):
    val proof = mateRelation("duplicate-proof")
    val fixture = causeFixture(
      id = "duplicate",
      kind = RelativeCauseKind.KingForcing,
      directProof = proof,
      contrastProof = None
    )
    val causeRecord = fixture.records.collectFirst {
      case record @ EvidenceRecord(_, RelativeCauseFactEvidence(_), _) => record
    }.getOrElse(fail("expected Cause record"))
    val duplicateRef = fixture.causeRef.copy(id = "duplicate-cause-copy")
    val duplicateRecord = causeRecord.copy(ref = duplicateRef)
    val graph = graphOf(fixture.records :+ duplicateRecord)
    def claim(id: String, causeRef: EvidenceRef): JudgmentClaim =
      JudgmentClaim(
        id = id,
        family = ClaimFamily.Tactical,
        subject = ClaimSubject.ReferenceMove,
        primaryPosition = position,
        primaryLine = Some(reference),
        subjectMove = Some(reference.rootMove),
        evidence = List(causeRef, proof.ref, fixture.comparisonRef),
        scope = EvidenceScope.BestLine,
        confidence = EvidenceConfidence.EngineBacked
      )
    val first = ClaimTruthPolicy.evaluate(claim("duplicate-first", fixture.causeRef), graph)
    val second = ClaimTruthPolicy.evaluate(claim("duplicate-second", duplicateRef), graph)

    assertEquals(first.status, ClaimAdmissionStatus.Certified)
    assertEquals(second.status, ClaimAdmissionStatus.Certified)
    val result = ClaimDeduplicator.deduplicateDetailed(List(first, second), graph)
    assertEquals(result.decisions.map(_.claim.id), List("duplicate-first"))
    assertEquals(
      result.trace.map(trace => (trace.originalClaimId, trace.keptClaimId, trace.reason)),
      List(("duplicate-second", "duplicate-first", ClaimDeduplicationReason.SameRankKeyLowerScore))
    )
    assertEquals(
      PlayerFacingCauseReadinessPolicy.collect(result.decisions.head.claim, graph).map(_._2.id),
      List(fixture.causeRef.id)
    )
    val ranking = ClaimArbitrator.rankDetailed(
      ClaimCandidateGraph(List(first, second), graph),
      List(relativeAssessment)
    )
    val disposition = ranking.causeDispositionLedger.byCauseEvidenceId(duplicateRef.id)
    assertEquals(disposition.status, CauseDispositionStatus.Redundant)
    assertEquals(disposition.reason, CauseDispositionReason.CertifiedClaimDeduplicated)
    assertEquals(disposition.certifiedClaimIds, List("duplicate-second"))
    assertEquals(disposition.rankEligibleClaimIds, Nil)
    assertEquals(disposition.relatedClaimIds, List("duplicate-first"))

  test("tactical wrapper cannot own a primitive through a forged same-ID source ref"):
    val primitive = mateRelation("wrapper-primitive")
    val fixture = causeFixture(
      id = "wrapper-source",
      kind = RelativeCauseKind.MissedTacticalResource,
      directProof = primitive,
      contrastProof = None
    )
    val forgedPrimitiveRef = primitive.ref.copy(confidence = EvidenceConfidence.Heuristic)
    val mechanismRef = ref(
      "wrapper-mechanism",
      EvidenceProducer.TacticalMechanismProducer,
      EvidenceLayer.TacticalMechanism,
      Some(reference),
      EvidenceConfidence.LegalReplayVerified
    )
    val mechanism = EvidenceRecord(
      mechanismRef,
      TacticalMechanismEvidence(
        kind = TacticalMechanismKind.KingForcing,
        moveUci = Some(reference.rootMove),
        line = Some(reference),
        signals = List(TacticalMechanismSignal(
          kind = TacticalMechanismSignalKind.Relation,
          label = "mate-net",
          sourceLayer = EvidenceLayer.Relation,
          source = Some(forgedPrimitiveRef),
          relationKind = Some(RelationFactKind.MateNet)
        ))
      ),
      parents = List(forgedPrimitiveRef)
    )
    val graph = graphOf(fixture.records :+ mechanism)
    val cause = graph.record(fixture.causeRef).collect {
      case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) => cause
    }.getOrElse(fail("expected registered Cause"))
    val comparison = graph.record(fixture.comparisonRef).collect {
      case EvidenceRecord(_, CandidateComparisonEvidence(comparison), _) => comparison
    }.getOrElse(fail("expected registered comparison"))
    val binding = graph.requiredRelativeCauseBinding(cause)

    assertEquals(
      RootOwnedCausePolicy
        .directProofRecords(
          graph = graph,
          fact = comparison,
          kind = RelativeCauseKind.MissedTacticalResource,
          binding = binding,
          attributionKind = CauseAttributionKind.ReferenceCreatesResource,
          records = List(primitive, mechanism)
        )
        .map(_.ref.id),
      List(primitive.ref.id)
    )

  test("Ja rejects ready Cause hosts with a forged subject or line binding"):
    List(
      (
        "unrelated-played-host",
        mateRelation("unrelated-host-proof"),
        ClaimSubject.PlayedMove,
        played,
        EvidenceScope.PlayedLine
      ),
      (
        "same-move-other-line-host",
        mateRelation("same-move-other-line-proof"),
        ClaimSubject.CandidateLine,
        LineNodeRef("same-move-alternative", reference.rootMove, 2, LineNodeRole.Alternative),
        EvidenceScope.CandidateLine
      )
    ).foreach { case (id, proof, subject, line, scope) =>
      val fixture = causeFixture(
        id = id,
        kind = RelativeCauseKind.KingForcing,
        directProof = proof,
        contrastProof = None
      )
      val graph = graphOf(fixture.records)
      val claim = JudgmentClaim(
        id = id,
        family = ClaimFamily.Tactical,
        subject = subject,
        primaryPosition = position,
        primaryLine = Some(line),
        subjectMove = Some(line.rootMove),
        evidence = List(fixture.causeRef, proof.ref, fixture.comparisonRef),
        scope = scope,
        confidence = EvidenceConfidence.EngineBacked
      )

      assertEquals(PlayerFacingCauseReadinessPolicy.collect(claim, graph).map(_._2.id), List(fixture.causeRef.id))
      assertEquals(ClaimTruthPolicy.evaluate(claim, graph).status, ClaimAdmissionStatus.Rejected)
    }

  test("Ja does not use a context-only parent of a direct carrier as family proof"):
    val contextRef = ref(
      "context-only-material-line",
      EvidenceProducer.LegalLineProducer,
      EvidenceLayer.Line,
      Some(reference),
      EvidenceConfidence.LegalReplayVerified
    )
    val afterRoot = legalAfter(reference.rootMove)
    val replyMove = "e8f7"
    val afterReply = PrincipalVariationEvidence
      .legalFenAfter(afterRoot, replyMove)
      .getOrElse(fail(s"expected legal reply $replyMove"))
    val captureMove = "b5b8"
    val afterCapture = PrincipalVariationEvidence
      .legalFenAfter(afterReply, captureMove)
      .getOrElse(fail(s"expected legal capture $captureMove"))
    val contextRecord = EvidenceRecord(
      contextRef,
      LineFactEvidence(
        line = reference,
        material = Some(LineMaterialSummary(
          sideToMove = White,
          captures = List(LineMaterialCapture(
            moveUci = captureMove,
            plyOffset = 2,
            side = White,
            attackerRole = EvidencePieceRole(Queen.name),
            capturedRole = EvidencePieceRole(Rook.name),
            square = EvidenceSquare("b8"),
            valueCp = 500,
            recapture = false
          )),
          netCaptureCpForMover = 500,
          maxGainCpForMover = 500,
          maxLossCpForMover = 0,
          hasRecaptureChain = false,
          hasRecoveryWindow = false,
          promotionGainCpForMover = 0,
          materialWindowComplete = true
        )),
        replay = List(
          LineReplayStep(0, reference.rootMove, position.fen, afterRoot),
          LineReplayStep(1, replyMove, afterRoot, afterReply),
          LineReplayStep(2, captureMove, afterReply, afterCapture)
        ),
        consequences = List(LineConsequence(
          kind = LineConsequenceKind.MaterialGain,
          lineMoves = List(reference.rootMove, replyMove, captureMove),
          proofSignal = true,
          eventMove = Some(captureMove),
          rootMove = Some(reference.rootMove),
          rootSide = Some(White),
          beneficiary = Some(White)
        ))
      )
    )
    val direct = hangingRelation("direct-carrier-with-context-parent").copy(parents = List(contextRef))
    val fixture = causeFixture(
      id = "direct-parent-isolation",
      kind = RelativeCauseKind.MaterialSwing,
      directProof = direct,
      contrastProof = None
    )
    val graph = graphOf(fixture.records :+ contextRecord)
    val claim = JudgmentClaim(
      id = "direct-parent-isolation-host",
      family = ClaimFamily.Material,
      subject = ClaimSubject.ReferenceMove,
      primaryPosition = position,
      primaryLine = Some(reference),
      subjectMove = Some(reference.rootMove),
      evidence = List(fixture.causeRef, direct.ref, fixture.comparisonRef),
      scope = EvidenceScope.BestLine,
      confidence = EvidenceConfidence.EngineBacked
    )

    assertEquals(
      PlayerFacingCauseReadinessPolicy.collect(claim, graph).map(_._2.id),
      List(fixture.causeRef.id)
    )
    val decision = ClaimTruthPolicy.evaluate(claim, graph)
    assertEquals(decision.status, ClaimAdmissionStatus.Deferred)
    assert(!decision.presentLayers(EvidenceLayer.Line))

  private final case class CauseFixture(
      causeRef: EvidenceRef,
      comparisonRef: EvidenceRef,
      records: List[EvidenceRecord]
  )

  private final case class ExactPlanCauseFixture(
      causeRef: EvidenceRef,
      eventRef: EvidenceRef,
      contrastRef: EvidenceRef,
      contextRef: EvidenceRef,
      records: List[EvidenceRecord],
      input: NormalizedMoveReviewInput
  )

  private def exactPlanCauseFixture(
      id: String,
      kind: RelativeCauseKind
  ): ExactPlanCauseFixture =
    require(RelativeCauseKind.requiresExactPlanResult(kind))
    val refuted = kind == RelativeCauseKind.PlanContradiction
    val rootFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    val rootPosition = PositionNodeRef(rootFen, 0, Some(White))
    val rootStep = legalStep(rootFen, "b1c3", 1)
    val replyStep = legalStep(rootStep.fenAfter, "a7a6", 2)
    val resultStep = legalStep(replyStep.fenAfter, "c3d5", 3)
    val planLine = LineNodeRef(
      s"$id-plan",
      rootStep.moveUci,
      1,
      if refuted then LineNodeRole.Played else LineNodeRole.BestReference
    )
    val quietLine = LineNodeRef(
      s"$id-quiet",
      "a2a3",
      1,
      if refuted then LineNodeRole.BestReference else LineNodeRole.Played
    )
    val referenceLine = if refuted then quietLine else planLine
    val playedLine = if refuted then planLine else quietLine
    val eventLine = planLine
    val transitionRole = if refuted then TransitionEdgeRole.Played else TransitionEdgeRole.Reference
    val sourceSide =
      if refuted then RelativeCauseSourceSide.Candidate else RelativeCauseSourceSide.Reference
    val attributionKind =
      if refuted then CauseAttributionKind.CandidateAllowsLiability
      else CauseAttributionKind.ReferenceCreatesResource
    val afterRoot = PositionNodeRef(rootStep.fenAfter, 1, None)

    def localRef(
        suffix: String,
        producer: EvidenceProducer,
        layer: EvidenceLayer,
        line: Option[LineNodeRef],
        scope: EvidenceScope,
        confidence: EvidenceConfidence = EvidenceConfidence.EngineBacked
    ): EvidenceRef =
      EvidenceRef(s"$id-$suffix", producer, layer, rootPosition, line, scope, confidence)

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
      .getOrElse(fail("expected exact PlanEvent trajectory"))
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
        sourceProbeId = s"$id-probe",
        line = LineNodeRef(s"$id-reply-$index", move, index + 1, LineNodeRole.Threat),
        outcome = if refuted then PlanCausalBranchOutcome.Refuted else PlanCausalBranchOutcome.Realized,
        observedEpisode = Option.unless(refuted)(episode),
        observedConsequences = Option.unless(refuted)(List(consequence)).getOrElse(Nil),
        realizationMatch = Option.unless(refuted)(PlanCausalRealizationMatch.ExactMove),
        realizationMove = Option.unless(refuted)(resultNode.moveUci),
        requiredPlyOffset = 2,
        certifiedHorizonPlyOffset = 2,
        observedPlyOffset = 2,
        observedThroughPlyOffset = 2,
        terminalOutcome = Option.when(refuted)(PlanCausalTerminalOutcome.Defeat),
        terminalPlyOffset = Option.when(refuted)(2),
        terminalStep = None
      )
    }
    val event = PlanCausalEventEvidence(
      StructuralTransitionBinding(
        rootStep.moveUci,
        transitionRole,
        rootPosition,
        afterRoot,
        Some(eventLine),
        White
      ),
      episode,
      witnesses
    )
    if refuted then assert(event.exactRefutedPublicResultAssessment.nonEmpty)
    else assert(event.exactRobustPublicResultAssessment.nonEmpty)
    val eventRef = localRef(
      "event",
      EvidenceProducer.PlanCausalEventProducer,
      EvidenceLayer.PlanCausalEvent,
      Some(eventLine),
      if refuted then EvidenceScope.PlayedTransition else EvidenceScope.ReferenceTransition
    )
    val contrastRef = localRef(
      "contrast",
      EvidenceProducer.EngineEvalProducer,
      EvidenceLayer.Eval,
      Some(eventLine),
      if refuted then EvidenceScope.PlayedLine else EvidenceScope.BestLine
    )
    val contextRef = localRef(
      "context",
      EvidenceProducer.EngineEvalProducer,
      EvidenceLayer.Eval,
      Some(eventLine),
      if refuted then EvidenceScope.PlayedLine else EvidenceScope.BestLine
    )
    val comparisonRef = localRef(
      "comparison",
      EvidenceProducer.RelativeMoveProducer,
      EvidenceLayer.CandidateComparison,
      Some(playedLine),
      EvidenceScope.Counterfactual
    )
    val referenceEvalRef = localRef(
      "reference-eval",
      EvidenceProducer.EngineEvalProducer,
      EvidenceLayer.Eval,
      Some(referenceLine),
      EvidenceScope.BestLine
    )
    val playedEvalRef = localRef(
      "played-eval",
      EvidenceProducer.EngineEvalProducer,
      EvidenceLayer.Eval,
      Some(playedLine),
      EvidenceScope.PlayedLine
    )
    val comparison = CandidateComparisonFact(
      CandidateComparisonKind.PlayedVsBest,
      referenceLine,
      playedLine,
      EvalComparison.fromLines(
        White,
        CandidateLineNode(
          referenceLine,
          EngineLine(List(referenceLine.rootMove), 300, depth = 18),
          referenceEvalRef
        ),
        CandidateLineNode(
          playedLine,
          EngineLine(List(playedLine.rootMove), -300, depth = 18),
          playedEvalRef
        )
      )
    )
    val cause = RelativeCauseFact(
      kind = kind,
      comparisonEvidence = comparisonRef,
      supportEvidence = List(eventRef, contrastRef, contextRef),
      sourceSide = sourceSide,
      attribution = CauseAttribution(
        attributionKind,
        rootMoveMatched = true,
        directProofEligible = true
      ),
      proof = Some(RelativeCauseProof(
        directProof = RelativeCauseProofSection(
          RelativeCauseProofRole.DirectProof,
          RelativeCauseProofStrength.Primary,
          List(eventRef)
        ),
        contrastProof = RelativeCauseProofSection(
          RelativeCauseProofRole.ContrastProof,
          RelativeCauseProofStrength.Supporting,
          List(contrastRef)
        ),
        contextSupport = RelativeCauseProofSection(
          RelativeCauseProofRole.ContextSupport,
          RelativeCauseProofStrength.WeakHint,
          List(contextRef)
        )
      ))
    )
    val causeRef = localRef(
      "cause",
      EvidenceProducer.RelativeMoveProducer,
      EvidenceLayer.RelativeCause,
      Some(eventLine),
      EvidenceScope.Counterfactual
    )
    val records = List(
      EvidenceRecord(comparisonRef, CandidateComparisonEvidence(comparison)),
      EvidenceRecord(eventRef, event),
      EvidenceRecord(contrastRef, EvalFactEvidence(eventLine, 30, None, 18)),
      EvidenceRecord(contextRef, EvalFactEvidence(eventLine, 20, None, 18)),
      EvidenceRecord(
        causeRef,
        RelativeCauseFactEvidence(cause),
        parents = List(comparisonRef, eventRef, contrastRef, contextRef)
      )
    )
    ExactPlanCauseFixture(
      causeRef,
      eventRef,
      contrastRef,
      contextRef,
      records,
      NormalizedMoveReviewInput(
        beforeFen = rootFen,
        playedMoveUci = playedLine.rootMove,
        beforePly = rootPosition.ply,
        sideToMove = rootPosition.sideToMove,
        afterPlayedFen = PrincipalVariationEvidence
          .legalFenAfter(rootFen, playedLine.rootMove)
          .getOrElse(fail("expected legal played plan fixture move")),
        afterReferenceFen = Some(
          PrincipalVariationEvidence
            .legalFenAfter(rootFen, referenceLine.rootMove)
            .getOrElse(fail("expected legal reference plan fixture move"))
        ),
        lines = Nil,
        opening = None
      )
    )

  private def causeFixture(
      id: String,
      kind: RelativeCauseKind,
      directProof: EvidenceRecord,
      contrastProof: Option[EvidenceRecord]
  ): CauseFixture =
    val comparisonRef = ref(
      s"$id-comparison",
      EvidenceProducer.RelativeMoveProducer,
      EvidenceLayer.CandidateComparison,
      Some(played),
      EvidenceConfidence.EngineBacked
    )
    val comparison = CandidateComparisonFact(
      CandidateComparisonKind.PlayedVsBest,
      reference,
      played,
      EvalComparison.fromLines(
        White,
        CandidateLineNode(
          reference,
          EngineLine(List(reference.rootMove), 300, depth = 18),
          ref(
            s"$id-reference-eval",
            EvidenceProducer.EngineEvalProducer,
            EvidenceLayer.Eval,
            Some(reference),
            EvidenceConfidence.EngineBacked
          )
        ),
        CandidateLineNode(
          played,
          EngineLine(List(played.rootMove), -300, depth = 18),
          ref(
            s"$id-played-eval",
            EvidenceProducer.EngineEvalProducer,
            EvidenceLayer.Eval,
            Some(played),
            EvidenceConfidence.EngineBacked
          )
        )
      )
    )
    val cause = RelativeCauseFact(
      kind = kind,
      comparisonEvidence = comparisonRef,
      supportEvidence = directProof.ref :: contrastProof.toList.map(_.ref),
      sourceSide = RelativeCauseSourceSide.Reference,
      attribution = CauseAttribution(
        CauseAttributionKind.ReferenceCreatesResource,
        rootMoveMatched = true,
        directProofEligible = true
      ),
      proof = Some(RelativeCauseProof(
        directProof = RelativeCauseProofSection(
          RelativeCauseProofRole.DirectProof,
          RelativeCauseProofStrength.Primary,
          List(directProof.ref)
        ),
        contrastProof = RelativeCauseProofSection(
          RelativeCauseProofRole.ContrastProof,
          RelativeCauseProofStrength.Supporting,
          contrastProof.toList.map(_.ref)
        )
      ))
    )
    val causeRef = ref(
      s"$id-cause",
      EvidenceProducer.RelativeMoveProducer,
      EvidenceLayer.RelativeCause,
      Some(reference),
      EvidenceConfidence.EngineBacked
    )
    CauseFixture(
      causeRef,
      comparisonRef,
      List(
        EvidenceRecord(comparisonRef, CandidateComparisonEvidence(comparison)),
        directProof
      ) ++ contrastProof.toList ++ List(
        EvidenceRecord(causeRef, RelativeCauseFactEvidence(cause))
      )
    )

  private def admitted(claim: JudgmentClaim): ClaimAdmissionDecision =
    ClaimAdmissionDecision(
      claim,
      ClaimAdmissionStatus.Certified,
      claim.evidence.map(_.layer).toSet,
      Nil,
      Nil
    )

  private def relativeAssessment: RelativeMoveAssessment =
    val after = PositionNodeRef(legalAfter(played.rootMove), 1, None)
    val playedEvidence = ref(
      "played-line",
      EvidenceProducer.LegalLineProducer,
      EvidenceLayer.Line,
      Some(played),
      EvidenceConfidence.LegalReplayVerified
    )
    val referenceEvidence = ref(
      "reference-line",
      EvidenceProducer.LegalLineProducer,
      EvidenceLayer.Line,
      Some(reference),
      EvidenceConfidence.LegalReplayVerified
    )
    RelativeMoveAssessment(
      played = MoveTransitionEdge(
        TransitionEdgeRole.Played,
        position,
        played.rootMove,
        after,
        ref(
          "played-transition",
          EvidenceProducer.MoveTransitionProducer,
          EvidenceLayer.MoveTransition,
          Some(played),
          EvidenceConfidence.LegalReplayVerified
        )
      ),
      referenceTransition = None,
      reference = CandidateLineNode(
        reference,
        EngineLine(List(reference.rootMove), 300, depth = 18),
        referenceEvidence
      ),
      candidate = CandidateLineNode(
        played,
        EngineLine(List(played.rootMove), -300, depth = 18),
        playedEvidence
      ),
      evidence = ref(
        "assessment",
        EvidenceProducer.RelativeMoveProducer,
        EvidenceLayer.RelativeAssessment,
        Some(played),
        EvidenceConfidence.EngineBacked
      ),
      primaryComparisonEvidence = ref(
        "forcing-comparison",
        EvidenceProducer.RelativeMoveProducer,
        EvidenceLayer.CandidateComparison,
        Some(played),
        EvidenceConfidence.EngineBacked
      )
    )

  private def hangingRelation(id: String): EvidenceRecord =
    relationRecord(
      id,
      RelationWitnessDetail.HangingPiece(
        EvidenceSquare("b5"),
        EvidenceSquare("b8"),
        EvidencePieceRole(Queen.name),
        EvidencePieceRole(Rook.name)
      )
    )

  private def mateRelation(id: String): EvidenceRecord =
    relationRecord(
      id,
      RelationWitnessDetail.MatePattern(
        relationKind = "mate_net",
        kingSquare = EvidenceSquare("e8"),
        checkerSquares = List(EvidenceSquare("b5")),
        matingMove = reference.rootMove,
        patternId = Some(id)
      )
    )

  private def relationRecord(id: String, detail: RelationWitnessDetail): EvidenceRecord =
    EvidenceRecord(
      ref(
        id,
        EvidenceProducer.TacticalRelationProducer,
        EvidenceLayer.Relation,
        Some(reference),
        EvidenceConfidence.LegalReplayVerified
      ),
      RelationFactEvidence
        .from(detail, List(reference.rootMove))
        .getOrElse(fail("expected typed relation evidence"))
    )

  private def normalizedInput: NormalizedMoveReviewInput =
    NormalizedMoveReviewInput(
      beforeFen = position.fen,
      playedMoveUci = played.rootMove,
      beforePly = position.ply,
      sideToMove = position.sideToMove,
      afterPlayedFen = legalAfter(played.rootMove),
      afterReferenceFen = Some(legalAfter(reference.rootMove)),
      lines = Nil,
      opening = None
    )

  private def legalAfter(move: String): String =
    PrincipalVariationEvidence
      .legalFenAfter(position.fen, move)
      .getOrElse(fail(s"expected legal move $move"))

  private def legalStep(fen: String, move: String, ply: Int): LineReplayStep =
    LineReplayStep(
      ply,
      move,
      fen,
      PrincipalVariationEvidence.legalFenAfter(fen, move).getOrElse(fail(s"expected legal $move"))
    )

  private def ref(
      id: String,
      producer: EvidenceProducer,
      layer: EvidenceLayer,
      line: Option[LineNodeRef],
      confidence: EvidenceConfidence
  ): EvidenceRef =
    EvidenceRef(
      id,
      producer,
      layer,
      position,
      line,
      line.map(_.role.scope).getOrElse(EvidenceScope.Counterfactual),
      confidence
    )

  private def graphOf(records: List[EvidenceRecord]): TypedEvidenceGraph =
    ExplicitCauseAdmissionTestSupport.graph(records)
