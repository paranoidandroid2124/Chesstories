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

  test("an exact PlanEvent Cause owns one Plan host without contrast or context evidence"):
    val fixture = exactPlanCauseFixture("exact-plan-host", RelativeCauseKind.PlanImprovement)
    assertEquals(ClaimFamily.fromCause(RelativeCauseKind.PlanImprovement), Some(ClaimFamily.Plan))
    assertEquals(ClaimFamily.fromCause(RelativeCauseKind.PlanContradiction), Some(ClaimFamily.Plan))
    assertExactPlanCauseHost(
      fixture,
      RelativeCauseKind.PlanImprovement,
      RelativeCauseSourceSide.Reference,
      CauseAttributionKind.ReferenceCreatesResource
    )

  test("an exact refuted candidate PlanEvent owns one PlanContradiction host"):
    assertExactPlanCauseHost(
      exactPlanCauseFixture("exact-refuted-plan-host", RelativeCauseKind.PlanContradiction),
      RelativeCauseKind.PlanContradiction,
      RelativeCauseSourceSide.Candidate,
      CauseAttributionKind.CandidateAllowsLiability
    )

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

  test("Jp hosts a constructed Cause while Ja evaluates family truth independently of R readiness"):
    val proof = mateRelation("unready-defensive-proof")
    val fixture = causeFixture(
      id = "unready-defensive",
      kind = RelativeCauseKind.OnlyDefenseNecessity,
      directProof = proof,
      contrastProof = None
    )
    val admittedGraph = graphOf(fixture.records)
    val graph = admittedGraph.records
      .map {
        case record @ EvidenceRecord(ref, RelativeCauseFactEvidence(cause), _)
            if ref == fixture.causeRef =>
          record.copy(
            payload = RelativeCauseFactEvidence(
              cause.copy(directEffectAdmission = DirectEffectAdmission.Restricted(Set.empty))
            )
          )
        case record => record
      }
      .foldLeft(TypedEvidenceGraph.empty)((current, record) => current.add(record))
    val cause = graph.record(fixture.causeRef).collect {
      case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) => cause
    }.getOrElse(fail("expected registered defensive Cause"))
    val context = JudgmentAssemblyContext(
      input = normalizedInput,
      positions = Nil,
      lines = Nil,
      transitions = Nil,
      evidenceGraph = graph,
      claims = Nil
    )

    assert(!RelativeCauseConstructionAdmission.initiallyReady(cause, graph))
    val hosted = JudgmentClaimAssembler
      .propose(context)
      .filter(_.evidence.contains(fixture.causeRef))
    assertEquals(hosted.map(_.family), List(ClaimFamily.Defensive))
    assertEquals(PlayerFacingCauseReadinessPolicy.collect(hosted.head, graph), Nil)
    assertEquals(
      ClaimTruthPolicy.evaluate(hosted.head, graph).status,
      ClaimAdmissionStatus.Certified
    )

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

  test("claim dedup does not collide semantically different Cause sets"):
    val materialProof = hangingRelation("distinct-material-proof")
    val material = causeFixture(
      id = "distinct-material",
      kind = RelativeCauseKind.MaterialSwing,
      directProof = materialProof,
      contrastProof = None
    )
    val forcingProof = mateRelation("distinct-forcing-proof")
    val forcing = causeFixture(
      id = "distinct-forcing",
      kind = RelativeCauseKind.KingForcing,
      directProof = forcingProof,
      contrastProof = None
    )
    val graph = graphOf(material.records ++ forcing.records)
    def claim(id: String, cause: CauseFixture, proof: EvidenceRef): JudgmentClaim =
      JudgmentClaim(
        id = id,
        family = ClaimFamily.Tactical,
        subject = ClaimSubject.ReferenceMove,
        primaryPosition = position,
        primaryLine = Some(reference),
        subjectMove = Some(reference.rootMove),
        evidence = List(cause.causeRef, proof, cause.comparisonRef),
        scope = EvidenceScope.BestLine,
        confidence = EvidenceConfidence.EngineBacked
      )
    val result = ClaimDeduplicator.deduplicateDetailed(
      List(
        admitted(claim("distinct-material-host", material, materialProof.ref)),
        admitted(claim("distinct-forcing-host", forcing, forcingProof.ref))
      ),
      graph
    )

    assertEquals(
      result.decisions.map(_.claim.id),
      List("distinct-material-host", "distinct-forcing-host")
    )
    assertEquals(result.trace, Nil)

  test("Ja rejects a Cause host that reuses an ID with a different EvidenceRef"):
    val proof = mateRelation("forged-ref-proof")
    val fixture = causeFixture(
      id = "forged-ref",
      kind = RelativeCauseKind.KingForcing,
      directProof = proof,
      contrastProof = None
    )
    val graph = graphOf(fixture.records)
    val forgedCauseRef = fixture.causeRef.copy(confidence = EvidenceConfidence.Heuristic)
    val claim = JudgmentClaim(
      id = "forged-ref-host",
      family = ClaimFamily.Tactical,
      subject = ClaimSubject.ReferenceMove,
      primaryPosition = position,
      primaryLine = Some(reference),
      subjectMove = Some(reference.rootMove),
      evidence = List(forgedCauseRef, proof.ref, fixture.comparisonRef),
      scope = EvidenceScope.BestLine,
      confidence = EvidenceConfidence.EngineBacked
    )

    val decision = ClaimTruthPolicy.evaluate(claim, graph)
    assertEquals(decision.status, ClaimAdmissionStatus.Rejected)
    assertEquals(decision.missingEvidence, List(forgedCauseRef))
    assertEquals(PlayerFacingCauseReadinessPolicy.collect(claim, graph), Nil)

  test("root-owned effect admission rejects a carried source with only a matching ID"):
    val proof = mateRelation("forged-carried-source-proof")
    val fixture = causeFixture(
      id = "forged-carried-source",
      kind = RelativeCauseKind.KingForcing,
      directProof = proof,
      contrastProof = None
    )
    val graph = graphOf(fixture.records)
    val cause = graph.record(fixture.causeRef).collect {
      case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) => cause
    }.getOrElse(fail("expected registered Cause"))
    val channel = RelativeCauseConstructionAdmission
      .admittedDirectChannels(cause, graph)
      .headOption
      .getOrElse(fail("expected certified direct channel"))
    val forgedSource = channel.binding.source.copy(confidence = EvidenceConfidence.Heuristic)
    val forgedChannel = channel.copy(binding = channel.binding.copy(source = forgedSource))

    assert(RootOwnedEffectPolicy.admits(cause, graph, channel))
    assert(!RootOwnedEffectPolicy.admits(cause, graph, forgedChannel))

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

  test("Ja rejects a ready Cause that is unrelated to the claim subject"):
    val proof = mateRelation("unrelated-host-proof")
    val fixture = causeFixture(
      id = "unrelated-host",
      kind = RelativeCauseKind.KingForcing,
      directProof = proof,
      contrastProof = None
    )
    val graph = graphOf(fixture.records)
    val claim = JudgmentClaim(
      id = "unrelated-played-host",
      family = ClaimFamily.Tactical,
      subject = ClaimSubject.PlayedMove,
      primaryPosition = position,
      primaryLine = Some(played),
      subjectMove = Some(played.rootMove),
      evidence = List(fixture.causeRef, proof.ref, fixture.comparisonRef),
      scope = EvidenceScope.PlayedLine,
      confidence = EvidenceConfidence.EngineBacked
    )

    assertEquals(
      PlayerFacingCauseReadinessPolicy.collect(claim, graph).map(_._2.id),
      List(fixture.causeRef.id)
    )
    assertEquals(ClaimTruthPolicy.evaluate(claim, graph).status, ClaimAdmissionStatus.Rejected)

  test("Ja does not bind a Cause to a different line identity with the same root move"):
    val proof = mateRelation("same-move-other-line-proof")
    val fixture = causeFixture(
      id = "same-move-other-line",
      kind = RelativeCauseKind.KingForcing,
      directProof = proof,
      contrastProof = None
    )
    val graph = graphOf(fixture.records)
    val otherLine = LineNodeRef(
      "same-move-alternative",
      reference.rootMove,
      2,
      LineNodeRole.Alternative
    )
    val claim = JudgmentClaim(
      id = "same-move-other-line-host",
      family = ClaimFamily.Tactical,
      subject = ClaimSubject.CandidateLine,
      primaryPosition = position,
      primaryLine = Some(otherLine),
      subjectMove = Some(otherLine.rootMove),
      evidence = List(fixture.causeRef, proof.ref, fixture.comparisonRef),
      scope = EvidenceScope.CandidateLine,
      confidence = EvidenceConfidence.EngineBacked
    )

    assertEquals(
      PlayerFacingCauseReadinessPolicy.collect(claim, graph).map(_._2.id),
      List(fixture.causeRef.id)
    )
    assertEquals(ClaimTruthPolicy.evaluate(claim, graph).status, ClaimAdmissionStatus.Rejected)

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
