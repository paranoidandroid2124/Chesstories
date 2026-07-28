package lila.chessjudgment.analysis.assembly

import chess.{ Black, Queen, Square, White }
import lila.chessjudgment.analysis.policy.{ ClaimAdmissionDecision, ClaimAdmissionStatus }
import lila.chessjudgment.model.Motif
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.strategic.EngineLine

class CrossComparisonRankingIntegrationTest extends munit.FunSuite:

  private val before = PositionNodeRef(
    "4k3/8/8/8/8/8/4Q3/4K3 w - - 0 1",
    0,
    Some(White)
  )
  private val after = before.copy(ply = 1)
  private val playedLine = LineNodeRef("played", "e2e3", 2, LineNodeRole.Played)
  private val bestLine = LineNodeRef("best", "e2e7", 1, LineNodeRole.BestReference)
  private val thirdLine = LineNodeRef("third", "e2e4", 3, LineNodeRole.Alternative)

  test("R stores one public claim host and its OnlyMove qualifier"):
    val comparisonRef = ref(
      "comparison",
      EvidenceProducer.RelativeMoveProducer,
      EvidenceLayer.CandidateComparison,
      Some(playedLine)
    )
    val comparison = CandidateComparisonFact(
      CandidateComparisonKind.PlayedVsBest,
      bestLine,
      playedLine,
      comparisonEval,
      onlyMoveDescriptor
    )
    val proof = threatRecord("best-proof", bestLine, bestLine.rootMove, "e8")
    val cause = RelativeCauseFact(
      kind = RelativeCauseKind.OnlyDefenseNecessity,
      comparisonEvidence = comparisonRef,
      supportEvidence = List(proof.ref),
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
          List(proof.ref)
        )
      ))
    )
    val causeRef = ref(
      "cause",
      EvidenceProducer.RelativeMoveProducer,
      EvidenceLayer.RelativeCause,
      Some(bestLine)
    )
    val graph = graphOf(
      EvidenceRecord(comparisonRef, CandidateComparisonEvidence(comparison)),
      proof,
      EvidenceRecord(causeRef, RelativeCauseFactEvidence(cause))
    )
    val evidence = List(causeRef, comparisonRef, proof.ref)
    val defensive = claim("defensive", ClaimFamily.Defensive, evidence)
    val material = claim("material", ClaimFamily.Material, evidence)
    val candidates = ClaimCandidateGraph(
      List(defensive, material).map(admitted),
      graph
    )
    val ranking = ClaimArbitrator.rankDetailed(candidates, List(relativeAssessment(comparisonRef)))
    val hosts = ranking.ranked.filter(_.playerFacingCauseEvidenceIds.contains(causeRef.id))

    assertEquals(hosts.map(_.claim.id), List(defensive.id))
    assertEquals(hosts.head.exposureTier, PlayerFacingClaimTier.Primary)
    assertEquals(hosts.head.onlyMoveQualifiers.map(_.causeEvidence.id), List(causeRef.id))
    assertEquals(
      ranking.onlyMoveConstraintResolutions.flatMap(_.qualifiers).map(_.causeEvidence.id),
      List(causeRef.id)
    )
    assertEquals(
      ranking.ranked.find(_.claim.id == material.id).map(_.exposureTier),
      Some(PlayerFacingClaimTier.Diagnostic)
    )

  test("R selects one equivalent Cause and records the sibling disposition"):
    val selectedComparison = ref(
      "selected-comparison",
      EvidenceProducer.RelativeMoveProducer,
      EvidenceLayer.CandidateComparison,
      Some(playedLine)
    )
    val suppressedComparison = selectedComparison.copy(id = "suppressed-comparison")
    val selectedProof = threatRecord("selected-proof", bestLine, bestLine.rootMove, "e8")
    val suppressedProof = threatRecord("suppressed-proof", bestLine, bestLine.rootMove, "e8")
    val selectedCause = simpleCause(selectedComparison, selectedProof.ref)
    val suppressedCause = simpleCause(suppressedComparison, suppressedProof.ref)
    val selectedRef = ref("selected-cause", EvidenceProducer.RelativeMoveProducer, EvidenceLayer.RelativeCause, Some(bestLine))
    val suppressedRef = ref("suppressed-cause", EvidenceProducer.RelativeMoveProducer, EvidenceLayer.RelativeCause, Some(bestLine))
    val graph = graphOf(
      EvidenceRecord(
        selectedComparison,
        CandidateComparisonEvidence(comparison(selectedComparison.id))
      ),
      EvidenceRecord(
        suppressedComparison,
        CandidateComparisonEvidence(comparison(suppressedComparison.id))
      ),
      selectedProof,
      suppressedProof,
      EvidenceRecord(selectedRef, RelativeCauseFactEvidence(selectedCause)),
      EvidenceRecord(suppressedRef, RelativeCauseFactEvidence(suppressedCause))
    )
    val claim = this.claim(
      "combined",
      ClaimFamily.Tactical,
      List(
        selectedRef,
        selectedComparison,
        selectedProof.ref,
        suppressedRef,
        suppressedComparison,
        suppressedProof.ref
      )
    )
    val resolution = PlayerFacingCauseExposurePipeline.resolve(
      List(claim),
      graph,
      Set(playedLine.rootMove)
    )
    val allCauseIds = Set(selectedRef.id, suppressedRef.id)
    val selectedIds = resolution.selectedCauseEvidenceIds
    val decisionById = resolution.crossDecisions.map(decision => decision.causeEvidenceId -> decision).toMap

    assertEquals(selectedIds.size, 1)
    assert(selectedIds.subsetOf(allCauseIds))
    assertEquals(decisionById.keySet, allCauseIds)
    assert(selectedIds.forall(id => decisionById(id).selected))
    assertEquals(
      (allCauseIds -- selectedIds).map(id => decisionById(id).status),
      Set(CrossComparisonExposureStatus.RedundantAcrossComparison)
    )

  test("Cause disposition gives every C record one terminal Jp Ja or R state"):
    val comparisonRef = ref(
      "disposition-comparison",
      EvidenceProducer.RelativeMoveProducer,
      EvidenceLayer.CandidateComparison,
      Some(playedLine)
    )
    val proof = threatRecord("disposition-proof", bestLine, bestLine.rootMove, "e8")
    val baseCause = RelativeCauseFact(
      kind = RelativeCauseKind.OnlyDefenseNecessity,
      comparisonEvidence = comparisonRef,
      supportEvidence = List(proof.ref),
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
          List(proof.ref)
        )
      ))
    )
    val selectedRef = ref(
      "disposition-selected",
      EvidenceProducer.RelativeMoveProducer,
      EvidenceLayer.RelativeCause,
      Some(bestLine)
    )
    val unproposedRef = selectedRef.copy(id = "disposition-unproposed")
    val deferredRef = selectedRef.copy(id = "disposition-deferred")
    val rejectedRef = selectedRef.copy(id = "disposition-rejected")
    val unreadyRef = selectedRef.copy(id = "disposition-object-unready")
    val unreadyCause = baseCause.copy(
      attribution = baseCause.attribution.copy(rootMoveMatched = false)
    )
    val graph = graphOf(
      EvidenceRecord(
        comparisonRef,
        CandidateComparisonEvidence(comparison("disposition-comparison"))
      ),
      proof,
      EvidenceRecord(selectedRef, RelativeCauseFactEvidence(baseCause)),
      EvidenceRecord(unproposedRef, RelativeCauseFactEvidence(baseCause)),
      EvidenceRecord(deferredRef, RelativeCauseFactEvidence(baseCause)),
      EvidenceRecord(rejectedRef, RelativeCauseFactEvidence(baseCause)),
      EvidenceRecord(unreadyRef, RelativeCauseFactEvidence(unreadyCause))
    )
    def hosted(id: String, causeRef: EvidenceRef): JudgmentClaim =
      claim(id, ClaimFamily.Tactical, List(causeRef, comparisonRef, proof.ref))
    def decided(claim: JudgmentClaim, status: ClaimAdmissionStatus): ClaimAdmissionDecision =
      ClaimAdmissionDecision(
        claim,
        status,
        claim.evidence.map(_.layer).toSet,
        Nil,
        Nil
      )
    val decisions = List(
      admitted(hosted("disposition-selected-host", selectedRef)),
      decided(hosted("disposition-deferred-host", deferredRef), ClaimAdmissionStatus.Deferred),
      decided(hosted("disposition-rejected-host", rejectedRef), ClaimAdmissionStatus.Rejected),
      decided(hosted("disposition-unready-host", unreadyRef), ClaimAdmissionStatus.Rejected)
    )
    val ranking = ClaimArbitrator.rankDetailed(
      ClaimCandidateGraph(decisions, graph),
      List(relativeAssessment(comparisonRef))
    )
    val ledger = ranking.causeDispositionLedger
    val statusById = ledger.dispositions.map(item => item.causeEvidence.id -> item.status).toMap

    assertEquals(statusById.keySet, Set(
      selectedRef.id,
      unproposedRef.id,
      deferredRef.id,
      rejectedRef.id,
      unreadyRef.id
    ))
    assertEquals(statusById(selectedRef.id), CauseDispositionStatus.Selected)
    assertEquals(statusById(unproposedRef.id), CauseDispositionStatus.Unproposed)
    assertEquals(statusById(deferredRef.id), CauseDispositionStatus.AdmissionDeferred)
    assertEquals(statusById(rejectedRef.id), CauseDispositionStatus.Rejected)
    assertEquals(statusById(unreadyRef.id), CauseDispositionStatus.ObjectUnready)
    assertEquals(
      ledger.selectedCauseEvidenceIds,
      ranking.causeExposureResolution.selectedCauseEvidenceIds
    )

  private def comparison(id: String): CandidateComparisonFact =
    CandidateComparisonFact(
      CandidateComparisonKind.PlayedVsBest,
      bestLine,
      playedLine,
      comparisonEval
    )

  private def comparisonEval: EvalComparison =
    EvalComparison.fromLines(White, candidateNode(bestLine, 300), candidateNode(playedLine, -300))

  private def onlyMoveDescriptor: Option[CandidateSetDescriptor] =
    CandidateSetDescriptor.fromLines(
      White,
      List(
        candidateNode(bestLine, 300),
        candidateNode(playedLine, -300),
        candidateNode(thirdLine, -350)
      ),
      candidateNode(bestLine, 300)
    )

  private def candidateNode(line: LineNodeRef, scoreCp: Int): CandidateLineNode =
    CandidateLineNode(
      line,
      EngineLine(List(line.rootMove), scoreCp, depth = 18),
      ref(s"${line.id}-eval", EvidenceProducer.EngineEvalProducer, EvidenceLayer.Eval, Some(line))
    )

  private def simpleCause(comparisonRef: EvidenceRef, proofRef: EvidenceRef): RelativeCauseFact =
    RelativeCauseFact(
      RelativeCauseKind.OnlyDefenseNecessity,
      comparisonRef,
      List(proofRef),
      RelativeCauseSourceSide.Reference,
      CauseAttribution(
        CauseAttributionKind.ReferenceCreatesResource,
        rootMoveMatched = true,
        directProofEligible = true
      ),
      Some(RelativeCauseProof(
        directProof = RelativeCauseProofSection(
          RelativeCauseProofRole.DirectProof,
          RelativeCauseProofStrength.Primary,
          List(proofRef)
        )
      ))
    )

  private def claim(id: String, family: ClaimFamily, evidence: List[EvidenceRef]): JudgmentClaim =
    JudgmentClaim(
      id,
      family,
      ClaimSubject.ReferenceMove,
      before,
      Some(bestLine),
      Some(bestLine.rootMove),
      evidence,
      EvidenceScope.BestLine,
      EvidenceConfidence.EngineBacked
    )

  private def admitted(claim: JudgmentClaim): ClaimAdmissionDecision =
    ClaimAdmissionDecision(
      claim,
      ClaimAdmissionStatus.Certified,
      claim.evidence.map(_.layer).toSet,
      Nil,
      Nil
    )

  private def relativeAssessment(comparisonRef: EvidenceRef): RelativeMoveAssessment =
    val playedEvidence = ref("played-line", EvidenceProducer.LegalLineProducer, EvidenceLayer.Line, Some(playedLine))
    val bestEvidence = ref("best-line", EvidenceProducer.LegalLineProducer, EvidenceLayer.Line, Some(bestLine))
    val transitionEvidence = ref(
      "played-transition",
      EvidenceProducer.MoveTransitionProducer,
      EvidenceLayer.MoveTransition,
      Some(playedLine)
    )
    RelativeMoveAssessment(
      MoveTransitionEdge(
        TransitionEdgeRole.Played,
        before,
        playedLine.rootMove,
        after,
        transitionEvidence
      ),
      None,
      CandidateLineNode(bestLine, EngineLine(List(bestLine.rootMove), 300, depth = 18), bestEvidence),
      CandidateLineNode(playedLine, EngineLine(List(playedLine.rootMove), -300, depth = 18), playedEvidence),
      ref("assessment", EvidenceProducer.RelativeMoveProducer, EvidenceLayer.RelativeAssessment, Some(playedLine)),
      comparisonRef
    )

  private def threatRecord(
      id: String,
      line: LineNodeRef,
      onlyDefense: String,
      targetSquare: String
  ): EvidenceRecord =
    val square = Square.fromKey(targetSquare).getOrElse(fail(s"invalid square $targetSquare"))
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
    val evidence = ref(
      id,
      EvidenceProducer.ThreatPressureProducer,
      EvidenceLayer.ThreatPressure,
      Some(line)
    )
    EvidenceRecord(evidence, ThreatEpisodeEvidence(ThreatEpisode.fromThreat(threat, 0)))

  private def ref(
      id: String,
      producer: EvidenceProducer,
      layer: EvidenceLayer,
      line: Option[LineNodeRef]
  ): EvidenceRef =
    EvidenceRef(
      id,
      producer,
      layer,
      before,
      line,
      line.map(_.role.scope).getOrElse(EvidenceScope.Counterfactual),
      if layer == EvidenceLayer.CandidateComparison || layer == EvidenceLayer.RelativeCause ||
          layer == EvidenceLayer.RelativeAssessment then EvidenceConfidence.EngineBacked
      else EvidenceConfidence.LegalReplayVerified
    )

  private def graphOf(records: EvidenceRecord*): TypedEvidenceGraph =
    ExplicitCauseAdmissionTestSupport.graph(records.toList)
