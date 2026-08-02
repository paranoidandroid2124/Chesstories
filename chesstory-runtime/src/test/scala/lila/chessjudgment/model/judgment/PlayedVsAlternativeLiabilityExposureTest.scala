package lila.chessjudgment.model.judgment

import chess.{ Black, Queen, White }
import lila.chessjudgment.model.Motif
import lila.chessjudgment.model.line.PrincipalVariationEvidence

class PlayedVsAlternativeLiabilityExposureTest extends munit.FunSuite:

  private val position = PositionNodeRef(
    "4r1k1/8/8/8/8/r7/4Q3/4R1K1 w - - 0 1",
    0,
    Some(White)
  )
  private val played = LineNodeRef("played", "e2f3", 9, LineNodeRole.Played)
  private val best = LineNodeRef("best", "e2e7", 1, LineNodeRole.BestReference)
  private val closeAlternative = LineNodeRef("close-alternative", "e2e6", 2, LineNodeRole.Alternative)
  private val remoteAlternative = LineNodeRef("remote-alternative", "e2b5", 6, LineNodeRole.Alternative)
  private val captureCheckAlternative =
    LineNodeRef("capture-check-alternative", "e2e8", 3, LineNodeRole.Alternative)

  test("exact improving PVA liability survives as complementary without a PVB owner"):
    val liability = liabilityCause(
      "pva-only-liability",
      CandidateComparisonKind.PlayedVsAlternative,
      closeAlternative,
      -8.0,
      liabilityRecord("pva-only-proof", played)
    )
    val resolved = resolve(List(liability))
    val decision = resolved.cross(liability.causeRef.id)

    assertEquals(decision.status, CrossComparisonExposureStatus.SelectedComplementary)
    assertEquals(
      decision.reason,
      CrossComparisonExposureReason.BetterAlternativeExposesExactPlayedLiability
    )
    assertEquals(decision.effectMode, Some(PlayerFacingCauseEffectMode.PlayedLiability))

  test("an exact PVB liability remains primary and suppresses its PVA duplicate"):
    val proof = liabilityRecord("shared-liability-proof", played)
    val pvb = liabilityCause(
      "pvb-liability-owner",
      CandidateComparisonKind.PlayedVsBest,
      best,
      -12.0,
      proof
    )
    val pva = liabilityCause(
      "pva-liability-copy",
      CandidateComparisonKind.PlayedVsAlternative,
      closeAlternative,
      -8.0,
      proof
    )
    val resolved = resolve(List(pvb, pva))

    assertEquals(resolved.cross(pvb.causeRef.id).status, CrossComparisonExposureStatus.SelectedPrimary)
    assertEquals(
      resolved.cross(pva.causeRef.id).status,
      CrossComparisonExposureStatus.RedundantAcrossComparison
    )
    assertEquals(
      resolved.cross(pva.causeRef.id).representativeCauseEvidenceId,
      pvb.causeRef.id
    )

  test("an exact PVA liability below the playable-loss threshold remains inferior"):
    val liability = liabilityCause(
      "below-threshold-liability",
      CandidateComparisonKind.PlayedVsAlternative,
      closeAlternative,
      -2.0,
      liabilityRecord("below-threshold-proof", played)
    )
    val decision = resolve(List(liability)).cross(liability.causeRef.id)

    assertEquals(decision.status, CrossComparisonExposureStatus.InferiorAlternative)
    assertEquals(
      decision.reason,
      CrossComparisonExposureReason.AlternativeDoesNotImproveOnPlayed
    )
    assertEquals(decision.effectMode, Some(PlayerFacingCauseEffectMode.PlayedLiability))

  test("an unrelated PVB liability does not suppress a distinct exact PVA liability"):
    val pvb = liabilityCause(
      "material-pvb-liability",
      CandidateComparisonKind.PlayedVsBest,
      best,
      -12.0,
      liabilityRecord("material-pvb-proof", played)
    )
    val pva = liabilityCause(
      "threat-pva-liability",
      CandidateComparisonKind.PlayedVsAlternative,
      closeAlternative,
      -8.0,
      liabilityRecord("distinct-kind-pva-proof", played),
      RelativeCauseKind.TacticalRefutationOfPlayed
    )
    val resolved = resolve(List(pvb, pva))

    assertEquals(resolved.cross(pvb.causeRef.id).status, CrossComparisonExposureStatus.SelectedPrimary)
    assertEquals(
      resolved.cross(pva.causeRef.id).reason,
      CrossComparisonExposureReason.BetterAlternativeExposesExactPlayedLiability
    )
    assertEquals(
      resolved.cross(pva.causeRef.id).status,
      CrossComparisonExposureStatus.SelectedComplementary
    )
    assertEquals(
      resolved.cross(pva.causeRef.id).representativeCauseEvidenceId,
      pva.causeRef.id
    )

  test("PVA liability rank can precede a weaker unrelated alternative resource"):
    val liability = liabilityCause(
      "ranked-pva-liability",
      CandidateComparisonKind.PlayedVsAlternative,
      closeAlternative,
      -12.0,
      liabilityRecord("ranked-pva-liability-proof", played)
    )
    val resource = referenceResourceCause(
      "ranked-pva-resource",
      remoteAlternative,
      -8.0,
      motifRecord("ranked-pva-resource-proof", remoteAlternative)
    )
    val resolved = resolve(List(liability, resource))
    val liabilityDecision = resolved.cross(liability.causeRef.id)
    val resourceDecision = resolved.cross(resource.causeRef.id)

    assert(liabilityDecision.selected)
    assert(resourceDecision.selected)
    assert(
      liabilityDecision.comparisonExposureRank.getOrElse(Int.MaxValue) <
        resourceDecision.comparisonExposureRank.getOrElse(Int.MaxValue)
    )

  test("exact tactical refutation suppresses MaterialSwing, never the reverse"):
    val proof = liabilityRecord("exact-fallback-proof", played)
    val material = liabilityCause(
      "material-fallback",
      CandidateComparisonKind.PlayedVsAlternative,
      closeAlternative,
      -8.0,
      proof,
      RelativeCauseKind.MaterialSwing
    )
    val tactical = liabilityCause(
      "exact-tactical-refutation",
      CandidateComparisonKind.PlayedVsAlternative,
      closeAlternative,
      -8.0,
      proof,
      RelativeCauseKind.TacticalRefutationOfPlayed
    )
    val resolved = resolve(List(material, tactical))

    assertEquals(
      resolved.dominance(material.causeRef.id).status,
      RelativeCauseDominanceStatus.DominatedFallback
    )
    assertEquals(
      resolved.dominance(material.causeRef.id).dominatingCauseEvidenceIds,
      List(tactical.causeRef.id)
    )
    assert(resolved.dominance(tactical.causeRef.id).retained)

  test("disjoint tactical refutation and MaterialSwing channels coexist"):
    val material = referenceResourceCause(
      "disjoint-material",
      captureCheckAlternative,
      -8.0,
      rootCaptureRecord("disjoint-material-proof", captureCheckAlternative),
      RelativeCauseKind.MaterialSwing
    )
    val tactical = referenceResourceCause(
      "disjoint-tactical",
      captureCheckAlternative,
      -8.0,
      motifRecord("disjoint-tactical-proof", captureCheckAlternative),
      RelativeCauseKind.TacticalRefutationOfPlayed
    )
    val resolved = resolve(List(material, tactical))

    assert(resolved.cross(material.causeRef.id).selected)
    assert(resolved.cross(tactical.causeRef.id).selected)
    assert(resolved.dominance(material.causeRef.id).retained)
    assert(resolved.dominance(tactical.causeRef.id).retained)

  private final case class CauseCase(
      cause: RelativeCauseFact,
      causeRef: EvidenceRef,
      records: List[EvidenceRecord]
  )

  private final case class Resolution(
      cross: Map[String, CrossComparisonExposureDecision],
      dominance: Map[String, RelativeCauseDominanceDecision]
  )

  private def liabilityCause(
      id: String,
      comparisonKind: CandidateComparisonKind,
      reference: LineNodeRef,
      delta: Double,
      proof: EvidenceRecord,
      causeKind: RelativeCauseKind = RelativeCauseKind.CandidateTacticalLiability
  ): CauseCase =
    causeCase(
      id,
      comparisonKind,
      reference,
      delta,
      RelativeCauseSourceSide.Candidate,
      CauseAttributionKind.CandidateAllowsLiability,
      proof,
      causeKind
    )

  private def referenceResourceCause(
      id: String,
      reference: LineNodeRef,
      delta: Double,
      proof: EvidenceRecord,
      causeKind: RelativeCauseKind = RelativeCauseKind.KingForcing
  ): CauseCase =
    causeCase(
      id,
      CandidateComparisonKind.PlayedVsAlternative,
      reference,
      delta,
      RelativeCauseSourceSide.Reference,
      CauseAttributionKind.ReferenceCreatesResource,
      proof,
      causeKind
    )

  private def causeCase(
      id: String,
      comparisonKind: CandidateComparisonKind,
      reference: LineNodeRef,
      delta: Double,
      sourceSide: RelativeCauseSourceSide,
      attributionKind: CauseAttributionKind,
      proof: EvidenceRecord,
      causeKind: RelativeCauseKind
  ): CauseCase =
    val comparisonRef = ref(
      s"$id-comparison",
      EvidenceProducer.RelativeMoveProducer,
      EvidenceLayer.CandidateComparison,
      Some(played)
    )
    val comparison = CandidateComparisonFact(
      comparisonKind,
      reference,
      played,
      EvalComparison(White, delta, verdict(delta))
    )
    val unresolvedCause = RelativeCauseFact(
      kind = causeKind,
      comparisonEvidence = comparisonRef,
      supportEvidence = List(proof.ref),
      sourceSide = sourceSide,
      attribution = CauseAttribution(
        attributionKind,
        rootMoveMatched = true,
        directProofEligible = true
      ),
      proof = Some(RelativeCauseProof(
        RelativeCauseProofSection(
          RelativeCauseProofRole.DirectProof,
          RelativeCauseProofStrength.Primary,
          List(proof.ref)
        )
      ))
    )
    val eventLine = if sourceSide == RelativeCauseSourceSide.Candidate then played else reference
    val causeRef = ref(
      id,
      EvidenceProducer.RelativeMoveProducer,
      EvidenceLayer.RelativeCause,
      Some(eventLine)
    )
    CauseCase(
      unresolvedCause,
      causeRef,
      List(
        EvidenceRecord(comparisonRef, CandidateComparisonEvidence(comparison)),
        proof,
        EvidenceRecord(causeRef, RelativeCauseFactEvidence(unresolvedCause))
      )
    )

  private def verdict(delta: Double): MoveChoiceVerdict =
    if delta <= -10.0 then MoveChoiceVerdict.Blunder
    else if delta <= -2.5 then MoveChoiceVerdict.Inaccuracy
    else MoveChoiceVerdict.MatchesReference

  private def liabilityRecord(id: String, line: LineNodeRef): EvidenceRecord =
    materialLossRecord(
      id,
      line,
      replyMove = "a3f3",
      capturedRole = EvidencePieceRole("queen"),
      square = EvidenceSquare("f3"),
      valueCp = 900
    )

  private def materialLossRecord(
      id: String,
      line: LineNodeRef,
      replyMove: String,
      capturedRole: EvidencePieceRole,
      square: EvidenceSquare,
      valueCp: Int
  ): EvidenceRecord =
    val rootAfter = PrincipalVariationEvidence
      .legalFenAfter(position.fen, line.rootMove)
      .getOrElse(fail(s"expected legal liability root ${line.rootMove}"))
    val replyAfter = PrincipalVariationEvidence
      .legalFenAfter(rootAfter, replyMove)
      .getOrElse(fail(s"expected legal liability reply $replyMove"))
    val capture = LineMaterialCapture(
      moveUci = replyMove,
      plyOffset = 1,
      side = Black,
      attackerRole = EvidencePieceRole("rook"),
      capturedRole = capturedRole,
      square = square,
      valueCp = valueCp,
      recapture = false
    )
    val consequence = LineConsequence(
      kind = LineConsequenceKind.MaterialLoss,
      lineMoves = List(line.rootMove, replyMove),
      proofSignal = true,
      eventMove = Some(replyMove),
      rootMove = Some(line.rootMove),
      rootSide = Some(White),
      beneficiary = Some(Black),
      materialOutcome = Some(RootOwnedMaterialOutcome(
        RootOwnedMaterialEventSalience(
          replyMove,
          1,
          capture.capturedRole,
          capture.square,
          capture.valueCp
        ),
        Black,
        valueCp
      ))
    )
    val payload = LineFactEvidence(
      line = line,
      material = Some(LineMaterialSummary(
        White,
        List(capture),
        -valueCp,
        0,
        valueCp,
        hasRecaptureChain = false,
        hasRecoveryWindow = false,
        promotionGainCpForMover = 0,
        materialWindowComplete = true
      )),
      replay = List(
        LineReplayStep(0, line.rootMove, position.fen, rootAfter),
        LineReplayStep(1, replyMove, rootAfter, replyAfter)
      ),
      events = List(LineMoveEvent(
        LineEventKind.Capture,
        replyMove,
        1,
        Some(Black),
        Some(EvidencePieceRole("rook")),
        Some(capturedRole),
        Some(square)
      )),
      consequences = List(consequence)
    )
    EvidenceRecord(
      ref(id, EvidenceProducer.LegalLineProducer, EvidenceLayer.Line, Some(line)),
      payload
    )

  private def rootCaptureRecord(id: String, line: LineNodeRef): EvidenceRecord =
    val rootAfter = PrincipalVariationEvidence
      .legalFenAfter(position.fen, line.rootMove)
      .getOrElse(fail(s"expected legal capture-check root ${line.rootMove}"))
    val capture = LineMaterialCapture(
      line.rootMove,
      0,
      White,
      EvidencePieceRole("queen"),
      EvidencePieceRole("rook"),
      EvidenceSquare("e8"),
      500,
      recapture = false
    )
    val consequence = LineConsequence(
      LineConsequenceKind.MaterialGain,
      List(line.rootMove),
      proofSignal = true,
      eventMove = Some(line.rootMove),
      rootMove = Some(line.rootMove),
      rootSide = Some(White),
      beneficiary = Some(White),
      materialOutcome = Some(RootOwnedMaterialOutcome(
        RootOwnedMaterialEventSalience(
          line.rootMove,
          0,
          capture.capturedRole,
          capture.square,
          capture.valueCp
        ),
        White,
        500
      ))
    )
    EvidenceRecord(
      ref(id, EvidenceProducer.LegalLineProducer, EvidenceLayer.Line, Some(line)),
      LineFactEvidence(
        line = line,
        material = Some(LineMaterialSummary(
          White,
          List(capture),
          500,
          500,
          0,
          hasRecaptureChain = false,
          hasRecoveryWindow = false,
          promotionGainCpForMover = 0,
          materialWindowComplete = true
        )),
        replay = List(LineReplayStep(0, line.rootMove, position.fen, rootAfter)),
        events = List(LineMoveEvent(
          LineEventKind.Capture,
          line.rootMove,
          0,
          Some(White),
          Some(EvidencePieceRole("queen")),
          Some(EvidencePieceRole("rook")),
          Some(EvidenceSquare("e8"))
        )),
        consequences = List(consequence)
      )
    )

  private def motifRecord(id: String, line: LineNodeRef): EvidenceRecord =
    EvidenceRecord(
      ref(id, EvidenceProducer.MoveMotifProducer, EvidenceLayer.MoveMotif, Some(line)),
      MoveMotifEvidence(
        MoveMotifEvent.fromMotif(
          line.rootMove,
          Motif.Check(Queen, chess.Square.E8, Motif.CheckType.Normal, White, 0, Some(line.rootMove))
        )
      )
    )

  private def resolve(cases: List[CauseCase]): Resolution =
    val graph = ExplicitCauseAdmissionTestSupport.graph(
      cases.flatMap(_.records).distinctBy(_.ref.id)
    )
    val registered = cases.map(item =>
      ExplicitCauseAdmissionTestSupport
        .registeredCause(item.causeRef, graph)
        .getOrElse(fail(s"expected registered Cause ${item.causeRef.id}")) -> item.causeRef
    )
    val channels = registered.map { case (cause, causeRef) =>
      causeRef.id -> RelativeCauseConstructionAdmission.admittedDirectChannels(cause, graph)
    }.toMap
    val eligibility = CrossComparisonCauseExposurePolicy.resolveEligibilityForDominance(
      registered,
      graph,
      Set(played.rootMove)
    )
    val dominance = RelativeCauseDominancePolicy.resolve(
      registered,
      eligibility,
      channels,
      graph
    )
    val retainedIds = dominance.decisions.filter(_.retained).map(_.causeEvidenceId).toSet
    val retained = registered.filter { case (_, ref) => retainedIds(ref.id) }
    val representatives = CrossComparisonCauseExposurePolicy.resolve(
      retained,
      graph,
      Set(played.rootMove),
      dominance.fallbackObligationsByDominatorId
    )
    Resolution(
      CrossComparisonCauseExposurePolicy
        .resolveRetainedPvbResponsibility(
          representatives,
          retained,
          graph,
          Set(played.rootMove)
        )
        .decisions
        .map(decision => decision.causeEvidenceId -> decision)
        .toMap,
      dominance.decisions.map(decision => decision.causeEvidenceId -> decision).toMap
    )

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
      position,
      line,
      line.map(_.role.scope).getOrElse(EvidenceScope.Counterfactual),
      if layer == EvidenceLayer.CandidateComparison || layer == EvidenceLayer.RelativeCause then
        EvidenceConfidence.EngineBacked
      else EvidenceConfidence.LegalReplayVerified
    )
