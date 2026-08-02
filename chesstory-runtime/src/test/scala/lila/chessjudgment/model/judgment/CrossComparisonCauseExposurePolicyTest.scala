package lila.chessjudgment.model.judgment

import chess.{ Black, Queen, Square, White }
import lila.chessjudgment.model.Motif
import lila.chessjudgment.model.line.PrincipalVariationEvidence

class CrossComparisonCauseExposurePolicyTest extends munit.FunSuite:

  private val position = PositionNodeRef(
    "4k3/8/8/8/8/r7/4Q3/4K3 w - - 0 1",
    0,
    Some(White)
  )
  private val played = LineNodeRef("played", "e2e3", 9, LineNodeRole.Played)
  private val best = LineNodeRef("best", "e2e7", 1, LineNodeRole.BestReference)

  test("comparison authority preserves C records and selects only valid public relations"):
    val playedProof = motifRecord("played-proof", played, "e8")
    val playedLiabilityProof = liabilityRecord("played-liability-proof", played)
    val bestProof = motifRecord("best-proof", best, "h4")
    val inferiorLine = LineNodeRef("inferior", "e2e4", 5, LineNodeRole.Alternative)
    val inferiorProof = motifRecord("inferior-proof", inferiorLine, "a8")
    val second = LineNodeRef("second", "e2e6", 2, LineNodeRole.Alternative)
    val secondProof = motifRecord("second-proof", second, "b8")
    val indirect = LineNodeRef("indirect", "e2e5", 3, LineNodeRole.Alternative)
    val indirectProof = motifRecord("indirect-proof", indirect, "c8")

    val primary = causeCase(
      "primary",
      CandidateComparisonKind.PlayedVsBest,
      best,
      played,
      -12.0,
      RelativeCauseSourceSide.Candidate,
      CauseAttributionKind.CandidateAllowsLiability,
      playedLiabilityProof,
      causeKind = RelativeCauseKind.CandidateTacticalLiability
    )
    val nonBadPrimary = causeCase(
      "non-bad-primary",
      CandidateComparisonKind.PlayedVsBest,
      best,
      played,
      -1.0,
      RelativeCauseSourceSide.Candidate,
      CauseAttributionKind.CandidateAllowsLiability,
      playedLiabilityProof,
      causeKind = RelativeCauseKind.CandidateTacticalLiability
    )
    val invalidPrimary = causeCase(
      "invalid-primary",
      CandidateComparisonKind.PlayedVsBest,
      best,
      played,
      5.0,
      RelativeCauseSourceSide.Candidate,
      CauseAttributionKind.CandidateCreatesValue,
      playedProof
    )
    val duplicateLiability = causeCase(
      "duplicate-liability",
      CandidateComparisonKind.PlayedVsAlternative,
      second,
      played,
      -8.0,
      RelativeCauseSourceSide.Candidate,
      CauseAttributionKind.CandidateAllowsLiability,
      playedLiabilityProof,
      causeKind = RelativeCauseKind.CandidateTacticalLiability
    )
    val exactResource = causeCase(
      "exact-resource",
      CandidateComparisonKind.PlayedVsAlternative,
      best,
      played,
      -12.0,
      RelativeCauseSourceSide.Reference,
      CauseAttributionKind.ReferenceCreatesResource,
      bestProof
    )
    val inferiorResource = causeCase(
      "inferior-resource",
      CandidateComparisonKind.PlayedVsAlternative,
      inferiorLine,
      played,
      7.0,
      RelativeCauseSourceSide.Reference,
      CauseAttributionKind.ReferenceCreatesResource,
      inferiorProof
    )
    val constraint = causeCase(
      "constraint",
      CandidateComparisonKind.BestVsSecond,
      best,
      second,
      -5.0,
      RelativeCauseSourceSide.Reference,
      CauseAttributionKind.ReferenceCreatesResource,
      secondProof
    )
    val diagnostic = causeCase(
      "diagnostic",
      CandidateComparisonKind.ReferenceVsAlternative,
      second,
      indirect,
      -5.0,
      RelativeCauseSourceSide.Reference,
      CauseAttributionKind.ReferenceCreatesResource,
      indirectProof
    )
    val all = List(
      primary,
      nonBadPrimary,
      invalidPrimary,
      duplicateLiability,
      exactResource,
      inferiorResource,
      constraint,
      diagnostic
    )
    val graph = graphOf(all.flatMap(_.records)* )
    val decisions = resolve(all, graph)

    assertEquals(decisions("primary").status, CrossComparisonExposureStatus.SelectedPrimary)
    assertEquals(decisions("non-bad-primary").status, CrossComparisonExposureStatus.DiagnosticComparison)
    assertEquals(decisions("invalid-primary").status, CrossComparisonExposureStatus.DiagnosticComparison)
    assertEquals(decisions("duplicate-liability").status, CrossComparisonExposureStatus.RedundantAcrossComparison)
    assertEquals(decisions("duplicate-liability").representativeCauseEvidenceId, "primary")
    assertEquals(decisions("exact-resource").status, CrossComparisonExposureStatus.SelectedComplementary)
    assertEquals(decisions("inferior-resource").status, CrossComparisonExposureStatus.InferiorAlternative)
    assertEquals(decisions("constraint").status, CrossComparisonExposureStatus.DiagnosticComparison)
    assertEquals(decisions("diagnostic").status, CrossComparisonExposureStatus.DiagnosticComparison)
    assertEquals(all.map(_.causeRef.id).distinct.size, decisions.size)

  test("equivalent alternative resources choose one representative without suppressing a new target"):
    val stronger = LineNodeRef("stronger", "e2e6", 2, LineNodeRole.Alternative)
    val weaker = LineNodeRef("weaker", "e2e5", 3, LineNodeRole.Alternative)
    val different = LineNodeRef("different", "e2h5", 4, LineNodeRole.Alternative)
    val strongerCase = referenceResource("stronger-resource", stronger, "e8", -14.0)
    val weakerCase = referenceResource("weaker-resource", weaker, "e8", -7.0)
    val differentCase = referenceResource("different-resource", different, "h4", -9.0)
    val all = List(strongerCase, weakerCase, differentCase)
    val decisions = resolve(all, graphOf(all.flatMap(_.records)*))

    assertEquals(decisions("stronger-resource").status, CrossComparisonExposureStatus.SelectedComplementary)
    assertEquals(decisions("weaker-resource").status, CrossComparisonExposureStatus.RedundantAcrossComparison)
    assertEquals(decisions("weaker-resource").representativeCauseEvidenceId, "stronger-resource")
    assertEquals(decisions("different-resource").status, CrossComparisonExposureStatus.SelectedComplementary)

  test("exact-identity Causes with incomparable fallback obligations remain independent"):
    val firstProof = motifRecord("exact-obligation-first-proof", best, "e8")
    val secondProof = motifRecord("exact-obligation-second-proof", best, "e8")
    val first = causeCase(
      "exact-obligation-first",
      CandidateComparisonKind.PlayedVsBest,
      best,
      played,
      -12.0,
      RelativeCauseSourceSide.Reference,
      CauseAttributionKind.ReferenceCreatesResource,
      firstProof
    )
    val second = causeCase(
      "exact-obligation-second",
      CandidateComparisonKind.PlayedVsBest,
      best,
      played,
      -12.0,
      RelativeCauseSourceSide.Reference,
      CauseAttributionKind.ReferenceCreatesResource,
      secondProof
    )
    val all = List(first, second)
    val decisions = resolve(
      all,
      graphOf(all.flatMap(_.records)*),
      fallbackObligationsByCauseId = Map(
        first.causeRef.id -> Set("first-fallback"),
        second.causeRef.id -> Set("second-fallback")
      )
    )

    assert(decisions(first.causeRef.id).selected)
    assert(decisions(second.causeRef.id).selected)
    assertEquals(decisions(first.causeRef.id).representativeCauseEvidenceId, first.causeRef.id)
    assertEquals(decisions(second.causeRef.id).representativeCauseEvidenceId, second.causeRef.id)

  test("a PVA liability with a unique fallback obligation is not absorbed by PVB authority"):
    val alternative = LineNodeRef("obligated-liability-alternative", "e2e6", 2, LineNodeRole.Alternative)
    val proof = liabilityRecord("obligated-liability-proof", played)
    val pvb = causeCase(
      "obligated-pvb-liability",
      CandidateComparisonKind.PlayedVsBest,
      best,
      played,
      -12.0,
      RelativeCauseSourceSide.Candidate,
      CauseAttributionKind.CandidateAllowsLiability,
      proof,
      causeKind = RelativeCauseKind.CandidateTacticalLiability
    )
    val pva = causeCase(
      "obligated-pva-liability",
      CandidateComparisonKind.PlayedVsAlternative,
      alternative,
      played,
      -8.0,
      RelativeCauseSourceSide.Candidate,
      CauseAttributionKind.CandidateAllowsLiability,
      proof,
      causeKind = RelativeCauseKind.CandidateTacticalLiability
    )
    val all = List(pvb, pva)
    val decisions = resolve(
      all,
      graphOf(all.flatMap(_.records)*),
      fallbackObligationsByCauseId = Map(pva.causeRef.id -> Set("pva-owned-fallback"))
    )

    assert(decisions(pvb.causeRef.id).selected)
    assert(decisions(pva.causeRef.id).selected)
    assertEquals(decisions(pvb.causeRef.id).representativeCauseEvidenceId, pvb.causeRef.id)
    assertEquals(decisions(pva.causeRef.id).representativeCauseEvidenceId, pva.causeRef.id)

  test("played-value contrasts use the strongest alternative and the least exaggerated tie"):
    val strongest = LineNodeRef("strongest", "d1d3", 2, LineNodeRole.Alternative)
    val strongestTie = LineNodeRef("strongest-tie", "d1e2", 2, LineNodeRole.Alternative)
    val weak = LineNodeRef("weak", "d1h5", 4, LineNodeRole.Alternative)
    val playedProof = motifRecord("played-value-proof", played, "e8")
    val strongestCase = playedValue("strongest-contrast", strongest, playedProof, 8.0)
    val leastExaggerated = playedValue("least-exaggerated", strongestTie, playedProof, 4.0)
    val weakCase = playedValue("weak-contrast", weak, playedProof, 18.0)
    val all = List(strongestCase, leastExaggerated, weakCase)
    val decisions = resolve(all, graphOf(all.flatMap(_.records)*))

    assertEquals(decisions("least-exaggerated").status, CrossComparisonExposureStatus.SelectedComplementary)
    assertEquals(decisions("strongest-contrast").representativeCauseEvidenceId, "least-exaggerated")
    assertEquals(decisions("weak-contrast").representativeCauseEvidenceId, "least-exaggerated")

  test("attribution polarity and distinct causal objects remain independent"):
    val alternative = LineNodeRef("alternative", "e2e6", 2, LineNodeRole.Alternative)
    val referenceProof = motifRecord("reference-polarity-proof", alternative, "e8")
    val playedProof = motifRecord("candidate-polarity-proof", played, "e8")
    val resource = causeCase(
      "reference-polarity",
      CandidateComparisonKind.PlayedVsAlternative,
      alternative,
      played,
      -8.0,
      RelativeCauseSourceSide.Reference,
      CauseAttributionKind.ReferenceCreatesResource,
      referenceProof
    )
    val value = causeCase(
      "candidate-polarity",
      CandidateComparisonKind.PlayedVsAlternative,
      alternative,
      played,
      8.0,
      RelativeCauseSourceSide.Candidate,
      CauseAttributionKind.CandidateCreatesValue,
      playedProof
    )
    val all = List(resource, value)
    val decisions = resolve(all, graphOf(all.flatMap(_.records)*))

    assert(decisions("reference-polarity").selected)
    assert(decisions("candidate-polarity").selected)

  test("PlayedVsBest keeps an unrelated alternative resource primary"):
    val playedProof = motifRecord("pvb-played-proof", played, "e8")
    val playedLiabilityProof = liabilityRecord("pvb-played-liability-proof", played)
    val bestProof = motifRecord("pvb-best-proof", best, "e8")
    val liability = causeCase(
      "pvb-liability",
      CandidateComparisonKind.PlayedVsBest,
      best,
      played,
      -12.0,
      RelativeCauseSourceSide.Candidate,
      CauseAttributionKind.CandidateAllowsLiability,
      playedLiabilityProof,
      causeKind = RelativeCauseKind.CandidateTacticalLiability
    )
    val resource = causeCase(
      "pvb-resource",
      CandidateComparisonKind.PlayedVsBest,
      best,
      played,
      -12.0,
      RelativeCauseSourceSide.Reference,
      CauseAttributionKind.ReferenceCreatesResource,
      bestProof
    )
    val exactMatch = causeCase(
      "pvb-exact-match",
      CandidateComparisonKind.PlayedVsBest,
      best,
      played,
      0.0,
      RelativeCauseSourceSide.Candidate,
      CauseAttributionKind.CandidateCreatesValue,
      playedProof,
      verdictOverride = Some(MoveChoiceVerdict.MatchesReference)
    )
    val playableValue = causeCase(
      "pvb-playable-value",
      CandidateComparisonKind.PlayedVsBest,
      best,
      played,
      -1.0,
      RelativeCauseSourceSide.Candidate,
      CauseAttributionKind.CandidateCreatesValue,
      playedProof,
      verdictOverride = Some(MoveChoiceVerdict.PlayableLoss)
    )
    val shared = List(liability, resource)
    val sharedDecisions = resolve(shared, graphOf(shared.flatMap(_.records)*))
    val matchDecisions = resolve(List(exactMatch), graphOf(exactMatch.records*))
    val playableDecisions = resolve(List(playableValue), graphOf(playableValue.records*))

    assertEquals(sharedDecisions("pvb-liability").status, CrossComparisonExposureStatus.SelectedPrimary)
    assertEquals(sharedDecisions("pvb-resource").status, CrossComparisonExposureStatus.SelectedPrimary)
    assertEquals(matchDecisions("pvb-exact-match").status, CrossComparisonExposureStatus.SelectedPrimary)
    assertEquals(playableDecisions("pvb-playable-value").status, CrossComparisonExposureStatus.SelectedComplementary)

  test("PVB responsibility uses only explicit typed change complements"):
    val admitted = DirectCausalChange.values.toList.flatMap { liability =>
      DirectCausalChange.values.toList.collect {
        case resource
            if CrossComparisonCauseExposurePolicy
              .complementaryPvbDirectChanges(liability, resource) =>
          liability -> resource
      }
    }.toSet

    assertEquals(
      admitted,
      Set(
        DirectCausalChange.Occurred -> DirectCausalChange.Prevented,
        DirectCausalChange.Lost -> DirectCausalChange.Maintained,
        DirectCausalChange.Refuted -> DirectCausalChange.Occurred,
        DirectCausalChange.Missed -> DirectCausalChange.Occurred
      )
    )

  test("one exact PVB responsibility counterpart makes the resource complementary"):
    val liabilityProof = endgameRecord(
      "pvb-conversion-liability-proof",
      played,
      "Lucena",
      status = LineEndgameTechniqueHorizonStatus.Failed
    )
    val resourceProof = endgameRecord(
      "pvb-conversion-resource-proof",
      best,
      "Lucena",
      status = LineEndgameTechniqueHorizonStatus.Active
    )
    val liability = causeCase(
      "pvb-conversion-liability",
      CandidateComparisonKind.PlayedVsBest,
      best,
      played,
      -12.0,
      RelativeCauseSourceSide.Candidate,
      CauseAttributionKind.CandidateAllowsLiability,
      liabilityProof,
      causeKind = RelativeCauseKind.ConversionMiss
    )
    val resource = causeCase(
      "pvb-conversion-resource",
      CandidateComparisonKind.PlayedVsBest,
      best,
      played,
      -12.0,
      RelativeCauseSourceSide.Reference,
      CauseAttributionKind.ReferenceCreatesResource,
      resourceProof,
      causeKind = RelativeCauseKind.ConversionSecured
    )
    val all = List(liability, resource)
    val graph = graphOf(all.flatMap(_.records)*)
    val registeredLiability = ExplicitCauseAdmissionTestSupport
      .registeredCause(liability.causeRef, graph)
      .getOrElse(fail("expected registered liability Cause"))
    val registeredResource = ExplicitCauseAdmissionTestSupport
      .registeredCause(resource.causeRef, graph)
      .getOrElse(fail("expected registered resource Cause"))
    val causes = List(
      registeredLiability -> liability.causeRef,
      registeredResource -> resource.causeRef
    )
    val preliminary = CrossComparisonCauseExposurePolicy.resolve(
      causes,
      graph,
      Set(played.rootMove)
    )
    val preliminaryById = preliminary.map(decision => decision.causeEvidenceId -> decision).toMap
    val decisions = resolve(all, graph)
    val liabilityChannel = RelativeCauseConstructionAdmission
      .admittedDirectChannels(registeredLiability, graph)
      .headOption
      .getOrElse(fail("expected exact liability channel"))
    val resourceChannel = RelativeCauseConstructionAdmission
      .admittedDirectChannels(registeredResource, graph)
      .headOption
      .getOrElse(fail("expected exact resource channel"))

    assert(
      CrossComparisonCauseExposurePolicy.samePvbResponsibilityChannel(
        registeredLiability,
        liabilityChannel,
        registeredResource,
        resourceChannel,
        graph
      )
    )
    assert(
      !CrossComparisonCauseExposurePolicy.samePvbResponsibilityChannel(
        registeredLiability,
        liabilityChannel,
        registeredResource,
        resourceChannel.copy(proofSegmentAmbiguous = true),
        graph
      ),
      "an ambiguous public proof segment cannot establish a responsibility counterpart"
    )
    assertEquals(
      preliminaryById("pvb-conversion-resource").status,
      CrossComparisonExposureStatus.SelectedPrimary,
      "responsibility tiers must not run before fallback dominance"
    )
    assertEquals(
      decisions("pvb-conversion-liability").status,
      CrossComparisonExposureStatus.SelectedPrimary
    )
    assertEquals(
      decisions("pvb-conversion-resource").status,
      CrossComparisonExposureStatus.SelectedComplementary
    )
    assertEquals(
      decisions("pvb-conversion-liability").comparisonExposureRank,
      Some(0)
    )
    assertEquals(
      decisions("pvb-conversion-resource").comparisonExposureRank,
      Some(1)
    )
    val rankedSelections = List(
      (liability.causeRef, decisions("pvb-conversion-liability"), List(liabilityChannel)),
      (resource.causeRef, decisions("pvb-conversion-resource"), List(resourceChannel))
    ).flatMap { case (ref, decision, channels) =>
      for
        effectMode <- decision.effectMode
        rank <- decision.comparisonExposureRank
        selection <- PlayerFacingCauseSelectionPolicy.build(
          causeEvidence = ref,
          status = decision.status,
          effectMode = effectMode,
          channels = channels,
          onlyMoveQualifier = None
        )
      yield selection.copy(comparisonExposureRank = rank)
    }
    val unmeasuredImportance = DirectCauseImportanceResolution(
      selectedCauseEvidenceIds = rankedSelections.map(_.causeEvidence.id),
      profiles = Nil,
      relations = Nil,
      decisions = rankedSelections.map(selection =>
        DirectCauseImportanceDecision(
          causeEvidenceId = selection.causeEvidence.id,
          measuredChannelSignatures = Nil,
          unmeasuredChannelSignatures = selection.channels.map(_.causalSignature),
          dominatingCauseEvidenceIds = Nil,
          dominatedWithinDomain = false
        )
      )
    )
    val ordered = PlayerFacingIdeaOrderingPolicy.order(
      selections = rankedSelections,
      ideaUnits = PlayerFacingIdeaUnitPolicy.resolve(rankedSelections, Nil),
      causeImportanceResolution = unmeasuredImportance,
      ideaImportanceResolution = unmeasuredImportance
    )
    assertEquals(
      ordered.selections.map(selection =>
        (selection.causeEvidence.id, selection.comparisonExposureRank, selection.selectionOrder)
      ),
      List(
        ("pvb-conversion-liability", 0, 0),
        ("pvb-conversion-resource", 1, 1)
      )
    )
    val orphanResource = CrossComparisonCauseExposurePolicy
      .resolveRetainedPvbResponsibility(
        retainedDecisions = List(preliminaryById("pvb-conversion-resource")),
        causes = causes,
        graph = graph,
        playedMoves = Set(played.rootMove)
      )
      .decisions.head
    assertEquals(
      orphanResource.status,
      CrossComparisonExposureStatus.SelectedPrimary,
      "a liability removed by fallback dominance cannot keep the resource complementary"
    )
    assertEquals(orphanResource.comparisonExposureRank, Some(0))

    val responsibility = CrossComparisonCauseExposurePolicy.resolveRetainedPvbResponsibility(
      retainedDecisions = preliminary,
      causes = causes,
      graph = graph,
      playedMoves = Set(played.rootMove)
    )
    val ideaUnits = PlayerFacingIdeaUnitPolicy.resolve(
      rankedSelections,
      responsibility.exactPairs
    )
    assertEquals(
      responsibility.exactPairs,
      List(ExactPvbResponsibilityPair(liability.causeRef.id, resource.causeRef.id))
    )
    assertEquals(ideaUnits.size, 1)
    assertEquals(ideaUnits.head.id, liability.causeRef.id)
    assertEquals(ideaUnits.head.leadCauseEvidenceId, liability.causeRef.id)
    assertEquals(
      ideaUnits.head.memberCauseEvidenceIds,
      List(liability.causeRef.id, resource.causeRef.id)
    )
    assertEquals(ideaUnits.head.kind, PlayerFacingIdeaUnitKind.ExactPvbResponsibility)

  test("sibling PVB liabilities cannot pool channels to demote one resource"):
    val e7LiabilityProof = endgameRecord(
      "pvb-e7-liability-proof",
      played,
      "Lucena",
      status = LineEndgameTechniqueHorizonStatus.Failed,
      requiredSquares = List("e7")
    )
    val d7LiabilityProof = endgameRecord(
      "pvb-d7-liability-proof",
      played,
      "Lucena",
      status = LineEndgameTechniqueHorizonStatus.Failed,
      requiredSquares = List("d7")
    )
    val resourceProof = endgameRecordWithHorizons(
      "pvb-multi-resource-proof",
      best,
      List(
        endgameHorizon(best, "Lucena", LineEndgameTechniqueHorizonStatus.Active, List("e7")),
        endgameHorizon(best, "Lucena", LineEndgameTechniqueHorizonStatus.Active, List("d7"))
      )
    )
    val e7Liability = causeCase(
      "pvb-e7-liability",
      CandidateComparisonKind.PlayedVsBest,
      best,
      played,
      -12.0,
      RelativeCauseSourceSide.Candidate,
      CauseAttributionKind.CandidateAllowsLiability,
      e7LiabilityProof,
      causeKind = RelativeCauseKind.ConversionMiss
    )
    val d7Liability = causeCase(
      "pvb-d7-liability",
      CandidateComparisonKind.PlayedVsBest,
      best,
      played,
      -12.0,
      RelativeCauseSourceSide.Candidate,
      CauseAttributionKind.CandidateAllowsLiability,
      d7LiabilityProof,
      causeKind = RelativeCauseKind.ConversionMiss
    )
    val resource = causeCase(
      "pvb-multi-resource",
      CandidateComparisonKind.PlayedVsBest,
      best,
      played,
      -12.0,
      RelativeCauseSourceSide.Reference,
      CauseAttributionKind.ReferenceCreatesResource,
      resourceProof,
      causeKind = RelativeCauseKind.ConversionSecured
    )
    val all = List(e7Liability, d7Liability, resource)
    val decisions = resolve(all, graphOf(all.flatMap(_.records)*))

    assertEquals(decisions("pvb-e7-liability").status, CrossComparisonExposureStatus.SelectedPrimary)
    assertEquals(decisions("pvb-d7-liability").status, CrossComparisonExposureStatus.SelectedPrimary)
    assertEquals(decisions("pvb-multi-resource").status, CrossComparisonExposureStatus.SelectedPrimary)

  test("root-triggered endgame horizons remain eligible without a generic delayed actor"):
    val conversionLine = LineNodeRef("conversion", "e2e6", 2, LineNodeRole.Alternative)
    val lineRef = evidenceRef(
      "conversion-line-proof",
      EvidenceProducer.LegalLineProducer,
      EvidenceLayer.Line,
      Some(conversionLine)
    )
    val horizon = LineEndgameTechniqueHorizon(
      pattern = "Lucena",
      rookPattern = None,
      techniqueSide = White,
      entryPlyOffset = 0,
      terminalPlyOffset = 4,
      status = LineEndgameTechniqueHorizonStatus.Active,
      triggerMove = Some(conversionLine.rootMove),
      requiredSquares = List("e7")
    )
    val lineRecord = EvidenceRecord(
      lineRef,
      LineFactEvidence(
        line = conversionLine,
        replay = List(LineReplayStep(
          ply = 0,
          moveUci = conversionLine.rootMove,
          fenBefore = position.fen,
          fenAfter = PrincipalVariationEvidence
            .legalFenAfter(position.fen, conversionLine.rootMove)
            .getOrElse(fail("expected legal conversion root move"))
        )),
        endgameHorizons = List(horizon)
      )
    )
    val conversion = causeCase(
      "conversion-resource",
      CandidateComparisonKind.PlayedVsAlternative,
      conversionLine,
      played,
      -8.0,
      RelativeCauseSourceSide.Reference,
      CauseAttributionKind.ReferenceCreatesResource,
      lineRecord,
      causeKind = RelativeCauseKind.ConversionSecured
    )
    val decisions = resolve(List(conversion), graphOf(conversion.records*))

    assertEquals(decisions("conversion-resource").status, CrossComparisonExposureStatus.SelectedComplementary)

  test("an exact completed winning horizon secures conversion only with concrete root-owned squares"):
    val conversionLine = LineNodeRef("completed-conversion", "e2e6", 2, LineNodeRole.Alternative)
    val lineRef = evidenceRef(
      "completed-conversion-line-proof",
      EvidenceProducer.LegalLineProducer,
      EvidenceLayer.Line,
      Some(conversionLine)
    )
    val replay = legalReplay(
      position.fen,
      List(conversionLine.rootMove, "e8d8", "e6d6", "d8e8", "d6e6")
    )
    val completed = LineEndgameTechniqueHorizon(
      pattern = "Lucena",
      rookPattern = None,
      techniqueSide = White,
      entryPlyOffset = 0,
      terminalPlyOffset = 4,
      status = LineEndgameTechniqueHorizonStatus.Completed,
      triggerMove = Some(conversionLine.rootMove),
      requiredSquares = List("e7")
    )
    val line = LineFactEvidence(
      line = conversionLine,
      replay = replay,
      endgameHorizons = List(completed)
    )
    val lineRecord = EvidenceRecord(lineRef, line)
    val conversion = causeCase(
      "completed-conversion-resource",
      CandidateComparisonKind.PlayedVsAlternative,
      conversionLine,
      played,
      -8.0,
      RelativeCauseSourceSide.Reference,
      CauseAttributionKind.ReferenceCreatesResource,
      lineRecord,
      causeKind = RelativeCauseKind.ConversionSecured
    )
    val decisions = resolve(List(conversion), graphOf(conversion.records*))

    assertEquals(
      line.endgameTechniquesTriggeredByRootMove(
        conversionLine.rootMove,
        RelativeCauseKind.ConversionSecured
      ),
      List(completed)
    )
    assertEquals(
      line.copy(endgameHorizons = List(completed.copy(requiredSquares = Nil)))
        .endgameTechniquesTriggeredByRootMove(
          conversionLine.rootMove,
          RelativeCauseKind.ConversionSecured
        ),
      Nil
    )
    assertEquals(
      decisions("completed-conversion-resource").status,
      CrossComparisonExposureStatus.SelectedComplementary
    )

  test("readiness is family-neutral but rejects a direct proof without the complete root actor"):
    val validProof = threatRecord("ready-threat-proof", best, best.rootMove, "e1")
    val valid = causeCase(
      "ready-cause",
      CandidateComparisonKind.PlayedVsBest,
      best,
      played,
      -12.0,
      RelativeCauseSourceSide.Reference,
      CauseAttributionKind.ReferenceCreatesResource,
      validProof,
      causeKind = RelativeCauseKind.OnlyDefenseNecessity
    )
    val incompleteProof = threatRecord("incomplete-actor-proof", best, "e2e6", "e1")
    val incomplete = causeCase(
      "incomplete-actor-cause",
      CandidateComparisonKind.PlayedVsBest,
      best,
      played,
      -12.0,
      RelativeCauseSourceSide.Reference,
      CauseAttributionKind.ReferenceCreatesResource,
      incompleteProof,
      causeKind = RelativeCauseKind.OnlyDefenseNecessity
    )
    val graph = graphOf((valid.records ++ incomplete.records)* )
    val tactical = claim("tactical-host", ClaimFamily.Tactical, valid.causeRef, played)
    val defensive = claim("defensive-host", ClaimFamily.Defensive, valid.causeRef, played)
    val invalid = claim("invalid-host", ClaimFamily.Tactical, incomplete.causeRef, played)

    assertEquals(
      PlayerFacingCauseReadinessPolicy.collect(tactical, graph).map(_._2.id),
      List(valid.causeRef.id)
    )
    assertEquals(
      PlayerFacingCauseReadinessPolicy.collect(defensive, graph).map(_._2.id),
      List(valid.causeRef.id)
    )
    assertEquals(PlayerFacingCauseReadinessPolicy.collect(invalid, graph), Nil)

  test("played best owns its exact value through BestVsSecond"):
    val second = LineNodeRef("played-best-second", "e2e6", 2, LineNodeRole.Alternative)
    val bestProof = motifRecord("played-best-proof", best, "e8")
    val bestValue = causeCase(
      "played-best-value",
      CandidateComparisonKind.BestVsSecond,
      best,
      second,
      -12.0,
      RelativeCauseSourceSide.Reference,
      CauseAttributionKind.ReferenceCreatesResource,
      bestProof,
      candidateSet = Some(CandidateSetDescriptor(CandidateSetType.OnlyMove))
    )
    val decisions = resolve(
      List(bestValue),
      graphOf(bestValue.records*),
      Set(best.rootMove)
    )

    assertEquals(decisions("played-best-value").status, CrossComparisonExposureStatus.SelectedPrimary)
    assertEquals(
      decisions("played-best-value").effectMode,
      Some(PlayerFacingCauseEffectMode.PlayedValue)
    )

  test("BestVsSecond outranks an equivalent PlayedVsAlternative played-value cause across source ids"):
    val second = LineNodeRef("dedup-second", "e2e6", 2, LineNodeRole.Alternative)
    val bvsProof = motifRecord("bvs-proof-source", best, "e8")
    val pvaProof = motifRecord("pva-proof-source", best, "e8")
    val bvs = causeCase(
      "bvs-played-value",
      CandidateComparisonKind.BestVsSecond,
      best,
      second,
      -12.0,
      RelativeCauseSourceSide.Reference,
      CauseAttributionKind.ReferenceCreatesResource,
      bvsProof,
      candidateSet = Some(CandidateSetDescriptor(CandidateSetType.OnlyMove))
    )
    val pva = causeCase(
      "pva-played-value",
      CandidateComparisonKind.PlayedVsAlternative,
      second,
      best,
      8.0,
      RelativeCauseSourceSide.Candidate,
      CauseAttributionKind.CandidateCreatesValue,
      pvaProof
    )
    val all = List(bvs, pva)
    val decisions = resolve(all, graphOf(all.flatMap(_.records)*), Set(best.rootMove))

    assertEquals(decisions("bvs-played-value").status, CrossComparisonExposureStatus.SelectedPrimary)
    assertEquals(decisions("pva-played-value").status, CrossComparisonExposureStatus.RedundantAcrossComparison)
    assertEquals(decisions("pva-played-value").representativeCauseEvidenceId, "bvs-played-value")

  test("a representation-only duplicate collapses but a distinct mechanism remains independent"):
    val alternative = LineNodeRef("mechanism-alternative", "e2e6", 2, LineNodeRole.Alternative)
    val firstProof = endgameRecord("vancura-proof", alternative, "VancuraDefense")
    val duplicateProof = endgameRecord("vancura-proof-copy", alternative, "VancuraDefense")
    val distinctProof = endgameRecord("philidor-proof", alternative, "PhilidorDefense")
    val first = causeCase(
      "vancura-resource",
      CandidateComparisonKind.PlayedVsAlternative,
      alternative,
      played,
      -8.0,
      RelativeCauseSourceSide.Reference,
      CauseAttributionKind.ReferenceCreatesResource,
      firstProof,
      causeKind = RelativeCauseKind.DrawResource
    )
    val duplicate = causeCase(
      "vancura-resource-copy",
      CandidateComparisonKind.PlayedVsAlternative,
      alternative,
      played,
      -8.0,
      RelativeCauseSourceSide.Reference,
      CauseAttributionKind.ReferenceCreatesResource,
      duplicateProof,
      causeKind = RelativeCauseKind.DrawResource
    )
    val distinct = causeCase(
      "philidor-resource",
      CandidateComparisonKind.PlayedVsAlternative,
      alternative,
      played,
      -8.0,
      RelativeCauseSourceSide.Reference,
      CauseAttributionKind.ReferenceCreatesResource,
      distinctProof,
      causeKind = RelativeCauseKind.DrawResource
    )
    val all = List(first, duplicate, distinct)
    val decisions = resolve(all, graphOf(all.flatMap(_.records)*))

    assert(decisions("vancura-resource").selected)
    assertEquals(decisions("vancura-resource-copy").status, CrossComparisonExposureStatus.RedundantAcrossComparison)
    assert(decisions("philidor-resource").selected)

  test("same exact causal truth contradiction fails closed across Cause kinds"):
    val exactPosition = PositionNodeRef(
      "6k1/8/2r5/8/1r5r/8/4Q3/4K3 w - - 0 1",
      0,
      Some(White)
    )
    val exactReference = LineNodeRef("exact-reference", "e2e8", 1, LineNodeRole.Alternative)
    val exactPlayed = LineNodeRef("exact-played", "e1f1", 9, LineNodeRole.Played)
    val weak = materialCauseCase(
      "exact-weak-material",
      exactPosition,
      exactReference,
      exactPlayed,
      List("e2e8", "g8g7", "e8c6"),
      valueCp = 100,
      causeKind = RelativeCauseKind.MaterialSwing
    )
    val strongOtherKind = materialCauseCase(
      "exact-strong-compensation",
      exactPosition,
      exactReference,
      exactPlayed,
      List("e2e8", "g8g7", "e8c6"),
      valueCp = 500,
      causeKind = RelativeCauseKind.SacrificeCompensation
    )
    val all = List(weak, strongOtherKind)
    val decisions = resolve(
      all,
      graphOf(all.flatMap(_.records)*),
      Set(exactPlayed.rootMove)
    )

    all.foreach { item =>
      val decision = decisions(item.causeRef.id)
      assertEquals(decision.status, CrossComparisonExposureStatus.DiagnosticComparison)
      assertEquals(decision.reason, CrossComparisonExposureReason.ConflictingRootOwnedEffectTruth)
      assertEquals(decision.representativeCauseEvidenceId, item.causeRef.id)
    }

  test("same exact effect chooses one representative while proof-segment disagreement stays null"):
    val exactPosition = PositionNodeRef(
      "6k1/8/2r5/8/1r5r/8/4Q3/4K3 w - - 0 1",
      0,
      Some(White)
    )
    val exactReference = LineNodeRef("segment-reference", "e2e8", 1, LineNodeRole.Alternative)
    val exactPlayed = LineNodeRef("segment-played", "e1f1", 9, LineNodeRole.Played)
    val viaG7 = materialCauseCase(
      "segment-via-g7",
      exactPosition,
      exactReference,
      exactPlayed,
      List("e2e8", "g8g7", "e8c6"),
      valueCp = 500,
      causeKind = RelativeCauseKind.MaterialSwing
    )
    val viaH7 = materialCauseCase(
      "segment-via-h7",
      exactPosition,
      exactReference,
      exactPlayed,
      List("e2e8", "g8h7", "e8c6"),
      valueCp = 500,
      causeKind = RelativeCauseKind.MaterialSwing
    )
    val all = List(viaG7, viaH7)
    val graph = graphOf(all.flatMap(_.records)*)
    val decisions = resolve(
      all,
      graph,
      Set(exactPlayed.rootMove)
    )

    assertEquals(
      decisions.values.map(_.status).toSet,
      Set(
        CrossComparisonExposureStatus.SelectedComplementary,
        CrossComparisonExposureStatus.RedundantAcrossComparison
      )
    )
    val representative = decisions.values.find(_.selected)
      .getOrElse(fail("expected one selected causal representative"))
    assertEquals(decisions.values.count(_.selected), 1)
    assertEquals(
      decisions.values.map(_.representativeCauseEvidenceId).toSet,
      Set(representative.causeEvidenceId)
    )
    assertEquals(
      decisions.values.filterNot(_.selected).map(_.reason).toSet,
      Set(CrossComparisonExposureReason.HigherAuthorityEquivalentCause)
    )

    val canonicalChannels = EvidenceObjectBinding.canonicalCauseChannels(
      all.flatMap(item =>
        val registered = ExplicitCauseAdmissionTestSupport
          .registeredCause(item.causeRef, graph)
          .getOrElse(fail(s"expected registered Cause ${item.causeRef.id}"))
        RelativeCauseConstructionAdmission.admittedDirectChannels(registered, graph)
      )
    )
    assertEquals(canonicalChannels.size, 1)
    assert(canonicalChannels.head.proofSegmentAmbiguous)
    assertEquals(canonicalChannels.head.proofSegment, None)

  test("cross-line novelty with different exact descriptors preserves both Causes"):
    val capturePosition = PositionNodeRef(
      "4k3/8/8/8/8/8/4K3/R2q3R w - - 0 1",
      0,
      Some(White)
    )
    val left = LineNodeRef("left-capture", "a1d1", 2, LineNodeRole.Alternative)
    val right = LineNodeRef("right-capture", "h1d1", 3, LineNodeRole.Alternative)
    val capturePlayed = LineNodeRef("capture-played", "e2e3", 9, LineNodeRole.Played)
    val leftCase = materialCauseCase(
      "left-magnitude",
      capturePosition,
      left,
      capturePlayed,
      List(left.rootMove),
      valueCp = 900,
      causeKind = RelativeCauseKind.MaterialSwing,
      capturedRole = "queen"
    )
    val rightCase = materialCauseCase(
      "right-magnitude",
      capturePosition,
      right,
      capturePlayed,
      List(right.rootMove),
      valueCp = 500,
      causeKind = RelativeCauseKind.MaterialSwing,
      capturedRole = "queen"
    )
    val all = List(leftCase, rightCase)
    val decisions = resolve(
      all,
      graphOf(all.flatMap(_.records)*),
      Set(capturePlayed.rootMove)
    )

    assert(decisions("left-magnitude").selected)
    assert(decisions("right-magnitude").selected)
    assertEquals(decisions("left-magnitude").representativeCauseEvidenceId, "left-magnitude")
    assertEquals(decisions("right-magnitude").representativeCauseEvidenceId, "right-magnitude")

  test("PVA liability cannot borrow PVB authority across an exact descriptor contradiction"):
    val primaryProof = liabilityRecord("descriptor-pvb-proof", played, valueCp = 900)
    val pvaProof = liabilityRecord("descriptor-pva-proof", played, valueCp = 500)
    val second = LineNodeRef("descriptor-second", "e2e6", 2, LineNodeRole.Alternative)
    val primary = causeCase(
      "descriptor-pvb",
      CandidateComparisonKind.PlayedVsBest,
      best,
      played,
      -12.0,
      RelativeCauseSourceSide.Candidate,
      CauseAttributionKind.CandidateAllowsLiability,
      primaryProof,
      causeKind = RelativeCauseKind.CandidateTacticalLiability
    )
    val pva = causeCase(
      "descriptor-pva",
      CandidateComparisonKind.PlayedVsAlternative,
      second,
      played,
      -8.0,
      RelativeCauseSourceSide.Candidate,
      CauseAttributionKind.CandidateAllowsLiability,
      pvaProof,
      causeKind = RelativeCauseKind.CandidateTacticalLiability
    )
    val all = List(primary, pva)
    val decisions = resolve(all, graphOf(all.flatMap(_.records)*))

    all.foreach { item =>
      val decision = decisions(item.causeRef.id)
      assertEquals(decision.status, CrossComparisonExposureStatus.DiagnosticComparison)
      assertEquals(decision.reason, CrossComparisonExposureReason.ConflictingRootOwnedEffectTruth)
      assertEquals(decision.representativeCauseEvidenceId, item.causeRef.id)
    }

  private final case class CauseCase(
      cause: RelativeCauseFact,
      causeRef: EvidenceRef,
      records: List[EvidenceRecord]
  )

  private def materialCauseCase(
      id: String,
      rootPosition: PositionNodeRef,
      referenceLine: LineNodeRef,
      candidateLine: LineNodeRef,
      moves: List[String],
      valueCp: Int,
      causeKind: RelativeCauseKind,
      capturedRole: String = "rook"
  ): CauseCase =
    val comparisonRef = customEvidenceRef(
      s"$id-comparison",
      EvidenceProducer.RelativeMoveProducer,
      EvidenceLayer.CandidateComparison,
      rootPosition,
      Some(candidateLine),
      EvidenceConfidence.EngineBacked
    )
    val proofRef = customEvidenceRef(
      s"$id-proof",
      EvidenceProducer.LegalLineProducer,
      EvidenceLayer.Line,
      rootPosition,
      Some(referenceLine),
      EvidenceConfidence.LegalReplayVerified
    )
    val causeRef = customEvidenceRef(
      id,
      EvidenceProducer.RelativeMoveProducer,
      EvidenceLayer.RelativeCause,
      rootPosition,
      Some(referenceLine),
      EvidenceConfidence.EngineBacked
    )
    val replay = legalReplay(rootPosition.fen, moves)
    val eventMove = moves.last
    val eventOffset = moves.size - 1
    val capture = LineMaterialCapture(
      eventMove,
      eventOffset,
      White,
      EvidencePieceRole(if moves.size == 1 then "rook" else "queen"),
      EvidencePieceRole(capturedRole),
      EvidenceSquare(eventMove.slice(2, 4)),
      valueCp,
      recapture = false
    )
    val consequence = LineConsequence(
      LineConsequenceKind.MaterialGain,
      moves,
      proofSignal = true,
      eventMove = Some(eventMove),
      rootMove = Some(referenceLine.rootMove),
      rootSide = Some(White),
      beneficiary = Some(White),
      materialOutcome = Some(RootOwnedMaterialOutcome(
        event = RootOwnedMaterialEventSalience(
          moveUci = capture.moveUci,
          plyOffset = capture.plyOffset,
          capturedRole = capture.capturedRole,
          square = capture.square,
          targetValueCp = capture.valueCp
        ),
        beneficiary = White,
        durableNetCp = valueCp
      ))
    )
    val line = LineFactEvidence(
      line = referenceLine,
      material = Some(LineMaterialSummary(
        White,
        List(capture),
        valueCp,
        valueCp,
        0,
        hasRecaptureChain = false,
        hasRecoveryWindow = false,
        promotionGainCpForMover = 0,
        materialWindowComplete = true
      )),
      replay = replay,
      consequences = List(consequence)
    )
    val comparison = CandidateComparisonFact(
      CandidateComparisonKind.PlayedVsAlternative,
      referenceLine,
      candidateLine,
      EvalComparison(White, -8.0, MoveChoiceVerdict.Inaccuracy)
    )
    val cause = RelativeCauseFact(
      causeKind,
      comparisonRef,
      List(proofRef),
      RelativeCauseSourceSide.Reference,
      CauseAttribution(
        CauseAttributionKind.ReferenceCreatesResource,
        rootMoveMatched = true,
        directProofEligible = true
      ),
      Some(RelativeCauseProof(
        RelativeCauseProofSection(
          RelativeCauseProofRole.DirectProof,
          RelativeCauseProofStrength.Primary,
          List(proofRef)
        )
      ))
    )
    CauseCase(
      cause,
      causeRef,
      List(
        EvidenceRecord(comparisonRef, CandidateComparisonEvidence(comparison)),
        EvidenceRecord(proofRef, line),
        EvidenceRecord(causeRef, RelativeCauseFactEvidence(cause))
      )
    )

  private def customEvidenceRef(
      id: String,
      producer: EvidenceProducer,
      layer: EvidenceLayer,
      rootPosition: PositionNodeRef,
      line: Option[LineNodeRef],
      confidence: EvidenceConfidence
  ): EvidenceRef =
    EvidenceRef(
      id,
      producer,
      layer,
      rootPosition,
      line,
      line.map(_.role.scope).getOrElse(EvidenceScope.Counterfactual),
      confidence
    )

  private def referenceResource(
      id: String,
      reference: LineNodeRef,
      target: String,
      delta: Double
  ): CauseCase =
    val proof = motifRecord(s"$id-proof", reference, target)
    causeCase(
      id,
      CandidateComparisonKind.PlayedVsAlternative,
      reference,
      played,
      delta,
      RelativeCauseSourceSide.Reference,
      CauseAttributionKind.ReferenceCreatesResource,
      proof
    )

  private def playedValue(
      id: String,
      reference: LineNodeRef,
      proof: EvidenceRecord,
      delta: Double
  ): CauseCase =
    causeCase(
      id,
      CandidateComparisonKind.PlayedVsAlternative,
      reference,
      played,
      delta,
      RelativeCauseSourceSide.Candidate,
      CauseAttributionKind.CandidateCreatesValue,
      proof
    )

  private def causeCase(
      id: String,
      kind: CandidateComparisonKind,
      reference: LineNodeRef,
      candidate: LineNodeRef,
      delta: Double,
      sourceSide: RelativeCauseSourceSide,
      attributionKind: CauseAttributionKind,
      proofRecord: EvidenceRecord,
      causeKind: RelativeCauseKind = RelativeCauseKind.KingForcing,
      candidateSet: Option[CandidateSetDescriptor] = None,
      verdictOverride: Option[MoveChoiceVerdict] = None
  ): CauseCase =
    val comparisonRef = evidenceRef(
      s"$id-comparison",
      EvidenceProducer.RelativeMoveProducer,
      EvidenceLayer.CandidateComparison,
      Some(candidate)
    )
    val comparison = CandidateComparisonFact(
      kind,
      reference,
      candidate,
      EvalComparison(White, delta, verdictOverride.getOrElse(verdict(delta))),
      candidateSet
    )
    val cause = RelativeCauseFact(
      kind = causeKind,
      comparisonEvidence = comparisonRef,
      supportEvidence = List(proofRecord.ref),
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
          List(proofRecord.ref)
        )
      ))
    )
    val eventLine = if sourceSide == RelativeCauseSourceSide.Candidate then candidate else reference
    val causeRef = evidenceRef(id, EvidenceProducer.RelativeMoveProducer, EvidenceLayer.RelativeCause, Some(eventLine))
    CauseCase(
      cause,
      causeRef,
      List(
        EvidenceRecord(comparisonRef, CandidateComparisonEvidence(comparison)),
        proofRecord,
        EvidenceRecord(causeRef, RelativeCauseFactEvidence(cause))
      )
    )

  private def verdict(delta: Double): MoveChoiceVerdict =
    if delta >= 2.5 then MoveChoiceVerdict.ImprovesOnReference
    else if delta <= -10.0 then MoveChoiceVerdict.Blunder
    else if delta <= -2.5 then MoveChoiceVerdict.Inaccuracy
    else MoveChoiceVerdict.MatchesReference

  private def motifRecord(id: String, line: LineNodeRef, target: String): EvidenceRecord =
    val square = Square.fromKey(target).getOrElse(fail(s"invalid square $target"))
    val payload = MoveMotifEvidence(
      MoveMotifEvent.fromMotif(
        line.rootMove,
        Motif.Check(Queen, square, Motif.CheckType.Normal, White, 0, Some(line.rootMove))
      )
    )
    val ref = evidenceRef(id, EvidenceProducer.MoveMotifProducer, EvidenceLayer.MoveMotif, Some(line))
    EvidenceRecord(ref, payload)

  private def liabilityRecord(
      id: String,
      line: LineNodeRef,
      valueCp: Int = 900
  ): EvidenceRecord =
    val rootAfter = PrincipalVariationEvidence
      .legalFenAfter(position.fen, line.rootMove)
      .getOrElse(fail(s"expected legal liability root ${line.rootMove}"))
    val replyMove = "a3e3"
    val replyAfter = PrincipalVariationEvidence
      .legalFenAfter(rootAfter, replyMove)
      .getOrElse(fail(s"expected legal liability reply $replyMove"))
    val capture = LineMaterialCapture(
      moveUci = replyMove,
      plyOffset = 1,
      side = Black,
      attackerRole = EvidencePieceRole("rook"),
      capturedRole = EvidencePieceRole("queen"),
      square = EvidenceSquare("e3"),
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
        event = RootOwnedMaterialEventSalience(
          moveUci = replyMove,
          plyOffset = 1,
          capturedRole = capture.capturedRole,
          square = capture.square,
          targetValueCp = capture.valueCp
        ),
        beneficiary = Black,
        durableNetCp = valueCp
      ))
    )
    val payload = LineFactEvidence(
      line = line,
      material = Some(LineMaterialSummary(
        sideToMove = White,
        captures = List(capture),
        netCaptureCpForMover = -valueCp,
        maxGainCpForMover = 0,
        maxLossCpForMover = valueCp,
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
        kind = LineEventKind.Capture,
        moveUci = replyMove,
        plyOffset = 1,
        side = Some(Black),
        pieceRole = Some(EvidencePieceRole("rook")),
        targetRole = Some(EvidencePieceRole("queen")),
        square = Some(EvidenceSquare("e3"))
      )),
      consequences = List(consequence)
    )
    EvidenceRecord(
      evidenceRef(id, EvidenceProducer.LegalLineProducer, EvidenceLayer.Line, Some(line)),
      payload
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
    val ref = evidenceRef(id, EvidenceProducer.ThreatPressureProducer, EvidenceLayer.ThreatPressure, Some(line))
    EvidenceRecord(ref, ThreatEpisodeEvidence(ThreatEpisode.fromThreat(threat, 0)))

  private def endgameRecord(id: String, line: LineNodeRef, pattern: String): EvidenceRecord =
    endgameRecord(
      id,
      line,
      pattern,
      LineEndgameTechniqueHorizonStatus.Active,
      List("e7")
    )

  private def endgameRecord(
      id: String,
      line: LineNodeRef,
      pattern: String,
      status: LineEndgameTechniqueHorizonStatus,
      requiredSquares: List[String] = List("e7")
  ): EvidenceRecord =
    endgameRecordWithHorizons(
      id,
      line,
      List(endgameHorizon(line, pattern, status, requiredSquares))
    )

  private def endgameRecordWithHorizons(
      id: String,
      line: LineNodeRef,
      horizons: List[LineEndgameTechniqueHorizon]
  ): EvidenceRecord =
    val fenAfter = PrincipalVariationEvidence
      .legalFenAfter(position.fen, line.rootMove)
      .getOrElse(fail(s"expected legal root move ${line.rootMove}"))
    val payload = LineFactEvidence(
      line = line,
      replay = List(LineReplayStep(0, line.rootMove, position.fen, fenAfter)),
      endgameHorizons = horizons
    )
    val ref = evidenceRef(id, EvidenceProducer.LegalLineProducer, EvidenceLayer.Line, Some(line))
    EvidenceRecord(ref, payload)

  private def endgameHorizon(
      line: LineNodeRef,
      pattern: String,
      status: LineEndgameTechniqueHorizonStatus,
      requiredSquares: List[String]
  ): LineEndgameTechniqueHorizon =
    LineEndgameTechniqueHorizon(
      pattern = pattern,
      rookPattern = None,
      techniqueSide = White,
      entryPlyOffset = 0,
      terminalPlyOffset = 4,
      status = status,
      triggerMove = Some(line.rootMove),
      requiredSquares = requiredSquares
    )

  private def claim(
      id: String,
      family: ClaimFamily,
      causeRef: EvidenceRef,
      line: LineNodeRef
  ): JudgmentClaim =
    JudgmentClaim(
      id = id,
      family = family,
      subject = ClaimSubject.PlayedMove,
      primaryPosition = position,
      primaryLine = Some(line),
      subjectMove = Some(line.rootMove),
      evidence = List(causeRef),
      scope = line.role.scope,
      confidence = EvidenceConfidence.EngineBacked
    )

  private def evidenceRef(
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

  private def legalReplay(fen: String, moves: List[String]): List[LineReplayStep] =
    moves.zipWithIndex.foldLeft(fen -> List.empty[LineReplayStep]) {
      case ((fenBefore, steps), (move, index)) =>
        val fenAfter = PrincipalVariationEvidence
          .legalFenAfter(fenBefore, move)
          .getOrElse(fail(s"expected legal test move $move after $fenBefore"))
        fenAfter -> (steps :+ LineReplayStep(index, move, fenBefore, fenAfter))
    }._2

  private def resolve(
      cases: List[CauseCase],
      graph: TypedEvidenceGraph,
      playedMoves: Set[String] = Set(played.rootMove),
      fallbackObligationsByCauseId: Map[String, Set[String]] = Map.empty
  ): Map[String, CrossComparisonExposureDecision] =
    val causes = cases.map(item =>
      ExplicitCauseAdmissionTestSupport
        .registeredCause(item.causeRef, graph)
        .getOrElse(fail(s"expected registered Cause ${item.causeRef.id}")) -> item.causeRef
    )
    val preliminary = CrossComparisonCauseExposurePolicy.resolve(
      causes,
      graph,
      playedMoves,
      fallbackObligationsByCauseId
    )
    CrossComparisonCauseExposurePolicy
      .resolveRetainedPvbResponsibility(
        preliminary,
        causes,
        graph,
        playedMoves
      )
      .decisions
      .map(decision => decision.causeEvidenceId -> decision)
      .toMap

  private def graphOf(records: EvidenceRecord*): TypedEvidenceGraph =
    ExplicitCauseAdmissionTestSupport.graph(records.distinctBy(_.ref.id).toList)
