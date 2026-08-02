package lila.chessjudgment.model.judgment

import chess.{ Queen, White }

class CrossComparisonFallbackSpecificityTest extends munit.FunSuite:

  private val position = PositionNodeRef(
    "1r2k3/8/8/8/8/8/4Q3/4K3 w - - 0 1",
    0,
    Some(White)
  )
  private val best = LineNodeRef("best", "e2b5", 1, LineNodeRole.BestReference)
  private val alternative = LineNodeRef("alternative", "e2e7", 2, LineNodeRole.Alternative)
  private val played = LineNodeRef("played", "e2e3", 9, LineNodeRole.Played)

  test("a complementary specific Cause cannot lower a primary fallback's exposure authority"):
    val genericProof = zwischenzugRecord("generic-proof", best)
    val specificProof = zwischenzugRecord("specific-proof", best)
    val generic = causeCase(
      "generic-cause",
      RelativeCauseKind.MissedTacticalResource,
      CandidateComparisonKind.PlayedVsBest,
      best,
      -12.0,
      genericProof
    )
    val specific = causeCase(
      "specific-cause",
      RelativeCauseKind.WrongMoveOrder,
      CandidateComparisonKind.PlayedVsAlternative,
      best,
      -8.0,
      specificProof
    )
    val all = List(generic, specific)
    val resolved = resolve(all)
    val decisions = resolved.cross

    assert(decisions("specific-cause").selected)
    assert(decisions("generic-cause").selected)
    assert(resolved.dominance("generic-cause").retained)
    assert(resolved.dominance("specific-cause").retained)

  test("a primary specific Cause replaces a complementary generic fallback on the same channel"):
    val genericProof = zwischenzugRecord("complementary-generic-proof", best)
    val specificProof = zwischenzugRecord("primary-specific-proof", best)
    val generic = causeCase(
      "complementary-generic-cause",
      RelativeCauseKind.MissedTacticalResource,
      CandidateComparisonKind.PlayedVsAlternative,
      best,
      -8.0,
      genericProof
    )
    val specific = causeCase(
      "primary-specific-cause",
      RelativeCauseKind.WrongMoveOrder,
      CandidateComparisonKind.PlayedVsBest,
      best,
      -12.0,
      specificProof
    )
    val resolved = resolve(List(generic, specific))

    assert(resolved.cross(specific.causeRef.id).selected)
    assert(!resolved.cross.contains(generic.causeRef.id))
    assertEquals(
      resolved.dominance(generic.causeRef.id).dominatingCauseEvidenceIds,
      List(specific.causeRef.id)
    )
    assert(resolved.dominance(specific.causeRef.id).retained)

  test("pipeline elects a closed retained representative after fallback dominance"):
    val primaryGenericProof = zwischenzugRecord("closed-primary-generic-proof", best)
    val crossLineGenericProof = zwischenzugRecord("closed-cross-line-generic-proof", alternative)
    val specificProof = zwischenzugRecord("closed-specific-proof", best)
    val primaryGeneric = causeCase(
      "closed-primary-generic",
      RelativeCauseKind.MissedTacticalResource,
      CandidateComparisonKind.PlayedVsBest,
      best,
      -12.0,
      primaryGenericProof
    )
    val crossLineGeneric = causeCase(
      "closed-cross-line-generic",
      RelativeCauseKind.MissedTacticalResource,
      CandidateComparisonKind.PlayedVsAlternative,
      alternative,
      -8.0,
      crossLineGenericProof
    )
    val specific = causeCase(
      "closed-specific",
      RelativeCauseKind.WrongMoveOrder,
      CandidateComparisonKind.PlayedVsBest,
      best,
      -12.0,
      specificProof
    )
    val all = List(primaryGeneric, crossLineGeneric, specific)
    val graph = graphOf(all.flatMap(_.records))
    val claims = all.map { item =>
      val line = item.causeRef.line.getOrElse(fail("expected Cause event line"))
      JudgmentClaim(
        id = s"${item.causeRef.id}-host",
        family = ClaimFamily.Tactical,
        subject = ClaimSubject.ReferenceMove,
        primaryPosition = position,
        primaryLine = Some(line),
        subjectMove = Some(line.rootMove),
        evidence = List(item.causeRef),
        scope = line.role.scope,
        confidence = EvidenceConfidence.EngineBacked
      )
    }
    val preliminary = CrossComparisonCauseExposurePolicy.resolve(
      all.map(item => item.cause -> item.causeRef),
      graph,
      Set(played.rootMove)
    ).map(decision => decision.causeEvidenceId -> decision).toMap

    assert(preliminary(primaryGeneric.causeRef.id).selected)
    assertEquals(
      preliminary(crossLineGeneric.causeRef.id).representativeCauseEvidenceId,
      primaryGeneric.causeRef.id
    )

    val resolution = PlayerFacingCauseExposurePipeline.resolve(
      claims,
      graph,
      Set(played.rootMove)
    )
    val cross = resolution.crossDecisions.map(decision => decision.causeEvidenceId -> decision).toMap
    val dominance = resolution.dominanceDecisions.map(decision => decision.causeEvidenceId -> decision).toMap

    assertEquals(
      dominance(primaryGeneric.causeRef.id).dominatingCauseEvidenceIds,
      List(specific.causeRef.id)
    )
    assert(dominance(crossLineGeneric.causeRef.id).retained)
    assert(!cross.contains(primaryGeneric.causeRef.id))
    assert(cross(crossLineGeneric.causeRef.id).selected)
    assertEquals(
      cross(crossLineGeneric.causeRef.id).representativeCauseEvidenceId,
      crossLineGeneric.causeRef.id
    )
    assert(
      cross.values.forall { decision =>
        cross.get(decision.representativeCauseEvidenceId).exists(_.selected)
      }
    )

  test("cross-line novelty cannot absorb the only selected dominator of a fallback"):
    val fallbackProof = zwischenzugRecord("obligation-fallback-proof", alternative)
    val directDominatorProof = zwischenzugRecord("obligation-direct-dominator-proof", alternative)
    val higherAuthorityProof = zwischenzugRecord("obligation-higher-authority-proof", best)
    val fallback = causeCase(
      "obligation-fallback",
      RelativeCauseKind.MissedTacticalResource,
      CandidateComparisonKind.PlayedVsAlternative,
      alternative,
      -8.0,
      fallbackProof
    )
    val directDominator = causeCase(
      "obligation-direct-dominator",
      RelativeCauseKind.WrongMoveOrder,
      CandidateComparisonKind.PlayedVsAlternative,
      alternative,
      -8.0,
      directDominatorProof
    )
    val higherAuthority = causeCase(
      "obligation-higher-authority",
      RelativeCauseKind.WrongMoveOrder,
      CandidateComparisonKind.PlayedVsBest,
      best,
      -12.0,
      higherAuthorityProof
    )
    val all = List(fallback, directDominator, higherAuthority)
    val graph = graphOf(all.flatMap(_.records))
    val directChannel = channels(directDominator, graph).head
    val higherChannel = channels(higherAuthority, graph).head
    val resolution = PlayerFacingCauseExposurePipeline.resolve(
      all.map(hostClaim),
      graph,
      Set(played.rootMove)
    )
    val cross = resolution.crossDecisions.map(decision => decision.causeEvidenceId -> decision).toMap
    val dominance = resolution.dominanceDecisions.map(decision => decision.causeEvidenceId -> decision).toMap

    assertEquals(directChannel.explanatoryNoveltySignature, higherChannel.explanatoryNoveltySignature)
    assertNotEquals(directChannel.causalSignature, higherChannel.causalSignature)
    assertEquals(
      dominance(fallback.causeRef.id).dominatingCauseEvidenceIds,
      List(directDominator.causeRef.id)
    )
    assert(!cross.contains(fallback.causeRef.id))
    assert(cross(directDominator.causeRef.id).selected)
    assert(cross(higherAuthority.causeRef.id).selected)
    assertEquals(cross(directDominator.causeRef.id).representativeCauseEvidenceId, directDominator.causeRef.id)
    assertEquals(cross(higherAuthority.causeRef.id).representativeCauseEvidenceId, higherAuthority.causeRef.id)
    assert(
      resolution.dominanceDecisions
        .filter(_.status == RelativeCauseDominanceStatus.DominatedFallback)
        .forall(decision => decision.dominatingCauseEvidenceIds.exists(id => cross.get(id).exists(_.selected)))
    )

  test("a specific kind on another root actor or line cannot suppress the generic channel"):
    val genericProof = zwischenzugRecord("generic-root-proof", best)
    val specificProof = zwischenzugRecord("specific-root-proof", alternative)
    val generic = causeCase(
      "generic-root-cause",
      RelativeCauseKind.MissedTacticalResource,
      CandidateComparisonKind.PlayedVsBest,
      best,
      -12.0,
      genericProof
    )
    val specific = causeCase(
      "specific-root-cause",
      RelativeCauseKind.WrongMoveOrder,
      CandidateComparisonKind.PlayedVsAlternative,
      alternative,
      -8.0,
      specificProof
    )
    val all = List(generic, specific)
    val graph = graphOf(all.flatMap(_.records))
    val genericChannel = channels(generic, graph).head
    val specificChannel = channels(specific, graph).head
    val resolved = resolve(all, graph)
    val decisions = resolved.cross

    assertEquals(
      genericChannel.explanatoryNoveltySignature,
      specificChannel.explanatoryNoveltySignature
    )
    assertNotEquals(genericChannel.causalSignature, specificChannel.causalSignature)
    assert(decisions(generic.causeRef.id).selected)
    assert(decisions(specific.causeRef.id).selected)
    assert(resolved.dominance.values.forall(_.retained))

  test("incomparable kinds sharing an exact cross-comparison channel remain co-selected"):
    val moveOrderProof = zwischenzugRecord("move-order-proof", best)
    val recapturerProof = zwischenzugRecord("recapturer-proof", best)
    val moveOrder = causeCase(
      "move-order-cause",
      RelativeCauseKind.WrongMoveOrder,
      CandidateComparisonKind.PlayedVsBest,
      best,
      -12.0,
      moveOrderProof
    )
    val recapturer = causeCase(
      "recapturer-cause",
      RelativeCauseKind.WrongRecapturer,
      CandidateComparisonKind.PlayedVsAlternative,
      best,
      -8.0,
      recapturerProof
    )
    val all = List(moveOrder, recapturer)
    val graph = graphOf(all.flatMap(_.records))
    val moveOrderChannel = channels(moveOrder, graph).head
    val recapturerChannel = channels(recapturer, graph).head
    val resolved = resolve(all, graph)
    val decisions = resolved.cross

    assertEquals(moveOrderChannel.causalSignature, recapturerChannel.causalSignature)
    assert(
      !RelativeCauseDominancePolicy.moreSpecificKinds(moveOrder.cause.kind)(recapturer.cause.kind)
    )
    assert(
      !RelativeCauseDominancePolicy.moreSpecificKinds(recapturer.cause.kind)(moveOrder.cause.kind)
    )
    assert(decisions(moveOrder.causeRef.id).selected)
    assert(decisions(recapturer.causeRef.id).selected)
    assert(resolved.dominance.values.forall(_.retained))

  test("a ready but non-selectable specific Cause cannot suppress a public fallback"):
    val genericProof = zwischenzugRecord("selected-generic-proof", best)
    val diagnosticProof = zwischenzugRecord("diagnostic-specific-proof", best)
    val generic = causeCase(
      "selected-generic-cause",
      RelativeCauseKind.MissedTacticalResource,
      CandidateComparisonKind.PlayedVsBest,
      best,
      -12.0,
      genericProof
    )
    val diagnosticSpecific = causeCase(
      "diagnostic-specific-cause",
      RelativeCauseKind.WrongMoveOrder,
      CandidateComparisonKind.PlayedVsAlternative,
      best,
      1.0,
      diagnosticProof
    )
    val resolved = resolve(List(generic, diagnosticSpecific))

    assert(resolved.cross(generic.causeRef.id).selected)
    assert(!resolved.cross(diagnosticSpecific.causeRef.id).selected)
    assert(resolved.dominance(generic.causeRef.id).retained)
    assert(resolved.dominance(diagnosticSpecific.causeRef.id).retained)

  test("sibling specific Causes cannot union their channels to suppress an aggregate fallback"):
    val firstProof = zwischenzugRecord("aggregate-first-proof", best)
    val secondProof = zwischenzugRecord(
      "aggregate-second-proof",
      best,
      expectedRecaptureSquare = "a5"
    )
    val generic = withAdditionalProof(
      causeCase(
        "aggregate-generic-cause",
        RelativeCauseKind.MissedTacticalResource,
        CandidateComparisonKind.PlayedVsBest,
        best,
        -12.0,
        firstProof
      ),
      secondProof
    )
    val firstSpecific = causeCase(
      "aggregate-first-specific",
      RelativeCauseKind.WrongMoveOrder,
      CandidateComparisonKind.PlayedVsBest,
      best,
      -12.0,
      firstProof
    )
    val secondSpecific = causeCase(
      "aggregate-second-specific",
      RelativeCauseKind.WrongMoveOrder,
      CandidateComparisonKind.PlayedVsBest,
      best,
      -12.0,
      secondProof
    )
    val all = List(generic, firstSpecific, secondSpecific)
    val graph = graphOf(all.flatMap(_.records))
    val resolved = resolve(all, graph)

    assertEquals(channels(generic, graph).size, 2)
    assertEquals(channels(firstSpecific, graph).size, 1)
    assertEquals(channels(secondSpecific, graph).size, 1)
    assert(resolved.cross(generic.causeRef.id).selected)
    assert(resolved.cross(firstSpecific.causeRef.id).selected)
    assert(resolved.cross(secondSpecific.causeRef.id).selected)
    assert(resolved.dominance.values.forall(_.retained))

  test("generic and specific kind ordering is directional"):
    assert(
      RelativeCauseDominancePolicy
        .moreSpecificKinds(RelativeCauseKind.MissedTacticalResource)(RelativeCauseKind.WrongMoveOrder)
    )
    assert(
      !RelativeCauseDominancePolicy
        .moreSpecificKinds(RelativeCauseKind.WrongMoveOrder)(RelativeCauseKind.MissedTacticalResource)
    )
    assert(
      !RelativeCauseDominancePolicy
        .moreSpecificKinds(RelativeCauseKind.MaterialSwing)(RelativeCauseKind.CandidateTacticalLiability)
    )
    assert(
      RelativeCauseDominancePolicy
        .moreSpecificKinds(RelativeCauseKind.MaterialSwing)(RelativeCauseKind.TacticalRefutationOfPlayed)
    )
    assert(
      !RelativeCauseDominancePolicy
        .moreSpecificKinds(RelativeCauseKind.TacticalRefutationOfPlayed)(RelativeCauseKind.MaterialSwing)
    )
    assert(
      RelativeCauseDominancePolicy
        .moreSpecificKinds(RelativeCauseKind.MissedTacticalResource)(RelativeCauseKind.MaterialSwing)
    )
    assert(
      RelativeCauseDominancePolicy
        .moreSpecificKinds(RelativeCauseKind.MaterialSwing)(RelativeCauseKind.SacrificeCompensation)
    )
    assert(
      !RelativeCauseDominancePolicy
        .moreSpecificKinds(RelativeCauseKind.MaterialSwing)(RelativeCauseKind.WrongMoveOrder)
    )
    assert(
      RelativeCauseDominancePolicy
        .moreSpecificKinds(RelativeCauseKind.MaterialSwing)(RelativeCauseKind.TempoLoss)
    )
    assert(
      !RelativeCauseDominancePolicy
        .moreSpecificKinds(RelativeCauseKind.MaterialSwing)(RelativeCauseKind.ConversionMiss)
    )
    assert(
      !RelativeCauseDominancePolicy
        .moreSpecificKinds(RelativeCauseKind.MissedTacticalResource)(RelativeCauseKind.OnlyDefenseNecessity)
    )
    assert(
      !RelativeCauseDominancePolicy
        .moreSpecificKinds(RelativeCauseKind.MissedTacticalResource)(RelativeCauseKind.DefensiveResource)
    )
    assert(
      !RelativeCauseDominancePolicy
        .moreSpecificKinds(RelativeCauseKind.TacticalRefutationOfPlayed)(RelativeCauseKind.WrongMoveOrder)
    )
    assert(
      !RelativeCauseDominancePolicy
        .moreSpecificKinds(RelativeCauseKind.TempoLoss)(RelativeCauseKind.WrongMoveOrder)
    )
    assert(
      !RelativeCauseDominancePolicy
        .moreSpecificKinds(RelativeCauseKind.StructuralImprovement)(RelativeCauseKind.ConversionSecured)
    )
    assert(
      !RelativeCauseDominancePolicy
        .moreSpecificKinds(RelativeCauseKind.StrategicConcession)(RelativeCauseKind.ConversionMiss)
    )
    assert(
      !RelativeCauseDominancePolicy
        .moreSpecificKinds(RelativeCauseKind.MissedTacticalResource)(RelativeCauseKind.DrawResource)
    )
    assert(
      !RelativeCauseDominancePolicy
        .moreSpecificKinds(RelativeCauseKind.MissedTacticalResource)(RelativeCauseKind.ConversionSecured)
    )
    assert(
      !RelativeCauseDominancePolicy
        .moreSpecificKinds(RelativeCauseKind.TacticalRefutationOfPlayed)(RelativeCauseKind.ConversionMiss)
    )
    assert(
      RelativeCauseDominancePolicy
        .moreSpecificKinds(RelativeCauseKind.TacticalRefutationOfPlayed)(RelativeCauseKind.TempoLoss)
    )
    assert(
      RelativeCauseDominancePolicy
        .moreSpecificKinds(RelativeCauseKind.TacticalRefutationOfPlayed)(RelativeCauseKind.KingForcing)
    )
    assertEquals(
      RelativeCauseDominancePolicy.moreSpecificKinds(RelativeCauseKind.PlanContradiction),
      Set.empty
    )

  test("material fallback edges follow the direct line-proof intersection"):
    def accepted(kind: RelativeCauseKind): Set[LineConsequenceKind] =
      LineConsequenceKind.values
        .filter(LineConsequenceKind.tacticalDriver)
        .filter(RelativeCauseKind.acceptsLineConsequence(kind, _))
        .toSet

    val genericTactical = accepted(RelativeCauseKind.MissedTacticalResource)
    val material = accepted(RelativeCauseKind.MaterialSwing)
    val wrongRecapturer = accepted(RelativeCauseKind.WrongRecapturer)
    val compensation = accepted(RelativeCauseKind.SacrificeCompensation)

    assert(genericTactical.intersect(material).nonEmpty)
    assertEquals(
      material.intersect(wrongRecapturer),
      Set(LineConsequenceKind.MaterialLoss)
    )
    assertEquals(
      material.intersect(compensation),
      Set(LineConsequenceKind.MaterialGain, LineConsequenceKind.MaterialLoss)
    )
    List(
      RelativeCauseKind.WrongMoveOrder,
      RelativeCauseKind.TempoLoss,
      RelativeCauseKind.ConversionMiss
    ).foreach(kind => assert(material.intersect(accepted(kind)).isEmpty))

  test("an exact tactical refutation replaces generic liability and material labels"):
    val proof = zwischenzugRecord("material-result-proof", best)
    val material = causeCase(
      "material-result-cause",
      RelativeCauseKind.MaterialSwing,
      CandidateComparisonKind.PlayedVsBest,
      best,
      -12.0,
      proof
    )
    val liability = causeCase(
      "broad-liability-cause",
      RelativeCauseKind.CandidateTacticalLiability,
      CandidateComparisonKind.PlayedVsBest,
      best,
      -12.0,
      proof
    )
    val refutation = causeCase(
      "broad-refutation-cause",
      RelativeCauseKind.TacticalRefutationOfPlayed,
      CandidateComparisonKind.PlayedVsBest,
      best,
      -12.0,
      proof
    )
    val missed = causeCase(
      "missed-tactical-material-cause",
      RelativeCauseKind.MissedTacticalResource,
      CandidateComparisonKind.PlayedVsBest,
      best,
      -12.0,
      proof
    )
    val all = List(material, liability, refutation, missed)
    val channelFixture = exactChannelFixture(proof)
    val result = resolveDominanceOnChannel(all, channelFixture)

    assertEquals(result.cross.keySet, Set(refutation.causeRef.id))
    assertEquals(
      result.dominance(material.causeRef.id).dominatingCauseEvidenceIds,
      List(refutation.causeRef.id)
    )
    assertEquals(
      result.dominance(liability.causeRef.id).dominatingCauseEvidenceIds,
      List(refutation.causeRef.id)
    )
    assert(result.dominance(refutation.causeRef.id).retained)
    assertEquals(
      result.dominance(missed.causeRef.id).dominatingCauseEvidenceIds,
      List(refutation.causeRef.id)
    )

  test("an exact material-loss mechanism can replace the material-result fallback in only one direction"):
    val proof = zwischenzugRecord("material-mechanism-proof", best)
    val material = causeCase(
      "material-fallback-cause",
      RelativeCauseKind.MaterialSwing,
      CandidateComparisonKind.PlayedVsBest,
      best,
      -12.0,
      proof
    )
    val mechanism = causeCase(
      "material-mechanism-cause",
      RelativeCauseKind.WrongRecapturer,
      CandidateComparisonKind.PlayedVsBest,
      best,
      -12.0,
      proof
    )
    val result = resolveDominanceOnChannel(List(material, mechanism), exactChannelFixture(proof))

    assert(!result.cross.contains(material.causeRef.id))
    assert(result.cross(mechanism.causeRef.id).selected)
    assertEquals(
      result.dominance(material.causeRef.id).dominatingCauseEvidenceIds,
      List(mechanism.causeRef.id)
    )
    assert(result.dominance(mechanism.causeRef.id).retained)

  test("sacrifice compensation is the maximal refinement of one exact material channel"):
    val proof = zwischenzugRecord("sacrifice-material-channel-proof", best)
    val generic = causeCase(
      "sacrifice-generic-cause",
      RelativeCauseKind.MissedTacticalResource,
      CandidateComparisonKind.PlayedVsBest,
      best,
      -12.0,
      proof
    )
    val material = causeCase(
      "sacrifice-material-cause",
      RelativeCauseKind.MaterialSwing,
      CandidateComparisonKind.PlayedVsBest,
      best,
      -12.0,
      proof
    )
    val compensation = causeCase(
      "sacrifice-compensation-cause",
      RelativeCauseKind.SacrificeCompensation,
      CandidateComparisonKind.PlayedVsBest,
      best,
      -12.0,
      proof
    )
    val result = resolveDominanceOnChannel(
      List(generic, material, compensation),
      exactChannelFixture(proof)
    )

    assertEquals(result.cross.keySet, Set(compensation.causeRef.id))
    assertEquals(
      result.dominance(generic.causeRef.id).dominatingCauseEvidenceIds,
      List(compensation.causeRef.id)
    )
    assertEquals(
      result.dominance(material.causeRef.id).dominatingCauseEvidenceIds,
      List(compensation.causeRef.id)
    )
    assert(result.dominance(compensation.causeRef.id).retained)

  test("an exact king-forcing resource replaces the generic missed-tactical-resource label"):
    val proof = zwischenzugRecord("king-forcing-channel-proof", best)
    val generic = causeCase(
      "king-forcing-generic-cause",
      RelativeCauseKind.MissedTacticalResource,
      CandidateComparisonKind.PlayedVsBest,
      best,
      -12.0,
      proof
    )
    val forcing = causeCase(
      "king-forcing-specific-cause",
      RelativeCauseKind.KingForcing,
      CandidateComparisonKind.PlayedVsBest,
      best,
      -12.0,
      proof
    )
    val result = resolveDominanceOnChannel(List(generic, forcing), exactChannelFixture(proof))

    assert(!result.cross.contains(generic.causeRef.id))
    assert(result.cross(forcing.causeRef.id).selected)
    assertEquals(
      result.dominance(generic.causeRef.id).dominatingCauseEvidenceIds,
      List(forcing.causeRef.id)
    )
    assert(result.dominance(forcing.causeRef.id).retained)

  test("a concrete mechanism suppresses MTR through one owned role superset across comparisons"):
    val genericProof = zwischenzugRecord("role-superset-generic-proof", best)
    val specificProof = zwischenzugRecord("role-superset-specific-proof", best)
    val generic = causeCase(
      "role-superset-generic",
      RelativeCauseKind.MissedTacticalResource,
      CandidateComparisonKind.PlayedVsAlternative,
      best,
      -8.0,
      genericProof
    )
    val specific = causeCase(
      "role-superset-specific",
      RelativeCauseKind.KingForcing,
      CandidateComparisonKind.PlayedVsBest,
      best,
      -12.0,
      specificProof
    )
    val all = List(generic, specific)
    val graph = graphOf(all.flatMap(_.records))
    val genericChannel = exactChannelFixture(genericProof)
    val specificChannel = genericChannel
    val refinedChannel = specificChannel.copy(binding = specificChannel.binding.copy(
      mechanism = specificChannel.binding.mechanism :+
        ConcreteChessObject(EvidenceObjectKind.Mechanism, "forcing-check-sequence"),
      consequence = specificChannel.binding.consequence :+
        ConcreteChessObject(EvidenceObjectKind.Consequence, "king-displacement")
    ))
    val preliminary = List(
      CrossComparisonExposureDecision(
        generic.causeRef.id,
        CrossComparisonExposureStatus.SelectedComplementary,
        CrossComparisonExposureReason.PlayedVsBestAlternativeResource,
        generic.causeRef.id,
        Some(PlayerFacingCauseEffectMode.AlternativeResource),
        Some(1)
      ),
      CrossComparisonExposureDecision(
        specific.causeRef.id,
        CrossComparisonExposureStatus.SelectedPrimary,
        CrossComparisonExposureReason.PlayedVsBestAlternativeResource,
        specific.causeRef.id,
        Some(PlayerFacingCauseEffectMode.AlternativeResource),
        Some(0)
      )
    )
    val dominance = RelativeCauseDominancePolicy.resolve(
      all.map(item => item.cause -> item.causeRef),
      preliminary,
      Map(
        generic.causeRef.id -> List(genericChannel),
        specific.causeRef.id -> List(refinedChannel)
      ),
      graph
    )
    val byId = dominance.decisions.map(item => item.causeEvidenceId -> item).toMap

    assertNotEquals(genericChannel.causalSignature, refinedChannel.causalSignature)
    assertEquals(byId(generic.causeRef.id).dominatingCauseEvidenceIds, List(specific.causeRef.id))
    assert(byId(specific.causeRef.id).retained)

  test("different plan-axis identities cannot suppress the same surface fallback"):
    val proof = zwischenzugRecord("cross-plan-proof", best)
    val generic = causeCase(
      "cross-plan-generic",
      RelativeCauseKind.MissedTacticalResource,
      CandidateComparisonKind.PlayedVsBest,
      best,
      -12.0,
      proof
    )
    val specific = causeCase(
      "cross-plan-specific",
      RelativeCauseKind.KingForcing,
      CandidateComparisonKind.PlayedVsBest,
      best,
      -12.0,
      proof
    )
    val all = List(generic, specific)
    val graph = graphOf(all.flatMap(_.records))
    val base = exactChannelFixture(proof)
    def planChannel(label: String): DirectCauseChannel =
      base.copy(rootOwnedProof = base.rootOwnedProof.map(proof =>
        RootOwnedEffectProof.StrategicAxis(
          proof,
          StrategicAxisDetail(
            StrategicAxisKind.PlanCoherence,
            StrategicAxisPolarity.Gain,
            label
          ),
          None
        )
      ))
    val fallbackChannel = planChannel("plan-a")
    val candidateChannel = planChannel("plan-b")
    val preliminary = all.zipWithIndex.map { case (item, rank) =>
      CrossComparisonExposureDecision(
        item.causeRef.id,
        CrossComparisonExposureStatus.SelectedPrimary,
        CrossComparisonExposureReason.PlayedVsBestAlternativeResource,
        item.causeRef.id,
        Some(PlayerFacingCauseEffectMode.AlternativeResource),
        Some(rank)
      )
    }
    val dominance = RelativeCauseDominancePolicy.resolve(
      all.map(item => item.cause -> item.causeRef),
      preliminary,
      Map(
        generic.causeRef.id -> List(fallbackChannel),
        specific.causeRef.id -> List(candidateChannel)
      ),
      graph
    )

    assertNotEquals(
      fallbackChannel.rootOwnedEffectDescriptor,
      candidateChannel.rootOwnedEffectDescriptor
    )
    assert(dominance.decisions.forall(_.retained))

  test("a plan label cannot refine a generic concession through a non-plan surface channel"):
    val proof = zwischenzugRecord("non-plan-surface-proof", best)
    val fallback = causeCase(
      "strategic-concession-cause",
      RelativeCauseKind.StrategicConcession,
      CandidateComparisonKind.PlayedVsBest,
      best,
      -12.0,
      proof
    )
    val plan = causeCase(
      "plan-contradiction-cause",
      RelativeCauseKind.PlanContradiction,
      CandidateComparisonKind.PlayedVsBest,
      best,
      -12.0,
      proof
    )
    val graph = graphOf(List(
      causeCase(
        "channel-fixture",
        RelativeCauseKind.MissedTacticalResource,
        CandidateComparisonKind.PlayedVsBest,
        best,
        -12.0,
        proof
      )
    ).flatMap(_.records))
    val channelFixture = graph.records.collectFirst {
      case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) => cause
    }.toList.flatMap(cause => RelativeCauseConstructionAdmission.admittedDirectChannels(cause, graph)).head
    val preliminary = List(fallback, plan).zipWithIndex.map { case (item, rank) =>
      CrossComparisonExposureDecision(
        causeEvidenceId = item.causeRef.id,
        status = CrossComparisonExposureStatus.SelectedPrimary,
        reason = CrossComparisonExposureReason.PlayedVsBestAlternativeResource,
        representativeCauseEvidenceId = item.causeRef.id,
        effectMode = Some(PlayerFacingCauseEffectMode.AlternativeResource),
        comparisonExposureRank = Some(rank)
      )
    }
    val dominance = RelativeCauseDominancePolicy.resolve(
      List(fallback.cause -> fallback.causeRef, plan.cause -> plan.causeRef),
      preliminary,
      Map(
        fallback.causeRef.id -> List(channelFixture),
        plan.causeRef.id -> List(channelFixture)
      ),
      graph
    )
    val byId = dominance.decisions.map(item => item.causeEvidenceId -> item).toMap

    assert(!RelativeCauseDominancePolicy.ownsPlanCausalProof(channelFixture))
    assert(byId(fallback.causeRef.id).retained)
    assert(byId(plan.causeRef.id).retained)

  private final case class CauseCase(
      cause: RelativeCauseFact,
      causeRef: EvidenceRef,
      records: List[EvidenceRecord]
  )

  private final case class Resolved(
      cross: Map[String, CrossComparisonExposureDecision],
      dominance: Map[String, RelativeCauseDominanceDecision]
  )

  private def causeCase(
      id: String,
      causeKind: RelativeCauseKind,
      comparisonKind: CandidateComparisonKind,
      reference: LineNodeRef,
      delta: Double,
      proof: EvidenceRecord
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
      EvalComparison(
        White,
        delta,
        if delta <= -10.0 then MoveChoiceVerdict.Blunder else MoveChoiceVerdict.Inaccuracy
      )
    )
    val unresolvedCause = RelativeCauseFact(
      kind = causeKind,
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
    val supportingRecords = List(
      EvidenceRecord(comparisonRef, CandidateComparisonEvidence(comparison)),
      proof
    )
    val supportingGraph = graphOf(supportingRecords)
    val admittedSignatures = EvidenceObjectBinding
      .rawDirectSentenceChannelsForProjection(unresolvedCause, supportingGraph)
      .map(_.causalSignature)
      .toSet
    val cause = unresolvedCause.copy(
      directEffectAdmission = DirectEffectAdmission.Restricted(admittedSignatures)
    )
    val causeRef = ref(
      id,
      EvidenceProducer.RelativeMoveProducer,
      EvidenceLayer.RelativeCause,
      Some(reference)
    )
    CauseCase(
      cause,
      causeRef,
      supportingRecords ++ List(
        EvidenceRecord(causeRef, RelativeCauseFactEvidence(cause))
      )
    )

  private def zwischenzugRecord(id: String, eventLine: LineNodeRef): EvidenceRecord =
    zwischenzugRecord(id, eventLine, expectedRecaptureSquare = "d5")

  private def zwischenzugRecord(
      id: String,
      eventLine: LineNodeRef,
      expectedRecaptureSquare: String
  ): EvidenceRecord =
    val detail = RelationWitnessDetail.Zwischenzug(
      intermediateMove = eventLine.rootMove,
      expectedRecaptureSquare = EvidenceSquare(expectedRecaptureSquare),
      checkingPieceSquare = EvidenceSquare(eventLine.rootMove.slice(2, 4)),
      checkingPieceRole = EvidencePieceRole(Queen.name),
      checkedKingSquare = EvidenceSquare("e8"),
      threatType = RelationThreatSignal.Check
    )
    val evidence = ref(
      id,
      EvidenceProducer.TacticalRelationProducer,
      EvidenceLayer.Relation,
      Some(eventLine)
    )
    EvidenceRecord(
      evidence,
      RelationFactEvidence
        .from(detail, List(eventLine.rootMove))
        .getOrElse(fail("expected a typed zwischenzug relation"))
    )

  private def withAdditionalProof(base: CauseCase, additional: EvidenceRecord): CauseCase =
    val proof = base.cause.proof.getOrElse(fail("expected direct Cause proof"))
    val unresolvedCause = base.cause.copy(
      supportEvidence = (base.cause.supportEvidence :+ additional.ref).distinctBy(_.id),
      proof = Some(proof.copy(
        directProof = proof.directProof.copy(
          sourceRefs = (proof.directProof.sourceRefs :+ additional.ref).distinctBy(_.id)
        )
      )),
      directEffectAdmission = DirectEffectAdmission.Unresolved
    )
    val supportingRecords =
      (base.records.filterNot(_.ref.id == base.causeRef.id) :+ additional).distinctBy(_.ref.id)
    val supportingGraph = graphOf(supportingRecords)
    val cause = unresolvedCause.copy(
      directEffectAdmission = DirectEffectAdmission.Restricted(
        EvidenceObjectBinding
          .rawDirectSentenceChannelsForProjection(unresolvedCause, supportingGraph)
          .map(_.causalSignature)
          .toSet
      )
    )
    CauseCase(
      cause,
      base.causeRef,
      supportingRecords ++ List(
        EvidenceRecord(base.causeRef, RelativeCauseFactEvidence(cause))
      )
    )

  private def channels(causeCase: CauseCase, graph: TypedEvidenceGraph): List[DirectCauseChannel] =
    RelativeCauseConstructionAdmission.admittedDirectChannels(causeCase.cause, graph)

  private def hostClaim(item: CauseCase): JudgmentClaim =
    val line = item.causeRef.line.getOrElse(fail("expected Cause event line"))
    JudgmentClaim(
      id = s"${item.causeRef.id}-host",
      family = ClaimFamily.Tactical,
      subject = ClaimSubject.ReferenceMove,
      primaryPosition = position,
      primaryLine = Some(line),
      subjectMove = Some(line.rootMove),
      evidence = List(item.causeRef),
      scope = line.role.scope,
      confidence = EvidenceConfidence.EngineBacked
    )

  private def resolve(
      causes: List[CauseCase],
      graph: TypedEvidenceGraph
  ): Resolved =
    val causePairs = causes.map(item => item.cause -> item.causeRef)
    val directChannels = causes.map(item =>
      item.causeRef.id -> RelativeCauseConstructionAdmission.admittedDirectChannels(item.cause, graph)
    ).toMap
    val eligibility = CrossComparisonCauseExposurePolicy
      .resolveEligibilityForDominance(causePairs, graph, Set(played.rootMove))
    val dominance = RelativeCauseDominancePolicy.resolve(causePairs, eligibility, directChannels, graph)
    val retainedIds = dominance.decisions.filter(_.retained).map(_.causeEvidenceId).toSet
    val retainedPairs = causePairs.filter { case (_, ref) => retainedIds(ref.id) }
    val representatives = CrossComparisonCauseExposurePolicy.resolve(
      retainedPairs,
      graph,
      Set(played.rootMove),
      dominance.fallbackObligationsByDominatorId
    )
    Resolved(
      cross = CrossComparisonCauseExposurePolicy
        .resolveRetainedPvbResponsibility(
          representatives,
          retainedPairs,
          graph,
          Set(played.rootMove)
        )
        .decisions
        .map(decision => decision.causeEvidenceId -> decision)
        .toMap,
      dominance = dominance.decisions.map(decision => decision.causeEvidenceId -> decision).toMap
    )

  private def resolve(causes: List[CauseCase]): Resolved =
    resolve(causes, graphOf(causes.flatMap(_.records)))

  private def exactChannelFixture(proof: EvidenceRecord): DirectCauseChannel =
    val fixture = causeCase(
      s"${proof.ref.id}-channel-fixture",
      RelativeCauseKind.MissedTacticalResource,
      CandidateComparisonKind.PlayedVsBest,
      best,
      -12.0,
      proof
    )
    val graph = graphOf(fixture.records)
    channels(fixture, graph).head

  private def resolveDominanceOnChannel(
      causes: List[CauseCase],
      channel: DirectCauseChannel
  ): Resolved =
    val graph = graphOf(causes.flatMap(_.records))
    val preliminary = causes.zipWithIndex.map { case (item, rank) =>
      CrossComparisonExposureDecision(
        causeEvidenceId = item.causeRef.id,
        status = CrossComparisonExposureStatus.SelectedPrimary,
        reason = CrossComparisonExposureReason.PlayedVsBestAlternativeResource,
        representativeCauseEvidenceId = item.causeRef.id,
        effectMode = Some(PlayerFacingCauseEffectMode.AlternativeResource),
        comparisonExposureRank = Some(rank)
      )
    }
    val dominance = RelativeCauseDominancePolicy.resolve(
      causes.map(item => item.cause -> item.causeRef),
      preliminary,
      causes.map(item => item.causeRef.id -> List(channel)).toMap,
      graph
    )
    val causePairs = causes.map(item => item.cause -> item.causeRef)
    val retainedIds = dominance.decisions.filter(_.retained).map(_.causeEvidenceId).toSet
    val retainedPairs = causePairs.filter { case (_, ref) => retainedIds(ref.id) }
    Resolved(
      cross = CrossComparisonCauseExposurePolicy
        .resolveRetainedPvbResponsibility(
          preliminary.filter(decision => retainedIds(decision.causeEvidenceId)),
          retainedPairs,
          graph,
          Set(played.rootMove)
        )
        .decisions
        .map(decision => decision.causeEvidenceId -> decision)
        .toMap,
      dominance = dominance.decisions.map(decision => decision.causeEvidenceId -> decision).toMap
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

  private def graphOf(records: List[EvidenceRecord]): TypedEvidenceGraph =
    records
      .distinctBy(_.ref.id)
      .foldLeft(TypedEvidenceGraph.empty)((graph, record) => graph.add(record))
