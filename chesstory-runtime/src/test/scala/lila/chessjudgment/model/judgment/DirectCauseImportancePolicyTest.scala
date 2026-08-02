package lila.chessjudgment.model.judgment

import chess.{ Black, White }
import lila.chessjudgment.model.line.PrincipalVariationEvidence

class DirectCauseImportancePolicyTest extends munit.FunSuite:

  private val rootBoardState = "4k3/8/8/8/8/8/4Q3/4K3 w - -"
  private val reference = LineNodeRef("reference", "e2e4", 1, LineNodeRole.BestReference)
  private val played = LineNodeRef("played", "e2e3", 9, LineNodeRole.Played)
  private val comparison = CandidateComparisonFact(
    CandidateComparisonKind.PlayedVsBest,
    reference,
    played,
    EvalComparison(White, -12.0, MoveChoiceVerdict.Blunder)
  )
  private val baseFrame = DirectCauseImportanceFrame(
    comparison = CandidateComparisonSemanticKey.from(comparison),
    eventLine = SemanticLineKey.from(reference),
    sourceSide = RelativeCauseSourceSide.Reference,
    attribution = CauseAttributionKind.ReferenceCreatesResource,
    exposure = PlayerFacingCauseExposureTier.Complementary,
    effectMode = PlayerFacingCauseEffectMode.AlternativeResource,
    actor = White,
    directChange = DirectCausalChange.Occurred,
    playedChange = PlayerFacingCausalChange.Missed,
    stake = DirectCauseEffectStake.Benefits(White)
  )
  private val baseUniverse = DirectCauseImportanceUniverse.from(
    s"$rootBoardState 0 1",
    baseFrame.effectMode,
    baseFrame.actor,
    baseFrame.directChange,
    baseFrame.playedChange,
    baseFrame.stake
  ).getOrElse(throw IllegalStateException("expected a base importance universe"))

  test("reviewed-move impact lets a missed mate dominate a played strategic liability"):
    val mateUniverse = baseUniverse
    val harmFrame = baseFrame.copy(
      eventLine = SemanticLineKey.from(played),
      sourceSide = RelativeCauseSourceSide.Candidate,
      attribution = CauseAttributionKind.CandidateAllowsLiability,
      exposure = PlayerFacingCauseExposureTier.Primary,
      effectMode = PlayerFacingCauseEffectMode.PlayedLiability,
      directChange = DirectCausalChange.Lost,
      playedChange = PlayerFacingCausalChange.Lost,
      stake = DirectCauseEffectStake.Benefits(Black)
    )
    val harmUniverse = DirectCauseImportanceUniverse.from(
      s"$rootBoardState 99 42",
      harmFrame.effectMode,
      harmFrame.actor,
      harmFrame.directChange,
      harmFrame.playedChange,
      harmFrame.stake
    ).getOrElse(fail("expected a semantic root board"))
    val mate = DirectCauseImportanceProfile(
      causeEvidenceId = "missed-mate",
      causalSignature = "missed-mate-channel",
      frame = baseFrame,
      universe = mateUniverse,
      domain = DirectCauseImportanceDomain.BoardMate,
      measure = DirectCauseImportanceMeasure.MateArrival(3)
    )
    val strategicHarm = DirectCauseImportanceProfile(
      causeEvidenceId = "strategic-harm",
      causalSignature = "strategic-harm-channel",
      frame = harmFrame,
      universe = harmUniverse,
      domain = DirectCauseImportanceDomain.Structural(
        DirectCauseStructuralOrigin.RootTransition,
        TransitionConsequenceKind.MobilityLoss,
        StructuralSignalPolarity.Loss,
        None
      ),
      measure = DirectCauseImportanceMeasure.StructuralStrength(5),
      effectIdentity = RootOwnedEffectIdentity(
        RootOwnedEffectPrimitiveKind.StructuralTransition,
        List("square:e4"),
        Nil,
        List(RootOwnedStrategicAxisIdentity(
          StrategicAxisKind.Activity,
          StrategicAxisPolarity.Loss,
          "mover-activity",
          None
        ))
      )
    )
    val laterMate = mate.copy(
      causeEvidenceId = "later-missed-mate",
      causalSignature = "later-missed-mate-channel",
      measure = DirectCauseImportanceMeasure.MateArrival(5)
    )
    val beneficialMateFrame = baseFrame.copy(
      effectMode = PlayerFacingCauseEffectMode.PlayedValue,
      playedChange = PlayerFacingCausalChange.Occurred
    )
    val beneficialMate = mate.copy(
      causeEvidenceId = "beneficial-mate",
      causalSignature = "beneficial-mate-channel",
      frame = beneficialMateFrame,
      universe = DirectCauseImportanceUniverse.from(
        s"$rootBoardState 0 1",
        beneficialMateFrame.effectMode,
        beneficialMateFrame.actor,
        beneficialMateFrame.directChange,
        beneficialMateFrame.playedChange,
        beneficialMateFrame.stake
      ).getOrElse(fail("expected a beneficial mate impact"))
    )
    val wrappedMate = mate.copy(
      causeEvidenceId = "wrapped-mate",
      causalSignature = "wrapped-mate-channel",
      effectIdentity = RootOwnedEffectIdentity(
        RootOwnedEffectPrimitiveKind.LineEpisode,
        Nil,
        Nil,
        List(RootOwnedStrategicAxisIdentity(
          StrategicAxisKind.PlanCoherence,
          StrategicAxisPolarity.Gain,
          "mate-plan",
          None
        ))
      )
    )
    val otherBoardHarm = strategicHarm.copy(
      causeEvidenceId = "other-board-harm",
      causalSignature = "other-board-harm-channel",
      universe = harmUniverse.copy(
        rootBoardState = "4k3/8/8/8/8/8/8/4KQ2 w - -"
      )
    )

    assertEquals(mateUniverse, harmUniverse)
    assertEquals(
      DirectCauseImportancePolicy.compare(mate, strategicHarm),
      DirectCauseImportanceRelation.Dominates
    )
    assertEquals(
      DirectCauseImportancePolicy.compare(strategicHarm, mate),
      DirectCauseImportanceRelation.DominatedBy
    )
    assertEquals(
      DirectCauseImportancePolicy.compare(mate, laterMate),
      DirectCauseImportanceRelation.Dominates
    )
    assertEquals(
      DirectCauseImportancePolicy.compare(mate, beneficialMate),
      DirectCauseImportanceRelation.Incomparable
    )
    assertEquals(
      DirectCauseImportancePolicy.compare(mate, wrappedMate),
      DirectCauseImportanceRelation.Tied
    )
    assertEquals(
      DirectCauseImportancePolicy.compare(mate, otherBoardHarm),
      DirectCauseImportanceRelation.Incomparable
    )
    val resolution = DirectCauseImportancePolicy.resolveProfiles(
      List(
        "strategic-harm" -> List("strategic-harm-channel"),
        "missed-mate" -> List("missed-mate-channel")
      ),
      List(strategicHarm, mate)
    )
    assertEquals(resolution.frontierCauseEvidenceIds, List("missed-mate"))
    assertEquals(resolution.uniqueTopCauseEvidenceId, Some("missed-mate"))

  test("reviewed-move impact compares missed material with played liability but separates good value"):
    val missedQueen = material("missed-queen", "missed-queen-channel", 900, 1)
    val liabilityFrame = baseFrame.copy(
      eventLine = SemanticLineKey.from(played),
      sourceSide = RelativeCauseSourceSide.Candidate,
      attribution = CauseAttributionKind.CandidateAllowsLiability,
      exposure = PlayerFacingCauseExposureTier.Primary,
      effectMode = PlayerFacingCauseEffectMode.PlayedLiability,
      directChange = DirectCausalChange.Lost,
      playedChange = PlayerFacingCausalChange.Lost,
      stake = DirectCauseEffectStake.Harms(White)
    )
    val liabilityUniverse = DirectCauseImportanceUniverse.from(
      s"$rootBoardState 0 1",
      liabilityFrame.effectMode,
      liabilityFrame.actor,
      liabilityFrame.directChange,
      liabilityFrame.playedChange,
      liabilityFrame.stake
    ).getOrElse(fail("expected a played liability impact"))
    val allowedPawnLoss = DirectCauseImportanceProfile(
      causeEvidenceId = "allowed-pawn-loss",
      causalSignature = "allowed-pawn-loss-channel",
      frame = liabilityFrame,
      universe = liabilityUniverse,
      domain = DirectCauseImportanceDomain.Material,
      measure = DirectCauseImportanceMeasure.MaterialOutcome(100, 3),
      effectIdentity = RootOwnedEffectIdentity(
        RootOwnedEffectPrimitiveKind.LineEpisode,
        List("square:a7"),
        Nil,
        List(RootOwnedStrategicAxisIdentity(
          StrategicAxisKind.PlanCoherence,
          StrategicAxisPolarity.Concede,
          "conversion-plan",
          None
        ))
      )
    )
    val playedValueFrame = baseFrame.copy(
      eventLine = SemanticLineKey.from(played),
      sourceSide = RelativeCauseSourceSide.Candidate,
      attribution = CauseAttributionKind.CandidateCreatesValue,
      exposure = PlayerFacingCauseExposureTier.Primary,
      effectMode = PlayerFacingCauseEffectMode.PlayedValue,
      playedChange = PlayerFacingCausalChange.Occurred
    )
    val playedValueUniverse = DirectCauseImportanceUniverse.from(
      s"$rootBoardState 0 1",
      playedValueFrame.effectMode,
      playedValueFrame.actor,
      playedValueFrame.directChange,
      playedValueFrame.playedChange,
      playedValueFrame.stake
    ).getOrElse(fail("expected a played value impact"))
    val goodQueen = DirectCauseImportanceProfile(
      causeEvidenceId = "good-queen",
      causalSignature = "good-queen-channel",
      frame = playedValueFrame,
      universe = playedValueUniverse,
      domain = DirectCauseImportanceDomain.Material,
      measure = DirectCauseImportanceMeasure.MaterialOutcome(900, 1)
    )

    assertEquals(baseUniverse, liabilityUniverse)
    assertNotEquals(baseUniverse, playedValueUniverse)
    assertEquals(
      DirectCauseImportancePolicy.compare(missedQueen, allowedPawnLoss),
      DirectCauseImportanceRelation.Dominates
    )
    assertEquals(
      DirectCauseImportancePolicy.compare(missedQueen, goodQueen),
      DirectCauseImportanceRelation.Incomparable
    )
    assertEquals(
      PlayerFacingImpact.from(
        PlayerFacingCauseEffectMode.AlternativeResource,
        White,
        DirectCausalChange.Occurred,
        PlayerFacingCausalChange.Missed,
        DirectCauseEffectStake.Benefits(Black)
      ),
      None
    )

    assertEquals(
      DirectCauseImportanceUniverse.from(
        s"$rootBoardState 0 1",
        PlayerFacingCauseEffectMode.AlternativeResource,
        Black,
        DirectCausalChange.Occurred,
        PlayerFacingCausalChange.Missed,
        DirectCauseEffectStake.Benefits(Black)
      ),
      None
    )
    assertEquals(
      PlayerFacingImpact.from(
        PlayerFacingCauseEffectMode.PlayedLiability,
        White,
        DirectCausalChange.Missed,
        PlayerFacingCausalChange.Missed,
        DirectCauseEffectStake.Benefits(White)
      ),
      Some(PlayerFacingImpact.HarmsReviewedMover(White))
    )
    assertEquals(
      PlayerFacingImpact.from(
        PlayerFacingCauseEffectMode.PlayedLiability,
        White,
        DirectCausalChange.Occurred,
        PlayerFacingCausalChange.Occurred,
        DirectCauseEffectStake.Benefits(White)
      ),
      None
    )

  test("stronger effects cross comparison provenance and reorder only the public selection order"):
    val alternative = LineNodeRef("alternative", "e2f3", 2, LineNodeRole.Alternative)
    val playedVsAlternative = CandidateComparisonFact(
      CandidateComparisonKind.PlayedVsAlternative,
      alternative,
      played,
      EvalComparison(White, -8.0, MoveChoiceVerdict.Inaccuracy)
    )
    val weaker = material("weaker", "weaker-channel", 100, 3)
    val stronger = material("stronger", "stronger-channel", 900, 1).copy(
      frame = baseFrame.copy(
        comparison = CandidateComparisonSemanticKey.from(playedVsAlternative),
        eventLine = SemanticLineKey.from(alternative)
      )
    )

    assert(weaker.frame != stronger.frame)
    assertEquals(weaker.universe, stronger.universe)
    assertEquals(
      DirectCauseImportancePolicy.compare(stronger, weaker),
      DirectCauseImportanceRelation.Dominates
    )

    val importance = DirectCauseImportancePolicy.resolveProfiles(
      List(
        "weaker" -> List("weaker-channel"),
        "stronger" -> List("stronger-channel")
      ),
      List(weaker, stronger)
    )
    val original = List(
      playerFacingSelection("weaker", "weaker-channel", comparisonRank = 0, selectionOrder = 0),
      playerFacingSelection("stronger", "stronger-channel", comparisonRank = 2, selectionOrder = 1)
    )
    val ordered = orderSingletonIdeas(original, importance)

    assertEquals(ordered.selections.map(_.causeEvidence.id), List("stronger", "weaker"))
    assertEquals(ordered.selections.map(_.selectionOrder), List(0, 1))
    assertEquals(
      ordered.selections.map(selection => selection.causeEvidence.id -> selection.comparisonExposureRank).toMap,
      Map("weaker" -> 0, "stronger" -> 2)
    )
    assertEquals(
      ordered.selections.map(selection =>
        selection.causeEvidence.id -> selection.copy(selectionOrder = Int.MaxValue)
      ).toMap,
      original.map(selection =>
        selection.causeEvidence.id -> selection.copy(selectionOrder = Int.MaxValue)
      ).toMap
    )
    assertEquals(
      ordered.ideaImportanceResolution.selectedCauseEvidenceIds,
      ordered.selections.map(_.causeEvidence.id)
    )
    assertEquals(ordered.ideaImportanceResolution.uniqueTopCauseEvidenceId, Some("stronger"))

  test("an exact PVB responsibility pair is one importance unit, not two competing Causes"):
    val resourceProfile = material("resource", "resource-channel", 900, 1)
    val liabilityProfile = material("liability", "liability-channel", 900, 1).copy(
      frame = baseFrame.copy(
        eventLine = SemanticLineKey.from(played),
        sourceSide = RelativeCauseSourceSide.Candidate,
        attribution = CauseAttributionKind.CandidateAllowsLiability,
        exposure = PlayerFacingCauseExposureTier.Primary,
        effectMode = PlayerFacingCauseEffectMode.PlayedLiability,
        playedChange = PlayerFacingCausalChange.Occurred,
        stake = DirectCauseEffectStake.Benefits(Black)
      )
    )
    val weakerProfile = material("weaker-single", "weaker-single-channel", 100, 3)
    val liabilitySelection = playerFacingSelection(
      "liability",
      "liability-channel",
      comparisonRank = 0,
      selectionOrder = 0
    ).copy(
      exposure = PlayerFacingCauseExposureTier.Primary,
      effectMode = PlayerFacingCauseEffectMode.PlayedLiability,
      channels = playerFacingSelection(
        "liability",
        "liability-channel",
        comparisonRank = 0,
        selectionOrder = 0
      ).channels.map(channel =>
        channel.copy(playedChange = PlayerFacingCausalChange.Occurred)
      )
    )
    val resourceSelection = playerFacingSelection(
      "resource",
      "resource-channel",
      comparisonRank = 1,
      selectionOrder = 1
    )
    val weakerSelection = playerFacingSelection(
      "weaker-single",
      "weaker-single-channel",
      comparisonRank = 2,
      selectionOrder = 2
    )
    val selections = List(liabilitySelection, resourceSelection, weakerSelection)
    val causeImportance = DirectCauseImportancePolicy.resolveProfiles(
      selections.map(selection =>
        selection.causeEvidence.id -> selection.channels.map(_.causalSignature)
      ),
      List(liabilityProfile, resourceProfile, weakerProfile)
    )
    assertEquals(causeImportance.uniqueTopCauseEvidenceId, None)

    val units = PlayerFacingIdeaUnitPolicy.resolve(
      selections,
      List(ExactPvbResponsibilityPair("liability", "resource"))
    )
    val ideaImportance = DirectCauseImportancePolicy.resolveProfiles(
      List(
        "liability" -> List("liability-channel"),
        "weaker-single" -> List("weaker-single-channel")
      ),
      List(liabilityProfile, weakerProfile)
    )
    val ordered = PlayerFacingIdeaOrderingPolicy.order(
      selections,
      units,
      causeImportance,
      ideaImportance
    )

    assertEquals(ordered.ideaImportanceResolution.uniqueTopCauseEvidenceId, Some("liability"))
    assertEquals(ordered.ideaUnits.map(_.id), List("liability", "weaker-single"))
    assertEquals(
      ordered.ideaUnits.head.memberCauseEvidenceIds,
      List("liability", "resource")
    )
    assertEquals(
      ordered.ideaUnits.head.priorityStatus,
      PlayerFacingIdeaPriorityStatus.UniqueTop
    )
    assertEquals(
      ordered.selections.map(_.causeEvidence.id),
      List("liability", "resource", "weaker-single")
    )

  test("proven dominance chains form layers before comparison rank is consulted"):
    val top = material("top", "top-channel", 900, 1)
    val middle = material("middle", "middle-channel", 500, 2)
    val bottom = material("bottom", "bottom-channel", 100, 3)
    val importance = DirectCauseImportancePolicy.resolveProfiles(
      List(
        "bottom" -> List("bottom-channel"),
        "top" -> List("top-channel"),
        "middle" -> List("middle-channel")
      ),
      List(bottom, top, middle)
    )
    val original = List(
      playerFacingSelection("bottom", "bottom-channel", comparisonRank = 0, selectionOrder = 0),
      playerFacingSelection("top", "top-channel", comparisonRank = 5, selectionOrder = 1),
      playerFacingSelection("middle", "middle-channel", comparisonRank = 9, selectionOrder = 2)
    )
    val ordered = orderSingletonIdeas(original, importance)

    assertEquals(ordered.selections.map(_.causeEvidence.id), List("top", "middle", "bottom"))
    assertEquals(
      importance.decisions.find(_.causeEvidenceId == "middle").map(_.dominatingCauseEvidenceIds.toSet),
      Some(Set("top"))
    )
    assertEquals(
      importance.decisions.find(_.causeEvidenceId == "bottom").map(_.dominatingCauseEvidenceIds.toSet),
      Some(Set("top", "middle"))
    )

  test("a fully measured Cause with one undominated channel stays in the comparison layer"):
    val stronger = material("stronger", "stronger-channel", 900, 1)
    val partialMaterial = material("partial", "partial-material-channel", 100, 3)
    val partialStructural = structural(
      "partial",
      "partial-structural-channel",
      2,
      RootOwnedEffectIdentity(
        RootOwnedEffectPrimitiveKind.StructuralTransition,
        List("square:e5"),
        Nil,
        Nil
      )
    )
    val importance = DirectCauseImportancePolicy.resolveProfiles(
      List(
        "partial" -> List("partial-material-channel", "partial-structural-channel"),
        "stronger" -> List("stronger-channel")
      ),
      List(stronger, partialMaterial, partialStructural)
    )
    val partialDecision = importance.decisions.find(_.causeEvidenceId == "partial")
      .getOrElse(fail("expected a partial Cause decision"))
    val ordered = orderSingletonIdeas(
      List(
        playerFacingSelection(
          "partial",
          "partial-material-channel",
          comparisonRank = 0,
          selectionOrder = 0,
          additionalSignatures = List("partial-structural-channel")
        ),
        playerFacingSelection("stronger", "stronger-channel", comparisonRank = 5, selectionOrder = 1)
      ),
      importance
    )

    assert(partialDecision.fullyMeasured)
    assertEquals(partialDecision.dominatingCauseEvidenceIds, Nil)
    assert(!partialDecision.dominatedWithinDomain)
    assertEquals(ordered.selections.map(_.causeEvidence.id), List("partial", "stronger"))

  test("sibling Causes cannot union disjoint profile dominance to demote a multi-channel Cause"):
    val compoundMaterial = material("compound", "compound-material", 100, 3)
    val compoundThreat = threatProfile("compound", "compound-threat", turnsToImpact = 3)
    val materialOnly = material("material-only", "material-only-channel", 500, 1)
    val threatOnly = threatProfile("threat-only", "threat-only-channel", turnsToImpact = 1)
    val resolution = DirectCauseImportancePolicy.resolveProfiles(
      List(
        "compound" -> List("compound-material", "compound-threat"),
        "material-only" -> List("material-only-channel"),
        "threat-only" -> List("threat-only-channel")
      ),
      List(compoundMaterial, compoundThreat, materialOnly, threatOnly)
    )
    val compound = resolution.decisions.find(_.causeEvidenceId == "compound")
      .getOrElse(fail("expected a compound Cause decision"))

    assert(compound.fullyMeasured)
    assertEquals(compound.dominatingCauseEvidenceIds, Nil)
    assert(!compound.dominatedWithinDomain)
    assertEquals(
      resolution.frontierCauseEvidenceIds.toSet,
      Set("compound", "material-only", "threat-only")
    )
    assertEquals(resolution.uniqueTopCauseEvidenceId, None)

  test("one Cause that dominates every profile demotes a weaker multi-channel Cause"):
    val weakerMaterial = material("weaker", "weaker-material", 100, 3)
    val weakerThreat = threatProfile("weaker", "weaker-threat", turnsToImpact = 3)
    val strongerMaterial = material("stronger", "stronger-material", 500, 1)
    val strongerThreat = threatProfile("stronger", "stronger-threat", turnsToImpact = 1)
    val resolution = DirectCauseImportancePolicy.resolveProfiles(
      List(
        "weaker" -> List("weaker-material", "weaker-threat"),
        "stronger" -> List("stronger-material", "stronger-threat")
      ),
      List(weakerMaterial, weakerThreat, strongerMaterial, strongerThreat)
    )
    val weaker = resolution.decisions.find(_.causeEvidenceId == "weaker")
      .getOrElse(fail("expected a weaker Cause decision"))

    assert(weaker.fullyMeasured)
    assertEquals(weaker.dominatingCauseEvidenceIds, List("stronger"))
    assert(weaker.dominatedWithinDomain)
    assertEquals(resolution.frontierCauseEvidenceIds, List("stronger"))
    assertEquals(resolution.uniqueTopCauseEvidenceId, Some("stronger"))

  test("cyclic proven dominance falls back to stable comparison authority order"):
    val first = playerFacingSelection("first", "first-channel", comparisonRank = 1, selectionOrder = 0)
    val second = playerFacingSelection("second", "second-channel", comparisonRank = 0, selectionOrder = 1)
    val cyclic = DirectCauseImportanceResolution(
      selectedCauseEvidenceIds = List("first", "second"),
      profiles = Nil,
      relations = Nil,
      decisions = List(
        DirectCauseImportanceDecision(
          "first",
          List("first-channel"),
          Nil,
          List("second"),
          dominatedWithinDomain = true
        ),
        DirectCauseImportanceDecision(
          "second",
          List("second-channel"),
          Nil,
          List("first"),
          dominatedWithinDomain = true
        )
      )
    )

    val original = List(first, second)
    val ordered = orderSingletonIdeas(original, cyclic)

    assertEquals(ordered.selections.map(_.causeEvidence.id), List("second", "first"))
    assertEquals(ordered.selections.map(_.causeEvidence.id).toSet, original.map(_.causeEvidence.id).toSet)
    assertEquals(
      ordered.selections.map(selection =>
        selection.causeEvidence.id -> selection.copy(selectionOrder = Int.MaxValue)
      ).toMap,
      original.map(selection =>
        selection.causeEvidence.id -> selection.copy(selectionOrder = Int.MaxValue)
      ).toMap
    )
    assertEquals(
      ordered.ideaImportanceResolution.selectedCauseEvidenceIds,
      ordered.selections.map(_.causeEvidence.id)
    )

    val invalidEdge = cyclic.copy(
      decisions = List(
        cyclic.decisions.head.copy(dominatingCauseEvidenceIds = List("missing-cause")),
        cyclic.decisions(1).copy(
          dominatingCauseEvidenceIds = Nil,
          dominatedWithinDomain = false
        )
      )
    )
    val invalidOrdered = orderSingletonIdeas(original, invalidEdge)
    assertEquals(invalidOrdered.selections.map(_.causeEvidence.id), List("second", "first"))

  test("incomparable and unmeasured Causes preserve comparison authority order"):
    val materialProfile = material("z-material", "material-channel", 500, 1)
    val structuralProfile = structural(
      "a-structure",
      "structure-channel",
      3,
      RootOwnedEffectIdentity(
        RootOwnedEffectPrimitiveKind.StructuralTransition,
        List("square:e5"),
        Nil,
        Nil
      )
    )
    val importance = DirectCauseImportancePolicy.resolveProfiles(
      List(
        "unmeasured" -> List("unmeasured-channel"),
        "a-structure" -> List("structure-channel"),
        "z-material" -> List("material-channel")
      ),
      List(materialProfile, structuralProfile)
    )
    val original = List(
      playerFacingSelection("unmeasured", "unmeasured-channel", comparisonRank = 2, selectionOrder = 2),
      playerFacingSelection("a-structure", "structure-channel", comparisonRank = 0, selectionOrder = 1),
      playerFacingSelection("z-material", "material-channel", comparisonRank = 0, selectionOrder = 0)
    )
    val ordered = orderSingletonIdeas(original, importance)

    assertEquals(
      ordered.selections.map(_.causeEvidence.id),
      List("a-structure", "z-material", "unmeasured")
    )
    assertEquals(ordered.ideaImportanceResolution.dominatedWithinDomainCauseEvidenceIds, Nil)
    assertEquals(ordered.ideaImportanceResolution.uniqueTopCauseEvidenceId, None)

  test("equal measures tie without an evidence-id importance tiebreak"):
    val left = material("z-cause", "left-channel", 500, 2)
    val right = material("a-cause", "right-channel", 500, 2)

    assertEquals(
      DirectCauseImportancePolicy.compare(left, right),
      DirectCauseImportanceRelation.Tied
    )
    assertEquals(
      DirectCauseImportancePolicy.compare(right, left),
      DirectCauseImportanceRelation.Tied
    )

  test("importance comparison preserves Pareto and typed scope boundaries"):
    val scope = RootOwnedEffectIdentity(
      RootOwnedEffectPrimitiveKind.StructuralTransition,
      List("Square:e5"),
      Nil,
      Nil
    )
    val stronger = structural("stronger", "stronger-channel", 4, scope)
    val strategic = stronger.copy(
      causeEvidenceId = "strategic",
      causalSignature = "strategic-channel",
      measure = DirectCauseImportanceMeasure.StrategicStrength(4)
    )
    val mate = DirectCauseImportanceProfile(
      causeEvidenceId = "mate",
      causalSignature = "mate-channel",
      frame = baseFrame,
      universe = baseUniverse,
      domain = DirectCauseImportanceDomain.BoardMate,
      measure = DirectCauseImportanceMeasure.MateArrival(1)
    )
    val rows = List(
      (
        "material Pareto trade-off",
        material("later-queen", "later-queen-channel", 900, 4),
        material("earlier-rook", "earlier-rook-channel", 500, 1),
        DirectCauseImportanceRelation.Incomparable
      ),
      (
        "same structural scope",
        stronger,
        structural("weaker", "weaker-channel", 2, scope),
        DirectCauseImportanceRelation.Dominates
      ),
      (
        "different structural target",
        stronger,
        structural(
          "other",
          "other-channel",
          1,
          scope.copy(targetSignatures = List("Square:c5"))
        ),
        DirectCauseImportanceRelation.Incomparable
      ),
      (
        "unscoped structural target",
        stronger,
        structural("unscoped", "unscoped-channel", 1, RootOwnedEffectIdentity.unscoped),
        DirectCauseImportanceRelation.Incomparable
      ),
      (
        "inventory-only strategic",
        strategic,
        strategic.copy(
          causeEvidenceId = "other-strategic",
          causalSignature = "other-strategic-channel",
          measure = DirectCauseImportanceMeasure.StrategicStrength(2)
        ),
        DirectCauseImportanceRelation.Incomparable
      ),
      ("inventory-only strategic versus mate", strategic, mate, DirectCauseImportanceRelation.Incomparable)
    )

    rows.foreach { case (label, left, right, expected) =>
      assertEquals(DirectCauseImportancePolicy.compare(left, right), expected, label)
    }

  test("measured effects retain typed root-proof semantics"):
    val position = PositionNodeRef(s"$rootBoardState 0 1", 0, Some(White))
    val source = ref("measured-source", EvidenceLayer.Line, position, Some(reference))
    val binding = EvidenceObjectBinding(
      source,
      target = List(ConcreteChessObject(EvidenceObjectKind.Square, "h2")),
      line = Some(reference)
    )
    val threat = ThreatEpisodeEvidence(ThreatEpisode.fromThreat(
      Threat(White, ThreatKind.Material, 2, Nil, List("h2"), List("rook"), None, 2),
      0
    ))
    val threatChannel = DirectCauseChannel(
      binding,
      DirectCausalChange.Occurred,
      rootOwnedProof = Some(RootOwnedEffectProof.ThreatCreation(source, threat))
    )
    val consequence = TransitionConsequence(
      TransitionConsequenceKind.TargetPressureGain,
      StructuralSignalPolarity.Gain,
      4,
      targetSubjects = List("h2")
    )
    val delta = StructuralDeltaEvidence(
      StructuralTransitionBinding(
        reference.rootMove,
        TransitionEdgeRole.Reference,
        position,
        position.copy(ply = 1, sideToMove = Some(Black)),
        Some(reference),
        White
      ),
      Nil,
      List(consequence)
    )
    val structuralChannel = threatChannel.copy(
      rootOwnedProof = Some(RootOwnedEffectProof.StructuralTransition(source, delta, consequence))
    )
    val threatEffect = DirectCauseMeasuredEffect
      .fromChannel(threatChannel)
      .getOrElse(fail("expected threat effect"))
    val structuralEffect = DirectCauseMeasuredEffect
      .fromChannel(structuralChannel)
      .getOrElse(fail("expected structural effect"))
    val wrappedChannel = structuralChannel.copy(rootOwnedProof = Some(
      RootOwnedEffectProof.StrategicAxis(
        structuralChannel.rootOwnedProof.getOrElse(fail("expected structural proof")),
        StrategicAxisDetail(StrategicAxisKind.Activity, StrategicAxisPolarity.Gain, "activity"),
        Some(StrategicAxisComparisonOutcome.ReferenceOnly)
      )
    ))
    val wrappedEffect = DirectCauseMeasuredEffect
      .fromChannel(wrappedChannel)
      .getOrElse(fail("expected wrapped effect"))

    assertEquals(
      (threatEffect.stake, threatEffect.domain, threatEffect.measure),
      (
        DirectCauseEffectStake.Benefits(White),
        DirectCauseImportanceDomain.Threat(ThreatKind.Material, "Square:h2"),
        DirectCauseImportanceMeasure.ThreatHorizon(2)
      )
    )
    assertEquals(
      (structuralEffect.stake, structuralEffect.domain, structuralEffect.measure),
      (
        DirectCauseEffectStake.Benefits(White),
        DirectCauseImportanceDomain.Structural(
          DirectCauseStructuralOrigin.RootTransition,
          TransitionConsequenceKind.TargetPressureGain,
          StructuralSignalPolarity.Gain,
          None
        ),
        DirectCauseImportanceMeasure.StructuralStrength(4)
      )
    )
    List(threatChannel, structuralChannel, wrappedChannel).foreach { channel =>
      assertEquals(
        DirectCauseMeasuredEffect.fromChannel(channel).map(_.effectIdentity),
        channel.rootOwnedEffectDescriptor.map(_.identity)
      )
    }
    assertEquals(
      (wrappedEffect.stake, wrappedEffect.domain, wrappedEffect.measure),
      (structuralEffect.stake, structuralEffect.domain, structuralEffect.measure)
    )
    assertNotEquals(wrappedEffect.effectIdentity, structuralEffect.effectIdentity)
    assertEquals(
      DirectCauseMeasuredEffect.fromChannel(threatChannel.copy(importanceDescriptorAmbiguous = true)),
      None
    )
    assertEquals(DirectCauseMeasuredEffect.fromChannel(threatChannel.copy(rootOwnedProof = None)), None)

  test("same-signature profile conflicts remain unmeasured in either input order"):
    val weak = material("conflict", "conflict-channel", 100, 2)
    val strong = weak.copy(measure = DirectCauseImportanceMeasure.MaterialOutcome(500, 2))

    List(List(weak, strong), List(strong, weak)).foreach { profiles =>
      val resolution = DirectCauseImportancePolicy.resolveProfiles(
        List("conflict" -> List("conflict-channel")),
        profiles
      )
      val decision = resolution.decisions.head
      assertEquals(resolution.profiles, Nil)
      assertEquals(decision.measuredChannelSignatures, Nil)
      assertEquals(decision.unmeasuredChannelSignatures, List("conflict-channel"))
      assertEquals(resolution.uniqueTopCauseEvidenceId, None)
    }

  test("descriptor conflict remains ambiguous after wrapper shadowing"):
    val position = PositionNodeRef(s"$rootBoardState 0 1", 0, Some(White))
    val source = ref("descriptor-source", EvidenceLayer.Line, position, Some(reference))
    val binding = EvidenceObjectBinding(
      source,
      target = List(ConcreteChessObject(EvidenceObjectKind.Square, "h2")),
      line = Some(reference)
    )
    val weak = DirectCauseChannel(
      binding,
      DirectCausalChange.Occurred,
      rootOwnedProof = Some(RootOwnedEffectProof.ThreatCreation(
        source,
        ThreatEpisodeEvidence(ThreatEpisode.fromThreat(
          Threat(White, ThreatKind.Material, 2, Nil, List("h2"), List("rook"), None, 2),
          0
        ))
      ))
    )
    val strong = weak.copy(rootOwnedProof = Some(RootOwnedEffectProof.ThreatCreation(
      source,
      ThreatEpisodeEvidence(ThreatEpisode.fromThreat(
        Threat(White, ThreatKind.Material, 1, Nil, List("h2"), List("rook"), None, 2),
        1
      ))
    )))
    val wrapped = weak.copy(
      binding = binding.copy(
        source = source.copy(id = "descriptor-wrapper"),
        mechanism = List(ConcreteChessObject(EvidenceObjectKind.Mechanism, "activity")),
        provenance = List(source)
      ),
      primitiveCausalSignature = Some(weak.causalSignature),
      rootOwnedProof = Some(RootOwnedEffectProof.StrategicAxis(
        weak.rootOwnedProof.getOrElse(fail("expected primitive proof")),
        StrategicAxisDetail(StrategicAxisKind.Activity, StrategicAxisPolarity.Gain, "activity"),
        Some(StrategicAxisComparisonOutcome.ReferenceOnly)
      ))
    )

    val canonical = EvidenceObjectBinding.canonicalCauseChannels(List(weak, strong, wrapped))
    assertEquals(canonical.size, 1)
    assert(canonical.head.binding.provenance.nonEmpty)
    assert(canonical.head.importanceDescriptorAmbiguous)

  test("frontier resolution requires every channel before declaring a Cause dominated"):
    val stronger = material("stronger", "stronger-channel", 900, 1)
    val weaker = material("weaker", "weaker-channel", 100, 2)
    val resolved = DirectCauseImportancePolicy.resolveProfiles(
      List(
        "stronger" -> List("stronger-channel"),
        "weaker" -> List("weaker-channel")
      ),
      List(stronger, weaker)
    )

    assertEquals(resolved.frontierCauseEvidenceIds, List("stronger"))
    assertEquals(resolved.dominatedWithinDomainCauseEvidenceIds, List("weaker"))
    assertEquals(resolved.uniqueTopCauseEvidenceId, Some("stronger"))
    assertEquals(
      resolved.copy(decisions = resolved.decisions.take(1)).uniqueTopCauseEvidenceId,
      None
    )

    val withUnmeasuredChannel = DirectCauseImportancePolicy.resolveProfiles(
      List(
        "stronger" -> List("stronger-channel"),
        "weaker" -> List("weaker-channel", "unmeasured-channel")
      ),
      List(stronger, weaker)
    )

    assertEquals(withUnmeasuredChannel.frontierCauseEvidenceIds, List("stronger", "weaker"))
    assertEquals(withUnmeasuredChannel.dominatedWithinDomainCauseEvidenceIds, Nil)
    assertEquals(withUnmeasuredChannel.uniqueTopCauseEvidenceId, None)
    assertEquals(
      withUnmeasuredChannel.decisions.find(_.causeEvidenceId == "weaker").map(_.unmeasuredChannelSignatures),
      Some(List("unmeasured-channel"))
    )

  test("nonterminal cross-family profiles remain a plural frontier"):
    val materialProfile = material("material", "material-channel", 900, 1)
    val structuralProfile = structural(
      "structure",
      "structure-channel",
      4,
      RootOwnedEffectIdentity(
        RootOwnedEffectPrimitiveKind.StructuralTransition,
        List("square:e5"),
        Nil,
        Nil
      )
    )
    val resolution = DirectCauseImportancePolicy.resolveProfiles(
      List(
        "material" -> List("material-channel"),
        "structure" -> List("structure-channel")
      ),
      List(materialProfile, structuralProfile)
    )

    assertEquals(resolution.frontierCauseEvidenceIds, List("material", "structure"))
    assertEquals(resolution.uniqueTopCauseEvidenceId, None)
    assertEquals(resolution.incomparableProfilePairCount, 1)

  test("an unregistered root proof cannot create an importance profile"):
    val position = PositionNodeRef(
      "4k3/8/8/8/8/8/4Q3/4K3 w - - 0 1",
      0,
      Some(White)
    )
    val comparisonRef = ref("comparison", EvidenceLayer.CandidateComparison, position, Some(played))
    val causeRef = ref("cause", EvidenceLayer.RelativeCause, position, Some(reference))
    val source = ref("line", EvidenceLayer.Line, position, Some(reference))
    val actor = RootCausalActor(
      reference.rootMove,
      EvidencePieceRole("queen"),
      White,
      EvidenceSquare("e2"),
      EvidenceSquare("e4")
    )
    val consequence = LineConsequence(
      LineConsequenceKind.Mate,
      List(reference.rootMove),
      proofSignal = true,
      eventMove = Some(reference.rootMove),
      rootMove = Some(reference.rootMove),
      rootSide = Some(White),
      beneficiary = Some(White)
    )
    val episode = RootOwnedCausalEpisode(
      reference,
      actor,
      EvidenceSquare("e8"),
      List(RootCausalLink(
        RootCausalLinkKind.ImmediateRootAction,
        reference.rootMove,
        reference.rootMove,
        EvidenceSquare("e8")
      )),
      consequence,
      eventPlyOffset = 0,
      chainMoves = List(reference.rootMove)
    )
    val line = LineFactEvidence(reference)
    val binding = EvidenceObjectBinding(
      source = source,
      actor = List(
        ConcreteChessObject(EvidenceObjectKind.Move, reference.rootMove),
        ConcreteChessObject(EvidenceObjectKind.Side, "white"),
        ConcreteChessObject(EvidenceObjectKind.Piece, "queen"),
        ConcreteChessObject(EvidenceObjectKind.Square, "e2"),
        ConcreteChessObject(EvidenceObjectKind.Square, "e4")
      ),
      target = List(ConcreteChessObject(EvidenceObjectKind.Square, "e8")),
      mechanism = List(ConcreteChessObject(EvidenceObjectKind.Mechanism, "mate")),
      consequence = List(ConcreteChessObject(EvidenceObjectKind.Consequence, "mate")),
      line = Some(reference),
      proofRole = Some(RelativeCauseProofRole.DirectProof)
    )
    val channel = DirectCauseChannel(
      binding,
      DirectCausalChange.Occurred,
      rootOwnedProof = Some(RootOwnedEffectProof.LineEpisode(source, line, episode))
    )
    val selectedChannel = PlayerFacingCauseChannelSelection(
      source,
      channel.causalSignature,
      DirectCausalChange.Occurred,
      PlayerFacingCausalChange.Missed
    )
    val selection = PlayerFacingCauseSelection(
      causeRef,
      PlayerFacingCauseExposureTier.Complementary,
      PlayerFacingCauseEffectMode.AlternativeResource,
      List(selectedChannel)
    )
    val cause = RelativeCauseFact(
      RelativeCauseKind.KingForcing,
      comparisonRef,
      List(source),
      RelativeCauseSourceSide.Reference,
      CauseAttribution(
        CauseAttributionKind.ReferenceCreatesResource,
        rootMoveMatched = true,
        directProofEligible = true
      )
    )

    assertEquals(
      DirectCauseImportancePolicy.profile(
        cause,
        selection,
        selectedChannel,
        channel,
        TypedEvidenceGraph.empty
      ),
      None
    )

  test("a registered exact material episode keeps durable outcome and captured-target salience separate"):
    val fen = "4k3/8/8/8/8/8/4Q2r/4K3 w - - 0 1"
    val eventLine = LineNodeRef("material-reference", "e2h2", 1, LineNodeRole.BestReference)
    val candidateLine = LineNodeRef("material-played", "e2e3", 9, LineNodeRole.Played)
    val eventPosition = PositionNodeRef(fen, 0, Some(White))
    val comparisonRef = EvidenceRef(
      "material-comparison",
      EvidenceProducer.RelativeMoveProducer,
      EvidenceLayer.CandidateComparison,
      eventPosition,
      Some(candidateLine),
      candidateLine.role.scope,
      EvidenceConfidence.EngineBacked
    )
    val sourceRef = EvidenceRef(
      "material-line",
      EvidenceProducer.LegalLineProducer,
      EvidenceLayer.Line,
      eventPosition,
      Some(eventLine),
      eventLine.role.scope,
      EvidenceConfidence.LegalReplayVerified
    )
    val causeRef = EvidenceRef(
      "material-cause",
      EvidenceProducer.RelativeMoveProducer,
      EvidenceLayer.RelativeCause,
      eventPosition,
      Some(eventLine),
      eventLine.role.scope,
      EvidenceConfidence.EngineBacked
    )
    val comparison = CandidateComparisonFact(
      CandidateComparisonKind.PlayedVsBest,
      eventLine,
      candidateLine,
      EvalComparison(White, -12.0, MoveChoiceVerdict.Blunder)
    )
    val consequence = LineConsequence(
      LineConsequenceKind.MaterialGain,
      List(eventLine.rootMove),
      proofSignal = true,
      eventMove = Some(eventLine.rootMove),
      rootMove = Some(eventLine.rootMove),
      rootSide = Some(White),
      beneficiary = Some(White),
      materialOutcome = Some(RootOwnedMaterialOutcome(
        event = RootOwnedMaterialEventSalience(
          moveUci = eventLine.rootMove,
          plyOffset = 0,
          capturedRole = EvidencePieceRole("rook"),
          square = EvidenceSquare("h2"),
          targetValueCp = 500
        ),
        beneficiary = White,
        durableNetCp = 500
      ))
    )
    val capture = LineMaterialCapture(
      eventLine.rootMove,
      plyOffset = 0,
      side = White,
      EvidencePieceRole("queen"),
      EvidencePieceRole("rook"),
      EvidenceSquare("h2"),
      valueCp = 500,
      recapture = false
    )
    val fenAfter = PrincipalVariationEvidence
      .legalFenAfter(fen, eventLine.rootMove)
      .getOrElse(fail("expected a legal material test move"))
    val line = LineFactEvidence(
      line = eventLine,
      material = Some(LineMaterialSummary(
        sideToMove = White,
        captures = List(capture),
        netCaptureCpForMover = 500,
        maxGainCpForMover = 500,
        maxLossCpForMover = 0,
        hasRecaptureChain = false,
        hasRecoveryWindow = false,
        promotionGainCpForMover = 0,
        materialWindowComplete = true
      )),
      replay = List(LineReplayStep(0, eventLine.rootMove, fen, fenAfter)),
      consequences = List(consequence)
    )
    val cause = RelativeCauseFact(
      RelativeCauseKind.MaterialSwing,
      comparisonRef,
      List(sourceRef),
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
          List(sourceRef)
        )
      ))
    )
    val baseRecords = List(
      EvidenceRecord(comparisonRef, CandidateComparisonEvidence(comparison)),
      EvidenceRecord(sourceRef, line)
    )
    val provisionalGraph = (baseRecords :+
      EvidenceRecord(causeRef, RelativeCauseFactEvidence(cause))
    ).foldLeft(TypedEvidenceGraph.empty)((current, record) => current.add(record))
    val admittedSignatures = EvidenceObjectBinding
      .rawDirectSentenceChannelsForProjection(cause, provisionalGraph)
      .map(_.causalSignature)
      .toSet
    val admittedCause = cause.copy(
      directEffectAdmission = DirectEffectAdmission.Restricted(admittedSignatures)
    )
    val graph = (baseRecords :+
      EvidenceRecord(causeRef, RelativeCauseFactEvidence(admittedCause))
    ).foldLeft(TypedEvidenceGraph.empty)((current, record) => current.add(record))
    val channels = RelativeCauseConstructionAdmission.admittedDirectChannels(admittedCause, graph)
    val selection = PlayerFacingCauseSelectionPolicy
      .build(
        causeRef,
        CrossComparisonExposureStatus.SelectedComplementary,
        PlayerFacingCauseEffectMode.AlternativeResource,
        channels,
        onlyMoveQualifier = None
      )
      .getOrElse(fail("expected an exact selected material Cause"))
    val resolution = DirectCauseImportancePolicy.resolve(
      List(admittedCause -> causeRef),
      List(selection),
      Map(causeRef.id -> channels),
      graph
    )

    assertEquals(channels.size, 1)
    assertEquals(
      resolution.profiles.map(_.measure),
      List(DirectCauseImportanceMeasure.MaterialOutcome(500, 0))
    )
    assertEquals(
      channels.flatMap(_.rootOwnedEffectDescriptor.flatMap(_.materialEventSalience)),
      List(RootOwnedMaterialEventSalience(
        moveUci = eventLine.rootMove,
        plyOffset = 0,
        capturedRole = EvidencePieceRole("rook"),
        square = EvidenceSquare("h2"),
        targetValueCp = 500
      ))
    )
    assertEquals(
      resolution.profiles.map(_.domain),
      List(DirectCauseImportanceDomain.Material)
    )
    assertEquals(
      resolution.profiles.map(_.frame.stake),
      List(DirectCauseEffectStake.Benefits(White))
    )
    assertEquals(resolution.uniqueTopCauseEvidenceId, Some(causeRef.id))

  private def material(
      causeId: String,
      signature: String,
      valueCp: Int,
      plyOffset: Int
  ): DirectCauseImportanceProfile =
    DirectCauseImportanceProfile(
      causeEvidenceId = causeId,
      causalSignature = signature,
      frame = baseFrame,
      universe = baseUniverse,
      domain = DirectCauseImportanceDomain.Material,
      measure = DirectCauseImportanceMeasure.MaterialOutcome(valueCp, plyOffset)
    )

  private def structural(
      causeId: String,
      signature: String,
      strength: Int,
      identity: RootOwnedEffectIdentity
  ): DirectCauseImportanceProfile =
    DirectCauseImportanceProfile(
      causeEvidenceId = causeId,
      causalSignature = signature,
      frame = baseFrame,
      universe = baseUniverse,
      domain = DirectCauseImportanceDomain.Structural(
        DirectCauseStructuralOrigin.RootTransition,
        TransitionConsequenceKind.TargetPressureGain,
        StructuralSignalPolarity.Gain,
        None
      ),
      measure = DirectCauseImportanceMeasure.StructuralStrength(strength),
      effectIdentity = identity
    )

  private def threatProfile(
      causeId: String,
      signature: String,
      turnsToImpact: Int
  ): DirectCauseImportanceProfile =
    DirectCauseImportanceProfile(
      causeEvidenceId = causeId,
      causalSignature = signature,
      frame = baseFrame,
      universe = baseUniverse,
      domain = DirectCauseImportanceDomain.Threat(ThreatKind.Material, "piece:rook|square:h2"),
      measure = DirectCauseImportanceMeasure.ThreatHorizon(turnsToImpact)
    )

  private def playerFacingSelection(
      causeId: String,
      signature: String,
      comparisonRank: Int,
      selectionOrder: Int,
      additionalSignatures: List[String] = Nil
  ): PlayerFacingCauseSelection =
    val position = PositionNodeRef(s"$rootBoardState 0 1", 0, Some(White))
    def channel(channelSignature: String) = PlayerFacingCauseChannelSelection(
      carrierEvidence = ref(
        s"$causeId-$channelSignature-carrier",
        EvidenceLayer.Line,
        position,
        Some(reference)
      ),
      causalSignature = channelSignature,
      directChange = DirectCausalChange.Occurred,
      playedChange = PlayerFacingCausalChange.Missed
    )
    PlayerFacingCauseSelection(
      causeEvidence = ref(causeId, EvidenceLayer.RelativeCause, position, Some(reference)),
      exposure = PlayerFacingCauseExposureTier.Complementary,
      effectMode = PlayerFacingCauseEffectMode.AlternativeResource,
      channels = (signature :: additionalSignatures).map(channel),
      comparisonExposureRank = comparisonRank,
      selectionOrder = selectionOrder
    )

  private def orderSingletonIdeas(
      selections: List[PlayerFacingCauseSelection],
      importance: DirectCauseImportanceResolution
  ): PlayerFacingIdeaOrderingResult =
    PlayerFacingIdeaOrderingPolicy.order(
      selections = selections,
      ideaUnits = PlayerFacingIdeaUnitPolicy.resolve(selections, Nil),
      causeImportanceResolution = importance,
      ideaImportanceResolution = importance
    )

  private def ref(
      id: String,
      layer: EvidenceLayer,
      position: PositionNodeRef,
      line: Option[LineNodeRef]
  ): EvidenceRef =
    EvidenceRef(
      id,
      EvidenceProducer.RelativeMoveProducer,
      layer,
      position,
      line,
      line.map(_.role.scope).getOrElse(EvidenceScope.Counterfactual),
      EvidenceConfidence.EngineBacked
    )
