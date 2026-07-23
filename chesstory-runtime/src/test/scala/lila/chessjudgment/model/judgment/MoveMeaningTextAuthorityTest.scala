package lila.chessjudgment.model.judgment

class MoveMeaningTextAuthorityTest extends munit.FunSuite:

  test("pawn and king-pressure meaning gates require typed consequences"):
    val textOnly = PositionPlanTechniqueSemanticDetail(
      unit = PositionPlanTechniqueUnit.StructuralTransformation,
      axisKey = Some("PawnBreak:Gain:break-file-g-created-tension-resolved-tension"),
      label = Some("pawn-break king-safety-pressure resolved-tension"),
      structuralRouteMove = Some("g2g4"),
      objectBindingSignatures = List("actor=Move:g2g4|target=Piece:king")
    )

    assert(!MoveMeaningClaim.pawnBreakTimingDetail(textOnly))
    assert(!MoveMeaningClaim.pawnBreakConcreteTransitionCarrier(textOnly))
    assert(!MoveMeaningClaim.flankKingPressurePawnAdvanceDetail(textOnly))
    assert(!MoveMeaningClaim.pawnBreakResolutionDetail(textOnly))

    val typedGain = textOnly.copy(
      axisKey = Some("opaque-axis"),
      label = Some("opaque-label"),
      breakFile = Some("g"),
      structuralConsequenceKinds = List(
        TransitionConsequenceKind.PawnTensionGain,
        TransitionConsequenceKind.KingSafetyPressure
      )
    )
    assert(MoveMeaningClaim.pawnBreakTimingDetail(typedGain))
    assert(MoveMeaningClaim.pawnBreakConcreteTransitionCarrier(typedGain))
    assert(MoveMeaningClaim.flankKingPressurePawnAdvanceDetail(typedGain))
    assert(!MoveMeaningClaim.pawnBreakResolutionDetail(typedGain))

    val typedResolution = typedGain.copy(
      breakFile = None,
      structuralConsequenceKinds = List(TransitionConsequenceKind.PawnTensionResolution)
    )
    assert(MoveMeaningClaim.pawnBreakResolutionDetail(typedResolution))

  test("counterplay race authority ignores descriptive text"):
    val textOnly = PositionPlanTechniqueSemanticDetail(
      unit = PositionPlanTechniqueUnit.CounterplayRace,
      axisKey = Some("Counterplay:Gain:dynamic-counterplay-race"),
      label = Some("dynamic-counterplay-race"),
      resourceContestKinds = List(BoardAnchorKind.CounterplayRestraint.toString),
      resourceContestSquares = List("d5")
    )
    assert(!MoveMeaningClaim.counterplayRaceSemanticProof(textOnly))

    val typedDynamic = textOnly.copy(
      axisKey = Some("opaque-axis"),
      label = Some("opaque-label"),
      threatKind = Some("Positional"),
      turnsToImpact = Some(2)
    )
    assert(MoveMeaningClaim.counterplayRaceSemanticProof(typedDynamic))

    val typedKingConcession = PositionPlanTechniqueSemanticDetail(
      unit = PositionPlanTechniqueUnit.CounterplayRace,
      label = Some("opaque-label"),
      raceLeadingLineRole = Some(LineNodeRole.Played),
      structuralConsequenceKinds = List(TransitionConsequenceKind.KingSafetyConcession)
    )
    assert(!MoveMeaningClaim.counterplayRaceSemanticProof(typedKingConcession))
    assert(MoveMeaningClaim.counterplayRaceSemanticProof(
      typedKingConcession.copy(
        label = Some("king-safety-concession"),
        structuralConsequenceKinds = Nil
      )
    ))

  test("restriction and negative-plan admission ignore axis and label wording"):
    def claim(
        meaningKind: String,
        causeKinds: List[RelativeCauseKind] = Nil,
        axisPolarity: Option[StrategicAxisPolarity] = Some(StrategicAxisPolarity.Restrain),
        label: Option[String] = Some("opponent-mobility-restriction")
    ): MoveMeaningClaim =
      MoveMeaningClaim(
        meaningKind = meaningKind,
        role = if meaningKind == "PlanContinuity" then "ExecutesCurrentPlan" else "PreventsCounterplay",
        laneKey = "typed-authority-test",
        conflictKey = None,
        supportLevel = "owned_cause_linked",
        visibility = "reason_grade",
        surfaceLane = "current_move_owned",
        lineRole = "candidate",
        moveUci = "g2g4",
        frameId = "typed-authority-test",
        unit = PositionPlanTechniqueUnit.SpacePreventionResourceDenial,
        axisKey = Some("Counterplay:Restrain:opponent-mobility-restriction"),
        axisKind = Some(StrategicAxisKind.Counterplay),
        axisPolarity = axisPolarity,
        label = label,
        causeKinds = causeKinds,
        causeSourceSides = Nil,
        causeEvidenceIds = Nil,
        sourceEvidenceIds = List("opaque-source"),
        objectBindingSignatures = List("actor=Move:g2g4|target=Square:g5"),
        objectCarrierReady = true,
        boardCarriers = List(
          MoveMeaningSurfaceBoardCarrier("actor", "Move", "g2g4"),
          MoveMeaningSurfaceBoardCarrier("target", "Square", "g5")
        ),
        targetSquares = List("g5")
      )

    val textOnlyRestriction = claim("CounterplayControl")
    assert(!MoveMeaningClaim.opponentRestrictionHasBoardCarrier(textOnlyRestriction))
    assert(MoveMeaningClaim.opponentRestrictionHasBoardCarrier(
      textOnlyRestriction.copy(
        axisKey = Some("opaque-axis"),
        label = Some("opaque-label"),
        causeKinds = List(RelativeCauseKind.OpponentRestriction)
      )
    ))

    val textOnlyNegativePlan = claim(
      meaningKind = "PlanContinuity",
      axisPolarity = None,
      label = Some("mobility-loss")
    )
    assert(!MoveMeaningClaim.unprovedNegativePlanContinuity(textOnlyNegativePlan))
    assert(MoveMeaningClaim.unprovedNegativePlanContinuity(
      textOnlyNegativePlan.copy(label = Some("opaque-label"), axisPolarity = Some(StrategicAxisPolarity.Concede))
    ))
    assert(MoveMeaningClaim.unprovedNegativePlanContinuity(
      textOnlyNegativePlan.copy(label = Some("opaque-label"), causeKinds = List(RelativeCauseKind.ActivityLoss))
    ))
