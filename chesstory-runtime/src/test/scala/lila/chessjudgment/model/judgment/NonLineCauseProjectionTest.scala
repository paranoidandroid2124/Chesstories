package lila.chessjudgment.model.judgment

import chess.{ Bishop, Black, Knight, Pawn, Queen, White }
import lila.chessjudgment.model.{ Motif, PlanEventIdentity }
import lila.chessjudgment.model.line.PrincipalVariationEvidence
import lila.chessjudgment.model.strategic.EngineLine
import lila.chessjudgment.model.strategic.PlanTaxonomy.PlanKind

class NonLineCauseProjectionTest extends munit.FunSuite:

  private val queenFen = "4k3/8/8/8/8/8/8/3QK3 w - - 0 1"
  private val queenPosition = PositionNodeRef(queenFen, 0, Some(White))
  private val queenLine = LineNodeRef("queen-reference", "d1h5", 1, LineNodeRole.BestReference)

  test("relation projection requires exact root move and destination-bound geometry"):
    val exact = relationRecord(
      "exact-relation",
      queenLine,
      RelationWitnessDetail.HangingPiece(
        EvidenceSquare("h5"),
        EvidenceSquare("e8"),
        EvidencePieceRole(Queen.name),
        EvidencePieceRole("king")
      ),
      List(queenLine.rootMove)
    )
    val laterOnly = relationRecord(
      "later-relation",
      queenLine,
      RelationWitnessDetail.HangingPiece(
        EvidenceSquare("h5"),
        EvidenceSquare("e8"),
        EvidencePieceRole(Queen.name),
        EvidencePieceRole("king")
      ),
      List("e8e7")
    )
    val departureCoincidence = relationRecord(
      "departure-only-relation",
      queenLine,
      RelationWitnessDetail.HangingPiece(
        EvidenceSquare("d1"),
        EvidenceSquare("e8"),
        EvidencePieceRole(Queen.name),
        EvidencePieceRole("king")
      ),
      List(queenLine.rootMove)
    )
    val unrelatedGeometry = relationRecord(
      "unrelated-relation",
      queenLine,
      RelationWitnessDetail.HangingPiece(
        EvidenceSquare("a1"),
        EvidenceSquare("e8"),
        EvidencePieceRole(Queen.name),
        EvidencePieceRole("king")
      ),
      List(queenLine.rootMove)
    )

    val exactBindings = projection(RelativeCauseKind.MissedTacticalResource, exact)
    assertEquals(exactBindings.count(_.specificTargetMechanismReady), 1)
    val binding = exactBindings.head
    assertRootActor(binding, queenLine.rootMove, "white", "queen", "d1", "h5")
    assert(objectKeys(binding.target, EvidenceObjectKind.Square)("e8"))
    assertEquals(projectionChannels(RelativeCauseKind.MissedTacticalResource, exact).map(_.directChange), List(DirectCausalChange.Occurred))
    assertEquals(projection(RelativeCauseKind.MissedTacticalResource, laterOnly), Nil)
    assertEquals(projection(RelativeCauseKind.MissedTacticalResource, departureCoincidence), Nil)
    assertEquals(projection(RelativeCauseKind.MissedTacticalResource, unrelatedGeometry), Nil)

  test("central effect admission binds every primitive source to the comparison root board"):
    val exact = relationRecord(
      "root-board-relation",
      queenLine,
      RelationWitnessDetail.HangingPiece(
        EvidenceSquare("h5"),
        EvidenceSquare("e8"),
        EvidencePieceRole(Queen.name),
        EvidencePieceRole("king")
      ),
      List(queenLine.rootMove)
    )
    val context = projectionContext(RelativeCauseKind.MissedTacticalResource, exact)
    val valid = EvidenceObjectBinding
      .directCauseChannelsForProjection(context.cause, context.graph)
      .headOption
      .getOrElse(fail("expected valid root relation"))
    val otherPosition = PositionNodeRef(
      "4k3/p7/8/8/8/8/8/3QK3 w - - 0 1",
      0,
      Some(White)
    )
    val otherRef = exact.ref.copy(id = "other-root-board-relation", position = otherPosition)
    val relation = exact.payload match
      case payload: RelationFactEvidence => payload
      case _                             => fail("expected relation")
    val otherGraph = context.graph.add(EvidenceRecord(otherRef, relation))
    val forged = valid.copy(
      binding = valid.binding.copy(source = otherRef, provenance = Nil),
      rootOwnedProof = Some(RootOwnedEffectProof.RootRelation(otherRef, relation))
    )
    assert(!RootOwnedEffectPolicy.admits(context.cause, otherGraph, forged))

    val sameOccurrencePosition = queenPosition.copy(
      fen = "4k3/8/8/8/8/8/8/3QK3 w - - 17 42"
    )
    val sameOccurrenceRef = exact.ref.copy(
      id = "same-root-occurrence-relation",
      position = sameOccurrencePosition
    )
    val sameOccurrenceGraph = context.graph.add(EvidenceRecord(sameOccurrenceRef, relation))
    val sameOccurrence = valid.copy(
      binding = valid.binding.copy(source = sameOccurrenceRef, provenance = Nil),
      rootOwnedProof = Some(RootOwnedEffectProof.RootRelation(sameOccurrenceRef, relation))
    )
    assert(RootOwnedEffectPolicy.admits(context.cause, sameOccurrenceGraph, sameOccurrence))

    val repeatedPosition = queenPosition.copy(ply = queenPosition.ply + 4)
    val repeatedRef = exact.ref.copy(
      id = "repeated-root-board-relation",
      position = repeatedPosition
    )
    val repeatedGraph = context.graph.add(EvidenceRecord(repeatedRef, relation))
    val repeatedPrimitive = valid.copy(
      binding = valid.binding.copy(source = repeatedRef, provenance = Nil),
      rootOwnedProof = Some(RootOwnedEffectProof.RootRelation(repeatedRef, relation))
    )
    assert(!RootOwnedEffectPolicy.admits(context.cause, repeatedGraph, repeatedPrimitive))

    val repeatedCarrier = valid.copy(
      binding = valid.binding.copy(provenance = List(repeatedRef))
    )
    assert(!RootOwnedEffectPolicy.admits(context.cause, repeatedGraph, repeatedCarrier))

  test("exact causal-root occurrence is semantic-FEN plus ply for every primitive family"):
    val sameOccurrence = queenPosition.copy(
      fen = "4k3/8/8/8/8/8/8/3QK3 w - - 17 42"
    )
    val repeatedOccurrence = queenPosition.copy(ply = queenPosition.ply + 4)
    val oppositeSideToMove = queenPosition.copy(
      fen = "4k3/8/8/8/8/8/8/3QK3 b - - 0 1",
      sideToMove = Some(Black)
    )

    RootOwnedEffectPrimitiveKind.values
      .filterNot(_ == RootOwnedEffectPrimitiveKind.Unspecified)
      .foreach { primitiveKind =>
        assert(
          RootOwnedEffectPolicy.sameCausalRootOccurrence(queenPosition, sameOccurrence),
          s"$primitiveKind must accept the same semantic board occurrence"
        )
        assert(
          !RootOwnedEffectPolicy.sameCausalRootOccurrence(queenPosition, repeatedOccurrence),
          s"$primitiveKind must reject the same board at another ply"
        )
        assert(
          !RootOwnedEffectPolicy.sameCausalRootOccurrence(queenPosition, oppositeSideToMove),
          s"$primitiveKind must keep side to move in the semantic board"
        )
      }

    val repeatedRecordBase = relationRecord(
      "repeated-projection-root-relation",
      queenLine,
      RelationWitnessDetail.HangingPiece(
        EvidenceSquare("h5"),
        EvidenceSquare("e8"),
        EvidencePieceRole(Queen.name),
        EvidencePieceRole("king")
      ),
      List(queenLine.rootMove)
    )
    val sameOccurrenceRecord = repeatedRecordBase.copy(
      ref = repeatedRecordBase.ref.copy(position = sameOccurrence)
    )
    val repeatedRecord = repeatedRecordBase.copy(
      ref = repeatedRecordBase.ref.copy(position = repeatedOccurrence)
    )
    assertEquals(
      projection(RelativeCauseKind.MissedTacticalResource, sameOccurrenceRecord).size,
      1
    )
    assertEquals(projection(RelativeCauseKind.MissedTacticalResource, repeatedRecord), Nil)


  test("plan-event root ownership rejects a repeated board occurrence at another ply"):
    val fixture = planEventRecord("plan-root-occurrence", robust = true)
    val event = fixture.record.payload match
      case payload: PlanCausalEventEvidence => payload
      case _                                => fail("expected plan event")
    assert(
      RootOwnedEffectPolicy.planEventOwnsRoot(
        fixture.record.ref,
        event,
        fixture.line,
        White
      )
    )

    val sameOccurrenceSource = fixture.record.ref.copy(
      id = "plan-root-same-occurrence",
      position = fixture.position.copy(
        fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 17 42"
      )
    )
    assert(
      RootOwnedEffectPolicy.planEventOwnsRoot(
        sameOccurrenceSource,
        event,
        fixture.line,
        White
      )
    )

    val repeatedSource = fixture.record.ref.copy(
      id = "plan-root-repeated-occurrence",
      position = fixture.position.copy(ply = fixture.position.ply + 4)
    )
    assert(
      !RootOwnedEffectPolicy.planEventOwnsRoot(
        repeatedSource,
        event,
        fixture.line,
        White
      )
    )

    val repeatedTransition = event.copy(rootTransition = event.rootTransition.copy(
      from = event.rootTransition.from.copy(ply = event.rootTransition.from.ply + 4)
    ))
    assert(
      !RootOwnedEffectPolicy.planEventOwnsRoot(
        fixture.record.ref,
        repeatedTransition,
        fixture.line,
        White
      )
    )

  test("defensive threat projection keeps the defender as root actor"):
    val fen = "4k3/8/8/8/7q/8/6P1/4K3 w - - 0 1"
    val position = PositionNodeRef(fen, 0, Some(White))
    val defenseLine = LineNodeRef("only-defense", "g2g3", 1, LineNodeRole.BestReference)
    val threat = Threat(
      threatActor = Black,
      kind = ThreatKind.Mate,
      turnsToImpact = 1,
      motifs = List(Motif.Check(
        Queen,
        chess.Square.fromKey("e1").getOrElse(fail("expected e1")),
        Motif.CheckType.Normal,
        Black,
        0,
        Some("d8h4")
      )),
      attackSquares = List("e1"),
      targetPieces = List("king"),
      bestDefense = Some(defenseLine.rootMove),
      defenseCount = 1
    )
    val exact = EvidenceRecord(
      evidenceRef(
        "exact-defense",
        EvidenceProducer.ThreatPressureProducer,
        EvidenceLayer.ThreatPressure,
        position,
        Some(defenseLine),
        EvidenceScope.ReferenceTransition
      ),
      ThreatEpisodeEvidence(ThreatEpisode.fromThreat(threat, 0))
    )
    val bindings = projection(
      RelativeCauseKind.OnlyDefenseNecessity,
      exact,
      position,
      defenseLine,
      candidateRoot = "e1f1"
    )
    assertEquals(bindings.count(_.specificTargetMechanismReady), 1)
    val binding = bindings.head
    assertRootActor(binding, defenseLine.rootMove, "white", Pawn.name, "g2", "g3")
    assertEquals(objectKeys(binding.actor, EvidenceObjectKind.Side), Set("white"))
    assert(objectKeys(binding.witness, EvidenceObjectKind.Side)("black"))
    assert(objectKeys(binding.target, EvidenceObjectKind.Square)("e1"))
    assertEquals(
      projectionChannels(
        RelativeCauseKind.OnlyDefenseNecessity,
        exact,
        position,
        defenseLine,
        candidateRoot = "e1f1"
      ).map(_.directChange),
      List(DirectCausalChange.Prevented)
    )

    val sibling = LineNodeRef("sibling-defense", defenseLine.rootMove, 7, LineNodeRole.Threat)
    val siblingRecord = exact.copy(ref = exact.ref.copy(line = Some(sibling)))
    assertEquals(
      projection(
        RelativeCauseKind.OnlyDefenseNecessity,
        siblingRecord,
        position,
        defenseLine,
        candidateRoot = "e1f1"
      ),
      Nil
    )

    val roleOnlyThreat = threat.copy(
      kind = ThreatKind.Material,
      attackSquares = Nil,
      targetPieces = List(Queen.name)
    )
    val roleOnly = exact.copy(
      ref = exact.ref.copy(id = "role-only-defense"),
      payload = ThreatEpisodeEvidence(ThreatEpisode.fromThreat(roleOnlyThreat, 0))
    )
    assertEquals(
      projection(
        RelativeCauseKind.OnlyDefenseNecessity,
        roleOnly,
        position,
        defenseLine,
        candidateRoot = "e1f1"
      ),
      Nil
    )

  test("candidate self-threat cannot be laundered as a candidate liability"):
    val referenceLine = LineNodeRef("self-threat-reference", "e1f1", 1, LineNodeRole.BestReference)
    val candidateLine = LineNodeRef(
      s"${referenceLine.id}-candidate",
      queenLine.rootMove,
      2,
      LineNodeRole.Played
    )
    val threat = Threat(
      threatActor = White,
      kind = ThreatKind.Mate,
      turnsToImpact = 1,
      motifs = List(Motif.Check(
        Queen,
        chess.Square.fromKey("e8").getOrElse(fail("expected e8")),
        Motif.CheckType.Normal,
        White,
        0,
        Some(candidateLine.rootMove)
      )),
      attackSquares = List("e8"),
      targetPieces = List("king"),
      bestDefense = None,
      defenseCount = 0
    )
    val record = EvidenceRecord(
      evidenceRef(
        "candidate-self-threat",
        EvidenceProducer.ThreatPressureProducer,
        EvidenceLayer.ThreatPressure,
        queenPosition,
        Some(candidateLine),
        EvidenceScope.PlayedTransition
      ),
      ThreatEpisodeEvidence(ThreatEpisode.fromThreat(threat, 0))
    )

    assertEquals(
      projectionChannels(
        RelativeCauseKind.CandidateTacticalLiability,
        record,
        queenPosition,
        referenceLine,
        candidateRoot = candidateLine.rootMove,
        sourceSide = RelativeCauseSourceSide.Candidate,
        attributionKind = CauseAttributionKind.CandidateAllowsLiability
      ),
      Nil
    )
    val valueChannels = projectionChannels(
      RelativeCauseKind.KingForcing,
      record,
      queenPosition,
      referenceLine,
      candidateRoot = candidateLine.rootMove,
      sourceSide = RelativeCauseSourceSide.Candidate,
      attributionKind = CauseAttributionKind.CandidateCreatesValue
    )
    assertEquals(valueChannels.map(_.directChange), List(DirectCausalChange.Occurred))



  test("central effect gate preserves exact restriction but rejects zero delta and maintenance relabeling"):
    val to = PositionNodeRef(
      PrincipalVariationEvidence.legalFenAfter(queenFen, queenLine.rootMove).getOrElse(fail("legal queen root")),
      1,
      Some(Black)
    )

    def structuralRecord(id: String, consequence: TransitionConsequence): EvidenceRecord =
      val ref = evidenceRef(
        id,
        EvidenceProducer.StructuralDeltaProducer,
        EvidenceLayer.StructuralDelta,
        queenPosition,
        Some(queenLine),
        EvidenceScope.ReferenceTransition
      )
      EvidenceRecord(
        ref,
        StructuralDeltaEvidence(
          StructuralTransitionBinding(
            queenLine.rootMove,
            TransitionEdgeRole.Reference,
            queenPosition,
            to,
            Some(queenLine),
            White
          ),
          signals = Nil,
          consequences = List(consequence)
        )
      )

    val restriction = structuralRecord(
      "exact-restriction",
      TransitionConsequence(
        TransitionConsequenceKind.OpponentMobilityRestriction,
        StructuralSignalPolarity.Gain,
        strength = 1,
        subjects = List("pawn:a7-a6:advance-restricted")
      )
    )
    val restrictionChannels = projectionChannels(RelativeCauseKind.OpponentRestriction, restriction)
    assertEquals(restrictionChannels.map(_.directChange), List(DirectCausalChange.Prevented))
    assert(objectKeys(restrictionChannels.head.binding.target, EvidenceObjectKind.Square)("a6"))

    val zeroDelta = restriction.copy(
      ref = restriction.ref.copy(id = "zero-restriction"),
      payload = restriction.payload match
        case delta: StructuralDeltaEvidence =>
          delta.copy(consequences = delta.consequences.map(_.copy(strength = 0)))
        case _ => fail("expected structural delta")
    )
    assertEquals(projection(RelativeCauseKind.OpponentRestriction, zeroDelta), Nil)

    val centerLeaf = structuralRecord(
      "center-leaf",
      TransitionConsequence(
        TransitionConsequenceKind.CenterControlGain,
        StructuralSignalPolarity.Gain,
        strength = 2,
        subjects = List("e4")
      )
    )
    val centerCarrier = EvidenceRecord(
      evidenceRef(
        "center-maintenance-carrier",
        EvidenceProducer.StrategicMechanismProducer,
        EvidenceLayer.StrategicMechanism,
        queenPosition,
        Some(queenLine),
        EvidenceScope.ReferenceTransition
      ),
      StrategicMechanismEvidence(
        StrategicMechanismKind.CenterControl,
        List(StrategicMechanismSignal(
          StrategicMechanismSignalKind.StructuralDelta,
          "static-center-support",
          centerLeaf.ref,
          2,
          Some(StrategicAxisDetail(
            StrategicAxisKind.SpaceCenter,
            StrategicAxisPolarity.Support,
            "static-center-support"
          ))
        )),
        Nil
      ),
      parents = List(centerLeaf.ref)
    )
    assertEquals(
      projection(
        RelativeCauseKind.CenterControlGain,
        centerCarrier,
        extraRecords = List(centerLeaf)
      ),
      Nil
    )

  test("wrong recapturer is explained by the owned loss rather than the recapture event"):
    assert(RelativeCauseKind.acceptsLineConsequence(
      RelativeCauseKind.WrongRecapturer,
      LineConsequenceKind.MaterialLoss
    ))
    assert(!RelativeCauseKind.acceptsLineConsequence(
      RelativeCauseKind.WrongRecapturer,
      LineConsequenceKind.RecaptureSequence
    ))
    assert(!RelativeCauseKind.acceptsLineConsequence(
      RelativeCauseKind.WrongRecapturer,
      LineConsequenceKind.RecoveryWindow
    ))
    assert(RelativeCauseKind.acceptsLineConsequence(
      RelativeCauseKind.RecaptureRecoveryWindow,
      LineConsequenceKind.RecoveryWindow
    ))

  test("tactical carrier needs an exact root-owned primitive and never completes from labels"):
    val target = chess.Square.fromKey("e8").getOrElse(fail("expected e8"))
    val motifRef = evidenceRef(
      "root-motif",
      EvidenceProducer.MoveMotifProducer,
      EvidenceLayer.MoveMotif,
      queenPosition,
      Some(queenLine),
      EvidenceScope.ReferenceTransition
    )
    val motif = EvidenceRecord(
      motifRef,
      MoveMotifEvidence(MoveMotifEvent.fromMotif(
        queenLine.rootMove,
        Motif.Check(Queen, target, Motif.CheckType.Normal, White, 0, Some(queenLine.rootMove))
      ))
    )
    val carrierRef = evidenceRef(
      "tactical-carrier",
      EvidenceProducer.TacticalMechanismProducer,
      EvidenceLayer.TacticalMechanism,
      queenPosition,
      Some(queenLine),
      EvidenceScope.ReferenceTransition
    )
    val carrier = EvidenceRecord(
      carrierRef,
      TacticalMechanismEvidence(
        TacticalMechanismKind.KingForcing,
        Some(queenLine.rootMove),
        Some(queenLine),
        List(TacticalMechanismSignal(
          TacticalMechanismSignalKind.Motif,
          "root-check",
          EvidenceLayer.MoveMotif,
          Some(motifRef)
        ))
      ),
      parents = List(motifRef)
    )
    val bindings = projection(
      RelativeCauseKind.KingForcing,
      carrier,
      extraRecords = List(motif)
    )
    assertEquals(bindings.count(_.specificTargetMechanismReady), 1)
    assertEquals(bindings.head.source.id, carrierRef.id)
    assertEquals(bindings.head.provenance.map(_.id), List(motifRef.id))

    val labelOnly = carrier.copy(
      ref = carrier.ref.copy(id = "label-only-carrier"),
      payload = TacticalMechanismEvidence(
        TacticalMechanismKind.KingForcing,
        Some(queenLine.rootMove),
        Some(queenLine),
        List(TacticalMechanismSignal(
          TacticalMechanismSignalKind.Motif,
          "king-forcing",
          EvidenceLayer.MoveMotif,
          None
        ))
      ),
      parents = Nil
    )
    assertEquals(projection(RelativeCauseKind.KingForcing, labelOnly), Nil)

  test("positive structural change cannot substitute for an exact PlanResult"):
    val to = PositionNodeRef(
      PrincipalVariationEvidence.legalFenAfter(queenFen, queenLine.rootMove).getOrElse(fail("legal queen root")),
      queenPosition.ply + 1,
      Some(Black)
    )
    val consequence = TransitionConsequence(
      TransitionConsequenceKind.OutpostGain,
      StructuralSignalPolarity.Gain,
      strength = 2,
      subjects = List("outpost:queen:h5")
    )
    val ref = evidenceRef(
      "structural-only-plan-improvement",
      EvidenceProducer.StructuralDeltaProducer,
      EvidenceLayer.StructuralDelta,
      queenPosition,
      Some(queenLine),
      EvidenceScope.ReferenceTransition
    )
    val structuralOnly = EvidenceRecord(
      ref,
      StructuralDeltaEvidence(
        StructuralTransitionBinding(
          queenLine.rootMove,
          TransitionEdgeRole.Reference,
          queenPosition,
          to,
          Some(queenLine),
          White
        ),
        signals = Nil,
        consequences = List(consequence)
      )
    )

    assertEquals(RelativeCauseKind.structuralConsequences(RelativeCauseKind.PlanImprovement, structuralOnly.payload.asInstanceOf[StructuralDeltaEvidence]), Nil)
    assertEquals(projectionChannels(RelativeCauseKind.PlanImprovement, structuralOnly), Nil)

  test("plan projection distinguishes robust improvement from refuted contradiction"):
    val robust = planEventRecord("robust-plan", robust = true)
    val improvement = projection(
      RelativeCauseKind.PlanImprovement,
      robust.record,
      robust.position,
      robust.line,
      candidateRoot = "a2a3"
    )
    assertEquals(improvement.count(_.specificTargetMechanismReady), 1)
    assertRootActor(improvement.head, robust.line.rootMove, "white", Knight.name, "b1", "c3")
    assert(objectKeys(improvement.head.target, EvidenceObjectKind.Square)("d5"))
    assertEquals(objectKeys(improvement.head.target, EvidenceObjectKind.PlanSubject), Set.empty)
    assert(objectKeys(improvement.head.witness, EvidenceObjectKind.PlanSubject).nonEmpty)
    assert(improvement.head.consequence.exists(_.key.endsWith(PlanCausalRobustness.Robust.toString.toLowerCase)))
    assertEquals(
      projectionChannels(
        RelativeCauseKind.PlanImprovement,
        robust.record,
        robust.position,
        robust.line,
        candidateRoot = "a2a3"
      ).map(_.directChange),
      List(DirectCausalChange.Occurred)
    )

    val (refuted, refutedReferenceLine) = candidatePlanFixture(
      planEventRecord("refuted-plan", robust = false),
      "refuted-plan"
    )
    val contradictionChannels = projectionChannels(
      RelativeCauseKind.PlanContradiction,
      refuted.record,
      refuted.position,
      refutedReferenceLine,
      candidateRoot = refuted.line.rootMove,
      sourceSide = RelativeCauseSourceSide.Candidate,
      attributionKind = CauseAttributionKind.CandidateAllowsLiability
    )
    val contradiction = contradictionChannels.map(_.binding)
    assertEquals(contradiction.count(_.specificTargetMechanismReady), 1)
    assert(contradiction.head.consequence.exists(_.key.endsWith(PlanCausalRobustness.Refuted.toString.toLowerCase)))
    assert(!contradiction.head.consequence.exists(_.key.endsWith(PlanCausalRobustness.Robust.toString.toLowerCase)))
    assertEquals(
      contradictionChannels.map(_.directChange),
      List(DirectCausalChange.Refuted)
    )
    assert(contradictionChannels.forall(RelativeCauseDominancePolicy.ownsPlanCausalProof))
    assert(
      RelativeCauseDominancePolicy
        .moreSpecificKinds(RelativeCauseKind.StrategicConcession)(RelativeCauseKind.PlanContradiction)
    )
    val genericRef = refuted.record.ref.copy(
      id = "refuted-plan-generic-cause",
      layer = EvidenceLayer.RelativeCause,
      confidence = EvidenceConfidence.EngineBacked
    )
    val specificRef = genericRef.copy(id = "refuted-plan-specific-cause")
    val dominanceContext = projectionContext(
      RelativeCauseKind.StrategicConcession,
      refuted.record,
      refuted.position,
      refutedReferenceLine,
      candidateRoot = refuted.line.rootMove,
      sourceSide = RelativeCauseSourceSide.Candidate,
      attributionKind = CauseAttributionKind.CandidateAllowsLiability
    )
    val genericCause = dominanceContext.cause
    val specificCause = genericCause.copy(kind = RelativeCauseKind.PlanContradiction)
    def selected(ref: EvidenceRef, rank: Int) =
      CrossComparisonExposureDecision(
        causeEvidenceId = ref.id,
        status = CrossComparisonExposureStatus.SelectedPrimary,
        reason = CrossComparisonExposureReason.PlayedVsBestOwnsPlayedResponsibility,
        representativeCauseEvidenceId = ref.id,
        effectMode = Some(PlayerFacingCauseEffectMode.PlayedLiability),
        comparisonExposureRank = Some(rank)
      )
    val dominance = RelativeCauseDominancePolicy.resolve(
      List(genericCause -> genericRef, specificCause -> specificRef),
      List(selected(genericRef, 0), selected(specificRef, 1)),
      Map(
        genericRef.id -> contradictionChannels,
        specificRef.id -> contradictionChannels
      ),
      dominanceContext.graph
    )
    val dominanceById = dominance.decisions.map(item => item.causeEvidenceId -> item).toMap
    assertEquals(
      dominanceById(genericRef.id).dominatingCauseEvidenceIds,
      List(specificRef.id)
    )
    assert(dominanceById(specificRef.id).retained)



  test("taxonomy aliases collapse only when the exact PlanResult and causal route agree"):
    val first = planEventRecord(
      "plan-result-alias-first",
      robust = true,
      planKind = PlanKind.WorstPieceImprovement
    )
    val alias = planEventRecord(
      "plan-result-alias-second",
      robust = true,
      planKind = PlanKind.OutpostEntrenchment
    )
    def admitted(
        fixture: PlanFixture,
        causeId: String
    ): (RelativeCauseFact, EvidenceRef, DirectCauseChannel, TypedEvidenceGraph) =
      val context = projectionContext(
        RelativeCauseKind.PlanImprovement,
        fixture.record,
        fixture.position,
        fixture.line,
        candidateRoot = "a2a3"
      )
      val raw = EvidenceObjectBinding.rawDirectSentenceChannelsForProjection(context.cause, context.graph)
      assertEquals(raw.size, 1)
      val cause = context.cause.copy(
        directEffectAdmission = DirectEffectAdmission.Restricted(Set(raw.head.causalSignature))
      )
      val causeRef = evidenceRef(
        causeId,
        EvidenceProducer.RelativeMoveProducer,
        EvidenceLayer.RelativeCause,
        fixture.position,
        Some(fixture.line),
        EvidenceScope.Counterfactual,
        EvidenceConfidence.EngineBacked
      )
      val graph = context.graph.add(EvidenceRecord(
        causeRef,
        RelativeCauseFactEvidence(cause),
        parents = cause.supportEvidence
      ))
      assert(PlayerFacingCauseReadinessPolicy.ready(cause, causeRef, graph))
      (cause, causeRef, raw.head, graph)

    val firstCase = admitted(first, "plan-result-alias-first-cause")
    val aliasCase = admitted(alias, "plan-result-alias-second-cause")
    val firstDescriptor = firstCase._3.rootOwnedEffectDescriptor.getOrElse(fail("expected descriptor"))
    val aliasDescriptor = aliasCase._3.rootOwnedEffectDescriptor.getOrElse(fail("expected descriptor"))
    assertNotEquals(firstDescriptor.identity.planIds, aliasDescriptor.identity.planIds)
    assertEquals(firstDescriptor.identity.planResult, aliasDescriptor.identity.planResult)
    assertEquals(firstCase._3.causalSignature, aliasCase._3.causalSignature)
    assertEquals(firstCase._3.explanatoryNoveltySignature, aliasCase._3.explanatoryNoveltySignature)
    assert(
      RootOwnedEffectTruthView.from(List(firstCase._3)).agreesCausallyWith(
        RootOwnedEffectTruthView.from(List(aliasCase._3))
      )
    )
    assertEquals(objectKeys(firstCase._3.binding.target, EvidenceObjectKind.PlanSubject), Set.empty)
    assertEquals(
      objectKeys(firstCase._3.binding.witness, EvidenceObjectKind.PlanSubject),
      Set(PlanKind.WorstPieceImprovement.id)
    )
    assertEquals(
      objectKeys(aliasCase._3.binding.witness, EvidenceObjectKind.PlanSubject),
      Set(PlanKind.OutpostEntrenchment.id)
    )

    val combinedGraph = (firstCase._4.records ++ aliasCase._4.records)
      .distinctBy(_.ref.id)
      .foldLeft(TypedEvidenceGraph.empty)((graph, record) => graph.add(record))
    val decisions = CrossComparisonCauseExposurePolicy.resolve(
      List(firstCase._1 -> firstCase._2, aliasCase._1 -> aliasCase._2),
      combinedGraph,
      playedMoves = Set("a2a3")
    ).map(decision => decision.causeEvidenceId -> decision).toMap
    assertEquals(decisions.values.count(_.selected), 1)
    assertEquals(decisions.values.count(_.status == CrossComparisonExposureStatus.RedundantAcrossComparison), 1)

    val event = first.record.payload match
      case value: PlanCausalEventEvidence => value
      case _                              => fail("expected plan event")
    val assessment = event.exactRobustPublicResultAssessment.getOrElse(fail("expected robust result"))
    val identity = PlanResultSemanticIdentity.from(event, assessment)
    val differentTarget = PlanResultSemanticIdentity.from(
      event,
      assessment.copy(consequence = assessment.consequence.copy(subjects = List("knight:c3-e4")))
    )
    val differentSource = PlanResultSemanticIdentity.from(
      event,
      assessment.copy(sourcePlyOffset = assessment.sourcePlyOffset + 1)
    )
    val differentBranches = PlanResultSemanticIdentity.from(
      event,
      assessment.copy(observations = assessment.observations.updated(
        0,
        assessment.observations.head.copy(outcome = PlanCausalBranchOutcome.Diverted)
      ))
    )
    val duplicateObservation = PlanResultSemanticIdentity.from(
      event,
      assessment.copy(observations = assessment.observations :+ assessment.observations.head)
    )
    val differentReplyRoute = PlanResultSemanticIdentity.from(
      event,
      assessment.copy(observations = assessment.observations.updated(
        0,
        assessment.observations.head.copy(replyMove = "b7b5")
      ))
    )
    val differentRoute = identity.copy(causalRoute = identity.causalRoute.map(route =>
      route.copy(plyOffset = route.plyOffset + 1)
    ))
    assertNotEquals(identity, differentTarget)
    assertNotEquals(identity, differentSource)
    assertNotEquals(identity, differentBranches)
    assertEquals(identity, duplicateObservation)
    assertNotEquals(identity, differentReplyRoute)
    assertNotEquals(identity, differentRoute)





  test("plan restriction re-proves deterrence and selects only its canonical consequence"):
    val exact = planRestrictionRecord("exact-plan-restriction", includeCanonical = true)
    val exactContext = projectionContext(
      RelativeCauseKind.OpponentRestriction,
      exact.record,
      exact.position,
      exact.line,
      candidateRoot = "e1d1",
      extraRecords = exact.extraRecords
    )
    val exactEvent = exact.record.payload match
      case event: PlanCausalEventEvidence => event
      case _                              => fail("expected plan restriction event")
    val exactDeterrence = exactEvent.opponentResourceDeterrence.getOrElse(
      fail("expected opponent resource deterrence")
    )
    val exactLines = exactContext.graph.canonicalCandidateLinesFromEvidence
    assertEquals(
      exactLines.map(_.ref).toSet,
      exact.extraRecords.flatMap(_.ref.line).toSet
    )
    assertEquals(
      exactDeterrence.resourceMove(exactEvent.perspective, exactLines, exactContext.graph),
      Some("f7f6")
    )
    assert(
      exactDeterrence.proven(
        exactEvent.rootLine,
        exactEvent.rootTransition,
        exactEvent.episode.getOrElse(fail("expected causal episode")),
        exactLines,
        exactContext.graph
      )
    )
    val channels = EvidenceObjectBinding.directCauseChannelsForProjection(
      exactContext.cause,
      exactContext.graph
    )
    assertEquals(channels.size, 1)
    assertEquals(channels.map(_.directChange), List(DirectCausalChange.Prevented))
    val channel = channels.head
    assertEquals(channel.binding.line, Some(exact.line))
    assertEquals(
      channel.binding.actor.map(obj => obj.kind -> obj.key).toSet,
      Set(
        EvidenceObjectKind.Move -> "e5c4",
        EvidenceObjectKind.Side -> "white",
        EvidenceObjectKind.Piece -> "knight",
        EvidenceObjectKind.Square -> "e5",
        EvidenceObjectKind.Square -> "c4"
      )
    )
    assert(
      Set(
        EvidenceObjectKind.Piece -> "pawn",
        EvidenceObjectKind.Square -> "f7",
        EvidenceObjectKind.Square -> "f6"
      ).subsetOf(channel.binding.target.map(obj => obj.kind -> obj.key).toSet)
    )
    assertEquals(
      channel.binding.mechanism.map(obj => obj.kind -> obj.key),
      List(EvidenceObjectKind.Mechanism -> "opponentmobilityrestriction")
    )
    assert(channel.binding.consequence.exists(_.key == exact.canonical.anchorKey.toLowerCase))
    assert(!channel.binding.consequence.exists(_.key.startsWith("targetpressuregain")))

    val mismatchedComparison = exact.record.copy(
      ref = exact.record.ref.copy(id = "mismatched-plan-restriction-comparison"),
      payload = exactEvent.copy(opponentResourceDeterrence = Some(exactDeterrence.copy(
        comparisons = exactDeterrence.comparisons.map(_.copy(rootMove = "a7a6"))
      )))
    )
    assertEquals(
      projectionChannels(
        RelativeCauseKind.OpponentRestriction,
        mismatchedComparison,
        exact.position,
        exact.line,
        candidateRoot = "e1d1",
        extraRecords = exact.extraRecords
      ),
      Nil
    )

    val siblingOnly = planRestrictionRecord("sibling-only-plan-restriction", includeCanonical = false)
    assertEquals(
      projectionChannels(
        RelativeCauseKind.OpponentRestriction,
        siblingOnly.record,
        siblingOnly.position,
        siblingOnly.line,
        candidateRoot = "e1d1",
        extraRecords = siblingOnly.extraRecords
      ),
      Nil
    )

  private final case class PlanFixture(
      record: EvidenceRecord,
      position: PositionNodeRef,
      line: LineNodeRef
  )

  private final case class PlanRestrictionFixture(
      record: EvidenceRecord,
      position: PositionNodeRef,
      line: LineNodeRef,
      canonical: TransitionConsequence,
      extraRecords: List[EvidenceRecord]
  )

  private def candidatePlanFixture(
      fixture: PlanFixture,
      id: String
  ): (PlanFixture, LineNodeRef) =
    val referenceLine = LineNodeRef(s"$id-comparison-reference", "a2a3", 1, LineNodeRole.BestReference)
    val candidateLine = LineNodeRef(
      s"${referenceLine.id}-candidate",
      fixture.line.rootMove,
      2,
      LineNodeRole.Played
    )
    val payload = fixture.record.payload match
      case event: PlanCausalEventEvidence =>
        event.copy(rootTransition = event.rootTransition.copy(
          role = TransitionEdgeRole.Played,
          line = Some(candidateLine)
        ))
      case _ =>
        fail("expected plan event")
    val record = fixture.record.copy(
      ref = fixture.record.ref.copy(
        line = Some(candidateLine),
        scope = EvidenceScope.PlayedTransition
      ),
      payload = payload
    )
    PlanFixture(record, fixture.position, candidateLine) -> referenceLine

  private def planRestrictionRecord(
      id: String,
      includeCanonical: Boolean
  ): PlanRestrictionFixture =
    val fen = "4k3/5p2/8/4N3/8/2B5/8/4K3 w - - 0 1"
    val rootStep = legalStep(fen, "e5c4", 1)
    val resourceStep = legalStep(rootStep.fenAfter, "f7f6", 2)
    val captureStep = legalStep(resourceStep.fenAfter, "c3f6", 3)
    val comparisonStep = legalStep(rootStep.fenAfter, "f7f5", 2)
    val position = PositionNodeRef(fen, 0, Some(White))
    val afterRoot = PositionNodeRef(rootStep.fenAfter, 1, Some(Black))
    val rootLine = LineNodeRef(s"$id-root", rootStep.moveUci, 1, LineNodeRole.BestReference)
    val resourceLine = LineNodeRef(s"$id-resource", resourceStep.moveUci, 1, LineNodeRole.Threat)
    val comparisonLine = LineNodeRef(s"$id-comparison", comparisonStep.moveUci, 2, LineNodeRole.Threat)
    val canonical = OpponentResourceDeterrenceProof.consequence(resourceStep.moveUci)
    val sibling = TransitionConsequence(
      TransitionConsequenceKind.TargetPressureGain,
      StructuralSignalPolarity.Gain,
      strength = 3,
      subjects = List("king:e8")
    )
    val rootConsequences = if includeCanonical then List(canonical, sibling) else List(sibling)
    val rootIdentity = PlanEventIdentity(
      rootStep.moveUci,
      PlanKind.WorstPieceImprovement,
      Some(Knight.name),
      Some("e5"),
      Some("c4"),
      Nil,
      Nil
    )
    val captureIdentity = PlanEventIdentity(
      captureStep.moveUci,
      PlanKind.WorstPieceImprovement,
      Some(Bishop.name),
      Some("c3"),
      Some("f6"),
      List("pawn:f6"),
      List("material-gain")
    )
    val root = PlanCausalEventNode(rootIdentity, rootStep, White, rootConsequences, Nil)
    val captureEvent = PlanCausalEventNode(captureIdentity, captureStep, White, Nil, Nil)
    val trajectory = LineAccessTrajectory
      .find(rootStep, captureStep, List(resourceStep))
      .getOrElse(fail("expected root clearance to enable the deterrence capture"))
    val dependency = PlanCausalEventDependency(
      root,
      captureEvent,
      PlanCausalDependencyKind.LineAccessPrecondition,
      PlanCausalDependencyProof.LineAccess(trajectory),
      plyOffset = 2
    )
    val episode = PlanCausalEpisode(root, List(captureEvent), List(dependency), Nil)
    assert(episode.causalEpisodeProven)
    val deterrence = OpponentResourceDeterrenceProof(
      sourceProbeId = s"$id-resource-probe",
      resourceLine = resourceLine,
      comparisons = List(OpponentResourceComparison(
        rootMove = comparisonLine.rootMove,
        sourceProbeId = s"$id-comparison-probe",
        resourceLine = comparisonLine
      )),
      materialGainPlyOffset = 1
    )
    val event = PlanCausalEventEvidence(
      StructuralTransitionBinding(
        rootStep.moveUci,
        TransitionEdgeRole.Reference,
        position,
        afterRoot,
        Some(rootLine),
        White
      ),
      episode,
      branchWitnesses = Nil,
      opponentResourceDeterrence = Some(deterrence)
    )
    val planRef = evidenceRef(
      id,
      EvidenceProducer.PlanCausalEventProducer,
      EvidenceLayer.PlanCausalEvent,
      position,
      Some(rootLine),
      EvidenceScope.ReferenceTransition,
      EvidenceConfidence.EngineBacked
    )

    def lineRecord(
        recordId: String,
        line: LineNodeRef,
        recordPosition: PositionNodeRef,
        replay: List[LineReplayStep],
        material: Option[LineMaterialSummary] = None
    ): EvidenceRecord =
      val ref = evidenceRef(
        recordId,
        EvidenceProducer.LegalLineProducer,
        EvidenceLayer.Line,
        recordPosition,
        Some(line),
        line.role.scope
      )
      EvidenceRecord(
        ref,
        LineFactEvidence(line = line, material = material, replay = replay)
      )

    def evalRecord(recordId: String, lineRecord: EvidenceRecord, scoreCp: Int): EvidenceRecord =
      val line = lineRecord.ref.line.getOrElse(fail("expected evaluated line"))
      val lineMoves = lineRecord.payload match
        case facts: LineFactEvidence => facts.lineReplaySteps.map(_.moveUci)
        case _                       => fail("expected line facts for evaluated line")
      EvidenceRecord(
        evidenceRef(
          recordId,
          EvidenceProducer.EngineEvalProducer,
          EvidenceLayer.Eval,
          lineRecord.ref.position,
          Some(line),
          line.role.scope,
          EvidenceConfidence.EngineBacked
        ),
        CandidateLineEvaluationEvidence(
          line,
          lila.chessjudgment.model.line.CandidateLineEvaluation.EngineSearch(
            EngineLine(lineMoves, scoreCp, mate = None, depth = 18)
          )
        ),
        parents = List(lineRecord.ref)
      )

    val capture = LineMaterialCapture(
      moveUci = captureStep.moveUci,
      plyOffset = 1,
      side = White,
      attackerRole = EvidencePieceRole(Bishop.name),
      capturedRole = EvidencePieceRole(Pawn.name),
      square = EvidenceSquare("f6"),
      valueCp = 100,
      recapture = false
    )
    val material = LineMaterialSummary(
      sideToMove = Black,
      captures = List(capture),
      netCaptureCpForMover = -100,
      maxGainCpForMover = 0,
      maxLossCpForMover = 100,
      hasRecaptureChain = false,
      hasRecoveryWindow = false,
      promotionGainCpForMover = 0,
      materialWindowComplete = true
    )
    val rootFacts = lineRecord(s"$id-root-line", rootLine, position, List(rootStep))
    val resourceFacts = lineRecord(
      s"$id-resource-line",
      resourceLine,
      afterRoot,
      List(resourceStep, captureStep),
      Some(material)
    )
    val comparisonFacts = lineRecord(
      s"$id-comparison-line",
      comparisonLine,
      afterRoot,
      List(comparisonStep)
    )
    val extraRecords = List(
      rootFacts,
      resourceFacts,
      comparisonFacts,
      evalRecord(s"$id-root-eval", rootFacts, scoreCp = 0),
      evalRecord(s"$id-resource-eval", resourceFacts, scoreCp = 1000),
      evalRecord(s"$id-comparison-eval", comparisonFacts, scoreCp = -1000)
    )
    PlanRestrictionFixture(EvidenceRecord(planRef, event), position, rootLine, canonical, extraRecords)

  private def planEventRecord(
      id: String,
      robust: Boolean,
      planKind: PlanKind = PlanKind.WorstPieceImprovement
  ): PlanFixture =
    val fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    val rootStep = legalStep(fen, "b1c3", 1)
    val replyStep = legalStep(rootStep.fenAfter, "a7a6", 2)
    val futureStep = legalStep(replyStep.fenAfter, "c3d5", 3)
    val line = LineNodeRef(s"$id-reference", rootStep.moveUci, 1, LineNodeRole.BestReference)
    val position = PositionNodeRef(fen, 0, Some(White))
    val afterRoot = PositionNodeRef(rootStep.fenAfter, 1, Some(Black))
    val rootIdentity = PlanEventIdentity(
      rootStep.moveUci,
      planKind,
      Some(Knight.name),
      Some("b1"),
      Some("c3"),
      Nil,
      Nil
    )
    val futureIdentity = PlanEventIdentity(
      futureStep.moveUci,
      planKind,
      Some(Knight.name),
      Some("c3"),
      Some("d5"),
      List("knight:c3-d5"),
      List("mobility-gain")
    )
    val result = TransitionConsequence(
      TransitionConsequenceKind.MobilityGain,
      StructuralSignalPolarity.Gain,
      strength = 2,
      subjects = List("knight:c3-d5")
    )
    val root = PlanCausalEventNode(rootIdentity, rootStep, White, Nil, Nil)
    val future = PlanCausalEventNode(futureIdentity, futureStep, White, List(result), Nil)
    val trajectory = LineObjectTrajectory
      .find(rootStep, List(replyStep, futureStep), maxPlyOffset = 2)
      .getOrElse(fail("expected same-knight trajectory"))
    val dependency = PlanCausalEventDependency(
      root,
      future,
      PlanCausalDependencyKind.ObjectStatePrecondition,
      PlanCausalDependencyProof.ObjectState(trajectory),
      plyOffset = 2
    )
    val episode = PlanCausalEpisode(root, List(future), List(dependency), Nil)
    assert(episode.causalEpisodeProven)
    val replies = List("a7a6", "a7a5", "b7b6").zipWithIndex.map { case (move, index) =>
      PlanCausalBranchWitness(
        sourceProbeId = s"$id-probe",
        line = LineNodeRef(s"$id-reply-$index", move, index + 1, LineNodeRole.Threat),
        outcome = if robust then PlanCausalBranchOutcome.Realized else PlanCausalBranchOutcome.Refuted,
        observedEpisode = Option.when(robust)(episode),
        observedConsequences = Option.when(robust)(List(result)).getOrElse(Nil),
        realizationMatch = Option.when(robust)(PlanCausalRealizationMatch.ExactMove),
        realizationMove = Option.when(robust)(future.moveUci),
        requiredPlyOffset = 2,
        certifiedHorizonPlyOffset = 2,
        observedPlyOffset = 2,
        observedThroughPlyOffset = 2,
        terminalOutcome = Option.unless(robust)(PlanCausalTerminalOutcome.Defeat),
        terminalPlyOffset = Option.unless(robust)(2),
        terminalStep = None
      )
    }
    val payload = PlanCausalEventEvidence(
      StructuralTransitionBinding(
        rootStep.moveUci,
        TransitionEdgeRole.Reference,
        position,
        afterRoot,
        Some(line),
        White
      ),
      episode,
      replies
    )
    val expectedRobustness =
      if robust then PlanCausalRobustness.Robust else PlanCausalRobustness.Refuted
    assert(
      payload.causalResultAssessments.nonEmpty &&
        payload.causalResultAssessments.forall(_.robustness == expectedRobustness)
    )
    val ref = evidenceRef(
      id,
      EvidenceProducer.PlanCausalEventProducer,
      EvidenceLayer.PlanCausalEvent,
      position,
      Some(line),
      EvidenceScope.ReferenceTransition,
      EvidenceConfidence.EngineBacked
    )
    PlanFixture(EvidenceRecord(ref, payload), position, line)

  private def relationRecord(
      id: String,
      line: LineNodeRef,
      detail: RelationWitnessDetail,
      lineMoves: List[String]
  ): EvidenceRecord =
    EvidenceRecord(
      evidenceRef(
        id,
        EvidenceProducer.TacticalRelationProducer,
        EvidenceLayer.Relation,
        queenPosition,
        Some(line),
        EvidenceScope.ReferenceTransition
      ),
      RelationFactEvidence.from(detail, lineMoves).getOrElse(fail("expected typed relation"))
    )

  private def projection(
      kind: RelativeCauseKind,
      proofRecord: EvidenceRecord,
      position: PositionNodeRef = queenPosition,
      referenceLine: LineNodeRef = queenLine,
      candidateRoot: String = "e1f1",
      extraRecords: List[EvidenceRecord] = Nil
  ): List[EvidenceObjectBinding] =
    projectionChannels(kind, proofRecord, position, referenceLine, candidateRoot, extraRecords).map(_.binding)

  private def projectionChannels(
      kind: RelativeCauseKind,
      proofRecord: EvidenceRecord,
      position: PositionNodeRef = queenPosition,
      referenceLine: LineNodeRef = queenLine,
      candidateRoot: String = "e1f1",
      extraRecords: List[EvidenceRecord] = Nil,
      sourceSide: RelativeCauseSourceSide = RelativeCauseSourceSide.Reference,
      attributionKind: CauseAttributionKind = CauseAttributionKind.ReferenceCreatesResource,
      additionalDirectProofRefs: List[EvidenceRef] = Nil
  ): List[DirectCauseChannel] =
    val context = projectionContext(
      kind,
      proofRecord,
      position,
      referenceLine,
      candidateRoot,
      extraRecords,
      sourceSide,
      attributionKind,
      additionalDirectProofRefs
    )
    EvidenceObjectBinding.directCauseChannelsForProjection(context.cause, context.graph)

  private final case class ProjectionContext(
      cause: RelativeCauseFact,
      graph: TypedEvidenceGraph
  )

  private def projectionContext(
      kind: RelativeCauseKind,
      proofRecord: EvidenceRecord,
      position: PositionNodeRef = queenPosition,
      referenceLine: LineNodeRef = queenLine,
      candidateRoot: String = "e1f1",
      extraRecords: List[EvidenceRecord] = Nil,
      sourceSide: RelativeCauseSourceSide = RelativeCauseSourceSide.Reference,
      attributionKind: CauseAttributionKind = CauseAttributionKind.ReferenceCreatesResource,
      additionalDirectProofRefs: List[EvidenceRef] = Nil
  ): ProjectionContext =
    val candidateLine = LineNodeRef(
      s"${referenceLine.id}-candidate",
      candidateRoot,
      2,
      LineNodeRole.Played
    )
    val comparisonRef = evidenceRef(
      s"${proofRecord.ref.id}-comparison",
      EvidenceProducer.RelativeMoveProducer,
      EvidenceLayer.CandidateComparison,
      position,
      Some(candidateLine),
      candidateLine.role.scope,
      EvidenceConfidence.EngineBacked
    )
    val comparison = CandidateComparisonFact(
      CandidateComparisonKind.PlayedVsBest,
      referenceLine,
      candidateLine,
      EvalComparison.fromLines(
        White,
        CandidateLineNode(
          referenceLine,
          lila.chessjudgment.model.line.CandidateLineEvaluation.EngineSearch(EngineLine(List(referenceLine.rootMove), 300, depth = 18)),
          evalRef(s"${proofRecord.ref.id}-reference-eval", position, referenceLine)
        ),
        CandidateLineNode(
          candidateLine,
          lila.chessjudgment.model.line.CandidateLineEvaluation.EngineSearch(EngineLine(List(candidateLine.rootMove), -300, depth = 18)),
          evalRef(s"${proofRecord.ref.id}-candidate-eval", position, candidateLine)
        )
      ).get,
      VerdictConfidence.EngineBacked
    )
    val directProofRefs = (proofRecord.ref :: additionalDirectProofRefs).distinctBy(_.id)
    val cause = RelativeCauseFact(
      kind,
      comparisonRef,
      directProofRefs,
      sourceSide,
      CauseAttribution(
        attributionKind,
        rootMoveMatched = true,
        directProofEligible = true
      ),
      Some(RelativeCauseProof(
        directProof = RelativeCauseProofSection(
          RelativeCauseProofRole.DirectProof,
          RelativeCauseProofStrength.Primary,
          directProofRefs
        )
      ))
    )
    val graph = (EvidenceRecord(comparisonRef, CandidateComparisonEvidence(comparison)) ::
      proofRecord :: extraRecords)
      .foldLeft(TypedEvidenceGraph.empty)((current, record) => current.add(record))
    ProjectionContext(cause, graph)

  private def evidenceRef(
      id: String,
      producer: EvidenceProducer,
      layer: EvidenceLayer,
      position: PositionNodeRef,
      line: Option[LineNodeRef],
      scope: EvidenceScope,
      confidence: EvidenceConfidence = EvidenceConfidence.LegalReplayVerified
  ): EvidenceRef =
    EvidenceRef(id, producer, layer, position, line, scope, confidence)

  private def evalRef(id: String, position: PositionNodeRef, line: LineNodeRef): EvidenceRef =
    evidenceRef(
      id,
      EvidenceProducer.EngineEvalProducer,
      EvidenceLayer.Eval,
      position,
      Some(line),
      line.role.scope,
      EvidenceConfidence.EngineBacked
    )

  private def legalStep(fen: String, move: String, ply: Int): LineReplayStep =
    LineReplayStep(
      ply,
      move,
      fen,
      PrincipalVariationEvidence.legalFenAfter(fen, move).getOrElse(fail(s"expected legal $move"))
    )

  private def assertRootActor(
      binding: EvidenceObjectBinding,
      move: String,
      side: String,
      piece: String,
      from: String,
      to: String
  ): Unit =
    assert(objectKeys(binding.actor, EvidenceObjectKind.Move)(move.toLowerCase))
    assert(objectKeys(binding.actor, EvidenceObjectKind.Side)(side.toLowerCase))
    assert(objectKeys(binding.actor, EvidenceObjectKind.Piece)(piece.toLowerCase))
    assert(Set(from.toLowerCase, to.toLowerCase).subsetOf(objectKeys(binding.actor, EvidenceObjectKind.Square)))

  private def objectKeys(
      objects: List[ConcreteChessObject],
      kind: EvidenceObjectKind
  ): Set[String] =
    objects.collect { case obj if obj.kind == kind => obj.key.toLowerCase }.toSet
