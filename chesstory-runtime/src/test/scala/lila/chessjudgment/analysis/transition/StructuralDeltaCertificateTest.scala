package lila.chessjudgment.model.judgment

import chess.{ Black, White }

import lila.chessjudgment.analysis.position.PositionRelationExtractor
import lila.chessjudgment.analysis.structure.StructuralDeltaAnalyzer
import lila.chessjudgment.analysis.transition.TransitionFactNormalizer
import lila.chessjudgment.model.line.PrincipalVariationEvidence

class StructuralDeltaCertificateTest extends munit.FunSuite:

  test("a legal transition certificate does not certify copied structural output"):
    val beforeFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    val record = structuralRecord(beforeFen, "e2e4")
    val payload = record.payload.asInstanceOf[StructuralDeltaEvidence]

    assert(payload.exactOutputInventoryCertified)
    assert(payload.relationChanges.nonEmpty)

    val omittedRelationChange = payload.copy(relationChanges = payload.relationChanges.tail)
    assert(!omittedRelationChange.exactOutputInventoryCertified)

    val injected = payload.copy(
      consequences = TransitionConsequence(
        TransitionConsequenceKind.BatteryFormation,
        strength = 1,
        subjectBindings = List(
          StructuralSubjectBinding.unbound(
            StructuralSubject.Battery(
              RelationBatteryFormationWitness(
                White,
                RelationColoredPieceWitness(
                  EvidenceSquare("g4"),
                  EvidencePieceRole("bishop"),
                  White
                ),
                RelationColoredPieceWitness(
                  EvidenceSquare("h5"),
                  EvidencePieceRole("queen"),
                  White
                ),
                RelationAxisSignal.Diagonal
              )
            )
          )
        ),
        targetBindings = List(
          StructuralSubjectBinding.unbound(
            StructuralSubject.PieceAt(Black, EvidencePieceRole("king"), EvidenceSquare("e8"))
          )
        )
      ) :: payload.consequences
    )
    assert(!injected.exactOutputInventoryCertified)

  test("distant ray churn does not recreate an existing battery formation"):
    val record = structuralRecord(
      "4k3/Q7/8/n7/8/R7/8/R6K w - - 0 1",
      "a7b7"
    )
    val payload = record.payload.asInstanceOf[StructuralDeltaEvidence]
    assert(!payload.signals.exists(_.kind == StructuralSignalKind.BatteryCreated))
    assert(!payload.consequences.exists(_.kind == TransitionConsequenceKind.BatteryFormation))

  test("one battery formation preserves both established directional ray proofs"):
    val record = structuralRecord(
      "7k/8/8/8/8/8/1R6/R6K w - - 0 1",
      "b2a2"
    )
    val payload = record.payload.asInstanceOf[StructuralDeltaEvidence]
    val battery =
      payload.consequences.filter(_.kind == TransitionConsequenceKind.BatteryFormation) match
        case only :: Nil => only
        case found       => fail(s"expected one battery formation, found ${found.size}")
    val proofKeys =
      battery.subjectBindings match
        case only :: Nil => only.relationKeys
        case found       => fail(s"expected one battery subject, found ${found.size}")

    assertEquals(proofKeys.size, 2)
    assertEquals(proofKeys, proofKeys.sortBy(_.stableKey))
    val sources = proofKeys.map(key =>
      payload.relationChanges.find(_.key == key).getOrElse(fail(s"missing battery source ${key.stableKey}"))
    )
    assert(sources.forall(_.direction == RelationChangeDirection.Established))
    assertEquals(
      sources.map(_.detail).collect {
        case RelationWitnessDetail.RayBarrier(White, attacker, _, _, _) => attacker.key
      }.toSet,
      Set("a1", "a2")
    )

  test("Ra1-a2 preserves exact file and rank reach changes from every derived result"):
    val record = structuralRecord(
      "4k3/8/8/8/8/P7/8/R3K3 w - - 0 1",
      "a1a2"
    )
    val payload = record.payload.asInstanceOf[StructuralDeltaEvidence]
    val consequence = payload.consequencesOf(TransitionConsequenceKind.SliderReachChanged) match
      case only :: Nil => only
      case found       => fail(s"expected one slider-reach consequence, found ${found.size}")
    val changes = consequence.subjectFacts.collect {
      case change: StructuralSubject.SliderReachChange => change
    }

    assertEquals(consequence.strength, changes.size)
    assertEquals(
      changes.map(_.direction.axis).toSet,
      Set(RelationAxisSignal.File, RelationAxisSignal.Rank)
    )
    val rookBefore = RelationPieceWitness(EvidenceSquare("a1"), EvidencePieceRole("rook"))
    val rookAfter = RelationPieceWitness(EvidenceSquare("a2"), EvidencePieceRole("rook"))
    assert(changes.forall(change => change.sliderBefore.contains(rookBefore) && change.sliderAfter.contains(rookAfter)))

    val down = changes.find(_.direction == RelationRayDirection(0, -1)).getOrElse(fail("expected the gained a1 ray"))
    assertEquals(
      down.gained,
      List(RelationControlReachWitness(EvidenceSquare("a1"), RelationControlTarget.Empty))
    )
    assertEquals(down.lost, Nil)
    val up = changes.find(_.direction == RelationRayDirection(0, 1)).getOrElse(fail("expected the lost a2 ray"))
    assertEquals(up.gained, Nil)
    assertEquals(
      up.lost,
      List(RelationControlReachWitness(EvidenceSquare("a2"), RelationControlTarget.Empty))
    )
    val rank = changes.find(_.direction == RelationRayDirection(1, 0)).getOrElse(fail("expected the rank ray"))
    assert(
      rank.lost.contains(
        RelationControlReachWitness(
          EvidenceSquare("e1"),
          RelationControlTarget.Friendly(EvidencePieceRole("king"))
        )
      )
    )

    val boundKeys = consequence.subjectBindings.flatMap(_.derivedRelationKeys)
    val sliderSourceKeys = payload.derivedRelationSources.map(_.key)
      .filter(_.kind == RelationFactKind.SliderReachDelta)
    assertEquals(boundKeys.toSet, sliderSourceKeys.toSet)
    assertEquals(boundKeys.size, boundKeys.distinct.size)
    assert(payload.consequences.exists(_.kind == TransitionConsequenceKind.GeometricControlSetChanged))
    assert(payload.exactOutputInventoryCertified)

  test("a passed pawn that changes file can lose passed status at its destination"):
    val record = structuralRecord(
      "k7/5p2/4n3/3P4/8/8/8/7K w - - 0 1",
      "d5e6"
    )
    val payload = record.payload.asInstanceOf[StructuralDeltaEvidence]
    val losses = payload.consequencesOf(TransitionConsequenceKind.PassedPawnConcession)

    assertEquals(losses.size, 1)
    assertEquals(
      losses.head.subjectFacts,
      List(StructuralSubject.PassedPawnLost(White, EvidenceSquare("e6")))
    )
    assertEquals(losses.head.subjectBindings.head.relationKeys.size, 2)

  test("canonical consequence proof rejects a relation source owned by the wrong occurrence"):
    val record = structuralRecord("4k3/8/8/3p1p2/8/8/4P3/4K3 w - - 0 1", "e2e4")
    val payload = record.payload.asInstanceOf[StructuralDeltaEvidence]
    val original = payload.relationChanges.headOption.getOrElse(fail("expected canonical relation changes"))
    val wrongPosition = original.direction match
      case RelationChangeDirection.Established => payload.transition.from
      case RelationChangeDirection.Removed     => payload.transition.to
    val wrongSourceNode = original.sourceNode.copy(
      record = original.sourceNode.record.copy(
        ref = original.source.copy(position = wrongPosition)
      )
    )
    val wrongChanges = original.copy(sourceNode = wrongSourceNode) ::
      payload.relationChanges.tail

    assert(
      !TransitionConsequenceRelationProof.provesCanonical(
        payload.consequences,
        wrongChanges,
        payload.transition,
        payload.canonicalTransitionProof
          .map(_.relationDelta)
          .getOrElse(fail("expected canonical transition proof")),
        payload.canonicalDeltaProof
          .map(_.derivedRelations)
          .getOrElse(fail("expected canonical delta proof"))
      )
    )

  private def structuralRecord(beforeFen: String, moveUci: String): EvidenceRecord =
    val legal = PrincipalVariationEvidence
      .legalMoveReplay(beforeFen, List(moveUci), startPly = 0)
      .flatMap(_.headOption)
      .getOrElse(fail(s"expected legal $moveUci"))
    val replayStep = LineReplayStep(
      ply = 1,
      moveUci = legal.uci,
      fenBefore = beforeFen,
      fenAfter = chess.format.Fen.write(legal.after).value
    )
    val replay = CanonicalLineReplay
      .fromLegalReplay(List(legal))
      .getOrElse(fail("expected canonical replay"))
    val from = PositionNodeRef(beforeFen, 0, Some(White))
    val to = PositionNodeRef(replayStep.fenAfter, 1, Some(!White))
    val transitionRef = EvidenceRef(
      s"$moveUci-transition",
      EvidenceProducer.MoveTransitionProducer,
      EvidenceLayer.MoveTransition,
      from,
      None,
      EvidenceScope.PlayedTransition,
      EvidenceConfidence.LegalReplayVerified
    )
    val transition = MoveTransitionEdge(
      TransitionEdgeRole.Played,
      from,
      moveUci,
      to,
      transitionRef
    )
    val replayTransition = replay.onlyTransition.getOrElse(fail("expected one replay transition"))
    val beforeRelations = canonicalRelations(replayTransition.beforeAnalysis, from)
    val afterRelations = canonicalRelations(replayTransition.afterAnalysis, to)
    val delta = StructuralDeltaAnalyzer.bind(
      transition = replayTransition,
      canonicalRelations = CanonicalRelationDelta.bind(
        replayTransition.declared,
        replayTransition.relationDelta,
        beforeRelations,
        afterRelations
      )
    )
    val derivedRelationSources = delta.derivedRelations.zipWithIndex.map { case (relation, index) =>
      StructuralDerivedRelationSource(
        DerivedRelationResultKey.from(relation),
        EvidenceRef(
          id = s"$moveUci-vertical-$index-${relation.semanticId}",
          producer = EvidenceProducer.RelationProducer,
          layer = EvidenceLayer.Relation,
          position = from,
          line = None,
          scope = EvidenceScope.PlayedTransition,
          confidence = EvidenceConfidence.LegalReplayVerified
        )
      )
    }
    TransitionFactNormalizer.fromStructuralDelta(
      id = s"$moveUci-structural",
      delta = delta,
      transition = transition,
      replay = replay,
      line = None,
      perspective = White,
      derivedRelationSources = derivedRelationSources,
      parents = (
        List(
          transitionRef,
          positionFeatureRef(s"$moveUci-before-features", from, EvidenceScope.BeforePosition),
          positionFeatureRef(s"$moveUci-after-features", to, EvidenceScope.AfterPlayedPosition)
        ) ++ delta.canonicalRelations.sourceRefs ++ derivedRelationSources.map(_.source)
      ).distinctBy(_.id)
    )

  private def positionFeatureRef(
      id: String,
      position: PositionNodeRef,
      scope: EvidenceScope
  ): EvidenceRef =
    EvidenceRef(
      id = id,
      producer = EvidenceProducer.PositionFeatureProducer,
      layer = EvidenceLayer.PositionFeature,
      position = position,
      line = None,
      scope = scope,
      confidence = EvidenceConfidence.BoardDerived
    )

  private def canonicalRelations(
      analysis: lila.chessjudgment.analysis.position.PositionAnalysis,
      position: PositionNodeRef
  ): CanonicalPositionRelationSnapshot =
    TypedEvidenceGraph.empty
      .addAll(
        PositionRelationExtractor.records(
          analysis.boardRelations,
          position,
          EvidenceScope.CurrentPosition,
          relation => s"certificate:${position.ply}:${relation.semanticId}"
        )
      )
      .relationGraph
      .closedPositionRelationSnapshot(
        position,
        EvidenceScope.CurrentPosition,
        analysis.relationInventory
      )
