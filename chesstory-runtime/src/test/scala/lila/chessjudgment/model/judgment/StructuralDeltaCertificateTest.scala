package lila.chessjudgment.model.judgment

import chess.{ Black, White }

import lila.chessjudgment.analysis.position.PositionRelationExtractor
import lila.chessjudgment.analysis.structure.{ StructuralDeltaAnalyzer, StructuralDeltaContracts }
import lila.chessjudgment.analysis.transition.TransitionFactNormalizer
import lila.chessjudgment.model.line.PrincipalVariationEvidence

class StructuralDeltaCertificateTest extends munit.FunSuite:

  test("a legal transition certificate does not certify copied structural output"):
    val beforeFen = "4k3/8/8/3p1p2/8/8/4P3/4K3 w - - 0 1"
    val record = structuralRecord(beforeFen, "e2e4")
    val payload = record.payload.asInstanceOf[StructuralDeltaEvidence]

    assert(payload.canonicalOutputShapeCertified)
    assert(payload.relationChanges.nonEmpty)

    val omittedRelationChange = payload.copy(relationChanges = payload.relationChanges.tail)
    assert(!omittedRelationChange.canonicalOutputShapeCertified)

    val injected = payload.copy(
      consequences = TransitionConsequence(
        TransitionConsequenceKind.PawnTensionCreated,
        subjectBindings = List(
          StructuralSubjectBinding(
            StructuralSubject.PawnTensionCreated(White, EvidenceSquare("e4"), EvidenceSquare("d5")),
            Nil
          )
        )
      ) :: payload.consequences
    )
    assert(!injected.canonicalOutputShapeCertified)

  test("slider reach remains an exact L1 result without a duplicate structural consequence"):
    val beforeFen = "4k3/8/8/8/8/P7/8/R3K3 w - - 0 1"
    val legal = PrincipalVariationEvidence
      .legalMoveReplay(beforeFen, List("a1a2"), startPly = 0)
      .flatMap(_.headOption)
      .getOrElse(fail("expected legal a1a2"))
    val replay = CanonicalLineReplay
      .fromLegalReplay(List(legal))
      .getOrElse(fail("expected canonical replay"))
    val transition = replay.onlyTransition.getOrElse(fail("expected one replay transition"))
    val reaches = transition.verticalRelationsFor(VerticalRelationContractKind.SliderReachDelta)

    assert(reaches.nonEmpty)
    assert(reaches.forall(_.kind == RelationFactKind.SliderReachDelta))
    assertEquals(StructuralDeltaContracts.consequences(transition.structuralDelta), Nil)

  test("a passed pawn that changes file can lose passed status at its destination"):
    val record = structuralRecord(
      "k7/5p2/4n3/3P4/8/8/8/7K w - - 0 1",
      "d5e6"
    )
    val payload = record.payload.asInstanceOf[StructuralDeltaEvidence]
    val losses = payload.consequences.filter(_.kind == TransitionConsequenceKind.PassedPawnStatusRemoved)

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
    val otherOccurrence = structuralRecord(
      "4k3/8/8/3p1p2/8/8/4P3/4K3 w - - 37 92",
      "e2e4"
    ).payload.asInstanceOf[StructuralDeltaEvidence]
    val wrongOccurrence = otherOccurrence.relationChanges
      .find(_.key == original.key)
      .getOrElse(fail("expected the same semantic change in another occurrence"))
    val wrongChanges = wrongOccurrence ::
      payload.relationChanges.tail

    assert(
      !TransitionConsequenceBindingProof.provesCanonical(
        payload.consequences,
        wrongChanges,
        payload.transition,
        payload.canonicalDeltaProof
          .map(_.canonicalRelations)
          .getOrElse(fail("expected canonical occurrence proof"))
      )
    )

  private def structuralRecord(beforeFen: String, moveUci: String): EvidenceRecord =
    val legal = PrincipalVariationEvidence
      .legalMoveReplay(beforeFen, List(moveUci), startPly = 0)
      .flatMap(_.headOption)
      .getOrElse(fail(s"expected legal $moveUci"))
    val replay = CanonicalLineReplay
      .fromLegalReplay(List(legal))
      .getOrElse(fail("expected canonical replay"))
    val replayTransition = replay.onlyTransition.getOrElse(fail("expected one replay transition"))
    val from = PositionNodeRef(replayTransition.declared.fenBefore, 0, Some(White))
    val to = PositionNodeRef(replayTransition.declared.fenAfter, 1, Some(!White))
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
    val resultPremiseSources = replayTransition.structuralOccurrence.consequences
      .flatMap(_.resultPremiseKeys)
      .distinct
      .sortBy(_.stableKey)
      .zipWithIndex
      .map { case (key, index) =>
        TransitionResultPremiseSource(
          key,
          EvidenceRef(
            id = s"$moveUci-result-$index-${key.semanticId}",
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
      resultPremiseSources = resultPremiseSources,
      parents = (
        List(
          transitionRef,
          positionOccurrenceRef(s"$moveUci-before-occurrence", from, EvidenceScope.BeforePosition),
          positionOccurrenceRef(s"$moveUci-after-occurrence", to, EvidenceScope.AfterPlayedPosition)
        ) ++ delta.canonicalRelations.changes.map(_.source) ++ resultPremiseSources.map(_.source)
      ).distinctBy(_.id)
    )

  private def positionOccurrenceRef(
      id: String,
      position: PositionNodeRef,
      scope: EvidenceScope
  ): EvidenceRef =
    EvidenceRef(
      id = id,
      producer = EvidenceProducer.PositionOccurrenceProducer,
      layer = EvidenceLayer.PositionOccurrence,
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
