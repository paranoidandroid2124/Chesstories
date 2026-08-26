package lila.chessjudgment.model.judgment

import chess.{ Black, White }

import lila.chessjudgment.analysis.position.PositionRelationExtractor
import lila.chessjudgment.analysis.structure.StructuralDeltaAnalyzer
import lila.chessjudgment.analysis.transition.TransitionFactNormalizer
import lila.chessjudgment.model.line.PrincipalVariationEvidence
import lila.chessjudgment.model.position.PositionFeatures

class StructuralDeltaCertificateTest extends munit.FunSuite:

  test("a structural result kind is the sole owner of observed direction"):
    assertEquals(
      TransitionConsequence(TransitionConsequenceKind.OpenFileEstablished, 1).polarity,
      StructuralSignalPolarity.Neutral
    )
    assertEquals(
      TransitionConsequence(TransitionConsequenceKind.PassedPawnConcession, 1).polarity,
      StructuralSignalPolarity.Loss
    )
    assertEquals(
      TransitionConsequence(TransitionConsequenceKind.PawnTensionResolution, 1).polarity,
      StructuralSignalPolarity.Neutral
    )

  test("a legal transition certificate does not certify copied structural output"):
    val beforeFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    val record = structuralRecord(beforeFen, "e2e4")
    val payload = record.payload.asInstanceOf[StructuralDeltaEvidence]

    assert(payload.exactOutputInventoryCertified)
    assert(payload.relationChanges.nonEmpty)

    val omittedRelationChange = payload.copy(relationChanges = payload.relationChanges.tail)
    assert(!omittedRelationChange.exactOutputInventoryCertified)
    val omittedRecord = record.copy(
      ref = record.ref.copy(id = "omitted-relation-change"),
      payload = omittedRelationChange
    )
    val omittedGraph = graphWithRelationSources(omittedRecord)
    assert(!omittedGraph.proofEligible(omittedRecord))
    assertEquals(EvidenceObjectBinding.fromEvidenceRefs(omittedGraph, List(omittedRecord.ref)), Nil)

    val relationBindings = EvidenceObjectBinding
      .fromEvidenceRefs(graphWithRelationSources(record), List(record.ref))
      .filter(_.mechanism.exists(_.kind == EvidenceObjectKind.Relation))
    assertEquals(relationBindings.size, payload.relationChanges.size)
    assertEquals(
      relationBindings.flatMap(_.provenance.map(_.id)).toSet,
      payload.relationChanges.map(_.source.id).toSet
    )

    val injected = payload.copy(
      consequences = TransitionConsequence(
        TransitionConsequenceKind.BatteryFormation,
        strength = 1,
        subjectBindings = List(
          StructuralSubjectBinding.unbound(
            StructuralSubject.Battery(
              RelationWitnessDetail.RayBarrier(
                White,
                EvidenceSquare("g4"),
                EvidencePieceRole("bishop"),
                List(
                  RelationColoredPieceWitness(
                    EvidenceSquare("h5"),
                    EvidencePieceRole("queen"),
                    White
                  ),
                  RelationColoredPieceWitness(
                    EvidenceSquare("e8"),
                    EvidencePieceRole("king"),
                    Black
                  )
                ),
                RelationAxisSignal.Diagonal
              )
            )
          )
        ),
        targetBindings = List(
          StructuralSubjectBinding.unbound(
            StructuralSubject.PieceAt(EvidencePieceRole("king"), EvidenceSquare("e8"))
          )
        )
      ) :: payload.consequences
    )
    assert(!injected.exactOutputInventoryCertified)

  test("a relation-backed consequence projects only the sources bound to its subjects"):
    val record = structuralRecord("4k3/8/8/3p1p2/8/8/4P3/4K3 w - - 0 1", "e2e4")
    val payload = record.payload.asInstanceOf[StructuralDeltaEvidence]
    val tension = payload.consequences
      .find(_.kind == TransitionConsequenceKind.PawnTensionCreated)
      .getOrElse(fail("expected a two-edge pawn-tension consequence"))
    val sourceByKey = payload.relationChanges.map(change => change.key -> change.source).toMap
    val expectedSources = tension.relationKeys.map(sourceByKey).map(_.id).toSet
    val bindings = EvidenceObjectBinding
      .fromEvidenceRefs(graphWithRelationSources(record), List(record.ref))
      .filter(binding =>
        binding.consequence.nonEmpty &&
          binding.mechanism.exists(_.key.equalsIgnoreCase(TransitionConsequenceKind.PawnTensionCreated.toString))
      )

    assertEquals(expectedSources.size, 2)
    assertEquals(bindings.size, 3)
    val provenanceSets = bindings.map(_.provenance.map(_.id).toSet).toSet
    assertEquals(
      provenanceSets,
      expectedSources.map(Set(_)) + expectedSources
    )
    assertEquals(bindings.flatMap(_.provenance.map(_.id)).toSet, expectedSources)

  test("distant ray churn does not recreate an existing battery formation"):
    val record = structuralRecord(
      "4k3/Q7/8/n7/8/R7/8/R6K w - - 0 1",
      "a7b7"
    )
    val payload = record.payload.asInstanceOf[StructuralDeltaEvidence]
    assert(!payload.signals.exists(_.kind == StructuralSignalKind.BatteryCreated))
    assert(!payload.consequences.exists(_.kind == TransitionConsequenceKind.BatteryFormation))

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
        payload.transition
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
    TransitionFactNormalizer.fromStructuralDelta(
      id = s"$moveUci-structural",
      delta = delta,
      transition = transition,
      replay = replay,
      line = None,
      perspective = White,
      parents = (
        List(
          transitionRef,
          positionFeatureRef(s"$moveUci-before-features", from, EvidenceScope.BeforePosition),
          positionFeatureRef(s"$moveUci-after-features", to, EvidenceScope.AfterPlayedPosition)
        ) ++ delta.canonicalRelations.sourceRefs
      ).distinctBy(_.id)
    )

  private def graphWithRelationSources(record: EvidenceRecord): TypedEvidenceGraph =
    val payload = record.payload.asInstanceOf[StructuralDeltaEvidence]
    val directContext = record.parents.flatMap { parent =>
      parent.producer match
        case EvidenceProducer.MoveTransitionProducer =>
          List(EvidenceRecord(parent, MoveTransitionEvidence(payload.moveUci, payload.from, payload.to)))
        case EvidenceProducer.PositionFeatureProducer =>
          List(
            EvidenceRecord(
              parent,
              PositionFeatureEvidence(
                PositionFeatures(
                  fen = parent.position.fen,
                  sideToMove = parent.position.sideToMove.getOrElse(fail("position parent requires side to move")),
                  plyCount = parent.position.ply
                )
              )
            )
          )
        case _ => Nil
    }
    TypedEvidenceGraph.empty
      .addAll(directContext ++ payload.relationChanges.map(_.sourceNode.record))
      .add(record)

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
