package lila.chessjudgment.model.judgment

import chess.White
import chess.variant.Standard
import lila.chessjudgment.analysis.position.PositionAnalyzer
import lila.chessjudgment.model.line.PrincipalVariationEvidence
import lila.chessjudgment.model.position.PositionOccurrenceState

class TypedEvidenceGraphBatchTest extends munit.FunSuite:

  private val initialOccurrence = PositionOccurrenceState(Standard.initialFen.value, White, 0)
  private val position = PositionNodeRef(initialOccurrence.fen, 0, Some(White))
  private val relationPayload = PositionAnalyzer
    .analyze(
      PrincipalVariationEvidence.readPosition(position.fen).get,
      position.fen,
      position.ply
    )
    .boardRelations
    .head

  private def occurrenceRecord(id: String, parents: List[EvidenceRef] = Nil): EvidenceRecord =
    EvidenceRecord(
      EvidenceRef(
        id = id,
        producer = EvidenceProducer.PositionOccurrenceProducer,
        layer = EvidenceLayer.PositionOccurrence,
        position = position,
        line = None,
        scope = EvidenceScope.CurrentPosition,
        confidence = EvidenceConfidence.BoardDerived
      ),
      PositionOccurrenceEvidence(initialOccurrence),
      parents
    )

  private def relationRecord(id: String): EvidenceRecord =
    RelationFactEvidence.record(
      id = id,
      payload = relationPayload,
      position = position,
      line = None,
      scope = EvidenceScope.CurrentPosition,
      confidence = EvidenceConfidence.BoardDerived
    )

  test("batch insertion preserves order and ignores an identical repeated record"):
    val first = occurrenceRecord("first")
    val second = occurrenceRecord("second")
    val graph = TypedEvidenceGraph.empty.addAll(List(first, second, first))

    assertEquals(graph.records, List(first, second))
    assert(graph.addAll(List(first)) eq graph)

  test("batch insertion rejects a conflicting evidence id"):
    val first = occurrenceRecord("same-id")
    val conflicting = occurrenceRecord("same-id", List(first.ref))

    intercept[IllegalArgumentException] {
      TypedEvidenceGraph.empty.addAll(List(first, conflicting))
    }

  test("batch insertion reuses or updates the canonical relation graph exactly when needed"):
    val empty = TypedEvidenceGraph.empty
    val occurrenceOnly = empty.add(occurrenceRecord("occurrence"))
    assert(occurrenceOnly.relationGraph eq empty.relationGraph)

    val relation = relationRecord("relation")
    val withRelation = occurrenceOnly.add(relation)
    assert(!(withRelation.relationGraph eq occurrenceOnly.relationGraph))
    assertEquals(withRelation.relationGraph.nodes.map(_.record), List(relation))

    val unchangedRelations = withRelation.addAll(List(relation, occurrenceRecord("another-occurrence")))
    assert(unchangedRelations.relationGraph eq withRelation.relationGraph)

  test("incremental relation batches equal one cold build without revisiting existing nodes"):
    val firstPosition = position.copy(id = Some("first-position"))
    val secondPosition = position.copy(id = Some("second-position"), ply = 1)
    def at(record: EvidenceRecord, target: PositionNodeRef, id: String): EvidenceRecord =
      val relation = record.payload.asInstanceOf[RelationFactEvidence]
      RelationFactEvidence.record(
        id = id,
        payload = relation,
        position = target,
        line = None,
        scope = EvidenceScope.CurrentPosition,
        confidence = EvidenceConfidence.BoardDerived
      )

    val first = at(relationRecord("seed"), firstPosition, "first-relation")
    val second = at(relationRecord("seed-2"), secondPosition, "second-relation")
    val afterFirst = TypedEvidenceGraph.empty.add(first)
    val firstNode = afterFirst.relationGraph.byEvidenceId(first.ref.id)
    val incremental = afterFirst.add(second)
    val cold = TypedEvidenceGraph.empty.addAll(List(first, second)).relationGraph

    assertEquals(incremental.relationGraph.nodes, cold.nodes)
    assert(
      incremental.relationGraph.byEvidenceId(first.ref.id).asInstanceOf[AnyRef] eq
        firstNode.asInstanceOf[AnyRef]
    )

  test("proof authority rejects a payload under the wrong producer"):
    val wrongProducer = occurrenceRecord("wrong-producer").copy(
      ref = occurrenceRecord("wrong-producer").ref.copy(producer = EvidenceProducer.MoveTransitionProducer)
    )
    val graph = TypedEvidenceGraph.empty.add(wrongProducer)

    assert(!graph.proofEligible(wrongProducer))
