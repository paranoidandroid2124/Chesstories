package lila.chessjudgment.model.judgment

import chess.White
import chess.variant.Standard

import lila.chessjudgment.model.position.PositionOccurrenceState

class ConversionContextPolicyTest extends munit.FunSuite:

  test("position occurrence metadata alone is not a conversion mechanism"):
    val occurrence = PositionOccurrenceState(Standard.initialFen.value, White, 0)
    val position = PositionNodeRef(occurrence.fen, 0, Some(White))
    val record = EvidenceRecord(
      EvidenceRef(
        "low-material",
        EvidenceProducer.PositionOccurrenceProducer,
        EvidenceLayer.PositionOccurrence,
        position,
        None,
        EvidenceScope.CurrentPosition,
        EvidenceConfidence.BoardDerived
      ),
      PositionOccurrenceEvidence(occurrence)
    )

    assert(!ConversionContextPolicy.supports(List(record), RelativeCauseKind.ConversionSecured))
