package lila.chessjudgment.analysis.assembly

import chess.White
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.PrincipalVariationEvidence
import lila.chessjudgment.model.strategic.EngineLine

class RelativeCauseRecordConflictTest extends munit.FunSuite:

  test("mate completeness requires engine winner polarity to match the owned beneficiary"):
    val mateFen = "7k/8/5KQ1/8/8/8/8/8 w - - 0 1"
    val matePosition = PositionNodeRef(mateFen, 0, Some(White))
    val mateLine = LineNodeRef("mate-polarity", "g6g7", 1, LineNodeRole.BestReference)
    val mateAfter = PrincipalVariationEvidence
      .legalFenAfter(mateFen, mateLine.rootMove)
      .getOrElse(fail("expected legal mate"))
    val payload = LineFactEvidence(
      line = mateLine,
      replay = List(LineReplayStep(0, mateLine.rootMove, mateFen, mateAfter)),
      events = List(LineMoveEvent(
        kind = LineEventKind.Mate,
        moveUci = mateLine.rootMove,
        plyOffset = 0,
        side = Some(White),
        pieceRole = Some(EvidencePieceRole("queen")),
        targetRole = Some(EvidencePieceRole("king")),
        square = Some(EvidenceSquare("h8"))
      )),
      consequences = List(LineConsequence(
        kind = LineConsequenceKind.Mate,
        lineMoves = List(mateLine.rootMove),
        proofSignal = true,
        eventMove = Some(mateLine.rootMove),
        rootMove = Some(mateLine.rootMove),
        rootSide = Some(White),
        beneficiary = Some(White)
      ))
    )
    val source = EvidenceRef(
      "mate-polarity-line",
      EvidenceProducer.LegalLineProducer,
      EvidenceLayer.Line,
      matePosition,
      Some(mateLine),
      EvidenceScope.BestLine,
      EvidenceConfidence.LegalReplayVerified
    )
    val inventories = EvidenceObjectBinding.comparisonEndpointLineObservations(
      source,
      payload,
      matePosition
    )

    assertEquals(inventories.qualitative, ComparisonEndpointEffectInventory.Complete(Set.empty))

    val correct = RelativeAssessmentAssembler.enforceLineFamilyCompleteness(
      inventories,
      payload,
      Some(EngineLine(List(mateLine.rootMove), 900, mate = Some(1), depth = 20))
    )
    val reversed = RelativeAssessmentAssembler.enforceLineFamilyCompleteness(
      inventories,
      payload,
      Some(EngineLine(List(mateLine.rootMove), 900, mate = Some(-1), depth = 20))
    )

    assert(correct.mate.isInstanceOf[ComparisonEndpointEffectInventory.Complete])
    assertEquals(reversed.mate, ComparisonEndpointEffectInventory.Incomplete)
