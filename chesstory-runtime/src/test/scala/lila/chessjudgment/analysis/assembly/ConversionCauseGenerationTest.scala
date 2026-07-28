package lila.chessjudgment.analysis.assembly

import chess.{ Black, White }
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.PrincipalVariationEvidence
import lila.chessjudgment.model.strategic.EngineLine

class ConversionCauseGenerationTest extends munit.FunSuite:

  test("reference conversion and promotion resources produce resource-owned ConversionSecured drafts"):
    val promotionFen = "4k3/P7/8/8/8/8/8/4K3 w - - 0 1"
    val position = PositionNodeRef(promotionFen, 0, Some(White))
    val reference = LineNodeRef("promotion-reference", "a7a8q", 1, LineNodeRole.BestReference)
    val candidate = LineNodeRef("promotion-played", "e1f1", 2, LineNodeRole.Played)
    val promotion = lineRecord(
      "reference-promotion",
      position,
      reference,
      legalReplay(promotionFen, List(reference.rootMove)),
      consequences = List(LineConsequence(
        kind = LineConsequenceKind.Promotion,
        lineMoves = List(reference.rootMove),
        proofSignal = true,
        eventMove = Some(reference.rootMove),
        rootMove = Some(reference.rootMove),
        rootSide = Some(White),
        beneficiary = Some(White)
      ))
    )
    val fact = comparison(position, reference, candidate)
    val profile = RelativeCauseSignalProfile.from(
      fact = fact,
      referenceRecords = List(promotion),
      candidateRecords = Nil,
      sharedRecords = Nil
    )
    val comparisonRecord =
      ExplicitCauseAdmissionTestSupport.comparisonRecord("promotion-comparison", position, fact)
    val drafts = RelativeCauseDraftPlanner.drafts(profile, comparisonRecord)
    val referenceConversions = drafts.filter(draft =>
      draft.kind == RelativeCauseKind.ConversionSecured &&
        draft.sourceSide.contains(RelativeCauseSourceSide.Reference)
    )

    assert(referenceConversions.exists(draft =>
      draft.attributionKind == CauseAttributionKind.ReferenceCreatesResource &&
        draft.support.map(_.ref.id) == List(promotion.ref.id)
    ))
    assert(!drafts.exists(draft =>
      draft.kind == RelativeCauseKind.ConversionMiss &&
        draft.sourceSide.contains(RelativeCauseSourceSide.Reference)
    ))

  test("completed winning conversion requires an exact root, actor side, replay span, and concrete squares"):
    val fen = "8/1p1k4/1P6/2PK4/8/8/8/8 w - - 0 1"
    val position = PositionNodeRef(fen, 0, Some(White))
    val line = LineNodeRef("completed-conversion", "d5d4", 1, LineNodeRole.BestReference)
    val replay = legalReplay(fen, List("d5d4", "d7d8", "d4e5", "d8d7", "e5d5"))
    val completed = LineEndgameTechniqueHorizon(
      pattern = "Lucena",
      rookPattern = Some("Lucena"),
      techniqueSide = White,
      entryPlyOffset = 0,
      terminalPlyOffset = 4,
      status = LineEndgameTechniqueHorizonStatus.Completed,
      triggerMove = Some(line.rootMove),
      requiredSquares = List("d4", "d5", "e5")
    )
    val exact = completedRecord(
      "exact-completed-winning",
      position,
      line,
      replay,
      completed
    )
    val wrongSide = completedRecord("wrong-side", position, line, replay, completed.copy(techniqueSide = Black))
    val noSquares = completedRecord("no-required-squares", position, line, replay, completed.copy(requiredSquares = Nil))
    val wrongRoot = completedRecord("wrong-root", position, line, replay, completed.copy(triggerMove = Some("d5e5")))
    val incompleteReplay = completedRecord("incomplete-replay", position, line, replay.take(1), completed)
    val selected = RelativeCauseSignalProfile
      .conversionWindowRecords(
        List(exact, wrongSide, noSquares, wrongRoot, incompleteReplay),
        line,
        White
      )
      .map(_.ref.id)
      .toSet

    assertEquals(selected, Set(exact.ref.id))

  test("completed triangulation requires its verified zugzwang proof"):
    val fen = "8/1p1k4/1P6/2PK4/8/8/8/8 w - - 0 1"
    val position = PositionNodeRef(fen, 0, Some(White))
    val line = LineNodeRef("triangulation", "d5d4", 1, LineNodeRole.BestReference)
    val replay = legalReplay(fen, List("d5d4", "d7d8", "d4e5", "d8d7", "e5d5"))
    val horizon = LineEndgameTechniqueHorizon(
      pattern = "Triangulation",
      rookPattern = None,
      techniqueSide = White,
      entryPlyOffset = 0,
      terminalPlyOffset = 4,
      status = LineEndgameTechniqueHorizonStatus.Completed,
      triggerMove = Some(line.rootMove),
      requiredSquares = List("d4", "d5", "e5")
    )
    val proof = EndgameZugzwangProof
      .verified(
        constrainedSide = Black,
        terminalFen = replay.last.fenAfter,
        comparisonFen = replay.head.fenBefore,
        terminalDtm = -22,
        comparisonDtm = 27,
        legalReplies = List(
          EndgameZugzwangReplyProof("d7e7", 21),
          EndgameZugzwangReplyProof("d7e8", 19),
          EndgameZugzwangReplyProof("d7c8", 17),
          EndgameZugzwangReplyProof("d7d8", 15)
        ),
        horizon = horizon,
        replay = replay
      )
      .getOrElse(fail("expected a verified triangulation proof"))
    val unverified = completedRecord("unverified-triangulation", position, line, replay, horizon)
    val verified = lineRecord(
      "verified-triangulation",
      position,
      line,
      replay,
      horizons = List(horizon)
    ).copy(
      payload = LineFactEvidence(line = line, replay = replay, endgameHorizons = List(horizon))
        .withEndgameZugzwangProof(0, 4, proof)
    )

    assertEquals(
      RelativeCauseSignalProfile
        .conversionWindowRecords(List(unverified, verified), line, White)
        .map(_.ref.id),
      List(verified.ref.id)
    )

  private def completedRecord(
      id: String,
      position: PositionNodeRef,
      line: LineNodeRef,
      replay: List[LineReplayStep],
      horizon: LineEndgameTechniqueHorizon
  ): EvidenceRecord =
    lineRecord(id, position, line, replay, horizons = List(horizon))

  private def lineRecord(
      id: String,
      position: PositionNodeRef,
      line: LineNodeRef,
      replay: List[LineReplayStep],
      consequences: List[LineConsequence] = Nil,
      horizons: List[LineEndgameTechniqueHorizon] = Nil
  ): EvidenceRecord =
    EvidenceRecord(
      evidenceRef(id, EvidenceProducer.LegalLineProducer, EvidenceLayer.Line, position, line),
      LineFactEvidence(
        line = line,
        replay = replay,
        consequences = consequences,
        endgameHorizons = horizons
      )
    )

  private def comparison(
      position: PositionNodeRef,
      reference: LineNodeRef,
      candidate: LineNodeRef
  ): CandidateComparisonFact =
    CandidateComparisonFact(
      CandidateComparisonKind.PlayedVsBest,
      reference,
      candidate,
      EvalComparison.fromLines(
        White,
        CandidateLineNode(
          reference,
          EngineLine(List(reference.rootMove), scoreCp = 900, depth = 20),
          evidenceRef("reference-eval", EvidenceProducer.EngineEvalProducer, EvidenceLayer.Eval, position, reference)
        ),
        CandidateLineNode(
          candidate,
          EngineLine(List(candidate.rootMove), scoreCp = 0, depth = 20),
          evidenceRef("candidate-eval", EvidenceProducer.EngineEvalProducer, EvidenceLayer.Eval, position, candidate)
        )
      )
    )

  private def legalReplay(fen: String, moves: List[String]): List[LineReplayStep] =
    moves.zipWithIndex.foldLeft(fen -> List.empty[LineReplayStep]) {
      case ((fenBefore, steps), (move, index)) =>
        val fenAfter = PrincipalVariationEvidence
          .legalFenAfter(fenBefore, move)
          .getOrElse(fail(s"expected legal test move $move after $fenBefore"))
        fenAfter -> (steps :+ LineReplayStep(index, move, fenBefore, fenAfter))
    }._2

  private def evidenceRef(
      id: String,
      producer: EvidenceProducer,
      layer: EvidenceLayer,
      position: PositionNodeRef,
      line: LineNodeRef
  ): EvidenceRef =
    EvidenceRef(
      id = id,
      producer = producer,
      layer = layer,
      position = position,
      line = Some(line),
      scope = line.role.scope,
      confidence =
        if layer == EvidenceLayer.Eval then EvidenceConfidence.EngineBacked
        else EvidenceConfidence.LegalReplayVerified
    )
