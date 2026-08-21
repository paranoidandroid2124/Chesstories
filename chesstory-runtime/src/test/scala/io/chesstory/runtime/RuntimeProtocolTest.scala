package io.chesstory.runtime

import java.net.{ InetSocketAddress, URI }
import java.net.http.{ HttpClient, HttpRequest, HttpResponse }
import java.nio.charset.StandardCharsets

import chess.opening.OpeningDb
import lila.chessjudgment.analysis.assembly.MoveReviewInputNormalizer
import lila.chessjudgment.model.evaluation.PerspectiveMath
import lila.chessjudgment.analysis.opening.OpeningRecognitionIndex
import lila.chessjudgment.model.{ ProbePurpose, ProbeRequest, ProbeResolution }
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.{
  AutomaticTerminal,
  CanonicalPositionHistory,
  CanonicalPositionHistoryFailure,
  DrawClaimAvailability,
  DrawClaimRule,
  DeclaredDrawClaim,
  FivefoldRepetitionKnowledge,
  PositionRuleAssessment,
  CandidateLineEvaluation,
  ThreefoldClaimKnowledge
}
import play.api.libs.json.*
import lila.chessjudgment.model.strategic.EngineLine

class RuntimeProtocolTest extends munit.FunSuite:

  private val engineProfile = CommentaryEngineProfile.ServerAdmitted

  private def commentaryPolicy(deadlineEpochMs: Long = Long.MaxValue): CommentaryJobPolicy =
    CommentaryJobPolicy(16, 512, deadlineEpochMs, engineProfile)

  private def canonicalHistory(
      initialFen: String,
      moves: List[String] = Nil
  ): CanonicalPositionHistory =
    CanonicalPositionHistory
      .from(initialFen, Nil, initialFen)
      .toOption
      .get
      .extend(moves)
      .toOption
      .get

  private def rootInventory(
      history: CanonicalPositionHistory,
      terminalMove: String,
      terminalLine: EngineLine
  ) =
    val orderedLegalMoves = history.currentPosition.legalMoves.map(_.toUci.uci)
    val evaluations = orderedLegalMoves.map { move =>
      val line =
        if move == terminalMove then terminalLine
        else EngineLine(List(move), 0, None, 16)
      CandidateLineEvaluation.EngineSearch(line)
    }
    MoveReviewInputNormalizer.prepareMoveReviewInventory(
      history,
      orderedLegalMoves,
      PositionRuleAssessment.assess(history),
      evaluations,
      Set.empty
    )

  private val input = Json.obj(
    "fen" -> chess.variant.Standard.initialFen.value,
    "playedMoveUci" -> "d2d4",
    "variations" -> Json.arr(
      Json.obj("moves" -> Json.arr("e2e4", "e7e5", "g1f3"), "scoreCp" -> 30, "depth" -> 16),
      Json.obj("moves" -> Json.arr("d2d4", "d7d5", "g1f3"), "scoreCp" -> 20, "depth" -> 16)
    ),
    "ply" -> 0,
    "movePrefixUci" -> Json.arr()
  )

  private def request(schema: String = RuntimeProtocol.RequestSchema, body: JsObject = input): JsObject =
    Json.obj("schema_version" -> schema, "request_id" -> "test-1", "input" -> body)

  test("opening recognition reuses the canonical scalachess database"):
    val canonical = OpeningDb.all.head
    val moves = canonical.uci.value.split("\\s+").toList
    val recognized = OpeningRecognitionIndex.default.recognize(moves, canonical.fen.value, moves.size)
    assert(recognized.exists(_.candidates.exists(_.identity.name.contains(canonical.name.value))))

  test("a unique canonical position preserves opening identity across move-order transposition"):
    val canonical = OpeningDb.all.find: opening =>
      opening.eco.value == "E08" &&
        opening.name.value == "Catalan Opening: Closed" &&
        opening.uci.value.endsWith("d1c2")
    val alternateMoves =
      "d2d4 g8f6 c2c4 e7e6 g1f3 d7d5 g2g3 f8e7 f1g2 e8g8 e1g1 b8d7 d1c2".split(" ").toList
    val recognized = canonical.flatMap: opening =>
      OpeningRecognitionIndex.default.recognize(alternateMoves, opening.fen.value, alternateMoves.size)
    assertEquals(recognized.map(_.matchedBy), Some(OpeningRecognitionMatchKind.PositionTransposition))
    assertEquals(recognized.flatMap(_.bestIdentity.flatMap(_.name)), Some("Catalan Opening: Closed"))

  test("canonical position history allows the terminal-reaching move and rejects every continuation"):
    val initialFen =
      "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 149 501"
    val currentFen =
      "rnbqkbnr/pppppppp/8/8/8/5N2/PPPPPPPP/RNBQKB1R b KQkq - 150 501"
    val history = CanonicalPositionHistory.from(initialFen, List("g1f3"), currentFen).toOption.get
    assertEquals(history.currentFen, currentFen)
    assertEquals(
      PositionRuleAssessment.assess(history),
      PositionRuleAssessment.Terminal(AutomaticTerminal.SeventyFiveMoveRule)
    )
    assertEquals(history.extend(Nil).map(_.currentFen), Right(currentFen))
    assertEquals(
      history.extend(List("g8f6")),
      Left(CanonicalPositionHistoryFailure.IllegalMovePrefix)
    )
    assertEquals(
      CanonicalPositionHistory.from(
        initialFen,
        List("g1f3", "g8f6"),
        "rnbqkb1r/pppppppp/5n2/8/8/5N2/PPPPPPPP/RNBQKB1R w KQkq - 151 502"
      ),
      Left(CanonicalPositionHistoryFailure.IllegalMovePrefix)
    )

  test("canonical position history rejects illegal prefixes and current-FEN mismatch"):
    val initialFen = chess.variant.Standard.initialFen.value
    assertEquals(
      CanonicalPositionHistory.from(initialFen, List("e2e5"), initialFen),
      Left(CanonicalPositionHistoryFailure.IllegalMovePrefix)
    )
    assertEquals(
      CanonicalPositionHistory.from(initialFen, List("e2e4"), initialFen),
      Left(CanonicalPositionHistoryFailure.CurrentFenMismatch)
    )

  test("engine admission projects legal terminal tails into engine-backed logical prefixes"):
    val drawFen = "7k/8/8/8/K7/1p6/8/8 w - - 0 1"
    val drawWork = IssuedEngineWork(
      engineProfile,
      "work:draw",
      drawFen,
      Nil,
      drawFen,
      drawFen,
      Nil,
      requiredDepth = 16,
      multiPv = 1
    )
    val legalTail = List("a4b3", "h8h7", "b3c3")
    List(-24 -> 16, 9 -> 17).foreach { case (score, depth) =>
      EngineLineAdmission.bindReportedLineSuffixes(
        drawWork,
        List(ReportedEngineLineSuffix(legalTail, ReportedWhiteEngineScore.Centipawns(score), depth))
      ) match
        case Some(List(CandidateLineEvaluation.EngineSearch(EngineLine(moves, admittedScore, None, admittedDepth)))) =>
          assertEquals(moves, List("a4b3"))
          assertEquals(admittedScore, score)
          assertEquals(admittedDepth, depth)
        case other => fail(s"expected engine-backed projected tail for cp $score, found $other")
    }
    assertEquals(
      EngineLineAdmission.bindReportedLineSuffixes(
        drawWork,
        List(
          ReportedEngineLineSuffix(
            List("a4b3", "h8h7", "b3b5"),
            ReportedWhiteEngineScore.Centipawns(0),
            16
          )
        )
      ),
      None
    )
    List(
      ReportedWhiteEngineScore.Mate(0),
      ReportedWhiteEngineScore.Mate(1001),
      ReportedWhiteEngineScore.Mate(-1001),
      ReportedWhiteEngineScore.Mate(Int.MinValue),
      ReportedWhiteEngineScore.Centipawns(100001),
      ReportedWhiteEngineScore.Centipawns(-100001),
      ReportedWhiteEngineScore.Centipawns(Int.MinValue)
    ).foreach { score =>
      assertEquals(
        EngineLineAdmission.bindReportedLineSuffixes(
          drawWork,
          List(ReportedEngineLineSuffix(legalTail, score, 16))
        ),
        None
      )
    }

  test("reported mate observations remain engine-backed with normalized distances and defense ordering"):
    val initialFen = chess.variant.Standard.initialFen.value
    val prefix = List("e2e4", "e7e5")
    val searchFen = canonicalHistory(initialFen, prefix).currentFen
    val mateWork = IssuedEngineWork(
      engineProfile,
      "work:mate",
      initialFen,
      prefix,
      searchFen,
      initialFen,
      prefix,
      requiredDepth = 16,
      multiPv = 2
    )
    val admitted = EngineLineAdmission.bindReportedLineSuffixes(
      mateWork,
      List(
        ReportedEngineLineSuffix(List("g1f3"), ReportedWhiteEngineScore.Mate(-1), 16),
        ReportedEngineLineSuffix(List("b1c3"), ReportedWhiteEngineScore.Mate(-5), 17)
      )
    )
    admitted match
      case Some(List(CandidateLineEvaluation.EngineSearch(shorter), CandidateLineEvaluation.EngineSearch(longer))) =>
        assertEquals(shorter.moves, prefix ++ List("g1f3"))
        assertEquals(longer.moves, prefix ++ List("b1c3"))
        assertEquals(shorter.scoreCp, -10000)
        assertEquals(longer.scoreCp, -10000)
        assertEquals(shorter.mate, Some(-2))
        assertEquals(longer.mate, Some(-6))
        assertEquals(shorter.depth, 16)
        assertEquals(longer.depth, 17)
        assert(
          CandidateLineEvaluation.EngineSearch(longer).rankingScoreForMover(chess.Color.White) >
            CandidateLineEvaluation.EngineSearch(shorter).rankingScoreForMover(chess.Color.White)
        )
        val comparison = PerspectiveMath.compareForMover(
          chess.Color.White,
          PerspectiveMath.EvalPoint(longer.scoreCp, longer.mate),
          PerspectiveMath.EvalPoint(shorter.scoreCp, shorter.mate)
        )
        assertEquals(comparison.winPercentLossForMover, 0.0)
        assertEquals(comparison.mateDistanceLossForMover, Some(4))
      case other => fail(s"expected normalized engine mate observations, found $other")

  test("physical MultiPV preserves ordered engine-backed terminal-prefix candidates"):
    val initialFen = "7k/8/8/8/K7/1p6/8/8 b - - 0 1"
    val work = IssuedEngineWork(
      engineProfile,
      "work:terminal-prefixes",
      initialFen,
      Nil,
      initialFen,
      initialFen,
      Nil,
      requiredDepth = 16,
      multiPv = 2
    )
    val mixedReports = List(
      ReportedEngineLineSuffix(
        List("h8h7", "a4b3", "h7g6"),
        ReportedWhiteEngineScore.Centipawns(-24),
        17
      ),
      ReportedEngineLineSuffix(
        List("h8g8", "a4a5"),
        ReportedWhiteEngineScore.Centipawns(9),
        16
      )
    )
    val mixed = EngineLineAdmission
      .bindReportedLineSuffixes(work, mixedReports)
      .getOrElse(fail("expected mixed physical candidates"))
    assertEquals(mixed.size, 2)
    assertEquals(mixed.map(_.moves), List(List("h8h7", "a4b3"), List("h8g8", "a4a5")))
    assert(mixed.forall(_.isInstanceOf[CandidateLineEvaluation.EngineSearch]))
    assertEquals(mixed.flatMap(_.engineLine.map(_.depth)), List(17, 16))
    val mixedAggregate: ProbeResolution.EngineSearch = ProbeResolution.EngineSearch(
      mixed,
      mixedReports.map(_.depth).min
    )
    assertEquals(mixedAggregate.depth, 16)
    assertEquals(mixedAggregate.evaluations, mixed)

    val allTerminalReports = List(
      ReportedEngineLineSuffix(
        List("h8h7", "a4b3", "h7g6"),
        ReportedWhiteEngineScore.Centipawns(-24),
        17
      ),
      ReportedEngineLineSuffix(
        List("h8g8", "a4b3", "g8f7"),
        ReportedWhiteEngineScore.Centipawns(9),
        16
      )
    )
    val allTerminal = EngineLineAdmission
      .bindReportedLineSuffixes(work, allTerminalReports)
      .getOrElse(fail("expected all terminal-prefix candidates"))
    assertEquals(allTerminal.size, 2)
    assertEquals(allTerminal.map(_.moves), List(List("h8h7", "a4b3"), List("h8g8", "a4b3")))
    assert(allTerminal.forall(_.isInstanceOf[CandidateLineEvaluation.EngineSearch]))
    assertEquals(allTerminal.flatMap(_.engineLine.map(_.depth)), List(17, 16))
    val noWork = ProbeResolution.ExactAutomaticTerminal(
      CandidateLineEvaluation.ExactAutomaticTerminal(List("a4b3"), AutomaticTerminal.InsufficientMaterial)
    )
    assert(noWork.isInstanceOf[ProbeResolution.ExactAutomaticTerminal])

  test("root normalizer accepts bounded engine observations for overrun-capable terminals"):
    val insufficient = canonicalHistory("7k/8/8/8/K7/1p6/8/8 w - - 0 1")
    assert(rootInventory(insufficient, "a4b3", EngineLine(List("a4b3"), -24, None, 16)).nonEmpty)

    val fivefoldPrefix = List.fill(3)(List("g1f3", "g8f6", "f3g1", "f6g8")).flatten ++
      List("g1f3", "g8f6", "f3g1")
    val fivefold = canonicalHistory(chess.variant.Standard.initialFen.value, fivefoldPrefix)
    assertEquals(
      PositionRuleAssessment.assess(fivefold),
      PositionRuleAssessment.Nonterminal(
        FivefoldRepetitionKnowledge.CertifiedNonterminal,
        ThreefoldClaimKnowledge.Known(DrawClaimAvailability.AvailableNow),
        DrawClaimAvailability.Unavailable
      )
    )
    assert(rootInventory(fivefold, "f6g8", EngineLine(List("f6g8"), 9, None, 16)).nonEmpty)

    val seventyFive = canonicalHistory(
      "4k3/8/8/8/8/8/8/4K2R w K - 148 1",
      List("h1h2")
    )
    assert(rootInventory(seventyFive, "e8d7", EngineLine(List("e8d7"), 9, None, 16)).nonEmpty)

    assertEquals(
      rootInventory(insufficient, "a4b3", EngineLine(List("a4b3", "h8h7"), 9, None, 16)),
      None
    )
    val historyUnavailable = canonicalHistory(
      "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 16 1"
    )
    val unavailableMove = historyUnavailable.currentPosition.legalMoves.head.toUci.uci
    assertEquals(
      rootInventory(historyUnavailable, unavailableMove, EngineLine(List(unavailableMove), 0, None, 16)),
      None
    )

    val checkmate = canonicalHistory("8/8/8/8/8/8/8/k1KQ4 w - - 0 17")
    assertEquals(
      rootInventory(checkmate, "d1a4", EngineLine(List("d1a4"), -10000, Some(-1), 16)),
      None
    )
    val stalemate = canonicalHistory("7k/8/4Q1K1/8/8/8/8/8 w - - 0 1")
    assertEquals(
      rootInventory(stalemate, "e6f7", EngineLine(List("e6f7"), 9, None, 16)),
      None
    )

  test("nonterminal reported work remains engine-backed"):
    val initialFen = chess.variant.Standard.initialFen.value
    val searchFen = canonicalHistory(initialFen, List("e2e4")).currentFen
    val nonterminalWork = IssuedEngineWork(
      engineProfile,
      "work:nonterminal",
      initialFen,
      List("e2e4"),
      searchFen,
      initialFen,
      List("e2e4"),
      requiredDepth = 16,
      multiPv = 1
    )
    assertEquals(
      EngineLineAdmission.bindReportedLineSuffixes(
        nonterminalWork,
        List(ReportedEngineLineSuffix(List("e7e5"), ReportedWhiteEngineScore.Centipawns(23), 16))
      ),
      Some(List(CandidateLineEvaluation.EngineSearch(EngineLine(List("e2e4", "e7e5"), 23, None, 16))))
    )

  test("position rules certify automatic terminals in FIDE precedence order"):
    val checkmateAtSeventyFive = canonicalHistory(
      "7k/6Q1/6K1/8/8/8/8/8 b - - 150 80"
    )
    assertEquals(
      PositionRuleAssessment.assess(checkmateAtSeventyFive),
      PositionRuleAssessment.Terminal(AutomaticTerminal.Checkmate(chess.Color.White))
    )

    val stalemate = canonicalHistory("7k/5Q2/6K1/8/8/8/8/8 b - - 0 1")
    assertEquals(
      PositionRuleAssessment.assess(stalemate),
      PositionRuleAssessment.Terminal(AutomaticTerminal.Stalemate)
    )

    val insufficient = canonicalHistory("8/8/8/8/8/8/7k/K7 w - - 0 1")
    assertEquals(
      PositionRuleAssessment.assess(insufficient),
      PositionRuleAssessment.Terminal(AutomaticTerminal.InsufficientMaterial)
    )

    val seventyFive = canonicalHistory("4k3/8/8/8/8/8/8/4K2R w K - 150 1")
    assertEquals(
      PositionRuleAssessment.assess(seventyFive),
      PositionRuleAssessment.Terminal(AutomaticTerminal.SeventyFiveMoveRule)
    )

    val knightCycle = List("g1f3", "g8f6", "f3g1", "f6g8")
    val incompleteInitial =
      "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 1 1"
    val incomplete = canonicalHistory(incompleteInitial)
    assert(!incomplete.repetitionHistoryComplete)
    assertEquals(
      PositionRuleAssessment.assess(incomplete),
      PositionRuleAssessment.Nonterminal(
        FivefoldRepetitionKnowledge.CertifiedNonterminal,
        ThreefoldClaimKnowledge.Known(DrawClaimAvailability.Unavailable),
        DrawClaimAvailability.Unavailable
      )
    )
    val provenThreefold = canonicalHistory(incompleteInitial, List.fill(2)(knightCycle).flatten)
    assert(!provenThreefold.repetitionHistoryComplete)
    assertEquals(
      PositionRuleAssessment.assess(provenThreefold),
      PositionRuleAssessment.Nonterminal(
        FivefoldRepetitionKnowledge.CertifiedNonterminal,
        ThreefoldClaimKnowledge.Known(DrawClaimAvailability.AvailableNow),
        DrawClaimAvailability.Unavailable
      )
    )
    val provenFivefold = canonicalHistory(incompleteInitial, List.fill(4)(knightCycle).flatten)
    assert(!provenFivefold.repetitionHistoryComplete)
    assertEquals(
      PositionRuleAssessment.assess(provenFivefold),
      PositionRuleAssessment.Terminal(AutomaticTerminal.FivefoldRepetition)
    )
    val incompleteFifty = canonicalHistory(
      "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 100 1"
    )
    assertEquals(
      PositionRuleAssessment.assess(incompleteFifty),
      PositionRuleAssessment.Nonterminal(
        FivefoldRepetitionKnowledge.HistoryUnavailable,
        ThreefoldClaimKnowledge.HistoryUnavailable,
        DrawClaimAvailability.AvailableNow
      )
    )

  test("incomplete repetition history uses the exact eight- and sixteen-ply impossibility bounds"):
    def fenAtHalfMoveClock(clock: Int): String =
      s"rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - $clock 1"

    (0 to 7).foreach { clock =>
      val fen = fenAtHalfMoveClock(clock)
      val history = canonicalHistory(fen)
      assertEquals(
        PositionRuleAssessment.assess(history),
        PositionRuleAssessment.Nonterminal(
          FivefoldRepetitionKnowledge.CertifiedNonterminal,
          ThreefoldClaimKnowledge.Known(DrawClaimAvailability.Unavailable),
          DrawClaimAvailability.Unavailable
        )
      )
      CommentaryJobReducer.startJob(
        fen,
        Nil,
        fen,
        commentaryPolicy(),
        nowEpochMs = 0L
      ) match
        case Right(_: AwaitingEngineWork) => ()
        case other                         => fail(s"clock $clock should proceed, found $other")
    }

    (8 to 15).foreach { clock =>
      val fen = fenAtHalfMoveClock(clock)
      assertEquals(
        PositionRuleAssessment.assess(canonicalHistory(fen)),
        PositionRuleAssessment.Nonterminal(
          FivefoldRepetitionKnowledge.CertifiedNonterminal,
          ThreefoldClaimKnowledge.HistoryUnavailable,
          DrawClaimAvailability.Unavailable
        )
      )
      CommentaryJobReducer.startJob(
        fen,
        Nil,
        fen,
        commentaryPolicy(),
        nowEpochMs = 0L
      ) match
        case Right(StoppedCommentaryJob(`engineProfile`, CommentaryJobStopCondition.RepetitionHistoryUnavailable, _)) => ()
        case other => fail(s"clock $clock should stop on unknown threefold history, found $other")
    }

    val sixteenFen = fenAtHalfMoveClock(16)
    assertEquals(
      PositionRuleAssessment.assess(canonicalHistory(sixteenFen)),
      PositionRuleAssessment.Nonterminal(
        FivefoldRepetitionKnowledge.HistoryUnavailable,
        ThreefoldClaimKnowledge.HistoryUnavailable,
        DrawClaimAvailability.Unavailable
      )
    )
    CommentaryJobReducer.startJob(
      sixteenFen,
      Nil,
      sixteenFen,
      commentaryPolicy(),
      nowEpochMs = 0L
    ) match
      case Right(StoppedCommentaryJob(`engineProfile`, CommentaryJobStopCondition.RepetitionHistoryUnavailable, _)) => ()
      case other => fail(s"clock 16 should stop on unknown repetition history, found $other")

  test("positive repetition remains certified with an incomplete initial history"):
    val incompleteInitial =
      "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 1 1"
    val cycle = List("g1f3", "g8f6", "f3g1", "f6g8")
    val threefoldMoves = List.fill(2)(cycle).flatten
    val threefold = canonicalHistory(incompleteInitial, threefoldMoves)
    assert(!threefold.repetitionHistoryComplete)
    assertEquals(
      PositionRuleAssessment.assess(threefold),
      PositionRuleAssessment.Nonterminal(
        FivefoldRepetitionKnowledge.CertifiedNonterminal,
        ThreefoldClaimKnowledge.Known(DrawClaimAvailability.AvailableNow),
        DrawClaimAvailability.Unavailable
      )
    )
    CommentaryJobReducer.startJob(
      incompleteInitial,
      threefoldMoves,
      threefold.currentFen,
      commentaryPolicy(),
      nowEpochMs = 0L
    ) match
      case Right(_: AwaitingEngineWork) => ()
      case other                         => fail(s"positive threefold should proceed, found $other")

    val fivefoldMoves = List.fill(4)(cycle).flatten
    val fivefold = canonicalHistory(incompleteInitial, fivefoldMoves)
    assert(!fivefold.repetitionHistoryComplete)
    assertEquals(
      PositionRuleAssessment.assess(fivefold),
      PositionRuleAssessment.Terminal(AutomaticTerminal.FivefoldRepetition)
    )
    CommentaryJobReducer.startJob(
      incompleteInitial,
      fivefoldMoves,
      fivefold.currentFen,
      commentaryPolicy(),
      nowEpochMs = 0L
    ) match
      case Right(completed: CompletedCommentaryJob) =>
        assertEquals(
          completed.result,
          CompletedPositionCommentary.PositionAction(
            PlayerFacingPositionAction.AutomaticTerminal(AutomaticTerminal.FivefoldRepetition)
          )
        )
        assertEquals(completed.progress.engineWorkIssued, 0)
      case other => fail(s"positive fivefold should complete exactly, found $other")

  test("draw claims distinguish current availability from an exact declared move"):
    val knightCycle = List("g1f3", "g8f6", "f3g1", "f6g8")
    val beforeDeclared = canonicalHistory(
      chess.variant.Standard.initialFen.value,
      knightCycle ++ knightCycle.dropRight(1)
    )
    val afterDeclared = beforeDeclared.extend(List("f6g8")).toOption.get
    val beforeAssessment = PositionRuleAssessment.assess(beforeDeclared)
    val afterAssessment = PositionRuleAssessment.assess(afterDeclared)
    assertEquals(
      beforeAssessment,
      PositionRuleAssessment.Nonterminal(
        FivefoldRepetitionKnowledge.CertifiedNonterminal,
        ThreefoldClaimKnowledge.Known(DrawClaimAvailability.Unavailable),
        DrawClaimAvailability.Unavailable
      )
    )
    assertEquals(
      afterAssessment,
      PositionRuleAssessment.Nonterminal(
        FivefoldRepetitionKnowledge.CertifiedNonterminal,
        ThreefoldClaimKnowledge.Known(DrawClaimAvailability.AvailableNow),
        DrawClaimAvailability.Unavailable
      )
    )
    assertEquals(
      PositionRuleAssessment.declaredDrawClaims("a1a2", beforeAssessment, afterAssessment),
      Set(DeclaredDrawClaim("a1a2", DrawClaimRule.ThreefoldRepetition))
    )
    assertEquals(
      PositionRuleAssessment.declaredDrawClaims(
        "a1a2",
        PositionRuleAssessment.Nonterminal(
          FivefoldRepetitionKnowledge.CertifiedNonterminal,
          ThreefoldClaimKnowledge.Known(DrawClaimAvailability.Unavailable),
          DrawClaimAvailability.Unavailable
        ),
        PositionRuleAssessment.Nonterminal(
          FivefoldRepetitionKnowledge.CertifiedNonterminal,
          ThreefoldClaimKnowledge.Known(DrawClaimAvailability.Unavailable),
          DrawClaimAvailability.AvailableNow
        )
      ),
      Set(DeclaredDrawClaim("a1a2", DrawClaimRule.FiftyMoveRule))
    )

  test("automatic root and successor terminals issue no engine work"):
    val insufficientFen = "8/8/8/8/8/8/7k/K7 w - - 0 1"
    val rootTerminal = CommentaryJobReducer
      .startJob(
        insufficientFen,
        Nil,
        insufficientFen,
        commentaryPolicy(),
        nowEpochMs = 0L
      )
      .toOption
      .get
    val completedRoot = rootTerminal match
      case completed: CompletedCommentaryJob => completed
      case other                             => fail(s"expected completed root terminal, found $other")
    assertEquals(
      completedRoot.result,
      CompletedPositionCommentary.PositionAction(
        PlayerFacingPositionAction.AutomaticTerminal(AutomaticTerminal.InsufficientMaterial)
      )
    )
    assertEquals(completedRoot.progress.engineWorkIssued, 0)

    val beforeSuccessors = "4k3/8/8/8/8/8/8/4K2R w K - 148 1"
    val successorRoot = canonicalHistory(beforeSuccessors, List("h1h2"))
    assert(successorRoot.repetitionHistoryComplete)
    assertEquals(
      PositionRuleAssessment.assess(successorRoot),
      PositionRuleAssessment.Nonterminal(
        FivefoldRepetitionKnowledge.CertifiedNonterminal,
        ThreefoldClaimKnowledge.Known(DrawClaimAvailability.Unavailable),
        DrawClaimAvailability.AvailableNow
      )
    )
    val successorTerminals = CommentaryJobReducer
      .startJob(
        beforeSuccessors,
        List("h1h2"),
        successorRoot.currentFen,
        commentaryPolicy(),
        nowEpochMs = 0L
      )
      .toOption
      .get
    val completedSuccessors = successorTerminals match
      case completed: CompletedCommentaryJob => completed
      case other                             => fail(s"expected completed successor terminals, found $other")
    val successorReviews = completedSuccessors.result match
      case CompletedPositionCommentary.MoveChoices(_, reviews) => reviews
      case other                                                => fail(s"expected move choices, found $other")
    val terminalEndpoints = successorReviews.flatMap(_.judgmentPacket.evidenceGraph.records.collect {
      case EvidenceRecord(
            _,
            CandidateLineEvaluationEvidence(
              _,
              CandidateLineEvaluation.ExactAutomaticTerminal(_, terminal)
            ),
            _
          ) => terminal
    })
    assertEquals(successorReviews.size, completedSuccessors.progress.legalMoveCount)
    assert(terminalEndpoints.nonEmpty)
    assertEquals(terminalEndpoints.toSet, Set(AutomaticTerminal.SeventyFiveMoveRule))
    assertEquals(completedSuccessors.progress.engineWorkIssued, 0)
    assertEquals(completedSuccessors.progress.rootLinesCollected, completedSuccessors.progress.legalMoveCount)
    val projectedSuccessors = RuntimeProtocol.encodePositionCommentaryResponse(
      "successor-terminals",
      completedSuccessors
    )
    val projectedEndpoints = (projectedSuccessors \ "result" \ "move_commentaries")
      .as[List[JsObject]]
      .flatMap { record =>
        val primary = (record \ "commentary" \ "primary").as[JsObject]
        List("reference_endpoint", "played_endpoint", "best_endpoint", "runner_up_endpoint")
          .flatMap(endpointKey => (primary \ endpointKey).asOpt[JsObject])
      }
    val exactEndpoints = projectedEndpoints.filter(endpoint =>
      (endpoint \ "kind").asOpt[String].contains("exact_automatic_terminal")
    )
    assert(exactEndpoints.nonEmpty, projectedSuccessors)
    exactEndpoints.foreach: endpoint =>
      assert((endpoint \ "moves").as[List[String]].nonEmpty, endpoint)
      assert((endpoint \ "terminal").asOpt[JsObject].nonEmpty, endpoint)
      assertEquals((endpoint \ "win_percent_for_mover").toOption, None, endpoint)


  test("position commentary v4 request and issued work bind history and engine profile"):
    val initialFen = chess.variant.Standard.initialFen.value
    val currentFen =
      "rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2"
    val body = Json.obj(
      "schema_version" -> PositionCommentaryJobProtocol.JobRequestSchema,
      "request_id" -> "history-test",
      "initial_fen" -> initialFen,
      "move_prefix_uci" -> Json.arr("e2e4", "e7e5"),
      "current_fen" -> currentFen,
      "engine_profile" -> engineProfile.wireId
    )
    val parsed = PositionCommentaryJobProtocol
      .parseJobRequest(Json.stringify(body).getBytes(StandardCharsets.UTF_8))
      .toOption
      .get
    assertEquals(parsed.initialFen, initialFen)
    assertEquals(parsed.movePrefixUci, List("e2e4", "e7e5"))
    assertEquals(parsed.currentFen, currentFen)
    assertEquals(parsed.engineProfile, engineProfile)
    val awaiting = CommentaryJobReducer
      .startJob(
        parsed.initialFen,
        parsed.movePrefixUci,
        parsed.currentFen,
        commentaryPolicy(),
        nowEpochMs = 0L
      )
      .toOption
      .collect { case state: AwaitingEngineWork => state }
      .get
    assertEquals(awaiting.issuedWork.enginePositionInitialFen, initialFen)
    assertEquals(awaiting.engineProfile, engineProfile)
    assertEquals(awaiting.issuedWork.engineProfile, engineProfile)
    assertEquals(awaiting.issuedWork.enginePositionMovesUci, List("e2e4", "e7e5", "a2a3"))
    assertEquals(
      awaiting.issuedWork.searchFen,
      "rnbqkbnr/pppp1ppp/8/4p3/4P3/P7/1PPP1PPP/RNBQKBNR b KQkq - 0 2"
    )
    assertEquals(awaiting.issuedWork.commentaryLineOriginFen, currentFen)
    assertEquals(awaiting.issuedWork.commentaryLinePrefixUci, List("a2a3"))
    val status = PositionCommentaryJobProtocol.projectJobSnapshot(
      CommentaryJobSnapshot("a" * 32, "history-test", Long.MaxValue, awaiting)
    )
    assertEquals(
      (status \ "issued_engine_work" \ "engine_position_moves_uci").as[List[String]],
      List("e2e4", "e7e5", "a2a3")
    )
    assertEquals(
      (status \ "issued_engine_work" \ "engine_position_initial_fen").as[String],
      initialFen
    )
    assertEquals((status \ "engine_profile").as[String], engineProfile.wireId)
    assertEquals((status \ "issued_engine_work" \ "engine_profile").as[String], engineProfile.wireId)
    val legacy = Json.obj(
      "schema_version" -> "chesstory.position-commentary.job-request.v1",
      "request_id" -> "history-test",
      "root_fen" -> currentFen
    )
    assertEquals(
      PositionCommentaryJobProtocol.parseJobRequest(
        Json.stringify(legacy).getBytes(StandardCharsets.UTF_8)
      ),
      Left(PositionCommentaryJobProtocol.PositionCommentaryJobWireFailure.UnsupportedJobRequestSchema)
    )

  test("position commentary v4 rejects missing, unknown, and legacy request profiles"):
    val base = Json.obj(
      "schema_version" -> PositionCommentaryJobProtocol.JobRequestSchema,
      "request_id" -> "profile-request",
      "initial_fen" -> chess.variant.Standard.initialFen.value,
      "move_prefix_uci" -> Json.arr(),
      "current_fen" -> chess.variant.Standard.initialFen.value,
      "engine_profile" -> engineProfile.wireId
    )
    def parse(value: JsObject) =
      PositionCommentaryJobProtocol.parseJobRequest(
        Json.stringify(value).getBytes(StandardCharsets.UTF_8)
      )

    assertEquals(
      parse(base - "engine_profile"),
      Left(PositionCommentaryJobProtocol.PositionCommentaryJobWireFailure.InvalidEngineProfile)
    )
    assertEquals(
      parse(base + ("engine_profile" -> JsString("sf18-smallnet-t2-h16-v2"))),
      Left(PositionCommentaryJobProtocol.PositionCommentaryJobWireFailure.UnsupportedEngineProfile)
    )
    List(
      "chesstory.position-commentary.job-request.v2",
      "chesstory.position-commentary.job-request.v3"
    ).foreach: legacySchema =>
      assertEquals(
        parse(base + ("schema_version" -> JsString(legacySchema))),
        Left(PositionCommentaryJobProtocol.PositionCommentaryJobWireFailure.UnsupportedJobRequestSchema)
      )
    val unavailableError = PositionCommentaryJobProtocol.jobErrorJson(None, None, None, "invalid_engine_profile")
    assertEquals(unavailableError.keys, Set("schema_version", "error"))
    assert(!Json.stringify(unavailableError).contains("null"), unavailableError)

  test("position commentary v4 HTTP route preserves issued work on unknown profiles and stops executor failures"):
    val server = ChesstoryRuntime.start(new InetSocketAddress("127.0.0.1", 0), None, workerCount = 1)
    try
      val client = HttpClient.newHttpClient()
      val jobsUri = URI.create(s"http://127.0.0.1:${server.getAddress.getPort}/v1/position-commentary-jobs")
      val create = client.send(
        HttpRequest
          .newBuilder(jobsUri)
          .header("Content-Type", "application/json")
          .POST(
            HttpRequest.BodyPublishers.ofString(
              Json.stringify(
                Json.obj(
                  "schema_version" -> PositionCommentaryJobProtocol.JobRequestSchema,
                  "request_id" -> "profile-report",
                  "initial_fen" -> chess.variant.Standard.initialFen.value,
                  "move_prefix_uci" -> Json.arr(),
                  "current_fen" -> chess.variant.Standard.initialFen.value,
                  "engine_profile" -> engineProfile.wireId
                )
              )
            )
          )
          .build(),
        HttpResponse.BodyHandlers.ofString()
      )
      assertEquals(create.statusCode(), 201)
      val issued = Json.parse(create.body()).as[JsObject]
      val issuedWork = (issued \ "issued_engine_work").as[JsObject]
      val issuedProfile = (issued \ "engine_profile").as[String]
      val issuedWorkId = (issuedWork \ "work_id").as[String]
      val reportsAccepted = (issued \ "progress" \ "engine_work_reports_accepted").as[Int]
      val workIssued = (issued \ "progress" \ "engine_work_issued").as[Int]
      val jobUri = URI.create(s"$jobsUri/${(issued \ "job_id").as[String]}")
      assertEquals((issued \ "schema_version").as[String], PositionCommentaryJobProtocol.JobStatusSchema)
      assertEquals((issued \ "state").as[String], "awaiting_engine_work")
      assertEquals(issuedProfile, engineProfile.wireId)
      assertEquals((issuedWork \ "engine_profile").as[String], issuedProfile)

      val unknownProfile = client.send(
        HttpRequest
          .newBuilder(URI.create(s"$jobUri/engine-work-reports"))
          .header("Content-Type", "application/json")
          .POST(
            HttpRequest.BodyPublishers.ofString(
              Json.stringify(
                Json.obj(
                  "schema_version" -> PositionCommentaryJobProtocol.EngineWorkReportSchema,
                  "engine_profile" -> "sf18-smallnet-t2-h16-v2",
                  "work_id" -> issuedWorkId,
                  "outcome" -> Json.obj("kind" -> "executor_failed")
                )
              )
            )
          )
          .build(),
        HttpResponse.BodyHandlers.ofString()
      )
      assertEquals(unknownProfile.statusCode(), 400)
      assertEquals(
        (Json.parse(unknownProfile.body()) \ "error").as[String],
        "unsupported_engine_profile"
      )

      val afterUnknownProfile = client.send(
        HttpRequest.newBuilder(jobUri).GET().build(),
        HttpResponse.BodyHandlers.ofString()
      )
      assertEquals(afterUnknownProfile.statusCode(), 200)
      val stillIssued = Json.parse(afterUnknownProfile.body()).as[JsObject]
      assertEquals((stillIssued \ "state").as[String], "awaiting_engine_work")
      assertEquals(
        (stillIssued \ "progress" \ "engine_work_reports_accepted").as[Int],
        reportsAccepted
      )
      assertEquals((stillIssued \ "issued_engine_work").as[JsObject], issuedWork)

      val executorFailed = client.send(
        HttpRequest
          .newBuilder(URI.create(s"$jobUri/engine-work-reports"))
          .header("Content-Type", "application/json")
          .POST(
            HttpRequest.BodyPublishers.ofString(
              Json.stringify(
                Json.obj(
                  "schema_version" -> PositionCommentaryJobProtocol.EngineWorkReportSchema,
                  "engine_profile" -> issuedProfile,
                  "work_id" -> issuedWorkId,
                  "outcome" -> Json.obj("kind" -> "executor_failed")
                )
              )
            )
          )
          .build(),
        HttpResponse.BodyHandlers.ofString()
      )
      assertEquals(executorFailed.statusCode(), 200)
      val stopped = Json.parse(executorFailed.body()).as[JsObject]
      assertEquals((stopped \ "schema_version").as[String], PositionCommentaryJobProtocol.JobStatusSchema)
      assertEquals((stopped \ "state").as[String], "stopped")
      assertEquals((stopped \ "engine_profile").as[String], issuedProfile)
      assertEquals((stopped \ "stop_condition").as[String], "engine_execution_failed")
      assertEquals((stopped \ "issued_engine_work").toOption, None)
      assertEquals((stopped \ "progress" \ "engine_work_issued").as[Int], workIssued)
      assertEquals(
        (stopped \ "progress" \ "engine_work_reports_accepted").as[Int],
        reportsAccepted + 1
      )
    finally server.stop(0)

  test("completed position commentary HTTP response retains the admitted profile"):
    val server = ChesstoryRuntime.start(new InetSocketAddress("127.0.0.1", 0), None, workerCount = 1)
    try
      val client = HttpClient.newHttpClient()
      val jobsUri = URI.create(s"http://127.0.0.1:${server.getAddress.getPort}/v1/position-commentary-jobs")
      val terminalFen = "8/8/8/8/8/8/7k/K7 w - - 0 1"
      val created = client.send(
        HttpRequest
          .newBuilder(jobsUri)
          .header("Content-Type", "application/json")
          .POST(
            HttpRequest.BodyPublishers.ofString(
              Json.stringify(
                Json.obj(
                  "schema_version" -> PositionCommentaryJobProtocol.JobRequestSchema,
                  "request_id" -> "profile-complete",
                  "initial_fen" -> terminalFen,
                  "move_prefix_uci" -> Json.arr(),
                  "current_fen" -> terminalFen,
                  "engine_profile" -> engineProfile.wireId
                )
              )
            )
          )
          .build(),
        HttpResponse.BodyHandlers.ofString()
      )
      assertEquals(created.statusCode(), 201)
      val response = Json.parse(created.body()).as[JsObject]
      assertEquals((response \ "schema_version").as[String], "chesstory.position-commentary.response.v4")
      assertEquals((response \ "engine_profile").as[String], engineProfile.wireId)
    finally server.stop(0)

  test("v4 work-required envelope exposes only exact outstanding engine work"):
    RuntimeResources.verify()
    val result = RuntimeProtocol.evaluate(request())
    assertEquals(result.httpStatus, 202)
    assertEquals((result.body \ "schema_version").as[String], RuntimeProtocol.ResponseSchema)
    assertEquals((result.body \ "request_id").as[String], "test-1")
    assertEquals((result.body \ "status").as[String], "engine_work_required")
    assert((result.body \ "probe_requests").as[JsArray].value.nonEmpty, result.body)
    assertEquals((result.body \ "move_commentary").toOption, None)

  test("a request without a primary engine comparison is unavailable before projection"):
    val noPlayedLine = input + ("variations" -> Json.arr(
      Json.obj(
        "moves" -> Json.arr("e2e4", "e7e5", "g1f3"),
        "scoreCp" -> 30,
        "depth" -> 16
      )
    ))
    val result = RuntimeProtocol.evaluate(request(body = noPlayedLine))
    assertEquals(result.httpStatus, 422)
    assertEquals((result.body \ "error").as[String], "move_review_not_buildable")

  test("v3 selected Cause projects its comparison and direct channel"):
    val selectedCauseInput = Json.obj(
      "fen" -> "8/5ppp/8/5P1P/2k3P1/2p5/5P2/2K5 b - - 0 1",
      "playedMoveUci" -> "h7h6",
      "variations" -> Json.arr(
        Json.obj(
          "moves" -> Json.arr("f7f6", "h5h6", "g7h6", "c1d1", "c4d5", "d1c2", "d5e4", "g4g5", "h6g5", "c2c3", "g5g4", "c3c2", "e4f4", "c2c3"),
          "scoreCp" -> -534,
          "depth" -> 20
        ),
        Json.obj(
          "moves" -> Json.arr("h7h6", "f5f6", "g7f6", "f2f4", "c4d4", "g4g5", "h6g5", "f4g5", "f6f5", "h5h6", "f5f4", "h6h7", "f4f3", "h7h8q", "d4e3", "h8h5", "c3c2", "c1c2", "e3f4", "h5h4", "f4f5"),
          "scoreCp" -> 682,
          "depth" -> 20
        )
      )
    )
    val initial = RuntimeProtocol.evaluate(request(body = selectedCauseInput))
    assertEquals(initial.httpStatus, 202)
    assertEquals((initial.body \ "move_commentary").toOption, None)
    val issued = (initial.body \ "probe_requests").as[JsArray].value.map(_.as[JsObject])
    val playedBranch = issued.find(probe =>
      (probe \ "candidateMove").asOpt[String].contains("h7h6") &&
        (probe \ "horizon").asOpt[String].contains("ply:6")
    ).getOrElse(fail("expected played branch horizon work"))
    val playedResource = issued.find(probe =>
      (probe \ "candidateMove").asOpt[String].contains("h7h6") &&
        (probe \ "opponentResourceMove").asOpt[String].contains("g4g5")
    ).getOrElse(fail("expected played resource work"))
    val referenceResource = issued.find(probe =>
      (probe \ "candidateMove").asOpt[String].contains("f7f6") &&
        (probe \ "opponentResourceMove").asOpt[String].contains("g4g5")
    ).getOrElse(fail("expected reference resource work"))
    assertEquals(issued.size, 3)
    val commonProbeKeys = Set(
      "id",
      "fen",
      "moves",
      "depth",
      "purpose",
      "multiPv",
      "candidateMove",
      "variationHash"
    )
    assertEquals(playedBranch.keys, commonProbeKeys + "horizon")
    assertEquals(playedResource.keys, commonProbeKeys + "opponentResourceMove")
    assertEquals(referenceResource.keys, commonProbeKeys + "opponentResourceMove")
    List(playedBranch, playedResource, referenceResource).foreach: probe =>
      assert(!Json.stringify(probe).contains("null"), probe)
    val probeResults = Json.arr(
      Json.obj(
        "id" -> (playedBranch \ "id").as[String],
        "fen" -> (playedBranch \ "fen").as[String],
        "replyLines" -> Json.arr(
          Json.obj(
            "moves" -> Json.arr(
              "f5f6", "g7f6", "f2f4", "c4d4", "g4g5", "h6g5", "f4g5", "f6f5", "h5h6", "f5f4",
              "h6h7", "f4f3", "h7h8q", "d4e3", "h8h5", "c3c2", "c1c2", "e3f4", "h5h4", "f4f5"
            ),
            "scoreCp" -> 682,
            "depth" -> (playedBranch \ "depth").as[Int]
          ),
          Json.obj(
            "moves" -> Json.arr("g4g5", "h6g5", "f2f4", "c4d4", "h5h6", "g5f4"),
            "scoreCp" -> 700,
            "depth" -> (playedBranch \ "depth").as[Int]
          ),
          Json.obj(
            "moves" -> Json.arr("f2f4", "c4d4", "g4g5", "h6g5", "h5h6", "g5f4"),
            "scoreCp" -> 650,
            "depth" -> (playedBranch \ "depth").as[Int]
          )
        ),
        "purpose" -> (playedBranch \ "purpose").as[String],
        "probedMove" -> (playedBranch \ "candidateMove").as[String],
        "depth" -> (playedBranch \ "depth").as[Int],
        "candidateMove" -> (playedBranch \ "candidateMove").as[String],
        "horizon" -> (playedBranch \ "horizon").as[String],
        "variationHash" -> (playedBranch \ "variationHash").as[String]
      ),
      Json.obj(
        "id" -> (playedResource \ "id").as[String],
        "fen" -> (playedResource \ "fen").as[String],
        "replyLines" -> Json.arr(Json.obj(
          "moves" -> Json.arr("g4g5", "h6g5", "f2f4", "c4d4", "h5h6", "g5f4"),
          "scoreCp" -> 682,
          "depth" -> (playedResource \ "depth").as[Int]
        )),
        "purpose" -> (playedResource \ "purpose").as[String],
        "probedMove" -> (playedResource \ "candidateMove").as[String],
        "depth" -> (playedResource \ "depth").as[Int],
        "candidateMove" -> (playedResource \ "candidateMove").as[String],
        "opponentResourceMove" -> (playedResource \ "opponentResourceMove").as[String],
        "variationHash" -> (playedResource \ "variationHash").as[String]
      ),
      Json.obj(
        "id" -> (referenceResource \ "id").as[String],
        "fen" -> (referenceResource \ "fen").as[String],
        "replyLines" -> Json.arr(Json.obj(
          "moves" -> Json.arr("g4g5", "h7h6", "c1d1", "c4d5", "d1c2", "d5e4"),
          "scoreCp" -> -534,
          "depth" -> (referenceResource \ "depth").as[Int]
        )),
        "purpose" -> (referenceResource \ "purpose").as[String],
        "probedMove" -> (referenceResource \ "candidateMove").as[String],
        "depth" -> (referenceResource \ "depth").as[Int],
        "candidateMove" -> (referenceResource \ "candidateMove").as[String],
        "opponentResourceMove" -> (referenceResource \ "opponentResourceMove").as[String],
        "variationHash" -> (referenceResource \ "variationHash").as[String]
      )
    )
    val result = RuntimeProtocol.evaluate(
      request(body = selectedCauseInput + ("probeResults" -> probeResults))
    )
    assertEquals(result.httpStatus, 200)
    assertEquals((result.body \ "status").as[String], "ready")
    val commentary = (result.body \ "move_commentary").as[JsObject]
    val primary = (commentary \ "primary").as[JsObject]
    List("comparison_kind", "played_move", "reference_move", "runner_up_move").foreach: deletedKey =>
      assertEquals((primary \ deletedKey).toOption, None, clues(deletedKey))
    assert((primary \ "reference_endpoint" \ "moves").as[List[String]].nonEmpty, primary)
    assert((primary \ "played_endpoint" \ "moves").as[List[String]].nonEmpty, primary)
    val delta = (primary \ "delta").as[JsObject]
    assertEquals(delta.keys, Set("kind", "candidate_win_percent_delta_for_mover"))
    List(
      "raw_cp_loss_for_mover",
      "mate_distance_loss_for_mover",
      "win_percent_loss_for_mover"
    ).foreach: lossKey =>
      assertEquals((delta \ lossKey).toOption, None, clues(lossKey))
    assertEquals((commentary \ "causal_explanation").toOption, None)
    val explanations = (commentary \ "causal_explanations").as[List[JsObject]]
    assert(explanations.nonEmpty, commentary)
    val explanation = explanations.head
    assertEquals(explanation.keys, Set("kind", "presentation", "facets"))
    val unitKind = (explanation \ "kind").as[String]
    assertEquals(unitKind, "single_cause")
    val presentation = (explanation \ "presentation").as[String]
    assert(presentation.nonEmpty, explanation)
    val facets = (explanation \ "facets").as[List[JsObject]]
    assertEquals(facets.size, 1)
    val facet = facets.head
    assertEquals((facet \ "facet_role").as[String], "lead")
    assertEquals((facet \ "presentation").toOption, None)
    List(
      "cause_evidence_id",
      "kind",
      "proof_confidence",
      "effect_mode",
      "exposure",
      "source_side",
      "event_move",
      "comparison_kind"
    ).foreach: diagnosticKey =>
      assert((facet \ diagnosticKey).asOpt[String].exists(_.nonEmpty), clues(diagnosticKey))
    assert((facet \ "proof_confidence").asOpt[String].exists(_.nonEmpty), facet)
    assertEquals((facet \ "comparison_kind").as[String], "played_vs_best")
    val channel = (facet \ "channels")(0).as[JsObject]
    assert((channel \ "causal_signature").asOpt[String].exists(_.nonEmpty), channel)
    assert((channel \ "direct_change").asOpt[String].exists(_.nonEmpty), channel)
    assert((channel \ "played_change").asOpt[String].exists(_.nonEmpty), channel)

  test("v4 rejects a shallow primary comparison without a partial success envelope"):
    val lowDepthVariations = (input \ "variations").as[JsArray].value.map: variation =>
      variation.as[JsObject] + ("depth" -> JsNumber(1))
    val result = RuntimeProtocol.evaluate(request(body = input + ("variations" -> JsArray(lowDepthVariations))))

    assertEquals(result.httpStatus, 422)
    assertEquals((result.body \ "status").as[String], "error")
    assertEquals((result.body \ "error").as[String], "move_review_not_buildable")
    assertEquals((result.body \ "move_commentary").toOption, None)
    assertEquals((result.body \ "probe_requests").toOption, None)
    assertEquals((result.body \ "availability").toOption, None)

  test("v1 rejects unknown schema and oversized PV bundles"):
    val unsupported = RuntimeProtocol.evaluate(request("v2"))
    assertEquals(unsupported.httpStatus, 400)
    assertEquals((unsupported.body \ "error").as[String], "unsupported_schema_version")
    val oversized = input + ("variations" -> JsArray(List.fill(17)((input \ "variations")(0))))
    val result = RuntimeProtocol.evaluate(request(body = oversized))
    assertEquals(result.httpStatus, 400)
    assertEquals((result.body \ "error").as[String], "input_limits_exceeded")
    val zeroMate = input + ("variations" -> Json.arr(
      Json.obj(
        "moves" -> Json.arr("e2e4", "e7e5"),
        "scoreCp" -> 0,
        "mate" -> 0,
        "depth" -> 18
      )
    ))
    val zeroMateResult = RuntimeProtocol.evaluate(request(body = zeroMate))
    assertEquals(zeroMateResult.httpStatus, 400)
    assertEquals((zeroMateResult.body \ "error").as[String], "input_limits_exceeded")

  test("public probe request exposes only the certified ordinary-reply contract"):
    val json = RuntimeProtocol.publicProbeRequestJson(
      ProbeRequest(
        id = "opaque-probe",
        fen = chess.variant.Standard.initialFen.value,
        moves = Nil,
        depth = 18,
        purpose = Some(ProbePurpose.ReplyMultipv),
        multiPv = Some(3),
        objective = Some("internal-objective"),
        requiredSignals = List("replyLines"),
        horizon = Some("ply:8"),
        candidateMove = Some("d2d4"),
        opponentResourceMove = None,
        depthFloor = Some(16),
        variationHash = Some("opaque-hash")
      )
    )
    assertEquals(
      json.keys,
      Set("id", "fen", "moves", "depth", "purpose", "multiPv", "candidateMove", "variationHash", "horizon")
    )
    List(
      "objective",
      "requiredSignals",
      "depthFloor",
      "opponentResourceMove"
    ).foreach: internalKey =>
      assert((json \ internalKey).toOption.isEmpty, clues(internalKey))
    assertEquals((json \ "horizon").as[String], "ply:8")
    assert(!Json.stringify(json).contains("internal"))
    assert(!Json.stringify(json).contains("null"), json)

  test("stripped client probe result still closes the certified branch contract"):
    val baseInput = Json.obj(
      "fen" -> "r1b2rk1/pp2ppbp/5np1/q1nP4/4PB2/2N2P2/PP4PP/2RQKBNR b K - 0 10",
      "playedMoveUci" -> "b7b5",
      "variations" -> Json.arr(
        Json.obj("moves" -> Json.arr("e7e5"), "scoreCp" -> -50, "depth" -> 16),
        Json.obj(
          "moves" -> Json.arr("b7b5", "b2b4", "a5b4", "f4d2", "b4b2", "c1c2", "b2a3", "f1e2", "b5b4"),
          "scoreCp" -> 0,
          "depth" -> 16
        )
      ),
      "ply" -> 19,
      "openingContext" -> Json.obj("name" -> "Grunfeld / b5-b4 counterplay versus center"),
      "movePrefixUci" -> Json.arr(
        "d2d4", "g8f6", "c2c4", "g7g6", "b1c3", "d7d5", "c1f4", "f8g7", "e2e3", "c7c5",
        "d4c5", "d8a5", "a1c1", "e8h8", "c4d5", "b8d7", "f2f3", "d7c5", "e3e4"
      )
    )
    val initial = RuntimeProtocol.evaluate(request(body = baseInput))
    assertEquals(initial.httpStatus, 202)
    val issued = (initial.body \ "probe_requests").as[JsArray].value.map(_.as[JsObject])
    val branchProbe = issued.find(probe =>
      (probe \ "candidateMove").asOpt[String].contains("b7b5") &&
        (probe \ "horizon").asOpt[String].exists(_.matches("ply:[1-9][0-9]*"))
    ).getOrElse(fail("expected b7b5 branch probe"))
    val playedResource = issued.find(probe =>
      (probe \ "candidateMove").asOpt[String].contains("b7b5") &&
        (probe \ "opponentResourceMove").asOpt[String].contains("a2a4")
    ).getOrElse(fail("expected b7b5 resource probe"))
    val referenceResource = issued.find(probe =>
      (probe \ "candidateMove").asOpt[String].contains("e7e5") &&
        (probe \ "opponentResourceMove").asOpt[String].contains("a2a4")
    ).getOrElse(fail("expected e7e5 resource probe"))
    assertEquals(issued.size, 3)
    val branchResult = Json.obj(
      "id" -> (branchProbe \ "id").as[String],
      "fen" -> (branchProbe \ "fen").as[String],
      "replyLines" -> Json.arr(
        Json.obj(
          "moves" -> Json.arr("b2b4", "a5b4", "f4d2", "b4b2", "c1c2", "b2a3", "f1e2", "b5b4"),
          "scoreCp" -> 0,
          "depth" -> (branchProbe \ "depth").as[Int]
        ),
        Json.obj("moves" -> Json.arr("f1e2", "b5b4"), "scoreCp" -> 0, "depth" -> (branchProbe \ "depth").as[Int]),
        Json.obj("moves" -> Json.arr("h2h3", "b5b4"), "scoreCp" -> 0, "depth" -> (branchProbe \ "depth").as[Int])
      ),
      "purpose" -> (branchProbe \ "purpose").as[String],
      "probedMove" -> (branchProbe \ "candidateMove").as[String],
      "depth" -> (branchProbe \ "depth").as[Int],
      "candidateMove" -> (branchProbe \ "candidateMove").as[String],
      "horizon" -> (branchProbe \ "horizon").as[String],
      "variationHash" -> (branchProbe \ "variationHash").as[String]
    )
    val playedResourceResult = Json.obj(
      "id" -> (playedResource \ "id").as[String],
      "fen" -> (playedResource \ "fen").as[String],
      "replyLines" -> Json.arr(Json.obj(
        "moves" -> Json.arr("a2a4", "a5a4"),
        "scoreCp" -> 0,
        "depth" -> (playedResource \ "depth").as[Int]
      )),
      "purpose" -> (playedResource \ "purpose").as[String],
      "probedMove" -> (playedResource \ "candidateMove").as[String],
      "depth" -> (playedResource \ "depth").as[Int],
      "candidateMove" -> (playedResource \ "candidateMove").as[String],
      "opponentResourceMove" -> (playedResource \ "opponentResourceMove").as[String],
      "variationHash" -> (playedResource \ "variationHash").as[String]
    )
    val referenceResourceResult = Json.obj(
      "id" -> (referenceResource \ "id").as[String],
      "fen" -> (referenceResource \ "fen").as[String],
      "replyLines" -> Json.arr(Json.obj(
        "moves" -> Json.arr("a2a4", "a5a4"),
        "scoreCp" -> -50,
        "depth" -> (referenceResource \ "depth").as[Int]
      )),
      "purpose" -> (referenceResource \ "purpose").as[String],
      "probedMove" -> (referenceResource \ "candidateMove").as[String],
      "depth" -> (referenceResource \ "depth").as[Int],
      "candidateMove" -> (referenceResource \ "candidateMove").as[String],
      "opponentResourceMove" -> (referenceResource \ "opponentResourceMove").as[String],
      "variationHash" -> (referenceResource \ "variationHash").as[String]
    )
    val results = Json.arr(branchResult, playedResourceResult, referenceResourceResult)
    val closed = RuntimeProtocol.evaluate(request(body = baseInput + ("probeResults" -> results)))
    assertEquals(closed.httpStatus, 200)
    assertEquals((closed.body \ "status").as[String], "ready")
    assertEquals((closed.body \ "probe_requests").toOption, None)
    val primary = (closed.body \ "move_commentary" \ "primary").as[JsObject]
    assertEquals((primary \ "kind").as[String], "move_verdict")
    List("comparison_kind", "played_move", "reference_move", "runner_up_move").foreach: deletedKey =>
      assertEquals((primary \ deletedKey).toOption, None, clues(deletedKey))
    assert((primary \ "reference_endpoint" \ "moves").as[List[String]].nonEmpty, primary)
    assert((primary \ "played_endpoint" \ "moves").as[List[String]].nonEmpty, primary)
    val delta = (primary \ "delta").as[JsObject]
    assertEquals(delta.keys, Set("kind", "candidate_win_percent_delta_for_mover"))
    List(
      "raw_cp_loss_for_mover",
      "mate_distance_loss_for_mover",
      "win_percent_loss_for_mover"
    ).foreach: lossKey =>
      assertEquals((delta \ lossKey).toOption, None, clues(lossKey))
    assertEquals((closed.body \ "move_commentary" \ "causal_explanations").toOption, None)
    val duplicate = RuntimeProtocol.evaluate(
      request(body = baseInput + ("probeResults" -> Json.arr(
        branchResult,
        playedResourceResult,
        referenceResourceResult,
        branchResult
      )))
    )
    assertEquals(duplicate.httpStatus, 400)
    assertEquals((duplicate.body \ "error").as[String], "input_limits_exceeded")
