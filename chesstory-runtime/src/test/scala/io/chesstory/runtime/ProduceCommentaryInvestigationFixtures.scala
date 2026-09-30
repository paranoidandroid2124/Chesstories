package io.chesstory.runtime

import java.nio.file.{ Files, Paths }
import lila.chessjudgment.model.judgment.{ CommentaryLineSource, SuppliedCommentaryLine }
import lila.chessjudgment.model.line.CanonicalPositionHistory
import play.api.libs.json.*

/** Physical work/report admission generates the fixtures; scores are invented. */
object ProduceCommentaryInvestigationFixtures:
  def main(args: Array[String]): Unit =
    val fen = chess.variant.Standard.initialFen.value
    val owner = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    val profile = CommentaryEngineProfile.ServerAdmitted
    val played = "g1f3"
    val resultFen = owner.extend(List(played)).toOption.get.currentFen
    val policy = CommentaryJobPolicy(16, 10000L, profile, investigateChoices = true)
    val root = CommentaryJobReducer.startJob(fen, Nil, fen, played, resultFen, policy, 0L).toOption.get
      .asInstanceOf[AwaitingEngineWork]
    def report(state: AwaitingEngineWork, moves: List[(String, Int)]): CommentaryJobState =
      val work = state.issuedWork
      val roots = work.rootRestriction match
        case EngineRootRestriction.Restricted(values) => values
        case EngineRootRestriction.Unrestricted => moves.map(_._1)
      require(moves.forall((move, _) => roots.contains(move)))
      val sampled = (moves ++ roots.filterNot(move => moves.exists(_._1 == move)).map(_ -> -20)).take(work.multiPv)
      CommentaryJobReducer.submitIssuedWorkReport(state, EngineWorkReport.Completed(profile, work.workId,
        work.executionKeySha256, sampled.map { (move, score) =>
          ReportedEngineLineSuffix(List(move), ReportedWhiteEngineScore.Centipawns(score), 16)
        }), 1L).toOption.get
    val awaiting = report(root, List("e2e4" -> 30, "d2d4" -> 20, "g1f3" -> 10)).asInstanceOf[AwaitingEngineWork]
    val refresh = report(awaiting, List("e2e4" -> 30, "g1h3" -> 80)).asInstanceOf[AwaitingEngineWork]
    val states = List(
      "awaiting-probe" -> awaiting,
      "refuted-alternative" -> report(awaiting, List("e2e4" -> 30, "g1h3" -> -50)),
      "awaiting-refresh" -> refresh,
      "refreshed-assessment" -> report(refresh, List("g1h3" -> 70, "g1f3" -> -10)),
      "stopped-refresh" -> CommentaryJobReducer.cancelJob(refresh),
      "deadline-before-answer" -> CommentaryJobReducer.expireJob(awaiting, 10000L))
    val rows = states.zipWithIndex.map { case ((name, state), index) =>
      Json.obj("name" -> name, "fen" -> fen, "move" -> played, "resulting_fen" -> resultFen,
        "response" -> PositionCommentaryJobProtocol.projectJobSnapshot(
          CommentaryJobSnapshot((index + 1).toString * 32, name, 10000L, state)),
        "note" -> "synthetic admitted reports; no engine run or chess-quality measurement")
    }
    val opening = owner.extend(List("e2e4", "e7e5", "g1f3", "b8c6")).toOption.get
    val openingMove = "f1b5"
    val openingResult = opening.extend(List(openingMove)).toOption.get.currentFen
    val openingStart = CommentaryJobReducer.startJob(fen, opening.movePrefixUci, opening.currentFen,
      openingMove, openingResult, policy, 0L).toOption.get.asInstanceOf[AwaitingEngineWork]
    val openingProbe = report(openingStart, List("f1b5" -> 30, "b1c3" -> 20, "d2d4" -> 20))
      .asInstanceOf[AwaitingEngineWork]
    val openingDone = report(openingProbe, List("f1b5" -> 30, "f1c4" -> 30))
    val openingRows = List("opening-pv-priority" -> openingProbe, "opening-equal-alternative" -> openingDone)
      .zipWithIndex.map { case ((name, state), index) =>
        Json.obj("name" -> name, "fen" -> opening.currentFen, "move" -> openingMove,
          "resulting_fen" -> openingResult,
          "response" -> PositionCommentaryJobProtocol.projectJobSnapshot(
            CommentaryJobSnapshot((index + 7).toString * 32, name, 10000L, state)),
          "note" -> "known opening with synthetic admitted scores; priority and delivery regression, no engine measurement")
      }
    def submitPaths(state: AwaitingEngineWork, paths: List[(List[String], Int)]): CommentaryJobState =
      val work = state.issuedWork
      CommentaryJobReducer.submitIssuedWorkReport(state, EngineWorkReport.Completed(profile, work.workId,
        work.executionKeySha256, paths.map { (moves, score) =>
          ReportedEngineLineSuffix(moves, ReportedWhiteEngineScore.Centipawns(score), 16)
        }), 1L).toOption.get
    def boardStart(board: String, actual: String, supplied: List[List[String]] = Nil): AwaitingEngineWork =
      val history = CanonicalPositionHistory.from(board, Nil, board).toOption.get
      CommentaryJobReducer.startJob(board, Nil, board, actual,
        history.extend(List(actual)).toOption.get.currentFen, policy, 0L,
        supplied.map(SuppliedCommentaryLine(CommentaryLineSource.Analysis, _))).toOption.get.asInstanceOf[AwaitingEngineWork]
    def completeProbe(state: CommentaryJobState, referencePath: List[String]): CommentaryJobState =
      val awaiting = state.asInstanceOf[AwaitingEngineWork]
      if awaiting.issuedWork.purpose == EngineWorkPurpose.ResponseComparison then
        require(awaiting.issuedWork.searchAnchor.get.prefixMoves == List("d2d3", "d5e4", "d3e4"))
        submitPaths(awaiting, List(List("d8d1") -> -900, List("d8d7") -> 0, List("d8d6") -> 10))
      else
        require(awaiting.issuedWork.purpose == EngineWorkPurpose.InvestigationComparison)
        val pair = awaiting.issuedWork.rootRestriction match
          case EngineRootRestriction.Restricted(moves) => moves
          case _ => sys.error("expected an owned comparison")
        val sampled = (referencePath.head :: pair.filterNot(_ == referencePath.head)).take(awaiting.issuedWork.multiPv)
        submitPaths(awaiting, sampled.map(move =>
          if move == referencePath.head then referencePath -> 30 else List(move) -> -20))
    val targetFen = "3r2k1/8/8/3p4/4P3/5N2/3P4/3Q1B1K w - - 0 1"
    // Existing authored continuations are retained independently from the short
    // current engine samples; they do not choose a deeper exposure before both arms.
    val targetStart = boardStart(targetFen, "d2d3", List(
      List("d2d3", "d5e4", "d3e4"), List("f1d3", "d5e4", "d3e4")))
    val responseAwaiting = submitPaths(targetStart, List(
      List("f1d3") -> 30, List("d2d3", "d8d7") -> 20, List("e4d5") -> -20))
      .asInstanceOf[AwaitingEngineWork]
    require(responseAwaiting.issuedWork.purpose == EngineWorkPurpose.ResponseComparison)
    require(responseAwaiting.issuedWork.searchAnchor.get.rootMove == "d2d3")
    val referenceSurvey = submitPaths(responseAwaiting, List(
      List("d5e4") -> -100, List("d8d7") -> 40, List("d8d6") -> 50)).asInstanceOf[AwaitingEngineWork]
    require(referenceSurvey.issuedWork.searchAnchor.get.rootMove == "f1d3")
    val responseAnswered = submitPaths(referenceSurvey, List(
      List("d5e4", "d3e4") -> -100, List("d8d7") -> 40, List("d8d6") -> 50))
    val suppliedDecline = List(List("d2d3", "d8d7"))
    val observedStart = boardStart(targetFen, "d2d3", suppliedDecline :+ List("d2d3", "d5e4", "d3e4"))
    val observedSurvey = submitPaths(observedStart, List(List("d2d3") -> 30,
      List("f1d3") -> 20, List("e4d5") -> -20)).asInstanceOf[AwaitingEngineWork]
    // Three returned declines cannot stand in for an unreturned capture class.
    val classAwaiting = submitPaths(observedSurvey, List(
      List("d8d7") -> -60, List("d8d6") -> -50, List("g8h8") -> -40)).asInstanceOf[AwaitingEngineWork]
    require(classAwaiting.issuedWork.rootRestriction == EngineRootRestriction.Restricted(List("d5e4")))
    require(classAwaiting.issuedWork.multiPv == 1)
    val observed = submitPaths(classAwaiting, List(List("d5e4", "d3e4") -> -1000))
    // The previously observed weaker decline remains a bounded function reading after reselection.
    val quietProbe = submitPaths(observedSurvey, List(
      List("g8h8") -> -200, List("d5e4", "d3e4") -> -100, List("d8d7") -> 40))
    val thirdQuiet = completeProbe(quietProbe, List("d2d3", "d5e4", "d3e4"))
    val partialAwaiting = submitPaths(boardStart(targetFen, "d2d3", suppliedDecline), List(
      List("d2d3", "d5e4") -> 30, List("f1d3") -> 20, List("e4d5") -> -20)).asInstanceOf[AwaitingEngineWork]
    val pinnedFen = "4r2k/8/8/2b5/3N4/8/4R3/4K3 w - - 0 1"
    val pinnedStart = boardStart(pinnedFen, "e2e4", List(List("e2e4", "e8e7")))
    val pinnedSurvey = submitPaths(pinnedStart, List(List("e2e4", "c5d4") -> 30,
      List("d4c6") -> 20, List("d4b5") -> -20)).asInstanceOf[AwaitingEngineWork]
    val pinnedClasses = submitPaths(pinnedSurvey, List(
      List("e8e7") -> -150, List("e8e6") -> -140, List("e8e5") -> -130)).asInstanceOf[AwaitingEngineWork]
    require(pinnedClasses.issuedWork.rootRestriction == EngineRootRestriction.Restricted(List("c5d4", "e8e4")))
    val pinned = submitPaths(pinnedClasses, List(List("c5d4") -> -100, List("e8e4") -> 0))
    val responseRows = List("response-awaiting" -> responseAwaiting,
      "response-answered" -> responseAnswered, "response-stopped" -> CommentaryJobReducer.cancelJob(partialAwaiting),
      "response-observed" -> observed, "response-pinned-recapture" -> pinned,
      "response-class-awaiting" -> classAwaiting, "response-third-quiet" -> thirdQuiet).zipWithIndex.map { case ((name, state), index) =>
        val board = if name == "response-pinned-recapture" then pinnedFen else targetFen
        Json.obj("name" -> name, "fen" -> board, "move" -> state.playedMoveUci,
          "resulting_fen" -> state.resultingFen,
          "response" -> PositionCommentaryJobProtocol.projectJobSnapshot(
            CommentaryJobSnapshot((index + 20).toString * 16, name, 10000L, state)),
          "note" -> "synthetic legal-board reports and fixture-author continuations; conditional child scores, not automatic discovery, chess-quality or speed measurement")
      }
    val anand = "2rq1rk1/p3bppp/bpn1p3/1P1n4/3P4/P1N2N2/1BB2PPP/R2QR1K1 b - - 0 15"
    val delayed = List("d5c3", "d1d3", "g7g6", "b2c3", "d8c7", "b5a6", "c6a5", "c3a5")
    val continuationAwaiting = submitPaths(boardStart(anand, delayed.head), List(
      List(delayed.head) -> -50, List("a6b7") -> -40, List("c6b8") -> -30)).asInstanceOf[AwaitingEngineWork]
    val deepAwaiting = submitPaths(continuationAwaiting, List(delayed.tail -> 120,
      List("b2c3", "a6b5") -> 10, List("b5c6") -> 0)).asInstanceOf[AwaitingEngineWork]
    require(deepAwaiting.issuedWork.searchAnchor.get.prefixMoves == delayed.take(2))
    val deepComplete = submitPaths(deepAwaiting, List(delayed.drop(2) -> 120, List("f7f5") -> 10, List("f7f6") -> 20))
    val bareBoard = "r1bqkbnr/1pp2ppp/p1p5/4p3/4P3/5N2/PPP2PPP/RNB1K2R w KQkq - 0 5"
    val bareCapture = submitPaths(boardStart(bareBoard, "f3e5"), List(
      List("f3e5") -> 30, List("e1g1") -> 20, List("c2c3") -> 10))
    val orton = "1rr3k1/4q1pp/p2pp3/4p1N1/BPn2nQ1/P5PP/1P3P2/1R2R1K1 b - - 0 27"
    val moveOrder = submitPaths(boardStart(orton, "h7h5"), List(
      List("h7h5", "g4h4", "e7g5") -> -50, List("e7g5") -> 100, List("g8h8") -> 150))
    val exchange = owner.extend(List("e2e4", "e7e5", "g1f3", "b8c6", "f1b5", "a7a6", "b5c6", "d7c6")).toOption.get
    val recapturedKnight = List("f3e5", "d8d4", "b1c3", "d4e5")
    val otherPawn = List("f3e5", "d8d4", "e5f3", "d4e4")
    val exchangeStart = CommentaryJobReducer.startJob(fen, exchange.movePrefixUci, exchange.currentFen,
      "f3e5", exchange.extend(List("f3e5")).toOption.get.currentFen, policy, 0L, Nil)
      .toOption.get.asInstanceOf[AwaitingEngineWork]
    val exchangeReply = submitPaths(exchangeStart, List(List("f3e5") -> 30,
      List("e1g1") -> 20, List("d2d4") -> 10)).asInstanceOf[AwaitingEngineWork]
    require(exchangeReply.issuedWork.searchAnchor.get.prefixMoves == List("f3e5"))
    val exchangeChoice = submitPaths(exchangeReply, List(recapturedKnight.tail -> -20,
      List("g8f6") -> 0, List("c8e6") -> 10)).asInstanceOf[AwaitingEngineWork]
    require(exchangeChoice.issuedWork.searchAnchor.get.prefixMoves == List("f3e5", "d8d4"))
    val exchangeCompared = submitPaths(exchangeChoice, List(otherPawn.drop(2) -> 30,
      recapturedKnight.drop(2) -> -20, List("c2c3", "d4e5") -> -40))
    require(exchangeCompared.responseSearches.exists(_.anchor.prefixMoves == List("f3e5", "d8d4")))
    val continuationRows = List(
      ("continuation-awaiting", anand, continuationAwaiting),
      ("continuation-deep-awaiting", anand, deepAwaiting),
      ("continuation-deep-complete", anand, deepComplete),
      ("continuation-deep-stopped", anand, CommentaryJobReducer.cancelJob(deepAwaiting)),
      ("continuation-bare-capture", bareBoard, bareCapture),
      ("continuation-move-order", orton, CommentaryJobReducer.cancelJob(moveOrder)),
      ("continuation-other-target", exchange.currentFen, exchangeCompared)
    ).zipWithIndex.map { case ((name, board, state), index) =>
      Json.obj("name" -> name, "fen" -> board, "move" -> state.playedMoveUci, "resulting_fen" -> state.resultingFen,
        "response" -> PositionCommentaryJobProtocol.projectJobSnapshot(
          CommentaryJobSnapshot((index + 40).toString * 16, name, 10000L, state)),
        "note" -> "author-supplied study lines or a constructed control board, with synthetic reports; observed relations and investigation delivery, not engine discovery or performance")
    }
    val discoveryStart = CommentaryJobReducer.startJob(fen, exchange.movePrefixUci, exchange.currentFen,
      "f3e5", exchange.extend(List("f3e5")).toOption.get.currentFen, policy, 0L)
      .toOption.get.asInstanceOf[AwaitingEngineWork]
    val discoveryReply = submitPaths(discoveryStart, List(List("f3e5") -> 50,
      List("e1g1") -> 40, List("d2d4") -> 30)).asInstanceOf[AwaitingEngineWork]
    val discoveryAwaiting = submitPaths(discoveryReply, List(List("d8d4", "e5f3") -> -20,
      List("d8e7", "b1c3") -> 0, List("f8d6") -> 10)).asInstanceOf[AwaitingEngineWork]
    require(discoveryAwaiting.issuedWork.searchAnchor.get.prefixMoves == List("f3e5", "d8d4", "e5f3"))
    val discoveryComplete = submitPaths(discoveryAwaiting, List(
      List("d4e4", "d1e2", "e4e2", "e1e2") -> 0, List("d4d6") -> 40, List("d4d8") -> 50))
    val recoveryRelost = submitPaths(discoveryAwaiting, List(
      List("d4e4", "e1f1", "e4e2", "f1e2") -> 900, List("d4d6") -> 40, List("d4d8") -> 50))
    // The same synthetic report may end at check instead of supplying its answer.
    // A balanced material ledger must not hide that unanswered finite endpoint.
    val checkEndpoint = submitPaths(discoveryAwaiting, List(
      List("d4e4") -> 0, List("d4d6") -> 40, List("d4d8") -> 50)).asInstanceOf[CompletedCommentaryJob]
    val unansweredCheck = checkEndpoint.continuationInquiries.find(_.question.prefixMoves == otherPawn).get
    require(unansweredCheck.question.kinds.contains("check_response_not_observed"))
    require(unansweredCheck.question.observedMoves.isEmpty)
    require(unansweredCheck.state == "deferred" && unansweredCheck.stopReason.contains("investigation_limit"))
    require(checkEndpoint.continuationReadings.find(_.path.moves == otherPawn).get.openLossEventIds.isEmpty)
    val discoveryRows = List("continuation-discovery-awaiting" -> discoveryAwaiting,
      "continuation-discovery-complete" -> discoveryComplete, "continuation-recovery-relost" -> recoveryRelost,
      "continuation-check-endpoint-deferred" -> checkEndpoint)
      .zipWithIndex.map { case ((name, state), index) =>
      Json.obj("name" -> name, "fen" -> exchange.currentFen, "move" -> state.playedMoveUci,
        "resulting_fen" -> state.resultingFen,
        "response" -> PositionCommentaryJobProtocol.projectJobSnapshot(
          CommentaryJobSnapshot((index + 60).toString * 16, name, 10000L, state)),
        "note" -> "no supplied recovery path; synthetic short reports exercise question discovery and follow-up delivery, not engine strength or discovery-rate measurement")
    }
    val routeStart = CommentaryJobReducer.startJob(fen, exchange.movePrefixUci, exchange.currentFen,
      "f3e5", exchange.extend(List("f3e5")).toOption.get.currentFen, policy, 0L)
      .toOption.get.asInstanceOf[AwaitingEngineWork]
    val routeSurvey = submitPaths(routeStart, List(recapturedKnight -> 50,
      List("e1g1") -> 40, List("d2d4") -> 30)).asInstanceOf[AwaitingEngineWork]
    require(routeSurvey.issuedWork.searchAnchor.get.prefixMoves == recapturedKnight.take(2))
    val routeAwaiting = submitPaths(routeSurvey, List(recapturedKnight.drop(2) -> 20,
      List("c2c3") -> 10, List("d2d3") -> 0)).asInstanceOf[AwaitingEngineWork]
    require(routeAwaiting.issuedWork.rootRestriction match
      case EngineRootRestriction.Restricted(moves) => moves.size == 9 && moves.contains("b1c3")
      case _ => false)
    val routeSampled = submitPaths(routeAwaiting, List(List("e5f3") -> 100,
      List("e5d3") -> 90, List("e5g4") -> 80))
    val routeCompared = submitPaths(routeAwaiting, List(List("e5f3") -> 100,
      List("e5d3") -> 90, recapturedKnight.drop(2) -> 0))
    val routeRows = List("continuation-route-awaiting" -> routeAwaiting,
      "continuation-route-sampled" -> routeSampled, "continuation-route-compared" -> routeCompared,
      "continuation-route-cancelled" -> CommentaryJobReducer.cancelJob(routeAwaiting))
      .zipWithIndex.map { case ((name, state), index) =>
        Json.obj("name" -> name, "fen" -> exchange.currentFen, "move" -> state.playedMoveUci,
          "resulting_fen" -> state.resultingFen,
          "response" -> PositionCommentaryJobProtocol.projectJobSnapshot(
            CommentaryJobSnapshot((index + 70).toString * 16, name, 10000L, state)),
          "note" -> "recovery route in the current synthetic root report; complete legal challenge class, same-work comparison or explicitly missing reference, no engine strength measurement")
      }
    val supportFen = "7k/8/8/3p2P1/4P3/5P2/3N4/6K1 w - - 0 1"
    val supportPrefix = List("g1h1", "d5e4")
    def supportChoice(board: String, donor: List[String]): AwaitingEngineWork =
      submitPaths(boardStart(board, "g1h1"), List(donor -> 100,
        List("g1h2") -> 90, List("d2c4") -> 80)).asInstanceOf[AwaitingEngineWork]
    val supportSuffix = List("h8g7", "h1g2", "g7g6", "g2h2")
    val supportKnight = supportPrefix ++ List("d2e4") ++ supportSuffix
    val supportPawn = supportPrefix ++ List("f3e4") ++ supportSuffix
    val supportSurvey = supportChoice(supportFen, supportKnight)
    require(supportSurvey.issuedWork.searchAnchor.get.prefixMoves == supportPrefix)
    val supportProbe = submitPaths(supportSurvey, List(List("h1h2") -> 300,
      List("f3f4") -> 200, List("g5g6") -> 100)).asInstanceOf[AwaitingEngineWork]
    require(supportProbe.issuedWork.rootRestriction == EngineRootRestriction.Restricted(List("d2e4", "f3e4", "h1h2")))
    val supportCompared = submitPaths(supportProbe, List(List("h1h2") -> 300,
      supportKnight.drop(2) -> 200, supportPawn.drop(2) -> 100))
    val supportShort = submitPaths(supportProbe, List(List("h1h2") -> 300,
      List("d2e4") -> 200, List("f3e4") -> 100))
    val supportUseAwaiting = submitPaths(supportSurvey, List(List("h1h2") -> 300,
      List("d2e4") -> 200, List("f3e4") -> 100)).asInstanceOf[AwaitingEngineWork]
    require(supportUseAwaiting.issuedWork.searchAnchor.get.prefixMoves == (supportPrefix :+ "f3e4"))
    val supportUseComplete = submitPaths(supportUseAwaiting, List(supportSuffix -> 0,
      List("h8g8") -> 10, List("h8h7") -> 20))
    val supportRows = List("continuation-support-discovery-awaiting" -> supportSurvey,
      "continuation-support-class-awaiting" -> supportProbe, "continuation-support-application" -> supportCompared,
      "continuation-support-use-unobserved" -> supportShort,
      "continuation-support-frontier-awaiting" -> supportUseAwaiting,
      "continuation-support-frontier-complete" -> supportUseComplete)
      .zipWithIndex.map { case ((name, state), index) =>
        Json.obj("name" -> name, "fen" -> supportFen, "move" -> state.playedMoveUci,
          "resulting_fen" -> state.resultingFen,
          "response" -> PositionCommentaryJobProtocol.projectJobSnapshot(
            CommentaryJobSnapshot((index + 80).toString * 16, name, 10000L, state)),
          "note" -> "constructed board and synthetic reports; one observed knight-support use discovers the recapture question, omitted recapturers remain separate classes, no engine strength or discovery-rate measurement")
      }
    val directFen = "6rk/8/8/3p2P1/4P3/5P2/3N4/6K1 w - - 0 1"
    val directKnight = supportPrefix ++ List("d2e4", "g8g5", "e4g5")
    val directSurvey = supportChoice(directFen, directKnight)
    require(directSurvey.issuedWork.searchAnchor.get.prefixMoves == supportPrefix)
    val directFrontier = submitPaths(directSurvey, List(List("h1h2") -> 300,
      List("d2e4") -> 200, List("f3e4") -> 100)).asInstanceOf[AwaitingEngineWork]
    require(directFrontier.issuedWork.searchAnchor.get.prefixMoves == (supportPrefix :+ "f3e4"))
    val directLost = submitPaths(directFrontier, List(List("g8g5") -> -100,
      List("h8h7") -> 10, List("g8g7") -> 20))
    val directRecaptured = submitPaths(directFrontier, List(
      List("g8g5", "d2f3", "h8h7", "f3g5") -> 100, List("h8h7") -> 10, List("g8g7") -> 20))
    val directRows = List("continuation-direct-support-awaiting" -> directSurvey,
      "continuation-direct-support-frontier-awaiting" -> directFrontier,
      "continuation-direct-support-target-lost" -> directLost,
      "continuation-direct-support-recaptured" -> directRecaptured)
      .zipWithIndex.map { case ((name, state), index) =>
        Json.obj("name" -> name, "fen" -> directFen, "move" -> state.playedMoveUci,
          "resulting_fen" -> state.resultingFen,
          "response" -> PositionCommentaryJobProtocol.projectJobSnapshot(
            CommentaryJobSnapshot((index + 86).toString * 16, name, 10000L, state)),
          "note" -> "constructed board with synthetic, deliberately cooperative continuations; a target capture alone leaves recovery unobserved, another actual provider may recover it; no best-play claim")
      }
    val exposedBishop = List("f1d3", "d5e4", "d3e4")
    val exposedPawn = List("d2d3", "d5e4", "d3e4")
    val exposureSurvey = submitPaths(boardStart(targetFen, exposedBishop.head), List(exposedPawn -> 100,
      exposedBishop -> 90, List("e4d5") -> 80)).asInstanceOf[AwaitingEngineWork]
    require(exposureSurvey.issuedWork.searchAnchor.get.prefixMoves == exposedBishop)
    val exposureOtherArm = submitPaths(exposureSurvey, List(List("d8d7") -> -10,
      List("d8d6") -> 0, List("g8h8") -> 10)).asInstanceOf[AwaitingEngineWork]
    require(exposureOtherArm.issuedWork.searchAnchor.get.prefixMoves == exposedPawn)
    require(exposureOtherArm.issuedWork.rootRestriction == EngineRootRestriction.Unrestricted)
    val exposureComplete = submitPaths(exposureOtherArm, List(List("d8d1") -> -900,
      List("d8d7") -> 0, List("d8d6") -> 10))
    val exposureRows = List("continuation-exposure-awaiting" -> exposureSurvey,
      "continuation-exposure-other-arm" -> exposureOtherArm, "continuation-exposure-complete" -> exposureComplete)
      .zipWithIndex.map { case ((name, state), index) =>
        Json.obj("name" -> name, "fen" -> targetFen, "move" -> state.playedMoveUci,
          "resulting_fen" -> state.resultingFen,
          "response" -> PositionCommentaryJobProtocol.projectJobSnapshot(
            CommentaryJobSnapshot((index + 90).toString * 16, name, 10000L, state)),
          "note" -> "synthetic reports compare both selected exposure anchors before supplementing omitted captures; Rxd2 has legal Qxd2, whereas the observed Rxd1 removes the queen; legal availability alone does not establish gain")
      }
    val conditionalSupport = submitPaths(boardStart(directFen, "g1h1", List(directKnight)), List(
      List("g1h2") -> 110, List("g1h1") -> 100, List("d2c4") -> 90)).asInstanceOf[AwaitingEngineWork]
    require(conditionalSupport.issuedWork.searchAnchor.get.prefixMoves == List("g1h1"))
    val conditionalAfterReply = submitPaths(conditionalSupport, List(List("g8g5") -> -100,
      List("g8g7") -> 0, List("h8h7") -> 10)).asInstanceOf[AwaitingEngineWork]
    require(conditionalAfterReply.issuedWork.searchAnchor.get.prefixMoves == List("g1h2"))
    val provenanceRows = List("continuation-supplied-support-current-reply" -> conditionalSupport,
      "continuation-supplied-support-other-current-arm" -> conditionalAfterReply)
      .zipWithIndex.map { case ((name, state), index) =>
        Json.obj("name" -> name, "fen" -> directFen, "move" -> state.playedMoveUci,
          "resulting_fen" -> state.resultingFen,
          "response" -> PositionCommentaryJobProtocol.projectJobSnapshot(
            CommentaryJobSnapshot((index + 93).toString * 16, name, 10000L, state)),
          "note" -> "the cooperative support use is author-supplied and absent from current short root reports; current Rxg5 replies and the still-empty reference arm precede that conditional premise and an unsampled exd5 recovery proposal, without calling the omitted route weak or impossible")
      }
    val quietExposureFen = "3r2k1/8/8/8/8/8/3N4/3Q3K w - - 0 1"
    val quietExposure = submitPaths(boardStart(quietExposureFen, "d2c4"), List(List("d2c4") -> 100,
      List("d2f3") -> 90, List("d2b3") -> 80)).asInstanceOf[AwaitingEngineWork]
    require(quietExposure.issuedWork.searchAnchor.get.prefixMoves == List("d2c4"))
    require(quietExposure.continuationReadings.exists(reading => reading.path.moves == List("d2c4") &&
      reading.events.isEmpty && reading.captureExposures.exists(_.captures.exists(_.move == "d8d1"))))
    val quietExposureRows = List(Json.obj("name" -> "continuation-quiet-exposure-awaiting", "fen" -> quietExposureFen,
      "move" -> quietExposure.playedMoveUci, "resulting_fen" -> quietExposure.resultingFen,
      "response" -> PositionCommentaryJobProtocol.projectJobSnapshot(
        CommentaryJobSnapshot("95" * 16, "continuation-quiet-exposure-awaiting", 10000L, quietExposure)),
      "note" -> "one quiet move in a synthetic root report vacates a ray barrier and exposes a legal queen capture; no supplied capture suffix and no claim that the capture was played or its gain proved"))
    val danish = owner.extend(List("e2e4", "e7e5", "d2d4", "e5d4", "c2c3", "d4c3", "f1c4", "c3b2",
      "c1b2", "d7d5", "c4d5", "g8f6")).toOption.get
    val checkOnlyFen = "rnbqkb1r/ppp2ppp/5n2/3B4/8/8/PB3PPP/RN1QK1NR w KQkq - 0 7"
    val checkOnly = CanonicalPositionHistory.from(checkOnlyFen, Nil, checkOnlyFen).toOption.get
    val queenGain = List("d5f7", "e8f7", "d1d8")
    val danishRows = List(("continuation-danish-counterreply-awaiting", danish, false),
      ("continuation-danish-check-only-awaiting", checkOnly, true)).zipWithIndex.flatMap {
      case ((name, history, onlyChecks), index) =>
        require(history.extend(queenGain :+ "f8b4").toOption.get.currentPosition.check.yes)
        val started = CommentaryJobReducer.startJob(history.initialFen, history.movePrefixUci, history.currentFen,
          queenGain.head, history.extend(queenGain.take(1)).toOption.get.currentFen, policy, 0L)
          .toOption.get.asInstanceOf[AwaitingEngineWork]
        val first = submitPaths(started, List(queenGain -> 0, List("b1c3") -> -10, List("g1f3") -> -20))
          .asInstanceOf[AwaitingEngineWork]
        require(first.issuedWork.searchAnchor.get.prefixMoves == queenGain.take(1))
        val waiting = submitPaths(first, List(queenGain.tail -> 0, List("e8e7") -> 10))
          .asInstanceOf[AwaitingEngineWork]
        require(waiting.issuedWork.searchAnchor.get.prefixMoves == queenGain)
        require(waiting.issuedWork.rootRestriction == EngineRootRestriction.Unrestricted)
        val reading = waiting.continuationReadings.find(_.path.moves == queenGain).get
        val view = waiting.preparedPaths.get.resourceViews(waiting.observationsPacket.get)(reading.path.positions.last.id.get)
        require(view.checkingMoves.exists(_.exists(_.move == "f8b4")))
        if onlyChecks then require(view.legalRows.get.forall(_.capture.isEmpty))
        require(waiting.continuationInquiries.exists(inquiry => inquiry.question.prefixMoves == queenGain &&
          inquiry.question.kinds.contains("material_followup") && inquiry.state == "awaiting_engine"))
        val response = PositionCommentaryJobProtocol.projectJobSnapshot(
          CommentaryJobSnapshot((index + 96).toString * 16, name, 10000L, waiting))
        val row = Json.obj("name" -> name, "fen" -> history.currentFen, "move" -> waiting.playedMoveUci,
          "resulting_fen" -> waiting.resultingFen,
          "response" -> response,
          "note" -> ((if onlyChecks then "constructed Danish sibling with Pe4 removed and no legal capture at the inquiry anchor; "
            else "exact standard-start Danish move history through 6...Nf6; ") +
            "invented root and child reports supply Bxf7 Kxf7 Qxd8 but no Bb4 continuation; the existing complete legal-move owner identifies Bb4 as checking, and the whole opponent choice awaits analysis; no engine run, automatic engine discovery or strength measurement"))
        if onlyChecks then List(row)
        else
          // Explicit evidence-loss control: cancel through the normal lifecycle first,
          // then remove material context and let the real refresh determine its state.
          val cancelled = CommentaryJobReducer.cancelJob(waiting).asInstanceOf[CompletedCommentaryJob]
          val selected = cancelled.continuationInquiries.find(inquiry => inquiry.question.prefixMoves == queenGain &&
            inquiry.question.kinds.contains("material_followup")).get
          require(selected.state == "stopped" && selected.stopReason.contains("cancelled"))
          require(reading.path.certified.flatMap(_._2.material).nonEmpty)
          val stripped = lila.chessjudgment.testing.CheckResponseEvidenceFixture.withoutMaterial(
            cancelled.observationsPacket.get)
          val strippedPrepared = ObservedLineProtocol.prepare(stripped)
          val strippedSearches = cancelled.responseSearches.map { search =>
            val child = stripped.assembly.input.positionHistory.extend(search.anchor.prefixMoves).toOption.get
            TargetResponseInvestigation.readSearch(stripped,
              CommentaryInvestigation.EngineCandidates(search.work, search.lines), child, strippedPrepared).get
          }
          val retained = strippedPrepared.paths.find(_.line.ref.id == reading.path.line.ref.id).get
          require(retained.moves == reading.path.moves && retained.ids == reading.path.ids)
          require(retained.certified.nonEmpty && retained.certified.flatMap(_._2.material).isEmpty)
          require(strippedPrepared.continuationReadings.isEmpty)
          val refreshed = CommentaryJobReducer.refreshContinuationInquiries(stripped, strippedPrepared,
            Nil, List(selected), strippedSearches)
          require(refreshed.size == 1 && refreshed.head.question.premises == selected.question.premises)
          require(refreshed.head.state == "stopped" && refreshed.head.stopReason == selected.stopReason)
          val material = refreshed.head.premiseObservations.filter(_.premise.origin.kind == "material_followup")
          require(material.nonEmpty && material.forall(observation =>
            observation.observationState == "unavailable" && observation.witnessLineIds.isEmpty))
          require(material.flatMap(_.premise.sourceLineIds).forall(id =>
            strippedPrepared.paths.exists(path => path.line.ref.id == id && path.certified.nonEmpty)))
          require(material.flatMap(_.premise.sourceLineIds).forall(id => strippedSearches.exists(search =>
            search.lines.size > 1 && search.pathsByMove.values.exists(_.line.ref.id == id))))
          val unavailableName = "continuation-material-context-unavailable"
          val unavailableResponse = PositionCommentaryJobProtocol.projectJobSnapshot(
            CommentaryJobSnapshot("98" * 16, unavailableName, 10000L, cancelled)) ++ Json.obj(
            "observations" -> RuntimeProtocol.observationsJson(stripped, prepared = Some(strippedPrepared),
              responseSearches = strippedSearches, continuationInquiries = refreshed, rootSearches = cancelled.rootSearches))
          List(row, Json.obj("name" -> unavailableName, "fen" -> history.currentFen,
            "move" -> waiting.playedMoveUci, "resulting_fen" -> waiting.resultingFen,
            "response" -> unavailableResponse,
            "note" -> "constructed missing-context control from the normally cancelled standard-start Danish synthetic report: only the optional material ledger is removed; the exact admitted replay, stopped inquiry and selected provenance remain, and the real reducer refresh emits unavailable; no engine run or changed chess history"))
    }
    val produced = rows ++ openingRows ++ responseRows ++ continuationRows ++ discoveryRows ++ routeRows ++
      supportRows ++ directRows ++ exposureRows ++ provenanceRows ++ quietExposureRows ++ danishRows
    require(produced.size == 49)
    Files.writeString(Paths.get("..", "judgment-evaluation", "fixtures", "public-commentary-v6",
      "commentary-investigation-produced.json"), Json.prettyPrint(JsArray(produced)) + "\n")
