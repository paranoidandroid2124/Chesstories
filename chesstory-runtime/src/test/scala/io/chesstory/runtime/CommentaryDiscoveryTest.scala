package io.chesstory.runtime

import lila.chessjudgment.model.line.CanonicalPositionHistory

/** Development examples with synthetic ranking reports. Questions are discovered without hints;
  * one case explicitly requests an otherwise unnecessary continuation after checking that boundary.
  */
class CommentaryDiscoveryTest extends munit.FunSuite:
  import CommentaryFact.*
  import CommentaryProposal.*
  private val initial = chess.variant.Standard.initialFen.value
  private val profile = CommentaryEngineProfile.ServerAdmitted
  private val opening = List("e2e4", "e7e5", "g1f3", "b8c6")

  private def begin(prefix: List[String], rows: List[List[String]], initialFen: String = initial,
      limit: Int = 600, proposals: List[Request] = Nil,
      supplied: List[lila.chessjudgment.model.judgment.SuppliedCommentaryLine] = Nil): CommentaryJobState =
    val history = CanonicalPositionHistory.from(initialFen, Nil, initialFen).toOption.get.extend(prefix).toOption.get
    val played = rows.head.head
    val start = CommentaryJobReducer.startJob(initialFen, prefix, history.currentFen, played,
      history.extend(List(played)).toOption.get.currentFen,
      CommentaryJobPolicy(16, 10000L, profile, investigateChoices = true, maximumLocalExpansions = limit), 0L,
      inputLines = supplied, proposals = proposals)
      .toOption.get.asInstanceOf[AwaitingEngineWork]
    val work = start.issuedWork
    assertEquals(rows.size, work.multiPv, "the synthetic receipt must cover the issued root search")
    CommentaryJobReducer.submitIssuedWorkReport(start, EngineWorkReport.Completed(profile, work.workId,
      work.executionKeySha256, rows.zipWithIndex.map { (moves, index) =>
        ReportedEngineLineSuffix(moves, ReportedWhiteEngineScore.Centipawns(30 - index * 10), 16)
      }), 1L).toOption.get

  private def matching(state: CommentaryJobState, kind: String, predicate: Predicate,
      alternative: List[String]): Entry =
    state.proposalEntries.find { entry =>
      entry.request.origin.question.producer == s"C-$kind" && entry.request.requirements.exists {
        case fact: Fact => fact.id == s"alternative_${alternative.last}" && fact.predicate == predicate &&
          fact.path.source == Declared(alternative)
        case _ => false
      }
    }.getOrElse(fail(s"missing $kind $predicate on $alternative; generated ${state.proposalEntries.size} entries"))

  private def values(entry: Entry, id: String): Set[String] =
    entry.readings.find(_.requirementId == id).toList.flatMap(_.result.toList.flatMap(_.samples.flatMap(_.value))).toSet

  test("a full supplied game retains its evidence beyond the finite discovery horizon"):
    import lila.chessjudgment.model.judgment.{SuppliedCommentaryLine, CommentaryLineSource}
    // Public Englisch-Steinitz 1883 mainline, not engine answers or explanation labels.
    val moves = ("e2e4 e7e5 g1f3 b8c6 f1b5 g7g6 d2d4 e5d4 f3d4 f8g7 c1e3 g8f6 b1c3 e8g8 e1g1 c6e7 " +
      "d1d2 d7d5 e4d5 e7d5 c3d5 d8d5 b5e2 f6g4 e2g4 c8g4 d4b3 d5d2 b3d2 a8d8 c2c3 f8e8 d2b3 b7b6 " +
      "h2h3 g4e6 f1d1 c7c5 e3g5 f7f6 g5f4 g8f7 f2f3 g6g5 d1d8 e8d8 f4e3 h7h6 a1e1 f6f5 f3f4 g7f6 " +
      "g2g3 a7a5 b3c1 a5a4 a2a3 e6c4 g1f2 g5f4 e3f4 f6g5 f4g5 h6g5 f2e3 f7f6 h3h4 g5h4 g3h4 d8e8 " +
      "e3f2 e8e1 f2e1 f6e5 c1e2 c4e2 e1e2 e5f4 c3c4 f4g4 e2e3 f5f4 e3e4 f4f3 e4e3 g4g3").split(" ").toList
    val state = begin(Nil, List(List("e2e4", "e7e5"), List("d2d4", "d7d5"), List("g1f3", "d7d5")),
      supplied = List(SuppliedCommentaryLine(CommentaryLineSource.PgnRecord, moves)))
    assert(state.proposalEntries.nonEmpty)
    assert(validRequests(state.proposalEntries.map(_.request)))
    assert(state.observationsPacket.get.assembly.input.suppliedLines.exists(_.acceptedMoves == moves))
    assert(state.discoveryState.scans.exists(scan => scan.state == "partial" && scan.reason.nonEmpty))

  test("an ordinary opening discovers the off-PV knight's support and the c-pawn passage it blocks"):
    val state = begin(opening, List(List("f1b5", "a7a6"), List("f1c4", "g8f6"), List("d2d4", "e5d4")))
    val support = matching(state, "support", Predicate("support", Some(Piece("white", "knight", "b1")),
      Some(Piece("white", "pawn", "e4"))), List("b1c3"))
    val passage = matching(state, "pawn_passage", Predicate("pawn_advance", Some(Piece("white", "pawn", "c2")),
      square = Some("c3")), List("b1c3"))
    assertEquals(values(support, "reference"), Set("absent"))
    assertEquals(values(support, "alternative_b1c3"), Set("present"))
    assertEquals(values(passage, "reference"), Set("present"))
    assertEquals(values(passage, "alternative_b1c3"), Set("absent"))
    assertEquals(support.admission, "generated")
    assertEquals(support.request.origin.question.kind, "generator")
    assertEquals(support.request.origin.candidates.kind, "generator")
    assert(List(support, passage).forall(_.request.requirements.forall(!_.isInstanceOf[CompareMoves])),
      "support and passage facts alone do not grade the moves")
    assert(state.proposalEntries.forall(entry => entry.request.requirements.collect { case value: CompareMoves => value }
      .forall(value => entry.request.origin.question.producer == "C-opponent-choice" && value.prefixMovesUci.isEmpty)),
      "only the separate current opposing-option questions add a comparison here")
    assert(state.localExpansionsUsed <= 600)

  test("support contacts distinguish a pinned attacker from a pinned provider and retain a replacement recapturer"):
    def fact(entry: Entry, predicate: Predicate, moves: List[String], ply: Int): Fact =
      entry.request.requirements.collectFirst {
        case fact: Fact if fact.predicate == predicate && fact.path.source == Declared(moves) && fact.at == Ply(ply) => fact
      }.getOrElse(fail(s"missing $predicate at $moves ply $ply"))
    def refutedDifference(entry: Entry, left: Fact, right: Fact): FactComparison =
      val comparison = entry.request.requirements.collectFirst {
        case value: FactComparison if value.leftRequirementId == left.id && value.rightRequirementId == right.id => value
      }.getOrElse(fail(s"missing recapture comparison ${left.id} / ${right.id}"))
      assertEquals(comparison, FactComparison(comparison.id, left.id, right.id, "different"))
      assertEquals(entry.readings.find(_.requirementId == comparison.id).get.state, "refuted")
      comparison

    val pinnedEnemy = "4k3/4n3/8/3P4/5N2/8/8/4R2K w - - 0 1"
    val knight = Piece("white", "knight", "f4")
    val dPawn = Piece("white", "pawn", "d5")
    val enemyKnight = Piece("black", "knight", "e7")
    val contact = begin(Nil, List(
      List("h1h2", "e8f8", "h2h1", "e7d5", "f4d5"), List("f4h3", "e8f8"),
      List("h1g1", "e8f8")), pinnedEnemy)
    val knightSupport = matching(contact, "support", Predicate("support", Some(knight), Some(dPawn)), List("f4h3"))
    assertEquals(values(knightSupport, "reference"), Set("present"))
    assertEquals(values(knightSupport, "alternative_f4h3"), Set("absent"))
    val pinnedCapture = Predicate("legal_capture", Some(enemyKnight), Some(dPawn))
    List("h1h2", "f4h3").foreach { move =>
      assertEquals(values(knightSupport, fact(knightSupport, pinnedCapture, List(move), 1).id), Set("illegal"))
    }
    val knightArm = knightSupport.generatedArms.find(_.alternativeMove.contains("f4h3")).get
    val directUses = knightSupport.request.requirements.collect {
      case value: Fact if value.predicate.kind == "direct_support_use" &&
        Set[PathSource](Declared(List("h1h2")), Declared(List("f4h3")))(value.path.source) => value
    }
    assert(directUses.nonEmpty, "the witnessed later use remains separate from the illegal immediate capture")
    assert((directUses.map(_.id) ++ List("h1h2", "f4h3").map(move =>
      fact(knightSupport, pinnedCapture, List(move), 1).id)).forall(knightArm.memberRequirementIds.contains),
      "an arm retains its immediate counterexample together with the later witnessed use")
    assert(!knightSupport.request.requirements.exists {
      case value: Fact => value.path.source match
        case Declared(moves) => Set(List("h1h2", "e7d5"), List("f4h3", "e7d5"))(moves)
        case _ => false
      case _ => false
    }, "a geometric attack must not declare the pinned knight's illegal immediate capture")

    val pinnedProvider = "k3r3/8/8/8/8/1n6/3PR2P/4K3 w - - 0 1"
    val rook = Piece("white", "rook", "e2")
    val pawn = Piece("white", "pawn", "d2")
    val capturer = Piece("black", "knight", "b3")
    val king = Piece("white", "king", "e1")
    val defense = begin(Nil, List(List("h2h3"), List("e2e3"), List("e2e4")), pinnedProvider)
    val rookSupport = matching(defense, "support", Predicate("support", Some(rook), Some(pawn)), List("e2e3"))
    assertEquals(values(rookSupport, "reference"), Set("present"))
    val capture = Predicate("legal_capture", Some(capturer), Some(pawn))
    val rookRecapture = Predicate("legal_capture", Some(rook), Some(capturer))
    val kingRecapture = Predicate("legal_capture", Some(king), Some(capturer))
    val referenceRook = fact(rookSupport, rookRecapture, List("h2h3", "b3d2"), 2)
    val referenceKing = fact(rookSupport, kingRecapture, List("h2h3", "b3d2"), 2)
    val supportPredicate = Predicate("support", Some(rook), Some(pawn))
    val beforeSupport = fact(rookSupport, supportPredicate, Nil, 0)
    val referenceSupport = fact(rookSupport, supportPredicate, List("h2h3"), 1)
    val referenceCapture = fact(rookSupport, capture, List("h2h3"), 1)
    assertEquals(values(rookSupport, referenceRook.id), Set("illegal"))
    assertEquals(values(rookSupport, referenceKing.id), Set("legal"))
    assertEquals(rookSupport.request.requirements.count(_.id == referenceRook.id), 1)
    List(beforeSupport, referenceSupport, referenceCapture, referenceKing).foreach { shared =>
      assertEquals(rookSupport.request.requirements.count(_.id == shared.id), 1)
    }
    List("h2h3", "e2e3", "e2e4").foreach { move =>
      assertEquals(values(rookSupport, fact(rookSupport, capture, List(move), 1).id), Set("legal"))
    }
    List("e2e3", "e2e4").foreach { move =>
      assertEquals(values(rookSupport, s"alternative_$move"), Set("absent"))
      val alternativeRook = fact(rookSupport, rookRecapture, List(move, "b3d2"), 2)
      val alternativeKing = fact(rookSupport, kingRecapture, List(move, "b3d2"), 2)
      assertEquals(values(rookSupport, alternativeRook.id), Set("illegal"))
      assertEquals(values(rookSupport, alternativeKing.id), Set("legal"))
      val rookDifference = refutedDifference(rookSupport, referenceRook, alternativeRook)
      val kingDifference = refutedDifference(rookSupport, referenceKing, alternativeKing)
      val alternativeSupport = fact(rookSupport, supportPredicate, List(move), 1)
      val alternativeCapture = fact(rookSupport, capture, List(move), 1)
      val head = rookSupport.request.requirements.collectFirst {
        case value: FactComparison if value.leftRequirementId == referenceSupport.id &&
          value.rightRequirementId == alternativeSupport.id => value
      }.get
      val arm = rookSupport.generatedArms.find(_.alternativeMove.contains(move)).get
      assertEquals(arm.prefixMoves, Nil)
      assertEquals(arm.referenceMove, "h2h3")
      assertEquals(arm.headRequirementIds, List(head.id))
      assertEquals(arm.source.kind, "generator")
      assertEquals(arm.source.version, Some(CommentaryDiscovery.GeneratorVersion))
      assert(arm.sourceRefs.nonEmpty)
      val contextIds = List(beforeSupport, referenceSupport, alternativeSupport, referenceCapture,
        alternativeCapture, referenceRook, alternativeRook, referenceKing, alternativeKing).map(_.id) ++
        List(head.id, rookDifference.id, kingDifference.id)
      assert(contextIds.forall(arm.memberRequirementIds.contains),
        "selecting the geometric contrast must retain the pinned provider and replacement counterexamples")
      assert(arm.memberRequirementIds.forall(id => rookSupport.request.requirements.exists(_.id == id)))
      assertEquals(arm.memberRequirementIds.distinct, arm.memberRequirementIds)
      val otherMove = if move == "e2e3" then "e2e4" else "e2e3"
      assert(!arm.memberRequirementIds.contains(fact(rookSupport, rookRecapture, List(otherMove, "b3d2"), 2).id),
        "another alternative's operational facts do not become this arm's context")
    }
    assert(rookSupport.request.requirements.collect { case fact: Fact => fact }.forall(_.expected.isEmpty))

  test("a witnessed recapture connects c3 to its later actor without claiming a necessary move order"):
    val state = begin(opening ++ List("f1c4", "f8c5"), List(
      List("c2c3", "g8f6", "d2d4", "e5d4", "c3d4"),
      List("d2d4", "e5d4", "c2c3", "d7d6", "e1g1", "g8f6", "c3d4"),
      List("b1c3", "g8f6")))
    val route = matching(state, "action_route", Predicate("control", Some(Piece("white", "pawn", "c2")),
      square = Some("d4")), List("d2d4"))
    assertEquals(values(route, "reference"), Set("present"))
    assertEquals(values(route, "alternative_d2d4"), Set("absent"))
    assertEquals(values(route, "reference_action"), Set("illegal"))
    assertEquals(values(route, "alternative_action_d2d4"), Set("illegal"))
    assertEquals(values(route, "reference_event_condition"), Set("present"))
    assertEquals(values(route, "alternative_event_condition_d2d4"), Set("present"))
    assertEquals(values(route, "reference_outcome"), Set("d4"))
    assertEquals(values(route, "alternative_outcome_d2d4"), Set("d4"))
    assertEquals(route.readings.find(_.requirementId == "reference_event_condition").get.result.get.samples.map(_.fromPly).toSet, Set(4))
    assertEquals(route.readings.find(_.requirementId == "alternative_event_condition_d2d4").get.result.get.samples.map(_.fromPly).toSet, Set(6))
    assert(route.request.requirements.collect { case fact: Fact => fact }.forall(_.expected.isEmpty))

  test("h3 discovers a legal bishop entry and the changed capture response even when Bg4 is absent from every PV"):
    val prefix = opening ++ List("f1b5", "a7a6", "b5a4", "g8f6", "e1g1", "f8e7",
      "f1e1", "b7b5", "a4b3", "d7d6", "c2c3", "e8g8")
    val state = begin(prefix, List(List("h2h3", "c6a5", "b3c2"), List("d2d4", "e5d4"), List("a2a4", "b5b4")))
    val predicate = Predicate("legal_capture", Some(Piece("white", "pawn", "h2")), Some(Piece("black", "bishop", "c8")))
    val entry = state.proposalEntries.find { entry =>
      entry.request.origin.question.producer == "C-opponent_entry" && entry.request.requirements.exists {
        case fact: Fact => fact.id == "reference_reply_target" &&
          fact.predicate == Predicate("location", Some(Piece("white", "knight", "f3")))
        case _ => false
      } && entry.request.requirements.exists {
        case fact: Fact => fact.id.startsWith("alternative_reply_") && fact.predicate == predicate &&
          (fact.path.source match
            case Declared(List(move, "c8g4")) => Set("d2d4", "a2a4")(move)
            case _ => false)
        case _ => false
      }
    }.getOrElse(fail(s"missing Bg4 reply contrast; requirements=${state.proposalEntries.map(_.request.requirements.size).sum}, " +
      s"local=${state.localExpansionsUsed}, scans=${state.discoveryState.scans.map(scan => (scan.reason, scan.deferredRequestIds.size))}, " +
      s"kinds=${state.proposalEntries.groupMapReduce(_.request.origin.question.producer)(_ => 1)(_ + _)}"))
    assertEquals(values(entry, "reference_reply"), Set("legal"))
    assert(entry.request.requirements.exists {
      case fact: Fact => fact.id == "reference_reply_target" &&
        fact.predicate == Predicate("location", Some(Piece("white", "knight", "f3")))
      case _ => false
    })
    assertEquals(values(entry, "reference_reply_target"), Set("f3"))
    assertEquals(values(entry, "reference_reply_pressure_before"), Set("absent"))
    assertEquals(values(entry, "reference_reply_pressure_after"), Set("present"))
    val alternative = entry.request.requirements.collectFirst {
      case fact: Fact if fact.id.startsWith("alternative_reply_") => fact.id
    }.get
    assertEquals(values(entry, alternative), Set("illegal"))
    assertEquals(entry.admission, "generated")

  test("both legal king retreats can open the same pawn passage without turning that equality into equal evaluations"):
    val fen = "8/8/8/3k4/3p4/3K4/3P4/8 w - - 0 1"
    val state = begin(Nil, List(List("d3c2", "d5c4"), List("d3e2", "d5e4")), fen)
    val passage = matching(state, "pawn_passage", Predicate("pawn_advance", Some(Piece("white", "pawn", "d2")),
      square = Some("d3")), List("d3e2"))
    assertEquals(values(passage, "before"), Set("absent"))
    assertEquals(values(passage, "reference"), Set("present"))
    assertEquals(values(passage, "alternative_d3e2"), Set("present"))
    assertEquals(passage.readings.find(_.requirementId == "contrast_d3e2").get.state, "supported")
    assert(passage.request.requirements.contains(FactComparison("contrast_d3e2", "reference", "alternative_d3e2", "same")))

  test("a pawn move can remove a king entry entirely rather than merely change a capture response"):
    val fen = "8/8/8/7k/8/8/4K2P/8 w - - 0 1"
    val state = begin(Nil, List(List("h2h3", "h5h6"), List("h2h4", "h5g4"), List("e2e3", "h5g4")), fen)
    val predicate = Predicate("legal_move", Some(Piece("black", "king", "h5")), square = Some("g4"))
    val entry = state.proposalEntries.find(entry => entry.request.origin.question.producer == "C-opponent_access" &&
      entry.request.requirements.exists {
        case fact: Fact => fact.id == "alternative_access_h2h4" && fact.predicate == predicate && fact.path.source == Declared(List("h2h4"))
        case _ => false
      }).getOrElse(fail("missing the changed legality of Kg4"))
    assertEquals(values(entry, "reference_access"), Set("illegal"))
    assertEquals(values(entry, "alternative_access_h2h4"), Set("legal"))

  test("accepted child evidence rereads a generated pawn question without replacing its conditions or other frontiers"):
    val fen = "7k/8/8/8/8/8/2P5/KN6 w - - 0 1"
    val rows = List(List("b1c3", "h8h7"), List("b1a3", "h8h7"), List("b1d2"))
    val pawn = Piece("white", "pawn", "c2")
    val passage = Predicate("pawn_advance", Some(pawn), square = Some("c3"))
    val advance = Predicate("legal_move", Some(pawn), square = Some("c3"))
    val closed = begin(Nil, rows, fen)
    assert(closed.isInstanceOf[CompletedCommentaryJob], "settled passage contrasts do not justify every optional continuation")
    val followup = closed.proposalEntries.filter(_.request.origin.question.producer == "C-pawn_passage")
      .flatMap(entry => entry.request.requirements.collect {
        case value: Fact if value.predicate == advance && entry.readings.exists(reading =>
          reading.requirementId == value.id && reading.stopReason.contains("not_required_for_decision") &&
            reading.result.exists(_.needs.exists(_.kind == "continuation"))) => value
      }).head
    val source = Source("user", "explicit-continuation-test")
    val request = Request("continue-use", Origin(source, source, source, source), List(followup.copy(id = "use")))
    val pending = begin(Nil, rows, fen, proposals = List(request)) match
      case state: AwaitingEngineWork => state
      case state => fail(s"expected a generated fact frontier, got ${state.getClass.getSimpleName}")
    val work = pending.issuedWork
    assertEquals(work.purpose, EngineWorkPurpose.ResponseComparison)
    val prefix = work.searchAnchor.get.prefixMoves
    val (prior, fact) = pending.proposalEntries.iterator.filter(entry => entry.admission == "generated" &&
      entry.request.origin.question.producer == "C-pawn_passage" &&
      entry.request.requirements.exists {
        case value: Fact => value.predicate == passage
        case _ => false
      }).flatMap { entry =>
      entry.request.requirements.collect {
        case value: Fact if value.predicate == advance && entry.readings.exists(reading =>
            reading.requirementId == value.id && reading.result.exists(_.needs.exists(need =>
              need.kind == "continuation" && need.prefixMoves == prefix))) => entry -> value
      }
    }.nextOption().getOrElse(fail(s"issued frontier $prefix must belong to the generated original c2-pawn question"))
    val earlier = prior.readings.find(_.requirementId == fact.id).get.result.get
    assertEquals(earlier.state, "partial")
    val known = prior.readings.flatMap(reading => reading.result.toList.flatMap(_.samples)
      .filter(_.value.nonEmpty).map(reading.requirementId -> _))
    assert(known.nonEmpty)
    val otherNeeds = prior.readings.flatMap(_.result.toList.flatMap(_.needs)).filter(need =>
      need.kind == "continuation" && need.prefixMoves != prefix).distinct
    assert(otherNeeds.nonEmpty, "one issued response must not stand in for every alternative's reply")

    // The report uses only canonical legal replies to the actually issued position, without an engine or injected readings.
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get.extend(prefix).toOption.get
    val replies = history.currentLegalMoveUcis.take(work.multiPv)
    val after = CommentaryJobReducer.submitIssuedWorkReport(pending, EngineWorkReport.Completed(profile,
      work.workId, work.executionKeySha256, replies.zipWithIndex.map { (move, index) =>
        ReportedEngineLineSuffix(List(move), ReportedWhiteEngineScore.Centipawns(index * 10), 16)
      }), 2L).toOption.get
    assertNotEquals(after.discoveryState.inputKey, pending.discoveryState.inputKey)
    assert(after.responseSearches.exists(_.work.workId == work.workId))
    assert(after.discoveryState.scans.exists(_.anchor.prefixMoves == prefix))
    val retained = after.proposalEntries.find(_.request.id == prior.request.id).get
    val requirements = retained.request.requirements.map(value => value.id -> value).toMap
    prior.request.requirements.foreach(value => assertEquals(requirements.get(value.id), Some(value)))
    known.foreach { (requirementId, sample) =>
      assert(retained.readings.find(_.requirementId == requirementId).get.result.get.samples.contains(sample),
        "new evidence must preserve the earlier exact sample alongside the same question")
    }
    val resolved = retained.readings.find(_.requirementId == fact.id).get.result.get
    assertEquals(resolved.state, "observed")
    assertEquals(resolved.needs, Nil)
    assertEquals(resolved.samples.flatMap(_.value).toSet, Set("legal"))
    val acceptedPaths = replies.map(prefix :+ _).toSet
    assert(resolved.samples.exists(sample => acceptedPaths(sample.moves)))
    assert(resolved.proofs.exists(_.bindings.values.exists(acceptedPaths)))
    val remaining = retained.readings.flatMap(_.result.toList.flatMap(_.needs))
    assert(otherNeeds.forall(remaining.contains), "a returned reply only settles its own exact branch")
