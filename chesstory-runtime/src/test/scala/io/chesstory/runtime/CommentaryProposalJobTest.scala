package io.chesstory.runtime

import lila.chessjudgment.model.line.CanonicalPositionHistory
import lila.chessjudgment.model.judgment.{ CommentaryLineSource, SuppliedCommentaryLine }
import play.api.libs.json.*

/** Supplied hypotheses and invented reports test ownership, not chess strength or discovery. */
class CommentaryProposalJobTest extends munit.FunSuite:
  import CommentaryProposal.*

  private val initial = chess.variant.Standard.initialFen.value
  private val owner = CanonicalPositionHistory.from(initial, Nil, initial).toOption.get
  private val profile = CommentaryEngineProfile.ServerAdmitted
  private val origin = Origin(
    Source("user", "proposal-question-test"),
    Source("user", "proposal-candidate-test"),
    Source("none", "proposal-priority-test"),
    Source("none", "proposal-evidence-test"))
  private val longPv = List("e2e4", "e7e5", "g1f3", "b8c6")
  private val rootRows = List(longPv -> 30, List("d2d4", "d7d5") -> 20,
    List("g1f3", "d7d5") -> 10)

  private def request(id: String, requirements: Requirement*): Request =
    Request(id, origin, requirements.toList)

  private def start(requests: List[Request], limit: Int = 600): AwaitingEngineWork =
    CommentaryJobReducer.startJob(initial, Nil, initial, "d2d4",
      owner.extend(List("d2d4")).toOption.get.currentFen,
      // This suite isolates submitted requirements; automatic discovery has its own integration cases.
      CommentaryJobPolicy(16, 10000L, profile, investigateChoices = false,
        maximumLocalExpansions = limit), 0L, proposals = requests).toOption.get
      .asInstanceOf[AwaitingEngineWork]

  private def submit(state: AwaitingEngineWork, rows: List[(List[String], Int)]): CommentaryJobState =
    val work = state.issuedWork
    CommentaryJobReducer.submitIssuedWorkReport(state, EngineWorkReport.Completed(profile,
      work.workId, work.executionKeySha256, rows.map { (moves, cp) =>
        ReportedEngineLineSuffix(moves, ReportedWhiteEngineScore.Centipawns(cp), 16)
      }), 1L).toOption.get

  private def completed(state: CommentaryJobState): CompletedCommentaryJob = state match
    case done: CompletedCommentaryJob => done
    case other => fail(s"expected a completed job, got ${other.getClass.getSimpleName}")

  private def awaiting(state: CommentaryJobState): AwaitingEngineWork = state match
    case active: AwaitingEngineWork => active
    case other => fail(s"expected an outstanding proposal work, got ${other.getClass.getSimpleName}")

  private def reading(state: CommentaryJobState, requestId: String, requirementId: String): Reading =
    state.proposalEntries.find(_.request.id == requestId).get.readings
      .find(_.requirementId == requirementId).get

  private def review(state: CompletedCommentaryJob): CompletedMoveReview = state.result match
    case CompletedPositionCommentary.FocusedMoveReview(_, value) => value
    case _ => fail("expected the preserved root review")

  private def rootDecision(state: CompletedCommentaryJob): JsObject =
    val record = review(state)
    val selected = (RuntimeProtocol.completedResultJson(state) \ "selected_move_reviews").as[List[JsObject]]
    val played = selected.find(value => (value \ "move_uci").as[String] == record.moveUci).get
    Json.obj("move_uci" -> record.moveUci, "best_move_uci" -> record.bestMoveUci,
      "legal_move_index" -> record.legalMoveIndex, "best_legal_move_index" -> record.bestLegalMoveIndex,
      "played_root_rank" -> record.playedRootRank, "best_root_rank" -> record.bestRootRank,
      "primary" -> (played \ "commentary" \ "primary").as[JsObject])

  test("an interior move of a longer certified PV reuses its exact occurrence with no expansion"):
    val supplied = request("interior", ObserveMove("move", List("e2e4", "e7e5"), "g1f3"),
      Uninterpreted("meaning", "This maneuver has lasting positional value."))
    val done = completed(submit(start(List(supplied), limit = 0), rootRows))
    val observed = reading(done, "interior", "move")
    assertEquals(observed.state, "observed")
    assertEquals(done.localExpansionsUsed, 0)
    assertEquals(done.progress.physicalWorksIssued, 1)
    val packet = done.observationsPacket.get
    val path = ObservedLineProtocol.prepare(packet).path(packet, observed.lineId.get).get
    assertEquals(path.moves, longPv)
    assertEquals(observed.occurrenceId, Some(path.ids(2)))
    assert(observed.positionId.nonEmpty)
    assertEquals(observed.answer, None)
    assertEquals(reading(done, "interior", "meaning").state, "unsupported_reading")
    assertEquals(done.proposalEntries.head.request, supplied)

  test("a pending reply's conditions stay in its question without preceding the root comparison's observations"):
    import CommentaryFact.{ Declared, Path, Piece, Ply, Predicate }
    val supplied = request("presentation", CompareMoves("contrast_opponent_choice", Nil, "e2e4", "d2d4"))
    val started = CommentaryJobReducer.startJob(initial, Nil, initial, "d2d4",
      owner.extend(List("d2d4")).toOption.get.currentFen,
      CommentaryJobPolicy(16, 10000L, profile, investigateChoices = false), 0L,
      inputLines = List(SuppliedCommentaryLine(CommentaryLineSource.Study, List("e2e4", "c7c5"))),
      proposals = List(supplied)).toOption.get.asInstanceOf[AwaitingEngineWork]
    val done = completed(submit(started, rootRows))
    val packet = done.observationsPacket.get
    val prepared = ObservedLineProtocol.prepare(packet)
    val base = done.proposalEntries.head
    val pawn = Piece("black", "pawn", "e7")
    val reference = Path("reply-reference", Declared(List("e2e4", "e7e5")))
    val alternative = Path("reply-alternative", Declared(List("e2e4", "c7c5")))
    val extra = List(
      CompareMoves("tactical_strength", List("e2e4"), "e7e5", "c7c5"),
      Fact("tactical_effect_pawn_reference", reference, Predicate("location", subject = Some(pawn)), Ply(2)),
      Fact("tactical_effect_pawn_alternative", alternative, Predicate("location", subject = Some(pawn)), Ply(2)),
      Fact("reply_present", reference, Predicate("presence", subject = Some(pawn)), Ply(1), expected = Some("present")),
      FactGroup("tactical_context", "all", List("reply_present")))
    val entry = base.copy(request = base.request.copy(requirements = base.request.requirements ++ extra),
      readings = base.readings ++ extra.map(requirement => Reading(requirement.id)),
      generatedArms = List(
        GeneratedArm("root", Nil, "e2e4", Some("d2d4"), List("contrast_opponent_choice"),
          List("contrast_opponent_choice"), origin.question, Nil),
        GeneratedArm("reply", List("e2e4"), "e7e5", Some("c7c5"), List("tactical_context", "tactical_strength"),
          extra.map(_.id), origin.question, Nil)),
      questionText = Some("After e4, how does c5 compare with e5?"))
    val observed = CommentaryFactReader.read(packet, prepared, List(entry)).head
    assertEquals(observed.readings.find(_.requirementId == "tactical_context").get.state, "supported")
    val candidates = comparisonCandidates(packet, prepared, List(observed))
    val main = candidates.find(value => (value \ "detail" \ "prefix_moves_uci").as[List[String]].isEmpty).get
    assertEquals((main \ "selection").as[String], "selected")
    assertEquals((main \ "detail" \ "evaluation_relation").as[String], "worse")
    val mainText = (main \ "detail" \ "text").as[String]
    assert(!mainText.contains("check c7c5 instead of e7e5"), mainText)
    assert(!mainText.contains("Conditions:"), mainText)
    val pending = candidates.find(value => (value \ "detail" \ "prefix_moves_uci").as[List[String]] == List("e2e4")).get
    assertEquals((pending \ "detail" \ "evaluation_relation").get, JsNull)
    assert((pending \ "detail" \ "text").as[String].contains("remains unverified"))
    assertEquals((pending \ "detail" \ "question_refs").as[List[JsObject]].map(value =>
      (value \ "requirement_id").as[String]), List("tactical_strength"))
    val retained = snapshot(observed)
    val conditions = (retained \ "requirements").as[List[JsObject]]
    val unresolved = conditions.find(value => (value \ "id").as[String] == "tactical_strength").get
    assertEquals((unresolved \ "state").as[String], "deferred")
    assertEquals((unresolved \ "answer").get, JsNull)
    assertEquals((retained \ "question_text").as[String], entry.questionText.get)
    List("reference" -> "e5", "alternative" -> "e7").foreach { (arm, square) =>
      val condition = conditions.find(value => (value \ "id").as[String] == s"tactical_effect_pawn_$arm").get
      assertEquals((condition \ "result" \ "samples").as[List[JsObject]].flatMap(value =>
        (value \ "value").asOpt[String]).distinct, List(square))
    }
    assertEquals(done.progress.physicalWorksIssued, 1)
    assertEquals(done.progress.physicalReportsAccepted, 1)

  test("unanswered comparison aliases with different stop reasons retain questions without a mixed card"):
    val supplied = request("first", CompareMoves("pair", Nil, "e2e4", "d2d4"))
    val done = completed(submit(start(List(supplied)), rootRows))
    val packet = done.observationsPacket.get
    val prepared = ObservedLineProtocol.prepare(packet)
    val first = done.proposalEntries.head.copy(
      readings = List(Reading("pair", "stopped", Some("investigation_limit"))),
      generatedArms = List(GeneratedArm("root", Nil, "e2e4", Some("d2d4"), List("pair"), List("pair"),
        origin.question, Nil)))
    val second = first.copy(request = first.request.copy(id = "second"),
      readings = List(Reading("pair", "stopped", Some("not_required_for_decision"))))
    assertEquals(comparisonCandidates(packet, prepared, List(first, second)), Nil)
    assertEquals(List(first, second).map(entry =>
      (snapshot(entry) \ "requirements").as[List[JsObject]].head.value("stop_reason").as[String]),
      List("investigation_limit", "not_required_for_decision"))
    val sameReason = second.copy(readings = first.readings)
    val merged = comparisonCandidates(packet, prepared, List(first, sameReason))
    assertEquals(merged.size, 1)
    assertEquals((merged.head \ "detail" \ "question_refs").as[List[JsObject]].size, 2)

  test("selected root pairs publish an identical answered reply bundle once without merging its evidence"):
    import CommentaryFact.{ Declared, Path, Piece, Ply, Predicate }
    val roots = List(
      request("first-owner", CompareMoves("contrast_opponent_choice", Nil, "e2e4", "d2d4")),
      request("second-owner", CompareMoves("contrast_opponent_choice", Nil, "e2e4", "g1f3")))
    val reply = request("shared-reply", CompareMoves("pair", List("e2e4"), "e7e5", "c7c5"))
    val active = awaiting(submit(start(roots :+ reply), rootRows))
    val done = completed(submit(active, List(
      List("e7e5", "g1f3", "b8c6", "d2d4", "e5d4", "f3d4") -> 20,
      List("c7c5", "g1f3", "d7d6", "d2d4", "c5d4", "f3d4") -> 10)))
    val packet = done.observationsPacket.get
    val prepared = ObservedLineProtocol.prepare(packet)
    val replyReading = reading(done, "shared-reply", "pair").copy(requirementId = "tactical_strength")
    val reference = Path("reply-reference", Declared(List("e2e4", "e7e5")))
    val alternative = Path("reply-alternative", Declared(List("e2e4", "c7c5")))
    val attacker = Piece("black", "pawn", "e7")
    def withCounter(base: Entry, target: Piece): Entry =
      val extra = List(
        Fact("tactical_current", reference, Predicate("presence", subject = Some(attacker)), Ply(1), expected = Some("present")),
        FactGroup("tactical_context", "all", List("tactical_current")),
        CompareMoves("tactical_strength", List("e2e4"), "e7e5", "c7c5"),
        Fact("tactical_effect_pawn_reference", reference, Predicate("location", subject = Some(attacker)), Ply(2)),
        Fact("tactical_effect_pawn_alternative", alternative, Predicate("location", subject = Some(attacker)), Ply(2)),
        Fact("tactical_direct_capture_reference", reference,
          Predicate("legal_capture", subject = Some(attacker), target = Some(target)), Ply(2)),
        Fact("tactical_direct_capture_alternative", alternative,
          Predicate("legal_capture", subject = Some(attacker), target = Some(target)), Ply(2)))
      base.copy(request = base.request.copy(requirements = base.request.requirements ++ extra),
        readings = base.readings ++ extra.map(requirement =>
          if requirement.id == "tactical_strength" then replyReading else Reading(requirement.id)),
        generatedArms = List(
          GeneratedArm("root", Nil, "e2e4", Some(base.request.requirements.head.asInstanceOf[CompareMoves].alternativeMove),
            List("contrast_opponent_choice"), List("contrast_opponent_choice"), origin.question, Nil),
          GeneratedArm("reply", List("e2e4"), "e7e5", Some("c7c5"), List("tactical_context", "tactical_strength"),
            extra.map(_.id), origin.question, Nil)), questionText = Some("After e4, compare c5 with e5."))
    val bases = roots.map(root => done.proposalEntries.find(_.request.id == root.id).get)
    def observed(target: Piece) = CommentaryFactReader.read(packet, prepared,
      List(withCounter(bases.head, Piece("white", "pawn", "e2")), withCounter(bases(1), target)))
    val entries = observed(Piece("white", "pawn", "e2"))
    val snapshots = entries.map(entry => snapshot(entry))
    def selected(entries: List[Entry]) = comparisonCandidates(packet, prepared, entries).filter(candidate =>
      (candidate \ "selection").as[String] == "selected" &&
        (candidate \ "detail" \ "prefix_moves_uci").as[List[String]].isEmpty)
    def texts(entries: List[Entry]) = selected(entries).map(candidate => (candidate \ "detail" \ "text").as[String])
    val marker = "After e4, check c5 instead of e5."
    val candidates = selected(entries)
    assertEquals(candidates.size, 2)
    assertEquals(candidates.map(candidate => (candidate \ "detail" \ "alternative_move").as[String]).toSet,
      Set("d2d4", "g1f3"))
    val published = texts(entries)
    assertEquals(published.count(_.contains(marker)), 1)
    val bundle = published.find(_.contains(marker)).get
    assert(bundle.contains("Conditions:"), bundle)
    assert(bundle.contains("beginning e5"), bundle)
    assert(bundle.contains("beginning c5"), bundle)
    assert(bundle.contains("black loses 1 pawn; white loses 1 pawn"), bundle)
    assertEquals(entries.map(entry => snapshot(entry)), snapshots)
    entries.foreach { entry =>
      assertEquals(entry.readings.find(_.requirementId == "tactical_strength").get, replyReading)
      assertEquals(entry.readings.find(_.requirementId == "tactical_context").get.state, "supported")
    }
    val sampleIds = entries.map(_.readings.find(_.requirementId == "tactical_current").get.result.get.samples.map(_.id))
    assertNotEquals(sampleIds.head, sampleIds(1)) // Proposal ownership stays distinct in the public facts.
    assertEquals(selected(entries.reverse), candidates)

    // A different physical victim is another explanation, even with the same engine receipt.
    val differentTarget = texts(observed(Piece("white", "pawn", "d2")))
    assertEquals(differentTarget.count(_.contains(marker)), 2)
    assert(differentTarget.exists(_.contains("white pawn originally on e2 remains on the board")))
    assert(differentTarget.exists(_.contains("white pawn originally on d2 is captured")))

    // Presentation sharing must not cross a receipt, verification depth or returned line.
    val answer = replyReading.answer.get
    val otherLine = prepared.paths.find(_.moves == longPv).get
    List(answer.copy(workId = "different-work"), answer.copy(executionKeySha256 = "different-execution"),
      answer.copy(requiredDepth = answer.requiredDepth + 1),
      answer.copy(referenceLineId = otherLine.line.ref.id, referenceOccurrenceId = otherLine.ids(1))).foreach { changed =>
      val second = entries(1).copy(readings = entries(1).readings.map(value =>
        if value.requirementId == "tactical_strength" then value.copy(answer = Some(changed)) else value))
      assertEquals(texts(List(entries.head, second)).count(_.contains(marker)), 2)
    }
    assertEquals(done.progress.physicalWorksIssued, 2)
    assertEquals(done.progress.physicalReportsAccepted, 2)

  test("a transposed board cannot borrow a different history's free observation"):
    val observedOrder = List("g1f3", "g8f6", "b1c3", "b8c6")
    val requestedOrder = List("b1c3", "b8c6", "g1f3", "g8f6")
    assertEquals(owner.extend(observedOrder).toOption.get.currentFen,
      owner.extend(requestedOrder).toOption.get.currentFen)
    val supplied = request("transposition", ObserveMove("move", requestedOrder, "d2d3"))
    val rows = List(List("e2e4") -> 30, List("d2d4") -> 20, (observedOrder :+ "d2d3") -> 10)
    val done = completed(submit(start(List(supplied), limit = 0), rows))
    val missing = reading(done, "transposition", "move")
    assertEquals(missing.state, "stopped")
    assertEquals(missing.stopReason, Some("local_expansion_limit"))
    assertEquals(missing.lineId, None)
    assertEquals(missing.occurrenceId, None)
    assertEquals(missing.answer, None)
    assertEquals(done.localExpansionsUsed, 0)
    assertEquals(done.progress.physicalWorksIssued, 1)

  test("an illegal prefix and illegal candidate are invalid without poisoning other requirements"):
    val supplied = request("invalid", ObserveMove("prefix", List("e2e4", "g1f3"), "b8c6"),
      CompareMoves("candidate", Nil, "e2e4", "e2e5"), ObserveMove("valid", Nil, "e2e4"))
    val done = completed(submit(start(List(supplied)), rootRows))
    assertEquals(reading(done, "invalid", "prefix").state, "invalid_input")
    assertEquals(reading(done, "invalid", "candidate").state, "invalid_input")
    assertEquals(reading(done, "invalid", "valid").state, "observed")
    assertEquals(reading(done, "invalid", "prefix").answer, None)
    assertEquals(reading(done, "invalid", "candidate").answer, None)
    assertEquals(review(done).bestMoveUci, "e2e4")
    assertEquals(done.progress.physicalWorksIssued, 1)

  test("arbitrary child pairs use the actual mover and exact ply offset at both parities"):
    val cases = List(
      (List("e2e4"), "e7e5", "c7c5", "better"),
      (List("e2e4", "e7e5"), "g1f3", "b1c3", "worse"))
    cases.foreach { (prefix, reference, alternative, expected) =>
      val supplied = request("child", CompareMoves("pair", prefix, reference, alternative))
      val active = awaiting(submit(start(List(supplied), limit = 0), rootRows))
      val baseline = rootDecision(active.assessment.get)
      val work = active.issuedWork
      assertEquals(work.purpose, EngineWorkPurpose.ResponseComparison)
      assertEquals(work.enginePositionInitialFen, initial)
      assertEquals(work.enginePositionMovesUci, prefix)
      assertEquals(work.searchFen, owner.extend(prefix).toOption.get.currentFen)
      assertEquals(work.searchAnchor.get.prefixMoves, prefix)
      assertEquals(work.searchAnchor.get.plyOffset, prefix.size)
      val done = completed(submit(active, List(List(reference) -> 80, List(alternative) -> 20)))
      val result = reading(done, "child", "pair")
      assertEquals(result.state, "answered")
      val answer = result.answer.get
      assertEquals(answer.evaluationRelation, expected)
      assertEquals(answer.workId, work.workId)
      val packet = done.observationsPacket.get
      val prepared = ObservedLineProtocol.prepare(packet)
      val referencePath = prepared.path(packet, answer.referenceLineId).get
      val alternativePath = prepared.path(packet, answer.alternativeLineId).get
      assertEquals(referencePath.moves.take(prefix.size + 1), prefix :+ reference)
      assertEquals(alternativePath.moves.take(prefix.size + 1), prefix :+ alternative)
      assertEquals(answer.referenceOccurrenceId, referencePath.ids(prefix.size))
      assertEquals(answer.alternativeOccurrenceId, alternativePath.ids(prefix.size))
      assertEquals((answer.referenceEvaluation \ "source").as[String], "response_comparison")
      assertEquals(rootDecision(done), baseline)
      assertEquals(done.assessmentNeedsRefresh, false)
      assertEquals(done.localExpansionsUsed, 0)
      assertEquals(done.progress.physicalWorksIssued, 2)
    }

  test("a grouped child work retains its issued question owners after deeper verification retires its answers"):
    val requests = List(
      request("child-c5", CompareMoves("tactical_strength", List("e2e4"), "e7e5", "c7c5")),
      request("child-e6", CompareMoves("tactical_strength", List("e2e4"), "e7e5", "e7e6")))
    val started = CommentaryJobReducer.startJob(initial, Nil, initial, "d2d4",
      owner.extend(List("d2d4")).toOption.get.currentFen,
      CommentaryJobPolicy(16, 10000L, profile, previewDepth = Some(8)), 0L,
      proposals = requests).toOption.get.asInstanceOf[AwaitingEngineWork]
    val active = awaiting(submit(awaiting(submit(started, rootRows)), rootRows))
    val work = active.issuedWork
    assertEquals(work.rootRestriction, EngineRootRestriction.Restricted(List("e7e5", "c7c5", "e7e6")))
    assertEquals(work.proposalRefs, List("child-c5" -> "tactical_strength", "child-e6" -> "tactical_strength"))
    val verify = awaiting(submit(active, List(List("e7e5") -> -400, List("c7c5") -> -600, List("e7e6") -> -500)))
    assertEquals(verify.issuedWork.purpose, EngineWorkPurpose.RootSearch)
    assertEquals(verify.issuedWork.requiredDepth, 17)
    val done = CommentaryJobReducer.cancelJob(verify).asInstanceOf[CompletedCommentaryJob]
    assert(done.responseSearches.find(_.work.workId == work.workId).exists(_.work eq work))
    requests.foreach { request =>
      val retained = reading(done, request.id, "tactical_strength")
      assertEquals(retained.state, "stopped")
      assertEquals(retained.answer, None)
      assertEquals(retained.workId, None)
    }
    val wire = RuntimeProtocol.encodePositionCommentaryResponse(
      CommentaryJobSnapshot("1" * 32, "retired-child-owners", 10000L, done), done)
    val search = (wire \ "observations" \ "selection" \ "response_searches").as[List[JsObject]]
      .find(value => (value \ "work_id").as[String] == work.workId).get
    val ownership = (search \ "proposal_ownership").as[JsObject]
    assertEquals((ownership \ "work_id").as[String], work.workId)
    assertEquals((ownership \ "execution_key_sha256").as[String], work.executionKeySha256)
    assertEquals((ownership \ "question_refs").as[List[JsObject]].map(value =>
      (value \ "proposal_id").as[String] -> (value \ "requirement_id").as[String]), work.proposalRefs)

  test("duplicate root comparisons share an answer and verify new candidates with actual and the old reference"):
    val positive = request("positive", CompareMoves("relation", Nil, "c2c4", "g2g3", Some("better")))
    val negative = request("negative", CompareMoves("relation", Nil, "c2c4", "g2g3", Some("worse")),
      Uninterpreted("meaning", "The higher score proves the king is safer."))
    val active = awaiting(submit(start(List(positive, negative)), rootRows))
    val baseline = rootDecision(active.assessment.get)
    val scope = active.assessment.get.assessmentScope
    val work = active.issuedWork
    assertEquals(work.enginePositionMovesUci, Nil)
    assertEquals(work.rootRestriction, EngineRootRestriction.Restricted(List("c2c4", "g2g3")))
    val verify = awaiting(submit(active, List(List("c2c4") -> 800, List("g2g3") -> 900)))
    assertEquals(rootDecision(verify.assessment.get), baseline)
    assertEquals(verify.assessmentNeedsRefresh, true)
    assertEquals(verify.issuedWork.rootRestriction,
      EngineRootRestriction.Restricted(List("e2e4", "d2d4", "g2g3")))
    val cancelled = CommentaryJobReducer.cancelJob(verify).asInstanceOf[CompletedCommentaryJob]
    assertEquals(rootDecision(cancelled), baseline)
    assertEquals(cancelled.assessmentNeedsRefresh, true)
    val stoppedWire = RuntimeProtocol.encodePositionCommentaryResponse(
      CommentaryJobSnapshot("1" * 32, "cancelled-root-proposal", 10000L, cancelled), cancelled)
    assertEquals((stoppedWire \ "assessment_state").as[String], "needs_refresh")
    val done = completed(submit(verify, List(List("e2e4") -> 950, List("d2d4") -> 20, List("g2g3") -> 900)))
    val supported = reading(done, "positive", "relation")
    val refuted = reading(done, "negative", "relation")
    assertEquals(supported.state, "supported")
    assertEquals(refuted.state, "refuted")
    assertEquals(supported.answer.get.evaluationRelation, "better")
    assertEquals(supported.answer, refuted.answer)
    assertEquals(supported.workId, refuted.workId)
    assertEquals(supported.answer.get.workId, work.workId)
    assertEquals(reading(done, "negative", "meaning").state, "unsupported_reading")
    assertEquals(reading(done, "negative", "meaning").answer, None)
    assertEquals(done.proposalEntries.map(_.request), List(positive, negative))
    assertEquals(done.progress.physicalWorksIssued, 3)
    assertEquals(done.progress.physicalReportsAccepted, 3)
    assertEquals(review(done).bestMoveUci, "e2e4")
    assertNotEquals(rootDecision(done), baseline)
    assertEquals(scope, "root_search")
    assertEquals(done.assessmentScope, "investigation_pair")
    assertEquals(done.assessmentNeedsRefresh, false)
    val wire = RuntimeProtocol.encodePositionCommentaryResponse(
      CommentaryJobSnapshot("1" * 32, "refreshed-root-proposal", 10000L, done), done)
    assertEquals((wire \ "assessment_scope").as[String], "investigation_pair")
    val search = done.rootSearches.find(_.work.workId == work.workId).get
    assert(search.work eq work)
    assert(search.receipt.get.work eq work)

  test("duplicate replays share one local unit and exhausted capacity still permits existing evidence"):
    val requests = List(
      request("first", ObserveMove("move", Nil, "g2g3")),
      request("duplicate", ObserveMove("move", Nil, "g2g3")),
      request("other", ObserveMove("move", Nil, "b2b3")),
      request("existing", ObserveMove("move", Nil, "e2e4")))
    val done = completed(submit(start(requests, limit = 1), rootRows))
    val first = reading(done, "first", "move")
    val duplicate = reading(done, "duplicate", "move")
    val other = reading(done, "other", "move")
    assertEquals(first, duplicate)
    assertEquals(List(first, other).count(_.state == "observed"), 1)
    val blocked = List(first, other).find(_.state != "observed").get
    assertEquals(blocked.state, "stopped")
    assertEquals(blocked.stopReason, Some("local_expansion_limit"))
    assertEquals(blocked.lineId, None)
    assertEquals(blocked.answer, None)
    assertEquals(reading(done, "existing", "move").state, "observed")
    assertEquals(done.localExpansionsUsed, 1)
    assertEquals(done.progress.physicalWorksIssued, 1)
    assertEquals(done.proposalEntries.map(_.request), requests)

  test("new prefix admission charges each missing edge before its requested observation"):
    val prefix = List("g2g3", "g7g6")
    val supplied = request("new-prefix", ObserveMove("move", prefix, "f1g2"))
    val blocked = completed(submit(start(List(supplied), limit = 2), rootRows))
    assertEquals(blocked.localExpansionsUsed, 2)
    assertEquals(reading(blocked, "new-prefix", "move").state, "stopped")
    assertEquals(reading(blocked, "new-prefix", "move").stopReason, Some("local_expansion_limit"))
    assertEquals(reading(blocked, "new-prefix", "move").lineId, None)
    val done = completed(submit(start(List(supplied), limit = 3), rootRows))
    val observed = reading(done, "new-prefix", "move")
    assertEquals(observed.state, "observed")
    assertEquals(done.localExpansionsUsed, 3)
    val packet = done.observationsPacket.get
    val path = ObservedLineProtocol.prepare(packet).path(packet, observed.lineId.get).get
    assertEquals(path.moves.take(3), prefix :+ "f1g2")
    assertEquals(observed.occurrenceId, Some(path.ids(2)))
    assertEquals(done.progress.physicalWorksIssued, 1)

  test("cancellation preserves resolved and unsupported requirements beside the unfinished pair"):
    val supplied = request("partial", CompareMoves("known", Nil, "e2e4", "d2d4", Some("better")),
      CompareMoves("pending", List("e2e4", "e7e5"), "g1f3", "b1c3"),
      Uninterpreted("meaning", "The plan wins in every continuation."))
    val active = awaiting(submit(start(List(supplied), limit = 0), rootRows))
    val baseline = rootDecision(active.assessment.get)
    val known = reading(active, "partial", "known")
    val unsupported = reading(active, "partial", "meaning")
    assertEquals(known.state, "refuted")
    assertEquals(known.answer.get.workId, "work:0")
    assertEquals(reading(active, "partial", "pending").answer, None)
    val done = completed(CommentaryJobReducer.cancelJob(active))
    assertEquals(reading(done, "partial", "known"), known)
    assertEquals(reading(done, "partial", "meaning"), unsupported)
    assertEquals(reading(done, "partial", "pending").state, "stopped")
    assertEquals(reading(done, "partial", "pending").stopReason, Some("cancelled"))
    assertEquals(reading(done, "partial", "pending").answer, None)
    assertEquals(done.proposalEntries.head.request, supplied)
    assertEquals(rootDecision(done), baseline)
    assertEquals(done.progress.physicalReportsAccepted, 1)

  test("a short completed pair report is rejected before admission and preserves its unanswered request"):
    val supplied = request("omitted", CompareMoves("pair", Nil, "e2e4", "c2c4"))
    val active = awaiting(submit(start(List(supplied)), rootRows))
    val work = active.issuedWork
    val baseline = rootDecision(active.assessment.get)
    assertEquals(work.rootRestriction, EngineRootRestriction.Restricted(List("e2e4", "c2c4")))
    assertEquals(work.multiPv, 2)
    val done = completed(submit(active, List(List("c2c4") -> 90)))
    val missing = reading(done, "omitted", "pair")
    assertEquals(missing.answer, None)
    assertEquals(missing.state, "stopped")
    assertEquals(missing.stopReason, Some("invalid_engine_work_report"))
    val retained = done.rootSearches.find(_.work.workId == work.workId).get
    assertEquals(retained.work.rootRestriction, work.rootRestriction)
    assertEquals(retained.receipt, None) // The completed-report count guard runs before line admission.
    assertEquals(retained.state, "stopped")
    assertEquals(retained.stopReason, Some(CommentaryJobStopCondition.InvalidEngineWorkReport))
    assertEquals(rootDecision(done), baseline)
    assertEquals(done.assessmentNeedsRefresh, false)
    assertEquals(done.progress.physicalReportsAccepted, 1)
    assertEquals(done.proposalEntries.head.request, supplied)

  test("a later report can supply an initially capped child prefix and reactivate its comparison"):
    val suppliedLater = List("e2e4", "e7e5", "b1c3", "b8c6")
    val blocked = request("later-child", CompareMoves("pair", suppliedLater, "g1f3", "f1c4"))
    val ready = request("first-child", CompareMoves("pair", List("e2e4", "e7e5"), "g1f3", "b1c3"))
    val first = awaiting(submit(start(List(blocked, ready), limit = 0), rootRows))
    assertEquals(first.issuedWork.enginePositionMovesUci, List("e2e4", "e7e5"))
    val initiallyMissing = reading(first, "later-child", "pair")
    assert(Set("deferred", "unavailable_input")(initiallyMissing.state))
    assertEquals(initiallyMissing.stopReason, Some("local_expansion_limit"))
    assertEquals(initiallyMissing.answer, None)
    val later = awaiting(submit(first, List(
      List("g1f3", "b8c6") -> 80, List("b1c3", "b8c6") -> 20)))
    assertEquals(later.issuedWork.enginePositionMovesUci, suppliedLater)
    assertEquals(later.issuedWork.searchAnchor.get.plyOffset, 4)
    assertEquals(reading(later, "later-child", "pair").stopReason, None)
    assertEquals(reading(later, "later-child", "pair").answer, None)
    assertEquals(later.localExpansionsUsed, 0)
    val firstAnswer = reading(later, "first-child", "pair")
    assertEquals(firstAnswer.state, "answered")
    assertEquals(firstAnswer.answer.get.workId, first.issuedWork.workId)
    val done = completed(submit(later, List(List("g1f3") -> 40, List("f1c4") -> 40)))
    assertEquals(reading(done, "first-child", "pair"), firstAnswer)
    assertEquals(reading(done, "later-child", "pair").state, "answered")
    assertEquals(reading(done, "later-child", "pair").answer.get.workId, later.issuedWork.workId)
    assertEquals(reading(done, "later-child", "pair").answer.get.evaluationRelation, "equal")
    assertEquals(done.localExpansionsUsed, 0)
    assertEquals(done.progress.physicalWorksIssued, 3)

  test("a forced position action retains the observed move and explicitly stops a later comparison"):
    // The same public board and forced move are used in RuntimeProtocolTest.
    val fen = "B5Q1/5n1k/2n5/1P3p2/1P1PN1P1/1b1N2R1/8/2B3K1 b - - 0 56"
    val forced = "h7g8"
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    val supplied = request("forced", ObserveMove("observed", Nil, forced),
      CompareMoves("later", List(forced), "d4d5", "g4g5"))
    val initialWork = awaiting(CommentaryJobReducer.startJob(fen, Nil, fen, forced,
      history.extend(List(forced)).toOption.get.currentFen,
      CommentaryJobPolicy(16, 10000L, profile, investigateChoices = true, maximumLocalExpansions = 0),
      0L, proposals = List(supplied)).toOption.get)
    assertEquals(initialWork.issuedWork.multiPv, 1)
    val done = completed(submit(initialWork, List(List(forced) -> 0)))
    done.result match
      case CompletedPositionCommentary.PositionAction(_) => ()
      case _ => fail("expected the forced position action")
    assertEquals((RuntimeProtocol.completedResultJson(done) \ "kind").as[String], "forced_single_move")
    val observed = reading(done, "forced", "observed")
    assertEquals(observed.state, "observed")
    assert(observed.lineId.nonEmpty && observed.occurrenceId.nonEmpty)
    assertEquals(reading(done, "forced", "later").state, "stopped")
    assertEquals(reading(done, "forced", "later").stopReason, Some("position_action"))
    assertEquals(reading(done, "forced", "later").answer, None)
    assertEquals(done.proposalEntries.head.request, supplied)
    assertEquals(done.localExpansionsUsed, 0)
    assertEquals(done.progress.physicalWorksIssued, 1)

  test("an already expired start retains all submitted requirements without issuing work"):
    val supplied = request("expired", ObserveMove("move", Nil, "e2e4"),
      CompareMoves("pair", Nil, "e2e4", "d2d4"),
      Uninterpreted("meaning", "This is the only sound long-term plan."))
    val state = CommentaryJobReducer.startJob(initial, Nil, initial, "d2d4",
      owner.extend(List("d2d4")).toOption.get.currentFen,
      CommentaryJobPolicy(16, 0L, profile, investigateChoices = true),
      0L, proposals = List(supplied)).toOption.get
    val stopped = state match
      case value: StoppedCommentaryJob => value
      case _ => fail("the original deadline must stop the initial request")
    assertEquals(stopped.stopCondition, CommentaryJobStopCondition.DeadlineExceeded)
    assertEquals(stopped.proposalEntries.head.request, supplied)
    List("move", "pair").foreach { id =>
      assertEquals(reading(stopped, "expired", id).state, "stopped")
      assertEquals(reading(stopped, "expired", id).stopReason, Some("deadline_exceeded"))
      assertEquals(reading(stopped, "expired", id).answer, None)
    }
    assertEquals(reading(stopped, "expired", "meaning").state, "unsupported_reading")
    assertEquals(stopped.localExpansionsUsed, 0)
    assertEquals(stopped.progress.physicalWorksIssued, 0)
    assertEquals(stopped.progress.physicalReportsAccepted, 0)
