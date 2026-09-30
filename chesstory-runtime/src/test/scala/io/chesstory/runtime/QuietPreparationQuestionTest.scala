package io.chesstory.runtime

import lila.chessjudgment.model.line.CanonicalPositionHistory
import play.api.libs.json.*

/** Exposed development observations with invented scores. The response lifecycle
  * checks supply their exact branches; the PV-scope check supplies only the plan.
  */
class QuietPreparationQuestionTest extends munit.FunSuite:
  import CommentaryFact.*
  import CommentaryProposal.*

  private val initial = chess.variant.Standard.initialFen.value
  private val history = CanonicalPositionHistory.from(initial, Nil, initial).toOption.get.extend(List(
    "e2e4", "e7e5", "g1f3", "b8c6", "f1b5", "g7g6", "d2d4", "e5d4", "f3d4", "f8g7",
    "c1e3", "g8f6", "b1c3", "e8g8", "e1g1", "c6e7", "d1d2", "d7d5", "e4d5", "e7d5",
    "c3d5", "d8d5", "b5e2", "f6g4", "e2g4", "c8g4", "d4b3", "d5d2", "b3d2", "a8d8",
    "c2c3", "f8e8", "d2b3", "b7b6", "h2h3", "g4e6", "f1d1", "c7c5", "e3g5", "f7f6",
    "g5f4", "g8f7", "f2f3", "g6g5", "d1d8", "e8d8", "f4e3")).toOption.get
  private val prepared = List("h7h6", "a1e1", "f6f5")
  private val plan = prepared ++ List("f3f4", "g7f6", "g2g3", "a7a5")
  private val direct = List("f6f5")
  private val counter = "e3g5"
  private val profile = CommentaryEngineProfile.ServerAdmitted
  private val counterObservations = List(direct :+ counter, prepared :+ counter)

  private def submit(state: AwaitingEngineWork, rows: List[List[String]], now: Long = 1L): CommentaryJobState =
    submitScores(state, rows.zipWithIndex.map((moves, rank) => moves -> (-50 + rank * 10)), now)

  private def submitScores(state: AwaitingEngineWork, rows: List[(List[String], Int)], now: Long = 1L): CommentaryJobState =
    val work = state.issuedWork
    CommentaryJobReducer.submitIssuedWorkReport(state, EngineWorkReport.Completed(profile, work.workId,
      work.executionKeySha256, rows.map { (moves, score) =>
        ReportedEngineLineSuffix(moves, ReportedWhiteEngineScore.Centipawns(score), work.requiredDepth)
      }), now).toOption.get

  private def observed(line: List[String] = plan, supplied: List[List[String]] = counterObservations,
      proposals: List[Request] = Nil): CommentaryJobState =
    val root = CommentaryJobReducer.startJob(initial, history.movePrefixUci, history.currentFen, "h7h6",
      history.extend(List("h7h6")).toOption.get.currentFen,
      CommentaryJobPolicy(16, 10000L, profile, investigateChoices = true, previewDepth = Some(8)), 0L,
      inputLines = supplied.map(moves => lila.chessjudgment.model.judgment.SuppliedCommentaryLine(
        lila.chessjudgment.model.judgment.CommentaryLineSource.Study, moves)), proposals = proposals)
      .toOption.get.asInstanceOf[AwaitingEngineWork]
    val short = List(List("h7h6"), List("g5g4"), List("a7a5"))
    val preview = submit(root, short).asInstanceOf[AwaitingEngineWork]
    assert(!preview.proposalEntries.exists(_.admission == "generated"))
    submit(preview, short.updated(0, line))

  private def preparation(state: CommentaryJobState): Entry =
    state.proposalEntries.find(entry => entry.request.origin.question.producer == "C-preparation" &&
      entry.generatedArms.exists(arm => arm.referenceMove == "h7h6" && arm.alternativeMove.contains("f6f5")))
      .getOrElse(fail("the observed ...f5 must discover immediate ...f5 and the different answer to Bxg5"))

  private def strength(entry: Entry): (CompareMoves, Reading) =
    val requirement = entry.request.requirements.collectFirst {
      case value: CompareMoves if value.prefixMovesUci == prepared &&
          value.referenceMove == "f3f4" && value.alternativeMove == counter => value
    }.getOrElse(fail("the discovered counter must be compared with the independently observed reply"))
    requirement -> entry.readings.find(_.requirementId == requirement.id).get

  private def wire(state: CommentaryJobState): JsObject =
    PositionCommentaryJobProtocol.projectJobSnapshot(CommentaryJobSnapshot("q" * 32, "quiet-question", 10000L, state))

  private def explanation(state: CommentaryJobState): JsObject =
    (wire(state) \ "observations" \ "selection" \ "candidates").as[List[JsObject]].find(value =>
      (value \ "detail" \ "type").as[String] == "question_comparison" &&
        (value \ "detail" \ "prefix_moves_uci").as[List[String]] == prepared &&
        (value \ "detail" \ "reference_move").as[String] == "f3f4" &&
        (value \ "detail" \ "alternative_move").as[String] == counter).get

  test("a quiet preparation joins its observed action, immediate alternative and counter-capture branches"):
    val state = observed()
    val entry = preparation(state)
    val arm = entry.generatedArms.find(_.alternativeMove.contains("f6f5")).get
    val pawn = Piece("black", "pawn", "f6")
    val bishop = Piece("white", "bishop", "e3")
    val target = Piece("black", "pawn", "g5")
    val recapturer = Piece("black", "pawn", "h7")
    def fact(predicate: Predicate, moves: List[String], point: Point): Fact =
      entry.request.requirements.collectFirst {
        case value: Fact if value.predicate == predicate && value.path.source == Declared(moves) && value.at == point => value
      }.getOrElse(fail(s"missing exact question member $predicate at $moves / $point"))
    def values(value: Fact) = entry.readings.find(_.requirementId == value.id).get.result.get.samples.flatMap(_.value).toSet
    val action = Predicate("legal_move", Some(pawn), square = Some("f5"))
    val capture = Predicate("legal_capture", Some(bishop), Some(target))
    val response = Predicate("legal_capture", Some(recapturer), Some(bishop))
    val members = List(fact(action, prepared, Ply(2)), fact(action, direct, Ply(0)),
      fact(capture, prepared, Ply(3)), fact(capture, direct, Ply(1)),
      fact(response, prepared :+ counter, Ply(4)), fact(response, direct :+ counter, Ply(2)))
    assertEquals(members.map(values), List(Set("legal"), Set("legal"), Set("legal"), Set("legal"), Set("legal"), Set("illegal")))
    assert(members.map(_.id).forall(arm.memberRequirementIds.contains))
    assert(arm.headRequirementIds.exists(id => entry.request.requirements.exists {
      case group: FactGroup => group.id == id && group.mode == "all"
      case _ => false
    }))
    val (comparison, pending) = strength(entry)
    assert(arm.headRequirementIds.contains(comparison.id))
    assert(arm.headRequirementIds.filterNot(_ == comparison.id)
      .forall(id => entry.readings.find(_.requirementId == id).get.state == "supported"))
    assertEquals(pending.state, "awaiting_engine")
    assertEquals(pending.answer, None)
    val work = state.asInstanceOf[AwaitingEngineWork].issuedWork
    assertEquals(work.searchAnchor.get.prefixMoves, prepared)
    assertEquals(work.rootRestriction, EngineRootRestriction.Restricted(List("f3f4", counter)))
    assert(state.observationsPacket.get.assembly.input.legalLines.exists(_.moves == (direct :+ counter)))
    assert(entry.readings.flatMap(_.result.toList.flatMap(_.needs)).exists(need =>
      need.kind == "continuation" && need.prefixMoves == (prepared :+ counter)),
      "legal recapture and its unobserved continuation are separate obligations")
    val body = PositionCommentaryJobProtocol.projectJobSnapshot(CommentaryJobSnapshot("q" * 32, "quiet-question", 10000L, state))
    val publicEntries = (body \ "observations" \ "selection" \ "proposals").as[List[JsObject]]
    assert(publicEntries.exists(value => (value \ "id").as[String] == entry.request.id))
    assertEquals(state.verification.get.completedDepth, Some(16))

  test("board probes do not invent a future plan when all independent lines stop at the first move"):
    val state = observed(List("h7h6"), supplied = Nil)
    assert(!state.proposalEntries.exists(_.request.origin.question.producer == "C-preparation"))

  test("PV scope asks the immediate-action question before materializing unseen alternatives and counters"):
    val state = observed(supplied = Nil).asInstanceOf[AwaitingEngineWork]
    val observedPaths = List(plan, List("g5g4"), List("a7a5"))
    assert(state.observationsPacket.get.assembly.input.legalLines.forall(line =>
      observedPaths.exists(_.startsWith(line.moves))), "unselected possibilities must not become public calculation branches")
    assertEquals(state.localExpansionsUsed, 0)
    assert(!state.proposalEntries.exists(entry => Set("C-preparation", "C-plan-counter", "C-preparation-alternative")
      (entry.request.origin.question.producer)))
    assertEquals(state.issuedWork.searchAnchor, None)
    assertEquals(state.issuedWork.rootRestriction, EngineRootRestriction.Restricted(List("h7h6", "f6f5")))
    assert(state.discoveryState.scans.exists(_.state == "partial"),
      "PV-guided discovery must not claim to have exhausted the legal alternatives")

    val returned = List(prepared ++ List(counter, "h6g5"), direct :+ counter)
    val after = submitScores(state, returned.zip(List(-50, 150)))
    val discovered = preparation(after)
    assert(discovered.request.requirements.exists {
      case fact: Fact => fact.path.source == Declared(prepared :+ counter)
      case _ => false
    }, "a selected comparison's new PV can expose the preparation's answer to the counter")
    assert(after.observationsPacket.get.assembly.input.legalLines.forall(line =>
      (observedPaths ++ returned).exists(_.startsWith(line.moves))),
      "new engine evidence must not license unrelated counterfactual expansion")

  test("one practical idea links preparation, immediate action and the changed answer without adopting an unanswered plan"):
    val pending = observed().asInstanceOf[AwaitingEngineWork]
    val original = preparation(pending)
    def idea(state: CommentaryJobState) = CommentaryIdea.read(state.proposalEntries)
      .find(_.members.exists(_.entry.request.id == original.request.id)).get
    val before = idea(pending)
    assertEquals(before.key.branch, prepared)
    assert(before.members.exists(_.entry.request.origin.question.producer == "C-preparation-effect"))
    assert(before.contribution)
    assertEquals(before.state, "candidate")
    val direct = before.members.flatMap(_.comparisons).find(_.id == "contrast_immediate").get
    assert(before.priority(strength(original)._1).get._2 < before.priority(direct).get._2,
      "the specific counter whose answer the preparation changes is tested before declaring the preparation useful")
    val after = submitScores(pending, List(List("f3f4") -> -50, List(counter, "h6g5") -> -100))
    val updated = idea(after)
    assertEquals(updated.id, before.id)
    assert(updated.text.contains("does not establish a stronger refutation"))
    assert(updated.text.contains("f6f5 versus h7h6"), "refuting Bxg5 does not answer whether preparation was preferable")
    assertEquals(updated.state, "candidate")
    assert((explanation(after) \ "detail" \ "text").as[String].contains("does not establish a stronger refutation"))
    val packet = after.observationsPacket.get
    val paths = ObservedLineProtocol.prepare(packet)
    def candidates(entries: List[Entry]) =
      val delivered = scala.collection.mutable.Map.empty[String, List[MoveWorkProjection.Context]]
      val values = CommentaryProposal.comparisonCandidates(packet, paths, entries, Some((candidate, context) =>
        delivered((candidate \ "id").as[String]) = context
        candidate))
      values -> delivered.toMap
    val refs = (explanation(after) \ "detail" \ "question_refs").as[List[JsObject]].map(ref =>
      (ref \ "proposal_id").as[String] -> (ref \ "requirement_id").as[String]).toSet
    val shown = after.proposalEntries.flatMap(entry => entry.request.requirements.collect {
      case value: CompareMoves if refs(entry.request.id -> value.id) =>
        entry.readings.find(_.requirementId == value.id).flatMap(_.answer)
          .map(CommentaryIdea.AnsweredComparison(entry.request.id, value, _))
    }.flatten).toSet
    assert(shown.nonEmpty)
    assert(shown.forall(owner => refs(owner.proposalId -> owner.comparison.id)))
    val synthesisContext = updated.explanationDetails(packet, paths, shown)
    assert(synthesisContext.nonEmpty)
    // Inspect the actual compositor input. Rewritten prose is not an ownership key.
    def owners(values: Map[String, List[MoveWorkProjection.Context]], context: List[MoveWorkProjection.Context]) =
      values.toList.collect { case (id, delivered) if delivered.containsSlice(context) => id }.sorted
    val (all, delivered) = candidates(after.proposalEntries)
    val related = all.filter(value => (value \ "detail" \ "question_refs").as[List[JsObject]].exists(ref =>
      updated.members.exists(_.entry.request.id == (ref \ "proposal_id").as[String])))
    assert(related.size > 1, "different comparisons must keep their own answers to this shared idea")
    val ownerIds = owners(delivered, synthesisContext)
    assertEquals(ownerIds, List((explanation(after) \ "id").as[String]))
    // Input enumeration must not select an unshown context or duplicate the synthesis.
    val (reordered, reorderedContext) = candidates(after.proposalEntries.reverse)
    assertEquals(reordered.find(value => (value \ "id").as[String] == ownerIds.head)
      .map(value => (value \ "selection").as[String]), Some("selected"))
    assertEquals(reordered.map(value => (value \ "id").as[String]).toSet,
      all.map(value => (value \ "id").as[String]).toSet)
    val orderedIdea = CommentaryIdea.read(after.proposalEntries.reverse).find(_.id == updated.id).get
    assertEquals(owners(reorderedContext, orderedIdea.explanationDetails(packet, paths, shown)), ownerIds)
    val otherBranch = original.copy(generatedArms = original.generatedArms.map(arm =>
      arm.copy(prefixMoves = List("a7a5"))))
    assertEquals(CommentaryIdea.read(List(otherBranch)), Nil,
      "a similar action cannot import evidence from a different anchored history")

  test("a preparation consumes a matching continuation returned by its immediate-action comparison"):
    val fen = "6k1/7p/5p2/6p1/8/4B3/8/K7 b - - 0 1"
    val owner = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    val prefix = List("h7h6", "a1a2", "f6f5")
    val start = CommentaryJobReducer.startJob(fen, Nil, fen, "h7h6",
      owner.extend(List("h7h6")).toOption.get.currentFen,
      CommentaryJobPolicy(16, 10000L, profile, investigateChoices = true, previewDepth = Some(8)), 0L,
      inputLines = List(prefix :+ counter, direct :+ counter, List("h7h5", "a1a2")).map(moves =>
        lila.chessjudgment.model.judgment.SuppliedCommentaryLine(
          lila.chessjudgment.model.judgment.CommentaryLineSource.Study, moves)))
      .toOption.get.asInstanceOf[AwaitingEngineWork]
    val rows = List(prefix, List("g5g4"), List("h7h5"))
    val preview = submit(start, rows.map(_.take(1))).asInstanceOf[AwaitingEngineWork]
    val pending = submit(preview, rows).asInstanceOf[AwaitingEngineWork]
    val work = pending.issuedWork
    val inquiryPrefix = work.searchAnchor.toList.flatMap(_.prefixMoves)
    val entry = preparation(pending)
    val needed = entry.readings.find(_.result.exists(_.needs.exists(_.prefixMoves == (prefix :+ counter)))).get
    assertEquals(work.purpose, EngineWorkPurpose.InvestigationComparison)
    assertEquals(inquiryPrefix, Nil)
    assertEquals(needed.state, "partial")
    val continuation = prefix ++ List(counter, "h6g5")
    val suffix = continuation.drop(inquiryPrefix.size)
    val otherReplies = work.rootRestriction match
      case EngineRootRestriction.Restricted(moves) => moves.filterNot(_ == suffix.head).map(List(_))
      case _ => fail("the immediate action must have its own exact pair")
    val directQuestion = pending.proposalEntries.find(entry => entry.request.origin.question.producer == "C-preparation-effect" &&
      entry.request.requirements.exists {
        case value: CompareMoves => value.prefixMovesUci.isEmpty && value.referenceMove == "h7h6" && value.alternativeMove == "f6f5"
        case _ => false
      }).get
    assert((CommentaryProposal.snapshot(directQuestion) \ "answer_text").as[String].contains("unverified"))
    val after = submitScores(pending, (suffix -> -50) :: otherReplies.map(_ -> 150))
    assert(after.rootSearches.exists(search => search.work.workId == work.workId && search.receipt.nonEmpty),
      s"the pair must be admitted before rereading: ${work.rootRestriction} / ${after.progress}")
    val retained = after.proposalEntries.find(_.request.id == entry.request.id).get
    assertEquals(retained.request.requirements, entry.request.requirements)
    val result = retained.readings.find(_.requirementId == needed.requirementId).get.result.get
    assertEquals(result.state, "observed")
    assertEquals(result.needs, Nil)
    assert(result.samples.exists(_.moves == continuation))
    val updated = after.proposalEntries.find(_.request.id == directQuestion.request.id).get
    val directAnswer = updated.readings.flatMap(_.answer).head
    assertEquals(directAnswer.evaluationRelation, "worse")
    assert((CommentaryProposal.snapshot(updated) \ "answer_text").as[String].contains("f6f5 evaluates below h7h6"))
    assert((CommentaryProposal.snapshot(updated) \ "answer_text").as[String].contains("whole continuations"))
    val adopted = CommentaryIdea.read(after.proposalEntries).find(_.members.exists(_.entry.request.id == updated.request.id)).get
    assertEquals(adopted.state, "adopted", adopted.text)
    assert(adopted.text.contains("within the tested branches"))
    val changed = submitScores(pending, (List("h7h6", "a1a2", "g5g4") -> -50) :: otherReplies.map(_ -> 150))
    val withdrawn = CommentaryIdea.read(changed.proposalEntries).find(_.id == adopted.id).get
    assertEquals(withdrawn.state, "withdrawn", withdrawn.text)
    val currentIds = CommentaryIdea.currentQuestions(changed.proposalEntries).map(_.request.id).toSet
    assert(withdrawn.members.forall(member => !currentIds(member.entry.request.id)),
      "withdrawn siblings no longer keep computation or comparison work alive")
    val closed = finish(changed.proposalEntries, "investigation_limit").filter(entry =>
      withdrawn.members.exists(_.entry.request.id == entry.request.id))
    assert(closed.flatMap(_.readings).filterNot(isResolved).forall(reading =>
      !reading.stopReason.contains("investigation_limit")), "an unused dependency is not an exhausted investigation")
    assert(withdrawn.members.exists(member => member.current.isEmpty && member.contribution),
      "the historical response contrast is still true, but cannot keep a vanished current action adopted")
    val changedCandidates = (wire(changed) \ "observations" \ "selection" \ "candidates").as[List[JsObject]]
    assert(changedCandidates.filter(value => (value \ "detail" \ "type").as[String] == "question_comparison" &&
      (value \ "detail" \ "reference_move").as[String] == "h7h6" &&
      (value \ "detail" \ "alternative_move").as[String] == "f6f5").forall(value =>
        (value \ "selection").as[String] == "context"), "withdrawal reaches the existing explanation selection")
    val alternative = pending.proposalEntries.find(entry => entry.request.origin.question.producer == "C-preparation-alternative" &&
      entry.request.requirements.exists {
        case value: CompareMoves => value.prefixMovesUci.isEmpty && value.referenceMove == "h7h6" && value.alternativeMove == "h7h5"
        case _ => false
      }).getOrElse(fail("another move by the preparer must be checked under the same intervening reply"))
    val otherAction = alternative.request.requirements.collectFirst { case fact: Fact if fact.id == "alternative_action" => fact }.get
    assertEquals(otherAction.path.source, Declared(List("h7h5", "a1a2")))
    assertEquals(alternative.readings.find(_.requirementId == otherAction.id).get.result.get.samples.flatMap(_.value).toSet, Set("legal"))
    val alternativeValue = alternative.readings.find(_.requirementId == "contrast_value").get
    assertEquals(alternativeValue.answer.map(_.evaluationRelation), Some("worse"))
    assertEquals(alternativeValue.answer.map(_.workId), Some(preview.issuedWork.workId))
    val alternativeText = (CommentaryProposal.snapshot(alternative) \ "answer_text").as[String]
    assert(alternativeText.contains("h7h5 evaluates below h7h6"),
      "another preparation can allow the same action while the owned root comparison evaluates it lower")
    assert(alternativeText.contains("whole continuations"))
    val returnedGoal = updated.request.requirements.collectFirst {
      case fact: Fact if fact.path.source == Comparison("contrast_immediate", "reference") => fact
    }.get
    assertEquals(updated.readings.find(_.requirementId == returnedGoal.id).get.state, "observed")
    val changedPlan = submitScores(pending, List(List("h7h6", "a1a2", "g8f8") -> -50, List("f6f5") -> 150))
    val changedQuestion = changedPlan.proposalEntries.find(_.request.id == directQuestion.request.id).get
    assertEquals(changedQuestion.readings.find(_.requirementId == returnedGoal.id).get.result.get.reason, Some("event_absent"))
    val changedText = (CommentaryProposal.snapshot(changedQuestion) \ "answer_text").as[String]
    assert(changedText.contains("f6f5 evaluates below h7h6"))
    assert(changedText.contains("returned reference line (h7h6 from the starting position) does not contain"),
      "a better preparation evaluation must disclose that its returned line changes the named plan")
    sys.props.get("chesstory.preparationWireDump").foreach(path => java.nio.file.Files.writeString(
      java.nio.file.Path.of(path), Json.stringify(Json.obj("root_pending" -> wire(pending),
        "root_answered" -> wire(after), "root_plan_changed" -> wire(changedPlan)))))

  test("a newly returned recapture settles only the named continuation and preserves the original question"):
    import lila.chessjudgment.analysis.assembly.{MoveReviewAssembler, MoveReviewInputAdmission}
    import lila.chessjudgment.model.judgment.*
    val before = observed()
    val entry = preparation(before)
    val packet = before.observationsPacket.get
    val future = entry.request.requirements.collectFirst {
      case fact: Fact if fact.path.source == Declared(prepared :+ counter) && fact.path.extend => fact
    }.get
    val original = entry.readings.find(_.requirementId == future.id).get.result.get
    assert(original.needs.exists(need => need.kind == "continuation" && need.prefixMoves == (prepared :+ counter)))
    def read(line: List[String]) =
      val input = MoveReviewInputAdmission.admitIndependent(packet.assembly.input.positionHistory, "h7h6",
        lines = List(line)).get
      val refreshed = EvidenceBackedJudgmentPacket.fromAssembly(
        MoveReviewAssembler.assembleOccurrences(input, Some(packet.assembly))).get
      CommentaryFactReader.read(refreshed, ObservedLineProtocol.prepare(refreshed), List(entry)).head
    val unrelated = read(List("h7h6", counter, "h6g5"))
    assert(unrelated.readings.find(_.requirementId == future.id).get.result.get.needs
      .exists(_.prefixMoves == (prepared :+ counter)), "an earlier similar recapture cannot answer the later exact history")
    val answered = read(prepared ++ List(counter, "h6g5"))
    assertEquals(answered.request, entry.request)
    val result = answered.readings.find(_.requirementId == future.id).get.result.get
    assertEquals(result.state, "observed")
    assertEquals(result.needs, Nil)
    assert(result.samples.exists(sample => sample.moves == prepared ++ List(counter, "h6g5") && sample.value.contains("present")))
    val (comparison, pending) = strength(entry)
    assertEquals(answered.readings.find(_.requirementId == comparison.id).get, pending,
      "observing a recapture does not establish the counter's relative strength")
    entry.generatedArms.flatMap(_.headRequirementIds).filterNot(_ == comparison.id).foreach { id =>
      assertEquals(answered.readings.find(_.requirementId == id).get.state, "supported")
    }

  test("a returned child pair updates the explanation or withdraws it immediately while root verification is pending"):
    val pending = observed().asInstanceOf[AwaitingEngineWork]
    val before = explanation(pending)
    assert((before \ "state").toOption.isEmpty)
    assertEquals((before \ "detail" \ "evaluation_relation").get, JsNull)
    assertEquals((before \ "selection").as[String], "context")
    val cases = List((300, "better", "supported"), (-100, "worse", "refuted"), (-50, "equal", "refuted"))
    val snapshots = cases.map { (score, relation, state) =>
      // Invented scores vary only the comparison answer, leaving exact history and legality fixed.
      val after = submitScores(pending, List(List("f3f4") -> -50, List(counter, "h6g5") -> score))
      val (requirement, answer) = strength(preparation(after))
      val candidate = explanation(after)
      assertEquals((candidate \ "detail" \ "perspective").as[String], "white")
      assertEquals((candidate \ "id").as[String], (before \ "id").as[String])
      assert((candidate \ "detail" \ "question_refs").as[List[JsObject]].exists(value =>
        (value \ "requirement_id").as[String] == requirement.id))
      val scenes = (wire(after) \ "observations" \ "selection" \ "scenes").as[List[JsObject]]
      val selectedScene = scenes.exists(scene =>
        (scene \ "candidate_ids").as[List[String]].contains((candidate \ "id").as[String]))
      if relation == "better" then
        val next = after.asInstanceOf[AwaitingEngineWork]
        assertEquals(next.issuedWork.purpose, EngineWorkPurpose.RootSearch)
        assertEquals(next.issuedWork.requiredDepth, 19)
        assertEquals(next.verification.get.completedDepth, Some(16))
        assertEquals(next.assessmentNeedsRefresh, true)
        assertEquals(next.assessment.map(_.result), pending.assessment.map(_.result),
          "child scores trigger verification, not a replacement root judgment")
        assertEquals(answer.state, "deferred")
        assertEquals(answer.answer, None)
        assertEquals(answer.stopReason, Some("comparison_not_current"))
        assert((candidate \ "state").toOption.isEmpty)
        assertEquals((candidate \ "selection").as[String], "context")
        assertEquals((candidate \ "detail" \ "evaluation_relation").asOpt[String], None)
        assert(!selectedScene)
        val retained = next.responseSearches.find(_.work.workId == pending.issuedWork.workId).get
          .pairAnswer(next.observationsPacket.get, "f3f4", counter).get
        assertEquals(retained.evaluationRelation, "better")
        assertEquals(retained.referenceDepth, Some(16))
        assertEquals(retained.alternativeDepth, Some(16))
      else
        assertEquals(answer.state, state)
        assertEquals(answer.answer.get.evaluationRelation, relation)
        assertEquals((candidate \ "selection").as[String], "selected")
        assertEquals((candidate \ "detail" \ "evaluation_relation").as[String], relation)
        assert(selectedScene)
        assertEquals(after.verification.get.completedDepth, Some(16))
        assert(!after.verification.exists(_.reason == "child_challenge"))
      relation -> wire(after)
    }
    val stopped = CommentaryJobReducer.cancelJob(pending)
    val unresolved = strength(preparation(stopped))._2
    assertEquals(unresolved.state, "stopped")
    assertEquals(unresolved.stopReason, Some("cancelled"))
    assertEquals(unresolved.answer, None)
    assertEquals((explanation(stopped) \ "detail" \ "evaluation_relation").asOpt[String], None)
    sys.props.get("chesstory.questionWireDump").foreach { target =>
      java.nio.file.Files.writeString(java.nio.file.Path.of(target), Json.prettyPrint(
        JsObject(List("pending" -> wire(pending), "stopped" -> wire(stopped)) ++ snapshots)))
    }

  test("a fresh deeper pair can support or refute the counter after its shallow answer was withdrawn"):
    val pending = observed().asInstanceOf[AwaitingEngineWork]
    val strong = submitScores(pending, List(List("f3f4") -> -50, List(counter, "h6g5") -> 300))
      .asInstanceOf[AwaitingEngineWork]
    val firstWorkId = pending.issuedWork.workId
    assertEquals(strength(preparation(strong))._2.answer, None)
    assertEquals((explanation(strong) \ "selection").as[String], "context")
    // The new root assessment includes the strongest fresh child score, so a new contradiction is not introduced.
    val reopened = submitScores(strong, List(List("g5g4") -> -50, plan -> 350, List("a7a5") -> 400))
      .asInstanceOf[AwaitingEngineWork]
    assertEquals(reopened.issuedWork.searchAnchor.get.prefixMoves, prepared)
    assertEquals(reopened.issuedWork.requiredDepth, 19)
    val pendingAgain = strength(preparation(reopened))._2
    assertEquals(pendingAgain.state, "awaiting_engine")
    assertEquals(pendingAgain.answer, None)
    assert((explanation(reopened) \ "state").toOption.isEmpty)
    assertEquals((explanation(reopened) \ "detail" \ "evaluation_relation").get, JsNull)
    List((350, "better", "supported"), (300, "equal", "refuted"), (250, "worse", "refuted")).foreach {
      (score, relation, state) =>
        val revised = submitScores(reopened, List(List("f3f4") -> 300, List(counter, "h6g5") -> score))
        val answer = strength(preparation(revised))._2
        assertEquals(answer.state, state)
        assertEquals(answer.answer.get.evaluationRelation, relation)
        assertEquals(answer.answer.get.workId, reopened.issuedWork.workId)
        assertNotEquals(answer.answer.get.workId, firstWorkId)
        assertEquals(answer.answer.get.referenceDepth, Some(19))
        assertEquals(answer.answer.get.alternativeDepth, Some(19))
        assert(revised.responseSearches.exists(_.work.workId == firstWorkId),
          "the old conditional observation survives; it no longer answers the current question")
        assertEquals((explanation(revised) \ "id").as[String], (explanation(strong) \ "id").as[String])
        assertEquals((explanation(revised) \ "selection").as[String], "selected")
        assertEquals((explanation(revised) \ "detail" \ "evaluation_relation").as[String], relation)
        val idea = CommentaryIdea.read(revised.proposalEntries)
          .find(_.members.exists(_.entry.request.id == preparation(revised).request.id)).get
        if relation == "better" then
          assertEquals(idea.state, "revised")
          assert(idea.text.contains("this response challenges the continuation") ||
            idea.text.contains(s"clearly favors $counter over f3f4"),
            "the explicit decision can own this same answer's challenge without repeating its summary")
        assert(!revised.isInstanceOf[AwaitingEngineWork], "the shared investigation allowance must close with explicit remaining unknowns")
        assert(revised.proposalEntries.flatMap(_.readings).forall(reading =>
          isResolved(reading) || Set("invalid_input", "unsupported_reading", "stopped")(reading.state)))
    }

  test("the current assessment reply is compared before a different observed reply at the same prefix"):
    val state = observed(supplied = counterObservations :+ (prepared :+ "a2a4")).asInstanceOf[AwaitingEngineWork]
    val pairs = preparation(state).request.requirements.collect { case value: CompareMoves => value }
    assert(pairs.exists(_.referenceMove == "a2a4"))
    assert(pairs.exists(_.referenceMove == "f3f4"))
    assertEquals(state.issuedWork.searchAnchor.get.prefixMoves, prepared)
    assertEquals(state.issuedWork.rootRestriction, EngineRootRestriction.Restricted(List("f3f4", counter)))

  test("automatic and submitted questions share one pair answer even when their expected conclusions disagree"):
    val source = Source("user", "quiet-comparison-test")
    val request = Request("independent-comparison", Origin(source, source, source, source),
      List(CompareMoves("same-answer", prepared, "f3f4", counter, Some("worse"))))
    val pending = observed(proposals = List(request)).asInstanceOf[AwaitingEngineWork]
    assertEquals(pending.issuedWork.searchAnchor.get.prefixMoves, prepared)
    val after = submitScores(pending, List(List("f3f4") -> -50, List(counter, "h6g5") -> -100))
    val automatic = strength(preparation(after))._2
    val submitted = after.proposalEntries.find(_.request.id == request.id).get.readings.head
    assertEquals(automatic.state, "refuted")
    assertEquals(submitted.state, "supported")
    assertEquals(submitted.answer, automatic.answer)
    assertEquals(after.responseSearches.count(_.anchor.prefixMoves == prepared), 1)
    val candidates = (wire(after) \ "observations" \ "selection" \ "candidates").as[List[JsObject]]
    val id = (explanation(after) \ "id").as[String]
    assertEquals(candidates.count(value => (value \ "id").as[String] == id), 1)

  test("a resolved group releases irrelevant branches while an independent decision and its unknown scope survive"):
    val original = preparation(observed())
    val (comparison, _) = strength(original)
    val context = original.generatedArms.head.headRequirementIds.filterNot(_ == comparison.id).head
    val needed = neededRequirementIds(original)
    assertEquals(needed, Set(comparison.id), "supporting continuation samples do not justify work after the context is proven")
    val ended = finish(List(original), "investigation_limit").head
    val remaining = ended.readings.find(_.requirementId == comparison.id).get
    assertEquals(remaining.state, "stopped")
    assertEquals(remaining.stopReason, Some("investigation_limit"))
    val unused = ended.readings.filter(_.stopReason.contains("not_required_for_decision"))
    assert(unused.nonEmpty)
    assert(unused.exists(_.result.exists(_.needs.nonEmpty)), "stopping preserves the exact unobserved continuation")
    val unresolvedContext = original.copy(readings = original.readings.map(value =>
      if value.requirementId == context then value.copy(state = "partial") else value))
    assert(!neededRequirementIds(unresolvedContext)(comparison.id))
    assertEquals(finish(List(unresolvedContext), "investigation_limit").head.readings
      .find(_.requirementId == comparison.id).get.stopReason, Some("prerequisite_unresolved"))
    val falseContext = original.copy(readings = original.readings.map(value =>
      if value.requirementId == context then value.copy(state = "refuted") else value))
    assertEquals(finish(List(falseContext), "investigation_limit").head.readings
      .find(_.requirementId == comparison.id).get.stopReason, Some("premise_refuted"))

  private def preparationFromLines(fen: String, played: String, rows: List[(List[String], Int)],
      branch: List[String]): CommentaryIdea.Idea =
    val owner = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    rows.foreach { (moves, _) => assert(owner.extend(moves).isRight, s"illegal inline line: $moves") }
    val started = CommentaryJobReducer.startJob(fen, Nil, fen, played,
      owner.extend(List(played)).toOption.get.currentFen,
      CommentaryJobPolicy(16, 10000L, profile, investigateChoices = true, previewDepth = Some(8)), 0L,
      inputLines = rows.map { (moves, _) => lila.chessjudgment.model.judgment.SuppliedCommentaryLine(
        lila.chessjudgment.model.judgment.CommentaryLineSource.Study, moves) }).toOption.get.asInstanceOf[AwaitingEngineWork]
    val preview = submitScores(started, rows.map((moves, score) => moves.take(1) -> score))
      .asInstanceOf[AwaitingEngineWork]
    val assessed = submitScores(preview, rows)
    CommentaryIdea.read(assessed.proposalEntries).find(_.key.branch == branch)
      .getOrElse(fail(s"missing observed preparation for $branch"))

  test("WCC14 king routing remains a factual question without adopting its route as a useful preparation"):
    val route = List("f2f1", "d2c2", "f1g1")
    val idea = preparationFromLines("8/1B3b2/8/5pk1/1R4p1/6P1/3r1K2/8 w - - 4 49", "f2g1",
      List(route -> 100, List("f2e1", "d2c2") -> 100, List("f2g1", "d2a2") -> 0), route)
    assert(idea.contribution, "the current king move still owns its observed location change")
    assert(idea.members.exists(_.entry.request.origin.question.producer == "C-preparation-alternative"))
    assert(idea.members.exists(member => member.values("alternative_action") == List("illegal")))
    assert(idea.essential.nonEmpty && idea.essential.forall(_._3.answer.nonEmpty),
      "an unanswered comparison must not be the reason this idea remains a candidate")
    assert(!idea.mechanism, "a different origin is not a functional benefit of reaching the same empty square")
    assertEquals(idea.state, "candidate", idea.text)
    assert(!idea.text.contains("may help prepare"))
    assert(idea.text.contains("The observed line after f2f1 includes") &&
      idea.text.contains("after f2f1 d2c2"), idea.text)

  test("a different friendly piece clearing the observed action destination remains a functional contribution"):
    val branch = List("g1h2", "h8h7", "f3g1")
    val idea = preparationFromLines("7k/p7/8/8/8/5N2/P7/5RK1 w - - 0 1", "g1h2",
      List(branch -> 100, List("f1f2", "h8h7") -> 0, List("g1h1", "h8h7", "f3g1") -> 100), branch)
    assert(idea.mechanism, idea.text)
    assert(idea.text.contains("may help prepare white knight originally on f3 moving to g1"))
    assert(!idea.members.exists(_.mechanism),
      "the root clearance and controlled action contrast are legitimately owned by different question members")
    val changedOwners = idea.members.map { member =>
      if member.ownedEffects.isEmpty then member
      else member.copy(entry = member.entry.copy(request = member.entry.request.copy(
        requirements = member.entry.request.requirements.map {
          case fact: Fact if fact.path.source == Declared(branch.init) =>
            fact.copy(path = fact.path.copy(source = Declared(List("g1h2", "a7a6"))))
          case requirement => requirement
        })))
    }
    assert(!idea.members.exists(_.mechanismWith(changedOwners)),
      "a clearance belonging to another intervening history cannot establish this mechanism")
    val mistimed = idea.members.map { member =>
      member.copy(entry = member.entry.copy(request = member.entry.request.copy(
        requirements = member.entry.request.requirements.map {
          case fact: Fact if fact.id == "alternative_action" => fact.copy(at = Ply(1))
          case requirement => requirement
        })))
    }
    assert(!mistimed.exists(_.mechanismWith(idea.members)),
      "action availability at a different observation point cannot establish the contribution")

  test("the same actor's new attack can contribute to its observed capture without treating every route as preparation"):
    val branch = List("g1f3", "h7h6", "f3e5")
    val idea = preparationFromLines("6k1/7p/8/4p3/8/8/P7/6NK w - - 0 1", "g1f3",
      List(branch -> 100, List("g1h3", "h7h6") -> 0, List("h1h2", "h7h6") -> 0), branch)
    assertEquals(idea.key.action.kind, "legal_capture")
    assertEquals(idea.key.action.target, Some(Piece("black", "pawn", "e5")))
    assert(idea.mechanism, idea.text)
    assert(idea.text.contains("may help prepare white knight originally on g1 capturing black pawn originally on e5"))
    val withoutControl = idea.members.map { member =>
      val controls = member.ownedEffects.filter(effect => member.requirements(effect.leftRequirementId) match
        case fact: Fact => fact.predicate.kind == "control"
        case _ => false).map(_.id).toSet
      member.copy(entry = member.entry.copy(readings = member.entry.readings.map(reading =>
        if controls(reading.requirementId) then reading.copy(state = "partial") else reading)))
    }
    assert(!idea.members.exists(_.mechanismWith(withoutControl)),
      "the capture's new attack must be proven; the knight's changed location is insufficient")

  test("a comparison body states its owned answer once and retains the response interpretation"):
    val pending = observed().asInstanceOf[AwaitingEngineWork]
    val after = submitScores(pending, List(List("f3f4") -> -50, List(counter, "h6g5") -> -100))
    val candidate = explanation(after)
    val text = (candidate \ "detail" \ "text").as[String]
    val entry = preparation(after)
    val (comparison, reading) = strength(entry)
    val answer = reading.answer.get
    val notation = ObservedLineProtocol.prepare(after.observationsPacket.get).notation
    val ranking = s"${notation.move(comparison.prefixMovesUci, comparison.alternativeMove)} evaluates below ${notation.move(comparison.prefixMovesUci, comparison.referenceMove)}"
    assertEquals(text.sliding(ranking.length).count(_ == ranking), 1, text)
    assert(text.contains("does not establish a stronger refutation"),
      "the pair's result does not replace the meaning of this particular response test")
    val idea = CommentaryIdea.read(after.proposalEntries)
      .find(_.members.exists(_.entry.request.id == entry.request.id)).get
    val (_, unresolved, pendingReading) = idea.essential.find { (_, value, _) =>
      value.prefixMovesUci.isEmpty && Set(value.referenceMove, value.alternativeMove) == Set("f6f5", "h7h6")
    }.get
    assert(pendingReading.answer.isEmpty,
      "the different choice must keep its unanswered receipt")
    val pendingPair = s"${notation.move(unresolved.prefixMovesUci, unresolved.alternativeMove)} versus ${notation.move(unresolved.prefixMovesUci, unresolved.referenceMove)}"
    assert(text.contains(pendingPair), "the different unanswered choice must remain visible")
    assertEquals((candidate \ "detail" \ "evaluation_relation").as[String], "worse")
    assertEquals(answer.evaluationRelation, "worse")
    assertEquals((candidate \ "detail" \ "prefix_moves_uci").as[List[String]], comparison.prefixMovesUci)
    assertEquals((candidate \ "detail" \ "reference_move").as[String], comparison.referenceMove)
    assertEquals((candidate \ "detail" \ "alternative_move").as[String], comparison.alternativeMove)
    assertEquals((candidate \ "line_ids").as[List[String]], List(answer.referenceLineId, answer.alternativeLineId))
    assert((candidate \ "detail" \ "question_refs").as[List[JsObject]].exists(ref =>
      (ref \ "proposal_id").as[String] == entry.request.id &&
        (ref \ "requirement_id").as[String] == comparison.id))

  test("only the same question prefix and full answer receipt can replace an answer summary"):
    val pending = observed().asInstanceOf[AwaitingEngineWork]
    val after = submitScores(pending, List(List("f3f4") -> -50, List(counter, "h6g5") -> -100))
    val entry = preparation(after)
    val idea = CommentaryIdea.read(after.proposalEntries).find(_.members.exists(_.entry.request.id == entry.request.id)).get
    val (member, comparison, reading) = idea.knownDecisions.find { (_, value, reading) =>
      value.prefixMovesUci == prepared && value.referenceMove == "f3f4" &&
        value.alternativeMove == counter && reading.answer.nonEmpty
    }.get
    val answer = reading.answer.get
    val owner = CommentaryIdea.AnsweredComparison(member.entry.request.id, comparison, answer)
    val packet = after.observationsPacket.get
    val paths = ObservedLineProtocol.prepare(packet)
    def details(shown: Set[CommentaryIdea.AnsweredComparison]) = idea.explanationDetails(packet, paths, shown)
    val original = details(Set.empty)
    val shared = details(Set(owner))
    val replaced = original.diff(shared)
    val replacement = shared.diff(original)
    assertEquals(replaced.size, 1, "the exact owned receipt replaces only its answer context")
    assertEquals(replacement.size, 1, "the response interpretation remains as a context")
    assertEquals(replaced.head.copy(text = replacement.head.text), replacement.head,
      "sharing an answer must not alter any non-text context ownership or witness")
    assert(replaced.head.text.contains(paths.notation.move(comparison.prefixMovesUci, comparison.alternativeMove)))
    assert(replaced.head.text.contains(paths.notation.move(comparison.prefixMovesUci, comparison.referenceMove)))
    assert(replaced.head.text.contains("evaluates below"))
    assert(replacement.head.text.contains("does not establish a stronger refutation"))
    val otherOwners = List(
      owner.copy(proposalId = owner.proposalId + "-other"),
      owner.copy(comparison = comparison.copy(id = comparison.id + "-other")),
      owner.copy(comparison = comparison.copy(prefixMovesUci = prepared.dropRight(1))),
      owner.copy(comparison = comparison.copy(referenceMove = counter, alternativeMove = "f3f4")),
      owner.copy(answer = answer.copy(workId = answer.workId + "-other",
        executionKeySha256 = "0" * 64)),
      owner.copy(answer = answer.copy(requiredDepth = answer.requiredDepth + 3,
        referenceDepth = answer.referenceDepth.map(_ + 3), alternativeDepth = answer.alternativeDepth.map(_ + 3))),
      owner.copy(answer = answer.copy(alternativeLineId = answer.alternativeLineId + "-other",
        alternativeOccurrenceId = answer.alternativeOccurrenceId + "-other")))
    otherOwners.foreach(other => assertEquals(details(Set(other)), original,
      "a similar pair from another question, history, work, depth or returned branch still needs its own summary"))

  test("an idea choice replaces only its own exact answer summary"):
    val route = List("f2f1", "d2c2", "f1g1")
    // The played move keeps this route eligible for discovery even when another move ranks higher.
    // A close pair isolates the named choice from a separate, explicit revision decision.
    val idea = preparationFromLines("8/1B3b2/8/5pk1/1R4p1/6P1/3r1K2/8 w - - 4 49", route.head,
      List(route -> 0, List("f2e1", "d2c2") -> 1, List("f2g1", "d2a2") -> 2), route)
    val owned = idea.knownDecisions.filter { (member, value, reading) =>
      idea.choices.exists((id, choice, answer) => id == member.entry.request.id && choice == value &&
        answer.answer == reading.answer && idea.strongerComparisons.contains(choice -> answer))
    }
    assert(owned.nonEmpty, "the inline comparison must be both a decision and a current stronger choice")
    val anotherQuestion = idea.copy(choices = idea.choices.map((id, value, reading) =>
      (id + "-other", value, reading)))
    owned.foreach { (_, value, reading) =>
      val relative = reading.answer.get.evaluationRelation match
        case "better" => "above"
        case "worse" => "below"
        case _ => "equal to"
      val ranking = s"${value.alternativeMove} evaluates $relative ${value.referenceMove} for the side to move"
      assert(!idea.text.contains(ranking), idea.text)
      assert(anotherQuestion.text.contains(ranking),
        "the same moves and receipt under a different question owner do not replace this answer")
    }
    assertEquals(anotherQuestion.state, idea.state)
    assertEquals(anotherQuestion.strongerComparisons, idea.strongerComparisons)
    val shown = owned.map((member, value, reading) =>
      CommentaryIdea.AnsweredComparison(member.entry.request.id, value, reading.answer.get)).toSet
    assertEquals(shown.size, 2, "both independent alternatives must have an owned answer")
    val onlyDecision = idea.decisionText(shown).mkString(" ")
    assert(!onlyDecision.contains("At this idea's starting position"), onlyDecision)
    assert(onlyDecision.contains("small differences alone do not rule out this idea"),
      "the pair text owns the ranking, not the idea's close-comparison qualification")
    assertEquals(anotherQuestion.decisionText(shown), anotherQuestion.decisionText,
      "a different question with the same moves and answer is not the displayed owner")
    owned.foreach { (member, value, reading) =>
      val answer = reading.answer.get
      val owner = CommentaryIdea.AnsweredComparison(member.entry.request.id, value, answer)
      val move = if value.referenceMove == idea.key.move then value.alternativeMove else value.referenceMove
      val otherMoves = idea.strongerChoices.filterNot(_ == move)
      val partial = idea.decisionText(Set(owner))
      assert(partial.exists(_.contains(s"${otherMoves.mkString(" or ")} evaluates above ${idea.key.move}")),
        "the answer to one comparison cannot remove another candidate's ranking")
      val changedOwners = List(
        owner.copy(proposalId = owner.proposalId + "-other"),
        owner.copy(comparison = value.copy(prefixMovesUci = List("f2f1"))),
        owner.copy(answer = answer.copy(workId = answer.workId + "-other", executionKeySha256 = "0" * 64)),
        owner.copy(answer = answer.copy(requiredDepth = answer.requiredDepth + 1,
          referenceDepth = answer.referenceDepth.map(_ + 1), alternativeDepth = answer.alternativeDepth.map(_ + 1))),
        owner.copy(answer = answer.copy(alternativeLineId = answer.alternativeLineId + "-other",
          alternativeOccurrenceId = answer.alternativeOccurrenceId + "-other")))
      changedOwners.foreach(other => assertEquals(idea.decisionText(Set(other)), idea.decisionText))
      val otherReceipt = reading.copy(answer = Some(answer.copy(workId = answer.workId + "-other")))
      val conflicting = idea.copy(choices = idea.choices :+ (member.entry.request.id + "-other", value, otherReceipt))
      assertEquals(conflicting.decisionText(Set(owner)), conflicting.decisionText,
        "an exact displayed owner cannot cover the same candidate's different report")
      val reversed = value.copy(id = value.id + "-reversed", referenceMove = value.alternativeMove,
        alternativeMove = value.referenceMove)
      val reverseAnswer = answer.copy(
        referenceLineId = answer.alternativeLineId, alternativeLineId = answer.referenceLineId,
        referenceOccurrenceId = answer.alternativeOccurrenceId, alternativeOccurrenceId = answer.referenceOccurrenceId,
        referenceDepth = answer.alternativeDepth, alternativeDepth = answer.referenceDepth,
        referenceEvaluation = answer.alternativeEvaluation, alternativeEvaluation = answer.referenceEvaluation,
        evaluationRelation = if answer.evaluationRelation == "better" then "worse" else "better")
      val equivalent = idea.copy(choices = idea.choices :+ (member.entry.request.id + "-reversed", reversed,
        reading.copy(requirementId = reversed.id, answer = Some(reverseAnswer))))
      assertEquals(equivalent.decisionText(Set(owner)), partial,
        "a reverse orientation repeats the same ranking only when every owned receipt field agrees")
    }
