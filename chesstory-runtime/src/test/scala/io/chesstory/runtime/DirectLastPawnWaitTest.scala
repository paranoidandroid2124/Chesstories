package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.{MoveReviewAssembler, MoveReviewInputAdmission}
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.CanonicalPositionHistory
import play.api.libs.json.*

/** Legal saved-history and book counterexamples. The matched value receipts
  * are synthetic: they test ownership and delivery, not the engine judgment
  * of the original g4/Ke3 choice or the book's outcome labels. */
class DirectLastPawnWaitTest extends munit.FunSuite:
  import CommentaryFact.*
  import CommentaryProposal.*
  import PrincipalVariationComparison.QualityBand

  private val fen = "8/5p2/8/2p1k3/ppp1P3/P1P2KP1/1P6/8 w - - 0 35"
  private def moves(value: String) = value.split(" ").toList
  private val actual = moves("g3g4 f7f6 f3e3 b4b3 e3f3 e5d6 f3f4 d6e6")
  private val late = moves("f3e3 f7f6 e3f3 e5d6 f3e2 d6e5 e2e3 e5d6 e3d2 b4a3 " +
    "b2a3 d6e5 d2e3 f6f5 e4f5 e5f5 e3f3 f5e5 g3g4")
  private val otherwise = late ++ List("e5f6", "f3e4")
  private val entry = late ++ List("e5f6", "f3f4")
  private val pawn = Piece("white", "pawn", "g3")
  private val king = Piece("white", "king", "f3")
  private val guard = Piece("black", "king", "e5")

  private case class Fixture(packet: EvidenceBackedJudgmentPacket,
      paths: ObservedLineProtocol.PreparedPaths, found: List[ObservedLineProtocol.PreparedPath])

  private def prepare(lines: List[List[String]], start: String = fen): Fixture =
    val history = CanonicalPositionHistory.from(start, Nil, start).toOption.get
    val input = MoveReviewInputAdmission.admitIndependent(history, actual.head,
      suppliedLines = lines.map(SuppliedCommentaryLine(CommentaryLineSource.Study, _))).get
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(input)).get
    val paths = ObservedLineProtocol.prepare(packet)
    val found = lines.map(line => paths.certifiedPath(packet, line)
      .getOrElse(fail("the full legal history was not certified: " + line.mkString(" "))))
    Fixture(packet, paths, found)

  private def answer(f: Fixture, left: Int, right: Int, offset: Int,
      relation: String = "equal", quality: QualityBand = QualityBand.Close) =
    CommentaryInvestigation.Answer("synthetic-direct-" + offset, "synthetic-direct-key-" + offset,
      CommentaryEngineProfile.ServerAdmitted.wireId, 16, Some(16), Some(16), relation,
      f.found(left).line.ref.id, f.found(right).line.ref.id, Json.obj(), Json.obj(),
      f.found(left).ids(offset), f.found(right).ids(offset), Some(quality))

  private def sources(lines: List[List[String]]) =
    lines.map(line => CommentaryDiscovery.SourcePath(line, Source("user", "direct-wait-test")))

  private def direct(entries: List[Entry]) = entries.filter(entry =>
    entry.request.origin.question.producer == "C-reserve-entry" &&
      entry.request.requirements.exists(_.id == "direct_wait"))

  private def directFixture() =
    val f = prepare(List(actual, otherwise, entry))
    // The current source PV chooses Ke4. The legal Kf4 test arm is admitted
    // as a sibling history, not supplied as the parent's principal variation.
    val batch = CommentaryDiscovery.discover(f.packet, f.paths,
      List(CommentaryDiscovery.Anchor(Nil, otherwise.head)), sources(List(actual, otherwise)), Nil, pvGuided = true)
    val generated = direct(batch.additions).find(_.request.requirements.exists {
      case test: CompareMoves => test.id == "reserve_entry_value" && test.prefixMovesUci == entry.init &&
        test.referenceMove == "f3f4" && test.alternativeMove == "f3e4"
      case _ => false
    }).getOrElse(fail("the actual last wait needs a separate f4/Ke4 value question even without Kf4 in the parent PV"))
    val rooted = generated.copy(readings = generated.readings.map {
      case reading if reading.requirementId == "reserve_root" => reading.copy(state = "answered",
        answer = Some(answer(f, 0, 1, 0, "better", QualityBand.Separated)))
      case reading => reading
    })
    val pending = CommentaryFactReader.read(f.packet, f.paths, List(rooted)).head
    val completed = CommentaryFactReader.read(f.packet, f.paths, List(rooted.copy(readings = rooted.readings.map {
      case reading if reading.requirementId == "reserve_entry_value" => reading.copy(state = "answered",
        answer = Some(answer(f, 2, 1, 20)))
      case reading => reading
    }))).head
    (f, generated, pending, completed)

  test("the actual last wait releases one common f4 entry under every remaining legal king reply"):
    val replies = List("e5d5", "e5d6", "e5e6", "e5f6")
    val lines = replies.map(reply => late ++ List(reply, "f3f4"))
    val f = prepare(actual :: lines)
    for path <- f.found.tail do
      val proof = RootSubjectComparison.lastPawnWaitEntry(f.paths, path, 18,
        pawn.witness, king.witness, guard.witness)
      assertEquals(proof.map(_._1), Some("f4"))
      val waiting = f.paths.positionView(path, 19).get
      val position = new MoveWorkProjection.Position(waiting)
      assertEquals(waiting.legalRows.get.map(_.move).sorted, replies.sorted)
      val legalSources = position.sourceRefs(waiting.legalRows.get.map(_.sourceId)).toSet
      assert(legalSources.subsetOf(proof.get._2.toSet), "all legal replies, including absent PV replies, own the release claim")
      assert(proof.get._2.nonEmpty)

  test("a remaining pawn move and a wait by another original piece reject the direct no-pawn scope"):
    val extra = "8/5p2/7p/2p1k3/ppp1P3/P1P2KP1/1P6/8 w - - 0 35"
    val f = prepare(List(actual, otherwise, entry), extra)
    assertEquals(RootSubjectComparison.lastPawnWaitEntry(f.paths, f.found(1), 18,
      pawn.witness, king.witness, guard.witness), None)
    val plain = prepare(List(actual, otherwise, entry))
    assertEquals(RootSubjectComparison.lastPawnWaitEntry(plain.paths, plain.found(1), 16,
      pawn.witness, king.witness, guard.witness), None, "Kf3 at16 is not a wait by the reserved pawn")
    assertEquals(RootSubjectComparison.lastPawnWaitEntry(plain.paths, plain.found(1), 18,
      Piece("white", "pawn", "e4").witness, king.witness, guard.witness), None,
      "an already removed pawn cannot borrow the actual g3 pawn's wait")

  test("the early book g4 with b3 and the correct later g4 with f5 remain outside the no-pawn branch"):
    val early = moves("f3e3 f7f6 e3f3 e5d6 f3f4 d6e6 g3g4 e6e7 f4f5")
    val bookLate = moves("f3e3 f7f6 e3f3 e5d6 f3e2 d6e5 e2e3 e5d6 e3d2 b4b3 " +
      "d2e3 d6e5 e3f3 e5d6 f3f4 d6e6 g3g4 e6e7 f4f5")
    for (line, offset, counter) <- List((early, 6, "b4b3"), (bookLate, 16, "f6f5")) do
      val f = prepare(List(actual, line))
      val waiting = f.paths.positionView(f.found(1), offset + 1).get
      assert(waiting.legalRows.exists(_.exists(_.move == counter)), "the book's actual remaining pawn option must stay visible")
      assertEquals(RootSubjectComparison.lastPawnWaitEntry(f.paths, f.found(1), offset,
        pawn.witness, king.witness, guard.witness), None,
        "this narrow scope's rejection does not judge the book move bad")
      val batch = CommentaryDiscovery.discover(f.packet, f.paths,
        List(CommentaryDiscovery.Anchor(Nil, line.head)), sources(List(actual, line)), Nil, pvGuided = true)
      assertEquals(direct(batch.additions), Nil)

  test("discovery creates an explicit direct entry value question without inventing a pawn counter"):
    val (_, generated, _, _) = directFixture()
    assert(!generated.request.requirements.exists(_.id == "reserve_counter"))
    assert(generated.request.requirements.exists(_.id == "direct_context"))
    val test = generated.request.requirements.collectFirst {
      case value: CompareMoves if value.id == "reserve_entry_value" => value
    }.get
    assertEquals(test.prefixMovesUci, late :+ "e5f6")
    assertEquals(test.referenceMove, "f3f4")
    assertEquals(test.alternativeMove, "f3e4")
    assert(!test.prefixMovesUci.isEmpty && test.prefixMovesUci.head == "f3e3")

  test("an entirely unobserved Kf4 arm is a pending value obligation rather than its own prerequisite"):
    val f = prepare(List(actual, otherwise))
    assert(!f.paths.paths.exists(_.moves.startsWith(entry)), "there is no admitted Kf4 arm to hide a dependency cycle")
    val batch = CommentaryDiscovery.discover(f.packet, f.paths,
      List(CommentaryDiscovery.Anchor(Nil, otherwise.head)), sources(List(actual, otherwise)), Nil, pvGuided = true)
    val generated = direct(batch.additions).find(_.request.requirements.exists {
      case test: CompareMoves => test.id == "reserve_entry_value" && test.prefixMovesUci == entry.init &&
        test.referenceMove == "f3f4" && test.alternativeMove == "f3e4"
      case _ => false
    }).get
    val rooted = generated.copy(readings = generated.readings.map {
      case reading if reading.requirementId == "reserve_root" => reading.copy(state = "answered",
        answer = Some(answer(f, 0, 1, 0, "better", QualityBand.Separated)))
      case reading => reading
    })
    val pending = CommentaryFactReader.read(f.packet, f.paths, List(rooted)).head
    assert(CommentaryProposal.neededRequirementIds(pending)("reserve_entry_value"),
      "the comparison must be reachable before its own named-entry execution is observed")
    assert(pending.readings.exists(reading => reading.requirementId == "reserve_entry_value" && reading.state == "deferred"))
    assertEquals(CommentaryIdea.directReserveEntries(List(pending)).size, 1,
      "typed dependency scheduling needs the current pending proof even before the future entry receipt")

  test("geometry alone cannot deliver the entry but a separately owned comparable value receipt can"):
    val (f, _, pending, completed) = directFixture()
    val pair = f.found.take(2)
    val previous = RootQuestionSummary.completePairAnswer(f.packet, f.paths, pair, "selected_comparison", Nil).get
    val untested = RootSubjectComparison.withDirectReserveEntry(f.packet, f.paths, pair,
      CommentaryIdea.directReserveEntries(List(pending)), previous)
    assertEquals(untested, previous, "a missing comparison is not verified by no-pawn geometry")
    val selected = RootSubjectComparison.withDirectReserveEntry(f.packet, f.paths, pair,
      CommentaryIdea.directReserveEntries(List(completed)), previous)
    assertEquals(selected.question, "investigated_mechanism")
    val value = selected.core.flatMap(_.statements).collectFirst { case value: CommentaryAnswer.DirectReserveEntry => value }.get
    assert(RootSubjectComparison.validDirectReserveEntry(f.paths, value))
    val text = RootQuestionSummary.realize(f.paths, pair, selected).get.text
    assert(text.contains("Kf4"), text)
    assert(text.contains("Ke4"), "the recorded alternative plan must remain in the explanation: " + text)
    assert(!text.contains("keeps that square guarded"), "no guarding pawn counter exists: " + text)
    assert(!text.contains("only winning") && !text.contains("forced") && !text.contains("the reason"), text)

  test("a wrong value occurrence, a withdrawn value grade and an unrelated original pawn cannot keep the direct answer"):
    val (f, _, _, completed) = directFixture()
    val pair = f.found.take(2)
    val previous = RootQuestionSummary.completePairAnswer(f.packet, f.paths, pair, "selected_comparison", Nil).get
    val selected = RootSubjectComparison.withDirectReserveEntry(f.packet, f.paths, pair,
      CommentaryIdea.directReserveEntries(List(completed)), previous)
    val value = selected.core.flatMap(_.statements).collectFirst { case value: CommentaryAnswer.DirectReserveEntry => value }.get
    val forged = List(value.proof.copy(pawn = Piece("white", "pawn", "e4")),
      value.proof.copy(square = "e4"),
      value.proof.copy(testAnswer = value.proof.testAnswer.map(_.copy(referenceOccurrenceId = f.found.head.ids.head))),
      value.proof.copy(testAnswer = value.proof.testAnswer.map(_.copy(evaluationRelation = "unknown", qualityBand = None))),
      value.proof.copy(testAnswer = value.proof.testAnswer.map(_.copy(evaluationRelation = "better", qualityBand = Some(QualityBand.Separated)))))
    for proof <- forged do assert(!RootSubjectComparison.validDirectReserveEntry(f.paths, value.copy(proof = proof)))

  test("the current source owns the wait while a separate value receipt may replace its forecast king reply"):
    val otherEntry = late ++ List("e5d5", "f3f4")
    val otherChoice = late ++ List("e5d5", "f3e3")
    val f = prepare(List(actual, otherwise, entry, otherEntry, otherChoice))
    val batch = CommentaryDiscovery.discover(f.packet, f.paths,
      List(CommentaryDiscovery.Anchor(Nil, otherwise.head)), sources(List(actual, otherwise)), Nil, pvGuided = true)
    val generated = direct(batch.additions).find(_.request.requirements.exists {
      case test: CompareMoves => test.id == "reserve_entry_value" && test.prefixMovesUci == entry.init &&
        test.referenceMove == "f3f4" && test.alternativeMove == "f3e4"
      case _ => false
    }).get
    val owned = CommentaryFactReader.read(f.packet, f.paths, List(generated.copy(readings = generated.readings.map {
      case reading if reading.requirementId == "reserve_root" => reading.copy(state = "answered",
        answer = Some(answer(f, 0, 1, 0, "better", QualityBand.Separated)))
      case reading if reading.requirementId == "reserve_entry_value" => reading.copy(state = "answered",
        answer = Some(answer(f, 2, 1, 20)))
      case reading => reading
    }))).head
    // This is a new owned child question, not a reused f6 receipt. Its Ke3
    // alternative is legal after Kd5; the previous Ke4 is illegal there.
    val original = CommentaryIdea.directReserveEntries(List(owned)).head
    val proof = original.copy(branch = otherEntry,
      test = CompareMoves("reserve_entry_value", otherEntry.init, "f3f4", "f3e3"),
      testAnswer = Some(answer(f, 3, 4, 20)))
    val pair = f.found.take(2)
    val previous = RootQuestionSummary.completePairAnswer(f.packet, f.paths, pair, "selected_comparison", Nil).get
    val selected = RootSubjectComparison.withDirectReserveEntry(f.packet, f.paths, pair, List(proof), previous)
    assertEquals(selected.question, "investigated_mechanism")
    val value = selected.core.flatMap(_.statements).collectFirst { case value: CommentaryAnswer.DirectReserveEntry => value }.get
    assert(RootSubjectComparison.validDirectReserveEntry(f.paths, value))
    val text = RootQuestionSummary.realize(f.paths, pair, selected).get.text
    assert(text.contains("Kd5"), "the actual tested reply must belong to the new value child: " + text)
    assert(!RootSubjectComparison.validDirectReserveEntry(f.paths, value.copy(proof = proof.copy(
      testAnswer = original.testAnswer))), "the previous f6 receipt cannot answer this different child")

  test("a separately observed wait needs its actual current source instead of an unrelated original root suffix"):
    val rootOther = moves("f3e3 e5e6 e3f4 e6f6 e4e5 f6g6")
    val f = prepare(List(actual, rootOther, otherwise, entry))
    val initial = CommentaryDiscovery.discover(f.packet, f.paths,
      List(CommentaryDiscovery.Anchor(Nil, rootOther.head)), sources(List(actual, rootOther)), Nil, pvGuided = true)
    val response = initial.additions.find(_.request.origin.question.producer == "C-reserve-response").get
    val readResponse = CommentaryFactReader.read(f.packet, f.paths, List(response.copy(readings = response.readings.map {
      case reading if reading.requirementId == "reserve_root" => reading.copy(state = "answered",
        answer = Some(answer(f, 0, 1, 0, "better", QualityBand.Separated)))
      case reading if reading.requirementId == "reserve_response" => reading.copy(state = "answered",
        answer = Some(answer(f, 1, 2, 1)))
      case reading => reading
    }))).head
    val source = CommentaryDiscovery.SourcePath(otherwise, Source("generator", "response_comparison",
      sourceRefs = List("synthetic-direct-1")))
    val next = CommentaryDiscovery.discover(f.packet, f.paths,
      List(CommentaryDiscovery.Anchor(Nil, rootOther.head)), sources(List(actual, rootOther)) :+ source,
      List(readResponse), pvGuided = true)
    val generated = direct(next.additions).find(_.request.requirements.exists(_.id == "reserve_source")).get
    val owned = CommentaryFactReader.read(f.packet, f.paths, List(generated.copy(readings = generated.readings.map {
      case reading if reading.requirementId == "reserve_root" => reading.copy(state = "answered",
        answer = Some(answer(f, 0, 1, 0, "better", QualityBand.Separated)))
      case reading if reading.requirementId == "reserve_source" => reading.copy(state = "answered",
        answer = Some(answer(f, 1, 2, 1)))
      case reading if reading.requirementId == "reserve_entry_value" => reading.copy(state = "answered",
        answer = Some(answer(f, 3, 2, 20)))
      case reading => reading
    }))).head
    val proof = CommentaryIdea.directReserveEntries(List(owned)).head
    assert(proof.source.nonEmpty)
    val previous = RootQuestionSummary.completePairAnswer(f.packet, f.paths, f.found.take(2), "selected_comparison", Nil).get
    val selected = RootSubjectComparison.withDirectReserveEntry(f.packet, f.paths, f.found.take(2), List(proof), previous)
    assertEquals(selected.question, "investigated_mechanism")
    val value = selected.core.flatMap(_.statements).collectFirst { case value: CommentaryAnswer.DirectReserveEntry => value }.get
    assert(!RootSubjectComparison.validDirectReserveEntry(f.paths, value.copy(proof = proof.copy(source = None))))
    val stale = owned.copy(readings = owned.readings.map(reading =>
      if reading.requirementId == "reserve_source" then reading.copy(state = "deferred", answer = None) else reading))
    assertEquals(CommentaryIdea.directReserveEntries(List(stale)), Nil)

  test("the original pawn-counter entry keeps its mandatory counter receipt and remains a separate mode"):
    val retained = moves("f3e3 e5e6 e3f4 e6f6 e4e5 f6e7 f4e4 e7e6 g3g4 b4b3 g4g5 e6d7 e4d5")
    val challenged = retained.take(11) ++ List("f7f5", "e5f6")
    val f = prepare(List(actual, retained, challenged))
    val batch = CommentaryDiscovery.discover(f.packet, f.paths,
      List(CommentaryDiscovery.Anchor(Nil, retained.head)), sources(List(actual, retained)), Nil, pvGuided = true)
    val generated = batch.additions.find(candidate => candidate.request.origin.question.producer == "C-reserve-entry" &&
      candidate.request.requirements.exists(_.id == "reserve_counter")).get
    assert(!generated.request.requirements.exists(_.id == "direct_wait"))
    val rooted = generated.copy(readings = generated.readings.map {
      case reading if reading.requirementId == "reserve_root" => reading.copy(state = "answered",
        answer = Some(answer(f, 0, 1, 0, "better", QualityBand.Separated)))
      case reading => reading
    })
    val pending = CommentaryFactReader.read(f.packet, f.paths, List(rooted)).head
    assert(!CommentaryIdea.reserveEntries(List(pending)).head.verified)
    assertEquals(CommentaryIdea.directReserveEntries(List(pending)), Nil)
    val completed = CommentaryFactReader.read(f.packet, f.paths, List(rooted.copy(readings = rooted.readings.map {
      case reading if reading.requirementId == "reserve_counter" => reading.copy(state = "answered",
        answer = Some(answer(f, 1, 2, 11)))
      case reading => reading
    }))).head
    assert(CommentaryIdea.reserveEntries(List(completed)).head.verified)
    assertEquals(CommentaryIdea.directReserveEntries(List(completed)), Nil)

  test("an already observed Kf4 entry still asks for a legal distinct value comparator without a prepared alternative arm"):
    val f = prepare(List(actual, entry))
    assert(!f.paths.paths.exists(path => path.moves.startsWith(entry.init) &&
      path.moves.lift(20).exists(_ != "f3f4")), "no other White choice is supplied at this child")
    val batch = CommentaryDiscovery.discover(f.packet, f.paths,
      List(CommentaryDiscovery.Anchor(Nil, entry.head)), sources(List(actual, entry)), Nil, pvGuided = true)
    val generated = direct(batch.additions).find(_.request.requirements.exists {
      case value: CompareMoves => value.id == "reserve_entry_value" &&
        value.prefixMovesUci == entry.init && value.referenceMove == "f3f4"
      case _ => false
    }).getOrElse(fail("observing Kf4 already must not remove its independent value question"))
    val comparison = generated.request.requirements.collectFirst {
      case value: CompareMoves if value.id == "reserve_entry_value" => value
    }.get
    val rows = f.paths.positionView(f.found(1), 20).get.legalRows.get
    assert(comparison.alternativeMove != comparison.referenceMove)
    assert(rows.exists(row => row.move == comparison.alternativeMove && row.actor.role.name == "king" &&
      row.actor.side == king.witness.side && row.capture.isEmpty && row.promotionRole.isEmpty),
      "the comparator is a named legal quiet king hypothesis, not a claim of strongest or best")
    val rooted = generated.copy(readings = generated.readings.map {
      case reading if reading.requirementId == "reserve_root" => reading.copy(state = "answered",
        answer = Some(answer(f, 0, 1, 0, "better", QualityBand.Separated)))
      case reading => reading
    })
    val pending = CommentaryFactReader.read(f.packet, f.paths, List(rooted)).head
    assert(CommentaryProposal.neededRequirementIds(pending)("reserve_entry_value"))
    val proof = CommentaryIdea.directReserveEntries(List(pending)).head
    assertEquals(proof.test, comparison)
    assertEquals(proof.testAnswer, None)
    assert(!proof.verified, "an already observed move and all-reply geometry do not supply its value receipt")


  test("a changed final forecast preserves the same wait conditions and separate entry value inquiry"):
    val f = prepare(List(actual, otherwise, entry))
    val origin = Source("user", "changed-parent-forecast")
    val anchor = List(CommentaryDiscovery.Anchor(Nil, otherwise.head))
    val beforeSources = List(
      CommentaryDiscovery.SourcePath(actual, origin),
      CommentaryDiscovery.SourcePath(otherwise, origin, Some(late -> 0)))
    val before = CommentaryDiscovery.discover(f.packet, f.paths, anchor, beforeSources, Nil, pvGuided = true)
    val generated = direct(before.additions).find(_.request.requirements.exists {
      case value: CompareMoves => value.id == "reserve_entry_value" && value.prefixMovesUci == entry.init &&
        value.referenceMove == entry.last && value.alternativeMove == otherwise.last
      case _ => false
    }).get
    // Only the root receipt is synthetic and answered. The entry's value is
    // still a separate unanswered child question when the forecast changes.
    val rooted = CommentaryFactReader.read(f.packet, f.paths, List(generated.copy(
      readings = generated.readings.map {
        case reading if reading.requirementId == "reserve_root" => reading.copy(state = "answered",
          answer = Some(answer(f, 0, 1, 0, "better", QualityBand.Separated)))
        case reading => reading
      }))).head
    val afterSources = List(
      CommentaryDiscovery.SourcePath(actual, origin),
      CommentaryDiscovery.SourcePath(entry, origin, Some(late -> 0)),
      CommentaryDiscovery.SourcePath(otherwise, origin, Some(entry.init -> 1)))
    val rebuilt = CommentaryDiscovery.discover(f.packet, f.paths, anchor, afterSources, Nil, pvGuided = true)
    val currentQuestion = direct(rebuilt.additions).find(_.request.id == rooted.request.id).get
    assertEquals(currentQuestion.request.requirements, rooted.request.requirements,
      "independent rediscovery must still contain the same concrete inquiry")
    val after = CommentaryDiscovery.discover(f.packet, f.paths, anchor, afterSources, List(rooted), pvGuided = true)
    // Batch.additions is a delta: an unchanged inquiry remains in prior state.
    val retained = direct(after.additions).find(_.request.id == rooted.request.id).getOrElse(rooted)
    assertEquals(retained.request.requirements, rooted.request.requirements,
      "a later legal choice cannot change the already observed wait/reply conditions")
    val current = CommentaryFactReader.read(f.packet, f.paths, List(retained)).head
    assertEquals(current.readings.find(_.requirementId == "direct_context").map(_.state), Some("supported"))
    assert(current.readings.find(_.requirementId == "reserve_entry_value").exists(_.answer.isEmpty),
      "the prior root receipt must not answer the entry's child value comparison")
    assert(CommentaryIdea.directReserveEntries(List(current)).exists(!_.verified),
      "an observed legal option remains unverified until its own value receipt returns")

  test("an uncertified raw source suffix cannot authorize the earlier owned wait"):
    val rootOther = moves("f3e3 e5e6 e3f4 e6f6 e4e5 f6g6")
    val f = prepare(List(actual, rootOther, otherwise, entry))
    val anchor = List(CommentaryDiscovery.Anchor(Nil, rootOther.head))
    val rootSources = sources(List(actual, rootOther))
    val initial = CommentaryDiscovery.discover(f.packet, f.paths, anchor, rootSources, Nil, pvGuided = true)
    val response = initial.additions.find(_.request.origin.question.producer == "C-reserve-response").get
    val readResponse = CommentaryFactReader.read(f.packet, f.paths, List(response.copy(
      readings = response.readings.map {
        case reading if reading.requirementId == "reserve_root" => reading.copy(state = "answered",
          answer = Some(answer(f, 0, 1, 0, "better", QualityBand.Separated)))
        case reading if reading.requirementId == "reserve_response" => reading.copy(state = "answered",
          answer = Some(answer(f, 1, 2, 1)))
        case reading => reading
      }))).head
    val origin = Source("generator", "response_comparison", sourceRefs = List("synthetic-direct-1"))
    def discover(line: List[String]) = CommentaryDiscovery.discover(f.packet, f.paths, anchor,
      rootSources :+ CommentaryDiscovery.SourcePath(line, origin), List(readResponse), pvGuided = true)
    val accepted = direct(discover(otherwise).additions)
    assert(accepted.exists(_.request.requirements.exists(_.id == "reserve_source")),
      "the work-id metadata does not erase its actual certified source history")
    // The next side is Black and f3 is empty: this extra White move is not
    // a certified continuation even though the preceding wait was legal.
    val raw = otherwise :+ "f3f4"
    assert(!f.paths.paths.exists(_.moves.startsWith(raw)))
    val unaccepted = direct(discover(raw).additions)
    assert(unaccepted.nonEmpty, "earlier observed boards remain available without certifying the raw suffix")
    assert(!unaccepted.exists(_.request.requirements.exists(_.id == "reserve_source")),
      "a work-id claim and a legal prefix cannot stand in for a complete accepted source")
    val read = CommentaryFactReader.read(f.packet, f.paths, unaccepted)
    assert(!CommentaryIdea.directReserveEntries(read).exists(_.verified),
      "the unrelated root suffix cannot certify that separately claimed source")

  test("a reserve decision rejects a foreign packet even when its UCI history and original actors match"):
    val lines = List(actual, otherwise, entry)
    val current = prepare(lines)
    val foreign = prepare(lines)
    val actors = List(pawn.witness, king.witness, guard.witness)
    assert(CommentaryDiscovery.sameReserveDecision(current.paths, current.found(1), current.found(2),
      late.size, actors))
    assert(!CommentaryDiscovery.sameReserveDecision(current.paths, foreign.found(1), current.found(2),
      late.size, actors), "a source path from another prepared packet is not owned")
    assert(!CommentaryDiscovery.sameReserveDecision(current.paths, current.found(1), foreign.found(2),
      late.size, actors), "a witness path from another prepared packet is not owned")
    assert(!CommentaryDiscovery.sameReserveDecision(current.paths, current.found(1), current.found(2),
      otherwise.size, actors), "different later moves do not own the same longer decision")
    assert(!CommentaryDiscovery.sameReserveDecision(current.paths, current.found(1), current.found(2),
      late.size, actors :+ Piece("white", "pawn", "e4").witness),
      "a required original actor already captured before this decision cannot survive by matching UCI")


  test("only a supported direct inquiry's actually returned child carries its prior source"):
    val rootOther = moves("f3e3 e5e6 e3f4 e6f6 e4e5 f6g6")
    val wrongReply = late ++ List("e5d5", "f3f4")
    val f = prepare(List(actual, rootOther, otherwise, entry, wrongReply))
    val anchor = List(CommentaryDiscovery.Anchor(Nil, rootOther.head))
    val rootSources = sources(List(actual, rootOther))
    val initial = CommentaryDiscovery.discover(f.packet, f.paths, anchor, rootSources, Nil, pvGuided = true)
    val response = initial.additions.find(_.request.origin.question.producer == "C-reserve-response").get
    val readResponse = CommentaryFactReader.read(f.packet, f.paths, List(response.copy(
      readings = response.readings.map {
        case reading if reading.requirementId == "reserve_root" => reading.copy(state = "answered",
          answer = Some(answer(f, 0, 1, 0, "better", QualityBand.Separated)))
        case reading if reading.requirementId == "reserve_response" => reading.copy(state = "answered",
          answer = Some(answer(f, 1, 2, 1)))
        case reading => reading
      }))).head
    val parentOrigin = Source("generator", "response_comparison", sourceRefs = List("synthetic-direct-1"))
    val first = CommentaryDiscovery.discover(f.packet, f.paths, anchor,
      rootSources :+ CommentaryDiscovery.SourcePath(otherwise, parentOrigin), List(readResponse), pvGuided = true)
    val generated = direct(first.additions).find(_.request.requirements.exists(_.id == "reserve_source")).get
    // All three receipts remain explicitly synthetic. The child is exact to
    // Kf6 followed by Kf4/Ke4, and cannot be a receipt for the different Kd5 reply.
    val owned = CommentaryFactReader.read(f.packet, f.paths, List(generated.copy(
      readings = generated.readings.map {
        case reading if reading.requirementId == "reserve_root" => reading.copy(state = "answered",
          answer = Some(answer(f, 0, 1, 0, "better", QualityBand.Separated)))
        case reading if reading.requirementId == "reserve_source" => reading.copy(state = "answered",
          answer = Some(answer(f, 1, 2, 1)))
        case reading if reading.requirementId == "reserve_entry_value" => reading.copy(state = "answered",
          answer = Some(answer(f, 3, 2, 20)))
        case reading => reading
      }))).head
    assert(CommentaryIdea.directReserveEntries(List(owned)).exists(_.verified))
    def refresh(line: List[String], previous: Entry) =
      val reported = f.paths.certifiedPath(f.packet, line).get
      val origin = Source("generator", "response_comparison",
        sourceRefs = List("synthetic-direct-20", reported.line.ref.id))
      CommentaryDiscovery.discover(f.packet, f.paths, anchor, rootSources ++ List(
        CommentaryDiscovery.SourcePath(line, origin, Some(late -> 0)),
        CommentaryDiscovery.SourcePath(otherwise, parentOrigin, Some(entry.init -> 1))),
        List(previous), pvGuided = true)
    val carriedCandidates = direct(refresh(entry, owned).additions)
    val carried = carriedCandidates.find(_.request.id == owned.request.id).getOrElse(owned)
    assertEquals(carried.request.requirements, owned.request.requirements)
    assert(!carriedCandidates.exists(_.request.id != owned.request.id),
      "the same answered direct inquiry must retain its established source route")
    assertEquals(CommentaryIdea.directReserveEntries(List(carried)).map(_.source.map(_.answer.workId)),
      List(Some("synthetic-direct-1")))

    val wrong = direct(refresh(wrongReply, owned).additions).filter(_.request.requirements.exists {
      case value: CompareMoves => value.id == "reserve_entry_value" && value.prefixMovesUci == wrongReply.init
      case _ => false
    })
    assert(wrong.nonEmpty, "the different legal king reply retains its own inquiry")
    assert(!wrong.exists(_.request.requirements.exists(_.id == "reserve_source")),
      "a forged work-id claim on another canonical reply cannot carry the original source")
    val wrongRead = CommentaryFactReader.read(f.packet, f.paths, wrong)
    assert(!CommentaryIdea.directReserveEntries(wrongRead).exists(_.verified),
      "the old child value is not an answer for the different reply")

    for state <- List("stopped", "unknown") do
      val unavailable = CommentaryFactReader.read(f.packet, f.paths, List(owned.copy(
        readings = owned.readings.map {
          case reading if reading.requirementId == "reserve_source" =>
            if state == "stopped" then reading.copy(state = "deferred", answer = None,
              stopReason = Some("comparison_stopped"))
            else reading.copy(answer = reading.answer.map(_.copy(
              evaluationRelation = "unknown", qualityBand = None)))
          case reading => reading
        }))).head
      assert(!CommentaryIdea.directReserveEntries(List(unavailable)).exists(_.verified),
        "the required source is not established: " + state)
      val pending = direct(refresh(entry, unavailable).additions)
      assert(!pending.exists(_.request.requirements.exists(_.id == "reserve_source")),
        "the completed child cannot repair or bypass its unavailable required source: " + state)
      assert(!CommentaryIdea.directReserveEntries(
        CommentaryFactReader.read(f.packet, f.paths, pending)).exists(_.verified))
