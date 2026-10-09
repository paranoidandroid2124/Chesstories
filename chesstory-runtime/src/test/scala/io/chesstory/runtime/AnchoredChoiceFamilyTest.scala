package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.*
import lila.chessjudgment.model.judgment.*

/** Finite supplied receipts exercise family ownership and response-dependent
  * decisions. They are not engine studies or automatic human-utility results.
  */
private[runtime] object AnchoredChoiceFamilyFixture:
  import AnchoredChoiceTestFixture.{AnchoredFixture, root, keepRoot, early, kept, keeping,
    other, changed, resistance, peer, pawnReply, capture, rootPacket}
  import CommentaryFact.*
  import CommentaryProposal.*

  val blockade = List(keepRoot, "f7f6")
  val kingReply = List(keepRoot, "e5e6")
  val kingPlan = "e3d2"
  val quietPrefix = List(keepRoot, "f7f6", keeping, "e5d6", "f3e2", "d6e5",
    "e2e3", "b4b3", "e3f3", "e5d6", "f3f4", "d6e6")
  val checkingPrefix = List(keepRoot, "f7f6", keeping, "f6f5", "e4f5", "e5f5")
  val quietOther = "f4f3"
  val checkingOther = "f3e2"

  // The current reserve is legal at all six defended boards, although the
  // supplied initial resistance PVs do not show an advance there. Each board
  // has an independent owned local-choice duty; these fixtures supply values
  // rather than interpreting legal availability as usefulness.
  val fallbackPrefixes =
    List("e5d6", "e5e6", "f6f5").map(reply => blockade ++ List(keeping, reply)) ++
      List("f7f6", "e6d6", "e6e5").map(reply => kingReply ++ List(kingPlan, reply))
  val fallbackOther = "c3b4"

  private lazy val fixtureHistory = rootPacket(lines = List(early, kept))._1.assembly.input.positionHistory
  private def currentHistory(at: List[String]) = fixtureHistory.extend(at).toOption.get
  private def actualStep(at: List[String], move: String) =
    currentHistory(at :+ move).segmentReplaySteps.last
  private def currentKingOther(at: List[String]): String =
    currentHistory(at).currentLegalMoveUcis.find(move =>
      val step = actualStep(at, move)
      step.move.piece.role == chess.King && step.capturedRole.isEmpty).get
  private def fallbackOwnMoves(at: List[String]): List[String] =
    List(root, fallbackOther, "a3b4", currentKingOther(at)).distinct
  private def fallbackCaptures(at: List[String]): List[String] =
    currentHistory(at).currentLegalMoveUcis.filter(move => actualStep(at, move).capturedRole.nonEmpty)
  private def fallbackReplies(at: List[String]): List[String] =
    val legal = currentHistory(at).currentLegalMoveUcis
    val captures = fallbackCaptures(at)
    val quietKings = legal.filter(move =>
      val step = actualStep(at, move)
      step.move.piece.role == chess.King && step.capturedRole.isEmpty)
    (captures ++ quietKings ++ legal).distinct.take(3)
  private def fallbackLine(at: List[String], reply: String): List[String] =
    val withReply = at :+ reply
    val step = actualStep(at, reply)
    // Retain a legal immediate recapture, including bxc3+ / b2xc3 check reply.
    // Both the capturing side and original victim still come from admission.
    val recapture = Option.when(step.capturedRole.nonEmpty)(
      currentHistory(withReply).currentLegalMoveUcis.find(move =>
        val next = actualStep(withReply, move)
        next.capturedRole.nonEmpty && next.move.dest == step.move.dest)).flatten
    withReply ++ recapture.toList
  private val fallbackLines = fallbackPrefixes.flatMap { at =>
    fallbackOwnMoves(at).flatMap(move =>
      (fallbackReplies(at :+ move) ++ fallbackCaptures(at :+ move)).distinct
        .map(reply => fallbackLine(at :+ move, reply)))
  }

  private val first = List(root, keepRoot).flatMap(move =>
    List("f7f6", "e5e6", "e5d6").map(reply => List(move, reply)))
  private val blockadeLines = List(root, keeping, other, changed).map(blockade :+ _) ++
    List(root, keeping).flatMap(move => List(
      blockade ++ List(move, resistance), blockade ++ List(move, peer),
      blockade ++ List(move, pawnReply), blockade ++ List(move, capture, "b2c3")))
  private val kingLines = List(root, kingPlan, changed).map(kingReply :+ _) ++
    List(root, kingPlan).flatMap(move => List("f7f6", "e6d6", "e6e5")
      .map(reply => kingReply ++ List(move, reply)) :+
        (kingReply ++ List(move, capture, "b2c3")))
  private val quietLines = List(root, quietOther).flatMap(move =>
    List("e6e7", "e6d6", "f6f5").map(reply => quietPrefix ++ List(move, reply)))
  private val checkingLines = List(root, checkingOther).flatMap(move =>
    List("f5e5", "f5g5", "f5e6").map(reply => checkingPrefix ++ List(move, reply)))
  val lines: List[List[String]] =
    (List(early, kept) ++ first ++ blockadeLines ++ kingLines ++ quietLines ++ checkingLines ++ fallbackLines).distinct

  def fixture(): AnchoredFixture =
    val (packet, paths, receipt) = rootPacket(lines = lines)
    val batch = CommentaryDiscovery.discover(packet, paths, List(CommentaryDiscovery.Anchor(Nil, keepRoot)),
      lines.map(moves => CommentaryDiscovery.SourcePath(moves, Source("user", "anchored-family-test"))),
      Nil, pvGuided = true)
    val generated = batch.additions.find(entry =>
      entry.request.origin.question.producer == "C-reserve-use" && entry.request.requirements.exists {
        case value: CompareMoves => value.id == "reserve_root" &&
          value.referenceMove == root && value.alternativeMove == keepRoot
        case _ => false
      }).get
    val answer = CommentaryInvestigation.pairedAnswer(packet, root, keepRoot,
      receipt.work, receipt.lines, Some(paths)).get
    val answered = generated.copy(readings = generated.readings.map(reading =>
      if reading.requirementId == "reserve_root" then pairReading(
        generated.request.requirements.collectFirst { case value: CompareMoves if value.id == "reserve_root" => value }.get,
        answer) else reading))
    AnchoredFixture(packet, paths, CommentaryFactReader.read(packet, paths, List(answered)).head, receipt)

  def supplied(f: AnchoredFixture, id: String, at: List[String], values: List[(String, Int)],
      restricted: Boolean = false, longest: Boolean = false, returnedDepth: Int = 16,
      requestedDepth: Int = 16)
      : TargetResponseInvestigation.Search =
    val history = f.packet.assembly.input.positionHistory.extend(at).toOption.get
    val path = CommentaryProposal.observedPrefix(f.packet, f.paths, at).get
    val work = IssuedEngineWork.create(CommentaryEngineProfile.ServerAdmitted, id,
      EngineWorkPurpose.ResponseComparison, history.initialFen, history.movePrefixUci, history.currentFen,
      if restricted then EngineRootRestriction.Restricted(values.map(_._1)) else EngineRootRestriction.Unrestricted,
      history.currentFen, Nil, EngineSearchLimits(requestedDepth, 2_000_000, 2_500, values.size), 3_500,
      Some(EngineSearchAnchor(at.head, path.ids.head, path.positions(at.size).id.get, at)))
    val reported = values.map { (move, cp) =>
      val source = f.paths.paths.filter(_.moves.startsWith(at :+ move)).sortBy(path =>
        (if longest then -path.moves.size else path.moves.size, path.line.ref.id)).head
      ReportedEngineLineSuffix(source.moves.drop(at.size), ReportedWhiteEngineScore.Centipawns(cp), returnedDepth)
    }
    val physical = EngineLineAdmission.admitReportedLineSuffixes(work, reported, Some(history)).get
    val receipt = CommentaryInvestigation.EngineCandidates(work,
      EngineLineAdmission.bindAdmittedLineSuffixes(work, physical).get)
    TargetResponseInvestigation.readSearch(f.packet, receipt, history, f.paths).get

  def read(f: AnchoredFixture, searches: List[TargetResponseInvestigation.Search],
      entries: Option[List[Entry]] = None) =
    AnchoredChoiceReading.family(f.packet, f.paths, entries.getOrElse(List(f.entry)), searches,
      Set(root, keepRoot), f.rootReceipt.work.engineProfile.wireId, 16)

  def firstPools(f: AnchoredFixture, restricted: Boolean = false) = List(
    supplied(f, "spent-first", List(root), List("f7f6" -> 0, "e5e6" -> 300, "e5d6" -> 400), restricted),
    supplied(f, "retained-first", List(keepRoot), List("f7f6" -> 500, "e5e6" -> 510, "e5d6" -> 800), restricted))
  def ownPools(f: AnchoredFixture) = List(
    supplied(f, "blockade-own", blockade, List(keeping -> 500, other -> 100, changed -> 90)),
    supplied(f, "blockade-own-pair", blockade, List(keeping -> 500, root -> 0), restricted = true),
    supplied(f, "king-own", kingReply, List(kingPlan -> 600, root -> 550, changed -> 100)))
  def resistances(f: AnchoredFixture, longest: Boolean = false) =
    List(root, keeping).map(move =>
      val base = if move == root then 0 else 500
      supplied(f, "blockade-response-" + move, blockade :+ move,
        List(resistance -> base, peer -> (base + 10), pawnReply -> (base + 20)), longest = longest)) ++
      List(root, kingPlan).map(move =>
        val base = if move == root then 550 else 600
        supplied(f, "king-response-" + move, kingReply :+ move,
          List("f7f6" -> base, "e6d6" -> (base + 10), "e6e5" -> (base + 20))))
  def captures(f: AnchoredFixture, longest: Boolean = false) =
    List(root, keeping).map(move =>
      val base = if move == root then 0 else 500
      supplied(f, "blockade-capture-" + move, blockade :+ move,
        List(resistance -> base, capture -> (base + 15)), restricted = true, longest = longest)) ++
      List(root, kingPlan).map(move =>
        val base = if move == root then 550 else 600
        supplied(f, "king-capture-" + move, kingReply :+ move,
          List("f7f6" -> base, capture -> (base + 15)), restricted = true))
  def initial(f: AnchoredFixture) = firstPools(f) ++ ownPools(f) ++ resistances(f) ++ captures(f)
  def initialCompleteSearches(f: AnchoredFixture, longest: Boolean = false) =
    firstPools(f) ++ ownPools(f) ++ resistances(f, longest) ++ captures(f, longest)
  def absentAdvanceSearches(f: AnchoredFixture, prefixes: List[List[String]] = fallbackPrefixes,
      includeNamed: Boolean = true, includeResponses: Boolean = true,
      includeCaptures: Boolean = true,
      requestedDepth: Int = 16, returnedDepth: Int = 16): List[TargetResponseInvestigation.Search] =
    prefixes.flatMap { at =>
      val key = at.mkString("-")
      // g4 is omitted from the full returned pool. The named pair is therefore
      // required and supplies its current value; the old source supplies none.
      val full = supplied(f, "absent-own-" + key, at,
        List(fallbackOther -> 700, "a3b4" -> 650, currentKingOther(at) -> 100),
        requestedDepth = requestedDepth, returnedDepth = returnedDepth)
      val pair = Option.when(includeNamed)(supplied(f, "absent-pair-" + key, at,
        List(root -> 0, fallbackOther -> 700), restricted = true,
        requestedDepth = requestedDepth, returnedDepth = returnedDepth)).toList
      val responses = if !includeResponses then Nil else
        List(root, fallbackOther).flatMap { move =>
          val base = if move == root then 0 else 700
          val after = at :+ move
          val replies = fallbackReplies(after)
          val pool = supplied(f, "absent-response-" + key + "-" + move, after,
            replies.zipWithIndex.map((reply, index) => reply -> (base + index * 10)),
            requestedDepth = requestedDepth, returnedDepth = returnedDepth)
          // Four captures are legal after the f5/g4 branch. The finite top
          // three cannot erase the fourth: keep its exact named reply duty.
          val omitted = if !includeCaptures then Nil else
            fallbackCaptures(after).filterNot(replies.contains).zipWithIndex.map { (reply, index) =>
              supplied(f, "absent-capture-" + key + "-" + move + "-" + reply, after,
                List(replies.head -> base, reply -> (base + (replies.size + index) * 10)),
                restricted = true, requestedDepth = requestedDepth, returnedDepth = returnedDepth)
            }
          pool :: omitted
        }
      full :: (pair ++ responses)
    }
  def completeSearches(f: AnchoredFixture, longest: Boolean = false) =
    initialCompleteSearches(f, longest) ++ (if longest then later(f) else Nil) ++ absentAdvanceSearches(f)
  def later(f: AnchoredFixture) =
    List((quietPrefix, quietOther, List("e6e7", "e6d6", "f6f5")),
      (checkingPrefix, checkingOther, List("f5e5", "f5g5", "f5e6"))).flatMap { (at, alternative, replies) =>
      supplied(f, "late-own-" + at.size, at, List(root -> 700, alternative -> 600)) ::
        List(root, alternative).map(move =>
          val base = if move == root then 700 else 600
          supplied(f, "late-response-" + at.size + "-" + move, at :+ move,
            replies.zipWithIndex.map((reply, index) => reply -> (base + index * 10))))
    }

class AnchoredChoiceFamilyTest extends munit.FunSuite:
  import AnchoredChoiceTestFixture.{root, keepRoot, keeping, changed}
  import AnchoredChoiceFamilyFixture.*
  import AnchoredChoiceReading.*
  import CommentaryFact.*
  import CommentaryProposal.*

  test("the original current pawn question owns two full Black duties before any witnessed king cycle"):
    val f = fixture()
    assertEquals(CommentaryIdea.reserveTimings(List(f.entry)), Nil)
    val seed = CommentaryIdea.reserveChoices(List(f.entry)).head
    val pending = read(f, Nil).get
    assertEquals(pending.owner, seed.owner)
    assertEquals(pending.pawn, Piece("white", "pawn", "g3"))
    assertEquals(pending.roots.map(_.pawnAfter.square.key), List("g4", "g3"))
    assertEquals(pending.obligations.map(_.phase).toSet, Set(-2))
    assertEquals(pending.obligations.map(_.comparison.prefixMovesUci).toSet, Set(List(root), List(keepRoot)))
    assertEquals(pending.stages, Nil)
    assert(!pending.initialComplete && !pending.complete)
    val appended = withQuestions(f.packet, f.paths, List(f.entry), Nil, Set(root, keepRoot),
      f.rootReceipt.work.engineProfile.wireId, 16).head
    assertEquals(appended.request.requirements.take(f.entry.request.requirements.size), f.entry.request.requirements)
    assertEquals(appended.readings.take(f.entry.readings.size), f.entry.readings)
    assert(CommentaryProposal.validRequests(List(appended.request)))

  test("a restricted observed root reply cannot select the first leading defence group"):
    val f = fixture()
    val pending = read(f, firstPools(f, restricted = true)).get
    assert(pending.roots.forall(_.resistance.pool.isEmpty))
    assert(pending.initialStages.isEmpty)
    assert(!pending.complete)
    assert(pending.obligations.forall(_.phase == -2))

  test("two near-leading replies become distinct decisions under one original pawn family"):
    val f = fixture()
    val current = read(f, firstPools(f)).get
    assertEquals(current.initialExpectedPrefixes, Set(blockade, kingReply))
    assertEquals(current.initialStages.map(_.stage.prefix).toSet, Set(blockade, kingReply))
    assert(current.initialStages.forall(_.stage.proof.isEmpty))
    current.initialStages.foreach { task =>
      val source = task.stage.source.get
      assert(source.belongsTo(f.packet, f.paths))
      assertEquals(source.question.prefixMovesUci :+ source.question.referenceMove, task.stage.prefix)
      assert(task.stage.anchor eq source.reference)
      assertEquals(task.stage.rootOwner.answer, current.owner.answer)
      assertEquals(task.stage.currentPawn.square.key, "g3")
    }
    assertEquals(current.owner.comparison.alternativeMove, keepRoot)
    assert(current.obligations.exists(_.phase == 0))
    assert(!current.initialComplete)

  test("response-sensitive value keeps a different own plan without borrowing its purpose or omitting a capture"):
    val f = fixture()
    val searches = initialCompleteSearches(f)
    val current = read(f, searches).get
    assert(current.initialComplete)
    assert(!current.complete, "six current legal reserves still need their own choice and resistance values")
    assert(!current.followupComplete)
    val underPawn = current.initialStages.find(_.stage.prefix == blockade).get
    val underKing = current.initialStages.find(_.stage.prefix == kingReply).get
    assertEquals(underPawn.ownQuestion.alternativeMove, keeping)
    assertEquals(underKing.ownQuestion.alternativeMove, kingPlan)
    assert(underPawn.use.get.refuted && underKing.use.get.useful)
    assertEquals(current.followupStages.map(_.stage.prefix).toSet, fallbackPrefixes.toSet)
    assert(current.followupStages.forall(task => !task.stage.witnessedAdvance && task.use.isEmpty))
    assert(current.obligations.exists(_.phase == 2))
    assertEquals(current.owner.answer, CommentaryIdea.reserveChoices(List(f.entry)).head.owner.answer)
    val completed = read(f, completeSearches(f)).get
    assert(completed.complete && completed.followupComplete)
    assertEquals(completed.followupStages.map(_.stage.prefix).toSet, fallbackPrefixes.toSet)
    assert(completed.followupStages.forall(task => task.use.get.refuted && !task.use.get.useful))
    assert(completed.followupStages.forall(_.ownQuestion.alternativeMove == fallbackOther))
    assertEquals(completed.owner.answer, current.owner.answer)
    val missing = read(f, firstPools(f) ++ ownPools(f) ++ resistances(f) ++ captures(f).dropRight(1)).get
    assert(!missing.initialComplete)
    assert(missing.initialStages.exists(_.branches.exists(_.resistance.captures.exists(_.value.isEmpty))))

  test("the complete per-owner stage union never stops a live sibling when another own plan changes"):
    val f = fixture()
    val searches = firstPools(f) ++ ownPools(f)
    val before = read(f, searches).get
    val one = before.initialStages.find(_.stage.prefix == blockade).get.ownQuestion
    val two = before.initialStages.find(_.stage.prefix == kingReply).get.ownQuestion
    val appended = withQuestions(f.packet, f.paths, List(f.entry), searches, Set(root, keepRoot),
      f.rootReceipt.work.engineProfile.wireId, 16).head
    val pending = appended.copy(readings = appended.readings.map(row =>
      if Set(one.id, two.id)(row.requirementId) then row.copy(state = "awaiting_engine") else row))
    val changedPool = supplied(f, "changed-king-plan", kingReply, List(changed -> 900, root -> 550, kingPlan -> 100))
    val updated = withQuestions(f.packet, f.paths, List(pending), searches :+ changedPool,
      Set(root, keepRoot), f.rootReceipt.work.engineProfile.wireId, 16).head
    assertEquals(updated.readings.find(_.requirementId == one.id).get.state, "awaiting_engine")
    assertEquals(updated.readings.find(_.requirementId == two.id).get.stopReason, Some("not_required_for_decision"))
    val after = read(f, searches :+ changedPool, Some(List(updated))).get
    val replacement = after.initialStages.find(_.stage.prefix == kingReply).get.ownQuestion
    assertNotEquals(replacement.id, two.id)
    assertEquals(replacement.alternativeMove, changed)
    assert(updated.request.requirements.contains(two) && updated.request.requirements.contains(replacement))
    assertEquals(updated.request.requirements.take(f.entry.request.requirements.size), f.entry.request.requirements)

  test("foreign first-pool paths cannot supply a stage or a current representative"):
    val f = fixture()
    val foreign = fixture()
    val current = read(f, firstPools(foreign)).get
    assert(current.initialStages.isEmpty && !current.complete)
    val prepared = ObservedLineProtocol.prepare(f.packet)
    val oldSearches = firstPools(f)
    val rePrepared = family(f.packet, prepared, List(f.entry), oldSearches, Set(root, keepRoot),
      f.rootReceipt.work.engineProfile.wireId, 16).get
    assert(rePrepared.initialStages.isEmpty && !rePrepared.initialComplete)
    assert(family(f.packet, foreign.paths, List(f.entry), Nil, Set(root, keepRoot),
      f.rootReceipt.work.engineProfile.wireId, 16).isEmpty)

  test("a later quiet use and checking use keep separate exact sources without a fixed e-pawn or King route"):
    val f = fixture()
    val sources = initialCompleteSearches(f, longest = true)
    val waiting = read(f, sources).get
    assert(waiting.initialComplete)
    val follow = waiting.followupStages
    val unobserved = fallbackPrefixes.toSet -- Set(blockade ++ List(keeping, "e5d6"),
      blockade ++ List(keeping, "f6f5"))
    val expected = unobserved ++ Set(quietPrefix, checkingPrefix)
    assertEquals(follow.map(_.stage.prefix).toSet, expected)
    assert(follow.forall(_.stage.level == 1))
    assert(follow.forall(_.stage.parentStageId.nonEmpty))
    follow.foreach { task =>
      assert(task.stage.anchor eq task.stage.source.get.reference)
      assert(task.stage.source.get.belongsTo(f.packet, f.paths))
      assert(task.stage.stateFacts.forall(_.at == Ply(task.stage.prefix.size)))
      assertEquals(task.stage.currentPawn.square.key, "g3")
    }
    val quiet = follow.find(_.stage.prefix == quietPrefix).get.stage
    val checking = follow.find(_.stage.prefix == checkingPrefix).get.stage
    assert(quiet.witnessedAdvance && checking.witnessedAdvance)
    val peerPrefix = blockade ++ List(keeping, "e5e6")
    val peerAdvance = follow.find(_.stage.prefix == peerPrefix).get
    assertEquals(peerAdvance.stage.anchor.moves, peerPrefix ++ List(root, "b4c3", "b2c3"))
    assert(peerAdvance.stage.witnessedAdvance,
      "longest source selection shows this current advance; it does not supply the advance's value")
    assert(follow.filter(task => (unobserved - peerPrefix)(task.stage.prefix))
      .forall(task => !task.stage.witnessedAdvance))
    assert(follow.filter(task => unobserved(task.stage.prefix)).forall(task => task.use.isEmpty && !task.complete))
    val ePawn = Piece("white", "pawn", "e4").witness
    assert(f.paths.trackedPieceAfter(quiet.anchor, ePawn, quietPrefix.size).flatten.nonEmpty)
    assertEquals(f.paths.trackedPieceAfter(checking.anchor, ePawn, checkingPrefix.size).flatten, None)
    assert(f.paths.positionView(quiet.anchor, quietPrefix.size + 1).get.currentCheckSources(chess.Black).isEmpty)
    assert(f.paths.positionView(checking.anchor, checkingPrefix.size + 1).get.currentCheckSources(chess.Black).nonEmpty)
    assert(!waiting.complete, "the later use cannot borrow its source's earlier value")
    assert(waiting.obligations.exists(_.phase == 2))
    val witnessed = read(f, sources ++ later(f)).get
    assert(!witnessed.complete, "completing two witnessed uses cannot close the other tied current choices")
    assert(witnessed.followupStages.filter(task => Set(quietPrefix, checkingPrefix)(task.stage.prefix))
      .forall(_.complete))
    assert(witnessed.followupStages.filter(task => unobserved(task.stage.prefix)).forall(task => !task.complete))
    assert(witnessed.obligations.exists(_.phase == 3))
    val finished = read(f, completeSearches(f, longest = true)).get
    assert(finished.complete && finished.followupComplete)
    assert(finished.followupStages.forall(_.complete))
    assertEquals(finished.followupStages.map(_.stage.prefix).toSet, expected)
    assertEquals(finished.owner.answer, waiting.owner.answer)

  test("an absent-PV legal advance at the defended King route still needs an exact omitted-resource pair"):
    val f = fixture()
    val at = kingReply ++ List(kingPlan, "e6e5")
    val otherPrefixes = fallbackPrefixes.filterNot(_ == at)
    val supplied = initialCompleteSearches(f) ++ absentAdvanceSearches(f, otherPrefixes) ++
      absentAdvanceSearches(f, List(at), includeNamed = false)
    val current = read(f, supplied).get
    val local = current.followupStages.find(_.stage.prefix == at).get
    assert(current.initialComplete && !current.complete)
    assert(!local.stage.witnessedAdvance)
    assert(local.stage.source.get.belongsTo(f.packet, f.paths))
    assert(local.stage.anchor eq local.stage.source.get.reference)
    assertEquals(local.stage.rootOwner.answer, current.owner.answer)
    assertEquals(local.stage.currentPawn.square.key, "g3")
    assertEquals(local.ownQuestion.referenceMove, root)
    assertEquals(local.ownQuestion.alternativeMove, fallbackOther)
    assert(local.use.isEmpty && !local.complete)
    assert(local.stage.stateFacts.forall(fact => fact.path.source == Declared(at) && fact.at == Ply(at.size)))
    val completed = read(f, supplied :+ AnchoredChoiceFamilyFixture.supplied(f,
      "new-exact-omitted-resource", at, List(root -> 0, fallbackOther -> 700), restricted = true)).get
    val answered = completed.followupStages.find(_.stage.prefix == at).get
    assert(completed.complete && answered.complete)
    assert(answered.use.get.refuted && !answered.use.get.useful)
    assertEquals(answered.use.get.answer.workId, "new-exact-omitted-resource")
    assertEquals(completed.owner.answer, current.owner.answer)

  test("both current Black response pools and an omitted capture remain required after the advance is refuted"):
    val f = fixture()
    val at = kingReply ++ List(kingPlan, "e6e5")
    val withoutResponses = initialCompleteSearches(f) ++
      absentAdvanceSearches(f, fallbackPrefixes.filterNot(_ == at)) ++
      absentAdvanceSearches(f, List(at), includeResponses = false)
    val waiting = read(f, withoutResponses).get
    val task = waiting.followupStages.find(_.stage.prefix == at).get
    assert(waiting.initialComplete && !waiting.complete)
    assert(task.use.get.refuted)
    assert(task.branches.size == 2 && task.branches.forall(_.resistance.pool.isEmpty))
    assert(task.obligations.exists(_.phase == 3))
    val all = completeSearches(f)
    val oneReplyMissing = all.filterNot(search => search.anchor.prefixMoves == (at :+ fallbackOther) &&
      search.work.rootRestriction == EngineRootRestriction.Unrestricted)
    val one = read(f, oneReplyMissing).get
    assert(one.initialComplete && !one.complete)
    assert(one.followupStages.find(_.stage.prefix == at).get.branches
      .find(_.ownMove == fallbackOther).get.resistance.pool.isEmpty)
    val omitted = read(f, initialCompleteSearches(f) ++ absentAdvanceSearches(f, includeCaptures = false)).get
    val captureStage = omitted.followupStages.find(_.stage.prefix == (blockade ++ List(keeping, "f6f5"))).get
    val spent = captureStage.branches.find(_.ownMove == root).get
    assert(omitted.initialComplete && !omitted.complete)
    assert(spent.resistance.requiredCaptureMoves.exists(move =>
      !spent.resistance.pool.get.lines.exists(_.evaluation.moves.head == move)))
    assert(spent.resistance.captures.exists(_.value.isEmpty))
    assert(read(f, all).get.complete)

  test("foreign or lower-depth local receipts cannot close the legal absent-PV choices"):
    val f = fixture()
    val foreign = fixture()
    val initial = initialCompleteSearches(f)
    val foreignValues = read(f, initial ++ absentAdvanceSearches(foreign)).get
    assert(foreignValues.initialComplete && !foreignValues.complete)
    assertEquals(foreignValues.followupStages.map(_.stage.prefix).toSet, fallbackPrefixes.toSet)
    assert(foreignValues.followupStages.forall(_.use.isEmpty))
    val lower = read(f, initial ++ absentAdvanceSearches(f, requestedDepth = 15, returnedDepth = 15)).get
    assert(lower.initialComplete && !lower.complete)
    assert(lower.followupStages.forall(_.use.isEmpty))
    assertEquals(lower.owner.answer, foreignValues.owner.answer)
    val completed = read(f, completeSearches(f)).get
    assert(completed.complete)
    assert(completed.followupStages.forall(_.stage.level == 1))
    assertEquals(completed.expectedStagePrefixes,
      completed.initialExpectedPrefixes ++ fallbackPrefixes.toSet)

  test("stopped or insufficient root numerical ownership cannot be replaced by current board labels"):
    val f = fixture()
    val stopped = f.entry.copy(readings = f.entry.readings.map(row =>
      if row.requirementId == "reserve_root" then row.copy(state = "stopped", answer = None,
        stopReason = Some("engine_report_failed")) else row))
    assert(read(f, firstPools(f), Some(List(stopped))).isEmpty)
    assert(family(f.packet, f.paths, List(f.entry), firstPools(f), Set(root, keepRoot),
      f.rootReceipt.work.engineProfile.wireId, 24).isEmpty)
    assert(family(f.packet, f.paths, List(f.entry), firstPools(f), Set(root, keepRoot), "foreign-profile", 16).isEmpty)
