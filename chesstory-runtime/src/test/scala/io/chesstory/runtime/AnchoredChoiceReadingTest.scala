package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.*
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.CanonicalPositionHistory

/** Supplied finite receipts test current selection/ownership, not engine
  * strength, automatic discovery success or the whole original score cause.
  */
private[runtime] object AnchoredChoiceTestFixture:
  import CommentaryFact.*
  import CommentaryProposal.*
  import AnchoredChoiceReading.*

  val fen = "8/5p2/8/2p1k3/ppp1P3/P1P2KP1/1P6/8 w - - 0 35"
  val root = "g3g4"
  val keepRoot = "f3e3"
  val decisionPrefix = List(keepRoot, "f7f6")
  val keeping = "e3f3"
  val other = "e3f2"
  val changed = "e3e2"
  val resistance = "e5d6"
  val peer = "e5e6"
  val pawnReply = "f6f5"
  val capture = "b4c3"
  val early = List(root, "f7f6", "f3e3")
  val kept = decisionPrefix :+ keeping
  private val legalLines = (List(early, kept, decisionPrefix :+ root, decisionPrefix :+ other, decisionPrefix :+ changed) ++
    List(root, keeping).flatMap(move => List(
      decisionPrefix ++ List(move, resistance), decisionPrefix ++ List(move, peer),
      decisionPrefix ++ List(move, pawnReply), decisionPrefix ++ List(move, capture, "b2c3")))).distinct

  case class AnchoredFixture(packet: EvidenceBackedJudgmentPacket,
      paths: ObservedLineProtocol.PreparedPaths, entry: Entry,
      rootReceipt: CommentaryInvestigation.EngineCandidates)

  def rootPacket(currentFen: String = fen, original: List[String] = early,
      alternative: List[String] = kept, lines: List[List[String]] = legalLines,
      currentHistory: Option[CanonicalPositionHistory] = None) =
    val history = currentHistory.getOrElse(CanonicalPositionHistory.from(currentFen, Nil, currentFen).toOption.get)
    val input = MoveReviewInputAdmission.admitIndependent(history, original.head,
      suppliedLines = lines.distinct.map(SuppliedCommentaryLine(CommentaryLineSource.Study, _))).get
    val work = IssuedEngineWork.create(CommentaryEngineProfile.ServerAdmitted, "supplied-root",
      EngineWorkPurpose.RootSearch, history.initialFen, history.movePrefixUci, history.currentFen,
      EngineRootRestriction.Restricted(List(original.head, alternative.head)), history.currentFen, Nil,
      EngineSearchLimits(16, 2_000_000, 2_500, 2), 3_500)
    val reported = List(alternative -> 500, original -> 0).map { (moves, cp) =>
      ReportedEngineLineSuffix(moves, ReportedWhiteEngineScore.Centipawns(cp), 16)
    }
    val physical = EngineLineAdmission.admitReportedLineSuffixes(work, reported, Some(history)).get
    val receipt = CommentaryInvestigation.EngineCandidates(work,
      EngineLineAdmission.bindAdmittedLineSuffixes(work, physical).get)
    val retained = MoveReviewInputAdmission.retainAdmittedLines(input, receipt.lines, EvaluationSource.RootSearch)
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(retained)).get
    val paths = ObservedLineProtocol.prepare(packet)
    (packet, paths, receipt)

  def fixture(): AnchoredFixture =
    val (packet, paths, receipt) = rootPacket()
    val batch = CommentaryDiscovery.discover(packet, paths, List(CommentaryDiscovery.Anchor(Nil, keepRoot)),
      legalLines.map(moves => CommentaryDiscovery.SourcePath(moves, Source("user", "anchored-choice-test"))),
      Nil, pvGuided = true)
    val generated = batch.additions.find(entry => entry.request.origin.question.producer == "C-reserve-timing" &&
      entry.request.requirements.exists {
        case value: CompareMoves => value.id == "reserve_timing" && value.alternativeMove == keeping
        case _ => false
      }).get
    val answer = CommentaryInvestigation.pairedAnswer(packet, root, keepRoot,
      receipt.work, receipt.lines, Some(paths)).get
    val rootAnswered = generated.copy(readings = generated.readings.map(reading =>
      if reading.requirementId == "reserve_root" then pairReading(
        generated.request.requirements.collectFirst { case value: CompareMoves if value.id == "reserve_root" => value }.get,
        answer) else reading))
    AnchoredFixture(packet, paths, CommentaryFactReader.read(packet, paths, List(rootAnswered)).head, receipt)

  def supplied(f: AnchoredFixture, id: String, at: List[String], values: List[(String, Int)],
      restricted: Boolean = false, requestedDepth: Int = 16, returnedDepth: Int = 16)
      : TargetResponseInvestigation.Search =
    val history = f.packet.assembly.input.positionHistory.extend(at).toOption.get
    val path = CommentaryProposal.observedPrefix(f.packet, f.paths, at).get
    val restriction = if restricted then EngineRootRestriction.Restricted(values.map(_._1))
      else EngineRootRestriction.Unrestricted
    val work = IssuedEngineWork.create(CommentaryEngineProfile.ServerAdmitted, id,
      EngineWorkPurpose.ResponseComparison, history.initialFen, history.movePrefixUci, history.currentFen,
      restriction, history.currentFen, Nil,
      EngineSearchLimits(requestedDepth, 2_000_000, 2_500, values.size), 3_500,
      Some(EngineSearchAnchor(at.head, path.ids.head, path.positions(at.size).id.get, at)))
    val reported = values.map { (move, cp) =>
      val suffix = f.paths.paths.filter(_.moves.startsWith(at :+ move))
        .sortBy(path => (path.moves.size, path.line.ref.id)).head.moves.drop(at.size)
      ReportedEngineLineSuffix(suffix, ReportedWhiteEngineScore.Centipawns(cp), returnedDepth)
    }
    val physical = EngineLineAdmission.admitReportedLineSuffixes(work, reported, Some(history)).get
    val receipt = CommentaryInvestigation.EngineCandidates(work,
      EngineLineAdmission.bindAdmittedLineSuffixes(work, physical).get)
    TargetResponseInvestigation.readSearch(f.packet, receipt, history, f.paths).get

  def read(f: AnchoredFixture, searches: List[TargetResponseInvestigation.Search], entries: List[Entry] = Nil,
      minimumDepth: Int = 16, profile: Option[String] = None) =
    task(f.packet, f.paths, if entries.isEmpty then List(f.entry) else entries, searches,
      Set(root, keepRoot), profile.getOrElse(f.rootReceipt.work.engineProfile.wireId), minimumDepth)

  def own(f: AnchoredFixture) =
    val pool = supplied(f, "own-full", decisionPrefix, List(keeping -> 500, other -> 100, changed -> 90))
    val pair = supplied(f, "own-pair", decisionPrefix, List(keeping -> 500, root -> 0), restricted = true)
    List(pool, pair)

  def opposing(f: AnchoredFixture) = List(root, keeping).map { move =>
    val base = if move == root then 0 else 500
    supplied(f, "resistance-" + move, decisionPrefix :+ move,
      List(resistance -> base, peer -> (base + 10), pawnReply -> (base + 20)))
  }

  def capturePairs(f: AnchoredFixture) = List(root, keeping).map { move =>
    val base = if move == root then 0 else 500
    supplied(f, "capture-" + move, decisionPrefix :+ move,
      List(resistance -> base, capture -> (base + 15)), restricted = true)
  }

class AnchoredChoiceReadingTest extends munit.FunSuite:
  import CommentaryFact.*
  import CommentaryProposal.*
  import AnchoredChoiceReading.*
  import AnchoredChoiceTestFixture.*

  test("a current timing context creates a shallow stage before its answer or any complete king cycle"):
    val f = fixture()
    val proof = CommentaryIdea.reserveTimings(List(f.entry)).head
    assertEquals(proof.answer, None)
    val pending = read(f, Nil).get
    assertEquals(pending.stage.prefix, decisionPrefix)
    assertEquals(pending.stage.pawn, Piece("white", "pawn", "g3"))
    assertEquals(pending.stage.currentPawn.square.key, "g3")
    assert(pending.stage.belongsTo(f.packet, f.paths))
    assertEquals(pending.use, None)
    assertEquals(pending.obligations.map(_.phase), List(0))
    assert(!pending.complete)
    assert(pending.stage.stateFacts.forall(fact => fact.path.source == Declared(decisionPrefix) && fact.at == Ply(decisionPrefix.size)))
    assertEquals(pending.stage.stateContext.members, pending.stage.stateFacts.map(_.id))

    val appended = withQuestions(f.packet, f.paths, List(f.entry), Nil, Set(root, keepRoot),
      f.rootReceipt.work.engineProfile.wireId, 16).head
    assertEquals(appended.request.requirements.take(f.entry.request.requirements.size), f.entry.request.requirements)
    assertEquals(appended.readings.take(f.entry.readings.size), f.entry.readings)
    assert(CommentaryProposal.validRequests(List(appended.request)))
    val facts = CommentaryFactReader.read(f.packet, f.paths, List(appended)).head
    assert(facts.readings.exists(row => row.requirementId == pending.obligations.head.context.id && row.state == "supported"))
    assertEquals(CommentaryIdea.reserveTimings(List(facts)).head.answer, None)

  test("a named receipt alone is not strongest-choice selection and an omitted advance receives its own value"):
    val f = fixture()
    val values = own(f)
    assert(read(f, values.tail).get.use.isEmpty)
    val current = read(f, values).get
    assertEquals(current.ownQuestion.alternativeMove, keeping)
    assert(current.use.get.refuted)
    assertEquals(current.use.get.answer.workId, "own-pair")
    assertEquals(current.use.get.survey.work.workId, "own-full")
    assertEquals(current.branches.map(_.ownMove).toSet, Set(root, keeping))
    val spent = current.branches.find(_.ownMove == root).get
    val kept = current.branches.find(_.ownMove == keeping).get
    assert(spent.spendsAdvance && !spent.keepsPawnHere)
    assertEquals(spent.pawnAfter.square.key, "g4")
    assert(!kept.spendsAdvance && kept.keepsPawnHere)
    assertEquals(kept.pawnAfter.square.key, "g3")
    assert(!current.complete, "each own action still needs independently owned immediate resistance")

  test("the current full pool can itself own both values without a duplicate pair"):
    val f = fixture()
    val full = supplied(f, "one-full-owner", decisionPrefix, List(keeping -> 500, other -> 100, root -> 0))
    val current = read(f, List(full)).get
    assert(current.use.nonEmpty)
    assert(current.use.get.survey eq current.use.get.comparison)
    assertEquals(current.use.get.question.alternativeMove, keeping)

  test("both current response leaders and a known omitted legal capture must be valued before local completion"):
    val f = fixture()
    val basic = own(f) ++ opposing(f)
    val pending = read(f, basic).get
    assert(!pending.complete)
    assert(pending.branches.forall(_.resistance.leaderMove.contains(resistance)))
    assert(pending.branches.forall(_.resistance.requiredCaptureMoves == Set(capture)))
    assert(pending.branches.forall(_.resistance.captures.exists(value => value.move == capture && value.value.isEmpty)))
    assert(pending.obligations.exists(value => value.comparison.referenceMove == capture &&
      value.comparison.alternativeMove == resistance))
    val completed = read(f, basic ++ capturePairs(f)).get
    assert(completed.complete)
    assert(completed.branches.forall(branch => branch.resistance.value.exists(value =>
      value.belongsTo(f.packet, f.paths) && value.question.prefixMovesUci == branch.afterChoicePrefix)))
    assert(completed.branches.forall(_.resistance.captures.forall(_.value.nonEmpty)))
    val missingOne = read(f, basic ++ capturePairs(f).take(1)).get
    assert(!missingOne.complete, "the other action's capture value cannot be borrowed")

  test("current actual depth, profile, prepared owner and history are mandatory"):
    val f = fixture()
    val full = supplied(f, "low-full", decisionPrefix, List(keeping -> 500, other -> 100, changed -> 90))
    val overrun = supplied(f, "overrun-pair", decisionPrefix, List(keeping -> 500, root -> 0),
      restricted = true, requestedDepth = 16, returnedDepth = 24)
    assert(read(f, List(full, overrun)).get.use.isEmpty)
    assertEquals(CommentaryChoiceValue.selectionDepth(f.packet, prefixQuestion(f), List(full, overrun),
      f.rootReceipt.work.engineProfile.wireId, 16), 24)
    assert(read(f, own(f), minimumDepth = 24).isEmpty)
    assert(read(f, own(f), profile = Some("foreign-profile")).isEmpty)
    val foreign = fixture()
    assert(read(f, own(foreign)).get.use.isEmpty)
    assert(task(f.packet, foreign.paths, List(f.entry), Nil, Set(root, keepRoot),
      f.rootReceipt.work.engineProfile.wireId, 16).isEmpty)
    val rePrepared = ObservedLineProtocol.prepare(f.packet)
    assert(task(f.packet, rePrepared, List(f.entry), Nil, Set(root, keepRoot),
      f.rootReceipt.work.engineProfile.wireId, 16).nonEmpty,
      "the canonical stage can be freshly prepared without borrowing old search paths")
    val oldSearches = own(f)
    val oldPool = CommentaryChoiceValue.survey(f.packet, decisionPrefix, oldSearches,
      f.rootReceipt.work.engineProfile.wireId, 16).get
    assert(oldPool.belongsTo(f.packet))
    assert(oldPool.pathsByMove.values.forall(path => !rePrepared.paths.exists(_ eq path)))
    assertEquals(CommentaryChoiceValue.strongestOther(f.packet, rePrepared, oldPool, root), None)
    assert(task(f.packet, rePrepared, List(f.entry), oldSearches, Set(root, keepRoot),
      f.rootReceipt.work.engineProfile.wireId, 16).isEmpty,
      "an old pool cannot choose a representative for another preparation")

  private def prefixQuestion(f: AnchoredFixture) = read(f, Nil).get.ownQuestion

  test("a new strongest other plan adds an immutable question without changing the original or old closed answer"):
    val f = fixture()
    val first = own(f)
    val before = read(f, first).get
    val appended = withQuestions(f.packet, f.paths, List(f.entry), first, Set(root, keepRoot),
      f.rootReceipt.work.engineProfile.wireId, 16).head
    val owned = appended.copy(readings = appended.readings.map(reading =>
      if reading.requirementId == before.ownQuestion.id then pairReading(before.ownQuestion, before.use.get.answer)
      else reading))
    val changedPool = supplied(f, "changed-full", decisionPrefix, List(changed -> 700, other -> 100, keeping -> 90))
    val after = read(f, first :+ changedPool, List(owned)).get
    assertEquals(after.ownQuestion.alternativeMove, changed)
    assertNotEquals(after.ownQuestion.id, before.ownQuestion.id)
    assertEquals(after.use, None)
    val rewritten = withQuestions(f.packet, f.paths, List(owned), first :+ changedPool, Set(root, keepRoot),
      f.rootReceipt.work.engineProfile.wireId, 16).head
    assertEquals(rewritten.request.requirements.take(f.entry.request.requirements.size), f.entry.request.requirements)
    assertEquals(rewritten.readings.find(_.requirementId == before.ownQuestion.id),
      owned.readings.find(_.requirementId == before.ownQuestion.id))
    assert(rewritten.request.requirements.contains(before.ownQuestion))
    assert(rewritten.request.requirements.contains(after.ownQuestion))

  private def rebound(f: AnchoredFixture, currentFen: String, alternative: List[String]) =
    val (packet, paths, receipt) = rootPacket(currentFen, early, alternative, List(early, alternative))
    val rootQuestion = CompareMoves("reserve_root", Nil, root, alternative.head)
    val answer = CommentaryInvestigation.pairedAnswer(packet, root, alternative.head,
      receipt.work, receipt.lines, Some(paths)).get
    // These forged supported readings deliberately try to retain an old
    // question. The new reader must still check the exact physical pawn.
    val request = f.entry.request.copy(requirements = f.entry.request.requirements.map {
      case value: CompareMoves if value.id == "reserve_root" => rootQuestion
      case value: CompareMoves if value.id == "reserve_timing" =>
        value.copy(prefixMovesUci = alternative.take(2), alternativeMove = alternative(2))
      case value => value
    })
    val entry = f.entry.copy(request = request, readings = f.entry.readings.map { reading =>
      if reading.requirementId == "reserve_root" then pairReading(rootQuestion, answer)
      else if request.requirements.exists(value => value.id == reading.requirementId &&
          (value.isInstanceOf[Fact] || value.isInstanceOf[FactGroup])) then
        reading.copy(state = "supported", stopReason = None, result = None)
      else reading
    })
    AnchoredFixture(packet, paths, entry, receipt)

  test("a removed or already spent original pawn cannot be replaced by supplied supported context labels"):
    val f = fixture()
    val removed = rebound(f, "6r1/5p2/8/2p1k3/ppp1P3/P1P2KP1/1P6/8 w - - 0 35",
      List(keepRoot, "g8g3", "e3e2"))
    assert(CommentaryIdea.reserveTimings(List(removed.entry)).nonEmpty)
    assert(task(removed.packet, removed.paths, List(removed.entry), Nil, Set(root, keepRoot),
      removed.rootReceipt.work.engineProfile.wireId, 16).isEmpty)
    val spent = rebound(f, "8/5p2/8/2p1k3/ppp1P2p/P1P2KP1/1P6/8 w - - 0 35",
      List("g3h4", "f7f6", "f3e3"))
    assert(CommentaryIdea.reserveTimings(List(spent.entry)).nonEmpty)
    assert(task(spent.packet, spent.paths, List(spent.entry), Nil, Set(root, "g3h4"),
      spent.rootReceipt.work.engineProfile.wireId, 16).isEmpty)

  test("same placement through another canonical history cannot reuse the old root receipt or stage"):
    val f = fixture()
    val history = f.packet.assembly.input.positionHistory.extend(List(
      "f3e3", "e5d6", "e3f3", "d6e5")).toOption.get
    val (packet, paths, _) = rootPacket(currentHistory = Some(history))
    assert(task(packet, paths, List(f.entry), Nil, Set(root, keepRoot),
      f.rootReceipt.work.engineProfile.wireId, 16).isEmpty)

  test("a later owned capture that refutes the current pool leader leaves the local task unresolved"):
    val f = fixture()
    val before = own(f) ++ opposing(f) ++ capturePairs(f)
    assert(read(f, before).get.complete)
    val challenger = supplied(f, "capture-refutes-leader", decisionPrefix :+ root,
      List(capture -> -500, resistance -> 0), restricted = true)
    val answer = challenger.pairAnswer(f.packet, resistance, capture).get
    assertEquals(answer.evaluationRelation, "better")
    assertEquals(answer.qualityBand, Some(PrincipalVariationComparison.QualityBand.Separated))
    assert(!read(f, before :+ challenger).exists(_.complete),
      "testing every capture does not preserve a leader that a later same-depth pair refutes")
