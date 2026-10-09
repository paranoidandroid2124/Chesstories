package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.*
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.CanonicalPositionHistory

/** Supplied values exercise ownership and selection, not engine strength. */
class CommentaryChoiceValueTest extends munit.FunSuite:
  import CommentaryProposal.CompareMoves
  import CommentaryChoiceValue.*

  private val fen = "3r2k1/8/8/3p4/4P3/5N2/3P4/3Q1B1K w - - 0 1"
  private val root = "d2d3"
  private val resource = "d5e4"
  private val leader = "d8d7"
  private val other = "g8h8"
  private val lines = List(List(root, resource, "d3e4"), List(root, leader), List(root, other))

  private def packet() =
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    val input = MoveReviewInputAdmission.admitIndependent(history, root,
      suppliedLines = lines.map(SuppliedCommentaryLine(CommentaryLineSource.Study, _))).get
    EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(input)).get

  private def receipt(source: EvidenceBackedJudgmentPacket, id: String,
      values: List[(String, Int)], restricted: Boolean = false, depth: Int = 16,
      explicitAnchor: Boolean = false)
      : CommentaryInvestigation.EngineCandidates =
    val paths = ObservedLineProtocol.prepare(source)
    val path = paths.certifiedPath(source, lines.head).get
    val child = source.assembly.input.positionHistory.extend(List(root)).toOption.get
    val restriction = if restricted then EngineRootRestriction.Restricted(values.map(_._1))
      else EngineRootRestriction.Unrestricted
    val work = IssuedEngineWork.create(CommentaryEngineProfile.ServerAdmitted, id,
      EngineWorkPurpose.ResponseComparison, child.initialFen, child.movePrefixUci, child.currentFen,
      restriction, child.currentFen, Nil, EngineSearchLimits(depth, 2_000_000, 2_500, values.size), 3_500,
      Some(EngineSearchAnchor(root, path.ids.head, path.positions(1).id.get,
        if explicitAnchor then List(root) else Nil)))
    val reported = values.map { (move, cp) => ReportedEngineLineSuffix(
      lines.find(_(1) == move).get.tail, ReportedWhiteEngineScore.Centipawns(cp), depth) }
    val physical = EngineLineAdmission.admitReportedLineSuffixes(work, reported, Some(child)).get
    CommentaryInvestigation.EngineCandidates(work, EngineLineAdmission.bindAdmittedLineSuffixes(work, physical).get)

  private def returnedAt(source: EvidenceBackedJudgmentPacket, original: CommentaryInvestigation.EngineCandidates,
      depths: List[Int]): CommentaryInvestigation.EngineCandidates =
    val work = original.work
    val child = source.assembly.input.positionHistory.extend(List(root)).toOption.get
    val supplied = Map(resource -> -300, leader -> -310, other -> 0)
    val reported = original.lines.zip(depths).map { (line, depth) => ReportedEngineLineSuffix(
      line.evaluation.moves, ReportedWhiteEngineScore.Centipawns(supplied(line.evaluation.moves.head)), depth) }
    val physical = EngineLineAdmission.admitReportedLineSuffixes(work, reported, Some(child)).get
    CommentaryInvestigation.EngineCandidates(work, EngineLineAdmission.bindAdmittedLineSuffixes(work, physical).get)

  private def search(source: EvidenceBackedJudgmentPacket, paths: ObservedLineProtocol.PreparedPaths,
      receipt: CommentaryInvestigation.EngineCandidates) =
    val child = source.assembly.input.positionHistory.extend(List(root)).toOption.get
    TargetResponseInvestigation.readSearch(source, receipt, child, paths).get

  private def question(alternative: String = leader) = CompareMoves("local_value", List(root), resource, alternative)

  test("a full survey selects an omitted resource's strongest other plan, then one pair owns both values"):
    val source = packet()
    val paths = ObservedLineProtocol.prepare(source)
    val pool = search(source, paths, receipt(source, "pool", List(leader -> -300, other -> 0)))
    val pair = search(source, paths, receipt(source, "pair", List(resource -> 100, leader -> -290), restricted = true))
    assertEquals(strongestOther(source, paths, pool, resource), Some(leader))
    assert(pool.pairAnswer(source, resource, leader).isEmpty, "an omitted value is not borrowed from another work")
    val answer = read(source, paths, question(), List(pool, pair), pool.work.engineProfile.wireId, 16).get
    assert(answer.refuted && !answer.useful)
    assertEquals(answer.answer.workId, "pair")
    assertEquals(answer.answer.executionKeySha256, pair.work.executionKeySha256)
    assertEquals(answer.answer.referenceEvaluation, pair.pairAnswer(source, resource, leader).get.referenceEvaluation)
    assertEquals(answer.answer.alternativeEvaluation, pair.pairAnswer(source, resource, leader).get.alternativeEvaluation)
    assertEquals(answer.reference.moves.take(2), List(root, resource))
    assertEquals(answer.alternative.moves.take(2), List(root, leader))
    assert(answer.belongsTo(source, paths))

  test("a default first-child survey accepts only its exact explicit-prefix omitted-resource pair"):
    val source = packet()
    val paths = ObservedLineProtocol.prepare(source)
    val pool = search(source, paths, receipt(source, "default-first-pool", List(leader -> -300, other -> 0)))
    val pair = search(source, paths, receipt(source, "explicit-first-pair",
      List(resource -> 100, leader -> -290), restricted = true, explicitAnchor = true))
    assertNotEquals(pool.anchor, pair.anchor, "the normal factories preserve their different wire forms")
    assertEquals(pool.anchor.prefixMoves, pair.anchor.prefixMoves)
    assertNotEquals(pool.work.executionKeySha256, pair.work.executionKeySha256)
    assert(pool.pairAnswer(source, resource, leader).isEmpty)
    val answer = read(source, paths, question(), List(pool, pair), pool.work.engineProfile.wireId, 16).get
    assert(answer.refuted && !answer.useful)
    assert(answer.survey eq pool)
    assert(answer.comparison eq pair)
    assertEquals(answer.answer, pair.pairAnswer(source, resource, leader).get)
    assertEquals(answer.answer.executionKeySha256, pair.work.executionKeySha256)
    assertEquals(read(source, paths, question(), List(pool, pair), "foreign-profile", 16), None)
    assertEquals(read(source, paths, question(), List(pool, pair), pool.work.engineProfile.wireId, 17), None)
    val deeper = search(source, paths, receipt(source, "explicit-first-deeper-pair",
      List(resource -> 100, leader -> -290), restricted = true, depth = 17, explicitAnchor = true))
    assertEquals(read(source, paths, question(), List(pool, deeper), pool.work.engineProfile.wireId, 16), None)
    assertEquals(CommentaryComparisonConflict.betweenCurrentValues(source, pool, deeper), Nil)

  test("a material explicit-prefix counter withdraws a default pool's old useful resource value"):
    val source = packet()
    val paths = ObservedLineProtocol.prepare(source)
    val pool = search(source, paths, receipt(source, "default-useful-pool",
      List(resource -> -300, leader -> -310, other -> 0)))
    val old = read(source, paths, question(), List(pool), pool.work.engineProfile.wireId, 16).get
    assert(old.useful)
    val pair = search(source, paths, receipt(source, "explicit-refuting-pair",
      List(resource -> 100, leader -> -290), restricted = true, explicitAnchor = true))
    val before = pool.pairAnswer(source, resource, leader).get
    assert(pair.pairAnswer(source, resource, leader).get.qualityBand.contains(
      PrincipalVariationComparison.QualityBand.Separated))
    val current = List(pool, pair)
    assertEquals(read(source, paths, question(), current, pool.work.engineProfile.wireId, 16), None,
      "the old useful value cannot survive a same-scope later material refutation")
    val conflicts = CommentaryComparisonConflict.read(source, current, 16)
    assert(conflicts.exists(value => value.move == resource &&
      value.workIds == List(pool.work.workId, pair.work.workId)))
    assertEquals(pool.pairAnswer(source, resource, leader), Some(before), "old receipts stay immutable")
    assertEquals(old.answer, before)

  test("a named pair cannot rank the whole legal class and a legal seed cannot replace its strongest member"):
    val source = packet()
    val paths = ObservedLineProtocol.prepare(source)
    val pair = search(source, paths, receipt(source, "pair-only", List(resource -> 0, other -> 20), restricted = true))
    assert(read(source, paths, question(other), List(pair), pair.work.engineProfile.wireId, 16).isEmpty)
    val pool = search(source, paths, receipt(source, "full", List(leader -> -300, other -> 0)))
    assert(read(source, paths, question(other), List(pool, pair), pool.work.engineProfile.wireId, 16).isEmpty)

  test("a current full survey can itself own a close resource comparison without a duplicate query"):
    val source = packet()
    val paths = ObservedLineProtocol.prepare(source)
    val pool = search(source, paths, receipt(source, "one-owner", List(resource -> -300, leader -> -310, other -> 0)))
    val answer = read(source, paths, question(), List(pool), pool.work.engineProfile.wireId, 16).get
    assert(answer.useful && !answer.refuted)
    assert(answer.survey eq answer.comparison)
    assertEquals(adequateReplies(source, pool).toSet, Set(resource, leader))

  test("a higher-depth pair, stale survey, changed prefix or foreign owner cannot certify the local choice"):
    val source = packet()
    val paths = ObservedLineProtocol.prepare(source)
    val low = search(source, paths, receipt(source, "low-pool", List(leader -> -300, other -> 0)))
    val high = search(source, paths, receipt(source, "high-pair", List(resource -> -300, leader -> -310), true, 24))
    assert(read(source, paths, question(), List(low, high), low.work.engineProfile.wireId, 16).isEmpty)
    val pair = search(source, paths, receipt(source, "matching-pair", List(resource -> -300, leader -> -310), true))
    assert(read(source, paths, question(), List(low, pair), low.work.engineProfile.wireId, 24).isEmpty)
    assert(read(source, paths, question().copy(prefixMovesUci = List("d2d4")),
      List(low, pair), low.work.engineProfile.wireId, 16).isEmpty)
    val replacement = packet()
    assert(read(replacement, ObservedLineProtocol.prepare(replacement), question(),
      List(low, pair), low.work.engineProfile.wireId, 16).isEmpty)
    assert(read(source, ObservedLineProtocol.prepare(source), question(),
      List(low, pair), low.work.engineProfile.wireId, 16).isEmpty)

  test("a newer sufficient survey changes the representative without rewriting an older answer"):
    val source = packet()
    val paths = ObservedLineProtocol.prepare(source)
    val old = search(source, paths, receipt(source, "old-pool", List(leader -> -300, resource -> -290)))
    val before = read(source, paths, question(), List(old), old.work.engineProfile.wireId, 16).get
    val current = search(source, paths, receipt(source, "new-pool", List(other -> -600, leader -> -300)))
    assert(read(source, paths, question(), List(old, current), old.work.engineProfile.wireId, 16).isEmpty)
    assertEquals(before.answer.workId, "old-pool")
    assertEquals(strongestOther(source, paths, current, resource), Some(other))

  test("requested depth does not hide an overrun or mixed completed numerical level"):
    val source = packet()
    val paths = ObservedLineProtocol.prepare(source)
    val poolReceipt = receipt(source, "target16-pool", List(leader -> -310, other -> 0))
    val pairReceipt = receipt(source, "target16-pair", List(resource -> -300, leader -> -310), true)
    val pool = search(source, paths, poolReceipt)
    val overrun = search(source, paths, returnedAt(source, pairReceipt, List(24, 24)))
    assert(read(source, paths, question(), List(pool, overrun), pool.work.engineProfile.wireId, 16).isEmpty)
    assertEquals(selectionDepth(source, question(), List(pool, overrun), pool.work.engineProfile.wireId, 16), 24,
      "the current full class must be surveyed at the pair's actual completed level")
    val mixed = returnedAt(source, poolReceipt, List(16, 24))
    val child = source.assembly.input.positionHistory.extend(List(root)).toOption.get
    assert(TargetResponseInvestigation.readSearch(source, mixed, child, paths).isEmpty,
      "mixed levels are rejected at the existing Search admission boundary")
    val matched = search(source, paths, returnedAt(source, poolReceipt, List(24, 24)))
    assert(read(source, paths, question(), List(matched, overrun), pool.work.engineProfile.wireId, 16).nonEmpty)
    assert(survey(source, List(root), List(matched), pool.work.engineProfile.wireId, 24).nonEmpty,
      "a completed homogeneous level is not discarded merely because its requested minimum was lower")

  test("an owned later separated counter retires the latest full pool without falling back to an older leader"):
    val source = packet()
    val paths = ObservedLineProtocol.prepare(source)
    val earlier = search(source, paths, receipt(source, "earlier", List(leader -> -300, other -> 0)))
    val pool = search(source, paths, receipt(source, "current", List(leader -> -300, other -> 0)))
    val counter = search(source, paths, receipt(source, "counter", List(resource -> -700, leader -> -300), true))
    val current = List(earlier, pool, counter)
    assertEquals(survey(source, List(root), current, pool.work.engineProfile.wireId, 16), None)
    assertEquals(read(source, paths, question(), current, pool.work.engineProfile.wireId, 16), None)
    assertEquals(pairDepth(source, List(root), current, pool.work.engineProfile.wireId, 16), 16)
    assert(counter.pairAnswer(source, leader, resource).get.qualityBand.contains(
      PrincipalVariationComparison.QualityBand.Separated))

  test("a refreshed selection cannot reuse the old omitted counter as its current value"):
    val source = packet()
    val paths = ObservedLineProtocol.prepare(source)
    val first = search(source, paths, receipt(source, "first", List(leader -> -300, other -> 0)))
    val counter = search(source, paths, receipt(source, "counter", List(resource -> -700, leader -> -300), true))
    val refreshed = search(source, paths, receipt(source, "refreshed", List(leader -> -300, other -> 0)))
    val before = List(first, counter, refreshed)
    assert(survey(source, List(root), before, refreshed.work.engineProfile.wireId, 16).contains(refreshed))
    assert(read(source, paths, question(), before, refreshed.work.engineProfile.wireId, 16).isEmpty)
    val current = search(source, paths, receipt(source, "new-pair", List(resource -> -250, leader -> -300), true))
    assert(read(source, paths, question(), before :+ current, refreshed.work.engineProfile.wireId, 16).nonEmpty)

  test("retiring a completed higher-level pool does not silently lower the next selection request"):
    val source = packet()
    val paths = ObservedLineProtocol.prepare(source)
    val high = search(source, paths, receipt(source, "higher", List(leader -> -300, other -> 0), depth = 24))
    val counter = search(source, paths, receipt(source, "counter", List(resource -> -700, leader -> -300), true, 24))
    assert(survey(source, List(root), List(high, counter), high.work.engineProfile.wireId, 16).isEmpty)
    assertEquals(pairDepth(source, List(root), List(high, counter), high.work.engineProfile.wireId, 16), 24)
