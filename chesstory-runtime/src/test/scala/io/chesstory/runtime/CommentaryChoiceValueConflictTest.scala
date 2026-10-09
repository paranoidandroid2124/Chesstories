package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.*
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.CanonicalPositionHistory

/** Legal supplied receipts reproduce the 541/315 ownership boundary. They
  * make no claim about engine strength or automatic H05 explanation success.
  */
class CommentaryChoiceValueConflictTest extends munit.FunSuite:
  import CommentaryProposal.CompareMoves
  import CommentaryChoiceValue.*
  import TargetResponseInvestigation.Search

  private val fen = "8/5p2/8/2p1k3/ppp1P3/P1P2KP1/1P6/8 w - - 0 35"
  private val root = "f3e3"
  private val decisionPrefix = List(root, "e5e6")
  private val resource = "g3g4"
  private val leader = "e3f2"
  private val other = "e3e2"
  private val peer = "e3f4"
  private val shared = decisionPrefix ++ List(leader, "e6f6", "f2e3", "f6e5", "e3f3")
  private val referenceContinuation = shared :+ "e5e6"
  private val alternativeContinuation = shared :+ "e5f6"
  private val simple = List(resource, leader, other, peer).map(move => decisionPrefix :+ move)
  private val legalLines = (simple ++ List(referenceContinuation, alternativeContinuation,
    decisionPrefix ++ List(resource, "b4c3", "b2c3"))).distinct

  private def packet() =
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    val input = MoveReviewInputAdmission.admitIndependent(history, root,
      suppliedLines = legalLines.map(SuppliedCommentaryLine(CommentaryLineSource.Study, _))).get
    EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(input)).get

  private def receipt(source: EvidenceBackedJudgmentPacket, paths: ObservedLineProtocol.PreparedPaths,
      id: String, at: List[String], values: List[(List[String], Int)],
      restricted: Boolean = false, depth: Int = 17, defaultFirstAnchor: Boolean = false): Search =
    val history = source.assembly.input.positionHistory.extend(at).toOption.get
    val path = CommentaryProposal.observedPrefix(source, paths, at).get
    val restriction = if restricted then EngineRootRestriction.Restricted(values.map(_._1.head))
      else EngineRootRestriction.Unrestricted
    val work = IssuedEngineWork.create(CommentaryEngineProfile.ServerAdmitted, id,
      EngineWorkPurpose.ResponseComparison, history.initialFen, history.movePrefixUci, history.currentFen,
      restriction, history.currentFen, Nil, EngineSearchLimits(depth, 2_000_000, 2_500, values.size), 3_500,
      Some(EngineSearchAnchor(at.head, path.ids.head, path.positions(at.size).id.get,
        if defaultFirstAnchor then { require(at.size == 1); Nil } else at)))
    val reported = values.map { (moves, cp) =>
      ReportedEngineLineSuffix(moves, ReportedWhiteEngineScore.Centipawns(cp), depth)
    }
    val physical = EngineLineAdmission.admitReportedLineSuffixes(work, reported, Some(history)).get
    val supplied = CommentaryInvestigation.EngineCandidates(work,
      EngineLineAdmission.bindAdmittedLineSuffixes(work, physical).get)
    TargetResponseInvestigation.readSearch(source, supplied, history, paths).get

  private def pool(source: EvidenceBackedJudgmentPacket, paths: ObservedLineProtocol.PreparedPaths,
      id: String = "pool-541", cp: Int = 541, depth: Int = 17): Search =
    receipt(source, paths, id, decisionPrefix, List(
      referenceContinuation.drop(decisionPrefix.size) -> cp,
      List(other) -> (cp - 5), List(peer) -> (cp - 11)), depth = depth)

  private def pair(source: EvidenceBackedJudgmentPacket, paths: ObservedLineProtocol.PreparedPaths,
      id: String = "pair-315", cp: Int = 315, depth: Int = 17): Search =
    receipt(source, paths, id, decisionPrefix,
      List(List(resource) -> 0, alternativeContinuation.drop(decisionPrefix.size) -> cp),
      restricted = true, depth = depth)

  private val question = CompareMoves("owned_stage_value", decisionPrefix, resource, leader)

  private val firstReplies = List("e5e6", "f7f6", "e5f6")

  private def firstChildPacket() =
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    val input = MoveReviewInputAdmission.admitIndependent(history, root,
      suppliedLines = firstReplies.map(move =>
        SuppliedCommentaryLine(CommentaryLineSource.Study, List(root, move)))).get
    EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(input)).get

  test("default and explicit first-child measurements share one conflict and refresh epoch"):
    val source = firstChildPacket()
    val paths = ObservedLineProtocol.prepare(source)
    val prefix = List(root)
    val local = CompareMoves("first_child_value", prefix, "e5e6", "f7f6")
    val survey = receipt(source, paths, "default-first-541", prefix,
      List(List("f7f6") -> 541, List("e5f6") -> 545), defaultFirstAnchor = true)
    val compared = receipt(source, paths, "explicit-first-315", prefix,
      List(List("e5e6") -> 1000, List("f7f6") -> 315), restricted = true)
    assertNotEquals(survey.anchor, compared.anchor)
    assertEquals(survey.anchor.prefixMoves, compared.anchor.prefixMoves)
    assertNotEquals(survey.work.executionKeySha256, compared.work.executionKeySha256)
    val before = compared.pairAnswer(source, "e5e6", "f7f6").get
    val current = List(survey, compared)
    val conflicts = CommentaryComparisonConflict.read(source, current, 17)
    assertEquals(conflicts.map(_.move), List("f7f6"))
    assertEquals(conflicts.head.workIds, List(survey.work.workId, compared.work.workId))
    assertEquals(conflicts.head.comparison, None, "no divergent child is invented for an identical line")
    assertEquals(read(source, paths, local, current, survey.work.engineProfile.wireId, 17), None)
    val renewed = receipt(source, paths, "default-first-refresh", prefix,
      List(List("f7f6") -> 315, List("e5f6") -> 320), defaultFirstAnchor = true)
    val refreshed = current :+ renewed
    assertEquals(CommentaryComparisonConflict.read(source, refreshed, 17), Nil)
    assertEquals(read(source, paths, local, refreshed, survey.work.engineProfile.wireId, 17), None,
      "a pre-refresh named receipt cannot become a current omitted-resource value")
    val fresh = receipt(source, paths, "explicit-fresh-pair", prefix,
      List(List("e5e6") -> 1000, List("f7f6") -> 315), restricted = true)
    val checked = read(source, paths, local, refreshed :+ fresh, survey.work.engineProfile.wireId, 17).get
    assert(checked.survey eq renewed)
    assert(checked.comparison eq fresh)
    assertEquals(checked.answer, fresh.pairAnswer(source, "e5e6", "f7f6").get)
    assertEquals(compared.pairAnswer(source, "e5e6", "f7f6"), Some(before))

  test("normal factory work with foreign first-child history or occurrence fails before numerical reuse"):
    val source = firstChildPacket()
    val paths = ObservedLineProtocol.prepare(source)
    val path = CommentaryProposal.observedPrefix(source, paths, List(root)).get
    val child = source.assembly.input.positionHistory.extend(List(root)).toOption.get
    val own = EngineSearchAnchor(root, path.ids.head, path.positions(1).id.get, List(root))
    def admitted(id: String, history: CanonicalPositionHistory, anchor: EngineSearchAnchor): Option[Search] =
      val work = IssuedEngineWork.create(CommentaryEngineProfile.ServerAdmitted, id,
        EngineWorkPurpose.ResponseComparison, history.initialFen, history.movePrefixUci, history.currentFen,
        EngineRootRestriction.Restricted(List("e5e6", "f7f6")), history.currentFen, Nil,
        EngineSearchLimits(17, 2_000_000, 2_500, 2), 3_500, Some(anchor))
      val reported = List(List("e5e6") -> 1000, List("f7f6") -> 315).map { (moves, cp) =>
        ReportedEngineLineSuffix(moves, ReportedWhiteEngineScore.Centipawns(cp), 17)
      }
      val physical = EngineLineAdmission.admitReportedLineSuffixes(work, reported, Some(history)).get
      val candidates = CommentaryInvestigation.EngineCandidates(work,
        EngineLineAdmission.bindAdmittedLineSuffixes(work, physical).get)
      TargetResponseInvestigation.readSearch(source, candidates, history, paths)
    assert(admitted("owned-explicit-first", child, own).nonEmpty)
    assertEquals(admitted("foreign-first-occurrence", child,
      own.copy(rootOccurrenceId = "foreign-root-occurrence")), None)
    assertEquals(admitted("foreign-first-position", child,
      own.copy(positionId = path.positions.head.id.get)), None)
    assertEquals(admitted("foreign-first-prefix", child,
      own.copy(prefixMovesUci = List(root, "e5e6"))), None)
    val cycle = source.assembly.input.positionHistory.extend(
      List(root, "e5e6", "e3f3", "e6e5", root)).toOption.get
    assertEquals(admitted("different-whole-history", cycle, own), None)

  test("same-level owned pool 541 and pair 315 cannot certify a current choice"):
    val source = packet()
    val paths = ObservedLineProtocol.prepare(source)
    val survey = pool(source, paths)
    val compared = pair(source, paths)
    val searches = List(survey, compared)
    val profile = survey.work.engineProfile.wireId
    assertEquals(strongestOther(source, paths, survey, resource), Some(leader))
    assert(compared.pairAnswer(source, resource, leader).nonEmpty)
    assert(read(source, paths, question, searches, profile, 17).isEmpty)
    val missing = conflict(source, paths, question, searches, profile, 17).get
    assertEquals(missing.prefix, decisionPrefix)
    assertEquals(missing.move, leader)
    assertEquals(missing.depth, 17)
    assertEquals(missing.workIds, List("pool-541", "pair-315"))
    assertEquals(missing.comparison.get.prefixMovesUci, shared)
    assertEquals(missing.comparison.get.referenceMove, "e5e6")
    assertEquals(missing.comparison.get.alternativeMove, "e5f6")
    assertEquals(CommentaryComparisonConflict.read(source, searches, 17), List(missing))
    assertEquals(missing.entry.get.request.origin.question.sourceRefs, missing.workIds)

  test("an answered divergent child cannot supply a parent value or close the selection conflict"):
    val source = packet()
    val paths = ObservedLineProtocol.prepare(source)
    val survey = pool(source, paths)
    val compared = pair(source, paths)
    val searches = List(survey, compared)
    val before = compared.pairAnswer(source, resource, leader).get
    val missing = conflict(source, paths, question, searches, survey.work.engineProfile.wireId, 17).get
    val child = receipt(source, paths, "child-only", missing.comparison.get.prefixMovesUci,
      List(List("e5e6") -> 9000, List("e5f6") -> 0), restricted = true)
    assert(child.pairAnswer(source, "e5e6", "e5f6").nonEmpty)
    assert(read(source, paths, question, searches :+ child, survey.work.engineProfile.wireId, 17).isEmpty)
    assertEquals(compared.pairAnswer(source, resource, leader), Some(before))
    assertEquals(conflict(source, paths, question, searches :+ child,
      survey.work.engineProfile.wireId, 17), Some(missing))

  test("a named retry cannot erase the unresolved pool epoch"):
    val source = packet()
    val paths = ObservedLineProtocol.prepare(source)
    val survey = pool(source, paths)
    val compared = pair(source, paths)
    val repeated = pair(source, paths, "pair-retry", 541)
    val searches = List(survey, compared, repeated)
    assert(read(source, paths, question, searches, survey.work.engineProfile.wireId, 17).isEmpty)
    assert(conflict(source, paths, question, searches, survey.work.engineProfile.wireId, 17).nonEmpty)
    assert(CommentaryComparisonConflict.read(source, searches, 17).exists(_.workIds.contains("pair-315")))

  test("different owned values on the same continuation require a full refresh without inventing a child"):
    val source = packet()
    val paths = ObservedLineProtocol.prepare(source)
    val survey = pool(source, paths)
    val samePath = receipt(source, paths, "same-path-pair", decisionPrefix,
      List(List(resource) -> 0, referenceContinuation.drop(decisionPrefix.size) -> 315), restricted = true)
    val searches = List(survey, samePath)
    val missing = conflict(source, paths, question, searches, survey.work.engineProfile.wireId, 17).get
    assertEquals(missing.comparison, None)
    assertEquals(missing.entry, None)
    assert(read(source, paths, question, searches, survey.work.engineProfile.wireId, 17).isEmpty)
    val renewed = pool(source, paths, "same-path-renewal", 315)
    assertEquals(CommentaryComparisonConflict.read(source, searches :+ renewed, 17), Nil)
    assert(read(source, paths, question, searches :+ renewed, survey.work.engineProfile.wireId, 17).isEmpty)

  test("a renewed full pool omitting the resource retires old numeric participation, not its legal or capture facts"):
    val source = packet()
    val paths = ObservedLineProtocol.prepare(source)
    val survey = pool(source, paths)
    val compared = pair(source, paths)
    val renewed = pool(source, paths, "renewed-full", 315)
    val searches = List(survey, compared, renewed)
    val oldAnswer = compared.pairAnswer(source, resource, leader).get
    assertEquals(CommentaryComparisonConflict.read(source, searches, 17), Nil)
    assertEquals(conflict(source, paths, question, searches, survey.work.engineProfile.wireId, 17), None)
    assert(read(source, paths, question, searches, survey.work.engineProfile.wireId, 17).isEmpty,
      "the omitted resource still needs a fresh exact pair")
    val resourcePath = paths.certifiedPath(source, decisionPrefix ++ List(resource, "b4c3", "b2c3")).get
    assert(paths.positionView(resourcePath, decisionPrefix.size).flatMap(_.legalRows).get.exists(_.move == resource))
    assert(paths.positionView(resourcePath, decisionPrefix.size + 1).flatMap(_.legalRows).get
      .exists(row => row.move == "b4c3" && row.capture.nonEmpty), "the known capture remains an independent duty")
    assertEquals(compared.pairAnswer(source, resource, leader), Some(oldAnswer))
    val fresh = pair(source, paths, "fresh-exact-pair", 320)
    val checked = read(source, paths, question, searches :+ fresh, survey.work.engineProfile.wireId, 17).get
    assertEquals(checked.survey.work.workId, "renewed-full")
    assertEquals(checked.answer.workId, "fresh-exact-pair")
    assertEquals(checked.answer, fresh.pairAnswer(source, resource, leader).get)
    assert(checked.refuted)
    assertEquals(CommentaryComparisonConflict.read(source, searches :+ fresh, 17), Nil)

  test("ordinary nonconflicting achieved-depth alignment can reuse its matching prior pair"):
    val source = packet()
    val paths = ObservedLineProtocol.prepare(source)
    val lower = pool(source, paths, "lower-pool", 541, 16)
    val completedPair = pair(source, paths, "achieved17-pair", 315)
    val aligned = pool(source, paths, "aligned17-pool", 315)
    val searches = List(lower, completedPair, aligned)
    val checked = read(source, paths, question, searches, aligned.work.engineProfile.wireId, 16).get
    assertEquals(checked.answer.workId, "achieved17-pair")
    assertEquals(selectionDepth(source, question, searches, aligned.work.engineProfile.wireId, 16), 17)
    assertEquals(CommentaryComparisonConflict.read(source, searches, 17), Nil)

  test("a lower-depth full sample cannot retire the required higher-level conflict"):
    val source = packet()
    val paths = ObservedLineProtocol.prepare(source)
    val survey = pool(source, paths)
    val compared = pair(source, paths)
    val lower = pool(source, paths, "insufficient-renewal", 315, 16)
    val searches = List(survey, compared, lower)
    assert(read(source, paths, question, searches, survey.work.engineProfile.wireId, 17).isEmpty)
    assert(conflict(source, paths, question, searches, survey.work.engineProfile.wireId, 17).nonEmpty)
    assert(CommentaryComparisonConflict.read(source, searches, 17).nonEmpty)

  test("wrong owner, prepared paths, prefix, profile or achieved depth cannot manufacture this conflict"):
    val source = packet()
    val paths = ObservedLineProtocol.prepare(source)
    val survey = pool(source, paths)
    val compared = pair(source, paths)
    val searches = List(survey, compared)
    val foreign = packet()
    assertEquals(CommentaryComparisonConflict.betweenCurrentValues(foreign, survey, compared), Nil)
    assertEquals(conflict(foreign, ObservedLineProtocol.prepare(foreign), question,
      searches, survey.work.engineProfile.wireId, 17), None)
    assertEquals(conflict(source, ObservedLineProtocol.prepare(source), question,
      searches, survey.work.engineProfile.wireId, 17), None)
    assertEquals(conflict(source, paths, question.copy(prefixMovesUci = List(root, "f7f6")),
      searches, survey.work.engineProfile.wireId, 17), None)
    assertEquals(conflict(source, paths, question, searches, "foreign-profile", 17), None)
    assertEquals(conflict(source, paths, question, searches, survey.work.engineProfile.wireId, 18), None)
    val deeper = pair(source, paths, "higher-only", 315, 24)
    assertEquals(CommentaryComparisonConflict.betweenCurrentValues(source, survey, deeper), Nil)
    assertEquals(read(source, paths, question, List(survey, deeper), survey.work.engineProfile.wireId, 17), None)

  test("a different move's material conflict does not become this exact pair's value conflict"):
    val source = packet()
    val paths = ObservedLineProtocol.prepare(source)
    val survey = pool(source, paths, "current-pool", 315)
    val compared = pair(source, paths, "current-pair", 320)
    val independent = receipt(source, paths, "other-moves-only", decisionPrefix,
      List(List(other) -> 0, List(peer) -> -50), restricted = true)
    val searches = List(survey, compared, independent)
    assert(CommentaryComparisonConflict.read(source, searches, 17).exists(_.move == other))
    assertEquals(conflict(source, paths, question, searches, survey.work.engineProfile.wireId, 17), None)
    val checked = read(source, paths, question, searches, survey.work.engineProfile.wireId, 17).get
    assertEquals(checked.answer.workId, "current-pair")

  test("immutable conflict entries include the exact receipt epoch and depth"):
    val source = packet()
    val paths = ObservedLineProtocol.prepare(source)
    val survey = pool(source, paths)
    val compared = pair(source, paths)
    val first = conflict(source, paths, question, List(survey, compared), survey.work.engineProfile.wireId, 17).get
    val renewed = pool(source, paths, "new-epoch-full", 541)
    val different = pair(source, paths, "new-epoch-pair", 315)
    val second = conflict(source, paths, question, List(survey, compared, renewed, different),
      survey.work.engineProfile.wireId, 17).get
    assertNotEquals(first.entry.get.request.id, second.entry.get.request.id)
    assertEquals(first.entry.get.request.origin.question.sourceRefs, List("pool-541", "pair-315"))
    assertEquals(second.entry.get.request.origin.question.sourceRefs, List("new-epoch-full", "new-epoch-pair"))
    assertNotEquals(first.entry.get.request.id, first.copy(depth = 18).entry.get.request.id)
