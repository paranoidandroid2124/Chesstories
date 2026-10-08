package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.{MoveReviewAssembler, MoveReviewInputAdmission}
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.CanonicalPositionHistory
import play.api.libs.json.*

/** Legal H05 branches, deliberately synthetic scores/receipts. Fresh engine
  * runs separately check whether these obligations can actually be completed. */
class AvailableEntryTest extends munit.FunSuite:
  import CommentaryFact.*
  import CommentaryProposal.*
  import PrincipalVariationComparison.QualityBand

  private val fen = "8/5p2/8/2p1k3/ppp1P3/P1P2KP1/1P6/8 w - - 0 35"
  private val actual = "g3g4 f7f6 f3e3 b4b3 e3f3 e5d6".split(" ").toList
  private val retained = "f3e3 e5e6 e3f4 e6f6 e4e5 f6e7 f4e4 e7e6 g3g4 b4b3 g4g5 e6d7 e4d5".split(" ").toList
  private val changed = retained.init :+ "e4f4"
  private val counter = retained.take(11) ++ List("f7f5", "e5f6")
  private val strong = retained.take(11) ++ List("f7f6", "e5f6")

  private def fixture() =
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    val lines = List(actual, retained, changed, counter, strong)
    val input = MoveReviewInputAdmission.admitIndependent(history, actual.head,
      suppliedLines = lines.map(SuppliedCommentaryLine(CommentaryLineSource.Study, _))).get
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(input)).get
    val paths = ObservedLineProtocol.prepare(packet)
    val found = lines.map(moves => paths.certifiedPath(packet, moves).get)
    def answer(left: Int, right: Int, offset: Int, relation: String) =
      CommentaryInvestigation.Answer("synthetic-option-" + offset, "synthetic-option-key-" + offset,
        CommentaryEngineProfile.ServerAdmitted.wireId, 16, Some(16), Some(16), relation,
        found(left).line.ref.id, found(right).line.ref.id, Json.obj(), Json.obj(),
        found(left).ids(offset), found(right).ids(offset), Some(QualityBand.Separated))
    val sources = lines.take(2).map(moves => CommentaryDiscovery.SourcePath(moves, Source("user", "original")))
    val generated = CommentaryDiscovery.discover(packet, paths, List(CommentaryDiscovery.Anchor(Nil, retained.head)),
      sources, Nil, pvGuided = true).additions.find(entry => entry.request.origin.question.producer == "C-reserve-entry" &&
        entry.request.requirements.exists {
          case pair: CompareMoves => pair.id == "reserve_counter" && pair.alternativeMove == "f7f5"
          case _ => false
        }).get
    def read(entry: Entry, includeValue: Boolean) = CommentaryFactReader.read(packet, paths,
      List(entry.copy(readings = entry.readings.map {
        case reading if reading.requirementId == "reserve_root" => reading.copy(state = "answered", answer = Some(answer(0, 1, 0, "better")))
        case reading if reading.requirementId == "reserve_counter" => reading.copy(state = "answered", answer = Some(answer(2, 3, 11, "worse")))
        case reading if reading.requirementId == "reserve_entry_value" && includeValue =>
          reading.copy(state = "answered", answer = Some(answer(1, 2, 12, "worse")))
        case reading => reading
      }))).head
    val parent = read(generated, false)
    val next = CommentaryDiscovery.discover(packet, paths, List(CommentaryDiscovery.Anchor(Nil, retained.head)),
      sources :+ CommentaryDiscovery.SourcePath(changed, Source("generator", "response_comparison",
        sourceRefs = List("synthetic-option-11"))), List(parent), pvGuided = true)
    val option = next.additions.find(_.request.requirements.exists(_.id == "reserve_entry_value")).get
    val previous = RootQuestionSummary.completePairAnswer(packet, paths, found.take(2), "selected_comparison", Nil).get
    (packet, paths, found, parent, read(option, true), previous)

  test("a refreshed PV can keep a valuable entry as a separately tested available choice"):
    val (packet, paths, found, parent, option, previous) = fixture()
    val oldProof = CommentaryIdea.reserveEntries(List(parent)).head
    assertEquals(RootSubjectComparison.withReserveEntry(packet, paths, found.take(2), List(oldProof), previous), previous,
      "legality without an independent value receipt is insufficient")
    val proof = CommentaryIdea.reserveEntries(List(option)).head
    assertEquals(proof.optionTest, Some(CompareMoves("reserve_entry_value", retained.init, "e4d5", "e4f4")))
    val selected = RootSubjectComparison.withReserveEntry(packet, paths, found.take(2), List(proof), previous)
    assertEquals(selected.question, "investigated_mechanism")
    val value = selected.core.head.statements.head.asInstanceOf[CommentaryAnswer.ReserveEntry]
    assert(value.available.contains(found(1)))
    assert(!value.references.exists(ref => (ref.path eq found(2)) && ref.offset == 12))
    val text = RootQuestionSummary.realize(paths, found.take(2), selected).get.text
    assert(text.contains("the king can enter with Kd5"), text)
    assert(!text.contains("Kf4 enters") && !text.contains("Kd5 enters"), text)

  test("a legal entry with a materially worse result does not survive as a useful option"):
    val (packet, paths, found, _, option, previous) = fixture()
    val proof = CommentaryIdea.reserveEntries(List(option)).head
    val refuted = proof.copy(optionAnswer = proof.optionAnswer.map(_.copy(evaluationRelation = "better")))
    assertEquals(RootSubjectComparison.withReserveEntry(packet, paths, found.take(2), List(refuted), previous), previous)
    val unknown = proof.copy(optionAnswer = None)
    assertEquals(RootSubjectComparison.withReserveEntry(packet, paths, found.take(2), List(unknown), previous), previous)
    val close = refuted.copy(optionAnswer = refuted.optionAnswer.map(_.copy(qualityBand = Some(QualityBand.Close))))
    assertEquals(RootSubjectComparison.withReserveEntry(packet, paths, found.take(2), List(close), previous).question,
      "investigated_mechanism", "a small rank difference does not imply the option lost its value")

  test("another occurrence, alternative or root history cannot lend the entry a value receipt"):
    val (packet, paths, found, _, option, previous) = fixture()
    val proof = CommentaryIdea.reserveEntries(List(option)).head
    val changed = List(
      proof.copy(optionAnswer = proof.optionAnswer.map(_.copy(referenceOccurrenceId = found(0).ids.head))),
      proof.copy(optionTest = proof.optionTest.map(_.copy(alternativeMove = "e4d4"))),
      proof.copy(optionTest = proof.optionTest.map(_.copy(prefixMovesUci = retained.init.drop(2)))),
      proof.copy(owner = proof.owner.copy(answer = proof.owner.answer.copy(alternativeLineId = found(0).line.ref.id))))
    changed.foreach(value => assertEquals(
      RootSubjectComparison.withReserveEntry(packet, paths, found.take(2), List(value), previous), previous))

  test("a weaker first pawn counter cannot represent a class containing a stronger defense"):
    val (_, _, _, parent, _, _) = fixture()
    val seed = CommentaryIdea.reserveEntries(List(parent)).head
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    val lines = List(actual, retained, counter, strong)
    val input = MoveReviewInputAdmission.admitIndependent(history, actual.head,
      suppliedLines = lines.map(SuppliedCommentaryLine(CommentaryLineSource.Study, _))).get
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(input)).get
    val paths = ObservedLineProtocol.prepare(packet)
    val prefix = retained.take(11)
    val child = history.extend(prefix).toOption.get
    val root = paths.certifiedPath(packet, retained).get
    def search(restriction: EngineRootRestriction) =
      val rows = List(retained.drop(11) -> 700, counter.drop(11) -> 650, strong.drop(11) -> 0)
        .filter((moves, _) => restriction match
          case EngineRootRestriction.Unrestricted => true
          case EngineRootRestriction.Restricted(pool) => pool.contains(moves.head))
      val work = IssuedEngineWork.create(CommentaryEngineProfile.ServerAdmitted, "synthetic-pawn-class",
        EngineWorkPurpose.ResponseComparison, child.initialFen, child.movePrefixUci, child.currentFen,
        restriction, child.currentFen, Nil, EngineSearchLimits(16, 2_000_000, 2_500, rows.size), 3_500,
        Some(EngineSearchAnchor(prefix.head, root.ids.head, root.positions(prefix.size).id.get, prefix)))
      val reported = rows.map((moves, cp) => ReportedEngineLineSuffix(moves, ReportedWhiteEngineScore.Centipawns(cp), 16))
      val physical = EngineLineAdmission.admitReportedLineSuffixes(work, reported, Some(child)).get
      val receipt = CommentaryInvestigation.EngineCandidates(work, EngineLineAdmission.bindAdmittedLineSuffixes(work, physical).get)
      TargetResponseInvestigation.readSearch(packet, receipt, child, paths).get
    val full = search(EngineRootRestriction.Unrestricted)
    val weak = seed.copy(testAnswer = full.pairAnswer(packet, "e6d7", "f7f5"))
    assert(!RootSubjectComparison.strongestReserveCounter(packet, paths, weak, List(full)))
    val best = seed.copy(test = seed.test.copy(alternativeMove = "f7f6"),
      testAnswer = full.pairAnswer(packet, "e6d7", "f7f6"))
    assert(RootSubjectComparison.strongestReserveCounter(packet, paths, best, List(full)))
    val narrow = search(EngineRootRestriction.Restricted(List("e6d7", "f7f5")))
    assert(!RootSubjectComparison.strongestReserveCounter(packet, paths,
      seed.copy(testAnswer = narrow.pairAnswer(packet, "e6d7", "f7f5")), List(narrow)))
    assert(!RootSubjectComparison.strongestReserveCounter(packet, paths,
      best.copy(testAnswer = best.testAnswer.map(_.copy(workId = "foreign"))), List(full)))
