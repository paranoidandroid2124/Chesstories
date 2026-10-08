package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.{MoveReviewAssembler, MoveReviewInputAdmission}
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.CanonicalPositionHistory
import play.api.libs.json.*

/** Legal SG02 continuations already exposed by the stored optionv3 execution.
  * Receipts and ranking relations below are deliberately synthetic: these tests
  * concern question/realization ownership, never adequate chess compensation. */
class MeaningConditionScopeTest extends munit.FunSuite:
  import CommentaryFact.*
  import CommentaryProposal.*
  import PrincipalVariationComparison.QualityBand

  private val fen = "2r3k1/1p3rp1/2p4p/1p1b2q1/1Q1P4/P4PB1/1P4PP/2RR2K1 b - - 0 1"
  private val original =
    "f7f3 b4d2 g5g4 d1f1 f3b3 g3e5 c8e8 f1f2 d5c4 c1c3 b3c3 b2c3 e8a8 d2c1 g4g6".split(" ").toList
  private val changed =
    "f7f3 b4d2 g5g4 d1e1 c8f8 e1f1 f3b3 f1f8 g8f8 c1c3 b3c3 d2c3".split(" ").toList
  private val alternative = List("c8e8")
  private val otherCapture = List("d5f3")

  private def fixture(rootWitness: List[String] = original, includeReply: Boolean = true,
      relation: String = "worse") =
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    val rows = List(original, changed, alternative, otherCapture)
    rows.foreach(moves => assert(history.extend(moves).isRight, s"illegal stored scope witness $moves"))
    val input = MoveReviewInputAdmission.admitIndependent(history, original.head,
      suppliedLines = rows.map(SuppliedCommentaryLine(CommentaryLineSource.Study, _))).get
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(input)).get
    val paths = ObservedLineProtocol.prepare(packet)
    val found = rows.map(moves => moves -> paths.certifiedPath(packet, moves).get).toMap
    val source = Source("user", "stored-realization-scope")
    val generated = CommentaryDiscovery.discover(packet, paths,
      List(CommentaryDiscovery.Anchor(Nil, original.head)),
      rows.map(moves => CommentaryDiscovery.SourcePath(moves, source)), Nil, pvGuided = true).additions
      .filter(_.request.origin.question.producer == "C-sacrifice")
    def seed(moves: List[String]) = generated.find(_.request.requirements.exists {
      case fact: Fact => fact.id == "sacrifice_loss_0" && fact.path.source == Declared(moves)
      case _ => false
    }).getOrElse(fail(s"missing generated material question for $moves"))
    def answer(id: String, reference: List[String], other: List[String], result: String) =
      CommentaryInvestigation.Answer("synthetic-scope-" + id, "synthetic-scope-key-" + id,
        CommentaryEngineProfile.ServerAdmitted.wireId, 16, Some(16), Some(16), result,
        found(reference).line.ref.id, found(other).line.ref.id, Json.obj(), Json.obj(),
        found(reference).ids.head, found(other).ids.head, Some(QualityBand.Separated))
    def read(entry: Entry) = CommentaryFactReader.read(packet, paths,
      List(entry.copy(readings = entry.readings.map {
        case value if value.requirementId == "sacrifice_value" =>
          value.copy(state = "answered", answer = Some(answer(value.requirementId, rootWitness, alternative, relation)))
        case value if value.requirementId == "sacrifice_other_capture" && includeReply =>
          value.copy(state = "answered", answer = Some(answer(value.requirementId, changed, otherCapture, "worse")))
        case value => value
      }))).head
    (packet, paths, read(seed(original)), read(seed(changed)))

  private def reading(entry: Entry, id: String): Reading = entry.readings.find(_.requirementId == id).get
  private def comparison(entry: Entry, id: String): CompareMoves =
    entry.request.requirements.collectFirst { case value: CompareMoves if value.id == id => value }.get
  private def idea(entries: List[Entry], owner: Entry): CommentaryIdea.Idea =
    CommentaryIdea.read(entries).find(_.members.exists(_.entry.request.id == owner.request.id))
      .getOrElse(fail("the original loss question must retain its exact identity"))

  test("another root receipt cannot withdraw the still-current original question"):
    val (packet, paths, entry, _) = fixture()
    assertEquals(reading(entry, "sacrifice_current").state, "supported")
    assertEquals(reading(entry, "returned_sacrifice_other_capture_sacrifice_gain_11").state, "not_applicable")
    assertEquals(reading(entry, "returned_sacrifice_other_capture_sacrifice_gain_11").stopReason, Some("event_absent"))
    assert(!Set("supported", "observed")(reading(entry, "sacrifice_reply_sacrifice_other_capture").state))
    val value = idea(List(entry), entry)
    assert(!value.withdrawn,
      "the other comparison's changed realization does not refute this root comparison's own observations")
    assert(value.live.exists(_.entry.request.id == entry.request.id))
    assert(CommentaryIdea.currentQuestions(List(entry)).exists(_.request.id == entry.request.id))
    val rootReceipt = reading(entry, "sacrifice_value").answer.get
    assertNotEquals(rootReceipt.workId, reading(entry, "sacrifice_other_capture").answer.get.workId)
    val candidates = CommentaryProposal.comparisonCandidates(packet, paths, List(entry))
    assert(candidates.exists(candidate => (candidate \ "selection").as[String] == "selected" &&
      (candidate \ "detail" \ "reference_move").as[String] == "f7f3" &&
      (candidate \ "detail" \ "alternative_move").as[String] == "c8e8"),
      "the still-current root comparison must remain present after a different receipt loses its return")

  test("a completed receipt with missing returns cannot borrow another receipt's compensation"):
    val (_, _, entry, _) = fixture()
    val value = idea(List(entry), entry)
    val member = value.members.find(_.entry.request.id == entry.request.id).get
    val root = comparison(entry, "sacrifice_value")
    val other = comparison(entry, "sacrifice_other_capture")
    assertEquals(root.prefixMovesUci, other.prefixMovesUci)
    assertEquals(root.referenceMove, other.referenceMove)
    assertNotEquals(root.alternativeMove, other.alternativeMove)
    assertEquals(reading(entry, "sacrifice_gain_11").state, "supported",
      "the exact historical gain is still true on its original finite path")
    assert(member.explains(root), "the root's own returned gain is independently supported")
    assert(!member.explains(other),
      "a answered pair score is not a returned compensation witness; supported historical gains cannot repair its absent return")
    assertEquals(value.priority(other), None,
      "the absent completed realization must not give follow-up compensation priority")
    assert(!value.withdrawn, "reject this realization without deleting the original current question")

  test("a stale sibling realization cannot displace a still-current sibling"):
    val (_, _, current, stale) = fixture()
    assertEquals(reading(current, "sacrifice_current").state, "supported")
    assertEquals(reading(stale, "current_sacrifice_gain_7").stopReason, Some("event_absent"))
    val value = idea(List(current, stale), current)
    assertEquals(value.members.map(_.entry.request.id).toSet, Set(current.request.id, stale.request.id),
      "these generated material questions concern the same original f3 pawn loss, not different investors")
    assert(!value.withdrawn)
    assert(value.live.exists(_.entry.request.id == current.request.id))
    assert(!value.live.exists(_.entry.request.id == stale.request.id),
      "a currently absent story does not survive merely because its sibling's root comparison remains valid")

  test("a refreshed root that actually loses the named story still withdraws that realization"):
    val (_, _, entry, _) = fixture(rootWitness = changed)
    assertEquals(reading(entry, "current_sacrifice_gain_11").stopReason, Some("event_absent"))
    val value = idea(List(entry), entry)
    assert(value.withdrawn,
      "separating comparison receipts must not resurrect the old story when its own root receipt has replaced it")
    assertEquals(CommentaryIdea.currentQuestions(List(entry)), Nil)
    assertEquals(value.priority(comparison(entry, "sacrifice_other_capture")), None)

  test("an unanswered comparison remains investigable without establishing its return or compensation"):
    val (_, _, entry, _) = fixture(includeReply = false)
    assertEquals(reading(entry, "sacrifice_current").state, "supported")
    assertEquals(reading(entry, "sacrifice_other_capture").answer, None)
    assert(!Set("supported", "observed")(reading(entry, "sacrifice_reply_sacrifice_other_capture").state))
    val value = idea(List(entry), entry)
    assert(!value.withdrawn)
    assertEquals(value.state, "candidate")
    assert(value.priority(comparison(entry, "sacrifice_other_capture")).nonEmpty,
      "the missing result is a reason to finish the required comparison, not to delete the question")
    assert(value.text.contains("Candidate idea"), value.text)
    assert(value.text.contains("Still needed"), value.text)
    assert(!value.text.contains("Adopted as a conditional idea"), value.text)

  test("a stronger tested alternative revises the ranking while the exact old observations remain facts"):
    val (_, _, entry, _) = fixture(relation = "better")
    assertEquals(reading(entry, "sacrifice_current").state, "supported")
    val value = idea(List(entry), entry)
    assert(!value.withdrawn, "a lower score is different from absence of the observed events")
    assertEquals(value.state, "revised")
    assert(value.text.contains("Revise this idea"), value.text)
    assert(!value.text.contains("Adopted as a conditional idea"), value.text)
    assertEquals(reading(entry, "sacrifice_gain_11").state, "supported")

  test("a supported Ply open-file consequence remains positional compensation to investigate"):
    val rootFen = "6k1/2b5/8/8/8/8/6KP/4R3 w - - 0 1"
    val lossLine = List("e1e2", "c7h2", "e2e3")
    val avoid = List("e1f1")
    val decline = List("e1e2", "c7d8")
    val rows = List(lossLine, avoid, decline)
    val history = CanonicalPositionHistory.from(rootFen, Nil, rootFen).toOption.get
    rows.foreach(line => assert(history.extend(line).isRight, s"illegal file-opening witness $line"))
    val input = MoveReviewInputAdmission.admitIndependent(history, lossLine.head,
      suppliedLines = rows.map(SuppliedCommentaryLine(CommentaryLineSource.Study, _))).get
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(input)).get
    val paths = ObservedLineProtocol.prepare(packet)
    val reference = paths.certifiedPath(packet, lossLine).get
    val alternative = paths.certifiedPath(packet, avoid).get
    val generated = CommentaryDiscovery.discover(packet, paths,
      List(CommentaryDiscovery.Anchor(Nil, lossLine.head)),
      rows.map(line => CommentaryDiscovery.SourcePath(line, Source("user", "positional-return-scope"))),
      Nil, pvGuided = true).additions.find(entry =>
      entry.request.origin.question.producer == "C-sacrifice" && entry.request.requirements.exists {
        case fact: Fact => fact.id == "sacrifice_loss_1" && fact.path.source == Declared(lossLine)
        case _ => false
      }).get
    val receipt = CommentaryInvestigation.Answer("synthetic-file-root", "synthetic-file-root-key",
      CommentaryEngineProfile.ServerAdmitted.wireId, 16, Some(16), Some(16), "worse",
      reference.line.ref.id, alternative.line.ref.id, Json.obj(), Json.obj(),
      reference.ids.head, alternative.ids.head, Some(QualityBand.Separated))
    val entry = CommentaryFactReader.read(packet, paths, List(generated.copy(readings = generated.readings.map {
      case value if value.requirementId == "sacrifice_value" =>
        value.copy(state = "answered", answer = Some(receipt))
      case value => value
    }))).head
    assertEquals(reading(entry, "sacrifice_current").state, "supported")
    assertEquals(reading(entry, "current_sacrifice_file_h").state, "supported")
    val file = entry.request.requirements.collectFirst {
      case fact: Fact if fact.id == "current_sacrifice_file_h" => fact
    }.get
    assertEquals(file.at, Ply(3))
    assertEquals(file.predicate, Predicate("file_occupancy", property = Some("h")))
    assert(!entry.request.requirements.exists {
      case fact: Fact => fact.id.startsWith("current_sacrifice_gain_") ||
        fact.id.startsWith("current_sacrifice_check_") || fact.id.startsWith("current_sacrifice_promotion_")
      case _ => false
    }, "this line has only a pawn loss and a quiet move; no tactical return can supply this assertion")
    val value = idea(List(entry), entry)
    val member = value.live.find(_.entry.request.id == entry.request.id).get
    assert(member.current.contains(file))
    assert(member.compensationFacts.contains(file),
      "receipt-local compensation includes supported positional Ply facts, not only Event witnesses")
    assert(member.hasReturn)
    assert(value.observedReturn)
    assert(member.explains(comparison(entry, "sacrifice_value")))
    assertEquals(value.state, "candidate",
      "an open file is observed compensation to investigate, never an adequate-compensation verdict")
