package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.*
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.*
import play.api.libs.json.*

/** Synthetic reports test ownership and question contracts; no engine is run. */
class CommentaryInvestigationTest extends munit.FunSuite:
  import CommentaryInvestigation.*

  private val opening = "r1bqkbnr/pppp1ppp/2n5/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 2 3"
  private def packet(ctx: JudgmentAssemblyContext): EvidenceBackedJudgmentPacket =
    EvidenceBackedJudgmentPacket.fromAssembly(ctx).get
  private def fromFen(fen: String, move: String): EvidenceBackedJudgmentPacket =
    packet(MoveWorkMigrationFixtures.fromFen(fen, move))

  private def openingBatch(alternativeDepth: Int = 16): (EvidenceBackedJudgmentPacket, EngineCandidates) =
    val prefix = List("e2e4", "e7e5", "g1f3", "b8c6")
    val initial = chess.variant.Standard.initialFen.value
    val history = CanonicalPositionHistory.from(initial, prefix, opening).toOption.get
    val work = IssuedEngineWork.create(CommentaryEngineProfile.ServerAdmitted, "root-three",
      EngineWorkPurpose.RootSearch, initial, prefix, opening, EngineRootRestriction.Unrestricted,
      opening, Nil, EngineSearchLimits(16, 5_000_000, 5_000, 3), 6_000)
    val reports = List(
      ReportedEngineLineSuffix(List("f1c4", "g8f6"), ReportedWhiteEngineScore.Centipawns(80), 16),
      ReportedEngineLineSuffix(List("f1b5", "a7a6"), ReportedWhiteEngineScore.Centipawns(70), 16),
      ReportedEngineLineSuffix(List("b1c3", "g8f6"), ReportedWhiteEngineScore.Centipawns(60), alternativeDepth))
    val physical = EngineLineAdmission.admitReportedLineSuffixes(work, reports).get
    val admitted = EngineLineAdmission.bindAdmittedLineSuffixes(work, physical).get
    val input = MoveReviewInputAdmission.retainAdmittedLines(
      MoveReviewInputAdmission.admitIndependent(history, "f1b5").get, admitted, EvaluationSource.RootSearch)
    packet(MoveReviewAssembler.assembleOccurrences(input)) -> EngineCandidates(work, admitted)

  test("capture and non-pawn quiet families preserve the complete pool and exact legal sources"):
    val source = fromFen(opening, "f1b5")
    val questions = discover(source, "f1c4")
    assert(Set("f1a6", "f1d3", "f1e2").subsetOf(questions.map(_.alternativeMove).toSet))
    assert(!questions.exists(q => Set("f1b5", "f1c4", "a2a3", "g2g3")(q.alternativeMove)))
    val view = source.resources.find(_.position == source.assembly.playedTransition.get.from).get
    val semanticIds = view.records.collect { case EvidenceRecord(_, fact: RelationFactEvidence, _) => fact.semanticId }.toSet
    questions.foreach { question =>
      assert(question.reasons.forall(reason => reason.evidence == view.occurrence && reason.sourceId.exists(semanticIds)))
      assertEquals((snapshot(question) \ "state").as[String], "deferred")
    }

  test("one foil keeps multiple observed reasons without becoming a same-purpose claim"):
    val source = fromFen("6k1/8/8/8/8/8/2K5/R6R w - - 0 1", "h1e1")
    val questions = discover(source, "a1a2")
    val foil = questions.filter(_.alternativeMove == "a1e1")
    assertEquals(foil.size, 1)
    assertEquals(foil.head.reasons.map(_.kind).toSet, Set(Kind.SameDestination, Kind.SameActorChoice))
    assertEquals(foil.head.reasons.map(_.anchorMove).toSet, Set("h1e1", "a1a2"))

  test("current pawn control does not masquerade as a post-move danger or importance score"):
    val questions = discover(fromFen(opening, "f1b5"), "f1b5")
    val quiet = questions.filter(q => q.reasons.exists(_.kind == Kind.SameActorChoice))
    assert(quiet.exists(_.alternativeMove == "f1a6"))
    assert(quiet.forall(_.orderingSourceIds.isEmpty))

  test("unranked capture hypotheses preserve their victims without assigning a strength bonus"):
    val source = fromFen("8/7k/3p4/2r5/q2Q4/8/8/7K w - - 0 1", "d4e4")
    val captures = discover(source, "d4d3").filter(_.reasons.exists(_.kind == Kind.CaptureOption))
    val victims = captures.flatMap(question => question.reasons.find(_.kind == Kind.CaptureOption)
      .flatMap(_.capture).map(piece => question.alternativeMove -> piece.role.name)).toMap
    assertEquals(victims, Map("d4a4" -> "queen", "d4c5" -> "rook", "d4d6" -> "pawn"))
    assertEquals(captures.size, victims.size)

  test("en passant victims, promotion choices and root check response literals survive discovery"):
    val ep = discover(fromFen("4k3/8/8/3pP3/8/8/8/1N2K3 w - d6 0 2", "b1c3"), "b1a3")
      .find(_.alternativeMove == "e5d6").get
    assertEquals(ep.reasons.head.capture.map(_.square.key), Some("d5"))
    val promotion = discover(fromFen("r3k3/1P6/8/8/8/8/8/4K3 w - - 0 1", "e1d1"), "e1f1")
      .filter(_.alternativeMove.startsWith("b7a8"))
    assertEquals(promotion.flatMap(_.reasons.head.promotionRole).toSet, Set("queen", "rook", "bishop", "knight"))
    val responses = discover(fromFen("4r2k/8/8/8/8/8/8/R3K3 w - - 0 1", "e1d1"), "e1f1")
    assert(responses.nonEmpty)
    assert(responses.flatMap(_.reasons).forall(_.respondsToCheck))

  test("same-work MPV3 supplies a matched pair and connects quiet root observations"):
    val (source, batch) = openingBatch()
    val questions = discover(source, "f1c4", Some(batch))
    val question = questions.find(_.alternativeMove == "b1c3").get
    assert(question.reasons.exists(_.kind == Kind.EngineAlternative))
    val anchorObservations = source.observations.filter(o => question.anchorOccurrenceIds.contains(o.transition.evidence.id))
    assert(anchorObservations.exists(_.meaning.isInstanceOf[MoveObservationMeaning.Development]))
    assert(anchorObservations.exists(_.meaning.isInstanceOf[MoveObservationMeaning.PieceControl]))
    val result = answer(question, batch.work, batch.lines, source).get
    assertEquals(result.evaluationRelation, "worse")
    assertEquals(result.referenceDepth, Some(16))
    assertEquals(result.alternativeDepth, Some(16))
    def controlTargets(occurrence: String): Set[String] = source.observations.collect {
      case observation if observation.transition.evidence.id == occurrence => observation.meaning match
        case MoveObservationMeaning.PieceControl(_, after) => after.target.map(_.square.key)
        case _ => None
    }.flatten.toSet
    assert(controlTargets(result.referenceOccurrenceId)("f7"))
    assert(controlTargets(result.alternativeOccurrenceId)("e4"))
    assertNotEquals(controlTargets(result.referenceOccurrenceId), controlTargets(result.alternativeOccurrenceId))
    val json = snapshot(question, "answered", Some(result))
    assertEquals((json \ "answer" \ "reference_evaluation" \ "source").as[String], "root_search")
    assertEquals((json \ "answer" \ "alternative_evaluation" \ "white_score" \ "value").as[Int], 60)
    assertEquals((json \ "anchor_occurrence_ids").as[List[String]], question.anchorOccurrenceIds)

  test("mismatched levels and a changed search root do not become paired answers"):
    val (source, batch) = openingBatch(alternativeDepth = 17)
    val question = discover(source, "f1c4", Some(batch)).find(_.alternativeMove == "b1c3").get
    assertEquals(answer(question, batch.work, batch.lines, source), None)
    assertEquals(answer(question, batch.work.copy(searchFen = chess.variant.Standard.initialFen.value), batch.lines, source), None)
    val stopped = snapshot(question, "deferred", stopReason = Some("investigation_limit"))
    assertEquals((stopped \ "answer").get, JsNull)
    assertEquals((stopped \ "stop_reason").as[String], "investigation_limit")
