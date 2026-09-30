package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.CanonicalPositionHistory
import play.api.libs.json.*

class PieceControlDeliveryTest extends munit.FunSuite:
  private def context(fen: String, move: String): JudgmentAssemblyContext =
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    MoveReviewAssembler.assemble(MoveReviewInputAdmission.admitIndependent(history, move).get)

  private def relations(ctx: JudgmentAssemblyContext): List[(PieceControlPoint, PieceControlPoint)] =
    OccurrenceCommentary.fromAssembly(ctx).collect {
      case MoveObservation(_, _, MoveObservationMeaning.PieceControl(before, after), _) => before -> after
    }

  test("book examples and adversarial positions retain both physical endpoints, gains, losses and maintenance"):
    val stream = getClass.getResourceAsStream("/piece-control-cases.json")
    val cases = try Json.parse(stream).as[List[JsObject]] finally stream.close()
    for row <- cases do
      val ctx = context((row \ "beforeFEN").as[String], (row \ "uci").as[String])
      val observations = relations(ctx)
      for fact <- (row \ "facts").as[List[JsObject]] do
        val from = (fact \ "subject" \ "before").as[String]
        val to = (fact \ "target" \ "before").as[String]
        val relation = observations.find { case (before, _) =>
          before.controller.exists(_.square.key == from) && before.target.exists(_.square.key == to)
        }.getOrElse(fail(s"${(row \ "id").as[String]} missing $from -> $to"))
        for (point, phase) <- List(relation._1 -> "before", relation._2 -> "after") do
          val expected = if (fact \ phase).as[String] == "none" then PieceControlState.Absent else PieceControlState.Present
          assertEquals(point.state, expected, s"${(row \ "id").as[String]} $from->$to $phase")
          assertEquals(point.controller.map(_.square.key), (fact \ "subject" \ phase).asOpt[String])
          assertEquals(point.target.map(_.square.key), (fact \ "target" \ phase).asOpt[String])
      assert(OccurrenceCommentary.fromAssembly(ctx).forall(_.premises.forall(ref =>
        ctx.evidenceGraph.record(ref).exists(ctx.evidenceGraph.proofEligible))))
      assertEquals(EvidenceBackedJudgmentPacket.fromAssembly(ctx).get.primary, None)

  test("missing positive evidence is unknown, not a refutation; the other phase survives"):
    val ctx = context("7k/r7/8/8/8/8/8/R6K b - - 0 1", "a7a6")
    val relation = relations(ctx).find(_._1.controller.exists(_.square.key == "a1")).get
    assertEquals(relation._1.state, PieceControlState.Present)
    assertEquals(relation._2.state, PieceControlState.Present)
    val control = relation._1.premises.find(ref => ctx.evidenceGraph.record(ref).exists(_.payload match
      case fact: RelationFactEvidence => fact.kind == RelationFactKind.GeometricControl
      case _ => false)).get
    val damaged = ctx.copy(evidenceGraph = TypedEvidenceGraph.empty.addAll(
      ctx.evidenceGraph.records.filterNot(_.ref == control)))
    val surviving = relations(damaged).find(_._1.controller.exists(_.square.key == "a1")).get
    assertEquals(surviving._1.state, PieceControlState.Unknown)
    assertEquals(surviving._2.state, PieceControlState.Present)

  test("piece identity handles castling, promotion, capture and en passant without a second board parser"):
    val cases = List(
      ("4k3/8/8/8/8/8/8/4K2R w K - 0 1", "e1g1", "h1", "rook", "white", Some("f1" -> "rook")),
      ("4k3/P7/8/8/8/8/8/4K3 w - - 0 1", "a7a8n", "a7", "pawn", "white", Some("a8" -> "knight")),
      ("7k/8/8/8/8/8/r7/R6K w - - 0 1", "a1a2", "a2", "rook", "black", None),
      ("7k/8/8/3pP3/8/8/8/7K w - d6 0 1", "e5d6", "d5", "pawn", "black", None))
    for (fen, move, square, role, side, expected) <- cases do
      val ctx = context(fen, move)
      val effect = ctx.transitionReplays(ctx.playedTransition.get.evidence.id).singleStepReplays.head._1.boardFootprint
      val before = RelationColoredPieceWitness(EvidenceSquare(square), EvidencePieceRole(role),
        if side == "white" then chess.White else chess.Black)
      val after = before.after(effect)
      assertEquals(after.map(piece => piece.square.key -> piece.role.name), expected)
      after.foreach(piece => assertEquals(piece.before(effect), Some(before)))

  test("evaluation and full-line failure cannot erase independently certified piece work"):
    val ctx = context("4r2k/8/8/8/3b4/8/2P1N3/4K3 w - - 0 1", "c2c3")
    val excluded = Set(EvidenceLayer.Line, EvidenceLayer.Eval, EvidenceLayer.RelativeAssessment, EvidenceLayer.CandidateComparison)
    val lower = ctx.copy(evidenceGraph = TypedEvidenceGraph.empty.addAll(ctx.evidenceGraph.records.filterNot(r => excluded(r.ref.layer))))
    assertEquals(relations(lower), relations(ctx))
