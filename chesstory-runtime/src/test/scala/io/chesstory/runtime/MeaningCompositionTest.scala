package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.{MoveReviewAssembler, MoveReviewInputAdmission, RawMoveReviewInput}
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.{CanonicalPositionHistory, EngineLine}
import play.api.libs.json.*

/** Fixed, equally scored continuations inspect bounded explanation composition.
  * These are neither engine answers nor tests of move strength or release quality.
  */
class MeaningCompositionTest extends munit.FunSuite:
  private def moves(line: String): List[String] = line.split(" ").toList

  private def project(fen: String, first: String, second: String, output: Option[String] = None):
      (EvidenceBackedJudgmentPacket, ObservedLineProtocol.PreparedPaths, JsObject) =
    val rows = List(first, second).map(moves)
    val owner = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    rows.foreach(line => assert(owner.extend(line).isRight, line.mkString(" ")))
    val input = MoveReviewInputAdmission.admit(RawMoveReviewInput(fen = fen,
      playedMoveUci = rows.head.head, variations = rows.map(EngineLine(_, 0, None, 8)))).get
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assemble(input)).get
    val prepared = ObservedLineProtocol.prepare(packet)
    rows.foreach { line =>
      val path = prepared.paths.find(_.moves == line).get
      assertEquals(path.ids, path.declared)
      assertEquals(path.ids.size, line.size)
      assert(path.certified.nonEmpty)
    }
    val observations = RuntimeProtocol.observationsJson(packet)
    for directory <- sys.props.get("chesstory.reference-output"); name <- output do
      val out = java.nio.file.Path.of(directory)
      java.nio.file.Files.createDirectories(out)
      java.nio.file.Files.writeString(out.resolve(name + ".json"), Json.stringify(observations))
    val root = (observations \ "selection" \ "candidates").as[List[JsObject]].find(candidate =>
      (candidate \ "selection").as[String] == "selected" &&
        (candidate \ "detail" \ "type").asOpt[String].contains("root_comparison"))
      .getOrElse(fail("the directly admitted pair needs a root comparison"))
    (packet, prepared, root)

  private def text(root: JsObject): String = (root \ "detail" \ "text").as[String]
  private def histories(root: JsObject): List[JsObject] =
    (root \ "detail" \ "effects").asOpt[List[JsObject]].getOrElse(Nil)
      .filter(effect => (effect \ "scope").as[String] == "line_history")
  private def target(effect: JsObject, role: String, square: String): Boolean =
    (effect \ "target" \ "role").asOpt[String].contains(role) &&
      (effect \ "target" \ "square").asOpt[String].contains(square)

  test("bounded comparison text publishes retained item identities, not matching wording"):
    val shared = "The reply preserves its recapture condition."
    val examples = List(
      ("oversized predecessor containing the shorter item", List(shared + " explanation" * 500, shared), List(1)),
      ("rejected wording already inside an admitted item", List(shared.padTo(3880, 'x'), shared, "reply"), List(0, 2)),
      ("separately admitted items with equal wording", List(shared, shared), List(0, 1)),
      ("empty items do not acquire ownership", List("", shared, ""), List(1)))
    examples.foreach { (label, parts, expected) =>
      val result = CommentaryProposal.boundedText(parts)
      val identities = parts.indices.map(index => s"counter-$index").toList
      assertEquals(result.retained.map(identities.apply), expected.map(identities.apply), label)
      result.retained.foreach(index => assert(result.text.contains(parts(index)), label))
      assert(result.text.length <= 4000, label)
    }
    val decision = "At this position, the tested alternative is stronger."
    val observations = (1 to 30).map(n => s"Observation $n: " + "named conditional capture " * 12 + ".").toList
    val parts = decision :: observations
    val result = CommentaryProposal.boundedText(parts)
    assert(result.retained.contains(0) && result.retained.size < parts.size)
    assert(result.text.contains(decision) && result.text.length <= 4000)
    observations.foreach(observation =>
      assert(!result.text.contains(observation.take(16)) || result.text.contains(observation),
        "a claim and its qualification are admitted together"))

  test("a rejected support history leaves the complete immediate support and its attack evidence available"):
    val examples = List(
      ("7k/8/5n2/8/4P3/2N5/8/5RK1 w - - 0 1", "f1e1 h8h7 c3b5 f6e4 e1e4", "e1", "f6", false),
      ("7k/8/8/8/4P3/3R4/2b5/6K1 w - - 0 1", "d3e3 c2e4 e3e4", "e3", "c2", true))
    examples.foreach { (fen, line, square, oppositionSquare, opensAttack) =>
      val (packet, prepared, root) = project(fen, line, "g1h1")
      val projection = MoveWorkProjection.select(packet, prepared)
      val path = prepared.paths.find(_.moves == moves(line)).get
      val peer = prepared.paths.find(_.moves == List("g1h1")).get
      val rootMove = prepared.notation.line(path.moves).head
      val support = projection.local.find(_.note.detail match
        case MoveWorkProjection.Detail.Support(actor, victim, opposition, true) =>
          actor.square.key == square && victim.square.key == "e4" && opposition.exists(_.square.key == oppositionSquare)
        case _ => false).getOrElse(fail("missing the current attacked-pawn support"))
      val costEffect = Option.when(opensAttack) {
        (root \ "detail" \ "effects").as[List[JsObject]].find(value =>
          (value \ "scope").as[String] == "pair_difference" &&
            (value \ "actor" \ "square").as[String] == oppositionSquare && target(value, "pawn", "e4")).get
      }
      val cost = costEffect.map { effect =>
        assertEquals((effect \ "line_ids").as[List[String]].toSet, Set(path.line.ref.id, peer.line.ref.id))
        assertEquals((effect \ "occurrence_ids").as[List[String]].toSet, Set(path.ids.head, peer.ids.head))
        assertEquals((effect \ "actor" \ "side").as[String], "black")
        assertEquals((effect \ "actor" \ "role").as[String], "bishop")
        assertEquals((effect \ "target" \ "side").as[String], "white")
        val actor = CommentaryFact.Piece("black", "bishop", oppositionSquare).witness
        val victim = CommentaryFact.Piece("white", "pawn", "e4").witness
        val contrast = CommentaryAnswer.ControlContrast(RootQuestionSummary.MoveRef(path, 0), actor, Some(victim),
          "e4", true, RootQuestionSummary.MoveRef(peer, 0), actor, Some(victim), "e4", false)
        CommentaryEnglish.section(CommentaryAnswer.Section(List(contrast))).map {
          case value: String => value
          case ref: RootQuestionSummary.MoveRef => prepared.notation.line(ref.path.moves)(ref.offset)
        }.mkString
      }.getOrElse("")
      if !opensAttack then
        assert((root \ "detail" \ "effects").as[List[JsObject]].exists(effect =>
          (effect \ "scope").as[String] == "support_handoff"), text(root))
      else assert(text(root).contains("Bxe4") && text(root).contains("Rxe4"), text(root))
      val shortSupport = support.note.detail.after(rootMove)
      val available = List(shortSupport, cost).filter(_.nonEmpty).mkString("\n\n").length + 4
      val constrained = root + ("detail" -> ((root \ "detail").as[JsObject] - "effects" ++
        Json.obj("type" -> "question_comparison", "prefix_moves_uci" -> List.empty[String],
          "text" -> ("x" * (4000 - available)))))
      val result = projection.withMoveEffects(constrained)
      assert(text(result).contains(shortSupport), text(result).takeRight(600))
      if !opensAttack then
        assert(!(result \ "detail" \ "effects").as[List[JsObject]].exists(effect =>
          (effect \ "scope").as[String] == "support_handoff"), "the longer handoff must actually be rejected")
      else
        assert((result \ "detail" \ "effects").as[List[JsObject]].contains(costEffect.get),
          "the fallback retains the same attack target, both root paths and all of its proof references")
        List(rootMove, prepared.notation.line(peer.moves).head, "bishop on c2", "pawn on e4", "attacks", "does not attack")
          .foreach(clause => assert(text(result).contains(clause), text(result).takeRight(600)))
        assert(!text(result).contains("Bxe4") && !text(result).contains("Rxe4"), "the long trade really did not fit")
      assert(text(result).length <= 4000)
      val evidence = (result \ "detail" \ "effects").as[List[JsObject]]
        .flatMap(effect => (effect \ "evidence_ids").as[List[String]]).toSet
      assert(support.note.afterSources.toSet.subsetOf(evidence), "the current attack evidence remains with the support")
    }

  test("a closed attack names its actual ray blocker, not an off-ray remaining attacker"):
    // A synthetic geometry invariant, independent of the assessed game inputs.
    val (packet, prepared, root) = project("7k/8/5N2/3p4/4b3/8/8/R5K1 w - - 0 1",
      "a1a4 d5d4", "g1f1")
    val path = prepared.paths.find(_.moves == List("a1a4", "d5d4")).get
    val actor = CommentaryFact.Piece("white", "rook", "a4").witness
    val target = CommentaryFact.Piece("black", "bishop", "e4").witness
    val blocker = CommentaryFact.Piece("black", "pawn", "d4").witness
    val remaining = CommentaryFact.Piece("white", "knight", "f6").witness
    val after = new MoveWorkProjection.Position(packet.resources.find(_.position == path.positions(2)).get)
    val (source, blockers) = after.blockedRay(actor, target).get
    assertEquals(blockers, List(blocker))
    assertEquals(after.controllers(target.square.key, actor.side), List(remaining))
    val closed = MoveWorkProjection.select(packet, prepared).local.find(note =>
      note.edge.evidence.id == path.ids(1) && (note.note.detail match
        case MoveWorkProjection.Detail.ChangedLines(closed, _, _) =>
          closed.contains((actor, target, List(remaining)))
        case _ => false)).getOrElse(fail("the source note must distinguish the remaining attacker from the blocker"))
    assertEquals(closed.note.actor, blocker)
    val summary = (root \ "detail" \ "summary").as[String]
    assert(summary.contains("pawn on d4 blocks the line"), summary)
    assert(summary.contains("knight on f6 attacks"), summary)
    assert(!summary.contains("knight on f6 blocks"), summary)
    val units = (root \ "detail" \ "summary_question" \ "units").as[List[JsObject]]
    List("en", "ko").foreach { language =>
      val refs = units.flatMap(unit => (unit \ language).as[List[JsValue]].collect { case ref: JsObject => ref }).toSet
      assert(refs(RootQuestionSummary.MoveRef(path, 1).json), "the obstruction belongs to this exact after-move occurrence")
    }
    val sourceRef = after.sourceByRef.collectFirst { case (ref, `source`) => ref }.get
    assert((root \ "evidence_ids").as[List[String]].contains(sourceRef), "the named blocker retains the original ray proof")

    val ray = after.rays.find(_._1 == source).get._2
    val invalid: List[(String, List[(String, RelationWitnessDetail.RayBarrier)])] = List(
      "missing ray" -> Nil,
      "another actor" -> List(source -> ray.copy(attackerSquare = EvidenceSquare("a1"))),
      "off-ray remaining attacker" -> List(source -> ray.copy(occupants = List(remaining, target))),
      "wrong current piece" -> List(source -> ray.copy(occupants = List(blocker.copy(role = EvidencePieceRole("rook")), target))),
      "wrong ray geometry" -> List(source -> ray.copy(geometry = ray.geometry.copy(squares = List(remaining.square, target.square)))))
    invalid.foreach { (reason, values) =>
      val unproved = new MoveWorkProjection.Position(after.view):
        override lazy val rays: List[(String, RelationWitnessDetail.RayBarrier)] = values
      assertEquals(unproved.blockedRay(actor, target), None, reason)
      assert(unproved.complete(RelationFactKind.GeometricControl) && !unproved.controlledBy(actor)(target.square.key),
        "a missing obstruction proof does not erase the separately proven loss of attack")
      assertEquals(unproved.controllers(target.square.key, actor.side), List(remaining),
        "remaining control stays a separate fact even without a blocker witness")
    }
    assertEquals(after.blockedRay(actor, target.copy(role = EvidencePieceRole("pawn"))), None,
      "the target square alone cannot stand in for the actual target identity")

  test("an admitted support use retains every current attacker while absorbing its immediate support"):
    val (packet, prepared, root) = project("7k/1b6/5n2/8/4P3/8/8/5RK1 w - - 0 1",
      "f1e1 f6e4 e1e4", "g1h1")
    val path = prepared.paths.find(_.moves.head == "f1e1").get
    val support = MoveWorkProjection.select(packet, prepared).local.find(note =>
      note.edge.evidence.id == path.ids.head && (note.note.detail match
        case MoveWorkProjection.Detail.Support(actor, target, opposition, true) =>
          actor.square.key == "e1" && target.square.key == "e4" && opposition.map(piece =>
            (piece.side.white, piece.role.name, piece.square.key)).toSet == Set((false, "bishop", "b7"), (false, "knight", "f6"))
        case _ => false)).getOrElse(fail("the supported pawn must retain both current attackers"))
    assert(support.evidence.toSet.subsetOf((root \ "evidence_ids").as[List[String]].toSet))
    val uses = (root \ "detail" \ "effects").as[List[JsObject]].filter(effect =>
      (effect \ "scope").as[String] == "support_use" && target(effect, "pawn", "e4"))
    assertEquals(uses.size, 1)
    assertEquals((uses.head \ "line_ids").as[List[String]], List(path.line.ref.id))
    assertEquals((uses.head \ "occurrence_ids").as[List[String]], path.ids)
    val body = text(root)
    val summary = (root \ "detail" \ "summary").as[String]
    List(body, summary).foreach(reading =>
      List("bishop on b7", "knight on f6", "pawn on e4", "Nxe4", "Rxe4")
        .foreach(value => assert(reading.contains(value), reading)))
    val units = (root \ "detail" \ "summary_question" \ "units").as[List[JsObject]]
    for language <- List("en", "ko") do
      val refs = units.flatMap(unit => unit(language).as[List[JsValue]].collect { case ref: JsObject => ref }).toSet
      path.moves.indices.foreach(offset => assert(refs(RootQuestionSummary.MoveRef(path, offset).json),
        "short delivery preserves the support, target loss and actual recapture as one owned answer"))
    assert(!histories(root).exists(effect => target(effect, "pawn", "e4") &&
      (effect \ "actor" \ "square").as[String] == "e1" &&
      (effect \ "through_plies").as[List[Int]] == List(1)), "the support use owns the immediate support too")

  test("the actual opening exchange shares captures with attack history but keeps the later recovery role"):
    val (packet, prepared, root) = project("3rr1k1/pppq1pb1/5nnp/3Pp1p1/P7/2PPNNBP/1P3PP1/R2QR1K1 w - - 2 18",
      "d3d4 f6d5 d4e5 d5e3 d1d7 d8d7 e1e3", "c3c4 c7c6 d5c6 b7c6")
    val body = text(root)
    assert(body.contains("White pawn captures the black pawn on e5 with dxe5"), body)
    assert(!body.contains("captures the initially attacked Black pawn on e5"), body)
    assert(body.contains("was the target") && body.contains("This removes that earlier capturer"), body)
    assert(body.contains("White knight on e3") && body.contains("Qxd7") && body.contains("Rxd7"), body)
    val independent = root + ("detail" -> ((root \ "detail").as[JsObject] - "effects" ++
      Json.obj("type" -> "question_comparison", "prefix_moves_uci" -> List.empty[String], "text" -> "A separate comparison.")))
    val standalone = MoveWorkProjection.select(packet, prepared).withMoveEffects(independent)
    assert(text(standalone).contains("dxe5") && text(standalone).contains("captures the initially attacked Black pawn on e5"), text(standalone))
    assert(text(standalone).contains("Rxe3") && text(standalone).contains("later captures Black knight on e3"), text(standalone))
    val path = prepared.paths.find(_.moves.head == "d3d4").get
    val reading = prepared.continuationReadings.find(_.path eq path).get
    val loss = reading.events.find(_.plyOffset == 3).get
    val recovery = reading.events.find(_.plyOffset == 6).get
    assertEquals(loss.originalVictim.map(piece => (piece.role.name, piece.square.key)), Some("knight" -> "e3"))
    assertEquals(recovery.originalVictim, loss.originalActor, "the later rook takes that same earlier capturer")
    assert(histories(standalone).exists(effect => target(effect, "knight", "e3") &&
      (effect \ "actor" \ "role").as[String] == "knight" &&
      (effect \ "actor" \ "square").as[String] == "f6" &&
      (effect \ "line_ids").as[List[String]] == List(path.line.ref.id) &&
      (effect \ "prefix_moves_uci").as[List[String]].isEmpty &&
      (effect \ "attack_ply").asOpt[Int].contains(2) &&
      List(0, 1, 3, 6).forall(offset => (effect \ "occurrence_ids").as[List[String]].contains(path.ids(offset)))),
      "the recovery belongs to Nd5's attack on the white e3-knight: " + text(standalone))

  test("a selected delayed recovery retains its actual line-clearing knight and that knight's loss account"):
    // The already opened Rfe8/h6 pair checks delivery, not move strength.
    val fen = "r4rk1/qp1n1ppp/2pp1n2/2b2b2/2P5/1PN3P1/2NBPPBP/2RQ1RK1 b - - 0 15"
    val line = "f8e8 b3b4 f5c2 c1c2 c5b4 c3b5 c6b5 d2b4 b5c4 c2c4 d6d5 c4c1 a7a6 e2e3 a6e6 b4c3 a8c8 d1b3 b7b6 c3d4"
    val peer = "h7h6 c3a4 f8e8 d2c3 f6e4 g2e4 f5e4 c1a1 b7b5 a4c5 a7c5 a1a8 e8a8 c2e3 b5b4 c3d4 c5a5 d4b2 a5c5 d1d2 f7f6 f1d1 a8a2 g1f1 e4g6 f2f3"
    val (packet, prepared, root) = project(fen, line, peer)
    val path = prepared.paths.find(_.moves == moves(line)).get
    val other = prepared.paths.find(_.moves == moves(peer)).get
    val positions = packet.resources.map(view => view.position -> new MoveWorkProjection.Position(view)).toMap
    val arms = RootQuestionSummary.prepare(packet, prepared, List(path, other), positions).get
    val arm = arms.head
    val projection = MoveWorkProjection.select(packet, prepared)
    val notes = projection.local
    def ref(offset: Int) = RootQuestionSummary.MoveRef(path, offset)
    val selected = Set(ref(4), ref(7))
    val sections = MoveWorkProjection.recoveryClearances(arms, selected, notes)
    assertEquals(sections.size, 1)
    val opening = sections.head.statements.collectFirst { case value: CommentaryAnswer.Action => value }.get
    val actor = CommentaryFact.Piece("white", "bishop", "d2").witness
    val capturer = CommentaryFact.Piece("black", "bishop", "c5").witness
    val currentCapturer = CommentaryFact.Piece("black", "bishop", "b4").witness
    val blocker = CommentaryFact.Piece("white", "knight", "c3").witness
    assertEquals(opening.move, ref(5))
    assertEquals(opening.detail, MoveWorkProjection.Detail.ChangedLines(Nil, List(actor -> currentCapturer), Nil))
    val witness = notes.find(note => note.edge.evidence.id == path.ids(5) &&
      note.note.detail == MoveWorkProjection.Detail.Attack(actor, currentCapturer, true)).get
    assertEquals(sections.head.focus, List(CommentaryAnswer.Focus(ref(5), witness)))
    val gain = arm.captures.find(_.plyOffset == 4).get
    val recovery = arm.captures.find(_.plyOffset == 7).get
    assertEquals(gain.originalActor, Some(capturer))
    assertEquals(recovery.originalVictim, Some(capturer))
    assertEquals(recovery.originalActor, Some(actor))
    val account = sections.head.statements.collectFirst { case value: CommentaryAnswer.PieceExchange => value }.get
    assertEquals(account.events.map(_.plyOffset), List(6, 8, 9),
      "the clearing knight's capturer also takes the c4 pawn before that same pawn is recovered")
    assertEquals(account.events.head.originalVictim, Some(blocker))
    assertEquals(account.events.head.originalActor, Some(CommentaryFact.Piece("black", "pawn", "c6").witness))
    assertEquals(account.events.last.originalVictim, account.events.head.originalActor)
    assertEquals(account.recaptureOffsets, Set(9))
    assertEquals(sections.head.required.toSet, Set(5, 6, 8, 9).map(ref))
    assert(arm.proofs.nonEmpty)

    val units = (root \ "detail" \ "summary_question" \ "units").as[List[JsObject]]
    val selectedHighlight = sections.head.focus.head.note.highlight
    assert(!projection.highlights.contains(selectedHighlight), "the ordinary local cap omits this answer's frame")
    val delivered = projection.baseJson + ("candidates" -> Json.toJson(List(root)))
    assert(projection.highlightsFor(delivered).contains(selectedHighlight), "the selected answer publishes its original frame")
    val badUnits = units.map(unit => unit + ("focus" -> Json.toJson(
      (unit \ "focus").asOpt[List[JsObject]].getOrElse(Nil).map(focus =>
        if (focus \ "occurrence_id").as[String] == path.ids(5) then
          focus + ("source_ids" -> Json.arr("unproved-frame")) else focus))))
    val badRoot = root + ("detail" -> ((root \ "detail").as[JsObject] + ("summary_question" ->
      ((root \ "detail" \ "summary_question").as[JsObject] + ("units" -> Json.toJson(badUnits))))))
    assert(!projection.highlightsFor(projection.baseJson + ("candidates" -> Json.toJson(List(badRoot))))
      .contains(selectedHighlight), "an unmatched focus cannot synthesize a new highlight")
    val shown = units.flatMap(unit => (unit \ "en").as[List[JsValue]].collect { case value: JsObject => value }).toSet
    val covered = units.flatMap(unit => (unit \ "covered_moves").as[List[JsObject]]).toSet
    List(4, 5, 6, 7, 8, 9).foreach(offset => assert(shown(ref(offset).json), s"missing owned occurrence $offset"))
    List(4, 6, 7, 8, 9).foreach(offset => assert(covered(ref(offset).json), s"missing actual material event $offset"))
    val summary = (root \ "detail" \ "summary").as[String]
    List("Nb5", "cxb5", "knight", "c3").foreach(value => assert(summary.contains(value), summary))

    assertEquals(MoveWorkProjection.recoveryClearances(arms,
      Set(RootQuestionSummary.MoveRef(other, 4), ref(7)), notes), Nil,
      "an equally numbered capture in the other arm cannot own this recovery")
    val wrongActor = CommentaryFact.Piece("white", "bishop", "g2").witness
    val otherPieceNotes = notes.map { note =>
      if note.edge.evidence.id == path.ids(5) then
        val detail = note.note.detail match
          case MoveWorkProjection.Detail.Attack(`actor`, target, gained) =>
            MoveWorkProjection.Detail.Attack(wrongActor, target, gained)
          case MoveWorkProjection.Detail.ChangedLines(closed, opened, blocked) =>
            MoveWorkProjection.Detail.ChangedLines(closed,
              opened.map((original, target) => (if original == actor then wrongActor else original) -> target), blocked)
          case value => value
        note.copy(note = note.note.copy(detail = detail))
      else note
    }
    assertEquals(MoveWorkProjection.recoveryClearances(arms, selected, otherPieceNotes), Nil,
      "a different bishop's opened line cannot justify this same physical recoverer's cost")

  test("retained support connects the departing defender to the same target within the observed interval"):
    val cases = List(
      ("6k1/7p/5p2/6p1/4N3/4B3/8/K7 b - - 0 1", "h7h6 a1a2 f6f5", "f6f5", "f5", "g5"),
      ("rnbq1rk1/3nbppp/p3p3/1p2P3/3N4/1BN5/PPP3PP/R1BQ1RK1 w - - 1 12",
        "c1e3 d7e5 d1h5 b8c6 d4c6 e5c6 f1f3", "f1e1", "Qh5", "d4"))
    cases.foreach { (fen, line, peer, departure, square) =>
      val (packet, prepared, root) = project(fen, line, peer, Some(if square == "d4" then "retention-be3" else "retention-h6-attackers"))
      val body = text(root)
      val effect = (root \ "detail" \ "effects").as[List[JsObject]].find(effect =>
        (effect \ "scope").as[String] == "support_retention" && (effect \ "target" \ "square").asOpt[String].contains(square))
        .getOrElse(fail(body))
      assert(body.contains(s"$departure ends the support") && body.contains("keeps covering it through that move"), body)
      assertEquals((root \ "detail" \ "effects").as[List[JsObject]].count(_ == effect), 1,
        "the current support and its retained interval have one owner")
      assertEquals((effect \ "through_plies").as[List[Int]], List(3), "later target movement must not lengthen the earlier support claim")
      val path = prepared.paths.find(_.moves == moves(line)).get
      assertEquals((effect \ "occurrence_ids").as[List[String]], path.ids.take(3))
      val current = MoveWorkProjection.select(packet, prepared).local.find(note =>
        note.edge.evidence.id == path.ids.head && (note.note.detail match
          case MoveWorkProjection.Detail.Support(actor, target, _, true) =>
            actor.square.key == (effect \ "actor" \ "square").as[String] && target.square.key == square
          case _ => false)).getOrElse(fail("the retained interval must own its current support premise"))
      assert(current.note.afterSources.toSet.subsetOf((effect \ "evidence_ids").as[List[String]].toSet),
        "retention preserves the positive support and current attackers' evidence")
      val words = current.note.detail.text
      assertEquals(body.sliding(words.length).count(_ == words), 1,
        "the current support is displayed once even when immediate and interval proofs are both retained")
      if square == "g5" then assert(body.contains("knight on e4") && body.contains("bishop on e3"), body)
    }

  test("retention is not invented from a short line, interrupted support or a target moving away"):
    val fen = "7k/8/5n2/8/4P3/2N5/8/5RK1 w - - 0 1"
    List("f1e1 h8h7", "f1e1 h8h7 e1d1 h7h8 c3b5", "f1e1 h8h7 e4e5 h7h8 c3b5").foreach { line =>
      val (_, _, root) = project(fen, line, "g1h1")
      assert(!(root \ "detail" \ "effects").as[List[JsObject]].exists(effect =>
        (effect \ "scope").as[String] == "support_retention"), text(root))
    }

  test("a pinned pawn's geometric retention never claims an available recapture"):
    val fen = "7k/7p/5p2/6p1/8/4B3/8/K6R b - - 0 1"
    val (_, _, root) = project(fen,
      "h7h6 a1a2 f6f5", "f6f5", Some("retention-pinned"))
    assert(text(root).contains("keeps covering it through that move"), text(root))
    assert(!text(root).contains("actually recaptures"), text(root))
    val owner = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    assert(owner.extend(moves("h7h6 a1a2 f6f5 e3g5")).isRight)
    assert(owner.extend(moves("h7h6 a1a2 f6f5 e3g5 h6g5")).isLeft,
      "the apparent recapture exposes the king on h8")

  test("a future supported arrival does not erase a different current square's maintained support"):
    val (packet, prepared, root) = project("4r2k/8/5b2/8/8/4R3/8/2B4K w - - 0 1",
      "c1d2 h8g8 e3c3 f6c3 d2c3", "h1g1")
    val path = prepared.paths.find(_.moves.head == "c1d2").get
    val current = MoveWorkProjection.select(packet, prepared).local.find(note =>
      note.edge.evidence.id == path.ids.head && (note.note.detail match
        case MoveWorkProjection.Detail.Support(actor, target, opposition, false) =>
          actor.square.key == "d2" && target.square.key == "e3" && opposition.exists(piece => piece.role.name == "rook" && piece.square.key == "e8")
        case _ => false)).getOrElse(fail("the current rook support must survive the separate future arrival"))
    assert(current.evidence.toSet.subsetOf((root \ "evidence_ids").as[List[String]].toSet))
    List("keeps supporting", "rook on e3", "rook on e8").foreach(value => assert(text(root).contains(value), text(root)))
    assert(text(root).contains("empty c3 square") && text(root).contains("Bxc3"), text(root))

  test("the Qxd4 exchanges retain their captures, losses and physical target endings"):
    val fen = "3rr1k1/pppq1pb1/5nnp/3P2p1/P2p4/2P1NNBP/1P3PP1/R2QR1K1 w - - 0 19"
    val line = "d1d4 f6e4 g3e5 g7e5 f3e5 e8e5 e3g4 e5d5 d4e4"
    val peer = "c3d4 f6d5 e3d5 e8e1 d1e1 d7d5 g3c7 d8c8 a1c1 g7d4 e1d2 d5d7 f3d4 c8c7 c1c7 d7c7 d4f5"
    val (packet, prepared, root) = project(fen, line, peer)
    val body = text(root)
    List("Qxd4", "Bxe5", "Nxe5", "Rxe5", "Qxe4").foreach(move => assert(body.contains(move), body))
    val path = prepared.paths.find(_.moves == moves(line)).get
    assertEquals(prepared.pieceAfter(packet, path, CommentaryFact.Piece("white", "knight", "f3").witness, 6), Some(None))
    assertEquals(prepared.pieceAfter(packet, path, CommentaryFact.Piece("black", "knight", "f6").witness, 9), Some(None))
    val captured = histories(root).filter(effect => target(effect, "knight", "f6") &&
      (effect \ "line_ids").as[List[String]] == List(path.line.ref.id))
    assert(captured.nonEmpty, body)
    captured.foreach { effect =>
      assertEquals((effect \ "through_plies").as[List[Int]], List(9))
      assert((effect \ "occurrence_ids").as[List[String]].contains(path.ids(8)))
    }

  test("surviving target endings retain their physical identities and observed bounds"):
    val line = "d1d4 a4a5 h1g1 g4h5"
    val (packet, prepared, root) = project("8/7k/8/8/r5b1/8/8/3Q3K w - - 0 1", line, "d1d2")
    val body = text(root)
    val path = prepared.paths.find(_.moves == moves(line)).get
    List(("rook", "a4", "a5"), ("bishop", "g4", "h5")).foreach { (role, original, last) =>
      assert(body.contains(s"Black $role on $last") && body.contains(s"$role on $original"), body)
      assertEquals(prepared.pieceAfter(packet, path, CommentaryFact.Piece("black", role, original).witness, 4),
        Some(Some(CommentaryFact.Piece("black", role, last).witness)))
      assert(histories(root).exists(effect => target(effect, role, original) &&
        (effect \ "line_ids").as[List[String]] == List(path.line.ref.id) &&
        (effect \ "through_plies").as[List[Int]] == List(4)), body)
    }
    assert(!body.toLowerCase.contains("safe") && !body.toLowerCase.contains("all attacks ended"), body)

  test("endings preserve continued and renewed attacks and a different attacker's remaining control"):
    val simple = "r6k/8/8/8/8/8/8/R5K1 w - - 0 1"
    val (_, _, continued) = project(simple, "a1a2 a8a7", "g1f1")
    assert(text(continued).contains("remains on a7") && text(continued).contains("still attacks it there"), text(continued))
    val (_, _, renewed) = project(simple, "a1a2 a8b8 a2a3 b8a8", "g1f1")
    assert(text(renewed).contains("remains on a8") && text(renewed).contains("attacks it again there"), text(renewed))

    val nc6 = "b8c6 g5e7 c6e7"
    val nf6 = "e8f6 f1e1 b8c6 c2c3 b4c3 b2c3 a5a4 b3c2 c6e5"
    val (packet, prepared, root) = project("rn2nr1k/2qbbppp/3pp3/p5B1/1p2PN2/1B3NQ1/PPP2PPP/3R1RK1 b - - 3 16", nc6, nf6)
    val body = text(root)
    assert(body.contains("Nxe7") && body.contains("White rook on d1 attacks that target after this move"), body)
    val path = prepared.paths.find(_.moves == moves(nc6)).get
    assertEquals(prepared.pieceAfter(packet, path, CommentaryFact.Piece("black", "pawn", "d6").witness, 3),
      Some(Some(CommentaryFact.Piece("black", "pawn", "d6").witness)))
    assert(histories(root).exists(effect => target(effect, "pawn", "d6") &&
      (effect \ "actor" \ "role").asOpt[String].contains("bishop") &&
      (effect \ "actor" \ "square").asOpt[String].contains("g5") &&
      (effect \ "line_ids").as[List[String]] == List(path.line.ref.id) &&
      (effect \ "through_plies").as[List[Int]] == List(3)), body)
    assert(!body.toLowerCase.contains("pawn is safe"), body)

  test("a short arm cannot borrow the other arm's later capture of the same rook"):
    val short = "a2b4 h8g7"
    val long = "a2c3 h8g7 a1a8"
    val (packet, prepared, root) = project("r6k/8/8/8/8/8/N7/R5K1 w - - 0 1", short, long)
    val shortPath = prepared.paths.find(_.moves == moves(short)).get
    val longPath = prepared.paths.find(_.moves == moves(long)).get
    val original = CommentaryFact.Piece("black", "rook", "a8").witness
    assertEquals(prepared.pieceAfter(packet, shortPath, original, 2), Some(Some(original)))
    assertEquals(prepared.pieceAfter(packet, longPath, original, 3), Some(None))
    val captured = histories(root).filter(effect => target(effect, "rook", "a8") &&
      (effect \ "actor" \ "role").asOpt[String].contains("rook") &&
      (effect \ "actor" \ "square").asOpt[String].contains("a1"))
    assert(captured.nonEmpty && text(root).contains("Rxa8"), text(root))
    captured.foreach { effect =>
      assertEquals((effect \ "line_ids").as[List[String]], List(longPath.line.ref.id))
      assertEquals((effect \ "through_plies").as[List[Int]], List(3))
      assert((effect \ "occurrence_ids").as[List[String]].forall(longPath.ids.contains))
      assert((effect \ "occurrence_ids").as[List[String]].contains(longPath.ids(2)))
    }

  test("an explicit opened attack does not repeat its flat introduction but retains the capture proof"):
    val line = "a1a2 a4b5 a2a8"
    val (_, prepared, root) = project("r6k/8/8/8/b7/8/8/R5K1 w - - 0 1", line, "g1f1")
    val body = text(root)
    assert(body.contains("Opens the line from White rook on a2 to Black rook on a8"), body)
    assert(!body.contains("White rook on a2 attacks Black rook on a8"), body)
    assert(body.contains("Rxa8+"), body)
    val path = prepared.paths.find(_.moves == moves(line)).get
    assert(histories(root).exists(effect => target(effect, "rook", "a8") &&
      (effect \ "actor" \ "role").asOpt[String].contains("rook") &&
      (effect \ "actor" \ "square").asOpt[String].contains("a2") &&
      (effect \ "attack_ply").asOpt[Int].contains(2) &&
      (effect \ "line_ids").as[List[String]] == List(path.line.ref.id) &&
      (effect \ "through_plies").as[List[Int]] == List(3) &&
      (effect \ "occurrence_ids").as[List[String]].contains(path.ids(2))), body)
