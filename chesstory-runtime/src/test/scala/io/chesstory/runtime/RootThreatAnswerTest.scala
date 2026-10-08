package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.{MoveReviewAssembler, MoveReviewInputAdmission}
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.CanonicalPositionHistory

/** Inline continuations from the already opened Nxe5/Rd8 position. No fixture,
  * engine score, pass probe or new discovery policy supplies these facts.
  */
class RootThreatAnswerTest extends munit.FunSuite:
  import CommentaryAnswer.*
  import MoveWorkProjection.Position
  import RootQuestionSummary.MoveRef

  private val prefix = ("d2d4 g8f6 c2c4 e7e6 g1f3 b7b6 g2g3 c8a6 d1c2 a6b7 f1g2 c7c5 " +
    "d4d5 e6d5 c4d5 f6d5 e1g1 f8e7 f1d1 b8c6 c2f5 d5f6 e2e4 d7d6 e4e5 d8d7 " +
    "f5d7 f6d7 e5d6 e7f6 d1e1 e8f8 b1c3 c6b4 f3e5").split(" ").toList
  private val owner = CanonicalPositionHistory.from(chess.variant.Standard.initialFen.value, Nil,
    chess.variant.Standard.initialFen.value).toOption.get.extend(prefix).toOption.get
  private val removal = List("d7e5", "g2b7")
  private val support = List("a8d8", "g2b7")
  private val recapture = List("a8d8", "e5d7", "d8d7")

  private def admitted(prepared: List[String], witnesses: List[List[String]] = Nil) =
    val lines = List(removal, prepared) ++ witnesses
    lines.foreach(moves => assert(owner.extend(moves).isRight, moves.mkString(" ")))
    val input = MoveReviewInputAdmission.admitIndependent(owner, removal.head, lines = lines).get
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(input)).get
    val paths = ObservedLineProtocol.prepare(packet)
    val pair = List(removal, prepared).map(moves => paths.paths.find(_.moves == moves).get)
    val positions = packet.resources.map(view => view.position -> new Position(view)).toMap
    (packet, paths, pair, positions)

  private def piece(square: String, role: String, side: chess.Color) =
    RelationColoredPieceWitness(EvidenceSquare(square), EvidencePieceRole(role), side)

  test("a real support witness retains both physical knights, its provider and its own material path"):
    val (packet, paths, pair, positions) = admitted(support, List(recapture))
    val answer = RootThreatSummary.readAnswer(packet, paths, pair, positions).get
    val statements = answer.core.flatMap(_.statements)
    val contrast = statements.collectFirst { case value: AttackerRemovalOrSupport => value }.get
    val response = statements.collectFirst { case value: SupportRecapture => value }.get
    val materials = statements.collect { case value: Material => value }
    val attacker = piece("e5", "knight", chess.White)
    val target = piece("d7", "knight", chess.Black)
    val provider = piece("a8", "rook", chess.Black)
    assertEquals(answer.question, "attacker_removal_or_recapture")
    assertEquals((contrast.attacker, contrast.target, contrast.provider), (attacker, target, provider))
    assertEquals(contrast.references, pair.map(MoveRef(_, 0)))
    assert(response.after.path eq pair(1))
    assert(!pair.exists(_ eq response.witness))
    assert(paths.paths.exists(_ eq response.witness))
    assertEquals(response.witness.ids.head, pair(1).ids.head)
    assertEquals(response.witness.moves, recapture)
    assertEquals(response.use.originalCapturer, attacker)
    assertEquals(response.use.originalTarget, target)
    assertEquals(response.use.targetCapture.originalActor, Some(attacker))
    assertEquals(response.use.targetCapture.originalVictim, Some(target))
    assertEquals(response.use.recapture.originalActor, Some(provider))
    assertEquals(response.use.recapture.originalVictim, Some(attacker))
    assertEquals(response.use.provider.square, EvidenceSquare("d8"))
    assert(response.use.support.sourceIds.nonEmpty)
    val sources = positions(response.witness.positions(response.use.supportCount)).sourceRefs(response.use.support.sourceIds) ++
      List(response.use.targetCapture, response.use.recapture).flatMap(event =>
        positions(response.witness.positions(event.plyOffset)).sourceRefs(List(event.sourceId)))
    assert(sources.nonEmpty && sources.forall(answer.evidence.contains))
    answer.evidence.foreach(id => assert(packet.evidenceGraph.byId.get(id).exists(packet.evidenceGraph.proofEligible), id))
    assertEquals(materials.size, 3)
    materials.foreach { value =>
      val original = value.path.certified.flatMap(_._2.material).get
      assert(value.cost.events.forall(event => original.events.exists(_ eq event)))
      assert(value.references.forall(answer.required.contains))
    }
    pair.foreach(path => assert(path.moves.indices.forall(offset => answer.required.contains(MoveRef(path, offset)))))
    val witnessCost = materials.find(_.path eq response.witness).get.cost
    assertEquals(witnessCost.events.map(_.plyOffset), List(1, 2))
    assertEquals(witnessCost.commonExchanges, List(EvidencePieceRole("knight") -> 1))
    assertEquals(witnessCost.whiteAdditionalLosses, Nil)
    assertEquals(witnessCost.blackAdditionalLosses, Nil)
    assert(answer.core.forall(_.focus.isEmpty), "the reader has no selected local focus to invent")
    val result = RootThreatSummary.read(packet, paths, pair, positions).get
    assertEquals((result.json \ "witness_line_ids").as[List[String]], List(response.witness.line.ref.id))

  test("an observed primary keeps the later countercapture and avoids duplicating the witness account"):
    val extended = recapture ++ List("g2b7", "d7b7")
    val (packet, paths, pair, positions) = admitted(extended)
    val answer = RootThreatSummary.readAnswer(packet, paths, pair, positions).get
    val statements = answer.core.flatMap(_.statements)
    val response = statements.collectFirst { case value: SupportRecapture => value }.get
    val materials = statements.collect { case value: Material => value }
    assert(response.witness eq pair(1))
    assertEquals(materials.size, 2)
    val cost = materials.find(_.path eq pair(1)).get.cost
    assertEquals(cost.events.map(_.plyOffset), List(1, 2, 3, 4))
    assertEquals(cost.offsets, List(0, 1, 2, 3, 4))
    assertEquals(cost.commonExchanges, List(EvidencePieceRole("knight") -> 1, EvidencePieceRole("bishop") -> 1))
    assertEquals(cost.whiteAdditionalLosses, Nil)
    assertEquals(cost.blackAdditionalLosses, Nil)
    assert(pair(1).moves.indices.forall(offset => answer.required.contains(MoveRef(pair(1), offset))))
    assert(RootThreatSummary.read(packet, paths, pair, positions).nonEmpty)

  test("support geometry without the retained legal capture and recapture does not answer the threat"):
    val (packet, paths, pair, positions) = admitted(support)
    assert(RootThreatSummary.readAnswer(packet, paths, pair, positions).isEmpty)
