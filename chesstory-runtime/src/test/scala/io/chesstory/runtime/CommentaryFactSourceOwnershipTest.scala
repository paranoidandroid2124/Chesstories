package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.*
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.CanonicalPositionHistory

/** Synthetic legal paths test evidence ownership, never engine value or a book conclusion. */
class CommentaryFactSourceOwnershipTest extends munit.FunSuite:
  import CommentaryFact.*

  private val repeatedFen = "7k/7p/8/8/8/5N2/7P/6K1 w - - 0 1"
  private val repeatedMoves = List("f3h4", "h7h6", "h4f3", "h8g8")
  private val knightMove = Predicate("legal_move", Some(Piece("white", "knight", "f3")), square = Some("h4"))

  private def packet(fen: String, moves: List[String]): EvidenceBackedJudgmentPacket =
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    val continuation = history.admitToFirstAutomaticTerminal(moves).toOption.get
    assertEquals(continuation.stepsAfter(history).get.map(_.uci), moves)
    val input = MoveReviewInputAdmission.admitIndependent(history, moves.head).get
    val retained = MoveReviewInputAdmission.retainLegalContinuations(input, List(continuation))
    EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(retained)).get

  private def path(prepared: ObservedLineProtocol.PreparedPaths, moves: List[String]) =
    prepared.paths.find(_.moves == moves).get

  private def moveRecord(source: EvidenceBackedJudgmentPacket, position: PositionNodeRef,
      move: String): EvidenceRecord = source.resources.find(_.position == position).get.records.find(_.payload match
    case fact: RelationFactEvidence => fact.detail match
      case legal: RelationWitnessDetail.LegalMove => legal.moveUci == move
      case _ => false
    case _ => false).get

  private def eligible(source: EvidenceBackedJudgmentPacket, ids: List[String]): Unit =
    assert(ids.nonEmpty)
    assert(ids.forall(id => source.evidenceGraph.byId.get(id).exists(source.evidenceGraph.proofEligible)),
      "every published source must resolve to eligible evidence, not a reusable semantic ID")

  test("the same legal move at different positions keeps only its exact position evidence"):
    val source = packet(repeatedFen, repeatedMoves)
    val prepared = ObservedLineProtocol.prepare(source)
    val line = path(prepared, repeatedMoves)
    val before = moveRecord(source, line.positions(0), "f3h4")
    val later = moveRecord(source, line.positions(4), "f3h4")
    assertEquals(before.payload.asInstanceOf[RelationFactEvidence].semanticId,
      later.payload.asInstanceOf[RelationFactEvidence].semanticId)
    assertNotEquals(before.ref, later.ref)
    List(0 -> (before, later), 4 -> (later, before)).foreach { (count, records) =>
      val value = CommentaryFactAtoms.read(source, prepared, line, count, knightMove)
      assertEquals(value.value, Some("legal"))
      eligible(source, value.sourceIds)
      assert(value.sourceIds.contains(records._1.ref.id))
      assert(!value.sourceIds.contains(records._2.ref.id), "another position cannot lend the repeated move's evidence")
      assert(value.sourceIds.contains(prepared.positionView(line, count).get.occurrence.id))
      assert(line.ids.take(count).forall(value.sourceIds.contains), "original identity and move anchors must remain")
    }

  test("a missing current legal record cannot borrow the same move fact from an earlier position"):
    val source = packet(repeatedFen, repeatedMoves)
    val prepared = ObservedLineProtocol.prepare(source)
    val line = path(prepared, repeatedMoves)
    val missing = moveRecord(source, line.positions(4), "f3h4")
    val retained = moveRecord(source, line.positions(0), "f3h4")
    val incomplete = EvidenceBackedJudgmentPacket.fromAssembly(source.assembly.copy(
      evidenceGraph = TypedEvidenceGraph.empty.addAll(source.evidenceGraph.records.filterNot(_ == missing)))).get
    assert(incomplete.evidenceGraph.byId.contains(retained.ref.id))
    val incompletePaths = ObservedLineProtocol.prepare(incomplete)
    val value = CommentaryFactAtoms.read(incomplete, incompletePaths, path(incompletePaths, repeatedMoves), 4, knightMove)
    assertEquals(value.value, None)
    assert(!value.sourceIds.contains(retained.ref.id))
    assert(value.reason.nonEmpty)

  test("direct support use owns its support, loss and recapture at their separate event positions"):
    val moves = List("f1d3", "d5e4", "d3e4", "d8d3", "h1g2", "d3f3", "e4f3")
    val source = packet("3r2k1/8/8/3p4/4P3/5N2/3P4/3Q1B1K w - - 0 1", moves)
    val prepared = ObservedLineProtocol.prepare(source)
    val line = path(prepared, moves)
    val value = CommentaryFactAtoms.read(source, prepared, line, moves.size,
      Predicate("direct_support_use", Some(Piece("white", "bishop", "f1")), Some(Piece("white", "knight", "f3"))))
    assertEquals(value.value, Some("true"))
    eligible(source, value.sourceIds)
    val support = source.resources.find(_.position == line.positions(5)).get.records.find(_.payload match
      case fact: RelationFactEvidence => fact.detail match
        case RelationWitnessDetail.GeometricControl(chess.White, from, role, to) =>
          from.key == "e4" && role.name == "bishop" && to.key == "f3"
        case _ => false
      case _ => false).get
    assert(value.sourceIds.contains(support.ref.id))
    assert(value.sourceIds.contains(moveRecord(source, line.positions(5), "d3f3").ref.id))
    assert(value.sourceIds.contains(moveRecord(source, line.positions(6), "e4f3").ref.id))
    assert(line.ids.forall(value.sourceIds.contains), "the priced loss/recovery edges retain their original owners")

  test("a local probe keeps semantic sources and cannot silently repair admitted missing evidence"):
    val source = packet(repeatedFen, repeatedMoves)
    val prepared = ObservedLineProtocol.prepare(source)
    val line = path(prepared, repeatedMoves)
    val missing = moveRecord(source, line.positions(4), "f3h4")
    val incomplete = EvidenceBackedJudgmentPacket.fromAssembly(source.assembly.copy(
      evidenceGraph = TypedEvidenceGraph.empty.addAll(source.evidenceGraph.records.filterNot(_ == missing)))).get
    val history = incomplete.assembly.input.positionHistory
    val probe = OccurrenceResources.probePaths(incomplete, List(history.extend(repeatedMoves).toOption.get)).head.at(4).get
    val local = CommentaryFactAtoms.readProbe(probe, knightMove)
    assertEquals(local.value, Some("legal"))
    assert(local.sourceIds.contains(missing.payload.asInstanceOf[RelationFactEvidence].semanticId))
    assert(!local.sourceIds.exists(incomplete.evidenceGraph.byId.contains), "inspection is not public evidence admission")
    val incompletePaths = ObservedLineProtocol.prepare(incomplete)
    assertEquals(CommentaryFactAtoms.read(incomplete, incompletePaths,
      path(incompletePaths, repeatedMoves), 4, knightMove).value, None)

  test("an exact path from a foreign root is rejected before normalizing its sources"):
    val source = packet(repeatedFen, repeatedMoves)
    val prepared = ObservedLineProtocol.prepare(source)
    val foreign = packet("7k/p6p/8/8/8/5N2/7P/6K1 w - - 0 1", repeatedMoves)
    val value = CommentaryFactAtoms.read(foreign, prepared, path(prepared, repeatedMoves), 4, knightMove)
    assertEquals(value.value, None)
    assertEquals(value.reason, Some("packet_owner_mismatch"))
    assertEquals(value.sourceIds, Nil)

  test("long exact control and passage evidence does not enter compact discovery provenance"):
    import CommentaryProposal.*
    val source = packet(repeatedFen, repeatedMoves)
    val prepared = ObservedLineProtocol.prepare(source)
    val line = path(prepared, repeatedMoves)
    val records = source.resources.find(_.position == line.positions.head).get.records
    val cases = List(
      Predicate("control", Some(Piece("white", "knight", "f3")), square = Some("h4")) ->
        records.find(_.payload match
          case fact: RelationFactEvidence => fact.detail match
            case RelationWitnessDetail.GeometricControl(chess.White, from, role, to) =>
              from.key == "f3" && role.name == "knight" && to.key == "h4"
            case _ => false
          case _ => false).get,
      Predicate("pawn_advance", Some(Piece("white", "pawn", "h2")), square = Some("h3")) ->
        records.find(_.payload match
          case fact: RelationFactEvidence => fact.detail match
            case RelationWitnessDetail.PawnAdvanceAffordance(chess.White, from, to, _) =>
              from.key == "h2" && to.key == "h3"
            case _ => false
          case _ => false).get)
    cases.foreach { (predicate, record) =>
      val admitted = CommentaryFactAtoms.read(source, prepared, line, 0, predicate)
      val inspected = CommentaryFactAtoms.readForDiscovery(source, prepared, line, 0, predicate)
      assertEquals(admitted.value, Some("present"))
      assertEquals(inspected.value, admitted.value)
      assert(record.ref.id.length > 256, "this actual exact relation must exercise the request-source length boundary")
      assert(admitted.sourceIds.contains(record.ref.id))
      eligible(source, admitted.sourceIds)
      assert(inspected.sourceIds.contains(record.payload.asInstanceOf[RelationFactEvidence].semanticId))
      assert(!inspected.sourceIds.contains(record.ref.id), "inspection is provenance, not the public certificate")
      assert(inspected.sourceIds.forall(_.length <= 256))
      assert(inspected.sourceIds.contains(prepared.positionView(line, 0).get.occurrence.id))
      def request(ids: List[String]) = Request("compact-inspection", Origin(
        Source("generator", "inspection", sourceRefs = ids), Source("none", "existing-candidates"),
        Source("none", "existing-priority"), Source("none", "existing-evidence")),
        List(Fact("inspection_fact", Path("inspection", Declared(repeatedMoves)), predicate,
          Ply(0), expected = Some("present"))))
      assert(validRequests(List(request(inspected.sourceIds))))
      assert(!validRequests(List(request(admitted.sourceIds))), "the existing compact-source limit stays strict")
    }

  test("direct-use inspection retains three semantic positions while public proof keeps their exact owners"):
    val moves = List("f1d3", "d5e4", "d3e4", "d8d3", "h1g2", "d3f3", "e4f3")
    val source = packet("3r2k1/8/8/3p4/4P3/5N2/3P4/3Q1B1K w - - 0 1", moves)
    val prepared = ObservedLineProtocol.prepare(source)
    val line = path(prepared, moves)
    val predicate = Predicate("direct_support_use", Some(Piece("white", "bishop", "f1")),
      Some(Piece("white", "knight", "f3")))
    val admitted = CommentaryFactAtoms.read(source, prepared, line, moves.size, predicate)
    val inspected = CommentaryFactAtoms.readForDiscovery(source, prepared, line, moves.size, predicate)
    assertEquals(inspected.value, Some("true"))
    assertEquals(inspected.value, admitted.value)
    eligible(source, admitted.sourceIds)
    val support = source.resources.find(_.position == line.positions(5)).get.records.find(_.payload match
      case fact: RelationFactEvidence => fact.detail match
        case RelationWitnessDetail.GeometricControl(chess.White, from, role, to) =>
          from.key == "e4" && role.name == "bishop" && to.key == "f3"
        case _ => false
      case _ => false).get
    List(support, moveRecord(source, line.positions(5), "d3f3"),
      moveRecord(source, line.positions(6), "e4f3")).foreach { record =>
      assert(admitted.sourceIds.contains(record.ref.id))
      assert(inspected.sourceIds.contains(record.payload.asInstanceOf[RelationFactEvidence].semanticId))
      assert(!inspected.sourceIds.contains(record.ref.id))
    }
    assert(inspected.sourceIds.forall(_.length <= 256))
    assert(line.ids.forall(inspected.sourceIds.contains), "candidate provenance retains exact original identity anchors")

  test("candidate inspection does not repair a missing current record with an earlier semantic duplicate"):
    val source = packet(repeatedFen, repeatedMoves)
    val prepared = ObservedLineProtocol.prepare(source)
    val line = path(prepared, repeatedMoves)
    val missing = moveRecord(source, line.positions(4), "f3h4")
    val incomplete = EvidenceBackedJudgmentPacket.fromAssembly(source.assembly.copy(
      evidenceGraph = TypedEvidenceGraph.empty.addAll(source.evidenceGraph.records.filterNot(_ == missing)))).get
    val incompletePaths = ObservedLineProtocol.prepare(incomplete)
    val inspected = CommentaryFactAtoms.readForDiscovery(incomplete, incompletePaths,
      path(incompletePaths, repeatedMoves), 4, knightMove)
    assertEquals(inspected.value, None)
    assertEquals(inspected.reason, Some("incomplete_legal_inventory"))
    assertEquals(inspected.sourceIds, Nil)
