package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.*
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.CanonicalPositionHistory
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path as FilePath}
import java.security.MessageDigest
import java.util.zip.GZIPInputStream
import scala.concurrent.duration.*
import play.api.libs.json.*

/** Exposed development controls. The book supplies legal positions/history,
  * never a numerical receipt or an assertion that the sampled line is forced. */
class FunctionalRealizationReadingTest extends munit.FunSuite:
  import CommentaryFact.{Piece as OriginalPiece, *}
  import CommentaryProposal.*
  import CommentaryAnswer.*

  override def munitTimeout = 5.minutes

  private val fen = "3rnrk1/3n1pbp/1pq3p1/2p1p3/4P3/4B1PP/1PPNQPB1/R2R2K1 w - - 2 19"
  private val source = List("d2b1", "e8c7", "e2d3", "c5c4", "d3d2", "c7b5", "b1c3", "b5c3",
    "d2c3", "d8a8", "a1a8", "f8a8", "c3d2", "d7c5", "d2b4", "c5e6", "d1d6", "c6c8", "d6b6")
  private val arrival = source.take(17)
  private val captured = arrival :+ "c6d6"
  private val recovered = captured :+ "b4d6"
  private val earlyReference = List("d2c4", "e8f6", "d1d6", "c6d6")
  private val earlyAlternative = List("d2b1", "e8f6", "d1d6", "c6d6")
  private val earlyRecovery = earlyReference :+ "c4d6"
  private val originalUser = OriginalPiece("white", "rook", "d1")
  private val originalProvider = OriginalPiece("white", "knight", "d2")
  private val resource = FunctionalReplyReading.Resource(
    FunctionalReplyReading.RootOwner("synthetic-owned-function", CompareMoves("function_root", Nil, "d2c4", "d2b1")),
    Predicate("control", Some(originalProvider), square = Some("d6")), originalUser, "d6", "d1d6",
    List("d2c4", "e8f6"), "synthetic-function-context", "present", "absent", Nil)

  private def admitted(lines: List[List[String]], initialFen: String = fen,
      conditional: List[List[String]] = Nil) =
    val history = CanonicalPositionHistory.from(initialFen, Nil, initialFen).toOption.get
    val supplied = (lines ++ List(List("d2c4"), List("d2b1"))).distinct
      .map(SuppliedCommentaryLine(CommentaryLineSource.Study, _))
    val input = MoveReviewInputAdmission.admitIndependent(history, "d2b1", suppliedLines = supplied).get
    val initialPacket = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(input)).get
    val retained = if conditional.isEmpty then input else
      val histories = conditional.map(moves => input.positionHistory.extend(moves).toOption.get)
      val probes = OccurrenceResources.probePaths(initialPacket, histories)
      MoveReviewInputAdmission.retainProbePaths(input, probes, discoveryOnly = true).get
    val packet = if conditional.isEmpty then initialPacket else
      EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(retained)).get
    packet -> ObservedLineProtocol.prepare(packet)

  private def path(packet: EvidenceBackedJudgmentPacket, paths: ObservedLineProtocol.PreparedPaths,
      moves: List[String]) = paths.certifiedPath(packet, moves).get

  private def stage(packet: EvidenceBackedJudgmentPacket, paths: ObservedLineProtocol.PreparedPaths,
      moves: List[String] = source, offset: Int = 16) =
    FunctionalRealizationReading.stage(packet, paths, resource, path(packet, paths, moves), offset).get

  private def earlyOption(packet: EvidenceBackedJudgmentPacket, paths: ObservedLineProtocol.PreparedPaths) =
    val effect = Predicate("legal_capture", Some(originalProvider), Some(OriginalPiece("black", "queen", "c6")))
    val proof = FunctionalReplyReading.Proof(resource.owner, resource.condition, earlyReference, earlyAlternative,
      earlyRecovery, effect, effect, Nil)
    LegalReplyOption(path(packet, paths, earlyReference), path(packet, paths, earlyAlternative),
      path(packet, paths, earlyRecovery), proof)

  private def physical(move: RootQuestionSummary.MoveRef) =
    (move.path.ids(move.offset), move.path.positions(move.offset), move.path.positions(move.offset + 1),
      move.path.moves(move.offset))

  test("the same original rook can have a replacement queen supporter after the original knight is captured"):
    val (packet, paths) = admitted(List(source))
    val value = stage(packet, paths)
    assertEquals(paths.trackedPieceAfter(value.path, originalProvider.witness, 17), Some(None))
    assertEquals(value.providers.map(provider => (provider.original.square.key, provider.current.square.key)),
      List("e2" -> "b4"))
    assertEquals(value.before.square.key, "d1")
    assertEquals(value.arrived.square.key, "d6")
    val reading = FunctionalRealizationReading.read(packet, paths, value)
    assertEquals(reading.checked, None)
    assertEquals(reading.reason, Some("capture_trace_not_admitted"))
    assertEquals(reading.missing, List(captured))

  test("a legal queen capture does not certify the still-unadmitted queen recovery"):
    val (packet, paths) = admitted(List(source, captured))
    val reading = FunctionalRealizationReading.read(packet, paths, stage(packet, paths))
    assertEquals(reading.checked, None)
    assertEquals(reading.missing, List(recovered))
    assertEquals(reading.reason, Some("capture_trace_not_admitted"))

  test("the complete conditional price retains the lost original rook and the captured black queen"):
    val (packet, paths) = admitted(List(source, captured, recovered))
    val checked = FunctionalRealizationReading.read(packet, paths, stage(packet, paths)).checked.get
    assert(FunctionalRealizationReading.valid(paths, checked))
    assertEquals(checked.capturers.map(piece => (piece.role.name, piece.square.key)), List("queen" -> "c6"))
    assertEquals(checked.prices.size, 1)
    val events = checked.prices.head.events.filter(_.plyOffset > checked.stage.offset)
    assertEquals(events.map(event => (event.plyOffset, event.capture.get.capturedRole.name)),
      List(17 -> "rook", 18 -> "queen"))
    assertEquals(checked.covered.map(_.offset), List(17, 18))
    assertEquals(checked.priceReferences.map(_.offset), List(17, 18))
    assert(!checked.covered.exists(_.offset == 18) || !checked.prices.head.path.moves.take(19).contains("d6b6"),
      "the actual later pawn capture is not grafted onto the conditional queen recovery")

  test("the same geometric supporter is not a legal recovery while an intervening knight remains"):
    val held = source.take(15) ++ List("h7h5", "d1d6", "c6d6")
    val (packet, paths) = admitted(List(held))
    val line = path(packet, paths, held)
    assert(paths.positionView(line, 16).flatMap(_.legalRows).exists(_.exists(_.move == "d1d6")))
    assert(paths.positionView(line, 18).flatMap(_.legalRows).exists(rows => !rows.exists(_.move == "b4d6")))
    assertEquals(FunctionalRealizationReading.stage(packet, paths, resource, line, 16), None)

  test("retaining the second black rook closes both capturer branches and both second recaptures"):
    val prefix = List("d2b1", "e8c7", "e2d3", "c5c4", "d3d2", "c7e6", "d2b4", "d7f6", "d1d6")
    val byRook = prefix ++ List("d8d6", "b4d6", "c6d6")
    val byQueen = prefix ++ List("c6d6", "b4d6", "d8d6")
    val (packet, paths) = admitted(List(prefix, byRook, byQueen))
    val value = stage(packet, paths, prefix, 8)
    val checked = FunctionalRealizationReading.read(packet, paths, value).checked.get
    assertEquals(checked.capturers.map(_.role.name).sorted, List("queen", "rook"))
    assertEquals(checked.prices.map(_.through), List(11, 11))
    assert(checked.prices.forall(price => price.events.filter(_.plyOffset > 8).size == 3))
    assert(checked.prices.forall(price => price.events.last.capture.exists(_.capturedRole.name == "queen")))
    val (partialPacket, partialPaths) = admitted(List(prefix, byRook))
    val partial = FunctionalRealizationReading.read(partialPacket, partialPaths,
      stage(partialPacket, partialPaths, prefix, 8))
    assertEquals(partial.checked, None)
    assert(partial.missing.contains(prefix :+ "c6d6"))

  test("a queen supporting the destination can still block the original rook's access"):
    val qd3 = source.take(4)
    val qd2 = source.take(6)
    val (packet, paths) = admitted(List(qd3, qd2))
    List(qd3, qd2).foreach { moves =>
      val line = path(packet, paths, moves)
      assert(paths.positionView(line, moves.size).flatMap(_.legalRows).exists(rows =>
        !rows.exists(_.move == "d1d6")))
    }
    assertEquals(FunctionalRealizationReading.stages(packet, paths, List(resource)), Nil)

  test("foreign preparation, the wrong original rook and a stale original provider cannot own the stage"):
    val (packet, paths) = admitted(List(source, recovered))
    val checked = FunctionalRealizationReading.read(packet, paths, stage(packet, paths)).checked.get
    val (_, foreign) = admitted(List(source, recovered))
    assert(!FunctionalRealizationReading.valid(foreign, checked))
    assertEquals(FunctionalRealizationReading.stage(packet, paths,
      resource.copy(actor = OriginalPiece("white", "rook", "a1")), path(packet, paths, source), 16), None)
    assertEquals(FunctionalRealizationReading.stage(packet, paths, resource.copy(condition =
      Predicate("control", Some(OriginalPiece("white", "queen", "e2")), square = Some("d6"))),
      path(packet, paths, source), 16), None)

  test("an unrelated final forecast changes neither the functional key nor the needed capture prefix"):
    val other = source.take(17) ++ List("c6c7", "d6b6")
    val (packet, paths) = admitted(List(source, other))
    val left = stage(packet, paths)
    val right = stage(packet, paths, other)
    assertEquals(left.id, right.id)
    assertEquals(FunctionalRealizationReading.read(packet, paths, left).missing,
      FunctionalRealizationReading.read(packet, paths, right).missing)
    val entries = FunctionalRealizationReading.withQuestions(packet, paths, Nil, List(resource))
    assertEquals(entries.size, 1)
    assert(CommentaryProposal.validRequests(entries.map(_.request)))
    assertEquals(entries.head.request.requirements.collect { case move: ObserveMove =>
      move.prefixMovesUci :+ move.moveUci }, List(captured))

  test("a missing exact current legal row cannot be borrowed from a different position"):
    val (packet, paths) = admitted(List(source, recovered))
    val value = stage(packet, paths)
    val removed = packet.resources.find(_.position == value.path.positions(17)).get.records.find(_.payload match
      case fact: RelationFactEvidence => fact.detail match
        case move: RelationWitnessDetail.LegalMove => move.moveUci == "c6d6"
        case _ => false
      case _ => false).get
    val incomplete = EvidenceBackedJudgmentPacket.fromAssembly(packet.assembly.copy(
      evidenceGraph = TypedEvidenceGraph.empty.addAll(packet.evidenceGraph.records.filterNot(_ == removed)))).get
    val incompletePaths = ObservedLineProtocol.prepare(incomplete)
    assertEquals(FunctionalRealizationReading.stage(incomplete, incompletePaths, resource,
      path(incomplete, incompletePaths, source), 16), None)

  test("factual detail preserves the entire previous core and publishes every conditional cost reference"):
    val (packet, paths) = admitted(List(source, captured, recovered))
    val pair = List(List("d2c4"), List("d2b1")).map(path(packet, paths, _))
    val previous = RootQuestionSummary.completePairAnswer(packet, paths, pair, "recorded_scope", Nil).get
    val task = FunctionalChoiceReading.Task(resource, Nil, Nil, Set.empty, Set.empty)
    assert(!task.complete, "this factual statement does not borrow a useful-choice certificate")
    val result = FunctionalRealizationReading.withDetail(packet, paths, pair, List(task), previous)
    assertEquals(result.core, previous.core)
    assertEquals(result.question, previous.question)
    assertEquals(result.comparison, previous.comparison)
    val statement = result.details.flatMap(_.statements).collectFirst { case value: FunctionRealization => value }.get
    assertEquals(statement.covered.map(_.offset), List(17, 18))
    val delivered = RootQuestionSummary.realize(paths, pair, result).get
    val text = CommentaryEnglish.statement(statement).map {
      case move: RootQuestionSummary.MoveRef => move.path.moves(move.offset)
      case word: String => word
    }.mkString
    assert(text.contains("queen on b4"))
    assert(text.contains("c6d6"))
    assert(text.contains("b4d6"))
    assert(!text.contains("useful") && !text.contains("safe") && !text.contains("wins"))
    assert((delivered.json \ "detail_units").as[play.api.libs.json.JsArray].value.nonEmpty)

  test("a quiet checking arrival requires a response contract instead of a material-only supporter claim"):
    val initial = "3k4/8/R7/8/1Q6/8/3N4/7K w - - 0 1"
    val moves = List("d2b1", "d8e8", "h1h2", "e8d8", "a6d6")
    val (packet, paths) = admitted(List(moves), initial)
    val adjusted = resource.copy(actor = OriginalPiece("white", "rook", "a6"), move = "a6d6")
    val value = FunctionalRealizationReading.stage(packet, paths, adjusted, path(packet, paths, moves), 4).get
    val reading = FunctionalRealizationReading.read(packet, paths, value)
    assertEquals(reading.checked, None)
    assertEquals(reading.reason, Some("checking_arrival_requires_response"))

  test("an available fourth capture keeps the entire conditional price partial rather than dropping a recapture"):
    val initial = "3rnrk1/3n1pbp/1pq3p1/2p1B3/4P3/4B1PP/1PPNQPB1/R2R2K1 w - - 2 19"
    val prefix = List("d2b1", "e8c7", "e2d3", "c5c4", "d3d2", "c7e6", "d2b4", "d7f6", "d1d6")
    val byRook = prefix ++ List("d8d6", "b4d6", "c6d6")
    val byQueen = prefix ++ List("c6d6", "b4d6", "d8d6")
    val (packet, paths) = admitted(List(prefix, byRook, byQueen), initial)
    val value = FunctionalRealizationReading.stage(packet, paths, resource, path(packet, paths, prefix), 8).get
    val reading = FunctionalRealizationReading.read(packet, paths, value)
    assertEquals(reading.checked, None)
    assertEquals(reading.reason, Some("capture_horizon_exceeded"))

  test("detail can reuse only the option's exact already-certified recovery prefix without changing its old proof or prices"):
    val (packet, paths) = admitted(List(earlyReference, earlyAlternative, earlyRecovery))
    val option = earlyOption(packet, paths)
    assert(FunctionalReplyReading.valid(paths, option))
    val first = option.reference.certified.flatMap(_._2.material).get.events.filter(_.plyOffset == 3)
    val next = option.available.certified.flatMap(_._2.material).get.events.filter(_.plyOffset == 4)
    val section = Section(List(option, MaterialEvents(option.reference, first), MaterialEvents(option.available, next)),
      List(RootQuestionSummary.MoveRef(option.reference, 3), RootQuestionSummary.MoveRef(option.available, 4)))
    val normalized = FunctionalRealizationReading.normalizeDetail(paths, List(section)).get.head
    val changed = normalized.statements.collectFirst { case value: LegalReplyOption => value }.get
    assert(changed.reference eq option.available)
    assertEquals(changed.proof, option.proof)
    assertEquals(changed.alternative, option.alternative)
    assertEquals(changed.available, option.available)
    assertEquals(changed.price, option.price)
    assertEquals(section.covered.map(physical), normalized.covered.map(physical))
    assertEquals(section.required.map(physical), normalized.required.map(physical))
    assertEquals(normalized.statements.collect { case value: MaterialEvents => value.events }, List(first, next))
    assert(FunctionalReplyReading.valid(paths, changed))

  test("a foreign recovery with identical moves cannot normalize a current detail witness"):
    val lines = List(earlyReference, earlyAlternative, earlyRecovery)
    val (packet, paths) = admitted(lines)
    val (foreignPacket, foreignPaths) = admitted(lines)
    val original = earlyOption(packet, paths)
    val forged = original.copy(available = path(foreignPacket, foreignPaths, earlyRecovery))
    assertEquals(forged.available.moves, original.available.moves)
    assert(!FunctionalReplyReading.valid(paths, forged))
    val section = Section(List(forged))
    val normalized = FunctionalRealizationReading.normalizeDetail(paths, List(section)).get.head
    assertEquals(normalized, section)
    assert(normalized.statements.head.asInstanceOf[LegalReplyOption].reference eq original.reference)

  test("same board placement after a different legal history is not the owned recovery prefix"):
    val divergent = List("d2c4", "e8f6", "g1h2", "g8h8", "h2g1", "h8g8", "d1d6", "c6d6", "c4d6")
    val (packet, paths) = admitted(List(earlyReference, earlyAlternative, earlyRecovery, divergent))
    val original = earlyOption(packet, paths)
    val other = path(packet, paths, divergent)
    assertEquals(paths.trackedPieceAfter(other, originalProvider.witness, divergent.size).flatten,
      paths.trackedPieceAfter(original.available, originalProvider.witness, earlyRecovery.size).flatten)
    val forged = original.copy(available = other)
    assert(!FunctionalReplyReading.valid(paths, forged))
    assertEquals(FunctionalRealizationReading.normalizeDetail(paths, List(Section(List(forged)))).get,
      List(Section(List(forged))))

  test("automatic factual-detail composition preserves prior conditional prices within the unchanged five-witness bound"):
    val other = source.take(17) ++ List("c6c7", "d6b6")
    val (packet, paths) = admitted(List(source, other, earlyReference, earlyAlternative, earlyRecovery),
      conditional = List(captured, recovered))
    val option = earlyOption(packet, paths)
    val first = option.reference.certified.flatMap(_._2.material).get.events.filter(_.plyOffset == 3)
    val next = option.available.certified.flatMap(_._2.material).get.events.filter(_.plyOffset == 4)
    val alternativeCost = option.alternative.certified.flatMap(_._2.material).get.events.filter(_.plyOffset == 3)
    val availableFirst = option.available.certified.flatMap(_._2.material).get.events.filter(_.plyOffset == 3)
    val oldDetails = List(Section(List(option, MaterialEvents(option.reference, first), MaterialEvents(option.available, next),
        MaterialEvents(option.alternative, alternativeCost))),
      Section(List(MaterialEvents(option.available, availableFirst),
        RecordedContext(List(RootQuestionSummary.MoveRef(path(packet, paths, source), 16))))),
      Section(List(RecordedContext(List(RootQuestionSummary.MoveRef(path(packet, paths, other), 16))))))
    val pair = List(List("d2c4"), List("d2b1")).map(path(packet, paths, _))
    val previous = RootQuestionSummary.completePairAnswer(packet, paths, pair, "recorded_scope", Nil).get
      .copy(details = oldDetails)
    assert(RootQuestionSummary.realize(paths, pair, previous).nonEmpty)
    val task = FunctionalChoiceReading.Task(resource, Nil, Nil, Set.empty, Set.empty)
    val result = FunctionalRealizationReading.withDetail(packet, paths, pair, List(task), previous)
    assertEquals(result.core, previous.core)
    assertEquals(result.comparison, previous.comparison)
    assert(result.details.flatMap(_.statements).exists(_.isInstanceOf[FunctionRealization]))
    assertEquals(result.details.take(oldDetails.size).flatMap(_.covered).map(physical),
      oldDetails.flatMap(_.covered).map(physical))
    assertEquals(result.details.flatMap(_.statements).collect { case value: LegalReplyOption => value.proof },
      List(option.proof))
    assert(RootQuestionSummary.realize(paths, pair, result).nonEmpty)

  test("a complete local inspection remains planning data until its exact trace is admitted"):
    val (packet, paths) = admitted(List(source))
    val history = packet.assembly.input.positionHistory.extend(recovered).toOption.get
    val probes = OccurrenceResources.probePaths(packet, List(history))
    assertEquals(probes.map(_.moves), List(recovered))
    assertEquals(FunctionalRealizationReading.read(packet, paths, stage(packet, paths)).checked, None)
    val input = MoveReviewInputAdmission.retainProbePaths(packet.assembly.input, probes).get
    val admittedPacket = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(input)).get
    val admittedPaths = ObservedLineProtocol.prepare(admittedPacket)
    val observed = stage(admittedPacket, admittedPaths)
    assert(FunctionalRealizationReading.read(admittedPacket, admittedPaths, observed).checked.nonEmpty)
    assertEquals(FunctionalRealizationReading.stages(admittedPacket, admittedPaths, List(resource)).map(_.path.moves),
      List(source), "the conditional trace cannot become a new independent forecast-discovery source")

  test("too many known stages stays explicitly partial instead of silently selecting a convenient subset"):
    val fiveDistinctArrivals = List("c5e6", "c5a4", "c5a6", "c5b7", "c5d7")
      .map(move => source.take(15) ++ List(move, "d1d6"))
    val (packet, paths) = admitted(fiveDistinctArrivals)
    val readings = FunctionalRealizationReading.readings(packet, paths, List(resource))
    assertEquals(readings.size, 5)
    assert(readings.forall(value => value.checked.isEmpty && value.reason.contains("realization_inventory_over_limit")))
    assertEquals(FunctionalRealizationReading.withQuestions(packet, paths, Nil, List(resource)), Nil)

  test("the genuine native26 last-report lifecycle automatically admits and publishes the later factual price in detail"):
    def file(name: String): FilePath =
      val relative = "judgment-evaluation/fixtures/public-commentary-v6/" + name
      List(FilePath.of(relative), FilePath.of("..", relative)).find(Files.isRegularFile(_)).get
    def sha(bytes: Array[Byte]): String = MessageDigest.getInstance("SHA-256").digest(bytes)
      .map(byte => f"${byte & 0xff}%02x").mkString
    def canonical(value: JsValue): JsValue = value match
      case obj: JsObject => JsObject(obj.fields.sortBy(_._1).map((key, field) => key -> canonical(field)))
      case array: JsArray => JsArray(array.value.map(canonical))
      case value => value
    val fixture = file("functional-choice-native26-produced.json.gz")
    assertEquals(sha(Files.readAllBytes(fixture)),
      "5d0595d782c0f47499e17d8d3193b8e0598d34f94c122fb0b77d2a55ad6c8b2d")
    val stream = new GZIPInputStream(Files.newInputStream(fixture))
    val record = try Json.parse(stream.readAllBytes()).as[JsObject] finally stream.close()
    val expectedBytes = Files.readAllBytes(file("functional-choice-native26-expectations.json"))
    assertEquals(sha(expectedBytes), "b421b377fbad3e44e0754975f196576db8a1df1d8ddb2dc6029f15f744c6a53f")
    val expected = Json.parse(expectedBytes).as[JsObject]
    val request = PositionCommentaryJobProtocol.parseJobRequest(
      Json.stringify((record \ "request").get).getBytes(UTF_8)).toOption.get
    var state = CommentaryJobReducer.startJob(request.initialFen, request.movePrefixUci,
      request.currentFen, request.playedMoveUci, request.resultingFen,
      CommentaryJobPolicy(16, 600001L, request.engineProfile, investigateChoices = true, previewDepth = Some(8)),
      1L, request.inputLines, request.proposals).toOption.get
    val serializer = PositionCommentaryJobProtocol.getClass.getDeclaredMethods.find(
      _.getName == "issuedEngineWorkJson").get
    serializer.setAccessible(true)
    val reports = (record \ "saved_reports").as[List[JsObject]]
    assertEquals(reports.size, 26)
    reports.zipWithIndex.foreach { (saved, index) =>
      assert(state.isInstanceOf[AwaitingEngineWork], s"no actually issued work for native receipt $index")
      val work = serializer.invoke(PositionCommentaryJobProtocol,
        state.asInstanceOf[AwaitingEngineWork].issuedWork).asInstanceOf[JsObject]
      assertEquals(work, (saved \ "issued_work").as[JsObject],
        "whole native owners remain exact; the late conditional traces are local observations, not rekeyed values")
      val report = PositionCommentaryJobProtocol.parseEngineWorkReport(
        Json.stringify((saved \ "report").get).getBytes(UTF_8)).toOption.get
      state = CommentaryJobReducer.submitIssuedWorkReport(state, report, index.toLong + 2L).toOption.get
    }
    assert(state.isInstanceOf[CompletedCommentaryJob])
    val packet = state.observationsPacket.get
    val paths = state.preparedPaths.get
    val task = FunctionalChoiceReading.task(packet, paths, state.proposalEntries, state.responseSearches,
      Set("d2c4", "d2b1"), request.engineProfile.wireId, 16).get
    assert(task.complete, "the previously owned current value task is preserved, without a late value graft")
    val late = FunctionalRealizationReading.stages(packet, paths, List(task.resource)).find(value =>
      value.prefix == source.take(16)).get
    val checked = FunctionalRealizationReading.read(packet, paths, late).checked.get
    assertEquals(checked.covered.map(_.path.moves.take(19)).distinct, List(recovered))
    assertEquals(checked.covered.map(_.offset), List(17, 18))
    val observations = state.proposalEntries.filter(_.request.origin.question.producer == FunctionalRealizationReading.Producer)
    assert(observations.nonEmpty)
    assert(observations.flatMap(_.request.requirements).exists {
      case move: ObserveMove => move.prefixMovesUci :+ move.moveUci == recovered
      case _ => false
    })
    val wire = PositionCommentaryJobProtocol.projectJobSnapshot(
      CommentaryJobSnapshot("f" * 32, request.requestId, 600001L, state))
    val primary = (wire \ "result" \ "selected_move_reviews").as[List[JsObject]].find(value =>
      (value \ "selection" \ "roles").as[List[String]].contains("played")).get
      .apply("commentary").as[JsObject].apply("primary").as[JsObject]
    assertEquals(primary, (expected \ "baseline_primary").as[JsObject])
    val owner = task.resource.owner.answer.get
    val summary = (wire \ "observations" \ "selection" \ "candidates").as[List[JsObject]].filter(value =>
      (value \ "selection").as[String] == "selected" &&
        (value \ "detail" \ "reference_line_id").asOpt[String].contains(owner.referenceLineId) &&
        (value \ "detail" \ "candidate_line_id").asOpt[String].contains(owner.alternativeLineId))
      .flatMap(value => (value \ "detail" \ "summary_question").asOpt[JsObject]).head
    val core = (summary \ "units").as[List[JsObject]]
    val detail = (summary \ "detail_units").as[List[JsObject]]
    assertEquals(sha(Json.stringify(canonical(Json.toJson(core))).getBytes(UTF_8)),
      "b0a4e92d8b0c40ccd1b9628bf1a62db6a7b060820a3fa870bbacb43a494157f8",
      "the actual frozen10h CORE is preserved byte-for-byte in canonical form")
    def refs(value: JsValue): Set[JsObject] = value match
      case obj: JsObject if obj.keys.contains("line_id") && obj.keys.contains("occurrence_id") => Set(obj)
      case obj: JsObject => obj.values.flatMap(refs).toSet
      case array: JsArray => array.value.flatMap(refs).toSet
      case _ => Set.empty
    val roots = Set(owner.referenceLineId, owner.alternativeLineId)
    List(core, detail).foreach(layer =>
      assert(layer.size <= 8)
      assert(layer.forall(unit => List("en", "ko").forall(name => unit(name).as[JsArray].value.size <= 80)))
      assert(refs(Json.toJson(layer)).map(ref => (ref \ "line_id").as[String]).diff(roots).size <= 5))
    val shown = detail.flatMap(unit => (unit \ "en").as[List[JsValue]]).flatMap(_.asOpt[String]).mkString
    assert(shown.contains("queen on b4") && shown.contains("current geometric support"))
    val allCovered = (core ++ detail).flatMap(unit => (unit \ "covered_moves").as[List[JsObject]])
    def priceTuple(value: JsObject) =
      val path = paths.path(packet, (value \ "line_id").as[String]).get
      val offset = path.ids.indexOf((value \ "occurrence_id").as[String])
      val event = path.certified.flatMap(_._2.material).get.events.find(_.plyOffset == offset).get
      (path.ids(offset), path.positions(offset), path.positions(offset + 1), event)
    val oldPrices = (expected \ "original_core_covered").as[List[JsObject]] ++
      (expected \ "original_detail_covered").as[List[JsObject]]
    assert(oldPrices.map(priceTuple).forall(allCovered.map(priceTuple).contains),
      "every old physical transaction and its certified price survives exact-prefix detail normalization")
    val latePrices = allCovered.flatMap { value =>
      val path = paths.path(packet, (value \ "line_id").as[String]).get
      val offset = path.ids.indexOf((value \ "occurrence_id").as[String])
      Option.when(path.moves.take(19) == recovered && Set(17, 18)(offset))(offset)
    }
    assertEquals(latePrices.sorted, List(17, 18))

  test("a complete factual detail cannot borrow unused core witness capacity or evict five old owned detail witnesses"):
    val oldMoves = List("e8f6", "e8c7", "e8d6", "d7f6", "d7b8").map(reply => List("d2b1", reply))
    val (packet, paths) = admitted(source :: oldMoves, conditional = List(captured, recovered))
    val pair = List(List("d2c4"), List("d2b1")).map(path(packet, paths, _))
    val prior = RootQuestionSummary.completePairAnswer(packet, paths, pair, "recorded_scope", Nil).get
      .copy(details = oldMoves.map(moves => Section(List(RecordedContext(List(
        RootQuestionSummary.MoveRef(path(packet, paths, moves), 1)))))))
    assert(RootQuestionSummary.realize(paths, pair, prior).nonEmpty)
    assertEquals(prior.details.flatMap(_.references).map(_.path).distinct.size, 5)
    val task = FunctionalChoiceReading.Task(resource, Nil, Nil, Set.empty, Set.empty)
    val result = FunctionalRealizationReading.withDetail(packet, paths, pair, List(task), prior)
    assert(result eq prior, "a whole optional fact that cannot fit leaves the exact prior answer intact")
    assertEquals(result.core, prior.core)
    assertEquals(result.details, prior.details)
    assert(RootQuestionSummary.realize(paths, pair, result).nonEmpty)

  test("repeated projection of the same current certified later fact never duplicates its price or section"):
    val (packet, paths) = admitted(List(source), conditional = List(captured, recovered))
    val pair = List(List("d2c4"), List("d2b1")).map(path(packet, paths, _))
    val prior = RootQuestionSummary.completePairAnswer(packet, paths, pair, "recorded_scope", Nil).get
    val task = FunctionalChoiceReading.Task(resource, Nil, Nil, Set.empty, Set.empty)
    val first = FunctionalRealizationReading.withDetail(packet, paths, pair, List(task), prior)
    assert(first.details.flatMap(_.statements).exists(_.isInstanceOf[FunctionRealization]))
    val repeated = FunctionalRealizationReading.withDetail(packet, paths, pair, List(task), first)
    assert(repeated eq first)
    assertEquals(repeated.details.flatMap(_.covered).map(physical), first.details.flatMap(_.covered).map(physical))
    assert(RootQuestionSummary.realize(paths, pair, repeated).nonEmpty)

  test("an original pawn promoted to a queen cannot certify preservation of the original user's role"):
    val promotedFen = "7k/4P3/8/1Q6/8/8/8/K3R3 w - - 0 1"
    val moves = List("e7e8q", "h8h7", "b5b4", "h7h6", "e8e6", "h6g5", "e6e8")
    val history = CanonicalPositionHistory.from(promotedFen, Nil, promotedFen).toOption.get
    val supplied = List(moves, List("e7e8q"), List("e7e8r")).map(
      SuppliedCommentaryLine(CommentaryLineSource.Study, _))
    val input = MoveReviewInputAdmission.admitIndependent(history, "e7e8q", suppliedLines = supplied).get
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(input)).get
    val paths = ObservedLineProtocol.prepare(packet)
    val line = paths.certifiedPath(packet, moves).get
    val pawn = OriginalPiece("white", "pawn", "e7")
    val formerProvider = OriginalPiece("white", "queen", "b5")
    val resource = FunctionalReplyReading.Resource(
      FunctionalReplyReading.RootOwner("synthetic-promoted-user", CompareMoves("promoted_function_root", Nil, "e7e8q", "e7e8r")),
      Predicate("control", Some(formerProvider), square = Some("e8")), pawn, "e8", "e7e8q",
      List("e7e8q"), "synthetic-promoted-function-context", "present", "absent", Nil)
    val before = paths.trackedPieceAfter(line, pawn.witness, 6).flatten.get
    val arrived = paths.trackedPieceAfter(line, pawn.witness, 7).flatten.get
    assertEquals(before.role.name, "queen")
    assertEquals(arrived.role.name, "queen")
    assertEquals(arrived.square.key, "e8")
    assertEquals(pawn.witness.role.name, "pawn")
    val current = paths.positionView(line, 7).get
    assert(current.legalRows.nonEmpty)
    assertEquals(new MoveWorkProjection.Position(current).controllers("e8", pawn.witness.side)
      .map(piece => piece.role.name -> piece.square.key), List("rook" -> "e1"))
    assert(FunctionalRealizationReading.stage(packet, paths, resource, line, 6).isEmpty,
      "the legally returning queen is the same original identity, but its pawn role has not survived")
