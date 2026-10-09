package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.{MoveReviewAssembler, MoveReviewInputAdmission, OccurrenceResources, RawMoveReviewInput}
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.EngineLine

/** Composed legal boards isolate one conditional function of root-owned
  * control. Scores are synthetic admission input, not evidence of best play,
  * free material, compensation, or the reason for a root engine evaluation.
  */
class FunctionalQuietRecaptureTest extends munit.FunSuite:
  import CommentaryFact.*
  import CommentaryProposal.*
  import CommentaryAnswer.*
  import ObservedLineProtocol.{PreparedPath, PreparedPaths}

  private def moves(value: String): List[String] = value.split(" ").toList
  private val fen = "6k1/p5b1/8/8/8/5N2/8/R6K w - - 0 1"
  private val reference = moves("a1d1 a7a6 f3d4 g7d4")
  private val alternative = moves("a1b1 a7a6 f3d4 g7d4")
  private val option = reference :+ "d1d4"
  private val provider = Piece("white", "rook", "a1")
  private val friendly = Piece("white", "knight", "f3")
  private val enemy = Piece("black", "bishop", "g7")
  private val control = Predicate("control", Some(provider), square = Some("d4"))
  private val capture = Predicate("legal_capture", Some(provider), Some(enemy))

  private case class Fixture(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      pair: List[PreparedPath], proof: FunctionalReplyReading.Proof, previous: Answer):
    def path(prefix: List[String]): PreparedPath = paths.paths.filter(_.moves.startsWith(prefix))
      .minByOption(_.moves.size).getOrElse(fail(s"missing certified conditional path $prefix"))
    def statement: LegalReplyOption = LegalReplyOption(path(proof.reference),
      path(proof.alternative), path(proof.option), proof)
    def answer: Answer = FunctionalReplyReading.withAnswer(packet, paths, pair, List(proof), previous)

  private def fixture(initialFen: String = fen,
      current: List[List[String]] = List(reference, alternative),
      left: List[String] = reference, right: List[String] = alternative,
      available: List[String] = option, discoveryOnly: Boolean = false): Fixture =
    val input = MoveReviewInputAdmission.admit(RawMoveReviewInput(initialFen, current.head.head,
      current.map(line => EngineLine(line, 0, None, 16)))).get
    val local = List(left, right, available).distinct.filterNot(current.contains).map { line =>
      val continuation = input.positionHistory.admitToFirstAutomaticTerminal(line).toOption
        .getOrElse(fail(s"illegal test path $line"))
      assertEquals(continuation.stepsAfter(input.positionHistory).get.map(_.uci), line)
      continuation
    }
    val retained = if discoveryOnly then
      val original = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(input)).get
      val probes = OccurrenceResources.probePaths(original, local.map(_.resultingHistory))
      MoveReviewInputAdmission.retainProbePaths(input, probes, discoveryOnly = true).get
    else MoveReviewInputAdmission.retainLegalContinuations(input, local)
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(retained)).get
    val paths = ObservedLineProtocol.prepare(packet)
    val pair = current.map(line => paths.certifiedPath(packet, line).get)
    val root = CompareMoves("function_root", Nil, left.head, right.head)
    val proof = FunctionalReplyReading.Proof(FunctionalReplyReading.RootOwner("synthetic-quiet-control", root),
      control, left, right, available, capture, capture, Nil)
    val previous = RootQuestionSummary.completePairAnswer(packet, paths, pair, "selected_comparison", Nil).get
    Fixture(packet, paths, pair, proof, previous)

  private def generatedEntry(value: Fixture): Entry = CommentaryDiscovery.discover(value.packet, value.paths,
    List(CommentaryDiscovery.Anchor(Nil, reference.head)),
    value.pair.map(path => CommentaryDiscovery.SourcePath(path.moves,
      Source("user", "synthetic-discovery-only-quiet-arrival"))), Nil, pvGuided = true).additions.find(entry =>
      entry.request.origin.question.producer == "C-reply-function" && entry.request.requirements.exists {
        case fact: Fact => fact.id == "function_effect_reference" && fact.path.source == Declared(reference)
        case _ => false
      } && entry.request.requirements.exists {
        case fact: Fact => fact.id == "function_option" && fact.path.source == Declared(option)
        case _ => false
      }).getOrElse(fail("the independently observed friendly arrival must generate its conditional recapture question"))

  private def generatedProof(value: Fixture, entry: Entry): FunctionalReplyReading.Proof =
    FunctionalReplyReading.read(CommentaryFactReader.read(value.packet, value.paths, List(entry))).find(proof =>
      proof.reference == reference && proof.alternative == alternative && proof.option == option)
      .getOrElse(fail("the generated root-owned condition and declared recapture facts must be supported"))

  private def english(value: Fixture, statement: LegalReplyOption): String =
    CommentaryEnglish.statement(statement).map {
      case text: String => text
      case move: RootQuestionSummary.MoveRef => value.paths.notation.line(move.path.moves)(move.offset)
    }.mkString

  test("resource task retains its original actor independently of the old capture realization"):
    val value = fixture()
    val read = CommentaryFactReader.read(value.packet, value.paths, List(generatedEntry(value)))
    val task = FunctionalReplyReading.resources(read).headOption.getOrElse(fail("missing quiet resource task"))
    assertEquals(task.actor, friendly)
    assertEquals(task.square, "d4")
    assertEquals(task.move, "f3d4")
    assertEquals(task.witnessedPrefix, reference.take(2))
    val changed = read.map(entry => entry.copy(readings = entry.readings.map(reading =>
      if Set("function_effect_reference", "function_effect_alternative", "function_option", "function_context")
          (reading.requirementId) then reading.copy(state = "refuted", answer = None)
      else reading)))
    assertEquals(FunctionalReplyReading.resources(changed), List(task),
      "a replaced downstream capture is not a falsifier of the original resource's value question")
    assertEquals(FunctionalReplyReading.read(changed), Nil)

  test("a current longer line owns the same legal recapture proof without inheriting a numerical receipt"):
    val longerPeer = alternative :+ "h1h2"
    val value = fixture(current = List(option, longerPeer))
    val original = value.statement
    val selected = List(value.path(option), value.path(longerPeer))
    val rebased = FunctionalReplyReading.rebase(value.paths, value.pair, original, selected).get
    assert(rebased.reference eq selected.head)
    assert(rebased.available eq selected.head)
    assert(rebased.alternative eq selected(1))
    assertEquals(rebased.proof, original.proof)
    assertEquals(rebased.proof.owner.answer, original.proof.owner.answer)
    assert(FunctionalReplyReading.valid(value.paths, rebased))
    assertEquals(rebased.actionReferences, Nil,
      "the conditional option does not select the current line's entire later price account")

  test("a replaced mechanical proof requires its whole canonical prefix and every represented arm"):
    val longerPeer = alternative :+ "h1h2"
    val changed = moves("a1d1 a7a5 f3d4 g7d4 d1d4")
    val value = fixture(current = List(option, longerPeer), available = option)
    assertEquals(FunctionalReplyReading.rebase(value.paths, value.pair, value.statement,
      List(value.path(option))), None)
    val other = fixture(current = List(changed, longerPeer), left = changed.take(4), available = changed)
    assertEquals(FunctionalReplyReading.rebase(value.paths, value.pair, value.statement,
      List(other.path(changed), value.path(longerPeer))), None,
      "a different reply and a foreign prepared owner cannot replace a matching move name")

  test("resource admission requires its exact independent root task arm"):
    val value = fixture()
    val read = CommentaryFactReader.read(value.packet, value.paths, List(generatedEntry(value)))
    assert(FunctionalReplyReading.resources(read).nonEmpty)
    val missing = read.map(entry => entry.copy(generatedArms = entry.generatedArms.filterNot(
      _.headRequirementIds == List("function_task_context"))))
    assertEquals(FunctionalReplyReading.resources(missing), Nil)
    val wrong = read.map(entry => entry.copy(generatedArms = entry.generatedArms.map(arm =>
      if arm.headRequirementIds == List("function_task_context") then arm.copy(referenceMove = alternative.head)
      else arm)))
    assertEquals(FunctionalReplyReading.resources(wrong), Nil)

  test("a capturing arrival is the same original resource with a freshly priced transaction"):
    val value = fixture(initialFen = "6k1/p5b1/8/8/3p4/5N2/8/R6K w - - 0 1")
    val entry = generatedEntry(value)
    val read = CommentaryFactReader.read(value.packet, value.paths, List(entry))
    val task = FunctionalReplyReading.resources(read).head
    assertEquals(task.actor, friendly)
    assertEquals(task.move, "f3d4")
    assertEquals(task.square, "d4")
    val current = FunctionalChoiceReading.originalRow(value.paths, value.path(reference), 2, task.actor, task.square).get
    assert(current.capture.nonEmpty)
    val material = value.path(reference).certified.flatMap(_._2.material).get
    assert(material.events.exists(event => event.plyOffset == 2 && event.capture.nonEmpty),
      "the initial pawn capture is a new price, not the old quiet-arrival transaction")
    assert(entry.request.requirements.exists {
      case fact: Fact => fact.id == "function_resource" && fact.predicate.kind == "legal_move" &&
        fact.predicate.subject.contains(friendly) && fact.predicate.square.contains("d4")
      case _ => false
    })

  test("preserving an existing function and creating a new one share the same resource contract"):
    val left = moves("h1g1 a7a6 f3d4 g7d4")
    val right = moves("d1b1 a7a6 f3d4 g7d4")
    val value = fixture(initialFen = "6k1/p5b1/8/8/8/5N2/8/3R3K w - - 0 1",
      current = List(left, right), left = left, right = right, available = left :+ "d1d4")
    val additions = CommentaryDiscovery.discover(value.packet, value.paths,
      List(CommentaryDiscovery.Anchor(Nil, left.head)),
      List(left, right).map(line => CommentaryDiscovery.SourcePath(line, Source("user", "preserved-function"))),
      Nil, pvGuided = true).additions
    val entries = CommentaryFactReader.read(value.packet, value.paths, additions)
    val tasks = FunctionalReplyReading.resources(entries)
    val task = tasks.find(task => task.actor == friendly && task.square == "d4").getOrElse(
      fail("the quiet king move retains the original rook's already-present d4 support"))
    assertEquals(task.condition.subject, Some(Piece("white", "rook", "d1")))
    val owner = entries.find(_.request.id == task.owner.proposalId).get
    assert(owner.request.requirements.contains(FactComparison("function_owned", "function_before",
      "function_root_reference", "same")))
    assertEquals(FunctionalReplyReading.conditionAt(value.paths, value.path(left), 0, task.condition), Some("present"))
    assertEquals(FunctionalReplyReading.conditionAt(value.paths, value.path(left), 1, task.condition), Some("present"))
    assertEquals(FunctionalReplyReading.conditionAt(value.paths, value.path(right), 1, task.condition), Some("absent"))

  test("a task cannot borrow its before condition from another root or a price premise"):
    val value = fixture()
    val read = CommentaryFactReader.read(value.packet, value.paths, List(generatedEntry(value)))
    val wrong = read.map(entry => entry.copy(request = entry.request.copy(requirements =
      entry.request.requirements.map {
        case fact: Fact if fact.id == "function_before" => fact.copy(path = Path("foreign", Declared(alternative)))
        case other => other
      })))
    assertEquals(FunctionalReplyReading.resources(wrong), Nil)
    val dependent = read.map(entry => entry.copy(request = entry.request.copy(requirements =
      entry.request.requirements.map {
        case group: FactGroup if group.id == "function_task_context" => group.copy(members = group.members :+ "function_option")
        case other => other
      })))
    assertEquals(FunctionalReplyReading.resources(dependent), Nil)

  test("current resource legality follows the original actor rather than a destination or old UCI"):
    val value = fixture(initialFen = "6k1/p5b1/8/8/8/1N3N2/8/R6K w - - 0 1")
    val path = value.path(reference)
    val rows = value.paths.positionView(path, 2).flatMap(_.legalRows).get
    assert(rows.exists(_.move == "b3d4") && rows.exists(_.move == "f3d4"))
    assertEquals(FunctionalChoiceReading.originalRow(value.paths, path, 2, friendly, "d4")
      .map(_.move), Some("f3d4"))
    assertEquals(FunctionalChoiceReading.originalRow(value.paths, path, 4, friendly, "d4"), None,
      "another surviving knight does not replace the captured original resource")
    assertEquals(FunctionalChoiceReading.originalRow(value.paths, path, 2, provider, "d4")
      .map(_.move), Some("d1d4"), "a relocated original actor supplies its current legal UCI")

  test("root control supports a conditional quiet recapture of the same enemy after it takes the same friendly arrival"):
    val value = fixture()
    val statement = value.statement
    List(statement.reference, statement.alternative).foreach { path =>
      val victim = value.paths.trackedPieceAfter(path, friendly.witness, 3).flatten.get
      val taker = value.paths.trackedPieceAfter(path, enemy.witness, 3).flatten.get
      assertEquals(victim.square.key, "d4")
      assert(value.paths.positionView(path, 3).flatMap(_.legalRows)
        .exists(_.exists(row => row.move == "g7d4" && row.actor == taker && row.capture.contains(victim))))
      assertEquals(value.paths.trackedPieceAfter(path, friendly.witness, 4), Some(None))
      assert(value.paths.positionView(path, 4).exists(_.currentCheckSources(chess.White).isEmpty),
        "this is a quiet Bxd4 capture, not the earlier checking-arrival case")
    }
    assert(FunctionalReplyReading.valid(value.paths, statement))
    assertEquals(statement.actionReferences, Nil)
    assertEquals(statement.covered, Nil,
      "the conditional realization cannot own the captures in the actual root ledger")
    assertEquals(value.answer.question, "functional_response")
    val text = english(value, statement)
    assert(text.contains("Rd1") && text.contains("Nd4") && text.contains("Bxd4") && text.contains("Rxd4"), text)
    assert(text.toLowerCase.contains("recapture") && !text.toLowerCase.contains("check"), text)
    List("forced", "must", "best", "winning", "safe", "free", "compensates", "evaluates").foreach { word =>
      assert(!text.toLowerCase.contains(word), text)
    }
    val batch = CommentaryDiscovery.discover(value.packet, value.paths,
      List(CommentaryDiscovery.Anchor(Nil, reference.head)),
      List(reference, alternative).map(line => CommentaryDiscovery.SourcePath(line,
        Source("user", "synthetic-quiet-arrival"))), Nil, pvGuided = true)
    val generated = batch.additions.filter(_.request.origin.question.producer == "C-reply-function")
      .filter(_.request.requirements.exists {
        case fact: Fact => fact.id == "function_option" && fact.path.source == Declared(option)
        case _ => false
      })
    assert(generated.nonEmpty,
      "the same functional producer must accept the witnessed friendly arrival without a SAN or check-only branch")
    val proofs = FunctionalReplyReading.read(CommentaryFactReader.read(value.packet, value.paths, generated))
    assert(proofs.exists(proof => proof.reference == reference && proof.alternative == alternative &&
      proof.condition == control && proof.effect == capture && proof.option == option))

  test("different current replies and PV identities preserve the independently reread quiet function"):
    val known = fixture()
    val current = List(moves("a1d1 g8f8"), moves("a1b1 a7a5"))
    val value = fixture(current = current)
    assert(!current.exists(_.contains("f3d4")), "the new principal paths do not repeat the local realization")
    assertNotEquals(value.pair.head.line.ref.id, known.pair.head.line.ref.id)
    assertEquals(value.proof, known.proof,
      "the conditional board question retains its root-owned meaning without a numerical receipt")
    assert(FunctionalReplyReading.valid(value.paths, value.statement.copy(proof = known.proof)))
    assertEquals(value.answer.question, "functional_response")
    val text = english(value, value.statement)
    assert(text.contains("Bxd4") && text.contains("Rxd4"), text)
    assert(!text.contains("Kf8") && !text.contains("a5"),
      "changing the current forecast cannot import its suffix into the bounded conditional example")
    val foreign = value.statement.copy(alternative = known.statement.alternative)
    assert(!FunctionalReplyReading.valid(value.paths, foreign),
      "a foreign prepared path does not become owned merely because its moves and board meaning match")

  test("another legal recapturer keeps the provider-specific fact true but cannot own the central answer"):
    val value = fixture(initialFen = "6k1/p5b1/8/8/8/4BN2/8/R6K w - - 0 1")
    assert(FunctionalReplyReading.valid(value.paths, value.statement))
    assert(value.paths.positionView(value.statement.alternative, 4).flatMap(_.legalRows)
      .exists(_.exists(_.move == "e3d4")), "the original Be3 can also recapture the same enemy after Rb1")
    assertEquals(value.answer, value.previous,
      "the missing same-rook action is not a central recapture difference when Bxd4 remains available")

  test("restored endpoint control and an unrelated quiet enemy arrival do not certify the essential recapture conditions"):
    val tail = moves("a7a6 f3d2 g7f6 d2b3 a6a5 b3d4 f6d4")
    val left = "a1d1" :: tail
    val right = "a1b1" :: tail
    val restored = fixture(left = left, right = right, available = left :+ "d1d4")
    val providerAt3 = restored.paths.trackedPieceAfter(restored.statement.reference, provider.witness, 3).flatten.get
    assert(restored.paths.positionView(restored.statement.reference, 3).exists(view =>
      !new MoveWorkProjection.Position(view).controlledBy(providerAt3)("d4")),
      "Nd2 interrupts the root-owned rook ray before Nb3 reopens it")
    assert(restored.paths.positionView(restored.statement.reference, left.size).flatMap(_.legalRows)
      .exists(_.exists(_.move == "d1d4")), "matching legal endpoints alone would misleadingly accept this proof")
    assert(!FunctionalReplyReading.valid(restored.paths, restored.statement))
    val quietLeft = moves("a1d1 a7a6 f3h4 g7d4")
    val quietRight = moves("a1b1 a7a6 f3h4 g7d4")
    val unrelated = fixture(left = quietLeft, right = quietRight, available = quietLeft :+ "d1d4")
    assert(unrelated.paths.positionView(unrelated.statement.reference, 4).flatMap(_.legalRows)
      .exists(_.exists(_.move == "d1d4")))
    assert(unrelated.paths.trackedPieceAfter(unrelated.statement.reference, friendly.witness, 4).flatten.nonEmpty,
      "Bd4 did not capture the friendly knight, which is still on h4")
    assert(!FunctionalReplyReading.valid(unrelated.paths, unrelated.statement),
      "an enemy's nonchecking noncapture is not a recapture price witness")
    val valid = fixture()
    val wrongOriginal = capture.copy(target = Some(Piece("black", "bishop", "f8")))
    assert(!FunctionalReplyReading.valid(valid.paths, valid.statement.copy(proof = valid.proof.copy(
      effect = wrongOriginal, action = wrongOriginal))), "the same role cannot replace the original captured enemy")

  test("a generated quiet proof reads its discovery-only peer without promoting an investigation seed"):
    val value = fixture(current = List(reference, moves("a1b1 a7a5")), discoveryOnly = true)
    val proof = generatedProof(value, generatedEntry(value))
    val statement = value.statement.copy(proof = proof)
    val peer = statement.alternative
    assert(value.packet.assembly.input.legalLines.exists(line => line.moves == peer.sourceMoves && line.discoveryOnly))
    val seedsBefore = value.paths.investigationPaths.map(_.line.ref.id).toSet
    val readingsBefore = value.paths.continuationReadings.map(_.path.line.ref.id).toSet
    assert(!seedsBefore(peer.line.ref.id) && !readingsBefore(peer.line.ref.id),
      "a legal declaration is not an independently observed plan or scheduling source")
    val reading = value.paths.continuationReadingFor(peer).get
    val loss = reading.events.find(_.plyOffset == alternative.size - 1).get
    assertEquals(loss.originalActor, Some(enemy.witness))
    assertEquals(loss.originalVictim, Some(friendly.witness))
    assert(FunctionalReplyReading.valid(value.paths, statement))
    assertEquals(FunctionalReplyReading.withAnswer(value.packet, value.paths, value.pair,
      List(proof), value.previous).question, "functional_response")
    assertEquals(value.paths.investigationPaths.map(_.line.ref.id).toSet, seedsBefore)
    assertEquals(value.paths.continuationReadings.map(_.path.line.ref.id).toSet, readingsBefore,
      "reading one certified material witness must not widen aggregate investigation discovery")

  test("a changed current PV rereads the declared quiet function without borrowing a numerical receipt"):
    val known = fixture(current = List(reference, moves("a1b1 a7a5")), discoveryOnly = true)
    val entry = generatedEntry(known)
    val current = List(moves("a1d1 g8f8"), moves("a1b1 a7a5"))
    val value = fixture(current = current, discoveryOnly = true)
    val proof = generatedProof(value, entry)
    val statement = value.statement.copy(proof = proof)
    assertEquals(proof.owner.answer, None)
    assertNotEquals(value.pair.head.line.ref.id, known.pair.head.line.ref.id)
    assert(!value.paths.investigationPaths.exists(_.moves.startsWith(reference)))
    assert(!value.paths.investigationPaths.exists(_.moves.startsWith(alternative)))
    assert(FunctionalReplyReading.valid(value.paths, statement))
    assertEquals(FunctionalReplyReading.withAnswer(value.packet, value.paths, value.pair,
      List(proof), value.previous).question, "functional_response")
    val foreign = statement.copy(alternative = known.statement.alternative)
    assertEquals(value.paths.continuationReadingFor(known.statement.alternative), None,
      "identical moves and board meaning do not authorize a foreign prepared path")
    assertEquals(LineContinuationInvestigation.readOwnedPath(known.packet, statement.reference, value.paths), None,
      "a foreign packet returns unknown before entering owner assertions or the material cache")
    assert(!FunctionalReplyReading.valid(value.paths, foreign),
      "foreign material cannot certify the same friendly loss")

  test("another recapturer still prevents a discovery-only provider fact from owning the central answer"):
    val value = fixture(initialFen = "6k1/p5b1/8/8/8/4BN2/8/R6K w - - 0 1",
      current = List(reference, moves("a1b1 a7a5")), discoveryOnly = true)
    val proof = generatedProof(value, generatedEntry(value))
    val statement = value.statement.copy(proof = proof)
    assert(!value.paths.investigationPaths.exists(_ eq statement.alternative))
    assert(FunctionalReplyReading.valid(value.paths, statement))
    assert(value.paths.positionView(statement.alternative, 4).flatMap(_.legalRows)
      .exists(_.exists(_.move == "e3d4")), "Be3 retains its own legal Bxd4 in the peer root context")
    assertEquals(FunctionalReplyReading.withAnswer(value.packet, value.paths, value.pair,
      List(proof), value.previous), value.previous,
      "certifying a generated material event cannot erase an independent alternative capturer")
