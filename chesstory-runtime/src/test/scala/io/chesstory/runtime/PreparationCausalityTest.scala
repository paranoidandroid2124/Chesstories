package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.{MoveReviewAssembler, MoveReviewInputAdmission, OccurrenceResources}
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.CanonicalPositionHistory

/** Small legal positions and synthetic owned receipts; no engine-quality claims. */
class PreparationCausalityTest extends munit.FunSuite:
  import CommentaryFact.*
  import CommentaryProposal.*

  private val source = Source("author", "inline-observed-plan")
  private val profile = CommentaryEngineProfile.ServerAdmitted

  private def discover(fen: String, plan: List[String], additionalAnchors: List[CommentaryDiscovery.Anchor] = Nil,
      pvGuided: Boolean = false, previous: List[Entry] = Nil): (EvidenceBackedJudgmentPacket, List[Entry]) =
    val history = CanonicalPositionHistory.from(fen, Nil, fen).toOption.get
    val input = MoveReviewInputAdmission.admitIndependent(history, plan.head,
      suppliedLines = List(SuppliedCommentaryLine(CommentaryLineSource.Study, plan))).get
    assertEquals(input.suppliedLines.map(_.acceptedMoves), List(plan))
    val original = EvidenceBackedJudgmentPacket.fromAssembly(MoveReviewAssembler.assembleOccurrences(input)).get
    val prepared = ObservedLineProtocol.prepare(original)
    val anchors = CommentaryDiscovery.Anchor(Nil, plan.head) :: additionalAnchors
    val sources = List(CommentaryDiscovery.SourcePath(plan, source))
    var probes = List.empty[OccurrenceResources.ProbePath]
    var attempted = Set.empty[List[String]]
    def scan() = CommentaryDiscovery.discover(original, prepared, anchors, sources, previous, probes, pvGuided)
    var batch = scan()
    var pending = batch.replayNeeds.filterNot(attempted)
    while pending.nonEmpty do
      attempted ++= pending
      val histories = pending.flatMap(moves => history.extend(moves).toOption)
      probes = OccurrenceResources.probePaths(original, histories, probes)
      batch = scan()
      pending = batch.replayNeeds.filterNot(attempted)
    val relevant = (batch.additions ++ previous).distinctBy(_.request.id).filter(entry => Set("C-action-enablement", "C-preparation-effect",
      "C-preparation-alternative", "C-plan-counter")(entry.request.origin.question.producer))
    val inspection = CommentaryFactReader.inspect(original, prepared, relevant, probes)
    val prefixes = inspection.values.toList.flatMap(_.values).flatMap(_.promotionNeeds).filter(_.nonEmpty).distinct
    val byMoves = OccurrenceResources.probePaths(original,
      prefixes.map(moves => history.extend(moves).toOption.get), probes).map(path => path.moves -> path).toMap
    val selected = prefixes.map(byMoves)
    val retained = MoveReviewInputAdmission.retainProbePaths(input, selected).get
    val packet = EvidenceBackedJudgmentPacket.fromAssembly(
      MoveReviewAssembler.assembleOccurrences(retained, Some(original.assembly))).get
    packet -> CommentaryFactReader.read(packet, ObservedLineProtocol.prepare(packet), relevant)

  private def facts(entry: Entry, predicate: Predicate, moves: List[String], ply: Int): List[Fact] =
    entry.request.requirements.collect {
      case fact: Fact if fact.predicate == predicate && fact.path.source == Declared(moves) &&
          fact.at == Ply(ply) => fact
    }

  private def values(entry: Entry, requirement: Requirement): Set[String] =
    entry.readings.find(_.requirementId == requirement.id).toList
      .flatMap(_.result.toList.flatMap(_.samples.flatMap(_.value))).toSet

  private def reading(entry: Entry, requirement: Requirement): Reading =
    entry.readings.find(_.requirementId == requirement.id).get

  private def difference(entry: Entry, predicate: Predicate, leftMoves: List[String], leftPly: Int,
      rightMoves: List[String], rightPly: Int): FactComparison =
    val left = facts(entry, predicate, leftMoves, leftPly).map(_.id).toSet
    val right = facts(entry, predicate, rightMoves, rightPly).map(_.id).toSet
    entry.request.requirements.collectFirst {
      case comparison: FactComparison if comparison.relation == "different" &&
          left(comparison.leftRequirementId) && right(comparison.rightRequirementId) => comparison
    }.getOrElse(fail(s"missing transition-owned difference for $predicate at $leftMoves/$leftPly -> $rightMoves/$rightPly"))

  private def observedValue(packet: EvidenceBackedJudgmentPacket, predicate: Predicate,
      moves: List[String], ply: Int): Set[String] =
    val fact = Fact("inline_value", Path("inline_path", Declared(moves)), predicate, Ply(ply))
    val entry = entries(List(Request("inline_observation", Origin(source, source, source, source), List(fact)))).head
    values(CommentaryFactReader.read(packet, ObservedLineProtocol.prepare(packet), List(entry)).head, fact)

  private def counter(entries: List[Entry], prefix: List[String], observed: String, alternative: String): Entry =
    entries.find(entry => entry.request.origin.question.producer == "C-plan-counter" &&
      entry.request.requirements.exists {
        case comparison: CompareMoves => comparison.prefixMovesUci == prefix &&
          comparison.referenceMove == observed && comparison.alternativeMove == alternative
        case _ => false
      }).getOrElse(fail(s"missing quiet counter $alternative against $observed at $prefix"))

  private def strength(entry: Entry): CompareMoves =
    entry.request.requirements.collectFirst { case comparison: CompareMoves => comparison }.get

  private def supportedContext(entry: Entry): Unit =
    val groups = entry.request.requirements.collect { case group: FactGroup => group }
    assert(groups.nonEmpty)
    groups.foreach(group => assertEquals(reading(entry, group).state, "supported"))
    val comparison = strength(entry)
    assertEquals(comparison.expectedRelation, Some("better"))
    assertEquals(reading(entry, comparison).answer, None)
    assert(!isResolved(reading(entry, comparison)), "a changed condition does not establish a stronger reply")

  private def answer(packet: EvidenceBackedJudgmentPacket, comparison: CompareMoves,
      alternativeCp: Int): CommentaryInvestigation.Answer =
    val prepared = ObservedLineProtocol.prepare(packet)
    val prefix = comparison.prefixMovesUci
    assertEquals(prefix.size, 1)
    val history = packet.assembly.input.positionHistory.extend(prefix).toOption.get
    val rows = List(comparison.referenceMove -> 0, comparison.alternativeMove -> alternativeCp).map { (move, cp) =>
      val path = prepared.paths.filter(_.moves.startsWith(prefix :+ move)).sortBy(path => (path.moves.size, path.line.ref.id))
        .flatMap(path => prepared.certifiedPath(packet, path.moves)).headOption
        .getOrElse(fail(s"no complete admitted receipt path for $prefix / $move"))
      path -> cp
    }
    val path = rows.head._1
    val work = IssuedEngineWork.create(profile, s"quiet-strength-${alternativeCp + 100}",
      EngineWorkPurpose.ResponseComparison, history.initialFen, history.movePrefixUci, history.currentFen,
      EngineRootRestriction.Restricted(List(comparison.referenceMove, comparison.alternativeMove)),
      history.currentFen, Nil, EngineSearchLimits(16, 2_000_000, 2_500, 2), 3_500,
      Some(EngineSearchAnchor(prefix.head, path.ids.head, path.positions(1).id.get)))
    // Search receipts bind complete admitted paths; a merely observed prefix is not such a path.
    val reported = rows.map { (path, cp) =>
      ReportedEngineLineSuffix(path.moves.drop(prefix.size), ReportedWhiteEngineScore.Centipawns(cp), work.requiredDepth)
    }
    val physical = EngineLineAdmission.admitReportedLineSuffixes(work, reported, Some(history)).get
    val receipt = CommentaryInvestigation.EngineCandidates(work,
      EngineLineAdmission.bindAdmittedLineSuffixes(work, physical).get)
    assert(CommentaryInvestigation.ownsSearch(history, work, receipt.lines), "the receipt must own this exact child history")
    assert(CommentaryInvestigation.candidatesReady(work, receipt.lines), "both returned scores must meet this work's depth")
    val search = TargetResponseInvestigation.readSearch(packet, receipt, history, prepared)
      .getOrElse(fail(s"readSearch rejected owned receipt paths ${rows.map(_._1.moves)} at $prefix"))
    search.pairAnswer(packet, comparison.referenceMove, comparison.alternativeMove)
      .getOrElse(fail(s"pairAnswer rejected ${comparison.referenceMove}/${comparison.alternativeMove} in ${work.workId}"))

  test("becoming legal keeps the king preparation and the opponent's removal of the pin on their own transitions"):
    val fen = "k7/8/8/8/8/8/r3P2K/8 w - - 0 1"
    val pawn = Piece("white", "pawn", "e2")
    val king = Piece("white", "king", "h2")
    val rook = Piece("black", "rook", "a2")
    val action = Predicate("legal_move", Some(pawn), square = Some("e3"))
    val passage = Predicate("pawn_advance", Some(pawn), square = Some("e3"))
    val location = Predicate("location", Some(king))
    val control = Predicate("control", Some(rook), square = Some("e3"))
    List(List("h2h1", "a8a7", "e2e3"), List("h2g2", "a2a3", "e2e3")).foreach { plan =>
      val (packet, found) = discover(fen, plan)
      val entry = found.find(entry => entry.request.origin.question.producer == "C-action-enablement" &&
        facts(entry, action, plan, 2).nonEmpty).getOrElse(fail(s"missing observed enablement on $plan"))
      assertEquals(values(entry, facts(entry, action, Nil, 0).head), Set("illegal"))
      assertEquals(values(entry, facts(entry, action, plan, 2).head), Set("legal"))
      assertEquals(observedValue(packet, action, plan, 1), Set("not_turn"))
      assertEquals(observedValue(packet, passage, plan, 0), Set("present"))
      assertEquals(observedValue(packet, passage, plan, 2), Set("present"))
      val kingEffect = difference(entry, location, plan.init, 0, plan.init, 1)
      assertEquals(reading(entry, kingEffect).state, "supported")
      assertEquals(values(entry, facts(entry, location, plan.init, 0).head), Set("h2"))
      assertEquals(values(entry, facts(entry, location, plan.init, 1).head), Set(plan.head.drop(2)))
      assert(!entry.request.requirements.exists {
        case fact: Fact => fact.predicate == passage
        case _ => false
      }, "the already open pawn passage must not be presented as a preparation effect")
      if plan(1) == "a2a3" then
        val rookEffect = difference(entry, control, plan.init, 1, plan.init, 2)
        assertEquals(reading(entry, rookEffect).state, "supported")
        assertEquals(values(entry, facts(entry, control, plan.init, 1).head), Set("absent"))
        assertEquals(values(entry, facts(entry, control, plan.init, 2).head), Set("present"))
        assert(facts(entry, control, plan.init, 0).isEmpty,
          "the opponent's rook transition must not be credited to the first king move")
        val stillPinned = counter(found, List("h2g2"), "a2a3", "a8a7")
        assertEquals(values(stillPinned, facts(stillPinned, action, List("h2g2", "a8a7"), 2).head), Set("illegal"))
      else assert(!entry.request.requirements.exists {
        case fact: Fact => fact.predicate == control
        case _ => false
      })
    }

  test("a quiet target escape changes the original capture although the same coordinate move stays legal"):
    val fen = "7k/8/8/8/8/n7/8/R6K w - - 0 1"
    val plan = List("h1h2", "h8g8", "a1a3")
    val alternative = List("h1h2", "a3b5")
    val (packet, found) = discover(fen, plan)
    val rook = Piece("white", "rook", "a1")
    val target = Piece("black", "knight", "a3")
    val capture = Predicate("legal_capture", Some(rook), Some(target))
    val location = Predicate("location", Some(target))
    val entry = counter(found, List("h1h2"), "h8g8", "a3b5")
    List(capture -> ("legal", "illegal"), location -> ("a3", "b5")).foreach { (predicate, expected) =>
      val changed = difference(entry, predicate, plan.init, 2, alternative, 2)
      assertEquals(reading(entry, changed).state, "supported")
      assertEquals(values(entry, facts(entry, predicate, plan.init, 2).head), Set(expected._1))
      assertEquals(values(entry, facts(entry, predicate, alternative, 2).head), Set(expected._2))
    }
    assertEquals(observedValue(packet, Predicate("legal_move", Some(rook), square = Some("a3")), alternative, 2), Set("legal"))
    supportedContext(entry)
    assertEquals(strength(entry).prefixMovesUci, List("h1h2"))
    List(-100 -> ("better", "supported"), 0 -> ("equal", "refuted"), 100 -> ("worse", "refuted")).foreach {
      case (cp, (relation, state)) =>
        val compared = pairReading(strength(entry), answer(packet, strength(entry), cp))
        assertEquals(compared.answer.map(_.evaluationRelation), Some(relation))
        assertEquals(compared.state, state)
        val updated = entry.copy(readings = entry.readings.map(value =>
          if value.requirementId == compared.requirementId then compared else value))
        val reread = CommentaryFactReader.read(packet, ObservedLineProtocol.prepare(packet), List(updated)).head
        assertEquals(reading(reread, strength(entry)), compared)
        entry.request.requirements.collect { case group: FactGroup => group }.foreach { group =>
          assertEquals(reading(reread, group).state, "supported",
            "a weak or equal counter still changes the named capture condition")
        }
        assertEquals(neededRequirementIds(reread), Set.empty[String])
    }

  test("a quiet interposition blocks the observed rook route without certifying the reply's strength"):
    val fen = "7k/8/8/8/8/8/2n5/R6K w - - 0 1"
    val plan = List("h1h2", "h8g8", "a1a8")
    val alternative = List("h1h2", "c2a3")
    val (packet, found) = discover(fen, plan)
    val action = Predicate("legal_move", Some(Piece("white", "rook", "a1")), square = Some("a8"))
    val entry = counter(found, List("h1h2"), "h8g8", "c2a3")
    val changed = difference(entry, action, plan.init, 2, alternative, 2)
    assertEquals(reading(entry, changed).state, "supported")
    assertEquals(values(entry, facts(entry, action, plan.init, 2).head), Set("legal"))
    assertEquals(values(entry, facts(entry, action, alternative, 2).head), Set("illegal"))
    assertEquals(observedValue(packet, Predicate("location", Some(Piece("black", "knight", "c2"))), alternative, 2), Set("a3"))
    supportedContext(entry)
    val preparation = found.find(entry => entry.request.origin.question.producer == "C-preparation-effect" &&
      facts(entry, action, plan, 2).nonEmpty).get
    assert(preparation.request.requirements.exists {
      case comparison: CompareMoves => comparison.prefixMovesUci == Nil &&
        comparison.referenceMove == "h1h2" && comparison.alternativeMove == "a1a8"
      case _ => false
    }, "immediate action is compared at the original position, independently from the opponent's later reply")

  test("a quiet defender changes the capture condition while the original target and legal capture remain"):
    val fen = "7k/2n5/8/8/8/p7/8/R6K w - - 0 1"
    val plan = List("h1h2", "h8g8", "a1a3")
    val alternative = List("h1h2", "c7b5")
    val (packet, found) = discover(fen, plan)
    val target = Piece("black", "pawn", "a3")
    val capture = Predicate("legal_capture", Some(Piece("white", "rook", "a1")), Some(target))
    val control = Predicate("control", Some(Piece("black", "knight", "c7")), square = Some("a3"))
    val entry = counter(found, List("h1h2"), "h8g8", "c7b5")
    val changed = difference(entry, control, plan.init, 2, alternative, 2)
    assertEquals(reading(entry, changed).state, "supported")
    assertEquals(values(entry, facts(entry, control, plan.init, 2).head), Set("absent"))
    assertEquals(values(entry, facts(entry, control, alternative, 2).head), Set("present"))
    List(plan.init, alternative).foreach { moves =>
      assertEquals(observedValue(packet, capture, moves, 2), Set("legal"))
      assertEquals(observedValue(packet, Predicate("location", Some(target)), moves, 2), Set("a3"))
    }
    supportedContext(entry)

  test("the same later pawn action retains distinct root and child preparation questions"):
    val plan = List("h2h3", "b8c6", "d2d4", "d7d5", "e2e4")
    val childPrefix = plan.take(2)
    val anchors = List(CommentaryDiscovery.Anchor(childPrefix, "d2d4"))
    val (_, first) = discover(chess.variant.Standard.initialFen.value, plan,
      additionalAnchors = anchors, pvGuided = true)
    // The root first asks about d4, while e4 is already the child's next action.
    // Resolve that earlier scheduling frontier before testing the two e4 owners.
    val previous = first.map(entry => if entry.generatedArms.exists(_.prefixMoves.isEmpty) then
      entry.copy(readings = entry.readings.map(_.copy(state = "observed", stopReason = None, result = None)))
      else entry)
    val (_, found) = discover(chess.variant.Standard.initialFen.value, plan,
      additionalAnchors = anchors, pvGuided = true, previous = previous)
    val action = Predicate("legal_move", Some(Piece("white", "pawn", "e2")), square = Some("e4"))
    val questions = found.filter(entry => entry.request.origin.question.producer == "C-preparation-effect" &&
      facts(entry, action, plan, 4).nonEmpty)
    def question(prefix: List[String], preparation: String): Entry =
      questions.find(_.generatedArms.exists(arm => arm.prefixMoves == prefix && arm.referenceMove == preparation))
        .getOrElse(fail(s"missing separately owned e4 question after $prefix / $preparation"))
    val root = question(Nil, "h2h3")
    val child = question(childPrefix, "d2d4")
    assertNotEquals(root.request.id, child.request.id)
    assert(root.generatedArms.map(_.id).toSet.intersect(child.generatedArms.map(_.id).toSet).isEmpty)
    List((root, List.empty[String], "h2h3"), (child, childPrefix, "d2d4")).foreach { (entry, prefix, preparation) =>
      assertEquals(values(entry, facts(entry, action, prefix, prefix.size).head), Set("legal"))
      assertEquals(values(entry, facts(entry, action, plan, 4).head), Set("legal"))
      val comparisons = entry.request.requirements.collect { case comparison: CompareMoves => comparison }
      assertEquals(comparisons.map(value => (value.prefixMovesUci, value.referenceMove, value.alternativeMove)),
        List((prefix, preparation, "e2e4")))
      assert(entry.generatedArms.forall(arm => arm.prefixMoves == prefix && arm.referenceMove == preparation),
        "one observed continuation must not merge the different starting conditions")
    }
    assert(facts(root, action, childPrefix, 2).isEmpty)
    assert(facts(child, action, Nil, 0).isEmpty)

  test("an unsupplied preparation by another original knight preserves the same pawn action and reply"):
    val fen = "7k/8/8/8/3P4/8/8/1N3N1K w - - 0 1"
    val plan = List("b1c3", "h8g8", "d4d5")
    val alternative = List("f1e3", "h8g8")
    val (packet, found) = discover(fen, plan)
    val action = Predicate("legal_move", Some(Piece("white", "pawn", "d4")), square = Some("d5"))
    val entry = found.find(entry => entry.request.origin.question.producer == "C-preparation-alternative" &&
      facts(entry, action, plan, 2).nonEmpty && facts(entry, action, alternative, 2).nonEmpty)
      .getOrElse(fail("the other original knight must be discovered without supplying its plan"))
    assertEquals(packet.assembly.input.suppliedLines.map(_.acceptedMoves), List(plan))
    assert(packet.assembly.input.legalLines.exists(line => line.moves == alternative && line.discoveryOnly),
      "the exact shared opponent reply must come from a requested replay, not an independent supplied plan")
    val actions = entry.request.requirements.collect { case fact: Fact if fact.predicate.kind == "legal_move" => fact }
    assertEquals(actions.size, 2)
    actions.foreach { fact =>
      assertEquals(fact.predicate, action, "changing the preparer must not replace the original goal actor")
      assertEquals(values(entry, fact), Set("legal"))
    }
    val comparison = strength(entry)
    assertEquals((comparison.prefixMovesUci, comparison.referenceMove, comparison.alternativeMove),
      (Nil, "b1c3", "f1e3"))
    assertEquals(reading(entry, comparison).answer, None)
    entry.request.requirements.collect { case group: FactGroup => group }.foreach { group =>
      assertEquals(reading(entry, group).state, "supported")
    }
    List((Piece("white", "knight", "b1"), plan.init),
      (Piece("white", "knight", "f1"), alternative)).foreach { (knight, moves) =>
      val control = Predicate("control", Some(knight), square = Some("d5"))
      assertEquals(observedValue(packet, control, moves, 0), Set("absent"))
      assertEquals(observedValue(packet, control, moves, 1), Set("present"))
    }
