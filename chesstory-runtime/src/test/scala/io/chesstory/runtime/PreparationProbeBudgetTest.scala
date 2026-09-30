package io.chesstory.runtime

import lila.chessjudgment.model.line.CanonicalPositionHistory
import play.api.libs.json.*

/** Inline legal paths and synthetic receipts exercise the question budget without an engine. */
class PreparationProbeBudgetTest extends munit.FunSuite:
  import CommentaryFact.*
  import CommentaryProposal.*

  private val initial = chess.variant.Standard.initialFen.value
  private val profile = CommentaryEngineProfile.ServerAdmitted

  private def submit(state: AwaitingEngineWork, rows: List[List[String]], now: Long): CommentaryJobState =
    val work = state.issuedWork
    CommentaryJobReducer.submitIssuedWorkReport(state, EngineWorkReport.Completed(profile, work.workId,
      work.executionKeySha256, rows.zipWithIndex.map { (moves, rank) =>
        ReportedEngineLineSuffix(moves, ReportedWhiteEngineScore.Centipawns(30 - rank * 10), work.requiredDepth)
      }), now).toOption.get

  test("three long observed plans keep the first immediate-action question within the finite budgets"):
    val owner = CanonicalPositionHistory.from(initial, Nil, initial).toOption.get
    val tail = List("b8c6", "d2d4", "d7d5", "e2e4", "e7e6", "b1c3", "g8f6", "g1f3",
      "f8e7", "f1d3", "e8g8", "e1g1", "a7a6", "f1e1", "b7b5")
    val rows = List("h2h3", "h2h4", "a2a3").map(_ :: tail)
    rows.foreach { moves =>
      assertEquals(moves.size, 16)
      assert(owner.extend(moves).isRight, s"the inline observation must be legal: $moves")
    }
    val root = CommentaryJobReducer.startJob(initial, Nil, initial, "h2h3",
      owner.extend(List("h2h3")).toOption.get.currentFen,
      CommentaryJobPolicy(16, 10000L, profile, investigateChoices = true,
        maximumLocalExpansions = 600, previewDepth = Some(8)), 0L).toOption.get.asInstanceOf[AwaitingEngineWork]
    // The preview exposes the independent observations before any generated probe is retained.
    val preview = submit(root, rows, 1L).asInstanceOf[AwaitingEngineWork]
    assert(!preview.proposalEntries.exists(_.admission == "generated"))
    val packet = preview.observationsPacket.get
    val sources = rows.map(moves => CommentaryDiscovery.SourcePath(moves,
      Source("generator", "root_search", sourceRefs = List(root.issuedWork.workId))))
    val pending = CommentaryDiscovery.discover(packet, ObservedLineProtocol.prepare(packet),
      List(CommentaryDiscovery.Anchor(Nil, "h2h3")), sources, Nil, pvGuided = true).replayNeeds
    assertEquals(pending, Nil,
      "the supplied PVs are already materialized; unseen replies wait for a selected engine investigation")

    val state = submit(preview, rows, 2L)
    assertEquals(state.localExpansionsUsed, 0)
    assert(state.observationsPacket.get.assembly.input.legalLines.forall(line => rows.exists(_.startsWith(line.moves))))
    assert(state.proposalEntries.map(_.request.requirements.size).sum <= MaximumRequirements)
    assert(validRequests(state.proposalEntries.map(_.request)))
    val entry = state.proposalEntries.find(entry =>
      entry.request.origin.question.producer == "C-preparation-effect" &&
        entry.request.requirements.exists {
          case value: CompareMoves => value.prefixMovesUci.isEmpty &&
            value.referenceMove == "h2h3" && value.alternativeMove == "d2d4"
          case _ => false
        }).getOrElse(fail("the first observed d4 must retain its preparation-versus-immediate question"))
    val action = Predicate("legal_move", Some(Piece("white", "pawn", "d2")), square = Some("d4"))
    val actionFacts = List(Declared(List("h2h3", "b8c6", "d2d4")) -> Ply(2), Declared(Nil) -> Ply(0))
      .map { (path, point) =>
        entry.request.requirements.collectFirst {
          case value: Fact if value.predicate == action && value.path.source == path && value.at == point => value
        }.getOrElse(fail(s"the first question needs its exact legal-action witness at $path / $point"))
      }
    actionFacts.foreach { fact =>
      val reading = entry.readings.find(_.requirementId == fact.id).get
      assertEquals(reading.state, "supported")
      assertEquals(reading.result.get.samples.flatMap(_.value).toSet, Set("legal"))
      assert(entry.generatedArms.exists(_.memberRequirementIds.contains(fact.id)))
    }
    val comparison = entry.request.requirements.collectFirst {
      case value: CompareMoves if value.prefixMovesUci.isEmpty &&
          value.referenceMove == "h2h3" && value.alternativeMove == "d2d4" => value
    }.get
    val reading = entry.readings.find(_.requirementId == comparison.id).get
    assert(Set("deferred", "awaiting_engine")(reading.state),
      "board facts keep the strength inquiry pending; they cannot certify an engine comparison")
    assertEquals(reading.answer, None)
    assert(neededRequirementIds(entry)(comparison.id),
      "an admitted early question must retain an actionable strength inquiry after local discovery")

    def actionPrefixes(entries: List[Entry]): Set[List[String]] = entries.flatMap(_.request.requirements.collect {
      case fact: Fact if fact.id == "reference_action" => fact.path.source
    }.collect { case Declared(moves) => moves }).toSet
    assert(actionPrefixes(state.proposalEntries).forall(_.size <= 3),
      "the rest of each PV remains a source, not eagerly allocated questions and samples")
    assert(state.discoveryState.scans.exists(_.reason.contains("action_frontier")))

    // This transition tests scheduling only: it supplies resolved heads, not a chess verdict.
    val settled = state.proposalEntries.map(entry => entry.copy(readings = entry.readings.map(reading =>
      reading.copy(state = "observed", stopReason = None, result = None))))
    val observed = state.observationsPacket.get
    val advanced = CommentaryDiscovery.discover(observed, ObservedLineProtocol.prepare(observed),
      List(CommentaryDiscovery.Anchor(Nil, "h2h3")), sources, settled, pvGuided = true)
    assert(actionPrefixes(advanced.additions).contains(List("h2h3", "b8c6", "d2d4", "d7d5", "e2e4")),
      "answering the first action must reveal the next action from the same unchanged PV")
    assert(actionPrefixes(advanced.additions).forall(_.size <= 5))
    assert(actionPrefixes(advanced.additions).contains(List("h2h3", "b8c6", "d2d4")),
      "already issued questions remain available for reconsideration")
    assertEquals(advanced.replayNeeds, Nil)
    assert(advanced.scans.exists(_.reason.contains("action_frontier")))

  test("a long generated answer retains strength uncertainty and conditional scope before omitting details"):
    // These inline reader results stress prose composition, not the truth of a chess claim.
    val subjects = List(Piece("white", "knight", "b1"), Piece("black", "knight", "b8"),
      Piece("white", "bishop", "c1"), Piece("black", "bishop", "c8"))
    val effects = (0 until 199).toList.map { index =>
      val predicate = Predicate("control", Some(subjects(index / 64)),
        square = Some(chess.Square.all(index % 64).key))
      val path = Path(s"effect_$index", Declared(List("h2h3")))
      // Distinct declared scopes exercise the longer path comparison at the same
      // reader limit; the compact adjacent-move wording no longer fills that budget.
      val before = Fact(s"effect_${index}_before", Path(s"effect_${index}_root", Declared(Nil)), predicate, Ply(0))
      val after = Fact(s"effect_${index}_after", path, predicate, Ply(1))
      (before, after, FactComparison(s"effect_$index", before.id, after.id, "different"))
    }
    val strength = CompareMoves("strength", Nil, "h2h3", "a2a3")
    val unknown = CompareMoves("unverified_strength", Nil, "h2h3", "d2d4")
    val requirements: List[Requirement] = effects.flatMap { (before, after, compare) =>
      List(before, after, compare)
    } ++ List(strength, unknown)
    val source = Source("generator", "inline-answer-budget")
    val request = Request("long-preparation-question", Origin(source, source, source, source), requirements)
    assertEquals(requirements.size, 599)
    assert(validRequests(List(request)))
    def observed(fact: Fact, value: String): Reading =
      val moves = fact.path.source match
        case Declared(moves) => moves
        case _ => fail("this inline reader case uses declared paths")
      Reading(fact.id, "observed", result = Some(Result("observed",
        samples = List(Sample(fact.id + "-sample", "inline-line", fact.path.binding, moves,
          fact.at.horizon, fact.at.horizon, Some(value), "known")))))
    val answer = CommentaryInvestigation.Answer("inline-work", "f" * 64, profile.wireId, 16,
      Some(16), Some(16), "better", "reference-line", "alternative-line", Json.obj(), Json.obj(),
      "reference-occurrence", "alternative-occurrence", None)
    val readings = effects.flatMap { (before, after, compare) =>
      List(observed(before, "absent"), observed(after, "present"),
        Reading(compare.id, "supported", result = Some(Result("supported"))))
    } ++ List(pairReading(strength, answer), Reading(unknown.id, "deferred", Some("investigation_limit")))
    val arm = GeneratedArm("long-preparation-arm", Nil, "h2h3", None,
      List(strength.id, unknown.id), requirements.map(_.id), source, Nil)
    val entry = Entry(request, readings, "generated", List(arm),
      Some("Which moves change the preparation, how strong is the alternative, and which conditions remain unknown?"))
    val body = snapshot(entry)
    val text = (body \ "answer_text").as[String]
    assert(text.length <= 16000)
    assert(text.contains("a2a3 evaluates above h2h3 for the side to move at the starting position."))
    assert(text.contains("d2d4 versus h2h3: unverified (investigation_limit)."))
    assert(text.contains("Unresolved conditions: 1"))
    assert(text.contains("Structural effects belong to the named moves."))
    assert(text.contains("Evaluation compares whole continuations; the observed plan is conditional and may change in the returned line."))
    assert(text.contains("further observations are omitted from this answer."))
    val rows = (body \ "requirements").as[List[JsObject]]
    assertEquals(rows.map(row => (row \ "id").as[String]), requirements.map(_.id),
      "omitting prose details must preserve each structured reading in its original order")
    val byId = rows.map(row => (row \ "id").as[String] -> row).toMap
    readings.foreach { reading =>
      val row = byId(reading.requirementId)
      assertEquals((row \ "state").as[String], reading.state)
      assertEquals((row \ "stop_reason").asOpt[String], reading.stopReason)
      assertEquals((row \ "answer").asOpt[JsObject], reading.answer.map(CommentaryInvestigation.answerJson))
      assertEquals((row \ "result").asOpt[JsObject], reading.result.map(_.json))
    }
    requirements.collect { case fact: Fact => fact }.foreach { fact =>
      val row = byId(fact.id)
      assertEquals((row \ "path").as[JsObject], fact.path.json)
      assertEquals((row \ "predicate").as[JsObject], fact.predicate.json)
      assertEquals((row \ "at").as[JsObject], fact.at.json)
    }
