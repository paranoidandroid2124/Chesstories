package io.chesstory.runtime

import lila.chessjudgment.model.judgment.EvidenceBackedJudgmentPacket
import play.api.libs.json.*

/** Submitted hypotheses name work and individual effects; they do not certify their own evidence. */
object CommentaryProposal:
  final case class Source(kind: String, producer: String, version: Option[String] = None,
      sourceRefs: List[String] = Nil)
  final case class Origin(question: Source, candidates: Source, priority: Source, evidence: Source)
  final case class Request(id: String, origin: Origin, requirements: List[Requirement])

  sealed trait Requirement:
    def id: String
  final case class ObserveMove(id: String, prefixMovesUci: List[String], moveUci: String) extends Requirement
  final case class CompareMoves(id: String, prefixMovesUci: List[String], referenceMove: String,
      alternativeMove: String, expectedRelation: Option[String] = None) extends Requirement
  final case class Uninterpreted(id: String, text: String) extends Requirement
  final case class Fact(id: String, path: CommentaryFact.Path, predicate: CommentaryFact.Predicate,
      at: CommentaryFact.Point, through: Option[CommentaryFact.Point] = None,
      expected: Option[String] = None) extends Requirement
  final case class FactComparison(id: String, leftRequirementId: String, rightRequirementId: String,
      relation: String) extends Requirement
  final case class FactGroup(id: String, mode: String, members: List[String]) extends Requirement

  val MaximumRequirements = 600
  val MaximumLocalExpansions = 600
  private val SourceKinds = Set("user", "author", "model", "generator", "none")
  private val Relations = Set("better", "equal", "worse")

  def validRequests(requests: List[Request]): Boolean =
    def text(value: String, maximum: Int) = value.nonEmpty && value.length <= maximum
    def source(value: Source) = SourceKinds(value.kind) && text(value.producer, 128) &&
      value.version.forall(text(_, 128)) && value.sourceRefs.size <= 64 && value.sourceRefs.forall(text(_, 256))
    def prefix(value: List[String]) = value.size < CommentaryFact.MaximumPly && value.forall(EngineLineAdmission.acceptsCanonicalLowercaseUci)
    def valid(requirement: Requirement): Boolean = RuntimeProtocol.acceptsRequestId(requirement.id) && (requirement match
      case ObserveMove(_, moves, move) => prefix(moves) && EngineLineAdmission.acceptsCanonicalLowercaseUci(move)
      case CompareMoves(_, moves, reference, alternative, relation) => prefix(moves) && reference != alternative &&
        List(reference, alternative).forall(EngineLineAdmission.acceptsCanonicalLowercaseUci) && relation.forall(Relations)
      case Uninterpreted(_, value) => text(value, 2000)
      case Fact(_, path, predicate, at, through, expected) => CommentaryFact.validPath(path) &&
        CommentaryFact.validPredicate(predicate) && CommentaryFact.validPoint(at) && through.forall(CommentaryFact.validPoint) &&
        expected.forall(CommentaryFact.values(predicate)) && (through.isEmpty || expected.nonEmpty)
      case FactComparison(_, left, right, relation) => left != right &&
        Set("same", "different", "before", "after", "same_time")(relation)
      case FactGroup(_, mode, members) => Set("all", "any")(mode) && members.nonEmpty &&
        members.size <= MaximumRequirements && members.distinct.size == members.size)
    requests.size <= MaximumRequirements && requests.map(_.id).distinct.size == requests.size &&
      requests.foldLeft(0L)(_ + _.requirements.size) <= MaximumRequirements && requests.forall { request =>
        RuntimeProtocol.acceptsRequestId(request.id) && request.requirements.nonEmpty &&
          request.requirements.map(_.id).distinct.size == request.requirements.size && request.requirements.forall(valid) &&
          List(request.origin.question, request.origin.candidates, request.origin.priority, request.origin.evidence).forall(source) &&
          request.requirements.zipWithIndex.forall { (requirement, index) =>
            val prior = request.requirements.take(index).map(value => value.id -> value).toMap
            def assertion(id: String): Boolean = prior.get(id).exists {
              case fact: Fact => fact.expected.nonEmpty
              case _: FactComparison | _: FactGroup => true
              case _ => false
            }
            requirement match
              case fact: Fact => fact.path.source match
                case CommentaryFact.Comparison(id, _) => prior.get(id).exists(_.isInstanceOf[CompareMoves])
                case _ => true
              case FactComparison(_, left, right, relation) => (prior.get(left), prior.get(right)) match
                case (Some(a: Fact), Some(b: Fact)) => a.through.isEmpty && b.through.isEmpty &&
                  (if Set("same", "different")(relation) then a.predicate.kind == b.predicate.kind
                   else a.path.binding == b.path.binding)
                case _ => false
              case FactGroup(_, _, members) => members.forall(assertion)
              case _ => true
          }
      }

  private[runtime] final case class Reading(requirementId: String, state: String = "deferred",
      stopReason: Option[String] = None, answer: Option[CommentaryInvestigation.Answer] = None,
      lineId: Option[String] = None, occurrenceId: Option[String] = None,
      positionId: Option[String] = None, workId: Option[String] = None,
      result: Option[CommentaryFact.Result] = None)
  /** Heads are independent decisions; a newly observed comparator adds a head without changing
    * any earlier question. Members also include supporting observations, not an all assertion.
    */
  private[runtime] final case class GeneratedArm(id: String, prefixMoves: List[String], referenceMove: String,
      alternativeMove: Option[String], headRequirementIds: List[String], memberRequirementIds: List[String],
      source: Source, sourceRefs: List[String])
  private[runtime] final case class Entry(request: Request, readings: List[Reading], admission: String = "submitted",
      generatedArms: List[GeneratedArm] = Nil, questionText: Option[String] = None)

  private[runtime] def entries(requests: List[Request]): List[Entry] = requests.map(request =>
    Entry(request, request.requirements.map {
      case value: Uninterpreted => Reading(value.id, "unsupported_reading", Some("uninterpreted_hypothesis"))
      case value => Reading(value.id)
    }))

  private[runtime] def prefix(requirement: Requirement): List[String] = requirement match
    case value: ObserveMove => value.prefixMovesUci
    case value: CompareMoves => value.prefixMovesUci
    case _: Uninterpreted => Nil
    case fact: Fact => fact.path.source match
      case CommentaryFact.Declared(moves) => moves
      case _ => Nil
    case _: FactComparison | _: FactGroup => Nil

  /** A longer certified line can witness this exact historical prefix without a new replay. */
  private[runtime] def observedPrefix(packet: EvidenceBackedJudgmentPacket,
      prepared: ObservedLineProtocol.PreparedPaths, moves: List[String]): Option[ObservedLineProtocol.PreparedPath] =
    prepared.requireOwner(packet)
    prepared.paths.filter(path => path.certified.nonEmpty && path.ids == path.declared &&
      path.moves.startsWith(moves)).sortBy(path => (path.moves.size, path.line.ref.id)).headOption

  private[runtime] def pairReading(requirement: CompareMoves, answer: CommentaryInvestigation.Answer): Reading =
    Reading(requirement.id, requirement.expectedRelation.fold("answered")(expected =>
      if expected == answer.evaluationRelation then "supported" else "refuted"), answer = Some(answer),
      workId = Some(answer.workId))

  private[runtime] def isResolved(reading: Reading): Boolean =
    Set("observed", "answered", "supported", "refuted")(reading.state) ||
      (reading.state == "not_applicable" && reading.result.exists(result =>
        result.reason.contains("event_absent") && result.needs.isEmpty))

  /** The facts this comparison tests, including already satisfied premises. Keep
    * arm ownership and all/any decisions; supporting members are not extra heads.
    * Facts read from this comparison's answer cannot be its own prerequisite.
    */
  private[runtime] def comparisonContext(entry: Entry, comparison: CompareMoves, readyOnly: Boolean = false): List[Fact] =
    comparisonContexts(entry, comparison, readyOnly).flatten.distinct

  /** Keep each arm intact when several facts must describe the same condition. */
  private[runtime] def comparisonContexts(entry: Entry, comparison: CompareMoves,
      readyOnly: Boolean = false): List[List[Fact]] =
    val requirements = entry.request.requirements.map(value => value.id -> value).toMap
    val readings = entry.readings.map(value => value.requirementId -> value).toMap
    def facts(heads: List[String]): List[Fact] =
      val visited = scala.collection.mutable.Set.empty[String]
      def visit(id: String): List[Fact] =
        if !visited.add(id) then Nil else requirements.get(id).toList.flatMap {
          case fact: Fact => fact.path.source match
            case CommentaryFact.Comparison(source, _) if source == comparison.id => Nil
            case _ => List(fact)
          case FactComparison(_, left, right, _) => visit(left) ++ visit(right)
          case FactGroup(_, mode, members) =>
            val supported = members.filter(id => readings.get(id).exists(value =>
              Set("supported", "observed")(value.state)))
            (if mode == "any" && supported.nonEmpty then supported else members).flatMap(visit)
          case _ => Nil
        }
      heads.flatMap(visit)
    entry.generatedArms.filter(arm => arm.headRequirementIds.contains(comparison.id) &&
      arm.headRequirementIds.filterNot(id => requirements.get(id).exists(_.isInstanceOf[CompareMoves])).forall(id =>
        readings.get(id).exists(value => if readyOnly then Set("supported", "observed")(value.state)
          else !Set("refuted", "invalid_input", "unsupported_reading", "stopped", "not_applicable")(value.state))))
      .map(arm => facts(arm.headRequirementIds))

  /** Only unresolved decisions and their dependencies can justify more work. A resolved all/any
    * group does not require exploring the branches that cannot change its answer.
    */
  private[runtime] def neededRequirementIds(entry: Entry): Set[String] =
    val requirements = entry.request.requirements.map(value => value.id -> value).toMap
    val readings = entry.readings.map(value => value.requirementId -> value).toMap
    val needed = scala.collection.mutable.Set.empty[String]
    def visit(id: String): Unit =
      if readings.get(id).exists(value => !isResolved(value) &&
          !Set("invalid_input", "unsupported_reading", "stopped")(value.state)) && needed.add(id) then
        requirements.get(id).foreach {
          case FactGroup(_, _, members) => members.foreach(visit)
          case FactComparison(_, left, right, _) => visit(left); visit(right)
          case fact: Fact => fact.path.source match
            case CommentaryFact.Comparison(comparison, _) => visit(comparison)
            case _ => ()
          case _ => ()
        }
    if entry.generatedArms.isEmpty then entry.request.requirements.foreach(value => visit(value.id))
    else entry.generatedArms.foreach { arm =>
      val (comparisons, context) = arm.headRequirementIds.partition(id => requirements.get(id).exists(_.isInstanceOf[CompareMoves]))
      context.foreach(visit)
      if context.forall(id => readings.get(id).exists(value => Set("supported", "observed")(value.state))) then
        comparisons.foreach(visit)
    }
    needed.toSet

  private[runtime] def finish(entries: List[Entry], reason: String): List[Entry] =
    val current = CommentaryIdea.currentQuestions(entries).map(_.request.id).toSet
    entries.map { entry =>
      val needed = if current(entry.request.id) then neededRequirementIds(entry) else Set.empty[String]
      val comparisons = entry.request.requirements.collect { case value: CompareMoves => value.id }.toSet
      val readings = entry.readings.map(value => value.requirementId -> value).toMap
      val blocked = entry.generatedArms.flatMap { arm =>
        val context = arm.headRequirementIds.filterNot(comparisons).map(readings)
        Option.when(context.exists(value => !Set("supported", "observed")(value.state)))(
          arm.headRequirementIds.filter(comparisons).map(_ ->
            (if context.exists(_.state == "refuted") then "premise_refuted" else "prerequisite_unresolved")))
          .toList.flatten
      }.toMap
      entry.copy(readings = entry.readings.map { reading =>
        if isResolved(reading) || Set("invalid_input", "unsupported_reading", "stopped")(reading.state) then reading
        else reading.copy(state = "stopped", stopReason = Some(
          if !needed(reading.requirementId) && blocked.contains(reading.requirementId) then blocked(reading.requirementId)
          else if !needed(reading.requirementId) then "not_required_for_decision"
          else reading.stopReason.filter(Set("local_expansion_limit", "not_returned", "comparison_conflict")).getOrElse(reason)))
      })
    }

  /** Questions retain their oriented answers. Equivalent explanations share the exact
    * comparison witness instead of repeating it in both directions.
    */
  private[runtime] def comparisonCandidates(packet: EvidenceBackedJudgmentPacket,
      prepared: ObservedLineProtocol.PreparedPaths, entries: List[Entry],
      compose: Option[(JsObject, List[MoveWorkProjection.Context]) => JsObject] = None): List[JsObject] =
    prepared.requireOwner(packet)
    val notation = prepared.notation
    lazy val projection = MoveWorkProjection.select(packet, prepared)
    val render = compose.getOrElse((candidate: JsObject, context: List[MoveWorkProjection.Context]) =>
      projection.withMoveEffects(candidate, context = context))
    val root = packet.assembly.playedTransition.get.evidence.id
    val pathsById = prepared.paths.map(path => path.line.ref.id -> path).toMap
    val ideas = CommentaryIdea.read(entries)
    val comparisons = entries.flatMap { entry =>
      val requirements = entry.request.requirements.map(value => value.id -> value).toMap
      val readings = entry.readings.map(value => value.requirementId -> value).toMap
      val heads = entry.generatedArms.filter(arm => arm.headRequirementIds
        .filterNot(id => requirements(id).isInstanceOf[CompareMoves])
        .forall(id => Set("supported", "observed")(readings(id).state))).flatMap(_.headRequirementIds).toSet
      entry.request.requirements.collect {
        case value: CompareMoves if heads(value.id) =>
          (entry.request.id, value, readings(value.id))
      }
    }
    val answeredByQuestion = comparisons.flatMap { (id, value, reading) =>
      reading.answer.map(answer => (id -> value.id) -> CommentaryIdea.AnsweredComparison(id, value, answer))
    }.toMap
    val rows = comparisons.groupBy((_, value, _) => (value.prefixMovesUci, value.referenceMove, value.alternativeMove))
      .toList.sortBy((key, _) => key.toString).flatMap { case ((prefix, reference, alternative), unorderedMembers) =>
        val members = unorderedMembers.sortBy((id, value, _) => (id, value.id))
        val answers = members.map(_._3.answer).distinct
        for
          context <- (if prefix.isEmpty then prepared.paths.find(_.ids.headOption.contains(root))
            else observedPrefix(packet, prepared, prefix)).toList
          position <- context.positions.lift(prefix.size).toList
          mover <- position.sideToMove.toList
          if answers.size == 1 && (answers.head.nonEmpty || members.map(_._3.stopReason).distinct.size == 1)
          answer = answers.head
          paths = answer.fold(List(context))(value =>
            List(value.referenceLineId, value.alternativeLineId).flatMap(pathsById.get))
          if paths.size == (if answer.nonEmpty then 2 else 1)
        yield
          val relation = answer.map(_.evaluationRelation)
          val side = if mover.white then "white" else "black"
          val comparison = relation.map {
            case "better" => "above"
            case "worse" => "below"
            case _ => "equal to"
          }
          val pairText = comparison.fold(
            s"The strength of ${notation.move(prefix, alternative)} relative to ${notation.move(prefix, reference)} for $side remains unverified at this position.")(
            value => s"At this position, ${notation.move(prefix, alternative)} evaluates $value ${notation.move(prefix, reference)} for $side.")
          val presentChoice = members.exists((_, value, _) => value.id == "contrast_opponent_choice")
          val options = entries.filter(entry => members.exists(_._1 == entry.request.id))
            .sortBy(entry => entry.readings.find(_.requirementId == "tactical_strength").flatMap(_.answer)
              .fold(2)(answer => if answer.evaluationRelation == "better" then 0 else 1))
            .flatMap { entry =>
              val parts = opponentOptions(entry, notation, includeCounter = prefix.isEmpty) ++
                (if prefix.isEmpty then counterConsequences(packet, prepared, entry)
                  .map(text => ComparisonPart(text, counter = counterKey(entry))) else Nil)
              val (counter, other) = parts.partition(_.counter.nonEmpty)
              // Keep a reply's conditions and both owned outcomes together at the text limit.
              counter.headOption.map(first => first.copy(text = counter.map(_.text).distinct.mkString(" "))).toList ++ other
            }.distinct
          val decisionFirst = relation.exists(result =>
            (prefix.isEmpty && ((reference == packet.assembly.input.playedMoveUci && result == "better") ||
              (alternative == packet.assembly.input.playedMoveUci && result == "worse"))) ||
              (result == "better" && members.exists(_._2.expectedRelation.contains("better"))))
          val relatedIdeas = ideas.filter(idea => idea.members.exists(member => members.exists(_._1 == member.entry.request.id)) ||
            (!idea.withdrawn && idea.strongerComparisons.exists((value, _) => value.prefixMovesUci == prefix &&
              Set(value.referenceMove, value.alternativeMove) == Set(reference, alternative))))
          val idea = relatedIdeas.find(_.state != "withdrawn").orElse(relatedIdeas.headOption)
          val selected = answer.nonEmpty && context.ids.headOption.contains(root) &&
            ((presentChoice && options.nonEmpty) || !idea.exists(_.state == "withdrawn"))
          val priority = idea.toList.flatMap(value => members.flatMap(member => value.priority(member._2)))
            .minOption.getOrElse(Int.MaxValue -> Int.MaxValue)
          val id = CommentarySelection.stableId("question-comparison", prefix ++ List(reference, alternative))
          val candidate = Json.obj("id" -> id,
            "kind" -> "comparison",
            "selection" -> (if selected then "selected" else "context"),
            "root_occurrence_id" -> root,
            "occurrence_ids" -> (root :: paths.flatMap(_.ids)).distinct,
            "position_ids" -> paths.flatMap(_.positions.flatMap(_.id)).filter(prepared.resourceIds).distinct,
            "line_ids" -> paths.map(_.line.ref.id), "evidence_ids" -> Json.arr(), "source_refs" -> Json.arr(),
            "detail" -> Json.obj("type" -> "question_comparison", "scope" -> "same_position_pair",
              "prefix_moves_uci" -> prefix, "reference_move" -> reference, "alternative_move" -> alternative,
              "perspective" -> side, "question_refs" -> members.map((id, value, _) =>
                Json.obj("proposal_id" -> id, "requirement_id" -> value.id)).distinct,
              "evaluation_relation" -> relation, "stop_reason" ->
                (if answer.nonEmpty then None else members.flatMap(_._3.stopReason).headOption),
              "text" -> pairText))
          val equivalent = answer.map { value =>
            val witnesses = List(
              (reference, value.referenceLineId, value.referenceOccurrenceId, value.referenceDepth, value.referenceEvaluation),
              (alternative, value.alternativeLineId, value.alternativeOccurrenceId, value.alternativeDepth, value.alternativeEvaluation)
            ).sortBy(_._1)
            val relation = if reference < alternative then value.evaluationRelation else value.evaluationRelation match
              case "better" => "worse"
              case "worse" => "better"
              case other => other
            (prefix, witnesses, value.workId, value.executionKeySha256, value.engineProfile,
              value.requiredDepth, relation, idea.map(_.id))
          }
          (candidate, idea, (if selected then 0 else 1, if decisionFirst then 0 else if presentChoice then 1 else 2,
            priority._1, priority._2, id), equivalent, ComparisonPart(pairText) :: options)
      }
    // Unanswered or differently owned comparisons are never collapsed. The original
    // proposals still carry every expectation, orientation and answer independently.
    val distinct = rows.groupBy { (candidate, _, _, equivalent, _) =>
      (equivalent, Option.when(equivalent.isEmpty)((candidate \ "id").as[String]))
    }.values.map { equivalentRows =>
      val (candidate, idea, priority, equivalent, parts) = equivalentRows.minBy(_._3)
      // Opposite orientations can own different valid observations of this same
      // receipt. Share their text; question references retain the selected orientation.
      val combined = parts.take(1) ++ equivalentRows.toList.sortBy(_._3).flatMap(_._5.drop(1)).distinct
      (candidate, idea, priority, equivalent, combined)
    }.toList.sortBy(_._3)
    // Keep each pair's answer and question references. Only the shared synthesis has one
    // owner, preferring a visible comparison that tests the idea's essential dependency.
    // Recompute from current evidence so revision/withdrawal cannot leave an old owner alive.
    val owners = distinct.flatMap { (candidate, idea, priority, _, _) =>
      idea.map(value => value.id -> (priority, (candidate \ "id").as[String]))
    }.groupMap(_._1)(_._2).view.mapValues(_.minBy(_._1)._2).toMap
    val publishedCounters = scala.collection.mutable.Set.empty[(String, CounterKey)]
    distinct.map { (candidate, idea, _, _, parts) =>
      val synthesis = idea.filter(value => owners.get(value.id).contains((candidate \ "id").as[String]))
      val detail = (candidate \ "detail").as[JsObject]
      val selected = (candidate \ "selection").as[String] == "selected"
      val occurrence = (candidate \ "root_occurrence_id").as[String]
      val answered = (detail \ "question_refs").as[List[JsObject]].flatMap(ref =>
        answeredByQuestion.get((ref \ "proposal_id").as[String] -> (ref \ "requirement_id").as[String])).toSet
      val answer = answered.map(_.answer).toList match
        case List(value) => Some(value)
        case _ => None
      val compact = compactTactics(parts, answer,
        (detail \ "prefix_moves_uci").as[List[String]],
        (candidate \ "line_ids").as[List[String]].flatMap(pathsById.get), notation)
      // Unanswered reply conditions stay with their linked question. Their option still
      // participates in selection above, without preceding observed effects in this body.
      val fresh = compact.filter(part => part.publish && (!selected ||
        part.counter.forall(key => !publishedCounters(occurrence -> key))))
      // Keep the answer and its direct conditions intact. Later context competes
      // only after the same compositor has admitted observed replies and costs.
      val (direct, background) = fresh.drop(1).partition(part => part.direct || part.counter.nonEmpty)
      val required = fresh.take(1) ++ synthesis.toList.flatMap(_.decisionText(answered, notation.line))
        .map(ComparisonPart(_)) ++ direct
      val context = background.map(part => MoveWorkProjection.Context(part.text, part.qualification, part.attack,
        part.tactic.filter(_ => part.attack.nonEmpty), part.tacticScope)) ++
        synthesis.toList.flatMap(_.explanationDetails(packet, prepared, answered)) ++
        Option.when(synthesis.isEmpty)(idea.toList.flatMap(_.questionFocus(packet, prepared, answered)).headOption)
          .flatten.map(focus => MoveWorkProjection.Context("", question = Some(focus))).toList
      val wording = boundedText(required.map(_.text))
      val updated = render(candidate + ("detail" -> (detail + ("text" -> JsString(wording.text)))), context)
      // The compositor retains its mandatory input. Ownership follows the
      // admitted parts, independent of punctuation or later wording changes.
      if selected then wording.retained.foreach(index =>
        required(index).counter.foreach(key => publishedCounters += (occurrence -> key)))
      updated
    }

  private[runtime] def stop(entries: List[Entry], reason: String): List[Entry] = entries.map(entry =>
    entry.copy(readings = entry.readings.map(reading =>
      if isResolved(reading) || Set("invalid_input", "unsupported_reading")(reading.state) then reading
      else reading.copy(state = "stopped", stopReason = Some(reason)))))

  private[runtime] final case class BoundedText(text: String, retained: List[Int])

  private[runtime] def comparisonText(parts: List[String]): String = boundedText(parts).text

  private[runtime] def boundedText(parts: List[String]): BoundedText =
    val text = new StringBuilder
    val retained = scala.collection.mutable.ListBuffer.empty[Int]
    var omitted = 0
    parts.zipWithIndex.foreach { (part, index) =>
      if part.isEmpty then ()
      else if text.length + part.length + 1 <= 3900 then
        if text.nonEmpty then text.append(' ')
        text.append(part)
        retained += index
      else omitted += 1
    }
    if omitted > 0 then text.append(s" $omitted further explanations remain in the linked questions.")
    BoundedText(text.toString, retained.toList)

  private type CounterKey = (CompareMoves, CommentaryInvestigation.Answer, List[(Fact, Option[Reading])])
  private case class ComparisonPart(text: String, publish: Boolean = true, counter: Option[CounterKey] = None,
      direct: Boolean = false, tactic: Option[TacticStep] = None, qualification: Option[String] = None,
      attack: Option[MoveWorkProjection.AttackContext] = None,
      tacticScope: Option[(List[String], String, Int)] = None)

  private val TacticCaveat = "These concrete attacks do not by themselves establish a forced gain."
  private[runtime] enum TacticClause:
    case Contact(attacker: CommentaryFact.Piece, target: CommentaryFact.Piece, square: String)
    case Loss(at: CommentaryFact.Event, victim: CommentaryFact.Piece)
    case Departure(at: CommentaryFact.Event, blocker: CommentaryFact.Piece, square: String, removed: Boolean)

  private[runtime] case class TacticStep(line: List[String], at: CommentaryFact.Event, actor: CommentaryFact.Piece,
      contacts: List[(CommentaryFact.Piece, CommentaryFact.Piece, String)],
      loss: Option[(CommentaryFact.Event, CommentaryFact.Piece)],
      departures: List[(CommentaryFact.Event, CommentaryFact.Piece, String, Boolean)],
      owner: Option[(CommentaryIdea.AnsweredComparison, String)]):
    def clauses: List[TacticClause] = contacts.map(TacticClause.Contact.apply.tupled) ++
      loss.toList.map(TacticClause.Loss.apply.tupled) ++ departures.map(TacticClause.Departure.apply.tupled)

    def text(notation: CommentaryNotation, scope: Option[(List[String], String, Int)] = None,
        omitted: Set[TacticClause] = Set.empty, includeQualification: Boolean = true,
        locate: (CommentaryFact.Piece, Int) => Option[String] = (_, _) => None): String =
      def piece(value: CommentaryFact.Piece, ply: Int) = locate(value, ply)
        .getOrElse(s"${value.side} ${value.role} originally on ${value.square}")
      def step(event: CommentaryFact.Event) = notation.line(line)(event.fromPly)
      val introduction = scope.fold(s"In the returned continuation ${notation.line(line).mkString(" ")}, ") { (prefix, move, from) =>
        val between = notation.line(line).slice(from, at.fromPly)
        s"In the returned ${notation.move(prefix, move)} line" + (if prefix.isEmpty then "" else s" after ${notation.line(prefix).mkString(" ")}") +
          (if between.isEmpty then ", "
           else s", after ${between.mkString(" ")}, ")
      }
      val capture = loss.filterNot(value => omitted(TacticClause.Loss.apply.tupled(value)))
        .map((event, victim) => s"${step(event)} removes the ${piece(victim, event.fromPly)}").toList
      val remainingDepartures = departures.filterNot(value => omitted(TacticClause.Departure.apply.tupled(value)))
      // A new line-opening condition still qualifies the original contact set.
      // Removing just its already-shared slider contact could attach that condition
      // to an unrelated newly introduced direct attack instead.
      val remainingContacts = if remainingDepartures.nonEmpty then contacts
        else contacts.filterNot(value => omitted(TacticClause.Contact.apply.tupled(value)))
      val mutual = remainingContacts.collect {
        case (first, second, _) if locate(first, at.throughPly).nonEmpty && locate(second, at.throughPly).nonEmpty &&
            remainingContacts.exists((actor, target, _) => actor == second && target == first) => first -> second
      }.distinctBy((first, second) => Set(first, second))
      val paired = mutual.flatMap((first, second) => List(first -> second, second -> first)).toSet
      val separate = remainingContacts.filterNot((actor, target, _) => paired(actor -> target))
      val attacks = (mutual.map { (first, second) =>
        remainingContacts.indexWhere(row => row._1 == first && row._2 == second) ->
          s"the ${piece(first, at.throughPly)} and the ${piece(second, at.throughPly)} attack each other"
      } ++ separate.map(_._1).distinct.map { attacker =>
        val targets = separate.collect { case (`attacker`, victim, square) =>
          "the " + locate(victim, at.throughPly).getOrElse(s"${piece(victim, at.throughPly)} on $square")
        }
        remainingContacts.indexWhere(_._1 == attacker) ->
          s"the ${piece(attacker, at.throughPly)} attacks ${targets.mkString(" and ")}"
      }).sortBy(_._1).map(_._2)
      val opened = remainingDepartures.map(_._3).distinct.flatMap { square =>
        val changes = remainingDepartures.filter(_._3 == square).sortBy(_._1.fromPly)
        val (last, _, _, removed) = changes.last
        // Only already-described captures can be referred to by their moves.
        // Otherwise this explanation still has to name every captured blocker.
        if changes.size > 1 && !removed && changes.init.forall { (event, blocker, _, captured) =>
            captured && omitted(TacticClause.Loss(event, blocker))
          } then List(s"${step(last)} clears $square after the captures ${changes.init.map(row => step(row._1)).mkString(", ")}")
        else changes.map { (event, blocker, _, removed) =>
          if removed then "the " + locate(blocker, event.fromPly).getOrElse(s"${piece(blocker, event.fromPly)} on $square") +
            s" is removed by ${step(event)}"
          else s"the ${piece(blocker, event.fromPly)} leaves $square with ${step(event)}"
        }
      }
      if capture.isEmpty && attacks.isEmpty && opened.isEmpty then ""
      else introduction + (if attacks.nonEmpty then
        val action = if locate(actor, at.fromPly).nonEmpty then s"${step(at)}: ${attacks.mkString("; ")}"
          else s"${step(at)} by the ${piece(actor, at.fromPly)} creates these new attacks: ${attacks.mkString("; ")}"
        (capture :+ action).mkString("; ") + "."
        else s"For the attacks created by ${step(at)} with the ${piece(actor, at.fromPly)}" +
          (if capture.isEmpty then "." else s", ${capture.mkString("; ")}.")) +
        (if opened.isEmpty then "" else s" The line opens after ${opened.mkString(" and ")}.") +
        (if scope.nonEmpty || !includeQualification then "" else " " + TacticCaveat)

  private def compactTactics(parts: List[ComparisonPart], answer: Option[CommentaryInvestigation.Answer],
      prefix: List[String], paths: List[ObservedLineProtocol.PreparedPath], notation: CommentaryNotation): List[ComparisonPart] =
    val owned = parts.map(_.tactic.flatMap { tactic =>
      for
        (question, lineId) <- tactic.owner
        comparison = question.comparison
        receipt = question.answer
        if answer.contains(receipt) && comparison.prefixMovesUci == prefix
        path <- paths.find(_.line.ref.id == lineId)
        if path.certified.nonEmpty && path.ids == path.declared && path.moves.startsWith(tactic.line) &&
          tactic.at.fromPly == tactic.line.size - 1 && tactic.at.throughPly == tactic.line.size
        move <- (if lineId == receipt.referenceLineId then Some(comparison.referenceMove)
          else Option.when(lineId == receipt.alternativeLineId)(comparison.alternativeMove))
        if path.moves.startsWith(prefix :+ move)
      yield ((receipt, lineId), move)
    })
    val boundaries = parts.zip(owned).collect {
      case (part, Some((key, _))) => key -> part.tactic.get.at.throughPly
    }.groupMap(_._1)(_._2)
    val shared = boundaries.iterator.collect { case (key, plies) if plies.size > 1 => key }.toSet
    // Each event stays independently admissible at the existing text limit.
    // Sharing the PV label must not make a later event depend on an earlier paragraph surviving.
    parts.zip(owned).map { (part, ownership) =>
      val anchored = part.copy(attack = ownership.map { (key, _) =>
        val tactic = part.tactic.get
        MoveWorkProjection.AttackContext(key._2, tactic.line,
          tactic.contacts.map((actor, target, _) => actor -> target), Some(tactic.actor))
      }, text = if ownership.isEmpty then part.text else part.tactic.get.text(notation, includeQualification = false),
        qualification = if ownership.isEmpty then part.qualification else Some(TacticCaveat))
      ownership.filter((key, _) => shared(key)).fold(anchored) { (key, move) =>
        val tactic = part.tactic.get
        // Retain intervening replies once; a shorter label must not erase an
        // exchange or relocation that was visible in the original full prefix.
        val from = boundaries(key).filter(_ <= tactic.at.fromPly).maxOption.getOrElse(prefix.size + 1)
        val scope = Some((prefix, move, from))
        anchored.copy(text = tactic.text(notation, scope), qualification = Some(TacticCaveat), tacticScope = scope)
      }
    }

  private def counterKey(entry: Entry): Option[CounterKey] =
    for
      comparison <- entry.request.requirements.collectFirst { case value: CompareMoves if value.id == "tactical_strength" => value }
      answer <- entry.readings.find(_.requirementId == comparison.id).flatMap(_.answer)
    yield (comparison, answer, entry.request.requirements.collect {
      case fact: Fact if fact.id.startsWith("tactical_") =>
        // Sample IDs name the submitting proposal. Preserve their proof links and
        // every actual line, history, source and value while sharing the same reading.
        fact -> entry.readings.find(_.requirementId == fact.id).map(reading => reading.copy(
          result = reading.result.map { result =>
            val ids = result.samples.zipWithIndex.map((sample, index) => sample.id -> index.toString).toMap
            result.copy(samples = result.samples.map(sample => sample.copy(id = ids(sample.id))),
              proofs = result.proofs.map(proof => proof.copy(sampleIds = proof.sampleIds.map(id => ids.getOrElse(id, id)))))
          }))
    }.sortBy(_._1.id))

  private def opponentOptions(entry: Entry, notation: CommentaryNotation, includeCounter: Boolean): List[ComparisonPart] =
    val facts = entry.request.requirements.collect { case fact: Fact => fact }.map(fact => fact.id -> fact).toMap
    val readings = entry.readings.map(reading => reading.requirementId -> reading).toMap
    def sampled(id: String): Option[String] = readings.get(id).filter(reading => Set("supported", "observed")(reading.state))
      .flatMap(_.result).flatMap(result => result.samples.flatMap(_.value).distinct match
        case List(value) => Some(value)
        case _ => None)
    def piece(value: CommentaryFact.Piece): String = s"${value.side} ${value.role} originally on ${value.square}"
    val captures = for
      contrast <- entry.request.requirements.collect { case value: FactComparison if value.id.startsWith("opponent_contrast") => value }
      left <- facts.get(contrast.leftRequirementId).toList
      right <- facts.get(contrast.rightRequirementId).toList
      actor <- left.predicate.subject.toList
      target <- left.predicate.target.toList
      av <- sampled(left.id).toList
      bv <- sampled(right.id).toList
      am <- (left.path.source match
        case CommentaryFact.Declared(moves) => notation.line(moves).lastOption.toList
        case _ => Nil)
      bm <- (right.path.source match
        case CommentaryFact.Declared(moves) => notation.line(moves).lastOption.toList
        case _ => Nil)
    yield s"After $am, capture by the ${piece(actor)} of the ${piece(target)} is ${if av == "legal" then "available" else "unavailable"}; after $bm it is ${if bv == "legal" then "available" else "unavailable"}."
    val replies = entry.request.requirements.collect {
      case fact: Fact if fact.id.startsWith("reply_") && fact.id.endsWith("_action") => fact
    }.flatMap { action =>
      for
        _ <- sampled(action.id)
        _ <- sampled("current_" + action.id)
        line <- (action.path.source match
          case CommentaryFact.Declared(moves) => Some(moves)
          case _ => None)
        targets = entry.request.requirements.collect {
          case fact: Fact if fact.path == action.path && fact.id.endsWith("_target") => fact
        }.flatMap { target =>
          for
            square <- sampled(target.id)
            _ <- sampled(target.id.stripSuffix("_target") + "_after").filter(_ == "present")
            actor <- target.predicate.subject
          yield (actor, square)
        }.sortBy((actor, _) => if actor.square == line.head.take(2) then 0 else 1)
        if targets.size >= 2
      yield s"After ${notation.line(line).head}, the returned reply ${notation.line(line).last} simultaneously attacks ${targets.map((actor, square) => s"${piece(actor)} on $square").mkString(" and ")}. This geometry does not establish a forced gain; the comparison evaluates the whole continuation."
    }
    val tactics = facts.values.toList.filter(fact => fact.id.startsWith("tactic_") && fact.id.endsWith("_cause"))
      .sortBy(fact => (fact.at.horizon, fact.id)).flatMap { cause =>
        val stem = cause.id.stripSuffix("_cause")
        for
          line <- (cause.path.source match
            case CommentaryFact.Declared(moves) => Some(moves)
            case _ => None)
          event <- (cause.at match
            case value: CommentaryFact.Event => Some(value)
            case _ => None)
          actor <- cause.predicate.subject
          if facts.values.filter(_.id.startsWith(stem + "_")).forall(fact => sampled(fact.id).nonEmpty)
          // A repeated event at the same ply cannot keep the causal story of a
          // different intervening history current.
          if readings.get("current_" + cause.id).filter(reading => Set("supported", "observed")(reading.state))
            .flatMap(_.result).exists(_.samples.exists(_.moves.take(line.size) == line))
          contacts = facts.values.toList.filter(fact => fact.id.startsWith(stem + "_contact_") && fact.id.endsWith("_target"))
            .sortBy(_.id).flatMap { target =>
              for
                victim <- target.predicate.subject
                square <- sampled(target.id)
                attack <- facts.get(target.id.stripSuffix("_target") + "_after")
                attacker <- attack.predicate.subject
              yield (attacker, victim, square)
            }
          if contacts.nonEmpty
        yield
          val loss = facts.get(stem + "_exchange").flatMap(fact => fact.predicate.subject.map(victim =>
            fact.at.asInstanceOf[CommentaryFact.Event] -> victim))
          val departures = facts.values.toList.filter(fact => fact.id.startsWith(stem + "_contact_") && fact.id.endsWith("_left"))
            .sortBy(fact => (fact.at.horizon, fact.id)).flatMap { fact =>
              for
                blocker <- fact.predicate.subject
                square <- sampled(fact.id.stripSuffix("_left") + "_before")
              yield (fact.at.asInstanceOf[CommentaryFact.Event], blocker, square, sampled(fact.id).contains("removed"))
            }.distinct
          val owner = for
            current <- facts.get("current_" + cause.id)
            if current.predicate == cause.predicate && current.at == cause.at &&
              current.through == cause.through && current.expected == cause.expected
            (requirementId, arm) <- (current.path.source match
              case CommentaryFact.Comparison(id, arm) => Some(id -> arm)
              case _ => None)
            comparison <- entry.request.requirements.collectFirst {
              case value: CompareMoves if value.id == requirementId => value
            }
            answer <- readings.get(requirementId).flatMap(_.answer)
            lineId <- arm match
              case "reference" => Some(answer.referenceLineId)
              case "alternative" => Some(answer.alternativeLineId)
              case _ => None
          yield (CommentaryIdea.AnsweredComparison(entry.request.id, comparison, answer), lineId)
          TacticStep(line, event, actor, contacts, loss, departures, owner)
      }
    val defenses = facts.values.toList.filter(fact => fact.id.startsWith("defense_") && fact.id.endsWith("_reference"))
      .sortBy(_.id).flatMap { fact =>
        val stem = fact.id.stripSuffix("_reference")
        for
          alternative <- facts.get(stem + "_alternative")
          a <- sampled(fact.id)
          b <- sampled(alternative.id)
          _ <- sampled(stem + "_current_reference")
          _ <- sampled(stem + "_current_alternative")
          provider <- fact.predicate.subject
          target <- fact.predicate.target
          left <- (fact.path.source match
            case CommentaryFact.Declared(moves) => Some(moves)
            case _ => None)
          right <- (alternative.path.source match
            case CommentaryFact.Declared(moves) => Some(moves)
            case _ => None)
        yield
          val arm = if a == "present" then "reference" else "alternative"
          val leftSan = notation.line(left)
          val rightSan = notation.line(right)
          val move = if a == "present" then leftSan.head else rightSan.head
          val use = facts.values.filter(value => value.id.startsWith(stem + s"_use_${arm}_") &&
            value.predicate.kind == "direct_support_use").exists(value =>
            sampled(value.id).contains("true") && sampled(value.id + "_continuity").contains("present"))
          (if leftSan.last == rightSan.last then
            s"Against the common reply ${leftSan.last}, support by the ${piece(provider)} for the ${piece(target)} is $a after ${leftSan.head} and $b after ${rightSan.head}. "
          else s"After ${leftSan.mkString(" ")}, support by the ${piece(provider)} for the ${piece(target)} is $a; after ${rightSan.mkString(" ")}, it is $b. ") +
            (if use then s"The current tested continuation after $move actually uses that defense to recapture the attacker. "
              else "Use of this support in a recapture has not yet been established in the current compared lines. ") +
            "This distinguishes the defensive condition; it does not make the common reply illegal or explain the whole evaluation difference."
      }
    val counters = entry.request.requirements.collect { case value: CompareMoves if value.id == "tactical_strength" => value }
      .filter(_ => includeCounter && readings.get("tactical_context").exists(_.state == "supported")).map { value =>
        val answer = readings.get(value.id).flatMap(_.answer)
        val assessment = answer.fold(
          "The relative strength of these replies remains unverified.") { answer =>
          val relation = answer.evaluationRelation match
            case "better" => "above"
            case "worse" => "below"
            case _ => "equal to"
          val location = if value.prefixMovesUci.isEmpty then "at the starting position"
            else "after " + notation.line(value.prefixMovesUci).mkString(" ")
          val meaning = answer.qualityBand match
            case Some(PrincipalVariationComparison.QualityBand.Close) =>
              "The engine evaluations are close; they do not establish a stronger refutation. Whether this reply changes the explanation depends on what happens in its continuation."
            case Some(PrincipalVariationComparison.QualityBand.Separated) =>
              if answer.evaluationRelation == "better" then "Revise the explanation that relies on the weaker reply; the root choice needs verification."
              else "This reply is not rated higher; whether the same explanation still applies depends on its recorded continuation."
            case None => "The scores alone do not yet show whether one reply is clearly stronger; its continuation still matters."
          s"In this tested reply comparison $location, ${notation.move(value.prefixMovesUci, value.alternativeMove)} evaluates $relation ${notation.move(value.prefixMovesUci, value.referenceMove)}. $meaning"
        }
        val changes = facts.values.toList.filter(fact =>
          (fact.id.startsWith("tactical_direct_") || fact.id.startsWith("tactical_effect_")) && fact.id.endsWith("_reference"))
          .sortBy(fact => (if fact.predicate.kind == "check" then 0 else if fact.predicate.kind == "support" then 1 else 2, fact.id))
          .flatMap { fact =>
            for
              a <- sampled(fact.id)
              b <- sampled(fact.id.stripSuffix("_reference") + "_alternative")
            yield
              val condition = fact.predicate.kind match
                case "support" => s"support by the ${piece(fact.predicate.subject.get)} for the ${piece(fact.predicate.target.get)}"
                case "check" => s"check against ${fact.predicate.side.get}"
                case "legal_capture" => s"capture of the ${piece(fact.predicate.target.get)} by the ${piece(fact.predicate.subject.get)}"
                case "control" => s"control of ${fact.predicate.square.get} by the ${piece(fact.predicate.subject.get)}"
                case "location" => s"location of the ${piece(fact.predicate.subject.get)}"
                case _ => s"presence of the ${piece(fact.predicate.subject.get)}"
              if a == b then s"$condition remains $a in both replies" else s"$condition changes from $a to $b"
          }
        ComparisonPart(s"After ${notation.line(value.prefixMovesUci).mkString(" ")}, check ${notation.move(value.prefixMovesUci, value.alternativeMove)} instead of ${notation.move(value.prefixMovesUci, value.referenceMove)}. $assessment " +
          changes.mkString("Conditions: ", "; ", ". ") +
          "These conditions explain what is being tested, not the whole evaluation difference.", answer.nonEmpty, counterKey(entry))
      }
    // The actual reply answers the present choice before an unrelated later
    // line opening. Conditions and consequences inside each part stay together.
    counters ++ (replies ++ defenses).map(ComparisonPart(_, direct = true)) ++
      tactics.map(value => ComparisonPart(value.text(notation), tactic = Some(value))) ++
      Option.when(captures.nonEmpty)(ComparisonPart(
        "This identifies an opposing option; its legality alone does not establish its tactical value. " + captures.mkString(" ")))

  /** Read only the two lines owned by the current answer. An illustrative study line
    * cannot supply the consequences of an engine comparison with another continuation.
    */
  private def counterConsequences(packet: EvidenceBackedJudgmentPacket,
      prepared: ObservedLineProtocol.PreparedPaths, entry: Entry): List[String] =
    if !entry.readings.exists(reading => reading.requirementId == "tactical_context" && reading.state == "supported") then Nil
    else entry.request.requirements.collect { case value: CompareMoves if value.id == "tactical_strength" => value }.flatMap { value =>
      entry.readings.find(_.requirementId == value.id).flatMap(_.answer).toList.flatMap { answer =>
        val pair = List(value.referenceMove -> answer.referenceLineId, value.alternativeMove -> answer.alternativeLineId).flatMap { (move, id) =>
          for
            line <- prepared.continuationReadings.find(reading => reading.path.line.ref.id == id &&
              reading.path.moves.startsWith(value.prefixMovesUci :+ move)).toList
            ledger <- line.path.certified.flatMap(_._2.material).toList
          yield
            val events = ledger.events.filter(_.plyOffset >= value.prefixMovesUci.size)
            val losses = events.flatMap(_.capture).groupMapReduce(capture =>
              ((if capture.side.white then "black" else "white"), capture.capturedRole.name))(_ => 1)(_ + _)
              .toList.sortBy(_._1).map { case ((side, role), count) => s"$side loses $count $role${if count > 1 then "s" else ""}" }
            val promotions = events.filter(_.promotionGainCp > 0).map(event =>
              s"${if event.movement.side.white then "white" else "black"} promotes to ${event.movement.afterRole.name} with ${prepared.notation.line(line.path.moves)(event.plyOffset)}")
            val captures = line.events.filter(event => event.plyOffset >= value.prefixMovesUci.size && event.originalVictim.nonEmpty)
            val targets = entry.request.requirements.collect {
              case fact: Fact if fact.predicate.kind == "legal_capture" && fact.id.startsWith("tactical_") => fact.predicate.target.get
            }.distinct.map { target =>
              val name = s"the ${target.side} ${target.role} originally on ${target.square}"
              val absent = if prepared.pieceAfter(packet, line.path, target.witness, line.path.moves.size).exists(_.nonEmpty) then
                s"$name remains on the board at this line's end"
              else s"the fate of $name is not established by the identified capture events"
              captures.find(_.originalVictim.contains(target.witness)).fold(absent) { capture =>
                s"$name is captured on ${prepared.notation.line(line.path.moves)(capture.plyOffset)}"
              }
            }
            val exchanges = captures.flatMap { loss =>
              line.captureLinks.filter(_.lossEventId == loss.id).flatMap(link => line.events.find(_.id == link.captureEventId))
                .map(returned => s"the capturer on ${prepared.notation.line(line.path.moves)(loss.plyOffset)} is itself captured on ${prepared.notation.line(line.path.moves)(returned.plyOffset)}")
            }.distinct
            val checks = (value.prefixMovesUci.size until line.path.moves.size).filter { offset =>
              line.path.positions.lift(offset + 1).exists(position => position.sideToMove.exists(side =>
                position.id.flatMap(prepared.resourceViews(packet).get).exists(_.currentCheckSources(side).nonEmpty)))
            }.map(offset => prepared.notation.line(line.path.moves)(offset)).toList
            val observed = (targets ++ losses ++ promotions ++ exchanges.take(2) ++
              Option.when(checks.nonEmpty)("check occurs with " + checks.mkString(", "))).mkString("; ")
            s"In the returned continuation (${prepared.notation.line(line.path.moves).drop(value.prefixMovesUci.size).mkString(" ")}) beginning ${prepared.notation.move(value.prefixMovesUci, move)}, " +
              (if observed.isEmpty then "no capture, promotion or check is observed" else observed) +
              "."
        }
        // A length cap must not keep one arm's gain while silently dropping the
        // other arm's cost. The matched consequences are one indivisible part.
        if pair.size != 2 then List("The matched reply consequences are not yet available for both owned lines.")
        else List(pair.mkString(" ") + " These are the recorded consequences through each line's end, not a forced result or the cause of the entire root evaluation.")
      }
    }

  private def sourceJson(value: Source): JsObject = Json.obj("kind" -> value.kind, "producer" -> value.producer,
    "source_refs" -> value.sourceRefs) ++ value.version.fold(Json.obj())(v => Json.obj("version" -> v))
  private def requirementJson(value: Requirement): JsObject = value match
    case ObserveMove(id, moves, move) => Json.obj("id" -> id, "kind" -> "observe_move",
      "prefix_moves_uci" -> moves, "move_uci" -> move)
    case CompareMoves(id, moves, reference, alternative, relation) => Json.obj("id" -> id, "kind" -> "compare_moves",
      "prefix_moves_uci" -> moves, "reference_move" -> reference, "alternative_move" -> alternative) ++
      relation.fold(Json.obj())(v => Json.obj("expected_relation" -> v))
    case Uninterpreted(id, text) => Json.obj("id" -> id, "kind" -> "uninterpreted", "text" -> text)
    case Fact(id, path, predicate, at, through, expected) => Json.obj("id" -> id, "kind" -> "fact",
      "path" -> path.json, "predicate" -> predicate.json, "at" -> at.json) ++
      through.fold(Json.obj())(value => Json.obj("through" -> value.json)) ++
      expected.fold(Json.obj())(value => Json.obj("expected" -> value))
    case FactComparison(id, left, right, relation) => Json.obj("id" -> id, "kind" -> "fact_comparison",
      "left_requirement_id" -> left, "right_requirement_id" -> right, "relation" -> relation)
    case FactGroup(id, mode, members) => Json.obj("id" -> id, "kind" -> "fact_group", "mode" -> mode, "members" -> members)

  private[runtime] def snapshots(entries: List[Entry], render: CommentaryIdea.Idea => String = _.text,
      renderLine: List[String] => List[String] = identity): List[JsObject] =
    val summaries = CommentaryIdea.summaries(entries, render)
    entries.map(entry => snapshot(entry, summaries.get(entry.request.id), renderLine))

  private[runtime] def snapshot(entry: Entry, idea: Option[String] = None,
      renderLine: List[String] => List[String] = identity): JsObject =
    val origin = entry.request.origin
    Json.obj("id" -> entry.request.id, "admission" -> entry.admission,
      "origin" -> Json.obj("question" -> sourceJson(origin.question), "candidates" -> sourceJson(origin.candidates),
        "priority" -> sourceJson(origin.priority), "evidence" -> sourceJson(origin.evidence)),
      "requirements" -> entry.request.requirements.map { requirement =>
        val reading = entry.readings.find(_.requirementId == requirement.id).get
        requirementJson(requirement) ++ Json.obj("state" -> reading.state, "stop_reason" -> reading.stopReason,
          "answer" -> reading.answer.map(CommentaryInvestigation.answerJson),
          "line_id" -> reading.lineId, "occurrence_id" -> reading.occurrenceId, "position_id" -> reading.positionId,
          "work_id" -> reading.workId) ++ reading.result.fold(Json.obj())(result => Json.obj("result" -> result.json))
      }) ++ entry.questionText.fold(Json.obj())(question => Json.obj(
        "question_text" -> question, "answer_text" ->
          (idea.fold("")(_ + " ") + questionAnswer(entry, renderLine)).take(16000)))

  /** The prose reads the same facts and receipts as the structured answer. It never turns a
    * whole-line score into a causal score for one preparation or one intervening move.
    */
  private def questionAnswer(entry: Entry, renderLine: List[String] => List[String]): String =
    def label(value: CompareMoves, move: String) = renderLine(value.prefixMovesUci :+ move).last
    val requirements = entry.request.requirements.map(value => value.id -> value).toMap
    val readings = entry.readings.map(value => value.requirementId -> value).toMap
    def piece(value: CommentaryFact.Piece): String = s"${value.side} ${value.role} originally on ${value.square}"
    def position(fact: Fact, ply: Int): String = fact.path.source match
      case CommentaryFact.Declared(moves) if ply > 0 && ply <= moves.size => s"After ${renderLine(moves)(ply - 1)}"
      case _ if ply == 0 => "At the starting position"
      case _ => "In the recorded position"
    def predicate(value: CommentaryFact.Predicate): String =
      List(value.kind, value.subject.map(piece).getOrElse(""), value.target.map(piece).getOrElse(""),
        value.square.getOrElse("")).filter(_.nonEmpty).mkString(" ")
    def sampled(id: String): Option[String] = readings.get(id).filter(isResolved).flatMap(_.result)
      .flatMap(result => Option.when(result.samples.flatMap(_.value).distinct.size == 1)(result.samples.flatMap(_.value).head))
    val effects = entry.request.requirements.collect { case compare: FactComparison => compare }.flatMap { compare =>
      for
        left <- requirements.get(compare.leftRequirementId).collect { case fact: Fact => fact }
        right <- requirements.get(compare.rightRequirementId).collect { case fact: Fact => fact }
        a <- sampled(left.id)
        b <- sampled(right.id)
      yield
        val context = (left.path.source, right.path.source, left.at, right.at) match
          case (CommentaryFact.Declared(moves), CommentaryFact.Declared(other),
              CommentaryFact.Ply(before), CommentaryFact.Ply(after)) if moves == other && after == before + 1 =>
            s"${renderLine(moves)(before)} changes"
          case _ => "The named paths compare"
        s"$context ${predicate(left.predicate)}: $a / $b."
    }.distinct
    val actions = entry.request.requirements.collect { case fact: Fact if
      Set("reference_action", "alternative_action", "opponent_reference", "opponent_alternative")
        .exists(name => fact.id == name || fact.id.startsWith(name + "_")) => fact }.flatMap(fact => sampled(fact.id).map { result =>
        val at = fact.at match
          case CommentaryFact.Ply(count) => position(fact, count)
          case _ => "At the named event"
        s"$at, ${predicate(fact.predicate)} is $result."
      })
    val comparisons = entry.request.requirements.collect { case value: CompareMoves =>
      val reading = readings(value.id)
      reading.answer.fold(s"${label(value, value.alternativeMove)} versus ${label(value, value.referenceMove)}: unverified (${reading.stopReason.getOrElse(reading.state)}).")(
        answer =>
          val relation = answer.evaluationRelation match
            case "better" => "above"
            case "worse" => "below"
            case _ => "equal to"
          val location = if value.prefixMovesUci.isEmpty then "the starting position" else "the position after " + renderLine(value.prefixMovesUci).mkString(" ")
          s"${label(value, value.alternativeMove)} evaluates $relation ${label(value, value.referenceMove)} for the side to move at $location.")
    }
    val sacrificeFacts = entry.request.requirements.collect {
      case fact: Fact if fact.id.startsWith("sacrifice_") && !fact.id.endsWith("_role") => fact
    }
    val material = sacrificeFacts.flatMap { fact => sampled(fact.id).map { result =>
      fact.at match
        case event: CommentaryFact.Event =>
          val where = position(fact, event.throughPly)
          if event.kind == "capture" then
            val kind = if fact.id.startsWith("sacrifice_loss_") then "loss" else "material gain"
            val target = event.target.map(piece).getOrElse("the target")
            val role = sampled(fact.id + "_role").map(value => s" (captured as $value)").getOrElse("")
            s"$where, observed $kind: $target$role is $result by ${event.actor.map(piece).getOrElse("the capturer")}."
          else if event.kind == "check" then
            s"$where, check against ${fact.predicate.side.getOrElse("the opposing side")} is $result."
          else s"$where, promotion of ${event.actor.map(piece).getOrElse("the actor")} reaches role $result."
        case CommentaryFact.Ply(count) =>
          s"${position(fact, count)}, file ${fact.predicate.property.getOrElse("unknown")} is $result."
    }}
    val currentSacrifice = entry.request.requirements.collect {
      case fact: Fact if fact.id.startsWith("current_sacrifice_") => readings(fact.id)
    }
    val sacrificeStatus = Option.when(sacrificeFacts.nonEmpty) {
      if currentSacrifice.exists(reading => reading.state == "refuted" ||
          reading.result.exists(_.reason.contains("event_absent"))) then
        "The earlier sacrifice continuation is withdrawn as a current explanation: the returned root line no longer contains its required observations. Its exact historical observations remain available."
      else if currentSacrifice.nonEmpty && currentSacrifice.exists(reading => !Set("observed", "supported")(reading.state)) then
        "Whether the sacrifice continuation still explains the current assessment is unverified; its historical observations alone cannot establish that."
      else "The named material changes and positional observations belong to this finite continuation."
    }.toList
    val returned = entry.request.requirements.collect {
      case fact @ Fact(_, CommentaryFact.Path(_, CommentaryFact.Comparison(comparisonId, arm), _), _,
          event: CommentaryFact.Event, _, _) if !fact.id.endsWith("_role") =>
        val reading = readings(fact.id)
        val action = event.actor.map(piece).getOrElse("the actor") + event.target.map(target => " capturing " + piece(target))
          .getOrElse(event.kind match
            case "check" => " giving check"
            case "promotion" => " promoting"
            case _ => " moving to " + event.square.getOrElse("the named square"))
        val observation = if Set("observed", "supported")(reading.state) then "contains"
          else if reading.result.exists(_.reason.contains("event_absent")) then "does not contain"
          else "has not established"
        val context = requirements.get(comparisonId).collect { case comparison: CompareMoves =>
          val move = if arm == "reference" then comparison.referenceMove else comparison.alternativeMove
          val where = if comparison.prefixMovesUci.isEmpty then "the starting position"
            else "the position after " + renderLine(comparison.prefixMovesUci).mkString(" ")
          s" (${label(comparison, move)} from $where)"
        }.getOrElse("")
        s"The returned $arm line$context $observation $action in its recorded continuation."
    }.distinct
    val unresolved = entry.generatedArms.flatMap(_.headRequirementIds).distinct.filter(id => !isResolved(readings(id)))
    val limitation = if entry.request.origin.question.producer == "C-comparison-conflict" then
      "This answer compares the two divergent continuations at their exact shared position. It does not replace the earlier comparisons or the starting assessment, which require their own verification."
    else if sacrificeFacts.nonEmpty then
      "Material returns, checks and open files do not by themselves prove adequate compensation or an intended sacrifice. Evaluation compares whole continuations for the named side to move; untested declines and later defenses remain unverified."
    else if comparisons.nonEmpty then
      "Structural effects belong to the named moves. Evaluation compares whole continuations; the observed plan is conditional and may change in the returned line."
    else "The later action is conditional on the named intervening replies. Each observed change belongs to its exact move."
    val unanswered = entry.request.requirements.collect { case value: CompareMoves if readings(value.id).answer.isEmpty => value }.size
    val counterStatus = Option.when(entry.request.origin.question.producer == "C-tactical-counter") {
      val current = entry.readings.filter(_.requirementId.startsWith("tactical_current_"))
      if current.exists(reading => reading.state == "refuted" || reading.result.exists(_.reason.contains("event_absent"))) then
        "Withdraw this earlier reply explanation from the current choice: the returned root line no longer contains its required continuation. The historical comparison remains recorded."
      else if entry.readings.find(_.requirementId == "tactical_strength").exists(_.stopReason.contains("comparison_not_current")) then
        "The reply comparison must be refreshed against the current root assessment before its explanation can be retained."
      else "The reply explanation applies only to the current tested continuation."
    }.toList
    val decision = (sacrificeStatus ++ counterStatus ++ Option.when(comparisons.nonEmpty)(s"Comparison answers: ${comparisons.size - unanswered}; $unanswered remain unverified.").toList ++ Option.when(unresolved.nonEmpty)(
      s"Unresolved conditions: ${unresolved.size}; their exact scopes and stop reasons remain recorded.") :+ limitation)
      .mkString(" ")
    val answer = new StringBuilder(decision)
    val details = comparisons.distinct ++ material ++ actions ++ effects ++ returned
    var omitted = 0
    details.foreach { detail =>
      if answer.length + detail.length < 15800 then answer.append(' ').append(detail)
      else omitted += 1
    }
    if omitted > 0 then answer.append(s" $omitted further observations are omitted from this answer.")
    answer.toString
