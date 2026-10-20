package io.chesstory.runtime

import scala.collection.mutable
import lila.chessjudgment.analysis.assembly.*
import lila.chessjudgment.model.judgment.*
import play.api.libs.json.*

/** Selects admitted observations/resources, with a scoped exact-capture safety query.
  * Selection retains its premises before the deterministic wording is rendered.
  */
private[runtime] object MoveWorkProjection:
  import CommentaryEnglish.{at, roleName, sideName, name, possessiveName}
  private[runtime] def unavailableAnswer(question: Option[String], reading: Option[RootQuestionSummary.Result]): JsObject =
    question.filter(_ => reading.isEmpty).fold(Json.obj())(value =>
      Json.obj("summary_unavailable" -> Json.obj("question" -> value, "reason" -> "answer_not_delivered")))
  final case class AttackContext(lineId: String, moves: List[String],
      contacts: List[(CommentaryFact.Piece, CommentaryFact.Piece)], cause: Option[CommentaryFact.Piece] = None)
  final case class Context(text: String, qualification: Option[String] = None,
      attack: Option[AttackContext] = None, tactic: Option[CommentaryProposal.TacticStep] = None,
      tacticScope: Option[(List[String], String, Int)] = None,
      question: Option[CommentaryIdea.QuestionFocus] = None,
      // The condition's typed witness decides whether it is already covered.
      questionTest: Option[String] = None,
      additional: List[String] = Nil,
      replySpace: Option[CommentaryIdea.ReplySpace] = None,
      reserveEntry: Option[CommentaryIdea.ReserveEntry] = None,
      directReserveEntry: Option[CommentaryIdea.DirectReserveEntry] = None,
      reserveHold: Option[CommentaryIdea.ReserveHold] = None,
      reserveTiming: Option[CommentaryIdea.ReserveTiming] = None,
      reserveResponse: Option[CommentaryIdea.ReserveResponse] = None,
      replyFunction: Option[FunctionalReplyReading.Proof] = None,
      functionalChoice: Option[FunctionalChoiceReading.Task] = None,
      anchoredChoice: Option[AnchoredChoiceReading.Task] = None,
      anchoredFamily: Option[AnchoredChoiceReading.Family] = None,
      firstTarget: Option[TargetResponseInvestigation.FirstTarget] = None,
      purposeSearches: List[TargetResponseInvestigation.Search] = Nil,
      purposeRoots: List[CommentaryInvestigation.EngineCandidates] = Nil)
  private val values = Map("pawn" -> 1, "knight" -> 3, "bishop" -> 3, "rook" -> 5, "queen" -> 9, "king" -> 0)
  private[runtime] type Piece = RelationColoredPieceWitness

  private[runtime] class Position(val view: PositionResourceView):
    val sourceByRef = view.records.collect {
      case EvidenceRecord(ref, fact: RelationFactEvidence, _) => ref.id -> fact.semanticId
    }.toMap
    val facts = view.records.collect {
      case EvidenceRecord(_, fact: RelationFactEvidence, _) => fact.semanticId -> fact.detail
    }
    lazy val passageSources = view.records.collect {
      case EvidenceRecord(_, fact: RelationFactEvidence, _) if fact.kind == RelationFactKind.PawnPassage => fact.semanticId
    }.toSet
    def occupant(at: String): Option[Piece] = view.occupantAt(EvidenceSquare(at))
    def contains(p: Piece): Boolean = occupant(p.at).contains(p)
    def empty(at: String): Boolean = occupant(at).isEmpty
    val controls = facts.collect {
      case (id, RelationWitnessDetail.GeometricControl(side, from, role, to)) =>
        (RelationColoredPieceWitness(from, role, side), to.key, id)
    }.filter(row => contains(row._1)).groupBy(_._1).view.mapValues(_.map(r => r._2 -> r._3).toMap).toMap
    def controlledBy(p: Piece): Set[String] = controls.getOrElse(p, Map.empty).keySet
    lazy val rays: List[(String, RelationWitnessDetail.RayBarrier)] = facts.collect {
      case (id, detail: RelationWitnessDetail.RayBarrier) => id -> detail
    }
    /** A relative pin is an observed alignment, not a claim that the front
      * piece has no legal move. Both consumers use the same board witness. */
    def relativeAlignments(actor: Piece): List[(String, Piece, Piece)] =
      if !complete(RelationFactKind.RayBarrier) || !contains(actor) then Nil
      else for
        (id, ray) <- rays
        if actor == RelationColoredPieceWitness(ray.attackerSquare, ray.attackerRole, ray.side)
        front <- ray.occupants.headOption.toList
        rear <- ray.occupants.drop(1).headOption.toList
        if front.side == rear.side && front.side != actor.side && front.role.name != "king" &&
          rear.role.name != "king" && values(rear.role.name) > values(front.role.name)
      yield (id, front, rear)
    /** A positive obstruction witness from this exact position. Remaining
      * controllers alone do not certify occupancy of this ray. */
    def blockedRay(actor: Piece, target: Piece): Option[(String, List[Piece])] =
      if !contains(actor) || !contains(target) then None
      else rays.iterator.flatMap { (id, ray) =>
        val squares = ray.geometry.squares
        val direction = ray.geometry.direction
        val ordered = (actor.square :: squares).sliding(2).forall { pair =>
          val from = pair.head.key
          val to = pair.last.key
          to.head - from.head == direction.fileStep && to.last - from.last == direction.rankStep
        }
        val blockers = ray.occupants.takeWhile(_ != target)
        Option.when(ray.side == actor.side && ray.attackerSquare == actor.square &&
          ray.attackerRole == actor.role && ordered && squares.contains(target.square) &&
          ray.occupants.contains(target) && blockers.nonEmpty &&
          squares.flatMap(square => occupant(square.key)) == ray.occupants)(id -> blockers)
      }.nextOption()
    def sources(p: Piece, squares: List[String]): List[String] =
      squares.flatMap(controls.getOrElse(p, Map.empty).get)
    def sourceRefs(sources: List[String]): List[String] =
      val wanted = sources.toSet
      sourceByRef.iterator.collect { case (ref, source) if wanted(source) => ref }.toList.sorted
    private val controllerLookups = mutable.Map.empty[(String, chess.Color), List[Piece]]
    def controllers(square: String, side: chess.Color): List[Piece] =
      controllerLookups.getOrElseUpdate(square -> side,
        controls.iterator.collect { case (p, targets) if p.side == side && targets.contains(square) => p }.toList.sortBy(_.at))
    def complete(kind: RelationFactKind): Boolean = view.completeKinds(kind)

  private[runtime] enum Detail:
    case Development(role: String, from: String, to: String, centralSquares: List[String])
    case Castle(from: String, to: String)
    case Capture(role: String, at: String, recapture: LineMaterialRecaptureStatus)
    case Evades(target: Piece, attacker: Piece)
    case Attack(actor: Piece, target: Piece, gained: Boolean)
    case Support(actor: Piece, target: Piece, opposition: List[Piece], gained: Boolean)
    case RookConnection(actor: Piece, target: Piece)
    case PassedPawn(to: String, before: Option[Boolean])
    case PawnStructure(pawns: List[Piece], doubled: Boolean)
    case KingEntry(actor: Piece, squares: List[String])
    case PawnBlock(to: String, pawn: Piece, destination: String)
    case OpenedRay(from: String, actor: Piece, end: String)
    case ChangedLines(closed: List[(Piece, Piece, List[Piece])], opened: List[(Piece, Piece)],
        blockedSquares: List[(Piece, String, List[String])])
    case RookFile(actor: Piece, fileType: String, supported: Option[(String, List[Piece])])

    def text: String = CommentaryEnglish.description(this)
    def after(move: String, description: String = text): String = CommentaryEnglish.after(this, move, description)

  private[runtime] case class Note(kind: String, actor: Piece, detail: Detail, squares: List[String], priority: Int,
      purpose: List[String], premises: List[String], beforeSources: List[String], afterSources: List[String]):
    // Ranking, representative grouping and wording are not exact fact identity.
    def factKey = (kind, actor, detail, premises, beforeSources, afterSources)

  private[runtime] case class LocalCandidate(edge: MoveTransitionEdge, before: Position, after: Position,
      note: Note, index: Int, selectedIndex: Option[Int], selection: CommentarySelection.Display):
    lazy val id: String = CommentarySelection.stableId("fact", List(edge.evidence.id, note.factKey.toString))
    def evidence: List[String] =
      note.premises ++ before.sourceRefs(note.beforeSources) ++ after.sourceRefs(note.afterSources) :+ edge.evidence.id

    lazy val highlight: JsObject =
      val owners = Set(edge.evidence.id, before.view.occurrence.id, after.view.occurrence.id)
      val premises = note.premises.distinct.filterNot(id =>
        owners(id) || before.sourceByRef.contains(id) || after.sourceByRef.contains(id))
      def frame(phase: String, position: Position, sources: List[String]): JsObject = Json.obj(
        "phase" -> phase, "position_id" -> position.view.position.id.get,
        "squares" -> note.squares.distinct,
        "source_ids" -> (sources ++ note.premises.flatMap(position.sourceByRef.get)).distinct.sorted)
      Json.obj("occurrence_id" -> edge.evidence.id, "text" -> note.detail.text,
        "squares" -> note.squares.distinct, "premise_ids" -> premises,
        "frames" -> List(frame("before", before, note.beforeSources), frame("after", after, note.afterSources)))

    lazy val detailJson: JsObject =
      def piece(value: Piece, position: PositionNodeRef = edge.to): JsObject = Json.obj(
        "position_id" -> position.id.get, "side" -> value.sideName,
        "role" -> value.roleName, "square" -> value.at)
      val fields = note.detail match
        case Detail.Attack(actor, target, gained) =>
          Json.obj("actor" -> piece(actor), "target" -> piece(target), "gained" -> gained)
        case Detail.Support(actor, target, opposition, gained) =>
          Json.obj("actor" -> piece(actor), "target" -> piece(target),
            "opposition" -> opposition.map(p => piece(p)), "gained" -> gained)
        case Detail.Evades(target, attacker) => Json.obj("target" -> piece(target), "attacker" -> piece(attacker))
        case Detail.RookConnection(actor, target) => Json.obj("actor" -> piece(actor), "target" -> piece(target))
        case Detail.Development(role, from, to, squares) =>
          Json.obj("before_role" -> role, "from" -> from, "to" -> to, "central_squares" -> squares)
        case Detail.Castle(from, to) => Json.obj("rook_from" -> from, "rook_to" -> to)
        case Detail.Capture(role, at, recapture) => Json.obj("captured_role" -> role,
          "captured_square" -> at, "captured_position_id" -> edge.from.id.get,
          "recapture" -> (recapture match
            case LineMaterialRecaptureStatus.Proven(_) => "proven"
            case LineMaterialRecaptureStatus.Excluded => "excluded"
            case LineMaterialRecaptureStatus.Unknown => "unknown"))
        case Detail.PassedPawn(to, previous) => Json.obj("to" -> to, "previously_passed" -> previous)
        case Detail.PawnStructure(pawns, doubled) => Json.obj("pawns" -> pawns.map(p => piece(p)), "doubled" -> doubled)
        case Detail.KingEntry(_, squares) => Json.obj("controlled_squares" -> squares)
        case Detail.PawnBlock(to, pawn, destination) =>
          Json.obj("to" -> to, "pawn" -> piece(pawn), "destination" -> destination)
        case Detail.OpenedRay(from, _, end) => Json.obj("vacated_square" -> from, "end" -> end)
        case Detail.ChangedLines(closed, opened, blockedSquares) => Json.obj(
          "closed" -> closed.map((actor, target, remaining) => Json.obj(
            "actor" -> piece(actor), "target" -> piece(target), "remaining_attackers" -> remaining.map(p => piece(p)))),
          "opened" -> opened.map((actor, target) => Json.obj("actor" -> piece(actor), "target" -> piece(target))),
          "blocked_squares" -> blockedSquares.map((actor, square, _) => Json.obj("actor" -> piece(actor), "square" -> square)))
        case Detail.RookFile(_, fileType, supported) => Json.obj("file_type" -> fileType,
          "supported_square" -> supported.map(_._1), "helpers" -> supported.toList.flatMap(_._2).map(p => piece(p)))
      Json.obj("type" -> note.kind, "actor" -> piece(note.actor)) ++ fields

  /** Both wire projections use this result; callers publishing both should compute it once. */
  def select(packet: EvidenceBackedJudgmentPacket): CommentarySelection.Result =
    select(packet, ObservedLineProtocol.prepare(packet))

  def select(packet: EvidenceBackedJudgmentPacket, paths: ObservedLineProtocol.PreparedPaths): CommentarySelection.Result =
    paths.requireOwner(packet)
    paths.workSelection

  /** Only the immutable packet's base selection is shared; answers and work state are composed by the caller. */
  private[runtime] def buildSelection(packet: EvidenceBackedJudgmentPacket,
      paths: ObservedLineProtocol.PreparedPaths): CommentarySelection.Result =
    paths.requireOwner(packet)
    val positions = packet.resources.map(view => view.position -> new Position(view)).toMap
    val edges = packet.assembly.transitions.map(edge => edge.evidence.id -> edge).toMap
    val observations = packet.observations.groupBy(_.transition.evidence.id)
    val destinations = edges.toList.flatMap { (id, edge) =>
      observations.getOrElse(id, Nil).headOption.map(_.mover.to.key).orElse(
        positions.get(edge.from).flatMap(_.facts.collectFirst {
          case (_, move: RelationWitnessDetail.LegalMove) if EvidenceRef.sameMove(move.moveUci, edge.moveUci) =>
            move.destinationSquare.key
        })).map(id -> _)
    }.toMap
    // Only an already observed immediate reply makes a lost empty square relevant.
    // This index selects a local fact; comparison wording still binds the exact path.
    val replySquares = paths.paths.flatMap(_.ids.sliding(2).flatMap {
      case List(first, next) => destinations.get(next).map(first -> _)
      case _ => None
    }).groupMap(_._1)(_._2).view.mapValues(_.toSet).toMap
    val local = localCandidates(packet, positions, observations, replySquares)
    val readers = comparisonEffects(packet, local, positions, edges, paths, observations, destinations)
    CommentarySelection.fromPacket(packet, local, paths, readers.compose, readers.rootUses)

  final case class Projection(highlights: List[JsObject], selection: JsObject)

  def projectFromPacket(packet: EvidenceBackedJudgmentPacket): Projection =
    projectFromPacket(packet, ObservedLineProtocol.prepare(packet))

  def projectFromPacket(packet: EvidenceBackedJudgmentPacket, paths: ObservedLineProtocol.PreparedPaths): Projection =
    val selected = select(packet, paths)
    val selection = selected.json
    Projection(selected.highlightsFor(selection), selection)

  def selectionFromPacket(packet: EvidenceBackedJudgmentPacket): JsObject = select(packet).json

  def fromPacket(packet: EvidenceBackedJudgmentPacket): List[JsObject] =
    select(packet).highlights

  /** Research input from the same typed candidates, before either display filter.
    * This is not added to the public response and does not discover board facts.
    */
  def relationsFromPacket(packet: EvidenceBackedJudgmentPacket): List[JsObject] =
    select(packet).local.filter(c => c.note.kind == "attack" || c.note.kind == "support").map { candidate =>
      // Retain the old research shape; the shared candidate carries exact position anchors as well.
      val detail = candidate.detailJson
      def unanchored(value: JsValue): JsObject = value.as[JsObject] - "position_id"
      val relation = Json.obj("kind" -> candidate.note.kind,
        "actor" -> unanchored((detail \ "actor").get), "target" -> unanchored((detail \ "target").get),
        "gained" -> (detail \ "gained").as[Boolean]) ++
        (detail \ "opposition").asOpt[List[JsObject]].map(values =>
          Json.obj("opposition" -> values.map(_ - "position_id"))).getOrElse(Json.obj())
      candidate.highlight ++ Json.obj("relation" -> relation, "candidate_index" -> candidate.index,
        "selected_index" -> candidate.selectedIndex,
        "selection" -> (candidate.selection match
          case CommentarySelection.Display.Selected => "selected"
          case CommentarySelection.Display.DisplayCap => "display_cap"
          case _ => "purpose_dedup"))
    }

  /** A displayed explanation's references, not another fact or readiness verdict. */
  private case class FunctionalUse(offset: Int, actor: Piece)
  private case class ComparisonEffect(text: String, scope: String, lines: List[(String, Int)],
      prefix: List[String], position: PositionNodeRef, actor: Piece, target: Option[Piece], square: String,
      occurrences: List[String], evidence: List[String], use: Option[FunctionalUse] = None,
      geometricCost: Boolean = false, immediate: Boolean = false, attackPly: Option[Int] = None,
      actions: List[LocalCandidate] = Nil, lostSupport: Option[Piece] = None,
      absorbedNotes: Set[String] = Set.empty, followupNotes: List[LocalCandidate] = Nil,
      withoutSupportIntroduction: Option[String] = None,
      meaning: Option[CommentaryAnswer.Section] = None):
    private def factKey = (scope, lines, prefix, position, actor, target, square, occurrences.toSet,
      evidence.toSet, use, geometricCost, immediate, attackPly, actions.map(_.id), lostSupport,
      absorbedNotes, followupNotes.map(_.id))
    override def equals(other: Any): Boolean = other match
      case value: ComparisonEffect => factKey == value.factKey
      case _ => false
    override def hashCode: Int = factKey.hashCode
    def json(publicSource: String => String): JsObject =
      def piece(value: Piece): JsObject = Json.obj("square" -> value.at, "role" -> value.roleName,
        "side" -> value.sideName, "position_id" -> position.id.get)
      Json.obj("scope" -> scope, "line_ids" -> lines.map(_._1), "through_plies" -> lines.map(_._2),
        "prefix_moves_uci" -> prefix, "actor" -> piece(actor), "target" -> target.map(piece),
        "square" -> square, "occurrence_ids" -> occurrences.distinct, "evidence_ids" -> evidence.map(publicSource).distinct) ++
        attackPly.fold(Json.obj())(ply => Json.obj("attack_ply" -> ply))

  /** Presentation only: inspect the two paths already owned by a comparison.
    * The investigation reader's relation, priority and engine authority stay unchanged.
    */
  private case class ComparisonReaders(
      compose: (List[(JsObject, List[Context])], Set[JsObject]) => List[JsObject],
      rootUses: List[ObservedLineProtocol.PreparedPath] => List[(CommentaryAnswer.Section, List[String])])

  private def comparisonEffects(packet: EvidenceBackedJudgmentPacket, local: List[LocalCandidate],
      positions: Map[PositionNodeRef, Position], edges: Map[String, MoveTransitionEdge],
      paths: ObservedLineProtocol.PreparedPaths, observations: Map[String, List[MoveObservation]],
      destinations: Map[String, String]): ComparisonReaders =
    def sans(path: ObservedLineProtocol.PreparedPath): List[String] = paths.notation.line(path.moves)
    def render(section: CommentaryAnswer.Section): String = CommentaryEnglish.section(section).map {
      case text: String => text
      case move: RootQuestionSummary.MoveRef => sans(move.path)(move.offset)
    }.mkString
    def label(edge: MoveTransitionEdge): String = paths.paths.find(_.ids.contains(edge.evidence.id))
      .map(path => sans(path)(path.ids.indexOf(edge.evidence.id))).getOrElse("?")
    val byOccurrence = local.groupBy(_.edge.evidence.id)
    def publicSource(id: String): String = packet.evidenceGraph.byId.get(id) match
      case Some(EvidenceRecord(_, fact: RelationFactEvidence, _)) if fact.isPositionRelation => fact.semanticId
      case _ => id
    def transported(piece: Piece, edge: MoveTransitionEdge, after: Position): Option[Piece] =
      if after.contains(piece) then Some(piece)
      else observations.getOrElse(edge.evidence.id, Nil).flatMap { observation =>
        val mover = observation.mover
        val moved = Option.when(piece == RelationColoredPieceWitness(mover.from, mover.beforeRole, mover.side))(
          RelationColoredPieceWitness(mover.to, mover.afterRole, mover.side))
        val rook = observation.meaning match
          case MoveObservationMeaning.Castling(rook) if piece.roleName == "rook" &&
              piece.side == mover.side && piece.square == rook.from =>
            Some(RelationColoredPieceWitness(rook.to, piece.role, piece.side))
          case _ => None
        moved.orElse(rook)
      }.find(after.contains)
    def sourceRefs(position: Position, sources: List[String]): List[String] =
      position.sourceRefs(sources)
    def noteEvidence(note: LocalCandidate): List[String] =
      note.evidence
    // Current support/evasion is already read locally. Only an actual use in
    // this same certified arm extends it; a plan name or another arm cannot.
    lazy val continuations = paths.continuationReadings.map(reading => reading.path.line.ref.id -> reading).toMap
    val currentEffectReadings = mutable.Map.empty[(String, Int), List[ComparisonEffect]]
    def currentEffects(path: ObservedLineProtocol.PreparedPath, index: Int): List[ComparisonEffect] =
      currentEffectReadings.getOrElseUpdate(path.line.ref.id -> index, {
        if path.certified.isEmpty || path.ids != path.declared || path.moves.size <= index then Nil
        else
          import CommentaryAnswer.*
          import RootQuestionSummary.MoveRef
          val notes = path.ids.map(id => byOccurrence.getOrElse(id, Nil)).toVector
          lazy val identities = paths.identitiesAfter(packet, path, index + 1)
          def original(piece: Piece): Option[Piece] = identities.collectFirst {
            case (root, Some(Some(current))) if current == piece => root
          }
          def tracked(piece: Piece, count: Int): Option[Piece] =
            paths.pieceAfter(packet, path, piece, count).flatten
          def movement(offset: Int): Option[(Piece, Piece, List[String])] = for
            edge <- edges.get(path.ids(offset))
            before <- positions.get(edge.from)
            after <- positions.get(edge.to)
            to <- destinations.get(edge.evidence.id)
            from = observations.getOrElse(edge.evidence.id, Nil).headOption
              .map(_.mover.from.key).getOrElse(edge.moveUci.take(2))
            prior <- before.occupant(from)
            next <- after.occupant(to).filter(_.side == prior.side)
          yield
            val legal = before.facts.collect {
              case (id, value: RelationWitnessDetail.LegalMove)
                  if EvidenceRef.sameMove(value.moveUci, edge.moveUci) => id
            }
            (prior, next, List(edge.evidence.id, before.view.occurrence.id, after.view.occurrence.id) ++
              sourceRefs(before, legal))
          def effect(meaning: Section, actor: Piece, target: Option[Piece], square: String, through: Int,
              offsets: List[Int], evidence: List[String]): ComparisonEffect =
            ComparisonEffect(render(meaning), "line_history", List(path.line.ref.id -> through), path.moves.take(index),
              path.positions(index + 1), actor, target, square, offsets.map(path.ids),
              (evidence ++ path.certified.map(_._1.ref.id)).distinct, meaning = Some(meaning))
          val currentMove = movement(index)
          def position(count: Int): Option[Position] = path.positions.lift(count).flatMap(positions.get)
          def originalAt(piece: Piece, count: Int): Option[Piece] =
            paths.identitiesAfter(packet, path, count).collectFirst {
              case (root, Some(Some(current))) if current == piece => root
            }
          def ray(position: Position, slider: Piece, end: String): Option[(String, List[String])] =
            position.rays.collectFirst {
              case (id, RelationWitnessDetail.RayBarrier(side, from, role, _, geometry))
                  if slider == RelationColoredPieceWitness(from, role, side) &&
                    geometry.squares.exists(_.key == end) =>
                id -> geometry.squares.map(_.key).takeWhile(_ != end).filterNot(_ == slider.at)
            }
          // A function uses the same physical actors and certified path as all
          // other effects. Every intervening position owns its continuity proof.
          def functionEffect(scope: String, meaning: Section, actor: Piece, target: Piece, square: String,
              through: Int, sources: List[String]): ComparisonEffect =
            effect(meaning, actor, Some(target), square, through, (index until through).toList,
              sources ++ movement(index).toList.flatMap(_._3) ++ movement(through - 1).toList.flatMap(_._3) ++
                (index to through).flatMap(count => position(count).map(_.view.occurrence.id)) ++
                path.ids.slice(index, through)).copy(scope = scope,
                  use = movement(through - 1).map((_, user, _) => FunctionalUse(through - 1, user)))
          def includeSupport(value: ComparisonEffect, helper: Piece, target: Piece): ComparisonEffect =
            val covered = notes(index).filter(_.note.detail match
              case Detail.Support(actor, victim, _, _) => actor == helper && victim == target
              case _ => false)
            val current = value.meaning.get
            val move = MoveRef(path, index)
            val meaning = current.copy(statements = covered.map(note => Action(move, note.note.detail)) ++ current.statements,
              focus = (current.focus ++ covered.filter(_.selection == CommentarySelection.Display.Selected)
                .map(note => Focus(move, note))).distinct)
            value.copy(text = render(meaning), meaning = Some(meaning),
              evidence = (value.evidence ++ covered.flatMap(noteEvidence)).distinct,
              absorbedNotes = covered.map(_.id).toSet,
              withoutSupportIntroduction = Option.when(covered.nonEmpty)(value.text))
          lazy val recaptures = continuations.get(path.line.ref.id).filter(_.path eq path).toList.flatMap { reading =>
            val events = reading.events.map(event => event.id -> event).toMap
            reading.captureLinks.flatMap(link => for
              loss <- events.get(link.lossEventId)
              recovery <- events.get(link.captureEventId)
              if loss.plyOffset > index && recovery.plyOffset > loss.plyOffset
            yield loss -> recovery)
          }
          val handoffs = (for
            (_, mover, _) <- currentMove.toList
            rootMover <- original(mover).toList
            initial <- position(index).toList
            (loss, recovery) <- recaptures
            provider <- recovery.originalActor.toList
            victim <- loss.originalVictim.filter(_.side == mover.side).toList
            capturer <- loss.originalActor.toList
            if recovery.originalVictim.contains(capturer)
            initialVictim <- tracked(victim, index).toList
            target <- tracked(victim, index + 1).toList
            helper <- tracked(provider, index + 1).toList
            if helper.side == mover.side && helper != target
            oldProviders = initial.controllers(initialVictim.at, mover.side).flatMap(old =>
              originalAt(old, index).filter(identity => identity != provider &&
                (rootMover == provider || rootMover == identity)).map(old -> _))
            if oldProviders.nonEmpty
            if (index + 1 to loss.plyOffset).forall(count =>
              paths.supportAt(packet, path, count, provider, victim).state == "present")
            use <- LineContinuationInvestigation.directSupportUse(packet, path, paths, recovery.plyOffset + 1,
              provider, victim).uses.find(use => use.targetCapture.id == loss.id && use.recapture.id == recovery.id).toList
            afterLoss <- position(loss.plyOffset + 1).toList
            capturedBy <- tracked(capturer, loss.plyOffset + 1).toList
            // A later unpinning/check response must not be attributed to the
            // original support move. The recapture was already legal at the loss.
            legal <- afterLoss.view.legalRows.toList.flatten.find(row =>
              row.actor == use.provider && row.capture.contains(capturedBy)).toList
            (old, oldIdentity) <- oldProviders
            departure <- (index + 1 to loss.plyOffset).find { count =>
              val state = paths.supportAt(packet, path, count, oldIdentity, victim).state
              Set("absent", "actor_removed")(state) &&
                paths.supportAt(packet, path, count - 1, oldIdentity, victim).state == "present" &&
                tracked(oldIdentity, count - 1).nonEmpty &&
                tracked(oldIdentity, count) != tracked(oldIdentity, count - 1)
            }.toList
            if rootMover == provider || departure == index + 1
            if (departure to loss.plyOffset).forall(count =>
              Set("absent", "actor_removed")(paths.supportAt(packet, path, count, oldIdentity, victim).state))
          yield
            val supportSources = (index to loss.plyOffset).toList.flatMap { count =>
              position(count).toList.flatMap(p => sourceRefs(p,
                List(provider, oldIdentity).flatMap(actor => paths.supportAt(packet, path, count, actor, victim).sourceIds)))
            }
            val meaning = Section(List(SupportTransfer(MoveRef(path, index), helper, target, old, oldIdentity,
              MoveRef(path, departure - 1), use)))
            includeSupport(functionEffect("support_handoff", meaning, mover, target, target.at, recovery.plyOffset + 1,
              supportSources ++ sourceRefs(initial, initial.sources(old, List(initialVictim.at))) ++
                sourceRefs(afterLoss, List(legal.sourceId)) ++ movement(loss.plyOffset).toList.flatMap(_._3)), helper, target))
            .distinctBy(effect => (effect.target, effect.lines))
          // Start from an actual use, not every possible future target. A new
          // support can follow a moving ally or await its arrival on an empty square.
          val supportUses = for
            (prior, mover, _) <- currentMove.toList
            provider <- original(mover).toList
            (loss, recovery) <- recaptures
            if recovery.originalActor.contains(provider)
            victim <- loss.originalVictim.filter(_.side == mover.side).toList
            target <- tracked(victim, index + 1).toList
            if !handoffs.exists(value => value.target.contains(target) && value.lines == List(path.line.ref.id -> (recovery.plyOffset + 1)))
            use <- LineContinuationInvestigation.directSupportUse(packet, path, paths, recovery.plyOffset + 1,
              provider, victim).uses.find(value => value.targetCapture.id == loss.id && value.recapture.id == recovery.id).toList
            if (index + 1 to loss.plyOffset).forall(count => tracked(provider, count).contains(mover))
            initial <- position(index).toList
            afterLoss <- position(loss.plyOffset + 1).toList
            capturedBy <- tracked(use.originalCapturer, loss.plyOffset + 1).toList
            legal <- afterLoss.view.legalRows.toList.flatten.find(row =>
              row.actor == use.provider && row.capture.contains(capturedBy)).toList
            supportedFromStart = paths.supportAt(packet, path, index, provider, victim).state == "absent" &&
              (index + 1 to loss.plyOffset).forall(count =>
                paths.supportAt(packet, path, count, provider, victim).state == "present")
            square = use.target.at
            arrival = (index + 1 to loss.plyOffset).find(count => tracked(victim, count).exists(_.at == square))
            preparedSquare = arrival.exists { count =>
              count > index + 1 && initial.complete(RelationFactKind.GeometricControl) &&
                !initial.controlledBy(prior)(square) &&
                (index + 1 until count).forall(at => position(at).exists(p =>
                  p.empty(square) && p.controlledBy(mover)(square))) &&
                (count to loss.plyOffset).forall(at => tracked(victim, at).exists(_.at == square) &&
                  paths.supportAt(packet, path, at, provider, victim).state == "present")
            }
            if supportedFromStart || preparedSquare
          yield
            val arrivalCount = if supportedFromStart then index + 1 else arrival.get
            val controlSources = (index + 1 to loss.plyOffset).toList.flatMap { count =>
              position(count).toList.flatMap(p => sourceRefs(p,
                if count < arrivalCount then p.sources(mover, List(square))
                else paths.supportAt(packet, path, count, provider, victim).sourceIds))
            }
            val arrivalCapture = arrival.filter(_ > index + 1).flatMap(count => notes(count - 1).find(_.note.detail match
              case Detail.Capture(_, _, _) => true
              case _ => false))
            val move = MoveRef(path, index)
            val introduction = Option.when(supportedFromStart && !notes(index).exists(_.note.detail match
              case Detail.Support(helper, victim, _, _) => helper == mover && victim == target
              case _ => false))(Action(move, Detail.Support(mover, target, Nil, true))).toList
            val meaning = Section(introduction :+ SupportedCapture(move, mover, target, use, supportedFromStart,
              if !supportedFromStart then Some(MoveRef(path, arrivalCount - 1))
              else Option.when(target != use.target)(MoveRef(path, arrival.get - 1)),
              arrivalCapture.map(_.note.detail)))
            includeSupport(functionEffect("support_use", meaning, mover, target, square, recovery.plyOffset + 1,
              controlSources ++ arrivalCapture.toList.flatMap(noteEvidence) ++ sourceRefs(afterLoss, List(legal.sourceId)) ++
                movement(loss.plyOffset).toList.flatMap(_._3)), mover, target)
          // Start from the current mover's gained support, not every future pair.
          // The old provider's first departure bounds the claim to this interval;
          // geometry alone does not establish a legal recapture or target safety.
          val retainedSupport = for
            (_, mover, _) <- currentMove.toList
            provider <- original(mover).toList
            note <- notes(index)
            target <- (note.note.detail match
              case Detail.Support(actor, target, _, true) if actor == mover => List(target)
              case _ => Nil)
            if !(handoffs ++ supportUses).exists(_.target.contains(target))
            victim <- original(target).toList
            if paths.supportAt(packet, path, index, provider, victim).state == "absent"
            initial <- position(index).toList
            initialTarget <- tracked(victim, index).toList
            old <- initial.controllers(initialTarget.at, mover.side)
            oldIdentity <- originalAt(old, index).filter(_ != provider).toList
            departure <- (index + 1 to path.moves.size).find(count =>
              paths.supportAt(packet, path, count, oldIdentity, victim).state != "present").toList
            if departure > index + 1 &&
              paths.supportAt(packet, path, departure, oldIdentity, victim).state == "absent" &&
              (index + 1 to departure).forall(count =>
                paths.supportAt(packet, path, count, provider, victim).state == "present")
            (beforeOld, afterOld, _) <- movement(departure - 1).toList
            if tracked(oldIdentity, departure - 1).contains(beforeOld) && tracked(oldIdentity, departure).contains(afterOld)
            nowTarget <- tracked(victim, departure).toList
          yield
            val sources = (index to departure).toList.flatMap(count => position(count).toList.flatMap(p =>
              sourceRefs(p, List(provider, oldIdentity).flatMap(actor =>
                paths.supportAt(packet, path, count, actor, victim).sourceIds))))
            val meaning = Section(List(SupportRetention(MoveRef(path, index), provider, mover, victim, target,
              oldIdentity, old, MoveRef(path, departure - 1), nowTarget)))
            includeSupport(functionEffect("support_retention", meaning, mover, target, target.at, departure, sources), mover, target)
              // Losing an old duty is not an actual use enabling this move's attack/check.
              .copy(use = None)
          val blockerHandoffs = for
            (prior, mover, _) <- currentMove.toList
            rootMover <- original(mover).toList
            initial <- position(index).toList
            offset <- (index + 1 until path.moves.size).toList
            (_, entry, _) <- movement(offset).toList
            (rayId, detail) <- initial.rays
            slider = RelationColoredPieceWitness(detail.attackerSquare, detail.attackerRole, detail.side)
            if slider.side != entry.side && initial.contains(slider) && detail.geometry.squares.exists(_.key == entry.at)
            between = detail.geometry.squares.map(_.key).takeWhile(_ != entry.at).filterNot(_ == slider.at)
            if between.contains(mover.at) && !between.contains(prior.at)
            old = between.flatMap(initial.occupant)
            if old.nonEmpty
            originalSlider <- originalAt(slider, index).toList
            if (index + 1 to offset + 1).forall(count =>
              tracked(rootMover, count).contains(mover) && tracked(originalSlider, count).contains(slider))
            beforeUse <- position(offset).toList
            if between.flatMap(beforeUse.occupant) == List(mover)
            departure <- (index + 1 to offset).find(count => position(count).exists(p =>
              between.flatMap(p.occupant) == List(mover))).toList
            if (departure to offset + 1).forall(count => position(count).exists(p =>
              between.flatMap(p.occupant) == List(mover) && p.complete(RelationFactKind.GeometricControl) &&
                !p.controlledBy(slider)(entry.at)))
            last <- position(offset + 1).toList
            finalRay <- ray(last, slider, entry.at).toList
          yield
            val meaning = Section(List(BlockedLineTransfer(MoveRef(path, index), rootMover, mover,
              originalSlider, slider, MoveRef(path, departure - 1), MoveRef(path, offset), entry)))
            functionEffect("blocker_handoff", meaning, mover, slider, entry.at, offset + 1,
              sourceRefs(initial, List(rayId)) ++ sourceRefs(last, List(finalRay._1)))
          val usedCorridors = mutable.Set.empty[(Piece, Option[Piece], Option[String])]
          val corridorUses = for
            (prior, mover, _) <- currentMove.toList
            offset <- (index + 1 until path.moves.size).toList
            (beforeUser, user, _) <- movement(offset).toList
            if user.side == mover.side && Set("bishop", "rook", "queen")(user.roleName)
            userIdentity <- originalAt(user, offset + 1).toList
            if !original(mover).contains(userIdentity)
            anchoredUser <- tracked(userIdentity, index + 1).toList
            beforeUse <- position(offset).toList
            afterUse <- position(offset + 1).toList
            // Passing through the cleared square, or using a newly occupied
            // diagonal/file toward a real piece, are both concrete corridor uses.
            uses = ray(beforeUse, beforeUser, user.at).filter(_._2.contains(prior.at))
              .map((id, squares) => (beforeUse, List(id), squares, Option.empty[Piece])).toList ++
              afterUse.controlledBy(user).toList.sorted.flatMap(end => for
                target <- afterUse.occupant(end).toList
                if target.side != user.side || notes(offset).exists(_.note.detail match
                  case Detail.Support(actor, supported, opposition, true) =>
                    actor == user && supported == target && opposition.nonEmpty
                  case _ => false)
                (id, squares) <- ray(afterUse, user, end).filter(_._2.contains(prior.at)).toList
              yield (afterUse, id :: afterUse.sources(user, List(end)), squares, Some(target)))
            continuous = uses.filter { (_, _, corridor, _) =>
              (index + 1 to offset + 1).forall(count => position(count).exists(p =>
                p.empty(prior.at) && corridor.forall(square => p.occupant(square).forall(piece =>
                  tracked(userIdentity, count).contains(piece)))))
            }
             // A route can require more than its first clearance. Track only
             // the original blockers: none may return, and no replacement may
             // enter the corridor. This is a witnessed dependency, not a claim
             // that the later activity is forced or that every PV move helps it.
             prepared = if index != 0 || offset >= 18 then Nil else uses.filter(_._4.isEmpty).flatMap { use =>
               val corridor = (use._3 :+ user.at).distinct
               val initial = position(index).get
               val blockers = corridor.flatMap(initial.occupant).filterNot(_ == beforeUser).distinct
               val identities = blockers.flatMap(originalAt(_, index))
               def remains(piece: Piece, count: Int) = tracked(piece, count).exists(p => corridor.contains(p.at))
               val steps = (index until offset).filter(n => identities.exists(piece =>
                 remains(piece, n) && tracked(piece, n) != tracked(piece, n + 1))).toList
               val certified = blockers.contains(prior) && blockers.forall(_.side == mover.side) &&
                 identities.size == blockers.size && steps.headOption.contains(index) && steps.size > 1 &&
                 (index to offset).forall(count => tracked(userIdentity, count).contains(beforeUser) &&
                   position(count).exists(p => p.complete(RelationFactKind.GeometricControl) &&
                     corridor.flatMap(p.occupant).forall(piece => piece == beforeUser ||
                       identities.exists(identity => tracked(identity, count).contains(piece))))) &&
                 (index + 1 to offset).forall(count => position(count).exists(_.empty(prior.at))) &&
                 identities.forall { identity =>
                   val left = (index + 1 to offset).find(count => !remains(identity, count))
                   left.exists(count => (count to offset).forall(n => !remains(identity, n)))
                 }
               Option.when(certified)(use -> steps)
             }
            fresh = continuous.filter { (_, _, _, target) =>
              usedCorridors.add((userIdentity, target.flatMap(originalAt(_, offset + 1)), target.map(_.roleName)))
            }
            actions = notes(offset).filter(_.note.detail match
              case Detail.Attack(actor, _, true) => actor == user
              case Detail.Support(actor, _, opposition, true) => actor == user && opposition.nonEmpty
              case _ => false)
            checks = observations.getOrElse(path.ids(offset), Nil).exists(_.meaning match
              case MoveObservationMeaning.CertifiedRelation(_: RelationWitnessDetail.CreatedCheckResponseInventory) => true
              case _ => false)
            used = if fresh.nonEmpty then fresh else if actions.nonEmpty || checks then continuous else Nil
             chosen <- if used.nonEmpty then List(used -> List.empty[Int]) else
               prepared.take(1).filter(_ => usedCorridors.add((userIdentity, None, None)))
                 .map((use, steps) => List(use) -> steps)
          yield
            val development = notes(index).collectFirst {
              case note if note.note.detail.isInstanceOf[Detail.Development] =>
                note.note.detail
            }
            val meaning = Section(List(ClearedRoute(MoveRef(path, index), prior, mover, userIdentity,
               MoveRef(path, offset), user, chosen._1.flatMap(_._4).distinct, chosen._1.exists(_._4.isEmpty), development,
               chosen._2.map(MoveRef(path, _)))))
            functionEffect("clearance_use", meaning, mover, anchoredUser, prior.at, offset + 1,
               chosen._1.flatMap(use => sourceRefs(use._1, use._2)) ++ actions.flatMap(noteEvidence) ++
                 chosen._2.flatMap(n => path.ids.lift(n))).copy(actions = actions)
          // Follow the same piece through quiet intermediate posts to its first
          // useful move. An intervening exchange ends this physical route.
          val routes = for
            (_, mover, _) <- currentMove.toList
            rootMover <- original(mover).toList
            route = (index + 1 until math.min(path.moves.size, 18)).filter(at =>
              movement(at).exists((_, next, _) => tracked(rootMover, at + 1).contains(next))).toList
            selectedUse = Option.when(index == 0)(paths.functionalPurpose).flatten
              .filter(q => q.actor == rootMover && q.path.moves.head == path.moves.head)
              .flatMap(_.plan).filter(p => !p.potential && (p.path eq path)).map(_.use).filter(route.contains)
            offset <- selectedUse.orElse(route.find(at => movement(at).exists { (_, next, _) =>
              notes(at).exists(_.note.detail match
                case Detail.Attack(actor, _, true) => actor == next
                case Detail.Support(actor, _, opposition, true) => actor == next && opposition.nonEmpty
                case _ => false)
            })).toList
            (_, user, _) <- movement(offset).toList
            if user.role == mover.role && (index + 1 to offset + 1).forall(count => tracked(rootMover, count).nonEmpty)
            if (index + 1 to offset + 1).forall(count =>
              tracked(rootMover, count).forall(_.at != path.moves(index).take(2)))
            note <- notes(offset).filter(_.note.detail match
              case Detail.Attack(actor, _, true) => actor == user
              case Detail.Support(actor, _, opposition, true) => actor == user && opposition.nonEmpty
              case _ => false)
            related <- (note.note.detail match
              case Detail.Attack(_, target, _) => Some(target)
              case Detail.Support(_, target, _, _) => Some(target)
              case _ => None).toList
            relatedIdentity <- originalAt(related, offset + 1).toList
            anchoredTarget <- tracked(relatedIdentity, index + 1).toList
            // A renewed attack already belongs to the current attack's history.
            if !notes(index).exists(_.note.detail match
              case Detail.Attack(actor, target, _) => actor == mover && target == anchoredTarget
              case _ => false)
          yield
            val lostSupport = Option.when(related.side == mover.side &&
              paths.supportAt(packet, path, index, rootMover, relatedIdentity).state == "present" &&
              paths.supportAt(packet, path, index + 1, rootMover, relatedIdentity).state == "absent")(anchoredTarget)
            val earlierSources = (index to index + 1).toList.flatMap(count => position(count).toList.flatMap(p =>
              sourceRefs(p, paths.supportAt(packet, path, count, rootMover, relatedIdentity).sourceIds)))
            val evasion = notes(offset).filter(_.note.detail match
              case Detail.Evades(target, _) => target == user
              case _ => false)
            val actions = note :: evasion
            functionEffect("piece_route", Section(List(PieceRoute(path, rootMover, index :: route.takeWhile(_ <= offset)))),
              mover, anchoredTarget, user.at, offset + 1, earlierSources ++ actions.flatMap(noteEvidence))
                .copy(actions = actions, lostSupport = lostSupport)
          val functions = handoffs ++ supportUses ++ retainedSupport ++
            blockerHandoffs.distinctBy(e => (e.target, e.square, e.use)) ++ corridorUses ++ routes
          def at(offset: Int) = observations.getOrElse(path.ids(offset), Nil)
          def givesCheck(offset: Int): Boolean = offset < path.moves.size && at(offset).exists(_.meaning match
            case MoveObservationMeaning.CertifiedRelation(_: RelationWitnessDetail.CreatedCheckResponseInventory) => true
            case _ => false)
          // Follow an observed check and its next checking turn, or a function's
          // exact use. Unrelated later checks are not seeds for this explanation.
          val supportLosses = functions.filter(value => Set("support_use", "support_handoff")(value.scope)).flatMap { value =>
            recaptures.collect {
              case (loss, recovery) if value.use.exists(_.offset == recovery.plyOffset) &&
                  loss.originalVictim.flatMap(tracked(_, index + 1)) == value.target => loss.plyOffset
            }
          }
          val checkSeeds = (List(index, index + 1) ++ supportLosses ++ functions.flatMap(_.use.map(_.offset)) ++ (if index == 0 then
            path.certified.flatMap(_._2.material).flatMap(CommentarySelection.openingCaptureChain)
              .toList.flatMap(_.map(_.plyOffset)) else Nil)).filter(_ < path.moves.size).distinct.sorted
          val checkOffsets = checkSeeds.flatMap(start =>
            Iterator.iterate(start)(_ + 2).takeWhile(givesCheck).toList).distinct.sorted
          val checkStepsInOrder = checkSteps(path, checkOffsets, Some(index), observations)
          val checkSequence = for
            (_, actor, movementEvidence) <- currentMove.toList
            first <- checkStepsInOrder.headOption.toList
            king <- identities.collectFirst {
              case (_, Some(Some(p))) if p.side == first._4 && p.roleName == "king" => p
            }.toList
          yield
            val offsets = (index :: checkStepsInOrder.map(_._1)).distinct.sorted
            val meaning = Section(List(CheckSequence(MoveRef(path, index), checkStepsInOrder.map(_._2))),
              required = offsets.map(MoveRef(path, _)))
            effect(meaning, actor, Some(king), king.at, offsets.last + 1, offsets,
              movementEvidence ++ checkStepsInOrder.flatMap(_._3)).copy(scope = "check_sequence")
          val immediate = notes(index).flatMap { note =>
            note.note.detail match
              case Detail.Support(actor, target, _, gained) if gained ||
                  currentMove.exists((_, mover, _) => mover == actor || mover == target) =>
                val move = MoveRef(path, index)
                val meaning = Section(List(Action(move, note.note.detail)), focus =
                  Option.when(note.selection == CommentarySelection.Display.Selected)(Focus(move, note)).toList)
                List(effect(meaning, actor, Some(target), target.at,
                  index + 1, List(index), noteEvidence(note)).copy(actions = List(note)))
              case Detail.Evades(target, attacker) =>
                val later = for
                  originalTarget <- original(target)
                  originalAttacker <- original(attacker)
                  used <- (index + 1 until path.moves.size).iterator.flatMap { offset =>
                    notes(offset).collectFirst {
                      case next if (next.note.detail match
                        case Detail.Attack(actor, victim, gained) =>
                          tracked(originalTarget, offset + 1).contains(actor) &&
                            tracked(originalAttacker, offset + 1).contains(victim) &&
                            (gained || movement(offset).exists((_, mover, _) => mover == actor))
                        case _ => false) => offset -> next
                    }
                  }.nextOption()
                yield
                  val (offset, use) = used
                  val route = (index + 1 to offset).flatMap { at =>
                    movement(at).filter((_, next, _) => tracked(originalTarget, at + 1).contains(next))
                      .map(value => at -> value._3)
                  }.toList
                  (offset, use, route)
                // The general next-move route already introduces this same use.
                // Keep this evasion and its dependency, without introducing it twice.
                val unintroduced = later.filterNot { (offset, use, _) => routes.exists(value =>
                  value.actor == target && value.target.contains(attacker) &&
                    value.use.contains(FunctionalUse(offset, use.note.actor)))
                }
                val move = MoveRef(path, index)
                val continuation = unintroduced.toList.flatMap { (offset, use, route) =>
                  Option.when(route.nonEmpty)(PieceRoute(path, original(target).get,
                    index :: route.map(_._1))).toList :+ Action(MoveRef(path, offset), use.note.detail)
                }
                val meaning = Section(Action(move, note.note.detail) :: continuation, focus =
                  Option.when(note.selection == CommentarySelection.Display.Selected)(Focus(move, note)).toList)
                List(effect(meaning, target, Some(attacker), attacker.at, later.fold(index + 1)(_._1 + 1),
                  List(index) ++ later.toList.flatMap((offset, _, route) => route.map(_._1) :+ offset),
                  noteEvidence(note) ++ later.toList.flatMap((_, use, route) => noteEvidence(use) ++ route.flatMap(_._2)))
                    .copy(actions = List(note), use = later.map((offset, use, _) => FunctionalUse(offset, use.note.actor))))
              case _ => Nil
          }
          val clearance = for
            (prior, actor, firstEvidence) <- currentMove.toList
            after <- positions.get(path.positions(index + 1)).toList
            if after.empty(prior.at)
            // Stop at the first reoccupation, including an unknown position.
            // A later departure must not donate its clearance to this move.
            count <- (index + 2 to path.moves.size).find(count =>
              positions.get(path.positions(count)).forall(position => !position.empty(prior.at))).toList
            (_, entry, entryEvidence) <- movement(count - 1).toList
            if entry.at == prior.at && entry.side == actor.side
            originalActor <- original(actor).toList
            originalEntry <- paths.identitiesAfter(packet, path, count).collectFirst {
              case (root, Some(Some(current))) if current == entry => root
            }.toList
            if originalEntry != originalActor
            initialEntry <- tracked(originalEntry, index + 1).toList
            // A bare stop on the square adds no new function after this same
            // piece already used the corridor. A new attack/support/check is retained.
            if !corridorUses.exists(_.target.contains(initialEntry)) || givesCheck(count - 1) ||
              notes(count - 1).exists(_.note.detail match
                case Detail.Attack(actor, _, true) => actor == entry
                case Detail.Support(actor, _, opposition, true) => actor == entry && opposition.nonEmpty
                case _ => false)
          yield
            val evasions = notes(count - 1).filter(_.note.detail match
              case Detail.Evades(target, _) => target == entry
              case _ => false)
            val meaning = Section(List(SquareReoccupation(MoveRef(path, index), originalActor, actor,
              prior.at, MoveRef(path, count - 1), originalEntry, entry)) ++
              evasions.map(note => Action(MoveRef(path, count - 1), note.note.detail)))
            val emptyPositions = (index + 1 until count).flatMap(at => positions.get(path.positions(at)).map(_.view.occurrence.id))
            effect(meaning, actor, Some(initialEntry), prior.at, count, List(index, count - 1),
              firstEvidence ++ entryEvidence ++ emptyPositions ++ evasions.flatMap(_.evidence))
              .copy(use = Some(FunctionalUse(count - 1, entry)), actions = evasions)
          checkSequence ++ functions ++ immediate.map(_.copy(immediate = true)) ++ clearance
      })
    // Compare the same physical task in the two already selected paths. Seeds
    // are actual root pawn moves/captures and the current slider/king's rays;
    // unchanged relations elsewhere on the board are not new explanation work.
    val meaningReadings = mutable.Map.empty[(List[String], Int), List[ComparisonEffect]]
    def meaningEffects(pair: List[ObservedLineProtocol.PreparedPath], index: Int): List[ComparisonEffect] =
      meaningReadings.getOrElseUpdate(pair.map(_.line.ref.id) -> index, {
        if pair.size != 2 || pair.exists(p => p.certified.isEmpty || p.ids != p.declared || p.moves.size <= index) then Nil
        else
          import CommentaryAnswer.*
          import RootQuestionSummary.MoveRef
          def pos(path: ObservedLineProtocol.PreparedPath, count: Int) = positions(path.positions(count))
          def original(path: ObservedLineProtocol.PreparedPath, piece: Piece, count: Int): Option[Piece] =
            paths.identitiesAfter(packet, path, count).collectFirst {
              case (root, Some(Some(current))) if current == piece => root
            }
          def track(path: ObservedLineProtocol.PreparedPath, piece: Piece, anchor: Int, count: Int): Option[Piece] =
            original(path, piece, anchor).flatMap(paths.pieceAfter(packet, path, _, count).flatten)
          def notes(path: ObservedLineProtocol.PreparedPath, offset: Int) = byOccurrence.getOrElse(path.ids(offset), Nil)
          def mover(path: ObservedLineProtocol.PreparedPath, offset: Int) =
            pos(path, offset).occupant(path.moves(offset).take(2))
          def advance(position: Position, pawn: Piece, square: String) = position.facts.collectFirst {
            case (id, a: RelationWitnessDetail.PawnAdvanceAffordance)
                if a.side == pawn.side && a.pawnSquare == pawn.square && a.destinationSquare.key == square => id
          }
          def evidence(path: ObservedLineProtocol.PreparedPath, through: Int): List[String] =
            path.ids.slice(index, through) ++ (index to through).map(pos(path, _).view.occurrence.id) ++
              (index until through).flatMap { offset =>
                val before = pos(path, offset)
                sourceRefs(before, before.facts.collect {
                  case (id, move: RelationWitnessDetail.LegalMove)
                      if EvidenceRef.sameMove(move.moveUci, path.moves(offset)) => id
                })
              } ++ path.certified.map(_._1.ref.id)
          def effect(scope: String, meaning: Section, arms: List[(ObservedLineProtocol.PreparedPath, Int)],
              anchor: Int, actor: Piece, target: Option[Piece], square: String, sources: List[String],
              absorbed: Set[String] = Set.empty): ComparisonEffect =
            ComparisonEffect(render(meaning), scope, arms.map((p, n) => p.line.ref.id -> n), pair.head.moves.take(index),
              arms.head._1.positions(anchor), actor, target, square,
              arms.flatMap((p, n) => p.ids.slice(index, n)),
              arms.flatMap((p, n) => evidence(p, n)) ++ sources, absorbedNotes = absorbed, meaning = Some(meaning))
          val comparisons = List(pair.head -> pair.last, pair.last -> pair.head).flatMap { (path, peer) =>
            val before = pos(path, index)
            val after = pos(path, index + 1)
            val peerAfter = pos(peer, index + 1)
            val move = MoveRef(path, index)
            val otherMove = MoveRef(peer, index)
            val options = (for
              pawn <- mover(peer, index).filter(_.roleName == "pawn").toList
              if after.contains(pawn)
              firstRank = if pawn.side.white then '2' else '7'
              if pawn.at.last == firstRank && peer.moves(index).take(2).head == peer.moves(index).drop(2).head
              peerPawn <- track(peer, pawn, index, index + 1).toList
              if math.abs(peerPawn.at.last - pawn.at.last) == 1
              square = pawn.at.head.toString + (if pawn.side.white then '4' else '5')
              kept <- advance(after, pawn, square).toList
              remaining <- advance(peerAfter, peerPawn, square).toList
              firstUse = (index + 1 until path.moves.size).find(offset =>
                track(path, pawn, index, offset).exists(p => path.moves(offset).take(2) == p.at))
              use = firstUse.filter(offset => path.moves(offset) == pawn.at + square &&
                track(path, pawn, index, offset).contains(pawn))
              through = use.fold(index + 1)(_ + 1)
              used = track(path, pawn, index, through)
              supported = use.toList.flatMap(_ => used.toList.flatMap(p => pos(path, through).controlledBy(p).toList.sorted
                .flatMap(pos(path, through).occupant).filter(t => t.side == p.side && t.roleName == "pawn")
                .flatMap(t => original(path, t, through).flatMap(root =>
                  paths.pieceAfter(packet, path, root, index).flatten.map(initial => (root, initial, t)))))).headOption
            yield
              val support = for
                offset <- use
                (root, initial, current) <- supported
              yield
                val route = (index until offset).filter(n => track(path, initial, index, n)
                  .exists(p => path.moves(n).take(2) == p.at)).map(MoveRef(path, _)).toList
                SupportedPawn(root, current, route)
              val meaning = Section(List(PawnAdvanceOption(move, pawn, otherMove, peerPawn, square,
                use.map(MoveRef(path, _)), support)))
              effect("pawn_option", meaning,
                List(path -> through, peer -> (index + 1)), index, pawn, supported.map(_._2), square,
                sourceRefs(after, List(kept)) ++ sourceRefs(peerAfter, List(remaining)) ++ supported.toList.flatMap { (_, _, target) =>
                  used.toList.flatMap(p => sourceRefs(pos(path, through), pos(path, through).sources(p, List(target.at))))
                }
              ))
            val captures = (for
              actor <- mover(peer, index).filter(_.roleName == "pawn").toList
              if after.contains(actor)
              capture <- notes(path, index).filter(_.note.detail.isInstanceOf[Detail.Capture])
              peerCapture <- notes(peer, index).filter(_.note.detail.isInstanceOf[Detail.Capture])
              Detail.Capture(_, at, _) = capture.note.detail: @unchecked
              Detail.Capture(_, otherAt, _) = peerCapture.note.detail: @unchecked
              if at == otherAt
              target <- before.occupant(at).toList
              capturer <- mover(path, index).filter(_ != actor).toList
              arrived <- track(path, capturer, index, index + 1).filter(_.at == at).toList
              if after.controlledBy(actor)(arrived.at)
              if track(peer, actor, index, index + 1).exists(_.at == at)
              supports = notes(path, index).filter(_.note.detail match
                case Detail.Support(a, t, _, _) => a == actor && t == arrived
                case _ => false)
              opposition = supports.flatMap(_.note.detail match
                case Detail.Support(_, _, attackers, _) => attackers
                case _ => Nil).distinct
            yield effect("capture_choice", Section(List(PawnCaptureChoice(move, capturer, target, actor, arrived, opposition, otherMove))),
              List(path -> (index + 1), peer -> (index + 1)), index, actor, Some(target), at,
              noteEvidence(capture) ++ noteEvidence(peerCapture) ++ supports.flatMap(noteEvidence) ++
                sourceRefs(after, after.sources(actor, List(at))), Set(capture.id, peerCapture.id) ++ supports.map(_.id)))
            val advances = (for
              block <- notes(path, index)
              (pawn, square) <- (block.note.detail match
                case Detail.PawnBlock(_, pawn, destination) => List(pawn -> destination)
                case _ => Nil)
              blocker <- mover(path, index).filter(_.roleName == "pawn").toList
              if path.moves(index).drop(2).take(2) == square && peer.moves.size > index + 1
              if peer.moves(index + 1) == pawn.at + square && peerAfter.contains(pawn)
              available <- advance(peerAfter, pawn, square).toList
              if track(peer, blocker, index, index + 1).contains(blocker)
            yield
              val capture = notes(peer, index).filter(_.note.detail.isInstanceOf[Detail.Capture])
              val attacks = notes(peer, index + 1).filter(_.note.detail match
                case Detail.Attack(_, _, true) => true
                case _ => false)
              val opened = notes(peer, index + 1).flatMap(_.note.detail match
                case Detail.ChangedLines(_, opened, _) => opened
                case _ => Nil).toSet
              val reply = MoveRef(peer, index + 1)
              val response = attacks.map(n => n.note.detail match
                case Detail.Attack(a, t, _) if opened(a -> t) =>
                  Action(reply, Detail.ChangedLines(Nil, List(a -> t), Nil))
                case other => Action(reply, other)).distinct
              val meaning = Section(List(Action(move, block.note.detail)) ++ capture.map(note => Action(otherMove, note.note.detail)) ++
                List(PawnAdvanceReply(otherMove, pawn, reply)) ++ response)
              effect("advance_choice", meaning,
                List(path -> (index + 1), peer -> (index + 2)), index, pawn, Some(blocker), square,
                noteEvidence(block) ++ sourceRefs(peerAfter, List(available)) ++ (capture ++ attacks).flatMap(noteEvidence),
                (block :: capture ::: attacks).map(_.id).toSet))
            options ++ captures ++ advances
          }
          val alignments = pair.flatMap { path =>
            val before = pos(path, index)
            val after = pos(path, index + 1)
            val pins = for
              prior <- mover(path, index).toList
              actor <- track(path, prior, index, index + 1).toList
              (id, target, back) <- after.relativeAlignments(actor)
            yield
              def relation(count: Int): Option[String] = for
                a <- track(path, actor, index + 1, count)
                f <- track(path, target, index + 1, count)
                b <- track(path, back, index + 1, count)
                if b.roleName != "king" && values(b.roleName) > values(f.roleName)
                row <- pos(path, count).rays.find { (_, r) =>
                  a == RelationColoredPieceWitness(r.attackerSquare, r.attackerRole, r.side) &&
                    r.occupants.take(2).map(p => RelationColoredPieceWitness(p.square, p.role, p.side)) == List(f, b)
                }
              yield row._1
              val end = (index + 2 to path.moves.size).find(n => relation(n).isEmpty)
              val through = end.getOrElse(path.moves.size)
              // The same rear piece can later take part in an already observed
              // exchange. Keep those raw actions with this explanation, separately
              // from the alignment's first-end witness; do not extend the pin.
              val exchange = end.toList.flatMap { released =>
                (released until path.moves.size).find(offset =>
                  track(path, back, index + 1, offset).exists(mover(path, offset).contains)).filter { offset =>
                    notes(path, offset).exists(_.note.detail match
                      case Detail.Capture(_, _, _: LineMaterialRecaptureStatus.Proven) => true
                      case _ => false)
                }.toList.flatMap { offset =>
                  val captures = List(offset - 1, offset).flatMap(n => notes(path, n).filter(_.note.detail.isInstanceOf[Detail.Capture]))
                  val opened = if offset < 2 then Nil else notes(path, offset - 2).filter(n => n.note.detail match
                    case Detail.ChangedLines(_, opened, _) => captures.headOption.exists { capture =>
                      val capturer = mover(path, offset - 1)
                      capture.note.detail match
                        case Detail.Capture(_, square, _) => opened.exists((a, t) => capturer.contains(a) && t.at == square)
                        case _ => false
                    }
                    case _ => false)
                  opened ++ captures
                }
              }
              val kept = (index + 2 until through).filter(n => path.moves(n - 1).take(2) != path.moves(n - 1).drop(2).take(2) &&
                track(path, actor, index + 1, n) != track(path, actor, index + 1, n - 1)).map(n => MoveRef(path, n - 1)).toList
              val meaning = Section(List(RelativeAlignment(MoveRef(path, index), actor, target, back,
                kept, end.map(n => MoveRef(path, n - 1)))))
              effect("relative_pin", meaning,
                List(path -> through), index + 1, actor, Some(target), back.at,
                (index + 1 to through).flatMap(n => relation(n).toList.flatMap(r => sourceRefs(pos(path, n), List(r)))).toList)
                .copy(followupNotes = exchange)
            val releases = for
              king <- mover(path, index).filter(_.roleName == "king").toList
              next <- track(path, king, index, index + 1).toList
              if before.empty(next.at) && math.abs(king.at.head - next.at.head) <= 1 && math.abs(king.at.last - next.at.last) <= 1
              (id, ray) <- before.rays
              front <- ray.occupants.headOption.toList
              rear <- ray.occupants.drop(1).headOption.toList
              actor = RelationColoredPieceWitness(front.square, front.role, front.side)
              if actor.side == king.side && actor.roleName != "king" && after.contains(actor) &&
                RelationColoredPieceWitness(rear.square, rear.role, rear.side) == king && ray.side != king.side
              square <- before.controlledBy(actor).toList.sorted
              target <- before.occupant(square).filter(p => p.side != king.side && after.contains(p)).toList
              if target.square != ray.attackerSquare &&
                !ray.geometry.squares.takeWhile(_ != king.square).contains(target.square)
              if before.view.ordinaryCaptureKeepsKingSafe(actor, target).contains(false) &&
                after.view.ordinaryCaptureKeepsKingSafe(actor, target).contains(true)
            yield
              val reply = Option.when(path.moves.size > index + 1 && mover(path, index + 1).contains(target))(index + 1)
              val replyNotes = reply.toList.flatMap(n => notes(path, n).filter(_.note.detail match
                case Detail.Attack(a, _, true) => track(path, target, index, n + 1).contains(a)
                case Detail.Capture(_, _, _) => true
                case _ => false))
              val pinner = RelationColoredPieceWitness(ray.attackerSquare, ray.attackerRole, ray.side)
              val response = reply.toList.flatMap(offset => replyNotes.map(note => Action(MoveRef(path, offset), note.note.detail))).distinct
              val meaning = Section(List(ReleasedCapture(MoveRef(path, index), king, pinner, actor, target,
                reply.map(MoveRef(path, _)))) ++ response)
              effect("capture_release", meaning,
                List(path -> reply.fold(index + 1)(_ + 1)), index, actor, Some(target), target.at,
                sourceRefs(before, List(id) ++ before.sources(actor, List(target.at))) ++
                  sourceRefs(after, after.sources(actor, List(target.at))) ++ replyNotes.flatMap(noteEvidence),
                replyNotes.map(_.id).toSet)
            // Missing relation inventory/identity is not an observed unpinning.
            pins.filter(e => (index + 1 to e.lines.head._2).forall(n =>
              pos(path, n).complete(RelationFactKind.RayBarrier) &&
                List(e.actor, e.target.get, after.occupant(e.square).get).forall(p =>
                  original(path, p, index + 1).exists(root => paths.pieceAfter(packet, path, root, n).nonEmpty)))) ++ releases
          }
          val separateAlignments = alignments.filterNot { pin =>
            pin.scope == "relative_pin" && comparisons.exists(c => c.scope == "advance_choice" &&
              c.target.nonEmpty && pin.target.contains(c.actor) && c.lines.contains(pin.lines.head) &&
              pair.find(_.line.ref.id == pin.lines.head._1).exists(path => notes(path, index + 1).exists(_.note.detail match
                case Detail.ChangedLines(_, opened, _) => opened.exists((a, t) => a == pin.actor && t.at == pin.square)
                case _ => false)))
          }
          (comparisons ++ separateAlignments).distinctBy(e => (e.scope, e.lines, e.actor, e.target, e.square))
      })
    // The same owned line can occur in several existing comparisons. Read its
    // observed attacks once, independently of display selection, using existing physical-piece traces.
    enum CaptureRole:
      case Plain, Target, Attacker, Recapture, LaterRecovery
    case class HistoryStep(offset: Int, action: String, detail: String, evidence: List[String],
        exchangeAddition: Boolean = false, checkAction: Boolean = false, observation: Option[String] = None,
        recapture: Boolean = false, opensAttack: Boolean = false, captureRole: Option[CaptureRole] = None,
        plainObservation: Boolean = false, withdrawal: Option[(Piece, Piece, Piece)] = None,
        briefDetail: String = "", meaning: List[CommentaryAnswer.Statement] = Nil):
      def eventKey: String = withdrawal.fold(observation.fold(
        if meaning.nonEmpty then CommentarySelection.stableId("history-meaning", evidence.distinct.sorted)
        else s"transition:$offset:$checkAction:$captureRole")("note:" + _)) {
        (before, after, _) => s"withdraw:$before:$after"
      }
    case class AttackHistory(path: ObservedLineProtocol.PreparedPath, start: Int, originalAttacker: Piece, originalTarget: Piece,
        changes: List[(Int, String, Option[Piece])], steps: List[HistoryStep], endTarget: Option[Piece],
        endAttack: Option[Piece], resumed: Boolean,
        opening: String, openingObservation: String, effect: ComparisonEffect, introduced: Boolean = false)
    val outcomes = mutable.Map.empty[(String, Int, Set[Piece]), List[AttackHistory]]
    def attackOutcomes(path: ObservedLineProtocol.PreparedPath, index: Int,
        users: Set[Piece] = Set.empty): List[AttackHistory] =
      outcomes.getOrElseUpdate((path.line.ref.id, index, users), {
        if path.certified.isEmpty || path.ids != path.declared || path.moves.size <= index + 1 then Nil
        else
          val end = path.moves.size
          val notes = path.ids.map(id => byOccurrence.getOrElse(id, Nil)).toVector
          lazy val atAttack = paths.identitiesAfter(packet, path, index + 1)
          def original(piece: Piece): Option[Piece] = atAttack.collectFirst {
            case (root, Some(Some(current))) if current == piece => root
          }
          notes(index).distinctBy(_.note.detail).flatMap { first =>
            first.note.detail match
              case Detail.Attack(attacker, target, _) if users.isEmpty || users(attacker) =>
                (for
                  originalAttacker <- original(attacker)
                  originalTarget <- original(target)
                  endTarget <- paths.pieceAfter(packet, path, originalTarget, end)
                yield
                  def tracked(piece: Piece, count: Int) = paths.pieceAfter(packet, path, piece, count).flatten
                  def captureAt(piece: Piece, offset: Int): Option[LocalCandidate] =
                    tracked(piece, offset).flatMap { before =>
                      notes(offset).find(candidate => candidate.note.detail match
                        case Detail.Capture(_, square, _) => before.at == square &&
                          candidate.before.contains(before) &&
                          paths.pieceAfter(packet, path, piece, offset + 1).contains(None)
                        case _ => false)
                    }
                  val captures = (index + 1 until end).flatMap(offset =>
                    captureAt(originalTarget, offset).map(offset -> _)).toList
                  // Use the existing physical capturer-removal links. A different
                  // piece on the same square, or a longer supplied line, cannot close this exchange.
                  val recoveries = if captures.isEmpty then Nil else
                    continuations.get(path.line.ref.id).filter(_.path eq path).toList.flatMap { reading =>
                      reading.events.filter(loss => loss.originalVictim.contains(originalTarget) &&
                        captures.exists((offset, _) => path.ids(offset) == loss.occurrenceId)).flatMap { loss =>
                        reading.captureLinks.filter(_.lossEventId == loss.id).flatMap { link =>
                          for
                            recovery <- reading.events.find(_.id == link.captureEventId).toList
                            capturer <- loss.originalActor.toList
                            beforeCapturer <- tracked(capturer, recovery.plyOffset).toList
                            note <- captureAt(capturer, recovery.plyOffset).toList
                          yield
                            val opened = note.after.view.pawnFiles.filter(file => file.occupancy == "open" &&
                              note.before.view.pawnFiles.exists(prior => prior.file == file.file && prior.occupancy != "open"))
                            val wording = s"${if link.elapsedPlies == 1 then "recaptures" else "later captures"} ${beforeCapturer.name}."
                            val relation = s" The captured ${beforeCapturer.roleName} played ${sans(path)(loss.plyOffset)}." +
                              opened.map(file => s" This opens the ${file.file.key}-file.").mkString
                            val sources = sourceRefs(note.after, opened.flatMap(_.sourceIds)) ++
                              sourceRefs(note.before, opened.flatMap(file =>
                                note.before.view.pawnFiles.filter(_.file == file.file).flatMap(_.sourceIds)))
                            // A delayed removal must not erase what this same capturer
                            // obtained in between. Keep those captures in the same bundle.
                            val gains = reading.events.filter(event => event.originalActor.contains(capturer) &&
                              event.plyOffset > loss.plyOffset && event.plyOffset < recovery.plyOffset).flatMap { event =>
                              for
                                identity <- event.originalVictim.toList
                                victim <- tracked(identity, event.plyOffset).toList
                                capture <- captureAt(identity, event.plyOffset).toList
                                actor <- capture.before.occupant(capture.edge.moveUci.take(2)).toList
                              yield HistoryStep(event.plyOffset,
                                s"${actor.name} also captures ${victim.name}.",
                                s" This precedes that ${actor.roleName}'s removal with ${sans(path)(recovery.plyOffset)}.",
                                noteEvidence(capture), observation = Some(capture.id), captureRole = Some(CaptureRole.Plain))
                            }
                            (HistoryStep(recovery.plyOffset, wording, relation, noteEvidence(note) ++ sources,
                              observation = Some(note.id), recapture = link.elapsedPlies == 1,
                              captureRole = Some(if link.elapsedPlies == 1 then CaptureRole.Recapture
                                else CaptureRole.LaterRecovery),
                              briefDetail = opened.map(file => s" This opens the ${file.file.key}-file.").mkString),
                              recovery.originalActor, gains)
                        }
                      }
                    }
                  val interveningChecks = if captures.isEmpty then Nil else
                    continuations.get(path.line.ref.id).filter(_.path eq path).toList.flatMap { reading =>
                      for
                        link <- reading.captureLinks
                        loss <- reading.events.find(_.id == link.lossEventId).toList
                        recovery <- reading.events.find(_.id == link.captureEventId).toList
                        // The original target can capture its attacker first; the same
                        // physical capture link owns the later removal in either direction.
                        if (loss.originalVictim.contains(originalTarget) &&
                          captures.exists((offset, _) => path.ids(offset) == loss.occurrenceId)) ||
                          (loss.originalVictim.contains(originalAttacker) && loss.originalActor.contains(originalTarget) &&
                            captures.exists((offset, _) => path.ids(offset) == recovery.occurrenceId))
                        capturer <- loss.originalActor.toList
                        recoveringActor <- recovery.originalActor.toList
                        (record, fact) <- path.certified.toList
                        check <- fact.lineEvents.filter(event => event.kind == LineEventKind.Check &&
                          event.side.contains(recoveringActor.side) && event.targetRole.contains(EvidencePieceRole(chess.King.name)) &&
                          event.plyOffset > loss.plyOffset && event.plyOffset + 1 < recovery.plyOffset)
                        square <- check.square.toList
                        reply <- fact.lineEvents.find(event => event.kind == LineEventKind.CheckEvasion &&
                          event.plyOffset == check.plyOffset + 1 && event.side.contains(capturer.side) &&
                          event.square.contains(square)).toList
                      yield
                        val king = s"${capturer.sideName.capitalize} king"
                        val checkingMove = (for
                          capture <- fact.lineEvents.find(event =>
                            (event.kind == LineEventKind.Capture || event.kind == LineEventKind.Recapture) &&
                              event.plyOffset == check.plyOffset && event.moveUci == check.moveUci && event.side == check.side)
                          role <- capture.targetRole
                          at <- capture.square
                        yield s"captures the ${capturer.sideName.capitalize} ${role.name} on ${at.key} " +
                          s"with check against the $king on ${square.key} before the later capture.")
                          .getOrElse(s"checks the $king on ${square.key} before the later capture.")
                        val response = if reply.pieceRole.contains(EvidencePieceRole(chess.King.name)) then
                          s"moves the $king from ${square.key} to ${reply.moveUci.slice(2, 4)} in reply to that check."
                          else "answers that check."
                        List(
                          HistoryStep(check.plyOffset, checkingMove,
                            "", List(record.ref.id, path.ids(check.plyOffset)), checkAction = true),
                          HistoryStep(reply.plyOffset, response, "", List(record.ref.id, path.ids(reply.plyOffset)),
                            checkAction = true))
                    }.flatten
                  val evasions = (index + 1 until end).flatMap { offset =>
                    for
                      afterTarget <- tracked(originalTarget, offset + 1)
                      afterAttacker <- tracked(originalAttacker, offset + 1)
                      note <- notes(offset).find(_.note.detail == Detail.Evades(afterTarget, afterAttacker))
                    yield offset -> note
                  }.toList
                  val relocations = (index + 1 until end).flatMap { offset =>
                    for
                      beforeTarget <- tracked(originalTarget, offset)
                      afterTarget <- tracked(originalTarget, offset + 1)
                      if beforeTarget != afterTarget
                      beforeAttacker <- tracked(originalAttacker, offset)
                      afterAttacker <- tracked(originalAttacker, offset + 1)
                      before <- positions.get(path.positions(offset))
                      after <- positions.get(path.positions(offset + 1))
                      if before.controlledBy(beforeAttacker)(beforeTarget.at) &&
                        after.controlledBy(afterAttacker)(afterTarget.at)
                    yield HistoryStep(offset,
                      s"moves the initially attacked ${target.name} to ${afterTarget.at}; " +
                        s"${afterAttacker.name} still attacks it there.", "",
                      List(path.ids(offset), before.view.occurrence.id, after.view.occurrence.id) ++
                        sourceRefs(before, before.sources(beforeAttacker, List(beforeTarget.at))) ++
                        sourceRefs(after, after.sources(afterAttacker, List(afterTarget.at))))
                  }.toList
                  var observedInterruption = false
                  val interventions = (index + 1 until end).flatMap { offset =>
                    for
                      beforeTarget <- tracked(originalTarget, offset)
                      afterTarget <- tracked(originalTarget, offset + 1)
                      beforeAttacker <- tracked(originalAttacker, offset)
                      before <- positions.get(path.positions(offset))
                      after <- positions.get(path.positions(offset + 1))
                      active = before.controlledBy(beforeAttacker)(beforeTarget.at)
                      removal = Option.when(active)(captureAt(originalAttacker, offset)).flatten
                      afterAttacker = tracked(originalAttacker, offset + 1)
                      change <- removal.map(note => (Some(note), "capture", beforeAttacker)).orElse {
                        Option.when(beforeTarget == afterTarget && active && after.complete(RelationFactKind.GeometricControl))(
                          afterAttacker.filter(a => a != beforeAttacker && !after.controlledBy(a)(afterTarget.at))
                            .map(a => (None, "withdraw", a))).flatten
                      }.orElse {
                        notes(offset).flatMap { note => note.note.detail match
                          case Detail.ChangedLines(closed, opened, _) if beforeTarget == afterTarget &&
                              afterAttacker.contains(beforeAttacker) =>
                            if closed.exists((a, t, _) => a == beforeAttacker && t == beforeTarget) then
                              Some((Some(note), "block", beforeAttacker))
                            else Option.when((observedInterruption || !active) && opened.contains(beforeAttacker -> beforeTarget))(
                              (Some(note), "reopen", beforeAttacker))
                          case Detail.Attack(a, t, true) if (observedInterruption || !active) && afterAttacker.contains(a) && t == afterTarget =>
                            Some((Some(note), "reopen", a))
                          case _ => None
                        }.headOption
                      }
                    yield
                      val (note, kind, currentAttacker) = change
                      if kind == "block" || kind == "withdraw" then observedInterruption = true
                      else if kind == "reopen" then observedInterruption = false
                      val remaining = if kind == "reopen" then Nil else after.controllers(afterTarget.at, attacker.side)
                      val obstruction = Option.when(kind == "block")(
                        after.blockedRay(currentAttacker, afterTarget)).flatten
                      val action = kind match
                        case "capture" => s"captures the original attacker ${beforeAttacker.name}."
                        case "block" if obstruction.nonEmpty =>
                          s"blocks ${beforeAttacker.name}'s line to the initially attacked ${target.name}, which stays on ${afterTarget.at}."
                        case "block" => s"ends ${beforeAttacker.name}'s attack on the initially attacked ${target.name}, which stays on ${afterTarget.at}."
                        case "withdraw" => s"moves the original attacker from ${beforeAttacker.at} to ${currentAttacker.at} and ends its attack on the initially attacked ${target.name}."
                        case _ => s"restores ${currentAttacker.name}'s attack on the initially attacked ${target.name}."
                      val detail = (if kind == "capture" then s" The initially attacked ${target.name} " +
                        s"${if beforeTarget == afterTarget then "stays on" else "moves to"} ${afterTarget.at}." else "") +
                        (if remaining.isEmpty then "" else
                        s" ${remaining.map(_.name).mkString(" and ")} ${if remaining.size == 1 then "attacks" else "attack"} " +
                          (if kind == "withdraw" then afterTarget.name else "that target") + " after this move.")
                      val movementEvidence = if note.nonEmpty then Nil else
                        observations.getOrElse(path.ids(offset), Nil).filter(_.meaning match
                          case MoveObservationMeaning.PieceControl(prior, next) =>
                            prior.controller.contains(beforeAttacker) && prior.target.contains(beforeTarget) &&
                              next.controller == afterAttacker && next.target.contains(afterTarget) &&
                              prior.state == PieceControlState.Present && next.state == PieceControlState.Absent
                          case _ => false).flatMap(_.premises.map(_.id))
                      val evidence = note.toList.flatMap(noteEvidence) ++ movementEvidence ++ List(path.ids(offset)) ++
                        sourceRefs(before, before.sources(beforeAttacker, List(beforeTarget.at))) ++
                        sourceRefs(after, remaining.flatMap(piece => after.sources(piece, List(afterTarget.at))) ++
                          obstruction.toList.map(_._1))
                      val move = RootQuestionSummary.MoveRef(path, offset)
                      val blocked = if kind != "block" then Nil else obstruction match
                        case Some((_, blockers)) => blockers.map(blocker =>
                          CommentaryAnswer.BlockedAttack(move, currentAttacker, afterTarget, blocker))
                        case None => List(CommentaryAnswer.ControlState(move, currentAttacker,
                          Some(afterTarget), afterTarget.at, false))
                      val retained = Option.when(kind == "capture")(
                        CommentaryAnswer.PiecePresence(move, originalTarget, afterTarget)).toList
                      val pressure = remaining.map(actor =>
                        CommentaryAnswer.ControlState(move, actor, Some(afterTarget), afterTarget.at, true))
                      (offset, note, kind, action, detail, evidence,
                        Option.when(kind == "withdraw")((beforeAttacker, currentAttacker, afterTarget)),
                        blocked ++ retained ++ pressure)
                  }.toList
                  // Survival alone is not a resolution. Keep the observed mechanism
                  // and later reopening/capture; no inferred permanent safety.
                  val resolved = captures.nonEmpty || evasions.nonEmpty || interventions.nonEmpty
                  val firstOutcome = ((captures ++ evasions).map(_._1) ++ interventions.map(_._1)).minOption.getOrElse(end)
                  // An actual response to the immediate counterattack can
                  // explain the intervening move. Same color alone cannot.
                  val counterAttacks = if !resolved || firstOutcome <= index + 1 then Nil else
                    notes(index + 1).filter(candidate => candidate.note.detail match
                      case Detail.Attack(actor, _, _) => actor.side == target.side
                      case _ => false).distinctBy(_.note.detail)
                  lazy val replyIdentities = paths.identitiesAfter(packet, path, index + 2)
                  def replyOriginal(piece: Piece): Option[Piece] = replyIdentities.collectFirst {
                    case (root, Some(Some(current))) if current == piece => root
                  }
                  val counterPairs = counterAttacks.flatMap { candidate => candidate.note.detail match
                    case Detail.Attack(actor, victim, _) =>
                      for
                        a <- replyOriginal(actor)
                        t <- replyOriginal(victim)
                      yield a -> t
                    case _ => None
                  }
                  val responses = (index + 2 until math.min(firstOutcome + 1, end)).flatMap { offset =>
                    notes(offset).filter(candidate => candidate.note.detail match
                      case Detail.Evades(victim, actor) => counterPairs.exists { (a, t) =>
                        tracked(a, offset + 1).contains(actor) && tracked(t, offset + 1).contains(victim)
                      }
                      case _ => false).map(offset -> _)
                  }.toList
                  val middle = if responses.isEmpty then Nil else counterAttacks.map(index + 1 -> _) ++ responses
                  // A direct reply can open a stationary slider's attack. The
                  // observation already owns the blocker departure and both endpoints.
                  val openedAttack = notes(index).flatMap { note => note.note.detail match
                    case Detail.ChangedLines(_, opened, _) if opened.contains(attacker -> target) =>
                      List(HistoryStep(index, s"Opens the line from ${attacker.name} to ${target.name}.", "", noteEvidence(note),
                        opensAttack = true, meaning = List(CommentaryAnswer.Action(RootQuestionSummary.MoveRef(path, index),
                          Detail.ChangedLines(Nil, List(attacker -> target), Nil)))))
                    case _ => Nil
                  }
                  // Follow only the next actual response to a new attack on the
                  // same target, or its counterattack on the original attacker.
                  // A move elsewhere in the PV cannot supply that response.
                  val renewedAttacks = (index + 1 until end - 1).flatMap { offset =>
                    for
                      next <- notes(offset)
                      pair <- (next.note.detail match
                        case Detail.Attack(a, t, true) if tracked(originalTarget, offset + 1).contains(t) &&
                            a.side == attacker.side && !tracked(originalAttacker, offset + 1).contains(a) => Some(a -> t)
                        case Detail.Attack(a, t, true) if tracked(originalTarget, offset + 1).contains(a) &&
                            tracked(originalAttacker, offset + 1).contains(t) => Some(a -> t)
                        case _ => None).toList
                      (a, t) = pair
                      response <- notes(offset + 1).find(_.note.detail match
                        case Detail.Evades(victim, source) => source == a &&
                          paths.identitiesAfter(packet, path, offset + 1).exists {
                            case (root, Some(Some(piece))) if piece == t => tracked(root, offset + 2).contains(victim)
                            case _ => false
                          }
                        case _ => false).toList
                    yield List(HistoryStep(offset, next.note.detail.text, "", noteEvidence(next),
                      observation = Some(next.id), plainObservation = true),
                      HistoryStep(offset + 1, response.note.detail.text, "", noteEvidence(response),
                        observation = Some(response.id), plainObservation = true))
                  }.toList.flatten
                  // When the attacked defender answers immediately, connect its
                  // lost support to the next actual capture, without asserting
                  // that the capture has just become legal or wins material.
                  val defenderDeparture = if index + 2 >= end then Nil else (for
                    beforeDefender <- tracked(originalTarget, index + 1).toList
                    afterDefender <- tracked(originalTarget, index + 2).filter(_ != beforeDefender).toList
                    departure <- observations.getOrElse(path.ids(index + 1), Nil)
                    victim <- (departure.meaning match
                      case MoveObservationMeaning.PieceControl(prior, next) if
                          prior.controller.contains(beforeDefender) && next.controller.contains(afterDefender) &&
                          prior.state == PieceControlState.Present && next.state == PieceControlState.Absent &&
                          prior.target == next.target => next.target.filter(_.side == target.side)
                      case _ => None).toList
                    capture <- notes(index + 2).find(note => note.note.actor.side == attacker.side &&
                      note.before.contains(victim) && (note.note.detail match
                        case Detail.Capture(role, square, _) => role == victim.roleName && square == victim.at
                        case _ => false)).toList
                  yield List(
                    HistoryStep(index + 1, s"${beforeDefender.name} ends its support of ${victim.name}.", "",
                      departure.premises.map(_.id) :+ path.ids(index + 1), meaning = List(
                        CommentaryAnswer.ControlState(RootQuestionSummary.MoveRef(path, index + 1),
                          afterDefender, Some(victim), victim.at, false))),
                    HistoryStep(index + 2, s"${capture.before.occupant(capture.edge.moveUci.take(2)).get.name} captures ${victim.name}.",
                      s" This follows the end of ${afterDefender.name}'s support.", noteEvidence(capture),
                      observation = Some(capture.id), captureRole = Some(CaptureRole.Plain))
                  )).flatten
                  val observedSequence = ((middle ++ evasions ++ captures).map { (offset, candidate) =>
                    val wording =
                      if captures.exists(_._2 == candidate) then
                        val capturer = if tracked(originalAttacker, offset + 1).contains(candidate.note.actor) then ""
                          else candidate.before.occupant(candidate.edge.moveUci.take(2)).map(_.name + " ").getOrElse("")
                        val victim = tracked(originalTarget, offset).getOrElse(target)
                        s"${capturer}captures the initially attacked ${victim.name}."
                      else if evasions.exists(_._2 == candidate) then
                        val Detail.Evades(_, currentAttacker) = candidate.note.detail: @unchecked
                        s"moves the initially attacked ${target.name} out of ${currentAttacker.name}'s attack."
                      else candidate.note.detail.text
                    HistoryStep(offset, wording, "", noteEvidence(candidate), observation = Some(candidate.id),
                      captureRole = Option.when(captures.exists(_._2 == candidate))(CaptureRole.Target),
                      plainObservation = !captures.exists(_._2 == candidate) && !evasions.exists(_._2 == candidate))
                  } ++ interventions.map(row => HistoryStep(row._1, row._4, row._5, row._6,
                    observation = row._2.filter(_ => row._3 == "capture").map(_.id),
                    captureRole = Option.when(row._3 == "capture")(CaptureRole.Attacker), withdrawal = row._7,
                    meaning = row._8)) ++
                     relocations ++ recoveries.map(_._1) ++ recoveries.flatMap(_._3) ++ interveningChecks ++ openedAttack ++
                     renewedAttacks ++ defenderDeparture).sortBy(_.offset)
                  // An observed escape can itself capture. The original target's
                  // departure can also open the original attacker's next capture.
                  // Both use existing notes at those exact occurrences, not nearby trades.
                  def captureStep(offset: Int, note: LocalCandidate): Option[HistoryStep] = note.note.detail match
                    case Detail.Capture(_, square, _) => note.before.occupant(square).map { victim =>
                      val actor = note.before.occupant(note.edge.moveUci.take(2)).get
                      val promotion = if actor.roleName == note.note.actor.roleName then ""
                        else s" ${actor.name} promotes to a ${note.note.actor.roleName}."
                      HistoryStep(offset, s"${actor.name} captures ${victim.name}.",
                        promotion, noteEvidence(note), observation = Some(note.id), captureRole = Some(CaptureRole.Plain))
                    }
                    case _ => None
                  val evasionCaptures = observedSequence.flatMap { step =>
                    Option.when(notes(step.offset).exists(note => step.observation.contains(note.id) &&
                      note.note.detail.isInstanceOf[Detail.Evades]))(step.offset)
                  }.distinct.flatMap(offset => notes(offset).flatMap(captureStep(offset, _)))
                  val openedReplyCaptures = evasions.flatMap { (offset, _) =>
                    if offset + 1 >= end then Nil else (for
                      actor <- tracked(originalAttacker, offset + 1).toList
                      departure <- tracked(originalTarget, offset).toList
                      note <- notes(offset + 1)
                      if tracked(originalAttacker, offset + 2).contains(note.note.actor)
                      step <- captureStep(offset + 1, note).toList
                      victim <- (note.note.detail match
                        case Detail.Capture(_, square, _) => note.before.occupant(square)
                        case _ => None).toList
                      opened <- notes(offset).find(_.note.detail match
                        case Detail.ChangedLines(_, pairs, _) => pairs.contains(actor -> victim)
                        case _ => false).toList
                    yield step.copy(detail = s" This uses the attack line opened when ${departure.name} moved away.",
                      briefDetail = s" This uses the attack line opened when ${departure.name} moved away.",
                      evidence = (step.evidence ++ noteEvidence(opened)).distinct,
                      meaning = step.meaning :+ CommentaryAnswer.Action(RootQuestionSummary.MoveRef(path, offset),
                        Detail.ChangedLines(Nil, List(actor -> victim), Nil))))
                  }
                  val responseCaptures = evasionCaptures ++ openedReplyCaptures
                  val connectedCaptures = defenderDeparture.filter(_.offset == index + 2).map(_.offset).toSet
                  val attackerRemovals = interventions.filter(_._3 == "capture").map(_._1).toSet
                  val exchangeSteps = if captures.isEmpty && connectedCaptures.isEmpty && responseCaptures.isEmpty && attackerRemovals.isEmpty then Nil else
                    continuations.get(path.line.ref.id).filter(_.path eq path).toList.flatMap { reading =>
                      path.certified.toList.flatMap { (_, fact) => fact.material.toList.flatMap { material =>
                        val events = reading.events.map(event => event.id -> event).toMap
                        val ledger = material.captures.map(capture => capture.plyOffset -> capture).toMap
                        val immediate = reading.captureLinks.filter(link => link.elapsedPlies == 1 &&
                          ledger.get(events(link.captureEventId).plyOffset).exists(_.recapture))
                        val before = immediate.map(link => link.captureEventId -> link.lossEventId).toMap
                        val after = immediate.map(link => link.lossEventId -> link.captureEventId).toMap
                        def follow(start: String, links: Map[String, String]) =
                          List.unfold(Option(start))(id => id.map(value => events(value) -> links.get(value)))
                        val anchors = captures.map(_._1).toSet ++ recoveries.map(_._1.offset) ++ connectedCaptures ++
                          responseCaptures.map(_.offset)
                        val shown = anchors ++ recoveries.flatMap(_._3.map(_.offset)) ++ interventions.filter(_._3 == "capture").map(_._1) ++
                          interveningChecks.filter(step => fact.lineEvents.exists(event =>
                            event.kind == LineEventKind.Check && event.plyOffset == step.offset)).map(_.offset)
                        val transactions = reading.events.filter(event => anchors(event.plyOffset) || attackerRemovals(event.plyOffset)).flatMap { anchor =>
                          // Removing the attacker also needs the ensuing recapture.
                          // Its preceding exchange is not this new obligation.
                          val preceding = if anchors(anchor.plyOffset) then follow(anchor.id, before) else Nil
                          val chain = (preceding ++ follow(anchor.id, after)).distinctBy(_.id)
                          if chain.exists(event => material.events.exists(value =>
                            value.plyOffset == event.plyOffset && value.promotionGainCp > 0)) then Nil else chain
                        }.distinctBy(_.id).filter(event => event.plyOffset >= index && !shown(event.plyOffset))
                        transactions.flatMap { event =>
                          for
                            originalVictim <- event.originalVictim.toList
                            victim <- tracked(originalVictim, event.plyOffset).toList
                            note <- captureAt(originalVictim, event.plyOffset).toList
                            capture <- ledger.get(event.plyOffset).toList
                          yield
                            val action = s"${if capture.recapture then "recaptures" else "captures"} ${victim.name}."
                            HistoryStep(event.plyOffset, action, "", noteEvidence(note), exchangeAddition = true,
                              observation = Some(note.id), recapture = capture.recapture,
                              captureRole = Some(if capture.recapture then CaptureRole.Recapture else CaptureRole.Plain))
                        }
                      }}
                    }
                  val transactionSteps = observedSequence ++ responseCaptures ++ exchangeSteps
                  val captureOffsets = path.certified.flatMap(_._2.material).toList.flatMap(_.captures.map(_.plyOffset)).toSet
                  val checkingCaptures = transactionSteps.map(_.offset).distinct.filter(captureOffsets).filter(offset =>
                    observations.getOrElse(path.ids(offset), Nil).exists(_.meaning match
                      case MoveObservationMeaning.CertifiedRelation(_: RelationWitnessDetail.CreatedCheckResponseInventory) => true
                      case _ => false))
                  val captureChecks = checkSteps(path, checkingCaptures, None, observations).filterNot { (offset, _, _, _) =>
                    transactionSteps.exists(step => step.checkAction && step.offset == offset)
                  }.map { (offset, action, proof, _) =>
                    HistoryStep(offset, CommentaryEnglish.checkDetail(action) + ".", "", proof :+ path.ids(offset), checkAction = true)
                  }
                  val sequence = (transactionSteps ++ captureChecks).distinct.sortBy(_.offset)
                  val endPosition = path.positions.lastOption.flatMap(positions.get)
                  val endAttacker = tracked(originalAttacker, end)
                  val renewed = for
                    piece <- endTarget
                    actor <- endAttacker
                    position <- endPosition
                    if position.controlledBy(actor)(piece.at)
                  yield (actor, piece, position)
                  val opening = s"In the provided ${sans(path)(index)} line, after ${attacker.name} attacks ${target.name}, "
                  val evidence = noteEvidence(first) ++ sequence.flatMap(_.evidence) ++
                    path.certified.map(_._1.ref.id) ++ renewed.toList.flatMap { (actor, piece, position) =>
                      sourceRefs(position, position.sources(actor, List(piece.at)))
                    }
                  def originalMover(offset: Int, note: LocalCandidate): Option[Piece] =
                    paths.identitiesAfter(packet, path, offset + 1).collectFirst {
                      case (root, Some(Some(current))) if current == note.note.actor => root
                    }
                  val changes = (captures.map((offset, note) => (offset, "target_captured", originalMover(offset, note))) ++
                    evasions.map((offset, _) => (offset, "evades", Some(originalTarget))) ++
                    relocations.map(step => (step.offset, "relocates", Some(originalTarget))) ++
                    interventions.map((offset, note, kind, _, _, _, _, _) =>
                      (offset, kind, note.flatMap(originalMover(offset, _)).orElse(Some(originalAttacker)))) ++
                     recoveries.map((step, actor, _) => (step.offset, "capturer_removed", actor))).sortBy(_._1)
                  val effect = ComparisonEffect("", "line_history", List(path.line.ref.id -> end),
                    path.moves.take(index), path.positions(index + 1), attacker, Some(target), target.at,
                    first.edge.evidence.id :: sequence.map(row => path.ids(row.offset)), evidence.distinct)
                  List(AttackHistory(path, index, originalAttacker, originalTarget, changes, sequence, endTarget,
                    renewed.map(_._1), resolved,
                    opening, first.id, effect))
                ).toList.flatten
              case _ => Nil
          }
      })
    def rootUses(pair: List[ObservedLineProtocol.PreparedPath]): List[(CommentaryAnswer.Section, List[String])] =
      pair.flatMap { path =>
        val root = RootQuestionSummary.MoveRef(path, 0)
        val mover = positions(path.positions.head).occupant(path.moves.head.take(2)).get
        local.filter(_.edge.evidence.id == path.ids.head).flatMap { note =>
          val opening = note.note.detail match
            case ray: Detail.OpenedRay if mover.role.name == "pawn" => Some(ray)
            case Detail.ChangedLines(_, opened, _) =>
              val exposure = opened.filter((actor, target) => actor.side != mover.side && target.side == mover.side)
              Option.when(exposure.nonEmpty)(Detail.ChangedLines(Nil, exposure, Nil))
            case _ => None
          opening.map(detail => CommentaryAnswer.Section(List(CommentaryAnswer.Action(root, detail))) -> note.evidence)
        }
      } ++ pair.flatMap(path => currentEffects(path, 0)).flatMap(effect =>
        effect.meaning.map { meaning =>
          val actions = if effect.scope == "piece_route" then effect.actions.flatMap(note =>
            pair.find(_.ids.contains(note.edge.evidence.id)).map(path => CommentaryAnswer.Action(
              RootQuestionSummary.MoveRef(path, path.ids.indexOf(note.edge.evidence.id)), note.note.detail))) else Nil
          meaning.copy(statements = meaning.statements ++ actions) -> effect.evidence
        })

    def compose(requests: List[(JsObject, List[Context])], explained: Set[JsObject]): List[JsObject] =
      val (candidate, _) = requests.head
      val detail = (candidate \ "detail").as[JsObject]
      // Only co-displayed root questions may share a reading. Other questions,
      // horizons and PVs retain standalone explanations even when moves coincide.
      val shared = requests.tail.forall { (other, _) =>
        (detail \ "type").as[String] == "root_comparison" &&
          (candidate \ "selection").as[String] == "selected" &&
          (other \ "selection").as[String] == "selected" &&
          (candidate \ "root_occurrence_id") == (other \ "root_occurrence_id") &&
          (candidate \ "line_ids").as[List[String]].sorted == (other \ "line_ids").as[List[String]].sorted &&
          (other \ "detail" \ "type").as[String] == "question_comparison" &&
          (other \ "detail" \ "prefix_moves_uci").as[List[String]].isEmpty &&
          (other \ "detail" \ "evaluation_relation").asOpt[String].nonEmpty
      }
      if !shared then return requests.flatMap(request => compose(List(request), explained))
      val supported = Set("root_comparison", "pv_comparison", "question_comparison")
      val pair = if supported((detail \ "type").as[String]) then
        (candidate \ "line_ids").as[List[String]].flatMap(paths.path(packet, _)) else Nil
      val prefix = (detail \ "prefix_moves_uci").asOpt[List[String]].getOrElse(Nil)
      val index = prefix.size
      val openingCaptures = if (detail \ "type").as[String] == "root_comparison" &&
          (detail \ "material_relation").asOpt[String].exists(Set("equal", "different")) then
        pair.flatMap(path => path.certified.flatMap(_._2.material)
          .flatMap(CommentarySelection.openingExchange(_, sans(path)))
          .map(value => path.line.ref.id -> value._1.map(_.plyOffset).toSet)).toMap
        else Map.empty[String, Set[Int]]
      val openingFactCaptures = pair.flatMap(path => path.certified.flatMap(_._2.material)
        .flatMap(CommentarySelection.openingCaptureChain)
        .map(events => path.line.ref.id -> events.map(_.plyOffset).toSet)).toMap
      val observedDifferences = pair match
        case List(left, right) if left.ids.take(index) == right.ids.take(index) &&
            left.moves.take(index) == prefix && right.moves.take(index) == prefix =>
          List(left -> right, right -> left).flatMap { (path, peer) =>
            for
              edge <- path.ids.lift(index).flatMap(edges.get).toList
              peerEdge <- peer.ids.lift(index).flatMap(edges.get).toList
              if edge.from == peerEdge.from && edge.evidence != peerEdge.evidence
              peerAfter <- positions.get(peerEdge.to).toList
              note <- byOccurrence.getOrElse(edge.evidence.id, Nil)
               premises = noteEvidence(note)
              effect <- (note.note.detail match
                case Detail.ChangedLines(closed, opened, blockedSquares) =>
                  val usedSquares = blockedSquares.filter((_, square, _) =>
                    path.ids.lift(index + 1).flatMap(destinations.get).contains(square))
                  closed.map((actor, target, remaining) => (actor, Some(target), target.at, false, remaining, Nil)) ++
                    opened.map((actor, target) => (actor, Some(target), target.at, true, Nil, Nil)) ++
                    usedSquares.map((actor, square, ray) => (actor, None, square, false, Nil, ray))
                case _ => Nil)
              (actor, target, square, present, remaining, ray) = effect
              peerActor <- transported(actor, peerEdge, peerAfter).toList
              peerTarget <- target.fold(List(Option.empty[Piece]).filter(_ => peerAfter.empty(square)))(value =>
                transported(value, peerEdge, peerAfter).map(Some(_)).toList)
              peerSquare = peerTarget.fold(square)(_.at)
              peerPresent = peerAfter.controlledBy(peerActor)(peerSquare)
              if peerPresent != present && (peerPresent || peerAfter.complete(RelationFactKind.GeometricControl))
            yield
              val meaning = CommentaryAnswer.Section(List(CommentaryAnswer.ControlContrast(
                RootQuestionSummary.MoveRef(path, index), actor, target, square, present,
                RootQuestionSummary.MoveRef(peer, index), peerActor, peerTarget, peerSquare,
                peerPresent, remaining, Option.when(target.isEmpty)(RootQuestionSummary.MoveRef(path, index + 1)))))
              val text = render(meaning)
               val evidence = premises ++ sourceRefs(peerAfter, peerAfter.sources(peerActor, List(peerSquare))) ++
                List(edge.evidence.id, peerEdge.evidence.id, note.before.view.occurrence.id,
                  note.after.view.occurrence.id, peerAfter.view.occurrence.id) ++
                Option.when(target.isEmpty)(path.ids(index + 1)).toList
              val effect = ComparisonEffect(text, "pair_difference",
                List(path.line.ref.id -> (index + (if target.isEmpty then 2 else 1)), peer.line.ref.id -> (index + 1)),
                prefix, edge.from, actor, target, square,
                List(edge.evidence.id, peerEdge.evidence.id) ++ Option.when(target.isEmpty)(path.ids(index + 1)).toList,
                evidence.distinct, geometricCost = present != (actor.side == note.note.actor.side),
                meaning = Some(meaning))
              ((actor, target, square), effect, ray)
          }.distinctBy(_._1)
        case _ => Nil
      val differences = observedDifferences.filterNot { row =>
        row._1._2.exists(target => observedDifferences.exists(other =>
            other._1._1 == row._1._1 && other._3.contains(target.at)))
      }.filterNot(row => explained(row._2.json(publicSource)))
      val pv = (detail \ "type").as[String] == "pv_comparison"
      val limit = if pv then 1000 else 4000
      val originalText = (detail \ "text").asOpt[String].getOrElse("")
      val ownedPair = pair match
        case List(left, right) if left.ids.take(index) == right.ids.take(index) &&
            left.moves.take(index) == prefix && right.moves.take(index) == prefix &&
            left.positions.lift(index).nonEmpty && left.positions.lift(index) == right.positions.lift(index) => pair
        case _ => Nil
      // The reviewed move and its owned first replies form one question. Read
      // that account before constructing unrelated long-line explanations.
      val purposeOwners = requests.flatMap(_._2.flatMap(_.firstTarget)).distinctBy(_.id)
      val purposeSearches = requests.flatMap(_._2.flatMap(_.purposeSearches)).distinct
      def deliver(reading: RootQuestionSummary.Result) = requests.zipWithIndex.map { case ((value, _), owner) =>
        if owner != 0 then value else value ++ Json.obj(
          "evidence_ids" -> ((value \ "evidence_ids").as[List[String]] ++ reading.evidence).distinct,
          "detail" -> (((value \ "detail").as[JsObject] - "summary_unavailable") ++ Json.obj(
            "summary" -> reading.text, "summary_kind" -> "question_answer", "summary_question" -> reading.json)))
      }
      if index == 0 && (detail \ "type").as[String] == "root_comparison" && ownedPair.size == 2 then
        val idea = Option.when(ReferencePurposeReading.assessmentPair(packet, paths).toSet == ownedPair.toSet)(())
          .flatMap(_ => MoveIntent.summary(packet, paths, purposeSearches))
        if idea.nonEmpty then return deliver(idea.get)
        lazy val functionalReading = MovePurpose.read(packet, paths, purposeSearches, local,
          requests.flatMap(_._2.flatMap(_.purposeRoots)).distinct)
        val referenceReading = ReferencePurposeReading.read(packet, paths, ownedPair, rootUses(ownedPair),
          responses = functionalReading)
        def realizeReference(reading: ReferencePurposeReading.Reading) =
          ReferencePurposeReading.answer(packet, paths, ownedPair, reading,
            value => completeSelectedConditions(packet, paths, ownedPair, positions, local, value), functionalReading)
            .map(answer => FunctionalReplyReading.withAnswer(packet, paths, ownedPair,
              requests.flatMap(_._2.flatMap(_.replyFunction)).distinct, answer,
              (value, sections) => protectRootUses(packet, paths, ownedPair, positions, local, value, sections)))
            .map(answer => ResponsePurposeReading.withAnswer(packet, paths, ownedPair, purposeOwners, local, answer,
              value => completeSelectedConditions(packet, paths, ownedPair, positions, local, value), rootUses))
            .flatMap(RootQuestionSummary.realize(paths, ownedPair, _))
        val referencePurpose = referenceReading.filter(_.completePurpose).flatMap(realizeReference)
        if referencePurpose.nonEmpty then return deliver(referencePurpose.get)
        val functional = functionalReading.flatMap { reading =>
          MovePurpose.answer(packet, paths, ownedPair, reading,
            value => completeSelectedConditions(packet, paths, ownedPair, positions, local, value))
            .flatMap(RootQuestionSummary.realize(paths, ownedPair, _)).map(value => value.copy(
              json = value.json + ("move_purpose" -> reading.ownershipJson)))
        }
        if functional.nonEmpty then return deliver(functional.get)
        val partialReference = referenceReading.filterNot(_.completePurpose).flatMap(realizeReference)
        if partialReference.nonEmpty then return deliver(partialReference.get)
      if index == 0 && (detail \ "type").as[String] == "root_comparison" &&
          ownedPair.size == 2 && purposeOwners.size == 1 &&
          !packet.assembly.input.positionHistory.currentPosition.check.yes then
        val seed = CommentaryAnswer.Answer("selected_comparison", List(CommentaryAnswer.Section(
          List(CommentaryAnswer.Continuation(ownedPair.map(RootQuestionSummary.MoveRef(_, 0)))))), Nil, Nil)
        val purpose = ResponsePurposeReading.withAnswer(packet, paths, ownedPair, purposeOwners, local, seed,
          value => completeSelectedConditions(packet, paths, ownedPair, positions, local, value), rootUses)
        if purpose.question != seed.question then
          val reading = RootQuestionSummary.realize(paths, ownedPair, purpose).map(value => value.copy(
            json = value.json + ("first_target_response" -> purposeOwners.head.ownershipJson)))
          if reading.nonEmpty then return deliver(reading.get)
      val questionFocus = requests.flatMap { (recipient, context) =>
        val questionRefs = (recipient \ "detail" \ "question_refs").asOpt[List[JsObject]].getOrElse(Nil)
        context.flatMap(_.question).filter { focus =>
          val comparison = focus.owner.comparison
          val answer = focus.owner.answer
          val key = focus.key
          val witness = focus.mechanism
          def arm(line: String, move: String, occurrence: String) = ownedPair.find(path =>
            path.line.ref.id == line && path.certified.nonEmpty && path.ids == path.declared &&
              path.moves.startsWith(prefix :+ move) && path.ids.lift(index).contains(occurrence))
          val reference = arm(answer.referenceLineId, comparison.referenceMove, answer.referenceOccurrenceId)
          val alternative = arm(answer.alternativeLineId, comparison.alternativeMove, answer.alternativeOccurrenceId)
          questionRefs.exists(ref => (ref \ "proposal_id").asOpt[String].contains(focus.owner.proposalId) &&
            (ref \ "requirement_id").asOpt[String].contains(comparison.id)) &&
            comparison.prefixMovesUci == prefix && key.anchor == prefix && key.move == comparison.referenceMove &&
            (recipient \ "detail" \ "reference_move").asOpt[String].contains(comparison.referenceMove) &&
            (recipient \ "detail" \ "alternative_move").asOpt[String].contains(comparison.alternativeMove) &&
            (recipient \ "detail" \ "evaluation_relation").asOpt[String].contains(answer.evaluationRelation) &&
            alternative.nonEmpty && reference.exists { path =>
              val use = key.branch.size - 1
              val observedAction = for
                actor <- key.action.subject
                before <- path.positions.lift(use).flatMap(positions.get)
                current <- paths.pieceAfter(packet, path, actor.witness, use).flatten
                row <- before.view.legalRows.toList.flatten.find(row => row.move == key.branch.last && row.actor == current)
              yield if key.action.kind == "legal_capture" then
                key.action.target.flatMap(target => paths.pieceAfter(packet, path, target.witness, use).flatten) == row.capture &&
                  row.capture.nonEmpty
              else key.action.kind == "legal_move" && row.capture.isEmpty && key.action.square.contains(row.to.key) &&
                key.action.property == row.promotionRole
              val condition = (witness.before.path.source, witness.after.path.source, witness.before.at, witness.after.at) match
                case (CommentaryFact.Declared(a), CommentaryFact.Declared(b), CommentaryFact.Ply(n), CommentaryFact.Ply(m)) =>
                  a == b && n == index && m == index + 1 && a.take(m) == path.moves.take(m) &&
                    witness.before.predicate == witness.after.predicate
                case _ => false
              path.moves.startsWith(key.branch) && key.branch.size > index && observedAction.contains(true) && condition
            }
        }
      }.distinct
      // The root comparison owns this context once. Matching board states do not
      // share occurrences, physical-piece histories, evaluations or later evidence.
      val convergenceMeaning = ownedPair match
        case List(left, right) if (detail \ "type").as[String] == "root_comparison" &&
            List(left, right).forall(path => path.certified.nonEmpty && path.ids == path.declared) &&
            left.moves.lift(index) != right.moves.lift(index) =>
          left.positions.iterator.zip(right.positions.iterator).zipWithIndex.drop(index + 1)
            .find { case ((l, r), _) => l.fen == r.fen }.flatMap { case (_, count) =>
              import CommentaryAnswer.*
              import RootQuestionSummary.MoveRef
              val shared = left.moves.drop(count).zip(right.moves.drop(count)).takeWhile((l, r) => l == r).size
              val next = count + shared
              val convergence = Convergence((index until count).map(MoveRef(left, _)).toList,
                (index until count).map(MoveRef(right, _)).toList, fullState = true)
              val continuation = Option.when(shared > 0)(Continuation((count until next).map(MoveRef(left, _)).toList)).toList
              val split = Option.when(left.moves.lift(next).nonEmpty || right.moves.lift(next).nonEmpty)(
                Divergence(MoveRef(left, index), MoveRef(right, index),
                  left.moves.lift(next).map(_ => MoveRef(left, next)), right.moves.lift(next).map(_ => MoveRef(right, next)))).toList
              RootQuestionSummary.completePairAnswer(packet, paths, ownedPair, "selected_comparison",
                List(Section(List(convergence))),
                continuation.map(step => Section(List(step))) ++ split.map(step => Section(List(step))))
            }
        case _ => None
      val convergenceAnswer = convergenceMeaning.flatMap(RootQuestionSummary.realize(paths, ownedPair, _))
      val convergence = convergenceAnswer.filter(answer => answer.text.length + originalText.length + 1 <= limit).map(_.text).toList
      // ponytail: only immediate same-role trades followed by neutral recaptures.
      // More complex costs keep the complete overview until a closed answer exists.
      val exchangeMeaning = Option.when((detail \ "type").as[String] == "root_comparison" &&
          ownedPair.size == 2 && index == 0 && convergenceMeaning.isEmpty)(ownedPair).toList.flatten.flatMap { path =>
        def neutral(p: ObservedLineProtocol.PreparedPath): Boolean = p.certified.exists { (_, fact) =>
          p.ids == p.declared && p.moves.size >= 3 &&
            p.automaticTerminal.isEmpty && !fact.lineEvents.exists(event =>
              event.plyOffset == p.ids.size - 1 && Set(LineEventKind.Mate, LineEventKind.Stalemate)(event.kind)) &&
            fact.material.exists(material => material.events.forall(_.promotionGainCp == 0) &&
              material.captures.grouped(2).forall {
                case List(a, b) => b.plyOffset == a.plyOffset + 1 && b.recapture &&
                  a.side != b.side && a.capturedRole == b.capturedRole
                case _ => false
              })
        }
        for
          peer <- ownedPair.filterNot(_ == path)
          if neutral(path) && neutral(peer) && path.moves.head != peer.moves.head &&
            path.moves.head.take(2) == peer.moves.head.take(2) &&
            !path.certified.toList.flatMap(_._2.lineEvents).exists(event =>
              event.kind == LineEventKind.Check && event.plyOffset == 2)
          reading <- continuations.get(path.line.ref.id).toList
          actor <- positions.get(path.positions.head).flatMap(_.occupant(path.moves.head.take(2))).toList
          loss <- reading.events.find(_.plyOffset == 1).toList
          target <- loss.originalActor.toList
          recovery <- reading.events.find(_.plyOffset == 2).toList
          recapturer <- recovery.originalActor.toList
          if loss.originalVictim.contains(actor) && recovery.originalVictim.contains(target) &&
            actor.side != target.side && actor.role == target.role &&
            Set("knight", "bishop", "rook", "queen")(actor.roleName) &&
            path.certified.flatMap(_._2.material).exists(_.captures.headOption.exists(_.plyOffset == 1)) &&
            List(actor, target).forall(piece => paths.pieceAfter(packet, peer, piece, peer.moves.size).flatten.nonEmpty)
        yield
          RootQuestionSummary.completePairAnswer(packet, paths, ownedPair, "observed_exchange",
            List(CommentaryAnswer.Section(List(CommentaryAnswer.ExchangeRetention(
              (0 to 2).map(RootQuestionSummary.MoveRef(path, _)).toList,
              List(RootQuestionSummary.MoveRef(peer, 0), RootQuestionSummary.MoveRef(peer, peer.moves.size - 1)),
              actor.roleName, recapturer.roleName)))))
      }.flatten.headOption
      def alreadyOwned(effect: ComparisonEffect): Boolean =
        val value = effect.json(publicSource)
        // Exact occurrence facts can have different enclosing line summaries.
        // Keep the physical anchors and every non-summary premise when sharing ownership.
        val premises = effect.evidence.filterNot(id => packet.evidenceGraph.byId.get(id).exists {
          case EvidenceRecord(_, _: LineFactEvidence, _) => true
          case _ => false
        }).map(publicSource).toSet
        explained(value) || explained.exists { prior =>
          List("scope", "prefix_moves_uci", "actor", "target", "square").forall(key => (prior \ key) == (value \ key)) &&
            effect.occurrences.toSet.subsetOf((prior \ "occurrence_ids").as[List[String]].toSet) &&
            premises.subsetOf((prior \ "evidence_ids").as[List[String]].toSet)
        }
      val meanings = meaningEffects(ownedPair, index)
      val current = ownedPair.flatMap(path => currentEffects(path, index)) ++ meanings.filter(_.lines.size == 1)
      // A moving provider can lose a duty without blocking a stationary piece's
      // line. Compare that observed loss with the same provider in the other arm.
      val supportDifferences = ownedPair.flatMap { path =>
        for
          peer <- ownedPair.filter(_.line.ref != path.line.ref)
          if List(path, peer).forall(p => p.certified.nonEmpty && p.ids == p.declared)
          edge <- path.ids.lift(index).flatMap(edges.get).toList
          peerEdge <- peer.ids.lift(index).flatMap(edges.get).toList
          if edge.from == peerEdge.from && edge.evidence != peerEdge.evidence
          before <- positions.get(edge.from).toList
          after <- positions.get(edge.to).toList
          peerAfter <- positions.get(peerEdge.to).toList
          observation <- observations.getOrElse(edge.evidence.id, Nil)
          (prior, next) <- (observation.meaning match
            case MoveObservationMeaning.PieceControl(prior, next) if
                prior.state == PieceControlState.Present && next.state == PieceControlState.Absent => List(prior -> next)
            case _ => Nil)
          original <- prior.controller.filter(before.contains).toList
          actor <- next.controller.filter(after.contains).toList
          target <- prior.target.filter(before.contains).toList
          if original != actor && original.side == target.side && target.roleName != "king" &&
            next.target.contains(target) && after.contains(target) &&
            transported(original, edge, after).contains(actor) &&
            after.complete(RelationFactKind.GeometricControl) && !after.controlledBy(actor)(target.at)
          // ponytail: only current threats or an already explained loss; extend
          // relevance when another selected question needs a quiet support duty.
          if before.controllers(target.at, !target.side).nonEmpty || after.controllers(target.at, !target.side).nonEmpty ||
            current.exists(effect => effect.lines.head._1 == path.line.ref.id &&
              effect.actor == actor && effect.lostSupport.contains(target))
          peerActor <- transported(original, peerEdge, peerAfter).toList
          peerTarget <- transported(target, peerEdge, peerAfter).toList
          if peerAfter.controlledBy(peerActor)(peerTarget.at)
        yield
          val text = s"${label(edge)} gives up that ${actor.roleName}'s support of ${target.name}; " +
            s"after ${label(peerEdge)}, ${peerActor.name} still supports ${peerTarget.name}."
          ComparisonEffect(text, "pair_difference", List(path.line.ref.id -> (index + 1), peer.line.ref.id -> (index + 1)),
            prefix, edge.from, original, Some(target), target.at, List(edge.evidence.id, peerEdge.evidence.id),
            (observation.premises.map(_.id) ++ sourceRefs(peerAfter, peerAfter.sources(peerActor, List(peerTarget.at))) ++
              List(edge.evidence.id, peerEdge.evidence.id, before.view.occurrence.id,
                after.view.occurrence.id, peerAfter.view.occurrence.id)).distinct, geometricCost = true,
            meaning = Some(CommentaryAnswer.Section(List(CommentaryAnswer.ControlContrast(RootQuestionSummary.MoveRef(path, index), actor, Some(target),
              target.at, false, RootQuestionSummary.MoveRef(peer, index), peerActor, Some(peerTarget), peerTarget.at, true))))) -> actor
      }.distinct.filterNot((effect, _) => explained(effect.json(publicSource)))
      def historyKey(history: AttackHistory) =
        (history.path.line.ref.id, history.start, history.originalAttacker, history.originalTarget)
      def atQuestion(history: AttackHistory): Option[AttackHistory] =
        val path = history.path
        val start = history.start
        if start == index then Some(history)
        else for
          actor <- paths.pieceAfter(packet, path, history.originalAttacker, index + 1).flatten
          target <- paths.pieceAfter(packet, path, history.originalTarget, index + 1).flatten
        yield history.copy(
          opening = s"In the provided ${sans(path)(index)} line, after ${sans(path)(start)}, " +
            s"${history.effect.actor.name} attacks ${history.effect.target.get.name}; ",
          effect = history.effect.copy(prefix = prefix, position = path.positions(index + 1),
            actor = actor, target = Some(target), square = target.at,
            occurrences = path.ids(index) :: history.effect.occurrences,
            evidence = (path.ids(index) :: history.effect.evidence) ++ current.filter(effect =>
              effect.lines.map(_._1) == List(path.line.ref.id) && effect.use.exists(use =>
                use.offset == start && use.actor == history.effect.actor)).flatMap(_.evidence),
            attackPly = Some(start + 1)))
      // An immediate reply belongs to this choice. Later attacks need an exact
      // functional use, not just another move by the same piece somewhere in the PV.
      val functionalHistories = ownedPair.flatMap { path =>
        val uses = current.filter(_.lines.map(_._1) == List(path.line.ref.id)).flatMap(_.use)
        val mover = positions.get(path.positions(index)).flatMap(_.occupant(path.moves(index).take(2)))
        val originalMover = mover.flatMap(piece => paths.identitiesAfter(packet, path, index).collectFirst {
          case (root, Some(Some(current))) if current == piece => root
        })
        val starts = (List(index, index + 1) ++ uses.map(_.offset)).filter(_ < path.moves.size).distinct.sorted
        starts.flatMap { start =>
          attackOutcomes(path, start, if start <= index + 1 then Set.empty
            else uses.filter(_.offset == start).map(_.actor).toSet)
            .flatMap { history =>
            def piece(original: Piece, count: Int) = paths.pieceAfter(packet, path, original, count).flatten
            val alreadyTracked = start > index + 1 && byOccurrence.getOrElse(path.ids(index), Nil).exists(_.note.detail match
              case Detail.Attack(actor, target, _) =>
                piece(history.originalAttacker, index + 1).contains(actor) &&
                  piece(history.originalTarget, index + 1).contains(target)
              case _ => false)
            val counter = start == index && index + 3 < path.moves.size &&
              originalMover.contains(history.originalTarget) &&
              piece(history.originalAttacker, index + 1) == piece(history.originalAttacker, index + 2) &&
              byOccurrence.getOrElse(path.ids(index + 2), Nil).exists(_.note.detail match
                case Detail.Attack(actor, target, true) =>
                  piece(history.originalTarget, index + 3).contains(actor) &&
                    piece(history.originalAttacker, index + 3).contains(target)
                case _ => false)
            val openedReply = start == index + 1 && originalMover.contains(history.originalAttacker) &&
              byOccurrence.getOrElse(path.ids(start), Nil).exists(_.note.detail match
                case Detail.ChangedLines(_, opened, _) => opened.contains(history.effect.actor -> history.effect.target.get)
                case _ => false)
            val side = positions.get(path.positions(start)).flatMap(_.occupant(path.moves(start).take(2))).map(_.side)
            if alreadyTracked || (start <= index + 1 && !side.contains(history.effect.actor.side) && !counter && !openedReply) then None
            else atQuestion(history)
          }
        }
      }.distinctBy(historyKey)
      // A selected tactical explanation already owns its contacts. Carry only
      // those exact contacts into the existing history reader, not every later attack.
      val functionalKeys = functionalHistories.map(historyKey).toSet
      // Reuse the typed explanation's exact binding for both admission and shared
      // wording. A matching PV alone does not make it this recipient's answer.
      val tacticBindings = requests.flatMap(_._2).distinct.flatMap { context =>
        for
          tactic <- context.tactic
          anchor <- context.attack
          (question, lineId) <- tactic.owner
          if lineId == anchor.lineId && tactic.line == anchor.moves && anchor.cause.contains(tactic.actor) &&
            tactic.contacts.map((actor, target, _) => actor -> target) == anchor.contacts &&
            Set(question.answer.referenceLineId, question.answer.alternativeLineId) == ownedPair.map(_.line.ref.id).toSet &&
            question.comparison.prefixMovesUci == prefix && tactic.at.fromPly == tactic.line.size - 1 &&
            tactic.at.throughPly == tactic.line.size
          path <- ownedPair.find(path => path.line.ref.id == lineId && path.certified.nonEmpty &&
            path.ids == path.declared && path.moves.startsWith(tactic.line))
        yield context -> (question, path)
      }.toMap
      val ownedTactics = requests.zipWithIndex.flatMap { case ((recipient, contexts), owner) =>
        val detail = (recipient \ "detail").as[JsObject]
        val refs = (detail \ "question_refs").asOpt[List[JsObject]].getOrElse(Nil)
        contexts.filter(context => tacticBindings.get(context).exists { (question, _) =>
          val comparison = question.comparison
          val answer = question.answer
          def arm(line: String, move: String, occurrence: String) = ownedPair.exists(path =>
            path.line.ref.id == line && path.certified.nonEmpty && path.ids == path.declared &&
              path.moves.startsWith(prefix :+ move) && path.ids.lift(index).contains(occurrence))
          refs.exists(ref => (ref \ "proposal_id").asOpt[String].contains(question.proposalId) &&
            (ref \ "requirement_id").asOpt[String].contains(comparison.id)) &&
            (detail \ "reference_move").asOpt[String].contains(comparison.referenceMove) &&
            (detail \ "alternative_move").asOpt[String].contains(comparison.alternativeMove) &&
            (detail \ "evaluation_relation").asOpt[String].contains(answer.evaluationRelation) &&
            arm(answer.referenceLineId, comparison.referenceMove, answer.referenceOccurrenceId) &&
            arm(answer.alternativeLineId, comparison.alternativeMove, answer.alternativeOccurrenceId)
        }).map(owner -> _)
      }.toSet
      val contextHistories = requests.zipWithIndex.flatMap { case ((_, context), owner) =>
        context.map { part =>
          val histories = part.attack.toList.flatMap { anchor =>
            ownedPair.find(path => path.line.ref.id == anchor.lineId &&
              path.moves.startsWith(anchor.moves) && anchor.moves.size > index).toList.flatMap { path =>
              val start = anchor.moves.size - 1
              val declaredContacts = anchor.contacts.map((actor, target) => actor.witness -> target.witness).distinct
              val identities = paths.identitiesAfter(packet, path, start + 1)
              // A selected line-opening cause can also attack directly with the
              // moving piece. Read only its newly gained contacts at this exact
              // owned occurrence, retaining the ordinary attack-history proof.
              val direct = anchor.cause.toList.flatMap { cause =>
                paths.pieceAfter(packet, path, cause.witness, start).flatten.toList.filter(p =>
                  path.moves(start).take(2) == p.at).flatMap { _ =>
                  val moved = paths.pieceAfter(packet, path, cause.witness, start + 1).flatten
                  byOccurrence.getOrElse(path.ids(start), Nil).flatMap(_.note.detail match
                    // The same reply moves this attacked defender away from the
                    // declared target. That observed loss of support belongs to
                    // the selected task; unrelated contacts must not crowd it out.
                    case Detail.Attack(actor, target, true) if moved.contains(actor) && declaredContacts.exists { (provider, victim) =>
                        provider != cause.witness && path.moves.lift(start + 1).exists(_.take(2) == target.at) &&
                        paths.pieceAfter(packet, path, victim, start + 1).flatten.exists(t =>
                          t.side == target.side && positions(path.positions(start + 1)).controlledBy(target)(t.at) &&
                            paths.pieceAfter(packet, path, victim, start + 2).flatten.exists { nextVictim =>
                              identities.collectFirst { case (root, Some(Some(current))) if current == target => root }
                                .flatMap(paths.pieceAfter(packet, path, _, start + 2).flatten).exists { defender =>
                                  val afterReply = positions(path.positions(start + 2))
                                  afterReply.complete(RelationFactKind.GeometricControl) &&
                                    !afterReply.controlledBy(defender)(nextVictim.at)
                                }
                            })
                      } =>
                      identities.collectFirst { case (root, Some(Some(current))) if current == target =>
                        cause.witness -> root
                      }
                    case _ => None)
                }
              }
              val contacts = (declaredContacts ++ direct).distinct
              val actors = identities.collect {
                case (root, Some(Some(actor))) if contacts.exists(_._1 == root) => actor
              }.toSet
              val bound = if actors.isEmpty then Nil else attackOutcomes(path, start, actors).filter(history =>
                contacts.contains(history.originalAttacker -> history.originalTarget)).flatMap(atQuestion).distinctBy(historyKey)
              // Retain the original paragraph unless every stated attack can be
              // carried with its outcomes. A partial replacement would erase a target.
              if contacts.nonEmpty && bound.size == contacts.size && bound.exists(_.changes.nonEmpty) &&
                  !bound.exists(h => functionalKeys(historyKey(h)) || explained(h.effect.json(publicSource))) then
                // Opening another piece's attack can cost the clearing piece.
                // Keep its observed immediate capture, without inferring a forced sacrifice.
                val loss = for
                  cause <- anchor.cause.toList
                  moved <- paths.pieceAfter(packet, path, cause.witness, start).flatten.toList
                  if path.moves(start).take(2) == moved.at
                  before <- paths.pieceAfter(packet, path, cause.witness, start + 1).flatten.toList
                  if paths.pieceAfter(packet, path, cause.witness, start + 2).contains(None)
                  note <- byOccurrence.getOrElse(path.ids(start + 1), Nil)
                  if (note.note.detail match
                    case Detail.Capture(_, square, _) => square == before.at && note.before.contains(before)
                    case _ => false)
                  actor <- note.before.occupant(path.moves(start + 1).take(2)).toList
                  if !bound.exists(_.steps.exists(_.observation.contains(note.id)))
                yield HistoryStep(start + 1, s"${actor.name} captures ${before.name}.",
                  s" The captured ${before.roleName} played ${sans(path)(start)}.", noteEvidence(note),
                  observation = Some(note.id), captureRole = Some(CaptureRole.Plain))
                bound.zipWithIndex.map { (h, n) =>
                  val cost = if n == 0 then loss else Nil
                  val inIntroduction = declaredContacts.contains(h.originalAttacker -> h.originalTarget)
                  val extraOpening = Option.when(!inIntroduction) {
                    val actor = paths.pieceAfter(packet, path, h.originalAttacker, start + 1).flatten.get
                    val target = paths.pieceAfter(packet, path, h.originalTarget, start + 1).flatten.get
                    HistoryStep(start, s"${actor.name} attacks ${target.name}.", "", Nil,
                      observation = Some(h.openingObservation))
                  }.toList
                  h.copy(steps = (h.steps.filterNot(step => inIntroduction && step.offset == start) ++ cost ++ extraOpening).sortBy(_.offset),
                    introduced = true,
                    effect = h.effect.copy(
                      occurrences = (h.effect.occurrences ++ cost.map(row => path.ids(row.offset))).distinct,
                      evidence = (h.effect.evidence ++ cost.flatMap(_.evidence)).distinct))
                }
              else Nil
            }
          }
          (part, owner, histories)
        }
      }.distinct
      val followed = (functionalHistories ++ contextHistories.flatMap(_._3)).distinctBy(historyKey)
      // Compare only what both supplied arms have reached. A longer arm can keep
      // its later events in its own history, never donate them to the peer.
      def stateAt(history: AttackHistory, through: Int): Option[(Option[Piece], String)] =
        val events = history.changes.filter(_._1 < through)
        paths.pieceAfter(packet, history.path, history.originalTarget, through).flatMap {
          case None => Option.when(events.exists(_._2 == "target_captured"))(None -> "captured")
          case Some(target) =>
            paths.pieceAfter(packet, history.path, history.originalAttacker, through).flatMap {
              case None => Option.when(events.exists(_._2 == "capture"))(Some(target) -> "attacker_captured")
              case Some(actor) => positions.get(history.path.positions(through)).flatMap { position =>
                if position.controlledBy(actor)(target.at) then Some(Some(target) -> "attacked")
                else Option.when(position.complete(RelationFactKind.GeometricControl))(Some(target) -> "not_attacked")
              }
            }
        }
      val grouped = functionalHistories.groupBy(h => (h.start, h.originalAttacker, h.originalTarget))
      val paired = mutable.Set.empty[(String, Int, Piece, Piece)]
      def key(history: AttackHistory) = historyKey(history)
      def alreadyExplained(history: AttackHistory) = explained(history.effect.json(publicSource))
      // Share an actual response only within the same owned path and occurrence.
      // Its different qualifications and each physical target's outcome survive.
      def openingKey(history: AttackHistory) =
        (history.path.line.ref.id, history.start, "note:" + history.openingObservation)
      def stepKeys(history: AttackHistory) =
        val keys = history.steps.map(step => (history.path.line.ref.id, step.offset, step.eventKey)).toSet + openingKey(history)
        // A capture can also start an attack or be an escape. Those typed notes
        // differ, but belong to one actual capturing move on this owned path.
        val captures = (history.start :: history.steps.map(_.offset)).distinct.flatMap { offset =>
          byOccurrence.getOrElse(history.path.ids(offset), Nil).collect {
            case note if note.note.detail.isInstanceOf[Detail.Capture] =>
              (history.path.line.ref.id, offset, "note:" + note.id)
          }
        }
        // One physical move can evade an attack, abandon another target and
        // attack a new one. Keep its distinct typed meanings on the same event.
        val movements = (history.start :: history.steps.map(_.offset)).distinct.flatMap { offset =>
          List(history.originalAttacker, history.originalTarget).distinct.flatMap { original =>
            for
              before <- paths.pieceAfter(packet, history.path, original, offset).flatten
              after <- paths.pieceAfter(packet, history.path, original, offset + 1).flatten
              if before != after
            yield (history.path.line.ref.id, offset, "move:" + original.toString)
          }
        }
        keys ++ captures ++ movements
      val historyEvents = followed.map(history => historyKey(history) -> stepKeys(history)).toMap
      type ObservationEvent = (String, Int, String)
      def captureEvent(path: ObservedLineProtocol.PreparedPath, offset: Int, id: String): Option[ObservationEvent] =
        Option.when(path.ids.lift(offset).exists(occurrence => byOccurrence.getOrElse(occurrence, Nil).exists(note =>
          note.id == id && note.note.detail.isInstanceOf[Detail.Capture])))((path.line.ref.id, offset, id))
      def historyText(histories: List[AttackHistory], continuation: Boolean,
          checkedOccurrences: Set[String], sharedCaptures: mutable.Set[ObservationEvent],
          sharedObservations: mutable.Set[ObservationEvent], additions: Map[Int, List[String]],
          introductions: Map[Int, List[() => String]]): String =
        val history = histories.head
        val joined = histories.size > 1
        val openings = if !joined then Nil else histories.filterNot(h => h.introduced ||
          h.steps.exists(s => s.offset == h.start && s.opensAttack)).map { h =>
          val actor = paths.pieceAfter(packet, h.path, h.originalAttacker, h.start + 1).flatten.get
          val target = paths.pieceAfter(packet, h.path, h.originalTarget, h.start + 1).flatten.get
          HistoryStep(h.start, s"${actor.name} attacks ${target.name}.", "", Nil,
            observation = Some(h.openingObservation))
        }
        val described = mutable.Set.from(sharedCaptures.collect {
          case (line, offset, id) if line == history.path.line.ref.id => offset -> id
        })
        val events = (histories.flatMap(_.steps) ++ openings).groupBy(_.offset)
        val steps = (events.keySet ++ additions.keySet ++ introductions.keySet).toList.sorted.flatMap { offset =>
          val rows = events.getOrElse(offset, Nil)
          val checks = rows.filter(_.checkAction)
          val capture = byOccurrence.getOrElse(history.path.ids(offset), Nil)
            .find(_.note.detail.isInstanceOf[Detail.Capture])
          val canonical = if checks.size > 1 then
            checkSteps(history.path, List(offset), Some(offset), observations).filter(_._1 == offset)
            else Nil
          // Several histories may describe the same check differently. Share its
          // typed response only with already-owned proof and a separate capture
          // clause; an embedded capture must not disappear with the old wording.
          val compact = if canonical.nonEmpty &&
              capture.forall(note => rows.exists(s => s.observation.contains(note.id) && s.captureRole.nonEmpty)) &&
              canonical.flatMap(_._3).toSet.subsetOf(rows.flatMap(_.evidence).toSet) then
            rows.filterNot(_.checkAction) ++ canonical.map { (ply, action, proof, _) =>
              HistoryStep(ply, CommentaryEnglish.checkDetail(action) + ".", "", proof, checkAction = true)
            }
          else rows
          val eligible = compact.filterNot(step => (step.exchangeAddition &&
            openingCaptures.getOrElse(history.path.line.ref.id, Set.empty)(offset)) ||
            (step.checkAction && checkedOccurrences(history.path.ids(offset))))
          val actions = eligible.groupBy(_.eventKey).values.toList.sortBy(group => eligible.indexOf(group.head)).zipWithIndex.map { (group, clause) =>
            group.flatMap(_.observation).foreach(id => described += (offset -> id))
            // One physical action can remove a target, end another attack, and
            // close a delayed recovery. Keep all of those relational details.
            val step = group.find(_.recapture).getOrElse(group.head)
            val capture = step.observation.flatMap(captureEvent(history.path, offset, _))
            val observation = step.observation.map(id => (history.path.line.ref.id, offset, id))
            // Only explicitly separated capture clauses can be shared. A check,
            // evasion or role-specific relation is never inferred from a note ID.
            val repeated = capture.exists(sharedCaptures) && group.forall(_.captureRole.nonEmpty)
            val action = if observation.exists(sharedObservations) && group.forall(_.plainObservation) then ""
              else if repeated then
              val roles = group.flatMap(_.captureRole).toSet
              val relation = List(CaptureRole.Attacker -> "attacker", CaptureRole.Target -> "target").collect {
                case (role, label) if roles(role) => label
              }
              val roleText = if relation.isEmpty then Nil else
                val note = byOccurrence(history.path.ids(offset)).find(note => step.observation.contains(note.id)).get
                val victim = note.before.occupant(note.note.detail.asInstanceOf[Detail.Capture].at).get
                List(s"The captured ${victim.sideName} ${victim.roleName} on ${victim.at} was the ${relation.mkString(" and ")}.")
              (roleText ++ Option.when(roles(CaptureRole.Recapture))("This is the immediate recapture.") ++
                Option.when(roles(CaptureRole.LaterRecovery))("This removes that earlier capturer.")).mkString(" ")
              else if group.exists(_.withdrawal.nonEmpty) then
              val moves = group.flatMap(_.withdrawal)
              val (before, after, _) = moves.head
              val targets = moves.map(_._3).distinct
              s"${before.name} moves to ${after.at}, ending its ${if targets.size == 1 then "attack" else "attacks"} " +
                s"on ${targets.map(_.name).mkString(" and ")}."
              else if clause == 0 then step.action else
              byOccurrence.getOrElse(history.path.ids(offset), Nil).find(note => step.observation.contains(note.id))
                .map(_.note.detail) match
                case Some(Detail.Evades(target, attacker)) =>
                  // Castling also moves a rook. The evader need not be the UCI mover.
                  val beforeTarget = paths.identitiesAfter(packet, history.path, offset + 1).collectFirst {
                    case (root, Some(Some(piece))) if piece == target => root
                  }.flatMap(root => paths.pieceAfter(packet, history.path, root, offset).flatten).getOrElse(target)
                  s"${beforeTarget.name} moves out of ${attacker.name}'s attack."
                case Some(_: Detail.Capture) if step.action.headOption.exists(_.isLower) =>
                  val mover = positions(history.path.positions(offset)).occupant(history.path.moves(offset).take(2)).get
                  s"${mover.name} ${step.action}"
                case _ if step.action.headOption.exists(_.isLower) => step.action.head.toUpper.toString + step.action.tail
                case _ => step.action
            if action.nonEmpty && !repeated then
              capture.foreach(sharedCaptures.add)
              observation.foreach(sharedObservations.add)
            (action + group.map(_.detail).filter(_.nonEmpty).distinct.mkString).trim
          }.filter(_.nonEmpty).distinct
          val complete = actions ++ additions.getOrElse(offset, Nil)
          // Render the context after accounting for this event's actual losses,
          // then show its attack and response together at this exact occurrence.
          val context = introductions.getOrElse(offset, Nil).map(_()).filter(_.nonEmpty)
          val event = context ++ Option.when(complete.nonEmpty) {
            val move = sans(history.path)(offset)
            val description = complete.mkString(" ")
            if description.headOption.exists(_.isLower) then s"$move $description" else s"$move: $description"
          }
          Option.when(event.nonEmpty)(event.mkString(" "))
        }.foldLeft(List.empty[String]) { (paragraphs, event) =>
          // ponytail: wrap between whole events; one event can exceed this soft
          // length when its qualifications cannot safely be separated.
          paragraphs.lastOption.filter(_.length + event.length + 1 <= 600)
            .fold(paragraphs :+ event)(last => paragraphs.init :+ s"$last $event")
        }.mkString("\n\n")
        val opening = if histories.forall(_.introduced) then ""
          else if joined || history.steps.exists(s => s.offset == history.start && s.opensAttack) then
            s"In the provided ${sans(history.path)(index)} line, "
          else if continuation && history.start == index then
          s"In the provided continuation beginning ${sans(history.path)(index)}, "
          else history.opening
        // A displayed capture already gives the original target's fate. Surviving
        // targets still need a bounded outcome; survival never means all attacks ended.
        val endpoints = histories.filterNot(h => h.changes.exists { (offset, kind, _) =>
          kind == "target_captured" && h.steps.exists(s => s.offset == offset && s.observation.exists(id => described(offset -> id)))
        })
        val inactive = endpoints.filter(h => h.endTarget.nonEmpty && h.endAttack.isEmpty)
          .distinctBy(_.originalTarget)
        val inactiveTargets = inactive.map(_.originalTarget).toSet
        val groupedSurvivors = if inactive.size < 2 then Nil else
          val names = inactive.map { h =>
            val current = h.endTarget.get
            if current == h.originalTarget then current.name
            else s"${current.name} (originally the ${h.originalTarget.roleName} on ${h.originalTarget.at})"
          }
          List(s"At this line's end, these original targets remain: ${names.mkString("; ")}.")
        val endings = (endpoints.filterNot(h => groupedSurvivors.nonEmpty && inactiveTargets(h.originalTarget) &&
            h.endAttack.isEmpty).map { h =>
          val target = s"The ${h.originalTarget.sideName} ${h.originalTarget.roleName} originally on ${h.originalTarget.at}"
          h.endTarget.fold(s"$target is captured within this line.") { piece =>
            s"$target remains on ${piece.at} at this line's end." + h.endAttack.fold("") { attacker =>
              val actor = if joined then attacker.name else "The original attacker"
              s" $actor ${if h.resumed then "attacks it again" else "still attacks it"} there."
            }
          }
        }.distinct ++ groupedSurvivors).mkString(" ")
        List(s"$opening$steps".trim, endings).filter(_.nonEmpty).mkString(" ")
      // A bundle answers this exact question and retains its qualifications.
      // Admission and presentation use the same order; omitted bundles own nothing.
      case class CurrentFunction(path: ObservedLineProtocol.PreparedPath, notes: List[LocalCandidate], offset: Int = index,
          comparedAttack: Boolean = false):
        def note = notes.head
        // Select the certified relationship independently of its wording.
        def details: List[Detail] = notes.map(_.note.detail).distinct.map {
          case Detail.ChangedLines(_, opened, _) if offset != index =>
            val capture = path.moves(offset + 1)
            Detail.ChangedLines(Nil, opened.filter((actor, target) =>
              actor.at == capture.take(2) && target.at == capture.drop(2).take(2)), Nil)
          case value => value
        }
        private def moves = if comparedAttack then notes.map(n => label(n.edge)).distinct else List(sans(path)(offset))
        def atMove = CommentaryEnglish.currentFunction(details, moves, None)
        def text = CommentaryEnglish.currentFunction(details, moves,
          Option.when(offset != index)(sans(path)(index) -> note.before.occupant(path.moves(offset).take(2)).get))
      case class Explanation(effects: List[ComparisonEffect], histories: List[AttackHistory] = Nil,
          functions: List[CurrentFunction] = Nil, textParts: List[Context] = Nil, owner: Option[Int] = None)
      // Display fragments retain their source identity; text is never their key.
      case class Fragment(text: String, effects: Set[ComparisonEffect] = Set.empty,
          context: Option[Context] = None)
      case class Paragraph(before: List[Fragment], histories: List[AttackHistory],
          actions: List[(String, LocalCandidate, String)], after: List[Fragment], current: Boolean,
          effects: List[ComparisonEffect], functions: List[CurrentFunction], owner: Option[Int], qualifications: List[String],
          context: Option[Context] = None)
      val relationChanges = differences.map(_._2) ++ supportDifferences.map(_._1)
      def sameSupportLoss(contrast: ComparisonEffect, line: String, actor: Piece, target: Piece): Boolean =
        supportDifferences.exists((effect, mover) => effect == contrast && mover == actor &&
          effect.lines.head._1 == line && effect.target.contains(target))
      def costs(lines: Set[String]): List[ComparisonEffect] =
        val costLines = relationChanges.filter(e => e.geometricCost && lines(e.lines.head._1))
          .map(_.lines.head._1).toSet
        relationChanges.filter(e => costLines(e.lines.head._1))
      def withCosts(effects: List[ComparisonEffect]): List[ComparisonEffect] =
        val lines = effects.flatMap(_.lines.map(_._1)).toSet
        // A later attack must carry its independently verifiable function in this
        // answer even when another answer has already displayed that function.
        (effects.filter(e => e.use.nonEmpty || !alreadyOwned(e)) ++
          costs(lines)).distinct
      def usesHistory(effect: ComparisonEffect, history: AttackHistory): Boolean =
        effect.lines.map(_._1) == List(history.path.line.ref.id) &&
          (effect.use.exists(use => use.offset == history.start && paths.pieceAfter(packet, history.path,
            history.originalAttacker, use.offset + 1).flatten.contains(use.actor)) ||
            (effect.scope == "relative_pin" && history.start == index &&
              effect.actor == history.effect.actor && effect.target == history.effect.target))
      def supportsTarget(effect: ComparisonEffect, history: AttackHistory): Boolean =
        effect.immediate && effect.actor.side == history.effect.target.get.side &&
          effect.target == history.effect.target && effect.lines.map(_._1) == List(history.path.line.ref.id)
      val comparedTargets = mutable.Set.empty[(Int, Piece, Piece)]
      val historyBundles = functionalHistories.flatMap { history =>
        if paired(key(history)) || alreadyExplained(history) then Nil
        else
          val peer = grouped((history.start, history.originalAttacker, history.originalTarget)).find { other =>
            val through = math.min(history.path.moves.size, other.path.moves.size)
            val leftEvents = history.changes.filter(_._1 < through)
            val rightEvents = other.changes.filter(_._1 < through)
            other.path.line.ref != history.path.line.ref && (leftEvents.nonEmpty || rightEvents.nonEmpty) &&
              stateAt(history, through).exists(left => stateAt(other, through).exists(right =>
                left != right || leftEvents != rightEvents))
          }
          peer.foreach(value => paired += key(value))
          if peer.nonEmpty then comparedTargets += ((history.start, history.originalAttacker, history.originalTarget))
          val fresh = (history :: peer.toList).filterNot(alreadyExplained)
          val premises = current.filter(effect => fresh.exists(h => usesHistory(effect, h) || supportsTarget(effect, h)))
          if peer.isEmpty && history.changes.isEmpty then Nil
          else List(Explanation(withCosts(premises) ++ fresh.map(_.effect), fresh))
      }
      val checks = current.filter(_.scope == "check_sequence").filterNot(alreadyOwned).map { check =>
        val premises = current.filter(effect => effect.scope != "check_sequence" && effect.use.exists(use =>
          ownedPair.find(path => effect.lines.map(_._1) == List(path.line.ref.id)).exists(path =>
            check.lines.map(_._1) == List(path.line.ref.id) && check.occurrences.contains(path.ids(use.offset)))))
        Explanation(withCosts(premises :+ check))
      }
      def uses(bundle: Explanation) = bundle.effects.flatMap(effect =>
        effect.use.map(use => (effect.lines.head._1, use))).toSet
      // Several targets or a check can belong to one actual use. Close that
      // finite dependency before applying the text limit, never one claim at a time.
      val usedFunctions = current.filter(effect => effect.use.nonEmpty && !alreadyOwned(effect))
        .map(effect => Explanation(withCosts(List(effect))))
      val linked = (checks ++ historyBundles ++ usedFunctions).foldLeft(List.empty[Explanation]) { (all, next) =>
        val (related, separate) = all.partition(bundle => uses(bundle).intersect(uses(next)).nonEmpty)
        if related.isEmpty then all :+ next
        else
          val merged = related :+ next
          separate :+ Explanation(merged.flatMap(_.effects).distinct,
            merged.flatMap(_.histories).distinctBy(historyKey))
      }
      val boundEffects = linked.flatMap(_.effects).toSet
      def standalone(effects: List[ComparisonEffect]) = effects.filterNot(e => alreadyOwned(e) || boundEffects(e))
        .map(effect => Explanation(withCosts(List(effect))))
      val relationBundles = relationChanges.map(_.lines.head._1).distinct.map(line =>
        Explanation(relationChanges.filter(_.lines.head._1 == line)))
      def answers(focus: CommentaryIdea.QuestionFocus, bundle: Explanation): Boolean =
        val path = ownedPair.find(_.line.ref.id == focus.owner.answer.referenceLineId).get
        val condition = focus.mechanism.after.predicate
        val provider = condition.subject.flatMap(piece => paths.pieceAfter(packet, path, piece.witness, index + 1).flatten)
        val use = focus.key.branch.size - 1
        val actor = focus.key.action.subject.flatMap(piece => paths.pieceAfter(packet, path, piece.witness, use + 1).flatten)
        bundle.textParts.exists(_.question.contains(focus)) || bundle.effects.exists { effect =>
          effect.lines.exists(_._1 == path.line.ref.id) && (
            effect.use.exists(value => value.offset == use && actor.contains(value.actor)) ||
            (effect.scope == "support_retention" && effect.lines == List(path.line.ref.id -> (use + 1)) &&
              condition.kind == "control" && provider.contains(effect.actor) &&
              effect.target.exists(target => condition.square.contains(target.at))) ||
            (condition.kind == "control" && effect.immediate && effect.actions.exists(_.note.detail match
              case Detail.Support(helper, target, _, _) => provider.contains(helper) && condition.square.contains(target.at)
              case _ => false)))
        }
      def answersQuestion(bundle: Explanation): Boolean = questionFocus.exists(answers(_, bundle))
      def coreImmediate(effect: ComparisonEffect): Boolean = effect.immediate && effect.actions.exists { note =>
        note.note.detail match
          // Existing support and quiet new support remain optional unless an
          // owned question or observed use already requires that exact condition.
          case Detail.Support(_, _, opposition, gained) => gained && opposition.nonEmpty
          case Detail.Evades(_, _) => true
          case _ => false
      }
      val immediateBundles = ownedPair.flatMap { path =>
        val effects = current.filter(effect => effect.immediate && effect.lines.head._1 == path.line.ref.id &&
          !alreadyOwned(effect) && !boundEffects(effect))
        // A necessary evasion or attacked support must not promote every unrelated
        // quiet support on the same arm. Each tier still shares its exact witnesses.
        val (answer, other) = effects.partition(effect => answersQuestion(Explanation(List(effect))))
        val (essential, quiet) = other.partition(coreImmediate)
        List(answer, essential, quiet).filter(_.nonEmpty).map(values => Explanation(withCosts(values)))
      }
      val immediateFallbacks = current.filter(effect => effect.immediate && boundEffects(effect) && !alreadyOwned(effect))
        .map(effect => Explanation(withCosts(List(effect))))
      // Reuse the existing typed observation at this question's move. These are
      // current facts, not attack histories or invented future uses.
      // Establish a common target before the choices' different consequences.
      // Each arm must supply its own current mover's attack on the same piece
      // from the common position; a later PV attack cannot supply either half.
      val comparedAttacks = ownedPair match
        case List(left, right) if (detail \ "type").as[String] == "root_comparison" &&
            ownedPair.forall(path => path.certified.nonEmpty && path.ids == path.declared) &&
            left.moves.lift(index) != right.moves.lift(index) =>
          def attacks(path: ObservedLineProtocol.PreparedPath) =
            path.ids.lift(index).toList.flatMap(id => byOccurrence.getOrElse(id, Nil)).filter { note =>
              note.note.detail match
                case Detail.Attack(actor, target, _) =>
                  note.after.occupant(path.moves(index).drop(2).take(2)).contains(actor) &&
                    note.before.contains(target) && note.after.contains(target)
                case _ => false
            }
          attacks(left).flatMap { first =>
            val Detail.Attack(_, target, _) = first.note.detail: @unchecked
            attacks(right).find(_.note.detail match
              case Detail.Attack(_, other, _) => target == other
              case _ => false).map(second => CurrentFunction(left, List(first, second), comparedAttack = true))
          }
        case _ => Nil
      val currentFunctions = (comparedAttacks ++
        (if (detail \ "type").as[String] == "root_comparison" || prefix.nonEmpty then ownedPair else Nil)
        .flatMap(path => byOccurrence.getOrElse(path.ids(index), Nil).collect {
        case note if !comparedAttacks.exists(_.notes.contains(note)) && (note.note.detail match
          case Detail.Development(_, _, _, squares) => squares.nonEmpty ||
            !current.exists(effect => effect.scope == "piece_route" && effect.lines.head._1 == path.line.ref.id)
          case Detail.Castle(_, _) | Detail.PawnStructure(_, _) => true
          case Detail.Capture(_, _, _) => !openingFactCaptures.getOrElse(path.line.ref.id, Set.empty)(index)
          case Detail.Attack(actor, target, true) =>
            positions.get(path.positions(index)).flatMap(_.occupant(path.moves(index).take(2))).exists(_.side == actor.side) &&
              !followed.exists(history =>
            history.path.line.ref == path.line.ref && history.start == index &&
              history.effect.actor == actor && history.effect.target.contains(target) &&
              (history.changes.nonEmpty || comparedTargets((history.start, history.originalAttacker, history.originalTarget))))
          case Detail.RookConnection(_, _) | Detail.RookFile(_, _, _) |
              Detail.PassedPawn(_, _) | Detail.PawnBlock(_, _, _) => true
          case _ => false) => CurrentFunction(path, List(note))
      })).groupBy(value => value.note.note.detail match
        case Detail.RookConnection(actor, target) => (value.note.edge.evidence.id, Set(actor, target))
        case Detail.PawnBlock(to, pawn, _) => (value.note.edge.evidence.id, (to, pawn))
        case _ => (value.note.edge.evidence.id, value.note.note.detail)).values.toList
        .map(group => group.head.copy(notes = group.flatMap(_.notes).distinct))
        .sortBy(value => (ownedPair.indexOf(value.path), value.note.index))
        .map(value => Explanation(costs(if value.comparedAttack then ownedPair.map(_.line.ref.id).toSet
          else Set(value.path.line.ref.id)), functions = List(value)))
      val focalLine = Option.when((detail \ "type").as[String] == "root_comparison")(
        (candidate \ "root_occurrence_id").asOpt[String].flatMap(id => ownedPair.find(_.ids.lift(index).contains(id)))
          .map(_.line.ref.id)).flatten.orElse((detail \ "reference_line_id").asOpt[String])
      def basicAction(value: CurrentFunction): Boolean = value.note.note.detail match
        case Detail.Development(_, _, _, _) | Detail.Castle(_, _) | Detail.Capture(_, _, _) |
            Detail.PawnStructure(_, _) | Detail.Attack(_, _, _) => true
        case _ => false
      def currentAction(bundle: Explanation): Boolean = bundle.functions.exists(_.comparedAttack) || focalLine.exists(line =>
        bundle.effects.exists(effect => effect.lines.exists(_._1 == line) &&
          (Set("support_use", "clearance_use")(effect.scope) ||
            // Reoccupation of the cleared square uses the existing line-history
            // contract; immediate evasions and separate handoffs are not clearances.
            (effect.scope == "line_history" && effect.use.nonEmpty && !effect.immediate) ||
            (meanings.contains(effect) && effect.scope != "pawn_option") ||
            (effect.scope == "piece_route" && ownedPair.find(_.line.ref.id == line).exists(path =>
              !byOccurrence.getOrElse(path.ids(index), Nil).exists(_.note.detail.isInstanceOf[Detail.Capture]))))) ||
          bundle.functions.exists(value => value.path.line.ref.id == line && basicAction(value)))
      def answersTactic(bundle: Explanation): Boolean =
        bundle.owner.exists(owner => bundle.textParts.exists(context => ownedTactics(owner -> context)))
      def core(bundle: Explanation): Boolean = answersQuestion(bundle) || answersTactic(bundle) || bundle.histories.nonEmpty || bundle.effects.exists(effect =>
        (meanings.contains(effect) && (effect.scope != "pawn_option" || effect.lines.exists(_._2 > index + 1))) ||
          effect.use.nonEmpty || effect.scope == "check_sequence" || coreImmediate(effect)) ||
        bundle.functions.exists(basicAction)
      // An independently received answer still needs the condition it explains,
      // even when the root comparison has already displayed that exact relation.
      val ownedQuestionConditions = current.filter(effect => effect.scope == "support_retention" && alreadyOwned(effect) &&
        questionFocus.exists(focus => answers(focus, Explanation(List(effect)))))
        .map(effect => Explanation(effect :: costs(effect.lines.map(_._1).toSet)))
      val physicalBundles = (meanings.filter(_.lines.size == 2).map(e => Explanation(withCosts(List(e)))) ++
        ownedQuestionConditions ++ linked ++ relationBundles ++ immediateBundles ++
        standalone(current.filter(effect => effect.scope != "check_sequence" &&
          !effect.immediate)) ++ currentFunctions).map { bundle =>
            val followups = bundle.effects.flatMap(e => e.followupNotes.flatMap(note =>
              ownedPair.find(_.line.ref.id == e.lines.head._1).map(path =>
                CurrentFunction(path, List(note), path.ids.indexOf(note.edge.evidence.id)))))
            bundle.copy(functions = (bundle.functions ++ followups).distinctBy(_.note.id))
          }
      val bundles = contextHistories.flatMap { (text, owner, histories) =>
        val related = text.question.filter(questionFocus.contains).toList.flatMap(focus => physicalBundles.filter(answers(focus, _)))
        val physicalCondition = text.question.filter(questionFocus.contains).exists { focus =>
          val condition = focus.mechanism.after.predicate
          val path = ownedPair.find(_.line.ref.id == focus.owner.answer.referenceLineId).get
          val provider = condition.subject.flatMap(piece => paths.pieceAfter(packet, path, piece.witness, index + 1).flatten)
          related.flatMap(_.effects).exists(effect => effect.scope == "support_retention" &&
            effect.lines.map(_._1) == List(path.line.ref.id) && effect.absorbedNotes.exists(id =>
              byOccurrence.getOrElse(path.ids(index), Nil).exists(note => note.id == id && (note.note.detail match
                case Detail.Support(helper, target, _, _) => condition.kind == "control" &&
                  provider.contains(helper) && condition.square.contains(target.at)
                case _ => false))))
        }
        val wording = text.questionTest.filter(_ => physicalCondition)
          .fold(text)(test => text.copy(text = test))
        // The question and its matched physical explanation are offered whole,
        // including the existing costs/replies. Keep the original physical
        // bundles as fallbacks if that larger explanation cannot fit.
        val required = if ownedTactics(owner -> text) then histories else Nil
        val premises = current.filter(effect => required.exists(h => usesHistory(effect, h) || supportsTarget(effect, h)))
        val complete = Explanation((related.flatMap(_.effects) ++ withCosts(premises ++ required.map(_.effect))).distinct,
          (related.flatMap(_.histories) ++ required).distinctBy(historyKey),
          related.flatMap(_.functions).distinct, List(wording), Some(owner))
        // If the complete explanation cannot fit, its original bounded text
        // remains available. Only an admitted paragraph can consume it.
        complete :: Option.when(required.nonEmpty)(Explanation(costs(required.map(_.path.line.ref.id).toSet),
          textParts = List(text), owner = Some(owner))).toList
      } ++ physicalBundles
      val orderedBundles = bundles.sortBy { bundle =>
          val owners = bundle.effects.filterNot(_.scope == "pair_difference").flatMap(_.lines.map(_._1)) ++
            bundle.functions.map(_.path.line.ref.id)
          val focal = focalLine.exists(line => if owners.nonEmpty then owners.contains(line)
            else bundle.effects.exists(_.lines.exists(_._1 == line)))
          val checked = bundle.effects.exists(effect => effect.scope == "check_sequence" &&
            (!focal || focalLine.exists(line => effect.lines.exists(_._1 == line))))
          // Explain both choices' actions and observed uses before independent
          // static context. Each accepted action still carries its costs/replies.
          (if answersQuestion(bundle) then 0 else if answersTactic(bundle) then
              (if bundle.histories.isEmpty then 1 else 2) else if core(bundle) then 3 else 4,
            if bundle.textParts.exists(_.question.exists(questionFocus.contains)) then 0 else 1,
            if bundle.functions.exists(_.comparedAttack) then 0 else 1,
            if focal then 0 else 1, if checked then 0
            else if currentAction(bundle) then 1 else 2)
        }
      val selected = mutable.ListBuffer.empty[ComparisonEffect]
      val selectedFunctions = mutable.ListBuffer.empty[CurrentFunction]
      val paragraphs = mutable.ListBuffer.empty[Paragraph]
      var admittedConvergence = List.empty[String]
      // Several quiet supports are one current action, with separate witnesses.
      // Group identical qualifications; never hide an attacked target in a quiet list.
      def supportTexts(effects: List[ComparisonEffect]): (Set[ComparisonEffect], List[Fragment]) =
        val rows = effects.flatMap(effect => Option.when(effect.immediate)(effect.actions).toList.flatten.collect {
          case note if note.note.detail.isInstanceOf[Detail.Support] =>
            val Detail.Support(actor, target, opposition, gained) = note.note.detail: @unchecked
            (effect, actor, target, opposition, gained)
        })
        val texts = rows.groupBy(row => (row._1.lines, row._1.prefix, row._4, row._5)).values.toList
          .sortBy(group => rows.indexOf(group.head)).flatMap { group =>
            val multipleProviders = group.groupBy(_._3).values.toList.filter(_.size > 1)
              .sortBy(values => group.indexOf(values.head))
            val grouped = multipleProviders.flatten.map(_._1).toSet
            val remaining = group.filterNot(row => grouped(row._1)).groupBy(_._2).values.toList
              .sortBy(values => group.indexOf(values.head))
            (multipleProviders ++ remaining).map { values =>
              val text = if values.size == 1 then values.head._1.text
              else
                val actors = values.map(_._2).distinct
                val targets = values.map(_._3).distinct
                val gained = values.head._5
                val verb = if gained then (if actors.size == 1 then "adds support to" else "add support to")
                  else if actors.size == 1 then "keeps supporting" else "keep supporting"
                val move = sans(ownedPair.find(_.line.ref.id == values.head._1.lines.head._1).get)(index)
                s"After $move, ${actors.map(_.possessiveName).mkString(" and ")} $verb ${targets.map(_.possessiveName).mkString(" and ")}" +
                  (if values.head._4.isEmpty then "." else
                    s", which ${if targets.size == 1 then "is" else "are"} under attack from ${values.head._4.map(_.possessiveName).mkString(", ")}.")
              Fragment(text, values.map(_._1).toSet)
            }
          }
        rows.map(_._1).toSet -> texts
      // Each existing candidate keeps its mandatory answer and text limit. Shared
      // histories are rendered once, then complete explanations fill those fields.
      case class Rendered(text: String, current: Boolean, effects: List[ComparisonEffect],
          functions: List[CurrentFunction], owner: Option[Int], qualifications: List[String],
          meaning: List[Paragraph])
      val originals = requests.map((value, _) => (value \ "detail" \ "text").asOpt[String].getOrElse(""))
      def textFor(owner: Int, parts: List[Rendered]): String =
        val base = (originals(owner) :: parts.flatMap(_.qualifications).distinct).filter(_.nonEmpty).mkString(" ")
        val rendered = parts.filter(_.text.nonEmpty)
        val content = if owner > 0 || focalLine.exists(line => openingCaptures.getOrElse(line, Set.empty)(index)) ||
            (detail \ "type").as[String] != "root_comparison" then
          (base :: rendered.map(_.text)).filter(_.nonEmpty).mkString("\n\n")
        else if focalLine.exists(line => openingCaptures.getOrElse(line, Set.empty).nonEmpty) then
          val (action, remaining) = rendered.partition(_.current)
          (action.map(_.text) ++ List(base) ++ remaining.map(_.text)).filter(_.nonEmpty).mkString("\n\n")
        else (rendered.map(_.text) :+ base).filter(_.nonEmpty).mkString("\n\n")
        (Option.when(owner == 0)(admittedConvergence).toList.flatten :+ content).filter(_.nonEmpty).mkString("\n\n")
      def bodies(admitted: List[Paragraph], checkedOccurrences: Set[String], share: Boolean = true): Option[List[List[Rendered]]] =
        // Reserve a question before optional root detail without reversing the
        // card's reading order or moving a shared capture out of its root body.
        val parts = if requests.size == 1 then admitted else admitted.sortBy(_.owner.getOrElse(0))
        // Admission stays per explanation. Only admitted histories share a timeline;
        // an omitted bundle cannot consume an event or make a larger bundle mandatory.
        val histories = parts.flatMap(_.histories)
        val order = histories.zipWithIndex.map((history, offset) => historyKey(history) -> offset).toMap
        def sameContextOwner(left: AttackHistory, right: AttackHistory): Boolean =
          left.introduced && right.introduced && parts.exists(part => part.histories.contains(left) &&
            part.owner.nonEmpty && parts.exists(other => other.histories.contains(right) && other.owner == part.owner))
        val groups = histories.foldLeft(List.empty[List[AttackHistory]]) { (all, next) =>
          val keys = historyEvents(historyKey(next))
          val (related, separate) = all.partition(_.exists(h => historyEvents(historyKey(h)).exists(keys) &&
            (parts.exists(part => part.histories.contains(h) && part.histories.contains(next)) ||
              (share && ((!h.introduced && !next.introduced) || sameContextOwner(h, next))))))
          separate :+ (related.flatten :+ next).sortBy(h => order(historyKey(h)))
        }
        def owner(group: List[AttackHistory]): Int =
          val members = parts.indices.filter(i => parts(i).histories.exists(group.contains))
          // Selected contacts keep every original introduction. Their shared
          // timeline follows those introductions, within the same receiving field.
          if group.forall(_.introduced) then members.last
          else members.find(i => parts(i).before.nonEmpty).getOrElse(members.head)
        val headings = mutable.Set.empty[(Int, Piece, Piece)]
        val shareEvents = share && parts.exists(_.context.exists(_.tactic.nonEmpty))
        // These captures occur in the mandatory original exchange summary. Other
        // events enter this set only when this invocation actually renders them.
        val shareOpening = share && requests.size == 1 && openingCaptures.nonEmpty
        val sharedCaptures = mutable.Set.from(if !shareEvents && !shareOpening then Nil else ownedPair.flatMap(path =>
          openingCaptures.getOrElse(path.line.ref.id, Set.empty).toList.flatMap(offset =>
            byOccurrence.getOrElse(path.ids(offset), Nil).flatMap(note => captureEvent(path, offset, note.id)))))
        val sharedObservations = mutable.Set.empty[ObservationEvent]
        val described = histories.flatMap(h => (h.openingObservation :: h.steps.flatMap(_.observation))
          .map(id => h.path.line.ref.id -> id)).toSet
        val embeddedActions = mutable.Map.empty[(String, String), Int]
        val embeddedContexts = mutable.Set.empty[Int]
        case class TacticKey(owner: Option[Int], answer: CommentaryInvestigation.Answer, lineId: String, moves: List[String],
            at: CommentaryFact.Event, actor: CommentaryFact.Piece, scope: Option[(List[String], String, Int)])
        val writtenTactics = mutable.Map.empty[TacticKey, Set[CommentaryProposal.TacticClause]]
        def contextText(part: Paragraph): Option[String] = part.context.flatMap { context =>
          for
            tactic <- context.tactic
            recipient <- part.owner
            (question, path) <- tacticBindings.get(context)
            receipt = question.answer
            lineId = path.line.ref.id
          yield
            // Share wording across recipients only inside this already validated
            // co-displayed card. Their answers, histories and proofs keep their owners.
            val key = TacticKey(Option.when(!share)(recipient), receipt, lineId, tactic.line, tactic.at, tactic.actor, context.tacticScope)
            val prior = writtenTactics.getOrElse(key, Set.empty)
            val losses = (tactic.loss.toList ++ tactic.departures.collect {
              case (event, blocker, _, true) => event -> blocker
            }).distinct
            val observedLoss = if !shareEvents then Nil else losses.collect {
              case (event, victim) if event.throughPly == event.fromPly + 1 && event.fromPly >= 0 &&
                  event.throughPly <= tactic.line.size && paths.pieceAfter(packet, path, victim.witness, event.throughPly).contains(None) &&
                  paths.pieceAfter(packet, path, victim.witness, event.fromPly).flatten.exists(before =>
                    byOccurrence.getOrElse(path.ids(event.fromPly), Nil).exists(note =>
                      note.before.contains(before) && (note.note.detail match
                        case Detail.Capture(_, square, _) => square == before.at &&
                          sharedCaptures((lineId, event.fromPly, note.id))
                        case _ => false))) => CommentaryProposal.TacticClause.Loss(event, victim)
            }
            val earlierLosses = writtenTactics.iterator.filter { (previous, _) =>
              previous.owner == key.owner && previous.answer == key.answer && previous.lineId == lineId
            }.flatMap(_._2.collect { case loss: CommentaryProposal.TacticClause.Loss => loss }).toSet
            val omitted = prior ++ observedLoss ++ earlierLosses
            // The bound path resolves the same physical piece at each event.
            // Contacts use the resulting position; losses/departures use the prior one.
            val text = tactic.text(paths.notation, context.tacticScope, omitted, includeQualification = false,
              locate = (piece, ply) => paths.pieceAfter(packet, path, piece.witness, ply).flatten.map(_.name))
            writtenTactics(key) = prior ++ tactic.clauses
            text
        }
        val historyParts = groups.sortBy(owner).map { group =>
          val path = group.head.path
          val introductions = if !share || !group.forall(_.introduced) then Map.empty[Int, List[() => String]] else
            parts.zipWithIndex.flatMap { (part, partIndex) =>
              for
                context <- part.context.toList
                tactic <- context.tactic.toList
                if part.owner.nonEmpty && !embeddedContexts(partIndex) &&
                  tacticBindings.get(context).exists(_._2.line.ref == path.line.ref) &&
                  group.exists(h => part.histories.contains(h) && h.start == tactic.at.fromPly)
              yield tactic.at.fromPly -> (() => {
                embeddedContexts += partIndex
                contextText(part).get
              })
            }.groupMap(_._1)(_._2)
          // A selected bundle already owns these notes. Place them at their
          // exact occurrence, only inside that bundle's connected history.
          val additions = parts.filter(_.histories.exists(group.contains)).flatMap(_.actions).flatMap { (line, note, identity) =>
            val at = path.ids.indexOf(note.edge.evidence.id)
            val key = line -> note.id
            if line == path.line.ref.id && !described(key) && !embeddedActions.contains(key) &&
                group.exists(h => h.start == at || h.steps.exists(_.offset == at)) then
              embeddedActions(key) = owner(group)
              List(at -> (note.note.detail.text + identity))
            else Nil
          }.groupMap(_._1)(_._2)
          val comparisons = group.map(h => (h.start, h.originalAttacker, h.originalTarget)).distinct
            .filter(comparedTargets).filter(headings.add)
          val heading = comparisons.map { (start, actor, target) =>
            val both = histories.count(h => h.start == start && h.originalAttacker == actor && h.originalTarget == target) == 2
            val label = if both then "Compared continuations for the same" else "The other continuation for the compared"
            s"$label ${target.sideName.capitalize} ${target.roleName} originally on ${target.at}: " +
              s"the original attacker is the ${actor.sideName} ${actor.roleName} from ${actor.at}. "
          }.mkString
          (owner(group), heading + historyText(group,
            comparedTargets((group.head.start, group.head.originalAttacker, group.head.originalTarget)), checkedOccurrences,
            if shareEvents || shareOpening then sharedCaptures else mutable.Set.empty[ObservationEvent],
            if shareEvents then sharedObservations else mutable.Set.empty[ObservationEvent], additions, introductions), group)
        }.groupBy(_._1)
        val rendered = parts.zipWithIndex.map { (part, offset) =>
          val rewritten = if embeddedContexts(offset) then None else contextText(part)
          val repeatedRoutes = part.effects.filter(effect => effect.scope == "piece_route" && effect.use.exists(use =>
            groups.exists(group => group.exists(part.histories.contains) && group.exists(h =>
              h.start == index && effect.lines.exists(_._1 == h.path.line.ref.id) && h.steps.exists(step =>
                step.offset == use.offset && step.withdrawal.exists(_._2 == use.actor)))))).toSet
          def wording(fragment: Fragment) = if fragment.effects.exists(repeatedRoutes) ||
              (embeddedContexts(offset) && fragment.context == part.context && fragment.context.nonEmpty) then ""
            else if fragment.context == part.context && fragment.context.nonEmpty then rewritten.getOrElse(fragment.text)
            else fragment.text
          val actions = part.actions.collect {
            case (line, note, identity) if !described(line -> note.id) && !embeddedActions.contains(line -> note.id) =>
              s"At ${label(note.edge)}, ${note.note.detail.text}$identity"
          }
          val ownHistories = historyParts.getOrElse(offset, Nil)
          val owned = ownHistories.flatMap(_._3)
          // The receiver verifies each candidate independently. A shared sentence
          // must not remove a later attack's functional proof from its new owner.
          val dependencies = parts.flatMap(_.effects).filter(effect => owned.exists(usesHistory(effect, _)) ||
            effect.actions.exists(note => effect.lines.exists((line, _) => embeddedActions.get(line -> note.id).contains(offset))))
          Rendered((part.before.map(wording) ++ ownHistories.map(_._2) ++ actions ++ part.after.map(wording))
            .filter(_.nonEmpty).mkString("\n\n"), part.current,
            (part.effects.filterNot(effect => part.histories.exists(_.effect == effect)) ++
              owned.map(_.effect) ++ dependencies).distinct, part.functions, part.owner, part.qualifications, List(part))
        }
        // Histories sharing an event retain all their attached qualifications in
        // the same paragraph. Independent explanations remain independently admitted.
        val linked = if requests.size == 1 || !share then parts.indices.map(Set(_)).toList else
          parts.indices.toList.foldLeft(List.empty[Set[Int]]) { (all, next) =>
            val links = groups.filter(group => parts(next).histories.exists(group.contains))
              .flatMap(group => parts.indices.filter(i => parts(i).histories.exists(group.contains))).toSet + next
            val (related, separate) = all.partition(_.intersect(links).nonEmpty)
            separate :+ (related.flatten.toSet ++ links)
          }.sortBy(_.min)
        val blocks = linked.map { indices =>
          val values = indices.toList.sorted.map(rendered)
          Rendered(values.map(_.text).filter(_.nonEmpty).mkString("\n\n"), values.exists(_.current),
            values.flatMap(_.effects).distinct, values.flatMap(_.functions).distinct,
            values.flatMap(_.owner).headOption, values.flatMap(_.qualifications).distinct, values.flatMap(_.meaning))
        }.filter(block => block.text.nonEmpty || block.qualifications.nonEmpty || block.effects.nonEmpty || block.functions.nonEmpty)
        val allocated = Array.fill(requests.size)(List.empty[Rendered])
        var cursor = 0
        val fits = blocks.forall { block =>
          val choices = block.owner.toList match
            case Nil => (cursor until requests.size).toList
            case values => values
          choices.find(owner => textFor(owner, allocated(owner) :+ block).length <= limit) match
            case Some(owner) =>
              allocated(owner) = allocated(owner) :+ block
              if block.owner.isEmpty then cursor = owner
              true
            case None => false
        }
        if fits then Some(allocated.toList)
        // Sharing prose is optional. A large connected history must not make
        // separately fitting, complete explanations disappear at a field limit.
        else if share && requests.size > 1 then bodies(parts, checkedOccurrences, share = false)
        else None
      def admit(bundle: Explanation, receivingField: Option[Int] = None): Unit =
        val existing = paragraphs.indexWhere(p => bundle.owner == p.owner &&
          bundle.textParts.size == 1 && p.context.contains(bundle.textParts.head))
        val replacing = existing >= 0 && bundle.histories.nonEmpty && paragraphs(existing).histories.isEmpty
        if existing >= 0 && !replacing then return
        val retained = if replacing then paragraphs.toList.patch(existing, Nil, 1) else paragraphs.toList
        val retainedEffects = if replacing then retained.flatMap(_.effects) else selected.toList
        val retainedFunctions = if replacing then retained.flatMap(_.functions) else selectedFunctions.toList
        // Absorption is part of the same admission trial as its complete wording.
        // Rejected bundles consume nothing; the original short support remains available.
        val absorbed = (retainedEffects ++ bundle.effects).flatMap(_.absorbedNotes).toSet
        val fresh = bundle.effects.filterNot(e => retainedEffects.contains(e) ||
          (e.immediate && e.actions.nonEmpty && e.actions.forall(n => absorbed(n.id)))).distinct
        val functions = bundle.functions.filterNot(value => retainedFunctions.exists(_.note.id == value.note.id) ||
          value.notes.forall(n => absorbed(n.id)))
        if fresh.nonEmpty || functions.nonEmpty || bundle.textParts.nonEmpty then
          val histories = bundle.histories.filter(h => fresh.contains(h.effect))
          val premises = fresh.filterNot(effect => histories.exists(_.effect == effect)).sortBy(effect =>
            if focalLine.exists(line => effect.lines.exists(_._1 == line)) then 0 else 1)
          val checkedOccurrences = (retainedEffects ++ premises).filter(_.scope == "check_sequence")
            .flatMap(_.occurrences).toSet
          val actions = premises.filterNot(_.immediate).flatMap(effect =>
            effect.actions.map(note => effect.lines.head._1 -> note)).distinctBy((line, note) => line -> note.id)
          val actionsText = actions.map { (line, note) =>
            val identity = note.note.detail match
              case Detail.Support(_, target, _, _) => premises.find(effect => effect.scope == "piece_route" &&
                effect.actions.exists(_.id == note.id)).flatMap(_.target).filter(_ != target).fold("")(anchor =>
                  s" The supported ${target.roleName} is the same one from ${anchor.at}.")
              case _ => ""
            (line, note, identity)
          }
          val (supports, supportParts) = supportTexts(premises)
          val supportLosses = premises.flatMap(effect => effect.lostSupport.map(target =>
            (effect.lines.head._1, effect.actor, target))).distinct.filterNot { (line, actor, target) =>
            (retainedEffects ++ premises).exists(sameSupportLoss(_, line, actor, target))
          }.map { (line, actor, target) =>
            val move = sans(ownedPair.find(_.line.ref.id == line).get)(index)
            s"$move gives up that ${actor.roleName}'s support of ${target.name}."
          }
          val seenClearance = mutable.Set.from(retainedEffects.filter(_.scope == "clearance_use").map(e => (e.actor, e.square)))
          val (qualifications, separateRelations) = premises.filterNot(supports).partition { effect =>
            effect.scope != "pair_difference" || premises.exists(function => function.use.nonEmpty &&
              (function.actor == effect.actor && (function.target == effect.target || function.lostSupport == effect.target) ||
                function.lostSupport.exists(sameSupportLoss(effect, function.lines.head._1, function.actor, _)))) ||
              histories.exists(history => history.effect.actor == effect.actor && history.effect.target == effect.target)
          }
          val describedSupport = retainedEffects.flatMap(effect => effect.absorbedNotes ++
            (if effect.immediate then effect.actions.map(_.id) else Nil)).toSet
          // The same mover's observed recapture already gives this destination.
          // Fold only the route introduction; retain its actions, costs and proof.
          val coveredRoutes = qualifications.filter(route => route.scope == "piece_route" &&
            route.use.nonEmpty && qualifications.exists(support =>
              Set("support_use", "support_handoff")(support.scope) &&
                support.lines == route.lines && support.actor == route.actor &&
                support.use == route.use && support.square == route.square)).toSet
          val premiseText = qualifications.filterNot(coveredRoutes)
            .sortBy(effect => if supportDifferences.exists(_._1 == effect) then 0 else 1).map { effect =>
            val text = if requests.size == 1 && effect.absorbedNotes.nonEmpty &&
                effect.absorbedNotes.subsetOf(describedSupport) && effect.withoutSupportIntroduction.nonEmpty then
              effect.withoutSupportIntroduction.get
            else if effect.scope != "clearance_use" || seenClearance.add((effect.actor, effect.square)) then effect.text
            else
              effect.meaning.toList.flatMap(_.statements).collectFirst {
                case value: CommentaryAnswer.ClearedRoute => CommentaryEnglish.clearedRouteUse(value).map {
                  case text: String => text
                  case ref: RootQuestionSummary.MoveRef => sans(ref.path)(ref.offset)
                }.mkString
              }.getOrElse(effect.text)
            Fragment(text, Set(effect))
          }.distinctBy(_.effects)
          val later = functions.filter(_.offset != index)
          val laterText = later.map(_.path.line.ref.id).distinct.map { line =>
            val actions = later.filter(_.path.line.ref.id == line)
            val introduction = s"Later in the provided ${sans(actions.head.path)(index)} line"
            s"$introduction: " + actions.map(_.atMove).mkString(" ")
          }
          val (introductions, trailingContext) = bundle.textParts.partition(context => histories.nonEmpty &&
            bundle.owner.exists(owner => ownedTactics(owner -> context)))
          val paragraph = Paragraph(supportLosses.map(text => Fragment(text)) ++ premiseText ++
            functions.filter(_.offset == index).map(value => Fragment(value.text)) ++
            introductions.map(value => Fragment(value.text, context = Some(value))), histories, actionsText,
            separateRelations.map(effect => Fragment(effect.text, Set(effect))).distinctBy(_.effects) ++
              supportParts ++ laterText.map(text => Fragment(text)) ++
              trailingContext.map(value => Fragment(value.text, context = Some(value))),
            currentAction(bundle), fresh, functions, bundle.owner,
            bundle.textParts.flatMap(context => context.qualification.toList ++ context.additional),
            bundle.textParts.headOption)
          val trial = if replacing then paragraphs.toList.updated(existing, paragraph) else paragraphs.toList :+ paragraph
          val bounded = receivingField.fold(trial)(owner => trial.updated(
            if replacing then existing else trial.size - 1, paragraph.copy(owner = Some(owner))))
          if bodies(bounded, checkedOccurrences).nonEmpty &&
            (receivingField.isEmpty || bodies(trial, checkedOccurrences).nonEmpty) then
            if replacing then
              paragraphs(existing) = paragraph
              selected.clear()
              selected ++= retainedEffects ++ fresh
              selectedFunctions.clear()
              selectedFunctions ++= retainedFunctions ++ functions
            else
              paragraphs += paragraph
              selected ++= fresh
              selectedFunctions ++= functions
      val (coreBundles, supplements) = orderedBundles.span(core)
      val (tactical, otherCore) = coreBundles.partition(answersTactic)
      val (introductions, completeTactics) = tactical.partition(_.histories.isEmpty)
      val (directAnswers, physicalCore) = otherCore.partition(answersQuestion)
      // The existing core policy chooses chess content before prose admission.
      // A display limit or a different first paragraph cannot change this answer.
      val physicalAnswerBundles = otherCore.filter(_.textParts.isEmpty)
      // Try the root's complete current explanations in their existing order,
      // without spending a later question's room. This is an admission bound,
      // not permanent ownership: later shared events may still be regrouped.
      if requests.size > 1 then physicalCore.foreach(admit(_, Some(0)))
      directAnswers.foreach(admit(_))
      // Give complete claims with their replies/costs the first admission trial.
      // A bounded introduction remains the fallback when that whole unit cannot fit.
      def addsOutcome(histories: List[AttackHistory], described: Set[(String, String)]): Boolean =
        histories.exists(h => h.steps.exists(s => s.offset > h.start && s.observation.nonEmpty &&
          !described(h.path.line.ref.id -> s.eventKey) &&
          !openingCaptures.getOrElse(h.path.line.ref.id, Set.empty)(s.offset)))
      val answeredEvents = paragraphs.flatMap(_.histories).flatMap(h => h.steps.map(s => h.path.line.ref.id -> s.eventKey)).toSet
      completeTactics.sortBy(bundle => if addsOutcome(bundle.histories, answeredEvents) then 0 else 1).foreach(admit(_))
      introductions.foreach(admit(_))
      // Still-valid physical explanations can use the remaining space after the
      // owned answer and its observed replies have had their complete trial.
      physicalCore.foreach(admit(_))
      // Context leads the reading, but cannot evict a complete action/reply bundle.
      // Reserve it before admitting independent supplementary observations.
      if convergence.nonEmpty then
        admittedConvergence = convergence
        if bodies(paragraphs.toList, selected.filter(_.scope == "check_sequence").flatMap(_.occurrences).toSet).isEmpty then
          admittedConvergence = Nil
      val (contextBundles, independentSupplements) = supplements.partition(_.textParts.nonEmpty)
      contextBundles.foreach(admit(_))
      // Complete selected tactical explanations before quiet supplements. Their
      // original introductions, qualifications and all admitted core answers stay
      // reserved; a failed expansion still keeps the original bounded paragraph.
      val describedEvents = paragraphs.flatMap(_.histories).flatMap(h => h.steps.map(s => h.path.line.ref.id -> s.eventKey)).toSet
      // A cost/reply already carried by the card can wait; fill an unreported
      // physical outcome first. This orders optional expansions, not engine moves
      // or the original selected answers, all of which remain reserved above.
      // Owned answers already had their complete admission trial with the
      // required premises and costs. Do not bypass it with a history-only retry.
      val contextContinuations = contextHistories.filterNot((context, owner, _) =>
        ownedTactics(owner -> context)).sortBy { (_, _, histories) =>
        if addsOutcome(histories, describedEvents) then 0 else 1
      }
      contextContinuations.foreach { (context, owner, histories) =>
        val at = paragraphs.indexWhere(p => p.owner.contains(owner) && p.context.contains(context))
        val fresh = histories.filterNot(h => selected.contains(h.effect))
        if at >= 0 && fresh.nonEmpty then
          val prior = paragraphs(at)
          val expanded = prior.copy(before = prior.before :+ Fragment(context.text, context = Some(context)),
            after = prior.after.filterNot(_.context.contains(context)), histories = fresh,
            effects = prior.effects ++ fresh.map(_.effect))
          val trial = paragraphs.toList.updated(at, expanded)
          val checks = selected.filter(_.scope == "check_sequence").flatMap(_.occurrences).toSet
          if bodies(trial, checks).nonEmpty then
            paragraphs(at) = expanded
              selected ++= fresh.map(_.effect)
      }
      // Bound support is a fallback, not a reason to evict the complete history
      // while that history still has an admission/continuation attempt pending.
      immediateFallbacks.foreach(admit(_))
      val explainedMoves = (current.filter(effect => alreadyOwned(effect) && effect.use.nonEmpty).flatMap(_.lines.map(_._1)) ++
        followed.filter(history => alreadyExplained(history) && history.start == index).map(_.path.line.ref.id) ++
        paragraphs.flatMap(part =>
        part.effects.filter(_.use.nonEmpty).flatMap(_.lines.map(_._1)) ++
          part.histories.filter(_.start == index).map(_.path.line.ref.id))).toSet
      // ponytail: trim only independent quiet detail after an observed use/current
      // attack. Ranking between complete explanations remains a separate problem.
      independentSupplements.filter { bundle =>
        val lines = bundle.effects.flatMap(_.lines.map(_._1)) ++ bundle.functions.map(_.path.line.ref.id)
        bundle.effects.exists(effect => !selected.contains(effect) &&
          (effect.scope == "pair_difference" || effect.lines.size > 1 || effect.lines.exists(_._2 > index + 1))) ||
          lines.exists(line => !explainedMoves(line))
      }.foreach(admit(_))
      // Preserve the existing verdict and the entire cost/benefit bundle at the wire limit.
      // PV evidence IDs are the exact two line summaries; local proofs remain at
      // its existing occurrence/position links and must not replace that contract.
      val allocated = bodies(paragraphs.toList, selected.filter(_.scope == "check_sequence").flatMap(_.occurrences).toSet).get
      // The certified comparison answers the summary independently of detail
      // admission. Keep the full record's complete reply/cost bundles intact.
      val terminalMeaning = Option.when(index == 0 && (detail \ "type").as[String] == "root_comparison" &&
        ownedPair.exists(_.automaticTerminal.nonEmpty))(()).flatMap { _ =>
          RootQuestionSummary.completePairAnswer(packet, paths, ownedPair, "observed_terminal_comparison", Nil)
            .map(answer =>
              import CommentaryAnswer.*
              import RootQuestionSummary.MoveRef
              // A final mate and material total can hide the first useful reply.
              // Retain that checking entry, its actual response, and an immediate
              // follow-up capture before the finite outcome. The rest of the
              // ledger remains intact; this does not establish forced play.
              val entries = ownedPair.flatMap { path =>
                val material = path.certified.flatMap(_._2.material).get
                val start = path.positions.headOption.flatMap(positions.get)
                val actor = start.flatMap(_.occupant(path.moves.head.take(2)))
                if path.moves.size <= 2 || !actor.exists(piece => path.automaticTerminal.contains(
                    lila.chessjudgment.model.line.AutomaticTerminal.Checkmate(!piece.side))) ||
                    material.events.exists(_.plyOffset == 0) then Nil
                else
                  val first = checkSteps(path, List(1), None, observations)
                  if !first.exists(_._2.isInstanceOf[CheckingMove]) then Nil else
                    val capture = material.events.filter(_.plyOffset == 3)
                    val later = Option.when(capture.nonEmpty)(3).toList
                    val steps = checkSteps(path, 1 :: later, None, observations)
                    val destination = path.moves(1).slice(2, 4)
                    val lost = for
                      before <- start.toList if before.complete(RelationFactKind.GeometricControl)
                      after <- path.positions.lift(1).flatMap(positions.get).toList
                        if after.complete(RelationFactKind.GeometricControl)
                      original <- actor.toList
                      current <- paths.trackedPieceAfter(path, original, 1).flatten.toList
                      if before.controlledBy(original)(destination) && !after.controlledBy(current)(destination)
                    yield ControlState(MoveRef(path, 0), current, None, destination, present = false) ->
                      before.sourceRefs(before.sources(original, List(destination)))
                    val content = lost.map(_._1) ++ orderedCaptureSteps(List(path),
                      List(CheckSequence(MoveRef(path, 0), steps.map(_._2))),
                      Option.when(capture.nonEmpty)(MaterialEvents(path, capture)).toList)
                    List((path, content, steps.flatMap(_._3) ++ lost.flatMap(_._2)))
              }
              answer.copy(core = answer.core.map { section =>
                entries.find((path, _, _) => section.references.exists(move => move.offset == 0 && (move.path eq path)))
                  .fold(section)((_, content, _) => section.copy(statements = content ++ section.statements))
              }.sortBy(section =>
                if section.references.exists(move => move.offset == 0 && packet.assembly.playedTransition
                  .exists(edge => EvidenceRef.sameMove(move.path.moves.head, edge.moveUci))) then 0 else 1),
                evidence = (answer.evidence ++ entries.flatMap(_._3)).distinct))
        }
      val eligibleQuestion = (detail \ "type").as[String] == "root_comparison" && index == 0
      // Shared original subjects are chosen before the histories below lose
      // their bundle boundaries. Verified root condition/use receipts can
      // coexist with that comparison instead of being silently displaced.
      val subjectMeaning = Option.when(eligibleQuestion && terminalMeaning.isEmpty &&
        convergenceMeaning.isEmpty && exchangeMeaning.isEmpty)(()).flatMap(_ =>
        RootSubjectComparison.answer(packet, paths, ownedPair, positions))
      val legacyChoice = Option.when(eligibleQuestion && terminalMeaning.isEmpty && convergenceMeaning.isEmpty &&
        exchangeMeaning.isEmpty)(()).flatMap(_ => RootQuestionSummary.choose(packet, paths, ownedPair, positions))
      val threatMeaning = Option.when(eligibleQuestion && legacyChoice.isEmpty)(()).flatMap(_ =>
        RootThreatSummary.readAnswer(packet, paths, ownedPair, positions))
      val contacts = physicalAnswerBundles.filter(part => part.owner.forall(_ == 0) &&
        part.functions.isEmpty && part.effects.toSet == part.histories.map(_.effect).toSet)
        .flatMap(_.histories).flatMap { history =>
          val occurrences = (history.start -> history.openingObservation) ::
            history.steps.flatMap(step => step.observation.map(step.offset -> _))
          occurrences.flatMap { (offset, id) =>
            byOccurrence.getOrElse(history.path.ids(offset), Nil).filter(_.id == id).map(history.path -> _)
          }
        }.distinct
      val contactMeaning = Option.when(eligibleQuestion && legacyChoice.isEmpty && threatMeaning.isEmpty)(()).flatMap(_ =>
        RootQuestionSummary.selectedAnswer(packet, paths, ownedPair, positions, contacts))
      val physicalAnswers = Option.when(eligibleQuestion && (questionFocus.size == 1 ||
        legacyChoice.isEmpty && threatMeaning.isEmpty && contactMeaning.isEmpty))(()).flatMap { _ =>
          import CommentaryAnswer.*
          import RootQuestionSummary.MoveRef
          val arms = RootQuestionSummary.prepare(packet, paths, ownedPair, positions).getOrElse(Nil)
          def unsupportedTargetLoss(statement: Statement): List[Statement] =
            val condition = statement match
              case ControlState(move, actor, Some(target), _, false, _) if actor.side == target.side =>
                Some((move, actor, target))
              case contrast: ControlContrast if !contrast.present && contrast.target.exists(_.side == contrast.actor.side) =>
                Some((contrast.move, contrast.actor, contrast.target.get))
              case _ => None
            condition.toList.flatMap { (move, actor, target) =>
              val accounts = for
                arm <- arms.filter(_.path eq move.path)
                originalActor <- arm.originals.find(piece => arm.at(piece, move.offset + 1).contains(actor)).toList
                originalTarget <- arm.originals.find(piece => arm.at(piece, move.offset + 1).contains(target)).toList
                loss <- arm.captures.find(event => event.plyOffset > move.offset &&
                  event.originalVictim.contains(originalTarget)).toList
                if (move.offset + 1 to loss.plyOffset).forall(count =>
                  arm.control(count, originalActor, originalTarget).contains(false))
                account = arm.account(loss, originalTarget)
                if account.complete && account.dutyCosts.isEmpty
              yield (arm, account)
              accounts.flatMap { (arm, account) =>
                val trade = arm.pieceExchange(account.events)
                val context = account.offsets.map(arm.ref).filterNot(trade.references.contains)
                trade :: Option.when(context.nonEmpty)(Continuation(context)).toList
              }
            }
          // Keep the physical bundle boundary until its own use and price can
          // close. A different user's unknown account cannot retain this bundle.
          val sections = physicalAnswerBundles.map { part =>
            val meanings = part.effects.flatMap(_.meaning).distinct
            val selections = part.functions.flatMap { function =>
              if function.comparedAttack then function.notes.flatMap(note =>
                ownedPair.find(_.ids.lift(function.offset).contains(note.edge.evidence.id)).map(path =>
                  (MoveRef(path, function.offset), note.note.detail, note)))
              else function.details.map(detail => (MoveRef(function.path, function.offset), detail, function.note))
            } ++ part.effects.flatMap(effect => effect.actions.map(effect.lines.head._1 -> _)).flatMap { (line, note) =>
              ownedPair.find(_.line.ref.id == line).map(path =>
                (MoveRef(path, path.ids.indexOf(note.edge.evidence.id)), note.note.detail, note))
            } ++ part.histories.flatMap { history =>
              val observations = (history.start -> history.openingObservation) ::
                history.steps.flatMap(step => step.observation.map(step.offset -> _))
              observations.flatMap { (offset, id) =>
                byOccurrence.getOrElse(history.path.ids(offset), Nil).find(_.id == id).map(note =>
                  (MoveRef(history.path, offset), note.note.detail, note))
              }
            }
            val actions = selections.filterNot(_._2.isInstanceOf[Detail.Capture])
              .map((move, detail, _) => Action(move, detail)).distinct
            val lostSupport = part.effects.flatMap { effect => effect.lostSupport.filterNot(target =>
              part.effects.exists(sameSupportLoss(_, effect.lines.head._1, effect.actor, target))).flatMap { target =>
              ownedPair.find(_.line.ref.id == effect.lines.head._1).map(path =>
                ControlState(MoveRef(path, index), effect.actor, Some(target), target.at, false))
            }}
            val historyStates = part.histories.flatMap { history =>
              val withdrawals = history.steps.flatMap(step => step.withdrawal.map { (before, after, target) =>
                AttackWithdrawal(MoveRef(history.path, step.offset), before, after, target)
              })
              val relocations = history.changes.collect { case (offset, "relocates", _) => offset }.flatMap { offset =>
                for
                  actor <- paths.pieceAfter(packet, history.path, history.originalAttacker, offset + 1).flatten
                  target <- paths.pieceAfter(packet, history.path, history.originalTarget, offset + 1).flatten
                yield ControlState(MoveRef(history.path, offset), actor, Some(target), target.at, true)
              }
              val checking = checkSteps(history.path, history.steps.filter(_.checkAction).map(_.offset), None, observations).map(_._2)
              val checks = Option.when(checking.nonEmpty)(CheckSequence(MoveRef(history.path, history.start), checking)).toList
              val end = MoveRef(history.path, history.path.moves.size - 1)
              val retained = history.endTarget.toList.map(PiecePresence(end, history.originalTarget, _))
              val pressure = history.endTarget.toList.flatMap(target => history.endAttack.toList.map(actor =>
                ControlState(end, actor, Some(target), target.at, true)))
              history.steps.flatMap(_.meaning) ++ withdrawals ++ relocations ++ checks ++ retained ++ pressure
            }
            // Preserve the admitted bundle, including later restoration/actions.
            // A shared typed claim is realized once, independent of its wording.
            val conditions = (meanings.flatMap(_.statements) ++ actions ++ lostSupport ++ historyStates).distinct
            // A selected missing-support claim retains the same physical
            // target's observed loss and account, unless support is restored.
            val statements = (conditions ++ conditions.flatMap(unsupportedTargetLoss)).distinct
            val required = meanings.flatMap(_.required).distinct
            val references = statements.flatMap(_.references).toSet
            val focus = (meanings.flatMap(_.focus) ++ selections.collect {
              case (move, _, note) if references(move) && note.selection == CommentarySelection.Display.Selected =>
                Focus(move, note)
            }).distinctBy(_.move)
            val described = statements.filterNot(_.isInstanceOf[Material]).flatMap(_.covered).toSet
            val captures = ownedPair.flatMap { path =>
              val offsets = part.histories.filter(_.path eq path).flatMap(history =>
                history.start :: history.steps.filter(_.captureRole.nonEmpty).map(_.offset)).toSet
              val events = path.certified.flatMap(_._2.material).toList.flatMap(_.events.filter(event =>
                offsets(event.plyOffset) && !described(MoveRef(path, event.plyOffset))))
              Option.when(events.nonEmpty)(MaterialEvents(path, events))
            }
            val checking = statements.collect { case sequence: CheckSequence => sequence }
            Section(statements.filterNot(_.isInstanceOf[CheckSequence]) ++
              orderedCaptureSteps(ownedPair, checking, captures), required, focus)
          }.filter(_.statements.nonEmpty)
          // Selected histories already classify the original attacker's loss,
          // target capture and recovery. Each bundle keeps these prices before
          // global deduplication, including an interrupted quiet user's loss.
          val selectedReferences = sections.flatMap(_.references).toSet
          val clearances = recoveryClearances(arms, selectedReferences, local)
          val selectedSections = physicalEpisodes(ownedPair, sections ++ clearances)
          val protectedSubject = subjectMeaning.map(answer => protectRootUses(packet, paths, ownedPair,
            positions, local, answer, selectedSections))
          val lead = selectedSections
          // A verified question owns its root condition and exact physical use.
          // Other facts at that use, or elsewhere on its arm, are not its answer.
          val investigated = questionFocus match
            case focus :: Nil if arms.exists(_.path.line.ref.id == focus.owner.answer.referenceLineId) =>
              val condition = focus.mechanism.after.predicate
              val reference = arms.find(_.path.line.ref.id == focus.owner.answer.referenceLineId).get
              val use = MoveRef(reference.path, focus.key.branch.size - 1)
              val before = condition.subject.flatMap(p => reference.at(p.witness, index))
              val after = condition.subject.flatMap(p => reference.at(p.witness, index + 1))
              val action = focus.key.action.subject.map(_.witness).filter(reference.mover(use.offset).contains)
              val capture = reference.captures.find(_.plyOffset == use.offset)
              val exactAction = if focus.key.action.kind == "legal_capture" then
                capture.exists(event => event.originalActor == action &&
                  event.originalVictim == focus.key.action.target.map(_.witness))
              else focus.key.action.kind == "legal_move" && capture.isEmpty
              def ownsCondition(effect: ComparisonEffect): Boolean =
                val usesAction = effect.use.exists(value => value.offset == use.offset &&
                  action.flatMap(reference.at(_, use.offset + 1)).contains(value.actor)) ||
                  effect.meaning.toList.flatMap(_.statements).exists {
                    case value: SupportRetention => value.departure == use && action.contains(value.originalOldProvider)
                    case _ => false
                  }
                usesAction && effect.meaning.exists(_.references.contains(use)) && answers(focus, Explanation(List(effect))) && (
                  (condition.kind == "control" && after.contains(effect.actor) && condition.square.contains(effect.square)) ||
                  (condition.kind == "location" && after.contains(effect.actor) && before.exists(_.at == effect.square)) ||
                  effect.meaning.toList.flatMap(_.statements).exists {
                    case route: ClearedRoute => route.move == MoveRef(reference.path, index) && route.use == use &&
                      ((condition.kind == "location" && before.contains(route.before)) ||
                        (condition.kind == "control" && condition.subject.exists(_.witness == route.originalUser) &&
                          condition.square.contains(reference.path.moves(use.offset).slice(2, 4))))
                    case _ => false
                  })
              val selectedEffects = physicalAnswerBundles.flatMap(_.effects).distinct.filter(ownsCondition)
              val uses = selectedEffects.flatMap(_.meaning).distinct
              val retained = uses.flatMap(_.statements).collect { case value: SupportRetention => value }
              // The old provider's duty is the exact comparison behind retention,
              // not every other relation change on either arm.
              val contrastEffects = physicalBundles.flatMap(_.effects).distinct.filter(effect =>
                effect.meaning.toList.flatMap(_.statements).exists {
                  case value: ControlContrast => retained.exists { retention =>
                    def samePair(move: MoveRef, actor: Piece, target: Option[Piece]): Boolean =
                      move.offset == index && arms.exists(arm => (arm.path eq move.path) &&
                        arm.at(retention.originalOldProvider, index + 1).contains(actor) &&
                        target.exists(t => arm.at(retention.originalTarget, index + 1).contains(t)))
                    samePair(value.move, value.actor, value.target) &&
                      samePair(value.alternative, value.alternativeActor, value.alternativeTarget)
                  }
                  case _ => false
                })
              val meanings = (uses ++ contrastEffects.flatMap(_.meaning)).distinct
              if !exactAction || action.isEmpty || meanings.isEmpty then None
              else
                val accounts = arms.map { arm =>
                  val subjects = Option.when(arm eq reference)(action.toList ++ selectedEffects.flatMap(_.target)
                    .flatMap(target => arm.originals.find(p => arm.at(p, index + 1).contains(target)))).toList.flatten.toSet
                  val seeds = arm.captures.filter(event => event.plyOffset == 0 || event.originalVictim == arm.root) ++
                    Option.when(arm eq reference)(capture.toList ++ arm.captures.filter(event =>
                      event.plyOffset > use.offset && event.originalVictim.exists(subjects))).toList.flatten
                  arm -> seeds.distinct.map(event => arm.account(event,
                    if event.plyOffset == 0 || event.originalVictim == arm.root then arm.root.get
                    else event.originalVictim.filter(subjects).getOrElse(action.get)))
                }
                if accounts.exists(_._2.exists(value => !value.complete || value.dutyCosts.nonEmpty)) then None
                else
                  val described = meanings.flatMap(_.statements).filterNot(_.isInstanceOf[Material]).flatMap(_.covered).toSet
                  val references = meanings.flatMap(_.references).toSet
                  val costs = accounts.flatMap { (arm, values) =>
                    val offsets = values.flatMap(_.offsets).distinct.sorted
                    val promotions = arm.material.events.filter(event => event.promotionGainCp > 0 && offsets.contains(event.plyOffset))
                    val events = values.flatMap(_.events).distinct.filterNot(event => described(arm.ref(event.plyOffset)) ||
                      promotions.exists(_.plyOffset == event.plyOffset))
                      .sortBy(_.plyOffset)
                    val required = offsets.map(arm.ref).filterNot(references)
                    val exchange = Option.when(events.nonEmpty)(arm.pieceExchange(events)).toList
                    val content: List[Statement] = exchange ++
                      Option.when(promotions.nonEmpty)(MaterialEvents(arm.path, promotions)).toList
                    val missing = required.filterNot(content.flatMap(_.references).contains)
                    Option.when(content.nonEmpty || missing.nonEmpty)(Section(content ++
                      Option.when(missing.nonEmpty)(Continuation(missing)).toList, required)).toList
                  }
                  val selected = meanings ++ costs
                  val included = selected.flatMap(_.statements).toSet
                  val supplementary = lead.map(section =>
                    val remaining = section.statements.filterNot(included)
                    val refs = remaining.flatMap(_.references).toSet
                    Section(remaining, section.required.filter(refs), section.focus.filter(f => refs(f.move))))
                    .filter(_.statements.nonEmpty)
                  RootQuestionSummary.completePairAnswer(packet, paths, ownedPair,
                    "investigated_mechanism", selected, supplementary).map(answer => answer.copy(
                      evidence = (answer.evidence ++ contrastEffects.flatMap(_.evidence)).distinct))
            case _ => None
          val fallback = protectedSubject.orElse(RootQuestionSummary.pairedSupportAnswer(packet, paths, ownedPair, positions, lead))
            .orElse(RootQuestionSummary.vacancyResponseAnswer(packet, paths, ownedPair, positions, lead))
            .orElse(RootQuestionSummary.captureTargetAnswer(packet, paths, ownedPair, positions, lead))
            .orElse(RootQuestionSummary.completePairAnswer(packet, paths, ownedPair, "selected_comparison", lead))
          fallback.map(answer =>
            val evidence = (answer.evidence ++ arms.flatMap(_.proofs) ++ physicalAnswerBundles.flatMap(bundle =>
              bundle.effects.flatMap(_.evidence) ++ bundle.functions.flatMap(_.notes.flatMap(_.evidence)) ++
                bundle.histories.flatMap(_.effect.evidence))).distinct
            investigated.map(value => value.copy(evidence = (value.evidence ++ evidence).distinct)) ->
              answer.copy(evidence = evidence))
        }
      val investigatedMeaning = physicalAnswers.flatMap(_._1)
      val physicalMeaning = physicalAnswers.map(_._2)
      // Selection survives realization failure. A different question cannot be
      // substituted because its sentence happens to fit or translate.
      val answerChoice: Option[Either[RootQuestionSummary.SelectedQuestion, CommentaryAnswer.Answer]] =
        terminalMeaning.orElse(convergenceMeaning).orElse(exchangeMeaning).map(Right(_))
          .orElse(investigatedMeaning.map(Right(_))).orElse(legacyChoice.map(Left(_)))
          .orElse(threatMeaning.orElse(contactMeaning).orElse(physicalMeaning).map(Right(_)))
      val firstTargets = requests.flatMap(_._2.flatMap(_.firstTarget)).distinctBy(_.id)
      val commonReading = answerChoice.flatMap {
        case Left(chosen) => Some(chosen.confirmedFacts)
        case Right(answer) => Some(answer)
      }.map(answer => withRootActions(ownedPair, local, answer))
        .map(answer => RootSubjectComparison.withImmediateDefense(packet, paths, ownedPair, positions, answer))
        .map(answer => RootSubjectComparison.withReplySpace(packet, paths, ownedPair,
          requests.flatMap(_._2.flatMap(_.replySpace)).distinct, answer))
        .map(answer => RootSubjectComparison.withReserveBreak(packet, paths, ownedPair,
          requests.flatMap(_._2.flatMap(_.reserveResponse)).distinct, answer))
        .map(answer => RootSubjectComparison.withReserveEntry(packet, paths, ownedPair,
          requests.flatMap(_._2.flatMap(_.reserveEntry)).distinct, answer))
        .map(answer => RootSubjectComparison.withReserveHold(packet, paths, ownedPair,
          requests.flatMap(_._2.flatMap(_.reserveHold)).distinct, answer))
        .map(answer => RootSubjectComparison.withReserveTempo(packet, paths, ownedPair,
          requests.flatMap(_._2.flatMap(_.reserveTiming)).distinct, answer))
        .map(answer => RootSubjectComparison.withDirectReserveEntry(packet, paths, ownedPair,
          requests.flatMap(_._2.flatMap(_.directReserveEntry)).distinct, answer))
        .map(answer => FunctionalReplyReading.withAnswer(packet, paths, ownedPair,
          requests.flatMap(_._2.flatMap(_.replyFunction)).distinct, answer,
          (value, sections) => protectRootUses(packet, paths, ownedPair, positions, local, value, sections)))
        .map(answer => TargetResponseInvestigation.withOwnTargetSupport(packet, paths, ownedPair, answer))
        .map(answer => FunctionalChoiceReading.withAnswer(packet, paths, ownedPair,
          requests.flatMap(_._2.flatMap(_.functionalChoice)).distinct, answer,
          (value, sections) => protectRootUses(packet, paths, ownedPair, positions, local, value, sections)))
        .map(answer => FunctionalRealizationReading.withDetail(packet, paths, ownedPair,
          requests.flatMap(_._2.flatMap(_.functionalChoice)).distinct, answer))
        .map(answer => AnchoredChoiceAnswer.withAnswer(packet, paths, ownedPair,
          requests.flatMap(_._2.flatMap(_.anchoredChoice)).distinct, answer,
          (value, sections) => protectRootUses(packet, paths, ownedPair, positions, local, value, sections)))
        .map(answer => AnchoredChoiceAnswer.withFamily(packet, paths, ownedPair,
          requests.flatMap(_._2.flatMap(_.anchoredFamily)).distinct, answer,
          (value, sections) => protectRootUses(packet, paths, ownedPair, positions, local, value, sections)))
        .map(answer => ResponsePurposeReading.withAnswer(packet, paths, ownedPair, firstTargets, local, answer,
          value => completeSelectedConditions(packet, paths, ownedPair, positions, local, value), rootUses))
        .flatMap(answer => completeSelectedConditions(packet, paths, ownedPair, positions, local, answer))
        .map(answer => BasicPurposeReading.simplify(packet, paths, ownedPair, positions, local, answer))
        .map(answer => RootSubjectComparison.withMaterialContrast(answer, ownedPair, packet.assembly.playedTransition.map(_.moveUci)))
        .flatMap(RootQuestionSummary.realize(paths, ownedPair, _))
        .map(reading => if (reading.json \ "question").asOpt[String].exists(
            Set("first_target_response", "exchange_function")) &&
          firstTargets.size == 1 then reading.copy(json = reading.json +
            ("first_target_response" -> firstTargets.head.ownershipJson)) else reading)
      val unavailable = unavailableAnswer(answerChoice.map(_.fold(_.question, _.question)), commonReading)
      requests.zip(allocated).zipWithIndex.map { case (((value, _), parts), owner) =>
        val text = textFor(owner, parts)
        val effects = parts.flatMap(_.effects).distinct
        val functions = parts.flatMap(_.functions).distinct
        val summary = commonReading.filter(_ => owner == 0).map(_.text)
        val baseValue = value + ("detail" -> (((value \ "detail").as[JsObject] - "summary" - "summary_kind" - "summary_exchange" - "summary_question" - "summary_unavailable") ++
          (if owner == 0 then unavailable else Json.obj())))
        if text == originals(owner) && summary.isEmpty then baseValue else
          baseValue ++ Json.obj("evidence_ids" -> ((value \ "evidence_ids").as[List[String]] ++
            (if pv then Nil else effects.flatMap(_.evidence) ++ functions.flatMap(_.notes.flatMap(noteEvidence))) ++
            (if owner == 0 then commonReading.toList.flatMap(_.evidence) else Nil)).distinct,
            "detail" -> (((baseValue \ "detail").as[JsObject] - "effects") ++ Json.obj("text" -> text) ++
              summary.fold(Json.obj())(value => Json.obj("summary" -> value)) ++
              commonReading.filter(_ => owner == 0 && summary.nonEmpty).fold(Json.obj()) { reading =>
                Json.obj("summary_kind" -> "question_answer", "summary_question" -> reading.json)
              } ++
              Option.when(effects.nonEmpty)(Json.obj("effects" -> effects.map(_.json(publicSource)))).getOrElse(Json.obj())))
      }
    ComparisonReaders(compose, rootUses)

  /** Independent physical uses keep separate ownership. Only an actual shared
    * non-root capture can join their price episodes; a common PV, endpoint,
    * root frame or co-observed check is not such a dependency. */
  private[runtime] def physicalEpisodes(pair: List[ObservedLineProtocol.PreparedPath],
      sections: List[CommentaryAnswer.Section]): List[CommentaryAnswer.Section] =
    import CommentaryAnswer.*
    import RootQuestionSummary.MoveRef
    val captures = pair.flatMap(path => path.certified.flatMap(_._2.material).toList.flatMap(
      _.captures.filter(_.plyOffset > 0).map(event => MoveRef(path, event.plyOffset)))).toSet
    def prices(section: Section) = section.covered.filter(captures).toSet
    sections.foldLeft(List.empty[Section]) { (groups, section) =>
      val owned = prices(section)
      val shared = groups.filter(group => prices(group).exists(owned))
      if shared.isEmpty then groups :+ section
      else
        val joined = shared :+ section
        val merged = Section(joined.flatMap(_.statements).distinct,
          joined.flatMap(_.required).distinct, joined.flatMap(_.focus).distinctBy(_.move))
        val first = groups.indexWhere(shared.contains)
        groups.take(first) ++ (merged :: groups.drop(first).filterNot(shared.contains))
    }

  /** A check between two selected captures stays between them. Only the flat
    * capture/check tail is ordered here; complete support/route units stay intact. */
  private[runtime] def orderedCaptureSteps(pair: List[ObservedLineProtocol.PreparedPath],
      checks: List[CommentaryAnswer.CheckSequence], captures: List[CommentaryAnswer.MaterialEvents])
      : List[CommentaryAnswer.Statement] =
    import CommentaryAnswer.*
    pair.flatMap { path =>
      val captured: List[(Int, Int, Statement)] = captures.filter(_.path eq path).flatMap(_.events).distinct.map(event =>
        (event.plyOffset, 0, MaterialEvents(path, List(event))))
      val sequences = checks.filter(_.move.path eq path)
      val events = sequences.flatMap(_.events).distinct.sortBy(_.move.offset)
      // Keep the original sequence anchor on its first check occurrence.
      val checking: List[(Int, Int, Statement)] = events.headOption.toList.flatMap { first =>
        val (head, tail) = events.span(_.move.offset == first.move.offset)
        (first.move.offset, 1, CheckSequence(sequences.head.move, head)) ::
          tail.map(event => (event.move.offset, 1, event))
      }
      (captured ++ checking).sortBy(value => value._1 -> value._2).map(_._3)
    }

  /** Read checking moves and their actual replies from the certified inventory. */
  private[runtime] def checkSteps(path: ObservedLineProtocol.PreparedPath, offsets: List[Int],
      initialResponse: Option[Int], observations: Map[String, List[MoveObservation]])
      : List[(Int, CommentaryAnswer.CheckEvent, List[String], chess.Color)] =
    def at(offset: Int) = observations.getOrElse(path.ids(offset), Nil)
    val checks = offsets.flatMap(offset => at(offset).collect {
      case observation @ MoveObservation(_, _, MoveObservationMeaning.CertifiedRelation(
          detail: RelationWitnessDetail.CreatedCheckResponseInventory), _) => (offset, observation, detail)
    })
    val responses = (initialResponse.toList ++ checks.map(_._1 + 1)).filter(_ < path.moves.size).distinct.sorted
      .flatMap(offset => at(offset).collect {
        case observation @ MoveObservation(_, _, MoveObservationMeaning.CertifiedRelation(
            detail: RelationWitnessDetail.RootCheckResponse), _)
            if EvidenceRef.sameMove(detail.response.resource.moveUci, path.moves(offset)) =>
          (offset, observation, detail)
      })
    val checking = checks.map { (offset, observation, detail) =>
      val checkers = detail.checkers.map(p => RelationColoredPieceWitness(p.square, p.role, detail.mover.side))
      val opened = at(offset).filter(_.meaning match
        case MoveObservationMeaning.PieceControl(before, after) =>
          before.state == PieceControlState.Absent && after.state == PieceControlState.Present &&
            before.controller == after.controller && before.target == after.target &&
            after.controller.exists(p => checkers.contains(p) && p.at != detail.mover.to.key &&
              Set("bishop", "rook", "queen")(p.roleName)) &&
            after.target.exists(p => p.side == detail.checkedSide && p.square == detail.kingSquare)
        case _ => false)
      val openedCheckers = opened.flatMap(_.meaning match
        case MoveObservationMeaning.PieceControl(_, after) => after.controller
        case _ => None).distinct
      val action = CommentaryAnswer.CheckingMove(RootQuestionSummary.MoveRef(path, offset), detail, openedCheckers)
      (offset, action, (observation :: opened).flatMap(_.premises.map(_.id)), detail.checkedSide)
    }
    val responsesInOrder = responses.map { (offset, observation, detail) =>
      (offset, CommentaryAnswer.CheckReply(RootQuestionSummary.MoveRef(path, offset), detail),
        observation.premises.map(_.id), detail.respondingSide)
    }
    (responsesInOrder ++ checking).sortBy(_._1)

  /** A named move, convergence, or empty material ledger does not explain the
    * move's current role. Reuse a proved basic action only where that role is
    * still missing; it neither answers a different question nor claims safety. */
  private[runtime] def withRootActions(pair: List[ObservedLineProtocol.PreparedPath],
      notes: List[LocalCandidate], answer: CommentaryAnswer.Answer): CommentaryAnswer.Answer =
    import CommentaryAnswer.*
    import RootQuestionSummary.MoveRef
    val statements = answer.core.flatMap(_.statements)
    val explained = statements.flatMap {
      case _: Continuation | _: Convergence | _: ComparisonSubject | _: Divergence |
          _: NoMaterialChange | _: RecordedEnd => Nil
      case statement => statement.actionReferences
    }.toSet
    val additions = pair.filter(_.automaticTerminal.isEmpty).flatMap { path =>
      val root = MoveRef(path, 0)
      if explained(root) then None
      else notes.iterator.filter(note => path.ids.headOption.contains(note.edge.evidence.id)).flatMap { note =>
        val action = note.note.detail match
          case value: Detail.Development => Some(value)
          case value: Detail.Castle => Some(value)
          // A shared empty square is extra geometry, not the reason an open
          // file must matter. Its original fact remains available in detail.
          case Detail.RookFile(actor, fileType, _) => Some(Detail.RookFile(actor, fileType, None))
          case _ => None
        action.map { value =>
          // A narrower purpose must not reintroduce an omitted helper
          // relation through its hover frame. Keep its proof, and use the
          // ordinary exact-move frame when the displayed fact was narrowed.
          Section(List(Action(root, value)), focus =
            Option.when(value == note.note.detail)(Focus(root, note)).toList) -> note.evidence
        }
      }.nextOption()
    }
    if additions.isEmpty then answer
    else
      val sections = additions.map(_._1)
      val roots = sections.flatMap(_.references).toSet
      val (context, core) = answer.core.partition(section => section.statements.nonEmpty &&
        section.statements.forall {
          case NoMaterialChange(root, _) => roots(root)
          case _ => false
        })
      answer.copy(core = sections ++ core, details = (context ++ answer.details).distinct,
        evidence = (answer.evidence ++ additions.flatMap(_._2)).distinct)

  /** Complete only the actual uses in the chosen answer. End-position anchors
    * and expandable continuations do not activate every earlier move. */
  /** Replace an episode only after its selected root uses independently close.
    * Price events remain obligations; they never become new idea seeds. An
    * unknown or interrupted user account keeps that particular old episode.
    */
  private[runtime] def protectRootUses(packet: EvidenceBackedJudgmentPacket,
      paths: ObservedLineProtocol.PreparedPaths, pair: List[ObservedLineProtocol.PreparedPath], positions: Map[PositionNodeRef, Position],
      notes: List[LocalCandidate], answer: CommentaryAnswer.Answer,
      bundles: List[CommentaryAnswer.Section]): CommentaryAnswer.Answer =
    import CommentaryAnswer.*
    paths.requireOwner(packet)
    // Only an originally selected first enemy reply may seed another use.
    // Prices added by completion are obligations, not new idea selections.
    val selectedReplies = answer.completionSources.toList.flatten.flatMap(_.actionReferences).filter { ref =>
      val path = ref.path
      ref.offset == 1 && pair.exists(_ eq path) && paths.paths.exists(_ eq path) &&
        path.certified.nonEmpty && path.ids == path.declared && path.moves == path.sourceMoves &&
        paths.positionView(path, 0).flatMap(_.legalRows).flatMap(_.find(row =>
          path.moves.headOption.contains(row.move))).exists { root =>
          paths.positionView(path, 1).flatMap(_.legalRows).flatMap(_.find(row =>
            path.moves.lift(1).contains(row.move))).exists(_.actor.side != root.actor.side)
        }
    }.toSet
    def close(section: Section): Option[List[Section]] =
      val uses = section.statements.flatMap(conditionUses).collect {
        case (condition, use) if condition.offset == 0 || selectedReplies(condition) => use
      }.distinct
      val usedPaths = (pair ++ uses.map(_.path)).distinct
      val arms = usedPaths.flatMap(path => paths.continuationReadings.find(reading =>
        (reading.path eq path) && reading.complete && path.certified.flatMap(_._2.material).nonEmpty)
        .map(reading => new RootQuestionSummary.Arm(packet, paths, path, positions, reading)))
      if uses.isEmpty || arms.size != usedPaths.size then return None
      val users = uses.map(use => arms.find(_.path eq use.path).flatMap(_.mover(use.offset)))
      if users.exists(_.isEmpty) then return None
      val accounts = uses.zip(users.flatten).map { (use, user) =>
        val arm = arms.find(_.path eq use.path).get
        arm.captures.find(_.plyOffset == use.offset) match
          case Some(capture) => Some(List(arm.comparisonAccount(capture, user)))
          case None =>
            val after = arm.at(user, use.offset + 1)
            arm.captures.find(event => event.plyOffset > use.offset && event.originalVictim.contains(user)) match
              case Some(loss) if after.nonEmpty && (use.offset + 1 to loss.plyOffset).forall(count =>
                  arm.position(count).nonEmpty && arm.at(user, count) == after) =>
                // This actual quiet user is lost before moving again. Its
                // capturer's connected recovery may also be its price.
                val targets = section.statements.collect {
                  case Action(move, Detail.Attack(actor, target, _)) if move == use && after.contains(actor) => target
                }.flatMap(target => arm.originals.find(arm.at(_, use.offset + 1).contains(target)))
                  .filter(target => target.side != user.side &&
                    arm.control(use.offset + 1, user, target).contains(true)).toSet
                Some(List(arm.usedPieceLossAccount(loss, user, targets)))
              case Some(_) => None
              case None => Some(Nil)
      }
      if accounts.exists(_.isEmpty) || uses.zip(accounts.flatten).exists { (use, values) =>
          val arm = arms.find(_.path eq use.path).get
          values.exists { account =>
            val unanswered = account.offsets.filter(offset => arm.checks(offset) && !account.offsets.contains(offset + 1))
            !account.complete && !(unanswered.nonEmpty && unanswered.forall(offset =>
              ownedPriceEnding(arm, section.statements, offset)))
          }
        } then return None
      val duties = uses.zip(accounts.flatten).flatMap { (use, values) => values.flatMap(account =>
        account.offsets.map(RootQuestionSummary.MoveRef(use.path, _))) }
      // An exact quiet use can answer the opponent's capturing arrival. Keep
      // that immediate gain when the chosen action attacks its actual mover;
      // the target's remaining lifetime is not a new price account.
      val arrivals = uses.flatMap { use =>
        val arm = arms.find(_.path eq use.path).get
        section.statements.collect { case Action(move, Detail.Attack(actor, target, _)) if move == use => (actor, target) }
          .flatMap { (actor, target) =>
            for
              user <- arm.mover(use.offset).toList if arm.at(user, use.offset + 1).contains(actor) &&
                actor.side != target.side && !arm.captures.exists(_.plyOffset == use.offset)
              original <- arm.originals.find(arm.at(_, use.offset).contains(target)).toList
              capture <- arm.captures.filter(event => event.plyOffset == use.offset - 1 &&
                event.originalActor.contains(original) && event.originalVictim.exists(_.side == actor.side))
              if arm.at(original, use.offset + 1).contains(target) && arm.covers(use.offset + 1, user, original)
            yield RootQuestionSummary.MoveRef(use.path, capture.plyOffset)
          }
      }
      val requiredDuties = (duties ++ arrivals).distinct
      val already = section.covered.toSet
      val existingChecks = section.statements.flatMap {
        case value: CheckSequence => value.events
        case value: CheckEvent => List(value)
        case _ => Nil
      }.toSet
      val price = usedPaths.flatMap { path =>
        val arm = arms.find(_.path eq path).get
        val material = path.certified.flatMap(_._2.material).get
        val events = material.events.filter(event => requiredDuties.contains(RootQuestionSummary.MoveRef(path, event.plyOffset)) &&
          !already(RootQuestionSummary.MoveRef(path, event.plyOffset)) &&
          !(event.plyOffset == 0 && pair.exists(_ eq path)))
        val checkOffsets = requiredDuties.collect { case ref if (ref.path eq path) && arm.checks(ref.offset) => ref.offset }
          .distinct.sorted
        val checks = arm.checkEvents(checkOffsets).filterNot(existingChecks)
        val checking = Option.when(checks.nonEmpty)(CheckSequence(checks.head.move, checks)).toList
        val captures = Option.when(events.nonEmpty)(MaterialEvents(path, events)).toList
        val statements = orderedCaptureSteps(List(path), checking, captures)
        Option.when(statements.nonEmpty)(Section(statements))
      }
      val rootCaptures = pair.flatMap { path =>
        val events = path.certified.flatMap(_._2.material).toList.flatMap(_.events.filter(event =>
          event.plyOffset == 0 && !already(RootQuestionSummary.MoveRef(path, 0))))
        Option.when(events.nonEmpty)(Section(List(MaterialEvents(path, events))))
      }
      val seeds = section.statements ++ rootCaptures.flatMap(_.statements)
      // The episode can belong to either arm. Both ordinary root anchors are
      // still needed for a comparison preview; they are not new idea seeds.
      val roots = Section(List(Continuation(pair.map(RootQuestionSummary.MoveRef(_, 0)))))
      val candidate = Answer(answer.question, roots :: section :: (rootCaptures ++ price), Nil, answer.evidence,
        comparison = Some(ComparisonScope(users.flatten.toSet,
          protectedActions = section.actionReferences.toSet ++ rootCaptures.flatMap(_.actionReferences))),
        completionSources = Some(seeds))
      completeSelectedConditions(packet, paths, pair, positions, notes, candidate).filter { closed =>
        val required = closed.required.toSet
        val covered = closed.core.flatMap(_.covered).toSet
        requiredDuties.forall(required) && uses.forall(required) && requiredDuties.filter(ref =>
          ref.path.certified.flatMap(_._2.material).exists(_.captures.exists(_.plyOffset == ref.offset))).forall(covered) &&
          RootQuestionSummary.realize(paths, pair, closed).nonEmpty
      }.map(_.core)
    RootSubjectComparison.protect(answer, bundles, close, selectedReplies)

  /** A captured promoted role still belongs to its exact original pawn.
    * Keep the certified role/material change even when it was not a check. */
  private[runtime] def capturedOriginalPromotions(arm: RootQuestionSummary.Arm,
      events: List[ObservedLineMaterialEvent]): List[ObservedLineMaterialEvent] =
    events.flatMap { capture => capture.capture.toList.flatMap { cost =>
      arm.captures.find(_.plyOffset == capture.plyOffset).flatMap(_.originalVictim).toList.flatMap { original =>
        arm.material.events.filter(event => event.plyOffset < capture.plyOffset &&
          event.promotionGainCp > 0 && event.movement.beforeRole.name == "pawn" &&
          event.movement.afterRole == cost.capturedRole &&
          arm.path.moves.lift(event.plyOffset).contains(event.moveUci) &&
          arm.reading.events.exists(use => use.plyOffset == event.plyOffset && use.originalActor.contains(original) &&
            arm.path.ids.lift(use.plyOffset).contains(use.occurrenceId)) &&
          arm.at(original, event.plyOffset + 1).exists(_.role == cost.capturedRole) &&
          arm.at(original, capture.plyOffset).exists(_.role == cost.capturedRole))
      }
    }}.distinct.sortBy(_.plyOffset)

  private[runtime] def completeSelectedConditions(packet: EvidenceBackedJudgmentPacket,
      paths: ObservedLineProtocol.PreparedPaths, pair: List[ObservedLineProtocol.PreparedPath],
      positions: Map[PositionNodeRef, Position], notes: List[LocalCandidate],
      answer: CommentaryAnswer.Answer): Option[CommentaryAnswer.Answer] =
    import CommentaryAnswer.*
    paths.requireOwner(packet)
    if pair.exists(path => !paths.paths.exists(_ eq path)) ||
        answer.required.exists(ref => !paths.paths.exists(_ eq ref.path) || ref.offset < 0 || ref.offset >= ref.path.moves.size)
    then return None
    // A price added while closing this answer is not a newly selected idea.
    // Re-reading that loss as a fresh move would create another attack/escape
    // episode and pull unrelated earlier trades into the basic explanation.
    val completionSources = answer.completionSources.getOrElse(answer.core.flatMap(_.statements))
    val selectedPaths = (pair ++ answer.core.flatMap(_.actionReferences.map(_.path))).distinct
    val arms = selectedPaths.flatMap(path => paths.continuationReadings.find(reading =>
      (reading.path eq path) && reading.complete && path.certified.flatMap(_._2.material).nonEmpty)
      .map(reading => new RootQuestionSummary.Arm(packet, paths, path, positions, reading)))
    def promotionContexts(core: List[Section]): List[Section] =
      val statedPromotions = core.flatMap(_.statements).collect { case value: MaterialEvents => value }
        .flatMap(value => value.events.filter(_.promotionGainCp > 0).map(event => value.path -> event))
      core.map { section => section.copy(statements = section.statements.map {
        case value: MaterialEvents =>
          val promotions = arms.find(_.path eq value.path).toList.flatMap { arm =>
            capturedOriginalPromotions(arm, value.events).filterNot(event =>
              statedPromotions.exists((path, stated) => (path eq value.path) && stated == event))
          }
          value.copy(events = (value.events ++ promotions).distinct.sortBy(_.plyOffset))
        case statement => statement
      }) }
    val selectedAnswer = answer.copy(completionSources = Some(completionSources),
      core = promotionContexts(answer.core).map { section =>
      val responses = section.statements.collect { case response: TargetResponse if arms.exists(arm =>
        targetResponseActions(arm, response).nonEmpty) => response }
      // ponytail: remove only same-section escape introductions; cross-section
      // routes retain their metadata until a real need justifies moving it.
      section.copy(statements = section.statements.filterNot {
        case route: PieceRoute => responses.exists(response => (route.path eq response.path) &&
          route.original == response.originalTarget && route.offsets == response.escape.toList)
        case _ => false
      })
    })
    val additions = selectedConditionSections(arms, selectedAnswer, notes)
    val core = additions.foldLeft(selectedAnswer.core) { case (sections, (use, condition)) =>
      // A reply's conditions belong with its action, after any preceding comparison of values.
      val owners = sections.indices.filter(index => sections(index).references.contains(use) &&
        (condition.statements.nonEmpty || condition.required.forall(sections(index).references.contains)))
      val owner = owners.find(index => sections(index).actionReferences.contains(use)).getOrElse(owners.head)
      val selected = sections(owner)
      val before = selected.statements.indexWhere(_.references.contains(use))
      // A complete comparison states the decision and the tested reply together.
      // Keep that lead intact; supplementary rules of a later reply follow it.
      val at = before + (selected.statements(before) match
        case _: ReserveEntry | _: DirectReserveEntry | _: ReserveHold | _: ReserveTempo | _: ReserveBreak | _: ReplyConstraint |
            _: ReferencePurpose | _: ReferencePosition => 1
        case _ => 0)
      val statements = (selected.statements.take(at) ++ condition.statements ++ selected.statements.drop(at)).distinct
      sections.updated(owner, selected.copy(statements = statements,
        required = (selected.required ++ condition.required).distinct,
        focus = (selected.focus ++ condition.focus).distinctBy(_.move)))
    }
    completeSelectedPrices(arms, selectedAnswer.copy(core = core), completionSources, additions)
      .flatMap { completed =>
        // A newly owned actual loss may need its victim's earlier role change.
        // This adds identity context after pricing; it cannot open a new price interval.
        val contextual = completed.copy(core = promotionContexts(completed.core))
        // Actual displayed moves always carry their intrinsic capture/check
        // duties. An unchanged selection can still lack those receipts (for
        // example a capturing check already present in a protected sequence).
        // Closing them does not promote the generated prices into idea seeds.
        RootQuestionSummary.completePairAnswer(packet, paths, pair, answer.question, contextual.core)
          .map(closed => closed.copy(details = answer.details,
            evidence = (answer.evidence ++ closed.evidence ++ arms.flatMap(_.proofs)).distinct,
            comparison = answer.comparison, completionSources = Some(completionSources)))
      }

  /** Prices belong to the originally selected attack/support episode. Material
    * inside its actual exchange interval is retained, but cannot seed another
    * episode. A quiet final position never enlarges that interval.
    */
  private def ownedPriceEnding(arm: RootQuestionSummary.Arm,
      selected: List[CommentaryAnswer.Statement], offset: Int): Boolean =
    val path = arm.path
    offset == path.moves.size - 1 && arm.reading.complete && (arm.reading.path eq path) &&
      path.ids == path.declared && path.moves == path.sourceMoves && selected.exists {
        case CommentaryAnswer.RecordedEnd(move) => (move.path eq path) && move.offset == offset && path.automaticTerminal.isEmpty
        case CommentaryAnswer.Terminal(move, result) => (move.path eq path) && move.offset == offset &&
          path.automaticTerminal.contains(result)
        case _ => false
      }

  private[runtime] def completeSelectedPrices(arms: List[RootQuestionSummary.Arm],
      answer: CommentaryAnswer.Answer, selected: List[CommentaryAnswer.Statement],
      dependencies: List[(RootQuestionSummary.MoveRef, CommentaryAnswer.Section)] = Nil)
      : Option[CommentaryAnswer.Answer] =
    import CommentaryAnswer.*
    import LineContinuationInvestigation.{DirectSupportUse, Event}
    import RootQuestionSummary.MoveRef
    val supportTargets = selected.flatMap {
      case Action(move, Detail.Support(provider, target, opposition, _)) if move.offset == 0 =>
        arms.find(_.path eq move.path).toList.flatMap { owner =>
          for
            p <- owner.originals.find(owner.at(_, 1).contains(provider)).toList
            t <- owner.originals.find(owner.at(_, 1).contains(target)).toList
            actor <- opposition.flatMap(enemy => owner.originals.find(owner.at(_, 1).contains(enemy)))
            if p.side == t.side && actor.side != t.side && owner.control(1, p, t).contains(true)
          yield (p, t, actor)
        }
      case _ => Nil
    }.distinct
    def complete(arm: RootQuestionSummary.Arm, current: Answer): Option[Answer] =
      val path = arm.path
      val scoped = answer.comparison.nonEmpty
      def original(piece: Piece, count: Int) = arm.originals.find(arm.at(_, count).contains(piece))
      def root(move: MoveRef) = (move.path eq path) && move.offset == 0
      def ownedEnding(offset: Int): Boolean = ownedPriceEnding(arm, selected, offset)
      def otherTargets(attacker: Piece, target: Piece, start: Int): List[(Event, Piece, List[Event])] =
        arm.captures.flatMap { event =>
          event.originalVictim.filter(other => other != target && other.side == target.side &&
            event.originalActor.contains(attacker) && event.plyOffset >= start &&
            (start to event.plyOffset).forall(arm.covers(_, attacker, other)))
            .map(other => (event, other, Nil))
        }
      val forks = selected.flatMap {
        case Action(move, Detail.Evades(after, attacker)) if root(move) =>
          (for
            target <- original(after, 1).toList if arm.root.contains(target)
            actor <- original(attacker, 0).toList
            if arm.covers(0, actor, target) && arm.control(1, actor, target).contains(false)
          yield otherTargets(actor, target, 0)).flatten
        case response: TargetResponse if (response.path eq path) && response.start == 0 &&
            arm.at(response.originalTarget, 1).contains(response.target) &&
            arm.covers(1, response.originalAttacker, response.originalTarget) =>
          otherTargets(response.originalAttacker, response.originalTarget, 1)
        case _ => Nil
      }
      val supportUses: List[DirectSupportUse] = selected.flatMap {
        case value: SupportRecapture if root(value.after) && (value.witness eq path) => List(value.use)
        case value: SupportTransfer if root(value.move) => List(value.use)
        case value: SupportedCapture if root(value.move) => List(value.use)
        case _ => Nil
      }
      val supports = selected.flatMap {
        case Action(move, Detail.Support(provider, target, _, _)) if root(move) =>
          for p <- original(provider, 1).toList; t <- original(target, 1).toList yield p -> t
        case value: SupportTransfer if root(value.move) =>
          for p <- original(value.provider, 1).toList; t <- original(value.target, 1).toList yield p -> t
        case value: SupportedCapture if root(value.move) =>
          for p <- original(value.provider, 1).toList; t <- original(value.target, 1).toList yield p -> t
        case value: SupportRetention if root(value.move) &&
            arm.at(value.originalProvider, 1).contains(value.provider) &&
            arm.at(value.originalTarget, 1).contains(value.target) =>
          List(value.originalProvider -> value.originalTarget)
        case value: SupportRecapture if root(value.after) && (value.witness eq path) =>
          original(value.use.provider, value.use.supportCount).toList.map(_ -> value.use.originalTarget)
        case _ => Nil
      }.distinct
      def sameRoles(start: Int, end: Int, pieces: Piece*): Boolean = start <= end && pieces.forall(piece =>
        arm.at(piece, start).exists(first => (start to end).forall(n => arm.at(piece, n).exists(_.role == first.role))))
      def attackedUntil(provider: Piece, loss: Event, start: Int): Boolean =
        loss.originalVictim.contains(provider) && loss.originalActor.exists(attacker =>
          attacker.side != provider.side && sameRoles(start, loss.plyOffset, provider, attacker) &&
            (start to loss.plyOffset).forall(arm.covers(_, attacker, provider)))
      val verifiedSupportUses = supports.flatMap { (provider, target) =>
        supportUses.filter(use => use.originalTarget == target &&
          use.recapture.originalActor.contains(provider) &&
          arm.captures.contains(use.targetCapture) && arm.captures.contains(use.recapture) &&
          arm.at(provider, use.supportCount).contains(use.provider) &&
          sameRoles(1, use.targetCapture.plyOffset, provider, target) &&
          (1 to use.supportCount).forall(count => arm.position(count).nonEmpty &&
            arm.at(provider, count).contains(use.provider)) && arm.covers(use.supportCount, provider, target))
      }.distinct
      val supportPrices = supports.flatMap { (provider, target) =>
        val uses = verifiedSupportUses.filter(use => use.originalTarget == target &&
          use.recapture.originalActor.contains(provider) &&
          (1 to use.targetCapture.plyOffset).forall(arm.covers(_, provider, target)))
        arm.captures.find(_.originalVictim.contains(provider)).flatMap { loss =>
          val retained = sameRoles(1, loss.plyOffset, provider, target) && attackedUntil(provider, loss, 1) &&
            (1 to loss.plyOffset).forall(arm.control(_, provider, target).contains(true))
          val used = uses.filter(use => arm.recovery(use.recapture).contains(loss) &&
            attackedUntil(provider, loss, use.recapture.plyOffset + 1))
          Option.when(retained || used.nonEmpty)((loss, provider,
            used.flatMap(use => List(use.targetCapture, use.recapture))))
        }
      }
      val targetPrices = supportTargets.flatMap { (provider, target, attacker) =>
        for
          supported <- arm.control(1, provider, target).toList
          loss <- arm.captures.find(_.originalVictim.contains(target)).toList
          if loss.plyOffset >= 1 && loss.originalActor.contains(attacker) &&
            (0 to loss.plyOffset).forall(arm.covers(_, attacker, target)) &&
            (1 to loss.plyOffset).forall(arm.control(_, provider, target).contains(supported))
        yield (loss, target, Nil)
      }
      val seeds = (forks ++ supportPrices ++ targetPrices).distinctBy((seed, focus, _) => seed.id -> focus)
      if seeds.nonEmpty && arm.root.isEmpty then return None
      val accounts = seeds
        .map { (seed, focus, used) =>
          val protectedUse = answer.comparison.exists(_.protectedActions(arm.ref(seed.plyOffset)))
          val protectedSupport = used.nonEmpty && answer.comparison.exists(scope =>
            used.exists(event => scope.protectedActions(arm.ref(event.plyOffset))))
          val account = if scoped && !protectedUse && !protectedSupport then arm.comparisonAccount(seed, focus)
            else arm.account(seed, focus)
          // A selected loss may have a delayed recovery without making its
          // capturer's later lifetime part of the question.
          val recoveries = (account.events ++ used).distinct.flatMap(arm.continuousRecovery)
          val events = (account.events ++ used ++ recoveries ++ arm.captures.filter(_.plyOffset == 0)).distinct
          val checks = recoveries.filter(event => arm.checks(event.plyOffset - 1)).map(_.plyOffset - 1)
          val unanswered = account.offsets.filter(n => arm.checks(n) && !account.offsets.contains(n + 1))
          // An owned recorded ending bounds this observed account; it does
          // not supply a check reply or change the account's completeness.
          val bounded = account.complete || (unanswered.nonEmpty && unanswered.forall(ownedEnding))
          (events.map(_.plyOffset), (account.offsets ++ events.map(_.plyOffset) ++ checks :+ 0).distinct.sorted, bounded)
        }
      if accounts.exists(!_._3) then return None
      def stated(statement: Statement): List[(List[ObservedLineMaterialEvent], List[Int])] = statement match
        case value: Material if value.path eq path => List(value.cost.events -> value.cost.offsets)
        case value: MaterialEvents if value.path eq path =>
          if value.events.exists(_.promotionGainCp != 0) then List(value.events -> value.references.map(_.offset))
          else value.events.map(event => List(event) -> List(event.plyOffset))
        case value: TargetResponse if value.path eq path => value.material.toList.flatMap(stated)
        case _ => Nil
      val previous = current.core.flatMap(_.statements.flatMap(stated)).filter(_._1.nonEmpty)
      if previous.exists((events, _) => !events.forall(arm.material.events.contains)) then return None
      def interval(offsets: List[Int]) = arm.material.events.filter(event =>
        if scoped then offsets.contains(event.plyOffset)
        else offsets.nonEmpty && event.plyOffset >= offsets.min && event.plyOffset <= offsets.max)
      // Each tuple is one existing account or one selected relation's price;
      // only a shared actual material event permits joining two accounts.
      // These dependencies start with the selected content, never with newly
      // found prices. Reading an available capture is not observing its use.
      val uses = selected.flatMap(_.covered).filter(_.path eq path).map(_.offset).toSet
      val captures = arm.captures.filter(event => uses(event.plyOffset))
      val stationarySupportLosses = verifiedSupportUses.flatMap { use =>
        for
          provider <- use.recapture.originalActor.toList
          after <- arm.at(provider, use.recapture.plyOffset + 1).toList
          loss <- arm.captures.find(event => event.plyOffset > use.recapture.plyOffset &&
            event.originalVictim.contains(provider)).toList
          if arm.identityRoles(List(loss)) &&
            (use.recapture.plyOffset + 1 to loss.plyOffset).forall(count =>
              arm.position(count).nonEmpty && arm.at(provider, count).contains(after))
        yield use -> loss
      }
      val namedPrices = selected.flatMap(stated).flatMap(_._1).toSet
      val identityPromotions = previous.flatMap(_._1).filter(event => event.promotionGainCp != 0 &&
        !namedPrices(event) && !uses(event.plyOffset)).toSet
      val clearances = dependencies.flatMap { (use, section) =>
        section.statements.collect { case value: ClearedRoute if value.use == use &&
            (use.path eq path) && uses(use.offset) => value }.flatMap { value =>
          arm.captures.find(_.plyOffset == use.offset).toList.flatMap { after =>
            val departure = arm.captures.filter(_.plyOffset == value.move.offset)
            // A new attack needs its actual opening/use receipt. Physical
            // identity alone cannot reconnect an interrupted attack episode.
            val gains = captures.filter(event => event.plyOffset < value.move.offset &&
              event.originalActor == after.originalVictim && event.originalActor.exists(target =>
                arm.at(target, value.move.offset).exists(value.targets.contains) &&
                  (event.plyOffset + 1 to use.offset).forall(count =>
                    arm.at(target, count) == arm.at(target, value.move.offset)) &&
                  (value.move.offset + 1 to use.offset).forall(arm.covers(_, value.originalUser, target))))
            (departure ++ gains).distinct.map { before =>
              (List(before.plyOffset, after.plyOffset) ++ arm.continuousRecovery(after).map(_.plyOffset)) -> List.empty[Statement]
            }
          }
        }
      }
      val exposures = captures.flatMap { selectedCapture =>
        arm.reading.captureExposures.filter(exposure => exposure.occurrenceId == selectedCapture.occurrenceId &&
          exposure.prefixMoves == path.moves.take(selectedCapture.plyOffset + 1) &&
          exposure.blockers.exists((original, _) => selectedCapture.originalActor.contains(original)))
          .flatMap { exposure =>
            for
              use <- arm.captures.find(event => event.plyOffset > selectedCapture.plyOffset &&
                event.originalActor.contains(exposure.originalAttacker) &&
                event.originalVictim.contains(exposure.originalTarget) &&
                exposure.captures.exists(row => EvidenceRef.sameMove(row.move, path.moves(event.plyOffset)))).toList
              if (selectedCapture.plyOffset + 1 to use.plyOffset).forall(count =>
                arm.at(exposure.originalAttacker, count).contains(exposure.attacker) &&
                arm.at(exposure.originalTarget, count).contains(exposure.target) &&
                arm.covers(count, exposure.originalAttacker, exposure.originalTarget))
              (blocker, before) <- exposure.blockers.filter((original, _) =>
                selectedCapture.originalActor.contains(original))
              mover <- arm.at(blocker, selectedCapture.plyOffset + 1).toList
              user <- arm.at(exposure.originalAttacker, use.plyOffset + 1).toList
            yield
              arm.position(selectedCapture.plyOffset).foreach(position =>
                arm.proofs ++= position.sourceRefs(List(exposure.raySourceId)))
              arm.position(selectedCapture.plyOffset + 1).foreach(position =>
                arm.proofs ++= position.sourceRefs(exposure.controlSourceIds ++ exposure.captures.map(_.sourceId)))
              val route = ClearedRoute(arm.ref(selectedCapture.plyOffset), before, mover,
                exposure.originalAttacker, arm.ref(use.plyOffset), user, List(exposure.target),
                passage = true, development = None)
              val offsets = List(selectedCapture.plyOffset, use.plyOffset) ++ arm.continuousRecovery(use).map(_.plyOffset)
              offsets -> List[Statement](route)
          }
      }
      // The alternative takes this very attacker with the threatened piece.
      // In this arm the actual first reply takes that piece instead. The
      // selected attack's use owns that price, not unrelated earlier losses.
      val initialReplies = selected.flatMap {
        // A delayed escape from a root attack must retain the actual first
        // capturing reply and its immediate recovery. This is a bounded reply,
        // not an interval containing the rest of the target's lifetime.
        case response: TargetResponse if (response.path eq path) && response.start == 0 &&
            arm.root.contains(response.originalAttacker) && response.escape.exists(_ > 1) =>
          for
            escape <- response.escape.toList
            if (1 to escape).forall(arm.covers(_, response.originalAttacker, response.originalTarget))
            reply <- arm.captures.filter(_.plyOffset == 1)
          yield
            val recovery = arm.continuousRecovery(reply).filter(event => event.plyOffset == 2 && event.plyOffset < escape)
            val events = reply :: recovery.toList
            val receipt: Statement = if arm.identityRoles(events) then arm.pieceExchange(events)
              else MaterialEvents(path, arm.material.events.filter(row => events.exists(_.plyOffset == row.plyOffset)))
            events.map(_.plyOffset) -> List[Statement](response, receipt)
        case Action(move, Detail.Attack(actor, target, _)) if root(move) =>
          for
            user <- original(actor, 1).toList if arm.root.contains(user)
            subject <- original(target, 1).toList
            use <- captures.filter(event => event.originalActor.contains(user) &&
              event.originalVictim.contains(subject) && event.plyOffset > move.offset)
            if (move.offset + 1 to use.plyOffset).forall(arm.covers(_, user, subject))
            reply <- arm.captures.filter(_.plyOffset == move.offset + 1)
            if reply.originalActor.zip(reply.originalVictim).exists((attacker, threatened) =>
              (move.offset to reply.plyOffset).forall(arm.covers(_, attacker, threatened)))
            other <- arms.filter(a => (a.path ne path) && a.path.positions.headOption == path.positions.headOption)
            take <- other.captures.filter(event => event.plyOffset == 0 &&
              selected.exists(_.covered.contains(other.ref(0))) &&
              event.originalActor == reply.originalVictim && event.originalVictim == reply.originalActor)
          yield
            val removed = arm.continuousRecovery(use).toList
            val recovered = removed.flatMap(arm.continuousRecovery)
            (List(reply.plyOffset, use.plyOffset) ++ (removed ++ recovered).map(_.plyOffset)) -> List.empty[Statement]
        case _ => Nil
      }
      // Observe the actual reply when a selected capturing departure ends
      // selected support. The support anchor is proof, not the price start.
      val supportDepartures = selected.flatMap {
        case action @ Action(move, Detail.Support(provider, target, opposition, _)) if (move.path eq path) =>
          for
            defender <- original(provider, move.offset + 1).toList
            subject <- original(target, move.offset + 1).toList
            enemy <- opposition.flatMap(original(_, move.offset + 1))
            departure <- captures.filter(event => event.plyOffset > move.offset &&
              event.originalActor.contains(defender))
            loss <- arm.captures.filter(event => event.plyOffset == departure.plyOffset + 1 &&
              event.originalActor.contains(enemy) && event.originalVictim.contains(subject))
            if (move.offset + 1 to departure.plyOffset).forall(arm.covers(_, defender, subject)) &&
              arm.control(departure.plyOffset + 1, defender, subject).contains(false) &&
              (move.offset + 1 to loss.plyOffset).forall(count =>
                arm.at(subject, count).contains(target) && arm.covers(count, enemy, subject))
            after <- arm.at(defender, departure.plyOffset + 1).toList
          yield
            val events = List(departure, loss)
            val receipt: Statement = if arm.identityRoles(events) then arm.pieceExchange(events)
              else MaterialEvents(path, arm.material.events.filter(event => events.exists(_.plyOffset == event.plyOffset)))
            List(departure.plyOffset, loss.plyOffset) -> List[Statement](action,
              ControlState(arm.ref(departure.plyOffset), after, Some(target), target.at, present = false), receipt)
        case _ => Nil
      }
      // An actual supporting piece can answer its ally's capture by taking a
      // different target instead of making the still-legal recapture. Both
      // actual captures are the observed price; the unplayed legal action is
      // evidence only, never an invented continuation or a claim of necessity.
      val supportedCounterReplies = captures.flatMap { use =>
        for
          provider <- use.originalActor.toList
          loss <- arm.captures.filter(_.plyOffset == use.plyOffset - 1)
          target <- loss.originalVictim.toList if target != provider && target.side == provider.side
          capturer <- loss.originalActor.toList if use.originalVictim.exists(_ != capturer)
          if arm.control(loss.plyOffset, provider, target).contains(true)
          after <- arm.position(use.plyOffset).toList
          actor <- arm.at(provider, use.plyOffset).toList
          enemy <- arm.at(capturer, use.plyOffset).toList
          recapture <- after.view.legalRows.toList.flatten.find(row =>
            row.actor == actor && row.captures(enemy)).toList
        yield
          arm.proofs ++= after.sourceRefs(List(recapture.sourceId))
          List(loss.plyOffset, use.plyOffset) -> List.empty[Statement]
      }
      // The existing economic link can also close an actual other-square
      // reply without a still-legal recapture. Keep its exact response episode:
      // an immediate countercapture, or an attack, escape and countercapture.
      // The latter needs the same capturer/chaser throughout, not a later gain
      // elsewhere in the line. Newly added prices are never seeds here.
      val observedCounterReplies = arm.reading.otherTargetGains.flatMap { link =>
        for
          loss <- captures.find(_.id == link.lossEventId).toList
          // A mandatory loss of the used provider closes that use. It is
          // still a price, even when initial closure already selected it.
          if !stationarySupportLosses.exists(_._2 == loss)
          gain <- arm.captures.find(_.id == link.gainEventId).toList
          if !uses(gain.plyOffset) && !stationarySupportLosses.exists(_._2 == gain)
          if !scoped || answer.comparison.exists(scope =>
            gain.originalActor.exists(scope.subjects) || gain.originalVictim.exists(scope.subjects))
          subject <- loss.originalActor.toList
          attacker <- gain.originalActor.toList if attacker.side != subject.side &&
            !gain.originalVictim.contains(subject)
          if gain.plyOffset == loss.plyOffset + 1 ||
            gain.plyOffset == loss.plyOffset + 3 &&
              arm.mover(loss.plyOffset + 1).contains(attacker) &&
              arm.mover(loss.plyOffset + 2).contains(subject) &&
              arm.covers(loss.plyOffset + 2, attacker, subject) &&
              arm.control(loss.plyOffset + 3, attacker, subject).contains(false)
        yield
          val events = List(loss, gain)
          val receipt: Statement = if arm.identityRoles(events) then arm.pieceExchange(events)
            else MaterialEvents(path, arm.material.events.filter(row => events.exists(_.plyOffset == row.plyOffset)))
          val offsets = (loss.plyOffset to gain.plyOffset).toList
          val quiet = offsets.map(arm.ref).filterNot(ref => receipt.references.contains(ref) ||
            selected.exists(_.references.contains(ref)))
          offsets -> (receipt :: Option.when(quiet.nonEmpty)(Continuation(quiet)).toList)
      }
      val directRelations = (clearances ++ exposures ++ initialReplies ++ supportDepartures ++
        supportedCounterReplies ++ observedCounterReplies).distinct
      val directReplyMaterial = directRelations.flatMap(_._2).flatMap {
        case value: PieceExchange => value.events.map(_.plyOffset)
        case value: MaterialEvents => value.events.map(_.plyOffset)
        case _ => Nil
      }
      val recoveries = captures.flatMap(arm.continuousRecovery)
      val ownedMaterial = uses ++ namedPrices.map(_.plyOffset) ++ recoveries.map(_.plyOffset) ++ directReplyMaterial
      val removals = captures.flatMap(event => arm.continuousRecovery(event)
        .filterNot(removal => uses(removal.plyOffset))
        // Only original content, its directly certified relation reply, or
        // an actual check answer can own intervening material. Neither the
        // generated price interval nor a recovery of a recovery is a seed.
        .filter(removal => arm.material.events.filter(row => row.plyOffset > event.plyOffset &&
          row.plyOffset < removal.plyOffset).forall(row => ownedMaterial(row.plyOffset) ||
            (arm.checks(row.plyOffset - 1) && arm.checkEvents(List(row.plyOffset - 1)).exists {
              case value: CheckReply => value.move == arm.ref(row.plyOffset)
              case _ => false
            }))).map { removal =>
          List(event.plyOffset, removal.plyOffset) -> List.empty[Statement]
        })
      val relations = (removals ++ directRelations).distinct
      val identityPrices = relations.flatMap(_._2).collect { case value: MaterialEvents => value }.toSet
      def context(offsets: List[Int]): List[Int] =
        val checks = if scoped then offsets.filter(arm.checks) else (offsets.min to offsets.max).filter(arm.checks).toList
        (offsets ++ checks ++ checks.filterNot(ownedEnding).map(_ + 1)).distinct.sorted
      // A connected reply already stated by physical identity needs no second
      // material owner. Its condition receipts still join the selected core.
      def priced(offsets: List[Int]): Boolean = scoped && interval(offsets).nonEmpty &&
        interval(offsets).forall(event => uses(event.plyOffset))
      var pending = previous.map { (events, offsets) => (events, offsets, false) } ++
        accounts.filterNot((events, _, _) => priced(events)).map { (events, offsets, _) => (interval(events), offsets, true) } ++
        relations.filterNot((offsets, _) => priced(offsets)).map { (offsets, _) => (interval(offsets), context(offsets), true) }
      var core = relations.foldLeft(current.core) { case (sections, (offsets, conditions)) =>
        val fresh = conditions.filterNot(sections.flatMap(_.statements).contains).flatMap {
          case value: PieceExchange if scoped && (value.path eq path) =>
            val missing = value.events.filterNot(event => uses(event.plyOffset))
            Option.when(missing.nonEmpty)(value.copy(events = missing,
              recaptureOffsets = value.recaptureOffsets.intersect(missing.map(_.plyOffset).toSet))).toList
          case statement if scoped && statement.covered.nonEmpty && statement.covered.forall(ref =>
              (ref.path eq path) && uses(ref.offset)) => Nil
          case statement => List(statement)
        }
        val owner = sections.indexWhere(_.statements.exists(conditions.contains)) match
          case -1 => sections.indexWhere(section => section.covered.exists(ref =>
            (ref.path eq path) && offsets.contains(ref.offset)))
          case index => index
        if fresh.isEmpty || owner < 0 then sections else
          val section = sections(owner)
          sections.updated(owner, section.copy(statements = section.statements ++ fresh,
            required = (section.required ++ fresh.flatMap(_.references)).distinct))
      }
      while pending.nonEmpty do
        var (events, offsets, changed) = pending.head
        pending = pending.tail
        var overlap = pending.filter(price => price._1.exists(events.contains))
        while overlap.nonEmpty do
          val joined = (events ++ overlap.flatMap(_._1)).distinct.sortBy(_.plyOffset)
          offsets = (offsets ++ overlap.flatMap(_._2)).distinct.sorted
          changed ||= overlap.exists(_._3)
          // An added role-change witness stays with the victim, but its earlier
          // date cannot make unrelated intervening trades this answer's price.
          events = if changed then (interval(joined.filterNot(identityPromotions).map(_.plyOffset)) ++
            joined.filter(identityPromotions)).distinct.sortBy(_.plyOffset) else joined
          pending = pending.filterNot(overlap.contains)
          overlap = pending.filter(price => price._1.exists(events.contains))
        if changed then
          if events.isEmpty || !offsets.forall(n => n >= 0 && n < path.moves.size) then return None
          offsets = (offsets ++ events.map(_.plyOffset) :+ 0).distinct.sorted
          val cost = CommentaryMaterialAccount.calculate(arm.material, events.map(_.plyOffset), offsets, path.moves.size)
          // Sparse connected prices are physical transactions, never a claim
          // that no other material changed between their dates. Keep the
          // interval-account guard and give only new captures an owner here.
          val priceEvents = if scoped then events.filterNot(event => uses(event.plyOffset)) else events
          val physicalEvents = arm.captures.filter(event => priceEvents.exists(_.plyOffset == event.plyOffset))
          val physicalPrice = Option.when(scoped && priceEvents.nonEmpty && physicalEvents.size == priceEvents.size &&
            priceEvents.forall(event => event.capture.nonEmpty && event.promotionGainCp == 0) &&
            arm.identityRoles(physicalEvents))(arm.pieceExchange(physicalEvents))
          if cost.isEmpty && physicalPrice.isEmpty && !events.exists(_.promotionGainCp != 0) then return None
          // Keep an already selected identity-bearing reply when it covers
          // this entire ordinary price. Additional prices cannot be hidden by
          // a smaller reply receipt, and promotion still needs its role change.
          val receipt = Option.when(events.forall(event => event.capture.nonEmpty && event.promotionGainCp == 0))(
            core.flatMap(_.statements).collectFirst[Statement] {
              case value: PieceExchange if (value.path eq path) &&
                arm.identityRoles(value.events) &&
                value.events.map(_.plyOffset).toSet == priceEvents.map(_.plyOffset).toSet => value
              case value: MaterialEvents if (value.path eq path) && value.events == events &&
                relations.exists(_._2.contains(value)) => value
            }).flatten
          val price: Statement = receipt.orElse(physicalPrice).getOrElse(
            // A sparse promotion price cannot retell an already owned role
            // change. Its original promotion remains identity context in core.
            if scoped then MaterialEvents(path, priceEvents)
            else cost.fold[Statement](MaterialEvents(path, events))(Material(path, _)))
          val existingChecks = core.flatMap(_.statements).flatMap {
            case value: CheckSequence => value.events
            case value: CheckEvent => List(value)
            case _ => Nil
          }
          val checking = arm.checkEvents(offsets.filter(arm.checks)).filterNot(existingChecks.contains)
          def overlaps(statement: Statement) = receipt.contains(statement) || stated(statement).exists(_._1.exists(priceEvents.contains))
          val owner = core.indexWhere(_.statements.contains(price)) match
            // A cross-arm root contrast is proof of both choices, not the
            // owner of every sibling price. Prefer the actual selected use.
            case -1 => core.indexWhere(_.covered.exists(ref => (ref.path eq path) &&
                events.exists(_.plyOffset == ref.offset))) match
              case -1 => core.indexWhere(section => section.references.contains(arm.ref(0))) match
                case -1 => core.indexWhere(_.statements.exists(overlaps))
                case index => index
              case index => index
            case index => index
          if owner < 0 then return None
          val movedRequired = mutable.ListBuffer.empty[MoveRef]
          val movedFocus = mutable.ListBuffer.empty[Focus]
          core = core.map { section =>
            val removed = section.statements.flatMap {
              case value: TargetResponse if overlaps(value) => value.material.toList.flatMap(_.references)
              case value: MaterialEvents if identityPrices(value) && !receipt.contains(value) => Nil
              case value: MaterialEvents if value.path eq path =>
                value.events.filter(priceEvents.contains).map(event => arm.ref(event.plyOffset))
              case statement if overlaps(statement) => statement.references
              case _ => Nil
            }.distinct
            val retained = section.statements.flatMap {
              case value: TargetResponse if overlaps(value) => List(value.copy(material = None))
              // A specific actual reply remains meaningful when the whole
              // price also contains another loss or recovery.
              case value: MaterialEvents if identityPrices(value) && !receipt.contains(value) => List(value)
              case value: MaterialEvents if value.path eq path =>
                val rest = value.events.filterNot(priceEvents.contains)
                Option.when(rest.nonEmpty)(value.copy(events = rest)).toList
              case statement if overlaps(statement) => Nil
              case statement => List(statement)
            }
            val stillOwned = retained.flatMap(_.references)
            val moved = removed.filterNot(stillOwned.contains)
            movedRequired ++= section.required.filter(moved.contains)
            movedFocus ++= section.focus.filter(value => moved.contains(value.move))
            section.copy(statements = retained, required = section.required.filterNot(moved.contains),
              focus = section.focus.filterNot(value => moved.contains(value.move)))
          }
          val delegated = (movedRequired.toList ++ movedFocus.map(_.move)).distinct.flatMap { ref =>
            val index = core.indexWhere(_.references.contains(ref))
            Option.when(index >= 0)(ref -> index)
          }
          delegated.foreach { (ref, index) =>
            val retained = core(index)
            core = core.updated(index, retained.copy(
              required = (retained.required ++ Option.when(movedRequired.contains(ref))(ref)).distinct,
              focus = (retained.focus ++ movedFocus.filter(_.move == ref)).distinctBy(_.move)))
          }
          val delegatedRefs = delegated.map(_._1).toSet
          val required = movedRequired.filterNot(delegatedRefs)
          val focus = movedFocus.filterNot(value => delegatedRefs(value.move))
          val ownedReferences = core.flatMap(_.references) ++ price.references ++ checking.flatMap(_.references)
          val localReferences = core(owner).references ++ price.references ++ checking.flatMap(_.references)
          val missing = (offsets.map(arm.ref).filterNot(ownedReferences.contains) ++
            (required.toList ++ focus.map(_.move)).filterNot(localReferences.contains)).distinct
          val statements = Option.when(checking.nonEmpty)(CheckSequence(arm.ref(0), checking)).toList ++
            List(price) ++ Option.when(missing.nonEmpty)(Continuation(missing)).toList
          val section = core(owner)
          core = core.updated(owner, section.copy(statements = section.statements ++ statements,
            required = (section.required ++ required ++ statements.flatMap(_.references)).distinct,
            focus = (section.focus ++ focus).distinctBy(_.move)))
            .filterNot(section => section.statements.isEmpty && section.required.isEmpty && section.focus.isEmpty)
      // A used provider that stays on its recapture square has an exact
      // recorded outcome even if the later attack was newly opened. Preserve
      // that original piece's loss beside the selected use, without claiming
      // a continuous attack or widening an interval through unrelated trades.
      val usedOutcomes = stationarySupportLosses.flatMap { (use, loss) =>
        for
          _ <- Option.when(!core.flatMap(_.covered).contains(arm.ref(loss.plyOffset)))(()).toList
          owner = core.indexWhere(_.statements.exists(statement =>
            statement.covered.contains(arm.ref(use.recapture.plyOffset)) &&
              conditionUses(statement).exists((condition, action) => root(condition) &&
                action == arm.ref(use.recapture.plyOffset))))
          if owner >= 0
        yield (use, loss, owner, arm.checkEvents(List(loss.plyOffset)))
      }
      if usedOutcomes.exists { (_, loss, _, checking) => arm.checks(loss.plyOffset) &&
          !ownedEnding(loss.plyOffset) && !checking.exists {
            case reply: CheckReply => reply.move == arm.ref(loss.plyOffset + 1)
            case _ => false
          } } then return None
      usedOutcomes.foreach { (use, loss, owner, checking) =>
        if !core.flatMap(_.covered).contains(arm.ref(loss.plyOffset)) then
          val section = core(owner)
          val receipt = arm.pieceExchange(List(loss))
          val additions = receipt :: checking.filterNot(core.flatMap(_.statements).contains)
          val at = section.statements.indexWhere(statement => conditionUses(statement).exists((_, action) =>
            action == arm.ref(use.recapture.plyOffset)))
          core = core.updated(owner, section.copy(
            statements = section.statements.patch(at + 1, additions, 0),
            required = (section.required ++ additions.flatMap(_.references)).distinct))
      }
      Some(current.copy(core = core))
    arms.foldLeft(Option(answer))((result, arm) => result.flatMap(complete(arm, _)))

  /** An existing response owns only its exact, continuously attacked physical
    * target and that target's actual escape on the same prepared path. */
  private def targetResponseActions(arm: RootQuestionSummary.Arm,
      response: CommentaryAnswer.TargetResponse): List[CommentaryAnswer.Action] =
    import CommentaryAnswer.Action
    import RootQuestionSummary.MoveRef
    (for
      escape <- response.escape.toList
      if (response.path eq arm.path) && response.start >= 0 && response.start < escape &&
        escape < arm.path.moves.size && response.originalAttacker.side != response.originalTarget.side &&
        arm.mover(escape).contains(response.originalTarget)
      actor <- arm.at(response.originalAttacker, response.start + 1).toList
      target <- arm.at(response.originalTarget, response.start + 1).toList if target == response.target
      attacker <- arm.at(response.originalAttacker, escape).toList
      before <- arm.at(response.originalTarget, escape).toList
      after <- arm.at(response.originalTarget, escape + 1).toList if before != after
      if (response.start + 1 to escape).forall(arm.control(_, response.originalAttacker,
        response.originalTarget).contains(true)) &&
        arm.control(escape + 1, response.originalAttacker, response.originalTarget).contains(false)
    yield List(Action(MoveRef(arm.path, response.start), Detail.Attack(actor, target, true)),
      Action(MoveRef(arm.path, escape), Detail.Evades(after, attacker)))).flatten

  private[runtime] def selectedConditionSections(arms: List[RootQuestionSummary.Arm],
      answer: CommentaryAnswer.Answer, notes: List[LocalCandidate])
      : List[(RootQuestionSummary.MoveRef, CommentaryAnswer.Section)] =
    import CommentaryAnswer.*
    import RootQuestionSummary.MoveRef
    def uses(statement: Statement): List[MoveRef] = statement match
      case value: PieceExchange => value.references
      case value: MaterialEvents => value.covered
      case value: Material => value.covered
      case value: EnPassantCapture => List(value.capture)
      case value: TargetResponse => value.escape.toList.map(MoveRef(value.path, _)) ++
        value.targetCapture.toList.flatMap(_.references) ++ value.attackerRemoval.toList.flatMap(_.references) ++
        value.material.toList.flatMap(_.covered)
      case value: TargetOutcome => value.removal.toList.flatMap(_.references) ++ value.departure.toList
      case Action(move, _: Detail.Evades) => List(move)
      case value: PieceRoute => value.references
      case value: RouteUse => List(value.use)
      case value: ClearedRoute => List(value.use)
      case value: SquareReoccupation => List(value.use)
      case value: SupportedCapture => value.arrival.toList :+ MoveRef(value.move.path, value.use.recapture.plyOffset)
      case value: SupportRecapture => List(MoveRef(value.witness, value.use.recapture.plyOffset))
      case value: SupportTransfer => List(MoveRef(value.move.path, value.use.recapture.plyOffset))
      case value: CheckingMove => List(value.move)
      case value: CheckSequence => value.events.collect { case check: CheckingMove => check.move }
      case value => conditionUses(value).map(_._2)
    val selected = answer.core.flatMap(_.statements)
    val describedActivities = selected.flatMap {
      case ReferencePurpose(value) => value.arms.flatMap(_.activity)
      case ActivityUse(value, _) => List(value)
      case FunctionReply(_, branch) => branch.purposeActivity.toList
      case _ => Nil
    }
    val selectedChecks = selected.flatMap {
      case value: CheckSequence => value.events
      case value: CheckEvent => List(value)
      case _ => Nil
    }.toSet
    val used = answer.completionSources.getOrElse(selected).flatMap(uses).distinct
    val comparedPaths = selected.collect { case ReferencePurpose(value) => value.arms.map(_.path) }.flatten.toSet
    val comparedReplies = selected.collect { case choice: FirstReplyChoice if
      comparedPaths.exists(CommentarySequenceText.sameRoot(_, choice.root.path)) =>
        List(choice.preferred, choice.alternative)
    }.flatten.toSet
    val meaningUses = answer.completionSources.getOrElse(selected).filterNot {
      case _: Material | _: MaterialEvents => true
      case _ => false
    }.flatMap(_.actionReferences).toSet
    arms.flatMap { arm =>
      val path = arm.path
      val describedResponses = selected.collect { case response: TargetResponse => response }
        .flatMap(targetResponseActions(arm, _))
      def at(offset: Int) = notes.filter(note => offset >= 0 && offset < path.ids.size &&
        note.edge.evidence.id == path.ids(offset) && note.edge.from == path.positions(offset) &&
        note.edge.to == path.positions(offset + 1))
      def original(piece: Piece, count: Int) = arm.originals.find(arm.at(_, count).contains(piece))
      def legal(offset: Int): Boolean = arm.position(offset).exists { position =>
        position.facts.exists {
          case (id, value: RelationWitnessDetail.LegalMove) if EvidenceRef.sameMove(value.moveUci, path.moves(offset)) =>
            arm.proofs ++= position.sourceRefs(List(id))
            true
          case _ => false
        }
      }
      def section(statements: List[Statement], sources: List[LocalCandidate] = Nil): Section =
        arm.proofs ++= sources.flatMap(_.evidence)
        val described = statements.collect { case action: Action if (action.move.path eq path) &&
          describedResponses.contains(action) => action }
        val expressed = statements.filter(statement => describedActivities.exists(_.expresses(statement)))
        val fresh = statements.filterNot(statement => selected.contains(statement) || described.contains(statement) ||
          expressed.contains(statement)).distinct
        // The response already says this content. Its exact occurrences still
        // receive newly admitted proof/focus metadata in the owning section.
        val references = (fresh.flatMap(_.references) ++ described.flatMap(_.references) ++ expressed.flatMap(_.references)).distinct
        Section(fresh, references, sources.flatMap(note =>
          path.ids.indexOf(note.edge.evidence.id) match
            case offset if offset >= 0 && references.contains(arm.ref(offset)) => Some(Focus(arm.ref(offset), note))
            case _ => None).distinctBy(_.move))
      used.filter(ref => (ref.path eq path) && ref.offset > 0 && ref.offset < path.moves.size).flatMap { use =>
        val offset = use.offset
        val users = for
          user <- arm.mover(offset).toList
          beforeUser <- arm.at(user, offset).toList
          afterUser <- arm.at(user, offset + 1).toList
          if beforeUser != afterUser && legal(offset) && arm.position(offset + 1).exists(_.contains(afterUser))
        yield (user, beforeUser, afterUser)
        users.flatMap { (user, beforeUser, afterUser) =>
          // An actual use that answers check needs that preceding check, even
          // when the use was selected for a different (e.g. blocked-line) role.
          val checkOffsets = List(offset - 1, offset).filter(arm.checks)
          val checking = arm.checkEvents(checkOffsets).filterNot(selectedChecks)
          val checkContext = Option.when(checking.nonEmpty)(
            section(List(CheckSequence(arm.ref(checkOffsets.head), checking)))).toList
          val evasions = at(offset).filter(_.note.detail match
            case Detail.Evades(target, attacker) => target == afterUser && original(attacker, offset).exists(actor =>
              arm.control(offset, actor, user).contains(true) && arm.control(offset + 1, actor, user).contains(false))
            case _ => false)
          val responses = evasions.flatMap { evasion =>
            val Detail.Evades(_, attacker) = evasion.note.detail: @unchecked
            val actor = original(attacker, offset).get
            val incoming = (0 until offset).filter(n =>
              arm.control(n, actor, user).contains(false) &&
                (n + 1 to offset).forall(arm.control(_, actor, user).contains(true))).lastOption
            val attack = incoming.toList.flatMap { n => for
              a <- arm.at(actor, n + 1).toList
              target <- arm.at(user, n + 1).toList
            yield Action(arm.ref(n), Detail.Attack(a, target, true)) }
            val attackNotes = incoming.toList.flatMap(n => at(n).filter(note => attack.exists(_.detail == note.note.detail)))
            // A priced recapture need not retell an unrelated pre-existing
            // attack. A selected use or newly created attack still owns its escape.
            val replyIntroduction = comparedReplies(use) && offset == 1
            Option.when(!comparedPaths(path) && !replyIntroduction ||
              meaningUses(use) && !replyIntroduction || incoming.nonEmpty)(
              section(attack :+ Action(use, evasion.note.detail), evasion :: attackNotes)).toList
          }
          val vacancies = (0 until offset).flatMap { departure =>
            for
              blocker <- arm.mover(departure).toList if blocker != user
              before <- arm.at(blocker, departure).toList if before.at == afterUser.at
              after <- arm.at(blocker, departure + 1).toList if after.at != before.at
              if legal(departure) && (departure + 1 to offset).forall(n =>
                arm.position(n).exists(_.empty(before.at)))
              if !selected.exists {
                case value: SquareReoccupation => (value.use.path eq path) && value.use.offset == offset &&
                  value.move.offset == departure && value.originalEntry == user
                case _ => false
              }
            yield section(List(SquareReoccupation(arm.ref(departure), blocker, after, before.at, use, user, afterUser)))
          }.toList
          val openings = arm.captures.filter(_.plyOffset == offset).flatMap { capture =>
            capture.originalVictim.toList.flatMap { victim =>
              arm.at(victim, offset).toList.flatMap { target =>
                (0 until offset).flatMap { departure =>
                  for
                    blocker <- arm.mover(departure).toList if blocker != user && blocker != victim
                    beforeBlocker <- arm.at(blocker, departure).toList
                    afterBlocker <- arm.at(blocker, departure + 1).toList if beforeBlocker != afterBlocker
                    if legal(departure) && (departure to offset).forall(n =>
                      arm.at(user, n).contains(beforeUser) && arm.at(victim, n).contains(target))
                    before <- arm.position(departure).toList if before.complete(RelationFactKind.RayBarrier)
                    obstruction <- before.blockedRay(beforeUser, target).toList if obstruction._2 == List(beforeBlocker)
                    if arm.control(departure, user, victim).contains(false) &&
                      (departure + 1 to offset).forall(arm.control(_, user, victim).contains(true))
                    opening <- at(departure).filter(_.note.detail match
                      case Detail.ChangedLines(_, opened, _) => opened.contains(beforeUser -> target)
                      case Detail.Attack(actor, subject, true) => actor == beforeUser && subject == target
                      case _ => false).take(1)
                    if !selected.exists {
                      case value: ClearedRoute => (value.use.path eq path) && value.use.offset == offset &&
                        value.move.offset == departure && value.originalUser == user
                      case _ => false
                    }
                  yield
                    arm.proofs ++= before.sourceRefs(List(obstruction._1))
                    section(List(ClearedRoute(arm.ref(departure), beforeBlocker, afterBlocker, user,
                      use, afterUser, List(target), passage = true, development = None)), List(opening))
                }
              }
            }
          }
          val enPassant = for
            event <- arm.material.events.filter(_.plyOffset == offset)
            capture <- event.capture.toList if beforeUser.roleName == "pawn" && capture.capturedRole.name == "pawn" &&
              capture.square != event.movement.to
            actual <- arm.captures.find(_.plyOffset == offset).toList
            pawn <- actual.originalVictim.toList if actual.originalActor.contains(user) && arm.mover(offset - 1).contains(pawn)
            prior <- arm.at(pawn, offset - 1).toList
            advanced <- arm.at(pawn, offset).toList if prior.side != beforeUser.side &&
              prior.roleName == "pawn" && advanced.roleName == "pawn" && advanced.square == capture.square &&
              prior.at.head == advanced.at.head && math.abs(prior.at.last - advanced.at.last) == 2 &&
              prior.at.last == (if prior.side.white then '2' else '7') &&
              advanced.at.last == beforeUser.at.last && event.movement.to.key.head == advanced.at.head
            before <- arm.position(offset).toList if before.empty(event.movement.to.key) && legal(offset - 1)
            if before.facts.exists {
              case (_, move: RelationWitnessDetail.LegalMove) => EvidenceRef.sameMove(move.moveUci, path.moves(offset)) &&
                move.capture.exists(c => c.capturedSquare == advanced.square && c.capturedSide == advanced.side &&
                  c.capturedRole == advanced.role)
              case _ => false
            }
          yield section(List(EnPassantCapture(arm.ref(offset - 1), use, pawn, user, advanced, afterUser.at)))
          (checkContext ++ responses ++ vacancies ++ openings ++ enPassant).map(use -> _)
        }
      }
    }.filter(value => value._2.statements.nonEmpty || value._2.required.nonEmpty).distinct

  /** A selected delayed recovery retains the actual clearing piece's loss.
    * This follows an existing opened line and physical capture link; unrelated
    * events between the two selected captures do not become part of the claim.
    */
  private[runtime] def recoveryClearances(arms: List[RootQuestionSummary.Arm],
      selected: Set[RootQuestionSummary.MoveRef], notes: List[LocalCandidate]): List[CommentaryAnswer.Section] =
    import CommentaryAnswer.*
    import RootQuestionSummary.MoveRef
    arms.flatMap { arm =>
      val path = arm.path
      def owns(offset: Int): Boolean = selected.exists(ref => (ref.path eq path) && ref.offset == offset)
      for
        link <- arm.reading.captureLinks if link.elapsedPlies > 1
        gain <- arm.captures.find(_.id == link.lossEventId).toList if owns(gain.plyOffset)
        recovery <- arm.captures.find(_.id == link.captureEventId).toList if owns(recovery.plyOffset)
        capturer <- gain.originalActor.toList if recovery.originalVictim.contains(capturer)
        recovering <- recovery.originalActor.toList
        actor <- arm.at(recovering, gain.plyOffset + 1).toList
        target <- arm.at(capturer, gain.plyOffset + 1).toList
        if (gain.plyOffset + 1 to recovery.plyOffset).forall(count =>
          arm.at(recovering, count).contains(actor) && arm.at(capturer, count).contains(target))
        offset <- (gain.plyOffset + 1 until recovery.plyOffset).toList
        note <- notes.filter(note => note.edge.evidence.id == path.ids(offset) &&
          note.edge.from == path.positions(offset) && note.edge.to == path.positions(offset + 1) &&
          (note.note.detail match
            case Detail.ChangedLines(_, opened, _) => opened.contains(actor -> target)
            case Detail.Attack(sameActor, sameTarget, true) => sameActor == actor && sameTarget == target
            case _ => false))
        blocker <- arm.mover(offset).toList if blocker.side == recovering.side && blocker != recovering
        beforeBlocker <- arm.at(blocker, offset).toList
        afterBlocker <- arm.at(blocker, offset + 1).toList
        if beforeBlocker != afterBlocker && path.moves(offset).take(2) == beforeBlocker.at &&
          path.moves(offset).slice(2, 4) == afterBlocker.at &&
          !arm.material.events.exists(_.plyOffset == offset)
        before <- arm.position(offset).toList if before.complete(RelationFactKind.RayBarrier)
        (rayId, ray) <- before.rays if RelationColoredPieceWitness(ray.attackerSquare, ray.attackerRole, ray.side) == actor &&
          ray.geometry.squares.exists(_.key == target.at)
        corridor = ray.geometry.squares.map(_.key).takeWhile(_ != target.at).filterNot(_ == actor.at)
        if corridor.flatMap(before.occupant) == List(beforeBlocker) &&
          arm.control(offset, recovering, capturer).contains(false) &&
          (offset + 1 to recovery.plyOffset).forall(count =>
            arm.control(count, recovering, capturer).contains(true) &&
              arm.position(count).exists(position => corridor.forall(position.empty)))
        loss <- arm.captures.find(event => event.plyOffset > offset && event.plyOffset < recovery.plyOffset &&
          event.originalVictim.contains(blocker)).toList
        account = arm.account(loss, blocker) if account.complete && account.dutyCosts.isEmpty
      yield
        arm.proofs ++= note.evidence ++ before.sourceByRef.collect { case (ref, source) if source == rayId => ref }
        val opening = Action(MoveRef(path, offset), Detail.ChangedLines(Nil, List(actor -> target), Nil))
        val exchange = arm.pieceExchange(account.events)
        val required = (MoveRef(path, offset) :: account.offsets.map(MoveRef(path, _))).distinct
        val missing = required.filterNot(ref => opening.references.contains(ref) || exchange.references.contains(ref))
        Section(List(opening, exchange) ++ Option.when(missing.nonEmpty)(Continuation(missing)).toList,
          required, List(Focus(MoveRef(path, offset), note)))
    }.distinct

  private def localCandidates(packet: EvidenceBackedJudgmentPacket, positions: Map[PositionNodeRef, Position],
      observations: Map[String, List[MoveObservation]], replySquares: Map[String, Set[String]]): List[LocalCandidate] =
    packet.assembly.transitions.flatMap { edge =>
      for
        before <- positions.get(edge.from).toList
        after <- positions.get(edge.to).toList
        if packet.evidenceGraph.record(edge.evidence).exists(r => edge.matches(r) && packet.evidenceGraph.proofEligible(r))
        // Exact typed facts are deduplicated here. A display purpose is not fact identity.
        ranked = project(edge, before, after, observations.getOrElse(edge.evidence.id, Nil),
          replySquares.getOrElse(edge.evidence.id, Set.empty)).distinctBy(_.factKey).zipWithIndex
      yield
        val representatives = ranked.distinctBy(_._1.purpose).map(_._2).toSet
        val selected = mutable.ListBuffer.empty[Note]
        ranked.map { (note, index) =>
          // A ray is redundant only if its covering fact actually survived selection.
          val coveredRay = note.detail match
            case Detail.OpenedRay(from, actor, _) =>
              val beyond = before.facts.collect {
                case (id, RelationWitnessDetail.RayBarrier(_, _, _, _, geometry)) if note.beforeSources.contains(id) =>
                  geometry.squares.map(_.key).dropWhile(_ != from).drop(1)
              }.flatten.toSet
              selected.exists(n => n.actor == actor && Set("attack", "rook_connection")(n.kind) && n.squares.exists(beyond))
            case _ => false
          val display =
            if !representatives(index) || coveredRay then CommentarySelection.Display.DisplayRepresentative
            else if selected.size >= 3 then CommentarySelection.Display.DisplayCap
            else CommentarySelection.Display.Selected
          val selectedIndex = Option.when(display == CommentarySelection.Display.Selected)(selected.size)
          if selectedIndex.nonEmpty then selected += note
          LocalCandidate(edge, before, after, note, index, selectedIndex, display)
        }
    }.flatten

  private def project(edge: MoveTransitionEdge, before: Position, after: Position,
      observations: List[MoveObservation], replySquares: Set[String]): List[Note] =
    val legal = before.facts.collectFirst {
      case (id, move: RelationWitnessDetail.LegalMove) if EvidenceRef.sameMove(move.moveUci, edge.moveUci) => id -> move
    }
    // The admitted observation owns castling/promotion destinations. The legal
    // catalogue also supplies a mover when an independent statement is absent.
    val movement = observations.headOption.map(o => (o.mover.from.key, o.mover.to.key)).orElse(
      legal.map((_, move) => edge.moveUci.take(2) -> move.destinationSquare.key))
    (for
      (from, to) <- movement.toList
      previousMover <- before.occupant(from).toList
      mover <- after.occupant(to).filter(_.side == previousMover.side).toList
    yield
      val side = mover.side
      val other = !side
      val candidates = mutable.ListBuffer.empty[Note]
      val changedLines = mutable.ListBuffer.empty[(Piece, Piece, Boolean, List[EvidenceRef])]
      val blockedSquares = mutable.ListBuffer.empty[(Piece, String, String, List[String])]
      def add(kind: String, actor: Piece, detail: Detail, squares: List[String], priority: Int,
          purpose: List[String] = Nil, premises: List[EvidenceRef] = Nil,
          beforeSources: List[String] = Nil, afterSources: List[String] = Nil): Unit =
        candidates += Note(kind, actor, detail, squares, priority,
          if purpose.isEmpty then List(kind, actor.at) else purpose, premises.map(_.id),
          beforeSources ++ Option.when(observations.isEmpty)(legal.toList.map(_._1)).getOrElse(Nil), afterSources)
      observations.foreach { observation =>
        val premises = observation.premises
        observation.meaning match
          case MoveObservationMeaning.Development(squares) =>
            add("development", mover, Detail.Development(previousMover.roleName, from, to, squares.map(_.key)),
              List(from, to) ++ squares.map(_.key), 65, premises = premises)
          case MoveObservationMeaning.Castling(rook) =>
            add("castling", mover, Detail.Castle(rook.from.key, rook.to.key), List(from, to, rook.from.key, rook.to.key), 65, premises = premises)
          case MoveObservationMeaning.Capture(role, square, recapture) =>
            add("capture", mover, Detail.Capture(role.name, square.key, recapture),
              List(from, to, square.key), 80, premises = premises)
          case MoveObservationMeaning.PieceControl(prior, next) =>
            for
              a <- next.controller.filter(after.contains)
              t <- next.target.filter(after.contains)
              oldA <- prior.controller.filter(before.contains)
              oldT <- prior.target.filter(before.contains)
            do
              if prior.state == PieceControlState.Present && next.state == PieceControlState.Absent &&
                  a.side == other && t.side == side && t != oldT then
                add("evades_attack", t, Detail.Evades(t, a), List(oldT.at, t.at, a.at), 78,
                  List("evades_attack", t.at), premises)
              val gained = prior.state == PieceControlState.Absent
              val maintained = prior.state == PieceControlState.Present
              // The moving piece can change two stationary pieces' relationship.
              // Keep blocked lines and opened enemy lines together so the
              // display cap cannot erase the concession made by the same move.
              if a == oldA && t == oldT && t.roleName != "king" &&
                  Set("bishop", "rook", "queen")(a.roleName) &&
                  ((a.side == other && t.side == side && gained && next.state == PieceControlState.Present) ||
                    (maintained && next.state == PieceControlState.Absent)) then
                changedLines += ((a, t, gained, premises))
              if next.state == PieceControlState.Present && t.roleName != "king" && (gained || maintained) then
                if t.side != a.side && (gained || oldA != a || oldT != t) then
                  add("attack", a, Detail.Attack(a, t, gained), List(a.at, t.at),
                    (if a.side == side then 70 else 55) + values(t.roleName), List("attack", t.at), premises)
                else if a.side == side && t.side == side then
                  val opposition = after.controllers(t.at, other)
                  if opposition.nonEmpty || (gained && !(a.roleName == "rook" && t.roleName == "rook")) then
                    add("support", a, Detail.Support(a, t, opposition, gained), List(a.at, t.at) ++ opposition.map(_.at),
                      (if opposition.nonEmpty then 75 else 60) + (if gained then 1 else 0), List("support", t.at), premises,
                      afterSources = opposition.flatMap(p => after.sources(p, List(t.at))))
                  if gained && a.roleName == "rook" && t.roleName == "rook" then
                    add("rook_connection", a, Detail.RookConnection(a, t), List(a.at, t.at), 64, List("rook_connection"), premises)
          case MoveObservationMeaning.CertifiedRelation(RelationWitnessDetail.PawnTopologyTransition(
              _, Some(prior), Some(next), changed, _)) if next.side == side && next.square == mover.square &&
              prior.square == previousMover.square && changed.contains(RelationPawnTopologyFacet.Doubled) =>
            val pawns = after.view.pawnStates.filter(_.square.key.head == to.head).flatMap(pawn =>
              after.occupant(pawn.square.key).filter(_.side == side)).sortBy(_.at)
            if pawns.nonEmpty then add("pawn_structure", mover, Detail.PawnStructure(pawns, next.doubled),
              from :: pawns.map(_.at), 79, premises = premises)
          case _ => ()
      }
      if replySquares.nonEmpty && after.complete(RelationFactKind.GeometricControl) then after.facts.foreach {
        case (id, RelationWitnessDetail.RayBarrier(actorSide, at, role, occupants, geometry)) =>
          val actor = RelationColoredPieceWitness(at, role, actorSide)
          if actorSide == side && before.contains(actor) && after.contains(actor) &&
              occupants.headOption.contains(mover) then
            val ray = geometry.squares.map(_.key)
            ray.dropWhile(_ != to).drop(1).filter(replySquares).foreach { square =>
              if before.empty(square) && after.empty(square) && before.controlledBy(actor)(square) &&
                  !after.controlledBy(actor)(square) then blockedSquares += ((actor, square, id, ray))
            }
        case _ => ()
      }
      if changedLines.nonEmpty || blockedSquares.nonEmpty then
        val closed = changedLines.toList.filterNot(_._3).map { (actor, target, _, _) =>
          val remaining = before.controllers(target.at, actor.side).filter(p => p != actor &&
            after.contains(p) && after.controlledBy(p)(target.at))
          (actor, target, remaining)
        }
        val opened = changedLines.toList.filter(_._3).map((actor, target, _, _) => actor -> target)
        val retainedSources = closed.flatMap((_, target, remaining) => remaining.map(_ -> target.at))
        add("changed_lines", mover, Detail.ChangedLines(closed, opened,
          blockedSquares.toList.map((actor, square, _, ray) => (actor, square, ray))),
          List(from, to) ++ changedLines.toList.flatMap((actor, target, _, _) => List(actor.at, target.at)) ++
            closed.flatMap(_._3.map(_.at)) ++ blockedSquares.toList.flatMap((actor, square, _, _) => List(actor.at, square)), 78,
          premises = changedLines.toList.flatMap(_._4),
          beforeSources = retainedSources.flatMap((actor, square) => before.sources(actor, List(square))) ++
            blockedSquares.toList.flatMap((actor, square, _, _) => before.sources(actor, List(square))),
          afterSources = retainedSources.flatMap((actor, square) => after.sources(actor, List(square))) ++ blockedSquares.toList.map(_._3))
      val controlled = after.controlledBy(mover)
      if previousMover.roleName == "pawn" && mover.roleName == "pawn" then
        val prior = before.view.pawnStates.find(_.square.key == from)
        after.view.pawnStates.find(p => p.square.key == to && p.passed.contains(true)).foreach { pawn =>
          add("passed_pawn", mover, Detail.PassedPawn(to, prior.flatMap(_.passed)), List(from, to), 65,
            beforeSources = prior.toList.filter(_.passed.isDefined).flatMap(_.sourceIds).filter(before.passageSources),
            afterSources = pawn.sourceIds.filter(after.passageSources))
        }
      if before.complete(RelationFactKind.GeometricControl) then
        after.controls.keys.filter(p => p.side == other && p.roleName == "king").foreach { king =>
          val squares = (controlled.intersect(after.controlledBy(king)) -- before.controlledBy(previousMover)).filter(after.empty).toList.sorted
          if squares.nonEmpty then
            add("king_entry", mover, Detail.KingEntry(mover, squares), List(to, king.at) ++ squares, 82,
              afterSources = after.sources(mover, squares) ++ after.sources(king, squares))
        }
      if after.complete(RelationFactKind.PawnAdvanceAffordance) then
        val remaining = after.facts.collect { case (_, detail: RelationWitnessDetail.PawnAdvanceAffordance) => detail }
        before.facts.foreach {
          case (id, advance @ RelationWitnessDetail.PawnAdvanceAffordance(pawnSide, at, destination, traversed)) =>
            val pawn = RelationColoredPieceWitness(at, EvidencePieceRole("pawn"), pawnSide)
            if pawnSide == other && before.contains(pawn) && after.contains(pawn) && traversed.exists(_.key == to) &&
                !remaining.exists(r => r.side == pawnSide && r.pawnSquare == at && r.destinationSquare == destination) then
              add("pawn_block", mover, Detail.PawnBlock(to, pawn, destination.key), List(to, pawn.at, destination.key),
                83, List("pawn_block", pawn.at), beforeSources = List(id))
          case _ => ()
        }
      if after.empty(from) then before.facts.foreach {
        case (id, RelationWitnessDetail.RayBarrier(actorSide, at, role, occupants, geometry)) =>
          val actor = RelationColoredPieceWitness(at, role, actorSide)
          if occupants.headOption.exists(_.square.key == from) && actorSide == side && before.contains(actor) && after.contains(actor) then
            val beyond = geometry.squares.map(_.key).dropWhile(_ != from).drop(1)
            val squares = beyond.filter(sq => after.empty(sq) && after.controlledBy(actor)(sq))
            if squares.nonEmpty then
              add("opened_ray", actor, Detail.OpenedRay(from, actor, squares.last), List(actor.at, from, squares.last), 62,
                beforeSources = List(id), afterSources = after.sources(actor, squares))
        case _ => ()
      }
      if mover.roleName == "rook" then after.view.pawnFiles.find(_.file.key == to.take(1)).foreach { file =>
        if Set("open", s"${if other.white then "white" else "black"}_pawns_only")(file.occupancy) then
          val fileType = if file.occupancy == "open" then "open" else "semi-open"
          val route = controlled.toList.sorted.filter(sq => sq.head == to.head && after.empty(sq) &&
            (sq.last - to.last) * (if side.white then 1 else -1) > 0)
          val supported = route.map(sq => sq -> after.controllers(sq, side).filterNot(_ == mover)).filter(_._2.nonEmpty)
            .sortBy((sq, _) => -math.abs(sq.last - to.last)).headOption
          val squares = List(to) ++ supported.toList.flatMap((sq, helpers) => sq :: helpers.map(_.at))
          val sources = file.sourceIds ++ supported.toList.flatMap((sq, helpers) => (mover :: helpers).flatMap(p => after.sources(p, List(sq))))
          add("rook_file", mover, Detail.RookFile(mover, fileType, supported), squares, if supported.nonEmpty then 63 else 40,
            afterSources = sources)
      }
      // R1 priorities remain a declared fallback policy, not a usefulness estimate.
      // Typed tie keys cannot change when the English wording is edited.
      candidates.toList.map(n => n.copy(premises = n.premises.distinct.sorted,
        beforeSources = n.beforeSources.distinct.sorted, afterSources = n.afterSources.distinct.sorted))
        .sortBy(n => (-n.priority, n.kind, n.actor.at, n.detail.toString))
    ).flatten
