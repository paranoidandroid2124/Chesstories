package io.chesstory.runtime

import scala.collection.mutable
import lila.chessjudgment.analysis.assembly.*
import lila.chessjudgment.model.judgment.*
import play.api.libs.json.*

/** Selects admitted observations/resources, with a scoped exact-capture safety query.
  * Selection retains its premises before the deterministic wording is rendered.
  */
private[runtime] object MoveWorkProjection:
  final case class AttackContext(lineId: String, moves: List[String],
      contacts: List[(CommentaryFact.Piece, CommentaryFact.Piece)], cause: Option[CommentaryFact.Piece] = None)
  final case class Context(text: String, qualification: Option[String] = None,
      attack: Option[AttackContext] = None, tactic: Option[CommentaryProposal.TacticStep] = None,
      tacticScope: Option[(List[String], String, Int)] = None,
      question: Option[CommentaryIdea.QuestionFocus] = None,
      // Original complete wording -> test-only wording after an admitted physical
      // explanation covers its condition. Added qualifications prevent replacement.
      questionTest: Option[(String, String)] = None)
  private val values = Map("pawn" -> 1, "knight" -> 3, "bishop" -> 3, "rook" -> 5, "queen" -> 9, "king" -> 0)
  private[runtime] type Piece = RelationColoredPieceWitness
  extension (p: Piece)
    private def at: String = p.square.key
    private def roleName: String = p.role.name
    private def sideName: String = if p.side.white then "white" else "black"
    private def name: String = s"${p.sideName.capitalize} ${p.roleName} on ${p.at}"
    private def possessiveName: String = s"${p.sideName.capitalize}'s ${p.roleName} on ${p.at}"

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
    def sources(p: Piece, squares: List[String]): List[String] =
      squares.flatMap(controls.getOrElse(p, Map.empty).get)
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

    def text: String = this match
      case Development(role, from, to, squares) => s"Develops the $role from $from to $to" +
        (if squares.isEmpty then "." else s", controlling ${squares.mkString(" and ")}.")
      case Castle(from, to) => s"Castles, bringing the rook from $from to $to."
      case Capture(role, at, recapture) =>
        s"${if recapture.isInstanceOf[LineMaterialRecaptureStatus.Proven] then "Recaptures" else "Captures"} the $role on $at."
      case Evades(target, attacker) => s"Moves the ${target.roleName} out of the attack from ${attacker.possessiveName}."
      case Attack(actor, target, gained) => s"${actor.possessiveName} ${if gained then "now targets" else "keeps targeting"} ${target.possessiveName}."
      case Support(actor, target, opposition, gained) =>
        s"${actor.possessiveName} ${if gained then "adds support to" else "keeps supporting"} ${target.possessiveName}" +
          (if opposition.isEmpty then "." else s", which is under attack from ${opposition.map(_.possessiveName).mkString(", ")}.")
      case RookConnection(actor, target) => s"Connects the rooks on ${actor.at} and ${target.at}."
      case PassedPawn(to, Some(true)) => s"Advances the passed pawn to $to."
      case PassedPawn(to, Some(false)) => s"Creates a passed pawn on $to."
      case PassedPawn(to, None) => s"The pawn on $to is a passed pawn."
      case PawnStructure(pawns, doubled) =>
        if doubled then s"Leaves doubled ${pawns.head.sideName} pawns on ${pawns.map(_.at).mkString(" and ")}."
        else s"The ${pawns.head.sideName} pawn on ${pawns.head.at} is no longer doubled."
      case KingEntry(actor, squares) =>
        s"${actor.possessiveName} now controls ${squares.mkString(", ")} next to the opposing king; the king cannot enter ${if squares.size > 1 then "these squares" else "this square"}."
      case PawnBlock(to, pawn, destination) => s"Occupies $to, blocking ${pawn.possessiveName} from advancing to $destination."
      case OpenedRay(from, actor, end) => s"Clears $from, opening a line for ${actor.possessiveName} toward $end."
      case ChangedLines(closed, opened, blockedSquares) =>
        (distinctClosedLines(closed, blockedSquares).map { (actor, target, remaining) =>
          s"Blocks the line from ${actor.name} to ${target.name}" +
            (if remaining.isEmpty then "." else
              s"; ${remaining.map(_.name).mkString(" and ")} still ${if actor.side == target.side then "support" else "attack"}${if remaining.size == 1 then "s" else ""} that target.")
        } ++ opened.map { (actor, target) => s"Opens the line from ${actor.name} to ${target.name}." } ++
          blockedSquares.map { (actor, square, _) => s"Blocks the line from ${actor.name} toward $square." }).mkString(" ")
      case RookFile(actor, fileType, Some((square, helpers))) =>
        s"Uses the $fileType ${actor.at.head}-file: ${actor.name} controls $square, also supported by ${helpers.map(_.name).mkString(", ")}."
      case RookFile(actor, fileType, None) => s"Places the rook on the $fileType ${actor.at.head}-file."

    def after(move: String, description: String = text): String = this match
      case Attack(_, _, _) | Support(_, _, _, _) | KingEntry(_, _) | PawnStructure(_, false) | PassedPawn(_, None) =>
        s"After $move, ${description.stripPrefix("The ") match
          case rest if description.startsWith("The ") => "the " + rest
          case _ => description}"
      // Several independent clauses can occur here; a move label keeps their subjects explicit.
      case ChangedLines(_, _, _) => s"$move: $description"
      case _ => s"$move ${description.take(1).toLowerCase(java.util.Locale.ROOT)}${description.drop(1)}"

  // A reply's empty square represents the same obstruction to a farther target
  // on this certified ray. Keep the raw relations, but do not count one cost twice.
  private def distinctClosedLines(closed: List[(Piece, Piece, List[Piece])],
      blockedSquares: List[(Piece, String, List[String])]): List[(Piece, Piece, List[Piece])] =
    closed.filterNot((actor, target, _) => blockedSquares.exists((provider, _, ray) =>
      provider == actor && ray.contains(target.at)))

  private[runtime] case class Note(kind: String, actor: Piece, detail: Detail, squares: List[String], priority: Int,
      purpose: List[String], premises: List[String], beforeSources: List[String], afterSources: List[String]):
    // Ranking, representative grouping and wording are not exact fact identity.
    def factKey = (kind, actor, detail, premises, beforeSources, afterSources)

  private[runtime] case class LocalCandidate(edge: MoveTransitionEdge, before: Position, after: Position,
      note: Note, index: Int, selectedIndex: Option[Int], selection: CommentarySelection.Display):
    lazy val id: String = CommentarySelection.stableId("fact", List(edge.evidence.id, note.factKey.toString))

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
    CommentarySelection.fromPacket(packet, local, paths,
      comparisonEffects(packet, local, positions, edges, paths, observations, destinations))

  final case class Projection(highlights: List[JsObject], selection: JsObject)

  def projectFromPacket(packet: EvidenceBackedJudgmentPacket): Projection =
    projectFromPacket(packet, ObservedLineProtocol.prepare(packet))

  def projectFromPacket(packet: EvidenceBackedJudgmentPacket, paths: ObservedLineProtocol.PreparedPaths): Projection =
    val selected = select(packet, paths)
    Projection(selected.highlights, selected.json)

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
      withoutSupportIntroduction: Option[String] = None):
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
  private def comparisonEffects(packet: EvidenceBackedJudgmentPacket, local: List[LocalCandidate],
      positions: Map[PositionNodeRef, Position], edges: Map[String, MoveTransitionEdge],
      paths: ObservedLineProtocol.PreparedPaths, observations: Map[String, List[MoveObservation]],
      destinations: Map[String, String]): (List[(JsObject, List[Context])], Set[JsObject]) => List[JsObject] =
    def sans(path: ObservedLineProtocol.PreparedPath): List[String] = paths.notation.line(path.moves)
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
      val wanted = sources.toSet
      position.sourceByRef.iterator.collect { case (ref, source) if wanted(source) => ref }.toList.sorted
    def noteEvidence(note: LocalCandidate): List[String] =
      note.note.premises ++ sourceRefs(note.before, note.note.beforeSources) ++
        sourceRefs(note.after, note.note.afterSources) :+ note.edge.evidence.id
    // Current support/evasion is already read locally. Only an actual use in
    // this same certified arm extends it; a plan name or another arm cannot.
    lazy val continuations = paths.continuationReadings.map(reading => reading.path.line.ref.id -> reading).toMap
    def checkSteps(path: ObservedLineProtocol.PreparedPath, offsets: List[Int], initialResponse: Option[Int])
        : List[(Int, String, List[String], chess.Color)] =
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
      val checkSteps = checks.map { (offset, observation, detail) =>
        val king = s"${if detail.checkedSide.white then "White" else "Black"} king on ${detail.kingSquare.key}"
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
        val action = if openedCheckers.nonEmpty then
          s"opens ${openedCheckers.map(_.name).mkString(" and ")}'s line, giving check to the $king" +
            (if checkers.toSet == openedCheckers.toSet then "" else
              s"; ${checkers.filterNot(openedCheckers.contains).map(_.name).mkString(" and ")} also gives check")
        else s"gives check to the $king with ${checkers.map(_.name).mkString(" and ")}"
        (offset, action, (observation :: opened).flatMap(_.premises.map(_.id)), detail.checkedSide)
      }
      val responseSteps = responses.map { (offset, observation, detail) =>
        val resource = detail.response.resource
        val modes = detail.response.modes
        val actions = List(
          Option.when(modes.contains(RelationCheckResponseMode.KingMove))(
            s"moves the king to ${resource.movement.to.key}"),
          Option.when(modes.contains(RelationCheckResponseMode.CaptureChecker))(resource.capture.map(c =>
            s"captures the checking ${c.capturedRole.name} on ${c.capturedSquare.key}")).flatten,
          Option.when(modes.contains(RelationCheckResponseMode.Interpose))(
            s"blocks the check on ${resource.movement.to.key}")
        ).flatten
        (offset, actions.mkString(" and "), observation.premises.map(_.id), detail.respondingSide)
      }
      (responseSteps ++ checkSteps).sortBy(_._1)
    val currentEffectReadings = mutable.Map.empty[(String, Int), List[ComparisonEffect]]
    def currentEffects(path: ObservedLineProtocol.PreparedPath, index: Int): List[ComparisonEffect] =
      currentEffectReadings.getOrElseUpdate(path.line.ref.id -> index, {
        if path.certified.isEmpty || path.ids != path.declared || path.moves.size <= index then Nil
        else
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
          def effect(text: String, actor: Piece, target: Option[Piece], square: String, through: Int,
              offsets: List[Int], evidence: List[String]): ComparisonEffect =
            ComparisonEffect(text, "line_history", List(path.line.ref.id -> through), path.moves.take(index),
              path.positions(index + 1), actor, target, square, offsets.map(path.ids),
              (evidence ++ path.certified.map(_._1.ref.id)).distinct)
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
          def functionEffect(scope: String, text: String, actor: Piece, target: Piece, square: String,
              through: Int, sources: List[String]): ComparisonEffect =
            effect(text, actor, Some(target), square, through, (index until through).toList,
              sources ++ movement(index).toList.flatMap(_._3) ++ movement(through - 1).toList.flatMap(_._3) ++
                (index to through).flatMap(count => position(count).map(_.view.occurrence.id)) ++
                path.ids.slice(index, through)).copy(scope = scope,
                  use = movement(through - 1).map((_, user, _) => FunctionalUse(through - 1, user)))
          def includeSupport(value: ComparisonEffect, helper: Piece, target: Piece): ComparisonEffect =
            val covered = notes(index).filter(_.note.detail match
              case Detail.Support(actor, victim, _, _) => actor == helper && victim == target
              case _ => false)
            value.copy(text = (covered.map(note => s"After ${sans(path)(index)}, ${note.note.detail.text}") :+
              value.text).mkString(" "), evidence = (value.evidence ++ covered.flatMap(noteEvidence)).distinct,
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
            val text = s"In the provided ${sans(path)(index)} line, ${helper.name} maintains support of ${target.name} " +
              s"after ${sans(path)(departure - 1)} ends the control of that target's square by the ${old.sideName} " +
              s"${old.roleName} originally on ${old.at}; following ${sans(path)(loss.plyOffset)}, " +
              s"${sans(path)(recovery.plyOffset)} actually recaptures the piece that took that target."
            includeSupport(functionEffect("support_handoff", text, mover, target, target.at, recovery.plyOffset + 1,
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
            val start = if supportedFromStart then
              (if notes(index).exists(_.note.detail match
                case Detail.Support(helper, victim, _, _) => helper == mover && victim == target
                case _ => false) then "" else s"${sans(path)(index)} adds support from ${mover.name} to ${target.name}; ") +
                (if target == use.target then "that support is maintained" else
                  s"the supported ${target.roleName} from ${target.at} " +
                    arrivalCapture.fold(s"reaches ${use.target.at}")(_.note.detail.text.stripSuffix(".").toLowerCase) + " with " +
                    s"${sans(path)(arrival.get - 1)} while that support is maintained")
            else s"${sans(path)(index)} gives ${mover.name} control of the empty $square square; " +
              s"${sans(path)(arrivalCount - 1)} later brings ${use.target.name} there under that support"
            val text = s"In the provided line, $start. After ${sans(path)(loss.plyOffset)} captures the supported " +
              s"${use.target.roleName} on ${use.target.at}, ${sans(path)(recovery.plyOffset)} by the supporting ${mover.roleName} on ${mover.at} " +
              "actually recaptures its capturer."
            includeSupport(functionEffect("support_use", text, mover, target, square, recovery.plyOffset + 1,
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
            val identity = if nowTarget == target then s"that same ${target.roleName} on ${target.at}"
              else s"the same ${target.roleName} from ${target.at}, now on ${nowTarget.at}"
            val text = s"In the provided ${sans(path)(index)} line, ${sans(path)(departure - 1)} ends the support " +
              s"of $identity by the ${old.sideName} ${old.roleName} originally on ${old.at}; " +
              s"the ${mover.roleName} from ${mover.at} keeps covering it through that move."
            includeSupport(functionEffect("support_retention", text, mover, target, target.at, departure, sources), mover, target)
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
            val text = s"${sans(path)(index)} keeps the line from ${slider.name} toward ${entry.at} blocked: " +
              s"after ${sans(path)(departure - 1)}, ${mover.name} remains in that line as its sole blocker. " +
              s"In the same provided line, ${sans(path)(offset)} brings ${entry.name} there while that line is still blocked."
            functionEffect("blocker_handoff", text, mover, slider, entry.at, offset + 1,
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
              .map((id, squares) => (beforeUse, List(id), squares, s"passes through ${prior.at}", Option.empty[Piece])).toList ++
              afterUse.controlledBy(user).toList.sorted.flatMap(end => for
                target <- afterUse.occupant(end).toList
                if target.side != user.side || notes(offset).exists(_.note.detail match
                  case Detail.Support(actor, supported, opposition, true) =>
                    actor == user && supported == target && opposition.nonEmpty
                  case _ => false)
                (id, squares) <- ray(afterUse, user, end).filter(_._2.contains(prior.at)).toList
              yield (afterUse, id :: afterUse.sources(user, List(end)), squares,
                s"uses the line through ${prior.at} toward ${target.name}", Some(target)))
            continuous = uses.filter { (_, _, corridor, _, _) =>
              (index + 1 to offset + 1).forall(count => position(count).exists(p =>
                p.empty(prior.at) && corridor.forall(square => p.occupant(square).forall(piece =>
                  tracked(userIdentity, count).contains(piece)))))
            }
            fresh = continuous.filter { (_, _, _, _, target) =>
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
            if used.nonEmpty
          yield
            val development = notes(index).collectFirst {
              case note if note.note.detail.isInstanceOf[Detail.Development] =>
                s"${sans(path)(index)} develops the ${mover.roleName} to ${mover.at}"
            }.orElse(Option.when(Set("bishop", "knight")(mover.roleName) &&
              Set('1', '8')(prior.at.last) && mover.at.last != prior.at.last)(
              s"${sans(path)(index)} brings the ${mover.roleName} to ${mover.at}"))
            val opening = development.getOrElse(s"${sans(path)(index)} clears ${prior.at}")
            val passage = used.exists(_._5.isEmpty)
            val action = if passage then
              s"passes through ${if development.nonEmpty then s"the vacated ${prior.at} square" else "that square"}"
            else s"uses the cleared line to target ${used.flatMap(_._5).distinct.map(_.possessiveName).mkString(" and ")}"
            val text = s"$opening; later ${sans(path)(offset)} $action in this line."
            functionEffect("clearance_use", text, mover, anchoredUser, prior.at, offset + 1,
              used.flatMap(use => sourceRefs(use._1, use._2)) ++ actions.flatMap(noteEvidence)).copy(actions = actions)
          // The mover's first subsequent move has an observed role. This is a
          // route in this line, not a claim that the route was forced or its original purpose.
          val routes = for
            (_, mover, _) <- currentMove.toList
            rootMover <- original(mover).toList
            offset <- (index + 1 until path.moves.size).find(at =>
              movement(at).exists((_, next, _) => tracked(rootMover, at + 1).contains(next))).toList
            (_, user, _) <- movement(offset).toList
            if user.role == mover.role && (index + 1 to offset).forall(count => tracked(rootMover, count).contains(mover))
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
            functionEffect("piece_route",
              s"In the provided ${sans(path)(index)} line, the same ${mover.roleName} next moves with ${sans(path)(offset)} to ${user.at}.",
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
            path.certified.flatMap(_._2.material).flatMap(CommentarySelection.openingExchange(_, sans(path)))
              .toList.flatMap(_._1.map(_.plyOffset)) else Nil)).filter(_ < path.moves.size).distinct.sorted
          val checkOffsets = checkSeeds.flatMap(start =>
            Iterator.iterate(start)(_ + 2).takeWhile(givesCheck).toList).distinct.sorted
          val checkStepsInOrder = checkSteps(path, checkOffsets, Some(index))
          val checkSequence = for
            (_, actor, movementEvidence) <- currentMove.toList
            first <- checkStepsInOrder.headOption.toList
            king <- identities.collectFirst {
              case (_, Some(Some(p))) if p.side == first._4 && p.roleName == "king" => p
            }.toList
          yield
            val offsets = (index :: checkStepsInOrder.map(_._1)).distinct.sorted
            val text = s"In the provided ${sans(path)(index)} line: " + checkStepsInOrder.groupBy(_._1)
              .toList.sortBy(_._1).map { (offset, steps) =>
                s"${sans(path)(offset)} ${steps.map(_._2).mkString(" and ")}"
              }.mkString("; ") + "."
            effect(text, actor, Some(king), king.at, offsets.last + 1, offsets,
              movementEvidence ++ checkStepsInOrder.flatMap(_._3)).copy(scope = "check_sequence")
          val immediate = notes(index).flatMap { note =>
            note.note.detail match
              case Detail.Support(actor, target, _, gained) if gained ||
                  currentMove.exists((_, mover, _) => mover == actor || mover == target) =>
                List(effect(s"After ${sans(path)(index)}, ${note.note.detail.text}", actor, Some(target), target.at,
                  index + 1, List(index), noteEvidence(note)).copy(actions = List(note)))
              case Detail.Evades(target, attacker) =>
                val later = for
                  originalTarget <- original(target)
                  originalAttacker <- original(attacker)
                  used <- (index + 1 until path.moves.size).iterator.flatMap { offset =>
                    notes(offset).collectFirst {
                      case next if (next.note.detail match
                        case Detail.Attack(actor, victim, _) =>
                          tracked(originalTarget, offset + 1).contains(actor) &&
                            tracked(originalAttacker, offset + 1).contains(victim)
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
                val text = note.note.detail.after(sans(path)(index)) + unintroduced.fold("") { (offset, use, route) =>
                  val action = if route.nonEmpty then
                    s"that ${target.roleName} continues ${route.map(row => sans(path)(row._1)).mkString(", ")}; "
                    else s"after ${sans(path)(offset)}, "
                  s" In the same provided line, $action${use.note.detail.text}"
                }
                List(effect(text, target, Some(attacker), attacker.at, later.fold(index + 1)(_._1 + 1),
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
            val text = s"${sans(path)(index)} clears ${prior.at}; " +
              s"${sans(path)(count - 1)} later occupies that square in this line."
            val emptyPositions = (index + 1 until count).flatMap(at => positions.get(path.positions(at)).map(_.view.occurrence.id))
            effect(text, actor, Some(initialEntry), prior.at, count, List(index, count - 1),
              firstEvidence ++ entryEvidence ++ emptyPositions).copy(use = Some(FunctionalUse(count - 1, entry)))
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
          def effect(scope: String, text: String, arms: List[(ObservedLineProtocol.PreparedPath, Int)],
              anchor: Int, actor: Piece, target: Option[Piece], square: String, sources: List[String],
              absorbed: Set[String] = Set.empty): ComparisonEffect =
            ComparisonEffect(text, scope, arms.map((p, n) => p.line.ref.id -> n), pair.head.moves.take(index),
              arms.head._1.positions(anchor), actor, target, square,
              arms.flatMap((p, n) => p.ids.slice(index, n)),
              arms.flatMap((p, n) => evidence(p, n)) ++ sources, absorbedNotes = absorbed)
          val comparisons = List(pair.head -> pair.last, pair.last -> pair.head).flatMap { (path, peer) =>
            val before = pos(path, index)
            val after = pos(path, index + 1)
            val peerAfter = pos(peer, index + 1)
            val move = sans(path)(index)
            val otherMove = sans(peer)(index)
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
                  paths.pieceAfter(packet, path, root, index).flatten.map(_ -> t))))).headOption
            yield
              val usage = use.fold("") { offset =>
                val support = supported.fold("") { (initial, current) =>
                  val route = (index until offset).filter(n => track(path, initial, index, n)
                    .exists(p => path.moves(n).take(2) == p.at)).map(sans(path)(_)).mkString("–")
                  s", supporting the same pawn from ${initial.at}, now on ${current.at}" +
                    (if route.isEmpty then "" else s" after $route")
                }
                s" In the provided $move line, ${sans(path)(offset)} uses that two-square advance$support."
              }
              effect("pawn_option", s"$move keeps the ${pawn.at}-pawn on its starting square with the route to $square clear; " +
                s"$otherMove commits that same pawn to ${peerPawn.at}, from where a one-square advance to $square also has a clear route." + usage,
                List(path -> through, peer -> (index + 1)), index, pawn, supported.map(_._1), square,
                sourceRefs(after, List(kept)) ++ sourceRefs(peerAfter, List(remaining)) ++ supported.toList.flatMap { (_, target) =>
                  used.toList.flatMap(p => sourceRefs(pos(path, through), pos(path, through).sources(p, List(target.at))))
                }))
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
            yield effect("capture_choice",
              s"$move captures the ${target.roleName} on $at with the ${capturer.roleName}, keeping the pawn on ${actor.at} to support it on $at" +
                (if opposition.isEmpty then ". " else s", under attack from ${opposition.map(_.name).mkString(" and ")}. ") +
                s"$otherMove captures that same ${target.roleName} with the ${actor.at}-pawn instead.",
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
              val trade = capture.map(n => s"$otherMove: ${n.note.detail.text}").mkString(" ")
              val opened = notes(peer, index + 1).flatMap(_.note.detail match
                case Detail.ChangedLines(_, opened, _) => opened
                case _ => Nil).toSet
              val response = attacks.map(n => n.note.detail match
                case Detail.Attack(a, t, _) if opened(a -> t) =>
                  s"Leaving ${pawn.at} opens ${a.name}'s attack on ${t.name}."
                case other => other.text).distinct.mkString(" ")
              effect("advance_choice", s"$move occupies $square, stopping the ${pawn.at}-pawn's advance there. " +
                s"$trade After $otherMove, that same pawn advances with ${sans(peer)(index + 1)}. $response".trim,
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
              if after.complete(RelationFactKind.RayBarrier)
              (id, ray) <- after.rays
              if actor == RelationColoredPieceWitness(ray.attackerSquare, ray.attackerRole, ray.side)
              front <- ray.occupants.headOption.toList
              rear <- ray.occupants.drop(1).headOption.toList
              if front.side == rear.side && front.side != actor.side && front.role.name != "king" && rear.role.name != "king" &&
                values(rear.role.name) > values(front.role.name)
              target = RelationColoredPieceWitness(front.square, front.role, front.side)
              back = RelationColoredPieceWitness(rear.square, rear.role, rear.side)
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
                track(path, actor, index + 1, n) != track(path, actor, index + 1, n - 1)).map(n => sans(path)(n - 1))
              val continuation = (if kept.isEmpty then "" else s" The alignment remains after ${kept.mkString(", ")}.") +
                end.fold("")(n => s" ${sans(path)(n - 1)} breaks that alignment in this line.")
              effect("relative_pin", s"${sans(path)(index)} lines up ${actor.name} with ${back.name} through ${target.name}; " +
                s"the ${target.roleName} screens the ${back.roleName}.$continuation",
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
              val response = reply.fold("")(n => s" The provided reply is ${sans(path)(n)}. " +
                replyNotes.map(_.note.detail.text).distinct.mkString(" "))
              val pinner = RelationColoredPieceWitness(ray.attackerSquare, ray.attackerRole, ray.side)
              effect("capture_release", s"${sans(path)(index)} moves the king off ${pinner.name}'s line, unpinning the ${actor.at}-${actor.roleName}. " +
                s"${actor.name} could now capture ${target.name} without exposing its king, but the opponent moves first." + response,
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
        briefDetail: String = ""):
      def eventKey: String = withdrawal.fold(observation.fold("action:" + action)("note:" + _)) {
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
                      val action = kind match
                        case "capture" => s"captures the original attacker ${beforeAttacker.name}."
                        case "block" => s"blocks ${beforeAttacker.name}'s line to the initially attacked ${target.name}, which stays on ${afterTarget.at}."
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
                        sourceRefs(after, remaining.flatMap(piece => after.sources(piece, List(afterTarget.at))))
                      (offset, note, kind, action, detail, evidence,
                        Option.when(kind == "withdraw")((beforeAttacker, currentAttacker, afterTarget)))
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
                        opensAttack = true))
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
                      departure.premises.map(_.id) :+ path.ids(index + 1)),
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
                    captureRole = Option.when(row._3 == "capture")(CaptureRole.Attacker), withdrawal = row._7)) ++
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
                      evidence = (step.evidence ++ noteEvidence(opened)).distinct))
                  }
                  val responseCaptures = evasionCaptures ++ openedReplyCaptures
                  val connectedCaptures = defenderDeparture.filter(_.offset == index + 2).map(_.offset).toSet
                  val exchangeSteps = if captures.isEmpty && connectedCaptures.isEmpty && responseCaptures.isEmpty then Nil else
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
                        val transactions = reading.events.filter(event => anchors(event.plyOffset)).flatMap { anchor =>
                          val chain = (follow(anchor.id, before) ++ follow(anchor.id, after)).distinctBy(_.id)
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
                  val captureChecks = checkSteps(path, checkingCaptures, None).filterNot { (offset, _, _, _) =>
                    transactionSteps.exists(step => step.checkAction && step.offset == offset)
                  }.map { (offset, action, proof, _) =>
                    HistoryStep(offset, action + ".", "", proof :+ path.ids(offset), checkAction = true)
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
                    interventions.map((offset, note, kind, _, _, _, _) =>
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
              def relation(a: Piece, t: Option[Piece], at: String, active: Boolean): String =
                val verb = t.fold("control")(p => if a.side == p.side then "support" else "attack")
                s"${a.name} ${if active then verb + "s" else "does not " + verb} ${t.fold(at)(_.name)}"
              val text = s"After ${label(edge)}, ${relation(actor, target, square, present)}; " +
                s"after ${label(peerEdge)}, ${relation(peerActor, peerTarget, peerSquare, peerPresent)}." +
                (if remaining.isEmpty then "" else s" ${remaining.map(_.name).mkString(" and ")} still " +
                  s"${if actor.side == target.get.side then "support" else "attack"}${if remaining.size == 1 then "s" else ""} that target after ${label(edge)}.") +
                (if target.nonEmpty then "" else s" In the provided ${label(edge)} line, the immediate reply is ${sans(path)(index + 1)}.")
               val evidence = premises ++ sourceRefs(peerAfter, peerAfter.sources(peerActor, List(peerSquare))) ++
                List(edge.evidence.id, peerEdge.evidence.id, note.before.view.occurrence.id,
                  note.after.view.occurrence.id, peerAfter.view.occurrence.id) ++
                Option.when(target.isEmpty)(path.ids(index + 1)).toList
              val effect = ComparisonEffect(text, "pair_difference",
                List(path.line.ref.id -> (index + (if target.isEmpty then 2 else 1)), peer.line.ref.id -> (index + 1)),
                prefix, edge.from, actor, target, square,
                List(edge.evidence.id, peerEdge.evidence.id) ++ Option.when(target.isEmpty)(path.ids(index + 1)).toList,
                evidence.distinct, geometricCost = present != (actor.side == note.note.actor.side))
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
      val convergence = ownedPair match
        case List(left, right) if (detail \ "type").as[String] == "root_comparison" &&
            List(left, right).forall(path => path.certified.nonEmpty && path.ids == path.declared) &&
            left.moves.lift(index) != right.moves.lift(index) =>
          left.positions.iterator.zip(right.positions.iterator).zipWithIndex.drop(index + 1)
            .find { case ((l, r), _) => l.fen == r.fen }.map { case (_, count) =>
              val shared = left.moves.drop(count).zip(right.moves.drop(count)).takeWhile((l, r) => l == r).map(_._1)
              val next = count + shared.size
              val continuation = Option.when(shared.nonEmpty)(s" Both then play ${sans(left).slice(count, next).mkString(" ")}.").getOrElse("")
              val split = (left.moves.lift(next), right.moves.lift(next)) match
                case (Some(l), Some(r)) => s" From that common board state, the ${sans(left)(index)} line continues with ${sans(left)(next)}, " +
                  s"while the ${sans(right)(index)} line continues with ${sans(right)(next)}."
                case _ => ""
              s"In the provided lines, ${sans(left).slice(index, count).mkString(" ")} and " +
                s"${sans(right).slice(index, count).mkString(" ")} reach the same board state with the same side to move." +
                continuation + split
            }.filter(text => text.length + originalText.length + 1 <= limit).toList
        case _ => Nil
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
                after.view.occurrence.id, peerAfter.view.occurrence.id)).distinct, geometricCost = true) -> actor
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
            tactic.at.throughPly == tactic.line.size &&
            context.text == tactic.text(paths.notation, context.tacticScope, includeQualification = false)
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
        context.filter(_.text.nonEmpty).map { part =>
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
            checkSteps(history.path, List(offset), Some(offset)).filter(_._1 == offset)
            else Nil
          // Several histories may describe the same check differently. Share its
          // typed response only with already-owned proof and a separate capture
          // clause; an embedded capture must not disappear with the old wording.
          val compact = if canonical.nonEmpty &&
              capture.forall(note => rows.exists(s => s.observation.contains(note.id) && s.captureRole.nonEmpty)) &&
              canonical.flatMap(_._3).toSet.subsetOf(rows.flatMap(_.evidence).toSet) then
            rows.filterNot(_.checkAction) ++ canonical.map { (ply, action, proof, _) =>
              HistoryStep(ply, action + ".", "", proof, checkAction = true)
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
          val target = if !joined then "That original target" else
            s"The ${h.originalTarget.sideName} ${h.originalTarget.roleName} originally on ${h.originalTarget.at}"
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
        def text =
          val description = note.note.detail match
            case Detail.ChangedLines(_, opened, _) if offset != index =>
              val capture = path.moves(offset + 1)
              Detail.ChangedLines(Nil, opened.filter((actor, target) =>
                actor.at == capture.take(2) && target.at == capture.drop(2).take(2)), Nil).text
            case Detail.PawnBlock(to, pawn, _) =>
              val destinations = notes.collect { case value if value.note.detail.isInstanceOf[Detail.PawnBlock] =>
                val Detail.PawnBlock(_, _, destination) = value.note.detail: @unchecked
                destination
              }.distinct
              s"Occupies $to, blocking the advance of ${pawn.name} to ${destinations.mkString(" or ")}."
            case _ => note.note.detail.text
          if comparedAttack then
            val Detail.Attack(_, target, _) = note.note.detail: @unchecked
            s"Both ${notes.map(n => label(n.edge)).distinct.mkString(" and ")} attack ${target.name}."
          else if offset == index then note.note.detail.after(sans(path)(index), description)
          else
            val actor = note.before.occupant(path.moves(offset).take(2)).get
            s"Later in the provided ${sans(path)(index)} line, ${sans(path)(offset)} " +
              s"(${actor.sideName.capitalize} ${actor.roleName} from ${actor.at}): $description"
      case class Explanation(effects: List[ComparisonEffect], histories: List[AttackHistory] = Nil,
          functions: List[CurrentFunction] = Nil, textParts: List[Context] = Nil, owner: Option[Int] = None)
      case class Paragraph(before: List[String], histories: List[AttackHistory],
          actions: List[(String, LocalCandidate, String)], after: List[String], current: Boolean,
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
          case Detail.Capture(_, _, _) => !openingCaptures.getOrElse(path.line.ref.id, Set.empty)(index)
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
        val wording = text.questionTest.filter((original, _) => original == text.text && physicalCondition)
          .fold(text)((_, test) => text.copy(text = test))
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
      def supportTexts(effects: List[ComparisonEffect]): (Set[ComparisonEffect], List[String]) =
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
              if values.size == 1 then values.head._1.text
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
            }
          }
        rows.map(_._1).toSet -> texts
      // Each existing candidate keeps its mandatory answer and text limit. Shared
      // histories are rendered once, then complete explanations fill those fields.
      case class Rendered(text: String, current: Boolean, effects: List[ComparisonEffect],
          functions: List[CurrentFunction], owner: Option[Int], qualifications: List[String])
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
        def contextText(part: Paragraph): Option[(String, String)] = part.context.flatMap { context =>
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
            context.text -> text
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
                contextText(part).get._2
              })
            }.groupMap(_._1)(_._2)
          // A selected bundle already owns these notes. Place them at their
          // exact occurrence, only inside that bundle's connected history.
          val additions = parts.filter(_.histories.exists(group.contains)).flatMap(_.actions).flatMap { (line, note, text) =>
            val at = path.ids.indexOf(note.edge.evidence.id)
            val key = line -> note.id
            if line == path.line.ref.id && !described(key) && !embeddedActions.contains(key) &&
                group.exists(h => h.start == at || h.steps.exists(_.offset == at)) then
              embeddedActions(key) = owner(group)
              List(at -> text.stripPrefix(s"At ${sans(path)(at)}, "))
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
                step.offset == use.offset && step.withdrawal.exists(_._2 == use.actor)))))).map(_.text).toSet
          def wording(text: String) = if repeatedRoutes(text) ||
              (embeddedContexts(offset) && part.context.exists(_.text == text)) then ""
            else rewritten.filter(_._1 == text).fold(text)(_._2)
          val actions = part.actions.collect {
            case (line, note, text) if !described(line -> note.id) && !embeddedActions.contains(line -> note.id) => text
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
              owned.map(_.effect) ++ dependencies).distinct, part.functions, part.owner, part.qualifications)
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
            values.flatMap(_.owner).headOption, values.flatMap(_.qualifications).distinct)
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
            (line, note, s"At ${label(note.edge)}, ${note.note.detail.text}$identity")
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
          val premiseText = qualifications.sortBy(effect => if supportDifferences.exists(_._1 == effect) then 0 else 1).map { effect =>
            if requests.size == 1 && effect.absorbedNotes.nonEmpty &&
                effect.absorbedNotes.subsetOf(describedSupport) && effect.withoutSupportIntroduction.nonEmpty then
              effect.withoutSupportIntroduction.get
            else if effect.scope != "clearance_use" || seenClearance.add((effect.actor, effect.square)) then effect.text
            else
              val move = sans(ownedPair.find(_.line.ref.id == effect.lines.head._1).get)(index)
              effect.text.stripPrefix(s"$move clears ${effect.square}; ").capitalize
          }.distinct
          val later = functions.filter(_.offset != index)
          val laterText = later.map(_.path.line.ref.id).distinct.map { line =>
            val actions = later.filter(_.path.line.ref.id == line)
            val introduction = s"Later in the provided ${sans(actions.head.path)(index)} line"
            s"$introduction: " + actions.map(_.text.stripPrefix(introduction + ", ")).mkString(" ")
          }
          val (introductions, trailingContext) = bundle.textParts.partition(context => histories.nonEmpty &&
            bundle.owner.exists(owner => ownedTactics(owner -> context)))
          val paragraph = Paragraph(supportLosses ++ premiseText ++ functions.filter(_.offset == index).map(_.text) ++
            introductions.map(_.text), histories, actionsText,
            separateRelations.map(_.text).distinct ++ supportParts ++ laterText ++ trailingContext.map(_.text),
            currentAction(bundle), fresh, functions, bundle.owner, bundle.textParts.flatMap(_.qualification),
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
          val expanded = prior.copy(before = prior.before :+ context.text,
            after = prior.after.filterNot(_ == context.text), histories = fresh,
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
      // Keep the complete explanation as the record. A short reading may reuse
      // selected conditions only together with every capture in its exact pair.
      val overview = Option.when((detail \ "type").as[String] == "root_comparison" &&
        ownedPair.size == 2 && (detail \ "material_relation").asOpt[String].exists(Set("equal", "different"))) {
        val owned = allocated.head.flatMap(_.effects).toSet
        val physical = paragraphs.filter(part => part.context.isEmpty &&
          part.effects.forall(owned) && part.functions.forall(allocated.head.flatMap(_.functions).contains))
        val captureIntroductions = physical.flatMap(_.functions)
          .filter(_.note.note.detail.isInstanceOf[Detail.Capture]).map(_.text).toSet
        val conditions = physical.filter(part =>
          core(Explanation(part.effects, part.histories, part.functions)) ||
            part.effects.exists(effect => effect.geometricCost || effect.lines.size == 2))
          .flatMap(part => part.before ++ part.after).filter(text => text.nonEmpty && !captureIntroductions(text))
          .distinct.mkString(" ")
        val followups = physical.flatMap(_.actions).toList.distinctBy((line, note, _) => line -> note.id)
          .groupBy(_._1).toList.sortBy((line, _) => ownedPair.indexWhere(_.line.ref.id == line)).map { (line, actions) =>
            val path = ownedPair.find(_.line.ref.id == line).get
            val moves = actions.groupBy(_._2.edge.evidence.id).toList.sortBy((id, _) => path.ids.indexOf(id)).map { (_, notes) =>
              val move = label(notes.head._2.edge)
              notes.map { (_, note, description) =>
                note.note.detail.after(move, description.stripPrefix(s"At $move, "))
              }.mkString(" ")
            }
            line -> moves.mkString(" ")
          }.toMap
        val lines = ownedPair.flatMap { path =>
          path.certified.flatMap(_._2.material).toList.flatMap { material =>
            val steps = physical.flatMap(_.histories).filter(_.path.line.ref == path.line.ref).flatMap(_.steps)
            // The short reading owns the entire exchange once, including the opening.
            val captures = material.captures.map { capture =>
              val qualifications = steps.filter(_.offset == capture.plyOffset).map(_.briefDetail).filter(_.nonEmpty).distinct.mkString
              val check = if sans(path)(capture.plyOffset).endsWith("+") then ", giving check"
                else if sans(path)(capture.plyOffset).endsWith("#") then ", giving checkmate" else ""
              capture.plyOffset -> (s"${sans(path)(capture.plyOffset)} ${if capture.recapture then "recaptures" else "captures"} " +
                s"${if capture.side.white then "Black" else "White"}'s ${capture.capturedRole.name} on ${capture.square.key}$check." + qualifications)
            }
            val captureOffsets = material.captures.map(_.plyOffset).toSet
            val checks = steps.filter(step => step.checkAction && !captureOffsets(step.offset)).map(step =>
              step.offset -> s"${sans(path)(step.offset)} ${step.action}${step.detail}").toList
            val promotions = material.events.filter(_.promotionGainCp > 0).map(event => event.plyOffset ->
              s"${sans(path)(event.plyOffset)} promotes the pawn to a ${event.movement.afterRole.name}.")
            val events = (captures ++ checks ++ promotions).distinct.sortBy(_._1).map(_._2)
            val result = CommentarySelection.materialText(CommentarySelection.materialSignature(material))
            List((List(s"${sans(path).drop(index).take(2).mkString(" → ")}.") ++
              followups.get(path.line.ref.id).toList ++ events :+
              s"In this line, $result.").mkString(" "))
          }
        }
        (List(conditions) ++ lines :+
          "These are observed continuations, not a forced outcome or proof of why the evaluations differ.")
          .filter(_.nonEmpty).mkString("\n\n")
      // ponytail: keep every owned capture; use the full explanation when this
      // complete overview is too long instead of cutting a later recovery.
      }.filter(text => text.nonEmpty && text.length <= 2200 &&
        text.length < allocated.zipWithIndex.map((parts, owner) => textFor(owner, parts).length).sum)
        .orElse(Option.when((detail \ "type").as[String] == "root_comparison" && ownedPair.size == 2 &&
          ownedPair.forall(_.certified.flatMap(_._2.material).nonEmpty)) {
          // Keep the entire exchange sequence when a prose overview does not fit.
          // These are chronological observations, not a claim that the replies are forced.
          val lines = ownedPair.map { path =>
            val material = path.certified.flatMap(_._2.material).get
            val moves = sans(path)
            val captures = material.captures.map(capture => capture.plyOffset -> (
              s"${moves(capture.plyOffset)} ${if capture.recapture then "recaptures" else "captures"} " +
                s"a ${if capture.side.white then "black" else "white"} ${capture.capturedRole.name}"))
            val promotions = material.events.filter(_.promotionGainCp > 0).map(event => event.plyOffset ->
              s"${moves(event.plyOffset)} promotes to a ${event.movement.afterRole.name}")
            val exchanges = (captures ++ promotions).sortBy(_._1).map(_._2)
            val result = CommentarySelection.materialText(CommentarySelection.materialSignature(material))
            s"${moves.drop(index).take(2).mkString(" → ")}. " +
              (if exchanges.isEmpty then "No captures or promotions in the recorded continuation."
               else s"${exchanges.mkString("; ")}.") + s" In this line, $result."
          }
          (allocated.head.flatMap(_.functions).filterNot(_.note.note.detail.isInstanceOf[Detail.Capture])
            .headOption.map(_.text).toList ++ lines :+
            "These are observed continuations, not a forced outcome or proof of why the evaluations differ.")
            .mkString("\n\n")
        }.filter(_.length <= 4000))
      requests.zip(allocated).zipWithIndex.map { case (((value, _), parts), owner) =>
        val text = textFor(owner, parts)
        val effects = parts.flatMap(_.effects).distinct
        val functions = parts.flatMap(_.functions).distinct
        val summary = if owner == 0 then overview else overview.map { _ =>
          (List(originals(owner)) ++ parts.flatMap(_.qualifications) ++
            paragraphs.filter(_.owner.contains(owner)).flatMap(_.context).filter(_.tactic.isEmpty).map(_.text))
            .filter(_.nonEmpty).distinct.mkString("\n\n")
        }.filter(value => value.nonEmpty && value.length < text.length)
        val baseValue = value + ("detail" -> ((value \ "detail").as[JsObject] - "summary"))
        if text == originals(owner) && summary.isEmpty then baseValue else
          baseValue ++ Json.obj("evidence_ids" -> ((value \ "evidence_ids").as[List[String]] ++
            (if pv then Nil else effects.flatMap(_.evidence) ++ functions.flatMap(_.notes.flatMap(noteEvidence)))).distinct,
            "detail" -> (((baseValue \ "detail").as[JsObject] - "effects") ++ Json.obj("text" -> text) ++
              summary.fold(Json.obj())(value => Json.obj("summary" -> value)) ++
              Option.when(effects.nonEmpty)(Json.obj("effects" -> effects.map(_.json(publicSource)))).getOrElse(Json.obj())))
      }
    compose

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
