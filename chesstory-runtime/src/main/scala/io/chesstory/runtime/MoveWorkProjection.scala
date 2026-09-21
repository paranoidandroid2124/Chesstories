package io.chesstory.runtime

import scala.collection.mutable
import lila.chessjudgment.analysis.assembly.*
import lila.chessjudgment.model.judgment.*
import play.api.libs.json.*

/** Selects existing typed observations/resources. No board replay or search.
  * Selection retains its premises before the deterministic wording is rendered.
  */
private[runtime] object MoveWorkProjection:
  private val values = Map("pawn" -> 1, "knight" -> 3, "bishop" -> 3, "rook" -> 5, "queen" -> 9, "king" -> 0)
  private[runtime] type Piece = RelationColoredPieceWitness
  extension (p: Piece)
    private def at: String = p.square.key
    private def roleName: String = p.role.name
    private def sideName: String = if p.side.white then "white" else "black"
    private def name: String = s"${p.sideName.capitalize} ${p.roleName} on ${p.at}"

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
    def sources(p: Piece, squares: List[String]): List[String] =
      squares.flatMap(controls.getOrElse(p, Map.empty).get)
    private val controllerLookups = mutable.Map.empty[(String, chess.Color), List[Piece]]
    def controllers(square: String, side: chess.Color): List[Piece] =
      controllerLookups.getOrElseUpdate(square -> side,
        controls.iterator.collect { case (p, targets) if p.side == side && targets.contains(square) => p }.toList.sortBy(_.at))
    def complete(kind: RelationFactKind): Boolean = view.completeKinds(kind)

  private[runtime] enum Detail:
    case Development(role: String, from: String, to: String)
    case Castle(from: String, to: String)
    case Capture(role: String, at: String, recapture: LineMaterialRecaptureStatus)
    case Evades(target: Piece, attacker: Piece)
    case Attack(actor: Piece, target: Piece, gained: Boolean)
    case Support(actor: Piece, target: Piece, opposition: List[Piece], gained: Boolean)
    case RookConnection(actor: Piece, target: Piece)
    case PassedPawn(to: String, before: Option[Boolean])
    case KingEntry(actor: Piece, squares: List[String])
    case PawnBlock(to: String, pawn: Piece, destination: String)
    case OpenedRay(from: String, actor: Piece, end: String)
    case ChangedLines(closed: List[(Piece, Piece, List[Piece])], opened: List[(Piece, Piece)],
        blockedSquares: List[(Piece, String, List[String])])
    case RookFile(actor: Piece, fileType: String, supported: Option[(String, List[Piece])])

    def text: String = this match
      case Development(role, from, to) => s"Develops the $role from $from to $to."
      case Castle(from, to) => s"Castles, bringing the rook from $from to $to."
      case Capture(role, at, recapture) =>
        s"${if recapture.isInstanceOf[LineMaterialRecaptureStatus.Proven] then "Recaptures" else "Captures"} the $role on $at."
      case Evades(target, attacker) => s"Moves the ${target.roleName} away from the attack of ${attacker.name}."
      case Attack(actor, target, gained) => s"${actor.name} ${if gained then "now targets" else "keeps targeting"} ${target.name}."
      case Support(actor, target, opposition, gained) =>
        s"${actor.name} ${if gained then "adds support to" else "keeps supporting"} ${target.name}, under attack from ${opposition.map(_.name).mkString(", ")}."
      case RookConnection(actor, target) => s"Connects the rooks on ${actor.at} and ${target.at}."
      case PassedPawn(to, Some(true)) => s"Advances the passed pawn to $to."
      case PassedPawn(to, Some(false)) => s"Creates a passed pawn on $to."
      case PassedPawn(to, None) => s"The pawn on $to is a passed pawn."
      case KingEntry(actor, squares) =>
        s"${actor.name} now controls ${squares.mkString(", ")} next to the opposing king; the king cannot enter ${if squares.size > 1 then "these squares" else "this square"}."
      case PawnBlock(to, pawn, destination) => s"Occupies $to, blocking the advance of ${pawn.name} to $destination."
      case OpenedRay(from, actor, end) => s"Clears $from, opening a line for ${actor.name} toward $end."
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
        case Detail.Development(role, from, to) => Json.obj("before_role" -> role, "from" -> from, "to" -> to)
        case Detail.Castle(from, to) => Json.obj("rook_from" -> from, "rook_to" -> to)
        case Detail.Capture(role, at, recapture) => Json.obj("captured_role" -> role,
          "captured_square" -> at, "captured_position_id" -> edge.from.id.get,
          "recapture" -> (recapture match
            case LineMaterialRecaptureStatus.Proven(_) => "proven"
            case LineMaterialRecaptureStatus.Excluded => "excluded"
            case LineMaterialRecaptureStatus.Unknown => "unknown"))
        case Detail.PassedPawn(to, previous) => Json.obj("to" -> to, "previously_passed" -> previous)
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

  /** Presentation only: inspect the two paths already owned by a comparison.
    * The investigation reader's relation, priority and engine authority stay unchanged.
    */
  private def comparisonEffects(packet: EvidenceBackedJudgmentPacket, local: List[LocalCandidate],
      positions: Map[PositionNodeRef, Position], edges: Map[String, MoveTransitionEdge],
      paths: ObservedLineProtocol.PreparedPaths, observations: Map[String, List[MoveObservation]],
      destinations: Map[String, String]): JsObject => JsObject =
    val byOccurrence = local.groupBy(_.edge.evidence.id)
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
    // The same owned line can occur in several existing comparisons. Read its
    // selected attacks once, using the packet's existing physical-piece traces.
    val outcomes = mutable.Map.empty[(String, Int), List[(String, List[String])]]
    def attackOutcomes(path: ObservedLineProtocol.PreparedPath, index: Int): List[(String, List[String])] =
      outcomes.getOrElseUpdate(path.line.ref.id -> index, {
        if path.certified.isEmpty || path.ids != path.declared || path.moves.size <= index + 1 then Nil
        else
          val end = path.moves.size
          val notes = path.ids.map(id => byOccurrence.getOrElse(id, Nil)).toVector
          lazy val atAttack = paths.identitiesAfter(packet, path, index + 1)
          def original(piece: Piece): Option[Piece] = atAttack.collectFirst {
            case (root, Some(Some(current))) if current == piece => root
          }
          notes(index).filter(_.selection == CommentarySelection.Display.Selected).flatMap { first =>
            first.note.detail match
              case Detail.Attack(attacker, target, _) =>
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
                  val evasions = (index + 1 until end).flatMap { offset =>
                    for
                      afterTarget <- tracked(originalTarget, offset + 1)
                      afterAttacker <- tracked(originalAttacker, offset + 1)
                      note <- notes(offset).find(_.note.detail == Detail.Evades(afterTarget, afterAttacker))
                    yield offset -> note
                  }.toList
                  var observedBlock = false
                  val stationary = (index + 1 until end).flatMap { offset =>
                    for
                      beforeTarget <- tracked(originalTarget, offset)
                      afterTarget <- tracked(originalTarget, offset + 1)
                      if beforeTarget == afterTarget
                      beforeAttacker <- tracked(originalAttacker, offset)
                      before <- positions.get(path.positions(offset))
                      after <- positions.get(path.positions(offset + 1))
                      active = before.controlledBy(beforeAttacker)(beforeTarget.at)
                      removal = Option.when(active)(captureAt(originalAttacker, offset)).flatten
                      afterAttacker = tracked(originalAttacker, offset + 1)
                      change <- removal.map(note => (note, "capture", beforeAttacker)).orElse {
                        notes(offset).flatMap { note => note.note.detail match
                          case Detail.ChangedLines(closed, opened, _) if afterAttacker.contains(beforeAttacker) =>
                            if closed.exists((a, t, _) => a == beforeAttacker && t == beforeTarget) then
                              Some((note, "block", beforeAttacker))
                            else Option.when(observedBlock && opened.contains(beforeAttacker -> beforeTarget))(
                              (note, "reopen", beforeAttacker))
                          case Detail.Attack(a, t, true) if observedBlock && afterAttacker.contains(a) && t == afterTarget =>
                            Some((note, "reopen", a))
                          case _ => None
                        }.headOption
                      }
                    yield
                      val (note, kind, currentAttacker) = change
                      if kind == "block" then observedBlock = true
                      else if kind == "reopen" then observedBlock = false
                      val remaining = if kind == "reopen" then Nil else after.controllers(afterTarget.at, attacker.side)
                      val action = kind match
                        case "capture" => s"captures the original attacker ${beforeAttacker.name}; the initially attacked ${target.name} stays on ${afterTarget.at}."
                        case "block" => s"blocks ${beforeAttacker.name}'s line to the initially attacked ${target.name}, which stays on ${afterTarget.at}."
                        case _ => s"restores ${currentAttacker.name}'s attack on the initially attacked ${target.name}."
                      val text = action + (if remaining.isEmpty then "" else
                        s" ${remaining.map(_.name).mkString(" and ")} ${if remaining.size == 1 then "attacks" else "attack"} that target after this move.")
                      val evidence = sourceRefs(before, before.sources(beforeAttacker, List(beforeTarget.at))) ++
                        sourceRefs(after, remaining.flatMap(piece => after.sources(piece, List(afterTarget.at))))
                      (offset, note, text, evidence)
                  }.toList
                  // Survival alone is not a resolution. Keep the observed mechanism
                  // and later reopening/capture; no inferred permanent safety.
                  if captures.isEmpty && evasions.isEmpty && stationary.isEmpty then Nil
                  else
                    val firstOutcome = ((captures ++ evasions).map(_._1) ++ stationary.map(_._1)).min
                    // An actual response to the immediate counterattack can
                    // explain the intervening move. Same color alone cannot.
                    val counterAttacks = if firstOutcome <= index + 1 then Nil else
                      notes(index + 1).filter(candidate => candidate.selection == CommentarySelection.Display.Selected &&
                        (candidate.note.detail match
                          case Detail.Attack(actor, _, _) => actor.side == target.side
                          case _ => false))
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
                    val responses = (index + 2 until firstOutcome).flatMap { offset =>
                      notes(offset).filter(candidate => candidate.note.detail match
                        case Detail.Evades(victim, actor) => counterPairs.exists { (a, t) =>
                          tracked(a, offset + 1).contains(actor) && tracked(t, offset + 1).contains(victim)
                        }
                        case _ => false).map(offset -> _)
                    }.toList
                    val middle = if responses.isEmpty then Nil else counterAttacks.map(index + 1 -> _) ++ responses
                    val sequence = (middle ++ evasions ++ captures ++ stationary.map(row => row._1 -> row._2)).sortBy(_._1)
                    val steps = sequence.groupBy(_._1).toList.sortBy(_._1).map { (offset, rows) =>
                      val wording = rows.map { (_, candidate) =>
                        if captures.exists(_._2 == candidate) then s"captures the initially attacked ${target.name}."
                        else if evasions.exists(_._2 == candidate) then
                          val Detail.Evades(_, currentAttacker) = candidate.note.detail: @unchecked
                          s"moves the initially attacked ${target.name} out of ${currentAttacker.name}'s attack."
                        else stationary.find(row => row._1 == offset && row._2 == candidate)
                          .fold(candidate.note.detail.text)(_._3)
                      }.distinct.mkString(" ")
                      s"${path.moves(offset)}: $wording"
                    }.mkString(" ")
                    val endPosition = path.positions.lastOption.flatMap(positions.get)
                    val endAttacker = tracked(originalAttacker, end)
                    val renewed = for
                      piece <- endTarget
                      actor <- endAttacker
                      position <- endPosition
                      if position.controlledBy(actor)(piece.at)
                    yield (actor, piece, position)
                    val ending = endTarget.fold("That original target is captured within this line.") { piece =>
                      s"That original target remains on ${piece.at} at this line's end." +
                        renewed.fold("")(_ => " The original attacker attacks it again there.")
                    }
                    val text = s"In the provided ${path.moves(index)} line, after ${attacker.name} attacks ${target.name}, " +
                      s"$steps $ending"
                    val evidence = (first :: sequence.map(_._2)).flatMap(noteEvidence) ++ stationary.flatMap(_._4) ++
                      path.certified.map(_._1.ref.id) ++ renewed.toList.flatMap { (actor, piece, position) =>
                        sourceRefs(position, position.sources(actor, List(piece.at)))
                      }
                    List(text -> evidence.distinct)
                ).toList.flatten
              case _ => Nil
          }
      })
    candidate =>
      val detail = (candidate \ "detail").as[JsObject]
      val supported = Set("evaluation_comparison", "observed_material_comparison", "pv_comparison", "question_comparison")
      val pair = if supported((detail \ "type").as[String]) then
        (candidate \ "line_ids").as[List[String]].flatMap(paths.path(packet, _)) else Nil
      val prefix = (detail \ "prefix_moves_uci").asOpt[List[String]].getOrElse(Nil)
      val index = prefix.size
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
              val text = s"After ${edge.moveUci}, ${relation(actor, target, square, present)}; " +
                s"after ${peerEdge.moveUci}, ${relation(peerActor, peerTarget, peerSquare, peerPresent)}." +
                (if remaining.isEmpty then "" else s" ${remaining.map(_.name).mkString(" and ")} still " +
                  s"${if actor.side == target.get.side then "support" else "attack"}${if remaining.size == 1 then "s" else ""} that target after ${edge.moveUci}.") +
                (if target.nonEmpty then "" else s" In the provided ${edge.moveUci} line, the immediate reply is ${path.moves(index + 1)}.")
               val evidence = premises ++ sourceRefs(peerAfter, peerAfter.sources(peerActor, List(peerSquare))) ++
                List(edge.evidence.id, peerEdge.evidence.id, note.before.view.occurrence.id,
                  note.after.view.occurrence.id, peerAfter.view.occurrence.id) ++
                Option.when(target.isEmpty)(path.ids(index + 1)).toList
              ((actor, target, square), text, evidence, ray)
          }.distinctBy(_._1)
        case _ => Nil
      val differences = observedDifferences.filterNot { row =>
        row._1._2.exists(target => observedDifferences.exists(other =>
          other._1._1 == row._1._1 && other._4.contains(target.at)))
      }
      val pv = (detail \ "type").as[String] == "pv_comparison"
      val limit = if pv then 1000 else 4000
      val originalText = (detail \ "text").asOpt[String].getOrElse("")
      val changedText = originalText + " " + differences.map(_._2).mkString(" ")
      val (effectText, effectEvidence) = if differences.nonEmpty && changedText.length <= limit then
        changedText -> differences.flatMap(_._3) else originalText -> Nil
      val followed = (if differences.nonEmpty && changedText.length > limit then Nil else pair) match
        case List(left, right) if left.ids.take(index) == right.ids.take(index) &&
            left.moves.take(index) == prefix && right.moves.take(index) == prefix &&
            left.positions.lift(index).nonEmpty && left.positions.lift(index) == right.positions.lift(index) =>
          pair.flatMap(path => attackOutcomes(path, index))
        case _ => Nil
      val withOutcomes = effectText + " " + followed.map(_._1).mkString(" ")
      val (text, evidence) = if followed.nonEmpty && withOutcomes.length <= limit then
        withOutcomes -> (effectEvidence ++ followed.flatMap(_._2)) else effectText -> effectEvidence
      // Preserve the existing verdict and the entire cost/benefit bundle at the wire limit.
      // PV evidence IDs are the exact two line summaries; local proofs remain at
      // its existing occurrence/position links and must not replace that contract.
      if text == originalText then candidate else
        candidate ++ Json.obj("evidence_ids" -> ((candidate \ "evidence_ids").as[List[String]] ++
          (if pv then Nil else evidence)).distinct,
          "detail" -> (detail + ("text" -> JsString(text))))

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
          case MoveObservationMeaning.Development(_) =>
            add("development", mover, Detail.Development(previousMover.roleName, from, to), List(from, to), 65, premises = premises)
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
              if next.state == PieceControlState.Present && a.side == side && t.roleName != "king" && (gained || maintained) then
                if t.side == other && (gained || oldA != a) then
                  add("attack", a, Detail.Attack(a, t, gained), List(a.at, t.at), 70 + values(t.roleName), List("attack", t.at), premises)
                else if t.side == side then
                  val opposition = after.controllers(t.at, other)
                  if opposition.nonEmpty then
                    add("support", a, Detail.Support(a, t, opposition, gained), List(a.at, t.at) ++ opposition.map(_.at),
                      75 + (if gained then 1 else 0), List("support", t.at), premises,
                      afterSources = opposition.flatMap(p => after.sources(p, List(t.at))))
                  if gained && a.roleName == "rook" && t.roleName == "rook" then
                    add("rook_connection", a, Detail.RookConnection(a, t), List(a.at, t.at), 64, List("rook_connection"), premises)
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
