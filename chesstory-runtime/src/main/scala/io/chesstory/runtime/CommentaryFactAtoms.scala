package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.{ LegalMoveResource, OccurrenceResources, PositionFacts }
import lila.chessjudgment.model.judgment.*

/** Scalar readings of one existing exact occurrence. Temporal claims belong to the caller. */
private[runtime] object CommentaryFactAtoms:
  import ObservedLineProtocol.{ PreparedPath, PreparedPaths }

  case class Value(value: Option[String], sourceIds: List[String] = Nil, reason: Option[String] = None)

  private case class Context(sideToMove: Option[chess.Color], view: Option[PositionFacts],
      current: CommentaryFact.Piece => Option[Option[RelationColoredPieceWitness]],
      support: (CommentaryFact.Piece, CommentaryFact.Piece) => LineContinuationInvestigation.Support,
      directSupportUse: () => Value, identitySources: List[String] = Nil, pointSources: List[String] = Nil)

  def read(packet: EvidenceBackedJudgmentPacket, prepared: PreparedPaths, path: PreparedPath,
      count: Int, predicate: CommentaryFact.Predicate): Value =
    readOwned(packet, prepared, path, count, predicate, inspection = false)

  /** Candidate provenance names the inspected semantic relation and its exact
    * point/identity anchors. It is not the public evidence certificate. Keep
    * both readings behind the same exact admitted-source validation. */
  def readForDiscovery(packet: EvidenceBackedJudgmentPacket, prepared: PreparedPaths, path: PreparedPath,
      count: Int, predicate: CommentaryFact.Predicate): Value =
    readOwned(packet, prepared, path, count, predicate, inspection = true)

  private def readOwned(packet: EvidenceBackedJudgmentPacket, prepared: PreparedPaths, path: PreparedPath,
      count: Int, predicate: CommentaryFact.Predicate, inspection: Boolean): Value =
    def unavailable(reason: String) = Value(None, reason = Some(reason))
    if !CommentaryFact.validPredicate(predicate) then unavailable("invalid_predicate")
    else if !prepared.belongsTo(packet) then unavailable("packet_owner_mismatch")
    else if !prepared.path(packet, path.line.ref.id).exists(_ eq path) then unavailable("path_owner_mismatch")
    else if count < 0 || count > path.moves.size then unavailable("point_outside_observed_path")
    else
      val position = path.positions(count)
      val views = prepared.resourceViews(packet)
      val view = position.id.flatMap(views.get).filter(_.position == position)
      val identitySources = path.positions.head.id.flatMap(views.get).map(_.occurrence.id).toList ++ path.ids.take(count)
      // A semantic relation can recur at several positions. Only its actual
      // queried/event position can supply the public evidence reference.
      def sourceRefs(at: Int, sources: List[String]): Option[List[String]] =
        lazy val point = path.positions.lift(at).flatMap(position => position.id.flatMap(views.get)
          .filter(_.position == position)).map(new MoveWorkProjection.Position(_))
        sources.foldLeft(Option(List.empty[String])) { (collected, source) =>
          val refs = packet.evidenceGraph.byId.get(source) match
            case Some(record) => Option.when(packet.evidenceGraph.proofEligible(record))(List(source))
            case None => point.flatMap { current =>
              val exact = current.sourceRefs(List(source))
              Option.when(exact.nonEmpty && exact.forall(id => packet.evidenceGraph.byId.get(id)
                .exists(packet.evidenceGraph.proofEligible)))(exact)
            }
          for previous <- collected; current <- refs yield (previous ++ current).distinct
        }
      lazy val directUse: (Value, Value) =
        val observation = LineContinuationInvestigation.directSupportUse(packet, path, prepared, count,
          predicate.subject.get.witness, predicate.target.get.witness)
        if observation.uses.nonEmpty then
          val sources = observation.uses.map { use =>
            for
              support <- sourceRefs(use.supportCount, use.support.sourceIds)
              capture <- sourceRefs(use.targetCapture.plyOffset, List(use.targetCapture.sourceId))
              recapture <- sourceRefs(use.recapture.plyOffset, List(use.recapture.sourceId))
            yield support ++ capture ++ recapture
          }
          if sources.forall(_.nonEmpty) then
            Value(Some("true"), observation.uses.flatMap(use =>
              use.support.sourceIds ++ List(use.targetCapture.sourceId, use.recapture.sourceId)).distinct) ->
              Value(Some("true"), sources.flatten.flatten.distinct)
          else
            val missing = unavailable("missing_position_evidence")
            missing -> missing
        else
          val value = if observation.complete then Value(Some("false"), path.certified.map(_._1.ref.id).toList)
            else Value(None, reason = observation.reason)
          value -> value
      val context = Context(position.sideToMove, view,
        piece => prepared.pieceAfter(packet, path, piece.witness, count),
        (actor, target) => prepared.supportAt(packet, path, count, actor.witness, target.witness),
        () => directUse._2, identitySources, view.map(_.occurrence.id).toList)
      val value = readCore(predicate, context)
      val admitted = sourceRefs(count, value.sourceIds).fold(unavailable("missing_position_evidence"))(
        sources => value.copy(sourceIds = sources))
      if inspection && admitted.value.nonEmpty then
        readCore(predicate, context.copy(directSupportUse = () => directUse._1))
      else admitted

  /** Local semantic readings are candidate-generation inputs, never public occurrence evidence. */
  def readProbe(position: OccurrenceResources.ProbePosition, predicate: CommentaryFact.Predicate): Value =
    if !CommentaryFact.validPredicate(predicate) then Value(None, reason = Some("invalid_predicate"))
    else
      lazy val support = new LineContinuationInvestigation.SupportPoint(Some(position.facts), position.identities)
      readCore(predicate, Context(Some(position.sideToMove), Some(position.facts),
        piece => position.piece(piece.witness),
        (actor, target) => support.support(actor.witness, target.witness),
        () => Value(None, reason = Some("probe_requires_admitted_line"))))

  private def readCore(predicate: CommentaryFact.Predicate, context: Context): Value =
      def unavailable(reason: String) = Value(None, reason = Some(reason))
      def known(value: String, sources: List[String] = Nil): Value =
        Value(Some(value), (sources ++ context.pointSources).distinct)
      def current(piece: CommentaryFact.Piece) = context.current(piece)
      def withSubject(removed: String)(read: RelationColoredPieceWitness => Value): Value =
        current(predicate.subject.get) match
          case None => unavailable("missing_subject_identity")
          case Some(None) => known(removed)
          case Some(Some(actor)) => read(actor)
      def withTarget(read: RelationColoredPieceWitness => Value): Value =
        current(predicate.target.get) match
          case None => unavailable("missing_target_identity")
          case Some(None) => known("target_removed")
          case Some(Some(target)) => read(target)
      def withView(read: PositionFacts => Value): Value =
        context.view.fold(unavailable("missing_position_resources"))(read)
      def legal(actor: RelationColoredPieceWitness)(matches: LegalMoveResource => Boolean): Value =
        context.sideToMove match
          case None => unavailable("missing_side_to_move")
          case Some(side) if side != actor.side => known("not_turn")
          case _ => withView { resource =>
            resource.legalRows.fold(unavailable("incomplete_legal_inventory")) { rows =>
              val witnesses = rows.filter(row => row.actor == actor && matches(row))
              known(if witnesses.nonEmpty then "legal" else "illegal", witnesses.map(_.sourceId))
            }
          }

      val result = predicate.kind match
        case "presence" => withSubject("removed")(_ => known("present"))
        case "location" => withSubject("removed")(actor => known(actor.square.key))
        case "role" => withSubject("removed")(actor => known(actor.role.name))
        case "control" => withSubject("actor_removed") { actor =>
          withView { resource =>
            val sources = resource.controlSources(actor, EvidenceSquare(predicate.square.get))
            if sources.nonEmpty then known("present", sources)
            else if resource.completeKinds(RelationFactKind.GeometricControl) then known("absent")
            else unavailable("incomplete_control_inventory")
          }
        }
        case "support" => withSubject("actor_removed") { _ =>
          withTarget { _ =>
            val support = context.support(predicate.subject.get, predicate.target.get)
            if support.state == "unknown" then unavailable("incomplete_support_inventory")
            else known(support.state, support.sourceIds)
          }
        }
        case "legal_move" => withSubject("actor_removed") { actor =>
          legal(actor)(row => row.to.key == predicate.square.get &&
            predicate.property.forall(role => row.promotionRole.contains(role)))
        }
        case "legal_capture" => withSubject("actor_removed") { actor =>
          withTarget(target => legal(actor)(_.captures(target)))
        }
        case "check" => withView { resource =>
          val side = if predicate.side.contains("white") then chess.White else chess.Black
          val sources = resource.currentCheckSources(side).map(_.semanticId)
          if sources.nonEmpty then known("present", sources)
          else if resource.completeKinds(RelationFactKind.GeometricControl) then known("absent")
          else unavailable("incomplete_control_inventory")
        }
        case "pawn_property" => withSubject("actor_removed") { actor =>
          if actor.role.name != chess.Pawn.name then known("not_pawn")
          else withView { resource =>
            resource.pawnStates.find(_.square == actor.square).fold(unavailable("missing_pawn_state")) { state =>
              val value = predicate.property.get match
                case "passed" => state.passed
                case "isolated" => state.isolated
                case "doubled" => state.doubled
                case _ => None
              value.fold(unavailable("missing_pawn_" + predicate.property.get + "_sources"))(
                present => known(present.toString, state.sourceIds))
            }
          }
        }
        // Empty forward passage is independent of turn and king safety; it is not legal_move.
        case "pawn_advance" => withSubject("actor_removed") { actor =>
          if actor.role.name != chess.Pawn.name then known("not_pawn")
          else withView { resource =>
            resource.completeFacts(RelationFactKind.PawnAdvanceAffordance)
              .fold(unavailable("incomplete_pawn_advance_inventory")) { facts =>
                val sources = facts.filter(_.detail match
                  case RelationWitnessDetail.PawnAdvanceAffordance(side, pawn, destination, _) =>
                    side == actor.side && pawn == actor.square && destination.key == predicate.square.get
                  case _ => false).map(_.semanticId)
                known(if sources.nonEmpty then "present" else "absent", sources)
              }
          }
        }
        case "file_occupancy" => withView { resource =>
          resource.pawnFiles.find(_.file.key == predicate.property.get).fold(unavailable("incomplete_pawn_file_inventory"))(
            file => known(file.occupancy, file.sourceIds))
        }
        case "direct_support_use" =>
          val observation = context.directSupportUse()
          observation.value.fold(observation)(value => known(value, observation.sourceIds))
        case _ => unavailable("unsupported_predicate")
      if predicate.subject.nonEmpty && result.value.nonEmpty then
        result.copy(sourceIds = (context.identitySources ++ result.sourceIds).distinct)
      else result
