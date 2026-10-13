package io.chesstory.runtime

import lila.chessjudgment.model.judgment.*
import ObservedLineProtocol.{PreparedPath, PreparedPaths}
import MoveWorkProjection.{Piece, Position}
import CommentaryEnglish.at
import play.api.libs.json.*

/** A shared destination motivates a question, never functional equivalence.
  * The two returned continuations must establish the actual difference. */
private[runtime] object PurposeChoice:
  final case class Route(original: Piece, current: Option[Piece], blocked: List[(String, Piece)],
      contested: List[(String, List[Piece])], open: List[String])
  final case class Entry(square: String, originalTargets: List[Piece], remaining: List[Piece])
  final case class Neutralization(owner: Reading, alternative: Arm, reply: MovePurpose.Branch, target: Piece,
      post: String, guard: Piece, retainedGuard: Piece, opponent: Piece)
  final case class Arm(path: PreparedPath, actor: Piece, through: Int, state: ExchangeActivity.State,
      retained: Route, entry: List[Entry], blocksRoute: Boolean, constrained: Option[(Int, Route)])
  final class Reading private[PurposeChoice](private val owner: PreparedPaths,
      val question: MovePurpose.Question, val answer: CommentaryInvestigation.Answer, val arms: List[Arm]):
    def belongsTo(paths: PreparedPaths) = owner eq paths
    lazy val escaped = (for
      before <- owner.purposePosition(arms.head.path, 0)
      after <- owner.purposePosition(arms.head.path, 1)
      moved <- owner.trackedPieceAfter(arms.head.path, question.actor, 1).flatten
    yield before.controllers(question.actor.at, !question.actor.side).filter(attacker =>
      owner.trackedPieceAfter(arms.head.path, attacker, 1).flatten.exists(current =>
        !after.controlledBy(current)(moved.at)))).getOrElse(Nil)
    def json = Json.obj("alternative_move" -> arms(1).path.moves.head,
      "work_id" -> answer.workId, "execution_key_sha256" -> answer.executionKeySha256,
      "line_ids" -> arms.map(_.path.line.ref.id))

  def inquiry(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths): Option[CommentaryInvestigation.Question] =
    paths.requireOwner(packet)
    for
      q <- paths.movePurpose.filter(_.activity.nonEmpty)
      before <- paths.purposePosition(q.path, 0)
      row <- before.view.legalRows.toList.flatten.filter(row => row.actor.side == q.actor.side &&
        row.actor != q.actor && row.to.key == q.square && row.capture.isEmpty && row.promotionRole.isEmpty &&
        Set("bishop", "knight", "rook", "queen")(row.actor.role.name)).sortBy(_.move).headOption
    yield CommentaryInvestigation.Question(
      CommentarySelection.stableId("purpose-choice", List(q.path.ids.head, q.rootMove, row.move)),
      q.path.ids.head, q.path.positions.head.id.get, q.rootMove, row.move,
      List(CommentaryInvestigation.Reason(CommentaryInvestigation.Kind.SameDestination,
        before.view.occurrence, Some(row.sourceId), None, q.rootMove, row.actor, row.capture,
        row.promotionRole, false)), List(q.path.ids.head), Nil)

  private def route(paths: PreparedPaths, path: PreparedPath, original: Piece, count: Int): Option[Route] =
    paths.purposePosition(path, count).map { after =>
      val current = paths.trackedPieceAfter(path, original, count).flatten
      val moves = current.toList.flatMap(after.controlledBy).sorted
      val blocked = moves.flatMap(square => after.occupant(square).filter(_.side == original.side).map(square -> _))
      val available = moves.filterNot(square => blocked.exists(_._1 == square))
      val contested = available.flatMap(square => Option.when(after.controllers(square, !original.side).nonEmpty)(
        square -> after.controllers(square, !original.side)))
      Route(original, current, blocked, contested, available.filterNot(square => contested.exists(_._1 == square)))
    }

  /** Track the targets of a possible double attack through an actual exchange.
    * This is a geometric option, not a proof that the entry wins material. */
  private def entries(paths: PreparedPaths, path: PreparedPath, actor: Piece, through: Int): List[Entry] =
    if actor.role.name != "knight" then return Nil
    val positions = for
      start <- paths.purposePosition(path, 1)
      end <- paths.purposePosition(path, through + 1)
      moved <- paths.trackedPieceAfter(path, actor, 1).flatten
    yield (start, end, moved)
    positions.toList.flatMap { (start, end, moved) =>
      start.controlledBy(moved).toList.sorted.filter(start.empty).flatMap { square =>
        val targets = lila.chessjudgment.analysis.assembly.OccurrenceResources.planningKnightControls(square, actor.side)
          .toList.sorted.flatMap(start.occupant).filter(p => p.side != actor.side && Set("rook", "queen", "king")(p.role.name))
        val current = targets.flatMap(target => paths.trackedPieceAfter(path, target, through + 1).flatten)
        val remaining = current.filter(p =>
          lila.chessjudgment.analysis.assembly.OccurrenceResources.planningKnightControls(square, actor.side)(p.at))
        Option.when(targets.size >= 2 && remaining.size < targets.size)(Entry(square, targets, remaining))
      }
    }

  def read(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      receipts: List[CommentaryInvestigation.EngineCandidates]): Option[Reading] =
    paths.requireOwner(packet)
    for
      q <- paths.movePurpose.filter(_.activity.nonEmpty)
      comparison <- inquiry(packet, paths)
      answer <- CommentaryInvestigation.preferredReceipts(receipts).iterator.flatMap(receipt =>
        CommentaryInvestigation.answer(comparison, receipt.work, receipt.lines, packet, Some(paths))).nextOption()
      left <- paths.paths.find(_.line.ref.id == answer.referenceLineId)
      right <- paths.paths.find(_.line.ref.id == answer.alternativeLineId)
      arms = List(left, right).map { path =>
        for
          before <- paths.purposePosition(path, 0)
          actor <- before.occupant(path.moves.head.take(2))
          if path.moves.size >= 5
          // Compare the first consolidation, before an incidental later retreat
          // turns a successfully neutralized line into an apparent lasting bind.
          use = 4
          // Keep enough of a different actor's line to see the fate of the
          // piece it left behind, including the next consecutive recaptures.
          loss = (1 until math.min(path.moves.size, 12)).find(offset =>
            paths.trackedPieceAfter(path, q.actor, offset).flatten.nonEmpty &&
              paths.trackedPieceAfter(path, q.actor, offset + 1).flatten.isEmpty)
          end = if actor != q.actor then math.max(use, loss.getOrElse(use)) else use
          state <- ExchangeActivity.state(paths, path, actor, q.activity.get, end)
          retained <- route(paths, path, q.actor, state.end + 1)
          blocks = actor != q.actor && before.controlledBy(q.actor)(q.square) &&
            paths.trackedPieceAfter(path, q.actor, 1).flatten.contains(q.actor)
          constrained = Option.when(blocks)((1 to state.end).iterator.flatMap { offset =>
            route(paths, path, q.actor, offset).filter(r => r.current.exists(p =>
              path.moves(offset).take(2) == p.at) && r.open.isEmpty && r.contested.nonEmpty &&
              r.blocked.exists(_._1 == q.square)).map(offset -> _)
          }.nextOption()).flatten
        yield Arm(path, actor, state.end, state, retained, entries(paths, path, actor, state.end), blocks, constrained)
      }
      if arms.forall(_.nonEmpty)
    yield new Reading(paths, q, answer, arms.flatten)

  def neutralization(value: Reading, branches: List[MovePurpose.Branch], paths: PreparedPaths): Option[Neutralization] =
    if !value.belongsTo(paths) then return None
    val q = value.question
    val alternative = value.arms(1)
    for
      before <- paths.purposePosition(alternative.path, 0)
      after <- paths.purposePosition(alternative.path, alternative.through + 1)
      result <- q.activity.toList.flatMap(_.targets).iterator.flatMap { target =>
        val at = paths.trackedPieceAfter(alternative.path, target, alternative.through + 1).flatten
        at.toList.filter(p => p.at != target.at).flatMap { current =>
          before.controllers(current.at, q.actor.side).flatMap { guard =>
            branches.flatMap { reply =>
              for
                board <- paths.purposePosition(reply.path, reply.through + 1).toList
                retained <- paths.trackedPieceAfter(reply.path, guard, reply.through + 1).flatten.toList
                opponent <- paths.trackedPieceAfter(reply.path, target, reply.through + 1).flatten.toList
                if paths.trackedPieceAfter(alternative.path, guard, alternative.through + 1).flatten.isEmpty &&
                  after.controllers(current.at, q.actor.side).isEmpty && board.controlledBy(retained)(current.at) &&
                  opponent.at != current.at && reply.path.moves.indices.exists(offset =>
                    paths.trackedPieceAfter(reply.path, target, offset + 1).flatten.exists(_.at == current.at))
              yield Neutralization(value, alternative, reply, target, current.at, guard, retained, opponent)
            }
          }
        }
      }.nextOption()
    yield result
