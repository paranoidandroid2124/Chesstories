package io.chesstory.runtime

import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.CandidateLineEvaluation
import ObservedLineProtocol.{PreparedPath, PreparedPaths}
import MoveWorkProjection.{Piece, Position}
import CommentaryAnswer.*
import CommentaryEnglish.at
import RootQuestionSummary.MoveRef

/** Read the pair that actually owns the verdict before a supplementary choice.
  * A supported post is a shared question: preparation, immediate occupation,
  * and the position after the first exchange are different commitments.
  * This explains an observed positional cost, not a decomposition of the score.
  */
private[runtime] object ReferencePurposeReading:
  final case class Arm(path: PreparedPath, actor: Piece, moved: Piece, through: Int,
      attacks: List[Piece], shields: List[(Piece, Piece)], guards: List[Piece],
      remainingGuards: List[Piece], occupant: Option[Piece],
      retainedMover: Option[Piece], reply: Piece, replyAt: Option[Piece], replyGuards: List[Piece])
  final class Reading private[ReferencePurposeReading](private val owner: PreparedPaths,
      val square: String, val reference: Arm, val played: Arm,
      val retainedChoice: Option[Piece], val referencePreferred: Boolean, val evidence: List[String]):
    def belongsTo(paths: PreparedPaths) = owner eq paths
    def arms = List(reference, played)

  def assessmentPair(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths): List[PreparedPath] =
    paths.requireOwner(packet)
    packet.primary.toList.flatMap {
      case PlayerFacingPrimary.MoveVerdict(ref) => List(ref)
      case PlayerFacingPrimary.BestChoice(ref) => List(ref)
    }.flatMap(packet.evidenceGraph.candidateComparisonRecord).flatMap(_.payload match
      case CandidateComparisonEvidence(value) => List(value.referenceLine, value.candidateLine)
      case _ => Nil).flatMap(ref => paths.paths.find(_.line.ref == ref))

  def read(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      pair: List[PreparedPath]): Option[Reading] =
    paths.requireOwner(packet)
    val primary = packet.primary.map {
      case PlayerFacingPrimary.MoveVerdict(ref) => ref
      case PlayerFacingPrimary.BestChoice(ref) => ref
    }
    val comparison = primary.flatMap(packet.evidenceGraph.candidateComparisonRecord).flatMap(_.payload match
      case CandidateComparisonEvidence(value) => Some(value)
      case _ => None).filter(value => pair.size == 2 &&
        Set(value.referenceLine, value.candidateLine) == pair.map(_.line.ref).toSet)
    if comparison.isEmpty || pair.exists(path => !paths.paths.exists(_ eq path) ||
        path.moves.size < 3 || path.moves != path.sourceMoves || path.ids != path.declared ||
        path.automaticTerminal.nonEmpty || path.certified.flatMap(_._2.material).isEmpty ||
        !packet.candidateLines.find(_.ref == path.line.ref).exists(_.evaluation match
          case CandidateLineEvaluation.EngineSearch(line) => line.mate.isEmpty
          case _ => false)) then return None
    val playedRoot = packet.assembly.playedTransition.map(_.moveUci)
    val ordered = pair.sortBy(path => if playedRoot.contains(path.moves.head) then 1 else 0)
    if !playedRoot.contains(ordered(1).moves.head) then return None
    val before = paths.purposePosition(ordered.head, 0).get
    if !before.complete(RelationFactKind.GeometricControl) || before.view.currentCheckSources(
        before.occupant(ordered.head.moves.head.take(2)).get.side).nonEmpty then return None
    val side = before.occupant(ordered.head.moves.head.take(2)).get.side
    // shortcut: pawn-backed central posts only; extend when another purpose has an owned pair of reply witnesses.
    val posts = ordered.flatMap(path => before.occupant(path.moves.head.take(2))
      .filter(p => Set("bishop", "knight")(p.role.name)).map(_ => path.moves.head.slice(2, 4)))
      .filter(s => Set('c', 'd', 'e', 'f')(s.head) && Set('3', '4', '5', '6')(s.last)).distinct
    val proof = scala.collection.mutable.ListBuffer.empty[String]
    def position(path: PreparedPath, count: Int): Option[Position] = paths.purposePosition(path, count)
      .filter(_.complete(RelationFactKind.GeometricControl)).map { value =>
        proof += value.view.occurrence.id
        value
      }
    def pawns(at: Position, square: String) =
      val guards = at.controllers(square, side).filter(_.role.name == "pawn")
      proof ++= at.sourceRefs(guards.flatMap(p => at.sources(p, List(square))))
      guards
    posts.iterator.flatMap { square =>
      val arms = ordered.map { path =>
        for
          actor <- before.occupant(path.moves.head.take(2))
          after <- position(path, 1)
          moved <- paths.trackedPieceAfter(path, actor, 1).flatten
          // Quiet roots only. Capturing and checking roots retain their existing
          // complete tactical question rather than acquiring a strategic gloss.
          if !path.certified.get._2.material.get.events.exists(_.plyOffset == 0) &&
            !path.certified.get._2.lineEvents.exists(e => e.plyOffset == 0 && e.kind == LineEventKind.Check)
          // A capture on another square can continue the same opening exchange.
          // Stop at its first quiet move, not at the first non-recapture: otherwise
          // the apparent remaining knight can already be lost in the price closure.
          through =
            val captures = path.certified.get._2.material.get.events.map(_.plyOffset).toSet
            var end = 2
            while end + 1 < path.moves.size && captures(end + 1) do end += 1
            end
          end <- position(path, through + 1)
          reply <- after.occupant(path.moves(1).take(2))
        yield
          val attacks = after.controlledBy(moved).toList.sorted.flatMap(after.occupant).filter(_.side != side)
          proof ++= after.sourceRefs(after.sources(moved, attacks.map(_.at)))
          val shields = before.controls.keys.toList.filter(p => p.side == side && p != actor).flatMap { target =>
            before.controllers(target.at, !side).flatMap { attacker =>
              val shield = Option.when(after.contains(attacker) && after.contains(target) &&
                !after.controlledBy(attacker)(target.at) &&
                after.blockedRay(attacker, target).exists(_._2.contains(moved)))(attacker -> target)
              if shield.nonEmpty then
                proof ++= before.sourceRefs(before.sources(attacker, List(target.at)))
                proof ++= after.sourceRefs(after.blockedRay(attacker, target).toList.map(_._1))
              shield
            }
          }.sortBy(_._2.at)
          val replyAt = paths.trackedPieceAfter(path, reply, through + 1).flatten
          val replyGuards = replyAt.toList.flatMap(p => end.controllers(p.at, p.side))
          proof ++= end.sourceRefs(replyGuards.flatMap(p => end.sources(p, replyAt.toList.map(_.at))))
          Arm(path, actor, moved, through, attacks, shields, pawns(after, square), pawns(end, square),
            end.occupant(square), paths.trackedPieceAfter(path, actor, through + 1).flatten,
            reply, replyAt, replyGuards)
      }
      arms match
        case List(Some(reference), Some(played)) if reference.guards.nonEmpty && played.guards.isEmpty &&
            reference.remainingGuards.nonEmpty && played.remainingGuards.isEmpty && played.attacks.nonEmpty =>
          val retained = paths.trackedPieceAfter(reference.path, played.actor, reference.through + 1).flatten
          Some(new Reading(paths, square, reference, played, retained,
            reference.path.line.ref == comparison.get.referenceLine,
            (proof.toList ++ primary.toList.map(_.id)).distinct))
        case _ => None
    }.nextOption()

  def answer(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths, pair: List[PreparedPath],
      reading: Reading, complete: Answer => Option[Answer]): Option[Answer] =
    if !reading.belongsTo(paths) || reading.arms.map(_.path).toSet != pair.toSet then return None
    val lead = Section(List(ReferencePurpose(reading)))
    val continuations = reading.arms.map { arm =>
      val refs = (0 to arm.through).map(MoveRef(arm.path, _)).toList
      val material = arm.path.certified.get._2.material.get
      val events = material.events.filter(_.plyOffset <= arm.through)
      val cost = CommentaryMaterialAccount.calculate(material, events.map(_.plyOffset),
        refs.map(_.offset), arm.path.moves.size)
      Section(List(Continuation(refs), ReferencePosition(reading, arm)) ++
        cost.map(Material(arm.path, _)).toList ++
        Option.when(events.nonEmpty && cost.isEmpty)(MaterialEvents(arm.path, events)).toList)
    }
    RootQuestionSummary.completePairAnswer(packet, paths, pair, "selected_comparison", lead :: continuations)
      .map(value => value.copy(evidence = (value.evidence ++ reading.evidence).distinct))
      .flatMap(complete).map(_.copy(details = Nil))
