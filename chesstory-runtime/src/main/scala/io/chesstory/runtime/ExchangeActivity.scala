package io.chesstory.runtime

import play.api.libs.json.*
import ObservedLineProtocol.{PreparedPath, PreparedPaths}
import MoveWorkProjection.{Piece, Position}
import CommentaryEnglish.at

/** An exchange ends a piece's life, not necessarily the positional question.
  * Track the contested opponents, the successor's actual work and the opponent's
  * resulting deployment separately. A shared square is not functional identity.
  */
private[runtime] object ExchangeActivity:
  final case class Task(targets: List[Piece], offers: List[String], openFile: Option[String]):
    def json = Json.obj("targets" -> targets.map(p => Json.obj("side" ->
      (if p.side.white then "white" else "black"), "role" -> p.role.name, "square" -> p.at)),
      "offers" -> offers, "open_file" -> openFile)
  final case class Retreat(original: Piece, current: Piece, exits: List[String],
      contested: List[(String, List[Piece])])
  final case class FileWork(actor: Piece, file: String, opponent: Option[Piece])
  final case class PawnWork(actor: Piece, targets: List[Piece], next: String, guards: List[Piece],
      continuing: Boolean, original: Piece, offset: Int)
  final case class State(end: Int, mover: Option[Piece], targets: List[(Piece, Option[Piece])],
      successor: Option[Piece], central: List[Piece], retreats: List[Retreat],
      file: Option[FileWork], pawn: Option[PawnWork], queensGone: Boolean)
  enum Continuity:
    case Deployment(opponent: Piece)
    case PawnPressure(square: String, targets: List[Piece], next: String)

  def continuity(left: State, right: State, side: chess.Color): Option[Continuity] =
    val deployment = left.retreats.find(a => right.retreats.exists(_.original == a.original))
      .filter(_ => left.central.nonEmpty && right.central.nonEmpty).map(r => Continuity.Deployment(r.original))
    deployment.orElse(for
      a <- left.pawn
      b <- right.pawn
      if a.actor.side == side && a.continuing && b.continuing && a.original == b.original &&
        a.actor == b.actor && a.targets == b.targets &&
        a.next == b.next && a.guards.nonEmpty && b.guards.nonEmpty
    yield Continuity.PawnPressure(a.actor.at, a.targets, a.next))

  private def central(piece: Piece) = Set("knight", "bishop")(piece.role.name) &&
    Set('c', 'd', 'e', 'f')(piece.at.head) &&
    (if piece.side.white then Set('4', '5', '6')(piece.at.last) else Set('3', '4', '5')(piece.at.last))

  def discover(move: String, original: Piece, before: Position, after: Position): Option[Task] =
    if !Set("knight", "bishop", "rook", "queen")(original.role.name) then return None
    val moved = after.occupant(move.slice(2, 4)).get
    val offers = after.view.legalRows.toList.flatten.filter(row =>
      row.capture.contains(moved) && row.actor.role == moved.role).map(_.move).sorted
    val targets = after.controlledBy(moved).toList.sorted.flatMap(after.occupant).filter(p =>
      p.side != original.side && p.role.name != "pawn" && p.role.name != "king" &&
        !before.controlledBy(original)(p.at))
    val file = Option.when(Set("rook", "queen")(moved.role.name) &&
      targets.exists(p => p.role == moved.role && p.at.head == moved.at.head) &&
      !after.controls.keys.exists(p => p.role.name == "pawn" && p.at.head == moved.at.head))(moved.at.head.toString)
    Option.when(offers.nonEmpty && targets.nonEmpty)(Task(targets, offers, file))

  /** Withdrawal of a newly challenged piece is the contrasting reply class.
    * Other captures can transpose into the same exchange: first-capture labels
    * cannot decide whether the longer task survives.
    */
  def challenges(task: Task, after: Position): List[String] =
    after.view.legalRows.toList.flatten.filter(row =>
      task.targets.contains(row.actor) && row.capture.isEmpty).map(_.move).sorted

  def settled(path: PreparedPath, use: Int): Int =
    val events = path.certified.flatMap(_._2.material).toList.flatMap(_.events)
    var end = use
    if events.exists(_.plyOffset == end + 1) then
      end += 1
      while events.exists(e => e.plyOffset == end + 1 && e.capture.exists(_.recapture)) do end += 1
    end

  /** The same pawn and the same target must persist. A replacement on either
    * square does not restore the old task. Include the root pawn move too. */
  def pawnWork(paths: PreparedPaths, path: PreparedPath, use: Int, end: Int): Option[PawnWork] =
    for
      before <- paths.purposePosition(path, 0)
      after <- paths.purposePosition(path, end + 1)
      work <- (0 to use).reverseIterator.flatMap { offset =>
        for
          at <- paths.purposePosition(path, offset + 1)
          p <- at.occupant(path.moves(offset).slice(2, 4)).filter(_.role.name == "pawn")
          original <- before.controls.keys.find(original =>
            paths.trackedPieceAfter(path, original, offset + 1).flatten.contains(p))
          targets = at.controlledBy(p).toList.sorted.flatMap(at.occupant).filter(t =>
            t.side != p.side && t.role.name == "pawn")
          if targets.nonEmpty
          next = p.at.head.toString + (p.at.last + (if p.side.white then 1 else -1)).toChar
          stableTargets = targets.forall(target => (0 to end + 1).forall(count =>
            paths.trackedPieceAfter(path, target, count).flatten.contains(target)))
        yield PawnWork(p, targets, next,
          if after.empty(next) then after.controllers(next, p.side).filter(_.role.name != "pawn") else Nil,
          stableTargets && (offset + 1 to end + 1).forall(count =>
            paths.trackedPieceAfter(path, original, count).flatten.contains(p)), original, offset)
      }.nextOption()
    yield work

  def state(paths: PreparedPaths, path: PreparedPath, original: Piece, task: Task, use: Int): Option[State] =
    val end = settled(path, use)
    for
      before <- paths.purposePosition(path, 0)
      after <- paths.purposePosition(path, end + 1)
    yield
      val own = after.controls.keys.toList.filter(_.side == original.side)
      val centres = own.filter(central).sortBy(_.at)
      val retreats = before.controls.keys.toList.filter(p =>
        task.targets.contains(p) && Set("knight", "bishop")(p.role.name)).flatMap { p =>
        paths.trackedPieceAfter(path, p, end + 1).flatten.filter(current =>
          current.at != p.at && (if p.side.white then current.at.last <= '2' && p.at.last > '2'
          else current.at.last >= '7' && p.at.last < '7')).map { current =>
          val exits = after.controlledBy(current).toList.filter(s =>
            after.empty(s) && (if current.side.white then s.last > current.at.last else s.last < current.at.last))
            .sortBy(s => (if Set('c', 'd', 'e', 'f')(s.head) then 0 else 1, s))
          val contested = exits.flatMap(square => Option.when(after.controllers(square, !p.side).nonEmpty)(
            square -> after.controllers(square, !p.side)))
          Retreat(p, current, exits.filterNot(s => contested.exists(_._1 == s)), contested)
        }
      }.sortBy(_.current.at)
      // Observed rook use of a pawn-free file is activity; it need not be the
      // original mover. Opposing heavy pieces mean a contest, not ownership.
      val file = (2 to use by 2).reverseIterator.flatMap { offset =>
        val move = path.moves(offset)
        after.occupant(move.slice(2, 4)).filter(p => p.side == original.side && p.role.name == "rook" &&
          !after.controls.keys.exists(pawn => pawn.role.name == "pawn" && pawn.at.head == p.at.head) &&
          after.controlledBy(p).count(_.head == p.at.head) >= 3).map { p =>
          FileWork(p, p.at.head.toString, after.controlledBy(p).toList.sorted.flatMap(after.occupant)
            .find(enemy => enemy.side != p.side && Set("rook", "queen")(enemy.role.name) && enemy.at.head == p.at.head))
        }
      }.nextOption()
      // Record the pawn action that actually reaches a pawn contact, including
      // an opponent's break. A planned f-pawn push cannot be credited after the
      // opponent has already changed that structure.
      val pawn = pawnWork(paths, path, use, end)
      val square = path.moves.head.slice(2, 4)
      State(end, paths.trackedPieceAfter(path, original, end + 1).flatten,
        task.targets.map(p => p -> paths.trackedPieceAfter(path, p, end + 1).flatten),
        after.occupant(square).filter(p => p.side == original.side && p.role.name != "pawn"),
        centres, retreats, file, pawn, !after.controls.keys.exists(_.role.name == "queen"))

  def use(paths: PreparedPaths, path: PreparedPath, original: Piece, task: Task): Option[Int] =
    val candidates = (2 until math.min(path.moves.size, 14) by 2).toList.flatMap { offset =>
      state(paths, path, original, task, offset).map(offset -> _)
    }
    candidates.filter { (_, s) => s.file.nonEmpty || s.retreats.nonEmpty && s.central.nonEmpty ||
      s.pawn.nonEmpty }.sortBy { (offset, s) =>
      (if s.retreats.nonEmpty && s.central.nonEmpty then 0 else if s.file.nonEmpty then 1 else 2, offset)
    }.headOption.orElse(candidates.find(_._1 >= 6)).orElse(candidates.lastOption).map(_._1)
