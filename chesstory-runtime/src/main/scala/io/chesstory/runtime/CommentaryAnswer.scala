package io.chesstory.runtime

import lila.chessjudgment.model.line.AutomaticTerminal
import lila.chessjudgment.model.judgment.{ObservedLineMaterialEvent, RelationWitnessDetail}

/** Selected, already verified chess content. This contains no prose and never
  * searches, scores a move, or discovers an additional board relationship.
  */
private[runtime] object CommentaryAnswer:
  import RootQuestionSummary.MoveRef
  import MoveWorkProjection.{Detail, Piece}
  import ObservedLineProtocol.PreparedPath

  sealed trait Statement:
    def references: List[MoveRef]
    def covered: List[MoveRef] = Nil

  final case class Action(move: MoveRef, detail: Detail) extends Statement:
    def references = List(move)

  final case class Material(path: PreparedPath, cost: CommentaryMaterialAccount.Cost) extends Statement:
    def references = cost.offsets.map(MoveRef(path, _))
    override def covered = cost.events.map(event => MoveRef(path, event.plyOffset))

  final case class MaterialEvents(path: PreparedPath, events: List[ObservedLineMaterialEvent]) extends Statement:
    def references = events.map(event => MoveRef(path, event.plyOffset))
    override def covered = events.filter(_.capture.nonEmpty).map(event => MoveRef(path, event.plyOffset))

  final case class PieceExchange(path: PreparedPath, events: List[LineContinuationInvestigation.Event],
      recaptureOffsets: Set[Int]) extends Statement:
    def references = events.map(event => MoveRef(path, event.plyOffset))
    override def covered = events.filter(_.originalVictim.nonEmpty).map(event => MoveRef(path, event.plyOffset))

  final case class TargetResponse(path: PreparedPath, start: Int, target: Piece,
      originalAttacker: Piece, originalTarget: Piece,
      escape: Option[Int], material: Option[Material], survives: Boolean,
      targetCapture: Option[PieceExchange] = None, attackerRemoval: Option[PieceExchange] = None,
      targetEnd: Option[PiecePresence] = None) extends Statement:
    def introduction = (if start == 0 then List(0) else if start == 2 then List(0, 1, 2)
      else List(0, start)).map(MoveRef(path, _))
    def references = (introduction ++ escape.toList.map(MoveRef(path, _)) ++
      material.toList.flatMap(_.references) ++ targetCapture.toList.flatMap(_.references) ++
      attackerRemoval.toList.flatMap(_.references) ++ targetEnd.toList.flatMap(_.references)).distinct
    // The outcome identifies the pieces; the complete material account owns
    // its captures and the replies on which the stated cost depends.
    override def covered = material.toList.flatMap(_.covered)

  final case class RouteUse(move: MoveRef, use: MoveRef, role: String,
      opening: Option[Detail] = None) extends Statement:
    def references = List(move, use)

  final case class AttackerRemovalOrSupport(removal: MoveRef, supportMove: MoveRef,
      attacker: Piece, target: Piece, provider: Piece) extends Statement:
    def references = List(removal, supportMove)

  final case class SupportRecapture(after: MoveRef, witness: PreparedPath,
      use: LineContinuationInvestigation.DirectSupportUse) extends Statement:
    def references = after :: List(use.targetCapture.plyOffset, use.recapture.plyOffset).map(MoveRef(witness, _))

  final case class SupportTransfer(move: MoveRef, provider: Piece, target: Piece,
      oldProvider: Piece, originalOldProvider: Piece, departure: MoveRef,
      use: LineContinuationInvestigation.DirectSupportUse) extends Statement:
    def references = List(move, departure) ++
      List(use.targetCapture.plyOffset, use.recapture.plyOffset).map(MoveRef(move.path, _))

  final case class SupportedCapture(move: MoveRef, provider: Piece, target: Piece,
      use: LineContinuationInvestigation.DirectSupportUse, fromStart: Boolean,
      arrival: Option[MoveRef], arrivalCapture: Option[Detail]) extends Statement:
    def references = (List(move) ++ arrival.toList ++
      List(use.targetCapture.plyOffset, use.recapture.plyOffset).map(MoveRef(move.path, _))).distinct

  final case class SupportRetention(move: MoveRef, originalProvider: Piece, provider: Piece,
      originalTarget: Piece, target: Piece, originalOldProvider: Piece, oldProvider: Piece,
      departure: MoveRef, currentTarget: Piece) extends Statement:
    def references = List(move, departure)

  final case class BlockedLineTransfer(move: MoveRef, originalBlocker: Piece, blocker: Piece,
      originalSlider: Piece, slider: Piece, departure: MoveRef, arrival: MoveRef, entrant: Piece) extends Statement:
    def references = List(move, departure, arrival).distinct

  final case class BlockedAttack(after: MoveRef, attacker: Piece, target: Piece, blocker: Piece) extends Statement:
    def references = List(after)

  final case class ClearedRoute(move: MoveRef, before: Piece, mover: Piece, originalUser: Piece,
      use: MoveRef, user: Piece, targets: List[Piece], passage: Boolean,
      development: Option[Detail]) extends Statement:
    def references = List(move, use)

  final case class SquareReoccupation(move: MoveRef, originalActor: Piece, actor: Piece,
      square: String, use: MoveRef, originalEntry: Piece, entry: Piece) extends Statement:
    def references = List(move, use)

  final case class SupportedPawn(original: Piece, current: Piece, route: List[MoveRef])

  final case class PawnAdvanceOption(move: MoveRef, pawn: Piece, alternative: MoveRef,
      advancedPawn: Piece, square: String, use: Option[MoveRef], supported: Option[SupportedPawn]) extends Statement:
    def references = List(move, alternative) ++ use.toList ++ supported.toList.flatMap(_.route)

  final case class PawnCaptureChoice(move: MoveRef, capturer: Piece, target: Piece, pawn: Piece,
      arrived: Piece, opposition: List[Piece], alternative: MoveRef) extends Statement:
    def references = List(move, alternative)

  final case class PawnAdvanceReply(after: MoveRef, pawn: Piece, advance: MoveRef) extends Statement:
    def references = List(after, advance)

  final case class RelativeAlignment(move: MoveRef, actor: Piece, front: Piece, rear: Piece,
      retained: List[MoveRef], ended: Option[MoveRef]) extends Statement:
    def references = List(move) ++ retained ++ ended.toList

  final case class ReleasedCapture(move: MoveRef, king: Piece, pinner: Piece, actor: Piece,
      target: Piece, reply: Option[MoveRef]) extends Statement:
    def references = move :: reply.toList

  sealed trait CheckEvent extends Statement:
    def move: MoveRef
    def references = List(move)

  final case class CheckingMove(move: MoveRef, detail: RelationWitnessDetail.CreatedCheckResponseInventory,
      openedCheckers: List[Piece]) extends CheckEvent

  final case class CheckReply(move: MoveRef, detail: RelationWitnessDetail.RootCheckResponse) extends CheckEvent

  final case class CheckSequence(move: MoveRef, events: List[CheckEvent]) extends Statement:
    def references = (move :: events.map(_.move)).distinct

  final case class Continuation(moves: List[MoveRef]) extends Statement:
    def references = moves

  final case class NoMaterialChange(root: MoveRef, end: MoveRef) extends Statement:
    def references = List(root, end).distinct

  final case class PieceRoute(path: PreparedPath, original: Piece, offsets: List[Int]) extends Statement:
    def references = offsets.map(MoveRef(path, _))

  final case class ControlState(move: MoveRef, actor: Piece, target: Option[Piece], square: String,
      present: Boolean, remaining: Option[List[Piece]] = None) extends Statement:
    def references = List(move)

  final case class CheckStatus(move: MoveRef, king: Piece, checking: Boolean,
      reply: Option[MoveRef] = None) extends Statement:
    def references = move :: reply.toList

  final case class RecordedEnd(move: MoveRef) extends Statement:
    def references = List(move)

  final case class PiecePresence(after: MoveRef, original: Piece, current: Piece) extends Statement:
    def references = List(after)

  final case class AttackWithdrawal(move: MoveRef, before: Piece, after: Piece, target: Piece) extends Statement:
    def references = List(move)

  final case class ControlContrast(move: MoveRef, actor: Piece, target: Option[Piece], square: String,
      present: Boolean, alternative: MoveRef, alternativeActor: Piece, alternativeTarget: Option[Piece],
      alternativeSquare: String, alternativePresent: Boolean, remaining: List[Piece] = Nil,
      reply: Option[MoveRef] = None) extends Statement:
    def references = List(move, alternative) ++ reply.toList

  final case class Terminal(move: MoveRef, result: AutomaticTerminal) extends Statement:
    def references = List(move)

  final case class Convergence(left: List[MoveRef], right: List[MoveRef], fullState: Boolean) extends Statement:
    def references = left ++ right

  final case class ExchangeRetention(trade: List[MoveRef], retained: List[MoveRef],
      role: String, recapturer: String) extends Statement:
    def references = trade ++ retained

  final case class Divergence(leftRoot: MoveRef, rightRoot: MoveRef,
      left: Option[MoveRef], right: Option[MoveRef]) extends Statement:
    def references = List(leftRoot, rightRoot) ++ left.toList ++ right.toList

  final case class Focus(move: MoveRef, note: MoveWorkProjection.LocalCandidate, phase: String = "after")

  final case class Section(statements: List[Statement], required: List[MoveRef] = Nil,
      focus: List[Focus] = Nil):
    def references = statements.flatMap(_.references).distinct
    def covered = statements.flatMap(_.covered).distinct

  final case class Answer(question: String, core: List[Section], details: List[Section], evidence: List[String]):
    def required = core.flatMap(section => section.required ++ section.references).distinct
