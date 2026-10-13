package io.chesstory.runtime

import lila.chessjudgment.model.line.AutomaticTerminal
import lila.chessjudgment.model.judgment.{ObservedLineMaterialEvent, RelationColoredPieceWitness, RelationWitnessDetail}

/** Selected, already verified chess content. This contains no prose and never
  * searches, scores a move, or discovers an additional board relationship.
  */
private[runtime] object CommentaryAnswer:
  import RootQuestionSummary.MoveRef
  import MoveWorkProjection.{Detail, Piece}
  import ObservedLineProtocol.PreparedPath

  sealed trait Statement:
    def references: List[MoveRef]
    // A position witness proves content without selecting its last transition
    // as another action whose capture and reply must be explained.
    def actionReferences: List[MoveRef] = references
    def covered: List[MoveRef] = Nil

  final case class Action(move: MoveRef, detail: Detail) extends Statement:
    def references = List(move)

  final case class ReferencePurpose(value: ReferencePurposeReading.Reading) extends Statement:
    def references = value.arms.map(arm => MoveRef(arm.path, 0))

  final case class ReferencePosition(value: ReferencePurposeReading.Reading,
      arm: ReferencePurposeReading.Arm) extends Statement:
    def references = List(MoveRef(arm.path, arm.through))
    override def actionReferences = Nil

  final case class FunctionPurpose(value: MovePurpose.Reading) extends Statement:
    override def actionReferences = List(MoveRef(value.question.path, 0)) ++ value.question.plan.toList.flatMap(plan =>
      (0 to plan.through).map(MoveRef(plan.path, _)) ++
        (0 until plan.join).map(MoveRef(value.question.path, _)))
    def references = actionReferences ++
      (if value.activityContinuity.nonEmpty then value.branches.map(branch => MoveRef(branch.path, branch.through)) else Nil)

  final case class FunctionReply(value: MovePurpose.Reading, branch: MovePurpose.Branch) extends Statement:
    def references = List(MoveRef(branch.path, 1), MoveRef(branch.path, branch.use), MoveRef(branch.path, branch.through)).distinct

  final case class ChoicePurpose(value: PurposeChoice.Reading) extends Statement:
    def references = value.arms.map(arm => MoveRef(arm.path, 0))
  final case class ChoiceReply(value: PurposeChoice.Reading, arm: PurposeChoice.Arm) extends Statement:
    def references = (List(MoveRef(arm.path, 0), MoveRef(arm.path, arm.through)) ++
      arm.constrained.toList.map((offset, _) => MoveRef(arm.path, offset))).distinct
    override def actionReferences = Nil
  final case class ChoiceNeutralization(value: PurposeChoice.Neutralization) extends Statement:
    def references = List(MoveRef(value.alternative.path, value.alternative.through),
      MoveRef(value.reply.path, 1), MoveRef(value.reply.path, value.reply.through)).distinct
    override def actionReferences = Nil

  final case class Material(path: PreparedPath, cost: CommentaryMaterialAccount.Cost) extends Statement:
    def references = cost.offsets.map(MoveRef(path, _))
    override def covered = cost.events.map(event => MoveRef(path, event.plyOffset))

  /** An overview of captures already owned by the core's complete episodes.
    * It does not own them again or select their moves as additional actions.
    * None is a certified empty ledger, never an unobserved response. A through
    * endpoint bounds a complete prefix; it does not price the rest of the line. */
  final case class MaterialBalance(path: PreparedPath, cost: Option[CommentaryMaterialAccount.Cost],
      through: Option[Int] = None) extends Statement:
    def references = (List(MoveRef(path, 0)) ++ cost.toList.flatMap(_.offsets.map(MoveRef(path, _))) ++
      Option.when(cost.isEmpty)(MoveRef(path, path.moves.size - 1)).toList).distinct
    override def actionReferences = Nil

  final case class MaterialEvents(path: PreparedPath, events: List[ObservedLineMaterialEvent]) extends Statement:
    def references = events.map(event => MoveRef(path, event.plyOffset))
    override def covered = events.filter(_.capture.nonEmpty).map(event => MoveRef(path, event.plyOffset))

  final case class PieceExchange(path: PreparedPath, events: List[LineContinuationInvestigation.Event],
      recaptureOffsets: Set[Int]) extends Statement:
    def references = events.map(event => MoveRef(path, event.plyOffset))
    override def covered = events.filter(_.originalVictim.nonEmpty).map(event => MoveRef(path, event.plyOffset))

  final case class EnPassantCapture(advance: MoveRef, capture: MoveRef, originalPawn: Piece,
      originalCapturer: Piece, captured: Piece, destination: String) extends Statement:
    def references = List(advance, capture)
    override def covered = List(capture)

  final case class TargetResponse(path: PreparedPath, start: Int, target: Piece,
      originalAttacker: Piece, originalTarget: Piece,
      escape: Option[Int], material: Option[Material], survives: Boolean,
      targetCapture: Option[PieceExchange] = None, attackerRemoval: Option[PieceExchange] = None,
      targetEnd: Option[PiecePresence] = None) extends Statement:
    def introduction = (if start == 0 then List(0) else if start == 2 then List(0, 1, 2)
      else List(0, start)).map(MoveRef(path, _))
    override def actionReferences = (introduction ++ escape.toList.map(MoveRef(path, _)) ++
      material.toList.flatMap(_.references) ++ targetCapture.toList.flatMap(_.references) ++
      attackerRemoval.toList.flatMap(_.references)).distinct
    def references = (actionReferences ++ targetEnd.toList.flatMap(_.references)).distinct
    // The outcome identifies the pieces; the complete material account owns
    // its captures and the replies on which the stated cost depends.
    override def covered = material.toList.flatMap(_.covered)

  /** The same original target in a provided comparison. Removal and recorded
    * survival are observations, not a claim that the root caused a later loss.
    */
  final case class TargetOutcome(root: MoveRef, original: Piece,
      removal: Option[PieceExchange], departure: Option[MoveRef],
      retained: Option[PiecePresence]) extends Statement:
    override def actionReferences = (List(root) ++ removal.toList.flatMap(_.references) ++ departure.toList).distinct
    def references = (actionReferences ++ retained.toList.flatMap(_.references)).distinct
    override def covered = removal.toList.flatMap(_.covered)

  final case class RouteUse(move: MoveRef, use: MoveRef, role: String,
      opening: Option[Detail] = None) extends Statement:
    def references = List(move, use)

  final case class AttackerRemovalOrSupport(removal: MoveRef, supportMove: MoveRef,
      attacker: Piece, target: Piece, provider: Piece) extends Statement:
    def references = List(removal, supportMove)

  // A support use says what was captured, but does not explain promotion.
  // Keep a capture-and-promotion event in the explicit material explanation.
  private def supportCaptures(path: PreparedPath, use: LineContinuationInvestigation.DirectSupportUse): List[MoveRef] =
    val offsets = Set(use.targetCapture.plyOffset, use.recapture.plyOffset)
    path.certified.flatMap(_._2.material).toList.flatMap(_.events).filter(event =>
      offsets(event.plyOffset) && event.capture.nonEmpty && event.promotionGainCp == 0).map(event => MoveRef(path, event.plyOffset))

  final case class SupportRecapture(after: MoveRef, witness: PreparedPath,
      use: LineContinuationInvestigation.DirectSupportUse) extends Statement:
    def references = after :: List(use.targetCapture.plyOffset, use.recapture.plyOffset).map(MoveRef(witness, _))
    override def covered = supportCaptures(witness, use)

  final case class SupportTransfer(move: MoveRef, provider: Piece, target: Piece,
      oldProvider: Piece, originalOldProvider: Piece, departure: MoveRef,
      use: LineContinuationInvestigation.DirectSupportUse) extends Statement:
    def references = List(move, departure) ++
      List(use.targetCapture.plyOffset, use.recapture.plyOffset).map(MoveRef(move.path, _))
    override def covered = supportCaptures(move.path, use)

  final case class SupportedCapture(move: MoveRef, provider: Piece, target: Piece,
      use: LineContinuationInvestigation.DirectSupportUse, fromStart: Boolean,
      arrival: Option[MoveRef], arrivalCapture: Option[Detail]) extends Statement:
    def references = (List(move) ++ arrival.toList ++
      List(use.targetCapture.plyOffset, use.recapture.plyOffset).map(MoveRef(move.path, _))).distinct
    override def covered = supportCaptures(move.path, use)

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

  /** Identify a separate recorded forecast without selecting its prefix as
    * another action or using its old values to price a current decision. */
  final case class RecordedContext(moves: List[MoveRef]) extends Statement:
    def references = moves
    override def actionReferences = Nil

  final case class NoMaterialChange(root: MoveRef, end: MoveRef) extends Statement:
    def references = List(root, end).distinct

  final case class PieceRoute(path: PreparedPath, original: Piece, offsets: List[Int]) extends Statement:
    def references = offsets.map(MoveRef(path, _))

  final case class ControlState(move: MoveRef, actor: Piece, target: Option[Piece], square: String,
      present: Boolean, remaining: Option[List[Piece]] = None) extends Statement:
    def references = List(move)

  final case class ResourceDecision(value: FunctionalChoiceReading.Decision) extends Statement:
    def references = (value.sequence ++ value.other.toList ++
      value.conditions.flatMap(_.references) ++ value.useRoles.flatMap(_.references) ++
      value.replyRoles.flatMap(_.references)).distinct

  final case class FirstReplyChoice(root: MoveRef, preferred: MoveRef, alternative: MoveRef,
      quality: PrincipalVariationComparison.QualityBand) extends Statement:
    def references = List(root, preferred, alternative)
    override def actionReferences = Nil

  final case class ExchangeFunction(value: ExchangeFunctionReading.Checked) extends Statement:
    def references = (List(0, 1, 2).map(MoveRef(value.capture, _)) ++ value.access.toList.map(_.after) ++
      value.pawnFile.toList.map(file => MoveRef(file.decline, 1))).distinct

  final case class RetainedExchangeFunction(value: ExchangeFunctionReading.Checked) extends Statement:
    def references = value.references
    override def actionReferences = value.kept.toList.flatMap(role => (0 to role.use).map(MoveRef(role.path, _)))

  /** A later observed use has its own current supporters and a complete local
    * conditional capture price. This is factual detail, not another score or
    * a claim that the original support arrangement caused the root judgment. */
  final case class FunctionRealization(value: FunctionalRealizationReading.Checked) extends Statement:
    def references = value.references
    override def actionReferences = value.priceReferences
    override def covered = value.covered

  final case class AnchoredDecision(value: AnchoredChoiceAnswer.Reading) extends Statement:
    def references = value.references

  /** The complete current control inventory of a threatened ally. This says
    * who attacks/supports it, never that a capture is forced or uncompensated. */
  final case class TargetDefense(move: MoveRef, target: Piece, attackers: List[Piece],
      defenders: List[Piece]) extends Statement:
    def references = List(move)

  /** The same first attack tests a square that the root occupied. The child
    * receipt compares two legal retreats in one position; it does not turn
    * the opponent's reply into a forced move or the whole evaluation cause. */
  final case class ReplyConstraint(actual: PreparedPath, alternative: PreparedPath,
      available: PreparedPath, otherwise: PreparedPath, proof: CommentaryIdea.ReplySpace) extends Statement:
    def references = List(MoveRef(actual, 0), MoveRef(actual, 1), MoveRef(actual, 2),
      MoveRef(alternative, 0), MoveRef(available, 1), MoveRef(available, 2), MoveRef(otherwise, 2))

  enum ReserveReply:
    case PassedPawn(pawn: Piece, supported: Boolean)
    case LastPawnWait(waitOffset: Int, square: String, counterExchange: Boolean = false)

  final case class ReserveEntry(actual: PreparedPath, alternative: PreparedPath,
      waited: PreparedPath, counter: PreparedPath, reply: ReserveReply,
      proof: CommentaryIdea.ReserveEntry, available: Option[PreparedPath] = None) extends Statement:
    def entry = available.getOrElse(waited)
    def sequence = (proof.first to proof.last).toList.map(MoveRef(waited, _))
    def references = List(MoveRef(actual, 0), MoveRef(alternative, 0)) ++ sequence ++
      List(MoveRef(waited, proof.last + 1), MoveRef(entry, proof.last + 2),
        MoveRef(counter, proof.last + 1)) ++ (reply match
        case ReserveReply.PassedPawn(_, _) => List(MoveRef(counter, proof.last + 2))
        case ReserveReply.LastPawnWait(offset, _, exchange) =>
          Option.when(exchange)(MoveRef(counter, proof.last + 2)).toList ++
            List(MoveRef(counter, offset), MoveRef(counter, offset + 1)))

  final case class ReserveHold(actual: PreparedPath, alternative: PreparedPath,
      waited: PreparedPath, retreat: PreparedPath, square: String,
      proof: CommentaryIdea.ReserveHold) extends Statement:
    def references = List(MoveRef(actual, 0), MoveRef(alternative, 0),
      MoveRef(waited, proof.offset), MoveRef(retreat, proof.offset), MoveRef(retreat, proof.offset + 1))

  final case class DirectReserveEntry(actual: PreparedPath, alternative: PreparedPath,
      waited: PreparedPath, entry: PreparedPath, otherwise: PreparedPath,
      proof: CommentaryIdea.DirectReserveEntry) extends Statement:
    def references = List(MoveRef(actual, 0), MoveRef(alternative, 0),
      MoveRef(entry, proof.offset), MoveRef(entry, proof.offset + 1), MoveRef(entry, proof.offset + 2),
      MoveRef(otherwise, proof.offset + 2))

  /** A real king route restores the piece placement with the other side to
    * move. The same-position comparison separately tests spending the pawn
    * now; the route is witnessed, never asserted to be forced. */
  final case class ReserveTempo(actual: PreparedPath, alternative: PreparedPath,
      spent: PreparedPath, kept: PreparedPath, route: PreparedPath, through: Int,
      pawn: Piece, proof: CommentaryIdea.ReserveTiming) extends Statement:
    def sequence = (2 until through).toList.map(MoveRef(route, _))
    def references = (List(MoveRef(actual, 0), MoveRef(alternative, 0), MoveRef(route, 1),
      MoveRef(spent, 2), MoveRef(kept, 2)) ++ sequence).distinct

  /** The original pawn used to guard the breakthrough square. A real pawn
    * break exploits the lost guard; the same tested reply in
    * the other root arm retains it. Neither line is asserted to be forced. */
  final case class ReserveBreak(actual: PreparedPath, alternative: PreparedPath,
      held: PreparedPath, otherReply: PreparedPath, offset: Int, guard: Piece, passer: Piece,
      proof: CommentaryIdea.ReserveResponse) extends Statement:
    def sequence = (1 to offset).toList.map(MoveRef(actual, _))
    def retainedSequence = List(MoveRef(held, 1))
    def references = (List(MoveRef(actual, 0), MoveRef(alternative, 0), MoveRef(otherReply, 1)) ++
      sequence ++ retainedSequence).distinct

  final case class CheckStatus(move: MoveRef, king: Piece, checking: Boolean,
      reply: Option[MoveRef] = None) extends Statement:
    def references = move :: reply.toList

  /** An independently replayed conditional resource, not another forecast.
    * The option is legal in this exact position; it is neither an observed
    * engine choice nor a claim about its value or the root's whole score. */
  final case class LegalReplyOption(reference: PreparedPath, alternative: PreparedPath,
      available: PreparedPath, proof: FunctionalReplyReading.Proof, price: Option[PreparedPath] = None,
      remainingCapturers: Option[List[RelationColoredPieceWitness]] = None) extends Statement:
    def referenceSequence = proof.reference.indices.toList.map(MoveRef(reference, _))
    def alternativeSequence = proof.alternative.indices.toList.map(MoveRef(alternative, _))
    def option = MoveRef(available, proof.option.size - 1)
    def priceMove = price.flatMap(path => proof.price.map(moves => MoveRef(path, moves.size - 1)))
    def references = (referenceSequence ++ alternativeSequence ++ List(option) ++ priceMove.toList).distinct
    override def actionReferences = Nil

  final case class RecordedEnd(move: MoveRef) extends Statement:
    def references = List(move)

  final case class PiecePresence(after: MoveRef, original: Piece, current: Piece) extends Statement:
    def references = List(after)
    override def actionReferences = Nil

  final case class PieceAbsence(after: MoveRef, original: Piece) extends Statement:
    def references = List(after)
    override def actionReferences = Nil

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

  /** The original pieces shared by the two observed alternatives. A subject
    * names the question; it neither asserts a forced reply nor a total price. */
  final case class ComparisonSubject(roots: List[MoveRef], originals: List[Piece]) extends Statement:
    def references = roots
    // Naming the question does not describe either root's capture or reply.
    // The core delivery boundary independently requires both actual roots.
    override def actionReferences = Nil

  /** Existing condition/use receipts are distinct from a route or position
    * history. Their actual use cannot be evicted by a different target story. */
  def conditionUses(statement: Statement): List[(MoveRef, MoveRef)] = statement match
    case value: ReserveBreak => List(MoveRef(value.actual, 0) -> MoveRef(value.actual, value.offset))
    case value: ReserveTempo => List(MoveRef(value.alternative, 0) -> MoveRef(value.route, value.through - 1))
    case value: ReserveHold => List(MoveRef(value.alternative, 0) -> MoveRef(value.waited, value.proof.offset))
    case value: ReserveEntry => List(MoveRef(value.entry, value.proof.last) -> MoveRef(value.entry, value.proof.last + 2)) ++
      (value.reply match
        case ReserveReply.LastPawnWait(offset, _, _) => List(MoveRef(value.counter, offset) -> MoveRef(value.counter, offset + 1))
        case _ => Nil)
    case value: DirectReserveEntry => List(MoveRef(value.entry, value.proof.offset) -> MoveRef(value.entry, value.proof.offset + 2))
    case value: RouteUse => List(value.move -> value.use)
    case value: ClearedRoute => List(value.move -> value.use)
    case value: SquareReoccupation => List(value.move -> value.use)
    case value: BlockedLineTransfer => List(value.move -> value.arrival)
    case value: SupportRecapture => List(value.after -> MoveRef(value.witness, value.use.recapture.plyOffset))
    case value: SupportedCapture => List(value.move -> MoveRef(value.move.path, value.use.recapture.plyOffset))
    case value: SupportTransfer => List(value.move -> MoveRef(value.move.path, value.use.recapture.plyOffset))
    case value: SupportRetention => List(value.move -> value.departure)
    case value: PawnAdvanceOption => value.use.toList.map(value.move -> _)
    case value: EnPassantCapture => List(value.advance -> value.capture)
    case _ => Nil

  final case class Focus(move: MoveRef, note: MoveWorkProjection.LocalCandidate, phase: String = "after")

  final case class Section(statements: List[Statement], required: List[MoveRef] = Nil,
      focus: List[Focus] = Nil):
    def references = statements.flatMap(_.references).distinct
    def actionReferences = statements.flatMap(_.actionReferences).distinct
    def covered = statements.flatMap(_.covered).distinct

  final case class ComparisonScope(subjects: Set[Piece], protectedActions: Set[MoveRef] = Set.empty)

  final case class Answer(question: String, core: List[Section], details: List[Section], evidence: List[String],
      comparison: Option[ComparisonScope] = None, completionSources: Option[List[Statement]] = None):
    def required = core.flatMap(section => section.required ++ section.references).distinct
