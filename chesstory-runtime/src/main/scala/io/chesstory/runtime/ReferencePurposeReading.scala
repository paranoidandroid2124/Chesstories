package io.chesstory.runtime

import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.CandidateLineEvaluation
import ObservedLineProtocol.{PreparedPath, PreparedPaths}
import MoveWorkProjection.{Piece, Position}
import CommentaryAnswer.*
import CommentaryEnglish.at
import RootQuestionSummary.MoveRef

/** The two assessed moves own both the question and its positional aftermath.
  * Follow physical pieces through a closed exchange, including a delayed copy
  * of the other root's action. This is not a decomposition of the engine score.
  */
private[runtime] object ReferencePurposeReading:
  final case class Protection(target: Piece, attackers: List[Piece], afterReply: Boolean = false)
  final case class Function(original: Piece, current: Option[Piece], attacks: List[Piece],
      supports: List[Piece], openFile: Option[String])
  final case class PawnChange(original: Piece, current: Piece, guards: List[Piece],
      doubled: List[Piece], isolated: Boolean, passed: Option[Boolean])
  final case class SharedCapture(actor: Piece, victim: Piece, referenceOffset: Int, playedOffset: Int)
  final case class FunctionContrast(reference: Function, played: Function)
  final case class SupportedArrival(original: Piece, target: Piece, offset: Int)
  final case class ControlUse(path: PreparedPath, controller: Piece, change: Int, removed: Boolean,
      target: Option[Piece], originalUser: Piece, user: Piece, use: Int, remaining: List[Piece],
      evidence: List[String]):
    def through = ExchangeActivity.settled(path, use)
  final case class ReplyPressure(actor: Piece, targets: List[Piece], alignments: List[(Piece, Piece)],
      identities: List[(Piece, Piece)])
  final case class ReplyActivity(control: Option[ControlUse], activity: PurposeActivityReading.Reading):
    def through = activity.through
    def references = activity.references ++ control.toList.flatMap(use =>
      List(MoveRef(use.path, use.change), MoveRef(use.path, use.use)))
  final case class ReplyDefense(entry: MoveRef, capture: MoveRef, controller: Piece):
    def references = List(MoveRef(entry.path, 0), entry, capture)
  final case class CheckEvasion(king: Piece, checkers: List[Piece], kind: String)
  final case class CastlePreparation(original: Piece, offset: Int, kingSide: Boolean,
      blocked: List[(Piece, Piece, List[String])])
  final case class Arm(path: PreparedPath, actor: Piece, moved: Piece, through: Int,
      attacks: List[Piece], shields: List[(Piece, Piece)], guards: List[Piece],
      remainingGuards: List[Piece], occupant: Option[Piece],
      retainedMover: Option[Piece], reply: Piece, replyAt: Option[Piece], replyGuards: List[Piece],
      captured: Option[Piece], protections: List[Protection], replyCapture: Option[Piece],
      replyEvades: Boolean, replyShields: List[Piece], escaped: List[Piece],
      functions: List[Function], pawns: List[PawnChange], losses: List[Piece] = Nil,
      supportUse: Option[LineContinuationInvestigation.DirectSupportUse] = None,
      supportedArrival: Option[SupportedArrival] = None,
      rookFile: Option[String] = None, replyPressure: Option[ReplyPressure] = None,
      exchange: Option[ExchangeActivity.Task] = None, pawnWork: Option[ExchangeActivity.PawnWork] = None,
      releasedGuards: List[Piece] = Nil, controlUse: Option[ControlUse] = None,
      activity: Option[PurposeActivityReading.Reading] = None, counterplay: Option[ReplyActivity] = None,
      evasion: Option[CheckEvasion] = None, castlePreparation: Option[CastlePreparation] = None,
      deferredCastle: Option[CastlePreparation] = None)
  final class Reading private[ReferencePurposeReading](private val owner: PreparedPaths,
      val supportedPost: Option[String], val reference: Arm, val played: Arm,
      val retainedChoice: Option[Piece], val sharedCapture: Option[SharedCapture],
      val contrasts: List[FunctionContrast], val rootUses: List[Statement], val evidence: List[String],
      val preparation: Option[MoveIntent.Reading], val functional: Option[MovePurpose.Reading],
      val conditionalPurposes: List[LegalReplyOption] = Nil):
    def belongsTo(paths: PreparedPaths) = owner eq paths
    def physicalLosses(price: Material): Option[List[Piece]] =
      val ends = arms.filter(_.path eq price.path).map(_.through) ++
        preparation.toList.flatMap(_.branches.filter(_.path eq price.path).map(_.through))
      ends.find(n => price.cost.offsets.max == n && (0 to n).forall(price.cost.offsets.contains) &&
        price.path.certified.flatMap(_._2.material).exists(_.events.filter(_.plyOffset <= n) == price.cost.events))
        .flatMap(_ => CommentaryMaterialAccount.physicalLosses(owner, price))
    def arms = List(reference, played)
    def replyDefense(arm: Arm): Option[ReplyDefense] = arm.counterplay.flatMap(_.control).flatMap { use =>
      arms.filterNot(_ eq arm).iterator.flatMap { other =>
        (1 until other.through by 2).iterator.flatMap { offset =>
          for
            user <- owner.trackedPieceAfter(other.path, use.originalUser, offset).flatten
            arrived <- owner.trackedPieceAfter(other.path, use.originalUser, offset + 1).flatten
            if other.path.moves(offset).take(2) == user.at && arrived == use.user
            controller <- owner.trackedPieceAfter(other.path, use.controller, offset + 1).flatten
            at <- owner.purposePosition(other.path, offset + 1)
            row <- at.view.legalRows.flatMap(_.find(_.move == other.path.moves(offset + 1)))
            if row.actor == controller && row.capture.contains(arrived)
          yield ReplyDefense(MoveRef(other.path, offset), MoveRef(other.path, offset + 1), controller)
        }
      }.nextOption()
    }
    def hasPurpose(arm: Arm): Boolean = preparation.exists(_.root eq arm.path) ||
      functional.exists(_.question.rootMove == arm.path.moves.head) || arm.activity.nonEmpty || purposeKnown(arm, rootUses) ||
        conditionalPurposes.exists(_.proof.owner.comparison.referenceMove == arm.path.moves.head)
    def withConditionalPurposes(values: List[LegalReplyOption]): Reading =
      require(values.forall(value => FunctionalReplyReading.valid(owner, value) &&
        FunctionalReplyReading.relevant(owner, arms.map(_.path), value)))
      new Reading(owner, supportedPost, reference, played, retainedChoice, sharedCapture,
        contrasts, rootUses, (evidence ++ values.flatMap(_.proof.evidence)).distinct,
        preparation, functional, values)
    def completePurpose: Boolean = arms.forall(hasPurpose)
    def preparedDevelopment(arm: Arm): Option[(CastlePreparation, LegalReplyOption)] =
      (for
        preparation <- arm.deferredCastle.toList if preparation.blocked.isEmpty
        other <- arms.filterNot(_ eq arm) if other.actor == preparation.original
        immediate <- other.castlePreparation.toList
        option <- conditionalPurposes if option.proof.owner.comparison.referenceMove == arm.path.moves.head &&
          option.proof.action.target.exists(target => immediate.blocked.exists(_._1 == target.witness)) &&
          (1 to preparation.offset + 1).forall(count => FunctionalReplyReading.conditionAt(
            owner, arm.path, count, option.proof.condition).contains("present"))
      yield preparation -> option).headOption
    def sameAftermath: Boolean = ReferencePurposeReading.sameAftermath(owner, arms)
    def sharedControlUse: Option[(ControlUse, ControlUse)] = for
      left <- reference.controlUse.filter(_.path eq reference.path)
      right <- played.controlUse.filter(_.path eq played.path)
      if left.controller == right.controller && left.originalUser == right.originalUser &&
        left.user == right.user && left.remaining.toSet == right.remaining.toSet &&
        left.removed == right.removed && left.target == right.target
    yield left -> right
    def withResponseUses(uses: List[ControlUse], activities: List[PurposeActivityReading.Reading] = Nil): Reading =
      def attach(arm: Arm) =
        val control = arm.controlUse.orElse(uses.find(use => CommentarySequenceText.sameRoot(use.path, arm.path)))
        arm.copy(controlUse = control, activity = Option.when(control.isEmpty)(PurposeActivityReading.choose(
          arm.activity.toList ++ activities.filter(use => use.original.side == arm.actor.side &&
            CommentarySequenceText.sameRoot(use.path, arm.path)))).flatten)
      new Reading(owner, supportedPost, attach(reference), attach(played), retainedChoice, sharedCapture,
        contrasts, rootUses, (evidence ++ uses.flatMap(_.evidence) ++ activities.flatMap(_.evidence)).distinct,
        preparation, functional, conditionalPurposes)
    def sharedPreparation: List[(PreparedPath, Int)] = preparation.toList.flatMap { value =>
      val uses = arms.flatMap(arm => value.branches.find(_.path eq arm.path)
        .flatMap(_.realization).map(arm.path -> _))
      Option.when(uses.size == 2)(uses).toList.flatten
    }
    def samePreparationPosition: Boolean = sharedPreparation match
      case List((left, a), (right, b)) =>
        left.positions(a + 1).fen.split(" ").take(4).toList ==
          right.positions(b + 1).fen.split(" ").take(4).toList &&
          owner.purposePosition(left, 0).exists(_.controls.keys.forall(p =>
            owner.trackedPieceAfter(left, p, a + 1) == owner.trackedPieceAfter(right, p, b + 1)))
      case _ => false
    // Shared structure is narrower than shared function. The same original
    // pawns can be doubled in both choices while only one has a defender.
    lazy val sharedPawnStructure: Set[Piece] = reference.pawns.filter { left =>
      played.pawns.exists { right =>
        val sameShape = left.original == right.original && left.current == right.current &&
          left.doubled.toSet == right.doubled.toSet && left.isolated == right.isolated && left.passed == right.passed
        val samePieces = (left.current :: left.doubled).forall { pawn =>
          val originals = arms.map { arm => owner.purposePosition(arm.path, 0).toList.flatMap(_.controls.keys)
            .find(original => owner.trackedPieceAfter(arm.path, original, arm.through + 1).flatten.contains(pawn)) }
          originals.head.nonEmpty && originals.head == originals.last
        }
        sameShape && samePieces && (left.current != left.original || left.doubled.nonEmpty ||
          left.isolated || left.passed.contains(true))
      }
    }.map(_.original).toSet
    lazy val sharedReplyPressure: Option[ReplyPressure] =
      def checks(arm: Arm) = arm.path.certified.toList.flatMap(_._2.lineEvents).exists(e =>
        e.plyOffset == 1 && e.kind == LineEventKind.Check)
      reference.replyPressure.filter(pressure => played.replyPressure.contains(pressure) &&
        reference.path.moves(1) == played.path.moves(1) && checks(reference) == checks(played))
    def purposeUsers(arm: Arm): Set[Piece] = rootUses.collect {
      case route: ClearedRoute if route.move.path eq arm.path => route.originalUser
      case route: SquareReoccupation if route.move.path eq arm.path => route.originalEntry
      case route: PieceRoute if route.path eq arm.path => route.original
    }.toSet ++ arm.controlUse.filter(_.path eq arm.path).map(_.originalUser) ++ arm.activity.map(_.original)

    def sameOriginalOccupants(left: MoveRef, right: MoveRef, squares: List[String]): Boolean =
      val moves = List(left, right)
      moves.forall(move => arms.exists(_.path eq move.path)) && squares.forall { square =>
        val origins = moves.map { move => for
          at <- owner.purposePosition(move.path, move.offset + 1)
          piece <- at.occupant(square)
          original <- owner.purposePosition(move.path, 0).toList.flatMap(_.controls.keys).find(original =>
            owner.trackedPieceAfter(move.path, original, move.offset + 1).flatten.contains(piece))
        yield original }
        origins.head.nonEmpty && origins.head == origins.last
      }

    private def continuousContact(arm: Arm, originalActor: Piece, actor: Piece, target: Piece,
        from: Int): Boolean =
      val originalTarget = owner.purposePosition(arm.path, 0).toList.flatMap(_.controls.keys).find(original =>
        owner.trackedPieceAfter(arm.path, original, from).flatten.contains(target))
      from <= arm.through + 1 && originalTarget.exists(original => (from to arm.through + 1).forall(count =>
        owner.trackedPieceAfter(arm.path, originalActor, count).flatten.contains(actor) &&
        owner.trackedPieceAfter(arm.path, original, count).flatten.contains(target) &&
        owner.purposePosition(arm.path, count).exists(at =>
          at.complete(RelationFactKind.GeometricControl) && at.controlledBy(actor)(target.at))))

    // A root contact owns its lifetime only when the same pieces keep that
    // relation throughout the observed line, not merely at both endpoints.
    def continuedRootContact(arm: Arm, target: Piece, attack: Boolean): Boolean =
      !preparation.exists(_.root eq arm.path) &&
        (if attack then arm.attacks.contains(target) &&
          !(arm.exchange.nonEmpty && target.role == arm.actor.role)
        else arm.protections.exists(_.target == target) && !arm.shields.exists(_._2 == target)) &&
        coreFunctions(arm).exists(f => f.original == arm.actor && f.current.contains(arm.moved) &&
          (if attack then f.attacks.contains(target) && f.openFile.isEmpty &&
            (purposeUsers(arm)(f.original) || arm.supportedArrival.exists(_.original == f.original) ||
              arm.occupant.contains(arm.moved)) else f.supports.contains(target))) &&
        continuousContact(arm, arm.actor, arm.moved, target, 1)

    def hasContinuedRootContact(arm: Arm): Boolean =
      arm.protections.exists(p => continuedRootContact(arm, p.target, attack = false)) ||
        arm.attacks.exists(continuedRootContact(arm, _, attack = true))

    def alreadyExplainedContact(arm: Arm, function: Function, target: Piece, attack: Boolean,
        rootAnnounced: Boolean): Boolean =
      rootAnnounced && function.original == arm.actor && function.current.contains(arm.moved) &&
        continuedRootContact(arm, target, attack) || rootAnnounced && arm.activity.exists(activity =>
          (activity.path eq arm.path) && function.original == activity.original &&
            function.current.contains(activity.actor) &&
            (if attack then activity.attacks.contains(target) else activity.guards.contains(target)) &&
            continuousContact(arm, function.original, activity.actor, target, activity.use + 1)) || rootUses.flatMap {
      case Action(move, detail) if (move.path eq arm.path) && (detail match
          case MoveWorkProjection.Detail.Support(actor, guarded, _, _) if !attack =>
            function.current.contains(actor) && guarded == target
          case MoveWorkProjection.Detail.Attack(actor, attacked, _) if attack =>
            function.current.contains(actor) && attacked == target
          case _ => false) => Some(move)
      case route: ClearedRoute if (route.use.path eq arm.path) && !route.passage &&
          route.originalUser == function.original && function.current.contains(route.user) &&
          route.targets.contains(target) && (target.side != route.user.side) == attack => Some(route.use)
      case _ => None
    }.exists(move => continuousContact(arm, function.original, function.current.get, target, move.offset + 1))

    def alreadyExplainedSupport(arm: Arm, function: Function, target: Piece, rootAnnounced: Boolean = false): Boolean =
      alreadyExplainedContact(arm, function, target, attack = false, rootAnnounced)

    def alreadyExplainedAttack(arm: Arm, function: Function, target: Piece, rootAnnounced: Boolean = false): Boolean =
      alreadyExplainedContact(arm, function, target, attack = true, rootAnnounced)

    // Core selection follows the root's actors and actual uses, not every
    // remaining defender. The full reading still owns all observed relations.
    def coreFunctions(arm: Arm): List[Function] =
      val actors = arms.map(_.actor).toSet
      val users = rootUses.collect {
        case route: ClearedRoute => route.originalUser
        case route: SquareReoccupation => route.originalEntry
        case route: PieceRoute => route.original
      }.toSet
      val selected = contrasts.filter(c => actors(c.reference.original) || users(c.reference.original) ||
        c.reference.openFile != c.played.openFile || c.reference.current.isDefined != c.played.current.isDefined)
      val differences = (if selected.nonEmpty then selected else contrasts.take(1)).map(_.reference.original).toSet
      arm.functions.filter { f =>
        val performsPurpose = purposeUsers(arm)(f.original) || arm.supportedArrival.exists(_.original == f.original) ||
          f.current == arm.occupant
        f.current.nonEmpty && (differences(f.original) || performsPurpose &&
          (f.attacks.nonEmpty || f.supports.nonEmpty || f.openFile.nonEmpty))
      }

  private def purposeKnown(arm: Arm, uses: List[Statement]): Boolean =
    arm.evasion.nonEmpty || arm.castlePreparation.nonEmpty || arm.controlUse.nonEmpty || arm.captured.nonEmpty || arm.attacks.nonEmpty || arm.escaped.nonEmpty || arm.protections.nonEmpty ||
      arm.shields.nonEmpty || arm.guards.nonEmpty || arm.supportedArrival.nonEmpty ||
      arm.rookFile.nonEmpty || uses.exists {
        case Action(_, _: MoveWorkProjection.Detail.ChangedLines) => false
        case statement => statement.references.contains(MoveRef(arm.path, 0))
      }

  /** A root or first reply changes opposing control, and an actual later move uses it.
    * Loss of one controller is not a claim that a square or capture is safe. */
  def controlUse(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths, path: PreparedPath,
      start: Int = 0): Option[ControlUse] =
    paths.requireOwner(packet)
    if !paths.paths.exists(_ eq path) || path.certified.isEmpty || path.moves != path.sourceMoves ||
        path.ids != path.declared || start < 0 || start > 1 || path.moves.size < start + 3 then return None
    def position(count: Int) = paths.purposePosition(path, count).filter(_.complete(RelationFactKind.GeometricControl))
    def at(original: Piece, count: Int) = paths.trackedPieceAfter(path, original, count).flatten
    val before = position(start) match
      case Some(value) => value
      case None => return None
    val rootAt = before.occupant(path.moves(start).take(2)) match
      case Some(value) => value
      case None => return None
    val originals = position(0).toList.flatMap(_.controls.keys)
    val root = originals.find(at(_, start).contains(rootAt)) match
      case Some(value) => value
      case None => return None
    val moved = at(root, start + 1) match
      case Some(value) => value
      case None => return None
    val openingEnd = ExchangeActivity.settled(path, start)
    val rootCapture = path.certified.get._2.material.toList.flatMap(_.events).exists(e =>
      e.plyOffset == start && e.capture.nonEmpty)
    val exchange = position(start + 1).flatMap(after => ExchangeActivity.discover(path.moves(start), rootAt, before, after))
    // A reply exchange owns its next turn, not a later unrelated plan.
    val uses = if start == 0 then (2 until math.min(path.moves.size, 18)).toList else
      (openingEnd + 1 until math.min(path.moves.size, 18)).find(_ % 2 == start % 2).toList
    val candidates = uses.flatMap { use =>
      for
        prior <- position(use).toList
        after <- position(use + 1).toList
        moving <- prior.occupant(path.moves(use).take(2)).toList
        if moving.side == root.side
        original <- originals.find(at(_, use).contains(moving)).toList
        user <- at(original, use + 1).filter(_ != moving).toList
        target = before.occupant(user.at)
        if target.forall(_.side != root.side) && target.forall(p => originals.find(at(_, start).contains(p)).exists(id =>
          at(id, use).contains(p) && paths.trackedPieceAfter(path, id, use + 1).contains(None)))
        controllerAt <- before.controllers(user.at, !root.side)
        controller <- originals.find(at(_, start).contains(controllerAt)).toList
        removed = Option.when(use > openingEnd && (rootCapture || exchange.exists(_.targets.contains(controllerAt))))(
          (start to openingEnd).find(n => at(controller, n).nonEmpty &&
            paths.trackedPieceAfter(path, controller, n + 1).contains(None))).flatten
        blocked = position(start + 1).exists(p => p.contains(controllerAt) && !p.controlledBy(controllerAt)(user.at)) &&
          (start + 1 to use).forall(n => at(root, n).contains(moved) && at(controller, n).contains(controllerAt) &&
            position(n).exists(p => !p.controlledBy(controllerAt)(user.at)))
        if removed.nonEmpty || blocked
        change = removed.getOrElse(start)
        if (change + 1 to use).forall(n => position(n).exists(p =>
          paths.trackedPieceAfter(path, controller, n).exists(_.forall(current => !p.controlledBy(current)(user.at)))))
      yield
        val remaining = after.controllers(user.at, !root.side)
        val sources = before.sourceRefs(before.sources(controllerAt, List(user.at))) ++
          after.sourceRefs(remaining.flatMap(p => after.sources(p, List(user.at)))) ++
          (0 to use + 1).toList.flatMap(position(_).map(_.view.occurrence.id))
        ControlUse(path, controller, change, removed.nonEmpty, target, original, user, use, remaining, sources.distinct)
    }
    candidates.sortBy(value => (if value.target.nonEmpty then 0 else 1, value.use, value.controller.at)).headOption

  private def sameAftermath(paths: PreparedPaths, arms: List[Arm]): Boolean =
    arms.size == 2 && arms.map(a => a.path.positions(a.through + 1).fen.split(" ").take(4).toList).distinct.size == 1 &&
      paths.purposePosition(arms.head.path, 0).exists(_.controls.keys.forall(original =>
        arms.map(a => paths.trackedPieceAfter(a.path, original, a.through + 1)).distinct.size == 1))

  def counterplay(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths, path: PreparedPath): Option[ReplyActivity] =
    paths.requireOwner(packet)
    if !paths.paths.exists(_ eq path) || path.moves.size < 3 then return None
    val enabled = controlUse(packet, paths, path, start = 1).flatMap(use =>
      PurposeActivityReading.followup(paths, path, use.originalUser, use.use, use.evidence, Some(use))
        .map(ReplyActivity(Some(use), _)))
    val firstReply = paths.purposePosition(path, 0).toList.flatMap(_.controls.keys)
      .find(p => paths.trackedPieceAfter(path, p, 1).flatten.exists(_.at == path.moves(1).take(2)))
      .flatMap(original => PurposeActivityReading.followup(paths, path, original, 1, Nil))
      .filter(activity => activity.use == 1 && (activity.recovery.nonEmpty || activity.continuation.nonEmpty))
      .map(ReplyActivity(None, _))
    enabled.orElse(firstReply)

  def assessmentPair(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths): List[PreparedPath] =
    paths.requireOwner(packet)
    packet.primary.toList.flatMap {
      case PlayerFacingPrimary.MoveVerdict(ref) => List(ref)
      case PlayerFacingPrimary.BestChoice(ref) => List(ref)
    }.flatMap(packet.evidenceGraph.candidateComparisonRecord).flatMap(_.payload match
      case CandidateComparisonEvidence(value) => List(value.referenceLine, value.candidateLine)
      case _ => Nil).flatMap(ref => paths.paths.find(_.line.ref == ref))

  def read(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      pair: List[PreparedPath], rootUses: => List[(Section, List[String])] = Nil,
      minimumThrough: Map[PreparedPath, Int] = Map.empty,
      preparation: Option[MoveIntent.Reading] = None, responses: Option[MovePurpose.Reading] = None): Option[Reading] =
    paths.requireOwner(packet)
    if preparation.exists(value => !value.belongsTo(paths) || value.pair.toSet != pair.toSet) then return None
    val functional = responses.filter(value => value.belongsTo(paths) &&
      (value.witnessedPlan.nonEmpty || value.witnessedSupport.nonEmpty) &&
      pair.exists(path => path.moves.head == value.question.rootMove && path.ids.head == value.question.path.ids.head) &&
      !pair.exists(_ eq value.witnessedPlan.map(_.path).orElse(value.witnessedSupport.map(_.path)).get))
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
    if !before.complete(RelationFactKind.GeometricControl) then return None
    val side = before.occupant(ordered.head.moves.head.take(2)).get.side
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
    def contacts(at: Position, actor: Piece, targets: List[Piece]): List[Piece] =
      val found = targets.filter(p => at.controlledBy(actor)(p.at)).sortBy(_.at)
      proof ++= at.sourceRefs(at.sources(actor, found.map(_.at)))
      found
    val originals = paths.identitiesAfter(packet, ordered.head, 0).map(_._1)
    val actors = ordered.map(path => before.occupant(path.moves.head.take(2)).get)
    def current(path: PreparedPath, original: Piece, count: Int) =
      paths.trackedPieceAfter(path, original, count).flatten
    def captures(path: PreparedPath) = path.certified.get._2.material.get.events.filter(_.capture.nonEmpty)
    def castlePreparation(path: PreparedPath, offset: Int): Option[CastlePreparation] =
      (for
        from <- position(path, offset).toList
        after <- position(path, offset + 1).toList
        developer <- from.occupant(path.moves(offset).take(2)).toList if developer.side == side
        original <- originals.filter(current(path, _, offset).contains(developer))
        rank = if side.white then "1" else "8"
        king <- from.occupant("e" + rank).toList if king.side == side && king.role.name == "king"
        kingSide <- List(true, false)
        rook <- from.occupant((if kingSide then "h" else "a") + rank).toList
        if rook.side == side && rook.role.name == "rook" && after.contains(king) && after.contains(rook)
        right = (if kingSide then "K" else "Q").map(c => if side.white then c else c.toLower)
        if path.positions(offset).fen.split(" ")(2).contains(right) &&
          path.positions(offset + 1).fen.split(" ")(2).contains(right)
        passage = (if kingSide then List("f", "g") else List("b", "c", "d")).map(_ + rank)
        traversal = (if kingSide then List("e", "f", "g") else List("e", "d", "c")).map(_ + rank)
        if passage.contains(developer.at) && passage.forall(after.empty) &&
          passage.filterNot(_ == developer.at).forall(from.empty) &&
          traversal.forall(square => after.controllers(square, !side).isEmpty)
      yield
        val blocked = position(path, offset + 2).toList.flatMap(reply => traversal.flatMap(square =>
          reply.controllers(square, !side).flatMap(attacker =>
            originals.find(current(path, _, offset + 2).contains(attacker)).map(identity => (identity, attacker, square)))))
          .groupMap((identity, attacker, _) => identity -> attacker)(_._3).toList
          .map((pieces, squares) => (pieces._1, pieces._2, squares.distinct)).sortBy(_._2.at)
        CastlePreparation(original, offset, kingSide, blocked)
      ).headOption
    def close(path: PreparedPath, offset: Int): Int =
      val offsets = captures(path).map(_.plyOffset).toSet
      var end = offset
      val checking = path.certified.toList.flatMap(_._2.lineEvents)
        .filter(_.kind == LineEventKind.Check).map(_.plyOffset).toSet
      while end + 1 < path.moves.size && (offsets(end + 1) || checking(end)) do end += 1
      end
    def rootSupport(path: PreparedPath, actor: Piece,
        original: Piece, target: Piece, count: Int): Boolean =
      PurposeActivityReading.rootSupport(paths, path, actor, original, target.at, count).exists { witness =>
        proof ++= witness.evidence
        true
      }
    def supportedArrivals(path: PreparedPath, actor: Piece, moved: Piece, through: Int): List[SupportedArrival] =
      (2 to through).flatMap { offset =>
        for
          beforeUse <- position(path, offset)
          arriver <- beforeUse.occupant(path.moves(offset).take(2))
          original <- originals.find(p => p != actor && p.side == side &&
            current(path, p, offset).contains(arriver))
          afterUse <- position(path, offset + 1)
          target <- current(path, original, offset + 1)
          if target != arriver && rootSupport(path, actor, original, target, offset + 1) &&
            afterUse.controlledBy(moved)(target.at)
        yield
          proof ++= afterUse.sourceRefs(afterUse.sources(moved, List(target.at)))
          proof ++= position(path, 1).toList.flatMap(at =>
            at.sourceRefs(at.sources(moved, List(target.at))))
          SupportedArrival(original, target, offset)
      }.toList
    val movedRoots = ordered.zip(actors).flatMap((path, actor) =>
      current(path, actor, 1).map(moved => path -> (actor, moved))).toMap
    val exchangeTasks = movedRoots.flatMap { (path, root) =>
      position(path, 1).flatMap(after => ExchangeActivity.discover(path.moves.head, root._1, before, after))
        .map(path -> _)
    }
    val rootMechanisms = rootUses.flatMap { (section, evidence) =>
      val route = section.statements.collectFirst {
        case route: PieceRoute if route.offsets.headOption.contains(0) && route.offsets.size >= 2 &&
          route.offsets.last < route.path.moves.size && movedRoots.get(route.path).exists(_._1 == route.original) &&
          current(route.path, route.original, close(route.path, route.offsets.last) + 1).nonEmpty => route
      }
      val uses = route.toList.flatMap(r => section.statements.collect {
        case action @ Action(move, MoveWorkProjection.Detail.Support(actor, _, opposition, true)) if
          (move.path eq r.path) && move.offset == r.offsets.last && opposition.nonEmpty &&
          current(r.path, r.original, move.offset + 1).contains(actor) => action
        case action @ Action(move, MoveWorkProjection.Detail.Attack(actor, _, true)) if
          (move.path eq r.path) && move.offset == r.offsets.last &&
          current(r.path, r.original, move.offset + 1).contains(actor) => action
      })
      section.statements.collect {
        // A pawn can release a slider without the finite PV using that line.
        // This states the present opening only, together with any newly opened
        // enemy attack; it does not invent a later destination or force play.
        case action @ Action(move, MoveWorkProjection.Detail.OpenedRay(from, user, end)) if
          move.offset == 0 && movedRoots.get(move.path).exists { (actor, _) =>
            actor.role.name == "pawn" && actor.at == from && user.side == actor.side &&
              before.contains(user) && !before.controlledBy(user)(end) &&
              position(move.path, 1).exists(at => at.contains(user) && at.controlledBy(user)(end))
          } => action -> evidence
        case action @ Action(move, MoveWorkProjection.Detail.ChangedLines(Nil, opened, Nil)) if
          move.offset == 0 && opened.nonEmpty && movedRoots.contains(move.path) &&
            opened.forall { (attacker, target) => attacker.side != side && target.side == side &&
              before.contains(attacker) && before.contains(target) && !before.controlledBy(attacker)(target.at) &&
              position(move.path, 1).exists(at => at.contains(attacker) && at.contains(target) &&
                at.controlledBy(attacker)(target.at)) } => action -> evidence
        case route: ClearedRoute if route.move.offset == 0 && (route.move.path eq route.use.path) &&
          movedRoots.get(route.move.path).contains(route.before -> route.mover) => route -> evidence
        case route: SquareReoccupation if route.move.offset == 0 && (route.move.path eq route.use.path) &&
          movedRoots.get(route.move.path).contains(route.originalActor -> route.actor) => route -> evidence
      } ++ (if uses.isEmpty then Nil else (route.toList ++ uses).map(_ -> evidence))
    }.distinctBy(_._1)
    val controlUses = movedRoots.keys.flatMap(path => controlUse(packet, paths, path).map { use =>
      proof ++= use.evidence
      path -> use
    }).toMap
    val counterplays = movedRoots.keys.flatMap { path =>
      counterplay(packet, paths, path).map { activity =>
        proof ++= activity.activity.evidence
        path -> activity
      }
    }.toMap
    // A complete preparation already owns its realization, response and price.
    // Its remaining local contacts must not start a second, later purpose.
    val activities = movedRoots.keys.filter(_ => preparation.isEmpty).filterNot(controlUses.contains)
      .flatMap(path => PurposeActivityReading.choose(PurposeActivityReading.discover(packet, paths, path,
      rootMechanisms.map((statement, evidence) => Section(List(statement)) -> evidence))).map { activity =>
        proof ++= activity.evidence
        path -> activity
      }).toMap
    val purposeOffsets = movedRoots.map { (path, root) =>
      val arrivals = supportedArrivals(path, root._1, root._2, math.min(9, path.moves.size - 1)).map(_.offset)
      val routes = rootMechanisms.flatMap {
        case (r: ClearedRoute, _) if r.use.path eq path => List(r.use.offset)
        case (r: SquareReoccupation, _) if r.use.path eq path => List(r.use.offset)
        case (r: PieceRoute, _) if r.path eq path => List(r.offsets.last)
        case _ => Nil
      }
      // A queen trade can transfer the play to the pawns without changing a
      // pawn file during the trade. Read the first actual pawn confrontation,
      // not a speculative advance or the whole later pawn ending.
      val pawnUse = exchangeTasks.get(path).filter(_ => root._1.role.name == "queen")
        .filter(_.openFile.nonEmpty).toList.flatMap { _ =>
          captures(path).headOption.toList.flatMap { capture =>
            (close(path, capture.plyOffset) + 1 until math.min(10, path.moves.size)).find { offset =>
              ExchangeActivity.pawnWork(paths, path, offset, close(path, offset))
                .exists(work => work.offset == offset && work.continuing)
            }
          }
        }
      path -> (arrivals ++ routes ++ pawnUse ++ controlUses.get(path).map(_.use)).sorted.headOption
    }
    // New local discovery stays bounded above, but a root use already proved
    // by the shared readers must reach its actual occurrence. An arbitrary
    // ten-ply cutoff must not turn that known use into an unknown purpose.
    // Choose the earliest use and close its captures/checks; do not scan for
    // another plan or continue beyond this returned PV.
    def horizon(path: PreparedPath): Int =
      val first = captures(path).find(_.plyOffset < 10).map(_.plyOffset).getOrElse(2)
      close(path, List(2, first, purposeOffsets.get(path).flatten.filter(_ < 10).getOrElse(2),
        activities.get(path).map(_.through).getOrElse(2), counterplays.get(path).map(_.through).getOrElse(2)).max)
    val base = ordered.zip(actors).map { (path, actor) =>
      for
        after <- position(path, 1)
        moved <- current(path, actor, 1)
        reply <- after.occupant(path.moves(1).take(2))
        replied <- position(path, 2)
        replyMoved <- current(path, reply, 2)
        end <- position(path, horizon(path) + 1)
      yield
        val attacks = contacts(after, moved, after.controls.keys.toList.filter(p => p.side != side &&
          !before.controlledBy(actor)(p.at)))
        val escaped = before.controllers(actor.at, !side).filter(p => after.contains(p) &&
          !after.controlledBy(p)(moved.at))
        proof ++= before.sourceRefs(escaped.flatMap(p => before.sources(p, List(actor.at))))
        val protections = contacts(after, moved, originals.filter(p => p.side == side && p != actor &&
          after.contains(p) && !before.controlledBy(actor)(p.at))).flatMap { target =>
          // A captured attacker cannot remain the reason for a new defence.
          // Read the current obligation after the root, not its old inventory.
          val present = after.controllers(target.at, !side)
          val anticipated = present.isEmpty && replied.contains(moved) && replied.contains(target) &&
            replied.controlledBy(moved)(target.at)
          val at = if anticipated then replied else after
          val attackers = at.controllers(target.at, !side)
          proof ++= at.sourceRefs(attackers.flatMap(p => at.sources(p, List(target.at))))
          Option.when(attackers.nonEmpty)(Protection(target, attackers, anticipated))
        }
        val releasedGuards = originals.filter(target => target.side == side && target != actor &&
          before.controlledBy(actor)(target.at) && after.contains(target) &&
          !after.controlledBy(moved)(target.at) && after.controllers(target.at, !side).nonEmpty)
        proof ++= before.sourceRefs(before.sources(actor, releasedGuards.map(_.at)))
        proof ++= after.sourceRefs(releasedGuards.flatMap(p => after.controllers(p.at, !side)
          .flatMap(attacker => after.sources(attacker, List(p.at)))))
        val shields = originals.filter(p => p.side == side && p != actor).flatMap { target =>
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
        val captured = before.view.legalRows.toList.flatten.find(_.move == path.moves.head).flatMap(_.capture)
        // An initial check is a shared obligation, not an exclusion from the
        // purpose comparison. Certify its actual legal resolution before
        // reading what each choice allows the other side to do afterward.
        val evasion = for
          king <- originals.find(p => p.side == side && p.role.name == "king")
          if before.view.currentCheckSources(side).nonEmpty
          currentKing <- current(path, king, 1)
          if after.view.currentCheckSources(side).isEmpty
          row <- before.view.legalRows.flatMap(_.find(r => r.move == path.moves.head && r.actor == actor))
          checkers = before.controllers(king.at, !side)
          kind <- if actor == king then Some("king_move")
            else if captured.exists(checkers.contains) then Some("capture_checker")
            else Option.when(checkers.forall(checker =>
              after.blockedRay(checker, currentKing).exists(_._2.contains(moved))))("interpose")
        yield
          proof ++= before.sourceRefs(before.view.currentCheckSources(side).map(_.semanticId))
          CheckEvasion(currentKing, checkers, kind)
        val replyCapture = after.view.legalRows.toList.flatten.find(_.move == path.moves(1)).flatMap(_.capture)
        val replyShields = attacks.filter(target => replied.contains(moved) && replied.contains(target) &&
          !replied.controlledBy(moved)(target.at) &&
          replied.blockedRay(moved, target).exists(_._2.contains(replyMoved)))
        proof ++= replied.sourceRefs(replyShields.flatMap(target => replied.blockedRay(moved, target).map(_._1)))
        val file = Option.when(actor.role.name == "rook")(after.view.pawnFiles.find(_.file.key == moved.at.take(1)))
          .flatten.filter(f => Set("open", s"${if side.white then "black" else "white"}_pawns_only")(f.occupancy))
        proof ++= file.toList.flatMap(f => after.sourceRefs(f.sourceIds))
        // A recapturing piece can briefly attack other men and disappear in
        // the same local transaction. The exchange owns that transient state;
        // it is not another lasting threat. Checks retain their own receipts.
        val replyClose = close(path, 1) + 1
        val pressure = Option.when(current(path, reply, replyClose).nonEmpty)(
          contacts(replied, replyMoved, replied.controls.keys.toList.filter(p => p.side == side &&
            !after.controlledBy(reply)(p.at) && p.role.name != "king" &&
            originals.find(original => current(path, original, 2).contains(p))
              .exists(original => (2 to replyClose).forall(n => current(path, original, n).contains(p))))))
          .getOrElse(Nil)
        val alignments = replied.relativeAlignments(replyMoved).filter((_, front, _) => pressure.contains(front))
        proof ++= replied.sourceRefs(alignments.map(_._1))
        val pressureIdentities = (replyMoved :: pressure ::: alignments.map(_._3)).distinct.flatMap(p =>
          originals.find(original => current(path, original, 2).contains(p)).map(_ -> p)).sortBy(_._1.at)
        Arm(path, actor, moved, horizon(path), attacks, shields, Nil, Nil, None,
          current(path, actor, horizon(path) + 1), reply, None, Nil, captured, protections,
          replyCapture, attacks.contains(reply), replyShields, escaped, Nil, Nil,
          rookFile = file.map(f => if f.occupancy == "open" then "open" else "semi-open"),
          replyPressure = Option.when(pressure.nonEmpty)(ReplyPressure(replyMoved, pressure,
            alignments.map((_, front, rear) => front -> rear), pressureIdentities)), exchange = exchangeTasks.get(path),
          releasedGuards = releasedGuards, controlUse = controlUses.get(path), activity = activities.get(path),
          counterplay = counterplays.get(path), evasion = evasion,
          castlePreparation = castlePreparation(path, 0), deferredCastle =
            (2 to horizon(path) by 2).find(offset => ordered.exists(other => (other ne path) &&
              path.moves(offset) == other.moves.head)).flatMap(castlePreparation(path, _)))
    }
    if base.exists(_.isEmpty) then return None
    if before.view.currentCheckSources(side).nonEmpty && base.flatten.exists(_.evasion.isEmpty) then return None
    val initial = base.flatten.map { arm =>
      val localUses = rootMechanisms.map(_._1).filter(_.references.forall(move =>
        (move.path ne arm.path) || move.offset <= arm.through))
      // A late witnessed use can fill a missing root purpose. Once this root
      // already has a bounded account, another distant use is not a reason to
      // pull all intervening exchanges and unrelated aftermath into the core.
      val routeUse = rootMechanisms.collect {
        case (route: PieceRoute, _) if (route.path eq arm.path) && route.offsets.size > 2 => route.offsets.last
         case (route: ClearedRoute, _) if (route.move.path eq arm.path) && route.preparations.nonEmpty &&
             arm.activity.exists(_.seed.mechanism.contains(route)) => route.use.offset
      }.minOption
      val late = routeUse.orElse(purposeOffsets.get(arm.path).flatten)
        .filter(_ > arm.through).filter(offset => routeUse.contains(offset) || !purposeKnown(arm, localUses))
      late.fold(arm) { offset =>
        val end = close(arm.path, offset)
        arm.copy(through = end, retainedMover = current(arm.path, arm.actor, end + 1))
      }
    }
    // A later occurrence of the other root's capture makes this a timing
    // comparison. Matching the same original capturer and victim prevents a
    // same-square replacement from being called the same exchange.
    def captureOf(arm: Arm, actor: Piece, victim: Piece): Option[Int] =
      captures(arm.path).filter(_.plyOffset < 10).find { event =>
        val n = event.plyOffset
        position(arm.path, n).exists(at => at.view.legalRows.toList.flatten.exists(row =>
          row.move == arm.path.moves(n) && current(arm.path, actor, n).contains(row.actor) &&
            current(arm.path, victim, n) == row.capture && row.capture.nonEmpty))
      }.map(_.plyOffset)
    val shared = initial.flatMap(arm => arm.captured.toList.flatMap { victim =>
      for
        left <- captureOf(initial.head, arm.actor, victim)
        right <- captureOf(initial.last, arm.actor, victim)
        if left != right
      yield SharedCapture(arm.actor, victim, left, right)
    }).headOption
    val exchanged = initial.zipWithIndex.map { (arm, index) =>
      val aligned = shared.map(s => if index == 0 then s.referenceOffset else s.playedOffset).getOrElse(arm.through)
      val through = close(arm.path, math.max(arm.through, aligned))
      val changedPawns = originals.filter(p => p.side == side && p.role.name == "pawn" &&
        current(arm.path, p, through + 1).exists(now => now.at.head != p.at.head))
      // One actual follow-up may take over the exchanged piece's work.
      // It must support a pawn transformed by that exchange.
      val nextOwn = if through % 2 == 0 then through + 2 else through + 1
      val use = Option.when(nextOwn < math.min(arm.path.moves.size, 12))(nextOwn).filter { n =>
        (for
          at <- position(arm.path, n + 1)
          mover <- at.occupant(arm.path.moves(n).slice(2, 4))
        yield changedPawns.flatMap(p => current(arm.path, p, n + 1))
          .exists(p => at.controlledBy(mover)(p.at))).contains(true)
      }
      val end = close(arm.path, use.getOrElse(through))
      arm.copy(through = end, retainedMover = current(arm.path, arm.actor, end + 1))
    }
    // Compare the same number of moves where both PVs allow it; an earlier
    // cutoff must not manufacture an activity advantage for a delayed trade.
    val commonEnd = exchanged.map(_.through).max
    val extended = exchanged.map { arm =>
      val end = close(arm.path, math.min(math.max(commonEnd,
        minimumThrough.getOrElse(arm.path, 0)), arm.path.moves.size - 1))
      arm.copy(through = end, retainedMover = current(arm.path, arm.actor, end + 1))
    }
    // A finite PV still certifies its final board. Do not discard that board
    // because a further reply is missing; state that boundary in the answer.
    // An exchange begun within the budget must close even beyond 12 plies.
    if extended.exists(arm => position(arm.path, arm.through + 1).isEmpty ||
        paths.identitiesAfter(packet, arm.path, arm.through + 1).exists(_._2.isEmpty)) then return None
    // Reuse the existing physical clearance reader. A new root purpose must
    // own an actual use inside this same bounded account, not a possible route.
    val mechanisms = rootMechanisms.filter { (statement, _) =>
      val use = statement match
        case route: ClearedRoute => route.use
        case route: SquareReoccupation => route.use
        case route: PieceRoute => MoveRef(route.path, route.offsets.last)
        case action: Action => action.move
        case _ => throw new IllegalStateException("Unselected root mechanism")
      extended.exists(a => (a.path eq use.path) && use.offset <= a.through)
    }
    val uses = mechanisms.map(_._1).filterNot {
      // The observed use of the same opened line already explains its availability.
      case Action(move, MoveWorkProjection.Detail.OpenedRay(from, actor, _)) => mechanisms.exists {
        case (route: ClearedRoute, _) => route.move == move && route.before.at == from && route.originalUser == actor
        case (route: SquareReoccupation, _) => route.move == move && route.square == from && route.originalEntry == actor
        case _ => false
      }
      case _ => false
    }.sortBy {
      case Action(_, _: MoveWorkProjection.Detail.ChangedLines) => 1
      case _ => 0
    }
    proof ++= mechanisms.flatMap(_._2)
    val users = uses.collect {
      case route: ClearedRoute => route.originalUser
      case route: SquareReoccupation => route.originalEntry
      case route: PieceRoute => route.original
    }.toSet ++ extended.flatMap(_.controlUse.map(_.originalUser))
    val posts = actors.zip(ordered).collect {
      case (actor, path) if Set("bishop", "knight")(actor.role.name) => path.moves.head.slice(2, 4)
    }.filter(s => Set('c', 'd', 'e', 'f')(s.head) && Set('3', '4', '5', '6')(s.last)).distinct
    val post = posts.find { square =>
      val guards = extended.map(arm => (pawns(position(arm.path, 1).get, square),
        pawns(position(arm.path, arm.through + 1).get, square)))
      guards.head._1.nonEmpty && guards.last._1.isEmpty &&
        extended.last.attacks.nonEmpty && extended.forall(_.captured.isEmpty)
    }
    val changedPawns = originals.filter(p => p.role.name == "pawn" && extended.exists(arm =>
      (1 to arm.through + 1).exists(count => current(arm.path, p, count).exists(now => now.at.head != p.at.head))))
    val subjects = originals.filter(p => p.side == side && p.role.name != "pawn" && p.role.name != "king" &&
      (actors.contains(p) || users(p) || extended.map(arm => current(arm.path, p, arm.through + 1).nonEmpty).distinct.size == 2 ||
        extended.exists(arm => current(arm.path, p, arm.through + 1).exists(_ != p))))
      .sortBy(p => (if actors.contains(p) then 0 else 1, p.at))
    val arms = extended.map { arm =>
      val end = position(arm.path, arm.through + 1).get
      val replyAt = current(arm.path, arm.reply, arm.through + 1)
      val replyGuards = replyAt.toList.flatMap(p => end.controllers(p.at, p.side))
      proof ++= end.sourceRefs(replyGuards.flatMap(p => end.sources(p, replyAt.toList.map(_.at))))
      val assets = (changedPawns ++ initial.flatMap(a => a.protections.map(_.target) ++ a.shields.map(_._2) ++ a.releasedGuards)).distinct
        .flatMap(p => current(arm.path, p, arm.through + 1))
      val functions = subjects.map { original =>
        val now = current(arm.path, original, arm.through + 1)
        val targets = now.toList.flatMap(p => contacts(end, p, end.controls.keys.toList.filter(_.side != side)))
        val supports = now.toList.flatMap(p => contacts(end, p, assets.filter(_.side == side)))
        val file = now.filter(p => p.role.name == "rook" &&
          !end.controls.keys.exists(q => q.role.name == "pawn" && q.at.head == p.at.head) &&
          end.controlledBy(p).count(_.head == p.at.head) >= 3).map(_.at.head.toString)
        file.foreach(_ => proof ++= end.sourceRefs(now.toList.flatMap(p => end.sources(p,
          end.controlledBy(p).toList.filter(_.head == p.at.head)))))
        Function(original, now, targets, supports, file)
      }
      val pawnChanges = changedPawns.flatMap { original => current(arm.path, original, arm.through + 1).map { pawn =>
        val guards = end.controllers(pawn.at, pawn.side)
        proof ++= end.sourceRefs(guards.flatMap(p => end.sources(p, List(pawn.at))))
        val own = end.controls.keys.filter(p => p.side == pawn.side && p.role.name == "pawn").toList
        val passage = end.view.pawnStates.find(_.square == pawn.square)
        proof ++= end.sourceRefs(passage.toList.flatMap(_.sourceIds).filter(end.passageSources))
        PawnChange(original, pawn, guards, own.filter(p => p != pawn && p.at.head == pawn.at.head).sortBy(_.at),
          !own.exists(p => math.abs(p.at.head - pawn.at.head) == 1), passage.flatMap(_.passed))
      }}
      val losses = paths.continuationReadings.find(r => (r.path eq arm.path) && r.complete).toList
        .flatMap(_.events).filter(e => e.plyOffset <= arm.through && e.originalVictim.nonEmpty)
      val materialEvents = arm.path.certified.get._2.material.get.events.filter(_.plyOffset <= arm.through)
      val identified = losses.size == materialEvents.size && losses.forall(e => materialEvents.exists(m =>
        m.plyOffset == e.plyOffset && m.promotionGainCp == 0 && m.capture.exists(c =>
          e.originalVictim.exists(p => p.role == c.capturedRole && p.side != c.side))))
      // A newly controlled square can be used without the provider making
      // the later recapture. Follow an actual allied arrival, not an empty
      // square's hypothetical plan or a different piece with the same role.
      val arrivals = supportedArrivals(arm.path, arm.actor, arm.moved, arm.through)
      val supported = losses.flatMap(_.originalVictim).filter(p => p.side == side && p != arm.actor).distinct.flatMap { target =>
        LineContinuationInvestigation.directSupportUse(packet, arm.path, paths,
          arm.through + 1, arm.actor, target).uses.filter(use => use.provider == arm.moved &&
            use.targetCapture.plyOffset > 1 && rootSupport(arm.path, arm.actor,
              target, use.target, use.targetCapture.plyOffset))
      }.distinct
      val supportUse = supported match
        case List(use) =>
          proof ++= use.support.sourceIds
          Some(use)
        case _ => None
      val pawnWork = ExchangeActivity.pawnWork(paths, arm.path, arm.through, arm.through)
        .filter(work => work.continuing && (work.original == arm.actor ||
          arm.actor.role.name == "queen" && arm.exchange.exists(_.openFile.nonEmpty)))
      proof ++= pawnWork.toList.flatMap { work =>
        val start = position(arm.path, work.offset + 1).get
        start.sourceRefs(start.sources(work.actor, work.targets.map(_.at))) ++
          end.sourceRefs(work.guards.flatMap(p => end.sources(p, List(work.next))))
      }
      arm.copy(guards = post.toList.flatMap(pawns(position(arm.path, 1).get, _)),
        remainingGuards = post.toList.flatMap(pawns(end, _)), occupant = post.flatMap(end.occupant),
        replyAt = replyAt, replyGuards = replyGuards, functions = functions, pawns = pawnChanges,
        losses = if identified then losses.flatMap(_.originalVictim) else Nil, supportUse = supportUse,
        supportedArrival = arrivals.headOption, pawnWork = pawnWork)
    }
    // Keep a shared physical subject only when its fate, task or occupation
    // distinguishes the choices. Moving somewhere is not itself a purpose.
    val relevant = subjects.filter(p => actors.contains(p) || users(p) || arms.exists(_.supportedArrival.exists(_.original == p)) ||
      arms.map(a => current(a.path, p, a.through + 1).nonEmpty).distinct.size == 2 ||
      arms.flatMap(_.functions).filter(_.original == p).exists(f => f.supports.nonEmpty || f.openFile.nonEmpty ||
        f.current.exists(now => actors.zip(ordered).exists((_, path) => now.at == path.moves.head.slice(2, 4)))))
    val focused = arms.map(arm => arm.copy(functions = arm.functions.filter(f => relevant.contains(f.original))))
    val contrasts = focused.head.functions.flatMap { left =>
      focused.last.functions.find(_.original == left.original).filter { right =>
        left.current.isDefined != right.current.isDefined || left.openFile != right.openFile ||
          left.supports != right.supports
      }.map(FunctionContrast(left, _))
    }.sortBy { c =>
      val work = List(c.reference, c.played)
      (if work.exists(_.supports.nonEmpty) then 0 else if work.exists(_.openFile.nonEmpty) then 1 else 2,
        c.reference.original.at)
    }
    val replyActivities = responses.filter(value => value.belongsTo(paths) && value.search.coversAllLegalMoves)
      .toList.flatMap(_.branches.flatMap(_.purposeActivity)).filter(_.original.side == side)
    val purposes = arms.map(arm => preparation.exists(_.root eq arm.path) ||
      functional.exists(_.question.rootMove == arm.path.moves.head) || purposeKnown(arm, uses) ||
        replyActivities.exists(use => CommentarySequenceText.sameRoot(use.path, arm.path)))
    val differentFate = originals.filterNot(_.role.name == "king").exists(p =>
      arms.map(a => current(a.path, p, a.through + 1).nonEmpty).distinct.size == 2)
    // Equal material and identical surviving pieces can still leave different
    // pawn structures. Compare the same original pawn, not just the loss list.
    val differentPawn = changedPawns.exists(p =>
      arms.map(a => current(a.path, p, a.through + 1)).distinct.size == 2)
    val full = purposes.forall(identity) && (preparation.nonEmpty || functional.nonEmpty || post.nonEmpty || shared.nonEmpty ||
        arms.exists(_.controlUse.nonEmpty) || arms.forall(_.captured.nonEmpty) && sameAftermath(paths, arms) ||
        changedPawns.nonEmpty && (differentPawn || differentFate || contrasts.nonEmpty) ||
        (uses.nonEmpty || exchangeTasks.nonEmpty) && differentFate)
    // Keep the actual question when its response and cost are observable but
    // one root's longer purpose is not. This partial account cannot terminate
    // investigation and must not displace a fuller owned-purpose answer.
    // A quiet reply can still lead to a certified material, pawn or function
    // difference. Requiring a new first-reply attack discarded that comparison
    // and resurrected an unrelated long-PV inventory.
    val partial = purposes.exists(identity) && (preparation.nonEmpty || functional.nonEmpty || replyActivities.nonEmpty ||
      differentFate || differentPawn || contrasts.nonEmpty)
    if !full && !partial then None
    else Some(new Reading(paths, post, focused.head, focused.last,
      current(arms.head.path, arms.last.actor, arms.head.through + 1), shared,
      contrasts, uses,
      (proof.toList ++ primary.toList.map(_.id)).distinct, preparation, functional)
      .withResponseUses(Nil, replyActivities))

  def answer(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths, pair: List[PreparedPath],
      reading: Reading, complete: Answer => Option[Answer],
      responses: Option[MovePurpose.Reading] = None): Option[Answer] =
    if !reading.belongsTo(paths) || reading.arms.map(_.path).toSet != pair.toSet then return None
    val prices = scala.collection.mutable.ListBuffer.empty[Material]
    def continuation(path: PreparedPath, through: Int, statement: Statement, priceHere: Boolean = false): Section =
      val refs = (0 to through).map(MoveRef(path, _)).toList
      val material = path.certified.get._2.material.get
      val events = material.events.filter(_.plyOffset <= through)
      val cost = CommentaryMaterialAccount.calculate(material, events.map(_.plyOffset),
        refs.map(_.offset), path.moves.size).map(Material(path, _))
      if !priceHere then prices ++= cost
      // The cost keeps every action/proof reference; the position states the
      // reply and bounded result without reciting the whole PV a second time.
      val checkOffsets = path.certified.get._2.lineEvents.filter(e =>
        e.kind == LineEventKind.Check && e.plyOffset <= through).map(_.plyOffset)
      val checking = MoveWorkProjection.checkSteps(path, checkOffsets, None,
        packet.observations.groupBy(_.transition.evidence.id)).map(_._2)
      Section(List(statement) ++ (if priceHere then cost.toList else Nil) ++
        Option.when(checking.nonEmpty)(CheckSequence(MoveRef(path, 0), checking)).toList ++
        Option.when(cost.isEmpty && events.nonEmpty)(Continuation(refs)).toList ++
        Option.when(events.nonEmpty && cost.isEmpty)(MaterialEvents(path, events)).toList ++
        Option.when(through == path.moves.size - 1)(RecordedEnd(MoveRef(path, through))).toList)
    val continuations = reading.arms.filterNot(arm => reading.preparation.exists(_.root eq arm.path)).map(arm =>
      continuation(arm.path, arm.through, ReferencePosition(reading, arm)))
    val alternatives = responses.filter(value => value.belongsTo(paths) &&
      value.question.rootMove == reading.played.path.moves.head &&
      value.question.path.positions.head == reading.played.path.positions.head).toList.flatMap { value =>
      def task(branch: MovePurpose.Branch) =
        (branch.pressure.map(p => (p.targets.toSet, p.sameTask)),
          branch.support.map(s => (s.asset, s.sameDuty, s.provider.nonEmpty, s.blockaderRemains)),
          branch.realizes,
          branch.controllers.toSet, branch.occupant, branch.relocated.toSet)
      val observed = value.branches.find(_.path.moves(1) == reading.played.path.moves(1))
      val supported = value.branches.flatMap(_.purposeActivity)
      val conditionalDifference = (observed.isEmpty || reading.functional.exists(_ eq value) &&
        value.witnessedSupport.nonEmpty) && value.branches.exists(_.realizes) &&
        value.branches.map(task).distinct.size > 1
      if conditionalDifference || supported.nonEmpty && (supported.size != value.branches.size || observed.isEmpty ||
          supported.size == 2 && (!supported.head.sameTask(supported(1)) || supported.head.differentOutcome(supported(1)))) then
        // The same first reply can lead to a different returned continuation.
        // Compare both owned realizations and prices, not the reply name alone.
        val cause = Option.when(conditionalDifference && supported.isEmpty &&
          value.question.plan.isEmpty && value.question.support.isEmpty && value.question.activity.isEmpty)(
          Section(List(FunctionPurpose(value)))).toList
        cause ++ value.branches.map { branch =>
          continuation(branch.path, branch.through, FunctionReply(value, branch), priceHere = true)
        }
      else value.branches.filter(branch => !reading.functional.exists(_.witnessedPlan.exists(_.path eq branch.path)) &&
        branch.path.moves(1) != reading.played.path.moves(1) &&
        (if value.question.activity.nonEmpty then
          branch.activity.exists(_.mover.nonEmpty != reading.played.retainedMover.nonEmpty)
        else observed.exists(main => task(main) != task(branch)))).take(1).map { branch =>
          val statement = if value.question.activity.nonEmpty then ReferenceExchangeReply(reading, value, branch)
            else FunctionReply(value, branch)
          continuation(branch.path, branch.through, statement, priceHere = true)
        }
    }
    val replyRanking = responses.toList.filter(value => value.question.replyActivities.nonEmpty &&
      value.branches.size == 2 && value.search.belongsTo(packet)).flatMap { value =>
      val first = value.branches.head.path
      val second = value.branches.last.path
      value.search.pairAnswer(packet, first.moves(1), second.moves(1)).flatMap(_.qualityBand)
        .filter(_ == PrincipalVariationComparison.QualityBand.Separated).map(quality =>
          Section(List(FirstReplyChoice(MoveRef(first, 0), MoveRef(first, 1), MoveRef(second, 1), quality))))
    }
    val uses = reading.rootUses.filterNot(statement => reading.preparation.exists(value =>
      statement.references.exists(move => (move.path eq value.root) && move.offset == 0)))
    val lead = Section(ReferencePurpose(reading) :: (uses ++ prices.toList),
      (uses ++ prices.toList).flatMap(_.references).distinct)
    val preparedReplies = reading.preparation.toList.flatMap(MoveIntent.sections)
    val functionalPlans = reading.functional.toList.flatMap(value => value.witnessedPlan.toList.filterNot(plan =>
      alternatives.flatMap(_.statements).exists {
        case FunctionReply(_, branch) => branch.purposeActivity.exists(activity =>
          (activity.path eq plan.path) && activity.original == value.question.actor && activity.use == plan.use &&
            plan.targets.forall((activity.attacks ++ activity.guards).contains))
        case _ => false
      }).map(plan =>
      continuation(plan.path, plan.through, FunctionPurpose(value), priceHere = true)) ++
      value.witnessedSupport.toList.filterNot(_ => alternatives.flatMap(_.statements).exists {
        case FunctionPurpose(existing) => existing eq value
        case _ => false
      }).map(branch => continuation(branch.path, branch.through, FunctionPurpose(value), priceHere = true)))
    // Each bounded aftermath now owns its selected functional differences.
    // A second inventory/comparison paragraph would repeat the same duties.
    RootQuestionSummary.completePairAnswer(packet, paths, pair,
      if reading.preparation.nonEmpty then "functional_response" else "selected_comparison",
      lead :: (functionalPlans ++ continuations ++ preparedReplies ++ replyRanking ++ alternatives))
      .map(value => value.copy(evidence = (value.evidence ++ reading.evidence).distinct))
      .map(value => if reading.preparation.isEmpty then value else value.copy(
        completionSources = Some(value.core.flatMap(_.statements).filterNot {
          case _: Material | _: MaterialEvents => true
          case _ => false
        })))
      .flatMap(complete).flatMap { completed =>
        val covered = completed.core.flatMap(_.covered).toSet
        val ends = reading.arms.map(arm => arm.path ->
          covered.filter(_.path eq arm.path).map(_.offset).foldLeft(arm.through)(math.max)).toMap
        if reading.arms.forall(arm => ends(arm.path) == arm.through) then Some(completed)
        else
          // Pricing may own a later connected loss. Refresh only the board
          // account to that already selected price; do not seed new episodes
          // or repeatedly expand the interval from newly discovered facts.
          val selectedUses = reading.arms.map(arm => Section(reading.rootUses.filter(
            _.references.exists(_.path eq arm.path))) -> reading.evidence)
          read(packet, paths, pair, selectedUses, ends, reading.preparation, reading.functional).filter(next => next.arms.forall(arm =>
            arm.path.certified.get._2.material.get.events.filter(_.plyOffset <= arm.through)
              .forall(event => covered(MoveRef(arm.path, event.plyOffset))))).map { next =>
            def replace(statement: Statement): Statement = statement match
              case ReferencePurpose(value) if value eq reading => ReferencePurpose(next)
              case ReferencePosition(value, arm) if value eq reading =>
                ReferencePosition(next, next.arms.find(_.path eq arm.path).get)
              case ReferenceExchangeReply(value, purpose, branch) if value eq reading =>
                ReferenceExchangeReply(next, purpose, branch)
              case other => other
            val core = completed.core.map { section =>
              val statements = section.statements.map(replace)
              val finalPositions = statements.collect { case ReferencePosition(_, arm) if
                  arm.through == arm.path.moves.size - 1 &&
                  !statements.contains(RecordedEnd(MoveRef(arm.path, arm.through))) =>
                RecordedEnd(MoveRef(arm.path, arm.through)) }
              section.copy(statements = statements ++ finalPositions)
            }
            completed.copy(core = core, evidence = (completed.evidence ++ next.evidence).distinct)
          }
      }.map(_.copy(details = Nil))
