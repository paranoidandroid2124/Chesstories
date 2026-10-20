package io.chesstory.runtime

import lila.chessjudgment.model.judgment.*
import ObservedLineProtocol.{PreparedPath, PreparedPaths}
import MoveWorkProjection.Piece
import CommentaryAnswer.*
import CommentaryEnglish.at
import RootQuestionSummary.MoveRef

/** A root-created opportunity belongs to the piece that actually uses it.
  * Read its first useful activity and the immediate answer to that activity,
  * rather than treating arrival, attack, retreat and recovery as unrelated facts.
  */
private[runtime] object PurposeActivityReading:
  final case class SupportWitness(follows: Boolean, evidence: List[String])

  def rootSupport(paths: PreparedPaths, path: PreparedPath, provider: Piece, original: Piece,
      destination: String, through: Int): Option[SupportWitness] =
    if !paths.paths.exists(_ eq path) || through < 1 || through > path.moves.size ||
        provider == original || provider.side != original.side then return None
    val positions = (0 to through).toList.map(paths.purposePosition(path, _)
      .filter(_.complete(RelationFactKind.GeometricControl)))
    if positions.exists(_.isEmpty) then return None
    val boards = positions.flatten
    for
      moved <- paths.trackedPieceAfter(path, provider, 1).flatten
      if boards.head.occupant(path.moves.head.take(2)).contains(provider) &&
        paths.trackedPieceAfter(path, original, through).flatten.exists(_.at == destination) &&
        (1 to through).forall(count => paths.trackedPieceAfter(path, provider, count).flatten.contains(moved))
      direct = !boards.head.controlledBy(provider)(destination) &&
        boards.tail.forall(_.controlledBy(moved)(destination))
      following = !boards.head.controlledBy(provider)(original.at) && (1 to through).forall(count =>
        paths.trackedPieceAfter(path, original, count).flatten.exists(user => boards(count).controlledBy(moved)(user.at)))
      if direct || following
    yield SupportWitness(!direct, boards.map(_.view.occurrence.id) ++ boards.zipWithIndex.flatMap { (at, count) =>
      paths.trackedPieceAfter(path, original, count).flatten.toList.flatMap(user =>
        at.sourceRefs(at.sources(if count == 0 then provider else moved, List(user.at, destination))))
    })

  final case class Seed(path: PreparedPath, original: Piece, entry: Int,
      mechanism: Option[Statement], provider: Option[Piece], evidence: List[String],
      control: Option[ReferencePurposeReading.ControlUse] = None, followingSupport: Boolean = false):
    def cause: Option[String] = mechanism match
      case Some(_: SquareReoccupation) => Some("cleared_square")
      case Some(_: ClearedRoute) => Some("cleared_route")
      case _ => provider.map(_ => "new_support").orElse(control.map(use =>
        if use.removed then "reply_exchange" else "reply_block"))
    def sameEntry(other: Seed): Boolean = original == other.original &&
      CommentarySequenceText.sameRoot(path, other.path) &&
      path.moves(entry).slice(2, 4) == other.path.moves(other.entry).slice(2, 4) &&
      provider == other.provider && cause == other.cause &&
      control.map(_.controller) == other.control.map(_.controller)
  final class Reading private[PurposeActivityReading](private val owner: PreparedPaths,
      val seed: Seed, val use: Int, val actor: Piece, val attacks: List[Piece],
      val guards: List[Piece], val capture: Option[Piece], val retreat: Option[(Int, Piece, Piece)],
      val recovery: Option[(Int, Piece)],
      val continuation: Option[(ReferencePurposeReading.ControlUse, List[Piece])],
      val through: Int, val evidence: List[String]):
    def belongsTo(paths: PreparedPaths): Boolean = owner eq paths
    def path = seed.path
    def original = seed.original
    def entry = seed.entry
    def preparations: List[Int] = seed.mechanism.collect {
      case route: ClearedRoute => route.preparations.map(_.offset)
    }.getOrElse(Nil)
    def incidentalDuty: Boolean = seed.cause.isEmpty && attacks.isEmpty && capture.isEmpty &&
      recovery.isEmpty && continuation.isEmpty
    def exchangedAtEnd: Boolean = owner.trackedPieceAfter(path, original, through + 1).contains(None)
    def recapturedGuard: Option[Piece] = recovery.flatMap { _ =>
      owner.purposePosition(path, use + 1).flatMap(_.view.legalRows)
        .flatMap(_.find(_.move == path.moves(use + 1))).flatMap(_.capture).filter(guards.contains)
    }
    def route: List[Int] = seed.mechanism.collect { case value: PieceRoute => value.offsets }.getOrElse(
      (entry to use by 2).filter(n => owner.trackedPieceAfter(path, original, n).flatten
        .exists(_.at == path.moves(n).take(2))).toList)
    def recapturingKing: Option[(Int, Piece)] = recovery.flatMap { (capture, _) =>
      val offset = capture + 1
      for
        _ <- Option.when(offset <= through)(())
        victim <- owner.trackedPieceAfter(path, original, offset).flatten
        at <- owner.purposePosition(path, offset)
        row <- at.view.legalRows.flatMap(_.find(_.move == path.moves(offset)))
        if row.actor.role.name == "king" && row.capture.contains(victim)
        after <- owner.purposePosition(path, offset + 1)
        king <- after.occupant(row.to.key)
      yield offset -> king
    }
    def references = (List(0, 1, entry, use) ++ route ++ preparations ++ retreat.map(_._1) ++ recovery.map(_._1) ++
      recapturedGuard.map(_ => use + 1) ++
      recapturingKing.map(_._1) ++ seed.control.map(_.change) ++
      continuation.toList.flatMap((next, _) => List(next.change, next.use)))
      .distinct.map(MoveRef(path, _))
    def expresses(statement: Statement): Boolean = seed.mechanism.contains(statement) || (statement match
      case value: SquareReoccupation if preparations.contains(value.move.offset) &&
          (value.move.path eq path) && (value.use.path eq path) && value.use.offset == entry &&
          value.originalEntry == original && value.square == path.moves(entry).slice(2, 4) => true
      case value: SquareReoccupation => seed.sameEntry(Seed(value.use.path,
        value.originalEntry, value.use.offset, Some(value), None, Nil))
      case value: ClearedRoute => seed.sameEntry(Seed(value.use.path,
        value.originalUser, value.use.offset, Some(value), None, Nil))
      case Action(move, MoveWorkProjection.Detail.Attack(user, target, true)) =>
        move == MoveRef(path, use) && user == actor && attacks.contains(target)
      case Action(move, MoveWorkProjection.Detail.Support(user, target, _, true)) =>
        move == MoveRef(path, use) && user == actor && guards.contains(target)
      case _ => false)
    def sameTask(other: Reading): Boolean = seed.sameEntry(other.seed)
    private def losses = path.certified.get._2.material.get.events.filter(_.plyOffset <= through)
      .flatMap(_.capture).map(c => (!c.side.white, c.capturedRole.name)).sorted
    def differentOutcome(other: Reading): Boolean = sameTask(other) &&
      (losses != other.losses || recovery.map(_._2) != other.recovery.map(_._2) ||
        attacks.toSet != other.attacks.toSet || guards.toSet != other.guards.toSet ||
        continuation.map((next, targets) => (next.originalUser, next.user, targets.toSet)) !=
          other.continuation.map((next, targets) => (next.originalUser, next.user, targets.toSet)) ||
        owner.trackedPieceAfter(path, original, through + 1) !=
          owner.trackedPieceAfter(other.path, original, other.through + 1))
    def sameActivity(other: Reading): Boolean =
      def identity(r: Reading, piece: Piece, count: Int) = owner.purposePosition(r.path, 0)
        .flatMap(_.controls.keys.find(p => owner.trackedPieceAfter(r.path, p, count).flatten.contains(piece)))
      def contacts(r: Reading) = (r.attacks.filterNot(p => r.recovery.exists(_._2 == p)) ++ r.guards)
        .map(p => (p, identity(r, p, r.use + 1))).toSet
      sameTask(other) && actor == other.actor && contacts(this) == contacts(other) &&
        retreat.map((_, before, after) => (identity(this, before, use + 1), before, after)) ==
          other.retreat.map((_, before, after) => (identity(other, before, other.use + 1), before, after))

  def discover(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths, path: PreparedPath,
      mechanisms: List[(Section, List[String])]): List[Reading] =
    entries(packet, paths, path, mechanisms).flatMap(read(paths, _))
      .sortBy(r => (if r.incidentalDuty then 1 else 0, r.entry, r.use, r.original.at))

  def choose(values: List[Reading]): Option[Reading] =
    val first = values.find(!_.incidentalDuty).orElse(values.headOption)
    first.flatMap { contact =>
      // A temporary contact is subsumed only by activity enabled by that same
      // original piece's clearance. Later activity or survival alone is no priority.
      values.find { later =>
        val enabledByContact = later.seed.mechanism.exists {
          case route: ClearedRoute => route.before == contact.original
          case route: SquareReoccupation => route.originalActor == contact.original
          case _ => false
        }
        (later.preparations.contains(contact.use) || enabledByContact) && later.use > contact.through &&
          contact.capture.exists(_.role == contact.original.role) &&
          contact.retreat.isEmpty && contact.recovery.isEmpty && contact.continuation.isEmpty &&
          contact.attacks.forall(_.role.name != "king") &&
          contact.path.moves.take(contact.through + 1) == later.path.moves.take(contact.through + 1) &&
          contact.path.positions.take(contact.through + 2) == later.path.positions.take(contact.through + 2) &&
          contact.exchangedAtEnd
      }.orElse(Some(contact))
    }

  def discoverReplies(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths, path: PreparedPath,
      mechanisms: List[(Section, List[String])]): List[Reading] =
    (discover(packet, paths, path, mechanisms) ++ ReferencePurposeReading.counterplay(packet, paths, path)
      .map(_.activity).toList).sortBy(r => (if r.incidentalDuty then 1 else 0, r.entry, r.use, r.original.at))

  def followup(paths: PreparedPaths, path: PreparedPath, original: Piece, entry: Int,
      evidence: List[String], control: Option[ReferencePurposeReading.ControlUse] = None): Option[Reading] =
    if !paths.paths.exists(_ eq path) || path.certified.isEmpty || path.ids != path.declared ||
        path.moves != path.sourceMoves then None
    else read(paths, Seed(path, original, entry, None, None, evidence, control))

  // Start with the engine's strongest reply; a task label must not substitute
  // an easier reply just because the original beneficiary is absent there.
  def replyPair(packet: EvidenceBackedJudgmentPacket, search: TargetResponseInvestigation.Search,
      activities: Map[PreparedPath, List[Reading]], preferred: Option[Reading] = None): Option[List[Reading]] =
    if !search.belongsTo(packet) || !search.coversAllLegalMoves then return None
    def selected(path: PreparedPath) =
      val values = activities.getOrElse(path, Nil).filterNot(_.incidentalDuty)
      preferred.flatMap(task => values.find(_.sameTask(task))).orElse(choose(values))
    for
      first <- search.lines.headOption.flatMap(line => search.pathsByMove.get(line.evaluation.moves.head))
      main <- selected(first)
      peers = search.lines.drop(1).flatMap(line => search.pathsByMove.get(line.evaluation.moves.head))
        .flatMap(selected).filter(other => (!main.sameTask(other) || main.differentOutcome(other)) &&
          search.pairAnswer(packet, first.moves(1), other.path.moves(1)).flatMap(_.qualityBand).nonEmpty)
      other <- peers.headOption
    yield List(main, other)

  def entries(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths, path: PreparedPath,
      mechanisms: List[(Section, List[String])]): List[Seed] =
    paths.requireOwner(packet)
    if !paths.paths.exists(_ eq path) || path.certified.isEmpty || path.ids != path.declared ||
        path.moves != path.sourceMoves || path.moves.size < 3 then return Nil
    val explicit = mechanisms.flatMap { (section, evidence) => section.statements.flatMap {
      case value: SquareReoccupation if value.move == MoveRef(path, 0) && (value.use.path eq path) =>
        Some(Seed(path, value.originalEntry, value.use.offset, Some(value), None, evidence))
      case value: ClearedRoute if value.move == MoveRef(path, 0) && (value.use.path eq path) &&
          PurposeEvidence.retainsRouteUse(paths, value) =>
        Some(Seed(path, value.originalUser, value.use.offset, Some(value), None, evidence))
      case value: PieceRoute if (value.path eq path) && value.offsets.headOption.contains(0) && value.offsets.size >= 2 =>
        Some(Seed(path, value.original, value.offsets.last, Some(value), None, evidence))
      case _ => None
    }}
    (explicit ++ supportedEntries(packet, paths, path)).distinctBy(s => (s.original, s.entry, s.provider))

  // Scheduling and explanation use the same physical beneficiary. Reaching
  // the same square with a replacement piece does not witness this task.
  def supportedEntries(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      path: PreparedPath): List[Seed] =
    paths.requireOwner(packet)
    if !paths.paths.exists(_ eq path) || path.certified.isEmpty || path.ids != path.declared ||
        path.moves != path.sourceMoves || path.moves.size < 3 then return Nil
    for
      before <- paths.purposePosition(path, 0).toList.filter(_.complete(RelationFactKind.GeometricControl))
      provider <- before.occupant(path.moves.head.take(2)).toList
      entry <- (2 until math.min(path.moves.size, 18) by 2).toList
      ready <- paths.purposePosition(path, entry).toList
      user <- ready.occupant(path.moves(entry).take(2)).toList
      original <- before.controls.keys.find(p => p != provider && p.side == provider.side &&
        paths.trackedPieceAfter(path, p, entry).flatten.contains(user)).toList
      destination = path.moves(entry).slice(2, 4)
      support <- rootSupport(paths, path, provider, original, destination, entry + 1).toList
    yield Seed(path, original, entry, None, Some(provider), support.evidence,
      followingSupport = support.follows)

  private def read(paths: PreparedPaths, seed: Seed): Option[Reading] =
    val path = seed.path
    def position(count: Int) = paths.purposePosition(path, count).filter(_.complete(RelationFactKind.GeometricControl))
    def current(piece: Piece, count: Int) = paths.trackedPieceAfter(path, piece, count).flatten
    def original(piece: Piece, count: Int) = position(0).flatMap(_.controls.keys.find(p => current(p, count).contains(piece)))
    for
      before <- position(0)
      if seed.entry >= 1 && seed.entry < math.min(path.moves.size, 18)
      found <- (seed.entry until math.min(path.moves.size, 18) by 2).iterator.flatMap { offset =>
        for
          prior <- current(seed.original, offset)
          after <- position(offset + 1)
          actor <- current(seed.original, offset + 1)
          if path.moves(offset).take(2) == prior.at && actor.at == path.moves(offset).slice(2, 4) &&
            (seed.entry + 1 to offset + 1).forall(count => current(seed.original, count).exists(_.at != seed.original.at))
          row <- position(offset).flatMap(_.view.legalRows).flatMap(_.find(_.move == path.moves(offset)))
          targets = after.controls.keys.toList.filter(p => p != actor && after.controlledBy(actor)(p.at) &&
            original(p, offset + 1).exists(was => !before.controlledBy(seed.original)(was.at)))
          attacks = targets.filter(_.side != actor.side).sortBy(_.at)
          guards = targets.filter(p => p.side == actor.side && after.controllers(p.at, !p.side).nonEmpty).sortBy(_.at)
          if attacks.nonEmpty || guards.nonEmpty || row.capture.nonEmpty
        yield (offset, actor, attacks, guards, row.capture)
      }.take(1).toList.headOption
    yield
      val (use, actor, attacks, guards, capture) = found
      val retreat = for
        move <- path.moves.lift(use + 1)
        target <- attacks.find(_.at == move.take(2))
        identity <- original(target, use + 1)
        relocated <- current(identity, use + 2)
        last <- position(use + 2)
        if relocated.at != target.at && last.contains(actor) && !last.controlledBy(actor)(relocated.at)
      yield (use + 1, target, relocated)
      // Only the next move by this same beneficiary can cash in its own contact.
      // A quiet intervening own plan cannot borrow an arbitrary later capture.
      val recovery = for
        move <- path.moves.lift(use + 2)
        at <- position(use + 2)
        user <- current(seed.original, use + 2)
        if move.take(2) == user.at
        row <- at.view.legalRows.flatMap(_.find(_.move == move))
        target <- row.capture
        attacked = attacks.contains(target) && (use + 1 to use + 2).forall(count => position(count).exists(p =>
          p.contains(target) && current(seed.original, count).exists(a => p.controlledBy(a)(target.at))))
        recaptured = position(use + 1).flatMap(_.view.legalRows)
          .flatMap(_.find(_.move == path.moves(use + 1))).exists(reply =>
            reply.capture.exists(guards.contains) && reply.to.key == target.at &&
              position(use + 2).exists(_.occupant(reply.to.key).contains(target)))
        if attacked || recaptured
      yield (use + 2, target)
      def settled(offset: Int): Int =
        var end = ExchangeActivity.settled(path, offset)
        while end + 1 < path.moves.size && path.certified.get._2.lineEvents.exists(e =>
            e.plyOffset == end && e.kind == LineEventKind.Check) do
          end = ExchangeActivity.settled(path, end + 1)
        end
      val exchangeEnd = settled(recovery.map(_._1).orElse(retreat.map(_._1)).getOrElse(use))
      // An exchanged opponent may have guarded the next piece's entry. Only
      // the next own turn can continue this local activity; do not bridge an
      // intervening own plan to an arbitrary later gain.
      val continuation = (for
        next <- (exchangeEnd + 1 until math.min(path.moves.size, 18)).find(_ % 2 == use % 2).toList
        ready <- position(next).toList
        moving <- ready.occupant(path.moves(next).take(2)).filter(_.side == actor.side).toList
        identity <- original(moving, next).filter(_ != seed.original).toList
        after <- position(next + 1).toList
        user <- current(identity, next + 1).filter(_.at == path.moves(next).slice(2, 4)).toList
        atActivity <- position(use + 1).toList
        controller <- attacks.filter(p => atActivity.controlledBy(p)(user.at))
        controllerIdentity <- original(controller, use + 1).toList
        change <- (use + 1 to exchangeEnd).find(n => current(controllerIdentity, n).nonEmpty &&
          paths.trackedPieceAfter(path, controllerIdentity, n + 1).contains(None)).toList
        if (change + 1 to next + 1).forall(n => paths.trackedPieceAfter(path, controllerIdentity, n).contains(None))
        previous <- current(identity, use + 1).toList
        targets = after.controls.keys.toList.filter(p => p.side != user.side && after.controlledBy(user)(p.at) &&
          original(p, next + 1).flatMap(current(_, use + 1)).exists(old => !atActivity.controlledBy(previous)(old.at))).sortBy(_.at)
        if targets.nonEmpty
        remaining = after.controllers(user.at, !user.side)
        sources = atActivity.sourceRefs(atActivity.sources(controller, List(user.at))) ++
          after.sourceRefs(after.sources(user, targets.map(_.at)) ++ remaining.flatMap(p => after.sources(p, List(user.at))))
      yield ReferencePurposeReading.ControlUse(path, controllerIdentity, change, true, None, identity, user,
        next, remaining, sources) -> targets).headOption
      val end = continuation.fold(exchangeEnd)((next, _) => settled(next.use))
      val sources = position(use + 1).toList.flatMap(p => p.sourceRefs(p.sources(actor, (attacks ++ guards).map(_.at))))
      new Reading(paths, seed, use, actor, attacks, guards, capture, retreat, recovery, continuation, end,
        (seed.evidence ++ sources ++ continuation.toList.flatMap(_._1.evidence) ++
          path.ids.take(end + 1) ++ path.positions.take(end + 2).flatMap(_.id)).distinct)
