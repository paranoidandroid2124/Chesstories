package io.chesstory.runtime

import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.analysis.assembly.OccurrenceResources
import play.api.libs.json.*

/** Scheduling and realization share a task. A route witnesses that task;
  * neither a different route nor a relocated target alone makes it identical. */
private[runtime] object MovePurpose:
  import ObservedLineProtocol.{PreparedPath, PreparedPaths}
  import MoveWorkProjection.{Piece, Position, LocalCandidate, Detail}
  import RootQuestionSummary.MoveRef
  import CommentaryAnswer.*
  import CommentaryEnglish.at
  import TargetResponseInvestigation.Search

  final case class Plan(path: PreparedPath, start: Int, use: Int, join: Int,
      route: List[Int], targets: List[Piece], squares: List[String] = Nil, clearance: List[Piece] = Nil):
    def potential = use < 0
    def through = if use >= 0 then ExchangeActivity.settled(path, use) else use
    def json = Json.obj("line_id" -> path.line.ref.id, "start" -> start, "use" -> use,
      "join" -> join, "route" -> route, "targets" -> targets.map(piece => Json.obj(
        "side" -> (if piece.side.white then "white" else "black"), "role" -> piece.role.name,
        "square" -> piece.square.key))) ++ (if potential then Json.obj("squares" -> squares,
          "clearance" -> clearance.map(_.at)) else Json.obj())

  final case class Question(path: PreparedPath, actor: Piece, user: Piece, use: Int,
      square: String, kind: String, challenges: List[String], plan: Option[Plan] = None,
      support: Option[SupportTask] = None, activity: Option[ExchangeActivity.Task] = None,
      purposeActivity: Option[PurposeActivityReading.Reading] = None,
      replyActivities: List[PurposeActivityReading.Reading] = Nil):
    def rootMove = path.moves.head
    def exchangeTarget = plan.toList.flatMap(_.targets).find(target => target.side != actor.side && actor.role.name == "knight" &&
      target.role == actor.role && OccurrenceResources.planningKnightControls(target.at, target.side)(square))
    def anchor = EngineSearchAnchor(rootMove, path.ids.head, path.positions(1).id.get)
    def id = CommentarySelection.stableId("move-purpose", List(path.ids.head, kind, user.square.key, square) ++
      plan.toList.flatMap(_.targets.map(_.square.key)) ++ support.toList.map(_.asset.at))
    def resourcePositions = (path.positions.take(if kind == "supported_advance" && use >= 0 then use + 2 else 2) ++ plan.toList.flatMap(p =>
      path.positions.take(p.join + 1) ++ p.path.positions.take(p.through + 2)) ++
      (purposeActivity.toList ++ replyActivities).flatMap(value => value.path.positions.take(value.through + 2))).flatMap(_.id)
    private def preparationJson(value: PurposeActivityReading.Reading): JsObject =
      if value.preparations.isEmpty then Json.obj() else Json.obj("preparations" -> value.preparations)
    def json: JsObject = Json.obj("id" -> id, "root_move" -> rootMove,
      "root_occurrence_id" -> path.ids.head, "child_position_id" -> path.positions(1).id.get,
      "source_line_id" -> path.line.ref.id, "use_move" ->
        (if activity.nonEmpty then rootMove else if support.nonEmpty then (if square == rootMove.slice(2, 4) then rootMove else rootMove.slice(2, 4) + square) else
          if plan.exists(_.potential) then plan.get.squares.takeRight(2).mkString else
          if use >= 0 then plan.map(_.path).getOrElse(path).moves(use) else
          (if user == actor then rootMove.slice(2, 4) else user.square.key) + square),
      "kind" -> kind, "square" -> square, "challenge_moves" -> challenges) ++
      plan.map(p => Json.obj("plan" -> p.json)).getOrElse(Json.obj()) ++
      support.map(p => Json.obj("support" -> p.json)).getOrElse(Json.obj()) ++
      activity.map(p => Json.obj("activity" -> p.json)).getOrElse(Json.obj()) ++
      purposeActivity.map(value => Json.obj("activity_plan" -> (Json.obj(
        "entry" -> value.entry, "route" -> value.route, "cause" -> value.seed.cause.get,
        "user" -> Json.obj("side" -> (if user.side.white then "white" else "black"),
          "role" -> user.role.name, "square" -> user.at)) ++ preparationJson(value)))).getOrElse(Json.obj()) ++
      (if replyActivities.isEmpty then Json.obj() else Json.obj("reply_activities" -> replyActivities.map(value =>
        Json.obj("line_id" -> value.path.line.ref.id, "entry" -> value.entry, "route" -> value.route,
          "cause" -> value.seed.cause.getOrElse(if value.entry == 1 then "reply_activity" else "relocation"), "user" -> Json.obj(
            "side" -> (if value.original.side.white then "white" else "black"),
            "role" -> value.original.role.name, "square" -> value.original.at)) ++
          value.seed.control.map(use => Json.obj("control" -> Json.obj(
            "controller" -> Json.obj("side" -> (if use.controller.side.white then "white" else "black"),
              "role" -> use.controller.role.name, "square" -> use.controller.at),
            "change" -> use.change, "removed" -> use.removed))).getOrElse(Json.obj()) ++ preparationJson(value))))

  final case class Release(rook: Piece, move: String, entry: String, clearance: List[Piece]):
    def json = Json.obj("rook" -> rook.at, "move" -> move, "entry" -> entry,
      "clearance" -> clearance.map(_.at))
  final case class SupportTask(asset: Piece, post: String, guards: List[Piece],
      blockader: Piece, release: Release):
    def json = Json.obj("asset" -> Json.obj("side" -> (if asset.side.white then "white" else "black"),
      "role" -> asset.role.name, "square" -> asset.at), "guards" -> guards.map(_.at),
      "blockader" -> blockader.at, "release" -> release.json)
  final case class SupportState(asset: Option[Piece], guards: List[Piece], provider: Option[Piece],
      sameDuty: Boolean, blockaderRemains: Boolean, activity: Option[Piece])

  final case class Duty(actor: Piece, targets: List[Piece], guards: List[Piece])
  final case class Guard(target: Piece, remaining: List[Piece], restored: Option[Int], option: Option[String])
  final case class Pressure(use: Int, actor: Piece, targets: List[Piece], sameTask: Boolean)

  final class Branch private[MovePurpose](val path: PreparedPath, val use: Int,
      val actor: Piece, val controllers: List[Piece], val contacts: List[Piece],
      val entrySquares: List[String], val occupant: Option[Piece],
      val pressure: Option[Pressure] = None, val relocated: List[(Piece, Option[Piece])] = Nil,
      val support: Option[SupportState] = None, val activity: Option[ExchangeActivity.State] = None,
      val realizes: Boolean = false, val purposeActivity: Option[PurposeActivityReading.Reading] = None,
      val activityRoute: List[Int] = Nil):
    val through: Int = math.max(ExchangeActivity.settled(path, use), purposeActivity.map(_.through).getOrElse(use))
  final class Reading private[MovePurpose](private val owner: PreparedPaths,
      val question: Question, val search: Search, val branches: List[Branch],
      val guards: List[LocalCandidate], val pawn: Option[(Piece, Piece)], val entrants: List[Piece],
      val retained: List[Duty] = Nil, val released: List[Guard] = Nil,
      val choice: Option[PurposeChoice.Reading] = None,
      val explanation: PurposeEvidence = PurposeEvidence(),
      val preparedChoice: Option[(PreparedPath, List[Piece])] = None):
    lazy val neutralization = choice.flatMap(PurposeChoice.neutralization(_, branches, owner))
    def belongsTo(paths: PreparedPaths): Boolean = owner eq paths
    def commonPreparedTargets: List[(Piece, Option[Piece])] =
      if question.plan.nonEmpty || question.support.nonEmpty || question.activity.nonEmpty || branches.size != 2 then Nil
      else branches.head.relocated.filter { change =>
        preparedChoice.exists(_._2.contains(change._1)) && branches.forall { branch =>
          branch.relocated.contains(change) &&
            owner.trackedPieceAfter(branch.path, change._1, branch.through + 1).contains(change._2) &&
            (change._2.isEmpty || owner.purposePosition(branch.path, branch.through + 1).exists(at =>
              owner.trackedPieceAfter(branch.path, question.user, branch.through + 1).flatten.exists(user =>
                !at.controlledBy(user)(change._2.get.at))))
        }
      }
    def followsSupport: Boolean = question.kind == "supported_advance" && question.use >= 0 &&
      PurposeActivityReading.rootSupport(owner, question.path, question.actor, question.user,
        question.square, question.use + 1).exists(_.follows)
    def witnessedSupport: Option[Branch] = Option.when(question.kind == "supported_advance" && question.use >= 0)(
      branches.find(branch => (branch.path eq question.path) && branch.realizes &&
        branch.use == question.use && PurposeActivityReading.rootSupport(owner, branch.path,
          question.actor, question.user, question.square, question.use + 1).nonEmpty)).flatten
    def witnessedPlan: Option[Plan] = question.plan.filter(p => !p.potential &&
      branches.exists(b => (b.path eq p.path) && b.pressure.exists(_.use == p.use)))
    lazy val activityContinuity = branches.flatMap(_.activity) match
      case left :: right :: Nil => ExchangeActivity.continuity(left, right, question.actor.side)
      case _ => None
    def companions(branch: Branch): List[(Piece, Option[Piece])] =
      retained.filter(duty => duty.actor.role == question.actor.role &&
        owner.trackedPieceAfter(branch.path, question.actor, branch.through + 1).flatten.nonEmpty).flatMap(duty =>
        owner.trackedPieceAfter(branch.path, duty.actor, branch.through + 1).map(duty.actor -> _))
    def ownershipJson = question.json ++ Json.obj("work_id" -> search.work.workId,
      "execution_key_sha256" -> search.work.executionKeySha256) ++
      choice.map(value => Json.obj("choice" -> value.json)).getOrElse(Json.obj()) ++
      preparedChoice.map((path, targets) => Json.obj("prepared_choice" -> Json.obj(
        "line_id" -> path.line.ref.id, "targets" -> targets.map(p => Json.obj(
          "side" -> (if p.side.white then "white" else "black"), "role" -> p.role.name, "square" -> p.at)))))
        .getOrElse(Json.obj())
    def replyContrast: Boolean =
      def outcome(b: Branch) =
        val losses = owner.continuationReadings.find(r => (r.path eq b.path) && r.complete).toList
          .flatMap(_.events).filter(_.plyOffset <= b.through).flatMap(_.originalVictim).toSet
        (b.realizes, b.pressure.map(p => (p.targets.toSet, p.sameTask)), b.relocated.toSet,
          b.support, b.activity.map(a => (a.mover, a.targets, a.successor, a.central, a.queensGone)),
          b.purposeActivity.map(a => (a.original, a.attacks.toSet, a.guards.toSet, a.capture,
            a.recovery.map(_._2), a.continuation.map((use, targets) => (use.originalUser, targets.toSet)))),
          losses)
      branches.size == 2 && outcome(branches.head) != outcome(branches.last)

  private def position(paths: PreparedPaths, path: PreparedPath, count: Int): Option[Position] =
    paths.purposePosition(path, count)

  private def controllers(board: chess.Board, side: chess.Color, square: String): Set[String] =
    val target = chess.Square.fromKey(square).get
    OccurrenceResources.planningControllers(board, side, target)

  private def sameArrangement(left: PreparedPath, right: PreparedPath, count: Int): Boolean =
    left.positions.lift(count).zip(right.positions.lift(count)).exists((a, b) =>
      a.fen.split(" ").take(4).toList == b.fen.split(" ").take(4).toList)

  private def kingDistance(a: String, b: String) = math.max(math.abs(a.head - b.head), math.abs(a.last - b.last))

  /** The pawn is the continuing task; the supporting piece and the active rook
    * are witnesses, not its identity. A future king post is only a question. */
  private def supportTask(root: PreparedPath, original: Piece, before: Position, after: Position): Option[SupportTask] =
    if original.role.name == "pawn" then return None
    val moved = after.occupant(root.moves.head.slice(2, 4)).get
    val tasks = for
      asset <- after.controls.keys.toList.filter(p => p.side == original.side && p.role.name == "pawn" &&
        before.contains(p) && (if p.side.white then p.at.last >= '5' else p.at.last <= '4') &&
        after.view.pawnStates.exists(state => state.square == p.square && state.passed.contains(true)))
      front = asset.at.head.toString + (asset.at.last + (if asset.side.white then 1 else -1)).toChar
      blockader <- after.occupant(front).filter(_.side != asset.side).toList
      guards = after.controllers(asset.at, asset.side)
      if guards.exists(p => Set("rook", "queen")(p.role.name)) && !before.controlledBy(original)(asset.at)
      post <- (if after.controlledBy(moved)(asset.at) then List(moved.at) else
        // shortcut: one quiet king step; longer unseen support routes need their own bounded witness.
        Option.when(original.role.name == "king")(
          after.controlledBy(moved).toList.sorted.filter(square => after.empty(square) &&
            kingDistance(square, asset.at) == 1 && kingDistance(original.at, square) > 1 &&
            after.controllers(square, !asset.side).isEmpty)).getOrElse(Nil))
      row <- before.view.legalRows.toList.flatten.filter(row => row.actor.side == asset.side &&
        row.actor.role.name == "rook" && row.actor != original && row.capture.isEmpty &&
        row.actor.at.last == row.to.key.last && row.actor.at.head != row.to.key.head &&
        (guards.contains(row.actor) || row.actor.at.head == asset.at.head))
      if after.empty(row.to.key) && after.controlledBy(row.actor)(row.to.key)
      entry = row.to.key.head.toString + (if asset.side.white then '7' else '2')
      if entry != row.to.key
      lane = ((math.min(row.to.key.last, entry.last).toInt to math.max(row.to.key.last, entry.last).toInt)
        .map(rank => row.to.key.head.toString + rank.toChar).filter(_ != row.to.key)).toList
      blockers = lane.flatMap(after.occupant)
      if blockers.forall(p => p.side == asset.side && p.role.name == "king")
    yield SupportTask(asset, post, guards, blockader, Release(row.actor, row.move, entry, blockers))
    tasks.sortBy(task => (math.abs(task.release.rook.at.head - task.release.move.charAt(2)),
      task.release.clearance.size, task.asset.at, task.post, task.release.move)).headOption

  /** A multi-step purpose needs an observed use, including interleaved preparation.
    * An alternate move order is a witness only after the arrangements meet;
    * its score and repetition history never transfer to the played line. */
  private def redeployment(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      root: PreparedPath, original: Piece, before: Position, witnesses: List[PreparedPath]): Option[Plan] =
    if Set("pawn", "king")(original.role.name) then return None
    val seeds = (packet.assembly.input.assessmentCandidates.flatMap(line =>
      paths.certifiedPath(packet, line.evaluation.moves)) ++ witnesses).distinct
    val plans = seeds.flatMap { seed =>
      val start = seed.moves.indexOf(root.moves.head)
      val join = if seed.moves.headOption == root.moves.headOption && sameArrangement(root, seed, 1) then Some(1) else
        (3 to math.min(5, math.min(root.moves.size, seed.moves.size)) by 2)
          .find(n => n > start && sameArrangement(root, seed, n))
      join.toList.filter(_ => start >= 0 && start <= 4 && start % 2 == 0 &&
        paths.trackedPieceAfter(seed, original, start).flatten.contains(original)).flatMap { joined =>
        (start + 2 until math.min(seed.moves.size, 18) by 2).flatMap { use =>
          for
            at <- position(paths, seed, use)
            after <- position(paths, seed, use + 1)
            prior <- paths.trackedPieceAfter(seed, original, use).flatten
            arrived <- paths.trackedPieceAfter(seed, original, use + 1).flatten
            if (start + 1 to use + 1).forall(count =>
              paths.trackedPieceAfter(seed, original, count).flatten.exists(_.at != original.at))
            if seed.moves(use).take(2) == prior.at && prior != arrived && use >= joined &&
              at.view.legalRows.exists(_.exists(row => row.move == seed.moves(use) && row.capture.isEmpty))
            targets = before.controls.keys.toList.filter(target => target != original &&
              (if target.side != original.side then !before.controlledBy(original)(target.at)
                else after.controllers(target.at, !original.side).nonEmpty) &&
              !position(paths, root, 1).exists(afterRoot => paths.trackedPieceAfter(root, original, 1)
                .flatten.exists(piece => afterRoot.controlledBy(piece)(target.at))) &&
              (1 to use + 1).forall(count => paths.trackedPieceAfter(seed, target, count).flatten.contains(target)) &&
              paths.trackedPieceAfter(seed, target, use + 1).flatten.exists(current =>
                after.controlledBy(arrived)(current.at))).sortBy(p => (if p.role.name == "pawn" then 1 else 0, p.at))
            if targets.nonEmpty
            route = (start to use by 2).filter(offset => paths.trackedPieceAfter(seed, original, offset)
              .flatten.exists(_.at == seed.moves(offset).take(2))).toList
            if route.size >= 2
          yield Plan(seed, start, use, joined, route, targets)
        }
      }
    }
    plans.sortBy(p => (if p.targets.exists(_.role.name != "pawn") then 0 else 1,
      if p.path == root then 0 else 1, p.use)).headOption

  /** A question can precede a PV realization. This bounded geometric route is
    * conditional on clearing its own king and on the opponent's play; it is
    * never an engine forecast or a certified legal multi-move sequence. */
  private def regrouping(root: PreparedPath, original: Piece, before: Position, after: Position): Option[Plan] =
    if original.role.name != "knight" then return None
    val from = root.moves.head.slice(2, 4)
    def reach(square: String) = OccurrenceResources.planningKnightControls(square, original.side)
    val candidates = for
      via <- reach(from).toList.sorted
      blocker = after.occupant(via)
      if via != original.at && blocker.forall(piece => piece.side == original.side && piece.role.name == "king")
      end <- reach(via).toList.sorted
      if end != from && end != original.at && after.empty(end)
      targets = reach(end).toList.sorted.flatMap(after.occupant).filter(target => target.side != original.side &&
        !before.controlledBy(original)(target.at) &&
        !after.occupant(from).exists(piece => after.controlledBy(piece)(target.at)))
      if targets.nonEmpty && after.controllers(end, !original.side).forall(piece =>
        targets.contains(piece) && piece.role == original.role)
      // Pawn attacks on the intermediate post cannot be wished away by the route.
      if after.controllers(via, !original.side).forall(_.role.name != "pawn")
    yield Plan(root, 0, -1, 1, Nil, targets, List(from, via, end), blocker.toList)
    candidates.sortBy(p => (if p.targets.exists(_.role.name != "pawn") then 0 else 1,
      p.clearance.size, -p.targets.size, p.squares.mkString)).headOption

  def discover(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      witnesses: List[PreparedPath] = Nil): Option[Question] =
    discovered(packet, paths, witnesses).sortBy(q => (questionEvidence(packet, paths, q).priority,
      if q.use >= 0 then q.use else Int.MaxValue, q.id)).headOption

  private def discovered(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      witnesses: List[PreparedPath]): List[Question] =
    paths.requireOwner(packet)
    (for
      played <- packet.assembly.playedTransition
      if !packet.assembly.input.positionHistory.currentPosition.check.yes
      line <- packet.assembly.input.assessmentCandidates.find(_.evaluation.moves.headOption.contains(played.moveUci))
      path <- paths.certifiedPath(packet, line.evaluation.moves)
      before <- position(paths, path, 0)
      after <- position(paths, path, 1)
      original <- before.occupant(played.moveUci.take(2))
      moved <- paths.trackedPieceAfter(path, original, 1).flatten
      if path.certified.flatMap(_._2.material).exists(!_.events.exists(_.plyOffset == 0))
      support = supportTask(path, original, before, after)
      activity = ExchangeActivity.discover(played.moveUci, original, before, after)
        .filter(task => ExchangeActivity.challenges(task, after).nonEmpty)
      plan = redeployment(packet, paths, path, original, before, witnesses)
        .orElse(regrouping(path, original, before, after))
      uses = (2 until math.min(path.moves.size, 14) by 2).toList.flatMap { offset =>
        for
          atUse <- position(paths, path, offset)
          row <- atUse.view.legalRows.toList.flatten.find(_.move == path.moves(offset))
          user <- paths.identitiesAfter(packet, path, offset).collectFirst {
            case (identity, Some(Some(current))) if current == row.actor => identity
          }
          if user.side == original.side && row.capture.isEmpty && row.promotionRole.isEmpty
          kind <- Option.when(offset == 2 && user == original && original.role.name != "pawn" &&
              before.view.legalRows.exists(!_.exists(r => r.actor == original && r.to == row.to)) &&
              after.controlledBy(moved)(row.to.key))("entry_route")
        yield (offset, user, row.to.key, kind)
      }
      supportedSeeds = (path :: witnesses.filter(CommentarySequenceText.sameRoot(path, _))).distinct
        .flatMap(PurposeActivityReading.supportedEntries(packet, paths, _)).filter(seed =>
          position(paths, seed.path, seed.entry).flatMap(_.view.legalRows).exists(_.exists(row =>
            row.move == seed.path.moves(seed.entry) && row.capture.isEmpty && row.promotionRole.isEmpty)))
      supported = supportedSeeds.map(seed =>
        (seed.entry, seed.original, seed.path.moves(seed.entry).slice(2, 4), "supported_advance"))
      entryRank = if original.side.white then '7' else '2'
      entries = Option.when(original.role.name == "rook")(after.controlledBy(moved).toList.sorted.filter(square =>
        square.last == entryRank && after.empty(square) && !before.controlledBy(original)(square))
        .map(square => (-1, original, square, "entry_route"))).getOrElse(Nil)
      // A legal advance newly supported by this move is a question even when
      // the root PV contains the opponent's prevention instead of its use.
      advances = before.view.legalRows.toList.flatten.filter(row => row.actor != original &&
        row.actor.side == original.side && row.actor.role.name == "pawn" && row.capture.isEmpty &&
        row.promotionRole.isEmpty && after.empty(row.to.key) && after.controlledBy(moved)(row.to.key) &&
        !before.controlledBy(original)(row.to.key)).map(row => (-1, row.actor, row.to.key, "supported_advance"))
      history <- packet.assembly.input.positionHistory.extend(List(path.moves.head)).toOption
      candidates = (support.toList.map(task => (-1, original, task.post, "support_release")) ++
        activity.toList.map(_ => (-1, original, moved.at, "exchange_activity")) ++ entries ++ plan.toList.map(p => (p.use, original,
        if p.potential then p.squares.last else p.path.moves(p.use).slice(2, 4), "piece_pressure")) ++
        advances ++ supported ++ uses).distinct.sortBy { (use, user, square, _) =>
          (if use >= 0 then 0 else 1, if use >= 0 then use else 0, user.at, square)
        }
    yield candidates.flatMap { (use, user, square, kind) =>
        val selectedPlan = plan.filter(_ => kind == "piece_pressure")
        val selectedSupport = support.filter(_ => kind == "support_release")
        val selectedActivity = activity.filter(_ => kind == "exchange_activity")
        val watched = selectedSupport.toList.flatMap(task => List(task.asset.at, moved.at)) ++
          selectedPlan.toList.flatMap(_.targets.filter(_.side == original.side).map(_.at)) :+ square
        val challenges = selectedActivity.map(ExchangeActivity.challenges(_, after)).getOrElse(
          history.currentLegalMoveUcis.filter(move => history.extend(List(move)).toOption.exists(next =>
            watched.distinct.exists(at => controllers(next.currentPosition.board, !original.side, at)
              .map(origin => if origin == move.slice(2, 4) then move.take(2) else origin)
              .diff(controllers(history.currentPosition.board, !original.side, at)).nonEmpty || move.slice(2, 4) == at) ||
              selectedSupport.nonEmpty && next.currentPosition.check.yes ||
              selectedPlan.exists(_.targets.exists(_.at == move.take(2))))))
        val source = if kind == "supported_advance" && use >= 0 then supportedSeeds.find(seed =>
          seed.entry == use && seed.original == user && seed.path.moves(use).slice(2, 4) == square).map(_.path).getOrElse(path)
          else path
        Option.when(challenges.nonEmpty)(Question(source, original, user, use, square, kind, challenges,
          selectedPlan, selectedSupport, selectedActivity))
      }).getOrElse(Nil).distinctBy(_.id)

  /** An independent reply survey can reveal the use absent from the short
    * root PV. A forced plan test cannot vote for the plan it was asked to play. */
  def candidate(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths, values: List[Search]): Option[Question] =
    selectedCandidate(packet, paths, values, Nil, Nil).map((q, reading) => reading.fold(q)(_.question))

  private def candidates(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      values: List[Search]): List[Question] =
    val witnesses = values.filter(search => search.belongsTo(packet) &&
      search.anchor.prefixMoves == List(packet.assembly.input.playedMoveUci) &&
      search.coversAllLegalMoves).flatMap(_.pathsByMove.values)
    val existing = discovered(packet, paths, witnesses)
    val root = ReferencePurposeReading.assessmentPair(packet, paths)
      .find(_.moves.head == packet.assembly.input.playedMoveUci)
    val sources = (root.toList ++ witnesses).distinct
    val mechanisms = MoveWorkProjection.select(packet, paths).rootUses(sources)
    // Immediate entry/capture already has its response account. This question
    // carries a beneficiary whose useful role appears only after that entry.
    // The actual assessed line can already contain the delayed useful role.
    // It must enter the same candidate pool as an independent survey, not
    // appear only later when the root paragraph is rendered.
    val activities = sources.flatMap(path => PurposeActivityReading.discover(packet, paths, path, mechanisms))
      .filter(value => value.seed.cause.nonEmpty && value.use > value.entry)
    val enabled = activities.iterator.flatMap { value =>
      for
        before <- position(paths, value.path, 0)
        actor <- before.occupant(value.path.moves.head.take(2))
        if actor != value.original && before.view.currentCheckSources(actor.side).isEmpty &&
          value.path.certified.flatMap(_._2.material).exists(!_.events.exists(_.plyOffset == 0))
        after <- packet.assembly.input.positionHistory.extend(List(value.path.moves.head)).toOption
        watched = List(value.path.moves(value.entry).slice(2, 4), value.actor.at)
        challenges = after.currentLegalMoveUcis.filter(move => after.extend(List(move)).toOption.exists(next =>
          watched.exists(at => move.slice(2, 4) == at ||
            controllers(next.currentPosition.board, !actor.side, at)
              .map(origin => if origin == move.slice(2, 4) then move.take(2) else origin)
              .diff(controllers(after.currentPosition.board, !actor.side, at)).nonEmpty)))
        if challenges.nonEmpty
      yield Question(value.path, actor, value.original, value.use, value.actor.at,
        "enabled_activity", challenges, purposeActivity = Some(value))
    }.toList
    (existing ++ enabled).distinctBy(_.id)

  def question(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths, values: List[Search]): Option[Question] =
    if MoveIntent.discover(packet, paths, values).nonEmpty then None else candidate(packet, paths, values)

  def searches(packet: EvidenceBackedJudgmentPacket, question: Question, values: List[Search]): List[Search] =
    values.filter(value => value.belongsTo(packet) && value.anchor.rootMove == question.rootMove &&
      value.anchor.rootOccurrenceId == question.anchor.rootOccurrenceId &&
      value.anchor.positionId == question.anchor.positionId && value.anchor.prefixMoves == List(question.rootMove))

  /** The same observed task owns both the requested and the returned reply pair.
    * This identifies which question was tested; each returned branch must still
    * supply its own realization and price. */
  def witnessedReplyMoves(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      q: Question, pool: Search): Option[List[String]] =
    if q.use < 0 || !pool.coversAllLegalMoves || searches(packet, q, List(pool)).isEmpty then return None
    val source = q.plan.map(_.path).getOrElse(q.path)
    val present = pool.lines.map(_.evaluation.moves.head)
    val actual = ReferencePurposeReading.assessmentPair(packet, paths).exists(_ eq source)
    val classesReturned = present.exists(q.challenges.contains) && present.exists(move => !q.challenges.contains(move))
    for
      _ <- Option.when(actual || classesReturned)(())
      witness <- source.moves.lift(1)
      other <- present.find(_ != witness)
    yield List(witness, other).sorted

  /** A root refresh changes the current PV, not the provenance of an already
    * issued reply comparison. Recover only the same physical supported action
    * from a completed root receipt that predates that comparison. */
  private def earlierWitnessMoves(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      q: Question, pool: Search, search: Search,
      roots: List[CommentaryInvestigation.EngineCandidates]): List[List[String]] =
    if q.kind != "supported_advance" || q.use < 2 || q.plan.nonEmpty ||
        q.purposeActivity.nonEmpty || q.replyActivities.nonEmpty then return Nil
    def order(id: String) = id.stripPrefix("work:").toIntOption
    for
      receipt <- roots
      work = receipt.work
      if work.searchAnchor.isEmpty && work.engineProfile == search.work.engineProfile &&
        work.requiredDepth >= search.work.requiredDepth &&
        order(work.workId).exists(n => order(search.work.workId).exists(n < _)) &&
        CommentaryInvestigation.ownsSearch(packet.assembly.input.positionHistory, work, receipt.lines) &&
        CommentaryInvestigation.candidatesReady(work, receipt.lines)
      line <- receipt.lines if line.evaluation.moves.headOption.contains(q.rootMove)
      source <- paths.certifiedPath(packet, line.evaluation.moves).toList
      if source.ids.head == q.path.ids.head && source.positions(1) == q.path.positions(1)
      use <- (2 until math.min(source.moves.size, 18) by 2).find(n =>
        source.moves(n) == q.path.moves(q.use) &&
          paths.trackedPieceAfter(source, q.user, n).flatten.exists(_.at == source.moves(n).take(2)) &&
          PurposeActivityReading.rootSupport(paths, source, q.actor, q.user, q.square, n + 1).nonEmpty).toList
      witness <- source.moves.lift(1).toList
      other <- pool.lines.map(_.evaluation.moves.head).find(_ != witness).toList
    yield List(witness, other).sorted

  /** Publish the same owned realization as the answer, including when a later
    * response comparison supplies a different route to the discovered task. */
  def observedQuestion(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      values: List[Search]): Option[Question] =
    read(packet, paths, values, Nil).map(_.question).orElse(question(packet, paths, values))

  def read(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths, values: List[Search],
      notes: List[LocalCandidate], roots: List[CommentaryInvestigation.EngineCandidates] = Nil): Option[Reading] =
    if MoveIntent.discover(packet, paths, values).nonEmpty then None
    else selectedReading(packet, paths, values, notes, roots)

  private def selectedReading(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths, values: List[Search],
      notes: List[LocalCandidate], roots: List[CommentaryInvestigation.EngineCandidates]): Option[Reading] =
    selectedCandidate(packet, paths, values, notes, roots).flatMap(_._2)

  private def questionEvidence(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths, q: Question): PurposeEvidence =
    val pair = ReferencePurposeReading.assessmentPair(packet, paths)
    val compared = directChoice(pair, q) || preparationDiffers(paths, pair, q)
    val usefulEntry = q.use >= 0 && PurposeActivityReading.discover(packet, paths, q.path, Nil).exists(a =>
      a.original == q.user && a.entry == q.use && a.use == a.entry && !a.incidentalDuty)
    val use = Option.when(q.use >= 0 && (directChoice(pair, q) || activePlan(q) ||
        q.purposeActivity.nonEmpty || usefulEntry))(
      MoveRef(q.plan.map(_.path).getOrElse(q.path), q.use)).toList
    PurposeEvidence(use, if compared then pair.map(MoveRef(_, 0)) else Nil,
      corresponding = PurposeEvidence.correspondingEntries(paths, pair, q.user, taskEntry(q)))

  // A route ending in another guard is still a fact until that support is
  // used. The activity reader below can supply its recovery/continuation.
  private def activePlan(q: Question): Boolean =
    q.plan.exists(_.targets.exists(_.side != q.actor.side))

  private def directChoice(pair: List[PreparedPath], q: Question): Boolean =
    pair.exists(path => path.moves.head != q.rootMove &&
      path.moves.head.take(2) == q.user.at && path.moves.head.slice(2, 4) == q.square)

  private def taskEntry(q: Question): String =
    q.purposeActivity.map(a => a.path.moves(a.entry).slice(2, 4))
      .orElse(q.plan.flatMap(p => p.route.drop(1).headOption.map(p.path.moves(_).slice(2, 4))))
      .getOrElse(q.square)

  private def preparationDiffers(paths: PreparedPaths, pair: List[PreparedPath], q: Question): Boolean =
    val entry = taskEntry(q)
    if q.kind == "supported_advance" || q.purposeActivity.exists(_.seed.provider.nonEmpty) then
      val following = q.purposeActivity.map(_.seed.followingSupport).getOrElse(
        q.use >= 0 && PurposeActivityReading.rootSupport(paths, q.path, q.actor, q.user,
          q.square, q.use + 1).exists(_.follows))
      PurposeEvidence.conditionDiffers(paths, pair) { (path, at) =>
        paths.trackedPieceAfter(path, q.actor, 1).flatten.exists(provider =>
          if following then paths.trackedPieceAfter(path, q.user, 1).flatten.exists(user =>
            at.controlledBy(provider)(user.at))
          else at.controlledBy(provider)(entry))
      }
    else PurposeEvidence.entryDiffers(paths, pair, q.user, entry)

  private def selectedCandidate(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths, values: List[Search],
      notes: List[LocalCandidate], roots: List[CommentaryInvestigation.EngineCandidates]): Option[(Question, Option[Reading])] =
    alternatives(packet, paths, values, notes, roots).headOption.map((q, reading, _) => q -> reading)

  private[runtime] def alternatives(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths, values: List[Search],
      notes: List[LocalCandidate] = Nil, roots: List[CommentaryInvestigation.EngineCandidates] = Nil)
      : List[(Question, Option[Reading], PurposeEvidence)] =
    val questions = candidates(packet, paths, values)
    val activityPaths = values.filter(s => s.belongsTo(packet) && s.coversAllLegalMoves &&
      questions.exists(q => s.anchor.scopeKey == q.anchor.scopeKey)).flatMap(_.pathsByMove.values).distinct
    val mechanisms = MoveWorkProjection.select(packet, paths).rootUses(activityPaths)
    val activities = activityPaths.map(path => path ->
      PurposeActivityReading.discoverReplies(packet, paths, path, mechanisms)).toMap
    questions.map { q =>
      val reading = readCandidate(packet, paths, values, notes, roots, q, mechanisms, activities)
      (reading.fold(q)(_.question), reading, reading.map(_.explanation).getOrElse(questionEvidence(packet, paths, q)))
    }.sortBy { (q, _, evidence) =>
      (evidence.priority, if q.use >= 0 then q.use else Int.MaxValue, q.id)
    }

  private def readCandidate(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths, values: List[Search],
      notes: List[LocalCandidate], roots: List[CommentaryInvestigation.EngineCandidates], q: Question,
      activityMechanisms: List[(Section, List[String])],
      activities: Map[PreparedPath, List[PurposeActivityReading.Reading]]): Option[Reading] =
      val choice = Option.when(q.activity.nonEmpty)(PurposeChoice.read(packet, paths, roots)).flatten
      // Preparing the actual alternative does not preserve its function.
      // Keep the original targets so each reply can qualify that comparison.
      val preparedChoice = ReferencePurposeReading.assessmentPair(packet, paths).find(path =>
        q.use >= 0 && path.moves.head != q.rootMove && path.moves.head.take(2) == q.user.at &&
          path.moves.head.slice(2, 4) == q.square).flatMap { path =>
        for
          before <- position(paths, path, 0)
          after <- position(paths, path, 1)
          mover <- paths.trackedPieceAfter(path, q.user, 1).flatten
          targets = before.controls.keys.toList.filter(p => p.side != q.user.side && after.contains(p) &&
            !before.controlledBy(q.user)(p.at) && after.controlledBy(mover)(p.at)).sortBy(_.at)
          if targets.nonEmpty
        yield path -> targets
      }
      val candidates = searches(packet, q, values)
      def activityOf(path: PreparedPath) = q.purposeActivity.flatMap(task =>
        PurposeActivityReading.discover(packet, paths, path, activityMechanisms).find(_.sameTask(task)))
      def activityProgress(path: PreparedPath): List[Int] = q.purposeActivity.toList.flatMap(task =>
        PurposeActivityReading.entries(packet, paths, path, activityMechanisms).find(_.sameEntry(task.seed)).toList.flatMap(seed =>
          (seed.entry until math.min(path.moves.size, 18) by 2).filter(offset =>
            paths.trackedPieceAfter(path, q.user, offset).flatten.exists(_.at == path.moves(offset).take(2)) &&
              (seed.entry + 1 to offset + 1).forall(count => paths.trackedPieceAfter(path, q.user, count)
                .flatten.exists(_.at != q.user.at))).toList))
      def supportActivity(path: PreparedPath): Option[Int] = q.support.flatMap { task =>
        (2 until math.min(path.moves.size, 18) by 2).find { use =>
          paths.identitiesAfter(packet, path, use).exists {
            case (original, Some(Some(current))) => original.side == q.actor.side &&
              Set("rook", "queen")(original.role.name) && original != q.actor &&
              (task.guards.contains(original) || original.at.head == task.asset.at.head) &&
              path.moves(use).take(2) == current.at && current.at.head != path.moves(use).charAt(2)
            case _ => false
          }
        }
      }
      def supportState(path: PreparedPath, use: Int): Option[SupportState] = q.support.flatMap { task =>
        for
          after <- position(paths, path, use + 1)
          asset <- paths.trackedPieceAfter(path, task.asset, use + 1)
        yield
          val guards = asset.toList.flatMap(piece => after.controllers(piece.at, piece.side))
          val provider = paths.trackedPieceAfter(path, q.actor, use + 1).flatten
            .filter(piece => asset.exists(p => after.controlledBy(piece)(p.at)))
          val same = (1 to use + 1).forall(count =>
            paths.trackedPieceAfter(path, task.asset, count).flatten.contains(task.asset) &&
              position(paths, path, count).exists(_.controllers(task.asset.at, task.asset.side).nonEmpty))
          SupportState(asset, guards, provider, same,
            (1 to use + 1).forall(count => paths.trackedPieceAfter(path, task.blockader, count).flatten.contains(task.blockader)),
            supportActivity(path).filter(_ == use).flatMap(_ => after.occupant(path.moves(use).slice(2, 4))))
      }
      def pressureOf(path: PreparedPath): Option[Pressure] = q.plan.flatMap { plan =>
        (2 until math.min(path.moves.size, 18) by 2).toList.flatMap { use =>
          for
            before <- position(paths, path, use)
            after <- position(paths, path, use + 1)
            prior <- before.occupant(path.moves(use).take(2))
            original <- paths.identitiesAfter(packet, path, use).collectFirst {
              case (identity, Some(Some(current))) if current == prior => identity
            }
            actor <- paths.trackedPieceAfter(path, original, use + 1).flatten
            if actor.side == q.actor.side
            targets = plan.targets.filter(target => (2 to use).forall(count =>
              paths.trackedPieceAfter(path, target, count).flatten.contains(target)))
              .flatMap(target => paths.trackedPieceAfter(path, target, use + 1).flatten
              .filter(current => after.controlledBy(actor)(current.at) &&
                !position(paths, path, 1).exists(root => paths.trackedPieceAfter(path, original, 1).flatten
                  .exists(piece => root.controlledBy(piece)(target.at)))))
            if targets.nonEmpty
          yield Pressure(use, actor, targets, q.exchangeTarget.forall(target =>
            actor.role == target.role && targets.contains(target) && after.view.legalRows.exists(_.exists(row =>
              row.actor == target && row.capture.contains(actor)))))
        }.sortBy(value => (if value.sameTask then 0 else 1,
          if paths.trackedPieceAfter(path, q.actor, value.use + 1).flatten
          .contains(value.actor) then 0 else 1, value.use)).headOption
      }
      // Match the function, retaining the actual piece as its witness. A new
      // route or a different pawn can perform the same supported advance.
      def useOf(path: PreparedPath): Option[Int] =
        if q.purposeActivity.nonEmpty then activityOf(path).map(_.use)
        else if q.activity.nonEmpty then ExchangeActivity.use(paths, path, q.actor, q.activity.get)
        else if q.support.nonEmpty then supportActivity(path).orElse(
          (2 until math.min(path.moves.size, 18) by 2).find(offset =>
            paths.trackedPieceAfter(path, q.actor, offset + 1).flatten.exists(_.at == q.square)))
        else if q.plan.nonEmpty then pressureOf(path).map(_.use)
        else if q.kind == "supported_advance" && q.user.role.name != "pawn" then
          PurposeActivityReading.supportedEntries(packet, paths, path).find(seed =>
            seed.original == q.user && path.moves(seed.entry).slice(2, 4) == q.square).map(_.entry)
        else
          (2 until math.min(path.moves.size, 12) by 2).find(offset =>
          path.moves(offset).slice(2, 4) == q.square && position(paths, path, offset).exists(before =>
            before.occupant(path.moves(offset).take(2)).exists(piece => piece.side == q.actor.side &&
              (if q.kind == "supported_advance" then piece.role.name == "pawn" &&
                paths.trackedPieceAfter(path, q.actor, offset).flatten.exists(provider =>
                  before.controlledBy(provider)(q.square))
              else piece.role == q.user.role))))
      def continuingContest(path: PreparedPath): Boolean =
        val old = position(paths, path, 1).get.controllers(q.square, !q.actor.side).toSet
        q.plan.exists(_.targets.exists(target => paths.trackedPieceAfter(path, target, 2).exists(
          _.forall(_.at != target.at)))) ||
          position(paths, path, 3).exists(after => after.controllers(q.square, !q.actor.side).exists(piece =>
          !paths.identitiesAfter(packet, path, 3).exists {
            case (original, Some(Some(current))) => current == piece && old(original)
            case _ => false
          }))
      def relevance(path: PreparedPath): (Int, Int) = useOf(path).fold((1, 0)) { offset =>
        val contacts = position(paths, path, offset + 1).toList.flatMap(after =>
          after.occupant(q.square).toList.flatMap(actor => after.controlledBy(actor).toList
            .flatMap(after.occupant).filter(_.side != actor.side))).size
        (0, -contacts)
      }
      val readings = candidates.reverseIterator.flatMap { search =>
        val contrasted = for
          free <- search.lines.filter(line => !q.challenges.contains(line.evaluation.moves.head))
            .sortBy { line =>
              val path = search.pathsByMove(line.evaluation.moves.head)
              (if q.plan.exists(_.path eq path) then 0 else 1, relevance(path))
            }.headOption
          challenged <- search.lines.filter(line => q.challenges.contains(line.evaluation.moves.head))
            .sortBy { line =>
              val path = search.pathsByMove(line.evaluation.moves.head)
              val matchingReorganization = choice.exists(value => value.arms(1).state.targets.exists {
                case (original, Some(current)) if current != original =>
                  paths.trackedPieceAfter(path, original, 2).flatten.contains(current)
                case _ => false
              })
              (if q.plan.exists(_.path eq path) then 0 else 1,
                if matchingReorganization then 0 else 1, if continuingContest(path) then 0 else 1)
            }.headOption
          if search.pairAnswer(packet, free.evaluation.moves.head, challenged.evaluation.moves.head).nonEmpty
        yield List(search.pathsByMove(free.evaluation.moves.head), search.pathsByMove(challenged.evaluation.moves.head))
        // Geometric challenges need not prevent the task. Preserve an actual
        // independent witness against the best other reply before classifying it.
        val surveyed = for
          _ <- Option.when(search.coversAllLegalMoves && q.use >= 0)(())
          source = q.plan.filterNot(_.potential).map(_.path).getOrElse(q.path)
          witness <- search.pathsByMove.values.find(path => (path eq source) && useOf(path).nonEmpty)
          other <- search.lines.iterator.map(line => search.pathsByMove(line.evaluation.moves.head))
            .find(path => !(path eq witness))
          if search.pairAnswer(packet, witness.moves(1), other.moves(1)).nonEmpty
        yield List(witness, other).sortBy(path => search.lines.indexWhere(_.evaluation.moves.head == path.moves(1)))
        val witnessed = for
          pool <- candidates.takeWhile(_ ne search).reverseIterator.find(previous =>
            previous.work.rootRestriction == EngineRootRestriction.Unrestricted &&
              previous.work.engineProfile == search.work.engineProfile &&
              previous.work.requiredDepth >= search.work.requiredDepth)
          moves <- (witnessedReplyMoves(packet, paths, q, pool).toList ++
            earlierWitnessMoves(packet, paths, q, pool, search, roots))
            .find(moves => search.work.rootRestriction == EngineRootRestriction.Restricted(moves))
          if search.pairAnswer(packet, moves.head, moves.last).nonEmpty
        yield search.lines.filter(line => moves.contains(line.evaluation.moves.head))
          .map(line => search.pathsByMove(line.evaluation.moves.head))
        val replyActivities = PurposeActivityReading.replyPair(packet, search, activities, q.purposeActivity)
        val paired = replyActivities.map(_.map(_.path)).orElse(surveyed).orElse(witnessed).orElse(contrasted)
        paired.flatMap { pair =>
          val branches = pair.map { path =>
            val exact = useOf(path)
            val replyActivity = replyActivities.flatMap(_.find(_.path eq path))
            val differentUse = q.plan.flatMap(_ => (2 until math.min(path.moves.size, 18) by 2).find(offset =>
              q.plan.get.targets.exists(target => (2 to offset).forall(count =>
                paths.trackedPieceAfter(path, target, count).flatten.contains(target))) &&
              paths.trackedPieceAfter(path, q.actor, offset).flatten.exists(_.at == path.moves(offset).take(2)) &&
                paths.trackedPieceAfter(path, q.actor, offset + 1).flatten.exists(actor =>
                  position(paths, path, offset + 1).exists(after => after.controlledBy(actor).exists(square =>
                    after.occupant(square).exists(_.side != q.actor.side))))))
            val progress = activityProgress(path)
            val next = replyActivity.map(_.use).orElse(exact).orElse(progress.lastOption)
              .orElse(differentUse).orElse(Option.when(path.moves.size > 2)(2))
            for
              use <- next
              child <- position(paths, path, 2)
              before <- position(paths, path, use)
              after <- position(paths, path, use + 1)
              prior <- before.occupant(path.moves(use).take(2))
              original <- paths.identitiesAfter(packet, path, use).collectFirst {
                case (identity, Some(Some(current))) if current == prior => identity
              }
              actor <- paths.trackedPieceAfter(path, original, use + 1).flatten
              if before.view.legalRows.exists(_.exists(row => row.move == path.moves(use) && row.actor == prior))
              controls = after.controlledBy(actor)
              targets = controls.toList.sorted.flatMap(after.occupant).filter(_.side != actor.side)
              rank = if actor.side.white then '7' else '2'
              entries = Option.when(actor.role.name == "rook" && actor.square.key.last == rank)(
                controls.toList.filter(s => s.last == rank && after.empty(s))
                  .sortBy(s => (math.abs(s.head - actor.square.key.head), s)).take(2)).getOrElse(Nil)
              originalControllers = position(paths, path, 1).get.controllers(q.square, !q.actor.side).toSet
              gained = child.controllers(q.square, !q.actor.side).filterNot(controller =>
                paths.identitiesAfter(packet, path, 2).exists {
                  case (original, Some(Some(current))) => current == controller && originalControllers(original)
                  case _ => false
                })
              relocated = (q.plan.toList.flatMap(_.targets) ++ preparedChoice.toList.flatMap(_._2)).distinct.flatMap(target =>
                paths.trackedPieceAfter(path, target, use + 1).filter(_.forall(_.at != target.at))
                  .map(target -> _))
            yield new Branch(path, use, actor, gained, targets, entries,
              child.occupant(q.square).filter(_.side != q.actor.side), pressureOf(path), relocated, supportState(path, use),
              q.activity.flatMap(task => ExchangeActivity.state(paths, path, q.actor, task, use)), exact.nonEmpty,
              replyActivity.orElse(activityOf(path)).orElse(Option.when(q.kind == "supported_advance" && q.use >= 0 && exact.nonEmpty)(
                PurposeActivityReading.discover(packet, paths, path, Nil).find(reading =>
                  reading.original == q.user && reading.entry == use &&
                    path.moves(use).slice(2, 4) == q.square)).flatten), progress.filter(_ <= use))
          }
          Option.when(branches.forall(_.nonEmpty) && branches.flatten.exists(branch =>
            if q.activity.nonEmpty then branch.activity.nonEmpty
            else if q.support.nonEmpty then branch.support.nonEmpty
            else if q.plan.nonEmpty then branch.pressure.nonEmpty || branch.relocated.nonEmpty || branch.controllers.nonEmpty
            else branch.realizes || branch.purposeActivity.nonEmpty)) {
            // A later reply comparison may witness the same task on a new PV.
            // Keep its own search receipt; discovery still requires the earlier
            // unrestricted survey, not the restricted test of this task.
            val realized = q.plan.filterNot(_.potential).flatMap { plan =>
              branches.flatten.find(branch => (branch.path eq plan.path) &&
                branch.pressure.exists(_.use == plan.use)).map(_ => plan).orElse {
                branches.flatten.iterator.flatMap { branch =>
                  branch.pressure.filter(pressure => pressure.sameTask &&
                    pressure.actor.at == q.square && plan.targets.forall(pressure.targets.contains) &&
                    plan.targets.filter(_.side == q.actor.side).forall(target =>
                      position(paths, branch.path, pressure.use + 1).exists(_.controllers(target.at, !target.side).nonEmpty)) &&
                    paths.trackedPieceAfter(branch.path, q.actor, pressure.use + 1).flatten.contains(pressure.actor) &&
                    (1 to pressure.use + 1).forall(count =>
                      paths.trackedPieceAfter(branch.path, q.actor, count).flatten.exists(_.at != q.actor.at)))
                    .map { pressure =>
                      val route = (0 to pressure.use).filter(offset =>
                        paths.trackedPieceAfter(branch.path, q.actor, offset).flatten
                          .exists(_.at == branch.path.moves(offset).take(2))).toList
                      Plan(branch.path, 0, pressure.use, 1, route, plan.targets)
                    }
                }.nextOption()
              }
            }
            val supported = Option.when(q.kind == "supported_advance" && q.use < 0 && search.coversAllLegalMoves)(
              branches.flatten.find(b => b.realizes && paths.trackedPieceAfter(b.path, q.user, b.use).flatten
                .exists(_.at == b.path.moves(b.use).take(2)))).flatten
            val witnessed = realized.fold(supported.fold(q)(b => q.copy(path = b.path, use = b.use)))(
              plan => q.copy(use = plan.use, plan = Some(plan)))
              .copy(replyActivities = replyActivities.getOrElse(Nil))
            val root = position(paths, q.path, 1).get
            val guards = notes.filter(note => note.edge.evidence.id == q.path.ids.head && (note.note.detail match
              case Detail.Support(_, target, opposition, _) => opposition.nonEmpty &&
                position(paths, q.path, 0).exists(_.controlledBy(q.actor)(target.square.key))
              case _ => false)).distinctBy(_.note.detail)
            val pawn = guards.iterator.flatMap(note => note.note.detail match
              case Detail.Support(_, target, _, _) if target.role.name == "pawn" &&
                  target.square.key.last == (if target.side.white then '7' else '2') =>
                val promotion = target.square.key.head.toString + (if target.side.white then "8" else "1")
                root.occupant(promotion).filter(_.side != target.side).map(target -> _)
              case _ => None).nextOption()
            val entrants = root.view.legalRows.toList.flatten.filter(row => row.to.key == q.square &&
              row.actor.side != q.actor.side && row.actor.role.name == "knight").map(_.actor).distinct
            val retained = if q.plan.isEmpty then Nil else root.controls.keys.toList.filter(piece =>
              piece.side == q.actor.side && piece.at != q.rootMove.slice(2, 4) && piece.role.name != "king")
              .flatMap(piece =>
                val targets = root.controlledBy(piece).toList.sorted.flatMap(root.occupant).filter(_.side != piece.side)
                val guards = root.controlledBy(piece).toList.sorted.flatMap(root.occupant).filter(target =>
                  target.side == piece.side && root.controllers(target.at, !piece.side).nonEmpty)
                Option.when(targets.nonEmpty)(Duty(piece, targets, guards)))
              .sortBy(duty => (if duty.actor.role == q.actor.role then 0 else 1, duty.actor.at)).take(2)
            val released = witnessed.plan.toList.flatMap { plan =>
              val before = position(paths, q.path, 0).get
              val moved = paths.trackedPieceAfter(q.path, q.actor, 1).flatten.get
              before.controlledBy(q.actor).toList.sorted.flatMap(before.occupant)
                .filter(target => target.side == q.actor.side && root.contains(target) &&
                  !root.controlledBy(moved)(target.at) && root.controllers(target.at, !target.side).nonEmpty)
                .map(target => Guard(target, root.controllers(target.at, target.side), plan.route.find(offset =>
                  paths.trackedPieceAfter(plan.path, q.actor, offset + 1).flatten.exists(piece =>
                    position(paths, plan.path, offset + 1).exists(_.controlledBy(piece)(target.at)))),
                  plan.squares.find(square => OccurrenceResources.planningKnightControls(square, q.actor.side)(target.at))))
            }
            val observed = branches.flatten
            val pair = ReferencePurposeReading.assessmentPair(packet, paths)
            val actualUses = pair.flatMap(path => useOf(path).map(MoveRef(path, _)))
            val compared = directChoice(pair, q) || preparationDiffers(paths, pair, q) ||
              actualUses.map(_.path).distinct.size == 2
            // Arrival is a step, not the useful role. A longer owned activity
            // supplies its own candidate; do not promote its entry as a rival.
            def useful(move: MoveRef): Boolean = activePlan(q) || q.purposeActivity.nonEmpty ||
              q.support.nonEmpty || q.activity.nonEmpty || directChoice(pair, q) ||
              PurposeActivityReading.discover(packet, paths, move.path, activityMechanisms).exists(a =>
                a.original == q.user && a.entry == move.offset && a.use == a.entry && !a.incidentalDuty)
            val uses = (actualUses ++ observed.flatMap(b => useOf(b.path).map(MoveRef(b.path, _)))).filter(useful)
            val answeredReplies = observed.size == 2 && observed.forall(b => b.realizes ||
              b.purposeActivity.nonEmpty || b.controllers.nonEmpty || b.occupant.nonEmpty ||
              b.relocated.nonEmpty || b.activity.nonEmpty || b.support.exists(_.sameDuty))
            val strongest = search.lines.headOption.map(_.evaluation.moves.head)
            val leading = observed.filter(b => strongest.contains(b.path.moves(1))).flatMap { b =>
              val useful = b.purposeActivity.filterNot(_.incidentalDuty).filter(a =>
                a.original == q.user && q.purposeActivity.fold(
                  if q.plan.nonEmpty then b.pressure.exists(_.use == a.use)
                  else useOf(b.path).contains(a.entry))(_.sameTask(a)))
              useful.map(a => MoveRef(a.path, a.use)).toList ++
                Option.when(b.activity.nonEmpty || b.support.exists(_.activity.nonEmpty))(MoveRef(b.path, b.use))
            }
            val explanation = PurposeEvidence(uses.distinct,
              if compared then pair.map(MoveRef(_, 0)) else Nil,
              if answeredReplies then observed.map(b => MoveRef(b.path, 1)) else Nil, leading,
              PurposeEvidence.correspondingEntries(paths, pair, q.user, taskEntry(q)))
            new Reading(paths, witnessed, search, observed, guards, pawn, entrants, retained, released, choice, explanation,
              preparedChoice)
          }
        }
      }.toList
      readings.sortBy(value => (value.explanation.priority,
        if value.witnessedPlan.nonEmpty then 0 else 1)).headOption

  def answer(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths, pair: List[PreparedPath],
      reading: Reading, complete: Answer => Option[Answer]): Option[Answer] =
    if !reading.belongsTo(paths) || pair.size != 2 then return None
    // A played-only reply account or a supplementary same-destination choice
    // cannot own the verdict's comparison by merely naming both roots at the end.
    if !reading.choice.exists(_.arms.map(_.path.moves.head).toSet == pair.map(_.moves.head).toSet) then return None
    val q = reading.question
    val intro = Section(reading.choice.toList.map(ChoicePurpose(_)) ++ List(FunctionPurpose(reading)) ++
      reading.neutralization.toList.map(ChoiceNeutralization(_)) ++ reading.guards.filter(_ => reading.choice.isEmpty).map(note =>
      Action(MoveRef(q.path, 0), note.note.detail)))
    val planCosts = q.plan.toList.map(plan => plan.path -> plan.through).flatMap { (path, through) =>
      path.certified.flatMap(_._2.material).toList.flatMap(_.events)
        .filter(_.plyOffset <= through) match
        case Nil => Nil
        case events => List(Section(List(MaterialEvents(path, events))))
    }
    val branches = reading.branches.map { branch =>
      val refs = (1 to branch.through).map(MoveRef(branch.path, _)).toList
      val events = branch.path.certified.flatMap(_._2.material).toList.flatMap(_.events)
        .filter(_.plyOffset <= branch.through)
      Section(List(Continuation(refs), FunctionReply(reading, branch)) ++
        Option.when(events.nonEmpty)(MaterialEvents(branch.path, events)).toList)
    }
    // The played move already has two response witnesses below. Its extra
    // root PV would repeat the same exchange and crowd out the actual contrast.
    val choices = reading.choice.toList.flatMap(value => value.arms.tail.map { arm =>
      val events = arm.path.certified.flatMap(_._2.material).toList.flatMap(_.events).filter(_.plyOffset <= arm.through)
      Section(List(Continuation((0 to arm.through).map(MoveRef(arm.path, _)).toList), ChoiceReply(value, arm)) ++
        Option.when(events.nonEmpty)(MaterialEvents(arm.path, events)).toList)
    })
    RootQuestionSummary.completePairAnswer(packet, paths, pair, "move_purpose",
      intro :: planCosts ::: choices ::: branches ::: List(Section(List(Continuation(pair.map(MoveRef(_, 0)))))))
      .flatMap(complete)
      // Selected prices have closed in core; the full original ledgers remain
      // on observation paths without duplicating unrelated late PV prose.
      .map(_.copy(details = Nil))
      .filter(value => RootQuestionSummary.realize(paths, pair, value).nonEmpty)
