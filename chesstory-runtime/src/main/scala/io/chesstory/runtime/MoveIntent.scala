package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.OccurrenceResources
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.CanonicalPositionHistory
import ObservedLineProtocol.{PreparedPath, PreparedPaths}
import MoveWorkProjection.{Piece, Position}
import CommentaryEnglish.at
import play.api.libs.json.{Json, JsObject}

/** A preparation needs a useful next action and a causal bridge from the root.
  * Legal examples propose that bridge; only admitted paths can explain it.
  */
private[runtime] object MoveIntent:
  enum Aim:
    case KingAttack, CoordinatedPressure, RookCoordination, CheckingTempo, SupportedActivity

  final case class Question(root: PreparedPath, actor: Piece, user: Piece, action: String,
      aim: Aim, target: Piece, partner: Piece, examples: List[List[String]],
      actionOffset: Int = 2, checkOffsets: List[Int] = Nil):
    def id = CommentarySelection.stableId("move-intent", List(root.ids.head, aim.toString, action, target.at, partner.at))
    def prefix = root.moves.take(2)
    def rootMove = root.moves.head

  private def square(key: String) = chess.Square.fromKey(key).get
  private def piece(history: CanonicalPositionHistory, at: String): Option[Piece] =
    history.currentPosition.board.pieceAt(square(at)).map(p =>
      RelationColoredPieceWitness(EvidenceSquare(at), EvidencePieceRole(p.role.name), p.color))
  private def controls(history: CanonicalPositionHistory, side: chess.Color, at: String): Set[String] =
    OccurrenceResources.planningControllers(history.currentPosition.board, side, square(at))
  private def step(history: CanonicalPositionHistory, move: String): Option[CanonicalPositionHistory] =
    Option.when(history.currentLegalMoveUcis.contains(move))(()).flatMap(_ => history.extend(List(move)).toOption)
  private def distance(a: String, b: String): Int =
    math.max(math.abs(a.head - b.head), math.abs(a.last - b.last))
  private def centrality(at: String): Int =
    math.abs(2 * (at.head - 'a') - 7) + math.abs(2 * (at.last - '1') - 7)

  private def observedUse(paths: PreparedPaths, q: Question, path: PreparedPath): Option[Int] =
    (2 until path.moves.size by 2).find(n => realizes(paths, q, path, n + 1))

  /** Prefer a witnessed useful relationship that distinguishes the actual roots.
    * An action's spelling or its occurrence without that relationship is not evidence. */
  private def replySurvey(packet: EvidenceBackedJudgmentPacket, root: PreparedPath,
      searches: List[TargetResponseInvestigation.Search]): Option[TargetResponseInvestigation.Search] =
    searches.reverse.find(search => search.belongsTo(packet) &&
      search.anchor.prefixMoves == root.moves.take(1) && search.anchor.rootOccurrenceId == root.ids.head &&
      search.anchor.positionId == root.positions(1).id.get &&
      CommentaryInvestigation.candidatesReady(search.work, search.lines) && search.lines.size >= 2 &&
      search.coversAllLegalMoves)

  private[runtime] def explanations(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      pair: List[PreparedPath], questions: List[Question],
      searches: List[TargetResponseInvestigation.Search]): List[(Question, PurposeEvidence)] =
    questions.distinctBy(q => (q.aim, q.action, q.user, q.target, q.partner)).map { q =>
      import RootQuestionSummary.MoveRef
      val use = observedUse(paths, q, q.root)
      val other = pair.find(_.moves.head != q.rootMove).flatMap(observedUse(paths, q, _))
      // Only an all-legal reply survey supplies independent branch evidence.
      // Forcing our own proposed action in a child test must not give it priority.
      val survey = replySurvey(packet, q.root, searches)
      val surveyed = survey.toList.flatMap(search => search.lines.zipWithIndex.flatMap {
        (line, rank) =>
          val path = search.pathsByMove(line.evaluation.moves.head)
          observedUse(paths, q, path).map(n => rank -> MoveRef(path, n))
      })
      val directChoice = pair.exists(path => path.moves.head != q.rootMove && path.moves.head == q.action)
      val actual = pair.flatMap(path => observedUse(paths, q, path).map(MoveRef(path, _)))
      val preparationDiffers = PurposeEvidence.conditionDiffers(paths, pair) { (path, at) =>
        val partner = paths.trackedPieceAfter(path, q.partner, 1).flatten
        val target = paths.trackedPieceAfter(path, q.target, 1).flatten
        q.aim match
          case Aim.KingAttack | Aim.CoordinatedPressure =>
            partner.exists(p => target.exists(t => at.controlledBy(p)(t.at)))
          case Aim.SupportedActivity => partner.exists(p => at.controlledBy(p)(q.action.slice(2, 4)))
          case Aim.RookCoordination => partner.exists(p => p.at.head == q.action.charAt(2)) &&
            paths.trackedPieceAfter(path, q.user, 1).flatten.exists(p =>
              at.empty(q.action.slice(2, 4)) && at.controlledBy(p)(q.action.slice(2, 4)))
          case _ => false
      }
      val compared = directChoice || actual.size == 2 ||
        preparationDiffers ||
        q.aim == Aim.CheckingTempo && q.examples.map(_.head).toSet == pair.map(_.moves.head).toSet
      val evidence = PurposeEvidence(actual ++ surveyed.map(_._2),
        if compared then pair.map(MoveRef(_, 0)) else Nil,
        // One realized branch and another without that relationship can
        // explain a conditional plan. Requiring two successful realizations
        // perversely favors a later contact over the plan's actual limitation.
        if surveyed.nonEmpty then survey.toList.flatMap(search =>
          search.lines.map(line => MoveRef(search.pathsByMove(line.evaluation.moves.head), 1))) else Nil,
        surveyed.filter(_._1 == 0).map(_._2),
        PurposeEvidence.correspondingEntries(paths, pair, q.user, q.action.slice(2, 4)))
      // Among equally explained choices, retain the first useful action.
      // Surviving a trade does not make a later contact the central idea.
      (q, evidence, use.orElse(other).orElse(surveyed.headOption.map(_._2.offset)).getOrElse(Int.MaxValue))
    }.sortBy((q, evidence, use) => (evidence.priority, use, q.action, q.target.at))
      .map((q, evidence, _) => q -> evidence)

  def discover(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      searches: List[TargetResponseInvestigation.Search] = Nil): Option[Question] =
    paths.requireOwner(packet)
    val pair = ReferencePurposeReading.assessmentPair(packet, paths)
    // Observed beneficiary activity already has its own owned reader. The
    // missing case is a legal but unseen idea after an independent survey;
    // testing our example must not turn it into discovery evidence.
    val questions = paths.moveIdeas.filter(q => q.aim != Aim.SupportedActivity ||
      replySurvey(packet, q.root, searches).exists(survey =>
        !(pair ++ survey.pathsByMove.values).exists(path => observedUse(paths, q, path).nonEmpty)))
    explanations(packet, paths, pair, questions, searches).headOption.filterNot { (_, evidence) =>
      MovePurpose.alternatives(packet, paths, searches).headOption.exists { (_, _, other) =>
        other.outranks(evidence)
      }
    }.map(_._1)

  def candidates(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths): List[Question] =
    paths.requireOwner(packet)
    val pair = ReferencePurposeReading.assessmentPair(packet, paths)
    val history = packet.assembly.input.positionHistory
    if pair.size != 2 || history.currentPosition.check.yes then return Nil
    (for
      root <- pair.find(_.moves.headOption.contains(packet.assembly.input.playedMoveUci))
      if root.moves.size >= 3 && root.certified.flatMap(_._2.material).exists(!_.events.exists(_.plyOffset == 0))
      actor <- piece(history, root.moves.head.take(2))
      after <- history.extend(List(root.moves.head)).toOption
      ready <- history.extend(root.moves.take(2)).toOption
    yield if actor.role.name == "king" then checkingPreparation(packet, paths, pair, root, actor, after, ready)
        else activation(packet, paths, root, actor, after, ready)
    ).getOrElse(Nil)

  private def activation(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      root: PreparedPath, actor: Piece, after: CanonicalPositionHistory,
      ready: CanonicalPositionHistory): List[Question] =
    val history = packet.assembly.input.positionHistory
    val moved = root.moves.head.slice(2, 4)
    val newlyPressured = chess.Square.all.flatMap(s => piece(after, s.key)).filter(p =>
      p.side != actor.side && p.role.name != "king" && piece(history, p.at).contains(p) &&
        controls(after, actor.side, p.at)(moved) && !controls(history, actor.side, p.at)(actor.at)).map(_.at).toSet
    val originals = paths.identitiesAfter(packet, root, 2).collect {
      case (identity, Some(Some(now))) => now -> identity
    }.toMap
    val king = chess.Square.all.flatMap(s => piece(ready, s.key)).find(p => p.side != actor.side && p.role.name == "king")
    val candidates = ready.currentLegalMoveUcis.flatMap { action =>
      val from = action.take(2)
      val to = action.slice(2, 4)
      for
        user <- piece(ready, from).toList
        if user.side == actor.side && user.role.name != "king" &&
          action.length == 4 && piece(ready, to).isEmpty
        original <- (if from == moved then Some(actor) else piece(history, from).filter(_ == user)).toList
        routeOpened = Set("rook", "queen", "bishop")(user.role.name) &&
          controls(after, actor.side, to)(from) && (!controls(history, actor.side, to)(original.at) ||
            piece(history, to).exists(_.side == actor.side))
        // Reject actions with no possible causal bridge before replaying them.
        // This is the same admission condition checked below, not a new cap.
        if routeOpened || newlyPressured.nonEmpty
        end <- step(ready, action).toList
        current <- piece(end, to).toList
        enemyKing <- king.toList
        target <- chess.Square.all.flatMap(s => piece(end, s.key)).filter(p => p.side != actor.side &&
          p.role.name != "king" && controls(end, actor.side, p.at)(current.at) &&
          piece(history, p.at).contains(p)).toList
        partner <- controls(end, actor.side, target.at).toList.sorted.filterNot(_ == current.at)
          .flatMap(at => piece(end, at)).filter(p => p.role.name != "pawn" && p.role.name != "king")
        originalPartner <- originals.get(partner).toList
        preparedPressure = originalPartner == actor && newlyPressured(target.at)
        if controls(after, actor.side, target.at)(partner.at) &&
          !controls(after, actor.side, target.at)(from) &&
          (routeOpened && distance(target.at, enemyKing.at) <= 1 || preparedPressure)
        aim = if distance(target.at, enemyKing.at) <= 1 then Aim.KingAttack else Aim.CoordinatedPressure
      yield Question(root, actor, original, action, aim, target, originalPartner,
        List(root.moves.take(2) :+ action))
    }
    // A rook battery's rear rook controls its partner, not the enemy through it.
    val batteries = ready.currentLegalMoveUcis.flatMap { action =>
      for
        user <- piece(ready, action.take(2)).toList if user.role.name == "rook" && action.length == 4
        if user.at != moved && piece(ready, action.slice(2, 4)).isEmpty
        original <- piece(history, user.at).filter(_ == user).toList
        if controls(after, actor.side, action.slice(2, 4))(user.at) &&
          (!controls(history, actor.side, action.slice(2, 4))(user.at) ||
            piece(history, action.slice(2, 4)).exists(_.side == actor.side))
        end <- step(ready, action).toList
        front <- piece(end, moved).toList if front.role.name == "rook" &&
          front.at.head == action.charAt(2) && controls(end, actor.side, front.at)(action.slice(2, 4))
        target <- chess.Square.all.flatMap(s => piece(end, s.key)).filter(p =>
          p.side != actor.side && p.at.head == front.at.head && controls(end, actor.side, p.at)(front.at)).toList
      yield Question(root, actor, original, action, Aim.RookCoordination, target, actor,
        List(root.moves.take(2) :+ action))
    }
    val supported = ready.currentLegalMoveUcis.flatMap { action =>
      val to = action.slice(2, 4)
      for
        user <- piece(ready, action.take(2)).toList
        original <- originals.get(user).toList
        if original != actor && user.side == actor.side && !Set("pawn", "king")(user.role.name) &&
          action.length == 4 && piece(ready, to).isEmpty &&
          !controls(history, actor.side, to)(actor.at) && controls(after, actor.side, to)(moved) &&
          piece(ready, moved).contains(actor.copy(square = EvidenceSquare(moved))) &&
          controls(ready, actor.side, to)(moved)
        end <- step(ready, action).toList
        if controls(end, actor.side, to)(moved)
        target <- chess.Square.all.flatMap(s => piece(end, s.key)).filter(p =>
          p.side != actor.side && p.role.name != "king" && piece(history, p.at).contains(p) &&
          controls(end, actor.side, p.at)(to) && !controls(history, actor.side, p.at)(original.at)).toList
      yield Question(root, actor, original, action, Aim.SupportedActivity, target, actor,
        List(root.moves.take(2) :+ action))
    }
    candidates ++ batteries ++ supported

  private def checkingPreparation(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      pair: List[PreparedPath], root: PreparedPath, actor: Piece,
      after: CanonicalPositionHistory, ready: CanonicalPositionHistory): List[Question] =
    val history = packet.assembly.input.positionHistory
    val other = pair.find(_ ne root).get
    val reply = root.moves(1)
    val observed = pair.flatMap(_.moves.zipWithIndex.collect {
      case (move, n) if n % 2 == 0 => move
    }).toSet
    val actions = ready.currentLegalMoveUcis.filter(move => move.length == 4 &&
      piece(ready, move.slice(2, 4)).isEmpty && piece(ready, move.take(2)).exists(p =>
        p.side == actor.side && p.role.name != "king" && p.role.name != "pawn"))
      .sortBy(move => (if observed(move) then 0 else 1,
        centrality(move.slice(2, 4)), move))
    // Bound the question generator. It never recursively promotes its own examples into plans.
    val questions = actions.take(16).iterator.flatMap { action =>
      val user = piece(ready, action.take(2)).get
      val prefixes = List(root.moves.take(2) :+ action,
        if other.moves.head == action then List(action) else List(other.moves.head, reply, action))
      val starts = prefixes.map(history.extend(_).toOption)
      (if starts.forall(_.nonEmpty) && piece(history, user.at).contains(user) then
        val left = starts.head.get
        val right = starts.last.get
        left.currentLegalMoveUcis.iterator.filter(move => move.length == 4 && piece(left, move.take(2)).exists(p =>
          p.side != actor.side && p.role.name == "pawn") && piece(left, move.slice(2, 4)).isEmpty &&
          right.currentLegalMoveUcis.contains(move)).flatMap { lever =>
          val l = step(left, lever).get
          val r = step(right, lever).get
          l.currentLegalMoveUcis.filter(move => move.slice(2, 4) == lever.slice(2, 4) &&
            move.take(2) != action.slice(2, 4) && r.currentLegalMoveUcis.contains(move))
            .sortBy(move => (if piece(l, move.take(2)).exists(_.role.name == "pawn") then 1 else 0, move))
            .iterator.flatMap { capture =>
            val a = step(l, capture).get
            val b = step(r, capture).get
            // The vacated pawn square admits the same slider, with a different checking tempo.
            val arrivals = a.currentLegalMoveUcis.filter(move => move.slice(2, 4) == lever.take(2) &&
              piece(a, move.take(2)).exists(p => Set("queen", "bishop", "rook")(p.role.name)))
            arrivals.iterator.flatMap { arrival =>
              val slider = piece(a, arrival.take(2)).get
              val peers = b.currentLegalMoveUcis.filter(move => move.slice(2, 4) == arrival.slice(2, 4) &&
                piece(b, move.take(2)).exists(p => p.side == slider.side && p.role == slider.role))
              peers.iterator.flatMap { peer =>
                val noCheck = step(a, arrival).get
                val checked = step(b, peer).get
                if !noCheck.currentPosition.check.yes && checked.currentPosition.check.yes &&
                  checked.currentLegalMoveUcis.contains(root.moves.head) then
                  val leftMoves = prefixes.head ++ List(lever, capture, arrival)
                  val rightMoves = prefixes.last ++ List(lever, capture, peer, root.moves.head)
                  paths.identitiesAfter(packet, root, 2).collectFirst {
                    case (identity, Some(Some(now))) if now == slider => identity
                  }.map { checker =>
                    Question(root, actor, user, action, Aim.CheckingTempo,
                      piece(left, lever.take(2)).get, checker, List(leftMoves, rightMoves),
                      checkOffsets = List(leftMoves.size - 1, rightMoves.size - 2))
                  }
                else None
              }
            }
          }
        }
      else Iterator.empty).take(1)
    }
    questions.take(3).toList

  /** Test the plan at its child and survey the opponent's actual choice there.
    * A later PV move being legal now does not make it a useful first reply.
    */
  def probes(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths, q: Question,
      searches: List[TargetResponseInvestigation.Search]): List[(EngineSearchAnchor, List[String])] =
    val ready = packet.assembly.input.positionHistory.extend(q.prefix).toOption.get
    val deferredChoice = ReferencePurposeReading.assessmentPair(packet, paths).find(_.moves.head != q.rootMove)
      .map(_.moves.head).filter(ready.currentLegalMoveUcis.contains)
    val alternatives = (List(q.action, q.root.moves(2)) ++ deferredChoice).distinct
    val actionTest = Option.when(alternatives.size > 1)(
      EngineSearchAnchor(q.rootMove, q.root.ids.head, q.root.positions(2).id.get, q.prefix) ->
        alternatives)
    val child = packet.assembly.input.positionHistory.extend(List(q.rootMove)).toOption.get
    val replyTest = Option.when(child.currentLegalMoveUcis.size >= 2)(
      EngineSearchAnchor(q.rootMove, q.root.ids.head, q.root.positions(1).id.get) -> child.currentLegalMoveUcis)
    val grounded = ReferencePurposeReading.assessmentPair(packet, paths).exists(path => observedUse(paths, q, path).nonEmpty)
    val order = if grounded then actionTest.toList ++ replyTest.toList else replyTest.toList ++ actionTest.toList
    order.filterNot { (anchor, moves) => searches.exists(search =>
      search.belongsTo(packet) && search.anchor.scopeKey == anchor.scopeKey &&
        (if anchor.prefixMoves.size == 1 then search.coversAllLegalMoves else search.work.rootRestriction match
          case EngineRootRestriction.Restricted(scope) => moves.toSet.subsetOf(scope.toSet)
          case _ => moves.forall(search.pathsByMove.contains))) }

  final class Reading private[MoveIntent](private val owner: PreparedPaths,
      val question: Question, val examples: List[PreparedPath], val pair: List[PreparedPath],
      val choices: Option[TargetResponseInvestigation.Search],
      val replies: Option[TargetResponseInvestigation.Search], private val packet: EvidenceBackedJudgmentPacket):
    def belongsTo(paths: PreparedPaths) = owner eq paths
    def root = pair.find(_.moves.head == question.rootMove).get
    def reference = pair.find(_.moves.head != question.rootMove).get
    lazy val comparison = ReferencePurposeReading.read(packet, owner, pair,
      MoveWorkProjection.select(packet, owner).rootUses(pair),
      minimumThrough = branches.filter(b => pair.exists(_ eq b.path) && b.realization.nonEmpty)
        .map(b => b.path -> b.through).toMap, preparation = Some(this))
    def supported: Boolean = (pair ++ replies.toList.flatMap(_.pathsByMove.values)).exists(path =>
      observedUse(owner, question, path).nonEmpty) || planChoice.exists { (preferred, path, answer) =>
      (preferred eq path) || answer.exists(_.qualityBand.contains(PrincipalVariationComparison.QualityBand.Close))
    }
    // A negative action comparison is also an answer to a conditional idea.
    // It never certifies the idea as sound or changes the actual root verdict.
    def investigated = supported || question.aim == Aim.SupportedActivity && replies.nonEmpty &&
      planChoice.exists((_, _, answer) => answer.flatMap(_.qualityBand).nonEmpty)
    def newEntryControl(branch: Branch): List[Piece] =
      if question.aim != Aim.SupportedActivity then Nil else
        (for
          root <- owner.purposePosition(branch.path, 0).toList
          before <- owner.purposePosition(branch.path, 1).toList
          after <- owner.purposePosition(branch.path, 2).toList
          original <- root.controls.keys.toList.filter(_.side != question.actor.side)
          old <- owner.trackedPieceAfter(branch.path, original, 1).flatten.toList
          current <- owner.trackedPieceAfter(branch.path, original, 2).flatten.toList
          if !before.controlledBy(old)(question.action.slice(2, 4)) &&
            after.controlledBy(current)(question.action.slice(2, 4))
        yield current).sortBy(_.at)
    def complete = investigated && (choices.nonEmpty || question.action == root.moves(2)) && replies.nonEmpty &&
      comparison.exists(_.completePurpose)
    lazy val branches: List[Branch] = MoveIntent.branches(packet, owner, this)
    def action = RootQuestionSummary.MoveRef(examples.head, question.actionOffset)
    def exampleCaptures = if question.aim != Aim.CheckingTempo then Nil else examples.flatMap { path =>
      path.certified.toList.flatMap(_._2.material.toList.flatMap(_.events))
        .filter(_.capture.nonEmpty).map(event => RootQuestionSummary.MoveRef(path, event.plyOffset))
    }
    def referenceChoice = choices.flatMap { search =>
      val delayed = reference.moves.head
      val best = search.lines.head.evaluation.moves.head
      search.pairAnswer(packet, best, delayed).filter(_.evaluationRelation == "worse").map(answer =>
        (search.pathsByMove(best), search.pathsByMove(delayed), answer))
    }
    def planChoice = choices.map { search =>
      val best = search.lines.head.evaluation.moves.head
      (search.pathsByMove(best), search.pathsByMove(question.action), search.pairAnswer(packet, best, question.action))
    }

  def read(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      searches: List[TargetResponseInvestigation.Search]): Option[Reading] =
    paths.requireOwner(packet)
    for
      q <- discover(packet, paths, searches)
      pair = ReferencePurposeReading.assessmentPair(packet, paths)
      examples = q.examples.flatMap(moves => paths.certifiedPath(packet, moves)
        .filter(p => p.moves == moves && p.ids == p.declared && p.certified.flatMap(_._2.material).nonEmpty))
      if examples.size == q.examples.size && examples.forall(path =>
        pair.exists(p => p.ids.head == path.ids.head && p.moves.head == path.moves.head && p.positions.head == path.positions.head))
      if validBridge(packet, paths, q, examples)
    yield
      def matching(prefix: List[String]) = searches.reverse.find(search => search.belongsTo(packet) &&
        search.anchor.prefixMoves == prefix && search.anchor.rootOccurrenceId == q.root.ids.head &&
        search.anchor.positionId == q.root.positions(prefix.size).id.get &&
        CommentaryInvestigation.candidatesReady(search.work, search.lines) && search.lines.size >= 2)
      new Reading(paths, q, examples, pair,
        matching(q.prefix).filter(s => List(q.action, q.root.moves(2)).forall(s.pathsByMove.contains)),
        replySurvey(packet, q.root, searches), packet)

  def questionJson(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      searches: List[TargetResponseInvestigation.Search]): Option[JsObject] = read(packet, paths, searches).map(value =>
    Json.obj("id" -> value.question.id, "root_move" -> value.question.rootMove,
      "root_occurrence_id" -> value.root.ids.head, "source_line_id" -> value.root.line.ref.id,
      "reference_line_id" -> value.reference.line.ref.id, "use_move" -> value.question.action,
      "example_line_ids" -> value.examples.map(_.line.ref.id)))

  private def validBridge(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      q: Question, examples: List[PreparedPath]): Boolean =
    def pos(path: PreparedPath, count: Int) = paths.purposePosition(path, count)
    def current(path: PreparedPath, p: Piece, count: Int) = paths.trackedPieceAfter(path, p, count).flatten
    val first = examples.head
    val use = q.actionOffset
    val common = for
      before <- pos(first, 0)
      prepared <- pos(first, 1)
      start <- pos(first, use)
      end <- pos(first, use + 1)
      user <- current(first, q.user, use)
      moved <- current(first, q.user, use + 1)
      if first.moves(use) == q.action && user.at == q.action.take(2) && moved.at == q.action.slice(2, 4)
    yield (before, prepared, start, end, moved)
    common.exists { (before, prepared, start, end, moved) => q.aim match
      case Aim.SupportedActivity => first.certified.get._2.material.get.events.isEmpty &&
        PurposeActivityReading.discover(packet, paths, first, Nil).exists(value =>
          value.original == q.user && value.entry == use && value.use == use &&
            value.seed.provider.contains(q.actor) && value.attacks.contains(q.target))
      case Aim.CheckingTempo =>
        examples.size == 2 && q.checkOffsets.size == 2 && examples.zip(q.checkOffsets).zipWithIndex.forall {
          case ((path, check), index) =>
            val action = if path.moves.head == q.action then 0 else use
            val events = path.certified.get._2.material.get.events
            (for
              at <- pos(path, check + 1)
              checker <- current(path, q.partner, check + 1)
              king <- current(path, q.actor, check + 1)
              planUser <- current(path, q.user, check + 1)
              if planUser.at == q.action.slice(2, 4) && path.moves(action) == q.action &&
                checker.at == first.moves(q.checkOffsets.head).slice(2, 4) &&
                (1 to check).forall(n => current(path, q.actor, n).contains(
                  q.actor.copy(square = EvidenceSquare(if index == 0 then q.rootMove.slice(2, 4) else q.actor.at))))
              if events.size == 1 && events.head.capture.nonEmpty &&
                paths.trackedPieceAfter(path, q.target, events.head.plyOffset + 1).contains(None) &&
                current(path, q.target, events.head.plyOffset).exists(p => events.head.capture.exists(_.square == p.square))
            yield
              at.controlledBy(checker)(q.actor.at) && !at.controlledBy(checker)(q.rootMove.slice(2, 4)) &&
                (at.view.currentCheckSources(q.actor.side).nonEmpty == (index == 1)) &&
                (index == 0 || path.moves.last == q.rootMove && check == path.moves.size - 2)
            ).contains(true)
        }
      case aim =>
        val opensRoute = current(first, q.user, 1).exists(p => prepared.controlledBy(p)(moved.at)) &&
          (!before.controlledBy(q.user)(moved.at) || before.occupant(moved.at).exists(_.side == q.actor.side))
        val bringsPartner = q.partner == q.actor && current(first, q.partner, 1).exists(p =>
          prepared.controlledBy(p)(q.target.at) && !before.controlledBy(q.partner)(q.target.at))
        first.certified.get._2.material.get.events.isEmpty &&
          start.view.currentCheckSources(q.actor.side).isEmpty &&
          (opensRoute || bringsPartner) &&
          current(first, q.target, use + 1).contains(q.target) &&
          current(first, q.partner, use + 1).exists { partner => aim match
            case Aim.KingAttack | Aim.CoordinatedPressure =>
              end.controlledBy(moved)(q.target.at) && end.controlledBy(partner)(q.target.at) &&
                prepared.controlledBy(partner)(q.target.at) &&
                (aim == Aim.CoordinatedPressure || end.controls.keys.exists(king =>
                  king.side != q.actor.side && king.role.name == "king" && distance(king.at, q.target.at) <= 1))
            case Aim.RookCoordination =>
              List(moved, partner).forall(_.role.name == "rook") && moved.at.head == partner.at.head &&
                partner.at.head == q.target.at.head && end.controlledBy(moved)(partner.at) &&
                end.controlledBy(partner)(q.target.at)
            case _ => false
          }
    }

  enum Course:
    case Reference, MainReply, TestedPlan, DelayedReference, OtherReply

  final class Branch private[MoveIntent](val owner: Reading, val path: PreparedPath, val through: Int,
      val threats: List[Piece], val course: Course, val preferred: Option[PreparedPath],
      val separated: Boolean, val realization: Option[Int], val retained: Boolean,
      val lost: List[Piece], val answered: List[Piece], val referenceRelieves: List[Piece],
      val attempt: Option[Int], val changedTarget: Option[Piece], val blockers: List[Piece],
      val successor: Option[ExchangeActivity.PawnWork]):
    // A compared action can be the very same occurrence already explained in
    // the actual root line. Keep its tested receipt as proof, without opening
    // a second capture episode merely to name that identical choice.
    def displayedPreferred = preferred.map { candidate =>
      val actual = owner.root
      if candidate.moves.take(3) == actual.moves.take(3) && candidate.ids.take(3) == actual.ids.take(3) &&
          candidate.positions.take(4) == actual.positions.take(4) then actual else candidate
    }
    def actionReferences = (List(0, 1, 2, through).distinct.map(RootQuestionSummary.MoveRef(path, _)) ++
      realization.toList.map(RootQuestionSummary.MoveRef(path, _)) ++
      successor.toList.map(work => RootQuestionSummary.MoveRef(path, work.offset)) ++
      attempt.toList.map(RootQuestionSummary.MoveRef(path, _)) ++
      displayedPreferred.toList.map(RootQuestionSummary.MoveRef(_, 2)) :+
      RootQuestionSummary.MoveRef(owner.reference, 0)).distinct
    def references = (actionReferences ++ preferred.toList.map(RootQuestionSummary.MoveRef(_, 2))).distinct

  /** The same intended relationship is reread under each reply. Equal notation
    * is insufficient: a blocker, traded partner or missing target changes it. */
  private def realizes(paths: PreparedPaths, q: Question, path: PreparedPath, count: Int): Boolean =
    def current(original: Piece) = paths.trackedPieceAfter(path, original, count).flatten
    (for
      position <- paths.purposePosition(path, count)
    yield q.aim match
      // The central piece may be exchanged as part of the intended play.
      // Its survival is not the king preparation's continuing function.
      case Aim.CheckingTempo => path.moves.take(count).zipWithIndex.exists { (move, n) =>
        n >= 2 && n % 2 == 0 && move == q.action &&
          paths.trackedPieceAfter(path, q.user, n).flatten.exists(_.at == move.take(2))
      } && current(q.actor).exists(_.at == q.rootMove.slice(2, 4))
      case _ => (for
        user <- current(q.user) if user.at == q.action.slice(2, 4)
        partner <- current(q.partner)
        target <- current(q.target)
      yield q.aim match
        case Aim.KingAttack | Aim.CoordinatedPressure => position.controlledBy(user)(target.at) &&
          position.controlledBy(partner)(target.at) && (q.aim == Aim.CoordinatedPressure ||
            position.controls.keys.exists(p => p.side != user.side && p.role.name == "king" && distance(target.at, p.at) <= 1))
        case Aim.RookCoordination => user.at.head == partner.at.head && partner.at.head == target.at.head &&
          position.controlledBy(user)(partner.at) && position.controlledBy(partner)(target.at)
        case Aim.SupportedActivity => partner.at == q.rootMove.slice(2, 4) &&
          position.controlledBy(partner)(user.at) && position.controlledBy(user)(target.at)
        case _ => false
      ).contains(true)
    ).contains(true)

  private def replyThreats(paths: PreparedPaths, path: PreparedPath, side: chess.Color): List[Piece] =
    (for
      before <- paths.purposePosition(path, 1)
      after <- paths.purposePosition(path, 2)
      reply <- after.occupant(path.moves(1).slice(2, 4))
      if reply.side != side
    yield after.controlledBy(reply).toList.sorted.flatMap(after.occupant).filter(p =>
      p.side == side && before.contains(p) &&
        !before.controllers(p.at, reply.side).exists(_.at == path.moves(1).take(2)))).getOrElse(Nil)

  private def continuingPawn(paths: PreparedPaths, q: Question, path: PreparedPath, end: Int)
      : Option[ExchangeActivity.PawnWork] =
    Option.when(q.aim == Aim.CoordinatedPressure && q.user.role.name == "pawn")(
      ExchangeActivity.pawnWork(paths, path, end, end)).flatten.filter(work =>
        work.continuing && work.actor.side == q.user.side && work.original != q.user &&
          work.actor.at == q.action.slice(2, 4))

  private def branches(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths, value: Reading): List[Branch] =
    def separated(answer: CommentaryInvestigation.Answer) =
      answer.qualityBand.contains(PrincipalVariationComparison.QualityBand.Separated)
    val delayed = value.referenceChoice.map((preferred, path, answer) =>
      (path, Course.DelayedReference, Some(preferred), separated(answer)))
    val plan = value.planChoice.map((preferred, path, answer) =>
      (path, Course.TestedPlan, Option.when(preferred ne path)(preferred), answer.exists(separated)))
    val primary = List((value.reference, Course.Reference, None, false),
      (value.root, Course.MainReply, None, false))
    val explained = primary ++ plan.toList ++ delayed.toList
    val hasRealization = explained.exists { (path, _, _, _) =>
      observedUse(paths, value.question, path).exists { use =>
        val end = ExchangeActivity.settled(path, math.max(math.min(4, path.moves.size - 1), use))
        realizes(paths, value.question, path, end + 1) ||
          continuingPawn(paths, value.question, path, end).nonEmpty
      }
    }
    val other = value.replies.toList.flatMap(_.lines).map(_.evaluation.moves.head)
      .filter(_ != value.root.moves(1)).flatMap(move => value.replies.get.pathsByMove.get(move))
      .sortBy(path => if hasRealization || observedUse(paths, value.question, path).nonEmpty then 0 else 1)
      .headOption
      .map(path => (path, Course.OtherReply, None, false))
    val candidates = explained ++ other.toList
    // Matching the first choice does not make the later continuations equal.
    // In particular, keep both actual root paths when a child search returns
    // a different exchange or a shorter continuation for that same choice.
    candidates.distinctBy(_._1.moves)
      .filter(_._1.moves.size >= 3).map { (path, course, preferred, separated) =>
        val q = value.question
        val threats = replyThreats(paths, path, q.actor.side)
        val localEnd = ExchangeActivity.settled(path, math.min(4, path.moves.size - 1))
        val realization = observedUse(paths, q, path)
        // Close the first witnessed purpose, not every later event in its PV.
        val end = ExchangeActivity.settled(path, math.max(localEnd, realization.getOrElse(localEnd)))
        val retained = realization.nonEmpty && realizes(paths, q, path, end + 1)
        val successor = Option.when(realization.nonEmpty && !retained)(
          continuingPawn(paths, q, path, end)).flatten
        val attempt = (2 to end by 2).find(n => path.moves(n) == q.action &&
          paths.trackedPieceAfter(path, q.user, n).flatten.exists(_.at == q.action.take(2)))
        val changedTarget = paths.trackedPieceAfter(path, q.target, end + 1).flatten.filter(_.at != q.target.at)
        val blockers = (for
          position <- paths.purposePosition(path, end + 1).toList
          target <- paths.trackedPieceAfter(path, q.target, end + 1).flatten.toList
          original <- (if q.aim == Aim.RookCoordination then List(q.partner)
            else if q.aim == Aim.KingAttack || q.aim == Aim.CoordinatedPressure then List(q.user, q.partner) else Nil)
          current <- paths.trackedPieceAfter(path, original, end + 1).flatten.toList
          ray <- position.blockedRay(current, target).toList
          blocker <- ray._2
        yield blocker).distinct
        val lost = List(q.user, q.partner).distinct.filter(p =>
          paths.trackedPieceAfter(path, p, end + 1).contains(None))
        val answered = threats.filter { p =>
          val original = paths.identitiesAfter(packet, path, 2).collectFirst {
            case (identity, Some(Some(now))) if now == p => identity
          }
          original.flatMap(paths.trackedPieceAfter(path, _, 3).flatten).exists(now =>
            paths.purposePosition(path, 3).exists(position =>
              !position.controllers(now.at, !q.actor.side).exists(_.at == path.moves(1).slice(2, 4))))
        }
        val relieves = if course != Course.Reference then Nil else
          replyThreats(paths, value.root, q.actor.side).filter(p => path.moves.head.take(2) == p.at)
        new Branch(value, path, end, threats, course, preferred, separated, realization,
          retained, lost, answered, relieves, attempt, changedTarget, blockers, successor)
    }

  def sections(intent: Reading): List[CommentaryAnswer.Section] =
    import CommentaryAnswer.*
    intent.branches.filterNot(_.course == Course.Reference).map { branch =>
        val material = branch.path.certified.get._2.material.get
        val events = material.events.filter(_.plyOffset <= branch.through)
        val cost = CommentaryMaterialAccount.calculate(material, events.map(_.plyOffset),
          (0 to branch.through).toList, branch.path.moves.size).map(Material(branch.path, _))
        Section(List(PreparedReply(branch)) ++ cost.toList ++
          Option.when(events.nonEmpty && cost.isEmpty)(MaterialEvents(branch.path, events)).toList)
    }

  def summary(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      searches: List[TargetResponseInvestigation.Search]): Option[RootQuestionSummary.Result] =
    read(packet, paths, searches).flatMap(realize(packet, paths, _))

  def realize(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      intent: Reading): Option[RootQuestionSummary.Result] =
    for
      _ <- Option.when(intent.belongsTo(paths) && intent.investigated)(())
      comparison <- intent.comparison
      positions = packet.resources.map(view => view.position -> new Position(view)).toMap
      local = MoveWorkProjection.select(packet, paths).local
      closed <- ReferencePurposeReading.answer(packet, paths, intent.pair, comparison,
        value => MoveWorkProjection.completeSelectedConditions(packet, paths, intent.pair, positions, local, value))
      result <- RootQuestionSummary.realize(paths, intent.pair, closed)
    yield result

  def purpose(value: Reading, ko: Boolean): List[CommentaryEnglish.Part] =
    import RootQuestionSummary.MoveRef
    val q = value.question
    val root = MoveRef(value.root, 0)
    val action = value.action
    val roles = Map("pawn" -> "폰", "knight" -> "나이트", "bishop" -> "비숍", "rook" -> "룩", "queen" -> "퀸", "king" -> "킹")
    q.aim match
      case Aim.SupportedActivity =>
        List(root, if ko then s"로 ${q.action.slice(2, 4)} 칸을 새로 지원합니다. "
          else s" newly supports ${q.action.slice(2, 4)}. ", action,
          if ko then s"로 원래 ${q.user.at}의 ${roles(q.user.role.name)}${if q.user.role.name == "knight" then "가" else "이"} ${q.target.at} ${roles(q.target.role.name)}을 상대할 수 있는 구상입니다. 실제 응수에서 이 선택의 대가를 따로 비교했습니다."
          else s" is a possible way for the original ${q.user.at} ${q.user.role.name} to challenge the ${q.target.at} ${q.target.role.name}. Its cost is tested against the actual reply below.")
      case Aim.CoordinatedPressure =>
        List(root, if ko then s"는 ${q.target.at}에 압박을 걸어 후속 행동과 연결합니다. "
          else s" prepares combined pressure on ${q.target.at}. ", action,
          if ko then s" 뒤에는 ${roles(q.user.role.name)}과 ${roles(q.partner.role.name)}이 같은 ${q.target.at} ${roles(q.target.role.name)}을 함께 압박합니다. 상대 응수에 따라 실행 순서와 대가가 달라집니다."
          else s" joins the ${q.user.role.name} and ${q.partner.role.name} against the same ${q.target.at} ${q.target.role.name}; the reply determines its timing and cost.")
      case Aim.KingAttack =>
        List(root, if ko then s"는 ${q.actor.at}의 ${roles(q.actor.role.name)}을 공격에 합류시키는 준비입니다. "
          else s" prepares to bring the ${q.actor.at} ${q.actor.role.name} into the attack. ",
          action, if ko then s"로 옮기면 ${q.partner.at} ${roles(q.partner.role.name)}과 함께 킹 옆의 ${q.target.at}을 겨냥합니다. 상대의 응수를 확인해야 하는 구상입니다."
          else s" would join the ${q.partner.at} ${q.partner.role.name} against ${q.target.at} beside the king; its realization depends on the reply.")
      case Aim.RookCoordination =>
        List(root, if ko then s"는 ${q.actor.at} 칸을 비워 다른 룩을 같은 파일에 겹치려는 준비입니다. "
          else s" vacates ${q.actor.at} to bring the other rook onto the same file. ",
          action, if ko then s" 뒤에는 두 룩이 ${q.target.at.head} 파일에 연결되어 앞의 룩이 ${q.target.at}을 압박합니다."
          else s" connects the rooks on the ${q.target.at.head}-file, with the front rook bearing on ${q.target.at}.")
      case Aim.CheckingTempo =>
        val prepared = value.examples.head
        val other = value.examples.last
        val check = MoveRef(prepared, q.checkOffsets.head)
        val oldCheck = MoveRef(other, q.checkOffsets.last)
        List(root, if ko then "는 중앙의 말을 움직이기 전에 킹을 체크가 들어올 선에서 벗어나게 합니다. 예를 들어 "
          else " steps the king off a potential checking line before central piece play. In the conditional example ",
          action, " ", MoveRef(prepared, 3), " ", MoveRef(prepared, 4),
          if ko then s"로 ${if q.actor.side.white then "흑" else "백"}의 ${q.target.at} 폰을 잡아 그 칸이 비면 "
          else s", capturing the ${q.target.at} pawn clears its square for ",
          check, if ko then "가 가능합니다. 킹을 미리 옮기지 않은 " else ". Without the king preparation, ",
          MoveRef(value.reference, 0), if ko then s" 쪽의 같은 ${roles(q.partner.role.name)} 진입은 " else " permits the corresponding checking arrival ",
          oldCheck, if ko then "이고 " else ", answered here by ",
          MoveRef(other, other.moves.size - 1), if ko then "처럼 체크에 응수해야 합니다. 준비한 쪽에서는 그 체크 템포가 없습니다. 이 예는 상대가 폰을 내주는 조건부 수순이며 최선 응수라는 뜻은 아닙니다."
          else ". The prepared king avoids that checking tempo. This pawn-giving example is conditional, not a claim of best defense.")

  private def samePrefix(a: Branch, b: Branch, count: Int): Boolean =
    (a.owner eq b.owner) && a.path.moves.take(count) == b.path.moves.take(count) &&
      a.path.ids.take(count) == b.path.ids.take(count) && a.path.positions.take(count + 1) == b.path.positions.take(count + 1)

  def sameObservedOutcome(a: Branch, b: Branch): Boolean =
    a.through == b.through && samePrefix(a, b, a.through + 1) &&
      (a.threats, a.realization, a.retained, a.lost, a.answered, a.attempt, a.changedTarget, a.blockers, a.successor) ==
      (b.threats, b.realization, b.retained, b.lost, b.answered, b.attempt, b.changedTarget, b.blockers, b.successor)

  def groupedReplies(section: CommentaryAnswer.Section, ko: Boolean,
      announced: Set[Reading] = Set.empty): Option[CommentaryEnglish.Rendered] =
    import CommentaryAnswer.*
    import CommentaryEnglish.{Part, Rendered}
    val replies = section.statements.collect { case reply: PreparedReply => reply }
    replies.headOption.map { first =>
      val shared = ReferencePurposeText.groupedCosts(section, ko)
      val consumed = scala.collection.mutable.ListBuffer.empty[Statement]
      val parts = replies.zipWithIndex.flatMap { (claim, index) =>
        if consumed.contains(claim) then Nil else
          val matching = replies.filter(peer => sameObservedOutcome(peer.value, claim.value))
          // One actual PV and its matched choice test may describe the same closed prefix.
          // A longer/different outcome or another evaluation comparison remains separate.
          val peers = if matching.exists(_.value.course == Course.MainReply) &&
            matching.count(_.value.course != Course.MainReply) <= 1 then matching else List(claim)
          val branch = peers.find(_.value.course != Course.MainReply).getOrElse(claim).value
          val prior = consumed.collect { case PreparedReply(value) => value }.toList
          val prices = section.statements.collect { case cost: Material if peers.exists(_.value.path eq cost.path) &&
            cost.cost.offsets.max == branch.through && !shared.exists(group => group.expressed.contains(cost) &&
              group.expressed.collect { case other: Material => other }.exists(p => !peers.exists(_.value.path eq p.path))) => cost }
          val accounts = prices.flatMap(cost => branch.owner.comparison.flatMap(_.physicalLosses(cost)).map(cost -> _))
          val price = Option.when(accounts.nonEmpty && accounts.size == prices.size &&
            accounts.map(_._2.toSet).distinct.size == 1)(accounts.head._2)
          consumed ++= peers
          if price.nonEmpty then consumed ++= prices
          val priceText = price.map { lost =>
            val otherLosses = sections(branch.owner).flatMap(_.statements).collect {
              case cost: Material if cost.path ne branch.path => cost
            }.flatMap(cost => branch.owner.comparison.flatMap(_.physicalLosses(cost)))
            val distinguish = lost.map(_.role.name).toSet.filter(role => otherLosses.exists { other =>
              val ours = lost.filter(_.role.name == role).toSet
              val theirs = other.filter(_.role.name == role).toSet
              theirs.nonEmpty && ours != theirs
            }) ++ Option.when(branch.successor.nonEmpty)("pawn")
            ReferencePurposeText.lossText(lost, ko, distinguish)
          }
          val sameReply = prior.nonEmpty && samePrefix(branch, first.value, 2)
          (if index == 0 then Nil else List[Part](" ")) ++
            reply(branch, ko, sameReply, priceText, prior.exists(samePrefix(_, branch, 3)),
              announced(branch.owner))
      }
      Rendered(parts, consumed.toList)
    }

  def reply(value: Branch, ko: Boolean, introduced: Boolean = false,
      price: Option[String] = None, repeatedAction: Boolean = false,
      purposeAnnounced: Boolean = false): List[CommentaryEnglish.Part] =
    import RootQuestionSummary.MoveRef
    val p = value.path
    val roles = Map("pawn" -> "폰", "knight" -> "나이트", "bishop" -> "비숍", "rook" -> "룩", "queen" -> "퀸", "king" -> "킹")
    val sameReplyAlreadyIntroduced = (value.course == Course.TestedPlan || value.course == Course.DelayedReference) &&
      value.owner.root.moves.take(3) != p.moves.take(3)
    val pressure = if value.threats.isEmpty || sameReplyAlreadyIntroduced then "" else
      if ko then value.threats.map(p => s"${p.at} ${roles(p.role.name)}").mkString("·") +
        (if value.threats.last.role.name == "knight" then "를" else "을") + " 겨냥합니다. "
      else "targets " + value.threats.map(p => s"the ${p.at} ${p.role.name}").mkString(" and ") + ". "
    val start: List[CommentaryEnglish.Part] = List(MoveRef(p, 0),
      if value.course == Course.OtherReply then
        if ko then "에 대해 조사한 다른 응수 " else " has the other tested reply "
      else if ko then " 뒤 " else " after ", MoveRef(p, 1),
      if pressure.isEmpty then (if ko then "에 " else ": ")
      else (if ko then "는 " else " ") + pressure)
    val choice: List[CommentaryEnglish.Part] = value.course match
      case Course.Reference =>
        List(if ko then "비교 수 " else "The other root choice ", MoveRef(p, 0),
          if value.referenceRelieves.nonEmpty then
            if ko then s"는 ${value.referenceRelieves.map(p => s"${p.at} ${roles(p.role.name)}").mkString("·")}을 먼저 움직입니다. "
            else " moves the piece that the delayed line leaves as the reply's target. "
          else if ko then "의 수순은 " else " continues ",
          MoveRef(p, 1), " ", MoveRef(p, 2), if ko then "입니다. " else ". ")
      case Course.TestedPlan | Course.DelayedReference =>
        (if repeatedAction then List(if ko then "같은 " else "For the same choice, ")
          else if introduced then List(if ko then "반면 " else "Instead, ")
          else if pressure.isEmpty then List(MoveRef(p, 0), " ", MoveRef(p, 1), if ko then " 뒤 " else ", ")
          else start) ++ Option.when(!ko)("continuing with ").toList ++ List(
          MoveRef(p, 2), if repeatedAction then
            if ko then "를 따로 비교한 수순은 " else " in the separate matched line "
          else if value.course == Course.DelayedReference then
            if ko then "로 비교 수를 뒤늦게 두는 선택은 " else " (the delayed root choice) "
          else if ko then "로 준비한 구상을 실행하는 선택은 " else " (the prepared idea) ") ++
          value.displayedPreferred.fold[List[CommentaryEnglish.Part]](List(if ko then
            "이 국면에서 조사한 선택지 중 가장 높게 평가됩니다. "
            else "is preferred among the tested continuations. ")) { preferred =>
            if ko then List(MoveRef(preferred, 2), if value.separated then
              "보다 이 국면에서 불리하게 평가됩니다. " else "와 이 국면에서 평가가 가깝습니다. ")
            else List(if value.separated then "is inferior to " else "is close to ", MoveRef(preferred, 2),
              " in this matched comparison. ")
          }
      case _ => start ++ List(MoveRef(p, 2), if ko then "를 먼저 둡니다. " else " is played first. ") ++
        Option.when(value.answered.nonEmpty)(
          if p.moves(2).slice(2, 4) == p.moves(1).slice(2, 4) then
            if ko then "공격한 기물을 잡아 그 공격을 없앱니다. " else "This captures the attacker to remove that attack. "
          else if value.answered.exists(_.at == p.moves(2).take(2)) then
            if ko then "공격받은 말을 옮겨 그 공격에서 벗어납니다. " else "This moves the targeted piece out of that attack. "
          else if ko then "그 공격을 해소합니다. " else "This resolves that attack. ").toList ++
        Option.when(p.moves(2) == value.owner.reference.moves.head)(if ko then
          "이 응수에는 비교 수를 한 수 늦춰 둘 수 있습니다. " else
          "This reply allows the reference move one turn later. ").toList
    val outcome: List[CommentaryEnglish.Part] = if value.course == Course.Reference then Nil else
      value.realization.toList.flatMap(n =>
        (if n == 2 then List[CommentaryEnglish.Part](if ko then "이때 " else "This ")
          else List[CommentaryEnglish.Part](MoveRef(p, n), if ko then " 뒤 " else " ")) ++ List[CommentaryEnglish.Part](
        if value.retained then
          if value.owner.question.aim == Aim.CheckingTempo then
            if ko then "중앙 전개를 시작하며, 킹은 미리 옮긴 자리에 남습니다. "
            else "starts the central play, while the king remains on its prepared square. "
          else if value.owner.question.aim == Aim.RookCoordination then
            if purposeAnnounced then
              if ko then "준비한 룩 연결이 실현됩니다. " else "establishes the prepared rook connection. "
            else if ko then "두 룩의 파일 연결과 앞 룩의 압박이 이루어집니다. "
            else "establishes the rook connection and the front rook's pressure. "
          else if value.owner.question.aim == Aim.SupportedActivity then
            if ko then "지원받는 기물이 진출하여 상대 기물을 압박합니다. "
            else "brings the supported piece into play against the opposing piece. "
          else if purposeAnnounced then
            if ko then "준비한 공동 공격이 실현됩니다. " else "realizes the prepared joint attack. "
          else if ko then "준비한 두 기물의 공동 공격이 이루어집니다. "
            else "establishes the prepared joint attack. "
        else if value.successor.nonEmpty then
          if price.nonEmpty then
            if ko then "공동 압박이 생깁니다. " else "establishes the joint pressure. "
          else if ko then s"공동 압박이 생기지만, 원래 ${value.owner.question.user.at} 폰은 교환됩니다. "
          else s"establishes the joint pressure, but the original ${value.owner.question.user.at} pawn is exchanged. "
        else if value.lost.nonEmpty then
          if ko then "준비한 관계가 생깁니다. " else "establishes the arrangement. "
        else if ko then "준비한 관계가 생기지만, 이 수순의 끝까지 유지되지는 않습니다. "
          else "establishes the arrangement, but it does not survive this continuation. ")) ++
      Option.when(value.realization.isEmpty && value.attempt.nonEmpty)(
        if ko then "준비한 수를 두지만 처음 의도한 압박까지 이루어지지는 않습니다. "
        else "The prepared move is played, but its intended pressure is not established. ").toList ++
      value.changedTarget.filter(_ => value.attempt.nonEmpty && !value.retained).toList.map { target =>
        val original = value.owner.question.target
        if ko then s"처음 겨냥한 ${original.at} ${roles(original.role.name)}이 ${target.at}로 옮겨 압박 대상의 위치도 달라집니다. "
        else s"The intended target, the ${original.at} ${original.role.name}, has moved to ${target.at}. "
      } ++
      Option.when(value.attempt.nonEmpty && !value.retained && value.blockers.nonEmpty)(
        if ko then s"${value.blockers.map(p => s"${p.at} ${roles(p.role.name)}").mkString("·")} 때문에 목표로 향하는 선이 막혀 있습니다. "
        else s"The line to the target is blocked by ${value.blockers.map(p => s"the ${p.at} ${p.role.name}").mkString(" and ")}. ").toList ++
      (value.owner.newEntryControl(value) match
        case Nil => Nil
        case pieces => List(if ko then
          s"이 응수로 ${pieces.map(p => s"${p.at} ${roles(p.role.name)}").mkString("·")}이 진출 칸 ${value.owner.question.action.slice(2, 4)}을 새로 통제합니다. "
          else s"This reply adds control of the entry square ${value.owner.question.action.slice(2, 4)} from ${pieces.map(p => s"the ${p.at} ${p.role.name}").mkString(" and ")}. ")) ++
      (value.lost.filterNot(p => value.successor.nonEmpty && p == value.owner.question.user) match
        case lost if lost.nonEmpty && value.owner.question.aim != Aim.CheckingTempo => List(if ko then
          s"다만 참여하던 ${lost.map(p => s"${p.at} ${roles(p.role.name)}").mkString("·")}${if lost.last.role.name == "knight" then "는" else "은"} 이 수순 뒤 남지 않습니다. " +
            (if value.realization.nonEmpty && !value.retained then "그 관계도 끝까지 유지되지는 않습니다. " else "")
          else s"But the plan's ${lost.map(p => s"${p.at} ${p.role.name}").mkString(" and ")} does not remain after this continuation. " +
            (if value.realization.nonEmpty && !value.retained then "The arrangement does not survive either. " else ""))
        case _ => Nil)
    val replacement = value.successor.toList.flatMap { work =>
      List[CommentaryEnglish.Part](RootQuestionSummary.MoveRef(p, work.offset),
        if ko then s" 뒤에는 원래 ${work.original.at} 폰이 ${work.actor.at}에서 역할을 이어받아 ${work.targets.map(p => s"${p.at} ${roles(p.role.name)}").mkString("·")}을 압박합니다. "
        else s" leaves the original ${work.original.at} pawn on ${work.actor.at} taking over the pressure on ${work.targets.map(p => s"the ${p.at} ${p.role.name}").mkString(" and ")}. ")
    }
    val endAlreadyShown = value.successor.exists(_.offset == value.through) ||
      value.realization.contains(value.through)
    val end: List[CommentaryEnglish.Part] =
      if endAlreadyShown then List(if ko then "이 지점까지 " else "Up to this point, ")
      else List(MoveRef(p, value.through), if ko then "까지 " else ": up to this point, ")
    val unfulfilled = value.realization.isEmpty && value.attempt.isEmpty
    val conclusion = if unfulfilled then
      (if ko then "준비한 배치는 실현되지 " + (if price.nonEmpty then "않으며, " else "않습니다.")
        else "the intended arrangement is not realized" + (if price.nonEmpty then "; " else ".")) + price.getOrElse("")
    else price.getOrElse(if ko then "확인한 수순입니다." else "this is the observed outcome.")
    choice ++ outcome ++ replacement ++ end ++ List(conclusion)
