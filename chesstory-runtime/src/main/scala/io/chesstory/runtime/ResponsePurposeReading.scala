package io.chesstory.runtime

import lila.chessjudgment.model.judgment.*

/** One reviewed move, one completed reply comparison, and the functions that
  * survive those replies. Readers contribute facts to this account; a previous
  * prose label does not decide whether the original question can be answered. */
private[runtime] object ResponsePurposeReading:
  import CommentaryAnswer.*
  import MoveWorkProjection.{Detail, LocalCandidate, Position}
  import ObservedLineProtocol.{PreparedPath, PreparedPaths}
  import RootQuestionSummary.MoveRef

  def required(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      question: TargetResponseInvestigation.Question): Boolean =
    TargetResponseInvestigation.reviewedMover(packet, paths, question)

  def withAnswer(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths, pair: List[PreparedPath],
      proofs: List[TargetResponseInvestigation.FirstTarget], notes: List[LocalCandidate], previous: Answer,
      complete: Answer => Option[Answer]): Answer =
    paths.requireOwner(packet)
    if pair.size != 2 || pair.exists(_.automaticTerminal.nonEmpty) || previous.question == "current_defense" then
      return previous
    val owners = proofs.filter(value => value.belongsTo(packet, paths) && required(packet, paths, value.question) &&
      pair.exists(path => path.moves.headOption.contains(value.question.rootMove) &&
        path.ids.headOption.contains(value.question.rootMoveOccurrenceId) &&
        path.positions.lift(1).flatMap(_.id).contains(value.question.childPositionId))).distinctBy(_.question.id)
    if owners.size != 1 then return previous
    val proof = owners.head
    val q = proof.question
    val capture = proof.captureDecline.pathsByMove(q.referenceMove)
    val decline = proof.captureDecline.pathsByMove(q.alternativeMove)
    val leader = proof.captureDecline.pathsByMove(proof.leaderMove)
    val other = if leader eq capture then decline else capture
    def ownsFirstAction(path: PreparedPath): Boolean =
      path.moves.size >= 3 && paths.paths.exists(_ eq path) && path.certified.nonEmpty &&
        path.ids == path.declared && path.moves == path.sourceMoves &&
        paths.positionView(path, 2).flatMap(_.legalRows).exists(_.exists(row =>
          row.move == path.moves(2) && row.actor.side == q.originalTarget.side &&
            packet.observations.exists(observation => observation.transition.evidence.id == path.ids(2) &&
              observation.transition.from == path.positions(2) && observation.transition.to == path.positions(3) &&
              observation.mover.from == row.actor.square && observation.mover.beforeRole == row.actor.role &&
              observation.mover.side == row.actor.side)))
    if !List(capture, decline).forall(ownsFirstAction) then return previous
    val comparison = proof.captureDecline.pairAnswer(packet, proof.leaderMove, other.moves(1))
      .filter(value => value.qualityBand.contains(PrincipalVariationComparison.QualityBand.Close) ||
        value.evaluationRelation == "worse")
    if comparison.flatMap(_.qualityBand).isEmpty then return previous
    val function = ExchangeFunctionReading.read(packet, paths, pair, proof, notes)
    val root = MoveRef(leader, 0)
    val child = paths.positionView(leader, 1).map(new Position(_))
      .filter(_.complete(RelationFactKind.GeometricControl))
    if child.isEmpty || !child.get.contains(q.target) then return previous
    val roles = notes.filter(note => note.edge.evidence.id == leader.ids.head && note.note.actor == q.target &&
      (note.note.detail match
        case Detail.Attack(actor, _, true) => actor == q.target
        case _: Detail.Development => true
        case _ => false)).distinctBy(_.note.detail)
    def events(path: PreparedPath, first: Int, last: Int) = path.certified.flatMap(_._2.material).toList
      .flatMap(_.events).filter(event => event.plyOffset >= first && event.plyOffset <= last)
    val rootEvents = events(leader, 0, 0)
    val attackers = child.get.controllers(q.target.square.key, !q.target.side)
    val introduction = Section(Option.when(rootEvents.nonEmpty)(MaterialEvents(leader, rootEvents)).toList ++
      roles.map(note => Action(root, note.note.detail)) ++ Option.when(function.isEmpty)(
        TargetDefense(root, q.target, attackers, child.get.controllers(q.target.square.key, q.target.side))).toList,
      focus = roles.map(note => Focus(root, note)).distinctBy(_.move))
    def response(path: PreparedPath): Option[Section] =
      val proposedEnd = if function.exists(_.access.nonEmpty) then
        if path eq capture then 3 else math.min(4, path.moves.size - 1)
      else 2
      val end = if events(path, proposedEnd + 1, proposedEnd + 1).exists(_.capture.exists(_.recapture)) then
        proposedEnd + 1 else proposedEnd
      val moves = (1 to end).map(MoveRef(path, _)).toList
      val local = notes.filter(note => path.ids.slice(1, end + 1).contains(note.edge.evidence.id) &&
        (note.note.detail match
          case Detail.Evades(actor, _) => actor.side == q.originalTarget.side
          case Detail.Attack(actor, _, true) => actor.side == q.originalTarget.side
          case _ => false)).distinctBy(_.note.detail)
      val support = paths.pieceAfter(packet, path, q.originalTarget, 2) match
        case Some(None) => Some(Nil)
        case Some(Some(_)) => TargetResponseInvestigation.supportThroughOwnAction(packet, paths, q, path)
        case None => None
      support.map { controls =>
        val prices = events(path, 1, end)
        Section(List(Continuation(moves)) ++ Option.when(prices.nonEmpty)(MaterialEvents(path, prices)).toList ++
          local.map(note => Action(MoveRef(path, path.ids.indexOf(note.edge.evidence.id)), note.note.detail)) ++
          controls ++ Option.when(path eq capture)(function.map(ExchangeFunction(_))).flatten.toList,
          moves, local.map(note => Focus(MoveRef(path, path.ids.indexOf(note.edge.evidence.id)), note)).distinctBy(_.move))
      }
    val branches = List(leader, other).map(response)
    if branches.exists(_.isEmpty) then return previous
    val kept = function.toList.flatMap(value => value.kept.toList.map(role => Section(List(
      Continuation((0 to role.use).map(MoveRef(role.path, _)).toList),
      MaterialEvents(role.path, events(role.path, 0, role.use)), RetainedExchangeFunction(value)))))
    val core = List(introduction, Section(List(FirstReplyChoice(root, MoveRef(leader, 1),
      MoveRef(other, 1), comparison.get.qualityBand.get)))) ++ branches.flatten ++ kept ++
      List(Section(List(Continuation(pair.map(MoveRef(_, 0))))))
    val selected = core.filter(_.statements.nonEmpty)
    val recorded = previous.core ++ previous.details
    val detail = Section(recorded.flatMap(_.statements).distinct, recorded.flatMap(_.required).distinct,
      recorded.flatMap(_.focus).distinctBy(_.move))
    RootQuestionSummary.completePairAnswer(packet, paths, pair,
      if function.nonEmpty then "exchange_function" else "first_target_response", selected, List(detail))
      .flatMap(answer => complete(answer.copy(comparison = Some(ComparisonScope(Set(q.originalTarget))),
        completionSources = Some(selected.flatMap(_.statements)),
        evidence = (answer.evidence ++ previous.evidence ++ roles.flatMap(_.evidence) ++
          function.toList.flatMap(_.evidence) ++ proof.fullPool.pathsByMove(proof.leaderMove).ids.take(2)).distinct)))
      .filter(answer => RootQuestionSummary.realize(paths, pair, answer).nonEmpty).getOrElse(previous)
