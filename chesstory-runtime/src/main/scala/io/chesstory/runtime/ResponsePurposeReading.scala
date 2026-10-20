package io.chesstory.runtime

import lila.chessjudgment.model.judgment.*

/** One reviewed move, one completed reply comparison, and the functions that
  * survive those replies. Readers contribute facts to this account; a previous
  * prose label does not decide whether the original question can be answered. */
private[runtime] object ResponsePurposeReading:
  import CommentaryAnswer.*
  import CommentaryEnglish.at
  import MoveWorkProjection.{Detail, LocalCandidate, Position}
  import ObservedLineProtocol.{PreparedPath, PreparedPaths}
  import RootQuestionSummary.MoveRef

  def required(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      question: TargetResponseInvestigation.Question): Boolean =
    TargetResponseInvestigation.reviewedMover(packet, paths, question)

  def withAnswer(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths, pair: List[PreparedPath],
      proofs: List[TargetResponseInvestigation.FirstTarget], notes: List[LocalCandidate], previous: Answer,
      complete: Answer => Option[Answer],
      rootUses: List[PreparedPath] => List[(Section, List[String])] = _ => Nil): Answer =
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
    val paired = previous.core.flatMap(_.statements).collectFirst { case ReferencePurpose(value) => value }
    val surveyedActivities = Option.when(paired.nonEmpty && proof.fullPool.coversAllLegalMoves)(
      proof.fullPool.lines.map(line => proof.fullPool.pathsByMove(line.evaluation.moves.head)).map { path =>
        val witness = proof.fullPool.continuationWitness(path.moves(1), List(proof.captureDecline)).get
        path -> PurposeActivityReading.discoverReplies(packet, paths, witness, rootUses(List(witness)))
      }).toList.flatten.toMap
    // A capture/noncapture seed is a way to ask for evidence, not a permanent
    // definition of the meaningful choice. Prefer another strong returned reply
    // when it realizes the same root opportunity with a different function/price.
    val activityPair = PurposeActivityReading.replyPair(packet, proof.fullPool, surveyedActivities,
      paired.flatMap(_.played.activity))
    val leader = activityPair.map(_.head.path).getOrElse(proof.captureDecline.pathsByMove(proof.leaderMove))
    val other = activityPair.map(_.last.path).getOrElse(if leader eq capture then decline else capture)
    val chosenSearch = if activityPair.nonEmpty then proof.fullPool else proof.captureDecline
    val rankedLeader = chosenSearch.pathsByMove(leader.moves(1))
    val rankedOther = chosenSearch.pathsByMove(other.moves(1))
    val activities = activityPair.getOrElse(List(leader, other).flatMap(path =>
      surveyedActivities.getOrElse(path, Nil).headOption))
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
    val comparison = chosenSearch.pairAnswer(packet, proof.leaderMove, other.moves(1))
      .filter(value => value.qualityBand.contains(PrincipalVariationComparison.QualityBand.Close) ||
        value.evaluationRelation == "worse")
    if comparison.flatMap(_.qualityBand).isEmpty then return previous
    val function = if activityPair.nonEmpty then None else ExchangeFunctionReading.read(packet, paths, pair, proof, notes)
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
    val independentUses = Option.when(paired.nonEmpty && proof.fullPool.coversAllLegalMoves)(
      proof.fullPool.pathsByMove.values.toList.flatMap(ReferencePurposeReading.controlUse(packet, paths, _))).toList.flatten
    val controlUses = List(leader, other).flatMap(ReferencePurposeReading.controlUse(packet, paths, _)).filter(use =>
      independentUses.exists(found => found.controller == use.controller && found.removed == use.removed &&
        found.target == use.target && found.originalUser == use.originalUser && found.user == use.user))
    val attackers = child.get.controllers(q.target.square.key, !q.target.side)
    val introduction = Section(Option.when(rootEvents.nonEmpty)(MaterialEvents(leader, rootEvents)).toList ++
      roles.map(note => Action(root, note.note.detail)) ++ Option.when(function.isEmpty)(
        TargetDefense(root, q.target, attackers, child.get.controllers(q.target.square.key, q.target.side))).toList,
      focus = roles.map(note => Focus(root, note)).distinctBy(_.move))
    def response(path: PreparedPath): Option[Section] =
      // The first own action can threaten a pawn that a different allied
      // piece takes on the next own turn. Keep the intervening reply and that
      // observed use, instead of ending on a temporary pawn deficit. This is
      // an actual continuation, not a claim that the threat forces recovery.
      val firstAction = notes.filter(_.edge.evidence.id == path.ids(2)).flatMap { note => note.note.detail match
        case Detail.Attack(actor, target, true) if path.moves(2).slice(2, 4) == actor.square.key =>
          val originals = paths.identitiesAfter(packet, path, 0).map(_._1)
          for
            originalActor <- originals.find(paths.trackedPieceAfter(path, _, 3).flatten.contains(actor))
            originalTarget <- originals.find(paths.trackedPieceAfter(path, _, 3).flatten.contains(target))
          yield (originalActor, originalTarget, target)
        case _ => None
      }
      val immediateUse = events(path, 4, 4).exists(event => event.capture.nonEmpty &&
        event.movement.side == q.originalTarget.side && firstAction.exists { (actor, target, atStart) =>
          paths.trackedPieceAfter(path, target, 4).flatten.contains(atStart) &&
            event.capture.exists(c => c.square == atStart.square && c.capturedRole == atStart.role) &&
            (3 to 4).forall(count => (for
              at <- paths.purposePosition(path, count)
              a <- paths.trackedPieceAfter(path, actor, count).flatten
              t <- paths.trackedPieceAfter(path, target, count).flatten
            yield at.controlledBy(a)(t.square.key)).contains(true))
        })
      // The first own action may instead support the reviewed piece's next
      // actual arrival. Keep that use only with an unchanged, newly gained guard.
      val supportedUse = for
        own <- paths.purposePosition(path, 2)
        actor <- own.occupant(path.moves(2).take(2))
        after <- paths.purposePosition(path, 3)
        moved <- after.occupant(path.moves(2).slice(2, 4))
        arrival <- path.moves.lift(4)
        targetBefore <- paths.trackedPieceAfter(path, q.originalTarget, 4).flatten
        target <- paths.trackedPieceAfter(path, q.originalTarget, 5).flatten
        if actor.side == q.originalTarget.side && arrival.take(2) == targetBefore.at &&
          arrival.slice(2, 4) == target.at && !own.controlledBy(actor)(target.at) &&
          (3 to 5).forall(count => paths.purposePosition(path, count).exists(at =>
            at.contains(moved) && at.controlledBy(moved)(target.at)))
        note <- notes.find(note => note.edge.evidence.id == path.ids(4) && (note.note.detail match
          case Detail.Support(provider, guarded, _, true) => provider == moved && guarded == target
          case _ => false))
      yield note
      val proposedEnd = if function.exists(_.access.nonEmpty) then
        if path eq capture then 3 else math.min(4, path.moves.size - 1)
      else if immediateUse || supportedUse.nonEmpty then 4 else 2
      var end = controlUses.find(_.path eq path).fold(proposedEnd)(use => math.max(proposedEnd, use.through))
      val activity = activities.find(_.path eq path)
      end = math.max(end, activity.map(_.through).getOrElse(end))
      while end + 1 < path.moves.size && (events(path, end + 1, end + 1).exists(_.capture.nonEmpty) ||
          path.certified.get._2.lineEvents.exists(e => e.plyOffset == end && e.kind == LineEventKind.Check)) do end += 1
      val moves = (1 to end).map(MoveRef(path, _)).toList
      def survives(piece: MoveWorkProjection.Piece, offset: Int): Boolean =
        paths.identitiesAfter(packet, path, offset + 1).exists {
          case (original, Some(Some(current))) if current == piece =>
            paths.trackedPieceAfter(path, original, end + 1).flatten.nonEmpty
          case _ => false
        }
      val local = notes.filter(note => path.ids.slice(2, end + 1).contains(note.edge.evidence.id) &&
        !activity.exists(a => note.edge.evidence.id == path.ids(a.use) && note.note.actor == a.actor) &&
        (note.note.detail match
          case Detail.Evades(actor, _) => actor.side == q.originalTarget.side &&
            path.moves(path.ids.indexOf(note.edge.evidence.id)).slice(2, 4) == actor.at &&
            survives(actor, path.ids.indexOf(note.edge.evidence.id))
          case Detail.Attack(actor, _, true) => actor.side == q.originalTarget.side &&
            path.moves(path.ids.indexOf(note.edge.evidence.id)).slice(2, 4) == actor.at &&
            survives(actor, path.ids.indexOf(note.edge.evidence.id))
          case _: Detail.Support => supportedUse.contains(note)
          case _ => false)).distinctBy(_.note.detail)
      val support = paths.pieceAfter(packet, path, q.originalTarget, 2) match
        case Some(None) => Some(Nil)
        case Some(Some(_)) => TargetResponseInvestigation.supportThroughOwnAction(packet, paths, q, path)
        case None => None
      support.map { controls =>
        // Declining a recapture can leave only the root's loss. Keep that
        // price with this reply even when its continuation has no capture.
        val prices = events(path, if paired.nonEmpty then 0 else 1, end)
        val activityMoves = activity.toList.flatMap(_.references).toSet
        val price = Option.when(prices.nonEmpty)(
          Option.when(paired.nonEmpty)(CommentaryMaterialAccount.calculate(path.certified.get._2.material.get,
            prices.map(_.plyOffset), (0 to end).toList, path.moves.size)).flatten
            .fold[Statement](MaterialEvents(path, prices))(Material(path, _))).toList
        val priceMoves = price.flatMap(_.references).toSet
        val sequence = Option.when(activity.isEmpty || moves.exists(m => !activityMoves(m) && !priceMoves(m)))(Continuation(moves)).toList
        val shared = Option.when(path eq other)(activities.find(_.path eq leader)).flatten
          .filter(previous => activity.exists(_.sameActivity(previous)))
        Section(sequence ++ activity.map(ActivityUse(_, shared)).toList ++
          price ++
          local.map(note => Action(MoveRef(path, path.ids.indexOf(note.edge.evidence.id)), note.note.detail)) ++
          controls.filter(_ => paths.trackedPieceAfter(path, q.originalTarget, end + 1).flatten.nonEmpty) ++
          Option.when(path eq capture)(function.map(ExchangeFunction(_))).flatten.toList,
          moves, local.map(note => Focus(MoveRef(path, path.ids.indexOf(note.edge.evidence.id)), note)).distinctBy(_.move))
      }
    val branches = List(leader, other).map(response)
    if branches.exists(_.isEmpty) then return previous
    val kept = function.toList.flatMap(value => value.kept.toList.map(role => Section(List(
      Continuation((0 to role.use).map(MoveRef(role.path, _)).toList),
      MaterialEvents(role.path, events(role.path, 0, role.use)), RetainedExchangeFunction(value)))))
    // The reply investigation supplements an already explained assessment
    // pair. It must not replace both root purposes with a one-target question.
    if paired.nonEmpty && function.isEmpty then
      val replies = branches.flatten
      val statements = FirstReplyChoice(MoveRef(rankedLeader, 0), MoveRef(rankedLeader, 1), MoveRef(rankedOther, 1),
        comparison.get.qualityBand.get) :: replies.flatMap(_.statements).map {
          case MaterialEvents(path, prices) =>
            val through = replies.flatMap(_.required).filter(_.path eq path).map(_.offset).max
            val completePrices = events(path, 0, through)
            CommentaryMaterialAccount.calculate(path.certified.get._2.material.get, completePrices.map(_.plyOffset),
              (0 to through).toList, path.moves.size)
              .fold[Statement](MaterialEvents(path, completePrices))(Material(path, _))
          case value => value
        }
      val supplement = Section(statements, replies.flatMap(_.required).distinct,
        replies.flatMap(_.focus).distinctBy(_.move))
      val reading = paired.get.withResponseUses(controlUses, activities)
      def enrich(statement: Statement): Statement = statement match
        case ReferencePurpose(value) if value eq paired.get => ReferencePurpose(reading)
        case ReferencePosition(value, arm) if value eq paired.get =>
          ReferencePosition(reading, reading.arms.find(_.path eq arm.path).get)
        case ReferenceExchangeReply(value, purpose, branch) if value eq paired.get =>
          ReferenceExchangeReply(reading, purpose, branch)
        case other => other
      val core = previous.core.map(section => section.copy(statements = section.statements.map(enrich)))
      return complete(previous.copy(core = core :+ supplement,
        completionSources = Some((previous.completionSources.getOrElse(previous.core.flatMap(_.statements)).map(enrich) ++
          supplement.statements).distinct),
        evidence = (previous.evidence ++ reading.evidence ++ activities.flatMap(_.evidence) ++
          proof.fullPool.pathsByMove(proof.leaderMove).ids.take(2)).distinct))
        .filter(answer => RootQuestionSummary.realize(paths, pair, answer).nonEmpty).getOrElse(previous)
    if paired.nonEmpty then return previous
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
