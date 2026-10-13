package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.{LegalMoveResource, MoveObservationMeaning, PieceControlState}
import lila.chessjudgment.model.judgment.*

/** The same original resource may have different useful timings, replies and
  * prices. Its task is not the old capture sequence and another good plan
  * does not inherit its functional explanation.
  */
private[runtime] object FunctionalChoiceReading:
  import CommentaryFact.*
  import CommentaryProposal.*
  import FunctionalReplyReading.Resource
  import ObservedLineProtocol.{PreparedPath, PreparedPaths}
  import TargetResponseInvestigation.Search

  val ValuePrefix = "function_value_"
  private val SourceVersion = "1"

  case class Obligation(comparison: CompareMoves, facts: List[Fact], phase: Int,
      sourceIds: List[String])
  case class Branch(rootMove: String, firstReply: String, firstPool: Search,
      resourceMove: String, resourceQuestion: CompareMoves,
      use: Option[CommentaryChoiceValue.Checked],
      response: Option[CommentaryChoiceValue.Checked], retainsCondition: Boolean,
      matchedReply: Boolean = false)
  case class Task(resource: Resource, branches: List[Branch], obligations: List[Obligation],
      expectedReplies: Set[(String, String)], surveyedRoots: Set[String]):
    def complete: Boolean = surveyedRoots == Set(resource.owner.comparison.referenceMove,
      resource.owner.comparison.alternativeMove) && expectedReplies.nonEmpty &&
      branches.map(value => value.rootMove -> value.firstReply).toSet == expectedReplies &&
      branches.forall(value => value.use.exists(use => use.useful || use.refuted) && value.response.nonEmpty)

  final class Decision private[FunctionalChoiceReading] (val branch: Branch,
      val use: CommentaryChoiceValue.Checked, val resistance: CommentaryChoiceValue.Checked,
      val path: PreparedPath, val other: Option[RootQuestionSummary.MoveRef],
      val conditions: List[CommentaryAnswer.Statement], val replyRoles: List[CommentaryAnswer.Statement],
      val replyEvidence: List[String], val useRoles: List[CommentaryAnswer.Statement]):
    def sequence = (0 to 3).toList.map(RootQuestionSummary.MoveRef(path, _))

  private[runtime] case class ReplyRoleReading(statements: List[CommentaryAnswer.Statement], evidence: List[String],
      useRoles: List[CommentaryAnswer.Statement] = Nil)

  /** Read the same admitted control transition as the local note projection,
    * restricted to the tested reply and the original resource it answers.
    * Its geometry explains an action, never its safety or numerical value.
    */
  private[runtime] def selectedReplyRoles(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      path: PreparedPath, originalUser: Piece,
      positions: Map[PositionNodeRef, MoveWorkProjection.Position]): ReplyRoleReading =
    import CommentaryAnswer.*
    import CommentaryEnglish.at
    import MoveWorkProjection.Detail
    paths.requireOwner(packet)
    val empty = ReplyRoleReading(Nil, Nil)
    val reading = for
      _ <- Option.when(paths.paths.exists(_ eq path) && path.certified.nonEmpty &&
        path.ids == path.declared && path.moves == path.sourceMoves && path.moves.size > 3)(())
      before <- positions.get(path.positions(3))
      after <- positions.get(path.positions(4))
      if before.view.position == path.positions(3) && after.view.position == path.positions(4) &&
        before.complete(RelationFactKind.GeometricControl) && after.complete(RelationFactKind.GeometricControl)
      userAtChoice <- paths.trackedPieceAfter(path, originalUser.witness, 2).flatten
      userBefore <- paths.trackedPieceAfter(path, originalUser.witness, 3).flatten
      userAfter <- paths.trackedPieceAfter(path, originalUser.witness, 4).flatten
      if userBefore == userAfter
      ownRows <- paths.positionView(path, 2).flatMap(_.legalRows)
      if ownRows.exists(row => row.move == path.moves(2) && row.actor == userAtChoice && row.to == userBefore.square)
      replyBefore <- before.occupant(path.moves(3).take(2))
      originals = paths.identitiesAfter(packet, path, 3).collect {
        case (original, Some(Some(current))) if current == replyBefore => original
      }.distinct
      originalReply <- originals match
        case original :: Nil => Some(original)
        case _ => None
      replyAfter <- paths.trackedPieceAfter(path, originalReply, 4).flatten
      if replyBefore.side != userBefore.side && after.contains(replyAfter) && before.contains(userBefore) &&
        after.contains(userAfter) && replyAfter.square.key == path.moves(3).slice(2, 4)
      rows <- paths.positionView(path, 3).flatMap(_.legalRows)
      if rows.exists(row => row.move == path.moves(3) && row.actor == replyBefore && row.to == replyAfter.square)
      edge <- packet.assembly.transitions.find(edge => edge.evidence.id == path.ids(3) &&
        edge.from == path.positions(3) && edge.to == path.positions(4))
      if packet.evidenceGraph.record(edge.evidence).exists(record => edge.matches(record) &&
        packet.evidenceGraph.proofEligible(record))
    yield
      val admitted = packet.observations.filter(observation => observation.transition == edge)
      val selected = admitted.flatMap { observation =>
        observation.meaning match
          case MoveObservationMeaning.PieceControl(prior, next) if
              prior.controller.contains(userBefore) && prior.target.contains(replyBefore) &&
              next.controller.contains(userAfter) && next.target.contains(replyAfter) &&
              prior.state == PieceControlState.Present && next.state == PieceControlState.Absent &&
              replyBefore != replyAfter && before.controlledBy(userBefore)(replyBefore.at) &&
              !after.controlledBy(userAfter)(replyAfter.at) =>
            List(Action(RootQuestionSummary.MoveRef(path, 3), Detail.Evades(replyAfter, userAfter)) -> observation)
          case MoveObservationMeaning.PieceControl(prior, next) if
              prior.controller.contains(replyBefore) && prior.target.contains(userBefore) &&
              next.controller.contains(replyAfter) && next.target.contains(userAfter) &&
              next.state == PieceControlState.Present && after.controlledBy(replyAfter)(userAfter.at) &&
              ((prior.state == PieceControlState.Present && before.controlledBy(replyBefore)(userBefore.at)) ||
                (prior.state == PieceControlState.Absent && !before.controlledBy(replyBefore)(userBefore.at))) =>
            List(Action(RootQuestionSummary.MoveRef(path, 3),
              Detail.Attack(replyAfter, userAfter, prior.state == PieceControlState.Absent)) -> observation)
          case _ => Nil
      }
      val controlSources = before.sourceRefs(before.sources(userBefore, List(replyBefore.at)) ++
        before.sources(replyBefore, List(userBefore.at))) ++
        after.sourceRefs(after.sources(userAfter, List(replyAfter.at)) ++ after.sources(replyAfter, List(userAfter.at)))
      // An admitted evasion already proves the attack at the end of the
      // resource move. State that purpose before its answer; do not infer
      // a new attack or a won piece from the move's evaluation.
      val useRoles = Option.when(selected.exists {
        case (Action(_, _: Detail.Evades), _) => true
        case _ => false
      })(ControlState(RootQuestionSummary.MoveRef(path, 2), userBefore, Some(replyBefore),
        replyBefore.at, present = true)).toList
      ReplyRoleReading(selected.map(_._1).distinct,
        Option.when(selected.nonEmpty)(List(edge.evidence.id, before.view.occurrence.id, after.view.occurrence.id) ++
          selected.flatMap(_._2.premises.map(_.id)) ++ controlSources).getOrElse(Nil).distinct, useRoles)
    reading.getOrElse(empty)

  private def decision(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      resource: Resource, branch: Branch, showOther: Boolean,
      positions: Map[PositionNodeRef, MoveWorkProjection.Position]): Option[Decision] =
    for
      use <- branch.use if use.belongsTo(packet, paths)
      response <- branch.response if response.belongsTo(packet, paths)
      prefix = List(branch.rootMove, branch.firstReply)
      if use.question.prefixMovesUci == prefix && use.question.referenceMove == branch.resourceMove &&
        response.question.prefixMovesUci == prefix :+ branch.resourceMove
      strongest <- response.survey.lines.headOption.map(_.evaluation.moves.head)
      path <- response.comparison.pathsByMove.get(strongest)
      if paths.paths.exists(_ eq path) && path.moves.startsWith(prefix :+ branch.resourceMove) &&
        path.ids.take(3) == use.reference.ids.take(3) && path.positions.take(4) == use.reference.positions.take(4)
      row <- originalRow(paths, path, 2, resource.actor, resource.square) if row.move == branch.resourceMove
      state <- FunctionalReplyReading.conditionAt(paths, path, 2, resource.condition)
      provider <- resource.condition.subject
      current <- paths.trackedPieceAfter(path, provider.witness, 2)
      roles = selectedReplyRoles(packet, paths, path, resource.actor, positions)
    yield new Decision(branch, use, response, path,
      Option.when(showOther || use.refuted)(RootQuestionSummary.MoveRef(use.alternative, 2)),
      current.fold(List[CommentaryAnswer.Statement](CommentaryAnswer.PieceAbsence(
        RootQuestionSummary.MoveRef(path, 1), provider.witness)))(actor =>
        List(CommentaryAnswer.ControlState(RootQuestionSummary.MoveRef(path, 1), actor, None,
          resource.square, state == "present"))), roles.statements, roles.evidence, roles.useRoles)

  def withAnswer(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      pair: List[PreparedPath], tasks: List[Task], previous: CommentaryAnswer.Answer,
      protect: (CommentaryAnswer.Answer, List[CommentaryAnswer.Section]) => CommentaryAnswer.Answer)
      : CommentaryAnswer.Answer =
    withAnswer(packet, paths, pair, tasks, previous, protect, showUsefulOther = true)

  private[runtime] def withAnswer(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      pair: List[PreparedPath], tasks: List[Task], previous: CommentaryAnswer.Answer,
      protect: (CommentaryAnswer.Answer, List[CommentaryAnswer.Section]) => CommentaryAnswer.Answer,
      showUsefulOther: Boolean): CommentaryAnswer.Answer =
    import CommentaryAnswer.*
    import RootQuestionSummary.MoveRef
    paths.requireOwner(packet)
    def owned(path: PreparedPath): Boolean = paths.paths.exists(_ eq path) &&
      path.certified.nonEmpty && path.ids == path.declared && path.moves == path.sourceMoves &&
      pair.exists(root => path.moves.headOption == root.moves.headOption &&
        path.ids.headOption == root.ids.headOption && path.positions.take(2) == root.positions.take(2))
    if pair.size != 2 || pair.exists(_.automaticTerminal.nonEmpty) ||
        previous.required.exists(ref => !owned(ref.path)) then return previous
    val candidates = tasks.filter(task => task.complete && task.resource.owner.answer.exists(answer =>
      Set(answer.referenceLineId, answer.alternativeLineId) == pair.map(_.line.ref.id).toSet))
    val task = candidates.distinct match
      case value :: Nil => value
      case _ => return previous
    val resource = task.resource
    val root = resource.owner.comparison
    val selected = List(root.referenceMove, root.alternativeMove).flatMap(move => task.branches.find(branch =>
      branch.rootMove == move && branch.firstPool.lines.headOption.exists(_.evaluation.moves.head == branch.firstReply)))
    if selected.size != 2 then return previous
    val matched = List(root.referenceMove, root.alternativeMove).flatMap(move =>
      task.branches.find(branch => branch.rootMove == move && branch.matchedReply))
    val differentValue = matched.size == 2 && matched.flatMap(_.use).map(_.useful).distinct.size == 2 &&
      matched.flatMap(_.use).forall(value => value.useful || value.refuted)
    val current = if differentValue then matched ++ selected.filterNot(matched.contains) else selected
    val useful = task.branches.find(branch => branch.rootMove == root.referenceMove &&
      branch.use.exists(_.useful) && FunctionalReplyReading.conditionAt(paths,
        branch.use.get.reference, 2, resource.condition).contains("present") && !current.contains(branch))
    val chosen = current ++ useful.toList
    val positions = packet.resources.map(view => view.position -> new MoveWorkProjection.Position(view)).toMap
    val decisions = chosen.map(branch => branch -> (chosen.size <= 2 || branch.use.exists(_.refuted))).map((branch, other) =>
      decision(packet, paths, resource, branch, other, positions))
    if decisions.exists(_.isEmpty) then return previous
    val owner = pair.find(_.moves.headOption.contains(root.referenceMove)).get
    val peer = pair.find(_.moves.headOption.contains(root.alternativeMove)).get
    val provider = resource.condition.subject.get
    val condition = for
      left <- paths.trackedPieceAfter(owner, provider.witness, 1).flatten
      right <- paths.trackedPieceAfter(peer, provider.witness, 1).flatten
      actual <- FunctionalReplyReading.conditionAt(paths, owner, 1, resource.condition)
      alternative <- FunctionalReplyReading.conditionAt(paths, peer, 1, resource.condition)
      if actual == resource.expectedReference && alternative == resource.expectedAlternative
    yield ControlContrast(MoveRef(owner, 0), left, None, resource.square, actual == "present",
      MoveRef(peer, 0), right, None, resource.square, alternative == "present")
    if condition.isEmpty then return previous
    // A root's ordinary relocation is part of its purpose even when another
    // resource is compared. Keep its first observed return on the same
    // original piece, without claiming that the forecast was forced.
    val route = RootQuestionSummary.prepare(packet, paths, pair, positions).toList.flatten.flatMap { arm =>
      for
        actor <- arm.root.toList
        if actor.role.name != "pawn" && actor.role.name != "king"
        if actor == provider.witness && FunctionalReplyReading.conditionAt(paths, arm.path, 1,
          resource.condition).contains("absent")
        next <- arm.route(actor).find(_ > 0).toList
      yield PieceRoute(arm.path, actor, List(0, next))
    }
    val rootSection = Section(condition.get :: route)
    val lead = rootSection :: decisions.flatten.map(value => Section(List(ResourceDecision(value))))
    val evidence = resource.evidence ++ decisions.flatten.flatMap(_.replyEvidence) ++
      task.branches.flatMap(branch => branch.use.toList ++ branch.response.toList).flatMap(value =>
        value.reference.ids ++ value.alternative.ids)
    // Read both roots against their own leading defence, then one defence
    // which changes the value of the same plan. A shared reply alone can make
    // a conditional resource sound like an unconditional purpose of the move.
    val changedReply = selected.iterator.flatMap { leader =>
      task.branches.find(branch => branch.rootMove == leader.rootMove && branch.firstReply != leader.firstReply &&
        CommentaryChoiceValue.adequateReplies(packet, branch.firstPool).contains(branch.firstReply) &&
        branch.use.exists(use => leader.use.exists(_.useful != use.useful)))
    }.nextOption()
    val additional = changedReply.orElse(Option.when(differentValue)(matched.find(branch => !selected.contains(branch))).flatten)
    val responseBranches = selected.flatMap(leader => leader :: additional.filter(_.rootMove == leader.rootMove).toList)
    val responseDecisions = responseBranches.map(branch => decision(packet, paths, resource, branch,
      showOther = false, positions))
    val responseAnswer = Option.when(responseDecisions.forall(_.nonEmpty))(()).flatMap { _ =>
      boundedAnswer(packet, paths, pair, rootSection, responseDecisions.flatten, previous,
        evidence ++ responseDecisions.flatten.flatMap(_.replyEvidence))
    }
    lazy val narrow = Option.when(differentValue && matched.map(_.firstReply).distinct.size == 1)(matched)
      .flatMap { primary =>
        val center = primary.map(branch => decision(packet, paths, resource, branch, showUsefulOther, positions))
        val other = decisions.flatten.filterNot(value => primary.contains(value.branch))
        if center.exists(_.isEmpty) then None else
          matchedAnswer(packet, paths, pair, resource, primary, rootSection,
            center.flatten, other, previous, evidence, protect)
      }
    // A mechanical prefix or delivery bound may reject the smaller question.
    // Keep the already supported wider value answer as the fallback.
    responseAnswer.orElse(narrow)
      .orElse(completeAnswer(packet, paths, pair, lead, previous, evidence, protect)).getOrElse(previous)

  /** Replace the selected explanation as a whole. Its displayed decisions
    * keep their immediate capture/check prices; the old explanation keeps
    * its own uses and prices together in detail, without reseeding them into
    * this smaller question. This is a local plan comparison, not a new root
    * assessment or an account of every surveyed defence.
    */
  private def boundedAnswer(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      pair: List[PreparedPath], root: CommentaryAnswer.Section, decisions: List[Decision],
      previous: CommentaryAnswer.Answer, evidence: List[String]): Option[CommentaryAnswer.Answer] =
    import CommentaryAnswer.*
    val completed = decisions.map { value =>
      RootQuestionSummary.completePairAnswer(packet, paths, pair, "functional_response",
        List(root, Section(List(ResourceDecision(value))))).map(answer => grouped(answer.core.filterNot(_ == root)))
    }
    if completed.exists(_.isEmpty) then None else
      val lead = root :: completed.flatten
      // Keep the old selected episodes and the root ledger. Independent
      // detail-only examples stay in their existing proposal cards; copying
      // every such witness here can exceed the detail layer's path bound.
      val recordedPaths = (pair ++ previous.core.flatMap(_.references.map(_.path))).distinct
      val history = previous.core ++ previous.details.flatMap { section =>
        val statements = section.statements.filter(_.references.forall(ref => recordedPaths.exists(_ eq ref.path)))
        val refs = statements.flatMap(_.references).toSet
        Option.when(statements.nonEmpty)(section.copy(statements = statements,
          required = section.required.filter(refs), focus = section.focus.filter(f => refs(f.move))))
      }
      val recorded = grouped(history)
      RootQuestionSummary.completePairAnswer(packet, paths, pair, "functional_response", lead, List(recorded))
        .map(answer => answer.copy(details = List(recorded),
          evidence = (previous.evidence ++ evidence ++ answer.evidence).distinct,
          completionSources = Some(root.statements ++ decisions.map(ResourceDecision(_)))))
        .filter(answer => RootQuestionSummary.realize(paths, pair, answer).nonEmpty)

  private def matchedAnswer(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      pair: List[PreparedPath], resource: Resource, branches: List[Branch],
      rootSection: CommentaryAnswer.Section, center: List[Decision], other: List[Decision],
      previous: CommentaryAnswer.Answer, evidence: List[String],
      protect: (CommentaryAnswer.Answer, List[CommentaryAnswer.Section]) => CommentaryAnswer.Answer)
      : Option[CommentaryAnswer.Answer] =
    import CommentaryAnswer.*
    import RootQuestionSummary.MoveRef
    val represented = branches.flatMap(_.response.toList)
      .flatMap(_.comparison.pathsByMove.toList.sortBy(_._1).map(_._2)).distinct
    val old = previous.core.flatMap(_.statements).collect { case value: LegalReplyOption => value }
      .filter(value => value.proof.owner.proposalId == resource.owner.proposalId &&
        value.proof.owner.comparison == resource.owner.comparison && value.proof.condition == resource.condition &&
        value.proof.price.isEmpty && value.proof.action.kind == "legal_capture" &&
        branches.forall(_.response.exists(_.question.prefixMovesUci.size + 1 == value.proof.reference.size)))
    val rebased = old.flatMap(FunctionalReplyReading.rebase(paths, pair, _, represented)).distinct
    rebased match
      case proof :: Nil =>
        val count = proof.proof.reference.size
        def capturedUser(path: PreparedPath) = paths.continuationReadingFor(path).exists(_.events.exists(event =>
          event.plyOffset == count - 1 && event.originalVictim.contains(resource.actor.witness)))
        if !capturedUser(proof.reference) || !capturedUser(proof.alternative) then return None
        val transaction = List(MoveRef(proof.reference, count - 1),
          MoveRef(proof.available, proof.proof.option.size - 1), MoveRef(proof.alternative, count - 1)).distinct
        // The legal option is position-only. Close only its represented current
        // capture/recapture; computed prices must not seed a lifetime account.
        val seed = Section(List(Continuation(transaction)))
        val cost = RootQuestionSummary.completePairAnswer(packet, paths, pair, "functional_response", List(seed), Nil)
          .map(_.core.filterNot(_ == seed))
        val completedDetails = other.map { value =>
          RootQuestionSummary.completePairAnswer(packet, paths, pair, "functional_response",
            List(Section(List(ResourceDecision(value)))), Nil).map(answer => grouped(answer.core))
        }
        if cost.isEmpty || completedDetails.exists(_.isEmpty) then None else
          val question = Section(center.map(value => ResourceDecision(value)) :+ proof)
          val lead = List(rootSection, question)
          completeAnswerWithClosedParts(packet, paths, pair, lead, previous, evidence, protect,
            cost.get, completedDetails.flatten)
      case _ => None

  private def grouped(sections: List[CommentaryAnswer.Section]): CommentaryAnswer.Section =
    CommentaryAnswer.Section(sections.flatMap(_.statements).distinct,
      sections.flatMap(_.required).distinct, sections.flatMap(_.focus).distinct)

  private def withInheritedPriceContext(paths: PreparedPaths, pair: List[PreparedPath],
      answer: CommentaryAnswer.Answer): CommentaryAnswer.Answer =
    import CommentaryAnswer.*
    import RootQuestionSummary.MoveRef
    val leaders = (answer.core ++ answer.details).flatMap(_.statements).collect {
      case ResourceDecision(value) => value.path
    }.distinct
    def common(left: PreparedPath, right: PreparedPath): Int =
      (0 until left.moves.size.min(right.moves.size)).takeWhile(offset =>
        left.moves(offset) == right.moves(offset) && left.ids(offset) == right.ids(offset) &&
          left.positions.take(offset + 2) == right.positions.take(offset + 2)).size
    val core = answer.core.map { section =>
      val prices = section.statements.collect {
        case value: MaterialEvents if value.covered.nonEmpty => value.path
        case value: PieceExchange if value.covered.nonEmpty => value.path
      }.distinct
      val context = prices match
        case path :: Nil if pair.exists(_ eq path) && !section.statements.exists(_.isInstanceOf[RecordedContext]) =>
          val divergent = leaders.filter(leader => (leader ne path) &&
            leader.moves.headOption == path.moves.headOption && leader.ids.headOption == path.ids.headOption &&
            leader.positions.take(2) == path.positions.take(2)).map(leader => leader -> common(path, leader))
            .filter((leader, count) => count > 0 && count < path.moves.size && count < leader.moves.size &&
              path.moves(count) != leader.moves(count) &&
              section.covered.exists(ref => (ref.path eq path) && ref.offset >= count))
            .sortBy((leader, count) => (-count, leader.line.ref.id)).headOption
          divergent.map { (_, count) => RecordedContext((0 to count).toList.map(MoveRef(path, _))) }
        case _ => None
      context.fold(section)(value => section.copy(statements = value :: section.statements))
    }
    answer.copy(core = core)

  private[runtime] def completeAnswer(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      pair: List[PreparedPath], lead: List[CommentaryAnswer.Section], previous: CommentaryAnswer.Answer,
      evidence: List[String],
      protect: (CommentaryAnswer.Answer, List[CommentaryAnswer.Section]) => CommentaryAnswer.Answer)
      : Option[CommentaryAnswer.Answer] =
    completeAnswerWithClosedParts(packet, paths, pair, lead, previous, evidence, protect, Nil, Nil)

  private[runtime] def completeAnswerWithClosedParts(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      pair: List[PreparedPath], lead: List[CommentaryAnswer.Section], previous: CommentaryAnswer.Answer,
      evidence: List[String],
      protect: (CommentaryAnswer.Answer, List[CommentaryAnswer.Section]) => CommentaryAnswer.Answer,
      closedCore: List[CommentaryAnswer.Section], closedDetails: List[CommentaryAnswer.Section])
      : Option[CommentaryAnswer.Answer] =
    import CommentaryAnswer.*
    paths.requireOwner(packet)
    // A finite prior selected response keeps its own capture/check duties;
    // another child witness cannot borrow them from a matching prefix.
    val retained = previous.core.filter(_.actionReferences.exists(ref => !pair.exists(_ eq ref.path)))
    // A matched response, its two evaluated choices and their conditional cost
    // are one question. Share its container without removing any obligation.
    val closedLead = if closedCore.isEmpty || lead.isEmpty then lead ++ closedCore else
      lead.dropRight(1) :+ grouped(lead.takeRight(1) ++ closedCore)
    val selectedLead = closedLead ++ retained
    val sources = (lead.flatMap(_.statements) ++ previous.completionSources.toList.flatten).distinct
    // Root histories keep their ledger and verified uses. Independent
    // conditional examples remain available in their original proposal/card;
    // they are not copied as the prices of these newly investigated decisions.
    val recorded = previous.core.filterNot(retained.contains) ++ previous.details
    val details = closedDetails ++ recorded.map(section => section.copy(statements = section.statements.filter(statement =>
      statement.references.forall(ref => pair.exists(_ eq ref.path)))))
      .filter(_.statements.nonEmpty).map(section =>
        val refs = section.references.toSet
        section.copy(required = section.required.filter(refs), focus = section.focus.filter(f => refs(f.move))))
    RootQuestionSummary.completePairAnswer(packet, paths, pair, "functional_response", selectedLead, details)
      .map(answer => protect(answer.copy(evidence = (previous.evidence ++ evidence ++ answer.evidence).distinct,
        comparison = previous.comparison, completionSources = Some(sources)), previous.core))
      .flatMap { answer =>
        val contextual = withInheritedPriceContext(paths, pair, answer)
        Option.when(RootQuestionSummary.realize(paths, pair, contextual).nonEmpty)(contextual)
          .orElse(Option.when(RootQuestionSummary.realize(paths, pair, answer).nonEmpty)(answer))
      }

  private[runtime] def originalRow(paths: PreparedPaths, path: PreparedPath, at: Int,
      actor: Piece, square: String): Option[LegalMoveResource] =
    for
      current <- paths.trackedPieceAfter(path, actor.witness, at).flatten
      rows <- paths.positionView(path, at).flatMap(_.legalRows)
      row <- rows.find(row => row.actor == current && row.to.key == square && row.promotionRole.isEmpty)
    yield row

  private def selected(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      entries: List[Entry], roots: Set[String], profile: String,
      minimumDepth: Int): Option[Resource] =
    val resources = FunctionalReplyReading.resources(entries).filter { resource =>
      val owner = resource.owner
      val pair = owner.comparison
      resource.condition.kind == "control" && resource.condition.subject.nonEmpty &&
        resource.condition.square.contains(resource.square) &&
        roots == Set(pair.referenceMove, pair.alternativeMove) && owner.answer.exists(answer =>
        answer.engineProfile == profile && answer.referenceDepth.exists(_ >= minimumDepth) &&
        answer.alternativeDepth.exists(_ >= minimumDepth) &&
        List(answer.referenceLineId -> pair.referenceMove, answer.alternativeLineId -> pair.alternativeMove)
          .forall((id, move) => paths.paths.find(_.line.ref.id == id).exists(path =>
            path.certified.nonEmpty && path.ids == path.declared && path.moves == path.sourceMoves &&
              path.moves.headOption.contains(move))) &&
        paths.paths.find(_.line.ref.id == answer.referenceLineId).exists(path =>
          path.ids.headOption.contains(answer.referenceOccurrenceId) &&
            FunctionalReplyReading.conditionAt(paths, path, 0, resource.condition).nonEmpty &&
            FunctionalReplyReading.conditionAt(paths, path, 1, resource.condition)
              .contains(resource.expectedReference)) &&
        paths.paths.find(_.line.ref.id == answer.alternativeLineId).exists(path =>
          path.ids.headOption.contains(answer.alternativeOccurrenceId) &&
            FunctionalReplyReading.conditionAt(paths, path, 1, resource.condition)
              .contains(resource.expectedAlternative))) &&
        paths.paths.find(_.moves.startsWith(resource.witnessedPrefix :+ resource.move)).exists(path =>
          originalRow(paths, path, resource.witnessedPrefix.size, resource.actor, resource.square)
            .exists(_.move == resource.move))
    }
    val functions = resources.groupBy(resource => (resource.condition, resource.square))
    // Independent questions need an importance decision. Do not silently pick
    // one by UCI or amount of available detail.
    if functions.size != 1 then None else
      val first = resources.filter(_.witnessedPrefix.size == resources.map(_.witnessedPrefix.size).min)
      // Two later users of the same support are realizations of one function,
      // not two independent importance votes. Start at its earliest witnessed
      // decision, but do not pick between distinct users at that same time.
      Option.when(first.map(_.actor).distinct.size == 1)(first.sortBy(_.owner.proposalId).head)

  def task(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      entries: List[Entry], searches: List[Search], roots: Set[String], profile: String,
      minimumDepth: Int): Option[Task] =
    paths.requireOwner(packet)
    selected(packet, paths, entries, roots, profile, minimumDepth).map { resource =>
      val obligations = scala.collection.mutable.ListBuffer.empty[Obligation]
      val expected = scala.collection.mutable.Set.empty[(String, String)]
      val surveyed = scala.collection.mutable.Set.empty[String]
      val entry = entries.find(_.request.id == resource.owner.proposalId).get
      val optionalCapturer = entry.request.requirements.collectFirst {
        case fact: Fact if fact.id == "function_option" && fact.predicate.kind == "legal_capture" =>
          fact.predicate.target
      }.flatten
      def pool(prefix: List[String]) = CommentaryChoiceValue.survey(packet, prefix, searches, profile, minimumDepth)
      def owned(prefix: List[String]): Option[PreparedPath] =
        CommentaryProposal.observedPrefix(packet, paths, prefix)
      def rows(path: PreparedPath, at: Int) = paths.positionView(path, at).flatMap(_.legalRows)
      def comparison(role: String, prefix: List[String], reference: String, alternative: String) =
        CompareMoves(CommentarySelection.stableId(ValuePrefix + role,
          List(SourceVersion, resource.owner.proposalId, prefix.mkString(" "), reference, alternative,
            profile, CommentaryChoiceValue.pairDepth(packet, prefix, searches, profile, minimumDepth).toString)),
          prefix, reference, alternative)
      def choice(role: String, prefix: List[String], reference: String,
          path: PreparedPath, facts: List[Fact], phase: Int): Option[CompareMoves] =
        val survey = pool(prefix)
        val alternative = survey.flatMap(CommentaryChoiceValue.strongestOther(packet, paths, _, reference))
          .orElse(Option.when(survey.isEmpty)(rows(path, prefix.size).toList.flatten
            .map(_.move).sorted.find(_ != reference)).flatten)
        alternative.map { other =>
          val question = comparison(role, prefix, reference, other)
          obligations += Obligation(question, facts, phase,
            survey.toList.flatMap(value => value.work.workId :: value.pathsByMove.values.toList.flatMap(_.ids)))
          question
        }
      val branches = List(resource.owner.comparison.referenceMove, resource.owner.comparison.alternativeMove)
        .flatMap { rootMove =>
          pool(List(rootMove)) match
            case None =>
              owned(List(rootMove)).foreach { path =>
                val legal = rows(path, 1).toList.flatten.map(_.move)
                val reference = resource.witnessedPrefix.lift(1).filter(legal.contains).orElse(legal.sorted.headOption)
                reference.foreach(move => choice("first", List(rootMove), move, path, Nil, 0))
              }
              Nil
            case Some(firstPool) =>
              surveyed += rootMove
              val adequate = CommentaryChoiceValue.adequateReplies(packet, firstPool)
              // The already witnessed reply tests the same function under
              // both roots. It remains a named conditional test if this
              // finite survey did not return it among its adequate replies.
              val matched = resource.witnessedPrefix.lift(1).filter(reply =>
                owned(List(rootMove)).exists(path => rows(path, 1).exists(_.exists(_.move == reply))))
              (adequate ++ matched).distinct.flatMap { firstReply =>
                expected += rootMove -> firstReply
                val prefix = List(rootMove, firstReply)
                for
                  path <- firstPool.pathsByMove.get(firstReply).orElse(owned(prefix)).toList
                  row <- originalRow(paths, path, 2, resource.actor, resource.square).toList
                  current <- FunctionalReplyReading.conditionAt(paths, path, 2, resource.condition).toList
                  useFact = Fact(CommentarySelection.stableId(ValuePrefix + "legal", prefix :+ row.move),
                    Path("resource-decision", Declared(prefix)),
                    Predicate("legal_move", Some(resource.actor), square = Some(resource.square)),
                    Ply(2), expected = Some("legal"))
                  controlFact = Fact(CommentarySelection.stableId(ValuePrefix + "condition", prefix),
                    Path("resource-decision", Declared(prefix)), resource.condition, Ply(2), expected = Some(current))
                  question <- choice("use", prefix, row.move, path, List(useFact, controlFact), 1).toList
                yield
                  val use = CommentaryChoiceValue.read(packet, paths, question, searches, profile, minimumDepth)
                  val response = use.flatMap { value =>
                    val after = prefix :+ row.move
                    val resourcePath = value.reference
                    val legal = rows(resourcePath, after.size).toList.flatten
                    val victim = paths.trackedPieceAfter(resourcePath, resource.actor.witness, after.size).flatten
                    val capture = optionalCapturer.flatMap(actor =>
                      paths.trackedPieceAfter(resourcePath, actor.witness, after.size).flatten)
                      .flatMap(actor => legal.find(reply => reply.actor == actor && victim.exists(reply.capture.contains)))
                    val forecast = resourcePath.moves.lift(after.size).flatMap(move => legal.find(_.move == move))
                    val reference = capture.orElse(forecast).orElse(legal.sortBy(_.move).headOption)
                    val presence = Fact(CommentarySelection.stableId(ValuePrefix + "present", after),
                      Path("resource-response", Declared(after)), Predicate("presence", Some(resource.actor)),
                      Ply(after.size), expected = Some("present"))
                    reference.flatMap(reply => choice("response", after, reply.move,
                      resourcePath, List(presence), 2)).flatMap(q =>
                        CommentaryChoiceValue.read(packet, paths, q, searches, profile, minimumDepth))
                  }
                  val expected = if rootMove == resource.owner.comparison.referenceMove then resource.expectedReference
                    else resource.expectedAlternative
                  Branch(rootMove, firstReply, firstPool, row.move, question, use, response, current == expected,
                    matched.contains(firstReply))
              }
        }
      Task(resource, branches, obligations.toList, expected.toSet, surveyed.toSet)
    }

  /** Append immutable decisions to the existing task; the original fact-only
    * arm stays independent. A superseded seed is a historical question, never
    * a current strongest-choice obligation.
    */
  def withQuestions(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      entries: List[Entry], searches: List[Search], roots: Set[String], profile: String,
      minimumDepth: Int): List[Entry] =
    task(packet, paths, entries, searches, roots, profile, minimumDepth).fold(entries) { task =>
      val currentIds = task.obligations.map(_.comparison.id).toSet
      var room = (MaximumRequirements - entries.map(_.request.requirements.size).sum).max(0)
      entries.map { entry =>
        if entry.request.id != task.resource.owner.proposalId then entry
        else
          var current = entry.withCurrentDecisions(ValuePrefix, currentIds)
          task.obligations.sortBy(value => (value.phase, value.comparison.prefixMovesUci.mkString(" ")))
            .foreach { obligation =>
              val requirements = obligation.facts :+ obligation.comparison
              val known = current.request.requirements.map(value => value.id -> value).toMap
              require(requirements.forall(value => known.get(value.id).forall(_ == value)),
                "a resource realization cannot change an earlier exact question")
              val added = requirements.filterNot(value => known.contains(value.id))
              if added.size <= room then
                room -= added.size
                val source = Source("generator", "functional-choice-value", Some(SourceVersion),
                  (task.resource.evidence ++ obligation.sourceIds).distinct.take(64))
                val heads = (task.resource.contextId :: obligation.facts.map(_.id)) :+ obligation.comparison.id
                val members = (current.request.requirements.map(_.id) ++ added.map(_.id)).distinct
                val arm = GeneratedArm(CommentarySelection.stableId("functional-value-arm",
                  List(current.request.id, obligation.comparison.id)), Nil,
                  task.resource.owner.comparison.referenceMove, Some(task.resource.owner.comparison.alternativeMove),
                  heads, members, source, source.sourceRefs)
                current = current.copy(request = current.request.copy(requirements = current.request.requirements ++ added),
                  readings = current.readings ++ added.map(value => Reading(value.id)),
                  generatedArms = if current.generatedArms.exists(_.id == arm.id) then current.generatedArms
                    else current.generatedArms :+ arm)
            }
          current
      }
    }
