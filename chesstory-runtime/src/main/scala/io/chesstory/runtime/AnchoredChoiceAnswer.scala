package io.chesstory.runtime

import lila.chessjudgment.model.judgment.*

/** A completed local choice explains an owned root's retained advance. It
  * does not turn a conditional choice value into the whole root score cause. */
private[runtime] object AnchoredChoiceAnswer:
  import ObservedLineProtocol.{PreparedPath, PreparedPaths}
  import RootQuestionSummary.MoveRef
  import CommentaryAnswer.*

  final class Reading private[AnchoredChoiceAnswer] (
      val task: AnchoredChoiceReading.Task, val roots: List[PreparedPath],
      val advance: PreparedPath, val other: PreparedPath,
      val states: List[Statement], val roleEvidence: List[String],
      val priceClosure: Option[List[Section]] = Some(Nil),
      val laterAdvanceUnestablished: Boolean = false):
    val at = task.stage.prefix.size
    def closedPrices: List[Section] = priceClosure.toList.flatten
    def pricesComplete: Boolean = priceClosure.nonEmpty
    def rootAdvance = MoveRef(roots.head, 0)
    def rootDeferred = MoveRef(roots(1), 0)
    def prefix = (0 until at).toList.map(MoveRef(advance, _))
    def choice = MoveRef(advance, at)
    def alternative = MoveRef(other, at)
    def replies = List(MoveRef(advance, at + 1), MoveRef(other, at + 1))
    def references = (List(rootAdvance, rootDeferred) ++ prefix ++
      List(choice, alternative) ++ replies ++ states.flatMap(_.references)).distinct

  private def pawnRole(paths: PreparedPaths, path: PreparedPath, at: Int,
      original: RelationColoredPieceWitness): (List[Statement], List[String]) =
    val role = for
      before <- paths.positionView(path, at) if before.completeKinds(RelationFactKind.PawnPassage)
      after <- paths.positionView(path, at + 1) if after.completeKinds(RelationFactKind.PawnPassage)
      old <- paths.trackedPieceAfter(path, original, at).flatten
      pawn <- paths.trackedPieceAfter(path, original, at + 1).flatten if pawn.role.name == "pawn"
      state <- after.pawnStates.find(value => value.square == pawn.square && value.passed.contains(true))
    yield
      val position = new MoveWorkProjection.Position(after)
      val earlier = new MoveWorkProjection.Position(before)
      val move = MoveRef(path, at)
      val prior = before.pawnStates.find(_.square == old.square)
      val previous = prior.flatMap(_.passed)
      val kings = if position.complete(RelationFactKind.GeometricControl) then
        position.controllers(pawn.square.key, pawn.side).filter(_.role.name == "king") else Nil
      val support = kings.map(king => ControlState(move, king, Some(pawn), pawn.square.key, present = true))
      val checked = Option.when(position.complete(RelationFactKind.GeometricControl) &&
        after.currentCheckSources(!pawn.side).nonEmpty)(position.controls.keys.find(king =>
          king.role.name == "king" && king.side != pawn.side)).flatten
      val check = checked.toList.map(king => CheckStatus(move, king, checking = true,
        reply = Some(MoveRef(path, at + 1))))
      val passage = position.sourceRefs(state.sourceIds.filter(position.passageSources)) ++
        prior.toList.flatMap(value => earlier.sourceRefs(value.sourceIds.filter(earlier.passageSources)))
      val guards = kings.flatMap(king => position.sourceRefs(position.sources(king, List(pawn.square.key))))
      val checks = checked.toList.flatMap(king => position.sourceRefs(position.sources(pawn, List(king.square.key))))
      (Action(move, MoveWorkProjection.Detail.PassedPawn(pawn.square.key, previous)) :: (support ++ check),
        (passage ++ guards ++ checks).distinct)
    role.getOrElse(Nil -> Nil)

  /** Presence after the tested reply is a time-qualified fact. A selected
    * resource can still be spent later on that exact forecast. Close only
    * its first recorded removal and connected exchange/check obligations;
    * another branch's loss and unrelated later gains cannot price it. */
  private def resourcePrices(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      path: PreparedPath, at: Int, original: RelationColoredPieceWitness): Option[List[Section]] =
    for
      reading <- paths.continuationReadingFor(path) if reading.complete
      immediately <- paths.trackedPieceAfter(path, original, at + 2)
      last <- paths.trackedPieceAfter(path, original, path.moves.size)
      result <- reading.events.find(event => event.plyOffset >= at + 2 &&
        event.originalVictim.contains(original)) match
        case None => Option.when(immediately.isEmpty || last.nonEmpty)(Nil)
        case Some(loss) =>
          val positions = packet.resources.map(view =>
            view.position -> new MoveWorkProjection.Position(view)).toMap
          val arm = new RootQuestionSummary.Arm(packet, paths, path, positions, reading)
          val account = arm.comparisonAccount(loss, original)
          val captures = account.events.flatMap(event => arm.material.events.find(row =>
            row.plyOffset == event.plyOffset && path.ids.lift(row.plyOffset).contains(event.occurrenceId)))
          val promotions = MoveWorkProjection.capturedOriginalPromotions(arm, captures)
          val material = (captures ++ promotions).distinct.sortBy(_.plyOffset)
          val offsets = (account.offsets ++ promotions.map(_.plyOffset)).distinct.sorted
          val checks = arm.checkEvents(offsets)
          val stated = material.map(_.plyOffset).toSet ++ checks.flatMap(_.references.map(_.offset))
          val remaining = offsets.filterNot(stated).map(MoveRef(path, _))
          Option.when(account.complete && captures.size == account.events.size)(List(Section(
            List(RecordedContext(List(MoveRef(path, at), MoveRef(path, at + 1))),
              MaterialEvents(path, material), PieceAbsence(MoveRef(path, loss.plyOffset), original)) ++
              checks ++ Option.when(remaining.nonEmpty)(Continuation(remaining)).toList,
            offsets.map(MoveRef(path, _)))))
    yield result

  private def read(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      pair: List[PreparedPath], task: AnchoredChoiceReading.Task): Option[Reading] =
    def owned(path: PreparedPath) = paths.paths.exists(_ eq path) && path.certified.nonEmpty &&
      path.ids == path.declared && path.moves == path.sourceMoves
    def resistance(move: String): Option[PreparedPath] = for
      branch <- task.branches.find(_.ownMove == move)
      value <- branch.resistance.value if value.belongsTo(packet, paths)
      leader <- branch.resistance.leaderMove
      path <- value.comparison.pathsByMove.get(leader) if owned(path)
      if path.moves.startsWith(branch.afterChoicePrefix :+ leader) &&
        path.ids.take(branch.afterChoicePrefix.size) == branch.path.ids.take(branch.afterChoicePrefix.size) &&
        path.positions.take(branch.afterChoicePrefix.size + 1) == branch.path.positions.take(branch.afterChoicePrefix.size + 1)
    yield path
    for
      _ <- Option.when(pair.size == 2 && task.complete && task.stage.belongsTo(packet, paths)) (())
      use <- task.use if use.belongsTo(packet, paths)
      owner = task.stage.rootOwner
      if Set(owner.answer.referenceLineId, owner.answer.alternativeLineId) == pair.map(_.line.ref.id).toSet &&
        pair.forall(owned) && pair.forall(_.automaticTerminal.isEmpty)
      spent <- pair.find(_.line.ref.id == owner.answer.referenceLineId)
      deferred <- pair.find(_.line.ref.id == owner.answer.alternativeLineId)
      if task.stage.anchor.moves.headOption == deferred.moves.headOption &&
        task.stage.anchor.ids.headOption == deferred.ids.headOption &&
        task.stage.anchor.positions.take(2) == deferred.positions.take(2)
      advance <- resistance(use.question.referenceMove)
      other <- resistance(use.question.alternativeMove)
      if List(advance, other).forall(path =>
        path.moves.take(task.stage.prefix.size) == task.stage.prefix &&
          path.ids.take(task.stage.prefix.size) == task.stage.anchor.ids.take(task.stage.prefix.size) &&
          path.positions.take(task.stage.prefix.size + 1) == task.stage.anchor.positions.take(task.stage.prefix.size + 1))
      count = task.stage.prefix.size + 2
      a <- paths.trackedPieceAfter(advance, task.stage.pawn.witness, count)
      b <- paths.trackedPieceAfter(other, task.stage.pawn.witness, count)
      aPrices = resourcePrices(packet, paths, advance, task.stage.prefix.size, task.stage.pawn.witness)
      bPrices = resourcePrices(packet, paths, other, task.stage.prefix.size, task.stage.pawn.witness)
    yield
      def state(path: PreparedPath, current: Option[RelationColoredPieceWitness]): Statement =
        current.fold[Statement](PieceAbsence(MoveRef(path, count - 1), task.stage.pawn.witness))(actor =>
          PiecePresence(MoveRef(path, count - 1), task.stage.pawn.witness, actor))
      val role = pawnRole(paths, advance, task.stage.prefix.size, task.stage.pawn.witness)
      new Reading(task, List(spent, deferred), advance, other,
        role._1 ++ List(state(advance, a), state(other, b)), role._2,
        for a <- aPrices; b <- bPrices yield a ++ b)

  def withAnswer(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      pair: List[PreparedPath], tasks: List[AnchoredChoiceReading.Task], previous: Answer,
      protect: (Answer, List[Section]) => Answer): Answer =
    paths.requireOwner(packet)
    val current = tasks.distinct.flatMap(task => read(packet, paths, pair, task))
    if current.size != 1 then return previous
    val value = current.head
    if !value.pricesComplete then return previous
    val lead = List(Section(List(AnchoredDecision(value))))
    FunctionalChoiceReading.completeAnswerWithClosedParts(packet, paths, pair, lead, previous,
      value.task.stage.evidence ++ value.roleEvidence ++ value.task.branches.flatMap(_.resistance.value.toList)
        .flatMap(checked => checked.reference.ids ++ checked.alternative.ids), protect,
      value.closedPrices, Nil).getOrElse(previous)

  def withFamily(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      pair: List[PreparedPath], families: List[AnchoredChoiceReading.Family], previous: Answer,
      protect: (Answer, List[Section]) => Answer): Answer =
    paths.requireOwner(packet)
    val current = families.distinct.filter(value => value.belongsTo(packet, paths) && value.complete)
    if current.size != 1 then return previous
    val family = current.head
    val readings = family.stages.flatMap(task => read(packet, paths, pair, task))
    if readings.size != family.stages.size || readings.isEmpty then return previous
    val lead = readings.map(value => Section(List(AnchoredDecision(value))))
    val whole = Option.when(readings.forall(_.pricesComplete))(readings).flatMap(values =>
      FunctionalChoiceReading.completeAnswerWithClosedParts(packet, paths, pair, lead, previous,
        evidence(values), protect, values.flatMap(_.closedPrices), Nil))
    whole.orElse(
      withLeadingPlan(packet, paths, pair, family, readings, previous, protect)).getOrElse(previous)

  private def evidence(readings: List[Reading]): List[String] = readings.flatMap(value =>
    value.task.stage.evidence ++ value.roleEvidence ++
      value.task.branches.flatMap(_.resistance.value.toList)
        .flatMap(checked => checked.reference.ids ++ checked.alternative.ids)).distinct

  /** A displayed decision retains its complete prices in this same layer.
    * References or an absence statement do not express a capture/material
    * change, and CORE cannot borrow a price from DETAIL or another forecast. */
  private[runtime] def pricesExpressed(content: List[Statement]): Boolean =
    val references = content.flatMap(_.references).toSet
    val covered = content.flatMap(_.covered).toSet
    val material = content.flatMap {
      case value: MaterialEvents => List(value.path -> value.events)
      case value: Material => List(value.path -> value.cost.events)
      case _ => Nil
    }
    val exchanges = content.collect { case value: PieceExchange => value }
    def expressedExchange(path: PreparedPath, event: ObservedLineMaterialEvent): Boolean =
      // The ordinary exchange renderer names the original roles. Promotions
      // still need their literal material event, including a promoted victim.
      event.promotionGainCp == 0 && event.capture.exists(capture => exchanges.exists(value =>
        (value.path eq path) && value.events.exists(physical =>
          physical.plyOffset == event.plyOffset &&
            path.ids.lift(event.plyOffset).contains(physical.occurrenceId) &&
            path.certified.flatMap(_._2.material).exists(_.events.contains(event)) &&
            physical.originalActor.exists(actor => actor.side == event.movement.side &&
              actor.role == event.movement.beforeRole && actor.role == capture.attackerRole) &&
            physical.originalVictim.exists(victim => victim.side != event.movement.side &&
              victim.role == capture.capturedRole))))
    content.collect { case AnchoredDecision(value) => value }.forall(value =>
      value.priceClosure.exists(prices => prices.flatMap(_.required).forall(references) &&
        prices.flatMap(_.covered).forall(covered) && prices.flatMap(_.statements).forall {
          case expected: MaterialEvents => expected.events.forall(event =>
            material.exists { (path, events) => (path eq expected.path) && events.contains(event) } ||
              expressedExchange(expected.path, event))
          case _ => true
        }))

  /** A current first-response receipt selects the central plan. Its own
    * leading resistance selects the later use; a sibling's continuation is
    * not a substitute. Other finite plans are independent, price-closed
    * detail accounts. Every selected account retains its original witnesses.
    */
  private def withLeadingPlan(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      pair: List[PreparedPath], family: AnchoredChoiceReading.Family,
      readings: List[Reading], previous: Answer,
      protect: (Answer, List[Section]) => Answer): Option[Answer] =
    def sameSource(value: Option[CommentaryChoiceValue.Checked], source: CommentaryChoiceValue.Checked): Boolean =
      value.exists(current => current.belongsTo(packet, paths) && source.belongsTo(packet, paths) &&
        current.question == source.question && current.answer == source.answer &&
        current.survey.work == source.survey.work && current.comparison.work == source.comparison.work &&
        (current.reference eq source.reference) && (current.alternative eq source.alternative))
    val initial = for
      root <- family.roots.find(_.rootMove == family.owner.comparison.alternativeMove)
      leader <- root.resistance.leaderMove
      source <- root.resistance.value if source.belongsTo(packet, paths)
      value <- readings.find(value => value.task.stage.level == 0 &&
        value.task.stage.prefix == List(root.rootMove, leader) &&
        sameSource(value.task.stage.source, source))
    yield value
    initial.flatMap { first =>
      val later = for
        use <- first.task.use.toList if use.refuted || use.useful
        branch <- first.task.branches.filter(_.keepsPawnHere)
        source <- branch.resistance.value.toList if source.belongsTo(packet, paths)
        leader <- branch.resistance.leaderMove.toList
        next <- readings.filter(value => value.task.stage.parentStageId.contains(first.task.stage.id) &&
          sameSource(value.task.stage.source, source) &&
          value.task.stage.prefix.startsWith(branch.afterChoicePrefix :+ leader))
      yield next
      // There is no importance rule for two independent later uses. Do not
      // choose one by UCI, prefix length or how much prose is available.
      if later.distinct.size > 1 || (first :: later.distinct).exists(!_.pricesComplete) then None
      else
        val centered = first :: later.distinct
        val boundary = first.task.branches.filter(_.keepsPawnHere).exists(branch =>
          branch.resistance.value.exists(source => family.followupBoundary.exists(value =>
            value.parentStageId.contains(first.task.stage.id) &&
              value.reason == "no_owned_later_advance" && sameSource(value.source, source) &&
              branch.resistance.leaderMove.exists(move => value.prefix == (branch.afterChoicePrefix :+ move)))))
        val boundedFirst = if boundary then new Reading(first.task, first.roots, first.advance,
          first.other, first.states, first.roleEvidence, first.priceClosure,
          laterAdvanceUnestablished = true) else first
        val core = (boundedFirst :: later.distinct).map(value => Section(List(AnchoredDecision(value))))
        val others = readings.filterNot(centered.contains).sortBy(value =>
          (value.task.stage.level, value.task.stage.prefix.mkString(" ")))
        def closed(value: Reading): Option[Section] =
          Option.when(value.pricesComplete)(value).flatMap(current =>
            RootQuestionSummary.completePairAnswer(packet, paths, pair, "functional_response",
              List(Section(List(AnchoredDecision(current)))) ++ current.closedPrices)).flatMap { answer =>
            // The outer pair completion independently owns its root prices.
            // Keep the whole conditional account, including every foreign
            // capture/check added by this account's own completion.
            val sections = answer.core.filter(_.references.exists(ref => !pair.exists(_ eq ref.path)))
            Option.when(sections.nonEmpty)(Section(sections.flatMap(_.statements).distinct,
              sections.flatMap(_.required).distinct, sections.flatMap(_.focus).distinct))
          }
        def complete(details: List[Section], selected: List[Reading]): Option[Answer] =
          FunctionalChoiceReading.completeAnswerWithClosedParts(packet, paths, pair,
            core, previous, evidence(centered ++ selected), protect,
            centered.flatMap(_.closedPrices), details)
        // The bounded claim stands on the current leading plan, rather than
        // requiring every completed family member in the same basic answer.
        // A detail account enters only as a whole closed proposition. Those
        // outside this display remain in their original comparison cards.
        complete(Nil, Nil).map { base =>
          others.foldLeft((base, List.empty[Section], List.empty[Reading])) {
            case ((current, details, selected), value) =>
              closed(value).flatMap(section => complete(details :+ section, selected :+ value)
                .map(answer => (answer, details :+ section, selected :+ value)))
                .getOrElse((current, details, selected))
          }._1
        }
    }
