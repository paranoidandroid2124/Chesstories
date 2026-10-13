package io.chesstory.runtime

import scala.collection.mutable
import lila.chessjudgment.model.judgment.*

/** Select the shared physical question before histories are merged into prose.
  * Every response and price below is an action in an admitted finite line.
  * Different recorded outcomes do not assert causality or forced play. */
private[runtime] object RootSubjectComparison:
  import CommentaryAnswer.*
  import MoveWorkProjection.{Detail, Piece, Position}
  import ObservedLineProtocol.{PreparedPath, PreparedPaths}
  import RootQuestionSummary.{Arm, MoveRef}
  import LineContinuationInvestigation.Event

  private def selectedRootPair(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      pair: List[PreparedPath], answer: CommentaryInvestigation.Answer): Option[(PreparedPath, PreparedPath)] =
    for
      actual <- paths.paths.find(_.line.ref.id == answer.referenceLineId)
      alternative <- paths.paths.find(_.line.ref.id == answer.alternativeLineId)
      if pair.exists(_ eq actual) && pair.exists(_ eq alternative) &&
        packet.assembly.playedTransition.exists(_.moveUci == actual.moves.head)
    yield actual -> alternative

  /** Read only the current owned root pair and its separate same-position
    * retreat test. Exact move/piece checks also protect typed realization. */
  def validReplyConstraint(paths: PreparedPaths, value: ReplyConstraint): Boolean =
    val proof = value.proof
    val root = proof.owner.answer
    val test = proof.testAnswer
    val blocker = proof.blocker.witness
    val challenged = proof.challenged.witness
    val attacker = proof.attacker.witness
    val square = EvidenceSquare(proof.square)
    def position(path: PreparedPath, count: Int) = paths.positionView(path, count).map(new Position(_))
    def at(path: PreparedPath, piece: Piece, count: Int) = paths.trackedPieceAfter(path, piece, count).flatten
    def quiet(path: PreparedPath, offset: Int, original: Option[Piece] = None) =
      position(path, offset).exists(p => p.view.legalRows.exists(_.exists(row =>
        path.moves.lift(offset).contains(row.move) && row.capture.isEmpty && row.promotionRole.isEmpty &&
          original.forall(piece => at(path, piece, offset).contains(row.actor))))) &&
        position(path, offset + 1).exists(p =>
          p.view.currentCheckSources(chess.White).isEmpty && p.view.currentCheckSources(chess.Black).isEmpty)
    def controls(path: PreparedPath, count: Int, target: String) = for
      p <- position(path, count) if p.complete(RelationFactKind.GeometricControl)
      actor <- at(path, attacker, count)
    yield p.controlledBy(actor)(target)
    val declared = List(value.actual, value.alternative, value.available, value.otherwise).forall(path =>
      paths.paths.exists(_ eq path) && path.certified.nonEmpty && path.ids == path.declared &&
        path.moves == path.sourceMoves && path.moves.size >= 3)
    declared && proof.verified && root.evaluationRelation == "better" && root.qualityBand.nonEmpty &&
      value.actual.line.ref.id == root.referenceLineId && value.alternative.line.ref.id == root.alternativeLineId &&
      value.actual.ids.headOption.contains(root.referenceOccurrenceId) &&
      value.alternative.ids.headOption.contains(root.alternativeOccurrenceId) &&
      value.actual.moves.startsWith(proof.actual) && proof.actual.size == 3 &&
      proof.alternative == List(proof.owner.comparison.alternativeMove, proof.actual(1)) &&
      value.alternative.moves.head == proof.alternative.head &&
      value.available.moves.startsWith(proof.alternative :+ proof.test.referenceMove) &&
      value.otherwise.moves.startsWith(proof.alternative :+ proof.test.alternativeMove) &&
      proof.test.prefixMovesUci == proof.alternative && proof.test.alternativeMove == proof.actual(2) &&
      proof.test.referenceMove == challenged.square.key + proof.square &&
      test.exists(answer => answer.referenceLineId == value.available.line.ref.id &&
        answer.alternativeLineId == value.otherwise.line.ref.id &&
        value.available.ids.lift(2).contains(answer.referenceOccurrenceId) &&
        value.otherwise.ids.lift(2).contains(answer.alternativeOccurrenceId)) &&
      value.available.ids.take(2) == value.otherwise.ids.take(2) &&
      List(value.available, value.otherwise).forall(path =>
        path.ids.headOption == value.alternative.ids.headOption &&
          path.positions.headOption == value.alternative.positions.headOption) &&
      blocker != challenged && blocker.side == challenged.side && attacker.side != challenged.side &&
      challenged.role.name != "king" &&
      (0 to 2).forall(n => quiet(value.actual, n, Some(List(blocker, attacker, challenged)(n)))) &&
      List(value.available, value.otherwise).forall(path => quiet(path, 0) &&
        quiet(path, 1, Some(attacker)) && quiet(path, 2, Some(challenged))) &&
      List(value.actual, value.available, value.otherwise).forall(path =>
        (0 to 2).forall(count => at(path, challenged, count).contains(challenged))) &&
      (1 to 2).forall(count => at(value.actual, blocker, count).exists(_.square == square)) &&
      position(value.available, 2).exists(_.empty(proof.square)) &&
      at(value.available, challenged, 3).exists(_.square == square) &&
      controls(value.actual, 1, challenged.square.key).contains(false) &&
      List(value.actual, value.available, value.otherwise).forall(path =>
        controls(path, 2, challenged.square.key).contains(true) &&
          at(path, challenged, 3).exists(after => controls(path, 3, after.square.key).contains(false)))

  def withReplySpace(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      pair: List[PreparedPath], proofs: List[CommentaryIdea.ReplySpace], previous: Answer): Answer =
    paths.requireOwner(packet)
    if previous.question != "selected_comparison" || pair.size != 2 then return previous
    def path(id: String) = paths.paths.find(_.line.ref.id == id)
    val readings = proofs.flatMap { proof =>
      for
        (actual, alternative) <- selectedRootPair(packet, paths, pair, proof.owner.answer)
        test <- proof.testAnswer
        available <- path(test.referenceLineId)
        otherwise <- path(test.alternativeLineId)
        value = ReplyConstraint(actual, alternative, available, otherwise, proof)
        if validReplyConstraint(paths, value)
      yield value
    }.distinct
    readings match
      case value :: Nil =>
        val recorded = previous.core ++ previous.details
        val detail = Section(recorded.flatMap(_.statements).distinct, recorded.flatMap(_.required).distinct,
          recorded.flatMap(_.focus).distinctBy(_.move))
        // A generic history is not a separately verified competing answer.
        // Move its complete episodes together to detail. The first actual
        // check/capture in either arm still owns its reply and immediate trade;
        // the common completion path supplies those exact obligations.
        val firstReplies = pair.filter(path => path.certified.exists { (_, fact) =>
          fact.material.exists(_.captures.exists(_.plyOffset == 1)) ||
            fact.lineEvents.exists(event => event.plyOffset == 1 && event.kind == LineEventKind.Check)
        }).map(path => MoveRef(path, 1))
        // A retreat may also challenge the piece that drove it back. Name
        // that immediate functional difference only when both tested boards
        // certify it; the comparison still evaluates the whole continuation.
        val activity = for
          available <- paths.positionView(value.available, 3).map(new Position(_)).toList
          other <- paths.positionView(value.otherwise, 3).map(new Position(_)).toList
          if List(available, other).forall(_.complete(RelationFactKind.GeometricControl))
          mover <- paths.trackedPieceAfter(value.available, value.proof.challenged.witness, 3).flatten.toList
          peer <- paths.trackedPieceAfter(value.otherwise, value.proof.challenged.witness, 3).flatten.toList
          target <- paths.trackedPieceAfter(value.available, value.proof.attacker.witness, 3).flatten.toList
          if paths.trackedPieceAfter(value.otherwise, value.proof.attacker.witness, 3).flatten.contains(target) &&
            available.controlledBy(mover)(target.square.key) && !other.controlledBy(peer)(target.square.key)
        yield ControlContrast(MoveRef(value.available, 2), mover, Some(target), target.square.key, true,
          MoveRef(value.otherwise, 2), peer, Some(target), target.square.key, false) ->
          available.sourceRefs(available.sources(mover, List(target.square.key)))
        val core = List(Section(value :: activity.map(_._1))) ++
          Option.when(firstReplies.nonEmpty)(Section(List(Continuation(firstReplies)))).toList
        RootQuestionSummary.completePairAnswer(packet, paths, pair, "investigated_mechanism", core, List(detail))
          .map(answer => answer.copy(evidence = (answer.evidence ++ previous.evidence ++ value.proof.evidence ++
            activity.flatMap(_._2)).distinct,
            comparison = Some(ComparisonScope(Set(value.proof.blocker.witness, value.proof.challenged.witness)))))
          .getOrElse(previous)
      case _ => previous

  def currentReserveHold(paths: PreparedPaths, proof: CommentaryIdea.ReserveHold): Boolean =
    currentReserveBranch(paths, proof.owner, proof.branch, proof.test.prefixMovesUci.size,
      proof.ancestors, proof.source, proof.sourceArm)

  def currentReserveEntry(paths: PreparedPaths, proof: CommentaryIdea.ReserveEntry): Boolean =
    // The source owns the decision position. The tested receipt below owns
    // both the opponent's reply and the actual entry, which may replace the
    // source's forecast without borrowing another earlier history.
    currentReserveBranch(paths, proof.owner, proof.branch.take(proof.test.prefixMovesUci.size), proof.test.prefixMovesUci.size,
      proof.ancestors, proof.source, proof.sourceArm)

  def currentDirectReserveEntry(paths: PreparedPaths, proof: CommentaryIdea.DirectReserveEntry): Boolean =
    currentReserveBranch(paths, proof.owner, proof.branch.take(proof.offset + 1), proof.offset + 1,
      proof.ancestors, proof.source, proof.sourceArm)

  private def reserveTimingRouteIds(proof: CommentaryIdea.ReserveTiming): List[String] =
    proof.answer.toList.map(_.alternativeLineId) ++ List(proof.source.fold(proof.owner.answer.alternativeLineId)(source =>
      if proof.sourceArm == "reference" then source.answer.referenceLineId else source.answer.alternativeLineId))

  def validReserveTempo(paths: PreparedPaths, value: ReserveTempo): Boolean =
    val proof = value.proof
    val root = proof.owner.answer
    val prefix = proof.test.prefixMovesUci
    def position(path: PreparedPath, count: Int) = paths.positionView(path, count)
    def at(path: PreparedPath, count: Int) = paths.trackedPieceAfter(path, value.pawn, count).flatten
    def pieces(path: PreparedPath, count: Int) = position(path, count).filter(_.legalRows.nonEmpty).map(view =>
      chess.Square.all.flatMap(square => view.occupantAt(EvidenceSquare(square.key))).toSet)
    def move(path: PreparedPath, count: Int) = position(path, count).flatMap(_.legalRows)
      .flatMap(_.find(row => path.moves.lift(count).contains(row.move)))
    def owned(path: PreparedPath) = paths.paths.exists(_ eq path) && path.certified.nonEmpty &&
      path.ids == path.declared && path.moves == path.sourceMoves
    val tested = proof.answer.exists(answer => answer.evaluationRelation == "better" &&
      answer.qualityBand.contains(PrincipalVariationComparison.QualityBand.Separated) &&
      answer.referenceLineId == value.spent.line.ref.id && answer.alternativeLineId == value.kept.line.ref.id &&
      value.spent.ids.lift(2).contains(answer.referenceOccurrenceId) && value.kept.ids.lift(2).contains(answer.alternativeOccurrenceId))
    val physical = for
      before <- pieces(value.route, 2)
      returned <- pieces(value.route, value.through)
      spent <- pieces(value.spent, 3)
      rootMove <- move(value.actual, 0)
      keeping <- move(value.alternative, 0)
      now <- move(value.spent, 2)
      king <- move(value.kept, 2)
      advanced <- at(value.spent, 3)
    yield before == returned && before.forall(piece => Set("king", "pawn")(piece.role.name)) &&
      spent == (returned - value.pawn + advanced) && advanced != value.pawn &&
      rootMove.actor == value.pawn && rootMove.capture.isEmpty && rootMove.promotionRole.isEmpty &&
      keeping.actor != value.pawn && keeping.capture.isEmpty && keeping.promotionRole.isEmpty &&
      now.actor == value.pawn && now.move == rootMove.move && now.capture.isEmpty && now.promotionRole.isEmpty &&
      king.actor.role.name == "king" && king.actor.side == value.pawn.side &&
      at(value.alternative, 1).contains(value.pawn) && at(value.route, 2).contains(value.pawn) &&
      at(value.route, value.through).contains(value.pawn) &&
      (2 until value.through).forall(count => move(value.route, count).exists(row =>
        row.actor.role.name == "king" && row.capture.isEmpty && row.promotionRole.isEmpty) &&
        position(value.route, count + 1).exists(view => List(chess.White, chess.Black).forall(color =>
          view.currentCheckSources(color).isEmpty)))
    List(value.actual, value.alternative, value.spent, value.kept, value.route).forall(owned) &&
      value.pawn.role.name == "pawn" && prefix.size == 2 && value.through >= 5 && value.through % 2 == 1 &&
      value.through <= value.route.moves.size && value.through <= CommentaryFact.MaximumPly &&
      proof.owner.comparison.prefixMovesUci.isEmpty && root.evaluationRelation == "better" &&
      root.qualityBand.contains(PrincipalVariationComparison.QualityBand.Separated) && tested &&
      value.actual.line.ref.id == root.referenceLineId && value.alternative.line.ref.id == root.alternativeLineId &&
      value.actual.ids.headOption.contains(root.referenceOccurrenceId) && value.alternative.ids.headOption.contains(root.alternativeOccurrenceId) &&
      value.actual.moves.headOption.contains(proof.owner.comparison.referenceMove) &&
      value.alternative.moves.headOption.contains(proof.owner.comparison.alternativeMove) &&
      prefix.headOption == value.alternative.moves.headOption && proof.test.referenceMove == value.actual.moves.head &&
      value.spent.moves.startsWith(prefix :+ proof.test.referenceMove) && value.kept.moves.startsWith(prefix :+ proof.test.alternativeMove) &&
      value.spent.ids.take(2) == value.kept.ids.take(2) && value.route.ids.take(2) == value.kept.ids.take(2) &&
      value.route.moves.startsWith(prefix :+ proof.test.alternativeMove) && reserveTimingRouteIds(proof).contains(value.route.line.ref.id) &&
      currentReserveBranch(paths, proof.owner, prefix :+ proof.test.alternativeMove, 2, Nil, proof.source, proof.sourceArm) &&
      physical.contains(true)

  def withReserveTempo(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      pair: List[PreparedPath], proofs: List[CommentaryIdea.ReserveTiming], previous: Answer): Answer =
    paths.requireOwner(packet)
    val existingReserve = previous.core.flatMap(_.statements).exists {
      case _: ReserveHold | _: ReserveEntry => true
      case _ => false
    }
    if pair.size != 2 || (previous.question != "selected_comparison" && !existingReserve) then return previous
    def path(id: String) = paths.paths.find(_.line.ref.id == id)
    val candidates = for
      proof <- proofs
      (actual, alternative) <- selectedRootPair(packet, paths, pair, proof.owner.answer).toList
      answer <- proof.answer.toList
      spent <- path(answer.referenceLineId).toList
      kept <- path(answer.alternativeLineId).toList
      pawn <- paths.positionView(actual, 0).flatMap(_.legalRows).toList.flatten.filter(row =>
        actual.moves.headOption.contains(row.move) && row.actor.role.name == "pawn").map(_.actor)
      route <- reserveTimingRouteIds(proof).distinct.flatMap(path)
      through <- (5 to math.min(CommentaryFact.MaximumPly, route.moves.size)).by(2).toList
      value = ReserveTempo(actual, alternative, spent, kept, route, through, pawn, proof)
      if validReserveTempo(paths, value)
    yield value
    candidates.sortBy(value => (value.through, value.route.moves.mkString(" "))).headOption.fold(previous) { value =>
      if existingReserve then previous.copy(core = Section(List(value)) :: previous.core)
      else
        val prior = previous.core ++ previous.details
        val detail = Section(prior.flatMap(_.statements).distinct, prior.flatMap(_.required).distinct,
          prior.flatMap(_.focus).distinctBy(_.move))
        val prices = List(value.actual, value.spent, value.kept).distinct.flatMap(path =>
          paths.continuationReadings.find(_.path eq path).toList.flatMap(reading =>
            reading.events.find(event => event.plyOffset > 0 && event.originalVictim.contains(value.pawn)).toList
              .flatMap(event => path.certified.flatMap(_._2.material).toList.flatMap(_.events.filter(_.plyOffset == event.plyOffset)))
              .map(event => Section(List(MaterialEvents(path, List(event)))))))
        RootQuestionSummary.completePairAnswer(packet, paths, pair, "investigated_mechanism",
          Section(List(value)) :: prices, List(detail)).map(answer => answer.copy(
            evidence = (answer.evidence ++ previous.evidence).distinct,
            comparison = Some(ComparisonScope(Set(value.pawn))))).getOrElse(previous)
    }

  def validReserveBreak(paths: PreparedPaths, value: ReserveBreak): Boolean =
    val proof = value.proof
    val root = proof.owner.answer
    val offset = value.offset
    def owned(path: PreparedPath) = paths.paths.exists(_ eq path) && path.certified.nonEmpty &&
      path.ids == path.declared && path.moves == path.sourceMoves
    def position(path: PreparedPath, count: Int) = paths.positionView(path, count).map(new Position(_))
    def at(path: PreparedPath, piece: Piece, count: Int) = paths.trackedPieceAfter(path, piece, count).flatten
    def row(path: PreparedPath, count: Int) = position(path, count).flatMap(_.view.legalRows)
      .flatMap(_.find(move => path.moves.lift(count).contains(move.move)))
    val tested = proof.answer.exists(answer => answer.evaluationRelation == "worse" &&
      answer.qualityBand.contains(PrincipalVariationComparison.QualityBand.Separated) &&
      answer.referenceLineId == value.otherReply.line.ref.id && answer.alternativeLineId == value.held.line.ref.id &&
      value.otherReply.ids.lift(1).contains(answer.referenceOccurrenceId) &&
      value.held.ids.lift(1).contains(answer.alternativeOccurrenceId))
    val ownership = List(value.actual, value.alternative, value.held, value.otherReply).forall(owned) &&
      offset >= 3 && offset <= 7 && offset % 2 == 1 && value.actual.moves.size > offset && value.held.moves.size >= 2 &&
      proof.owner.comparison.prefixMovesUci.isEmpty && root.evaluationRelation == "better" &&
      root.qualityBand.contains(PrincipalVariationComparison.QualityBand.Separated) && tested &&
      value.actual.line.ref.id == root.referenceLineId && value.alternative.line.ref.id == root.alternativeLineId &&
      value.actual.ids.headOption.contains(root.referenceOccurrenceId) && value.alternative.ids.headOption.contains(root.alternativeOccurrenceId) &&
      value.actual.moves.headOption.contains(proof.owner.comparison.referenceMove) &&
      value.alternative.moves.headOption.contains(proof.owner.comparison.alternativeMove) &&
      proof.test.prefixMovesUci == value.alternative.moves.take(1) &&
      value.actual.moves.lift(1).contains(proof.test.alternativeMove) &&
      value.held.moves.startsWith(proof.test.prefixMovesUci :+ proof.test.alternativeMove) &&
      value.otherReply.moves.startsWith(proof.test.prefixMovesUci :+ proof.test.referenceMove) &&
      value.held.ids.headOption == value.alternative.ids.headOption && value.otherReply.ids.headOption == value.alternative.ids.headOption
    if !ownership then return false
    val physical = for
      rootBoard <- position(value.actual, 0)
      actualBefore <- position(value.actual, offset)
      actualAfter <- position(value.actual, offset + 1)
      retained <- position(value.held, 2)
      if List(rootBoard, actualBefore, retained).forall(_.complete(RelationFactKind.GeometricControl))
      move <- row(value.actual, 0)
      other <- row(value.alternative, 0)
      reply <- row(value.actual, 1)
      sameReply <- row(value.held, 1)
      advance <- row(value.actual, offset)
      guard <- at(value.actual, value.guard, offset)
      oldPawn <- at(value.actual, value.passer, offset)
      newPawn <- at(value.actual, value.passer, offset + 1)
      before <- actualBefore.view.pawnStates.find(_.square == oldPawn.square)
      after <- actualAfter.view.pawnStates.find(_.square == newPawn.square)
      reading <- paths.continuationReadings.find(_.path eq value.actual)
    yield value.guard.role.name == "pawn" && value.passer.role.name == "pawn" &&
      value.guard.side != value.passer.side && move.actor == value.guard &&
      move.capture.isEmpty && move.promotionRole.isEmpty && other.actor != value.guard &&
      other.capture.isEmpty && other.promotionRole.isEmpty &&
      rootBoard.contains(value.guard) && rootBoard.contains(value.passer) && rootBoard.contains(reply.actor) &&
      reply.actor.role.name == "pawn" && reply.actor.side == value.passer.side && sameReply.actor == reply.actor &&
      reply.capture.isEmpty && reply.promotionRole.isEmpty &&
      // Quiet intervening moves do not change the pawn structure. A non-pawn
      // capture could remove the obstacle for another reason and is excluded.
      (1 to offset).forall(n => row(value.actual, n).exists(move =>
        move.actor.role.name == "pawn" || (move.capture.isEmpty && move.promotionRole.isEmpty))) &&
      (if advance.capture.isEmpty then reply.actor != value.passer && reading.events.exists(event =>
        event.plyOffset > 1 && event.plyOffset < offset && event.originalVictim.contains(reply.actor))
       else reply.actor == value.passer && advance.capture.exists(_.role.name == "pawn")) &&
      (1 to offset).forall(count => position(value.actual, count).exists(board =>
        board.complete(RelationFactKind.GeometricControl) && at(value.actual, value.guard, count).exists(current =>
          !board.controlledBy(current)(advance.to.key)))) &&
      at(value.held, value.guard, 2).contains(value.guard) &&
      at(value.actual, value.passer, 2) == at(value.held, value.passer, 2) &&
      advance.actor == oldPawn && advance.promotionRole.isEmpty &&
      before.passed.contains(false) && after.passed.contains(true) &&
      rootBoard.controlledBy(value.guard)(advance.to.key) && !actualBefore.controlledBy(guard)(advance.to.key) &&
      retained.controlledBy(value.guard)(advance.to.key)
    physical.contains(true)

  def withReserveBreak(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      pair: List[PreparedPath], proofs: List[CommentaryIdea.ReserveResponse], previous: Answer): Answer =
    paths.requireOwner(packet)
    val terminal = previous.question == "observed_terminal_comparison"
    if (previous.question != "selected_comparison" && !terminal) || pair.size != 2 then return previous
    def path(id: String) = paths.paths.find(_.line.ref.id == id)
    val candidates = for
      proof <- proofs
      (actual, alternative) <- selectedRootPair(packet, paths, pair, proof.owner.answer).toList
      answer <- proof.answer.toList
      held <- path(answer.alternativeLineId).toList
      otherReply <- path(answer.referenceLineId).toList
      first <- paths.positionView(actual, 0).flatMap(_.legalRows).toList.flatten.filter(row =>
        actual.moves.headOption.contains(row.move))
      root <- paths.positionView(actual, 0).toList
      offset <- (3 to math.min(7, actual.moves.size - 1)).by(2).toList
      event <- paths.positionView(actual, offset).flatMap(_.legalRows).toList.flatten.filter(row =>
        actual.moves.lift(offset).contains(row.move) && row.actor.role.name == "pawn")
      passer <- chess.Square.all.flatMap(square => root.occupantAt(EvidenceSquare(square.key))).filter(piece =>
        piece.role.name == "pawn" && paths.trackedPieceAfter(actual, piece, offset).flatten.contains(event.actor))
      value = ReserveBreak(actual, alternative, held, otherReply, offset, first.actor, passer, proof)
      if validReserveBreak(paths, value)
    yield value
    candidates.sortBy(value => (value.offset, value.actual.moves.mkString(" "))).headOption.fold(previous) { value =>
      val prior = previous.core ++ previous.details
      val detail = Section(prior.flatMap(_.statements).distinct, prior.flatMap(_.required).distinct,
        prior.flatMap(_.focus).distinctBy(_.move))
      // A recorded terminal result and this verified early cause answer the same
      // root choice. Keep that result and its complete prices visible together.
      val retained = if terminal then previous.core else Nil
      val next = value.offset + 1
      // Creating a passer does not erase an immediate countercapture elsewhere.
      // Keep that actual reply (or promotion/check) with the claimed progress;
      // the common completion path still supplies its recapture/check answer.
      val reply = value.actual.certified.toList.flatMap { (_, fact) =>
        val events = fact.material.toList.flatMap(_.events.filter(event => event.plyOffset > 0 && event.plyOffset <= next &&
          (event.capture.nonEmpty || event.promotionGainCp > 0)))
        val ref = MoveRef(value.actual, next)
        if events.exists(_.plyOffset == next) && !retained.flatMap(_.covered).contains(ref) then
          List(Section(List(MaterialEvents(value.actual, events))))
        else if fact.lineEvents.exists(event => event.plyOffset == next && event.kind == LineEventKind.Check) &&
            !retained.flatMap(_.actionReferences).contains(ref) then List(Section(List(Continuation(List(ref)))))
        else Nil
      }
      val core = Section(List(value)) :: (retained ++ reply)
      RootQuestionSummary.completePairAnswer(packet, paths, pair,
        if terminal then previous.question else "investigated_mechanism",
        core, if terminal then previous.details else List(detail)).map(answer => answer.copy(
          evidence = (answer.evidence ++ previous.evidence).distinct,
          comparison = if terminal then previous.comparison
            else Some(ComparisonScope(Set(value.guard, value.passer))))).getOrElse(previous)
    }

  private def currentReserveBranch(paths: PreparedPaths, owner: CommentaryIdea.AnsweredComparison,
      branch: List[String], offset: Int, ancestors: List[(CommentaryIdea.AnsweredComparison, String)],
      source: Option[CommentaryIdea.AnsweredComparison], arm: String): Boolean =
    def owned(path: PreparedPath) = paths.paths.exists(_ eq path) && path.certified.nonEmpty &&
      path.ids == path.declared && path.moves == path.sourceMoves
    val alternative = paths.paths.find(_.line.ref.id == owner.answer.alternativeLineId)
    val chain = ancestors ++ source.toList.map(_ -> arm)
    val offsets = chain.map(_._1.comparison.prefixMovesUci.size)
    if chain.size > 3 || (ancestors.nonEmpty && source.isEmpty) ||
        offsets.lastOption.exists(_ >= offset) ||
        offsets.sliding(2).exists(pair => pair.size == 2 && pair.head >= pair.last) then return false
    val sourcePath = chain.foldLeft(alternative) { case (previous, (source, arm)) =>
      val answer = source.answer
      val prefix = source.comparison.prefixMovesUci
      val id = if arm == "reference" then answer.referenceLineId else answer.alternativeLineId
      val occurrence = if arm == "reference" then answer.referenceOccurrenceId else answer.alternativeOccurrenceId
      paths.paths.find(_.line.ref.id == id).filter(path =>
        source.proposalId == owner.proposalId && prefix.nonEmpty &&
          previous.exists(root => owned(root) && prefix.startsWith(root.moves.take(prefix.size))) &&
          CommentaryIdea.comparableReserveArm(answer, arm) &&
          path.ids.lift(prefix.size).contains(occurrence) && path.moves.startsWith(prefix :+
            (if arm == "reference" then source.comparison.referenceMove else source.comparison.alternativeMove)))
    }
    sourcePath.exists(path => owned(path) && branch.startsWith(path.moves.take(branch.size)))

  /** A worse sampled king retreat cannot justify the pawn's value when another
    * king move saves the position. Require one owned search of the entire legal
    * king class, and take its best returned member as the actual comparison. */
  def strongestReserveHold(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      proof: CommentaryIdea.ReserveHold, searches: List[TargetResponseInvestigation.Search]): Boolean =
    proof.testAnswer.exists(answer => searches.exists(search =>
      search.belongsTo(packet) && search.work.workId == answer.workId &&
        search.anchor.prefixMoves == proof.test.prefixMovesUci &&
        search.pathsByMove.get(proof.test.alternativeMove).flatMap(paths.positionView(_, proof.offset)).exists(view =>
          val kings = view.legalRows.toList.flatten.filter(row => row.actor.role.name == "king").map(_.move)
          search.representativePair(proof.test.alternativeMove, kings).exists { (waited, retreat) =>
            waited.line.ref.id == answer.alternativeLineId && retreat.line.ref.id == answer.referenceLineId
          })))

  def validReserveHold(paths: PreparedPaths, value: ReserveHold): Boolean =
    val proof = value.proof
    val offset = proof.offset
    val pawn = proof.pawn.witness
    val king = proof.king.witness
    def position(path: PreparedPath, count: Int) = paths.positionView(path, count).map(new Position(_))
    def at(path: PreparedPath, piece: Piece, count: Int) = paths.trackedPieceAfter(path, piece, count).flatten
    def move(path: PreparedPath, count: Int) = position(path, count).flatMap(_.view.legalRows)
      .flatMap(_.find(row => path.moves.lift(count).contains(row.move)))
    def owned(path: PreparedPath) = paths.paths.exists(_ eq path) && path.certified.nonEmpty &&
      path.ids == path.declared && path.moves == path.sourceMoves
    def control(path: PreparedPath, count: Int) = for
      board <- position(path, count) if board.complete(RelationFactKind.GeometricControl)
      guard <- at(path, king, count)
    yield board.controlledBy(guard)(value.square)
    val root = proof.owner.answer
    val tested = proof.testAnswer.exists(answer =>
      answer.referenceLineId == value.retreat.line.ref.id && answer.alternativeLineId == value.waited.line.ref.id &&
        value.retreat.ids.lift(offset).contains(answer.referenceOccurrenceId) &&
        value.waited.ids.lift(offset).contains(answer.alternativeOccurrenceId))
    val physical = for
      before <- position(value.waited, offset)
      waited <- position(value.waited, offset + 1)
      retreat <- position(value.retreat, offset + 1)
      wait <- move(value.waited, offset)
      step <- move(value.retreat, offset)
      entry <- move(value.retreat, offset + 1)
      rows <- waited.view.legalRows
    yield before.empty(value.square) && at(value.waited, pawn, offset).contains(pawn) &&
      at(value.waited, pawn, offset).contains(wait.actor) && wait.capture.isEmpty && wait.promotionRole.isEmpty &&
      wait.move == value.actual.moves.head && waited.view.currentCheckSources(chess.White).isEmpty &&
      waited.view.currentCheckSources(chess.Black).isEmpty &&
      at(value.retreat, king, offset).contains(step.actor) && step.capture.isEmpty && step.promotionRole.isEmpty &&
      at(value.waited, king, offset) == at(value.waited, king, offset + 1) &&
      entry.actor.role.name == "king" && entry.actor.side != king.side && entry.to.key == value.square && entry.capture.isEmpty &&
      control(value.waited, offset).contains(true) && control(value.waited, offset + 1).contains(true) &&
      control(value.retreat, offset + 1).contains(false) &&
      !rows.exists(row => row.actor == entry.actor && row.to.key == value.square) &&
      retreat.view.currentCheckSources(king.side).isEmpty
    proof.verified && tested && currentReserveHold(paths, proof) && physical.contains(true) &&
      List(value.actual, value.alternative, value.waited, value.retreat).forall(owned) && offset > 0 &&
      pawn.role.name == "pawn" && king.role.name == "king" && pawn.side == king.side &&
      proof.owner.comparison.prefixMovesUci.isEmpty && root.evaluationRelation == "better" &&
      root.qualityBand.contains(PrincipalVariationComparison.QualityBand.Separated) &&
      value.actual.line.ref.id == root.referenceLineId && value.alternative.line.ref.id == root.alternativeLineId &&
      value.actual.ids.headOption.contains(root.referenceOccurrenceId) && value.alternative.ids.headOption.contains(root.alternativeOccurrenceId) &&
      value.actual.moves.headOption.contains(proof.owner.comparison.referenceMove) &&
      value.alternative.moves.headOption.contains(proof.owner.comparison.alternativeMove) &&
      at(value.actual, pawn, 0).contains(pawn) && at(value.alternative, pawn, 1).contains(pawn) &&
      proof.branch.head == value.alternative.moves.head && proof.branch.last == value.actual.moves.head &&
      proof.test.prefixMovesUci == proof.branch.init && proof.test.alternativeMove == proof.branch.last &&
      value.waited.moves.startsWith(proof.branch) &&
      value.retreat.moves.startsWith(proof.branch.init :+ proof.test.referenceMove) &&
      value.waited.ids.take(offset) == value.retreat.ids.take(offset)

  def withReserveHold(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      pair: List[PreparedPath], proofs: List[CommentaryIdea.ReserveHold], previous: Answer): Answer =
    paths.requireOwner(packet)
    if previous.question != "selected_comparison" || pair.size != 2 then return previous
    def path(id: String) = paths.paths.find(_.line.ref.id == id)
    val candidates = proofs.flatMap { proof =>
      for
        (actual, alternative) <- selectedRootPair(packet, paths, pair, proof.owner.answer)
        test <- proof.testAnswer
        waited <- path(test.alternativeLineId)
        retreat <- path(test.referenceLineId)
        reply <- retreat.moves.lift(proof.offset + 1)
        value = ReserveHold(actual, alternative, waited, retreat, reply.slice(2, 4), proof)
        if validReserveHold(paths, value)
      yield value
    }.distinct.sortBy(value => (value.proof.offset, value.proof.branch.mkString(" ")))
    candidates.headOption.fold(previous) { value =>
      val prior = previous.core ++ previous.details
      val detail = Section(prior.flatMap(_.statements).distinct, prior.flatMap(_.required).distinct,
        prior.flatMap(_.focus).distinctBy(_.move))
      val events = List(value.actual, value.waited, value.retreat).distinct.flatMap { path =>
        paths.continuationReadings.find(_.path eq path).toList.flatMap { reading =>
          val prices = reading.events.find(event => event.plyOffset > 0 && event.originalVictim.contains(value.proof.pawn.witness)).toList
          val consequence = if !(path eq value.retreat) then Nil else
            reading.events.find(event => event.plyOffset > value.proof.offset + 1 &&
              event.originalActor.exists(actor => actor.role.name == "king" && actor.side != value.proof.king.witness.side) &&
              event.originalVictim.nonEmpty).toList
          (prices ++ consequence).distinct.flatMap(event => path.certified.flatMap(_._2.material).toList
            .flatMap(_.events.filter(_.plyOffset == event.plyOffset)).map(material => Section(List(MaterialEvents(path, List(material))))))
        }
      }
      val firstReplies = pair.filter(path => path.certified.exists { (_, fact) =>
        fact.material.exists(_.captures.exists(_.plyOffset == 1)) ||
          fact.lineEvents.exists(event => event.plyOffset == 1 && event.kind == LineEventKind.Check)
      }).map(path => MoveRef(path, 1))
      val core = Section(List(value)) :: events ++
        Option.when(firstReplies.nonEmpty)(Section(List(Continuation(firstReplies)))).toList
      RootQuestionSummary.completePairAnswer(packet, paths, pair, "investigated_mechanism", core, List(detail))
        .map(answer => answer.copy(evidence = (answer.evidence ++ previous.evidence ++ value.proof.evidence).distinct,
          comparison = Some(ComparisonScope(Set(value.proof.pawn.witness, value.proof.king.witness)))))
        .getOrElse(previous)
    }

  /** The tested guard-preserving pawn reply yields a concrete concession:
    * its immediate capture leaves a new passer. The waiting pawn need not be
    * that passer: a reserve on the other wing can serve the same purpose.
    * A weak score alone cannot supply this meaning. */
  private def reservePawnReply(paths: PreparedPaths, path: PreparedPath,
      proof: CommentaryIdea.ReserveEntry): Option[(Piece, Piece, Boolean, List[String])] =
    val offset = proof.last + 2
    for
      reading <- paths.continuationReadings.find(_.path eq path)
      capture <- reading.events.find(_.plyOffset == offset)
      original <- capture.originalActor if original.role.name == "pawn" && original.side == proof.pawn.witness.side
      victim <- capture.originalVictim if victim.role.name == "pawn"
      before <- paths.positionView(path, offset - 1).map(new Position(_))
      previous <- before.view.legalRows.flatMap(_.find(row => path.moves.lift(offset - 1).contains(row.move)))
      if paths.trackedPieceAfter(path, victim, offset - 1).flatten.contains(previous.actor)
      after <- paths.positionView(path, offset + 1).map(new Position(_))
      from <- paths.trackedPieceAfter(path, original, offset - 1).flatten
      pawn <- paths.trackedPieceAfter(path, original, offset + 1).flatten if pawn.role.name == "pawn"
      oldState <- before.view.pawnStates.find(_.square == from.square) if oldState.passed.contains(false)
      state <- after.view.pawnStates.find(_.square == pawn.square) if state.passed.contains(true)
      reserved <- paths.trackedPieceAfter(path, proof.pawn.witness, offset + 1).flatten
      supported = original != proof.pawn.witness && after.complete(RelationFactKind.GeometricControl) &&
        after.controlledBy(reserved)(pawn.square.key)
      sources = before.sourceRefs(oldState.sourceIds.filter(before.passageSources)) ++
        after.sourceRefs(state.sourceIds.filter(after.passageSources)) ++
        Option.when(supported)(after.sourceRefs(after.sources(reserved, List(pawn.square.key)))).toList.flatten
      if sources.nonEmpty
    yield (original, pawn, supported, sources.distinct)

  /** The counter can instead spend the opponent's last pawn wait. A later
    * move of the SAME reserved pawn holds the king while every remaining
    * legal reply releases the actual entry. Read the real side to move;
    * never manufacture a pass to count hypothetical pawn moves. */
  /** A last wait is a board condition, not a pawn-counter story. Keep the
    * same original kings, inspect every legal reply, and identify one common
    * released square. Its usefulness still needs its own value receipt. */
  def lastPawnWaitEntry(paths: PreparedPaths, path: PreparedPath, offset: Int,
      pawn: Piece, king: Piece, guard: Piece): Option[(String, List[String])] =
    def position(count: Int) = paths.positionView(path, count).map(new Position(_))
    def at(original: Piece, count: Int) = paths.trackedPieceAfter(path, original, count).flatten
    def row(count: Int) = position(count).flatMap(_.view.legalRows)
      .flatMap(_.find(move => path.moves.lift(count).contains(move.move)))
    def adjacent(a: String, b: String) =
      math.max(math.abs(a(0) - b(0)), math.abs(a(1) - b(1))) <= 1
    for
      before <- position(offset) if offset >= 0 && pawn.role.name == "pawn" &&
        king.role.name == "king" && guard.role.name == "king" && pawn.side == king.side && guard.side != king.side
      wait <- row(offset) if at(pawn, offset).contains(wait.actor) && wait.capture.isEmpty && wait.promotionRole.isEmpty
      waiting <- position(offset + 1) if waiting.complete(RelationFactKind.GeometricControl) &&
        waiting.view.currentCheckSources(chess.White).isEmpty && waiting.view.currentCheckSources(chess.Black).isEmpty
      if chess.Square.all.forall(square => waiting.occupant(square.key)
        .forall(piece => Set("king", "pawn")(piece.role.name)))
      ownKing <- at(king, offset) if at(king, offset + 1).contains(ownKing)
      enemyKing <- at(guard, offset) if at(guard, offset + 1).contains(enemyKing)
      reply <- row(offset + 1) if reply.actor == enemyKing && reply.capture.isEmpty && reply.promotionRole.isEmpty
      replies <- waiting.view.legalRows if replies.nonEmpty && replies.forall(_.actor == enemyKing)
      after <- position(offset + 2) if after.complete(RelationFactKind.GeometricControl)
      available <- after.view.legalRows
      entries = available.filter(entry => entry.actor == ownKing && entry.capture.isEmpty && entry.promotionRole.isEmpty &&
        before.empty(entry.to.key) && waiting.empty(entry.to.key) && waiting.controlledBy(enemyKing)(entry.to.key) &&
        replies.forall(move => !adjacent(move.to.key, entry.to.key)))
      entry <- Option.when(entries.size == 1)(entries.head)
      if at(guard, offset + 2).exists(current => !after.controlledBy(current)(entry.to.key))
      sources = waiting.sourceRefs(waiting.sources(enemyKing, List(entry.to.key)) ++ replies.map(_.sourceId)) ++
        before.sourceRefs(List(wait.sourceId)) ++ after.sourceRefs(List(entry.sourceId))
    yield entry.to.key -> sources.distinct

  private def reserveLastWait(paths: PreparedPaths, path: PreparedPath,
      proof: CommentaryIdea.ReserveEntry): Option[(ReserveReply, List[String])] =
    def position(count: Int) = paths.positionView(path, count).map(new Position(_))
    def at(piece: Piece, count: Int) = paths.trackedPieceAfter(path, piece, count).flatten
    def row(offset: Int) = position(offset).flatMap(_.view.legalRows)
      .flatMap(_.find(row => path.moves.lift(offset).contains(row.move)))
    val pawn = proof.pawn.witness
    val king = proof.king.witness
    val guard = proof.guard.witness
    (proof.last + 2 until path.moves.size - 1).iterator.flatMap { offset =>
      for
        release <- lastPawnWaitEntry(paths, path, offset, pawn, king, guard)
        (square, sources) = release
        // A counter may be a capture followed by its immediate pawn recapture.
        // Track the pawn left by that exact exchange. Neither a later unrelated
        // trade nor an unobserved recapture can turn a live pawn option into a
        // claim that the opponent has spent its last pawn move.
        counter <- row(proof.last + 1)
        initial <- position(0)
        counterPawn <- ('a' to 'h').iterator.flatMap(file => ('1' to '8').iterator
          .flatMap(rank => initial.occupant(s"$file$rank"))).find(piece =>
          piece.role.name == "pawn" && at(piece, proof.last + 1).contains(counter.actor))
        counterAt <- at(counterPawn, proof.last + 2)
        settled <- if counter.capture.isEmpty then Some((counterPawn, counterAt, proof.last + 2)) else
          for
            recapture <- row(proof.last + 2) if counter.capture.exists(_.role.name == "pawn") &&
              recapture.actor.role.name == "pawn" && recapture.actor.side == pawn.side &&
              recapture.capture.contains(counterAt) && recapture.promotionRole.isEmpty
            reading <- paths.continuationReadings.find(_.path eq path)
            capturer <- reading.events.find(_.plyOffset == proof.last + 2).flatMap(_.originalActor)
            after <- at(capturer, proof.last + 3)
          yield (capturer, after, proof.last + 3)
        if (settled._3 to offset + 1).forall(count => at(settled._1, count).contains(settled._2))
        if (settled._3 until offset).forall(index => row(index).exists(move =>
          move.capture.isEmpty && move.promotionRole.isEmpty && move.actor.role.name == "king"))
      yield (ReserveReply.LastPawnWait(offset, square, counter.capture.nonEmpty), sources.distinct)
    }.nextOption()

  private def reserveReply(paths: PreparedPaths, path: PreparedPath,
      proof: CommentaryIdea.ReserveEntry): Option[(ReserveReply, List[String])] =
    reservePawnReply(paths, path, proof).map(response =>
      ReserveReply.PassedPawn(response._2, response._3) -> response._4)
      .orElse(reserveLastWait(paths, path, proof))

  /** Test the retained choice at its original decision, even when a refreshed
    * PV uses another move. The legal option alone is never its value receipt. */
  def reserveEntryValue(paths: PreparedPaths, proof: CommentaryIdea.ReserveEntry): Option[CommentaryProposal.CompareMoves] =
    for
      answer <- proof.testAnswer if proof.verified && currentReserveEntry(paths, proof)
      waited <- paths.paths.find(_.line.ref.id == answer.referenceLineId)
      counter <- paths.paths.find(_.line.ref.id == answer.alternativeLineId)
      if waited.moves.startsWith(proof.branch.init) && reserveReply(paths, counter, proof).nonEmpty
      next <- waited.moves.lift(proof.last + 2) if next != proof.branch.last
      board <- paths.positionView(waited, proof.last + 2)
      king <- paths.trackedPieceAfter(waited, proof.king.witness, proof.last + 2).flatten
      if board.legalRows.exists(_.exists(row => row.move == proof.branch.last && row.actor == king &&
        row.capture.isEmpty && row.promotionRole.isEmpty))
    yield CommentaryProposal.CompareMoves("reserve_entry_value", proof.branch.init, proof.branch.last, next)

  /** The opponent can use any legal pawn move, including captures/promotions.
    * An unmodelled stronger counter blocks this explanation instead of being
    * silently replaced by a weaker quiet move with a more convenient story. */
  def strongestReserveCounter(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      proof: CommentaryIdea.ReserveEntry, searches: List[TargetResponseInvestigation.Search]): Boolean =
    proof.testAnswer.exists(answer => searches.exists(search =>
      search.belongsTo(packet) && search.work.workId == answer.workId &&
        search.anchor.prefixMoves == proof.test.prefixMovesUci &&
        search.pathsByMove.get(proof.test.referenceMove).flatMap(paths.positionView(_, proof.last + 1)).exists(view =>
          val pawns = view.legalRows.toList.flatten.filter(_.actor.role.name == "pawn").map(_.move)
          search.representativePair(proof.test.referenceMove, pawns).exists { (retreat, counter) =>
            retreat.line.ref.id == answer.referenceLineId && counter.line.ref.id == answer.alternativeLineId
          })))

  def validReserveEntry(paths: PreparedPaths, value: ReserveEntry): Boolean =
    val proof = value.proof
    val root = proof.owner.answer
    val last = proof.last
    val pawn = proof.pawn.witness
    val king = proof.king.witness
    val guard = proof.guard.witness
    def position(path: PreparedPath, count: Int) = paths.positionView(path, count).map(new Position(_))
    def at(path: PreparedPath, piece: Piece, count: Int) = paths.trackedPieceAfter(path, piece, count).flatten
    def move(path: PreparedPath, offset: Int) = position(path, offset).flatMap(_.view.legalRows)
      .flatMap(_.find(row => path.moves.lift(offset).contains(row.move)))
    def quiet(path: PreparedPath, offset: Int, actor: Piece) = move(path, offset).exists(row =>
      at(path, actor, offset).contains(row.actor) && row.capture.isEmpty && row.promotionRole.isEmpty) &&
      position(path, offset + 1).exists(p => p.view.currentCheckSources(chess.White).isEmpty &&
        p.view.currentCheckSources(chess.Black).isEmpty)
    def guards(path: PreparedPath, count: Int) = for
      p <- position(path, count) if p.complete(RelationFactKind.GeometricControl)
      current <- at(path, guard, count)
    yield p.controlledBy(current)(proof.square)
    def entry(path: PreparedPath, count: Int) = for
      p <- position(path, count)
      current <- at(path, king, count)
      rows <- p.view.legalRows
    yield rows.exists(row => row.actor == current && row.to.key == proof.square && row.capture.isEmpty)
    val owned = List(value.actual, value.alternative, value.waited, value.counter, value.entry).forall(path =>
      paths.paths.exists(_ eq path) && path.certified.nonEmpty && path.ids == path.declared && path.moves == path.sourceMoves)
    val tested = proof.testAnswer.exists(answer =>
      answer.referenceLineId == value.waited.line.ref.id && answer.alternativeLineId == value.counter.line.ref.id &&
        value.waited.ids.lift(last + 1).contains(answer.referenceOccurrenceId) &&
        value.counter.ids.lift(last + 1).contains(answer.alternativeOccurrenceId))
    val currentBranch = currentReserveEntry(paths, proof)
    val valuedEntry = value.available.fold(value.waited.moves.startsWith(proof.branch)) { available =>
      (for
        test <- proof.optionTest if reserveEntryValue(paths, proof).contains(test)
        answer <- proof.optionAnswer if CommentaryIdea.comparableReserveArm(answer, "reference")
        other <- paths.paths.find(_.line.ref.id == answer.alternativeLineId)
      yield available.line.ref.id == answer.referenceLineId && available.moves.startsWith(proof.branch) &&
        other.certified.nonEmpty && other.ids == other.declared && other.moves == other.sourceMoves &&
        other.moves.startsWith(test.prefixMovesUci :+ test.alternativeMove) &&
        available.ids.lift(last + 2).contains(answer.referenceOccurrenceId) &&
        other.ids.lift(last + 2).contains(answer.alternativeOccurrenceId) &&
        available.ids.take(last + 2) == value.waited.ids.take(last + 2) &&
        other.ids.take(last + 2) == available.ids.take(last + 2) &&
        available.positions.headOption == value.waited.positions.headOption).contains(true)
    }
    owned && proof.verified && tested && reserveReply(paths, value.counter, proof).exists(_._1 == value.reply) &&
      proof.owner.comparison.prefixMovesUci.isEmpty && proof.first > 0 && proof.first <= last &&
      (last - proof.first) % 2 == 0 && proof.branch.size == last + 3 &&
      pawn.role.name == "pawn" && king.role.name == "king" && guard.role.name == "king" &&
      pawn.side == king.side && guard.side != king.side &&
      root.evaluationRelation == "better" && root.qualityBand.contains(PrincipalVariationComparison.QualityBand.Separated) &&
      value.actual.line.ref.id == root.referenceLineId && value.alternative.line.ref.id == root.alternativeLineId &&
      value.actual.ids.headOption.contains(root.referenceOccurrenceId) &&
      value.alternative.ids.headOption.contains(root.alternativeOccurrenceId) &&
      value.actual.moves.headOption.contains(proof.owner.comparison.referenceMove) &&
      value.alternative.moves.headOption.contains(proof.owner.comparison.alternativeMove) &&
      value.waited.moves.startsWith(proof.branch.init) && valuedEntry && currentBranch &&
      proof.branch(proof.first) == value.actual.moves.head &&
      proof.test.prefixMovesUci == proof.branch.take(last + 1) &&
      proof.test.referenceMove == proof.branch(last + 1) &&
      value.counter.moves.startsWith(proof.test.prefixMovesUci :+ proof.test.alternativeMove) &&
      value.counter.moves.size > last + 2 && value.waited.ids.take(last + 1) == value.counter.ids.take(last + 1) &&
      value.waited.positions.headOption == value.actual.positions.headOption &&
      at(value.actual, pawn, 0).contains(pawn) && at(value.alternative, pawn, 1).contains(pawn) &&
      quiet(value.actual, 0, pawn) && move(value.alternative, 0).exists(row => row.actor != pawn &&
        row.actor.side == pawn.side && quiet(value.alternative, 0, row.actor)) &&
      (proof.first to last by 2).forall(offset => quiet(value.waited, offset, pawn)) &&
      (proof.first + 1 until last by 2).forall(offset => move(value.waited, offset).exists(row =>
        row.actor.role.name == "pawn" && row.actor.side == guard.side && row.capture.isEmpty && row.promotionRole.isEmpty)) &&
      (proof.first to last + 1).forall(count =>
        at(value.waited, king, count) == at(value.waited, king, proof.first) &&
          at(value.waited, guard, count) == at(value.waited, guard, proof.first) &&
          guards(value.waited, count).contains(true)) &&
      position(value.waited, proof.first).exists(_.empty(proof.square)) &&
      quiet(value.waited, last + 1, guard) && quiet(value.entry, last + 2, king) &&
      at(value.entry, king, last + 3).exists(_.square.key == proof.square) &&
      guards(value.waited, last + 2).contains(false) && entry(value.waited, last + 2).contains(true) &&
      move(value.counter, last + 1).exists(row => row.actor.side == guard.side && row.actor.role.name == "pawn" &&
        row.promotionRole.isEmpty) &&
      at(value.counter, guard, last + 2) == at(value.waited, guard, last + 1) &&
      guards(value.counter, last + 2).contains(true) && entry(value.counter, last + 2).contains(false)

  def withReserveEntry(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      pair: List[PreparedPath], proofs: List[CommentaryIdea.ReserveEntry], previous: Answer): Answer =
    paths.requireOwner(packet)
    if previous.question != "selected_comparison" || pair.size != 2 then return previous
    def path(id: String) = paths.paths.find(_.line.ref.id == id)
    val candidates = proofs.flatMap { proof =>
      for
        (actual, alternative) <- selectedRootPair(packet, paths, pair, proof.owner.answer)
        test <- proof.testAnswer
        waited <- path(test.referenceLineId)
        counter <- path(test.alternativeLineId)
        response <- reserveReply(paths, counter, proof)
        available = Option.when(!waited.moves.startsWith(proof.branch))(
          proof.optionAnswer.flatMap(answer => path(answer.referenceLineId))).flatten
        value = ReserveEntry(actual, alternative, waited, counter, response._1,
          proof.copy(evidence = (proof.evidence ++ response._2).distinct), available)
        if validReserveEntry(paths, value)
      yield value
    }.distinct
    candidates match
      case value :: Nil =>
        val previousSections = previous.core ++ previous.details
        val detail = Section(previousSections.flatMap(_.statements).distinct,
          previousSections.flatMap(_.required).distinct, previousSections.flatMap(_.focus).distinctBy(_.move))
        val firstReplies = pair.filter(path => path.certified.exists { (_, fact) =>
          fact.material.exists(_.captures.exists(_.plyOffset == 1)) ||
            fact.lineEvents.exists(event => event.plyOffset == 1 && event.kind == LineEventKind.Check)
        }).map(path => MoveRef(path, 1))
        val pawnPrices = List(value.actual -> 0, value.waited -> value.proof.first, value.counter -> value.proof.first,
          value.entry -> value.proof.first)
          .distinctBy(_._1).flatMap { (path, from) =>
            val usedPawns = Set(value.proof.pawn.witness) ++ Option.when(path eq value.counter)(
              reservePawnReply(paths, path, value.proof).map(_._1)).flatten ++
              Option.when((path eq value.counter) && (value.reply match
                case ReserveReply.LastPawnWait(_, _, true) => true
                case _ => false))(paths.continuationReadings.find(_.path eq path).flatMap(_.events
                  .find(_.plyOffset == value.proof.last + 2).flatMap(_.originalActor))).flatten
            paths.continuationReadings.find(_.path eq path).toList.flatMap { reading =>
              usedPawns.toList.flatMap(pawn => reading.events.find(event => event.plyOffset > from && event.originalVictim.contains(pawn)))
                .distinct.flatMap { loss =>
                  path.certified.flatMap(_._2.material).toList.flatMap(_.events.filter(_.plyOffset == loss.plyOffset))
                    .map(event => Section(List(MaterialEvents(path, List(event)))))
                }
            }
          }
        val counterTrade = value.counter.certified.flatMap(_._2.material).toList.flatMap(material =>
          Option.when(material.events.exists(event => event.plyOffset == value.proof.last + 1 && event.capture.nonEmpty))(
            Section(List(MaterialEvents(value.counter, material.events.filter(event =>
              event.plyOffset >= value.proof.last + 1 && event.plyOffset <= value.proof.last + 2))))))
        val core = List(Section(List(value))) ++ counterTrade ++ pawnPrices ++
          Option.when(firstReplies.nonEmpty)(Section(List(Continuation(firstReplies)))).toList
        RootQuestionSummary.completePairAnswer(packet, paths, pair, "investigated_mechanism", core, List(detail))
          .map(answer => answer.copy(evidence = (answer.evidence ++ previous.evidence ++ value.proof.evidence).distinct,
            comparison = Some(ComparisonScope(Set(value.proof.pawn.witness, value.proof.king.witness)))))
          .getOrElse(previous)
      case _ => previous

  /** Stop extending this question once its own functional answer and prices
    * can be completed. This is only used after the normal two inquiries;
    * it cannot displace an independent topic's existing allowance. */
  def reserveEntryReady(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      proofs: List[CommentaryIdea.ReserveEntry]): Boolean =
    proofs.groupBy(proof => (proof.owner.answer.referenceLineId, proof.owner.answer.alternativeLineId))
      .exists { case ((actualId, otherId), candidates) =>
        val pair = List(actualId, otherId).flatMap(id => paths.paths.find(_.line.ref.id == id))
        pair.size == 2 && RootQuestionSummary.completePairAnswer(packet, paths, pair, "selected_comparison", Nil)
          .exists(previous =>
            val answer = withReserveEntry(packet, paths, pair, candidates, previous)
            answer.question == "investigated_mechanism" && RootQuestionSummary.realize(paths, pair, answer).nonEmpty)
      }

  def validDirectReserveEntry(paths: PreparedPaths, value: DirectReserveEntry): Boolean =
    val proof = value.proof
    val root = proof.owner.answer
    val offset = proof.offset
    def owned(path: PreparedPath) = paths.paths.exists(_ eq path) && path.certified.nonEmpty &&
      path.ids == path.declared && path.moves == path.sourceMoves
    def at(path: PreparedPath, original: Piece, count: Int) = paths.trackedPieceAfter(path, original, count).flatten
    def row(path: PreparedPath, count: Int) = paths.positionView(path, count).flatMap(_.legalRows)
      .flatMap(_.find(move => path.moves.lift(count).contains(move.move)))
    val receipt = proof.testAnswer.exists(answer => proof.verified &&
      answer.referenceLineId == value.entry.line.ref.id && answer.alternativeLineId == value.otherwise.line.ref.id &&
      value.entry.ids.lift(offset + 2).contains(answer.referenceOccurrenceId) &&
      value.otherwise.ids.lift(offset + 2).contains(answer.alternativeOccurrenceId))
    val rootWait = row(value.actual, 0).exists(move => move.actor == proof.pawn.witness &&
      move.capture.isEmpty && move.promotionRole.isEmpty)
    val rootKept = row(value.alternative, 0).exists(move => move.actor != proof.pawn.witness &&
      move.actor.side == proof.pawn.witness.side && move.capture.isEmpty && move.promotionRole.isEmpty)
    List(value.actual, value.alternative, value.waited, value.entry, value.otherwise).forall(owned) &&
      offset > 0 && proof.branch.size == offset + 3 && proof.branch(offset) == proof.owner.comparison.referenceMove &&
      proof.owner.comparison.prefixMovesUci.isEmpty && root.evaluationRelation == "better" &&
      root.qualityBand.contains(PrincipalVariationComparison.QualityBand.Separated) && receipt && rootWait && rootKept &&
      value.actual.line.ref.id == root.referenceLineId && value.alternative.line.ref.id == root.alternativeLineId &&
      value.actual.ids.headOption.contains(root.referenceOccurrenceId) &&
      value.alternative.ids.headOption.contains(root.alternativeOccurrenceId) &&
      value.actual.moves.headOption.contains(proof.owner.comparison.referenceMove) &&
      value.alternative.moves.headOption.contains(proof.owner.comparison.alternativeMove) &&
      proof.branch.head == proof.owner.comparison.alternativeMove &&
      proof.test.id == "reserve_entry_value" && proof.test.prefixMovesUci == proof.branch.init &&
      proof.test.referenceMove == proof.branch.last && proof.square == proof.branch.last.slice(2, 4) &&
      value.entry.moves.startsWith(proof.branch) &&
      value.otherwise.moves.startsWith(proof.test.prefixMovesUci :+ proof.test.alternativeMove) &&
      value.waited.moves.startsWith(proof.branch.take(offset + 1)) && currentDirectReserveEntry(paths, proof) &&
      at(value.alternative, proof.pawn.witness, 1).contains(proof.pawn.witness) &&
      at(value.entry, proof.pawn.witness, offset).contains(proof.pawn.witness) &&
      value.entry.ids.take(offset + 1) == value.waited.ids.take(offset + 1) &&
      value.entry.ids.take(offset + 2) == value.otherwise.ids.take(offset + 2) &&
      value.entry.positions.headOption == value.alternative.positions.headOption &&
      lastPawnWaitEntry(paths, value.entry, offset, proof.pawn.witness, proof.king.witness, proof.guard.witness)
        .exists(_._1 == proof.square) &&
      row(value.entry, offset + 2).exists(move => at(value.entry, proof.king.witness, offset + 2).contains(move.actor) &&
        move.to.key == proof.square && move.capture.isEmpty && move.promotionRole.isEmpty) &&
      at(value.entry, proof.king.witness, offset + 3).exists(_.square.key == proof.square)

  def withDirectReserveEntry(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      pair: List[PreparedPath], proofs: List[CommentaryIdea.DirectReserveEntry], previous: Answer): Answer =
    paths.requireOwner(packet)
    val retainedMeaning = previous.core.flatMap(_.statements).exists {
      case _: ReserveTempo | _: ReserveHold => true
      case _ => false
    }
    if pair.size != 2 || previous.question != "selected_comparison" && !retainedMeaning then return previous
    def path(id: String) = paths.paths.find(_.line.ref.id == id)
    val candidates = proofs.flatMap { proof =>
      for
        (actual, alternative) <- selectedRootPair(packet, paths, pair, proof.owner.answer)
        answer <- proof.testAnswer
        entry <- path(answer.referenceLineId)
        otherwise <- path(answer.alternativeLineId)
        sourceId = proof.source.fold(proof.owner.answer.alternativeLineId)(source =>
          if proof.sourceArm == "reference" then source.answer.referenceLineId else source.answer.alternativeLineId)
        waited <- path(sourceId)
        release <- lastPawnWaitEntry(paths, entry, proof.offset, proof.pawn.witness, proof.king.witness, proof.guard.witness)
        value = DirectReserveEntry(actual, alternative, waited, entry, otherwise,
          proof.copy(evidence = (proof.evidence ++ release._2).distinct))
        if validDirectReserveEntry(paths, value)
      yield value
    }.distinct
    // Several owned continuations can implement the same retained resource.
    // They are evidence for one function, not competing questions merely
    // because the waiting manoeuvre or child comparator changed.
    val functions = candidates.groupBy(value => (value.proof.owner.comparison,
      value.actual.ids.headOption, value.alternative.ids.headOption,
      value.proof.pawn, value.proof.king, value.proof.guard, value.proof.square))
    functions.values.toList match
      case values :: Nil =>
        val value = values.minBy(value => (value.proof.offset, value.proof.branch.mkString(" "),
          value.proof.test.alternativeMove))
        val prior = previous.core ++ previous.details
        val detail = Section(prior.flatMap(_.statements).distinct, prior.flatMap(_.required).distinct,
          prior.flatMap(_.focus).distinctBy(_.move))
        val retained = if retainedMeaning then previous.core else Nil
        val firstReplies = pair.filter(path => path.certified.exists { (_, fact) =>
          fact.material.exists(_.captures.exists(_.plyOffset == 1)) ||
            fact.lineEvents.exists(event => event.plyOffset == 1 && event.kind == LineEventKind.Check)
        }).map(MoveRef(_, 1))
        val core = retained ++ List(Section(List(value))) ++
          Option.when(firstReplies.nonEmpty)(Section(List(Continuation(firstReplies)))).toList
        RootQuestionSummary.completePairAnswer(packet, paths, pair, "investigated_mechanism", core,
          if retainedMeaning then previous.details else List(detail)).map(answer => answer.copy(
            evidence = (answer.evidence ++ previous.evidence ++ value.proof.evidence).distinct,
            comparison = Some(ComparisonScope(Set(value.proof.pawn.witness, value.proof.king.witness)))))
          .getOrElse(previous)
      case _ => previous

  /** Lead with the differing transaction when the selected answer already
    * explains the complete finite material ledger in both arms. A sparse
    * price receipt cannot become a whole-line balance. A shared root capture
    * can lead with its differing price while retaining the actual root use;
    * when later material is only in detail, name the closed prefix's endpoint.
    * other root mechanisms keep the lead. No evaluation, causal conclusion
    * or compensation judgment is inferred from these counts. */
  def withMaterialContrast(answer: Answer, pair: List[PreparedPath], playedMove: Option[String] = None): Answer =
    val headings = answer.core.filter(section => section.statements.nonEmpty &&
      section.statements.forall(_.isInstanceOf[ComparisonSubject]))
    val rootCaptures = pair.flatMap(_.certified.flatMap(_._2.material).toList.flatMap(_.captures)
      .filter(_.plyOffset == 0))
    val sameRootTarget = rootCaptures match
      case List(left, right) => left.side == right.side && left.square == right.square &&
        left.capturedRole == right.capturedRole
      case _ => false
    if answer.comparison.isEmpty || headings.isEmpty || pair.size != 2 ||
        pair.exists(path => path.moves.size < 2 || path.automaticTerminal.nonEmpty) ||
        !sameRootTarget && answer.core.flatMap(_.statements).exists(statement =>
          conditionUses(statement).exists((condition, _) => condition.offset == 0)) then return answer
    val covered = answer.core.flatMap(_.covered).toSet
    val balances = pair.map { path =>
      path.certified.flatMap(_._2.material).flatMap { material =>
        val offsets = material.events.map(_.plyOffset)
        if material.events.isEmpty then Some(MaterialBalance(path, None))
        else
          val selected = offsets.filter(offset => covered(MoveRef(path, offset)))
          if selected == offsets then
            CommentaryMaterialAccount.calculate(material, selected, selected, path.moves.size)
              .map(cost => MaterialBalance(path, Some(cost)))
          else if sameRootTarget && selected.headOption.contains(0) &&
              material.events.filter(_.plyOffset <= selected.max).map(_.plyOffset) == selected then
            CommentaryMaterialAccount.calculate(material, selected, selected, path.moves.size)
              .map(cost => MaterialBalance(path, Some(cost), Some(selected.max)))
          else None
      }
    }
    def signature(value: MaterialBalance) = value.cost.fold((Nil, Nil))(cost =>
      (cost.whiteAdditionalLosses, cost.blackAdditionalLosses))
    balances match
      case List(Some(left), Some(right)) if signature(left) != signature(right) =>
        val ordered = List(left, right).sortBy(value => if playedMove.contains(value.path.moves.head) then 0 else 1)
        answer.copy(core = Section(ordered) :: answer.core.filterNot(headings.contains),
          details = headings ++ answer.details)
      case _ => answer

  /** A current defence is a smaller question than the original piece's whole
    * future history. Admit it only with an actual first-reply capture in the
    * unprotected arm and new unpinned support or an actual evasion in the other. All previously
    * owned prices, check replies and root-use episodes remain basic content;
    * no material count or different PV is used to infer the evaluation cause. */
  def withImmediateDefense(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths,
      pair: List[PreparedPath], positions: Map[PositionNodeRef, Position], previous: Answer): Answer =
    paths.requireOwner(packet)
    if previous.question != "selected_comparison" || previous.comparison.isEmpty || pair.size != 2 then return previous
    val arms = RootQuestionSummary.prepare(packet, paths, pair, positions).getOrElse(Nil)
    if arms.size != 2 || arms.exists(arm => arm.path.moves.size < 2 || arm.root.isEmpty ||
        arm.path.automaticTerminal.nonEmpty || arm.checks(0) || arm.captures.exists(_.plyOffset == 0) ||
        arm.material.events.exists(_.promotionGainCp > 0)) then return previous
    val candidates = for
      exposed <- arms
      loss <- exposed.captures.filter(_.plyOffset == 1)
      target <- loss.originalVictim.toList if exposed.root.exists(p => p.side == target.side && p != target)
      attacker <- loss.originalActor.toList if exposed.covers(0, attacker, target)
      before <- exposed.position(0).toList if before.complete(RelationFactKind.GeometricControl)
      now <- exposed.position(1).toList if now.complete(RelationFactKind.GeometricControl)
      if exposed.at(target, 1).contains(target) && now.controllers(target.square.key, target.side).isEmpty &&
        before.view.currentCheckSources(target.side).isEmpty &&
        previous.core.flatMap(_.covered).contains(exposed.ref(1))
      supported <- arms.filterNot(_ eq exposed)
      provider <- supported.root.toList if provider.side == target.side && provider.role.name != "king"
      current <- supported.at(target, 1).toList
      other <- supported.position(1).toList if other.complete(RelationFactKind.GeometricControl)
      support = provider != target && supported.gained(target) && supported.unpinned(1, provider) &&
        supported.covers(1, attacker, target) && current == target
      evasion = provider == target && current != target && supported.control(1, attacker, target).contains(false) &&
        other.controllers(current.square.key, !target.side).isEmpty
      if support || evasion
      legal <- now.view.legalRows.toList.flatten.filter(row =>
        EvidenceRef.sameMove(row.move, exposed.path.moves(1)) && row.captures(target))
    yield
      exposed.proofs ++= now.sourceRefs(List(legal.sourceId))
      target -> List(exposed, supported).map { arm =>
        val position = arm.position(1).get
        val after = arm.at(target, 1).get
        val attackers = position.controllers(after.square.key, !target.side)
        val defenders = position.controllers(after.square.key, target.side)
        (attackers ++ defenders).foreach(piece =>
          arm.proofs ++= position.sourceRefs(position.sources(piece, List(after.square.key))))
        val statement: Statement = if (arm eq supported) && evasion then
          Action(arm.ref(0), Detail.Evades(after, arm.at(attacker, 1).get))
        else TargetDefense(arm.ref(0), after, attackers, defenders)
        arm -> statement
      }
    val chosen = candidates.distinct match
      case (_, states) :: Nil => states
      case _ => return previous
    // Leaving one pawn attacked can be a counterattack on another. Select
    // only an actually used root attack; price that root mover's later loss
    // even after it relocates. A different pawn capture is not proof of equal
    // compensation, but omitting it would misdescribe this current choice.
    val counterplay = arms.flatMap { arm => arm.root.toList.flatMap { actor =>
      arm.captures.filter(event => event.plyOffset > 0 && event.originalActor.contains(actor) &&
        event.originalVictim.exists(target => target.side != actor.side && arm.gained(target) &&
          (1 to event.plyOffset).forall(arm.covers(_, actor, target))))
        .groupBy(_.originalVictim).values.map(_.minBy(_.plyOffset)).toList.map { gain =>
          val target = gain.originalVictim.get
          val account = arm.comparisonAccount(gain, actor)
          (arm, actor, target, account)
        }
    }}
    if counterplay.exists((arm, _, _, account) => !account.complete || account.dutyCosts.nonEmpty ||
        !arm.identityRoles(account.events)) then return previous
    val rootUses = previous.core.flatMap(_.statements).filter(statement =>
      conditionUses(statement).exists((condition, _) => condition.offset == 0)).toSet
    val retained = previous.core.map { section =>
      // Existing use/price episodes have independent obligations. A smaller
      // defence question does not certify that they can be broken apart.
      val episode = section.statements.exists(rootUses) && section.statements.exists(statement =>
        statement.covered.nonEmpty || (statement match
          case _: CheckSequence | _: CheckEvent => true
          case _ => false))
      val statements = if episode then section.statements else section.statements.filter(statement =>
        rootUses(statement) || statement.covered.nonEmpty || (statement match
          case _: MaterialEvents | _: CheckSequence | _: CheckEvent | _: Terminal | _: RecordedEnd => true
          case _ => false))
      val refs = statements.flatMap(_.references).toSet
      section.copy(statements = statements, required = section.required.filter(refs),
        focus = section.focus.filter(f => refs(f.move)))
    }.filter(_.statements.nonEmpty)
    val ordered = chosen.sortBy((arm, _) =>
      if packet.assembly.playedTransition.exists(_.moveUci == arm.path.moves.head) then 0 else 1)
    val single = retained.filter(section => section.references.map(_.path).distinct.size == 1)
    val joint = retained.filterNot(single.contains)
    val owned = retained.flatMap(_.covered).toSet
    val core = ordered.flatMap { (arm, state) =>
      val counters = counterplay.filter(_._1 eq arm)
      val attacks = counters.map { (_, actor, target, _) =>
        Action(arm.ref(0), Detail.Attack(arm.at(actor, 1).get, arm.at(target, 1).get, true))
      }
      val events = counters.flatMap(_._4.events).distinct.filterNot(event => owned(arm.ref(event.plyOffset)))
        .sortBy(_.plyOffset)
      val existingChecks = retained.flatMap(_.statements).flatMap {
        case value: CheckSequence => value.events
        case value: CheckEvent => List(value)
        case _ => Nil
      }
      val checks = arm.checkEvents(counters.flatMap(_._4.offsets).distinct.sorted).filterNot(existingChecks.contains)
      val offsets = events.map(_.plyOffset).toSet
      val price = Option.when(events.nonEmpty)(MaterialEvents(arm.path,
        arm.material.events.filter(event => offsets(event.plyOffset)))).toList
      val added = MoveWorkProjection.orderedCaptureSteps(List(arm.path), checks.headOption.toList.map(event =>
        CheckSequence(event.move, checks)), price)
      Section(state :: attacks) :: (single.filter(_.references.head.path eq arm.path) ++
        Option.when(added.nonEmpty)(Section(added)).toList)
    } ++ joint
    val recorded = previous.core ++ previous.details
    val detail = Section(recorded.flatMap(_.statements).distinct, recorded.flatMap(_.required).distinct,
      recorded.flatMap(_.focus).distinctBy(_.move))
    val selectedConditions = ordered.flatMap { (arm, state) =>
      state :: counterplay.filter(_._1 eq arm).map { (_, actor, target, _) =>
        Action(arm.ref(0), Detail.Attack(arm.at(actor, 1).get, arm.at(target, 1).get, true))
      }
    }
    val completionSources = previous.completionSources.map(sources =>
      (sources.filter(statement => retained.exists(_.statements.contains(statement))) ++ selectedConditions).distinct)
    previous.copy(question = "current_defense", core = core, details = List(detail),
      evidence = (previous.evidence ++ arms.flatMap(_.proofs)).distinct,
      comparison = previous.comparison.map(scope => scope.copy(subjects = scope.subjects ++
        counterplay.flatMap((_, actor, target, _) => List(actor, target)))),
      completionSources = completionSources)

  /** Preserve an independently verified root use and its own dependencies.
    * This is an explicit coexisting answer, not a replacement by the easier
    * subject story. Captures keep one core owner across both answers. */
  def protect(answer: Answer, bundles: List[Section],
      close: Section => Option[List[Section]] = _ => None,
      selectedReplies: Set[MoveRef] = Set.empty): Answer =
    val rootUses = bundles.flatMap(_.statements).filter(statement =>
      conditionUses(statement).exists((condition, _) => condition.offset == 0 || selectedReplies(condition))).toSet
    val uses = rootUses.toList.flatMap(conditionUses).map(_._2).toSet
    val admittedSources = mutable.ListBuffer.empty[Statement]
    var narrowed = false
    val protectedCore = bundles.flatMap { section =>
      // The selected use keeps what that exact action does (for example a
      // recapturing pawn's new support or a queen's attacks after using a
      // cleared route). It does not promote the rest of the bundle's history.
      // An already assembled price/check episode is an obligation, including
      // its replies and preconditions. Until that episode has an independently
      // closed narrower account, do not treat its other facts as dispensable.
      val hasEpisode = section.statements.exists(rootUses) && section.statements.exists(statement =>
        !rootUses(statement) && (statement.covered.nonEmpty || (statement match
          case _: CheckSequence | _: CheckEvent => true
          case _ => false)))
      val selected = section.statements.filter(statement =>
        rootUses(statement) || (statement match
          case value: Action => uses(value.move)
          case _: RecordedEnd | _: Terminal => statement.references.exists(ref =>
            uses.exists(use => use.path eq ref.path))
          case _ => false))
      val refs = selected.flatMap(_.references).toSet
      val candidate = Section(selected, section.required.filter(refs), section.focus.filter(value => refs(value.move)))
      val closed = Option.when(selected.exists(rootUses))(close(candidate)).flatten
      closed match
        case Some(core) =>
          narrowed = true
          admittedSources ++= selected
          core
        case None =>
          val retained = if hasEpisode then section else candidate
          admittedSources ++= retained.statements
          List(retained)
    }.filter(_.statements.nonEmpty)
    if protectedCore.isEmpty then answer else
      val statements = protectedCore.flatMap(_.statements).distinct
      val covered = statements.flatMap(_.covered).toSet
      // Sharing a bundle with an actual use does not make every neighbouring
      // relation part of its answer. Keep those facts available in detail;
      // the common completion path closes the selected use's replies/prices.
      val supplementary = bundles.map { section =>
        val remaining = section.statements.filterNot(statements.contains)
        val refs = remaining.flatMap(_.references).toSet
        Section(remaining, section.required.filter(refs), section.focus.filter(value => refs(value.move)))
      }.filter(_.statements.nonEmpty)
      val movedRequired = mutable.ListBuffer.empty[MoveRef]
      val movedFocus = mutable.ListBuffer.empty[Focus]
      val core = answer.core.map { section =>
        val retained = section.statements.flatMap {
          case value: PieceExchange =>
            val remaining = value.events.filterNot(event => covered(MoveRef(value.path, event.plyOffset)))
            Option.when(remaining.nonEmpty)(value.copy(events = remaining,
              recaptureOffsets = value.recaptureOffsets.intersect(remaining.map(_.plyOffset).toSet))).toList
          case value: MaterialEvents =>
            val remaining = value.events.filterNot(event => covered(MoveRef(value.path, event.plyOffset)))
            Option.when(remaining.nonEmpty)(value.copy(events = remaining)).toList
          case value if statements.contains(value) => Nil
          case value => List(value)
        }
        val refs = retained.flatMap(_.references).toSet
        movedRequired ++= section.required.filterNot(refs)
        movedFocus ++= section.focus.filterNot(value => refs(value.move))
        section.copy(statements = retained, required = section.required.filter(refs),
          focus = section.focus.filter(value => refs(value.move)))
      }.filter(_.statements.nonEmpty)
      val detail = Option.when(supplementary.nonEmpty)(Section(supplementary.flatMap(_.statements).distinct,
        supplementary.flatMap(_.required).distinct, supplementary.flatMap(_.focus).distinctBy(_.move))).toList
      // Independent closure must survive the next protection pass. Flattening
      // here would let one still-unclosed user retain another episode again.
      val required = (protectedCore.flatMap(_.required) ++ movedRequired).distinct
      val focus = (protectedCore.flatMap(_.focus) ++ movedFocus).distinctBy(_.move)
      val seen = mutable.Set.from(core.flatMap(_.statements))
      val protectedSections = protectedCore.map(section =>
        section.copy(statements = section.statements.filter(seen.add)))
        .filter(_.statements.nonEmpty)
      val retained = (core ++ protectedSections).map { section =>
        val refs = section.references.toSet
        section.copy(required = (section.required ++ required.filter(refs)).distinct,
          focus = (section.focus ++ focus.filter(value => refs(value.move))).distinctBy(_.move))
      }
      val references = retained.flatMap(_.references).toSet
      val context = required.filterNot(references)
      val framing = Option.when(context.nonEmpty)(Section(List(Continuation(context)), context,
        focus.filter(value => context.contains(value.move)))).toList
      answer.copy(core = retained ++ framing,
        details = (answer.details ++ detail).distinct,
        completionSources = if narrowed then Some((answer.completionSources.getOrElse(
          answer.core.flatMap(_.statements)) ++ admittedSources).distinct) else answer.completionSources,
        comparison = answer.comparison.map(scope => scope.copy(protectedActions = scope.protectedActions ++
          protectedCore.flatMap(_.actionReferences).toSet)))

  def answer(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths, pair: List[PreparedPath],
      positions: Map[PositionNodeRef, Position]): Option[Answer] =
    val arms = RootQuestionSummary.prepare(packet, paths, pair, positions) match
      case Some(value) => value
      case None => return None
    // A root-only alternative supplies no observed reply. Its initial board
    // presence cannot count as a completed comparison with an actual response.
    if arms.exists(arm => arm.path.moves.size < 2 || arm.material.events.exists(_.promotionGainCp > 0)) then return None
    val originals = arms.head.originals.filter(piece => arms.last.originals.contains(piece))
    def enemies(piece: Piece): List[Piece] = originals.filter(p => p.side != piece.side && arms.head.covers(0, p, piece))
    val anchors = originals.filter { piece => arms.exists { arm =>
      arm.captures.exists(event => event.plyOffset == 0 && event.originalVictim.contains(piece)) ||
        arm.root.contains(piece) && enemies(piece).exists(enemy => arm.control(1, enemy, piece).contains(false)) ||
        arm.gained(piece) && enemies(piece).nonEmpty
    }}
    if anchors.isEmpty then return None

    // A recapturer used for this subject is the same original piece in both
    // arms. Its later actual loss remains a price even after it relocates.
    def usedProviders(subjects: Set[Piece]): Set[Piece] = arms.flatMap { arm =>
      arm.captures.flatMap { loss =>
        for
          target <- loss.originalVictim.filter(subjects).toList
          attacker <- loss.originalActor.toList
          reply <- arm.captures.filter(event => event.plyOffset == loss.plyOffset + 1 &&
            event.originalVictim.contains(attacker))
          provider <- reply.originalActor.toList
          if arm.covers(0, provider, target) && arm.covers(loss.plyOffset, provider, target)
        yield provider
      } ++ arm.captures.filter(event => event.plyOffset == 0 && event.originalVictim.exists(subjects))
        .flatMap(_.originalActor)
    }.toSet

    // The actual first reply can attack the evading subject and an ally at
    // once. Retain that ally's observed capture/recapture; no hypothetical fork
    // continuation is generated from a geometric attack alone.
    def replyPartners(subjects: Set[Piece]): Set[Piece] = arms.flatMap { arm =>
      for
        attacker <- arm.mover(1).toList
        if subjects.exists(target => attacker.side != target.side && arm.covers(2, attacker, target))
        reply <- arm.captures.filter(_.plyOffset == 2)
        actor <- reply.originalActor.toList
        victim <- reply.originalVictim.toList
        if victim == attacker && arm.covers(2, attacker, actor)
      yield actor
    }.toSet

    // A different-square reply can trade an initial attacker for an existing
    // defender of the same supported target. Both participants are certified
    // by that initial contact, rather than by adjacent capture dates alone.
    // Read the actual pair: the running material ledger may already own an
    // earlier loss and therefore not name this second exchange as a link.
    def compensatedDefenders(subjects: Set[Piece]): Set[Piece] =
      val targets = subjects.filter(target => arms.exists(arm =>
        arm.root.exists(_.side == target.side) && arm.gained(target)))
      arms.flatMap { arm => arm.captures.flatMap { loss =>
        for
          gain <- arm.captures.filter(_.plyOffset == loss.plyOffset + 1)
          attacker <- loss.originalVictim.toList
          defender <- gain.originalVictim.toList
          if targets.exists(target => enemies(target).contains(attacker) && defender.side == target.side &&
            arm.covers(0, defender, target))
        yield defender
      }}.toSet

    case class Reading(arm: Arm, events: List[Event], conditions: List[Statement], subjects: Set[Piece], complete: Boolean)
    def read(arm: Arm, subjects: Set[Piece], users: Set[Piece]): Reading =
      val participants = subjects ++ users
      val seeds = arm.captures.filter(event => event.originalVictim.exists(participants) ||
        event.originalActor.exists(participants))
      val accounts = seeds.map { seed =>
        val focus = seed.originalActor.filter(participants).orElse(seed.originalVictim.filter(participants)).get
        arm.comparisonAccount(seed, focus)
      }
      val selected = mutable.ListBuffer.from(accounts.flatMap(_.events).distinct)
      var complete = accounts.forall(_.complete) && participants.forall(piece =>
        paths.trackedPieceAfter(arm.path, piece, arm.path.moves.size).nonEmpty)
      val conditions = mutable.ListBuffer.empty[Statement]
      val dependents = mutable.Set.empty[Piece]
      // A selected capturing departure can withdraw the sole support of a
      // different ally. This observed duty connects that ally's actual loss;
      // the intervening unrelated material is not a new price interval.
      seeds.foreach { departure =>
        departure.originalActor.filter(users).foreach { provider =>
          arm.captures.filter(_.plyOffset > departure.plyOffset).foreach { loss =>
            val duty = for
              target <- loss.originalVictim.filter(p => p.side == provider.side && p != provider)
              attacker <- loss.originalActor
              before <- arm.at(provider, departure.plyOffset)
              after <- arm.at(provider, departure.plyOffset + 1)
              victim <- arm.at(target, departure.plyOffset)
              position <- arm.position(departure.plyOffset)
              if position.complete(RelationFactKind.GeometricControl) &&
                position.controllers(victim.square.key, victim.side) == List(before) &&
                arm.control(departure.plyOffset + 1, provider, target).contains(false) &&
                (departure.plyOffset + 1 to loss.plyOffset).forall(count =>
                  arm.at(target, count).contains(victim) && arm.covers(count, attacker, target) &&
                    arm.control(count, provider, target).contains(false))
            yield (target, before, after, victim)
            duty.foreach { (target, before, after, victim) =>
              val account = arm.comparisonAccount(loss, target)
              complete &&= account.complete
              if account.complete then
                selected ++= account.events
                dependents += target
                val prior = seeds.filter(event => event.plyOffset < departure.plyOffset &&
                  event.originalActor.contains(provider) && arm.covers(event.plyOffset + 1, provider, target))
                  .sortBy(_.plyOffset).lastOption
                prior.foreach { event => arm.at(provider, event.plyOffset + 1).foreach { at =>
                  conditions += ControlState(arm.ref(event.plyOffset), at, Some(victim), victim.square.key, true)
                }}
                conditions += ControlState(arm.ref(departure.plyOffset), after, Some(victim), victim.square.key, false)
            }
          }
        }
      }
      Reading(arm, selected.toList.distinct.sortBy(_.plyOffset), conditions.toList.distinct,
        subjects ++ dependents, complete)

    // Root capturers differ by construction, so their identity alone is not a
    // differing answer. Compare later actual transactions and the original
    // subject's finite fate; independent equal opening trades stay equal.
    def signature(reading: Reading, subjects: Set[Piece]): (List[(Piece, Option[Piece])], List[(Option[Piece], Option[Piece])]) =
      subjects.toList.sortBy(_.square.key).map(piece => piece -> reading.arm.at(piece, reading.arm.path.moves.size)) ->
        reading.events.filter(_.plyOffset > 0).map(event => event.originalActor -> event.originalVictim)
    val initialSubjects = anchors.toSet
    val initialOpposition = initialSubjects.flatMap(enemies)
    val initialUsers = usedProviders(initialSubjects ++ initialOpposition) ++ replyPartners(initialSubjects) ++
      compensatedDefenders(initialSubjects)
    val initial = arms.map(read(_, initialSubjects, initialUsers))
    if initial.exists(value => !value.complete || !value.arm.identityRoles(value.events)) ||
        signature(initial.head, initialSubjects) == signature(initial.last, initialSubjects)
    then return None

    // Keep each subject's question separate until verified related actions
    // are known. Subjects with identical recorded outcomes and prices do not
    // become core merely because their root relation is explainable.
    val subjects = anchors.filter { piece =>
      val same = Set(piece)
      val users = usedProviders(same ++ same.flatMap(enemies)) ++ replyPartners(same) ++ compensatedDefenders(same)
      val readings = arms.map(read(_, same, users))
      signature(readings.head, same) != signature(readings.last, same)
    }.toSet
    if subjects.isEmpty then return None
    val users = usedProviders(subjects ++ subjects.flatMap(enemies)) ++ replyPartners(subjects) ++ compensatedDefenders(subjects)
    val preliminary = arms.map(read(_, subjects, users))
    val removedOpposition = subjects.flatMap(enemies).filter(piece => preliminary.exists(_.events.exists(_.originalVictim.contains(piece))))
    // A root support question owns both participants of the initial contact.
    // Equal opening exchanges cannot hide that original attacker's later loss
    // (or its different actual capturer) in one of the two continuations.
    val supportedOpposition = subjects.filter(piece => arms.exists(arm =>
      arm.root.exists(_.side == piece.side) && arm.gained(piece))).flatMap(enemies)
    val shared = subjects ++ supportedOpposition ++ removedOpposition ++ preliminary.flatMap(_.subjects)
    val readings = arms.map(read(_, shared, users))
    if readings.exists(value => !value.complete || !value.arm.identityRoles(value.events)) then return None

    def response(arm: Arm, subject: Piece): List[Statement] =
      arm.route(subject).flatMap { escape =>
        originals.filter(_.side != subject.side).flatMap { enemy =>
          if !arm.covers(escape, enemy, subject) || !arm.control(escape + 1, enemy, subject).contains(false) then Nil
          else
            val attack = (0 until escape).reverse.find(offset => arm.mover(offset).contains(enemy) &&
              arm.control(offset, enemy, subject).contains(false) && arm.covers(offset + 1, enemy, subject) &&
              (offset + 1 to escape).forall(arm.covers(_, enemy, subject)))
            val initial = escape == 0 && arm.covers(0, enemy, subject)
            if !initial && attack.isEmpty then Nil else
              val incoming = attack.toList.flatMap { offset =>
                for a <- arm.at(enemy, offset + 1).toList; t <- arm.at(subject, offset + 1).toList
                yield Action(arm.ref(offset), Detail.Attack(a, t, true))
              }
              val evasion = for a <- arm.at(enemy, escape).toList; t <- arm.at(subject, escape + 1).toList
                yield Action(arm.ref(escape), Detail.Evades(t, a))
              incoming ++ evasion
        }
      }
    def section(reading: Reading): Section =
      val arm = reading.arm
      val contacts = shared ++ shared.flatMap(enemies)
      val rootActions = arm.root.toList.flatMap { provider => contacts.toList.flatMap { target =>
        for
          actor <- arm.at(provider, 1).toList
          current <- arm.at(target, 1).toList
          if provider != target && arm.gained(target)
          detail = if provider.side != target.side then Detail.Attack(actor, current, true)
            else Detail.Support(actor, current, enemies(target).flatMap(arm.at(_, 1)), true)
        yield Action(arm.ref(0), detail)
      }}
      val replies = contacts.toList.flatMap(piece =>
        val values = response(arm, piece)
        if shared(piece) then values else
          values.grouped(2).filter(_.head.references.head.offset == 0).flatten.toList)
      val replyActions = arm.mover(1).toList.flatMap { attacker => (shared ++ users).toList.flatMap { target =>
        for
          actor <- arm.at(attacker, 2).toList
          current <- arm.at(target, 2).toList
          if attacker.side != target.side && arm.control(1, attacker, target).contains(false) && arm.covers(2, attacker, target)
        yield Action(arm.ref(1), Detail.Attack(actor, current, true))
      }}
      val responsePrices = replies.collect { case Action(move, Detail.Attack(actor, target, _)) =>
        replies.collect { case Action(escape, Detail.Evades(after, enemy)) if escape.offset > move.offset &&
          arm.originals.exists(piece => arm.at(piece, move.offset + 1).contains(actor) &&
            arm.at(piece, escape.offset).contains(enemy)) &&
          arm.originals.exists(piece => arm.at(piece, move.offset + 1).contains(target) &&
            arm.at(piece, escape.offset + 1).contains(after)) => escape }
          .flatMap(escape => arm.captures.filter(event => event.plyOffset > move.offset && event.plyOffset < escape.offset))
      }.flatten
      // A selected quiet retreat can itself open an opponent's actual capture
      // ray. Keep that reply as its price, just as for a capturing departure.
      val openings = shared.toList.flatMap { blocker => arm.route(blocker).flatMap { departure =>
        arm.captures.filter(_.plyOffset > departure).flatMap { use =>
          for
            beforeBlocker <- arm.at(blocker, departure).toList
            afterBlocker <- arm.at(blocker, departure + 1).toList
            user <- use.originalActor.toList if user != blocker
            victim <- use.originalVictim.toList if victim != blocker
            actor <- arm.at(user, departure).toList
            target <- arm.at(victim, departure).toList
            before <- arm.position(departure).toList if before.complete(RelationFactKind.RayBarrier)
            obstruction <- before.blockedRay(actor, target).toList if obstruction._2 == List(beforeBlocker)
            if arm.control(departure, user, victim).contains(false) &&
              (departure + 1 to use.plyOffset).forall(count => arm.at(user, count).contains(actor) &&
                arm.at(victim, count).contains(target) && arm.control(count, user, victim).contains(true))
            atUse <- arm.position(use.plyOffset).toList
            capture <- atUse.view.legalRows.toList.flatten.find(row => row.actor == actor && row.captures(target) &&
              EvidenceRef.sameMove(row.move, arm.path.moves(use.plyOffset))).toList
            afterUser <- arm.at(user, use.plyOffset + 1).toList
          yield
            arm.proofs ++= before.sourceRefs(List(obstruction._1)) ++ atUse.sourceRefs(List(capture.sourceId))
            ClearedRoute(arm.ref(departure), beforeBlocker, afterBlocker, user, arm.ref(use.plyOffset),
              afterUser, List(target), passage = true, development = None) -> arm.comparisonAccount(use, victim).events
        }
      }}
      val events = (reading.events ++ responsePrices ++ openings.flatMap(_._2)).distinct.sortBy(_.plyOffset)
      val otherLosses = readings.filterNot(_.arm eq arm).flatMap(_.events.flatMap(_.originalVictim)).toSet
      val retained = (shared ++ users).toList.filter(piece => subjects(piece) || otherLosses(piece))
        .sortBy(_.square.key).flatMap(piece => arm.at(piece, arm.path.moves.size).map(current =>
          PiecePresence(arm.ref(arm.path.moves.size - 1), piece, current)))
      val conditions = (rootActions ++ replyActions ++ replies ++ reading.conditions ++ openings.map(_._1))
        .distinct.sortBy(_.references.head.offset)
      val content = mutable.ListBuffer.empty[Statement]
      val introductions = List(0, 1).filter(offset => !conditions.exists(_.references.contains(arm.ref(offset))) &&
        !events.exists(_.plyOffset == offset)).map(arm.ref)
      val ordered = (conditions ++ Option.when(introductions.nonEmpty)(Continuation(introductions)).toList)
        .sortBy(_.references.head.offset)
      // Whole physical statements retain chronology and a single capture
      // owner. Split an exchange only where a condition changes its meaning.
      var pending = events
      ordered.foreach { condition =>
        val (before, after) = pending.span(_.plyOffset <= condition.references.head.offset)
        if before.nonEmpty then content += arm.pieceExchange(before)
        pending = after
        content += condition
      }
      if pending.nonEmpty then content += arm.pieceExchange(pending)
      content ++= retained
      Section(content.toList)
    val comparisonCore = readings.map(section)
    val heading = Section(List(ComparisonSubject(pair.map(MoveRef(_, 0)), subjects.toList.sortBy(_.square.key))))
    RootQuestionSummary.completePairAnswer(packet, paths, pair, "selected_comparison",
      heading :: comparisonCore).map(value => value.copy(
        evidence = (value.evidence ++ arms.flatMap(_.proofs)).distinct,
        comparison = Some(ComparisonScope(shared ++ users))))
