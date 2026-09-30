package io.chesstory.runtime

import lila.chessjudgment.model.judgment.*

/** A real root capture and a retained legal recapture answer one physical question.
  * A passed-turn candidate, its score and its line are never evidence here.
  */
private[runtime] object RootThreatSummary:
  import MoveWorkProjection.{ Piece, Position }
  import ObservedLineProtocol.{ PreparedPath, PreparedPaths }
  import RootQuestionSummary.{ Arm, Result }
  import CommentaryAnswer.*

  def read(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths, pair: List[PreparedPath],
      positions: Map[PositionNodeRef, Position]): Option[Result] =
    readAnswer(packet, paths, pair, positions).flatMap(answer => RootQuestionSummary.realize(paths, pair, answer))

  def readAnswer(packet: EvidenceBackedJudgmentPacket, paths: PreparedPaths, pair: List[PreparedPath],
      positions: Map[PositionNodeRef, Position]): Option[Answer] =
    val arms = RootQuestionSummary.prepare(packet, paths, pair, positions) match
      case Some(value) if value.forall(a => a.root.nonEmpty && a.path.moves.size > 1 &&
          !a.material.events.exists(_.promotionGainCp > 0)) => value
      case _ => return None

    // Keep both actual continuations intact. A small threat account cannot
    // replace the root mover's later recovery, loss or ordinary replies.
    def material(a: Arm, events: List[LineContinuationInvestigation.Event], offsets: List[Int]): Option[Material] =
      CommentaryMaterialAccount.calculate(a.material, events.map(_.plyOffset), offsets, a.path.moves.size)
        .map(Material(a.path, _))

    def primary(a: Arm, attacker: Piece, target: Piece): Option[Statement] =
      val focal = Set(attacker, target, a.root.get)
      val seeds = a.captures.filter(e => e.plyOffset <= 1 ||
        e.originalActor.exists(focal) || e.originalVictim.exists(focal))
      val accounts = seeds.map(e => a.account(e,
        if e.originalActor == a.root || e.originalVictim == a.root then a.root.get else attacker))
      val events = accounts.flatMap(_.events).distinct.sortBy(_.plyOffset)
      val offsets = a.path.moves.indices.toList
      if !accounts.forall(c => c.complete && c.dutyCosts.isEmpty && c.offsets.forall(offsets.contains)) ||
          events.toSet != a.captures.toSet || a.checks(offsets.last) then None
      else if events.nonEmpty then material(a, events, offsets)
      else Some(Continuation(offsets.map(a.ref)))

    val readings = paths.continuationReadings
    arms.zipWithIndex.iterator.flatMap { (remove, n) =>
      val prepare = arms(1 - n)
      for
        removal <- remove.captures.find(_.plyOffset == 0).iterator
        attacker <- removal.originalVictim.iterator
        target <- remove.root.iterator
        provider <- prepare.root.filter(p => p != target && p.side == target.side).iterator
        if removal.originalActor.contains(target) && attacker.side != target.side &&
          remove.covers(0, attacker, target) &&
          paths.pieceAfter(packet, remove.path, attacker, 1).contains(None) &&
          prepare.quiet(0) && prepare.gained(target) && prepare.at(attacker, 1).nonEmpty
        direct <- primary(remove, attacker, target).iterator
        prepared <- primary(prepare, attacker, target).iterator
        reading <- readings.iterator.filter { reading =>
          val path = reading.path
          reading.complete && path.moves.size >= 3 && path.moves == path.sourceMoves &&
            path.ids == path.declared && path.automaticTerminal.isEmpty &&
            path.certified.flatMap(_._2.material).exists(_.events.forall(_.promotionGainCp == 0)) &&
            path.positions.headOption == prepare.path.positions.headOption &&
            path.ids.headOption == prepare.path.ids.headOption &&
            path.moves.headOption == prepare.path.moves.headOption &&
            paths.paths.exists(_ eq path) && reading.events.exists(e =>
              e.plyOffset == 1 && e.originalActor.contains(attacker) && e.originalVictim.contains(target))
        }.toList.sortBy(r => (if r.path eq prepare.path then 0 else 1,
          r.path.moves.size, r.path.line.ref.id)).iterator
        witness = new Arm(packet, paths, reading.path, positions, reading)
        if witness.root.contains(provider)
        use <- witness.supportUse(target).iterator.filter(use =>
          use.originalCapturer == attacker && use.originalTarget == target &&
            use.targetCapture.plyOffset == 1 && use.recapture.plyOffset == 2 &&
            witness.mover(1).contains(attacker) &&
            paths.pieceAfter(packet, witness.path, attacker, 3).contains(None))
        cost = witness.account(use.targetCapture, attacker)
        // ponytail: one immediate, balanced recapture; wider accounts stay
        // with their existing readers instead of being shortened to this motif.
        if cost.complete && cost.dutyCosts.isEmpty &&
          cost.events.toSet == Set(use.targetCapture, use.recapture) &&
          cost.offsets.forall(Set(0, 1, 2)) && witness.balanced(cost.events) &&
          witness.captures.filter(_.plyOffset <= 2).toSet == cost.events.toSet
        responseCost <- material(witness, cost.events, (0 :: cost.offsets).distinct.sorted).iterator
      yield
        val statements = List(
          AttackerRemovalOrSupport(remove.ref(0), prepare.ref(0), attacker, target, provider),
          direct, prepared, SupportRecapture(prepare.ref(0), witness.path, use)) ++
          (if prepare.path eq witness.path then Nil else List(responseCost))
        Answer("attacker_removal_or_recapture", statements.map(statement =>
          Section(List(statement), statement.references)), Nil,
          (arms :+ witness).flatMap(_.proofs.toList.sorted) ++ use.support.sourceIds ++
            List(use.targetCapture.sourceId, use.recapture.sourceId))
    }.nextOption()
