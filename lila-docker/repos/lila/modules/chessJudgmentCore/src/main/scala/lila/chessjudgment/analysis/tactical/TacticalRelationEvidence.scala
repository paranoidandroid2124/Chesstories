package lila.chessjudgment.analysis.tactical

import _root_.chess.{ Bishop, Bitboard, Board, Color, King, Knight, Move, Pawn, Position, Queen, Role, Rook, Square }
import _root_.chess.format.{ Fen, Uci }
import _root_.chess.variant.Standard

import lila.chessjudgment.analysis.line.PrincipalVariationEvidence
import lila.chessjudgment.analysis.evaluation.JudgmentThresholds
import lila.chessjudgment.analysis.material.MaterialValue
import lila.chessjudgment.analysis.tactical.TacticalPatternDetectors
import lila.chessjudgment.analysis.structure.WeaknessTargetProfile
import lila.chessjudgment.model.judgment.{ EvidenceSquare, LineReplayStep }
import lila.chessjudgment.model.strategic.VariationLine

private[chessjudgment] object TacticalRelationEvidence:

  def normalizedTopUciMoves(variations: List[VariationLine]): List[String] =
    variations.headOption.toList.flatMap(normalizedLineMoves)

  private[chessjudgment] val DrawResourceRelationReplayMaxPlies = 12

  def normalizedLineMoves(line: VariationLine): List[String] =
    val rawUciMoves = normalizeAllUci(line.moves)
    if rawUciMoves.exists(_.nonEmpty) then rawUciMoves.get
    else
      normalizeAllUci(line.parsedMoves.flatMap(move => clean(move.uci))).getOrElse(Nil)

  def boundedTopReplay(
      fen: String,
      variations: List[VariationLine],
      maxPlies: Int
  ): Option[List[BoundedReplayStep]] =
    boundedReplay(fen, normalizedTopUciMoves(variations), maxPlies)

  def boundedTopReplayPrefix(
      fen: String,
      variations: List[VariationLine],
      minPlies: Int,
      maxPlies: Int
  ): Option[List[BoundedReplayStep]] =
    boundedReplayPrefix(fen, normalizedTopUciMoves(variations), minPlies, maxPlies)

  def boundedReplay(
      fen: String,
      moves: List[String],
      maxPlies: Int
  ): Option[List[BoundedReplayStep]] =
    val normalizedMoves = normalizedBoundedMoves(moves, maxPlies)
    replayLegalPrefix(fen, normalizedMoves).filter(_.size == normalizedMoves.size)

  def boundedReplayFromSteps(
      steps: List[LineReplayStep],
      maxPlies: Int
  ): Option[List[BoundedReplayStep]] =
    val bounded = steps.take(maxPlies)
    Option.when(bounded.nonEmpty)(()).flatMap { _ =>
      val accepted = scala.collection.mutable.ListBuffer.empty[BoundedReplayStep]
      var ok = true
      val it = bounded.iterator
      while it.hasNext && ok do
        val step = it.next()
        Fen.read(Standard, Fen.Full(step.fenBefore)).flatMap { before =>
          val normalized = PrincipalVariationEvidence.normalizeUci(step.moveUci)
          legalMove(before, normalized).filter { move =>
            boardStateFen(Fen.write(move.after).value) == boardStateFen(step.fenAfter)
          }.map { move =>
            val capturedRole =
              move.capture
                .flatMap(before.board.roleAt)
                .orElse(before.board.roleAt(move.dest))
            BoundedReplayStep(
              uci = normalized,
              before = before,
              move = move,
              after = move.after,
              capturedRole = capturedRole
            )
          }
        } match
          case Some(replayed) =>
            accepted += replayed
          case None =>
            ok = false
      Option.when(ok)(accepted.toList)
    }

  def boundedReplayPrefix(
      fen: String,
      moves: List[String],
      minPlies: Int,
      maxPlies: Int
  ): Option[List[BoundedReplayStep]] =
    val normalizedMoves = normalizedBoundedMoves(moves, maxPlies)
    Option
      .when(minPlies > 0 && maxPlies >= minPlies && normalizedMoves.size >= minPlies)(normalizedMoves)
      .flatMap(replayLegalPrefix(fen, _))
      .filter(_.size >= minPlies)

  private def replayLegalPrefix(
      fen: String,
      normalizedMoves: List[String]
  ): Option[List[BoundedReplayStep]] =
    Option.when(normalizedMoves.nonEmpty)(()).flatMap { _ =>
      val startOpt = Fen.read(Standard, Fen.Full(fen))

      startOpt.flatMap { start =>
        val accepted = scala.collection.mutable.ListBuffer.empty[BoundedReplayStep]
        var current = start
        var ok = true
        val it = normalizedMoves.iterator
        while it.hasNext && ok do
          val uci = it.next()
          legalMove(current, uci) match
            case Some(move) =>
              val capturedRole =
                move.capture
                  .flatMap(current.board.roleAt)
                  .orElse(current.board.roleAt(move.dest))
              accepted += BoundedReplayStep(
                uci = uci,
                before = current,
                move = move,
                after = move.after,
                capturedRole = capturedRole
              )
              current = move.after
            case None =>
              ok = false
        Option.when(ok)(accepted.toList)
      }
    }

  private def normalizedBoundedMoves(moves: List[String], maxPlies: Int): List[String] =
    normalizeAllUci(moves.take(maxPlies)).getOrElse(Nil)

  private def normalizeAllUci(moves: List[String]): Option[List[String]] =
    val normalized = moves.map(PrincipalVariationEvidence.normalizeUci)
    Option.when(normalized.forall(isUciMove))(normalized)

  def defenderTradeBranch(
      replay: List[BoundedReplayStep],
      playedMove: String,
      targetHints: List[EvidenceSquare] = Nil
  ): Option[DefenderTradeBranch] =
    val normalizedPlayed = PrincipalVariationEvidence.normalizeUci(playedMove)
    for
      first <- replay.headOption.filter(_.uci == normalizedPlayed)
      defenderStep <- replay.lift(1)
      recaptureStep <- replay.lift(2)
      exchangeSquare = first.move.dest
      defenderSquare = defenderStep.move.orig
      if defenderStep.move.dest == exchangeSquare
      if recaptureStep.move.dest == exchangeSquare
      if defenderStep.move.captures && recaptureStep.move.captures
      defender <- defenderStep.before.board.pieceAt(defenderSquare)
      movingSide = first.move.piece.color
      if defender.color != movingSide
      target <- defenderTradeTargetSquare(
        board = first.before.board,
        movingSide = movingSide,
        defenderSquare = defenderSquare,
        exchangeSquare = exchangeSquare,
        targetHints = targetHints
      )
      if defenseRelationRemoved(
        before = first.before.board,
        after = recaptureStep.after.board,
        defenderColor = defender.color,
        defenderSquare = defenderSquare,
        targetSquare = target
      )
    yield DefenderTradeBranch(
      defenderSquare = defenderSquare.key,
      exchangeSquare = exchangeSquare.key,
      targetSquare = target,
      lineMoves = replayUcis(replay, 0, 3)
    )

  def relationWitnesses(
      replay: List[BoundedReplayStep],
      playedMove: String,
      targetHints: List[EvidenceSquare] = Nil,
      continuationLines: List[List[String]] = Nil,
      engineScoreCp: Option[Int] = None,
      engineMate: Option[Int] = None,
      drawishWinPercent: Option[Double] = None,
      includeDrawResources: Boolean = true
  ): List[RelationWitness] =
    val zwischenzug = zwischenzugWitness(replay, playedMove, targetHints)
    val drawResourceWitnesses =
      if includeDrawResources then
        List(
          stalemateTrapWitness(replay, playedMove, engineScoreCp, engineMate, drawishWinPercent),
          perpetualCheckWitness(replay, playedMove, engineScoreCp, engineMate, drawishWinPercent)
        ).flatten
      else Nil
    val fork = forkWitness(replay, playedMove, targetHints)
    val loosePiecePressure =
      val trapped = trappedPieceWitness(replay, playedMove, targetHints)
      trapped
        .orElse(dominationWitness(replay, playedMove, targetHints))
        .orElse(hangingPieceWitness(replay, playedMove, targetHints))
    val witnesses = List(
      defenderTradeBranch(replay, playedMove, targetHints).map(defenderTradeWitness),
      badPieceLiquidationBranch(replay, playedMove).map(badPieceLiquidationWitness),
      overloadWitness(replay, playedMove, targetHints),
      deflectionWitness(replay, playedMove, targetHints),
      discoveredAttackWitness(replay, playedMove, targetHints),
      doubleCheckWitness(replay, playedMove),
      backRankMateWitness(replay, playedMove),
      mateNetWitness(replay, playedMove),
      greekGiftWitness(replay, playedMove, continuationLines)
    ).flatten ++ drawResourceWitnesses ++ List(
      zwischenzug,
      fork,
      loosePiecePressure,
      pinWitness(replay, playedMove, targetHints),
      xrayWitness(replay, playedMove, targetHints),
      clearanceWitness(replay, playedMove, targetHints),
      batteryWitness(replay, playedMove, targetHints),
      skewerWitness(replay, playedMove, targetHints),
      interferenceWitness(replay, playedMove, targetHints),
      decoyWitness(replay, playedMove, targetHints)
    ).flatten
    prioritizeTargetHintMatches(witnesses, targetHints)

  def defenderTradeRelationWitness(
      replay: List[BoundedReplayStep],
      playedMove: String,
      targetHints: List[EvidenceSquare] = Nil
  ): Option[RelationWitness] =
    defenderTradeBranch(replay, playedMove, targetHints).map(defenderTradeWitness)

  def badPieceLiquidationRelationWitness(
      replay: List[BoundedReplayStep],
      playedMove: String
  ): Option[RelationWitness] =
    badPieceLiquidationBranch(replay, playedMove).map(badPieceLiquidationWitness)

  def typedDetailsFromWitness(witness: RelationWitness): Option[RelationDetails] =
    witness.details match
      case RelationDetails.Empty => None
      case details: RelationDetails.DefenderTrade if witness.kind == RelationKind.DefenderTrade => Some(details)
      case details: RelationDetails.BadPieceLiquidation if witness.kind == RelationKind.BadPieceLiquidation => Some(details)
      case details: RelationDetails.Overload if witness.kind == RelationKind.Overload => Some(details)
      case details: RelationDetails.Deflection if witness.kind == RelationKind.Deflection => Some(details)
      case details: RelationDetails.DiscoveredAttack if witness.kind == RelationKind.DiscoveredAttack => Some(details)
      case details: RelationDetails.DoubleCheck if witness.kind == RelationKind.DoubleCheck => Some(details)
      case details: RelationDetails.MatePattern
          if (witness.kind == RelationKind.BackRankMate || witness.kind == RelationKind.MateNet) &&
            details.relationKind == witness.kind =>
        Some(details)
      case details: RelationDetails.GreekGift if witness.kind == RelationKind.GreekGift => Some(details)
      case details: RelationDetails.Fork if witness.kind == RelationKind.Fork => Some(details)
      case details: RelationDetails.HangingPiece if witness.kind == RelationKind.HangingPiece => Some(details)
      case details: RelationDetails.TrappedPiece if witness.kind == RelationKind.TrappedPiece => Some(details)
      case details: RelationDetails.Domination if witness.kind == RelationKind.Domination => Some(details)
      case details: RelationDetails.Zwischenzug if witness.kind == RelationKind.Zwischenzug => Some(details)
      case details: RelationDetails.Decoy if witness.kind == RelationKind.Decoy => Some(details)
      case details: RelationDetails.XRay if witness.kind == RelationKind.XRay => Some(details)
      case details: RelationDetails.Clearance if witness.kind == RelationKind.Clearance => Some(details)
      case details: RelationDetails.Battery if witness.kind == RelationKind.Battery => Some(details)
      case details: RelationDetails.Interference if witness.kind == RelationKind.Interference => Some(details)
      case details: RelationDetails.Pin if witness.kind == RelationKind.Pin => Some(details)
      case details: RelationDetails.Skewer if witness.kind == RelationKind.Skewer => Some(details)
      case details: RelationDetails.StalemateTrap if witness.kind == RelationKind.StalemateTrap => Some(details)
      case details: RelationDetails.PerpetualCheck if witness.kind == RelationKind.PerpetualCheck => Some(details)
      case _ => None

  def defenderTradeBranchFromWitness(witness: RelationWitness): Option[DefenderTradeBranch] =
    typedDetailsFromWitness(witness).collect {
      case details: RelationDetails.DefenderTrade =>
        DefenderTradeBranch(
          defenderSquare = details.defenderSquare,
          exchangeSquare = details.exchangeSquare,
          targetSquare = details.targetSquare,
          lineMoves = witness.lineMoves
        )
    }

  def badPieceLiquidationBranchFromWitness(witness: RelationWitness): Option[BadPieceLiquidationBranch] =
    typedDetailsFromWitness(witness).collect {
      case details: RelationDetails.BadPieceLiquidation =>
        BadPieceLiquidationBranch(
          badPieceSquare = details.badPieceSquare,
          exchangeSquare = details.exchangeSquare,
          lineMoves = witness.lineMoves
        )
    }

  def defenderTradeWitness(branch: DefenderTradeBranch): RelationWitness =
    RelationWitness(
      kind = RelationKind.DefenderTrade,
      focusSquares = List(branch.targetSquare, branch.exchangeSquare),
      lineMoves = branch.lineMoves,
      targetSquare = Some(branch.targetSquare),
      details = RelationDetails.DefenderTrade(
        defenderSquare = branch.defenderSquare,
        exchangeSquare = branch.exchangeSquare,
        targetSquare = branch.targetSquare
      )
    )

  def badPieceLiquidationWitness(branch: BadPieceLiquidationBranch): RelationWitness =
    RelationWitness(
      kind = RelationKind.BadPieceLiquidation,
      focusSquares = List(branch.badPieceSquare, branch.exchangeSquare),
      lineMoves = branch.lineMoves,
      targetSquare = Some(branch.exchangeSquare),
      details = RelationDetails.BadPieceLiquidation(
        badPieceSquare = branch.badPieceSquare,
        exchangeSquare = branch.exchangeSquare
      )
    )

  def overloadWitness(
      replay: List[BoundedReplayStep],
      playedMove: String,
      targetHints: List[EvidenceSquare] = Nil
  ): Option[RelationWitness] =
    val normalizedPlayed = PrincipalVariationEvidence.normalizeUci(playedMove)
    for
      first <- replay.headOption.filter(_.uci == normalizedPlayed)
      movingSide = first.move.piece.color
      targetSquares = relationTargetSquares(first.after.board, movingSide, targetHints)
      witness <- overloadedDefender(first.before.board, first.after.board, movingSide, first.move.dest, targetSquares)
    yield witness.copy(lineMoves = relationPayoffUcis(replay, witnessTargetKeys(witness)))

  def deflectionWitness(
      replay: List[BoundedReplayStep],
      playedMove: String,
      targetHints: List[EvidenceSquare] = Nil
  ): Option[RelationWitness] =
    val normalizedPlayed = PrincipalVariationEvidence.normalizeUci(playedMove)
    for
      first <- replay.headOption.filter(_.uci == normalizedPlayed)
      reply <- replay.lift(1)
      movingSide = first.move.piece.color
      defenderSquare = reply.move.orig
      defender <- first.before.board.pieceAt(defenderSquare)
      if defender.color != movingSide
      if first.after.board.attackers(defenderSquare, movingSide).exists(_ == first.move.dest)
      if reply.move.piece.color == defender.color
      target <- relationTargetSquares(first.before.board, movingSide, targetHints)
        .find(target =>
          first.before.board.attackers(target, defender.color).exists(_ == defenderSquare) &&
            !reply.after.board.attackers(target, defender.color).exists(_ == defenderSquare) &&
            reply.after.board.attackers(target, defender.color).count < first.before.board.attackers(target, defender.color).count
        )
      payoffLine = relationPayoffUcis(replay, List(target.key, defenderSquare.key))
    yield
      RelationWitness(
        kind = RelationKind.Deflection,
        focusSquares = List(target.key, defenderSquare.key, first.move.dest.key),
        lineMoves = if payoffLine.size > 1 then payoffLine else replayUcis(replay, 0, 2),
        targetSquare = Some(target.key),
        details = RelationDetails.Deflection(
          defenderSquare = defenderSquare.key,
          targetSquare = target.key,
          attackerSquare = first.move.dest.key
        )
      )

  def discoveredAttackWitness(
      replay: List[BoundedReplayStep],
      playedMove: String,
      targetHints: List[EvidenceSquare] = Nil
  ): Option[RelationWitness] =
    val normalizedPlayed = PrincipalVariationEvidence.normalizeUci(playedMove)
    for
      first <- replay.headOption.filter(_.uci == normalizedPlayed)
      movingSide = first.move.piece.color
      targetSet = relationTargetSquares(first.after.board, movingSide, targetHints).toSet
      witness <- discoveredAttackAfterMove(first.before.board, first.after.board, movingSide, first.move.orig, first.move.dest, targetSet)
    yield witness.copy(lineMoves = replayUcis(replay, 0, 1))

  def xrayWitness(
      replay: List[BoundedReplayStep],
      playedMove: String,
      targetHints: List[EvidenceSquare] = Nil
  ): Option[RelationWitness] =
    val normalizedPlayed = PrincipalVariationEvidence.normalizeUci(playedMove)
    for
      first <- replay.headOption.filter(_.uci == normalizedPlayed)
      movingSide = first.move.piece.color
      targetSet = relationTargetSquares(first.after.board, movingSide, targetHints).toSet
      witness <- xrayAfterMove(first.after.board, movingSide, first.move.dest, first.move.piece.role, targetSet)
    yield witness.copy(lineMoves = replayUcis(replay, 0, 1))

  def doubleCheckWitness(
      replay: List[BoundedReplayStep],
      playedMove: String
  ): Option[RelationWitness] =
    val normalizedPlayed = PrincipalVariationEvidence.normalizeUci(playedMove)
    for
      first <- replay.headOption.filter(_.uci == normalizedPlayed)
      movingSide = first.move.piece.color
      king <- first.after.board.kingPosOf(!movingSide)
      checkers = first.after.checkers.squares.toList.map(_.key).sorted
      if first.after.check.yes && checkers.size >= 2
    yield
      RelationWitness(
        kind = RelationKind.DoubleCheck,
        focusSquares = (king.key :: checkers).distinct,
        lineMoves = replayUcis(replay, 0, 1),
        targetSquare = Some(king.key),
        details = RelationDetails.DoubleCheck(
          kingSquare = king.key,
          checkerSquares = checkers,
          moverSquare = first.move.dest.key,
          moverRole = first.move.piece.role.name
        )
      )

  def backRankMateWitness(
      replay: List[BoundedReplayStep],
      playedMove: String
  ): Option[RelationWitness] =
    val normalizedPlayed = PrincipalVariationEvidence.normalizeUci(playedMove)
    for
      first <- replay.headOption.filter(_.uci == normalizedPlayed)
      pattern <- tacticalPattern(RelationKind.BackRankMate)
      if first.after.checkMate
      if pattern.matches(Some(first.before), first.after, first.uci)
      king <- first.after.board.kingPosOf(first.after.color)
      checkers = first.after.checkers.squares.toList.map(_.key).sorted
    yield
      RelationWitness(
        kind = RelationKind.BackRankMate,
        focusSquares = (king.key :: checkers).distinct,
        lineMoves = replayUcis(replay, 0, 1),
        targetSquare = Some(king.key),
        details = RelationDetails.MatePattern(
          relationKind = RelationKind.BackRankMate,
          kingSquare = king.key,
          checkerSquares = checkers,
          matingMove = first.uci,
          patternId = Some(pattern.id)
        )
      )

  def mateNetWitness(
      replay: List[BoundedReplayStep],
      playedMove: String
  ): Option[RelationWitness] =
    val normalizedPlayed = PrincipalVariationEvidence.normalizeUci(playedMove)
    for
      first <- replay.headOption.filter(_.uci == normalizedPlayed)
      if first.after.checkMate
      pattern <- matePatternExcept(RelationKind.BackRankMate, first)
      king <- first.after.board.kingPosOf(first.after.color)
      checkers = first.after.checkers.squares.toList.map(_.key).sorted
    yield
      RelationWitness(
        kind = RelationKind.MateNet,
        focusSquares = (king.key :: checkers).distinct,
        lineMoves = replayUcis(replay, 0, 1),
        targetSquare = Some(king.key),
        details = RelationDetails.MatePattern(
          relationKind = RelationKind.MateNet,
          kingSquare = king.key,
          checkerSquares = checkers,
          matingMove = first.uci,
          patternId = Some(pattern.id)
        )
      )

  def greekGiftWitness(
      replay: List[BoundedReplayStep],
      playedMove: String,
      continuationLines: List[List[String]] = Nil
  ): Option[RelationWitness] =
    val normalizedPlayed = PrincipalVariationEvidence.normalizeUci(playedMove)
    for
      first <- replay.headOption.filter(_.uci == normalizedPlayed)
      pattern <- tacticalPattern(RelationKind.GreekGift)
      movingSide = first.move.piece.color
      target <- squareFromKey(if movingSide.white then "h7" else "h2")
      if pattern.matchesWithContinuations(Some(first.before), first.after, first.uci, continuationLines)
    yield
      RelationWitness(
        kind = RelationKind.GreekGift,
        focusSquares = List(first.move.dest.key, target.key).distinct,
        lineMoves = replayUcis(replay, 0, 1),
        targetSquare = Some(target.key),
        details = RelationDetails.GreekGift(
          bishopSquare = first.move.dest.key,
          targetSquare = target.key,
          entryMove = first.uci,
          patternId = pattern.id
        )
      )

  def zwischenzugWitness(
      replay: List[BoundedReplayStep],
      playedMove: String,
      targetHints: List[EvidenceSquare] = Nil
  ): Option[RelationWitness] =
    val normalizedPlayed = PrincipalVariationEvidence.normalizeUci(playedMove)
    for
      first <- replay.headOption.filter(_.uci == normalizedPlayed)
      movingSide = first.move.piece.color
      targetHintSquares = typedTargetHintSquares(targetHints)
      checkers = first.after.checkers.squares.toList
      if first.after.check.yes && checkers.exists(_ == first.move.dest)
      recapture <- legalRecaptureCandidates(first.before, movingSide, targetHintSquares)
        .flatMap { target =>
          first.before.legalMoves.toList.flatMap { candidate =>
            val capturedRole =
              candidate.capture
                .flatMap(first.before.board.roleAt)
                .orElse(first.before.board.roleAt(candidate.dest))
            capturedRole
              .filter(role => candidate.piece.color == movingSide && candidate.dest == target && candidate.captures && role != Pawn)
              .map(role => target -> role)
          }
        }
        .sortBy { case (target, role) =>
          (targetHintPenalty(targetHintSquares.toSet, target), -MaterialValue.tacticalValueCp(role), target.key)
        }
        .headOption
      (expectedRecaptureSquare, _) = recapture
      if first.move.dest != expectedRecaptureSquare
      king <- first.after.board.kingPosOf(first.after.color)
      threatType = if first.after.checkMate then RelationThreatType.MateCheck else RelationThreatType.Check
      lineMoves = replayUcis(replay, 0, 1)
    yield
      RelationWitness(
        kind = RelationKind.Zwischenzug,
        focusSquares = List(first.move.dest.key, expectedRecaptureSquare.key, king.key),
        lineMoves = lineMoves,
        targetSquare = Some(expectedRecaptureSquare.key),
        details = RelationDetails.Zwischenzug(
          intermediateMove = first.uci,
          expectedRecaptureSquare = expectedRecaptureSquare.key,
          checkingPieceSquare = first.move.dest.key,
          checkingPieceRole = first.move.piece.role.name,
          checkedKingSquare = king.key,
          threatType = threatType
        )
      )

  def forkWitness(
      replay: List[BoundedReplayStep],
      playedMove: String,
      targetHints: List[EvidenceSquare] = Nil
  ): Option[RelationWitness] =
    val normalizedPlayed = PrincipalVariationEvidence.normalizeUci(playedMove)
    for
      first <- replay.headOption.filter(_.uci == normalizedPlayed)
      movingSide = first.move.piece.color
      targetHintSet = typedTargetHintSquares(targetHints).toSet
      targetSet = relationTargetSquares(first.after.board, movingSide, targetHints).toSet
      witness <- forkAfterMove(first.after.board, movingSide, first.move.dest, first.move.piece.role, targetSet, targetHintSet)
    yield witness.copy(lineMoves = replayUcis(replay, 0, 1))

  def hangingPieceWitness(
      replay: List[BoundedReplayStep],
      playedMove: String,
      targetHints: List[EvidenceSquare] = Nil
  ): Option[RelationWitness] =
    val normalizedPlayed = PrincipalVariationEvidence.normalizeUci(playedMove)
    for
      first <- replay.headOption.filter(_.uci == normalizedPlayed)
      movingSide = first.move.piece.color
      targetHintSet = typedTargetHintSquares(targetHints).toSet
      targetSet = relationTargetSquares(first.after.board, movingSide, targetHints).toSet
      witness <- hangingPieceAfterMove(
        board = first.after.board,
        movingSide = movingSide,
        attacker = first.move.dest,
        attackerRole = first.move.piece.role,
        targetSet = targetSet,
        targetHintSet = targetHintSet
      )
    yield witness.copy(lineMoves = replayUcis(replay, 0, 1))

  def trappedPieceWitness(
      replay: List[BoundedReplayStep],
      playedMove: String,
      targetHints: List[EvidenceSquare] = Nil
  ): Option[RelationWitness] =
    val normalizedPlayed = PrincipalVariationEvidence.normalizeUci(playedMove)
    for
      first <- replay.headOption.filter(_.uci == normalizedPlayed)
      movingSide = first.move.piece.color
      targetHintSet = typedTargetHintSquares(targetHints).toSet
      targetSet = relationTargetSquares(first.after.board, movingSide, targetHints).toSet
      witness <- trappedPieceAfterMove(
        position = first.after,
        movingSide = movingSide,
        attacker = first.move.dest,
        attackerRole = first.move.piece.role,
        targetSet = targetSet,
        targetHintSet = targetHintSet
      )
    yield witness.copy(lineMoves = replayUcis(replay, 0, 1))

  def dominationWitness(
      replay: List[BoundedReplayStep],
      playedMove: String,
      targetHints: List[EvidenceSquare] = Nil
  ): Option[RelationWitness] =
    val normalizedPlayed = PrincipalVariationEvidence.normalizeUci(playedMove)
    for
      first <- replay.headOption.filter(_.uci == normalizedPlayed)
      movingSide = first.move.piece.color
      targetHintSet = typedTargetHintSquares(targetHints).toSet
      targetSet = relationTargetSquares(first.after.board, movingSide, targetHints).toSet
      witness <- dominationAfterMove(
        position = first.after,
        movingSide = movingSide,
        attacker = first.move.dest,
        attackerRole = first.move.piece.role,
        targetSet = targetSet,
        targetHintSet = targetHintSet
      )
    yield witness.copy(lineMoves = replayUcis(replay, 0, 1))

  def clearanceWitness(
      replay: List[BoundedReplayStep],
      playedMove: String,
      targetHints: List[EvidenceSquare] = Nil
  ): Option[RelationWitness] =
    val normalizedPlayed = PrincipalVariationEvidence.normalizeUci(playedMove)
    for
      first <- replay.headOption.filter(_.uci == normalizedPlayed)
      movingSide = first.move.piece.color
      targetSet = relationTargetSquares(first.after.board, movingSide, targetHints).toSet
      witness <- clearanceAfterMove(first.before.board, first.after.board, movingSide, first.move.orig, first.move.dest, targetSet)
    yield witness.copy(lineMoves = relationPayoffUcis(replay, witness.targetSquare.toList))

  def batteryWitness(
      replay: List[BoundedReplayStep],
      playedMove: String,
      targetHints: List[EvidenceSquare] = Nil
  ): Option[RelationWitness] =
    val normalizedPlayed = PrincipalVariationEvidence.normalizeUci(playedMove)
    for
      first <- replay.headOption.filter(_.uci == normalizedPlayed)
      movingSide = first.move.piece.color
      targetHintSet = typedTargetHintSquares(targetHints).toSet
      targetSet = relationTargetSquares(first.after.board, movingSide, targetHints).toSet
      witness <- batteryAfterMove(
        board = first.after.board,
        movingSide = movingSide,
        movedTo = first.move.dest,
        movedRole = first.move.piece.role,
        targetSet = targetSet,
        targetHintSet = targetHintSet
      )
    yield witness.copy(lineMoves = replayUcis(replay, 0, 1))

  def pinWitness(
      replay: List[BoundedReplayStep],
      playedMove: String,
      targetHints: List[EvidenceSquare] = Nil
  ): Option[RelationWitness] =
    val normalizedPlayed = PrincipalVariationEvidence.normalizeUci(playedMove)
    for
      first <- replay.headOption.filter(_.uci == normalizedPlayed)
      movingSide = first.move.piece.color
      targetSet = relationTargetSquares(first.after.board, movingSide, targetHints).toSet
      witness <- pinAfterMove(first.after.board, movingSide, first.move.dest, first.move.piece.role, targetSet)
        .orElse(
          attackedPinnedPieceAfterMove(
            before = first.before.board,
            after = first.after.board,
            movingSide = movingSide,
            movedFrom = first.move.orig,
            movedTo = first.move.dest,
            movedRole = first.move.piece.role,
            targetSet = targetSet
          )
        )
    yield witness.copy(lineMoves = relationPayoffUcis(replay, witnessTargetKeys(witness)))

  def skewerWitness(
      replay: List[BoundedReplayStep],
      playedMove: String,
      targetHints: List[EvidenceSquare] = Nil
  ): Option[RelationWitness] =
    val normalizedPlayed = PrincipalVariationEvidence.normalizeUci(playedMove)
    for
      first <- replay.headOption.filter(_.uci == normalizedPlayed)
      movingSide = first.move.piece.color
      targetSet = relationTargetSquares(first.after.board, movingSide, targetHints).toSet
      witness <- skewerAfterMove(first.after.board, movingSide, first.move.dest, first.move.piece.role, targetSet)
    yield witness.copy(lineMoves = replayUcis(replay, 0, 1))

  def interferenceWitness(
      replay: List[BoundedReplayStep],
      playedMove: String,
      targetHints: List[EvidenceSquare] = Nil
  ): Option[RelationWitness] =
    val normalizedPlayed = PrincipalVariationEvidence.normalizeUci(playedMove)
    for
      first <- replay.headOption.filter(_.uci == normalizedPlayed)
      movingSide = first.move.piece.color
      targetSet = relationTargetSquares(first.after.board, movingSide, targetHints).toSet
      witness <- interferenceAfterMove(
        before = first.before.board,
        after = first.after.board,
        movingSide = movingSide,
        blocker = first.move.dest,
        blockerRole = first.move.piece.role,
        targetSet = targetSet
      )
    yield witness.copy(lineMoves = relationPayoffUcis(replay, witness.targetSquare.toList))

  def decoyWitness(
      replay: List[BoundedReplayStep],
      playedMove: String,
      targetHints: List[EvidenceSquare] = Nil
  ): Option[RelationWitness] =
    val normalizedPlayed = PrincipalVariationEvidence.normalizeUci(playedMove)
    for
      first <- replay.headOption.filter(_.uci == normalizedPlayed)
      reply <- replay.lift(1)
      response <- replay.lift(2)
      movingSide = first.move.piece.color
      baitSquare = first.move.dest
      if reply.move.piece.color != movingSide
      if response.move.piece.color == movingSide
      if reply.move.dest == baitSquare && reply.move.captures
      if reply.capturedRole.contains(first.move.piece.role)
      if response.move.dest == baitSquare && response.move.captures
      if response.capturedRole.contains(reply.move.piece.role)
      if MaterialValue.tacticalValueCp(reply.move.piece.role) > MaterialValue.tacticalValueCp(first.move.piece.role)
      targetSet = relationTargetSquares(reply.after.board, movingSide, targetHints).toSet
      if targetSet.contains(baitSquare) || targetSet.contains(reply.move.orig)
    yield
      RelationWitness(
        kind = RelationKind.Decoy,
        focusSquares = List(first.move.orig.key, baitSquare.key, reply.move.orig.key),
        lineMoves = replayUcis(replay, 0, 3),
        targetSquare = Some(baitSquare.key),
        details = RelationDetails.Decoy(
          baitFromSquare = first.move.orig.key,
          baitSquare = baitSquare.key,
          luredFromSquare = reply.move.orig.key,
          executionFromSquare = response.move.orig.key,
          executionToSquare = response.move.dest.key,
          baitRole = first.move.piece.role.name,
          luredRole = reply.move.piece.role.name
        )
      )

  def stalemateTrapWitness(
      replay: List[BoundedReplayStep],
      playedMove: String,
      engineScoreCp: Option[Int],
      engineMate: Option[Int],
      drawishWinPercent: Option[Double]
  ): Option[RelationWitness] =
    val normalizedPlayed = PrincipalVariationEvidence.normalizeUci(playedMove)
    for
      first <- replay.headOption.filter(_.uci == normalizedPlayed)
      if drawResourceScoreStable(drawishWinPercent, engineMate)
      terminal <- replay.lastOption
      if terminal.after.staleMate
      king <- terminal.after.board.kingPosOf(terminal.after.color)
    yield
      val lineMoves = replayUcis(replay, 0, replay.length)
      RelationWitness(
        kind = RelationKind.StalemateTrap,
        focusSquares = List(king.key, terminal.move.dest.key),
        lineMoves = lineMoves,
        targetSquare = Some(king.key),
        details = RelationDetails.StalemateTrap(
          stalematedKingSquare = king.key,
          resourceSquare = terminal.move.dest.key,
          entryMove = first.uci,
          terminalMove = terminal.uci,
          scoreCp = engineScoreCp
        )
      )

  def perpetualCheckWitness(
      replay: List[BoundedReplayStep],
      playedMove: String,
      engineScoreCp: Option[Int],
      engineMate: Option[Int],
      drawishWinPercent: Option[Double]
  ): Option[RelationWitness] =
    val normalizedPlayed = PrincipalVariationEvidence.normalizeUci(playedMove)
    for
      first <- replay.headOption.filter(step => step.uci == normalizedPlayed && step.after.check.yes)
      if drawResourceScoreStable(drawishWinPercent, engineMate)
      cycle <- repeatedCheckingCycle(replay, first.move.piece.color)
    yield
      val lineMoves = replayUcis(replay, 0, replay.length)
      RelationWitness(
        kind = RelationKind.PerpetualCheck,
        focusSquares = (cycle.kingSquare :: cycle.checkerSquares).distinct,
        lineMoves = lineMoves,
        targetSquare = Some(cycle.kingSquare),
        details = RelationDetails.PerpetualCheck(
          checkedKingSquare = cycle.kingSquare,
          checkerSquares = cycle.checkerSquares,
          checkingSide = cycle.checkingSide,
          entryMove = first.uci,
          cycleStartMove = cycle.startMove,
          cycleReturnMove = cycle.returnMove,
          repeatedPositionKey = cycle.positionKey,
          scoreCp = engineScoreCp
        )
      )

  private def drawResourceScoreStable(winPercent: Option[Double], mate: Option[Int]): Boolean =
    mate.isEmpty && winPercent.exists(wp => math.abs(wp - 50.0) <= JudgmentThresholds.DRAW_RESOURCE_BALANCE_EDGE_WP)

  private final case class RepeatedCheckingCycle(
      kingSquare: String,
      checkerSquares: List[String],
      checkingSide: String,
      startMove: String,
      returnMove: String,
      positionKey: String
  )

  private def repeatedCheckingCycle(
      replay: List[BoundedReplayStep],
      checkingSide: Color
  ): Option[RepeatedCheckingCycle] =
    val checkingPositions =
      replay.zipWithIndex.flatMap { case (step, index) =>
        Option.when(step.move.piece.color == checkingSide && step.after.check.yes)(
          for
            king <- step.after.board.kingPosOf(step.after.color)
            checkers = step.after.checkers.squares.toList.map(_.key).sorted
            if checkers.nonEmpty
          yield (index, step, repetitionPositionKey(step.after), king.key, checkers)
        ).flatten
      }
    Option.when(checkingPositions.size >= 3)(()).flatMap { _ =>
      checkingPositions
        .groupBy(_._3)
        .values
        .toList
        .flatMap { positions =>
          val sorted = positions.sortBy(_._1)
          for
            start <- sorted.headOption
            end <- sorted.drop(1).headOption
            if end._1 - start._1 >= 4
          yield RepeatedCheckingCycle(
            kingSquare = end._4,
            checkerSquares = end._5,
            checkingSide = if checkingSide.white then "white" else "black",
            startMove = start._2.uci,
            returnMove = end._2.uci,
            positionKey = end._3
          )
        }
        .sortBy(cycle => replay.indexWhere(_.uci == cycle.startMove))
        .headOption
    }

  private def repetitionPositionKey(position: Position): String =
    Fen.write(position).value.split("\\s+").take(4).mkString(" ")

  def badPieceLiquidationBranch(
      replay: List[BoundedReplayStep],
      playedMove: String
  ): Option[BadPieceLiquidationBranch] =
    val normalizedPlayed = PrincipalVariationEvidence.normalizeUci(playedMove)
    for
      first <- replay.headOption.filter(_.uci == normalizedPlayed)
      movingSide = first.move.piece.color
      badPieceSquare = first.move.orig
      firstDest = first.move.dest
      piece <- first.before.board.pieceAt(badPieceSquare)
      if piece.color == movingSide && piece.role == Bishop
      if isBadBishopOnCurrentBoard(first.before.board, movingSide, badPieceSquare)
      branch <- immediateBadPieceLiquidation(replay, movingSide, badPieceSquare, firstDest)
        .orElse(sameBranchBadPieceLiquidation(replay, movingSide, badPieceSquare, firstDest))
    yield branch

  def queenTradeShieldLine(replay: List[BoundedReplayStep]): Option[List[String]] =
    replay.zipWithIndex.collectFirst {
      case (step, index)
          if step.move.piece.color == replay.head.before.color &&
            step.move.piece.role == _root_.chess.Queen &&
            step.move.captures &&
            step.capturedRole.contains(_root_.chess.Queen) &&
            replay.lift(index + 1).exists(reply =>
              reply.move.piece.color != step.move.piece.color &&
                reply.move.piece.role == _root_.chess.King &&
                reply.move.dest == step.move.dest &&
                reply.move.captures &&
                reply.capturedRole.contains(_root_.chess.Queen)
            ) =>
        replayUcis(replay, 0, index + 2)
    }

  def immediateExchangeSquare(replay: List[BoundedReplayStep]): Option[String] =
    (replay.headOption, replay.lift(1)) match
      case (Some(first), Some(second))
          if first.move.captures &&
            second.move.captures &&
            first.move.dest == second.move.dest &&
            first.move.piece.color != second.move.piece.color =>
        Some(first.move.dest.key)
      case _ => None

  private def replayUcis(replay: List[BoundedReplayStep], fromPly: Int, maxPlies: Int): List[String] =
    replay.drop(fromPly).take(maxPlies).map(_.uci)

  private def relationPayoffUcis(
      replay: List[BoundedReplayStep],
      targetSquares: List[String],
      maxPlies: Int = 3
  ): List[String] =
    val movingSide = replay.headOption.map(_.move.piece.color)
    val targets = targetSquares.flatMap(squareFromKey).toSet
    replay
      .take(maxPlies)
      .zipWithIndex
      .collectFirst {
        case (step, index)
            if index > 0 &&
              movingSide.contains(step.move.piece.color) &&
              (step.after.check.yes || (step.move.captures && targets.contains(step.move.dest))) =>
          replayUcis(replay, 0, index + 1)
      }
      .getOrElse(Nil)

  def legalMove(position: Position, uci: String): Option[Move] =
    Uci(uci).collect { case move: Uci.Move => move }.flatMap(position.move(_).toOption)

  private def boardStateFen(fen: String): String =
    Option(fen).getOrElse("").trim.split("\\s+").take(4).mkString(" ")

  def isUciMove(move: String): Boolean =
    move.matches("""[a-h][1-8][a-h][1-8][qrbn]?""")

  def squareFromKey(key: String): Option[Square] =
    Square.fromKey(Option(key).map(_.trim.toLowerCase).getOrElse(""))

  def isConstrainedBadBishop(board: Board, color: Color, square: Square): Boolean =
    isBadBishopOnCurrentBoard(board, color, square)

  private def defenderTradeTargetSquare(
      board: Board,
      movingSide: Color,
      defenderSquare: Square,
      exchangeSquare: Square,
      targetHints: List[EvidenceSquare]
  ): Option[String] =
    val hintSquares = typedTargetHintSquares(targetHints)
    val structuralTargets =
      WeaknessTargetProfile.targetsForPressure(board, movingSide).flatMap(target => squareFromKey(target.targetSquare))
    val materialTargets =
      board.byColor(!movingSide).squares.filterNot(square => board.roleAt(square).contains(King)).toList
    val candidateTargets = (hintSquares ++ structuralTargets ++ materialTargets).distinct
    candidateTargets
      .filterNot(_ == exchangeSquare)
      .sortBy(target => (targetHintPenalty(hintSquares.toSet, target), target.key))
      .find(target => board.attackers(target, !movingSide).exists(_ == defenderSquare))
      .map(_.key)

  private def defenseRelationRemoved(
      before: Board,
      after: Board,
      defenderColor: Color,
      defenderSquare: Square,
      targetSquare: String
  ): Boolean =
    squareFromKey(targetSquare).exists { target =>
      val beforeDefenders = before.attackers(target, defenderColor)
      val afterDefenders = after.attackers(target, defenderColor)
      beforeDefenders.exists(_ == defenderSquare) &&
        !afterDefenders.exists(_ == defenderSquare) &&
        afterDefenders.count < beforeDefenders.count
    }

  private def relationTargetSquares(
      board: Board,
      movingSide: Color,
      targetHints: List[EvidenceSquare]
  ): List[Square] =
    val hints = typedTargetHintSquares(targetHints)
    val structural =
      WeaknessTargetProfile.targetsForPressure(board, movingSide).flatMap(target => squareFromKey(target.targetSquare))
    val material =
      board.byColor(!movingSide).squares.filterNot(square => board.roleAt(square).contains(King)).toList
    (hints ++ structural ++ material).distinct

  private def typedTargetHintSquares(targetHints: List[EvidenceSquare]): List[Square] =
    targetHints.flatMap(hint => squareFromKey(hint.key)).distinct

  private def typedTargetHintKeys(targetHints: List[EvidenceSquare]): Set[String] =
    targetHints.map(_.key.trim.toLowerCase).filter(_.nonEmpty).toSet

  private def targetHintPenalty(targetHints: Set[Square], target: Square): Int =
    if targetHints.contains(target) then 0 else 1

  private def prioritizeTargetHintMatches(
      witnesses: List[RelationWitness],
      targetHints: List[EvidenceSquare]
  ): List[RelationWitness] =
    val hintKeys = typedTargetHintKeys(targetHints)
    if hintKeys.isEmpty then witnesses
    else
      witnesses.sortBy { witness =>
        val matched = witnessTargetKeys(witness).exists(hintKeys.contains)
        (if matched then 0 else 1, witness.kind, witness.targetSquare.getOrElse(""), witness.focusSquares.mkString(","))
      }

  private def witnessTargetKeys(witness: RelationWitness): List[String] =
    (witness.targetSquare.toList ++ witness.focusSquares).map(_.trim.toLowerCase).filter(_.nonEmpty).distinct

  private def legalRecaptureCandidates(
      position: Position,
      movingSide: Color,
      targetHints: List[Square]
  ): List[Square] =
    val legalCaptureTargets =
      position.legalMoves.toList
        .filter(move => move.piece.color == movingSide && move.captures)
        .map(_.dest)
        .distinct
    (targetHints ++ legalCaptureTargets).distinct

  private def overloadedDefender(
      before: Board,
      after: Board,
      movingSide: Color,
      movedTo: Square,
      targets: List[Square]
  ): Option[RelationWitness] =
    val defenderColor = !movingSide
    val dutiesByDefender =
      targets
        .filter(target => after.attackers(target, movingSide).nonEmpty)
        .flatMap { target =>
          val pressureWasCreated =
            !before.attackers(target, movingSide).exists(_ == movedTo) &&
              after.attackers(target, movingSide).exists(_ == movedTo)
          val targetIsPressed =
            pressureWasCreated || after.attackers(target, movingSide).count > before.attackers(target, movingSide).count
          if targetIsPressed then
            after
              .attackers(target, defenderColor)
              .squares
              .filterNot(square => after.roleAt(square).contains(King))
              .map(_ -> target)
          else Nil
        }
        .groupMap(_._1)(_._2)

    dutiesByDefender.collectFirst {
      case (defenderSquare, duties) if duties.distinct.size >= 2 =>
        val dutySquares = duties.distinct.map(_.key).sorted
        RelationWitness(
          kind = RelationKind.Overload,
          focusSquares = (defenderSquare.key :: dutySquares).take(4),
          lineMoves = Nil,
          targetSquare = dutySquares.headOption,
          details = RelationDetails.Overload(
            defenderSquare = defenderSquare.key,
            targetSquares = dutySquares,
            attackerSquare = movedTo.key
          )
        )
    }

  private def discoveredAttackAfterMove(
      before: Board,
      after: Board,
      movingSide: Color,
      clearedSquare: Square,
      movedTo: Square,
      targetSet: Set[Square]
  ): Option[RelationWitness] =
    after.byColor(!movingSide).squares.toList
      .filter(targetSet.contains)
      .filterNot(target => after.roleAt(target).contains(King))
      .flatMap { target =>
        val beforeAttackers = before.attackers(target, movingSide)
        val afterAttackers = after.attackers(target, movingSide)
        (afterAttackers & ~beforeAttackers).squares.toList.flatMap { attacker =>
          if attacker == movedTo then Nil
          else
          after.roleAt(attacker).filter(isLongRangeRole).flatMap { role =>
            Option.when(Bitboard.between(attacker, target).contains(clearedSquare)) {
              RelationWitness(
                kind = RelationKind.DiscoveredAttack,
                focusSquares = List(attacker.key, clearedSquare.key, target.key),
                lineMoves = Nil,
                targetSquare = Some(target.key),
                details = RelationDetails.DiscoveredAttack(
                  attackerSquare = attacker.key,
                  clearedSquare = clearedSquare.key,
                  targetSquare = target.key,
                  attackerRole = role.name
                )
              )
            }
          }
        }
      }
      .headOption

  private def xrayAfterMove(
      board: Board,
      movingSide: Color,
      attacker: Square,
      attackerRole: Role,
      targetSet: Set[Square]
  ): Option[RelationWitness] =
    Option.when(isLongRangeRole(attackerRole))(()).flatMap { _ =>
      rayDirections(attackerRole).flatMap { case (fileStep, rankStep) =>
        for
          blocker <- firstOccupiedOnRay(board, attacker, fileStep, rankStep)
          target <- firstOccupiedOnRay(board, blocker, fileStep, rankStep)
          if targetSet.contains(target)
          blockerPiece <- board.pieceAt(blocker)
          targetPiece <- board.pieceAt(target)
          if blockerPiece.color != movingSide && targetPiece.color != movingSide
          if targetPiece.role != Pawn
        yield
          RelationWitness(
            kind = RelationKind.XRay,
            focusSquares = List(attacker.key, blocker.key, target.key),
            lineMoves = Nil,
            targetSquare = Some(target.key),
            details = RelationDetails.XRay(
              attackerSquare = attacker.key,
              blockerSquare = blocker.key,
              targetSquare = target.key,
              attackerRole = attackerRole.name,
              blockerRole = blockerPiece.role.name,
              targetRole = targetPiece.role.name
            )
          )
      }.headOption
    }

  private def forkAfterMove(
      board: Board,
      movingSide: Color,
      attacker: Square,
      attackerRole: Role,
      targetSet: Set[Square],
      targetHintSet: Set[Square]
  ): Option[RelationWitness] =
    val targets =
      roleAttacks(attackerRole, attacker, movingSide, board.occupied).squares.toList
        .filter(targetSet.contains)
        .flatMap(target =>
          board.pieceAt(target).filter(_.color != movingSide).map(piece => target -> piece.role)
        )
        .filter { case (target, role) =>
          role == King ||
            MaterialValue.tacticalValueCp(role) > MaterialValue.tacticalValueCp(attackerRole) ||
            board.attackers(target, !movingSide).isEmpty
        }
        .sortBy { case (target, role) => (targetHintPenalty(targetHintSet, target), -MaterialValue.tacticalValueCp(role), target.key) }
        .take(4)
    Option.when(targets.size >= 2) {
      val targetSquares = targets.map(_._1.key)
      val targetPieces =
        targets.map { case (target, role) => RelationDetails.TargetPiece(target.key, role.name) }
      RelationWitness(
        kind = RelationKind.Fork,
        focusSquares = (attacker.key :: targetSquares).distinct,
        lineMoves = Nil,
        targetSquare = targetSquares.headOption,
        details = RelationDetails.Fork(
          attackerSquare = attacker.key,
          attackerRole = attackerRole.name,
          targets = targetPieces
        )
      )
    }

  private def hangingPieceAfterMove(
      board: Board,
      movingSide: Color,
      attacker: Square,
      attackerRole: Role,
      targetSet: Set[Square],
      targetHintSet: Set[Square]
  ): Option[RelationWitness] =
    val attackedTargets =
      roleAttacks(attackerRole, attacker, movingSide, board.occupied).squares.toList
        .filter(targetSet.contains)
        .flatMap(target =>
          board.pieceAt(target).filter(_.color != movingSide).map(piece => target -> piece.role)
        )
        .filter { case (target, role) =>
          role != King &&
            board.attackers(target, !movingSide).isEmpty &&
            !sameColorRayPieceBehind(board, attacker, target) &&
            (role != Pawn || targetHintSet.contains(target)) &&
            (MaterialValue.tacticalValueCp(role) > MaterialValue.tacticalValueCp(attackerRole) || targetHintSet.contains(target))
        }
        .sortBy { case (target, role) => (targetHintPenalty(targetHintSet, target), -MaterialValue.tacticalValueCp(role), target.key) }
    attackedTargets.headOption.map { case (target, role) =>
      RelationWitness(
        kind = RelationKind.HangingPiece,
        focusSquares = List(attacker.key, target.key),
        lineMoves = Nil,
        targetSquare = Some(target.key),
        details = RelationDetails.HangingPiece(
          attackerSquare = attacker.key,
          targetSquare = target.key,
          attackerRole = attackerRole.name,
          targetRole = role.name
        )
      )
    }

  private def trappedPieceAfterMove(
      position: Position,
      movingSide: Color,
      attacker: Square,
      attackerRole: Role,
      targetSet: Set[Square],
      targetHintSet: Set[Square]
  ): Option[RelationWitness] =
    val board = position.board
    Option.when(position.color == !movingSide)(()).flatMap { _ =>
      roleAttacks(attackerRole, attacker, movingSide, board.occupied).squares.toList
        .filter(targetSet.contains)
        .flatMap(target =>
          board.pieceAt(target).filter(_.color != movingSide).map(piece => target -> piece.role)
        )
        .filter { case (target, role) =>
          role != King &&
            role != Pawn &&
            MaterialValue.tacticalValueCp(role) > MaterialValue.tacticalValueCp(attackerRole) &&
            safeEscapeSquares(position, target, role, !movingSide, movingSide).isEmpty
        }
        .sortBy { case (target, role) => (targetHintPenalty(targetHintSet, target), -MaterialValue.tacticalValueCp(role), target.key) }
        .headOption
        .map { case (target, role) =>
          RelationWitness(
            kind = RelationKind.TrappedPiece,
            focusSquares = List(attacker.key, target.key),
            lineMoves = Nil,
            targetSquare = Some(target.key),
            details = RelationDetails.TrappedPiece(
              attackerSquare = attacker.key,
              targetSquare = target.key,
              attackerRole = attackerRole.name,
              targetRole = role.name
            )
          )
        }
    }

  private def safeEscapeSquares(
      position: Position,
      target: Square,
      targetRole: Role,
      targetColor: Color,
      pressureSide: Color
  ): List[String] =
    position.legalMoves.toList
      .filter(move => move.orig == target && move.piece.color == targetColor && move.piece.role == targetRole)
      .flatMap { move =>
        Option.when(
          move.after.board.pieceAt(move.dest).exists(piece => piece.color == targetColor && piece.role == targetRole) &&
            move.after.board.attackers(move.dest, pressureSide).isEmpty
        )(move.dest.key)
      }
      .distinct

  private def dominationAfterMove(
      position: Position,
      movingSide: Color,
      attacker: Square,
      attackerRole: Role,
      targetSet: Set[Square],
      targetHintSet: Set[Square]
  ): Option[RelationWitness] =
    val board = position.board
    Option.when(position.color == !movingSide)(()).flatMap { _ =>
      roleAttacks(attackerRole, attacker, movingSide, board.occupied).squares.toList
        .filter(targetSet.contains)
        .flatMap(target =>
          board.pieceAt(target).filter(_.color != movingSide).map(piece => target -> piece.role)
        )
        .flatMap { case (target, role) =>
          val targetColor = !movingSide
          val pseudoEscapes = pseudoEscapeSquares(board, target, role, targetColor)
          val controlledEscapes =
            pseudoEscapes.filter(square => board.attackers(square, movingSide).nonEmpty).map(_.key).sorted
          Option.when(
            role != King &&
              role != Pawn &&
              MaterialValue.tacticalValueCp(role) <= MaterialValue.tacticalValueCp(attackerRole) &&
              pseudoEscapes.nonEmpty &&
              controlledEscapes.size == pseudoEscapes.size &&
              safeEscapeSquares(position, target, role, targetColor, movingSide).isEmpty &&
              !(isLongRangeRole(attackerRole) && sameColorRayPieceBehind(board, attacker, target))
          )(
            target -> role -> controlledEscapes
          )
        }
        .sortBy { case ((target, role), _) =>
          (targetHintPenalty(targetHintSet, target), -MaterialValue.tacticalValueCp(role), target.key)
        }
        .headOption
        .map { case ((target, role), controlledEscapes) =>
          RelationWitness(
            kind = RelationKind.Domination,
            focusSquares = (attacker.key :: target.key :: controlledEscapes).distinct,
            lineMoves = Nil,
            targetSquare = Some(target.key),
            details = RelationDetails.Domination(
              attackerSquare = attacker.key,
              targetSquare = target.key,
              attackerRole = attackerRole.name,
              targetRole = role.name,
              controlledEscapeSquares = controlledEscapes
            )
          )
        }
    }

  private def pseudoEscapeSquares(
      board: Board,
      target: Square,
      targetRole: Role,
      targetColor: Color
  ): List[Square] =
    roleAttacks(targetRole, target, targetColor, board.occupied).squares.toList
      .filter(square => !board.pieceAt(square).exists(_.color == targetColor))
      .distinct

  private def sameColorRayPieceBehind(
      board: Board,
      attacker: Square,
      target: Square
  ): Boolean =
    (for
      step <- rayStep(attacker, target)
      targetPiece <- board.pieceAt(target)
      behind <- firstOccupiedOnRay(board, target, step._1, step._2)
      behindPiece <- board.pieceAt(behind)
    yield behindPiece.color == targetPiece.color).getOrElse(false)

  private def rayStep(from: Square, to: Square): Option[(Int, Int)] =
    val fileDiff = to.file.value - from.file.value
    val rankDiff = to.rank.value - from.rank.value
    Option.when(
      fileDiff == 0 ||
        rankDiff == 0 ||
        fileDiff.abs == rankDiff.abs
    ) {
      (Integer.signum(fileDiff), Integer.signum(rankDiff))
    }.filter(_ != (0, 0))

  private def clearanceAfterMove(
      before: Board,
      after: Board,
      movingSide: Color,
      clearedSquare: Square,
      movedTo: Square,
      targetSet: Set[Square]
  ): Option[RelationWitness] =
    val friendlyLongRange =
      (after.byColor(movingSide) & (after.bishops | after.rooks | after.queens)).squares.toList.filterNot(_ == movedTo)
    friendlyLongRange.flatMap { beneficiary =>
      after.roleAt(beneficiary).filter(isLongRangeRole).toList.flatMap { role =>
        targetSet.toList.flatMap { target =>
          val beforeAttack = roleAttacks(role, beneficiary, movingSide, before.occupied).contains(target)
          val afterAttack = roleAttacks(role, beneficiary, movingSide, after.occupied).contains(target)
          val clearedRay =
            Bitboard.between(beneficiary, target).contains(clearedSquare) &&
              !Bitboard.between(beneficiary, target).contains(movedTo)
          for
            targetPiece <- after.pieceAt(target).toList
            if targetPiece.color != movingSide && targetPiece.role != Pawn
            if !beforeAttack && afterAttack && clearedRay
          yield
            RelationWitness(
              kind = RelationKind.Clearance,
              focusSquares = List(beneficiary.key, clearedSquare.key, target.key),
              lineMoves = Nil,
              targetSquare = Some(target.key),
              details = RelationDetails.Clearance(
                beneficiarySquare = beneficiary.key,
                clearedSquare = clearedSquare.key,
                targetSquare = target.key,
                beneficiaryRole = role.name,
                clearingTo = movedTo.key
              )
            )
        }
      }
    }.headOption

  private def batteryAfterMove(
      board: Board,
      movingSide: Color,
      movedTo: Square,
      movedRole: Role,
      targetSet: Set[Square],
      targetHintSet: Set[Square]
  ): Option[RelationWitness] =
    batteryPartnerRoles(movedRole).flatMap { partnerRole =>
      board.byPiece(movingSide, partnerRole).squares.toList.filterNot(_ == movedTo).flatMap { partner =>
        batteryLine(movedTo, movedRole, partner, partnerRole).toList.flatMap { case (axis, fileStep, rankStep) =>
          val clearBatteryLine =
            !(Bitboard.between(movedTo, partner) & board.occupied).nonEmpty
          Option.when(clearBatteryLine)(()).toList.flatMap { _ =>
            List(
              movedTo -> (-fileStep, -rankStep, partner),
              partner -> (fileStep, rankStep, movedTo)
            ).flatMap { case (front, (targetFileStep, targetRankStep, back)) =>
              for
                target <- firstOccupiedOnRay(board, front, targetFileStep, targetRankStep)
                if targetSet.contains(target)
                targetPiece <- board.pieceAt(target)
                if targetPiece.color != movingSide
                if targetPiece.role != Pawn || targetHintSet.contains(target)
                frontPiece <- board.pieceAt(front)
                backPiece <- board.pieceAt(back)
                frontRole = frontPiece.role.name
                backRole = backPiece.role.name
              yield
                RelationWitness(
                  kind = RelationKind.Battery,
                  focusSquares = List(front.key, back.key, target.key),
                  lineMoves = Nil,
                  targetSquare = Some(target.key),
                  details = RelationDetails.Battery(
                    frontSquare = front.key,
                    backSquare = back.key,
                    targetSquare = target.key,
                    frontRole = frontRole,
                    backRole = backRole,
                    axis = axis
                  )
                )
            }
          }
        }
      }
    }.headOption

  private def interferenceAfterMove(
      before: Board,
      after: Board,
      movingSide: Color,
      blocker: Square,
      blockerRole: Role,
      targetSet: Set[Square]
  ): Option[RelationWitness] =
    val defenderColor = !movingSide
    val defenders =
      (before.byColor(defenderColor) & (before.bishops | before.rooks | before.queens)).squares.toList
    defenders.flatMap { defender =>
      before.roleAt(defender).filter(isLongRangeRole).toList.flatMap { defenderRole =>
        targetSet.toList.flatMap { target =>
          val targetPiece = after.pieceAt(target)
          val defendedBefore =
            roleAttacks(defenderRole, defender, defenderColor, before.occupied).contains(target)
          val defendedAfter =
            roleAttacks(defenderRole, defender, defenderColor, after.occupied).contains(target)
          val lineBlocked =
            Bitboard.between(defender, target).contains(blocker) &&
              after.pieceAt(blocker).exists(_.color == movingSide)
          val targetIsPressured =
            after.attackers(target, movingSide).nonEmpty
          for
            piece <- targetPiece.toList
            if piece.color == defenderColor && piece.role != King
            if defender != target && blocker != target
            if defendedBefore && !defendedAfter && lineBlocked && targetIsPressured
          yield
            RelationWitness(
              kind = RelationKind.Interference,
              focusSquares = List(blocker.key, defender.key, target.key),
              lineMoves = Nil,
              targetSquare = Some(target.key),
              details = RelationDetails.Interference(
                blockerSquare = blocker.key,
                defenderSquare = defender.key,
                targetSquare = target.key,
                blockerRole = blockerRole.name,
                defenderRole = defenderRole.name,
                targetRole = piece.role.name
              )
            )
        }
      }
    }.headOption

  private def pinAfterMove(
      board: Board,
      movingSide: Color,
      attacker: Square,
      attackerRole: Role,
      targetSet: Set[Square]
  ): Option[RelationWitness] =
    Option.when(isLongRangeRole(attackerRole))(()).flatMap { _ =>
      rayDirections(attackerRole).flatMap { case (fileStep, rankStep) =>
        for
          pinned <- firstOccupiedOnRay(board, attacker, fileStep, rankStep)
          pinnedPiece <- board.pieceAt(pinned)
          if pinnedPiece.color != movingSide && pinnedPiece.role != King
          behind <- firstOccupiedOnRay(board, pinned, fileStep, rankStep)
          behindPiece <- board.pieceAt(behind)
          if behindPiece.color == pinnedPiece.color
          if behindPiece.role == King || MaterialValue.tacticalValueCp(behindPiece.role) > MaterialValue.tacticalValueCp(pinnedPiece.role)
          if targetSet.contains(pinned) || targetSet.contains(behind)
          target = if targetSet.contains(pinned) then pinned else behind
        yield
          RelationWitness(
            kind = RelationKind.Pin,
            focusSquares = List(attacker.key, pinned.key, behind.key),
            lineMoves = Nil,
            targetSquare = Some(target.key),
            details = RelationDetails.Pin(
              attackerSquare = attacker.key,
              pinnedSquare = pinned.key,
              behindSquare = behind.key,
              targetSquare = target.key,
              attackerRole = attackerRole.name,
              pinnedRole = pinnedPiece.role.name,
              behindRole = behindPiece.role.name,
              absolute = behindPiece.role == King
            )
          )
      }.headOption
    }

  private def attackedPinnedPieceAfterMove(
      before: Board,
      after: Board,
      movingSide: Color,
      movedFrom: Square,
      movedTo: Square,
      movedRole: Role,
      targetSet: Set[Square]
  ): Option[RelationWitness] =
    val newTargets =
      (roleAttacks(movedRole, movedTo, movingSide, after.occupied) & after.byColor(!movingSide)).squares.toList
        .filter(targetSet.contains)
        .filter(target =>
          after.attackers(target, movingSide).exists(_ == movedTo) &&
            !before.attackers(target, movingSide).exists(_ == movedFrom) &&
            after.attackers(target, movingSide).count > before.attackers(target, movingSide).count &&
            after.attackers(target, movingSide).count >= 2
        )
    newTargets.flatMap(target => existingPinOnTarget(after, movingSide, movedTo, target, targetSet)).headOption

  private def existingPinOnTarget(
      board: Board,
      movingSide: Color,
      pressureSquare: Square,
      pinned: Square,
      targetSet: Set[Square]
  ): Option[RelationWitness] =
    board.pieceAt(pinned).filter(piece => piece.color != movingSide && piece.role != King).toList.flatMap { pinnedPiece =>
      board
        .attackers(pinned, movingSide)
        .squares
        .toList
        .filterNot(_ == pressureSquare)
        .flatMap { pinner =>
          board.roleAt(pinner).filter(isLongRangeRole).toList.flatMap { pinnerRole =>
            rayStep(pinner, pinned).toList.flatMap { case (fileStep, rankStep) =>
              for
                behind <- firstOccupiedOnRay(board, pinned, fileStep, rankStep)
                behindPiece <- board.pieceAt(behind)
                if behindPiece.color == pinnedPiece.color
                if behindPiece.role == King || MaterialValue.tacticalValueCp(behindPiece.role) > MaterialValue.tacticalValueCp(pinnedPiece.role)
                if targetSet.contains(pinned) || targetSet.contains(behind)
                target = if targetSet.contains(pinned) then pinned else behind
              yield
                RelationWitness(
                  kind = RelationKind.Pin,
                  focusSquares = List(pressureSquare.key, pinner.key, pinned.key, behind.key),
                  lineMoves = Nil,
                  targetSquare = Some(target.key),
                  details = RelationDetails.Pin(
                    attackerSquare = pinner.key,
                    pinnedSquare = pinned.key,
                    behindSquare = behind.key,
                    targetSquare = target.key,
                    attackerRole = pinnerRole.name,
                    pinnedRole = pinnedPiece.role.name,
                    behindRole = behindPiece.role.name,
                    absolute = behindPiece.role == King
                  )
                )
            }
          }
        }
    }.headOption

  private def skewerAfterMove(
      board: Board,
      movingSide: Color,
      attacker: Square,
      attackerRole: Role,
      targetSet: Set[Square]
  ): Option[RelationWitness] =
    Option.when(isLongRangeRole(attackerRole))(()).flatMap { _ =>
      rayDirections(attackerRole).flatMap { case (fileStep, rankStep) =>
        for
          front <- firstOccupiedOnRay(board, attacker, fileStep, rankStep)
          frontPiece <- board.pieceAt(front)
          if frontPiece.color != movingSide
          back <- firstOccupiedOnRay(board, front, fileStep, rankStep)
          backPiece <- board.pieceAt(back)
          if backPiece.color == frontPiece.color && backPiece.role != Pawn
          if frontPiece.role == King || MaterialValue.tacticalValueCp(frontPiece.role) > MaterialValue.tacticalValueCp(backPiece.role)
          if targetSet.contains(front) || targetSet.contains(back)
          target = if targetSet.contains(front) then front else back
        yield
          RelationWitness(
            kind = RelationKind.Skewer,
            focusSquares = List(attacker.key, front.key, back.key),
            lineMoves = Nil,
            targetSquare = Some(target.key),
            details = RelationDetails.Skewer(
              attackerSquare = attacker.key,
              frontSquare = front.key,
              backSquare = back.key,
              targetSquare = target.key,
              attackerRole = attackerRole.name,
              frontRole = frontPiece.role.name,
              backRole = backPiece.role.name
            )
          )
      }.headOption
    }

  private def isLongRangeRole(role: Role): Boolean =
    role == Bishop || role == Rook || role == Queen

  private[chessjudgment] def roleAttacks(role: Role, square: Square, color: Color, occupied: Bitboard): Bitboard =
    role match
      case Pawn   => square.pawnAttacks(color)
      case Knight => square.knightAttacks
      case Bishop => square.bishopAttacks(occupied)
      case Rook   => square.rookAttacks(occupied)
      case Queen  => square.queenAttacks(occupied)
      case King   => square.kingAttacks

  private def tacticalPattern(id: String) =
    TacticalPatternDetectors.ordered.find(_.id == id)

  private def matePatternExcept(
      excludedId: String,
      step: BoundedReplayStep
  ) =
    TacticalPatternDetectors.ordered.find(detector =>
      detector.requiresMate &&
        detector.id != excludedId &&
        detector.matches(Some(step.before), step.after, step.uci)
    )

  private def rayDirections(role: Role): List[(Int, Int)] =
    val bishopDirections = List(1 -> 1, 1 -> -1, -1 -> 1, -1 -> -1)
    val rookDirections = List(1 -> 0, -1 -> 0, 0 -> 1, 0 -> -1)
    role match
      case Bishop => bishopDirections
      case Rook   => rookDirections
      case Queen  => bishopDirections ++ rookDirections
      case _      => Nil

  private def batteryPartnerRoles(role: Role): List[Role] =
    role match
      case Queen  => List(Rook, Bishop)
      case Rook   => List(Queen, Rook)
      case Bishop => List(Queen)
      case _      => Nil

  private def batteryLine(
      movedTo: Square,
      movedRole: Role,
      partner: Square,
      partnerRole: Role
  ): Option[(RelationAxis, Int, Int)] =
    val fileDiff = partner.file.value - movedTo.file.value
    val rankDiff = partner.rank.value - movedTo.rank.value
    val axis =
      if fileDiff == 0 && rankDiff != 0 then Some(RelationAxis.File)
      else if rankDiff == 0 && fileDiff != 0 then Some(RelationAxis.Rank)
      else if fileDiff.abs == rankDiff.abs && fileDiff != 0 then Some(RelationAxis.Diagonal)
      else None
    axis.filter(batteryPairCompatible(movedRole, partnerRole, _)).map { name =>
      (name, Integer.signum(fileDiff), Integer.signum(rankDiff))
    }

  private def batteryPairCompatible(left: Role, right: Role, axis: RelationAxis): Boolean =
    axis match
      case RelationAxis.Diagonal =>
        (left == Queen && right == Bishop) || (left == Bishop && right == Queen)
      case RelationAxis.File | RelationAxis.Rank =>
        (left == Queen && right == Rook) ||
          (left == Rook && right == Queen) ||
          (left == Rook && right == Rook)

  private def sameBranchBadPieceLiquidation(
      replay: List[BoundedReplayStep],
      movingSide: Color,
      originalSquare: Square,
      firstDest: Square
  ): Option[BadPieceLiquidationBranch] =
    def loop(remaining: List[(BoundedReplayStep, Int)], bishopSquare: Square): Option[BadPieceLiquidationBranch] =
      remaining match
        case Nil => None
        case (step, index) :: rest
            if step.move.piece.color == movingSide &&
              step.move.piece.role == Bishop &&
              step.move.orig == bishopSquare =>
          val exchange = step.move.dest
          val recapture = replay.lift(index + 1)
          val liquidation =
            step.move.captures &&
              step.capturedRole.exists(_ != Pawn) &&
              recapture.exists(reply =>
                reply.move.piece.color != movingSide &&
                  reply.move.dest == exchange &&
                  reply.move.captures &&
                  reply.capturedRole.contains(Bishop)
              )
          if liquidation then
            Some(
              BadPieceLiquidationBranch(
                badPieceSquare = originalSquare.key,
                exchangeSquare = exchange.key,
                lineMoves = replayUcis(replay, 0, index + 2)
              )
            )
          else loop(rest, step.move.dest)
        case _ :: rest =>
          loop(rest, bishopSquare)

    loop(replay.zipWithIndex.drop(2), firstDest)

  private def immediateBadPieceLiquidation(
      replay: List[BoundedReplayStep],
      movingSide: Color,
      originalSquare: Square,
      firstDest: Square
  ): Option[BadPieceLiquidationBranch] =
    (replay.headOption, replay.lift(1)) match
      case (Some(first), Some(reply))
          if first.move.piece.color == movingSide &&
            first.move.piece.role == Bishop &&
            first.move.dest == firstDest &&
            first.move.captures &&
            first.capturedRole.exists(_ != Pawn) &&
            reply.move.piece.color != movingSide &&
            reply.move.dest == firstDest &&
            reply.move.captures &&
            reply.capturedRole.contains(Bishop) =>
        Some(
          BadPieceLiquidationBranch(
            badPieceSquare = originalSquare.key,
            exchangeSquare = firstDest.key,
            lineMoves = replayUcis(replay, 0, 2)
          )
        )
      case _ => None

  private def isBadBishopOnCurrentBoard(board: Board, color: Color, square: Square): Boolean =
    board.pieceAt(square).exists(piece => piece.color == color && piece.role == Bishop) &&
      sameColorCentralPawnCount(board, color, square) >= 2 &&
      (bishopMobility(board, color, square) <= 5 || bishopOwnPawnBlockers(board, color, square) > 0)

  private def sameColorCentralPawnCount(board: Board, color: Color, square: Square): Int =
    board
      .byPiece(color, Pawn)
      .squares
      .count(pawn => Set("c", "d", "e", "f").contains(pawn.key.take(1)) && pawn.isLight == square.isLight)

  private def bishopMobility(board: Board, color: Color, square: Square): Int =
    (square.bishopAttacks(board.occupied) & ~board.byColor(color)).count

  private def bishopOwnPawnBlockers(board: Board, color: Color, square: Square): Int =
    List((1, 1), (1, -1), (-1, 1), (-1, -1)).count { case (fileStep, rankStep) =>
      firstOccupiedOnRay(board, square, fileStep, rankStep).exists(blocker =>
        board.pieceAt(blocker).exists(piece =>
          piece.color == color &&
            piece.role == Pawn &&
            Set("c", "d", "e", "f").contains(blocker.key.take(1))
        )
      )
    }

  private def firstOccupiedOnRay(
      board: Board,
      square: Square,
      fileStep: Int,
      rankStep: Int
  ): Option[Square] =
    def loop(file: Int, rank: Int): Option[Square] =
      Square.at(file, rank) match
        case Some(next) if board.pieceAt(next).nonEmpty => Some(next)
        case Some(next)                                => loop(next.file.value + fileStep, next.rank.value + rankStep)
        case None                                      => None
    loop(square.file.value + fileStep, square.rank.value + rankStep)

  private def clean(raw: String): Option[String] =
    Option(raw).map(_.trim).filter(_.nonEmpty)
