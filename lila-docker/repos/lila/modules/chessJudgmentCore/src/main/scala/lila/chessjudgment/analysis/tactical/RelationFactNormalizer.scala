package lila.chessjudgment.analysis.tactical

import lila.chessjudgment.model.judgment.*

object RelationFactNormalizer:

  def fromWitness(
      id: String,
      witness: RelationWitness,
      position: PositionNodeRef,
      line: Option[LineNodeRef],
      scope: EvidenceScope,
      confidence: EvidenceConfidence
  ): Option[EvidenceRecord] =
    for
      kind <- RelationFactKind.fromId(witness.kind)
      detail <- TacticalRelationEvidence.typedDetailsFromWitness(witness)
    yield
      val focusSquares = witness.focusSquares.map(EvidenceSquare(_))
      val targetSquare = witness.targetSquare.map(EvidenceSquare(_))
      val lineMoves = witness.lineMoves
      val relationParticipants = participants(detail, focusSquares)
      val witnessProof =
        RelationWitnessProof(
          sourceKind = witness.kind,
          detail = witnessDetail(detail),
          focusSquares = focusSquares,
          targetSquare = targetSquare,
          lineMoves = lineMoves,
          participants = relationParticipants,
          proofAtoms = proofAtoms(
            focusSquares = focusSquares,
            targetSquare = targetSquare,
            lineMoves = lineMoves,
            participants = relationParticipants,
            kind = witness.kind
          )
        )
      val ref =
        EvidenceRef(
          id = id,
          producer = EvidenceProducer.TacticalRelationProducer,
          layer = EvidenceLayer.Relation,
          position = position,
          line = line,
          scope = scope,
          confidence = confidence
        )
      EvidenceRecord(
        ref = ref,
        payload = RelationFactEvidence(
          kind = kind,
          focusSquares = focusSquares,
          targetSquare = targetSquare,
          lineMoves = lineMoves,
          participants = relationParticipants
        )(witnessProof)
      )

  private def witnessDetail(details: RelationDetails): RelationWitnessDetail =
    import RelationDetails.*
    details match
      case Empty =>
        RelationWitnessDetail.Empty
      case DefenderTrade(defenderSquare, exchangeSquare, targetSquare) =>
        RelationWitnessDetail.DefenderTrade(square(defenderSquare), square(exchangeSquare), square(targetSquare))
      case BadPieceLiquidation(badPieceSquare, exchangeSquare) =>
        RelationWitnessDetail.BadPieceLiquidation(square(badPieceSquare), square(exchangeSquare))
      case Overload(defenderSquare, targetSquares, attackerSquare) =>
        RelationWitnessDetail.Overload(square(defenderSquare), targetSquares.map(square), square(attackerSquare))
      case Deflection(defenderSquare, targetSquare, attackerSquare) =>
        RelationWitnessDetail.Deflection(square(defenderSquare), square(targetSquare), square(attackerSquare))
      case DiscoveredAttack(attackerSquare, clearedSquare, targetSquare, attackerRole) =>
        RelationWitnessDetail.DiscoveredAttack(square(attackerSquare), square(clearedSquare), square(targetSquare), piece(attackerRole))
      case DoubleCheck(kingSquare, checkerSquares, moverSquare, moverRole) =>
        RelationWitnessDetail.DoubleCheck(square(kingSquare), checkerSquares.map(square), square(moverSquare), piece(moverRole))
      case MatePattern(relationKind, kingSquare, checkerSquares, matingMove, patternId) =>
        RelationWitnessDetail.MatePattern(relationKind, square(kingSquare), checkerSquares.map(square), matingMove, patternId)
      case GreekGift(bishopSquare, targetSquare, entryMove, patternId) =>
        RelationWitnessDetail.GreekGift(square(bishopSquare), square(targetSquare), entryMove, patternId)
      case Fork(attackerSquare, attackerRole, targets) =>
        RelationWitnessDetail.Fork(
          square(attackerSquare),
          piece(attackerRole),
          targets.map(target => RelationWitnessTarget(square(target.square), piece(target.role)))
        )
      case HangingPiece(attackerSquare, targetSquare, attackerRole, targetRole) =>
        RelationWitnessDetail.HangingPiece(square(attackerSquare), square(targetSquare), piece(attackerRole), piece(targetRole))
      case TrappedPiece(attackerSquare, targetSquare, attackerRole, targetRole) =>
        RelationWitnessDetail.TrappedPiece(square(attackerSquare), square(targetSquare), piece(attackerRole), piece(targetRole))
      case Domination(attackerSquare, targetSquare, attackerRole, targetRole, controlledEscapeSquares) =>
        RelationWitnessDetail.Domination(
          square(attackerSquare),
          square(targetSquare),
          piece(attackerRole),
          piece(targetRole),
          controlledEscapeSquares.map(square)
        )
      case Zwischenzug(intermediateMove, expectedRecaptureSquare, checkingPieceSquare, checkingPieceRole, checkedKingSquare, threatType) =>
        RelationWitnessDetail.Zwischenzug(
          intermediateMove,
          square(expectedRecaptureSquare),
          square(checkingPieceSquare),
          piece(checkingPieceRole),
          square(checkedKingSquare),
          threatSignal(threatType)
        )
      case Decoy(baitFromSquare, baitSquare, luredFromSquare, executionFromSquare, executionToSquare, baitRole, luredRole) =>
        RelationWitnessDetail.Decoy(
          square(baitFromSquare),
          square(baitSquare),
          square(luredFromSquare),
          square(executionFromSquare),
          square(executionToSquare),
          piece(baitRole),
          piece(luredRole)
        )
      case XRay(attackerSquare, blockerSquare, targetSquare, attackerRole, blockerRole, targetRole) =>
        RelationWitnessDetail.XRay(
          square(attackerSquare),
          square(blockerSquare),
          square(targetSquare),
          piece(attackerRole),
          piece(blockerRole),
          piece(targetRole)
        )
      case Clearance(beneficiarySquare, clearedSquare, targetSquare, beneficiaryRole, clearingTo) =>
        RelationWitnessDetail.Clearance(
          square(beneficiarySquare),
          square(clearedSquare),
          square(targetSquare),
          piece(beneficiaryRole),
          square(clearingTo)
        )
      case Battery(frontSquare, backSquare, targetSquare, frontRole, backRole, axis) =>
        RelationWitnessDetail.Battery(
          square(frontSquare),
          square(backSquare),
          square(targetSquare),
          piece(frontRole),
          piece(backRole),
          axisSignal(axis)
        )
      case Interference(blockerSquare, defenderSquare, targetSquare, blockerRole, defenderRole, targetRole) =>
        RelationWitnessDetail.Interference(
          square(blockerSquare),
          square(defenderSquare),
          square(targetSquare),
          piece(blockerRole),
          piece(defenderRole),
          piece(targetRole)
        )
      case Pin(attackerSquare, pinnedSquare, behindSquare, targetSquare, attackerRole, pinnedRole, behindRole, absolute) =>
        RelationWitnessDetail.Pin(
          square(attackerSquare),
          square(pinnedSquare),
          square(behindSquare),
          square(targetSquare),
          piece(attackerRole),
          piece(pinnedRole),
          piece(behindRole),
          absolute
        )
      case Skewer(attackerSquare, frontSquare, backSquare, targetSquare, attackerRole, frontRole, backRole) =>
        RelationWitnessDetail.Skewer(
          square(attackerSquare),
          square(frontSquare),
          square(backSquare),
          square(targetSquare),
          piece(attackerRole),
          piece(frontRole),
          piece(backRole)
        )
      case StalemateTrap(stalematedKingSquare, resourceSquare, entryMove, terminalMove, scoreCp) =>
        RelationWitnessDetail.StalemateTrap(square(stalematedKingSquare), square(resourceSquare), entryMove, terminalMove, scoreCp)
      case PerpetualCheck(
            checkedKingSquare,
            checkerSquares,
            checkingSide,
            entryMove,
            cycleStartMove,
            cycleReturnMove,
            repeatedPositionKey,
            scoreCp
          ) =>
        RelationWitnessDetail.PerpetualCheck(
          square(checkedKingSquare),
          checkerSquares.map(square),
          checkingSide,
          entryMove,
          cycleStartMove,
          cycleReturnMove,
          repeatedPositionKey,
          scoreCp
        )

  private def proofAtoms(
      focusSquares: List[EvidenceSquare],
      targetSquare: Option[EvidenceSquare],
      lineMoves: List[String],
      participants: List[RelationParticipant],
      kind: String
  ): List[RelationProofAtom] =
    val participantAtoms =
      participants.map { participant =>
        RelationProofAtom(
          role = RelationProofAtomRole.Participant,
          square = Some(participant.square),
          participantRole = Some(participant.participantRole),
          pieceRole = participant.role
        )
      }
    val lineMoveAtoms =
      lineMoves.zipWithIndex.map { case (move, index) =>
        RelationProofAtom(
          role = RelationProofAtomRole.LineMove,
          moveUci = Some(move),
          label = Some(s"$kind:line:${index + 1}")
        )
      }
    val focusAtoms =
      focusSquares.map(square => RelationProofAtom(role = RelationProofAtomRole.Focus, square = Some(square)))
    val targetAtoms =
      targetSquare.toList.map(square => RelationProofAtom(role = RelationProofAtomRole.Target, square = Some(square)))
    (participantAtoms ++ lineMoveAtoms ++ focusAtoms ++ targetAtoms).distinct

  private def participants(details: RelationDetails, focusSquares: List[EvidenceSquare]): List[RelationParticipant] =
    import RelationDetails.*
    details match
      case Empty => Nil
      case DefenderTrade(defenderSquare, exchangeSquare, targetSquare) =>
        List(
          part(defenderSquare, RelationParticipantRole.Defender),
          part(exchangeSquare, RelationParticipantRole.Mover),
          part(targetSquare, RelationParticipantRole.Target)
        )
      case BadPieceLiquidation(badPieceSquare, exchangeSquare) =>
        List(
          part(badPieceSquare, RelationParticipantRole.Target),
          part(exchangeSquare, RelationParticipantRole.Mover)
        )
      case Overload(defenderSquare, targetSquares, attackerSquare) =>
        part(defenderSquare, RelationParticipantRole.Defender) ::
          part(attackerSquare, RelationParticipantRole.Attacker) ::
          targetSquares.map(part(_, RelationParticipantRole.Target))
      case Deflection(defenderSquare, targetSquare, attackerSquare) =>
        List(
          part(defenderSquare, RelationParticipantRole.Defender),
          part(targetSquare, RelationParticipantRole.Target),
          part(attackerSquare, RelationParticipantRole.Attacker)
        )
      case DiscoveredAttack(attackerSquare, clearedSquare, targetSquare, attackerRole) =>
        List(
          part(attackerSquare, RelationParticipantRole.Attacker, Some(attackerRole)),
          part(clearedSquare, RelationParticipantRole.Mover),
          part(targetSquare, RelationParticipantRole.Target)
        )
      case DoubleCheck(kingSquare, checkerSquares, moverSquare, moverRole) =>
        part(kingSquare, RelationParticipantRole.King) ::
          part(moverSquare, RelationParticipantRole.Mover, Some(moverRole)) ::
          checkerSquares.map(part(_, RelationParticipantRole.Attacker))
      case MatePattern(_, kingSquare, checkerSquares, matingMove, _) =>
        part(kingSquare, RelationParticipantRole.King) ::
          uciDestination(matingMove).map(part(_, RelationParticipantRole.Mover)).toList :::
          checkerSquares.map(part(_, RelationParticipantRole.Attacker))
      case GreekGift(bishopSquare, targetSquare, _, _) =>
        List(
          part(bishopSquare, RelationParticipantRole.Attacker, Some("bishop")),
          part(targetSquare, RelationParticipantRole.Target)
        )
      case Fork(attackerSquare, attackerRole, targets) =>
        part(attackerSquare, RelationParticipantRole.Attacker, Some(attackerRole)) ::
          targets.map(t => part(t.square, RelationParticipantRole.Target, Some(t.role)))
      case HangingPiece(attackerSquare, targetSquare, attackerRole, targetRole) =>
        List(
          part(attackerSquare, RelationParticipantRole.Attacker, Some(attackerRole)),
          part(targetSquare, RelationParticipantRole.Target, Some(targetRole))
        )
      case TrappedPiece(attackerSquare, targetSquare, attackerRole, targetRole) =>
        List(
          part(attackerSquare, RelationParticipantRole.Attacker, Some(attackerRole)),
          part(targetSquare, RelationParticipantRole.Target, Some(targetRole))
        )
      case Domination(attackerSquare, targetSquare, attackerRole, targetRole, controlledEscapeSquares) =>
        List(
          part(attackerSquare, RelationParticipantRole.Attacker, Some(attackerRole)),
          part(targetSquare, RelationParticipantRole.Target, Some(targetRole))
        ) ++ controlledEscapeSquares.map(part(_, RelationParticipantRole.Other))
      case Zwischenzug(intermediateMove, expectedRecaptureSquare, checkingPieceSquare, checkingPieceRole, checkedKingSquare, _) =>
        uciDestination(intermediateMove).map(part(_, RelationParticipantRole.Mover)).toList ++ List(
          part(expectedRecaptureSquare, RelationParticipantRole.Target),
          part(checkingPieceSquare, RelationParticipantRole.Attacker, Some(checkingPieceRole)),
          part(checkedKingSquare, RelationParticipantRole.King)
        )
      case Decoy(baitFromSquare, baitSquare, luredFromSquare, executionFromSquare, executionToSquare, baitRole, luredRole) =>
        List(
          part(baitFromSquare, RelationParticipantRole.Bait, Some(baitRole)),
          part(baitSquare, RelationParticipantRole.Bait, Some(baitRole)),
          part(luredFromSquare, RelationParticipantRole.Lured, Some(luredRole)),
          part(executionFromSquare, RelationParticipantRole.Attacker),
          part(executionToSquare, RelationParticipantRole.Target)
        )
      case XRay(attackerSquare, blockerSquare, targetSquare, attackerRole, blockerRole, targetRole) =>
        List(
          part(attackerSquare, RelationParticipantRole.Attacker, Some(attackerRole)),
          part(blockerSquare, RelationParticipantRole.Blocker, Some(blockerRole)),
          part(targetSquare, RelationParticipantRole.Target, Some(targetRole))
        )
      case Clearance(beneficiarySquare, clearedSquare, targetSquare, beneficiaryRole, clearingTo) =>
        List(
          part(beneficiarySquare, RelationParticipantRole.Beneficiary, Some(beneficiaryRole)),
          part(clearedSquare, RelationParticipantRole.Mover),
          part(clearingTo, RelationParticipantRole.Mover),
          part(targetSquare, RelationParticipantRole.Target)
        )
      case Battery(frontSquare, backSquare, targetSquare, frontRole, backRole, _) =>
        List(
          part(frontSquare, RelationParticipantRole.Attacker, Some(frontRole)),
          part(backSquare, RelationParticipantRole.Attacker, Some(backRole)),
          part(targetSquare, RelationParticipantRole.Target)
        )
      case Interference(blockerSquare, defenderSquare, targetSquare, blockerRole, defenderRole, targetRole) =>
        List(
          part(blockerSquare, RelationParticipantRole.Blocker, Some(blockerRole)),
          part(defenderSquare, RelationParticipantRole.Defender, Some(defenderRole)),
          part(targetSquare, RelationParticipantRole.Target, Some(targetRole))
        )
      case Pin(attackerSquare, pinnedSquare, behindSquare, targetSquare, attackerRole, pinnedRole, behindRole, _) =>
        val base =
          List(
            part(attackerSquare, RelationParticipantRole.Attacker, Some(attackerRole)),
            part(pinnedSquare, RelationParticipantRole.Defender, Some(pinnedRole)),
            part(behindSquare, RelationParticipantRole.Target, Some(behindRole)),
            part(targetSquare, RelationParticipantRole.Target)
          )
        val typedSquares = Set(attackerSquare, pinnedSquare, behindSquare, targetSquare)
        base ++ focusSquares.map(_.key).filterNot(typedSquares).map(part(_, RelationParticipantRole.Attacker))
      case Skewer(attackerSquare, frontSquare, backSquare, targetSquare, attackerRole, frontRole, backRole) =>
        List(
          part(attackerSquare, RelationParticipantRole.Attacker, Some(attackerRole)),
          part(frontSquare, RelationParticipantRole.Target, Some(frontRole)),
          part(backSquare, RelationParticipantRole.Target, Some(backRole)),
          part(targetSquare, RelationParticipantRole.Target)
        )
      case StalemateTrap(stalematedKingSquare, resourceSquare, _, _, _) =>
        List(
          part(stalematedKingSquare, RelationParticipantRole.King),
          part(resourceSquare, RelationParticipantRole.Other)
        )
      case PerpetualCheck(checkedKingSquare, checkerSquares, _, _, _, _, _, _) =>
        part(checkedKingSquare, RelationParticipantRole.King) ::
          checkerSquares.map(part(_, RelationParticipantRole.Attacker))

  private def part(
      square: String,
      participantRole: RelationParticipantRole,
      role: Option[String] = None
  ): RelationParticipant =
    RelationParticipant(
      square = EvidenceSquare(square),
      role = role.map(EvidencePieceRole(_)),
      participantRole = participantRole
    )

  private def square(key: String): EvidenceSquare =
    EvidenceSquare(key)

  private def piece(role: String): EvidencePieceRole =
    EvidencePieceRole(role)

  private def threatSignal(threatType: RelationThreatType): RelationThreatSignal =
    threatType match
      case RelationThreatType.MateCheck => RelationThreatSignal.MateCheck
      case RelationThreatType.Check     => RelationThreatSignal.Check

  private def axisSignal(axis: RelationAxis): RelationAxisSignal =
    axis match
      case RelationAxis.File     => RelationAxisSignal.File
      case RelationAxis.Rank     => RelationAxisSignal.Rank
      case RelationAxis.Diagonal => RelationAxisSignal.Diagonal

  private def uciDestination(move: String): Option[String] =
    val normalized = Option(move).getOrElse("").trim.toLowerCase
    val dest = normalized.drop(2).take(2)
    Option.when(dest.matches("[a-h][1-8]"))(dest)
