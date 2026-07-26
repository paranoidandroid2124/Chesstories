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
      rawDetail <- TacticalRelationEvidence.typedDetailsFromWitness(witness)
      detail = witnessDetail(rawDetail)
      payload <- RelationFactEvidence.from(detail = detail, lineMoves = witness.lineMoves)
      if RelationFactKind.fromId(witness.kind).contains(payload.kind)
    yield
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
        payload = payload
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
      case StalemateTrap(stalematedKingSquare, resourceSquare, entryMove, terminalMove) =>
        RelationWitnessDetail.StalemateTrap(square(stalematedKingSquare), square(resourceSquare), entryMove, terminalMove)
      case PerpetualCheck(
            checkedKingSquare,
            checkerSquares,
            checkingSide,
            entryMove,
            cycleStartMove,
            cycleReturnMove,
            repeatedPositionKey
          ) =>
        RelationWitnessDetail.PerpetualCheck(
          square(checkedKingSquare),
          checkerSquares.map(square),
          checkingSide,
          entryMove,
          cycleStartMove,
          cycleReturnMove,
          repeatedPositionKey
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
