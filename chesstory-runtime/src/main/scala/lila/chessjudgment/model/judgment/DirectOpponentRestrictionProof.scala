package lila.chessjudgment.model.judgment

import chess.*
import lila.chessjudgment.model.strategic.PlanTaxonomy.PlanTheme

/** Shared functional/persistence proof for direct opponent restrictions. The
  * plan-event producer and every public strategic-authority gate must use this
  * same boundary; a raw structural label cannot stand in for reply survival.
  */
private[chessjudgment] object DirectOpponentRestrictionProof:
  type ExactPawnBlockadePrimitive = (
      EvidenceRef,
      StructuralDeltaEvidence,
      EvidenceRef,
      LineFactEvidence,
      TransitionConsequence
  )

  /** Resolve only proof primitives directly owned by the public plan event.
    * Ancestors and siblings are deliberately excluded: they may explain how
    * the event was assembled, but cannot lend it a different root effect.
    */
  def exactRootPawnBlockadePrimitives(
      eventRef: EvidenceRef,
      event: PlanCausalEventEvidence,
      graph: TypedEvidenceGraph
  ): List[ExactPawnBlockadePrimitive] =
    graph.record(eventRef).toList.flatMap { eventRecord =>
      val directParents = eventRecord.parents.flatMap(graph.record)
      val structuralRecords = directParents.collect {
        case EvidenceRecord(structuralRef, structural: StructuralDeltaEvidence, _)
            if structuralRef.line.contains(event.rootLine) &&
              structural.line.contains(event.rootLine) &&
              structural.transition == event.rootTransition =>
          structuralRef -> structural
      }
      val lineRecords = directParents.collect {
        case EvidenceRecord(lineRef, line: LineFactEvidence, _)
            if lineRef.line.contains(event.rootLine) && line.line == event.rootLine =>
          lineRef -> line
      }
      Option
        .when(
          eventRecord.payload == event &&
            eventRef.confidence != EvidenceConfidence.Heuristic &&
            rootMoveDirectlyRestrictsOpponent(event)
        )(
          for
            (structuralRef, structural) <- structuralRecords
            (lineRef, line) <- lineRecords
            consequence <- structural.consequences
            if exactRootPawnBlockadeAuthority(event, structural, line, consequence)
          yield (structuralRef, structural, lineRef, line, consequence)
        )
        .getOrElse(Nil)
    }

  def exactRootPawnBlockadeConsequences(
      event: PlanCausalEventEvidence
  ): List[TransitionConsequence] =
    event.directGoalConsequences.filter { consequence =>
      val structural = StructuralDeltaEvidence(
        transition = event.rootTransition,
        signals = Nil,
        consequences = List(consequence)
      )
      StructuralDeltaEvidence
        .exactRootOccupiedPawnAdvanceRestrictions(structural, consequence)
        .nonEmpty
    }

  def directRestrictionSurvivesReply(
      rootLine: LineNodeRef,
      structural: StructuralDeltaEvidence,
      line: Option[LineFactEvidence]
  ): Boolean =
    val restrictionConsequences =
      structural.consequencesOf(TransitionConsequenceKind.OpponentMobilityRestriction)
    val syntacticDirectBlockades = restrictionConsequences.flatMap(consequence =>
      consequence.subjects
        .filter(StructuralDeltaEvidence.directlyBlockedPawnAdvance)
        .map(subject => consequence -> subject)
    )
    val everyDirectBlockadeIsExact = syntacticDirectBlockades.forall { case (consequence, subject) =>
      StructuralDeltaEvidence
        .exactRootOccupiedPawnAdvanceRestrictions(structural, consequence)
        .contains(subject)
    }
    syntacticDirectBlockades.isEmpty ||
      (everyDirectBlockadeIsExact &&
        !rootActorIsDevelopingMinor(rootLine, structural) &&
        line.exists(payload =>
          payload.line == rootLine &&
            payload.rootActorSurvivesReply.contains(true)
        ))

  def rootMoveDirectlyRestrictsOpponent(event: PlanCausalEventEvidence): Boolean =
    val restrictedEntries = event.directGoalConsequences
      .filter(_.kind == TransitionConsequenceKind.OpponentMobilityRestriction)
      .flatMap(_.subjects.flatMap(StructuralDeltaEvidence.restrictedOpponentEntry))
    val restrictionSubjects = event.directGoalConsequences
      .filter(_.kind == TransitionConsequenceKind.OpponentMobilityRestriction)
      .flatMap(_.subjects)
    val actorRole = event.identity.actorRole.map(_.toLowerCase)
    val kingMove = actorRole.contains("king")
    val developingMinor =
      actorRole.exists(Set("knight", "bishop")) &&
        event.identity.actorFrom.exists(square =>
          square.lastOption.exists(rank => rank == '1' || rank == '8')
        )
    event.identity.goalTheme == PlanTheme.RestrictionProphylaxis &&
      event.opponentResourceDeterrence.isEmpty &&
      principalRestriction(restrictedEntries, restrictionSubjects) &&
      !kingMove &&
      !developingMinor

  def exactRootPawnBlockadeAuthority(
      event: PlanCausalEventEvidence,
      structural: StructuralDeltaEvidence,
      line: LineFactEvidence,
      consequence: TransitionConsequence
  ): Boolean =
    val exactSubjects =
      StructuralDeltaEvidence
        .exactRootOccupiedPawnAdvanceRestrictions(structural, consequence)
        .toSet
    val eventOwnsExactConsequence = event.directGoalConsequences.exists(owned =>
      owned.kind == consequence.kind &&
        owned.polarity == consequence.polarity &&
        owned.strength == consequence.strength &&
        exactSubjects.subsetOf(owned.subjects.toSet)
    )
    exactSubjects.nonEmpty &&
      eventOwnsExactConsequence &&
      event.rootTransition == structural.transition &&
      event.rootLine == line.line &&
      EvidenceRef.sameMove(event.rootMove, line.line.rootMove) &&
      rootMoveDirectlyRestrictsOpponent(event) &&
      directRestrictionSurvivesReply(event.rootLine, structural, Some(line))

  private def principalRestriction(
      restrictedEntries: List[(String, String, String)],
      restrictionSubjects: List[String]
  ): Boolean =
    val centralPawnBreak = restrictedEntries.exists { case (role, from, to) =>
      role == "pawn" && from.headOption == to.headOption && to.matches("[c-f][45]")
    }
    val directPawnBlockade =
      restrictionSubjects.exists(StructuralDeltaEvidence.directlyBlockedPawnAdvance)
    val majorPieceInvasion = restrictedEntries.exists { case (role, _, to) =>
      Set("rook", "queen")(role) && to.lastOption.exists(rank => rank == '2' || rank == '7')
    }
    val latentKnightRoute = restrictionSubjects.exists(subject =>
      StructuralDeltaEvidence.restrictedOpponentRoute(subject).exists(_.size >= 3)
    )
    centralPawnBreak || directPawnBlockade || majorPieceInvasion || latentKnightRoute

  def rootActorIsDevelopingMinor(
      rootLine: LineNodeRef,
      structural: StructuralDeltaEvidence
  ): Boolean =
    (for
      position <- _root_.chess.format.Fen.read(
        _root_.chess.variant.Standard,
        _root_.chess.format.Fen.Full(structural.transition.from.fen)
      )
      from <- Square.fromKey(EvidenceRef.normalizeMove(rootLine.rootMove).take(2))
      piece <- position.board.pieceAt(from)
    yield
      Set(Knight, Bishop)(piece.role) &&
        from.key.lastOption.contains(if piece.color.white then '1' else '8')
    ).getOrElse(false)
