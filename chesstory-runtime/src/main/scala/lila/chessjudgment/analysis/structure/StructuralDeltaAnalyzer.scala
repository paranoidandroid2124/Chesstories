package lila.chessjudgment.analysis.structure

import _root_.chess.{ Color, File, Pawn, Square }

import lila.chessjudgment.analysis.position.PositionAnalysis
import lila.chessjudgment.model.line.PrincipalVariationEvidence
import lila.chessjudgment.model.judgment.{
  CanonicalRelationDelta,
  CanonicalReplayTransition,
  CanonicalPositionRelationSnapshot,
  EvidenceFile,
  EvidencePieceRole,
  EvidenceSquare,
  RelationChangeDirection,
  RelationChangeKey,
  RelationFactKind,
  RelationSemanticChange,
  RelationSemanticDelta,
  RelationRayProjection,
  RelationWitnessDetail,
  StructuralSubject,
  StructuralSubjectBinding,
  StructuralSignal,
  StructuralSignalKind,
  TransitionConsequence,
  TransitionConsequenceKind,
  TransitionConsequenceRelationProof
}

private[chessjudgment] final case class TransitionStructuralDelta(
    perspective: Color,
    relationDelta: RelationSemanticDelta,
    pawnTopology: PawnTopologyDelta,
    fileOccupation: List[FileOccupant],
    batteryCreated: List[StructuralBatteryChange]
)

/** Evidence-stage binding of one replay-owned structural calculation to the
  * exact canonical relation occurrences that certify it. The board semantics
  * stay in `structural`; this value adds ownership, not a second calculation.
  */
private[chessjudgment] final case class CanonicalTransitionStructuralDelta private[chessjudgment] (
    structural: TransitionStructuralDelta,
    canonicalRelations: CanonicalRelationDelta
):
  export structural.{
    perspective,
    relationDelta,
    pawnTopology,
    fileOccupation,
    batteryCreated
  }

private[chessjudgment] final case class PawnFileStateChange(file: File, relationKeys: List[RelationChangeKey])

private[chessjudgment] final case class DirectedPawnTension(
    from: Square,
    to: Square,
    relationKey: RelationChangeKey
)

private[chessjudgment] enum PassedPawnAdvanceKind:
  case ExistingPasserAdvanced
  case PassedStatusCreated
  case Promoted

private[chessjudgment] final case class PassedPawnAdvance(
    from: Square,
    to: Square,
    kind: PassedPawnAdvanceKind,
    relationKeys: List[RelationChangeKey]
)

private[chessjudgment] final case class PassedPawnChange(square: Square, relationKey: RelationChangeKey)

private[chessjudgment] final case class PawnTopologyDelta(
    openedFiles: List[PawnFileStateChange],
    semiOpenedFiles: List[PawnFileStateChange],
    createdTensions: List[DirectedPawnTension],
    resolvedTensions: List[DirectedPawnTension],
    passedCreated: List[PassedPawnChange],
    passedAdvanced: List[PassedPawnAdvance],
    passedLost: List[PassedPawnChange]
)

private[chessjudgment] final case class FileOccupant(
    file: EvidenceFile,
    square: EvidenceSquare,
    role: EvidencePieceRole,
    relationKey: RelationChangeKey
):
  def subject: StructuralSubject = StructuralSubject.FileOccupation(file, square, role)

private[chessjudgment] final case class StructuralBatteryChange(
    detail: RelationWitnessDetail.RayBarrier,
    relationKey: RelationChangeKey
)

private object StructuralBatteryProjection:
  def subject(battery: StructuralBatteryChange): StructuralSubject =
    StructuralSubject.Battery(battery.detail)

  def targetSubjects(battery: StructuralBatteryChange): List[StructuralSubject] =
    RelationRayProjection.immediateTarget(battery.detail)
      .map(target => StructuralSubject.PieceAt(target.role, target.square))
      .toList

private[chessjudgment] object StructuralDeltaContracts:
  import StructuralSignalKind.*
  import TransitionConsequenceKind.*

  def signals(delta: TransitionStructuralDelta): List[StructuralSignal] =
    val topology = delta.pawnTopology
    List(
      signal(
        StructuralSignalKind.PawnTensionCreated,
        topology.createdTensions.size,
        topology.createdTensions.map(createdTensionSubject)
      ),
      signal(PawnTensionResolved, topology.resolvedTensions.size, topology.resolvedTensions.map(resolvedTensionSubject)),
      signal(PassedPawnCreated, topology.passedCreated.size, topology.passedCreated.map(change => passedCreatedSubject(delta.perspective, change.square))),
      signal(PassedPawnAdvanced, topology.passedAdvanced.size, topology.passedAdvanced.map(passedAdvanceSubject)),
      signal(BatteryCreated, delta.batteryCreated.size, delta.batteryCreated.map(StructuralBatteryProjection.subject))
    ).flatten

  def consequences(delta: TransitionStructuralDelta): List[TransitionConsequence] =
    val topology = delta.pawnTopology
    val baseConsequences =
      List(
        Option.when(topology.openedFiles.nonEmpty)(
          TransitionConsequence(
            OpenFileEstablished,
            topology.openedFiles.size,
            subjectBindings = exactRelationBindings(
              topology.openedFiles.map(change => fileSubject(change.file) -> change.relationKeys)
            )
          )
        ),
        Option.when(topology.semiOpenedFiles.nonEmpty)(
          TransitionConsequence(
            SemiOpenFileEstablished,
            topology.semiOpenedFiles.size,
            subjectBindings = exactRelationBindings(
              topology.semiOpenedFiles.map(change => semiOpenFileSubject(change.file) -> change.relationKeys)
            )
          )
        ),
        Option.when(topology.createdTensions.nonEmpty)(
          TransitionConsequence(
            TransitionConsequenceKind.PawnTensionCreated,
            topology.createdTensions.size,
            subjectBindings = exactRelationBindings(topology.createdTensions.flatMap(edge =>
              List(
                createdTensionSubject(edge) -> List(edge.relationKey),
                breakFileSubject(edge) -> List(edge.relationKey)
              )
            ))
          )
        ),
        Option.when(topology.resolvedTensions.nonEmpty)(
          TransitionConsequence(
            PawnTensionResolution,
            topology.resolvedTensions.size,
            subjectBindings = exactRelationBindings(topology.resolvedTensions.flatMap(edge =>
              List(
                resolvedTensionSubject(edge) -> List(edge.relationKey),
                breakFileSubject(edge) -> List(edge.relationKey)
              )
            ))
          )
        ),
        Option.when(delta.fileOccupation.nonEmpty)(
          TransitionConsequence(
            FileOccupationEstablished,
            delta.fileOccupation.size,
            subjectBindings = exactRelationBindings(
              delta.fileOccupation.map(occupant => occupant.subject -> List(occupant.relationKey))
            )
          )
        ),
        Option.when(topology.passedCreated.nonEmpty || topology.passedAdvanced.nonEmpty)(
          TransitionConsequence(
            PassedPawnProgress,
            topology.passedCreated.size + topology.passedAdvanced.size,
            subjectBindings = exactRelationBindings(
              topology.passedCreated.map(change =>
                passedCreatedSubject(delta.perspective, change.square) -> List(change.relationKey)
              ) ++ topology.passedAdvanced.map(advance =>
                passedAdvanceSubject(advance) -> advance.relationKeys
              )
            )
          )
        ),
        Option.when(topology.passedLost.nonEmpty)(
          TransitionConsequence(
            PassedPawnConcession,
            topology.passedLost.size,
            subjectBindings = exactRelationBindings(
              topology.passedLost.map(change =>
                passedLostSubject(delta.perspective, change.square) -> List(change.relationKey)
              )
            )
          )
        )
      ).flatten
    val consequences =
      baseConsequences ++
        delta.batteryCreated.map(battery =>
          TransitionConsequence(
            BatteryFormation,
            1,
            subjectBindings = exactRelationBindings(List(
              StructuralBatteryProjection.subject(battery) -> List(battery.relationKey)
            )),
            targetBindings = exactRelationBindings(
              StructuralBatteryProjection.targetSubjects(battery).map(subject =>
                subject -> List(battery.relationKey)
              )
            ),
          )
        )
    val semanticKeys = consequences.map(consequence =>
      (
        consequence.kind,
        consequence.strength,
        consequence.subjectBindings.map(_.stableKey).sorted.mkString(","),
        consequence.targetBindings.map(_.stableKey).sorted.mkString(",")
      )
    )
    require(semanticKeys.distinct.size == semanticKeys.size, "duplicate structural consequences")
    require(
      TransitionConsequenceRelationProof.provesSemantic(consequences, delta.relationDelta.changes),
      "structural consequences must bind every relation-derived result to its exact canonical relation changes"
    )
    consequences

  /** A single interpreted subject may be witnessed by several exact relation
    * changes in the same transition. Preserve every local foreign key on one
    * subject binding instead of dropping a witness or emitting duplicate
    * semantic subjects.
    */
  private def exactRelationBindings(
      entries: List[(StructuralSubject, List[RelationChangeKey])]
  ): List[StructuralSubjectBinding] =
    entries
      .groupMap(_._1)(_._2)
      .toList
      .sortBy(_._1.stableKey)
      .map { case (subject, keyGroups) =>
        StructuralSubjectBinding.fromRelations(subject, keyGroups.flatten)
      }

  private def createdTensionSubject(edge: DirectedPawnTension): StructuralSubject =
    StructuralSubject.PawnTensionCreated(EvidenceSquare(edge.from.key), EvidenceSquare(edge.to.key))

  private def resolvedTensionSubject(edge: DirectedPawnTension): StructuralSubject =
    StructuralSubject.PawnTensionResolved(EvidenceSquare(edge.from.key), EvidenceSquare(edge.to.key))

  private def breakFileSubject(edge: DirectedPawnTension): StructuralSubject =
    StructuralSubject.BreakFile(EvidenceFile(edge.from.key.take(1)))

  private def fileKey(file: File): String =
    (('a'.toInt + file.value).toChar).toString

  private def fileSubject(file: File): StructuralSubject =
    StructuralSubject.OpenFile(EvidenceFile(fileKey(file)))

  private def semiOpenFileSubject(file: File): StructuralSubject =
    StructuralSubject.SemiOpenFile(EvidenceFile(fileKey(file)))

  private def passedCreatedSubject(side: Color, square: Square): StructuralSubject =
    StructuralSubject.PassedPawnCreated(side, EvidenceSquare(square.key))

  private def passedLostSubject(side: Color, square: Square): StructuralSubject =
    StructuralSubject.PassedPawnLost(side, EvidenceSquare(square.key))

  private def passedAdvanceSubject(advance: PassedPawnAdvance): StructuralSubject =
    val side = advanceSide(advance)
    advance.kind match
      case PassedPawnAdvanceKind.Promoted =>
        StructuralSubject.PassedPawnPromoted(side, EvidenceSquare(advance.from.key), EvidenceSquare(advance.to.key))
      case PassedPawnAdvanceKind.ExistingPasserAdvanced =>
        StructuralSubject.PassedPawnAdvanced(
          side,
          EvidenceSquare(advance.from.key),
          EvidenceSquare(advance.to.key),
          relativeRank(advance.to, side)
        )
      case PassedPawnAdvanceKind.PassedStatusCreated =>
        StructuralSubject.PassedStatusCreated(
          side,
          EvidenceSquare(advance.from.key),
          EvidenceSquare(advance.to.key),
          relativeRank(advance.to, side)
        )

  private def advanceSide(advance: PassedPawnAdvance): Color =
    if advance.to.rank.value > advance.from.rank.value then Color.White else Color.Black

  private def relativeRank(square: Square, side: Color): Int =
    if side.white then square.rank.value + 1 else 8 - square.rank.value

  private def signal(
      kind: StructuralSignalKind,
      magnitude: Int,
      subjects: List[StructuralSubject]
  ): Option[StructuralSignal] =
    Option.when(magnitude > 0)(StructuralSignal(kind, magnitude, subjects.distinct))

private[chessjudgment] object StructuralDeltaAnalyzer:

  def delta(
      beforeAnalysis: PositionAnalysis,
      afterAnalysis: PositionAnalysis,
      legalStep: lila.chessjudgment.model.line.LegalReplayStep,
      relationDelta: RelationSemanticDelta
  ): TransitionStructuralDelta =
    val transitionFootprint = admittedFootprint(beforeAnalysis, afterAnalysis, legalStep)
    require(
      relationDelta.beforeInventory.sameOwner(beforeAnalysis.relationInventory) &&
        relationDelta.afterInventory.sameOwner(afterAnalysis.relationInventory),
      "a structural delta must consume the exact replay-owned relation delta"
    )
    assembleDelta(beforeAnalysis, afterAnalysis, legalStep, transitionFootprint, relationDelta)

  def bind(
      transition: CanonicalReplayTransition,
      canonicalRelations: CanonicalRelationDelta
  ): CanonicalTransitionStructuralDelta =
    val delta = transition.structuralDelta
    require(
      canonicalRelations.owns(delta.relationDelta),
      "a structural delta must reuse its exact canonical relation binding"
    )
    CanonicalTransitionStructuralDelta(
      structural = delta,
      canonicalRelations = canonicalRelations
    )

  private def admittedFootprint(
      beforeAnalysis: PositionAnalysis,
      afterAnalysis: PositionAnalysis,
      legalStep: lila.chessjudgment.model.line.LegalReplayStep
  ): lila.chessjudgment.model.position.BoardTransitionFootprint =
    val beforePosition = beforeAnalysis.position
    val afterPosition = afterAnalysis.position
    val beforeFen = chess.format.Fen.write(legalStep.before).value
    val afterFen = chess.format.Fen.write(legalStep.after).value
    require(
      PrincipalVariationEvidence.sameBoardState(chess.format.Fen.write(beforePosition).value, beforeFen) &&
        PrincipalVariationEvidence.sameBoardState(chess.format.Fen.write(afterPosition).value, afterFen),
      "structural delta analyses must reuse the admitted legal transition positions"
    )
    afterAnalysis.transitionFootprint.getOrElse(
      throw IllegalArgumentException("the destination analysis must be produced from its admitted legal transition")
    )

  private def assembleDelta(
      beforeAnalysis: PositionAnalysis,
      afterAnalysis: PositionAnalysis,
      legalStep: lila.chessjudgment.model.line.LegalReplayStep,
      transitionFootprint: lila.chessjudgment.model.position.BoardTransitionFootprint,
      relationDelta: RelationSemanticDelta
  ): TransitionStructuralDelta =
    val side = legalStep.move.piece.color
    val createdTension = pawnTensions(relationDelta.establishedOf(RelationFactKind.PawnTension), side)
    val resolvedTension = pawnTensions(relationDelta.removedOf(RelationFactKind.PawnTension), side)
    val passedPawnDelta = passedPawnDeltaFor(relationDelta, transitionFootprint, side)
    val affectedPawnFiles = transitionFootprint.pawnFileChanges.map(_._2)
    val pawnFileDelta = pawnFileDeltaFor(
      relationDelta,
      beforeAnalysis,
      afterAnalysis,
      side,
      affectedPawnFiles
    )
    TransitionStructuralDelta(
        perspective = side,
        relationDelta = relationDelta,
        pawnTopology = PawnTopologyDelta(
          openedFiles = pawnFileDelta.opened,
          semiOpenedFiles = pawnFileDelta.semiOpened,
          createdTensions = createdTension,
          resolvedTensions = resolvedTension,
          passedCreated = passedPawnDelta.created,
          passedAdvanced = passedPawnDelta.advanced,
          passedLost = passedPawnDelta.lost
        ),
        fileOccupation = fileOccupationEstablishedFromRelations(relationDelta, side),
        batteryCreated = batteryRelations(relationDelta, side)
          .sortBy(battery => StructuralBatteryProjection.subject(battery).label)
      )

  private final case class PawnFileTransitionDelta(
      opened: List[PawnFileStateChange],
      semiOpened: List[PawnFileStateChange]
  )

  private def pawnFileDeltaFor(
      relationDelta: RelationSemanticDelta,
      before: PositionAnalysis,
      after: PositionAnalysis,
      side: Color,
      affectedFiles: Set[File]
  ): PawnFileTransitionDelta =
    val changesByFile = relationDelta.ofKind(RelationFactKind.PawnFileGroup).flatMap { change =>
      change.detail match
        case RelationWitnessDetail.PawnFileGroup(_, file, _) =>
          List(file.key.toLowerCase -> change.key)
        case _ => Nil
    }.groupMap(_._1)(_._2)
    val transitions = affectedFiles.toList.sortBy(_.value).map { file =>
      val keys = changesByFile.getOrElse(boardFileKey(file), Nil).distinct.sortBy(_.stableKey)
      val beforeState = before.pawnTopology.fileState(file)
      val afterState = after.pawnTopology.fileState(file)
      val change = PawnFileStateChange(file, keys)
      (
        Option.when(keys.nonEmpty && !beforeState.isOpen && afterState.isOpen)(change),
        Option.when(
          keys.nonEmpty && !beforeState.isSemiOpenFor(side) && afterState.isSemiOpenFor(side)
        )(change)
      )
    }
    PawnFileTransitionDelta(
      opened = transitions.flatMap(_._1),
      semiOpened = transitions.flatMap(_._2)
    )

  private final case class PassedPawnTransitionDelta(
      created: List[PassedPawnChange],
      advanced: List[PassedPawnAdvance],
      lost: List[PassedPawnChange]
  )

  private final case class PassedPawnState(passed: Boolean, relationKey: RelationChangeKey)

  private def passedPawnDeltaFor(
      relationDelta: RelationSemanticDelta,
      transitionFootprint: lila.chessjudgment.model.position.BoardTransitionFootprint,
      side: Color
  ): PassedPawnTransitionDelta =
    val removedPassages = passedStates(relationDelta.removedOf(RelationFactKind.PawnPassage), side)
    val establishedPassages = passedStates(relationDelta.establishedOf(RelationFactKind.PawnPassage), side)
    val beforePassers = removedPassages.collect { case (square, state) if state.passed => square.key }.toSet
    val afterPassers = establishedPassages.collect { case (square, state) if state.passed => square.key }.toSet
    val advanced = passedPawnAdvance(
      transitionFootprint,
      side,
      removedPassages,
      establishedPassages
    )
    val advancedFrom = advanced.map(_.from).toSet
    val advancedTo = advanced.map(_.to).toSet
    PassedPawnTransitionDelta(
      created = establishedPassages.collect {
        case (square, state) if state.passed && !beforePassers(square.key) && !advancedTo(square) =>
          PassedPawnChange(square, state.relationKey)
      }.toList.sortBy(_.square.key),
      advanced = advanced.toList,
      lost = removedPassages.collect {
        case (square, state) if state.passed && !afterPassers(square.key) && !advancedFrom(square) =>
          PassedPawnChange(square, state.relationKey)
      }.toList.sortBy(_.square.key)
    )

  private def passedStates(
      changes: List[RelationSemanticChange],
      side: Color
  ): Map[Square, PassedPawnState] =
    val states = changes.flatMap(change =>
      change.detail match
        case RelationWitnessDetail.PawnPassage(owner, pawn, blockers) if owner == side =>
          squareAt(pawn.key).map(_ -> PassedPawnState(blockers.isEmpty, change.key)).toList
        case _ => Nil
    )
    states.foldLeft(Map.empty[Square, PassedPawnState]) { case (inventory, (square, state)) =>
      require(!inventory.contains(square), "a relation delta cannot repeat one pawn-passage state")
      inventory.updated(square, state)
    }

  private def pawnTensions(
      changes: List[RelationSemanticChange],
      side: Color
  ): List[DirectedPawnTension] =
    changes.flatMap(change =>
      change.detail match
        case RelationWitnessDetail.PawnTension(whitePawn, blackPawn) =>
          for
            white <- squareAt(whitePawn.key).toList
            black <- squareAt(blackPawn.key).toList
          yield
            if side.white then DirectedPawnTension(white, black, change.key)
            else DirectedPawnTension(black, white, change.key)
        case _ => Nil
    ).distinct.sortBy(edge => (edge.from.key, edge.to.key))

  private def passedPawnAdvance(
      transitionFootprint: lila.chessjudgment.model.position.BoardTransitionFootprint,
      side: Color,
      removedPassages: Map[Square, PassedPawnState],
      establishedPassages: Map[Square, PassedPawnState]
  ): Option[PassedPawnAdvance] =
    transitionFootprint.pieceTransitions
      .find(movement => movement.side == side && movement.beforeRole == Pawn)
      .flatMap { movement =>
        val beforeState = removedPassages.get(movement.from)
        val afterState = establishedPassages.get(movement.to)
        if movement.afterRole != Pawn && beforeState.exists(_.passed) then
          Some(
            PassedPawnAdvance(
              movement.from,
              movement.to,
              PassedPawnAdvanceKind.Promoted,
              beforeState.toList.map(_.relationKey)
            )
          )
        else if
          movement.afterRole == Pawn && afterState.exists(_.passed) &&
            relativeRank(movement.to, side) > relativeRank(movement.from, side)
        then
          val kind =
            if beforeState.exists(_.passed) then PassedPawnAdvanceKind.ExistingPasserAdvanced
            else PassedPawnAdvanceKind.PassedStatusCreated
          Some(
            PassedPawnAdvance(
              movement.from,
              movement.to,
              kind,
              (beforeState.toList ++ afterState.toList).map(_.relationKey).distinct.sortBy(_.stableKey)
            )
          )
        else None
      }

  private def batteryRelations(
      relationDelta: RelationSemanticDelta,
      side: Color
  ): List[StructuralBatteryChange] =
    relationDelta.newlyEstablishedNamedRays.flatMap { change =>
      change.detail match
        case battery: RelationWitnessDetail.RayBarrier
            if battery.side == side && RelationRayProjection.batteryFormation(battery).nonEmpty =>
          List(StructuralBatteryChange(battery, change.key))
        case _ => Nil
    }

  private def fileOccupationEstablishedFromRelations(
      relationDelta: RelationSemanticDelta,
      side: Color
  ): List[FileOccupant] =
    val changedSquares = relationDelta.changedSquares.map(_.key.toLowerCase).toSet
    val before = majorFileOccupants(
      relationDelta.removedOf(RelationFactKind.MajorPieceFileOccupancy),
      side
    ).map(fileOccupantIdentity).toSet
    majorFileOccupants(relationDelta.establishedOf(RelationFactKind.MajorPieceFileOccupancy), side)
      .filterNot(occupant => before(fileOccupantIdentity(occupant)))
      .filter(occupant => changedSquares(occupant.square.key.toLowerCase))
      .toList
      .sortBy(occupant => (occupant.file.key, occupant.square.key, occupant.role.name))

  private def majorFileOccupants(
      changes: List[RelationSemanticChange],
      side: Color
  ): List[FileOccupant] =
    changes.flatMap(change =>
      change.detail match
        case RelationWitnessDetail.MajorPieceFileOccupancy(owner, file, occupants, _) if owner == side =>
          occupants.map(occupant =>
            FileOccupant(
              EvidenceFile(file.key.toLowerCase),
              EvidenceSquare(occupant.square.key.toLowerCase),
              EvidencePieceRole(occupant.role.name.toLowerCase),
              change.key
            )
          )
        case _ => Nil
    )

  private def fileOccupantIdentity(occupant: FileOccupant): (String, String, String) =
    (
      occupant.file.key.toLowerCase,
      occupant.square.key.toLowerCase,
      occupant.role.name.toLowerCase
    )

  private def relativeRank(square: Square, side: Color): Int =
    if side.white then square.rank.value + 1 else 8 - square.rank.value


  private def squareAt(key: String): Option[Square] =
    Square.fromKey(key.toLowerCase)

  private def boardFileKey(file: File): String =
    (('a'.toInt + file.value).toChar).toString
