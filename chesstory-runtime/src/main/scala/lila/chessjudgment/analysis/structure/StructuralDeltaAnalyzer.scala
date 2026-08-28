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
  RelationBatteryFormationWitness,
  RelationChangeDirection,
  RelationChangeKey,
  RelationColoredPieceWitness,
  RelationControlTarget,
  RelationFactEvidence,
  RelationFactKind,
  DerivedRelationResultKey,
  RelationPawnTopologyStateWitness,
  RelationSemanticDelta,
  RelationRayProjection,
  RelationWitnessDetail,
  StructuralSubject,
  StructuralSubjectBinding,
  StructuralSignal,
  StructuralSignalKind,
  TransitionConsequence,
  TransitionConsequenceKind,
  TransitionConsequenceRelationProof,
  VerticalRelationContracts
}

private[chessjudgment] final case class TransitionStructuralDelta(
    perspective: Color,
    relationDelta: RelationSemanticDelta,
    derivedRelations: List[RelationFactEvidence],
    pawnTopology: PawnTopologyDelta,
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
    derivedRelations,
    pawnTopology,
    batteryCreated
  }

private[chessjudgment] final case class PawnFileStateChange(file: File, relationKeys: List[RelationChangeKey])

private[chessjudgment] final case class DirectedPawnTension(
    side: Color,
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

private[chessjudgment] final case class PassedPawnChange(
    square: Square,
    relationKeys: List[RelationChangeKey]
):
  require(relationKeys.nonEmpty, "a passed-pawn state change needs exact passage relations")
  require(
    relationKeys.distinct.size == relationKeys.size,
    "a passed-pawn state change cannot repeat one passage relation"
  )

private[chessjudgment] final case class PawnTopologyDelta(
    openedFiles: List[PawnFileStateChange],
    semiOpenedFiles: List[PawnFileStateChange],
    createdTensions: List[DirectedPawnTension],
    resolvedTensions: List[DirectedPawnTension],
    passedCreated: List[PassedPawnChange],
    passedAdvanced: List[PassedPawnAdvance],
    passedLost: List[PassedPawnChange]
)

private final case class ClosedPawnSemanticTransition(
    createdTensions: List[DirectedPawnTension],
    resolvedTensions: List[DirectedPawnTension],
    passedCreated: List[PassedPawnChange],
    passedAdvanced: List[PassedPawnAdvance],
    passedLost: List[PassedPawnChange]
)

private final case class ClosedPawnStateTransition(
    relation: RelationFactEvidence,
    before: Option[RelationPawnTopologyStateWitness],
    after: Option[RelationPawnTopologyStateWitness]
)

private[chessjudgment] final case class StructuralBatteryChange(
    formation: RelationBatteryFormationWitness,
    relationKeys: List[RelationChangeKey],
    targetRelations: List[(RelationColoredPieceWitness, RelationChangeKey)]
):
  require(relationKeys.nonEmpty, "a battery formation needs an exact relation proof")
  require(
    relationKeys.distinct.size == relationKeys.size,
    "one battery formation cannot repeat a relation proof key"
  )

private object StructuralBatteryProjection:
  def subject(battery: StructuralBatteryChange): StructuralSubject =
    StructuralSubject.Battery(battery.formation)

  def targetBindings(
      battery: StructuralBatteryChange
  ): List[(StructuralSubject, List[RelationChangeKey])] =
    battery.targetRelations.map { case (target, relationKey) =>
      StructuralSubject.PieceAt(target.side, target.role, target.square) -> List(relationKey)
    }

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
    val controlSetBindings = exactDerivedRelationBindings(
      delta.derivedRelations
        .filter(_.kind == RelationFactKind.GeometricControlSetDelta)
        .map { relation =>
          val subject = StructuralSubject.fromGeometricControlSetDelta(relation).getOrElse(
            throw IllegalArgumentException("a closed control-set result lost its exact control delta")
          )
          subject -> List(DerivedRelationResultKey.from(relation))
        }
    )
    val sliderReachBindings = exactDerivedRelationBindings(
      delta.derivedRelations
        .filter(_.kind == RelationFactKind.SliderReachDelta)
        .map { relation =>
          val subject = StructuralSubject.fromSliderReachDelta(relation).getOrElse(
            throw IllegalArgumentException("a closed slider-reach result lost its exact reach delta")
          )
          subject -> List(DerivedRelationResultKey.from(relation))
        }
    )
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
                topology.semiOpenedFiles.map(change => semiOpenFileSubject(delta.perspective, change.file) -> change.relationKeys)
            )
          )
        ),
        Option.when(topology.createdTensions.nonEmpty)(
          TransitionConsequence(
            TransitionConsequenceKind.PawnTensionCreated,
            topology.createdTensions.size,
            subjectBindings = exactRelationBindings(topology.createdTensions.map(edge =>
              createdTensionSubject(edge) -> List(edge.relationKey)
            ))
          )
        ),
        Option.when(topology.resolvedTensions.nonEmpty)(
          TransitionConsequence(
            PawnTensionResolution,
            topology.resolvedTensions.size,
            subjectBindings = exactRelationBindings(topology.resolvedTensions.map(edge =>
              resolvedTensionSubject(edge) -> List(edge.relationKey)
            ))
          )
        ),
        Option.when(controlSetBindings.nonEmpty)(
          TransitionConsequence(
            GeometricControlSetChanged,
            controlSetBindings.size,
            subjectBindings = controlSetBindings
          )
        ),
        Option.when(sliderReachBindings.nonEmpty)(
          TransitionConsequence(
            SliderReachChanged,
            sliderReachBindings.size,
            subjectBindings = sliderReachBindings
          )
        ),
        Option.when(topology.passedCreated.nonEmpty || topology.passedAdvanced.nonEmpty)(
          TransitionConsequence(
            PassedPawnProgress,
            topology.passedCreated.size + topology.passedAdvanced.size,
            subjectBindings = exactRelationBindings(
              topology.passedCreated.map(change =>
                passedCreatedSubject(delta.perspective, change.square) -> change.relationKeys
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
                passedLostSubject(delta.perspective, change.square) -> change.relationKeys
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
              StructuralBatteryProjection.subject(battery) -> battery.relationKeys
            )),
            targetBindings = exactRelationBindings(
              StructuralBatteryProjection.targetBindings(battery)
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
      TransitionConsequenceRelationProof.provesSemantic(
        consequences,
        delta.relationDelta,
        delta.derivedRelations
      ),
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
        val keys = keyGroups.flatten
        require(
          keys.distinct.size == keys.size,
          s"structural subject '${subject.stableKey}' repeated one relation proof key"
        )
        StructuralSubjectBinding.fromRelations(subject, keys)
      }

  private def exactDerivedRelationBindings(
      entries: List[(StructuralSubject, List[DerivedRelationResultKey])]
  ): List[StructuralSubjectBinding] =
    entries
      .groupMap(_._1)(_._2)
      .toList
      .sortBy(_._1.stableKey)
      .map { case (subject, keyGroups) =>
        val keys = keyGroups.flatten
        require(
          keys.distinct.size == keys.size,
          s"structural subject '${subject.stableKey}' repeated one derived result key"
        )
        StructuralSubjectBinding.fromDerivedRelations(subject, keys)
      }

  private def createdTensionSubject(edge: DirectedPawnTension): StructuralSubject =
    StructuralSubject.PawnTensionCreated(edge.side, EvidenceSquare(edge.from.key), EvidenceSquare(edge.to.key))

  private def resolvedTensionSubject(edge: DirectedPawnTension): StructuralSubject =
    StructuralSubject.PawnTensionResolved(edge.side, EvidenceSquare(edge.from.key), EvidenceSquare(edge.to.key))

  private def fileKey(file: File): String =
    (('a'.toInt + file.value).toChar).toString

  private def fileSubject(file: File): StructuralSubject =
    StructuralSubject.OpenFile(EvidenceFile(fileKey(file)))

  private def semiOpenFileSubject(side: Color, file: File): StructuralSubject =
    StructuralSubject.SemiOpenFile(side, EvidenceFile(fileKey(file)))

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
    require(subjects.distinct.size == subjects.size, s"$kind repeated one structural subject")
    Option.when(magnitude > 0)(StructuralSignal(kind, magnitude, subjects))

private[chessjudgment] object StructuralDeltaAnalyzer:

  def delta(
      beforeAnalysis: PositionAnalysis,
      afterAnalysis: PositionAnalysis,
      legalStep: lila.chessjudgment.model.line.LegalReplayStep,
      relationDelta: RelationSemanticDelta,
      controlSetTransitions: List[RelationFactEvidence],
      namedRayTransitions: List[RelationFactEvidence],
      pawnTopologyTransitions: List[RelationFactEvidence],
      sliderReachTransitions: List[RelationFactEvidence]
  ): TransitionStructuralDelta =
    val transitionFootprint = admittedFootprint(beforeAnalysis, afterAnalysis, legalStep)
    require(
      relationDelta.beforeInventory.sameOwner(beforeAnalysis.relationInventory) &&
        relationDelta.afterInventory.sameOwner(afterAnalysis.relationInventory),
      "a structural delta must consume the exact replay-owned relation delta"
    )
    assembleDelta(
      beforeAnalysis,
      afterAnalysis,
      legalStep,
      transitionFootprint,
      relationDelta,
      controlSetTransitions,
      namedRayTransitions,
      pawnTopologyTransitions,
      sliderReachTransitions
    )

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
      relationDelta: RelationSemanticDelta,
      controlSetTransitions: List[RelationFactEvidence],
      namedRayTransitions: List[RelationFactEvidence],
      pawnTopologyTransitions: List[RelationFactEvidence],
      sliderReachTransitions: List[RelationFactEvidence]
  ): TransitionStructuralDelta =
    val side = legalStep.move.piece.color
    val verticalPawnDelta = pawnSemanticTransitionFrom(
      pawnTopologyTransitions,
      relationDelta,
      transitionFootprint,
      side
    )
    val affectedPawnFiles = transitionFootprint.pawnFileChanges.map(_._2)
    val pawnFileDelta = pawnFileDeltaFor(
      relationDelta,
      side,
      affectedPawnFiles
    )
    require(
      controlSetTransitions.forall(relation =>
        relation.kind == RelationFactKind.GeometricControlSetDelta &&
          relation.mentionsLineMove(relationDelta.moveUci)
      ),
      "a structural control-set projection accepts only the closed root-owned transition inventory"
    )
    require(
      sliderReachTransitions.forall(relation =>
        relation.kind == RelationFactKind.SliderReachDelta &&
          relation.mentionsLineMove(relationDelta.moveUci)
      ),
      "a structural slider-reach projection accepts only the closed root-owned L1 inventory"
    )
    val consumedDerivedRelations = controlSetTransitions ++ sliderReachTransitions
    val consumedDerivedKeys = consumedDerivedRelations.map(DerivedRelationResultKey.from)
    require(
      consumedDerivedKeys.distinct.size == consumedDerivedKeys.size,
      "a structural delta cannot repeat one derived result"
    )
    TransitionStructuralDelta(
        perspective = side,
        relationDelta = relationDelta,
        derivedRelations = consumedDerivedRelations.sortBy(relation => DerivedRelationResultKey.from(relation).stableKey),
        pawnTopology = PawnTopologyDelta(
          openedFiles = pawnFileDelta.opened,
          semiOpenedFiles = pawnFileDelta.semiOpened,
          createdTensions = verticalPawnDelta.createdTensions,
          resolvedTensions = verticalPawnDelta.resolvedTensions,
          passedCreated = verticalPawnDelta.passedCreated,
          passedAdvanced = verticalPawnDelta.passedAdvanced,
          passedLost = verticalPawnDelta.passedLost
        ),
        batteryCreated = batteryRelations(relationDelta, namedRayTransitions, side)
          .sortBy(battery => StructuralBatteryProjection.subject(battery).label)
      )

  private final case class PawnFileTransitionDelta(
      opened: List[PawnFileStateChange],
      semiOpened: List[PawnFileStateChange]
  )

  private def pawnFileDeltaFor(
      relationDelta: RelationSemanticDelta,
      side: Color,
      affectedFiles: Set[File]
  ): PawnFileTransitionDelta =
    if affectedFiles.isEmpty then PawnFileTransitionDelta(Nil, Nil)
    else
      val changesByFile = relationDelta.ofKind(RelationFactKind.PawnFileGroup).flatMap { change =>
        change.detail match
          case RelationWitnessDetail.PawnFileGroup(_, file, _) =>
            List(file.key.toLowerCase -> change.key)
          case _ => Nil
      }.groupMap(_._1)(_._2)
      def indexedFiles(
          values: List[lila.chessjudgment.analysis.position.PositionRelationExtractor.ClosedPawnFile],
          occurrence: String
      ) =
        val entries = values.map(file => file.file.key -> file)
        require(
          entries.map(_._1).distinct.size == entries.size,
          s"closed pawn-file facet repeated one $occurrence file"
        )
        entries.toMap
      val beforeFiles = indexedFiles(
        relationDelta.beforeInventory.pawnFiles(affectedFiles),
        "before"
      )
      val afterFiles = indexedFiles(
        relationDelta.afterInventory.pawnFiles(affectedFiles),
        "after"
      )
      val transitions = affectedFiles.toList.sortBy(_.value).map { file =>
        val unsortedKeys = changesByFile.getOrElse(boardFileKey(file), Nil)
        require(
          unsortedKeys.distinct.size == unsortedKeys.size,
          s"pawn-file relation inventory repeated ${boardFileKey(file)}"
        )
        val keys = unsortedKeys.sortBy(_.stableKey)
        val fileKey = boardFileKey(file)
        val beforeState = beforeFiles.getOrElse(fileKey, throw IllegalArgumentException(s"closed pawn topology lost file '$fileKey' before the move"))
        val afterState = afterFiles.getOrElse(fileKey, throw IllegalArgumentException(s"closed pawn topology lost file '$fileKey' after the move"))
        val change = PawnFileStateChange(file, keys)
        (
          Option.when(keys.nonEmpty && !beforeState.isOpen && afterState.isOpen)(change),
          Option.when(
            keys.nonEmpty && !beforeState.semiOpenFor(side) && afterState.semiOpenFor(side)
          )(change)
        )
      }
      PawnFileTransitionDelta(
        opened = transitions.flatMap(_._1),
        semiOpened = transitions.flatMap(_._2)
      )

  /** User-facing structural projections consume the already-closed L1 pawn
    * transition. L0 changes below are looked up only as occurrence proof keys;
    * they no longer run a second pawn-state interpretation.
    */
  private def pawnSemanticTransitionFrom(
      transitions: List[RelationFactEvidence],
      relationDelta: RelationSemanticDelta,
      transitionFootprint: lila.chessjudgment.model.position.BoardTransitionFootprint,
      side: Color
  ): ClosedPawnSemanticTransition =
    require(
      transitions.forall(relation =>
        relation.kind == RelationFactKind.PawnTopologyTransition &&
          relation.mentionsLineMove(relationDelta.moveUci)
      ),
      "a structural pawn projection accepts only the closed root-owned L1 inventory"
    )
    val states = transitions.map(relation => relation.detail match
      case RelationWitnessDetail.PawnTopologyTransition(_, before, after, _, _) =>
        ClosedPawnStateTransition(relation, before, after)
      case _ => throw IllegalArgumentException("L1 pawn topology inventory changed relation kind")
    ).filter(state => state.before.orElse(state.after).exists(_.side == side))
    require(
      states.map(state => state.before.map(_.square).orElse(state.after.map(_.square))).distinct.size == states.size,
      "the closed L1 pawn inventory repeated one pawn identity"
    )

    def exactSquare(square: EvidenceSquare): Square =
      squareAt(square.key).getOrElse(
        throw IllegalArgumentException(s"invalid L1 pawn square '${square.key}'")
      )

    def exactChange(
        state: ClosedPawnStateTransition,
        direction: RelationChangeDirection,
        kind: RelationFactKind,
        label: String
    )(matches: RelationWitnessDetail => Boolean): RelationChangeKey =
      val admittedSourceIds = VerticalRelationContracts.proofOf(state.relation.detail)
        .toList
        .flatMap(_.sourceSemanticIds)
        .toSet
      val candidates = (direction match
        case RelationChangeDirection.Removed => relationDelta.removedOf(kind)
        case RelationChangeDirection.Established => relationDelta.establishedOf(kind)
      ).filter(change => admittedSourceIds(change.semanticId) && matches(change.detail))
      candidates match
        case change :: Nil => change.key
        case found =>
          throw IllegalArgumentException(
            s"L1 pawn transition '$label' needs one exact L0 occurrence source, found ${found.size}"
          )

    def tensionKey(
        state: ClosedPawnStateTransition,
        direction: RelationChangeDirection,
        from: EvidenceSquare,
        to: EvidenceSquare
    ): RelationChangeKey =
      exactChange(state, direction, RelationFactKind.GeometricControl, s"${from.key}-${to.key}") {
        case RelationWitnessDetail.GeometricControl(
              Color.White,
              whitePawn,
              whiteRole,
              blackPawn,
              RelationControlTarget.Enemy(blackRole)
            ) =>
          whiteRole.name.equalsIgnoreCase(Pawn.name) && blackRole.name.equalsIgnoreCase(Pawn.name) &&
            (if side.white then whitePawn == from && blackPawn == to
             else blackPawn == from && whitePawn == to)
        case _ => false
      }

    def passageKey(
        state: ClosedPawnStateTransition,
        direction: RelationChangeDirection,
        pawn: EvidenceSquare
    ): RelationChangeKey =
      exactChange(state, direction, RelationFactKind.PawnPassage, pawn.key) {
        case RelationWitnessDetail.PawnPassage(owner, exactPawn, _) => owner == side && exactPawn == pawn
        case _ => false
      }

    val createdTensions = states.flatMap {
      case state @ ClosedPawnStateTransition(_, before, Some(after)) =>
        val previous = before.map(_.enemyPawnContacts.toSet).getOrElse(Set.empty)
        (after.enemyPawnContacts.toSet -- previous).toList.map { target =>
          DirectedPawnTension(
            side,
            exactSquare(after.square),
            exactSquare(target),
            tensionKey(state, RelationChangeDirection.Established, after.square, target)
          )
        }
      case _ => Nil
    }.sortBy(edge => edge.from.key -> edge.to.key)
    val resolvedTensions = states.flatMap {
      case state @ ClosedPawnStateTransition(_, Some(before), after) =>
        val remaining = after.map(_.enemyPawnContacts.toSet).getOrElse(Set.empty)
        (before.enemyPawnContacts.toSet -- remaining).toList.map { target =>
          DirectedPawnTension(
            side,
            exactSquare(before.square),
            exactSquare(target),
            tensionKey(state, RelationChangeDirection.Removed, before.square, target)
          )
        }
      case _ => Nil
    }.sortBy(edge => edge.from.key -> edge.to.key)

    val passedCreated = states.flatMap {
      case state @ ClosedPawnStateTransition(_, Some(before), Some(after))
          if before.square == after.square && !before.passed && after.passed =>
        List(PassedPawnChange(
          exactSquare(after.square),
          List(
            passageKey(state, RelationChangeDirection.Removed, before.square),
            passageKey(state, RelationChangeDirection.Established, after.square)
          ).sortBy(_.stableKey)
        ))
      case _ => Nil
    }.sortBy(_.square.key)
    val passedLost = states.flatMap {
      case state @ ClosedPawnStateTransition(_, Some(before), Some(after))
          if before.passed && !after.passed =>
        List(PassedPawnChange(
          exactSquare(after.square),
          List(
            passageKey(state, RelationChangeDirection.Removed, before.square),
            passageKey(state, RelationChangeDirection.Established, after.square)
          ).sortBy(_.stableKey)
        ))
      case _ => Nil
    }.sortBy(_.square.key)
    val passedAdvanced = states.flatMap {
      case state @ ClosedPawnStateTransition(_, Some(before), Some(after))
          if before.square != after.square && after.passed =>
        val from = exactSquare(before.square)
        val to = exactSquare(after.square)
        val movement = transitionFootprint.pieceTransitions.find(value =>
          value.side == side && value.beforeRole == Pawn && value.from == from && value.to == to
        )
        movement.toList.filter(value =>
          value.afterRole == Pawn && relativeRank(to, side) > relativeRank(from, side)
        ).map { _ =>
          val relationKeys = List(
            passageKey(state, RelationChangeDirection.Removed, before.square),
            passageKey(state, RelationChangeDirection.Established, after.square)
          )
          require(
            relationKeys.head != relationKeys.last,
            "a passed-pawn advance must retain distinct before and after occurrence proofs"
          )
          PassedPawnAdvance(
            from,
            to,
            if before.passed then PassedPawnAdvanceKind.ExistingPasserAdvanced
            else PassedPawnAdvanceKind.PassedStatusCreated,
            relationKeys.sortBy(_.stableKey)
          )
        }
      case state @ ClosedPawnStateTransition(_, Some(before), None) if before.passed =>
        val from = exactSquare(before.square)
        transitionFootprint.pieceTransitions.find(value =>
          value.side == side && value.beforeRole == Pawn && value.afterRole != Pawn && value.from == from
        ).toList.map(movement => PassedPawnAdvance(
          movement.from,
          movement.to,
          PassedPawnAdvanceKind.Promoted,
          List(passageKey(state, RelationChangeDirection.Removed, before.square))
        ))
      case _ => Nil
    }.sortBy(advance => advance.from.key -> advance.to.key)

    require(
      createdTensions.distinct.size == createdTensions.size &&
        resolvedTensions.distinct.size == resolvedTensions.size &&
        passedCreated.distinct.size == passedCreated.size &&
        passedAdvanced.distinct.size == passedAdvanced.size &&
        passedLost.distinct.size == passedLost.size,
      "the L1 pawn projection repeated one structural result"
    )
    ClosedPawnSemanticTransition(
      createdTensions = createdTensions,
      resolvedTensions = resolvedTensions,
      passedCreated = passedCreated,
      passedAdvanced = passedAdvanced,
      passedLost = passedLost
    )

  private def batteryRelations(
      relationDelta: RelationSemanticDelta,
      namedRayTransitions: List[RelationFactEvidence],
      side: Color
  ): List[StructuralBatteryChange] =
    require(
      namedRayTransitions.forall(_.kind == RelationFactKind.NamedRayTransition),
      "a structural battery projection accepts only certified named-ray transitions"
    )
    val projected = namedRayTransitions
      .flatMap { relation =>
        relation.detail match
          case named @ RelationWitnessDetail.NamedRayTransition(_, owner, _, _, _, target, _, pattern, direction, proof)
              if owner == side && pattern == lila.chessjudgment.model.judgment.RelationRayPattern.Battery &&
                direction == RelationChangeDirection.Established =>
            val sourceChanges = proof.premises.flatMap(premise =>
              Option.when(
                premise.occurrence == lila.chessjudgment.model.judgment.RelationPremiseOccurrence.Established &&
                  premise.kind == RelationFactKind.RayBarrier
              )(relationDelta.changeBySemanticId(premise.semanticId)).flatten
            )
            val sourceChange = sourceChanges match
              case only :: Nil => only
              case found =>
                throw IllegalArgumentException(
                  s"a named battery transition must retain exactly one established RayBarrier source, found ${found.size}"
                )
            RelationRayProjection.batteryFormation(named).map(formation =>
              (formation, target, sourceChange.key)
            ).toList
          case _ => Nil
      }
    projected
      .groupMap(_._1)(entry => entry._2 -> entry._3)
      .toList
      .map { case (formation, entries) =>
        val relationKeys = entries.map(_._2)
        require(
          relationKeys.distinct.size == relationKeys.size,
          "one battery formation cannot hide duplicate named-ray proof keys"
        )
        val targetRelations = entries.collect { case (Some(target), key) => target -> key }.sortBy {
          case (target, key) =>
            s"${target.side}:${target.square.key}:${target.role.name}" -> key.stableKey
        }
        StructuralBatteryChange(
          formation,
          relationKeys.sortBy(_.stableKey),
          targetRelations
        )
      }
      .sortBy(change => change.formation.firstSlider.square.key -> change.formation.secondSlider.square.key)

  private def relativeRank(square: Square, side: Color): Int =
    if side.white then square.rank.value + 1 else 8 - square.rank.value


  private def squareAt(key: String): Option[Square] =
    Square.fromKey(key.toLowerCase)

  private def boardFileKey(file: File): String =
    (('a'.toInt + file.value).toChar).toString
