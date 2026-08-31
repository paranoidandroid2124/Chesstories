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
  DerivedRelationResultKey,
  RelationChangeDirection,
  RelationChangeKey,
  RelationControlTarget,
  RelationFactEvidence,
  RelationFactKind,
  RelationPawnTopologyStateWitness,
  RelationSemanticChange,
  RelationSemanticDelta,
  RelationWitnessDetail,
  StructuralSubject,
  StructuralSubjectBinding,
  TransitionConsequence,
  TransitionConsequenceKind,
  VerticalRelationContracts
}

private[chessjudgment] final case class TransitionStructuralDelta(
    perspective: Color,
    relationDelta: RelationSemanticDelta,
    pawnTopology: PawnTopologyDelta
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
    pawnTopology
  }

private[chessjudgment] final case class PawnFileStateChange(file: File, relationKeys: List[RelationChangeKey])

private[chessjudgment] final case class DirectedPawnTension(
    side: Color,
    from: Square,
    to: Square,
    relationKey: RelationChangeKey,
    resultKey: DerivedRelationResultKey
)

private[chessjudgment] enum PassedPawnAdvanceKind:
  case ExistingPasserAdvanced
  case PassedStatusCreated
  case Promoted

private[chessjudgment] final case class PassedPawnAdvance(
    from: Square,
    to: Square,
    kind: PassedPawnAdvanceKind,
    relationKeys: List[RelationChangeKey],
    resultKey: DerivedRelationResultKey
)

private[chessjudgment] final case class PassedPawnChange(
    square: Square,
    relationKeys: List[RelationChangeKey],
    resultKey: DerivedRelationResultKey
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
):
  val resultKey: DerivedRelationResultKey = DerivedRelationResultKey.from(relation)

private[chessjudgment] object StructuralDeltaContracts:
  import TransitionConsequenceKind.*

  def consequences(delta: TransitionStructuralDelta): List[TransitionConsequence] =
    val topology = delta.pawnTopology
    val baseConsequences =
      List(
        Option.when(topology.openedFiles.nonEmpty)(
          TransitionConsequence(
            OpenFileEstablished,
            subjectBindings = exactRelationBindings(
              topology.openedFiles.map(change => fileSubject(change.file) -> change.relationKeys)
            )
          )
        ),
        Option.when(topology.semiOpenedFiles.nonEmpty)(
          TransitionConsequence(
            SemiOpenFileEstablished,
            subjectBindings = exactRelationBindings(
                topology.semiOpenedFiles.map(change => semiOpenFileSubject(delta.perspective, change.file) -> change.relationKeys)
            )
          )
        ),
        Option.when(topology.createdTensions.nonEmpty)(
          TransitionConsequence(
            TransitionConsequenceKind.PawnTensionCreated,
            subjectBindings = exactRelationBindings(topology.createdTensions.map(edge =>
              createdTensionSubject(edge) -> List(edge.relationKey)
            )),
            resultPremiseKeys = exactResultKeys(topology.createdTensions.map(_.resultKey))
          )
        ),
        Option.when(topology.resolvedTensions.nonEmpty)(
          TransitionConsequence(
            PawnTensionResolution,
            subjectBindings = exactRelationBindings(topology.resolvedTensions.map(edge =>
              resolvedTensionSubject(edge) -> List(edge.relationKey)
            )),
            resultPremiseKeys = exactResultKeys(topology.resolvedTensions.map(_.resultKey))
          )
        ),
        Option.when(topology.passedCreated.nonEmpty || topology.passedAdvanced.nonEmpty)(
          TransitionConsequence(
            PassedPawnProgress,
            subjectBindings = exactRelationBindings(
              topology.passedCreated.map(change =>
                passedCreatedSubject(delta.perspective, change.square) -> change.relationKeys
              ) ++ topology.passedAdvanced.map(advance =>
                passedAdvanceSubject(advance) -> advance.relationKeys
              )
            ),
            resultPremiseKeys = exactResultKeys(
              topology.passedCreated.map(_.resultKey) ++ topology.passedAdvanced.map(_.resultKey)
            )
          )
        ),
        Option.when(topology.passedLost.nonEmpty)(
          TransitionConsequence(
            PassedPawnStatusRemoved,
            subjectBindings = exactRelationBindings(
              topology.passedLost.map(change =>
                passedLostSubject(delta.perspective, change.square) -> change.relationKeys
              )
            ),
            resultPremiseKeys = exactResultKeys(topology.passedLost.map(_.resultKey))
          )
        )
      ).flatten
    val consequences = baseConsequences
    val semanticKeys = consequences.map(consequence =>
      (
        consequence.kind,
        consequence.subjectBindings.map(_.stableKey).sorted.mkString(","),
        consequence.targetBindings.map(_.stableKey).sorted.mkString(",")
      )
    )
    require(semanticKeys.distinct.size == semanticKeys.size, "duplicate structural consequences")
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

  private def exactResultKeys(keys: List[DerivedRelationResultKey]): List[DerivedRelationResultKey] =
    keys.distinct.sortBy(_.stableKey)

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

private[chessjudgment] object StructuralDeltaAnalyzer:

  def delta(
      beforeAnalysis: PositionAnalysis,
      afterAnalysis: PositionAnalysis,
      legalStep: lila.chessjudgment.model.line.LegalReplayStep,
      relationDelta: RelationSemanticDelta,
      pawnTopologyTransitions: List[RelationFactEvidence]
  ): TransitionStructuralDelta =
    val transitionFootprint = admittedFootprint(beforeAnalysis, afterAnalysis, legalStep)
    require(
      relationDelta.beforeInventory.sameOwner(beforeAnalysis.relationInventory) &&
        relationDelta.afterInventory.sameOwner(afterAnalysis.relationInventory),
      "a structural delta must consume the exact replay-owned relation delta"
    )
    assembleDelta(
      transitionFootprint,
      relationDelta,
      pawnTopologyTransitions
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
      transitionFootprint: lila.chessjudgment.model.position.BoardTransitionFootprint,
      relationDelta: RelationSemanticDelta,
      pawnTopologyTransitions: List[RelationFactEvidence]
  ): TransitionStructuralDelta =
    val side = relationDelta.rootMove.side
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
    TransitionStructuralDelta(
        perspective = side,
        relationDelta = relationDelta,
        pawnTopology = PawnTopologyDelta(
          openedFiles = pawnFileDelta.opened,
          semiOpenedFiles = pawnFileDelta.semiOpened,
          createdTensions = verticalPawnDelta.createdTensions,
          resolvedTensions = verticalPawnDelta.resolvedTensions,
          passedCreated = verticalPawnDelta.passedCreated,
          passedAdvanced = verticalPawnDelta.passedAdvanced,
          passedLost = verticalPawnDelta.passedLost
        )
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

    def matchingChanges(
        state: ClosedPawnStateTransition,
        direction: RelationChangeDirection,
        kind: RelationFactKind
    )(matches: RelationWitnessDetail => Boolean): List[RelationSemanticChange] =
      val admittedSourceIds = VerticalRelationContracts.proofOf(state.relation.detail)
        .toList
        .flatMap(_.sourceSemanticIds)
        .toSet
      (direction match
        case RelationChangeDirection.Removed => relationDelta.removedOf(kind)
        case RelationChangeDirection.Established => relationDelta.establishedOf(kind)
      ).filter(change => admittedSourceIds(change.semanticId) && matches(change.detail))

    def exactChange(
        state: ClosedPawnStateTransition,
        direction: RelationChangeDirection,
        kind: RelationFactKind,
        label: String
    )(matches: RelationWitnessDetail => Boolean): RelationChangeKey =
      val candidates = matchingChanges(state, direction, kind)(matches)
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
      val changedControls = matchingChanges(
        state,
        direction,
        RelationFactKind.GeometricControl
      ) {
        case RelationWitnessDetail.GeometricControl(
              Color.White,
              whitePawn,
              whiteRole,
              blackPawn
            ) =>
          whiteRole.name.equalsIgnoreCase(Pawn.name) &&
            (if side.white then whitePawn == from && blackPawn == to
             else blackPawn == from && whitePawn == to)
        case _ => false
      }
      val exactSources =
        if changedControls.nonEmpty then changedControls
        else
          matchingChanges(state, direction, RelationFactKind.PawnPassage) {
            case RelationWitnessDetail.PawnPassage(owner, pawn, opponents) =>
              owner == side && pawn == from && opponents.contains(to)
            case _ => false
          }
      exactSources match
        case change :: Nil => change.key
        case found =>
          throw IllegalArgumentException(
            s"L1 pawn transition '${from.key}-${to.key}' needs one exact changed control or passage source, found ${found.size}"
          )

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
            tensionKey(state, RelationChangeDirection.Established, after.square, target),
            state.resultKey
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
            tensionKey(state, RelationChangeDirection.Removed, before.square, target),
            state.resultKey
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
          ).sortBy(_.stableKey),
          state.resultKey
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
          ).sortBy(_.stableKey),
          state.resultKey
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
            relationKeys.sortBy(_.stableKey),
            state.resultKey
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
          List(passageKey(state, RelationChangeDirection.Removed, before.square)),
          state.resultKey
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

  private def relativeRank(square: Square, side: Color): Int =
    if side.white then square.rank.value + 1 else 8 - square.rank.value


  private def squareAt(key: String): Option[Square] =
    Square.fromKey(key.toLowerCase)

  private def boardFileKey(file: File): String =
    (('a'.toInt + file.value).toChar).toString
