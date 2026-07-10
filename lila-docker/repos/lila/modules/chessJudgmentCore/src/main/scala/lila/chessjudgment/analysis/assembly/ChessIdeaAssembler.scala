package lila.chessjudgment.analysis.assembly

import chess.Pawn
import chess.format.{ Fen, Uci }
import lila.chessjudgment.analysis.evaluation.JudgmentThresholds
import lila.chessjudgment.analysis.policy.ClaimTruthPolicy
import lila.chessjudgment.model.judgment.*

final case class ChessIdeaAssembly(
    input: NormalizedMoveReviewInput,
    context: JudgmentAssemblyContext
)

object ChessIdeaAssembler:

  private enum TacticalIdeaDriver(val id: String):
    case KingForcing extends TacticalIdeaDriver("king-forcing")
    case MaterialGain extends TacticalIdeaDriver("material-gain")
    case RecaptureChoice extends TacticalIdeaDriver("recapture-choice")
    case Tempo extends TacticalIdeaDriver("tempo")
    case RelationMechanism extends TacticalIdeaDriver("relation-mechanism")
    case Conversion extends TacticalIdeaDriver("conversion")
    case Refutation extends TacticalIdeaDriver("refutation")
    case DrawResource extends TacticalIdeaDriver("draw-resource")
    case PawnPromotion extends TacticalIdeaDriver("pawn-promotion")

  private final case class OpeningMoveBinding(
      subject: IdeaSubject,
      primaryLine: Option[LineNodeRef],
      moveUci: Option[String],
      evidence: List[EvidenceRef]
  )

  def assemble(raw: RawMoveReviewInput): Option[ChessIdeaAssembly] =
    RelativeAssessmentAssembler.assemble(raw).map(enrich)

  def enrich(assembly: RelativeAssessmentAssembly): ChessIdeaAssembly =
    val allocator = JudgmentProvenanceAllocator.forInput(assembly.input)
    val context = assembly.context
    val ideas =
      List
        .concat(
          tacticalIdeas(context, allocator),
          pawnStructureIdeas(context, allocator),
          openingIdeas(context, allocator),
          defensiveIdeas(context, allocator),
          relativeCauseIdeas(context, allocator),
          evaluationIdeas(context, allocator),
          strategicIdeas(context, allocator)
        )
        .distinctBy(_.ref.id)
    val ideaRecords = ideas.map(idea => ChessIdeaBuilder.evidenceRecord(s"${idea.ref.id}:evidence", idea))
    val withEvidence = context.withEvidence(ideaRecords)
    val withIdeas = ideas.foldLeft(withEvidence)((ctx, idea) => ctx.withIdea(idea))
    ChessIdeaAssembly(assembly.input, withIdeas)

  private def tacticalIdeas(
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator
  ): List[ChessIdea] =
    (
      context.lines.flatMap(line => compositeTacticalIdeas(context, allocator, line)) ++
        playedTransitionTacticalIdeas(context, allocator)
    ).distinctBy(_.ref.id)

  private def playedTransitionTacticalIdeas(
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator
  ): List[ChessIdea] =
    context.playedTransition.toList
      .filterNot(edge => context.line(LineNodeRole.Played).exists(_.ref.rootMove == edge.moveUci))
      .flatMap { edge =>
        val transitionRecords =
          context.evidenceGraph.records.filter(record =>
            record.ref.scope == EvidenceScope.PlayedTransition &&
              record.ref.position == edge.from &&
              transitionRecordMentionsMove(record, edge.moveUci)
          )
        val mechanismRecords =
          transitionRecords.collect {
            case record @ EvidenceRecord(_, payload: TacticalMechanismEvidence, _) if payload.canAnchorTacticalIdea =>
              record
          }
        val drivers = mechanismRecords.collect {
          case EvidenceRecord(_, payload: TacticalMechanismEvidence, _) => tacticalDriverForMechanism(payload.kind)
        }.distinct
        drivers.flatMap { driver =>
          val evidence =
            transitionMechanismEvidence(
              context = context,
              edge = edge,
              driver = driver,
              mechanismRecords = mechanismRecords
            )
          Option.when(evidence.nonEmpty) {
            ChessIdeaBuilder.fromEvidence(
              id = allocator.evidenceId(s"idea:tactical:${driver.id}:played-transition:${edge.moveUci}"),
              family = ChessIdeaFamily.Tactical,
              subject = IdeaSubject.PlayedMove,
              primaryPosition = edge.from,
              primaryLine = None,
              moveUci = Some(edge.moveUci),
              evidence = evidence.distinctBy(_.id),
              scope = EvidenceScope.PlayedTransition,
              confidence = EvidenceConfidence.LegalReplayVerified
            )
          }
        }
      }

  private def compositeTacticalIdeas(
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator,
      line: CandidateLineNode
  ): List[ChessIdea] =
    val lineRecords = context.evidenceGraph.recordsFor(line.ref)
    val lineFactRecords = lineRecords.collect { case record @ EvidenceRecord(_, _: LineFactEvidence, _) => record }
    val evalRecords = lineRecords.collect { case record @ EvidenceRecord(_, _: EvalFactEvidence, _) => record }
    val mechanismRecords =
      lineRecords.collect {
        case record @ EvidenceRecord(_, payload: TacticalMechanismEvidence, _) if payload.canAnchorTacticalIdea =>
          record
      }
    val relative = relativeAssessmentsForLine(context, line.ref)
    val primaryPosition =
      (mechanismRecords ++ lineFactRecords ++ evalRecords).headOption
        .map(_.ref.position)
        .orElse(context.root)
    val drivers =
      mechanismRecords.collect {
        case EvidenceRecord(_, payload: TacticalMechanismEvidence, _) => tacticalDriverForMechanism(payload.kind)
      }.distinct
    primaryPosition.toList.flatMap { position =>
      drivers.flatMap { driver =>
        val evidence = compositeMechanismEvidence(
          context = context,
          position = position,
          driver = driver,
          lineRecords = lineFactRecords,
          evalRecords = evalRecords,
          mechanismRecords = mechanismRecords,
          relativeAssessments = relative
        )
        Option.when(evidence.nonEmpty) {
          ChessIdeaBuilder.fromEvidence(
            id = allocator.evidenceId(s"idea:tactical:${driver.id}:${allocator.key(line.role)}:${line.ref.rank}:${line.ref.rootMove}"),
            family = ChessIdeaFamily.Tactical,
            subject = line.ref.role.subject,
            primaryPosition = position,
            primaryLine = Some(line.ref),
            moveUci = Some(line.ref.rootMove),
            evidence = evidence.distinctBy(_.id),
            scope = line.ref.role.scope,
            confidence = tacticalIdeaConfidence(driver, relative, evalRecords)
          )
        }
      }
    }

  private def pawnStructureIdeas(
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator
  ): List[ChessIdea] =
    context.evidenceGraph.records.flatMap {
      case EvidenceRecord(ref, payload: StrategicMechanismEvidence, parents)
          if payload.canAnchorPawnStructureIdea =>
        val primaryLine = pawnStructureMechanismPrimaryLine(context, ref)
        val evidence =
          longTermIdeaEvidence(
            ref :: parents ++
              pawnStructureMechanismTransitionEvidence(context, ref) ++
              primaryLine.toList.flatMap(lineLayerRefs(context, _)) ++
              recordsForPosition(context, EvidenceLayer.Board, ref.position)
          )
        Option.when(primaryLine.nonEmpty && evidence.nonEmpty) {
          ChessIdeaBuilder.fromEvidence(
            id = allocator.evidenceId(s"idea:pawn-structure-mechanism:${allocator.key(ref.id)}"),
            family = ChessIdeaFamily.PawnStructure,
            subject = primaryLine.map(_.role.subject).getOrElse(IdeaSubject.Position),
            primaryPosition = ref.position,
            primaryLine = primaryLine,
            moveUci = primaryLine.map(_.rootMove),
            evidence = evidence,
            scope = ref.scope,
            confidence = ref.confidence
          )
        }
      case _ =>
        None
      }

  private def defensiveIdeas(
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator
  ): List[ChessIdea] =
    val threatEpisodeIdeas = context.evidenceGraph.records.collect {
      case EvidenceRecord(ref, payload: ThreatEpisodeEvidence, parents)
          if ref.position.sideToMove.forall(_ == payload.sideUnderPressure) &&
            payload.canAnchorDefensiveResource =>
        val evidence =
          (ref :: parents ++
            ref.line.toList.flatMap(lineLayerRefs(context, _))).distinctBy(_.id)
        ChessIdeaBuilder.fromEvidence(
          id = allocator.evidenceId(s"idea:defensive:${allocator.key(ref.id)}"),
          family = ChessIdeaFamily.Defensive,
          subject = IdeaSubject.Threat,
          primaryPosition = ref.position,
          primaryLine = ref.line,
          moveUci = payload.onlyDefense.orElse(payload.episode.bestDefense),
          evidence = evidence,
          scope = ref.scope,
          confidence = ref.confidence
        )
    }
    val mechanismIdeas = context.evidenceGraph.records.collect {
      case EvidenceRecord(ref, payload: TacticalMechanismEvidence, parents) if payload.canAnchorDefensiveIdea =>
        val evidence =
          (ref :: parents ++
            payload.line.toList.flatMap(lineLayerRefs(context, _))).distinctBy(_.id)
        ChessIdeaBuilder.fromEvidence(
          id = allocator.evidenceId(s"idea:defensive-mechanism:${allocator.key(ref.id)}"),
          family = ChessIdeaFamily.Defensive,
          subject = IdeaSubject.Threat,
          primaryPosition = ref.position,
          primaryLine = payload.line,
          moveUci = payload.moveUci,
          evidence = evidence,
          scope = ref.scope,
          confidence = ref.confidence
        )
    }
    (threatEpisodeIdeas ++ mechanismIdeas).distinctBy(_.ref.id)

  private def evaluationIdeas(
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator
  ): List[ChessIdea] =
    context.relativeAssessments.map { assessment =>
      val evidence =
        (assessment.evidence ::
          (assessment.counterfactualEvidence ++
            primaryCandidateComparisonEvidence(context, assessment) ++
            assessment.verdictCertificationEvidence.toList) ++
          lineLayerRefs(context, assessment.reference.ref) ++
          lineLayerRefs(context, assessment.candidate.ref)).distinctBy(_.id)
      ChessIdeaBuilder.fromEvidence(
        id = allocator.evidenceId(s"idea:evaluation:${allocator.key(assessment.evidence.id)}"),
        family = ChessIdeaFamily.Evaluation,
        subject = IdeaSubject.PlayedMove,
        primaryPosition = assessment.played.from,
        primaryLine = Some(assessment.candidate.ref),
        moveUci = Some(assessment.played.moveUci),
        evidence = evidence,
        scope = assessment.evidence.scope,
        confidence = assessment.confidence
      )
    }

  private def primaryCandidateComparisonEvidence(
      context: JudgmentAssemblyContext,
      assessment: RelativeMoveAssessment
  ): List[EvidenceRef] =
    assessment.candidateComparisonEvidence.filter { ref =>
      context.evidenceGraph.byId.get(ref.id).exists {
        case EvidenceRecord(_, CandidateComparisonEvidence(fact), _) =>
          fact.kind == CandidateComparisonKind.PlayedVsBest &&
            fact.referenceLine == assessment.reference.ref &&
            fact.candidateLine == assessment.candidate.ref
        case _ =>
          false
      }
    }

  private def relativeCauseIdeas(
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator
  ): List[ChessIdea] =
    val standaloneCauses =
      context.evidenceGraph.records.collect { case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) => cause.identityKey }.toSet
    context.evidenceGraph.records.flatMap {
      case EvidenceRecord(ref, RelativeCauseFactEvidence(cause), parents) =>
        relativeCauseIdeasFromRecord(context, allocator, ref, cause, parents)
      case EvidenceRecord(ref, MoveVerdictCertificationEvidence(certification), parents) =>
        certification.causes
          .filterNot(cause => standaloneCauses.contains(cause.identityKey))
          .flatMap(cause => relativeCauseIdeasFromRecord(context, allocator, ref, cause, parents))
      case _ =>
        Nil
    }

  private def relativeCauseIdeasFromRecord(
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator,
      ref: EvidenceRef,
      cause: RelativeCauseFact,
      parents: List[EvidenceRef]
  ): List[ChessIdea] =
    val subjectLine = cause.eventLine
    val supportRefs = relativeCauseIdeaSupportRefs(context, cause, parents)
    val depthProofRefs = relativeCauseIdeaDepthProofRefs(cause)
    familiesForRelativeCause(context, ref, cause, supportRefs).map { family =>
      val familyEvidence = relativeCauseIdeaEvidence(context, ref, supportRefs, depthProofRefs, family)
      ChessIdeaBuilder.fromEvidence(
        id = relativeCauseIdeaId(allocator, family, cause, subjectLine, ref),
        family = family,
        subject = subjectForRelativeCause(cause, subjectLine),
        primaryPosition = ref.position,
        primaryLine = Some(subjectLine),
        moveUci = Some(subjectLine.rootMove),
        evidence = familyEvidence,
        scope = ref.scope,
        confidence = ref.confidence
      )
    }

  private def relativeCauseIdeaId(
      allocator: JudgmentProvenanceAllocator,
      family: ChessIdeaFamily,
      cause: RelativeCauseFact,
      subjectLine: LineNodeRef,
      ref: EvidenceRef
  ): String =
    val key = cause.identityKey
    val identityHash = Integer.toHexString(key.toString.hashCode)
    allocator.evidenceId(
      s"idea:${allocator.key(family)}:relative-cause:${allocator.key(key.kind)}:${allocator.key(key.comparisonKind)}:${allocator.key(key.role)}:${allocator.key(key.sourceSide)}:${allocator.key(key.importance)}:${allocator.key(subjectLine.rootMove)}:$identityHash:${allocator.key(ref.id)}"
    )

  private def relativeCauseIdeaSupportRefs(
      context: JudgmentAssemblyContext,
      cause: RelativeCauseFact,
      parents: List[EvidenceRef]
  ): List[EvidenceRef] =
    val comparisonParents = parents.filter(ref =>
      context.evidenceGraph.byId.get(ref.id).exists {
        case EvidenceRecord(_, CandidateComparisonEvidence(_), _) => true
        case _                                                    => false
      }
    )
    val proofSources =
      cause.proof.toList.flatMap(proof =>
        proof.directProof.sourceRefs ++ proof.contrastProof.sourceRefs ++ proof.contextSupport.sourceRefs
    )
    (comparisonParents ++ cause.supportEvidence ++ proofSources).distinctBy(_.id)

  private def relativeCauseIdeaDepthProofRefs(
      cause: RelativeCauseFact
  ): List[EvidenceRef] =
    val proofSources =
      cause.proof.toList.flatMap(proof => proof.directProof.sourceRefs ++ proof.contrastProof.sourceRefs)
    proofSources.distinctBy(_.id)

  private def relativeCauseIdeaEvidence(
      context: JudgmentAssemblyContext,
      ref: EvidenceRef,
      supportRefs: List[EvidenceRef],
      depthProofRefs: List[EvidenceRef],
      family: ChessIdeaFamily
  ): List[EvidenceRef] =
    val supportEvidence = (ref :: supportRefs).distinctBy(_.id)
    val depthEvidence = (ref :: depthProofRefs).distinctBy(_.id)
    family match
      case ChessIdeaFamily.Tactical | ChessIdeaFamily.Material | ChessIdeaFamily.Defensive =>
        depthEvidence
      case ChessIdeaFamily.Conversion =>
        (depthEvidence ++ conversionContextEvidence(context, ref.position)).distinctBy(_.id)
      case ChessIdeaFamily.Strategic =>
        (ref :: longTermIdeaEvidence(supportRefs)).distinctBy(_.id)
      case ChessIdeaFamily.PawnStructure | ChessIdeaFamily.Opening =>
        (ref :: longTermIdeaEvidence(supportRefs)).distinctBy(_.id)
      case _ =>
        supportEvidence

  private def familyForRelativeCause(kind: RelativeCauseKind): ChessIdeaFamily =
    ClaimEventCluster.kindForCause(kind) match
      case Some(ClaimEventClusterKind.TacticalEvent)   => ChessIdeaFamily.Tactical
      case Some(ClaimEventClusterKind.DefensiveEvent)  => ChessIdeaFamily.Defensive
      case Some(ClaimEventClusterKind.ConversionEvent) => ChessIdeaFamily.Conversion
      case Some(ClaimEventClusterKind.MaterialEvent)   => ChessIdeaFamily.Material
      case None                                        => ChessIdeaFamily.Strategic

  private def familiesForRelativeCause(
      context: JudgmentAssemblyContext,
      ref: EvidenceRef,
      cause: RelativeCauseFact,
      supportRefs: List[EvidenceRef]
  ): List[ChessIdeaFamily] =
    val supportRecords = recordsForRefs(context, supportRefs)
    cause.kind match
      case RelativeCauseKind.MaterialSwing =>
        val promoted =
          List(
            Option.when(cause.hasOwnedTypedDepth)(ChessIdeaFamily.Material),
            Option.when(
              materialSwingHasTacticalProof(cause)
            )(ChessIdeaFamily.Tactical),
            Option.when(
              cause.hasOwnedTypedDepth &&
                hasConversionContext(context, ref.position, supportRefs)
            )(ChessIdeaFamily.Conversion)
          ).flatten
        promoted.distinct
      case RelativeCauseKind.SacrificeCompensation =>
        val promoted =
          List(
            Option.when(cause.hasOwnedTypedDepth)(ChessIdeaFamily.Material),
            Option.when(relativeCauseHasTacticalProof(cause))(ChessIdeaFamily.Tactical)
          ).flatten
        promoted.distinct
      case kind if strategicRelativeCause(kind) =>
        List(
          Option.when(cause.hasOwnedAdmissibleLongTermProof && hasStrategicRelativeCauseSupport(cause, supportRecords))(
            ChessIdeaFamily.Strategic
          ),
          Option.when(cause.hasOwnedAdmissibleLongTermProof && hasPawnStructureRelativeCauseSupport(cause, supportRecords))(
            ChessIdeaFamily.PawnStructure
          ),
          Option.when(cause.hasOwnedAdmissibleLongTermProof && hasOpeningRelativeCauseSupport(supportRecords))(
            ChessIdeaFamily.Opening
          ),
          Option.when(relativeCauseHasTacticalProof(cause))(
            ChessIdeaFamily.Tactical
          )
        ).flatten.distinct
      case _ =>
        val base = familyForRelativeCause(cause.kind)
        val baseFamily =
          Option.when(
            (base != ChessIdeaFamily.Tactical || relativeCauseHasTacticalProof(cause)) &&
              (base != ChessIdeaFamily.Material || cause.hasOwnedTypedDepth) &&
              (base != ChessIdeaFamily.Conversion || cause.hasOwnedTypedDepth) &&
              (base != ChessIdeaFamily.Defensive || ClaimTruthPolicy.defensiveRelativeCauseCanSeedIdea(cause))
          )(base)
        val conversionFamily =
          Option.when(
            materialConversionCause(cause.kind) &&
              cause.hasOwnedTypedDepth &&
            hasConversionContext(context, ref.position, supportRefs)
          )(ChessIdeaFamily.Conversion)
        (baseFamily.toList ++ conversionFamily.toList).distinct

  private def materialConversionCause(kind: RelativeCauseKind): Boolean =
    kind == RelativeCauseKind.RecaptureRecoveryWindow || kind == RelativeCauseKind.MaterialSwing

  private def materialSwingHasTacticalProof(cause: RelativeCauseFact): Boolean =
    val engineBackedMaterialSwing =
      cause.winPercentLossForMover >= JudgmentThresholds.INACCURACY_WP ||
        cause.candidateWinPercentDeltaForMover >= JudgmentThresholds.PLAYABLE_LOSS_WP
    engineBackedMaterialSwing && relativeCauseHasTacticalProof(cause)

  private def relativeCauseHasTacticalProof(cause: RelativeCauseFact): Boolean =
    cause.hasOwnedTacticalProof

  private def strategicRelativeCause(kind: RelativeCauseKind): Boolean =
    RelativeCauseKind.strategicContrastBacked(kind)

  private def recordsForRefs(
      context: JudgmentAssemblyContext,
      refs: List[EvidenceRef]
  ): List[EvidenceRecord] =
    refs.flatMap(ref => context.evidenceGraph.byId.get(ref.id))

  private def hasStrategicRelativeCauseSupport(cause: RelativeCauseFact, records: List[EvidenceRecord]): Boolean =
    cause.proof.exists(_.directProof.transitionConsequences.nonEmpty) ||
    records.exists {
      case EvidenceRecord(_, payload: StrategicMechanismContrastEvidence, _) =>
        payload.hasActionableContrast
      case EvidenceRecord(_, payload: StrategicMechanismEvidence, _) =>
        payload.canSupportStrategicCause
      case _ =>
        false
    }

  private def hasPawnStructureRelativeCauseSupport(cause: RelativeCauseFact, records: List[EvidenceRecord]): Boolean =
    cause.kind == RelativeCauseKind.PawnWeaknessTarget ||
      cause.kind == RelativeCauseKind.PawnBreakOpportunity ||
      cause.proof.exists(proof =>
        proof.directProof.transitionConsequences.exists(consequence =>
          consequence.consequence.kind == TransitionConsequenceKind.WeakPawnTargetCreated
        )
      ) ||
    records.exists {
      case EvidenceRecord(_, payload: StrategicMechanismContrastEvidence, _) =>
        payload.actionableComparisons.exists(_.axis.kind == StrategicAxisKind.PawnBreak)
      case EvidenceRecord(_, payload: StrategicMechanismEvidence, _) =>
        payload.canAnchorPawnStructureIdea
      case _ =>
        false
    }

  private def hasOpeningRelativeCauseSupport(records: List[EvidenceRecord]): Boolean =
    StrategicMechanismEvidence.openingClaimSupported(records)

  private def hasConversionContext(
      context: JudgmentAssemblyContext,
      position: PositionNodeRef,
      parents: List[EvidenceRef]
  ): Boolean =
    ClaimTruthPolicy.conversionContextCanSeedIdea(conversionContextRecords(context, position, parents))

  private def conversionContextEvidence(
      context: JudgmentAssemblyContext,
      position: PositionNodeRef
  ): List[EvidenceRef] =
    conversionContextRecords(context, position, Nil).map(_.ref)

  private def conversionContextRecords(
      context: JudgmentAssemblyContext,
      position: PositionNodeRef,
      parents: List[EvidenceRef]
  ): List[EvidenceRecord] =
    val positionRecords = context.evidenceGraph.recordsFor(position)
    val parentRecords = parents.flatMap(parent => context.evidenceGraph.byId.get(parent.id))
    (positionRecords ++ parentRecords)
      .filter(record => ClaimTruthPolicy.conversionContextCanSeedIdea(List(record)))
      .distinctBy(_.ref.id)

  private def subjectForRelativeCause(cause: RelativeCauseFact, line: LineNodeRef): IdeaSubject =
    cause.kind match
      case kind if ClaimEventCluster.kindForCause(kind).contains(ClaimEventClusterKind.DefensiveEvent) =>
        IdeaSubject.Threat
      case RelativeCauseKind.PlanImprovement | RelativeCauseKind.PlanContradiction =>
        IdeaSubject.Plan
      case _ =>
        line.role.subject

  private def strategicIdeas(
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator
  ): List[ChessIdea] =
    context.evidenceGraph.records.flatMap {
      case EvidenceRecord(ref, payload: StrategicMechanismEvidence, parents)
          if lineBoundLongTermMechanism(ref) && (payload.canAnchorStrategicIdea || payload.canAnchorPlanIdea) =>
        val subject =
          if payload.kind == StrategicMechanismKind.PlanPressure && payload.canAnchorPlanIdea then IdeaSubject.Plan
          else ref.line.map(_.role.subject).getOrElse(IdeaSubject.Position)
        val evidence =
          longTermIdeaEvidence(
            ref :: parents ++
              ref.line.toList.flatMap(lineLayerRefs(context, _)) ++
              recordsForPosition(context, EvidenceLayer.Board, ref.position) ++
              recordsForPosition(context, EvidenceLayer.SinglePosition, ref.position)
          )
        Option.when(evidence.nonEmpty) {
          ChessIdeaBuilder.fromEvidence(
            id = allocator.evidenceId(s"idea:strategic-mechanism:${allocator.key(ref.id)}"),
            family = ChessIdeaFamily.Strategic,
            subject = subject,
            primaryPosition = ref.position,
            primaryLine = ref.line,
            moveUci = ref.line.map(_.rootMove),
            evidence = evidence,
            scope = ref.scope,
            confidence = ref.confidence
          )
        }
      case _ =>
        None
    }

  private def lineBoundLongTermMechanism(ref: EvidenceRef): Boolean =
    ref.line.exists(_.role == LineNodeRole.Played)

  private def pawnStructureMechanismPrimaryLine(
      context: JudgmentAssemblyContext,
      ref: EvidenceRef
  ): Option[LineNodeRef] =
    ref.line.filter(_.role == LineNodeRole.Played).orElse {
      Option.when(playedPawnStructureTransition(context, ref).nonEmpty)(
        context.line(LineNodeRole.Played).map(_.ref)
      ).flatten
    }

  private def pawnStructureMechanismTransitionEvidence(
      context: JudgmentAssemblyContext,
      ref: EvidenceRef
  ): List[EvidenceRef] =
    playedPawnStructureTransition(context, ref).map(_.evidence).toList

  private def playedPawnStructureTransition(
      context: JudgmentAssemblyContext,
      ref: EvidenceRef
  ): Option[MoveTransitionEdge] =
    context.playedTransition
      .filter(edge =>
        ref.scope == EvidenceScope.AfterPlayedPosition &&
          edge.to == ref.position &&
          playedMoveIsPawnMove(edge)
      )

  private def playedMoveIsPawnMove(edge: MoveTransitionEdge): Boolean =
    Fen.read(chess.variant.Standard, Fen.Full(edge.from.fen))
      .flatMap(position =>
        Uci(edge.moveUci)
          .collect { case move: Uci.Move => move }
          .flatMap(move => position.board.roleAt(move.orig))
      )
      .contains(Pawn)

  private def openingIdeas(
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator
  ): List[ChessIdea] =
    context.evidenceGraph.records.flatMap {
      case record @ EvidenceRecord(ref, payload: StrategicMechanismEvidence, parents)
          if payload.canAnchorOpeningIdea =>
        val supportRecords = openingMechanismSupport(context, record)
        val assessment = supportRecords.collectFirst {
          case EvidenceRecord(_, ApplicabilityAssessmentEvidence(assessment), _) => assessment
        }
        val moveBinding = assessment.flatMap(openingMoveBinding(context, _, supportRecords.map(_.ref)))
        val primaryLine = moveBinding.flatMap(_.primaryLine)
        val evidence =
          longTermIdeaEvidence(
            ref :: parents ++
              supportRecords.map(_.ref) ++
              moveBinding.toList.flatMap(_.evidence) ++
              primaryLine.toList.flatMap(lineLayerRefs(context, _))
          )
        Option.when(StrategicMechanismEvidence.openingClaimSupported(supportRecords) && evidence.nonEmpty) {
          ChessIdeaBuilder.fromEvidence(
            id = allocator.evidenceId(s"idea:opening-mechanism:${allocator.key(ref.id)}"),
            family = ChessIdeaFamily.Opening,
            subject = moveBinding.map(_.subject).getOrElse(IdeaSubject.Position),
            primaryPosition = ref.position,
            primaryLine = primaryLine,
            moveUci = moveBinding.flatMap(_.moveUci),
            evidence = evidence,
            scope = ref.scope,
            confidence = ref.confidence
          )
        }
      case _ =>
        None
    }

  private def openingMechanismSupport(context: JudgmentAssemblyContext, record: EvidenceRecord): List[EvidenceRecord] =
    val parentRecords = record.parents.flatMap(parent => context.evidenceGraph.byId.get(parent.id))
    val siblingOpeningAnchors =
      context.evidenceGraph.records.filter {
        case EvidenceRecord(ref, payload: StrategicMechanismEvidence, _) =>
          ref.position == record.ref.position &&
            payload.hasOpeningAnchorSignal
        case _ =>
          false
      }
    val siblingParents =
      siblingOpeningAnchors.flatMap(_.parents.flatMap(parent => context.evidenceGraph.byId.get(parent.id)))
    (record :: parentRecords ++ siblingOpeningAnchors ++ siblingParents).distinctBy(_.ref.id)

  private def openingMoveBinding(
      context: JudgmentAssemblyContext,
      assessment: ApplicabilityAssessment,
      parents: List[EvidenceRef]
  ): Option[OpeningMoveBinding] =
    val supportedThemes = assessment.supportedThemes.toSet
    val supportedAnchors =
      parents
        .flatMap(parent => context.evidenceGraph.byId.get(parent.id))
        .collect {
          case record @ EvidenceRecord(_, FeatureAnchorEvidence(anchor), _)
              if supportedThemes.contains(anchor.theme) && anchor.canCorroborateOpeningPrior =>
            record
        }
    val sourceRecords =
      supportedAnchors
        .flatMap(anchorRecord => anchorRecord.parents.flatMap(parent => context.evidenceGraph.byId.get(parent.id)))
        .distinctBy(_.ref.id)
    List(
      openingLineBinding(sourceRecords, LineNodeRole.Played, IdeaSubject.PlayedMove),
      openingLineBinding(sourceRecords, LineNodeRole.BestReference, IdeaSubject.ReferenceMove),
      openingLineBinding(sourceRecords, LineNodeRole.Alternative, IdeaSubject.CandidateLine)
    ).flatten.sortBy(openingMoveBindingScore).lastOption

  private def openingLineBinding(
      sourceRecords: List[EvidenceRecord],
      role: LineNodeRole,
      subject: IdeaSubject
  ): Option[OpeningMoveBinding] =
    val roleSources = sourceRecords.filter(openingSourceMatchesRole(_, role))
    val primaryLine =
      roleSources.flatMap(record => openingSourceLine(record, role)).headOption
    val moveUci =
      roleSources.collectFirst { case EvidenceRecord(_, payload: StructuralDeltaEvidence, _) =>
        payload.moveUci
      }.orElse(
        roleSources.collectFirst { case EvidenceRecord(_, MoveTransitionEvidence(move, _, _), _) =>
          move
        }
      ).orElse(primaryLine.map(_.rootMove))
    Option.when(primaryLine.nonEmpty || moveUci.nonEmpty)(
      OpeningMoveBinding(
        subject = subject,
        primaryLine = primaryLine,
        moveUci = moveUci,
        evidence = roleSources.map(_.ref)
      )
    )

  private def openingMoveBindingScore(binding: OpeningMoveBinding): (Int, Int, Int, Int) =
    (
      binding.evidence.size,
      Option.when(binding.evidence.exists(_.layer == EvidenceLayer.StructuralDelta))(1).getOrElse(0),
      Option.when(binding.moveUci.nonEmpty)(1).getOrElse(0),
      binding.subject match
        case IdeaSubject.PlayedMove     => 2
        case IdeaSubject.ReferenceMove  => 1
        case IdeaSubject.CandidateLine  => 0
        case IdeaSubject.Position | IdeaSubject.Threat | IdeaSubject.Plan => 0
    )

  private def openingSourceMatchesRole(record: EvidenceRecord, role: LineNodeRole): Boolean =
    openingSourceLine(record, role).nonEmpty ||
      (role == LineNodeRole.Played && record.ref.scope == EvidenceScope.PlayedTransition)

  private def openingSourceLine(record: EvidenceRecord, role: LineNodeRole): Option[LineNodeRef] =
    record.payload match
      case payload: StructuralDeltaEvidence =>
        payload.line.filter(_.role == role)
      case MoveTransitionEvidence(_, _, _) =>
        record.ref.line.filter(_.role == role)
      case _ =>
        record.ref.line.filter(_.role == role)

  private def relativeSupportsTacticalIdea(assessment: RelativeMoveAssessment): Boolean =
    assessment.comparison.winPercentLossForMover >= JudgmentThresholds.SIGNIFICANT_THREAT_WP ||
      assessment.comparison.candidateWinPercentDeltaForMover >= JudgmentThresholds.PLAYABLE_LOSS_WP ||
      assessment.comparison.candidateSet.exists(set =>
        set.onlyMove ||
          set.bestToSecondWinPercentGapForMover.exists(_ >= JudgmentThresholds.ONLY_MOVE_GAP_WP)
      )

  private def tacticalDriverForMechanism(kind: TacticalMechanismKind): TacticalIdeaDriver =
    kind match
      case TacticalMechanismKind.KingForcing =>
        TacticalIdeaDriver.KingForcing
      case TacticalMechanismKind.MaterialGain =>
        TacticalIdeaDriver.MaterialGain
      case TacticalMechanismKind.RecaptureChoice =>
        TacticalIdeaDriver.RecaptureChoice
      case TacticalMechanismKind.Tempo =>
        TacticalIdeaDriver.Tempo
      case TacticalMechanismKind.RelationMechanism =>
        TacticalIdeaDriver.RelationMechanism
      case TacticalMechanismKind.Conversion =>
        TacticalIdeaDriver.Conversion
      case TacticalMechanismKind.Refutation =>
        TacticalIdeaDriver.Refutation
      case TacticalMechanismKind.DrawResource =>
        TacticalIdeaDriver.DrawResource
      case TacticalMechanismKind.PawnPromotion =>
        TacticalIdeaDriver.PawnPromotion
      case TacticalMechanismKind.DefensiveResource =>
        TacticalIdeaDriver.RelationMechanism

  private def compositeMechanismEvidence(
      context: JudgmentAssemblyContext,
      position: PositionNodeRef,
      driver: TacticalIdeaDriver,
      lineRecords: List[EvidenceRecord],
      evalRecords: List[EvidenceRecord],
      mechanismRecords: List[EvidenceRecord],
      relativeAssessments: List[RelativeMoveAssessment]
  ): List[EvidenceRef] =
    val driverRefs =
      mechanismRecords.flatMap {
        case EvidenceRecord(ref, payload: TacticalMechanismEvidence, parents) if tacticalDriverForMechanism(payload.kind) == driver =>
          ref :: parents
        case _ =>
          Nil
      }
    val lineRefs = (lineRecords ++ evalRecords).map(_.ref)
    val engineRefs = relativeEvidenceRefs(relativeAssessments)
    val boardRefs =
      context.evidenceGraph.records.collect {
        case record if record.ref.layer == EvidenceLayer.Board && record.ref.position == position =>
          record.ref
      }
    if driverRefs.isEmpty then Nil
    else (driverRefs ++ lineRefs ++ engineRefs ++ boardRefs.take(2)).distinctBy(_.id)

  private def transitionMechanismEvidence(
      context: JudgmentAssemblyContext,
      edge: MoveTransitionEdge,
      driver: TacticalIdeaDriver,
      mechanismRecords: List[EvidenceRecord]
  ): List[EvidenceRef] =
    val driverRefs =
      mechanismRecords.flatMap {
        case EvidenceRecord(ref, payload: TacticalMechanismEvidence, parents) if tacticalDriverForMechanism(payload.kind) == driver =>
          ref :: parents
        case _ =>
          Nil
      }
    val transitionRef =
      context.evidenceGraph.byId.get(edge.evidence.id).map(_.ref).toList
    val boardRefs =
      context.evidenceGraph.records.collect {
        case record if record.ref.layer == EvidenceLayer.Board && record.ref.position == edge.from =>
          record.ref
      }
    (driverRefs ++ transitionRef ++ boardRefs.take(2)).distinctBy(_.id)

  private def longTermIdeaEvidence(refs: List[EvidenceRef]): List[EvidenceRef] =
    refs
      .filterNot(ref => ClaimSupportCluster.longTermSupportExcludedLayer(ref.layer))
      .filterNot(ref => StrategicMechanismEvidence.rawStrategicSourceLayer(ref.layer))
      .distinctBy(_.id)

  private def transitionRecordMentionsMove(record: EvidenceRecord, moveUci: String): Boolean =
    record.payload match
      case payload: MoveMotifEvidence =>
        payload.moveUci == moveUci
      case MoveTransitionEvidence(move, _, _) =>
        move == moveUci
      case payload: RelationFactEvidence =>
        payload.mentionsLineMove(moveUci) || record.ref.scope == EvidenceScope.PlayedTransition
      case payload: TacticalMechanismEvidence =>
        payload.moveUci.exists(EvidenceRef.sameMove(_, moveUci)) ||
          payload.line.exists(_.rootMove == moveUci) ||
          record.ref.scope == EvidenceScope.PlayedTransition
      case _ =>
        false

  private def relativeEvidenceRefs(assessments: List[RelativeMoveAssessment]): List[EvidenceRef] =
    assessments.flatMap(assessment => assessment.evidence :: assessment.counterfactualEvidence).distinctBy(_.id)

  private def relativeAssessmentsForLine(
      context: JudgmentAssemblyContext,
      line: LineNodeRef
  ): List[RelativeMoveAssessment] =
    context.relativeAssessments.filter(assessment =>
      assessment.candidate.ref == line ||
        assessment.reference.ref == line ||
        assessment.comparison.candidateLine == line ||
        assessment.comparison.referenceLine == line
    )

  private def tacticalIdeaConfidence(
      driver: TacticalIdeaDriver,
      relativeAssessments: List[RelativeMoveAssessment],
      evalRecords: List[EvidenceRecord]
  ): EvidenceConfidence =
    if evalRecords.exists {
        case EvidenceRecord(_, EvalFactEvidence(_, _, mate, _), _) => mate.nonEmpty
        case _                                                     => false
      } || relativeAssessments.exists(relativeSupportsTacticalIdea)
    then EvidenceConfidence.EngineBacked
    else if driver == TacticalIdeaDriver.RelationMechanism then EvidenceConfidence.LegalReplayVerified
    else EvidenceConfidence.Mixed

  private def lineLayerRefs(
      context: JudgmentAssemblyContext,
      line: LineNodeRef
  ): List[EvidenceRef] =
    context.evidenceGraph.recordsFor(line).collect {
      case record if record.ref.layer == EvidenceLayer.Line || record.ref.layer == EvidenceLayer.Eval => record.ref
    }

  private def recordsForPosition(
      context: JudgmentAssemblyContext,
      layer: EvidenceLayer,
      position: PositionNodeRef
  ): List[EvidenceRef] =
    context.evidenceGraph.recordsFor(position).collect {
      case record if record.ref.layer == layer => record.ref
    }
