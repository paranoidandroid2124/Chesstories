package lila.chessjudgment.analysis.assembly

import chess.Pawn
import chess.format.{ Fen, Uci }
import lila.chessjudgment.model.evaluation.JudgmentThresholds
import lila.chessjudgment.analysis.policy.ClaimTruthPolicy
import lila.chessjudgment.model.judgment.*

object JudgmentClaimAssembler:

  private enum TacticalClaimDriver(val id: String):
    case KingForcing extends TacticalClaimDriver("king-forcing")
    case MaterialGain extends TacticalClaimDriver("material-gain")
    case RecaptureChoice extends TacticalClaimDriver("recapture-choice")
    case Tempo extends TacticalClaimDriver("tempo")
    case RelationMechanism extends TacticalClaimDriver("relation-mechanism")
    case Conversion extends TacticalClaimDriver("conversion")
    case Refutation extends TacticalClaimDriver("refutation")
    case DrawResource extends TacticalClaimDriver("draw-resource")
    case PawnPromotion extends TacticalClaimDriver("pawn-promotion")

  private final case class OpeningMoveBinding(
      subject: ClaimSubject,
      primaryLine: Option[LineNodeRef],
      moveUci: Option[String],
      evidence: List[EvidenceRef]
  )

  def assemble(raw: RawMoveReviewInput): Option[JudgmentAssemblyContext] =
    RelativeAssessmentAssembler.assemble(raw).map(enrich)

  def enrich(context: JudgmentAssemblyContext): JudgmentAssemblyContext =
    val allocator = JudgmentProvenanceAllocator.forInput(context.input)
    val claimCandidates =
      consistentClaimsById(
        List.concat(
          tacticalClaims(context, allocator),
          pawnStructureClaims(context, allocator),
          openingClaims(context, allocator),
          defensiveClaims(context, allocator),
          relativeCauseClaims(context, allocator),
          evaluationClaims(context, allocator),
          strategicClaims(context, allocator)
        )
      )
    val candidateGraph = ClaimCandidateGraphAssembler.fromClaims(claimCandidates, context.evidenceGraph)
    val claims = ClaimArbitrator.rank(candidateGraph, context.relativeAssessments)
    claims.foldLeft(context)((ctx, claim) => ctx.withClaim(claim))

  private def consistentClaimsById(claims: List[JudgmentClaim]): List[JudgmentClaim] =
    claims.foldLeft(List.empty[JudgmentClaim]) { (accepted, claim) =>
      accepted.find(_.id == claim.id) match
        case None =>
          accepted :+ claim
        case Some(existing) if existing == claim =>
          accepted
        case Some(_) =>
          throw IllegalArgumentException(
            s"claim id collision for '${claim.id}': independently produced claims differ"
          )
    }

  private def judgmentClaimFromEvidence(
      id: String,
      family: ClaimFamily,
      subject: ClaimSubject,
      primaryPosition: PositionNodeRef,
      primaryLine: Option[LineNodeRef],
      moveUci: Option[String],
      evidence: List[EvidenceRef],
      scope: EvidenceScope,
      confidence: EvidenceConfidence
  ): JudgmentClaim =
    val canonicalFamily =
      if subject == ClaimSubject.Plan then ClaimFamily.Plan
      else family
    JudgmentClaim(
      id = id,
      family = canonicalFamily,
      subject = subject,
      primaryPosition = primaryPosition,
      primaryLine = primaryLine,
      subjectMove = moveUci,
      evidence = evidence,
      scope = scope,
      confidence = confidence
    )

  private def tacticalClaims(
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator
  ): List[JudgmentClaim] =
    consistentClaimsById(
      context.lines.flatMap(line => compositeTacticalClaims(context, allocator, line)) ++
        playedTransitionTacticalClaims(context, allocator)
    )

  private def playedTransitionTacticalClaims(
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator
  ): List[JudgmentClaim] =
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
            case record @ EvidenceRecord(_, payload: TacticalMechanismEvidence, _) if payload.canAnchorTacticalClaim =>
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
            judgmentClaimFromEvidence(
              id = allocator.evidenceId(s"claim:tactical:${driver.id}:played-transition:${edge.moveUci}"),
              family = ClaimFamily.Tactical,
              subject = ClaimSubject.PlayedMove,
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

  private def compositeTacticalClaims(
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator,
      line: CandidateLineNode
  ): List[JudgmentClaim] =
    val lineRecords = context.evidenceGraph.recordsFor(line.ref)
    val lineFactRecords = lineRecords.collect { case record @ EvidenceRecord(_, _: LineFactEvidence, _) => record }
    val evalRecords = lineRecords.collect { case record @ EvidenceRecord(_, _: EvalFactEvidence, _) => record }
    val mechanismRecords =
      lineRecords.collect {
        case record @ EvidenceRecord(_, payload: TacticalMechanismEvidence, _) if payload.canAnchorTacticalClaim =>
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
          judgmentClaimFromEvidence(
            id = allocator.evidenceId(s"claim:tactical:${driver.id}:${allocator.key(line.role)}:${line.ref.rank}:${line.ref.rootMove}"),
            family = ClaimFamily.Tactical,
            subject = line.ref.role.subject,
            primaryPosition = position,
            primaryLine = Some(line.ref),
            moveUci = Some(line.ref.rootMove),
            evidence = evidence.distinctBy(_.id),
            scope = line.ref.role.scope,
            confidence = tacticalClaimConfidence(context.evidenceGraph, driver, relative, evalRecords)
          )
        }
      }
    }

  private def pawnStructureClaims(
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator
  ): List[JudgmentClaim] =
    context.evidenceGraph.records.flatMap {
      case EvidenceRecord(ref, payload: StrategicMechanismEvidence, parents)
          if payload.canAnchorPawnStructureClaim =>
        val primaryLine = pawnStructureMechanismPrimaryLine(context, ref)
        val evidence =
          longTermClaimEvidence(
            ref :: parents ++
              pawnStructureMechanismTransitionEvidence(context, ref) ++
              primaryLine.toList.flatMap(lineLayerRefs(context, _)) ++
              recordsForPosition(context, EvidenceLayer.Board, ref.position)
          )
        Option.when(primaryLine.nonEmpty && evidence.nonEmpty) {
          judgmentClaimFromEvidence(
            id = allocator.evidenceId(s"claim:pawn-structure-mechanism:${allocator.key(ref.id)}"),
            family = ClaimFamily.PawnStructure,
            subject = primaryLine.map(_.role.subject).getOrElse(ClaimSubject.Position),
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

  private def defensiveClaims(
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator
  ): List[JudgmentClaim] =
    val threatEpisodeClaims = context.evidenceGraph.records.collect {
      case EvidenceRecord(ref, payload: ThreatEpisodeEvidence, parents)
          if ref.position.sideToMove.forall(_ == payload.sideUnderPressure) &&
            payload.canAnchorDefensiveResource =>
        val evidence =
          (ref :: parents ++
            ref.line.toList.flatMap(lineLayerRefs(context, _))).distinctBy(_.id)
        judgmentClaimFromEvidence(
          id = allocator.evidenceId(s"claim:defensive:${allocator.key(ref.id)}"),
          family = ClaimFamily.Defensive,
          subject = ClaimSubject.Threat,
          primaryPosition = ref.position,
          primaryLine = ref.line,
          moveUci = payload.onlyDefense.orElse(payload.episode.bestDefense),
          evidence = evidence,
          scope = ref.scope,
          confidence = ref.confidence
        )
    }
    val mechanismClaims = context.evidenceGraph.records.collect {
      case EvidenceRecord(ref, payload: TacticalMechanismEvidence, parents) if payload.canAnchorDefensiveClaim =>
        val evidence =
          (ref :: parents ++
            payload.line.toList.flatMap(lineLayerRefs(context, _))).distinctBy(_.id)
        judgmentClaimFromEvidence(
          id = allocator.evidenceId(s"claim:defensive-mechanism:${allocator.key(ref.id)}"),
          family = ClaimFamily.Defensive,
          subject = ClaimSubject.Threat,
          primaryPosition = ref.position,
          primaryLine = payload.line,
          moveUci = payload.moveUci,
          evidence = evidence,
          scope = ref.scope,
          confidence = ref.confidence
        )
    }
    consistentClaimsById(threatEpisodeClaims ++ mechanismClaims)

  private def evaluationClaims(
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator
  ): List[JudgmentClaim] =
    context.relativeAssessments.map { assessment =>
      val evidence =
        (assessment.evidence ::
          primaryCandidateComparisonEvidence(context, assessment) ++
          lineLayerRefs(context, assessment.reference.ref) ++
          lineLayerRefs(context, assessment.candidate.ref)).distinctBy(_.id)
      judgmentClaimFromEvidence(
        id = allocator.evidenceId(s"claim:evaluation:${allocator.key(assessment.evidence.id)}"),
        family = ClaimFamily.Evaluation,
        subject = ClaimSubject.PlayedMove,
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
    context.evidenceGraph
      .candidateComparisonRecord(assessment.primaryComparisonEvidence)
      .filter {
        case EvidenceRecord(_, CandidateComparisonEvidence(fact), _) =>
          fact.kind == CandidateComparisonKind.PlayedVsBest &&
            fact.referenceLine == assessment.reference.ref &&
            fact.candidateLine == assessment.candidate.ref
        case _ =>
          false
      }
      .map(_.ref)
      .toList

  private def relativeCauseClaims(
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator
  ): List[JudgmentClaim] =
    context.evidenceGraph.records.flatMap {
      case EvidenceRecord(ref, RelativeCauseFactEvidence(cause), parents) =>
        relativeCauseClaimsFromRecord(context, allocator, ref, cause, parents)
      case _ =>
        Nil
    }

  private def relativeCauseClaimsFromRecord(
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator,
      ref: EvidenceRef,
      cause: RelativeCauseFact,
      parents: List[EvidenceRef]
  ): List[JudgmentClaim] =
    val subjectLine = context.evidenceGraph.requiredRelativeCauseBinding(cause).eventLine
    val supportRefs = relativeCauseClaimSupportRefs(context, cause, parents)
    val depthProofRefs = relativeCauseClaimDepthProofRefs(cause)
    familiesForRelativeCause(context, ref, cause, supportRefs).map { family =>
      val familyEvidence = relativeCauseClaimEvidence(context, ref, supportRefs, depthProofRefs, family)
      judgmentClaimFromEvidence(
        id = relativeCauseClaimId(context.evidenceGraph, allocator, family, cause, subjectLine, ref),
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

  private def relativeCauseClaimId(
      graph: TypedEvidenceGraph,
      allocator: JudgmentProvenanceAllocator,
      family: ClaimFamily,
      cause: RelativeCauseFact,
      subjectLine: LineNodeRef,
      ref: EvidenceRef
  ): String =
    val key = cause.identityKey(graph)
    val identityHash = Integer.toHexString(key.toString.hashCode)
    allocator.evidenceId(
      s"claim:${allocator.key(family)}:relative-cause:${allocator.key(key.kind)}:${allocator.key(key.comparisonKind)}:${allocator.key(key.role)}:${allocator.key(key.sourceSide)}:${allocator.key(key.importance)}:${allocator.key(subjectLine.rootMove)}:$identityHash:${allocator.key(ref.id)}"
    )

  private def relativeCauseClaimSupportRefs(
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

  private def relativeCauseClaimDepthProofRefs(
      cause: RelativeCauseFact
  ): List[EvidenceRef] =
    val proofSources =
      cause.proof.toList.flatMap(proof => proof.directProof.sourceRefs ++ proof.contrastProof.sourceRefs)
    proofSources.distinctBy(_.id)

  private def relativeCauseClaimEvidence(
      context: JudgmentAssemblyContext,
      ref: EvidenceRef,
      supportRefs: List[EvidenceRef],
      depthProofRefs: List[EvidenceRef],
      family: ClaimFamily
  ): List[EvidenceRef] =
    val supportEvidence = (ref :: supportRefs).distinctBy(_.id)
    val depthEvidence = (ref :: depthProofRefs).distinctBy(_.id)
    family match
      case ClaimFamily.Tactical | ClaimFamily.Material | ClaimFamily.Defensive =>
        depthEvidence
      case ClaimFamily.Conversion =>
        (depthEvidence ++ conversionContextEvidence(context, ref.position)).distinctBy(_.id)
      case ClaimFamily.Strategic =>
        (ref :: longTermClaimEvidence(supportRefs)).distinctBy(_.id)
      case ClaimFamily.PawnStructure | ClaimFamily.Opening =>
        (ref :: longTermClaimEvidence(supportRefs)).distinctBy(_.id)
      case _ =>
        supportEvidence

  private def familyForRelativeCause(kind: RelativeCauseKind): ClaimFamily =
    ClaimFamily.fromCause(kind).getOrElse(ClaimFamily.Strategic)

  private def familiesForRelativeCause(
      context: JudgmentAssemblyContext,
      ref: EvidenceRef,
      cause: RelativeCauseFact,
      supportRefs: List[EvidenceRef]
  ): List[ClaimFamily] =
    val supportRecords = recordsForRefs(context, supportRefs)
    cause.kind match
      case RelativeCauseKind.MaterialSwing =>
        val promoted =
          List(
            Option.when(cause.hasOwnedTypedDepth(context.evidenceGraph))(ClaimFamily.Material),
            Option.when(
              materialSwingHasTacticalProof(context.evidenceGraph, cause)
            )(ClaimFamily.Tactical),
            Option.when(
              cause.hasOwnedTypedDepth(context.evidenceGraph) &&
                hasConversionContext(context, ref.position, supportRefs)
            )(ClaimFamily.Conversion)
          ).flatten
        promoted.distinct
      case RelativeCauseKind.SacrificeCompensation =>
        val promoted =
          List(
            Option.when(cause.hasOwnedTypedDepth(context.evidenceGraph))(ClaimFamily.Material),
            Option.when(relativeCauseHasTacticalProof(cause, context.evidenceGraph))(ClaimFamily.Tactical)
          ).flatten
        promoted.distinct
      case kind if strategicRelativeCause(kind) =>
        List(
          Option.when(
            cause.hasOwnedAdmissibleLongTermProof(context.evidenceGraph) &&
              hasStrategicRelativeCauseSupport(cause, supportRecords, context.evidenceGraph)
          )(
            ClaimFamily.Strategic
          ),
          Option.when(
            cause.hasOwnedAdmissibleLongTermProof(context.evidenceGraph) &&
              hasPawnStructureRelativeCauseSupport(cause, supportRecords, context.evidenceGraph)
          )(
            ClaimFamily.PawnStructure
          ),
          Option.when(
            cause.hasOwnedAdmissibleLongTermProof(context.evidenceGraph) &&
              hasOpeningRelativeCauseSupport(supportRecords)
          )(
            ClaimFamily.Opening
          ),
          Option.when(relativeCauseHasTacticalProof(cause, context.evidenceGraph))(
            ClaimFamily.Tactical
          )
        ).flatten.distinct
      case _ =>
        val base = familyForRelativeCause(cause.kind)
        val baseFamily =
          Option.when(
            (base != ClaimFamily.Tactical || relativeCauseHasTacticalProof(cause, context.evidenceGraph)) &&
              (base != ClaimFamily.Material || cause.hasOwnedTypedDepth(context.evidenceGraph)) &&
              (base != ClaimFamily.Conversion || cause.hasOwnedTypedDepth(context.evidenceGraph)) &&
              (base != ClaimFamily.Defensive ||
                ClaimTruthPolicy.defensiveRelativeCauseCanSupportClaim(cause, context.evidenceGraph))
          )(base)
        val conversionFamily =
          Option.when(
            materialConversionCause(cause.kind) &&
              cause.hasOwnedTypedDepth(context.evidenceGraph) &&
            hasConversionContext(context, ref.position, supportRefs)
          )(ClaimFamily.Conversion)
        (baseFamily.toList ++ conversionFamily.toList).distinct

  private def materialConversionCause(kind: RelativeCauseKind): Boolean =
    kind == RelativeCauseKind.RecaptureRecoveryWindow || kind == RelativeCauseKind.MaterialSwing

  private def materialSwingHasTacticalProof(graph: TypedEvidenceGraph, cause: RelativeCauseFact): Boolean =
    val engineBackedMaterialSwing =
      graph.comparisonFor(cause).exists(fact =>
        fact.comparison.winPercentLossForMover >= JudgmentThresholds.INACCURACY_WP ||
          fact.comparison.candidateWinPercentDeltaForMover >= JudgmentThresholds.PLAYABLE_LOSS_WP
      )
    engineBackedMaterialSwing && relativeCauseHasTacticalProof(cause, graph)

  private def relativeCauseHasTacticalProof(
      cause: RelativeCauseFact,
      graph: TypedEvidenceGraph
  ): Boolean =
    cause.hasOwnedTacticalProof(graph)

  private def strategicRelativeCause(kind: RelativeCauseKind): Boolean =
    RelativeCauseKind.strategicContrastBacked(kind)

  private def recordsForRefs(
      context: JudgmentAssemblyContext,
      refs: List[EvidenceRef]
  ): List[EvidenceRecord] =
    refs.flatMap(ref => context.evidenceGraph.byId.get(ref.id))

  private def hasStrategicRelativeCauseSupport(
      cause: RelativeCauseFact,
      records: List[EvidenceRecord],
      graph: TypedEvidenceGraph
  ): Boolean =
    cause.proof.exists(proof =>
      graph.relativeCauseTransitionConsequences(cause.kind, proof.directProof).nonEmpty
    ) ||
    records.exists {
      case EvidenceRecord(_, payload: StrategicMechanismContrastEvidence, _) =>
        payload.hasActionableContrast
      case EvidenceRecord(_, payload: StrategicMechanismEvidence, _) =>
        payload.canSupportStrategicCause
      case _ =>
        false
    }

  private def hasPawnStructureRelativeCauseSupport(
      cause: RelativeCauseFact,
      records: List[EvidenceRecord],
      graph: TypedEvidenceGraph
  ): Boolean =
    cause.kind == RelativeCauseKind.PawnWeaknessTarget ||
      cause.kind == RelativeCauseKind.PawnBreakOpportunity ||
      cause.proof.exists(proof =>
        graph
          .relativeCauseTransitionConsequences(cause.kind, proof.directProof)
          .exists { case (_, _, consequence) =>
            consequence.kind == TransitionConsequenceKind.WeakPawnTargetCreated
          }
      ) ||
    records.exists {
      case EvidenceRecord(_, payload: StrategicMechanismContrastEvidence, _) =>
        payload.actionableComparisons.exists(_.axis.kind == StrategicAxisKind.PawnBreak)
      case EvidenceRecord(_, payload: StrategicMechanismEvidence, _) =>
        payload.canAnchorPawnStructureClaim
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
    ClaimTruthPolicy.conversionContextCanSupportClaim(conversionContextRecords(context, position, parents))

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
      .filter(record => ClaimTruthPolicy.conversionContextCanSupportClaim(List(record)))
      .distinctBy(_.ref.id)

  private def subjectForRelativeCause(cause: RelativeCauseFact, line: LineNodeRef): ClaimSubject =
    cause.kind match
      case kind if ClaimFamily.fromCause(kind).contains(ClaimFamily.Defensive) =>
        ClaimSubject.Threat
      case RelativeCauseKind.PlanImprovement | RelativeCauseKind.PlanContradiction =>
        ClaimSubject.Plan
      case _ =>
        line.role.subject

  private def strategicClaims(
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator
  ): List[JudgmentClaim] =
    context.evidenceGraph.records.flatMap {
      case EvidenceRecord(ref, payload: StrategicMechanismEvidence, parents)
          if lineBoundLongTermMechanism(ref) && (payload.canAnchorStrategicClaim || payload.canAnchorPlanClaim) =>
        val subject =
          if payload.kind == StrategicMechanismKind.PlanPressure && payload.canAnchorPlanClaim then ClaimSubject.Plan
          else ref.line.map(_.role.subject).getOrElse(ClaimSubject.Position)
        val evidence =
          longTermClaimEvidence(
              ref :: parents ++
              ref.line.toList.flatMap(lineLayerRefs(context, _)) ++
              recordsForPosition(context, EvidenceLayer.Board, ref.position)
          )
        Option.when(evidence.nonEmpty) {
          judgmentClaimFromEvidence(
            id = allocator.evidenceId(s"claim:strategic-mechanism:${allocator.key(ref.id)}"),
            family = ClaimFamily.Strategic,
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

  private def openingClaims(
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator
  ): List[JudgmentClaim] =
    context.evidenceGraph.records.flatMap {
      case record @ EvidenceRecord(ref, payload: StrategicMechanismEvidence, parents)
          if payload.canAnchorOpeningClaim =>
        val supportRecords = openingMechanismSupport(context, record)
        val assessment = supportRecords.collectFirst {
          case EvidenceRecord(_, ApplicabilityAssessmentEvidence(assessment), _) => assessment
        }
        val moveBinding = assessment.flatMap(openingMoveBinding(context, _, supportRecords.map(_.ref)))
        val primaryLine = moveBinding.flatMap(_.primaryLine)
        val evidence =
          longTermClaimEvidence(
            ref :: parents ++
              supportRecords.map(_.ref) ++
              moveBinding.toList.flatMap(_.evidence) ++
              primaryLine.toList.flatMap(lineLayerRefs(context, _))
          )
        Option.when(StrategicMechanismEvidence.openingClaimSupported(supportRecords) && evidence.nonEmpty) {
          judgmentClaimFromEvidence(
            id = allocator.evidenceId(s"claim:opening-mechanism:${allocator.key(ref.id)}"),
            family = ClaimFamily.Opening,
            subject = moveBinding.map(_.subject).getOrElse(ClaimSubject.Position),
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
      openingLineBinding(sourceRecords, LineNodeRole.Played, ClaimSubject.PlayedMove),
      openingLineBinding(sourceRecords, LineNodeRole.BestReference, ClaimSubject.ReferenceMove),
      openingLineBinding(sourceRecords, LineNodeRole.Alternative, ClaimSubject.CandidateLine)
    ).flatten.sortBy(openingMoveBindingScore).lastOption

  private def openingLineBinding(
      sourceRecords: List[EvidenceRecord],
      role: LineNodeRole,
      subject: ClaimSubject
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
        case ClaimSubject.PlayedMove     => 2
        case ClaimSubject.ReferenceMove  => 1
        case ClaimSubject.CandidateLine  => 0
        case ClaimSubject.Position | ClaimSubject.Threat | ClaimSubject.Plan => 0
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

  private def relativeSupportsTacticalClaim(
      graph: TypedEvidenceGraph,
      assessment: RelativeMoveAssessment
  ): Boolean =
    graph.comparisonFor(assessment).exists(fact =>
      fact.comparison.winPercentLossForMover >= JudgmentThresholds.SIGNIFICANT_THREAT_WP ||
        fact.comparison.candidateWinPercentDeltaForMover >= JudgmentThresholds.PLAYABLE_LOSS_WP ||
        graph.candidateSetFor(fact).exists(_.onlyMove)
    )

  private def tacticalDriverForMechanism(kind: TacticalMechanismKind): TacticalClaimDriver =
    kind match
      case TacticalMechanismKind.KingForcing =>
        TacticalClaimDriver.KingForcing
      case TacticalMechanismKind.MaterialGain =>
        TacticalClaimDriver.MaterialGain
      case TacticalMechanismKind.RecaptureChoice =>
        TacticalClaimDriver.RecaptureChoice
      case TacticalMechanismKind.Tempo =>
        TacticalClaimDriver.Tempo
      case TacticalMechanismKind.RelationMechanism =>
        TacticalClaimDriver.RelationMechanism
      case TacticalMechanismKind.Conversion =>
        TacticalClaimDriver.Conversion
      case TacticalMechanismKind.Refutation =>
        TacticalClaimDriver.Refutation
      case TacticalMechanismKind.DrawResource =>
        TacticalClaimDriver.DrawResource
      case TacticalMechanismKind.PawnPromotion =>
        TacticalClaimDriver.PawnPromotion
      case TacticalMechanismKind.DefensiveResource =>
        TacticalClaimDriver.RelationMechanism

  private def compositeMechanismEvidence(
      context: JudgmentAssemblyContext,
      position: PositionNodeRef,
      driver: TacticalClaimDriver,
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
      driver: TacticalClaimDriver,
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

  private def longTermClaimEvidence(refs: List[EvidenceRef]): List[EvidenceRef] =
    refs
      .filterNot(ref => ClaimEvidenceSemantics.longTermSupportExcludedLayer(ref.layer))
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
    assessments
      .flatMap(assessment =>
        assessment.evidence :: assessment.primaryComparisonEvidence :: assessment.relatedComparisonEvidence
      )
      .distinctBy(_.id)

  private def relativeAssessmentsForLine(
      context: JudgmentAssemblyContext,
      line: LineNodeRef
  ): List[RelativeMoveAssessment] =
    context.relativeAssessments.filter(assessment =>
      assessment.candidate.ref == line ||
        assessment.reference.ref == line
    )

  private def tacticalClaimConfidence(
      graph: TypedEvidenceGraph,
      driver: TacticalClaimDriver,
      relativeAssessments: List[RelativeMoveAssessment],
      evalRecords: List[EvidenceRecord]
  ): EvidenceConfidence =
    if evalRecords.exists {
        case EvidenceRecord(_, EvalFactEvidence(_, _, mate, _), _) => mate.nonEmpty
        case _                                                     => false
      } || relativeAssessments.exists(relativeSupportsTacticalClaim(graph, _))
    then EvidenceConfidence.EngineBacked
    else if driver == TacticalClaimDriver.RelationMechanism then EvidenceConfidence.LegalReplayVerified
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
