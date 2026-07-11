package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.analysis.evaluation.JudgmentThresholds
import lila.chessjudgment.analysis.policy.{ ClaimTruthDecision, ClaimTruthPolicy, ClaimTruthStatus }
import lila.chessjudgment.model.judgment.*

final case class ClaimCandidateGraph(
    decisions: List[ClaimTruthDecision],
    evidenceGraph: TypedEvidenceGraph
):
  def certified: List[ClaimTruthDecision] =
    decisions.filter(_.status == ClaimTruthStatus.Certified)

  def deferred: List[ClaimTruthDecision] =
    decisions.filter(_.status == ClaimTruthStatus.Deferred)

  def rejected: List[ClaimTruthDecision] =
    decisions.filter(_.status == ClaimTruthStatus.Rejected)

object ClaimCandidateGraphAssembler:

  def fromClaims(
      claims: List[ClaimSeed],
      graph: TypedEvidenceGraph
  ): ClaimCandidateGraph =
    ClaimCandidateGraph(claims.map(ClaimTruthPolicy.evaluate(_, graph)), graph)

private object RelativeCauseClaimDepth:

  def hasOwnedDepth(cause: RelativeCauseFact): Boolean =
    cause.hasOwnedTypedDepth

  def hasTacticalDepth(cause: RelativeCauseFact): Boolean =
    isTactical(cause.kind) &&
      cause.hasOwnedTacticalProof

  def hasMaterialDepth(cause: RelativeCauseFact): Boolean =
      ClaimEventCluster.kindForCause(cause.kind).contains(ClaimEventClusterKind.MaterialEvent) &&
      hasOwnedDepth(cause)

  def hasConversionDepth(cause: RelativeCauseFact): Boolean =
    (
      ClaimEventCluster.kindForCause(cause.kind).contains(ClaimEventClusterKind.ConversionEvent) ||
        cause.kind == RelativeCauseKind.RecaptureRecoveryWindow ||
      cause.kind == RelativeCauseKind.MaterialSwing
    ) &&
      hasOwnedDepth(cause)

  def hasMaterialSwingTacticalDepth(cause: RelativeCauseFact): Boolean =
    cause.kind == RelativeCauseKind.MaterialSwing &&
      ClaimEventCluster.kindForCause(cause.kind).contains(ClaimEventClusterKind.MaterialEvent) &&
      cause.hasOwnedTacticalProof

  def hasLineDriver(proof: RelativeCauseProof): Boolean =
    proof.directProof.lineConsequences.exists(proof => LineConsequenceKind.tacticalDriver(proof.kind)) ||
      proof.contrastProof.lineConsequences.exists(proof => LineConsequenceKind.tacticalDriver(proof.kind))

  def hasTransitionDriver(proof: RelativeCauseProof): Boolean =
    proof.directProof.transitionConsequences.nonEmpty ||
      proof.contrastProof.transitionConsequences.nonEmpty

  def hasStrategicContrastDepth(cause: RelativeCauseFact): Boolean =
    cause.hasOwnedAdmissibleLongTermProof

  private def isTactical(kind: RelativeCauseKind): Boolean =
    ClaimEventCluster.kindForCause(kind).contains(ClaimEventClusterKind.TacticalEvent)

object ClaimDeduplicator:

  final case class ClaimDeduplicationResult(
      winners: List[ClaimTruthDecision],
      droppedByClaimId: Map[String, String],
      sourceCandidateIdsByWinnerId: Map[String, List[String]]
  )

  private final case class AggregatedClaimDecision(
      index: Int,
      decision: ClaimTruthDecision,
      sourceClaimIds: List[String]
  )

  private def aggregateCertifiedWithSources(
      decisions: List[ClaimTruthDecision],
      graph: TypedEvidenceGraph
  ): List[AggregatedClaimDecision] =
    val grouped =
      decisions.zipWithIndex
        .collect { case (decision, index) if aggregationEligible(decision.claim, graph) =>
          aggregationKey(decision.claim, graph) -> (decision, index)
        }
        .groupBy(_._1)
    val aggregated =
      grouped.values.map { entries =>
        val decisionsInGroup = entries.map(_._2._1)
        AggregatedClaimDecision(
          index = entries.map(_._2._2).min,
          decision = mergeDecisions(decisionsInGroup, graph),
          sourceClaimIds = decisionsInGroup.map(_.claim.id).distinct
        )
      }.toList
    val passthrough =
      decisions.zipWithIndex.collect { case (decision, index) if !aggregationEligible(decision.claim, graph) =>
        AggregatedClaimDecision(index, decision, List(decision.claim.id))
      }
    (passthrough ++ aggregated).sortBy(_.index)

  def deduplicateWithDiagnostics(
      decisions: List[ClaimTruthDecision],
      graph: TypedEvidenceGraph
  ): ClaimDeduplicationResult =
    val aggregated = aggregateCertifiedWithSources(decisions, graph)
    val winners =
      aggregated
        .groupBy(item => key(item.decision.claim))
        .values
        .map(_.maxBy(item => score(item.decision, graph)))
        .toList
    val winnerIds = winners.map(_.decision.claim.id).toSet
    val winnerByKey =
      winners.map(item => key(item.decision.claim) -> item.decision.claim.id).toMap
    val droppedByAggregation =
      winners.flatMap(item =>
        item.sourceClaimIds.filterNot(_ == item.decision.claim.id).map(_ -> item.decision.claim.id)
      )
    val droppedByDedupe =
      aggregated
        .filterNot(item => winnerIds.contains(item.decision.claim.id))
        .flatMap(item =>
          item.sourceClaimIds.map(sourceId => sourceId -> winnerByKey.getOrElse(key(item.decision.claim), item.decision.claim.id))
        )
    ClaimDeduplicationResult(
      winners = winners.map(_.decision),
      droppedByClaimId = (droppedByAggregation ++ droppedByDedupe).toMap,
      sourceCandidateIdsByWinnerId = winners.map(item => item.decision.claim.id -> item.sourceClaimIds.distinct.sorted).toMap
    )

  private def key(claim: ClaimSeed): (ClaimFamily, IdeaSubject, Option[LineNodeRef], Option[String], Option[ChessIdeaRef]) =
    (
      claim.family,
      claim.subject,
      claim.primaryLine,
      claim.subjectMove,
      claim.idea
    )

  private final case class AggregationKey(
      family: ClaimFamily,
      subject: IdeaSubject,
      primaryPosition: PositionNodeRef,
      primaryLine: Option[LineNodeRef],
      subjectMove: Option[String],
      scope: Option[EvidenceScope],
      semanticAnchors: List[EvidenceSemanticAnchor]
  )

  private def aggregationEligible(
      claim: ClaimSeed,
      graph: TypedEvidenceGraph
  ): Boolean =
    claim.family.isLongTerm &&
      claim.engineComparison.isEmpty &&
      claim.evidence.nonEmpty &&
      !claim.evidence.exists(ref => ClaimSupportCluster.longTermSupportExcludedLayer(ref.layer)) &&
      !claim.evidence.exists(ref => StrategicMechanismEvidence.rawStrategicSourceLayer(ref.layer)) &&
      ClaimSupportCluster.semanticAnchors(claim, graph).nonEmpty

  private def aggregationKey(
      claim: ClaimSeed,
      graph: TypedEvidenceGraph
  ): AggregationKey =
    AggregationKey(
      family = claim.family,
      subject = claim.subject,
      primaryPosition = claim.primaryPosition,
      primaryLine = claim.primaryLine,
      subjectMove = claim.subjectMove,
      scope = scopeAnchor(claim),
      semanticAnchors = ClaimSupportCluster.semanticAnchors(claim, graph)
    )

  private def scopeAnchor(claim: ClaimSeed): Option[EvidenceScope] =
    Option.when(claim.primaryLine.nonEmpty || claim.subjectMove.nonEmpty)(claim.scope)

  private def mergeDecisions(decisions: Iterable[ClaimTruthDecision], graph: TypedEvidenceGraph): ClaimTruthDecision =
    val list = decisions.toList
    val mergedClaim = mergeSeeds(list.map(_.claim), graph)
    val mergedPresentLayers = list.flatMap(_.presentLayers).toSet
    val mergedMissingLayerGroups = list.flatMap(_.missingLayerGroups).distinct
    val mergedMissingEvidence = list.flatMap(_.missingEvidence).distinctBy(_.id)
    list.maxBy(decision => score(decision, graph)).copy(
      claim = mergedClaim,
      presentLayers = mergedPresentLayers,
      missingLayerGroups = mergedMissingLayerGroups,
      missingEvidence = mergedMissingEvidence
    )

  private def mergeSeeds(claims: Iterable[ClaimSeed], graph: TypedEvidenceGraph): ClaimSeed =
    val seeds = claims.toList
    val mergeBase = seeds.maxBy(seed => seedScore(seed, graph))
    if seeds.size == 1 then mergeBase
    else
      mergeBase.copy(
        evidence = seeds.flatMap(_.evidence).distinctBy(_.id),
        confidence = seeds.map(_.confidence).maxBy(confidenceScore),
        relatedIdeas = seeds.flatMap(_.ideaRefs).distinctBy(_.id),
        supportStatus = None,
        salience = None
      )

  private def seedScore(claim: ClaimSeed, graph: TypedEvidenceGraph): Int =
    claimEvidencePriority(claim, graph) * 10 +
      claim.engineComparison.map(_ => 50).getOrElse(0) +
      confidenceScore(claim.confidence)

  private def score(decision: ClaimTruthDecision, graph: TypedEvidenceGraph): Int =
    claimEvidencePriority(decision.claim, graph) * 10 +
      confidenceScore(decision.claim.confidence)

  private def claimEvidencePriority(claim: ClaimSeed, graph: TypedEvidenceGraph): Int =
    val weightedEvidence =
      claim.evidence
        .flatMap(ref => graph.byId.get(ref.id))
        .filter(record => priorityEvidenceRecord(claim.family, record))
        .map(_.ref.id)
        .distinct
        .size
    if claim.evidence.nonEmpty then weightedEvidence.max(1) else 0

  private def priorityEvidenceRecord(family: ClaimFamily, record: EvidenceRecord): Boolean =
    family match
      case ClaimFamily.Tactical =>
        tacticalPriorityEvidenceRecord(record)
      case family if family.isLongTerm =>
        !ClaimSupportCluster.longTermSupportExcludedLayer(record.ref.layer) &&
          !StrategicMechanismEvidence.rawStrategicSourceLayer(record.ref.layer)
      case _ =>
        true

  private def tacticalPriorityEvidenceRecord(record: EvidenceRecord): Boolean =
    record.payload match
      case _: TacticalMechanismEvidence | _: RelationFactEvidence | _: MoveMotifEvidence | _: LineFactEvidence |
          EvalFactEvidence(_, _, _, _) | _: ThreatEpisodeEvidence | _: ThreatPressureEvidence |
          MoveTransitionEvidence(_, _, _) | RelativeAssessmentEvidence(_) |
          CounterfactualFactEvidence(_, _, _) | CandidateComparisonEvidence(_) =>
        true
      case RelativeCauseFactEvidence(cause) =>
        RelativeCauseClaimDepth.hasTacticalDepth(cause)
      case MoveVerdictCertificationEvidence(certification) =>
        certification.causes.exists(RelativeCauseClaimDepth.hasTacticalDepth)
      case _ =>
        false

  private def confidenceScore(confidence: EvidenceConfidence): Int =
    confidence match
      case EvidenceConfidence.LegalReplayVerified => 40
      case EvidenceConfidence.EngineBacked        => 30
      case EvidenceConfidence.BoardDerived        => 25
      case EvidenceConfidence.Mixed               => 15
      case EvidenceConfidence.Heuristic           => 5

final case class ClaimArbitrationResult(
    claims: List[ClaimSeed],
    lifecycle: List[ClaimLifecycleDiagnostic]
)

object ClaimArbitrator:

  private[assembly] def relativeCauseSalienceForAudit(cause: RelativeCauseFact): Int =
    relativeCauseSalience(cause)

  def rank(
      graph: ClaimCandidateGraph,
      relativeAssessments: List[RelativeMoveAssessment]
  ): List[ClaimSeed] =
    rankWithLifecycle(graph, relativeAssessments).claims

  def rankWithLifecycle(
      graph: ClaimCandidateGraph,
      relativeAssessments: List[RelativeMoveAssessment]
  ): ClaimArbitrationResult =
    val playedMoves = relativeAssessments.map(assessment => normalizeMove(assessment.played.moveUci)).toSet
    val dedupe = ClaimDeduplicator.deduplicateWithDiagnostics(graph.certified, graph.evidenceGraph)
    val certifiedDecisions = dedupe.winners
    val claims = certifiedDecisions.map(_.claim)
    val rankedClaims = certifiedDecisions
      .map { decision =>
        val salience = claimSalience(decision.claim, graph.evidenceGraph)
        val exposureTier = PlayerFacingClaimPolicy.tier(decision.claim, graph.evidenceGraph, playedMoves)
        (
          decision,
          salience.copy(interactions = claimInteractions(decision.claim, claims, graph.evidenceGraph, playedMoves)),
          exposureTier
        )
      }
      .sortBy { case (decision, salience, exposureTier) =>
        (
          -PlayerFacingClaimPolicy.rankPriority(exposureTier),
          -JudgmentSubjectBinding.bindingScore(
            JudgmentSubjectBinding.claimBinding(decision.claim, graph.evidenceGraph, playedMoves)
          ),
          -priority(decision, salience, graph.evidenceGraph)
        )
      }
      .map { case (decision, salience, _) =>
        decision.claim.copy(
          supportStatus = Some(supportCheck(decision)),
          salience = Some(salience)
        )
      }
    ClaimArbitrationResult(
      claims = rankedClaims,
      lifecycle =
        claimLifecycleDiagnostics(
          graph = graph,
          playedMoves = playedMoves,
          dedupeDroppedByClaimId = dedupe.droppedByClaimId,
          sourceCandidateIdsByWinnerId = dedupe.sourceCandidateIdsByWinnerId,
          finalClaims = rankedClaims
        )
    )

  private def claimLifecycleDiagnostics(
      graph: ClaimCandidateGraph,
      playedMoves: Set[String],
      dedupeDroppedByClaimId: Map[String, String],
      sourceCandidateIdsByWinnerId: Map[String, List[String]],
      finalClaims: List[ClaimSeed]
  ): List[ClaimLifecycleDiagnostic] =
    val finalIds = finalClaims.map(_.id).toSet
    val finalById = finalClaims.map(claim => claim.id -> claim).toMap
    val finalRanks = finalClaims.zipWithIndex.map { case (claim, index) => claim.id -> (index + 1) }.toMap
    graph.decisions.map { decision =>
      val claim = decision.claim
      val finalIncluded = finalIds.contains(claim.id)
      val dedupeWinnerId = dedupeDroppedByClaimId.get(claim.id)
      val finalClaimId = Option.when(finalIncluded)(claim.id).orElse(dedupeWinnerId.filter(finalIds.contains))
      val finalClaim = finalClaimId.flatMap(finalById.get)
      val lifecycleRelativeCauses = claimLifecycleRelativeCauses(claim, graph.evidenceGraph)
      val arbitrationSuppressed =
        decision.status == ClaimTruthStatus.Certified && dedupeWinnerId.isEmpty && !finalIncluded
      ClaimLifecycleDiagnostic(
        candidateId = claim.id,
        claimId = claim.id,
        finalClaimId = finalClaimId,
        sourceCandidateIds =
          sourceCandidateIdsByWinnerId.get(claim.id).orElse(finalClaimId.flatMap(sourceCandidateIdsByWinnerId.get)).getOrElse(List(claim.id)),
        family = claim.family,
        subject = claim.subject,
        subjectBinding = JudgmentSubjectBinding.claimBinding(claim, graph.evidenceGraph, playedMoves),
        primaryLine = claim.primaryLine,
        subjectMove = claim.subjectMove,
        ideaIds = claim.ideaRefs.map(_.id).distinct.sorted,
        finalIdeaIds = finalClaim.map(_.ideaRefs.map(_.id).distinct.sorted).getOrElse(Nil),
        evidenceIds = claim.evidence.map(_.id).distinct.sorted,
        finalEvidenceIds = finalClaim.map(_.evidence.map(_.id).distinct.sorted).getOrElse(Nil),
        relativeCauses = lifecycleRelativeCauses,
        strategicAxisLineage = ClaimStrategicAxisLineage.fromClaim(claim, graph.evidenceGraph),
        truthStatus = Some(claimLifecycleTruthStatus(decision.status)),
        presentLayers = decision.presentLayers,
        missingLayerGroups = decision.missingLayerGroups,
        missingEvidenceIds = decision.missingEvidence.map(_.id).distinct.sorted,
        stages = claimLifecycleStages(decision.status, dedupeWinnerId.nonEmpty, arbitrationSuppressed, finalIncluded),
        dedupeWinnerId = dedupeWinnerId,
        arbitrationRank = finalRanks.get(claim.id),
        finalPacketIncluded = finalIncluded
      )
    }

  private def claimLifecycleTruthStatus(status: ClaimTruthStatus): ClaimLifecycleTruthStatus =
    status match
      case ClaimTruthStatus.Certified => ClaimLifecycleTruthStatus.Certified
      case ClaimTruthStatus.Deferred  => ClaimLifecycleTruthStatus.Deferred
      case ClaimTruthStatus.Rejected  => ClaimLifecycleTruthStatus.Rejected

  private def claimLifecycleStages(
      status: ClaimTruthStatus,
      dedupeDropped: Boolean,
      arbitrationSuppressed: Boolean,
      finalIncluded: Boolean
  ): List[ClaimLifecycleStage] =
    List(
      Some(ClaimLifecycleStage.CandidateCreated),
      Some(
        status match
          case ClaimTruthStatus.Certified => ClaimLifecycleStage.TruthCertified
          case ClaimTruthStatus.Deferred  => ClaimLifecycleStage.TruthDeferred
          case ClaimTruthStatus.Rejected  => ClaimLifecycleStage.TruthRejected
      ),
      Option.when(dedupeDropped)(ClaimLifecycleStage.DedupeDropped),
      Option.when(arbitrationSuppressed)(ClaimLifecycleStage.ArbitrationSuppressed),
      Option.when(finalIncluded)(ClaimLifecycleStage.FinalPacketIncluded)
    ).flatten

  private def claimLifecycleRelativeCauses(
      claim: ClaimSeed,
      graph: TypedEvidenceGraph
  ): List[ClaimLifecycleRelativeCause] =
    claimRecords(claim, graph).flatMap {
      case EvidenceRecord(ref, RelativeCauseFactEvidence(cause), _) =>
        List(claimLifecycleRelativeCause(ref.id, cause, graph))
      case EvidenceRecord(ref, MoveVerdictCertificationEvidence(certification), _) =>
        certification.causes.zipWithIndex.map { case (cause, index) =>
          claimLifecycleRelativeCause(s"${ref.id}:cause:$index:${cause.kind}", cause, graph)
        }
      case _ =>
        Nil
    }.distinctBy(_.id)

  private def claimLifecycleRelativeCause(
      id: String,
      cause: RelativeCauseFact,
      graph: TypedEvidenceGraph
  ): ClaimLifecycleRelativeCause =
    val strategicProof = cause.strategicProofIdentity
    ClaimLifecycleRelativeCause(
      id = id,
      kind = cause.kind,
      role = cause.role,
      comparisonKind = cause.comparisonKind,
      sourceSide = cause.sourceSide,
      importance = cause.importance,
      referenceLine = cause.referenceLine,
      candidateLine = cause.candidateLine,
      eventLine = cause.eventLine,
      proofDirectSourceIds = cause.proof.toList.flatMap(_.directProof.sourceRefs.map(_.id)).distinct.sorted,
      proofContrastSourceIds = cause.proof.toList.flatMap(_.contrastProof.sourceRefs.map(_.id)).distinct.sorted,
      proofContextSupportSourceIds = cause.proof.toList.flatMap(_.contextSupport.sourceRefs.map(_.id)).distinct.sorted,
      proofStrategicAxisKeys = strategicProof.axisKeys,
      proofStrategicMechanismKinds = strategicProof.mechanismKinds,
      proofStrategicMechanismSourceIds = strategicProof.mechanismSourceIds,
      proofStrategicMechanismSignalSourceIds = strategicProof.signalSourceIds,
      supportEvidenceSourceIds = cause.supportEvidence.map(_.id).distinct.sorted,
      proofDirectKinds = cause.proof.toList.flatMap(_.directProof.kindLabels).distinct.sorted,
      proofContrastKinds = cause.proof.toList.flatMap(_.contrastProof.kindLabels).distinct.sorted,
      proofContextSupportKinds = cause.proof.toList.flatMap(_.contextSupport.kindLabels).distinct.sorted,
      attributionKind = cause.attribution.kind,
      attributionOwnedEvidenceIds = cause.attribution.ownedEvidence.map(_.id).distinct.sorted,
      attributionContrastEvidenceIds = cause.attribution.contrastEvidence.map(_.id).distinct.sorted,
      attributionContextEvidenceIds = cause.attribution.contextEvidence.map(_.id).distinct.sorted,
      attributionRootMoveMatched = cause.attribution.rootMoveMatched,
      attributionDirectProofEligible = cause.attribution.directProofEligible,
      attributionReason = cause.attribution.reason,
      objectBindingSignatures =
        EvidenceObjectBinding.objectSignatures(EvidenceObjectBinding.fromRelativeCause(cause, graph))
    )

  private def priority(
      decision: ClaimTruthDecision,
      salience: ClaimSalience,
      graph: TypedEvidenceGraph
  ): Int =
    val claim = decision.claim
    val verdict =
      if claim.family == ClaimFamily.Evaluation then
        claim.engineComparison.map(_.verdict)
      else claimRelativeVerdicts(claim, graph).headOption
    val hasRelativeProof = claimRelativeComparisons(claim, graph).nonEmpty || claimRelativeVerdicts(claim, graph).nonEmpty
    salience.score * 1000 +
      verdictFit(claim, verdict, hasRelativeProof) * 200 +
      familyBaseline(claim.family) * 50 +
      confidencePriority(claim.confidence) * 40 +
      decision.presentLayers.size * 10 +
      claimEvidencePriority(claim, graph)

  private def claimEvidencePriority(claim: ClaimSeed, graph: TypedEvidenceGraph): Int =
    val weightedEvidence =
      salienceRecordsForClaim(claim, claimRecords(claim, graph)).map(_.ref.id).distinct.size
    if claim.evidence.nonEmpty then weightedEvidence.max(1) else 0

  private def supportCheck(decision: ClaimTruthDecision): ClaimSupportCheck =
    ClaimSupportCheck(
      status =
        decision.status match
          case ClaimTruthStatus.Certified => ClaimSupportStatus.Certified
          case ClaimTruthStatus.Deferred  => ClaimSupportStatus.Deferred
          case ClaimTruthStatus.Rejected  => ClaimSupportStatus.Deferred,
      presentLayers = decision.presentLayers,
      missingLayerGroups = decision.missingLayerGroups,
      missingEvidence = decision.missingEvidence
    )

  private def claimSalience(claim: ClaimSeed, graph: TypedEvidenceGraph): ClaimSalience =
    val records = claim.evidence.flatMap(ref => graph.byId.get(ref.id))
    val scoringRecords = salienceRecordsForClaim(claim, records)
    val evidenceScore =
      scoringRecords
        .groupBy(_.ref.layer)
        .values
        .map(recordsForLayer => recordsForLayer.map(recordSalience).maxOption.getOrElse(0))
        .sum
    val topLevelComparisonSalience =
      if claim.family == ClaimFamily.Evaluation then engineComparisonSalience(claim.engineComparison) else 0
    val drivers =
      (scoringRecords.flatMap(recordDrivers) ++
        Option.when(topLevelComparisonSalience > 0)(ClaimSalienceDriver.EngineSwing) ++
        Option.when(claim.family == ClaimFamily.Evaluation)(claim.engineComparison).flatten.flatMap(_.candidateSet).toList.flatMap(cs =>
          Option.when(cs.onlyMove || cs.candidateCount <= 2)(ClaimSalienceDriver.CandidateConstraint)
        )).distinct
    ClaimSalience(
      score = (evidenceScore + topLevelComparisonSalience).min(30),
      drivers = drivers
    )

  private def claimInteractions(
      claim: ClaimSeed,
      claims: List[ClaimSeed],
      graph: TypedEvidenceGraph,
      playedMoves: Set[String]
  ): List[ClaimInteraction] =
    def relatedFor(accept: ClaimSeed => Boolean): List[(ClaimSeed, List[EvidenceRef], List[ClaimInteractionBasis])] =
      claims
        .filterNot(_.id == claim.id)
        .filter(accept)
        .flatMap { other =>
          val evidence = interactionEvidence(claim, other, graph)
          val basis = interactionBasis(claim, other, graph, evidence)
          Option.when(evidence.nonEmpty && basis.nonEmpty)(other, evidence, basis)
        }
    val causes = relativeCauses(claim, graph)
    val primaryInteractionCauses = causes.filter(primaryInteractionCause)
    val badVerdict = claimHasBadRelativeOutcome(claim, graph)
    val tacticalLongTermInteraction = tacticalLongTermInteractionFor(claim, graph, playedMoves)
    List.concat(
      tacticalLongTermInteraction
        .filter { case (kind, _) =>
          kind == ClaimInteractionKind.TacticalConstrainsLongTerm ||
            kind == ClaimInteractionKind.TacticalRefutesStrategicPlan
        }
        .map { case (kind, strength) =>
          relatedFor(_.family.isLongTerm).map { case (other, evidence, basis) =>
            ClaimInteraction(
              kind = kind,
              relatedClaimId = other.id,
              strength = strength,
              interactionEvidence = evidence,
              basis = basis
            )
          }
        }
        .getOrElse(Nil),
      tacticalLongTermInteraction
        .filter(_._1 == ClaimInteractionKind.TacticalSupportsStrategicPlan)
        .map { case (_, strength) =>
          relatedFor(_.family.isLongTerm).map { case (other, evidence, basis) =>
            ClaimInteraction(
              kind = ClaimInteractionKind.TacticalSupportsStrategicPlan,
              relatedClaimId = other.id,
              strength = strength,
              interactionEvidence = evidence,
              basis = basis
            )
          }
        }
        .getOrElse(Nil),
      Option
        .when(claim.family.isLongTerm)(
          claims.filterNot(_.id == claim.id).filter(_.family == ClaimFamily.Tactical).flatMap { other =>
            tacticalLongTermInteractionFor(other, graph, playedMoves).flatMap {
              case (kind, strength)
                  if kind == ClaimInteractionKind.TacticalConstrainsLongTerm ||
                    kind == ClaimInteractionKind.TacticalRefutesStrategicPlan =>
                val evidence = interactionEvidence(claim, other, graph)
                val basis = interactionBasis(claim, other, graph, evidence)
                Some(
                  ClaimInteraction(
                    kind = ClaimInteractionKind.LongTermConstrainedByTactic,
                    relatedClaimId = other.id,
                    strength = strength,
                    interactionEvidence = evidence,
                    basis = basis
                  )
                ).filter(interaction => interaction.interactionEvidence.nonEmpty && interaction.basis.nonEmpty)
              case (ClaimInteractionKind.TacticalSupportsStrategicPlan, strength) =>
                val supportEvidence = interactionEvidence(claim, other, graph)
                val supportBasis = interactionBasis(claim, other, graph, supportEvidence)
                Some(
                  ClaimInteraction(
                    kind = ClaimInteractionKind.TacticalSupportsStrategicPlan,
                    relatedClaimId = other.id,
                    strength = strength,
                    interactionEvidence = supportEvidence,
                    basis = supportBasis
                  )
                ).filter(interaction => interaction.interactionEvidence.nonEmpty && interaction.basis.nonEmpty)
              case _ =>
                None
            }
          }
        )
        .getOrElse(Nil),
      Option
        .when(claim.family == ClaimFamily.Defensive && primaryInteractionCauses.exists(cause => defensiveNecessityCause(cause.kind)))(
          relatedFor(_.family.isLongTerm).map { case (other, evidence, basis) =>
            ClaimInteraction(
              kind = ClaimInteractionKind.DefensiveNecessityOverridesPlan,
              relatedClaimId = other.id,
              strength = 6,
              interactionEvidence = evidence,
              basis = basis
            )
          }
        )
        .getOrElse(Nil),
      Option
        .when(
          claim.family == ClaimFamily.Conversion &&
            primaryInteractionCauses.exists(conversionInteractionCause)
        )(
          relatedFor(other =>
            other.family.isLongTerm || other.family == ClaimFamily.Evaluation
          ).map { case (other, evidence, basis) =>
            ClaimInteraction(
              kind = ClaimInteractionKind.ConversionSecuresAdvantage,
              relatedClaimId = other.id,
              strength = 4,
              interactionEvidence = evidence,
              basis = basis
            )
          }
        )
        .getOrElse(Nil),
      Option
        .when(
          claim.family == ClaimFamily.Material &&
            primaryInteractionCauses.exists(_.kind == RelativeCauseKind.SacrificeCompensation)
        )(
          relatedFor(_.family.isLongTerm).map { case (other, evidence, basis) =>
            ClaimInteraction(
              kind = ClaimInteractionKind.StrategicCompensationSupportsSacrifice,
              relatedClaimId = other.id,
              strength = 4,
              interactionEvidence = evidence,
              basis = basis
            )
          }
        )
        .getOrElse(Nil),
      Option
        .when(badVerdict && claim.family != ClaimFamily.Evaluation)(
          relatedFor(_.family == ClaimFamily.Evaluation).map { case (other, evidence, basis) =>
            ClaimInteraction(
              kind = ClaimInteractionKind.BadVerdictPreservesLocalIdea,
              relatedClaimId = other.id,
              strength = 5,
              interactionEvidence = evidence,
              basis = basis
            )
          }
        )
        .getOrElse(Nil)
    )
      .filter(_.interactionEvidence.nonEmpty)
      .distinctBy(interaction => (interaction.kind, interaction.relatedClaimId, interaction.basis.map(ClaimInteractionBasis.stableKey).sorted))

  private def tacticalLongTermInteractionFor(
      claim: ClaimSeed,
      graph: TypedEvidenceGraph,
      playedMoves: Set[String]
  ): Option[(ClaimInteractionKind, Int)] =
    Option.when(claim.family == ClaimFamily.Tactical) {
      val primaryPlayedCause = strongTacticalLongTermBinding(claim, graph, playedMoves)
      val constrains = tacticalConstrainsLongTerm(claim, graph)
      val supports = tacticalSupportsLongTerm(claim, graph)
      if primaryPlayedCause && constrains then
        Some((tacticalLongTermConstraintKind(claim, graph), tacticalConstraintStrength(claim, graph)))
      else if primaryPlayedCause && supports then
        Some((ClaimInteractionKind.TacticalSupportsStrategicPlan, 3))
      else None
    }.flatten

  private def strongTacticalLongTermBinding(
      claim: ClaimSeed,
      graph: TypedEvidenceGraph,
      playedMoves: Set[String]
  ): Boolean =
    PlayerFacingClaimPolicy.tier(claim, graph, playedMoves) == PlayerFacingClaimTier.Primary &&
      claimRecords(claim, graph).exists(record =>
        JudgmentSubjectBinding.recordBinding(record, playedMoves) == SubjectBindingClass.PrimaryPlayedCause
      )

  private def tacticalLongTermConstraintKind(claim: ClaimSeed, graph: TypedEvidenceGraph): ClaimInteractionKind =
    if relativeCauses(claim, graph).exists(cause =>
        primaryInteractionCause(cause) &&
          RelativeCauseClaimDepth.hasTacticalDepth(cause) &&
          (
            cause.kind == RelativeCauseKind.TacticalRefutationOfPlayed ||
              cause.kind == RelativeCauseKind.MissedTacticalResource ||
              cause.kind == RelativeCauseKind.CandidateTacticalLiability
          )
      )
    then ClaimInteractionKind.TacticalRefutesStrategicPlan
    else ClaimInteractionKind.TacticalConstrainsLongTerm

  private def tacticalConstrainsLongTerm(claim: ClaimSeed, graph: TypedEvidenceGraph): Boolean =
    val drivers = claimDrivers(claim, graph)
    val causes = relativeCauses(claim, graph)
    val hasMaterialTactic = causes.exists(RelativeCauseClaimDepth.hasMaterialSwingTacticalDepth)
    val hasTacticalDriver = drivers.contains(ClaimSalienceDriver.TacticalRelation) || hasMaterialTactic
    val hasForcingOrEngine =
      causes.exists(RelativeCauseClaimDepth.hasTacticalDepth) ||
      drivers.contains(ClaimSalienceDriver.ForcingLine) ||
      claimRelativeComparisons(claim, graph).exists(comparisonProvesTacticalConstraint)
    claim.family == ClaimFamily.Tactical && hasTacticalDriver && hasForcingOrEngine

  private def tacticalSupportsLongTerm(claim: ClaimSeed, graph: TypedEvidenceGraph): Boolean =
    val drivers = claimDrivers(claim, graph)
    claim.family == ClaimFamily.Tactical &&
      drivers.contains(ClaimSalienceDriver.TacticalRelation) &&
      !tacticalConstrainsLongTerm(claim, graph) &&
      claimRelativeComparisons(claim, graph).exists(_.winPercentLossForMover < JudgmentThresholds.INACCURACY_WP)

  private def tacticalConstraintStrength(claim: ClaimSeed, graph: TypedEvidenceGraph): Int =
    val drivers = claimDrivers(claim, graph)
    val engineStrength =
      claimRelativeComparisons(claim, graph).map { comparison =>
        if comparison.winPercentLossForMover >= JudgmentThresholds.BLUNDER_WP then 4
        else if comparison.winPercentLossForMover >= JudgmentThresholds.INACCURACY_WP then 3
        else if comparison.candidateWinPercentDeltaForMover >= JudgmentThresholds.PLAYABLE_LOSS_WP then 2
        else 0
      }.maxOption.getOrElse(0)
    List(
      Option.when(drivers.contains(ClaimSalienceDriver.TacticalRelation))(2),
      Option.when(drivers.contains(ClaimSalienceDriver.ForcingLine))(3),
      Option.when(drivers.contains(ClaimSalienceDriver.CandidateConstraint))(1),
      Option.when(drivers.contains(ClaimSalienceDriver.EngineSwing))(1),
      Option.when(engineStrength > 0)(engineStrength)
    ).flatten.sum.max(1).min(10)

  private def claimDrivers(claim: ClaimSeed, graph: TypedEvidenceGraph): List[ClaimSalienceDriver] =
    val records = salienceRecordsForClaim(claim, claimRecords(claim, graph))
    (records.flatMap(recordDrivers) ++
      Option.when(claim.family == ClaimFamily.Evaluation && claim.engineComparison.nonEmpty)(ClaimSalienceDriver.EngineSwing) ++
      Option.when(claim.family == ClaimFamily.Evaluation)(claim.engineComparison).flatten.flatMap(_.candidateSet).toList.flatMap(cs =>
        Option.when(cs.onlyMove || cs.candidateCount <= 2)(ClaimSalienceDriver.CandidateConstraint)
      )).distinct

  private def salienceRecordsForClaim(claim: ClaimSeed, records: List[EvidenceRecord]): List[EvidenceRecord] =
    claim.family match
      case ClaimFamily.Tactical =>
        records.filter(tacticalSalienceRecord)
      case family if family.isLongTerm =>
        records.filter(longTermSalienceRecord)
      case _ =>
        records

  private def longTermSalienceRecord(record: EvidenceRecord): Boolean =
    !ClaimSupportCluster.longTermSupportExcludedLayer(record.ref.layer) &&
      (record.ref.layer match
        case layer if StrategicMechanismEvidence.rawStrategicSourceLayer(layer) =>
          false
        case _ =>
          true
      )

  private def tacticalSalienceRecord(record: EvidenceRecord): Boolean =
    record.payload match
      case _: TacticalMechanismEvidence | _: RelationFactEvidence | _: MoveMotifEvidence | _: LineFactEvidence |
          EvalFactEvidence(_, _, _, _) | _: ThreatEpisodeEvidence | _: ThreatPressureEvidence |
          MoveTransitionEvidence(_, _, _) | RelativeAssessmentEvidence(_) |
          CounterfactualFactEvidence(_, _, _) | CandidateComparisonEvidence(_) =>
        true
      case RelativeCauseFactEvidence(cause) =>
        RelativeCauseClaimDepth.hasTacticalDepth(cause)
      case MoveVerdictCertificationEvidence(certification) =>
        certification.causes.exists(RelativeCauseClaimDepth.hasTacticalDepth)
      case _ =>
        false

  private def comparisonProvesTacticalConstraint(comparison: EvalComparison): Boolean =
    comparison.winPercentLossForMover >= JudgmentThresholds.INACCURACY_WP ||
      comparison.candidateWinPercentDeltaForMover >= JudgmentThresholds.PLAYABLE_LOSS_WP ||
      comparison.candidateSet.exists(_.onlyMove)

  private def normalizeMove(raw: String): String =
    Option(raw).getOrElse("").trim.toLowerCase

  private def exactSharedEvidence(left: ClaimSeed, right: ClaimSeed): List[EvidenceRef] =
    val leftIds = left.evidence.map(_.id).toSet
    right.evidence.filter(ref => leftIds.contains(ref.id)).distinctBy(_.id)

  private def interactionEvidence(
      left: ClaimSeed,
      right: ClaimSeed,
      graph: TypedEvidenceGraph
  ): List[EvidenceRef] =
    if eventLongTermPair(left, right) then
      eventLongTermInteractionEvidence(left, right, graph)
    else eventEventInteractionEvidence(left, right, graph)

  private def interactionBasis(
      left: ClaimSeed,
      right: ClaimSeed,
      graph: TypedEvidenceGraph,
      evidence: List[EvidenceRef]
  ): List[ClaimInteractionBasis] =
    val evidenceIds = evidence.map(_.id).toSet
    val eventLongTerm = eventLongTermPair(left, right)
    val leftCauses = relativeCauses(left, graph)
    val rightCauses = relativeCauses(right, graph)
    val sharedSignatures =
      if eventLongTerm then Set.empty
      else leftCauses.map(relativeCauseSignature).toSet.intersect(rightCauses.map(relativeCauseSignature).toSet)
    val causes =
      if eventLongTerm then
        val eventClaim = if left.family.isEvent then left else right
        relativeCauses(eventClaim, graph)
      else
        leftCauses ++ rightCauses
    causes
      .filter(cause =>
        sharedSignatures.contains(relativeCauseSignature(cause)) ||
          interactionCauseBacksEvidence(cause, evidenceIds)
      )
      .map(interactionBasisFromCause)
      .distinctBy(basis =>
        (
          basis.causeKind,
          basis.comparisonKind,
          basis.causeRole,
          basis.causeSourceSide,
          basis.causeImportance,
          basis.attributionKind,
          basis.attributionRootMoveMatched,
          basis.attributionDirectProofEligible,
          basis.referenceLine,
          basis.candidateLine,
          basis.eventLine,
          basis.proofDirectSourceIds,
          basis.proofContrastSourceIds,
          basis.proofContextSupportSourceIds,
          basis.proofStrategicAxisKeys,
          basis.proofStrategicMechanismKinds,
          basis.proofStrategicMechanismSourceIds,
          basis.proofStrategicMechanismSignalSourceIds,
          basis.supportEvidenceSourceIds
        )
      )

  private def interactionCauseBacksEvidence(
      cause: RelativeCauseFact,
      evidenceIds: Set[String]
  ): Boolean =
    val directIds = cause.proof.toList.flatMap(_.directProof.sourceRefs.map(_.id)).toSet
    val contrastIds = cause.proof.toList.flatMap(_.contrastProof.sourceRefs.map(_.id)).toSet
    val contextIds = cause.proof.toList.flatMap(_.contextSupport.sourceRefs.map(_.id)).toSet
    val supportIds = cause.supportEvidence.map(_.id).toSet
    val proofIds = directIds ++ contrastIds ++ contextIds
    proofIds.intersect(evidenceIds).nonEmpty || supportIds.intersect(evidenceIds).nonEmpty

  private def interactionBasisFromCause(cause: RelativeCauseFact): ClaimInteractionBasis =
    val strategicProof = cause.strategicProofIdentity
    ClaimInteractionBasis(
      causeKind = cause.kind,
      comparisonKind = cause.comparisonKind,
      causeRole = cause.role,
      causeSourceSide = cause.sourceSide,
      causeImportance = cause.importance,
      attributionKind = cause.attribution.kind,
      attributionRootMoveMatched = cause.attribution.rootMoveMatched,
      attributionDirectProofEligible = cause.attribution.directProofEligible,
      referenceLine = cause.referenceLine,
      candidateLine = cause.candidateLine,
      eventLine = cause.eventLine,
      proofDirectSourceIds = cause.proof.toList.flatMap(_.directProof.sourceRefs.map(_.id)).distinct.sorted,
      proofContrastSourceIds = cause.proof.toList.flatMap(_.contrastProof.sourceRefs.map(_.id)).distinct.sorted,
      proofContextSupportSourceIds = cause.proof.toList.flatMap(_.contextSupport.sourceRefs.map(_.id)).distinct.sorted,
      proofStrategicAxisKeys = strategicProof.axisKeys,
      proofStrategicMechanismKinds = strategicProof.mechanismKinds,
      proofStrategicMechanismSourceIds = strategicProof.mechanismSourceIds,
      proofStrategicMechanismSignalSourceIds = strategicProof.signalSourceIds,
      supportEvidenceSourceIds = cause.supportEvidence.map(_.id).distinct.sorted
    )

  private def eventLongTermPair(left: ClaimSeed, right: ClaimSeed): Boolean =
    (left.family.isEvent && right.family.isLongTerm) ||
      (right.family.isEvent && left.family.isLongTerm)

  private def eventLongTermInteractionEvidence(
      left: ClaimSeed,
      right: ClaimSeed,
      graph: TypedEvidenceGraph
  ): List[EvidenceRef] =
    val (eventClaim, longTermClaim) =
      if left.family.isEvent then (left, right) else (right, left)
    val longTermIds = longTermClaim.evidence.map(_.id).toSet
    val proofOverlap =
      eventDepthProofRefs(eventClaim, graph)
        .filter(ref => longTermIds.contains(ref.id))
        .distinctBy(_.id)
    proofOverlap

  private def eventEventInteractionEvidence(
      left: ClaimSeed,
      right: ClaimSeed,
      graph: TypedEvidenceGraph
  ): List[EvidenceRef] =
    val shared = exactSharedEvidence(left, right)
    if shared.nonEmpty then shared
    else if relativeCauseSignatureOverlap(left, right, graph) then
      comparisonEvidence(left, graph) ++ comparisonEvidence(right, graph)
    else
      Nil

  private def eventDepthProofRefs(claim: ClaimSeed, graph: TypedEvidenceGraph): List[EvidenceRef] =
    relativeCauses(claim, graph)
      .flatMap(_.proof.toList)
      .flatMap(proof => proof.directProof.sourceRefs ++ proof.contrastProof.sourceRefs)
      .distinctBy(_.id)

  private def relativeCauseSignatureOverlap(
      left: ClaimSeed,
      right: ClaimSeed,
      graph: TypedEvidenceGraph
  ): Boolean =
    val leftSignatures = relativeCauseSignatures(left, graph)
    leftSignatures.nonEmpty && relativeCauseSignatures(right, graph).exists(leftSignatures.contains)

  private type RelativeCauseInteractionSignature =
    (
        RelativeCauseKind,
        CandidateComparisonKind,
        RelativeCauseRole,
        RelativeCauseSourceSide,
        RelativeCauseImportance,
        CauseAttributionKind,
        Boolean,
        Boolean,
        LineNodeRef,
        LineNodeRef,
        LineNodeRef,
        List[String],
        List[String],
        List[String],
        List[String],
        List[StrategicMechanismKind],
        List[String],
        List[String],
        List[String]
    )

  private def relativeCauseSignatures(claim: ClaimSeed, graph: TypedEvidenceGraph): Set[RelativeCauseInteractionSignature] =
    relativeCauses(claim, graph)
      .map(relativeCauseSignature)
      .toSet

  private def relativeCauseSignature(cause: RelativeCauseFact): RelativeCauseInteractionSignature =
    val strategicProof = cause.strategicProofIdentity
    (
      cause.kind,
      cause.comparisonKind,
      cause.role,
      cause.sourceSide,
      cause.importance,
      cause.attribution.kind,
      cause.attribution.rootMoveMatched,
      cause.attribution.directProofEligible,
      cause.referenceLine,
      cause.candidateLine,
      cause.eventLine,
      cause.proof.toList.flatMap(_.directProof.sourceRefs.map(_.id)).distinct.sorted,
      cause.proof.toList.flatMap(_.contrastProof.sourceRefs.map(_.id)).distinct.sorted,
      cause.proof.toList.flatMap(_.contextSupport.sourceRefs.map(_.id)).distinct.sorted,
      strategicProof.axisKeys,
      strategicProof.mechanismKinds,
      strategicProof.mechanismSourceIds,
      strategicProof.signalSourceIds,
      cause.supportEvidence.map(_.id).distinct.sorted
    )

  private def comparisonEvidence(claim: ClaimSeed, graph: TypedEvidenceGraph): List[EvidenceRef] =
    claim.evidence.filter(ref =>
      graph.byId.get(ref.id).exists(record =>
        record.payload match
          case RelativeCauseFactEvidence(_) | MoveVerdictCertificationEvidence(_) |
              RelativeAssessmentEvidence(_) | CandidateComparisonEvidence(_) |
              CounterfactualFactEvidence(_, _, _) =>
            true
          case _ =>
            false
      )
    ).distinctBy(_.id)

  private def claimRecords(claim: ClaimSeed, graph: TypedEvidenceGraph): List[EvidenceRecord] =
    claim.evidence.flatMap(ref => graph.byId.get(ref.id))

  private def claimRelativeComparisons(claim: ClaimSeed, graph: TypedEvidenceGraph): List[EvalComparison] =
    val recordComparisons =
      claimRecords(claim, graph).collect {
        case record @ EvidenceRecord(_, RelativeAssessmentEvidence(assessment), _) if comparisonRecordUsable(record) =>
          assessment.comparison
        case record @ EvidenceRecord(_, CounterfactualFactEvidence(_, _, comparison), _) if comparisonRecordUsable(record) =>
          comparison
        case record @ EvidenceRecord(_, CandidateComparisonEvidence(fact), _) if comparisonRecordUsable(record) =>
          fact.comparison
        case record @ EvidenceRecord(_, MoveVerdictCertificationEvidence(certification), _) if comparisonRecordUsable(record) =>
          certification.primaryComparison.comparison
      }
    val topLevelEvaluation =
      Option.when(claim.family == ClaimFamily.Evaluation)(claim.engineComparison).flatten.toList
    (recordComparisons ++ topLevelEvaluation).distinct

  private def comparisonRecordUsable(record: EvidenceRecord): Boolean =
    record.ref.confidence == EvidenceConfidence.EngineBacked

  private def claimRelativeVerdicts(claim: ClaimSeed, graph: TypedEvidenceGraph): List[MoveChoiceVerdict] =
    val recordVerdicts =
      claimRecords(claim, graph).flatMap {
        case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) =>
          List(cause.verdict)
        case EvidenceRecord(_, MoveVerdictCertificationEvidence(certification), _) =>
          certification.verdict :: certification.causes.map(_.verdict)
        case _ =>
          Nil
      }
    (recordVerdicts ++ claimRelativeComparisons(claim, graph).map(_.verdict)).distinct

  private def claimHasBadRelativeOutcome(claim: ClaimSeed, graph: TypedEvidenceGraph): Boolean =
    claimRelativeVerdicts(claim, graph).exists(badVerdict)

  private def badVerdict(verdict: MoveChoiceVerdict): Boolean =
    verdict == MoveChoiceVerdict.Inaccuracy ||
      verdict == MoveChoiceVerdict.Mistake ||
      verdict == MoveChoiceVerdict.Blunder

  private def relativeCauses(claim: ClaimSeed, graph: TypedEvidenceGraph): List[RelativeCauseFact] =
    claimRecords(claim, graph).flatMap {
      case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) =>
        List(cause)
      case EvidenceRecord(_, MoveVerdictCertificationEvidence(certification), _) =>
        certification.causes
      case _ =>
        Nil
    }.distinct

  private def primaryInteractionCause(cause: RelativeCauseFact): Boolean =
    cause.role == RelativeCauseRole.PrimaryPlayedCause &&
      cause.importance == RelativeCauseImportance.Primary &&
      badVerdict(cause.verdict)

  private def conversionInteractionCause(cause: RelativeCauseFact): Boolean =
    cause.kind == RelativeCauseKind.ConversionSecured &&
      RelativeCauseClaimDepth.hasConversionDepth(cause)

  private def defensiveNecessityCause(kind: RelativeCauseKind): Boolean =
    kind != RelativeCauseKind.DrawResource &&
      ClaimEventCluster.kindForCause(kind).contains(ClaimEventClusterKind.DefensiveEvent)

  private def recordSalience(record: EvidenceRecord): Int =
    record.payload match
      case _: RelationFactEvidence =>
        0
      case payload: TacticalMechanismEvidence =>
        6 + tacticalMechanismSalience(payload.kind) +
          Option.when(payload.hasLineProof)(2).getOrElse(0) +
          Option.when(payload.hasThreatProof)(1).getOrElse(0) +
          payload.signals.size.min(2)
      case _: ThreatEpisodeEvidence =>
        0
      case _: ThreatPressureEvidence =>
        0
      case PlanPressureEvidence(_, _) | PawnStructureFactEvidence(_, _, _) | StrategicFactEvidence(_, _, _, _) =>
        0
      case _: StrategicMechanismEvidence =>
        0
      case _: StrategicMechanismContrastEvidence =>
        0
      case RelativeAssessmentEvidence(assessment) =>
        engineComparisonSalience(Some(assessment.comparison))
      case CounterfactualFactEvidence(_, _, comparison) =>
        engineComparisonSalience(Some(comparison))
      case CandidateComparisonEvidence(fact) =>
        engineComparisonSalience(Some(fact.comparison)) + candidateComparisonKindSalience(fact.kind)
      case RelativeCauseFactEvidence(cause) =>
        relativeCauseSalience(cause)
      case MoveVerdictCertificationEvidence(certification) =>
        engineComparisonSalience(Some(certification.primaryComparison.comparison)) +
          certificationScoringCauses(certification).map(relativeCauseSalience).maxOption.getOrElse(0)
      case _: StructuralDeltaEvidence =>
        0
      case _: PlanCausalEventEvidence =>
        0
      case OpeningContextEvidence(_, _, _, _) =>
        0
      case FeatureAnchorEvidence(_) | ApplicabilityAssessmentEvidence(_) =>
        0
      case SinglePositionEvidence(_) =>
        1
      case _: BoardFactEvidence =>
        0
      case _: LineFactEvidence =>
        0
      case EvalFactEvidence(_, _, _, _) =>
        0
      case _: MoveMotifEvidence =>
        0
      case MoveTransitionEvidence(_, _, _) | ChessIdeaEvidence(_) | ClaimEvidence(_) =>
        1
      case PlanTransitionEvidence(_) =>
        0

  private def recordDrivers(record: EvidenceRecord): List[ClaimSalienceDriver] =
    record.payload match
      case _: RelationFactEvidence =>
        Nil
      case payload: TacticalMechanismEvidence if payload.defensive =>
        Option.when(payload.canAnchorDefensiveIdea)(ClaimSalienceDriver.DefensiveUrgency).toList
      case payload: TacticalMechanismEvidence =>
        ClaimSalienceDriver.TacticalRelation ::
          Option
            .when(payload.hasEngineOrForcingProof || payload.kind == TacticalMechanismKind.KingForcing)(ClaimSalienceDriver.ForcingLine)
            .toList
      case payload: ThreatEpisodeEvidence =>
        Option.when(!payload.insufficientData && payload.defenseRequired)(ClaimSalienceDriver.DefensiveUrgency).toList
      case _: ThreatPressureEvidence =>
        Nil
      case PlanPressureEvidence(_, _) | PawnStructureFactEvidence(_, _, _) | StrategicFactEvidence(_, _, _, _) =>
        Nil
      case _: StrategicMechanismEvidence =>
        Nil
      case _: StrategicMechanismContrastEvidence =>
        Nil
      case _: StructuralDeltaEvidence | FeatureAnchorEvidence(_) | ApplicabilityAssessmentEvidence(_) =>
        Nil
      case RelativeAssessmentEvidence(_) | CounterfactualFactEvidence(_, _, _) =>
        List(ClaimSalienceDriver.EngineSwing)
      case CandidateComparisonEvidence(fact) =>
        ClaimSalienceDriver.EngineSwing ::
          Option.when(fact.kind == CandidateComparisonKind.BestVsSecond)(ClaimSalienceDriver.CandidateConstraint).toList
      case RelativeCauseFactEvidence(cause) =>
        relativeCauseDrivers(cause)
      case MoveVerdictCertificationEvidence(certification) =>
        (ClaimSalienceDriver.EngineSwing :: certificationScoringCauses(certification).flatMap(relativeCauseDrivers)).distinct
      case payload: LineFactEvidence if payload.hasConcreteLineConsequence =>
        List(ClaimSalienceDriver.ForcingLine)
      case _ =>
        Nil

  private def tacticalMechanismSalience(kind: TacticalMechanismKind): Int =
    kind match
      case TacticalMechanismKind.KingForcing =>
        6
      case TacticalMechanismKind.MaterialGain | TacticalMechanismKind.RecaptureChoice | TacticalMechanismKind.Tempo |
          TacticalMechanismKind.RelationMechanism | TacticalMechanismKind.Refutation | TacticalMechanismKind.PawnPromotion =>
        4
      case TacticalMechanismKind.Conversion | TacticalMechanismKind.DrawResource | TacticalMechanismKind.DefensiveResource =>
        3

  private def engineComparisonSalience(comparison: Option[EvalComparison]): Int =
    comparison.map { cmp =>
      val verdictScore =
        cmp.verdict match
          case MoveChoiceVerdict.Blunder             => 10
          case MoveChoiceVerdict.Mistake             => 8
          case MoveChoiceVerdict.Inaccuracy          => 6
          case MoveChoiceVerdict.PlayableLoss        => 4
          case MoveChoiceVerdict.MatchesReference    => 3
          case MoveChoiceVerdict.ImprovesOnReference => 4
      verdictScore +
        math.round(cmp.winPercentLossForMover.min(20.0) / 4.0).toInt +
        cmp.candidateSet.map(cs => if cs.onlyMove then 3 else if cs.candidateCount <= 2 then 1 else 0).getOrElse(0)
    }.getOrElse(0)

  private def candidateComparisonKindSalience(kind: CandidateComparisonKind): Int =
    kind match
      case CandidateComparisonKind.PlayedVsBest          => 4
      case CandidateComparisonKind.BestVsSecond          => 3
      case CandidateComparisonKind.PlayedVsAlternative   => 2
      case CandidateComparisonKind.ReferenceVsAlternative => 1

  private def relativeCauseSalience(cause: RelativeCauseFact): Int =
    if cause.strategicCauseKind && !RelativeCauseClaimDepth.hasStrategicContrastDepth(cause) then 0
    else
      val causeScore =
        cause.kind match
          case RelativeCauseKind.TacticalRefutationOfPlayed | RelativeCauseKind.MissedTacticalResource |
              RelativeCauseKind.CandidateTacticalLiability |
              RelativeCauseKind.WrongRecapturer | RelativeCauseKind.RecaptureRecoveryWindow |
              RelativeCauseKind.TempoLoss | RelativeCauseKind.KingForcing =>
            if RelativeCauseClaimDepth.hasTacticalDepth(cause) then 8
            else if RelativeCauseClaimDepth.hasOwnedDepth(cause) then 4
            else 0
          case RelativeCauseKind.OnlyMoveNecessity | RelativeCauseKind.OnlyDefenseNecessity =>
            7
          case RelativeCauseKind.ConversionMiss | RelativeCauseKind.ConversionSecured =>
            if RelativeCauseClaimDepth.hasConversionDepth(cause) then 5 else 0
          case RelativeCauseKind.PlanContradiction =>
            if RelativeCauseClaimDepth.hasStrategicContrastDepth(cause) then 3 else 0
          case RelativeCauseKind.StrategicConcession |
              RelativeCauseKind.MissedStrategicImprovement | RelativeCauseKind.StructuralImprovement |
              RelativeCauseKind.TargetPressureGain | RelativeCauseKind.TargetPressureRelease |
              RelativeCauseKind.CenterControlGain |
              RelativeCauseKind.KingSafetyConcession | RelativeCauseKind.PawnWeaknessTarget |
              RelativeCauseKind.PawnBreakOpportunity | RelativeCauseKind.ActivityGain | RelativeCauseKind.ActivityLoss |
              RelativeCauseKind.OpponentRestriction =>
            if RelativeCauseClaimDepth.hasStrategicContrastDepth(cause) then 5 else 0
          case RelativeCauseKind.DefensiveResource | RelativeCauseKind.DrawResource |
              RelativeCauseKind.SacrificeCompensation =>
            4
          case RelativeCauseKind.PlanImprovement =>
            if RelativeCauseClaimDepth.hasStrategicContrastDepth(cause) then 4 else 0
          case RelativeCauseKind.MaterialSwing =>
            if RelativeCauseClaimDepth.hasMaterialDepth(cause) then 3 else 0
          case RelativeCauseKind.WrongMoveOrder =>
            if RelativeCauseClaimDepth.hasTacticalDepth(cause) then 6 else 0
      val roleScore =
        cause.importance match
          case RelativeCauseImportance.Primary    => 4
          case RelativeCauseImportance.Supporting => 2
          case RelativeCauseImportance.Context    => 0
      val sourceScore =
        cause.sourceSide match
          case RelativeCauseSourceSide.Candidate | RelativeCauseSourceSide.Reference => 2
          case RelativeCauseSourceSide.Mixed                                        => 1
          case RelativeCauseSourceSide.Shared                                       => 0
      val comparisonScore =
        candidateComparisonKindSalience(cause.comparisonKind)
      val proofScore =
        if RelativeCauseClaimDepth.hasOwnedDepth(cause) then
          cause.proof
            .map(proof =>
              List(
                Option.when(proof.hasRawDirectProof)(3),
                Option.when(proof.hasRawContrastProof)(2)
              ).flatten.sum
            )
            .getOrElse(0)
        else 0
      causeScore +
        roleScore +
        sourceScore +
        comparisonScore +
        proofScore +
        math.round(cause.winPercentLossForMover.min(20.0) / 5.0).toInt

  private def certificationScoringCauses(certification: MoveVerdictCertification): List[RelativeCauseFact] =
    certification.causes.filter(cause =>
      RelativeCauseClaimDepth.hasOwnedDepth(cause) &&
        (!cause.strategicCauseKind || RelativeCauseClaimDepth.hasStrategicContrastDepth(cause))
    )

  private def relativeCauseDrivers(cause: RelativeCauseFact): List[ClaimSalienceDriver] =
    if cause.strategicCauseKind && !RelativeCauseClaimDepth.hasStrategicContrastDepth(cause) then Nil
    else
      val kindDrivers =
        cause.kind match
          case RelativeCauseKind.MissedTacticalResource | RelativeCauseKind.TacticalRefutationOfPlayed |
              RelativeCauseKind.CandidateTacticalLiability |
              RelativeCauseKind.WrongRecapturer | RelativeCauseKind.RecaptureRecoveryWindow |
              RelativeCauseKind.WrongMoveOrder | RelativeCauseKind.TempoLoss | RelativeCauseKind.KingForcing =>
            if RelativeCauseClaimDepth.hasTacticalDepth(cause) then
              List(ClaimSalienceDriver.TacticalRelation, ClaimSalienceDriver.EngineSwing)
            else List(ClaimSalienceDriver.EngineSwing)
          case RelativeCauseKind.MaterialSwing =>
            List(ClaimSalienceDriver.StructuralChange, ClaimSalienceDriver.EngineSwing)
          case RelativeCauseKind.SacrificeCompensation =>
            List(ClaimSalienceDriver.EngineSwing)
          case RelativeCauseKind.OnlyMoveNecessity =>
            List(ClaimSalienceDriver.CandidateConstraint, ClaimSalienceDriver.EngineSwing)
          case RelativeCauseKind.OnlyDefenseNecessity | RelativeCauseKind.DefensiveResource | RelativeCauseKind.DrawResource =>
            List(ClaimSalienceDriver.DefensiveUrgency, ClaimSalienceDriver.EngineSwing)
          case RelativeCauseKind.ConversionMiss | RelativeCauseKind.ConversionSecured =>
            List(ClaimSalienceDriver.StructuralChange, ClaimSalienceDriver.EngineSwing)
          case RelativeCauseKind.StructuralImprovement =>
            List(ClaimSalienceDriver.StrategicFeature, ClaimSalienceDriver.StructuralChange, ClaimSalienceDriver.EngineSwing)
          case RelativeCauseKind.TargetPressureGain | RelativeCauseKind.CenterControlGain =>
            List(ClaimSalienceDriver.StrategicFeature, ClaimSalienceDriver.EngineSwing)
          case RelativeCauseKind.TargetPressureRelease | RelativeCauseKind.KingSafetyConcession |
              RelativeCauseKind.PawnWeaknessTarget |
              RelativeCauseKind.PawnBreakOpportunity | RelativeCauseKind.ActivityGain | RelativeCauseKind.ActivityLoss |
              RelativeCauseKind.OpponentRestriction =>
            List(ClaimSalienceDriver.StrategicFeature, ClaimSalienceDriver.StructuralChange, ClaimSalienceDriver.EngineSwing)
          case RelativeCauseKind.StrategicConcession |
              RelativeCauseKind.MissedStrategicImprovement | RelativeCauseKind.PlanImprovement |
              RelativeCauseKind.PlanContradiction =>
            List(ClaimSalienceDriver.StrategicFeature, ClaimSalienceDriver.EngineSwing)
      val contextDrivers =
        List(
          Option.when(cause.comparisonKind == CandidateComparisonKind.BestVsSecond)(ClaimSalienceDriver.CandidateConstraint),
          Option.when(cause.role == RelativeCauseRole.CandidateSetConstraint)(ClaimSalienceDriver.CandidateConstraint),
          Option.when(RelativeCauseClaimDepth.hasTacticalDepth(cause))(ClaimSalienceDriver.TacticalRelation),
          Option.when(
            RelativeCauseClaimDepth.hasOwnedDepth(cause) &&
              cause.proof.exists(RelativeCauseClaimDepth.hasLineDriver)
          )(ClaimSalienceDriver.ForcingLine),
          Option.when(
            RelativeCauseClaimDepth.hasOwnedDepth(cause) &&
              cause.proof.exists(RelativeCauseClaimDepth.hasTransitionDriver)
          )(ClaimSalienceDriver.StructuralChange)
        ).flatten
      (kindDrivers ++ contextDrivers).distinct

  private def verdictFit(
      claim: ClaimSeed,
      verdict: Option[MoveChoiceVerdict],
      hasRelativeProof: Boolean
  ): Int =
    verdict match
      case Some(MoveChoiceVerdict.Blunder | MoveChoiceVerdict.Mistake | MoveChoiceVerdict.Inaccuracy) =>
        claim.family match
          case ClaimFamily.Evaluation => 6
          case ClaimFamily.Tactical | ClaimFamily.Defensive => 5
          case ClaimFamily.Material => 4
          case ClaimFamily.PawnStructure | ClaimFamily.Strategic | ClaimFamily.Plan =>
            if hasRelativeProof then 4 else 2
          case ClaimFamily.Conversion | ClaimFamily.Opening => 2
      case _ =>
        claim.family match
          case ClaimFamily.Tactical      => 5
          case ClaimFamily.Defensive     => 5
          case ClaimFamily.Material      => 4
          case ClaimFamily.PawnStructure => 4
          case ClaimFamily.Strategic | ClaimFamily.Plan => 4
          case ClaimFamily.Conversion    => 3
          case ClaimFamily.Evaluation    => 2
          case ClaimFamily.Opening       => 2

  private def familyBaseline(family: ClaimFamily): Int =
    family match
      case ClaimFamily.Tactical | ClaimFamily.Defensive => 3
      case ClaimFamily.Material => 2
      case ClaimFamily.Strategic | ClaimFamily.PawnStructure | ClaimFamily.Plan => 2
      case ClaimFamily.Evaluation | ClaimFamily.Conversion | ClaimFamily.Opening => 1

  private def confidencePriority(confidence: EvidenceConfidence): Int =
    confidence match
      case EvidenceConfidence.LegalReplayVerified => 5
      case EvidenceConfidence.EngineBacked        => 4
      case EvidenceConfidence.BoardDerived        => 3
      case EvidenceConfidence.Mixed               => 2
      case EvidenceConfidence.Heuristic           => 1
