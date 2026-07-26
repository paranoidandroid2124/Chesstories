package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.model.evaluation.JudgmentThresholds
import lila.chessjudgment.analysis.policy.{ ClaimAdmissionDecision, ClaimTruthPolicy, ClaimAdmissionStatus }
import lila.chessjudgment.model.judgment.*

final case class ClaimCandidateGraph(
    decisions: List[ClaimAdmissionDecision],
    evidenceGraph: TypedEvidenceGraph
):
  def certified: List[ClaimAdmissionDecision] =
    decisions.filter(_.status == ClaimAdmissionStatus.Certified)

  def deferred: List[ClaimAdmissionDecision] =
    decisions.filter(_.status == ClaimAdmissionStatus.Deferred)

  def rejected: List[ClaimAdmissionDecision] =
    decisions.filter(_.status == ClaimAdmissionStatus.Rejected)

object ClaimCandidateGraphAssembler:

  def fromClaims(
      claims: List[JudgmentClaim],
      graph: TypedEvidenceGraph
  ): ClaimCandidateGraph =
    claims.foreach(claim => comparisonForClaim(claim, graph))
    ClaimCandidateGraph(claims.map(ClaimTruthPolicy.evaluate(_, graph)), graph)

  private[assembly] def comparisonForClaim(
      claim: JudgmentClaim,
      graph: TypedEvidenceGraph
  ): Option[EvalComparison] =
    if claim.family != ClaimFamily.Evaluation then None
    else
      val comparisons =
        claim.evidence.flatMap(ref =>
          graph.byId.get(ref.id).toList.flatMap {
            case EvidenceRecord(_, RelativeAssessmentEvidence(assessment), _) =>
              graph.comparisonFor(assessment).map(_.comparison).toList
            case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) =>
              graph.comparisonFor(cause).map(_.comparison).toList
            case EvidenceRecord(_, CandidateComparisonEvidence(fact), _)
                if fact.kind == CandidateComparisonKind.PlayedVsBest =>
              List(fact.comparison)
            case _ =>
              Nil
          }
        ).distinct
      comparisons match
        case Nil =>
          None
        case comparison :: Nil =>
          Some(comparison)
        case _ =>
          throw IllegalArgumentException(
            s"evaluation claim '${claim.id}' refers to conflicting engine comparisons"
          )

  private[assembly] def candidateSetForClaim(
      claim: JudgmentClaim,
      graph: TypedEvidenceGraph
  ): Option[CandidateSetDescriptor] =
    val descriptors =
      claim.evidence.flatMap(ref =>
        graph.byId.get(ref.id).toList.flatMap {
          case EvidenceRecord(_, RelativeAssessmentEvidence(assessment), _) =>
            graph.candidateSetFor(assessment).toList
          case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) =>
            graph.comparisonFor(cause).flatMap(graph.candidateSetFor).toList
          case EvidenceRecord(_, CandidateComparisonEvidence(fact), _) =>
            graph.candidateSetFor(fact).toList
          case _ =>
            Nil
        }
      ).distinct
    descriptors match
      case descriptor :: Nil => Some(descriptor)
      case Nil                => None
      case _ =>
        throw IllegalArgumentException(
          s"claim '${claim.id}' refers to conflicting candidate-set descriptors"
        )

private object RelativeCauseClaimDepth:

  def hasOwnedDepth(cause: RelativeCauseFact, graph: TypedEvidenceGraph): Boolean =
    cause.hasOwnedTypedDepth(graph)

  def hasTacticalDepth(cause: RelativeCauseFact, graph: TypedEvidenceGraph): Boolean =
    isTactical(cause.kind) &&
      cause.hasOwnedTacticalProof(graph)

  def hasMaterialDepth(cause: RelativeCauseFact, graph: TypedEvidenceGraph): Boolean =
      ClaimFamily.fromCause(cause.kind).contains(ClaimFamily.Material) &&
      hasOwnedDepth(cause, graph)

  def hasConversionDepth(cause: RelativeCauseFact, graph: TypedEvidenceGraph): Boolean =
    (
      ClaimFamily.fromCause(cause.kind).contains(ClaimFamily.Conversion) ||
        cause.kind == RelativeCauseKind.RecaptureRecoveryWindow ||
      cause.kind == RelativeCauseKind.MaterialSwing
    ) &&
      hasOwnedDepth(cause, graph)

  def hasMaterialSwingTacticalDepth(cause: RelativeCauseFact, graph: TypedEvidenceGraph): Boolean =
    cause.kind == RelativeCauseKind.MaterialSwing &&
      ClaimFamily.fromCause(cause.kind).contains(ClaimFamily.Material) &&
      cause.hasOwnedTacticalProof(graph)

  def hasLineDriver(cause: RelativeCauseFact, proof: RelativeCauseProof, graph: TypedEvidenceGraph): Boolean =
    graph.relativeCauseProofHasLineDriver(cause.kind, proof)

  def hasTransitionDriver(cause: RelativeCauseFact, proof: RelativeCauseProof, graph: TypedEvidenceGraph): Boolean =
    graph.relativeCauseProofHasTransitionDriver(cause.kind, proof)

  def hasStrategicContrastDepth(cause: RelativeCauseFact, graph: TypedEvidenceGraph): Boolean =
    cause.hasOwnedAdmissibleLongTermProof(graph)

  private def isTactical(kind: RelativeCauseKind): Boolean =
    ClaimFamily.fromCause(kind).contains(ClaimFamily.Tactical)

object ClaimDeduplicator:

  private final case class AggregatedClaimDecision(
      index: Int,
      decision: ClaimAdmissionDecision
  )

  private def aggregateCertified(
      decisions: List[ClaimAdmissionDecision],
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
          decision = mergeDecisions(decisionsInGroup, graph)
        )
      }.toList
    val passthrough =
      decisions.zipWithIndex.collect { case (decision, index) if !aggregationEligible(decision.claim, graph) =>
        AggregatedClaimDecision(index, decision)
      }
    (passthrough ++ aggregated).sortBy(_.index)

  def deduplicate(
      decisions: List[ClaimAdmissionDecision],
      graph: TypedEvidenceGraph
  ): List[ClaimAdmissionDecision] =
    val aggregated = aggregateCertified(decisions, graph)
    aggregated
      .groupBy(item => key(item.decision.claim))
      .values
      .map(_.maxBy(item => score(item.decision, graph)).decision)
      .toList

  private def key(claim: JudgmentClaim): (ClaimFamily, ClaimSubject, Option[LineNodeRef], Option[String]) =
    (
      claim.family,
      claim.subject,
      claim.primaryLine,
      claim.subjectMove
    )

  private final case class AggregationKey(
      family: ClaimFamily,
      subject: ClaimSubject,
      primaryPosition: PositionNodeRef,
      primaryLine: Option[LineNodeRef],
      subjectMove: Option[String],
      scope: Option[EvidenceScope],
      semanticAnchors: List[EvidenceSemanticAnchor]
  )

  private def aggregationEligible(
      claim: JudgmentClaim,
      graph: TypedEvidenceGraph
  ): Boolean =
    claim.family.isLongTerm &&
      ClaimCandidateGraphAssembler.comparisonForClaim(claim, graph).isEmpty &&
      claim.evidence.nonEmpty &&
      !claim.evidence.exists(ref => ClaimEvidenceSemantics.longTermSupportExcludedLayer(ref.layer)) &&
      !claim.evidence.exists(ref => StrategicMechanismEvidence.rawStrategicSourceLayer(ref.layer)) &&
      ClaimEvidenceSemantics.semanticAnchors(claim, graph).nonEmpty

  private def aggregationKey(
      claim: JudgmentClaim,
      graph: TypedEvidenceGraph
  ): AggregationKey =
    AggregationKey(
      family = claim.family,
      subject = claim.subject,
      primaryPosition = claim.primaryPosition,
      primaryLine = claim.primaryLine,
      subjectMove = claim.subjectMove,
      scope = scopeAnchor(claim),
      semanticAnchors = ClaimEvidenceSemantics.semanticAnchors(claim, graph)
    )

  private def scopeAnchor(claim: JudgmentClaim): Option[EvidenceScope] =
    Option.when(claim.primaryLine.nonEmpty || claim.subjectMove.nonEmpty)(claim.scope)

  private def mergeDecisions(decisions: Iterable[ClaimAdmissionDecision], graph: TypedEvidenceGraph): ClaimAdmissionDecision =
    val list = decisions.toList
    val mergedClaim = mergeClaims(list.map(_.claim), graph)
    val mergedPresentLayers = list.flatMap(_.presentLayers).toSet
    val mergedMissingLayerGroups = list.flatMap(_.missingLayerGroups).distinct
    val mergedMissingEvidence = list.flatMap(_.missingEvidence).distinctBy(_.id)
    list.maxBy(decision => score(decision, graph)).copy(
      claim = mergedClaim,
      presentLayers = mergedPresentLayers,
      missingLayerGroups = mergedMissingLayerGroups,
      missingEvidence = mergedMissingEvidence
    )

  private def mergeClaims(claims: Iterable[JudgmentClaim], graph: TypedEvidenceGraph): JudgmentClaim =
    val claimList = claims.toList
    val mergeBase = claimList.maxBy(claim => claimScore(claim, graph))
    if claimList.size == 1 then mergeBase
    else
      mergeBase.copy(
        evidence = claimList.flatMap(_.evidence).distinctBy(_.id),
        confidence = claimList.map(_.confidence).maxBy(confidenceScore),
        salience = None
      )

  private def claimScore(claim: JudgmentClaim, graph: TypedEvidenceGraph): Int =
    claimEvidencePriority(claim, graph) * 10 +
      ClaimCandidateGraphAssembler.comparisonForClaim(claim, graph).map(_ => 50).getOrElse(0) +
      confidenceScore(claim.confidence)

  private def score(decision: ClaimAdmissionDecision, graph: TypedEvidenceGraph): Int =
    claimEvidencePriority(decision.claim, graph) * 10 +
      confidenceScore(decision.claim.confidence)

  private def claimEvidencePriority(claim: JudgmentClaim, graph: TypedEvidenceGraph): Int =
    val weightedEvidence =
      claim.evidence
        .flatMap(ref => graph.byId.get(ref.id))
        .filter(record => priorityEvidenceRecord(claim.family, record, graph))
        .map(_.ref.id)
        .distinct
        .size
    if claim.evidence.nonEmpty then weightedEvidence.max(1) else 0

  private def priorityEvidenceRecord(
      family: ClaimFamily,
      record: EvidenceRecord,
      graph: TypedEvidenceGraph
  ): Boolean =
    family match
      case ClaimFamily.Tactical =>
        tacticalPriorityEvidenceRecord(record, graph)
      case family if family.isLongTerm =>
        !ClaimEvidenceSemantics.longTermSupportExcludedLayer(record.ref.layer) &&
          !StrategicMechanismEvidence.rawStrategicSourceLayer(record.ref.layer)
      case _ =>
        true

  private def tacticalPriorityEvidenceRecord(record: EvidenceRecord, graph: TypedEvidenceGraph): Boolean =
    record.payload match
      case _: TacticalMechanismEvidence | _: RelationFactEvidence | _: MoveMotifEvidence | _: LineFactEvidence |
          EvalFactEvidence(_, _, _, _) | _: ThreatEpisodeEvidence |
          MoveTransitionEvidence(_, _, _) | RelativeAssessmentEvidence(_) |
          CandidateComparisonEvidence(_) =>
        true
      case RelativeCauseFactEvidence(cause) =>
        RelativeCauseClaimDepth.hasTacticalDepth(cause, graph)
      case _ =>
        false

  private def confidenceScore(confidence: EvidenceConfidence): Int =
    confidence match
      case EvidenceConfidence.LegalReplayVerified => 40
      case EvidenceConfidence.EngineBacked        => 30
      case EvidenceConfidence.BoardDerived        => 25
      case EvidenceConfidence.Mixed               => 15
      case EvidenceConfidence.Heuristic           => 5

object ClaimArbitrator:

  def rank(
      graph: ClaimCandidateGraph,
      relativeAssessments: List[RelativeMoveAssessment]
  ): List[JudgmentClaim] =
    val playedMoves = relativeAssessments.map(assessment => normalizeMove(assessment.played.moveUci)).toSet
    val certifiedDecisions = ClaimDeduplicator.deduplicate(graph.certified, graph.evidenceGraph)
    val claims = certifiedDecisions.map(_.claim)
    certifiedDecisions
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
          -priority(decision, salience, graph.evidenceGraph)
        )
      }
      .map { case (decision, salience, _) =>
        decision.claim.copy(
          salience = Some(salience)
        )
      }

  private def priority(
      decision: ClaimAdmissionDecision,
      salience: ClaimSalience,
      graph: TypedEvidenceGraph
  ): Int =
    val claim = decision.claim
    val verdict =
      if claim.family == ClaimFamily.Evaluation then
        ClaimCandidateGraphAssembler.comparisonForClaim(claim, graph).map(_.verdict)
      else claimRelativeVerdicts(claim, graph).headOption
    val hasRelativeProof = claimRelativeComparisons(claim, graph).nonEmpty || claimRelativeVerdicts(claim, graph).nonEmpty
    salience.score * 1000 +
      verdictFit(claim, verdict, hasRelativeProof) * 200 +
      familyBaseline(claim.family) * 50 +
      confidencePriority(claim.confidence) * 40 +
      decision.presentLayers.size * 10 +
      claimEvidencePriority(claim, graph)

  private def claimEvidencePriority(claim: JudgmentClaim, graph: TypedEvidenceGraph): Int =
    val weightedEvidence =
      salienceRecordsForClaim(claim, claimRecords(claim, graph), graph).map(_.ref.id).distinct.size
    if claim.evidence.nonEmpty then weightedEvidence.max(1) else 0

  private def claimSalience(claim: JudgmentClaim, graph: TypedEvidenceGraph): ClaimSalience =
    val records = claim.evidence.flatMap(ref => graph.byId.get(ref.id))
    val scoringRecords = salienceRecordsForClaim(claim, records, graph)
    val evidenceScore =
      scoringRecords
        .groupBy(_.ref.layer)
        .values
        .map(recordsForLayer => recordsForLayer.map(recordSalience(_, graph)).maxOption.getOrElse(0))
        .sum
    val topLevelComparisonSalience =
      if claim.family == ClaimFamily.Evaluation then
        engineComparisonSalience(
          ClaimCandidateGraphAssembler.comparisonForClaim(claim, graph),
          ClaimCandidateGraphAssembler.candidateSetForClaim(claim, graph)
        )
      else 0
    val claimComparison = ClaimCandidateGraphAssembler.comparisonForClaim(claim, graph)
    val claimCandidateSet = ClaimCandidateGraphAssembler.candidateSetForClaim(claim, graph)
    val drivers =
      (scoringRecords.flatMap(record => recordDrivers(record, graph)) ++
        Option.when(topLevelComparisonSalience > 0)(ClaimSalienceDriver.EngineSwing) ++
        claimCandidateSet.toList.flatMap(cs =>
          Option.when(cs.onlyMove)(ClaimSalienceDriver.CandidateConstraint)
        )).distinct
    ClaimSalience(
      score = (evidenceScore + topLevelComparisonSalience).min(30),
      drivers = drivers
    )

  private def claimInteractions(
      claim: JudgmentClaim,
      claims: List[JudgmentClaim],
      graph: TypedEvidenceGraph,
      playedMoves: Set[String]
  ): List[ClaimInteraction] =
    def relatedFor(accept: JudgmentClaim => Boolean): List[(JudgmentClaim, List[EvidenceRef], List[EvidenceRef])] =
      claims
        .filterNot(_.id == claim.id)
        .filter(accept)
        .flatMap { other =>
          val evidence = interactionEvidence(claim, other, graph)
          val basis = interactionBasis(claim, other, graph, evidence)
          Option.when(evidence.nonEmpty && basis.nonEmpty)(other, evidence, basis)
        }
    val causes = relativeCauses(claim, graph)
    val primaryInteractionCauses = causes.filter(cause => primaryInteractionCause(cause, graph))
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
              causeEvidence = basis
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
              causeEvidence = basis
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
                    causeEvidence = basis
                  )
                ).filter(interaction => interaction.interactionEvidence.nonEmpty && interaction.causeEvidence.nonEmpty)
              case (ClaimInteractionKind.TacticalSupportsStrategicPlan, strength) =>
                val supportEvidence = interactionEvidence(claim, other, graph)
                val supportBasis = interactionBasis(claim, other, graph, supportEvidence)
                Some(
                  ClaimInteraction(
                    kind = ClaimInteractionKind.TacticalSupportsStrategicPlan,
                    relatedClaimId = other.id,
                    strength = strength,
                    interactionEvidence = supportEvidence,
                    causeEvidence = supportBasis
                  )
                ).filter(interaction => interaction.interactionEvidence.nonEmpty && interaction.causeEvidence.nonEmpty)
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
              causeEvidence = basis
            )
          }
        )
        .getOrElse(Nil),
      Option
        .when(
          claim.family == ClaimFamily.Conversion &&
            primaryInteractionCauses.exists(cause => conversionInteractionCause(cause, graph))
        )(
          relatedFor(other =>
            other.family.isLongTerm || other.family == ClaimFamily.Evaluation
          ).map { case (other, evidence, basis) =>
            ClaimInteraction(
              kind = ClaimInteractionKind.ConversionSecuresAdvantage,
              relatedClaimId = other.id,
              strength = 4,
              interactionEvidence = evidence,
              causeEvidence = basis
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
              causeEvidence = basis
            )
          }
        )
        .getOrElse(Nil),
      Option
        .when(badVerdict && claim.family != ClaimFamily.Evaluation)(
          relatedFor(_.family == ClaimFamily.Evaluation).map { case (other, evidence, basis) =>
            ClaimInteraction(
              kind = ClaimInteractionKind.BadVerdictPreservesLocalClaim,
              relatedClaimId = other.id,
              strength = 5,
              interactionEvidence = evidence,
              causeEvidence = basis
            )
          }
        )
        .getOrElse(Nil)
    )
      .filter(_.interactionEvidence.nonEmpty)
      .distinctBy(interaction =>
        (interaction.kind, interaction.relatedClaimId, interaction.causeEvidence.map(_.id).sorted)
      )

  private def tacticalLongTermInteractionFor(
      claim: JudgmentClaim,
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
      claim: JudgmentClaim,
      graph: TypedEvidenceGraph,
      playedMoves: Set[String]
  ): Boolean =
    PlayerFacingClaimPolicy.tier(claim, graph, playedMoves) == PlayerFacingClaimTier.Primary &&
      claimRecords(claim, graph).exists(record =>
        JudgmentSubjectBinding.recordBinding(record, graph, playedMoves) == SubjectBindingClass.PrimaryPlayedCause
      )

  private def tacticalLongTermConstraintKind(claim: JudgmentClaim, graph: TypedEvidenceGraph): ClaimInteractionKind =
    if relativeCauses(claim, graph).exists(cause =>
        primaryInteractionCause(cause, graph) &&
          RelativeCauseClaimDepth.hasTacticalDepth(cause, graph) &&
          (
            cause.kind == RelativeCauseKind.TacticalRefutationOfPlayed ||
              cause.kind == RelativeCauseKind.MissedTacticalResource ||
              cause.kind == RelativeCauseKind.CandidateTacticalLiability
          )
      )
    then ClaimInteractionKind.TacticalRefutesStrategicPlan
    else ClaimInteractionKind.TacticalConstrainsLongTerm

  private def tacticalConstrainsLongTerm(claim: JudgmentClaim, graph: TypedEvidenceGraph): Boolean =
    val drivers = claimDrivers(claim, graph)
    val causes = relativeCauses(claim, graph)
    val hasMaterialTactic = causes.exists(cause =>
      RelativeCauseClaimDepth.hasMaterialSwingTacticalDepth(cause, graph)
    )
    val hasTacticalDriver = drivers.contains(ClaimSalienceDriver.TacticalRelation) || hasMaterialTactic
    val hasForcingOrEngine =
      causes.exists(cause => RelativeCauseClaimDepth.hasTacticalDepth(cause, graph)) ||
      drivers.contains(ClaimSalienceDriver.ForcingLine) ||
      drivers.contains(ClaimSalienceDriver.CandidateConstraint) ||
      claimRelativeComparisons(claim, graph).exists(comparisonProvesTacticalConstraint)
    claim.family == ClaimFamily.Tactical && hasTacticalDriver && hasForcingOrEngine

  private def tacticalSupportsLongTerm(claim: JudgmentClaim, graph: TypedEvidenceGraph): Boolean =
    val drivers = claimDrivers(claim, graph)
    claim.family == ClaimFamily.Tactical &&
      drivers.contains(ClaimSalienceDriver.TacticalRelation) &&
      !tacticalConstrainsLongTerm(claim, graph) &&
      claimRelativeComparisons(claim, graph).exists(_.winPercentLossForMover < JudgmentThresholds.INACCURACY_WP)

  private def tacticalConstraintStrength(claim: JudgmentClaim, graph: TypedEvidenceGraph): Int =
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

  private def claimDrivers(claim: JudgmentClaim, graph: TypedEvidenceGraph): List[ClaimSalienceDriver] =
    val records = salienceRecordsForClaim(claim, claimRecords(claim, graph), graph)
    val comparison = ClaimCandidateGraphAssembler.comparisonForClaim(claim, graph)
    val candidateSet = ClaimCandidateGraphAssembler.candidateSetForClaim(claim, graph)
    (records.flatMap(record => recordDrivers(record, graph)) ++
      Option.when(comparison.nonEmpty)(ClaimSalienceDriver.EngineSwing) ++
      candidateSet.toList.flatMap(cs =>
        Option.when(cs.onlyMove)(ClaimSalienceDriver.CandidateConstraint)
      )).distinct

  private def salienceRecordsForClaim(
      claim: JudgmentClaim,
      records: List[EvidenceRecord],
      graph: TypedEvidenceGraph
  ): List[EvidenceRecord] =
    claim.family match
      case ClaimFamily.Tactical =>
        records.filter(record => tacticalSalienceRecord(record, graph))
      case family if family.isLongTerm =>
        records.filter(longTermSalienceRecord)
      case _ =>
        records

  private def longTermSalienceRecord(record: EvidenceRecord): Boolean =
    !ClaimEvidenceSemantics.longTermSupportExcludedLayer(record.ref.layer) &&
      (record.ref.layer match
        case layer if StrategicMechanismEvidence.rawStrategicSourceLayer(layer) =>
          false
        case _ =>
          true
      )

  private def tacticalSalienceRecord(record: EvidenceRecord, graph: TypedEvidenceGraph): Boolean =
    record.payload match
      case _: TacticalMechanismEvidence | _: RelationFactEvidence | _: MoveMotifEvidence | _: LineFactEvidence |
          EvalFactEvidence(_, _, _, _) | _: ThreatEpisodeEvidence |
          MoveTransitionEvidence(_, _, _) | RelativeAssessmentEvidence(_) |
          CandidateComparisonEvidence(_) =>
        true
      case RelativeCauseFactEvidence(cause) =>
        RelativeCauseClaimDepth.hasTacticalDepth(cause, graph)
      case _ =>
        false

  private def comparisonProvesTacticalConstraint(comparison: EvalComparison): Boolean =
    comparison.winPercentLossForMover >= JudgmentThresholds.INACCURACY_WP ||
      comparison.candidateWinPercentDeltaForMover >= JudgmentThresholds.PLAYABLE_LOSS_WP

  private def normalizeMove(raw: String): String =
    Option(raw).getOrElse("").trim.toLowerCase

  private def exactSharedEvidence(left: JudgmentClaim, right: JudgmentClaim): List[EvidenceRef] =
    val leftIds = left.evidence.map(_.id).toSet
    right.evidence.filter(ref => leftIds.contains(ref.id)).distinctBy(_.id)

  private def interactionEvidence(
      left: JudgmentClaim,
      right: JudgmentClaim,
      graph: TypedEvidenceGraph
  ): List[EvidenceRef] =
    if eventLongTermPair(left, right) then
      eventLongTermInteractionEvidence(left, right, graph)
    else eventEventInteractionEvidence(left, right, graph)

  private def interactionBasis(
      left: JudgmentClaim,
      right: JudgmentClaim,
      graph: TypedEvidenceGraph,
      evidence: List[EvidenceRef]
  ): List[EvidenceRef] =
    val evidenceIds = evidence.map(_.id).toSet
    val eventLongTerm = eventLongTermPair(left, right)
    val leftCauses = relativeCauses(left, graph)
    val rightCauses = relativeCauses(right, graph)
    val sharedSignatures =
      if eventLongTerm then Set.empty
      else
        leftCauses
          .map(cause => relativeCauseSignature(cause, graph))
          .toSet
          .intersect(rightCauses.map(cause => relativeCauseSignature(cause, graph)).toSet)
    val causes =
      if eventLongTerm then
        val eventClaim = if left.family.isEvent then left else right
        relativeCauses(eventClaim, graph)
      else
        leftCauses ++ rightCauses
    causes
      .filter(cause =>
        sharedSignatures.contains(relativeCauseSignature(cause, graph)) ||
          interactionCauseBacksEvidence(cause, evidenceIds)
      )
      .flatMap(cause => relativeCauseEvidenceRefs(cause, graph))
      .distinctBy(_.id)

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

  private def relativeCauseEvidenceRefs(
      cause: RelativeCauseFact,
      graph: TypedEvidenceGraph
  ): List[EvidenceRef] =
    val fullIdentity = cause.identityKey(graph)
    graph.records.collect {
      case EvidenceRecord(ref, RelativeCauseFactEvidence(candidate), _)
          if candidate == cause && candidate.identityKey(graph) == fullIdentity =>
        ref
    }

  private def eventLongTermPair(left: JudgmentClaim, right: JudgmentClaim): Boolean =
    (left.family.isEvent && right.family.isLongTerm) ||
      (right.family.isEvent && left.family.isLongTerm)

  private def eventLongTermInteractionEvidence(
      left: JudgmentClaim,
      right: JudgmentClaim,
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
      left: JudgmentClaim,
      right: JudgmentClaim,
      graph: TypedEvidenceGraph
  ): List[EvidenceRef] =
    val shared = exactSharedEvidence(left, right)
    if shared.nonEmpty then shared
    else if relativeCauseSignatureOverlap(left, right, graph) then
      comparisonEvidence(left, graph) ++ comparisonEvidence(right, graph)
    else
      Nil

  private def eventDepthProofRefs(claim: JudgmentClaim, graph: TypedEvidenceGraph): List[EvidenceRef] =
    relativeCauses(claim, graph)
      .flatMap(_.proof.toList)
      .flatMap(proof => proof.directProof.sourceRefs ++ proof.contrastProof.sourceRefs)
      .distinctBy(_.id)

  private def relativeCauseSignatureOverlap(
      left: JudgmentClaim,
      right: JudgmentClaim,
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

  private def relativeCauseSignatures(claim: JudgmentClaim, graph: TypedEvidenceGraph): Set[RelativeCauseInteractionSignature] =
    relativeCauses(claim, graph)
      .map(cause => relativeCauseSignature(cause, graph))
      .toSet

  private def relativeCauseSignature(
      cause: RelativeCauseFact,
      graph: TypedEvidenceGraph
  ): RelativeCauseInteractionSignature =
    val comparison = requiredComparison(cause, graph)
    val binding = graph.requiredRelativeCauseBinding(cause)
    val strategicProof = cause.strategicProofIdentity(graph)
    (
      cause.kind,
      comparison.kind,
      binding.role,
      cause.sourceSide,
      binding.importance,
      cause.attribution.kind,
      cause.attribution.rootMoveMatched,
      cause.attribution.directProofEligible,
      comparison.referenceLine,
      comparison.candidateLine,
      binding.eventLine,
      cause.proof.toList.flatMap(_.directProof.sourceRefs.map(_.id)).distinct.sorted,
      cause.proof.toList.flatMap(_.contrastProof.sourceRefs.map(_.id)).distinct.sorted,
      cause.proof.toList.flatMap(_.contextSupport.sourceRefs.map(_.id)).distinct.sorted,
      strategicProof.axisKeys,
      strategicProof.mechanismKinds,
      strategicProof.mechanismSourceIds,
      strategicProof.signalSourceIds,
      cause.supportEvidence.map(_.id).distinct.sorted
    )

  private def comparisonEvidence(claim: JudgmentClaim, graph: TypedEvidenceGraph): List[EvidenceRef] =
    claim.evidence.filter(ref =>
      graph.byId.get(ref.id).exists(record =>
        record.payload match
          case RelativeCauseFactEvidence(_) | RelativeAssessmentEvidence(_) | CandidateComparisonEvidence(_) =>
            true
          case _ =>
            false
      )
    ).distinctBy(_.id)

  private def claimRecords(claim: JudgmentClaim, graph: TypedEvidenceGraph): List[EvidenceRecord] =
    claim.evidence.flatMap(ref => graph.byId.get(ref.id))

  private def claimRelativeComparisons(claim: JudgmentClaim, graph: TypedEvidenceGraph): List[EvalComparison] =
    val recordComparisons =
      claimRecords(claim, graph).collect {
        case record @ EvidenceRecord(_, RelativeAssessmentEvidence(assessment), _) if comparisonRecordUsable(record) =>
          graph.comparisonFor(assessment).map(_.comparison)
        case record @ EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) if comparisonRecordUsable(record) =>
          graph.comparisonFor(cause).map(_.comparison)
        case record @ EvidenceRecord(_, CandidateComparisonEvidence(fact), _) if comparisonRecordUsable(record) =>
          Some(fact.comparison)
      }.flatten
    val topLevelEvaluation =
      ClaimCandidateGraphAssembler.comparisonForClaim(claim, graph).toList
    (recordComparisons ++ topLevelEvaluation).distinct

  private def comparisonRecordUsable(record: EvidenceRecord): Boolean =
    record.ref.confidence == EvidenceConfidence.EngineBacked

  private def claimRelativeVerdicts(claim: JudgmentClaim, graph: TypedEvidenceGraph): List[MoveChoiceVerdict] =
    claimRelativeComparisons(claim, graph).map(_.verdict).distinct

  private def claimHasBadRelativeOutcome(claim: JudgmentClaim, graph: TypedEvidenceGraph): Boolean =
    claimRelativeVerdicts(claim, graph).exists(badVerdict)

  private def badVerdict(verdict: MoveChoiceVerdict): Boolean =
    verdict == MoveChoiceVerdict.Inaccuracy ||
      verdict == MoveChoiceVerdict.Mistake ||
      verdict == MoveChoiceVerdict.Blunder

  private def relativeCauses(claim: JudgmentClaim, graph: TypedEvidenceGraph): List[RelativeCauseFact] =
    claimRecords(claim, graph).flatMap {
      case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) =>
        List(cause)
      case _ =>
        Nil
    }.distinct

  private def requiredComparison(
      cause: RelativeCauseFact,
      graph: TypedEvidenceGraph
  ): CandidateComparisonFact =
    graph.comparisonFor(cause).getOrElse(
      throw IllegalArgumentException(
        s"relative cause '${cause.comparisonEvidence.id}' is not bound to a candidate comparison"
      )
    )

  private def primaryInteractionCause(cause: RelativeCauseFact, graph: TypedEvidenceGraph): Boolean =
    graph.relativeCauseBinding(cause).exists(binding =>
      binding.role == RelativeCauseRole.PrimaryPlayedCause &&
        binding.importance == RelativeCauseImportance.Primary
    ) &&
      graph.comparisonFor(cause).exists(fact => badVerdict(fact.comparison.verdict))

  private def conversionInteractionCause(cause: RelativeCauseFact, graph: TypedEvidenceGraph): Boolean =
    cause.kind == RelativeCauseKind.ConversionSecured &&
      RelativeCauseClaimDepth.hasConversionDepth(cause, graph)

  private def defensiveNecessityCause(kind: RelativeCauseKind): Boolean =
    kind != RelativeCauseKind.DrawResource &&
      ClaimFamily.fromCause(kind).contains(ClaimFamily.Defensive)

  private def recordSalience(record: EvidenceRecord, graph: TypedEvidenceGraph): Int =
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
      case _: PlanPressureEvidence | PawnStructureFactEvidence(_, _) | _: StrategicFactEvidence =>
        0
      case _: StrategicMechanismEvidence =>
        0
      case _: StrategicMechanismContrastEvidence =>
        0
      case RelativeAssessmentEvidence(assessment) =>
        engineComparisonSalience(
          graph.comparisonFor(assessment).map(_.comparison),
          graph.candidateSetFor(assessment)
        )
      case CandidateComparisonEvidence(fact) =>
        engineComparisonSalience(Some(fact.comparison), graph.candidateSetFor(fact)) +
          candidateComparisonKindSalience(fact.kind)
      case RelativeCauseFactEvidence(cause) =>
        relativeCauseSalience(cause, graph)
      case _: StructuralDeltaEvidence =>
        0
      case _: PlanCausalEventEvidence =>
        0
      case OpeningContextEvidence(_, _, _, _) =>
        0
      case FeatureAnchorEvidence(_) | ApplicabilityAssessmentEvidence(_) =>
        0
      case _: BoardFactEvidence =>
        0
      case _: LineFactEvidence =>
        0
      case EvalFactEvidence(_, _, _, _) =>
        0
      case _: MoveMotifEvidence =>
        0
      case MoveTransitionEvidence(_, _, _) =>
        1
      case PlanTransitionEvidence(_) =>
        0

  private def recordDrivers(record: EvidenceRecord, graph: TypedEvidenceGraph): List[ClaimSalienceDriver] =
    record.payload match
      case _: RelationFactEvidence =>
        Nil
      case payload: TacticalMechanismEvidence if payload.defensive =>
        Option.when(payload.canAnchorDefensiveClaim)(ClaimSalienceDriver.DefensiveUrgency).toList
      case payload: TacticalMechanismEvidence =>
        ClaimSalienceDriver.TacticalRelation ::
          Option
            .when(payload.hasEngineOrForcingProof || payload.kind == TacticalMechanismKind.KingForcing)(ClaimSalienceDriver.ForcingLine)
            .toList
      case payload: ThreatEpisodeEvidence =>
        Option.when(!payload.insufficientData && payload.defenseRequired)(ClaimSalienceDriver.DefensiveUrgency).toList
      case _: PlanPressureEvidence | PawnStructureFactEvidence(_, _) | _: StrategicFactEvidence =>
        Nil
      case _: StrategicMechanismEvidence =>
        Nil
      case _: StrategicMechanismContrastEvidence =>
        Nil
      case _: StructuralDeltaEvidence | FeatureAnchorEvidence(_) | ApplicabilityAssessmentEvidence(_) =>
        Nil
      case RelativeAssessmentEvidence(_) =>
        List(ClaimSalienceDriver.EngineSwing)
      case CandidateComparisonEvidence(fact) =>
        ClaimSalienceDriver.EngineSwing ::
          Option.when(fact.kind == CandidateComparisonKind.BestVsSecond)(ClaimSalienceDriver.CandidateConstraint).toList
      case RelativeCauseFactEvidence(cause) =>
        relativeCauseDrivers(cause, graph)
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

  private def engineComparisonSalience(
      comparison: Option[EvalComparison],
      candidateSet: Option[CandidateSetDescriptor]
  ): Int =
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
        candidateSet.map(cs => if cs.onlyMove then 3 else 0).getOrElse(0)
    }.getOrElse(0)

  private def candidateComparisonKindSalience(kind: CandidateComparisonKind): Int =
    kind match
      case CandidateComparisonKind.PlayedVsBest          => 4
      case CandidateComparisonKind.BestVsSecond          => 3
      case CandidateComparisonKind.PlayedVsAlternative   => 2
      case CandidateComparisonKind.ReferenceVsAlternative => 1

  private def relativeCauseSalience(cause: RelativeCauseFact, graph: TypedEvidenceGraph): Int =
    if cause.strategicCauseKind && !RelativeCauseClaimDepth.hasStrategicContrastDepth(cause, graph) then 0
    else
      val comparison = requiredComparison(cause, graph)
      val binding = graph.requiredRelativeCauseBinding(cause)
      val causeScore =
        cause.kind match
          case RelativeCauseKind.TacticalRefutationOfPlayed | RelativeCauseKind.MissedTacticalResource |
              RelativeCauseKind.CandidateTacticalLiability |
              RelativeCauseKind.WrongRecapturer | RelativeCauseKind.RecaptureRecoveryWindow |
              RelativeCauseKind.TempoLoss | RelativeCauseKind.KingForcing =>
            if RelativeCauseClaimDepth.hasTacticalDepth(cause, graph) then 8
            else if RelativeCauseClaimDepth.hasOwnedDepth(cause, graph) then 4
            else 0
          case RelativeCauseKind.OnlyMoveNecessity | RelativeCauseKind.OnlyDefenseNecessity =>
            7
          case RelativeCauseKind.ConversionMiss | RelativeCauseKind.ConversionSecured =>
            if RelativeCauseClaimDepth.hasConversionDepth(cause, graph) then 5 else 0
          case RelativeCauseKind.PlanContradiction =>
            if RelativeCauseClaimDepth.hasStrategicContrastDepth(cause, graph) then 3 else 0
          case RelativeCauseKind.StrategicConcession |
              RelativeCauseKind.MissedStrategicImprovement | RelativeCauseKind.StructuralImprovement |
              RelativeCauseKind.TargetPressureGain | RelativeCauseKind.TargetPressureRelease |
              RelativeCauseKind.CenterControlGain |
              RelativeCauseKind.KingSafetyConcession | RelativeCauseKind.PawnWeaknessTarget |
              RelativeCauseKind.PawnBreakOpportunity | RelativeCauseKind.ActivityGain | RelativeCauseKind.ActivityLoss |
              RelativeCauseKind.OpponentRestriction =>
            if RelativeCauseClaimDepth.hasStrategicContrastDepth(cause, graph) then 5 else 0
          case RelativeCauseKind.DefensiveResource | RelativeCauseKind.DrawResource |
              RelativeCauseKind.SacrificeCompensation =>
            4
          case RelativeCauseKind.PlanImprovement =>
            if RelativeCauseClaimDepth.hasStrategicContrastDepth(cause, graph) then 4 else 0
          case RelativeCauseKind.MaterialSwing =>
            if RelativeCauseClaimDepth.hasMaterialDepth(cause, graph) then 3 else 0
          case RelativeCauseKind.WrongMoveOrder =>
            if RelativeCauseClaimDepth.hasTacticalDepth(cause, graph) then 6 else 0
      val roleScore =
        binding.importance match
          case RelativeCauseImportance.Primary    => 4
          case RelativeCauseImportance.Supporting => 2
          case RelativeCauseImportance.Context    => 0
      val sourceScore =
        cause.sourceSide match
          case RelativeCauseSourceSide.Candidate | RelativeCauseSourceSide.Reference => 2
          case RelativeCauseSourceSide.Mixed                                        => 1
          case RelativeCauseSourceSide.Shared                                       => 0
      val comparisonScore =
        candidateComparisonKindSalience(comparison.kind)
      val proofScore =
        if RelativeCauseClaimDepth.hasOwnedDepth(cause, graph) then
          cause.proof
            .map(proof =>
              List(
                Option.when(graph.relativeCauseProofHasRawDirectProof(cause.kind, proof))(3),
                Option.when(graph.relativeCauseProofHasRawContrastProof(cause.kind, proof))(2)
              ).flatten.sum
            )
            .getOrElse(0)
        else 0
      causeScore +
        roleScore +
        sourceScore +
        comparisonScore +
        proofScore +
        graph
          .comparisonFor(cause)
          .map(fact => math.round(fact.comparison.winPercentLossForMover.min(20.0) / 5.0).toInt)
          .getOrElse(0)

  private def relativeCauseDrivers(
      cause: RelativeCauseFact,
      graph: TypedEvidenceGraph
  ): List[ClaimSalienceDriver] =
    if cause.strategicCauseKind && !RelativeCauseClaimDepth.hasStrategicContrastDepth(cause, graph) then Nil
    else
      val comparison = requiredComparison(cause, graph)
      val binding = graph.requiredRelativeCauseBinding(cause)
      val kindDrivers =
        cause.kind match
          case RelativeCauseKind.MissedTacticalResource | RelativeCauseKind.TacticalRefutationOfPlayed |
              RelativeCauseKind.CandidateTacticalLiability |
              RelativeCauseKind.WrongRecapturer | RelativeCauseKind.RecaptureRecoveryWindow |
              RelativeCauseKind.WrongMoveOrder | RelativeCauseKind.TempoLoss | RelativeCauseKind.KingForcing =>
            if RelativeCauseClaimDepth.hasTacticalDepth(cause, graph) then
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
          Option.when(comparison.kind == CandidateComparisonKind.BestVsSecond)(ClaimSalienceDriver.CandidateConstraint),
          Option.when(binding.role == RelativeCauseRole.CandidateSetConstraint)(ClaimSalienceDriver.CandidateConstraint),
          Option.when(RelativeCauseClaimDepth.hasTacticalDepth(cause, graph))(ClaimSalienceDriver.TacticalRelation),
          Option.when(
            RelativeCauseClaimDepth.hasOwnedDepth(cause, graph) &&
              cause.proof.exists(proof => RelativeCauseClaimDepth.hasLineDriver(cause, proof, graph))
          )(ClaimSalienceDriver.ForcingLine),
          Option.when(
            RelativeCauseClaimDepth.hasOwnedDepth(cause, graph) &&
              cause.proof.exists(proof => RelativeCauseClaimDepth.hasTransitionDriver(cause, proof, graph))
          )(ClaimSalienceDriver.StructuralChange)
        ).flatten
      (kindDrivers ++ contextDrivers).distinct

  private def verdictFit(
      claim: JudgmentClaim,
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
