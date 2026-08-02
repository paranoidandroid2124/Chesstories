package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.analysis.policy.{ ClaimAdmissionDecision, ClaimTruthPolicy, ClaimAdmissionStatus }
import lila.chessjudgment.model.judgment.*

final case class ClaimCandidateGraph(
    decisions: List[ClaimAdmissionDecision],
    evidenceGraph: TypedEvidenceGraph
):
  def certified: List[ClaimAdmissionDecision] =
    decisions.filter(_.status == ClaimAdmissionStatus.Certified)

enum ClaimDeduplicationReason(val id: String):
  case SemanticAggregation extends ClaimDeduplicationReason("semantic_aggregation")
  case SameRankKeyLowerScore extends ClaimDeduplicationReason("same_rank_key_lower_score")

final case class ClaimDeduplicationTrace(
    order: Int,
    originalClaimId: String,
    keptClaimId: String,
    reason: ClaimDeduplicationReason
)

final case class ClaimDeduplicationResult(
    decisions: List[ClaimAdmissionDecision],
    trace: List[ClaimDeduplicationTrace]
)

final case class RankedClaimDecision(
    claim: JudgmentClaim,
    exposureTier: PlayerFacingClaimTier,
    playerFacingCauseSelections: List[PlayerFacingCauseSelection]
):
  def playerFacingCauseEvidenceIds: List[String] =
    playerFacingCauseSelections.map(_.causeEvidence.id)
  def onlyMoveQualifiers: List[OnlyMoveCauseQualifier] =
    playerFacingCauseSelections.flatMap(_.onlyMoveQualifier).distinct

final case class ClaimRankingResult(
    ranked: List[RankedClaimDecision],
    deduplicationTrace: List[ClaimDeduplicationTrace],
    causeExposureResolution: PlayerFacingCauseExposureResolution,
    causeDispositionLedger: CauseDispositionLedger,
    causeDominanceDecisions: List[RelativeCauseDominanceDecision],
    crossComparisonExposureDecisions: List[CrossComparisonExposureDecision],
    onlyMoveConstraintResolutions: List[OnlyMoveConstraintResolution],
    selectedContentClaimIds: List[String]
):
  def rankedClaims: List[JudgmentClaim] =
    ranked.map(_.claim)

  def playerFacingClaimDecisions: List[PlayerFacingClaimDecision] =
    ranked.map(decision =>
      PlayerFacingClaimDecision(
        claimId = decision.claim.id,
        tier = decision.exposureTier,
        causeSelections = decision.playerFacingCauseSelections
      )
    )

object ClaimCandidateGraphAssembler:

  def fromClaims(
      claims: List[JudgmentClaim],
      context: JudgmentAssemblyContext
  ): ClaimCandidateGraph =
    val graph = context.evidenceGraph
    claims.foreach(claim => comparisonForClaim(claim, graph))
    ClaimCandidateGraph(claims.map(ClaimTruthPolicy.evaluate(_, context)), graph)

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
            case EvidenceRecord(_, CandidateComparisonEvidence(fact), _) =>
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

private object RelativeCauseClaimDepth:

  def hasTacticalDepth(cause: RelativeCauseFact, graph: TypedEvidenceGraph): Boolean =
    isTactical(cause.kind) &&
      cause.hasOwnedTacticalProof(graph)

  private def isTactical(kind: RelativeCauseKind): Boolean =
    ClaimFamily.fromCause(kind).contains(ClaimFamily.Tactical)

object ClaimDeduplicator:

  private final case class AggregatedClaimDecision(
      index: Int,
      decision: ClaimAdmissionDecision,
      aggregationTrace: List[IndexedDeduplicationTrace]
  )

  private final case class RankKeySelection(
      firstIndex: Int,
      kept: AggregatedClaimDecision,
      suppressed: List[AggregatedClaimDecision]
  )

  private final case class IndexedDeduplicationTrace(
      index: Int,
      originalClaimId: String,
      keptClaimId: String,
      reason: ClaimDeduplicationReason
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
        val mergedDecision = mergeDecisions(decisionsInGroup, graph)
        AggregatedClaimDecision(
          index = entries.map(_._2._2).min,
          decision = mergedDecision,
          aggregationTrace = entries
            .map(_._2)
            .collect {
              case (decision, index) if decision.claim.id != mergedDecision.claim.id =>
                IndexedDeduplicationTrace(
                  index = index,
                  originalClaimId = decision.claim.id,
                  keptClaimId = mergedDecision.claim.id,
                  reason = ClaimDeduplicationReason.SemanticAggregation
                )
            }
        )
      }.toList
    val passthrough =
      decisions.zipWithIndex.collect { case (decision, index) if !aggregationEligible(decision.claim, graph) =>
        AggregatedClaimDecision(index, decision, Nil)
      }
    (passthrough ++ aggregated).sortBy(_.index)

  def deduplicateDetailed(
      decisions: List[ClaimAdmissionDecision],
      graph: TypedEvidenceGraph
  ): ClaimDeduplicationResult =
    val aggregated = aggregateCertified(decisions, graph)
    val selections =
      aggregated
        .groupBy(item => key(item.decision.claim, graph))
      .values
      .map { items =>
        val kept = items.maxBy(item => (score(item.decision, graph), -item.index))
        RankKeySelection(
          firstIndex = items.map(_.index).min,
          kept = kept,
          suppressed = items.filterNot(_.index == kept.index).sortBy(_.index)
        )
      }
      .toList
    val aggregationTrace =
      aggregated
        .flatMap(_.aggregationTrace)
        .sortBy(item => (item.index, item.originalClaimId, item.keptClaimId))
    val suppressionTrace =
      selections
        .sortBy(_.firstIndex)
        .flatMap(selection =>
          selection.suppressed.map(item =>
            IndexedDeduplicationTrace(
              index = item.index,
              originalClaimId = item.decision.claim.id,
              keptClaimId = selection.kept.decision.claim.id,
              reason = ClaimDeduplicationReason.SameRankKeyLowerScore
            )
          )
        )
    val trace =
      (aggregationTrace ++ suppressionTrace).zipWithIndex.map { case (item, order) =>
        ClaimDeduplicationTrace(
          order = order,
          originalClaimId = item.originalClaimId,
          keptClaimId = item.keptClaimId,
          reason = item.reason
        )
      }
    ClaimDeduplicationResult(
      decisions = selections.sortBy(_.firstIndex).map(_.kept.decision),
      trace = trace
    )

  private final case class SemanticPositionKey(
      fen: String,
      ply: Int,
      sideToMove: Option[chess.Color]
  )

  private enum RankEvidenceKey:
    case CauseKeys(values: List[String])
    case EvidenceAnchors(values: List[String])
    case Unique(claimId: String)

  private final case class ClaimRankKey(
      family: ClaimFamily,
      subject: ClaimSubject,
      primaryPosition: SemanticPositionKey,
      primaryLine: Option[SemanticLineKey],
      subjectMove: Option[String],
      evidence: RankEvidenceKey,
      contentIdentity: Option[JudgmentClaimContent]
  )

  private def key(
      claim: JudgmentClaim,
      graph: TypedEvidenceGraph
  ): ClaimRankKey =
    val causeKeys = claim.evidence
      .flatMap(ref => graph.byId.get(ref.id))
      .collect { case EvidenceRecord(ref, RelativeCauseFactEvidence(cause), _) =>
        relativeCauseRankSignature(cause, ref.id, graph)
      }
      .distinct
      .sorted
    val anchorKeys = ClaimEvidenceSemantics
      .semanticAnchors(claim, graph)
      .map(_.stableKey)
      .distinct
      .sorted
    val evidenceKey =
      if causeKeys.nonEmpty then RankEvidenceKey.CauseKeys(causeKeys)
      else if anchorKeys.nonEmpty then RankEvidenceKey.EvidenceAnchors(anchorKeys)
      else RankEvidenceKey.Unique(claim.id)
    ClaimRankKey(
      family = claim.family,
      subject = claim.subject,
      primaryPosition = semanticPositionKey(claim.primaryPosition),
      primaryLine = claim.primaryLine.map(semanticLineKey),
      subjectMove = semanticMove(claim.subjectMove),
      evidence = evidenceKey,
      contentIdentity = claim.content
    )

  private def relativeCauseRankSignature(
      cause: RelativeCauseFact,
      causeEvidenceId: String,
      graph: TypedEvidenceGraph
  ): String =
    RelativeCauseSemanticKey.from(cause, causeEvidenceId, graph).stableKey

  private def semanticLineKey(line: LineNodeRef): SemanticLineKey =
    SemanticLineKey.from(line)

  private def semanticPositionKey(position: PositionNodeRef): SemanticPositionKey =
    SemanticPositionKey(
      fen = position.fen.trim.split("\\s+").filter(_.nonEmpty).mkString(" "),
      ply = position.ply,
      sideToMove = position.sideToMove
    )

  private def semanticMove(move: Option[String]): Option[String] =
    move.map(EvidenceRef.normalizeMove)

  private final case class AggregationKey(
      family: ClaimFamily,
      subject: ClaimSubject,
      primaryPosition: SemanticPositionKey,
      primaryLine: Option[SemanticLineKey],
      subjectMove: Option[String],
      scope: Option[EvidenceScope],
      semanticAnchors: List[EvidenceSemanticAnchor]
  )

  private def aggregationEligible(
      claim: JudgmentClaim,
      graph: TypedEvidenceGraph
  ): Boolean =
    claim.family.isLongTerm &&
      claim.content.isEmpty &&
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
      primaryPosition = semanticPositionKey(claim.primaryPosition),
      primaryLine = claim.primaryLine.map(semanticLineKey),
      subjectMove = semanticMove(claim.subjectMove),
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
    val mergedMissingEvidence = list.flatMap(_.missingEvidence).distinct
    list.maxBy(decision => score(decision, graph)).copy(
      claim = mergedClaim,
      presentLayers = mergedPresentLayers,
      missingLayerGroups = mergedMissingLayerGroups,
      missingEvidence = mergedMissingEvidence
    )

  private def mergeClaims(claims: Iterable[JudgmentClaim], graph: TypedEvidenceGraph): JudgmentClaim =
    val claimList = claims.toList
    val mergeBase = claimList.maxBy(claim => score(claim, graph))
    if claimList.size == 1 then mergeBase
    else
      mergeBase.copy(
        evidence = claimList.flatMap(_.evidence).distinctBy(_.id),
        confidence = claimList.map(_.confidence).maxBy(confidenceScore)
      )

  private def score(claim: JudgmentClaim, graph: TypedEvidenceGraph): Int =
    claimEvidencePriority(claim, graph) * 10 +
      confidenceScore(claim.confidence)

  private def score(decision: ClaimAdmissionDecision, graph: TypedEvidenceGraph): Int =
    score(decision.claim, graph)

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

  /** Content selection is intentionally separate from Cause exposure and host
    * tiering. Ja has already certified the exact F binding; R emits one claim
    * id per semantic identity in deterministic serialization order.
    */
  private[assembly] def contentClaimIds(
    decisions: List[ClaimAdmissionDecision]
  ): List[String] =
    val eligible = decisions.flatMap {
      case ClaimAdmissionDecision(claim, ClaimAdmissionStatus.Certified, _, _, _) =>
        claim.content.map(claim -> _)
      case _ => None
    }
    eligible
      .groupBy { case (_, content) => selectionIdentity(content) }
      .values
      .collect { case entry :: Nil => entry }
      .toList
      .sortBy { case (claim, content) => (contentOrderKey(content), claim.id) }
      .map(_._1.id)

  private def selectionIdentity(
      content: JudgmentClaimContent
  ): Either[CandidateComparisonSemanticKey, EvidenceRef] =
    content match
      case JudgmentClaimContent.CandidateComparison(_, comparison) => Left(comparison)
      case JudgmentClaimContent.StrategicMechanism(carrier) => Right(carrier)

  private def contentOrderKey(content: JudgmentClaimContent): (Int, String) =
    content match
      case JudgmentClaimContent.CandidateComparison(_, comparison) => 0 -> comparison.stableKey
      case JudgmentClaimContent.StrategicMechanism(carrier) => 1 -> carrier.id

  private[assembly] def canonicalHostRows(
      claims: List[JudgmentClaim],
      exposure: PlayerFacingCauseExposureResolution,
      evidenceGraph: TypedEvidenceGraph,
      playedMoves: Set[String]
  ): List[RankedClaimDecision] =
    claims
      .map { claim =>
        RankedClaimDecision(
          claim = claim,
          exposureTier =
            PlayerFacingClaimDecision.tierFor(claim, exposure, evidenceGraph, playedMoves),
          playerFacingCauseSelections =
            exposure.selectionsForClaim(claim.id).sortBy(_.selectionOrder)
        )
      }
      .sortBy(decision =>
        (-PlayerFacingClaimPolicy.rankPriority(decision.exposureTier), decision.claim.id)
      )

  def rankDetailed(
      graph: ClaimCandidateGraph,
      relativeAssessments: List[RelativeMoveAssessment]
  ): ClaimRankingResult =
    val playedMoves =
      relativeAssessments
        .map(assessment => JudgmentSubjectBinding.normalizeMove(assessment.played.moveUci))
        .filter(_.nonEmpty)
        .toSet
    val deduplication =
      ClaimDeduplicator.deduplicateDetailed(graph.certified, graph.evidenceGraph)
    val claims = deduplication.decisions.map(_.claim)
    val exposure =
      PlayerFacingCauseExposurePipeline.resolve(claims, graph.evidenceGraph, playedMoves)
    ClaimRankingResult(
      ranked = canonicalHostRows(claims, exposure, graph.evidenceGraph, playedMoves),
      deduplicationTrace = deduplication.trace,
      causeExposureResolution = exposure,
      causeDispositionLedger =
        CauseDispositionPolicy.resolve(graph, deduplication, exposure),
      causeDominanceDecisions = exposure.dominanceDecisions,
      crossComparisonExposureDecisions = exposure.crossDecisions,
      onlyMoveConstraintResolutions = exposure.onlyMoveConstraintResolutions,
      selectedContentClaimIds = contentClaimIds(deduplication.decisions)
    )
