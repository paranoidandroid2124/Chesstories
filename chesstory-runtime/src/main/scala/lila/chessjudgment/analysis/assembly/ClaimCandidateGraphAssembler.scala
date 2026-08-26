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
  case SameRankKeyAggregation extends ClaimDeduplicationReason("same_rank_key_aggregation")

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
)

final case class ClaimRankingResult(
    primary: PlayerFacingPrimary,
    ranked: List[RankedClaimDecision],
    deduplicationTrace: List[ClaimDeduplicationTrace],
    causeExposureResolution: PlayerFacingCauseExposureResolution,
    causeDispositionLedger: CauseDispositionLedger,
    causeDominanceDecisions: List[RelativeCauseDominanceDecision],
    crossComparisonExposureDecisions: List[CrossComparisonExposureDecision],
    onlyMoveConstraintResolutions: List[OnlyMoveConstraintResolution]
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

object ClaimDeduplicator:

  private final case class ExactClaimSelection(
      firstIndex: Int,
      kept: ClaimAdmissionDecision,
      suppressed: List[(ClaimAdmissionDecision, Int)]
  )

  private final case class IndexedDeduplicationTrace(
      index: Int,
      originalClaimId: String,
      keptClaimId: String,
      reason: ClaimDeduplicationReason
  )

  def deduplicateDetailed(
      decisions: List[ClaimAdmissionDecision],
      graph: TypedEvidenceGraph
  ): ClaimDeduplicationResult =
    val proofCarrierIndex =
      decisions
        .flatMap(_.claim.evidence)
        .distinct
        .map(carrier => carrier -> proofCarrierKey(carrier, graph))
        .toMap
    val selections =
      decisions.zipWithIndex
        .groupBy { case (decision, _) => key(decision.claim, graph, proofCarrierIndex) }
        .values
        .map { entries =>
          val ordered = entries.toList.sortBy(_._2)
          val canonical = ordered.minBy { case (decision, index) => decision.claim.id -> index }
          ExactClaimSelection(
            firstIndex = ordered.head._2,
            kept = canonical._1,
            suppressed = ordered.filterNot(_._2 == canonical._2)
          )
        }
        .toList
    val indexedTrace =
      selections
        .flatMap(selection =>
          selection.suppressed.map { case (decision, index) =>
            IndexedDeduplicationTrace(
              index = index,
              originalClaimId = decision.claim.id,
              keptClaimId = selection.kept.claim.id,
              reason = ClaimDeduplicationReason.SemanticAggregation
            )
          }
        )
        .sortBy(item => (item.index, item.originalClaimId, item.keptClaimId))
    val trace =
      indexedTrace.zipWithIndex.map { case (item, order) =>
        ClaimDeduplicationTrace(
          order = order,
          originalClaimId = item.originalClaimId,
          keptClaimId = item.keptClaimId,
          reason = item.reason
        )
      }
    ClaimDeduplicationResult(
      decisions = selections.sortBy(_.firstIndex).map(_.kept),
      trace = trace
    )

  private final case class SemanticPositionKey(
      fen: String,
      ply: Int,
      sideToMove: Option[chess.Color],
      occurrenceId: Option[String]
  )

  private final case class SemanticLineOccurrenceKey(
      id: String,
      semantic: SemanticLineKey
  )

  private final case class ProofCarrierKey(
      carrier: EvidenceRef,
      route: List[EvidenceRef]
  )

  private final case class ClaimRankKey(
      family: ClaimFamily,
      subject: ClaimSubject,
      primaryPosition: SemanticPositionKey,
      primaryLine: Option[SemanticLineOccurrenceKey],
      subjectMove: Option[String],
      scope: EvidenceScope,
      confidence: EvidenceConfidence,
      semanticAnchors: List[EvidenceSemanticAnchor],
      proofCarriers: List[ProofCarrierKey],
      contentIdentity: Option[JudgmentClaimContent]
  )

  private def key(
      claim: JudgmentClaim,
      graph: TypedEvidenceGraph,
      proofCarrierIndex: Map[EvidenceRef, ProofCarrierKey]
  ): ClaimRankKey =
    val semanticAnchors = ClaimEvidenceSemantics.semanticAnchors(claim, graph)
    ClaimRankKey(
      family = claim.family,
      subject = claim.subject,
      primaryPosition = semanticPositionKey(claim.primaryPosition),
      primaryLine = claim.primaryLine.map(semanticLineOccurrenceKey),
      subjectMove = semanticMove(claim.subjectMove),
      scope = claim.scope,
      confidence = claim.confidence,
      semanticAnchors = semanticAnchors,
      proofCarriers = proofCarrierKeys(claim, proofCarrierIndex),
      contentIdentity = claim.content
    )

  private def semanticLineOccurrenceKey(line: LineNodeRef): SemanticLineOccurrenceKey =
    SemanticLineOccurrenceKey(line.id, SemanticLineKey.from(line))

  private def semanticPositionKey(position: PositionNodeRef): SemanticPositionKey =
    SemanticPositionKey(
      fen = position.fen.trim.split("\\s+").filter(_.nonEmpty).mkString(" "),
      ply = position.ply,
      sideToMove = position.sideToMove,
      occurrenceId = position.id
    )

  private def proofCarrierKey(
      carrier: EvidenceRef,
      graph: TypedEvidenceGraph
  ): ProofCarrierKey =
    val route = graph
      .record(carrier)
      .toList
      .flatMap(record => record :: graph.parentClosure(record))
      .map(_.ref)
      .distinctBy(_.id)
      .sortBy(_.id)
    ProofCarrierKey(carrier, route)

  private def proofCarrierKeys(
      claim: JudgmentClaim,
      index: Map[EvidenceRef, ProofCarrierKey]
  ): List[ProofCarrierKey] =
    claim.evidence.distinct.sortBy(_.id).map(index)

  private def semanticMove(move: Option[String]): Option[String] =
    move.map(EvidenceRef.normalizeMove)

object ClaimArbitrator:

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
  ): Option[ClaimRankingResult] =
    val playedMoves =
      relativeAssessments
        .map(assessment => EvidenceRef.normalizeMove(assessment.played.moveUci))
        .filter(_.nonEmpty)
        .toSet
    val deduplication =
      ClaimDeduplicator.deduplicateDetailed(graph.certified, graph.evidenceGraph)
    val claims = deduplication.decisions.map(_.claim)
    val exposure =
      PlayerFacingCauseExposurePipeline.resolve(claims, graph.evidenceGraph, playedMoves)
    val ranked = canonicalHostRows(claims, exposure, graph.evidenceGraph, playedMoves)
    val primary = relativeAssessments match
      case assessment :: Nil =>
        val ref = assessment.primaryComparisonEvidence
        graph.evidenceGraph.candidateComparisonRecord(ref).flatMap {
          case EvidenceRecord(_, CandidateComparisonEvidence(fact), _)
              if fact.kind == CandidateComparisonKind.PlayedVsBest &&
                fact.referenceLine == assessment.reference.ref &&
                fact.candidateLine == assessment.candidate.ref &&
                fact.hasDistinctRootMoves =>
            Some(PlayerFacingPrimary.MoveVerdict(ref))
          case EvidenceRecord(_, CandidateComparisonEvidence(fact), _)
              if fact.kind == CandidateComparisonKind.BestVsSecond &&
                EvidenceRef.sameMove(assessment.played.moveUci, assessment.reference.ref.rootMove) &&
                EvidenceRef.sameMove(assessment.candidate.ref.rootMove, assessment.reference.ref.rootMove) &&
                fact.referenceLine == assessment.reference.ref &&
                fact.candidateLine.role == LineNodeRole.Alternative &&
                fact.candidateLine.rank == 2 &&
                fact.hasDistinctRootMoves &&
                fact.candidateSet.nonEmpty =>
            Some(PlayerFacingPrimary.BestChoice(ref))
          case _ => None
        }
      case _ => None
    primary.map(primary => ClaimRankingResult(
      primary = primary,
      ranked = ranked,
      deduplicationTrace = deduplication.trace,
      causeExposureResolution = exposure,
      causeDispositionLedger =
        CauseDispositionPolicy.resolve(graph, deduplication, exposure),
      causeDominanceDecisions = exposure.dominanceDecisions,
      crossComparisonExposureDecisions = exposure.crossDecisions,
      onlyMoveConstraintResolutions = exposure.onlyMoveConstraintResolutions
    ))
