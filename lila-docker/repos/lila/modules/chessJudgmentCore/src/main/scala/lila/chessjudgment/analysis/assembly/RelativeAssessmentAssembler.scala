package lila.chessjudgment.analysis.assembly

import chess.Color
import lila.chessjudgment.analysis.evaluation.{ JudgmentThresholds, PerspectiveMath, VerdictThresholdPolicy }
import lila.chessjudgment.analysis.transition.TransitionFactNormalizer
import lila.chessjudgment.model.judgment.*

final case class RelativeAssessmentAssembly(
    input: NormalizedMoveReviewInput,
    context: JudgmentAssemblyContext
)

object RelativeAssessmentAssembler:

  private final case class StrategicAxisEntry(
      axis: StrategicAxisDetail,
      strength: Int,
      sources: List[EvidenceRef]
  )

  private final case class ComparisonEvidenceNeighborhood(
      referenceEndpoint: List[EvidenceRecord],
      candidateEndpoint: List[EvidenceRecord],
      rootContext: List[EvidenceRecord],
      referenceTransition: List[EvidenceRecord],
      candidateTransition: List[EvidenceRecord],
      referenceAfterPosition: List[EvidenceRecord],
      candidateAfterPosition: List[EvidenceRecord],
      parents: List[EvidenceRecord]
  ):
    val referenceRecords: List[EvidenceRecord] =
      (referenceEndpoint ++ referenceTransition ++ referenceAfterPosition).distinctBy(_.ref.id)
    val candidateRecords: List[EvidenceRecord] =
      (candidateEndpoint ++ candidateTransition ++ candidateAfterPosition).distinctBy(_.ref.id)
    val sharedRecords: List[EvidenceRecord] =
      (rootContext ++ parents).distinctBy(_.ref.id)
    val involvedRecords: List[EvidenceRecord] =
      (referenceRecords ++ candidateRecords ++ sharedRecords).distinctBy(_.ref.id)

  private final case class RelativeCauseProofRecords(
      directProof: List[EvidenceRecord],
      contrastProof: List[EvidenceRecord],
      contextSupport: List[EvidenceRecord]
  ):
    def all: List[EvidenceRecord] =
      (directProof ++ contrastProof ++ contextSupport).distinctBy(_.ref.id)

  def assemble(raw: RawMoveReviewInput): Option[RelativeAssessmentAssembly] =
    EvidenceFactAssembler.assemble(raw).map(enrich)

  def enrich(assembly: EvidenceFactAssembly): RelativeAssessmentAssembly =
    val input = assembly.input
    val context = assembly.context
    val relativeAssembly =
      for
        played <- context.playedTransition
        referenceTransition <- context.referenceTransition
        reference <- context.line(LineNodeRole.BestReference)
        candidate <- context.line(LineNodeRole.Played)
        root <- context.position(PositionNodeRole.Before).map(_.ref)
        mover <- root.sideToMove.orElse(input.sideToMove)
      yield {
      val allocator = JudgmentProvenanceAllocator.forInput(input)
      val comparison = compare(mover, reference, candidate, candidateSetComparison(mover, context.lines, reference))
      val primaryConfidence = comparisonConfidence(reference, candidate)
      val comparisonRecords =
        candidateComparisonRecords(
          mover = mover,
          context = context,
          reference = reference,
          candidate = candidate,
          primaryComparison = comparison,
          root = root,
          allocator = allocator
        )
      val strategicContrastRecords =
        comparisonRecords.flatMap(record => strategicMechanismContrastRecords(context, root, allocator, record))
      val causeContext = context.withEvidence(strategicContrastRecords)
      val counterfactual =
        TransitionFactNormalizer.fromCounterfactual(
          id = allocator.evidenceId(s"counterfactual:played-vs-reference:${candidate.ref.rootMove}"),
          referenceLine = reference.ref,
          candidateLine = candidate.ref,
          comparison = comparison,
          position = root,
          scope = EvidenceScope.Counterfactual,
          confidence = primaryConfidence,
          parents = parentsForLine(context, reference) ++ parentsForLine(context, candidate) ++ List(played.evidence)
        )
      val causeRecords =
        comparisonRecords.flatMap(record => relativeCauseRecords(causeContext, root, allocator, record))
      val playedMoveSet = Set(JudgmentSubjectBinding.normalizeMove(played.moveUci))
      val playedCauseRecords =
        causeRecords.filter(record =>
          JudgmentSubjectBinding.primaryPlayed(JudgmentSubjectBinding.recordBinding(record, playedMoveSet))
        )
      val playedVerdictCauses =
        playedCauseRecords.collect {
          case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) => cause
        }
      val certification =
        MoveVerdictCertification(
          playedMove = played.moveUci,
          verdict = comparison.verdict,
          primaryComparison = CandidateComparisonFact(
            kind = CandidateComparisonKind.PlayedVsBest,
            referenceLine = reference.ref,
            candidateLine = candidate.ref,
            comparison = comparison
          ),
          causes = playedVerdictCauses
        )
      val certificationRecord =
        TransitionFactNormalizer.fromMoveVerdictCertification(
          id = allocator.evidenceId(s"move-verdict:${candidate.ref.rootMove}"),
          certification = certification,
          position = root,
          scope = EvidenceScope.Counterfactual,
          confidence = primaryConfidence,
          parents = (comparisonRecords
            .filter(record =>
              JudgmentSubjectBinding.primaryPlayed(JudgmentSubjectBinding.recordBinding(record, playedMoveSet))
            )
            .map(_.ref) ++
            playedCauseRecords.map(_.ref) :+ counterfactual.ref).distinctBy(_.id)
        )
      val relativeEvidence =
        allocator.evidenceRef(
          suffix = s"relative-assessment:${candidate.ref.rootMove}",
          producer = EvidenceProducer.RelativeMoveProducer,
          layer = EvidenceLayer.RelativeAssessment,
          position = root,
          line = Some(candidate.ref),
          scope = EvidenceScope.Counterfactual,
          confidence = primaryConfidence
        )
      val assessment =
        RelativeMoveAssessmentBuilder.fromComparison(
          played = played,
          referenceTransition = Some(referenceTransition),
          reference = reference,
          candidate = candidate,
          comparison = comparison,
          collapse = None,
          confidence = primaryConfidence,
          evidence = relativeEvidence,
          counterfactualEvidence = List(counterfactual.ref),
          candidateComparisonEvidence = comparisonRecords.map(_.ref),
          relativeCauseEvidence = playedCauseRecords.map(_.ref),
          verdictCertificationEvidence = Some(certificationRecord.ref)
        )
      val assessmentRecord = TransitionFactNormalizer.fromRelativeAssessment(assessment)
      RelativeAssessmentAssembly(
        input = input,
        context = context
          .withEvidence(comparisonRecords ++ strategicContrastRecords ++ List(counterfactual) ++ causeRecords ++ List(certificationRecord, assessmentRecord))
          .withRelativeAssessment(assessment)
      )
      }
    relativeAssembly.getOrElse(RelativeAssessmentAssembly(input = input, context = context))

  private def compare(
      mover: Color,
      reference: CandidateLineNode,
      candidate: CandidateLineNode,
      candidateSet: Option[CandidateSetComparison]
  ): EvalComparison =
    val delta =
      PerspectiveMath.compareForMover(
        mover = mover,
        reference = PerspectiveMath.EvalPoint(reference.whitePovEvalCp, reference.mate),
        candidate = PerspectiveMath.EvalPoint(candidate.whitePovEvalCp, candidate.mate)
      )
    EvalComparison(
      mover = mover,
      referenceLine = reference.ref,
      candidateLine = candidate.ref,
      rawCandidateDeltaCpForDiagnostics = delta.rawCandidateDeltaCpForMover,
      candidateWinPercentDeltaForMover = delta.candidateWinPercentDeltaForMover,
      rawCpLossForDiagnostics = delta.rawCpLossForMover,
      winPercentLossForMover = delta.winPercentLossForMover,
      verdict = VerdictThresholdPolicy.verdictFromWinPercent(
        delta.candidateWinPercentDeltaForMover,
        delta.winPercentLossForMover
      ),
      candidateSet = candidateSet
    )

  private def candidateSetComparison(
      mover: Color,
      lines: List[CandidateLineNode],
      reference: CandidateLineNode
  ): Option[CandidateSetComparison] =
    val ordered =
      lines
        .filterNot(_.role == LineNodeRole.Threat)
        .sortBy(_.ref.rank)
        .groupBy(_.ref.rootMove)
        .values
        .map(_.minBy(_.ref.rank))
        .toList
        .sortBy(_.ref.rank)
    val second = ordered.find(_.ref.rootMove != reference.ref.rootMove)
    val gap =
      second.map(line =>
        PerspectiveMath.compareForMover(
          mover = mover,
          reference = PerspectiveMath.EvalPoint(reference.whitePovEvalCp, reference.mate),
          candidate = PerspectiveMath.EvalPoint(line.whitePovEvalCp, line.mate)
        )
      )
    Option.when(ordered.nonEmpty)(
      CandidateSetComparison(
        secondLine = second.map(_.ref),
        rawBestToSecondCpGapForDiagnostics = gap.map(_.rawCpLossForMover),
        bestToSecondWinPercentGapForMover = gap.map(_.winPercentLossForMover),
        candidateCount = ordered.size,
        onlyMove = gap.exists(_.winPercentLossForMover >= JudgmentThresholds.ONLY_MOVE_GAP_WP)
      )
    )
  private def candidateComparisonRecords(
      mover: Color,
      context: JudgmentAssemblyContext,
      reference: CandidateLineNode,
      candidate: CandidateLineNode,
      primaryComparison: EvalComparison,
      root: PositionNodeRef,
      allocator: JudgmentProvenanceAllocator
  ): List[EvidenceRecord] =
    val ordered = uniqueLines(context.lines)
    val second = ordered.find(_.ref.rootMove != reference.ref.rootMove)
    val alternatives =
      ordered.filter(line =>
        line.role == LineNodeRole.Alternative &&
          line.ref.rootMove != candidate.ref.rootMove &&
          line.ref.rootMove != reference.ref.rootMove
      )
    val comparisons =
      List(
        Some((CandidateComparisonKind.PlayedVsBest, reference, candidate, primaryComparison)),
        second.map(line =>
          (
            CandidateComparisonKind.BestVsSecond,
            reference,
            line,
            compare(mover, reference, line, None)
          )
        )
      ).flatten ++
        alternatives.flatMap { alternative =>
          List(
            (
              CandidateComparisonKind.PlayedVsAlternative,
              alternative,
              candidate,
              compare(mover, alternative, candidate, None)
            ),
            (
              CandidateComparisonKind.ReferenceVsAlternative,
              reference,
              alternative,
              compare(mover, reference, alternative, None)
            )
          )
        }
    comparisons
      .distinctBy { case (kind, ref, cand, _) => (kind, ref.ref.id, cand.ref.id) }
      .zipWithIndex
      .map { case ((kind, refLine, candLine, cmp), index) =>
        TransitionFactNormalizer.fromCandidateComparison(
          id = allocator.evidenceId(
            s"candidate-comparison:${allocator.key(kind)}:${allocator.key(refLine.ref.rootMove)}:${allocator.key(candLine.ref.rootMove)}:$index"
          ),
          comparison = CandidateComparisonFact(
            kind = kind,
            referenceLine = refLine.ref,
            candidateLine = candLine.ref,
            comparison = cmp
          ),
          position = root,
          scope = EvidenceScope.Counterfactual,
          confidence = comparisonConfidence(refLine, candLine),
          parents = (parentsForLine(context, refLine) ++ parentsForLine(context, candLine)).distinctBy(_.id)
        )
      }

  private def strategicMechanismContrastRecords(
      context: JudgmentAssemblyContext,
      root: PositionNodeRef,
      allocator: JudgmentProvenanceAllocator,
      comparisonRecord: EvidenceRecord
  ): List[EvidenceRecord] =
    comparisonRecord.payload match
      case CandidateComparisonEvidence(fact) =>
        val neighborhood = comparisonNeighborhood(context, root, fact)
        val referenceProfile = strategicAxisProfile(neighborhood.referenceRecords)
        val candidateProfile = strategicAxisProfile(neighborhood.candidateRecords)
        val axisComparisons = strategicAxisComparisons(referenceProfile, candidateProfile)
        val planComparison = strategicPlanComparison(axisComparisons)
        val sustainability =
          strategicSustainability(
            context = context,
            fact = fact,
            axisComparisons = axisComparisons
          )
        val support =
          strategicContrastSupport(axisComparisons, neighborhood.sharedRecords)
        Option
          .when(axisComparisons.exists(_.hasContrast) || planComparison.exists(_.hasPlanDelta)) {
            EvidenceRecord(
              ref = EvidenceRef(
                id = allocator.evidenceId(
                  s"strategic-contrast:${allocator.key(fact.kind)}:${allocator.key(fact.referenceLine.rootMove)}:${allocator.key(fact.candidateLine.rootMove)}"
                ),
                producer = EvidenceProducer.StrategicMechanismProducer,
                layer = EvidenceLayer.StrategicMechanism,
                position = root,
                line = Some(fact.candidateLine),
                scope = EvidenceScope.Counterfactual,
                confidence = comparisonConfidence(context, fact)
              ),
              payload = StrategicMechanismContrastEvidence(
                comparisonKind = fact.kind,
                referenceLine = fact.referenceLine,
                candidateLine = fact.candidateLine,
                axisComparisons = axisComparisons,
                planComparison = planComparison,
                sustainability = sustainability,
                support = support
              ),
              parents = (comparisonRecord.ref :: support.all).distinctBy(_.id)
            )
          }
          .toList
      case _ =>
        Nil

  private def strategicAxisProfile(records: List[EvidenceRecord]): Map[String, StrategicAxisEntry] =
    records
      .collect { case EvidenceRecord(ref, payload: StrategicMechanismEvidence, _) =>
        payload.signals.flatMap(signal =>
          signal.axis.map(axis =>
            axis.stableKey -> StrategicAxisEntry(
              axis = axis,
              strength = signal.strength.max(1),
              sources = List(ref, signal.source).distinctBy(_.id)
            )
          )
        )
      }
      .flatten
      .groupBy(_._1)
      .view
      .mapValues(entries =>
        val values = entries.map(_._2)
        val first = values.head
        StrategicAxisEntry(
          axis = first.axis,
          strength = values.map(_.strength).sum,
          sources = values.flatMap(_.sources).distinctBy(_.id)
        )
      )
      .toMap

  private def strategicAxisComparisons(
      referenceProfile: Map[String, StrategicAxisEntry],
      candidateProfile: Map[String, StrategicAxisEntry]
  ): List[StrategicAxisComparison] =
    (referenceProfile.keySet ++ candidateProfile.keySet).toList.sorted.map { key =>
      val reference = referenceProfile.get(key)
      val candidate = candidateProfile.get(key)
      val axis = reference.orElse(candidate).map(_.axis).get
      val referenceStrength = reference.map(_.strength).getOrElse(0)
      val candidateStrength = candidate.map(_.strength).getOrElse(0)
      StrategicAxisComparison(
        axis = axis,
        outcome = strategicAxisComparisonOutcome(axis, referenceStrength, candidateStrength),
        referenceStrength = referenceStrength,
        candidateStrength = candidateStrength,
        referenceSources = reference.map(_.sources).getOrElse(Nil),
        candidateSources = candidate.map(_.sources).getOrElse(Nil)
      )
    }

  private def strategicAxisComparisonOutcome(
      axis: StrategicAxisDetail,
      referenceStrength: Int,
      candidateStrength: Int
  ): StrategicAxisComparisonOutcome =
    val candidateNegative =
      axis.polarity == StrategicAxisPolarity.Loss ||
        axis.polarity == StrategicAxisPolarity.Release ||
        axis.polarity == StrategicAxisPolarity.Concede
    if referenceStrength > 0 && candidateStrength == 0 then
      if axis.kind == StrategicAxisKind.PlanCoherence && axis.polarity == StrategicAxisPolarity.Preserve then
        StrategicAxisComparisonOutcome.ReferencePreservesPlan
      else StrategicAxisComparisonOutcome.ReferenceOnly
    else if candidateStrength > 0 && referenceStrength == 0 then
      if candidateNegative then StrategicAxisComparisonOutcome.CandidateConcession
      else StrategicAxisComparisonOutcome.CandidateOnly
    else if referenceStrength > candidateStrength then
      if axis.kind == StrategicAxisKind.PlanCoherence && axis.polarity == StrategicAxisPolarity.Preserve then
        StrategicAxisComparisonOutcome.ReferencePreservesPlan
      else StrategicAxisComparisonOutcome.ReferenceStronger
    else if candidateStrength > referenceStrength then
      if candidateNegative then StrategicAxisComparisonOutcome.CandidateConcession
      else StrategicAxisComparisonOutcome.CandidateStronger
    else StrategicAxisComparisonOutcome.SharedSustained

  private def strategicPlanComparison(
      axisComparisons: List[StrategicAxisComparison]
  ): Option[StrategicPlanComparison] =
    val planAxis = axisComparisons.filter(_.axis.kind == StrategicAxisKind.PlanCoherence)
    Option.when(planAxis.nonEmpty) {
      val referencePlans =
        planAxis.filter(_.referenceStrength > 0).map(_.axis.label).distinct.sorted
      val candidatePlans =
        planAxis.filter(_.candidateStrength > 0).map(_.axis.label).distinct.sorted
      val outcome =
        if referencePlans.nonEmpty && candidatePlans.isEmpty then StrategicAxisComparisonOutcome.ReferenceOnly
        else if candidatePlans.nonEmpty && referencePlans.isEmpty then StrategicAxisComparisonOutcome.CandidateOnly
        else if referencePlans == candidatePlans then StrategicAxisComparisonOutcome.SharedSustained
        else if referencePlans.nonEmpty then StrategicAxisComparisonOutcome.ReferencePreservesPlan
        else StrategicAxisComparisonOutcome.CandidateStronger
      StrategicPlanComparison(referencePlans, candidatePlans, outcome)
    }

  private def strategicSustainability(
      context: JudgmentAssemblyContext,
      fact: CandidateComparisonFact,
      axisComparisons: List[StrategicAxisComparison]
  ): StrategicSustainabilityAssessment =
    val referencePlyCount = context.lines.find(_.ref == fact.referenceLine).map(_.line.moves.size).getOrElse(0)
    val candidatePlyCount = context.lines.find(_.ref == fact.candidateLine).map(_.line.moves.size).getOrElse(0)
    val horizonPlyCount = math.min(referencePlyCount, candidatePlyCount)
    val horizon =
      if horizonPlyCount >= 9 then StrategicSustainabilityHorizon.LongPv
      else if horizonPlyCount >= 5 then StrategicSustainabilityHorizon.MediumPv
      else if horizonPlyCount >= 2 then StrategicSustainabilityHorizon.ShortPv
      else if horizonPlyCount >= 1 then StrategicSustainabilityHorizon.Immediate
      else StrategicSustainabilityHorizon.Unknown
    StrategicSustainabilityAssessment(
      horizon = horizon,
      lineMaintained = axisComparisons.nonEmpty,
      pvMaintained = horizon != StrategicSustainabilityHorizon.Unknown && horizon != StrategicSustainabilityHorizon.Immediate,
      referencePlyCount = referencePlyCount,
      candidatePlyCount = candidatePlyCount
    )

  private def strategicContrastSupport(
      axisComparisons: List[StrategicAxisComparison],
      sharedRecords: List[EvidenceRecord]
  ): StrategicContrastSupport =
    val directSources =
      axisComparisons.flatMap { comparison =>
        if comparison.candidateLead then comparison.candidateSources
        else if comparison.referenceLead then comparison.referenceSources
        else comparison.sources
      }.distinctBy(_.id)
    val contrastSources =
      axisComparisons.flatMap { comparison =>
        if comparison.candidateLead then comparison.referenceSources
        else if comparison.referenceLead then comparison.candidateSources
        else Nil
      }.distinctBy(_.id)
    val contextSources =
      sharedRecords
        .collect { case EvidenceRecord(ref, payload: StrategicMechanismEvidence, _) if payload.hasStrategicAxis => ref }
        .distinctBy(_.id)
    StrategicContrastSupport(
      directSources = directSources,
      contrastSources = contrastSources,
      contextSources = contextSources
    )

  private def comparisonConfidence(reference: CandidateLineNode, candidate: CandidateLineNode): EvidenceConfidence =
    if lineHasEngineDepth(reference) && lineHasEngineDepth(candidate) then EvidenceConfidence.EngineBacked
    else EvidenceConfidence.Mixed

  private def comparisonConfidence(context: JudgmentAssemblyContext, fact: CandidateComparisonFact): EvidenceConfidence =
    val reference = context.lines.find(_.ref == fact.referenceLine)
    val candidate = context.lines.find(_.ref == fact.candidateLine)
    reference.zip(candidate)
      .map { case (referenceLine, candidateLine) => comparisonConfidence(referenceLine, candidateLine) }
      .getOrElse(EvidenceConfidence.Mixed)

  private def lineHasEngineDepth(line: CandidateLineNode): Boolean =
    JudgmentThresholds.engineBackedByDepth(line.depth, line.mate)

  private def uniqueLines(lines: List[CandidateLineNode]): List[CandidateLineNode] =
    lines
      .filterNot(_.role == LineNodeRole.Threat)
      .sortBy(_.ref.rank)
      .groupBy(_.ref.rootMove)
      .values
      .map(_.minBy(_.ref.rank))
      .toList
      .sortBy(_.ref.rank)

  private def relativeCauseRecords(
      context: JudgmentAssemblyContext,
      root: PositionNodeRef,
      allocator: JudgmentProvenanceAllocator,
      comparisonRecord: EvidenceRecord
  ): List[EvidenceRecord] =
    comparisonRecord.payload match
      case CandidateComparisonEvidence(fact) =>
        val neighborhood = comparisonNeighborhood(context, root, fact)
        val comparisonProof = comparisonProofRecords(neighborhood)
        val causes = mergeCauseCandidates(inferCauseCandidates(neighborhood, fact))
        causes.zipWithIndex.map { case (candidate, index) =>
          val kind = candidate.kind
          val support = candidate.support
          val supportRefs = support.map(_.ref).distinctBy(_.id)
          val binding =
            RelativeCauseFact.binding(
              kind = kind,
              comparisonKind = fact.kind,
              referenceLine = fact.referenceLine,
              candidateLine = fact.candidateLine,
              supportEvidence = supportRefs,
              explicitSourceSide = candidate.sourceSide
            )
          val rawProofRecords =
            relativeCauseProofRecords(context.evidenceGraph, fact, kind, binding, support, comparisonProof, neighborhood)
          val attribution =
            causeAttribution(candidate, binding, rawProofRecords, supportRefs)
          val proofRecords =
            if attribution.directProofEligible then rawProofRecords
            else
              rawProofRecords.copy(
                directProof = Nil,
                contextSupport = (rawProofRecords.directProof ++ rawProofRecords.contextSupport).distinctBy(_.ref.id)
              )
          val proof = relativeCauseProof(
            context.evidenceGraph,
            kind,
            binding.sourceSide,
            proofRecords.directProof,
            proofRecords.contrastProof,
            proofRecords.contextSupport
          )
          val retainedProof =
            Some(proof).filter(proof => (attribution.directProofEligible && proof.hasRawTypedDepth) || proof.hasRawContextSupport)
          val cause =
            RelativeCauseFact(
              kind = kind,
              comparisonKind = fact.kind,
              referenceLine = fact.referenceLine,
              candidateLine = fact.candidateLine,
              verdict = fact.comparison.verdict,
              winPercentLossForMover = fact.comparison.winPercentLossForMover,
              candidateWinPercentDeltaForMover = fact.comparison.candidateWinPercentDeltaForMover,
              supportEvidence = supportRefs,
              evidenceLines = binding.evidenceLines,
              role = binding.role,
              eventLine = binding.eventLine,
              sourceSide = binding.sourceSide,
              importance = binding.importance,
              attribution = attribution
            )(proof = retainedProof)
          TransitionFactNormalizer.fromRelativeCause(
            id = allocator.evidenceId(
              s"relative-cause:${allocator.key(fact.kind)}:${allocator.key(kind)}:${allocator.key(fact.referenceLine.rootMove)}:${allocator.key(fact.candidateLine.rootMove)}:$index"
            ),
            cause = cause,
            position = root,
            scope = EvidenceScope.Counterfactual,
            confidence = comparisonConfidence(context, fact),
            parents = (comparisonRecord.ref :: proofRecords.all.map(_.ref)).distinctBy(_.id)
          )
        }
      case _ => Nil

  private def relativeCauseProofRecords(
      graph: TypedEvidenceGraph,
      fact: CandidateComparisonFact,
      kind: RelativeCauseKind,
      binding: RelativeCauseBinding,
      support: List[EvidenceRecord],
      comparisonProof: List[EvidenceRecord],
      neighborhood: ComparisonEvidenceNeighborhood
  ): RelativeCauseProofRecords =
    val directSeed =
      support.filter(record => directProofSource(graph, fact, kind, binding, record)).distinctBy(_.ref.id)
    val directRecords =
      proofSectionRecords(graph, directSeed, Some(kind), Some(binding))
        .filter(record => directProofSource(graph, fact, kind, binding, record))
        .distinctBy(_.ref.id)
    val supportIds = supportClosureRecordIds(graph, support)
    val directIds = directRecords.map(_.ref.id).toSet
    val contrastRecords =
      comparisonProof
        .filterNot(record => directIds.contains(record.ref.id) || supportIds.contains(record.ref.id))
        .distinctBy(_.ref.id)
    val contrastIds = proofSectionRecords(graph, contrastRecords, Some(kind), Some(binding)).map(_.ref.id).toSet
    val proofIds = directIds ++ contrastIds
    val contextSupportCandidates =
      support.filterNot(record => directIds.contains(record.ref.id))
    val supportContextRecords =
      contextSupportCandidates
        .filterNot(record => proofSectionRecordIds(graph, List(record)).exists(proofIds.contains))
    val sharedContextRecords =
      neighborhood.sharedRecords
        .filter(record => contextSupportRecord(fact, record))
        .filterNot(record => proofSectionRecordIds(graph, List(record)).exists(proofIds.contains))
    val contextRecords =
      (supportContextRecords ++ sharedContextRecords)
        .distinctBy(_.ref.id)
    RelativeCauseProofRecords(
      directProof = directRecords,
      contrastProof = contrastRecords,
      contextSupport = contextRecords
    )

  private def causeAttribution(
      draft: RelativeCauseDraft,
      binding: RelativeCauseBinding,
      proofRecords: RelativeCauseProofRecords,
      supportRefs: List[EvidenceRef]
  ): CauseAttribution =
    val ownedRefs = proofRecords.directProof.map(_.ref).distinctBy(_.id)
    val contrastRefs = proofRecords.contrastProof.map(_.ref).distinctBy(_.id)
    val rootMatched =
      ownedRefs.nonEmpty &&
        proofRecords.directProof.exists(record => recordMatchesEventRoot(record, binding.eventLine.rootMove))
    val attributionKind = effectiveAttributionKind(draft, binding)
    val directEligible =
      ownedRefs.nonEmpty &&
        rootMatched &&
        attributionKind != CauseAttributionKind.SharedContext &&
        attributionKind != CauseAttributionKind.ContextOnly &&
        attributionKind != CauseAttributionKind.Unattributed
    val finalKind =
      if directEligible then attributionKind
      else if attributionKind == CauseAttributionKind.Unattributed then CauseAttributionKind.Unattributed
      else CauseAttributionKind.ContextOnly
    val attributedOwnedRefs = if directEligible then ownedRefs else Nil
    val contextRefs =
      (
        supportRefs.filterNot(ref => attributedOwnedRefs.exists(_.id == ref.id) || contrastRefs.exists(_.id == ref.id)) ++
          proofRecords.contextSupport.map(_.ref) ++
          Option.when(!directEligible)(ownedRefs).toList.flatten
      ).distinctBy(_.id)
    CauseAttribution(
      kind = finalKind,
      ownedEvidence = attributedOwnedRefs,
      contrastEvidence = contrastRefs,
      contextEvidence = contextRefs,
      rootMoveMatched = rootMatched,
      directProofEligible = directEligible,
      reason =
        Option.when(!directEligible)(
          if ownedRefs.isEmpty then "no-owned-direct-proof"
          else if !rootMatched then "root-mismatch"
          else "context-only-attribution"
        )
    )

  private def effectiveAttributionKind(
      draft: RelativeCauseDraft,
      binding: RelativeCauseBinding
  ): CauseAttributionKind =
    if draft.attributionKind != CauseAttributionKind.Unattributed then draft.attributionKind
    else
      binding.sourceSide match
        case RelativeCauseSourceSide.Reference =>
          CauseAttributionKind.ReferenceCreatesResource
        case RelativeCauseSourceSide.Candidate =>
          draft.kind match
            case RelativeCauseKind.RecaptureRecoveryWindow | RelativeCauseKind.ConversionSecured |
                RelativeCauseKind.SacrificeCompensation | RelativeCauseKind.StructuralImprovement |
                RelativeCauseKind.TargetPressureGain | RelativeCauseKind.CenterControlGain |
                RelativeCauseKind.PawnWeaknessTarget | RelativeCauseKind.PlanImprovement |
                RelativeCauseKind.DefensiveResource | RelativeCauseKind.DrawResource =>
              CauseAttributionKind.CandidateCreatesValue
            case _ =>
              CauseAttributionKind.CandidateAllowsLiability
        case RelativeCauseSourceSide.Shared | RelativeCauseSourceSide.Mixed =>
          CauseAttributionKind.SharedContext

  private def relativeCauseProof(
      graph: TypedEvidenceGraph,
      kind: RelativeCauseKind,
      sourceSide: RelativeCauseSourceSide,
      directRecords: List[EvidenceRecord],
      contrastRecords: List[EvidenceRecord],
      contextRecords: List[EvidenceRecord]
  ): RelativeCauseProof =
    RelativeCauseProof(
      directProof = relativeCauseProofSection(
        role = RelativeCauseProofRole.DirectProof,
        strength = RelativeCauseProofStrength.Primary,
        kind = kind,
        sourceSide = sourceSide,
        graph = graph,
        records = directRecords,
        includeContextLayers = false
      ),
      contrastProof = relativeCauseProofSection(
        role = RelativeCauseProofRole.ContrastProof,
        strength = RelativeCauseProofStrength.Supporting,
        kind = kind,
        sourceSide = sourceSide,
        graph = graph,
        records = contrastRecords,
        includeContextLayers = false
      ),
      contextSupport = relativeCauseProofSection(
        role = RelativeCauseProofRole.ContextSupport,
        strength = RelativeCauseProofStrength.WeakHint,
        kind = kind,
        sourceSide = sourceSide,
        graph = graph,
        records = contextRecords,
        includeContextLayers = true
      )
    )

  private def relativeCauseProofSection(
      role: RelativeCauseProofRole,
      strength: RelativeCauseProofStrength,
      kind: RelativeCauseKind,
      sourceSide: RelativeCauseSourceSide,
      graph: TypedEvidenceGraph,
      records: List[EvidenceRecord],
      includeContextLayers: Boolean
  ): RelativeCauseProofSection =
    val proofRecords = records.distinctBy(_.ref.id)
    val structuralProofSourceIds =
      proofRecords.collect {
        case EvidenceRecord(ref, structural: StructuralDeltaEvidence, _)
            if structuralConsequencesForCause(kind, structural).nonEmpty &&
              strategicStructuralProofReady(kind, structural) =>
          ref.id
      }.toSet
    RelativeCauseProofSection(
      role = role,
      strength = strength,
      boardAnchors = proofRecords.flatMap {
        case EvidenceRecord(ref, payload: BoardFactEvidence, _) =>
          payload.proofSignalAnchorKinds.map(kind => BoardAnchorProof(ref, kind))
        case _ =>
          Nil
      }.distinct,
      lineEvents = proofRecords.flatMap {
        case EvidenceRecord(ref, payload: LineFactEvidence, _) =>
          payload.lineEvents.map(event =>
            LineEventProof(
              source = ref,
              kind = event.kind,
              moveUci = Some(event.moveUci),
              plyOffset = Some(event.plyOffset),
              side = event.side,
              square = event.square
            )
          )
        case _ =>
          Nil
      }.distinct,
      lineConsequences = proofRecords.flatMap {
        case EvidenceRecord(ref, payload: LineFactEvidence, _) =>
          lineConsequenceProofsForCause(kind, ref, payload)
        case _ =>
          Nil
      }.distinct,
      relationProofs = proofRecords.collect { case EvidenceRecord(ref, payload: RelationFactEvidence, _) if payload.hasConcreteRelationProof =>
        RelationCauseProof(
          source = ref,
          kind = payload.kind,
          proof = payload.witnessProof
        )
      }.distinct,
      tacticalMechanisms = proofRecords.collect {
        case EvidenceRecord(ref, payload: TacticalMechanismEvidence, _) if payload.hasConcreteProof =>
          TacticalMechanismProof(
            source = ref,
            kind = payload.kind,
            signals = payload.signals
          )
      }.distinct,
      strategicMechanisms = proofRecords.collect {
        case EvidenceRecord(ref, payload: StrategicMechanismEvidence, _) if payload.canSupportStrategicCause =>
          val signals = strategicMechanismProofSignals(graph, kind, payload, sourceSide, structuralProofSourceIds)
          StrategicMechanismProof(
            source = ref,
            kind = payload.kind,
            signals = signals
          )
      }.filter(_.signals.nonEmpty).distinct,
      strategicMechanismContrasts = proofRecords.collect {
        case EvidenceRecord(ref, payload: StrategicMechanismContrastEvidence, _) if payload.hasActionableContrast =>
          StrategicMechanismContrastProof(
            source = ref,
            comparisonKind = payload.comparisonKind,
            referenceLine = payload.referenceLine,
            candidateLine = payload.candidateLine,
            axisComparisons = payload.axisComparisons,
            sustainability = payload.sustainability
          )
      }.distinct,
      threatEpisodes = proofRecords.collect {
        case EvidenceRecord(ref, payload: ThreatEpisodeEvidence, _) if payload.isProofSignalDefensivePressure =>
          ThreatEpisodeCauseProof(
            source = ref,
            driver = payload.episode.driver,
            kind = payload.episode.kind,
            severity = payload.episode.severity
          )
      }.distinct,
      transitionConsequences = proofRecords.flatMap {
        case EvidenceRecord(ref, payload: StructuralDeltaEvidence, _) =>
          structuralConsequencesForCause(kind, payload).map(consequence =>
            TransitionConsequenceProof(
              source = ref,
              transition = payload.transition,
              consequence = consequence
            )
          )
        case _ =>
          Nil
      }.distinct,
      contextLayers = if includeContextLayers then proofRecords.map(_.ref.layer).distinct else Nil
    )

  private def lineConsequenceProofsForCause(
      kind: RelativeCauseKind,
      ref: EvidenceRef,
      payload: LineFactEvidence
  ): List[LineConsequenceProof] =
    payload.proofSignalConsequences
      .filter(consequence => lineConsequenceCanProveRelativeCause(kind, consequence.kind))
      .map(consequence =>
        LineConsequenceProof(
          source = ref,
          kind = consequence.kind,
          eventMove = consequence.eventMove,
          lineMoves = consequence.lineMoves
        )
      )
      .distinct

  private def lineConsequenceCanProveRelativeCause(
      kind: RelativeCauseKind,
      consequenceKind: LineConsequenceKind
  ): Boolean =
    kind match
      case RelativeCauseKind.WrongRecapturer | RelativeCauseKind.RecaptureRecoveryWindow =>
        consequenceKind == LineConsequenceKind.RecaptureSequence ||
          consequenceKind == LineConsequenceKind.RecoveryWindow
      case RelativeCauseKind.WrongMoveOrder | RelativeCauseKind.TempoLoss =>
        consequenceKind == LineConsequenceKind.ImmediateReplyCheck
      case RelativeCauseKind.KingForcing =>
        consequenceKind == LineConsequenceKind.Mate ||
          consequenceKind == LineConsequenceKind.ForcedTheme
      case RelativeCauseKind.DrawResource =>
        consequenceKind == LineConsequenceKind.DrawResource
      case RelativeCauseKind.ConversionMiss | RelativeCauseKind.ConversionSecured =>
        consequenceKind == LineConsequenceKind.RecaptureSequence ||
          consequenceKind == LineConsequenceKind.RecoveryWindow ||
          consequenceKind == LineConsequenceKind.PromotionRace
      case RelativeCauseKind.MaterialSwing | RelativeCauseKind.SacrificeCompensation =>
        consequenceKind == LineConsequenceKind.MaterialGain ||
          consequenceKind == LineConsequenceKind.MaterialLoss ||
          consequenceKind == LineConsequenceKind.Sacrifice
      case RelativeCauseKind.MissedTacticalResource | RelativeCauseKind.TacticalRefutationOfPlayed |
          RelativeCauseKind.CandidateTacticalLiability =>
        LineConsequenceKind.tacticalDriver(consequenceKind)
      case _ =>
        false

  private def proofSectionRecords(
      graph: TypedEvidenceGraph,
      records: List[EvidenceRecord],
      kind: Option[RelativeCauseKind] = None,
      binding: Option[RelativeCauseBinding] = None
  ): List[EvidenceRecord] =
    val mechanismParents =
      records.collect {
        case record @ EvidenceRecord(_, payload: TacticalMechanismEvidence, _) if payload.hasConcreteProof =>
          parentClosure(graph, record).filter(record => proofSourceLayer(record.ref.layer))
      }.flatten
    val strategicStructuralParents =
      records.collect {
        case EvidenceRecord(_, payload: StrategicMechanismEvidence, _) if payload.canSupportStrategicCause =>
          payload.signals
            .filter(signal =>
              (kind, binding) match
                case (Some(causeKind), Some(causeBinding)) =>
                  strategicSignalDirectlyOwnsCause(graph, signal, causeKind, causeBinding.eventLine.rootMove, causeBinding.sourceSide, Some(causeBinding.eventLine))
                case _ =>
                  signal.kind == StrategicMechanismSignalKind.StructuralDelta
            )
            .flatMap(signal => graph.byId.get(signal.source.id))
            .collect { case record @ EvidenceRecord(_, _: StructuralDeltaEvidence, _) => record }
      }.flatten
    (records ++ mechanismParents ++ strategicStructuralParents).distinctBy(_.ref.id)

  private def proofSectionRecordIds(graph: TypedEvidenceGraph, records: List[EvidenceRecord]): Set[String] =
    proofSectionRecords(graph, records).map(_.ref.id).toSet

  private def supportClosureRecordIds(graph: TypedEvidenceGraph, records: List[EvidenceRecord]): Set[String] =
    records
      .flatMap(record => record :: parentClosure(graph, record))
      .map(_.ref.id)
      .toSet

  private def contextSupportRecord(
      fact: CandidateComparisonFact,
      record: EvidenceRecord
  ): Boolean =
    val referencesComparedLine =
      record.referencesLine(fact.referenceLine) || record.referencesLine(fact.candidateLine)
    !referencesComparedLine &&
      (
        record.ref.scope == EvidenceScope.BeforePosition ||
          record.ref.scope == EvidenceScope.CurrentPosition ||
          record.ref.line.nonEmpty
      )

  private def proofSourceLayer(layer: EvidenceLayer): Boolean =
    layer match
      case EvidenceLayer.Board | EvidenceLayer.Line | EvidenceLayer.Relation | EvidenceLayer.ThreatPressure |
          EvidenceLayer.StrategicMechanism =>
        true
      case _ =>
        false

  private def directProofSource(
      graph: TypedEvidenceGraph,
      fact: CandidateComparisonFact,
      kind: RelativeCauseKind,
      binding: RelativeCauseBinding,
      record: EvidenceRecord
  ): Boolean =
    val rootMove = binding.eventLine.rootMove
    record.payload match
      case payload: LineFactEvidence =>
        record.referencesLine(binding.eventLine) &&
          lineFactDirectlyOwnsCause(kind, payload, rootMove)
      case payload: TacticalMechanismEvidence =>
        payload.canAnchorTacticalIdea &&
          record.referencesLine(binding.eventLine) &&
          tacticalMechanismDirectlyOwnsRoot(graph, record, rootMove)
      case payload: RelationFactEvidence =>
        payload.hasConcreteRelationProof &&
          record.referencesLine(binding.eventLine) &&
          relationCanDirectlyProveCause(kind, payload)
      case payload: ThreatEpisodeEvidence =>
        defensiveCause(kind) &&
          payload.isProofSignalDefensivePressure &&
          (
            record.referencesLine(binding.eventLine) ||
              payload.onlyDefense.exists(move => normalizeMove(move) == normalizeMove(rootMove))
          )
      case payload: StructuralDeltaEvidence =>
        record.referencesLine(binding.eventLine) &&
          payload.line.contains(binding.eventLine) &&
          normalizeMove(payload.moveUci) == normalizeMove(rootMove) &&
          structuralConsequencesForCause(kind, payload).nonEmpty
      case _: StrategicMechanismEvidence =>
        strategicCause(kind) &&
          record.referencesLine(binding.eventLine) &&
          strategicMechanismDirectlyOwnsCause(graph, record, kind, binding.eventLine.rootMove, binding.sourceSide)
      case payload: StrategicMechanismContrastEvidence =>
        strategicCause(kind) &&
          payload.comparisonKind == fact.kind &&
          payload.referenceLine == fact.referenceLine &&
          payload.candidateLine == fact.candidateLine &&
          strategicContrastCanDirectlyProveCause(kind, payload, binding.sourceSide)
      case _ =>
        false

  private def lineFactDirectlyOwnsCause(
      kind: RelativeCauseKind,
      payload: LineFactEvidence,
      rootMove: String
  ): Boolean =
    val rootOwnedConsequences = payload.rootOwnedProofSignalConsequences(rootMove)
    kind match
      case RelativeCauseKind.WrongRecapturer | RelativeCauseKind.RecaptureRecoveryWindow =>
        payload.hasRootCaptureEvent(rootMove) &&
          rootOwnedConsequences.exists(consequence =>
            consequence.kind == LineConsequenceKind.RecaptureSequence ||
              consequence.kind == LineConsequenceKind.RecoveryWindow
          )
      case RelativeCauseKind.WrongMoveOrder | RelativeCauseKind.TempoLoss =>
        rootOwnedConsequences.exists(_.kind == LineConsequenceKind.ImmediateReplyCheck) ||
          payload.rootOwnedLineEvents(rootMove).exists(event =>
            event.kind == LineEventKind.Tempo || event.kind == LineEventKind.Check
          )
      case RelativeCauseKind.KingForcing =>
        rootOwnedConsequences.exists(consequence =>
          consequence.kind == LineConsequenceKind.Mate ||
            consequence.kind == LineConsequenceKind.ForcedTheme
        ) ||
          payload.rootOwnedLineEvents(rootMove).exists(event =>
            event.kind == LineEventKind.Check || event.kind == LineEventKind.Mate
          )
      case RelativeCauseKind.DrawResource =>
        rootOwnedConsequences.exists(_.kind == LineConsequenceKind.DrawResource) ||
          payload.rootOwnedEndgameTechniqueHorizons(rootMove, kind).nonEmpty
      case RelativeCauseKind.MaterialSwing | RelativeCauseKind.SacrificeCompensation =>
        rootOwnedConsequences.exists(consequence =>
          consequence.kind == LineConsequenceKind.MaterialGain ||
            consequence.kind == LineConsequenceKind.MaterialLoss ||
            consequence.kind == LineConsequenceKind.Sacrifice
        )
      case RelativeCauseKind.ConversionMiss | RelativeCauseKind.ConversionSecured =>
        rootOwnedConsequences.exists(consequence =>
          consequence.kind == LineConsequenceKind.RecaptureSequence ||
            consequence.kind == LineConsequenceKind.RecoveryWindow ||
            consequence.kind == LineConsequenceKind.PromotionRace
        ) ||
          payload.rootOwnedEndgameTechniqueHorizons(rootMove, kind).nonEmpty
      case RelativeCauseKind.MissedTacticalResource | RelativeCauseKind.TacticalRefutationOfPlayed |
          RelativeCauseKind.CandidateTacticalLiability =>
        rootOwnedConsequences.exists(consequence => LineConsequenceKind.tacticalDriver(consequence.kind)) ||
          payload.rootOwnedLineEvents(rootMove).exists(event =>
            event.kind == LineEventKind.Capture ||
              event.kind == LineEventKind.Recapture ||
              event.kind == LineEventKind.Check ||
              event.kind == LineEventKind.Mate ||
              event.kind == LineEventKind.Promotion
          )
      case _ =>
        false

  private def tacticalMechanismDirectlyOwnsRoot(
      graph: TypedEvidenceGraph,
      record: EvidenceRecord,
      rootMove: String
  ): Boolean =
    record.payload match
      case payload: TacticalMechanismEvidence =>
        payload.moveUci.exists(move => normalizeMove(move) == normalizeMove(rootMove)) ||
          parentClosure(graph, record).exists {
            case EvidenceRecord(_, line: LineFactEvidence, _) =>
              lineFactDirectlyOwnsCause(TacticalMechanismKind.relativeCauseKind(payload.kind, badLoss = false, playedCandidate = false), line, rootMove)
            case EvidenceRecord(_, relation: RelationFactEvidence, _) =>
              relation.hasConcreteRelationProof && record.ref.line.exists(_.rootMove == rootMove)
            case EvidenceRecord(_, threat: ThreatEpisodeEvidence, _) =>
              threat.isProofSignalDefensivePressure &&
                threat.onlyDefense.exists(move => normalizeMove(move) == normalizeMove(rootMove))
            case _ =>
              false
          }
      case _ =>
        false

  private def relationCanDirectlyProveCause(kind: RelativeCauseKind, payload: RelationFactEvidence): Boolean =
    kind match
      case RelativeCauseKind.MissedTacticalResource | RelativeCauseKind.TacticalRefutationOfPlayed |
          RelativeCauseKind.CandidateTacticalLiability =>
        payload.hasConcreteRelationProof
      case RelativeCauseKind.WrongMoveOrder | RelativeCauseKind.TempoLoss =>
        payload.kind == RelationFactKind.Zwischenzug && payload.hasConcreteRelationProof
      case RelativeCauseKind.MaterialSwing =>
        payload.kind == RelationFactKind.HangingPiece ||
          payload.kind == RelationFactKind.TrappedPiece ||
          payload.kind == RelationFactKind.Domination
      case RelativeCauseKind.ConversionMiss | RelativeCauseKind.ConversionSecured =>
        payload.kind == RelationFactKind.BadPieceLiquidation
      case _ =>
        false

  private def strategicMechanismDirectlyOwnsCause(
      graph: TypedEvidenceGraph,
      record: EvidenceRecord,
      kind: RelativeCauseKind,
      rootMove: String,
      sourceSide: RelativeCauseSourceSide
  ): Boolean =
    record.payload match
      case payload: StrategicMechanismEvidence if payload.canSupportStrategicCause =>
        payload.signals.exists(signal =>
          strategicSignalDirectlyOwnsCause(graph, signal, kind, rootMove, sourceSide, record.ref.line)
        )
      case _ =>
        false

  private def strategicSignalDirectlyOwnsCause(
      graph: TypedEvidenceGraph,
      signal: StrategicMechanismSignal,
      kind: RelativeCauseKind,
      rootMove: String,
      sourceSide: RelativeCauseSourceSide,
      eventLine: Option[LineNodeRef]
  ): Boolean =
    val normalizedRoot = normalizeMove(rootMove)
    signal.kind == StrategicMechanismSignalKind.StructuralDelta &&
      signal.axis.exists(axis => strategicAxisCanProveCause(kind, axis, sourceSide)) &&
      graph.byId.get(signal.source.id).exists {
        case EvidenceRecord(sourceRef, structural: StructuralDeltaEvidence, _) =>
          val provingConsequences = structuralConsequencesForCause(kind, structural, signal.axis)
          eventLine.forall(line => sourceRef.line.contains(line) && structural.line.contains(line)) &&
            normalizeMove(structural.moveUci) == normalizedRoot &&
            provingConsequences.nonEmpty &&
            strategicStructuralProofReady(kind, provingConsequences)
        case _ =>
          false
      }

  private[chessjudgment] def strategicMechanismProofSignals(
      graph: TypedEvidenceGraph,
      kind: RelativeCauseKind,
      payload: StrategicMechanismEvidence,
      sourceSide: RelativeCauseSourceSide,
      selectedStructuralSourceIds: Set[String] = Set.empty
  ): List[StrategicMechanismSignal] =
    val axisMatched =
      payload.signals.filter(signal =>
        signal.axis.exists(axis => strategicAxisCanProveCause(kind, axis, sourceSide))
      )
    val structuralMatched =
      axisMatched.filter(signal =>
        signal.kind == StrategicMechanismSignalKind.StructuralDelta &&
          (selectedStructuralSourceIds.isEmpty || selectedStructuralSourceIds.contains(signal.source.id)) &&
          graph.byId.get(signal.source.id).exists {
            case EvidenceRecord(_, structural: StructuralDeltaEvidence, _) =>
              val provingConsequences = structuralConsequencesForCause(kind, structural, signal.axis)
              provingConsequences.nonEmpty &&
                strategicStructuralProofReady(kind, provingConsequences)
            case _ =>
              false
          }
      )
    if selectedStructuralSourceIds.nonEmpty then structuralMatched.distinct
    else if structuralMatched.nonEmpty then structuralMatched.distinct
    else axisMatched.distinct

  private def strategicStructuralProofReady(
      kind: RelativeCauseKind,
      structural: StructuralDeltaEvidence
  ): Boolean =
    strategicStructuralProofReady(kind, structuralConsequencesForCause(kind, structural))

  private def strategicStructuralProofReady(
      kind: RelativeCauseKind,
      provingConsequences: List[TransitionConsequence]
  ): Boolean =
    if kind == RelativeCauseKind.ActivityGain then
      provingConsequences.exists(activityGainConsequenceHasConcreteRoute)
    else
      provingConsequences.exists(consequenceHasConcreteStrategicTarget)

  private def activityGainConsequenceHasConcreteRoute(consequence: TransitionConsequence): Boolean =
    import TransitionConsequenceKind.*
    val routeKind =
      consequence.kind == OutpostGain ||
        consequence.kind == BatteryPressureGain ||
        consequence.kind == FileOccupationGain ||
        consequence.kind == FileAccessGain ||
        consequence.kind == RookLiftActivation ||
        consequence.kind == LineUnlockGain
    consequenceHasConcreteStrategicTarget(consequence) &&
      (
        routeKind ||
          consequence.subjects.exists(activityGainQualifiedRouteSubject)
      )

  private def activityGainQualifiedRouteSubject(subject: String): Boolean =
    val normalized = Option(subject).getOrElse("").trim.toLowerCase
    normalized.contains("outpost") ||
      normalized.contains("battery") ||
      normalized.contains("diagonal") ||
      normalized.contains("filecontrol") ||
      normalized.contains("file-control") ||
      normalized.contains("fileaccess") ||
      normalized.contains("file-access") ||
      normalized.contains("fileoccupation") ||
      normalized.contains("file-occupation") ||
      normalized.contains("maneuver")

  private def consequenceHasConcreteStrategicTarget(consequence: TransitionConsequence): Boolean =
    consequence.subjects.exists(subject =>
      val normalized = Option(subject).getOrElse("").trim.toLowerCase
      normalized.matches(".*[a-h][1-8].*") ||
        normalized.matches(".*\\b[a-h]\\b.*") ||
        normalized.startsWith("file:")
    )

  private def strategicContrastCanDirectlyProveCause(
      kind: RelativeCauseKind,
      payload: StrategicMechanismContrastEvidence,
      sourceSide: RelativeCauseSourceSide
  ): Boolean =
    payload.actionableComparisons.exists(axis =>
      strategicAxisCanProveCause(kind, axis.axis, sourceSide) &&
        axis.hasContrast
    )

  private def strategicAxisCanProveCause(
      kind: RelativeCauseKind,
      axis: StrategicAxisDetail,
      sourceSide: RelativeCauseSourceSide
  ): Boolean =
    kind match
      case RelativeCauseKind.PlanImprovement | RelativeCauseKind.PlanContradiction =>
        axis.kind == StrategicAxisKind.PlanCoherence
      case RelativeCauseKind.TargetPressureGain | RelativeCauseKind.PawnWeaknessTarget =>
        axis.kind == StrategicAxisKind.Target && axis.polarity == StrategicAxisPolarity.Gain
      case RelativeCauseKind.TargetPressureRelease =>
        axis.kind == StrategicAxisKind.Target && axis.polarity == StrategicAxisPolarity.Release
      case RelativeCauseKind.PawnBreakOpportunity =>
        axis.kind == StrategicAxisKind.PawnBreak &&
          (
            axis.polarity == StrategicAxisPolarity.Support ||
              axis.polarity == StrategicAxisPolarity.Preserve ||
              axis.polarity == StrategicAxisPolarity.Release ||
              axis.polarity == StrategicAxisPolarity.Gain
          ) &&
          (
            axis.polarity == StrategicAxisPolarity.Release ||
              !pawnBreakResolutionAxis(axis)
          )
      case RelativeCauseKind.CenterControlGain =>
        axis.kind == StrategicAxisKind.SpaceCenter
      case RelativeCauseKind.ActivityGain =>
        axis.kind == StrategicAxisKind.Activity &&
          axis.polarity == StrategicAxisPolarity.Gain &&
          (sourceSide == RelativeCauseSourceSide.Reference || sourceSide == RelativeCauseSourceSide.Candidate)
      case RelativeCauseKind.ActivityLoss =>
        axis.kind == StrategicAxisKind.Activity && sourceSide == RelativeCauseSourceSide.Candidate
      case RelativeCauseKind.OpponentRestriction =>
        axis.kind == StrategicAxisKind.Counterplay && axis.polarity == StrategicAxisPolarity.Restrain
      case RelativeCauseKind.KingSafetyConcession =>
        axis.kind == StrategicAxisKind.Counterplay && axis.label.contains("king-safety")
      case RelativeCauseKind.StructuralImprovement | RelativeCauseKind.MissedStrategicImprovement |
          RelativeCauseKind.StrategicConcession =>
        true
      case _ =>
        false

  private def defensiveCause(kind: RelativeCauseKind): Boolean =
    kind == RelativeCauseKind.OnlyDefenseNecessity ||
      kind == RelativeCauseKind.DefensiveResource ||
      kind == RelativeCauseKind.OnlyMoveNecessity

  private def strategicCause(kind: RelativeCauseKind): Boolean =
    RelativeCauseKind.strategicContrastBacked(kind)

  private def recordMatchesEventRoot(record: EvidenceRecord, rootMove: String): Boolean =
    record.payload match
      case payload: LineFactEvidence =>
        payload.rootOwnedLineEvents(rootMove).nonEmpty ||
          payload.rootOwnedProofSignalConsequences(rootMove).nonEmpty ||
          payload.rootOwnedEndgameTechniqueHorizons(rootMove, RelativeCauseKind.ConversionSecured).nonEmpty ||
          payload.rootOwnedEndgameTechniqueHorizons(rootMove, RelativeCauseKind.ConversionMiss).nonEmpty ||
          payload.rootOwnedEndgameTechniqueHorizons(rootMove, RelativeCauseKind.DrawResource).nonEmpty
      case payload: TacticalMechanismEvidence =>
        payload.moveUci.exists(move => normalizeMove(move) == normalizeMove(rootMove))
      case payload: StructuralDeltaEvidence =>
        normalizeMove(payload.moveUci) == normalizeMove(rootMove)
      case _: RelationFactEvidence | _: StrategicMechanismEvidence | _: StrategicMechanismContrastEvidence |
          _: ThreatEpisodeEvidence =>
        record.ref.line.exists(line => normalizeMove(line.rootMove) == normalizeMove(rootMove)) ||
          record.payloadLineRefs.exists(line => normalizeMove(line.rootMove) == normalizeMove(rootMove))
      case _ =>
        false

  private def normalizeMove(raw: String): String =
    Option(raw).getOrElse("").trim.toLowerCase

  private def parentClosure(graph: TypedEvidenceGraph, record: EvidenceRecord): List[EvidenceRecord] =
    def loop(refs: List[EvidenceRef], seen: Set[String]): List[EvidenceRecord] =
      refs.flatMap { ref =>
        if seen.contains(ref.id) then Nil
        else
          graph.byId.get(ref.id).toList.flatMap { parent =>
            parent :: loop(parent.parents, seen + ref.id)
          }
      }
    loop(record.parents, Set.empty).distinctBy(_.ref.id)

  private def structuralConsequencesForCause(
      kind: RelativeCauseKind,
      payload: StructuralDeltaEvidence
  ): List[TransitionConsequence] =
    import TransitionConsequenceKind.*
    kind match
      case RelativeCauseKind.TargetPressureGain =>
        payload.consequencesOf(TargetPressureGain)
      case RelativeCauseKind.TargetPressureRelease =>
        payload.consequencesOf(TargetPressureRelease)
      case RelativeCauseKind.CenterControlGain =>
        payload.consequencesOf(CenterControlGain)
      case RelativeCauseKind.OpponentRestriction =>
        opponentRestrictionConsequences(payload) ++
          counterBreakCarrierConsequences(payload)
      case RelativeCauseKind.KingSafetyConcession =>
        payload.consequencesOf(KingSafetyConcession) ++
          payload.consequencesOf(KingRingPressureConcession)
      case RelativeCauseKind.PawnWeaknessTarget =>
        payload.consequencesOf(WeakPawnTargetCreated)
      case RelativeCauseKind.PawnBreakOpportunity =>
        payload.consequencesOf(PawnTensionGain) ++
          payload.consequencesOf(PawnTensionResolution)
      case RelativeCauseKind.ActivityGain =>
        payload.consequencesOf(DevelopmentLagReduced) ++
          payload.consequencesOf(DevelopmentPieceActivated) ++
          payload.consequencesOf(DevelopmentMobilityGain) ++
          payload.consequencesOf(DevelopmentCenterControlGain) ++
          payload.consequencesOf(DevelopmentSafePlacement) ++
          payload.consequencesOf(FileOccupationGain) ++
          payload.consequencesOf(MobilityGain) ++
          payload.consequencesOf(LineUnlockGain) ++
          payload.consequencesOf(FileAccessGain) ++
          payload.consequencesOf(BatteryPressureGain) ++
          payload.consequencesOf(OutpostGain)
      case RelativeCauseKind.ActivityLoss =>
        payload.consequencesOf(DevelopmentLagIncreased) ++
          payload.consequencesOf(DevelopmentPieceRetreated) ++
          payload.consequencesOf(DevelopmentMobilityLoss) ++
          payload.consequencesOf(DevelopmentCenterControlLoss) ++
          payload.consequencesOf(DevelopmentUnsafePlacement) ++
          payload.consequencesOf(MobilityLoss) ++
          payload.consequencesOf(FileAccessLoss) ++
          payload.consequencesOf(OutpostConcession)
      case RelativeCauseKind.StructuralImprovement | RelativeCauseKind.MissedStrategicImprovement |
          RelativeCauseKind.PlanImprovement | RelativeCauseKind.PlanContradiction =>
        payload.positiveConsequences.filter(consequence =>
          StructuralDeltaEvidence.isStructuralAnchorConsequence(consequence.kind)
        )
      case RelativeCauseKind.ConversionMiss | RelativeCauseKind.ConversionSecured =>
        payload.positiveConsequences.filter(consequence =>
          consequence.kind == PromotionPressureGain ||
            consequence.kind == PassedPawnProgress
        )
      case RelativeCauseKind.StrategicConcession =>
        payload.negativeConsequences.filter(consequence =>
          StructuralDeltaEvidence.isStrategicSupportConsequence(consequence.kind)
        )
      case _ =>
        Nil

  private def structuralConsequencesForCause(
      kind: RelativeCauseKind,
      payload: StructuralDeltaEvidence,
      axis: Option[StrategicAxisDetail]
  ): List[TransitionConsequence] =
    import TransitionConsequenceKind.*
    kind match
      case RelativeCauseKind.PawnBreakOpportunity =>
        axis match
          case Some(detail) if detail.kind == StrategicAxisKind.PawnBreak && detail.polarity == StrategicAxisPolarity.Release =>
            payload.consequencesOf(PawnTensionResolution)
          case Some(detail)
              if detail.kind == StrategicAxisKind.PawnBreak &&
                !pawnBreakResolutionAxis(detail) &&
                (
                  detail.polarity == StrategicAxisPolarity.Support ||
                    detail.polarity == StrategicAxisPolarity.Preserve ||
                    detail.polarity == StrategicAxisPolarity.Gain
                ) =>
            payload.consequencesOf(PawnTensionGain)
          case Some(_) =>
            Nil
          case None =>
            structuralConsequencesForCause(kind, payload)
      case RelativeCauseKind.OpponentRestriction =>
        axis match
          case Some(detail) if RelativeCauseSignalProfile.currentMoveCounterBreakAxis(detail) =>
            opponentRestrictionConsequences(payload) ++
              counterBreakCarrierConsequences(payload)
          case _ =>
            opponentRestrictionConsequences(payload)
      case _ =>
        structuralConsequencesForCause(kind, payload)

  private def pawnBreakResolutionAxis(axis: StrategicAxisDetail): Boolean =
    val normalized = axis.label.toLowerCase
    normalized.contains("resolved-tension") ||
      normalized.contains("-release-")

  private def opponentRestrictionConsequences(payload: StructuralDeltaEvidence): List[TransitionConsequence] =
    payload.consequencesOf(TransitionConsequenceKind.OpponentMobilityRestriction).filter(consequence =>
      consequence.subjects.exists(StructuralDeltaEvidence.validOpponentMobilityRestrictionSubject)
    )

  private def counterBreakCarrierConsequences(payload: StructuralDeltaEvidence): List[TransitionConsequence] =
    payload.consequences.filter(counterBreakCarrierConsequence)

  private def counterBreakCarrierConsequence(consequence: TransitionConsequence): Boolean =
    (
      consequence.kind == TransitionConsequenceKind.PawnTensionGain ||
        consequence.kind == TransitionConsequenceKind.PawnTensionResolution
    ) &&
      consequence.subjects.exists(subject =>
        val normalized = Option(subject).getOrElse("").trim.toLowerCase
        normalized.startsWith("break-file:") ||
          normalized.startsWith("created-tension:") ||
          normalized.startsWith("resolved-tension:")
      )

  private def mergeCauseCandidates(candidates: List[RelativeCauseDraft]): List[RelativeCauseDraft] =
    candidates
      .groupBy(candidate => (candidate.kind, candidate.sourceSide, candidate.attributionKind, supportSignature(candidate.support)))
      .toList
      .sortBy { case ((kind, sourceSide, attributionKind, signature), _) =>
        (kind.toString, sourceSide.map(_.toString).getOrElse("none"), attributionKind.toString, signature)
      }
      .map { case ((kind, sourceSide, attributionKind, _), grouped) =>
        val first = grouped.headOption
        RelativeCauseDraft(kind, first.map(_.support).getOrElse(Nil), sourceSide, attributionKind)
      }

  private def supportSignature(records: List[EvidenceRecord]): String =
    records.map(_.ref.id).distinct.sorted.mkString("|")

  private def comparisonProofRecords(neighborhood: ComparisonEvidenceNeighborhood): List[EvidenceRecord] =
    (neighborhood.referenceEndpoint ++ neighborhood.candidateEndpoint)
      .filter(record => record.ref.layer == EvidenceLayer.Line || record.ref.layer == EvidenceLayer.Eval)
      .distinctBy(_.ref.id)

  private def inferCauseCandidates(
      neighborhood: ComparisonEvidenceNeighborhood,
      fact: CandidateComparisonFact
  ): List[RelativeCauseDraft] =
    val profile =
      RelativeCauseSignalProfile.from(
        fact = fact,
        referenceRecords = neighborhood.referenceRecords,
        candidateRecords = neighborhood.candidateRecords,
        sharedRecords = neighborhood.sharedRecords
      )
    RelativeCauseDraftPlanner.drafts(profile)

  private def parentsForLine(
      context: JudgmentAssemblyContext,
      line: CandidateLineNode
  ): List[EvidenceRef] =
    context.evidenceGraph.recordsFor(line.ref).map(_.ref)

  private def comparisonNeighborhood(
      context: JudgmentAssemblyContext,
      root: PositionNodeRef,
      fact: CandidateComparisonFact
  ): ComparisonEvidenceNeighborhood =
    val referenceTransition = transitionForLine(context, fact.referenceLine)
    val candidateTransition = transitionForLine(context, fact.candidateLine)
    val baseRecords =
      (
        recordsForLineEndpoint(context, fact.referenceLine) ++
          recordsForLineEndpoint(context, fact.candidateLine) ++
          recordsForRootContext(context, root) ++
          recordsForTransition(context, referenceTransition) ++
          recordsForTransition(context, candidateTransition) ++
          recordsForAfterPosition(context, referenceTransition) ++
          recordsForAfterPosition(context, candidateTransition)
      ).distinctBy(_.ref.id)
    val parentRecords =
      baseRecords.flatMap(record => record.parents.flatMap(parent => context.evidenceGraph.byId.get(parent.id)))
    ComparisonEvidenceNeighborhood(
      referenceEndpoint = recordsForLineEndpoint(context, fact.referenceLine),
      candidateEndpoint = recordsForLineEndpoint(context, fact.candidateLine),
      rootContext = recordsForRootContext(context, root),
      referenceTransition = recordsForTransition(context, referenceTransition),
      candidateTransition = recordsForTransition(context, candidateTransition),
      referenceAfterPosition = recordsForAfterPosition(context, referenceTransition),
      candidateAfterPosition = recordsForAfterPosition(context, candidateTransition),
      parents = parentRecords.distinctBy(_.ref.id)
    )

  private def recordsForLineEndpoint(
      context: JudgmentAssemblyContext,
      line: LineNodeRef
  ): List[EvidenceRecord] =
    context.evidenceGraph.records.filter(record =>
      endpointLayer(record.ref.layer) &&
        record.referencesLine(line)
    )

  private def recordsForRootContext(
      context: JudgmentAssemblyContext,
      root: PositionNodeRef
  ): List[EvidenceRecord] =
    context.evidenceGraph.records.filter(record =>
      rootContextLayer(record.ref.layer) &&
        record.ref.position == root &&
        (record.ref.scope == EvidenceScope.BeforePosition || record.ref.scope == EvidenceScope.CurrentPosition)
    )

  private def recordsForTransition(
      context: JudgmentAssemblyContext,
      transition: Option[MoveTransitionEdge]
  ): List[EvidenceRecord] =
    transition.toList.flatMap { edge =>
      context.evidenceGraph.records.filter(record =>
        transitionLayer(record.ref.layer) &&
          record.ref.position == edge.from &&
          recordMatchesTransition(record, edge)
      )
    }

  private def recordsForAfterPosition(
      context: JudgmentAssemblyContext,
      transition: Option[MoveTransitionEdge]
  ): List[EvidenceRecord] =
    transition.toList.flatMap(edge =>
      context.evidenceGraph.records.filter(record =>
        afterPositionLayer(record.ref.layer) &&
          record.ref.position == edge.to
      )
    )

  private def transitionForLine(
      context: JudgmentAssemblyContext,
      line: LineNodeRef
  ): Option[MoveTransitionEdge] =
    context.transitions.find(edge =>
      edge.moveUci == line.rootMove &&
        line.role == edge.role.lineRole
    )

  private def recordMatchesTransition(record: EvidenceRecord, edge: MoveTransitionEdge): Boolean =
    record.payload match
      case MoveTransitionEvidence(moveUci, from, to) =>
        record.ref.scope == edge.role.scope &&
          moveUci == edge.moveUci &&
          from == edge.from &&
          to == edge.to
      case payload: StructuralDeltaEvidence =>
        payload.role == edge.role &&
          payload.moveUci == edge.moveUci &&
          payload.from == edge.from &&
          payload.to == edge.to
      case _: StrategicMechanismEvidence =>
        record.ref.scope == edge.role.scope &&
          (
            record.ref.line.exists(line => line.rootMove == edge.moveUci && line.role == edge.role.lineRole) ||
              record.parents.exists(_.id == edge.evidence.id)
          )
      case _ =>
        false

  private def endpointLayer(layer: EvidenceLayer): Boolean =
    layer match
      case EvidenceLayer.Line | EvidenceLayer.Eval | EvidenceLayer.MoveMotif | EvidenceLayer.Relation |
          EvidenceLayer.TacticalMechanism | EvidenceLayer.StrategicMechanism =>
        true
      case _ =>
        false

  private def rootContextLayer(layer: EvidenceLayer): Boolean =
    layer match
      case EvidenceLayer.Board | EvidenceLayer.SinglePosition | EvidenceLayer.ThreatPressure |
          EvidenceLayer.StrategicMechanism =>
        true
      case _ =>
        false

  private def transitionLayer(layer: EvidenceLayer): Boolean =
    layer match
      case EvidenceLayer.MoveTransition | EvidenceLayer.StructuralDelta | EvidenceLayer.StrategicMechanism =>
        true
      case _ =>
        false

  private def afterPositionLayer(layer: EvidenceLayer): Boolean =
    layer match
      case EvidenceLayer.Board | EvidenceLayer.SinglePosition | EvidenceLayer.ThreatPressure |
          EvidenceLayer.StrategicMechanism =>
        true
      case _ =>
        false
