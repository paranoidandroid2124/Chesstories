package lila.chessjudgment.analysis.assembly

import chess.Color
import lila.chessjudgment.model.evaluation.{ JudgmentThresholds, PerspectiveMath }
import lila.chessjudgment.model.judgment.CandidateSetType
import lila.chessjudgment.analysis.transition.TransitionFactNormalizer
import lila.chessjudgment.model.judgment.*

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

  private final case class RelativeAssemblyInputs(
      input: NormalizedMoveReviewInput,
      played: MoveTransitionEdge,
      referenceTransition: MoveTransitionEdge,
      reference: CandidateLineNode,
      candidate: CandidateLineNode,
      root: PositionNodeRef,
      mover: Color,
      allocator: JudgmentProvenanceAllocator
  )

  def assemble(raw: RawMoveReviewInput): Option[JudgmentAssemblyContext] =
    EvidenceFactAssembler.assemble(raw).map(enrich)

  def enrich(context: JudgmentAssemblyContext): JudgmentAssemblyContext =
    enrichCauses(enrichFacts(context))

  def enrichFacts(context: JudgmentAssemblyContext): JudgmentAssemblyContext =
    relativeAssemblyInputs(context)
      .map { inputs =>
        val candidateSet = candidateSetDescriptor(inputs.mover, context.lines, inputs.reference)
        val comparison =
          compare(
            inputs.mover,
            inputs.reference,
            inputs.candidate,
            candidateSet.map(_.candidateSetType)
          )
        val comparisonRecords =
          candidateComparisonRecords(
            mover = inputs.mover,
            context = context,
            reference = inputs.reference,
            candidate = inputs.candidate,
            primaryComparison = comparison,
            candidateSet = candidateSet,
            root = inputs.root,
            allocator = inputs.allocator
          )
        requiredPrimaryComparisonRecord(comparisonRecords, inputs.reference, inputs.candidate)
        context.withEvidence(comparisonRecords)
      }
      .getOrElse(context)

  def enrichCauses(context: JudgmentAssemblyContext): JudgmentAssemblyContext =
    relativeAssemblyInputs(context)
      .map { inputs =>
        val comparisonRecords = assembledComparisonRecords(context, inputs.root)
        val primaryComparisonRecord =
          requiredPrimaryComparisonRecord(comparisonRecords, inputs.reference, inputs.candidate)
        val strategicContrastRecords =
          comparisonRecords.flatMap(record =>
            strategicMechanismContrastRecords(context, inputs.root, inputs.allocator, record)
          )
        val causeContext = context.withEvidence(strategicContrastRecords)
        val rawCauseRecords =
          comparisonRecords.flatMap(record =>
            relativeCauseRecords(inputs.input, causeContext, inputs.root, inputs.allocator, record)
          )
        val causeRecords = canonicalizeRelativeCauseRecords(rawCauseRecords, causeContext.evidenceGraph)
        val playedMoveSet = Set(JudgmentSubjectBinding.normalizeMove(inputs.played.moveUci))
        val playedCauseRecords =
          causeRecords.filter(record =>
            JudgmentSubjectBinding.primaryPlayed(
              JudgmentSubjectBinding.recordBinding(record, causeContext.evidenceGraph, playedMoveSet)
            )
          )
        val relativeEvidence =
          inputs.allocator.evidenceRef(
            suffix = s"relative-assessment:${inputs.candidate.ref.rootMove}",
            producer = EvidenceProducer.RelativeMoveProducer,
            layer = EvidenceLayer.RelativeAssessment,
            position = inputs.root,
            line = Some(inputs.candidate.ref),
            scope = EvidenceScope.Counterfactual,
            confidence = primaryComparisonRecord.ref.confidence
          )
        val assessment =
          RelativeMoveAssessment(
            played = inputs.played,
            referenceTransition = Some(inputs.referenceTransition),
            reference = inputs.reference,
            candidate = inputs.candidate,
            evidence = relativeEvidence,
            primaryComparisonEvidence = primaryComparisonRecord.ref,
            relatedComparisonEvidence = comparisonRecords.map(_.ref).filterNot(_.id == primaryComparisonRecord.ref.id),
            relativeCauseEvidence = playedCauseRecords.map(_.ref)
          )
        val assessmentRecord = TransitionFactNormalizer.fromRelativeAssessment(assessment)
        causeContext.withEvidence(causeRecords :+ assessmentRecord)
      }
      .getOrElse(context)

  private def relativeAssemblyInputs(context: JudgmentAssemblyContext): Option[RelativeAssemblyInputs] =
    val input = context.input
    for
      played <- context.playedTransition
      referenceTransition <- context.referenceTransition
      reference <- context.line(LineNodeRole.BestReference)
      candidate <- context.line(LineNodeRole.Played)
      root <- context.position(PositionNodeRole.Before).map(_.ref)
      mover <- root.sideToMove.orElse(input.sideToMove)
    yield RelativeAssemblyInputs(
      input = input,
      played = played,
      referenceTransition = referenceTransition,
      reference = reference,
      candidate = candidate,
      root = root,
      mover = mover,
      allocator = JudgmentProvenanceAllocator.forInput(input)
    )

  private def assembledComparisonRecords(
      context: JudgmentAssemblyContext,
      root: PositionNodeRef
  ): List[EvidenceRecord] =
    context.evidenceGraph.records.collect {
      case record @ EvidenceRecord(ref, CandidateComparisonEvidence(fact), _)
          if ref.producer == EvidenceProducer.RelativeMoveProducer &&
            ref.layer == EvidenceLayer.CandidateComparison &&
            ref.position == root &&
            ref.scope == EvidenceScope.Counterfactual &&
            context.lines.exists(_.ref == fact.referenceLine) &&
            context.lines.exists(_.ref == fact.candidateLine) =>
        record
    }

  private def requiredPrimaryComparisonRecord(
      records: List[EvidenceRecord],
      reference: CandidateLineNode,
      candidate: CandidateLineNode
  ): EvidenceRecord =
    records
      .find {
        case EvidenceRecord(_, CandidateComparisonEvidence(fact), _) =>
          fact.kind == CandidateComparisonKind.PlayedVsBest &&
            fact.referenceLine == reference.ref &&
            fact.candidateLine == candidate.ref
        case _ =>
          false
      }
      .getOrElse(
        throw IllegalStateException("played-vs-best candidate comparison was not assembled")
      )

  private def compare(
      mover: Color,
      reference: CandidateLineNode,
      candidate: CandidateLineNode,
      candidateSetType: Option[CandidateSetType]
  ): EvalComparison =
    EvalComparison.fromLines(mover, reference, candidate, candidateSetType)

  private def candidateSetDescriptor(
      mover: Color,
      lines: List[CandidateLineNode],
      reference: CandidateLineNode
  ): Option[CandidateSetDescriptor] =
    CandidateSetDescriptor.fromLines(mover, lines, reference)

  private def candidateComparisonRecords(
      mover: Color,
      context: JudgmentAssemblyContext,
      reference: CandidateLineNode,
      candidate: CandidateLineNode,
      primaryComparison: EvalComparison,
      candidateSet: Option[CandidateSetDescriptor],
      root: PositionNodeRef,
      allocator: JudgmentProvenanceAllocator
  ): List[EvidenceRecord] =
    val ordered = uniqueLines(context.lines)
    val second = ordered.find(_.ref.rootMove != reference.ref.rootMove)
    val candidateIsSecond =
      second.exists(line => EvidenceRef.sameMove(line.ref.rootMove, candidate.ref.rootMove))
    val alternatives =
      ordered.filter(line =>
        line.role == LineNodeRole.Alternative &&
          line.ref.rootMove != candidate.ref.rootMove &&
          line.ref.rootMove != reference.ref.rootMove
      )
    val comparisons =
      List(
        Some(
          (
            CandidateComparisonKind.PlayedVsBest,
            reference,
            candidate,
            primaryComparison,
            Option.when(candidateIsSecond)(candidateSet).flatten
          )
        ),
        second.filterNot(_ => candidateIsSecond).map(line =>
          (
            CandidateComparisonKind.BestVsSecond,
            reference,
            line,
            compare(mover, reference, line, candidateSet.map(_.candidateSetType)),
            candidateSet
          )
        )
      ).flatten ++
        alternatives.flatMap { alternative =>
          List(
            (
              CandidateComparisonKind.PlayedVsAlternative,
              alternative,
              candidate,
              compare(mover, alternative, candidate, None),
              None
            ),
          ) ++
            Option
              .unless(second.exists(line => EvidenceRef.sameMove(line.ref.rootMove, alternative.ref.rootMove)))(
                (
                  CandidateComparisonKind.ReferenceVsAlternative,
                  reference,
                  alternative,
                  compare(mover, reference, alternative, None),
                  None
                )
              )
              .toList
        }
    val descriptorParents =
      ordered.take(3).flatMap(parentsForLine(context, _)).distinctBy(_.id)
    val records = comparisons
      .distinctBy { case (kind, ref, cand, _, _) => (kind, ref.ref.id, cand.ref.id) }
      .zipWithIndex
      .map { case ((kind, refLine, candLine, cmp, descriptor), index) =>
        TransitionFactNormalizer.fromCandidateComparison(
          id = allocator.evidenceId(
            s"candidate-comparison:${allocator.key(kind)}:${allocator.key(refLine.ref.rootMove)}:${allocator.key(candLine.ref.rootMove)}:$index"
          ),
          comparison = CandidateComparisonFact(
            kind = kind,
            referenceLine = refLine.ref,
            candidateLine = candLine.ref,
            comparison = cmp,
            candidateSet = descriptor
          ),
          position = root,
          scope = EvidenceScope.Counterfactual,
          confidence = comparisonConfidence(refLine, candLine),
          parents = (
            parentsForLine(context, refLine) ++
              parentsForLine(context, candLine) ++
              Option.when(descriptor.nonEmpty)(descriptorParents).toList.flatten
          ).distinctBy(_.id)
        )
      }
    val candidateSetParent =
      records.collectFirst {
        case EvidenceRecord(ref, CandidateComparisonEvidence(fact), _)
            if fact.candidateSet.nonEmpty =>
          ref
      }
    records.map {
      case record @ EvidenceRecord(_, CandidateComparisonEvidence(fact), parents)
          if fact.kind == CandidateComparisonKind.PlayedVsBest && fact.candidateSet.isEmpty =>
        record.copy(parents = (parents ++ candidateSetParent).distinctBy(_.id))
      case record =>
        record
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
        val payload = StrategicMechanismContrastEvidence(
          comparisonKind = fact.kind,
          referenceLine = fact.referenceLine,
          candidateLine = fact.candidateLine,
          axisComparisons = axisComparisons,
          planComparison = planComparison,
          sustainability = sustainability,
          support = support
        )
        Option
          .when(
            fact.hasDistinctRootMoves && payload.hasActionableContrast
          ) {
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
              payload = payload,
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
    val sustainedAxisKeys =
      axisComparisons
        .filter(_.hasContrast)
        .filter(strategicAxisHasPersistenceWitness(context, _))
        .map(_.axisKey)
        .distinct
        .sorted
    StrategicSustainabilityAssessment(
      horizon = horizon,
      lineMaintained = sustainedAxisKeys.nonEmpty,
      pvMaintained = sustainedAxisKeys.nonEmpty,
      referencePlyCount = referencePlyCount,
      candidatePlyCount = candidatePlyCount,
      sustainedAxisKeys = sustainedAxisKeys
    )

  private def strategicAxisHasPersistenceWitness(
      context: JudgmentAssemblyContext,
      comparison: StrategicAxisComparison
  ): Boolean =
    val graph = context.evidenceGraph
    val axisLeafRefs = graph.strategicAxisLeafSourceRefs(comparison)
    val axisLeafIds = axisLeafRefs.map(_.id).toSet
    val matchingTransitionRecords = graph.records.collect {
      case record @ EvidenceRecord(_, PlanTransitionEvidence(transition), parents)
          if parents.exists(parent => axisLeafIds(parent.id)) &&
            strategicAxisTransitionMatchesPlan(comparison.axis, transition) =>
        record
    }
    (axisLeafRefs.flatMap(graph.record) ++ matchingTransitionRecords)
      .distinctBy(_.ref.id)
      .exists {
        case EvidenceRecord(_, payload: PlanCausalEventEvidence, _) =>
          strategicAxisEventMatchesPlan(comparison.axis, payload) &&
            payload.representativeResult.exists { case (sourceEvent, consequence) =>
              sourceEvent.step.ply > payload.causalEpisode.root.step.ply &&
                payload.resultAdvancesGoal(sourceEvent, consequence)
            }
        case EvidenceRecord(_, PlanTransitionEvidence(transition), _) =>
          transition.continuity.exists(_.consecutivePlies >= 2)
        case _ =>
          false
      }

  private def strategicAxisEventMatchesPlan(
      axis: StrategicAxisDetail,
      event: PlanCausalEventEvidence
  ): Boolean =
    axis.kind != StrategicAxisKind.PlanCoherence || event.planId.id == axis.label

  private def strategicAxisTransitionMatchesPlan(
      axis: StrategicAxisDetail,
      transition: lila.chessjudgment.model.PlanSequenceSummary
  ): Boolean =
    axis.kind != StrategicAxisKind.PlanCoherence ||
      (transition.primaryPlanId.toList ++ transition.previousPlanId.toList).exists(_.id == axis.label)

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
    CandidateSetDescriptor.uniqueLines(lines)

  private def relativeCauseRecords(
      input: NormalizedMoveReviewInput,
      context: JudgmentAssemblyContext,
      root: PositionNodeRef,
      allocator: JudgmentProvenanceAllocator,
      comparisonRecord: EvidenceRecord
  ): List[EvidenceRecord] =
    comparisonRecord.payload match
      case CandidateComparisonEvidence(fact) =>
        val threatLineOwners = threatBranchLineOwners(input, context)
        val neighborhood = comparisonNeighborhood(context, root, fact, Some(input))
        val comparisonProof = comparisonProofRecords(neighborhood)
        val causes =
          mergeCauseCandidates(
            inferCauseCandidates(neighborhood, fact, context.evidenceGraph.candidateSetFor(fact))
          )
        causes.zipWithIndex.flatMap { case (candidate, index) =>
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
          val attributionKind = effectiveAttributionKind(candidate, binding)
          val rawProofRecords =
            relativeCauseProofRecords(
              context.evidenceGraph,
              fact,
              kind,
              binding,
              attributionKind,
              support,
              comparisonProof,
              neighborhood,
              threatLineOwners
            )
          val attribution =
            causeAttribution(candidate, binding, rawProofRecords, threatLineOwners)
          val proofRecords =
            if attribution.directProofEligible then rawProofRecords
            else
              rawProofRecords.copy(
                directProof = Nil,
                contextSupport = (rawProofRecords.directProof ++ rawProofRecords.contextSupport).distinctBy(_.ref.id)
              )
          val proof = relativeCauseProof(
            context.evidenceGraph,
            fact,
            kind,
            binding,
            attributionKind,
            proofRecords.directProof,
            proofRecords.contrastProof,
            proofRecords.contextSupport
          )
          val retainedProof =
            Some(proof).filter(proof =>
              (attribution.directProofEligible && context.evidenceGraph.relativeCauseProofHasRawTypedDepth(kind, proof)) ||
                context.evidenceGraph.relativeCauseProofHasRawContextSupport(kind, proof)
            )
          val causeSupportRefs = supportRefs
          val cause =
            RelativeCauseFact(
              kind = kind,
              comparisonEvidence = comparisonRecord.ref,
              supportEvidence = causeSupportRefs,
              sourceSide = binding.sourceSide,
              attribution = attribution,
              proof = retainedProof
            )
          val causeDepthAdmissible =
            if cause.strategicCauseKind then cause.hasOwnedAdmissibleLongTermProof(context.evidenceGraph)
            else cause.hasOwnedTypedDepth(context.evidenceGraph)
          val sourceAttributionAdmissible =
            RelativeCauseKind.sourceAttributionCompatible(kind, cause.sourceSide, cause.attribution.kind)
          val causalMechanismAdmissible =
            cause.kind != RelativeCauseKind.PlanContradiction ||
              context.evidenceGraph
                .relativeCausePlanCausalEventRefs(cause)
                .flatMap(context.evidenceGraph.record)
                .exists {
                  case EvidenceRecord(_, event: PlanCausalEventEvidence, _) => event.allCausalResultsRefuted
                  case _                                                    => false
                }
          Option.when(
            (!attribution.contextOnly || context.evidenceGraph.relativeCauseProofHasRawDirectProof(kind, proof)) &&
              causeDepthAdmissible &&
              sourceAttributionAdmissible &&
              causalMechanismAdmissible
          ) {
            TransitionFactNormalizer.fromRelativeCause(
              id = allocator.evidenceId(
                s"relative-cause:${allocator.key(fact.kind)}:${allocator.key(kind)}:${allocator.key(fact.referenceLine.rootMove)}:${allocator.key(fact.candidateLine.rootMove)}:$index"
              ),
              cause = cause,
              binding = binding,
              position = root,
              scope = EvidenceScope.Counterfactual,
              confidence = comparisonConfidence(context, fact),
              parents = (comparisonRecord.ref :: (supportRefs ++ proofRecords.all.map(_.ref))).distinctBy(_.id)
            )
          }
        }
      case _ => Nil

  private def relativeCauseProofRecords(
      graph: TypedEvidenceGraph,
      fact: CandidateComparisonFact,
      kind: RelativeCauseKind,
      binding: RelativeCauseBinding,
      attributionKind: CauseAttributionKind,
      support: List[EvidenceRecord],
      comparisonProof: List[EvidenceRecord],
      neighborhood: ComparisonEvidenceNeighborhood,
      threatLineOwners: Map[String, String]
  ): RelativeCauseProofRecords =
    val intrinsicProof =
      if kind == RelativeCauseKind.OnlyMoveNecessity then
        graph.records.collect {
          case record @ EvidenceRecord(_, CandidateComparisonEvidence(candidateFact), _)
              if candidateFact == fact && candidateFact.candidateSet.exists(_.onlyMove) =>
            record
        }
      else Nil
    val proofSupport = (support ++ intrinsicProof).distinctBy(_.ref.id)
    val supportIds = supportClosureRecordIds(graph, proofSupport)
    val directSeed =
      (proofSupport ++ comparisonProof.filter(record => supportIds.contains(record.ref.id)))
        .filter(record => directProofSource(graph, fact, kind, binding, attributionKind, record, threatLineOwners))
        .distinctBy(_.ref.id)
    val ownedDirectRecords =
      proofSectionRecords(graph, directSeed, Some(kind), Some(binding))
        .filter(record => directProofSource(graph, fact, kind, binding, attributionKind, record, threatLineOwners))
        .filter(record => supportIds.contains(record.ref.id))
        .distinctBy(_.ref.id)
    val relationSupportRecords =
      (
        support ++ graph.records.collect {
          case record @ EvidenceRecord(_, relation: RelationFactEvidence, _)
              if relation.hasConcreteRelationProof && relationReferencesEventLine(record, relation, binding.eventLine) =>
            record
        }
      ).distinctBy(_.ref.id)
    val directRelationSupportRecords =
      if !ownedDirectRecords.exists(record => relationSupportDirectProofCarrier(kind, record)) then Nil
      else
        relationSupportRecords.collect {
          case record @ EvidenceRecord(_, relation: RelationFactEvidence, _)
              if relation.hasConcreteRelationProof &&
                relationCanDirectlyProveCause(kind, relation) &&
                relationSupportOverlapsDirectProof(kind, relation, ownedDirectRecords, binding.eventLine.rootMove) =>
            record
        }.distinctBy(_.ref.id)
    val directRecords = (ownedDirectRecords ++ directRelationSupportRecords).distinctBy(_.ref.id)
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
      threatLineOwners: Map[String, String]
  ): CauseAttribution =
    val ownedRefs = proofRecords.directProof.map(_.ref).distinctBy(_.id)
    val rootMatched =
      ownedRefs.nonEmpty &&
        proofRecords.directProof.exists(record =>
          recordMatchesEventRoot(record, binding.eventLine.rootMove) ||
            threatBranchRecordOwnsRoot(record, binding.eventLine.rootMove, threatLineOwners)
        )
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
    CauseAttribution(
      kind = finalKind,
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
    else RelativeCauseKind.defaultAttributionKind(draft.kind, Some(binding.sourceSide))

  private def relativeCauseProof(
      graph: TypedEvidenceGraph,
      fact: CandidateComparisonFact,
      kind: RelativeCauseKind,
      binding: RelativeCauseBinding,
      attributionKind: CauseAttributionKind,
      directRecords: List[EvidenceRecord],
      contrastRecords: List[EvidenceRecord],
      contextRecords: List[EvidenceRecord]
  ): RelativeCauseProof =
    RelativeCauseProof(
      directProof = relativeCauseProofSection(
        role = RelativeCauseProofRole.DirectProof,
        strength = RelativeCauseProofStrength.Primary,
        fact = fact,
        kind = kind,
        binding = binding,
        attributionKind = attributionKind,
        graph = graph,
        records = directRecords,
        includeContextLayers = false
      ),
      contrastProof = relativeCauseProofSection(
        role = RelativeCauseProofRole.ContrastProof,
        strength = RelativeCauseProofStrength.Supporting,
        fact = fact,
        kind = kind,
        binding = binding,
        attributionKind = attributionKind,
        graph = graph,
        records = contrastRecords,
        includeContextLayers = false
      ),
      contextSupport = relativeCauseProofSection(
        role = RelativeCauseProofRole.ContextSupport,
        strength = RelativeCauseProofStrength.WeakHint,
        fact = fact,
        kind = kind,
        binding = binding,
        attributionKind = attributionKind,
        graph = graph,
        records = contextRecords,
        includeContextLayers = true
      )
    )

  private def relativeCauseProofSection(
      role: RelativeCauseProofRole,
      strength: RelativeCauseProofStrength,
      fact: CandidateComparisonFact,
      kind: RelativeCauseKind,
      binding: RelativeCauseBinding,
      attributionKind: CauseAttributionKind,
      graph: TypedEvidenceGraph,
      records: List[EvidenceRecord],
      includeContextLayers: Boolean
  ): RelativeCauseProofSection =
    val proofRecords = records.distinctBy(_.ref.id)
    val structuralProofSourceIds =
      proofRecords.collect {
        case EvidenceRecord(ref, structural: StructuralDeltaEvidence, _)
            if RelativeCauseKind.structuralConsequences(kind, structural).nonEmpty &&
              strategicStructuralProofReady(kind, structural) =>
          ref.id
      }.toSet
    val typedProofSources =
      proofRecords.filter {
        case EvidenceRecord(_, payload: BoardFactEvidence, _) =>
          payload.proofSignalAnchorKinds.nonEmpty
        case record @ EvidenceRecord(_, payload: LineFactEvidence, _) =>
          lineFactDirectlyOwnsCause(
            graph,
            fact,
            kind,
            binding,
            attributionKind,
            record,
            payload
          )
        case EvidenceRecord(_, payload: RelationFactEvidence, _) =>
          payload.hasConcreteRelationProof
        case record @ EvidenceRecord(_, payload: TacticalMechanismEvidence, _) =>
          payload.hasConcreteProof &&
            tacticalMechanismDirectionMatchesCause(kind, payload) &&
            tacticalMechanismDirectlyOwnsRoot(
              graph,
              fact,
              kind,
              binding,
              attributionKind,
              record
            )
        case EvidenceRecord(_, payload: StrategicMechanismEvidence, _) =>
          if kind == RelativeCauseKind.SacrificeCompensation then
            payload.canSupportCompensation
          else
            payload.canSupportStrategicCause &&
              strategicMechanismProofSignals(graph, kind, payload, binding.sourceSide, structuralProofSourceIds).nonEmpty
        case EvidenceRecord(_, payload: StrategicMechanismContrastEvidence, _) =>
          strategicContrastCanDirectlyProveCause(kind, payload, binding.sourceSide, graph)
        case EvidenceRecord(_, payload: ThreatEpisodeEvidence, _) =>
          payload.isProofSignalDefensivePressure
        case EvidenceRecord(_, payload: StructuralDeltaEvidence, _) =>
          RelativeCauseKind.structuralConsequences(kind, payload).nonEmpty
        case EvidenceRecord(_, CandidateComparisonEvidence(fact), _) =>
          kind == RelativeCauseKind.OnlyMoveNecessity && fact.candidateSet.exists(_.onlyMove)
        case _ =>
          false
      }
    RelativeCauseProofSection(
      role = role,
      strength = strength,
      sourceRefs =
        (if includeContextLayers then proofRecords else typedProofSources)
          .map(_.ref)
          .distinctBy(_.id)
    )

  private def proofSectionRecords(
      graph: TypedEvidenceGraph,
      records: List[EvidenceRecord],
      kind: Option[RelativeCauseKind] = None,
      binding: Option[RelativeCauseBinding] = None
  ): List[EvidenceRecord] =
    val mechanismParents =
      records.collect {
        case record @ EvidenceRecord(_, payload: TacticalMechanismEvidence, _) if payload.hasConcreteProof =>
          graph.parentClosure(record).filter(record => proofSourceLayer(record.ref.layer))
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
    val relationLineParents =
      records.collect {
        case record @ EvidenceRecord(_, payload: RelationFactEvidence, _) if payload.hasConcreteRelationProof =>
          graph.parentClosure(record).filter(_.ref.layer == EvidenceLayer.Line)
      }.flatten
    (records ++ mechanismParents ++ strategicStructuralParents ++ relationLineParents).distinctBy(_.ref.id)

  private def proofSectionRecordIds(graph: TypedEvidenceGraph, records: List[EvidenceRecord]): Set[String] =
    proofSectionRecords(graph, records).map(_.ref.id).toSet

  private def supportClosureRecordIds(graph: TypedEvidenceGraph, records: List[EvidenceRecord]): Set[String] =
    records
      .flatMap(record => record :: graph.parentClosure(record))
      .map(_.ref.id)
      .toSet

  private def relationSupportDirectProofCarrier(kind: RelativeCauseKind, record: EvidenceRecord): Boolean =
    record.payload match
      case payload: LineFactEvidence =>
        payload.hasTacticalLineConsequence || payload.hasProofSignalMaterialEvent
      case payload: TacticalMechanismEvidence =>
        payload.hasConcreteProof && payload.tactical
      case payload: StrategicMechanismEvidence =>
        relationSupportStrategicCause(kind) && payload.canSupportStrategicCause
      case payload: StructuralDeltaEvidence =>
        relationSupportStrategicCause(kind) &&
          RelativeCauseKind.structuralConsequences(kind, payload).exists(consequenceHasConcreteStrategicTarget)
      case _ =>
        false

  private def relationSupportOverlapsDirectProof(
      kind: RelativeCauseKind,
      relation: RelationFactEvidence,
      directRecords: List[EvidenceRecord],
      rootMove: String
  ): Boolean =
    val normalizedRoot = normalizeMove(rootMove)
    val rootDestination =
      Option.when(normalizedRoot.length >= 4)(normalizedRoot.slice(2, 4)).toList
    val relationSquares =
      (
        relation.focusSquares.map(_.key) ++
          relation.targetSquare.map(_.key).toList ++
          relation.participants.map(_.square.key)
      ).map(_.toLowerCase).toSet
    val proofSquares =
      (
        rootDestination ++
          directRecords.flatMap {
            case EvidenceRecord(_, line: LineFactEvidence, _) =>
              line.eventsForRootMove(rootMove).flatMap(_.square.map(_.key)) ++
                line.materialCaptures
                  .filter(capture => EvidenceRef.sameMove(capture.moveUci, rootMove))
                  .map(_.square.key)
            case EvidenceRecord(_, mechanism: TacticalMechanismEvidence, _) =>
              mechanism.moveUci.toList.flatMap(move =>
                val normalized = normalizeMove(move)
                Option.when(normalized.length >= 4)(normalized.slice(2, 4))
              )
            case EvidenceRecord(_, structural: StructuralDeltaEvidence, _) =>
              RelativeCauseKind.structuralConsequences(kind, structural)
                .flatMap(_.subjects)
                .flatMap(subject => "[a-h][1-8]".r.findAllIn(Option(subject).getOrElse("").toLowerCase).toList)
            case _ =>
              Nil
          }
      ).map(_.toLowerCase).toSet
    relationSquares.intersect(proofSquares).nonEmpty

  private def relationSupportStrategicCause(kind: RelativeCauseKind): Boolean =
    kind == RelativeCauseKind.TargetPressureGain ||
      kind == RelativeCauseKind.PawnWeaknessTarget

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
      attributionKind: CauseAttributionKind,
      record: EvidenceRecord,
      threatLineOwners: Map[String, String]
  ): Boolean =
    val rootMove = binding.eventLine.rootMove
    val rootRecaptureCause =
      kind == RelativeCauseKind.WrongRecapturer ||
        kind == RelativeCauseKind.RecaptureRecoveryWindow
    val verifiedRootRecapture =
      !rootRecaptureCause ||
        (record :: graph.parentClosure(record)).exists {
          case EvidenceRecord(_, payload: LineFactEvidence, _) => payload.rootIsRecapture(rootMove)
          case _                                                => false
        }
    verifiedRootRecapture &&
      (record.payload match
        case payload: LineFactEvidence =>
          if threatBranchRecordOwnsRoot(record, rootMove, threatLineOwners) then threatBranchLineFactCanProveCause(kind, payload)
          else
            record.referencesLine(binding.eventLine) &&
              lineFactDirectlyOwnsCause(
                graph,
                fact,
                kind,
                binding,
                attributionKind,
                record,
                payload
              )
        case payload: TacticalMechanismEvidence =>
          tacticalMechanismDirectionMatchesCause(kind, payload) &&
            (if defensiveCause(kind) then payload.canAnchorDefensiveClaim else payload.canAnchorTacticalClaim) &&
            (
              threatBranchRecordOwnsRoot(record, rootMove, threatLineOwners) ||
                (
                  record.referencesLine(binding.eventLine) &&
                    tacticalMechanismDirectlyOwnsRoot(
                      graph,
                      fact,
                      kind,
                      binding,
                      attributionKind,
                      record
                    )
                )
            )
        case payload: RelationFactEvidence =>
          payload.hasConcreteRelationProof &&
            payload.hasLineProof &&
            (
              payload.kind != RelationFactKind.PerpetualCheck ||
                record.ref.confidence == EvidenceConfidence.LegalReplayVerified
            ) &&
            (payload.lineProofCount > 1 || (payload.hasParticipantProof && payload.mentionsLineMove(rootMove))) &&
            (threatBranchRecordOwnsRoot(record, rootMove, threatLineOwners) || relationReferencesEventLine(record, payload, binding.eventLine)) &&
            relationCanDirectlyProveCause(kind, payload)
        case payload: ThreatEpisodeEvidence =>
          val ownsDefensiveCause =
            defensiveCause(kind) &&
              payload.isProofSignalDefensivePressure &&
              (
                threatBranchRecordOwnsRoot(record, rootMove, threatLineOwners) ||
                  record.referencesLine(binding.eventLine) ||
                  threatEpisodeOwnsDefensiveCause(payload, kind, rootMove)
              )
          val ownsMateThreat =
            kind == RelativeCauseKind.KingForcing &&
              RelativeCauseSignalProfile.currentMoveMateThreatRecord(record) &&
              (
                threatBranchRecordOwnsRoot(record, rootMove, threatLineOwners) ||
                  record.referencesLine(binding.eventLine)
              )
          ownsDefensiveCause || ownsMateThreat
        case payload: StructuralDeltaEvidence =>
          record.referencesLine(binding.eventLine) &&
            payload.line.contains(binding.eventLine) &&
            normalizeMove(payload.moveUci) == normalizeMove(rootMove) &&
            RelativeCauseKind.structuralConsequences(kind, payload).nonEmpty
        case payload: StrategicMechanismEvidence =>
          record.referencesLine(binding.eventLine) &&
            (
              (
                kind == RelativeCauseKind.SacrificeCompensation &&
                  payload.canSupportCompensation &&
                  eventLineHasRootSacrifice(
                    graph,
                    binding.eventLine,
                    rootMove
                  )
              ) ||
                (
                  strategicCause(kind) &&
                    strategicMechanismDirectlyOwnsCause(
                      graph,
                      record,
                      kind,
                      binding.eventLine.rootMove,
                      binding.sourceSide
                    )
                )
            )
        case payload: StrategicMechanismContrastEvidence =>
          strategicCause(kind) &&
            payload.comparisonKind == fact.kind &&
            payload.referenceLine == fact.referenceLine &&
            payload.candidateLine == fact.candidateLine &&
            strategicContrastCanDirectlyProveCause(
              kind,
              payload,
              binding.sourceSide,
              graph,
              Some(binding.eventLine)
            )
        case CandidateComparisonEvidence(candidateFact) =>
          kind == RelativeCauseKind.OnlyMoveNecessity &&
            candidateFact == fact &&
            candidateFact.candidateSet.exists(_.onlyMove) &&
            binding.sourceSide == RelativeCauseSourceSide.Reference &&
            candidateFact.referenceLine == binding.eventLine
        case _ =>
          false)

  private def lineFactDirectlyOwnsCause(
      graph: TypedEvidenceGraph,
      fact: CandidateComparisonFact,
      kind: RelativeCauseKind,
      binding: RelativeCauseBinding,
      attributionKind: CauseAttributionKind,
      record: EvidenceRecord,
      payload: LineFactEvidence,
  ): Boolean =
    val rootMove = binding.eventLine.rootMove
    val ownedConsequences =
      graph
        .ownedLineConsequences(fact, binding.sourceSide, attributionKind)
        .collect { case (ref, consequence) if ref.id == record.ref.id => consequence }
    lineFactDirectlyOwnsCause(kind, payload, rootMove, ownedConsequences)

  private def lineFactDirectlyOwnsCause(
      kind: RelativeCauseKind,
      payload: LineFactEvidence,
      rootMove: String,
      ownedConsequences: List[LineConsequence]
  ): Boolean =
    val rootMoveConsequences = ownedConsequences.filter(_.rootMoveMatched(rootMove))
    kind match
      case RelativeCauseKind.WrongRecapturer | RelativeCauseKind.RecaptureRecoveryWindow =>
        payload.rootIsRecapture(rootMove) &&
          rootMoveConsequences.exists(consequence =>
            consequence.kind == LineConsequenceKind.RecaptureSequence ||
              consequence.kind == LineConsequenceKind.RecoveryWindow
          )
      case RelativeCauseKind.WrongMoveOrder =>
        payload.eventsForRootMove(rootMove).exists(_.kind == LineEventKind.Tempo)
      case RelativeCauseKind.TempoLoss =>
        ownedConsequences.exists(_.kind == LineConsequenceKind.ImmediateReplyCheck)
      case RelativeCauseKind.KingForcing =>
        ownedConsequences.exists(_.kind == LineConsequenceKind.Mate) ||
          payload.eventsForRootMove(rootMove).exists(event =>
            event.kind == LineEventKind.Check || event.kind == LineEventKind.Mate
          )
      case RelativeCauseKind.DrawResource =>
        ownedConsequences.exists(_.kind == LineConsequenceKind.DrawResource) ||
          payload.endgameTechniquesTriggeredByRootMove(rootMove, kind).nonEmpty
      case RelativeCauseKind.MaterialSwing =>
        ownedConsequences.exists(consequence =>
          consequence.kind == LineConsequenceKind.MaterialGain ||
            consequence.kind == LineConsequenceKind.MaterialLoss
        )
      case RelativeCauseKind.SacrificeCompensation =>
        false
      case RelativeCauseKind.ConversionMiss | RelativeCauseKind.ConversionSecured =>
        ownedConsequences.exists(consequence =>
          consequence.kind == LineConsequenceKind.RecaptureSequence ||
            consequence.kind == LineConsequenceKind.RecoveryWindow ||
            consequence.kind == LineConsequenceKind.Promotion ||
            consequence.kind == LineConsequenceKind.PromotionRace
        ) ||
          payload.endgameTechniquesTriggeredByRootMove(rootMove, kind).nonEmpty ||
          (
            kind == RelativeCauseKind.ConversionSecured &&
              payload.eventsForRootMove(rootMove).exists(_.kind == LineEventKind.DefenderMove)
          )
      case RelativeCauseKind.MissedTacticalResource | RelativeCauseKind.TacticalRefutationOfPlayed |
          RelativeCauseKind.CandidateTacticalLiability =>
        ownedConsequences.exists(consequence =>
          RelativeCauseKind.acceptsDirectLineConsequence(kind, payload, rootMove, consequence) &&
            LineConsequenceKind.tacticalDriver(consequence.kind)
        ) ||
          payload.eventsForRootMove(rootMove).exists(event =>
            event.kind == LineEventKind.Capture ||
              event.kind == LineEventKind.Recapture ||
              event.kind == LineEventKind.Check ||
              event.kind == LineEventKind.Mate ||
              event.kind == LineEventKind.Promotion
          )
      case _ =>
        false

  private def eventLineHasRootSacrifice(
      graph: TypedEvidenceGraph,
      eventLine: LineNodeRef,
      rootMove: String
  ): Boolean =
    graph.records.exists {
      case EvidenceRecord(ref, payload: LineFactEvidence, _)
          if ref.line.contains(eventLine) =>
        payload
          .consequencesForRootMove(rootMove)
          .exists(_.kind == LineConsequenceKind.Sacrifice)
      case _ =>
        false
    }

  private def threatBranchLineFactCanProveCause(
      kind: RelativeCauseKind,
      payload: LineFactEvidence
  ): Boolean =
    payload.proofSignalConsequences.exists(consequence =>
      RelativeCauseKind.acceptsLineConsequence(kind, consequence.kind)
    ) ||
      (
        tacticalCause(kind) &&
          (payload.hasTacticalLineConsequence || payload.hasProofSignalMaterialEvent)
      )

  private def tacticalMechanismDirectlyOwnsRoot(
      graph: TypedEvidenceGraph,
      fact: CandidateComparisonFact,
      kind: RelativeCauseKind,
      binding: RelativeCauseBinding,
      attributionKind: CauseAttributionKind,
      record: EvidenceRecord
  ): Boolean =
    record.payload match
      case payload: TacticalMechanismEvidence =>
        val rootMove = binding.eventLine.rootMove
        val signalsBySourceId =
          payload.signals.flatMap(signal => signal.source.map(_.id -> signal)).groupMap(_._1)(_._2)
        val consequenceLabelsBySourceId = payload.lineConsequenceSourceLabelsByEvidenceId
        val ownedConsequences =
          graph.ownedLineConsequences(
            fact,
            binding.sourceSide,
            attributionKind,
            consequenceLabelsBySourceId
          )
        signalsBySourceId.exists { case (sourceId, signals) =>
          graph.byId.get(sourceId).exists {
            case EvidenceRecord(ref, motif: MoveMotifEvidence, _) =>
              signals.exists(_.kind == TacticalMechanismSignalKind.Motif) &&
                motif.recordLineBound(ref) &&
                normalizeMove(motif.moveUci) == normalizeMove(rootMove)
            case EvidenceRecord(_, line: LineFactEvidence, _) =>
              val consequenceSignals = signals.filter(_.kind == TacticalMechanismSignalKind.LineConsequence)
              val eventSignals = signals.filter(_.kind == TacticalMechanismSignalKind.LineEvent)
              val sourceConsequences = ownedConsequences.collect {
                case (ref, consequence) if ref.id == sourceId => consequence
              }
              val consequenceOwned =
                consequenceSignals.nonEmpty &&
                  lineFactDirectlyOwnsCause(kind, line, rootMove, sourceConsequences)
              val eventOwned =
                eventSignals.nonEmpty &&
                  lineFactDirectlyOwnsCause(kind, line, rootMove, Nil)
              consequenceOwned || eventOwned
            case EvidenceRecord(_, relation: RelationFactEvidence, _) =>
              signals.exists(_.kind == TacticalMechanismSignalKind.Relation) &&
                relation.hasConcreteRelationProof &&
                relation.mentionsLineMove(rootMove) &&
                relationCanDirectlyProveCause(kind, relation)
            case EvidenceRecord(_, threat: ThreatEpisodeEvidence, _) =>
              signals.exists(_.kind == TacticalMechanismSignalKind.ThreatEpisode) &&
                threat.isProofSignalDefensivePressure &&
                threatEpisodeOwnsDefensiveCause(threat, kind, rootMove)
            case EvidenceRecord(ref, EvalFactEvidence(_, _, mate, _), _) =>
              signals.exists(_.kind == TacticalMechanismSignalKind.MateBranch) &&
                kind == RelativeCauseKind.KingForcing &&
                mate.nonEmpty &&
                ref.line.contains(binding.eventLine)
            case _ =>
              false
          }
        }
      case _ =>
        false

  private def tacticalMechanismDirectionMatchesCause(
      kind: RelativeCauseKind,
      payload: TacticalMechanismEvidence
  ): Boolean =
    kind match
      case RelativeCauseKind.TempoLoss =>
        payload.kind == TacticalMechanismKind.Tempo
      case RelativeCauseKind.WrongMoveOrder if payload.kind == TacticalMechanismKind.Tempo =>
        payload.hasMoverZwischenzug
      case RelativeCauseKind.TacticalRefutationOfPlayed | RelativeCauseKind.CandidateTacticalLiability
          if payload.kind == TacticalMechanismKind.RecaptureChoice =>
        false
      case _ =>
        true

  private def threatEpisodeOwnsDefensiveCause(
      payload: ThreatEpisodeEvidence,
      kind: RelativeCauseKind,
      rootMove: String
  ): Boolean =
    payload.onlyDefense.exists(move => normalizeMove(move) == normalizeMove(rootMove)) ||
      (
        kind == RelativeCauseKind.DefensiveResource &&
          payload.episode.bestDefense.exists(move => normalizeMove(move) == normalizeMove(rootMove))
      )

  private def relationCanDirectlyProveCause(kind: RelativeCauseKind, payload: RelationFactEvidence): Boolean =
    kind match
      case RelativeCauseKind.MissedTacticalResource | RelativeCauseKind.TacticalRefutationOfPlayed |
          RelativeCauseKind.CandidateTacticalLiability =>
        payload.hasConcreteRelationProof
      case RelativeCauseKind.WrongRecapturer | RelativeCauseKind.RecaptureRecoveryWindow =>
        payload.hasConcreteRelationProof
      case RelativeCauseKind.WrongMoveOrder =>
        payload.kind == RelationFactKind.Zwischenzug && payload.hasConcreteRelationProof
      case RelativeCauseKind.TempoLoss =>
        false
      case RelativeCauseKind.MaterialSwing =>
        payload.kind == RelationFactKind.HangingPiece ||
          payload.kind == RelationFactKind.TrappedPiece ||
          payload.kind == RelationFactKind.Domination ||
          RelativeCauseSignalProfile.relationMaterialPayoffKind(payload.kind)
      case RelativeCauseKind.TargetPressureGain =>
        RelativeCauseSignalProfile.relationTargetPressureProofKind(payload.kind)
      case RelativeCauseKind.PawnWeaknessTarget =>
        RelativeCauseSignalProfile.relationMaterialPayoffKind(payload.kind)
      case RelativeCauseKind.ConversionMiss | RelativeCauseKind.ConversionSecured =>
        payload.kind == RelationFactKind.BadPieceLiquidation
      case RelativeCauseKind.DrawResource =>
        payload.kind == RelationFactKind.StalemateTrap ||
          payload.kind == RelationFactKind.PerpetualCheck
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
    signal.axis.exists(axis => RelativeCauseKind.strategicAxisCanProveCause(kind, axis, sourceSide)) &&
      (signal.kind match
        case StrategicMechanismSignalKind.StructuralDelta =>
          graph.byId.get(signal.source.id).exists {
            case EvidenceRecord(sourceRef, structural: StructuralDeltaEvidence, _) =>
              val provingConsequences = RelativeCauseKind.structuralConsequences(kind, structural, signal.axis)
              eventLine.forall(line => sourceRef.line.contains(line) && structural.line.contains(line)) &&
                normalizeMove(structural.moveUci) == normalizedRoot &&
                provingConsequences.nonEmpty &&
                strategicStructuralProofReady(kind, provingConsequences)
            case _ =>
              false
          }
        case StrategicMechanismSignalKind.PlanPressure =>
          graph.byId.get(signal.source.id).exists {
            case EvidenceRecord(sourceRef, event: PlanCausalEventEvidence, _)
                if sourceRef.confidence != EvidenceConfidence.Heuristic =>
              eventLine.forall(sourceRef.line.contains) &&
                normalizeMove(event.rootMove) == normalizedRoot &&
                RelativeCauseKind.planCausalEventCanProveCause(kind, event)
            case _ =>
              false
          }
        case _ =>
          false)

  private[chessjudgment] def strategicMechanismProofSignals(
      graph: TypedEvidenceGraph,
      kind: RelativeCauseKind,
      payload: StrategicMechanismEvidence,
      sourceSide: RelativeCauseSourceSide,
      selectedStructuralSourceIds: Set[String] = Set.empty
  ): List[StrategicMechanismSignal] =
    val axisMatched =
      payload.signals.filter(signal =>
        signal.axis.exists(axis => RelativeCauseKind.strategicAxisCanProveCause(kind, axis, sourceSide))
      )
    val structuralMatched =
      axisMatched.filter(signal =>
        signal.kind == StrategicMechanismSignalKind.StructuralDelta &&
          (selectedStructuralSourceIds.isEmpty || selectedStructuralSourceIds.contains(signal.source.id)) &&
          graph.byId.get(signal.source.id).exists {
            case EvidenceRecord(_, structural: StructuralDeltaEvidence, _) =>
              val provingConsequences = RelativeCauseKind.structuralConsequences(kind, structural, signal.axis)
              provingConsequences.nonEmpty &&
                strategicStructuralProofReady(kind, provingConsequences)
            case _ =>
              false
          }
      )
    if selectedStructuralSourceIds.nonEmpty then structuralMatched.distinct
    else if structuralMatched.nonEmpty then structuralMatched.distinct
    else if typedStructuralAxisCause(kind) then Nil
    else axisMatched.distinct

  private def typedStructuralAxisCause(kind: RelativeCauseKind): Boolean =
    kind == RelativeCauseKind.PawnWeaknessTarget ||
      kind == RelativeCauseKind.KingSafetyConcession ||
      kind == RelativeCauseKind.PawnBreakOpportunity

  private def strategicStructuralProofReady(
      kind: RelativeCauseKind,
      structural: StructuralDeltaEvidence
  ): Boolean =
    strategicStructuralProofReady(kind, RelativeCauseKind.structuralConsequences(kind, structural))

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
      consequence.kind == DevelopmentPieceActivated ||
        consequence.kind == DevelopmentCenterControlGain ||
        consequence.kind == OutpostGain ||
        consequence.kind == BatteryPressureGain ||
        consequence.kind == FileOccupationGain ||
        consequence.kind == FileAccessGain ||
        consequence.kind == RookLiftActivation ||
        consequence.kind == LineUnlockGain
    consequenceHasConcreteStrategicTarget(consequence) &&
      (
        routeKind ||
          (consequence.kind == MobilityGain && consequence.subjects.exists(activityGainMoveRouteSubject)) ||
          consequence.subjects.exists(activityGainQualifiedRouteSubject)
      )

  private def activityGainMoveRouteSubject(subject: String): Boolean =
    val normalized = Option(subject).getOrElse("").trim.toLowerCase
    normalized.matches(".*\\b(king|queen|rook|bishop|knight|pawn):[a-h][1-8]-[a-h][1-8].*")

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
      sourceSide: RelativeCauseSourceSide,
      graph: TypedEvidenceGraph,
      boundEventLine: Option[LineNodeRef] = None
  ): Boolean =
    val sourceEventLine = sourceSide match
      case RelativeCauseSourceSide.Reference => Some(payload.referenceLine)
      case RelativeCauseSourceSide.Candidate => Some(payload.candidateLine)
      case RelativeCauseSourceSide.Shared | RelativeCauseSourceSide.Mixed => None
    val ownedEventLine = sourceEventLine.filter(line => boundEventLine.forall(_ == line))
    def ownedPlanEventCanProveCause(comparison: StrategicAxisComparison): Boolean =
      ownedEventLine.exists(line =>
        graph
          .strategicComparisonPlanEventRefs(comparison, sourceSide, line)
          .flatMap(graph.record)
          .exists {
            case EvidenceRecord(_, event: PlanCausalEventEvidence, _) =>
              RelativeCauseKind.planCausalEventCanProveCause(kind, event)
            case _ =>
              false
          }
      )
    payload.sustainedCauseComparisons(kind, sourceSide).exists { comparison =>
      kind match
        case RelativeCauseKind.PlanImprovement | RelativeCauseKind.PlanContradiction =>
          ownedPlanEventCanProveCause(comparison)
        case RelativeCauseKind.OpponentRestriction
            if comparison.axis.label == "opponent-resource-deterrence" =>
          ownedPlanEventCanProveCause(comparison)
        case RelativeCauseKind.PawnWeaknessTarget =>
          RelativeCauseSignalProfile.comparisonHasStructuralConsequence(
            comparison,
            sourceSide,
            graph.records,
            Set(TransitionConsequenceKind.WeakPawnTargetCreated, TransitionConsequenceKind.WeakSquareTargetCreated)
          )
        case RelativeCauseKind.KingSafetyConcession =>
          RelativeCauseSignalProfile.comparisonHasStructuralConsequence(
            comparison,
            sourceSide,
            graph.records,
            Set(TransitionConsequenceKind.KingSafetyConcession, TransitionConsequenceKind.KingRingPressureConcession)
          )
        case RelativeCauseKind.PawnBreakOpportunity =>
          val consequenceKind =
            if comparison.axis.polarity == StrategicAxisPolarity.Release then
              TransitionConsequenceKind.PawnTensionResolution
            else TransitionConsequenceKind.PawnTensionGain
          RelativeCauseSignalProfile.comparisonHasStructuralConsequence(
            comparison,
            sourceSide,
            graph.records,
            Set(consequenceKind)
          )
        case _ =>
          true
    }

  private def defensiveCause(kind: RelativeCauseKind): Boolean =
    kind == RelativeCauseKind.OnlyDefenseNecessity ||
      kind == RelativeCauseKind.DefensiveResource ||
      kind == RelativeCauseKind.DrawResource ||
      kind == RelativeCauseKind.OnlyMoveNecessity

  private def tacticalCause(kind: RelativeCauseKind): Boolean =
    kind == RelativeCauseKind.MissedTacticalResource ||
      kind == RelativeCauseKind.TacticalRefutationOfPlayed ||
      kind == RelativeCauseKind.CandidateTacticalLiability ||
      kind == RelativeCauseKind.KingForcing ||
      kind == RelativeCauseKind.MaterialSwing

  private def strategicCause(kind: RelativeCauseKind): Boolean =
    RelativeCauseKind.strategicContrastBacked(kind)

  private def recordMatchesEventRoot(record: EvidenceRecord, rootMove: String): Boolean =
    record.payload match
      case payload: LineFactEvidence =>
        payload.eventsForRootMove(rootMove).nonEmpty ||
          payload.consequencesForRootMove(rootMove).nonEmpty ||
          payload.endgameTechniquesTriggeredByRootMove(rootMove, RelativeCauseKind.ConversionSecured).nonEmpty ||
          payload.endgameTechniquesTriggeredByRootMove(rootMove, RelativeCauseKind.ConversionMiss).nonEmpty ||
          payload.endgameTechniquesTriggeredByRootMove(rootMove, RelativeCauseKind.DrawResource).nonEmpty
      case payload: TacticalMechanismEvidence =>
        payload.moveUci.exists(move => normalizeMove(move) == normalizeMove(rootMove))
      case payload: StructuralDeltaEvidence =>
        normalizeMove(payload.moveUci) == normalizeMove(rootMove)
      case payload: RelationFactEvidence =>
        payload.mentionsLineMove(rootMove) ||
          record.ref.line.exists(line => normalizeMove(line.rootMove) == normalizeMove(rootMove)) ||
          record.payloadLineRefs.exists(line => normalizeMove(line.rootMove) == normalizeMove(rootMove))
      case _: StrategicMechanismEvidence | _: StrategicMechanismContrastEvidence | _: ThreatEpisodeEvidence =>
        record.ref.line.exists(line => normalizeMove(line.rootMove) == normalizeMove(rootMove)) ||
          record.payloadLineRefs.exists(line => normalizeMove(line.rootMove) == normalizeMove(rootMove))
      case CandidateComparisonEvidence(fact) =>
        normalizeMove(fact.referenceLine.rootMove) == normalizeMove(rootMove)
      case _ =>
        false

  private def relationReferencesEventLine(
      record: EvidenceRecord,
      payload: RelationFactEvidence,
      eventLine: LineNodeRef
  ): Boolean =
    record.referencesLine(eventLine) ||
      (
        payload.mentionsLineMove(eventLine.rootMove) &&
          LineNodeRole.fromMoveEvidenceScope(record.ref.scope).contains(eventLine.role)
      )

  private def threatBranchRecordOwnsRoot(
      record: EvidenceRecord,
      rootMove: String,
      threatLineOwners: Map[String, String]
  ): Boolean =
    record.ref.line.exists(line =>
      line.role == LineNodeRole.Threat &&
        threatLineOwners.get(line.id).exists(ownerMove => normalizeMove(ownerMove) == normalizeMove(rootMove))
    )

  private def normalizeMove(raw: String): String =
    Option(raw).getOrElse("").trim.toLowerCase

  private def canonicalizeRelativeCauseRecords(
      records: List[EvidenceRecord],
      graph: TypedEvidenceGraph
  ): List[EvidenceRecord] =
    val canonical = records.zipWithIndex
      .groupBy { case (record, _) =>
        relativeCauseFromRecord(record).map(cause => RelativeCauseSemanticKey.from(cause, graph))
      }
      .values
      .map(entries =>
        entries.maxBy { case (record, index) =>
          relativeCauseFromRecord(record)
            .map(cause => relativeCauseCanonicalQuality(cause, graph, index))
            .getOrElse((0, 0, 0, 0, -index))
        }
      )
      .toList
      .sortBy(_._2)
      .map(_._1)
    val withoutDuplicatePlayedLiability = suppressDuplicatePlayedAlternativeLiabilities(canonical, graph)
    withoutDuplicatePlayedLiability

  private def relativeCauseCanonicalQuality(
      cause: RelativeCauseFact,
      graph: TypedEvidenceGraph,
      index: Int
  ): (Int, Int, Int, Int, Int) =
    val bindings = EvidenceObjectBinding.fromRelativeCauseForProjection(cause, graph)
    val directProofCount = cause.proof.toList.flatMap(_.directProof.sourceRefs).map(_.id).distinct.size
    (
      bindings.count(_.specificTargetMechanismReady),
      bindings.count(_.hasConcreteObject),
      directProofCount,
      cause.supportEvidence.map(_.id).distinct.size,
      -index
    )

  private def suppressDuplicatePlayedAlternativeLiabilities(
      records: List[EvidenceRecord],
      graph: TypedEvidenceGraph
  ): List[EvidenceRecord] =
    val primaryLiabilityKeys = records.flatMap(record =>
      relativeCauseFromRecord(record).flatMap { cause =>
        graph.comparisonFor(cause).flatMap(comparison =>
          Option.when(
            comparison.kind == CandidateComparisonKind.PlayedVsBest &&
              cause.attribution.kind == CauseAttributionKind.CandidateAllowsLiability
          )(playedLiabilitySemanticKey(cause, graph))
        )
      }
    ).toSet
    records.filterNot(record =>
      relativeCauseFromRecord(record).exists { cause =>
        graph.comparisonFor(cause).exists(_.kind == CandidateComparisonKind.PlayedVsAlternative) &&
        cause.attribution.kind == CauseAttributionKind.CandidateAllowsLiability &&
        primaryLiabilityKeys.contains(playedLiabilitySemanticKey(cause, graph))
      }
    )

  private def playedLiabilitySemanticKey(cause: RelativeCauseFact, graph: TypedEvidenceGraph): String =
    val key = RelativeCauseSemanticKey.from(cause, graph)
    List(
      key.kind.toString,
      key.eventLineId,
      key.attributionKind.toString,
      key.sourceSide.toString
    ).mkString("|")

  private def relativeCauseFromRecord(record: EvidenceRecord): Option[RelativeCauseFact] =
    record.payload match
      case RelativeCauseFactEvidence(cause) => Some(cause)
      case _                                => None

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
      .filterNot(_.ref.line.exists(_.role == LineNodeRole.Threat))
      .distinctBy(_.ref.id)

  private def inferCauseCandidates(
      neighborhood: ComparisonEvidenceNeighborhood,
      fact: CandidateComparisonFact,
      candidateSet: Option[CandidateSetDescriptor]
  ): List[RelativeCauseDraft] =
    val profile =
      RelativeCauseSignalProfile.from(
        fact = fact,
        candidateSet = candidateSet,
        referenceRecords = neighborhood.referenceRecords,
        candidateRecords = neighborhood.candidateRecords,
        sharedRecords = neighborhood.sharedRecords
      )
    RelativeCauseDraftPlanner.drafts(profile)

  private def parentsForLine(
      context: JudgmentAssemblyContext,
      line: CandidateLineNode
  ): List[EvidenceRef] =
    context.evidenceGraph
      .recordsFor(line.ref)
      .collect {
        case record
            if record.ref.layer == EvidenceLayer.Line ||
              record.ref.layer == EvidenceLayer.Eval =>
          record.ref
      }

  private def comparisonNeighborhood(
      context: JudgmentAssemblyContext,
      root: PositionNodeRef,
      fact: CandidateComparisonFact,
      input: Option[NormalizedMoveReviewInput] = None
  ): ComparisonEvidenceNeighborhood =
    val referenceTransition = transitionForLine(context, fact.referenceLine)
    val candidateTransition = transitionForLine(context, fact.candidateLine)
    val referenceEndpoint =
      (recordsForLineEndpoint(context, fact.referenceLine) ++ input.toList.flatMap(recordsForThreatBranchRoot(_, context, fact.referenceLine.rootMove)))
        .distinctBy(_.ref.id)
    val candidateEndpoint =
      (recordsForLineEndpoint(context, fact.candidateLine) ++ input.toList.flatMap(recordsForThreatBranchRoot(_, context, fact.candidateLine.rootMove)))
        .distinctBy(_.ref.id)
    val baseRecords =
      (
        referenceEndpoint ++
          candidateEndpoint ++
          recordsForRootContext(context, root) ++
          recordsForTransition(context, referenceTransition) ++
          recordsForTransition(context, candidateTransition) ++
          recordsForAfterPosition(context, referenceTransition) ++
          recordsForAfterPosition(context, candidateTransition)
      ).distinctBy(_.ref.id)
    val parentRecords =
      baseRecords.flatMap(record => record.parents.flatMap(parent => context.evidenceGraph.byId.get(parent.id)))
    ComparisonEvidenceNeighborhood(
      referenceEndpoint = referenceEndpoint,
      candidateEndpoint = candidateEndpoint,
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

  private def recordsForThreatBranchRoot(
      input: NormalizedMoveReviewInput,
      context: JudgmentAssemblyContext,
      rootMove: String
  ): List[EvidenceRecord] =
    val ownedThreatLineIds =
      threatBranchLineOwners(input, context).collect {
        case (lineId, ownerMove) if normalizeMove(ownerMove) == normalizeMove(rootMove) => lineId
      }.toSet
    if ownedThreatLineIds.isEmpty then Nil
    else
      context.evidenceGraph.records.filter(record =>
        endpointLayer(record.ref.layer) &&
          record.ref.line.exists(line => line.role == LineNodeRole.Threat && ownedThreatLineIds.contains(line.id))
      )

  private def threatBranchLineOwners(
      input: NormalizedMoveReviewInput,
      context: JudgmentAssemblyContext
  ): Map[String, String] =
    val lineByKey =
      context.lines
        .filter(_.role == LineNodeRole.Threat)
        .map(line => (line.ref.rank, normalizeMove(line.ref.rootMove)) -> line.ref)
        .toMap
    input.threatBranches
      .flatMap(branch =>
        branch.lines.flatMap(line =>
          line.rootMove.flatMap(rootMove =>
            lineByKey
              .get((line.rank, normalizeMove(rootMove)))
              .map(ref => ref.id -> branch.probedMoveUci)
          )
        )
      )
      .toMap

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
          edge.matches(record)
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

  private def endpointLayer(layer: EvidenceLayer): Boolean =
    layer match
      case EvidenceLayer.Line | EvidenceLayer.Eval | EvidenceLayer.MoveMotif | EvidenceLayer.Relation |
          EvidenceLayer.TacticalMechanism | EvidenceLayer.StrategicMechanism =>
        true
      case _ =>
        false

  private def rootContextLayer(layer: EvidenceLayer): Boolean =
    layer match
      case EvidenceLayer.Board | EvidenceLayer.ThreatPressure |
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
      case EvidenceLayer.Board | EvidenceLayer.ThreatPressure |
          EvidenceLayer.StrategicMechanism =>
        true
      case _ =>
        false
