package lila.chessjudgment.analysis.assembly

import chess.Color
import lila.chessjudgment.model.evaluation.{ JudgmentThresholds, PerspectiveMath }
import lila.chessjudgment.model.judgment.CandidateSetType
import lila.chessjudgment.analysis.transition.TransitionFactNormalizer
import lila.chessjudgment.model.line.PrincipalVariationEvidence
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

  private final case class ProvisionalRelativeCauseRecord(
      intent: RelativeCauseDraftIntent,
      cause: RelativeCauseFact,
      record: EvidenceRecord
  )

  private final case class ComparisonEndpointEffectInventoryPair(
      reference: ComparisonEndpointEffectInventory,
      candidate: ComparisonEndpointEffectInventory
  ):
    def forSide(
        side: RelativeCauseSourceSide
    ): Option[(ComparisonEndpointEffectInventory, ComparisonEndpointEffectInventory)] =
      side match
        case RelativeCauseSourceSide.Reference => Some(reference -> candidate)
        case RelativeCauseSourceSide.Candidate => Some(candidate -> reference)
        case RelativeCauseSourceSide.Shared | RelativeCauseSourceSide.Mixed => None

  private[analysis] final case class PlanResultEndpointInventory(
      observations: Set[ComparisonEndpointEffectObservation],
      enumeratedPlanIds: Set[String],
      incompletePlanIds: Set[String],
      extractionReady: Boolean,
      exactInducedResponseObservations: Map[EvidenceRef, ComparisonEndpointEffectObservation] = Map.empty
  ):
    /** Preserves the F-stage PlanCausal carrier that produced an exact
      * comparison-induced observation. Observation values omit the carrier
      * EvidenceRef, so consumers needing direct-proof identity must use this
      * mapping instead of set membership.
      */
    def exactInducedResponseObservationFor(
        source: EvidenceRef
    ): Option[ComparisonEndpointEffectObservation] =
      exactInducedResponseObservations.get(source)

    def uniqueObservationFor(
        scope: ComparisonEndpointEffectScopeKey
    ): Option[Option[ComparisonEndpointEffectObservation]] =
      scope.effectIdentity.planIds.distinct match
        case planId :: Nil
            if extractionReady && enumeratedPlanIds(planId) && !incompletePlanIds(planId) =>
          ComparisonEndpointEffectObservationPolicy.uniqueObservationFor(
            ComparisonEndpointEffectInventory.Complete(observations),
            scope
          )
        case _ =>
          None

    /** Projected lookup is complete only when every enumerated carrier is
      * resolved and every exact PlanResult can be projected without losing a
      * magnitude conflict. Equal duplicate observations are one fact. A real
      * semantic match may cross taxonomy; certified absence still requires
      * enumeration of the source taxonomy on this endpoint.
      */
    def uniqueComparisonMagnitudeFor(
        key: ComparisonEndpointEffectObservationPolicy.PlanResultComparisonKey,
        requiredAbsentPlanId: String
    ): Option[Option[ComparisonEndpointEffectMagnitude]] =
      if !extractionReady || incompletePlanIds.nonEmpty then None
      else
        val projected = observations.toList.map(observation =>
          ComparisonEndpointEffectObservationPolicy
            .planResultComparisonKey(observation)
            .map(_ -> observation.magnitude)
        )
        if projected.exists(_.isEmpty) then None
        else
          val byKey = projected.flatten.groupBy(_._1)
          if byKey.values.exists(_.map(_._2).distinct.size != 1) then None
          else
            byKey.get(key).flatMap(_.headOption).map(_._2) match
              case Some(magnitude) => Some(Some(magnitude))
              case None if enumeratedPlanIds(requiredAbsentPlanId) => Some(None)
              case None => None

  /** Typed PlanResult comparison is admissible only when the source owns the
    * exact observation and the counterpart inventory is complete for the
    * relevant comparison scope. Missing enumeration and unresolved carriers
    * fail closed; complete absence is a real differential. The source-keyed
    * neutral mapping alone selects projected comparison; an absent mapping
    * retains the generic exact-scope path and a mismatched mapping is invalid.
    */
  private[analysis] object PlanResultDifferentialPolicy:
    def admitted(
        sourceInventory: PlanResultEndpointInventory,
        counterpartInventory: PlanResultEndpointInventory,
        source: EvidenceRef,
        sourceObservation: ComparisonEndpointEffectObservation
    ): Boolean =
      val policy = ComparisonEndpointEffectObservationPolicy
      sourceInventory.exactInducedResponseObservationFor(source) match
        case Some(mappedObservation) if mappedObservation == sourceObservation =>
          (for
            sourcePlanId <- sourceObservation.scope.effectIdentity.planIds.distinct match
              case exact :: Nil => Some(exact)
              case _            => None
            key <- policy.planResultComparisonKey(sourceObservation)
            sourceLookup <- sourceInventory.uniqueComparisonMagnitudeFor(key, sourcePlanId)
            observedSourceMagnitude <- sourceLookup
            if observedSourceMagnitude == sourceObservation.magnitude
            counterpartLookup <- counterpartInventory.uniqueComparisonMagnitudeFor(key, sourcePlanId)
          yield counterpartLookup match
            case None => true
            case Some(counterpartMagnitude) =>
              policy.compareMagnitude(sourceObservation.magnitude, counterpartMagnitude) ==
                policy.MagnitudeRelation.LeftStrictlyStronger).getOrElse(false)
        case Some(_) =>
          false
        case None =>
          (for
            sourceLookup <- sourceInventory.uniqueObservationFor(sourceObservation.scope)
            observedSource <- sourceLookup
            if observedSource == sourceObservation
            counterpartLookup <- counterpartInventory.uniqueObservationFor(sourceObservation.scope)
          yield counterpartLookup match
            case None => true
            case Some(counterpartObservation) =>
              policy.compareMagnitude(sourceObservation.magnitude, counterpartObservation.magnitude) ==
                policy.MagnitudeRelation.LeftStrictlyStronger).getOrElse(false)

  private[analysis] final case class PlanResultEndpointInventoryPair(
      reference: PlanResultEndpointInventory,
      candidate: PlanResultEndpointInventory
  ):
    def forSide(
        side: RelativeCauseSourceSide
    ): Option[(PlanResultEndpointInventory, PlanResultEndpointInventory)] =
      side match
        case RelativeCauseSourceSide.Reference => Some(reference -> candidate)
        case RelativeCauseSourceSide.Candidate => Some(candidate -> reference)
        case RelativeCauseSourceSide.Shared | RelativeCauseSourceSide.Mixed => None

  /** Reuses the F-stage PlanResult endpoint inventory for a complete exact
    * comparison. Both sides are assembled before any Cause admission, so
    * consumers can test a differential without treating a Cause channel as
    * an absence oracle.
    */
  private[analysis] def planResultEndpointInventories(
      graph: TypedEvidenceGraph,
      comparison: CandidateComparisonFact,
      rootPosition: PositionNodeRef
  ): PlanResultEndpointInventoryPair =
    PlanResultEndpointInventoryPair(
      reference = planResultEndpointInventory(
        graph,
        comparison.referenceLine,
        comparison.comparison.mover,
        rootPosition,
        Some(comparison),
        Some(RelativeCauseSourceSide.Reference)
      ),
      candidate = planResultEndpointInventory(
        graph,
        comparison.candidateLine,
        comparison.comparison.mover,
        rootPosition,
        Some(comparison),
        Some(RelativeCauseSourceSide.Candidate)
      )
    )

  private final case class ComparisonEndpointEffectInventories(
      lineMaterial: ComparisonEndpointEffectInventoryPair,
      lineMate: ComparisonEndpointEffectInventoryPair,
      lineQualitative: ComparisonEndpointEffectInventoryPair,
      structural: ComparisonEndpointEffectInventoryPair,
      strategic: ComparisonEndpointEffectInventoryPair,
      planResult: PlanResultEndpointInventoryPair
  ):
    def forChannel(channel: DirectCauseChannel): Option[ComparisonEndpointEffectInventoryPair] =
      channel.rootOwnedEffectDescriptor.flatMap { descriptor =>
        if descriptor.identity.strategicAxes.nonEmpty then Some(strategic)
        else
          descriptor.identity.primitiveKind match
            case RootOwnedEffectPrimitiveKind.LineEpisode =>
              descriptor.magnitude match
                case DirectEffectMagnitudeKnowledge.Exact(
                      DirectCauseImportanceMeasure.MaterialOutcome(_, _)
                    ) => Some(lineMaterial)
                case DirectEffectMagnitudeKnowledge.Exact(
                      DirectCauseImportanceMeasure.MateArrival(_)
                    ) => Some(lineMate)
                case DirectEffectMagnitudeKnowledge.NotApplicable => Some(lineQualitative)
                case DirectEffectMagnitudeKnowledge.Exact(_) => None
                case DirectEffectMagnitudeKnowledge.ExpectedButMissing |
                    DirectEffectMagnitudeKnowledge.Ambiguous => None
            case RootOwnedEffectPrimitiveKind.RootLineEvent =>
              channel.rootOwnedProof match
                case Some(RootOwnedEffectProof.RootLineEvent(_, _, event))
                    if event.kind == LineEventKind.Mate => Some(lineMate)
                case Some(RootOwnedEffectProof.RootLineEvent(_, _, _)) => Some(lineQualitative)
                case _ => None
            case RootOwnedEffectPrimitiveKind.EndgameHorizon => Some(lineQualitative)
            case RootOwnedEffectPrimitiveKind.StructuralTransition => Some(structural)
            case _ => None
      }

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
        val factContext = context.withEvidence(comparisonRecords)
        val strategicContrastRecords = comparisonRecords.flatMap(record =>
          strategicMechanismContrastRecords(
            factContext,
            inputs.root,
            inputs.allocator,
            record
          )
        )
        factContext.withEvidence(strategicContrastRecords)
      }
      .getOrElse(context)

  def enrichCauses(context: JudgmentAssemblyContext): JudgmentAssemblyContext =
    relativeAssemblyInputs(context)
      .map { inputs =>
        val comparisonRecords = assembledComparisonRecords(context, inputs.root)
        val primaryComparisonRecord =
          requiredPrimaryComparisonRecord(comparisonRecords, inputs.reference, inputs.candidate)
        val snapshotsByComparisonId = comparisonEndpointEvidenceSnapshots(context)
          .map(snapshot => snapshot.comparisonEvidence.id -> snapshot)
          .toMap
        val rawCauseRecords =
          comparisonRecords.flatMap(record =>
            snapshotsByComparisonId.get(record.ref.id).toList.flatMap(snapshot =>
              relativeCauseRecords(
                inputs.input,
                context,
                inputs.root,
                inputs.allocator,
                record,
                snapshot
              )
            )
          )
        val causeRecords = canonicalizeRelativeCauseRecords(
          rawCauseRecords,
          context.evidenceGraph,
          snapshotsByComparisonId
        )
        val playedMoveSet = Set(JudgmentSubjectBinding.normalizeMove(inputs.played.moveUci))
        val playedCauseRecords =
          causeRecords.filter(record =>
            JudgmentSubjectBinding.primaryPlayed(
              JudgmentSubjectBinding.recordBinding(record, context.evidenceGraph, playedMoveSet)
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
        context.withEvidence(causeRecords :+ assessmentRecord)
      }
      .getOrElse(context)

  /** Public read-only reconstruction used identically by C admission and the
    * runtime adapter. Every snapshot is derived from the F graph before any
    * provisional Cause exists.
    */
  def comparisonEndpointEvidenceSnapshots(
      context: JudgmentAssemblyContext
  ): List[ComparisonEndpointEvidenceSnapshot] =
    relativeAssemblyInputs(context).toList.flatMap { inputs =>
      assembledComparisonRecords(context, inputs.root).flatMap { record =>
        record.payload match
          case CandidateComparisonEvidence(comparison) =>
            val neighborhood = comparisonNeighborhood(
              context,
              inputs.root,
              comparison,
              Some(inputs.input)
            )
            val carrierRecords = context.evidenceGraph.records.filter {
              case EvidenceRecord(ref, payload: StrategicMechanismContrastEvidence, _) =>
                carrierAtComparisonRoot(ref, inputs.root) &&
                  payload.comparisonKind == comparison.kind &&
                  payload.referenceLine == comparison.referenceLine &&
                  payload.candidateLine == comparison.candidateLine
              case EvidenceRecord(ref, _: TacticalMechanismEvidence | _: StrategicMechanismEvidence, _) =>
                carrierAtComparisonRoot(ref, inputs.root) &&
                  (ref.line.contains(comparison.referenceLine) ||
                    ref.line.contains(comparison.candidateLine))
              case _ => false
            }
            val involvedRecords =
              (neighborhood.involvedRecords ++ carrierRecords).distinctBy(_.ref.id)
            Some(ComparisonEndpointEvidenceSnapshot(
              comparisonEvidence = record.ref,
              comparison = comparison,
              reference = ComparisonEndpointEvidenceSideSnapshot(
                sourceSide = RelativeCauseSourceSide.Reference,
                line = comparison.referenceLine,
                witnesses = EvidenceObjectBinding.comparisonEndpointEvidenceWitnesses(
                  RelativeCauseSourceSide.Reference,
                  comparison.referenceLine,
                  inputs.root,
                  record.ref,
                  comparison,
                  canonicalEndpointEvidenceRecords(
                    neighborhood.referenceRecords,
                    comparison.referenceLine,
                    comparison.comparison.mover,
                    inputs.root,
                    context.evidenceGraph
                  ),
                  involvedRecords,
                  context.evidenceGraph
                )
              ),
              candidate = ComparisonEndpointEvidenceSideSnapshot(
                sourceSide = RelativeCauseSourceSide.Candidate,
                line = comparison.candidateLine,
                witnesses = EvidenceObjectBinding.comparisonEndpointEvidenceWitnesses(
                  RelativeCauseSourceSide.Candidate,
                  comparison.candidateLine,
                  inputs.root,
                  record.ref,
                  comparison,
                  canonicalEndpointEvidenceRecords(
                    neighborhood.candidateRecords,
                    comparison.candidateLine,
                    comparison.comparison.mover,
                    inputs.root,
                    context.evidenceGraph
                  ),
                  involvedRecords,
                  context.evidenceGraph
                )
              )
            ))
          case _ => None
      }
    }

  private def canonicalEndpointEvidenceRecords(
      records: List[EvidenceRecord],
      line: LineNodeRef,
      mover: Color,
      rootPosition: PositionNodeRef,
      graph: TypedEvidenceGraph
  ): List[EvidenceRecord] =
    val endpointRecords = (
      records ++ graph.records.filter(record =>
        carrierAtComparisonRoot(record.ref, rootPosition) && record.referencesLine(line)
      )
    ).distinctBy(_.ref.id)
    val canonicalLineRef = exactLegalLine(endpointRecords, line, rootPosition).map(_._1.id)
    val canonicalStructuralRef =
      exactRootStructuralDelta(endpointRecords, line, mover, rootPosition).map(_._1.id)
    endpointRecords.filter {
      case EvidenceRecord(ref, _: LineFactEvidence, _) => canonicalLineRef.contains(ref.id)
      case EvidenceRecord(ref, _: StructuralDeltaEvidence, _) => canonicalStructuralRef.contains(ref.id)
      case _ => true
    }

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
        val baseComparison = CandidateComparisonFact(
          kind = kind,
          referenceLine = refLine.ref,
          candidateLine = candLine.ref,
          comparison = cmp,
          candidateSet = descriptor
        )
        val defensiveRecaptureResource =
          Option
            .when(kind == CandidateComparisonKind.PlayedVsBest)(
              context.evidenceGraph.recordsFor(candLine.ref).collect {
                case EvidenceRecord(_, payload: LineFactEvidence, _) if payload.line == candLine.ref => payload
              }
            )
            .flatMap {
              case candidateLineFact :: Nil =>
                PlayedVsBestDefensiveRecaptureResource.derive(baseComparison, root, candidateLineFact)
              case _ => None
            }
        TransitionFactNormalizer.fromCandidateComparison(
          id = allocator.evidenceId(
            s"candidate-comparison:${allocator.key(kind)}:${allocator.key(refLine.ref.rootMove)}:${allocator.key(candLine.ref.rootMove)}:$index"
          ),
          comparison = baseComparison.copy(defensiveRecaptureResource = defensiveRecaptureResource),
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
        case record @ EvidenceRecord(_, payload: PlanCausalEventEvidence, _) =>
          strategicAxisEventMatchesPlan(comparison.axis, payload) &&
            (
              payload.representativeResult.exists { case (sourceEvent, consequence) =>
                sourceEvent.step.ply > payload.causalEpisode.root.step.ply &&
                  payload.resultAdvancesGoal(sourceEvent, consequence)
              } ||
                directPawnRestrictionPersistenceWitness(graph, record, comparison.axis, payload)
            )
        case EvidenceRecord(_, PlanTransitionEvidence(transition), _) =>
          transition.continuity.exists(_.consecutivePlies >= 2)
        case _ =>
          false
      }

  private def directPawnRestrictionPersistenceWitness(
      graph: TypedEvidenceGraph,
      eventRecord: EvidenceRecord,
      axis: StrategicAxisDetail,
      event: PlanCausalEventEvidence
  ): Boolean =
    val directClosure = eventRecord :: graph.parentClosure(eventRecord)
    val structuralRecords = directClosure.collect {
      case EvidenceRecord(ref, structural: StructuralDeltaEvidence, _)
          if ref.line.contains(event.rootLine) &&
            structural.line.contains(event.rootLine) &&
            EvidenceRef.sameMove(structural.moveUci, event.rootMove) =>
        structural
    }
    val lineRecords = directClosure.collect {
      case EvidenceRecord(ref, line: LineFactEvidence, _)
          if ref.line.contains(event.rootLine) && line.line == event.rootLine =>
        line
    }
    eventRecord.ref.confidence != EvidenceConfidence.Heuristic &&
      axis.kind == StrategicAxisKind.Counterplay &&
      axis.polarity == StrategicAxisPolarity.Restrain &&
      structuralRecords.exists(structural =>
        lineRecords.exists(line =>
          structural.consequences.exists(consequence =>
            StructuralDeltaEvidence
              .exactRootOccupiedPawnAdvanceRestrictions(structural, consequence)
              .flatMap(StructuralDeltaEvidence.directPawnAdvanceRestrictionAxisLabel)
              .contains(axis.label) &&
              DirectOpponentRestrictionProof
                .exactRootPawnBlockadeAuthority(event, structural, line, consequence)
          )
        )
      )

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
      comparisonRecord: EvidenceRecord,
      endpointSnapshot: ComparisonEndpointEvidenceSnapshot
  ): List[EvidenceRecord] =
    comparisonRecord.payload match
      case CandidateComparisonEvidence(fact) =>
        val neighborhood = comparisonNeighborhood(context, root, fact, Some(input))
        val comparisonProof = comparisonProofRecords(neighborhood)
        val causes =
          mergeCauseCandidates(
            inferCauseCandidates(
              neighborhood,
              fact,
              comparisonRecord,
              endpointSnapshot,
              context.evidenceGraph
            )
          )
        val provisional = causes.zipWithIndex.map { case (candidate, index) =>
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
              neighborhood
            )
          val causalProofRecords =
            rawProofRecords.copy(
              directProof = RootOwnedCausePolicy.directProofRecords(
                context.evidenceGraph,
                fact,
                kind,
                binding,
                attributionKind,
                rawProofRecords.directProof
              )
            )
          val attribution =
            causeAttribution(candidate, binding, causalProofRecords, fact.comparison.mover)
          val proofRecords =
            if attribution.directProofEligible then causalProofRecords
            else
              causalProofRecords.copy(
                directProof = Nil,
                contextSupport =
                  (causalProofRecords.directProof ++ causalProofRecords.contextSupport).distinctBy(_.ref.id)
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
              proof = retainedProof,
              directEffectAdmission = DirectEffectAdmission.Unresolved
            )
          val causeEvidenceId =
            allocator.evidenceId(
              s"relative-cause:${allocator.key(fact.kind)}:${allocator.key(kind)}:${allocator.key(fact.referenceLine.rootMove)}:${allocator.key(fact.candidateLine.rootMove)}:$index"
            )
          val record = TransitionFactNormalizer.fromRelativeCause(
            id = causeEvidenceId,
            cause = cause,
            binding = binding,
            position = root,
            scope = EvidenceScope.Counterfactual,
            confidence = comparisonConfidence(context, fact),
            parents = (comparisonRecord.ref :: (supportRefs ++ proofRecords.all.map(_.ref))).distinctBy(_.id)
          )
          ProvisionalRelativeCauseRecord(
            intent = candidate.intent,
            cause = cause,
            record = record
          )
        }
        restrictDirectEffectsForComparison(
          provisional,
          neighborhood,
          fact,
          comparisonRecord.ref.position,
          context.evidenceGraph,
          endpointSnapshot
        ).filter(item => RelativeCauseConstructionAdmission.initiallyReady(item.cause, context.evidenceGraph))
          .map(_.record)
      case _ => Nil

  private def restrictDirectEffectsForComparison(
      provisional: List[ProvisionalRelativeCauseRecord],
      neighborhood: ComparisonEvidenceNeighborhood,
      comparison: CandidateComparisonFact,
      rootPosition: PositionNodeRef,
      graph: TypedEvidenceGraph,
      endpointSnapshot: ComparisonEndpointEvidenceSnapshot
  ): List[ProvisionalRelativeCauseRecord] =
    val inventories = comparisonEndpointEffectInventories(
      neighborhood,
      comparison,
      comparisonRecordPosition = rootPosition,
      graph = graph
    )
    val rawChannels = provisional.map(item =>
      EvidenceObjectBinding.rawDirectSentenceChannelsForProjection(item.cause, graph)
    )

    provisional.zip(rawChannels).map { case (item, channels) =>
      val admittedSignatures = channels.filter { channel =>
        ComparisonEndpointEffectObservationPolicy
          .uniqueNeutralWitnessFor(
            endpointSnapshot,
            item.cause.sourceSide,
            channel,
            graph
          )
          .exists { neutralWitness =>
            val ownsStrategicAuthority =
              !item.cause.strategicCauseKind ||
                graph.relativeCauseStrategicChannelHasOwnedLongTermProof(item.cause, channel)
            ownsStrategicAuthority &&
              (
                ComparisonEndpointEffectObservationPolicy.retainedPlayedValueReady(
                  item.cause,
                  channel,
                  comparison,
                  graph
                ) ||
                  ComparisonEndpointEffectObservationPolicy.retainedPlayedLiabilityReady(
                    item.cause,
                    channel,
                    comparison,
                    graph
                  ) ||
                  differentialEndpointEffectAdmitted(
                    item.cause,
                    channel,
                    neutralWitness,
                    inventories,
                    graph
                  )
              )
          }
      }.map(_.causalSignature).toSet
      val restrictedCause = item.cause.copy(
        directEffectAdmission = DirectEffectAdmission.Restricted(admittedSignatures)
      )
      item.copy(
        cause = restrictedCause,
        record = item.record.copy(payload = RelativeCauseFactEvidence(restrictedCause))
      )
    }

  /** Draft intent classifies how a Cause was generated. It is deliberately
    * absent here: public admission is decided only by owned fact inventories,
    * attribution, and comparison semantics.
    */
  private def differentialEndpointEffectAdmitted(
      cause: RelativeCauseFact,
      channel: DirectCauseChannel,
      neutralWitness: ComparisonEndpointEvidenceWitness,
      inventories: ComparisonEndpointEffectInventories,
      graph: TypedEvidenceGraph
  ): Boolean =
    val policy = ComparisonEndpointEffectObservationPolicy
    channel.rootOwnedEffectDescriptor match
      case Some(descriptor)
          if descriptor.identity.primitiveKind == RootOwnedEffectPrimitiveKind.PlanResult =>
        (for
          (sourceInventory, counterpartInventory) <- inventories.planResult.forSide(cause.sourceSide)
          sourceObservation <- neutralWitness.observation
        yield PlanResultDifferentialPolicy.admitted(
          sourceInventory,
          counterpartInventory,
          neutralWitness.primitiveProofSource,
          sourceObservation
        )).getOrElse(false)
      case _ =>
        (for
          pair <- inventories.forChannel(channel)
          (sourceInventory, counterpartInventory) <- pair.forSide(cause.sourceSide)
          neutralObservation <- neutralWitness.observation
          scope = neutralObservation.scope
          sourceLookup <- policy.uniqueObservationFor(sourceInventory, scope)
          sourceObservation <- sourceLookup
          counterpartLookup <- policy.uniqueObservationFor(counterpartInventory, scope)
          if neutralObservation == sourceObservation
        yield counterpartLookup match
          case None => true
          case Some(counterpartObservation) =>
            policy.compareMagnitude(sourceObservation.magnitude, counterpartObservation.magnitude) ==
              policy.MagnitudeRelation.LeftStrictlyStronger).getOrElse(false)

  private def comparisonEndpointEffectInventories(
      neighborhood: ComparisonEvidenceNeighborhood,
      comparison: CandidateComparisonFact,
      comparisonRecordPosition: PositionNodeRef,
      graph: TypedEvidenceGraph
  ): ComparisonEndpointEffectInventories =
    val referenceLineInventories = lineEndpointInventories(
      neighborhood.referenceRecords,
      comparison.referenceLine,
      comparisonRecordPosition
    )
    val candidateLineInventories = lineEndpointInventories(
      neighborhood.candidateRecords,
      comparison.candidateLine,
      comparisonRecordPosition
    )
    ComparisonEndpointEffectInventories(
      lineMaterial = ComparisonEndpointEffectInventoryPair(
        referenceLineInventories.material,
        candidateLineInventories.material
      ),
      lineMate = ComparisonEndpointEffectInventoryPair(
        referenceLineInventories.mate,
        candidateLineInventories.mate
      ),
      lineQualitative = ComparisonEndpointEffectInventoryPair(
        referenceLineInventories.qualitative,
        candidateLineInventories.qualitative
      ),
      structural = ComparisonEndpointEffectInventoryPair(
        reference = structuralEndpointInventory(
          neighborhood.referenceRecords,
          comparison.referenceLine,
          comparison.comparison.mover,
          comparisonRecordPosition
        ),
        candidate = structuralEndpointInventory(
          neighborhood.candidateRecords,
          comparison.candidateLine,
          comparison.comparison.mover,
          comparisonRecordPosition
        )
      ),
      strategic = strategicEndpointInventories(
        neighborhood,
        comparison,
        comparisonRecordPosition
      ),
      planResult = planResultEndpointInventories(
        graph,
        comparison,
        comparisonRecordPosition
      )
    )

  private def lineEndpointInventories(
      records: List[EvidenceRecord],
      line: LineNodeRef,
      rootPosition: PositionNodeRef
  ): ComparisonEndpointLineEffectInventories =
    exactLegalLine(records, line, rootPosition) match
      case Some((source, payload)) =>
        val inventories = EvidenceObjectBinding.comparisonEndpointLineObservations(
          source,
          payload,
          rootPosition
        )
        enforceLineFamilyCompleteness(
          inventories,
          payload,
          exactLineEvaluation(records, line, rootPosition)
        )
      case None => ComparisonEndpointLineEffectInventories.incomplete

  /** Each endpoint family owns its own completeness predicate. A missing
    * material window cannot erase mate/endgame facts, and a missing or
    * inconsistent engine mate score cannot erase material/endgame facts.
    */
  private[assembly] def enforceLineFamilyCompleteness(
      inventories: ComparisonEndpointLineEffectInventories,
      payload: LineFactEvidence,
      evaluation: Option[EvalFactEvidence]
  ): ComparisonEndpointLineEffectInventories =
    val material =
      if payload.materialNetCaptureCpForMover.nonEmpty && payload.hasCompleteMaterialWindow then
        inventories.material
      else ComparisonEndpointEffectInventory.Incomplete
    val mate = evaluation.fold[ComparisonEndpointEffectInventory](
      ComparisonEndpointEffectInventory.Incomplete
    ) { exactEvaluation =>
      inventories.mate match
        case complete @ ComparisonEndpointEffectInventory.Complete(observations)
            if mateObservationConsistent(payload, exactEvaluation, observations) =>
          complete
        case _ => ComparisonEndpointEffectInventory.Incomplete
    }
    inventories.copy(material = material, mate = mate)

  private def structuralEndpointInventory(
      records: List[EvidenceRecord],
      line: LineNodeRef,
      mover: Color,
      rootPosition: PositionNodeRef
  ): ComparisonEndpointEffectInventory =
    exactRootStructuralDelta(records, line, mover, rootPosition) match
      case Some((source, payload)) =>
        EvidenceObjectBinding
          .comparisonEndpointStructuralObservations(
            source,
            payload,
            rootPosition,
            line
          )
          .map(ComparisonEndpointEffectInventory.Complete.apply)
          .getOrElse(ComparisonEndpointEffectInventory.Incomplete)
      case None => ComparisonEndpointEffectInventory.Incomplete

  private def exactLegalLine(
      records: List[EvidenceRecord],
      line: LineNodeRef,
      rootPosition: PositionNodeRef
  ): Option[(EvidenceRef, LineFactEvidence)] =
    val lineFacts = records.collect {
      case EvidenceRecord(ref, payload: LineFactEvidence, _)
          if carrierAtComparisonRoot(ref, rootPosition) &&
            ref.line.contains(line) &&
            payload.line == line &&
            payload.rootMove.exists(EvidenceRef.sameMove(_, line.rootMove)) &&
            payload.lineReplaySteps.headOption.exists(step =>
              EvidenceRef.sameMove(step.moveUci, line.rootMove) &&
                PrincipalVariationEvidence.sameBoardState(step.fenBefore, rootPosition.fen) &&
                PrincipalVariationEvidence
                  .legalFenAfter(step.fenBefore, step.moveUci)
                  .exists(PrincipalVariationEvidence.sameBoardState(_, step.fenAfter))
            ) &&
            exactReplayChain(payload.lineReplaySteps) =>
        ref -> payload
    }
    canonicalSemanticCarrier(lineFacts)

  private def exactLineEvaluation(
      records: List[EvidenceRecord],
      line: LineNodeRef,
      rootPosition: PositionNodeRef
  ): Option[EvalFactEvidence] =
    val evaluations = records.collect {
      case EvidenceRecord(ref, payload: EvalFactEvidence, _)
          if carrierAtComparisonRoot(ref, rootPosition) &&
            ref.line.contains(line) &&
            payload.line == line &&
            payload.depth > 0 &&
            ref.confidence == EvidenceConfidence.EngineBacked =>
        ref -> payload
    }
    canonicalSemanticCarrier(evaluations).map(_._2)

  /** Source ids identify carriers, not facts. Repeated carriers of one equal
    * payload collapse deterministically; genuinely different payloads leave
    * the endpoint incomplete instead of being selected by record order.
    */
  private[assembly] def canonicalSemanticCarrier[A <: EvidencePayload](
      candidates: List[(EvidenceRef, A)]
  ): Option[(EvidenceRef, A)] =
    candidates.groupBy(_._2).toList match
      case (_, equivalentCarriers) :: Nil => Some(equivalentCarriers.minBy(_._1.id))
      case _                              => None

  /** All exact endpoint carriers are rooted at the compared pre-move board.
    * Semantic FEN comparison deliberately ignores clocks, while ply prevents
    * a repeated board occurrence from being borrowed from another turn.
    */
  private[assembly] def carrierAtComparisonRoot(
      carrier: EvidenceRef,
      rootPosition: PositionNodeRef
  ): Boolean =
    RootOwnedEffectPolicy.sameCausalRootOccurrence(carrier.position, rootPosition)

  private def exactReplayChain(steps: List[LineReplayStep]): Boolean =
    steps.nonEmpty &&
      steps.zipWithIndex.forall { case (step, index) =>
        step.ply == steps.head.ply + index &&
          PrincipalVariationEvidence
            .legalFenAfter(step.fenBefore, step.moveUci)
            .exists(PrincipalVariationEvidence.sameBoardState(_, step.fenAfter)) &&
          steps.lift(index + 1).forall(next =>
            PrincipalVariationEvidence.sameBoardState(step.fenAfter, next.fenBefore)
          )
      }

  private def mateObservationConsistent(
      payload: LineFactEvidence,
      evaluation: EvalFactEvidence,
      observations: Set[ComparisonEndpointEffectObservation]
  ): Boolean =
    val ownedMateEpisodes = payload
      .rootOwnedCausalEpisodes(payload.line.rootMove)
      .filter(_.consequence.kind == LineConsequenceKind.Mate)
    val observedMateCount = observations.count(_.magnitude match
      case ComparisonEndpointEffectMagnitude.Exact(DirectCauseImportanceMeasure.MateArrival(_)) => true
      case _ => false
    )
    evaluation.mate match
      case Some(mate) if mate != 0 =>
        val winner = if mate > 0 then chess.White else chess.Black
        ownedMateEpisodes.nonEmpty &&
          ownedMateEpisodes.forall(_.consequence.beneficiary.contains(winner)) &&
          observedMateCount == ownedMateEpisodes.size
      case Some(_) =>
        false
      case None =>
        ownedMateEpisodes.isEmpty && observedMateCount == 0

  private def exactRootStructuralDelta(
      records: List[EvidenceRecord],
      line: LineNodeRef,
      mover: Color,
      rootPosition: PositionNodeRef
  ): Option[(EvidenceRef, StructuralDeltaEvidence)] =
    canonicalSemanticCarrier(records.collect {
      case EvidenceRecord(ref, payload: StructuralDeltaEvidence, _)
          if carrierAtComparisonRoot(ref, rootPosition) &&
            ref.line.contains(line) &&
            payload.line.contains(line) &&
            EvidenceRef.sameMove(payload.moveUci, line.rootMove) &&
            payload.perspective == mover &&
            PrincipalVariationEvidence.sameBoardState(payload.from.fen, rootPosition.fen) &&
            PrincipalVariationEvidence
              .legalFenAfter(payload.from.fen, line.rootMove)
              .exists(PrincipalVariationEvidence.sameBoardState(_, payload.to.fen)) =>
        ref -> payload
    })

  /** Plan-result absence is meaningful only after the same deterministic F
    * enumeration path was available on that endpoint. The canonical pressure,
    * structural transition, and legal line must jointly enumerate the plan;
    * otherwise an empty event set remains incomplete rather than negative.
    */
  private[assembly] def exactPostRootPlanPressureScope(
      role: LineNodeRole
  ): Option[EvidenceScope] =
    role match
      case LineNodeRole.Played        => Some(EvidenceScope.AfterPlayedPosition)
      case LineNodeRole.BestReference => Some(EvidenceScope.AfterReferencePosition)
      case LineNodeRole.Alternative   => Some(EvidenceScope.AlternativeTransition)
      case LineNodeRole.Threat        => None

  private[assembly] def planResultEndpointInventory(
      graph: TypedEvidenceGraph,
      line: LineNodeRef,
      mover: Color,
      rootPosition: PositionNodeRef,
      comparison: Option[CandidateComparisonFact] = None,
      sourceSide: Option[RelativeCauseSourceSide] = None
  ): PlanResultEndpointInventory =
    val endpointRecords = graph.records.filter(_.referencesLine(line))
    val exactAfterRoot = PrincipalVariationEvidence.legalFenAfter(rootPosition.fen, line.rootMove)
    val afterRootScope = exactPostRootPlanPressureScope(line.role)
    val pressureCandidates = endpointRecords.collect {
      case record @ EvidenceRecord(ref, payload: PlanPressureEvidence, _)
          if ref.line.contains(line) &&
            afterRootScope.contains(ref.scope) &&
            ref.position.ply == rootPosition.ply + 1 &&
            exactAfterRoot.exists(
              PrincipalVariationEvidence.sameBoardState(_, ref.position.fen)
            ) =>
        record -> payload
    }
    val canonicalInputs = for
      (_, lineFact) <- exactLegalLine(endpointRecords, line, rootPosition)
      (_, structural) <- exactRootStructuralDelta(
        endpointRecords,
        line,
        mover,
        rootPosition
      )
      (_, pressure) <- canonicalSemanticCarrier(pressureCandidates.map { case (record, payload) =>
        record.ref -> payload
      })
      lineRefIds = endpointRecords.collect {
        case EvidenceRecord(ref, payload: LineFactEvidence, _)
            if payload == lineFact &&
              carrierAtComparisonRoot(ref, rootPosition) &&
              ref.line.contains(line) =>
          ref.id
      }.toSet
      structuralRefIds = endpointRecords.collect {
        case EvidenceRecord(ref, payload: StructuralDeltaEvidence, _)
            if payload == structural &&
              carrierAtComparisonRoot(ref, rootPosition) &&
              ref.line.contains(line) =>
          ref.id
      }.toSet
      pressureRefIds = pressureCandidates.collect {
        case (record, payload)
            if payload == pressure &&
              record.parents.exists(parent => structuralRefIds(parent.id)) =>
          record.ref.id
      }.toSet
      if lineRefIds.nonEmpty && structuralRefIds.nonEmpty && pressureRefIds.nonEmpty
    yield (lineFact, structural, pressure, lineRefIds, structuralRefIds, pressureRefIds)

    canonicalInputs match
      case None =>
        PlanResultEndpointInventory(Set.empty, Set.empty, Set.empty, extractionReady = false)
      case Some((lineFact, structural, pressure, lineRefIds, structuralRefIds, pressureRefIds)) =>
        val enumeratedPlanIds = PlanCausalEventProof
          .eventCandidatePlans(pressure, line, structural, lineFact)
          .map(_.plan.id.id)
          .toSet
        val eventRecords = endpointRecords.collect {
          case record @ EvidenceRecord(ref, event: PlanCausalEventEvidence, _)
              if carrierAtComparisonRoot(ref, rootPosition) &&
                ref.line.contains(line) &&
                event.rootLine == line =>
            record -> event
        }
        val evaluated = eventRecords.groupBy(_._2.planId.id).toList.map {
          case (planId, records) =>
            val projections = records.map { case (record, event) =>
              val exactAssessments = List(
                event.exactRobustPublicResultAssessment,
                event.exactRefutedPublicResultAssessment
              ).flatten.distinct
              val carrierReady =
                enumeratedPlanIds(planId) &&
                  record.ref.confidence != EvidenceConfidence.Heuristic &&
                  record.parents.exists(parent => pressureRefIds(parent.id)) &&
                  record.parents.exists(parent => structuralRefIds(parent.id)) &&
                  record.parents.exists(parent => lineRefIds(parent.id)) &&
                  exactAssessments.nonEmpty
              if carrierReady then
                val ordinary = exactAssessments.map(assessment =>
                  ComparisonEndpointEffectObservationPolicy.fromExactPlanResult(
                    rootPosition,
                    line,
                    record.ref,
                    event,
                    assessment
                  )
                )
                val inducedResponseMoveOrder =
                  for
                    exactComparison <- comparison.toList
                    exactSourceSide <- sourceSide.toList
                    rootActor <- RootCausalActor.fromPosition(rootPosition, line.rootMove).toList
                    (assessment, response) <-
                      ComparisonEndpointEffectObservationPolicy
                        .exactInducedResponseMoveOrder(
                          exactComparison,
                          exactSourceSide,
                          record.ref,
                          event,
                          graph
                        )
                        .toList
                    if exactAssessments.contains(assessment)
                    binding <- EvidenceObjectBinding
                      .inducedResponseMoveOrderBinding(
                        exactComparison,
                        exactSourceSide,
                        record.ref,
                        event,
                        rootActor,
                        assessment,
                        response,
                        line
                      )
                      .toList
                  yield ComparisonEndpointEffectObservationPolicy.fromExactPlanResult(
                    rootPosition,
                    line,
                    record.ref,
                    event,
                    assessment,
                    Some(binding),
                    selectedInducedResponse = Some(response)
                  )
                val exactInducedResponseObservation = inducedResponseMoveOrder.flatten match
                  case observation :: Nil => Some(observation)
                  case _                  => None
                record.ref -> (ordinary ++ inducedResponseMoveOrder, exactInducedResponseObservation)
              else record.ref -> (List(None), None)
            }
            val successful = projections.flatMap(_._2._1).flatten.toSet
            val complete =
              projections.forall(_._2._1.forall(_.nonEmpty)) &&
                successful
                  .groupBy(_.scope)
                  .forall(_._2.map(_.magnitude).size == 1)
            val exactInducedResponseObservations = projections.flatMap {
              case (source, (_, Some(observation))) => Some(source -> observation)
              case _                                 => None
            }.toMap
            (planId, successful, complete, exactInducedResponseObservations)
        }
        PlanResultEndpointInventory(
          observations = evaluated.flatMap(_._2).toSet,
          enumeratedPlanIds = enumeratedPlanIds,
          incompletePlanIds = evaluated.collect { case (planId, _, false, _) => planId }.toSet,
          extractionReady = true,
          exactInducedResponseObservations = evaluated.flatMap(_._4).toMap
        )

  private def strategicEndpointInventories(
      neighborhood: ComparisonEvidenceNeighborhood,
      comparison: CandidateComparisonFact,
      rootPosition: PositionNodeRef
  ): ComparisonEndpointEffectInventoryPair =
    val exactPayloads = neighborhood.involvedRecords.collect {
      case EvidenceRecord(ref, payload: StrategicMechanismContrastEvidence, _)
          if carrierAtComparisonRoot(ref, rootPosition) &&
            payload.comparisonKind == comparison.kind &&
            payload.referenceLine == comparison.referenceLine &&
            payload.candidateLine == comparison.candidateLine =>
        ref -> payload
    }
    canonicalSemanticCarrier(exactPayloads) match
      case Some((_, payload)) if strategicSourcesComplete(payload, comparison) =>
        (
          strategicEndpointObservations(
              payload,
              comparison.referenceLine,
              RelativeCauseSourceSide.Reference,
              rootPosition
            ),
          strategicEndpointObservations(
              payload,
              comparison.candidateLine,
              RelativeCauseSourceSide.Candidate,
              rootPosition
            )
        ) match
          case (Some(reference), Some(candidate)) =>
            ComparisonEndpointEffectInventoryPair(
              reference = ComparisonEndpointEffectInventory.Complete(reference),
              candidate = ComparisonEndpointEffectInventory.Complete(candidate)
            )
          case _ =>
            ComparisonEndpointEffectInventoryPair(
              ComparisonEndpointEffectInventory.Incomplete,
              ComparisonEndpointEffectInventory.Incomplete
            )
      case _ =>
        ComparisonEndpointEffectInventoryPair(
          ComparisonEndpointEffectInventory.Incomplete,
          ComparisonEndpointEffectInventory.Incomplete
        )

  private def strategicSourcesComplete(
      payload: StrategicMechanismContrastEvidence,
      comparison: CandidateComparisonFact
  ): Boolean =
    payload.axisComparisons.nonEmpty &&
      payload.axisComparisons.map(_.axisKey).distinct.size == payload.axisComparisons.size &&
      payload.axisComparisons.forall { axis =>
      val referenceComplete =
        axis.referenceStrength == 0 ||
          axis.referenceSources.exists(_.line.contains(comparison.referenceLine))
      val candidateComplete =
        axis.candidateStrength == 0 ||
          axis.candidateSources.exists(_.line.contains(comparison.candidateLine))
      axis.referenceStrength >= 0 &&
        axis.candidateStrength >= 0 &&
        (axis.referenceStrength > 0 || axis.candidateStrength > 0) &&
        strategicAxisComparisonOutcome(
          axis.axis,
          axis.referenceStrength,
          axis.candidateStrength
        ) == axis.outcome &&
        referenceComplete && candidateComplete
      }

  private def strategicEndpointObservations(
      payload: StrategicMechanismContrastEvidence,
      line: LineNodeRef,
      sourceSide: RelativeCauseSourceSide,
      rootPosition: PositionNodeRef
  ): Option[Set[ComparisonEndpointEffectObservation]] =
    val projected = payload.axisComparisons.flatMap { axis =>
      val (strength, ownsSource) = sourceSide match
        case RelativeCauseSourceSide.Reference =>
          axis.referenceStrength -> axis.referenceSources.exists(_.line.contains(line))
        case RelativeCauseSourceSide.Candidate =>
          axis.candidateStrength -> axis.candidateSources.exists(_.line.contains(line))
        case RelativeCauseSourceSide.Shared | RelativeCauseSourceSide.Mixed =>
          0 -> false
      Option.when(strength > 0)(
        Option
          .when(ownsSource)(())
          .flatMap(_ =>
            ComparisonEndpointEffectObservationPolicy.fromStrategicAxis(
              rootPosition,
              line,
              axis.axis,
              strength
            )
          )
      )
    }
    EvidenceObjectBinding.completeEndpointObservations(projected)

  private def relativeCauseProofRecords(
      graph: TypedEvidenceGraph,
      fact: CandidateComparisonFact,
      kind: RelativeCauseKind,
      binding: RelativeCauseBinding,
      attributionKind: CauseAttributionKind,
      support: List[EvidenceRecord],
      comparisonProof: List[EvidenceRecord],
      neighborhood: ComparisonEvidenceNeighborhood
  ): RelativeCauseProofRecords =
    val proofSupport = support.distinctBy(_.ref.id)
    val supportIds = supportClosureRecordIds(graph, proofSupport)
    val directRecords =
      ownedCauseDirectProofRecords(
        graph,
        fact,
        kind,
        binding,
        attributionKind,
        proofSupport,
        comparisonProof
      )
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

  private[chessjudgment] def ownedCauseDirectProofRecords(
      graph: TypedEvidenceGraph,
      fact: CandidateComparisonFact,
      kind: RelativeCauseKind,
      binding: RelativeCauseBinding,
      attributionKind: CauseAttributionKind,
      support: List[EvidenceRecord],
      comparisonProof: List[EvidenceRecord] = Nil
  ): List[EvidenceRecord] =
    val proofSupport = support.distinctBy(_.ref.id)
    val ownedSupportSeeds =
      proofSupport
        .filter(record => directProofSource(graph, fact, kind, binding, attributionKind, record))
        .distinctBy(_.ref.id)
    val ownedSupportClosure =
      proofSectionRecords(graph, ownedSupportSeeds, Some(kind), Some(binding))
    val ownedSupportIds = supportClosureRecordIds(graph, ownedSupportClosure)
    val directSeed =
      (ownedSupportClosure ++ comparisonProof.filter(record => ownedSupportIds.contains(record.ref.id)))
        .filter(record => directProofSource(graph, fact, kind, binding, attributionKind, record))
        .distinctBy(_.ref.id)
    proofSectionRecords(graph, directSeed, Some(kind), Some(binding))
      .filter(record => directProofSource(graph, fact, kind, binding, attributionKind, record))
      .filter(record => ownedSupportIds.contains(record.ref.id))
      .distinctBy(_.ref.id)

  private def causeAttribution(
      draft: RelativeCauseDraft,
      binding: RelativeCauseBinding,
      proofRecords: RelativeCauseProofRecords,
      mover: chess.Color
  ): CauseAttribution =
    val ownedRefs = proofRecords.directProof.map(_.ref).distinctBy(_.id)
    val rootMatched =
      ownedRefs.nonEmpty &&
        proofRecords.directProof.exists(record =>
          recordMatchesEventRoot(record, binding.eventLine.rootMove, mover)
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
        case record @ EvidenceRecord(_, payload: PlanCausalEventEvidence, _) =>
          planCausalEventDirectlyOwnsCause(graph, fact, kind, binding, record, payload)
        case record @ EvidenceRecord(_, CandidateComparisonEvidence(_), _) =>
          comparisonDefensiveResourceDirectlyOwnsCause(
            graph,
            fact,
            kind,
            binding,
            attributionKind,
            record
          )
        case _ if RelativeCauseKind.requiresExactPlanResult(kind) =>
          false
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
          if Set(RelativeCauseKind.PlanContradiction, RelativeCauseKind.PlanImprovement)(kind) then
            false
          else if kind == RelativeCauseKind.SacrificeCompensation then
            payload.canSupportCompensation
          else
            payload.canSupportStrategicCause &&
              strategicMechanismProofSignals(graph, kind, payload, binding.sourceSide, structuralProofSourceIds).nonEmpty
        case EvidenceRecord(_, payload: StrategicMechanismContrastEvidence, _) =>
          !Set(RelativeCauseKind.PlanContradiction, RelativeCauseKind.PlanImprovement)(kind) &&
            strategicContrastCanDirectlyProveCause(kind, payload, binding.sourceSide, graph)
        case EvidenceRecord(_, payload: ThreatEpisodeEvidence, _) =>
          payload.isProofSignalDefensivePressure
        case EvidenceRecord(_, payload: StructuralDeltaEvidence, _) =>
          RelativeCauseKind.structuralConsequences(kind, payload).nonEmpty
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
    val ownedParentClosure =
      records
        .flatMap(graph.parentClosure)
        .filter(record => proofSourceLayer(record.ref.layer))
    val explicitTacticalRelationSources =
      records.collect {
        case EvidenceRecord(_, payload: TacticalMechanismEvidence, _) =>
          payload.signals
            .filter(_.kind == TacticalMechanismSignalKind.Relation)
            .flatMap(_.source)
            .flatMap(graph.record)
            .flatMap(source => source :: graph.parentClosure(source))
            .collect {
              case record @ EvidenceRecord(_, relation: RelationFactEvidence, _)
                  if relation.hasConcreteRelationProof =>
                record
            }
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
    val explicitStrategicRelationSources =
      records.collect {
        case EvidenceRecord(_, payload: StrategicMechanismEvidence, _) if payload.canSupportStrategicCause =>
          payload.signals
            .filter(signal =>
              (kind, binding) match
                case (Some(causeKind), Some(causeBinding)) =>
                  strategicSignalDirectlyOwnsCause(
                    graph,
                    signal,
                    causeKind,
                    causeBinding.eventLine.rootMove,
                    causeBinding.sourceSide,
                    Some(causeBinding.eventLine)
                  )
                case _ =>
                  false
            )
            .flatMap(signal => graph.record(signal.source))
            .flatMap(source => source :: graph.parentClosure(source))
            .collect {
              case record @ EvidenceRecord(_, relation: RelationFactEvidence, _)
                  if relation.hasConcreteRelationProof =>
                record
            }
      }.flatten
    (
      records ++
        ownedParentClosure ++
        explicitTacticalRelationSources ++
        strategicStructuralParents ++
        explicitStrategicRelationSources
    ).distinctBy(_.ref.id)

  private def proofSectionRecordIds(graph: TypedEvidenceGraph, records: List[EvidenceRecord]): Set[String] =
    proofSectionRecords(graph, records).map(_.ref.id).toSet

  private def supportClosureRecordIds(graph: TypedEvidenceGraph, records: List[EvidenceRecord]): Set[String] =
    records
      .flatMap(record => record :: graph.parentClosure(record))
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
          EvidenceLayer.StrategicMechanism | EvidenceLayer.CandidateComparison =>
        true
      case _ =>
        false

  private def directProofSource(
      graph: TypedEvidenceGraph,
      fact: CandidateComparisonFact,
      kind: RelativeCauseKind,
      binding: RelativeCauseBinding,
      attributionKind: CauseAttributionKind,
      record: EvidenceRecord
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
    val exactPlanResultCarrier =
      !RelativeCauseKind.requiresExactPlanResult(kind) ||
        record.payload.isInstanceOf[PlanCausalEventEvidence]
    verifiedRootRecapture && exactPlanResultCarrier &&
      (record.payload match
        case payload: LineFactEvidence =>
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
            record.ref.line.contains(binding.eventLine) &&
            payload.line.contains(binding.eventLine) &&
            payload.moveUci.exists(EvidenceRef.sameMove(_, rootMove)) &&
            tacticalMechanismDirectlyOwnsRoot(
              graph,
              fact,
              kind,
              binding,
              attributionKind,
              record
            )
        case payload: RelationFactEvidence =>
          payload.hasConcreteRelationProof &&
            payload.hasLineProof &&
            (
              payload.kind != RelationFactKind.PerpetualCheck ||
                record.ref.confidence == EvidenceConfidence.LegalReplayVerified
            ) &&
            record.ref.line.contains(binding.eventLine) &&
            payload.mentionsLineMove(rootMove) &&
            payload.rootGeometryConnected(rootMove) &&
            relationCanDirectlyProveCause(kind, payload)
        case payload: ThreatEpisodeEvidence =>
          val ownsDefensiveCause =
            defensiveCause(kind) &&
              payload.isProofSignalDefensivePressure &&
              record.ref.line.contains(binding.eventLine) &&
              threatEpisodeOwnsDefensiveCause(
                payload,
                kind,
                rootMove,
                fact.comparison.mover
              )
          val ownsMateThreat =
            kind == RelativeCauseKind.KingForcing &&
              record.ref.line.contains(binding.eventLine) &&
              RelativeCauseSignalProfile.currentMoveMateThreatRecord(
                record,
                rootMove,
                fact.comparison.mover
              )
          ownsDefensiveCause || ownsMateThreat
        case payload: StructuralDeltaEvidence =>
          record.referencesLine(binding.eventLine) &&
            payload.line.contains(binding.eventLine) &&
            normalizeMove(payload.moveUci) == normalizeMove(rootMove) &&
            RelativeCauseKind.structuralConsequences(kind, payload).nonEmpty
        case payload: StrategicMechanismEvidence =>
          !Set(RelativeCauseKind.PlanContradiction, RelativeCauseKind.PlanImprovement)(kind) &&
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
        case payload: PlanCausalEventEvidence =>
          planCausalEventDirectlyOwnsCause(graph, fact, kind, binding, record, payload)
        case payload: StrategicMechanismContrastEvidence =>
          !Set(RelativeCauseKind.PlanContradiction, RelativeCauseKind.PlanImprovement)(kind) &&
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
        case CandidateComparisonEvidence(_) =>
          comparisonDefensiveResourceDirectlyOwnsCause(
            graph,
            fact,
            kind,
            binding,
            attributionKind,
            record
          )
        case _ =>
          false)

  private def comparisonDefensiveResourceDirectlyOwnsCause(
      graph: TypedEvidenceGraph,
      fact: CandidateComparisonFact,
      kind: RelativeCauseKind,
      binding: RelativeCauseBinding,
      attributionKind: CauseAttributionKind,
      record: EvidenceRecord
  ): Boolean =
    record.payload match
      case CandidateComparisonEvidence(registered) =>
        val exactCandidateLineParents = record.parents.flatMap(graph.record).collect {
          case EvidenceRecord(ref, line: LineFactEvidence, _)
              if line.line == fact.candidateLine => ref
        }
        registered == fact &&
          kind == RelativeCauseKind.DefensiveResource &&
          fact.kind == CandidateComparisonKind.PlayedVsBest &&
          fact.comparison.verdict.isActionableLoss &&
          fact.defensiveRecaptureResource.nonEmpty &&
          binding.sourceSide == RelativeCauseSourceSide.Reference &&
          binding.eventLine == fact.referenceLine &&
          attributionKind == CauseAttributionKind.ReferenceCreatesResource &&
          record.ref.layer == EvidenceLayer.CandidateComparison &&
          exactCandidateLineParents.size == 1
      case _ => false

  private def planCausalEventDirectlyOwnsCause(
      graph: TypedEvidenceGraph,
      fact: CandidateComparisonFact,
      kind: RelativeCauseKind,
      binding: RelativeCauseBinding,
      record: EvidenceRecord,
      event: PlanCausalEventEvidence
  ): Boolean =
    record.ref.confidence != EvidenceConfidence.Heuristic &&
      RootOwnedEffectPolicy.planEventOwnsRoot(
        record.ref,
        event,
        binding.eventLine,
        fact.comparison.mover
      ) &&
      (
        if kind == RelativeCauseKind.WrongMoveOrder then
          ComparisonEndpointEffectObservationPolicy
            .exactInducedResponseMoveOrder(
              fact,
              binding.sourceSide,
              record.ref,
              event,
              graph
            )
            .nonEmpty
        else RelativeCauseKind.planCausalEventCanProveCause(kind, event)
      )

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
                ref.line.contains(binding.eventLine) &&
                motif.recordLineBound(ref) &&
                normalizeMove(motif.moveUci) == normalizeMove(rootMove) &&
                TacticalMechanismKind.fromMotif(motif.motif).contains(payload.kind)
            case EvidenceRecord(ref, line: LineFactEvidence, _) =>
              val consequenceSignals = signals.filter(_.kind == TacticalMechanismSignalKind.LineConsequence)
              val eventSignals = signals.filter(_.kind == TacticalMechanismSignalKind.LineEvent)
              val sourceConsequences = ownedConsequences.collect {
                case (ref, consequence) if ref.id == sourceId => consequence
              }
              val consequenceOwned =
                ref.line.contains(binding.eventLine) &&
                  line.line == binding.eventLine &&
                  consequenceSignals.nonEmpty &&
                  lineFactDirectlyOwnsCause(kind, line, rootMove, sourceConsequences)
              val eventOwned =
                ref.line.contains(binding.eventLine) &&
                  line.line == binding.eventLine &&
                  eventSignals.nonEmpty &&
                  lineFactDirectlyOwnsCause(kind, line, rootMove, Nil)
              consequenceOwned || eventOwned
            case EvidenceRecord(ref, relation: RelationFactEvidence, _) =>
              signals.exists(_.kind == TacticalMechanismSignalKind.Relation) &&
                ref.line.contains(binding.eventLine) &&
                relation.hasConcreteRelationProof &&
                relation.mentionsLineMove(rootMove) &&
                relation.rootGeometryConnected(rootMove) &&
                TacticalMechanismKind.fromRelation(relation.kind) == payload.kind &&
                relationCanDirectlyProveCause(kind, relation)
            case EvidenceRecord(ref, threat: ThreatEpisodeEvidence, _) =>
              signals.exists(_.kind == TacticalMechanismSignalKind.ThreatEpisode) &&
                ref.line.contains(binding.eventLine) &&
                threat.isProofSignalDefensivePressure &&
                (
                  if defensiveCause(kind) then
                    threatEpisodeOwnsDefensiveCause(
                      threat,
                      kind,
                      rootMove,
                      fact.comparison.mover
                    )
                  else
                    RelativeCauseSignalProfile.currentMoveMateThreatRecord(
                      EvidenceRecord(ref, threat),
                      rootMove,
                      fact.comparison.mover
                    )
                )
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
      rootMove: String,
      mover: chess.Color
  ): Boolean =
    payload.sideUnderPressure == mover &&
      (kind match
        case RelativeCauseKind.OnlyDefenseNecessity =>
          payload.onlyDefense.exists(EvidenceRef.sameMove(_, rootMove))
        case RelativeCauseKind.DefensiveResource =>
          payload.episode.bestDefense.exists(EvidenceRef.sameMove(_, rootMove))
        case RelativeCauseKind.DrawResource =>
          payload.onlyDefense.exists(EvidenceRef.sameMove(_, rootMove)) ||
            payload.episode.bestDefense.exists(EvidenceRef.sameMove(_, rootMove))
        case _ =>
          false)

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
      case RelativeCauseKind.KingForcing =>
        TacticalMechanismKind.fromRelation(payload.kind) == TacticalMechanismKind.KingForcing
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
      kind == RelativeCauseKind.DrawResource

  private def strategicCause(kind: RelativeCauseKind): Boolean =
    RelativeCauseKind.strategicContrastBacked(kind)

  private def recordMatchesEventRoot(
      record: EvidenceRecord,
      rootMove: String,
      mover: chess.Color
  ): Boolean =
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
        payload.mentionsLineMove(rootMove) && payload.rootGeometryConnected(rootMove)
      case payload: ThreatEpisodeEvidence =>
        (
          payload.sideUnderPressure == mover &&
            (
              payload.onlyDefense.exists(EvidenceRef.sameMove(_, rootMove)) ||
                payload.episode.bestDefense.exists(EvidenceRef.sameMove(_, rootMove))
            )
        ) ||
          (
            payload.episode.threatActor == mover &&
              payload.episode.motifs.exists(motif =>
                motif.plyIndex == 0 &&
                  motif.color == mover &&
                  motif.move.exists(EvidenceRef.sameMove(_, rootMove))
              )
          )
      case _: StrategicMechanismEvidence | _: StrategicMechanismContrastEvidence =>
        record.ref.line.exists(line => normalizeMove(line.rootMove) == normalizeMove(rootMove)) ||
          record.payloadLineRefs.exists(line => normalizeMove(line.rootMove) == normalizeMove(rootMove))
      case payload: PlanCausalEventEvidence =>
        record.ref.line.exists(line =>
          normalizeMove(line.rootMove) == normalizeMove(rootMove) &&
            RootOwnedEffectPolicy.planEventOwnsRoot(record.ref, payload, line, mover)
        )
      case CandidateComparisonEvidence(fact) =>
        fact.kind == CandidateComparisonKind.PlayedVsBest &&
          fact.defensiveRecaptureResource.nonEmpty &&
          EvidenceRef.sameMove(fact.referenceLine.rootMove, rootMove) &&
          fact.comparison.mover == mover
      case _ =>
        false

  private def normalizeMove(raw: String): String =
    Option(raw).getOrElse("").trim.toLowerCase

  private final case class CanonicalRelativeCauseEntry(
      record: EvidenceRecord,
      cause: RelativeCauseFact,
      semanticKey: RelativeCauseSemanticKey,
      truthView: RootOwnedEffectTruthView,
      inputIndex: Int
  )

  private[assembly] def canonicalizeRelativeCauseRecords(
      records: List[EvidenceRecord],
      graph: TypedEvidenceGraph,
      snapshotsByComparisonId: Map[String, ComparisonEndpointEvidenceSnapshot]
  ): List[EvidenceRecord] =
    val causeEntries = records.zipWithIndex.flatMap { case (record, index) =>
      relativeCauseFromRecord(record).map { cause =>
        val semanticKey = RelativeCauseSemanticKey.from(cause, record.ref.id, graph)
        val truthView = RootOwnedEffectTruthView.from(
          RelativeCauseConstructionAdmission.admittedDirectChannels(cause, graph)
        )
        CanonicalRelativeCauseEntry(record, cause, semanticKey, truthView, index)
      }
    }
    val causeIndexes = causeEntries.map(_.inputIndex).toSet
    val passThrough = records.zipWithIndex.collect {
      case (record, index) if !causeIndexes(index) => index -> record
    }
    val (ready, unready) = causeEntries.partition(_.semanticKey.sentenceReady)
    val mergedReady = ready
      .groupBy(_.semanticKey.frame)
      .values
      .toList
      .flatMap(overlappingCauseComponents)
      .map(component =>
        component.map(_.inputIndex).min -> mergeCauseComponent(
          component,
          graph,
          snapshotsByComparisonId
        )
      )
    val preservedUnready = unready.map(entry => entry.inputIndex -> entry.record)
    (passThrough ++ preservedUnready ++ mergedReady).sortBy(_._1).map(_._2)

  /** Connected components, not pairwise winners: {A,B} and {B,C} must merge
    * together so disagreement on B is still visible when the complete direct
    * channel set is canonicalized. Disjoint meanings in one frame remain
    * separate Causes.
    */
  private def overlappingCauseComponents(
      entries: List[CanonicalRelativeCauseEntry]
  ): List[List[CanonicalRelativeCauseEntry]] =
    def grow(
        component: List[CanonicalRelativeCauseEntry],
        remaining: List[CanonicalRelativeCauseEntry]
    ): (List[CanonicalRelativeCauseEntry], List[CanonicalRelativeCauseEntry]) =
      val signatures = component.flatMap(_.truthView.causalSignatures).toSet
      val (overlapping, rest) = remaining.partition(entry =>
        entry.truthView.causalSignatures.exists(signatures)
      )
      if overlapping.isEmpty then component -> rest
      else grow(component ++ overlapping, rest)

    def collect(
        pending: List[CanonicalRelativeCauseEntry],
        components: List[List[CanonicalRelativeCauseEntry]]
    ): List[List[CanonicalRelativeCauseEntry]] =
      pending match
        case Nil => components.reverse
        case head :: tail =>
          val (component, rest) = grow(List(head), tail)
          collect(rest, component.sortBy(_.inputIndex) :: components)

    collect(entries.sortBy(_.inputIndex), Nil)

  private def mergeCauseComponent(
      component: List[CanonicalRelativeCauseEntry],
      graph: TypedEvidenceGraph,
      snapshotsByComparisonId: Map[String, ComparisonEndpointEvidenceSnapshot]
  ): EvidenceRecord =
    val ordered = component.sortBy(_.inputIndex)
    val carrier = ordered.head
    require(
      ordered.map(_.semanticKey.frame).distinct.size == 1,
      "relative Cause records may merge only after semantic-frame agreement"
    )
    val causes = ordered.map(_.cause)
    val mergedProofs = causes.flatMap(_.proof)
    val admittedBeforeMerge = ordered.flatMap(_.truthView.causalSignatures).toSet
    val mergedBeforeAdmissionValidation = carrier.cause.copy(
      supportEvidence = causes.flatMap(_.supportEvidence).distinctBy(_.id),
      proof = Option.when(mergedProofs.nonEmpty)(RelativeCauseProof.merge(mergedProofs)),
      directEffectAdmission = DirectEffectAdmission.Restricted(admittedBeforeMerge)
    )
    val mergedPubliclyAdmissibleSignatures =
      snapshotsByComparisonId
        .get(mergedBeforeAdmissionValidation.comparisonEvidence.id)
        .filter(snapshot =>
          snapshot.comparisonEvidence == mergedBeforeAdmissionValidation.comparisonEvidence &&
            graph.comparisonFor(mergedBeforeAdmissionValidation).contains(snapshot.comparison)
        )
        .toList
        .flatMap(snapshot =>
          RelativeCauseConstructionAdmission
            .admittedDirectChannels(mergedBeforeAdmissionValidation, graph)
            .filter(channel =>
              ComparisonEndpointEffectObservationPolicy
                .uniqueNeutralWitnessFor(
                  snapshot,
                  mergedBeforeAdmissionValidation.sourceSide,
                  channel,
                  graph
                )
                .nonEmpty
            )
        )
        .map(_.causalSignature)
        .toSet
    val mergedCause = mergedBeforeAdmissionValidation.copy(
      directEffectAdmission = DirectEffectAdmission.Restricted(
        mergedPubliclyAdmissibleSignatures
      )
    )
    carrier.record.copy(
      payload = RelativeCauseFactEvidence(mergedCause),
      parents = ordered.flatMap(_.record.parents).distinctBy(_.id)
    )

  private def relativeCauseFromRecord(record: EvidenceRecord): Option[RelativeCauseFact] =
    record.payload match
      case RelativeCauseFactEvidence(cause) => Some(cause)
      case _                                => None

  private def mergeCauseCandidates(candidates: List[RelativeCauseDraft]): List[RelativeCauseDraft] =
    candidates
      .groupBy(candidate =>
        (
          candidate.kind,
          candidate.sourceSide,
          candidate.attributionKind,
          candidate.intent,
          supportSignature(candidate.support)
        )
      )
      .toList
      .sortBy { case ((kind, sourceSide, attributionKind, intent, signature), _) =>
        (
          kind.toString,
          sourceSide.map(_.toString).getOrElse("none"),
          attributionKind.toString,
          intent.toString,
          signature
        )
      }
      .map { case ((kind, sourceSide, attributionKind, intent, _), grouped) =>
        val first = grouped.headOption
        RelativeCauseDraft(
          kind,
          first.map(_.support).getOrElse(Nil),
          sourceSide,
          attributionKind,
          intent
        )
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
      comparisonRecord: EvidenceRecord,
      endpointSnapshot: ComparisonEndpointEvidenceSnapshot,
      graph: TypedEvidenceGraph
  ): List[RelativeCauseDraft] =
    val endpointMoveOrderRecords =
      Option
        .when(
          endpointSnapshot.comparison == fact &&
            endpointSnapshot.comparisonEvidence == comparisonRecord.ref
        )(endpointSnapshot)
        .toList
        .flatMap(snapshot =>
          List(
            RelativeCauseSourceSide.Reference ->
              ComparisonEndpointEffectObservationPolicy
                .exactInducedResponseMoveOrderRecords(
                  snapshot,
                  RelativeCauseSourceSide.Reference,
                  graph
                ),
            RelativeCauseSourceSide.Candidate ->
              ComparisonEndpointEffectObservationPolicy
                .exactInducedResponseMoveOrderRecords(
                  snapshot,
                  RelativeCauseSourceSide.Candidate,
                  graph
                )
          )
        )
        .toMap
    val profile =
      RelativeCauseSignalProfile.from(
        fact = fact,
        referenceRecords = neighborhood.referenceRecords,
        candidateRecords = neighborhood.candidateRecords,
        sharedRecords = neighborhood.sharedRecords,
        referenceEndpointMoveOrderRecords =
          endpointMoveOrderRecords.getOrElse(RelativeCauseSourceSide.Reference, Nil),
        candidateEndpointMoveOrderRecords =
          endpointMoveOrderRecords.getOrElse(RelativeCauseSourceSide.Candidate, Nil)
      )
    RelativeCauseDraftPlanner.drafts(profile, comparisonRecord)

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
