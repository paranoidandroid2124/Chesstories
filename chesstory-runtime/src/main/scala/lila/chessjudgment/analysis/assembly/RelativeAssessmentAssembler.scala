package lila.chessjudgment.analysis.assembly

import chess.{ Color, Square }
import lila.chessjudgment.analysis.transition.TransitionFactNormalizer
import lila.chessjudgment.model.line.{ CandidateLineEvaluation, PrincipalVariationEvidence }
import lila.chessjudgment.model.judgment.*

object RelativeAssessmentAssembler:

  final private[assembly] case class StrategicAxisEntry(
      axis: StrategicAxisDetail,
      sources: List[EvidenceRef],
      planResults: List[StrategicAxisPlanResultBinding]
  )

  final private case class StrategicAxisContribution(
      axis: StrategicAxisDetail,
      signalKind: StrategicMechanismSignalKind,
      signalLabel: String,
      source: EvidenceRef,
      resultAssessments: List[PlanCausalResultAssessment],
      sources: List[EvidenceRef]
  ):
    def carrierIdentity: (String, StrategicMechanismSignalKind, String, EvidenceRef) =
      (axis.stableKey, signalKind, signalLabel, source)

  final private case class ComparisonEvidenceNeighborhood(
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

  final private case class RelativeCauseProofRecords(
      directProof: List[EvidenceRecord],
      contrastProof: List[EvidenceRecord],
      contextSupport: List[EvidenceRecord]
  ):
    def all: List[EvidenceRecord] =
      (directProof ++ contrastProof ++ contextSupport).distinctBy(_.ref.id)

  final private case class RelativeCauseConstructionCandidate(
      cause: RelativeCauseFact,
      evidenceId: String,
      binding: RelativeCauseBinding,
      parents: List[EvidenceRef]
  )

  final private case class ComparisonEndpointEffectInventoryPair(
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

  /** The unresolved form of one exact PlanResult. Robustness and branch
    * outcomes are deliberately omitted because those are precisely what a
    * pending branch census has not established. Distinct result targets,
    * causal functions, and source/response timing remain independent.
    */
  final private[analysis] case class PlanResultEndpointInventory(
      observations: Set[ComparisonEndpointEffectObservation],
      enumeratedPlanIds: Set[String],
      incompleteResultIdentities: Set[PlanResultSemanticIdentity],
      extractionReady: Boolean,
      exactInducedResponseObservations: Map[EvidenceRef, Set[ComparisonEndpointEffectObservation]] = Map.empty
  ):
    /** Preserves the F-stage PlanCausal carrier that produced an exact
      * comparison-induced observation. Observation values omit the carrier
      * EvidenceRef, so consumers needing direct-proof identity must use this
      * mapping instead of set membership.
      */
    def exactInducedResponseObservationFor(
        source: EvidenceRef
    ): Set[ComparisonEndpointEffectObservation] =
      exactInducedResponseObservations.getOrElse(source, Set.empty)

    def uniqueObservationFor(
        scope: ComparisonEndpointEffectScopeKey
    ): Option[Option[ComparisonEndpointEffectObservation]] =
      (scope.effectIdentity.planIds.distinct, scope.effectIdentity.planResult) match
        case (planId :: Nil, Some(identity))
            if extractionReady && enumeratedPlanIds(planId) && !incompleteResultIdentities(identity) =>
          ComparisonEndpointEffectObservationPolicy.uniqueObservationFor(
            ComparisonEndpointEffectInventory.Complete(observations),
            scope
          )
        case _ =>
          None

    /** Counterfactual lookup is complete only when all carriers resolved.
      * Every match must own an explicit root/result reversal and exact paired
      * dependency proof; absence is never inferred through a lossy key.
      */
    def uniqueCounterfactualMagnitudeFor(
        comparison: CandidateComparisonFact,
        sourceSide: RelativeCauseSourceSide,
        sourceObservation: ComparisonEndpointEffectObservation,
        requiredAbsentPlanId: String
    ): Option[Option[ComparisonEndpointEffectMagnitude]] =
      if !extractionReady || incompleteResultIdentities.nonEmpty then None
      else
        val matchedMagnitudes = observations.toList.flatMap(observation =>
          ComparisonEndpointEffectObservationPolicy
            .planResultCounterfactualCorrespondence(
              comparison,
              sourceSide,
              sourceObservation,
              observation
            )
            .map(_ => observation.magnitude)
        ).distinct
        matchedMagnitudes match
          case exact :: Nil => Some(Some(exact))
          case Nil if enumeratedPlanIds(requiredAbsentPlanId) => Some(None)
          case _ => None

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
        comparison: CandidateComparisonFact,
        sourceSide: RelativeCauseSourceSide,
        source: EvidenceRef,
        sourceObservation: ComparisonEndpointEffectObservation
    ): Boolean =
      val policy = ComparisonEndpointEffectObservationPolicy
      if sourceInventory.exactInducedResponseObservationFor(source)(sourceObservation) then
        (for
          sourcePlanId <- sourceObservation.scope.effectIdentity.planIds.distinct match
            case exact :: Nil => Some(exact)
            case _ => None
          counterpartLookup <- counterpartInventory.uniqueCounterfactualMagnitudeFor(
            comparison,
            sourceSide,
            sourceObservation,
            sourcePlanId
          )
        yield counterpartLookup match
          case None => true
          case Some(counterpartMagnitude) =>
            policy.compareMagnitude(sourceObservation.magnitude, counterpartMagnitude) ==
              policy.MagnitudeRelation.LeftStrictlyStronger
        ).getOrElse(false)
      else if sourceInventory.exactInducedResponseObservations.contains(source) then false
      else
        (for
          sourceLookup <- sourceInventory.uniqueObservationFor(sourceObservation.scope)
          observedSource <- sourceLookup
          if observedSource == sourceObservation
          counterpartLookup <- counterpartInventory.uniqueObservationFor(sourceObservation.scope)
        yield counterpartLookup match
          case None => true
          case Some(counterpartObservation) =>
            policy.compareMagnitude(sourceObservation.magnitude, counterpartObservation.magnitude) ==
              policy.MagnitudeRelation.LeftStrictlyStronger
        ).getOrElse(false)

  final private[analysis] case class PlanResultEndpointInventoryPair(
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

  final private case class ComparisonEndpointEffectInventories(
      lineMaterial: ComparisonEndpointEffectInventoryPair,
      lineMate: ComparisonEndpointEffectInventoryPair,
      lineQualitative: ComparisonEndpointEffectInventoryPair,
      strategic: ComparisonEndpointEffectInventoryPair,
      planResult: PlanResultEndpointInventoryPair,
      endpointSnapshot: ComparisonEndpointEvidenceSnapshot
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
                    ) =>
                  Some(lineMaterial)
                case DirectEffectMagnitudeKnowledge.Exact(
                      DirectCauseImportanceMeasure.MateArrival(_)
                    ) =>
                  Some(lineMate)
                case DirectEffectMagnitudeKnowledge.NotApplicable => Some(lineQualitative)
                case DirectEffectMagnitudeKnowledge.Exact(_) => None
                case DirectEffectMagnitudeKnowledge.ExpectedButMissing |
                    DirectEffectMagnitudeKnowledge.Ambiguous =>
                  None
            case RootOwnedEffectPrimitiveKind.RootLineEvent =>
              channel.rootOwnedProof match
                case Some(RootOwnedEffectProof.RootLineEvent(_, _, event))
                    if event.kind == LineEventKind.Mate =>
                  Some(lineMate)
                case Some(RootOwnedEffectProof.RootLineEvent(_, _, _)) => Some(lineQualitative)
                case _ => None
            case RootOwnedEffectPrimitiveKind.RootRelation | RootOwnedEffectPrimitiveKind.PlanResult |
                RootOwnedEffectPrimitiveKind.Unspecified =>
              None
      }

  final private case class RelativeAssemblyInputs(
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
      .flatMap { inputs =>
        val completeCandidateSet = inputs.input.completeCandidateSet
        val candidateSet = completeCandidateSet.map(_.descriptor)
        candidateComparisonRecords(
          mover = inputs.mover,
          context = context,
          reference = inputs.reference,
          candidate = inputs.candidate,
          candidateSet = candidateSet,
          completeCandidateSet = completeCandidateSet,
          root = inputs.root,
          allocator = inputs.allocator
        ).flatMap { comparisonRecords =>
          primaryComparisonRecord(comparisonRecords, inputs.reference, inputs.candidate).map { _ =>
            val factContext = context.withEvidence(comparisonRecords)
            val strategicContrastRecords =
              EvidenceFactAssembler.exactStrategicMechanisms(factContext).toList.flatMap { _ =>
                comparisonRecords.flatMap { record =>
                  strategicMechanismContrastRecords(
                    factContext,
                    inputs.root,
                    inputs.allocator,
                    record
                  )
                }
              }
            factContext.withEvidence(strategicContrastRecords)
          }
        }
      }
      .getOrElse(context)

  def enrichCauses(context: JudgmentAssemblyContext): JudgmentAssemblyContext =
    relativeAssemblyInputs(context)
      .flatMap { inputs =>
        val comparisonRecords = assembledComparisonRecords(context, inputs.root)
        primaryComparisonRecord(comparisonRecords, inputs.reference, inputs.candidate).map {
          primaryComparisonRecord =>
            val snapshotsByComparisonId = comparisonEndpointEvidenceSnapshots(context)
              .map(snapshot => snapshot.comparisonEvidence.id -> snapshot)
              .toMap
            val rawCauseRecords =
              comparisonRecords.flatMap(record =>
                snapshotsByComparisonId
                  .get(record.ref.id)
                  .toList
                  .flatMap(snapshot =>
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
            val playedMoveSet = Set(EvidenceRef.normalizeMove(inputs.played.moveUci))
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
                relatedComparisonEvidence =
                  comparisonRecords.map(_.ref).filterNot(_.id == primaryComparisonRecord.ref.id),
                relativeCauseEvidence = playedCauseRecords.map(_.ref)
              )
            val assessmentRecord = TransitionFactNormalizer.fromRelativeAssessment(assessment)
            context.withEvidence(causeRecords :+ assessmentRecord)
        }
      }
      .getOrElse(context)

  private[assembly] def exactStrategicContrasts(
      context: JudgmentAssemblyContext
  ): Option[List[EvidenceRecord]] =
    EvidenceFactAssembler.exactStrategicMechanisms(context).flatMap { _ =>
      val actual = context.evidenceGraph.records.collect {
        case record @ EvidenceRecord(_, _: StrategicMechanismContrastEvidence, _) => record
      }
      relativeAssemblyInputs(context) match
        case Some(_) =>
          Option.when(
            actual.forall(context.evidenceGraph.proofEligible) &&
              sourcesPrecedeContrasts(context, actual)
          )(actual)
        case None =>
          Option.when(actual.isEmpty)(Nil)
    }

  private def sourcesPrecedeContrasts(
      context: JudgmentAssemblyContext,
      expected: List[EvidenceRecord]
  ): Boolean =
    expected
      .flatMap(_.parents)
      .distinctBy(_.id)
      .forall(source =>
        context.evidenceGraph
          .record(source)
          .exists(record =>
            context.evidenceGraph.parentClosure(record).forall { parent =>
              !parent.payload.isInstanceOf[StrategicMechanismContrastEvidence]
            }
          )
      )

  /** Public read-only reconstruction used identically by C admission and the
    * runtime adapter. Every snapshot is derived from the F graph before any
    * provisional Cause exists.
    */
  def comparisonEndpointEvidenceSnapshots(
      context: JudgmentAssemblyContext
  ): List[ComparisonEndpointEvidenceSnapshot] =
    (for
      wrappers <- EvidenceFactAssembler.exactStrategicMechanisms(context).toList
      contrasts <- exactStrategicContrasts(context).toList
      inputs <- relativeAssemblyInputs(context).toList
    yield assembledComparisonRecords(context, inputs.root).flatMap { record =>
      record.payload match
        case CandidateComparisonEvidence(comparison) =>
          val neighborhood = comparisonNeighborhood(
            context,
            inputs.root,
            comparison,
            Some(inputs.input)
          )
          val tacticalCarrierRecords = context.evidenceGraph.records.collect {
            case record @ EvidenceRecord(ref, _: TacticalMechanismEvidence, _)
                if carrierAtComparisonRoot(ref, inputs.root) &&
                  (ref.line.contains(comparison.referenceLine) ||
                    ref.line.contains(comparison.candidateLine)) =>
              record
          }
          val strategicCarrierRecords =
            (
              contrasts.filter {
                case EvidenceRecord(ref, payload: StrategicMechanismContrastEvidence, _) =>
                  carrierAtComparisonRoot(ref, inputs.root) &&
                  payload.comparisonKind == comparison.kind &&
                  payload.referenceLine == comparison.referenceLine &&
                  payload.candidateLine == comparison.candidateLine
                case _ => false
              } ++
                wrappers.filter { record =>
                  carrierAtComparisonRoot(record.ref, inputs.root) &&
                  (record.ref.line.contains(comparison.referenceLine) ||
                    record.ref.line.contains(comparison.candidateLine))
                }
            ).distinctBy(_.ref.id)
          val carrierRecords =
            (tacticalCarrierRecords ++ strategicCarrierRecords).distinctBy(_.ref.id)
          val involvedRecords =
            (neighborhood.involvedRecords ++ carrierRecords).distinctBy(_.ref.id)
          Some(
            ComparisonEndpointEvidenceSnapshot(
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
            )
          )
        case _ => None
    }).flatten

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

  private def primaryComparisonRecord(
      records: List[EvidenceRecord],
      reference: CandidateLineNode,
      candidate: CandidateLineNode
  ): Option[EvidenceRecord] =
    val playedIsBest = EvidenceRef.sameMove(reference.ref.rootMove, candidate.ref.rootMove)
    records.collect {
      case record @ EvidenceRecord(_, CandidateComparisonEvidence(fact), _)
          if !playedIsBest &&
            fact.kind == CandidateComparisonKind.PlayedVsBest &&
            fact.referenceLine == reference.ref &&
            fact.candidateLine == candidate.ref =>
        record
      case record @ EvidenceRecord(_, CandidateComparisonEvidence(fact), _)
          if playedIsBest &&
            fact.kind == CandidateComparisonKind.BestVsSecond &&
            fact.referenceLine == reference.ref &&
            fact.candidateLine.role == LineNodeRole.Alternative &&
            fact.candidateLine.rank == 2 &&
            fact.hasDistinctRootMoves &&
            fact.candidateSet.nonEmpty =>
        record
    } match
      case record :: Nil => Some(record)
      case _ => None

  private def candidateComparisonRecords(
      mover: Color,
      context: JudgmentAssemblyContext,
      reference: CandidateLineNode,
      candidate: CandidateLineNode,
      candidateSet: Option[CandidateSetDescriptor],
      completeCandidateSet: Option[CompleteCandidateSet],
      root: PositionNodeRef,
      allocator: JudgmentProvenanceAllocator
  ): Option[List[EvidenceRecord]] =
    val ordered = context.lines
      .filterNot(_.role == LineNodeRole.Threat)
      .sortBy(_.ref.rank)
      .distinctBy(line => EvidenceRef.normalizeMove(line.ref.rootMove))
    val second = completeCandidateSet.map { complete =>
      if !EvidenceRef.sameMove(complete.bestMoveUci, reference.ref.rootMove) then
        throw IllegalStateException("complete candidate-set best move does not match the reference line")
      ordered
        .find(line => EvidenceRef.sameMove(line.ref.rootMove, complete.secondMoveUci))
        .getOrElse(throw IllegalStateException("complete candidate-set second move was not projected"))
    }
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
        Option.unless(EvidenceRef.sameMove(reference.ref.rootMove, candidate.ref.rootMove))(
          (
            CandidateComparisonKind.PlayedVsBest,
            reference,
            candidate,
            Option.when(candidateIsSecond)(candidateSet).flatten
          )
        ),
        second
          .filterNot(_ => candidateIsSecond)
          .map(line =>
            (
              CandidateComparisonKind.BestVsSecond,
              reference,
              line,
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
              None
            )
          ) ++
            Option
              .unless(
                second.exists(line => EvidenceRef.sameMove(line.ref.rootMove, alternative.ref.rootMove))
              )(
                (
                  CandidateComparisonKind.ReferenceVsAlternative,
                  reference,
                  alternative,
                  None
                )
              )
              .toList
        }
    val descriptorParents =
      completeCandidateSet.toList
        .flatMap(_ => reference :: second.toList)
        .flatMap(parentsForLine(context, _))
        .distinctBy(_.id)
    val records = comparisons
      .distinctBy { case (kind, ref, cand, _) => (kind, ref.ref.id, cand.ref.id) }
      .zipWithIndex
      .flatMap { case ((kind, refLine, candLine, descriptor), index) =>
        CandidateComparisonFact.fromLines(kind, mover, refLine, candLine, descriptor).map { baseComparison =>
          TransitionFactNormalizer.fromCandidateComparison(
            id = allocator.evidenceId(
              s"candidate-comparison:${allocator.key(kind)}:${allocator.key(refLine.ref.rootMove)}:${allocator.key(candLine.ref.rootMove)}:$index"
            ),
            comparison = baseComparison,
            position = root,
            scope = EvidenceScope.Counterfactual,
            parents = (
              parentsForLine(context, refLine) ++
                parentsForLine(context, candLine) ++
                Option.when(descriptor.nonEmpty)(descriptorParents).toList.flatten
            ).distinctBy(_.id)
          )
        }
      }
    val candidateSetParent =
      records.collectFirst {
        case EvidenceRecord(ref, CandidateComparisonEvidence(fact), _) if fact.candidateSet.nonEmpty =>
          ref
      }
    Option.when(records.size == comparisons.distinctBy { case (kind, ref, cand, _) =>
      (kind, ref.ref.id, cand.ref.id)
    }.size)(records.map {
      case record @ EvidenceRecord(_, CandidateComparisonEvidence(fact), parents)
          if fact.kind == CandidateComparisonKind.PlayedVsBest && fact.candidateSet.isEmpty =>
        record.copy(parents = (parents ++ candidateSetParent).distinctBy(_.id))
      case record =>
        record
    })

  private def strategicMechanismContrastRecords(
      context: JudgmentAssemblyContext,
      root: PositionNodeRef,
      allocator: JudgmentProvenanceAllocator,
      comparisonRecord: EvidenceRecord
  ): List[EvidenceRecord] =
    comparisonRecord.payload match
      case CandidateComparisonEvidence(fact) =>
        val neighborhood = comparisonNeighborhood(context, root, fact)
        val referenceProfile = strategicAxisProfile(context.evidenceGraph, neighborhood.referenceRecords)
        val candidateProfile = strategicAxisProfile(context.evidenceGraph, neighborhood.candidateRecords)
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
        val barePayload = StrategicMechanismContrastEvidence(
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
            fact.hasDistinctRootMoves && barePayload.hasActionableContrast
          ) {
            val ref = EvidenceRef(
              id = allocator.evidenceId(
                s"strategic-contrast:${allocator.key(fact.kind)}:${allocator.key(fact.referenceLine.rootMove)}:${allocator.key(fact.candidateLine.rootMove)}"
              ),
              producer = EvidenceProducer.StrategicMechanismProducer,
              layer = EvidenceLayer.StrategicMechanism,
              position = root,
              line = Some(fact.candidateLine),
              scope = EvidenceScope.Counterfactual,
              confidence = fact.verdictConfidence.evidenceConfidence
            )
            val parents = (comparisonRecord.ref :: support.all).distinctBy(_.id)
            val payload = barePayload.copy(
              assemblyProof = Some(
                StrategicMechanismContrastAssemblyProof.from(ref, parents, barePayload)
              )
            )
            EvidenceRecord(
              ref = ref,
              payload = payload,
              parents = parents
            )
          }
          .toList
      case _ =>
        Nil

  private[assembly] def strategicAxisProfile(
      graph: TypedEvidenceGraph,
      records: List[EvidenceRecord]
  ): Map[String, StrategicAxisEntry] =
    val projected = records
      .collect { case record @ EvidenceRecord(ref, payload: StrategicMechanismEvidence, _)
          if graph.proofEligible(record) =>
        payload.signals.flatMap { signal =>
          signal.axis.map { axis =>
            val contribution = for
              sourceRecord <- graph.record(signal.source)
              if sourceRecord.ref == signal.source
              if record.parents.contains(signal.source)
            yield StrategicAxisContribution(
              axis = axis,
              signalKind = signal.kind,
              signalLabel = signal.label,
              source = signal.source,
              resultAssessments = signal.planResultAssessment.toList,
              sources = List(ref, signal.source)
            )
            axis.stableKey -> contribution
          }
        }
      }
      .flatten

    projected.groupBy(_._1).flatMap { case (axisKey, entries) =>
      val carrierGroups = entries.flatMap(_._2).groupBy(_.carrierIdentity).values.toList
      carrierGroups.headOption.map { _ =>
        val carriers = carrierGroups.map { duplicates =>
          val first = duplicates.head
          first.copy(
            resultAssessments = duplicates.flatMap(_.resultAssessments).distinct,
            sources = duplicates.flatMap(_.sources).distinctBy(_.id).sortBy(_.id)
          )
        }
        val first = carriers.head
        axisKey -> StrategicAxisEntry(
          axis = first.axis,
          sources = carriers.flatMap(_.sources).distinctBy(_.id).sortBy(_.id),
          planResults = carriers
            .flatMap(carrier =>
              carrier.resultAssessments.map(assessment =>
                StrategicAxisPlanResultBinding(carrier.source, assessment)
              )
            )
            .distinct
        )
      }
    }

  private def strategicAxisComparisons(
      referenceProfile: Map[String, StrategicAxisEntry],
      candidateProfile: Map[String, StrategicAxisEntry]
  ): List[StrategicAxisComparison] =
    (referenceProfile.keySet ++ candidateProfile.keySet).toList.sorted.map { key =>
      val reference = referenceProfile.get(key)
      val candidate = candidateProfile.get(key)
      val axis = reference.orElse(candidate).map(_.axis).get
      StrategicAxisComparison(
        axis = axis,
        outcome = strategicAxisComparisonOutcome(axis, reference.nonEmpty, candidate.nonEmpty),
        referenceSources = reference.map(_.sources).getOrElse(Nil),
        candidateSources = candidate.map(_.sources).getOrElse(Nil),
        referencePlanResults = reference.map(_.planResults).getOrElse(Nil),
        candidatePlanResults = candidate.map(_.planResults).getOrElse(Nil)
      )
    }

  private def strategicAxisComparisonOutcome(
      axis: StrategicAxisDetail,
      referencePresent: Boolean,
      candidatePresent: Boolean
  ): StrategicAxisComparisonOutcome =
    val candidateNegative =
      axis.polarity == StrategicAxisPolarity.Concede
    if referencePresent && !candidatePresent then
      StrategicAxisComparisonOutcome.ReferenceOnly
    else if candidatePresent && !referencePresent then
      if candidateNegative then StrategicAxisComparisonOutcome.CandidateConcession
      else StrategicAxisComparisonOutcome.CandidateOnly
    else StrategicAxisComparisonOutcome.SharedSustained

  private def strategicPlanComparison(
      axisComparisons: List[StrategicAxisComparison]
  ): Option[StrategicPlanComparison] =
    val planAxis = axisComparisons.filter(_.axis.kind == StrategicAxisKind.PlanCoherence)
    Option.when(planAxis.nonEmpty) {
      val referencePlans =
        planAxis.filter(_.referenceSources.nonEmpty).map(_.axis.label).distinct.sorted
      val candidatePlans =
        planAxis.filter(_.candidateSources.nonEmpty).map(_.axis.label).distinct.sorted
      StrategicPlanComparison(referencePlans, candidatePlans)
    }

  private def strategicSustainability(
      context: JudgmentAssemblyContext,
      fact: CandidateComparisonFact,
      axisComparisons: List[StrategicAxisComparison]
  ): StrategicSustainabilityAssessment =
    val referencePlyCount =
      context.lines.find(_.ref == fact.referenceLine).map(_.evaluation.moves.size).getOrElse(0)
    val candidatePlyCount =
      context.lines.find(_.ref == fact.candidateLine).map(_.evaluation.moves.size).getOrElse(0)
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
      case record @ EvidenceRecord(_, PlanTransitionEvidence(proof), parents)
          if parents.exists(parent => axisLeafIds(parent.id)) &&
            strategicAxisTransitionMatchesPlan(comparison.axis, proof.summary) =>
        record
    }
    (axisLeafRefs.flatMap(graph.record) ++ matchingTransitionRecords)
      .distinctBy(_.ref.id)
      .exists {
        case EvidenceRecord(_, payload: PlanCausalEventEvidence, _) =>
          strategicAxisEventMatchesPlan(comparison.axis, payload) &&
          payload.observedGoalResultRoutes.exists(route =>
            route.sourceEvent.step.ply > payload.causalEpisode.root.step.ply
          )
        case EvidenceRecord(_, PlanTransitionEvidence(proof), _) =>
          proof.summary.continuity.exists(_.consecutivePlies >= 2)
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
      axisComparisons
        .flatMap { comparison =>
          if comparison.candidateLead then comparison.candidateSources
          else if comparison.referenceLead then comparison.referenceSources
          else comparison.sources
        }
        .distinctBy(_.id)
    val contrastSources =
      axisComparisons
        .flatMap { comparison =>
          if comparison.candidateLead then comparison.referenceSources
          else if comparison.referenceLead then comparison.candidateSources
          else Nil
        }
        .distinctBy(_.id)
    val contextSources =
      sharedRecords
        .collect {
          case EvidenceRecord(ref, payload: StrategicMechanismEvidence, _) if payload.hasStrategicAxis => ref
        }
        .distinctBy(_.id)
    StrategicContrastSupport(
      directSources = directSources,
      contrastSources = contrastSources,
      contextSources = contextSources
    )

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
          val attributionKind = candidate.attributionKind
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
            causeAttribution(
              context.evidenceGraph,
              candidate,
              binding,
              causalProofRecords,
              fact.comparison.mover
            )
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
              (attribution.directProofEligible && context.evidenceGraph
                .relativeCauseProofHasRawTypedDepth(kind, proof)) ||
                context.evidenceGraph.relativeCauseProofHasRawContextSupport(proof)
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
          RelativeCauseConstructionCandidate(
            cause = cause,
            evidenceId = causeEvidenceId,
            binding = binding,
            parents = (comparisonRecord.ref :: (supportRefs ++ proofRecords.all.map(_.ref))).distinctBy(_.id)
          )
        }
        restrictDirectEffectsForComparison(
          provisional,
          neighborhood,
          fact,
          comparisonRecord.ref.position,
          context.evidenceGraph,
          endpointSnapshot
        )
      case _ => Nil

  private def restrictDirectEffectsForComparison(
      provisional: List[RelativeCauseConstructionCandidate],
      neighborhood: ComparisonEvidenceNeighborhood,
      comparison: CandidateComparisonFact,
      rootPosition: PositionNodeRef,
      graph: TypedEvidenceGraph,
      endpointSnapshot: ComparisonEndpointEvidenceSnapshot
  ): List[EvidenceRecord] =
    val inventories = comparisonEndpointEffectInventories(
      neighborhood,
      comparison,
      comparisonRecordPosition = rootPosition,
      graph = graph,
      endpointSnapshot = endpointSnapshot
    )
    val rawChannels =
      provisional.map(item => EvidenceObjectBinding.rawDirectSentenceChannelsForProjection(item.cause, graph))

    provisional.zip(rawChannels).flatMap { case (item, channels) =>
      val admittedOccurrences = channels
        .filter { channel =>
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
                    inventories
                  )
              )
            }
        }
        .map(_.exactOccurrenceFingerprint)
        .toSet
      val restrictedCause = item.cause.copy(
        directEffectAdmission = DirectEffectAdmission.Restricted(admittedOccurrences)
      )
      Option
        .when(RelativeCauseConstructionAdmission.initiallyReady(restrictedCause, graph))(
          RelativeCauseConstructionAdmission.admittedDirectChannels(restrictedCause, graph)
        )
        .flatMap(proofConfidenceForAdmittedChannels)
        .map { confidence =>
          TransitionFactNormalizer.fromRelativeCause(
            id = item.evidenceId,
            cause = restrictedCause,
            binding = item.binding,
            position = rootPosition,
            scope = EvidenceScope.Counterfactual,
            confidence = confidence,
            parents = item.parents
          )
        }
    }

  private def proofConfidenceForAdmittedChannels(
      channels: List[DirectCauseChannel]
  ): Option[EvidenceConfidence] =
    channels.map(_.binding.source.confidence).distinct match
      case Nil => None
      case confidence :: Nil => Some(confidence)
      case _ => Some(EvidenceConfidence.Mixed)

  /** Draft intent classifies how a Cause was generated. It is deliberately
    * absent here: public admission is decided only by owned fact inventories,
    * attribution, and comparison semantics.
    */
  private def differentialEndpointEffectAdmitted(
      cause: RelativeCauseFact,
      channel: DirectCauseChannel,
      neutralWitness: ComparisonEndpointEvidenceWitness,
      inventories: ComparisonEndpointEffectInventories
  ): Boolean =
    val policy = ComparisonEndpointEffectObservationPolicy
    channel.rootOwnedEffectDescriptor match
      case Some(descriptor) if descriptor.identity.primitiveKind == RootOwnedEffectPrimitiveKind.PlanResult =>
        (for
          (sourceInventory, counterpartInventory) <- inventories.planResult.forSide(cause.sourceSide)
          sourceObservation <- neutralWitness.observation
        yield PlanResultDifferentialPolicy.admitted(
          sourceInventory,
          counterpartInventory,
          inventories.endpointSnapshot.comparison,
          cause.sourceSide,
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
              policy.MagnitudeRelation.LeftStrictlyStronger
        ).getOrElse(false)

  private def comparisonEndpointEffectInventories(
      neighborhood: ComparisonEvidenceNeighborhood,
      comparison: CandidateComparisonFact,
      comparisonRecordPosition: PositionNodeRef,
      graph: TypedEvidenceGraph,
      endpointSnapshot: ComparisonEndpointEvidenceSnapshot
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
      strategic = strategicEndpointInventories(
        graph,
        neighborhood,
        comparison,
        comparisonRecordPosition
      ),
      planResult = planResultEndpointInventories(
        graph,
        comparison,
        comparisonRecordPosition
      ),
      endpointSnapshot = endpointSnapshot
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
    * closed material outcome cannot erase mate/endgame facts, and a missing or
    * inconsistent engine mate score cannot erase material/endgame facts.
    */
  private[assembly] def enforceLineFamilyCompleteness(
      inventories: ComparisonEndpointLineEffectInventories,
      payload: LineFactEvidence,
      evaluation: Option[lila.chessjudgment.model.strategic.EngineLine]
  ): ComparisonEndpointLineEffectInventories =
    val material =
      if payload.materialClosedNetCpForMover.nonEmpty then
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
            payload.replayIsCertified &&
            payload.rootMove.exists(EvidenceRef.sameMove(_, line.rootMove)) &&
            payload.lineReplaySteps.headOption.exists(step =>
              EvidenceRef.sameMove(step.moveUci, line.rootMove) &&
                PrincipalVariationEvidence.sameBoardState(step.fenBefore, rootPosition.fen)
            ) &&
            exactReplayChain(payload.lineReplaySteps) =>
        ref -> payload
    }
    canonicalSemanticCarrier(lineFacts)

  private def exactLineEvaluation(
      records: List[EvidenceRecord],
      line: LineNodeRef,
      rootPosition: PositionNodeRef
  ): Option[lila.chessjudgment.model.strategic.EngineLine] =
    val evaluations = records.collect {
      case EvidenceRecord(
            ref,
            CandidateLineEvaluationEvidence(payloadLine, CandidateLineEvaluation.EngineSearch(engineLine)),
            _
          )
          if carrierAtComparisonRoot(ref, rootPosition) &&
            ref.line.contains(line) &&
            payloadLine == line &&
            engineLine.depth > 0 =>
        ref -> engineLine
    }
    canonicalSemanticCarrier(evaluations).map(_._2)

  /** Source ids identify carriers, not facts. Repeated carriers of one equal
    * payload collapse deterministically; genuinely different payloads leave
    * the endpoint incomplete instead of being selected by record order.
    */
  private[assembly] def canonicalSemanticCarrier[A](
      candidates: List[(EvidenceRef, A)]
  ): Option[(EvidenceRef, A)] =
    candidates.groupBy(_._2).toList match
      case (_, equivalentCarriers) :: Nil => Some(equivalentCarriers.minBy(_._1.id))
      case _ => None

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
        steps
          .lift(index + 1)
          .forall(next => PrincipalVariationEvidence.sameBoardState(step.fenAfter, next.fenBefore))
      }

  private def mateObservationConsistent(
      payload: LineFactEvidence,
      evaluation: lila.chessjudgment.model.strategic.EngineLine,
      observations: Set[ComparisonEndpointEffectObservation]
  ): Boolean =
    val ownedMateEpisodes = payload
      .rootOwnedCausalEpisodes(payload.line.rootMove)
      .filter(_.consequence.kind == LineConsequenceKind.Mate)
    val observedMateCount = observations.count(_.magnitude match
      case ComparisonEndpointEffectMagnitude.Exact(DirectCauseImportanceMeasure.MateArrival(_)) => true
      case _ => false)
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
            payload.transitionIsCertified =>
        ref -> payload
    })

  private[assembly] def planResultEndpointInventory(
      graph: TypedEvidenceGraph,
      line: LineNodeRef,
      mover: Color,
      rootPosition: PositionNodeRef,
      comparison: Option[CandidateComparisonFact] = None,
      sourceSide: Option[RelativeCauseSourceSide] = None
  ): PlanResultEndpointInventory =
    val endpointRecords = graph.records.filter(_.referencesLine(line))
    val canonicalInputs = for
      (_, lineFact) <- exactLegalLine(endpointRecords, line, rootPosition)
      (_, structural) <- exactRootStructuralDelta(
        endpointRecords,
        line,
        mover,
        rootPosition
      )
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
      if lineRefIds.nonEmpty && structuralRefIds.nonEmpty
    yield (lineFact, structural, lineRefIds, structuralRefIds)

    canonicalInputs match
      case None =>
        PlanResultEndpointInventory(Set.empty, Set.empty, Set.empty, extractionReady = false)
      case Some((lineFact, structural, lineRefIds, structuralRefIds)) =>
        val eventRecords = endpointRecords.collect {
          case record @ EvidenceRecord(ref, event: PlanCausalEventEvidence, _)
              if carrierAtComparisonRoot(ref, rootPosition) &&
                ref.line.contains(line) &&
                event.rootLine == line =>
            record -> event
        }
        val enumeratedPlanIds = eventRecords.map(_._2.planId.id).toSet
        val evaluated = eventRecords.map { case (record, event) =>
          val allGoalAssessments = event.causalResultAssessments
          val exactAssessments = (
            event.exactRobustPublicResultAssessments ++
              event.exactRefutedPublicResultAssessments
          ).distinct
          val carrierReady =
            graph.proofEligible(record) &&
              record.parents.exists(parent => structuralRefIds(parent.id)) &&
              record.parents.exists(parent => lineRefIds(parent.id))
          def identity(
              assessment: PlanCausalResultAssessment,
              selectedResponse: Option[PlanCausalResponse] = None
          ): PlanResultSemanticIdentity =
            PlanResultSemanticIdentity.from(event, assessment, selectedResponse)

          if carrierReady then
            val ordinary = exactAssessments.map(assessment =>
              assessment -> ComparisonEndpointEffectObservationPolicy.fromExactPlanResult(
                rootPosition,
                line,
                record.ref,
                event,
                assessment,
                graph
              )
            )
            val inducedCandidates =
              for
                exactComparison <- comparison.toList
                exactSourceSide <- sourceSide.toList
                rootActor <- RootCausalActor.fromPlanEvent(event).toList
                (assessment, response) <-
                  ComparisonEndpointEffectObservationPolicy
                    .exactInducedResponseMoveOrder(
                      exactComparison,
                      exactSourceSide,
                      record.ref,
                      event,
                      graph
                    )
                if exactAssessments.contains(assessment)
              yield
                val routeBindings = EvidenceObjectBinding
                  .inducedResponseMoveOrderBindings(
                    exactComparison,
                    exactSourceSide,
                    record.ref,
                    event,
                    rootActor,
                    assessment,
                    response,
                    line
                  )
                  .map(_._1)
                val routeObservations = routeBindings.flatMap(binding =>
                    ComparisonEndpointEffectObservationPolicy.fromExactPlanResult(
                      rootPosition,
                      line,
                      record.ref,
                      event,
                      assessment,
                      graph,
                      Some(binding),
                      selectedInducedResponse = Some(response)
                    )
                  )
                val observation = Option.when(
                  routeBindings.nonEmpty &&
                    routeObservations.size == routeBindings.size &&
                    routeObservations.distinct.size == 1
                )(routeObservations.head)
                (assessment, response, observation)
            val successful = (
              ordinary.flatMap(_._2) ++ inducedCandidates.flatMap(_._3)
            ).toSet
            val incomplete = (
              allGoalAssessments.filterNot(exactAssessments.contains).map(identity(_)) ++
                ordinary.collect { case (assessment, None) => identity(assessment) } ++
                inducedCandidates.collect {
                  case (assessment, response, None) => identity(assessment, Some(response))
                }
            ).toSet
            val exactInduced = inducedCandidates.flatMap(_._3).toSet
            (record.ref, successful, exactInduced, incomplete)
          else
            (
              record.ref,
              Set.empty[ComparisonEndpointEffectObservation],
              Set.empty[ComparisonEndpointEffectObservation],
              allGoalAssessments.map(identity(_)).toSet
            )
        }
        val allSuccessful = evaluated.flatMap(_._2).toSet
        val conflictingScopes = allSuccessful
          .groupBy(_.scope)
          .collect { case (scope, values) if values.map(_.magnitude).size != 1 => scope }
          .toSet
        val retainedObservations = allSuccessful.filterNot(observation => conflictingScopes(observation.scope))
        val conflictIdentities = conflictingScopes.flatMap(_.effectIdentity.planResult)
        PlanResultEndpointInventory(
          observations = retainedObservations,
          enumeratedPlanIds = enumeratedPlanIds,
          incompleteResultIdentities = evaluated.flatMap(_._4).toSet ++ conflictIdentities,
          extractionReady = true,
          exactInducedResponseObservations = evaluated
            .map { case (source, _, observations, _) =>
              source -> observations.filter(retainedObservations)
            }
            .toMap
        )

  private def strategicEndpointInventories(
      graph: TypedEvidenceGraph,
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
            graph,
            payload,
            comparison.referenceLine,
            RelativeCauseSourceSide.Reference,
            rootPosition
          ),
          strategicEndpointObservations(
            graph,
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
        val referencePresent = axis.referenceSources.nonEmpty
        val candidatePresent = axis.candidateSources.nonEmpty
        val referenceComplete =
          !referencePresent ||
            axis.referenceSources.exists(_.line.contains(comparison.referenceLine))
        val candidateComplete =
          !candidatePresent ||
            axis.candidateSources.exists(_.line.contains(comparison.candidateLine))
        (referencePresent || candidatePresent) &&
        strategicAxisComparisonOutcome(
          axis.axis,
          referencePresent,
          candidatePresent
        ) == axis.outcome &&
        referenceComplete && candidateComplete
      }

  private def strategicEndpointObservations(
      graph: TypedEvidenceGraph,
      payload: StrategicMechanismContrastEvidence,
      line: LineNodeRef,
      sourceSide: RelativeCauseSourceSide,
      rootPosition: PositionNodeRef
  ): Option[Set[ComparisonEndpointEffectObservation]] =
    val projected = payload.axisComparisons.flatMap { axis =>
      val ownsSource = sourceSide match
        case RelativeCauseSourceSide.Reference =>
          axis.referenceSources.exists(_.line.contains(line))
        case RelativeCauseSourceSide.Candidate =>
          axis.candidateSources.exists(_.line.contains(line))
        case RelativeCauseSourceSide.Shared | RelativeCauseSourceSide.Mixed =>
          false
      Option.when(ownsSource)(
        for
          actor <- graph.certifiedRootActorFor(line)
          observation <-
            ComparisonEndpointEffectObservationPolicy.fromStrategicAxis(
              rootPosition,
              actor,
              axis.axis
            )
        yield observation
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
    val contrastIds =
      proofSectionRecords(graph, contrastRecords, Some(kind), Some(binding)).map(_.ref.id).toSet
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
      graph: TypedEvidenceGraph,
      draft: RelativeCauseDraft,
      binding: RelativeCauseBinding,
      proofRecords: RelativeCauseProofRecords,
      mover: chess.Color
  ): CauseAttribution =
    val ownedRefs = proofRecords.directProof.map(_.ref).distinctBy(_.id)
    val rootMatched =
      ownedRefs.nonEmpty &&
        proofRecords.directProof.exists(record =>
          recordMatchesEventRoot(graph, record, binding.eventLine.rootMove, mover)
        )
    val attributionKind = draft.attributionKind
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
      reason = Option.when(!directEligible)(
        if ownedRefs.isEmpty then "no-owned-direct-proof"
        else if !rootMatched then "root-mismatch"
        else "context-only-attribution"
      )
    )

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
    val proofEligibleRelationIds = graph
      .proofEligibleRelationNodesByEvidenceIds(proofRecords.map(_.ref.id).toSet)
      .map(_.ref.id)
      .toSet
    val typedProofSources =
      proofRecords.filter {
        case record @ EvidenceRecord(_, payload: PlanCausalEventEvidence, _) =>
          planCausalEventDirectlyOwnsCause(graph, fact, kind, binding, record, payload)
        case _ if RelativeCauseKind.requiresExactPlanResult(kind) =>
          false
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
        case EvidenceRecord(ref, _: RelationFactEvidence, _) =>
          proofEligibleRelationIds(ref.id)
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
        case record @ EvidenceRecord(_, payload: StrategicMechanismEvidence, _) =>
          if Set(
              RelativeCauseKind.PlanContradiction,
              RelativeCauseKind.PlanImprovement,
              RelativeCauseKind.SacrificeCompensation
            )(kind)
          then false
          else
            graph.proofEligible(record) &&
            payload.canSupportStrategicCause &&
            strategicMechanismProofSignals(
              kind,
              payload,
              binding.sourceSide
            ).nonEmpty
        case EvidenceRecord(_, payload: StrategicMechanismContrastEvidence, _) =>
          !Set(RelativeCauseKind.PlanContradiction, RelativeCauseKind.PlanImprovement)(kind) &&
          strategicContrastCanDirectlyProveCause(
            kind,
            payload,
            binding.sourceSide,
            graph,
            Some(binding.eventLine)
          )
        case EvidenceRecord(_, payload: StructuralDeltaEvidence, _) =>
          kind != RelativeCauseKind.SacrificeCompensation &&
          RelativeCauseKind.structuralConsequences(kind, payload).nonEmpty
        case _ =>
          false
      }
    RelativeCauseProofSection(
      role = role,
      strength = strength,
      sourceRefs = (if includeContextLayers then proofRecords else typedProofSources)
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
    val tacticalRelationCandidates =
      records.collect { case EvidenceRecord(_, payload: TacticalMechanismEvidence, _) =>
        payload.signals
          .filter(_.kind == TacticalMechanismSignalKind.Relation)
          .flatMap(_.source)
          .flatMap(graph.record)
          .flatMap(source => source :: graph.parentClosure(source))
      }.flatten
    val explicitTacticalRelationSources = proofEligibleRelationRecords(graph, tacticalRelationCandidates)
    val strategicStructuralParents =
      records.collect {
        case record @ EvidenceRecord(_, payload: StrategicMechanismEvidence, _)
            if graph.proofEligible(record) && payload.canSupportStrategicCause =>
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
            .flatMap(signal => graph.byId.get(signal.source.id))
            .collect { case record @ EvidenceRecord(_, _: StructuralDeltaEvidence, _) => record }
      }.flatten
    val strategicRelationCandidates =
      records.collect {
        case record @ EvidenceRecord(_, payload: StrategicMechanismEvidence, _)
            if graph.proofEligible(record) && payload.canSupportStrategicCause =>
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
      }.flatten
    val explicitStrategicRelationSources = proofEligibleRelationRecords(graph, strategicRelationCandidates)
    (
      records ++
        ownedParentClosure ++
        explicitTacticalRelationSources ++
        strategicStructuralParents ++
        explicitStrategicRelationSources
    ).distinctBy(_.ref.id)

  private def proofEligibleRelationRecords(
      graph: TypedEvidenceGraph,
      candidates: List[EvidenceRecord]
  ): List[EvidenceRecord] =
    graph
      .proofEligibleRelationNodesByEvidenceIds(candidates.map(_.ref.id).toSet)
      .map(_.record)

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
      case EvidenceLayer.Line | EvidenceLayer.Relation | EvidenceLayer.StrategicMechanism |
          EvidenceLayer.CandidateComparison =>
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
    val rootRecaptureCause = kind == RelativeCauseKind.RecaptureRecoveryWindow
    val verifiedRootRecapture =
      !rootRecaptureCause ||
        (record :: graph.parentClosure(record)).exists {
          case EvidenceRecord(_, payload: LineFactEvidence, _) => payload.rootIsRecapture(rootMove)
          case _ => false
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
        graph.relationProofEligible(record) &&
        payload.hasLineProof &&
        record.ref.line.contains(binding.eventLine) &&
        payload.mentionsLineMove(rootMove) &&
        payload.rootGeometryConnected(rootMove) &&
        relationCanDirectlyProveCause(kind, payload)
      case payload: StructuralDeltaEvidence =>
        kind != RelativeCauseKind.SacrificeCompensation &&
        record.referencesLine(binding.eventLine) &&
        payload.line.contains(binding.eventLine) &&
        EvidenceRef.sameMove(payload.moveUci, rootMove) &&
        RelativeCauseKind.structuralConsequences(kind, payload).nonEmpty
      case _: StrategicMechanismEvidence =>
        !Set(
          RelativeCauseKind.PlanContradiction,
          RelativeCauseKind.PlanImprovement,
          RelativeCauseKind.SacrificeCompensation
        )(kind) &&
        record.referencesLine(binding.eventLine) &&
        strategicCause(kind) &&
        strategicMechanismDirectlyOwnsCause(
          graph,
          record,
          kind,
          binding.eventLine.rootMove,
          binding.sourceSide
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
      case _ =>
        false)

  private def planCausalEventDirectlyOwnsCause(
      graph: TypedEvidenceGraph,
      fact: CandidateComparisonFact,
      kind: RelativeCauseKind,
      binding: RelativeCauseBinding,
      record: EvidenceRecord,
      event: PlanCausalEventEvidence
  ): Boolean =
    graph.proofEligible(record) &&
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
      payload: LineFactEvidence
  ): Boolean =
    val rootMove = binding.eventLine.rootMove
    if kind == RelativeCauseKind.SacrificeCompensation then
      record.ref.line.contains(binding.eventLine) &&
      lineHasRootSacrifice(payload, binding.eventLine) &&
      graph.records.exists {
        case EvidenceRecord(_, contrast: StrategicMechanismContrastEvidence, _) =>
          contrast.comparisonKind == fact.kind &&
          contrast.referenceLine == fact.referenceLine &&
          contrast.candidateLine == fact.candidateLine &&
          strategicContrastCanDirectlyProveCause(
            kind,
            contrast,
            binding.sourceSide,
            graph,
            Some(binding.eventLine)
          )
        case _ =>
          false
      }
    else
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
      case RelativeCauseKind.RecaptureRecoveryWindow =>
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
        payload
          .eventsForRootMove(rootMove)
          .exists(event => event.kind == LineEventKind.Check || event.kind == LineEventKind.Mate)
      case RelativeCauseKind.DrawResource =>
        ownedConsequences.exists(_.kind == LineConsequenceKind.DrawResource)
      case RelativeCauseKind.MaterialSwing =>
        ownedConsequences.exists(consequence =>
          consequence.kind == LineConsequenceKind.MaterialGain ||
            consequence.kind == LineConsequenceKind.MaterialLoss
        )
      case RelativeCauseKind.SacrificeCompensation =>
        false
      case RelativeCauseKind.ConversionMiss | RelativeCauseKind.ConversionSecured =>
        ownedConsequences.exists(consequence =>
          RelativeCauseKind.acceptsDirectLineConsequence(kind, payload, rootMove, consequence)
        )
      case RelativeCauseKind.MissedTacticalResource | RelativeCauseKind.TacticalRefutationOfPlayed |
          RelativeCauseKind.CandidateTacticalLiability =>
        ownedConsequences.exists(consequence =>
          RelativeCauseKind.acceptsDirectLineConsequence(kind, payload, rootMove, consequence) &&
            LineConsequenceKind.tacticalDriver(consequence.kind)
        ) ||
        payload
          .eventsForRootMove(rootMove)
          .exists(event =>
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
      eventLine: LineNodeRef
  ): Boolean =
    graph.records.exists {
      case EvidenceRecord(ref, payload: LineFactEvidence, _) if ref.line.contains(eventLine) =>
        lineHasRootSacrifice(payload, eventLine)
      case _ =>
        false
    }

  private def lineHasRootSacrifice(
      payload: LineFactEvidence,
      eventLine: LineNodeRef
  ): Boolean =
    payload.line == eventLine &&
      payload
        .rootOwnedCausalEpisodes(eventLine.rootMove)
        .exists(_.consequence.kind == LineConsequenceKind.Sacrifice)

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
            case relationRecord @ EvidenceRecord(ref, relation: RelationFactEvidence, _) =>
              signals.exists(_.kind == TacticalMechanismSignalKind.Relation) &&
              ref.line.contains(binding.eventLine) &&
              graph.relationProofEligible(relationRecord) &&
              relation.mentionsLineMove(rootMove) &&
              relation.rootGeometryConnected(rootMove) &&
              TacticalMechanismKind.fromRelation(relation).contains(payload.kind) &&
              relationCanDirectlyProveCause(kind, relation)
            case EvidenceRecord(
                  ref,
                  CandidateLineEvaluationEvidence(_, CandidateLineEvaluation.EngineSearch(line)),
                  _
                ) =>
              signals.exists(_.kind == TacticalMechanismSignalKind.MateBranch) &&
              kind == RelativeCauseKind.KingForcing &&
              line.mate.nonEmpty &&
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
        false
      case RelativeCauseKind.TacticalRefutationOfPlayed | RelativeCauseKind.CandidateTacticalLiability
          if payload.kind == TacticalMechanismKind.RecaptureChoice =>
        false
      case _ =>
        true

  private def relationCanDirectlyProveCause(kind: RelativeCauseKind, payload: RelationFactEvidence): Boolean =
    RootOwnedEffectPolicy.relationDirectlyProvesCause(payload, kind)

  private def strategicMechanismDirectlyOwnsCause(
      graph: TypedEvidenceGraph,
      record: EvidenceRecord,
      kind: RelativeCauseKind,
      rootMove: String,
      sourceSide: RelativeCauseSourceSide
  ): Boolean =
    record.payload match
      case payload: StrategicMechanismEvidence if graph.proofEligible(record) && payload.canSupportStrategicCause =>
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
    val normalizedRoot = EvidenceRef.normalizeMove(rootMove)
    signal.axis.exists(axis => RelativeCauseKind.strategicAxisCanProveCause(kind, axis)) &&
    (signal.kind match
      case StrategicMechanismSignalKind.PlanPressure =>
        graph.byId.get(signal.source.id).exists {
          case record @ EvidenceRecord(sourceRef, event: PlanCausalEventEvidence, _)
              if graph.proofEligible(record) =>
            eventLine.forall(sourceRef.line.contains) &&
            EvidenceRef.sameMove(event.rootMove, normalizedRoot) &&
            RelativeCauseKind.planCausalEventCanProveCause(kind, event)
          case _ =>
            false
        }
      case _ =>
        false)

  private[chessjudgment] def strategicMechanismProofSignals(
      kind: RelativeCauseKind,
      payload: StrategicMechanismEvidence,
      sourceSide: RelativeCauseSourceSide
  ): List[StrategicMechanismSignal] =
    payload.signals
      .filter(signal =>
        signal.axis.exists(axis => RelativeCauseKind.strategicAxisCanProveCause(kind, axis))
      )
      .distinct

  private def strategicContrastCanDirectlyProveCause(
      kind: RelativeCauseKind,
      payload: StrategicMechanismContrastEvidence,
      sourceSide: RelativeCauseSourceSide,
      graph: TypedEvidenceGraph,
      boundEventLine: Option[LineNodeRef]
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
        case RelativeCauseKind.SacrificeCompensation =>
          ownedEventLine.exists(line =>
            eventLineHasRootSacrifice(graph, line) &&
              RelativeCauseSignalProfile
                .sacrificeCompensationReturnComparisons(payload, line, graph.records)
                .contains(comparison) &&
              compensationReturnDirectlyOwned(comparison, sourceSide, line, graph)
          )
        case RelativeCauseKind.PlanImprovement | RelativeCauseKind.PlanContradiction =>
          ownedPlanEventCanProveCause(comparison)
        case _ =>
          true
    }

  private def compensationReturnDirectlyOwned(
      comparison: StrategicAxisComparison,
      sourceSide: RelativeCauseSourceSide,
      eventLine: LineNodeRef,
      graph: TypedEvidenceGraph
  ): Boolean =
    graph
      .strategicComparisonPlanEventRefs(comparison, sourceSide, eventLine)
      .flatMap(graph.record)
      .exists {
        case record @ EvidenceRecord(_, event: PlanCausalEventEvidence, _) =>
          graph.proofEligible(record) &&
            RelativeCauseKind.planCausalEventCanProveCause(
              RelativeCauseKind.SacrificeCompensation,
              event
            )
        case _ => false
      }

  private def defensiveCause(kind: RelativeCauseKind): Boolean =
    kind == RelativeCauseKind.DefensiveResource ||
      kind == RelativeCauseKind.DrawResource

  private def strategicCause(kind: RelativeCauseKind): Boolean =
    RelativeCauseKind.strategicContrastBacked(kind)

  private def recordMatchesEventRoot(
      graph: TypedEvidenceGraph,
      record: EvidenceRecord,
      rootMove: String,
      mover: chess.Color
  ): Boolean =
    record.payload match
      case payload: LineFactEvidence =>
        payload.eventsForRootMove(rootMove).nonEmpty ||
        payload.consequencesForRootMove(rootMove).nonEmpty
      case payload: TacticalMechanismEvidence =>
        payload.moveUci.exists(move => EvidenceRef.sameMove(move, rootMove))
      case payload: StructuralDeltaEvidence =>
        EvidenceRef.sameMove(payload.moveUci, rootMove)
      case payload: RelationFactEvidence =>
        graph.relationProofEligible(record) &&
          payload.mentionsLineMove(rootMove) &&
          payload.rootGeometryConnected(rootMove)
      case _: StrategicMechanismEvidence | _: StrategicMechanismContrastEvidence =>
        record.ref.line.exists(line => EvidenceRef.sameMove(line.rootMove, rootMove)) ||
        record.payloadLineRefs.exists(line => EvidenceRef.sameMove(line.rootMove, rootMove))
      case payload: PlanCausalEventEvidence =>
        record.ref.line.exists(line =>
          EvidenceRef.sameMove(line.rootMove, rootMove) &&
            RootOwnedEffectPolicy.planEventOwnsRoot(record.ref, payload, line, mover)
        )
      case CandidateComparisonEvidence(_) =>
        false
      case _ =>
        false

  final private case class CanonicalRelativeCauseEntry(
      record: EvidenceRecord,
      cause: RelativeCauseFact,
      semanticKey: RelativeCauseSemanticKey,
      exactOccurrences: Set[RootOwnedEffectChannelOccurrenceFingerprint],
      inputIndex: Int
  )

  private[chessjudgment] def canonicalizeRelativeCauseRecords(
      records: List[EvidenceRecord],
      graph: TypedEvidenceGraph,
      snapshotsByComparisonId: Map[String, ComparisonEndpointEvidenceSnapshot]
  ): List[EvidenceRecord] =
    val causeEntries = records.zipWithIndex.flatMap { case (record, index) =>
      relativeCauseFromRecord(record).map { cause =>
        val semanticKey = RelativeCauseSemanticKey.from(cause, record.ref.id, graph)
        val channels = RelativeCauseConstructionAdmission.admittedDirectChannels(cause, graph)
        CanonicalRelativeCauseEntry(
          record,
          cause,
          semanticKey,
          channels.map(_.exactOccurrenceFingerprint).toSet,
          index
        )
      }
    }
    val causeIndexes = causeEntries.map(_.inputIndex).toSet
    val passThrough = records.zipWithIndex.collect {
      case (record, index) if !causeIndexes(index) => index -> record
    }
    val (ready, unready) = causeEntries.partition(_.semanticKey.sentenceReady)
    val mergedReady = ready
      .groupBy(entry => entry.semanticKey.frame -> entry.exactOccurrences)
      .values
      .toList
      .flatMap {
        case exact :: Nil => List(exact.inputIndex -> exact.record)
        case exactDuplicates =>
          mergeCauseComponent(
            exactDuplicates,
            graph,
            snapshotsByComparisonId
          ).map(record => exactDuplicates.map(_.inputIndex).min -> record).toList
      }
    val preservedUnready = unready.map(entry => entry.inputIndex -> entry.record)
    (passThrough ++ preservedUnready ++ mergedReady).map(_._2).sortBy(_.ref.id)

  private def mergeCauseComponent(
      component: List[CanonicalRelativeCauseEntry],
      graph: TypedEvidenceGraph,
      snapshotsByComparisonId: Map[String, ComparisonEndpointEvidenceSnapshot]
  ): Option[EvidenceRecord] =
    val ordered = component.sortBy(_.record.ref.id)
    val carrier = ordered.head
    require(
      ordered.map(_.semanticKey.frame).distinct.size == 1,
      "relative Cause records may merge only after semantic-frame agreement"
    )
    require(
      ordered.map(_.exactOccurrences).distinct.size == 1,
      "relative Cause records may merge only when every exact proof-route occurrence agrees"
    )
    val causes = ordered.map(_.cause)
    val mergedProofs = causes.flatMap(_.proof)
    val admittedBeforeMerge = carrier.exactOccurrences
    val mergedBeforeAdmissionValidation = carrier.cause.copy(
      supportEvidence = causes.flatMap(_.supportEvidence).distinctBy(_.id),
      proof = Option.when(mergedProofs.nonEmpty)(RelativeCauseProof.merge(mergedProofs)),
      directEffectAdmission = DirectEffectAdmission.Restricted(admittedBeforeMerge)
    )
    val mergedPubliclyAdmissibleChannels =
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
    val mergedPubliclyAdmissibleOccurrences =
      mergedPubliclyAdmissibleChannels.map(_.exactOccurrenceFingerprint).toSet
    val mergedCause = mergedBeforeAdmissionValidation.copy(
      directEffectAdmission = DirectEffectAdmission.Restricted(
        mergedPubliclyAdmissibleOccurrences
      )
    )
    proofConfidenceForAdmittedChannels(mergedPubliclyAdmissibleChannels).map { confidence =>
      carrier.record.copy(
        ref = carrier.record.ref.copy(confidence = confidence),
        payload = RelativeCauseFactEvidence(mergedCause),
        parents = ordered.flatMap(_.record.parents).distinctBy(_.id).sortBy(_.id)
      )
    }

  private def relativeCauseFromRecord(record: EvidenceRecord): Option[RelativeCauseFact] =
    record.payload match
      case RelativeCauseFactEvidence(cause) => Some(cause)
      case _ => None

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
        graph = graph,
        referenceRecords = neighborhood.referenceRecords,
        candidateRecords = neighborhood.candidateRecords,
        sharedRecords = neighborhood.sharedRecords,
        referenceEndpointMoveOrderRecords =
          endpointMoveOrderRecords.getOrElse(RelativeCauseSourceSide.Reference, Nil),
        candidateEndpointMoveOrderRecords =
          endpointMoveOrderRecords.getOrElse(RelativeCauseSourceSide.Candidate, Nil)
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
      (recordsForLineEndpoint(context, fact.referenceLine) ++ input.toList.flatMap(
        _ => recordsForThreatBranchRoot(context, fact.referenceLine.rootMove)
      ))
        .distinctBy(_.ref.id)
    val candidateEndpoint =
      (recordsForLineEndpoint(context, fact.candidateLine) ++ input.toList.flatMap(
        _ => recordsForThreatBranchRoot(context, fact.candidateLine.rootMove)
      ))
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
      baseRecords.flatMap(record =>
        record.parents.flatMap(parent => context.evidenceGraph.byId.get(parent.id))
      )
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
      context: JudgmentAssemblyContext,
      rootMove: String
  ): List[EvidenceRecord] =
    val ownedThreatLineIds =
      context.threatLineOwners.collect {
        case (line, owner) if EvidenceRef.sameMove(owner.probedMoveUci, rootMove) => line.id
      }.toSet
    if ownedThreatLineIds.isEmpty then Nil
    else
      context.evidenceGraph.records.filter(record =>
        endpointLayer(record.ref.layer) &&
          record.ref.line.exists(line =>
            line.role == LineNodeRole.Threat && ownedThreatLineIds.contains(line.id)
          )
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
      case EvidenceLayer.Line | EvidenceLayer.Eval | EvidenceLayer.Relation |
          EvidenceLayer.TacticalMechanism | EvidenceLayer.StrategicMechanism =>
        true
      case _ =>
        false

  private def rootContextLayer(layer: EvidenceLayer): Boolean =
    layer match
      case EvidenceLayer.PositionFeature | EvidenceLayer.Relation | EvidenceLayer.StrategicMechanism =>
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
      case EvidenceLayer.PositionFeature | EvidenceLayer.Relation | EvidenceLayer.StrategicMechanism =>
        true
      case _ =>
        false
