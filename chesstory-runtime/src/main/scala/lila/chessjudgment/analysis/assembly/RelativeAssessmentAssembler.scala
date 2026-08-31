package lila.chessjudgment.analysis.assembly

import chess.{ Color, Square }
import lila.chessjudgment.analysis.transition.TransitionFactNormalizer
import lila.chessjudgment.model.line.{ CandidateLineEvaluation, PrincipalVariationEvidence }
import lila.chessjudgment.model.judgment.*

object RelativeAssessmentAssembler:

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

  final private case class CanonicalEndpointEvidence(
      records: List[EvidenceRecord],
      linePayload: Option[LineFactEvidence],
      evaluation: Option[lila.chessjudgment.model.line.EngineLine],
      passedPawnResultClosureInput: ComparisonEndpointPassedPawnResultClosureInput
  )

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

  /** Typed PassedPawnResult comparison is admissible only when the source owns the
    * exact observation and the counterpart inventory is complete for the
    * relevant comparison scope. Missing enumeration and unresolved carriers
    * fail closed; complete absence is a real differential. The source-keyed
    * neutral mapping alone selects projected comparison; an absent mapping
    * retains the generic exact-scope path and a mismatched mapping is invalid.
    */
  private[analysis] object PassedPawnResultDifferentialPolicy:
    def admitted(
        sourceInventory: PassedPawnResultEndpointInventory,
        counterpartInventory: PassedPawnResultEndpointInventory,
        sourceObservation: ComparisonEndpointEffectObservation
    ): Boolean =
      val policy = ComparisonEndpointEffectObservationPolicy
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

  final private case class RelativeAssemblyInputs(
      input: AdmittedMoveReviewInput,
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
          primaryComparisonRecord(comparisonRecords, inputs.reference, inputs.candidate).map { primary =>
            val comparisonContext = context.withEvidence(comparisonRecords)
            val passedPawnResultEventRecords = PassedPawnResultEventAssembler.fromAssembly(
              inputs.input,
              comparisonContext,
              inputs.allocator,
              primary
            )
            val passedPawnResultContext = comparisonContext.withEvidence(passedPawnResultEventRecords)
            val passedPawnResultProofContext = passedPawnResultContext.withEvidence(
              PassedPawnResultProofAssembler.fromAssembly(
                passedPawnResultContext,
                inputs.allocator,
                primary
              )
            )
            val forcedReplyProofRecords = primary.payload match
              case CandidateComparisonEvidence(_) =>
                ForcedReplyResourceDifferentialAssembler.fromAssembly(
                  passedPawnResultProofContext,
                  inputs.allocator,
                  primary
                )
              case _ => Nil
            passedPawnResultProofContext.withEvidence(forcedReplyProofRecords)
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
            val rawCauseRecords = comparisonRecords.flatMap(record =>
              relativeCauseRecords(
                context,
                inputs.root,
                inputs.allocator,
                record
              )
            )
            val causeRecords = validateRelativeCauseOwnership(
              rawCauseRecords,
              context.evidenceGraph
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

  /** Cause-admission snapshot derived from the F graph before any provisional
    * Cause exists. Public output consumes the admitted Cause proof rather than
    * rebuilding this internal view.
    */
  private def comparisonEndpointEvidenceSnapshot(
      context: JudgmentAssemblyContext,
      root: PositionNodeRef,
      record: EvidenceRecord,
      comparison: CandidateComparisonFact,
      neighborhood: ComparisonEvidenceNeighborhood,
      demand: ComparisonEndpointWitnessDemand
  ): Option[ComparisonEndpointEvidenceSnapshot] =
    Option.when(demand.nonEmpty) {
      val demandedSourceRecords =
        demand.demandedSourceIds.toList.sorted
          .flatMap(context.evidenceGraph.byId.get)
      val involvedRecords =
        (neighborhood.involvedRecords ++ demandedSourceRecords)
          .distinctBy(_.ref.id)
          .filter(context.evidenceGraph.proofEligible)

      def sideSnapshot(
          sourceSide: RelativeCauseSourceSide,
          line: LineNodeRef,
          records: List[EvidenceRecord]
      ): ComparisonEndpointEvidenceSideSnapshot =
        if demand.entriesForEndpoint(sourceSide).isEmpty then
          ComparisonEndpointEvidenceSideSnapshot(sourceSide, line, Nil, Map.empty, None)
        else
          val sideDemandedRecords = demand.demandedSourceIds(sourceSide).toList.sorted
            .flatMap(context.evidenceGraph.byId.get)
          val canonical = canonicalEndpointEvidence(
            (records ++ sideDemandedRecords).distinctBy(_.ref.id),
            line,
            comparison.comparison.mover,
            root,
            sourceSide,
            demand,
            context.evidenceGraph
          )
          val projection = EvidenceObjectBinding.comparisonEndpointEvidenceProjection(
            sourceSide,
            line,
            root,
            canonical.records,
            involvedRecords,
            demand,
            Option.when(
              demand.closes(
                ComparisonEndpointWitnessFamily.Primitive(RootOwnedEffectPrimitiveKind.PassedPawnResult)
              )
            )(canonical.passedPawnResultClosureInput),
            context.evidenceGraph
          )
          val closedLineInventories = projection.lineInventories.map { case (family, inventory) =>
            family -> canonical.linePayload
              .map(payload =>
                enforceLineFamilyCompleteness(
                  inventory,
                  payload,
                  canonical.evaluation,
                  family
                )
              )
              .getOrElse(ComparisonEndpointEffectInventory.Incomplete)
          }
          ComparisonEndpointEvidenceSideSnapshot(
            sourceSide,
            line,
            projection.witnesses,
            closedLineInventories,
            projection.passedPawnResultInventory
          )

      ComparisonEndpointEvidenceSnapshot(
        comparisonEvidence = record.ref,
        comparison = comparison,
        reference = sideSnapshot(
          RelativeCauseSourceSide.Reference,
          comparison.referenceLine,
          neighborhood.referenceRecords
        ),
        candidate = sideSnapshot(
          RelativeCauseSourceSide.Candidate,
          comparison.candidateLine,
          neighborhood.candidateRecords
        )
      )
    }

  private def canonicalEndpointEvidence(
      records: List[EvidenceRecord],
      line: LineNodeRef,
      mover: Color,
      rootPosition: PositionNodeRef,
      sourceSide: RelativeCauseSourceSide,
      demand: ComparisonEndpointWitnessDemand,
      graph: TypedEvidenceGraph
  ): CanonicalEndpointEvidence =
    val lineFamilies = demand.lineFamiliesForEndpoint(sourceSide)
    val closesPassedPawnResults = demand.closes(
      ComparisonEndpointWitnessFamily.Primitive(RootOwnedEffectPrimitiveKind.PassedPawnResult)
    )
    val closedLineFamilies = lineFamilies.filter(family =>
      demand.closes(ComparisonEndpointWitnessFamily.Line(family))
    )
    val needsLinePayload = lineFamilies.nonEmpty || closesPassedPawnResults
    val needsStructural = closesPassedPawnResults
    val needsEvaluation = closedLineFamilies.exists(family =>
      family == ComparisonEndpointLineProofFamily.LineEpisodeMate ||
        family == ComparisonEndpointLineProofFamily.RootLineEventMate
    )
    val endpointRecords = (
      records ++ Option
        .when(demand.hasClosureForEndpoint(sourceSide))(
          graph.recordsFor(line).filter(record =>
            carrierAtComparisonRoot(record.ref, rootPosition)
          )
        )
        .toList
        .flatten
    ).distinctBy(_.ref.id).filter(graph.proofEligible)
    val canonicalLine = Option.when(needsLinePayload)(exactLegalLine(endpointRecords, line, rootPosition)).flatten
    val canonicalStructural = Option
      .when(needsStructural)(exactRootStructuralDelta(endpointRecords, line, mover, rootPosition))
      .flatten
    val lineRefIds = canonicalLine.toList.flatMap { case (_, payload) =>
      endpointRecords.collect {
        case EvidenceRecord(ref, candidate: LineFactEvidence, _)
            if candidate == payload &&
              carrierAtComparisonRoot(ref, rootPosition) &&
              ref.line.contains(line) =>
          ref.id
      }
    }.toSet
    val structuralRefIds = canonicalStructural.toList.flatMap { case (_, payload) =>
      endpointRecords.collect {
        case EvidenceRecord(ref, candidate: StructuralDeltaEvidence, _)
            if candidate == payload &&
              carrierAtComparisonRoot(ref, rootPosition) &&
              ref.line.contains(line) =>
          ref.id
      }
    }.toSet
    val canonicalLineRef = canonicalLine.map(_._1.id)
    val canonicalStructuralRef = canonicalStructural.map(_._1.id)
    val canonicalRecords = endpointRecords.filter {
      case EvidenceRecord(ref, _: LineFactEvidence, _) => canonicalLineRef.contains(ref.id)
      case EvidenceRecord(ref, _: StructuralDeltaEvidence, _) => canonicalStructuralRef.contains(ref.id)
      case _ => true
    }
    CanonicalEndpointEvidence(
      records = canonicalRecords,
      linePayload = canonicalLine.map(_._2),
      evaluation = Option
        .when(needsEvaluation)(exactLineEvaluation(endpointRecords, line, rootPosition))
        .flatten,
      passedPawnResultClosureInput = ComparisonEndpointPassedPawnResultClosureInput(
        lineRefIds,
        structuralRefIds,
        extractionReady = canonicalLine.nonEmpty && canonicalStructural.nonEmpty &&
          lineRefIds.nonEmpty && structuralRefIds.nonEmpty
      )
    )

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
    context.evidenceGraph.recordsFor(root).collect {
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
      .filterNot(_.role == LineNodeRole.BranchReply)
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
        val causes =
          inferCauseCandidates(
            neighborhood,
            fact,
            comparisonRecord,
            context.evidenceGraph
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
        val provisionalChannels = provisional.map(item =>
          item -> EvidenceObjectBinding.rawDirectSentenceChannelsForProjection(item.cause, context.evidenceGraph)
        )
        val demand = ComparisonEndpointWitnessDemand.fromEntries(
          provisionalChannels.flatMap { case (item, channels) =>
            channels.flatMap(channel =>
              endpointWitnessDemandMode(item.cause, channel, fact, context.evidenceGraph).flatMap(
                mode =>
                  ComparisonEndpointWitnessDemandEntry.fromChannel(
                    item.cause.sourceSide,
                    mode,
                    channel
                  )
              )
            )
          }
        )
        val endpointSnapshot = comparisonEndpointEvidenceSnapshot(
          context,
          root,
          comparisonRecord,
          fact,
          neighborhood,
          demand
        )
        endpointSnapshot.toList.flatMap(snapshot =>
          restrictDirectEffectsForComparison(
            provisionalChannels,
            fact,
            comparisonRecord.ref.position,
            context.evidenceGraph,
            snapshot
          )
        )
      case _ => Nil

  private def endpointWitnessDemandMode(
      cause: RelativeCauseFact,
      channel: DirectCauseChannel,
      comparison: CandidateComparisonFact,
      graph: TypedEvidenceGraph
  ): Option[ComparisonEndpointWitnessDemandMode] =
    val ownsPassedPawnResultAuthority =
      !cause.passedPawnResultCauseKind || graph.relativeCauseChannelHasOwnedPassedPawnResultProof(cause, channel)
    Option.when(ownsPassedPawnResultAuthority)(()).flatMap { _ =>
      if ComparisonEndpointEffectObservationPolicy.retainedPlayedValueReady(
          cause,
          channel,
          comparison,
          graph
        )
      then Some(ComparisonEndpointWitnessDemandMode.MembershipOnly)
      else
        channel.rootOwnedEffectDescriptor.flatMap(_.identity.primitiveKind match
          case RootOwnedEffectPrimitiveKind.CausalResourceDifferential =>
            Some(ComparisonEndpointWitnessDemandMode.MembershipOnly)
          case RootOwnedEffectPrimitiveKind.LineEpisode |
              RootOwnedEffectPrimitiveKind.RootLineEvent |
              RootOwnedEffectPrimitiveKind.PassedPawnResult =>
            Some(ComparisonEndpointWitnessDemandMode.DifferentialClosure)
          case RootOwnedEffectPrimitiveKind.RootRelation |
              RootOwnedEffectPrimitiveKind.Unspecified =>
            None
        )
    }

  private def restrictDirectEffectsForComparison(
      provisionalChannels: List[(RelativeCauseConstructionCandidate, List[DirectCauseChannel])],
      comparison: CandidateComparisonFact,
      rootPosition: PositionNodeRef,
      graph: TypedEvidenceGraph,
      endpointSnapshot: ComparisonEndpointEvidenceSnapshot
  ): List[EvidenceRecord] =
    provisionalChannels.flatMap { case (item, channels) =>
      val admittedEndpointObservations = canonicalEndpointObservationEntries(
        channels.flatMap { channel =>
          ComparisonEndpointEffectObservationPolicy
            .uniqueNeutralWitnessFor(
              endpointSnapshot,
              item.cause.sourceSide,
              channel,
              graph
            )
            .filter { neutralWitness =>
              val ownsPassedPawnResultAuthority =
                !item.cause.passedPawnResultCauseKind ||
                  graph.relativeCauseChannelHasOwnedPassedPawnResultProof(item.cause, channel)
              ownsPassedPawnResultAuthority && RootOwnedEffectPolicy.admits(item.cause, graph, channel) &&
              (
                ComparisonEndpointEffectObservationPolicy.retainedPlayedValueReady(
                  item.cause,
                  channel,
                  comparison,
                  graph
                ) ||
                  differentialEndpointEffectAdmitted(
                    item.cause,
                    channel,
                    neutralWitness,
                    endpointSnapshot
                  )
              )
            }
            .map(neutralWitness => channel.exactOccurrenceFingerprint -> neutralWitness.observation)
            .toList
        }
      )
      val restrictedCause = item.cause.copy(
        directEffectAdmission = DirectEffectAdmission.Restricted(admittedEndpointObservations)
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

  /** Exact duplicate demand entries share one transported endpoint value.
    * Conflicting values for the same occurrence fail closed instead of being
    * selected by traversal order.
    */
  private def canonicalEndpointObservationEntries(
      entries: List[(
        RootOwnedEffectChannelOccurrenceFingerprint,
        Option[ComparisonEndpointEffectObservation]
      )]
  ): Map[
    RootOwnedEffectChannelOccurrenceFingerprint,
    Option[ComparisonEndpointEffectObservation]
  ] =
    entries.groupMap(_._1)(_._2).flatMap { case (occurrence, observations) =>
      observations.distinct match
        case exact :: Nil => Some(occurrence -> exact)
        case _            => None
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
      endpointSnapshot: ComparisonEndpointEvidenceSnapshot
  ): Boolean =
    val policy = ComparisonEndpointEffectObservationPolicy
    val endpointSides = cause.sourceSide match
      case RelativeCauseSourceSide.Reference => Some(endpointSnapshot.reference -> endpointSnapshot.candidate)
      case RelativeCauseSourceSide.Candidate => Some(endpointSnapshot.candidate -> endpointSnapshot.reference)
      case RelativeCauseSourceSide.Shared | RelativeCauseSourceSide.Mixed => None
    channel.rootOwnedEffectDescriptor match
      case Some(descriptor)
          if descriptor.identity.primitiveKind == RootOwnedEffectPrimitiveKind.CausalResourceDifferential =>
        channel.rootOwnedProof.exists {
          case RootOwnedEffectProof.ForcedReplyResourceDifferential(source, result) =>
            val comparison = endpointSnapshot.comparison
            cause.kind == RelativeCauseKind.WrongMoveOrder &&
              cause.sourceSide == RelativeCauseSourceSide.Reference &&
              result.occurrence.referenceLine == comparison.referenceLine &&
              result.occurrence.playedLine == comparison.candidateLine &&
              neutralWitness.primitiveProofSource == source &&
              neutralWitness.observation.exists(observation =>
                observation.scope.directChange == DirectCausalChange.Occurred &&
                  observation.scope.effectIdentity.causalProofId.contains(result.semanticId)
              )
          case _ => false
        }
      case Some(descriptor) if descriptor.identity.primitiveKind == RootOwnedEffectPrimitiveKind.PassedPawnResult =>
        (for
          (sourceSide, counterpartSide) <- endpointSides
          sourceInventory <- sourceSide.passedPawnResultInventory
          counterpartInventory <- counterpartSide.passedPawnResultInventory
          sourceObservation <- neutralWitness.observation
        yield PassedPawnResultDifferentialPolicy.admitted(
          sourceInventory,
          counterpartInventory,
          sourceObservation
        )).getOrElse(false)
      case _ =>
        (for
          family <- ComparisonEndpointLineProofFamily.fromChannel(channel)
          (sourceSide, counterpartSide) <- endpointSides
          sourceInventory <- sourceSide.lineInventories.get(family)
          counterpartInventory <- counterpartSide.lineInventories.get(family)
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

  /** Each endpoint family owns its own completeness predicate. A missing
    * closed material outcome cannot erase mate/endgame facts, and a missing or
    * inconsistent engine mate score cannot erase material/endgame facts.
    */
  private[assembly] def enforceLineFamilyCompleteness(
      inventory: ComparisonEndpointEffectInventory,
      payload: LineFactEvidence,
      evaluation: Option[lila.chessjudgment.model.line.EngineLine],
      family: ComparisonEndpointLineProofFamily
  ): ComparisonEndpointEffectInventory =
    family match
      case ComparisonEndpointLineProofFamily.LineEpisodeMaterial =>
        if payload.materialClosedNetCpForMover.nonEmpty then inventory
        else ComparisonEndpointEffectInventory.Incomplete
      case ComparisonEndpointLineProofFamily.LineEpisodeMate |
          ComparisonEndpointLineProofFamily.RootLineEventMate =>
        evaluation.fold[ComparisonEndpointEffectInventory](
          ComparisonEndpointEffectInventory.Incomplete
        ) { exactEvaluation =>
          inventory match
            case complete @ ComparisonEndpointEffectInventory.Complete(observations)
                if mateObservationConsistent(payload, exactEvaluation, observations) =>
              complete
            case _ => ComparisonEndpointEffectInventory.Incomplete
        }
      case ComparisonEndpointLineProofFamily.LineEpisodeQualitative |
          ComparisonEndpointLineProofFamily.RootLineEventQualitative => inventory

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
    uniqueCarrier(lineFacts)

  private def exactLineEvaluation(
      records: List[EvidenceRecord],
      line: LineNodeRef,
      rootPosition: PositionNodeRef
  ): Option[lila.chessjudgment.model.line.EngineLine] =
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
    uniqueCarrier(evaluations).map(_._2)

  /** An exact endpoint fact has one producer. Multiple equal payloads are
    * still multiple owners and must not be hidden by a representative pick.
    */
  private[assembly] def uniqueCarrier[A](
      candidates: List[(EvidenceRef, A)]
  ): Option[(EvidenceRef, A)] =
    candidates match
      case exact :: Nil => Some(exact)
      case Nil          => None
      case _ =>
        throw IllegalStateException(
          s"an exact endpoint fact has multiple evidence owners: ${candidates.map(_._1.id).sorted.mkString(",")}"
        )

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
      evaluation: lila.chessjudgment.model.line.EngineLine,
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
    uniqueCarrier(records.collect {
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
        case record @ EvidenceRecord(_, payload: PassedPawnResultProofEvidence, _) =>
          passedPawnResultCausalProofDirectlyOwnsCause(graph, fact, kind, binding, record, payload)
        case record @ EvidenceRecord(_, _: ForcedReplyResourceDifferentialEvidence, _) =>
          directProofSource(graph, fact, kind, binding, attributionKind, record)
        case _ if RelativeCauseKind.requiresExactPassedPawnResult(kind) =>
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
    (
      records ++
        ownedParentClosure ++
        explicitTacticalRelationSources
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
      case EvidenceLayer.Line | EvidenceLayer.Relation | EvidenceLayer.CandidateComparison =>
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
    val exactKindCarrier = kind match
      case RelativeCauseKind.WrongMoveOrder =>
        record.payload.isInstanceOf[ForcedReplyResourceDifferentialEvidence]
      case _ if RelativeCauseKind.requiresExactPassedPawnResult(kind) =>
        record.payload.isInstanceOf[PassedPawnResultProofEvidence]
      case _ =>
        true
    verifiedRootRecapture && exactKindCarrier &&
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
      case payload: ForcedReplyResourceDifferentialEvidence =>
        kind == RelativeCauseKind.WrongMoveOrder &&
          binding.sourceSide == RelativeCauseSourceSide.Reference &&
          payload.occurrence.referenceLine == fact.referenceLine &&
          payload.occurrence.playedLine == fact.candidateLine &&
          binding.eventLine == fact.referenceLine &&
          graph.proofEligible(record)
      case payload: PassedPawnResultProofEvidence =>
        passedPawnResultCausalProofDirectlyOwnsCause(graph, fact, kind, binding, record, payload)
      case _ =>
        false)

  private def passedPawnResultCausalProofDirectlyOwnsCause(
      graph: TypedEvidenceGraph,
      fact: CandidateComparisonFact,
      kind: RelativeCauseKind,
      binding: RelativeCauseBinding,
      record: EvidenceRecord,
      proof: PassedPawnResultProofEvidence
  ): Boolean =
    graph.proofEligible(record) &&
      record.ref.line.contains(binding.eventLine) &&
      proof.rootLine == binding.eventLine &&
      EvidenceRef.sameMove(proof.rootMove, binding.eventLine.rootMove) &&
      proof.resultActor.side == fact.comparison.mover &&
      graph.record(proof.comparisonDemand).exists {
        case EvidenceRecord(_, CandidateComparisonEvidence(exact), _) => exact == fact
        case _ => false
      } &&
      kind != RelativeCauseKind.WrongMoveOrder &&
      RelativeCauseKind.passedPawnResultProofCanProveCause(kind, proof)

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
        false
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
      case RelativeCauseKind.ConversionSecured =>
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
      case RelativeCauseKind.TacticalRefutationOfPlayed | RelativeCauseKind.CandidateTacticalLiability
          if payload.kind == TacticalMechanismKind.RecaptureChoice =>
        false
      case _ =>
        true

  private def relationCanDirectlyProveCause(kind: RelativeCauseKind, payload: RelationFactEvidence): Boolean =
    RootOwnedEffectPolicy.relationDirectlyProvesCause(payload, kind)

  private def defensiveCause(kind: RelativeCauseKind): Boolean =
    kind == RelativeCauseKind.DrawResource

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
      case payload: PassedPawnResultProofEvidence =>
        graph.proofEligible(record) && record.ref.line.exists(line =>
          EvidenceRef.sameMove(line.rootMove, rootMove) &&
            payload.rootLine == line && payload.resultActor.side == mover
        )
      case payload: ForcedReplyResourceDifferentialEvidence =>
        graph.proofEligible(record) &&
          payload.semantic.trigger.side == mover &&
          EvidenceRef.sameMove(payload.occurrence.referenceLine.rootMove, rootMove) &&
          EvidenceRef.sameMove(payload.occurrence.triggerStep.moveUci, rootMove)
      case CandidateComparisonEvidence(_) =>
        false
      case _ =>
        false

  private[chessjudgment] def validateRelativeCauseOwnership(
      records: List[EvidenceRecord],
      graph: TypedEvidenceGraph
  ): List[EvidenceRecord] =
    val exactOwners = records.flatMap { record =>
      relativeCauseFromRecord(record).flatMap { cause =>
        val semanticKey = RelativeCauseSemanticKey.from(cause, record.ref.id, graph)
        val channels = RelativeCauseConstructionAdmission.admittedDirectChannels(cause, graph)
        val exactOccurrences = channels.map(_.exactOccurrenceFingerprint).toSet
        val endpointObservations = cause.directEffectAdmission match
          case DirectEffectAdmission.Restricted(observations) =>
            observations.filter { case (occurrence, _) => exactOccurrences(occurrence) }
          case DirectEffectAdmission.Unresolved => Map.empty
        Option.when(semanticKey.sentenceReady)(
          (semanticKey.frame, exactOccurrences, endpointObservations) -> record.ref.id
        )
      }
    }
    val duplicateOwners = exactOwners
      .groupMap(_._1)(_._2)
      .values
      .filter(_.size > 1)
      .map(_.sorted)
      .toList
    require(
      duplicateOwners.isEmpty,
      s"one exact relative Cause fact has multiple evidence owners: ${duplicateOwners.flatten.sorted.mkString(",")}"
    )
    records.sortBy(_.ref.id)

  private def relativeCauseFromRecord(record: EvidenceRecord): Option[RelativeCauseFact] =
    record.payload match
      case RelativeCauseFactEvidence(cause) => Some(cause)
      case _ => None

  private def comparisonProofRecords(neighborhood: ComparisonEvidenceNeighborhood): List[EvidenceRecord] =
    (neighborhood.referenceEndpoint ++ neighborhood.candidateEndpoint)
      .filter(record => record.ref.layer == EvidenceLayer.Line || record.ref.layer == EvidenceLayer.Eval)
      .filterNot(_.ref.line.exists(_.role == LineNodeRole.BranchReply))
      .distinctBy(_.ref.id)

  private def inferCauseCandidates(
      neighborhood: ComparisonEvidenceNeighborhood,
      fact: CandidateComparisonFact,
      demandSource: EvidenceRecord,
      graph: TypedEvidenceGraph
  ): List[RelativeCauseDraft] =
    val profile =
      RelativeCauseSignalProfile.from(
        fact = fact,
        demandSource = demandSource,
        graph = graph,
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
      fact: CandidateComparisonFact
  ): ComparisonEvidenceNeighborhood =
    val referenceTransition = transitionForLine(context, fact.referenceLine)
    val candidateTransition = transitionForLine(context, fact.candidateLine)
    val referenceEndpoint = recordsForLineEndpoint(context, fact.referenceLine).distinctBy(_.ref.id)
    val candidateEndpoint = recordsForLineEndpoint(context, fact.candidateLine).distinctBy(_.ref.id)
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
      (
        endpointLayer(record.ref.layer) ||
          (record.payload match
            case _: ForcedReplyResourceDifferentialEvidence | _: PassedPawnResultProofEvidence =>
              context.evidenceGraph.proofEligible(record)
            case _ => false)
      ) &&
        record.referencesLine(line)
    )

  private def recordsForRootContext(
      context: JudgmentAssemblyContext,
      root: PositionNodeRef
  ): List[EvidenceRecord] =
    context.evidenceGraph.recordsFor(root).filter(record =>
      rootContextLayer(record.ref.layer) &&
        record.ref.position == root &&
        (record.ref.scope == EvidenceScope.BeforePosition || record.ref.scope == EvidenceScope.CurrentPosition)
    )

  private def recordsForTransition(
      context: JudgmentAssemblyContext,
      transition: Option[MoveTransitionEdge]
  ): List[EvidenceRecord] =
    transition.toList.flatMap { edge =>
      context.evidenceGraph.recordsFor(edge.from).filter(record =>
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
      context.evidenceGraph.recordsFor(edge.to).filter(record =>
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
          EvidenceLayer.TacticalMechanism =>
        true
      case _ =>
        false

  private def rootContextLayer(layer: EvidenceLayer): Boolean =
    layer match
      case EvidenceLayer.PositionFeature | EvidenceLayer.Relation =>
        true
      case _ =>
        false

  private def transitionLayer(layer: EvidenceLayer): Boolean =
    layer match
      case EvidenceLayer.MoveTransition | EvidenceLayer.StructuralDelta =>
        true
      case _ =>
        false

  private def afterPositionLayer(layer: EvidenceLayer): Boolean =
    layer match
      case EvidenceLayer.PositionFeature | EvidenceLayer.Relation =>
        true
      case _ =>
        false
