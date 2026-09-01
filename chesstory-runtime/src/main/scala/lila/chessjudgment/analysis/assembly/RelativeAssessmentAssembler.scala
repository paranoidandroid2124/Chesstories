package lila.chessjudgment.analysis.assembly

import chess.{ Color, Square }
import lila.chessjudgment.analysis.transition.TransitionFactNormalizer
import lila.chessjudgment.model.judgment.*

object RelativeAssessmentAssembler:

  final private case class ComparisonEvidenceNeighborhood(
      referenceRecords: List[EvidenceRecord],
      candidateRecords: List[EvidenceRecord]
  )
  final private case class RelativeCauseConstructionCandidate(
      cause: RelativeCauseFact,
      binding: RelativeCauseBinding,
      parents: List[EvidenceRef]
  )

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
        val admittedRootRankingPair = inputs.input.admittedRootRankingPair
        candidateComparisonRecords(
          mover = inputs.mover,
          context = context,
          reference = inputs.reference,
          candidate = inputs.candidate,
          admittedRootRankingPair = admittedRootRankingPair,
          root = inputs.root,
          allocator = inputs.allocator
        ).flatMap { comparisonRecords =>
          primaryComparisonRecord(comparisonRecords, inputs.reference, inputs.candidate).map { primary =>
            val comparisonContext = context.withEvidence(comparisonRecords)
            val exactCausalInput = ExactPlayedVsBestCausalInput.from(comparisonContext, primary)
            val passedPawnResultDemand = exactCausalInput.flatMap(exact =>
              PassedPawnResultEventAssembler.changedDependencyDemand(
                comparisonContext,
                exact
              )
            )
            val causalDemand = exactCausalInput.map(RelationCausalProofDemand.from)
            val defenderDisplacementProofRecords = causalDemand.toList.flatMap(demand =>
              UniqueCheckReplyDefenderDisplacementBeforeCaptureAssembler.fromDemand(
                comparisonContext,
                inputs.allocator,
                demand
              )
            )
            val soleRecapturerRemovalProofRecords = causalDemand.toList.flatMap(demand =>
              SoleRecapturerRemovalBeforeTargetCaptureAssembler.fromDemand(
                comparisonContext,
                inputs.allocator,
                demand
              )
            )
            val vacatedGateCaptureProofRecords = causalDemand.toList.flatMap(demand =>
              VacatedGateEnablesUnrecapturableSliderCaptureAssembler.fromDemand(
                comparisonContext,
                inputs.allocator,
                demand
              )
            )
            val vacancyOccupationProofRecords = causalDemand.toList.flatMap(demand =>
              VacancyEnablesOccupationAssembler.fromDemand(
                comparisonContext,
                inputs.allocator,
                demand
              )
            )
            val passedPawnResultEventRecords = passedPawnResultDemand.toList.flatMap(demand =>
              PassedPawnResultEventAssembler.fromDemand(
                comparisonContext,
                inputs.allocator,
                demand
              )
            )
            val passedPawnResultContext = comparisonContext.withEvidence(passedPawnResultEventRecords)
            val passedPawnProgressRealizedAfterOnlyLegalReplyProofRecords = passedPawnResultDemand.toList.flatMap(demand =>
              PassedPawnProgressRealizedAfterOnlyLegalReplyProofAssembler.fromDemand(
                passedPawnResultContext,
                inputs.allocator,
                demand
              )
            )
            passedPawnResultContext.withEvidence(
              passedPawnProgressRealizedAfterOnlyLegalReplyProofRecords ++ defenderDisplacementProofRecords ++
                soleRecapturerRemovalProofRecords ++ vacatedGateCaptureProofRecords ++
                vacancyOccupationProofRecords
            )
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
            fact.hasDistinctRootMoves =>
        record
    } match
      case record :: Nil => Some(record)
      case _ => None

  private def candidateComparisonRecords(
      mover: Color,
      context: JudgmentAssemblyContext,
      reference: CandidateLineNode,
      candidate: CandidateLineNode,
      admittedRootRankingPair: Option[AdmittedRootRankingPair],
      root: PositionNodeRef,
      allocator: JudgmentProvenanceAllocator
  ): Option[List[EvidenceRecord]] =
    val ordered = context.lines
      .sortBy(_.ref.rank)
    val orderedRootMoves = ordered.map(line => EvidenceRef.normalizeMove(line.ref.rootMove))
    require(
      orderedRootMoves.distinct.size == orderedRootMoves.size,
      "candidate comparison inputs must have one admitted line per root move"
    )
    val second = admittedRootRankingPair.map { pair =>
      if !EvidenceRef.sameMove(pair.bestMoveUci, reference.ref.rootMove) then
        throw IllegalStateException("admitted ranking best move does not match the reference line")
      ordered
        .find(line => EvidenceRef.sameMove(line.ref.rootMove, pair.secondMoveUci))
        .getOrElse(throw IllegalStateException("admitted ranking second move was not projected"))
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
            candidate
          )
        ),
        second
          .filterNot(_ => candidateIsSecond)
          .map(line =>
            (
              CandidateComparisonKind.BestVsSecond,
              reference,
              line
            )
          )
      ).flatten ++
        alternatives.flatMap { alternative =>
          List(
            (
              CandidateComparisonKind.PlayedVsAlternative,
              alternative,
              candidate
            )
          ) ++
            Option
              .unless(
                second.exists(line => EvidenceRef.sameMove(line.ref.rootMove, alternative.ref.rootMove))
              )(
                (
                  CandidateComparisonKind.ReferenceVsAlternative,
                  reference,
                  alternative
                )
              )
              .toList
        }
    val comparisonInputs = comparisons.map { case (kind, refLine, candLine) =>
      (kind, refLine.ref.id, candLine.ref.id)
    }
    require(
      comparisonInputs.distinct.size == comparisonInputs.size,
      "candidate comparison construction cannot hide duplicate semantic inputs"
    )
    val records = comparisons.map { case (kind, refLine, candLine) =>
      val comparison = CandidateComparisonFact
        .fromLines(kind, mover, refLine, candLine)
        .getOrElse(
          throw IllegalStateException(
            s"admitted candidate comparison '$kind' has no exact distinct line pair"
          )
        )
      val parents =
        parentsForLine(context, refLine) ++ parentsForLine(context, candLine)
      require(
        parents.map(_.id).distinct.size == parents.size,
        s"candidate comparison '$kind' repeats a direct line owner"
      )
      val semanticKey = CandidateComparisonSemanticKey.from(comparison)
      TransitionFactNormalizer.fromCandidateComparison(
        id = allocator.evidenceId(
          s"candidate-comparison:${allocator.exactKey(List(semanticKey.stableKey))}"
        ),
        comparison = comparison,
        position = root,
        scope = EvidenceScope.Counterfactual,
        parents = parents
      )
    }
    Some(records)

  private def relativeCauseRecords(
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator,
      comparisonRecord: EvidenceRecord
  ): List[EvidenceRecord] =
    comparisonRecord.payload match
      case CandidateComparisonEvidence(fact)
          if ActionablePlayedVsBestCausalProofDemand.accepts(fact) =>
        val neighborhood = comparisonNeighborhood(context, fact)
        val causes =
          inferCauseCandidates(
            neighborhood,
            fact,
            comparisonRecord,
            context.evidenceGraph
          )
        val provisional = causes.map { candidate =>
          val kind = candidate.kind
          val support = candidate.support
          require(support.nonEmpty, s"relative Cause '$kind' requires an exact typed proof source")
          val proofSources = support.map(_.ref)
          require(
            proofSources.map(_.id).distinct.size == proofSources.size,
            s"relative Cause '$kind' draft repeats an exact typed proof source"
          )
          val binding =
            RelativeCauseFact.binding(
              comparisonKind = fact.kind,
              referenceLine = fact.referenceLine,
              candidateLine = fact.candidateLine,
              sourceSide = candidate.sourceSide
            )
          val cause =
            RelativeCauseFact(
              kind = kind,
              comparisonEvidence = comparisonRecord.ref,
              sourceSide = binding.sourceSide,
              proofSources = proofSources
            )
          RelativeCauseConstructionCandidate(
            cause = cause,
            binding = binding,
            parents = comparisonRecord.ref :: proofSources
          )
        }
        val provisionalChannels = provisional.map(item =>
          item -> DirectCauseChannel.certifiedForCause(item.cause, context.evidenceGraph)
        )
        val admitted = provisionalChannels.map { case (item, channels) =>
          require(
            channels.map(_.source.id).sorted == item.cause.proofSources.map(_.id),
            s"relative Cause '${item.cause.kind}' has a non-exact draft proof source"
          )
          require(
            RelativeCauseConstructionAdmission.initiallyReadyWithChannels(
              item.cause,
              context.evidenceGraph,
              channels
            ),
            s"relative Cause '${item.cause.kind}' failed exact construction admission"
          )
          val semanticKey = RelativeCauseSemanticKey(
            frame = RelativeCauseSemanticFrameKey(
              kind = item.cause.kind,
              comparison = CandidateComparisonSemanticKey.from(fact),
              eventLine = SemanticLineKey.from(item.binding.eventLine),
              sourceSide = item.cause.sourceSide
            ),
            exactProofOccurrences = channels.map(_.exactOccurrenceFingerprint)
          )
          (item, channels, semanticKey)
        }
        val duplicateSemanticOwners = admitted
          .groupMap(_._3.stableKey)(_._1.cause.proofSources.map(_.id))
          .collect { case (key, owners) if owners.size > 1 => key -> owners.flatten.sorted }
          .toList
        require(
          duplicateSemanticOwners.isEmpty,
          s"one exact relative Cause semantic occurrence has multiple drafts: ${duplicateSemanticOwners.flatMap(_._2).mkString(",")}"
        )
        admitted.map { case (item, channels, semanticKey) =>
          val evidenceId = allocator.evidenceId(
            s"relative-cause:${allocator.exactKey(List(semanticKey.stableKey))}"
          )
          TransitionFactNormalizer.fromRelativeCause(
            id = evidenceId,
            cause = item.cause,
            binding = item.binding,
            position = comparisonRecord.ref.position,
            scope = EvidenceScope.Counterfactual,
            confidence = proofConfidenceForAdmittedChannels(channels),
            parents = item.parents
          )
        }
      case _ => Nil

  private def proofConfidenceForAdmittedChannels(
      channels: List[DirectCauseChannel]
  ): EvidenceConfidence =
    channels.map(_.source.confidence).distinct match
      case Nil =>
        throw IllegalArgumentException("relative Cause admission requires at least one typed proof channel")
      case confidence :: Nil => confidence
      case _ => EvidenceConfidence.Mixed

  private[chessjudgment] def validateRelativeCauseOwnership(
      records: List[EvidenceRecord],
      graph: TypedEvidenceGraph
  ): List[EvidenceRecord] =
    val exactOwners = records.flatMap { record =>
      relativeCauseFromRecord(record).flatMap { cause =>
        val semanticKey = RelativeCauseSemanticKey.from(cause, record.ref.id, graph)
        Some((semanticKey.frame, semanticKey.exactProofOccurrences) -> record.ref.id)
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
        candidateRecords = neighborhood.candidateRecords
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
      fact: CandidateComparisonFact
  ): ComparisonEvidenceNeighborhood =
    ComparisonEvidenceNeighborhood(
      referenceRecords = typedProofRecordsForLine(context, fact.referenceLine),
      candidateRecords = typedProofRecordsForLine(context, fact.candidateLine)
    )

  private def typedProofRecordsForLine(
      context: JudgmentAssemblyContext,
      line: LineNodeRef
  ): List[EvidenceRecord] =
    context.evidenceGraph.records.filter { record =>
      val typedProof = record.payload match
        case _: UniqueCheckReplyDefenderDisplacementBeforeCaptureEvidence |
            _: SoleRecapturerRemovalBeforeTargetCaptureEvidence |
            _: VacatedGateEnablesUnrecapturableSliderCaptureEvidence |
            _: VacancyEnablesOccupationEvidence |
            _: PassedPawnProgressRealizedAfterOnlyLegalReplyProofEvidence => true
        case _ => false
      typedProof && context.evidenceGraph.proofEligible(record) && record.referencesLine(line)
    }
