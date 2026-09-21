package lila.chessjudgment.analysis.assembly

import chess.Color
import lila.chessjudgment.analysis.transition.TransitionFactNormalizer
import lila.chessjudgment.model.judgment.*

object RelativeAssessmentAssembler:

  final private case class RelativeAssemblyInputs(
      input: AdmittedMoveReviewInput,
      played: MoveTransitionEdge,
      referenceTransition: Option[MoveTransitionEdge],
      reference: CandidateLineNode,
      candidate: CandidateLineNode,
      root: PositionNodeRef,
      mover: Color,
      allocator: JudgmentProvenanceAllocator
  )

  def enrichComparisonEvidence(context: JudgmentAssemblyContext): JudgmentAssemblyContext =
    relativeAssemblyInputs(context)
      .flatMap { inputs =>
        val admittedRootRankingPair = inputs.input.admittedRootRankingPair
        candidateComparisonRecord(
          mover = inputs.mover,
          context = context,
          reference = inputs.reference,
          candidate = inputs.candidate,
          admittedRootRankingPair = admittedRootRankingPair,
          root = inputs.root,
          allocator = inputs.allocator
        ).map(context.withEvidence)
      }
      .getOrElse(context)

  /** Assessment-only projection. Typed occurrence relation proofs are owned
    * by the occurrence-directed producer before this stage.
    */
  def enrichAssessment(context: JudgmentAssemblyContext): JudgmentAssemblyContext =
    relativeAssemblyInputs(context)
      .flatMap { inputs =>
        val comparisonRecords = assembledComparisonRecords(context, inputs.root)
        soleComparisonRecord(comparisonRecords).map {
          primaryComparisonRecord =>
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
                referenceTransition = inputs.referenceTransition,
                reference = inputs.reference,
                candidate = inputs.candidate,
                evidence = relativeEvidence,
                primaryComparisonEvidence = primaryComparisonRecord.ref
              )
            context.withEvidence(TransitionFactNormalizer.fromRelativeAssessment(assessment))
        }
      }
      .getOrElse(context)

  private def relativeAssemblyInputs(context: JudgmentAssemblyContext): Option[RelativeAssemblyInputs] =
    val input = context.input
    for
      played <- context.playedTransition
      reference <- context.line(LineNodeRole.BestReference)
      candidate <- context.lineForRootMove(input.playedMoveUci)
      referenceTransition <-
        if EvidenceRef.sameMove(reference.ref.rootMove, input.playedMoveUci) then Some(None)
        else
          context
            .lineRootOccurrence(reference.ref)
            .map(Some(_))
      root <- context.position(PositionNodeRole.Before).map(_.ref)
      mover <- root.sideToMove
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

  private def soleComparisonRecord(records: List[EvidenceRecord]): Option[EvidenceRecord] =
    records match
      case record :: Nil => Some(record)
      case _             => None

  private def candidateComparisonRecord(
      mover: Color,
      context: JudgmentAssemblyContext,
      reference: CandidateLineNode,
      candidate: CandidateLineNode,
      admittedRootRankingPair: Option[AdmittedRootRankingPair],
      root: PositionNodeRef,
      allocator: JudgmentProvenanceAllocator
  ): Option[EvidenceRecord] =
    val ordered = context.lines.sortBy(_.rank)
    val playedIsBest = EvidenceRef.sameMove(reference.ref.rootMove, candidate.ref.rootMove)
    val comparisonInput =
      if !playedIsBest then
        Some((CandidateComparisonKind.PlayedVsBest, reference, candidate))
      else
        for
          pair <- admittedRootRankingPair
          if EvidenceRef.sameMove(pair.bestMoveUci, reference.ref.rootMove)
          second <- ordered.find(line => EvidenceRef.sameMove(line.ref.rootMove, pair.secondMoveUci))
        yield (CandidateComparisonKind.BestVsSecond, reference, second)
    comparisonInput.flatMap { case (kind, refLine, candLine) =>
      val parents =
        parentsForLine(context, refLine) ++ parentsForLine(context, candLine)
      for
        _ <- Option.when(parents.map(_.id).distinct.size == parents.size)(())
        comparison <- CandidateComparisonFact.fromLines(kind, mover, refLine, candLine)
      yield
        val semanticKey = CandidateComparisonSemanticKey.from(comparison)
        TransitionFactNormalizer.fromCandidateComparison(
          id = allocator.evidenceId(
            s"candidate-comparison:${EvidenceIdentity.digest(List(semanticKey.stableKey))}"
          ),
          comparison = comparison,
          position = root,
          scope = EvidenceScope.Counterfactual,
          parents = parents
        )
    }

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
