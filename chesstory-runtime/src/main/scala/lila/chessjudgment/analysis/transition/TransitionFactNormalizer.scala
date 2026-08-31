package lila.chessjudgment.analysis.transition

import chess.Color
import lila.chessjudgment.analysis.structure.CanonicalTransitionStructuralDelta
import lila.chessjudgment.model.judgment.*

object TransitionFactNormalizer:

  def fromMoveTransition(
      edge: MoveTransitionEdge,
      replay: CanonicalLineReplay
  ): EvidenceRecord =
    val legal = replay.legalSteps match
      case exact :: Nil => exact
      case _ => throw IllegalArgumentException("a move transition needs exactly one admitted legal replay step")
    val binding = StructuralTransitionBinding(
      moveUci = edge.moveUci,
      role = edge.role,
      from = edge.from,
      to = edge.to,
      line = None,
      perspective = legal.move.piece.color,
      actorRole = Some(EvidencePieceRole(legal.move.piece.role.name))
    )
    val proof = CanonicalTransitionProof.from(binding, replay).getOrElse(
      throw IllegalArgumentException("a move transition must reuse its exact admitted legal replay")
    )
    EvidenceRecord(
      ref = edge.evidence,
      payload = MoveTransitionEvidence(
        moveUci = edge.moveUci,
        from = edge.from,
        to = edge.to,
        canonicalTransitionProof = Some(proof)
      )
    )

  def fromStructuralDelta(
      id: String,
      delta: CanonicalTransitionStructuralDelta,
      transition: MoveTransitionEdge,
      replay: CanonicalLineReplay,
      line: Option[LineNodeRef],
      perspective: Color,
      derivedRelationSources: List[StructuralDerivedRelationSource],
      parents: List[EvidenceRef]
  ): EvidenceRecord =
    val structural = delta.structural
    val binding = StructuralTransitionBinding(
      moveUci = transition.moveUci,
      role = transition.role,
      from = transition.from,
      to = transition.to,
      line = line,
      perspective = perspective,
      actorRole = replay.legalSteps.headOption.map(step => EvidencePieceRole(step.move.piece.role.name))
    )
    val transitionProof = CanonicalTransitionProof
      .from(binding, replay)
      .getOrElse(
        throw IllegalArgumentException(
          s"structural transition '${transition.evidence.id}' must reuse its admitted legal move"
        )
      )
    val structuralOccurrence = replay.onlyTransition
      .filter(_.ownsStructuralDelta(structural))
      .map(_.structuralOccurrence)
      .getOrElse(
        throw IllegalArgumentException(
          s"structural transition '${transition.evidence.id}' must project its replay-owned structural occurrence"
        )
      )
    val signals = structuralOccurrence.signals
    val consequences = structuralOccurrence.consequences
    val relationChanges = delta.canonicalRelations.changes
    val deltaProof = CanonicalTransitionDeltaProof.from(
      transitionProof,
      signals,
      consequences,
      relationChanges,
      structural.derivedRelations,
      derivedRelationSources
    )
    val ref =
      EvidenceRef(
        id = id,
        producer = EvidenceProducer.StructuralDeltaProducer,
        layer = EvidenceLayer.StructuralDelta,
        position = transition.from,
        line = line,
        scope = transition.role.scope,
        confidence = EvidenceConfidence.BoardDerived
      )
    EvidenceRecord(
      ref = ref,
      payload = StructuralDeltaEvidence(
        transition = binding,
        signals = signals,
        consequences = consequences,
        relationChanges = relationChanges,
        derivedRelationSources = derivedRelationSources,
        canonicalTransitionProof = Some(transitionProof),
        canonicalDeltaProof = Some(deltaProof),
        replayStructuralOccurrence = Some(structuralOccurrence)
      ),
      parents = parents
    )

  def fromCandidateComparison(
      id: String,
      comparison: CandidateComparisonFact,
      position: PositionNodeRef,
      scope: EvidenceScope,
      parents: List[EvidenceRef] = Nil
  ): EvidenceRecord =
    val ref =
      EvidenceRef(
        id = id,
        producer = EvidenceProducer.RelativeMoveProducer,
        layer = EvidenceLayer.CandidateComparison,
        position = position,
        line = Some(comparison.candidateLine),
        scope = scope,
        confidence = comparison.verdictConfidence.evidenceConfidence
      )
    EvidenceRecord(
      ref = ref,
      payload = CandidateComparisonEvidence(comparison),
      parents = parents
    )

  def fromRelativeAssessment(assessment: RelativeMoveAssessment): EvidenceRecord =
    val parents =
      (
        List(
          assessment.played.evidence,
          assessment.reference.evidence,
          assessment.candidate.evidence
        ) ++
          assessment.referenceTransition.toList.map(_.evidence) ++
          (assessment.primaryComparisonEvidence :: assessment.relatedComparisonEvidence) ++
          assessment.relativeCauseEvidence
      ).distinctBy(_.id)
    EvidenceRecord(
      ref = assessment.evidence,
      payload = RelativeAssessmentEvidence(assessment),
      parents = parents
    )

  def fromRelativeCause(
      id: String,
      cause: RelativeCauseFact,
      binding: RelativeCauseBinding,
      position: PositionNodeRef,
      scope: EvidenceScope,
      confidence: EvidenceConfidence,
      parents: List[EvidenceRef] = Nil
  ): EvidenceRecord =
    val ref =
      EvidenceRef(
        id = id,
        producer = EvidenceProducer.RelativeMoveProducer,
        layer = EvidenceLayer.RelativeCause,
        position = position,
        line = Some(binding.eventLine),
        scope = scope,
        confidence = confidence
      )
    EvidenceRecord(
      ref = ref,
      payload = RelativeCauseFactEvidence(cause),
      parents = parents
    )
