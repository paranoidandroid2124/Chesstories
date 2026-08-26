package lila.chessjudgment.analysis.transition

import chess.Color
import lila.chessjudgment.analysis.structure.{ CanonicalTransitionStructuralDelta, StructuralDeltaContracts }
import lila.chessjudgment.model.judgment.*

object TransitionFactNormalizer:

  def fromMoveTransition(edge: MoveTransitionEdge): EvidenceRecord =
    EvidenceRecord(
      ref = edge.evidence,
      payload = MoveTransitionEvidence(
        moveUci = edge.moveUci,
        from = edge.from,
        to = edge.to
      )
    )

  def fromStructuralDelta(
      id: String,
      delta: CanonicalTransitionStructuralDelta,
      transition: MoveTransitionEdge,
      replay: CanonicalLineReplay,
      line: Option[LineNodeRef],
      perspective: Color,
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
    val signals = StructuralDeltaContracts.signals(structural)
    val consequences = StructuralDeltaContracts.consequences(structural)
    val relationChanges = delta.canonicalRelations.changes
    val deltaProof = CanonicalTransitionDeltaProof.from(
      transitionProof,
      signals,
      consequences,
      relationChanges
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
        canonicalTransitionProof = Some(transitionProof),
        canonicalDeltaProof = Some(deltaProof)
      ),
      parents = parents
    )

  def fromPlanTransition(
      id: String,
      proof: PlanSequenceProof,
      position: PositionNodeRef,
      line: Option[LineNodeRef],
      scope: EvidenceScope,
      confidence: EvidenceConfidence,
      parents: List[EvidenceRef] = Nil
  ): EvidenceRecord =
    val ref =
      EvidenceRef(
        id = id,
        producer = EvidenceProducer.PlanTransitionProducer,
        layer = EvidenceLayer.PlanTransition,
        position = position,
        line = line,
        scope = scope,
        confidence = confidence
      )
    EvidenceRecord(
      ref = ref,
      payload = PlanTransitionEvidence(proof),
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
