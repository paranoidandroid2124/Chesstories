package lila.chessjudgment.analysis.transition

import lila.chessjudgment.analysis.structure.CanonicalTransitionStructuralDelta
import lila.chessjudgment.model.judgment.*

object TransitionFactNormalizer:

  def fromMoveTransition(
      edge: MoveTransitionEdge,
      replay: CanonicalLineReplay
  ): EvidenceRecord =
    val root = replay.onlyTransition
      .map(_.relationDelta.rootMove)
      .getOrElse(throw IllegalArgumentException("a move transition needs exactly one admitted legal replay step"))
    val binding = StructuralTransitionBinding(
      moveUci = edge.moveUci,
      role = edge.role,
      from = edge.from,
      to = edge.to,
      line = None,
      perspective = root.side,
      actorRole = Some(root.beforeRole)
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
      resultPremiseSources: List[TransitionResultPremiseSource],
      parents: List[EvidenceRef]
  ): EvidenceRecord =
    val structural = delta.structural
    val root = replay.onlyTransition
      .map(_.relationDelta.rootMove)
      .getOrElse(throw IllegalArgumentException("a structural transition needs exactly one admitted legal replay step"))
    require(root.side == structural.perspective, "a structural transition must retain its canonical root perspective")
    val binding = StructuralTransitionBinding(
      moveUci = transition.moveUci,
      role = transition.role,
      from = transition.from,
      to = transition.to,
      line = line,
      perspective = root.side,
      actorRole = Some(root.beforeRole)
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
    val consequences = structuralOccurrence.consequences
    val requiredRelationKeys = consequences.flatMap(_.relationKeys).toSet
    val relationChanges = delta.canonicalRelations.changes.filter(change => requiredRelationKeys(change.key))
    require(
      relationChanges.map(_.key).toSet == requiredRelationKeys,
      "a structural transition must resolve every consequence premise from its canonical relation delta"
    )
    val deltaProof = CanonicalTransitionDeltaProof.from(
      transitionProof,
      delta.canonicalRelations,
      consequences,
      relationChanges,
      resultPremiseSources
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
        consequences = consequences,
        relationChanges = relationChanges,
        resultPremiseSources = resultPremiseSources,
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
