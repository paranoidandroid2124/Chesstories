package lila.chessjudgment.model.judgment

import lila.chessjudgment.model.line.PrincipalVariationEvidence

/** Canonical CandidateComparison record shape shared by the graph and exact
  * downstream certificates. The graph separately reconstructs the fact from
  * these resolved evaluation parents.
  */
private[chessjudgment] object CandidateComparisonRecordAuthority:
  def exactShape(
      source: EvidenceRecord,
      fact: CandidateComparisonFact,
      root: PositionNodeRef,
      referenceSource: EvidenceRecord,
      candidateSource: EvidenceRecord
  ): Boolean =
    source match
      case EvidenceRecord(ref, CandidateComparisonEvidence(value), parents) =>
        val lineParents = parents.filter(_.layer == EvidenceLayer.Line)
        val evaluationParents = parents.filter(_.layer == EvidenceLayer.Eval)
        val comparisonParents = parents.filter(_.layer == EvidenceLayer.CandidateComparison)
        value == fact && ref.producer == EvidenceProducer.RelativeMoveProducer &&
          ref.layer == EvidenceLayer.CandidateComparison && ref.position == root &&
          ref.line.contains(fact.candidateLine) && ref.scope == EvidenceScope.Counterfactual &&
          ref.confidence == fact.verdictConfidence.evidenceConfidence &&
          exactLineSource(referenceSource, fact.referenceLine, root) &&
          exactLineSource(candidateSource, fact.candidateLine, root) &&
          lineParents.size == 2 &&
          lineParents.toSet == Set(referenceSource.ref, candidateSource.ref) &&
          exactEvaluationParents(evaluationParents, fact, root) &&
          comparisonParents.isEmpty &&
          parents.map(_.id).distinct.size == parents.size &&
          parents.size == lineParents.size + evaluationParents.size + comparisonParents.size
      case _ => false

  private def exactLineSource(
      source: EvidenceRecord,
      line: LineNodeRef,
      root: PositionNodeRef
  ): Boolean =
    source match
      case EvidenceRecord(ref, payload: LineFactEvidence, _) =>
        ref.producer == EvidenceProducer.LegalLineProducer && ref.layer == EvidenceLayer.Line &&
          ref.confidence == EvidenceConfidence.LegalReplayVerified && ref.position == root &&
          ref.line.contains(line) && ref.scope == line.role.scope && payload.line == line &&
          payload.replayIsCertified &&
          payload.rootMove.exists(EvidenceRef.sameMove(_, line.rootMove)) &&
          payload.canonicalReplay.exists(_.replaySteps.headOption.exists(step =>
            PrincipalVariationEvidence.sameBoardState(step.fenBefore, root.fen)
          ))
      case _ => false

  private def exactEvaluationParents(
      parents: List[EvidenceRef],
      fact: CandidateComparisonFact,
      root: PositionNodeRef
  ): Boolean =
    val lines = Set(fact.referenceLine, fact.candidateLine)
    val exactRefs = parents.forall(parent =>
      parent.position == root && parent.line.exists(lines) &&
        parent.line.exists(line => parent.scope == line.role.scope) &&
        ((parent.producer == EvidenceProducer.EngineEvalProducer &&
          parent.confidence == EvidenceConfidence.EngineBacked) ||
          (parent.producer == EvidenceProducer.LegalLineProducer &&
            parent.confidence == EvidenceConfidence.LegalReplayVerified))
    )
    val exactConfidence = fact.verdictConfidence match
      case VerdictConfidence.EngineBacked =>
        parents.forall(_.confidence == EvidenceConfidence.EngineBacked)
      case VerdictConfidence.LegalReplayVerified =>
        parents.forall(_.confidence == EvidenceConfidence.LegalReplayVerified)
      case VerdictConfidence.MixedVerified =>
        parents.map(_.confidence).toSet ==
          Set(EvidenceConfidence.EngineBacked, EvidenceConfidence.LegalReplayVerified)
    parents.size == 2 && parents.flatMap(_.line).toSet == lines && exactRefs && exactConfidence
