package lila.chessjudgment.analysis.strategic

import lila.chessjudgment.analysis.singlePosition.{ PawnPlayAnalysis, ThreatAnalysis }
import lila.chessjudgment.model.{ ActivePlans, Fact, PlanId, PlanScoringResult }
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.structure.{ PlanAlignment, StructureProfile }

object StrategicFactNormalizer:

  def fromFacts(
      id: String,
      kind: StrategicFactKind,
      facts: List[Fact],
      relatedPlans: List[PlanId],
      confidence: Double,
      position: PositionNodeRef,
      line: Option[LineNodeRef],
      scope: EvidenceScope,
      parents: List[EvidenceRef] = Nil
  ): EvidenceRecord =
    val ref =
      EvidenceRef(
        id = id,
        producer = EvidenceProducer.StrategicFeatureProducer,
        layer = EvidenceLayer.Strategic,
        position = position,
        line = line,
        scope = scope,
        confidence = if confidence >= 0.75 then EvidenceConfidence.BoardDerived else EvidenceConfidence.Heuristic
      )
    EvidenceRecord(
      ref = ref,
      payload = StrategicFactEvidence(
        kind = kind,
        facts = facts,
        relatedPlans = relatedPlans,
        confidence = confidence
      )(),
      parents = parents
    )

  def fromBoardAnchors(
      id: String,
      kind: StrategicFactKind,
      anchors: List[BoardAnchor],
      relatedPlans: List[PlanId],
      confidence: Double,
      position: PositionNodeRef,
      line: Option[LineNodeRef],
      scope: EvidenceScope,
      parents: List[EvidenceRef] = Nil
  ): EvidenceRecord =
    val ref =
      EvidenceRef(
        id = id,
        producer = EvidenceProducer.StrategicFeatureProducer,
        layer = EvidenceLayer.Strategic,
        position = position,
        line = line,
        scope = scope,
        confidence = if confidence >= 0.75 then EvidenceConfidence.BoardDerived else EvidenceConfidence.Heuristic
      )
    EvidenceRecord(
      ref = ref,
      payload = StrategicFactEvidence(
        kind = kind,
        facts = Nil,
        relatedPlans = relatedPlans,
        confidence = confidence
      )(boardAnchors = anchors),
      parents = parents
    )

  def fromPawnStructure(
      id: String,
      profile: StructureProfile,
      alignment: Option[PlanAlignment],
      pawnPlay: Option[PawnPlayAnalysis],
      position: PositionNodeRef,
      scope: EvidenceScope,
      parents: List[EvidenceRef] = Nil
  ): EvidenceRecord =
    val ref =
      EvidenceRef(
        id = id,
        producer = EvidenceProducer.PawnStructureProducer,
        layer = EvidenceLayer.PawnStructure,
        position = position,
        line = None,
        scope = scope,
        confidence = if profile.confidence >= 0.75 then EvidenceConfidence.BoardDerived else EvidenceConfidence.Heuristic
      )
    EvidenceRecord(
      ref = ref,
      payload = PawnStructureFactEvidence(profile, alignment, pawnPlay),
      parents = parents
    )

  def fromThreatPressure(
      id: String,
      sideUnderPressure: _root_.chess.Color,
      threats: ThreatAnalysis,
      position: PositionNodeRef,
      line: Option[LineNodeRef],
      scope: EvidenceScope,
      parents: List[EvidenceRef] = Nil
  ): EvidenceRecord =
    val ref =
      EvidenceRef(
        id = id,
        producer = EvidenceProducer.ThreatPressureProducer,
        layer = EvidenceLayer.ThreatPressure,
        position = position,
        line = line,
        scope = scope,
        confidence = if threats.insufficientData then EvidenceConfidence.Mixed else EvidenceConfidence.EngineBacked
    )
    EvidenceRecord(ref = ref, payload = ThreatPressureEvidence(sideUnderPressure, threats), parents = parents)

  def fromThreatEpisode(
      id: String,
      episode: ThreatEpisode,
      summary: ThreatAnalysis,
      position: PositionNodeRef,
      line: Option[LineNodeRef],
      scope: EvidenceScope,
      parents: List[EvidenceRef] = Nil
  ): EvidenceRecord =
    val ref =
      EvidenceRef(
        id = id,
        producer = EvidenceProducer.ThreatPressureProducer,
        layer = EvidenceLayer.ThreatPressure,
        position = position,
        line = line,
        scope = scope,
        confidence =
          if episode.hasLineValueProof then EvidenceConfidence.EngineBacked
          else if episode.hasMotifProof then EvidenceConfidence.Mixed
          else EvidenceConfidence.Heuristic
      )
    EvidenceRecord(ref = ref, payload = ThreatEpisodeEvidence(episode, summary), parents = parents)

  def fromPlanPressure(
      id: String,
      scoring: PlanScoringResult,
      activePlans: ActivePlans,
      position: PositionNodeRef,
      line: Option[LineNodeRef],
      scope: EvidenceScope,
      parents: List[EvidenceRef] = Nil
  ): EvidenceRecord =
    val ref =
      EvidenceRef(
        id = id,
        producer = EvidenceProducer.PlanPressureProducer,
        layer = EvidenceLayer.PlanPressure,
        position = position,
        line = line,
        scope = scope,
        confidence = if scoring.confidence >= 0.75 then EvidenceConfidence.Mixed else EvidenceConfidence.Heuristic
      )
    EvidenceRecord(ref = ref, payload = PlanPressureEvidence(scoring, activePlans), parents = parents)
