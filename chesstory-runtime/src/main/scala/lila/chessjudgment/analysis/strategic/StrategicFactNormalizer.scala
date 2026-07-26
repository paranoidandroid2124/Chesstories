package lila.chessjudgment.analysis.strategic

import lila.chessjudgment.model.judgment.PawnPlayAnalysis
import lila.chessjudgment.model.{ ActivePlans, Fact, PlanScoringResult }
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.structure.{ PlanAlignment, StructureProfile }
import lila.chessjudgment.model.strategic.PlanTaxonomy.PlanKind

object StrategicFactNormalizer:

  def fromFacts(
      id: String,
      kind: StrategicFactKind,
      facts: List[Fact],
      relatedPlans: List[PlanKind],
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
      ),
      parents = parents
    )

  def fromBoardAnchors(
      id: String,
      kind: StrategicFactKind,
      anchors: List[BoardAnchor],
      relatedPlans: List[PlanKind],
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
        confidence = confidence,
        boardAnchors = anchors
      ),
      parents = parents
    )

  def fromPawnStructure(
      id: String,
      profile: StructureProfile,
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
      payload = PawnStructureFactEvidence(profile, pawnPlay),
      parents = parents
    )

  def fromThreatEpisode(
      id: String,
      episode: ThreatEpisode,
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
          if episode.hasMotifProof then EvidenceConfidence.Mixed
          else EvidenceConfidence.Heuristic
      )
    EvidenceRecord(ref = ref, payload = ThreatEpisodeEvidence(episode), parents = parents)

  def fromPlanPressure(
      id: String,
      scoring: PlanScoringResult,
      activePlans: ActivePlans,
      alignment: Option[PlanAlignment],
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
    EvidenceRecord(ref = ref, payload = PlanPressureEvidence(activePlans, alignment), parents = parents)
