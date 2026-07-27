package lila.chessjudgment.analysis.line

import chess.{ Queen, Square, White }
import chess.variant.Standard
import lila.chessjudgment.model.Motif
import lila.chessjudgment.model.line.PrincipalVariationEvidence
import lila.chessjudgment.analysis.position.PositionAnalyzer
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.strategic.EngineLine

class TruthBoundaryTest extends munit.FunSuite:

  test("material imbalance comes from board piece counts"):
    val balanced = PositionAnalyzer
      .extractFeatures(Standard.initialFen.value, plyCount = 0)
      .getOrElse(fail("expected initial-position features"))
    val extraWhiteKnight = PositionAnalyzer
      .extractFeatures("4k3/8/8/8/8/8/8/1N2K3 w - - 0 1", plyCount = 0)
      .getOrElse(fail("expected imbalanced-position features"))

    assertEquals(balanced.imbalance.whiteKnights, balanced.imbalance.blackKnights)
    assertEquals(extraWhiteKnight.imbalance.whiteKnights, 1)
    assertEquals(extraWhiteKnight.imbalance.blackKnights, 0)

  test("an exact named opening sequence is not itself semantic proof"):
    val afterE4 = PrincipalVariationEvidence
      .legalFenAfter(Standard.initialFen.value, "e2e4")
      .getOrElse(fail("expected 1. e4 to be legal"))
    val beforeQueenMove = PrincipalVariationEvidence
      .legalFenAfter(afterE4, "e7e5")
      .getOrElse(fail("expected 1... e5 to be legal"))
    val declaredLine = EngineLine(
      moves = List("d1h5", "b8c6", "f1c4", "g8f6", "h5f7"),
      scoreCp = 900,
      depth = 18
    )

    assertEquals(
      ForcedLineTruth.detect(beforeQueenMove, "d1h5", List(declaredLine)),
      None
    )

  test("conversion cause polarity is owned by exactly one comparison side"):
    assert(
      RelativeCauseKind.sourceAttributionCompatible(
        RelativeCauseKind.ConversionMiss,
        RelativeCauseSourceSide.Candidate,
        CauseAttributionKind.CandidateAllowsLiability
      )
    )
    assert(
      !RelativeCauseKind.sourceAttributionCompatible(
        RelativeCauseKind.ConversionMiss,
        RelativeCauseSourceSide.Reference,
        CauseAttributionKind.ReferenceCreatesResource
      )
    )
    assert(
      RelativeCauseKind.sourceAttributionCompatible(
        RelativeCauseKind.ConversionSecured,
        RelativeCauseSourceSide.Reference,
        CauseAttributionKind.ReferenceCreatesResource
      )
    )
    assert(
      !RelativeCauseKind.sourceAttributionCompatible(
        RelativeCauseKind.ConversionSecured,
        RelativeCauseSourceSide.Reference,
        CauseAttributionKind.CandidateAllowsLiability
      )
    )
    assert(
      RelativeCauseKind.sourceAttributionCompatible(
        RelativeCauseKind.MaterialSwing,
        RelativeCauseSourceSide.Candidate,
        CauseAttributionKind.CandidateAllowsLiability
      )
    )
    assert(
      !RelativeCauseKind.sourceAttributionCompatible(
        RelativeCauseKind.MaterialSwing,
        RelativeCauseSourceSide.Mixed,
        CauseAttributionKind.CandidateAllowsLiability
      )
    )

  test("a ready specific cause dominates a generic fallback only in the same causal channel"):
    val decisions = dominanceDecisions(
      fallbackTarget = "e8",
      specificTarget = "e8",
      specificSourceSide = RelativeCauseSourceSide.Reference
    )

    assertEquals(
      decisions("fallback").status,
      RelativeCauseDominanceStatus.DominatedFallback
    )
    assertEquals(decisions("fallback").dominatingCauseEvidenceIds, List("specific"))
    assert(decisions("specific").retained)

  test("a specific cause on another target cannot suppress an independent fallback"):
    val decisions = dominanceDecisions(
      fallbackTarget = "e8",
      specificTarget = "a8",
      specificSourceSide = RelativeCauseSourceSide.Reference
    )

    assert(decisions("fallback").retained)
    assert(decisions("specific").retained)

  test("source and attribution polarity separate otherwise identical fallback channels"):
    val decisions = dominanceDecisions(
      fallbackTarget = "e8",
      specificTarget = "e8",
      specificSourceSide = RelativeCauseSourceSide.Candidate
    )

    assert(decisions("fallback").retained)
    assert(decisions("specific").retained)

  test("a material outcome fallback yields to an owned tactical mechanism"):
    val decisions = dominanceDecisions(
      fallbackTarget = "e8",
      specificTarget = "e8",
      specificSourceSide = RelativeCauseSourceSide.Candidate,
      fallbackKind = RelativeCauseKind.MaterialSwing,
      fallbackSourceSide = RelativeCauseSourceSide.Candidate
    )

    assertEquals(
      decisions("fallback").status,
      RelativeCauseDominanceStatus.DominatedFallback
    )
    assertEquals(decisions("fallback").dominatingCauseEvidenceIds, List("specific"))

  test("an object-unready specific cause cannot suppress a ready fallback"):
    val decisions = dominanceDecisions(
      fallbackTarget = "e8",
      specificTarget = "e8",
      specificSourceSide = RelativeCauseSourceSide.Reference,
      specificReady = false
    )

    assert(decisions("fallback").retained)
    assert(decisions("specific").retained)

  private def dominanceDecisions(
      fallbackTarget: String,
      specificTarget: String,
      specificSourceSide: RelativeCauseSourceSide,
      fallbackKind: RelativeCauseKind = RelativeCauseKind.MissedTacticalResource,
      fallbackSourceSide: RelativeCauseSourceSide = RelativeCauseSourceSide.Reference,
      specificReady: Boolean = true
  ): Map[String, RelativeCauseDominanceDecision] =
    val position = PositionNodeRef(
      fen = "4k3/8/8/8/8/8/4Q3/4K3 w - - 0 1",
      ply = 0,
      sideToMove = Some(White)
    )
    val referenceLine = LineNodeRef("reference", "e2e7", 1, LineNodeRole.BestReference)
    val candidateLine = LineNodeRef("candidate", "e2e3", 2, LineNodeRole.Played)
    val comparisonRef = EvidenceRef(
      id = "comparison",
      producer = EvidenceProducer.RelativeMoveProducer,
      layer = EvidenceLayer.CandidateComparison,
      position = position,
      line = Some(candidateLine),
      scope = EvidenceScope.Counterfactual,
      confidence = EvidenceConfidence.EngineBacked
    )
    val comparison = CandidateComparisonFact(
      kind = CandidateComparisonKind.PlayedVsBest,
      referenceLine = referenceLine,
      candidateLine = candidateLine,
      comparison = EvalComparison.fromLines(
        mover = White,
        reference = CandidateLineNode(
          referenceLine,
          EngineLine(List(referenceLine.rootMove), scoreCp = 300, depth = 18),
          lineEvidenceRef("reference-line", referenceLine, position)
        ),
        candidate = CandidateLineNode(
          candidateLine,
          EngineLine(List(candidateLine.rootMove), scoreCp = -300, depth = 18),
          lineEvidenceRef("candidate-line", candidateLine, position)
        )
      )
    )
    val fallbackLine =
      if fallbackSourceSide == RelativeCauseSourceSide.Reference then referenceLine else candidateLine
    val specificLine =
      if specificSourceSide == RelativeCauseSourceSide.Reference then referenceLine else candidateLine
    val fallbackProofRef = motifRef("fallback-proof", fallbackLine, position)
    val specificProofRef =
      if specificReady then motifRef("specific-proof", specificLine, position)
      else
        motifRef("specific-proof", specificLine, position).copy(
          producer = EvidenceProducer.TacticalMechanismProducer,
          layer = EvidenceLayer.TacticalMechanism
        )
    val fallbackCause = cause(
      kind = fallbackKind,
      comparisonRef = comparisonRef,
      proofRef = fallbackProofRef,
      sourceSide = fallbackSourceSide
    )
    val specificCause = cause(
      kind = RelativeCauseKind.KingForcing,
      comparisonRef = comparisonRef,
      proofRef = specificProofRef,
      sourceSide = specificSourceSide
    )
    val fallbackRef = causeRef("fallback", fallbackLine, position)
    val specificRef = causeRef(
      "specific",
      specificLine,
      position
    )
    val records = List(
      EvidenceRecord(comparisonRef, CandidateComparisonEvidence(comparison)),
      EvidenceRecord(fallbackProofRef, motifEvidence(fallbackLine.rootMove, fallbackTarget)),
      EvidenceRecord(
        specificProofRef,
        if specificReady then motifEvidence(specificLine.rootMove, specificTarget)
        else
          TacticalMechanismEvidence(
            kind = TacticalMechanismKind.KingForcing,
            moveUci = Some(specificLine.rootMove),
            line = Some(specificLine),
            signals = Nil
          )
      ),
      EvidenceRecord(fallbackRef, RelativeCauseFactEvidence(fallbackCause)),
      EvidenceRecord(specificRef, RelativeCauseFactEvidence(specificCause))
    )
    val graph = records.foldLeft(TypedEvidenceGraph.empty)((current, record) => current.add(record))

    RelativeCauseDominancePolicy
      .resolve(List(fallbackCause -> fallbackRef, specificCause -> specificRef), graph)
      .map(decision => decision.causeEvidenceId -> decision)
      .toMap

  private def motifRef(
      id: String,
      line: LineNodeRef,
      position: PositionNodeRef
  ): EvidenceRef =
    EvidenceRef(
      id = id,
      producer = EvidenceProducer.MoveMotifProducer,
      layer = EvidenceLayer.MoveMotif,
      position = position,
      line = Some(line),
      scope = line.role.scope,
      confidence = EvidenceConfidence.LegalReplayVerified
    )

  private def lineEvidenceRef(
      id: String,
      line: LineNodeRef,
      position: PositionNodeRef
  ): EvidenceRef =
    EvidenceRef(
      id = id,
      producer = EvidenceProducer.LegalLineProducer,
      layer = EvidenceLayer.Line,
      position = position,
      line = Some(line),
      scope = line.role.scope,
      confidence = EvidenceConfidence.LegalReplayVerified
    )

  private def causeRef(
      id: String,
      line: LineNodeRef,
      position: PositionNodeRef
  ): EvidenceRef =
    EvidenceRef(
      id = id,
      producer = EvidenceProducer.RelativeMoveProducer,
      layer = EvidenceLayer.RelativeCause,
      position = position,
      line = Some(line),
      scope = EvidenceScope.Counterfactual,
      confidence = EvidenceConfidence.EngineBacked
    )

  private def motifEvidence(rootMove: String, target: String): MoveMotifEvidence =
    val square = Square.fromKey(target).getOrElse(fail(s"invalid test square '$target'"))
    MoveMotifEvidence(
      MoveMotifEvent.fromMotif(
        rootMove,
        Motif.Check(
          piece = Queen,
          targetSquare = square,
          checkType = Motif.CheckType.Normal,
          color = White,
          plyIndex = 0,
          move = Some(rootMove)
        )
      )
    )

  private def cause(
      kind: RelativeCauseKind,
      comparisonRef: EvidenceRef,
      proofRef: EvidenceRef,
      sourceSide: RelativeCauseSourceSide
  ): RelativeCauseFact =
    val attributionKind =
      if sourceSide == RelativeCauseSourceSide.Reference then CauseAttributionKind.ReferenceCreatesResource
      else CauseAttributionKind.CandidateAllowsLiability
    RelativeCauseFact(
      kind = kind,
      comparisonEvidence = comparisonRef,
      supportEvidence = List(proofRef),
      sourceSide = sourceSide,
      attribution = CauseAttribution(
        kind = attributionKind,
        rootMoveMatched = true,
        directProofEligible = true
      ),
      proof = Some(
        RelativeCauseProof(
          directProof = RelativeCauseProofSection(
            role = RelativeCauseProofRole.DirectProof,
            strength = RelativeCauseProofStrength.Primary,
            sourceRefs = List(proofRef)
          )
        )
      )
    )
