package lila.chessjudgment.model.judgment

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import lila.chessjudgment.analysis.line.PrincipalVariationEvidence
import lila.chessjudgment.model.{ CollapseAnalysis, PlanSequenceSummary, ProbePurpose, ProbeRequest }
import lila.chessjudgment.model.strategic.VariationLine

object PositionNodeBuilder:
  def fromAnalysis(
      role: PositionNodeRole,
      ref: PositionNodeRef
  ): PositionNode =
    PositionNode(
      role = role,
      ref = ref
    )

object CandidateLineNodeBuilder:
  def fromEngineLine(
      ref: LineNodeRef,
      line: VariationLine,
      evidence: EvidenceRef
  ): CandidateLineNode =
    CandidateLineNode(
      ref = ref,
      line = line,
      evidence = evidence
    )

object MoveTransitionEdgeBuilder:
  def fromMove(
      role: TransitionEdgeRole,
      from: PositionNodeRef,
      moveUci: String,
      to: PositionNodeRef,
      planTransition: Option[PlanSequenceSummary],
      evidence: EvidenceRef
  ): MoveTransitionEdge =
    MoveTransitionEdge(
      role = role,
      from = from,
      moveUci = moveUci,
      to = to,
      planTransition = planTransition,
      evidence = evidence
    )

object RelativeMoveAssessmentBuilder:
  def fromComparison(
      played: MoveTransitionEdge,
      referenceTransition: Option[MoveTransitionEdge],
      reference: CandidateLineNode,
      candidate: CandidateLineNode,
      comparison: EvalComparison,
      collapse: Option[CollapseAnalysis],
      confidence: EvidenceConfidence,
      evidence: EvidenceRef,
      counterfactualEvidence: List[EvidenceRef],
      candidateComparisonEvidence: List[EvidenceRef] = Nil,
      relativeCauseEvidence: List[EvidenceRef] = Nil,
      verdictCertificationEvidence: Option[EvidenceRef] = None
  ): RelativeMoveAssessment =
    RelativeMoveAssessment(
      played = played,
      referenceTransition = referenceTransition,
      reference = reference,
      candidate = candidate,
      comparison = comparison,
      collapse = collapse,
      confidence = confidence,
      evidence = evidence,
      counterfactualEvidence = counterfactualEvidence,
      candidateComparisonEvidence = candidateComparisonEvidence,
      relativeCauseEvidence = relativeCauseEvidence,
      verdictCertificationEvidence = verdictCertificationEvidence
    )

object ChessIdeaBuilder:
  def fromEvidence(
      id: String,
      family: ChessIdeaFamily,
      subject: IdeaSubject,
      primaryPosition: PositionNodeRef,
      primaryLine: Option[LineNodeRef],
      moveUci: Option[String],
      evidence: List[EvidenceRef],
      scope: EvidenceScope,
      confidence: EvidenceConfidence
  ): ChessIdea =
    ChessIdea(
      ref = ChessIdeaRef(id, family),
      subject = subject,
      primaryPosition = primaryPosition,
      primaryLine = primaryLine,
      moveUci = moveUci,
      evidence = evidence,
      requiredLayers = evidence.map(_.layer).distinct,
      scope = scope,
      confidence = confidence
    )

  def evidenceRecord(id: String, idea: ChessIdea): EvidenceRecord =
    val ref =
      EvidenceRef(
        id = id,
        producer = EvidenceProducer.ChessIdeaProducer,
        layer = EvidenceLayer.ChessIdea,
        position = idea.primaryPosition,
        line = idea.primaryLine,
        scope = idea.scope,
        confidence = idea.confidence
      )
    EvidenceRecord(
      ref = ref,
      payload = ChessIdeaEvidence(idea.ref),
      parents = idea.evidence
    )

object ClaimComposer:
  def fromIdea(
      id: String,
      family: ClaimFamily,
      idea: ChessIdea,
      engineComparison: Option[EvalComparison],
      confidence: EvidenceConfidence
  ): ClaimSeed =
    ClaimSeed(
      id = id,
      family = family,
      idea = Some(idea.ref),
      subject = idea.subject,
      primaryPosition = idea.primaryPosition,
      primaryLine = idea.primaryLine,
      subjectMove = idea.moveUci,
      evidence = idea.evidence,
      engineComparison = engineComparison,
      scope = idea.scope,
      confidence = confidence,
      relatedIdeas = List(idea.ref)
    )

  def fromEvidence(
      id: String,
      family: ClaimFamily,
      subject: IdeaSubject,
      primaryPosition: PositionNodeRef,
      primaryLine: Option[LineNodeRef],
      subjectMove: Option[String],
      evidence: List[EvidenceRef],
      engineComparison: Option[EvalComparison],
      scope: EvidenceScope,
      confidence: EvidenceConfidence
  ): ClaimSeed =
    ClaimSeed(
      id = id,
      family = family,
      idea = None,
      subject = subject,
      primaryPosition = primaryPosition,
      primaryLine = primaryLine,
      subjectMove = subjectMove,
      evidence = evidence,
      engineComparison = engineComparison,
      scope = scope,
      confidence = confidence
    )

  def evidenceRecord(id: String, claim: ClaimSeed): EvidenceRecord =
    val ref =
      EvidenceRef(
        id = id,
        producer = EvidenceProducer.ClaimComposer,
        layer = EvidenceLayer.Claim,
        position = claim.primaryPosition,
        line = claim.primaryLine,
        scope = claim.scope,
        confidence = claim.confidence
      )
    EvidenceRecord(
      ref = ref,
      payload = ClaimEvidence(claim.id),
      parents = claim.evidence
    )

object JudgmentPacketBuilder:
  def fromAssembly(ctx: JudgmentAssemblyContext): Option[EvidenceBackedJudgmentPacket] =
    ctx.root.map { rootRef =>
      val diagnostics = EvidenceLossDiagnostics.fromAssembly(ctx)
      val claimSupportClusters = ClaimSupportCluster.fromClaims(ctx.claims, ctx.evidenceGraph)
      val ideaVerdict = IdeaVerdictSplit.from(ctx.ideas, ctx.claims, ctx.relativeAssessments)
      val claimEventClusters = ClaimEventCluster.fromClaims(ctx.claims, ctx.evidenceGraph, claimSupportClusters)
      val moveJudgmentView =
        MoveJudgmentView.from(
          relativeAssessments = ctx.relativeAssessments,
          evidenceGraph = ctx.evidenceGraph,
          ideas = ctx.ideas,
          claims = ctx.claims,
          claimLifecycle = ctx.claimLifecycle,
          ideaVerdict = ideaVerdict,
          claimSupportClusters = claimSupportClusters,
          claimEventClusters = claimEventClusters
        )
      val probeRequests = BranchReplyProbePlanner.fromAssembly(ctx)
      EvidenceBackedJudgmentPacket(
        root = rootRef,
        positions = ctx.positions,
        candidateLines = ctx.lines,
        transitions = ctx.transitions,
        relativeAssessments = ctx.relativeAssessments,
        evidenceGraph = ctx.evidenceGraph,
        ideas = ctx.ideas,
        claims = ctx.claims,
        claimLifecycle = ctx.claimLifecycle,
        ideaVerdict = ideaVerdict,
        claimSupportClusters = claimSupportClusters,
        claimEventClusters = claimEventClusters,
        moveJudgmentView = moveJudgmentView,
        diagnostics = diagnostics,
        probeRequests = probeRequests,
        probeDiagnostics = ctx.probeDiagnostics
      )
    }

object BranchReplyProbeBinding:
  val ReplyMultiPv = 3
  val Depth = 16
  val DepthFloor = 12
  val Objective = "branch_reply_multipv"
  val RequiredSignals: List[String] = List("replyLines", "depth", "purpose", "variationHash")

  def variationHash(root: PositionNodeRef, line: CandidateLineNode): String =
    variationHash(
      rootFen = root.fen,
      role = line.ref.role,
      rootMove = line.ref.rootMove,
      whitePovEvalCp = line.whitePovEvalCp,
      mate = line.mate,
      depth = line.depth,
      moves = line.line.moves
    )

  def variationHash(
      rootFen: String,
      role: LineNodeRole,
      rootMove: String,
      whitePovEvalCp: Int,
      mate: Option[Int],
      depth: Int,
      moves: List[String]
  ): String =
    val raw =
      List(
        rootFen,
        role.toString,
        rootMove,
        whitePovEvalCp.toString,
        mate.map(_.toString).getOrElse(""),
        depth.toString,
        moves.mkString(",")
      ).mkString("||")
    MessageDigest
      .getInstance("SHA-256")
      .digest(raw.getBytes(StandardCharsets.UTF_8))
      .map(byte => f"${byte & 0xff}%02x")
      .mkString

object BranchReplyProbePlanner:
  def fromAssembly(ctx: JudgmentAssemblyContext): List[ProbeRequest] =
    ctx.root.toList.flatMap { root =>
      val coveredBranchFens =
        ctx.positions.filter(_.role == PositionNodeRole.AfterThreat).map(_.ref.fen).toSet
      selectedRootLines(ctx.lines).flatMap { line =>
        PrincipalVariationEvidence.legalFenAfter(root.fen, line.ref.rootMove).filterNot(coveredBranchFens.contains).map { branchFen =>
          ProbeRequest(
            id = s"${line.ref.id}:reply-multipv",
            fen = branchFen,
            moves = Nil,
            depth = BranchReplyProbeBinding.Depth,
            purpose = Some(ProbePurpose.ReplyMultipv),
            multiPv = Some(BranchReplyProbeBinding.ReplyMultiPv),
            baselineMove = Some(line.ref.rootMove),
            baselineEvalCp = Some(line.whitePovEvalCp),
            baselineMate = line.mate,
            baselineDepth = Some(line.depth).filter(_ > 0),
            objective = Some(BranchReplyProbeBinding.Objective),
            requiredSignals = BranchReplyProbeBinding.RequiredSignals,
            horizon = Some("short"),
            candidateMove = Some(line.ref.rootMove),
            depthFloor = Some(BranchReplyProbeBinding.DepthFloor),
            variationHash = Some(BranchReplyProbeBinding.variationHash(root, line))
          )
        }
      }
    }.distinctBy(_.id)

  private def selectedRootLines(lines: List[CandidateLineNode]): List[CandidateLineNode] =
    val rootLines = lines.filterNot(_.role == LineNodeRole.Threat)
    val primary =
      List(LineNodeRole.Played, LineNodeRole.BestReference).flatMap(role => rootLines.find(_.role == role))
    val alternatives =
      rootLines.filter(_.role == LineNodeRole.Alternative).sortBy(_.ref.rank).take(1)
    (primary ++ alternatives)
      .filter(_.line.moves.nonEmpty)
      .distinctBy(_.ref.rootMove)
      .take(BranchReplyProbeBinding.ReplyMultiPv)
