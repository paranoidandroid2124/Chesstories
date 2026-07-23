package lila.chessjudgment.model.judgment

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import lila.chessjudgment.analysis.line.PrincipalVariationEvidence
import lila.chessjudgment.model.{ ProbeAdmissionStatus, ProbeContractValidator, ProbeHorizon, ProbePurpose, ProbeRequest, ProbeResult }

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
      val probeRequests =
        (
          BranchReplyProbePlanner.fromAssembly(ctx) ++
            CounterResourceProbePlanner.fromAssembly(ctx) ++
            EndgameTablebaseProbePlanner.fromAssembly(ctx)
        ).distinctBy(_.id)
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
  val RequiredSignals: List[String] = List("replyLines", "depth", "purpose", "variationHash", "horizon")

  def requiredReplyCount(branchFen: String): Int =
    _root_.chess.format.Fen
      .read(_root_.chess.variant.Standard, _root_.chess.format.Fen.Full(branchFen))
      .map(_.legalMoves.size.min(ReplyMultiPv))
      .getOrElse(ReplyMultiPv)

  def horizon(requiredPlyOffset: Int): String = ProbeHorizon.renderPlyOffset(requiredPlyOffset)

  def horizonPlyOffset(value: String): Option[Int] =
    ProbeHorizon.plyOffset(value)

  def variationHash(
      root: PositionNodeRef,
      line: CandidateLineNode,
      certifiedHorizonPlyOffset: Int
  ): String =
    variationHash(
      rootFen = root.fen,
      role = line.ref.role,
      rootMove = line.ref.rootMove,
      whitePovEvalCp = line.whitePovEvalCp,
      mate = line.mate,
      depth = line.depth,
      moves = line.line.moves,
      certifiedHorizonPlyOffset = certifiedHorizonPlyOffset
    )

  def variationHash(
      rootFen: String,
      role: LineNodeRole,
      rootMove: String,
      whitePovEvalCp: Int,
      mate: Option[Int],
      depth: Int,
      moves: List[String],
      certifiedHorizonPlyOffset: Int
  ): String =
    digest(variationFields(rootFen, role, rootMove, whitePovEvalCp, mate, depth, moves) :+
      horizon(certifiedHorizonPlyOffset))

  private[judgment] def variationBaseHash(
      rootFen: String,
      role: LineNodeRole,
      rootMove: String,
      whitePovEvalCp: Int,
      mate: Option[Int],
      depth: Int,
      moves: List[String]
  ): String =
    digest(variationFields(rootFen, role, rootMove, whitePovEvalCp, mate, depth, moves))

  private def variationFields(
      rootFen: String,
      role: LineNodeRole,
      rootMove: String,
      whitePovEvalCp: Int,
      mate: Option[Int],
      depth: Int,
      moves: List[String]
  ): List[String] =
    List(
      rootFen,
      role.toString,
      rootMove,
      whitePovEvalCp.toString,
      mate.map(_.toString).getOrElse(""),
      depth.toString,
      moves.mkString(",")
    )

  private def digest(fields: List[String]): String =
    val raw = fields.mkString("||")
    MessageDigest
      .getInstance("SHA-256")
      .digest(raw.getBytes(StandardCharsets.UTF_8))
      .map(byte => f"${byte & 0xff}%02x")
      .mkString

object EndgameTablebaseProbeBinding:
  val Objective = "endgame_tablebase"
  val RequiredSignals: List[String] = List("tablebase", "purpose", "variationHash")

  def requestFor(
      line: CandidateLineNode,
      lineFacts: LineFactEvidence,
      horizon: LineEndgameTechniqueHorizon
  ): Option[ProbeRequest] =
    val replay = lineFacts.lineReplaySteps
    for
      trigger <- horizon.triggerMove.map(EvidenceRef.normalizeMove)
      if horizon.pattern == "Triangulation"
      if horizon.status == LineEndgameTechniqueHorizonStatus.Completed
      if horizon.zugzwangProof.isEmpty
      if horizon.entryPlyOffset == 0 && horizon.terminalPlyOffset == 4
      if EvidenceRef.sameMove(trigger, line.ref.rootMove)
      entry <- replay.lift(horizon.entryPlyOffset)
      terminal <- replay.lift(horizon.terminalPlyOffset)
      comparisonPosition <- position(entry.fenBefore)
      terminalPosition <- position(terminal.fenAfter)
      if comparisonPosition.color == horizon.techniqueSide
      if terminalPosition.color == !horizon.techniqueSide
      if sameBoardState(entry.fenBefore, terminal.fenAfter)
      if terminalPosition.board.occupied.count <= 7
      legalMoves = terminalPosition.legalMoves.map(_.toUci.uci).distinct.sorted
      if legalMoves.nonEmpty
      bindingHash = positionBindingHash(terminal.fenAfter, entry.fenBefore, legalMoves)
    yield ProbeRequest(
      id = s"endgame-tablebase:${bindingHash.take(24)}",
      fen = terminal.fenAfter,
      moves = legalMoves,
      depth = 0,
      purpose = Some(ProbePurpose.EndgameTablebase),
      objective = Some(Objective),
      requiredSignals = RequiredSignals,
      variationHash = Some(bindingHash),
      comparisonFen = Some(entry.fenBefore)
    )

  def admit(
      line: CandidateLineNode,
      lineFacts: LineFactEvidence,
      results: List[ProbeResult]
  ): LineFactEvidence =
    lineFacts.endgameTechniqueHorizons.foldLeft(lineFacts) { (current, horizon) =>
      val proofs = results.flatMap(exactProof(line, lineFacts, horizon, _)).distinct
      proofs match
        case proof :: Nil =>
          current.withEndgameZugzwangProof(horizon.entryPlyOffset, horizon.terminalPlyOffset, proof)
        case _ =>
          current
    }

  private def exactProof(
      line: CandidateLineNode,
      lineFacts: LineFactEvidence,
      horizon: LineEndgameTechniqueHorizon,
      result: ProbeResult
  ): Option[EndgameZugzwangProof] =
    for
      request <- requestFor(line, lineFacts, horizon)
      if result.id == request.id
      if result.purpose.contains(ProbePurpose.EndgameTablebase)
      if result.variationHash == request.variationHash
      if ProbeContractValidator.validateAgainstRequest(request, result).isValid
      tablebase <- result.tablebase
      comparisonFen <- request.comparisonFen
      if normalizeFen(tablebase.terminal.fen) == normalizeFen(request.fen)
      if normalizeFen(tablebase.comparison.fen) == normalizeFen(comparisonFen)
      terminalPosition <- position(request.fen)
      comparisonPosition <- position(comparisonFen)
      if terminalPosition.color == !horizon.techniqueSide
      if comparisonPosition.color == horizon.techniqueSide
      if tablebase.terminal.wdl == -2 && tablebase.comparison.wdl == 2
      terminalDtm <- tablebase.terminal.dtm.filter(_ < 0)
      comparisonDtm <- tablebase.comparison.dtm.filter(_ > 0)
      replies = tablebase.legalMoves.map(reply => reply.copy(moveUci = EvidenceRef.normalizeMove(reply.moveUci)))
      expectedMoves = request.moves.map(EvidenceRef.normalizeMove).distinct.sorted
      if replies.nonEmpty && replies.map(_.moveUci).distinct.size == replies.size
      if replies.map(_.moveUci).sorted == expectedMoves
      if replies.forall(_.resultingWdl == 2)
      replyDtms <- Option.when(replies.forall(_.resultingDtm.exists(_ > 0)))(replies.flatMap(_.resultingDtm))
      maxReplyDtm <- replyDtms.maxOption
      if terminalDtm.toLong.abs == maxReplyDtm.toLong + 1L
      if comparisonDtm > maxReplyDtm
    yield EndgameZugzwangProof(
      constrainedSide = terminalPosition.color,
      terminalFen = request.fen,
      comparisonFen = comparisonFen,
      terminalDtm = terminalDtm,
      comparisonDtm = comparisonDtm,
      legalReplies = replies.zip(replyDtms).map((reply, dtm) => EndgameZugzwangReplyProof(reply.moveUci, dtm)).sortBy(_.moveUci)
    )

  private def positionBindingHash(terminalFen: String, comparisonFen: String, legalMoves: List[String]): String =
    val raw = List(normalizeFen(terminalFen), normalizeFen(comparisonFen), legalMoves.sorted.mkString(",")).mkString("||")
    MessageDigest
      .getInstance("SHA-256")
      .digest(raw.getBytes(StandardCharsets.UTF_8))
      .map(byte => f"${byte & 0xff}%02x")
      .mkString

  private def position(fen: String): Option[_root_.chess.Position] =
    _root_.chess.format.Fen.read(_root_.chess.variant.Standard, _root_.chess.format.Fen.Full(fen))

  private def sameBoardState(left: String, right: String): Boolean =
    def state(fen: String): List[String] =
      normalizeFen(fen).split("\\s+").toList.zipWithIndex.collect { case (field, index) if index == 0 || index == 2 || index == 3 => field }
    state(left) == state(right)

  private def normalizeFen(fen: String): String =
    Option(fen).getOrElse("").trim.split("\\s+").filter(_.nonEmpty).mkString(" ")

object CounterResourceProbeBinding:
  val Objective = "opponent_resource_reply"
  val RequiredSignals: List[String] =
    List("replyLines", "depth", "purpose", "variationHash", "opponentResourceMove")

  def variationHash(root: PositionNodeRef, line: CandidateLineNode, opponentResourceMove: String): String =
    variationHash(
      rootFen = root.fen,
      role = line.ref.role,
      rootMove = line.ref.rootMove,
      whitePovEvalCp = line.whitePovEvalCp,
      mate = line.mate,
      depth = line.depth,
      moves = line.line.moves,
      opponentResourceMove = opponentResourceMove
    )

  def variationHash(
      rootFen: String,
      role: LineNodeRole,
      rootMove: String,
      whitePovEvalCp: Int,
      mate: Option[Int],
      depth: Int,
      moves: List[String],
      opponentResourceMove: String
  ): String =
    val raw =
      List(
        BranchReplyProbeBinding.variationBaseHash(
          rootFen,
          role,
          rootMove,
          whitePovEvalCp,
          mate,
          depth,
          moves
        ),
        EvidenceRef.normalizeMove(opponentResourceMove),
        Objective
      ).mkString("||")
    MessageDigest
      .getInstance("SHA-256")
      .digest(raw.getBytes(StandardCharsets.UTF_8))
      .map(byte => f"${byte & 0xff}%02x")
      .mkString

object EndgameTablebaseProbePlanner:
  def fromAssembly(ctx: JudgmentAssemblyContext): List[ProbeRequest] =
    ctx.root.toList.flatMap { root =>
      ctx.evidenceGraph.records.flatMap {
        case EvidenceRecord(ref, lineFacts: LineFactEvidence, _) if ref.position == root && ref.line.exists(_.role != LineNodeRole.Threat) =>
          ref.line.toList.flatMap(lineRef => ctx.lines.find(_.ref == lineRef)).flatMap { line =>
            lineFacts.endgameTechniqueHorizons.flatMap(EndgameTablebaseProbeBinding.requestFor(line, lineFacts, _))
          }
        case _ =>
          Nil
      }
    }.distinctBy(_.id)

object BranchReplyProbePlanner:
  private val DiscoveryHorizonPly = 6
  private val DirectRootEventKinds = Set(
    LineEventKind.Capture,
    LineEventKind.Recapture,
    LineEventKind.Check,
    LineEventKind.Mate,
    LineEventKind.Castling,
    LineEventKind.Stalemate,
    LineEventKind.Promotion
  )

  def fromAssembly(ctx: JudgmentAssemblyContext): List[ProbeRequest] =
    ctx.root.toList.flatMap { root =>
      val unresolvedCausalHorizons =
        ctx.evidenceGraph.records.collect {
          case EvidenceRecord(_, event: PlanCausalEventEvidence, _)
              if event.counterfactualContinuationProven &&
                event.causalResultAssessments.nonEmpty &&
                !event.branchCoverageComplete =>
            EvidenceRef.normalizeMove(event.rootMove) -> event.requiredHorizonPlyOffset
        }.groupMapReduce(_._1)(_._2)(_.max(_))
      val discoveryHorizons = discoveryCausalHorizons(ctx)
      val requestedCausalHorizons =
        (unresolvedCausalHorizons.toList ++ discoveryHorizons.toList)
          .groupMapReduce(_._1)(_._2)(_.max(_))
      val discoveryOnlyMoves = discoveryHorizons.keySet -- unresolvedCausalHorizons.keySet
      val admittedProbeIds =
        ctx.probeDiagnostics.collect {
          case diagnostic if diagnostic.status == ProbeAdmissionStatus.Admitted => diagnostic.probeId
        }.toSet
      selectedRootLines(ctx.lines).filter(line =>
        requestedCausalHorizons.contains(EvidenceRef.normalizeMove(line.ref.rootMove))
      ).flatMap { line =>
        PrincipalVariationEvidence.legalFenAfter(root.fen, line.ref.rootMove).toList.flatMap { branchFen =>
          val requiredReplies = BranchReplyProbeBinding.requiredReplyCount(branchFen)
          val requiredHorizon = requestedCausalHorizons(EvidenceRef.normalizeMove(line.ref.rootMove)).max(1)
          Option.when(requiredReplies > 0)(
            ProbeRequest(
              id = s"${line.ref.id}:reply-multipv:ply-$requiredHorizon",
              fen = branchFen,
              moves = Nil,
              depth = BranchReplyProbeBinding.Depth,
              purpose = Some(ProbePurpose.ReplyMultipv),
              multiPv = Some(requiredReplies),
              baselineMove = Some(line.ref.rootMove),
              baselineEvalCp = Some(line.whitePovEvalCp),
              baselineMate = line.mate,
              baselineDepth = Some(line.depth).filter(_ > 0),
              objective = Some(BranchReplyProbeBinding.Objective),
              requiredSignals = BranchReplyProbeBinding.RequiredSignals,
              horizon = Some(BranchReplyProbeBinding.horizon(requiredHorizon)),
              candidateMove = Some(line.ref.rootMove),
              depthFloor = Some(BranchReplyProbeBinding.DepthFloor),
              variationHash = Some(BranchReplyProbeBinding.variationHash(root, line, requiredHorizon))
            )
          )
        }
      }.filterNot(request =>
        admittedProbeIds(request.id) &&
          request.candidateMove.exists(move => discoveryOnlyMoves(EvidenceRef.normalizeMove(move)))
      )
    }.distinctBy(_.id)

  private def discoveryCausalHorizons(ctx: JudgmentAssemblyContext): Map[String, Int] =
    ctx.lines
      .find(_.role == LineNodeRole.Played)
      .filter(needsPlanDiscovery(ctx, _))
      .map(line => EvidenceRef.normalizeMove(line.ref.rootMove) -> DiscoveryHorizonPly)
      .toMap

  private def needsPlanDiscovery(ctx: JudgmentAssemblyContext, line: CandidateLineNode): Boolean =
    val rootMove = line.ref.rootMove
    val hasPlanPressure =
      ctx.evidenceGraph.records.exists {
        case EvidenceRecord(ref, _: PlanPressureEvidence, _) => ref.line.contains(line.ref)
        case _                                               => false
      }
    val hasPublicPlanProof =
      ctx.evidenceGraph.records.exists {
        case EvidenceRecord(ref, event: PlanCausalEventEvidence, _)
            if ref.confidence != EvidenceConfidence.Heuristic &&
              EvidenceRef.sameMove(event.rootMove, rootMove) =>
          event.publicGoalProofReady
        case _ =>
          false
      }
    val hasConcreteRootEvent =
      ctx.evidenceGraph.records.exists {
        case EvidenceRecord(ref, lineFacts: LineFactEvidence, _) if ref.line.contains(line.ref) =>
          lineFacts.lineEvents.exists(event =>
            event.plyOffset == 0 &&
              EvidenceRef.sameMove(event.moveUci, rootMove) &&
              DirectRootEventKinds(event.kind)
          )
        case _ => false
      }
    hasPlanPressure && !hasPublicPlanProof && !hasConcreteRootEvent

  private[judgment] def selectedRootLines(lines: List[CandidateLineNode]): List[CandidateLineNode] =
    val rootLines = lines.filterNot(_.role == LineNodeRole.Threat)
    val primary =
      List(LineNodeRole.Played, LineNodeRole.BestReference).flatMap(role => rootLines.find(_.role == role))
    val alternatives =
      rootLines.filter(_.role == LineNodeRole.Alternative).sortBy(_.ref.rank).take(1)
    (primary ++ alternatives)
      .filter(_.line.moves.nonEmpty)
      .distinctBy(_.ref.rootMove)
      .take(BranchReplyProbeBinding.ReplyMultiPv)

object CounterResourceProbePlanner:
  private val MaxResources = 3

  private final case class Candidate(
      moveUci: String,
      directBreak: Boolean,
      preparesBreak: Boolean,
      touchedByRootActor: Boolean
  ):
    def relevant: Boolean = directBreak || preparesBreak || touchedByRootActor
    def priority: Int =
      if directBreak then 0
      else if preparesBreak then 1
      else 2

  def fromAssembly(ctx: JudgmentAssemblyContext): List[ProbeRequest] =
    ctx.root.toList.flatMap { root =>
      val breakFiles = counterBreakFiles(ctx, root)
      val rootLines = BranchReplyProbePlanner.selectedRootLines(ctx.lines).take(2)
      val candidatesByLine = rootLines.map(line => line -> candidatesAfter(root, line, breakFiles))
      val shared = candidatesByLine
        .flatMap { case (line, candidates) => candidates.map(candidate => candidate.moveUci -> (line -> candidate)) }
        .groupBy(_._1)
        .toList
        .map { case (move, entries) =>
          val lineCandidates = entries.map(_._2).distinctBy(_._1.ref.rootMove)
          move -> lineCandidates
        }
        .filter { case (_, lineCandidates) => lineCandidates.size >= 2 && lineCandidates.exists(_._2.relevant) }
        .sortBy { case (move, lineCandidates) =>
          (lineCandidates.map(_._2.priority).min, move)
        }
        .take(MaxResources)
      val admittedIds = ctx.probeDiagnostics.collect {
        case diagnostic if diagnostic.status == ProbeAdmissionStatus.Admitted => diagnostic.probeId
      }.toSet
      shared.flatMap { case (resourceMove, lineCandidates) =>
        lineCandidates.map { case (line, _) =>
          val branchFen = PrincipalVariationEvidence.legalFenAfter(root.fen, line.ref.rootMove).get
          val bindingHash = CounterResourceProbeBinding.variationHash(root, line, resourceMove)
          ProbeRequest(
            id = s"${line.ref.id}:opponent-resource:${resourceMove}:${bindingHash.take(16)}",
            fen = branchFen,
            moves = List(resourceMove),
            depth = BranchReplyProbeBinding.Depth,
            purpose = Some(ProbePurpose.ReplyMultipv),
            multiPv = Some(1),
            baselineMove = Some(line.ref.rootMove),
            baselineEvalCp = Some(line.whitePovEvalCp),
            baselineMate = line.mate,
            baselineDepth = Some(line.depth).filter(_ > 0),
            objective = Some(CounterResourceProbeBinding.Objective),
            requiredSignals = CounterResourceProbeBinding.RequiredSignals,
            candidateMove = Some(line.ref.rootMove),
            opponentResourceMove = Some(resourceMove),
            depthFloor = Some(BranchReplyProbeBinding.DepthFloor),
            variationHash = Some(bindingHash)
          )
        }
      }.filterNot(request => admittedIds(request.id))
    }.distinctBy(_.id)

  private def counterBreakFiles(ctx: JudgmentAssemblyContext, root: PositionNodeRef): Set[String] =
    ctx.evidenceGraph.records.collectFirst {
      case EvidenceRecord(ref, payload: PawnStructureFactEvidence, _)
          if ref.position == root && ref.line.isEmpty =>
        payload.pawnPlay.toList.flatMap(_.counterBreakFiles)
    }.getOrElse(Nil).map(_.trim.toLowerCase).filter(_.matches("[a-h]")).toSet

  private def candidatesAfter(
      root: PositionNodeRef,
      line: CandidateLineNode,
      breakFiles: Set[String]
  ): List[Candidate] =
    for
      branchFen <- PrincipalVariationEvidence.legalFenAfter(root.fen, line.ref.rootMove).toList
      position <- _root_.chess.format.Fen
        .read(_root_.chess.variant.Standard, _root_.chess.format.Fen.Full(branchFen))
        .toList
      directTargets = position.board.byPiece(position.color, _root_.chess.Pawn).squares.flatMap { square =>
        val file = square.file.char.toString.toLowerCase
        val nextRank = square.rank.value + (if position.color.white then 1 else -1)
        Option
          .when(breakFiles(file))(_root_.chess.Square.at(square.file.value, nextRank))
          .flatten
          .filter(position.board.pieceAt(_).isEmpty)
      }.toSet
      move <- position.legalMoves.toList
      if move.piece.role == _root_.chess.Pawn && !move.captures && move.orig.file == move.dest.file
      file = move.orig.file.char.toString.toLowerCase
      direct = breakFiles(file)
      supportsBreak = directTargets.exists(target => move.dest.pawnAttacks(position.color).squares.contains(target))
      rootDestination = _root_.chess.Square.fromKey(EvidenceRef.normalizeMove(line.ref.rootMove).slice(2, 4))
      rootSide = !position.color
      touchedByRootActor = rootDestination.exists(destination =>
        position.board.attackers(move.orig, rootSide).squares.contains(destination) ||
          position.board.attackers(move.dest, rootSide).squares.contains(destination)
      )
    yield Candidate(
      moveUci = EvidenceRef.normalizeMove(move.toUci.uci),
      directBreak = direct,
      preparesBreak = supportsBreak,
      touchedByRootActor = touchedByRootActor
    )
