package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.{ OccurrenceResources, PositionResourceView }
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.{ AutomaticTerminal, CandidateLineEvaluation }
import play.api.libs.json.*

/** Observed events and material changes within this exact path, never a forced outcome. */
private[runtime] object ObservedLineProtocol:
  def evaluationSourceId(source: EvaluationSource): String = source match
    case EvaluationSource.DirectInput => "direct_input"
    case EvaluationSource.RootSearch => "root_search"
    case EvaluationSource.FocusComparison => "focus_comparison"
    case EvaluationSource.InvestigationComparison => "investigation_comparison"
    case EvaluationSource.ResponseComparison => "response_comparison"

  def evaluationJson(evaluation: CandidateLineEvaluation, source: EvaluationSource): JsObject =
    Json.obj("source" -> evaluationSourceId(source)) ++ (evaluation match
      case CandidateLineEvaluation.EngineSearch(line) =>
        Json.obj("kind" -> "engine_search", "depth" -> line.depth,
          "white_score" -> line.mate.fold(Json.obj("kind" -> "cp", "value" -> line.scoreCp))(
            mate => Json.obj("kind" -> "mate", "value" -> mate)))
      case CandidateLineEvaluation.ExactAutomaticTerminal(_, terminal) =>
        Json.obj("kind" -> "exact_automatic_terminal", "terminal" -> terminalJson(terminal)))

  def terminalJson(terminal: AutomaticTerminal): JsObject = terminal match
    case AutomaticTerminal.Checkmate(winner) =>
      Json.obj("kind" -> "checkmate", "winner" -> (if winner.white then "white" else "black"))
    case AutomaticTerminal.Stalemate => Json.obj("kind" -> "stalemate")
    case AutomaticTerminal.InsufficientMaterial => Json.obj("kind" -> "insufficient_material")
    case AutomaticTerminal.FivefoldRepetition => Json.obj("kind" -> "fivefold_repetition")
    case AutomaticTerminal.SeventyFiveMoveRule => Json.obj("kind" -> "seventy_five_move_rule")

  final case class PreparedPath private[ObservedLineProtocol] (
      line: LegalLineNode, declared: List[String], ids: List[String], moves: List[String],
      sourceMoves: List[String], positions: List[PositionNodeRef],
      certified: Option[(EvidenceRecord, LineFactEvidence)]
  )

  /** One immutable packet's response-local paths; never a cross-job or FEN cache. */
  final class PreparedPaths private[ObservedLineProtocol] (
      private val owner: EvidenceBackedJudgmentPacket,
      val eligible: Set[String], val resourceIds: Set[String], val paths: List[PreparedPath]
  ):
    def belongsTo(packet: EvidenceBackedJudgmentPacket): Boolean = owner eq packet
    def requireOwner(packet: EvidenceBackedJudgmentPacket): Unit =
      require(belongsTo(packet), "prepared paths must belong to this exact packet")
    def resourceId(position: PositionNodeRef): Option[String] = position.id.filter(resourceIds)

    private lazy val byLineId = paths.groupBy(_.line.ref.id)
    private lazy val byFullMoves = paths.groupBy(_.sourceMoves)
    /** Discovery witnesses remain available for facts; only independent observations seed older investigations. */
    lazy val investigationPaths: List[PreparedPath] =
      val qualified = owner.assembly.input.legalLines.filterNot(_.discoveryOnly).map(_.moves).toSet
      paths.filter(path => qualified(path.sourceMoves))
    private lazy val views = owner.resources.flatMap(view => view.position.id.map(_ -> view)).toMap
    private lazy val rootView = owner.assembly.playedTransition.flatMap(root =>
      root.from.id.flatMap(views.get).filter(_.position == root.from))
    private lazy val rootPieces = rootView.toList.flatMap(view =>
      chess.Square.all.flatMap(square => view.occupantAt(EvidenceSquare(square.key))))
    private lazy val rootTargets = rootView.toList.flatMap(_.attackedTargets(
      owner.assembly.input.positionHistory.currentPosition.color))
    private val pieceTrackings = scala.collection.mutable.Map.empty[String, Option[OccurrenceResources.PieceTracking]]
    private final class IdentitySnapshot(value: PreparedPath, afterPlyCount: Int):
      private val tracking = pieceTrackings.synchronized {
        pieceTrackings.getOrElseUpdate(value.line.ref.id,
          owner.assembly.lineReplays.get(value.line.ref).map(OccurrenceResources.pieceTracking))
      }
      private val pieces = scala.collection.mutable.Map.empty[RelationColoredPieceWitness,
        Option[Option[RelationColoredPieceWitness]]]
      def piece(original: RelationColoredPieceWitness): Option[Option[RelationColoredPieceWitness]] = pieces.synchronized {
        pieces.getOrElseUpdate(original, tracking.flatMap(_.piece(original, afterPlyCount)))
      }
      lazy val all: List[(RelationColoredPieceWitness, Option[Option[RelationColoredPieceWitness]])] =
        rootPieces.map(original => original -> piece(original))

    // Successor position IDs include their historical predecessor and move.
    // Equal FENs reached by a different route therefore never share a snapshot.
    private val identitySnapshots = scala.collection.mutable.Map.empty[
      Either[(String, Int), PositionNodeRef], IdentitySnapshot]
    private val supportPoints = scala.collection.mutable.Map.empty[
      IdentitySnapshot, LineContinuationInvestigation.SupportPoint]
    private val continuationByLine = scala.collection.mutable.Map.empty[String, Option[LineContinuationInvestigation.Reading]]
    private val continuationComparisons = scala.collection.mutable.Map.empty[
      (String, String), Option[LineContinuationComparison.Reading]]
    private val factResults = scala.collection.mutable.Map.empty[String,
      (CommentaryFactReader.Inputs, Map[String, CommentaryFact.Result])]
    private[runtime] def factReading(packet: EvidenceBackedJudgmentPacket, entryId: String,
        inputs: CommentaryFactReader.Inputs)(read: => Map[String, CommentaryFact.Result])
        : Map[String, CommentaryFact.Result] =
      requireOwner(packet)
      factResults.synchronized {
        factResults.get(entryId).filter(_._1 == inputs).map(_._2).getOrElse {
          val results = read
          factResults.update(entryId, inputs -> results)
          results
        }
      }
    lazy val continuationReadings: List[LineContinuationInvestigation.Reading] =
      LineContinuationInvestigation.readAll(owner, this)

    def continuationComparison(packet: EvidenceBackedJudgmentPacket,
        left: LineContinuationInvestigation.Reading, right: LineContinuationInvestigation.Reading)
        : Option[LineContinuationComparison.Reading] =
      requireOwner(packet)
      if !left.belongsTo(packet) || !right.belongsTo(packet) ||
          !path(packet, left.path.line.ref.id).exists(_ eq left.path) ||
          !path(packet, right.path.line.ref.id).exists(_ eq right.path) then None
      else continuationComparisons.synchronized {
        continuationComparisons.getOrElseUpdate(left.id -> right.id,
          LineContinuationComparison.compare(packet, left, right, this))
      }

    private[runtime] def continuationReading(packet: EvidenceBackedJudgmentPacket, value: PreparedPath)
        (read: => Option[LineContinuationInvestigation.Reading]): Option[LineContinuationInvestigation.Reading] =
      requireOwner(packet)
      require(path(packet, value.line.ref.id).exists(_ eq value), "continuation reading needs this owner's exact path")
      continuationByLine.synchronized { continuationByLine.getOrElseUpdate(value.line.ref.id, read) }

    def resourceViews(packet: EvidenceBackedJudgmentPacket): Map[String, PositionResourceView] =
      requireOwner(packet)
      views

    def attackedRootTargets(packet: EvidenceBackedJudgmentPacket)
        : List[(RelationColoredPieceWitness, List[RelationFactEvidence])] =
      requireOwner(packet)
      rootTargets

    private def identitySnapshot(packet: EvidenceBackedJudgmentPacket, value: PreparedPath, afterPlyCount: Int): IdentitySnapshot =
      requireOwner(packet)
      require(afterPlyCount >= 0, "identity snapshots use a nonnegative root-relative ply")
      require(path(packet, value.line.ref.id).exists(_ eq value), "identity snapshots need this owner's exact path")
      val key: Either[(String, Int), PositionNodeRef] = value.positions.lift(afterPlyCount).filter(_.id.nonEmpty)
        .map(Right(_)).getOrElse(Left(value.line.ref.id -> afterPlyCount))
      identitySnapshots.synchronized {
        identitySnapshots.getOrElseUpdate(key, new IdentitySnapshot(value, afterPlyCount))
      }

    /** A single target lookup does not force tracking every original piece at that ply. */
    def pieceAfter(packet: EvidenceBackedJudgmentPacket, value: PreparedPath,
        original: RelationColoredPieceWitness, afterPlyCount: Int): Option[Option[RelationColoredPieceWitness]] =
      identitySnapshot(packet, value, afterPlyCount).piece(original)

    /** Only requested snapshots are read. Nested None still distinguishes unknown from captured. */
    def identitiesAfter(packet: EvidenceBackedJudgmentPacket, value: PreparedPath, afterPlyCount: Int)
        : List[(RelationColoredPieceWitness, Option[Option[RelationColoredPieceWitness]])] =
      identitySnapshot(packet, value, afterPlyCount).all

    private[runtime] def supportPoint(packet: EvidenceBackedJudgmentPacket, value: PreparedPath, count: Int)
        : LineContinuationInvestigation.SupportPoint =
      requireOwner(packet)
      require(path(packet, value.line.ref.id).exists(_ eq value), "support needs this owner's exact path")
      require(count >= 0 && count <= value.moves.size, "support must refer to this exact observed path")
      val position = value.positions(count)
      val identities = identitySnapshot(packet, value, count)
      supportPoints.synchronized {
        supportPoints.getOrElseUpdate(identities, new LineContinuationInvestigation.SupportPoint(
          position.id.flatMap(views.get), identities.all.toMap))
      }

    /** Support needs the exact position and physical identities, independently of a material reading. */
    def supportAt(packet: EvidenceBackedJudgmentPacket, value: PreparedPath, count: Int,
        originalActor: RelationColoredPieceWitness, target: RelationColoredPieceWitness)
        : LineContinuationInvestigation.Support =
      supportPoint(packet, value, count).support(originalActor, target)

    def path(packet: EvidenceBackedJudgmentPacket, lineId: String): Option[PreparedPath] =
      requireOwner(packet)
      byLineId.get(lineId).collect { case path :: Nil => path }

    /** A complete, certified occurrence path, not a matching FEN or an admitted prefix. */
    def certifiedPath(packet: EvidenceBackedJudgmentPacket, fullMoves: List[String]): Option[PreparedPath] =
      requireOwner(packet)
      byFullMoves.get(fullMoves.map(EvidenceRef.normalizeMove)).collect {
        case path :: Nil if path.certified.nonEmpty && path.ids == path.declared => path
      }

  def prepare(packet: EvidenceBackedJudgmentPacket): PreparedPaths =
    val ctx = packet.assembly
    val graph = packet.evidenceGraph
    val edges = ctx.transitions.map(edge => edge.evidence.id -> edge).toMap
    val eligible = edges.collect { case (id, edge) if graph.record(edge.evidence).exists(record =>
      edge.matches(record) && graph.proofEligible(record)) => id }.toSet
    val paths = ctx.legalLines.flatMap { line =>
      val declared = ctx.lineTransitionIds.getOrElse(line.ref, Nil)
      val ids = declared.takeWhile(eligible)
      Option.when(ids.nonEmpty) {
        PreparedPath(line, declared, ids, ids.map(id => edges(id).moveUci),
          declared.flatMap(edges.get).map(edge => EvidenceRef.normalizeMove(edge.moveUci)),
          edges(ids.head).from :: ids.map(id => edges(id).to), certifiedFact(ctx, line))
      }
    }
    new PreparedPaths(packet, eligible, packet.resources.flatMap(_.position.id).toSet, paths)

  def summaryJson(path: PreparedPath): Option[JsObject] =
    path.certified.map { (record, fact) =>
      renderSummary(path.line.evidence.position.sideToMove.get, path.declared, record, fact)
    }

  def summaryJson(ctx: JudgmentAssemblyContext, line: LegalLineNode): Option[JsObject] =
    for
      (record, fact) <- certifiedFact(ctx, line)
      perspective <- line.evidence.position.sideToMove
      ids <- ctx.lineTransitionIds.get(line.ref)
    yield renderSummary(perspective, ids, record, fact)

  private def renderSummary(perspective: chess.Color, ids: List[String],
      record: EvidenceRecord, fact: LineFactEvidence): JsObject =
      val occurrenceIds = ids.toVector
      def occurrence(plyOffset: Int, move: String): JsObject = Json.obj(
        "ply_offset" -> plyOffset, "occurrence_id" -> occurrenceIds(plyOffset), "move_uci" -> move)
      val events = fact.lineEvents.sortBy(event => (event.plyOffset, event.kind.ordinal)).map { event =>
        occurrence(event.plyOffset, event.moveUci) ++ Json.obj("kind" -> eventKind(event.kind)) ++
          JsObject(Json.obj(
            "side" -> event.side.map(sideName), "piece_role" -> event.pieceRole.map(_.name),
            "target_role" -> event.targetRole.map(_.name), "square" -> event.square.map(_.key)
          ).fields.filterNot(_._2 == JsNull))
      }
      val terminal = fact.lineEvents.find(event => event.plyOffset == ids.size - 1 &&
        Set(LineEventKind.Mate, LineEventKind.Stalemate)(event.kind))
      val base = Json.obj(
        "scope" -> "observed_line", "perspective" -> sideName(perspective),
        "premise_ids" -> List(record.ref.id), "events" -> events,
        "ending" -> Json.obj(
          "kind" -> terminal.fold("line_end")(event => if event.kind == LineEventKind.Mate then "checkmate" else "stalemate"),
          "ply_offset" -> (ids.size - 1), "occurrence_id" -> ids.last))
      fact.material.fold(base) { summary =>
        val materialEvents = summary.events.zip(summary.signedDeltasForMover)
          .zip(summary.runningBalancesForMover).map { case ((event, delta), balance) =>
            val movement = event.movement
            occurrence(event.plyOffset, event.moveUci) ++ Json.obj(
              "side" -> sideName(movement.side), "from" -> movement.from.key, "to" -> movement.to.key,
              "before_role" -> movement.beforeRole.name, "after_role" -> movement.afterRole.name,
              "promotion_gain_cp" -> event.promotionGainCp, "delta_cp" -> delta, "balance_cp" -> balance
            ) ++ event.capture.map(capture => Json.obj("capture" -> Json.obj(
              "role" -> capture.capturedRole.name, "square" -> capture.square.key, "value_cp" -> capture.valueCp,
              "recapture" -> (capture.recaptureStatus match
                case LineMaterialRecaptureStatus.Proven(_) => "proven"
                case LineMaterialRecaptureStatus.Excluded => "excluded"
                case LineMaterialRecaptureStatus.Unknown => "unknown")
            ))).getOrElse(Json.obj())
          }
        base + ("material" -> Json.obj(
          "events" -> materialEvents, "net_cp" -> summary.observedNetCpForMover,
          "max_gain_cp" -> summary.observedMaxGainCpForMover, "max_loss_cp" -> summary.observedMaxLossCpForMover,
          "recovered_after_loss" -> summary.hasObservedRecovery))
      }

  /** Shared admission check for consumers of the typed ledger. It does not
    * serialize the summary and does not certify any alternative continuation.
    */
  def certifiedFact(ctx: JudgmentAssemblyContext, line: LegalLineNode): Option[(EvidenceRecord, LineFactEvidence)] =
    for
      (record, fact) <- ctx.evidenceGraph.uniqueProofEligibleLineFactRecordFor(line.ref)
      if record.ref == line.evidence && line.evidence.position.sideToMove.nonEmpty
      ids <- ctx.lineTransitionIds.get(line.ref)
      if ownsPath(ctx, line, fact, ids)
    yield record -> fact

  private def ownsPath(
      ctx: JudgmentAssemblyContext,
      line: LegalLineNode,
      fact: LineFactEvidence,
      ids: List[String]
  ): Boolean =
    val steps = fact.lineReplaySteps.toVector
    val transitions = ids.map(id => ctx.evidenceGraph.byId.get(id).collect {
      case record @ EvidenceRecord(_, value: MoveTransitionEvidence, _)
          if ctx.evidenceGraph.proofEligible(record) => value
    })
    val edges = transitions.flatten
    def ownsEvent(offset: Int, move: String): Boolean =
      steps.lift(offset).exists(step => EvidenceRef.sameMove(step.moveUci, move))
    ids.nonEmpty && ids.size == steps.size && ids.distinct.size == ids.size &&
      transitions.forall(_.nonEmpty) &&
      transitions.head.exists(_.from == line.evidence.position) &&
      edges.zip(steps).forall { (edge, step) =>
        edge.from.fen == step.fenBefore && edge.to.fen == step.fenAfter && edge.to.ply == step.ply &&
          EvidenceRef.sameMove(edge.moveUci, step.moveUci)
      } &&
      edges.zip(edges.drop(1)).forall((left, right) => left.to == right.from) &&
      fact.lineEvents.forall(event => ownsEvent(event.plyOffset, event.moveUci)) &&
      fact.material.forall(summary => line.evidence.position.sideToMove.contains(summary.sideToMove) &&
        summary.events.forall(event => ownsEvent(event.plyOffset, event.moveUci)))

  private def sideName(side: chess.Color): String = if side.white then "white" else "black"

  private def eventKind(kind: LineEventKind): String = kind match
    case LineEventKind.Capture => "capture"
    case LineEventKind.Recapture => "recapture"
    case LineEventKind.CheckEvasion => "check_evasion"
    case LineEventKind.Castling => "castling"
    case LineEventKind.Check => "check"
    case LineEventKind.Mate => "mate"
    case LineEventKind.Stalemate => "stalemate"
    case LineEventKind.Promotion => "promotion"
