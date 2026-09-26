package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.analysis.position.PositionRelationExtractor
import lila.chessjudgment.model.line.CanonicalPositionHistory

final case class PawnStateResource private[assembly] (
    square: EvidenceSquare,
    passed: Option[Boolean],
    isolated: Option[Boolean],
    doubled: Option[Boolean],
    sourceIds: List[String]
)

final case class PawnFileResource private[assembly] (
    file: EvidenceFile,
    occupancy: String,
    sourceIds: List[String]
)

/** One existing exact legal-row source, shared by root proposals and reply consumers. */
final case class LegalMoveResource private[assembly] (
    source: RelationFactEvidence, detail: RelationWitnessDetail.LegalMove
):
  val move: String = EvidenceRef.normalizeMove(detail.moveUci)
  val actor: RelationColoredPieceWitness = RelationColoredPieceWitness(detail.moverSquare, detail.moverRole, detail.side)
  def to: EvidenceSquare = detail.destinationSquare
  def sourceId: String = source.semanticId
  val capture: Option[RelationColoredPieceWitness] = detail.capture.map(value =>
    RelationColoredPieceWitness(value.capturedSquare, value.capturedRole, value.capturedSide))
  def captures(piece: RelationColoredPieceWitness): Boolean = capture.contains(piece)
  def promotionRole: Option[String] = Option.when(move.length == 5)(move.last).flatMap {
    case 'q' => Some("queen")
    case 'r' => Some("rook")
    case 'b' => Some("bishop")
    case 'n' => Some("knight")
    case _ => None
  }

/** Position queries carry no graph occurrence identity. A graph view can have incomplete
  * admitted families; a probe reads the existing closed canonical inventory directly.
  */
trait PositionFacts:
  protected def factsOf(kind: RelationFactKind): List[RelationFactEvidence]
  def completeKinds: Set[RelationFactKind]
  def occupantAt(square: EvidenceSquare): Option[RelationColoredPieceWitness]
  def pawnStates: List[PawnStateResource] = Nil
  def pawnFiles: List[PawnFileResource] = Nil

  private lazy val controls = factsOf(RelationFactKind.GeometricControl).flatMap { fact =>
    fact.detail match
      case RelationWitnessDetail.GeometricControl(side, from, role, to) =>
        Some((RelationColoredPieceWitness(from, role, side) -> to) -> fact.semanticId)
      case _ => None
  }.groupMap(_._1)(_._2)

  /** Positive admitted control only. An empty result is absence only when the
    * geometric inventory is complete; it never establishes capture legality.
    */
  def controlSources(actor: RelationColoredPieceWitness, square: EvidenceSquare): List[String] =
    controls.getOrElse(actor -> square, Nil)

  def completeFacts(kind: RelationFactKind): Option[List[RelationFactEvidence]] =
    Option.when(completeKinds(kind))(factsOf(kind))

  lazy val legalRows: Option[List[LegalMoveResource]] =
    completeFacts(RelationFactKind.LegalMove).map(_.flatMap(source => source.detail match
      case detail: RelationWitnessDetail.LegalMove => Some(LegalMoveResource(source, detail))
      case _ => None).sortBy(_.move))

  /** An admitted enemy control of this side's actual king is a current check.
    * No result is not a certificate of safety when controls are incomplete.
    */
  def currentCheckSources(side: chess.Color): List[RelationFactEvidence] =
    factsOf(RelationFactKind.GeometricControl).filter { fact => fact.detail match
      case RelationWitnessDetail.GeometricControl(enemy, _, _, square) if enemy != side =>
        occupantAt(square).exists(piece => piece.side == side && piece.role.name == "king")
      case _ => false
    }

/** A view of admitted records. Completeness belongs to each queried family,
  * so losing a ray cannot erase an independently certified control.
  */
final class PositionResourceView private[assembly] (
    val position: PositionNodeRef,
    val occurrence: EvidenceRef,
    val records: List[EvidenceRecord],
    val completeKinds: Set[RelationFactKind],
    private val inventory: PositionRelationExtractor.PositionRelationInventoryCertificate
) extends PositionFacts:
  private lazy val available = records.collect {
    case EvidenceRecord(_, fact: RelationFactEvidence, _) => fact.semanticId -> fact
  }.toMap
  private lazy val byKind = available.values.toList.groupBy(_.kind).view
    .mapValues(_.sortBy(_.semanticId)).toMap

  protected def factsOf(kind: RelationFactKind): List[RelationFactEvidence] = byKind.getOrElse(kind, Nil)

  // Reuse the admitted board state; consumers must not decode the public FEN.
  def occupantAt(square: EvidenceSquare): Option[RelationColoredPieceWitness] = inventory.occupantAt(square)

  /** One admitted geometric capture's frozen-board own-king safety, not turn legality.
    * King moves/captures, en passant and promotion are outside this query's scope.
    * The exact current pieces and contact must exist; a missing contact is unknown.
    */
  def ordinaryCaptureKeepsKingSafe(actor: RelationColoredPieceWitness,
      target: RelationColoredPieceWitness): Option[Boolean] =
    if controlSources(actor, target.square).isEmpty then None
    else inventory.ordinaryCaptureKeepsKingSafe(actor, target)

  lazy val checkingMoves: Option[List[LegalMoveResource]] = legalRows.map { rows =>
    val sources = inventory.checkingMoveSources.map(_.semanticId).toSet
    rows.filter(row => sources(row.sourceId))
  }

  /** Only an observation-backed proposal; the eventual admitted child supplies the support proof. */
  def supportCandidates(target: RelationColoredPieceWitness): List[RelationFactEvidence] =
    if !completeKinds(RelationFactKind.LegalMove) || !completeKinds(RelationFactKind.GeometricControl) then Nil
    else inventory.supportCandidates(target).filter(source => available.get(source.semanticId).contains(source))

  /** Positive geometric contacts only; neither legal captures nor an evaluation of danger. */
  def attackedTargets(side: chess.Color): List[(RelationColoredPieceWitness, List[RelationFactEvidence])] =
    byKind.getOrElse(RelationFactKind.GeometricControl, Nil).flatMap { fact => fact.detail match
        case RelationWitnessDetail.GeometricControl(enemy, _, _, square) if enemy != side =>
          occupantAt(square).filter(piece => piece.side == side && piece.role.name != "king").map(_ -> fact)
        case _ => None
    }.groupMap(_._1)(_._2).toList.sortBy { (piece, _) => (piece.square.key, piece.role.name) }

  private def hasSources(sources: List[RelationFactEvidence]): Boolean =
    sources.forall(source => available.get(source.semanticId).contains(source))

  // Reading file occupancy does not force the separate topology projection.
  override lazy val pawnFiles: List[PawnFileResource] =
    inventory.pawnFiles(chess.File.all.toSet).flatMap { file =>
      val sources = List(chess.Color.White, chess.Color.Black).flatMap(side =>
        inventory.relationsFor(RelationDependencyKey.PawnFile(side, chess.File.fromChar(file.file.key.head).get))
          .filter(_.kind == RelationFactKind.PawnFileGroup))
      Option.when(sources.size == 2 && hasSources(sources)) {
        val occupancy = file.occupancy match
          case PositionRelationExtractor.ClosedPawnFileOccupancy.Open => "open"
          case PositionRelationExtractor.ClosedPawnFileOccupancy.WhitePawnsOnly => "white_pawns_only"
          case PositionRelationExtractor.ClosedPawnFileOccupancy.BlackPawnsOnly => "black_pawns_only"
          case PositionRelationExtractor.ClosedPawnFileOccupancy.BothSides => "both_sides"
        PawnFileResource(file.file, occupancy, sources.map(_.semanticId))
      }
    }

  override lazy val pawnStates: List[PawnStateResource] =
    val topology = inventory.pawnTopologyView
    val states = topology.stateWitnesses.map(state => (state.side -> state.square) -> state).toMap
    topology.pawns.map { pawn =>
      val state = states(pawn.side -> pawn.square)
      val bound = inventory.certifyState(PositionRelationExtractor.ClosedPositionStateQuery.PawnTopology(state))
        .flatMap(certificate => inventory.bindState(certificate, position, occurrence.scope)).nonEmpty
      val passedSources = List(pawn.passageSource)
      val doubledSources = List(pawn.ownFileSource)
      // Isolation needs both adjacent files; non-isolation needs one occupied
      // adjacent file. A missing unrelated empty file cannot erase that fact.
      val isolationSources = if state.isolated then pawn.adjacentFileSources else
        pawn.adjacentFileSources.find(source => hasSources(List(source)) && (source.detail match
          case RelationWitnessDetail.PawnFileGroup(_, _, pawns) => pawns.nonEmpty
          case _ => false)).toList
      val passed = Option.when(bound && hasSources(passedSources))(state.passed)
      val isolated = Option.when(bound && isolationSources.nonEmpty && hasSources(isolationSources))(state.isolated)
      val doubled = Option.when(bound && hasSources(doubledSources))(state.doubled)
      val sources = passed.toList.flatMap(_ => passedSources) ++
        isolated.toList.flatMap(_ => isolationSources) ++ doubled.toList.flatMap(_ => doubledSources)
      PawnStateResource(state.square, passed, isolated, doubled, sources.map(_.semanticId).distinct.sorted)
    }

object OccurrenceResources:
  /** A local inspection owns exact replay calculations but has no admitted graph identity. */
  final class ProbePath private[OccurrenceResources] (
      private[assembly] val rootHistory: CanonicalPositionHistory,
      private[assembly] val history: CanonicalPositionHistory,
      private[assembly] val replay: CanonicalLineReplay,
      predecessorReplay: Option[CanonicalLineReplay]
  ):
    val moves: List[String] = replay.replaySteps.map(_.moveUci)
    private lazy val tracking = pieceTracking(replay)
    // These projections reuse the canonical transition owners without admitting a line,
    // allocating graph evidence, or forcing material/event work for an ordinary Ply read.
    private lazy val material = LineMaterialSummary(replay, rootHistory, predecessorReplay)
    def materialEvents: List[ObservedLineMaterialEvent] = material.events
    lazy val lineEvents: List[LineMoveEvent] =
      lila.chessjudgment.analysis.line.LineFactNormalizer.lineEvents(replay, Some(material))
    def belongsTo(packet: EvidenceBackedJudgmentPacket): Boolean =
      rootHistory eq packet.assembly.input.positionHistory
    private lazy val originals = replay.analysisBefore(replay.replaySteps.head).toList.flatMap(analysis =>
      chess.Square.all.flatMap(square => analysis.relationInventory.occupantAt(EvidenceSquare(square.key))))
    def at(count: Int): Option[ProbePosition] =
      if count < 0 || count > moves.size then None
      else
        val analysis = if count == 0 then replay.analysisBefore(replay.replaySteps.head)
          else replay.analysisAfter(replay.replaySteps(count - 1))
        analysis.map(value => new ProbePosition(value, originals, original => tracking.piece(original, count)))

  final class ProbePosition private[OccurrenceResources] (
      analysis: lila.chessjudgment.analysis.position.PositionAnalysis,
      originals: List[RelationColoredPieceWitness],
      current: RelationColoredPieceWitness => Option[Option[RelationColoredPieceWitness]]
  ):
    val fen: String = analysis.occurrence.fen
    val sideToMove: chess.Color = analysis.position.color
    val ply: Int = analysis.occurrence.plyCount
    val facts: PositionFacts = new PositionFacts:
      def completeKinds = kinds
      protected def factsOf(kind: RelationFactKind) = analysis.relationInventory.relationsOf(kind)
      def occupantAt(square: EvidenceSquare) = analysis.relationInventory.occupantAt(square)
    def piece(original: RelationColoredPieceWitness): Option[Option[RelationColoredPieceWitness]] = current(original)
    lazy val identities: Map[RelationColoredPieceWitness, Option[Option[RelationColoredPieceWitness]]] =
      originals.map(original => original -> piece(original)).toMap

  /** Reuse the canonical root and each exact prefix calculation. Histories have already
    * passed the job's edge quota; this adapter neither admits an edge nor creates graph evidence.
    */
  def probePaths(packet: EvidenceBackedJudgmentPacket, histories: List[CanonicalPositionHistory],
      retained: List[ProbePath] = Nil): List[ProbePath] =
    val ctx = packet.assembly
    val rootHistory = ctx.input.positionHistory
    val rootAnalysis = ctx.root.flatMap(ctx.positionAnalysis).getOrElse(
      throw IllegalArgumentException("probe paths require this packet's canonical root analysis"))
    require(retained.forall(path => (path.rootHistory eq rootHistory) && path.history.stepsAfter(rootHistory).nonEmpty),
      "retained probes must belong to the same exact reviewed history")
    require(histories.forall(_.stepsAfter(rootHistory).nonEmpty),
      "a probe history must be an exact nonempty root descendant")
    val known = retained.map(_.moves).toSet
    val owned = histories.distinctBy(_.movePrefixUci).filterNot(history =>
      known(history.movePrefixUci.drop(rootHistory.movePrefixUci.size))).flatMap { history =>
      history.stepsAfter(rootHistory).flatMap(CanonicalLineReplay.fromHistory).map(history -> _)
    }
    val shared = CanonicalLineReplay.sharePrefixes(owned.map(_._2), rootAnalysis,
      ctx.lineReplays.values.toList ++ retained.map(_.replay))
    (retained ++ owned.zip(shared).map { case ((history, _), replay) =>
      new ProbePath(rootHistory, history, replay, ctx.predecessorReplay)
    }).distinctBy(_.moves)

  /** One exact replay owns these incremental physical-piece traces. A requested
    * later point extends a trace once; querying earlier points never replays it.
    */
  final class PieceTracking private[OccurrenceResources] (replay: CanonicalLineReplay):
    private val length = replay.replaySteps.size
    private lazy val inventory = replay.analysisBefore(replay.replaySteps.head).map(_.relationInventory)
    private lazy val transitions = replay.singleStepReplays.iterator.map(_._1.boardFootprint)
    private val footprints = scala.collection.mutable.ArrayBuffer.empty[
      lila.chessjudgment.model.position.BoardTransitionFootprint]
    private val traces = scala.collection.mutable.Map.empty[RelationColoredPieceWitness,
      scala.collection.mutable.ArrayBuffer[Option[RelationColoredPieceWitness]]]

    def piece(original: RelationColoredPieceWitness, afterPlyCount: Int)
        : Option[Option[RelationColoredPieceWitness]] = synchronized {
      if afterPlyCount < 0 || afterPlyCount > length ||
          !inventory.exists(_.occupantAt(original.square).contains(original)) then None
      else
        while footprints.size < afterPlyCount && transitions.hasNext do footprints += transitions.next()
        if footprints.size < afterPlyCount then None
        else
          val trace = traces.getOrElseUpdate(original, scala.collection.mutable.ArrayBuffer(Some(original)))
          while trace.size <= afterPlyCount do
            trace += trace.last.flatMap(_.after(footprints(trace.size - 1)))
          Some(trace(afterPlyCount))
    }

  def pieceTracking(replay: CanonicalLineReplay): PieceTracking = new PieceTracking(replay)

  /** Count is relative to this certified replay, never an absolute PGN ply.
    * None is unavailable; Some(None) is the original piece's observed removal, never a new occupant.
    */
  def followPiece(replay: CanonicalLineReplay, original: RelationColoredPieceWitness,
      afterPlyCount: Int): Option[Option[RelationColoredPieceWitness]] =
    pieceTracking(replay).piece(original, afterPlyCount)

  private val kinds = Set(RelationFactKind.GeometricControl, RelationFactKind.RayBarrier,
    RelationFactKind.PawnAdvanceAffordance, RelationFactKind.LegalMove,
    RelationFactKind.PawnFileGroup, RelationFactKind.PawnPassage)

  def fromAssembly(ctx: JudgmentAssemblyContext): List[PositionResourceView] =
    val graph = ctx.evidenceGraph
    ctx.positions.flatMap { node =>
      ctx.positionAnalyses.get(node.ref).flatMap { analysis =>
        graph.recordsFor(node.ref).collectFirst {
          case record @ EvidenceRecord(_, PositionOccurrenceEvidence(value), _)
              if value == analysis.occurrence && graph.proofEligible(record) => record
        }.map { occurrence =>
          val expected = analysis.boardRelations.filter(relation => kinds(relation.kind))
            .map(relation => relation.semanticId -> relation).toMap
          val selected = graph.relationGraph.positionRelationsAt(node.ref, occurrence.ref.scope)
            .filter(relation => expected.get(relation.relation.semanticId).contains(relation.relation) &&
              graph.proofEligible(relation.record))
          val actual = selected.map(_.relation.semanticId).toSet
          val complete = kinds.filter(kind => expected.valuesIterator.filter(_.kind == kind)
            .forall(relation => actual(relation.semanticId)))
          new PositionResourceView(node.ref, occurrence.ref, selected.map(_.record), complete, analysis.relationInventory)
        }
      }
    }
