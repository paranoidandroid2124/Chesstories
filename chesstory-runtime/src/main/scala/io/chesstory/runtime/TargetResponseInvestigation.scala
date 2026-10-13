package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.{ LegalMoveResource, PositionResourceView }
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.{ AdmittedCandidateLine, CanonicalPositionHistory }
import play.api.libs.json.*

/** A fixed target and two classes of immediate replies. Geometry, legality and identity stay with existing owners. */
private[runtime] object TargetResponseInvestigation:
  // Selection records where this example came from; search scope and peers stay with Search.
  case class Selection(source: String = "legal_seed", workId: Option[String] = None, lineId: Option[String] = None):
    def json: JsObject = Json.obj("source" -> source, "work_id" -> workId, "line_id" -> lineId)

  case class Question(id: String, rootOccurrenceId: String, rootPositionId: String,
      rootMove: String, rootMoveOccurrenceId: String, childPositionId: String,
      originalTarget: RelationColoredPieceWitness, target: RelationColoredPieceWitness,
      pressureSourceIds: List[String], captureMoves: List[String], declineMoves: List[String],
      moveSourceIds: Map[String, String], referenceMove: String, alternativeMove: String,
      referenceSelection: Selection = Selection(), alternativeSelection: Selection = Selection())

  /** One exact continuation search and its certified full-root paths, shared by questions at that position. */
  final class Search private[TargetResponseInvestigation] (private val owner: EvidenceBackedJudgmentPacket,
      val work: IssuedEngineWork, val lines: List[AdmittedCandidateLine],
      val pathsByMove: Map[String, ObservedLineProtocol.PreparedPath], private val mover: chess.Color):
    def belongsTo(packet: EvidenceBackedJudgmentPacket): Boolean = owner eq packet
    val anchor: EngineSearchAnchor = work.searchAnchor.get
    lazy val completedDepth: Option[Int] =
      val depths = lines.flatMap(_.evaluation.engineLine.map(_.depth))
      Option.when(depths.nonEmpty && depths.size == lines.size && depths.distinct.size == 1)(depths.head)
    /** A requested pair at this exact child, independent of target-response discovery. */
    def pairAnswer(packet: EvidenceBackedJudgmentPacket, referenceMove: String, alternativeMove: String)
        : Option[CommentaryInvestigation.Answer] =
      for
        _ <- Option.when(belongsTo(packet) && referenceMove != alternativeMove)(())
        reference <- lines.find(_.evaluation.moves.head == referenceMove)
        alternative <- lines.find(_.evaluation.moves.head == alternativeMove)
        if CommentaryInvestigation.comparisonReady(work, List(reference, alternative))
        left <- pathsByMove.get(referenceMove)
        right <- pathsByMove.get(alternativeMove)
        leftOccurrence <- left.ids.lift(anchor.plyOffset)
        rightOccurrence <- right.ids.lift(anchor.plyOffset)
      yield CommentaryInvestigation.Answer(work.workId, work.executionKeySha256, work.engineProfile.wireId,
        work.requiredDepth, reference.evaluation.engineLine.map(_.depth), alternative.evaluation.engineLine.map(_.depth),
        CommentaryInvestigation.evaluationRelation(mover, reference.evaluation, alternative.evaluation),
        left.line.ref.id, right.line.ref.id,
        ObservedLineProtocol.evaluationJson(reference.evaluation, EvaluationSource.ResponseComparison),
        ObservedLineProtocol.evaluationJson(alternative.evaluation, EvaluationSource.ResponseComparison),
        leftOccurrence, rightOccurrence,
        PrincipalVariationComparison.quality(mover, reference.evaluation, alternative.evaluation))
    /** The returned child-mover leader and each returned peer, never an all-pairs ranking. */
    lazy val selectedLinePairs: List[(ObservedLineProtocol.PreparedPath, ObservedLineProtocol.PreparedPath)] =
      selectedMovePairs(lines.map(_.evaluation.moves.head))
    /** A question may name a functional family inside the same returned work.
      * Its strongest returned member is compared with each returned family peer.
      */
    def selectedMovePairs(moves: List[String]): List[(ObservedLineProtocol.PreparedPath, ObservedLineProtocol.PreparedPath)] =
      val paths = lines.filter(line => moves.contains(line.evaluation.moves.head))
        .map(line => pathsByMove(line.evaluation.moves.head))
      paths.headOption.toList.flatMap(first => paths.tail.map(first -> _))
    /** A named reference versus a whole legal class, using only this work's returned ranking.
      * This requested comparison does not enlarge the discovery star above.
      */
    def representativePair(referenceMove: String, classMoves: List[String])
        : Option[(ObservedLineProtocol.PreparedPath, ObservedLineProtocol.PreparedPath)] =
      val coversClass = work.rootRestriction match
        case EngineRootRestriction.Unrestricted => true
        case EngineRootRestriction.Restricted(moves) => classMoves.forall(moves.contains)
      for
        _ <- Option.when(classMoves.nonEmpty && !classMoves.contains(referenceMove) && coversClass)(())
        reference <- pathsByMove.get(referenceMove)
        challenger <- lines.find(line => classMoves.contains(line.evaluation.moves.head))
        alternative <- pathsByMove.get(challenger.evaluation.moves.head)
      yield reference -> alternative
    def json: JsObject = Json.obj("work_id" -> work.workId,
      "execution_key_sha256" -> work.executionKeySha256, "engine_profile" -> work.engineProfile.wireId,
      "required_depth" -> work.requiredDepth, "multi_pv" -> work.multiPv,
      "root_move" -> anchor.rootMove, "root_move_occurrence_id" -> anchor.rootOccurrenceId,
      "child_position_id" -> anchor.positionId, "prefix_moves_uci" -> anchor.prefixMoves,
      "ply_offset" -> anchor.plyOffset,
      "root_restriction" -> (work.rootRestriction match
        case EngineRootRestriction.Unrestricted => Json.obj("kind" -> "unrestricted")
        case EngineRootRestriction.Restricted(moves) => Json.obj("kind" -> "restricted", "moves_uci" -> moves)),
      "candidates" -> lines.map { line =>
        val move = line.evaluation.moves.head
        val path = pathsByMove(move)
        Json.obj("move_uci" -> move, "line_id" -> path.line.ref.id, "occurrence_id" -> path.ids(anchor.plyOffset),
          "evaluation" -> ObservedLineProtocol.evaluationJson(line.evaluation, EvaluationSource.ResponseComparison))
      })

  def readSearch(packet: EvidenceBackedJudgmentPacket, receipt: CommentaryInvestigation.EngineCandidates,
      child: CanonicalPositionHistory, prepared: ObservedLineProtocol.PreparedPaths): Option[Search] =
    prepared.requireOwner(packet)
    val work = receipt.work
    val history = packet.assembly.input.positionHistory
    for
      anchor <- work.searchAnchor
      if work.purpose == EngineWorkPurpose.ResponseComparison &&
        anchor.prefixMoves.headOption.contains(anchor.rootMove) &&
        child.initialFen == history.initialFen && child.movePrefixUci == (history.movePrefixUci ++ anchor.prefixMoves)
      if CommentaryInvestigation.ownsSearch(child, work, receipt.lines) &&
        CommentaryInvestigation.candidatesReady(work, receipt.lines)
      view <- prepared.resourceViews(packet).get(anchor.positionId)
      legal <- view.legalRows
      if (work.rootRestriction match
        case EngineRootRestriction.Unrestricted => true
        case EngineRootRestriction.Restricted(moves) => moves.forall(move => legal.exists(_.move == move)))
      paths <- receipt.lines.foldLeft(Option(Map.empty[String, ObservedLineProtocol.PreparedPath])) { (known, line) =>
        for
          values <- known
          path <- prepared.certifiedPath(packet, anchor.prefixMoves ++ line.evaluation.moves)
          if path.ids.headOption.contains(anchor.rootOccurrenceId) &&
            path.positions.lift(anchor.plyOffset).flatMap(_.id).contains(anchor.positionId)
        yield values.updated(line.evaluation.moves.head, path)
      }
    yield new Search(packet, work, receipt.lines.sortBy(line =>
      (-line.evaluation.rankingScoreForMover(child.currentPosition.color), line.evaluation.moves.head)), paths,
      child.currentPosition.color)

  /** The completed first reply pool and the answered capture/decline pair
    * retain separate owners. Their child scores do not explain the root grade. */
  final class FirstTarget private[TargetResponseInvestigation] (
      private val owner: EvidenceBackedJudgmentPacket, private val prepared: ObservedLineProtocol.PreparedPaths,
      val inquiry: CommentaryResponseInquiry, val fullPool: Search, val captureDecline: Search):
    val question: Question = inquiry.question
    val leaderMove: String = fullPool.lines.head.evaluation.moves.head
    val id: String = CommentarySelection.stableId("first-target", List(question.id,
      fullPool.work.executionKeySha256, captureDecline.work.executionKeySha256))
    def belongsTo(packet: EvidenceBackedJudgmentPacket, paths: ObservedLineProtocol.PreparedPaths): Boolean =
      (owner eq packet) && (prepared eq paths)
    def ownershipJson: JsObject = Json.obj("response_inquiry_id" -> question.id,
      "full_pool_work_id" -> fullPool.work.workId,
      "full_pool_execution_key_sha256" -> fullPool.work.executionKeySha256,
      "capture_decline_work_id" -> captureDecline.work.workId,
      "capture_decline_execution_key_sha256" -> captureDecline.work.executionKeySha256)

  def firstTargets(packet: EvidenceBackedJudgmentPacket, prepared: ObservedLineProtocol.PreparedPaths,
      inquiries: List[CommentaryResponseInquiry], searches: List[Search]): List[FirstTarget] =
    prepared.requireOwner(packet)
    inquiries.flatMap { inquiry =>
      val question = inquiry.question
      def atChild(search: Search): Boolean = search.belongsTo(packet) &&
        search.anchor.rootMove == question.rootMove &&
        search.anchor.rootOccurrenceId == question.rootMoveOccurrenceId &&
        search.anchor.positionId == question.childPositionId &&
        search.anchor.prefixMoves == List(question.rootMove) && search.anchor.plyOffset == 1
      for
        _ <- Option.when(inquiry.state == "answered" && inquiry.stopReason.isEmpty &&
          reviewedMover(packet, prepared, question))(())
        _ <- queryResources(packet, question, prepared)
        answer <- inquiry.answer
        if answer.searchPositionId == question.childPositionId && answer.plyOffset == 1
        _ <- inquiry.reading.filter(value => value.belongsTo(packet) && value.complete &&
          value.question.id == question.id &&
          value.lineIds == List(answer.pair.referenceLineId, answer.pair.alternativeLineId))
        if inquiry.readingOwner.exists(_ eq packet)
        comparison <- searches.find(search => atChild(search) &&
          search.work.workId == answer.pair.workId &&
          search.work.executionKeySha256 == answer.pair.executionKeySha256 &&
          search.pairAnswer(packet, question.referenceMove, question.alternativeMove).contains(answer.pair))
        pool <- searches.reverse.filter { search =>
          atChild(search) && search.work.rootRestriction == EngineRootRestriction.Unrestricted &&
            search.work.engineProfile == comparison.work.engineProfile &&
            search.work.requiredDepth >= comparison.work.requiredDepth
        }.sortBy(search => -search.work.requiredDepth).headOption
        leaderMove = pool.lines.head.evaluation.moves.head
        if Set(question.referenceMove, question.alternativeMove)(leaderMove)
        selection = if leaderMove == question.referenceMove then question.referenceSelection
          else question.alternativeSelection
        if selection.source == "child_search"
        selected <- searches.find(search => atChild(search) &&
          search.work.engineProfile == pool.work.engineProfile &&
          selection.workId.contains(search.work.workId) &&
          search.pathsByMove.get(leaderMove).exists(path => selection.lineId.contains(path.line.ref.id)))
        leader <- comparison.pathsByMove.get(leaderMove)
        observed <- pool.pathsByMove.get(leaderMove)
        current <- selected.pathsByMove.get(leaderMove)
        if leader.moves.take(2) == observed.moves.take(2) && leader.ids.take(2) == observed.ids.take(2) &&
          leader.positions.take(3) == observed.positions.take(3) &&
          current.moves.take(2) == observed.moves.take(2) && current.ids.take(2) == observed.ids.take(2) &&
          current.positions.take(3) == observed.positions.take(3)
      yield new FirstTarget(packet, prepared, inquiry, pool, comparison)
    }.distinctBy(_.question.id)

  /** An exact capture question about the actually reviewed mover, rather
    * than every geometrical contact at the same child position. */
  def reviewedMover(packet: EvidenceBackedJudgmentPacket, prepared: ObservedLineProtocol.PreparedPaths,
      question: Question): Boolean =
    prepared.requireOwner(packet)
    packet.assembly.playedTransition.exists { root =>
      question.rootMove == packet.assembly.input.playedMoveUci &&
      question.rootOccurrenceId == root.evidence.id && root.from.id.contains(question.rootPositionId) &&
      question.rootMoveOccurrenceId == root.evidence.id &&
      packet.observations.find(_.transition.evidence.id == root.evidence.id).exists { observation =>
        val mover = observation.mover
        question.originalTarget == RelationColoredPieceWitness(mover.from, mover.beforeRole, mover.side) &&
        question.target == RelationColoredPieceWitness(mover.to, mover.afterRole, mover.side) &&
        prepared.paths.exists(path => path.certified.nonEmpty && path.ids == path.declared &&
          path.moves == path.sourceMoves && path.moves.headOption.contains(question.rootMove) &&
          path.ids.headOption.contains(question.rootMoveOccurrenceId) &&
          path.positions.lift(1).flatMap(_.id).contains(question.childPositionId) &&
          prepared.positionView(path, 1).flatMap(_.legalRows).exists(rows =>
            question.captureMoves.nonEmpty && question.captureMoves.toSet ==
              rows.filter(_.capture.contains(question.target)).map(_.move).toSet))
      }
    }

  def discover(packet: EvidenceBackedJudgmentPacket, referenceMove: String,
      prepared: ObservedLineProtocol.PreparedPaths, searches: List[Search] = Nil): List[Question] =
    prepared.requireOwner(packet)
    val ctx = packet.assembly
    val views = prepared.resourceViews(packet)
    ctx.playedTransition.toList.flatMap { root =>
      root.from.id.flatMap(views.get).filter(_.position == root.from).toList.flatMap { _ =>
        val seeds = prepared.attackedRootTargets(packet)
        List(ctx.input.playedMoveUci, EvidenceRef.normalizeMove(referenceMove)).distinct.flatMap { move =>
          prepared.paths.find(path => path.moves.headOption.contains(move) && path.certified.nonEmpty).toList.flatMap { path =>
            for
              childPosition <- path.positions.lift(1).toList
              child <- childPosition.id.flatMap(views.get).filter(_.position == childPosition).toList
              replies <- child.legalRows.toList
              if replies.nonEmpty
              if ctx.lineReplays.contains(path.line.ref)
              owned = searches.reverse.filter(search => search.belongsTo(packet) &&
                search.anchor.prefixMoves == List(move) &&
                search.anchor.rootMove == move && search.anchor.rootOccurrenceId == path.ids.head &&
                childPosition.id.contains(search.anchor.positionId))
              rootHints = ctx.input.evaluationObservations.reverse.filter(observation =>
                observation.source != EvaluationSource.DirectInput && observation.source != EvaluationSource.ResponseComparison)
                .flatMap { observation =>
                prepared.certifiedPath(packet, observation.evaluation.moves).filter(_.moves.headOption.contains(move))
              }
              observed = prepared.investigationPaths.filter(candidate => candidate.moves.headOption.contains(move) &&
                candidate.certified.nonEmpty && candidate.ids == candidate.declared)
              // En passant captures an off-destination victim without geometric control of its square.
              contacted = child.attackedTargets(ctx.input.positionHistory.currentPosition.color) ++
                replies.flatMap(row => row.capture.filter(_.square != row.to).map(_ -> Nil))
              // Bind newly exposed targets to this exact child and their original physical identity.
              exposed = contacted.flatMap { (target, pressure) =>
                prepared.identitiesAfter(packet, path, 1).collectFirst {
                  case (original, Some(Some(current))) if current == target => original -> pressure
                }
              }
              (original, pressure) <- (seeds ++ exposed).groupMap(_._1)(_._2).toList
                .sortBy(_._1.square.key).map((piece, sources) => piece -> sources.flatten.distinct)
              target <- prepared.pieceAfter(packet, path, original, 1).flatten.toList
              captures = replies.filter(_.captures(target))
              declines = replies.filterNot(_.captures(target))
              if captures.nonEmpty && declines.nonEmpty
            yield
              def select(values: List[LegalMoveResource]): (String, Selection) =
                val members = values.map(_.move).toSet
                def fromSearch(search: Search) = search.lines.find(line => members(line.evaluation.moves.head)).map { line =>
                  val reply = line.evaluation.moves.head
                  reply -> Selection("child_search", Some(search.work.workId), Some(search.pathsByMove(reply).line.ref.id))
                }
                val (wholeClass, partialClass) = owned.partition(search => search.work.rootRestriction match
                  case EngineRootRestriction.Unrestricted => true
                  case EngineRootRestriction.Restricted(moves) => members.subsetOf(moves.toSet))
                def fromPaths(paths: List[ObservedLineProtocol.PreparedPath], source: String) =
                  paths.find(path => path.moves.headOption.contains(move) && path.moves.lift(1).exists(members))
                    .map(path => path.moves(1) -> Selection(source, lineId = Some(path.line.ref.id)))
                (wholeClass ++ partialClass).iterator.flatMap(fromSearch).take(1).toList.headOption
                  .orElse(fromPaths(rootHints, "root_pv_hint"))
                  .orElse(fromPaths(observed, "observed_line"))
                  .getOrElse(values.map(_.move).sorted.head -> Selection())
              val (capture, captureSelection) = select(captures)
              val (decline, declineSelection) = select(declines)
              Question(CommentarySelection.stableId("target-response", List(root.evidence.id, path.ids.head,
                original.side.toString, original.role.name, original.square.key)),
                root.evidence.id, root.from.id.get, move, path.ids.head, childPosition.id.get,
                original, target, pressure.map(_.semanticId).distinct.sorted,
                captures.map(_.move), declines.map(_.move), replies.map(value => value.move -> value.source.semanticId).toMap,
                capture, decline, captureSelection, declineSelection)
          }
        }
      }
    }.distinctBy(_.id)

  def snapshot(question: Question, state: String, answer: Option[JsObject], stopReason: Option[String],
      relatedInquiryIds: List[String] = Nil): JsObject =
    require(Set("deferred", "awaiting_engine", "answered", "stopped", "observed")(state), "unknown target-response lifecycle")
    require((state == "answered") == answer.nonEmpty, "only an answered response inquiry has an answer")
    Json.obj("id" -> question.id, "scope" -> "target_response", "source" -> "automatic_observation",
      "root_occurrence_id" -> question.rootOccurrenceId, "root_move" -> question.rootMove,
      "root_move_occurrence_id" -> question.rootMoveOccurrenceId, "child_position_id" -> question.childPositionId,
      "original_target" -> piece(question.originalTarget, question.rootPositionId),
      "target" -> piece(question.target, question.childPositionId), "pressure_source_ids" -> question.pressureSourceIds,
      "capture_moves" -> question.captureMoves, "decline_moves" -> question.declineMoves,
      "move_source_ids" -> question.moveSourceIds, "reference_move" -> question.referenceMove,
      "alternative_move" -> question.alternativeMove, "related_inquiry_ids" -> relatedInquiryIds,
      "reference_selection" -> question.referenceSelection.json, "alternative_selection" -> question.alternativeSelection.json,
      "state" -> state, "stop_reason" -> stopReason, "answer" -> answer)

  private case class PiecePair(original: RelationColoredPieceWitness, current: RelationColoredPieceWitness):
    def json(rootPosition: String, currentPosition: String): JsObject = Json.obj(
      "original" -> piece(original, rootPosition), "current" -> piece(current, currentPosition))

  private case class Providers(state: String, pieces: List[PiecePair], sourceIds: List[String])
  private case class Recaptures(state: String, moves: List[LegalMoveResource])
  private case class ObservedRecapture(state: String, occurrence: Option[String], position: Option[String],
      actor: Option[PiecePair], targets: List[PiecePair], sourceIds: List[String])
  private case class Branch(path: ObservedLineProtocol.PreparedPath, reply: String, occurrence: String,
      position: String, targetState: String, target: Option[RelationColoredPieceWitness], providers: Providers,
      capturer: Option[RelationColoredPieceWitness], recaptures: Recaptures, observed: ObservedRecapture):
    def complete: Boolean = targetState != "unknown" && providers.state != "unknown" && recaptures.state != "unknown" &&
      (recaptures.moves.isEmpty || observed.state == "complete")
    def json(question: Question): JsObject = Json.obj("line_id" -> path.line.ref.id,
      "reply_move" -> reply, "reply_occurrence_id" -> occurrence, "position_id" -> position,
      "target_state" -> targetState, "target" -> target.map(piece(_, position)),
      "providers" -> Json.obj("state" -> providers.state,
        "pieces" -> providers.pieces.map(_.json(question.rootPositionId, position)), "source_ids" -> providers.sourceIds),
      "capturer" -> capturer.map(piece(_, position)),
      "recaptures" -> Json.obj("state" -> recaptures.state, "moves" -> recaptures.moves.map(value =>
        Json.obj("move_uci" -> value.move, "source_id" -> value.source.semanticId, "actor" -> piece(value.actor, position)))),
      "observed_recapture" -> Json.obj("state" -> observed.state, "occurrence_id" -> observed.occurrence,
        "actor" -> observed.actor.flatMap(actor => observed.position.map(actor.json(question.rootPositionId, _))),
        "supported_targets" -> observed.position.toList.flatMap(at => observed.targets.map(_.json(question.rootPositionId, at))),
        "source_ids" -> observed.sourceIds))

  final class Reading private[TargetResponseInvestigation] (private val owner: EvidenceBackedJudgmentPacket,
      val question: Question, private val branches: List[Branch], private val prepared: ObservedLineProtocol.PreparedPaths):
    val id: String = CommentarySelection.stableId("target-function", question.id :: branches.map(_.path.line.ref.id))
    val lineIds: List[String] = branches.map(_.path.line.ref.id)
    def belongsTo(packet: EvidenceBackedJudgmentPacket): Boolean = owner eq packet
    def complete: Boolean = branches.forall(_.complete)
    def candidateJson(packet: EvidenceBackedJudgmentPacket): JsObject =
      require(belongsTo(packet), "a target reading belongs to its exact packet")
      val paths = branches.map(_.path)
      Json.obj("id" -> id, "kind" -> "comparison", "selection" -> "context",
        "root_occurrence_id" -> question.rootOccurrenceId,
        "occurrence_ids" -> (question.rootOccurrenceId :: paths.flatMap(_.ids)).distinct,
        "position_ids" -> paths.flatMap(_.positions.flatMap(_.id)).filter(prepared.resourceIds).distinct,
        "line_ids" -> paths.map(_.line.ref.id), "evidence_ids" -> paths.flatMap(_.certified.map(_._1.ref.id)).distinct,
        // Scores live in the response receipt/conditional_evaluations, not in this function proof.
        "source_refs" -> paths.map(path => Json.obj("line_id" -> path.line.ref.id, "kind" -> "admitted",
          "source" -> "legal_replay", "accepted_ply_count" -> path.moves.size, "stop" -> JsNull)),
        "detail" -> Json.obj("type" -> "target_response_comparison", "scope" -> "observed_lines",
          "response_inquiry_id" -> question.id, "original_target" -> piece(question.originalTarget, question.rootPositionId),
          "branches" -> branches.map(_.json(question)),
          "text" -> "These lines show whether the same target survives, its direct support, and legal immediate recaptures. "
            .concat("Support after a recapture is reported only when that recapture occurs in the provided line. ")
            .concat("These are conditional observations, not forced replies or the sole reason for a move's evaluation.")))

  /** Reads two already admitted full-root paths; child scores never enter this proof. */
  def read(packet: EvidenceBackedJudgmentPacket, question: Question, referenceLineId: String,
      alternativeLineId: String, prepared: ObservedLineProtocol.PreparedPaths): Option[Reading] =
    prepared.requireOwner(packet)
    for
      views <- queryResources(packet, question, prepared)
      reference <- prepared.path(packet, referenceLineId)
      alternative <- prepared.path(packet, alternativeLineId)
      first <- branch(packet, question, reference, question.referenceMove, views, prepared)
      second <- branch(packet, question, alternative, question.alternativeMove, views, prepared)
    yield new Reading(packet, question, List(first, second), prepared)

  private def queryResources(packet: EvidenceBackedJudgmentPacket, question: Question,
      prepared: ObservedLineProtocol.PreparedPaths): Option[Map[String, PositionResourceView]] =
    val ctx = packet.assembly
    val views = prepared.resourceViews(packet)
    for
      root <- ctx.playedTransition
      if root.evidence.id == question.rootOccurrenceId && root.from.id.contains(question.rootPositionId)
      rootView <- views.get(question.rootPositionId)
      if rootView.occupantAt(question.originalTarget.square).contains(question.originalTarget)
      child <- views.get(question.childPositionId)
      // Discovery combines the same physical target's pressure before and after the root move.
      rootPressure = prepared.attackedRootTargets(packet).find(_._1 == question.originalTarget).toList.flatMap(_._2)
      childPressure = child.attackedTargets(ctx.input.positionHistory.currentPosition.color)
        .find(_._1 == question.target).toList.flatMap(_._2)
      pressure = rootPressure ++ childPressure
      replies <- child.legalRows
      if pressure.map(_.semanticId).distinct.sorted == question.pressureSourceIds
      if pressure.nonEmpty || replies.exists(row => row.captures(question.target) && row.to != question.target.square)
      if replies.filter(_.captures(question.target)).map(_.move) == question.captureMoves
      if replies.filterNot(_.captures(question.target)).map(_.move) == question.declineMoves
      if replies.map(value => value.move -> value.source.semanticId).toMap == question.moveSourceIds
      if question.captureMoves.contains(question.referenceMove) && question.declineMoves.contains(question.alternativeMove)
    yield views

  /** Reuses the same function consumer, never merely the presence of two legal reply moves. */
  def existingContrast(packet: EvidenceBackedJudgmentPacket, question: Question,
      prepared: ObservedLineProtocol.PreparedPaths): Option[Reading] =
    inspectExisting(packet, question, prepared).filter(_.complete)

  /** Prefer a completed comparison, but retain an available partial comparison on interruption. */
  def inspectExisting(packet: EvidenceBackedJudgmentPacket, question: Question,
      prepared: ObservedLineProtocol.PreparedPaths): Option[Reading] =
    prepared.requireOwner(packet)
    queryResources(packet, question, prepared).flatMap { views =>
      // Each admitted path is inspected once. Pairing needs no cross product or new proof work.
      def select(reply: String): Option[Branch] =
        val candidates = prepared.paths.filter(_.moves.take(2) == List(question.rootMove, reply))
          .flatMap(path => branch(packet, question, path, reply, views, prepared))
        candidates.find(_.complete).orElse(candidates.headOption)
      for
        first <- select(question.referenceMove)
        second <- select(question.alternativeMove)
      yield new Reading(packet, question, List(first, second), prepared)
    }

  def hasExistingContrast(packet: EvidenceBackedJudgmentPacket, question: Question,
      prepared: ObservedLineProtocol.PreparedPaths): Boolean = existingContrast(packet, question, prepared).nonEmpty

  /** Compare the actual mover's answered first replies and their immediate roles.
    * Displayed actions keep their prices; earlier episodes stay together in detail.
    * Without a deliverable owned comparison, retain the existing factual reading. */
  def withAnswer(packet: EvidenceBackedJudgmentPacket, prepared: ObservedLineProtocol.PreparedPaths,
      pair: List[ObservedLineProtocol.PreparedPath], proofs: List[FirstTarget],
      notes: List[MoveWorkProjection.LocalCandidate], previous: CommentaryAnswer.Answer,
      complete: CommentaryAnswer.Answer => Option[CommentaryAnswer.Answer],
      protect: (CommentaryAnswer.Answer, List[CommentaryAnswer.Section]) => CommentaryAnswer.Answer)
      : CommentaryAnswer.Answer =
    ResponsePurposeReading.withAnswer(packet, prepared, pair, proofs, notes, previous, complete)

  /** The original target can keep a changed defense after an own action even
    * when the opposing reply captured a different piece. Complete inventories
    * certify the change; they do not certify safety or the value of that reply. */
  def supportThroughOwnAction(packet: EvidenceBackedJudgmentPacket, prepared: ObservedLineProtocol.PreparedPaths,
      question: Question, path: ObservedLineProtocol.PreparedPath): Option[List[CommentaryAnswer.ControlState]] =
    prepared.requireOwner(packet)
    for
      _ <- Option.when(prepared.paths.exists(_ eq path) && path.certified.nonEmpty &&
        path.ids == path.declared && path.moves == path.sourceMoves && path.moves.size >= 3 &&
        path.moves.headOption.contains(question.rootMove) &&
        path.ids.headOption.contains(question.rootMoveOccurrenceId) &&
        path.positions.lift(1).flatMap(_.id).contains(question.childPositionId) &&
        (question.captureMoves ++ question.declineMoves).contains(path.moves(1)))(())
      _ <- queryResources(packet, question, prepared)
      if prepared.pieceAfter(packet, path, question.originalTarget, 1).contains(Some(question.target))
      beforeView <- prepared.positionView(path, 2)
      afterView <- prepared.positionView(path, 3)
      if List(beforeView, afterView).forall(_.completeKinds(RelationFactKind.GeometricControl))
      beforeTarget <- prepared.pieceAfter(packet, path, question.originalTarget, 2).flatten
      afterTarget <- prepared.pieceAfter(packet, path, question.originalTarget, 3).flatten
      if beforeView.occupantAt(beforeTarget.square).contains(beforeTarget) &&
        afterView.occupantAt(afterTarget.square).contains(afterTarget)
      own <- beforeView.legalRows.flatMap(_.find(_.move == path.moves(2)))
      if own.actor.side == question.originalTarget.side
      occurrence <- path.ids.lift(2)
      observation <- packet.observations.find(_.transition.evidence.id == occurrence)
      if observation.transition.from == path.positions(2) && observation.transition.to == path.positions(3) &&
        observation.mover.from == own.actor.square && observation.mover.beforeRole == own.actor.role &&
        observation.mover.side == own.actor.side
      before = new MoveWorkProjection.Position(beforeView)
      after = new MoveWorkProjection.Position(afterView)
      beforeProviders = before.controllers(beforeTarget.square.key, beforeTarget.side)
      afterProviders = after.controllers(afterTarget.square.key, afterTarget.side)
      beforeIdentities = prepared.identitiesAfter(packet, path, 2)
      afterIdentities = prepared.identitiesAfter(packet, path, 3)
      if beforeProviders.forall(provider => beforeIdentities.exists(_._2.contains(Some(provider)))) &&
        afterProviders.forall(provider => afterIdentities.exists(_._2.contains(Some(provider))))
    yield
      def originals(providers: List[RelationColoredPieceWitness], identities:
          List[(RelationColoredPieceWitness, Option[Option[RelationColoredPieceWitness]])]) =
        providers.flatMap(provider => identities.collectFirst {
          case (original, Some(Some(current))) if current == provider => original
        })
      val beforeOriginals = originals(beforeProviders, beforeIdentities)
      val afterOriginals = originals(afterProviders, afterIdentities)
      val lost = beforeOriginals.filterNot(afterOriginals.contains)
      val gained = afterOriginals.filterNot(beforeOriginals.contains)
      val move = RootQuestionSummary.MoveRef(path, 2)
      val withdrawals = lost.flatMap(original => prepared.pieceAfter(packet, path, original, 3).flatten.map(actor =>
        CommentaryAnswer.ControlState(move, actor, Some(afterTarget), afterTarget.square.key, false,
          Option.when(gained.isEmpty)(afterProviders))))
      val arrivals = gained.flatMap(original => prepared.pieceAfter(packet, path, original, 3).flatten.map(actor =>
        CommentaryAnswer.ControlState(move, actor, Some(afterTarget), afterTarget.square.key, true)))
      withdrawals ++ arrivals

  /** Add no new selected action: the existing root answer already owns this
    * own move and will complete its prices after the defense is made explicit. */
  def withOwnTargetSupport(packet: EvidenceBackedJudgmentPacket, prepared: ObservedLineProtocol.PreparedPaths,
      pair: List[ObservedLineProtocol.PreparedPath], previous: CommentaryAnswer.Answer): CommentaryAnswer.Answer =
    prepared.requireOwner(packet)
    val additions = for
      path <- pair.filter(_.moves.headOption.contains(packet.assembly.input.playedMoveUci))
      if previous.core.exists(_.actionReferences.contains(RootQuestionSummary.MoveRef(path, 2)))
      reference <- pair.find(peer => !(peer eq path)).map(_.moves.head).toList
      question <- discover(packet, reference, prepared).filter(question =>
        question.rootMove == path.moves.head && reviewedMover(packet, prepared, question))
      statements <- supportThroughOwnAction(packet, prepared, question, path).toList
      if statements.nonEmpty
    yield path -> statements
    previous.copy(core = previous.core.map(section => additions.find((path, _) =>
      section.actionReferences.contains(RootQuestionSummary.MoveRef(path, 2))).fold(section) { (_, statements) =>
        section.copy(statements = (section.statements ++ statements).distinct)
      }))

  private def branch(packet: EvidenceBackedJudgmentPacket, question: Question,
      path: ObservedLineProtocol.PreparedPath, reply: String,
      views: Map[String, PositionResourceView], prepared: ObservedLineProtocol.PreparedPaths): Option[Branch] =
    val ctx = packet.assembly
    for
      _ <- Option.when(path.moves.take(2) == List(question.rootMove, reply))(())
      if path.certified.nonEmpty && path.ids == path.declared
      if path.ids.headOption.contains(question.rootMoveOccurrenceId)
      if path.positions.lift(1).flatMap(_.id).contains(question.childPositionId)
      occurrence <- path.ids.lift(1)
      position <- path.positions.lift(2).flatMap(_.id)
      if ctx.lineReplays.contains(path.line.ref)
      if prepared.pieceAfter(packet, path, question.originalTarget, 1).contains(Some(question.target))
    yield
      def identities(values: List[(RelationColoredPieceWitness, Option[Option[RelationColoredPieceWitness]])]) =
        values.flatMap { (original, state) => state.flatten.map(PiecePair(original, _)) }
      val replyIdentities = prepared.identitiesAfter(packet, path, 2)
      val afterReply = identities(replyIdentities)
      val tracked = replyIdentities.find(_._1 == question.originalTarget).flatMap(_._2)
      val target = tracked.flatten
      val targetState = tracked match
        case None => "unknown"
        case Some(None) => "captured"
        case Some(Some(_)) => "present"
      val view = views.get(position)
      val providers = target match
        case None => Providers(if targetState == "captured" then "not_applicable" else "unknown", Nil, Nil)
        case Some(target) => view.flatMap(_.completeFacts(RelationFactKind.GeometricControl)).map { facts =>
          val support = facts.flatMap { fact => fact.detail match
            case RelationWitnessDetail.GeometricControl(side, from, role, to) if side == target.side && to == target.square =>
              afterReply.find(_.current == RelationColoredPieceWitness(from, role, side)).map(_ -> fact.semanticId)
            case _ => None
          }
          Providers("complete", support.map(_._1).distinct, support.map(_._2).distinct.sorted)
        }.getOrElse(Providers("unknown", Nil, Nil))
      val capturer = Option.when(question.captureMoves.contains(reply)) {
        packet.observations.find(_.transition.evidence.id == occurrence).map(_.mover)
          .map(move => RelationColoredPieceWitness(move.to, move.afterRole, move.side))
      }.flatten
      val recaptures = capturer match
        case Some(actor) => view.flatMap(_.legalRows).map(rows => Recaptures("complete", rows.filter(_.captures(actor))))
          .getOrElse(Recaptures("unknown", Nil))
        case None => Recaptures(if question.captureMoves.contains(reply) then "unknown" else "not_applicable", Nil)
      val observed = (path.moves.lift(2), path.ids.lift(2), path.positions.lift(3).flatMap(_.id)) match
        case (Some(move), Some(id), Some(at)) if recaptures.state == "complete" && recaptures.moves.exists(_.move == move) =>
          val after = identities(prepared.identitiesAfter(packet, path, 3))
          val actor = packet.observations.find(_.transition.evidence.id == id).flatMap { observation =>
            after.find(_.current == RelationColoredPieceWitness(observation.mover.to,
              observation.mover.afterRole, observation.mover.side))
          }
          val controls = views.get(at).flatMap(_.completeFacts(RelationFactKind.GeometricControl))
          (actor, controls) match
            case (Some(actor), Some(facts)) =>
              val supported = facts.flatMap { fact => fact.detail match
                case RelationWitnessDetail.GeometricControl(side, from, role, to)
                    if RelationColoredPieceWitness(from, role, side) == actor.current =>
                  after.find(pair => pair.current.square == to && pair.current.side == actor.current.side &&
                    pair.current.role.name != "king" && pair.original != actor.original).map(_ -> fact.semanticId)
                case _ => None
              }
              ObservedRecapture("complete", Some(id), Some(at), Some(actor), supported.map(_._1).distinct,
                supported.map(_._2).distinct.sorted)
            case _ => ObservedRecapture("unknown", Some(id), Some(at), actor, Nil, Nil)
        case _ => ObservedRecapture(if recaptures.state == "unknown" then "unknown" else "not_observed", None, None, None, Nil, Nil)
      Branch(path, reply, occurrence, position, targetState, target, providers, capturer, recaptures, observed)

  private def piece(value: RelationColoredPieceWitness, positionId: String): JsObject = Json.obj(
    "position_id" -> positionId, "side" -> (if value.side.white then "white" else "black"),
    "role" -> value.role.name, "square" -> value.square.key)
