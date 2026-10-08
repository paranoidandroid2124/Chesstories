package io.chesstory.runtime

import scala.util.Try

import lila.chessjudgment.analysis.assembly.{
  MoveObservationMeaning,
  MoveReviewAssembler,
  MoveReviewInputAdmission,
  RawMoveReviewInput
}
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.{ AutomaticTerminal, CandidateLineEvaluation, DrawClaimAction, DrawClaimRule }
import lila.chessjudgment.model.line.EngineLine
import play.api.libs.json.*

object RuntimeProtocol:
  val RequestSchema = "chesstory.move-meaning.request.v1"
  val ResponseSchema = "chesstory.move-meaning.response.v6"

  final case class Result(httpStatus: Int, body: JsObject)

  private[runtime] def acceptsRequestId(value: String): Boolean =
    InputLimits.validRequestId(value)

  private final case class WireVariation(
      moves: List[String],
      scoreCp: Int,
      mate: Option[Int],
      depth: Int
  ):
    def toCore: EngineLine = EngineLine(moves, scoreCp, mate, depth)

  private object WireVariation:
    given Reads[WireVariation] = Json.reads[WireVariation]

  private final case class WireInput(
      fen: String,
      playedMoveUci: String,
      variations: Option[List[WireVariation]],
      ply: Option[Int],
      movePrefixUci: Option[List[String]]
  ):
    def toCore: RawMoveReviewInput =
      RawMoveReviewInput(
        fen = fen,
        playedMoveUci = playedMoveUci,
        variations = variations.getOrElse(Nil).map(_.toCore),
        ply = ply,
        movePrefixUci = movePrefixUci.getOrElse(Nil)
      )

  private object WireInput:
    given Reads[WireInput] = Json.reads[WireInput]

  def evaluate(bytes: Array[Byte]): Result =
    Try(Json.parse(bytes)).fold(
      _ => failure(None, 400, "invalid_json"),
      json => evaluate(json)
    )

  def evaluate(json: JsValue): Result =
    json match
      case obj: JsObject =>
        (obj \ "request_id").validateOpt[String] match
          case JsError(_) => failure(None, 400, "invalid_request_id")
          case JsSuccess(requestId, _) if !requestId.forall(InputLimits.validRequestId) =>
            failure(None, 400, "invalid_request_id")
          case JsSuccess(requestId, _) =>
            if (obj \ "schema_version").asOpt[String] != Some(RequestSchema) then
              failure(requestId, 400, "unsupported_schema_version")
            else
              (obj \ "input").validate[WireInput] match
                case JsError(_) => failure(requestId, 400, "invalid_move_review_input")
                case JsSuccess(input, _) =>
                  evaluate(requestId, input.toCore)
      case _ => failure(None, 400, "invalid_request")

  private def evaluate(
      requestId: Option[String],
      raw: RawMoveReviewInput
  ): Result =
    if !requestId.forall(InputLimits.validRequestId) then
      failure(None, 400, "invalid_request_id")
    else if !InputLimits.accepts(raw) then
      failure(requestId, 400, "input_limits_exceeded")
    else
      MoveReviewInputAdmission
        .admit(raw)
        .map(MoveReviewAssembler.assemble)
        .flatMap(EvidenceBackedJudgmentPacket.fromAssembly)
        .fold(
          failure(requestId, 422, "move_review_not_buildable")
        )(packet => encodeMoveMeaningResult(requestId, packet))

  private[runtime] def encodeMoveMeaningResult(
      requestId: Option[String],
    packet: EvidenceBackedJudgmentPacket
  ): Result =
    val requestField = requestId.map(value => Json.obj("request_id" -> value)).getOrElse(Json.obj())
    Result(
      200,
      Json.obj(
        "schema_version" -> ResponseSchema,
        "status" -> "ready",
        "move_commentary" -> moveCommentaryJson(packet)
      ) ++ requestField
    )

  private[runtime] def moveCommentaryJson(packet: EvidenceBackedJudgmentPacket): JsObject =
    MoveMeaningProjection.moveCommentaryJson(packet) ++ Json.obj("observations" -> observationsJson(packet))

  private[runtime] def observationsJson(packet: EvidenceBackedJudgmentPacket,
      inquiries: List[CommentaryInquiry] = Nil,
      pvComparisons: List[PrincipalVariationComparison.Reading] = Nil,
      responseInquiries: List[CommentaryResponseInquiry] = Nil,
      prepared: Option[ObservedLineProtocol.PreparedPaths] = None,
      responseSearches: List[TargetResponseInvestigation.Search] = Nil,
      continuationReadings: List[LineContinuationInvestigation.Reading] = Nil,
      continuationInquiries: List[CommentaryContinuationInquiry] = Nil,
      rootSearches: List[CommentaryRootSearch] = Nil,
      proposalEntries: List[CommentaryProposal.Entry] = Nil,
      localExpansionsUsed: Int = 0,
      discoveryState: CommentaryDiscovery.State = CommentaryDiscovery.State()): JsObject =
    def sideJson(side: chess.Color): String = if side.white then "white" else "black"
    def pieceJson(piece: RelationColoredPieceWitness): JsObject = Json.obj(
      "side" -> sideJson(piece.side),
      "role" -> piece.role.name, "square" -> piece.square.key)
    def uncoloredPieceJson(piece: RelationPieceWitness): JsObject = Json.obj(
      "role" -> piece.role.name, "square" -> piece.square.key)
    def legalResourceJson(resource: RelationLegalMoveResourceWitness): JsObject = Json.obj(
      "movement" -> Json.obj("side" -> sideJson(resource.movement.side),
        "from" -> resource.movement.from.key, "to" -> resource.movement.to.key,
        "before_role" -> resource.movement.beforeRole.name, "after_role" -> resource.movement.afterRole.name),
      "move_uci" -> EvidenceRef.normalizeMove(resource.moveUci),
      "capture" -> resource.capture.map(capture => Json.obj("side" -> sideJson(capture.capturedSide),
        "role" -> capture.capturedRole.name, "square" -> capture.capturedSquare.key)))
    def movementResourceJson(resource: RelationMovementResourceWitness): JsObject =
      val target = resource.target match
        case RelationControlTarget.Empty => Json.obj("kind" -> "empty")
        case RelationControlTarget.Friendly(role) => Json.obj("kind" -> "friendly", "role" -> role.name)
        case RelationControlTarget.Enemy(role) => Json.obj("kind" -> "enemy", "role" -> role.name)
      val mode = resource.mode match
        case RelationMovementResourceMode.ControlledDestination => Json.obj("mode" -> "controlled_destination")
        case RelationMovementResourceMode.PawnAdvance => Json.obj("mode" -> "pawn_advance")
        case RelationMovementResourceMode.PawnDoubleAdvance => Json.obj("mode" -> "pawn_double_advance")
        case RelationMovementResourceMode.EnPassantCapture(pawn) =>
          Json.obj("mode" -> "en_passant_capture", "captured_pawn" -> pieceJson(pawn))
      Json.obj("destination" -> resource.destination.key, "target" -> target) ++ mode
    def geometryJson(geometry: RelationRayGeometry): JsObject = Json.obj(
      "direction" -> Json.obj("file_step" -> geometry.direction.fileStep, "rank_step" -> geometry.direction.rankStep),
      "squares" -> geometry.squares.map(_.key))
    def restrictionJson(restriction: RelationRestrictedResourceWitness): JsObject = Json.obj(
      "piece" -> uncoloredPieceJson(restriction.piece), "resource" -> movementResourceJson(restriction.resource),
      "king_square" -> restriction.kingSquare.key,
      "post_move_controllers" -> restriction.postMoveControllers.map(uncoloredPieceJson),
      "absolute_pin_paths" -> restriction.absolutePinPaths.map(path => Json.obj(
        "pinner" -> uncoloredPieceJson(path.pinner), "pinned" -> uncoloredPieceJson(path.pinned),
        "king_square" -> path.kingSquare.key, "geometry" -> geometryJson(path.geometry))))
    def checkResponseJson(response: RelationCheckResponseWitness): JsObject = Json.obj(
      "resource" -> legalResourceJson(response.resource), "modes" -> response.modes.map {
        case RelationCheckResponseMode.KingMove => "king_move"
        case RelationCheckResponseMode.CaptureChecker => "capture_checker"
        case RelationCheckResponseMode.Interpose => "interpose"
      })
    def pawnStateJson(state: RelationPawnTopologyStateWitness): JsObject = Json.obj(
      "side" -> sideJson(state.side), "square" -> state.square.key,
      "doubled" -> state.doubled, "isolated" -> state.isolated, "passed" -> state.passed,
      "geometrically_protected_passer" -> state.geometricallyProtectedPasser,
      "front_square" -> state.frontSquare.map(_.key), "front_occupant" -> state.frontOccupant.map(pieceJson),
      "component_pawns" -> state.componentPawns.map(_.key),
      "connections" -> state.connections.map(connection => Json.obj(
        "peer" -> connection.peer.key,
        "kind" -> (connection.kind match
          case RelationPawnConnectionKind.GeometricSupport => "geometric_support"
          case RelationPawnConnectionKind.Phalanx => "phalanx"),
        "direction" -> (connection.direction match
          case RelationPawnConnectionDirection.Supports => "supports"
          case RelationPawnConnectionDirection.SupportedBy => "supported_by"
          case RelationPawnConnectionDirection.Peer => "peer"))),
      "enemy_pawn_contacts" -> state.enemyPawnContacts.map(_.key))
    def pawnFacetJson(facet: RelationPawnTopologyFacet): String = facet match
      case RelationPawnTopologyFacet.Doubled => "doubled"
      case RelationPawnTopologyFacet.Isolated => "isolated"
      case RelationPawnTopologyFacet.Passed => "passed"
      case RelationPawnTopologyFacet.GeometricallyProtectedPasser => "geometrically_protected_passer"
      case RelationPawnTopologyFacet.FrontState => "front_state"
      case RelationPawnTopologyFacet.Component => "component"
      case RelationPawnTopologyFacet.Connections => "connections"
      case RelationPawnTopologyFacet.EnemyPawnContacts => "enemy_pawn_contacts"
      case RelationPawnTopologyFacet.Removed => "removed"
    def structuralSubjectJson(subject: StructuralSubject): JsObject = subject match
      case StructuralSubject.OpenFile(file) => Json.obj("kind" -> "open_file", "file" -> file.key)
      case StructuralSubject.SemiOpenFile(side, file) =>
        Json.obj("kind" -> "semi_open_file", "side" -> sideJson(side), "file" -> file.key)
      case StructuralSubject.PawnTensionCreated(side, from, to) =>
        Json.obj("kind" -> "pawn_tension_created", "side" -> sideJson(side), "from" -> from.key, "to" -> to.key)
      case StructuralSubject.PawnTensionResolved(side, from, to) =>
        Json.obj("kind" -> "pawn_tension_resolved", "side" -> sideJson(side), "from" -> from.key, "to" -> to.key)
      case StructuralSubject.PassedPawnCreated(side, square) =>
        Json.obj("kind" -> "passed_pawn_created", "side" -> sideJson(side), "square" -> square.key)
      case StructuralSubject.PassedPawnLost(side, square) =>
        Json.obj("kind" -> "passed_pawn_lost", "side" -> sideJson(side), "square" -> square.key)
      case StructuralSubject.PassedPawnAdvanced(side, from, to, rank) =>
        Json.obj("kind" -> "passed_pawn_advanced", "side" -> sideJson(side),
          "from" -> from.key, "to" -> to.key, "relative_rank" -> rank)
      case StructuralSubject.PassedStatusCreated(side, from, to, rank) =>
        Json.obj("kind" -> "passed_status_created", "side" -> sideJson(side),
          "from" -> from.key, "to" -> to.key, "relative_rank" -> rank)
      case StructuralSubject.PassedPawnPromoted(side, from, to) =>
        Json.obj("kind" -> "passed_pawn_promoted", "side" -> sideJson(side), "from" -> from.key, "to" -> to.key)
    def structuralChangeJson(change: TransitionConsequence): JsObject =
      val kind = change.kind match
        case TransitionConsequenceKind.OpenFileEstablished => "open_file_established"
        case TransitionConsequenceKind.SemiOpenFileEstablished => "semi_open_file_established"
        case TransitionConsequenceKind.PawnTensionCreated => "pawn_tension_created"
        case TransitionConsequenceKind.PawnTensionResolution => "pawn_tension_resolution"
        case TransitionConsequenceKind.PassedPawnProgress => "passed_pawn_progress"
        case TransitionConsequenceKind.PassedPawnStatusRemoved => "passed_pawn_status_removed"
      Json.obj("kind" -> kind, "subjects" -> change.subjectFacts.map(structuralSubjectJson),
        "targets" -> change.targetSubjectFacts.map(structuralSubjectJson))
    def controlPointJson(point: lila.chessjudgment.analysis.assembly.PieceControlPoint): JsObject = Json.obj(
      "controller" -> point.controller.map(pieceJson), "target" -> point.target.map(pieceJson),
      "state" -> point.state.toString.toLowerCase, "premise_ids" -> point.premises.map(_.id))
    val ctx = packet.assembly
    val preparedPaths = prepared.getOrElse(ObservedLineProtocol.prepare(packet))
    preparedPaths.requireOwner(packet)
    val eligible = preparedPaths.eligible
    val resources = packet.resources
    val evaluationsByMoves = ctx.input.evaluationObservations.groupBy(_.evaluation.moves)
    responseSearches.foreach(search => require(search.belongsTo(packet), "child search belongs to its exact packet"))
    val searchedEvaluations = responseSearches.flatMap(search => search.lines.map { line =>
      search.pathsByMove(line.evaluation.moves.head).line.ref.id ->
        Json.obj("search_position_id" -> search.anchor.positionId, "ply_offset" -> search.anchor.plyOffset,
          "evaluation" -> ObservedLineProtocol.evaluationJson(line.evaluation, EvaluationSource.ResponseComparison))
    })
    val conditionalByLine = (searchedEvaluations ++ responseInquiries.flatMap(_.answer).flatMap { answer =>
      List(answer.pair.referenceLineId -> answer.pair.referenceEvaluation,
        answer.pair.alternativeLineId -> answer.pair.alternativeEvaluation).map { (lineId, evaluation) =>
        lineId -> Json.obj("search_position_id" -> answer.searchPositionId,
          "ply_offset" -> answer.plyOffset, "evaluation" -> evaluation)
      }
    }).groupMap(_._1)(_._2).view.mapValues(_.distinct).toMap
    def resourceId(position: PositionNodeRef): Option[String] = preparedPaths.resourceId(position)
    val paths = preparedPaths.paths.map { path =>
        val line = path.line
        val ids = path.ids
        val moves = path.moves
        val sources = ctx.input.suppliedLines.filter(_.acceptedMoves.startsWith(moves))
          .map(_.input.source.wireId).distinct.sorted
        Json.obj(
          "id" -> line.ref.id,
          "kind" -> (if ids.size == 1 && moves.head == ctx.input.playedMoveUci then "played" else "analysis"),
          "start_fen" -> ctx.input.positionHistory.currentFen,
          "moves" -> moves,
          "occurrence_ids" -> ids,
          "position_ids" -> path.positions.map(resourceId)
        ) ++ Option.when(sources.nonEmpty)(Json.obj("sources" -> sources)).getOrElse(Json.obj()) ++
          ObservedLineProtocol.summaryJson(path).map(value => Json.obj("line_summary" -> value)).getOrElse(Json.obj()) ++
          Option.when(evaluationsByMoves.contains(path.sourceMoves))(Json.obj("evaluations" ->
            evaluationsByMoves(path.sourceMoves).map(value => ObservedLineProtocol.evaluationJson(value.evaluation, value.source))))
            .getOrElse(Json.obj()) ++
          conditionalByLine.get(line.ref.id).map(values => Json.obj("conditional_evaluations" -> values))
            .getOrElse(Json.obj())
    }
    val historyPath = ctx.input.positionHistory.segmentReplaySteps.lastOption.toList.flatMap { previous =>
      ctx.playedTransition.toList.filter(edge => eligible(edge.evidence.id)).map { played =>
        Json.obj(
          "id" -> s"history:${played.evidence.id}",
          "kind" -> "history",
          "start_fen" -> previous.beforeFen,
          "moves" -> List(previous.uci, played.moveUci),
          "occurrence_ids" -> Json.arr(JsNull, JsString(played.evidence.id)),
          "position_ids" -> List(None, resourceId(played.from), resourceId(played.to))
        )
      }
    }
    val statements = packet.observations.map { observation =>
      val detail = observation.meaning match
        case MoveObservationMeaning.Capture(role, square, status) =>
          Json.obj(
            "kind" -> "capture", "captured_role" -> role.name, "captured_square" -> square.key,
            "recapture" -> (status match
              case LineMaterialRecaptureStatus.Proven(_) => "proven"
              case LineMaterialRecaptureStatus.Excluded => "excluded"
              case LineMaterialRecaptureStatus.Unknown => "unknown")
          )
        case MoveObservationMeaning.Development(squares) =>
          Json.obj("kind" -> "development", "central_squares" -> squares.map(_.key))
        case MoveObservationMeaning.Castling(rook) =>
          Json.obj("kind" -> "castling", "rook" -> Json.obj("from" -> rook.from.key, "to" -> rook.to.key))
        case MoveObservationMeaning.Promotion(role) =>
          Json.obj("kind" -> "promotion", "promoted_role" -> role.name)
        case MoveObservationMeaning.StructuralChange(consequences) =>
          Json.obj("kind" -> "structural_change", "phase" -> "transition",
            "changes" -> consequences.map(structuralChangeJson))
        case MoveObservationMeaning.PieceControl(before, after) =>
          val controller = before.controller.orElse(after.controller).get
          val target = before.target.orElse(after.target).get
          Json.obj("kind" -> "piece_control", "relation" -> (if controller.side == target.side then "support" else "attack"),
            "before" -> controlPointJson(before), "after" -> controlPointJson(after))
        case MoveObservationMeaning.CertifiedRelation(value) => value match
          case detail: RelationWitnessDetail.CaptureRecaptureInventory =>
            Json.obj("kind" -> "recapture_resources", "phase" -> "after",
              "captured" -> pieceJson(detail.captured),
              "geometric_recapturers" -> detail.geometricRecapturers.map(uncoloredPieceJson),
              "legal_recaptures" -> detail.legalRecaptures.map(legalResourceJson),
              "restricted_recaptures" -> detail.restrictedRecaptures.map(restrictionJson))
          case detail: RelationWitnessDetail.CreatedCheckResponseInventory =>
            Json.obj("kind" -> "check_responses", "phase" -> "after",
              "checked_side" -> sideJson(detail.checkedSide), "king_square" -> detail.kingSquare.key,
              "checkers" -> detail.checkers.map(uncoloredPieceJson),
              "responses" -> detail.responses.map(checkResponseJson),
              "controlled_king_destinations" -> detail.controlledKingDestinations.map(destination => Json.obj(
                "resource" -> movementResourceJson(destination.resource),
                "controllers" -> destination.controllers.map(uncoloredPieceJson))),
              "terminal" -> (detail.terminal match
                case RelationCheckTerminalState.Ongoing => "ongoing"
                case RelationCheckTerminalState.Checkmate => "checkmate"))
          case detail: RelationWitnessDetail.RootCheckResponse =>
            Json.obj("kind" -> "check_response", "phase" -> "before",
              "responding_side" -> sideJson(detail.respondingSide), "king_square" -> detail.kingSquare.key,
              "checkers" -> detail.checkers.map(uncoloredPieceJson), "response" -> checkResponseJson(detail.response))
          case detail: RelationWitnessDetail.PawnTopologyTransition =>
            Json.obj("kind" -> "pawn_structure", "phase" -> "transition",
              "before" -> detail.before.map(pawnStateJson), "after" -> detail.after.map(pawnStateJson),
              "changed_facets" -> detail.changedFacets.map(pawnFacetJson))
          case detail: RelationWitnessDetail.StalemateTransition =>
            Json.obj("kind" -> "stalemate", "phase" -> "after",
              "stalled_side" -> sideJson(detail.stalledSide), "king_square" -> detail.kingSquare.key)
      Json.obj(
        "occurrence_id" -> observation.transition.evidence.id,
        "side" -> (if observation.mover.side.white then "white" else "black"),
        "role" -> observation.mover.beforeRole.name,
        "from" -> observation.mover.from.key,
        "to" -> observation.mover.to.key,
        "premise_ids" -> observation.premises.map(_.id)
      ) ++ detail
    }
    val issues = ctx.input.suppliedLines.zipWithIndex.flatMap { case (line, index) =>
      line.stop.map(stop => Json.obj("input_line_index" -> index,
        "accepted_ply_count" -> line.acceptedPlyCount, "reason" -> stop.wireId))
    }
    val observations = Json.obj("statements" -> statements, "paths" -> (paths ++ historyPath),
      "positions" -> preparedPaths.resourceJson) ++
      Option.when(issues.nonEmpty)(Json.obj("input_issues" -> issues)).getOrElse(Json.obj())
    val projection = MoveWorkProjection.select(packet, preparedPaths)
    val trajectories = if continuationReadings.nonEmpty then continuationReadings
      else preparedPaths.continuationReadings
    val byLine = trajectories.map(reading => reading.path.line.ref.id -> reading).toMap
    val targetReadings = responseInquiries.flatMap(inquiry => inquiry.reading.toList ++ inquiry.retainedReadings).distinctBy(_.id)
    def targetKey(question: TargetResponseInvestigation.Question) =
      (question.rootOccurrenceId, question.rootPositionId, question.originalTarget)
    val targetPeers = responseInquiries.map(_.question).groupBy(targetKey)
    val targetReadingByQuestion = responseInquiries.flatMap(inquiry => inquiry.reading.map(inquiry.question.id -> _)).toMap
    // A short new engine sample does not erase an already certified exact
    // extension. Each reply branch owns its observation: an incomplete or
    // divergent decline must not erase a known capture. The continuation
    // comparer checks the individual facts; no entire reading is promoted.
    val extendedTargetPaths = responseInquiries.flatMap { inquiry => inquiry.reading.map { current =>
      val retained = (current :: inquiry.retainedReadings).filter(_.belongsTo(packet))
      inquiry.question.id -> current.lineIds.zipWithIndex.map { (lineId, index) =>
        preparedPaths.path(packet, lineId).flatMap { base =>
          val eligible = retained.flatMap(_.lineIds.lift(index)).flatMap(preparedPaths.path(packet, _))
            .filter(_.ids.startsWith(base.ids)).distinctBy(_.ids)
          val maximal = eligible.filterNot(path => eligible.exists(other =>
            other.ids.size > path.ids.size && other.ids.startsWith(path.ids)))
          // Divergent descendants keep their own observations, with no arbitrary
          // pair selection or cross product between possible continuations.
          maximal match
            case path :: Nil => Some(path.line.ref.id)
            case _ => None
        }
      }
    }}.toMap
    // These are the same question peers exposed by related_inquiry_ids, including a retained old reference.
    val relatedTargetPairs = targetPeers.values.toList.flatMap(_.distinctBy(_.id).sortBy(_.id).combinations(2)).flatMap {
      case List(left, right) if left.rootMove != right.rootMove =>
        val current = for
          l <- targetReadingByQuestion.get(left.id).toList
          r <- targetReadingByQuestion.get(right.id).toList
          (a, b) <- l.lineIds.zip(r.lineIds)
        yield List(a, b)
        val extended =
          for
            l <- extendedTargetPaths.get(left.id).toList
            r <- extendedTargetPaths.get(right.id).toList
            (leftId, rightId) <- l.zip(r)
            a <- leftId.toList
            b <- rightId.toList
          yield List(a, b)
        current ++ extended
      case _ => Nil
    }
    val routePairs = continuationInquiries.flatMap { inquiry => inquiry.routeChallenge.toList.flatMap { route =>
      route.moveClasses.flatMap { group => group.comparison.toList.flatMap { pair =>
        responseSearches.find(search => search.belongsTo(packet) && search.work.workId == pair.workId &&
          search.anchor.positionId == inquiry.question.positionId &&
          search.anchor.prefixMoves == inquiry.question.prefixMoves).flatMap(search =>
          search.representativePair(route.referenceMove, group.movesUci)).filter { (left, right) =>
            left.line.ref.id == pair.referenceLineId && right.line.ref.id == pair.challengeLineId
          }.map((left, right) => List(left.line.ref.id, right.line.ref.id))
      }}
    }}
    val replacementPairs = continuationInquiries.flatMap { inquiry => inquiry.routeChallenge.toList.flatMap { route =>
      val moves = route.moveClasses.filter(_.kind == "replacement_recapture").flatMap(_.movesUci).distinct
      responseSearches.filter(search => search.anchor.positionId == inquiry.question.positionId &&
        search.anchor.prefixMoves == inquiry.question.prefixMoves).flatMap(_.selectedMovePairs(moves))
        .map((left, right) => List(left.line.ref.id, right.line.ref.id))
    }}
    val pairs = pvComparisons.map(reading => List(reading.answer.referenceLineId, reading.answer.alternativeLineId)) ++
      targetReadings.map(_.lineIds) ++ relatedTargetPairs ++ responseSearches.flatMap(_.selectedLinePairs.map {
        (left, right) => List(left.line.ref.id, right.line.ref.id)
      }) ++ routePairs ++ replacementPairs
    // Only selected pairs and explicitly requested reference/class pairs are joined.
    // No all-path cross product or cross-work score ranking.
    val continuationComparisons = pairs.distinctBy(_.sorted).flatMap {
      case List(left, right) => for
        l <- byLine.get(left)
        r <- byLine.get(right)
        comparison <- preparedPaths.continuationComparison(packet, l, r)
      yield comparison
      case _ => None
    }.distinctBy(_.id)
    val comparedLines = continuationComparisons.flatMap(_.lineIds).toSet
    // Quiet paths are only presented here when a selected pair exposes a concrete later functional difference.
    val shownTrajectories = trajectories.filter(reading => reading.events.nonEmpty ||
      reading.captureExposures.nonEmpty || reading.path.moves.size > 1 && comparedLines(reading.path.line.ref.id))
    val trajectoryIds = shownTrajectories.map(reading => reading.path.line.ref.id -> reading.id).toMap
    def withTrajectories(candidate: JsObject): JsObject =
      val lines = (candidate \ "line_ids").as[List[String]]
      val related = lines.flatMap(trajectoryIds.get)
      val comparisons = continuationComparisons.filter(_.lineIds.sorted == lines.sorted).map(_.id)
      if related.isEmpty then candidate else candidate + ("detail" ->
        ((candidate \ "detail").as[JsObject] ++ Json.obj("continuation_candidate_ids" -> related,
          "continuation_comparison_ids" -> comparisons)))
    def effects(candidate: JsObject) = (candidate \ "detail" \ "effects").asOpt[List[JsObject]].getOrElse(Nil).toSet
    // Share only effects already carried by a visible explanation of this exact
    // occurrence. Each different line, prefix, horizon and proof keeps its own text.
    val explained = scala.collection.mutable.Map.empty[String, Set[JsObject]]
    def retainEffects(candidate: JsObject): Unit =
      if (candidate \ "selection").as[String] == "selected" then
        val root = (candidate \ "root_occurrence_id").as[String]
        explained(root) = explained.getOrElse(root, Set.empty) ++ effects(candidate)
    def withFreshEffects(candidate: JsObject, context: List[MoveWorkProjection.Context] = Nil): JsObject =
      val root = (candidate \ "root_occurrence_id").as[String]
      val updated = projection.withMoveEffects(candidate, explained.getOrElse(root, Set.empty), context)
      retainEffects(updated)
      updated
    // Delay wording until the root and its exact co-displayed questions are known.
    // A PV card is required: without it these are independently displayed scenes.
    val contexts = scala.collection.mutable.Map.empty[String, List[MoveWorkProjection.Context]]
    val questions = CommentaryProposal.comparisonCandidates(packet, preparedPaths, proposalEntries,
      Some((candidate, context) =>
        contexts((candidate \ "id").as[String]) = context
        candidate))
    val sharedQuestions = scala.collection.mutable.Map.empty[String, JsObject]
    val replySpaces = CommentaryIdea.replySpaces(proposalEntries).filter(_.verified)
    val reserveEntries = CommentaryIdea.reserveEntries(proposalEntries).filter(value => value.verified &&
      RootSubjectComparison.strongestReserveCounter(packet, preparedPaths, value, responseSearches))
    val directReserveEntries = CommentaryIdea.directReserveEntries(proposalEntries).filter(_.verified)
    val reserveHolds = CommentaryIdea.reserveHolds(proposalEntries).filter(value => value.verified &&
      RootSubjectComparison.strongestReserveHold(packet, preparedPaths, value, responseSearches))
    val reserveTimings = CommentaryIdea.reserveTimings(proposalEntries)
    val reserveResponses = CommentaryIdea.reserveResponses(proposalEntries)
    val replyFunctions = FunctionalReplyReading.read(proposalEntries)
    val firstTargets = TargetResponseInvestigation.firstTargets(packet, preparedPaths,
      responseInquiries, responseSearches)
    val rootCandidates = (projection.baseJson \ "candidates").as[List[JsObject]].map { candidate =>
      val detail = (candidate \ "detail").as[JsObject]
      val pair = (candidate \ "line_ids").as[List[String]]
      val cards = pvComparisons.filter(reading =>
        List(reading.answer.referenceLineId, reading.answer.alternativeLineId).sorted == pair.sorted)
      // A reversed PV may remain a separate card with its own report. That card
      // would include the unordered question without this oriented root body.
      val hasCard = (detail \ "type").as[String] == "root_comparison" && cards.nonEmpty && cards.forall(reading =>
        (detail \ "reference_line_id").asOpt[String].contains(reading.answer.referenceLineId) &&
          (detail \ "candidate_line_id").asOpt[String].contains(reading.answer.alternativeLineId))
      val peers = if !hasCard then Nil else questions.filter { other =>
        (other \ "selection").as[String] == "selected" &&
          (other \ "root_occurrence_id") == (candidate \ "root_occurrence_id") &&
          (other \ "line_ids").as[List[String]].sorted == pair.sorted &&
          (other \ "detail" \ "prefix_moves_uci").as[List[String]].isEmpty &&
          (other \ "detail" \ "evaluation_relation").asOpt[String].nonEmpty
      }
      val rootContext = replySpaces.filter(space =>
        List(space.owner.answer.referenceLineId, space.owner.answer.alternativeLineId).sorted == pair.sorted)
        .map(space => MoveWorkProjection.Context("", replySpace = Some(space))) ++
        reserveEntries.filter(value =>
          List(value.owner.answer.referenceLineId, value.owner.answer.alternativeLineId).sorted == pair.sorted)
          .map(value => MoveWorkProjection.Context("", reserveEntry = Some(value))) ++
        directReserveEntries.filter(value =>
          List(value.owner.answer.referenceLineId, value.owner.answer.alternativeLineId).sorted == pair.sorted)
          .map(value => MoveWorkProjection.Context("", directReserveEntry = Some(value))) ++
        reserveHolds.filter(value =>
          List(value.owner.answer.referenceLineId, value.owner.answer.alternativeLineId).sorted == pair.sorted)
          .map(value => MoveWorkProjection.Context("", reserveHold = Some(value))) ++
        reserveTimings.filter(value =>
          List(value.owner.answer.referenceLineId, value.owner.answer.alternativeLineId).sorted == pair.sorted)
          .map(value => MoveWorkProjection.Context("", reserveTiming = Some(value))) ++
        reserveResponses.filter(value =>
          List(value.owner.answer.referenceLineId, value.owner.answer.alternativeLineId).sorted == pair.sorted)
          .map(value => MoveWorkProjection.Context("", reserveResponse = Some(value))) ++
        replyFunctions.filter(value =>
          preparedPaths.paths.exists(path => pair.contains(path.line.ref.id) &&
            path.moves.headOption.contains(value.owner.comparison.referenceMove)))
          .map(value => MoveWorkProjection.Context("", replyFunction = Some(value))) ++
        firstTargets.filter(value => pair.flatMap(id => preparedPaths.path(packet, id)).exists(path =>
          path.moves.headOption.contains(value.question.rootMove) &&
            path.ids.headOption.contains(value.question.rootMoveOccurrenceId)))
          .map(value => MoveWorkProjection.Context("", firstTarget = Some(value)))
      val composed = projection.withSharedMoveEffects((candidate -> rootContext) :: peers.map(other =>
        other -> contexts((other \ "id").as[String])))
      composed.foreach(retainEffects)
      composed.tail.foreach(other => sharedQuestions((other \ "id").as[String]) = other)
      composed.head
    }
    val rootSelection = projection.baseJson + ("candidates" -> Json.toJson(rootCandidates))
    val questionComparisons = questions.map(candidate => sharedQuestions.getOrElse((candidate \ "id").as[String],
      withFreshEffects(candidate, contexts((candidate \ "id").as[String]))))
    val questionScenes = questionComparisons.filter(candidate => (candidate \ "selection").as[String] == "selected")
      .map(candidate => Json.obj("id" -> CommentarySelection.stableId("question-scene", List((candidate \ "id").as[String])),
        "root_occurrence_id" -> (candidate \ "root_occurrence_id").as[String],
        "occurrence_ids" -> (candidate \ "occurrence_ids").as[List[String]],
        "line_ids" -> (candidate \ "line_ids").as[List[String]],
        "candidate_ids" -> List((candidate \ "id").as[String])))
    val additions = questionComparisons ++ (pvComparisons.map(value => withFreshEffects(value.candidateJson(packet))) ++
      targetReadings.map(_.candidateJson(packet))).map(withTrajectories) ++
      shownTrajectories.map(_.candidateJson(packet)) ++ continuationComparisons.map { reading =>
        val candidate = reading.candidateJson(packet)
        candidate + ("detail" -> ((candidate \ "detail").as[JsObject] +
          ("continuation_candidate_ids" -> Json.toJson(reading.lineIds.flatMap(trajectoryIds.get)))))
      }
    val compared = rootSelection ++ Option.when(additions.nonEmpty)(Json.obj("candidates" ->
      ((rootSelection \ "candidates").as[List[JsObject]] ++ additions),
      "scenes" -> ((rootSelection \ "scenes").as[List[JsObject]] ++ questionScenes)))
      .getOrElse(Json.obj())
    val discovery = Option.when(discoveryState.inputKey.nonEmpty)(Json.obj("discovery" -> Json.obj(
      "version" -> 1, "input_key" -> discoveryState.inputKey,
      "scans" -> discoveryState.scans.map(scan => Json.obj(
        "prefix_moves_uci" -> scan.anchor.prefixMoves, "reference_move" -> scan.anchor.referenceMove,
        "legal_moves" -> scan.legalMoves, "examined_moves" -> scan.examinedMoves,
        "deferred_request_ids" -> scan.deferredRequestIds,
        "pending_replay_prefixes" -> scan.pendingReplayPrefixes,
        "state" -> scan.state, "reason" -> scan.reason)))))
    // A retained inquiry may outlive the PV comparison that originally ordered
    // it. Its lifecycle stays intact; public links must name current candidates.
    val orderingCandidates = pvComparisons.filter(_.eligibleQuietFocus).map(_.id).toSet
    val selection = compared ++ Option.when(proposalEntries.nonEmpty || localExpansionsUsed > 0 || discovery.nonEmpty)(Json.obj(
        "proposals" -> CommentaryProposal.snapshots(proposalEntries, _.explanation(packet, preparedPaths), preparedPaths.notation.line),
      "local_expansions_used" -> localExpansionsUsed)).getOrElse(Json.obj()) ++
      discovery.getOrElse(Json.obj()) ++
      Option.when(rootSearches.nonEmpty)(Json.obj("root_searches" -> rootSearches.map { search =>
      val work = search.work
      val root = packet.assembly.playedTransition.get
      Json.obj("work_id" -> work.workId, "execution_key_sha256" -> work.executionKeySha256,
        "engine_profile" -> work.engineProfile.wireId, "purpose" -> work.purpose.wireId,
        "required_depth" -> work.requiredDepth, "multi_pv" -> work.multiPv,
        "root_position_id" -> root.from.id.get, "root_occurrence_id" -> root.evidence.id,
        "root_restriction" -> (work.rootRestriction match
          case EngineRootRestriction.Unrestricted => Json.obj("kind" -> "unrestricted")
          case EngineRootRestriction.Restricted(moves) => Json.obj("kind" -> "restricted", "moves_uci" -> moves)),
        "state" -> search.state, "stop_reason" -> search.stopReason.map(PositionCommentaryJobProtocol.stopConditionCode),
        "candidates" -> search.receipt.toList.flatMap(_.lines).map { line =>
          val path = preparedPaths.certifiedPath(packet, line.evaluation.moves)
          Json.obj("move_uci" -> line.evaluation.moves.head, "line_id" -> path.map(_.line.ref.id),
            "occurrence_id" -> path.flatMap(_.ids.headOption))
        })
    })).getOrElse(Json.obj()) ++ Option.when(inquiries.nonEmpty)(Json.obj("inquiries" -> inquiries.map { inquiry =>
      val explained = inquiry.answer.toList.flatMap(answer => pvComparisons.filter(reading =>
        reading.answer.referenceLineId == answer.referenceLineId &&
          reading.answer.alternativeLineId == answer.alternativeLineId &&
          reading.answer.workId == answer.workId && reading.answer.executionKeySha256 == answer.executionKeySha256))
      val question = inquiry.question.copy(orderingCandidateIds =
        inquiry.question.orderingCandidateIds.filter(orderingCandidates))
      CommentaryInvestigation.snapshot(question, inquiry.state, inquiry.answer, inquiry.stopReason) ++
        Json.obj("explanation_candidate_ids" -> explained.map(_.id)) ++
        inquiry.assessmentEffect.map(value => Json.obj("assessment_effect" -> value)).getOrElse(Json.obj())
    })).getOrElse(Json.obj()) ++ Option.when(responseInquiries.nonEmpty)(Json.obj(
      "response_inquiries" -> responseInquiries.map(inquiry => TargetResponseInvestigation.snapshot(
        inquiry.question, inquiry.state, inquiry.answer.map(_.json), inquiry.stopReason,
        targetPeers(targetKey(inquiry.question)).filter(_.rootMove != inquiry.question.rootMove)
          .map(_.id).distinct.sorted)))).getOrElse(Json.obj()) ++
      Option.when(responseInquiries.nonEmpty || continuationInquiries.nonEmpty || responseSearches.nonEmpty)(
        Json.obj("response_searches" -> responseSearches.map(search => search.json ++
          Option.when(search.work.proposalRefs.nonEmpty)(Json.obj("proposal_ownership" -> Json.obj(
            "work_id" -> search.work.workId, "execution_key_sha256" -> search.work.executionKeySha256,
            "question_refs" -> search.work.proposalRefs.map((proposal, requirement) =>
              Json.obj("proposal_id" -> proposal, "requirement_id" -> requirement)))))
            .getOrElse(Json.obj())))).getOrElse(Json.obj()) ++
      Option.when(continuationInquiries.nonEmpty)(Json.obj("continuation_inquiries" -> continuationInquiries.map { inquiry =>
        val question = inquiry.question
        Json.obj("id" -> question.id, "scope" -> "line_continuation", "source" -> "automatic_observation",
          "root_occurrence_id" -> question.rootOccurrenceId, "root_position_id" -> question.rootPositionId,
          "position_id" -> question.positionId, "prefix_moves_uci" -> question.prefixMoves,
          "prefix_occurrence_ids" -> question.prefixOccurrenceIds, "trigger_occurrence_ids" -> question.triggerOccurrenceIds,
          "observed_moves" -> question.observedMoves, "source_line_ids" -> question.sourceLineIds,
          "comparison_sources" -> question.comparisonSources.map(_.json),
          "premises" -> inquiry.premiseObservations.map { observed =>
            val premise = observed.premise
            Json.obj("id" -> premise.id(question.id), "kind" -> premise.origin.kind,
              "trigger_occurrence_ids" -> premise.origin.triggerIds, "source_line_ids" -> premise.sourceLineIds,
              "comparison_source_indices" -> premise.origin.comparisonSources.map(question.comparisonSources.indexOf),
              "requirement_id" -> premise.origin.requirementId,
              "observation_state" -> observed.observationState, "witness_line_ids" -> observed.witnessLineIds,
              "unresolved_source_line_ids" -> observed.unresolvedSourceLineIds)
          },
          "kinds" -> question.kinds, "legal_moves" -> question.legalMoves, "move_source_ids" -> question.moveSourceIds,
          "state" -> inquiry.state, "stop_reason" -> inquiry.stopReason, "search_work_ids" -> inquiry.searchWorkIds,
          "unresolved_source_line_ids" -> inquiry.unresolvedSourceLineIds,
          "followup_question_ids" -> inquiry.followupQuestionIds,
          "route_challenge" -> inquiry.routeChallenge.map(route => Json.obj(
            "state" -> route.state, "stop_reason" -> route.stopReason,
            "reference_work_id" -> route.referenceWorkId, "reference_move" -> route.referenceMove,
            "reference_line_id" -> route.referenceLineId,
            "move_classes" -> route.moveClasses.map(group => Json.obj(
              "kind" -> group.kind, "moves_uci" -> group.movesUci, "premise_occurrence_ids" -> group.premiseOccurrenceIds,
              "source_line_ids" -> group.sourceLineIds, "sampled_work_ids" -> group.sampledWorkIds,
              "comparison" -> group.comparison.map(pair => Json.obj("work_id" -> pair.workId,
                "reference_line_id" -> pair.referenceLineId, "challenge_line_id" -> pair.challengeLineId)))))),
          "explanation_candidate_ids" -> ((question.sourceLineIds ++ question.comparisonSources.flatMap(_.observationLineId))
            .distinct.flatMap(trajectoryIds.get) ++
            continuationComparisons.filter(_.lineIds.forall(question.sourceLineIds.contains)).map(_.id)))
      })).getOrElse(Json.obj())
    observations ++ Json.obj("highlights" -> projection.highlightsFor(selection), "selection" -> selection)

  private[runtime] def completedResultJson(completed: CompletedCommentaryJob): JsObject =
    completed.result match
      case CompletedPositionCommentary.PositionAction(action) =>
        MoveMeaningProjection.positionActionJson(action)
      case CompletedPositionCommentary.FocusedMoveReview(drawClaims, record) if record.judgmentPacket.primary.isEmpty =>
        Json.obj("kind" -> "move_observations") ++ Option.when(drawClaims.nonEmpty)(
          Json.obj("draw_claims" -> drawClaims.map(MoveMeaningProjection.drawClaimJson))
        ).getOrElse(Json.obj())
      case CompletedPositionCommentary.FocusedMoveReview(drawClaims, record) =>
        val commentary = MoveMeaningProjection.moveCommentaryJson(record.judgmentPacket)
        val playedRoles =
          if record.moveUci == record.bestMoveUci then List("best", "played") else List("played")
        val playedReview = Json.obj(
          "legal_move_index" -> record.legalMoveIndex,
          "move_uci" -> record.moveUci,
          "selection" -> (Json.obj("roles" -> playedRoles) ++ record.playedRootRank
            .map(rank => Json.obj("root_rank" -> rank))
            .getOrElse(Json.obj())),
          "commentary" -> commentary
        )
        val selectedReviews =
          if record.moveUci == record.bestMoveUci then List(playedReview)
          else
            List(
              Json.obj(
                "legal_move_index" -> record.bestLegalMoveIndex,
                "move_uci" -> record.bestMoveUci,
                "selection" -> (Json.obj("roles" -> Json.arr("best")) ++
                  record.bestRootRank.map(rank => Json.obj("root_rank" -> rank)).getOrElse(Json.obj())),
                "line_insight" -> MoveMeaningProjection.referenceLineInsightJson(commentary)
              ),
              playedReview
            )
        Json.obj(
          "kind" -> "selected_move_choices",
          "selected_move_reviews" -> selectedReviews
        ) ++ Option.when(drawClaims.nonEmpty)(
          Json.obj("draw_claims" -> drawClaims.map(MoveMeaningProjection.drawClaimJson))
        ).getOrElse(Json.obj())
  private[runtime] def encodePositionCommentaryResponse(
      snapshot: CommentaryJobSnapshot,
      completed: CompletedCommentaryJob
  ): JsObject =
    Json.obj(
      "schema_version" -> "chesstory.position-commentary.response.v6",
      "annotation_policy_revision" -> "chesstory.verdict-threshold-policy.v2",
      "request_id" -> snapshot.requestId,
      "job_id" -> snapshot.jobId,
      "engine_profile" -> completed.engineProfile.wireId,
      "variant" -> PositionCommentaryJobProtocol.Variant,
      "current_fen" -> completed.currentFen,
      "focus" -> PositionCommentaryJobProtocol.focusJson(
        completed.playedMoveUci,
        completed.resultingFen
      ),
      "progress" -> PositionCommentaryJobProtocol.completedProgressJson(completed.progress),
      "result" -> completedResultJson(completed)
    ) ++ PositionCommentaryJobProtocol.verificationJson(completed.verification) ++
      Option.when(completed.verification.nonEmpty || completed.inquiries.nonEmpty || completed.responseInquiries.nonEmpty || completed.continuationInquiries.nonEmpty ||
      completed.proposalEntries.nonEmpty || completed.assessmentNeedsRefresh || completed.assessmentScope != "root_search")(Json.obj(
      "assessment_state" -> (if completed.assessmentNeedsRefresh then "needs_refresh" else "current"),
      "assessment_scope" -> completed.assessmentScope)).getOrElse(Json.obj()) ++
      completed.observationsPacket.map(packet => Json.obj("observations" ->
        observationsJson(packet, completed.inquiries, completed.pvComparisons,
          completed.responseInquiries, completed.preparedPaths, completed.responseSearches,
          completed.continuationReadings, completed.continuationInquiries, completed.rootSearches,
          completed.proposalEntries, completed.localExpansionsUsed, completed.discoveryState)))
      .getOrElse(Json.obj())

  private[runtime] def failure(requestId: Option[String], httpStatus: Int, error: String): Result =
    Result(
      httpStatus,
      Json.obj(
        "schema_version" -> ResponseSchema,
        "status" -> "error",
        "error" -> error
      ) ++ requestId.map(value => Json.obj("request_id" -> value)).getOrElse(Json.obj())
    )

  private object MoveMeaningProjection:
    def moveCommentaryJson(packet: EvidenceBackedJudgmentPacket): JsObject =
      packet.primary.map(primaryJson(packet, _)).getOrElse(Json.obj())

    private def primaryJson(packet: EvidenceBackedJudgmentPacket, primary: PlayerFacingPrimary): JsObject =
      val comparisonEvidence = primary match
        case PlayerFacingPrimary.MoveVerdict(ref) => ref
        case PlayerFacingPrimary.BestChoice(ref)  => ref
      val comparisonRecord =
        packet.evidenceGraph
          .candidateComparisonRecord(comparisonEvidence)
          .getOrElse(
            throw IllegalStateException(
              s"R-selected primary comparison is not registered: ${comparisonEvidence.id}"
            )
          )
      val fact = comparisonRecord.payload match
        case CandidateComparisonEvidence(value) => value
        case _ =>
          throw IllegalStateException(
            s"R-selected primary is not a candidate comparison: ${comparisonEvidence.id}"
          )
      val reference = packet.candidateLines.find(_.ref == fact.referenceLine).getOrElse(
        throw IllegalStateException("R-selected reference endpoint is not in the packet")
      )
      val candidate = packet.candidateLines.find(_.ref == fact.candidateLine).getOrElse(
        throw IllegalStateException("R-selected candidate endpoint is not in the packet")
      )
      val comparison = fact.comparison
      val verdictConfidence = fact.verdictConfidence match
        case VerdictConfidence.EngineBacked        => "engine_backed"
        case VerdictConfidence.LegalReplayVerified => "legal_replay_verified"
        case VerdictConfidence.MixedVerified       => "mixed_verified"
      val verdictCode = comparison.verdict match
        case MoveChoiceVerdict.ImprovesOnReference => "improves_on_reference"
        case MoveChoiceVerdict.MatchesReference    => "matches_reference"
        case MoveChoiceVerdict.PlayableLoss        => "playable_loss"
        case MoveChoiceVerdict.Inaccuracy          => "inaccuracy"
        case MoveChoiceVerdict.Mistake             => "mistake"
        case MoveChoiceVerdict.Blunder             => "blunder"
      val base = primary match
        case PlayerFacingPrimary.MoveVerdict(_) =>
          Json.obj(
            "primary" -> Json.obj(
              "kind" -> "move_verdict",
              "comparison_evidence_id" -> comparisonEvidence.id,
              "verdict_code" -> verdictCode,
              "verdict_confidence" -> verdictConfidence,
              "mover" -> (if comparison.mover.white then "white" else "black"),
              "delta" -> comparisonDeltaJson(comparison),
              "reference_endpoint" -> endpointJson(reference.evaluation, comparison.mover),
              "played_endpoint" -> endpointJson(candidate.evaluation, comparison.mover)
            )
          )
        case PlayerFacingPrimary.BestChoice(_) =>
          Json.obj(
            "primary" -> Json.obj(
              "kind" -> "best_choice",
              "comparison_evidence_id" -> comparisonEvidence.id,
              "runner_up_verdict_code" -> verdictCode,
              "verdict_confidence" -> verdictConfidence,
              "mover" -> (if comparison.mover.white then "white" else "black"),
              "delta" -> comparisonDeltaJson(comparison),
              "best_endpoint" -> endpointJson(reference.evaluation, comparison.mover),
              "runner_up_endpoint" -> endpointJson(candidate.evaluation, comparison.mover)
            )
          )
      base

    def referenceLineInsightJson(commentary: JsObject): JsObject =
      val primary = (commentary \ "primary").as[JsObject]
      val endpoint = (primary \ "reference_endpoint").asOpt[JsObject]
        .orElse((primary \ "best_endpoint").asOpt[JsObject])
        .getOrElse(throw IllegalStateException("focused review has no reference endpoint"))
      Json.obj("endpoint" -> endpoint)

    def positionActionJson(action: PlayerFacingPositionAction): JsObject =
      action match
        case PlayerFacingPositionAction.AutomaticTerminal(terminal) =>
          Json.obj(
            "kind" -> "automatic_terminal",
            "terminal" -> automaticTerminalJson(terminal)
          )
        case PlayerFacingPositionAction.ForcedSingleMove(moveUci, mover, supportingEvaluation, drawClaims) =>
          Json.obj(
            "kind" -> "forced_single_move",
            "move_uci" -> EvidenceRef.normalizeMove(moveUci),
            "supporting_endpoint" -> endpointJson(supportingEvaluation, mover)
          ) ++ Option.when(drawClaims.nonEmpty)(
            Json.obj("draw_claims" -> drawClaims.map(drawClaimJson))
          ).getOrElse(Json.obj())
        case PlayerFacingPositionAction.DominantDrawClaim(claims) =>
          Json.obj(
            "kind" -> "draw_claim_action",
            "claims" -> claims.map(drawClaimJson)
          )

    def drawClaimJson(claim: DrawClaimAction): JsObject =
      claim match
        case DrawClaimAction.AvailableNow(rule) =>
          Json.obj(
            "rule" -> drawClaimRule(rule),
            "availability" -> "available_now"
          )
        case DrawClaimAction.AvailableByDeclaredMoves(rule, moveUcis) =>
          Json.obj(
            "rule" -> drawClaimRule(rule),
            "availability" -> "available_by_declared_move",
            "move_ucis" -> moveUcis.toList.map(EvidenceRef.normalizeMove).sorted
          )

    private def comparisonDeltaJson(comparison: EvalComparison): JsObject =
      val base = Json.obj(
        "candidate_win_percent_delta_for_mover" -> comparison.candidateWinPercentDeltaForMover
      )
      comparison.detail match
        case CandidateComparisonDeltaDetail.EngineEvaluation(_, _) =>
          base ++ Json.obj("kind" -> "engine_evaluation")
        case CandidateComparisonDeltaDetail.OutcomeOnly =>
          base ++ Json.obj("kind" -> "outcome_only")

    private def endpointJson(evaluation: CandidateLineEvaluation, mover: chess.Color): JsObject =
      evaluation match
        case CandidateLineEvaluation.EngineSearch(line) =>
          Json.obj(
            "kind" -> "engine_search",
            "moves" -> line.moves.map(EvidenceRef.normalizeMove),
            "win_percent_for_mover" -> evaluation.winPercentForMover(mover),
            "depth" -> line.depth
          ) ++ line.mate
            .map(value => Json.obj("mate_forecast_white_pov" -> value))
            .getOrElse(Json.obj())
        case CandidateLineEvaluation.ExactAutomaticTerminal(moves, terminal) =>
          Json.obj(
            "kind" -> "exact_automatic_terminal",
            "moves" -> moves.map(EvidenceRef.normalizeMove),
            "terminal" -> automaticTerminalJson(terminal)
          )

    private def automaticTerminalJson(terminal: AutomaticTerminal): JsObject =
      terminal match
        case AutomaticTerminal.Checkmate(winner) =>
          Json.obj(
            "kind" -> "checkmate",
            "winner" -> (if winner.white then "white" else "black")
          )
        case AutomaticTerminal.Stalemate =>
          Json.obj("kind" -> "stalemate")
        case AutomaticTerminal.InsufficientMaterial =>
          Json.obj("kind" -> "insufficient_material")
        case AutomaticTerminal.FivefoldRepetition =>
          Json.obj("kind" -> "fivefold_repetition")
        case AutomaticTerminal.SeventyFiveMoveRule =>
          Json.obj("kind" -> "seventy_five_move_rule")

    private def drawClaimRule(rule: DrawClaimRule): String =
      rule match
        case DrawClaimRule.ThreefoldRepetition => "threefold_repetition"
        case DrawClaimRule.FiftyMoveRule        => "fifty_move_rule"
  private object InputLimits:
    private val MaxFenLength = 128
    private val MaxVariations = 16
    private val MaxMovePrefix = 600
    private val MaxPly = 1000
    private val RequestId = "[A-Za-z0-9._:-]{1,128}".r

    def validRequestId(value: String): Boolean = RequestId.matches(value)

    def accepts(raw: RawMoveReviewInput): Boolean =
      raw.fen.nonEmpty && raw.fen.length <= MaxFenLength &&
        validUci(raw.playedMoveUci) &&
        raw.variations.size <= MaxVariations &&
        raw.movePrefixUci.size <= MaxMovePrefix && raw.movePrefixUci.forall(validUci) &&
        raw.ply.forall(ply => ply >= 0 && ply <= MaxPly) &&
        raw.variations.forall(validLine)

    private def validLine(line: EngineLine): Boolean =
      EngineLineAdmission.acceptsRuntimeInputLine(line)

    private def validUci(value: String): Boolean = EngineLineAdmission.acceptsRuntimeInputUci(value)
