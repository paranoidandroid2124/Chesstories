package io.chesstory.runtime

import scala.util.Try

import lila.chessjudgment.analysis.assembly.{
  MoveReviewJudgmentOrchestrator,
  RawMoveReviewInput,
  RawOpeningContext
}
import lila.chessjudgment.analysis.opening.{ OpeningRecognitionIndex, OpeningThemePriorIndex }
import lila.chessjudgment.model.{ EndgameTablebaseEvidence, ProbeHorizon, ProbePurpose, ProbeRequest, ProbeResult }
import lila.chessjudgment.model.evaluation.PerspectiveMath
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.strategic.EngineLine
import play.api.libs.json.*

object RuntimeProtocol:
  val RequestSchema = "chesstory.move-meaning.request.v1"
  val ResponseSchema = "chesstory.move-meaning.response.v2"

  final case class Result(httpStatus: Int, body: JsObject)

  private final case class WireVariation(
      moves: List[String],
      scoreCp: Int,
      mate: Option[Int],
      depth: Int
  ):
    def toCore: EngineLine = EngineLine(moves, scoreCp, mate, depth)

  private object WireVariation:
    given Reads[WireVariation] = Json.reads[WireVariation]

  private final case class WireProbeResult(
      id: String,
      fen: String,
      replyLines: Option[List[WireVariation]],
      purpose: Option[ProbePurpose],
      probedMove: Option[String],
      depth: Option[Int],
      candidateMove: Option[String],
      opponentResourceMove: Option[String],
      horizon: Option[String],
      variationHash: Option[String],
      generatedAtEpochMs: Option[Long],
      tablebase: Option[EndgameTablebaseEvidence]
  ):
    def toCore: ProbeResult =
      ProbeResult(
        id = id,
        fen = Some(fen),
        replyLines = replyLines.map(_.map(_.toCore)),
        purpose = purpose,
        probedMove = probedMove,
        depth = depth,
        candidateMove = candidateMove,
        opponentResourceMove = opponentResourceMove,
        horizon = horizon,
        variationHash = variationHash,
        generatedAtEpochMs = generatedAtEpochMs,
        tablebase = tablebase
      )

  private object WireProbeResult:
    given Reads[WireProbeResult] = Json.reads[WireProbeResult]

  private final case class WireInput(
      fen: String,
      playedMoveUci: String,
      variations: List[WireVariation],
      ply: Option[Int],
      openingContext: Option[RawOpeningContext],
      movePrefixUci: Option[List[String]],
      probeResults: Option[List[WireProbeResult]]
  ):
    def toCore: RawMoveReviewInput =
      RawMoveReviewInput(
        fen = fen,
        playedMoveUci = playedMoveUci,
        variations = variations.map(_.toCore),
        ply = ply,
        openingContext = openingContext,
        movePrefixUci = movePrefixUci.getOrElse(Nil),
        probeResults = probeResults.getOrElse(Nil).map(_.toCore)
      )

  private object WireInput:
    given Reads[WireInput] = Json.reads[WireInput]

  def evaluate(bytes: Array[Byte]): Result =
    Try(Json.parse(bytes)).fold(_ => failure(None, 400, "invalid_json"), evaluate)

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

  def publicProbeRequestJson(request: ProbeRequest): JsObject =
    Json.obj(
      "id" -> request.id,
      "fen" -> request.fen,
      "moves" -> request.moves,
      "depth" -> request.depth,
      "purpose" -> request.purpose,
      "multiPv" -> request.multiPv,
      "baselineEvalCp" -> request.baselineEvalCp,
      "horizon" -> request.horizon,
      "candidateMove" -> request.candidateMove,
      "depthFloor" -> request.depthFloor,
      "variationHash" -> request.variationHash,
      "comparisonFen" -> request.comparisonFen
    ) ++ Option
      .when(request.opponentResourceMove.nonEmpty)(Json.obj("opponentResourceMove" -> request.opponentResourceMove))
      .getOrElse(Json.obj())

  private def evaluate(requestId: Option[String], raw: RawMoveReviewInput): Result =
    if !InputLimits.accepts(raw) then failure(requestId, 400, "input_limits_exceeded")
    else
      MoveReviewJudgmentOrchestrator.packet(raw) match
        case None => failure(requestId, 422, "move_review_not_buildable")
        case Some(packet) =>
          val primaryEngineBacked = MoveMeaningProjection.engineBacked(packet)
          val fullPayload =
            Option.when(primaryEngineBacked)(MoveMeaningProjection.publicPayloadJson(packet))
          val exactPayload = MoveMeaningProjection.exactPayloadJson(packet)
          val hasExactProof = (exactPayload \\ "exact_proof").nonEmpty
          val hasExplanation =
            fullPayload.exists(payload =>
              (payload \ "explanations").asOpt[JsArray].exists(_.value.nonEmpty)
            )
          val hasEngineMateOutcome =
            fullPayload.exists(payload =>
              (payload \ "verdict" \ "outcome" \ "state").asOpt[String].exists(state =>
                state == "forced_win" || state == "forced_loss"
              )
            )
          val primaryRenderable =
            primaryEngineBacked && (hasExplanation || hasEngineMateOutcome)
          val renderable = primaryRenderable || hasExactProof
          val payload =
            if primaryRenderable then fullPayload.get
            else if hasExactProof then exactPayload
            else MoveMeaningProjection.emptyPayloadJson
          val moveReview = Json.obj("renderable" -> renderable) ++ payload
          val status = if renderable then "ready" else "withheld"
          val reason =
            if !primaryEngineBacked && !hasExactProof then Some("insufficient_engine_depth")
            else if !renderable then Some("no_player_facing_reason")
            else None
          val probeRequests = packet.probeRequests.map(publicProbeRequestJson)
          Result(
            200,
            Json.obj(
              "schema_version" -> ResponseSchema,
              "request_id" -> requestId,
              "ok" -> true,
              "status" -> status,
              "availability" -> Json.obj("state" -> status, "reason" -> reason),
              "probe_requests" -> probeRequests,
              "move_review" -> moveReview
            )
          )

  private[runtime] def failure(requestId: Option[String], httpStatus: Int, error: String): Result =
    Result(
      httpStatus,
      Json.obj(
        "schema_version" -> ResponseSchema,
        "request_id" -> requestId,
        "ok" -> false,
        "status" -> "error",
        "error" -> error
      )
    )

  private object MoveMeaningProjection:
    private final case class PrimaryComparison(
        assessment: RelativeMoveAssessment,
        fact: CandidateComparisonFact
    )

    def engineBacked(packet: EvidenceBackedJudgmentPacket): Boolean =
      primaryComparison(packet).exists { primary =>
        packet.evidenceGraph.candidateComparisonRecord(primary.assessment.primaryComparisonEvidence).exists {
          case EvidenceRecord(ref, CandidateComparisonEvidence(fact), _) =>
            ref.confidence == EvidenceConfidence.EngineBacked &&
              fact == primary.fact
          case _ =>
            false
        }
      }

    def publicPayloadJson(packet: EvidenceBackedJudgmentPacket): JsObject =
      val claims = packet.claims
      val graph = packet.evidenceGraph
      val primary = primaryComparison(packet)
      val assessment = primary.map(_.assessment)
      val playedLine = packet.candidateLine(LineNodeRole.Played)
      val referenceLine = packet.candidateLine(LineNodeRole.BestReference)
      val playedMove =
        assessment.map(_.played.moveUci).orElse(playedLine.map(_.ref.rootMove)).getOrElse("")
      val referenceMove =
        assessment.map(_.reference.ref.rootMove).orElse(referenceLine.map(_.ref.rootMove)).getOrElse("")
      val moveQuality = primary.map(value => quality(value.fact.comparison.verdict)).getOrElse("unknown")
      val registeredClaims =
        claims.flatMap(claim => registeredClosure(claim, graph).map(records => claim -> records))
      val playedClaims = registeredClaims
        .filter { case (claim, records) =>
          val tier = PlayerFacingClaimPolicy.tier(packet, claim)
          claim.family != ClaimFamily.Evaluation &&
          (tier == PlayerFacingClaimTier.Primary || tier == PlayerFacingClaimTier.Secondary) &&
          explainsPlayedMove(claim, records, graph, playedMove)
        }
        .distinctBy(_._1.id)
      val ideas = playedClaims.map { case (claim, records) =>
        ideaJson(claim, records, graph, playedMove, packet.candidateLines)
      }
      val causeKinds = playedClaims
        .flatMap { case (_, records) => causes(records, graph, playedMove).map(_._1.kind) }
        .distinct
        .map(value => code(value.toString))
      val line = playedLine
        .filter(node => EvidenceRef.sameMove(node.ref.rootMove, playedMove))
        .map(_.line.moves)
        .getOrElse(Nil)
      val explanation =
        Option.when(playedMove.nonEmpty && ideas.nonEmpty)(
          Json.obj(
            "role" -> "played move",
            "move" -> playedMove,
            "reference_move" -> referenceMove,
            "move_quality" -> moveQuality,
            "ideas" -> ideas,
            "chess_relations" -> causeKinds,
            "line" -> line
          )
        )
      val resolvedPlans = playedClaims
        .flatMap { case (claim, records) => planEvents(claim, records, graph) }
        .distinctBy(event => (event.planId.id, event.identity.stableKey, event.rootLine.id))
        .map(event => ResolvedPlanEvent.from(event, packet.candidateLines, packet.evidenceGraph))
      val exactTechnique =
        exactEndgameTechniques(
          graph.records.filter(record =>
            record.ref.line.exists(line =>
              line.role == LineNodeRole.Played &&
                EvidenceRef.sameMove(line.rootMove, playedMove)
            )
          )
        )
      val claimedExactTechniqueKeys =
        playedClaims
          .flatMap { case (_, records) => exactEndgameTechniques(records) }
          .map(jsonKey)
          .toSet
      val unclaimedExactTechniques =
        exactTechnique.filterNot(technique => claimedExactTechniqueKeys(jsonKey(technique)))
      Json.obj(
        "verdict" -> primary.map(value => verdictJson(value.assessment, value.fact)),
        "explanations" -> explanation.toList,
        "tested_plan_limits" -> resolvedPlans
          .filter(hasTestedLimit)
          .map(event =>
            Json.obj(
              "role" -> planRole(event.rootMove, playedMove, referenceMove),
              "move" -> event.rootMoveReference.map(numberedMoveJson).getOrElse(Json.obj("uci" -> event.rootMove)),
              "plan" -> planJson(event)
            )
          )
      ) ++
        Option
          .when(unclaimedExactTechniques.nonEmpty)(
            Json.obj("exact_endgame_techniques" -> unclaimedExactTechniques)
          )
          .getOrElse(Json.obj())

    def emptyPayloadJson: JsObject =
      Json.obj(
        "verdict" -> JsNull,
        "explanations" -> Json.arr(),
        "tested_plan_limits" -> Json.arr()
      )

    def exactPayloadJson(packet: EvidenceBackedJudgmentPacket): JsObject =
      val playedMove =
        packet.primaryRelativeAssessment
          .map(_.played.moveUci)
          .orElse(packet.candidateLine(LineNodeRole.Played).map(_.ref.rootMove))
          .getOrElse("")
      val techniques =
        exactEndgameTechniques(
          packet.evidenceGraph.records.filter(record =>
            record.ref.line.exists(line =>
              line.role == LineNodeRole.Played &&
                EvidenceRef.sameMove(line.rootMove, playedMove)
            )
          )
        )
      emptyPayloadJson ++
        Option
          .when(techniques.nonEmpty)(
            Json.obj("exact_endgame_techniques" -> techniques)
          )
          .getOrElse(Json.obj())

    private def primaryComparison(
        packet: EvidenceBackedJudgmentPacket
    ): Option[PrimaryComparison] =
      for
        assessment <- packet.primaryRelativeAssessment
        fact <- packet.evidenceGraph.comparisonFor(assessment)
        played <- packet.candidateLine(LineNodeRole.Played)
        reference <- packet.candidateLine(LineNodeRole.BestReference)
        if assessment.candidate == played
        if assessment.reference == reference
        if fact.kind == CandidateComparisonKind.PlayedVsBest
        if fact.referenceLine == reference.ref
        if fact.candidateLine == played.ref
      yield PrimaryComparison(assessment, fact)

    private def verdictJson(
        assessment: RelativeMoveAssessment,
      fact: CandidateComparisonFact
    ): JsObject =
      val comparison = fact.comparison
      val referenceMate = PerspectiveMath.mateForMover(comparison.mover, assessment.reference.mate)
      val playedMate = PerspectiveMath.mateForMover(comparison.mover, assessment.candidate.mate)
      val mateDistanceLoss = PerspectiveMath.mateDistanceLoss(referenceMate, playedMate)
      Json.obj(
        "verdict_code" -> code(comparison.verdict.toString),
        "move_quality" -> quality(comparison.verdict),
        "played_move" -> assessment.played.moveUci,
        "reference_move" -> assessment.reference.ref.rootMove,
        "outcome" -> playedMate.map(mate =>
          Json.obj(
            "state" -> mateState(mate),
            "certainty" -> "engine_mate_score",
            "reference_state" -> referenceMate.map(mateState),
            "resistance" -> resistance(referenceMate, playedMate, mateDistanceLoss)
          )
        ),
        "mate" -> Option.when(referenceMate.nonEmpty || playedMate.nonEmpty)(
          Json.obj(
            "reference_for_mover" -> referenceMate,
            "played_for_mover" -> playedMate,
            "distance_loss" -> mateDistanceLoss
          )
        )
      )

    private def ideaJson(
        claim: JudgmentClaim,
        records: List[EvidenceRecord],
        graph: TypedEvidenceGraph,
        playedMove: String,
        lines: List[CandidateLineNode]
    ): JsObject =
      val causeEntries = causes(records, graph, playedMove)
      val planEvidence = planEvents(claim, records, graph)
      val plans = planEvidence.map(event => ResolvedPlanEvent.from(event, lines, graph)).distinct
      val techniques = exactEndgameTechniques(records)
      val semanticNames =
        (
          causeEntries.map(entry => label(entry._1.kind.toString)) ++
            planEvidence.map(event => label(event.identity.goalKey)) ++
            techniques.flatMap(json => (json \ "pattern").asOpt[String])
        ).distinct
      val semanticName =
        semanticNames match
          case name :: Nil => name
          case _           => label(claim.family.toString)
      Json.obj(
        "id" -> claim.id,
        "kind" -> code(claim.family.toString),
        "name" -> semanticName,
        "subject" -> code(claim.subject.toString),
        "scope" -> code(claim.scope.toString),
        "confidence" -> code(claim.confidence.toString),
        "target" -> targetJson(claim, graph),
        "evidence" -> Json.obj(
          "layers" -> records.map(record => code(record.ref.layer.toString)).distinct.sorted,
          "scopes" -> records.map(record => code(record.ref.scope.toString)).distinct.sorted,
          "causes" -> causeEntries.map(entry => code(entry._1.kind.toString)).distinct
        ),
        "plans" -> plans.map(planJson),
        "endgame_techniques" -> techniques
      )

    private def registeredClosure(
        claim: JudgmentClaim,
        graph: TypedEvidenceGraph
    ): Option[List[EvidenceRecord]] =
      val roots = claim.evidence.flatMap(ref => graph.byId.get(ref.id))
      Option.when(claim.evidence.nonEmpty && roots.size == claim.evidence.size)(
        (roots ++ roots.flatMap(graph.parentClosure)).distinctBy(_.ref.id)
      )

    private def explainsPlayedMove(
        claim: JudgmentClaim,
        records: List[EvidenceRecord],
        graph: TypedEvidenceGraph,
        playedMove: String
    ): Boolean =
      playedMove.nonEmpty && (
        claim.subject == ClaimSubject.PlayedMove ||
          claim.subjectMove.exists(EvidenceRef.sameMove(_, playedMove)) ||
          claim.primaryLine.exists(line =>
            line.role == LineNodeRole.Played && EvidenceRef.sameMove(line.rootMove, playedMove)
          ) ||
          records.exists {
            case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) =>
              graph.comparisonFor(cause).exists(fact =>
                EvidenceRef.sameMove(fact.candidateLine.rootMove, playedMove)
              )
            case EvidenceRecord(_, event: PlanCausalEventEvidence, _) =>
              EvidenceRef.sameMove(event.rootMove, playedMove)
            case _ =>
              false
          }
      )

    private def causes(
        records: List[EvidenceRecord],
        graph: TypedEvidenceGraph,
        playedMove: String
    ): List[(RelativeCauseFact, String)] =
      val all = records.collect {
        case EvidenceRecord(ref, RelativeCauseFactEvidence(cause), _) =>
          (cause, ref.id, graph.requiredRelativeCauseBinding(cause))
      }.distinctBy { case (cause, _, binding) =>
        (cause.kind, binding.role, cause.sourceSide, binding.eventLine.id)
      }
      val playedMoves = Set(EvidenceRef.normalizeMove(playedMove)).filter(_.nonEmpty)
      all
        .filter { case (cause, _, binding) =>
          binding.role == RelativeCauseRole.PrimaryPlayedCause &&
          binding.importance == RelativeCauseImportance.Primary &&
          JudgmentSubjectBinding.relativeCauseBinding(cause, graph, playedMoves) == SubjectBindingClass.PrimaryPlayedCause
        }
        .sortBy(_._1.kind.toString)
        .map { case (cause, refId, _) => cause -> refId }

    private def targetJson(claim: JudgmentClaim, graph: TypedEvidenceGraph): JsObject =
      val targets = EvidenceObjectBinding.fromClaim(claim, graph).flatMap(_.target)
      Json.obj(
        "squares" -> targets.collect {
          case ConcreteChessObject(EvidenceObjectKind.Square, key) => key
        }.distinct.sorted,
        "files" -> targets.collect {
          case ConcreteChessObject(EvidenceObjectKind.File, key) => key
        }.distinct.sorted,
        "pieces" -> targets.collect {
          case ConcreteChessObject(EvidenceObjectKind.Piece, key) => key
          case ConcreteChessObject(EvidenceObjectKind.Pawn, key)  => key
        }.distinct.sorted
      )

    private def planEvents(
        claim: JudgmentClaim,
        records: List[EvidenceRecord],
        graph: TypedEvidenceGraph
    ): List[PlanCausalEventEvidence] =
      val events = records.collect {
        case EvidenceRecord(_, event: PlanCausalEventEvidence, _) =>
          event
      }
      val directRoots =
        (claim.subjectMove.toList ++ claim.primaryLine.map(_.rootMove).toList)
          .map(EvidenceRef.normalizeMove)
          .filter(_.nonEmpty)
      val causeRoots = records.flatMap {
        case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) =>
          causeLinesForSubject(claim.subject, claim.primaryLine, cause, graph).map(_.rootMove)
        case _ =>
          Nil
      }.map(EvidenceRef.normalizeMove).filter(_.nonEmpty)
      val boundRoots = (directRoots ++ causeRoots).distinct
      val admissibleRoots =
        if boundRoots.nonEmpty then boundRoots
        else if claim.subject == ClaimSubject.Plan || claim.subject == ClaimSubject.Position then
          events.map(event => EvidenceRef.normalizeMove(event.rootMove)).distinct
        else Nil
      events
        .filter(event => admissibleRoots.exists(EvidenceRef.sameMove(_, event.rootMove)))
        .distinctBy(event => (event.planId.id, event.identity.stableKey, event.rootLine.id))

    private def causeLinesForSubject(
        subject: ClaimSubject,
        primaryLine: Option[LineNodeRef],
        cause: RelativeCauseFact,
        graph: TypedEvidenceGraph
    ): List[LineNodeRef] =
      val comparisonLines =
        graph.comparisonFor(cause).toList.flatMap(fact => List(fact.referenceLine, fact.candidateLine))
      val eventLine = graph.requiredRelativeCauseBinding(cause).eventLine
      val lines = (eventLine :: comparisonLines).distinct
      val roles = primaryLine.map(_.role).toSet ++ (subject match
        case ClaimSubject.PlayedMove   => Set(LineNodeRole.Played)
        case ClaimSubject.ReferenceMove => Set(LineNodeRole.BestReference)
        case ClaimSubject.CandidateLine => Set(LineNodeRole.Alternative)
        case ClaimSubject.Threat       => Set(LineNodeRole.Threat)
        case ClaimSubject.Plan | ClaimSubject.Position => Set(eventLine.role)
      )
      lines.filter(line => roles(line.role))

    private def planJson(event: ResolvedPlanEvent): JsObject =
      Json.obj(
        "goal" -> Json.obj(
          "theme" -> label(event.goalTheme),
          "kind" -> event.goalKind.map(label)
        ),
        "move" -> event.rootMoveReference.map(numberedMoveJson).getOrElse(Json.obj("uci" -> event.rootMove)),
        "move_role" -> event.moveRole.map(value => label(value.toString)),
        "transition" -> event.transitionType.map(value => label(value.toString)),
        "actor" -> Json.obj(
          "piece" -> event.actorRole,
          "from" -> event.actorFrom,
          "to" -> event.actorTo
        ),
        "targets" -> event.targets,
        "results" -> event.results.map(result =>
          Json.obj(
            "stage" -> result.stage,
            "kind" -> code(result.kind.toString),
            "direction" -> code(result.polarity.toString),
            "subjects" -> result.subjects,
            "move" -> result.source.map(numberedMoveJson),
            "tested_reply_status" -> result.robustness.map(value => code(value.toString))
          )
        ),
        "tested_continuation" -> event.testedContinuation.map(continuation =>
          Json.obj(
            "next_move" -> continuation.sequence
              .find(step => EvidenceRef.sameMove(step.move, continuation.futureMove))
              .flatMap(_.moveReference)
              .map(numberedMoveJson)
              .getOrElse(Json.obj("uci" -> continuation.futureMove)),
            "target" -> continuation.targetSquare,
            "connection" -> code(continuation.dependencyKind.toString),
            "status" -> code(continuation.robustness.toString),
            "successful_replies" -> continuation.realizedReplies,
            "tested_replies" -> continuation.testedReplies,
            "expected_replies" -> continuation.expectedReplies
          )
        )
      )

    private def hasTestedLimit(event: ResolvedPlanEvent): Boolean =
      event.testedContinuation.nonEmpty ||
        event.results.exists(result => result.stage == "future" && result.robustness.nonEmpty)

    private def planRole(rootMove: String, playedMove: String, referenceMove: String): String =
      if EvidenceRef.sameMove(rootMove, playedMove) then "played move"
      else if EvidenceRef.sameMove(rootMove, referenceMove) then "better choice"
      else "variation"

    private def exactEndgameTechniques(records: List[EvidenceRecord]): List[JsObject] =
      records.iterator
        .collect { case EvidenceRecord(ref, line: LineFactEvidence, _) => ref -> line }
        .flatMap { case (ref, line) =>
          line.endgameTechniqueHorizons.flatMap(horizon =>
            horizon.zugzwangProof.flatMap(proof =>
              exactProofJson(proof, horizon.terminalPlyOffset).map(exact =>
                Json.obj(
                  "evidence_id" -> ref.id,
                  "pattern" -> label(horizon.pattern),
                  "status" -> label(horizon.status.toString),
                  "exact_proof" -> exact
                )
              )
            )
          )
        }
        .toList
        .distinctBy(jsonKey)
        .sortBy(jsonKey)

    private def jsonKey(value: JsObject): String =
      Json.stringify(value)

    private def exactProofJson(
        proof: EndgameZugzwangProof,
        terminalPlyOffset: Int
    ): Option[JsObject] =
      val replies = proof.legalReplies.map(reply =>
        NumberedChessMove
          .fromFenAtOffset(proof.terminalFen, reply.moveUci, proof.comparisonFen, terminalPlyOffset + 1)
          .orElse(NumberedChessMove.fromFen(proof.terminalFen, reply.moveUci))
      )
      Option.when(replies.nonEmpty && replies.forall(_.nonEmpty))(
        Json.obj(
          "kind" -> "zugzwang",
          "side_to_move" -> proof.constrainedSideKey,
          "all_legal_moves_lose" -> true,
          "legal_replies" -> replies.flatten.map(numberedMoveJson)
        )
      )

    private def numberedMoveJson(move: NumberedChessMove): JsObject =
      Json.obj(
        "uci" -> move.uci,
        "san" -> move.san,
        "turn" -> Json.obj(
          "move_number" -> move.turn.moveNumber,
          "side" -> move.turn.side,
          "notation" -> move.turn.notation
        ),
        "notation" -> move.notation
      )

    private def quality(verdict: MoveChoiceVerdict): String =
      verdict match
        case MoveChoiceVerdict.ImprovesOnReference | MoveChoiceVerdict.MatchesReference => "good"
        case MoveChoiceVerdict.PlayableLoss                                             => "playable"
        case MoveChoiceVerdict.Inaccuracy | MoveChoiceVerdict.Mistake | MoveChoiceVerdict.Blunder =>
          "bad"

    private def mateState(mate: Int): String =
      if mate > 0 then "forced_win"
      else if mate < 0 then "forced_loss"
      else "unknown"

    private def resistance(
        referenceMate: Option[Int],
        playedMate: Option[Int],
        distanceLoss: Option[Int]
    ): Option[String] =
      referenceMate.zip(playedMate).collect {
        case (reference, played) if reference < 0 && played < 0 && distanceLoss.exists(_ > 0) =>
          "shortened"
        case (reference, played) if reference < 0 && played < 0 && played.abs > reference.abs =>
          "extended"
        case (reference, played) if reference < 0 && played < 0 =>
          "held"
      }

    private def code(value: String): String =
      value.trim
        .replaceAll("([a-z0-9])([A-Z])", "$1_$2")
        .replaceAll("[^A-Za-z0-9]+", "_")
        .replaceAll("_+", "_")
        .stripPrefix("_")
        .stripSuffix("_")
        .toLowerCase

    private def label(value: String): String =
      code(value).replace('_', ' ')

  private object InputLimits:
    private val MaxFenLength = 128
    private val MaxVariations = 16
    private val MaxPvMoves = 80
    private val MaxMovePrefix = 600
    private val MaxProbeResults = 12
    private val MaxReplyLines = 8
    private val MaxTablebaseMoves = 256
    private val MaxDepth = 100
    private val MaxPly = 1000
    private val MaxEval = 100000L
    private val MaxMate = 1000L
    private val MaxText = 256
    private val Uci = "[a-h][1-8][a-h][1-8][nbrq]?".r
    private val RequestId = "[A-Za-z0-9._:-]{1,128}".r

    def validRequestId(value: String): Boolean = RequestId.matches(value)

    def accepts(raw: RawMoveReviewInput): Boolean =
      val lines = raw.variations ++ raw.probeResults.flatMap(_.replyLines.getOrElse(Nil))
      raw.fen.nonEmpty && raw.fen.length <= MaxFenLength &&
        validUci(raw.playedMoveUci) &&
        raw.variations.nonEmpty && raw.variations.size <= MaxVariations &&
        raw.movePrefixUci.size <= MaxMovePrefix && raw.movePrefixUci.forall(validUci) &&
        raw.probeResults.size <= MaxProbeResults &&
        raw.probeResults.map(_.id).distinct.size == raw.probeResults.size &&
        raw.probeResults.forall(validProbe) &&
        raw.ply.forall(ply => ply >= 0 && ply <= MaxPly) &&
        raw.openingContext.forall(validOpening) &&
        lines.forall(validLine)

    private def validLine(line: EngineLine): Boolean =
      line.moves.nonEmpty && line.moves.size <= MaxPvMoves && line.moves.forall(validUci) &&
        bounded(line.scoreCp, MaxEval) && line.mate.forall(mate => mate != 0 && bounded(mate, MaxMate)) &&
        line.depth >= 0 && line.depth <= MaxDepth

    private def validProbe(probe: ProbeResult): Boolean =
      probe.id.nonEmpty && probe.id.length <= MaxText &&
        probe.fen.exists(fen => fen.nonEmpty && fen.length <= MaxFenLength) &&
        probe.replyLines.forall(_.size <= MaxReplyLines) &&
        probe.depth.forall(depth => depth >= 0 && depth <= MaxDepth) &&
        probe.probedMove.forall(validUci) && probe.candidateMove.forall(validUci) &&
        probe.opponentResourceMove.forall(validUci) &&
        probe.horizon.forall(value => value.length <= MaxText && ProbeHorizon.plyOffset(value).nonEmpty) &&
        probe.variationHash.forall(hash => hash.nonEmpty && hash.length <= MaxText) &&
        probe.tablebase.forall(validTablebase)

    private def validTablebase(tablebase: EndgameTablebaseEvidence): Boolean =
      val moves = tablebase.legalMoves
      validTablebasePosition(tablebase.terminal) && validTablebasePosition(tablebase.comparison) &&
        moves.nonEmpty && moves.size <= MaxTablebaseMoves &&
        moves.map(_.moveUci.trim.toLowerCase).distinct.size == moves.size &&
        moves.forall(move => validUci(move.moveUci) && validWdl(move.resultingWdl) && move.resultingDtm.forall(validDtm))

    private def validTablebasePosition(position: lila.chessjudgment.model.TablebasePositionEvidence): Boolean =
      position.fen.nonEmpty && position.fen.length <= MaxFenLength && validWdl(position.wdl) && position.dtm.forall(validDtm)

    private def validWdl(wdl: Int): Boolean = wdl >= -2 && wdl <= 2

    private def validDtm(dtm: Int): Boolean = dtm != 0 && bounded(dtm, MaxMate)

    private def validOpening(opening: RawOpeningContext): Boolean =
      List(opening.eco, opening.name, opening.family).flatten.forall(_.length <= MaxText)

    private def validUci(value: String): Boolean = Uci.matches(value.trim.toLowerCase)

    private def bounded(value: Int, limit: Long): Boolean = value.toLong.abs <= limit

object RuntimeResources:
  private val ThemePriorEntries = 43

  def verify(): Unit =
    require(
      OpeningRecognitionIndex.default.allEntries.nonEmpty,
      "scalachess opening database is empty"
    )
    require(
      OpeningThemePriorIndex.default.allEntries.size == ThemePriorEntries,
      "opening theme-prior resource is missing or incomplete"
    )
