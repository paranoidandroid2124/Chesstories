package io.chesstory.runtime

import java.nio.charset.StandardCharsets.UTF_8
import java.security.MessageDigest
import play.api.libs.json.*

/** Research transport only: uses the same reducer and serializer as the player job. */
object FullGameProbe:
  def main(args: Array[String]): Unit =
    var state: Option[CommentaryJobState] = None
    var requestId = ""
    var deadline = 0L
    var jobId = ""
    scala.io.Source.stdin.getLines().foreach { line =>
      val result = try
        val input = Json.parse(line).as[JsObject]
        val now = System.currentTimeMillis()
        (input \ "command").as[String] match
          case "begin" =>
            val body = (input \ "request").as[JsObject]
            val request = PositionCommentaryJobProtocol.parseJobRequest(Json.stringify(body).getBytes(UTF_8))
              .fold(error => throw IllegalArgumentException(error.toString), identity)
            requestId = request.requestId
            jobId = MessageDigest.getInstance("SHA-256").digest(requestId.getBytes(UTF_8))
              .map(byte => f"${byte & 0xff}%02x").mkString.take(32)
            deadline = now + request.remainingBudgetMs.getOrElse(10000)
            state = Some(CommentaryJobReducer.startJob(request.initialFen, request.movePrefixUci,
              request.currentFen, request.playedMoveUci, request.resultingFen,
              CommentaryJobPolicy(16, deadline, request.engineProfile, investigateChoices = true,
                previewDepth = Some(8), probeThreats = (input \ "probe_threats").asOpt[Boolean].getOrElse(false)),
              now, request.inputLines, request.proposals)
              .fold(error => throw IllegalArgumentException(error.toString), identity))
          case "report" =>
            val body = (input \ "report").as[JsObject]
            val report = PositionCommentaryJobProtocol.parseEngineWorkReport(Json.stringify(body).getBytes(UTF_8))
              .fold(error => throw IllegalArgumentException(error.toString), identity)
            state = Some(CommentaryJobReducer.submitIssuedWorkReport(state.get, report, now)
              .fold(error => throw IllegalArgumentException(error.toString), identity))
          case "expire" => state = state.map(CommentaryJobReducer.expireJob(_, now))
          case other => throw IllegalArgumentException(s"Unknown research command: $other")
        val current = state.get
        val snapshot = PositionCommentaryJobProtocol.projectJobSnapshot(CommentaryJobSnapshot(jobId, requestId, deadline, current))
        val diagnostics = if snapshot.value.contains("issued_engine_work") then Json.obj() else
          val answered = current.proposalEntries.flatMap { entry =>
            entry.request.requirements.collect { case comparison: CommentaryProposal.CompareMoves => comparison }
              .flatMap(comparison => entry.readings.find(_.requirementId == comparison.id).flatMap(_.answer)
                .map(CommentaryIdea.AnsweredComparison(entry.request.id, comparison, _)))
          }.toSet
          val ideas = CommentaryIdea.read(current.proposalEntries)
          val focuses = current.observationsPacket.toList.flatMap { packet =>
            val prepared = current.preparedPaths.getOrElse(ObservedLineProtocol.prepare(packet))
            ideas.flatMap(_.questionFocus(packet, prepared, answered))
          }.distinct
          Json.obj("proposal_count" -> current.proposalEntries.size,
            "answered_comparison_count" -> answered.size,
            "idea_states" -> ideas.map(idea => Json.obj("id" -> idea.id, "state" -> idea.state,
              "contribution" -> idea.contribution, "mechanism" -> idea.mechanism)),
            "mechanism_focuses" -> focuses.map(focus => Json.obj(
              "proposal_id" -> focus.owner.proposalId, "requirement_id" -> focus.owner.comparison.id,
              "prefix" -> focus.owner.comparison.prefixMovesUci, "branch" -> focus.key.branch,
              "reference_move" -> focus.owner.comparison.referenceMove,
              "alternative_move" -> focus.owner.comparison.alternativeMove,
              "reference_line_id" -> focus.owner.answer.referenceLineId,
              "alternative_line_id" -> focus.owner.answer.alternativeLineId,
              "condition" -> focus.mechanism.after.predicate.json,
              "action" -> focus.key.action.json)))
        Json.obj("snapshot" -> snapshot, "research_diagnostics" -> diagnostics)
      catch
        case error: Exception => Json.obj("research_error" -> error.toString, "request_id" -> requestId)
      println(Json.stringify(result))
      Console.out.flush()
    }
