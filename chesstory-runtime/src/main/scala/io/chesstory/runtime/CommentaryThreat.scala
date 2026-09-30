package io.chesstory.runtime

import lila.chessjudgment.model.judgment.EvidenceBackedJudgmentPacket
import lila.chessjudgment.model.line.{CanonicalPositionHistory, PositionRuleAssessment}

/** A passed turn suggests a question. Only a real legal branch can answer it. */
private[runtime] object CommentaryThreat:
  def hypothesis(origin: CanonicalPositionHistory): Option[CanonicalPositionHistory] =
    if origin.currentPosition.check.yes || PositionRuleAssessment.assess(origin).isInstanceOf[PositionRuleAssessment.Terminal] then None
    else
      val fields = origin.currentFen.split(" ")
      val fullmove = fields(5).toInt.toLong + (if fields(1) == "b" then 1 else 0)
      if fullmove > Int.MaxValue then None
      else
        fields(1) = if fields(1) == "w" then "b" else "w"
        fields(3) = "-"
        fields(5) = fullmove.toString
        val fen = fields.mkString(" ")
        // No fictional move, repetition history, or extra game-clock tick enters the real history.
        CanonicalPositionHistory.from(fen, Nil, fen).toOption.filter(_.currentLegalMoveUcis.nonEmpty)

  def proposals(packet: EvidenceBackedJudgmentPacket, origin: List[String], candidates: List[String],
      workId: String): List[CommentaryProposal.Entry] =
    import CommentaryProposal.*
    val history = packet.assembly.input.positionHistory
    val rootMoves = CommentarySelection.primaryComparisonRecords(packet).flatMap { (_, comparison, _) =>
      List(comparison.referenceLine.rootMove, comparison.candidateLine.rootMove)
    }.toSet
    val paths = packet.assembly.input.assessmentCandidates.map(_.evaluation.moves)
      .filter(moves => moves.headOption.exists(rootMoves) && moves.startsWith(origin) &&
        moves.size > origin.size + 1)
    val source = Source("generator", "pass-probe", Some("1"), List(workId))
    paths.distinctBy(_.take(origin.size + 1)).flatMap { line =>
      val prefix = line.take(origin.size + 1)
      val reference = line(prefix.size)
      history.extend(prefix).toOption.filter(at =>
        PositionRuleAssessment.assess(at).isInstanceOf[PositionRuleAssessment.Nonterminal]).toList.flatMap { at =>
        candidates.filter(move => move != reference && at.currentLegalMoveUcis.contains(move)).map { candidate =>
          val id = CommentarySelection.stableId("threat-question", prefix ++ List(reference, candidate))
          val requirements = List(ObserveMove("candidate", prefix, candidate),
            CompareMoves("comparison", prefix, reference, candidate))
          val request = Request(id, Origin(source, source, Source("none", "existing-agenda"),
            Source("generator", "canonical-board")), requirements)
          Entry(request, requirements.map(value => Reading(value.id)), "generated",
            List(GeneratedArm(id, prefix, reference, Some(candidate), requirements.map(_.id),
              requirements.map(_.id), source, List(workId))),
            Some("Does this tactical candidate still work after the actual reply?"))
        }
      }
    }
