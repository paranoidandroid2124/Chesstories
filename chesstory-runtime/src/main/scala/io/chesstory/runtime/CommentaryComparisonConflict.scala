package io.chesstory.runtime

import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.CandidateLineEvaluation
import CommentaryProposal.*

/** Contradictions between owned comparisons, derived from their receipts rather than stored scores. */
private[runtime] object CommentaryComparisonConflict:
  val RequirementId = "resolve_comparison_conflict"

  case class Conflict(prefix: List[String], move: String, depth: Int, workIds: List[String],
      comparison: Option[CompareMoves]):
    def affects(value: CompareMoves, answer: CommentaryInvestigation.Answer): Boolean =
      value.prefixMovesUci == prefix && Set(value.referenceMove, value.alternativeMove)(move) &&
        workIds.contains(answer.workId)

    def entry: Option[Entry] = comparison.map { value =>
      val id = CommentarySelection.stableId("comparison-conflict", prefix ++ List(move) ++
        value.prefixMovesUci ++ List(value.referenceMove, value.alternativeMove))
      val source = Source("generator", "C-comparison-conflict", sourceRefs = workIds)
      val request = Request(id, Origin(source, source, Source("none", "existing-verification"), source), List(value))
      Entry(request, List(Reading(value.id)), "generated", List(GeneratedArm(id, value.prefixMovesUci,
        value.referenceMove, Some(value.alternativeMove), List(value.id), List(value.id), source, workIds)),
        Some(s"Two comparisons disagree materially on $move after ${prefix.mkString(" ")}. " +
          s"At their first divergent continuation, after ${value.prefixMovesUci.mkString(" ")}, " +
          s"how strong is ${value.alternativeMove} relative to ${value.referenceMove}? " +
          "This local answer does not settle the earlier move's evaluation; its comparison and the starting assessment still need verification."))
    }

  private def materialDifference(left: CandidateLineEvaluation, right: CandidateLineEvaluation): Boolean =
    val mates = List(left, right).map(_.engineLine.flatMap(_.mate).map(Integer.signum))
    if mates.head.nonEmpty && mates.head == mates.last then false
    else mates.head != mates.last || List(left -> right, right -> left).exists { (a, b) =>
      EvalComparison.fromEvaluations(chess.White, a, b).exists(value =>
        Set(MoveChoiceVerdict.Inaccuracy, MoveChoiceVerdict.Mistake, MoveChoiceVerdict.Blunder)(value.verdict))
    }

  def read(packet: EvidenceBackedJudgmentPacket, searches: List[TargetResponseInvestigation.Search],
      requiredDepth: Int): List[Conflict] =
    // Repeated searches of the same pool supersede its older answer. Different histories,
    // profiles or achieved depths are not treated as repeated measurements of one position.
    def history(search: TargetResponseInvestigation.Search) =
      (search.work.enginePositionInitialFen, search.work.enginePositionMovesUci, search.work.engineProfile)
    val current = searches.filter(search => search.belongsTo(packet) &&
      search.lines.forall(_.evaluation.engineLine.forall(_.depth >= requiredDepth)))
      .reverse.foldLeft(List.empty[TargetResponseInvestigation.Search]) { (newer, search) =>
        if newer.exists(value => history(value) == history(search) && search.lines.forall(line =>
            value.lines.exists(fresh => fresh.evaluation.moves.head == line.evaluation.moves.head &&
              fresh.evaluation.engineLine.map(_.depth).getOrElse(Int.MaxValue) >=
                line.evaluation.engineLine.map(_.depth).getOrElse(Int.MaxValue)))) then newer
        else search :: newer
      }
    current.groupBy(search => (search.work.enginePositionInitialFen, search.work.enginePositionMovesUci,
      search.work.engineProfile, search.anchor.prefixMoves)).values.toList.flatMap { group =>
      group.zipWithIndex.flatMap { (left, index) => group.drop(index + 1).flatMap { right =>
        left.lines.flatMap { a => right.lines.find(_.evaluation.moves.head == a.evaluation.moves.head).flatMap { b =>
          val first = a.evaluation
          val second = b.evaluation
          Option.when(first.engineLine.exists(line => second.engineLine.exists(_.depth == line.depth)) &&
              materialDifference(first, second)) {
            val shared = first.moves.zip(second.moves).takeWhile((a, b) => a == b).map(_._1)
            val at = left.anchor.prefixMoves ++ shared
            val pair = for
              reference <- first.moves.lift(shared.size)
              alternative <- second.moves.lift(shared.size)
            yield CompareMoves(RequirementId, at, reference, alternative)
            Conflict(left.anchor.prefixMoves, first.moves.head, first.engineLine.get.depth,
              List(left.work.workId, right.work.workId), pair)
          }
        }}
      }}
    }.sortBy(value => (value.prefix.size, value.prefix.mkString(" "), value.move, value.workIds.mkString(" ")))

  /** A root recheck cannot close the obligation while it still chooses a reply that this
    * exact owned comparison found materially inferior. A different earlier branch is allowed.
    */
  def unansweredChallenge(packet: EvidenceBackedJudgmentPacket, searches: List[TargetResponseInvestigation.Search],
      entries: List[Entry]): Option[Conflict] = entries.iterator.filter(entry => entry.admission == "generated" &&
      Set("C-comparison-conflict", "C-tactical-counter")(entry.request.origin.question.producer)).flatMap { entry =>
    entry.request.requirements.iterator.collect {
      case value: CompareMoves if value.prefixMovesUci.nonEmpty => value
    }.flatMap { value =>
      for
        answer <- entry.readings.iterator.filter(_.requirementId == value.id).flatMap(_.answer)
        search <- searches.iterator.filter(search => search.belongsTo(packet) && search.work.workId == answer.workId)
        root <- packet.assembly.input.assessmentCandidates.iterator.map(_.evaluation)
          .filter(_.moves.startsWith(value.prefixMovesUci))
        move <- root.moves.lift(value.prefixMovesUci.size).iterator
        chosen <- search.lines.find(_.evaluation.moves.head == move).iterator
        best <- search.lines.headOption.iterator
        if best.evaluation != chosen.evaluation && materialDifference(best.evaluation, chosen.evaluation)
      yield Conflict(value.prefixMovesUci, move, best.evaluation.engineLine.map(_.depth).getOrElse(search.work.requiredDepth),
        List(search.work.workId), None)
    }
  }.take(1).toList.headOption
