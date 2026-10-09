package io.chesstory.runtime

import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.CandidateLineEvaluation
import CommentaryProposal.*
import TargetResponseInvestigation.Search

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
        value.prefixMovesUci ++ List(value.referenceMove, value.alternativeMove, depth.toString) ++ workIds)
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

  private def completedDepth(search: Search): Option[Int] =
    val depths = search.lines.flatMap(_.evaluation.engineLine.map(_.depth))
    Option.when(depths.nonEmpty && depths.size == search.lines.size && depths.distinct.size == 1)(depths.head)

  private def samePosition(left: Search, right: Search): Boolean =
    left.work.enginePositionInitialFen == right.work.enginePositionInitialFen &&
      left.work.enginePositionMovesUci == right.work.enginePositionMovesUci &&
      left.work.searchFen == right.work.searchFen && left.work.engineProfile == right.work.engineProfile &&
      left.anchor.scopeKey == right.anchor.scopeKey

  /** These two exact receipts measure the same position at the same achieved
    * level. A divergent child is a new question, never a replacement value. */
  def betweenCurrentValues(packet: EvidenceBackedJudgmentPacket, left: Search, right: Search): List[Conflict] =
    if (left eq right) || !left.belongsTo(packet) || !right.belongsTo(packet) ||
        !samePosition(left, right) || completedDepth(left).isEmpty ||
        completedDepth(left) != completedDepth(right) then Nil
    else left.lines.flatMap { a => right.lines.find(_.evaluation.moves.head == a.evaluation.moves.head).flatMap { b =>
      val first = a.evaluation
      val second = b.evaluation
      Option.when(materialDifference(first, second)) {
        val shared = first.moves.zip(second.moves).takeWhile((a, b) => a == b).map(_._1)
        val pair = for
          reference <- first.moves.lift(shared.size)
          alternative <- second.moves.lift(shared.size)
        yield CompareMoves(RequirementId, left.anchor.prefixMoves ++ shared, reference, alternative)
        Conflict(left.anchor.prefixMoves, first.moves.head, completedDepth(left).get,
          List(left.work.workId, right.work.workId), pair)
      }
    }}

  private def invalidatesSelection(packet: EvidenceBackedJudgmentPacket, pool: Search, counter: Search): Boolean =
    counter.work.rootRestriction != EngineRootRestriction.Unrestricted &&
      samePosition(pool, counter) && completedDepth(pool).nonEmpty && completedDepth(pool) == completedDepth(counter) &&
      (betweenCurrentValues(packet, pool, counter).nonEmpty || pool.lines.headOption.exists { leader =>
        counter.lines.exists(line => line.evaluation.moves.head != leader.evaluation.moves.head &&
          counter.pairAnswer(packet, leader.evaluation.moves.head, line.evaluation.moves.head).exists(answer =>
            answer.qualityBand.contains(PrincipalVariationComparison.QualityBand.Separated) &&
              answer.evaluationRelation == "better"))
      })

  private case class Epoch(active: List[(Search, Int)], resets: List[(Search, Int)])

  /** A completed full survey after a real contradiction starts a new selection
    * epoch even when its finite top-N no longer returns an old candidate.
    * The receipts remain historical witnesses; only current value use changes. */
  private def epochs(packet: EvidenceBackedJudgmentPacket, searches: List[Search]): Epoch =
    searches.zipWithIndex.filter((search, _) => search.belongsTo(packet) && completedDepth(search).nonEmpty)
      .foldLeft(Epoch(Nil, Nil)) { case (state, (search, index)) =>
        val refresh = search.work.rootRestriction == EngineRootRestriction.Unrestricted &&
          state.active.exists { (old, at) =>
            old.work.rootRestriction == EngineRootRestriction.Unrestricted && samePosition(old, search) &&
              completedDepth(old).get <= completedDepth(search).get &&
              state.active.exists { (counter, after) =>
                invalidatesSelection(packet, old, counter) &&
                  (after > at || betweenCurrentValues(packet, old, counter).nonEmpty)
              }
          }
        val active = state.active.filterNot { (old, _) =>
          search.work.rootRestriction == EngineRootRestriction.Unrestricted && samePosition(old, search) &&
            completedDepth(old).get <= completedDepth(search).get &&
            (refresh || old.work.rootRestriction == EngineRootRestriction.Unrestricted)
        }
        Epoch(active :+ (search -> index), if refresh then state.resets :+ (search -> index) else state.resets)
      }

  /** Ordinary depth alignment can reuse a matching pair. A resolved earlier
    * contradiction requires an exact pair owned by the new selection epoch. */
  def selectionEpochStart(packet: EvidenceBackedJudgmentPacket, pool: Search, searches: List[Search]): Int =
    val at = searches.indexWhere(_ eq pool)
    epochs(packet, searches).resets.collect {
      case (refresh, index) if index <= at && samePosition(refresh, pool) &&
          completedDepth(refresh).get <= completedDepth(pool).get => index
    }.lastOption.getOrElse(0)

  /** Repeating a named pair cannot erase a selection contradiction. Only a
    * subsequent coherent full survey can close that old epoch. */
  def selectionConflict(packet: EvidenceBackedJudgmentPacket, pool: Search,
      searches: List[Search], moves: Set[String] = Set.empty): Option[Conflict] =
    val active = epochs(packet, searches).active
    active.find((search, _) => search eq pool).flatMap { _ =>
      active.iterator.filter((counter, _) => counter.work.rootRestriction != EngineRootRestriction.Unrestricted).flatMap { (counter, _) =>
        betweenCurrentValues(packet, pool, counter).filter(value => moves.isEmpty || moves(value.move))
      }.take(1).toList.headOption
    }

  def read(packet: EvidenceBackedJudgmentPacket, searches: List[TargetResponseInvestigation.Search],
      requiredDepth: Int): List[Conflict] =
    // Repeated searches of the same pool supersede its older answer. Different histories,
    // profiles or achieved depths are not treated as repeated measurements of one position.
    val active = epochs(packet, searches).active.map(_._1)
      .filter(search => completedDepth(search).exists(_ >= requiredDepth))
    val current = active
      .reverse.foldLeft(List.empty[TargetResponseInvestigation.Search]) { (newer, search) =>
        if newer.exists(value => samePosition(value, search) &&
            ((value.work.rootRestriction == EngineRootRestriction.Unrestricted &&
              search.work.rootRestriction == EngineRootRestriction.Unrestricted &&
              completedDepth(value) == completedDepth(search)) || search.lines.forall(line =>
              value.lines.exists(fresh => fresh.evaluation.moves.head == line.evaluation.moves.head &&
                fresh.evaluation.engineLine.map(_.depth).getOrElse(Int.MaxValue) >=
                  line.evaluation.engineLine.map(_.depth).getOrElse(Int.MaxValue))))) then newer
        else search :: newer
      }
    val measured = current.groupBy(search => (search.work.enginePositionInitialFen, search.work.enginePositionMovesUci,
      search.work.engineProfile, search.anchor.scopeKey)).values.toList.flatMap { group =>
      group.zipWithIndex.flatMap { (left, index) => group.drop(index + 1).flatMap { right =>
        betweenCurrentValues(packet, left, right)
      }}
    }
    val unresolvedSelection = active.filter(_.work.rootRestriction == EngineRootRestriction.Unrestricted)
      .flatMap(pool => selectionConflict(packet, pool, active))
    (measured ++ unresolvedSelection).distinct
      .sortBy(value => (value.prefix.size, value.prefix.mkString(" "), value.move, value.workIds.mkString(" ")))

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
