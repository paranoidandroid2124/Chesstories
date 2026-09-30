package io.chesstory.runtime

import CommentaryFact.*
import CommentaryProposal.*
import lila.chessjudgment.model.judgment.*

/** A view of existing questions, not another evidence store or chess authority.
  * Identity includes the exact observed branch and original physical pieces. Different
  * replies may challenge that branch; their facts and answers never become interchangeable.
  */
private[runtime] object CommentaryIdea:
  type Pair = (List[String], Set[String])
  def pair(value: CompareMoves): Pair = value.prefixMovesUci -> Set(value.referenceMove, value.alternativeMove)

  final case class AnsweredComparison(proposalId: String, comparison: CompareMoves,
      answer: CommentaryInvestigation.Answer)
  // Retain the existing proof match for presentation. These are references to
  // certified requirements, not another reader or an estimate of move strength.
  final case class Mechanism(condition: FactComparison, before: Fact, after: Fact,
      test: FactComparison, action: Fact, alternative: Fact)
  final case class QuestionFocus(owner: AnsweredComparison, key: Key, mechanism: Mechanism)
  private case class ExplanationPart(text: String, comparison: Option[AnsweredComparison] = None,
      remainder: String = "", facts: Option[(Fact, Fact)] = None)

  final case class Key(anchor: List[String], move: String, branch: List[String], action: Predicate, loss: Boolean)
  final case class Member(entry: Entry, key: Key, observation: Fact):
    val requirements = entry.request.requirements.map(value => value.id -> value).toMap
    val readings = entry.readings.map(value => value.requirementId -> value).toMap
    val comparisons = entry.request.requirements.collect { case value: CompareMoves => value }
    def supported(id: String): Boolean = readings.get(id).exists(value => Set("supported", "observed")(value.state))
    def absent(id: String): Boolean = readings.get(id).exists(value => value.state == "refuted" ||
      value.result.exists(_.reason.contains("event_absent")))
    def values(id: String): List[String] = readings.get(id).toList.flatMap(_.result.toList
      .flatMap(_.samples.flatMap(_.value))).distinct
    val current = entry.request.requirements.collect {
      case fact @ Fact(_, Path(_, Comparison(id, "reference"), _), _, _: Event, _, _) if
          comparisons.exists(value => value.id == id && value.prefixMovesUci == key.anchor && value.referenceMove == key.move) => fact
    }
    val withdrawn = current.exists(fact => absent(fact.id))
    val currentConfirmed = current.nonEmpty && current.forall(fact => supported(fact.id))
    val differences = entry.request.requirements.collect {
      case value: FactComparison if value.relation == "different" && supported(value.id) => value
    }
    // Attribute each observed effect to its actual move before checking whether it
    // explains the action. Later effects keep their owner.
    val ownedEffects = differences.filter { difference =>
        (requirements(difference.leftRequirementId), requirements(difference.rightRequirementId)) match
          case (left: Fact, right: Fact) =>
            (left.path.source, right.path.source, left.at, right.at) match
              case (Declared(a), Declared(b), Ply(before), Ply(after)) =>
                a == b && before == key.anchor.size && after == before + 1 &&
                  a.startsWith(key.anchor :+ key.move)
              case _ => false
          case _ => false
      }
    val contribution = if key.loss then key.branch.size <= key.anchor.size + 2 && supported(observation.id)
      else ownedEffects.nonEmpty
    val lostResponses = differences.flatMap { difference =>
      (requirements(difference.leftRequirementId), requirements(difference.rightRequirementId)) match
        case (left: Fact, right: Fact) if left.predicate == right.predicate &&
            left.id.startsWith("reference_response") && left.predicate.kind == "legal_capture" &&
            values(right.id) == List("legal") && values(left.id).exists(Set("illegal", "actor_removed", "target_removed")) =>
          Some(left.predicate)
        case _ => None
    }
    // Question members may share a mechanism, but a route's changed location and an
    // unrelated legality contrast must not independently supply its two halves.
    private def functionalEffect(owners: List[Member], use: Fact): Option[(FactComparison, Fact, Fact)] =
      (use.path.source, use.at) match
        case (Declared(moves), Ply(count)) =>
          val destination = key.action.square.orElse(key.action.target.map(_ => key.branch.last.slice(2, 4)))
          owners.iterator.filter(_.key == key).flatMap { owner =>
            owner.ownedEffects.iterator.flatMap { difference =>
              (owner.requirements(difference.leftRequirementId), owner.requirements(difference.rightRequirementId)) match
                case (before: Fact, after: Fact) =>
                  val onTrace = before.path.source match
                    case Declared(trace) => trace.size >= count && trace.take(count) == moves.take(count)
                    case _ => false
                  val preserved = !owner.differences.exists { later =>
                    owner.requirements(later.leftRequirementId) match
                      case fact: Fact => fact.predicate == before.predicate && fact.path == before.path &&
                        fact.at.horizon > key.anchor.size && fact.at.horizon < count
                      case _ => false
                  }
                  val condition = before.predicate
                  val clearsDestination = condition.kind == "location" && condition.subject != key.action.subject &&
                    condition.subject.exists(piece => key.action.subject.exists(_.side == piece.side)) &&
                    destination.exists(square => owner.values(before.id) == List(square) &&
                      owner.values(after.id).nonEmpty && !owner.values(after.id).contains(square))
                  val movesActor = owner.ownedEffects.exists { effect => owner.requirements(effect.leftRequirementId) match
                    case fact: Fact => fact.predicate.kind == "location" && fact.predicate.subject == key.action.subject
                    case _ => false
                  }
                  val opensAction = condition.kind == "control" && condition.subject == key.action.subject &&
                    condition.square == destination && owner.values(before.id) == List("absent") &&
                    owner.values(after.id) == List("present") && (key.action.target.nonEmpty || !movesActor)
                  Option.when(onTrace && preserved && (clearsDestination || opensAction))((difference, before, after))
                case _ => None
            }
          }.nextOption()
        case _ => None

    // A response contrast already names its supporting condition. A move-order
    // contrast must likewise use a root-owned condition, not just a different origin.
    def mechanismWitness(owners: List[Member]): Option[Mechanism] = differences.iterator.flatMap { difference =>
      (requirements(difference.leftRequirementId), requirements(difference.rightRequirementId)) match
        case (left: Fact, right: Fact) if left.predicate == right.predicate =>
          val response = Option.when(left.id.startsWith("reference_response") && left.predicate.kind == "legal_capture" &&
            values(left.id) == List("legal") && values(right.id).exists(Set("illegal", "actor_removed", "target_removed")) &&
            ownedEffects.nonEmpty)(ownedEffects.find(effect => requirements(effect.leftRequirementId) match
              case fact: Fact => fact.predicate.kind == "control" && fact.predicate.subject == left.predicate.subject &&
                !differences.exists(later => requirements(later.leftRequirementId) match
                  case changed: Fact => changed.predicate == fact.predicate && changed.path == fact.path &&
                    changed.at.horizon > key.anchor.size
                  case _ => false)
              case _ => false)).flatten.map { effect =>
                (effect, requirements(effect.leftRequirementId).asInstanceOf[Fact],
                  requirements(effect.rightRequirementId).asInstanceOf[Fact])
              }
          val sameInterveningMoves = (left.path.source, right.path.source, left.at, right.at) match
            case (Declared(a), Declared(b), Ply(n), Ply(m)) if n == m && n > key.anchor.size && a.size >= n && b.size >= m =>
              a.take(n).startsWith(key.anchor :+ key.move) && b.take(m).startsWith(key.anchor) &&
                a(key.anchor.size) != b(key.anchor.size) &&
                a.take(n).drop(key.anchor.size + 1) == b.take(m).drop(key.anchor.size + 1)
            case _ => false
          response.orElse {
            if left.predicate == key.action && sameInterveningMoves &&
                values(left.id) == List("legal") && values(right.id) == List("illegal") then functionalEffect(owners, left)
            else None
          }.map { (condition, before, after) =>
            Mechanism(condition, before, after, difference, left, right)
          }
        case _ => None
    }.nextOption()
    def mechanismWith(owners: List[Member]): Boolean = mechanismWitness(owners).nonEmpty
    def mechanism: Boolean = mechanismWith(List(this))
    def ready(value: CompareMoves): Boolean = entry.generatedArms.exists(arm =>
      arm.headRequirementIds.contains(value.id) && arm.headRequirementIds
        .filterNot(id => requirements(id).isInstanceOf[CompareMoves]).forall(supported))

    private lazy val contexts = comparisons.map(value => value.id -> comparisonContext(entry, value, readyOnly = true)).toMap
    def explains(value: CompareMoves): Boolean = !key.loss || contexts.getOrElse(value.id, Nil).exists {
      case fact if fact.id.startsWith("sacrifice_") && !fact.id.startsWith("sacrifice_loss_") &&
          !fact.id.endsWith("_role") && supported(fact.id) => fact.at match
        case event: Event => event.fromPly >= key.branch.size
        case Ply(count) => count > key.branch.size
      case _ => false
    }

    /** Dependency roles, not weights for motif names. Acceptance/decline tests whether the
      * loss branch is relevant at all; an arbitrary way of preventing a later recapture
      * must not consume its slot. A preparation's specific changed answer tests its use.
      */
    def role(value: CompareMoves): Int =
      if key.loss && value.prefixMovesUci == key.branch.init && value.referenceMove == key.branch.last &&
          value.expectedRelation.isEmpty then 0
      else if !key.loss && value.expectedRelation.nonEmpty && differences.exists(difference =>
          requirements(difference.leftRequirementId) match
            case fact: Fact => fact.id.startsWith("reference_response")
            case _ => false) then 0
      else if value.prefixMovesUci == key.anchor && value.referenceMove == key.move &&
          (key.loss || value.id == "contrast_immediate") then 1
      else if value.prefixMovesUci == key.anchor && value.referenceMove == key.move then 2
      else 3

  final case class Idea(key: Key, members: List[Member], choices: List[(String, CompareMoves, Reading)] = Nil):
    val id = CommentarySelection.stableId("practical-idea", key.anchor ++ List(key.move,
      key.branch.mkString(" "), key.action.json.toString, key.loss.toString))
    // The direct-action comparison tests the preparation's claimed benefit. An older,
    // incomplete alternative-preparation answer cannot keep that specific claim adopted.
    val direct = members.filter(_.comparisons.exists(_.id == "contrast_immediate"))
    val tracking = (if direct.nonEmpty then direct else members).filter(_.current.nonEmpty)
    val withdrawn = tracking.nonEmpty && tracking.forall(_.withdrawn)
    // A sibling's current root observation also governs the counters derived from it.
    // Otherwise the historical counter would keep a withdrawn preparation alive forever.
    val live = if withdrawn then Nil else members.filterNot(_.withdrawn)
    val contribution = live.exists(_.contribution)
    private val mechanisms = live.flatMap(member => member.mechanismWitness(live).map(member -> _))
    val mechanism = key.loss || mechanisms.nonEmpty
    private val strongerOwnedComparisons = choices.filter { (_, value, reading) =>
      value.prefixMovesUci == key.anchor && reading.answer.exists(answer =>
        (value.referenceMove == key.move && answer.evaluationRelation == "better") ||
          (value.alternativeMove == key.move && answer.evaluationRelation == "worse"))
    }
    val strongerComparisons = strongerOwnedComparisons.map((_, value, reading) => value -> reading).distinct
    val strongerChoices = strongerComparisons.map { (value, _) =>
      if value.referenceMove == key.move then value.alternativeMove else value.referenceMove }.distinct.sorted
    val observedReturn = !key.loss || live.exists(member => member.entry.request.requirements.exists {
      case fact: Fact if fact.id.startsWith("sacrifice_") && !fact.id.startsWith("sacrifice_loss_") &&
          !fact.id.endsWith("_role") && member.supported(fact.id) => fact.at match
        case event: Event => event.fromPly >= key.branch.size
        case Ply(count) => count > key.branch.size
      case _ => false
    })
    private def decisionsFor(selected: List[Member]) = selected.flatMap(member =>
      member.comparisons.filter(member.ready).map(value => (member, value, member.readings(value.id))))
      .sortBy((member, value, _) => member.role(value)).distinctBy((_, value, _) => pair(value))
    val knownDecisions = decisionsFor(members)
    val decisions = decisionsFor(live)
    val essential = decisions.filter((member, value, _) => member.role(value) <= 1)
    val revised = strongerChoices.nonEmpty || decisions.exists { (member, value, reading) =>
      reading.answer.exists(_.evaluationRelation == "better") &&
        (value.expectedRelation.nonEmpty || value.prefixMovesUci == key.anchor || (key.loss && member.role(value) == 0))
    }
    val state = if live.isEmpty then "withdrawn"
      else if revised then "revised"
      else if contribution && mechanism && observedReturn && tracking.nonEmpty && tracking.forall(_.currentConfirmed) && essential.nonEmpty &&
          essential.forall(_._3.answer.nonEmpty) && live.forall(member =>
            member.entry.generatedArms.flatMap(_.headRequirementIds).filterNot(id =>
              member.requirements(id).isInstanceOf[CompareMoves]).forall(member.supported)) then "adopted"
      else "candidate"

    def priority(value: CompareMoves): Option[(Int, Int)] =
      live.flatMap(member => member.comparisons.filter(other => pair(other) == pair(value) &&
          member.ready(other) && member.explains(other))
        .map(other => (if contribution then 0 else 1, member.role(other)))).minOption

    def questionFocus(packet: EvidenceBackedJudgmentPacket, prepared: ObservedLineProtocol.PreparedPaths,
        answered: Set[AnsweredComparison]): List[QuestionFocus] =
      prepared.requireOwner(packet)
      if withdrawn || tracking.isEmpty || !tracking.forall(_.currentConfirmed) then Nil
      else mechanisms.flatMap { (member, witness) =>
        val response = witness.action.id.startsWith("reference_response")
        val exactCondition = !response || ((witness.action.path.source, witness.action.at) match
          case (Declared(moves), Ply(count)) if count >= 0 && count <= moves.size =>
            witness.action.predicate.target.exists(target => prepared.paths.exists(path =>
              path.certified.nonEmpty && path.ids == path.declared && path.moves.startsWith(moves) &&
                prepared.pieceAfter(packet, path, target.witness, count).flatten.exists(piece =>
                  witness.after.predicate.square.contains(piece.square.key)))) &&
              member.values(witness.before.id) == List("absent") && member.values(witness.after.id) == List("present")
          case _ => false)
        val testedAlternative = witness.alternative.path.source match
          case Declared(moves) if moves.startsWith(key.anchor) => moves.lift(key.anchor.size)
          case _ => None
        // The condition member need not own an engine comparison. Its sibling's
        // answer may supply that comparison only for this exact idea and tested alternative.
        Option.when(exactCondition)(live).toList.flatten.flatMap { decision =>
          decision.comparisons.filter(value => decision.ready(value) && value.prefixMovesUci == key.anchor &&
            value.referenceMove == key.move && testedAlternative.contains(value.alternativeMove)).flatMap { value =>
            decision.readings(value.id).answer.map(AnsweredComparison(decision.entry.request.id, value, _))
              .filter(answered).map(owner => QuestionFocus(owner, key, witness))
          }
        }
      }.distinct

    private def piece(value: Piece): String = s"${value.side} ${value.role} originally on ${value.square}"
    private def action(value: Predicate): String = value.subject.map(piece).getOrElse("the actor") +
      value.target.map(target => " capturing " + piece(target)).getOrElse(" moving to " + value.square.getOrElse("the named square"))
    private def describe(value: Predicate): String = value.kind match
      case "legal_capture" | "legal_move" => action(value)
      case "control" => value.subject.map(piece).getOrElse("the piece") + " control of " + value.square.getOrElse("the square")
      case "location" => value.subject.map(piece).getOrElse("the piece") + " location"
      case _ => value.kind + " " + value.subject.map(piece).orElse(value.property).getOrElse("")

    private lazy val phrasing = phrasingFor(Set.empty)
    private def phrasingFor(shown: Set[AnsweredComparison], renderLine: List[String] => List[String] = identity,
        focus: Option[Mechanism] = None): (List[String], List[ExplanationPart]) =
      import PrincipalVariationComparison.QualityBand
      def label(prefix: List[String], move: String) = renderLine(prefix :+ move).last
      def band(reading: Reading) = reading.answer.flatMap(_.qualityBand)
      def where(value: CompareMoves) = if value.prefixMovesUci.isEmpty then "at the starting position"
        else "after " + renderLine(value.prefixMovesUci).mkString(" ")
      def covered(rows: List[(String, CompareMoves, Reading)]): Boolean =
        // The displayed answer must own an exact question. Other questions can
        // repeat its ranking only if every physical/evaluation receipt agrees.
        rows.exists((id, value, reading) => reading.answer.exists(answer =>
          shown(AnsweredComparison(id, value, answer)))) && rows.map { (_, value, reading) =>
          val answer = reading.answer.get
          val witnesses = List(
            (value.referenceMove, answer.referenceLineId, answer.referenceOccurrenceId, answer.referenceDepth, answer.referenceEvaluation),
            (value.alternativeMove, answer.alternativeLineId, answer.alternativeOccurrenceId, answer.alternativeDepth, answer.alternativeEvaluation)
          ).sortBy(_._1)
          val relation = if value.referenceMove < value.alternativeMove then answer.evaluationRelation
            else answer.evaluationRelation match
              case "better" => "worse"
              case "worse" => "better"
              case other => other
          (value.prefixMovesUci, witnesses, answer.workId, answer.executionKeySha256, answer.engineProfile,
            answer.requiredDepth, relation, answer.qualityBand)
        }.distinct.size == 1
      // Keep raw comparison/state ownership intact; calibrate only what its score difference licenses us to say.
      val revisionComparisons = strongerOwnedComparisons ++ decisions.collect {
        case (member, value, reading) if reading.answer.exists(_.evaluationRelation == "better") &&
            (value.expectedRelation.nonEmpty || value.prefixMovesUci == key.anchor || (key.loss && member.role(value) == 0)) =>
          (member.entry.request.id, value, reading)
      }
      val separatedRevision = revisionComparisons.find((_, _, reading) => band(reading).contains(QualityBand.Separated))
      val purpose = if key.loss then
        s"After ${label(key.anchor, key.move)}, the observed loss of ${key.action.subject.map(piece).getOrElse("material")} to ${renderLine(key.branch).last} asks whether the follow-up justifies that loss and whether the opponent should accept it."
      else if mechanism then s"${label(key.anchor, key.move)} may help prepare ${action(key.action)} after ${renderLine(key.branch).init.mkString(" ")}."
      // This question's missing contrast does not negate independently certified
      // uses of a maintained relation or cleared corridor. Its decision state
      // still governs practical value; describe only the action here.
      else s"The observed line after ${label(key.anchor, key.move)} includes ${action(key.action)} after ${renderLine(key.branch).init.mkString(" ")}."
      val effects = live.flatMap(member => member.differences.flatMap { difference =>
        (member.requirements(difference.leftRequirementId), member.requirements(difference.rightRequirementId)) match
          case (left: Fact, right: Fact) if member.values(left.id).nonEmpty && member.values(right.id).nonEmpty =>
            val (order, scope) = (left.path.source, right.path.source, left.at, right.at) match
              case (Declared(a), Declared(b), Ply(before), Ply(after)) if a == b && after == before + 1 =>
                (if before == key.anchor.size then 0 else 2, s"${renderLine(a)(before)} changes")
              case (Declared(a), Declared(b), _, _) =>
                (if left.id.startsWith("reference_response") then 1 else 3,
                  s"The branches after ${renderLine(a).mkString(" ")} and ${renderLine(b).mkString(" ")} differ in")
              case _ => (3, "The named branches differ in")
            val required = focus.exists(value => (value.before == left && value.after == right) ||
              (value.action == left && value.alternative == right))
            Some((if required then 0 else 1, order) -> ExplanationPart(
              s"$scope ${describe(left.predicate)}: ${member.values(left.id).mkString("/")} versus ${member.values(right.id).mkString("/")}.",
              facts = Some(left -> right)))
          case _ => None
      }).sortBy(_._1).distinctBy(_._2.text).map(_._2).take(2)
      val returns = if !key.loss then Nil else live.flatMap(member => member.entry.request.requirements.collect {
        case fact: Fact if fact.id.startsWith("sacrifice_gain_") && !fact.id.endsWith("_role") && member.supported(fact.id) =>
          fact.at match
            case event: Event if event.fromPly >= key.branch.size =>
              Some(s"The observed follow-up includes ${event.actor.map(piece).getOrElse("the actor")} capturing ${event.target.map(piece).getOrElse("material")}.")
            case _ => None
      }.flatten).distinct.take(1)
      val decision = state match
        case "withdrawn" => "Withdrawn as a current explanation: its required events are absent from the current returned line."
        case "revised" if separatedRevision.nonEmpty =>
          val (_, value, reading) = separatedRevision.get
          val (higher, lower) = if reading.answer.exists(_.evaluationRelation == "better") then
            value.alternativeMove -> value.referenceMove else value.referenceMove -> value.alternativeMove
          if covered(separatedRevision.toList) then
            "Revise this idea: the observed possibility alone does not justify preferring the clearly lower-scoring continuation."
          else s"Revise this idea: the engine comparison ${where(value)} clearly favors ${label(value.prefixMovesUci, higher)} over ${label(value.prefixMovesUci, lower)}. The observed possibility alone does not justify preferring the lower-scoring continuation."
        case "revised" if revisionComparisons.forall((_, _, reading) => band(reading).contains(QualityBand.Close)) =>
          "The engine evaluations are close; the small differences alone do not rule out this idea."
        case "revised" =>
          "The search ranks another choice higher, but it has not established that the move is clearly stronger."
        case "adopted" => "Adopted as a conditional idea within the tested branches; this is not proof of the best plan or its sole cause."
        case _ if members.exists(_.entry.readings.exists(_.stopReason.contains("comparison_conflict"))) =>
          "Candidate idea under reconsideration: comparisons disagree on the same move; the divergent continuation and the earlier evaluation remain unverified."
        case _ if members.exists(_.entry.readings.exists(_.stopReason.contains("comparison_not_current"))) =>
          "Candidate idea under reconsideration: an earlier answer no longer meets the current verification depth."
        case _ => "Candidate idea: its practical value is not yet established."
      val pending = essential.filter(_._3.answer.isEmpty).take(2).map { (_, value, reading) =>
        s"Still needed: ${label(value.prefixMovesUci, value.alternativeMove)} versus ${label(value.prefixMovesUci, value.referenceMove)} ${where(value)} (${reading.stopReason.getOrElse(reading.state)})."
      }
      // Only an explicit conclusion owning this same question and full receipt
      // replaces its answer summary. A common move pair is not that ownership.
      val concluded = (strongerOwnedComparisons ++
        Option.when(state == "revised")(separatedRevision).flatten.toList).flatMap { (id, value, reading) =>
          reading.answer.map(AnsweredComparison(id, value, _))
        }.toSet
      val answered = knownDecisions.filter((member, value, reading) => reading.answer.nonEmpty &&
        (!withdrawn || member.role(value) <= 1)).sortBy((member, value, _) => member.role(value)).take(3).flatMap { (member, value, reading) =>
        val owner = AnsweredComparison(member.entry.request.id, value, reading.answer.get)
        val relation = reading.answer.get.evaluationRelation
        val meaning = if band(reading).contains(QualityBand.Close) then
          "the engine evaluations are close; this difference alone does not establish a stronger choice"
        else if band(reading).isEmpty then "it is not yet established whether either move is clearly stronger"
        else if value.expectedRelation.nonEmpty then
          if relation == "better" then "this response challenges the continuation" else "this tested response does not establish a stronger refutation"
        else if value.prefixMovesUci == key.anchor then
          if relation == "better" then "this alternative limits the reason to choose this move" else "this comparison does not favor the alternative"
        else "this tests the opponent's choice of response, not compensation by itself"
        val relative = relation match
          case "better" => "above"
          case "worse" => "below"
          case _ => "equal to"
        Option.unless(concluded(owner))(ExplanationPart(
          s"${label(value.prefixMovesUci, value.alternativeMove)} evaluates $relative ${label(value.prefixMovesUci, value.referenceMove)} for the side to move ${where(value)}; $meaning.",
          Some(owner), meaning.capitalize + ".")).toList
      }
      val choice = List(Some(QualityBand.Separated), Some(QualityBand.Close), None).flatMap { quality =>
        val moves = strongerOwnedComparisons.filter((_, _, reading) => band(reading) == quality).groupBy { (_, value, _) =>
          if value.referenceMove == key.move then value.alternativeMove else value.referenceMove
        }.toList.collect { case (move, rows) if !covered(rows) => move }.sorted
        Option.when(moves.nonEmpty) {
          val conclusion = quality match
            case Some(QualityBand.Separated) =>
              s"${moves.map(label(key.anchor, _)).mkString(" or ")} is stronger than ${label(key.anchor, key.move)} in the current tested comparisons. The observed plan does not override this choice."
            case Some(QualityBand.Close) =>
              s"${moves.map(label(key.anchor, _)).mkString(" or ")} evaluates above ${label(key.anchor, key.move)}, but the engine evaluations are close."
            case None =>
              s"${moves.map(label(key.anchor, _)).mkString(" or ")} evaluates above ${label(key.anchor, key.move)}; that ranking alone does not establish a clearly stronger move."
          s"At this idea's starting position, $conclusion"
        }
      }
      val costs = live.flatMap(_.lostResponses).distinct.map(response =>
        s"Compared with immediate execution, the prepared line gives up the recapture: ${action(response)}. This is a concession, not evidence that the preparation helps the action.")
      (List(decision) ++ choice ++ costs,
        List(ExplanationPart(purpose)) ++ effects ++ returns.map(text => ExplanationPart(text)) ++ answered ++
          (pending :+ "Other replies and later plans remain outside this conclusion. Exact branches and evidence are retained in the linked questions.")
            .map(text => ExplanationPart(text)))

    def decisionText: List[String] = phrasing._1
    def decisionText(shown: Set[AnsweredComparison], renderLine: List[String] => List[String] = identity): List[String] =
      phrasingFor(shown, renderLine)._1
    lazy val text: String = (decisionText ++ phrasing._2.map(_.text)).mkString(" ")

    /** Read only current answered branches, preserving the physical target and exact history.
      * A different first move is not a new strategic objective; describe the observed action.
      */
    def continuationText(packet: EvidenceBackedJudgmentPacket, prepared: ObservedLineProtocol.PreparedPaths): String =
      prepared.requireOwner(packet)
      if withdrawn || key.loss then return ""
      val views = prepared.resourceViews(packet)
      case class Step(ply: Int, move: String, actor: Piece, target: Option[Piece], to: String, promotion: Option[String]):
        def sameAction: Boolean = key.action.subject.contains(actor) &&
          (if key.action.kind == "legal_capture" then target == key.action.target
           else target.isEmpty && key.action.square.contains(to) && key.action.property == promotion)
        def text: String = piece(actor) + target.map(value => " capturing " + piece(value)).getOrElse(" moving to " + to)
      def steps(path: ObservedLineProtocol.PreparedPath, from: Int): List[Step] =
        path.moves.indices.take(MaximumPly).drop(from).toList.flatMap { offset =>
          val identities = prepared.identitiesAfter(packet, path, offset).flatMap {
            case (original, Some(Some(current))) => Some(current -> Piece(
              if original.side.white then "white" else "black", original.role.name, original.square.key))
            case _ => None
          }.toMap
          for
            position <- path.positions.lift(offset).toList
            view <- position.id.flatMap(views.get).toList
            row <- view.legalRows.toList.flatten.filter(_.move == path.moves(offset))
            actor <- identities.get(row.actor).toList if key.action.subject.exists(_.side == actor.side)
            target = row.capture.flatMap(identities.get)
            if row.capture.isEmpty || target.nonEmpty
          yield Step(offset, row.move, actor, target, row.to.key, row.promotionRole)
        }
      decisions.filter { (_, value, reading) =>
        value.expectedRelation.nonEmpty && reading.answer.nonEmpty &&
          value.prefixMovesUci.startsWith(key.anchor :+ key.move) &&
          value.prefixMovesUci.size < key.branch.size - 1
      }.flatMap { (_, value, reading) =>
        val answer = reading.answer.get
        def path(id: String, move: String) = prepared.paths.find(path => path.line.ref.id == id &&
          path.certified.nonEmpty && path.ids == path.declared && path.moves.startsWith(value.prefixMovesUci :+ move))
        for
          reference <- path(answer.referenceLineId, value.referenceMove).toList
          alternative <- path(answer.alternativeLineId, value.alternativeMove).toList
          expected <- steps(reference, value.prefixMovesUci.size + 1).find(_.sameAction).toList
          following = steps(alternative, value.prefixMovesUci.size + 1)
          continued = following.find(_.sameAction)
          replacement = following.find(step => key.action.kind == "legal_capture" && step.target == key.action.target)
          next <- continued.orElse(replacement).orElse(following.headOption).toList
        yield
          val result = if continued.nonEmpty then
            s"the same action still occurs${if next.ply > expected.ply then " later" else ""}: ${next.text}"
          else if replacement.nonEmpty then s"the same physical target is captured by a different piece: ${next.text}"
          else s"the returned continuation instead begins with ${next.text}; the original action is not observed within the inspected continuation"
          val line = prepared.notation.line(alternative.moves).drop(value.prefixMovesUci.size).take(next.ply - value.prefixMovesUci.size + 1)
          s"After the tested reply ${prepared.notation.move(value.prefixMovesUci, value.alternativeMove)}, $result (${line.mkString(" ")}). This is a conditional continuation, not a forced plan or proof that the original goal must be abandoned."
      }.distinct.take(2).mkString(" ")

    def explanation(packet: EvidenceBackedJudgmentPacket, prepared: ObservedLineProtocol.PreparedPaths): String =
      (decisionText(Set.empty, prepared.notation.line) ++ explanationDetails(packet, prepared).map(_.text)).mkString(" ")

    def explanationDetails(packet: EvidenceBackedJudgmentPacket, prepared: ObservedLineProtocol.PreparedPaths,
        answered: Set[AnsweredComparison] = Set.empty): List[MoveWorkProjection.Context] =
      val focus = questionFocus(packet, prepared, answered).headOption
      val parts = phrasingFor(Set.empty, prepared.notation.line, focus.map(_.mechanism))._2
        .map(part => part.copy(text = if part.comparison.exists(answered) then part.remainder else part.text))
        .filter(_.text.nonEmpty)
      val contexts = focus match
        case Some(value) =>
          val required = Set(value.mechanism.before -> value.mechanism.after,
            value.mechanism.action -> value.mechanism.alternative)
          val (conditions, other) = parts.drop(1).partition(_.facts.exists(required))
          // The purpose and its actual condition/test travel as one explanation;
          // a field limit cannot leave the preparatory claim without its reason.
          val complete = (parts.take(1) ++ conditions).map(_.text).mkString(" ")
          val test = conditions.filter(_.facts.contains(value.mechanism.action -> value.mechanism.alternative))
            .map(_.text).mkString(" ")
          MoveWorkProjection.Context(complete, question = focus,
            questionTest = Option.when(test.nonEmpty)(test)) ::
            other.map(part => MoveWorkProjection.Context(part.text))
        case None => parts.map(part => MoveWorkProjection.Context(part.text))
      contexts ++ List(continuationText(packet, prepared)).filter(_.nonEmpty).map(MoveWorkProjection.Context(_))

  def read(entries: List[Entry]): List[Idea] =
    val choices = entries.flatMap { entry =>
      val requirements = entry.request.requirements.map(value => value.id -> value).toMap
      val readings = entry.readings.map(value => value.requirementId -> value).toMap
      entry.generatedArms.filter(arm => arm.headRequirementIds.filterNot(id =>
        requirements(id).isInstanceOf[CompareMoves]).forall(id => Set("supported", "observed")(readings(id).state)))
        .flatMap(_.headRequirementIds).distinct.flatMap(id => requirements(id) match
          case value: CompareMoves if readings(id).answer.nonEmpty => List((entry.request.id, value, readings(id)))
          case _ => Nil)
    }.distinct
    entries.filter(_.admission == "generated").flatMap { entry =>
      val facts = entry.request.requirements.collect { case fact: Fact => fact }
      val lossBoundary = entry.request.requirements.collectFirst {
        case value: CompareMoves if Set("sacrifice_decline", "sacrifice_other_capture")(value.id) => value.prefixMovesUci.size + 1
      }
      val loss = facts.filter(fact => fact.id.startsWith("sacrifice_loss_") &&
        !fact.id.endsWith("_role") && lossBoundary.forall(_ == fact.at.horizon)).sortBy(_.at.horizon).headOption
      val observation = loss.orElse(facts.find(fact => fact.id == "reference_action" || fact.id.startsWith("reference_action_")))
      for
        fact <- observation.toList
        moves <- (fact.path.source match
          case Declared(moves) => List(moves)
          case _ => Nil)
        arm <- entry.generatedArms.headOption.toList
        count = if loss.nonEmpty then fact.at.horizon else fact.at.horizon + 1
        if count > arm.prefixMoves.size && moves.size >= count &&
          moves.startsWith(arm.prefixMoves :+ arm.referenceMove)
      yield Member(entry, Key(arm.prefixMoves, arm.referenceMove, moves.take(count), fact.predicate, loss.nonEmpty), fact)
    }.groupBy(_.key).toList.sortBy(_._1.toString).map((key, members) => Idea(key, members, choices))
      .filter(_.members.exists(_.contribution))

  /** One short synthesis per idea, on its first existing question. No copied receipts,
    * proof graph, retained planning state, or additional public evidence catalogue.
    */
  def summaries(entries: List[Entry], render: Idea => String = _.text): Map[String, String] = read(entries).map { idea =>
    val owner = idea.direct.headOption.orElse(idea.members.find(member => member.comparisons.exists(value =>
      value.expectedRelation.isEmpty && member.role(value) == 0))).getOrElse(idea.members.head).entry.request.id
    val choicePairs = idea.strongerComparisons.map((value, _) => pair(value)).toSet
    val choiceOwners = entries.filter(_.request.requirements.exists {
      case value: CompareMoves => choicePairs(pair(value))
      case _ => false
    }).map(_.request.id)
    val linked = (idea.members.map(_.entry.request.id) ++ choiceOwners).distinct.filterNot(_ == owner)
    owner -> (render(idea) + (if linked.isEmpty then "" else " Related questions: " + linked.take(4).mkString(", ") +
      (if linked.size > 4 then s"; ${linked.size - 4} further questions share this exact idea." else ".")))
  }.toMap

  def currentQuestions(entries: List[Entry]): List[Entry] =
    val withdrawn = read(entries).filter(_.withdrawn).flatMap(_.members.map(_.entry.request.id)).toSet
    entries.filterNot(entry => withdrawn(entry.request.id))
