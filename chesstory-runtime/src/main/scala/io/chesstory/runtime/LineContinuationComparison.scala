package io.chesstory.runtime

import lila.chessjudgment.model.judgment.*
import play.api.libs.json.*

/** Joins two selected observations by physical subject and explicit time. No score or forcedness inference. */
private[runtime] object LineContinuationComparison:
  import LineContinuationInvestigation.{ Event, SupportComparison }
  import ObservedLineProtocol.PreparedPaths

  final class Reading private[LineContinuationComparison] (
      private val owner: EvidenceBackedJudgmentPacket,
      private val left: LineContinuationInvestigation.Reading,
      private val right: LineContinuationInvestigation.Reading,
      private val differences: List[JsObject]
  ):
    val id: String = CommentarySelection.stableId("line-continuation-comparison", List(left.id, right.id))
    val lineIds: List[String] = List(left.path.line.ref.id, right.path.line.ref.id)
    def candidateJson(packet: EvidenceBackedJudgmentPacket): JsObject =
      require(owner eq packet, "a continuation comparison belongs to its exact packet")
      val root = packet.assembly.playedTransition.get
      val paths = List(left.path, right.path)
      Json.obj("id" -> id, "kind" -> "comparison", "selection" -> "context", "state" -> "ready",
        "root_occurrence_id" -> root.evidence.id,
        "occurrence_ids" -> (root.evidence.id :: paths.flatMap(_.ids)).distinct,
        "position_ids" -> paths.flatMap(_.positions.flatMap(_.id)).distinct,
        "line_ids" -> lineIds,
        "evidence_ids" -> paths.flatMap(_.certified.map(_._1.ref.id)).distinct,
        "source_refs" -> paths.map(path => Json.obj("line_id" -> path.line.ref.id, "kind" -> "admitted",
          "source" -> "legal_replay", "accepted_ply_count" -> path.moves.size, "stop" -> JsNull)),
        "detail" -> Json.obj("type" -> "line_continuation_comparison", "scope" -> "observed_line_pair",
          "continuation_candidate_ids" -> List(left.id, right.id), "differences" -> differences,
          "text" -> "These selected lines differ at the named physical subjects and observation points. "
            .concat("An unobserved follow-up is bounded by that line's end. Legal capture availability and direct support ")
            .concat("do not establish best play, sufficient compensation, or a forced outcome.")))

  def compare(packet: EvidenceBackedJudgmentPacket, left: LineContinuationInvestigation.Reading,
      right: LineContinuationInvestigation.Reading, prepared: PreparedPaths): Option[Reading] =
    prepared.requireOwner(packet)
    if !left.belongsTo(packet) || !right.belongsTo(packet) || left.path.line.ref == right.path.line.ref ||
        left.path.positions.headOption != right.path.positions.headOption ||
        !prepared.path(packet, left.path.line.ref.id).exists(_ eq left.path) ||
        !prepared.path(packet, right.path.line.ref.id).exists(_ eq right.path) then None
    else
      val rootId = left.path.positions.head.id.get
      val views = prepared.resourceViews(packet)
      def piece(value: RelationColoredPieceWitness): JsObject = Json.obj("position_id" -> rootId,
        "side" -> (if value.side.white then "white" else "black"), "role" -> value.role.name, "square" -> value.square.key)
      def sameTurn(leftCount: Int, rightCount: Int): Boolean =
        left.path.positions.lift(leftCount).flatMap(_.sideToMove).exists(side =>
          right.path.positions.lift(rightCount).flatMap(_.sideToMove).contains(side))
      def at(reading: LineContinuationInvestigation.Reading, count: Int): JsObject =
        Json.obj("line_id" -> reading.path.line.ref.id, "ply_count" -> count,
          "position_id" -> reading.path.positions(count).id.get,
          "preceding_occurrence_id" -> reading.path.ids.lift(count - 1))
      def original(reading: LineContinuationInvestigation.Reading, count: Int,
          current: RelationColoredPieceWitness): Option[RelationColoredPieceWitness] =
        prepared.identitiesAfter(packet, reading.path, count).collectFirst {
          case (value, Some(Some(found))) if found == current => value
        }
      def captures(reading: LineContinuationInvestigation.Reading, count: Int) =
        reading.path.positions(count).id.flatMap(views.get).flatMap(view => view.legalRows.flatMap { rows =>
          val identified = rows.flatMap(row => for
            victim <- row.capture
            actor <- original(reading, count, row.actor)
            target <- original(reading, count, victim)
          yield (actor -> target) -> row)
          Option.when(identified.size == rows.count(_.capture.nonEmpty))(
            view -> identified.groupMap(_._1)(_._2))
        })
      val comparedCaptureAvailability = scala.collection.mutable.Set.empty[
        (Int, Int, RelationColoredPieceWitness, RelationColoredPieceWitness)]
      def captureAvailability(leftCount: Int, rightCount: Int, matched: JsObject,
          subjects: Option[Set[(RelationColoredPieceWitness, RelationColoredPieceWitness)]] = None): List[JsObject] =
        if !sameTurn(leftCount, rightCount) then Nil
        else (for l <- captures(left, leftCount); r <- captures(right, rightCount) yield
          subjects.getOrElse(l._2.keySet ++ r._2.keySet).toList.sortBy { (actor, victim) =>
            (actor.side.toString, actor.square.key, victim.square.key)
          }.flatMap { capture =>
            val leftRows = l._2.getOrElse(capture, Nil)
            val rightRows = r._2.getOrElse(capture, Nil)
            // Absence comes from a complete legal pool with both original
            // pieces still present, never from a missing exposure observation.
            val present = List(left -> leftCount, right -> rightCount).forall { (reading, count) =>
              List(capture._1, capture._2).forall(subject =>
                prepared.pieceAfter(packet, reading.path, subject, count).exists(_.nonEmpty))
            }
            Option.when(present && (leftRows.nonEmpty != rightRows.nonEmpty) &&
              comparedCaptureAvailability.add((leftCount, rightCount, capture._1, capture._2)))(Json.obj(
              "kind" -> "legal_capture_availability", "original_actor" -> piece(capture._1),
              "original_victim" -> piece(capture._2),
              "left" -> (at(left, leftCount) ++ Json.obj("available" -> leftRows.nonEmpty,
                "moves_uci" -> leftRows.map(_.move), "source_ids" -> leftRows.map(_.sourceId),
                "position_occurrence_id" -> l._1.occurrence.id, "legal_inventory_complete" -> true)),
              "right" -> (at(right, rightCount) ++ Json.obj("available" -> rightRows.nonEmpty,
                "moves_uci" -> rightRows.map(_.move), "source_ids" -> rightRows.map(_.sourceId),
                "position_occurrence_id" -> r._1.occurrence.id, "legal_inventory_complete" -> true))) ++ matched)
          }).getOrElse(Nil)
      def captureKey(event: Event) = for actor <- event.originalActor; victim <- event.originalVictim yield actor -> victim
      val rightEvents = right.events.flatMap(event => captureKey(event).map(_ -> event)).toMap
      val materialIdentitiesKnown = List(left, right).map { reading =>
        val known = reading.path.certified.flatMap(_._2.material).exists { summary =>
          val captureOffsets = summary.captures.map(_.plyOffset).toSet
          summary.events.size == reading.events.size && reading.events.forall(event =>
            event.originalActor.nonEmpty && (!captureOffsets(event.plyOffset) || event.originalVictim.nonEmpty))
        }
        reading.id -> known
      }.toMap
      val commonCount = left.path.ids.zip(right.path.ids).takeWhile((l, r) => l == r).size
      val commonPosition = left.path.positions(commonCount).id.get
      def sharedAttackers(lLoss: Event, rLoss: Event): List[JsObject] =
        if commonCount >= left.path.moves.size || commonCount >= right.path.moves.size then Nil
        else
          def firstByActor(reading: LineContinuationInvestigation.Reading, loss: Event) =
            LineContinuationInvestigation.recoveryEvents(reading, loss).filter(_.plyOffset >= commonCount).flatMap(event =>
              event.originalActor.map(_ -> event)).groupMap(_._1)(_._2).view
                .mapValues(_.minBy(_.plyOffset)).toMap
          val rightByActor = firstByActor(right, rLoss)
          firstByActor(left, lLoss).toList.sortBy(_._1.square.key).flatMap { (actor, lEvent) =>
            for
              rEvent <- rightByActor.get(actor).toList
              lTarget <- lEvent.originalVictim.toList
              rTarget <- rEvent.originalVictim.toList
              if lTarget != rTarget
              currentActor <- prepared.pieceAfter(packet, left.path, actor, commonCount).flatten.toList
              currentLeft <- prepared.pieceAfter(packet, left.path, lTarget, commonCount).flatten.toList
              currentRight <- prepared.pieceAfter(packet, right.path, rTarget, commonCount).flatten.toList
              if List((left, lEvent, lTarget, currentLeft), (right, rEvent, rTarget, currentRight)).forall {
                (reading, event, target, currentTarget) => (commonCount to event.plyOffset).forall(count =>
                  prepared.pieceAfter(packet, reading.path, actor, count).flatten.contains(currentActor) &&
                    prepared.pieceAfter(packet, reading.path, target, count).flatten.contains(currentTarget))
              }
              view <- views.get(commonPosition).toList
              lSources = view.controlSources(currentActor, currentLeft.square)
              rSources = view.controlSources(currentActor, currentRight.square)
              if lSources.nonEmpty && rSources.nonEmpty
            yield
              def current(value: RelationColoredPieceWitness) = piece(value) ++ Json.obj("position_id" -> commonPosition)
              Json.obj("original_actor" -> piece(actor), "actor" -> current(currentActor),
                "position_id" -> commonPosition, "ply_count" -> commonCount,
                "left" -> Json.obj("event_id" -> lEvent.id, "target" -> current(currentLeft), "source_ids" -> lSources),
                "right" -> Json.obj("event_id" -> rEvent.id, "target" -> current(currentRight), "source_ids" -> rSources))
          }
      def followup(reading: LineContinuationInvestigation.Reading, loss: Event) =
        val removalIds = reading.captureLinks.filter(_.lossEventId == loss.id).map(_.captureEventId).distinct
        val gainIds = reading.otherTargetGains.filter(_.lossEventId == loss.id).map(_.gainEventId).distinct
        def subjects(ids: List[String]) = ids.flatMap(id => reading.events.find(_.id == id).flatMap(captureKey))
        val removals = subjects(removalIds)
        val gains = subjects(gainIds)
        val known = materialIdentitiesKnown(reading.id) && removalIds.size <= 1 &&
          removals.size == removalIds.size && gains.size == gainIds.size &&
          Set("present", "captured")(loss.actorEnd.state) &&
          ((loss.actorEnd.state == "captured") == removalIds.nonEmpty) &&
          loss.actorEnd.removedByEventId == removalIds.headOption
        Option.when(known)((removals.headOption, gains.toSet,
          at(reading, reading.path.moves.size) ++ Json.obj("loss_event_id" -> loss.id,
            "loss_ply_offset" -> loss.plyOffset,
            "observed_followup_plies" -> (reading.path.moves.size - loss.plyOffset - 1),
            "capturer_end" -> loss.actorEnd.json,
            "capturer_removal_state" -> (if removalIds.nonEmpty then "observed" else "not_observed_through_line_end"),
            "capturer_removal_event_id" -> removalIds.headOption,
            "other_target_gain_state" -> (if gainIds.nonEmpty then "observed" else "not_observed_through_line_end"),
            "other_target_gain_event_ids" -> gainIds)))
      val followupDifferences = left.events.flatMap { loss =>
        (for
          subject <- captureKey(loss)
          other <- rightEvents.get(subject)
          l <- followup(left, loss)
          r <- followup(right, other)
          // Compare physical follow-ups, not their path-local IDs or timing already handled below.
          if (l._1, l._2) != (r._1, r._2)
        yield Json.obj("kind" -> "loss_followup", "original_actor" -> piece(subject._1),
          "original_victim" -> piece(subject._2), "left" -> l._3, "right" -> r._3,
          "shared_attackers" -> sharedAttackers(loss, other))).toList
      }
      val captureDifferences = left.events.flatMap { before =>
        captureKey(before).toList.flatMap { key => rightEvents.get(key).toList.flatMap { after =>
          val leftCount = before.plyOffset + 1
          val rightCount = after.plyOffset + 1
          if !sameTurn(leftCount, rightCount) then Nil
          else
            val timing = Option.when(before.plyOffset != after.plyOffset)(Json.obj("kind" -> "capture_timing",
              "original_actor" -> piece(key._1), "original_victim" -> piece(key._2),
              "left" -> (at(left, leftCount) ++ Json.obj("event_id" -> before.id, "source_id" -> before.sourceId)),
              "right" -> (at(right, rightCount) ++ Json.obj("event_id" -> after.id, "source_id" -> after.sourceId)))).toList
            val availability = captureAvailability(leftCount, rightCount,
              Json.obj("matched_capture_event_ids" -> List(before.id, after.id)))
            timing ++ availability
        }}
      }
      def recapturerSupport(l: SupportComparison, r: SupportComparison, lc: Int, rc: Int): List[JsObject] =
        if lc <= 0 || rc <= 0 || !sameTurn(lc - 1, rc - 1) || !sameTurn(lc, rc) then Nil
        else
          val originals = prepared.identitiesAfter(packet, left.path, 0).map(_._1)
          List(l.afterActor, r.afterActor).distinct.flatMap { actor =>
            originals.filter(target => target.side == actor.side && target.role.name != "king" && target != actor)
              .flatMap { target =>
                val lb = left.supportAt(lc - 1, actor, target)
                val la = left.supportAt(lc, actor, target)
                val rb = right.supportAt(rc - 1, actor, target)
                val ra = right.supportAt(rc, actor, target)
                val states = List(lb, la, rb, ra).map(_.state)
                val changed = lb.state != la.state || rb.state != ra.state
                val differs = (lb.state, la.state) != (rb.state, ra.state)
                // The actor comes only from the selected recapture family. A
                // shared change, or an unknown control inventory, is no contrast.
                Option.when(!states.contains("unknown") && states.contains("present") && changed && differs) {
                  def point(reading: LineContinuationInvestigation.Reading, count: Int,
                      support: LineContinuationInvestigation.Support) = at(reading, count) ++ Json.obj("support" -> support.json)
                  Json.obj("kind" -> "recapturer_support", "original_subject" -> piece(actor),
                    "original_target" -> piece(target), "matched_support_comparison_ids" -> List(l.id, r.id),
                    "left" -> Json.obj("before" -> point(left, lc - 1, lb), "after" -> point(left, lc, la)),
                    "right" -> Json.obj("before" -> point(right, rc - 1, rb), "after" -> point(right, rc, ra)))
                }
              }
          }
      def supportRows(kind: String, l: SupportComparison, r: SupportComparison): List[JsObject] =
        val lc = left.path.positions.indexWhere(_.id.contains(l.afterPositionId))
        val rc = right.path.positions.indexWhere(_.id.contains(r.afterPositionId))
        if lc < 0 || rc < 0 || !sameTurn(lc, rc) then Nil
        else
          val exposureSubjects = if kind == "replacement_support" then
            (left.captureExposures.filter(_.positionId == l.afterPositionId) ++
              right.captureExposures.filter(_.positionId == r.afterPositionId))
              .map(value => value.originalAttacker -> value.originalTarget).toSet
          else Set.empty[(RelationColoredPieceWitness, RelationColoredPieceWitness)]
          val exposed = if exposureSubjects.isEmpty then Nil else
            captureAvailability(lc, rc, Json.obj("matched_support_comparison_ids" -> List(l.id, r.id)), Some(exposureSubjects))
          val support = (l.targets.map(_.originalTarget) ++ r.targets.map(_.originalTarget)).distinct.flatMap { target =>
            def row(leftCount: Int, rightCount: Int, applications: Option[(JsObject, JsObject)] = None,
                targetCaptures: Option[(Event, Event)] = None,
                directRecaptures: (Option[LineContinuationInvestigation.DirectSupportRecapture],
                  Option[LineContinuationInvestigation.DirectSupportRecapture]) = None -> None) =
              val ls = left.supportAt(leftCount, l.afterActor, target)
              val rs = right.supportAt(rightCount, r.afterActor, target)
              val known = ls.state != "unknown" && rs.state != "unknown"
              val differs = ls.state != rs.state || l.afterActor != r.afterActor
              def direct(value: Option[LineContinuationInvestigation.DirectSupportRecapture]) =
                value.fold(Json.obj())(use => Json.obj("direct_recapture" -> Json.obj(
                  "loss_event_id" -> use.targetCapture.id, "recapture_event_id" -> use.recapture.id)))
              Option.when(known && differs && (ls.state == "present" || rs.state == "present"))(Json.obj(
                "kind" -> kind, "original_subject" -> piece(l.beforeActor), "original_target" -> piece(target),
                "relation" -> (if ls.state == rs.state then "preserved_with_different_provider" else "different_support"),
                "left" -> (at(left, leftCount) ++ Json.obj("original_provider" -> piece(l.afterActor), "support" -> ls.json,
                  "comparison_id" -> l.id, "trigger_occurrence_ids" -> l.triggerOccurrenceIds) ++
                    applications.fold(Json.obj())(value => Json.obj("application" -> value._1)) ++
                    targetCaptures.fold(Json.obj())(value => Json.obj("target_capture_event_id" -> value._1.id)) ++
                    direct(directRecaptures._1)),
                "right" -> (at(right, rightCount) ++ Json.obj("original_provider" -> piece(r.afterActor), "support" -> rs.json,
                  "comparison_id" -> r.id, "trigger_occurrence_ids" -> r.triggerOccurrenceIds) ++
                    applications.fold(Json.obj())(value => Json.obj("application" -> value._2)) ++
                    targetCaptures.fold(Json.obj())(value => Json.obj("target_capture_event_id" -> value._2.id)) ++
                    direct(directRecaptures._2))))
            val rightPoints = right.kingCaptureOpportunities(r, target).groupBy(_.key)
            val applications = for
              lp <- left.kingCaptureOpportunities(l, target)
              rp <- rightPoints.getOrElse(lp.key, Nil)
              if sameTurn(lp.count, rp.count) && lp.available != rp.available &&
                ((lp.support.state == "present") != (rp.support.state == "present"))
              // The positive support must actually coincide with the unavailable king capture.
              if (lp.support.state != "present" || !lp.available) && (rp.support.state != "present" || !rp.available)
              result <- row(lp.count, rp.count, Some(lp.json(rootId, left.path.positions(lp.count).id.get) ->
                rp.json(rootId, right.path.positions(rp.count).id.get))).toList
            yield (lp.count, rp.count, result)
            val leftUses = left.directSupportRecaptures.filter(use => use.comparison.id == l.id && use.originalTarget == target)
            val rightUses = right.directSupportRecaptures.filter(use => use.comparison.id == r.id && use.originalTarget == target)
            val directApplications = (leftUses ++ rightUses).map(_.originalCapturer).distinct.flatMap { capturer =>
              for
                le <- left.events.find(event => event.originalActor.contains(capturer) &&
                  event.originalVictim.contains(target) && event.plyOffset >= lc).toList
                re <- right.events.find(event => event.originalActor.contains(capturer) &&
                  event.originalVictim.contains(target) && event.plyOffset >= rc).toList
                if sameTurn(le.plyOffset, re.plyOffset)
                lu = leftUses.find(_.targetCapture.id == le.id)
                ru = rightUses.find(_.targetCapture.id == re.id)
                if lu.nonEmpty || ru.nonEmpty
                result <- row(le.plyOffset, re.plyOffset, targetCaptures = Some(le -> re),
                  directRecaptures = lu -> ru).toList
              yield (le.plyOffset, re.plyOffset, result)
            }
            val uses = applications ++ directApplications
            val baseline = Option.unless(uses.exists((a, b, _) => a == lc && b == rc))(row(lc, rc)).flatten
            baseline.toList ++ uses.map(_._3)
          }
          val recapturers = if kind == "replacement_support" then recapturerSupport(l, r, lc, rc) else Nil
          exposed ++ support ++ recapturers
      def uniqueBySubject(reading: LineContinuationInvestigation.Reading, kind: String) =
        reading.supportComparisons.filter(row => row.kind == kind &&
          (kind != "actor_continuation" || row.beforePositionId == rootId))
          .groupBy(_.beforeActor).collect { case (actor, row :: Nil) => actor -> row }
      val supportDifferences = List("replacement_after_capture", "actor_continuation").flatMap { kind =>
        val rightRows = uniqueBySubject(right, kind)
        uniqueBySubject(left, kind).toList.sortBy(_._1.square.key).flatMap { (actor, row) =>
          rightRows.get(actor).toList.flatMap(other => supportRows(
            if kind == "actor_continuation" then "root_actor_support" else "replacement_support", row, other))
        }
      }
      val differences = captureDifferences ++ followupDifferences ++ supportDifferences
      Option.when(differences.nonEmpty)(new Reading(packet, left, right, differences))
