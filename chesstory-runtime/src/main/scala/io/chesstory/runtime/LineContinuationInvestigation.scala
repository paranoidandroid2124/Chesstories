package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.{ LegalMoveResource, PositionFacts }
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.line.{ CandidateLineEvaluation, PositionRuleAssessment }
import play.api.libs.json.*

/** Physical identities and support observed along one certified line. The
  * material ledger, legal replay and geometric-control owners remain unchanged.
  */
private[runtime] object LineContinuationInvestigation:
  import ObservedLineProtocol.{ PreparedPath, PreparedPaths }

  case class ComparisonSource(workId: String, donorLineId: String, peerLineId: String,
      kind: String = "material_recovery", observationLineId: Option[String] = None):
    def json: JsObject = Json.obj("work_id" -> workId, "donor_line_id" -> donorLineId,
      "peer_line_id" -> peerLineId, "kind" -> kind, "observation_line_id" -> observationLineId)

  case class ChoicePremise(kind: String, triggerIds: List[String],
      comparisonSources: List[ComparisonSource] = Nil, requirementId: Option[String] = None)

  case class ChoicePoint(prefixMovesUci: List[String], positionId: String, premises: List[ChoicePremise]):
    def triggerIds: List[String] = premises.flatMap(_.triggerIds).distinct.sorted
    def kinds: List[String] = premises.map(_.kind).distinct.sorted
    def comparisonSources: List[ComparisonSource] = premises.flatMap(_.comparisonSources).distinct

  case class RequirementObservation(id: String, triggerIds: List[String], witnessLineIds: List[String])

  /** A checked finite endpoint needs an observed legal response even when its
    * material ledger is balanced or unavailable. Existing longer observations
    * discharge this premise in the reducer; they do not prove the best reply.
    */
  def checkResponsePoints(packet: EvidenceBackedJudgmentPacket, prepared: PreparedPaths)
      : List[(PreparedPath, ChoicePoint)] =
    prepared.requireOwner(packet)
    val views = prepared.resourceViews(packet)
    prepared.investigationPaths.flatMap { path =>
      for
        _ <- path.certified.toList
        if path.moves.nonEmpty && path.ids == path.declared
        position <- path.positions.lastOption.toList
        positionId <- position.id.toList
        side <- position.sideToMove.toList
        view <- views.get(positionId).toList
        if view.currentCheckSources(side).nonEmpty && view.legalRows.exists(_.nonEmpty)
        // The admitted after-position retains clocks and repetition history.
        // A draw can already be automatic while legal check evasions remain.
        last <- packet.assembly.lineReplays.get(path.line.ref).flatMap(_.legalSteps.lastOption).toList
        if PositionRuleAssessment.automaticTerminal(last.after).isEmpty
      yield path -> ChoicePoint(path.moves, positionId,
        List(ChoicePremise("check_response_not_observed", List(path.ids.last))))
    }

  case class ReplacementFunctionChoice(point: ChoicePoint, recaptureMoves: List[String],
      recoveryOccurrenceIds: List[String])

  case class ActorEnd(state: String, positionId: String,
      piece: Option[RelationColoredPieceWitness], removedByEventId: Option[String]):
    def json: JsObject = Json.obj("state" -> state, "position_id" -> positionId,
      "piece" -> piece.map(pieceJson(_, positionId)), "removed_by_event_id" -> removedByEventId)

  case class Event(id: String, plyOffset: Int, occurrenceId: String, sourceId: String,
      originalActor: Option[RelationColoredPieceWitness], originalVictim: Option[RelationColoredPieceWitness],
      actorEnd: ActorEnd):
    def json(rootPositionId: String): JsObject = Json.obj("id" -> id, "ply_offset" -> plyOffset,
      "occurrence_id" -> occurrenceId, "source_id" -> sourceId,
      "original_actor" -> originalActor.map(pieceJson(_, rootPositionId)),
      "original_victim" -> originalVictim.map(pieceJson(_, rootPositionId)), "actor_end" -> actorEnd.json)

  case class CaptureLink(lossEventId: String, captureEventId: String, elapsedPlies: Int):
    def json: JsObject = Json.obj("loss_event_id" -> lossEventId,
      "capture_event_id" -> captureEventId, "elapsed_plies" -> elapsedPlies)

  case class OtherTargetGain(lossEventId: String, gainEventId: String):
    def json: JsObject = Json.obj("loss_event_id" -> lossEventId, "gain_event_id" -> gainEventId)

  case class RecoveryLoss(lossEventId: String, recoveryEventId: String, laterLossEventId: String):
    def json: JsObject = Json.obj("loss_event_id" -> lossEventId, "recovery_event_id" -> recoveryEventId,
      "later_loss_event_id" -> laterLossEventId)

  case class Support(state: String, sourceIds: List[String]):
    def json: JsObject = Json.obj("state" -> state, "source_ids" -> sourceIds)

  case class SupportTarget(originalTarget: RelationColoredPieceWitness, before: Support, after: Support):
    def json(rootPositionId: String): JsObject = Json.obj(
      "original_target" -> pieceJson(originalTarget, rootPositionId), "before" -> before.json, "after" -> after.json)

  case class SupportComparison(id: String, kind: String, beforeActor: RelationColoredPieceWitness,
      afterActor: RelationColoredPieceWitness, beforePositionId: String, afterPositionId: String,
      triggerOccurrenceIds: List[String], targets: List[SupportTarget], complete: Boolean):
    def json(rootPositionId: String): JsObject = Json.obj("id" -> id, "kind" -> kind,
      "state" -> (if complete then "complete" else "unknown"),
      "before_actor" -> pieceJson(beforeActor, rootPositionId), "after_actor" -> pieceJson(afterActor, rootPositionId),
      "before_position_id" -> beforePositionId, "after_position_id" -> afterPositionId,
      "trigger_occurrence_ids" -> triggerOccurrenceIds, "targets" -> targets.map(_.json(rootPositionId)))

  case class KingCaptureOpportunity(count: Int, originalAttacker: RelationColoredPieceWitness,
      actor: RelationColoredPieceWitness, victim: RelationColoredPieceWitness,
      contacts: List[String], support: Support, moves: List[LegalMoveResource]):
    def key = (originalAttacker, actor, victim)
    def available: Boolean = moves.nonEmpty
    def json(rootPositionId: String, positionId: String): JsObject = Json.obj("kind" -> "king_capture",
      "original_attacker" -> pieceJson(originalAttacker, rootPositionId),
      "attacker" -> pieceJson(actor, positionId), "target" -> pieceJson(victim, positionId),
      "contact_source_ids" -> contacts, "moves_uci" -> moves.map(_.move),
      "source_ids" -> moves.map(_.sourceId), "legal_inventory_complete" -> true)

  final class Reading private[LineContinuationInvestigation] (private val owner: EvidenceBackedJudgmentPacket,
      val path: PreparedPath, val events: List[Event], val captureLinks: List[CaptureLink],
      val otherTargetGains: List[OtherTargetGain], val supportComparisons: List[SupportComparison],
      initialChoicePoints: List[ChoicePoint], val openLossEventIds: Set[String],
      val recoveryLosses: List[RecoveryLoss], val lossOrigins: Map[String, List[String]],
      val complete: Boolean, val captureExposures: List[CaptureExposure], private val prepared: PreparedPaths):
    val id: String = CommentarySelection.stableId("line-continuation", List(path.line.ref.id))
    def belongsTo(packet: EvidenceBackedJudgmentPacket): Boolean = owner eq packet
    def supportAt(count: Int, actor: RelationColoredPieceWitness, target: RelationColoredPieceWitness): Support =
      prepared.supportAt(owner, path, count, actor, target)
    private[LineContinuationInvestigation] lazy val materialEventById = events.map(event => event.id -> event).toMap
    private[LineContinuationInvestigation] lazy val capturesByVictim =
      events.flatMap(event => event.originalVictim.map(_ -> event)).groupMap(_._1)(_._2)
    private[LineContinuationInvestigation] lazy val linksByLoss = captureLinks.groupBy(_.lossEventId)
    private val kingOpportunities = scala.collection.mutable.Map.empty[
      (String, RelationColoredPieceWitness, RelationColoredPieceWitness), List[KingCaptureOpportunity]]
    private val rawKingOpportunities = scala.collection.mutable.Map.empty[
      (RelationColoredPieceWitness, RelationColoredPieceWitness), (Int, List[KingCaptureOpportunity])]
    private lazy val originalKings = prepared.identitiesAfter(owner, path, 0).map(_._1).filter(_.role.name == "king")
    /** First point for each actual king/target geometry and support/legal state.
      * Both discovery and selected-pair comparison use this exact path owner.
      */
    def kingCaptureOpportunities(comparison: SupportComparison,
        target: RelationColoredPieceWitness, throughCount: Int = path.moves.size): List[KingCaptureOpportunity] =
      require(supportComparisons.contains(comparison), "support opportunities need this exact path's comparison")
      val limit = throughCount.min(path.moves.size).max(0)
      kingOpportunities.synchronized {
        def observe(): List[KingCaptureOpportunity] =
          val base = path.positions.indexWhere(_.id.contains(comparison.afterPositionId))
          if base < 0 || base > limit then Nil else
            val key = comparison.afterActor -> target
            val (scanned, previous) = rawKingOpportunities.getOrElse(key, -1 -> Nil)
            // A bounded caller must not force the rest of a long owned path. A
            // later wider caller reads only the suffix that has not been visited.
            val points = if scanned >= limit then previous else
              val views = prepared.resourceViews(owner)
              val added = originalKings.filter(_.side != target.side).flatMap { king =>
                (scanned + 1 to limit).toList.flatMap { count =>
                  (for
                    position <- path.positions.lift(count)
                    if position.sideToMove.contains(king.side)
                    actor <- prepared.pieceAfter(owner, path, king, count).flatten
                    victim <- prepared.pieceAfter(owner, path, target, count).flatten
                    view <- position.id.flatMap(views.get)
                    sources = view.controlSources(actor, victim.square)
                    if sources.nonEmpty
                    legal <- view.legalRows
                    support = supportAt(count, comparison.afterActor, target)
                    if support.state != "unknown"
                  yield KingCaptureOpportunity(count, king, actor, victim, sources, support,
                    legal.filter(move => move.actor == actor && move.capture.contains(victim)))).toList
                }
              }
              val accumulated = previous ++ added
              rawKingOpportunities.update(key, limit -> accumulated)
              accumulated
            points.filter(point => point.count >= base && point.count <= limit)
              .distinctBy(point => (point.key, point.support.state, point.available))
        if limit == path.moves.size then
          kingOpportunities.getOrElseUpdate((comparison.afterPositionId, comparison.afterActor, target), observe())
        else observe()
      }
    lazy val supportApplications: List[(SupportComparison, RelationColoredPieceWitness, KingCaptureOpportunity)] =
      supportComparisons.flatMap(comparison => comparison.targets.flatMap { target =>
        kingCaptureOpportunities(comparison, target.originalTarget)
          .filter(point => point.support.state == "present" && !point.available)
          .map(point => (comparison, target.originalTarget, point))
      })
    lazy val directSupportRecaptures: List[DirectSupportRecapture] =
      readDirectSupportRecaptures(owner, this, prepared)
    private lazy val exposureChoices: List[ChoicePoint] = captureExposures.flatMap { exposure =>
      val response = ChoicePoint(exposure.prefixMoves, exposure.positionId,
        List(ChoicePremise("capture_exposure_response", List(exposure.occurrenceId))))
      // Availability alone does not condemn the preceding move. Reconsider
      // that choice when this exact reply is actually observed to leave a loss.
      val count = exposure.prefixMoves.size
      val losses = events.filter(event => event.plyOffset == count &&
        event.originalActor.contains(exposure.originalAttacker) &&
        event.originalVictim.contains(exposure.originalTarget) &&
        openLossEventIds(event.id))
      val prevention = Option.when(count > 1 && losses.nonEmpty)(ChoicePoint(
        path.moves.take(count - 1), exposure.beforePositionId,
        List(ChoicePremise("exposure_prevention_choice", exposure.occurrenceId :: losses.map(_.occurrenceId)))))
      response :: prevention.toList
    }
    lazy val replacementFunctionChoices: List[ReplacementFunctionChoice] =
      val views = prepared.resourceViews(owner)
      supportComparisons.filter(_.kind == "replacement_after_capture").flatMap { comparison =>
        val count = path.positions.indexWhere(_.id.contains(comparison.afterPositionId)) - 1
        supportPremises(this, count).filter(_.subject == comparison.beforeActor).flatMap { premise => (for
          recovery <- events.find(event => event.plyOffset == count && event.originalActor.contains(comparison.afterActor))
          originalCapturer <- recovery.originalVictim
          capturer <- prepared.pieceAfter(owner, path, originalCapturer, count).flatten
          position <- path.positions.lift(count)
          legal <- position.id.flatMap(views.get).flatMap(_.legalRows)
          recaptures = legal.filter(_.capture.contains(capturer))
          identities = prepared.identitiesAfter(owner, path, count)
          actors = recaptures.flatMap(row => identities.collectFirst {
            case (original, Some(Some(current))) if current == row.actor => original
          })
          if count > 0 && count < 80 && actors.size == recaptures.size && actors.distinct.size >= 2
        yield ReplacementFunctionChoice(ChoicePoint(path.moves.take(count), position.id.get,
          List(ChoicePremise("replacement_function_choice", premise.triggerIds, requirementId = Some(premise.id)))),
          recaptures.map(_.move).distinct.sorted, List(recovery.occurrenceId))).toList }
      }
    lazy val choicePoints: List[ChoicePoint] =
      (initialChoicePoints ++ replacementFunctionChoices.map(_.point) ++ exposureChoices).groupBy(_.prefixMovesUci).values.map { points =>
        points.head.copy(premises = points.flatMap(_.premises).distinct)
      }.toList.sortBy(point => (point.prefixMovesUci.size, point.prefixMovesUci.mkString(" ")))
    def candidateJson(packet: EvidenceBackedJudgmentPacket): JsObject =
      require(belongsTo(packet), "a continuation reading belongs to its exact packet")
      val rootPositionId = path.positions.head.id.get
      Json.obj("id" -> id, "kind" -> "conditional_line", "selection" -> "context",
        "root_occurrence_id" -> packet.assembly.playedTransition.get.evidence.id,
        "occurrence_ids" -> (packet.assembly.playedTransition.get.evidence.id :: path.ids).distinct,
        "position_ids" -> path.positions.flatMap(_.id).filter(prepared.resourceIds).distinct,
        "line_ids" -> List(path.line.ref.id), "evidence_ids" -> path.certified.map(_._1.ref.id).toList,
        "source_refs" -> List(Json.obj("line_id" -> path.line.ref.id, "kind" -> "admitted",
          "source" -> "legal_replay", "accepted_ply_count" -> path.moves.size, "stop" -> JsNull)),
        "detail" -> Json.obj("type" -> "line_continuation", "scope" -> "observed_line",
          "line_id" -> path.line.ref.id, "events" -> events.map(_.json(rootPositionId)),
          "capture_links" -> captureLinks.map(_.json), "other_target_gains" -> otherTargetGains.map(_.json),
          "open_loss_event_ids" -> openLossEventIds.toList.sorted,
          "recovery_losses" -> recoveryLosses.map(_.json),
          "support_comparisons" -> supportComparisons.map(_.json(rootPositionId)),
          "capture_exposures" -> captureExposures.map(_.json(rootPositionId)),
          "direct_support_recaptures" -> directSupportRecaptures.map(_.json(rootPositionId)),
          "support_applications" -> supportApplications.map { (comparison, target, point) =>
            val positionId = path.positions(point.count).id.get
            Json.obj("comparison_id" -> comparison.id, "original_target" -> pieceJson(target, rootPositionId),
              "requirement_id" -> Option.when(comparison.kind == "replacement_after_capture")(
                supportRequirementId("king_capture", comparison.beforeActor, target, point.key)),
              "original_provider" -> pieceJson(comparison.afterActor, rootPositionId),
              "ply_count" -> point.count, "position_id" -> positionId,
              "preceding_occurrence_id" -> path.ids.lift(point.count - 1), "support" -> point.support.json,
              "application" -> point.json(rootPositionId, positionId))
          },
          "text" -> "These are physical-piece and direct-support observations within the supplied line. "
            .concat("A later gain is not proof of sufficient compensation, and geometric support is not a forced defense. ")
            .concat("Survival and missing recovery are bounded by this line's end; the existing line summary owns material values and terminal facts.")))

  def readAll(packet: EvidenceBackedJudgmentPacket, prepared: PreparedPaths,
      previous: List[Reading] = Nil): List[Reading] =
    prepared.requireOwner(packet)
    val retained = previous.filter(_.belongsTo(packet)).map(value => value.path.line.ref.id -> value).toMap
    prepared.investigationPaths.flatMap { path =>
      prepared.continuationReading(packet, path) {
        retained.get(path.line.ref.id).filter(_.path eq path).orElse(read(packet, path, prepared))
      }
    }

  /** Returned recaptures can still end before a named support use. Search coverage
    * does not supply that observation; exact longer descendants and physical losses do.
    */
  def unresolvedExposures(packet: EvidenceBackedJudgmentPacket, prefixMoves: List[String],
      triggerOccurrenceIds: List[String], sourceLineIds: List[String], ends: List[PreparedPath],
      prepared: PreparedPaths): List[String] =
    prepared.requireOwner(packet)
    val readings = prepared.continuationReadings
    val premises = readings.filter(reading => sourceLineIds.contains(reading.path.line.ref.id))
      .flatMap(_.captureExposures).filter(exposure => exposure.prefixMoves == prefixMoves &&
        triggerOccurrenceIds.contains(exposure.occurrenceId))
      .map(exposure => exposure.originalAttacker -> exposure.originalTarget).toSet
    ends.flatMap { path =>
      readings.find(reading => (reading.path eq path) && path.moves.startsWith(prefixMoves)).filter { reading =>
        reading.events.filter(event => event.plyOffset == prefixMoves.size &&
          event.originalActor.zip(event.originalVictim).exists(premises)).exists { capture =>
          reading.events.exists(event => reading.openLossEventIds(event.id) &&
            reading.lossOrigins.getOrElse(event.occurrenceId, List(event.occurrenceId)).contains(capture.occurrenceId))
        }
      }.map(_.path.line.ref.id)
    }.distinct.sorted

  def unresolvedReplacementFunctions(packet: EvidenceBackedJudgmentPacket, prefixMoves: List[String],
      triggerOccurrenceIds: List[String], sourceLineIds: List[String], ends: List[PreparedPath],
      prepared: PreparedPaths, requirementId: Option[String] = None): List[String] =
    prepared.requireOwner(packet)
    val readings = prepared.continuationReadings.map(reading => reading.path.line.ref.id -> reading).toMap
    val premises = readings.valuesIterator.filter(reading => sourceLineIds.contains(reading.path.line.ref.id) &&
      reading.path.moves.startsWith(prefixMoves)).flatMap { reading =>
      supportPremises(reading, prefixMoves.size).filter(premise =>
        premise.triggerIds.forall(triggerOccurrenceIds.contains) && requirementId.forall(_ == premise.id))
    }.toList.distinct
    ends.flatMap(path => readings.get(path.line.ref.id).filter(reading =>
      (reading.path eq path) && path.moves.startsWith(prefixMoves) &&
        premises.exists(premise => observesSupportUse(reading, premise, prefixMoves.size).contains(false)))
      .map(_.path.line.ref.id)).distinct.sorted

  private case class SupportPremise(subject: RelationColoredPieceWitness, target: RelationColoredPieceWitness,
      key: (RelationColoredPieceWitness, RelationColoredPieceWitness, RelationColoredPieceWitness),
      triggerIds: List[String], kind: String = "king_capture"):
    def id: String = supportRequirementId(kind, subject, target, key)

  private def supportRequirementId(kind: String, subject: RelationColoredPieceWitness,
      target: RelationColoredPieceWitness,
      key: (RelationColoredPieceWitness, RelationColoredPieceWitness, RelationColoredPieceWitness)): String =
    CommentarySelection.stableId("support-requirement",
      kind :: List(subject, target, key._1, key._2, key._3).flatMap(piece =>
        List(piece.side.toString, piece.role.name, piece.square.key)))

  private def supportPremises(reading: Reading, count: Int): List[SupportPremise] =
    val kings = reading.supportApplications.filter { (comparison, _, _) =>
      comparison.kind == "replacement_after_capture" &&
        reading.path.positions.indexWhere(_.id.contains(comparison.afterPositionId)) == count + 1
    }.map { (comparison, target, point) =>
      SupportPremise(comparison.beforeActor, target, point.key,
        (comparison.triggerOccurrenceIds ++ reading.path.ids.lift(point.count - 1))
          .flatMap(id => reading.lossOrigins.getOrElse(id, List(id))).distinct.sorted)
    }
    val recaptures = reading.directSupportRecaptures.filter(use =>
      reading.path.positions.indexWhere(_.id.contains(use.comparison.afterPositionId)) == count + 1).map { use =>
      SupportPremise(use.comparison.beforeActor, use.originalTarget,
        (use.originalCapturer, use.capturer, use.target),
        (use.comparison.triggerOccurrenceIds :+ use.recapture.occurrenceId)
          .flatMap(id => reading.lossOrigins.getOrElse(id, List(id))).distinct.sorted, "direct_recapture")
    }
    (kings ++ recaptures).distinct

  private def observesSupportUse(reading: Reading, premise: SupportPremise, count: Int): Option[Boolean] =
    reading.supportComparisons.find(comparison => comparison.kind == "replacement_after_capture" &&
      comparison.beforeActor == premise.subject &&
      reading.path.positions.indexWhere(_.id.contains(comparison.afterPositionId)) == count + 1).map { comparison =>
      // An actual loss is a different observed outcome. It does not imply that
      // another provider or every later king approach has also been examined.
      val laterLosses = reading.events.filter(event => event.plyOffset > count && event.originalVictim.nonEmpty)
      premise.kind match
        case "king_capture" =>
          laterLosses.exists(_.originalVictim.exists(piece => piece == premise.target || piece == comparison.afterActor)) ||
            reading.kingCaptureOpportunities(comparison, premise.target).exists(_.key == premise.key)
        case "direct_recapture" =>
          val targetLoss = laterLosses.find(_.originalVictim.contains(premise.target))
          // Removing the provider before the named attack, or losing the target
          // to another piece, is a concrete different outcome. A short line just
          // after the named capture still owes observation of the recapture.
          laterLosses.exists(event => event.originalVictim.contains(comparison.afterActor) &&
            targetLoss.forall(_.plyOffset > event.plyOffset)) || targetLoss.exists { loss =>
            !loss.originalActor.contains(premise.key._1) || reading.captureLinks.exists(_.lossEventId == loss.id)
          }
        case _ => false
    }

  private def supportWitnesses(readings: List[Reading], peer: Reading, premise: SupportPremise, count: Int): List[String] =
    readings.filter(reading => reading.path.ids.startsWith(peer.path.ids) &&
      observesSupportUse(reading, premise, count).contains(true)).map(_.path.line.ref.id).distinct.sorted

  private def transferredSupportPremises(donor: Reading, observation: Reading, peer: Reading,
      prefix: List[String]): Option[(Int, List[SupportPremise])] =
    val count = donor.path.ids.zip(peer.path.ids).takeWhile((left, right) => left == right).size
    val family = observation.replacementFunctionChoices.exists(choice =>
      choice.point.prefixMovesUci == donor.path.moves.take(count) &&
        List(donor.path.moves.lift(count), peer.path.moves.lift(count)).flatten.forall(choice.recaptureMoves.contains))
    Option.when(count > 0 && count < donor.path.moves.size && count < peer.path.moves.size &&
      peer.path.moves == prefix && observation.path.ids.startsWith(donor.path.ids) && family) {
      count -> supportPremises(observation, count).filter(premise => observesSupportUse(peer, premise, count).nonEmpty)
    }.filter(_._2.nonEmpty)

  def unresolvedSupportContinuations(packet: EvidenceBackedJudgmentPacket, prefixMoves: List[String],
      sources: List[ComparisonSource], ends: List[PreparedPath], prepared: PreparedPaths,
      requirementId: Option[String] = None): List[String] =
    prepared.requireOwner(packet)
    val readings = prepared.continuationReadings.map(reading => reading.path.line.ref.id -> reading).toMap
    sources.filter(_.kind == "support_application").flatMap { source =>
      (for
        donor <- readings.get(source.donorLineId)
        peer <- readings.get(source.peerLineId)
        observation <- source.observationLineId.flatMap(readings.get)
        (count, premises) <- transferredSupportPremises(donor, observation, peer, prefixMoves)
      yield ends.flatMap(path => readings.get(path.line.ref.id).filter(reading => (reading.path eq path) &&
        path.ids.startsWith(peer.path.ids) && premises.filter(premise => requirementId.forall(_ == premise.id)).exists(premise =>
          observesSupportUse(reading, premise, count).contains(false))).map(_.path.line.ref.id))).toList.flatten
    }.distinct.sorted

  case class RefutingMoveClass(movesUci: List[String], premiseOccurrenceIds: List[String], sourceLineIds: List[String],
      kind: String = "recovery_route")

  /** These legal replies remove a recovery's performer or move a victim that
    * stays put until that recorded capture. They challenge that particular route,
    * not every possible recovery or a different capture on a different square.
    * Reuse legal rows and identity tracking; do not simulate a second move tree.
    */
  def refutingMoves(packet: EvidenceBackedJudgmentPacket, prefixMoves: List[String],
      sourceLineIds: List[String], triggerOccurrenceIds: List[String], prepared: PreparedPaths): List[String] =
    refutingMoveClasses(packet, prefixMoves, sourceLineIds, triggerOccurrenceIds, prepared)
      .filter(_.kind == "recovery_route").flatMap(_.movesUci).distinct.sorted

  def refutingMoveClasses(packet: EvidenceBackedJudgmentPacket, prefixMoves: List[String],
      sourceLineIds: List[String], triggerOccurrenceIds: List[String], prepared: PreparedPaths): List[RefutingMoveClass] =
    prepared.requireOwner(packet)
    val classes = prepared.continuationReadings.filter(reading => sourceLineIds.contains(reading.path.line.ref.id) &&
      reading.path.moves.startsWith(prefixMoves)).flatMap { reading =>
      val count = prefixMoves.size
      val futureRecoveries = (reading.captureLinks.map(_.captureEventId) ++
        reading.otherTargetGains.map(_.gainEventId)).toSet
      val recoveries = reading.events.filter(event => event.plyOffset >= count && futureRecoveries(event.id) &&
        triggerOccurrenceIds.contains(event.occurrenceId))
      val rows = reading.path.positions.lift(count).flatMap(_.id).flatMap(prepared.resourceViews(packet).get)
        .flatMap(_.legalRows).toList.flatten
      val routes = recoveries.flatMap { event =>
        val performer = event.originalActor.flatMap(actor => prepared.pieceAfter(packet, reading.path, actor, count).flatten)
        val stationaryVictim = event.originalVictim.flatMap { victim =>
          prepared.pieceAfter(packet, reading.path, victim, count).flatten.filter(current =>
            (count to event.plyOffset).forall(at => prepared.pieceAfter(packet, reading.path, victim, at).flatten.contains(current)))
        }
        val moves = rows.filter(row => row.capture.exists(value => performer.contains(value)) ||
          stationaryVictim.contains(row.actor)).map(_.move).distinct.sorted
        Option.when(moves.nonEmpty)(RefutingMoveClass(moves, List(event.occurrenceId), List(reading.path.line.ref.id)))
      }
      val recaptures = reading.replacementFunctionChoices.filter(choice => choice.point.prefixMovesUci == prefixMoves &&
        choice.point.triggerIds.forall(triggerOccurrenceIds.contains)).flatMap { choice =>
        choice.recaptureMoves.map(move => RefutingMoveClass(List(move), choice.recoveryOccurrenceIds,
          List(reading.path.line.ref.id), "replacement_recapture"))
      }
      val exposures = reading.captureExposures.filter(exposure => exposure.prefixMoves == prefixMoves &&
        triggerOccurrenceIds.contains(exposure.occurrenceId)).map(exposure =>
        RefutingMoveClass(exposure.captures.map(_.move).distinct.sorted, List(exposure.occurrenceId),
          List(reading.path.line.ref.id), "exposed_capture"))
      routes ++ recaptures ++ exposures
    }
    // Identical legal experiments share work, while their individual recovery premises stay visible.
    classes.groupBy(group => group.kind -> group.movesUci).toList.map { case ((kind, moves), matching) =>
      RefutingMoveClass(moves, matching.flatMap(_.premiseOccurrenceIds).distinct.sorted,
        matching.flatMap(_.sourceLineIds).distinct.sorted, kind)
    }.sortBy(group => (group.kind, group.movesUci.mkString(" ")))

  /** Read the unresolved material choices in the returned representatives. A
    * witnessed follow-up is a question for its peer, never that peer's answer.
    * A legal capture at an open loss also supplies a question without a donor PV.
    */
  def unobservedContinuations(packet: EvidenceBackedJudgmentPacket, search: TargetResponseInvestigation.Search,
      prepared: PreparedPaths): List[(Reading, ChoicePoint)] =
    prepared.requireOwner(packet)
    if !search.belongsTo(packet) then Nil
    else
      val readings = prepared.continuationReadings.map(value => value.path.line.ref.id -> value).toMap
      val count = search.anchor.plyOffset
      val views = prepared.resourceViews(packet)
      val selected = search.selectedLinePairs
      def openEnd(reading: Reading): Boolean =
        reading.path.moves.size > count && reading.path.moves.startsWith(search.anchor.prefixMoves) &&
          views.get(reading.path.positions.last.id.get).flatMap(_.legalRows).exists(_.nonEmpty) &&
          !search.lines.find(_.evaluation.moves.head == reading.path.moves(count)).exists(_.evaluation match
            case CandidateLineEvaluation.ExactAutomaticTerminal(_, _) => true
            case _ => false)
      val transferred = selected.flatMap { (left, right) => List(left -> right, right -> left) }.flatMap { (donorPath, peerPath) =>
        for
          donor <- readings.get(donorPath.line.ref.id).filter(_.path eq donorPath).toList
          peer <- readings.get(peerPath.line.ref.id).filter(_.path eq peerPath).toList
          if donor.path.moves.startsWith(search.anchor.prefixMoves) && openEnd(peer)
          position <- peer.path.positions.last.id.toList
          (loss, removal) <- recoveryPremises(donor, peer, peer.path.moves)
          if !followupWitnesses(readings.values.toList, peer, loss, removal).nonEmpty
        yield peer -> ChoicePoint(peer.path.moves, position,
          List(ChoicePremise("continuation_not_observed",
            List(loss.occurrenceId, removal.occurrenceId, peer.path.ids.last).distinct,
            List(ComparisonSource(search.work.workId, donor.path.line.ref.id, peer.path.line.ref.id)),
            Some(recoveryRequirementId(loss, removal)))))
      }
      val families = readings.valuesIterator.flatMap(_.replacementFunctionChoices)
        .filter(choice => choice.point.prefixMovesUci == search.anchor.prefixMoves &&
          choice.point.positionId == search.anchor.positionId).map(_.recaptureMoves).toList.distinct
      val supportTransferred = families.flatMap(search.selectedMovePairs).distinct
        .flatMap((left, right) => List(left -> right, right -> left)).flatMap { (donorPath, peerPath) =>
          for
            donor <- readings.get(donorPath.line.ref.id).filter(_.path eq donorPath).toList
            peer <- readings.get(peerPath.line.ref.id).filter(reading => (reading.path eq peerPath) && openEnd(reading)).toList
            observation <- readings.valuesIterator.filter(_.path.ids.startsWith(donor.path.ids))
              .toList.sortBy(reading => (reading.path.moves.size, reading.path.line.ref.id))
            (originCount, premises) <- transferredSupportPremises(donor, observation, peer, peer.path.moves).toList
            premise <- premises
            if supportWitnesses(readings.values.toList, peer, premise, originCount).isEmpty
          yield peer -> ChoicePoint(peer.path.moves, peer.path.positions.last.id.get,
            List(ChoicePremise("support_application_not_observed", (premise.triggerIds :+ peer.path.ids.last).distinct.sorted,
              List(ComparisonSource(search.work.workId, donor.path.line.ref.id, peer.path.line.ref.id,
                "support_application", Some(observation.path.line.ref.id))), Some(premise.id))))
        }
      val legalQuestions = selected.flatMap { (left, right) => List(left, right) }.distinct.flatMap { path =>
        readings.get(path.line.ref.id).filter(reading => (reading.path eq path) && openEnd(reading)).toList.flatMap { reading =>
          val position = path.positions.last.id.get
          val view = views(position)
          val rows = view.legalRows.get
          val attacked = path.positions.last.sideToMove.toList.flatMap(view.attackedTargets).map(_._1).toSet
          reading.events.filter(loss => reading.openLossEventIds(loss.id)).flatMap { loss =>
            // The complete legal pool, including quiet alternatives, is searched.
            // A legal capture or check supplies a concrete response proposal,
            // not sufficient compensation or a preferred forcing move.
            val gain = Option.when(loss.originalVictim.exists(victim =>
              path.positions.last.sideToMove.contains(victim.side)) &&
                (rows.exists(_.capture.nonEmpty) || view.checkingMoves.exists(_.nonEmpty)) &&
                materialWitnesses(readings.values.toList, reading, loss).isEmpty)(
              reading -> ChoicePoint(path.moves, position,
                List(ChoicePremise("material_followup",
                  (reading.lossOrigins.getOrElse(loss.occurrenceId, List(loss.occurrenceId)) :+ path.ids.last).distinct))))
            val threatenedCapturer = Option.when(loss.actorEnd.piece.exists(piece =>
              path.positions.last.sideToMove.contains(piece.side) && attacked(piece)))(
              reading -> ChoicePoint(path.moves, position,
                List(ChoicePremise("capturer_reply",
                  (reading.lossOrigins.getOrElse(loss.occurrenceId, List(loss.occurrenceId)) :+ path.ids.last).distinct))))
            gain.toList ++ threatenedCapturer.toList
          }
        }
      }
      (transferred ++ supportTransferred ++ legalQuestions).distinct

  /** Retain each transferred requirement even when its exact witness is missing.
    * None means a source requirement could not be reconstructed, not an observed absence.
    * Another
    * physical provider can make the capture. A different observed recovery can
    * answer the material comparison without pretending to be the donor's event.
    * Neither observation nor a balanced ledger certifies tactical stability.
    */
  def continuationRequirements(packet: EvidenceBackedJudgmentPacket, prefixMoves: List[String],
      sources: List[ComparisonSource], prepared: PreparedPaths): Option[List[RequirementObservation]] =
    prepared.requireOwner(packet)
    val readings = prepared.continuationReadings.map(value => value.path.line.ref.id -> value).toMap
    val requirements = sources.map { source =>
      for
        donor <- readings.get(source.donorLineId)
        peer <- readings.get(source.peerLineId)
        answers <- source.kind match
          case "material_recovery" =>
            val premises = recoveryPremises(donor, peer, prefixMoves)
            Option.when(premises.nonEmpty)(premises.map { (loss, recovery) =>
              RequirementObservation(recoveryRequirementId(loss, recovery),
                List(loss.occurrenceId, recovery.occurrenceId, peer.path.ids.last).distinct.sorted,
                followupWitnesses(readings.values.toList, peer, loss, recovery))
            })
          case "support_application" =>
            for
              observation <- source.observationLineId.flatMap(readings.get)
              (count, premises) <- transferredSupportPremises(donor, observation, peer, prefixMoves)
            yield premises.map(premise => RequirementObservation(premise.id,
              (premise.triggerIds :+ peer.path.ids.last).distinct.sorted,
              supportWitnesses(readings.values.toList, peer, premise, count)))
          case _ => None
      yield answers
    }
    Option.when(sources.nonEmpty && requirements.forall(_.nonEmpty))(
      requirements.flatten.flatten.distinct.sortBy(_.id))

  def observedContinuations(packet: EvidenceBackedJudgmentPacket, prefixMoves: List[String],
      sources: List[ComparisonSource], prepared: PreparedPaths): Option[List[String]] =
    continuationRequirements(packet, prefixMoves, sources, prepared).filter(_.forall(_.witnessLineIds.nonEmpty))
      .map(_.flatMap(_.witnessLineIds).distinct.sorted)

  private def recoveryRequirementId(loss: Event, recovery: Event): String =
    CommentarySelection.stableId("recovery-requirement", List(loss.occurrenceId, recovery.occurrenceId))

  /** Link one comparison arm's existing facts to this pending result, never discharge it.
    * Resolve the named donor once per inquiry. Shared loss triggers alone cannot
    * connect a different gain, and a support target is not the captured attacker.
    */
  def conditionTests(packet: EvidenceBackedJudgmentPacket, prepared: PreparedPaths,
      inquiries: List[CommentaryContinuationInquiry], searches: List[TargetResponseInvestigation.Search] = Nil)
      : Map[String, List[(CommentaryProposal.Fact, CommentaryFact.Result)] => Boolean] =
    prepared.requireOwner(packet)
    if inquiries.isEmpty then Map.empty else
      val readings = prepared.continuationReadings.map(value => value.path.line.ref.id -> value).toMap
      val paths = prepared.paths.map(path => path.line.ref.id -> path).toMap
      val returned = searches.filter(_.belongsTo(packet)).map(search =>
        search.work.workId -> search.pathsByMove.values.map(_.line.ref.id).toSet).toMap
      inquiries.map(inquiry => inquiry.question.id -> conditionTest(inquiry, readings, paths, returned)).toMap

  private def conditionTest(inquiry: CommentaryContinuationInquiry, readings: Map[String, Reading],
      paths: Map[String, PreparedPath], returned: Map[String, Set[String]])
      : List[(CommentaryProposal.Fact, CommentaryFact.Result)] => Boolean =
    val question = inquiry.question
    def piece(value: RelationColoredPieceWitness) = CommentaryFact.Piece(
      if value.side.white then "white" else "black", value.role.name, value.square.key)
    case class Witness(path: PreparedPath, count: Int, conditions: List[(CommentaryFact.Predicate, String)])
    def recovery(path: PreparedPath, event: Event): List[Witness] = event.originalVictim.map(target =>
      Witness(path, event.plyOffset + 1, List(CommentaryFact.Predicate("presence", Some(piece(target))) -> "removed"))).toList
    def support(reading: Reading, premise: SupportPremise): List[Witness] = premise.kind match
      case "direct_recapture" => reading.directSupportRecaptures.filter(use =>
        supportRequirementId("direct_recapture", use.comparison.beforeActor, use.originalTarget,
          (use.originalCapturer, use.capturer, use.target)) == premise.id).map(use =>
        Witness(reading.path, use.recapture.plyOffset + 1, List(CommentaryFact.Predicate("direct_support_use",
          Some(piece(use.comparison.afterActor)), Some(piece(use.originalTarget))) -> "true")))
      case "king_capture" => reading.supportApplications.filter { (comparison, target, point) =>
        comparison.kind == "replacement_after_capture" &&
          supportRequirementId("king_capture", comparison.beforeActor, target, point.key) == premise.id
      }.map { (comparison, target, point) =>
        Witness(reading.path, point.count, List(
          CommentaryFact.Predicate("legal_capture", Some(piece(point.originalAttacker)), Some(piece(target))) -> "illegal",
          CommentaryFact.Predicate("support", Some(piece(comparison.afterActor)), Some(piece(target))) -> "present"))
      }
      case _ => Nil
    val witnesses = inquiry.premiseObservations.filter(observation =>
      observation.observationState == "unobserved" ||
        (observation.observationState == "not_applicable" && inquiry.state == "deferred")).flatMap { observation =>
      val premise = observation.premise
      val origin = premise.origin
      val transferred = origin.comparisonSources.filter(source =>
        returned.get(source.workId).exists(ids => ids(source.donorLineId) && ids(source.peerLineId))).flatMap { source =>
        (for
          donor <- readings.get(source.donorLineId)
          peer <- readings.get(source.peerLineId)
          if peer.path.ids == question.prefixOccurrenceIds &&
            peer.path.positions.last.id.contains(question.positionId)
        yield source.kind match
          case "material_recovery" => recoveryPremises(donor, peer, question.prefixMoves).filter { (loss, gain) =>
            origin.requirementId.contains(recoveryRequirementId(loss, gain)) &&
              List(loss.occurrenceId, gain.occurrenceId, peer.path.ids.last).forall(origin.triggerIds.contains)
          }.flatMap((_, gain) => recovery(donor.path, gain))
          case "support_application" => source.observationLineId.flatMap(readings.get).toList.flatMap { observed =>
            transferredSupportPremises(donor, observed, peer, question.prefixMoves).toList.flatMap { (_, premises) =>
              premises.filter(value => origin.requirementId.contains(value.id) &&
                value.triggerIds.forall(origin.triggerIds.contains)).flatMap(support(observed, _))
            }
          }
          case _ => Nil).toList.flatten
      }
      val local = if origin.comparisonSources.nonEmpty then Nil else premise.sourceLineIds.flatMap(readings.get)
        .filter(reading => reading.path.ids.startsWith(question.prefixOccurrenceIds) &&
          reading.path.positions.lift(question.prefixMoves.size).flatMap(_.id).contains(question.positionId))
        .flatMap { reading =>
          if origin.kind == "replacement_function_choice" then
            supportPremises(reading, question.prefixMoves.size).filter(value =>
              origin.requirementId.contains(value.id) && value.triggerIds.forall(origin.triggerIds.contains))
              .flatMap(support(reading, _))
          else if Set("delayed_capturer_removal", "other_target_gain")(origin.kind) then
            reading.events.filter(loss => origin.triggerIds.contains(loss.occurrenceId))
              .flatMap(recoveryEvents(reading, _)).filter(recovery =>
                recovery.plyOffset >= question.prefixMoves.size && origin.triggerIds.contains(recovery.occurrenceId))
              .flatMap(event => recovery(reading.path, event))
          else Nil
        }
      transferred ++ local
    }.distinct
    // A sample's resolved point works for both ply and event selectors. Its moves
    // may include a longer declared suffix; that length is not the observed time.
    // Join all atoms at one witnessed prefix, never across arms, times or histories.
    facts => witnesses.exists { witness =>
      witness.conditions.forall { (predicate, expected) =>
        facts.exists { (fact, result) =>
          fact.predicate == predicate && fact.expected.forall(_ == expected) &&
            Set("supported", "observed")(result.state) && result.samples.exists(sample =>
              sample.state == "known" && sample.value.contains(expected) &&
                sample.fromPly == witness.count && sample.throughPly == witness.count &&
                result.proofs.exists(proof => proof.requirementIds.contains(fact.id) && proof.sampleIds.contains(sample.id)) &&
                paths.get(sample.lineId).exists(owned => owned.certified.nonEmpty && owned.ids == owned.declared &&
                  sample.moves.size >= witness.count && sample.moves == owned.moves.take(sample.moves.size) &&
                  owned.ids.take(witness.count) == witness.path.ids.take(witness.count)))
        }
      }
    }

  /** Reuse an already recorded recovery below the exact legal-capture question.
    * A partial gain is an observed answer to this local probe, while any distinct
    * expected recovery and the remaining material deficit stay independently open.
    */
  def materialContinuationRequirements(packet: EvidenceBackedJudgmentPacket, prefixMoves: List[String],
      triggerOccurrenceIds: List[String], prepared: PreparedPaths): Option[List[RequirementObservation]] =
    prepared.requireOwner(packet)
    val readings = prepared.continuationReadings
    val requirements = readings.filter(_.path.moves == prefixMoves).flatMap { peer =>
      peer.events.filter(loss => peer.openLossEventIds(loss.id) && triggerOccurrenceIds.contains(loss.occurrenceId))
        .map(loss => loss.occurrenceId -> materialWitnesses(readings, peer, loss))
    }.groupMap(_._1)(_._2).toList.map { (lossOccurrence, witnesses) =>
      RequirementObservation(CommentarySelection.stableId("material-requirement", List(lossOccurrence)),
        List(lossOccurrence), witnesses.flatten.distinct.sorted)
    }.sortBy(_.id)
    Option.when(requirements.nonEmpty)(requirements)

  def observedMaterialContinuations(packet: EvidenceBackedJudgmentPacket, prefixMoves: List[String],
      triggerOccurrenceIds: List[String], prepared: PreparedPaths): Option[List[String]] =
    materialContinuationRequirements(packet, prefixMoves, triggerOccurrenceIds, prepared)
      .filter(_.forall(_.witnessLineIds.nonEmpty)).map(_.flatMap(_.witnessLineIds).distinct.sorted)

  def recoveryEvents(reading: Reading, loss: Event): List[Event] =
    val ids = reading.captureLinks.filter(_.lossEventId == loss.id).map(_.captureEventId) ++
      reading.otherTargetGains.filter(_.lossEventId == loss.id).map(_.gainEventId)
    ids.distinct.flatMap(id => reading.events.find(_.id == id))

  private def materialWitnesses(readings: List[Reading], peer: Reading, loss: Event): List[String] =
    readings.filter(reading => reading.path.ids.startsWith(peer.path.ids)).flatMap { reading =>
      reading.events.find(event => event.occurrenceId == loss.occurrenceId &&
        event.originalActor == loss.originalActor && event.originalVictim == loss.originalVictim).flatMap { sameLoss =>
        Option.when(recoveryEvents(reading, sameLoss).exists(_.plyOffset >= peer.path.moves.size))(
          reading.path.line.ref.id)
      }
    }.distinct.sorted

  private def followupWitnesses(readings: List[Reading], peer: Reading, loss: Event, recovery: Event): List[String] =
    readings.filter(reading => reading.path.ids.startsWith(peer.path.ids)).flatMap { reading =>
      reading.events.find(event => event.occurrenceId == loss.occurrenceId &&
        event.originalActor == loss.originalActor && event.originalVictim == loss.originalVictim).flatMap { sameLoss =>
        val outcomes = recoveryEvents(reading, sameLoss)
        // The expected physical victim can be captured after the original ledger
        // interval has closed. Observe that event without relabelling it as repayment.
        val expectedObserved = reading.events.exists(event => event.plyOffset > sameLoss.plyOffset &&
          event.originalVictim.nonEmpty && event.originalVictim == recovery.originalVictim)
        // A small intermediate gain (e.g. ...Nxh3+ before ...Nxg5) cannot close
        // the still-negative interval. Alternative gains remain different facts.
        val alternativeObserved = outcomes.nonEmpty && sameLoss.originalVictim.exists { victim =>
          reading.path.certified.flatMap(_._2.material).exists { ledger =>
            ledger.observedNetCpForMover * (if victim.side == ledger.sideToMove then 1 else -1) >= 0
          }
        }
        Option.when(expectedObserved || alternativeObserved)(reading.path.line.ref.id)
      }
    }.distinct.sorted

  private def recoveryPremises(donor: Reading, peer: Reading, prefix: List[String]): List[(Event, Event)] =
    val count = donor.path.ids.zip(peer.path.ids).takeWhile((left, right) => left == right).size
    if count < 1 || peer.path.moves != prefix || count >= donor.path.moves.size || count >= peer.path.moves.size then Nil
    else donor.events.filter(loss => loss.originalVictim.nonEmpty && loss.plyOffset < count).flatMap { loss =>
      for
        removal <- recoveryEvents(donor, loss)
        if removal.plyOffset > loss.plyOffset + 1 && removal.plyOffset >= count &&
          donor.choicePoints.exists(point => point.triggerIds.contains(loss.occurrenceId) &&
            point.triggerIds.contains(removal.occurrenceId))
        peerLoss <- peer.events.find(value => value.occurrenceId == loss.occurrenceId &&
          value.originalActor == loss.originalActor && value.originalVictim == loss.originalVictim).toList
        if Set("present", "captured")(peerLoss.actorEnd.state)
      yield loss -> removal
    }

  private case class Identified(event: ObservedLineMaterialEvent, id: String, occurrenceId: String,
      actor: Option[RelationColoredPieceWitness], victim: Option[RelationColoredPieceWitness])

  private def read(packet: EvidenceBackedJudgmentPacket, path: PreparedPath, prepared: PreparedPaths): Option[Reading] =
    for
      (_, fact) <- path.certified
      ledger <- fact.material
      exposures = readExposures(packet, path, prepared)
      if (ledger.events.nonEmpty || path.moves.size > 1 || exposures.nonEmpty) &&
        path.ids == path.declared && path.positions.forall(_.id.nonEmpty)
    yield
      val positions = path.positions.toVector
      val occurrenceIds = path.ids.toVector
      val endCount = path.moves.size
      val endId = positions(endCount).id.get
      val views = prepared.resourceViews(packet)
      val originals = prepared.identitiesAfter(packet, path, 0).map(_._1)
      def identityAt(current: RelationColoredPieceWitness, count: Int): Option[RelationColoredPieceWitness] =
        prepared.identitiesAfter(packet, path, count).collectFirst {
          case (original, Some(Some(value))) if value == current => original
        }
      val identified = ledger.events.map { event =>
        val move = event.movement
        val actor = identityAt(RelationColoredPieceWitness(move.from, move.beforeRole, move.side), event.plyOffset)
        val victim = event.capture.flatMap(capture => identityAt(
          RelationColoredPieceWitness(capture.square, capture.capturedRole, !move.side), event.plyOffset))
        val occurrenceId = occurrenceIds(event.plyOffset)
        Identified(event, CommentarySelection.stableId("continuation-event", List(path.line.ref.id, occurrenceId)),
          occurrenceId, actor, victim)
      }
      val removals = identified.flatMap(value => value.victim.map(_ -> value)).toMap
      val events = identified.map { value =>
        val state = value.actor.flatMap(prepared.pieceAfter(packet, path, _, endCount))
        val end = state match
          case None => ActorEnd("unknown", endId, None, None)
          case Some(None) => ActorEnd("captured", endId, None, value.actor.flatMap(removals.get).map(_.id))
          case Some(Some(piece)) => ActorEnd("present", endId, Some(piece), None)
        Event(value.id, value.event.plyOffset, value.occurrenceId, value.event.legalMoveSemanticId,
          value.actor, value.victim, end)
      }
      val capturesByActor = identified.filter(_.event.capture.nonEmpty).flatMap(value => value.actor.map(_ -> value))
        .groupMap(_._1)(_._2)
      val links = identified.flatMap { later =>
        later.victim.toList.flatMap(capturesByActor.getOrElse(_, Nil)).filter(_.event.plyOffset < later.event.plyOffset)
          .map(earlier => CaptureLink(earlier.id, later.id, later.event.plyOffset - earlier.event.plyOffset))
      }
      val rootActor = views.get(positions.head.id.get).flatMap(_.legalRows)
        .flatMap(_.find(_.move == path.moves.head)).map(_.actor)
      val seeds = rootActor.toSet ++ identified.find(_.event.plyOffset == 0).flatMap(_.victim).toSet ++
        prepared.attackedRootTargets(packet).map(_._1)
      // Only scheduling uses this identity closure. Every material event remains in the reading.
      val neighbors = identified.flatMap(value => for actor <- value.actor; victim <- value.victim yield
        List(actor -> victim, victim -> actor)).flatten.groupMap(_._1)(_._2)
      val related = scala.collection.mutable.Set.from(seeds)
      val pending = scala.collection.mutable.Queue.from(seeds)
      def connect(pieces: Iterable[RelationColoredPieceWitness]): Unit =
        pieces.foreach(piece => if related.add(piece) then pending.enqueue(piece))
        while pending.nonEmpty do
          neighbors.getOrElse(pending.dequeue(), Nil).foreach { piece =>
            if related.add(piece) then pending.enqueue(piece)
          }
      connect(Nil)
      def isRelated(value: Identified): Boolean = value.actor.exists(related) || value.victim.exists(related)
      val byId = identified.map(value => value.id -> value).toMap

      // One loss anchor per negative ledger interval, not every old loss times
      // every later capture. Zero closes this bookkeeping interval only.
      val balances = ledger.runningBalancesForMover.toVector
      val openLoss = scala.collection.mutable.Map.empty[chess.Color, Identified]
      val otherGains = scala.collection.mutable.ListBuffer.empty[OtherTargetGain]
      identified.zipWithIndex.foreach { (value, index) =>
        val before = if index == 0 then 0 else balances(index - 1)
        val after = balances(index)
        List(chess.Color.White, chess.Color.Black).foreach { side =>
          val sign = if side == ledger.sideToMove then 1 else -1
          if before * sign >= 0 then openLoss.remove(side)
          if value.event.capture.nonEmpty && value.event.movement.side == side && before * sign < 0 then
            openLoss.get(side).filter(loss => value.victim.nonEmpty && value.victim != loss.actor).foreach { loss =>
              otherGains += OtherTargetGain(loss.id, value.id)
              // Follow the piece that made this recovery if it is subsequently
              // lost. A briefly balanced ledger must not hide that new loss.
              connect(value.actor.toList ++ value.victim.toList)
            }
          if value.event.capture.nonEmpty && value.event.movement.side != side && after * sign < 0 &&
              value.actor.nonEmpty && value.victim.nonEmpty && !openLoss.contains(side) then
            openLoss.update(side, value)
          if after * sign >= 0 then openLoss.remove(side)
        }
      }

      def compare(kind: String, beforeActor: RelationColoredPieceWitness, beforeCount: Int,
          afterActor: RelationColoredPieceWitness, afterCount: Int, triggers: List[String]): SupportComparison =
        val before = prepared.supportPoint(packet, path, beforeCount)
        val after = prepared.supportPoint(packet, path, afterCount)
        val targets = originals.filter(piece => piece.side == beforeActor.side && piece.role.name != "king" &&
          piece != beforeActor && piece != afterActor).map { target =>
            SupportTarget(target, before.support(beforeActor, target), after.support(afterActor, target))
          }.filter(target => target.before.state == "present" || target.after.state == "present")
        SupportComparison(CommentarySelection.stableId("continuation-support", List(path.line.ref.id, kind,
          beforeActor.square.key, afterActor.square.key, beforeCount.toString, afterCount.toString)),
          kind, beforeActor, afterActor, positions(beforeCount).id.get, positions(afterCount).id.get, triggers.distinct,
          targets, before.complete && after.complete && targets.forall(target =>
            target.before.state != "unknown" && target.after.state != "unknown"))
      val actorContinuations = rootActor.toList.map(actor =>
        compare("actor_continuation", actor, 0, actor, endCount, List(path.ids.head))) ++
        identified.flatMap(value => value.actor.map(actor => compare("actor_continuation", actor,
          value.event.plyOffset + 1, actor, endCount, List(value.occurrenceId))))
      val replacements = links.flatMap { link =>
        val loss = byId(link.lossEventId)
        val recovery = byId(link.captureEventId)
        for
          victim <- loss.victim
          replacement <- recovery.actor
          if victim.side == replacement.side
        yield compare("replacement_after_capture", victim, loss.event.plyOffset, replacement,
          recovery.event.plyOffset + 1, List(loss.occurrenceId, recovery.occurrenceId))
      }
      val support = (actorContinuations ++ replacements).distinctBy(_.id)

      // A recovery's performer can itself be lost. Carry only those physical
      // dependencies, not every old loss into every later material question.
      val recoveryOwners = links.map(link => link.captureEventId -> link.lossEventId) ++
        otherGains.toList.map(gain => gain.gainEventId -> gain.lossEventId)
      val recoveryLosses = recoveryOwners.flatMap { (recoveryId, lossId) =>
        events.find(_.id == recoveryId).flatMap(_.actorEnd.removedByEventId).map(later =>
          RecoveryLoss(lossId, recoveryId, later))
      }.distinct
      val lossParents = recoveryLosses.groupMap(_.laterLossEventId)(_.lossEventId)
      val originsById = events.foldLeft(Map.empty[String, List[String]]) { (known, event) =>
        val origins = lossParents.getOrElse(event.id, Nil).flatMap(parent => known.getOrElse(parent, Nil))
        known.updated(event.id, (origins :+ event.occurrenceId).distinct)
      }
      val lossOrigins = events.map(event => event.occurrenceId -> originsById(event.id)).toMap

      val proposed = scala.collection.mutable.ListBuffer.empty[ChoicePoint]
      def propose(count: Int, triggers: List[String], kind: String): Unit =
        if count > 0 && count <= endCount && views.get(positions(count).id.get).flatMap(_.legalRows).exists(_.nonEmpty) then
          proposed += ChoicePoint(path.moves.take(count), positions(count).id.get,
            List(ChoicePremise(kind, triggers.flatMap(id => lossOrigins.getOrElse(id, List(id))).distinct)))
      identified.find(value => value.event.plyOffset == 0 && value.event.capture.nonEmpty).foreach { rootCapture =>
        val immediatelyRemoved = links.exists(link => link.lossEventId == rootCapture.id && link.elapsedPlies == 1)
        val onlyRootProjection = endCount == 1 && prepared.investigationPaths.exists(other =>
          other.moves.headOption == path.moves.headOption && other.moves.size > 1 &&
            other.certified.nonEmpty && other.ids == other.declared)
        if !immediatelyRemoved && !onlyRootProjection then
          propose(1, List(rootCapture.occurrenceId), "root_capture_response")
      }
      val ancestors = links.groupMap(_.captureEventId)(_.lossEventId)
      links.filter(link => link.elapsedPlies > 1 && isRelated(byId(link.lossEventId))).foreach { link =>
        val loss = byId(link.lossEventId)
        val recovery = byId(link.captureEventId)
        val premiseIds = scala.collection.mutable.Set(loss.id)
        val earlier = scala.collection.mutable.Queue(loss.id)
        while earlier.nonEmpty do
          ancestors.getOrElse(earlier.dequeue(), Nil).foreach { id => if premiseIds.add(id) then earlier.enqueue(id) }
        val triggers = (premiseIds.toList.map(byId).sortBy(_.event.plyOffset).map(_.occurrenceId) :+
          recovery.occurrenceId).distinct
        premiseIds.toList.map(byId).sortBy(_.event.plyOffset).foreach { premise =>
          propose(premise.event.plyOffset + 1, triggers, "delayed_capturer_removal")
        }
        // The threatened capturer's side chooses its preceding move, rather
        // than only asking about the opponent's already observed capture.
        propose(recovery.event.plyOffset - 1, triggers, "delayed_capturer_removal")
      }
      otherGains.foreach { gain =>
        val loss = byId(gain.lossEventId)
        val recovery = byId(gain.gainEventId)
        val triggers = List(loss.occurrenceId, recovery.occurrenceId)
        propose(loss.event.plyOffset + 1, triggers, "other_target_gain")
        propose(recovery.event.plyOffset - 1, triggers, "other_target_gain")
      }
      val choices = proposed.toList.groupBy(_.prefixMovesUci).values.map { values =>
        values.head.copy(premises = values.flatMap(_.premises).distinct)
      }.toList.sortBy(point => (point.prefixMovesUci.size, point.prefixMovesUci.mkString(" ")))
      val complete = rootActor.nonEmpty && identified.forall(value => value.actor.nonEmpty &&
        (value.event.capture.isEmpty || value.victim.nonEmpty)) && events.forall(_.actorEnd.state != "unknown") &&
        support.forall(_.complete)
      new Reading(packet, path, events, links, otherGains.toList, support, choices,
        openLoss.valuesIterator.map(_.id).toSet, recoveryLosses, lossOrigins, complete, exposures, prepared)

  case class DirectSupportRecapture(comparison: SupportComparison,
      originalTarget: RelationColoredPieceWitness, originalCapturer: RelationColoredPieceWitness,
      supportCount: Int, supportPositionId: String,
      provider: RelationColoredPieceWitness, target: RelationColoredPieceWitness, capturer: RelationColoredPieceWitness,
      support: Support, targetCapture: Event, recapture: Event):
    def json(rootPositionId: String): JsObject = Json.obj("kind" -> "direct_support_recapture",
      "comparison_id" -> comparison.id,
      "requirement_id" -> supportRequirementId("direct_recapture", comparison.beforeActor, originalTarget,
        (originalCapturer, capturer, target)),
      "original_provider" -> pieceJson(comparison.afterActor, rootPositionId),
      "original_target" -> pieceJson(originalTarget, rootPositionId),
      "original_capturer" -> pieceJson(originalCapturer, rootPositionId),
      "ply_count" -> supportCount, "position_id" -> supportPositionId,
      "provider" -> pieceJson(provider, supportPositionId), "target" -> pieceJson(target, supportPositionId),
      "capturer" -> pieceJson(capturer, supportPositionId),
      "support" -> support.json, "loss_event_id" -> targetCapture.id, "recapture_event_id" -> recapture.id,
      "loss_occurrence_id" -> targetCapture.occurrenceId, "recapture_occurrence_id" -> recapture.occurrenceId,
      "elapsed_plies" -> (recapture.plyOffset - targetCapture.plyOffset))

  /** The physical use is independent of why discovery selected this provider. */
  private[runtime] case class DirectSupportUse(originalTarget: RelationColoredPieceWitness,
      originalCapturer: RelationColoredPieceWitness, supportCount: Int, supportPositionId: String,
      provider: RelationColoredPieceWitness, target: RelationColoredPieceWitness, capturer: RelationColoredPieceWitness,
      support: Support, targetCapture: Event, recapture: Event)

  private[runtime] case class DirectSupportUseObservation(uses: List[DirectSupportUse],
      complete: Boolean, reason: Option[String] = None)

  private def directSupportUses(packet: EvidenceBackedJudgmentPacket, reading: Reading,
      prepared: PreparedPaths, originalProvider: RelationColoredPieceWitness,
      originalTarget: RelationColoredPieceWitness, fromCount: Int, throughCount: Int): List[DirectSupportUse] =
    val path = reading.path
    reading.capturesByVictim.getOrElse(originalTarget, Nil).filter(event =>
      event.plyOffset >= fromCount && event.plyOffset < throughCount).flatMap { loss =>
      reading.linksByLoss.getOrElse(loss.id, Nil).flatMap { link =>
        (for
          recovery <- reading.materialEventById.get(link.captureEventId)
          if recovery.plyOffset > loss.plyOffset && recovery.plyOffset < throughCount &&
            recovery.originalActor.contains(originalProvider)
          capturer <- loss.originalActor
          if recovery.originalVictim.contains(capturer)
          provider <- prepared.pieceAfter(packet, path, originalProvider, loss.plyOffset).flatten
          target <- prepared.pieceAfter(packet, path, originalTarget, loss.plyOffset).flatten
          attacker <- prepared.pieceAfter(packet, path, capturer, loss.plyOffset).flatten
          support = prepared.supportAt(packet, path, loss.plyOffset, originalProvider, originalTarget)
          if support.state == "present"
          afterCapture <- prepared.pieceAfter(packet, path, capturer, loss.plyOffset + 1).flatten
          // En passant is a physical capture link, but does not use support on the target's square.
          if afterCapture.square == target.square
          if (loss.plyOffset to recovery.plyOffset).forall(count =>
            prepared.pieceAfter(packet, path, originalProvider, count).flatten.contains(provider))
          if (loss.plyOffset + 1 to recovery.plyOffset).forall(count =>
            prepared.pieceAfter(packet, path, capturer, count).flatten.contains(afterCapture))
          positionId <- path.positions(loss.plyOffset).id
        yield DirectSupportUse(originalTarget, capturer, loss.plyOffset, positionId,
          provider, target, attacker, support, loss, recovery)).toList
      }
    }

  /** An exhaustive bounded query, including providers that discovery never selected.
    * A positive use needs its own witnesses; a negative additionally needs the
    * complete material, identity and support inventories throughout this prefix.
    */
  private[runtime] def directSupportUse(packet: EvidenceBackedJudgmentPacket, path: PreparedPath,
      prepared: PreparedPaths, throughCount: Int, originalProvider: RelationColoredPieceWitness,
      originalTarget: RelationColoredPieceWitness): DirectSupportUseObservation =
    def unavailable(reason: String) = DirectSupportUseObservation(Nil, false, Some(reason))
    if !prepared.belongsTo(packet) then unavailable("packet_owner_mismatch")
    else if !prepared.path(packet, path.line.ref.id).exists(_ eq path) then unavailable("path_owner_mismatch")
    else if throughCount < 0 || throughCount > path.moves.size then unavailable("point_outside_observed_path")
    else if path.ids != path.declared || path.certified.isEmpty then unavailable("uncertified_material_path")
    else if path.certified.flatMap(_._2.material).isEmpty then unavailable("missing_material_ledger")
    else
      val reading = prepared.continuationReading(packet, path) { read(packet, path, prepared) }
      val uses = reading.toList.flatMap(directSupportUses(packet, _, prepared,
        originalProvider, originalTarget, 0, throughCount))
      if uses.nonEmpty then DirectSupportUseObservation(uses, true)
      else if !(0 to throughCount).forall(count =>
          prepared.pieceAfter(packet, path, originalProvider, count).nonEmpty &&
            prepared.pieceAfter(packet, path, originalTarget, count).nonEmpty &&
            prepared.identitiesAfter(packet, path, count).forall(_._2.nonEmpty)) then
        unavailable("incomplete_piece_identities")
      else if !(0 to throughCount).forall(count => prepared.supportPoint(packet, path, count).complete) then
        unavailable("incomplete_support_inventory")
      else if path.certified.flatMap(_._2.material).exists(_.events.exists(_.plyOffset < throughCount)) && reading.isEmpty then
        unavailable("missing_material_event_reading")
      else DirectSupportUseObservation(Nil, true)

  /** The supporting replacement actually recaptures on its friendly target's
    * square. Both pieces stay in place between the loss and that recapture.
    * This is a fact about this supplied line, not a unique or forced defense.
    */
  def readDirectSupportRecaptures(packet: EvidenceBackedJudgmentPacket, reading: Reading,
      prepared: PreparedPaths): List[DirectSupportRecapture] =
    prepared.requireOwner(packet)
    val path = reading.path
    if !reading.belongsTo(packet) || !prepared.path(packet, path.line.ref.id).exists(_ eq path) ||
        path.certified.isEmpty || path.ids != path.declared then Nil
    else
      reading.supportComparisons.filter(_.kind == "replacement_after_capture").flatMap { comparison =>
        val base = path.positions.indexWhere(_.id.contains(comparison.afterPositionId))
        comparison.targets.flatMap { supported =>
          if base < 0 then Nil
          else directSupportUses(packet, reading, prepared, comparison.afterActor,
            supported.originalTarget, base, path.moves.size).map { use =>
              DirectSupportRecapture(comparison, use.originalTarget, use.originalCapturer, use.supportCount,
                use.supportPositionId, use.provider, use.target, use.capturer, use.support,
                use.targetCapture, use.recapture)
            }
        }
      }.distinctBy(value => (value.comparison.id, value.targetCapture.id, value.recapture.id))

  case class CaptureExposure(prefixMoves: List[String], occurrenceId: String,
      beforePositionId: String, positionId: String,
      originalAttacker: RelationColoredPieceWitness, attacker: RelationColoredPieceWitness,
      originalTarget: RelationColoredPieceWitness, target: RelationColoredPieceWitness,
      blockers: List[(RelationColoredPieceWitness, RelationColoredPieceWitness)],
      raySourceId: String, controlSourceIds: List[String], captures: List[LegalMoveResource]):
    def json(rootPositionId: String): JsObject = Json.obj("kind" -> "capture_exposure",
      "prefix_moves_uci" -> prefixMoves, "occurrence_id" -> occurrenceId,
      "before_position_id" -> beforePositionId, "position_id" -> positionId,
      "original_attacker" -> pieceJson(originalAttacker, rootPositionId),
      "attacker" -> pieceJson(attacker, positionId),
      "original_target" -> pieceJson(originalTarget, rootPositionId),
      "target" -> pieceJson(target, positionId),
      "vacated_blockers" -> blockers.map { (original, before) =>
        Json.obj("original" -> pieceJson(original, rootPositionId), "before" -> pieceJson(before, beforePositionId))
      },
      "ray_source_id" -> raySourceId, "control_source_ids" -> controlSourceIds,
      "moves_uci" -> captures.map(_.move), "source_ids" -> captures.map(_.sourceId),
      "legal_inventory_complete" -> true)

  /** A move has removed the observed blockers between a stationary opponent
    * slider and another original friendly piece, and the new capture is legal.
    * This reads an available response even when a supplied line declines it.
    */
  def readExposures(packet: EvidenceBackedJudgmentPacket, path: PreparedPath,
      prepared: PreparedPaths): List[CaptureExposure] =
    prepared.requireOwner(packet)
    if !prepared.path(packet, path.line.ref.id).exists(_ eq path) ||
        path.certified.isEmpty || path.ids != path.declared then Nil
    else
      val views = prepared.resourceViews(packet)
      (1 to path.moves.size).toList.flatMap { count =>
        val beforePosition = path.positions(count - 1)
        val afterPosition = path.positions(count)
        val identities = prepared.identitiesAfter(packet, path, count - 1)
        def original(piece: RelationColoredPieceWitness) = identities.collectFirst {
          case (value, Some(Some(current))) if current == piece => value
        }
        (for
          before <- beforePosition.id.flatMap(views.get)
          after <- afterPosition.id.flatMap(views.get)
          moverSide <- beforePosition.sideToMove
          legal <- after.legalRows
        yield before.records.flatMap { record => record.payload match
          case fact: RelationFactEvidence => fact.detail match
            case RelationWitnessDetail.RayBarrier(side, square, role, occupants, _)
                if side != moverSide && afterPosition.sideToMove.contains(side) =>
              val actor = RelationColoredPieceWitness(square, role, side)
              occupants.zipWithIndex.flatMap { (target, index) =>
                val blockers = occupants.take(index)
                (for
                  originalActor <- original(actor)
                  originalVictim <- original(target)
                  if target.side == moverSide && target.role.name != "king" && blockers.nonEmpty
                  if prepared.pieceAfter(packet, path, originalActor, count).flatten.contains(actor) &&
                    prepared.pieceAfter(packet, path, originalVictim, count).flatten.contains(target)
                  if blockers.forall(blocker => after.occupantAt(blocker.square).isEmpty)
                  mapped = blockers.flatMap(blocker => original(blocker).map(_ -> blocker))
                  if mapped.size == blockers.size
                  sources = after.controlSources(actor, target.square)
                  if sources.nonEmpty
                  captures = legal.filter(row => row.actor == actor && row.capture.contains(target))
                  if captures.nonEmpty
                yield CaptureExposure(path.moves.take(count), path.ids(count - 1), beforePosition.id.get,
                  afterPosition.id.get, originalActor, actor, originalVictim, target, mapped,
                  fact.semanticId, sources, captures)).toList
              }
            case _ => Nil
          case _ => Nil
        }).toList.flatten
      }.distinctBy(value => (value.occurrenceId, value.originalAttacker, value.originalTarget))

  /** Physical identity and completeness at one point; the resource owner indexes controls. */
  private[runtime] final class SupportPoint(view: Option[PositionFacts],
      identities: Map[RelationColoredPieceWitness, Option[Option[RelationColoredPieceWitness]]]):
    val complete: Boolean = view.exists(_.completeKinds(RelationFactKind.GeometricControl)) && identities.values.forall(_.nonEmpty)
    def support(actor: RelationColoredPieceWitness, target: RelationColoredPieceWitness): Support =
      (identities.get(actor).flatten, identities.get(target).flatten) match
        case (None, _) | (_, None) => Support("unknown", Nil)
        case (Some(None), _) => Support("actor_removed", Nil)
        case (_, Some(None)) => Support("target_removed", Nil)
        case (Some(Some(currentActor)), Some(Some(currentTarget))) =>
          val sources = view.toList.flatMap(_.controlSources(currentActor, currentTarget.square))
          if sources.nonEmpty then Support("present", sources)
          else Support(if complete then "absent" else "unknown", Nil)

  private def pieceJson(value: RelationColoredPieceWitness, positionId: String): JsObject = Json.obj(
    "position_id" -> positionId, "side" -> (if value.side.white then "white" else "black"),
    "role" -> value.role.name, "square" -> value.square.key)
