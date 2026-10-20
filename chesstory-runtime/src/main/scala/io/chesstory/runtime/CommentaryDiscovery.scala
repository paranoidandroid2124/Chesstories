package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.{LegalMoveResource, OccurrenceResources, PositionFacts}
import lila.chessjudgment.model.judgment.*

/** Finds concrete contrasts over certified boards. Evaluation and work allocation retain their owners. */
private[runtime] object CommentaryDiscovery:
  import CommentaryFact.*
  import CommentaryProposal.*
  import ObservedLineProtocol.{PreparedPath, PreparedPaths}

  val GeneratorVersion = "36"

  final case class Anchor(prefixMoves: List[String], referenceMove: String)
  final case class SourcePath(moves: List[String], source: Source,
      choiceRank: Option[(List[String], Int)] = None)
  final case class Scan(anchor: Anchor, legalMoves: List[String], examinedMoves: List[String],
      state: String, reason: Option[String] = None, deferredRequestIds: List[String] = Nil,
      pendingReplayPrefixes: List[List[String]] = Nil)
  final case class State(inputKey: String = "", scans: List[Scan] = Nil, inventoryKey: String = "")
  final case class Batch(additions: List[Entry], replayNeeds: List[List[String]], scans: List[Scan],
      functionalReplayNeeds: List[List[String]] = Nil)

  /** The source owns this decision, not the later witness's forecast or value. */
  private[runtime] def sameReserveDecision(prepared: PreparedPaths, source: PreparedPath,
      witness: PreparedPath, count: Int, actors: List[RelationColoredPieceWitness]): Boolean =
    def owned(path: PreparedPath) = prepared.paths.exists(_ eq path) && path.certified.nonEmpty &&
      path.ids == path.declared && path.moves == path.sourceMoves
    count > 0 && actors.nonEmpty && List(source, witness).forall(path => owned(path) &&
      path.moves.size >= count && path.ids.size >= count && path.positions.size > count) &&
      source.moves.take(count) == witness.moves.take(count) &&
      source.ids.take(count) == witness.ids.take(count) &&
      source.positions.take(count + 1) == witness.positions.take(count + 1) &&
      actors.distinct.forall { actor =>
        val current = prepared.trackedPieceAfter(source, actor, count).flatten
        current.nonEmpty && current == prepared.trackedPieceAfter(witness, actor, count).flatten
      }

  private case class Board(moves: List[String], count: Int, view: PositionFacts,
      originals: Map[RelationColoredPieceWitness, Piece], sideToMove: Option[chess.Color],
      path: Option[PreparedPath], probe: Option[OccurrenceResources.ProbePosition], occurrenceId: Option[String]):
    def key: (String, Int, String) = (moves.take(count).mkString(" "), count, occurrenceId.getOrElse("probe"))
    def original(current: RelationColoredPieceWitness): Option[Piece] = originals.get(current)

  private case class Action(actor: Piece, to: String, capture: Option[Piece], promotion: Option[String],
      source: Source, sourceRefs: List[String], observedAt: Option[Int] = None, ordinal: Int = 1):
    def predicate: Predicate = capture.filterNot(_.role == "king").fold(
      Predicate("legal_move", Some(actor), square = Some(to), property = promotion))(
      victim => Predicate("legal_capture", Some(actor), Some(victim)))
    def event(from: Int, phase: String): Event = Event(if capture.nonEmpty then "capture" else "move",
      ordinal, phase, from, observedAt.get + 1, Some(actor), capture,
      square = Option.when(capture.isEmpty)(to))

  private case class Focus(kind: String, predicate: Predicate, sourceRefs: List[String],
      action: Option[Action] = None, usageTarget: Option[Piece] = None)

  private def piece(value: RelationColoredPieceWitness): Piece =
    Piece(if value.side.white then "white" else "black", value.role.name, value.square.key)

  /** Empty-square changes need a specific action destination; contacts need an actual physical target.
    * Derived one/two-ply probes are not fed back as new plans or new scanning anchors.
    */
  def discover(packet: EvidenceBackedJudgmentPacket, prepared: PreparedPaths, anchors: List[Anchor],
      sources: List[SourcePath], existing: List[Entry], probes: List[OccurrenceResources.ProbePath] = Nil,
      pvGuided: Boolean = false): Batch =
    prepared.requireOwner(packet)
    val notation = prepared.notation
    // Supplied PGNs may be longer than the finite question language. Bound the
    // observation frontier before discovering events or constructing requirements;
    // the original supplied line and its evidence remain intact in the packet.
    val boundedSources = sources.map(source => source.copy(moves = source.moves.take(MaximumPly)))
    val views = prepared.resourceViews(packet)
    val paths = prepared.paths.filter(path => path.certified.nonEmpty && path.ids == path.declared)
    val boards = scala.collection.mutable.Map.empty[List[String], Option[Board]]
    def board(prefix: List[String]): Option[Board] = boards.getOrElseUpdate(prefix,
      paths.filter(_.moves.startsWith(prefix)).sortBy(path => (path.moves.size, path.line.ref.id)).headOption.flatMap { path =>
        for
          position <- path.positions.lift(prefix.size)
          view <- position.id.flatMap(views.get).filter(_.position == position)
        yield Board(path.moves, prefix.size, view, prepared.identitiesAfter(packet, path, prefix.size).flatMap {
          case (original, Some(Some(current))) => Some(current -> piece(original))
          case _ => None
        }.toMap, position.sideToMove, Some(path), None, Some(view.occurrence.id))
      }.orElse(probes.filter(_.moves.startsWith(prefix)).sortBy(_.moves.size).headOption.flatMap { path =>
        path.at(prefix.size).map(at => Board(path.moves, prefix.size, at.facts,
          at.identities.flatMap {
            case (original, Some(Some(current))) => Some(current -> piece(original))
            case _ => None
          }.toMap, Some(at.sideToMove), None, Some(at), None))
      }))
    val values = scala.collection.mutable.Map.empty[((String, Int, String), Predicate), CommentaryFactAtoms.Value]
    def value(at: Board, predicate: Predicate): CommentaryFactAtoms.Value =
      values.getOrElseUpdate(at.key -> predicate, at.path.fold(
        CommentaryFactAtoms.readProbe(at.probe.get, predicate))(
        path => CommentaryFactAtoms.readForDiscovery(packet, prepared, path, at.count, predicate)))
    val controlCache = scala.collection.mutable.Map.empty[(String, Int, String), List[(Piece, EvidenceSquare, String)]]
    def controls(at: Board) = controlCache.getOrElseUpdate(at.key,
      at.view.completeFacts(RelationFactKind.GeometricControl).toList.flatten.flatMap { fact =>
      fact.detail match
        case RelationWitnessDetail.GeometricControl(side, from, role, to) =>
          at.original(RelationColoredPieceWitness(from, role, side)).map(actor => (actor, to, fact.semanticId))
        case _ => None
    })
    val canonical = Source("generator", "canonical-board", Some(GeneratorVersion))
    // Reuse witnessed applications on the supplied/returned paths only. These
    // observations add context to existing pairs, never candidates or replays.
    lazy val kingSupportConditions =
      val readings = prepared.continuationReadings.map(reading => reading.path.line.ref.id -> reading).toMap
      boundedSources.flatMap { source =>
        board(source.moves).flatMap(_.path).flatMap(path => readings.get(path.line.ref.id)).toList.flatMap { reading =>
          for
            comparison <- reading.supportComparisons if comparison.kind == "replacement_after_capture"
            target <- comparison.targets.map(_.originalTarget)
            point <- reading.kingCaptureOpportunities(comparison, target, source.moves.size)
            if point.support.state == "present" && !point.available
          yield (reading.path.moves.take(point.count),
            Predicate("legal_capture", Some(piece(point.originalAttacker)), Some(piece(target))),
            Predicate("support", Some(piece(comparison.afterActor)), Some(piece(target))))
        }
      }.distinct.sortBy((moves, capture, support) => (moves.mkString(" "), capture.json.toString, support.json.toString))
    def action(at: Board, row: LegalMoveResource, source: Source, observedAt: Option[Int] = None): Option[Action] =
      at.original(row.actor).flatMap { actor =>
        val victim = row.capture.flatMap(at.original)
        Option.when(row.capture.isEmpty || victim.nonEmpty)(
          Action(actor, row.to.key, victim, row.promotionRole, source,
            at.occurrenceId.toList :+ row.sourceId, observedAt))
      }
    val additions = scala.collection.mutable.LinkedHashMap.empty[String, Entry]
    val replays = scala.collection.mutable.LinkedHashSet.empty[List[String]]
    val functionalReplays = scala.collection.mutable.LinkedHashSet.empty[List[String]]
    val priorEntries = existing.map(entry => entry.request.id -> entry).toMap
    val sourcePrefixes = boundedSources.flatMap(_.moves.inits).toSet
    val investigativeRootMoves = Set(packet.assembly.input.playedMoveUci) ++
      anchors.filter(_.prefixMoves.isEmpty).map(_.referenceMove)
    // Leave one requirement for the concrete contradiction that can trigger the single
    // child recheck. Speculative discovery must not fill every slot before answers arrive.
    val verificationRoom = if existing.exists(_.request.origin.question.producer == "C-comparison-conflict") then 0 else 1
    var remaining = (MaximumRequirements - verificationRoom - existing.map(_.request.requirements.size).sum).max(0)
    case class Draft(id: String, anchor: Anchor, kind: String, refs: List[String], origin: Source,
        requirements: List[Requirement], headRequirementIds: List[List[String]], alternative: Option[String], question: Option[String],
        bundles: List[List[Requirement]])
    val drafts = scala.collection.mutable.LinkedHashMap.empty[(String, Option[String]), Draft]
    val deferred = scala.collection.mutable.Map.empty[String, scala.collection.mutable.LinkedHashSet[String]]
    val tacticalProbes = scala.collection.mutable.LinkedHashMap.empty[(List[String], String, String), Int]
    // Direct replies retain their two slots. At most two further replies test different
    // conditions, within the same job budget; these are coverage choices, not strength scores.
    val tacticalChoices = scala.collection.mutable.LinkedHashMap.empty[(List[String], String, String), (Boolean, Set[String])]
    var followupRoom = 0
    var functionalRoom = 0

    def emit(anchor: Anchor, key: List[String], kind: String, refs: List[String], origin: Source,
        requirements: List[Requirement], headRequirementIds: List[String], alternative: Option[String] = None,
        question: Option[String] = None,
        additionalHeads: List[List[String]] = Nil, bundles: List[List[Requirement]] = Nil): Unit =
      val id = CommentarySelection.stableId("discovery", List(GeneratorVersion, kind,
        anchor.prefixMoves.mkString(" "), anchor.referenceMove) ++ key)
      val identity = id -> alternative
      val previous = drafts.get(identity)
      val heads = headRequirementIds :: additionalHeads
      require(previous.forall(value => value.anchor == anchor && value.requirements == requirements &&
        value.headRequirementIds == heads && value.bundles == bundles),
        s"one contrast retains its exact time and branch conditions: $kind $key ${previous.map(_.requirements)} != $requirements")
      drafts.update(identity, Draft(id, anchor, kind, (previous.toList.flatMap(_.refs) ++ refs).distinct,
        origin.copy(sourceRefs = (previous.toList.flatMap(_.origin.sourceRefs) ++ origin.sourceRefs).distinct),
        requirements, heads, alternative, question, bundles))

    def admit(draft: Draft): Unit =
      val Draft(id, anchor, kind, refs, origin, requirements, headRequirementIds, alternative, question, bundles) = draft
      val prior = additions.get(id).orElse(priorEntries.get(id))
      if prior.forall(_.admission == "generated") then
        // A returned comparison can move the useful wait further along its
        // line. Keep enough of the existing 600-item quota for the bounded
        // chain of four investigations; unrelated child contacts cannot spend
        // this room before the question they came from has been settled.
        val linked = Set("reserve-hold", "reserve-entry")(kind) && requirements.exists {
          case pair: CompareMoves => Set("hold_source", "reserve_source")(pair.id)
          case fact: Fact if Set("hold_start", "reserve_start")(fact.id) => fact.path.source match
            case Declared(moves) => boundedSources.exists(source =>
              source.source.producer == "response_comparison" && source.moves.startsWith(moves))
            case _ => false
          case _ => false
        }
        val functional = kind == "reply-function" && anchor.prefixMoves.isEmpty &&
          requirements.exists(_.id == "function_task_context") && alternative.exists(move =>
            Set(anchor.referenceMove, move) == investigativeRootMoves.toSet)
        val available = (remaining - (if linked then functionalRoom else followupRoom.max(functionalRoom))).max(0)
        // The pair can survive a tight budget with one complete observation. Further
        // observations keep their own deferred IDs rather than evicting the whole choice.
        val priorIds = prior.toList.flatMap(_.request.requirements.map(_.id)).toSet
        var room = available - requirements.count(value => !priorIds(value.id))
        val accepted = bundles.flatMap { bundle =>
          val size = bundle.count(value => !priorIds(value.id))
          if size <= room then
            room -= size
            bundle
          else
            deferred.getOrElseUpdate(anchor.prefixMoves.mkString(" "), scala.collection.mutable.LinkedHashSet.empty) +=
              CommentarySelection.stableId("discovery-observation", List(id, bundle.head.id))
            Nil
        }
        if bundles.nonEmpty && accepted.isEmpty && prior.isEmpty && kind != "opponent-choice" then return
        // One issue owns its baseline once. Branches share it instead of copying four facts per pair.
        def branchId(value: String): String = alternative.fold(value)(move =>
          if value.startsWith("alternative") || value.startsWith("contrast") then value + "_" + move else value)
        val mapped = (requirements ++ accepted).map {
          case fact: Fact => fact.copy(id = branchId(fact.id), path = fact.path.copy(binding = branchId(fact.path.binding)))
          case compare: FactComparison => compare.copy(id = branchId(compare.id),
            leftRequirementId = branchId(compare.leftRequirementId), rightRequirementId = branchId(compare.rightRequirementId))
          case group: FactGroup => group.copy(id = branchId(group.id), members = group.members.map(branchId))
          case compare: CompareMoves => compare.copy(id = branchId(compare.id))
          case other => other
        }
        val previous = prior.toList.flatMap(_.request.requirements)
        val previousById = previous.map(requirement => requirement.id -> requirement).toMap
        require(mapped.forall(requirement => previousById.get(requirement.id).forall(_ == requirement)),
          s"a semantic discovery ID must retain its exact question and time conditions: $kind $id " +
            mapped.filter(requirement => previousById.get(requirement.id).exists(_ != requirement))
              .map(requirement => previousById(requirement.id) -> requirement).mkString(","))
        val added = mapped.filterNot(requirement => previousById.contains(requirement.id))
        if added.size > available then
          deferred.getOrElseUpdate(anchor.prefixMoves.mkString(" "), scala.collection.mutable.LinkedHashSet.empty) +=
            alternative.fold(id)(move => s"$id:$move")
        else
          def joined(source: Source, extra: List[String]) = source.copy(sourceRefs = (source.sourceRefs ++ extra).distinct.take(64))
          val questionSource = prior.map(_.request.origin.question).getOrElse(Source("generator", s"C-$kind", Some(GeneratorVersion)))
          val evidenceSource = prior.map(_.request.origin.evidence).getOrElse(origin)
          val request = Request(id, Origin(joined(questionSource, refs),
            Source("generator", "legal-alternatives", Some(GeneratorVersion)), Source("none", "existing-agenda"),
            joined(evidenceSource, origin.sourceRefs)), previous ++ added)
          require(validRequests(List(request)), "the discovery producer must obey the existing proposition contract")
          val readings = prior.toList.flatMap(_.readings) ++ added.map(value => Reading(value.id))
          val bundledIds = bundles.flatten.map(_.id).toSet
          val acceptedIds = accepted.map(_.id).toSet
          val acceptedHeads = headRequirementIds.filter(_.forall(id => !bundledIds(id) || acceptedIds(id)))
          val arms = acceptedHeads.zipWithIndex.foldLeft(prior.toList.flatMap(_.generatedArms)) { case (previousArms, (heads, index)) =>
            val arm = GeneratedArm(CommentarySelection.stableId("discovery-arm", id :: alternative.toList ++
              Option.when(index > 0)(heads.mkString(" "))),
              anchor.prefixMoves, anchor.referenceMove, alternative, heads.map(branchId),
              // A later search can witness the identical question. Its report
              // kind is provenance, not a different semantic arm. The exact
              // requirement body was checked above; accumulate the new refs.
              mapped.map(_.id).distinct, joined(evidenceSource, origin.sourceRefs), refs)
            require(arm.headRequirementIds.nonEmpty && arm.headRequirementIds.forall(arm.memberRequirementIds.contains),
              "a generated arm's head must belong to its exact question context")
            val mergedArm = previousArms.find(_.id == arm.id).fold(arm) { previous =>
              require(previous.prefixMoves == arm.prefixMoves && previous.referenceMove == arm.referenceMove &&
                previous.alternativeMove == arm.alternativeMove &&
                previous.source.copy(sourceRefs = Nil) == arm.source.copy(sourceRefs = Nil),
                "a generated arm must retain its exact anchor, comparison and source identity")
              previous.copy(headRequirementIds = (previous.headRequirementIds ++ arm.headRequirementIds).distinct,
                memberRequirementIds = (previous.memberRequirementIds ++ arm.memberRequirementIds).distinct,
                source = previous.source.copy(sourceRefs = (previous.source.sourceRefs ++ arm.source.sourceRefs).distinct),
                sourceRefs = (previous.sourceRefs ++ arm.sourceRefs).distinct)
            }
            if previousArms.exists(_.id == arm.id) then
              previousArms.map(previous => if previous.id == arm.id then mergedArm else previous)
            else previousArms :+ mergedArm
          }
          if prior.isEmpty || added.nonEmpty || prior.exists(entry => entry.request != request || entry.generatedArms != arms) then
            additions.update(id, Entry(request, readings, "generated", arms, question.orElse(prior.flatMap(_.questionText))))
          remaining -= added.size

    def declared(binding: String, prefix: List[String], extend: Boolean = false) =
      Path(binding, Declared(prefix), extend)

    val scans = anchors.distinct.map { anchor =>
      val start = anchor.prefixMoves
      val ply = start.size
      val inspected = scala.collection.mutable.Map.empty[(String, Int, String), Board]
      def inspect(prefix: List[String]): Option[Board] = board(prefix).map { at => inspected.update(at.key, at); at }
      val pending = scala.collection.mutable.LinkedHashSet.empty[List[String]]
      var outsidePv = false
      var deferredActions = sources.exists(source => source.moves.startsWith(start) && source.moves.size > MaximumPly)
      def requireReplay(prefix: List[String]): Unit =
        if pvGuided && !sourcePrefixes(prefix) then outsidePv = true
        else
          pending += prefix
          replays += prefix
      val before = inspect(start)
      val legal = before.flatMap(_.view.legalRows)
      if legal.isEmpty then Scan(anchor, Nil, Nil, "unavailable", Some("incomplete_legal_inventory"))
      else if legal.get.isEmpty then Scan(anchor, Nil, Nil, "not_applicable", Some("automatic_terminal"))
      else
        val rows = legal.get
        val moves = rows.map(_.move)
        val selectedMoves = moves.filter(move => !pvGuided || sourcePrefixes(start :+ move))
        outsidePv = selectedMoves.size < moves.size
        val missing = selectedMoves.filter(move => inspect(start :+ move).isEmpty)
        missing.foreach(move => requireReplay(start :+ move))
        val branches = selectedMoves.flatMap(move => inspect(start :+ move).map(move -> _)).toMap
        branches.get(anchor.referenceMove) match
          case None => Scan(anchor, moves, moves.filter(branches.contains), "partial", Some("missing_reference_path"),
            pendingReplayPrefixes = pending.toList)
          case Some(reference) =>
            val root = before.get
            val mover = root.sideToMove.get
            val side = if mover.white then "white" else "black"
            val rootActions = rows.filter(_.actor.role.name == "pawn").flatMap(action(root, _, canonical))
            // Only accepted engine/document paths provide later goals. Internal probes never do.
            val witnessedActionPaths = boundedSources.flatMap { source =>
              if !source.moves.startsWith(start) then Nil
              else
                val seen = scala.collection.mutable.Map.empty[(Piece, String, Option[Piece], Option[String]), Int]
                source.moves.indices.drop(ply).flatMap { offset =>
                inspect(source.moves.take(offset)).toList.flatMap { at =>
                  if !at.sideToMove.contains(mover) then Nil
                  else at.view.legalRows.toList.flatten.filter(_.move == source.moves(offset))
                    .flatMap(action(at, _, source.source, Some(offset))).flatMap { goal =>
                      val key = (goal.actor, goal.to, goal.capture, goal.promotion)
                      val ordinal = seen.getOrElse(key, 0) + 1
                      seen.update(key, ordinal)
                      Option.when(offset > ply)(goal.copy(ordinal = ordinal) -> source.moves.take(offset + 1))
                    }
                }
              }.toList
            }
            def joinedActions(witnesses: List[Action]): Action =
              val refs = witnesses.flatMap(goal => goal.source.sourceRefs ++ goal.sourceRefs).distinct.sorted
              witnesses.head.copy(observedAt = Some(witnesses.flatMap(_.observedAt).max), sourceRefs = refs,
                source = Source("generator", "C-observed-action-sources", Some(GeneratorVersion), refs))
            val observedActions = witnessedActionPaths.map(_._1).groupBy(goal =>
              (goal.actor, goal.to, goal.capture, goal.promotion, goal.ordinal)).toList
              .sortBy(_._1.toString).map((_, witnesses) => joinedActions(witnesses))
            val goals = (rootActions ++ observedActions).distinct
            val sourceBoards = boundedSources.filter(_.moves.startsWith(start)).flatMap(source =>
              (ply to source.moves.size).flatMap(count => inspect(source.moves.take(count)))).distinctBy(_.key)

            // Capture witnesses depend on this anchor and pair, not on the compared first move.
            val supportEvents = scala.collection.mutable.Map.empty[(Piece, Piece), List[(Predicate, Event, List[String])]]
            def supportUses(provider: Piece, target: Piece): List[(Predicate, Event, List[String])] =
              supportEvents.getOrElseUpdate(provider -> target, {
                val uses = for
                  source <- boundedSources if source.moves.startsWith(start) && source.moves.size > ply + 1
                  end <- inspect(source.moves).toList
                  path <- end.path.toList
                  use <- LineContinuationInvestigation.directSupportUse(packet, path, prepared, end.count,
                    provider.witness, target.witness).uses
                  if use.targetCapture.plyOffset >= ply
                yield (Predicate("direct_support_use", Some(provider), Some(target)), piece(use.originalCapturer),
                  use.recapture.plyOffset + 1, source.source.sourceRefs)
                uses.groupBy(row => row._1 -> row._2).toList.sortBy(_._1.toString).map {
                  case ((predicate, capturer), witnesses) =>
                    (predicate, Event("capture", 1, "after", ply, witnesses.map(_._3).max,
                      predicate.subject, Some(capturer)), witnesses.flatMap(_._4).distinct)
                }
              })

            def contested(at: Board, square: EvidenceSquare): Boolean = controls(at).exists {
              case (actor, target, _) => actor.side != side && target == square
            }
            val contactEnemies = scala.collection.mutable.Map.empty[((String, Int, String), Piece), List[Piece]]
            def enemiesContacting(at: Board, target: Piece): List[Piece] =
              contactEnemies.getOrElseUpdate(at.key -> target, at.originals.collectFirst {
                case (current, original) if original == target => current.square
              }.toList.flatMap(square => controls(at).collect {
                case (actor, `square`, _) if actor.side != side => actor
              }).distinct)
            def exposed(at: Board, target: Piece): Boolean = enemiesContacting(at, target).nonEmpty

            def attacked(at: Board, enemy: Piece): List[Piece] = controls(at).filter(_._1 == enemy)
              .flatMap { (_, square, _) => at.view.occupantAt(square).flatMap(at.original) }
              .filter(_.side == side).distinct

            // Preserve the reason for a probe as ordinary facts. A downstream reader need not
            // rediscover which physical target made this enemy entry worth comparing.
            def pressure(name: String, prefix: List[String], prior: Board, after: Board,
                enemy: Piece, target: Piece, captured: Boolean): List[Requirement] =
              val later = prefix :+ after.moves(ply + 1)
              if captured then List(
                Fact(s"${name}_victim_before", declared(name + "_before", prefix),
                  Predicate("presence", Some(target)), Ply(ply + 1)),
                Fact(s"${name}_victim_after", declared(name, later), Predicate("presence", Some(target)),
                  Event("capture", 1, "after", ply + 1, ply + 2, Some(enemy), Some(target))))
              else
                def contact(at: Board, label: String, moves: List[String], count: Int): Fact =
                  at.originals.collectFirst { case (current, original) if original == target => current.square }.fold(
                    Fact(label, declared(name, moves), Predicate("presence", Some(target)), Ply(count))) { square =>
                    Fact(label, declared(name, moves), Predicate("control", Some(enemy), square = Some(square.key)), Ply(count))
                  }
                List(Fact(s"${name}_target", declared(name, later), Predicate("location", Some(target)), Ply(ply + 2)),
                  contact(prior, s"${name}_pressure_before", prefix, ply + 1),
                  contact(after, s"${name}_pressure_after", later, ply + 2))

            def focuses(at: Board): List[Focus] =
              val contacts = controls(at).filter(_._1.side == side).flatMap { (actor, square, sourceId) =>
                at.view.occupantAt(square).flatMap(at.original).toList.filterNot(_ == actor).flatMap { target =>
                  if target.side == side then
                    // A contact becomes a defense question when a real opposing contact names its use.
                    // Uncontested supports remain available in the original board facts.
                    Option.when(target.role != "king" && (root :: reference :: at :: sourceBoards).exists(exposed(_, target)))(
                      Focus("support", Predicate("support", Some(actor), Some(target)), List(sourceId), usageTarget = Some(target)))
                  else
                    Some(Focus("target_access", Predicate("control", Some(actor), square = Some(square.key)), List(sourceId),
                      Option.when(target.role != "king")(Action(actor, square.key, Some(target), None, canonical, List(sourceId)))))
                }
              }
              val passages = at.view.completeFacts(RelationFactKind.PawnAdvanceAffordance).toList.flatten.flatMap { fact =>
                fact.detail match
                  case RelationWitnessDetail.PawnAdvanceAffordance(color, from, to, _) if color == mover =>
                    at.original(RelationColoredPieceWitness(from, EvidencePieceRole("pawn"), color)).map { actor =>
                      Focus("pawn_passage", Predicate("pawn_advance", Some(actor), square = Some(to.key)), List(fact.semanticId),
                        Some(Action(actor, to.key, None, None, canonical, List(fact.semanticId))))
                    }
                  case _ => None
              }
              val destinations = controls(at).filter(_._1.side == side).flatMap { (provider, square, sourceId) =>
                goals.filter(goal => goal.to == square.key && (goal.actor != provider || goal.observedAt.nonEmpty) &&
                  (goal.observedAt.nonEmpty || (root :: reference :: at :: sourceBoards).exists(contested(_, square)))).map { goal =>
                  Focus(if goal.actor == provider then "action_route" else "action_support",
                    Predicate("control", Some(provider), square = Some(square.key)),
                    sourceId :: goal.sourceRefs, Some(goal), Option.when(goal.actor != provider)(goal.actor))
                }
              }
              contacts ++ passages ++ destinations

            val common = focuses(root) ++ focuses(reference)
            def between(from: String, to: String, square: String): Boolean =
              val dx = to(0) - from(0)
              val dy = to(1) - from(1)
              val sx = square(0) - from(0)
              val sy = square(1) - from(1)
              (dx == 0 || dy == 0 || math.abs(dx) == math.abs(dy)) &&
                dx * sy == dy * sx && square != from && square != to &&
                sx * dx + sy * dy > 0 && sx * sx + sy * sy < dx * dx + dy * dy
            def location(at: Board, actor: Piece): Option[String] = at.originals.collectFirst {
              case (current, original) if original == actor && current.role.name == original.role => current.square.key
            }
            def observedAction(line: List[String], offset: Int): Option[Action] = inspect(line.take(offset)).flatMap(at =>
              at.view.legalRows.toList.flatten.find(_.move == line(offset)).filter(_.promotionRole.isEmpty)
                .flatMap(action(at, _, canonical, Some(offset))))
            case class Attack(actor: Piece, target: Piece, square: String, departures: List[(Piece, String, Action)])
            case class Change(line: List[String], cause: Action, exchange: Option[Action], attacks: List[Attack])
            val changeCache = scala.collection.mutable.Map.empty[List[String], Option[Change]]
            def changedAttacks(line: List[String]): Option[Change] = changeCache.getOrElseUpdate(line, {
              val offset = line.size - 1
              for
                prior <- inspect(line.init)
                after <- inspect(line)
                cause <- observedAction(line, offset)
                exchange = Option.when(offset > 0)(offset - 1).flatMap(observedAction(line, _)).filter(previous =>
                  previous.capture.nonEmpty && cause.capture.contains(previous.actor) && cause.to == previous.to)
                contacts = controls(after).flatMap { (attacker, square, _) =>
                  for
                    from <- location(after, attacker).toList
                    target <- after.view.occupantAt(square).flatMap(after.original).toList
                    if target.side != attacker.side && target.role != "pawn" && target.role != "king" &&
                      location(prior, target).contains(square.key) && location(after, target).contains(square.key)
                    predicate = Predicate("control", Some(attacker), square = Some(square.key))
                    if value(prior, predicate).value.contains("absent")
                    // A stationary slider's new attack needs actual blockers leaving this
                    // very ray. A later capture elsewhere is not its cause.
                    departures = if attacker == cause.actor then Nil else
                      Option.when(Set("bishop", "rook", "queen")(attacker.role) && location(prior, attacker).contains(from)) {
                        val stable = (0 to offset).reverse.takeWhile(count => inspect(line.take(count)).exists(at =>
                          location(at, attacker).contains(from) && location(at, target).contains(square.key))).last
                        val since = (stable to offset).reverse.find(count => inspect(line.take(count)).exists(at =>
                          value(at, predicate).value.contains("present"))).getOrElse(stable)
                        val blockers = (since to offset).flatMap(count => inspect(line.take(count)).toList
                          .flatMap(_.originals.toList).filter((current, _) => between(from, square.key, current.square.key))
                          .map(_._2)).distinct
                        blockers.flatMap { blocker =>
                          (since to offset).reverseIterator.flatMap { index =>
                            for
                              before <- inspect(line.take(index))
                              old <- location(before, blocker) if between(from, square.key, old)
                              after <- inspect(line.take(index + 1))
                              if !location(after, blocker).exists(between(from, square.key, _))
                              step <- observedAction(line, index) if step.actor == blocker || step.capture.contains(blocker)
                            yield (blocker, old, step)
                          }.take(1).toList
                          }
                      }.toList.flatten
                    if attacker == cause.actor || departures.nonEmpty
                  yield Attack(attacker, target, square.key, departures)
                }
                distinct = contacts.distinctBy(contact => contact.actor -> contact.target)
                selected = distinct.filter(contact => contact.departures.nonEmpty ||
                  (exchange.nonEmpty && distinct.count(_.actor.side == contact.actor.side) >= 2))
                if selected.nonEmpty
              yield Change(line, cause, exchange, selected)
            })
            branches.toList.sortBy(_._1).filterNot(_._1 == anchor.referenceMove).foreach { (alternativeMove, alternative) =>
              val referencePrefix = start :+ anchor.referenceMove
              val alternativePrefix = start :+ alternativeMove
              val referenceRows = reference.view.legalRows.toList.flatten
              val alternativeRows = alternative.view.legalRows.toList.flatten
              val replyBoards = scala.collection.mutable.Map.empty[String, (Option[Board], Option[Board])]
              def afterReply(reply: LegalMoveResource, leftRow: Option[LegalMoveResource], rightRow: Option[LegalMoveResource]) =
                replyBoards.getOrElseUpdate(reply.move, (
                  leftRow.flatMap(_ => inspect(referencePrefix :+ reply.move)),
                  rightRow.flatMap(_ => inspect(alternativePrefix :+ reply.move))))
              val moved = rows.filter(row => Set(anchor.referenceMove, alternativeMove)(row.move)).flatMap(row => root.original(row.actor)).toSet
              val candidates = (common ++ focuses(alternative)).distinctBy(f => (f.kind, f.predicate, f.action, f.usageTarget))
              candidates.foreach { focus =>
                val initial = value(root, focus.predicate)
                val left = value(reference, focus.predicate)
                val right = value(alternative, focus.predicate)
                // Equal effects are meaningful when two different moves create/preserve the same changed condition.
                val changed = left.value.nonEmpty && right.value.nonEmpty &&
                  (left.value != right.value || initial.value.exists(v => !left.value.contains(v)))
                // Moving a pawn onto/past its old destination is not another piece blocking its advance.
                val passageChange = focus.kind != "pawn_passage" || !focus.predicate.subject.exists(moved)
                if changed && passageChange then
                  val relation = if left.value == right.value then "same" else "different"
                  val basis: List[Requirement] = List(
                    Fact("before", declared("before", start), focus.predicate, Ply(ply)),
                    Fact("reference", declared("reference", referencePrefix), focus.predicate, Ply(ply + 1)),
                    Fact("alternative", declared("alternative", alternativePrefix), focus.predicate, Ply(ply + 1)),
                    FactComparison("contrast", "reference", "alternative", relation))
                  // Contact alone does not establish a usable defense: either attacker or provider
                  // may be pinned. Compare the actual opposing turn, then the same legal capture.
                  val defense: List[Requirement] = if focus.kind != "support" then Nil else (for
                    provider <- focus.predicate.subject.toList
                    target <- focus.usageTarget.toList
                    enemy <- (root :: reference :: alternative :: sourceBoards).flatMap(enemiesContacting(_, target))
                      .distinct.sortBy(_.json.toString)
                  yield
                    val capture = Predicate("legal_capture", Some(enemy), Some(target))
                    val captureId = CommentarySelection.stableId("capture", List(enemy.json.toString, target.json.toString))
                    val conditions: List[Requirement] = List(
                      Fact(s"reference_$captureId", declared("reference", referencePrefix), capture, Ply(ply + 1)),
                      Fact(s"alternative_$captureId", declared("alternative", alternativePrefix), capture, Ply(ply + 1)))
                    val replies = if enemy.role == "king" then Nil else referenceRows.filter(row =>
                      reference.original(row.actor).contains(enemy) && row.capture.flatMap(reference.original).contains(target))
                      .flatMap { leftRow =>
                        alternativeRows.find(row => row.move == leftRow.move &&
                          alternative.original(row.actor).contains(enemy) && row.capture.flatMap(alternative.original).contains(target))
                          .toList.flatMap { rightRow =>
                            val leftMoves = referencePrefix :+ leftRow.move
                            val rightMoves = alternativePrefix :+ rightRow.move
                            val (leftEnd, rightEnd) = afterReply(leftRow, Some(leftRow), Some(rightRow))
                            if leftEnd.isEmpty then requireReplay(leftMoves)
                            if rightEnd.isEmpty then requireReplay(rightMoves)
                            val replyId = CommentarySelection.stableId("reply", List(captureId, leftRow.move))
                            // The legal inventory can also name a different piece that performs the
                            // recapture. It is not inferred from geometry or restricted to the provider.
                            val replacements = (leftEnd.toList ++ rightEnd.toList).flatMap { end =>
                              end.view.legalRows.toList.flatten.filter(_.capture.flatMap(end.original).contains(enemy))
                                .flatMap(row => end.original(row.actor))
                            }.filter(actor => actor.side == side && actor != provider).distinct.sortBy(_.json.toString)
                            (provider :: replacements).flatMap { recapturer =>
                              val suffix = CommentarySelection.stableId("recapture", List(replyId, recapturer.json.toString))
                              val predicate = Predicate("legal_capture", Some(recapturer), Some(enemy))
                              List(
                                Fact(s"reference_$suffix", declared(s"reference_$replyId", leftMoves), predicate, Ply(ply + 2)),
                                Fact(s"alternative_$suffix", declared(s"alternative_$replyId", rightMoves), predicate, Ply(ply + 2)),
                                FactComparison(s"contrast_$suffix", s"reference_$suffix", s"alternative_$suffix", "different"))
                            }
                          }
                      }
                    conditions ++ replies
                  ).flatten
                  // Test availability at the next decision by this same side, never by changing the turn bit.
                  val actionFacts = focus.action.toList.flatMap { goal =>
                    def availability(name: String, at: Board, prefix: List[String]): Fact =
                      val removedTarget = goal.capture.filter(target => value(at, Predicate("presence", Some(target))).value.contains("removed"))
                      val removedActor = value(at, Predicate("presence", Some(goal.actor))).value.contains("removed")
                      val arrived = goal.capture.isEmpty && value(at, Predicate("location", Some(goal.actor))).value.contains(goal.to)
                      if removedTarget.nonEmpty then
                        Fact(s"${name}_removed_target", declared(name, prefix), Predicate("presence", removedTarget), Ply(ply + 1))
                      else if removedActor then
                        Fact(s"${name}_removed_actor", declared(name, prefix), Predicate("presence", Some(goal.actor)), Ply(ply + 1))
                      else if arrived && goal.observedAt.isEmpty then
                        Fact(s"${name}_arrival", declared(name, prefix), Predicate("location", Some(goal.actor)), Ply(ply + 1))
                      else Fact(s"${name}_action", declared(name, prefix, true), goal.predicate, Ply(ply + 2))
                    val available: List[Requirement] = List(availability("reference", reference, referencePrefix),
                      availability("alternative", alternative, alternativePrefix))
                    available ++ goal.observedAt.toList.flatMap(_ => List(
                      Fact("reference_event_condition", declared("reference", referencePrefix, true),
                        focus.predicate, goal.event(ply, "before")),
                      Fact("alternative_event_condition", declared("alternative", alternativePrefix, true),
                        focus.predicate, goal.event(ply, "before")),
                      Fact("reference_outcome", declared("reference", referencePrefix, true),
                        Predicate("location", Some(goal.actor)), goal.event(ply, "after")),
                      Fact("alternative_outcome", declared("alternative", alternativePrefix, true),
                        Predicate("location", Some(goal.actor)), goal.event(ply, "after"))))
                  }
                  // Join the same capture event across different move orders, keeping every supplied witness.
                  // An event missing within this finite horizon is not proof that the action is impossible.
                  val useEvents = for
                    provider <- focus.predicate.subject.toList
                    target <- focus.usageTarget.toList
                    event <- supportUses(provider, target)
                  yield event
                  val usage = useEvents.flatMap { (predicate, event, _) =>
                    val suffix = CommentarySelection.stableId("use", List(predicate.json.toString, event.json.toString))
                    List(Fact(s"reference_$suffix", declared("reference", referencePrefix, true), predicate, event),
                      Fact(s"alternative_$suffix", declared("alternative", alternativePrefix, true), predicate, event))
                  }
                  emit(anchor, List(start.mkString(" "), anchor.referenceMove, focus.predicate.json.toString,
                    focus.action.map(goal => goal.predicate.json.toString + ":" +
                      goal.observedAt.map(_ => goal.event(ply, "before").json.toString).getOrElse("next_turn")).getOrElse(""),
                    useEvents.map(_._2.json.toString).mkString("|")),
                    focus.kind, focus.sourceRefs ++ initial.sourceIds ++ left.sourceIds ++ right.sourceIds,
                    Source("generator", "C-observed-conditions", Some(GeneratorVersion),
                      (useEvents.flatMap(_._3) ++ focus.action.toList.flatMap(_.source.sourceRefs)).distinct),
                    basis ++ defense ++ actionFacts ++ usage, List("contrast"), Some(alternativeMove))
              }

              // One current choice owns its comparison. Capture options and observed quiet
              // replies are evidence for that question, not independent copies of the pair.
              if start.isEmpty then
                val observations = scala.collection.mutable.ListBuffer.empty[List[Requirement]]
                val captures = (referenceRows.flatMap(action(reference, _, canonical)) ++
                  alternativeRows.flatMap(action(alternative, _, canonical)))
                  .filter(_.capture.nonEmpty).distinctBy(_.predicate).sortBy(_.predicate.json.toString)
                captures.foreach { capture =>
                  val predicate = capture.predicate
                  val left = value(reference, predicate).value
                  val right = value(alternative, predicate).value
                  if left.nonEmpty && right.nonEmpty && left != right &&
                      (left.contains("legal") || right.contains("legal")) then
                    val id = CommentarySelection.stableId("capture", List(predicate.json.toString))
                    observations += List(
                      Fact(s"opponent_reference_$id", declared("reference", referencePrefix), predicate, Ply(ply + 1), expected = left),
                      Fact(s"opponent_alternative_$id", declared("alternative", alternativePrefix), predicate, Ply(ply + 1), expected = right),
                      FactComparison(s"opponent_contrast_$id", s"opponent_reference_$id", s"opponent_alternative_$id", "different"))
                }
                // Inspect only already supplied next replies; do not expand a tactical tree.
                // Multiple geometric attacks identify a question, never a forced fork or gain.
                List(("reference", referencePrefix, reference), ("alternative", alternativePrefix, alternative)).foreach {
                  (arm, prefix, prior) =>
                    boundedSources.map(_.moves).filter(moves => moves.startsWith(prefix) && moves.size > prefix.size)
                      .map(_.take(prefix.size + 1)).distinct.sortBy(_.mkString(" ")).foreach { line =>
                        for
                          row <- prior.view.legalRows.toList.flatten.find(_.move == line.last)
                            if row.capture.isEmpty && row.promotionRole.isEmpty
                          enemy <- prior.original(row.actor)
                          after <- inspect(line)
                          targets = attacked(after, enemy).filter(_.role != "pawn").sortBy(_.json.toString)
                          if targets.size >= 2 && targets.exists(target => !attacked(prior, enemy).contains(target))
                        do
                          val id = s"reply_${arm}_${row.move}"
                          val path = declared(id, line)
                          val event = Event("move", 1, "after", ply + 1, ply + 2, Some(enemy), square = Some(row.to.key))
                          val actor = Fact(id + "_action", path, Predicate("location", Some(enemy)), event, expected = Some(row.to.key))
                          val contacts = targets.flatMap { target =>
                            val square = after.originals.collectFirst { case (current, original) if original == target => current.square.key }.get
                            val predicate = Predicate("control", Some(enemy), square = Some(square))
                            val name = s"${id}_${target.square}"
                            List(Fact(name + "_target", path, Predicate("location", Some(target)), Ply(ply + 2), expected = Some(square)),
                              Fact(name + "_before", path, predicate, Ply(ply + 1)),
                              Fact(name + "_after", path, predicate, Ply(ply + 2), expected = Some("present")))
                          }
                          observations += (actor :: contacts) :+ actor.copy(id = "current_" + actor.id,
                            path = Path("current_" + id, Comparison("contrast_opponent_choice", arm)))
                      }
                }
                val commonReplies = boundedSources.flatMap(source =>
                  Option.when(source.moves.startsWith(referencePrefix) || source.moves.startsWith(alternativePrefix))(
                    source.moves.lift(1)).flatten).distinct.sorted
                commonReplies.foreach { reply =>
                  for
                    left <- inspect(referencePrefix :+ reply)
                    right <- inspect(alternativePrefix :+ reply)
                    step <- observedAction(referencePrefix :+ reply, 1)
                  do
                    val targets = (attacked(left, step.actor) ++ attacked(right, step.actor)).filterNot(_.role == "king").distinct
                    targets.foreach { target =>
                      val providers = (controls(left) ++ controls(right)).map(_._1)
                        .filter(actor => actor.side == target.side && actor != target).distinct
                      providers.foreach { provider =>
                        val predicate = Predicate("support", Some(provider), Some(target))
                        val a = value(left, predicate).value
                        val b = value(right, predicate).value
                        if Set(a, b) == Set(Some("present"), Some("absent")) then
                          val id = s"defense_${reply}_${provider.square}_${target.square}"
                          val facts = List(
                            Fact(id + "_reference", declared(id + "_reference", referencePrefix :+ reply), predicate, Ply(2), expected = a),
                            Fact(id + "_alternative", declared(id + "_alternative", alternativePrefix :+ reply), predicate, Ply(2), expected = b))
                          val current = List("reference", "alternative").map(arm => Fact(s"${id}_current_$arm",
                            Path(s"${id}_current_$arm", Comparison("contrast_opponent_choice", arm)),
                            Predicate("location", Some(step.actor)), step.event(1, "after"), expected = Some(step.to)))
                          observations += facts ++ current
                          supportUses(provider, target).foreach { (use, event, _) =>
                            val suffix = CommentarySelection.stableId("use", List(use.json.toString, event.json.toString))
                            List("reference" -> a, "alternative" -> b).filter(_._2.contains("present")).foreach { (arm, _) =>
                              val name = s"${id}_use_${arm}_$suffix"
                              val path = Path(name, Comparison("contrast_opponent_choice", arm))
                              // A later recapture uses this initial support only if the
                              // same relationship survives until the target is captured.
                              val loss = Event("capture", 1, "before", 2, event.throughPly, event.target, Some(target))
                              observations += List(
                                Fact(name, path, use, event, expected = Some("true")),
                                Fact(name + "_continuity", path, predicate, Ply(2), Some(loss), expected = Some("present")))
                            }
                          }
                      }
                    }
                }
                List("reference" -> referencePrefix, "alternative" -> alternativePrefix).foreach { (arm, prefix) =>
                  val changes = boundedSources.filter(_.moves.startsWith(prefix)).flatMap(source =>
                    source.moves.inits.filter(_.size >= 3).flatMap(changedAttacks)).distinctBy(_.line)
                    .sortBy(change => (change.line.size, change.line.mkString(" ")))
                  changes.foreach { change =>
                    val line = change.line
                    val offset = line.size - 1
                    val id = "tactic_" + arm + "_" + CommentarySelection.stableId("step", line)
                    val path = declared(id, line)
                    def eventFact(name: String, subject: Piece, step: Action, expected: String) =
                      Fact(name, path, Predicate("location", Some(subject)), step.event(step.observedAt.get, "after"), expected = Some(expected))
                    val cause = eventFact(id + "_cause", change.cause.actor, change.cause, change.cause.to)
                    val exchange = change.exchange.toList.flatMap(step => step.capture.toList.map(target =>
                      eventFact(id + "_exchange", target, step, "removed")))
                    val facts = change.attacks.zipWithIndex.flatMap { (contact, index) =>
                      val name = s"${id}_contact_$index"
                      val pressure = Predicate("control", Some(contact.actor), square = Some(contact.square))
                      List(
                        Fact(name + "_target", path, Predicate("location", Some(contact.target)), Ply(line.size), expected = Some(contact.square)),
                        Fact(name + "_before", path, pressure, Ply(offset), expected = Some("absent")),
                        Fact(name + "_after", path, pressure, Ply(line.size), expected = Some("present"))) ++
                        contact.departures.zipWithIndex.flatMap { case ((blocker, square, step), index) =>
                          List(Fact(s"${name}_blocker_${index}_before", path, Predicate("location", Some(blocker)),
                            Ply(step.observedAt.get), expected = Some(square)),
                            eventFact(s"${name}_blocker_${index}_left", blocker, step,
                              if step.actor == blocker then step.to else "removed"))
                        }
                    }
                    observations += ((cause :: exchange) ++ facts :+ cause.copy(id = "current_" + cause.id,
                      path = Path("current_" + id, Comparison("contrast_opponent_choice", arm))))

                    // Follow the current compound capture, or a defensive move opening
                    // a counterattack. An unrelated single contact cannot expand an
                    // existing preparation investigation beyond its selected PV.
                    // One-ply probes cannot seed further plans or discovery anchors.
                    if change.attacks.exists(_.actor.side == change.cause.actor.side) then
                      // Select the current decision before choosing its earliest capture.
                      // A shorter historical branch must not hide a later current one.
                      val continuations = packet.assembly.input.assessmentCandidates.map(_.evaluation.moves.take(MaximumPly))
                        .filter(_.startsWith(line)).distinct
                      val compound = change.attacks.count(_.actor.side == change.cause.actor.side) >= 2
                      val counterplay = inspect(line.init).exists(before => location(before, change.cause.actor).exists(square =>
                        controls(before).exists((actor, target, _) => actor.side != change.cause.actor.side && target.key == square)))
                      val decisions = (if !compound then Option.when(counterplay && continuations.exists(_.size > line.size))(line).toList
                        else continuations.flatMap(moves => (line.size until moves.size).flatMap { count =>
                        observedAction(moves, count - 1).filter(step => step.actor.side == change.cause.actor.side &&
                          step.capture.exists(target => change.attacks.exists(_.target == target))).map(_ => moves.take(count))
                      })).distinct.sortBy(moves => (moves.size, moves.mkString(" ")))
                      decisions.headOption.foreach { atMoves =>
                        inspect(atMoves).foreach { at =>
                          val observed = continuations.find(_.startsWith(atMoves)).flatMap(_.lift(atMoves.size))
                          val pressure = change.attacks.filter(contact => contact.actor.side == change.cause.actor.side &&
                            location(at, contact.target).contains(contact.square) &&
                            value(at, Predicate("control", Some(contact.actor), square = Some(contact.square))).value.contains("present"))
                          val certified = at.path.flatMap(_.positions.lift(at.count)).flatMap(_.id).flatMap(views.get)
                          val checks = certified.toList.flatMap(_.checkingMoves.toList.flatten.map(_.move)).toSet
                          val support = pressure.flatMap(contact => at.originals.collectFirst {
                            case (current, original) if original == contact.target => current
                          }.toList.flatMap(target => certified.toList.flatMap(_.supportCandidates(target))
                            .map(_.semanticId -> contact.target))).groupMap(_._1)(_._2)
                          def kinds(row: LegalMoveResource): Set[String] =
                            pressure.flatMap { contact =>
                              val target = contact.target.json.toString
                              Option.when(at.original(row.actor).contains(contact.target) ||
                                row.capture.flatMap(at.original).contains(contact.actor))("direct:" + target).toList ++
                                Option.when(!at.original(row.actor).contains(contact.target) && location(at, contact.actor).exists(from =>
                                  Set("bishop", "rook", "queen")(contact.actor.role) && between(from, contact.square, row.to.key)))("block:" + target) ++
                                Option.when(support.getOrElse(row.sourceId, Nil).contains(contact.target))("support:" + target)
                            }.toSet ++ Option.when(checks(row.move))("check") ++
                              row.capture.flatMap(at.original).filter(target => target.role != "pawn" && target.role != "king" &&
                                !pressure.exists(_.actor == target)).map(target => "capture:" + target.json.toString)
                          val replies = at.view.legalRows.toList.flatten.filter(_.promotionRole.isEmpty)
                            .map(row => row -> kinds(row)).filter(_._2.nonEmpty).sortBy(_._1.move)
                          for
                            reply <- observed.toList
                            (counter, effects) <- replies if counter.move != reply
                          do
                            val choice = (atMoves, reply, counter.move)
                            val direct = effects.exists(_.startsWith("direct:"))
                            val previous = tacticalChoices.getOrElse(choice, false -> Set.empty[String])
                            tacticalChoices.update(choice, (previous._1 || direct, previous._2 ++ effects))
                            val actor = at.original(counter.actor)
                            val predicates = (pressure.flatMap(contact => List(
                              Predicate("legal_capture", Some(contact.actor), Some(contact.target)),
                              Predicate("control", Some(contact.actor), square = Some(contact.square)),
                              Predicate("location", Some(contact.target)),
                              Predicate("presence", Some(contact.actor)))) ++ actor.toList.flatMap(provider =>
                              pressure.filter(_.target != provider).map(contact => Predicate("support", Some(provider), Some(contact.target)))) ++
                              Option.when(checks(counter.move))(Predicate("check", side = Some(change.cause.actor.side))) ++
                              actor.filter(_ => effects.exists(_.startsWith("block:"))).map(value => Predicate("location", Some(value))) ++
                              counter.capture.flatMap(at.original).map(target => Predicate("presence", Some(target)))).distinct
                            val compared = List(reply, counter.move).map(move => inspect(atMoves :+ move))
                            if compared.exists(_.isEmpty) && packet.assembly.input.assessmentCandidates.exists(candidate =>
                                candidate.evaluation.moves.startsWith(atMoves :+ reply)) then
                              // Reserve the entire possible fact pair, not just the old
                              // capture-only cost, before spending a local replay.
                              val cost = atMoves.size + 4 + 2 * predicates.size
                              tacticalProbes.update(choice, cost.max(tacticalProbes.getOrElse(choice, 0)))
                            if compared.forall(_.nonEmpty) then
                              val left = compared.head.get
                              val right = compared.last.get
                              val observedConditions = predicates.flatMap { predicate =>
                                for
                                  a <- value(left, predicate).value
                                  b <- value(right, predicate).value if a != "not_turn" && b != "not_turn"
                                yield (predicate, a, b)
                              }
                              val changed = observedConditions.filter((predicate, a, b) => a != b || predicate.kind == "legal_capture")
                              if changed.exists((_, a, b) => a != b) then
                                val rootChoice = CompareMoves("tactical_root", Nil, anchor.referenceMove, alternativeMove)
                                val currentLine = atMoves :+ reply
                                val current = currentLine.indices.toList.flatMap(index => observedAction(currentLine, index).map(step =>
                                  Fact(s"tactical_current_$index", Path("tactical_current", Comparison(rootChoice.id, arm)),
                                    Predicate("location", Some(step.actor)), step.event(index, "after"), expected = Some(step.to))))
                                if current.size == currentLine.size then
                                  val conditions = changed.flatMap { (predicate, a, b) =>
                                    val id = (if direct then "tactical_direct_" else "tactical_effect_") +
                                      CommentarySelection.stableId("condition", List(predicate.json.toString))
                                    List(Fact(id + "_reference", declared(id + "_reference", atMoves :+ reply), predicate,
                                      Ply(atMoves.size + 1), expected = Some(a)),
                                      Fact(id + "_alternative", declared(id + "_alternative", atMoves :+ counter.move), predicate,
                                        Ply(atMoves.size + 1), expected = Some(b)))
                                  }
                                  emit(anchor, List(atMoves.mkString(" "), reply, counter.move, arm, alternativeMove,
                                    changed.map(_._1.json.toString).sorted.mkString(";"), direct.toString),
                                    "tactical-counter", Nil, canonical, List(rootChoice) ++ current ++ conditions ++ List(
                                      FactGroup("tactical_context", "all", current.map(_.id) ++ conditions.map(_.id)),
                                      CompareMoves("tactical_strength", atMoves, reply, counter.move, Some("better"))),
                                    List("tactical_context", "tactical_strength"),
                                    question = Some(s"After ${notation.line(atMoves).mkString(" ")}, what does ${notation.move(atMoves, counter.move)} change compared with ${notation.move(atMoves, reply)}, and what is gained or lost against the tested continuation? A changed defensive condition is not a successful defense; the original choice still requires root verification."),
                                    additionalHeads = List(List("tactical_root")))
                        }
                      }
                  }
                }
                // The current source candidates deserve a comparison even before a
                // recognizable contact explains their difference. Keep the existing
                // reference-to-candidate star; probes never become extra root candidates.
                if observations.nonEmpty || (sourcePrefixes(referencePrefix) && sourcePrefixes(alternativePrefix)) then
                  val kingContexts = kingSupportConditions.filter((moves, _, _) =>
                    moves.startsWith(referencePrefix) || moves.startsWith(alternativePrefix)).map { (moves, capture, support) =>
                    val id = CommentarySelection.stableId("king-support", List(moves.mkString(" "), capture.json.toString, support.json.toString))
                    List[Requirement](
                      Fact(id + "_capture", declared(id, moves), capture, Ply(moves.size), expected = Some("illegal")),
                      Fact(id + "_support", declared(id, moves), support, Ply(moves.size), expected = Some("present")),
                      FactGroup(id, "all", List(id + "_capture", id + "_support")))
                  }
                  val choices = List(("reference", anchor.referenceMove, referencePrefix), ("alternative", alternativeMove, alternativePrefix))
                    .map { (name, move, prefix) => Fact(s"choice_$name", declared(name, prefix),
                      action(root, rows.find(_.move == move).get, canonical).get.predicate, Ply(ply), expected = Some("legal")) }
                  val requirements = choices ++ List(
                    FactGroup("opponent_context", "all", choices.map(_.id)),
                    CompareMoves("contrast_opponent_choice", start, anchor.referenceMove, alternativeMove))
                  emit(anchor, List(alternativeMove), "opponent-choice", Nil, canonical, requirements,
                    List("opponent_context", "contrast_opponent_choice"),
                    question = Some(s"How do ${notation.move(start, anchor.referenceMove)} and ${notation.move(start, alternativeMove)} change the opponent's concrete options and replies, and which continuation is preferable? A legal capture alone does not establish a threat or a loss; simultaneous attacks do not establish a forced gain."),
                    additionalHeads = kingContexts.map(bundle => List(bundle.last.id, "contrast_opponent_choice")),
                    bundles = observations.toList.sortBy(bundle => if bundle.head.id.startsWith("reply_") then 0 else 1) ++ kingContexts)

              // A changed controlled destination is only a probe entrance. The opponent must actually affect
              // a physical target or check, and the corresponding capture response must differ across arms.
              val changedControls = (controls(reference) ++ controls(alternative)).filter(_._1.side == side)
                .distinctBy((actor, square, _) => actor -> square).filter { (actor, square, _) =>
                  val predicate = Predicate("control", Some(actor), square = Some(square.key))
                  val left = value(reference, predicate).value
                  val right = value(alternative, predicate).value
                  left.nonEmpty && right.nonEmpty && left != right
                }
              val repliesByDestination = (referenceRows ++ alternativeRows).distinctBy(_.move).map { reply =>
                (reply, referenceRows.find(_.move == reply.move), alternativeRows.find(_.move == reply.move))
              }.groupBy(_._1.to)
              val replyContexts = scala.collection.mutable.Map.empty[String, List[(String, List[Requirement])]]
              // Share destination/reply work without changing provider, draft, or replay order.
              changedControls.foreach { (provider, destination, sourceId) =>
                repliesByDestination.getOrElse(destination, Nil).foreach { (reply, leftRow, rightRow) =>
                  if leftRow.isDefined != rightRow.isDefined then
                    val availableBoard = if leftRow.nonEmpty then reference else alternative
                    val availablePrefix = if leftRow.nonEmpty then referencePrefix else alternativePrefix
                    val availableRow = leftRow.orElse(rightRow).get
                    availableBoard.original(availableRow.actor).foreach { enemy =>
                      val (leftEnd, rightEnd) = afterReply(reply, leftRow, rightRow)
                      val availableEnd = leftEnd.orElse(rightEnd)
                      if enemy.role != "king" && availableEnd.isEmpty then requireReplay(availablePrefix :+ reply.move)
                      val predicate = Predicate("legal_move", Some(enemy), square = Some(destination.key), property = reply.promotionRole)
                      val a = value(reference, predicate)
                      val b = value(alternative, predicate)
                      if a.value.nonEmpty && b.value.nonEmpty && a.value != b.value then
                        val contexts = if enemy.role == "king" then List("king_access" -> List(
                          Fact("reference_control", declared("reference", referencePrefix),
                            Predicate("control", Some(provider), square = Some(destination.key)), Ply(ply + 1)),
                          Fact("alternative_control", declared("alternative", alternativePrefix),
                            Predicate("control", Some(provider), square = Some(destination.key)), Ply(ply + 1))))
                        else replyContexts.getOrElseUpdate(reply.move, {
                          val victim = availableRow.capture.flatMap(availableBoard.original)
                          val targets = (victim.toList ++ availableEnd.toList.flatMap(attacked(_, enemy))).distinct
                          availableEnd.toList.flatMap(after => targets.map(target =>
                            (target.json.toString + ":" + victim.contains(target)) -> pressure(
                            if leftRow.nonEmpty then "reference_reason" else "alternative_reason", availablePrefix,
                            availableBoard, after, enemy, target, victim.contains(target))))
                        })
                        contexts.foreach { (context, facts) =>
                          emit(anchor, List(start.mkString(" "), anchor.referenceMove, reply.move, predicate.json.toString,
                            provider.json.toString, context),
                            "opponent_access", List(sourceId) ++ a.sourceIds ++ b.sourceIds, canonical, List(
                              Fact("reference_access", declared("reference", referencePrefix), predicate, Ply(ply + 1)),
                              Fact("alternative_access", declared("alternative", alternativePrefix), predicate, Ply(ply + 1)),
                              FactComparison("contrast", "reference_access", "alternative_access", "different")) ++ facts,
                            List("contrast"), Some(alternativeMove))
                        }
                    }
                  if leftRow.nonEmpty && rightRow.nonEmpty then
                    val leftMoves = referencePrefix :+ reply.move
                    val rightMoves = alternativePrefix :+ reply.move
                    val (leftEnd, rightEnd) = afterReply(reply, leftRow, rightRow)
                    if leftEnd.isEmpty then requireReplay(leftMoves)
                    if rightEnd.isEmpty then requireReplay(rightMoves)
                    for
                      left <- leftEnd
                      right <- rightEnd
                      enemy <- reference.original(leftRow.get.actor)
                      if alternative.original(rightRow.get.actor).contains(enemy) && enemy.role != "king"
                    do
                      val predicate = Predicate("legal_capture", Some(provider), Some(enemy))
                      val a = value(left, predicate)
                      val b = value(right, predicate)
                      if a.value.nonEmpty && b.value.nonEmpty && a.value != b.value then
                        val contexts = replyContexts.getOrElseUpdate(reply.move, {
                          val victims = (leftRow.get.capture.flatMap(reference.original).toList ++
                            rightRow.get.capture.flatMap(alternative.original)).distinct
                          val targets = (victims ++ attacked(left, enemy) ++ attacked(right, enemy)).distinct
                          targets.map(target => (target.json.toString + ":" + victims.contains(target)) -> (
                            pressure("reference_reply", referencePrefix, reference, left, enemy, target, victims.contains(target)) ++
                            pressure("alternative_reply", alternativePrefix, alternative, right, enemy, target, victims.contains(target))))
                        })
                        contexts.foreach { (context, facts) =>
                          emit(anchor, List(start.mkString(" "), anchor.referenceMove, reply.move, predicate.json.toString, context),
                            "opponent_entry", List(sourceId) ++ a.sourceIds ++ b.sourceIds, canonical, List(
                              Fact("reference_reply", declared("reference_reply", leftMoves), predicate, Ply(ply + 2)),
                              Fact("alternative_reply", declared("alternative_reply", rightMoves), predicate, Ply(ply + 2)),
                              FactComparison("contrast", "reference_reply", "alternative_reply", "different")) ++ facts,
                            List("contrast"), Some(alternativeMove))
                        }
                }
              }

              if start.isEmpty && Set(anchor.referenceMove, alternativeMove)
                  .contains(packet.assembly.input.playedMoveUci) then
                // A useful purpose can belong to either root, including a
                // close/equal alternative. Engine ordering is not proof direction.
                def inspectFunction(anchor: Anchor, alternativeMove: String,
                    reference: Board, alternative: Board,
                    referencePrefix: List[String], alternativePrefix: List[String]): Unit =
                  val referenceRows = reference.view.legalRows.toList.flatten
                  val alternativeRows = alternative.view.legalRows.toList.flatten
                  // These short paths test a possible resource. They never seed
                  // later goals/anchors and never supply an engine evaluation.
                  def functionalBoard(moves: List[String]): Option[Board] =
                    val current = inspect(moves)
                    if current.isEmpty then
                      functionalReplays += moves
                      replays += moves
                      pending += moves
                    current
                  def sameMover(left: Board, right: Board, a: LegalMoveResource, b: LegalMoveResource): Boolean =
                    a.move == b.move && left.original(a.actor).nonEmpty &&
                      left.original(a.actor) == right.original(b.actor) &&
                      a.promotionRole == b.promotionRole
                  def sameResource(left: Board, right: Board, a: LegalMoveResource, b: LegalMoveResource): Boolean =
                    sameMover(left, right, a, b) &&
                      a.capture.flatMap(left.original) == b.capture.flatMap(right.original)
                  def emitFunction(condition: Predicate, leftMoves: List[String], rightMoves: List[String],
                      effect: Predicate, option: LegalMoveResource, at: Board, refs: List[String],
                      price: Option[(LegalMoveResource, Board)] = None,
                      resource: Option[(Action, List[String])] = None): Unit =
                    val optionMoves = if price.isEmpty then leftMoves :+ option.move else leftMoves
                    val optionAt = leftMoves.size - (if price.isEmpty then 0 else 1)
                    val pair = CompareMoves("function_root", Nil, anchor.referenceMove, alternativeMove)
                    val optionAction = action(at, option, canonical).get
                    val facts = List[Requirement](
                      Fact("function_before", declared("function_reference", leftMoves), condition, Ply(0),
                        expected = value(root, condition).value),
                      Fact("function_root_reference", declared("function_reference", leftMoves), condition, Ply(1),
                        expected = value(reference, condition).value),
                      Fact("function_root_alternative", declared("function_alternative", rightMoves), condition, Ply(1),
                        expected = value(alternative, condition).value),
                      FactComparison("function_owned", "function_before", "function_root_reference",
                        if value(root, condition).value == value(reference, condition).value then "same" else "different"),
                      FactComparison("function_changed", "function_root_reference", "function_root_alternative", "different"),
                      Fact("function_live", declared("function_live", List(anchor.referenceMove)), condition, Ply(1),
                        expected = value(reference, condition).value),
                      Fact("function_effect_reference", declared("function_reference", leftMoves), effect, Ply(leftMoves.size),
                        expected = inspect(leftMoves).flatMap(value(_, effect).value)),
                      Fact("function_effect_alternative", declared("function_alternative", rightMoves), effect, Ply(rightMoves.size),
                        expected = inspect(rightMoves).flatMap(value(_, effect).value)),
                      FactComparison("function_effect_changed", "function_effect_reference", "function_effect_alternative", "different"),
                      Fact("function_option", declared("function_option", optionMoves), optionAction.predicate,
                        Ply(optionAt), expected = Some("legal")),
                      Fact("function_other_option", declared("function_alternative", rightMoves), optionAction.predicate,
                        Ply(optionAt), expected = Some(if price.isEmpty then "illegal" else "legal")),
                      Fact("function_option_end", declared("function_option", optionMoves),
                        Predicate("location", Some(optionAction.actor)), Ply(optionMoves.size), expected = Some(optionAction.to))) ++
                      price.toList.flatMap { (capture, _) => List(
                        Fact("function_price", declared("function_price", rightMoves :+ capture.move), effect,
                          Ply(rightMoves.size), expected = Some("legal")),
                        Fact("function_price_end", declared("function_price", rightMoves :+ capture.move),
                          Predicate("presence", effect.target), Ply(rightMoves.size + 1), expected = Some("removed"))) }
                    val context = FactGroup("function_context", "all", facts.map(_.id))
                    // The task owns the original actor and destination before
                    // its use. An incidental capture/recapture realization is
                    // not a prerequisite for valuing that same resource under
                    // a different current defense.
                    val task = resource.filter((goal, moves) => goal.promotion.isEmpty &&
                      moves.size >= 3 && condition.kind == "control" && condition.square.contains(goal.to))
                      .map { (goal, moves) =>
                        val use = Fact("function_resource", declared("function_resource", moves),
                          Predicate("legal_move", Some(goal.actor), square = Some(goal.to)),
                          Ply(moves.size - 1), expected = Some("legal"))
                        val group = FactGroup("function_task_context", "all", List("function_before",
                          "function_root_reference", "function_root_alternative", "function_owned",
                          "function_changed", "function_live", use.id))
                        List[Requirement](use, group)
                      }.getOrElse(Nil)
                    emit(anchor, List(condition.json.toString, effect.json.toString,
                      leftMoves.mkString(" "), rightMoves.mkString(" "), option.move), "reply-function", refs,
                      canonical, pair :: (facts ++ task :+ context), List(context.id), Some(alternativeMove),
                      additionalHeads = Option.when(task.nonEmpty)(List("function_task_context")).toList,
                      question = Some((if price.isEmpty then
                        "Which concrete reply remains or becomes usable against this same conditional resource? " else
                        "Which same legal capture has a different immediate price after this conditional resource? ") +
                        "The root-owned condition and the legal response are separate from the resource's strength and the whole evaluation."))

                  // A king relocation can remove the checking tempo of the same
                  // opponent move while leaving an already legal plan usable.
                  val king = root.originals.values.find(piece => piece.side == side && piece.role == "king")
                  king.foreach { king =>
                    val condition = Predicate("location", Some(king))
                    if value(root, condition).value != value(reference, condition).value &&
                      value(reference, condition).value != value(alternative, condition).value then
                      val checkedReference = reference.view.checkingMoves.toList.flatten.map(_.move).toSet
                      val checkedAlternative = alternative.view.checkingMoves.toList.flatten.map(_.move).toSet
                      alternativeRows.filter(row => checkedAlternative(row.move) && !checkedReference(row.move))
                        .sortBy(_.move).foreach { enemy =>
                          // The same checking move may cease to be a capture
                          // when the alternative has already moved its victim.
                          // Capture/recapture comparisons below still require
                          // the same victim; only the checking tempo differs here.
                          referenceRows.find(row => sameMover(reference, alternative, row, enemy)).foreach { _ =>
                            val leftMoves = referencePrefix :+ enemy.move
                            val rightMoves = alternativePrefix :+ enemy.move
                            for
                              left <- functionalBoard(leftMoves)
                              right <- functionalBoard(rightMoves)
                              if value(left, Predicate("check", side = Some(side))).value.contains("absent") &&
                                value(right, Predicate("check", side = Some(side))).value.contains("present")
                              // Moving the front of a relative pin can expose
                              // its rear piece; a checking tempo may forbid that capture.
                              exposed = root.path.toList.flatMap(path => prepared.purposePosition(path, 0).toList
                                .flatMap(position => left.view.legalRows.toList.flatten.flatMap(row =>
                                  action(left, row, canonical).filter(goal => goal.capture.nonEmpty &&
                                    position.relativeAlignments(goal.actor.witness).exists { (_, front, rear) =>
                                      alternative.original(enemy.actor).exists(_.witness == front) &&
                                        goal.capture.exists(_.witness == rear)
                                    }))))
                              goal <- (exposed ++ observedActions.filter(goal =>
                                value(root, goal.predicate).value.contains("legal"))).filter(goal =>
                                goal.actor.side == side && goal.actor.role != "king" &&
                                value(left, Predicate("location", Some(goal.actor))).value.nonEmpty &&
                                value(left, Predicate("location", Some(goal.actor))).value ==
                                  value(right, Predicate("location", Some(goal.actor))).value &&
                                value(left, goal.predicate).value.contains("legal") &&
                                value(right, goal.predicate).value.contains("illegal"))
                                .sortBy(goal => (if exposed.exists(_.predicate == goal.predicate) then 0 else 1,
                                  goal.observedAt.getOrElse(MaximumPly), goal.predicate.json.toString)).take(1)
                              option <- left.view.legalRows.toList.flatten.find(row =>
                                action(left, row, canonical).exists(_.predicate == goal.predicate))
                            do emitFunction(condition, leftMoves, rightMoves, Predicate("check", side = Some(side)),
                              option, left, goal.sourceRefs :+ enemy.sourceId)
                          }
                        }
                  }

                  // Reuse the current first capture, but inspect its natural
                  // recaptures even when a new PV chooses a different response.
                  // Only an actual checking arrival on a root-changed square
                  // can finish this control/use proof. Legality alone is not value.
                  val firstReplies = boundedSources.filter(source => source.moves.size >= 2 &&
                    Set(anchor.referenceMove, alternativeMove)(source.moves.head)).map(_.moves(1)).toSet
                  val relocated = root.originals.values.toList.distinct.filter(piece =>
                    piece.side == side && piece.role != "king" &&
                      value(root, Predicate("location", Some(piece))).value !=
                        value(reference, Predicate("location", Some(piece))).value &&
                      value(reference, Predicate("location", Some(piece))).value !=
                        value(alternative, Predicate("location", Some(piece))).value)
                  // The opponent's move can already be legal in both arms.
                  // Replay its arrival on a changed guarded square: losing the
                  // specific guard capture is a fact, even if another capture
                  // remains. It is not a safe-square or utility certificate.
                  for
                    guard <- relocated
                    (_, destination, sourceId) <- changedControls.filter(_._1 == guard)
                    arrival <- referenceRows.filter(row => row.to == destination && row.capture.isEmpty &&
                      row.promotionRole.isEmpty).sortBy(row => (if firstReplies(row.move) then 0 else 1, row.move))
                    peer <- alternativeRows.find(row => sameResource(reference, alternative, arrival, row))
                    enemy <- reference.original(arrival.actor) if enemy.role != "king" && enemy.side != guard.side
                    leftMoves = referencePrefix :+ arrival.move
                    rightMoves = alternativePrefix :+ peer.move
                    ends = functionalBoard(leftMoves) -> functionalBoard(rightMoves)
                    left <- ends._1
                    right <- ends._2
                    if firstReplies(arrival.move) || attacked(left, enemy).nonEmpty || attacked(right, enemy).nonEmpty
                    condition = Predicate("location", Some(guard))
                    if (1 to leftMoves.size).forall(count =>
                      inspect(leftMoves.take(count)).exists(at => value(at, condition).value == value(reference, condition).value) &&
                        inspect(rightMoves.take(count)).exists(at => value(at, condition).value == value(alternative, condition).value))
                    effect = Predicate("legal_capture", Some(guard), Some(enemy))
                    if value(left, effect).value.contains("illegal") && value(right, effect).value.contains("legal")
                    capture <- right.view.legalRows.toList.flatten.find(row =>
                      action(right, row, canonical).exists(_.predicate == effect))
                    paid <- functionalBoard(rightMoves :+ capture.move)
                  do emitFunction(condition, leftMoves, rightMoves, effect, arrival, reference,
                    List(sourceId, arrival.sourceId, peer.sourceId, capture.sourceId), Some(capture -> paid))
                  val ownedControls = changedControls.filter { (provider, destination, _) =>
                    val condition = Predicate("control", Some(provider), square = Some(destination.key))
                    value(root, condition).value.nonEmpty && value(reference, condition).value.contains("present") &&
                      value(alternative, condition).value.contains("absent")
                  }
                  // A root can open an unmoved ally's line against a quiet
                  // opposing resource; neither a check nor a recapture is required.
                  for
                    (provider, destination, sourceId) <- ownedControls
                    arrival <- referenceRows.filter(row => firstReplies(row.move) && row.to == destination &&
                      row.capture.isEmpty && row.promotionRole.isEmpty)
                    peer <- alternativeRows.find(row => sameResource(reference, alternative, arrival, row))
                    enemy <- reference.original(arrival.actor) if enemy.role != "king" && enemy.side != provider.side
                    leftMoves = referencePrefix :+ arrival.move
                    rightMoves = alternativePrefix :+ peer.move
                    left <- functionalBoard(leftMoves)
                    right <- functionalBoard(rightMoves)
                    effect = Predicate("legal_capture", Some(provider), Some(enemy))
                    if value(left, effect).value.contains("legal") && value(right, effect).value.contains("illegal")
                    option <- left.view.legalRows.toList.flatten.find(row =>
                      action(left, row, canonical).exists(_.predicate == effect))
                  do emitFunction(Predicate("control", Some(provider), square = Some(destination.key)),
                    leftMoves, rightMoves, effect, option, left, List(sourceId, arrival.sourceId, option.sourceId))
                  if ownedControls.nonEmpty then referenceRows.filter(row => firstReplies(row.move) &&
                      row.capture.nonEmpty && row.promotionRole.isEmpty).foreach { capture =>
                    alternativeRows.find(row => sameResource(reference, alternative, capture, row)).foreach { _ =>
                      val firstLeft = referencePrefix :+ capture.move
                      val firstRight = alternativePrefix :+ capture.move
                      for
                        left <- functionalBoard(firstLeft)
                        right <- functionalBoard(firstRight)
                        capturer <- reference.original(capture.actor)
                        recapture <- left.view.legalRows.toList.flatten.filter(row => row.promotionRole.isEmpty &&
                          row.capture.flatMap(left.original).contains(capturer))
                        other <- right.view.legalRows.toList.flatten.find(row => sameResource(left, right, recapture, row))
                        middleLeft = firstLeft :+ recapture.move
                        middleRight = firstRight :+ other.move
                        a <- functionalBoard(middleLeft)
                        b <- functionalBoard(middleRight)
                        arrival <- a.view.checkingMoves.toList.flatten
                        peer <- b.view.legalRows.toList.flatten.find(row => sameResource(a, b, arrival, row))
                        if b.view.checkingMoves.exists(_.exists(_.move == peer.move))
                        (provider, destination, sourceId) <- ownedControls if destination == arrival.to
                        condition = Predicate("control", Some(provider), square = Some(destination.key))
                        if (1 to 3).forall(count => inspect(middleLeft.take(count)).exists(at =>
                          value(at, condition).value.contains("present")) && inspect(middleRight.take(count)).exists(at =>
                          value(at, condition).value.contains("absent")))
                        leftMoves = middleLeft :+ arrival.move
                        rightMoves = middleRight :+ peer.move
                        endLeft <- functionalBoard(leftMoves)
                        endRight <- functionalBoard(rightMoves)
                        enemy <- a.original(arrival.actor)
                        effect = Predicate("legal_capture", Some(provider), Some(enemy))
                        if value(endLeft, effect).value.contains("legal") && value(endRight, effect).value.contains("illegal")
                        option <- endLeft.view.legalRows.toList.flatten.find(row =>
                          action(endLeft, row, canonical).exists(_.predicate == effect))
                      do emitFunction(condition, leftMoves, rightMoves, effect, option, endLeft,
                        List(sourceId, capture.sourceId, recapture.sourceId, arrival.sourceId))
                    }
                  }

                  // Reuse an observed friendly arrival, but inspect the price
                  // of its capture even when the opponent's PV avoids it.
                  // The same root-owned control can protect a rook, queen or
                  // other piece. The conditional recapture is not a value test.
                  for
                    (provider, destination, sourceId) <- ownedControls
                    (goal, witnessed) <- witnessedActionPaths.filter((goal, line) =>
                      (line.headOption.contains(anchor.referenceMove) ||
                        line.headOption.contains(alternativeMove) && goal.capture.nonEmpty) && line.size > 1 &&
                        goal.actor.side == side && goal.actor != provider && goal.to == destination.key)
                      .distinctBy(_._2).sortBy(_._2.size)
                    // The actual other choice can witness a recapture that the
                    // protected line avoids. Replay it under both roots; this
                    // supplies a conditional option, never the root's forecast.
                    middleLeft = referencePrefix ++ witnessed.drop(referencePrefix.size)
                    middleRight = alternativePrefix ++ middleLeft.drop(referencePrefix.size)
                    a <- functionalBoard(middleLeft)
                    b <- functionalBoard(middleRight)
                    condition = Predicate("control", Some(provider), square = Some(destination.key))
                    if (1 to middleLeft.size).forall(count => inspect(middleLeft.take(count)).exists(at =>
                      value(at, condition).value.contains("present")) && inspect(middleRight.take(count)).exists(at =>
                      value(at, condition).value.contains("absent")))
                    capture <- a.view.legalRows.toList.flatten.filter(row => row.promotionRole.isEmpty &&
                      row.to == destination && row.capture.flatMap(a.original).contains(goal.actor))
                    peer <- b.view.legalRows.toList.flatten.find(row => sameResource(a, b, capture, row))
                    enemy <- a.original(capture.actor)
                    leftMoves = middleLeft :+ capture.move
                    rightMoves = middleRight :+ peer.move
                    endLeft <- functionalBoard(leftMoves)
                    endRight <- functionalBoard(rightMoves)
                    effect = Predicate("legal_capture", Some(provider), Some(enemy))
                    if value(endLeft, effect).value.contains("legal") && value(endRight, effect).value.contains("illegal")
                    option <- endLeft.view.legalRows.toList.flatten.find(row =>
                      action(endLeft, row, canonical).exists(_.predicate == effect))
                  do emitFunction(condition, leftMoves, rightMoves, effect, option, endLeft,
                    (goal.sourceRefs ++ List(sourceId, capture.sourceId)).distinct,
                    resource = Some(goal -> middleLeft))

                  // A legal capture can remain available in both arms while
                  // its price changes. Follow one witnessed challenge, a quiet
                  // escape, and a same-piece central capture/recapture. Every
                  // local branch is replayed; none becomes a forced forecast.
                  if relocated.nonEmpty then referenceRows.filter(row => firstReplies(row.move) &&
                      row.capture.isEmpty && row.promotionRole.isEmpty).foreach { challenge =>
                    alternativeRows.find(row => sameResource(reference, alternative, challenge, row)).foreach { _ =>
                      val firstLeft = referencePrefix :+ challenge.move
                      val firstRight = alternativePrefix :+ challenge.move
                      for
                        left <- functionalBoard(firstLeft)
                        right <- functionalBoard(firstRight)
                        enemy <- reference.original(challenge.actor)
                        challenged <- attacked(left, enemy) if attacked(right, enemy).contains(challenged)
                        retreat <- left.view.legalRows.toList.flatten.filter(row => row.capture.isEmpty &&
                          row.promotionRole.isEmpty && left.original(row.actor).contains(challenged))
                        peer <- right.view.legalRows.toList.flatten.find(row => sameResource(left, right, retreat, row))
                        middleLeft = firstLeft :+ retreat.move
                        middleRight = firstRight :+ peer.move
                        a <- functionalBoard(middleLeft)
                        b <- functionalBoard(middleRight)
                        if !exposed(a, challenged) && !exposed(b, challenged)
                        capture <- a.view.legalRows.toList.flatten.filter(row => row.capture.nonEmpty && row.promotionRole.isEmpty)
                        other <- b.view.legalRows.toList.flatten.find(row => sameResource(a, b, capture, row))
                        capturedLeft = middleLeft :+ capture.move
                        capturedRight = middleRight :+ other.move
                        c <- functionalBoard(capturedLeft)
                        d <- functionalBoard(capturedRight)
                        capturer <- a.original(capture.actor)
                        recapture <- c.view.legalRows.toList.flatten.filter(row => row.promotionRole.isEmpty &&
                          row.capture.flatMap(c.original).contains(capturer))
                        matched <- d.view.legalRows.toList.flatten.find(row => sameResource(c, d, recapture, row))
                        optionLeft = capturedLeft :+ recapture.move
                        optionRight = capturedRight :+ matched.move
                        endLeft <- functionalBoard(optionLeft)
                        endRight <- functionalBoard(optionRight)
                        target <- relocated
                        condition = Predicate("location", Some(target))
                        if (1 to optionLeft.size).forall(count =>
                          inspect(optionLeft.take(count)).exists(at => value(at, condition).value == value(reference, condition).value) &&
                          inspect(optionRight.take(count)).exists(at => value(at, condition).value == value(alternative, condition).value))
                        price <- endRight.view.legalRows.toList.flatten.filter(row => row.capture.flatMap(endRight.original).contains(target))
                        taker <- endRight.original(price.actor)
                        effect = Predicate("legal_capture", Some(taker), Some(target))
                        if value(endLeft, effect).value.contains("illegal") && value(endRight, effect).value.contains("legal")
                        paid <- functionalBoard(optionRight :+ price.move)
                      do emitFunction(condition, optionLeft, optionRight, effect, recapture, c,
                        List(challenge.sourceId, retreat.sourceId, capture.sourceId, recapture.sourceId, price.sourceId),
                        Some(price -> paid))
                    }
                  }
                inspectFunction(anchor, alternativeMove, reference, alternative, referencePrefix, alternativePrefix)
                inspectFunction(anchor.copy(referenceMove = alternativeMove), anchor.referenceMove,
                  alternative, reference, alternativePrefix, referencePrefix)
            }
            // An independently observed later action can name its immediate legal alternative.
            // Keep the intervening history: a difference here asks about these two paths,
            // not whether the preparatory move forces the plan against every reply.
            // A quiet move away from a newly created attack is a response, not
            // evidence that the root intended to prepare that destination.
            def quietReaction(goal: Action, line: List[String]): Option[Action] =
              val offset = line.size - 1
              if goal.capture.nonEmpty || goal.promotion.nonEmpty || offset <= ply + 1 then None
              else for
                before <- inspect(line.take(offset - 1))
                challenged <- inspect(line.init)
                escaped <- inspect(line)
                reply <- observedAction(line, offset - 1)
                if reply.actor.side != goal.actor.side && reply.capture.isEmpty && reply.promotion.isEmpty
                target <- location(challenged, goal.actor)
                after <- location(escaped, goal.actor)
                if target != after && location(before, goal.actor).contains(target)
                attack = Predicate("control", Some(reply.actor), square = Some(target))
                if value(before, attack).value.contains("absent") && value(challenged, attack).value.contains("present") &&
                  value(escaped, Predicate("control", Some(reply.actor), square = Some(after))).value.contains("absent")
              yield reply

            // A root move can occupy an ally's useful reply square. Test that
            // exact reply after the same observed attack in the other root arm.
            // The declared branch is a hypothesis: the fact reader must replay
            // it and the child comparison must establish the reply's value.
            if start.isEmpty then witnessedActionPaths.distinctBy(_._2).foreach { (retreat, line) =>
              for
                attack <- quietReaction(retreat, line).toList
                if line.size == 3 && line.head == packet.assembly.input.playedMoveUci &&
                  line.head != anchor.referenceMove
                first <- rows.find(_.move == line.head).toList
                blocker <- root.original(first.actor).toList
                if first.capture.isEmpty && first.promotionRole.isEmpty && blocker != retreat.actor &&
                  blocker.side == retreat.actor.side && retreat.actor.role != "king"
                challenged <- inspect(line.init).toList
                originalSquare <- location(challenged, retreat.actor).toList
                if location(root, retreat.actor).contains(originalSquare) &&
                  location(challenged, blocker).contains(first.to.key) &&
                  challenged.view.currentCheckSources(mover).isEmpty &&
                  challenged.view.currentCheckSources(!mover).isEmpty
                available <- rows.filter(row => root.original(row.actor).contains(retreat.actor) &&
                  row.to == first.to && row.capture.isEmpty && row.promotionRole.isEmpty)
                other <- rows.find(_.move == anchor.referenceMove).toList
                if other.capture.isEmpty && other.promotionRole.isEmpty
                otherBoard <- branches.get(other.move).toList
                if otherBoard.view.occupantAt(first.to).isEmpty &&
                  location(otherBoard, retreat.actor).contains(originalSquare) &&
                  otherBoard.view.currentCheckSources(!mover).isEmpty &&
                  otherBoard.view.legalRows.exists(_.exists(row => row.move == line(1) &&
                    otherBoard.original(row.actor).contains(attack.actor)))
              do
                val alternate = List(other.move, line(1))
                val rootPair = CompareMoves("constraint_root", Nil, line.head, other.move)
                val replyPair = CompareMoves("constraint_reply", alternate, available.move, line(2))
                val locationPredicate = Predicate("location", Some(blocker))
                val actionPredicate = Predicate("legal_move", Some(retreat.actor), square = Some(first.to.key))
                val facts = List[Requirement](rootPair,
                  Fact("constraint_before", declared("constraint_root_path", line), locationPredicate, Ply(0),
                    expected = Some(first.actor.square.key)),
                  Fact("constraint_after", declared("constraint_root_path", line), locationPredicate, Ply(1),
                    expected = Some(first.to.key)),
                  FactComparison("constraint_owned", "constraint_before", "constraint_after", "different"),
                  Fact("constraint_blocked", declared("constraint_actual", line.init), actionPredicate, Ply(2),
                    expected = Some("illegal")),
                  Fact("constraint_available", declared("constraint_alternative", alternate), actionPredicate, Ply(2),
                    expected = Some("legal")),
                  FactComparison("constraint_changed", "constraint_blocked", "constraint_available", "different"),
                  Fact("constraint_other_retreat", declared("constraint_alternative", alternate),
                    retreat.predicate, Ply(2), expected = Some("legal")),
                  Fact("constraint_attack", declared("constraint_actual", line.init),
                    Predicate("control", Some(attack.actor), square = Some(originalSquare)), Ply(2), expected = Some("present")),
                  Fact("constraint_current_attack", Path("constraint_current", Comparison(rootPair.id, "reference")),
                    Predicate("location", Some(attack.actor)), Event("move", 1, "after", 1, 2,
                      Some(attack.actor), square = Some(line(1).slice(2, 4))), expected = Some(line(1).slice(2, 4))),
                  Fact("constraint_current_retreat", Path("constraint_current", Comparison(rootPair.id, "reference")),
                    Predicate("location", Some(retreat.actor)), Event("move", 1, "after", 2, 3,
                      Some(retreat.actor), square = Some(retreat.to)), expected = Some(retreat.to)))
                val context = FactGroup("constraint_context", "all",
                  facts.collect { case fact: Fact => fact.id; case fact: FactComparison => fact.id })
                emit(Anchor(Nil, line.head), line ++ List(other.move, available.move), "reply-space",
                  retreat.sourceRefs ++ attack.sourceRefs, retreat.source, facts ++ List(context, replyPair),
                  List(context.id, rootPair.id, replyPair.id),
                  question = Some(s"Does ${notation.move(Nil, line.head)} occupy a useful reply square after ${notation.move(List(line.head), line(1))}? Test that reply after ${notation.line(alternate).mkString(" ")} before attributing value to the missing square."))
            }

            // A lost supporting line may impose a duty before an opponent
            // actually attacks the target. Compare postponing its observed
            // departure after the same reply in both root arms. This only
            // asks the question; loss of support alone does not prove danger.
            if start.isEmpty then witnessedActionPaths.filter((goal, line) =>
              line.size == 3 && line.head == packet.assembly.input.playedMoveUci &&
                line.head != anchor.referenceMove && goal.actor.role != "king" && goal.actor.role != "pawn" &&
                goal.capture.isEmpty && goal.promotion.isEmpty).distinctBy(_._2).foreach { (departure, line) =>
              val alternative = List(anchor.referenceMove, line(1))
              val rootPair = CompareMoves("duty_root", Nil, line.head, anchor.referenceMove)
              for
                first <- rows.find(row => row.move == line.head && row.capture.isEmpty && row.promotionRole.isEmpty).toList
                other <- rows.find(row => row.move == anchor.referenceMove && row.capture.isEmpty && row.promotionRole.isEmpty).toList
                actual <- inspect(line.init).toList
                lost <- inspect(line.take(1)).toList
                adjusted <- inspect(line).toList
                retained <- inspect(List(other.move)).toList
                target <- root.originals.values.toList.distinct.filter(piece => piece.side == departure.actor.side && piece.role != "king")
                originalSquare <- location(root, target).toList
                if location(actual, target).contains(originalSquare) &&
                  retained.view.legalRows.exists(_.exists(_.move == line(1))) &&
                  List(chess.White, chess.Black).forall(color => actual.view.currentCheckSources(color).isEmpty)
                provider <- root.originals.values.toList.distinct.filter(piece => piece.side == target.side && piece != target)
                support = Predicate("support", Some(provider), Some(target))
                if value(root, support).value.contains("present") &&
                  value(lost, support).value.contains("absent") &&
                  value(actual, support).value.contains("absent") && value(retained, support).value.contains("present")
                adjustment = Predicate("support", Some(departure.actor), Some(target))
                if departure.actor == target || value(actual, adjustment).value.contains("absent") &&
                  value(adjusted, adjustment).value.contains("present")
                (otherResponse, otherLine) <- witnessedActionPaths.filter((goal, moves) =>
                  moves.size == 3 && Set(line.head, other.move)(moves.head) && goal.capture.isEmpty && goal.promotion.isEmpty &&
                    board(alternative).forall(at => value(at, goal.predicate).value.contains("legal")))
                  .sortBy((_, moves) => (if moves.take(2) == alternative then 0 else if moves.head == other.move then 1 else 2,
                    moves.mkString(" "))).distinctBy(_._2.last).take(1)
                (plan, planLine) <- witnessedActionPaths.filter((goal, moves) =>
                  Set(line.head, other.move)(moves.head) && moves.size > 3 && goal.actor.role == "pawn" &&
                    goal.capture.isEmpty && goal.promotion.isEmpty && location(root, goal.actor) == location(actual, goal.actor) &&
                    actual.view.legalRows.exists(_.exists(row => row.move == moves.last && actual.original(row.actor).contains(goal.actor))))
                  .sortBy((_, moves) => (if moves.head == line.head then 0 else 1, moves.size, moves.mkString(" "))).take(1)
                if planLine.last != otherLine.last
              do
                val planArm = if planLine.head == line.head then "reference" else "alternative"
                val responseArm = if otherLine.head == line.head then "reference" else "alternative"
                val path = declared("duty_actual", line)
                val otherPath = declared("duty_alternative", alternative)
                val facts = List[Requirement](
                  Fact("duty_before", path, support, Ply(0), expected = Some("present")),
                  Fact("duty_after_root", path, support, Ply(1), expected = Some("absent")),
                  Fact("duty_actual_support", path, support, Ply(2), expected = Some("absent")),
                  Fact("duty_other_support", otherPath, support, Ply(2), expected = Some("present")),
                  Fact("duty_actual_plan", path, plan.predicate, Ply(2), expected = Some("legal")),
                  Fact("duty_other_plan", otherPath, plan.predicate, Ply(2), expected = Some("legal")),
                  Fact("duty_other_response", otherPath, otherResponse.predicate, Ply(2), expected = Some("legal")),
                  Fact("duty_current_reply", Path("duty_current_reply", Comparison(rootPair.id, "reference")),
                    Predicate("location", observedAction(line, 1).map(_.actor)), Event("move", 1, "after", 1, 2,
                      observedAction(line, 1).map(_.actor), square = Some(line(1).slice(2, 4))), expected = Some(line(1).slice(2, 4))),
                  Fact("duty_current", Path("duty_current", Comparison(rootPair.id, "reference")),
                    Predicate("location", Some(departure.actor)), Event("move", 1, "after", 2, 3,
                      Some(departure.actor), square = Some(departure.to)), expected = Some(departure.to)),
                  Fact("duty_adjustment", path,
                    if departure.actor == target then Predicate("location", Some(target)) else adjustment, Ply(3),
                    expected = Some(if departure.actor == target then departure.to else "present")),
                  // This observed move is a hypothesis after the common reply.
                  // Its original source can have a different opponent reply;
                  // the declared legal fact and child search certify the new use.
                  Fact("duty_other_current", Path("duty_other_current", Comparison(rootPair.id, responseArm)),
                    Predicate("location", Some(otherResponse.actor)), Event("move", 1, "after", 2, 3,
                      Some(otherResponse.actor), square = Some(otherResponse.to)), expected = Some(otherResponse.to)),
                  Fact("duty_other_reply", Path("duty_other_reply", Comparison(rootPair.id, responseArm)),
                    Predicate("location", observedAction(otherLine, 1).map(_.actor)), Event("move", 1, "after", 1, 2,
                      observedAction(otherLine, 1).map(_.actor), square = Some(otherLine(1).slice(2, 4))), expected = Some(otherLine(1).slice(2, 4))),
                  Fact("duty_observed_plan", Path("duty_observed_plan", Comparison(rootPair.id, planArm)),
                    Predicate("location", Some(plan.actor)), Event("move", 1, "after", planLine.size - 1, planLine.size,
                      Some(plan.actor), square = Some(plan.to)), expected = Some(plan.to)))
                val context = FactGroup("duty_context", "all", facts.map(_.id))
                val current = CompareMoves("duty_current_choice", line.init, planLine.last, line.last)
                val otherChoice = CompareMoves("duty_other_choice", alternative, planLine.last, otherLine.last)
                emit(Anchor(Nil, line.head), line ++ List(other.move, responseArm, provider.json.toString, target.json.toString) ++
                  otherLine ++ planLine,
                  "current-support-duty", departure.sourceRefs ++ plan.sourceRefs, canonical,
                  rootPair :: facts ++ List(context, current, otherChoice),
                  List(rootPair.id, context.id, current.id, otherChoice.id),
                  question = Some(s"Does the lost support make ${notation.move(line.init, line.last)} a current duty? Test the observed pawn plan after the same ${notation.move(List(line.head), line(1))} in both root choices."))
            }

            // An irreversible pawn move can spend an option that another quiet move
            // retains. Ask where that exact option is used before assigning it
            // a purpose. The root score is only a reason to investigate; this
            // observation by itself is never an explanation of the score.
            if start.isEmpty && root.originals.keys.forall(piece => Set("king", "pawn")(piece.role.name)) then
              val played = packet.assembly.input.playedMoveUci
              for
                pawnMove <- rows.find(row => row.move == played && row.actor.role.name == "pawn" &&
                  row.capture.isEmpty && row.promotionRole.isEmpty).toList
                pawn <- root.original(pawnMove.actor).toList
                other <- branches.keys.toList.sorted if other != played
                preservingMove <- rows.find(row => row.move == other && row.actor != pawnMove.actor &&
                  row.capture.isEmpty && row.promotionRole.isEmpty).toList
                kept <- branches.get(other).toList
                spent <- branches.get(played).toList
                if location(kept, pawn).contains(pawnMove.actor.square.key) &&
                  List(kept, spent).forall(at => at.view.currentCheckSources(!mover).isEmpty)
              do
                val choice = CompareMoves("reserve_root", Nil, played, other)
                val before = Fact("reserve_before", declared("reserve_spent", List(played)),
                  Predicate("location", Some(pawn)), Ply(0), expected = Some(pawnMove.actor.square.key))
                val after = before.copy(id = "reserve_after", at = Ply(1), expected = Some(pawnMove.to.key))
                val preserve = before.copy(id = "reserve_kept", path = declared("reserve_kept", List(other)), at = Ply(1))
                val use = Fact("reserve_use", Path("reserve_use", Comparison(choice.id, "alternative"), extend = true),
                  Predicate("location", Some(pawn)),
                  Event("move", 1, "after", 1, MaximumPly, Some(pawn), square = Some(pawnMove.to.key)),
                  expected = Some(pawnMove.to.key))
                val reserveContext = FactGroup("reserve_context", "all", List(before.id, after.id, preserve.id))
                emit(Anchor(Nil, played), List(other, pawn.json.toString), "reserve-use", Nil, canonical,
                  List(choice, before, after, preserve, use, reserveContext),
                  List(choice.id, before.id, after.id, preserve.id, use.id, reserveContext.id),
                  question = Some(s"Does ${notation.move(Nil, other)} retain a useful later ${notation.move(Nil, played)}? Locate the actual use before attributing value to the reserved pawn move."))

                def reserveSource(goal: Action, line: List[String], offset: Int, id: String) =
                  // A completed counter comparison can change the king's
                  // actual entry square. Its prior source still owns the
                  // position BEFORE this test; the new receipt owns the reply.
                  val carried = CommentaryIdea.reserveEntries(existing).filter(entry =>
                    entry.owner.comparison.referenceMove == played && entry.owner.comparison.alternativeMove == other &&
                      entry.test.prefixMovesUci == line.take(offset) &&
                      entry.testAnswer.exists(answer => goal.source.sourceRefs.contains(answer.workId) &&
                        List(answer.referenceLineId, answer.alternativeLineId).exists(id =>
                          paths.find(_.line.ref.id == id).exists(_.moves.startsWith(line)))))
                    .flatMap(entry => entry.source.map(source =>
                      (source.comparison.copy(id = id), entry.sourceArm, entry.ancestors)))
                  val owned = CommentaryIdea.reserveResponses(existing).filter(response =>
                    response.owner.comparison.referenceMove == played && response.owner.comparison.alternativeMove == other)
                    .flatMap(response => response.answer.map(answer =>
                      (response.test, answer, List.empty[(CommentaryIdea.AnsweredComparison, String)]))) ++
                    CommentaryIdea.reserveTimings(existing).filter(timing =>
                      timing.owner.comparison.referenceMove == played && timing.owner.comparison.alternativeMove == other)
                      .flatMap(timing => timing.answer.map(answer =>
                        (timing.test, answer, timing.source.toList.map(_ -> timing.sourceArm)))) ++
                    CommentaryIdea.reserveHolds(existing).filter(hold =>
                      hold.owner.comparison.referenceMove == played && hold.owner.comparison.alternativeMove == other)
                      .flatMap(hold => hold.testAnswer.map(answer =>
                        (hold.test, answer, hold.ancestors ++ hold.source.toList.map(_ -> hold.sourceArm))))
                  // A later comparison may supply this goal after an earlier
                  // comparable arm already owned the identical wait prefix.
                  // Keep the goal's origin; neither its later moves nor value
                  // become evidence from the earlier receipt.
                  // Source refs can name work or resources. The accepted
                  // source's concrete history, not that metadata namespace,
                  // identifies the current certified witness.
                  val witnesses = paths.filter(path => path.moves.startsWith(line) &&
                    boundedSources.exists(source => source.source == goal.source &&
                      source.moves.startsWith(line) && path.moves.startsWith(source.moves)))
                  // An answered direct inquiry keeps the source that owned
                  // this wait. Its returned child must own the entire current
                  // source history, not merely share a work id or prefix.
                  val directCarried = CommentaryIdea.directReserveEntries(existing).filter(entry =>
                    entry.owner.comparison.referenceMove == played && entry.owner.comparison.alternativeMove == other &&
                      entry.pawn == pawn && goal.actor == entry.pawn && entry.offset + 1 == offset &&
                      entry.branch.take(offset) == line.take(offset) &&
                      entry.testAnswer.exists(answer => goal.source.sourceRefs.contains(answer.workId) &&
                        List(answer.referenceLineId, answer.alternativeLineId).exists(lineId =>
                          paths.find(_.line.ref.id == lineId).exists(source => witnesses.exists(witness =>
                            (source eq witness) && sameReserveDecision(prepared, source, witness, offset,
                              List(entry.pawn.witness, entry.king.witness, entry.guard.witness)))))))
                  carried.headOption.orElse {
                    directCarried.headOption match
                      case Some(entry) => entry.source.map(source =>
                        (source.comparison.copy(id = id), entry.sourceArm, entry.ancestors))
                      case None => owned.sortBy(value => -value._1.prefixMovesUci.size).flatMap { (test, answer, ancestors) =>
                        List("reference" -> answer.referenceLineId, "alternative" -> answer.alternativeLineId).collect {
                          case (arm, lineId) if test.prefixMovesUci.size < offset &&
                              paths.find(_.line.ref.id == lineId).exists(source => witnesses.exists(witness =>
                                sameReserveDecision(prepared, source, witness, offset, List(pawn.witness, goal.actor.witness)))) &&
                              CommentaryIdea.comparableReserveArm(answer, arm) =>
                            (test.copy(id = id), arm, ancestors)
                        }
                      }.headOption
                  }
                def ancestorFacts(lineage: List[(CommentaryIdea.AnsweredComparison, String)], name: String) =
                  lineage.zipWithIndex.flatMap { case ((owner, arm), index) =>
                    val pair = owner.comparison.copy(id = s"${name}_ancestor_$index")
                    val selected = if arm == "reference" then pair.referenceMove else pair.alternativeMove
                    for
                      at <- inspect(pair.prefixMovesUci)
                      row <- at.view.legalRows.flatMap(_.find(_.move == selected))
                      actor <- at.original(row.actor)
                    yield pair -> Fact(pair.id + "_binding", Path(pair.id, Comparison(pair.id, arm)),
                      Predicate("location", Some(actor)), Ply(pair.prefixMovesUci.size + 1), expected = Some(row.to.key))
                  }
                def linkKey(test: CompareMoves, arm: String) =
                  test.prefixMovesUci.mkString(" ") + ":" + test.referenceMove + ":" + test.alternativeMove + ":" + arm

                // Before extending a cooperative king retreat far into the
                // future, test the pawn reply already observed against the
                // spent option in the retained arm too. Both replies remain
                // hypotheses until the same-position engine comparison.
                for
                  spentLine <- boundedSources.map(_.moves).filter(line => line.headOption.contains(played) && line.size >= 2)
                    .distinctBy(_.take(2))
                  spentReply <- observedAction(spentLine, 1).toList if spentReply.capture.isEmpty && spentReply.promotion.isEmpty
                  keptLine <- boundedSources.map(_.moves).filter(line => line.headOption.contains(other) && line.size >= 2)
                    .distinctBy(_.take(2))
                  keptReply <- observedAction(keptLine, 1).toList if keptLine(1) != spentLine(1) &&
                    keptReply.promotion.isEmpty
                  legal <- kept.view.legalRows.toList.flatten.filter(row => row.move == spentLine(1) &&
                    kept.original(row.actor).contains(spentReply.actor))
                do
                  val test = CompareMoves("reserve_response", List(other), keptLine(1), spentLine(1))
                  val observed = List(
                    Fact("reserve_spent_reply", Path("reserve_spent_reply", Comparison(choice.id, "reference")),
                      Predicate("location", Some(spentReply.actor)), Event("move", 1, "after", 1, 2,
                        Some(spentReply.actor), square = Some(spentReply.to)), expected = Some(spentReply.to)),
                    Fact("reserve_retained_reply", Path("reserve_retained_reply", Comparison(choice.id, "alternative")),
                      Predicate("location", Some(keptReply.actor)), Event("move", 1, "after", 1, 2,
                        Some(keptReply.actor), square = Some(keptReply.to)), expected = Some(keptReply.to)))
                  val context = FactGroup("reserve_response_context", "all", List(before.id, after.id, preserve.id) ++ observed.map(_.id))
                  emit(Anchor(Nil, played), List(other, spentLine(1), keptLine(1)), "reserve-response",
                    spentReply.sourceRefs ++ keptReply.sourceRefs, canonical,
                    List(choice, before, after, preserve) ++ observed ++ List(context, test),
                    List(choice.id, context.id, test.id),
                    question = Some(s"Does the observed reply ${notation.move(List(played), spentLine(1))} also work after ${notation.move(Nil, other)}, where the pawn move is retained? Compare it with the recorded reply before extending that reply's future plan."))

                // Locate the first decision to keep the option, before looking
                // for its use in a distant winning PV. The opponent's reply
                // must already occur against the spent root move as well.
                witnessedActionPaths.filter((goal, line) => line.size == 3 && line.head == other &&
                  goal.actor.role == "king" && goal.capture.isEmpty && goal.promotion.isEmpty &&
                  boundedSources.exists(source => source.moves.take(2) == List(played, line(1))))
                  .distinctBy(_._2).foreach { (step, line) =>
                  val source = reserveSource(step, line, 2, "timing_source")
                  val binding = source.fold(Comparison(choice.id, "alternative"))((test, arm, _) => Comparison(test.id, arm))
                  for
                    at <- inspect(line.init).toList
                    if source.forall(_._3.isEmpty) && location(at, pawn).contains(pawnMove.actor.square.key) &&
                      List(chess.White, chess.Black).forall(color => at.view.currentCheckSources(color).isEmpty)
                    legal <- at.view.legalRows.toList.flatten.filter(row => row.move == played &&
                      at.original(row.actor).contains(pawn) && row.capture.isEmpty && row.promotionRole.isEmpty)
                  do
                    val path = declared("timing_position", line)
                    val facts = List(before, after, preserve,
                      Fact("timing_available", path, Predicate("legal_move", Some(pawn), square = Some(pawnMove.to.key)),
                        Ply(2), expected = Some("legal")),
                      Fact("timing_kept", path, Predicate("location", Some(pawn)), Ply(2), Some(Ply(3)),
                        Some(pawnMove.actor.square.key)),
                      Fact("timing_current", Path("timing_current", binding), Predicate("location", Some(step.actor)),
                        Event("move", 1, "after", 2, 3, Some(step.actor), square = Some(step.to)), expected = Some(step.to)),
                      Fact("timing_common_reply", Path("timing_common_reply", Comparison(choice.id, "reference")),
                        Predicate("location", observedAction(line, 1).map(_.actor)),
                        Event("move", 1, "after", 1, 2, observedAction(line, 1).map(_.actor), square = Some(line(1).slice(2, 4))),
                        expected = Some(line(1).slice(2, 4))))
                    val context = FactGroup("timing_context", "all", facts.map(_.id))
                    val test = CompareMoves("reserve_timing", line.init, played, line.last)
                    emit(Anchor(Nil, played), line ++ source.toList.map((test, arm, _) => linkKey(test, arm)),
                      "reserve-timing", step.sourceRefs ++ step.source.sourceRefs, canonical,
                      choice :: source.toList.map(_._1) ++ facts ++ List(context, test),
                      List(choice.id, context.id, test.id),
                      question = Some(s"After ${notation.line(line.init).mkString(" ")}, should the retained ${notation.move(Nil, played)} be used now? Compare it with the recorded king move before investigating its later use."))
                }

                // A witnessed use can be a sequence of pawn waits: the king
                // stays on its useful square while the opponent spends a pawn
                // move, then yields an entry. Test a legal pawn reply which
                // keeps guarding that entry; a lone cooperative PV is not enough.
                witnessedActionPaths.filter((goal, line) => goal.actor.role == "king" && line.head == other)
                  .distinctBy(_._2).foreach { (entry, line) =>
                  val last = line.size - 3
                  val first = line.indexOf(played, 1)
                  val responseSource = reserveSource(entry, line, last + 1, "reserve_source")
                  val lineage = responseSource.toList.flatMap(_._3)
                  val ancestors = ancestorFacts(lineage, "reserve")
                  val currentSource = responseSource.fold(Comparison(choice.id, "alternative"))((source, arm, _) =>
                    Comparison(source.id, arm))
                  if first > 0 && first <= last && (last - first) % 2 == 0 &&
                      ancestors.size == lineage.size && lineage.size <= 2 then
                    for
                      initial <- inspect(line.take(first)).toList
                      waiting <- inspect(line.take(last + 1)).toList
                      yielded <- inspect(line.init).toList
                      square <- location(initial, entry.actor).toList
                      guard <- initial.originals.values.filter(p => p.role == "king" && p.side != pawn.side).toList
                      guardSquare <- location(initial, guard).toList
                      if initial.view.occupantAt(EvidenceSquare(entry.to)).isEmpty &&
                        (first to last + 1).forall(count => inspect(line.take(count)).exists(at =>
                          location(at, entry.actor).contains(square) && location(at, guard).contains(guardSquare) &&
                            value(at, Predicate("control", Some(guard), square = Some(entry.to))).value.contains("present"))) &&
                        (first to last by 2).forall(offset => observedAction(line, offset).exists(step =>
                          step.actor == pawn && step.capture.isEmpty && step.promotion.isEmpty) &&
                          inspect(line.take(offset + 1)).exists(_.view.currentCheckSources(!mover).isEmpty)) &&
                        observedAction(line, last + 1).exists(step => step.actor == guard && step.capture.isEmpty) &&
                        value(yielded, Predicate("control", Some(guard), square = Some(entry.to))).value.contains("absent")
                      counter <- waiting.view.legalRows.toList.flatten.filter(row => row.actor.role.name == "pawn" &&
                        row.promotionRole.isEmpty).sortBy(_.move)
                    do
                      val path = declared("reserve_sequence", line)
                      val heldKing = Predicate("location", Some(entry.actor))
                      val guards = Predicate("control", Some(guard), square = Some(entry.to))
                      val enters = Predicate("legal_move", Some(entry.actor), square = Some(entry.to))
                      val counterPath = declared("reserve_counter", line.take(last + 1) :+ counter.move)
                      val sequence = List[Requirement](before, after, preserve,
                        Fact("reserve_start", path, Predicate("location", Some(pawn)), Ply(first),
                          expected = Some(pawnMove.actor.square.key)),
                        Fact("reserve_finish", path, Predicate("location", Some(pawn)), Ply(last + 1),
                          expected = Some(line(last).slice(2, 4))),
                        Fact("reserve_king", path, heldKing, Ply(first), Some(Ply(last + 1)), Some(square)),
                        Fact("reserve_guard", path, guards, Ply(first), Some(Ply(last + 1)), Some("present")),
                        Fact("reserve_yielded", path, guards, Ply(last + 2), expected = Some("absent")),
                        Fact("reserve_entry", path, enters, Ply(last + 2), expected = Some("legal")),
                        Fact("reserve_counter_guard", counterPath, guards, Ply(last + 2), expected = Some("present")),
                        Fact("reserve_counter_entry", counterPath, enters, Ply(last + 2), expected = Some("illegal")),
                        Fact("reserve_current", Path("reserve_current", currentSource, extend = true),
                          Predicate("location", Some(pawn)), Event("move", 1, "after", last, last + 1,
                            Some(pawn), square = Some(line(last).slice(2, 4))), expected = Some(line(last).slice(2, 4))),
                        // The counter's completed line must realize this
                        // entry. Its earlier parent need only own the wait;
                        // a newly tested reply may choose a different square.
                        Fact("reserve_current_entry", path,
                          heldKing, Event("move", 1, "after", last + 2, last + 3,
                            Some(entry.actor), square = Some(entry.to)), expected = Some(entry.to)))
                      val context = FactGroup("reserve_context", "all", sequence.map(_.id) ++ ancestors.map(_._2.id))
                      val test = CompareMoves("reserve_counter", line.take(last + 1), line(last + 1), counter.move)
                      emit(Anchor(Nil, played), line ++ List(counter.move) ++ responseSource.toList.map((source, arm, _) =>
                          linkKey(source, arm)) ++ lineage.map((owner, arm) => linkKey(owner.comparison, arm)), "reserve-entry", entry.sourceRefs, entry.source,
                        choice :: responseSource.toList.map(_._1) ++ ancestors.flatMap((pair, fact) => List(pair, fact)) ++
                          sequence ++ List(context, test), List(choice.id, context.id, test.id),
                        question = Some(s"Does the retained pawn move let the king hold $square and later enter ${entry.to}? Compare the recorded king retreat with the legal pawn reply ${notation.move(test.prefixMovesUci, counter.move)} which keeps that entry guarded."))
                }

                // Counter resources can already be spent. Reuse the same
                // entry question, but explicitly test a last-wait decision
                // instead of manufacturing a guarding pawn counter. The
                // entry's value is a separate inquiry even off the parent PV.
                witnessedActionPaths.filter((goal, line) => goal.actor == pawn && line.head == other &&
                  line.last == played && goal.capture.isEmpty && goal.promotion.isEmpty)
                  .distinctBy(_._2).foreach { (wait, line) =>
                  val offset = line.size - 1
                  val source = reserveSource(wait, line, offset + 1, "reserve_source")
                  val binding = source.fold(Comparison(choice.id, "alternative"))((test, arm, _) => Comparison(test.id, arm))
                  val lineage = source.toList.flatMap(_._3)
                  val ancestors = ancestorFacts(lineage, "reserve")
                  val parent = boundedSources.filter(current => current.moves.startsWith(line) && current.moves.size > offset + 2)
                    .sortBy(current => (if current.source == wait.source then 0 else 1,
                      current.choiceRank.filter(_._1 == line).map(_._2).getOrElse(Int.MaxValue), current.moves.mkString(" ")))
                    .headOption
                  for
                    current <- parent.toList
                    path <- paths.filter(_.moves.startsWith(current.moves.take(offset + 3))).take(1)
                    beforeWait <- inspect(line.init).toList
                    king <- beforeWait.originals.values.find(p => p.role == "king" && p.side == pawn.side).toList
                    guard <- beforeWait.originals.values.find(p => p.role == "king" && p.side != pawn.side).toList
                    kingSquare <- location(beforeWait, king).toList
                    release <- RootSubjectComparison.lastPawnWaitEntry(prepared, path, offset,
                      pawn.witness, king.witness, guard.witness).toList
                    (square, releaseRefs) = release
                    if ancestors.size == lineage.size && lineage.size <= 2 &&
                      location(beforeWait, pawn).contains(pawnMove.actor.square.key)
                    child <- inspect(current.moves.take(offset + 2)).toList
                    entry <- child.view.legalRows.toList.flatten.filter(row =>
                      child.original(row.actor).contains(king) && row.to.key == square && row.capture.isEmpty && row.promotionRole.isEmpty)
                    next = current.moves(offset + 2)
                    legalChoices = child.view.legalRows.toList.flatten.filter(_.move != entry.move)
                    childPrefix = current.moves.take(offset + 2)
                    returnedChoices = legalChoices.flatMap(row => boundedSources.filter(source =>
                      source.moves.startsWith(childPrefix :+ row.move)).flatMap(_.choiceRank.collect {
                        case (prefix, rank) if prefix == childPrefix => rank
                      }).minOption.map(rank => (rank, row))).sortBy((rank, row) => (rank, row.move))
                    // The entry can already be the parent PV move. Its value
                    // still needs a separate owned comparison; a quiet legal
                    // king alternative is only a hypothesis until that returns.
                    otherChoice <- (if next != entry.move then legalChoices.find(_.move == next)
                      else returnedChoices.headOption.map(_._2).orElse(legalChoices.filter(row =>
                        child.original(row.actor).contains(king) && row.capture.isEmpty && row.promotionRole.isEmpty)
                        .sortBy(_.move).headOption)).toList
                  do
                    val branch = current.moves.take(offset + 2) :+ entry.move
                    // The wait/reply conditions end before the optional
                    // forecast. Its separately owned entry/value stays below.
                    val path = declared("direct_sequence", current.moves.take(offset + 2))
                    val entered = declared("direct_entry", branch)
                    val heldKing = Predicate("location", Some(king))
                    val guards = Predicate("control", Some(guard), square = Some(square))
                    val facts = List[Requirement](before, after, preserve,
                      Fact("direct_wait", path, Predicate("location", Some(pawn)), Ply(offset + 1),
                        expected = Some(pawnMove.to.key)),
                      Fact("direct_king", path, heldKing, Ply(offset), Some(Ply(offset + 1)), Some(kingSquare)),
                      Fact("direct_guard", path, guards, Ply(offset), Some(Ply(offset + 1)), Some("present")),
                      Fact("direct_released", path, guards, Ply(offset + 2), expected = Some("absent")),
                      Fact("direct_option", path, Predicate("legal_move", Some(king), square = Some(square)),
                        Ply(offset + 2), expected = Some("legal")),
                      Fact("direct_live", Path("direct_live", binding, extend = true), Predicate("location", Some(pawn)),
                        Event("move", 1, "after", offset, offset + 1, Some(pawn), square = Some(pawnMove.to.key)),
                        expected = Some(pawnMove.to.key)))
                    // The current board can ask for this option before the
                    // value report supplies its actual move occurrence.
                    val entryFact = Fact("direct_entry", entered, heldKing, Ply(branch.size), expected = Some(square))
                    val context = FactGroup("direct_context", "all", facts.map(_.id) ++ ancestors.map(_._2.id))
                    val test = CompareMoves("reserve_entry_value", branch.init, entry.move, otherChoice.move)
                    emit(Anchor(Nil, played), List("direct_last_wait") ++ branch ++
                        source.toList.map((test, arm, _) => linkKey(test, arm)) ++ lineage.map((owner, arm) => linkKey(owner.comparison, arm)),
                      "reserve-entry", (wait.sourceRefs ++ releaseRefs.filter(_.length <= MaximumSourceRefLength)).distinct, wait.source,
                      choice :: source.toList.map(_._1) ++ ancestors.flatMap((pair, fact) => List(pair, fact)) ++
                        facts ++ List(context, test, entryFact), List(choice.id, context.id, test.id),
                      question = Some(s"After the retained pawn move and the recorded king reply, is the available ${notation.move(test.prefixMovesUci, test.referenceMove)} a comparable option alongside ${notation.move(test.prefixMovesUci, test.alternativeMove)}? All legal replies release $square; no guarding pawn counter is assumed."))
                }

                // A retained wait can also defend: compare it with a legal
                // king move in the very same position. Neither the root gap
                // nor keeping the king still establishes that value; the
                // child comparison and the opponent's actual entry do.
                witnessedActionPaths.filter((goal, line) => goal.actor == pawn && line.head == other &&
                  line.last == played && goal.capture.isEmpty && goal.promotion.isEmpty)
                  .distinctBy(_._2).foreach { (wait, line) =>
                  val offset = line.size - 1
                  val source = reserveSource(wait, line, offset, "hold_source")
                  val binding = source.fold(Comparison(choice.id, "alternative"))((test, arm, _) => Comparison(test.id, arm))
                  val lineage = source.toList.flatMap(_._3)
                  val ancestors = ancestorFacts(lineage, "hold")
                  for
                    beforeWait <- inspect(line.init).toList
                    afterWait <- inspect(line).toList
                    king <- beforeWait.originals.values.find(p => p.role == "king" && p.side == pawn.side).toList
                    kingSquare <- location(beforeWait, king).toList
                    if ancestors.size == lineage.size && lineage.size <= 2 &&
                      location(beforeWait, pawn).contains(pawnMove.actor.square.key) &&
                      location(afterWait, king).contains(kingSquare) && afterWait.view.currentCheckSources(!mover).isEmpty
                    // The best returned king move at this exact decision is
                    // the challenger. A king move elsewhere in the PV cannot
                    // make a weaker retreat look like the relevant alternative.
                    retreat <- beforeWait.view.legalRows.toList.flatten.filter(row =>
                      beforeWait.original(row.actor).contains(king) && row.capture.isEmpty && row.promotionRole.isEmpty)
                      .sortBy(row => (boundedSources.filter(source => source.moves.startsWith(line.init :+ row.move))
                        .flatMap(_.choiceRank.collect { case (prefix, rank) if prefix == line.init => rank }).minOption
                        .getOrElse(Int.MaxValue), row.move)).take(1)
                  do
                    val path = declared("hold_sequence", line)
                    val facts = List(before, after, preserve,
                      Fact("hold_start", path, Predicate("location", Some(pawn)), Ply(offset), expected = Some(pawnMove.actor.square.key)),
                      Fact("hold_use", path, Predicate("location", Some(pawn)), Ply(offset + 1), expected = Some(pawnMove.to.key)),
                      Fact("hold_king", path, Predicate("location", Some(king)), Ply(offset), Some(Ply(offset + 1)), Some(kingSquare)),
                      Fact("hold_current", Path("hold_current", binding, extend = true), Predicate("location", Some(pawn)),
                        Event("move", 1, "after", offset, offset + 1, Some(pawn), square = Some(pawnMove.to.key)), expected = Some(pawnMove.to.key)))
                    val context = FactGroup("hold_context", "all", facts.map(_.id) ++ ancestors.map(_._2.id))
                    val test = CompareMoves("reserve_hold", line.init, retreat.move, played)
                    emit(Anchor(Nil, played), line ++ List(retreat.move) ++ source.toList.map((test, arm, _) => linkKey(test, arm)) ++
                        lineage.map((owner, arm) => linkKey(owner.comparison, arm)),
                      "reserve-hold", wait.sourceRefs ++ wait.source.sourceRefs, canonical,
                      choice :: source.toList.map(_._1) ++ ancestors.flatMap((pair, fact) => List(pair, fact)) ++
                        facts ++ List(context, test), List(choice.id, context.id, test.id),
                      question = Some(s"Does the retained pawn move keep the king's useful position on $kingSquare? Compare the wait with ${notation.move(line.init, retreat.move)} in that same position and inspect the opponent's actual reply."))
                }

            val preparations = witnessedActionPaths.filter((goal, line) => quietReaction(goal, line).isEmpty)
              .groupBy(_._2).toList.sortBy(_._1.mkString(" "))
              .map((prefix, witnesses) => joinedActions(witnesses.map(_._1)) -> prefix)
            val previousActions = existing.filter(entry => entry.admission == "generated" &&
              entry.generatedArms.exists(_.prefixMoves == start)).flatMap(entry =>
              entry.request.requirements.collect { case fact: Fact if fact.id == "reference_action" => fact.path.source }
                .collect { case Declared(moves) => moves -> entry }).groupMap(_._1)(_._2)
            def settled(prefix: List[String]): Boolean = previousActions.get(prefix).exists(entries =>
              entries.nonEmpty && entries.forall(entry => entry.generatedArms.forall(arm =>
                arm.headRequirementIds.forall(id => entry.readings.find(_.requirementId == id).exists(isResolved)))))
            // Observe all supplied actions cheaply, but spend new replay work on the earliest
            // unanswered action for each preparation. Later continuations must not consume
            // the shared budget before the first concrete question can be investigated.
            val frontiers = preparations.filterNot((_, prefix) => settled(prefix)).groupBy(_._2(ply))
              .view.mapValues(_.map(_._2.size).min).toMap
            def sameAction(left: Action, right: Action): Boolean =
              left.actor == right.actor && left.to == right.to && left.capture == right.capture && left.promotion == right.promotion
            def goalName(goal: Action): String =
              s"the ${goal.actor.side} ${goal.actor.role} originally on ${goal.actor.square} acting on ${goal.to}"
            def returnedActions(comparisons: List[CompareMoves], goal: Action): List[Fact] =
              comparisons.flatMap(compare => List("reference", "alternative").map { arm =>
                Fact(s"returned_${compare.id}_$arm", Path(s"returned_${compare.id}_$arm", Comparison(compare.id, arm)),
                  Predicate("presence", Some(goal.actor)), goal.event(ply, "after"))
              })
            // This is only a cheap relevance filter. Exact replay and legal inventories
            // subsequently decide whether the move really changes or stops the action.
            def mayControl(actor: RelationColoredPieceWitness, from: String, to: String): Boolean =
              val dx = math.abs(to(0) - from(0))
              val dy = math.abs(to(1) - from(1))
              from != to && (actor.role.name match
                case "pawn" => dx == 1 && to(1) - from(1) == (if actor.side.white then 1 else -1)
                case "knight" => dx * dy == 2
                case "king" => math.max(dx, dy) == 1
                case "bishop" => dx == dy
                case "rook" => dx == 0 || dy == 0
                case "queen" => dx == 0 || dy == 0 || dx == dy
                case _ => false)
            def touches(row: LegalMoveResource, at: Board, squares: List[String]): Boolean =
              squares.exists(square => row.to.key == square || mayControl(row.actor, row.to.key, square) ||
                mayControl(row.actor, row.actor.square.key, square) ||
                at.originals.keys.exists(actor => mayControl(actor, actor.square.key, square) &&
                  (between(actor.square.key, square, row.to.key) || between(actor.square.key, square, row.actor.square.key))))
            // Each difference is owned by one actual transition. Reading only the endpoints
            // would silently credit the preparation for an intervening opponent's move.
            def trace(prefix: List[String], predicates: List[Predicate]): List[Requirement] =
              predicates.distinct.sortBy(_.json.toString).zipWithIndex.flatMap { (predicate, index) =>
                (ply until prefix.size).toList.flatMap { offset =>
                  for
                    before <- inspect(prefix.take(offset)).toList
                    after <- inspect(prefix.take(offset + 1)).toList
                    left <- value(before, predicate).value.toList
                    right <- value(after, predicate).value.toList if left != right
                    if !Set("not_turn", "actor_removed", "target_removed")(left) &&
                      !Set("not_turn", "actor_removed", "target_removed")(right)
                  yield
                    val id = s"effect_${index}_$offset"
                    List[Requirement](
                      Fact(id + "_before", declared("reference_effect", prefix), predicate, Ply(offset)),
                      Fact(id + "_after", declared("reference_effect", prefix), predicate, Ply(offset + 1)),
                      FactComparison(id, id + "_before", id + "_after", "different"))
                }.flatten
              }
            def planCounters(goal: Action, actionMoves: List[String], kingSquares: List[String],
                requested: List[String] => Option[Board], includeCaptures: Boolean = false): List[(String, List[Requirement])] =
              // Probe quiet alternatives to the last observed opponent reply. At the next
              // actual own turn legality includes pins and king safety, not just empty squares.
              val beforeAction = actionMoves.init
              val ready = inspect(beforeAction).get
              val counterPrefix = beforeAction.init
              val observedReply = beforeAction.last
              inspect(counterPrefix).toList.flatMap { counterBoard =>
                val quietReplies = counterBoard.view.legalRows.toList.flatten.filter(row => row.move != observedReply &&
                  (includeCaptures || row.capture.isEmpty) && row.promotionRole.isEmpty)
                def relevant(row: LegalMoveResource): Boolean =
                  counterBoard.original(row.actor).exists(goal.capture.contains) ||
                    touches(row, counterBoard, goal.to :: kingSquares) ||
                    counterBoard.originals.exists((current, original) => original == goal.actor &&
                      between(current.square.key, goal.to, row.to.key))
                // A quiet move can refute the plan by leaving an existing obstruction in
                // place, when the observed reply was the move that removed it. One neutral
                // representative asks this persistence question without expanding every wait.
                val unchanged = quietReplies.filterNot(relevant).minByOption(row =>
                  (if sourcePrefixes(counterPrefix :+ row.move) then 0 else 1, row.move)).map(_.move)
                quietReplies.filter(row => relevant(row) || unchanged.contains(row.move)).flatMap { row =>
                  requested(counterPrefix :+ row.move).toList.flatMap { blocked =>
                    val probes = goal.predicate :: goal.capture.toList.map(target => Predicate("location", Some(target)))
                    val defense = controls(blocked).collect {
                      case (actor, square, _) if actor.side != goal.actor.side && square.key == goal.to =>
                        Predicate("control", Some(actor), square = Some(goal.to))
                    }.filter(predicate => value(ready, predicate).value.contains("absent"))
                    val differences = (probes.filter(predicate => value(ready, predicate).value.nonEmpty &&
                      value(blocked, predicate).value.nonEmpty && value(ready, predicate).value != value(blocked, predicate).value) ++ defense)
                      .distinct.sortBy(_.json.toString)
                    if differences.nonEmpty then
                      val conditions = differences.zipWithIndex.flatMap { (predicate, index) => List[Requirement](
                        Fact(s"reference_condition_$index", declared("reference", beforeAction), predicate, Ply(beforeAction.size)),
                        Fact(s"alternative_condition_$index", declared("alternative", counterPrefix :+ row.move), predicate, Ply(beforeAction.size)),
                        FactComparison(s"contrast_condition_$index", s"reference_condition_$index", s"alternative_condition_$index", "different")) }
                      val facts = List[Requirement](
                        Fact("reference_action", declared("reference", actionMoves), goal.predicate,
                          Ply(beforeAction.size), expected = Some("legal"))) ++ conditions ++ List(
                        FactGroup("contrast_context", "all", "reference_action" :: conditions.collect { case fact: FactComparison => fact.id }),
                        CompareMoves("contrast_strength", counterPrefix, observedReply, row.move, Some("better")))
                      List(row.move -> facts)
                    else Nil
                  }
                }
              }
            // A material deficit asks for compensation, not for a voluntary-sacrifice label.
            // Unobserved acceptance remains owned by TargetResponseInvestigation; its returned
            // line can then supply this question without a duplicate speculative capture scan.
            if start.isEmpty then
              val sacrificeSources = boundedSources.filter(source => source.moves.headOption.exists(investigativeRootMoves))
                .groupBy(_.moves).toList.sortBy((moves, _) => (moves.size, moves.mkString(" ")))
              sacrificeSources.foreach { (line, witnesses) =>
                inspect(line).foreach { end =>
                  val events = end.path.flatMap(_.certified.flatMap(_._2.material.map(_.events)))
                    .orElse(probes.find(_.moves.startsWith(line)).map(_.materialEvents))
                    .toList.flatten.filter(_.plyOffset < line.size)
                  List(mover, !mover).foreach { investor =>
                    val investorSide = if investor.white then "white" else "black"
                    val byPly = events.map(event => event.plyOffset -> event).toMap
                    // Include the exact preceding gain even when recapture is delayed. Counting
                    // only the return capture would turn an unfinished pawn exchange into a sacrifice.
                    val priorGain = packet.assembly.input.positionHistory.segmentReplaySteps.lastOption
                      .filter(_.move.piece.color == investor).flatMap(_.capturedRole)
                      .map(lila.chessjudgment.analysis.material.MaterialValue.materialValueCp).getOrElse(0)
                    val balances = line.indices.scanLeft(priorGain) { (balance, offset) =>
                      balance + byPly.get(offset).map { event =>
                        val change = event.capture.map(_.valueCp).getOrElse(0) + event.promotionGainCp
                        if event.movement.side == investor then change else -change
                      }.getOrElse(0)
                    }.tail
                    // An ordinary exchange recovered on the very next own move is not
                    // a deferred compensation question. At a short line end it stays open.
                    val firstLoss = events.find { event =>
                      val nextOwnTurn = line.indices.drop(event.plyOffset + 1).find(offset =>
                        (if offset % 2 == 0 then mover else !mover) == investor)
                      balances(event.plyOffset) < 0 && event.capture.exists(_.side != investor) &&
                        nextOwnTurn.fold(balances.last < 0)(offset => balances(offset) < 0)
                    }
                    firstLoss.foreach { loss =>
                      val lossPrefix = line.take(loss.plyOffset)
                      val acceptedPrefix = lossPrefix :+ loss.moveUci
                      for
                        beforeLoss <- inspect(lossPrefix)
                        lossRow <- beforeLoss.view.legalRows.flatMap(_.find(_.move == loss.moveUci))
                        victim <- lossRow.capture.flatMap(beforeLoss.original)
                      do
                        val origin = Source("generator", "C-sacrifice-observations", Some(GeneratorVersion),
                          witnesses.flatMap(_.source.sourceRefs).distinct.sorted)
                        val observations = scala.collection.mutable.ListBuffer.empty[Fact]
                        line.zipWithIndex.foreach { (move, offset) =>
                          for
                            at <- inspect(line.take(offset))
                            row <- at.view.legalRows.flatMap(_.find(_.move == move))
                            actor <- at.original(row.actor)
                          do
                            byPly.get(offset).flatMap(_.capture).foreach { capture =>
                              row.capture.flatMap(at.original).foreach { target =>
                                val kind = if capture.side == investor then "gain" else "loss"
                                val point = Event("capture", 1, "after", offset, offset + 1, Some(actor), Some(target))
                                val path = declared("sacrifice", line)
                                observations += Fact(s"sacrifice_${kind}_${offset}_role", path, Predicate("role", Some(target)),
                                  point.copy(phase = "before"), expected = Some(capture.capturedRole.name))
                                observations += Fact(s"sacrifice_${kind}_$offset", path, Predicate("presence", Some(target)),
                                  point, expected = Some("removed"))
                              }
                            }
                            if actor.side == investorSide && row.promotionRole.nonEmpty then
                              observations += Fact(s"sacrifice_promotion_$offset", declared("sacrifice", line),
                                Predicate("role", Some(actor)), Event("promotion", 1, "after", offset, offset + 1, Some(actor)),
                                expected = row.promotionRole)
                            if offset > loss.plyOffset && actor.side == investorSide then
                              inspect(line.take(offset + 1)).foreach { after =>
                                if value(after, Predicate("check", side = Some(if investor.white then "black" else "white")))
                                    .value.contains("present") then
                                  observations += Fact(s"sacrifice_check_$offset", declared("sacrifice", line),
                                    Predicate("check", side = Some(if investor.white then "black" else "white")),
                                    Event("check", 1, "after", offset, offset + 1, Some(actor)), expected = Some("present"))
                              }
                        }
                        // Opening a file is observable positional compensation to investigate,
                        // not a numeric substitute for the pieces lost.
                        ('a' to 'h').foreach { file =>
                          val predicate = Predicate("file_occupancy", property = Some(file.toString))
                          if !value(root, predicate).value.contains("open") && value(end, predicate).value.contains("open") then
                            observations += Fact(s"sacrifice_file_$file", declared("sacrifice", line), predicate,
                              Ply(line.size), expected = Some("open"))
                        }
                        val facts = observations.toList
                        val rootMove = line.head
                        val rootAlternatives = sources.flatMap(_.moves.headOption).distinct.filter(_ != rootMove)
                          .sortBy(move => (if move == anchor.referenceMove || move == packet.assembly.input.playedMoveUci then 0 else 1, move))
                        val rootComparison = rootAlternatives.headOption.map(move =>
                          CompareMoves("sacrifice_value", Nil, rootMove, move))
                        val replyRows = beforeLoss.view.legalRows.toList.flatten.filter(_.move != loss.moveUci)
                        val declines = replyRows.filterNot(_.capture.flatMap(beforeLoss.original).contains(victim))
                        val otherCaptures = replyRows.filter(_.capture.flatMap(beforeLoss.original).contains(victim))
                        val laterActions = line.zipWithIndex.drop(loss.plyOffset + 1).flatMap { (move, offset) =>
                          inspect(line.take(offset)).flatMap(at => at.view.legalRows.flatMap(_.find(_.move == move))
                            .flatMap(row => action(at, row, origin)))
                        }
                        def representative(rows: List[LegalMoveResource]): Option[String] = rows.minByOption(row =>
                          (if sourcePrefixes(lossPrefix :+ row.move) then 0 else 1,
                            action(beforeLoss, row, origin).map(candidate => laterActions.indexWhere(sameAction(candidate, _)))
                              .filter(_ >= 0).getOrElse(Int.MaxValue), row.move)).map(_.move)
                        val declineMove = representative(declines)
                        val replyComparisons = (declineMove.map(move =>
                          CompareMoves("sacrifice_decline", lossPrefix, loss.moveUci, move)) ++
                          representative(otherCaptures).map(move =>
                            CompareMoves("sacrifice_other_capture", lossPrefix, loss.moveUci, move))).toList.map { compare =>
                          rootComparison.filter(root => root.prefixMovesUci == compare.prefixMovesUci &&
                            root.referenceMove == compare.referenceMove && root.alternativeMove == compare.alternativeMove)
                            .getOrElse(compare)
                        }
                        val comparisons = (rootComparison.toList ++ replyComparisons).distinctBy(_.id)
                        // A historical material story is a selected explanation only while
                        // the current root answer still contains its exact named observations.
                        val current = rootComparison.toList.flatMap { compare => facts.map(fact =>
                          fact.copy(id = "current_" + fact.id,
                            path = Path("current_sacrifice", Comparison(compare.id, "reference"))))
                        }
                        val returnedReplies = replyComparisons.flatMap { compare =>
                          facts.map(fact => fact.copy(id = s"returned_${compare.id}_${fact.id}",
                            path = Path(s"returned_${compare.id}", Comparison(compare.id, "reference")))) ++
                            Option.when(declineMove.contains(compare.alternativeMove))(Fact("declined_loss",
                              Path("declined_loss", Comparison(compare.id, "alternative")),
                              Predicate("presence", Some(victim)), Ply(loss.plyOffset + 1), expected = Some("present")))
                        }
                        val replyGroups = replyComparisons.map(compare => FactGroup(s"sacrifice_reply_${compare.id}", "all",
                          returnedReplies.filter(_.id.startsWith(s"returned_${compare.id}_")).map(_.id)))
                        val groups = List(FactGroup("sacrifice_observed", "all", facts.map(_.id))) ++
                          Option.when(current.nonEmpty)(FactGroup("sacrifice_current", "all", current.map(_.id)))
                        // A reply's continuation is a separate decision from its pair score.
                        // It cannot block an answered root comparison, or outlive its current story.
                        val replyContexts = replyGroups.map(group => FactGroup(group.id + "_context", "all", groups.map(_.id) :+ group.id))
                        val hasReturn = facts.exists(fact => !fact.id.startsWith("sacrifice_loss_") && (fact.at match
                          case event: Event => event.fromPly > loss.plyOffset
                          case _ => !value(beforeLoss, fact.predicate).value.contains("open")))
                        val question = s"After ${notation.move(Nil, rootMove)}, what compensates $investorSide for the observed material loss beginning with ${notation.line(line)(loss.plyOffset)}? " +
                          s"Does avoiding that capture or changing the response invalidate the named continuation? " +
                          s"There are ${declines.size} legal replies that do not take this target; only the named comparisons are tested."
                        emit(Anchor(Nil, rootMove), List(line.mkString(" "), investorSide, acceptedPrefix.mkString(" "),
                          comparisons.map(compare => (compare.prefixMovesUci :+ compare.referenceMove :+ compare.alternativeMove).mkString(" ")).mkString(" / ")),
                          "sacrifice", origin.sourceRefs, origin,
                          facts ++ comparisons ++ current ++ returnedReplies ++ groups ++ replyGroups ++ replyContexts,
                          groups.map(_.id) ++ comparisons.map(_.id), question = Some(question +
                            (if hasReturn then " Observed returns and their adequacy are separate questions."
                             else " No subsequent compensation has yet been observed in this finite line.")),
                          additionalHeads = replyContexts.map(group => List(group.id)))
                        // Reuse the plan counter test for the first observed return. A legal
                        // acceptance probe alone never supplies an invented compensating action.
                        val returnOffset = line.indices.find(offset => offset > loss.plyOffset && offset >= 2 &&
                          (facts.exists(_.id == s"sacrifice_gain_$offset") ||
                            facts.exists(_.id == s"sacrifice_promotion_$offset") ||
                            facts.exists(_.id == s"sacrifice_check_$offset")))
                        returnOffset.foreach { offset =>
                          val actionMoves = line.take(offset + 1)
                          val ready = inspect(actionMoves.init)
                          val goal = ready.flatMap(at => at.view.legalRows.flatMap(_.find(_.move == line(offset)))
                            .flatMap(row => action(at, row, origin, Some(offset))))
                          goal.foreach { goal =>
                            def requested(prefix: List[String]): Option[Board] = inspect(prefix).orElse {
                              requireReplay(prefix)
                              None
                            }
                            val kings = ready.toList.flatMap(_.originals.collect {
                              case (piece, original) if original.side == investorSide && original.role == "king" => piece.square.key
                            })
                            // A counter to this first return needs its loss and that return,
                            // not another copy of every later exchange in the material story.
                            val counterObservations = facts.filter(_.at match
                              case event: Event => event.fromPly == loss.plyOffset || event.fromPly == offset
                              case _ => false).map(fact => fact.copy(path = declared("sacrifice", actionMoves)))
                            val counterCurrent = rootComparison.toList.flatMap(compare => counterObservations.map(fact =>
                              fact.copy(id = "current_" + fact.id,
                                path = Path("current_sacrifice", Comparison(compare.id, "reference")))))
                            val counterGroups = List(FactGroup("sacrifice_observed", "all", counterObservations.map(_.id))) ++
                              Option.when(counterCurrent.nonEmpty)(FactGroup("sacrifice_current", "all", counterCurrent.map(_.id)))
                            planCounters(goal, actionMoves, kings, requested, includeCaptures = true).foreach { (reply, counterFacts) =>
                              emit(Anchor(Nil, rootMove), List(actionMoves.mkString(" "), investorSide, reply,
                                rootComparison.map(_.alternativeMove).getOrElse("")),
                                "sacrifice-counter", origin.sourceRefs, origin,
                                counterObservations ++ rootComparison.toList ++ counterCurrent ++ counterGroups ++ counterFacts,
                                counterGroups.map(_.id) ++ List("contrast_context", "contrast_strength"),
                                question = Some(s"After the observed loss to ${notation.line(line)(loss.plyOffset)}, does ${notation.move(actionMoves.dropRight(2), reply)} stop or change the return ${notation.line(line)(offset)}, and is it stronger than ${notation.line(actionMoves).init.last}? " +
                                  "The material observations describe this exact continuation; the comparison tests this response only."))
                            }
                          }
                        }
                    }
                  }
                }
              }
            val duePreparations = preparations.filter { (_, preparedMoves) =>
              val atFrontier = investigativeRootMoves(preparedMoves.head) &&
                frontiers.get(preparedMoves(ply)).contains(preparedMoves.size)
              val alreadyPublished = previousActions.get(preparedMoves).exists(_.exists(_.questionText.nonEmpty))
              // Preserve issued questions for reconsideration. Later actions remain in the
              // source PV, without allocating questions, samples or proof combinations yet.
              val due = atFrontier || alreadyPublished
              deferredActions ||= !due
              due
            }
            duePreparations.foreach { (goal, preparedMoves) =>
              val preparation = preparedMoves(ply)
              val atFrontier = investigativeRootMoves(preparedMoves.head) && frontiers.get(preparation).contains(preparedMoves.size)
              def requested(prefix: List[String]): Option[Board] = inspect(prefix).orElse {
                if atFrontier then requireReplay(prefix)
                None
              }
              val preparationRow = rows.find(_.move == preparation)
              val directRow = rows.find(row => action(root, row, canonical).exists(candidate =>
                candidate.actor == goal.actor && candidate.to == goal.to && candidate.capture == goal.capture &&
                  candidate.promotion == goal.promotion))
              val beforeAction = preparedMoves.init
              for
                first <- preparationRow if first.capture.isEmpty && first.promotionRole.isEmpty
                firstBoard <- inspect(start :+ preparation)
                if firstBoard.view.completeKinds(RelationFactKind.GeometricControl) &&
                  firstBoard.view.currentCheckSources(!mover).isEmpty
                ready <- inspect(beforeAction)
                if value(ready, goal.predicate).value.contains("legal") && beforeAction.size > ply + 1
              do
                val kings = root.originals.values.filter(piece => piece.side == side && piece.role == "king").toList
                val boards = (ply to beforeAction.size).flatMap(count => inspect(beforeAction.take(count))).toList
                val kingSquares = boards.flatMap(at => at.originals.collect {
                  case (current, original) if kings.contains(original) => current.square.key
                }).distinct
                val actors = boards.flatMap(_.originals.values).distinct
                val contestedSquares = inspect(preparedMoves).toList.flatMap(at => at.view.legalRows.toList.flatten
                  .filter(_.capture.flatMap(at.original).exists(_.side == side)).map(_.to.key))
                val actionSquares = (goal.to :: contestedSquares).distinct
                val predicates = List(Predicate("location", Some(goal.actor))) ++
                  goal.capture.toList.map(target => Predicate("location", Some(target))) ++
                  kings.map(king => Predicate("location", Some(king))) ++
                  Option.when(goal.actor.role == "pawn")(Predicate("pawn_advance", Some(goal.actor), square = Some(goal.to))) ++
                  actors.flatMap(actor => actionSquares.map(square => Predicate("control", Some(actor), square = Some(square)))) ++
                  actors.filter(_.side != side).flatMap(actor => kingSquares.map(square =>
                    Predicate("control", Some(actor), square = Some(square))))
                val effects = trace(beforeAction, predicates)
                val initial = value(root, goal.predicate).value.filter(Set("legal", "illegal"))
                initial.foreach { available =>
                  val facts = List[Requirement](
                    Fact("reference_action", declared("reference", preparedMoves), goal.predicate,
                      Ply(beforeAction.size), expected = Some("legal")),
                    Fact("alternative_action", declared("alternative", start), goal.predicate,
                      Ply(ply), expected = Some(available)),
                    FactGroup("contrast_context", "all", List("reference_action", "alternative_action")))
                  val comparisons = directRow.filter(_.move != preparation).toList.map(direct =>
                    CompareMoves("contrast_immediate", start, preparation, direct.move))
                  val kind = if available == "illegal" then "action-enablement" else "preparation-effect"
                  emit(Anchor(start, preparation), List(preparedMoves.mkString(" "), goal.predicate.json.toString),
                    kind, goal.sourceRefs, goal.source, facts ++ effects ++ comparisons ++ returnedActions(comparisons, goal),
                    "contrast_context" :: (effects.collect { case value: FactComparison => value.id } ++ comparisons.map(_.id)),
                    question = Some(s"What changes before ${goalName(goal)} after ${notation.move(start, preparation)}, which moves cause them, and is acting immediately available or preferable?"))
                }

                // Alternatives come from independent goal observations or from a quiet move
                // changing the same action-related condition. A shared reply sequence is held
                // fixed to test usability; its legality is never assumed for another move order.
                val firstEffects = effects.collect {
                  case fact: Fact if fact.at == Ply(ply + 1) && fact.id.endsWith("_after") => fact.predicate
                }
                def function(predicate: Predicate) = (predicate.kind,
                  Option.when(predicate.kind == "location")(predicate.subject), predicate.target, predicate.square)
                val effectFunctions = firstEffects.map(function).toSet
                val independentlyPrepared = preparations.filter((other, moves) =>
                  sameAction(goal, other) && moves(ply) != preparation).groupBy(_._2(ply))
                val effectSquares = firstEffects.flatMap(_.square).distinct
                val alternativeMoves = rows.filter(row => row.move != preparation && !directRow.exists(_.move == row.move) &&
                  row.capture.isEmpty && row.promotionRole.isEmpty &&
                  (independentlyPrepared.contains(row.move) || !root.original(row.actor).contains(goal.actor) ||
                    root.original(first.actor).contains(goal.actor)) &&
                  (independentlyPrepared.contains(row.move) || root.original(row.actor) == root.original(first.actor) ||
                    touches(row, root, effectSquares)))
                alternativeMoves.foreach { candidate =>
                  val observed = independentlyPrepared.get(candidate.move).toList.flatten.map(_._2).sortBy(_.mkString(" ")).headOption
                  val after = requested(start :+ candidate.move)
                  val comparable = root.original(candidate.actor) == root.original(first.actor) || after.exists(at => predicates.exists(predicate => effectFunctions(function(predicate)) &&
                    value(root, predicate).value.nonEmpty && value(at, predicate).value.nonEmpty &&
                    value(root, predicate).value != value(at, predicate).value))
                  if observed.nonEmpty || comparable then
                    val alternatePrefix = observed.map(_.init).getOrElse(start ++ (candidate.move :: beforeAction.drop(ply + 1)))
                    requested(alternatePrefix).foreach { alternate =>
                      value(alternate, goal.predicate).value.filter(Set("legal", "illegal")).foreach { availability =>
                        val comparison = CompareMoves("contrast_value", start, preparation, candidate.move)
                        val facts = List[Requirement](
                          Fact("reference_action", declared("reference", preparedMoves), goal.predicate,
                            Ply(beforeAction.size), expected = Some("legal")),
                          Fact("alternative_action", declared("alternative", alternatePrefix), goal.predicate,
                            Ply(alternatePrefix.size), expected = Some(availability)),
                          FactComparison("contrast_action", "reference_action", "alternative_action",
                            if availability == "legal" then "same" else "different"),
                          FactGroup("contrast_context", "all", List("reference_action", "alternative_action", "contrast_action")),
                          comparison) ++ returnedActions(List(comparison), goal)
                        emit(Anchor(start, preparation), List(preparedMoves.mkString(" "), alternatePrefix.mkString(" ")),
                          "preparation-alternative", goal.sourceRefs, goal.source, facts,
                          List("contrast_context", "contrast_value"),
                          question = Some(s"Does ${notation.move(start, candidate.move)} also prepare ${goalName(goal)}, and how does it compare with ${notation.move(start, preparation)}? The named continuations specify the scope of that comparison."))
                      }
                    }
                }

                planCounters(goal, preparedMoves, kingSquares, requested).foreach { (reply, facts) =>
                  val counterPrefix = beforeAction.init
                  val observedReply = beforeAction.last
                  emit(Anchor(start, preparation), List(counterPrefix.mkString(" "), observedReply, reply, goal.predicate.json.toString),
                    "plan-counter", goal.sourceRefs, goal.source, facts, List("contrast_context", "contrast_strength"),
                    question = Some(s"Does the quiet reply ${notation.move(counterPrefix, reply)} prevent or change ${goalName(goal)}, and is it stronger than ${notation.move(counterPrefix, observedReply)} at this position?"))
                }
              for
                first <- preparationRow if first.capture.isEmpty && first.promotionRole.isEmpty
                direct <- directRow if direct.move != preparation && preparedMoves.size < MaximumPly
                firstBoard <- inspect(start :+ preparation)
                if firstBoard.view.completeKinds(RelationFactKind.GeometricControl) &&
                  firstBoard.view.currentCheckSources(!mover).isEmpty
                planned <- inspect(preparedMoves)
              do
                val directMoves = start :+ direct.move
                // These probes are justified by the observed action and legal counter-capture;
                // they are not a new source of goals or an unrestricted off-PV scan.
                requested(directMoves).foreach { immediate =>
                  val replies = for
                    left <- planned.view.legalRows.toList.flatten if left.capture.nonEmpty
                    enemy <- planned.original(left.actor).toList
                    victim <- left.capture.flatMap(planned.original).toList if victim.side == side
                    right <- immediate.view.legalRows.toList.flatten.find(row =>
                      immediate.original(row.actor).contains(enemy) && row.capture.flatMap(immediate.original).contains(victim) &&
                        row.promotionRole == left.promotionRole).toList
                  yield (left, right, enemy, victim)
                  replies.foreach { (left, right, enemy, victim) =>
                    val referenceReply = preparedMoves :+ left.move
                    val alternativeReply = directMoves :+ right.move
                    val afterReference = requested(referenceReply)
                    val afterAlternative = requested(alternativeReply)
                    for
                      a <- afterReference.toList
                      b <- afterAlternative.toList
                      responders = (a :: b :: Nil).flatMap(at => at.view.legalRows.toList.flatten
                        .filter(_.capture.flatMap(at.original).contains(enemy)).flatMap(row => at.original(row.actor)))
                        .filter(_.side == side).distinct.sortBy(_.json.toString)
                      responder <- responders
                      predicate = Predicate("legal_capture", Some(responder), Some(enemy))
                      available = value(a, predicate)
                      unavailable = value(b, predicate)
                      if available.value.nonEmpty && unavailable.value.nonEmpty && available.value != unavailable.value
                    do
                      val counter = Predicate("legal_capture", Some(enemy), Some(victim))
                      val facts: List[Requirement] = List(
                        Fact("reference_action", declared("reference", preparedMoves), goal.predicate,
                          Ply(preparedMoves.size - 1), expected = Some("legal")),
                        Fact("alternative_action", declared("alternative", directMoves), goal.predicate,
                          Ply(ply), expected = Some("legal")),
                        Fact("reference_counter", declared("reference", preparedMoves), counter,
                          Ply(preparedMoves.size), expected = Some("legal")),
                        Fact("alternative_counter", declared("alternative", directMoves), counter,
                          Ply(ply + 1), expected = Some("legal")),
                        Fact("reference_response", declared("reference", referenceReply), predicate, Ply(referenceReply.size)),
                        Fact("alternative_response", declared("alternative", alternativeReply), predicate, Ply(alternativeReply.size)),
                        FactComparison("contrast_response", "reference_response", "alternative_response", "different"),
                        FactGroup("contrast_context", "all", List("reference_action", "alternative_action",
                          "reference_counter", "alternative_counter", "contrast_response")))
                      // A legal recapture is not an observed continuation or a verdict. Its use
                      // remains a finite unanswered event until an admitted line supplies it.
                      val continuations = List(("reference", referenceReply, available), ("alternative", alternativeReply, unavailable))
                        .filter((_, prefix, result) => prefix.size < MaximumPly && result.value.contains("legal"))
                        .map { (name, prefix, _) => Fact(s"${name}_continuation", declared(name + "_followup", prefix, true),
                          Predicate("presence", Some(responder)), Event("capture", 1, "after", prefix.size,
                            prefix.size + 1, Some(responder), Some(enemy))) }
                      // Compare the counter with an independently observed reply at this exact
                      // position. A recapture witness cannot answer this evaluation question.
                      val comparisons = sources.filter(_.moves.startsWith(preparedMoves))
                        .flatMap(_.moves.lift(preparedMoves.size)).distinct.sorted
                        .filter(move => move != left.move && planned.view.legalRows.exists(_.exists(_.move == move)))
                        .map(move => CompareMoves(s"contrast_strength_$move", preparedMoves, move, left.move, Some("better")))
                      val effects = trace(beforeAction, List(Predicate("control", Some(responder), square = Some(left.to.key))))
                      emit(Anchor(start, preparation), List(preparedMoves.mkString(" "), direct.move,
                        left.move, right.move, predicate.json.toString), "preparation",
                        goal.sourceRefs ++ planned.occurrenceId.toList ++ immediate.occurrenceId.toList ++
                          List(left.sourceId, right.sourceId) ++ available.sourceIds ++ unavailable.sourceIds,
                        goal.source, facts ++ effects ++ continuations ++ comparisons,
                        "contrast_context" :: comparisons.map(_.id), Some(direct.move),
                        Some(s"How does ${notation.move(start, preparation)} change the response to ${notation.move(preparedMoves, left.move)} after ${goalName(goal)}, compared with immediate ${notation.move(start, direct.move)}, and how strong is that counter?"))
                  }
                }
            }
            val completeInputs = inspected.values.forall(at =>
              Set(RelationFactKind.GeometricControl, RelationFactKind.PawnAdvanceAffordance).subsetOf(at.view.completeKinds) &&
              at.view.legalRows.nonEmpty && at.originals.size == chess.Square.all.count(square => at.view.occupantAt(EvidenceSquare(square.key)).nonEmpty))
            val reason = if !completeInputs then Some("incomplete_discovery_inventory")
              else if missing.nonEmpty then Some("candidate_replay_pending")
              else if pending.nonEmpty then Some("response_probe_pending")
              else if deferredActions then Some("action_frontier")
              else if outsidePv then Some("pv_scope") else None
            val examined = inspected.values.filter(_.count == ply + 1).map(_.moves(ply)).toSet
            Scan(anchor, moves, moves.filter(examined), if reason.nonEmpty then "partial" else "complete", reason,
              pendingReplayPrefixes = pending.toList)
    }
    // Admission is separate from finding candidates. Give each issue one contrast before
    // adding its other arms; an early alphabetic move must not own the whole requirement budget.
    def suppliedArm(draft: Draft): Boolean = boundedSources.exists(source => source.moves.startsWith(
      draft.anchor.prefixMoves ++ draft.alternative.toList))
    val covered = scala.collection.mutable.Map.empty[(List[String], Int), Boolean]
    val availableMoves = (paths.map(_.moves) ++ probes.map(_.moves)).distinct
    def missingPaths(draft: Draft): Int = draft.requirements.count {
      case fact: Fact => fact.path.source match
        case Declared(prefix) =>
          val horizon = (fact.at :: fact.through.toList).map(_.horizon).max
          !covered.getOrElseUpdate(prefix -> horizon, availableMoves.exists(moves =>
            moves.startsWith(prefix) && moves.size >= horizon))
        case _ => true
      case _ => false
    }
    // Selection depends on the decision's legal inventory, never on whether a probe
    // succeeds or an answer settles. Otherwise every refresh would buy more candidates.
    // ponytail: this is bounded condition coverage, not a strongest-defense finder;
    // an existing unrestricted reply survey would be needed to rank untested responses.
    val selectedTacticalChoices = tacticalChoices.toList.groupBy { case ((prefix, reference, _), _) => prefix -> reference }.values
      .flatMap { pool =>
        val direct = pool.filter(_._2._1).sortBy(_._1._3).take(2)
        var covered = direct.flatMap(_._2._2).toSet
        var rest = pool.filterNot(_._2._1)
        val additional = (0 until 2).toList.flatMap { _ =>
          val next = rest.filter(value => (value._2._2 -- covered).nonEmpty)
            .sortBy(value => (-(value._2._2 -- covered).size, value._1._3)).headOption
          next.foreach { value => covered ++= value._2._2; rest = rest.filterNot(_._1 == value._1) }
          next.toList
        }
        direct ++ additional
      }.map(_._1).toSet
    // A changed PV is a new value question at the same exact child history,
    // not evidence that the previously witnessed entry became worthless.
    // Keep the parent's physical facts and counter receipt; append only the
    // missing comparison with its newly recorded alternative.
    CommentaryIdea.reserveEntries(existing).filter(_.optionTest.isEmpty).foreach { proof =>
      for
        test <- RootSubjectComparison.reserveEntryValue(prepared, proof)
        parent <- existing.find(_.request.id == proof.owner.proposalId)
      do
        val requirements = parent.request.requirements :+ test
        // Extend the already owned question: copying its facts into another
        // proposal spent the last requirement slots before this one comparison
        // could be admitted. Required completion precedes optional new drafts.
        admit(Draft(parent.request.id, Anchor(Nil, proof.owner.comparison.referenceMove), "reserve-entry",
          proof.evidence, parent.request.origin.evidence, requirements,
          List(List("reserve_root", "reserve_context", "reserve_counter", test.id)), None,
          Some(s"After ${notation.line(test.prefixMovesUci).mkString(" ")}, does the available ${notation.move(test.prefixMovesUci, test.referenceMove)} remain a useful choice alongside ${notation.move(test.prefixMovesUci, test.alternativeMove)}? Test the option before retaining its explanation."), Nil))
    }
    val eligibleDrafts = drafts.values.filter(draft => draft.kind != "tactical-counter" || draft.requirements.exists {
      case compare: CompareMoves if compare.id == "tactical_strength" =>
        selectedTacticalChoices((compare.prefixMovesUci, compare.referenceMove, compare.alternativeMove))
      case _ => false
    }).toList
    followupRoom = if eligibleDrafts.exists(_.kind == "reserve-use") then 80 else 0
    functionalRoom = if eligibleDrafts.exists(draft => draft.kind == "reply-function" &&
      draft.anchor.prefixMoves.isEmpty && draft.requirements.exists(_.id == "function_task_context") &&
      draft.alternative.exists(move => Set(draft.anchor.referenceMove, move) == investigativeRootMoves.toSet)) ||
      existing.exists(entry => entry.request.requirements.exists(_.id == "function_task_context")) then
        (48 - existing.flatMap(_.request.requirements).count(_.id.startsWith(FunctionalChoiceReading.ValuePrefix))).max(0)
      else 0
    val byKind = eligibleDrafts.groupBy(_.id).values.toList.map(arms =>
      arms.sortBy(draft => (missingPaths(draft), if suppliedArm(draft) then 0 else 1, draft.alternative.getOrElse(""))))
      .groupBy(_.head.kind).toList.sortBy(_._1).map { (_, issues) =>
        issues.sortBy { arms =>
          val actionPly = arms.flatMap(_.requirements.collect {
            case fact: Fact if fact.id == "reference_action" => fact.at.horizon
          }).minOption.getOrElse(MaximumPly)
          (if arms.exists(_.anchor.referenceMove == packet.assembly.input.playedMoveUci) then 0 else 1,
            actionPly, if arms.exists(suppliedArm) then 0 else 1,
            arms.head.requirements.map(_.toString).mkString("\n"))
        }
      }
    // This is a bounded coverage fallback, not an importance/utility estimate: one family of
    // contacts must not exclude all passage or witnessed-action questions by being more numerous.
    val issues = (0 until byKind.map(_.size).maxOption.getOrElse(0)).toList.flatMap(index =>
      byKind.flatMap(_.lift(index)))
    (0 until issues.map(_.size).maxOption.getOrElse(0)).foreach { index =>
      issues.flatMap(_.lift(index)).sortBy(draft => if draft.kind == "opponent-choice" then 0 else 1).foreach(admit)
    }
    // An admitted unanswered counter holds the frontier. Do not spend local replay
    // on later/historical questions, or on observations the requirement budget cannot admit.
    val currentEntries = (additions.values.toList ++ existing).distinctBy(_.request.id)
    // Once admitted, the exact same-reply hypothesis needs its short board
    // witness even if that reply is absent from the other root PV. This is
    // local legal replay, not another search or another discovery source.
    currentEntries.filter(entry => Set("C-reply-space", "C-reserve-entry", "C-current-support-duty", "C-reply-function")(entry.request.origin.question.producer)).foreach { entry =>
      entry.request.requirements.collect { case fact: Fact => fact.path.source }.collect {
        case Declared(moves) if moves.nonEmpty && board(moves).isEmpty => moves
      }.distinct.foreach { moves =>
        replays.add(moves)
        if entry.request.origin.question.producer == "C-reply-function" then functionalReplays.add(moves)
      }
    }
    val openCounters = currentEntries.filter(_.request.origin.question.producer == "C-tactical-counter").flatMap { entry =>
      entry.request.requirements.collect {
        case compare: CompareMoves if compare.id == "tactical_strength" &&
            !entry.readings.find(_.requirementId == compare.id).exists(isResolved) &&
            packet.assembly.input.assessmentCandidates.exists(_.evaluation.moves.startsWith(compare.prefixMovesUci :+ compare.referenceMove)) =>
          (compare.prefixMovesUci, compare.referenceMove, compare.alternativeMove)
      }
    }.distinct.sortBy((prefix, reference, alternative) => (prefix.size, prefix.mkString(" "), reference, alternative))
    val orderedProbes = tacticalProbes.toList.filter(value => selectedTacticalChoices(value._1)).sortBy { case ((prefix, reference, alternative), _) =>
      (prefix.size, prefix.mkString(" "), reference, alternative)
    }
    val frontier = (openCounters.map(_._1) ++ orderedProbes.map(_._1._1)).sortBy(prefix => (prefix.size, prefix.mkString(" "))).headOption
    val occupied = openCounters.count(value => frontier.contains(value._1))
    // At most four alternatives across two existing three-line comparisons. Extra
    // conditions receive no budget of their own and may remain unverified.
    var probeRoom = remaining
    orderedProbes.filter(value => frontier.contains(value._1._1)).take((4 - occupied).max(0)).foreach {
      case ((prefix, reference, alternative), size) =>
        if size <= probeRoom then
          probeRoom -= size
          List(reference, alternative).map(prefix :+ _).filter(board(_).isEmpty).foreach(replays.add)
        else deferred.getOrElseUpdate("", scala.collection.mutable.LinkedHashSet.empty) +=
          CommentarySelection.stableId("tactical-probe", prefix ++ List(reference, alternative))
    }
    val finalScans = scans.map { scan =>
      val pending = deferred.get(scan.anchor.prefixMoves.mkString(" ")).toList.flatMap(_.toList)
      if pending.nonEmpty then scan.copy(state = "partial", reason = Some("requirement_limit"), deferredRequestIds = pending)
      else if scan.anchor.prefixMoves.isEmpty && replays.exists(prefix => !scan.pendingReplayPrefixes.contains(prefix)) then
        scan.copy(state = "partial", reason = Some("response_probe_pending"),
          pendingReplayPrefixes = (scan.pendingReplayPrefixes ++ replays).distinct)
      else scan
    }
    Batch(additions.values.toList, replays.toList, finalScans, functionalReplays.toList)
