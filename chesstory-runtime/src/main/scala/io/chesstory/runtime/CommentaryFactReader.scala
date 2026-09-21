package io.chesstory.runtime

import lila.chessjudgment.analysis.assembly.{LegalMoveResource, OccurrenceResources}
import lila.chessjudgment.model.judgment.*

/** One interpreter for finite propositions; only the admitted adapter can publish evidence. */
private[runtime] object CommentaryFactReader:
  import CommentaryFact.*
  import CommentaryProposal.*
  import ObservedLineProtocol.{PreparedPath, PreparedPaths}

  private val MaximumProofs = CommentaryProposal.MaximumRequirements
  /** Only within one immutable packet. Work/stop presentation is reapplied by the caller. */
  private[runtime] case class Inputs(requirements: List[Requirement], supplied: List[Reading],
      premises: List[(String, List[String], List[(String, List[String])])])

  /** Detached inspection data has no packet/path handles or public evidence identifiers. */
  final case class InspectionWitness(prefixMoves: List[String], fromPly: Int, throughPly: Int,
      value: Option[String], reason: Option[String])
  final case class Inspection(state: String, needs: List[Need], witnesses: List[InspectionWitness],
      witnessPrefixes: List[List[String]], conditionPrefixes: List[List[String]],
      promotionNeeds: List[List[String]], reason: Option[String])

  private sealed trait ReadPath:
    def moves: List[String]
    def admitted: Option[PreparedPath]
    def local: Option[OccurrenceResources.ProbePath]
    def key: Either[String, List[String]]
    def ordering: (Boolean, String) = admitted.isEmpty -> admitted.fold(moves.mkString(" "))(_.line.ref.id)
  private final class AdmittedPath(val path: PreparedPath) extends ReadPath:
    def moves = path.moves
    def admitted = Some(path)
    def local = None
    def key = Left(path.line.ref.id)
  private final class LocalPath(val path: OccurrenceResources.ProbePath) extends ReadPath:
    def moves = path.moves
    def admitted = None
    def local = Some(path)
    def key = Right(path.moves)

  private case class Bound(path: ReadPath, limit: Int, conditionEnd: Int)
  private case class Selection(count: Option[Int], evidenceEnd: Int, reason: Option[String] = None)
  private sealed trait WitnessKey
  private case class ExistingWitness(id: String) extends WitnessKey
  private case class DerivedWitness(requirementId: String, path: Either[String, List[String]],
      count: Int, evidenceEnd: Int) extends WitnessKey
  private case class WitnessPlan(key: WitnessKey, binding: String, moves: List[String],
      fromPly: Int, throughPly: Int, value: Option[String], state: String,
      sourceIds: List[String], reason: Option[String], admitted: Option[PreparedPath] = None,
      existing: Option[Sample] = None)
  private case class BindingPlan(bindings: Map[String, List[String]], witnesses: List[WitnessKey],
      requirementIds: List[String])
  private case class Scope(path: ReadPath, prefix: List[String], promote: Boolean)
  private case class Plan(state: String, samples: List[WitnessPlan] = Nil, needs: List[Need] = Nil,
      proofs: List[BindingPlan] = Nil, reason: Option[String] = None,
      scopes: List[Scope] = Nil, conditions: List[List[String]] = Nil)

  private def compatible(a: List[String], b: List[String]): Boolean = a.startsWith(b) || b.startsWith(a)
  private def merge(a: BindingPlan, b: BindingPlan): Option[BindingPlan] =
    Option.when(a.bindings.forall((key, moves) => b.bindings.get(key).forall(compatible(moves, _)))) {
      BindingPlan((a.bindings.keySet ++ b.bindings.keySet).map { key =>
        key -> (a.bindings.get(key).toList ++ b.bindings.get(key)).maxBy(_.size)
      }.toMap, (a.witnesses ++ b.witnesses).distinct, (a.requirementIds ++ b.requirementIds).distinct)
    }

  /** All caches here die with this read/inspection invocation, including local probe lookups. */
  private final class Interpreter(packet: EvidenceBackedJudgmentPacket, prepared: PreparedPaths,
      probes: List[OccurrenceResources.ProbePath], inspectScopes: Boolean):
    prepared.requireOwner(packet)
    require(probes.forall(_.belongsTo(packet)), "fact inspection probes need this exact root history")
    private val canonical = prepared.paths.filter(path => path.certified.nonEmpty && path.ids == path.declared)
    private lazy val registeredMoves = packet.assembly.input.legalLines.map(_.moves)
    private lazy val admittedPaths: List[ReadPath] = canonical.map(path => new AdmittedPath(path))
    private lazy val admittedByLength = admittedPaths.sortBy(path => (path.moves.size, path.ordering._2))
    // Query only the prefixes actually read, once per invocation. A prepared path may be
    // absent altogether when its registered line has no eligible observation edges.
    private val prefixOwners = scala.collection.mutable.Map.empty[List[String], (Boolean, Option[ReadPath])]
    private def knownOwner(prefix: List[String]): (Boolean, Option[ReadPath]) =
      prefixOwners.getOrElseUpdate(prefix, {
        val known = registeredMoves.exists(_.startsWith(prefix))
        known -> (if known then admittedByLength.find(_.moves.startsWith(prefix)) else None)
      })
    private def ownerAt(path: ReadPath, count: Int): Option[ReadPath] =
      if path.admitted.nonEmpty then Some(path)
      else
        val (known, owner) = knownOwner(path.moves.take(count))
        if known then owner else Some(path)
    private lazy val paths: List[ReadPath] =
      // Raw complete resources cannot silently replace a known path's missing admitted
      // evidence, including a path excluded above for incomplete certification/edges.
      val knownMoves = registeredMoves.toSet
      admittedPaths ++
        probes.filterNot(path => knownMoves(path.moves)).map(path => new LocalPath(path))
    private val atoms = scala.collection.mutable.Map.empty[
      (Either[String, List[String]], Int, Predicate), CommentaryFactAtoms.Value]
    private val samplePrefixes = scala.collection.mutable.Map.empty[(Either[String, List[String]], Int), List[String]]
    private val probePositions = scala.collection.mutable.Map.empty[
      (List[String], Int), Option[OccurrenceResources.ProbePosition]]
    private def localAt(path: ReadPath, count: Int) =
      probePositions.getOrElseUpdate(path.moves -> count, path.local.flatMap(_.at(count)))
    private def atom(path: ReadPath, count: Int, predicate: Predicate): CommentaryFactAtoms.Value =
      ownerAt(path, count).fold(CommentaryFactAtoms.Value(None, reason = Some("known_prefix_unavailable"))) { owned =>
        val owner = owned.admitted.fold[Either[String, List[String]]](Right(owned.moves.take(count))) { admitted =>
          Left(if predicate.kind == "direct_support_use" then admitted.line.ref.id
            else admitted.positions(count).id.getOrElse(admitted.line.ref.id))
        }
        atoms.getOrElseUpdate((owner, count, predicate), owned.admitted.fold(
          localAt(owned, count).fold(CommentaryFactAtoms.Value(None, reason = Some("point_outside_observed_path")))(
            CommentaryFactAtoms.readProbe(_, predicate)))(
          CommentaryFactAtoms.read(packet, prepared, _, count, predicate)))
      }
    private def pieceAt(path: ReadPath, original: Piece, count: Int): Option[Option[RelationColoredPieceWitness]] =
      path.admitted.fold(localAt(path, count).flatMap(_.piece(original.witness)))(
        prepared.pieceAfter(packet, _, original.witness, count))
    private def playedRow(path: ReadPath, count: Int): Option[LegalMoveResource] =
      val rows = path.admitted.fold(localAt(path, count).flatMap(_.facts.legalRows)) { admitted =>
        for
          positionId <- admitted.positions(count).id
          resource <- prepared.resourceViews(packet).get(positionId).filter(_.position == admitted.positions(count))
          rows <- resource.legalRows
        yield rows
      }
      rows.flatMap(_.find(_.move == path.moves(count)))
    private def hasMaterial(path: ReadPath): Boolean =
      path.admitted.fold(true)(_.certified.get._2.material.nonEmpty)
    private def material(path: ReadPath): Option[List[ObservedLineMaterialEvent]] =
      path.admitted.fold(path.local.map(_.materialEvents))(_.certified.get._2.material.map(_.events))
    private def events(path: ReadPath): List[LineMoveEvent] =
      path.admitted.fold(path.local.toList.flatMap(_.lineEvents))(_.certified.get._2.lineEvents)

    private def select(bound: Bound, point: Point): Selection = point match
      case Ply(count) => if count <= bound.limit then Selection(Some(count), count)
        else Selection(None, bound.limit, Some("path_end"))
      case event: Event =>
        if Set("capture", "promotion")(event.kind) && !hasMaterial(bound.path) then
          return Selection(None, math.min(event.throughPly, bound.limit), Some("event_material_unavailable"))
        val candidates = (event.fromPly until math.min(event.throughPly, bound.limit)).toList
        def actorAt(owner: ReadPath, original: Piece, offset: Int): Option[Boolean] =
          pieceAt(owner, original, offset).map(_.exists(_.square.key == owner.moves(offset).take(2)))
        def victimAt(owner: ReadPath, original: Piece, offset: Int): Option[Boolean] = for
          rows <- material(owner)
          movement <- rows.find(_.plyOffset == offset)
          current <- pieceAt(owner, original, offset)
        yield current.exists(piece => movement.capture.exists(_.square == piece.square))
        def destinationAt(owner: ReadPath, square: String, offset: Int): Option[Boolean] =
          playedRow(owner, offset).map(_.to.key == square)
        var unknown = false
        var missingDestination = false
        var missingSource = false
        var missingMaterial = false
        var seen = 0
        var found: Option[Int] = None
        candidates.foreach { offset =>
          if found.isEmpty then
            // An event belongs to the edge, including its actor/victim metadata. A new
            // edge can be observed after an unavailable old prefix without repairing it.
            ownerAt(bound.path, offset + 1) match
              case None =>
                unknown = true
                missingSource = true
              case Some(owner) if Set("capture", "promotion")(event.kind) && !hasMaterial(owner) =>
                unknown = true
                missingMaterial = true
              case Some(owner) =>
                val kindMatches = event.kind match
                  case "move" => true
                  case "capture" => events(owner).exists(e => e.plyOffset == offset && Set(LineEventKind.Capture, LineEventKind.Recapture)(e.kind))
                  case "promotion" => events(owner).exists(e => e.plyOffset == offset && e.kind == LineEventKind.Promotion)
                  case "check" => events(owner).exists(e => e.plyOffset == offset && Set(LineEventKind.Check, LineEventKind.Mate)(e.kind))
                  case "castling" => events(owner).exists(e => e.plyOffset == offset && e.kind == LineEventKind.Castling)
                  case _ => false
                if kindMatches then
                  val actor = event.actor.fold(Option(true))(actorAt(owner, _, offset))
                  val victim = event.target.fold(Option(true))(victimAt(owner, _, offset))
                  val destination = event.square.fold(Option(true))(destinationAt(owner, _, offset))
                  if !actor.contains(false) && !victim.contains(false) && !destination.contains(false) then
                    if actor.isEmpty || victim.isEmpty || destination.isEmpty then
                      unknown = true
                      missingDestination = missingDestination || destination.isEmpty
                    else
                      seen += 1
                      if seen == event.ordinal && !unknown then found = Some(offset)
        }
        found.fold(Selection(None, math.min(event.throughPly, bound.limit),
          Some(if missingSource then "event_source_unavailable" else if missingMaterial then "event_material_unavailable"
            else if missingDestination then "event_destination_unavailable" else if unknown then "event_identity_unavailable"
            else if bound.limit < event.throughPly then "path_end" else "event_absent"))) { offset =>
          Selection(Some(offset + (if event.phase == "after" then 1 else 0)), offset + 1)
        }

    def entry(entry: Entry, inquiries: List[CommentaryContinuationInquiry]): Map[String, Plan] =
      val originalReadings = entry.readings.map(reading => reading.requirementId -> reading).toMap
      val readings = scala.collection.mutable.LinkedHashMap.empty[String, Plan]
      val requirements = entry.request.requirements.map(requirement => requirement.id -> requirement).toMap
      // Most old Fact results are replaced below. Import only a dependency that actually
      // needs its supplied reading, rather than duplicating every prior sample/proof first.
      def previousResult(id: String): Option[Plan] = originalReadings.get(id).flatMap(_.result).map(fromPublic)
      def result(id: String): Plan = readings.getOrElseUpdate(id, previousResult(id).getOrElse(Plan("partial",
        needs = List(Need("input", "requirement_unread", requirementId = Some(id))))))
      val openRequirements = scala.collection.mutable.Map.empty[String, Boolean]
      def open(id: String): Boolean = openRequirements.getOrElseUpdate(id, requirements(id) match
          case fact: Fact => fact.path.extend
          case comparison: FactComparison => open(comparison.leftRequirementId) || open(comparison.rightRequirementId)
          case group: FactGroup => group.members.exists(open)
          case _ => false)
      def sample(fact: Fact, bound: Bound, count: Int, evidenceEnd: Int): WitnessPlan =
        val value = atom(bound.path, count, fact.predicate)
        val length = math.max(bound.conditionEnd, math.max(count, evidenceEnd))
        val moves = samplePrefixes.getOrElseUpdate(bound.path.key -> length, bound.path.moves.take(length))
        WitnessPlan(DerivedWitness(fact.id, bound.path.key, count, evidenceEnd), fact.path.binding,
          moves, count, count, value.value, if value.value.nonEmpty then "known" else "unavailable_input",
          value.sourceIds, value.reason, bound.path.admitted)
      def proof(fact: Fact, samples: List[WitnessPlan]): BindingPlan =
        BindingPlan(Map(fact.path.binding -> samples.map(_.moves).maxByOption(_.size).getOrElse(Nil)),
          samples.map(_.key), List(fact.id))

      def readFact(fact: Fact): Plan =
        val horizon = (fact.at :: fact.through.toList).map(_.horizon).max
        val source: Either[Need, List[(List[String], Option[String])]] = fact.path.source match
          case Declared(moves) => Right(List(moves -> None))
          case Comparison(id, arm) => originalReadings.get(id).flatMap(_.answer).map { answer =>
            val lineId = if arm == "reference" then answer.referenceLineId else answer.alternativeLineId
            canonical.find(_.line.ref.id == lineId).map(path => Right(List(path.moves -> Some(lineId))))
              .getOrElse(Left(Need("input", "comparison_path_unavailable", requirementId = Some(id))))
          }.getOrElse(Left(Need("comparison", "comparison_not_answered", requirementId = Some(id))))
          case Premise(inquiryId, premiseId) =>
            inquiries.find(_.question.id == inquiryId) match
              case None => Left(Need("input", "premise_owner_unavailable"))
              case Some(inquiry) => inquiry.premiseObservations.find(_.premise.id(inquiryId) == premiseId) match
                case None => Left(Need("input", "premise_unavailable"))
                case Some(observation) =>
                  val witnesses = observation.witnessLineIds.flatMap(id => canonical.find(_.line.ref.id == id).map(path => path.moves -> Some(id)))
                  if witnesses.nonEmpty then Right(witnesses)
                  else
                    val prefix = inquiry.question.prefixMoves
                    val executable = fact.path.extend && prefix.nonEmpty && prefix.size < horizon && prefix.size < 80
                    Left(Need(if executable then "continuation" else "input", "premise_witness_unavailable",
                      prefix, horizon = Some(horizon)))
        source match
          case Left(need) => Plan("partial", needs = List(need),
            conditions = Option.when(inspectScopes && need.prefixMoves.nonEmpty)(need.prefixMoves).toList)
          case Right(seeds) =>
            val bounds = seeds.flatMap { (seed, sourceId) =>
              val exact = paths.filter(path => path.moves.startsWith(seed) &&
                (fact.path.extend || sourceId.forall(id => path.admitted.exists(_.line.ref.id == id))))
              if exact.nonEmpty then
                // A closed condition already covered by an admitted longer line keeps
                // that line's source completeness; a shorter raw probe cannot repair it.
                val selected = if fact.path.extend then exact else
                  exact.sortBy(path => (path.admitted.isEmpty, path.moves.size, path.ordering._2)).take(1)
                selected.map(path => Bound(path, if fact.path.extend then math.min(path.moves.size, horizon) else seed.size, seed.size))
              else paths.filter(path => seed.startsWith(path.moves)).sortBy(path => (-path.moves.size, path.ordering)).take(1)
                .map(path => Bound(path, path.moves.size, seed.size))
            }.distinctBy(bound => (bound.path.moves.take(math.max(bound.limit, bound.conditionEnd)), bound.limit, bound.conditionEnd))
            if bounds.isEmpty then Plan("partial", needs = List(Need("input", "path_unavailable", horizon = Some(horizon))),
              conditions = if inspectScopes then seeds.map(_._1).distinct else Nil)
            else
              var samplesRemaining = MaximumProofs
              val observations = bounds.map { bound =>
                val start = select(bound, fact.at)
                val end = fact.through.fold(start)(select(bound, _))
                val availableStart = start.count
                val availableEnd = end.count.orElse(Option.when(end.reason.contains("path_end") && fact.through.nonEmpty)(bound.limit))
                val counts = (for
                  from <- availableStart.toList
                  until <- availableEnd.toList
                  if until >= from
                  count <- (from to until).toList
                yield count)
                val samples = counts.take(samplesRemaining).map(count => sample(fact, bound, count,
                  math.max(start.evidenceEnd, end.evidenceEnd)))
                val truncated = counts.size > samples.size
                samplesRemaining -= samples.size
                val absent = start.reason.contains("event_absent") || end.reason.contains("event_absent")
                val reversed = start.count.exists(from => end.count.exists(_ < from))
                val sourceComplete = bound.conditionEnd <= bound.path.moves.size
                val complete = sourceComplete && start.count.nonEmpty && end.count.nonEmpty && !reversed && !truncated && samples.forall(_.value.nonEmpty)
                val contradicted = fact.expected.exists(expected => samples.exists(_.value.exists(_ != expected)))
                val supported = complete && fact.expected.forall(expected => samples.forall(_.value.contains(expected)))
                val missing = (start.reason.toList ++ end.reason.toList ++ samples.flatMap(_.reason) ++
                  Option.when(truncated)("witness_limit") ++ Option.when(!sourceComplete)("declared_path_unavailable")).distinct
                // Extending an exact history cannot undo an already witnessed interval
                // violation. Other arms remain open, and the counterexample stays below.
                val needs = missing.filterNot(reason => reason == "event_absent" ||
                  (reason == "path_end" && contradicted && sourceComplete && !reversed)).map { reason =>
                  Need(if reason == "path_end" && fact.path.extend && bound.limit > 0 && bound.limit < 80 then "continuation" else "input",
                    reason, bound.path.moves.take(bound.limit), horizon = Some(horizon))
                }
                val state = if reversed then "invalid_input" else if supported then
                  if fact.expected.nonEmpty then "supported" else "observed"
                else if contradicted && !fact.path.extend && sourceComplete then "refuted"
                else if absent then "not_applicable" else "partial"
                val scopes = if !inspectScopes then Nil else
                  val witnessEnd = List(bound.conditionEnd, start.evidenceEnd, end.evidenceEnd,
                    samples.map(_.throughPly).maxOption.getOrElse(0)).max
                  val prefix = bound.path.moves.take(witnessEnd)
                  List(Scope(bound.path, prefix, bound.path.local.nonEmpty && !knownOwner(prefix)._1))
                Plan(state, samples, needs, if supported then List(proof(fact, samples)) else Nil,
                  if reversed then Some("reversed_interval") else if absent then Some("event_absent") else None,
                  scopes)
              }
              // Retire only an incomplete frontier, never its known samples or a complete
              // event reading. A longer line may lack the shorter line's material ledger.
              val perPath = observations.zip(bounds).filterNot { (result, bound) =>
                fact.path.extend && result.state == "partial" && result.needs.nonEmpty &&
                  result.needs.forall(need => need.kind == "continuation" && need.reason == "path_end") &&
                  bounds.exists(other => other.limit > bound.limit &&
                    other.path.moves.startsWith(bound.path.moves.take(math.max(bound.limit, bound.conditionEnd))))
              }.map(_._1)
              val supported = perPath.filter(r => Set("observed", "supported")(r.state))
              val state = if supported.nonEmpty then
                if fact.expected.nonEmpty then "supported" else if perPath.forall(_.state == "observed") then "observed" else "partial"
              else if perPath.forall(_.state == "refuted") then "refuted"
              else if perPath.forall(_.state == "not_applicable") then "not_applicable"
              else if perPath.forall(_.state == "invalid_input") then "invalid_input" else "partial"
              val unresolvedOpen = fact.path.extend && supported.isEmpty && !perPath.exists(_.needs.nonEmpty) && state != "invalid_input"
              val reason = perPath.flatMap(_.reason).distinct match
                case reason :: Nil => Some(reason)
                case _ => None
              Plan(state, observations.flatMap(_.samples).distinctBy(_.key), perPath.flatMap(_.needs).distinct ++
                Option.when(unresolvedOpen)(Need("input", "observed_extensions_not_exhaustive", horizon = Some(horizon))),
                supported.flatMap(_.proofs).take(MaximumProofs), reason,
                if inspectScopes then observations.flatMap(_.scopes).distinct else Nil,
                if inspectScopes then seeds.map(_._1).distinct else Nil)

      def readComparison(value: FactComparison): Plan =
        val left = result(value.leftRequirementId)
        val right = result(value.rightRequirementId)
        def usable(result: Plan, sample: WitnessPlan) = sample.value.nonEmpty &&
          !result.needs.exists(_.reason == "declared_path_unavailable")
        val candidates = (for
          a <- left.samples.iterator if usable(left, a)
          b <- right.samples.iterator if usable(right, b)
        yield a -> b).take(MaximumProofs + 1).toList
        val truncated = candidates.size > MaximumProofs
        val pairs = for
          (a, b) <- candidates.take(MaximumProofs)
          combined <- merge(BindingPlan(Map(a.binding -> a.moves), List(a.key), List(value.leftRequirementId)),
            BindingPlan(Map(b.binding -> b.moves), List(b.key), List(value.rightRequirementId))).toList
        yield
          val satisfied = value.relation match
            case "same" => a.value == b.value
            case "different" => a.value != b.value
            case "before" => a.fromPly < b.fromPly
            case "after" => a.fromPly > b.fromPly
            case "same_time" => a.fromPly == b.fromPly
            case _ => false
          satisfied -> combined.copy(requirementIds = combined.requirementIds :+ value.id)
        val proofs = pairs.collect { case (true, proof) => proof }.take(MaximumProofs)
        val complete = !truncated && !open(value.id) && List(left, right).forall(r =>
          !r.needs.exists(_.reason == "witness_limit") && Set("observed", "supported", "refuted")(r.state))
        Plan(if proofs.nonEmpty then "supported" else if complete then "refuted" else "partial", Nil,
          (left.needs ++ right.needs).distinct ++ Option.when(truncated)(Need("input", "witness_limit")), proofs,
          Option.when(complete && pairs.isEmpty)("incompatible_paths"),
          if inspectScopes then (left.scopes ++ right.scopes).distinct else Nil,
          if inspectScopes then (left.conditions ++ right.conditions).distinct else Nil)

      def readGroup(value: FactGroup): Plan =
        val children = value.members.map(result)
        val needs = children.flatMap(_.needs).distinct
        val interpreted =
          if value.mode == "any" then
            val available = children.iterator.filter(_.state == "supported").flatMap(_.proofs).take(MaximumProofs + 1).toList
            val proofs = available.take(MaximumProofs)
              .map(p => p.copy(requirementIds = (p.requirementIds :+ value.id).distinct))
            Plan(if proofs.nonEmpty then "supported" else if children.forall(_.state == "refuted") then "refuted" else "partial",
              Nil, needs ++ Option.when(available.size > MaximumProofs)(Need("input", "witness_limit")), proofs)
          else if children.exists(_.state == "refuted") then Plan("refuted", needs = needs)
          else if !children.forall(_.state == "supported") then Plan("partial", needs = needs)
          else
            var truncated = needs.exists(_.reason == "witness_limit")
            val joined = children.foldLeft(List(BindingPlan(Map.empty, Nil, Nil))) { (known, child) =>
              val pairs = known.iterator.flatMap(a => child.proofs.iterator.map(b => a -> b)).take(MaximumProofs + 1).toList
              if pairs.size > MaximumProofs then truncated = true
              pairs.take(MaximumProofs).flatMap((a, b) => merge(a, b)).distinct
            }.map(p => p.copy(requirementIds = (p.requirementIds :+ value.id).distinct))
            Plan(if joined.nonEmpty then "supported" else if truncated || open(value.id) then "partial" else "refuted",
              Nil, needs ++ Option.when(truncated)(Need("input", "witness_limit")), joined,
              Option.when(joined.isEmpty)(if truncated then "witness_limit" else "incompatible_paths"))
        if inspectScopes then interpreted.copy(scopes = children.flatMap(_.scopes).distinct,
          conditions = children.flatMap(_.conditions).distinct)
        else interpreted

      entry.request.requirements.foreach { requirement =>
        if originalReadings(requirement.id).state != "invalid_input" then
          val interpreted = requirement match
            case fact: Fact => Some(readFact(fact))
            case comparison: FactComparison => Some(readComparison(comparison))
            case group: FactGroup => Some(readGroup(group))
            case _ => None
          interpreted.foreach(value => readings.update(requirement.id, value))
      }
      interpretedIds(entry).toList.flatMap(id => readings.get(id).orElse(previousResult(id)).map(id -> _)).toMap

  private def interpretedIds(entry: Entry): Set[String] = entry.request.requirements.collect {
    case fact: Fact => fact.id
    case comparison: FactComparison => comparison.id
    case group: FactGroup => group.id
  }.toSet

  private def inputs(entry: Entry, inquiries: List[CommentaryContinuationInquiry]): Inputs =
    val interpreted = interpretedIds(entry)
    val premiseIds = entry.request.requirements.collect {
      case fact: Fact => fact.path.source
    }.collect { case Premise(inquiryId, _) => inquiryId }.toSet
    Inputs(entry.request.requirements, entry.readings.filter(reading =>
      !interpreted(reading.requirementId) || reading.state == "invalid_input"),
      inquiries.filter(inquiry => premiseIds(inquiry.question.id)).map(inquiry =>
        (inquiry.question.id, inquiry.question.prefixMoves, inquiry.premiseObservations.map(observation =>
          observation.premise.id(inquiry.question.id) -> observation.witnessLineIds))))

  private def fromPublic(value: Result): Plan = Plan(value.state, value.samples.map(sample =>
    WitnessPlan(ExistingWitness(sample.id), sample.binding, sample.moves, sample.fromPly, sample.throughPly,
      sample.value, sample.state, sample.sourceIds, sample.reason, existing = Some(sample))), value.needs,
    value.proofs.map(proof => BindingPlan(proof.bindings, proof.sampleIds.map(ExistingWitness(_)), proof.requirementIds)),
    value.reason)

  /** No local witness key has a public ID. Only this adapter constructs serializable evidence. */
  private def materialize(entryId: String, plan: Plan): Result =
    require(plan.scopes.forall(_.path.local.isEmpty), "an unadmitted probe cannot publish an event/absence result")
    def sampleId(key: WitnessKey): String = key match
      case ExistingWitness(id) => id
      case DerivedWitness(requirement, Left(line), count, evidenceEnd) =>
        CommentarySelection.stableId("fact-sample", List(entryId, requirement, line, count.toString, evidenceEnd.toString))
      case _: DerivedWitness => throw IllegalArgumentException("an unadmitted probe cannot publish a sample or proof")
    val samples = plan.samples.map { witness =>
      witness.existing.getOrElse {
        val path = witness.admitted.getOrElse(
          throw IllegalArgumentException("a public fact sample requires its admitted path"))
        Sample(sampleId(witness.key), path.line.ref.id, witness.binding, witness.moves,
          witness.fromPly, witness.throughPly, witness.value, witness.state, witness.sourceIds, witness.reason)
      }
    }
    Result(plan.state, samples, plan.needs, plan.proofs.map(proof =>
      Proof(proof.bindings, proof.witnesses.map(sampleId), proof.requirementIds)), plan.reason)

  def read(packet: EvidenceBackedJudgmentPacket, prepared: PreparedPaths, entries: List[Entry],
      inquiries: List[CommentaryContinuationInquiry] = Nil): List[Entry] =
    val interpreter = new Interpreter(packet, prepared, Nil, inspectScopes = false)
    entries.map { entry =>
      val results = prepared.factReading(packet, entry.request.id, inputs(entry, inquiries)) {
        interpreter.entry(entry, inquiries).map((id, plan) => id -> materialize(entry.request.id, plan))
      }
      entry.copy(readings = entry.readings.map { previous =>
        if previous.state == "invalid_input" then previous
        else results.get(previous.requirementId).fold(previous)(value => previous.copy(
          state = if previous.state == "stopped" then "stopped" else value.state,
          stopReason = if previous.state == "stopped" then previous.stopReason else value.reason, result = Some(value)))
      })
    }

  /** Inspection never updates entries, uses the packet Result cache, or admits a probe.
    * Even empty/event-absent readings retain their observed scope for later promotion.
    */
  def inspect(packet: EvidenceBackedJudgmentPacket, prepared: PreparedPaths, entries: List[Entry],
      probes: List[OccurrenceResources.ProbePath],
      inquiries: List[CommentaryContinuationInquiry] = Nil): Map[String, Map[String, Inspection]] =
    val interpreter = new Interpreter(packet, prepared, probes, inspectScopes = true)
    entries.map { entry =>
      entry.request.id -> interpreter.entry(entry, inquiries).map { (id, plan) =>
        val witnessPrefixes = (plan.scopes.map(_.prefix) ++ plan.samples.map(_.moves)).distinct
        val promotion = plan.scopes.filter(_.promote).map(_.prefix).filter(_.nonEmpty).distinct
        id -> Inspection(plan.state, plan.needs, plan.samples.map(sample =>
          InspectionWitness(sample.moves, sample.fromPly, sample.throughPly, sample.value, sample.reason)),
          witnessPrefixes, plan.conditions.distinct, promotion, plan.reason)
      }
    }.toMap
