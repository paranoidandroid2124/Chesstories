package io.chesstory.runtime

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import lila.chessjudgment.model.judgment.*
import play.api.libs.json.*

/** Selection over already admitted evidence. This does not search, grade moves,
  * or turn a supplied continuation into a forced line.
  */
private[runtime] object CommentarySelection:
  enum State(val wireId: String):
    case Ready extends State("ready")
    case Partial extends State("partial")
    case NeedsExplanation extends State("needs_explanation")

  enum Display(val wireId: String):
    case Selected extends Display("selected")
    case DisplayCap extends Display("display_cap")
    case DisplayRepresentative extends Display("display_representative")
    case Context extends Display("context")

  private enum ComparisonResult(val relation: String, val gap: Option[String], val state: State):
    case MissingPath extends ComparisonResult("unavailable", Some("missing_path"), State.Partial)
    case IncompleteLine extends ComparisonResult("unavailable", Some("incomplete_line"), State.Partial)
    case MaterialUnavailable extends ComparisonResult("unavailable", Some("material_evidence_unavailable"), State.Partial)
    case Equal extends ComparisonResult("equal", Some("beyond_material_scope"), State.NeedsExplanation)
    case Different extends ComparisonResult("different", None, State.Ready)

  private case class Source(lineId: String, kind: String, source: String, acceptedPlyCount: Int,
      stop: Option[CommentaryLineStop] = None):
    def json: JsObject = Json.obj("line_id" -> lineId, "kind" -> kind, "source" -> source,
      "accepted_ply_count" -> acceptedPlyCount, "stop" -> stop.map(_.wireId))

  private case class Path(line: LegalLineNode, ids: List[String], positions: List[String], moves: List[String],
      sources: List[Source], certified: Boolean, material: Option[LineMaterialSummary], complete: Boolean):
    def state: State = if complete && certified then State.Ready else State.Partial
    def id: String = s"line:${line.ref.id}"
    lazy val signature: Option[MaterialSignature] = material.map(materialSignature)
    lazy val materialDescription: String = signature.map(materialText).getOrElse("")

  private enum Content:
    case Conditional(value: Path)
    case Comparison(ref: EvidenceRef, fact: CandidateComparisonFact, reference: Option[Path],
        candidate: Option[Path], result: ComparisonResult)

    def kind: String = this match
      case Conditional(_) => "conditional_line"
      case Comparison(_, _, _, _, _) => "comparison"

    def detail: JsObject = this match
      case Conditional(_) => Json.obj("type" -> "observed_line", "scope" -> "observed_line")
      case Comparison(ref, fact, reference, candidate, result) =>
        val material = for
          _ <- Option.when(result.state != State.Partial)(())
          left <- reference
          right <- candidate
          leftMaterial <- left.material
          rightMaterial <- right.material
        yield (left, right, leftMaterial, rightMaterial)
        val observed = result == ComparisonResult.Different
        val base = Json.obj(
          "type" -> (if observed then "observed_material_comparison" else "evaluation_comparison"),
          "scope" -> "observed_lines", "comparison_evidence_id" -> ref.id,
          "comparison_kind" -> (fact.kind match
            case CandidateComparisonKind.PlayedVsBest => "played_vs_best"
            case CandidateComparisonKind.BestVsSecond => "best_vs_second"),
          "reference_line_id" -> reference.map(_.line.ref.id), "candidate_line_id" -> candidate.map(_.line.ref.id),
          "missing_line_ids" -> (Option.when(reference.isEmpty)(fact.referenceLine.id).toList ++
            Option.when(candidate.isEmpty)(fact.candidateLine.id).toList),
          "material_relation" -> result.relation, "explanation_gap" -> result.gap,
          "text" -> (if observed then material.map { (left, right, _, _) =>
            s"In the provided ${left.moves.size}-ply line beginning ${left.line.ref.rootMove}, ${left.materialDescription}; " +
              s"in the provided ${right.moves.size}-ply line beginning ${right.line.ref.rootMove}, ${right.materialDescription}. " +
              "These are observed exchanges, not a forced outcome or the sole reason for the evaluation."
          }.getOrElse("") else if result == ComparisonResult.MissingPath then
            "The comparison is available, but a referenced continuation is missing."
          else if result == ComparisonResult.IncompleteLine then
            "The comparison is available, but its continuation evidence is incomplete."
          else if result == ComparisonResult.MaterialUnavailable then
            "The continuations are complete, but certified material evidence is unavailable."
          else "The provided lines have the same counts of captured and promoted piece roles; " +
            "those counts alone do not explain the choice."))
        base ++ material.map { (_, _, left, right) => Json.obj(
          "perspective" -> sideName(left.sideToMove),
          "reference_net_cp" -> left.observedNetCpForMover, "candidate_net_cp" -> right.observedNetCpForMover)
        }.getOrElse(Json.obj())

  private case class Candidate(id: String, content: Content, state: State, display: Display,
      rootOccurrence: String, occurrences: List[String], positions: List[String], lines: List[String],
      evidence: List[String], sources: List[Source]):
    def json: JsObject = Json.obj("id" -> id, "kind" -> content.kind,
      "state" -> state.wireId, "selection" -> display.wireId,
      "root_occurrence_id" -> rootOccurrence, "occurrence_ids" -> occurrences.distinct,
      "position_ids" -> positions.distinct, "line_ids" -> lines.distinct,
      "evidence_ids" -> evidence.distinct, "source_refs" -> sources.distinct.map(_.json),
      "detail" -> content.detail)

  final class Result private[CommentarySelection] (
      private val candidates: List[Candidate], private val scenes: List[JsObject],
      private[runtime] val local: List[MoveWorkProjection.LocalCandidate],
      val withMoveEffects: JsObject => JsObject
  ):
    lazy val highlights: List[JsObject] = local.filter(_.selection == Display.Selected).map(_.highlight)
    // Local notes are consumed through highlights. Their candidate wrappers (including
    // rejected display alternatives) have no reader or scene dependency. Keep the typed
    // facts for selection/research, but never construct a second public representation.
    lazy val json: JsObject = Json.obj("version" -> 1,
      "candidates" -> candidates.map(candidate => withMoveEffects(candidate.json)), "scenes" -> scenes)

  def fromPacket(packet: EvidenceBackedJudgmentPacket,
      local: List[MoveWorkProjection.LocalCandidate], prepared: ObservedLineProtocol.PreparedPaths,
      withMoveEffects: JsObject => JsObject): Result =
    prepared.requireOwner(packet)
    val ctx = packet.assembly
    val graph = packet.evidenceGraph
    val root = ctx.playedTransition.get
    val resources = prepared.resourceIds
    val suppliedByMoves = ctx.input.suppliedLines.groupBy(_.acceptedMoves)
    val evaluationsByMoves = ctx.input.evaluationObservations.groupBy(_.evaluation.moves)
    val comparisonRecords = packet.primary.toList.flatMap { primary =>
      val ref = primary match
        case PlayerFacingPrimary.MoveVerdict(value) => value
        case PlayerFacingPrimary.BestChoice(value) => value
      graph.candidateComparisonRecord(ref).toList.collect {
        case record @ EvidenceRecord(_, CandidateComparisonEvidence(fact), _) if graph.proofEligible(record) =>
          (ref, fact, record.parents)
      }
    }
    val comparedLines = comparisonRecords.flatMap((_, fact, _) => List(fact.referenceLine, fact.candidateLine)).toSet

    // Only scenes consume these wrappers. Other exact lines stay in observations.paths
    // and retain their summaries, sources and independent evidence there.
    val paths = prepared.paths.filter(path => comparedLines(path.line.ref)).map { path =>
        val line = path.line
        val moves = path.moves.map(EvidenceRef.normalizeMove)
        val sourceMoves = path.sourceMoves
        val positions = path.positions.flatMap(_.id)
        // The existing summary owner checks the entire exact path and all ledger events.
        // Its absence cannot be replaced by a score or by an equal final FEN.
        val certified = path.certified
        val material = certified.flatMap(_._2.material)
        val supplied = suppliedByMoves.getOrElse(sourceMoves, Nil).map(value =>
          Source(line.ref.id, "supplied", value.input.source.wireId, value.acceptedPlyCount, value.stop))
        val evaluated = evaluationsByMoves.getOrElse(sourceMoves, Nil).map { value =>
          val source = value.source match
            case EvaluationSource.DirectInput => "direct_input"
            case EvaluationSource.RootSearch => "root_search"
            case EvaluationSource.FocusComparison => "focus_comparison"
            case EvaluationSource.InvestigationComparison => "investigation_comparison"
            case EvaluationSource.ResponseComparison => "response_comparison"
          Source(line.ref.id, "evaluation", source, sourceMoves.size)
        }
        val sources = (supplied ++ evaluated) match
          case Nil => List(Source(line.ref.id, "admitted", "legal_replay", moves.size))
          case values => values.distinct
        val incompleteInput = sources.forall(_.stop.exists(_ != CommentaryLineStop.AutomaticTerminal))
        Path(line, path.ids, positions.filter(resources), moves, sources, certified.nonEmpty, material,
          path.ids == path.declared && positions.forall(resources) && !incompleteInput)
    }
    val pathsByLine = paths.map(path => path.line.ref -> path).toMap

    // This is the already selected actual/best (or best/runner-up) question.
    // No new candidate quality, all-legal comparison, or intent is inferred here.
    val comparisons = comparisonRecords.map { (ref, fact, parents) =>
          val reference = pathsByLine.get(fact.referenceLine)
          val candidate = pathsByLine.get(fact.candidateLine)
          val pair = List(reference, candidate)
          val result =
            if pair.exists(_.isEmpty) then ComparisonResult.MissingPath
            else if pair.flatten.exists(_.state == State.Partial) then ComparisonResult.IncompleteLine
            else if pair.flatten.exists(_.material.isEmpty) then ComparisonResult.MaterialUnavailable
            else if (for
              left <- reference.flatMap(_.signature)
              right <- candidate.flatMap(_.signature)
            yield left != right).contains(true) then ComparisonResult.Different
            else ComparisonResult.Equal
          val owned = pair.flatten
          Candidate(s"comparison:${ref.id}", Content.Comparison(ref, fact, reference, candidate, result),
            result.state, Display.Selected, root.evidence.id,
            root.evidence.id :: owned.flatMap(_.ids), owned.flatMap(_.positions),
            owned.map(_.line.ref.id), ref.id :: parents.map(_.id), owned.flatMap(_.sources))
    }
    val lines = paths.map { path =>
      Candidate(path.id, Content.Conditional(path), path.state, Display.Context, path.ids.head,
        path.ids, path.positions, List(path.line.ref.id),
        Option.when(path.certified)(path.line.evidence.id).toList, path.sources)
    }
    // One comparison and its exact path evidence form one candidate scene.
    // Ordinary local notes and every supplied exchange are not promoted to scenes.
    // Across a PGN, callers preserve this root ID; equal boards are never a merge key.
    val scenes = comparisons.map { comparison =>
      val context = lines.filter(line => comparison.lines.exists(line.lines.contains))
      Json.obj("id" -> s"scene:${comparison.id}", "root_occurrence_id" -> root.evidence.id,
        "occurrence_ids" -> comparison.occurrences.distinct,
        "line_ids" -> comparison.lines.distinct,
        "candidate_ids" -> (comparison.id :: context.map(_.id)), "state" -> comparison.state.wireId)
    }
    // The only cross-kind order is obligation first, exact context next, optional facts last.
    // Local display ranking stays in MoveWorkProjection; it is not a game-scene importance score.
    new Result(comparisons ++ lines, scenes, local, withMoveEffects)

  private[runtime] type MaterialSignature = (List[(String, String, Int)], List[(String, String, String, Int)])

  private[runtime] def materialSignature(material: LineMaterialSummary): MaterialSignature =
    val losses = material.captures.groupMapReduce(capture =>
      sideName(!capture.side) -> capture.capturedRole.name)(_ => 1)(_ + _).toList
      .map { case ((side, role), count) => (side, role, count) }.sortBy(row => (row._1, row._2))
    val promotions = material.events.filter(_.promotionGainCp > 0).groupMapReduce(event =>
      (sideName(event.movement.side), event.movement.beforeRole.name, event.movement.afterRole.name))(_ => 1)(_ + _)
      .toList.map { case ((side, before, after), count) => (side, before, after, count) }
      .sortBy(row => (row._1, row._2, row._3))
    losses -> promotions

  private def materialText(signature: MaterialSignature): String =
    val (losses, promotions) = signature
    val rows = List("white", "black").flatMap { side =>
      val pieces = losses.collect { case (`side`, role, count) =>
        if count == 1 then s"one $role" else s"$count ${role}s"
      }
      Option.when(pieces.nonEmpty)(s"${side.capitalize} loses ${pieces.mkString(" and ")}")
    } ++ promotions.map { (side, before, after, count) =>
      s"${side.capitalize} promotes $count ${if count == 1 then before else s"${before}s"} to ${if count == 1 then s"a $after" else s"${after}s"}"
    }
    if rows.isEmpty then "neither side loses material or promotes" else rows.mkString(", ")

  private def sideName(side: chess.Color): String = if side.white then "white" else "black"

  private[runtime] def stableId(kind: String, parts: List[String]): String =
    val input = parts.map(value => s"${value.length}:$value").mkString("|")
    val digest = java.util.HexFormat.of().formatHex(
      MessageDigest.getInstance("SHA-256").digest(input.getBytes(StandardCharsets.UTF_8)))
    s"$kind:$digest"
