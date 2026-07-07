package lila.chessjudgment.analysis.qc

import java.nio.charset.StandardCharsets
import java.nio.file.{ Files, Path }
import java.security.MessageDigest

import chess.format.pgn.PgnStr
import lila.core.game.Game
import lila.chessjudgment.analysis.assembly.{
  MoveReviewInputNormalizer,
  MoveReviewJudgmentOrchestrator,
  MoveReviewJudgmentResult,
  RawOpeningContext,
  RawMoveReviewInput
}
import lila.chessjudgment.analysis.line.PrincipalVariationEvidence
import lila.chessjudgment.analysis.opening.{ OpeningRecognitionIndex, OpeningThemePriorIndex }
import lila.chessjudgment.model.{ ProbePurpose, ProbeResult }
import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.model.strategic.VariationLine
import lila.tree.{ Branch, ExportOptions, ParseImport, Root, TreeBuilder }
import play.api.libs.json.*

private object RawOpeningContextJson:

  given Reads[RawOpeningContext] = Reads { json =>
    JsSuccess(
      RawOpeningContext(
        eco = (json \ "eco").asOpt[String].orElse((json \ "ecoCode").asOpt[String]),
        name = (json \ "name").asOpt[String].orElse((json \ "opening").asOpt[String]),
        family = (json \ "family").asOpt[String].orElse((json \ "ecoFamily").asOpt[String])
      )
    )
  }

  given Writes[RawOpeningContext] = Json.writes[RawOpeningContext]

  def from(json: JsValue): Option[RawOpeningContext] =
    merge(
      (json \ "openingContext").toOption.flatMap(_.validate[RawOpeningContext].asOpt),
      (json \ "openingIdentity").toOption.flatMap(_.validate[RawOpeningContext].asOpt),
      (json \ "opening").toOption.collect { case obj: JsObject => obj }.flatMap(_.validate[RawOpeningContext].asOpt),
      flatFields(json)
    )

  private def flatFields(json: JsValue): Option[RawOpeningContext] =
    merge(
      Some(
        RawOpeningContext(
          eco = (json \ "eco").asOpt[String].orElse((json \ "ecoCode").asOpt[String]),
          name = (json \ "opening").asOpt[String],
          family = (json \ "ecoFamily").asOpt[String].orElse((json \ "family").asOpt[String])
        )
      )
    )

  private def merge(contexts: Option[RawOpeningContext]*): Option[RawOpeningContext] =
    val available = contexts.flatten.toList
    val merged =
      RawOpeningContext(
        eco = firstText(available.flatMap(_.eco)),
        name = firstText(available.flatMap(_.name)),
        family = firstText(available.flatMap(_.family))
      )
    Option.when(merged.eco.nonEmpty || merged.name.nonEmpty || merged.family.nonEmpty)(merged)

  private def firstText(values: List[String]): Option[String] =
    values.map(_.trim).find(_.nonEmpty)

private object MoveReviewPgnInputSamples:
  private final case class ParsedPgnSample(
      sampleId: String,
      pgn: String,
      snapshots: List[PgnMoveEngineSnapshot],
      openingContext: Option[RawOpeningContext],
      expectedSemanticSlots: List[JsValue],
      expectedQuestionIds: List[String]
  )

  private final case class PgnMoveEngineSnapshot(
      sampleId: Option[String],
      beforePly: Int,
      variations: List[VariationLine],
      currentEvalCp: Option[Int],
      openingContext: Option[RawOpeningContext],
      expectedSemanticSlots: List[JsValue],
      expectedQuestionIds: List[String]
  )

  private[qc] final case class RawSampleRow(
      sampleId: String,
      raw: RawMoveReviewInput,
      expectedSemanticSlots: List[JsValue],
      expectedQuestionIds: List[String]
  )

  private final case class MainlineMove(
      beforePly: Int,
      beforeFen: String,
      playedMoveUci: String,
      movePrefixUci: List[String]
  )

  private given Reads[PgnMoveEngineSnapshot] = Reads { json =>
    for
      beforePly <- (json \ "beforePly").validateOpt[Int].flatMap:
        case Some(ply) => JsSuccess(ply)
        case None      => (json \ "ply").validate[Int]
      variations <- (json \ "variations").validate[List[VariationLine]]
    yield
      PgnMoveEngineSnapshot(
        sampleId = (json \ "sampleId").asOpt[String],
        beforePly = beforePly,
        variations = variations,
        currentEvalCp = (json \ "currentEvalCp").asOpt[Int],
        openingContext = RawOpeningContextJson.from(json),
        expectedSemanticSlots = (json \ "expectedSemanticSlots").asOpt[List[JsValue]].getOrElse(Nil),
        expectedQuestionIds = parseExpectedQuestionIdValues(json)
      )
  }

  private[qc] def rawSamples(path: Path): List[(String, RawMoveReviewInput)] =
    rawSampleRows(path).map(row => row.sampleId -> row.raw)

  private[qc] def rawSampleRows(path: Path): List[RawSampleRow] =
    parseSamples(path).flatMap(expandRawSample)

  private def parseExpectedQuestionIdValues(json: JsValue): List[String] =
    (json \ "expectedQuestionIds").asOpt[List[String]].getOrElse(Nil).map(_.trim).filter(_.nonEmpty).distinct.sorted

  private def parseSamples(path: Path): List[ParsedPgnSample] =
    MoveReviewQualityInputFiles.parseJsonDocuments(path).zipWithIndex.map { case (json, index) =>
      val sampleId = (json \ "sampleId").asOpt[String].getOrElse((index + 1).toString)
      val snapshotsJson =
        (json \ "positions").toOption
          .orElse((json \ "moves").toOption)
          .orElse((json \ "snapshots").toOption)
      val snapshots = snapshotsJson match
        case Some(value) =>
          value.validate[List[PgnMoveEngineSnapshot]] match
            case JsSuccess(value, _) => value
            case JsError(errors)     => throw IllegalArgumentException(s"invalid PGN sample $sampleId snapshots: $errors")
        case None => throw IllegalArgumentException(s"invalid PGN sample $sampleId: missing positions, moves, or snapshots")
      val pgn = (json \ "pgn").asOpt[String].getOrElse:
        throw IllegalArgumentException(s"invalid PGN sample $sampleId: missing pgn")
      ParsedPgnSample(
        sampleId = sampleId,
        pgn = pgn,
        snapshots = snapshots,
        openingContext = RawOpeningContextJson.from(json),
        expectedSemanticSlots = (json \ "expectedSemanticSlots").asOpt[List[JsValue]].getOrElse(Nil),
        expectedQuestionIds = parseExpectedQuestionIdValues(json)
      )
    }

  private def expandRawSample(sample: ParsedPgnSample): List[RawSampleRow] =
    val movesByPly = parseMainline(sample)
    sample.snapshots.map { snapshot =>
      val move = movesByPly.getOrElse(
        snapshot.beforePly,
        throw IllegalArgumentException(s"PGN sample ${sample.sampleId}: no played move at beforePly ${snapshot.beforePly}")
      )
      val raw = RawMoveReviewInput(
        fen = move.beforeFen,
        playedMoveUci = move.playedMoveUci,
        variations = snapshot.variations,
        currentEvalCp = snapshot.currentEvalCp,
        ply = Some(snapshot.beforePly),
        openingContext = snapshot.openingContext.orElse(sample.openingContext),
        movePrefixUci = move.movePrefixUci
      )
      RawSampleRow(
        sampleId = snapshot.sampleId.getOrElse(s"${sample.sampleId}:${snapshot.beforePly}"),
        raw = raw,
        expectedSemanticSlots = sample.expectedSemanticSlots ++ snapshot.expectedSemanticSlots,
        expectedQuestionIds = (sample.expectedQuestionIds ++ snapshot.expectedQuestionIds).distinct.sorted
      )
    }

  private def parseMainline(sample: ParsedPgnSample): Map[Int, MainlineMove] =
    val imported =
      ParseImport.full(PgnStr(sample.pgn)) match
        case Right(result) => result
        case Left(error)   => throw IllegalArgumentException(s"invalid PGN sample ${sample.sampleId}: ${error.value}")
    imported.replayError.foreach: error =>
      throw IllegalArgumentException(s"invalid PGN sample ${sample.sampleId} replay: ${error.value}")
    val initialFen = imported.initialFen.getOrElse(imported.game.variant.initialFen)
    val game = Game.make(imported.game.variant, imported.initialFen).copy(chess = imported.game)
    val root =
      TreeBuilder(
        game = game,
        analysis = None,
        initialFen = initialFen,
        withFlags = ExportOptions.default,
        logChessError = error => throw IllegalArgumentException(s"invalid PGN sample ${sample.sampleId} tree: $error")
      )
    mainlineMoves(root).map(move => move.beforePly -> move).toMap

  private def mainlineMoves(root: Root): List[MainlineMove] =
    def loop(
        beforeFen: String,
        beforePly: Int,
        branches: List[Branch],
        prefix: List[String],
        acc: List[MainlineMove]
    ): List[MainlineMove] =
      branches match
        case Nil => acc.reverse
        case branch :: rest =>
          val played = branch.move.uci.uci
          val move =
            MainlineMove(
              beforePly = beforePly,
              beforeFen = beforeFen,
              playedMoveUci = played,
              movePrefixUci = prefix
            )
          loop(branch.fen.value, branch.ply.value, rest, prefix :+ played, move :: acc)
    loop(root.fen.value, root.ply.value, root.mainline, Nil, Nil)

object MoveReviewPgnRawInputRunner:
  import RawOpeningContextJson.given

  private given Writes[RawMoveReviewInput] = Json.writes[RawMoveReviewInput]

  def main(args: Array[String]): Unit =
    if args.isEmpty then
      throw IllegalArgumentException("usage: MoveReviewPgnRawInputRunner <input.json|jsonl> [output.jsonl]")
    val inputPath = Path.of(args(0))
    val outputPath = args.lift(1).map(Path.of(_))
    val lines =
      MoveReviewPgnInputSamples.rawSampleRows(inputPath).map { sample =>
        val base = Json.obj("sampleId" -> sample.sampleId, "input" -> Json.toJson(sample.raw))
        val row =
          if sample.expectedSemanticSlots.nonEmpty then
            base ++ Json.obj("expectedSemanticSlots" -> JsArray(sample.expectedSemanticSlots))
          else base
        val rowWithQuestions =
          if sample.expectedQuestionIds.nonEmpty then
            row ++ Json.obj("expectedQuestionIds" -> sample.expectedQuestionIds)
          else row
        Json.stringify(rowWithQuestions)
      }
    val output = lines.mkString(System.lineSeparator())
    outputPath match
      case Some(path) => Files.writeString(path, output, StandardCharsets.UTF_8)
      case None       => println(output)

object MoveReviewPhase3AuditRunner:
  import RawOpeningContextJson.given
  import MoveReviewPhase3AuditContract.{
    expectedSemanticSlotsJson,
    parseExpectedQuestionIds,
    parseExpectedSemanticSlots
  }

  private[qc] type AuditInputSample = MoveReviewPhase3AuditContract.AuditInputSample
  private[qc] val AuditInputSample = MoveReviewPhase3AuditContract.AuditInputSample
  private[qc] type ExpectedSemanticSlot = MoveReviewPhase3AuditContract.ExpectedSemanticSlot
  private[qc] val ExpectedSemanticSlot = MoveReviewPhase3AuditContract.ExpectedSemanticSlot

  private val SlimOutputFlag = "--slim"

  private given Writes[RawMoveReviewInput] = Json.writes[RawMoveReviewInput]

  private given Reads[RawMoveReviewInput] = Reads { json =>
    for
      fen <- (json \ "fen").validate[String]
      playedMoveUci <- (json \ "playedMoveUci").validate[String]
      variations <- (json \ "variations").validate[List[VariationLine]]
    yield
      RawMoveReviewInput(
        fen = fen,
        playedMoveUci = playedMoveUci,
        variations = variations,
        currentEvalCp = (json \ "currentEvalCp").asOpt[Int],
        ply = (json \ "ply").asOpt[Int],
        openingContext = RawOpeningContextJson.from(json),
        movePrefixUci = (json \ "movePrefixUci").asOpt[List[String]].getOrElse(Nil),
        probeResults = (json \ "probeResults").asOpt[List[ProbeResult]].getOrElse(Nil)
      )
  }

  def main(args: Array[String]): Unit =
    val slimOutput = args.contains(SlimOutputFlag)
    val pathArgs = args.filterNot(_ == SlimOutputFlag)
    if pathArgs.isEmpty then
      throw IllegalArgumentException(s"usage: MoveReviewPhase3AuditRunner <input.json|jsonl> [output.jsonl] [$SlimOutputFlag]")
    val inputPath = Path.of(pathArgs(0))
    val outputPath = pathArgs.lift(1).map(Path.of(_))
    val parsedSamples =
      MoveReviewQualityInputFiles.parseJsonDocuments(inputPath).zipWithIndex.map { case (json, index) =>
        normalizeExpectedSemanticSlots(parseSample(json, index))
      }
    val explicitQuestionIdsByFingerprint =
      parsedSamples
        .groupMap(sample => inputFingerprint(sample.raw))(_.expectedSemanticSlots.flatMap(_.questionId))
        .view
        .mapValues(_.flatten.toSet)
        .toMap
    val samples =
      parsedSamples.map { sample =>
        val explicitQuestionIds = explicitQuestionIdsByFingerprint.getOrElse(inputFingerprint(sample.raw), Set.empty)
        val inferredSlots =
          inferredExpectedSemanticSlots(sample).filterNot(slot =>
            sample.expectedSemanticSlots.exists(_.id == slot.id) ||
              slot.questionId.exists(explicitQuestionIds)
          )
        sample.copy(expectedSemanticSlots = sample.expectedSemanticSlots ++ inferredSlots)
      }
    outputPath.foreach(path => writeReplayInputArchive(path, samples))
    val fingerprints = samples.map(sample => inputFingerprint(sample.raw))
    val duplicateCounts = fingerprints.groupBy(identity).view.mapValues(_.size).toMap
    var seen = Map.empty[String, Int]
    val rows = samples.zip(fingerprints).map { case (sample, fingerprint) =>
      () =>
        val ordinal = seen.getOrElse(fingerprint, 0) + 1
        seen = seen.updated(fingerprint, ordinal)
        auditSample(
          sample = sample,
          inputFingerprint = fingerprint,
          inputDuplicateOrdinal = ordinal,
          inputDuplicateCount = duplicateCounts.getOrElse(fingerprint, 1),
          slimOutput = slimOutput
        )
    }
    outputPath match
      case Some(path) => writeAuditRowsWithSummary(path, rows.iterator.map(_.apply()))
      case None       => rows.foreach(row => println(Json.stringify(row())))

  private[qc] def writeReplayInputArchive(outputPath: Path, samples: List[AuditInputSample]): Path =
    val path = replayInputArchivePath(outputPath)
    val rows = samples.iterator.map(sample =>
      Json.obj(
        "schemaVersion" -> "move_review_phase3_replay_input.v1",
        "sampleId" -> sample.sampleId,
        "input" -> Json.toJson(sample.raw),
        "opening" -> sample.opening,
        "sliceKind" -> sample.sliceKind,
        "targetPly" -> sample.targetPly,
        "playedSan" -> sample.playedSan,
        "expectedSemanticSlots" -> expectedSemanticSlotsJson(sample.expectedSemanticSlots),
        "expectedQuestionIds" -> sample.expectedQuestionIds
      )
    )
    writeJsonLines(path, rows)
    path

  private def replayInputArchivePath(outputPath: Path): Path =
    val fileName = outputPath.getFileName.toString
    val replayFileName =
      if fileName.contains("phase3_audit_output") then
        fileName.replaceFirst("phase3_audit_output", "phase3_audit_input_replay")
      else
        val extensionStart = fileName.lastIndexOf('.')
        if extensionStart > 0 then
          s"${fileName.take(extensionStart)}.input_replay${fileName.drop(extensionStart)}"
        else s"$fileName.input_replay.jsonl"
    Option(outputPath.getParent).map(_.resolve(replayFileName)).getOrElse(Path.of(replayFileName))

  private final case class ExpectedSlotSpec(
      id: String,
      unit: PositionPlanTechniqueUnit,
      axisKey: Option[String],
      requiredSupportLevel: String = "view_surfaced",
      tokens: List[String] = Nil
  ):
    def toSlot(moveUci: String, questionId: String): ExpectedSemanticSlot =
      ExpectedSemanticSlot(
        id = id,
        unit = unit,
        axisKey = axisKey,
        lineRole = Some("candidate"),
        moveUci = Some(moveUci),
        questionId = Some(questionId),
        requiredSupportLevel = Some(requiredSupportLevel),
        requiredSemanticDetailTokens = tokens
      )

  private def spec(
      id: String,
      unit: PositionPlanTechniqueUnit,
      axisKey: String,
      requiredSupportLevel: String = "view_surfaced",
      tokens: List[String] = Nil
  ): ExpectedSlotSpec =
    ExpectedSlotSpec(id, unit, Some(axisKey), requiredSupportLevel, tokens)

  private val InferredExpectedSlotSpecs: Map[String, List[ExpectedSlotSpec]] =
    Map(
      "terminal_mate" -> List(
        ExpectedSlotSpec("terminal_mate_proof", PositionPlanTechniqueUnit.StructuralTransformation, None)
      ),
      "terminal_promotion_race" -> List(
        ExpectedSlotSpec(
          "terminal_promotion_race_proof",
          PositionPlanTechniqueUnit.StructuralTransformation,
          None,
          requiredSupportLevel = "owned_cause_linked",
          tokens = List("terminalConsequenceKind:PromotionRace")
        )
      ),
      "terminal_only_defense" -> List(
        ExpectedSlotSpec(
          "terminal_only_defense_resource",
          PositionPlanTechniqueUnit.PieceRerouteRoute,
          None,
          requiredSupportLevel = "owned_cause_linked"
        )
      ),
      "carrier_coverage_dual_purpose_defensive_resource" -> List(
        spec("qg4_target_pressure_carrier", PositionPlanTechniqueUnit.StructuralTransformation, "Target:Support:TargetFixation"),
        spec("qg4_queen_route_carrier", PositionPlanTechniqueUnit.PieceRerouteRoute, "Activity:Gain:activity-gain", tokens = List("route"))
      ),
      "pawn_lure_counterplay_chain" -> List(
        spec("nbd7_knight_route_carrier", PositionPlanTechniqueUnit.PieceRerouteRoute, "Activity:Gain:activity-gain", tokens = List("route")),
        spec("nbd7_target_fixation_carrier", PositionPlanTechniqueUnit.StructuralTransformation, "Target:Support:TargetFixation")
      ),
      "carrier_coverage_queen_interposition_defense" -> List(
        spec("qe8_queen_route_carrier", PositionPlanTechniqueUnit.PieceRerouteRoute, "Activity:Gain:activity-gain", tokens = List("route")),
        spec("qe8_target_pressure_carrier", PositionPlanTechniqueUnit.StructuralTransformation, "Target:Gain:target-pressure-gain")
      ),
      "carrier_coverage_open_diagonal_pawn_sacrifice" -> List(
        spec(
          "d5_pawn_break_carrier",
          PositionPlanTechniqueUnit.TensionBreakPolicyRoute,
          "PawnBreak:Support:break-file-d-created-tension-d5-e6"
        ),
        spec(
          "d5_diagonal_route_carrier",
          PositionPlanTechniqueUnit.PieceRerouteRoute,
          "Activity:Gain:activity-gain",
          tokens = List("diagonal", "route")
        )
      ),
      "blockade_conversion_piece_sacrifice" -> List(
        spec("nxc5_piece_route_carrier", PositionPlanTechniqueUnit.PieceRerouteRoute, "Activity:Gain:activity-gain", tokens = List("route")),
        spec(
          "nxc5_counterplay_restraint_carrier",
          PositionPlanTechniqueUnit.SpacePreventionResourceDenial,
          "Counterplay:Restrain:opponent-low-mobility"
        )
      ),
      "exchange_sacrifice_color_complex" -> List(
        spec("rxe3_rook_route_carrier", PositionPlanTechniqueUnit.StructuralTransformation, "Target:Support:TargetFixation"),
        spec(
          "rxe3_counterplay_square_carrier",
          PositionPlanTechniqueUnit.SpacePreventionResourceDenial,
          "Counterplay:Restrain:defensive-counter-break-b"
        )
      ),
      "discovered_attack_line_clearance_queen" -> List(
        spec("bb5_bishop_forcing_route_carrier", PositionPlanTechniqueUnit.PieceRerouteRoute, "Activity:Gain:activity-gain", tokens = List("route"))
      ),
      "quiet_pressure_b6_d5" -> List(
        spec("be3_bishop_route_carrier", PositionPlanTechniqueUnit.PieceRerouteRoute, "Activity:Gain:activity-gain", tokens = List("route")),
        spec("be3_b6_pressure_carrier", PositionPlanTechniqueUnit.StructuralTransformation, "Target:Support:TargetFixation")
      ),
      "carrier_coverage_clear_diagonal_tempo_fork" -> List(
        spec(
          "c6_queen_diagonal_route_carrier",
          PositionPlanTechniqueUnit.PieceRerouteRoute,
          "Activity:Gain:activity-gain",
          tokens = List("diagonal", "route")
        )
      ),
      "passed_pawn_carrier_object_binding" -> List(
        ExpectedSlotSpec("passed_pawn_object_carrier", PositionPlanTechniqueUnit.PlanOptionSet, None)
      ),
      "pawn_sacrifice_square_access" -> List(
        spec("c4_square_access_target_carrier", PositionPlanTechniqueUnit.StructuralTransformation, "Target:Gain:weak-pawn-target")
      ),
      "queenside_break_b_file_target" -> List(
        ExpectedSlotSpec(
          "b5_b_file_target_carrier",
          PositionPlanTechniqueUnit.StructuralTransformation,
          None,
          requiredSupportLevel = "owned_cause_linked"
        )
      )
    )

  private def inferredExpectedSemanticSlots(sample: AuditInputSample): List[ExpectedSemanticSlot] =
    sample.expectedQuestionIds.flatMap(questionId =>
      InferredExpectedSlotSpecs.getOrElse(questionId, Nil).map(_.toSlot(sample.raw.playedMoveUci, questionId))
    )

  private def normalizeExpectedSemanticSlots(sample: AuditInputSample): AuditInputSample =
    sample.copy(expectedSemanticSlots =
      sample.expectedSemanticSlots.map {
        case slot if slot.id == "qe8_target_pressure_carrier" =>
          slot.copy(axisKey = Some("Target:Gain:target-pressure-gain"))
        case slot if slot.id == "b5_b_file_target_carrier" =>
          slot.copy(axisKey = None)
        case slot if slot.id == "terminal_only_defense_resource" && slot.questionId.contains("terminal_only_defense") =>
          slot.copy(unit = PositionPlanTechniqueUnit.PieceRerouteRoute)
        case slot => slot
      }
    )

  private def writeAuditRowsWithSummary(path: Path, rows: Iterator[JsObject]): Unit =
    val coverages = scala.collection.mutable.ListBuffer.empty[JsValue]
    val writer = Files.newBufferedWriter(path, StandardCharsets.UTF_8)
    try
      var first = true
      rows.foreach: row =>
        if first then first = false else writer.newLine()
        writer.write(Json.stringify(row))
        semanticRubricCoverageFromAuditRow(row).foreach(coverages += _)
    finally writer.close()
    writeAuditSummary(summaryOutputPath(path), coverages.toList)

  private def semanticRubricCoverageFromAuditRow(row: JsObject): Option[JsValue] =
    (row \ "semanticCoverage" \ "semanticRubricExpectedSlotCoverage").toOption
      .orElse((row \ "semanticRubricExpectedSlotCoverage").toOption)

  private def writeAuditSummary(path: Path, coverages: List[JsValue]): Unit =
    Files.writeString(
      path,
      Json.prettyPrint(
        Json.obj(
          "schemaVersion" -> "move_review_phase3_audit_summary.v1",
          "semanticRubricExpectedSlotCorpusCoverage" -> semanticRubricExpectedSlotCorpusCoverageJson(coverages)
        )
      ),
      StandardCharsets.UTF_8
    )

  private def summaryOutputPath(outputPath: Path): Path =
    val fileName = outputPath.getFileName.toString
    val summaryFileName =
      val baseName =
        if fileName.contains("phase3_audit_output") then
          fileName.replaceFirst("phase3_audit_output", "phase3_audit_summary")
        else fileName
      if baseName.endsWith(".jsonl") then baseName.stripSuffix(".jsonl") + ".json"
      else if baseName.endsWith(".json") then baseName.stripSuffix(".json") + ".summary.json"
      else baseName + ".summary.json"
    Option(outputPath.getParent).map(_.resolve(summaryFileName)).getOrElse(Path.of(summaryFileName))

  private def writeJsonLines(path: Path, rows: Iterator[JsValue]): Unit =
    val writer = Files.newBufferedWriter(path, StandardCharsets.UTF_8)
    try
      var first = true
      rows.foreach: row =>
        if first then first = false else writer.newLine()
        writer.write(Json.stringify(row))
    finally writer.close()

  private def parseSample(json: JsValue, index: Int): AuditInputSample =
    parseDirectRaw(json, index) match
      case Some(sample) => sample
      case None =>
        val sampleId = (json \ "sampleId").asOpt[String].getOrElse((index + 1).toString)
        throw IllegalArgumentException(
          s"invalid phase3 sample $sampleId: expected RawMoveReviewInput"
        )

  private def parseDirectRaw(json: JsValue, index: Int): Option[AuditInputSample] =
    val sampleId = (json \ "sampleId").asOpt[String].getOrElse((index + 1).toString)
    val body = (json \ "input").toOption.getOrElse(json)
    body.validate[RawMoveReviewInput] match
      case JsSuccess(raw, _) =>
        val enrichedRaw =
          raw.copy(openingContext = raw.openingContext.orElse(RawOpeningContextJson.from(json)))
        Some(
          AuditInputSample(
            sampleId = sampleId,
            raw = enrichedRaw,
            opening = (json \ "opening").asOpt[String].orElse(enrichedRaw.openingContext.flatMap(_.name)),
            sliceKind = (json \ "sliceKind").asOpt[String],
            targetPly = (json \ "targetPly").asOpt[Int].orElse(enrichedRaw.ply),
            playedSan = (json \ "playedSan").asOpt[String],
            expectedSemanticSlots = parseExpectedSemanticSlots(json),
            expectedQuestionIds = parseExpectedQuestionIds(json)
          )
        )
      case JsError(_) => None

  private def auditSample(
      sample: AuditInputSample,
      inputFingerprint: String,
      inputDuplicateOrdinal: Int,
      inputDuplicateCount: Int,
      slimOutput: Boolean
  ): JsObject =
    val result = MoveReviewJudgmentOrchestrator.build(sample.raw)
    val base =
      Json.obj(
        "schemaVersion" -> "move_review_phase3_audit_row.v2",
        "sampleId" -> sample.sampleId,
        "opening" -> sample.opening,
        "sliceKind" -> sample.sliceKind,
        "targetPly" -> sample.targetPly,
        "fen" -> sample.raw.fen,
        "playedSan" -> sample.playedSan,
        "playedUci" -> sample.raw.playedMoveUci,
        "inputFingerprint" -> inputFingerprint,
        "inputDuplicateOrdinal" -> inputDuplicateOrdinal,
        "inputDuplicateCount" -> inputDuplicateCount,
        "inputDuplicate" -> (inputDuplicateCount > 1),
        "rawInputAudit" -> rawInputAudit(sample.raw),
        "expectedSemanticSlots" -> expectedSemanticSlotsJson(sample.expectedSemanticSlots),
        "expectedQuestionIds" -> sample.expectedQuestionIds,
        "manualReview" -> Json.obj(
          "status" -> "required",
          "openingRead" -> JsNull,
          "positionRead" -> JsNull,
          "expectedPrimaryTheme" -> JsNull,
          "expectedVerdict" -> JsNull,
          "graphGapKind" -> JsNull,
          "notes" -> JsNull
        )
      )
    result match
      case Some(built) =>
        val publicSurface =
          built.packet.moveJudgmentView.fold(Json.obj())(MoveReviewPhase3AuditViewJson.moveMeaningSurfacePayloadJson)
        val publicIdeaChainCount = (publicSurface \ "idea_chains").asOpt[JsArray].fold(0)(_.value.size)
        val slotCoverage =
          semanticRubricExpectedSlotCoverageJson(
            sample.expectedSemanticSlots,
            built.quality.semanticCoverage.comparisonDiagnostics,
            expectedQuestionIds = sample.expectedQuestionIds
          ) ++ Json.obj("publicIdeaChainCount" -> publicIdeaChainCount)
        val common =
          Json.obj(
            "auditMode" -> (if slimOutput then "slim" else "full"),
            "buildStatus" -> "built",
            "lineSummary" -> lineSummary(built),
            "relativeAssessment" -> relativeSummary(built),
            "semanticCoverage" -> semanticSummary(built, slimOutput, slotCoverage),
            "qcFailureSummary" -> qcFailureSummary(built),
            "layerGaps" -> layerGapSummary(built),
            "issueKinds" -> Json.toJson(built.quality.audit.issues.map(_.kind.toString)),
            "validationIssueKinds" -> Json.toJson(built.quality.validation.issues.map(_.kind.toString)),
            "issues" -> issueDetails(built),
            "validationIssues" -> validationIssueDetails(built),
            "evidenceLoss" -> evidenceLossSummary(built),
            "evidenceLayerCounts" -> evidenceLayerCounts(built),
            "relationKinds" -> relationKinds(built),
            "strategicKinds" -> strategicKinds(built),
            "probeRequests" -> probeRequestSummary(built),
            "probeDiagnostics" -> probeDiagnosticsSummary(built)
          )
        val fullDiagnostics =
          Json.obj(
            "claimSupportClusters" -> claimSupportClusters(built),
            "claimEventClusters" -> claimEventClusters(built),
            "moveJudgmentView" -> moveJudgmentView(built, publicSurface),
            "rankedPrimaryClaimDiagnostics" -> rankedClaimDiagnosticsByTier(built, PlayerFacingClaimTier.Primary),
            "rankedSecondaryClaimDiagnostics" -> rankedClaimDiagnosticsByTier(built, PlayerFacingClaimTier.Secondary),
            "rankedContextClaimDiagnostics" -> rankedClaimDiagnosticsByTier(built, PlayerFacingClaimTier.Context),
            "rankedDiagnosticClaimDiagnostics" -> rankedClaimDiagnosticsByTier(built, PlayerFacingClaimTier.Diagnostic)
          )
        base ++ common ++ (if slimOutput then Json.obj() else fullDiagnostics)
      case None =>
        base ++ Json.obj(
          "auditMode" -> (if slimOutput then "slim" else "full"),
          "buildStatus" -> "failed",
          "lineSummary" -> Json.obj(
            "inputVariationCount" -> sample.raw.variations.size,
            "inputRootMoves" -> sample.raw.variations.flatMap(_.moves.headOption)
          )
        )

  private def inputFingerprint(raw: RawMoveReviewInput): String =
    val key =
      List(
        raw.fen.trim,
        raw.playedMoveUci.trim,
        raw.ply.map(_.toString).getOrElse(""),
        raw.currentEvalCp.map(_.toString).getOrElse(""),
        raw.variations
          .map(line =>
            List(
              line.scoreCp.toString,
              line.mate.map(_.toString).getOrElse(""),
              line.depth.toString,
              line.moves.mkString(",")
            ).mkString(":")
          )
          .mkString("|"),
        raw.probeResults
          .map(probe =>
            List(
              probe.id,
              probe.purpose.map(_.key).getOrElse(""),
              probe.fen.getOrElse(""),
              probe.probedMove.orElse(probe.candidateMove).getOrElse(""),
              probe.replyLines
                .getOrElse(Nil)
                .map(line =>
                  List(
                    line.scoreCp.toString,
                    line.mate.map(_.toString).getOrElse(""),
                    line.depth.toString,
                    line.moves.mkString(",")
                  ).mkString(":")
                )
                .mkString("|")
            ).mkString(":")
          )
          .mkString("||")
      ).mkString("||")
    val digest = MessageDigest.getInstance("SHA-256").digest(key.getBytes(StandardCharsets.UTF_8))
    digest.map(byte => f"${byte & 0xff}%02x").mkString

  private def lineSummary(result: MoveReviewJudgmentResult): JsObject =
    def line(role: lila.chessjudgment.model.judgment.LineNodeRole): Option[JsObject] =
      result.packet.candidateLines.find(_.role == role).map(node =>
        Json.obj(
          "role" -> node.role.toString,
          "rootMove" -> node.ref.rootMove,
          "rank" -> node.ref.rank,
          "whitePovEvalCp" -> node.whitePovEvalCp,
          "mate" -> node.mate,
          "depth" -> node.depth,
          "moves" -> node.line.moves
        )
      )
    Json.obj(
      "candidateCount" -> result.packet.candidateLines.size,
      "reference" -> line(lila.chessjudgment.model.judgment.LineNodeRole.BestReference),
      "played" -> line(lila.chessjudgment.model.judgment.LineNodeRole.Played),
      "alternatives" -> JsArray(
        result.packet.candidateLines
          .filter(_.role == lila.chessjudgment.model.judgment.LineNodeRole.Alternative)
          .map(node =>
            Json.obj(
              "rootMove" -> node.ref.rootMove,
              "rank" -> node.ref.rank,
              "whitePovEvalCp" -> node.whitePovEvalCp,
              "mate" -> node.mate,
              "depth" -> node.depth
            )
          )
      )
    )

  private def probeRequestSummary(result: MoveReviewJudgmentResult): JsObject =
    val requests = result.packet.probeRequests
    Json.obj(
      "count" -> requests.size,
      "purposes" -> requests.flatMap(_.purpose.map(_.key)).distinct.sorted,
      "candidateMoves" -> requests.flatMap(_.candidateMove).distinct.sorted,
      "multiPv" -> requests.flatMap(_.multiPv).distinct.sorted,
      "depthFloors" -> requests.flatMap(_.depthFloor).distinct.sorted,
      "requiredSignals" -> requests.flatMap(_.requiredSignals).distinct.sorted,
      "requests" -> requests.take(5).map(request =>
        Json.obj(
          "id" -> request.id,
          "purpose" -> request.purpose.map(_.key),
          "candidateMove" -> request.candidateMove,
          "fen" -> request.fen,
          "multiPv" -> request.multiPv,
          "depth" -> request.depth,
          "depthFloor" -> request.depthFloor,
          "requiredSignals" -> request.requiredSignals,
          "variationHash" -> request.variationHash
        )
      )
    )

  private def probeDiagnosticsSummary(result: MoveReviewJudgmentResult): JsObject =
    val diagnostics = result.packet.probeDiagnostics
    Json.obj(
      "count" -> diagnostics.size,
      "statuses" -> diagnostics.groupMapReduce(_.status.toString)(_ => 1)(_ + _),
      "reasonCodes" -> diagnostics.flatMap(_.reasonCodes).groupMapReduce(identity)(_ => 1)(_ + _),
      "diagnostics" -> diagnostics.take(10).map(diagnostic =>
        Json.obj(
          "probeId" -> diagnostic.probeId,
          "status" -> diagnostic.status.toString,
          "reasonCodes" -> diagnostic.reasonCodes,
          "purpose" -> diagnostic.purpose.map(_.key),
          "candidateMove" -> diagnostic.candidateMove,
          "fen" -> diagnostic.fen,
          "admittedLineCount" -> diagnostic.admittedLineCount,
          "legalLineCount" -> diagnostic.legalLineCount,
          "scoredLineCount" -> diagnostic.scoredLineCount,
          "depthFloor" -> diagnostic.depthFloor,
          "variationHash" -> diagnostic.variationHash
        )
      )
    )

  private def relativeSummary(result: MoveReviewJudgmentResult): JsValue =
    result.packet.relativeAssessments.headOption match
      case Some(assessment) =>
        val comparison = assessment.comparison
        Json.obj(
          "verdict" -> comparison.verdict.toString,
          "mover" -> comparison.mover.name,
          "rawCpLossForDiagnostics" -> comparison.rawCpLossForDiagnostics,
          "winPercentLossForMover" -> comparison.winPercentLossForMover,
          "rawCandidateDeltaCpForDiagnostics" -> comparison.rawCandidateDeltaCpForDiagnostics,
          "candidateWinPercentDeltaForMover" -> comparison.candidateWinPercentDeltaForMover,
          "rawBestToSecondCpGapForDiagnostics" -> comparison.candidateSet.flatMap(_.rawBestToSecondCpGapForDiagnostics),
          "bestToSecondWinPercentGapForMover" -> comparison.candidateSet.flatMap(_.bestToSecondWinPercentGapForMover),
          "candidateCount" -> comparison.candidateSet.map(_.candidateCount),
          "onlyMove" -> comparison.candidateSet.exists(_.onlyMove)
        )
      case None => JsNull

  private def semanticSummary(
      result: MoveReviewJudgmentResult,
      slimOutput: Boolean,
      slotCoverage: JsObject
  ): JsObject =
    val semantic = result.quality.semanticCoverage
    if slimOutput then
      Json.obj(
        "tacticalIdeas" -> semantic.tacticalIdeas,
        "strategicIdeas" -> semantic.strategicIdeas,
        "pawnStructureIdeas" -> semantic.pawnStructureIdeas,
        "openingIdeas" -> semantic.openingIdeas,
        "defensiveIdeas" -> semantic.defensiveIdeas,
        "evaluationIdeas" -> semantic.evaluationIdeas,
        "conversionIdeas" -> semantic.conversionIdeas,
        "materialIdeas" -> semantic.materialIdeas,
        "claimFamilies" -> semantic.claimFamilies.map(_.toString).toList.sorted,
        "claimCandidateFamilies" -> semantic.claimCandidateFamilies.map(_.toString).toList.sorted,
        "claimCandidateFamilyCounts" -> claimLifecycleCountsJson(semantic.claimCandidateFamilyCounts),
        "claimLifecycleStageCounts" -> claimLifecycleCountsJson(semantic.claimLifecycleStageCounts),
        "claimLifecycleTruthCounts" -> claimLifecycleCountsJson(semantic.claimLifecycleTruthCounts),
        "hasRelativeAssessment" -> semantic.hasRelativeAssessment,
        "candidateComparisonFacts" -> semantic.candidateComparisonFacts,
        "relativeCauseFacts" -> semantic.relativeCauseFacts,
        "moveVerdictCertifications" -> semantic.moveVerdictCertifications,
        "playedRelatedComparisonFacts" -> semantic.playedRelatedComparisonFacts,
        "primaryPlayedComparisonFacts" -> semantic.primaryPlayedComparisonFacts,
        "playedRelativeCauseFacts" -> semantic.playedRelativeCauseFacts,
        "hasVerdict" -> semantic.hasVerdict,
        "hasCandidateSetComparison" -> semantic.hasCandidateSetComparison,
        "hasOnlyMoveSignal" -> semantic.hasOnlyMoveSignal,
        "hasForcedLineTheme" -> semantic.hasForcedLineTheme,
        "semanticRubricExpectedSlotCoverage" -> slotCoverage
      )
    else Json.obj(
      "tacticalIdeas" -> semantic.tacticalIdeas,
      "strategicIdeas" -> semantic.strategicIdeas,
      "pawnStructureIdeas" -> semantic.pawnStructureIdeas,
      "openingIdeas" -> semantic.openingIdeas,
      "defensiveIdeas" -> semantic.defensiveIdeas,
      "evaluationIdeas" -> semantic.evaluationIdeas,
      "conversionIdeas" -> semantic.conversionIdeas,
      "materialIdeas" -> semantic.materialIdeas,
      "claimFamilies" -> semantic.claimFamilies.map(_.toString).toList.sorted,
      "claimCandidateFamilies" -> semantic.claimCandidateFamilies.map(_.toString).toList.sorted,
      "claimCandidateFamilyCounts" -> claimLifecycleCountsJson(semantic.claimCandidateFamilyCounts),
      "claimLifecycleStageCounts" -> claimLifecycleCountsJson(semantic.claimLifecycleStageCounts),
      "claimLifecycleTruthCounts" -> claimLifecycleCountsJson(semantic.claimLifecycleTruthCounts),
      "claimLifecycleRelativeCauseIdeaDroppedIds" -> semantic.claimLifecycleRelativeCauseIdeaDroppedIds,
      "claimLifecycleRelativeCauseDroppedByStage" -> claimLifecycleDroppedByStageJson(
        semantic.claimLifecycleRelativeCauseDroppedByStage
      ),
      "claimLifecycleDiagnostics" -> claimLifecycleDiagnosticsJson(semantic.claimLifecycleDiagnostics),
      "claimSupportClusters" -> semantic.claimSupportClusters,
      "clusteredAnchorClaims" -> semantic.clusteredAnchorClaims,
      "clusteredSupportingClaims" -> semantic.clusteredSupportingClaims,
      "clusteredConstrainingClaims" -> semantic.clusteredConstrainingClaims,
      "clusteredLongTermSupportIdeas" -> semantic.clusteredLongTermSupportIdeas,
      "claimSupportClusterFamilies" -> semantic.claimSupportClusterFamilies.map(_.toString).toList.sorted,
      "claimSupportClusterLayers" -> semantic.claimSupportClusterLayers.map(_.toString).toList.sorted,
      "claimEventClusters" -> semantic.claimEventClusters,
      "clusteredEventClaims" -> semantic.clusteredEventClaims,
      "clusteredEventCauseClaims" -> semantic.clusteredEventCauseClaims,
      "clusteredEventEvaluationClaims" -> semantic.clusteredEventEvaluationClaims,
      "clusteredEventWitnessClaims" -> semantic.clusteredEventWitnessClaims,
      "clusteredEventIdeas" -> semantic.clusteredEventIdeas,
      "relatedEventSupportClusters" -> semantic.relatedEventSupportClusters,
      "claimEventClusterFamilies" -> semantic.claimEventClusterFamilies.map(_.toString).toList.sorted,
      "claimEventClusterLayers" -> semantic.claimEventClusterLayers.map(_.toString).toList.sorted,
      "claimEventClusterKinds" -> semantic.claimEventClusterKinds.map(_.toString).toList.sorted,
      "claimEventClusterCauses" -> semantic.claimEventClusterCauses.map(_.toString).toList.sorted,
      "claimEventClusterRoles" -> semantic.claimEventClusterRoles.map(_.toString).toList.sorted,
      "claimEventInteractionKinds" -> semantic.claimEventInteractionKinds.map(_.toString).toList.sorted,
      "unclusteredEventClaims" -> semantic.unclusteredEventClaims,
      "unclusteredEventClaimFamilies" -> semantic.unclusteredEventClaimFamilies.map(_.toString).toList.sorted,
      "verdictCarriersWithoutEventCauseOwner" -> semantic.verdictCarriersWithoutEventCauseOwner,
      "verdictOnlyEvaluationClaims" -> semantic.verdictOnlyEvaluationClaims,
      "localConcreteClaims" -> semantic.localConcreteClaims,
      "localConcreteClaimFamilies" -> semantic.localConcreteClaimFamilies.map(_.toString).toList.sorted,
      "localConcreteClaimDetails" -> localConcreteClaimDetails(semantic.localConcreteClaimDiagnostics),
      "boardAnchorFacts" -> semantic.boardAnchorFacts,
      "boardAttackDefenseFacts" -> semantic.boardAttackDefenseFacts,
      "lineReplayFacts" -> semantic.lineReplayFacts,
      "lineEventFacts" -> semantic.lineEventFacts,
      "lineConsequenceFacts" -> semantic.lineConsequenceFacts,
      "relativeCauseProofs" -> semantic.relativeCauseProofs,
      "strategicAxisIntegrity" -> strategicAxisIntegritySummary(result.packet),
      "objectBindingIntegrity" -> objectBindingIntegritySummary(result.packet),
      "hasRelativeAssessment" -> semantic.hasRelativeAssessment,
      "candidateComparisonFacts" -> semantic.candidateComparisonFacts,
      "relativeCauseFacts" -> semantic.relativeCauseFacts,
      "moveVerdictCertifications" -> semantic.moveVerdictCertifications,
      "playedRelatedComparisonFacts" -> semantic.playedRelatedComparisonFacts,
      "primaryPlayedComparisonFacts" -> semantic.primaryPlayedComparisonFacts,
      "playedRelativeCauseFacts" -> semantic.playedRelativeCauseFacts,
      "plan_transition_without_snapshot_pair" -> semantic.planTransitionWithoutSnapshotPairIds.size,
      "planTransitionWithoutSnapshotPairIds" -> semantic.planTransitionWithoutSnapshotPairIds,
      "branchReplyProbeRequests" -> semantic.branchReplyProbeRequests,
      "branchReplyProbeMoves" -> semantic.branchReplyProbeMoves,
      "branchReplyThreatLines" -> semantic.branchReplyThreatLines,
      "branchReplyThreatPressureRecords" -> semantic.branchReplyThreatPressureRecords,
      "branchReplyThreatEpisodeRecords" -> semantic.branchReplyThreatEpisodeRecords,
      "branchReplyProbeAdmittedResults" -> semantic.branchReplyProbeAdmittedResults,
      "branchReplyProbeRejectedResults" -> semantic.branchReplyProbeRejectedResults,
      "branchReplyProbeIgnoredResults" -> semantic.branchReplyProbeIgnoredResults,
      "branchReplyProbePendingRequests" -> semantic.branchReplyProbePendingRequests,
      "branchReplyProbeRejectReasons" -> semantic.branchReplyProbeRejectReasons,
      "branchReplyProbeAdmittedMoves" -> semantic.branchReplyProbeAdmittedMoves,
      "branchReplyProbeRejectedMoves" -> semantic.branchReplyProbeRejectedMoves,
      "branchReplyProbePendingMoves" -> semantic.branchReplyProbePendingMoves,
      "branchReplyProbeLifecycleState" -> semantic.branchReplyProbeLifecycleState,
      "branchReplyProbeLifecycle" -> branchReplyProbeLifecycle(result),
      "hasPendingBranchReplyDepth" -> semantic.hasPendingBranchReplyDepth,
      "hasUnexplainedEngineGap" -> semantic.hasUnexplainedEngineGap,
      "hasPlayedUnexplainedEngineGap" -> semantic.hasPlayedUnexplainedEngineGap,
      "hasContextUnexplainedEngineGap" -> semantic.hasContextUnexplainedEngineGap,
      "hasSecondaryContextEngineGap" -> semantic.hasSecondaryContextEngineGap,
      "secondaryContextWithPrimaryCoverageCount" -> semantic.secondaryContextWithPrimaryCoverageIds.size,
      "secondaryContextWithoutPrimaryCoverageCount" -> semantic.secondaryContextWithoutPrimaryCoverageIds.size,
      "secondaryContextComparisonIds" -> semantic.secondaryContextComparisonIds,
      "secondaryContextWithPrimaryCoverageIds" -> semantic.secondaryContextWithPrimaryCoverageIds,
      "secondaryContextWithoutPrimaryCoverageIds" -> semantic.secondaryContextWithoutPrimaryCoverageIds,
      "contextUnexplainedWithPrimaryCoverageCount" -> semantic.contextUnexplainedWithPrimaryCoverageIds.size,
      "contextUnexplainedWithoutPrimaryCoverageCount" -> semantic.contextUnexplainedWithoutPrimaryCoverageIds.size,
      "contextUnexplainedComparisonIds" -> semantic.contextUnexplainedComparisonIds,
      "contextUnexplainedWithPrimaryCoverageIds" -> semantic.contextUnexplainedWithPrimaryCoverageIds,
      "contextUnexplainedWithoutPrimaryCoverageIds" -> semantic.contextUnexplainedWithoutPrimaryCoverageIds,
      "hasTacticalDetectionFailure" -> semantic.hasTacticalDetectionFailure,
      "tacticalDetectionFailureComparisonIds" -> semantic.tacticalDetectionFailureComparisonIds,
      "tacticalDetectionFailureBlockedStages" -> semantic.tacticalDetectionFailureBlockedStages
        .map { case (stage, count) => tacticalLossStageId(stage) -> count },
      "contextTacticalOutrankingDiagnostics" -> contextTacticalOutrankingDiagnosticsJson(
        semantic.contextTacticalOutrankingDiagnostics
      ),
      "contextTacticalOutranksPrimaryPlayed" -> semantic.contextTacticalOutranksPrimaryPlayed,
      "openingApplicabilityDiagnostics" -> openingApplicabilityDiagnosticsSummary(semantic.openingApplicabilityDiagnostics),
      "openingSupportDiagnostics" -> openingSupportDiagnosticsSummary(semantic.openingSupportDiagnostics),
      "comparisonDiagnostics" -> comparisonDiagnosticsSummary(semantic.comparisonDiagnostics),
      "semanticRubricExpectedSlotCoverage" -> slotCoverage,
      "hasVerdict" -> semantic.hasVerdict,
      "hasCandidateSetComparison" -> semantic.hasCandidateSetComparison,
      "hasOnlyMoveSignal" -> semantic.hasOnlyMoveSignal,
      "hasForcedLineTheme" -> semantic.hasForcedLineTheme
    )

  private[qc] def positionPlanTechniqueObjectBindingsSampleJson(
      bindings: List[PositionPlanTechniqueObjectBinding]
  ): JsObject =
    MoveReviewPhase3AuditViewJson.positionPlanTechniqueObjectBindingsSampleJson(bindings)

  private def objectBindingIntegritySummary(packet: EvidenceBackedJudgmentPacket): JsObject =
    val graph = packet.evidenceGraph
    val claimWithoutConcreteObjectIds =
      packet.claims
        .filter(claim => PlayerFacingClaimPolicy.requiresConcreteObject(claim.family))
        .filterNot(claim => EvidenceObjectBinding.playerFacingReady(EvidenceObjectBinding.fromClaim(claim, graph)))
        .map(_.id)
        .distinct
        .sorted
    val strategicAxisWithoutSubjectIds =
      graph.records.collect {
        case record @ EvidenceRecord(_, payload: StrategicMechanismEvidence, _)
            if payload.signals.exists(signal =>
              signal.axis.nonEmpty &&
                !EvidenceObjectBinding.hasConcreteObject(EvidenceObjectBinding.fromEvidenceRefs(graph, List(signal.source)))
            ) =>
          record.ref.id
        case record @ EvidenceRecord(_, payload: StrategicMechanismContrastEvidence, _)
            if payload.axisComparisons.exists(axis =>
              axis.sources.nonEmpty &&
                !EvidenceObjectBinding.hasConcreteObject(EvidenceObjectBinding.fromEvidenceRefs(graph, axis.sources))
            ) =>
          record.ref.id
      }.distinct.sorted
    val tacticalMechanismWithoutTargetIds =
      graph.records.collect {
        case record @ EvidenceRecord(_, payload: TacticalMechanismEvidence, _)
            if payload.signals.nonEmpty &&
              EvidenceObjectBinding.fromEvidenceRefs(graph, List(record.ref)).forall(_.target.isEmpty) =>
          record.ref.id
      }.distinct.sorted
    val relativeCauseWithoutConcreteObjectIds =
      graph.records.collect {
        case EvidenceRecord(ref, RelativeCauseFactEvidence(cause), _)
            if !EvidenceObjectBinding.hasConcreteObject(EvidenceObjectBinding.fromRelativeCause(cause, graph)) =>
          ref.id
      }.distinct.sorted
    Json.obj(
      "claim_without_concrete_object" -> claimWithoutConcreteObjectIds,
      "strategic_axis_without_subject" -> strategicAxisWithoutSubjectIds,
      "tactical_mechanism_without_target" -> tacticalMechanismWithoutTargetIds,
      "relative_cause_without_concrete_object" -> relativeCauseWithoutConcreteObjectIds
    )

  private def strategicAxisIntegritySummary(packet: EvidenceBackedJudgmentPacket): JsObject =
    val records = packet.evidenceGraph.records
    val sourceRecords =
      records.filter(record => StrategicMechanismEvidence.sourceMechanisms(record).nonEmpty)
    val mechanismRecords =
      records.collect { case record @ EvidenceRecord(_, payload: StrategicMechanismEvidence, _) =>
        record -> payload
      }
    val mechanismSignalSourceIds =
      mechanismRecords.flatMap { case (_, payload) => payload.signals.map(_.source.id) }.toSet
    val mechanismParentIds =
      mechanismRecords.flatMap { case (record, _) => record.parents.map(_.id) }.toSet
    val sourceIds = sourceRecords.map(_.ref.id).toSet
    val orphanSourceIds =
      (sourceIds -- mechanismSignalSourceIds -- mechanismParentIds).toList.sorted
    val mechanismParentMismatchIds =
      mechanismRecords.collect {
        case (record, payload) if !payload.signals.forall(signal => record.parents.exists(_.id == signal.source.id)) =>
          record.ref.id
      }.sorted
    val axisSignals =
      mechanismRecords.flatMap { case (_, payload) => payload.signals.flatMap(signal => signal.axis.map(_ -> signal)) }
    val rawBypassClaimEvidenceIds =
      packet.claims
        .filter(_.family.isLongTerm)
        .flatMap(_.evidence)
        .filter(ref => StrategicMechanismEvidence.rawStrategicSourceLayer(ref.layer))
        .map(_.id)
        .distinct
        .sorted
    val strategicCauseAxisIds =
      records.collect {
        case EvidenceRecord(ref, RelativeCauseFactEvidence(cause), _)
            if cause.proof.exists(_.strategicMechanisms.exists(_.signals.exists(_.axis.nonEmpty))) =>
          ref.id
      }.sorted
    val lifecycleAxisLineage =
      packet.claimLifecycle.flatMap(_.strategicAxisLineage)
    val lifecycleClaimIdsWithAxis =
      packet.claimLifecycle.filter(_.strategicAxisLineage.nonEmpty).map(_.candidateId).distinct.sorted
    val finalLifecycleClaimIdsWithAxis =
      packet.claimLifecycle.filter(diagnostic => diagnostic.finalPacketIncluded && diagnostic.strategicAxisLineage.nonEmpty).map(_.candidateId).distinct.sorted
    val mechanismOnlyStrategicClaimIds =
      packet.claimLifecycle
        .filter(diagnostic =>
          diagnostic.family.isLongTerm &&
            diagnostic.finalPacketIncluded &&
            diagnostic.strategicAxisLineage.nonEmpty &&
            diagnostic.relativeCauses.isEmpty
        )
        .map(_.candidateId)
        .distinct
        .sorted
    val unboundMechanismOnlyStrategicClaimIds =
      packet.claimLifecycle
        .filter(diagnostic =>
          diagnostic.family.isLongTerm &&
            diagnostic.finalPacketIncluded &&
            diagnostic.strategicAxisLineage.nonEmpty &&
            diagnostic.relativeCauses.isEmpty &&
            diagnostic.subjectBinding != SubjectBindingClass.DirectPlayed
        )
        .map(_.candidateId)
        .distinct
        .sorted
    val transitionLocalStrategicClaimIds =
      packet.claimLifecycle
        .filter(diagnostic =>
          diagnostic.family.isLongTerm &&
            diagnostic.finalPacketIncluded &&
            diagnostic.strategicAxisLineage.nonEmpty &&
            diagnostic.relativeCauses.isEmpty &&
            diagnostic.subjectBinding == SubjectBindingClass.DirectPlayed
        )
        .map(_.candidateId)
        .distinct
        .sorted
    val viewClaimAxisKeys =
      packet.moveJudgmentView.toList.flatMap(_.verdictCarriers.flatMap(_.strategicAxisKeys)).distinct.sorted
    val viewCauseAxisKeys =
      packet.moveJudgmentView.toList.flatMap(view =>
        (view.causeAudit.primary ++ view.causeAudit.secondary ++ view.causeAudit.context).flatMap(_.proofStrategicAxisKeys)
      ).distinct.sorted
    val reachability = finalStrategicClaimReachability(packet)
    Json.obj(
      "sourceRecordsWithMechanismPotential" -> sourceRecords.size,
      "strategicMechanismRecords" -> mechanismRecords.size,
      "mechanismAxisSignals" -> axisSignals.size,
      "axisKindCounts" -> stringCountsJson(axisSignals.map { case (axis, _) => axis.kind.toString }),
      "axisSourceLayerCounts" -> stringCountsJson(axisSignals.map { case (_, signal) => signal.source.layer.toString }),
      "axislessStructuralAnchorInventory" -> axislessStructuralAnchorInventoryJson(packet.evidenceGraph),
      "orphanSourceCount" -> orphanSourceIds.size,
      "orphanSourceIds" -> orphanSourceIds,
      "mechanismParentMismatchCount" -> mechanismParentMismatchIds.size,
      "mechanismParentMismatchIds" -> mechanismParentMismatchIds,
      "rawBypassClaimEvidenceCount" -> rawBypassClaimEvidenceIds.size,
      "rawBypassClaimEvidenceIds" -> rawBypassClaimEvidenceIds,
      "relativeCauseWithStrategicAxisProofCount" -> strategicCauseAxisIds.size,
      "relativeCauseWithStrategicAxisProofIds" -> strategicCauseAxisIds,
      "claimStrategicAxisLineageCount" -> lifecycleAxisLineage.size,
      "claimStrategicAxisLineageWithRelativeCauseCount" -> lifecycleAxisLineage.count(_.relativeCauseIds.nonEmpty),
      "claimWithStrategicAxisLineageCount" -> lifecycleClaimIdsWithAxis.size,
      "claimWithStrategicAxisLineageIds" -> lifecycleClaimIdsWithAxis,
      "finalClaimWithStrategicAxisLineageCount" -> finalLifecycleClaimIdsWithAxis.size,
      "finalClaimWithStrategicAxisLineageIds" -> finalLifecycleClaimIdsWithAxis,
      "mechanismOnlyStrategicClaimCount" -> mechanismOnlyStrategicClaimIds.size,
      "mechanismOnlyStrategicClaimIds" -> mechanismOnlyStrategicClaimIds,
      "unboundMechanismOnlyStrategicClaimCount" -> unboundMechanismOnlyStrategicClaimIds.size,
      "unboundMechanismOnlyStrategicClaimIds" -> unboundMechanismOnlyStrategicClaimIds,
      "transitionLocalStrategicClaimCount" -> transitionLocalStrategicClaimIds.size,
      "transitionLocalStrategicClaimIds" -> transitionLocalStrategicClaimIds,
      "moveJudgmentClaimStrategicAxisCount" -> viewClaimAxisKeys.size,
      "moveJudgmentClaimStrategicAxisKeys" -> viewClaimAxisKeys,
      "moveJudgmentCauseStrategicAxisCount" -> viewCauseAxisKeys.size,
      "moveJudgmentCauseStrategicAxisKeys" -> viewCauseAxisKeys,
      "finalStrategicClaimReachabilityCounts" -> stringCountsJson(reachability.map(_("classification").as[String])),
      "finalStrategicClaimReachability" -> JsArray(reachability)
    )

  private[qc] def axislessStructuralAnchorInventoryJson(graph: TypedEvidenceGraph): JsObject =
    MoveReviewPhase3AuditMetrics.axislessStructuralAnchorInventoryJson(graph, lineRefSummary)

  private def finalStrategicClaimReachability(packet: EvidenceBackedJudgmentPacket): List[JsObject] =
    val viewClaimIds =
      packet.moveJudgmentView.toList.flatMap(view =>
        view.verdictCarriers.map(_.claimId)
      ).toSet
    val viewCauseClaimIds =
      packet.moveJudgmentView.toList.flatMap(view =>
        (view.causeAudit.primary ++ view.causeAudit.secondary ++ view.causeAudit.context).flatMap(frame =>
          frame.causeClaimIds ++ frame.evaluationClaimIds ++ frame.witnessClaimIds ++ frame.finalClaimIds
        )
      ).toSet
    val supportClusterClaimIds =
      packet.claimSupportClusters.flatMap(cluster =>
        cluster.anchorClaimIds ++ cluster.supportingClaimIds ++ cluster.constrainingClaimIds
      ).toSet
    val eventClusterClaimIds =
      packet.claimEventClusters.flatMap(cluster =>
        cluster.memberClaimIds ++ cluster.causeClaimIds ++ cluster.evaluationClaimIds ++ cluster.witnessClaimIds
      ).toSet

    packet.claimLifecycle
      .filter(diagnostic => diagnostic.finalPacketIncluded && diagnostic.strategicAxisLineage.nonEmpty)
      .map { diagnostic =>
        val claimIds =
          (List(diagnostic.candidateId, diagnostic.claimId) ++ diagnostic.finalClaimId.toList).distinct
        val causeBound =
          diagnostic.strategicAxisLineage.exists(_.relativeCauseIds.nonEmpty) ||
            diagnostic.relativeCauses.exists(_.proofStrategicAxisKeys.nonEmpty)
        val viewClaimBound = claimIds.exists(viewClaimIds.contains)
        val viewCauseBound = claimIds.exists(viewCauseClaimIds.contains)
        val supportClusterBound = claimIds.exists(supportClusterClaimIds.contains)
        val eventClusterBound = claimIds.exists(eventClusterClaimIds.contains)
        val classification =
          if causeBound then "cause_bound"
          else if viewClaimBound || viewCauseBound then "view_bound"
          else if supportClusterBound then "support_cluster_bound"
          else if eventClusterBound then "event_cluster_bound"
          else if diagnostic.relativeCauses.isEmpty && diagnostic.subjectBinding == SubjectBindingClass.DirectPlayed then "transition_local_mechanism_only"
          else if diagnostic.relativeCauses.isEmpty then "mechanism_only_unbound"
          else "unclassified"
        Json.obj(
          "candidateId" -> diagnostic.candidateId,
          "claimId" -> diagnostic.claimId,
          "finalClaimId" -> diagnostic.finalClaimId,
          "family" -> diagnostic.family.toString,
          "subjectBinding" -> diagnostic.subjectBinding.toString,
          "classification" -> classification,
          "causeBound" -> causeBound,
          "viewClaimBound" -> viewClaimBound,
          "viewCauseBound" -> viewCauseBound,
          "supportClusterBound" -> supportClusterBound,
          "eventClusterBound" -> eventClusterBound,
          "axisKeys" -> diagnostic.strategicAxisLineage.map(_.axisKey).distinct.sorted,
          "relativeCauseIds" -> diagnostic.strategicAxisLineage.flatMap(_.relativeCauseIds).distinct.sorted
        )
      }
      .sortBy(obj => (obj("classification").as[String], obj("candidateId").as[String]))

  private def qcFailureSummary(result: MoveReviewJudgmentResult): JsObject =
    val semantic = result.quality.semanticCoverage
    val relativeCauseSummary =
      relativeCauseQualitySummary(
        semantic.comparisonDiagnostics,
        moveJudgmentPrimaryCauseCount(result)
      )
    Json.obj(
      "hasTacticalDetectionFailure" -> semantic.hasTacticalDetectionFailure,
      "tacticalDetectionFailureComparisonIds" -> semantic.tacticalDetectionFailureComparisonIds,
      "tacticalDetectionFailureBlockedStages" -> semantic.tacticalDetectionFailureBlockedStages
        .map { case (stage, count) => tacticalLossStageId(stage) -> count },
      "contextTacticalOutranksPrimaryPlayed" -> semantic.contextTacticalOutranksPrimaryPlayed,
      "contextTacticalOutrankingDiagnostics" -> contextTacticalOutrankingDiagnosticsJson(
        semantic.contextTacticalOutrankingDiagnostics
      ),
      "relativeCauseQualitySummary" -> relativeCauseSummary
    )

  private def contextTacticalOutrankingDiagnosticsJson(
      diagnostics: List[ContextTacticalOutrankingDiagnostic]
  ): JsArray =
    JsArray(
      diagnostics.map(diagnostic =>
        Json.obj(
          "contextClaimId" -> diagnostic.contextClaimId,
          "contextRank" -> diagnostic.contextRank,
          "contextSubjectBinding" -> diagnostic.contextSubjectBinding.toString,
          "contextTier" -> diagnostic.contextTier.toString,
          "contextSalienceScore" -> diagnostic.contextSalienceScore,
          "contextComparisonIds" -> diagnostic.contextComparisonIds,
          "contextComparisonKinds" -> diagnostic.contextComparisonKinds.map(_.toString),
          "contextCauseRoles" -> diagnostic.contextCauseRoles.map(_.toString),
          "contextCauseSourceSides" -> diagnostic.contextCauseSourceSides.map(_.toString),
          "contextCauseImportances" -> diagnostic.contextCauseImportances.map(_.toString),
          "contextCauseEventLines" -> diagnostic.contextCauseEventLines.map(lineRefSummary),
          "contextEventClusterIds" -> diagnostic.contextEventClusterIds,
          "outrankedPrimaryClaimIds" -> diagnostic.outrankedPrimaryClaimIds,
          "outrankedPrimaryRanks" -> diagnostic.outrankedPrimaryRanks,
          "outrankedPrimaryFamilies" -> diagnostic.outrankedPrimaryFamilies.map(_.toString),
          "outrankedPrimarySalienceScores" -> diagnostic.outrankedPrimarySalienceScores,
          "outrankedPrimaryEventClusterIds" -> diagnostic.outrankedPrimaryEventClusterIds,
          "salienceDeltaToBestPrimary" -> diagnostic.salienceDeltaToBestPrimary
        )
      )
    )

  private def claimLifecycleCountsJson[K](counts: Map[K, Int]): JsObject =
    JsObject(
      counts.toList.sortBy(_._1.toString).map { case (key, count) =>
        key.toString -> JsNumber(count)
      }
    )

  private def claimLifecycleDroppedByStageJson(
      droppedByStage: Map[ClaimLifecycleStage, List[String]]
  ): JsObject =
    JsObject(
      droppedByStage.toList.sortBy(_._1.toString).map { case (stage, ids) =>
        stage.toString -> JsArray(ids.sorted.map(JsString.apply))
      }
    )

  private def claimLifecycleDiagnosticsJson(
      diagnostics: List[ClaimLifecycleDiagnostic]
  ): JsArray =
    JsArray(
      diagnostics.map(diagnostic =>
        Json.obj(
          "candidateId" -> diagnostic.candidateId,
          "claimId" -> diagnostic.claimId,
          "finalClaimId" -> diagnostic.finalClaimId,
          "sourceCandidateIds" -> diagnostic.sourceCandidateIds,
          "family" -> diagnostic.family.toString,
          "subject" -> diagnostic.subject.toString,
          "subjectBinding" -> diagnostic.subjectBinding.toString,
          "primaryLine" -> diagnostic.primaryLine.map(lineRefSummary),
          "subjectMove" -> diagnostic.subjectMove,
          "ideaIds" -> diagnostic.ideaIds,
          "finalIdeaIds" -> diagnostic.finalIdeaIds,
          "evidenceIds" -> diagnostic.evidenceIds,
          "finalEvidenceIds" -> diagnostic.finalEvidenceIds,
          "relativeCauseIds" -> diagnostic.relativeCauses.map(_.id),
          "relativeCauseKinds" -> diagnostic.relativeCauses.map(_.kind.toString).distinct,
          "relativeCauseRoles" -> diagnostic.relativeCauses.map(_.role.toString).distinct,
          "relativeCauseComparisonKinds" -> diagnostic.relativeCauses.map(_.comparisonKind.toString).distinct,
          "relativeCauseSourceSides" -> diagnostic.relativeCauses.map(_.sourceSide.toString).distinct,
          "relativeCauseImportances" -> diagnostic.relativeCauses.map(_.importance.toString).distinct,
          "relativeCauseAttributionKinds" -> diagnostic.relativeCauses.map(_.attributionKind.toString).distinct,
          "relativeCauseAttributionOwnedEvidenceIds" -> diagnostic.relativeCauses.flatMap(_.attributionOwnedEvidenceIds).distinct.sorted,
          "relativeCauseAttributionContrastEvidenceIds" -> diagnostic.relativeCauses.flatMap(_.attributionContrastEvidenceIds).distinct.sorted,
          "relativeCauseAttributionContextEvidenceIds" -> diagnostic.relativeCauses.flatMap(_.attributionContextEvidenceIds).distinct.sorted,
          "relativeCauseAttributionRootMoveMatched" -> diagnostic.relativeCauses.map(_.attributionRootMoveMatched).distinct,
          "relativeCauseAttributionDirectProofEligible" -> diagnostic.relativeCauses.map(_.attributionDirectProofEligible).distinct,
          "relativeCauseAttributionReasons" -> diagnostic.relativeCauses.flatMap(_.attributionReason).distinct.sorted,
          "relativeCauseReferenceLines" -> diagnostic.relativeCauses.map(_.referenceLine).distinct.map(lineRefSummary),
          "relativeCauseCandidateLines" -> diagnostic.relativeCauses.map(_.candidateLine).distinct.map(lineRefSummary),
          "relativeCauseEventLines" -> diagnostic.relativeCauses.map(_.eventLine).distinct.map(lineRefSummary),
          "relativeCauseProofDirectSourceIds" -> diagnostic.relativeCauses.flatMap(_.proofDirectSourceIds).distinct.sorted,
          "relativeCauseProofContrastSourceIds" -> diagnostic.relativeCauses.flatMap(_.proofContrastSourceIds).distinct.sorted,
          "relativeCauseProofContextSupportSourceIds" -> diagnostic.relativeCauses.flatMap(_.proofContextSupportSourceIds).distinct.sorted,
          "relativeCauseProofStrategicAxisKeys" -> diagnostic.relativeCauses.flatMap(_.proofStrategicAxisKeys).distinct.sorted,
          "relativeCauseSupportEvidenceSourceIds" -> diagnostic.relativeCauses.flatMap(_.supportEvidenceSourceIds).distinct.sorted,
          "relativeCauseProofDirectKinds" -> diagnostic.relativeCauses.flatMap(_.proofDirectKinds).distinct.sorted,
          "relativeCauseProofContrastKinds" -> diagnostic.relativeCauses.flatMap(_.proofContrastKinds).distinct.sorted,
          "relativeCauseProofContextSupportKinds" -> diagnostic.relativeCauses.flatMap(_.proofContextSupportKinds).distinct.sorted,
          "strategicAxisLineage" -> strategicAxisLineageJson(diagnostic.strategicAxisLineage),
          "strategicAxisKeys" -> diagnostic.strategicAxisLineage.map(_.axisKey).distinct.sorted,
          "strategicAxisMechanismEvidenceIds" -> diagnostic.strategicAxisLineage.map(_.mechanismEvidenceId).distinct.sorted,
          "strategicAxisSourceEvidenceIds" -> diagnostic.strategicAxisLineage.map(_.signalSourceEvidenceId).distinct.sorted,
          "strategicAxisRelativeCauseIds" -> diagnostic.strategicAxisLineage.flatMap(_.relativeCauseIds).distinct.sorted,
          "truthStatus" -> diagnostic.truthStatus.map(_.toString),
          "presentLayers" -> diagnostic.presentLayers.map(_.toString).toList.sorted,
          "missingLayerGroups" -> JsArray(
            diagnostic.missingLayerGroups.map(group => JsArray(group.map(_.toString).toList.sorted.map(JsString.apply)))
          ),
          "missingEvidenceIds" -> diagnostic.missingEvidenceIds,
          "stages" -> diagnostic.stages.map(_.toString),
          "dedupeWinnerId" -> diagnostic.dedupeWinnerId,
          "arbitrationRank" -> diagnostic.arbitrationRank,
          "finalPacketIncluded" -> diagnostic.finalPacketIncluded
        )
      )
    )

  private def strategicAxisLineageJson(lineage: List[ClaimStrategicAxisLineage]): JsArray =
    JsArray(
      lineage.map(entry =>
        Json.obj(
          "axisKey" -> entry.axisKey,
          "axisKind" -> entry.axisKind.toString,
          "axisPolarity" -> entry.axisPolarity.toString,
          "axisLabel" -> entry.axisLabel,
          "mechanismEvidenceId" -> entry.mechanismEvidenceId,
          "signalSourceEvidenceId" -> entry.signalSourceEvidenceId,
          "signalSourceLayer" -> entry.signalSourceLayer.toString,
          "relativeCauseIds" -> entry.relativeCauseIds
        )
      )
    )

  private def strategicAxisProofLineageJson(lineage: List[StrategicAxisProofLineage]): JsArray =
    JsArray(
      lineage.map(entry =>
        Json.obj(
          "axisKey" -> entry.axisKey,
          "axisKind" -> entry.axisKind.toString,
          "axisPolarity" -> entry.axisPolarity.toString,
          "axisLabel" -> entry.axisLabel,
          "mechanismEvidenceId" -> entry.mechanismEvidenceId,
          "signalSourceEvidenceId" -> entry.signalSourceEvidenceId,
          "signalSourceLayer" -> entry.signalSourceLayer.toString
        )
      )
    )

  private def localConcreteClaimDetails(diagnostics: List[LocalConcreteClaimDiagnostic]): JsArray =
    JsArray(
      diagnostics.map(diagnostic =>
        Json.obj(
          "id" -> diagnostic.id,
          "family" -> diagnostic.family.toString,
          "subject" -> diagnostic.subject.toString,
          "primaryLine" -> diagnostic.primaryLine.map(lineRefSummary),
          "subjectMove" -> diagnostic.subjectMove,
          "scope" -> diagnostic.scope.toString,
          "localKind" -> diagnostic.localKind.toString,
          "evidenceLayers" -> diagnostic.evidenceLayers.map(_.toString).toList.sorted,
          "evidenceIds" -> diagnostic.evidenceIds,
          "engineVerdict" -> diagnostic.engineVerdict.map(_.toString),
          "engineWinPercentLossForMover" -> diagnostic.engineWinPercentLossForMover,
          "threatSeverity" -> diagnostic.threatSeverity.map(_.toString),
          "threatCount" -> diagnostic.threatCount,
          "threatDefenseRequired" -> diagnostic.threatDefenseRequired,
          "threatProphylaxisNeeded" -> diagnostic.threatProphylaxisNeeded,
          "threatOnlyDefense" -> diagnostic.threatOnlyDefense,
          "threatMaxWinPercentLossIfIgnored" -> diagnostic.threatMaxWinPercentLossIfIgnored,
          "threatPrimaryDriver" -> diagnostic.threatPrimaryDriver.map(_.toString),
          "threatInsufficientData" -> diagnostic.threatInsufficientData,
          "threatEpisodes" -> diagnostic.threatEpisodes.map(episode =>
            Json.obj(
              "episodeId" -> episode.episodeId,
              "sourceThreatIndex" -> episode.sourceThreatIndex,
              "sideUnderPressure" -> episode.sideUnderPressure,
              "kind" -> episode.kind,
              "severity" -> episode.severity.toString,
              "driver" -> episode.driver.toString,
              "defenseRequired" -> episode.defenseRequired,
              "prophylaxisNeeded" -> episode.prophylaxisNeeded,
              "onlyDefense" -> episode.onlyDefense,
              "maxWinPercentLossIfIgnored" -> episode.maxWinPercentLossIfIgnored,
              "insufficientData" -> episode.insufficientData
            )
          ),
          "nearbyEventClusterIds" -> diagnostic.nearbyEventClusterIds
        )
      )
    )

  private def comparisonDiagnosticsSummary(diagnostics: List[CandidateComparisonDiagnostic]): JsArray =
    JsArray(diagnostics.map(comparisonDiagnosticJson))

  private[qc] def relativeCauseQualitySummaryJson(diagnostics: List[CandidateComparisonDiagnostic]): JsObject =
    relativeCauseQualitySummary(diagnostics, diagnostics.count(hasMoveJudgmentPrimaryCause))

  private def relativeCauseQualitySummary(
      diagnostics: List[CandidateComparisonDiagnostic],
      primaryCauseCount: Int
  ): JsObject =
    val referenceLeadSemanticAxisDiagnostics = diagnostics.filter(referenceLeadSemanticAxis)
    val referenceLeadTwoSemanticAxisDiagnostics = diagnostics.filter(referenceLeadTwoSemanticAxis)
    val referenceOnlySemanticAxisDiagnostics = diagnostics.filter(referenceOnlySemanticAxis)
    val referenceOnlyTwoSemanticAxisDiagnostics = diagnostics.filter(referenceOnlyTwoSemanticAxis)
    val referenceLeadTwoSemanticAxisNoEventCauseDiagnostics =
      referenceLeadTwoSemanticAxisDiagnostics.filter(noEventCauseComparison)
    val referenceLeadTwoSemanticAxisNoEventCauseNoPrimaryDiagnostics =
      referenceLeadTwoSemanticAxisNoEventCauseDiagnostics.filterNot(hasMoveJudgmentPrimaryCause)
    val referenceLeadNonTacticalDiagnostics =
      referenceLeadSemanticAxisDiagnostics.filter(nonTacticalComparison)
    val referenceLeadTwoSemanticAxisNonTacticalDiagnostics =
      referenceLeadTwoSemanticAxisDiagnostics.filter(nonTacticalComparison)
    val referenceLeadTwoSemanticAxisNonTacticalNoPrimaryDiagnostics =
      referenceLeadTwoSemanticAxisNonTacticalDiagnostics.filterNot(hasMoveJudgmentPrimaryCause)
    val referenceOnlyTwoSemanticAxisNonTacticalDiagnostics =
      referenceOnlyTwoSemanticAxisDiagnostics.filter(nonTacticalComparison)
    val referenceOnlyTwoSemanticAxisNonTacticalNoPrimaryDiagnostics =
      referenceOnlyTwoSemanticAxisNonTacticalDiagnostics.filterNot(hasMoveJudgmentPrimaryCause)
    val generatedCauseNoPrimaryDiagnostics =
      diagnostics.filter(generatedCauseWithoutPrimary)
    val primaryPlayedCauseNoPrimaryDiagnostics =
      diagnostics.filter(primaryPlayedCauseWithoutPrimary)
    val longTermCauseNoPrimaryDiagnostics =
      diagnostics.filter(longTermCauseWithoutPrimary)
    val primaryPlayedDiagnostics =
      diagnostics.filter(diagnostic =>
        diagnostic.comparisonKind == CandidateComparisonKind.PlayedVsBest &&
          diagnostic.subjectBinding == SubjectBindingClass.PrimaryPlayedCause
      )
    val primaryPlayedDiagnosticIds = primaryPlayedDiagnostics.map(_.id).toSet
    val contextInventoryDiagnostics =
      diagnostics.filterNot(diagnostic => primaryPlayedDiagnosticIds.contains(diagnostic.id))
    val generatedNoPrimaryFlows =
      generatedCauseNoPrimaryDiagnostics.flatMap(_.relativeCauseDiagnostics.causeFlow)
    val longTermNoPrimaryFlows =
      longTermCauseNoPrimaryDiagnostics.flatMap(_.relativeCauseDiagnostics.causeFlow.filter(noEventCauseFlow))
    Json.obj(
      "missingCauseComparisonIds" -> diagnostics.filter(_.relativeCauseDiagnostics.missingCause).map(_.id),
      "shallowProofComparisonIds" -> diagnostics.filter(_.relativeCauseDiagnostics.shallowProofCauseIds.nonEmpty).map(_.id),
      "strategic_cause_without_contrast" -> diagnostics.filter(
        _.relativeCauseDiagnostics.strategicCauseWithoutContrastIds.nonEmpty
      ).map(_.id),
      "strategic_claim_without_comparative_cause" -> diagnostics.filter(
        _.relativeCauseDiagnostics.strategicClaimWithoutComparativeCauseIds.nonEmpty
      ).map(_.id),
      "generic_structural_improvement_without_strategic_contrast" -> diagnostics.filter(
        _.relativeCauseDiagnostics.genericStructuralImprovementWithoutStrategicContrastIds.nonEmpty
      ).map(_.id),
      "context_support_used_as_direct_proof" -> diagnostics.filter(
        _.relativeCauseDiagnostics.contextSupportUsedAsDirectProofIds.nonEmpty
      ).map(_.id),
      "context_only_relative_cause" -> diagnostics.filter(
        _.relativeCauseDiagnostics.contextOnlyCauseIds.nonEmpty
      ).map(_.id),
      "unattributed_relative_cause" -> diagnostics.filter(
        _.relativeCauseDiagnostics.unattributedCauseIds.nonEmpty
      ).map(_.id),
      "root_mismatched_relative_cause" -> diagnostics.filter(
        _.relativeCauseDiagnostics.rootMismatchedCauseIds.nonEmpty
      ).map(_.id),
      "support_promoted_to_direct_proof" -> diagnostics.filter(
        _.relativeCauseDiagnostics.supportPromotedToDirectProofCauseIds.nonEmpty
      ).map(_.id),
      "relative_cause_without_concrete_object" -> diagnostics.filter(
        _.relativeCauseDiagnostics.relativeCauseWithoutObjectSignatureIds.nonEmpty
      ).map(_.id),
      "graph_object_lost_between_evidence_and_cause" -> diagnostics.filter(
        _.relativeCauseDiagnostics.objectLostBetweenEvidenceAndCauseIds.nonEmpty
      ).map(_.id),
      "graph_object_lost_between_cause_and_claim" -> diagnostics.filter(
        _.relativeCauseDiagnostics.objectLostBetweenCauseAndClaimIds.nonEmpty
      ).map(_.id),
      "public_move_meaning_without_carrier" -> diagnostics.filter(diagnostic =>
        diagnostic.moveJudgmentView.publicMoveMeaningClaimDiagnostics.exists(!_.hasCarrier)
      ).map(_.id),
      "unboundEvidenceComparisonIds" -> diagnostics.filter(_.relativeCauseDiagnostics.unboundEvidenceIds.nonEmpty).map(_.id),
      "causeWithoutIdeaComparisonIds" -> diagnostics.filter(_.relativeCauseDiagnostics.causeWithoutIdeaIds.nonEmpty).map(_.id),
      "ideaWithoutClaimComparisonIds" -> diagnostics.filter(_.relativeCauseDiagnostics.ideaWithoutClaimCauseIds.nonEmpty).map(_.id),
      "ideaWithoutClaimCandidateComparisonIds" -> diagnostics.filter(
        _.relativeCauseDiagnostics.ideaWithoutClaimCandidateCauseIds.nonEmpty
      ).map(_.id),
      "ideaWithoutFinalClaimComparisonIds" -> diagnostics.filter(
        _.relativeCauseDiagnostics.ideaWithoutFinalClaimCauseIds.nonEmpty
      ).map(_.id),
      "claimWithoutEventClusterComparisonIds" -> diagnostics.filter(
        _.relativeCauseDiagnostics.claimWithoutEventClusterCauseIds.nonEmpty
      ).map(_.id),
      "primaryFailures" -> Json.obj(
        "primaryPlayedCauseNoPrimary" -> comparisonGroupJson(primaryPlayedCauseNoPrimaryDiagnostics)
      ),
      "diagnosticInventory" -> Json.obj(
        "moveJudgmentPrimaryCause" -> Json.obj(
          "comparisonCount" -> primaryCauseCount,
          "comparisonIds" -> diagnostics.filter(hasMoveJudgmentPrimaryCause).map(_.id),
          "causeKindCounts" -> stringCountsJson(
            diagnostics.flatMap(_.moveJudgmentView.primaryCauseKinds.map(_.toString))
          )
        ),
        "generatedNoPrimary" -> noPrimaryInventoryGroupJson(
          generatedCauseNoPrimaryDiagnostics,
          generatedNoPrimaryFlows
        ),
        "longTermNoPrimary" -> noPrimaryInventoryGroupJson(
          longTermCauseNoPrimaryDiagnostics,
          longTermNoPrimaryFlows
        ),
        "semanticAxis" -> Json.obj(
          "referenceLead" -> comparisonGroupJson(referenceLeadSemanticAxisDiagnostics),
          "referenceLeadTwoAxis" -> comparisonGroupJson(referenceLeadTwoSemanticAxisDiagnostics),
          "referenceLeadTwoAxisNoEventCause" -> comparisonGroupJson(referenceLeadTwoSemanticAxisNoEventCauseDiagnostics),
          "referenceLeadTwoAxisNoEventCauseNoPrimary" ->
            comparisonGroupJson(referenceLeadTwoSemanticAxisNoEventCauseNoPrimaryDiagnostics),
          "referenceLeadNonTactical" -> comparisonGroupJson(referenceLeadNonTacticalDiagnostics),
          "referenceLeadTwoAxisNonTactical" -> comparisonGroupJson(referenceLeadTwoSemanticAxisNonTacticalDiagnostics),
          "referenceLeadTwoAxisNonTacticalNoPrimary" ->
            comparisonGroupJson(referenceLeadTwoSemanticAxisNonTacticalNoPrimaryDiagnostics),
          "referenceOnly" -> comparisonGroupJson(referenceOnlySemanticAxisDiagnostics),
          "referenceOnlyTwoAxis" -> comparisonGroupJson(referenceOnlyTwoSemanticAxisDiagnostics),
          "referenceOnlyTwoAxisNonTactical" -> comparisonGroupJson(referenceOnlyTwoSemanticAxisNonTacticalDiagnostics),
          "referenceOnlyTwoAxisNonTacticalNoPrimary" ->
            comparisonGroupJson(referenceOnlyTwoSemanticAxisNonTacticalNoPrimaryDiagnostics),
          "referenceLeadAxisLabelCounts" -> stringCountsJson(
            diagnostics.flatMap(_.decisionTrace.semanticAxisDiagnostics.referenceLeadAxes)
          ),
          "referenceOnlyAxisLabelCounts" -> stringCountsJson(
            diagnostics.flatMap(_.decisionTrace.semanticAxisDiagnostics.referenceOnlyAxes)
          )
        ),
        "failureClassCounts" -> stringCountsJson(diagnostics.map(diagnostic => failureClassId(diagnostic.failureClass))),
        "primaryFailureClassCounts" -> stringCountsJson(
          primaryPlayedDiagnostics.map(diagnostic => failureClassId(diagnostic.failureClass))
        ),
        "contextInventoryFailureClassCounts" -> stringCountsJson(
          contextInventoryDiagnostics.map(diagnostic => failureClassId(diagnostic.failureClass))
        ),
        "failureReasonCounts" -> stringCountsJson(diagnostics.flatMap(_.failureReasons.map(failureReasonId)))
      )
    )

  private def referenceLeadSemanticAxis(diagnostic: CandidateComparisonDiagnostic): Boolean =
    diagnostic.decisionTrace.semanticAxisDiagnostics.referenceLeadAxes.nonEmpty

  private def referenceLeadTwoSemanticAxis(diagnostic: CandidateComparisonDiagnostic): Boolean =
    diagnostic.decisionTrace.semanticAxisDiagnostics.referenceLeadAxisCount >= 2

  private def referenceOnlySemanticAxis(diagnostic: CandidateComparisonDiagnostic): Boolean =
    diagnostic.decisionTrace.semanticAxisDiagnostics.referenceOnlyAxes.nonEmpty

  private def referenceOnlyTwoSemanticAxis(diagnostic: CandidateComparisonDiagnostic): Boolean =
    diagnostic.decisionTrace.semanticAxisDiagnostics.referenceOnlyAxes.size >= 2

  private def nonTacticalComparison(diagnostic: CandidateComparisonDiagnostic): Boolean =
    noEventCauseComparison(diagnostic)

  private def noEventCauseComparison(diagnostic: CandidateComparisonDiagnostic): Boolean =
    !diagnostic.decisionTrace.tacticalLoss &&
      !diagnostic.decisionTrace.materialSwingEvidence &&
      diagnostic.causeKinds.forall(kind =>
        !ClaimEventCluster.kindForCause(kind).exists(kind =>
          kind == ClaimEventClusterKind.TacticalEvent ||
            kind == ClaimEventClusterKind.DefensiveEvent ||
            kind == ClaimEventClusterKind.ConversionEvent ||
            kind == ClaimEventClusterKind.MaterialEvent
        )
      )

  private def moveJudgmentPrimaryCauseCount(result: MoveReviewJudgmentResult): Int =
    result.packet.moveJudgmentView.fold(0)(_.causeAudit.primary.size)

  private def hasMoveJudgmentPrimaryCause(diagnostic: CandidateComparisonDiagnostic): Boolean =
    diagnostic.moveJudgmentView.hasPrimaryCause

  private def generatedCauseWithoutPrimary(diagnostic: CandidateComparisonDiagnostic): Boolean =
    diagnostic.relativeCauseDiagnostics.producedCauseIds.nonEmpty && !hasMoveJudgmentPrimaryCause(diagnostic)

  private def primaryPlayedCauseWithoutPrimary(diagnostic: CandidateComparisonDiagnostic): Boolean =
    !hasMoveJudgmentPrimaryCause(diagnostic) &&
      badVerdict(diagnostic.verdict) &&
      diagnostic.relativeCauseDiagnostics.causeFlow.exists(flow =>
        flow.causeRole == RelativeCauseRole.PrimaryPlayedCause &&
          flow.causeComparisonKind == CandidateComparisonKind.PlayedVsBest
      )

  private def badVerdict(verdict: MoveChoiceVerdict): Boolean =
    verdict match
      case MoveChoiceVerdict.Inaccuracy | MoveChoiceVerdict.Mistake | MoveChoiceVerdict.Blunder => true
      case _                                                                                   => false

  private def longTermCauseWithoutPrimary(diagnostic: CandidateComparisonDiagnostic): Boolean =
    !hasMoveJudgmentPrimaryCause(diagnostic) &&
      diagnostic.causeKinds.exists(kind => ClaimEventCluster.kindForCause(kind).isEmpty)

  private def comparisonGroupJson(diagnostics: List[CandidateComparisonDiagnostic]): JsObject =
    Json.obj(
      "comparisonCount" -> diagnostics.size,
      "comparisonIds" -> diagnostics.map(_.id)
    )

  private def noPrimaryInventoryGroupJson(
      diagnostics: List[CandidateComparisonDiagnostic],
      flows: List[RelativeCauseFlowDiagnostic]
  ): JsObject =
    Json.obj(
      "classification" -> "diagnostic_inventory",
      "comparisonCount" -> diagnostics.size,
      "comparisonIds" -> diagnostics.map(_.id),
      "inventory" -> noPrimaryInventoryJson(diagnostics, flows)
    )

  private def noPrimaryInventoryJson(
      diagnostics: List[CandidateComparisonDiagnostic],
      flows: List[RelativeCauseFlowDiagnostic]
  ): JsObject =
    Json.obj(
      "comparisonKindCounts" -> stringCountsJson(diagnostics.map(_.comparisonKind.toString)),
      "subjectBindingCounts" -> stringCountsJson(diagnostics.map(_.subjectBinding.toString)),
      "verdictCounts" -> stringCountsJson(diagnostics.map(_.verdict.toString)),
      "contributingCauseKindCounts" -> stringCountsJson(flows.map(_.causeKind.toString)),
      "contributingCauseRoleCounts" -> stringCountsJson(flows.map(_.causeRole.toString)),
      "contributingComparisonKindCounts" -> stringCountsJson(flows.map(_.causeComparisonKind.toString)),
      "coOccurringCauseKindCounts" -> stringCountsJson(diagnostics.flatMap(_.causeKinds.map(_.toString)))
    )

  private def auditFunnelMetrics: MoveReviewPhase3AuditFunnelMetrics =
    new MoveReviewPhase3AuditFunnelMetrics


  private def noEventCauseFlow(flow: RelativeCauseFlowDiagnostic): Boolean =
    auditFunnelMetrics.noEventCauseFlow(flow)




  private[qc] def semanticRubricExpectedSlotCoverageJson(
      expectedSlots: List[ExpectedSemanticSlot],
      diagnostics: List[CandidateComparisonDiagnostic],
      expectedQuestionIds: List[String] = Nil
  ): JsObject =
    auditFunnelMetrics.semanticRubricExpectedSlotCoverageJson(expectedSlots, diagnostics, expectedQuestionIds)

  private[qc] def semanticRubricExpectedSlotCorpusCoverageJson(coverages: List[JsValue]): JsObject =
    auditFunnelMetrics.semanticRubricExpectedSlotCorpusCoverageJson(coverages)

  private def stringCountsJson(values: List[String]): JsObject =
    MoveReviewPhase3AuditMetrics.stringCountsJson(values)
  private def openingApplicabilityDiagnosticsSummary(
      diagnostics: List[OpeningApplicabilityDiagnostic]
  ): JsArray =
    JsArray(
      diagnostics.map(diagnostic =>
        Json.obj(
          "id" -> diagnostic.id,
          "applicability" -> diagnostic.applicability.toString,
          "status" -> diagnostic.status.toString,
          "observedThemes" -> diagnostic.observedThemes.map(_.toString),
          "supportedThemes" -> diagnostic.supportedThemes.map(_.toString),
          "unverifiedPriorThemes" -> diagnostic.unverifiedPriorThemes.map(_.toString),
          "observedOnlyThemes" -> diagnostic.observedOnlyThemes.map(_.toString),
          "priorMatchSources" -> diagnostic.priorMatchSources.map(_.toString),
          "certifyingPriorPresent" -> diagnostic.certifyingPriorPresent,
          "anchorSourceLayers" -> diagnostic.anchorSourceLayers.map(_.toString),
          "anchorSignals" -> diagnostic.anchorSignals.map(_.toString),
          "supportedAnchorSourceLayers" -> diagnostic.supportedAnchorSourceLayers.map(_.toString),
          "supportedAnchorSignals" -> diagnostic.supportedAnchorSignals.map(_.toString),
          "internalAnchorAligned" -> diagnostic.internalAnchorAligned
        )
      )
    )

  private def openingSupportDiagnosticsSummary(
      diagnostics: List[OpeningSupportDiagnostic]
  ): JsArray =
    JsArray(
      diagnostics.map(diagnostic =>
        Json.obj(
          "contextIds" -> diagnostic.contextIds,
          "assessmentIds" -> diagnostic.assessmentIds,
          "anchorIds" -> diagnostic.anchorIds,
          "supportedAnchorIds" -> diagnostic.supportedAnchorIds,
          "ideaIds" -> diagnostic.ideaIds,
          "claimIds" -> diagnostic.claimIds,
          "candidateClaimIds" -> diagnostic.candidateClaimIds,
          "supportClusterIds" -> diagnostic.supportClusterIds,
          "identityPresent" -> diagnostic.identityPresent,
          "recognitionPresent" -> diagnostic.recognitionPresent,
          "themePriorPresent" -> diagnostic.themePriorPresent,
          "priorLineages" -> diagnostic.priorLineages,
          "requestedPriorLineages" -> diagnostic.requestedPriorLineages,
          "canonicalPriorLineages" -> diagnostic.canonicalPriorLineages,
          "priorMatchSources" -> diagnostic.priorMatchSources.map(_.toString),
          "openingSpecificPrior" -> diagnostic.openingSpecificPrior,
          "certifyingPriorPresent" -> diagnostic.certifyingPriorPresent,
          "certifyingPriorMatchSources" -> diagnostic.certifyingPriorMatchSources.map(_.toString),
          "priorFamilies" -> diagnostic.priorFamilies.map(_.toString),
          "typicalPawnStructures" -> diagnostic.typicalPawnStructures,
          "centerBreaks" -> diagnostic.centerBreaks,
          "developmentPriorities" -> diagnostic.developmentPriorities,
          "gambitCompensation" -> diagnostic.gambitCompensation,
          "strategicPlanPriors" -> diagnostic.strategicPlanPriors,
          "observedThemes" -> diagnostic.observedThemes.map(_.toString),
          "supportedThemes" -> diagnostic.supportedThemes.map(_.toString),
          "unverifiedPriorThemes" -> diagnostic.unverifiedPriorThemes.map(_.toString),
          "observedOnlyThemes" -> diagnostic.observedOnlyThemes.map(_.toString),
          "anchorSourceLayers" -> diagnostic.anchorSourceLayers.map(_.toString),
          "supportedAnchorSourceLayers" -> diagnostic.supportedAnchorSourceLayers.map(_.toString),
          "applicabilities" -> diagnostic.applicabilities.map(_.toString),
          "assessmentStatuses" -> diagnostic.assessmentStatuses.map(_.toString),
          "internalAnchorAligned" -> diagnostic.internalAnchorAligned
        )
      )
    )

  private def comparisonDiagnosticJson(diagnostic: CandidateComparisonDiagnostic): JsObject =
    Json.obj(
      "id" -> diagnostic.id,
      "comparisonFingerprint" -> diagnostic.comparisonFingerprint,
      "dedupeKey" -> diagnostic.dedupeKey,
      "dedupeClass" -> dedupeClassId(diagnostic.dedupeClass),
      "comparisonKind" -> diagnostic.comparisonKind.toString,
      "subjectBinding" -> diagnostic.subjectBinding.toString,
      "referenceLine" -> lineDiagnosticJson(diagnostic.referenceLine),
      "candidateLine" -> lineDiagnosticJson(diagnostic.candidateLine),
      "verdict" -> diagnostic.verdict.toString,
      "mover" -> diagnostic.mover,
      "rawCpLossForDiagnostics" -> diagnostic.rawCpLossForDiagnostics,
      "rawCandidateDeltaCpForDiagnostics" -> diagnostic.rawCandidateDeltaCpForDiagnostics,
      "winPercentLossForMover" -> diagnostic.winPercentLossForMover,
      "candidateWinPercentDeltaForMover" -> diagnostic.candidateWinPercentDeltaForMover,
      "causeKinds" -> diagnostic.causeKinds.map(_.toString),
      "causeRoles" -> diagnostic.causeRoles.map(_.toString),
      "causeSourceSides" -> diagnostic.causeSourceSides.map(_.toString),
      "causeImportances" -> diagnostic.causeImportances.map(_.toString),
      "causeEventLines" -> diagnostic.causeEventLines.map(lineRefSummary),
      "relativeCauseDiagnostics" -> relativeCauseDiagnosticsJson(diagnostic.relativeCauseDiagnostics),
      "moveJudgmentView" -> moveJudgmentViewDiagnosticJson(diagnostic.moveJudgmentView),
      "causeSupport" -> JsArray(
        diagnostic.causeSupport.map(support =>
          Json.obj(
            "id" -> support.id,
            "kind" -> support.kind.toString,
            "role" -> support.role.toString,
            "sourceSide" -> support.sourceSide.toString,
            "importance" -> support.importance.toString,
            "eventLine" -> lineRefSummary(support.eventLine),
            "evidenceLines" -> support.evidenceLines.map(lineRefSummary),
            "supportEvidenceIds" -> support.supportEvidenceIds,
            "parentEvidenceIds" -> support.parentEvidenceIds,
            "parentLayers" -> support.parentLayers.map(_.toString),
            "parentLayerSignature" -> support.parentLayerSignature,
            "semanticSupportKinds" -> support.semanticSupportKinds,
            "semanticSupportSignature" -> support.semanticSupportSignature,
            "hasOwnedTypedDepth" -> support.hasOwnedTypedDepth,
            "hasOwnedTacticalProof" -> support.hasOwnedTacticalProof,
            "hasOwnedStrategicContrastDepth" -> support.hasOwnedStrategicContrastDepth,
            "hasOwnedAdmissibleLongTermProof" -> support.hasOwnedAdmissibleLongTermProof,
            "rawProofHasDirectProof" -> support.rawProofHasDirectProof,
            "rawProofHasContrastProof" -> support.rawProofHasContrastProof,
            "rawProofHasContextSupport" -> support.rawProofHasContextSupport,
            "directProofSourceIds" -> support.directProofSourceIds,
            "contrastProofSourceIds" -> support.contrastProofSourceIds,
            "contextSupportSourceIds" -> support.contextSupportSourceIds,
            "directProofKinds" -> support.directProofKinds,
            "contrastProofKinds" -> support.contrastProofKinds,
            "contextSupportKinds" -> support.contextSupportKinds,
            "proofBoardAnchors" -> support.proofBoardAnchors.map(_.toString),
            "proofLineEvents" -> support.proofLineEvents.map(_.toString),
            "proofLineConsequences" -> support.proofLineConsequences.map(_.toString),
            "proofRelationKinds" -> support.proofRelationKinds.map(_.toString),
            "proofRelationDetails" -> support.proofRelationDetails,
            "proofRelationSourceIds" -> support.proofRelationSourceIds,
            "proofTacticalMechanismKinds" -> support.proofTacticalMechanismKinds.map(_.toString),
            "proofTacticalMechanismSourceIds" -> support.proofTacticalMechanismSourceIds,
            "proofStrategicAxisKeys" -> support.proofStrategicAxisKeys,
            "proofStrategicMechanismKinds" -> support.proofStrategicMechanismKinds.map(_.toString),
            "proofStrategicMechanismSourceIds" -> support.proofStrategicMechanismSourceIds,
            "proofStrategicMechanismSignalSourceIds" -> support.proofStrategicMechanismSignalSourceIds,
            "proofTransitionConsequences" -> support.proofTransitionConsequences.map(transitionConsequenceJson),
            "causeContextLayers" -> support.causeContextLayers.map(_.toString)
          )
        )
      ),
      "advisory" -> Json.obj(
        "causeHints" -> diagnostic.advisoryCauseHints.map(_.toString)
      ),
      "significanceReasons" -> diagnostic.significanceReasons.map(significanceReasonId),
      "lowSignalReasons" -> diagnostic.lowSignalReasons.map(lowSignalReasonId),
      "comparisonConfidence" -> diagnostic.comparisonConfidence.toString,
      "causeConfidences" -> diagnostic.causeConfidences.map(_.toString),
      "hasLowDepthCause" -> diagnostic.hasLowDepthCause,
      "hasUnexplainedEngineGap" -> diagnostic.hasUnexplainedEngineGap,
      "hasSecondaryContextEngineGap" -> diagnostic.hasSecondaryContextEngineGap,
      "evidenceLayers" -> Json.obj(
        "reference" -> evidenceLayerNeighborhoodJson(diagnostic.evidenceLayers.reference),
        "candidate" -> evidenceLayerNeighborhoodJson(diagnostic.evidenceLayers.candidate)
      ),
      "decisionTrace" -> causeDecisionTraceJson(diagnostic.decisionTrace),
      "tacticalLossTrace" -> tacticalLossTraceJson(diagnostic.tacticalLossTrace),
      "failureClass" -> failureClassId(diagnostic.failureClass),
      "failureReasons" -> diagnostic.failureReasons.map(failureReasonId)
    )

  private def moveJudgmentViewDiagnosticJson(diagnostic: ComparisonMoveJudgmentViewDiagnostics): JsObject =
    Json.obj(
      "hasPrimaryCause" -> diagnostic.hasPrimaryCause,
      "primaryCauseKinds" -> diagnostic.primaryCauseKinds.map(_.toString),
      "secondaryCauseKinds" -> diagnostic.secondaryCauseKinds.map(_.toString),
      "contextCauseKinds" -> diagnostic.contextCauseKinds.map(_.toString),
      "primaryCauseEvidenceIds" -> diagnostic.primaryCauseEvidenceIds,
      "primaryIdeaFamilies" -> diagnostic.primaryIdeaFamilies.map(_.toString).toList.sorted,
      "primaryClaimCandidateFamilies" -> diagnostic.primaryClaimCandidateFamilies.map(_.toString).toList.sorted,
      "primaryFinalClaimFamilies" -> diagnostic.primaryFinalClaimFamilies.map(_.toString).toList.sorted,
      "primaryFramedCauseKinds" -> diagnostic.primaryFramedCauseKinds.map(_.toString),
      "primaryUnframedCauseKinds" -> diagnostic.primaryUnframedCauseKinds.map(_.toString),
      "primaryRootCauseKinds" -> diagnostic.primaryRootCauseKinds.map(_.toString),
      "rootArbitrationTiers" -> diagnostic.rootArbitrationTiers.map(_.toString),
      "rootArbitrationTierCounts" -> stringCountsJson(diagnostic.rootArbitrationTiers.map(_.toString)),
      "primaryRootArbitrationTiers" -> diagnostic.primaryRootArbitrationTiers.map(_.toString),
      "primaryRootArbitrationTierCounts" -> stringCountsJson(diagnostic.primaryRootArbitrationTiers.map(_.toString)),
      "primaryTacticalWitnessCauseKinds" -> diagnostic.primaryTacticalWitnessCauseKinds.map(_.toString),
      "primaryPunishmentWitnessCauseKinds" -> diagnostic.primaryPunishmentWitnessCauseKinds.map(_.toString),
      "primaryContextualTacticalWitnessCauseKinds" -> diagnostic.primaryContextualTacticalWitnessCauseKinds.map(_.toString),
      "primaryRootCauseEvidenceIds" -> diagnostic.primaryRootCauseEvidenceIds,
      "primaryTacticalWitnessCauseEvidenceIds" -> diagnostic.primaryTacticalWitnessCauseEvidenceIds,
      "primaryPunishmentWitnessCauseEvidenceIds" -> diagnostic.primaryPunishmentWitnessCauseEvidenceIds,
      "primaryContextualTacticalWitnessCauseEvidenceIds" -> diagnostic.primaryContextualTacticalWitnessCauseEvidenceIds,
      "secondaryCauseEvidenceIds" -> diagnostic.secondaryCauseEvidenceIds,
      "contextCauseEvidenceIds" -> diagnostic.contextCauseEvidenceIds,
      "projectedContextCauseNoViewIds" -> diagnostic.projectedContextCauseNoViewIds,
      "playableLossPrimaryCauseEvidenceIds" -> diagnostic.playableLossPrimaryCauseEvidenceIds,
      "objectlessPrimaryCauseEvidenceIds" -> diagnostic.objectlessPrimaryCauseEvidenceIds,
      "objectlessSecondaryCauseEvidenceIds" -> diagnostic.objectlessSecondaryCauseEvidenceIds,
      "objectlessContextCauseEvidenceIds" -> diagnostic.objectlessContextCauseEvidenceIds,
      "positionPlanTechniqueFrameIds" -> diagnostic.positionPlanTechniqueFrameIds,
      "positionPlanTechniqueUnits" -> diagnostic.positionPlanTechniqueUnits.map(_.toString),
      "positionPlanTechniqueAxisKeys" -> diagnostic.positionPlanTechniqueAxisKeys,
      "positionPlanTechniqueSemanticDetailUnits" -> diagnostic.positionPlanTechniqueSemanticDetailUnits.map(_.toString),
      "positionPlanTechniqueSemanticDetailAxisKeys" -> diagnostic.positionPlanTechniqueSemanticDetailAxisKeys,
      "positionPlanTechniqueSemanticDetailMechanismKinds" ->
        diagnostic.positionPlanTechniqueSemanticDetailMechanismKinds.map(_.toString),
      "positionPlanTechniqueSemanticDetailAnchorKeys" -> diagnostic.positionPlanTechniqueSemanticDetailAnchorKeys,
      "positionPlanTechniqueSemanticDetailTokens" -> diagnostic.positionPlanTechniqueSemanticDetailTokens,
      "positionPlanTechniqueSemanticDetailTokenGroups" -> JsArray(
        diagnostic.positionPlanTechniqueSemanticDetailTokenGroups.map(tokens => JsArray(tokens.map(JsString.apply)))
      ),
      "positionPlanTechniqueEvidenceIds" -> diagnostic.positionPlanTechniqueEvidenceIds,
      "positionPlanTechniqueRelativeCauseEvidenceIds" -> diagnostic.positionPlanTechniqueRelativeCauseEvidenceIds,
      "moveMeaningClaimKinds" -> diagnostic.moveMeaningClaimKinds,
      "moveMeaningClaimRoles" -> diagnostic.moveMeaningClaimRoles,
      "moveMeaningClaimSupportLevels" -> diagnostic.moveMeaningClaimSupportLevels,
      "moveMeaningClaimVisibility" -> diagnostic.moveMeaningClaimVisibility,
      "moveMeaningClaimSurfaceLanes" -> diagnostic.moveMeaningClaimSurfaceLanes,
      "moveMeaningClaimLaneKeys" -> diagnostic.moveMeaningClaimLaneKeys,
      "moveMeaningClaimDiagnostics" -> diagnostic.moveMeaningClaimDiagnostics.map(publicMoveMeaningClaimDiagnosticJson),
      "publicMoveMeaningClaimDiagnostics" -> diagnostic.publicMoveMeaningClaimDiagnostics.map(publicMoveMeaningClaimDiagnosticJson)
    )

  private def publicMoveMeaningClaimDiagnosticJson(diagnostic: PublicMoveMeaningClaimDiagnostic): JsObject =
    Json.obj(
      "unit" -> diagnostic.unit.toString,
      "axisKey" -> diagnostic.axisKey,
      "meaningKind" -> diagnostic.meaningKind,
      "supportLevel" -> diagnostic.supportLevel,
      "surfaceLane" -> diagnostic.surfaceLane,
      "lineRole" -> diagnostic.lineRole,
      "moveUci" -> diagnostic.moveUci,
      "causeEvidenceIds" -> diagnostic.causeEvidenceIds,
      "sourceEvidenceIds" -> diagnostic.sourceEvidenceIds,
      "hasCarrier" -> diagnostic.hasCarrier,
      "hasBoardCarrier" -> diagnostic.hasBoardCarrier,
      "proofLevel" -> diagnostic.proofLevel,
      "boardCarriers" -> diagnostic.boardCarriers.map(carrier =>
        Json.obj(
          "role" -> carrier.role,
          "kind" -> carrier.kind,
          "value" -> carrier.value,
          "from" -> carrier.from,
          "to" -> carrier.to
        )
      ),
      "targetSquares" -> diagnostic.targetSquares,
      "targetFiles" -> diagnostic.targetFiles,
      "targetPieces" -> diagnostic.targetPieces,
      "routeIdentityParts" -> diagnostic.routeIdentityParts,
      "proofRelationKinds" -> diagnostic.proofRelationKinds.map(_.toString),
      "proofRelationDetails" -> diagnostic.proofRelationDetails
    )

  private def relativeCauseDiagnosticsJson(diagnostic: ComparisonRelativeCauseDiagnostics): JsObject =
    Json.obj(
      "expectedCauseHints" -> diagnostic.expectedCauseHints.map(_.toString),
      "missingExpectedCauseHints" -> diagnostic.missingExpectedCauseHints.map(_.toString),
      "producedCauseIds" -> diagnostic.producedCauseIds,
      "producedCauseKinds" -> diagnostic.producedCauseKinds.map(_.toString),
      "producedCauseRoles" -> diagnostic.producedCauseRoles.map(_.toString),
      "producedCauseSourceSides" -> diagnostic.producedCauseSourceSides.map(_.toString),
      "producedCauseEventLines" -> diagnostic.producedCauseEventLines.map(lineRefSummary),
      "missingCause" -> diagnostic.missingCause,
      "shallowProofCauseIds" -> diagnostic.shallowProofCauseIds,
      "ownedTypedDepthCauseIds" -> diagnostic.ownedTypedDepthCauseIds,
      "unboundEvidenceIds" -> diagnostic.unboundEvidenceIds,
      "wrongRoleCauseIds" -> diagnostic.wrongRoleCauseIds,
      "wrongSourceSideCauseIds" -> diagnostic.wrongSourceSideCauseIds,
      "wrongEventLineCauseIds" -> diagnostic.wrongEventLineCauseIds,
      "wrongImportanceCauseIds" -> diagnostic.wrongImportanceCauseIds,
      "causeWithoutIdeaIds" -> diagnostic.causeWithoutIdeaIds,
      "ideaWithoutClaimCandidateCauseIds" -> diagnostic.ideaWithoutClaimCandidateCauseIds,
      "ideaWithoutClaimCauseIds" -> diagnostic.ideaWithoutClaimCauseIds,
      "ideaWithoutFinalClaimCauseIds" -> diagnostic.ideaWithoutFinalClaimCauseIds,
      "claimWithoutEventClusterCauseIds" -> diagnostic.claimWithoutEventClusterCauseIds,
      "eventClusterSupportMissingCauseIds" -> diagnostic.eventClusterSupportMissingCauseIds,
      "strategicCauseWithoutContrastIds" -> diagnostic.strategicCauseWithoutContrastIds,
      "strategicClaimWithoutComparativeCauseIds" -> diagnostic.strategicClaimWithoutComparativeCauseIds,
      "genericStructuralImprovementWithoutStrategicContrastIds" ->
        diagnostic.genericStructuralImprovementWithoutStrategicContrastIds,
      "contextSupportUsedAsDirectProofIds" -> diagnostic.contextSupportUsedAsDirectProofIds,
      "contextOnlyCauseIds" -> diagnostic.contextOnlyCauseIds,
      "unattributedCauseIds" -> diagnostic.unattributedCauseIds,
      "rootMismatchedCauseIds" -> diagnostic.rootMismatchedCauseIds,
      "supportPromotedToDirectProofCauseIds" -> diagnostic.supportPromotedToDirectProofCauseIds,
      "relativeCauseWithoutConcreteObjectIds" -> diagnostic.relativeCauseWithoutObjectSignatureIds,
      "graphObjectLostBetweenEvidenceAndCauseIds" -> diagnostic.objectLostBetweenEvidenceAndCauseIds,
      "graphObjectLostBetweenCauseAndClaimIds" -> diagnostic.objectLostBetweenCauseAndClaimIds,
      "causeFlow" -> JsArray(diagnostic.causeFlow.map(relativeCauseFlowJson))
    )

  private def relativeCauseFlowJson(flow: RelativeCauseFlowDiagnostic): JsObject =
    Json.obj(
      "causeId" -> flow.causeId,
      "causeKind" -> flow.causeKind.toString,
      "causeRole" -> flow.causeRole.toString,
      "causeComparisonKind" -> flow.causeComparisonKind.toString,
      "causeSourceSide" -> flow.causeSourceSide.toString,
      "causeEventLine" -> lineRefSummary(flow.causeEventLine),
      "proofStrategicAxisKeys" -> flow.proofStrategicAxisKeys,
      "claimCandidateFamilies" -> flow.claimCandidateFamilies.map(_.toString).toList.sorted,
      "finalClaimFamilies" -> flow.finalClaimFamilies.map(_.toString).toList.sorted,
      "directProofSourceIds" -> flow.directProofSourceIds,
      "contrastProofSourceIds" -> flow.contrastProofSourceIds,
      "contextSupportSourceIds" -> flow.contextSupportSourceIds,
      "directProofKinds" -> flow.directProofKinds,
      "contrastProofKinds" -> flow.contrastProofKinds,
      "contextSupportKinds" -> flow.contextSupportKinds,
      "hasOwnedTypedDepth" -> flow.hasOwnedTypedDepth,
      "hasOwnedAdmissibleLongTermProof" -> flow.hasOwnedAdmissibleLongTermProof,
      "eventClusterExpected" -> flow.eventClusterExpected,
      "ideaIds" -> flow.ideaIds,
      "claimCandidateIds" -> flow.claimCandidateIds,
      "claimCandidateStages" -> flow.claimCandidateStages.map(_.toString).toList.sorted,
      "claimCandidateTruthStatuses" -> flow.claimCandidateTruthStatuses.map(_.toString).toList.sorted,
      "claimCandidateDroppedStages" -> flow.claimCandidateDroppedStages.map(_.toString).toList.sorted,
      "claimIds" -> flow.claimIds,
      "eventClusterIds" -> flow.eventClusterIds,
      "eventClusterMissingSupportEvidenceIds" -> flow.eventClusterMissingSupportEvidenceIds,
      "causeWithoutIdea" -> flow.causeWithoutIdea,
      "ideaWithoutClaimCandidate" -> flow.ideaWithoutClaimCandidate,
      "ideaWithoutFinalClaim" -> flow.ideaWithoutFinalClaim,
      "claimWithoutEventCluster" -> flow.claimWithoutEventCluster,
      "strategicCauseWithoutContrast" -> flow.strategicCauseWithoutContrast,
      "contextSupportUsedAsDirectProof" -> flow.contextSupportUsedAsDirectProof,
      "contextOnlyAttribution" -> flow.contextOnlyAttribution,
      "unattributedCause" -> flow.unattributedCause,
      "rootMismatchedAttribution" -> flow.rootMismatchedAttribution,
      "supportPromotedToDirectProof" -> flow.supportPromotedToDirectProof,
      "relativeCauseWithoutConcreteObject" -> flow.relativeCauseWithoutObjectSignature,
      "graphObjectLostBetweenEvidenceAndCause" -> flow.objectLostBetweenEvidenceAndCause,
      "graphObjectLostBetweenCauseAndClaim" -> flow.objectLostBetweenCauseAndClaim
    )

  private def tacticalLossTraceJson(trace: TacticalLossTrace): JsObject =
    Json.obj(
      "applicable" -> trace.applicable,
      "signalWinPercent" -> trace.signalWinPercent,
      "blockedAt" -> trace.blockedAt.map(tacticalLossStageId),
      "lastPresentStage" -> trace.lastPresentStage.map(tacticalLossStageId),
      "stages" -> JsArray(
        trace.stages.map(stage =>
          Json.obj(
            "stage" -> tacticalLossStageId(stage.stage),
            "present" -> stage.present,
            "evidenceIds" -> stage.evidenceIds,
            "detailKinds" -> stage.detailKinds
          )
        )
      )
    )

  private def tacticalLossStageId(stage: TacticalLossStage): String =
    stage match
      case TacticalLossStage.LineTacticalSignal => "line_tactical_signal"
      case TacticalLossStage.MoveMotifEvent     => "move_motif_event"
      case TacticalLossStage.RelationWitness    => "relation_witness"
      case TacticalLossStage.ThreatEpisode      => "threat_episode"
      case TacticalLossStage.TacticalMechanism  => "tactical_mechanism"
      case TacticalLossStage.RelativeCause      => "relative_cause"
      case TacticalLossStage.TacticalIdea       => "tactical_idea"
      case TacticalLossStage.TacticalClaim      => "tactical_claim"

  private def causeDecisionTraceJson(trace: CandidateCauseDecisionTrace): JsObject =
    Json.obj(
      "badLoss" -> trace.badLoss,
      "tacticalLoss" -> trace.tacticalLoss,
      "majorLoss" -> trace.majorLoss,
      "candidateBetter" -> trace.candidateBetter,
      "requiresExplanatoryCause" -> trace.requiresExplanatoryCause,
      "positiveContextAlternative" -> trace.positiveContextAlternative,
      "referenceTacticalMechanismKinds" -> trace.referenceTacticalMechanismKinds.map(_.toString),
      "candidateTacticalMechanismKinds" -> trace.candidateTacticalMechanismKinds.map(_.toString),
      "referenceConcreteLine" -> trace.referenceConcreteLine,
      "candidateConcreteLine" -> trace.candidateConcreteLine,
      "candidateTacticalRefutationBridge" -> trace.candidateTacticalRefutationBridge,
      "referenceTacticalRisk" -> trace.referenceTacticalRisk,
      "hasOnlyDefense" -> trace.hasOnlyDefense,
      "hasThreatResource" -> trace.hasThreatResource,
      "referenceProphylacticResource" -> trace.referenceProphylacticResource,
      "referenceKingStepResource" -> trace.referenceKingStepResource,
      "candidateKingStepResource" -> trace.candidateKingStepResource,
      "referenceCastlingResource" -> trace.referenceCastlingResource,
      "candidateCastlingResource" -> trace.candidateCastlingResource,
      "referencePreventivePawnResource" -> trace.referencePreventivePawnResource,
      "candidatePreventivePawnResource" -> trace.candidatePreventivePawnResource,
      "hasConversionWindow" -> trace.hasConversionWindow,
      "referenceConversionWindow" -> trace.referenceConversionWindow,
      "candidateConversionWindow" -> trace.candidateConversionWindow,
      "referenceStructuralTargetRelease" -> trace.referenceStructuralTargetRelease,
      "referenceReleasedTargets" -> trace.referenceReleasedTargets,
      "candidateReleasedTargets" -> trace.candidateReleasedTargets,
      "referenceCreatedTargets" -> trace.referenceCreatedTargets,
      "candidateCreatedTargets" -> trace.candidateCreatedTargets,
      "referenceStructuralImprovement" -> trace.referenceStructuralImprovement,
      "candidateStructuralImprovement" -> trace.candidateStructuralImprovement,
      "candidatePawnStructureImprovement" -> trace.candidatePawnStructureImprovement,
      "candidateTargetPressureGain" -> trace.candidateTargetPressureGain,
      "candidateCenterControlGain" -> trace.candidateCenterControlGain,
      "candidateDevelopmentActivation" -> trace.candidateDevelopmentActivation,
      "candidatePieceActivityGain" -> trace.candidatePieceActivityGain,
      "sameDestinationCaptureChoice" -> trace.sameDestinationCaptureChoice,
      "referenceStructuralSignals" -> trace.referenceStructuralSignals,
      "candidateStructuralSignals" -> trace.candidateStructuralSignals,
      "referenceStructuralConsequences" -> trace.referenceStructuralConsequences.map(_.toString),
      "candidateStructuralConsequences" -> trace.candidateStructuralConsequences.map(_.toString),
      "candidatePawnStructureSignals" -> trace.candidatePawnStructureSignals,
      "referenceSemanticAxisCount" -> trace.referenceSemanticAxisCount,
      "candidateSemanticAxisCount" -> trace.candidateSemanticAxisCount,
      "referenceSemanticAxisLead" -> trace.referenceSemanticAxisLead,
      "semanticAxisDiagnostics" -> semanticAxisDiagnosticsJson(trace.semanticAxisDiagnostics),
      "candidatePlanEvidence" -> trace.candidatePlanEvidence,
      "candidateStrategicEvidence" -> trace.candidateStrategicEvidence,
      "candidateStrategicConcessionEvidence" -> trace.candidateStrategicConcessionEvidence,
      "materialSwingEvidence" -> trace.materialSwingEvidence,
      "referenceMaterialNetCp" -> trace.referenceMaterialNetCp,
      "candidateMaterialNetCp" -> trace.candidateMaterialNetCp,
      "referenceMaterialMaxGainCp" -> trace.referenceMaterialMaxGainCp,
      "candidateMaterialMaxGainCp" -> trace.candidateMaterialMaxGainCp,
      "referenceMaterialPromotionGainCp" -> trace.referenceMaterialPromotionGainCp,
      "candidateMaterialPromotionGainCp" -> trace.candidateMaterialPromotionGainCp,
      "referenceMaterialRecapture" -> trace.referenceMaterialRecapture,
      "candidateMaterialRecapture" -> trace.candidateMaterialRecapture,
      "referenceMaterialRecovery" -> trace.referenceMaterialRecovery,
      "candidateMaterialRecovery" -> trace.candidateMaterialRecovery,
      "referenceMaterialComplete" -> trace.referenceMaterialComplete,
      "candidateMaterialComplete" -> trace.candidateMaterialComplete,
      "referenceRelationKinds" -> trace.referenceRelationKinds.map(_.toString),
      "candidateRelationKinds" -> trace.candidateRelationKinds.map(_.toString),
      "relationKinds" -> trace.relationKinds.map(_.toString),
      "referenceMotifs" -> trace.referenceMotifs,
      "candidateMotifs" -> trace.candidateMotifs
    )

  private def semanticAxisDiagnosticsJson(diagnostic: SemanticAxisDiagnostics): JsObject =
    Json.obj(
      "referenceAxisStrengths" -> intMapJson(diagnostic.referenceAxisStrengths),
      "candidateAxisStrengths" -> intMapJson(diagnostic.candidateAxisStrengths),
      "referenceAxisSourceIds" -> stringListMapJson(diagnostic.referenceAxisSourceIds),
      "candidateAxisSourceIds" -> stringListMapJson(diagnostic.candidateAxisSourceIds),
      "referenceAxisSourceLayers" -> stringListMapJson(
        diagnostic.referenceAxisSourceLayers.view.mapValues(_.map(_.toString)).toMap
      ),
      "candidateAxisSourceLayers" -> stringListMapJson(
        diagnostic.candidateAxisSourceLayers.view.mapValues(_.map(_.toString)).toMap
      ),
      "referenceAxes" -> diagnostic.referenceAxes,
      "candidateAxes" -> diagnostic.candidateAxes,
      "sharedAxes" -> diagnostic.sharedAxes,
      "referenceOnlyAxes" -> diagnostic.referenceOnlyAxes,
      "candidateOnlyAxes" -> diagnostic.candidateOnlyAxes,
      "referenceLeadAxes" -> diagnostic.referenceLeadAxes,
      "candidateLeadAxes" -> diagnostic.candidateLeadAxes,
      "referenceLeadAxisCount" -> diagnostic.referenceLeadAxisCount,
      "candidateLeadAxisCount" -> diagnostic.candidateLeadAxisCount,
      "semanticDeltaAxisCount" -> diagnostic.semanticDeltaAxisCount
    )

  private def intMapJson(values: Map[String, Int]): JsObject =
    JsObject(values.toList.sortBy(_._1).map { case (key, value) => key -> JsNumber(value) })

  private def stringListMapJson(values: Map[String, List[String]]): JsObject =
    JsObject(
      values.toList
        .sortBy(_._1)
        .map { case (key, value) => key -> JsArray(value.sorted.map(JsString.apply)) }
    )

  private def lineDiagnosticJson(line: CandidateLineDiagnostic): JsObject =
    Json.obj(
      "id" -> line.id,
      "rootMove" -> line.rootMove,
      "role" -> line.role.toString,
      "rank" -> line.rank,
      "moves" -> line.moves,
      "whitePovEvalCp" -> line.whitePovEvalCp,
      "mate" -> line.mate,
      "depth" -> line.depth
    )

  private def evidenceLayerNeighborhoodJson(neighborhood: EvidenceLayerNeighborhood): JsObject =
    Json.obj(
      "directLayers" -> neighborhood.directLayers.map(_.toString).toList.sorted,
      "directLayerCounts" -> layerCountsJson(neighborhood.directLayerCounts),
      "parentLayerCounts" -> layerCountsJson(neighborhood.parentLayerCounts)
    )

  private def layerCountsJson(counts: Map[EvidenceLayer, Int]): JsObject =
    JsObject(
      counts.toList
        .sortBy(_._1.toString)
        .map((layer, count) => layer.toString -> JsNumber(count))
    )

  private def failureClassId(failureClass: CandidateComparisonFailureClass): String =
    failureClass match
      case CandidateComparisonFailureClass.NoFailure                 => "none"
      case CandidateComparisonFailureClass.LowSignalEnginePreference => "low_signal_engine_preference"
      case CandidateComparisonFailureClass.SecondaryContextGap       => "secondary_context_gap"
      case CandidateComparisonFailureClass.MissingEvidence           => "missing_evidence"
      case CandidateComparisonFailureClass.UnboundEvidence           => "unbound_evidence"
      case CandidateComparisonFailureClass.LowDepthCause             => "low_depth_cause"
      case CandidateComparisonFailureClass.FailedCauseTemplate       => "failed_cause_template"
      case CandidateComparisonFailureClass.UnknownChessPattern       => "unknown_chess_pattern"

  private def failureReasonId(reason: CandidateComparisonFailureReason): String =
    reason match
      case CandidateComparisonFailureReason.LowSignalEnginePreference => "low_signal_engine_preference"
      case CandidateComparisonFailureReason.MissingLineEvidence       => "missing_line_evidence"
      case CandidateComparisonFailureReason.MissingEvalEvidence       => "missing_eval_evidence"
      case CandidateComparisonFailureReason.NoCauseGenerated          => "no_cause_generated"
      case CandidateComparisonFailureReason.GeneratedOnlyUnexplained  => "generated_only_unexplained"
      case CandidateComparisonFailureReason.TacticalEvidenceUnbound   => "tactical_evidence_unbound"
      case CandidateComparisonFailureReason.DefensiveEvidenceUnbound  => "defensive_evidence_unbound"
      case CandidateComparisonFailureReason.ConversionEvidenceUnbound => "conversion_evidence_unbound"
      case CandidateComparisonFailureReason.StrategicEvidenceUnbound  => "strategic_evidence_unbound"
      case CandidateComparisonFailureReason.LatentTacticalEvidence    => "latent_tactical_evidence"
      case CandidateComparisonFailureReason.LatentMaterialEvidence    => "latent_material_evidence"
      case CandidateComparisonFailureReason.LatentStrategicEvidence   => "latent_strategic_evidence"
      case CandidateComparisonFailureReason.TacticalEvidenceBelowThreshold => "tactical_evidence_below_threshold"
      case CandidateComparisonFailureReason.MaterialEvidenceBelowThreshold => "material_evidence_below_threshold"
      case CandidateComparisonFailureReason.StrategicEvidenceBelowThreshold => "strategic_evidence_below_threshold"
      case CandidateComparisonFailureReason.TacticalSignalNeedsWidthDepth => "tactical_signal_needs_width_depth"
      case CandidateComparisonFailureReason.StrategicContextBelowSignalFloor => "strategic_context_below_signal_floor"
      case CandidateComparisonFailureReason.StrategicEvidenceBelowCauseThreshold =>
        "strategic_evidence_below_cause_threshold"
      case CandidateComparisonFailureReason.PrimaryStrategicNearThresholdUnderbinding =>
        "primary_strategic_near_threshold_underbinding"
      case CandidateComparisonFailureReason.PrimaryStrategicSignalNeedsWidthDepth =>
        "primary_strategic_signal_needs_width_depth"
      case CandidateComparisonFailureReason.ContextAlternativeStrategicNearThreshold =>
        "context_alternative_strategic_near_threshold"
      case CandidateComparisonFailureReason.LowSignalTacticalContext  => "low_signal_tactical_context"
      case CandidateComparisonFailureReason.LowSignalMaterialContext  => "low_signal_material_context"
      case CandidateComparisonFailureReason.LowSignalStrategicContext => "low_signal_strategic_context"
      case CandidateComparisonFailureReason.LowDepthGeneratedCause    => "low_depth_generated_cause"
      case CandidateComparisonFailureReason.UnboundEvidenceWithGeneratedCause =>
        "unbound_evidence_with_generated_cause"
      case CandidateComparisonFailureReason.ContextCauseProjectionMissing => "context_cause_projection_missing"
      case CandidateComparisonFailureReason.PlayableLossPrimaryOverclaim  => "playable_loss_primary_overclaim"
      case CandidateComparisonFailureReason.ContextAlternativeComparison => "context_alternative_comparison"
      case CandidateComparisonFailureReason.NoSpecificEvidence        => "no_specific_evidence"
      case CandidateComparisonFailureReason.UnknownPattern            => "unknown_pattern"

  private def significanceReasonId(reason: CandidateComparisonSignificanceReason): String =
    reason match
      case CandidateComparisonSignificanceReason.PlayedLoss        => "played_loss"
      case CandidateComparisonSignificanceReason.CandidateImproves => "candidate_improves"
      case CandidateComparisonSignificanceReason.OnlyMove          => "only_move"

  private def lowSignalReasonId(reason: CandidateComparisonLowSignalReason): String =
    reason match
      case CandidateComparisonLowSignalReason.QuietNoDirection              => "quiet_no_direction"
      case CandidateComparisonLowSignalReason.BelowSignalFloor              => "below_signal_floor"
      case CandidateComparisonLowSignalReason.BelowBindingThreshold         => "below_binding_threshold"
      case CandidateComparisonLowSignalReason.NearThresholdPrimaryPlayed    => "near_threshold_primary_played"
      case CandidateComparisonLowSignalReason.NearThresholdContextAlternative =>
        "near_threshold_context_alternative"
      case CandidateComparisonLowSignalReason.ContextAlternative            => "context_alternative"

  private def dedupeClassId(dedupeClass: CandidateComparisonDedupeClass): String =
    dedupeClass match
      case CandidateComparisonDedupeClass.Unique           => "unique"
      case CandidateComparisonDedupeClass.DuplicateEpisode => "duplicate_episode"

  private def layerGapSummary(result: MoveReviewJudgmentResult): JsArray =
    JsArray(
      result.quality.layerGaps.layers.map(layer =>
        Json.obj(
          "layer" -> layer.layer.toString,
          "totalSlots" -> layer.totalSlots,
          "presentSlots" -> layer.presentSlots,
          "missingSlots" -> layer.missingSlots.map(_.slot.toString),
          "missingSlotDiagnostics" -> JsArray(
            layer.missingSlots.map(slot =>
              Json.obj(
                "slot" -> slot.slot.toString,
                "owner" -> slot.owner.toString,
                "applicable" -> slot.applicable,
                "upstreamCandidates" -> gapUpstreamCandidates(result, slot.slot)
              )
            )
          ),
          "gapPercent" -> layer.gapPercent
        )
      )
    )

  private def issueDetails(result: MoveReviewJudgmentResult): JsArray =
    JsArray(
      result.quality.audit.issues.map(issue =>
        Json.obj(
          "kind" -> issue.kind.toString,
          "subjectId" -> issue.subjectId,
          "validationKind" -> issue.validationKind.map(_.toString),
          "evidence" -> issue.evidence.map(evidenceRefSummary),
          "relatedComparisonIds" -> relatedComparisonIds(result, issue.subjectId)
        )
      )
    )

  private def validationIssueDetails(result: MoveReviewJudgmentResult): JsArray =
    JsArray(
      result.quality.validation.issues.map(issue =>
        Json.obj(
          "kind" -> issue.kind.toString,
          "subjectId" -> issue.subjectId,
          "evidence" -> issue.evidence.map(evidenceRefSummary),
          "payload" -> evidencePayloadSummary(result, issue.subjectId),
          "relatedComparisonIds" -> relatedComparisonIds(result, issue.subjectId)
        )
      )
    )

  private def gapUpstreamCandidates(
      result: MoveReviewJudgmentResult,
      slot: JudgmentGraphSlot
  ): JsArray =
    val candidates =
      slot match
        case JudgmentGraphSlot.BoardFact =>
          evidenceCandidates(result, Set(EvidenceLayer.Board))
        case JudgmentGraphSlot.BoardAnchorFact =>
          evidenceCandidates(result, Set(EvidenceLayer.Board))
        case JudgmentGraphSlot.SinglePositionFact | JudgmentGraphSlot.BeforeSinglePositionFact |
            JudgmentGraphSlot.AfterPlayedSinglePositionFact | JudgmentGraphSlot.AfterReferenceSinglePositionFact =>
          evidenceCandidates(result, Set(EvidenceLayer.SinglePosition, EvidenceLayer.Board))
        case JudgmentGraphSlot.PawnStructureFact =>
          evidenceCandidates(result, Set(EvidenceLayer.PawnStructure, EvidenceLayer.Board, EvidenceLayer.StructuralDelta))
        case JudgmentGraphSlot.ThreatPressureFact | JudgmentGraphSlot.ThreatEpisodeFact | JudgmentGraphSlot.DefensiveIdea =>
          evidenceCandidates(result, Set(EvidenceLayer.ThreatPressure, EvidenceLayer.Line, EvidenceLayer.Eval))
        case JudgmentGraphSlot.LineFact | JudgmentGraphSlot.LegalReplayLineFact | JudgmentGraphSlot.LineReplayFact |
            JudgmentGraphSlot.LineEventFact | JudgmentGraphSlot.LineConsequenceFact =>
          evidenceCandidates(result, Set(EvidenceLayer.Line))
        case JudgmentGraphSlot.EvalFact =>
          evidenceCandidates(result, Set(EvidenceLayer.Eval))
        case JudgmentGraphSlot.MoveMotifFact =>
          evidenceCandidates(result, Set(EvidenceLayer.MoveMotif, EvidenceLayer.Line))
        case JudgmentGraphSlot.MoveTransitionFact =>
          evidenceCandidates(result, Set(EvidenceLayer.MoveTransition))
        case JudgmentGraphSlot.RelationFact =>
          evidenceCandidates(result, Set(EvidenceLayer.Relation, EvidenceLayer.MoveMotif, EvidenceLayer.Line))
        case JudgmentGraphSlot.StructuralDeltaFact =>
          evidenceCandidates(result, Set(EvidenceLayer.StructuralDelta, EvidenceLayer.MoveTransition, EvidenceLayer.Board))
        case JudgmentGraphSlot.StructuralSignalFact =>
          evidenceCandidates(result, Set(EvidenceLayer.StructuralDelta, EvidenceLayer.MoveTransition, EvidenceLayer.Board))
        case JudgmentGraphSlot.StructuralConsequenceFact =>
          evidenceCandidates(result, Set(EvidenceLayer.StructuralDelta, EvidenceLayer.MoveTransition, EvidenceLayer.Board))
        case JudgmentGraphSlot.StructuralMeaningfulConsequenceFact =>
          evidenceCandidates(result, Set(EvidenceLayer.StructuralDelta, EvidenceLayer.MoveTransition, EvidenceLayer.Board))
        case JudgmentGraphSlot.StrategicFact =>
          evidenceCandidates(result, Set(EvidenceLayer.Strategic, EvidenceLayer.Board, EvidenceLayer.StructuralDelta))
        case JudgmentGraphSlot.OpeningContextFact =>
          evidenceCandidates(result, Set(EvidenceLayer.OpeningContext))
        case JudgmentGraphSlot.FeatureAnchorFact | JudgmentGraphSlot.ApplicabilityAssessmentFact |
            JudgmentGraphSlot.OpeningIdea | JudgmentGraphSlot.OpeningClaim =>
          evidenceCandidates(
            result,
            Set(EvidenceLayer.FeatureAnchor, EvidenceLayer.ApplicabilityAssessment, EvidenceLayer.OpeningContext)
          ) ++ ideaCandidates(result, Set(ChessIdeaFamily.Opening))
        case JudgmentGraphSlot.PlanPressureFact =>
          evidenceCandidates(result, Set(EvidenceLayer.PlanPressure, EvidenceLayer.StrategicMechanism)) ++
            ideaCandidates(result, Set(ChessIdeaFamily.Strategic))
        case JudgmentGraphSlot.PlanTransitionFact =>
          evidenceCandidates(result, Set(EvidenceLayer.PlanTransition, EvidenceLayer.StrategicMechanism))
        case JudgmentGraphSlot.PlanClaim =>
          evidenceCandidates(result, Set(EvidenceLayer.StrategicMechanism)) ++
            ideaCandidates(result, Set(ChessIdeaFamily.Strategic))
        case JudgmentGraphSlot.CandidateComparisonFact =>
          evidenceCandidates(result, Set(EvidenceLayer.CandidateComparison))
        case JudgmentGraphSlot.RelativeAssessmentFact =>
          evidenceCandidates(result, Set(EvidenceLayer.RelativeAssessment, EvidenceLayer.CandidateComparison))
        case JudgmentGraphSlot.CounterfactualFact =>
          evidenceCandidates(result, Set(EvidenceLayer.Counterfactual, EvidenceLayer.CandidateComparison))
        case JudgmentGraphSlot.RelativeCauseFact | JudgmentGraphSlot.RelativeCauseProof =>
          comparisonCandidates(result, diagnostic =>
            diagnostic.hasUnexplainedEngineGap ||
              diagnostic.hasSecondaryContextEngineGap ||
              diagnostic.decisionTrace.requiresExplanatoryCause
          )
        case JudgmentGraphSlot.MoveVerdictCertificationFact =>
          evidenceCandidates(result, Set(EvidenceLayer.MoveVerdictCertification, EvidenceLayer.RelativeAssessment))
        case JudgmentGraphSlot.TacticalIdea =>
          evidenceCandidates(
            result,
            Set(EvidenceLayer.Relation, EvidenceLayer.MoveMotif, EvidenceLayer.Line, EvidenceLayer.RelativeCause)
          )
        case JudgmentGraphSlot.StrategicIdea =>
          evidenceCandidates(
            result,
            Set(EvidenceLayer.StrategicMechanism)
          )
        case JudgmentGraphSlot.PawnStructureIdea | JudgmentGraphSlot.PawnStructureClaim =>
          evidenceCandidates(result, Set(EvidenceLayer.StrategicMechanism)) ++
            ideaCandidates(result, Set(ChessIdeaFamily.PawnStructure))
        case JudgmentGraphSlot.ConversionIdea | JudgmentGraphSlot.ConversionClaim =>
          evidenceCandidates(result, Set(EvidenceLayer.RelativeCause, EvidenceLayer.Line, EvidenceLayer.Eval)) ++
            ideaCandidates(result, Set(ChessIdeaFamily.Conversion))
        case JudgmentGraphSlot.MaterialIdea | JudgmentGraphSlot.MaterialClaim =>
          evidenceCandidates(result, Set(EvidenceLayer.Line, EvidenceLayer.RelativeCause, EvidenceLayer.Eval)) ++
            ideaCandidates(result, Set(ChessIdeaFamily.Material))
        case JudgmentGraphSlot.EvaluationIdea | JudgmentGraphSlot.EvaluationClaim =>
          evidenceCandidates(result, Set(EvidenceLayer.RelativeAssessment, EvidenceLayer.CandidateComparison, EvidenceLayer.Eval)) ++
            ideaCandidates(result, Set(ChessIdeaFamily.Evaluation))
        case JudgmentGraphSlot.TacticalClaim =>
          ideaCandidates(result, Set(ChessIdeaFamily.Tactical))
        case JudgmentGraphSlot.StrategicClaim =>
          ideaCandidates(result, Set(ChessIdeaFamily.Strategic)) ++
            evidenceCandidates(result, Set(EvidenceLayer.Strategic, EvidenceLayer.StructuralDelta))
        case JudgmentGraphSlot.DefensiveClaim =>
          ideaCandidates(result, Set(ChessIdeaFamily.Defensive)) ++
            evidenceCandidates(result, Set(EvidenceLayer.ThreatPressure, EvidenceLayer.RelativeCause))
        case JudgmentGraphSlot.IdeaVerdictSplit =>
          evidenceCandidates(result, Set(EvidenceLayer.RelativeAssessment, EvidenceLayer.CandidateComparison)) ++
            ideaCandidates(result, ChessIdeaFamily.values.toSet)
        case JudgmentGraphSlot.ClaimSupportCluster =>
          claimCandidates(result, _.family.isLongTerm)
        case JudgmentGraphSlot.ClaimEventCluster =>
          claimCandidates(result, _.family.isEvent) ++
            evidenceCandidates(result, Set(EvidenceLayer.RelativeCause, EvidenceLayer.MoveVerdictCertification))
        case JudgmentGraphSlot.EvidenceLossDiagnostics =>
          result.quality.evidenceLoss.take(20).map(loss =>
            Json.obj(
              "kind" -> "EvidenceLoss",
              "id" -> loss.diagnostic.subjectId,
              "detail" -> lossDetail(result, loss),
              "expectation" -> loss.expectation.toString
            )
          )
        case _ =>
          Nil
    JsArray(candidates.take(20))

  private def evidenceCandidates(
      result: MoveReviewJudgmentResult,
      layers: Set[EvidenceLayer]
  ): List[JsObject] =
    result.packet.evidenceGraph.records
      .filter(record => layers.contains(record.ref.layer))
      .map(record =>
        Json.obj(
          "kind" -> "Evidence",
          "id" -> record.ref.id,
          "layer" -> record.ref.layer.toString,
          "scope" -> record.ref.scope.toString,
          "line" -> record.ref.line.map(lineRefSummary)
        )
      )

  private def ideaCandidates(
      result: MoveReviewJudgmentResult,
      families: Set[ChessIdeaFamily]
  ): List[JsObject] =
    result.packet.ideas
      .filter(idea => families.contains(idea.ref.family))
      .map(idea =>
        Json.obj(
          "kind" -> "Idea",
          "id" -> idea.ref.id,
          "family" -> idea.ref.family.toString,
          "subject" -> idea.subject.toString,
          "moveUci" -> idea.moveUci,
          "primaryLine" -> idea.primaryLine.map(lineRefSummary)
        )
      )

  private def claimCandidates(
      result: MoveReviewJudgmentResult,
      keep: ClaimSeed => Boolean
  ): List[JsObject] =
    result.packet.claims
      .filter(keep)
      .map(claim =>
        Json.obj(
          "kind" -> "Claim",
          "id" -> claim.id,
          "family" -> claim.family.toString,
          "subject" -> claim.subject.toString,
          "subjectMove" -> claim.subjectMove,
          "primaryLine" -> claim.primaryLine.map(lineRefSummary),
          "supportStatus" -> claim.supportStatus.map(_.status.toString)
        )
      )

  private def comparisonCandidates(
      result: MoveReviewJudgmentResult,
      keep: CandidateComparisonDiagnostic => Boolean
  ): List[JsObject] =
    result.quality.semanticCoverage.comparisonDiagnostics
      .filter(keep)
      .map(diagnostic =>
        Json.obj(
          "kind" -> "Comparison",
          "id" -> diagnostic.id,
          "comparisonKind" -> diagnostic.comparisonKind.toString,
          "referenceMove" -> diagnostic.referenceLine.rootMove,
          "candidateMove" -> diagnostic.candidateLine.rootMove,
          "failureClass" -> failureClassId(diagnostic.failureClass),
          "tacticalBlockedAt" -> diagnostic.tacticalLossTrace.blockedAt.map(tacticalLossStageId),
          "tacticalLastPresentStage" -> diagnostic.tacticalLossTrace.lastPresentStage.map(tacticalLossStageId),
          "advisory" -> Json.obj(
            "causeHints" -> diagnostic.advisoryCauseHints.map(_.toString)
          )
        )
      )

  private def rawInputAudit(raw: RawMoveReviewInput): JsObject =
    val playedMove = MoveReviewInputNormalizer.normalizeUci(raw.playedMoveUci)
    val rootMoves = raw.variations.flatMap(_.moves.headOption).map(MoveReviewInputNormalizer.normalizeUci)
    val legalRootMoves =
      raw.variations
        .filter(line => line.moves.headOption.exists(move => legalFirstMove(raw.fen, move)))
        .flatMap(_.moves.headOption)
        .map(MoveReviewInputNormalizer.normalizeUci)
    val illegalRootMoves =
      raw.variations
        .filter(line => line.moves.headOption.exists(move => !legalFirstMove(raw.fen, move)))
        .flatMap(_.moves.headOption)
        .map(MoveReviewInputNormalizer.normalizeUci)
    val fullyLegalLines = raw.variations.filter(line => legalLine(raw.fen, line.moves))
    val fullyLegalRootMoves =
      fullyLegalLines.flatMap(_.moves.headOption).map(MoveReviewInputNormalizer.normalizeUci)
    val playedRootLineCount = rootMoves.count(_ == playedMove)
    val legalPlayedLineCount = legalRootMoves.count(_ == playedMove)
    val fullyLegalPlayedLineCount = fullyLegalRootMoves.count(_ == playedMove)
    val fenPly = plyFromFenForDiagnostics(raw.fen)
    val beforePly = raw.ply.orElse(fenPly)
    val rawMovePrefixPlyMismatch =
      beforePly.exists(ply => raw.movePrefixUci.nonEmpty && raw.movePrefixUci.size != ply)
    val openingMetadataProvided =
      raw.openingContext.exists(context =>
        context.eco.exists(_.trim.nonEmpty) || context.name.exists(_.trim.nonEmpty) || context.family.exists(_.trim.nonEmpty)
      )
    val openingFreshness =
      openingMetadataFreshness(raw, beforePly, rawMovePrefixPlyMismatch)
    Json.obj(
      "diagnosticScope" -> "raw_input",
      "inputVariationCount" -> raw.variations.size,
      "variationRootMoves" -> rootMoves,
      "legalRootLineCount" -> legalRootMoves.size,
      "illegalRootLineCount" -> illegalRootMoves.size,
      "legalRootMoves" -> legalRootMoves.distinct,
      "illegalRootMoves" -> illegalRootMoves.distinct,
      "fullyLegalLineCount" -> fullyLegalLines.size,
      "fullyLegalRootMoves" -> fullyLegalRootMoves.distinct,
      "playedMoveInVariations" -> rootMoves.contains(playedMove),
      "playedRootLineCount" -> playedRootLineCount,
      "legalPlayedRootLineCount" -> legalPlayedLineCount,
      "fullyLegalPlayedLineCount" -> fullyLegalPlayedLineCount,
      "illegalPlayedRootLineCount" -> (playedRootLineCount - legalPlayedLineCount).max(0),
      "playedLineHasLegalRootOnly" -> (legalPlayedLineCount > 0 && fullyLegalPlayedLineCount == 0),
      "playedLineInputFullyLegal" -> (fullyLegalPlayedLineCount > 0),
      "rawPlayedLineEvalUnavailable" -> (!rootMoves.contains(playedMove) || fullyLegalPlayedLineCount == 0),
      "currentEvalSource" -> raw.currentEvalCp.map(_ => "inputCurrentEvalCp").orElse(
        Option.when(rootMoves.nonEmpty)("normalizedReferenceScoreCp")
      ).getOrElse("missing"),
      "fenPly" -> fenPly,
      "inputPly" -> raw.ply,
      "movePrefixLength" -> raw.movePrefixUci.size,
      "rawMovePrefixPlyMismatch" -> rawMovePrefixPlyMismatch,
      "openingMetadataProvided" -> openingMetadataProvided,
      "openingMetadataFreshness" -> openingFreshness.json,
      "rawOpeningMetadataStaleRisk" -> openingFreshness.staleRisk
    )

  private final case class OpeningMetadataFreshnessDiagnostic(json: JsObject, staleRisk: Boolean)

  private def openingMetadataFreshness(
      raw: RawMoveReviewInput,
      beforePly: Option[Int],
      rawMovePrefixPlyMismatch: Boolean
  ): OpeningMetadataFreshnessDiagnostic =
    val metadata = normalizedOpeningMetadata(raw.openingContext)
    val movePrefix = raw.movePrefixUci.map(MoveReviewInputNormalizer.normalizeUci).filter(_.nonEmpty)
    val recognition = OpeningRecognitionIndex.default.recognize(movePrefix, raw.fen.trim, beforePly.getOrElse(0))
    val recognizedIdentity = recognition.flatMap(_.bestIdentity)
    val fieldMatches = metadata.zip(recognizedIdentity).toList.flatMap { case (input, recognized) =>
      List(
        Option.when(input.eco.nonEmpty)("eco" -> input.eco.exists(eco => recognized.eco.exists(_.equalsIgnoreCase(eco)))),
        Option.when(input.name.nonEmpty)("name" -> input.name.exists(name => recognized.name.exists(sameOpeningName(_, name)))),
        Option.when(input.family.nonEmpty)("family" -> input.family.exists(family => recognized.family.contains(family)))
      ).flatten
    }
    val matchedFields = fieldMatches.collect { case (field, true) => field }
    val conflictedFields = fieldMatches.collect { case (field, false) => field }
    val agreement =
      if metadata.isEmpty then "absent"
      else if recognition.isEmpty then "unverified"
      else if conflictedFields.nonEmpty then "conflicted"
      else if matchedFields.nonEmpty then "matched"
      else "unverified"
    val freshnessStatus =
      if metadata.isEmpty then "absent"
      else if conflictedFields.nonEmpty then "stale_suspect"
      else if rawMovePrefixPlyMismatch && matchedFields.isEmpty then "unverified_prefix_mismatch"
      else if matchedFields.nonEmpty && recognition.exists(_.matchedBy == OpeningRecognitionMatchKind.ExactPrefixAndPosition) then "fresh_exact"
      else if matchedFields.nonEmpty then "fresh_position_match"
      else "unverified"
    val freshnessConfidence =
      if metadata.isEmpty then "none"
      else if conflictedFields.nonEmpty then "low"
      else recognition.map(_.confidence).filter(_ >= 0.75).map(_ => "high")
        .orElse(recognition.map(_.confidence).filter(_ >= 0.5).map(_ => "medium"))
        .getOrElse("low")
    val staleRisk = freshnessStatus == "stale_suspect" || freshnessStatus == "unverified_prefix_mismatch"
    OpeningMetadataFreshnessDiagnostic(
      json = Json.obj(
        "metadataProvided" -> metadata.nonEmpty,
        "recognitionPresent" -> recognition.nonEmpty,
        "recognitionMatchKind" -> recognition.map(_.matchedBy.toString),
        "recognitionConfidence" -> recognition.map(_.confidence),
        "recognitionMatchedPly" -> recognition.map(_.matchedPly),
        "metadataRecognitionAgreement" -> agreement,
        "matchedFields" -> matchedFields,
        "conflictedFields" -> conflictedFields,
        "freshnessStatus" -> freshnessStatus,
        "freshnessConfidence" -> freshnessConfidence,
        "staleRisk" -> staleRisk
      ),
      staleRisk = staleRisk
    )

  private def normalizedOpeningMetadata(raw: Option[RawOpeningContext]): Option[OpeningIdentity] =
    raw.flatMap { context =>
      val eco = cleanText(context.eco).map(_.toUpperCase)
      val name = cleanText(context.name)
      val family =
        cleanText(context.family).flatMap(OpeningFamily.fromRaw)
          .orElse(eco.flatMap(OpeningFamily.fromEco))
          .orElse(name.flatMap(OpeningThemePriorIndex.familyHintForName))
      Option.when(eco.nonEmpty || name.nonEmpty || family.nonEmpty)(
        OpeningIdentity(eco = eco, name = name, family = family)
      )
    }

  private def cleanText(raw: Option[String]): Option[String] =
    raw.map(_.trim).filter(_.nonEmpty)

  private def sameOpeningName(left: String, right: String): Boolean =
    val leftKey = normalizeOpeningName(left)
    val rightKey = normalizeOpeningName(right)
    leftKey == rightKey || leftKey.startsWith(s"$rightKey ") || rightKey.startsWith(s"$leftKey ")

  private def normalizeOpeningName(raw: String): String =
    Option(raw).getOrElse("")
      .toLowerCase(java.util.Locale.ROOT)
      .replace('\u00e9', 'e')
      .replace('\u00e8', 'e')
      .replace('\u00fc', 'u')
      .replaceAll("[^a-z0-9]+", " ")
      .replaceAll("\\s+", " ")
      .trim

  private def legalFirstMove(fen: String, move: String): Boolean =
    PrincipalVariationEvidence.legalFenAfter(fen.trim, MoveReviewInputNormalizer.normalizeUci(move)).nonEmpty

  private def legalLine(fen: String, moves: List[String]): Boolean =
    var currentFen = fen.trim
    var legal = moves.nonEmpty
    val iterator = moves.iterator
    while iterator.hasNext && legal do
      PrincipalVariationEvidence.legalFenAfter(currentFen, MoveReviewInputNormalizer.normalizeUci(iterator.next())) match
        case Some(nextFen) => currentFen = nextFen
        case None          => legal = false
    legal

  private def plyFromFenForDiagnostics(fen: String): Option[Int] =
    val parts = fen.trim.split("\\s+").toList
    for
      side <- parts.lift(1)
      fullmoveText <- parts.lift(5)
      fullmove <- fullmoveText.toIntOption
    yield ((fullmove max 1) - 1) * 2 + (if side == "b" then 1 else 0)

  private def evidenceLossSummary(result: MoveReviewJudgmentResult): JsObject =
    val unexpected =
      result.quality.evidenceLoss.filter(_.expectation == EvidenceLossExpectation.Unexpected)
    val secondary =
      result.quality.evidenceLoss.filter(_.expectation == EvidenceLossExpectation.Secondary)
    val deferred =
      result.quality.evidenceLoss.filter(_.expectation == EvidenceLossExpectation.Deferred)
    val claimLifecycleLoss =
      result.quality.evidenceLoss.filter(loss => claimLifecycleLossExpectation(loss.expectation))
    val expected =
      result.quality.evidenceLoss.filter(_.expectation == EvidenceLossExpectation.Expected)
    Json.obj(
      "expectedCount" -> expected.size,
      "secondaryCount" -> secondary.size,
      "deferredCount" -> deferred.size,
      "claimLifecycleLossCount" -> claimLifecycleLoss.size,
      "unexpectedCount" -> unexpected.size,
      "expectedByLayer" -> countsBy(
        expected.flatMap(_.diagnostic.layer.map(_.toString))
      ),
      "expectedByReason" -> countsBy(
        expected.map(_.diagnostic.reason.toString)
      ),
      "deferredByLayer" -> countsBy(
        deferred.flatMap(_.diagnostic.layer.map(_.toString))
      ),
      "deferredByReason" -> countsBy(
        deferred.map(_.diagnostic.reason.toString)
      ),
      "claimLifecycleLossByDetail" -> countsBy(
        claimLifecycleLoss.map(lossDetail(result, _))
      ),
      "secondaryByLayer" -> countsBy(
        secondary.flatMap(_.diagnostic.layer.map(_.toString))
      ),
      "secondaryByReason" -> countsBy(
        secondary.map(_.diagnostic.reason.toString)
      ),
      "unexpectedByLayer" -> countsBy(
        unexpected.flatMap(_.diagnostic.layer.map(_.toString))
      ),
      "unexpectedByReason" -> countsBy(
        unexpected.map(_.diagnostic.reason.toString)
      ),
      "expectedByDetail" -> countsBy(
        expected.map(lossDetail(result, _))
      ),
      "deferredByDetail" -> countsBy(
        deferred.map(lossDetail(result, _))
      ),
      "secondaryByDetail" -> countsBy(
        secondary.map(lossDetail(result, _))
      ),
      "unexpectedByDetail" -> countsBy(
        unexpected.map(lossDetail(result, _))
      ),
      "unexpectedDiagnostics" -> JsArray(
        unexpected.take(20).map(loss => evidenceLossDiagnosticJson(result, loss))
      ),
      "deferredDiagnostics" -> JsArray(
        deferred.take(20).map(loss => evidenceLossDiagnosticJson(result, loss))
      ),
      "claimLifecycleLossDiagnostics" -> JsArray(
        claimLifecycleLoss.take(20).map(loss => evidenceLossDiagnosticJson(result, loss))
      ),
      "secondaryDiagnostics" -> JsArray(
        secondary.take(20).map(loss => evidenceLossDiagnosticJson(result, loss))
      )
    )

  private def claimLifecycleLossExpectation(expectation: EvidenceLossExpectation): Boolean =
    expectation match
      case EvidenceLossExpectation.TruthRejected | EvidenceLossExpectation.TruthDeferred |
          EvidenceLossExpectation.DedupeDropped | EvidenceLossExpectation.ArbitrationSuppressed =>
        true
      case _ =>
        false

  private def evidenceLossDiagnosticJson(
      result: MoveReviewJudgmentResult,
      loss: EvidenceLossClassification
  ): JsObject =
    val diagnostic = loss.diagnostic
    Json.obj(
      "expectation" -> loss.expectation.toString,
      "detail" -> lossDetail(result, loss),
      "stage" -> diagnostic.stage.toString,
      "reason" -> diagnostic.reason.toString,
      "subjectId" -> diagnostic.subjectId,
      "relatedComparisonIds" -> relatedComparisonIds(result, diagnostic.subjectId),
      "layer" -> diagnostic.layer.map(_.toString),
      "evidence" -> diagnostic.evidence.map(evidenceRefSummary),
      "evidencePayload" -> evidencePayloadSummary(result, diagnostic.subjectId),
      "idea" -> ideaSummary(result, diagnostic.subjectId),
      "claim" -> claimSummary(result, diagnostic.subjectId)
    )

  private def lossDetail(
      result: MoveReviewJudgmentResult,
      loss: EvidenceLossClassification
  ): String =
    val diagnostic = loss.diagnostic
    diagnostic.reason match
      case EvidenceLossReason.ReferenceNotRegistered =>
        "producer_unregistered_reference"
      case EvidenceLossReason.ClaimEvidenceMissing =>
        "claim_evidence_missing"
      case EvidenceLossReason.PacketMissingRoot =>
        "packet_root_missing"
      case EvidenceLossReason.EvidenceAvailableWithoutIdea =>
        loss.expectation match
          case EvidenceLossExpectation.Expected =>
            expectedEvidenceLossDetail(result, diagnostic)
          case EvidenceLossExpectation.Secondary =>
            "secondary_context"
          case EvidenceLossExpectation.Deferred =>
            "deferred_context"
          case EvidenceLossExpectation.TruthRejected =>
            "claim_truth_rejected"
          case EvidenceLossExpectation.TruthDeferred =>
            "claim_truth_deferred"
          case EvidenceLossExpectation.DedupeDropped =>
            "claim_dedupe_dropped"
          case EvidenceLossExpectation.ArbitrationSuppressed =>
            "claim_arbitration_suppressed"
          case EvidenceLossExpectation.Unexpected if relatedComparisons(result, diagnostic.subjectId).exists(thresholdMiss) =>
            "threshold_miss"
          case EvidenceLossExpectation.Unexpected if diagnostic.layer.exists(StrategicMechanismEvidence.rawStrategicSourceLayer) =>
            "unbound_raw_strategic_source"
          case EvidenceLossExpectation.Unexpected if diagnostic.layer.exists(concretePolicyLayer) =>
            "policy_gated"
          case EvidenceLossExpectation.Unexpected =>
            "producer_unbound"
      case EvidenceLossReason.IdeaAvailableWithoutClaim =>
        loss.expectation match
          case EvidenceLossExpectation.TruthRejected =>
            "claim_truth_rejected"
          case EvidenceLossExpectation.TruthDeferred =>
            "claim_truth_deferred"
          case EvidenceLossExpectation.DedupeDropped =>
            "claim_dedupe_dropped"
          case EvidenceLossExpectation.ArbitrationSuppressed =>
            "claim_arbitration_suppressed"
          case EvidenceLossExpectation.Deferred =>
            "claim_deferred"
          case EvidenceLossExpectation.Secondary =>
            "secondary_context"
          case EvidenceLossExpectation.Expected =>
            expectedEvidenceLossDetail(result, diagnostic)
          case EvidenceLossExpectation.Unexpected =>
            "policy_gated"

  private def expectedEvidenceLossDetail(
      result: MoveReviewJudgmentResult,
      diagnostic: EvidenceLossDiagnostic
  ): String =
    diagnostic.evidence
      .flatMap(ref => result.packet.evidenceGraph.byId.get(ref.id))
      .map {
        case record if StrategicMechanismEvidence.rawStrategicSourceLayer(record.ref.layer) =>
          if strategicMechanismConsumers(result.packet, record.ref).nonEmpty then "consumed_source"
          else "unbound_raw_strategic_source"
        case EvidenceRecord(_, payload: StrategicMechanismEvidence, _) =>
          if strategicMechanismCanSeedJudgment(payload) then "claim_grade_mechanism_unpromoted"
          else "source_only_mechanism"
        case _ =>
          "support_only_source"
      }
      .getOrElse("support_only_source")

  private def strategicMechanismConsumers(
      packet: EvidenceBackedJudgmentPacket,
      source: EvidenceRef
  ): List[EvidenceRef] =
    packet.evidenceGraph.records.collect {
      case EvidenceRecord(ref, _: StrategicMechanismEvidence, parents)
          if parents.exists(_.id == source.id) ||
            parents.exists(parent => evidenceHasAncestor(packet.evidenceGraph, parent, source.id)) =>
        ref
    }

  private def evidenceHasAncestor(
      graph: TypedEvidenceGraph,
      ref: EvidenceRef,
      ancestorId: String
  ): Boolean =
    def loop(next: List[EvidenceRef], seen: Set[String]): Boolean =
      next.exists { current =>
        current.id == ancestorId ||
          (!seen.contains(current.id) &&
            graph.byId.get(current.id).exists(record => loop(record.parents, seen + current.id)))
      }
    loop(List(ref), Set.empty)

  private def strategicMechanismCanSeedJudgment(payload: StrategicMechanismEvidence): Boolean =
    payload.canAnchorStrategicIdea ||
      payload.canAnchorPawnStructureIdea ||
      payload.canAnchorOpeningIdea ||
      payload.canAnchorPlanIdea ||
      payload.canSupportCompensation

  private def concretePolicyLayer(layer: EvidenceLayer): Boolean =
    layer match
      case EvidenceLayer.CandidateComparison | EvidenceLayer.RelativeCause | EvidenceLayer.ChessIdea |
          EvidenceLayer.TacticalMechanism | EvidenceLayer.Relation | EvidenceLayer.ThreatPressure | EvidenceLayer.Line |
          EvidenceLayer.StructuralDelta | EvidenceLayer.PawnStructure =>
        true
      case _ =>
        false

  private def thresholdMiss(diagnostic: CandidateComparisonDiagnostic): Boolean =
    diagnostic.failureReasons.exists(reason =>
      reason == CandidateComparisonFailureReason.TacticalEvidenceBelowThreshold ||
        reason == CandidateComparisonFailureReason.MaterialEvidenceBelowThreshold ||
        reason == CandidateComparisonFailureReason.StrategicEvidenceBelowThreshold ||
        reason == CandidateComparisonFailureReason.StrategicEvidenceBelowCauseThreshold ||
        reason == CandidateComparisonFailureReason.PrimaryStrategicNearThresholdUnderbinding ||
        reason == CandidateComparisonFailureReason.ContextAlternativeStrategicNearThreshold ||
        reason == CandidateComparisonFailureReason.LowSignalEnginePreference ||
        reason == CandidateComparisonFailureReason.LowSignalTacticalContext ||
        reason == CandidateComparisonFailureReason.LowSignalMaterialContext ||
        reason == CandidateComparisonFailureReason.LowSignalStrategicContext
    )

  private def relatedComparisonIds(result: MoveReviewJudgmentResult, subjectId: String): List[String] =
    relatedComparisons(result, subjectId).map(_.id).distinct.sorted

  private def relatedComparisons(
      result: MoveReviewJudgmentResult,
      subjectId: String
  ): List[CandidateComparisonDiagnostic] =
    val diagnostics = result.quality.semanticCoverage.comparisonDiagnostics
    if subjectId == "candidate-comparison" then diagnostics
    else if subjectId == "relative-cause" then
      diagnostics.filter(diagnostic =>
        diagnostic.hasUnexplainedEngineGap ||
          diagnostic.hasSecondaryContextEngineGap ||
          (diagnostic.causeKinds.isEmpty && diagnostic.failureReasons.nonEmpty)
      )
    else
      diagnostics.filter(diagnostic =>
        diagnostic.id == subjectId ||
          diagnostic.causeSupport.exists(support =>
            support.id == subjectId ||
              support.parentEvidenceIds.contains(subjectId)
          )
      )

  private def branchReplyProbeLifecycle(result: MoveReviewJudgmentResult): JsObject =
    val requests = result.packet.probeRequests.filter(request => request.purpose.exists(ProbePurpose.isBranchReply))
    val diagnostics = result.packet.probeDiagnostics
    val diagnosedIds = diagnostics.map(_.probeId).toSet
    val pending = requests.filterNot(request => diagnosedIds.contains(request.id))
    Json.obj(
      "state" -> result.quality.semanticCoverage.branchReplyProbeLifecycleState,
      "requestCount" -> requests.size,
      "diagnosticCount" -> diagnostics.size,
      "pendingRequestCount" -> pending.size,
      "pendingRequestIds" -> pending.map(_.id).take(20),
      "pendingMoves" -> pending.flatMap(_.candidateMove).distinct.sorted,
      "admittedProbeIds" -> diagnostics.filter(_.status == lila.chessjudgment.model.ProbeAdmissionStatus.Admitted).map(_.probeId),
      "rejectedProbeIds" -> diagnostics.filter(_.status == lila.chessjudgment.model.ProbeAdmissionStatus.Rejected).map(_.probeId),
      "ignoredProbeIds" -> diagnostics.filter(_.status == lila.chessjudgment.model.ProbeAdmissionStatus.Ignored).map(_.probeId)
    )

  private def evidenceRefSummary(ref: EvidenceRef): JsObject =
    Json.obj(
      "id" -> ref.id,
      "producer" -> ref.producer.toString,
      "layer" -> ref.layer.toString,
      "scope" -> ref.scope.toString,
      "confidence" -> ref.confidence.toString,
      "line" -> ref.line.map(lineRefSummary)
    )

  private def lineRefSummary(ref: LineNodeRef): JsObject =
    Json.obj(
      "id" -> ref.id,
      "rootMove" -> ref.rootMove,
      "rank" -> ref.rank,
      "role" -> ref.role.toString
    )

  private def interactionBasisJson(basis: ClaimInteractionBasis): JsObject =
    Json.obj(
      "causeKind" -> basis.causeKind.toString,
      "comparisonKind" -> basis.comparisonKind.toString,
      "causeRole" -> basis.causeRole.toString,
      "causeSourceSide" -> basis.causeSourceSide.toString,
      "causeImportance" -> basis.causeImportance.toString,
      "attributionKind" -> basis.attributionKind.toString,
      "attributionRootMoveMatched" -> basis.attributionRootMoveMatched,
      "attributionDirectProofEligible" -> basis.attributionDirectProofEligible,
      "referenceLine" -> lineRefSummary(basis.referenceLine),
      "candidateLine" -> lineRefSummary(basis.candidateLine),
      "eventLine" -> lineRefSummary(basis.eventLine),
      "proofDirectSourceIds" -> basis.proofDirectSourceIds,
      "proofContrastSourceIds" -> basis.proofContrastSourceIds,
      "proofContextSupportSourceIds" -> basis.proofContextSupportSourceIds,
      "proofStrategicAxisKeys" -> basis.proofStrategicAxisKeys,
      "proofStrategicMechanismKinds" -> basis.proofStrategicMechanismKinds.map(_.toString),
      "proofStrategicMechanismSourceIds" -> basis.proofStrategicMechanismSourceIds,
      "proofStrategicMechanismSignalSourceIds" -> basis.proofStrategicMechanismSignalSourceIds,
      "supportEvidenceSourceIds" -> basis.supportEvidenceSourceIds
    )

  private def transitionConsequenceJson(proof: TransitionConsequenceProof): JsObject =
    val consequence = proof.consequence
    Json.obj(
      "kind" -> consequence.kind.toString,
      "polarity" -> consequence.polarity.toString,
      "strength" -> consequence.strength,
      "sourceEvidenceId" -> proof.source.id,
      "moveUci" -> proof.transition.moveUci,
      "role" -> proof.transition.role.toString,
      "fromPositionId" -> proof.transition.from.id,
      "toPositionId" -> proof.transition.to.id,
      "line" -> proof.transition.line.map(lineRefSummary),
      "perspective" -> proof.transition.perspective.toString
    )

  private def tacticalMechanismProofJson(proof: TacticalMechanismProof): JsObject =
    Json.obj(
      "sourceEvidenceId" -> proof.source.id,
      "kind" -> proof.kind.toString,
      "signals" -> proof.signals.map(signal =>
        Json.obj(
          "kind" -> signal.kind.toString,
          "label" -> signal.label,
          "sourceLayer" -> signal.sourceLayer.toString
        )
      )
    )

  private def strategicMechanismProofJson(proof: StrategicMechanismProof): JsObject =
    Json.obj(
      "sourceEvidenceId" -> proof.source.id,
      "kind" -> proof.kind.toString,
      "signals" -> proof.signals.map(signal =>
        Json.obj(
          "kind" -> signal.kind.toString,
          "label" -> signal.label,
          "sourceLayer" -> signal.sourceLayer.toString,
          "sourceEvidenceId" -> signal.source.id,
          "sourcePositionId" -> signal.source.position.id,
          "sourceLine" -> signal.source.line.map(lineRefSummary),
          "strength" -> signal.strength,
          "axis" -> signal.axis.map(strategicAxisDetailJson)
        )
      )
    )

  private def strategicAxisDetailJson(axis: StrategicAxisDetail): JsObject =
    Json.obj(
      "kind" -> axis.kind.toString,
      "polarity" -> axis.polarity.toString,
      "label" -> axis.label,
      "stableKey" -> axis.stableKey
    )

  private def strategicMechanismContrastProofJson(proof: StrategicMechanismContrastProof): JsObject =
    Json.obj(
      "sourceEvidenceId" -> proof.source.id,
      "comparisonKind" -> proof.comparisonKind.toString,
      "referenceLine" -> lineRefSummary(proof.referenceLine),
      "candidateLine" -> lineRefSummary(proof.candidateLine),
      "axisComparisons" -> proof.axisComparisons.map(axis =>
        Json.obj(
          "axis" -> strategicAxisDetailJson(axis.axis),
          "outcome" -> axis.outcome.toString,
          "referenceLead" -> axis.referenceLead,
          "candidateLead" -> axis.candidateLead,
          "candidateNegative" -> axis.candidateNegative,
          "hasContrast" -> axis.hasContrast
        )
      ),
      "sustainability" -> Json.obj(
        "horizon" -> proof.sustainability.horizon.toString,
        "lineMaintained" -> proof.sustainability.lineMaintained,
        "pvMaintained" -> proof.sustainability.pvMaintained,
        "referencePlyCount" -> proof.sustainability.referencePlyCount,
        "candidatePlyCount" -> proof.sustainability.candidatePlyCount,
        "hasSustainedPv" -> proof.sustainability.hasSustainedPv
      )
    )

  private def boardAnchorProofJson(proof: BoardAnchorProof): JsObject =
    Json.obj(
      "sourceEvidenceId" -> proof.source.id,
      "kind" -> proof.kind.toString
    )

  private def lineEventProofJson(proof: LineEventProof): JsObject =
    Json.obj(
      "sourceEvidenceId" -> proof.source.id,
      "kind" -> proof.kind.toString,
      "moveUci" -> proof.moveUci,
      "plyOffset" -> proof.plyOffset,
      "side" -> proof.side.map(_.name),
      "square" -> proof.square.map(_.key)
    )

  private def lineConsequenceProofJson(proof: LineConsequenceProof): JsObject =
    Json.obj(
      "sourceEvidenceId" -> proof.source.id,
      "kind" -> proof.kind.toString,
      "eventMove" -> proof.eventMove,
      "lineMoves" -> proof.lineMoves
    )

  private def relativeCauseProofSectionJson(section: RelativeCauseProofSection): JsObject =
    Json.obj(
      "role" -> section.role.toString,
      "strength" -> section.strength.toString,
      "sourceEvidenceIds" -> section.sourceRefs.map(_.id).distinct.sorted,
      "boardAnchors" -> section.boardAnchors.map(boardAnchorProofJson),
      "lineEvents" -> section.lineEvents.map(lineEventProofJson),
      "lineConsequences" -> section.lineConsequences.map(lineConsequenceProofJson),
      "relationProofs" -> section.relationProofs.map(relationCauseProofJson),
      "tacticalMechanisms" -> section.tacticalMechanisms.map(tacticalMechanismProofJson),
      "strategicMechanisms" -> section.strategicMechanisms.map(strategicMechanismProofJson),
      "strategicMechanismContrasts" -> section.strategicMechanismContrasts.map(strategicMechanismContrastProofJson),
      "transitionConsequences" -> section.transitionConsequences.map(transitionConsequenceJson),
      "contextLayers" -> section.contextLayers.map(_.toString)
    )

  private def relationCauseProofJson(proof: RelationCauseProof): JsObject =
    Json.obj(
      "sourceEvidenceId" -> proof.source.id,
      "kind" -> proof.kind.toString,
      "detail" -> proof.detailName,
      "hasLineProof" -> proof.hasLineProof,
      "focusSquares" -> proof.proof.focusSquares.map(_.key),
      "targetSquare" -> proof.proof.targetSquare.map(_.key),
      "lineMoves" -> proof.proof.lineMoves,
      "participants" -> proof.proof.participants.map(participant =>
        Json.obj(
          "square" -> participant.square.key,
          "role" -> participant.role.map(_.name),
          "participantRole" -> participant.participantRole.toString
        )
      ),
      "proofAtoms" -> proof.proof.proofAtoms.map(atom =>
        Json.obj(
          "role" -> atom.role.toString,
          "square" -> atom.square.map(_.key),
          "moveUci" -> atom.moveUci,
          "participantRole" -> atom.participantRole.map(_.toString),
          "pieceRole" -> atom.pieceRole.map(_.name),
          "label" -> atom.label
        )
      )
    )

  private def evidencePayloadSummary(result: MoveReviewJudgmentResult, subjectId: String): Option[JsObject] =
    result.packet.evidenceGraph.byId.get(subjectId).map {
      case EvidenceRecord(_, CandidateComparisonEvidence(fact), _) =>
        Json.obj(
          "payload" -> "CandidateComparison",
          "kind" -> fact.kind.toString,
          "referenceLine" -> lineRefSummary(fact.referenceLine),
          "candidateLine" -> lineRefSummary(fact.candidateLine),
          "verdict" -> fact.comparison.verdict.toString,
          "winPercentLossForMover" -> fact.comparison.winPercentLossForMover,
          "candidateWinPercentDeltaForMover" -> fact.comparison.candidateWinPercentDeltaForMover
        )
      case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) =>
        Json.obj(
          "payload" -> "RelativeCause",
          "kind" -> cause.kind.toString,
          "role" -> cause.role.toString,
          "sourceSide" -> cause.sourceSide.toString,
          "importance" -> cause.importance.toString,
          "comparisonKind" -> cause.comparisonKind.toString,
          "referenceLine" -> lineRefSummary(cause.referenceLine),
          "candidateLine" -> lineRefSummary(cause.candidateLine),
          "eventLine" -> lineRefSummary(cause.eventLine),
          "evidenceLines" -> cause.evidenceLines.map(lineRefSummary),
          "supportEvidenceIds" -> cause.supportEvidence.map(_.id).distinct.sorted,
          "verdict" -> cause.verdict.toString,
          "winPercentLossForMover" -> cause.winPercentLossForMover,
          "hasOwnedTypedDepth" -> cause.hasOwnedTypedDepth,
          "hasOwnedTacticalProof" -> cause.hasOwnedTacticalProof,
          "hasOwnedStrategicContrastDepth" -> cause.hasOwnedStrategicContrastDepth,
          "hasOwnedAdmissibleLongTermProof" -> cause.hasOwnedAdmissibleLongTermProof,
          "proof" -> cause.proof.map(proof =>
            val depthProof = proof.depthProof
            Json.obj(
              "rawHasTypedDepth" -> proof.hasRawTypedDepth,
              "boardAnchors" -> depthProof.boardAnchors.map(_.toString),
              "lineEvents" -> depthProof.lineEvents.map(_.toString),
              "lineConsequences" -> depthProof.lineConsequences.map(_.toString),
              "relationKinds" -> depthProof.relationKinds.map(_.toString),
              "relationProofs" -> depthProof.relationProofs.map(relationCauseProofJson),
              "tacticalMechanisms" -> depthProof.tacticalMechanisms.map(tacticalMechanismProofJson),
              "strategicMechanisms" -> depthProof.strategicMechanisms.map(strategicMechanismProofJson),
              "transitionConsequences" -> depthProof.transitionConsequences.map(transitionConsequenceJson),
              "contextLayers" -> proof.contextLayers.map(_.toString),
              "directProof" -> relativeCauseProofSectionJson(proof.directProof),
              "contrastProof" -> relativeCauseProofSectionJson(proof.contrastProof),
              "contextSupport" -> relativeCauseProofSectionJson(proof.contextSupport)
            )
          )
        )
      case EvidenceRecord(_, payload: LineFactEvidence, _) =>
        Json.obj(
          "payload" -> "LineFact",
          "line" -> lineRefSummary(payload.line),
          "firstMove" -> payload.rootMove,
          "replyMove" -> payload.reply,
          "continuationMoves" -> payload.continuation,
          "forcedTheme" -> payload.forcedThemeId,
          "material" -> Json.obj(
            "netCaptureCpForMover" -> payload.materialNetCaptureCpForMover,
            "maxGainCpForMover" -> payload.materialMaxGainCpForMover,
            "promotionGainCpForMover" -> payload.materialPromotionGainCpForMover,
            "hasProofSignalMaterialEvent" -> payload.hasProofSignalMaterialEvent,
            "hasSacrificeMaterialEvent" -> payload.hasSacrificeMaterialEvent,
            "hasRecaptureChain" -> payload.hasMaterialRecaptureChain,
            "hasRecoveryWindow" -> payload.hasMaterialRecoveryWindow,
            "materialWindowComplete" -> payload.hasCompleteMaterialWindow
          ),
          "replay" -> payload.lineReplaySteps.map(step =>
            Json.obj(
              "ply" -> step.ply,
              "moveUci" -> step.moveUci,
              "fenBefore" -> step.fenBefore,
              "fenAfter" -> step.fenAfter
            )
          ),
          "events" -> payload.lineEvents.map(event =>
            Json.obj(
              "kind" -> event.kind.toString,
              "moveUci" -> event.moveUci,
              "plyOffset" -> event.plyOffset,
              "side" -> event.side.map(_.name),
              "pieceRole" -> event.pieceRole.map(_.name),
              "targetRole" -> event.targetRole.map(_.name),
              "square" -> event.square.map(_.key)
            )
          ),
          "consequences" -> payload.lineConsequences.map(consequence =>
            Json.obj(
              "kind" -> consequence.kind.toString,
              "lineMoves" -> consequence.lineMoves,
              "proofSignal" -> consequence.proofSignal,
              "eventMove" -> consequence.eventMove
            )
          )
        )
      case EvidenceRecord(_, payload: BoardFactEvidence, _) =>
        Json.obj(
          "payload" -> "BoardFact",
          "factCount" -> payload.factCount,
          "anchors" -> payload.boardAnchors.map(anchor =>
            Json.obj(
              "kind" -> anchor.kind.toString,
              "side" -> anchor.side.name,
              "signal" -> anchor.signal.toString,
              "magnitude" -> anchor.magnitude,
              "confidence" -> anchor.confidence
            )
          )
        )
      case EvidenceRecord(_, payload: RelationFactEvidence, _) =>
        Json.obj(
          "payload" -> "Relation",
          "kind" -> payload.kind.toString,
          "sourceKind" -> payload.witnessProof.sourceKind,
          "detail" -> payload.witnessProof.detailName,
          "hasTypedWitness" -> payload.hasTypedWitness,
          "focusSquares" -> payload.focusSquares.map(_.key),
          "targetSquare" -> payload.targetSquare.map(_.key),
          "lineMoves" -> payload.lineMoves,
          "participants" -> payload.participants.map(participant =>
            Json.obj(
              "square" -> participant.square.key,
              "role" -> participant.role.map(_.name),
              "participantRole" -> participant.participantRole.toString
            )
          ),
          "proofAtoms" -> payload.proofAtoms.map(atom =>
            Json.obj(
              "role" -> atom.role.toString,
              "square" -> atom.square.map(_.key),
              "moveUci" -> atom.moveUci,
              "participantRole" -> atom.participantRole.map(_.toString),
              "pieceRole" -> atom.pieceRole.map(_.name),
              "label" -> atom.label
            )
          )
        )
      case EvidenceRecord(_, payload: TacticalMechanismEvidence, _) =>
        Json.obj(
          "payload" -> "TacticalMechanism",
          "kind" -> payload.kind.toString,
          "moveUci" -> payload.moveUci,
          "line" -> payload.line.map(lineRefSummary),
          "canAnchorTacticalIdea" -> payload.canAnchorTacticalIdea,
          "canAnchorDefensiveIdea" -> payload.canAnchorDefensiveIdea,
          "hasLineProof" -> payload.hasLineProof,
          "hasThreatProof" -> payload.hasThreatProof,
          "signals" -> payload.signals.map(signal =>
            Json.obj(
              "kind" -> signal.kind.toString,
              "label" -> signal.label,
              "sourceLayer" -> signal.sourceLayer.toString
            )
          )
        )
      case EvidenceRecord(_, ThreatEpisodeEvidence(episode, _), _) =>
        Json.obj(
          "payload" -> "ThreatEpisode",
          "episodeId" -> episode.episodeId,
          "sourceThreatIndex" -> episode.sourceThreatIndex,
          "sideUnderPressure" -> episode.sideUnderPressure.name,
          "kind" -> episode.kind.toString,
          "severity" -> episode.severity.toString,
          "driver" -> episode.driver.toString,
          "evidenceSource" -> episode.evidenceSource.toString,
          "rawLossIfIgnoredCpForDiagnostics" -> episode.rawLossIfIgnoredCpForDiagnostics,
          "lossIfIgnoredWinPercent" -> episode.lossIfIgnoredWinPercent,
          "turnsToImpact" -> episode.turnsToImpact,
          "immediate" -> episode.immediate,
          "strategic" -> episode.strategic,
          "attackSquares" -> episode.attackSquares.map(_.key),
          "targetPieces" -> episode.targetPieces.map(_.name),
          "motifKinds" -> episode.motifKinds,
          "bestDefense" -> episode.bestDefense,
          "defenseCount" -> episode.defenseCount
        )
      case EvidenceRecord(_, ThreatPressureEvidence(sideUnderPressure, threats), _) =>
        Json.obj(
          "payload" -> "ThreatPressureSummary",
          "sideUnderPressure" -> sideUnderPressure.name,
          "sourceThreatCount" -> threats.threatCount,
          "hasEpisodeRecords" -> threats.threats.nonEmpty,
          "summarySeverity" -> threats.threatSeverity.toString,
          "summaryDefenseRequired" -> threats.defenseRequired,
          "summaryProphylaxisNeeded" -> threats.prophylaxisNeeded,
          "summaryOnlyDefense" -> threats.defense.onlyDefense,
          "summaryMaxWinPercentLossIfIgnored" -> threats.maxWinPercentLossIfIgnored,
          "summaryPrimaryDriver" -> threats.primaryDriver.toString,
          "summaryInsufficientData" -> threats.insufficientData
        )
      case EvidenceRecord(_, payload: MoveMotifEvidence, _) =>
        Json.obj(
          "payload" -> "MoveMotif",
          "rootMove" -> payload.rootMove,
          "eventMove" -> payload.eventMove,
          "plyOffset" -> payload.plyOffset,
          "lineRole" -> payload.lineRole.map(_.toString),
          "kind" -> payload.proof.kind,
          "category" -> payload.proof.category.toString,
          "subjectSquares" -> payload.proof.subjectSquares.map(_.key),
          "targetSquares" -> payload.proof.targetSquares.map(_.key),
          "relatedSquares" -> payload.proof.relatedSquares.map(_.key),
          "relatedFiles" -> payload.proof.relatedFiles.map(_.key),
          "roles" -> payload.proof.roles.map(_.name)
        )
      case EvidenceRecord(_, ChessIdeaEvidence(idea), _) =>
        Json.obj(
          "payload" -> "ChessIdea",
          "id" -> idea.id,
          "family" -> idea.family.toString
        )
      case EvidenceRecord(ref, payload, _) =>
        Json.obj(
          "payload" -> payload.layer.toString,
          "evidenceId" -> ref.id
        )
    }

  private def ideaSummary(result: MoveReviewJudgmentResult, subjectId: String): Option[JsObject] =
    result.packet.ideas.find(_.ref.id == subjectId).map { idea =>
      Json.obj(
        "id" -> idea.ref.id,
        "family" -> idea.ref.family.toString,
        "subject" -> idea.subject.toString,
        "moveUci" -> idea.moveUci,
        "primaryLine" -> idea.primaryLine.map(lineRefSummary),
        "scope" -> idea.scope.toString,
        "confidence" -> idea.confidence.toString,
        "evidenceLayers" -> idea.evidence.map(_.layer.toString).distinct
      )
    }

  private def claimSummary(result: MoveReviewJudgmentResult, subjectId: String): Option[JsObject] =
    result.packet.claims.find(_.id == subjectId).map { claim =>
      Json.obj(
        "id" -> claim.id,
        "family" -> claim.family.toString,
        "subject" -> claim.subject.toString,
        "subjectMove" -> claim.subjectMove,
        "primaryLine" -> claim.primaryLine.map(lineRefSummary),
        "scope" -> claim.scope.toString,
        "confidence" -> claim.confidence.toString,
        "evidenceLayers" -> claim.evidence.map(_.layer.toString).distinct
      )
    }

  private def countsBy(values: List[String]): JsObject =
    JsObject(
      values
        .groupMapReduce(identity)(_ => 1)(_ + _)
        .toList
        .sortBy(_._1)
        .map((key, count) => key -> JsNumber(count))
    )

  private def evidenceLayerCounts(result: MoveReviewJudgmentResult): JsObject =
    val counts =
      result.packet.evidenceGraph.records
        .groupMapReduce(_.ref.layer.toString)(_ => 1)(_ + _)
        .toList
        .sortBy(_._1)
    JsObject(counts.map((layer, count) => layer -> JsNumber(count)))

  private def relationKinds(result: MoveReviewJudgmentResult): JsArray =
    JsArray(
      result.packet.evidenceGraph.records.collect {
        case lila.chessjudgment.model.judgment.EvidenceRecord(
              _,
              payload: lila.chessjudgment.model.judgment.RelationFactEvidence,
              _
            ) =>
          JsString(payload.kind.toString)
      }
    )

  private def strategicKinds(result: MoveReviewJudgmentResult): JsArray =
    JsArray(
      result.packet.evidenceGraph.records.collect {
        case lila.chessjudgment.model.judgment.EvidenceRecord(
              _,
              payload: lila.chessjudgment.model.judgment.StrategicFactEvidence,
              _
            ) =>
          JsString(payload.kind.toString)
      }
    )

  private def claimSupportClusters(result: MoveReviewJudgmentResult): JsArray =
    JsArray(
      result.packet.claimSupportClusters.map(cluster =>
        Json.obj(
          "id" -> cluster.id,
          "kind" -> cluster.kind.toString,
          "families" -> cluster.families.map(_.toString),
          "subject" -> cluster.subject.toString,
          "primaryPosition" -> Json.obj(
            "fen" -> cluster.primaryPosition.fen,
            "ply" -> cluster.primaryPosition.ply,
            "sideToMove" -> cluster.primaryPosition.sideToMove.map(_.name)
          ),
          "primaryLine" -> cluster.primaryLine.map(lineRefSummary),
          "subjectMove" -> cluster.subjectMove,
          "scope" -> cluster.scope.map(_.toString),
          "anchorClaimIds" -> cluster.anchorClaimIds,
          "supportingClaimIds" -> cluster.supportingClaimIds,
          "constrainingClaimIds" -> cluster.constrainingClaimIds,
          "ideas" -> cluster.ideas.map(idea => Json.obj("id" -> idea.id, "family" -> idea.family.toString)),
          "evidenceLayers" -> cluster.evidence.map(_.layer.toString).distinct,
          "presentLayers" -> cluster.presentLayers.map(_.toString).toList.sorted,
          "confidence" -> cluster.confidence.toString,
          "salienceDrivers" -> cluster.salienceDrivers.map(_.toString),
          "interactions" -> cluster.interactions.map(interaction =>
            Json.obj(
              "kind" -> interaction.kind.toString,
              "sourceClaimId" -> interaction.sourceClaimId,
              "targetClaimId" -> interaction.targetClaimId,
              "strength" -> interaction.strength,
              "interactionEvidence" -> interaction.evidence.map(_.id),
              "basis" -> interaction.basis.map(interactionBasisJson)
            )
          )
        )
      )
    )

  private def claimEventClusters(result: MoveReviewJudgmentResult): JsArray =
    JsArray(
      result.packet.claimEventClusters.map(cluster =>
        Json.obj(
          "id" -> cluster.id,
          "kind" -> cluster.kind.toString,
          "causeKind" -> cluster.causeKind.toString,
          "comparisonKind" -> cluster.comparisonKind.toString,
          "causeRole" -> cluster.causeRole.toString,
          "causeSourceSide" -> cluster.causeSourceSide.toString,
          "causeImportance" -> cluster.causeImportance.toString,
          "attributionKind" -> cluster.attributionKind.toString,
          "attributionRootMoveMatched" -> cluster.attributionRootMoveMatched,
          "attributionDirectProofEligible" -> cluster.attributionDirectProofEligible,
          "referenceLine" -> lineRefSummary(cluster.referenceLine),
          "candidateLine" -> lineRefSummary(cluster.candidateLine),
          "eventLine" -> lineRefSummary(cluster.eventLine),
          "eventRootMove" -> cluster.eventRootMove,
          "verdict" -> cluster.verdict.toString,
          "winPercentLossForMover" -> cluster.winPercentLossForMover,
          "candidateWinPercentDeltaForMover" -> cluster.candidateWinPercentDeltaForMover,
          "families" -> cluster.families.map(_.toString),
          "primaryPosition" -> Json.obj(
            "fen" -> cluster.primaryPosition.fen,
            "ply" -> cluster.primaryPosition.ply,
            "sideToMove" -> cluster.primaryPosition.sideToMove.map(_.name)
          ),
          "scope" -> cluster.scope.toString,
          "memberClaimIds" -> cluster.memberClaimIds,
          "causeClaimIds" -> cluster.causeClaimIds,
          "evaluationClaimIds" -> cluster.evaluationClaimIds,
          "witnessClaimIds" -> cluster.witnessClaimIds,
          "relatedSupportClusterIds" -> cluster.relatedSupportClusterIds,
          "ideas" -> cluster.ideas.map(idea => Json.obj("id" -> idea.id, "family" -> idea.family.toString)),
          "evidenceLayers" -> cluster.evidence.map(_.layer.toString).distinct,
          "presentLayers" -> cluster.presentLayers.map(_.toString).toList.sorted,
          "proofBoardAnchors" -> cluster.proofBoardAnchors.map(_.toString),
          "proofLineEvents" -> cluster.proofLineEvents.map(_.toString),
          "proofLineConsequences" -> cluster.proofLineConsequences.map(_.toString),
          "proofRelationKinds" -> cluster.proofRelationKinds.map(_.toString),
          "proofRelationDetails" -> cluster.proofRelationDetails,
          "proofTacticalMechanisms" -> cluster.proofTacticalMechanisms.map(tacticalMechanismProofJson),
          "proofStrategicMechanisms" -> cluster.proofStrategicMechanisms.map(strategicMechanismProofJson),
          "proofTransitionConsequences" -> cluster.proofTransitionConsequences.map(transitionConsequenceJson),
          "proofDirectSourceIds" -> cluster.proofDirectSourceIds,
          "proofContrastSourceIds" -> cluster.proofContrastSourceIds,
          "proofContextSupportSourceIds" -> cluster.proofContextSupportSourceIds,
          "proofDirectKinds" -> cluster.proofDirectKinds,
          "proofContrastKinds" -> cluster.proofContrastKinds,
          "proofContextSupportKinds" -> cluster.proofContextSupportKinds,
          "causeProofs" -> cluster.causeProofs.map(proof =>
            Json.obj(
              "claimId" -> proof.claimId,
              "family" -> proof.family.toString,
              "memberRole" -> proof.memberRole.toString,
              "causeKind" -> proof.causeKind.toString,
              "comparisonKind" -> proof.comparisonKind.toString,
              "causeRole" -> proof.causeRole.toString,
              "causeSourceSide" -> proof.causeSourceSide.toString,
              "causeImportance" -> proof.causeImportance.toString,
              "attributionKind" -> proof.attributionKind.toString,
              "attributionRootMoveMatched" -> proof.attributionRootMoveMatched,
              "attributionDirectProofEligible" -> proof.attributionDirectProofEligible,
              "referenceLine" -> lineRefSummary(proof.referenceLine),
              "candidateLine" -> lineRefSummary(proof.candidateLine),
              "eventLine" -> lineRefSummary(proof.eventLine),
              "proofDirectSourceIds" -> proof.proofDirectSourceIds,
              "proofContrastSourceIds" -> proof.proofContrastSourceIds,
              "proofContextSupportSourceIds" -> proof.proofContextSupportSourceIds,
              "proofStrategicAxisKeys" -> proof.proofStrategicAxisKeys,
              "proofStrategicMechanismKinds" -> proof.proofStrategicMechanismKinds.map(_.toString),
              "proofStrategicMechanismSourceIds" -> proof.proofStrategicMechanismSourceIds,
              "proofStrategicMechanismSignalSourceIds" -> proof.proofStrategicMechanismSignalSourceIds,
              "supportEvidenceSourceIds" -> proof.supportEvidenceSourceIds,
              "proofDirectKinds" -> proof.proofDirectKinds,
              "proofContrastKinds" -> proof.proofContrastKinds,
              "proofContextSupportKinds" -> proof.proofContextSupportKinds
            )
          ),
          "causeContextLayers" -> cluster.causeContextLayers.map(_.toString).toList.sorted,
          "confidence" -> cluster.confidence.toString,
          "salienceDrivers" -> cluster.salienceDrivers.map(_.toString),
          "interactions" -> cluster.interactions.map(interaction =>
            Json.obj(
              "kind" -> interaction.kind.toString,
              "sourceClaimId" -> interaction.sourceClaimId,
              "targetClaimId" -> interaction.targetClaimId,
              "strength" -> interaction.strength,
              "interactionEvidence" -> interaction.evidence.map(_.id),
              "basis" -> interaction.basis.map(interactionBasisJson)
            )
          )
        )
      )
    )

  private def moveJudgmentView(result: MoveReviewJudgmentResult, publicSurface: JsObject): JsValue =
    result.packet.moveJudgmentView.fold[JsValue](JsNull)(view =>
      Json.obj(
        "verdict" -> view.verdict.map(moveJudgmentVerdictJson),
        "verdictCarriers" -> view.verdictCarriers.map(moveJudgmentClaimFrameJson),
        "causeAudit" -> Json.obj(
          "primary" -> view.causeAudit.primary.map(moveJudgmentCauseFrameJson(result.packet, _)),
          "secondary" -> view.causeAudit.secondary.map(moveJudgmentCauseFrameJson(result.packet, _)),
          "context" -> view.causeAudit.context.map(moveJudgmentCauseFrameJson(result.packet, _))
        ),
        "publicSurface" -> publicSurface,
        "moveMeaningClaims" -> view.moveMeaningClaims.map(moveMeaningClaimJson),
        "positionPlanTechniqueFrames" -> view.positionPlanTechniqueFrames.map(moveJudgmentPositionPlanTechniqueFrameJson),
        "supportContextClusterIds" -> view.supportContextClusterIds,
        "overriddenLocalIdeas" -> view.overriddenLocalIdeas.map(moveJudgmentLocalIdeaFrameJson),
        "preservedLocalIdeas" -> view.preservedLocalIdeas.map(moveJudgmentLocalIdeaFrameJson)
      )
    )

  private def moveJudgmentVerdictJson(frame: MoveJudgmentVerdictFrame): JsObject =
    Json.obj(
      "verdict" -> frame.verdict.toString,
      "winPercentLossForMover" -> frame.winPercentLossForMover,
      "candidateWinPercentDeltaForMover" -> frame.candidateWinPercentDeltaForMover,
      "relativeAssessmentEvidenceId" -> frame.relativeAssessmentEvidenceId,
      "verdictCertificationEvidenceId" -> frame.verdictCertificationEvidenceId,
      "comparisonKind" -> frame.comparisonKind.toString,
      "referenceLine" -> lineRefSummary(frame.referenceLine),
      "candidateLine" -> lineRefSummary(frame.candidateLine)
    )

  private def moveJudgmentClaimFrameJson(frame: MoveJudgmentClaimFrame): JsObject =
    Json.obj(
      "claimId" -> frame.claimId,
      "family" -> frame.family.toString,
      "tier" -> frame.tier.toString,
      "subjectBinding" -> frame.subjectBinding.toString,
      "ideaIds" -> frame.ideaIds,
      "evidenceIds" -> frame.evidenceIds,
      "strategicAxisLineage" -> strategicAxisLineageJson(frame.strategicAxisLineage),
      "strategicAxisKeys" -> frame.strategicAxisKeys,
      "strategicAxisMechanismEvidenceIds" -> frame.strategicAxisMechanismEvidenceIds,
      "strategicAxisSourceEvidenceIds" -> frame.strategicAxisSourceEvidenceIds,
      "strategicAxisRelativeCauseIds" -> frame.strategicAxisRelativeCauseIds,
      "concreteObjectReady" -> frame.concreteObjectReady
    )

  private def moveJudgmentCauseFrameJson(packet: EvidenceBackedJudgmentPacket, frame: MoveJudgmentCauseFrame): JsObject =
    val ideasById = packet.ideas.map(idea => idea.ref.id -> idea).toMap
    val claimsById = packet.claims.map(claim => claim.id -> claim).toMap
    val lifecycleByCandidateId = packet.claimLifecycle.map(diagnostic => diagnostic.candidateId -> diagnostic).toMap
    Json.obj(
      "role" -> frame.role.toString,
      "narrativeRole" -> frame.narrativeRole.toString,
      "clusterId" -> frame.clusterId,
      "framed" -> frame.framed,
      "causeEvidenceIds" -> frame.causeEvidenceIds,
      "causeKind" -> frame.causeKind.toString,
      "comparisonKind" -> frame.comparisonKind.toString,
      "causeRole" -> frame.causeRole.toString,
      "causeSourceSide" -> frame.causeSourceSide.toString,
      "causeImportance" -> frame.causeImportance.toString,
      "attributionKind" -> frame.attributionKind.toString,
      "attributionRootMoveMatched" -> frame.attributionRootMoveMatched,
      "attributionDirectProofEligible" -> frame.attributionDirectProofEligible,
      "referenceLine" -> lineRefSummary(frame.referenceLine),
      "candidateLine" -> lineRefSummary(frame.candidateLine),
      "eventLine" -> lineRefSummary(frame.eventLine),
      "eventRootMove" -> frame.eventRootMove,
      "causeClaimIds" -> frame.causeClaimIds,
      "evaluationClaimIds" -> frame.evaluationClaimIds,
      "witnessClaimIds" -> frame.witnessClaimIds,
      "ideaIds" -> frame.ideaIds,
      "ideaFamilies" -> frame.ideaIds.flatMap(id => ideasById.get(id).map(_.ref.family.toString)).distinct.sorted,
      "supportIdeaIds" -> frame.supportIdeaIds,
      "supportIdeaFamilies" -> frame.supportIdeaIds.flatMap(id => ideasById.get(id).map(_.ref.family.toString)).distinct.sorted,
      "claimCandidateIds" -> frame.claimCandidateIds,
      "claimCandidateFamilies" -> frame.claimCandidateIds.flatMap(id =>
        lifecycleByCandidateId.get(id).map(_.family.toString)
      ).distinct.sorted,
      "finalClaimIds" -> frame.finalClaimIds,
      "finalClaimFamilies" -> frame.finalClaimIds.flatMap(id => claimsById.get(id).map(_.family.toString)).distinct.sorted,
      "relatedSupportClusterIds" -> frame.relatedSupportClusterIds,
      "evidenceIds" -> frame.evidenceIds,
      "proofDirectSourceIds" -> frame.proofDirectSourceIds,
      "proofContrastSourceIds" -> frame.proofContrastSourceIds,
      "proofContextSupportSourceIds" -> frame.proofContextSupportSourceIds,
      "proofStrategicAxisLineage" -> strategicAxisProofLineageJson(frame.proofStrategicAxisLineage),
      "proofStrategicAxisKeys" -> frame.proofStrategicAxisKeys,
      "proofStrategicMechanismKinds" -> frame.proofStrategicMechanismKinds.map(_.toString),
      "proofStrategicMechanismSourceIds" -> frame.proofStrategicMechanismSourceIds,
      "proofStrategicMechanismSignalSourceIds" -> frame.proofStrategicMechanismSignalSourceIds,
      "supportEvidenceSourceIds" -> frame.supportEvidenceSourceIds,
      "proofRelationKinds" -> frame.proofRelationKinds.map(_.toString),
      "proofRelationDetails" -> frame.proofRelationDetails,
      "hasOwnedAdmissibleLongTermProof" -> frame.hasOwnedAdmissibleLongTermProof,
      "rootArbitrationTier" -> frame.rootArbitrationTier.toString,
      "tacticalWitnessCauseEvidenceIds" -> frame.tacticalWitnessCauseEvidenceIds,
      "tacticalWitnessCauseKinds" -> frame.tacticalWitnessCauseKinds.map(_.toString),
      "punishmentWitnessCauseEvidenceIds" -> frame.punishmentWitnessCauseEvidenceIds,
      "punishmentWitnessCauseKinds" -> frame.punishmentWitnessCauseKinds.map(_.toString),
      "contextualTacticalWitnessCauseEvidenceIds" -> frame.contextualTacticalWitnessCauseEvidenceIds,
      "contextualTacticalWitnessCauseKinds" -> frame.contextualTacticalWitnessCauseKinds.map(_.toString),
      "witnessBindingLevel" -> frame.witnessBindingLevel.toString,
      "witnessBindingSignals" -> frame.witnessBindingSignals.map(_.toString),
      "witnessBindingRootCauseEvidenceIds" -> frame.witnessBindingRootCauseEvidenceIds,
      "concreteObjectReady" -> frame.concreteObjectReady
    )

  private def moveMeaningClaimJson(claim: MoveMeaningClaim): JsObject =
    Json.obj(
      "meaningKind" -> claim.meaningKind,
      "role" -> claim.role,
      "laneKey" -> claim.laneKey,
      "conflictKey" -> claim.conflictKey,
      "supportLevel" -> claim.supportLevel,
      "visibility" -> claim.visibility,
      "surfaceLane" -> claim.surfaceLane,
      "lineRole" -> claim.lineRole,
      "moveUci" -> claim.moveUci,
      "frameId" -> claim.frameId,
      "unit" -> claim.unit.toString,
      "axisKey" -> claim.axisKey,
      "axisKind" -> claim.axisKind.map(_.toString),
      "axisPolarity" -> claim.axisPolarity.map(_.toString),
      "label" -> claim.label,
      "causeKinds" -> claim.causeKinds.map(_.toString),
      "causeSourceSides" -> claim.causeSourceSides.map(_.toString),
      "causeEvidenceIds" -> claim.causeEvidenceIds,
      "sourceEvidenceIds" -> claim.sourceEvidenceIds,
      "objectBindingSignatureCount" -> claim.objectBindingSignatures.size,
      "objectBindingSignaturesSample" -> MoveReviewPhase3AuditViewJson.objectBindingSignatureSample(claim.objectBindingSignatures),
      "objectCarrierReady" -> claim.objectCarrierReady,
      "boardCarrierCount" -> claim.boardCarriers.size,
      "boardCarriersSample" -> claim.boardCarriers.take(8).map(carrier =>
        Json.obj(
          "role" -> carrier.role,
          "kind" -> carrier.kind,
          "value" -> carrier.value,
          "from" -> carrier.from,
          "to" -> carrier.to
        )
      ),
      "publicHasCarrier" -> claim.publicHasCarrier,
      "publicProofLevel" -> claim.publicProofLevel,
      "publicTargetBound" -> claim.publicTargetBound,
      "targetSquares" -> claim.targetSquares,
      "targetFiles" -> claim.targetFiles,
      "targetPieces" -> claim.targetPieces,
      "routeIdentityParts" -> claim.routeIdentityParts,
      "breakIdentityParts" -> claim.breakIdentityParts,
      "breakFiles" -> claim.breakFiles,
      "comparisonMoveRefs" -> claim.comparisonMoveRefs.map(ref =>
        Json.obj(
          "role" -> ref.role,
          "uci" -> ref.uci
        )
      ),
      "terminalConsequenceKinds" -> claim.terminalConsequenceKinds,
      "proofRelationKinds" -> claim.proofRelationKinds.map(_.toString),
      "proofRelationDetails" -> claim.proofRelationDetails,
      "endgameTechniquePattern" -> claim.endgameTechniquePattern,
      "endgameTechniqueRookPattern" -> claim.endgameTechniqueRookPattern,
      "endgameTechniqueSide" -> claim.endgameTechniqueSide,
      "endgameTechniqueHorizonStatus" -> claim.endgameTechniqueHorizonStatus,
      "endgameTechniqueTriggerMove" -> claim.endgameTechniqueTriggerMove,
      "endgameTechniqueEntryPlyOffset" -> claim.endgameTechniqueEntryPlyOffset,
      "endgameTechniqueTerminalPlyOffset" -> claim.endgameTechniqueTerminalPlyOffset,
      "endgameTechniqueFailureReason" -> claim.endgameTechniqueFailureReason,
      "requiredSquares" -> claim.requiredSquares,
      "maintainedSquares" -> claim.maintainedSquares,
      "brokenSquares" -> claim.brokenSquares
    )

  private def moveJudgmentPositionPlanTechniqueFrameJson(frame: PositionPlanTechniqueFrame): JsObject =
    MoveReviewPhase3AuditViewJson.positionPlanTechniqueFrameJson(frame, lineRefSummary)
  private def moveJudgmentLocalIdeaFrameJson(frame: MoveJudgmentLocalIdeaFrame): JsObject =
    Json.obj(
      "ideaId" -> frame.ideaId,
      "relation" -> frame.relation.toString,
      "claimIds" -> frame.claimIds,
      "evidenceIds" -> frame.evidenceIds
    )

  private def rankedClaimDiagnosticsByTier(
      result: MoveReviewJudgmentResult,
      tier: PlayerFacingClaimTier
  ): JsArray =
    JsArray(
      result.packet.claims
        .filter(claim => PlayerFacingClaimPolicy.tier(result.packet, claim) == tier)
        .take(5)
        .map(claim => claimJson(result.packet, claim))
    )

  private def claimJson(packet: EvidenceBackedJudgmentPacket, claim: ClaimSeed): JsObject =
    val strategicAxisLineage = ClaimStrategicAxisLineage.fromClaim(claim, packet.evidenceGraph)
    Json.obj(
      "id" -> claim.id,
      "family" -> claim.family.toString,
      "subject" -> claim.subject.toString,
      "subjectMove" -> claim.subjectMove,
      "playedSubjectBinding" -> JudgmentSubjectBinding
        .playedSubjectBinding(
          claim.subjectMove,
          claim.primaryLine,
          JudgmentSubjectBinding.packetPlayedMoves(packet)
        )
        .toString,
      "subjectBinding" -> JudgmentSubjectBinding.claimBinding(packet, claim).toString,
      "playerFacingTier" -> PlayerFacingClaimPolicy.tier(packet, claim).toString,
      "scope" -> claim.scope.toString,
      "confidence" -> claim.confidence.toString,
      "supportStatus" -> claim.supportStatus.map(_.status.toString),
      "salienceScore" -> claim.salience.map(_.score),
      "salienceDrivers" -> claim.salience.map(_.drivers.map(_.toString)),
      "claimInteractions" -> claim.salience.map(_.interactions.map(interaction =>
        Json.obj(
          "kind" -> interaction.kind.toString,
          "relatedClaimId" -> interaction.relatedClaimId,
          "strength" -> interaction.strength,
          "interactionEvidence" -> interaction.interactionEvidence.map(_.id),
          "basis" -> interaction.basis.map(interactionBasisJson)
        )
      )),
      "strategicAxisLineage" -> strategicAxisLineageJson(strategicAxisLineage),
      "strategicAxisKeys" -> strategicAxisLineage.map(_.axisKey).distinct.sorted,
      "strategicAxisMechanismEvidenceIds" -> strategicAxisLineage.map(_.mechanismEvidenceId).distinct.sorted,
      "strategicAxisSourceEvidenceIds" -> strategicAxisLineage.map(_.signalSourceEvidenceId).distinct.sorted,
      "strategicAxisRelativeCauseIds" -> strategicAxisLineage.flatMap(_.relativeCauseIds).distinct.sorted,
      "engineVerdict" -> claim.engineComparison.map(_.verdict.toString),
      "engineWinPercentLossForMover" -> claim.engineComparison.map(_.winPercentLossForMover),
      "relatedComparisonIds" -> claimRelatedComparisonIds(packet, claim),
      "relativeCauseKinds" -> claimRelativeCauses(packet, claim).map(_.kind.toString).distinct,
      "relativeCauseRoles" -> claimRelativeCauses(packet, claim).map(_.role.toString).distinct,
      "relativeCauseSourceSides" -> claimRelativeCauses(packet, claim).map(_.sourceSide.toString).distinct,
      "relativeCauseImportances" -> claimRelativeCauses(packet, claim).map(_.importance.toString).distinct,
      "relativeCauseProof" -> claimRelativeCauseProofJson(packet, claim),
      "relativeCauseDetails" -> claimRelativeCauses(packet, claim).map(relativeCauseDetailJson),
      "comparisonKinds" -> claimComparisonKinds(packet, claim).map(_.toString),
      "missingLayerGroups" -> claim.supportStatus.map(_.missingLayerGroups.map(_.map(_.toString).toList.sorted)),
      "evidenceLayers" -> claim.evidence.map(_.layer.toString),
      "eventBinding" -> claimEventBinding(packet, claim)
    )

  private def claimRecords(packet: EvidenceBackedJudgmentPacket, claim: ClaimSeed): List[EvidenceRecord] =
    claim.evidence.flatMap(ref => packet.evidenceGraph.byId.get(ref.id))

  private def claimRelativeCauses(packet: EvidenceBackedJudgmentPacket, claim: ClaimSeed): List[RelativeCauseFact] =
    claimRecords(packet, claim).flatMap {
      case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) =>
        List(cause)
      case EvidenceRecord(_, MoveVerdictCertificationEvidence(certification), _) =>
        certification.causes
      case _ =>
        Nil
    }

  private def claimComparisonKinds(packet: EvidenceBackedJudgmentPacket, claim: ClaimSeed): List[CandidateComparisonKind] =
    claimRecords(packet, claim).flatMap {
      case EvidenceRecord(_, CandidateComparisonEvidence(fact), _) =>
        List(fact.kind)
      case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) =>
        List(cause.comparisonKind)
      case EvidenceRecord(_, MoveVerdictCertificationEvidence(certification), _) =>
        certification.primaryComparison.kind :: certification.causes.map(_.comparisonKind)
      case _ =>
        Nil
    }.distinct

  private def claimRelativeCauseProofJson(packet: EvidenceBackedJudgmentPacket, claim: ClaimSeed): JsObject =
    val causes = claimRelativeCauses(packet, claim)
    Json.obj(
      "hasOwnedTypedDepth" -> causes.exists(_.hasOwnedTypedDepth),
      "hasOwnedTacticalProof" -> causes.exists(_.hasOwnedTacticalProof),
      "hasOwnedStrategicContrastDepth" -> causes.exists(_.hasOwnedStrategicContrastDepth),
      "hasOwnedAdmissibleLongTermProof" -> causes.exists(_.hasOwnedAdmissibleLongTermProof),
      "directProofSourceIds" -> causes.flatMap(_.proof.toList.flatMap(_.directProof.sourceRefs.map(_.id))).distinct.sorted,
      "contrastProofSourceIds" -> causes.flatMap(_.proof.toList.flatMap(_.contrastProof.sourceRefs.map(_.id))).distinct.sorted,
      "contextSupportSourceIds" -> causes.flatMap(_.proof.toList.flatMap(_.contextSupport.sourceRefs.map(_.id))).distinct.sorted,
      "ownedEvidenceIds" -> causes.flatMap(_.attribution.ownedEvidence.map(_.id)).distinct.sorted,
      "attributionContrastEvidenceIds" -> causes.flatMap(_.attribution.contrastEvidence.map(_.id)).distinct.sorted,
      "attributionContextEvidenceIds" -> causes.flatMap(_.attribution.contextEvidence.map(_.id)).distinct.sorted
    )

  private def relativeCauseDetailJson(cause: RelativeCauseFact): JsObject =
    Json.obj(
      "kind" -> cause.kind.toString,
      "role" -> cause.role.toString,
      "sourceSide" -> cause.sourceSide.toString,
      "importance" -> cause.importance.toString,
      "comparisonKind" -> cause.comparisonKind.toString,
      "referenceLine" -> lineRefSummary(cause.referenceLine),
      "candidateLine" -> lineRefSummary(cause.candidateLine),
      "eventLine" -> lineRefSummary(cause.eventLine),
      "verdict" -> cause.verdict.toString,
      "winPercentLossForMover" -> cause.winPercentLossForMover,
      "candidateWinPercentDeltaForMover" -> cause.candidateWinPercentDeltaForMover,
      "hasOwnedTypedDepth" -> cause.hasOwnedTypedDepth,
      "hasOwnedTacticalProof" -> cause.hasOwnedTacticalProof,
      "hasOwnedStrategicContrastDepth" -> cause.hasOwnedStrategicContrastDepth,
      "hasOwnedAdmissibleLongTermProof" -> cause.hasOwnedAdmissibleLongTermProof,
      "attribution" -> Json.obj(
        "kind" -> cause.attribution.kind.toString,
        "ownedEvidenceIds" -> cause.attribution.ownedEvidence.map(_.id).distinct.sorted,
        "contrastEvidenceIds" -> cause.attribution.contrastEvidence.map(_.id).distinct.sorted,
        "contextEvidenceIds" -> cause.attribution.contextEvidence.map(_.id).distinct.sorted,
        "rootMoveMatched" -> cause.attribution.rootMoveMatched,
        "directProofEligible" -> cause.attribution.directProofEligible,
        "contextOnly" -> cause.attribution.contextOnly,
        "unattributed" -> cause.attribution.unattributed,
        "rootMismatch" -> cause.attribution.rootMismatch,
        "reason" -> cause.attribution.reason
      ),
      "evidenceLines" -> cause.evidenceLines.map(lineRefSummary),
      "supportEvidenceIds" -> cause.supportEvidence.map(_.id).distinct.sorted,
      "proof" -> cause.proof.map(proof =>
        Json.obj(
          "rawHasTypedDepth" -> proof.hasRawTypedDepth,
          "rawHasDirectProof" -> proof.hasRawDirectProof,
          "rawHasContrastProof" -> proof.hasRawContrastProof,
          "rawHasContextSupport" -> proof.hasRawContextSupport,
          "directProof" -> relativeCauseProofSectionJson(proof.directProof),
          "contrastProof" -> relativeCauseProofSectionJson(proof.contrastProof),
          "contextSupport" -> relativeCauseProofSectionJson(proof.contextSupport)
        )
      )
    )

  private def claimRelatedComparisonIds(packet: EvidenceBackedJudgmentPacket, claim: ClaimSeed): List[String] =
    claimRecords(packet, claim).flatMap {
      case EvidenceRecord(ref, CandidateComparisonEvidence(_), _) =>
        List(ref.id)
      case EvidenceRecord(_, RelativeCauseFactEvidence(_), parents) =>
        parents.filter(_.layer == EvidenceLayer.CandidateComparison).map(_.id)
      case EvidenceRecord(_, MoveVerdictCertificationEvidence(_), parents) =>
        parents.filter(_.layer == EvidenceLayer.CandidateComparison).map(_.id)
      case _ =>
        Nil
    }.distinct.sorted

  private def claimEventBinding(packet: EvidenceBackedJudgmentPacket, claim: ClaimSeed): JsObject =
    val clusterRoles =
      packet.claimEventClusters.flatMap { cluster =>
        val role =
          if cluster.causeClaimIds.contains(claim.id) then Some("CauseOwner")
          else if cluster.evaluationClaimIds.contains(claim.id) then Some("VerdictCarrier")
          else if cluster.witnessClaimIds.contains(claim.id) then Some("Witness")
          else if cluster.memberClaimIds.contains(claim.id) then Some("Member")
          else None
        role.map(role =>
          Json.obj(
            "clusterId" -> cluster.id,
            "role" -> role,
            "kind" -> cluster.kind.toString,
            "causeKind" -> cluster.causeKind.toString,
            "comparisonKind" -> cluster.comparisonKind.toString,
            "causeRole" -> cluster.causeRole.toString,
            "causeSourceSide" -> cluster.causeSourceSide.toString,
            "causeImportance" -> cluster.causeImportance.toString,
            "eventRootMove" -> cluster.eventRootMove
          )
        )
      }
    Json.obj(
      "hasDirectEventEvidence" -> claim.evidence.exists(ref =>
        ref.layer == EvidenceLayer.RelativeCause || ref.layer == EvidenceLayer.MoveVerdictCertification
      ),
      "clusterRoles" -> clusterRoles
    )

private object MoveReviewQualityInputFiles:

  def parseJsonDocuments(path: Path): List[JsValue] =
    val raw = Files.readString(path, StandardCharsets.UTF_8).stripPrefix("\uFEFF").trim
    if raw.startsWith("[") then Json.parse(raw).as[List[JsValue]]
    else raw.linesIterator.toList.filter(_.trim.nonEmpty).map(line => Json.parse(line))
