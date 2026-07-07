package lila.chessjudgment.analysis.assembly

import chess.Color
import chess.format.Fen
import chess.variant.Standard
import lila.chessjudgment.analysis.evaluation.{ EvaluationPerspectivePolicy, PerspectiveMath }
import lila.chessjudgment.analysis.move.{ MoveAnalyzer, MoveMotifNormalizer }
import lila.chessjudgment.analysis.opening.OpeningContextFactNormalizer
import lila.chessjudgment.analysis.plan.{ PlanInteractionContext, PlanMatcher }
import lila.chessjudgment.analysis.singlePosition.{ PawnPlayAssessor, PawnPlayDriver, PvLine, ThreatAnalysis, ThreatDriver, ThreatPressureAssessor }
import lila.chessjudgment.analysis.strategic.StrategicFactNormalizer
import lila.chessjudgment.analysis.structure.{
  PawnStructureAssessor,
  PlanAlignmentScorer,
  StructuralDeltaAnalyzer,
  StructuralPlaybook
}
import lila.chessjudgment.analysis.tactical.{ RelationFactNormalizer, TacticalMotifClassifier, TacticalRelationEvidence }
import lila.chessjudgment.analysis.transition.{ TransitionAnalyzer, TransitionFactNormalizer }
import lila.chessjudgment.model.{ CompatibilityAdjustment, Motif, PlanCategory, TransitionType }
import lila.chessjudgment.model.judgment.*

final case class EvidenceFactAssembly(
    input: NormalizedMoveReviewInput,
    context: JudgmentAssemblyContext
)

object EvidenceFactAssembler:

  private val OpeningRelevanceMaxPly = 20

  private final case class TacticalMechanismCandidate(
      kind: TacticalMechanismKind,
      records: List[EvidenceRecord],
      signals: List[TacticalMechanismSignal]
  )

  def assemble(raw: RawMoveReviewInput): Option[EvidenceFactAssembly] =
    NodeLineTransitionAssembler.assemble(raw).map(enrich)

  def enrich(assembly: NodeLineTransitionAssembly): EvidenceFactAssembly =
    val allocator = JudgmentProvenanceAllocator.forInput(assembly.input)
    val context = assembly.context
    val motifRecords = moveMotifRecords(assembly.input, context, allocator)
    val motifContext = context.withEvidence(motifRecords)
    val baseRecords =
      motifRecords ++ List.concat(
        relationRecords(assembly.input, context, allocator),
        pawnStructureRecords(motifContext, allocator),
        threatPressureRecords(assembly.input, motifContext, allocator),
        structuralDeltaRecords(context, allocator)
      )
    val baseContext = context.withEvidence(baseRecords)
    val mechanismRecords = tacticalMechanismRecords(baseContext, allocator)
    val mechanismContext = baseContext.withEvidence(mechanismRecords)
    val strategicRecords = strategicFeatureRecords(mechanismContext, allocator)
    val strategicContext = mechanismContext.withEvidence(strategicRecords)
    val planRecords = planPressureRecords(assembly.input, strategicContext, allocator)
    val planContext = strategicContext.withEvidence(planRecords)
    val openingRecords = featureApplicabilityRecords(assembly.input, planContext, allocator)
    val openingContext = planContext.withEvidence(openingRecords)
    val strategicMechanismOutput = strategicMechanismRecords(openingContext, allocator)
    EvidenceFactAssembly(assembly.input, openingContext.withEvidence(strategicMechanismOutput))

  private def moveMotifRecords(
      input: NormalizedMoveReviewInput,
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator
  ): List[EvidenceRecord] =
    val before = context.position(PositionNodeRole.Before).toList
    before.flatMap { root =>
      val transitionRecords =
        List(
          context.playedTransition.toList.flatMap { edge =>
            val line = lineForTransition(context, edge)
            moveMotifRecord(
              id = allocator.evidenceId(s"move-motif:played:${edge.moveUci}"),
              startFen = input.beforeFen,
              position = root.ref,
              moveUci = edge.moveUci,
              moves = List(edge.moveUci),
              line = line,
              scope = EvidenceScope.PlayedTransition,
              parents = transitionParents(context, edge, line)
            )
          },
          context.referenceTransition.toList.flatMap { edge =>
            val line = lineForTransition(context, edge)
            moveMotifRecord(
              id = allocator.evidenceId(s"move-motif:reference:${edge.moveUci}"),
              startFen = input.beforeFen,
              position = root.ref,
              moveUci = edge.moveUci,
              moves = List(edge.moveUci),
              line = line,
              scope = EvidenceScope.ReferenceTransition,
              parents = transitionParents(context, edge, line)
            )
          }
        ).flatten
      val lineRecords =
        context.lines.flatMap { line =>
          lineFactEvidence(context, line.ref).toList.flatMap { lineFacts =>
            val startPosition = startPositionForLine(input, context, root, line)
            moveMotifRecord(
              id = allocator.evidenceId(s"move-motif:line:${allocator.key(line.role)}:${line.ref.rank}:${line.ref.rootMove}"),
              startFen = startPosition.ref.fen,
              position = startPosition.ref,
              moveUci = line.ref.rootMove,
              moves = lineFacts.lineReplayMoves,
              line = Some(line),
              scope = line.role.scope,
              parents = lineParents(context, line)
            )
          }
        }
      transitionRecords ++ lineRecords
    }

  private def moveMotifRecord(
      id: String,
      startFen: String,
      position: PositionNodeRef,
      moveUci: String,
      moves: List[String],
      line: Option[CandidateLineNode],
      scope: EvidenceScope,
      parents: List[EvidenceRef]
  ): List[EvidenceRecord] =
    MoveAnalyzer.tokenizePv(startFen, moves).flatMap { motifs =>
      Option.when(motifs.nonEmpty) {
        MoveMotifNormalizer.fromMotifs(
          id = id,
          moveUci = moveUci,
          motifs = motifs,
          position = position,
          line = line.map(_.ref),
          scope = scope,
          parents = parents
        )
      }
    }.getOrElse(Nil)

  private def relationRecords(
      input: NormalizedMoveReviewInput,
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator
  ): List[EvidenceRecord] =
    val root = context.position(PositionNodeRole.Before).map(_.ref)
    root.toList.flatMap { rootRef =>
      val continuationLines =
        context.lines
          .filterNot(_.role == LineNodeRole.Threat)
          .flatMap(line => lineFactEvidence(context, line.ref).map(_.lineReplayMoves))
      val relationTargetHints = relationTargetHintsFromBoardFacts(context)
      val lineBackedRecords = context.lines.filterNot(_.role == LineNodeRole.Threat).flatMap { line =>
        lineFactEvidence(context, line.ref)
          .flatMap(lineFacts => TacticalRelationEvidence.boundedReplayFromSteps(lineFacts.lineReplaySteps, maxPlies = 8))
          .toList
          .flatMap { replay =>
            val witnesses =
              TacticalRelationEvidence
                .relationWitnesses(
                  replay = replay,
                  playedMove = line.ref.rootMove,
                  targetHints = relationTargetHints,
                  continuationLines = continuationLines,
                  engineScoreCp = Some(line.whitePovEvalCp),
                  engineMate = line.mate,
                  drawishWinPercent = Some(PerspectiveMath.winPercentFromWhiteEval(line.whitePovEvalCp, line.mate))
                )
                .distinctBy(witness => (witness.kind, witness.focusSquares, witness.targetSquare, witness.lineMoves))
            witnesses.zipWithIndex.flatMap { case (witness, index) =>
              RelationFactNormalizer.fromWitness(
                id = allocator.evidenceId(s"relation:${allocator.key(line.role)}:${line.ref.rank}:$index:${witness.kind}"),
                witness = witness,
                position = rootRef,
                line = Some(line.ref),
                scope = line.role.scope,
                confidence = EvidenceConfidence.LegalReplayVerified
              ).map { record =>
                record.copy(parents = lineParents(context, line))
              }
            }
          }
      }
      val transitionLocalRecords =
        context.playedTransition.toList
          .filter(edge => lineForTransition(context, edge).isEmpty)
          .flatMap { edge =>
            TacticalRelationEvidence
              .boundedReplay(input.beforeFen, List(edge.moveUci), maxPlies = 1)
              .toList
              .flatMap { replay =>
                val witnesses =
                  TacticalRelationEvidence
                    .relationWitnesses(
                      replay = replay,
                      playedMove = edge.moveUci,
                      targetHints = relationTargetHints,
                      continuationLines = continuationLines,
                      includeDrawResources = false
                    )
                    .distinctBy(witness => (witness.kind, witness.focusSquares, witness.targetSquare, witness.lineMoves))
                witnesses.zipWithIndex.flatMap { case (witness, index) =>
                  RelationFactNormalizer.fromWitness(
                    id = allocator.evidenceId(s"relation:${allocator.key(edge.role)}:transition:$index:${witness.kind}"),
                    witness = witness,
                    position = rootRef,
                    line = None,
                    scope = edge.role.scope,
                    confidence = EvidenceConfidence.LegalReplayVerified
                  ).map { record =>
                    record.copy(parents = transitionParents(context, edge, None))
                  }
                }
              }
          }
      lineBackedRecords ++ transitionLocalRecords
    }

  private def relationTargetHintsFromBoardFacts(context: JudgmentAssemblyContext): List[EvidenceSquare] =
    context.evidenceGraph.records.collect {
      case EvidenceRecord(_, payload: BoardFactEvidence, _) =>
        payload.targetHintSquares
    }.flatten.distinct

  private def pawnStructureRecords(
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator
  ): List[EvidenceRecord] =
    context.positions.flatMap { node =>
      (for
        features <- node.features
        position <- Fen.read(Standard, Fen.Full(node.ref.fen))
      yield
        val side = node.ref.sideToMove.getOrElse(position.color)
        val profile = PawnStructureAssessor.assess(features, position.board)
        val pawnPlay =
          node.assessment.flatMap { assessment =>
            PawnPlayAssessor.analyze(
              features = features,
              motifs = pawnPlayMotifsForPosition(context, node.role),
              positionAssessment = assessment,
              sideToMove = side
            )
          }
        val record = StrategicFactNormalizer.fromPawnStructure(
          id = allocator.evidenceId(s"pawn-structure:${allocator.key(node.role)}"),
          profile = profile,
          alignment = None,
          pawnPlay = pawnPlay,
          position = node.ref,
          scope = node.role.scope,
          parents = evidenceRefs(context, EvidenceLayer.Board, Some(node.ref), None) ++
            evidenceRefs(context, EvidenceLayer.SinglePosition, Some(node.ref), None)
        )
        record.payload match
          case payload: PawnStructureFactEvidence if StrategicMechanismEvidence.pawnStructureCanAnchorPlan(payload) =>
            Some(record)
          case _ =>
            None
      ).toList.flatten
    }

  private def threatPressureRecords(
      input: NormalizedMoveReviewInput,
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator
  ): List[EvidenceRecord] =
    val beforeRecords = context.position(PositionNodeRole.Before).toList.flatMap { node =>
      node.assessment.toList.flatMap { assessment =>
        node.ref.sideToMove.toList.flatMap { sideUnderPressure =>
          val threats =
            ThreatPressureAssessor.analyze(
              fen = input.beforeFen,
              motifs = motifsForLineRole(context, LineNodeRole.BestReference),
              multiPv = EvaluationPerspectivePolicy.sideToMovePvLines(sideUnderPressure, input.lines.map(_.line)),
              positionAssessment = assessment,
              sideToMove = sideUnderPressure
            )
          threatPressureBundle(
            baseId = s"threat-pressure:${allocator.key(sideUnderPressure.name)}:before",
            allocator = allocator,
            sideUnderPressure = sideUnderPressure,
            threats = threats,
            position = node.ref,
            line = input.referenceLine.flatMap(line => context.line(line.role).map(_.ref)),
            scope = EvidenceScope.BeforePosition,
            parents = evidenceRefs(context, EvidenceLayer.Board, Some(node.ref), None) ++
              evidenceRefs(context, EvidenceLayer.SinglePosition, Some(node.ref), None) ++
              context.line(LineNodeRole.BestReference).toList.flatMap(lineParents(context, _))
          )
        }
      }
    }
    val branchRecords =
      input.threatBranches.flatMap { branch =>
        val lineNodes = branch.lines.flatMap(lineNodeForNormalized(context, _))
        (for
          node <- context.positions.find(position =>
            position.role == PositionNodeRole.AfterThreat && position.ref.fen == branch.branchFen
          ).toList
          assessment <- node.assessment.toList
          sideUnderPressure <- node.ref.sideToMove.toList
        yield
          val threats =
            ThreatPressureAssessor.analyze(
              fen = branch.branchFen,
              motifs = lineNodes.flatMap(motifsForLineNode(context, _)).distinct,
              multiPv = EvaluationPerspectivePolicy.sideToMovePvLines(sideUnderPressure, branch.lines.map(_.line)),
              positionAssessment = assessment,
              sideToMove = sideUnderPressure
            )
          threatPressureBundle(
            baseId =
              s"threat-pressure:${allocator.key(sideUnderPressure.name)}:branch:${allocator.key(branch.sourceProbeId)}:${allocator.key(branch.probedMoveUci)}",
            allocator = allocator,
            sideUnderPressure = sideUnderPressure,
            threats = threats,
            position = node.ref,
            line = lineNodes.headOption.map(_.ref),
            scope = EvidenceScope.ThreatLine,
            parents = evidenceRefs(context, EvidenceLayer.Board, Some(node.ref), None) ++
              evidenceRefs(context, EvidenceLayer.SinglePosition, Some(node.ref), None) ++
              lineNodes.flatMap(lineParents(context, _))
          )).flatten
      }
    val afterLineRecords =
      context.transitions.flatMap { edge =>
        (for
          line <- lineForTransition(context, edge).toList
          lineFacts <- lineFactEvidence(context, line.ref).toList
          node <- context.positions.find(_.ref == edge.to).toList
          assessment <- node.assessment.toList
          sideUnderPressure <- node.ref.sideToMove.toList
          suffixMoves = lineFacts.lineReplayContinuationMoves
          if suffixMoves.nonEmpty
          continuationPv = PvLine(
            moves = suffixMoves,
            sideRelativeEvalCp = EvaluationPerspectivePolicy.sideToMoveScoreCp(sideUnderPressure, line.whitePovEvalCp),
            mate = EvaluationPerspectivePolicy.sideToMoveMate(sideUnderPressure, line.mate),
            depth = line.depth
          )
          threats = ThreatPressureAssessor.analyze(
            fen = edge.to.fen,
            motifs = motifsForLineRole(context, line.role),
            multiPv = List(continuationPv),
            positionAssessment = assessment,
            sideToMove = sideUnderPressure
          )
          if threats.hasThreat || threats.defenseRequired || threats.prophylaxisNeeded
        yield
          threatPressureBundle(
            baseId = s"threat-pressure:${allocator.key(sideUnderPressure.name)}:${allocator.key(edge.role)}:${edge.moveUci}",
            allocator = allocator,
            sideUnderPressure = sideUnderPressure,
            threats = threats,
            position = node.ref,
            line = Some(line.ref),
            scope = line.role.scope,
            parents = List(edge.evidence) ++
              evidenceRefs(context, EvidenceLayer.Board, Some(node.ref), None) ++
              evidenceRefs(context, EvidenceLayer.SinglePosition, Some(node.ref), None) ++
              lineParents(context, line)
          )).flatten
      }
    beforeRecords ++ branchRecords ++ afterLineRecords

  private def threatPressureBundle(
      baseId: String,
      allocator: JudgmentProvenanceAllocator,
      sideUnderPressure: Color,
      threats: ThreatAnalysis,
      position: PositionNodeRef,
      line: Option[LineNodeRef],
      scope: EvidenceScope,
      parents: List[EvidenceRef]
  ): List[EvidenceRecord] =
    val summary =
      StrategicFactNormalizer.fromThreatPressure(
        id = allocator.evidenceId(baseId),
        sideUnderPressure = sideUnderPressure,
        threats = threats,
        position = position,
        line = line,
        scope = scope,
        parents = parents
      )
    val episodes =
      ThreatEpisode.fromAnalysis(sideUnderPressure, threats).map { episode =>
        StrategicFactNormalizer.fromThreatEpisode(
          id = allocator.evidenceId(
            s"$baseId:episode:${episode.sourceThreatIndex}:${allocator.key(episode.kind.toString)}:${allocator.key(episode.evidenceSource.toString)}"
          ),
          episode = episode,
          summary = threats,
          position = position,
          line = line,
          scope = scope,
          parents = (summary.ref :: parents).distinctBy(_.id)
        )
    }
    summary :: episodes

  private def tacticalMechanismRecords(
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator
  ): List[EvidenceRecord] =
    val lineMechanisms =
      context.lines.flatMap(line => tacticalMechanismRecordsForLine(context, allocator, line))
    val unlinedTransitionMechanisms =
      context.transitions
        .filter(edge => lineForTransition(context, edge).isEmpty)
        .flatMap(edge => tacticalMechanismRecordsForTransition(context, allocator, edge))
    (lineMechanisms ++ unlinedTransitionMechanisms).distinctBy(_.ref.id)

  private def tacticalMechanismRecordsForLine(
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator,
      line: CandidateLineNode
  ): List[EvidenceRecord] =
    val lineRecords = context.evidenceGraph.recordsFor(line.ref)
    val position =
      lineRecords.headOption.map(_.ref.position).orElse(context.root)
    val records =
      position match
        case Some(nodeRef) if line.role == LineNodeRole.Threat =>
          (lineRecords ++ context.evidenceGraph.recordsFor(nodeRef).collect {
            case record @ EvidenceRecord(ref, _: ThreatEpisodeEvidence, _) if ref.scope == EvidenceScope.ThreatLine =>
              record
          }).distinctBy(_.ref.id)
        case _ =>
          lineRecords
    position.toList.flatMap { nodeRef =>
      tacticalMechanismCandidates(records).flatMap { candidate =>
        tacticalMechanismRecord(
          id = allocator.evidenceId(
            s"tactical-mechanism:line:${allocator.key(line.role)}:${line.ref.rank}:${line.ref.rootMove}:${allocator.key(candidate.kind)}"
          ),
          kind = candidate.kind,
          position = nodeRef,
          line = Some(line.ref),
          moveUci = Some(line.ref.rootMove),
          scope = line.role.scope,
          records = candidate.records,
          signals = candidate.signals
        )
      }
    }

  private def tacticalMechanismRecordsForTransition(
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator,
      edge: MoveTransitionEdge
  ): List[EvidenceRecord] =
    val records =
      context.evidenceGraph.records.filter(record =>
        record.ref.scope == edge.role.scope &&
          record.ref.position == edge.from &&
          transitionRecordMentionsMove(record, edge.moveUci)
      )
    tacticalMechanismCandidates(records).flatMap { candidate =>
      tacticalMechanismRecord(
        id = allocator.evidenceId(s"tactical-mechanism:transition:${allocator.key(edge.role)}:${edge.moveUci}:${allocator.key(candidate.kind)}"),
        kind = candidate.kind,
        position = edge.from,
        line = None,
        moveUci = Some(edge.moveUci),
        scope = edge.role.scope,
        records = candidate.records ++ context.evidenceGraph.byId.get(edge.evidence.id).toList,
        signals = candidate.signals
      )
    }

  private def tacticalMechanismCandidates(records: List[EvidenceRecord]): List[TacticalMechanismCandidate] =
    val baseEntries =
      records.flatMap(record =>
        record.payload match
          case payload: MoveMotifEvidence =>
            TacticalMotifClassifier.rootMotif(payload).toList.flatMap { motif =>
              TacticalMechanismKind.fromMotif(motif).map(kind =>
                TacticalMechanismCandidate(
                  kind,
                  List(record),
                  List(TacticalMechanismSignal(TacticalMechanismSignalKind.Motif, payload.proof.kind, EvidenceLayer.MoveMotif, Some(record.ref)))
                )
              )
            }
          case payload: RelationFactEvidence if payload.hasConcreteRelationProof && payload.hasLineProof =>
            List(
              TacticalMechanismCandidate(
                TacticalMechanismKind.fromRelation(payload.kind),
                List(record),
                List(TacticalMechanismSignal(TacticalMechanismSignalKind.Relation, payload.kind.toString, EvidenceLayer.Relation, Some(record.ref)))
              )
            )
          case payload: LineFactEvidence =>
            payload.tacticalLineConsequenceKinds.flatMap(kind =>
              TacticalMechanismKind.fromLineConsequence(kind).map(mechanismKind =>
                TacticalMechanismCandidate(
                  mechanismKind,
                  List(record),
                  List(TacticalMechanismSignal(TacticalMechanismSignalKind.LineConsequence, kind.toString, EvidenceLayer.Line, Some(record.ref)))
                )
              )
            )
          case EvalFactEvidence(_, _, mate, _) if mate.nonEmpty =>
            List(
              TacticalMechanismCandidate(
                TacticalMechanismKind.KingForcing,
                List(record),
                List(TacticalMechanismSignal(TacticalMechanismSignalKind.MateBranch, mate.map(_.toString).getOrElse("mate"), EvidenceLayer.Eval, Some(record.ref)))
              )
            )
          case _ =>
            Nil
      )
    val relationDrivenConsequenceKinds = Set(
      RelationFactKind.DefenderTrade,
      RelationFactKind.Overload,
      RelationFactKind.Deflection,
      RelationFactKind.DiscoveredAttack,
      RelationFactKind.DoubleCheck,
      RelationFactKind.BackRankMate,
      RelationFactKind.MateNet,
      RelationFactKind.Fork,
      RelationFactKind.Decoy,
      RelationFactKind.Interference,
      RelationFactKind.Clearance,
      RelationFactKind.XRay,
      RelationFactKind.Battery,
      RelationFactKind.Pin,
      RelationFactKind.Skewer,
      RelationFactKind.GreekGift
    )
    val relationRecords = records.collect {
      case record @ EvidenceRecord(_, payload: RelationFactEvidence, _)
          if payload.hasConcreteRelationProof && payload.hasLineProof && relationDrivenConsequenceKinds.contains(payload.kind) =>
        record -> payload
    }
    val lineConsequenceRecords = records.collect { case record @ EvidenceRecord(_, payload: LineFactEvidence, _) =>
      payload.proofSignalConsequences.flatMap { consequence =>
        val consequenceMoves = consequence.eventMove.toList ++ consequence.lineMoves
        val consequenceCaptureSquares =
          payload.materialCaptures
            .filter(capture => consequenceMoves.exists(EvidenceRef.sameMove(_, capture.moveUci)))
            .map(_.square)
        TacticalMechanismKind
          .fromLineConsequence(consequence.kind)
          .filter(kind =>
            kind == TacticalMechanismKind.KingForcing ||
              kind == TacticalMechanismKind.MaterialGain ||
              kind == TacticalMechanismKind.RecaptureChoice
          )
          .map(kind => (record, consequence, kind, consequenceMoves, consequenceCaptureSquares))
      }
    }.flatten
    val mateThreatRecords = records.collect {
      case record @ EvidenceRecord(_, payload: ThreatEpisodeEvidence, _)
          if payload.episode.driver == ThreatDriver.MateThreat &&
            payload.episode.hasConcreteThreatProof &&
            (payload.defenseRequired || payload.episode.immediate) =>
        record -> payload
    }
    val relationConsequenceEntries =
      relationRecords.flatMap { case (relationRecord, relation) =>
        lineConsequenceRecords.collect {
          case (lineRecord, consequence, mechanismKind, consequenceMoves, consequenceCaptureSquares)
              if relationRecord.ref.line.exists(lineRecord.referencesLine) &&
                (
                  consequenceMoves.exists(relation.mentionsLineMove) ||
                    consequenceCaptureSquares.exists(square => relation.targetSquare.contains(square) || relation.focusSquares.contains(square)) ||
                    (mechanismKind == TacticalMechanismKind.KingForcing &&
                      TacticalMechanismKind.fromRelation(relation.kind) == TacticalMechanismKind.KingForcing)
                ) =>
            TacticalMechanismCandidate(
              mechanismKind,
              List(relationRecord, lineRecord),
              List(
                TacticalMechanismSignal(TacticalMechanismSignalKind.Relation, relation.kind.toString, EvidenceLayer.Relation, Some(relationRecord.ref)),
                TacticalMechanismSignal(TacticalMechanismSignalKind.LineConsequence, consequence.kind.toString, EvidenceLayer.Line, Some(lineRecord.ref))
              )
            )
        }
      }
    val mateThreatMaterialEntries =
      mateThreatRecords.flatMap { case (threatRecord, threat) =>
        lineConsequenceRecords.collect {
          case (lineRecord, consequence, TacticalMechanismKind.MaterialGain, consequenceMoves, consequenceCaptureSquares)
              if consequence.kind == LineConsequenceKind.MaterialGain &&
                (
                  threatRecord.ref.line.exists(lineRecord.referencesLine) ||
                    (
                      threatRecord.ref.scope == EvidenceScope.ThreatLine &&
                        lineRecord.ref.scope == EvidenceScope.ThreatLine &&
                        threatRecord.ref.position == lineRecord.ref.position
                    )
                ) &&
                (
                  mateThreatMaterialLinked(threat, consequenceMoves, consequenceCaptureSquares) ||
                    threatRecord.ref.line.exists(threatLine =>
                      threatLine.role == LineNodeRole.Threat &&
                        lineRecord.ref.line.exists(line => line.role == LineNodeRole.Threat && line != threatLine)
                    )
                ) =>
            TacticalMechanismCandidate(
              TacticalMechanismKind.MaterialGain,
              List(threatRecord, lineRecord),
              List(
                TacticalMechanismSignal(TacticalMechanismSignalKind.ThreatEpisode, threat.episode.episodeId, EvidenceLayer.ThreatPressure, Some(threatRecord.ref)),
                TacticalMechanismSignal(TacticalMechanismSignalKind.LineConsequence, consequence.kind.toString, EvidenceLayer.Line, Some(lineRecord.ref))
              )
            )
        }
      }
    val defensiveResourceEntries =
      records.collect { case record @ EvidenceRecord(_, payload: LineFactEvidence, _) =>
        val rootMove = payload.rootMove.map(EvidenceRef.normalizeMove)
        val rootTo = rootMove.filter(_.length >= 4).map(_.slice(2, 4))
        val rootDefenderMove =
          payload.lineEventsOf(LineEventKind.DefenderMove).exists(event =>
            event.plyOffset == 0 &&
              rootMove.exists(root => EvidenceRef.sameMove(root, event.moveUci))
          )
        val rootOwnedRecoveryKinds =
          rootMove
            .map(payload.rootOwnedProofSignalConsequences)
            .getOrElse(Nil)
            .filter(consequence =>
              consequence.kind == LineConsequenceKind.RecaptureSequence ||
                consequence.kind == LineConsequenceKind.RecoveryWindow
            )
        val carriedDefenseRecoveryKinds =
          if payload.lineEventsOf(LineEventKind.DefenderMove).exists(event =>
              event.plyOffset > 0 &&
                rootTo.exists(to => EvidenceRef.normalizeMove(event.moveUci).take(2) == to)
            )
          then
            payload.proofSignalConsequences.filter(consequence =>
              consequence.kind == LineConsequenceKind.RecaptureSequence ||
                consequence.kind == LineConsequenceKind.RecoveryWindow
            )
          else Nil
        val recoveryKinds = (rootOwnedRecoveryKinds ++ carriedDefenseRecoveryKinds).distinct
        Option.when(rootDefenderMove && recoveryKinds.nonEmpty)(
          TacticalMechanismCandidate(
            TacticalMechanismKind.DefensiveResource,
            List(record),
            recoveryKinds.map(consequence =>
              TacticalMechanismSignal(TacticalMechanismSignalKind.LineConsequence, consequence.kind.toString, EvidenceLayer.Line, Some(record.ref))
            )
          )
        )
      }.flatten
    (baseEntries ++ relationConsequenceEntries ++ mateThreatMaterialEntries ++ defensiveResourceEntries)
      .groupBy(_.kind)
      .toList
      .map { case (kind, grouped) =>
        TacticalMechanismCandidate(
          kind = kind,
          records = grouped.flatMap(_.records).distinctBy(_.ref.id),
          signals = grouped.flatMap(_.signals).distinct
        )
      }
      .filter { candidate =>
        val payload = TacticalMechanismEvidence(candidate.kind, None, None, candidate.signals)
        payload.canAnchorTacticalIdea || payload.canAnchorDefensiveIdea
      }

  private def mateThreatMaterialLinked(
      threat: ThreatEpisodeEvidence,
      consequenceMoves: List[String],
      consequenceCaptureSquares: List[EvidenceSquare]
  ): Boolean =
    val attackSquares = threat.episode.attackSquares.toSet
    consequenceCaptureSquares.exists(attackSquares.contains) ||
      threat.episode.bestDefense.exists(defense => consequenceMoves.exists(EvidenceRef.sameMove(defense, _)))

  private def tacticalMechanismRecord(
      id: String,
      kind: TacticalMechanismKind,
      position: PositionNodeRef,
      line: Option[LineNodeRef],
      moveUci: Option[String],
      scope: EvidenceScope,
      records: List[EvidenceRecord],
      signals: List[TacticalMechanismSignal]
  ): Option[EvidenceRecord] =
    val payload = TacticalMechanismEvidence(kind, moveUci, line, signals)
    Option.when(payload.hasConcreteProof) {
      val confidence =
        if signals.exists(_.kind == TacticalMechanismSignalKind.MateBranch) then EvidenceConfidence.EngineBacked
        else if signals.exists(signal => signal.kind == TacticalMechanismSignalKind.Relation || signal.kind == TacticalMechanismSignalKind.LineConsequence)
        then EvidenceConfidence.LegalReplayVerified
        else EvidenceConfidence.Mixed
      EvidenceRecord(
        ref = EvidenceRef(
          id = id,
          producer = EvidenceProducer.TacticalMechanismProducer,
          layer = EvidenceLayer.TacticalMechanism,
          position = position,
          line = line,
          scope = scope,
          confidence = confidence
        ),
        payload = payload,
        parents = records.map(_.ref).distinctBy(_.id)
      )
    }

  private def transitionRecordMentionsMove(record: EvidenceRecord, moveUci: String): Boolean =
    record.payload match
      case payload: MoveMotifEvidence =>
        payload.moveUci == moveUci
      case MoveTransitionEvidence(move, _, _) =>
        move == moveUci
      case payload: RelationFactEvidence =>
        payload.mentionsLineMove(moveUci) || record.ref.scope == EvidenceScope.PlayedTransition
      case _ =>
        false

  private def structuralDeltaRecords(
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator
  ): List[EvidenceRecord] =
    context.transitions.flatMap { edge =>
      for
        fromPosition <- Fen.read(Standard, Fen.Full(edge.from.fen))
        toPosition <- Fen.read(Standard, Fen.Full(edge.to.fen))
        side <- edge.from.sideToMove.orElse(Some(fromPosition.color))
        (files, targets, createdTensionFrom) = moveStructureInputs(edge.moveUci)
        if files.nonEmpty
        delta <- StructuralDeltaAnalyzer.delta(
          beforeFen = edge.from.fen,
          beforeBoard = fromPosition.board,
          afterFen = edge.to.fen,
          afterBoard = toPosition.board,
          side = side,
          files = files,
          targets = targets,
          createdTensionFrom = createdTensionFrom,
          moveUci = Some(edge.moveUci)
        )
        record = TransitionFactNormalizer.fromStructuralDelta(
          id = allocator.evidenceId(s"structural-delta:${allocator.key(edge.role)}:${edge.moveUci}"),
          delta = delta,
          transition = edge,
          line = lineForTransition(context, edge).map(_.ref),
          perspective = side,
          parents = transitionParents(context, edge, lineForTransition(context, edge)) ++
            evidenceRefs(context, EvidenceLayer.Board, Some(edge.to), None)
        )
        if record.payload match
          case payload: StructuralDeltaEvidence => payload.hasTypedOutput
          case _                                => false
      yield record
    }

  private def strategicFeatureRecords(
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator
  ): List[EvidenceRecord] =
    context.positions.flatMap { node =>
      val boardParents = evidenceRefs(context, EvidenceLayer.Board, Some(node.ref), None)
      val boardFacts = boardFactEvidence(context, node.ref)
      val targetAnchors =
        boardFacts.toList.flatMap(_.looseMaterialAnchors)
      val targetFixation =
        Option
        .when(targetAnchors.nonEmpty) {
          StrategicFactNormalizer.fromBoardAnchors(
            id = allocator.evidenceId(
              s"strategic:target-fixation:${allocator.positionKey(node.role, node.ref.fen, node.ref.ply)}"
            ),
            kind = StrategicFactKind.TargetFixation,
            anchors = targetAnchors,
            relatedPlans = Nil,
            confidence = 0.78,
            position = node.ref,
            line = None,
            scope = node.role.scope,
            parents = boardParents
          )
        }
        .toList
      val outpostAnchors =
        boardFacts.toList.flatMap(_.outpostAnchors)
      val outpost =
        Option.when(outpostAnchors.nonEmpty) {
          StrategicFactNormalizer.fromBoardAnchors(
            id = allocator.evidenceId(s"strategic:outpost:${allocator.positionKey(node.role, node.ref.fen, node.ref.ply)}"),
            kind = StrategicFactKind.Outpost,
            anchors = outpostAnchors,
            relatedPlans = Nil,
            confidence = 0.78,
            position = node.ref,
            line = None,
            scope = node.role.scope,
            parents = boardParents
          )
        }.toList
      val endgameAnchors =
        boardFacts.toList.flatMap(_.endgameTechniqueAnchors)
      val endgame =
        Option.when(endgameAnchors.nonEmpty) {
          StrategicFactNormalizer.fromBoardAnchors(
            id = allocator.evidenceId(s"strategic:endgame:${allocator.positionKey(node.role, node.ref.fen, node.ref.ply)}"),
            kind = StrategicFactKind.Endgame,
            anchors = endgameAnchors,
            relatedPlans = Nil,
            confidence = endgameAnchors.map(_.confidence).max,
            position = node.ref,
            line = None,
            scope = node.role.scope,
            parents = boardParents
          )
        }.toList
      val featureRecords =
        boardFacts.toList.flatMap(featureStrategicRecords(node, _, allocator, boardParents))
      targetFixation ++ outpost ++ endgame ++ featureRecords
    }

  private def boardFactEvidence(context: JudgmentAssemblyContext, position: PositionNodeRef): Option[BoardFactEvidence] =
    context.evidenceGraph.records.collectFirst {
      case EvidenceRecord(ref, payload: BoardFactEvidence, _) if ref.position == position =>
        payload
    }

  private def lineFactEvidence(context: JudgmentAssemblyContext, line: LineNodeRef): Option[LineFactEvidence] =
    context.evidenceGraph.records.collectFirst {
      case record @ EvidenceRecord(_, payload: LineFactEvidence, _) if record.carriesLinePayload(line, EvidenceLayer.Line) =>
        payload
    }

  private def featureStrategicRecords(
      node: PositionNode,
      boardFacts: BoardFactEvidence,
      allocator: JudgmentProvenanceAllocator,
      parents: List[EvidenceRef]
  ): List[EvidenceRecord] =
    val nodeKey = allocator.positionKey(node.role, node.ref.fen, node.ref.ply)
    List(
      strategicAnchorRecord(
        id = allocator.evidenceId(s"strategic:file-control:$nodeKey"),
        node = node,
        kind = StrategicFactKind.FileControl,
        anchors = boardFacts.fileControlAnchors,
        parents = parents
      ),
      strategicAnchorRecord(
        id = allocator.evidenceId(s"strategic:space:$nodeKey"),
        node = node,
        kind = StrategicFactKind.Space,
        anchors = boardFacts.spaceAnchors,
        parents = parents
      ),
      strategicAnchorRecord(
        id = allocator.evidenceId(s"strategic:activity:$nodeKey"),
        node = node,
        kind = StrategicFactKind.Activity,
        anchors = boardFacts.activityAnchors,
        parents = parents
      ),
      strategicAnchorRecord(
        id = allocator.evidenceId(s"strategic:counterplay-restraint:$nodeKey"),
        node = node,
        kind = StrategicFactKind.CounterplayRestraint,
        anchors = boardFacts.counterplayRestraintAnchors,
        parents = parents
      )
    ).flatten

  private def strategicAnchorRecord(
      id: String,
      node: PositionNode,
      kind: StrategicFactKind,
      anchors: List[BoardAnchor],
      parents: List[EvidenceRef]
  ): Option[EvidenceRecord] =
    Option.when(anchors.nonEmpty) {
      StrategicFactNormalizer.fromBoardAnchors(
        id = id,
        kind = kind,
        anchors = anchors,
        relatedPlans = Nil,
        confidence = anchors.map(_.confidence).max,
        position = node.ref,
        line = None,
        scope = node.role.scope,
        parents = parents
      )
    }

  private def featureApplicabilityRecords(
      input: NormalizedMoveReviewInput,
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator
  ): List[EvidenceRecord] =
    val root = context.position(PositionNodeRole.Before)
    root.toList.flatMap { rootNode =>
      val openingPhase =
        rootNode.assessment.exists(_.gamePhase.isOpening) ||
          rootNode.features.exists(_.materialPhase.phase == "opening")
      val canAssessOpening =
        openingPhase || input.opening.nonEmpty || input.openingRecognition.nonEmpty
      val signals =
        input.openingSignals ++ List(
          Option.when(openingPhase)(OpeningContextSignal.OpeningPhase)
        ).flatten
      val contextRecord = Option.when(
        signals.nonEmpty ||
          input.opening.nonEmpty ||
          input.openingRecognition.nonEmpty ||
          input.openingThemePriorSelection.nonEmpty ||
          canAssessOpening
      ) {
        OpeningContextFactNormalizer.fromContext(
          id = allocator.evidenceId("opening-context:before"),
          identity = input.opening,
          signals = signals,
          recognition = input.openingRecognition,
          themePriorSelection = input.openingThemePriorSelection,
          position = rootNode.ref,
          line = None,
          scope = EvidenceScope.BeforePosition,
          confidence = openingContextConfidence(input.opening, input.openingRecognition, openingPhase),
          parents = openingContextParents(context, rootNode.ref)
        )
      }
      val anchorRecords = featureAnchorRecords(context, rootNode, allocator)
        .filter(record => StrategicMechanismEvidence.sourceMechanisms(record).nonEmpty)
      val anchors = anchorRecords.collect { case EvidenceRecord(_, FeatureAnchorEvidence(anchor), _) => anchor }
      val openingContextEvidence =
        contextRecord.collect { case EvidenceRecord(_, payload: OpeningContextEvidence, _) => payload }
      val applicabilityRecord =
        for
          assessment <- assessApplicability(openingContextEvidence, input.beforePly, rootNode, anchors)
          if assessment.canCertifyOpeningClaim
        yield
          applicabilityAssessmentRecord(
            id = allocator.evidenceId("applicability:before"),
            assessment = assessment,
            position = rootNode.ref,
            line = None,
            scope = EvidenceScope.BeforePosition,
            confidence = applicabilityConfidence(assessment),
            parents = (contextRecord.map(_.ref).toList ++ anchorRecords.map(_.ref)).distinctBy(_.id)
          )
      contextRecord.filter(_ => applicabilityRecord.nonEmpty).toList ++ anchorRecords ++ applicabilityRecord.toList
    }

  private def strategicMechanismRecords(
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator
  ): List[EvidenceRecord] =
    val candidates =
      context.evidenceGraph.records.flatMap { record =>
        StrategicMechanismEvidence.sourceMechanisms(record).map { case (kind, signal) =>
          StrategicMechanismCandidate(kind, record.ref.position, record.ref.line, record.ref.scope, signal, record)
        }
      }
    candidates
      .groupBy(candidate => (candidate.kind, candidate.position, candidate.line, candidate.scope))
      .toList
      .flatMap { case ((kind, position, line, scope), grouped) =>
        val routeRecords =
          if kind != StrategicMechanismKind.Activity && kind != StrategicMechanismKind.Endgame then Nil
          else
            context.transitions
              .filter(_.to == position)
              .flatMap(edge =>
                context.evidenceGraph.records.collect {
                  case record @ EvidenceRecord(ref, payload: MoveMotifEvidence, _)
                      if ref.position == edge.from &&
                        ref.scope == edge.role.scope &&
                        transitionRecordMentionsMove(record, edge.moveUci) &&
                        payload.isRootEvent &&
                        (payload.motif.isInstanceOf[Motif.KingStep] || payload.motif.isInstanceOf[Motif.Castling]) =>
                    record
                }
              )
              .distinctBy(_.ref.id)
        val routeSignals =
          routeRecords.map(record =>
            StrategicMechanismSignal(
              StrategicMechanismSignalKind.StrategicFact,
              "current-move-route",
              record.ref,
              1,
              Some(StrategicAxisDetail(StrategicAxisKind.Activity, StrategicAxisPolarity.Support, "current-move-route"))
            )
          )
        val sourceRecords = (grouped.map(_.source) ++ routeRecords).distinctBy(_.ref.id)
        val signals = (grouped.map(_.signal) ++ routeSignals).distinct
        val semanticAnchors =
          (
            EvidenceSemanticAnchor.of(EvidenceSemanticAnchorKind.StrategicMechanism, kind.toString) ::
              sourceRecords.flatMap(StrategicMechanismEvidence.sourceSemanticAnchors)
          ).distinctBy(_.stableKey)
        Option.when(signals.nonEmpty) {
          val payload = StrategicMechanismEvidence(kind, signals, semanticAnchors)
          val positionKey =
            position.id.map(allocator.key).getOrElse(s"${position.ply}:${Integer.toHexString(position.fen.hashCode)}")
          EvidenceRecord(
            ref = EvidenceRef(
              id = allocator.evidenceId(
                s"strategic-mechanism:${allocator.key(kind)}:$positionKey:${line.map(line => allocator.key(line.rootMove)).getOrElse("position")}:${allocator.key(scope)}"
              ),
              producer = EvidenceProducer.StrategicMechanismProducer,
              layer = EvidenceLayer.StrategicMechanism,
              position = position,
              line = line,
              scope = scope,
              confidence = strategicMechanismConfidence(sourceRecords)
            ),
            payload = payload,
            parents = sourceRecords.map(_.ref).distinctBy(_.id)
          )
        }
      }

  private final case class StrategicMechanismCandidate(
      kind: StrategicMechanismKind,
      position: PositionNodeRef,
      line: Option[LineNodeRef],
      scope: EvidenceScope,
      signal: StrategicMechanismSignal,
      source: EvidenceRecord
  )

  private def strategicMechanismConfidence(records: List[EvidenceRecord]): EvidenceConfidence =
    if records.exists(_.ref.confidence == EvidenceConfidence.EngineBacked) then EvidenceConfidence.Mixed
    else if records.forall(_.ref.confidence == EvidenceConfidence.BoardDerived) then EvidenceConfidence.BoardDerived
    else EvidenceConfidence.Mixed

  private def openingContextParents(
      context: JudgmentAssemblyContext,
      position: PositionNodeRef
  ): List[EvidenceRef] =
    val base =
      List(
        EvidenceLayer.Board,
        EvidenceLayer.SinglePosition,
        EvidenceLayer.MoveMotif
      ).flatMap(layer => evidenceRefs(context, layer, Some(position), None))
    val pawnStructure =
      context.evidenceGraph.records.collect {
        case EvidenceRecord(ref, PawnStructureFactEvidence(profile, _, pawnPlay), _)
            if ref.position == position &&
              (profile.primary != lila.chessjudgment.model.structure.StructureId.Unknown ||
                pawnPlay.exists(_.primaryDriver != PawnPlayDriver.Quiet)) =>
          ref
      }
    (base ++ pawnStructure).distinctBy(_.id)

  private def featureAnchorRecords(
      context: JudgmentAssemblyContext,
      node: PositionNode,
      allocator: JudgmentProvenanceAllocator
  ): List[EvidenceRecord] =
    val boardParents =
      evidenceRefs(context, EvidenceLayer.Board, Some(node.ref), None) ++
        evidenceRefs(context, EvidenceLayer.SinglePosition, Some(node.ref), None)
    val featureAnchors =
      boardFactEvidence(context, node.ref).toList.flatMap { boardFacts =>
        boardOpeningFeatureAnchors(boardFacts).map(anchor => anchor -> boardParents)
      }
    val evidenceAnchors =
      context.evidenceGraph.records
        .filter(_.ref.position == node.ref)
        .flatMap(evidenceFeatureAnchors)
    (featureAnchors ++ evidenceAnchors)
      .distinctBy { case (anchor, parents) =>
        (anchor.theme, anchor.signal, anchor.sourceLayer, parents.map(_.id).sorted.mkString("|"))
      }
      .map { case (anchor, parents) =>
        featureAnchorRecord(
          id = allocator.evidenceId(
            s"feature-anchor:${allocator.key(anchor.theme)}:${allocator.key(anchor.sourceLayer)}:${allocator.key(anchor.signal)}:${parents.headOption.map(parent => allocator.key(parent.id)).getOrElse("root")}"
          ),
          anchor = anchor,
          position = node.ref,
          line = None,
          scope = EvidenceScope.BeforePosition,
          confidence = if anchor.isBoardObservation then EvidenceConfidence.BoardDerived else EvidenceConfidence.Mixed,
          parents = parents.distinctBy(_.id)
        )
      }

  private def openingContextConfidence(
      identity: Option[OpeningIdentity],
      recognition: Option[OpeningRecognition],
      openingPhase: Boolean
  ): EvidenceConfidence =
    if (identity.nonEmpty || recognition.nonEmpty) && openingPhase then EvidenceConfidence.Mixed
    else if openingPhase || identity.nonEmpty || recognition.nonEmpty then EvidenceConfidence.BoardDerived
    else EvidenceConfidence.Heuristic

  private def featureAnchorRecord(
      id: String,
      anchor: FeatureAnchor,
      position: PositionNodeRef,
      line: Option[LineNodeRef],
      scope: EvidenceScope,
      confidence: EvidenceConfidence,
      parents: List[EvidenceRef]
  ): EvidenceRecord =
    val ref =
      EvidenceRef(
        id = id,
        producer = EvidenceProducer.FeatureAnchorProducer,
        layer = EvidenceLayer.FeatureAnchor,
        position = position,
        line = line,
        scope = scope,
        confidence = confidence
      )
    EvidenceRecord(
      ref = ref,
      payload = FeatureAnchorEvidence(anchor),
      parents = parents
    )

  private def applicabilityAssessmentRecord(
      id: String,
      assessment: ApplicabilityAssessment,
      position: PositionNodeRef,
      line: Option[LineNodeRef],
      scope: EvidenceScope,
      confidence: EvidenceConfidence,
      parents: List[EvidenceRef]
  ): EvidenceRecord =
    val ref =
      EvidenceRef(
        id = id,
        producer = EvidenceProducer.ApplicabilityAssessmentProducer,
        layer = EvidenceLayer.ApplicabilityAssessment,
        position = position,
        line = line,
        scope = scope,
        confidence = confidence
      )
    EvidenceRecord(
      ref = ref,
      payload = ApplicabilityAssessmentEvidence(assessment),
      parents = parents
    )

  private def assessApplicability(
      contextEvidence: Option[OpeningContextEvidence],
      beforePly: Int,
      node: PositionNode,
      anchors: List[FeatureAnchor]
  ): Option[ApplicabilityAssessment] =
    val observedThemes = anchors.map(_.theme).distinct
    Option.when(observedThemes.nonEmpty) {
      val priorSelections = contextEvidence.flatMap(_.themePriorSelection).toList
      val priorThemes = priorSelections.flatMap(_.prior.themes).distinct
      val supported = priorThemes.filter(theme =>
        anchors.exists(anchor => anchor.theme == theme && proofSignalOpeningAnchor(anchor))
      )
      val unverified = priorThemes.filterNot(supported.contains)
      val observedOnly = observedThemes.filterNot(priorThemes.contains)
      val ambiguousRecognition = contextEvidence.flatMap(_.recognition).exists(_.candidates.drop(1).nonEmpty)
      val applicability = featureApplicability(contextEvidence, beforePly, node, anchors, supported)
      val status =
        if applicability == FeatureApplicability.Contraindicated then ApplicabilityStatus.Contradicted
        else if priorThemes.isEmpty then ApplicabilityStatus.InternalOnly
        else if ambiguousRecognition && supported.isEmpty then ApplicabilityStatus.Ambiguous
        else if supported.nonEmpty && unverified.nonEmpty then ApplicabilityStatus.PartiallySupported
        else if supported.nonEmpty then ApplicabilityStatus.Supported
        else ApplicabilityStatus.Unverified
      ApplicabilityAssessment(
        applicability = applicability,
        status = status,
        observedThemes = observedThemes,
        supportedThemes = supported,
        unverifiedPriorThemes = unverified,
        observedOnlyThemes = observedOnly,
        priorMatchSources = priorSelections.map(_.matchSource).distinct
      )
    }

  private def proofSignalOpeningAnchor(anchor: FeatureAnchor): Boolean =
    anchor.canCorroborateOpeningPrior

  private def featureApplicability(
      contextEvidence: Option[OpeningContextEvidence],
      beforePly: Int,
      node: PositionNode,
      anchors: List[FeatureAnchor],
      supportedThemes: List[OpeningTheme]
  ): FeatureApplicability =
    val phase = node.assessment.map(_.gamePhase)
    val featurePhase = node.features.map(_.materialPhase.phase)
    val openingPhase = phase.exists(_.isOpening) || featurePhase.contains("opening")
    val openingWindow = beforePly <= OpeningRelevanceMaxPly
    val middlegamePhase = phase.exists(_.isMiddlegame) || featurePhase.contains("middlegame")
    val endgamePhase = phase.exists(_.isEndgame) || featurePhase.contains("endgame")
    val hasOpeningContext =
      contextEvidence.exists(context =>
        context.identity.nonEmpty ||
          context.recognition.nonEmpty ||
          context.themePriorSelection.nonEmpty ||
          context.signals.exists(signal =>
            signal == OpeningContextSignal.InputIdentity ||
              signal == OpeningContextSignal.RecognizedIdentity ||
              signal == OpeningContextSignal.ThemePrior
          )
      )
    val denseMaterial =
      phase.exists(result => result.queensOnBoard || result.minorPiecesCount >= 4) ||
        node.features.exists(features => features.materialPhase.whiteMaterial + features.materialPhase.blackMaterial >= 48)
    val openingSensitiveTheme =
      anchors.exists(anchor =>
        anchor.theme == OpeningTheme.Development ||
          anchor.theme == OpeningTheme.GambitInitiative ||
          anchor.theme == OpeningTheme.KingSafety ||
          anchor.theme == OpeningTheme.CenterControl
      )
    val themePriorSupportedByInternalAnchor = supportedThemes.nonEmpty
    if endgamePhase && !openingWindow then FeatureApplicability.EndgameRelevant
    else if openingPhase && !openingWindow && themePriorSupportedByInternalAnchor then FeatureApplicability.MiddlegameRelevant
    else if endgamePhase && openingSensitiveTheme && !themePriorSupportedByInternalAnchor then FeatureApplicability.Contraindicated
    else if openingWindow && denseMaterial && openingSensitiveTheme && themePriorSupportedByInternalAnchor then
      FeatureApplicability.OpeningRelevant
    else if openingWindow && themePriorSupportedByInternalAnchor then FeatureApplicability.OpeningRelevant
    else if endgamePhase then FeatureApplicability.EndgameRelevant
    else if hasOpeningContext || middlegamePhase || anchors.exists(anchor => anchor.sourceLayer == EvidenceLayer.PlanPressure || anchor.sourceLayer == EvidenceLayer.Strategic) then
      FeatureApplicability.MiddlegameRelevant
    else FeatureApplicability.ObservedOnly

  private def applicabilityConfidence(assessment: ApplicabilityAssessment): EvidenceConfidence =
    assessment.status match
      case ApplicabilityStatus.Supported | ApplicabilityStatus.PartiallySupported =>
        EvidenceConfidence.Mixed
      case ApplicabilityStatus.InternalOnly =>
        EvidenceConfidence.BoardDerived
      case ApplicabilityStatus.Unverified | ApplicabilityStatus.Ambiguous |
          ApplicabilityStatus.Contradicted =>
        EvidenceConfidence.Heuristic

  private def boardOpeningFeatureAnchors(boardFacts: BoardFactEvidence): List[FeatureAnchor] =
    boardFacts
      .openingContextAnchors
      .flatMap(boardOpeningFeatureAnchor)
      .distinctBy(anchor => (anchor.theme, anchor.signal, anchor.sourceLayer))

  private def boardOpeningFeatureAnchor(anchor: BoardAnchor): Option[FeatureAnchor] =
    anchor.kind match
      case BoardAnchorKind.CenterControl | BoardAnchorKind.Space =>
        Some(FeatureAnchor(OpeningTheme.CenterControl, FeatureAnchorSignal.CenterControlObserved, EvidenceLayer.Board, anchor.confidence.max(0.6)))
      case BoardAnchorKind.Development | BoardAnchorKind.Activity =>
        Some(FeatureAnchor(OpeningTheme.Development, FeatureAnchorSignal.DevelopmentTempoObserved, EvidenceLayer.Board, anchor.confidence.max(0.55)))
      case BoardAnchorKind.PawnStructure =>
        Some(FeatureAnchor(OpeningTheme.PawnStructure, FeatureAnchorSignal.PawnStructureObserved, EvidenceLayer.Board, anchor.confidence.max(0.55)))
      case BoardAnchorKind.KingSafety =>
        Some(FeatureAnchor(OpeningTheme.KingSafety, FeatureAnchorSignal.KingSafetyObserved, EvidenceLayer.Board, anchor.confidence.max(0.5)))
      case BoardAnchorKind.FileControl | BoardAnchorKind.BatteryPressure | BoardAnchorKind.WeakSquare =>
        Some(FeatureAnchor(OpeningTheme.PlanPressure, FeatureAnchorSignal.LinePressureObserved, EvidenceLayer.Board, anchor.confidence.max(0.5)))
      case BoardAnchorKind.CounterplayRestraint | BoardAnchorKind.LooseMaterial |
          BoardAnchorKind.PinPressure | BoardAnchorKind.SkewerPressure | BoardAnchorKind.ForkPressure |
          BoardAnchorKind.XRayPressure | BoardAnchorKind.Outpost | BoardAnchorKind.EndgameTechnique =>
        None

  private def evidenceFeatureAnchors(record: EvidenceRecord): List[(FeatureAnchor, List[EvidenceRef])] =
    record.payload match
      case PawnStructureFactEvidence(profile, alignment, pawnPlay)
          if profile.primary != lila.chessjudgment.model.structure.StructureId.Unknown ||
            pawnPlay.exists(_.primaryDriver != PawnPlayDriver.Quiet) ||
            alignment.nonEmpty =>
        val signal =
          pawnPlay match
            case Some(play) if play.pawnBreakReady || play.primaryDriver == PawnPlayDriver.BreakReady =>
              FeatureAnchorSignal.PawnBreakObserved
            case Some(play)
                if play.advanceOrCapture ||
                  play.primaryDriver == PawnPlayDriver.TensionCritical ||
                  play.primaryDriver == PawnPlayDriver.TensionActive =>
              FeatureAnchorSignal.CentralTensionObserved
            case _ =>
              FeatureAnchorSignal.PawnStructureObserved
        List(
          FeatureAnchor(
            OpeningTheme.PawnStructure,
            signal,
            EvidenceLayer.PawnStructure,
            profile.confidence.max(0.6)
          ) -> (record.ref :: record.parents)
        )
      case PlanPressureEvidence(_, activePlans)
          if activePlans.primary.plan.category == PlanCategory.Opening ||
            activePlans.secondary.exists(_.plan.category == PlanCategory.Opening) ||
            activePlans.compatibilityEvents.exists(_.adjustment == CompatibilityAdjustment.OpeningPhase) =>
        List(
          FeatureAnchor(
            OpeningTheme.PlanPressure,
            FeatureAnchorSignal.PlanPressureObserved,
            EvidenceLayer.PlanPressure,
            activePlans.primary.score.max(0.6)
          ) -> (record.ref :: record.parents)
        )
      case payload: StructuralDeltaEvidence if payload.hasTypedOutput =>
        val hasOpeningLinePressure =
          payload.hasAnyConsequence(
            Set(
              TransitionConsequenceKind.LineUnlockGain,
              TransitionConsequenceKind.FileAccessGain,
              TransitionConsequenceKind.RookLiftActivation,
              TransitionConsequenceKind.BatteryPressureGain,
              TransitionConsequenceKind.KingRingPressureGain,
              TransitionConsequenceKind.OutpostGain
            )
          )
        val center =
          Option.when(
            payload.hasConsequenceCategory(TransitionConsequenceCategory.OpeningCenterControl)
          )(
            FeatureAnchor(
              OpeningTheme.CenterControl,
              FeatureAnchorSignal.StructuralDeltaObserved,
              EvidenceLayer.StructuralDelta,
              0.65
            )
          )
        val pressure =
          Option.when(payload.hasTargetPressureGain || hasOpeningLinePressure)(
            FeatureAnchor(
              OpeningTheme.PlanPressure,
              if hasOpeningLinePressure then FeatureAnchorSignal.LinePressureObserved
              else FeatureAnchorSignal.StructuralDeltaObserved,
              EvidenceLayer.StructuralDelta,
              if payload.hasTargetPressureGain then 0.66 else 0.62
            )
        )
        val structure =
          val hasPawnStructureAnchor =
            payload.hasConsequenceCategory(TransitionConsequenceCategory.PawnStructure) ||
              payload.hasConsequence(TransitionConsequenceKind.PawnTensionResolution)
          val signal =
            if payload.hasAnyConsequence(
                Set(
                  TransitionConsequenceKind.PawnTensionGain,
                  TransitionConsequenceKind.PawnTensionResolution
                )
              )
            then FeatureAnchorSignal.CentralTensionObserved
            else if payload.hasAnyConsequence(
                Set(
                  TransitionConsequenceKind.OpenFileGain,
                  TransitionConsequenceKind.SemiOpenFileGain,
                  TransitionConsequenceKind.FileOccupationGain
                )
              )
            then FeatureAnchorSignal.LinePressureObserved
            else FeatureAnchorSignal.PawnStructureObserved
          Option.when(hasPawnStructureAnchor)(
            FeatureAnchor(
              OpeningTheme.PawnStructure,
              signal,
              EvidenceLayer.StructuralDelta,
              0.65
            )
          )
        val development =
          Option.when(payload.hasConsequenceCategory(TransitionConsequenceCategory.OpeningDevelopment))(
            FeatureAnchor(
              OpeningTheme.Development,
              if payload.hasConsequence(TransitionConsequenceKind.DevelopmentLagReduced) then
                FeatureAnchorSignal.DevelopmentLagObserved
              else FeatureAnchorSignal.DevelopmentTempoObserved,
              EvidenceLayer.StructuralDelta,
              0.62
            )
          )
        val kingSafety =
          Option.when(payload.hasKingSafetyPressure || payload.hasConsequence(TransitionConsequenceKind.KingRingPressureGain))(
            FeatureAnchor(
              OpeningTheme.KingSafety,
              FeatureAnchorSignal.StructuralDeltaObserved,
              EvidenceLayer.StructuralDelta,
              0.66
            )
          )
        List(center, pressure, structure, development, kingSafety).flatten.map(_ -> (record.ref :: record.parents))
      case payload @ StrategicFactEvidence(kind, facts, relatedPlans, confidence)
          if payload.hasTypedSupport && (facts.nonEmpty || relatedPlans.nonEmpty) =>
        val theme =
          kind match
            case StrategicFactKind.Space        => Some(OpeningTheme.CenterControl)
            case StrategicFactKind.Activity     => Some(OpeningTheme.Development)
            case StrategicFactKind.Compensation => Some(OpeningTheme.GambitInitiative)
            case StrategicFactKind.Structure    => Some(OpeningTheme.PawnStructure)
            case StrategicFactKind.PlanPressure => Some(OpeningTheme.PlanPressure)
            case _                              => None
        theme
          .map(openingTheme =>
            FeatureAnchor(
              openingTheme,
              FeatureAnchorSignal.PlanPressureObserved,
              EvidenceLayer.Strategic,
              confidence.max(0.55)
            ) -> (record.ref :: record.parents)
          )
          .toList
      case _ => Nil

  private def planPressureRecords(
      input: NormalizedMoveReviewInput,
      context: JudgmentAssemblyContext,
      allocator: JudgmentProvenanceAllocator
  ): List[EvidenceRecord] =
    context.position(PositionNodeRole.Before).toList.flatMap { node =>
      val records = for
        boardProfile <- boardFactEvidence(context, node.ref).flatMap(_.boardProfile)
        assessment <- node.assessment
        side <- node.ref.sideToMove.orElse(input.sideToMove)
        initialPosition <- Fen.read(Standard, Fen.Full(node.ref.fen))
      yield
        val motifs = motifsForLineRole(context, LineNodeRole.BestReference)
        val pawnStructureRecord = pawnStructureRecordFor(context, node.ref)
        val pawnStructure = pawnStructureRecord.map(_._2)
        val planContext =
          PlanInteractionContext(
            whitePovEvalCp = input.currentWhitePovEvalCp,
            positionAssessment = Some(assessment),
            pawnAnalysis = pawnStructure.flatMap(_.pawnPlay),
            threatEpisodesToUs = threatEpisodes(context, node.ref, side),
            threatEpisodesToThem = threatEpisodes(context, node.ref, !side),
            isWhiteToMove = side.white,
            positionKey = Some(node.ref.fen),
            boardProfile = Some(boardProfile),
            initialPos = Some(initialPosition),
            structureProfile = pawnStructure.map(_.profile),
            planAlignment = pawnStructure.flatMap(_.alignment)
        )
        val scoring = PlanMatcher.matchPlans(motifs, planContext, side)
        val alignment =
          for
            pawn <- pawnStructure
            entry <- StructuralPlaybook.lookup(pawn.profile.primary)
          yield
            PlanAlignmentScorer.score(
              structureProfile = pawn.profile,
              playbookEntry = entry,
              topPlans = scoring.topPlans,
              motifs = motifs,
              pawnAnalysis = pawn.pawnPlay,
              sideToMove = side
            )
        val alignedPlanContext = planContext.copy(planAlignment = alignment)
        PlanMatcher.toActivePlans(scoring.topPlans, scoring.compatibilityEvents).toList.flatMap { activePlans =>
          val planPressure =
            StrategicFactNormalizer.fromPlanPressure(
              id = allocator.evidenceId("plan-pressure:before"),
              scoring = scoring,
              activePlans = activePlans,
              position = node.ref,
              line = context.line(LineNodeRole.BestReference).map(_.ref),
              scope = EvidenceScope.BeforePosition,
              parents = evidenceRefs(context, EvidenceLayer.Board, Some(node.ref), None) ++
                evidenceRefs(context, EvidenceLayer.SinglePosition, Some(node.ref), None) ++
                evidenceRefs(context, EvidenceLayer.PawnStructure, Some(node.ref), None)
            )
          val alignedPawnStructure =
            for
              (original, _) <- pawnStructureRecord
              pawn <- pawnStructure
              planAlignment <- alignment
            yield
              StrategicFactNormalizer.fromPawnStructure(
                id = original.ref.id,
                profile = pawn.profile,
                alignment = Some(planAlignment),
                pawnPlay = pawn.pawnPlay,
                position = node.ref,
                scope = original.ref.scope,
                parents = (original.parents :+ planPressure.ref).distinctBy(_.id)
              )
          val transition = TransitionAnalyzer.analyze(activePlans, None, alignedPlanContext)
          val planTransition =
            Option.when(transition.transitionType != TransitionType.Opening) {
              TransitionFactNormalizer.fromPlanTransition(
                id = allocator.evidenceId("plan-transition:before"),
                transition = transition,
                position = node.ref,
                line = context.line(LineNodeRole.BestReference).map(_.ref),
                scope = EvidenceScope.BeforePosition,
                parents = planPressure.ref :: planPressure.parents
              )
            }
          List(planPressure) ++ alignedPawnStructure.toList ++ planTransition.toList
        }
      records.getOrElse(Nil)
    }

  private def motifsForLineRole(context: JudgmentAssemblyContext, role: LineNodeRole): List[Motif] =
    context.line(role).map(_.ref).flatMap { lineRef =>
      Some(context.evidenceGraph.records.collect {
        case EvidenceRecord(ref, payload: MoveMotifEvidence, _) if ref.line.contains(lineRef) => payload.motif
      })
    }.getOrElse(Nil)

  private def pawnPlayMotifsForPosition(context: JudgmentAssemblyContext, role: PositionNodeRole): List[Motif] =
    role match
      case PositionNodeRole.AfterPlayed =>
        motifsForLineRole(context, LineNodeRole.Played)
      case PositionNodeRole.AfterReference =>
        motifsForLineRole(context, LineNodeRole.BestReference)
      case PositionNodeRole.AfterAlternative =>
        motifsForLineRole(context, LineNodeRole.Alternative)
      case PositionNodeRole.AfterThreat =>
        motifsForLineRole(context, LineNodeRole.Threat)
      case PositionNodeRole.Before =>
        motifsForLineRole(context, LineNodeRole.BestReference)

  private def motifsForLineNode(context: JudgmentAssemblyContext, line: CandidateLineNode): List[Motif] =
    context.evidenceGraph.records.collect {
      case EvidenceRecord(ref, payload: MoveMotifEvidence, _) if ref.line.contains(line.ref) => payload.motif
    }

  private def startPositionForLine(
      input: NormalizedMoveReviewInput,
      context: JudgmentAssemblyContext,
      root: PositionNode,
      line: CandidateLineNode
  ): PositionNode =
    if line.role != LineNodeRole.Threat then root
    else
      input.threatBranches
        .find(branch => branch.lines.exists(normalizedLineMatches(_, line)))
        .flatMap(branch =>
          context.positions.find(position =>
            position.role == PositionNodeRole.AfterThreat && position.ref.fen == branch.branchFen
          )
        )
        .getOrElse(root)

  private def lineNodeForNormalized(
      context: JudgmentAssemblyContext,
      normalized: NormalizedCandidateLine
  ): Option[CandidateLineNode] =
    context.lines.find(line => normalizedLineMatches(normalized, line))

  private def normalizedLineMatches(normalized: NormalizedCandidateLine, line: CandidateLineNode): Boolean =
    normalized.role == line.role &&
      normalized.rank == line.ref.rank &&
      normalized.rootMove.contains(line.ref.rootMove)

  private def moveStructureInputs(moveUci: String): (List[Char], List[String], Option[String]) =
    val normalized = MoveReviewInputNormalizer.normalizeUci(moveUci)
    val destination = normalized.slice(2, 4)
    val files = ('a' to 'h').toList
    val createdTensionFrom = Option.when(destination.matches("[a-h][1-8]"))(destination)
    (files, Nil, createdTensionFrom)

  private def pawnStructureRecordFor(
      context: JudgmentAssemblyContext,
      position: PositionNodeRef
  ): Option[(EvidenceRecord, PawnStructureFactEvidence)] =
    context.evidenceGraph.records.collectFirst {
      case record @ EvidenceRecord(ref, payload: PawnStructureFactEvidence, _) if ref.position == position =>
        record -> payload
    }

  private def threatEpisodes(
      context: JudgmentAssemblyContext,
      position: PositionNodeRef,
      sideUnderPressure: Color
  ): List[ThreatEpisode] =
    context.evidenceGraph.records.collect {
      case EvidenceRecord(ref, payload: ThreatEpisodeEvidence, _)
          if ref.position == position && payload.sideUnderPressure == sideUnderPressure =>
        payload.episode
    }

  private def transitionParents(
      context: JudgmentAssemblyContext,
      edge: MoveTransitionEdge,
      line: Option[CandidateLineNode]
  ): List[EvidenceRef] =
    List(edge.evidence) ++
      line.toList.flatMap(lineParents(context, _)) ++
      evidenceRefs(context, EvidenceLayer.Board, Some(edge.from), None)

  private def lineParents(
      context: JudgmentAssemblyContext,
      line: CandidateLineNode
  ): List[EvidenceRef] =
    evidenceRefs(context, EvidenceLayer.Line, None, Some(line.ref)) ++
      evidenceRefs(context, EvidenceLayer.Eval, None, Some(line.ref))

  private def evidenceRefs(
      context: JudgmentAssemblyContext,
      layer: EvidenceLayer,
      position: Option[PositionNodeRef],
      line: Option[LineNodeRef]
  ): List[EvidenceRef] =
    context.evidenceGraph.records.collect {
      case record
          if record.payload.layer == layer &&
            position.forall(_ == record.ref.position) &&
            line.forall(record.ref.line.contains) =>
        record.ref
    }

  private def lineForTransition(
      context: JudgmentAssemblyContext,
      edge: MoveTransitionEdge
  ): Option[CandidateLineNode] =
    context.lines.find(line => line.role == edge.role.lineRole && line.ref.rootMove == edge.moveUci)
