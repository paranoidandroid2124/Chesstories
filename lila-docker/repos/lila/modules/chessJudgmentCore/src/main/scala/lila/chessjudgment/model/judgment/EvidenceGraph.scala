package lila.chessjudgment.model.judgment

import chess.*
import lila.chessjudgment.analysis.evaluation.{ JudgmentThresholds, PerspectiveMath }
import lila.chessjudgment.analysis.position.PositionFeatures
import lila.chessjudgment.analysis.singlePosition.{
  PawnPlayAnalysis,
  PawnPlayDriver,
  SinglePositionAssessment,
  Threat,
  ThreatAnalysis,
  ThreatDriver,
  ThreatEvidenceSource,
  ThreatKind,
  TensionPolicy,
  ThreatSeverity
}
import lila.chessjudgment.model.{ ActivePlans, Fact, Motif, MotifCategory, PlanScoringResult, PlanSequenceSummary, TransitionType }
import lila.chessjudgment.model.structure.{ AlignmentBand, PlanAlignment, StructureId, StructureProfile }

final case class EvidenceSquare(key: String)
final case class EvidenceFile(key: String)
final case class EvidencePieceRole(name: String)

enum EvidenceSemanticAnchorKind:
  case StrategicKind
  case StrategicMechanism
  case StrategicAxis
  case Plan
  case BoardAnchor
  case PawnStructure
  case StructurePlan
  case PawnPlay
  case OpeningAnchor
  case OpeningSupported
  case OpeningObserved
  case CandidateComparison
  case PlanPressure
  case PlanTransition
  case LineEvent
  case LineConsequence
  case StructuralDelta

final case class EvidenceSemanticAnchor(
    kind: EvidenceSemanticAnchorKind,
    values: List[String]
):
  def stableKey: String =
    (kind.toString :: values).mkString(":")

object EvidenceSemanticAnchor:
  def of(kind: EvidenceSemanticAnchorKind, values: String*): EvidenceSemanticAnchor =
    EvidenceSemanticAnchor(kind, values.toList)

enum EvidenceObjectKind:
  case Move
  case Piece
  case Side
  case Square
  case File
  case Pawn
  case PlanSubject
  case Relation
  case Motif
  case Line
  case Mechanism
  case Consequence
  case Horizon

final case class ConcreteChessObject(
    kind: EvidenceObjectKind,
    key: String
):
  def signaturePart: String =
    s"$kind:${key.trim.toLowerCase}"

final case class EvidenceObjectBinding(
    source: EvidenceRef,
    actor: List[ConcreteChessObject] = Nil,
    target: List[ConcreteChessObject] = Nil,
    mechanism: List[ConcreteChessObject] = Nil,
    consequence: List[ConcreteChessObject] = Nil,
    witness: List[ConcreteChessObject] = Nil,
    line: Option[LineNodeRef] = None,
    horizon: Option[String] = None,
    proofRole: Option[RelativeCauseProofRole] = None
):
  def concreteObjects: List[ConcreteChessObject] =
    (actor ++ target ++ mechanism ++ consequence).distinctBy(_.signaturePart)
  def hasConcreteObject: Boolean =
    target.nonEmpty ||
      (actor.nonEmpty && (mechanism.nonEmpty || consequence.nonEmpty))
  def playerFacingReady: Boolean =
    target.exists(EvidenceObjectBinding.specificSurfaceTargetObject) &&
      mechanism.nonEmpty &&
      (consequence.nonEmpty || witness.nonEmpty) &&
      proofRole.forall(_ != RelativeCauseProofRole.ContextSupport)
  def signature: String =
    val parts = List(
      "actor" -> actor,
      "target" -> target,
      "mechanism" -> mechanism,
      "consequence" -> consequence,
      "witness" -> witness
    ).flatMap { case (role, objects) =>
      objects.distinctBy(_.signaturePart).sortBy(_.signaturePart).map(obj => s"$role=${obj.signaturePart}")
    }
    val linePart = line.map(line => s"line=${line.id}").toList
    val horizonPart = horizon.map(horizon => s"horizon=${horizon.trim.toLowerCase}").toList
    val proofPart = proofRole.map(role => s"proof=$role").toList
    (parts ++ linePart ++ horizonPart ++ proofPart).mkString("|")

object EvidenceObjectBinding:

  def fromClaim(claim: ClaimSeed, graph: TypedEvidenceGraph): List[EvidenceObjectBinding] =
    fromEvidenceRefs(graph, claim.evidence)

  def fromEvidenceRefs(graph: TypedEvidenceGraph, refs: List[EvidenceRef]): List[EvidenceObjectBinding] =
    fromEvidenceRefs(graph, refs, Set.empty)

  private def fromEvidenceRefs(
      graph: TypedEvidenceGraph,
      refs: List[EvidenceRef],
      visited: Set[String]
  ): List[EvidenceObjectBinding] =
    refs
      .flatMap(ref => graph.byId.get(ref.id))
      .flatMap(record => fromRecord(record, graph, visited))
      .distinctBy(_.signature)

  def fromRelativeCause(cause: RelativeCauseFact, graph: TypedEvidenceGraph): List[EvidenceObjectBinding] =
    fromRelativeCause(cause, graph, Set.empty)

  private def fromRelativeCause(
      cause: RelativeCauseFact,
      graph: TypedEvidenceGraph,
      visited: Set[String]
  ): List[EvidenceObjectBinding] =
    val proofBindings =
      cause.proof.toList.flatMap { proof =>
        bindingsFromProofSection(proof.directProof, graph, visited) ++
          bindingsFromProofSection(proof.contrastProof, graph, visited) ++
          bindingsFromProofSection(proof.contextSupport, graph, visited)
      }
    val supportBindings =
      fromEvidenceRefs(graph, cause.supportEvidence, visited).map(_.copy(proofRole = Some(RelativeCauseProofRole.ContextSupport)))
    (proofBindings ++ supportBindings).distinctBy(_.signature)

  def objectSignatures(bindings: List[EvidenceObjectBinding]): List[String] =
    bindings.filter(_.hasConcreteObject).map(_.signature).distinct.sorted

  private[chessjudgment] def signatureParts(signature: String): List[String] =
    signature.split("\\|").toList.map(_.trim).filter(_.nonEmpty)

  private[chessjudgment] def signatureTokens(signatures: List[String], prefix: String): Set[String] =
    val normalizedPrefix = prefix.toLowerCase
    signatures.flatMap(signature =>
      signatureParts(signature).collect {
        case part if part.toLowerCase.startsWith(normalizedPrefix) => part
      }
    ).toSet

  private[chessjudgment] def signaturesForProofRole(
      signatures: Iterable[String],
      proofRole: Option[RelativeCauseProofRole]
  ): Set[String] =
    val proofPart = proofRole.map(role => s"proof=$role")
    signatures.filter(signature => proofPart.forall(signatureParts(signature).contains)).toSet

  private[chessjudgment] def objectTokens(
      signatures: Iterable[String],
      role: String,
      proofRole: Option[RelativeCauseProofRole] = None
  ): Set[String] =
    val prefix = s"$role="
    signatureTokens(signaturesForProofRole(signatures, proofRole).toList, prefix)
      .map(_.drop(prefix.length))

  private[chessjudgment] def signatureValues(signatures: List[String], role: String, kind: String): List[String] =
    val prefix = s"$role=$kind:"
    signatureTokens(signatures, prefix).toList.map(_.drop(prefix.length)).sorted

  private[chessjudgment] def concreteTargetToken(token: String): Boolean =
    !token.toLowerCase.contains("=side:") &&
      (
        token.contains("=Square:") ||
          token.contains("=File:") ||
          token.toLowerCase.contains("square:") ||
          token.toLowerCase.contains("file:") ||
          token.toLowerCase.matches(".*[a-h][1-8].*")
      )

  private[chessjudgment] def specificActorToken(token: String): Boolean =
    !token.toLowerCase.contains("=side:") &&
      !token.toLowerCase.contains("=move:") &&
      (
        token.contains("=Square:") ||
          token.toLowerCase.contains("square:") ||
        token.toLowerCase.matches(".*[a-h][1-8].*")
      )

  private[chessjudgment] def directProofSpecificTargetReady(signatures: Iterable[String]): Boolean =
    signaturesForProofRole(signatures, Some(RelativeCauseProofRole.DirectProof)).exists(specificTargetSurfaceReadySignature)

  private[chessjudgment] def playerFacingReadySignatures(signatures: Iterable[String]): Boolean =
    signatures.exists(playerFacingReadySignature)

  private def playerFacingReadySignature(signature: String): Boolean =
    specificTargetSurfaceReadySignature(signature) &&
      !signatureParts(signature).contains("proof=ContextSupport")

  private def specificTargetSurfaceReadySignature(signature: String): Boolean =
    val parts = signatureParts(signature)
    parts.exists(specificSurfaceTargetPart) &&
      parts.exists(_.startsWith("mechanism=")) &&
      parts.exists(part => part.startsWith("consequence=") || part.startsWith("witness="))

  private def specificSurfaceTargetPart(part: String): Boolean =
    val normalized = part.trim.toLowerCase
    normalized.startsWith("target=square:") ||
      normalized.startsWith("target=file:") ||
      normalized.startsWith("target=pawn:") ||
      normalized.startsWith("target=piece:") ||
      (
        normalized.startsWith("target=plansubject:") && {
          val subject = normalized.stripPrefix("target=plansubject:")
          subject.matches(".*[a-h][1-8].*")
        }
      )

  private[chessjudgment] def specificSurfaceTargetObject(obj: ConcreteChessObject): Boolean =
    obj.kind match
      case EvidenceObjectKind.Square | EvidenceObjectKind.File | EvidenceObjectKind.Pawn | EvidenceObjectKind.Piece =>
        true
      case EvidenceObjectKind.PlanSubject =>
        obj.key.toLowerCase.matches(".*[a-h][1-8].*")
      case _ =>
        false

  def playerFacingReady(bindings: List[EvidenceObjectBinding]): Boolean =
    bindings.exists(_.playerFacingReady)

  def hasConcreteObject(bindings: List[EvidenceObjectBinding]): Boolean =
    bindings.exists(_.hasConcreteObject)

  def lowLevelObjectAvailable(cause: RelativeCauseFact, graph: TypedEvidenceGraph): Boolean =
    val refs =
      cause.supportEvidence ++
        cause.proof.toList.flatMap(proof =>
          proof.directProof.sourceRefs ++ proof.contrastProof.sourceRefs ++ proof.contextSupport.sourceRefs
        )
    hasConcreteObject(fromEvidenceRefs(graph, refs))

  private def bindingsFromProofSection(
      section: RelativeCauseProofSection,
      graph: TypedEvidenceGraph,
      visited: Set[String]
  ): List[EvidenceObjectBinding] =
    fromEvidenceRefs(graph, section.sourceRefs, visited).map(_.copy(proofRole = Some(section.role)))

  private def fromRecord(
      record: EvidenceRecord,
      graph: TypedEvidenceGraph,
      visited: Set[String]
  ): List[EvidenceObjectBinding] =
    if visited.contains(record.ref.id) then Nil
    else
      val nextVisited = visited + record.ref.id
      record.payload match
        case payload: BoardFactEvidence =>
          payload.boardAnchors.flatMap(anchor => fromBoardAnchor(record.ref, anchor)) ++
            fromFacts(record.ref, payload.lowLevelFacts)
        case payload: StrategicFactEvidence =>
          payload.boardAnchors.flatMap(anchor => fromBoardAnchor(record.ref, anchor)) ++
            fromFacts(record.ref, payload.facts) ++
            payload.relatedPlans.map(plan =>
              EvidenceObjectBinding(
                source = record.ref,
                actor = Nil,
                target = objectOf(EvidenceObjectKind.PlanSubject, plan.toString),
                mechanism = objectOf(EvidenceObjectKind.Mechanism, payload.kind.toString),
                consequence = objectOf(EvidenceObjectKind.Consequence, payload.kind.toString),
                witness = objectOf(EvidenceObjectKind.PlanSubject, plan.toString),
                line = record.ref.line
              )
            )
        case payload: LineFactEvidence =>
          fromLineFact(record.ref, payload)
        case payload: MoveMotifEvidence =>
          List(fromMoveMotif(record.ref, payload))
        case payload: RelationFactEvidence =>
          List(fromRelation(record.ref, payload))
        case payload: ThreatEpisodeEvidence =>
          List(fromThreatEpisode(record.ref, payload))
        case payload: PawnStructureFactEvidence =>
          fromPawnStructure(record.ref, payload)
        case PlanPressureEvidence(_, activePlans) =>
          fromActivePlans(record.ref, activePlans)
        case PlanTransitionEvidence(transition) =>
          transition.primaryPlanId.toList.map(planId =>
            EvidenceObjectBinding(
              source = record.ref,
              actor = Nil,
              target = objectOf(EvidenceObjectKind.PlanSubject, planId),
              mechanism = objectOf(EvidenceObjectKind.Mechanism, "plan-transition"),
              consequence = objectOf(EvidenceObjectKind.Consequence, transition.transitionType.toString),
              witness = objectOf(EvidenceObjectKind.PlanSubject, planId),
              line = record.ref.line
            )
          )
        case payload: StructuralDeltaEvidence =>
          fromStructuralDelta(record.ref, payload)
        case payload: TacticalMechanismEvidence =>
          val sourceBindings =
            payload.signals.flatMap(signal =>
              signal.source.toList.flatMap(source =>
                graph.byId.get(source.id).toList.flatMap(sourceRecord =>
                  fromRecord(sourceRecord, graph, nextVisited).map(binding =>
                    binding.copy(
                      mechanism = (
                        binding.mechanism ++ objectOf(EvidenceObjectKind.Mechanism, payload.kind.toString)
                      ).distinctBy(_.signaturePart),
                      consequence = (
                        binding.consequence ++ objectOf(EvidenceObjectKind.Consequence, payload.kind.toString)
                      ).distinctBy(_.signaturePart),
                      line = binding.line.orElse(payload.line),
                      horizon = binding.horizon.orElse(payload.line.map(_.role.toString))
                    )
                  )
                )
              )
            )
          if sourceBindings.nonEmpty then sourceBindings.distinctBy(_.signature)
          else List(fromTacticalMechanism(record.ref, payload))
        case payload: StrategicMechanismEvidence =>
          payload.signals.flatMap(signal =>
            graph.byId
              .get(signal.source.id)
              .toList
              .flatMap(source =>
                fromRecord(source, graph, nextVisited).map(binding =>
                  binding.copy(
                    mechanism = (binding.mechanism ++ objectOf(EvidenceObjectKind.Mechanism, payload.kind.toString)).distinctBy(
                      _.signaturePart
                    ),
                    consequence = (
                      binding.consequence ++ signal.axis.toList.flatMap(axis =>
                        objectOf(EvidenceObjectKind.Consequence, axis.stableKey)
                      )
                    ).distinctBy(_.signaturePart),
                    horizon = binding.horizon.orElse(signal.axis.map(_.kind.toString))
                  )
                )
              )
          )
        case payload: StrategicMechanismContrastEvidence =>
          payload.axisComparisons.flatMap(axisComparison =>
            axisComparison.sources.flatMap(source =>
              graph.byId
                .get(source.id)
                .toList
                .flatMap(sourceRecord =>
                  fromRecord(sourceRecord, graph, nextVisited).map(binding =>
                    binding.copy(
                      mechanism = (
                        binding.mechanism ++ objectOf(EvidenceObjectKind.Mechanism, axisComparison.axis.kind.toString)
                      ).distinctBy(_.signaturePart),
                      consequence = (
                        binding.consequence ++ objectOf(EvidenceObjectKind.Consequence, axisComparison.outcome.toString)
                      ).distinctBy(_.signaturePart),
                      witness = (
                        binding.witness ++ lineObject(payload.referenceLine) ++ lineObject(payload.candidateLine)
                      ).distinctBy(_.signaturePart),
                      horizon = Some(payload.sustainability.horizon.toString)
                    )
                  )
                )
            )
          )
        case RelativeCauseFactEvidence(cause) =>
          fromRelativeCause(cause, graph, nextVisited)
        case MoveVerdictCertificationEvidence(certification) =>
          certification.causes.flatMap(cause => fromRelativeCause(cause, graph, nextVisited))
        case _ =>
          Nil

  private def fromBoardAnchor(ref: EvidenceRef, anchor: BoardAnchor): List[EvidenceObjectBinding] =
    val detail = anchor.detail
    val actor =
      objectOf(EvidenceObjectKind.Side, colorKey(anchor.side)) ++
        detail.toList.flatMap(detail =>
          squareObject(detail.attackerSquare) ++
            roleObject(detail.attackerRole) ++
            detail.attackerSquares.flatMap(square => objectOf(EvidenceObjectKind.Square, square.key)) ++
            squareObject(detail.subjectSquare) ++
            roleObject(detail.subjectRole) ++
            detail.subjectColor.toList.flatMap(color => objectOf(EvidenceObjectKind.Side, colorKey(color)))
        )
    val target =
      detail.toList.flatMap(detail =>
        squareObject(detail.targetSquare) ++
          roleObject(detail.targetRole) ++
          Option
            .when(anchor.kind == BoardAnchorKind.LooseMaterial)(
              squareObject(detail.subjectSquare) ++ roleObject(detail.subjectRole)
            )
            .toList
            .flatten ++
          fileObject(detail.file) ++
          detail.relatedSquares.flatMap(square => objectOf(EvidenceObjectKind.Square, square.key)) ++
          Option
            .when(anchor.kind == BoardAnchorKind.CounterplayRestraint)(detail.subjectColor)
            .flatten
            .toList
            .flatMap(color => objectOf(EvidenceObjectKind.Side, colorKey(color)))
      ) ++
        Option.when(detail.isEmpty)(anchor.focusSquares).toList.flatten.flatMap(square =>
          objectOf(EvidenceObjectKind.Square, square.key)
        )
    val mechanism =
      objectOf(EvidenceObjectKind.Mechanism, anchor.kind.toString) ++
        objectOf(EvidenceObjectKind.Mechanism, anchor.signal.toString) ++
        detail.toList.flatMap(_.axis.toList.flatMap(axis => objectOf(EvidenceObjectKind.Mechanism, axis.toString)))
    val consequence =
      objectOf(EvidenceObjectKind.Consequence, anchor.kind.toString)
    List(
      EvidenceObjectBinding(
        source = ref,
        actor = actor.distinctBy(_.signaturePart),
        target = target.distinctBy(_.signaturePart),
        mechanism = mechanism.distinctBy(_.signaturePart),
        consequence = consequence,
        witness = anchor.focusSquares.flatMap(square => objectOf(EvidenceObjectKind.Square, square.key)),
        line = ref.line
      )
    )

  private def fromFacts(ref: EvidenceRef, facts: List[Fact]): List[EvidenceObjectBinding] =
    facts.map { fact =>
      val focus = fact.squareFocus
      val factName = fact.getClass.getSimpleName.stripSuffix("$")
      EvidenceObjectBinding(
        source = ref,
        actor = focus.attackerSquares.flatMap(square => objectOf(EvidenceObjectKind.Square, square.key)) ++
          focus.subjectSquares.flatMap(square => objectOf(EvidenceObjectKind.Square, square.key)) ++
          factActorObjects(fact),
        target = focus.targetSquares.flatMap(square => objectOf(EvidenceObjectKind.Square, square.key)) ++
          focus.vulnerableMaterialSquares.flatMap(square => objectOf(EvidenceObjectKind.Square, square.key)) ++
          factTargetObjects(fact),
        mechanism = objectOf(EvidenceObjectKind.Mechanism, factName),
        consequence = objectOf(EvidenceObjectKind.Consequence, factName),
        witness = focus.relatedSquares.flatMap(square => objectOf(EvidenceObjectKind.Square, square.key)) ++
          fact.participants.flatMap(square => objectOf(EvidenceObjectKind.Square, square.key)),
        line = ref.line
      )
    }.filter(_.hasConcreteObject)

  private def factActorObjects(fact: Fact): List[ConcreteChessObject] =
    fact match
      case Fact.FileControl(_, color, _, _) =>
        objectOf(EvidenceObjectKind.Side, colorKey(color))
      case Fact.SpaceAdvantage(color, _, _) =>
        objectOf(EvidenceObjectKind.Side, colorKey(color))
      case _ =>
        Nil

  private def factTargetObjects(fact: Fact): List[ConcreteChessObject] =
    fact match
      case Fact.FileControl(file, _, _, _) =>
        objectOf(EvidenceObjectKind.File, file.toString.toLowerCase)
      case _ =>
        Nil

  private def fromLineFact(ref: EvidenceRef, payload: LineFactEvidence): List[EvidenceObjectBinding] =
    val rootBindings =
      payload.rootMove.toList.map { moveUci =>
        val move = normalize(moveUci)
        EvidenceObjectBinding(
          source = ref,
          actor = moveObjects(move),
          target = moveTargetSquare(move),
          mechanism = objectOf(EvidenceObjectKind.Mechanism, "LineRootMove"),
          consequence = objectOf(EvidenceObjectKind.Consequence, "LineRootMove"),
          witness = objectOf(EvidenceObjectKind.Move, move) ++ moveTargetSquare(move) ++ lineObject(payload.line),
          line = Some(payload.line)
        )
      }
    val continuationMoves =
      (payload.lineReplayContinuationMoves.take(4) ++ payload.lineReplayContinuationMoves.takeRight(4)).distinct
    val replayBindings =
      continuationMoves.map { moveUci =>
        val move = normalize(moveUci)
        EvidenceObjectBinding(
          source = ref,
          actor = moveObjects(move),
          target = moveTargetSquare(move),
          mechanism = objectOf(EvidenceObjectKind.Mechanism, "LineContinuation"),
          consequence = objectOf(EvidenceObjectKind.Consequence, "LineContinuation"),
          witness = objectOf(EvidenceObjectKind.Move, move) ++ moveTargetSquare(move) ++ lineObject(payload.line),
          line = Some(payload.line)
        )
      }
    val eventBindings =
      payload.lineEvents.map { event =>
        val move = normalize(event.moveUci)
        EvidenceObjectBinding(
          source = ref,
          actor = moveObjects(move) ++
            event.side.toList.flatMap(color => objectOf(EvidenceObjectKind.Side, colorKey(color))) ++
            roleObject(event.pieceRole),
          target = {
            val squareTarget = squareObject(event.square)
            (if squareTarget.nonEmpty then squareTarget else moveTargetSquare(move)) ++ roleObject(event.targetRole)
          },
          mechanism = objectOf(EvidenceObjectKind.Mechanism, event.kind.toString),
          consequence = objectOf(EvidenceObjectKind.Consequence, event.kind.toString),
          witness = objectOf(EvidenceObjectKind.Move, move) ++ moveTargetSquare(move) ++ lineObject(payload.line),
          line = Some(payload.line),
          horizon = Some(s"ply:${event.plyOffset}")
        )
      }
    val consequenceBindings =
      payload.proofSignalConsequences.map { consequence =>
        val lineMoves = consequence.lineMoves.map(normalize)
        val eventMove = consequence.eventMove.orElse(lineMoves.headOption).map(normalize)
        val consequenceMoves = (lineMoves ++ eventMove.toList).distinct
        val lineMoveWitness =
          consequenceMoves.flatMap(move => objectOf(EvidenceObjectKind.Move, move) ++ moveTargetSquare(move))
        val lineEventWitness =
          payload.lineEvents
            .filter(event => consequenceMoves.contains(normalize(event.moveUci)))
            .flatMap(event => squareObject(event.square) ++ roleObject(event.pieceRole) ++ roleObject(event.targetRole))
        EvidenceObjectBinding(
          source = ref,
          actor = eventMove.toList.flatMap(moveObjects),
          target = eventMove.toList.flatMap(moveTargetSquare),
          mechanism = objectOf(EvidenceObjectKind.Mechanism, consequence.kind.toString),
          consequence = objectOf(EvidenceObjectKind.Consequence, consequence.kind.toString),
          witness = lineMoveWitness ++ lineEventWitness ++ lineObject(payload.line),
          line = Some(payload.line)
        )
      }
    val materialCaptureBindings =
      payload.materialCaptures.map { capture =>
        val move = normalize(capture.moveUci)
        val rootMove = payload.rootMove.map(normalize)
        val rootOwnedCapture = capture.plyOffset == 0 || rootMove.contains(move)
        val prefix = if capture.recapture then "material-recapture" else "material-capture"
        val materialIdentityTargets =
          if rootOwnedCapture then
            objectOf(EvidenceObjectKind.PlanSubject, s"$prefix:${capture.square.key}") ++
              Option
                .when(payload.materialSacrificeCapture(capture))(s"material-sacrifice:${capture.square.key}")
                .toList
                .flatMap(objectOf(EvidenceObjectKind.PlanSubject, _))
          else Nil
        EvidenceObjectBinding(
          source = ref,
          actor = moveObjects(move) ++
            roleObject(Some(capture.attackerRole)) ++
            objectOf(EvidenceObjectKind.Side, colorKey(capture.side)),
          target = squareObject(Some(capture.square)) ++ roleObject(Some(capture.capturedRole)) ++ materialIdentityTargets,
          mechanism = objectOf(
            EvidenceObjectKind.Mechanism,
            if capture.recapture then "MaterialRecapture" else "MaterialCapture"
          ),
          consequence = objectOf(EvidenceObjectKind.Consequence, "MaterialCapture"),
          witness = objectOf(EvidenceObjectKind.Move, move) ++ squareObject(Some(capture.square)) ++ lineObject(payload.line),
          line = Some(payload.line),
          horizon = Some(s"ply:${capture.plyOffset}")
        )
      }
    val horizonBindings =
      payload.endgameTechniqueHorizons.map { horizon =>
        val triggerMove = horizon.triggerMove.map(normalize)
        EvidenceObjectBinding(
          source = ref,
          actor =
            objectOf(EvidenceObjectKind.Side, horizon.techniqueSideKey) ++
              triggerMove.toList.flatMap(moveObjects),
          target = horizon.requiredSquares.flatMap(square => objectOf(EvidenceObjectKind.Square, square)),
          mechanism = objectOf(EvidenceObjectKind.Mechanism, "EndgameTechniqueHorizon") ++
            objectOf(EvidenceObjectKind.Mechanism, horizon.pattern),
          consequence = objectOf(EvidenceObjectKind.Consequence, horizon.status.toString),
          witness = horizon.maintainedSquares.flatMap(square => objectOf(EvidenceObjectKind.Square, square)) ++
            horizon.brokenSquares.flatMap(square => objectOf(EvidenceObjectKind.Square, square)) ++
            lineObject(payload.line),
          line = Some(payload.line),
          horizon = Some(s"endgame:${horizon.status}:entry=${horizon.entryPlyOffset}:terminal=${horizon.terminalPlyOffset}")
        )
      }
    (rootBindings ++ replayBindings ++ eventBindings ++ consequenceBindings ++ horizonBindings ++ materialCaptureBindings).distinctBy(
      _.signature
    )

  private def fromMoveMotif(ref: EvidenceRef, payload: MoveMotifEvidence): EvidenceObjectBinding =
    val proof = payload.proof
    EvidenceObjectBinding(
      source = ref,
      actor = moveObjects(payload.eventMove.getOrElse(payload.rootMove)) ++
        proof.subjectSquares.flatMap(square => objectOf(EvidenceObjectKind.Square, square.key)) ++
        proof.roles.map(role => ConcreteChessObject(EvidenceObjectKind.Piece, normalize(role.name))),
      target = proof.targetSquares.flatMap(square => objectOf(EvidenceObjectKind.Square, square.key)) ++
        proof.relatedSquares.flatMap(square => objectOf(EvidenceObjectKind.Square, square.key)) ++
        proof.relatedFiles.flatMap(file => objectOf(EvidenceObjectKind.File, file.key)),
      mechanism = objectOf(EvidenceObjectKind.Motif, proof.kind) ++ objectOf(EvidenceObjectKind.Mechanism, proof.category.toString),
      consequence = objectOf(EvidenceObjectKind.Consequence, proof.category.toString),
      witness = payload.line.toList.flatMap(lineObject) ++ objectOf(EvidenceObjectKind.Move, payload.rootMove),
      line = payload.line,
      horizon = Some(s"ply:${payload.plyOffset}")
    )

  private def fromRelation(ref: EvidenceRef, payload: RelationFactEvidence): EvidenceObjectBinding =
    val actorParticipants =
      payload.participants.filter(participant =>
        participant.participantRole == RelationParticipantRole.Attacker ||
          participant.participantRole == RelationParticipantRole.Mover ||
          participant.participantRole == RelationParticipantRole.Beneficiary
      )
    val targetParticipants =
      payload.participants.filter(participant =>
        participant.participantRole == RelationParticipantRole.Target ||
          participant.participantRole == RelationParticipantRole.Defender ||
          participant.participantRole == RelationParticipantRole.King ||
          participant.participantRole == RelationParticipantRole.Blocker
      )
    EvidenceObjectBinding(
      source = ref,
      actor = actorParticipants.flatMap(participantObjects),
      target = payload.targetSquare.toList.flatMap(square => objectOf(EvidenceObjectKind.Square, square.key)) ++
        targetParticipants.flatMap(participantObjects),
      mechanism = objectOf(EvidenceObjectKind.Relation, payload.kind.toString) ++
        objectOf(EvidenceObjectKind.Mechanism, payload.detail.detailName),
      consequence = objectOf(EvidenceObjectKind.Consequence, payload.kind.toString),
      witness = payload.lineMoves.flatMap(move => objectOf(EvidenceObjectKind.Move, move)) ++
        payload.focusSquares.flatMap(square => objectOf(EvidenceObjectKind.Square, square.key)),
      line = ref.line
    )

  private def fromThreatEpisode(ref: EvidenceRef, payload: ThreatEpisodeEvidence): EvidenceObjectBinding =
    val episode = payload.episode
    val attackSquareObjects = episode.attackSquares.flatMap(square => objectOf(EvidenceObjectKind.Square, square.key))
    val defenseWitness =
      episode.bestDefense.toList.map(normalize).flatMap(move => objectOf(EvidenceObjectKind.Move, move) ++ moveTargetSquare(move))
    EvidenceObjectBinding(
      source = ref,
      actor = attackSquareObjects,
      target = attackSquareObjects ++
        objectOf(EvidenceObjectKind.Side, colorKey(episode.sideUnderPressure)) ++
        episode.targetPieces.flatMap(role => objectOf(EvidenceObjectKind.Piece, role.name)),
      mechanism = objectOf(EvidenceObjectKind.Mechanism, episode.kind.toString) ++
        objectOf(EvidenceObjectKind.Mechanism, episode.driver.toString),
      consequence = objectOf(EvidenceObjectKind.Consequence, episode.severity.toString),
      witness = defenseWitness ++
        attackSquareObjects ++
        episode.motifs.flatMap(motif =>
          objectOf(EvidenceObjectKind.Motif, motif.getClass.getSimpleName.stripSuffix("$"))
        ),
      line = ref.line,
      horizon = Some(s"turns:${episode.turnsToImpact}")
    )

  private def fromPawnStructure(ref: EvidenceRef, payload: PawnStructureFactEvidence): List[EvidenceObjectBinding] =
    val pawnPlayBindings =
      payload.pawnPlay.toList.flatMap { pawnPlay =>
        val target =
          pawnPlay.breakFile.toList.flatMap(file => objectOf(EvidenceObjectKind.File, file)) ++
            pawnPlay.tensionSquares.flatMap(subjectObject) ++
            pawnPlay.tensionEdges.flatMap(tensionEdgeObjects) ++
            pawnPlay.counterBreakFiles.flatMap(file => objectOf(EvidenceObjectKind.File, file)) ++
            pawnPlay.blockadeSquare.toList.flatMap(square => objectOf(EvidenceObjectKind.Square, square.key))
        Option.when(target.nonEmpty)(
          EvidenceObjectBinding(
            source = ref,
            actor = pawnPlay.blockadeRole.toList.flatMap(role => objectOf(EvidenceObjectKind.Piece, role.toString)),
            target = target.distinctBy(_.signaturePart),
            mechanism = objectOf(EvidenceObjectKind.Mechanism, pawnPlay.primaryDriver.toString),
            consequence = objectOf(EvidenceObjectKind.Consequence, payload.profile.primary.toString),
            witness = target.distinctBy(_.signaturePart),
            line = ref.line
          )
        )
      }
    val alignmentBindings =
      payload.alignment.toList.flatMap(_.matchedPlanIds).map(planId =>
        EvidenceObjectBinding(
          source = ref,
          actor = Nil,
          target = objectOf(EvidenceObjectKind.PlanSubject, planId),
          mechanism = objectOf(EvidenceObjectKind.Mechanism, payload.profile.primary.toString),
          consequence = objectOf(EvidenceObjectKind.Consequence, payload.profile.primary.toString),
          witness = objectOf(EvidenceObjectKind.PlanSubject, planId),
          line = ref.line
        )
      )
    (pawnPlayBindings ++ alignmentBindings).distinctBy(_.signature)

  private def fromActivePlans(ref: EvidenceRef, activePlans: ActivePlans): List[EvidenceObjectBinding] =
    (activePlans.primary :: activePlans.secondary.toList).map { plan =>
      val evidenceAtoms = plan.evidence
      val evidenceMoveWitnessObjects =
        evidenceAtoms
          .flatMap(_.motif.move.toList)
          .map(normalize)
          .filter(move => ref.line.exists(line => EvidenceRef.sameMove(line.rootMove, move)))
          .flatMap(move => objectOf(EvidenceObjectKind.Move, move) ++ moveTargetSquare(move))
          .distinctBy(_.signaturePart)
      EvidenceObjectBinding(
        source = ref,
        actor = evidenceAtoms.flatMap(planEvidenceActorObjects(_, ref)).distinctBy(_.signaturePart),
        target = objectOf(EvidenceObjectKind.PlanSubject, plan.plan.id.toString),
        mechanism = objectOf(EvidenceObjectKind.Mechanism, "plan-pressure"),
        consequence = objectOf(EvidenceObjectKind.Consequence, plan.plan.id.toString),
        witness = evidenceAtoms.flatMap(evidence => objectOf(EvidenceObjectKind.PlanSubject, evidence.toString)) ++
          evidenceMoveWitnessObjects,
        line = ref.line
      )
    }.distinctBy(_.signature)

  private def planEvidenceActorObjects(
      evidence: lila.chessjudgment.model.EvidenceAtom,
      ref: EvidenceRef
  ): List[ConcreteChessObject] =
    evidence.motif.move.toList
      .filter(move => ref.line.exists(line => EvidenceRef.sameMove(line.rootMove, move)))
      .flatMap(moveObjects)

  private def fromStructuralDelta(ref: EvidenceRef, payload: StructuralDeltaEvidence): List[EvidenceObjectBinding] =
    val actor =
      moveObjects(payload.moveUci) ++ objectOf(EvidenceObjectKind.Side, colorKey(payload.perspective))
    val signalBindings =
      payload.signals.map { signal =>
        EvidenceObjectBinding(
          source = ref,
          actor = actor,
          target = signal.subjects.flatMap(subjectObject),
          mechanism = objectOf(EvidenceObjectKind.Mechanism, signal.kind.toString),
          consequence = objectOf(EvidenceObjectKind.Consequence, signal.polarity.toString),
          witness = objectOf(EvidenceObjectKind.Move, payload.moveUci) ++ payload.line.toList.flatMap(lineObject),
          line = payload.line
        )
      }
    val consequenceBindings =
      payload.consequences.map { consequence =>
        EvidenceObjectBinding(
          source = ref,
          actor = actor,
          target = consequence.subjects.flatMap(subjectObject),
          mechanism = objectOf(EvidenceObjectKind.Mechanism, consequence.kind.toString),
          consequence = objectOf(EvidenceObjectKind.Consequence, consequence.anchorKey),
          witness = objectOf(EvidenceObjectKind.Move, payload.moveUci) ++ payload.line.toList.flatMap(lineObject),
          line = payload.line
        )
      }
    val developmentBindings =
      payload.developmentChoices.map { choice =>
        EvidenceObjectBinding(
          source = ref,
          actor = objectOf(EvidenceObjectKind.Piece, choice.role) ++
            objectOf(EvidenceObjectKind.Square, choice.from) ++
            objectOf(EvidenceObjectKind.Move, payload.moveUci),
          target = objectOf(EvidenceObjectKind.Square, choice.to),
          mechanism = objectOf(EvidenceObjectKind.Mechanism, StructuralSignalKind.DevelopmentChoice.toString),
          consequence = objectOf(EvidenceObjectKind.Consequence, TransitionConsequenceKind.DevelopmentPieceActivated.toString),
          witness = payload.line.toList.flatMap(lineObject),
          line = payload.line
        )
      }
    (signalBindings ++ consequenceBindings ++ developmentBindings).distinctBy(_.signature)

  private def fromTacticalMechanism(ref: EvidenceRef, payload: TacticalMechanismEvidence): EvidenceObjectBinding =
    EvidenceObjectBinding(
      source = ref,
      actor = payload.moveUci.toList.flatMap(moveObjects),
      target = Nil,
      mechanism = objectOf(EvidenceObjectKind.Mechanism, payload.kind.toString) ++
        payload.signals.flatMap(signal => objectOf(EvidenceObjectKind.Mechanism, signal.label)),
      consequence = objectOf(EvidenceObjectKind.Consequence, payload.kind.toString),
      witness = payload.line.toList.flatMap(lineObject),
      line = payload.line
    )

  private def participantObjects(participant: RelationParticipant): List[ConcreteChessObject] =
    objectOf(EvidenceObjectKind.Square, participant.square.key) ++
      participant.role.toList.flatMap(role => objectOf(EvidenceObjectKind.Piece, role.name)) ++
      objectOf(EvidenceObjectKind.Mechanism, participant.participantRole.toString)

  private def moveObjects(move: String): List[ConcreteChessObject] =
    val normalized = normalize(move)
    objectOf(EvidenceObjectKind.Move, normalized) ++ moveSourceSquare(normalized)

  private def moveSourceSquare(move: String): List[ConcreteChessObject] =
    if move.length >= 2 then objectOf(EvidenceObjectKind.Square, move.take(2)) else Nil

  private def moveTargetSquare(move: String): List[ConcreteChessObject] =
    if move.length >= 4 then objectOf(EvidenceObjectKind.Square, move.slice(2, 4)) else Nil

  private def squareObject(square: Option[EvidenceSquare]): List[ConcreteChessObject] =
    square.toList.flatMap(square => objectOf(EvidenceObjectKind.Square, square.key))

  private def fileObject(file: Option[EvidenceFile]): List[ConcreteChessObject] =
    file.toList.flatMap(file => objectOf(EvidenceObjectKind.File, file.key))

  private def roleObject(role: Option[EvidencePieceRole]): List[ConcreteChessObject] =
    role.toList.flatMap(role => objectOf(EvidenceObjectKind.Piece, role.name))

  private def lineObject(line: LineNodeRef): List[ConcreteChessObject] =
    objectOf(EvidenceObjectKind.Line, line.id) ++ objectOf(EvidenceObjectKind.Move, line.rootMove)

  private def subjectObject(raw: String): List[ConcreteChessObject] =
    val identityObject =
      StructuralPurposeSubject.structuralIdentity(raw).toList.flatMap { identity =>
        objectOf(EvidenceObjectKind.PlanSubject, identity)
      }
    val fileSquareTarget = StructuralPurposeSubject.fileSquareTarget(raw)
    val cleaned = StructuralPurposeSubject.carrierToken(raw)
    val weakPawnSquare = StructuralPurposeSubject.weakPawnSquare(cleaned)
    StructuralPurposeSubject.parse(cleaned) match
      case Some(StructuralPurposeSubject.PieceRoute(role, _, to)) =>
        identityObject ++ objectOf(EvidenceObjectKind.Piece, role) ++ objectOf(EvidenceObjectKind.Square, to)
      case Some(StructuralPurposeSubject.Outpost(role, square)) =>
        identityObject ++ objectOf(EvidenceObjectKind.Piece, role) ++ objectOf(EvidenceObjectKind.Square, square)
      case Some(StructuralPurposeSubject.PieceRestriction(role, square, blocker)) =>
        identityObject ++ objectOf(EvidenceObjectKind.Piece, role) ++
          objectOf(EvidenceObjectKind.Square, square) ++
          objectOf(EvidenceObjectKind.Square, blocker)
      case Some(StructuralPurposeSubject.PieceSquare(role, square)) =>
        identityObject ++ objectOf(EvidenceObjectKind.Piece, role) ++ objectOf(EvidenceObjectKind.Square, square)
      case Some(StructuralPurposeSubject.Battery(_, from, to, roles)) =>
        identityObject ++ roles.flatMap(role => objectOf(EvidenceObjectKind.Piece, role)) ++
          objectOf(EvidenceObjectKind.Square, from) ++
          objectOf(EvidenceObjectKind.Square, to)
      case Some(StructuralPurposeSubject.TensionEdge(from, to)) =>
        identityObject ++ objectOf(EvidenceObjectKind.Square, from) ++ objectOf(EvidenceObjectKind.Square, to)
      case None if weakPawnSquare.nonEmpty =>
        identityObject ++ weakPawnSquare.toList.flatMap(square =>
          objectOf(EvidenceObjectKind.Pawn, s"weak-pawn:$square") ++ objectOf(EvidenceObjectKind.Square, square)
        )
      case None if fileSquareTarget.nonEmpty =>
        identityObject ++ fileSquareTarget.toList.flatMap { case (file, square) =>
          objectOf(EvidenceObjectKind.File, file) ++ objectOf(EvidenceObjectKind.Square, square)
        }
      case None if cleaned.matches("[a-h][1-8]") => identityObject ++ objectOf(EvidenceObjectKind.Square, cleaned)
      case None if cleaned.matches("[a-h]")      => identityObject ++ objectOf(EvidenceObjectKind.File, cleaned)
      case None if cleaned.contains("pawn")      => identityObject ++ objectOf(EvidenceObjectKind.Pawn, cleaned)
      case None                                  => identityObject ++ objectOf(EvidenceObjectKind.PlanSubject, cleaned)

  private def tensionEdgeObjects(raw: String): List[ConcreteChessObject] =
    normalize(raw)
      .split("-")
      .toList
      .flatMap(subjectObject)
      .distinctBy(_.signaturePart)

  private def objectOf(kind: EvidenceObjectKind, raw: String): List[ConcreteChessObject] =
    val key = normalize(raw)
    Option.when(key.nonEmpty)(ConcreteChessObject(kind, key)).toList

  private def normalize(raw: String): String =
    Option(raw).getOrElse("").trim.toLowerCase

  private def colorKey(color: Color): String =
    if color.white then "white" else "black"

private[judgment] object StructuralPurposeSubject:
  sealed trait Parsed
  final case class PieceRoute(piece: String, from: String, to: String) extends Parsed
  final case class Outpost(piece: String, square: String) extends Parsed
  final case class PieceRestriction(piece: String, square: String, blocker: String) extends Parsed
  final case class PieceSquare(piece: String, square: String) extends Parsed
  final case class Battery(axis: String, from: String, to: String, roles: List[String]) extends Parsed
  final case class TensionEdge(from: String, to: String) extends Parsed

  private val pieceRoute = raw"([a-z]+):([a-h][1-8])-([a-h][1-8]).*".r
  private val outpost = raw"outpost:([a-z]+):([a-h][1-8]).*".r
  private val pieceRestriction = raw"([a-z]+):([a-h][1-8]):diagonal-denial:blocked-by:([a-h][1-8]).*".r
  private val pieceSquare = raw"([a-z]+):([a-h][1-8])(?::.*)?".r
  private val passedPawnRoute = raw"passed-pawn-(?:advanced|breakthrough|promoted):([a-h][1-8])-([a-h][1-8]).*".r
  private val passedPawnSquare = raw"passed-pawn-(?:created|lost):([a-h][1-8]).*".r
  private val passedPawn = raw"passed-pawn:([a-h][1-8]).*".r
  private val weakPawn = raw"weak-pawn:([a-h][1-8]).*".r
  private val battery = raw"battery:([a-z]+):([a-h][1-8])-([a-h][1-8])(?::([a-z-]+))?.*".r
  private val rookLift = raw"rook-lift:([a-h][1-8])-([a-h][1-8]):rank-([0-9]+).*".r
  private val tensionEdge = raw"([a-h][1-8])-([a-h][1-8])".r
  private val fileSquare = raw"([a-h]):([a-h][1-8])(?::.*)?".r
  private val carrierPrefixes = List(
    "square:",
    "file:",
    "target:",
    "subject:",
    "created-tension:",
    "resolved-tension:",
    "break-file:",
    "open-file:",
    "semi-open-file:",
    "weak-square:"
  )

  def carrierToken(raw: String): String =
    carrierPrefixes.foldLeft(normalize(raw))((value, prefix) => value.stripPrefix(prefix)) match
      case fileSquare(_, square) => square
      case value                 => value

  def parse(raw: String): Option[Parsed] =
    carrierToken(raw) match
      case outpost(piece, square) =>
        Some(Outpost(piece, square))
      case battery(axis, from, to, roles) =>
        Some(Battery(axis, from, to, Option(roles).toList.flatMap(_.split("-").toList).filter(_.nonEmpty).distinct.sorted))
      case rookLift(from, to, _) =>
        Some(PieceRoute("rook", from, to))
      case passedPawnRoute(from, to) =>
        Some(PieceRoute("pawn", from, to))
      case passedPawnSquare(square) =>
        Some(PieceSquare("pawn", square))
      case passedPawn(square) =>
        Some(PieceSquare("pawn", square))
      case pieceRoute(piece, from, to) =>
        Some(PieceRoute(piece, from, to))
      case pieceRestriction(piece, square, blocker) =>
        Some(PieceRestriction(piece, square, blocker))
      case pieceSquare(piece, square) =>
        Some(PieceSquare(piece, square))
      case tensionEdge(from, to) =>
        Some(TensionEdge(from, to))
      case _ =>
        None

  def weakPawnSquare(raw: String): Option[String] =
    carrierToken(raw) match
      case weakPawn(square) => Some(square)
      case _                => None

  def fileSquareTarget(raw: String): Option[(String, String)] =
    normalize(raw).stripPrefix("subject:") match
      case fileSquare(file, square) => Some(file -> square)
      case _                        => None

  def structuralIdentity(raw: String): Option[String] =
    val value = normalize(raw).stripPrefix("subject:")
    Option.when(
      value.startsWith("created-tension:") ||
        value.startsWith("resolved-tension:") ||
        value.startsWith("weak-square:") ||
        value.startsWith("rook-lift:") ||
        value.startsWith("passed-pawn-")
    )(value)

  private def normalize(raw: String): String =
    Option(raw).getOrElse("").trim.toLowerCase

enum RelationParticipantRole:
  case Attacker
  case Defender
  case Target
  case Blocker
  case Beneficiary
  case King
  case Mover
  case Bait
  case Lured
  case Other

final case class RelationParticipant(
    square: EvidenceSquare,
    role: Option[EvidencePieceRole],
    participantRole: RelationParticipantRole
)

enum RelationProofAtomRole:
  case Participant
  case LineMove
  case Focus
  case Target

final case class RelationProofAtom(
    role: RelationProofAtomRole,
    square: Option[EvidenceSquare] = None,
    moveUci: Option[String] = None,
    participantRole: Option[RelationParticipantRole] = None,
    pieceRole: Option[EvidencePieceRole] = None,
    label: Option[String] = None
)

enum RelationThreatSignal:
  case MateCheck
  case Check

enum RelationAxisSignal:
  case File
  case Rank
  case Diagonal

final case class RelationWitnessTarget(
    square: EvidenceSquare,
    role: EvidencePieceRole
)

enum RelationWitnessDetail:
  case Empty
  case DefenderTrade(defenderSquare: EvidenceSquare, exchangeSquare: EvidenceSquare, targetSquare: EvidenceSquare)
  case BadPieceLiquidation(badPieceSquare: EvidenceSquare, exchangeSquare: EvidenceSquare)
  case Overload(defenderSquare: EvidenceSquare, targetSquares: List[EvidenceSquare], attackerSquare: EvidenceSquare)
  case Deflection(defenderSquare: EvidenceSquare, targetSquare: EvidenceSquare, attackerSquare: EvidenceSquare)
  case DiscoveredAttack(
      attackerSquare: EvidenceSquare,
      clearedSquare: EvidenceSquare,
      targetSquare: EvidenceSquare,
      attackerRole: EvidencePieceRole
  )
  case DoubleCheck(kingSquare: EvidenceSquare, checkerSquares: List[EvidenceSquare], moverSquare: EvidenceSquare, moverRole: EvidencePieceRole)
  case MatePattern(
      relationKind: String,
      kingSquare: EvidenceSquare,
      checkerSquares: List[EvidenceSquare],
      matingMove: String,
      patternId: Option[String]
  )
  case GreekGift(bishopSquare: EvidenceSquare, targetSquare: EvidenceSquare, entryMove: String, patternId: String)
  case Fork(attackerSquare: EvidenceSquare, attackerRole: EvidencePieceRole, targets: List[RelationWitnessTarget])
  case HangingPiece(
      attackerSquare: EvidenceSquare,
      targetSquare: EvidenceSquare,
      attackerRole: EvidencePieceRole,
      targetRole: EvidencePieceRole
  )
  case TrappedPiece(
      attackerSquare: EvidenceSquare,
      targetSquare: EvidenceSquare,
      attackerRole: EvidencePieceRole,
      targetRole: EvidencePieceRole
  )
  case Domination(
      attackerSquare: EvidenceSquare,
      targetSquare: EvidenceSquare,
      attackerRole: EvidencePieceRole,
      targetRole: EvidencePieceRole,
      controlledEscapeSquares: List[EvidenceSquare]
  )
  case Zwischenzug(
      intermediateMove: String,
      expectedRecaptureSquare: EvidenceSquare,
      checkingPieceSquare: EvidenceSquare,
      checkingPieceRole: EvidencePieceRole,
      checkedKingSquare: EvidenceSquare,
      threatType: RelationThreatSignal
  )
  case Decoy(
      baitFromSquare: EvidenceSquare,
      baitSquare: EvidenceSquare,
      luredFromSquare: EvidenceSquare,
      executionFromSquare: EvidenceSquare,
      executionToSquare: EvidenceSquare,
      baitRole: EvidencePieceRole,
      luredRole: EvidencePieceRole
  )
  case XRay(
      attackerSquare: EvidenceSquare,
      blockerSquare: EvidenceSquare,
      targetSquare: EvidenceSquare,
      attackerRole: EvidencePieceRole,
      blockerRole: EvidencePieceRole,
      targetRole: EvidencePieceRole
  )
  case Clearance(
      beneficiarySquare: EvidenceSquare,
      clearedSquare: EvidenceSquare,
      targetSquare: EvidenceSquare,
      beneficiaryRole: EvidencePieceRole,
      clearingTo: EvidenceSquare
  )
  case Battery(
      frontSquare: EvidenceSquare,
      backSquare: EvidenceSquare,
      targetSquare: EvidenceSquare,
      frontRole: EvidencePieceRole,
      backRole: EvidencePieceRole,
      axis: RelationAxisSignal
  )
  case Interference(
      blockerSquare: EvidenceSquare,
      defenderSquare: EvidenceSquare,
      targetSquare: EvidenceSquare,
      blockerRole: EvidencePieceRole,
      defenderRole: EvidencePieceRole,
      targetRole: EvidencePieceRole
  )
  case Pin(
      attackerSquare: EvidenceSquare,
      pinnedSquare: EvidenceSquare,
      behindSquare: EvidenceSquare,
      targetSquare: EvidenceSquare,
      attackerRole: EvidencePieceRole,
      pinnedRole: EvidencePieceRole,
      behindRole: EvidencePieceRole,
      absolute: Boolean
  )
  case Skewer(
      attackerSquare: EvidenceSquare,
      frontSquare: EvidenceSquare,
      backSquare: EvidenceSquare,
      targetSquare: EvidenceSquare,
      attackerRole: EvidencePieceRole,
      frontRole: EvidencePieceRole,
      backRole: EvidencePieceRole
  )
  case StalemateTrap(stalematedKingSquare: EvidenceSquare, resourceSquare: EvidenceSquare, entryMove: String, terminalMove: String, scoreCp: Option[Int])
  case PerpetualCheck(
      checkedKingSquare: EvidenceSquare,
      checkerSquares: List[EvidenceSquare],
      checkingSide: String,
      entryMove: String,
      cycleStartMove: String,
      cycleReturnMove: String,
      repeatedPositionKey: String,
      scoreCp: Option[Int]
  )

  def detailName: String =
    toString.takeWhile(_ != '(')
  def detailSummary: String =
    this match
      case Empty =>
        detailName
      case DefenderTrade(defenderSquare, exchangeSquare, targetSquare) =>
        detail("DefenderTrade", s"defender=${sq(defenderSquare)}", s"exchange=${sq(exchangeSquare)}", s"target=${sq(targetSquare)}")
      case BadPieceLiquidation(badPieceSquare, exchangeSquare) =>
        detail("BadPieceLiquidation", s"bad_piece=${sq(badPieceSquare)}", s"exchange=${sq(exchangeSquare)}")
      case Overload(defenderSquare, targetSquares, attackerSquare) =>
        detail("Overload", s"defender=${sq(defenderSquare)}", s"targets=${squares(targetSquares)}", s"attacker=${sq(attackerSquare)}")
      case Deflection(defenderSquare, targetSquare, attackerSquare) =>
        detail("Deflection", s"defender=${sq(defenderSquare)}", s"target=${sq(targetSquare)}", s"attacker=${sq(attackerSquare)}")
      case DiscoveredAttack(attackerSquare, clearedSquare, targetSquare, attackerRole) =>
        detail(
          "DiscoveredAttack",
          s"attacker=${pieceAt(attackerRole, attackerSquare)}",
          s"cleared=${sq(clearedSquare)}",
          s"target=${sq(targetSquare)}"
        )
      case DoubleCheck(kingSquare, checkerSquares, moverSquare, moverRole) =>
        detail("DoubleCheck", s"king=${sq(kingSquare)}", s"checkers=${squares(checkerSquares)}", s"mover=${pieceAt(moverRole, moverSquare)}")
      case MatePattern(relationKind, kingSquare, checkerSquares, matingMove, patternId) =>
        detail(
          "MatePattern",
          s"relation=$relationKind",
          s"king=${sq(kingSquare)}",
          s"checkers=${squares(checkerSquares)}",
          s"mating_move=$matingMove",
          patternId.fold("")(id => s"pattern=$id")
        )
      case GreekGift(bishopSquare, targetSquare, entryMove, patternId) =>
        detail("GreekGift", s"bishop=${sq(bishopSquare)}", s"target=${sq(targetSquare)}", s"entry_move=$entryMove", s"pattern=$patternId")
      case Fork(attackerSquare, attackerRole, targets) =>
        detail("Fork", s"attacker=${pieceAt(attackerRole, attackerSquare)}", s"targets=${targets.map(targetSummary).distinct.sorted.mkString("/")}")
      case HangingPiece(attackerSquare, targetSquare, attackerRole, targetRole) =>
        detail("HangingPiece", s"attacker=${pieceAt(attackerRole, attackerSquare)}", s"target=${pieceAt(targetRole, targetSquare)}")
      case TrappedPiece(attackerSquare, targetSquare, attackerRole, targetRole) =>
        detail("TrappedPiece", s"attacker=${pieceAt(attackerRole, attackerSquare)}", s"target=${pieceAt(targetRole, targetSquare)}")
      case Domination(attackerSquare, targetSquare, attackerRole, targetRole, controlledEscapeSquares) =>
        detail(
          "Domination",
          s"attacker=${pieceAt(attackerRole, attackerSquare)}",
          s"target=${pieceAt(targetRole, targetSquare)}",
          s"escape_squares=${squares(controlledEscapeSquares)}"
        )
      case Zwischenzug(intermediateMove, expectedRecaptureSquare, checkingPieceSquare, checkingPieceRole, checkedKingSquare, threatType) =>
        detail(
          "Zwischenzug",
          s"intermediate_move=$intermediateMove",
          s"expected_recapture=${sq(expectedRecaptureSquare)}",
          s"checker=${pieceAt(checkingPieceRole, checkingPieceSquare)}",
          s"king=${sq(checkedKingSquare)}",
          s"threat=${threatType.toString}"
        )
      case Decoy(baitFromSquare, baitSquare, luredFromSquare, executionFromSquare, executionToSquare, baitRole, luredRole) =>
        detail(
          "Decoy",
          s"bait=${pieceAt(baitRole, baitFromSquare)}",
          s"bait_to=${sq(baitSquare)}",
          s"lured=${pieceAt(luredRole, luredFromSquare)}",
          s"execution=${sq(executionFromSquare)}-${sq(executionToSquare)}"
        )
      case XRay(attackerSquare, blockerSquare, targetSquare, attackerRole, blockerRole, targetRole) =>
        detail(
          "XRay",
          s"attacker=${pieceAt(attackerRole, attackerSquare)}",
          s"blocker=${pieceAt(blockerRole, blockerSquare)}",
          s"target=${pieceAt(targetRole, targetSquare)}"
        )
      case Clearance(beneficiarySquare, clearedSquare, targetSquare, beneficiaryRole, clearingTo) =>
        detail(
          "Clearance",
          s"beneficiary=${pieceAt(beneficiaryRole, beneficiarySquare)}",
          s"cleared=${sq(clearedSquare)}",
          s"clearing_to=${sq(clearingTo)}",
          s"target=${sq(targetSquare)}"
        )
      case Battery(frontSquare, backSquare, targetSquare, frontRole, backRole, axis) =>
        detail(
          "Battery",
          s"front=${pieceAt(frontRole, frontSquare)}",
          s"back=${pieceAt(backRole, backSquare)}",
          s"target=${sq(targetSquare)}",
          s"axis=${axis.toString}"
        )
      case Interference(blockerSquare, defenderSquare, targetSquare, blockerRole, defenderRole, targetRole) =>
        detail(
          "Interference",
          s"blocker=${pieceAt(blockerRole, blockerSquare)}",
          s"defender=${pieceAt(defenderRole, defenderSquare)}",
          s"target=${pieceAt(targetRole, targetSquare)}"
        )
      case Pin(attackerSquare, pinnedSquare, behindSquare, targetSquare, attackerRole, pinnedRole, behindRole, absolute) =>
        detail(
          "Pin",
          s"attacker=${pieceAt(attackerRole, attackerSquare)}",
          s"pinned=${pieceAt(pinnedRole, pinnedSquare)}",
          s"behind=${pieceAt(behindRole, behindSquare)}",
          s"target=${sq(targetSquare)}",
          s"absolute=$absolute"
        )
      case Skewer(attackerSquare, frontSquare, backSquare, targetSquare, attackerRole, frontRole, backRole) =>
        detail(
          "Skewer",
          s"attacker=${pieceAt(attackerRole, attackerSquare)}",
          s"front=${pieceAt(frontRole, frontSquare)}",
          s"back=${pieceAt(backRole, backSquare)}",
          s"target=${sq(targetSquare)}"
        )
      case StalemateTrap(stalematedKingSquare, resourceSquare, entryMove, terminalMove, scoreCp) =>
        detail(
          "StalemateTrap",
          s"king=${sq(stalematedKingSquare)}",
          s"resource=${sq(resourceSquare)}",
          s"entry_move=$entryMove",
          s"terminal_move=$terminalMove",
          scoreCp.fold("")(score => s"score_cp=$score")
        )
      case PerpetualCheck(checkedKingSquare, checkerSquares, checkingSide, entryMove, cycleStartMove, cycleReturnMove, _, scoreCp) =>
        detail(
          "PerpetualCheck",
          s"king=${sq(checkedKingSquare)}",
          s"checkers=${squares(checkerSquares)}",
          s"side=$checkingSide",
          s"entry_move=$entryMove",
          s"cycle=$cycleStartMove-$cycleReturnMove",
          scoreCp.fold("")(score => s"score_cp=$score")
        )

  private def detail(name: String, parts: String*): String =
    parts.map(_.trim).filter(_.nonEmpty).mkString(s"$name(", ",", ")")
  private def sq(square: EvidenceSquare): String =
    square.key.trim.toLowerCase
  private def piece(piece: EvidencePieceRole): String =
    piece.name.trim.toLowerCase
  private def pieceAt(pieceRole: EvidencePieceRole, square: EvidenceSquare): String =
    s"${piece(pieceRole)}@${sq(square)}"
  private def squares(values: List[EvidenceSquare]): String =
    values.map(sq).filter(_.nonEmpty).distinct.sorted.mkString("/")
  private def targetSummary(target: RelationWitnessTarget): String =
    pieceAt(target.role, target.square)

final case class RelationWitnessProof(
    sourceKind: String,
    detail: RelationWitnessDetail,
    focusSquares: List[EvidenceSquare],
    targetSquare: Option[EvidenceSquare],
    lineMoves: List[String],
    participants: List[RelationParticipant],
    proofAtoms: List[RelationProofAtom]
):
  def hasTypedDetail: Boolean =
    detail != RelationWitnessDetail.Empty
  def hasLineProof: Boolean =
    proofAtoms.exists(_.role == RelationProofAtomRole.LineMove)
  def detailName: String =
    detail.detailName
  def detailSummary: String =
    detail.detailSummary

object RelationWitnessProof:
  val empty: RelationWitnessProof =
    RelationWitnessProof(
      sourceKind = "unknown",
      detail = RelationWitnessDetail.Empty,
      focusSquares = Nil,
      targetSquare = None,
      lineMoves = Nil,
      participants = Nil,
      proofAtoms = Nil
    )

enum RelationFactKind:
  case DefenderTrade
  case BadPieceLiquidation
  case Overload
  case Deflection
  case DiscoveredAttack
  case DoubleCheck
  case BackRankMate
  case MateNet
  case Fork
  case HangingPiece
  case Decoy
  case Interference
  case Clearance
  case XRay
  case Battery
  case Pin
  case Skewer
  case Zwischenzug
  case Domination
  case TrappedPiece
  case GreekGift
  case StalemateTrap
  case PerpetualCheck

object RelationFactKind:
  private val byId: Map[String, RelationFactKind] =
    Map(
      "defender_trade" -> DefenderTrade,
      "bad_piece_liquidation" -> BadPieceLiquidation,
      "overload" -> Overload,
      "deflection" -> Deflection,
      "discovered_attack" -> DiscoveredAttack,
      "double_check" -> DoubleCheck,
      "back_rank_mate" -> BackRankMate,
      "mate_net" -> MateNet,
      "fork" -> Fork,
      "hanging_piece" -> HangingPiece,
      "decoy" -> Decoy,
      "interference" -> Interference,
      "clearance" -> Clearance,
      "xray" -> XRay,
      "battery" -> Battery,
      "pin" -> Pin,
      "skewer" -> Skewer,
      "zwischenzug" -> Zwischenzug,
      "domination" -> Domination,
      "trapped_piece" -> TrappedPiece,
      "greek_gift" -> GreekGift,
      "stalemate_trap" -> StalemateTrap,
      "perpetual_check" -> PerpetualCheck
    )

  def fromId(raw: String): Option[RelationFactKind] =
    byId.get(Option(raw).getOrElse("").trim.toLowerCase)

enum StrategicFactKind:
  case Outpost
  case FileControl
  case Space
  case CounterplayRestraint
  case TargetFixation
  case Structure
  case Endgame
  case Activity
  case Compensation
  case Practicality
  case PlanPressure

sealed trait EvidencePayload:
  def layer: EvidenceLayer

final case class BoardFactEvidence(
    private val facts: List[Fact],
    private val features: Option[PositionFeatures]
)(private val anchors: List[BoardAnchor] = Nil,
    private val attackDefense: List[BoardAttackDefenseEntry] = Nil,
    private val profile: Option[BoardPositionProfile] = None
) extends EvidencePayload:
  val layer: EvidenceLayer = EvidenceLayer.Board
  def boardAnchors: List[BoardAnchor] =
    anchors
  def boardAnchorCount: Int =
    anchors.size
  def hasBoardAnchors: Boolean =
    anchors.nonEmpty
  def anchorsOf(kind: BoardAnchorKind): List[BoardAnchor] =
    anchors.filter(_.kind == kind)
  def anchorsOfAny(kinds: Set[BoardAnchorKind]): List[BoardAnchor] =
    anchors.filter(anchor => kinds.contains(anchor.kind))
  def anchorsOfAtLeast(kind: BoardAnchorKind, minimumMagnitude: Int): List[BoardAnchor] =
    anchors.filter(anchor => anchor.kind == kind && anchor.magnitude >= minimumMagnitude)
  def semanticGroupingAnchors: List[EvidenceSemanticAnchor] =
    anchors.map(_.semanticGroupingAnchor)
  def proofSignalAnchors: List[BoardAnchor] =
    anchors.filter(BoardFactEvidence.isProofSignalAnchor)
  def proofSignalAnchorKinds: List[BoardAnchorKind] =
    proofSignalAnchors.map(_.kind).distinct
  def hasProofSignalAnchor(kind: BoardAnchorKind): Boolean =
    proofSignalAnchors.exists(_.kind == kind)
  def looseMaterialAnchors: List[BoardAnchor] =
    anchorsOf(BoardAnchorKind.LooseMaterial)
  def outpostAnchors: List[BoardAnchor] =
    anchorsOf(BoardAnchorKind.Outpost)
  def fileControlAnchors: List[BoardAnchor] =
    anchorsOf(BoardAnchorKind.FileControl)
  def spaceAnchors: List[BoardAnchor] =
    anchorsOf(BoardAnchorKind.Space)
  def activityAnchors: List[BoardAnchor] =
    anchorsOfAny(Set(BoardAnchorKind.Activity, BoardAnchorKind.CounterplayRestraint))
  def counterplayRestraintAnchors: List[BoardAnchor] =
    anchorsOfAtLeast(BoardAnchorKind.CounterplayRestraint, minimumMagnitude = 3)
  def endgameTechniqueAnchors: List[BoardAnchor] =
    anchorsOf(BoardAnchorKind.EndgameTechnique)
  def openingContextAnchors: List[BoardAnchor] =
    anchorsOfAny(
      Set(
        BoardAnchorKind.CenterControl,
        BoardAnchorKind.Space,
        BoardAnchorKind.Development,
        BoardAnchorKind.Activity,
        BoardAnchorKind.FileControl,
        BoardAnchorKind.BatteryPressure,
        BoardAnchorKind.WeakSquare,
        BoardAnchorKind.PawnStructure,
        BoardAnchorKind.KingSafety
      )
    )
  def anchorFocusSquares: List[EvidenceSquare] =
    anchors.flatMap(_.focusSquares).distinct
  def boardProfile: Option[BoardPositionProfile] =
    profile
  def factCount: Int =
    facts.size
  def hasBoardProfile: Boolean =
    profile.nonEmpty
  def hasAttackDefenseEntries: Boolean =
    attackDefense.nonEmpty
  def attackDefenseCount: Int =
    attackDefense.size
  def vulnerableAttackDefense: List[BoardAttackDefenseEntry] =
    attackDefense.filter(entry => entry.isLoose || entry.isUnderdefended)
  def targetHintSquares: List[EvidenceSquare] =
    val anchorSquares =
      anchors.flatMap(_.targetHintSquares)
    val materialSquares =
      vulnerableAttackDefense.map(_.square)
    (anchorSquares ++ materialSquares).distinct
  def lowLevelFacts: List[Fact] =
    facts

object BoardFactEvidence:
  def apply(facts: List[Fact], features: Option[PositionFeatures]): BoardFactEvidence =
    new BoardFactEvidence(facts, features)()

  private[chessjudgment] def isProofSignalAnchor(anchor: BoardAnchor): Boolean =
    anchor.kind match
      case BoardAnchorKind.LooseMaterial =>
        anchor.signal == BoardAnchorSignal.HangingPiece ||
          anchor.detail.exists(_.defenderSquares.isEmpty)
      case BoardAnchorKind.PinPressure =>
        anchor.detail.flatMap(_.isAbsolute).contains(true) || anchor.magnitude > 1
      case BoardAnchorKind.SkewerPressure | BoardAnchorKind.ForkPressure | BoardAnchorKind.XRayPressure |
          BoardAnchorKind.Outpost =>
        true
      case BoardAnchorKind.BatteryPressure =>
        anchor.detail.flatMap(_.axis).contains(BoardAnchorAxis.Diagonal)
      case BoardAnchorKind.CenterControl | BoardAnchorKind.Space | BoardAnchorKind.Development |
          BoardAnchorKind.FileControl | BoardAnchorKind.Activity | BoardAnchorKind.CounterplayRestraint |
          BoardAnchorKind.KingSafety | BoardAnchorKind.PawnStructure | BoardAnchorKind.WeakSquare |
          BoardAnchorKind.EndgameTechnique =>
        false

final case class BoardAttackDefenseEntry(
    square: EvidenceSquare,
    occupantColor: Color,
    occupantRole: EvidencePieceRole,
    attackerColor: Color,
    attackerSquares: List[EvidenceSquare],
    defenderSquares: List[EvidenceSquare],
    attackCount: Int,
    defenseCount: Int,
    pressureDelta: Int,
    materialValueCp: Int,
    isLoose: Boolean,
    isUnderdefended: Boolean
)

final case class BoardPositionProfile(
    centerLocked: Boolean,
    centerOpen: Boolean,
    pawnTensionCount: Int,
    whiteCenterControl: Int,
    blackCenterControl: Int,
    whiteCentralPawns: Int,
    blackCentralPawns: Int,
    spaceDiff: Int,
    whiteDevelopmentLag: Int,
    blackDevelopmentLag: Int,
    whiteLowMobilityPieces: Int,
    blackLowMobilityPieces: Int,
    whiteKingExposure: Int,
    blackKingExposure: Int,
    whitePawnWeaknesses: Int,
    blackPawnWeaknesses: Int,
    whitePassedPawns: Int,
    blackPassedPawns: Int,
    whiteEntrenchedPieces: Int,
    blackEntrenchedPieces: Int,
    whiteRookPawnMarchReady: Boolean,
    blackRookPawnMarchReady: Boolean,
    whiteHookCreationChance: Boolean,
    blackHookCreationChance: Boolean,
    whiteColorComplexClamp: Boolean,
    blackColorComplexClamp: Boolean,
    hasStrategicSnapshot: Boolean
):
  def centerControlEdgeFor(side: Color): Int =
    if side.white then whiteCenterControl - blackCenterControl
    else blackCenterControl - whiteCenterControl
  def centralPawnsFor(side: Color): Int =
    if side.white then whiteCentralPawns else blackCentralPawns
  def spaceFor(side: Color): Int =
    if side.white then spaceDiff else -spaceDiff
  def developmentLagFor(side: Color): Int =
    if side.white then whiteDevelopmentLag else blackDevelopmentLag
  def lowMobilityFor(side: Color): Int =
    if side.white then whiteLowMobilityPieces else blackLowMobilityPieces
  def kingExposureFor(side: Color): Int =
    if side.white then whiteKingExposure else blackKingExposure
  def opponentPawnWeaknessFor(side: Color): Int =
    if side.white then blackPawnWeaknesses else whitePawnWeaknesses
  def passedPawnsFor(side: Color): Int =
    if side.white then whitePassedPawns else blackPassedPawns
  def opponentPassedPawnsFor(side: Color): Int =
    if side.white then blackPassedPawns else whitePassedPawns
  def entrenchedPiecesFor(side: Color): Int =
    if side.white then whiteEntrenchedPieces else blackEntrenchedPieces
  def rookPawnReadyFor(side: Color): Boolean =
    if side.white then whiteRookPawnMarchReady else blackRookPawnMarchReady
  def hookChanceFor(side: Color): Boolean =
    if side.white then whiteHookCreationChance else blackHookCreationChance
  def colorComplexClampFor(side: Color): Boolean =
    if side.white then whiteColorComplexClamp else blackColorComplexClamp

enum BoardAnchorKind:
  case CenterControl
  case Space
  case Development
  case FileControl
  case Activity
  case CounterplayRestraint
  case KingSafety
  case PawnStructure
  case LooseMaterial
  case PinPressure
  case SkewerPressure
  case ForkPressure
  case XRayPressure
  case BatteryPressure
  case WeakSquare
  case Outpost
  case EndgameTechnique

enum BoardAnchorSignal:
  case CenterControlEdge
  case SpaceEdge
  case DevelopmentLead
  case OpenFileAccess
  case SemiOpenFileAccess
  case RookOnSeventh
  case MobilityEdge
  case OpponentLowMobility
  case KingExposure
  case KingPressure
  case PawnStructureShape
  case HangingPiece
  case AttackedTarget
  case AbsolutePin
  case RelativePin
  case SkewerLine
  case ForkTargets
  case XRayLine
  case BatteryLine
  case WeakSquareHole
  case OutpostSquare
  case EndgameKingActivity
  case EndgameOpposition
  case EndgameRuleOfSquare
  case EndgameTriangulation
  case EndgameRookPattern
  case EndgameOutcomeHint
  case EndgameZugzwang
  case EndgamePromotion
  case EndgameStalemateResource

enum BoardAnchorAxis:
  case File
  case Rank
  case Diagonal

final case class BoardAnchorDetail(
    subjectColor: Option[Color] = None,
    subjectSquare: Option[EvidenceSquare] = None,
    subjectRole: Option[EvidencePieceRole] = None,
    targetSquare: Option[EvidenceSquare] = None,
    targetRole: Option[EvidencePieceRole] = None,
    attackerColor: Option[Color] = None,
    attackerSquare: Option[EvidenceSquare] = None,
    attackerRole: Option[EvidencePieceRole] = None,
    attackerSquares: List[EvidenceSquare] = Nil,
    defenderSquares: List[EvidenceSquare] = Nil,
    relatedSquares: List[EvidenceSquare] = Nil,
    file: Option[EvidenceFile] = None,
    axis: Option[BoardAnchorAxis] = None,
    isAbsolute: Option[Boolean] = None,
    materialLossCp: Option[Int] = None,
    tags: List[String] = Nil
):
  def focusSquares: List[EvidenceSquare] =
    (
      subjectSquare.toList ++
        targetSquare.toList ++
        attackerSquare.toList ++
        attackerSquares ++
        defenderSquares ++
        relatedSquares
    ).distinct
  def targetHintSquares: List[EvidenceSquare] =
    (
      targetSquare.toList ++
        subjectSquare.toList ++
        relatedSquares
    ).distinct

final case class BoardAnchor(
    kind: BoardAnchorKind,
    side: Color,
    signal: BoardAnchorSignal,
    magnitude: Int,
    confidence: Double,
    detail: Option[BoardAnchorDetail] = None
):
  def focusSquares: List[EvidenceSquare] =
    detail.toList.flatMap(_.focusSquares).distinct
  def targetHintSquares: List[EvidenceSquare] =
    detail.toList.flatMap(_.targetHintSquares).distinct
  def semanticGroupingAnchor: EvidenceSemanticAnchor =
    val sideKey = if side.white then "white" else "black"
    val detailValues =
      detail.toList.flatMap(detail =>
        List(
          detail.subjectColor.map(color => s"subject-color:${colorKey(color)}"),
          detail.attackerColor.map(color => s"attacker-color:${colorKey(color)}"),
          detail.subjectSquare.map(square => s"subject-square:${square.key}"),
          detail.targetSquare.map(square => s"target-square:${square.key}"),
          Option
            .when(detail.relatedSquares.nonEmpty)(s"related:${detail.relatedSquares.map(_.key).sorted.mkString(",")}"),
          detail.file.map(file => s"file:${file.key}"),
          detail.axis.map(axis => s"axis:$axis")
        ).flatten ++ detail.tags
      )
    EvidenceSemanticAnchor.of(
      EvidenceSemanticAnchorKind.BoardAnchor,
      (List(sideKey, kind.toString, signal.toString) ++ detailValues)*
    )

  private def colorKey(color: Color): String =
    if color.white then "white" else "black"

final case class SinglePositionEvidence(
    assessment: SinglePositionAssessment
) extends EvidencePayload:
  val layer: EvidenceLayer = EvidenceLayer.SinglePosition

final case class PawnStructureFactEvidence(
    profile: StructureProfile,
    alignment: Option[PlanAlignment],
    pawnPlay: Option[PawnPlayAnalysis]
) extends EvidencePayload:
  val layer: EvidenceLayer = EvidenceLayer.PawnStructure

final case class StrategicFactEvidence(
    kind: StrategicFactKind,
    facts: List[Fact],
    relatedPlans: List[lila.chessjudgment.model.PlanId],
    confidence: Double
)(val boardAnchors: List[BoardAnchor] = Nil
) extends EvidencePayload:
  val layer: EvidenceLayer = EvidenceLayer.Strategic
  def hasTypedSupport: Boolean =
    facts.nonEmpty || relatedPlans.nonEmpty || boardAnchors.nonEmpty
  def semanticGroupingAnchors: List[EvidenceSemanticAnchor] =
    boardAnchors.map(_.semanticGroupingAnchor)

enum StrategicMechanismKind:
  case StructuralImprovement
  case TargetPressure
  case CenterControl
  case KingSafety
  case PawnWeakness
  case Activity
  case PawnStructure
  case PlanPressure
  case Compensation
  case Endgame
  case StrategicConcession
  case OpeningAlignment

enum StrategicMechanismSignalKind:
  case StrategicFact
  case PawnStructure
  case StructuralDelta
  case PlanPressure
  case PlanTransition
  case OpeningAnchor
  case OpeningApplicability
  case EndgamePosition

enum StrategicAxisKind:
  case Target
  case SpaceCenter
  case PawnBreak
  case Counterplay
  case Activity
  case PlanCoherence

enum StrategicAxisPolarity:
  case Gain
  case Loss
  case Preserve
  case Release
  case Concede
  case Restrain
  case Support

enum StrategicSustainabilityHorizon:
  case Immediate
  case ShortPv
  case MediumPv
  case LongPv
  case Unknown

enum StrategicAxisComparisonOutcome:
  case ReferenceOnly
  case CandidateOnly
  case ReferenceStronger
  case CandidateStronger
  case SharedSustained
  case CandidateConcession
  case ReferencePreservesPlan

final case class StrategicAxisDetail(
    kind: StrategicAxisKind,
    polarity: StrategicAxisPolarity,
    label: String
):
  def stableKey: String =
    s"$kind:$polarity:$label"

final case class StrategicMechanismSignal(
    kind: StrategicMechanismSignalKind,
    label: String,
    source: EvidenceRef,
    strength: Int,
    axis: Option[StrategicAxisDetail] = None
):
  def sourceLayer: EvidenceLayer = source.layer
  def axisKey: Option[String] =
    axis.map(_.stableKey)

final case class StrategicMechanismEvidence(
    kind: StrategicMechanismKind,
    signals: List[StrategicMechanismSignal],
    semanticAnchors: List[EvidenceSemanticAnchor]
) extends EvidencePayload:
  val layer: EvidenceLayer = EvidenceLayer.StrategicMechanism
  def signalKinds: Set[StrategicMechanismSignalKind] =
    signals.map(_.kind).toSet
  def hasSignals: Boolean =
    signals.nonEmpty
  def directStrength: Int =
    signals.map(_.strength).sum
  def hasCompositeSupport: Boolean =
    signals.size >= 2 || signalKinds.exists(kind =>
      kind == StrategicMechanismSignalKind.StructuralDelta ||
        kind == StrategicMechanismSignalKind.PawnStructure ||
        kind == StrategicMechanismSignalKind.PlanTransition ||
        kind == StrategicMechanismSignalKind.OpeningAnchor ||
        kind == StrategicMechanismSignalKind.EndgamePosition
    )
  def canAnchorStrategicIdea: Boolean =
    hasCompositeSupport &&
      kind != StrategicMechanismKind.OpeningAlignment &&
      (kind match
        case StrategicMechanismKind.StructuralImprovement | StrategicMechanismKind.StrategicConcession =>
          hasStrategicAxis
        case _ =>
          true
      )
  def canAnchorPawnStructureIdea: Boolean =
    kind == StrategicMechanismKind.PawnStructure && hasSignals
  def canAnchorOpeningIdea: Boolean =
    kind == StrategicMechanismKind.OpeningAlignment &&
      signalKinds.contains(StrategicMechanismSignalKind.OpeningApplicability)
  def canAnchorPlanIdea: Boolean =
    kind == StrategicMechanismKind.PlanPressure &&
      (
        hasCompositeSupport ||
          signalKinds.contains(StrategicMechanismSignalKind.PlanTransition)
      )
  def canSupportCompensation: Boolean =
    kind == StrategicMechanismKind.Compensation && hasSignals
  def canSupportStrategicCause: Boolean =
    canAnchorStrategicIdea || canAnchorPawnStructureIdea || canSupportCompensation
  def hasOpeningAnchorSignal: Boolean =
    signalKinds.contains(StrategicMechanismSignalKind.OpeningAnchor)
  def axisDetails: List[StrategicAxisDetail] =
    signals.flatMap(_.axis).distinctBy(_.stableKey)
  def hasStrategicAxis: Boolean =
    axisDetails.nonEmpty
  def hasAxis(kind: StrategicAxisKind, polarities: Set[StrategicAxisPolarity] = Set.empty): Boolean =
    axisDetails.exists(axis => axis.kind == kind && (polarities.isEmpty || polarities.contains(axis.polarity)))
  def hasTargetPressureGainAxis: Boolean =
    kind == StrategicMechanismKind.TargetPressure && canSupportStrategicCause &&
      hasAxis(StrategicAxisKind.Target, Set(StrategicAxisPolarity.Gain))
  def hasTargetPressureReleaseAxis: Boolean =
    kind == StrategicMechanismKind.TargetPressure &&
      hasAxis(StrategicAxisKind.Target, Set(StrategicAxisPolarity.Release))
  def hasCenterControlGainAxis: Boolean =
    kind == StrategicMechanismKind.CenterControl && canSupportStrategicCause &&
      hasAxis(StrategicAxisKind.SpaceCenter, Set(StrategicAxisPolarity.Gain))
  def hasKingSafetyConcessionAxis: Boolean =
    kind == StrategicMechanismKind.KingSafety && canSupportStrategicCause &&
      hasAxis(StrategicAxisKind.Counterplay, Set(StrategicAxisPolarity.Concede))
  def hasPawnWeaknessTargetAxis: Boolean =
    kind == StrategicMechanismKind.PawnWeakness && canSupportStrategicCause &&
      hasAxis(StrategicAxisKind.Target, Set(StrategicAxisPolarity.Gain))
  def hasActivityGainAxis: Boolean =
    kind == StrategicMechanismKind.Activity && canSupportStrategicCause &&
      hasAxis(StrategicAxisKind.Activity, Set(StrategicAxisPolarity.Gain))
  def hasActivityLossAxis: Boolean =
    kind == StrategicMechanismKind.Activity && canSupportStrategicCause &&
      hasAxis(StrategicAxisKind.Activity, Set(StrategicAxisPolarity.Loss))
  def hasPlanCoherenceAxis: Boolean =
    kind == StrategicMechanismKind.PlanPressure && canAnchorPlanIdea &&
      hasAxis(StrategicAxisKind.PlanCoherence)
  def hasStrategicConcessionAxis: Boolean =
    kind == StrategicMechanismKind.StrategicConcession && hasStrategicAxis
  def hasPassedPawnResourceSignal: Boolean =
    kind == StrategicMechanismKind.PawnStructure &&
      canAnchorPawnStructureIdea &&
      hasAnySignalLabel(Set("passed-pawn-progress", "promotion-pressure-gain"))
  def hasPassedPawnConcessionSignal: Boolean =
    kind == StrategicMechanismKind.StrategicConcession &&
      hasAnySignalLabel(Set("passed-pawn-concession", "promotion-pressure-concession"))
  private def hasAnySignalLabel(labels: Set[String]): Boolean =
    signals.exists(signal => labels.contains(signal.label))
  def semanticGroupingAnchors: List[EvidenceSemanticAnchor] =
    (semanticAnchors ++ signals.flatMap(_.axis).map(axis => EvidenceSemanticAnchor.of(EvidenceSemanticAnchorKind.StrategicAxis, axis.stableKey)))
      .distinctBy(_.stableKey)

final case class StrategicAxisComparison(
    axis: StrategicAxisDetail,
    outcome: StrategicAxisComparisonOutcome,
    referenceStrength: Int,
    candidateStrength: Int,
    referenceSources: List[EvidenceRef],
    candidateSources: List[EvidenceRef]
):
  def axisKey: String =
    axis.stableKey
  def sources: List[EvidenceRef] =
    (referenceSources ++ candidateSources).distinctBy(_.id)
  def hasContrast: Boolean =
    referenceStrength != candidateStrength ||
      outcome == StrategicAxisComparisonOutcome.ReferenceOnly ||
      outcome == StrategicAxisComparisonOutcome.CandidateOnly ||
      outcome == StrategicAxisComparisonOutcome.CandidateConcession ||
      outcome == StrategicAxisComparisonOutcome.ReferencePreservesPlan
  def candidateNegative: Boolean =
    (candidateStrength > 0 || outcome == StrategicAxisComparisonOutcome.CandidateConcession) &&
      (
        axis.polarity == StrategicAxisPolarity.Loss ||
          axis.polarity == StrategicAxisPolarity.Release ||
          axis.polarity == StrategicAxisPolarity.Concede ||
          outcome == StrategicAxisComparisonOutcome.CandidateConcession
      )
  def referenceLead: Boolean =
    outcome == StrategicAxisComparisonOutcome.ReferenceOnly ||
      outcome == StrategicAxisComparisonOutcome.ReferenceStronger ||
      outcome == StrategicAxisComparisonOutcome.ReferencePreservesPlan
  def candidateLead: Boolean =
    outcome == StrategicAxisComparisonOutcome.CandidateOnly ||
      outcome == StrategicAxisComparisonOutcome.CandidateStronger ||
      outcome == StrategicAxisComparisonOutcome.CandidateConcession

final case class StrategicPlanComparison(
    referencePlanIds: List[String],
    candidatePlanIds: List[String],
    outcome: StrategicAxisComparisonOutcome
):
  def hasPlanDelta: Boolean =
    referencePlanIds.sorted != candidatePlanIds.sorted ||
      outcome == StrategicAxisComparisonOutcome.ReferencePreservesPlan

final case class StrategicSustainabilityAssessment(
    horizon: StrategicSustainabilityHorizon,
    lineMaintained: Boolean,
    pvMaintained: Boolean,
    referencePlyCount: Int,
    candidatePlyCount: Int
):
  def hasSustainedPv: Boolean =
    pvMaintained &&
      (horizon == StrategicSustainabilityHorizon.ShortPv ||
        horizon == StrategicSustainabilityHorizon.MediumPv ||
        horizon == StrategicSustainabilityHorizon.LongPv)

final case class StrategicContrastSupport(
    directSources: List[EvidenceRef],
    contrastSources: List[EvidenceRef],
    contextSources: List[EvidenceRef]
):
  def all: List[EvidenceRef] =
    (directSources ++ contrastSources ++ contextSources).distinctBy(_.id)

final case class StrategicMechanismContrastEvidence(
    comparisonKind: CandidateComparisonKind,
    referenceLine: LineNodeRef,
    candidateLine: LineNodeRef,
    axisComparisons: List[StrategicAxisComparison],
    planComparison: Option[StrategicPlanComparison],
    sustainability: StrategicSustainabilityAssessment,
    support: StrategicContrastSupport
) extends EvidencePayload:
  val layer: EvidenceLayer = EvidenceLayer.StrategicMechanism
  def actionableComparisons: List[StrategicAxisComparison] =
    axisComparisons.filter(_.hasContrast)
  def hasActionableContrast: Boolean =
    actionableComparisons.nonEmpty || planComparison.exists(_.hasPlanDelta)
  def sourceRefs: List[EvidenceRef] =
    support.all
  def axisKeys: List[String] =
    axisComparisons.map(_.axisKey).distinct.sorted

object StrategicMechanismContrastEvidence:
  private[chessjudgment] def hasActionableContrastOrSameRootCarrier(
      payload: StrategicMechanismContrastEvidence,
      records: List[EvidenceRecord]
  ): Boolean =
    hasActionableContrastOrSameRootCarrier(
      records.collectFirst {
        case EvidenceRecord(_, CandidateComparisonEvidence(fact), _)
            if fact.kind == payload.comparisonKind &&
              fact.referenceLine == payload.referenceLine &&
              fact.candidateLine == payload.candidateLine =>
          fact
      },
      payload,
      records
    )

  private[chessjudgment] def hasActionableContrastOrSameRootCarrier(
      fact: CandidateComparisonFact,
      payload: StrategicMechanismContrastEvidence,
      records: List[EvidenceRecord]
  ): Boolean =
    hasActionableContrastOrSameRootCarrier(Some(fact), payload, records)

  private[chessjudgment] def hasActionableContrastOrSameRootCarrier(
      fact: Option[CandidateComparisonFact],
      payload: StrategicMechanismContrastEvidence,
      records: List[EvidenceRecord]
  ): Boolean =
    payload.hasActionableContrast ||
      fact.exists(exactSameRootConcreteCarrierContrast(_, payload, records))

  private[chessjudgment] def exactSameRootConcreteCarrierContrast(
      fact: CandidateComparisonFact,
      payload: StrategicMechanismContrastEvidence,
      records: List[EvidenceRecord]
  ): Boolean =
    fact.kind == CandidateComparisonKind.PlayedVsBest &&
      fact.comparison.verdict == MoveChoiceVerdict.MatchesReference &&
      JudgmentSubjectBinding.normalizeMove(fact.referenceLine.rootMove) == JudgmentSubjectBinding.normalizeMove(fact.candidateLine.rootMove) &&
      payload.axisComparisons.exists(comparison =>
        comparison.outcome == StrategicAxisComparisonOutcome.SharedSustained &&
          exactSameRootConcreteCarrierCauseKindsForAxis(fact.candidateLine, records, comparison.axis).nonEmpty
      )

  private[chessjudgment] def exactSameRootConcreteCarrierCauseKindsForAxis(
      candidateLine: LineNodeRef,
      records: List[EvidenceRecord],
      axis: StrategicAxisDetail
  ): List[RelativeCauseKind] =
    if axis.kind == StrategicAxisKind.Counterplay &&
        axis.polarity == StrategicAxisPolarity.Restrain &&
        currentMoveCounterBreakAxis(axis) &&
        currentMoveCounterplayRestraintCarrier(candidateLine, records)
    then List(RelativeCauseKind.OpponentRestriction)
    else if axis.kind == StrategicAxisKind.Target &&
        axis.polarity == StrategicAxisPolarity.Gain &&
        currentMoveConcreteTargetCarrierRecords(candidateLine, records).nonEmpty
    then
      if weakTargetAxis(axis) then List(RelativeCauseKind.PawnWeaknessTarget)
      else List(RelativeCauseKind.TargetPressureGain)
    else if currentMoveActivityValueAxis(axis) &&
        currentMoveConcreteActivityCarrierRecords(candidateLine, records).nonEmpty
    then List(RelativeCauseKind.ActivityGain)
    else if currentMovePlanCoherenceAxis(axis) &&
        currentMoveConcretePlanCarrierRecords(candidateLine, records).nonEmpty
    then List(RelativeCauseKind.PlanImprovement)
    else Nil

  private[chessjudgment] def currentMoveActivityValueAxis(axis: StrategicAxisDetail): Boolean =
    axis.kind == StrategicAxisKind.Activity &&
      (axis.polarity == StrategicAxisPolarity.Gain || axis.polarity == StrategicAxisPolarity.Support)

  private[chessjudgment] def currentMovePlanCoherenceAxis(axis: StrategicAxisDetail): Boolean =
    axis.kind == StrategicAxisKind.PlanCoherence &&
      (axis.polarity == StrategicAxisPolarity.Gain ||
        axis.polarity == StrategicAxisPolarity.Support ||
        axis.polarity == StrategicAxisPolarity.Preserve)

  private[chessjudgment] def currentMoveConcreteActivityCarrierRecords(
      candidateLine: LineNodeRef,
      records: List[EvidenceRecord]
  ): List[EvidenceRecord] =
    records.collect {
      case record @ EvidenceRecord(ref, payload: StructuralDeltaEvidence, _)
          if ref.scope == EvidenceScope.PlayedTransition &&
            payload.line.contains(candidateLine) &&
            payload.role == TransitionEdgeRole.Played &&
            JudgmentSubjectBinding.normalizeMove(payload.moveUci) == JudgmentSubjectBinding.normalizeMove(candidateLine.rootMove) &&
            currentMoveConcreteActivitySource(ref, records) =>
        record
    }.distinctBy(_.ref.id)

  private[chessjudgment] def currentMoveConcreteTargetCarrierRecords(
      candidateLine: LineNodeRef,
      records: List[EvidenceRecord]
  ): List[EvidenceRecord] =
    records.collect {
      case record @ EvidenceRecord(_, payload: StructuralDeltaEvidence, _)
          if payload.line.contains(candidateLine) &&
            payload.role == TransitionEdgeRole.Played &&
            JudgmentSubjectBinding.normalizeMove(payload.moveUci) == JudgmentSubjectBinding.normalizeMove(candidateLine.rootMove) &&
            payload.consequences.exists(currentMoveTargetCarrierConsequence) =>
        record
    }.distinctBy(_.ref.id)

  private[chessjudgment] def currentMovePlanAnchorCarrierRecords(
      candidateLine: LineNodeRef,
      records: List[EvidenceRecord]
  ): List[EvidenceRecord] =
    records.collect {
      case record @ EvidenceRecord(ref, payload: StructuralDeltaEvidence, _)
          if ref.scope == EvidenceScope.PlayedTransition &&
            payload.line.contains(candidateLine) &&
            payload.role == TransitionEdgeRole.Played &&
            JudgmentSubjectBinding.normalizeMove(payload.moveUci) == JudgmentSubjectBinding.normalizeMove(candidateLine.rootMove) &&
            payload.hasPositivePlanAnchor =>
        record
    }.distinctBy(_.ref.id)

  private[chessjudgment] def currentMoveConcretePlanCarrierRecords(
      candidateLine: LineNodeRef,
      records: List[EvidenceRecord]
  ): List[EvidenceRecord] =
    (
      currentMoveConcreteActivityCarrierRecords(candidateLine, records) ++
        currentMoveSameRootBreakCarrierRecords(candidateLine, records) ++
        currentMovePlanAnchorCarrierRecords(candidateLine, records) ++
        currentMoveFlankPawnAdvanceCarrierRecords(candidateLine, records)
    ).distinctBy(_.ref.id)

  private def currentMoveFlankPawnAdvanceCarrierRecords(
      candidateLine: LineNodeRef,
      records: List[EvidenceRecord]
  ): List[EvidenceRecord] =
    records.collect {
      case record @ EvidenceRecord(ref, payload: MoveMotifEvidence, _)
          if payload.recordLineBound(ref) &&
            ref.line.contains(candidateLine) &&
            JudgmentSubjectBinding.normalizeMove(payload.moveUci) == JudgmentSubjectBinding.normalizeMove(candidateLine.rootMove) &&
            currentMoveFlankPawnAdvance(payload.motif) =>
        record
    }.distinctBy(_.ref.id)

  private def currentMoveFlankPawnAdvance(motif: Motif): Boolean =
    motif match
      case Motif.PawnAdvance(file, _, _, _, _, _) =>
        file == File.A || file == File.B || file == File.G || file == File.H
      case _ =>
        false

  private[chessjudgment] def currentMoveConcreteActivitySource(
      source: EvidenceRef,
      records: List[EvidenceRecord]
  ): Boolean =
    records.exists {
      case EvidenceRecord(ref, payload: StructuralDeltaEvidence, _) if ref.id == source.id =>
        payload.consequencesOf(TransitionConsequenceKind.BatteryPressureGain).exists(consequence =>
          consequence.subjects.exists(currentMoveDiagonalBatterySubject)
        ) ||
          payload.consequencesOf(TransitionConsequenceKind.DevelopmentPieceActivated).exists(consequence =>
            consequence.subjects.exists(currentMoveDevelopmentRouteSubject)
          ) ||
          payload.consequencesOf(TransitionConsequenceKind.DevelopmentCenterControlGain).exists(consequence =>
            consequence.subjects.exists(currentMoveDevelopmentRouteSubject)
          ) ||
          payload.consequencesOf(TransitionConsequenceKind.MobilityGain).exists(consequence =>
            consequence.subjects.exists(currentMoveDevelopmentRouteSubject)
          ) ||
          (
            !payload.hasTargetPressureGain &&
              !payload.hasConsequence(TransitionConsequenceKind.WeakPawnTargetCreated) &&
              !payload.hasConsequence(TransitionConsequenceKind.WeakSquareTargetCreated) &&
              payload.consequencesOf(TransitionConsequenceKind.LineUnlockGain).exists(consequence =>
                consequence.subjects.exists(subject =>
                  val normalized = Option(subject).getOrElse("").trim.toLowerCase
                  normalized.contains("line-unlock") && normalized.matches(".*[a-h][1-8].*")
                )
              )
          ) ||
          payload.consequencesOf(TransitionConsequenceKind.OutpostGain).exists(consequence =>
            consequence.subjects.exists(currentMoveOutpostSubject)
          )
      case _ =>
        false
    }

  private def currentMoveDiagonalBatterySubject(subject: String): Boolean =
    val normalized = Option(subject).getOrElse("").trim.toLowerCase
    normalized.startsWith("battery:diagonal:")

  private def currentMoveDevelopmentRouteSubject(subject: String): Boolean =
    val normalized = Option(subject).getOrElse("").trim.toLowerCase
    normalized.matches(".*\\b(king|queen|rook|bishop|knight):[a-h][1-8]-[a-h][1-8].*")

  private def currentMoveOutpostSubject(subject: String): Boolean =
    val normalized = Option(subject).getOrElse("").trim.toLowerCase
    normalized.matches("outpost:(king|queen|rook|bishop|knight):[a-h][1-8]")

  private def weakTargetAxis(axis: StrategicAxisDetail): Boolean =
    val normalized = axis.label.toLowerCase
    normalized.contains("weak-pawn") || normalized.contains("weak-square")

  private def currentMoveTargetCarrierConsequence(consequence: TransitionConsequence): Boolean =
    (
      consequence.kind == TransitionConsequenceKind.TargetPressureGain ||
        consequence.kind == TransitionConsequenceKind.KingSafetyPressure ||
        consequence.kind == TransitionConsequenceKind.KingRingPressureGain ||
        consequence.kind == TransitionConsequenceKind.WeakPawnTargetCreated ||
        consequence.kind == TransitionConsequenceKind.WeakSquareTargetCreated
    ) &&
      consequence.subjects.exists(currentMoveConcreteTargetSubject)

  private def currentMoveConcreteTargetSubject(subject: String): Boolean =
    val normalized = Option(subject).getOrElse("").trim.toLowerCase
    normalized.matches(".*[a-h][1-8].*") ||
      normalized.startsWith("file:") ||
      normalized.startsWith("weak-pawn:") ||
      normalized.startsWith("weak-square:")

  private[chessjudgment] def currentMoveCounterBreakAxis(axis: StrategicAxisDetail): Boolean =
    axis.label.toLowerCase.startsWith("defensive-counter-break-")

  private[chessjudgment] def currentMoveSameRootBreakCarrier(
      candidateLine: LineNodeRef,
      records: List[EvidenceRecord]
  ): Boolean =
    currentMoveSameRootBreakCarrierRecords(candidateLine, records).nonEmpty

  private[chessjudgment] def currentMoveCounterplayRestraintCarrier(
      candidateLine: LineNodeRef,
      records: List[EvidenceRecord]
  ): Boolean =
    currentMoveSameRootBreakCarrier(candidateLine, records) ||
      currentMoveConcreteActivityCarrierRecords(candidateLine, records).nonEmpty

  private[chessjudgment] def currentMoveSameRootBreakCarrierRecords(
      candidateLine: LineNodeRef,
      records: List[EvidenceRecord]
  ): List[EvidenceRecord] =
    records.collect {
      case record @ EvidenceRecord(_, payload: StructuralDeltaEvidence, _)
          if payload.line.contains(candidateLine) &&
            payload.role == TransitionEdgeRole.Played &&
            JudgmentSubjectBinding.normalizeMove(payload.moveUci) == JudgmentSubjectBinding.normalizeMove(candidateLine.rootMove) &&
            payload.consequences.exists(currentMoveBreakCarrierConsequence) =>
        record
    }.distinctBy(_.ref.id)

  private def currentMoveBreakCarrierConsequence(consequence: TransitionConsequence): Boolean =
    (
      consequence.kind == TransitionConsequenceKind.PawnTensionGain ||
        consequence.kind == TransitionConsequenceKind.PawnTensionResolution
    ) &&
      consequence.subjects.exists(subject =>
        val normalized = Option(subject).getOrElse("").trim.toLowerCase
        normalized.startsWith("break-file:") ||
          normalized.startsWith("created-tension:") ||
          normalized.startsWith("resolved-tension:")
      )

object StrategicMechanismEvidence:
  def rawStrategicSourceLayer(layer: EvidenceLayer): Boolean =
    layer match
      case EvidenceLayer.Strategic | EvidenceLayer.PawnStructure | EvidenceLayer.StructuralDelta |
          EvidenceLayer.PlanPressure | EvidenceLayer.PlanTransition | EvidenceLayer.FeatureAnchor |
          EvidenceLayer.ApplicabilityAssessment | EvidenceLayer.OpeningContext =>
        true
      case _ =>
        false

  def openingClaimSupported(records: List[EvidenceRecord]): Boolean =
    val mechanisms = records.collect { case EvidenceRecord(_, payload: StrategicMechanismEvidence, _) => payload }
    mechanisms.exists(_.canAnchorOpeningIdea)

  def sourceMechanisms(record: EvidenceRecord): List[(StrategicMechanismKind, StrategicMechanismSignal)] =
    record.payload match
      case payload @ StrategicFactEvidence(kind, _, _, confidence) if confidence >= 0.35 && payload.hasTypedSupport =>
        val mechanism =
          kind match
            case StrategicFactKind.TargetFixation | StrategicFactKind.CounterplayRestraint =>
              StrategicMechanismKind.TargetPressure
            case StrategicFactKind.Space =>
              StrategicMechanismKind.CenterControl
            case StrategicFactKind.Structure =>
              StrategicMechanismKind.PawnStructure
            case StrategicFactKind.Activity | StrategicFactKind.Outpost | StrategicFactKind.FileControl =>
              StrategicMechanismKind.Activity
            case StrategicFactKind.Compensation =>
              StrategicMechanismKind.Compensation
            case StrategicFactKind.Endgame =>
              StrategicMechanismKind.Endgame
            case StrategicFactKind.PlanPressure =>
              StrategicMechanismKind.PlanPressure
            case StrategicFactKind.Practicality =>
              StrategicMechanismKind.StructuralImprovement
        List(
          mechanism -> signal(
            StrategicMechanismSignalKind.StrategicFact,
            strategicFactSignalLabel(payload),
            record.ref,
            math.round(confidence * 5).toInt.max(1),
            concreteAxis(record, strategicFactAxis(payload))
          )
        )
      case payload: PawnStructureFactEvidence if pawnStructureCanAnchorPlan(payload) =>
        val label = payload.profile.primary.toString
        val axis =
          payload.pawnPlay.flatMap(pawnPlayAxis).orElse(
            payload.alignment.map(_ => StrategicAxisDetail(StrategicAxisKind.PlanCoherence, StrategicAxisPolarity.Support, label))
          )
        List(StrategicMechanismKind.PawnStructure -> signal(StrategicMechanismSignalKind.PawnStructure, label, record.ref, 2, concreteAxis(record, axis)))
      case payload: StructuralDeltaEvidence if payload.hasTypedOutput =>
        List(
          Option.when(payload.hasStructuralAnchor)(
            StrategicMechanismKind.StructuralImprovement -> signal(
              StrategicMechanismSignalKind.StructuralDelta,
              "structural-improvement",
              record.ref,
              payload.structuralImprovementScore.max(1)
            )
          ),
          Option.when(payload.hasTargetPressureGain)(
            StrategicMechanismKind.TargetPressure -> signal(
              StrategicMechanismSignalKind.StructuralDelta,
              "target-pressure-gain",
              record.ref,
              3,
              concreteAxis(record, structuralDeltaAxis(StrategicAxisKind.Target, StrategicAxisPolarity.Gain, "target-pressure-gain"))
            )
          ),
          Option.when(payload.hasTargetPressureRelease)(
            StrategicMechanismKind.TargetPressure -> signal(
              StrategicMechanismSignalKind.StructuralDelta,
              "target-pressure-release",
              record.ref,
              2,
              concreteAxis(record, structuralDeltaAxis(StrategicAxisKind.Target, StrategicAxisPolarity.Release, "target-pressure-release"))
            )
          ),
          Option.when(payload.hasCenterControlGain)(
            StrategicMechanismKind.CenterControl -> signal(
              StrategicMechanismSignalKind.StructuralDelta,
              "center-control-gain",
              record.ref,
              2,
              concreteAxis(record, structuralDeltaAxis(StrategicAxisKind.SpaceCenter, StrategicAxisPolarity.Gain, "center-control-gain"))
            )
          ),
          Option.when(payload.hasAnyConsequence(Set(TransitionConsequenceKind.KingSafetyPressure, TransitionConsequenceKind.KingRingPressureGain)))(
            StrategicMechanismKind.TargetPressure -> signal(
              StrategicMechanismSignalKind.StructuralDelta,
              "king-safety-pressure",
              record.ref,
              3,
              concreteAxis(record, structuralDeltaAxis(StrategicAxisKind.Target, StrategicAxisPolarity.Gain, "king-safety-pressure"))
            )
          ),
          Option.when(payload.hasAnyConsequence(Set(TransitionConsequenceKind.KingSafetyConcession, TransitionConsequenceKind.KingRingPressureConcession)))(
            StrategicMechanismKind.KingSafety -> signal(
              StrategicMechanismSignalKind.StructuralDelta,
              "king-safety-concession",
              record.ref,
              3,
              concreteAxis(record, structuralDeltaAxis(StrategicAxisKind.Counterplay, StrategicAxisPolarity.Concede, "king-safety-concession"))
            )
          ),
          Option.when(payload.hasConsequence(TransitionConsequenceKind.WeakPawnTargetCreated))(
            StrategicMechanismKind.PawnWeakness -> signal(
              StrategicMechanismSignalKind.StructuralDelta,
              "weak-pawn-target",
              record.ref,
              2,
              concreteAxis(record, structuralDeltaAxis(StrategicAxisKind.Target, StrategicAxisPolarity.Gain, "weak-pawn-target"))
            )
          ),
          Option.when(payload.hasPieceActivityGain)(
            StrategicMechanismKind.Activity -> signal(
              StrategicMechanismSignalKind.StructuralDelta,
              "activity-gain",
              record.ref,
              2,
              concreteAxis(record, structuralDeltaAxis(StrategicAxisKind.Activity, StrategicAxisPolarity.Gain, "activity-gain"))
            )
          ),
          Option.when(payload.hasBatteryPressureGain)(
            StrategicMechanismKind.Activity -> signal(
              StrategicMechanismSignalKind.StructuralDelta,
              "battery-pressure-gain",
              record.ref,
              3,
              concreteAxis(record, structuralDeltaAxis(StrategicAxisKind.Activity, StrategicAxisPolarity.Gain, "battery-pressure-gain"))
            )
          ),
          Option.when(payload.hasOutpostGain)(
            StrategicMechanismKind.Activity -> signal(
              StrategicMechanismSignalKind.StructuralDelta,
              "outpost-gain",
              record.ref,
              3,
              concreteAxis(record, structuralDeltaAxis(StrategicAxisKind.Activity, StrategicAxisPolarity.Gain, "outpost-gain"))
            )
          ),
          Option.when(payload.hasOpponentMobilityRestriction)(
            StrategicMechanismKind.CenterControl -> signal(
              StrategicMechanismSignalKind.StructuralDelta,
              "opponent-diagonal-restriction",
              record.ref,
              3,
              concreteAxis(
                record,
                structuralDeltaAxis(StrategicAxisKind.Counterplay, StrategicAxisPolarity.Restrain, "opponent-diagonal-restriction")
              )
            )
          ),
          Option.when(
            payload.hasAnyConsequence(
              Set(
                TransitionConsequenceKind.DevelopmentLagIncreased,
                TransitionConsequenceKind.DevelopmentPieceRetreated,
                TransitionConsequenceKind.DevelopmentMobilityLoss,
                TransitionConsequenceKind.DevelopmentCenterControlLoss,
                TransitionConsequenceKind.DevelopmentUnsafePlacement,
                TransitionConsequenceKind.MobilityLoss,
                TransitionConsequenceKind.FileAccessLoss
              )
            )
          )(
            StrategicMechanismKind.Activity -> signal(
              StrategicMechanismSignalKind.StructuralDelta,
              "activity-loss",
              record.ref,
              2,
              concreteAxis(record, structuralDeltaAxis(StrategicAxisKind.Activity, StrategicAxisPolarity.Loss, "activity-loss"))
            )
          ),
          Option.when(payload.hasConsequence(TransitionConsequenceKind.OutpostConcession))(
            StrategicMechanismKind.Activity -> signal(
              StrategicMechanismSignalKind.StructuralDelta,
              "outpost-concession",
              record.ref,
              3,
              concreteAxis(record, structuralDeltaAxis(StrategicAxisKind.Activity, StrategicAxisPolarity.Loss, "outpost-concession"))
            )
          ),
          Option.when(payload.hasPassedPawnProgress)(
            StrategicMechanismKind.PawnStructure -> signal(StrategicMechanismSignalKind.StructuralDelta, "passed-pawn-progress", record.ref, 3)
          ),
          Option.when(payload.hasConsequence(TransitionConsequenceKind.PromotionPressureGain))(
            StrategicMechanismKind.PawnStructure -> signal(StrategicMechanismSignalKind.StructuralDelta, "promotion-pressure-gain", record.ref, 3)
          ),
          Option.when(payload.hasConsequence(TransitionConsequenceKind.PassedPawnConcession))(
            StrategicMechanismKind.StrategicConcession -> signal(StrategicMechanismSignalKind.StructuralDelta, "passed-pawn-concession", record.ref, 3)
          ),
          Option.when(payload.hasConsequence(TransitionConsequenceKind.PromotionPressureConcession))(
            StrategicMechanismKind.StrategicConcession -> signal(StrategicMechanismSignalKind.StructuralDelta, "promotion-pressure-concession", record.ref, 3)
          ),
          Option.when(payload.hasStrategicConcession)(
            StrategicMechanismKind.StrategicConcession -> signal(
              StrategicMechanismSignalKind.StructuralDelta,
              "strategic-concession",
              record.ref,
              3,
              concreteAxis(record, structuralDeltaAxis(StrategicAxisKind.PlanCoherence, StrategicAxisPolarity.Concede, "strategic-concession"))
            )
          )
        ).flatten ++ structuralPawnBreakSignals(record, payload)
      case PlanPressureEvidence(scoring, activePlans) if planPressureHasDirectEvidence(scoring, activePlans) =>
        val plans = activePlans.primary :: activePlans.secondary.toList
        List(
          StrategicMechanismKind.PlanPressure ->
            signal(
              StrategicMechanismSignalKind.PlanPressure,
              plans.map(_.plan.id.toString).mkString(","),
              record.ref,
              2,
              concreteAxis(record, Some(StrategicAxisDetail(StrategicAxisKind.PlanCoherence, StrategicAxisPolarity.Support, plans.map(_.plan.id.toString).mkString(","))))
            )
        )
      case PlanTransitionEvidence(transition) if planTransitionCanSupportPlan(transition) =>
        transition.primaryPlanId.toList.map(planId =>
          StrategicMechanismKind.PlanPressure ->
            signal(
              StrategicMechanismSignalKind.PlanTransition,
              planId,
              record.ref,
              2,
              concreteAxis(record, Some(StrategicAxisDetail(StrategicAxisKind.PlanCoherence, StrategicAxisPolarity.Preserve, planId)))
            )
        )
      case FeatureAnchorEvidence(anchor) if anchor.hasPositiveStrength && anchor.canCorroborateOpeningPrior =>
        val mechanism =
          anchor.theme match
            case OpeningTheme.CenterControl    => StrategicMechanismKind.CenterControl
            case OpeningTheme.Development      => StrategicMechanismKind.Activity
            case OpeningTheme.PawnStructure    => StrategicMechanismKind.PawnStructure
            case OpeningTheme.GambitInitiative => StrategicMechanismKind.Compensation
            case OpeningTheme.KingSafety       => StrategicMechanismKind.KingSafety
            case OpeningTheme.PlanPressure     => StrategicMechanismKind.TargetPressure
        List(
          mechanism -> signal(
            StrategicMechanismSignalKind.OpeningAnchor,
            s"${anchor.theme}:${anchor.signal}",
            record.ref,
            math.round(anchor.strength * 4).toInt.max(1),
            concreteAxis(record, openingAnchorAxis(anchor.theme, anchor.signal.toString))
          )
        )
      case ApplicabilityAssessmentEvidence(assessment) if assessment.canCertifyOpeningClaim =>
        List(
          StrategicMechanismKind.OpeningAlignment -> signal(
            StrategicMechanismSignalKind.OpeningApplicability,
            assessment.supportedThemes.map(_.toString).sorted.mkString(","),
            record.ref,
            2
          )
        )
      case SinglePositionEvidence(assessment) if assessment.gamePhase.isEndgame && assessment.simplifyBias.shouldSimplify =>
        List(StrategicMechanismKind.Endgame -> signal(StrategicMechanismSignalKind.EndgamePosition, "simplify-endgame", record.ref, 2))
      case payload: BoardFactEvidence if payload.endgameTechniqueAnchors.nonEmpty =>
        List(StrategicMechanismKind.Endgame -> signal(StrategicMechanismSignalKind.EndgamePosition, "endgame-technique", record.ref, 2))
      case _ =>
        Nil

  def sourceSemanticAnchors(record: EvidenceRecord): List[EvidenceSemanticAnchor] =
    record.payload match
      case payload @ StrategicFactEvidence(kind, _, relatedPlans, confidence) if confidence >= 0.35 && payload.hasTypedSupport =>
        EvidenceSemanticAnchor.of(EvidenceSemanticAnchorKind.StrategicKind, kind.toString) ::
          relatedPlans.map(plan => EvidenceSemanticAnchor.of(EvidenceSemanticAnchorKind.Plan, plan.toString)) ++
          payload.semanticGroupingAnchors
      case PawnStructureFactEvidence(profile, alignment, pawnPlay) =>
        (
          Option.when(profile.primary != StructureId.Unknown)(
            EvidenceSemanticAnchor.of(EvidenceSemanticAnchorKind.PawnStructure, profile.primary.toString)
          ).toList ++
            pawnStructureGenericAnchors(profile.primary) ++
            alignment.toList.map(alignment =>
            EvidenceSemanticAnchor.of(
              EvidenceSemanticAnchorKind.StructurePlan,
              alignment.band.toString,
              alignment.matchedPlanIds.sorted.mkString(",")
            )
          ) ++
            pawnPlay.toList.flatMap(pawnPlaySemanticAnchors)
        ).distinct
      case FeatureAnchorEvidence(anchor) =>
        List(EvidenceSemanticAnchor.of(EvidenceSemanticAnchorKind.OpeningAnchor, anchor.theme.toString, anchor.signal.toString))
      case ApplicabilityAssessmentEvidence(assessment) =>
        assessment.supportedThemes.map(theme => EvidenceSemanticAnchor.of(EvidenceSemanticAnchorKind.OpeningSupported, theme.toString)) ++
          assessment.observedThemes.map(theme => EvidenceSemanticAnchor.of(EvidenceSemanticAnchorKind.OpeningObserved, theme.toString))
      case PlanPressureEvidence(_, activePlans) =>
        (activePlans.primary :: activePlans.secondary.toList).map(plan =>
          EvidenceSemanticAnchor.of(EvidenceSemanticAnchorKind.PlanPressure, plan.plan.id.toString)
        )
      case PlanTransitionEvidence(transition) =>
        transition.primaryPlanId.map(plan => EvidenceSemanticAnchor.of(EvidenceSemanticAnchorKind.PlanTransition, plan)).toList
      case payload: StructuralDeltaEvidence =>
        (
          payload.signalAnchors.map(anchor => EvidenceSemanticAnchor.of(EvidenceSemanticAnchorKind.StructuralDelta, s"signal:$anchor")) ++
            payload.consequenceAnchors.map(anchor => EvidenceSemanticAnchor.of(EvidenceSemanticAnchorKind.StructuralDelta, s"consequence:$anchor"))
        ).distinct
      case payload: BoardFactEvidence =>
        payload.semanticGroupingAnchors
      case _ =>
        Nil

  private def pawnStructureGenericAnchors(structure: StructureId): List[EvidenceSemanticAnchor] =
    structure match
      case StructureId.Carlsbad =>
        List(EvidenceSemanticAnchor.of(EvidenceSemanticAnchorKind.PawnStructure, "carlsbad"))
      case StructureId.IQPWhite | StructureId.IQPBlack =>
        List(EvidenceSemanticAnchor.of(EvidenceSemanticAnchorKind.PawnStructure, "iqp"))
      case _ =>
        Nil

  private def pawnPlaySemanticAnchors(play: PawnPlayAnalysis): List[EvidenceSemanticAnchor] =
    val breakAnchors =
      play.breakFile.toList.flatMap { file =>
        val normalized = axisLabelToken(file)
        List(
          EvidenceSemanticAnchor.of(EvidenceSemanticAnchorKind.PawnPlay, s"break-file-$normalized")
        ) ++ Option
          .when(Set("c", "d", "e", "f").contains(normalized))(
            EvidenceSemanticAnchor.of(EvidenceSemanticAnchorKind.PawnPlay, "center-break")
          )
          .toList
      }
    val tensionAnchors =
      Option
        .when(play.tensionSquares.nonEmpty || play.tensionEdges.nonEmpty)(
          EvidenceSemanticAnchor.of(EvidenceSemanticAnchorKind.PawnPlay, "tension")
        )
        .toList
    EvidenceSemanticAnchor.of(EvidenceSemanticAnchorKind.PawnPlay, play.primaryDriver.toString) ::
      (breakAnchors ++ tensionAnchors)

  def planPressureHasDirectEvidence(scoring: PlanScoringResult, activePlans: ActivePlans): Boolean =
    scoring.confidence >= 0.35 &&
      (activePlans.primary :: activePlans.secondary.toList ++ scoring.topPlans)
        .exists(_.evidence.nonEmpty)

  def planTransitionCanSupportPlan(transition: PlanSequenceSummary): Boolean =
    transition.primaryPlanId.nonEmpty && transition.transitionType != TransitionType.Opening

  def pawnStructureCanAnchorPlan(payload: PawnStructureFactEvidence): Boolean =
    payload.profile.primary != StructureId.Unknown && payload.profile.confidence >= 0.65 ||
      payload.alignment.exists(alignment =>
        alignment.band == AlignmentBand.OnBook ||
          alignment.band == AlignmentBand.Playable ||
          alignment.band == AlignmentBand.OffPlan
      ) ||
      payload.pawnPlay.exists(_.primaryDriver != PawnPlayDriver.Quiet)

  private def signal(
      kind: StrategicMechanismSignalKind,
      label: String,
      source: EvidenceRef,
      strength: Int,
      axis: Option[StrategicAxisDetail] = None
  ): StrategicMechanismSignal =
    StrategicMechanismSignal(kind, label, source, strength.max(1), axis)

  private def concreteAxis(record: EvidenceRecord, axis: Option[StrategicAxisDetail]): Option[StrategicAxisDetail] =
    axis.filter(_ => sourceHasAxisSubject(record))

  private def sourceHasAxisSubject(record: EvidenceRecord): Boolean =
    record.payload match
      case payload: StrategicFactEvidence =>
        payload.relatedPlans.nonEmpty ||
          payload.boardAnchors.exists(anchor => anchor.targetHintSquares.nonEmpty || anchor.focusSquares.nonEmpty) ||
          payload.boardAnchors.exists(anchor =>
            anchor.kind == BoardAnchorKind.CounterplayRestraint && anchor.detail.exists(_.subjectColor.nonEmpty)
          ) ||
          payload.facts.exists(factHasAxisSubject)
      case payload: PawnStructureFactEvidence =>
        payload.alignment.exists(_.matchedPlanIds.nonEmpty) ||
          payload.pawnPlay.exists(pawnPlay =>
            pawnPlay.breakFile.exists(_.trim.nonEmpty) ||
              pawnPlay.tensionSquares.exists(_.trim.nonEmpty) ||
              pawnPlay.tensionEdges.exists(_.trim.nonEmpty) ||
              pawnPlay.counterBreakFiles.exists(_.trim.nonEmpty) ||
              pawnPlay.blockadeSquare.nonEmpty
          )
      case payload: StructuralDeltaEvidence =>
        payload.signals.exists(_.subjects.exists(_.trim.nonEmpty)) ||
          payload.consequences.exists(_.subjects.exists(_.trim.nonEmpty)) ||
          payload.developmentChoices.nonEmpty
      case PlanPressureEvidence(_, activePlans) =>
        (activePlans.primary :: activePlans.secondary.toList).nonEmpty
      case PlanTransitionEvidence(transition) =>
        transition.primaryPlanId.nonEmpty
      case payload: BoardFactEvidence =>
        payload.targetHintSquares.nonEmpty ||
          payload.anchorFocusSquares.nonEmpty ||
          payload.lowLevelFacts.exists(factHasAxisSubject)
      case FeatureAnchorEvidence(_) | ApplicabilityAssessmentEvidence(_) | SinglePositionEvidence(_) =>
        false
      case _ =>
        false

  private def factHasAxisSubject(fact: Fact): Boolean =
    val focus = fact.squareFocus
    focus.targetSquares.nonEmpty ||
      focus.relatedSquares.nonEmpty ||
      focus.subjectSquares.nonEmpty ||
      fact.isInstanceOf[Fact.FileControl]

  private def strategicFactAxis(payload: StrategicFactEvidence): Option[StrategicAxisDetail] =
    payload.kind match
      case StrategicFactKind.TargetFixation =>
        Some(StrategicAxisDetail(StrategicAxisKind.Target, StrategicAxisPolarity.Support, payload.kind.toString))
      case StrategicFactKind.CounterplayRestraint =>
        Some(StrategicAxisDetail(StrategicAxisKind.Counterplay, StrategicAxisPolarity.Restrain, strategicFactSignalLabel(payload)))
      case StrategicFactKind.Space =>
        Some(StrategicAxisDetail(StrategicAxisKind.SpaceCenter, StrategicAxisPolarity.Support, payload.kind.toString))
      case StrategicFactKind.Structure =>
        None
      case StrategicFactKind.Activity | StrategicFactKind.Outpost | StrategicFactKind.FileControl =>
        Some(StrategicAxisDetail(StrategicAxisKind.Activity, StrategicAxisPolarity.Support, payload.kind.toString))
      case StrategicFactKind.PlanPressure =>
        Some(StrategicAxisDetail(StrategicAxisKind.PlanCoherence, StrategicAxisPolarity.Support, payload.kind.toString))
      case StrategicFactKind.Practicality | StrategicFactKind.Compensation | StrategicFactKind.Endgame =>
        None

  private def strategicFactSignalLabel(payload: StrategicFactEvidence): String =
    payload.kind match
      case StrategicFactKind.CounterplayRestraint
          if payload.boardAnchors.exists(_.signal == BoardAnchorSignal.OpponentLowMobility) =>
        "opponent-low-mobility"
      case _ =>
        payload.kind.toString

  private def pawnPlayAxis(pawnPlay: PawnPlayAnalysis): Option[StrategicAxisDetail] =
    pawnPlay.primaryDriver match
      case PawnPlayDriver.BreakReady | PawnPlayDriver.TensionActive | PawnPlayDriver.TensionCritical =>
        Some(StrategicAxisDetail(StrategicAxisKind.PawnBreak, StrategicAxisPolarity.Support, pawnPlayAxisLabel(pawnPlay)))
      case PawnPlayDriver.Defensive =>
        Some(StrategicAxisDetail(StrategicAxisKind.Counterplay, StrategicAxisPolarity.Restrain, pawnPlayAxisLabel(pawnPlay)))
      case PawnPlayDriver.PassedPawn | PawnPlayDriver.Quiet =>
        None

  private def pawnPlayAxisLabel(pawnPlay: PawnPlayAnalysis): String =
    val base =
      pawnPlay.primaryDriver match
        case PawnPlayDriver.BreakReady =>
          pawnPlay.breakFile.map(file => s"break-file-${axisLabelToken(file)}").getOrElse("break-ready")
        case PawnPlayDriver.TensionCritical =>
          "tension-critical"
        case PawnPlayDriver.TensionActive =>
          "tension-active"
        case PawnPlayDriver.Defensive =>
          val files = pawnPlay.counterBreakFiles.map(axisLabelToken).filter(_.nonEmpty).distinct.sorted
          if files.nonEmpty then s"defensive-counter-break-${files.mkString("-")}" else "defensive"
        case other =>
          axisLabelToken(other.toString)
    val policy =
      Option.when(pawnPlay.tensionPolicy != TensionPolicy.Ignore)(axisLabelToken(pawnPlay.tensionPolicy.toString))
    val tension =
      val edges = pawnPlay.tensionEdges.map(axisLabelToken).filter(_.nonEmpty).distinct.sorted
      val squares = pawnPlay.tensionSquares.map(axisLabelToken).filter(_.nonEmpty).distinct.sorted
      Option.when(edges.nonEmpty || squares.nonEmpty)((if edges.nonEmpty then edges else squares).mkString("-"))
    List(Some(base), policy, tension).flatten.filter(_.nonEmpty).mkString("-")

  private def structuralPawnBreakSignals(
      record: EvidenceRecord,
      payload: StructuralDeltaEvidence
  ): List[(StrategicMechanismKind, StrategicMechanismSignal)] =
    List(
      structuralPawnBreakSignal(
        record,
        payload,
        TransitionConsequenceKind.PawnTensionGain,
        StrategicAxisPolarity.Support
      ),
      structuralPawnBreakSignal(
        record,
        payload,
        TransitionConsequenceKind.PawnTensionResolution,
        StrategicAxisPolarity.Release
      )
    ).flatten

  private def structuralPawnBreakSignal(
      record: EvidenceRecord,
      payload: StructuralDeltaEvidence,
      consequenceKind: TransitionConsequenceKind,
      polarity: StrategicAxisPolarity
  ): Option[(StrategicMechanismKind, StrategicMechanismSignal)] =
    val consequences = payload.consequencesOf(consequenceKind)
    structuralPawnBreakLabel(consequences).map(label =>
      StrategicMechanismKind.PawnStructure -> signal(
        StrategicMechanismSignalKind.StructuralDelta,
        label,
        record.ref,
        2,
        concreteAxis(record, structuralDeltaAxis(StrategicAxisKind.PawnBreak, polarity, label))
      )
    )

  private def structuralPawnBreakLabel(consequences: List[TransitionConsequence]): Option[String] =
    val tensionSubjects =
      consequences.flatMap(_.subjects).map(axisLabelToken).filter(_.nonEmpty).distinct.sorted
    Option.when(tensionSubjects.nonEmpty)(tensionSubjects.mkString("-"))

  private def axisLabelToken(raw: String): String =
    raw.trim.toLowerCase.replaceAll("[^a-z0-9]+", "-").stripPrefix("-").stripSuffix("-")

  private def structuralDeltaAxis(
      kind: StrategicAxisKind,
      polarity: StrategicAxisPolarity,
      label: String
  ): Option[StrategicAxisDetail] =
    Some(StrategicAxisDetail(kind, polarity, label))

  private def openingAnchorAxis(theme: OpeningTheme, label: String): Option[StrategicAxisDetail] =
    theme match
      case OpeningTheme.CenterControl =>
        Some(StrategicAxisDetail(StrategicAxisKind.SpaceCenter, StrategicAxisPolarity.Support, label))
      case OpeningTheme.Development =>
        Some(StrategicAxisDetail(StrategicAxisKind.Activity, StrategicAxisPolarity.Support, label))
      case OpeningTheme.PawnStructure =>
        Some(StrategicAxisDetail(StrategicAxisKind.PawnBreak, StrategicAxisPolarity.Support, label))
      case OpeningTheme.KingSafety | OpeningTheme.PlanPressure =>
        Some(StrategicAxisDetail(StrategicAxisKind.Counterplay, StrategicAxisPolarity.Support, label))
      case OpeningTheme.GambitInitiative =>
        None

enum OpeningFamily:
  case A
  case B
  case C
  case D
  case E

object OpeningFamily:
  def fromEco(raw: String): Option[OpeningFamily] =
    Option(raw)
      .map(_.trim.toUpperCase)
      .flatMap(_.headOption)
      .flatMap(ch => fromRaw(ch.toString))

  def fromRaw(raw: String): Option[OpeningFamily] =
    Option(raw).map(_.trim.toUpperCase).collect:
      case "A" => OpeningFamily.A
      case "B" => OpeningFamily.B
      case "C" => OpeningFamily.C
      case "D" => OpeningFamily.D
      case "E" => OpeningFamily.E

enum OpeningContextSignal:
  case InputIdentity
  case RecognizedIdentity
  case OpeningPhase
  case ThemePrior

enum OpeningTheme:
  case CenterControl
  case Development
  case PawnStructure
  case GambitInitiative
  case KingSafety
  case PlanPressure

enum OpeningThemePriorMatchSource:
  case ExactLineage
  case LineageAlias
  case NameHint
  case FamilyFallback

  def openingSpecific: Boolean =
    this != OpeningThemePriorMatchSource.FamilyFallback

  def canCertifyOpeningClaim: Boolean =
    this match
      case OpeningThemePriorMatchSource.ExactLineage | OpeningThemePriorMatchSource.LineageAlias =>
        true
      case OpeningThemePriorMatchSource.NameHint | OpeningThemePriorMatchSource.FamilyFallback =>
        false

enum FeatureAnchorSignal:
  case CenterControlObserved
  case DevelopmentTempoObserved
  case DevelopmentLagObserved
  case PawnStructureObserved
  case PawnBreakObserved
  case CentralTensionObserved
  case CompensationObserved
  case KingSafetyObserved
  case PlanPressureObserved
  case LinePressureObserved
  case StructuralDeltaObserved

final case class FeatureAnchor(
    theme: OpeningTheme,
    signal: FeatureAnchorSignal,
    sourceLayer: EvidenceLayer,
    strength: Double
):
  def isBoardObservation: Boolean =
    sourceLayer == EvidenceLayer.Board
  def canCorroborateOpeningPrior: Boolean =
    !isBoardObservation
  def hasPositiveStrength: Boolean =
    strength > 0.0

enum FeatureApplicability:
  case OpeningRelevant
  case MiddlegameRelevant
  case EndgameRelevant
  case ObservedOnly
  case Contraindicated

enum ApplicabilityStatus:
  case InternalOnly
  case Supported
  case PartiallySupported
  case Unverified
  case Ambiguous
  case Contradicted

final case class ApplicabilityAssessment(
    applicability: FeatureApplicability,
    status: ApplicabilityStatus,
    observedThemes: List[OpeningTheme],
    supportedThemes: List[OpeningTheme],
    unverifiedPriorThemes: List[OpeningTheme],
    observedOnlyThemes: List[OpeningTheme],
    priorMatchSources: List[OpeningThemePriorMatchSource] = Nil
):
  def hasInternalAnchorAlignment: Boolean =
    applicability == FeatureApplicability.OpeningRelevant &&
      observedThemes.nonEmpty &&
      supportedThemes.nonEmpty &&
      (status == ApplicabilityStatus.Supported || status == ApplicabilityStatus.PartiallySupported)

  def hasCertifyingPriorEvidence: Boolean =
    priorMatchSources.exists(_.canCertifyOpeningClaim)

  def canCertifyOpeningClaim: Boolean =
    hasInternalAnchorAlignment && hasCertifyingPriorEvidence

final case class OpeningIdentity(
    eco: Option[String],
    name: Option[String],
    family: Option[OpeningFamily]
)

final case class OpeningCandidate(
    identity: OpeningIdentity,
    lineage: Option[String],
    frequency: Int,
    sampleCount: Int,
    confidence: Double
)

enum OpeningRecognitionMatchKind:
  case ExactPrefixAndPosition
  case PositionTransposition

final case class OpeningRecognition(
    movePrefixHash: String,
    positionKey: String,
    matchedBy: OpeningRecognitionMatchKind,
    candidates: List[OpeningCandidate],
    matchedPly: Int,
    frequency: Int,
    sampleCount: Int,
    confidence: Double
):
  def bestCandidate: Option[OpeningCandidate] =
    candidates.headOption

  def bestIdentity: Option[OpeningIdentity] =
    bestCandidate.map(_.identity)

  def lineage: Option[String] =
    bestCandidate.flatMap(_.lineage)

final case class OpeningThemePrior(
    lineage: Option[String],
    family: Option[OpeningFamily],
    themes: List[OpeningTheme],
    typicalPawnStructures: List[String],
    centerBreaks: List[String],
    developmentPriorities: List[String],
    gambitCompensation: Boolean,
    strategicPlanPriors: List[String]
)

final case class OpeningThemePriorSelection(
    prior: OpeningThemePrior,
    matchSource: OpeningThemePriorMatchSource,
    requestedLineage: Option[String],
    canonicalLineage: Option[String]
):
  def openingSpecific: Boolean =
    matchSource.openingSpecific

  def canCertifyOpeningClaim: Boolean =
    matchSource.canCertifyOpeningClaim

final case class OpeningContextEvidence(
    identity: Option[OpeningIdentity],
    signals: List[OpeningContextSignal],
    recognition: Option[OpeningRecognition] = None,
    themePriorSelection: Option[OpeningThemePriorSelection] = None
) extends EvidencePayload:
  val layer: EvidenceLayer = EvidenceLayer.OpeningContext

final case class FeatureAnchorEvidence(
    anchor: FeatureAnchor
) extends EvidencePayload:
  val layer: EvidenceLayer = EvidenceLayer.FeatureAnchor

final case class ApplicabilityAssessmentEvidence(
    assessment: ApplicabilityAssessment
) extends EvidencePayload:
  val layer: EvidenceLayer = EvidenceLayer.ApplicabilityAssessment

final case class ThreatEpisode(
    episodeId: String,
    sourceThreatIndex: Int,
    sideUnderPressure: Color,
    kind: ThreatKind,
    severity: ThreatSeverity,
    driver: ThreatDriver,
    evidenceSource: ThreatEvidenceSource,
    rawLossIfIgnoredCpForDiagnostics: Int,
    lossIfIgnoredWinPercent: Option[Double],
    turnsToImpact: Int,
    attackSquares: List[EvidenceSquare],
    targetPieces: List[EvidencePieceRole],
    motifs: List[Motif],
    bestDefense: Option[String],
    defenseCount: Int
):
  def immediate: Boolean =
    turnsToImpact <= 2
  def strategic: Boolean =
    turnsToImpact >= 3
  def defenseRequired: Boolean =
    severity != ThreatSeverity.Low
  def hasLineValueProof: Boolean =
    evidenceSource == ThreatEvidenceSource.CandidateLineValueDelta ||
      evidenceSource == ThreatEvidenceSource.MotifAndLineValueDelta
  def hasMotifProof: Boolean =
    motifs.nonEmpty
  def hasConcreteThreatProof: Boolean =
    kind == ThreatKind.Mate || hasLineValueProof || hasMotifProof
  def motifKinds: List[String] =
    motifs.map(_.getClass.getSimpleName.stripSuffix("$")).distinct

object ThreatEpisode:
  def fromThreat(sideUnderPressure: Color, threat: Threat, index: Int): ThreatEpisode =
    ThreatEpisode(
      episodeId = s"${sideUnderPressure.name}:threat:$index:${threat.kind}:${threat.turnsToImpact}",
      sourceThreatIndex = index,
      sideUnderPressure = sideUnderPressure,
      kind = threat.kind,
      severity = threat.severity,
      driver = driverFor(threat),
      evidenceSource = threat.evidenceSource,
      rawLossIfIgnoredCpForDiagnostics = threat.lossIfIgnoredCp,
      lossIfIgnoredWinPercent = threat.lossIfIgnoredWinPercent,
      turnsToImpact = threat.turnsToImpact,
      attackSquares = threat.attackSquares.distinct.map(EvidenceSquare(_)),
      targetPieces = threat.targetPieces.distinct.map(EvidencePieceRole(_)),
      motifs = threat.motifs,
      bestDefense = threat.bestDefense.map(EvidenceRef.normalizeMove),
      defenseCount = threat.defenseCount
    )

  def fromAnalysis(sideUnderPressure: Color, analysis: ThreatAnalysis): List[ThreatEpisode] =
    analysis.threats.zipWithIndex.map { case (threat, index) =>
      fromThreat(sideUnderPressure, threat, index)
    }

  private def driverFor(threat: Threat): ThreatDriver =
    threat.kind match
      case ThreatKind.Mate       => ThreatDriver.MateThreat
      case ThreatKind.Material   => ThreatDriver.MaterialThreat
      case ThreatKind.Positional => ThreatDriver.PositionalThreat

final case class ThreatPressureEvidence(
    sideUnderPressure: Color,
    threats: ThreatAnalysis
) extends EvidencePayload:
  val layer: EvidenceLayer = EvidenceLayer.ThreatPressure
  def episodes: List[ThreatEpisode] =
    ThreatEpisode.fromAnalysis(sideUnderPressure, threats)
  def hasProofSignalThreatEpisode: Boolean =
    episodes.exists(_.hasConcreteThreatProof)

final case class ThreatEpisodeEvidence(
    episode: ThreatEpisode,
    summary: ThreatAnalysis
) extends EvidencePayload:
  val layer: EvidenceLayer = EvidenceLayer.ThreatPressure
  def sideUnderPressure: Color =
    episode.sideUnderPressure
  def defenseRequired: Boolean =
    episode.defenseRequired
  def onlyDefense: Option[String] =
    episode.bestDefense.filter(_ => episode.defenseCount == 1)
  def prophylaxisNeeded: Boolean =
    episode.strategic && episode.defenseRequired
  def insufficientData: Boolean =
    !episode.hasConcreteThreatProof
  def maxWinPercentLossIfIgnored: Option[Double] =
    episode.lossIfIgnoredWinPercent
  def isProofSignalDefensivePressure: Boolean =
    !insufficientData &&
      (
        defenseRequired ||
          prophylaxisNeeded ||
          episode.severity != ThreatSeverity.Low ||
          maxWinPercentLossIfIgnored.exists(_ >= JudgmentThresholds.SIGNIFICANT_THREAT_WP)
      )
  def canAnchorDefensiveResource: Boolean =
    isProofSignalDefensivePressure && onlyDefense.nonEmpty

final case class ForcedLineThemeEvidence(
    id: String,
    lineMoves: List[String]
)

final case class LineReplayStep(
    ply: Int,
    moveUci: String,
    fenBefore: String,
    fenAfter: String
)

enum LineEndgameTechniqueHorizonStatus:
  case Active
  case Transitioned
  case Failed
  case SupersededByTactic
  case ContradictedByTerminalProof

final case class LineEndgameTechniqueHorizon(
    pattern: String,
    rookPattern: String,
    techniqueSide: Color,
    entryPlyOffset: Int,
    terminalPlyOffset: Int,
    status: LineEndgameTechniqueHorizonStatus,
    triggerMove: Option[String] = None,
    requiredSquares: List[String] = Nil,
    maintainedSquares: List[String] = Nil,
    brokenSquares: List[String] = Nil,
    terminalConsequenceKinds: List[LineConsequenceKind] = Nil,
    failureReason: Option[String] = None
):
  def techniqueSideKey: String =
    if techniqueSide.white then "white" else "black"

object LineEndgameTechniqueHorizon:
  def maintained(status: LineEndgameTechniqueHorizonStatus): Boolean =
    status == LineEndgameTechniqueHorizonStatus.Active ||
      status == LineEndgameTechniqueHorizonStatus.Transitioned

  def winningPattern(pattern: String): Boolean =
    pattern == "Lucena"

  def defensivePattern(pattern: String): Boolean =
    pattern match
      case "WrongRookPawnWrongBishopFortress" | "VancuraDefense" | "PhilidorDefense" | "ShortSideDefense" |
          "OppositeColoredBishopsDraw" | "KnightBlockadeRookPawnDraw" | "TarraschDefenseActive" |
          "PassiveRookDefense" | "RookAndBishopVsRookDraw" | "SameColoredBishopsBlockade" =>
        true
      case _ =>
        false

  def terminalProofOverrides(kind: LineConsequenceKind): Boolean =
    kind match
      case LineConsequenceKind.Mate | LineConsequenceKind.MaterialGain | LineConsequenceKind.MaterialLoss |
          LineConsequenceKind.PromotionRace =>
        true
      case _ =>
        false

enum LineEventKind:
  case Capture
  case Recapture
  case DefenderMove
  case Threat
  case Castling
  case Check
  case Mate
  case Tempo
  case Stalemate
  case Promotion
  case PassedPawn
  case ForcedTheme

final case class LineMoveEvent(
    kind: LineEventKind,
    moveUci: String,
    plyOffset: Int,
    side: Option[Color] = None,
    pieceRole: Option[EvidencePieceRole] = None,
    targetRole: Option[EvidencePieceRole] = None,
    square: Option[EvidenceSquare] = None
)

enum LineConsequenceKind:
  case ForcedTheme
  case ImmediateReplyCheck
  case Mate
  case DrawResource
  case MaterialGain
  case MaterialLoss
  case RecaptureSequence
  case RecoveryWindow
  case Sacrifice
  case PromotionRace

object LineConsequenceKind:
  def terminalResultProof(kind: LineConsequenceKind): Boolean =
    kind match
      case LineConsequenceKind.Mate | LineConsequenceKind.MaterialGain | LineConsequenceKind.MaterialLoss |
          LineConsequenceKind.PromotionRace | LineConsequenceKind.DrawResource =>
        true
      case _ =>
        false

  def terminalResultProofName(kind: String): Boolean =
    values.find(_.toString == kind).exists(terminalResultProof)

  def tacticalDriver(kind: LineConsequenceKind): Boolean =
    kind match
      case LineConsequenceKind.MaterialGain | LineConsequenceKind.MaterialLoss |
          LineConsequenceKind.RecaptureSequence | LineConsequenceKind.RecoveryWindow |
          LineConsequenceKind.ImmediateReplyCheck | LineConsequenceKind.Mate |
          LineConsequenceKind.DrawResource | LineConsequenceKind.PromotionRace =>
        true
      case LineConsequenceKind.ForcedTheme | LineConsequenceKind.Sacrifice =>
        false

enum LineMaterialOutcomeSignal:
  case MoverCapture
  case OpponentCapture
  case PromotionGain
  case PromotionLoss
  case UnrecoveredPawnGain
  case UnrecoveredPawnLoss
  case RecoveryWindow

enum LineMaterialOutcomeMagnitude:
  case None
  case Pawn
  case Piece

final case class LineConsequence(
    kind: LineConsequenceKind,
    lineMoves: List[String],
    proofSignal: Boolean,
    eventMove: Option[String] = None
):
  def rootMoveMatched(rootMove: String): Boolean =
    val normalizedRoot = LineConsequence.normalizeUci(rootMove)
    eventMove.exists(move => LineConsequence.normalizeUci(move) == normalizedRoot) ||
      lineMoves.exists(move => LineConsequence.normalizeUci(move) == normalizedRoot)

object LineConsequence:
  private def normalizeUci(raw: String): String =
    Option(raw).getOrElse("").trim.toLowerCase

final case class LineConsequenceProfile(
    proofSignalKinds: List[LineConsequenceKind],
    hasConcreteProofSignal: Boolean,
    hasConversionConsequence: Boolean,
    hasMaterialResult: Boolean,
    hasRecaptureRecovery: Boolean,
    hasSacrifice: Boolean,
    hasPromotionRace: Boolean,
    hasMate: Boolean,
    hasDrawResource: Boolean
):
  def tacticalDriverKinds: List[LineConsequenceKind] =
    proofSignalKinds.filter(LineConsequenceKind.tacticalDriver)

final case class LineMaterialOutcomeProfile(
    gainSignals: Set[LineMaterialOutcomeSignal],
    lossSignals: Set[LineMaterialOutcomeSignal]
):
  def merge(other: LineMaterialOutcomeProfile): LineMaterialOutcomeProfile =
    LineMaterialOutcomeProfile(
      gainSignals = gainSignals ++ other.gainSignals,
      lossSignals = lossSignals ++ other.lossSignals
    )

  def gainMagnitude: LineMaterialOutcomeMagnitude =
    if gainSignals.exists(signal =>
        signal == LineMaterialOutcomeSignal.MoverCapture ||
          signal == LineMaterialOutcomeSignal.PromotionGain ||
          signal == LineMaterialOutcomeSignal.RecoveryWindow
      )
    then LineMaterialOutcomeMagnitude.Piece
    else if gainSignals.contains(LineMaterialOutcomeSignal.UnrecoveredPawnGain) then LineMaterialOutcomeMagnitude.Pawn
    else LineMaterialOutcomeMagnitude.None

  def lossMagnitude: LineMaterialOutcomeMagnitude =
    if lossSignals.exists(signal =>
        signal == LineMaterialOutcomeSignal.OpponentCapture ||
          signal == LineMaterialOutcomeSignal.PromotionLoss
      )
    then LineMaterialOutcomeMagnitude.Piece
    else if lossSignals.contains(LineMaterialOutcomeSignal.UnrecoveredPawnLoss) then LineMaterialOutcomeMagnitude.Pawn
    else LineMaterialOutcomeMagnitude.None

object LineMaterialOutcomeProfile:
  val empty: LineMaterialOutcomeProfile =
    LineMaterialOutcomeProfile(Set.empty, Set.empty)

final case class LineMaterialCapture(
    moveUci: String,
    plyOffset: Int,
    side: Color,
    attackerRole: EvidencePieceRole,
    capturedRole: EvidencePieceRole,
    square: EvidenceSquare,
    valueCp: Int,
    recapture: Boolean
)

final case class LineMaterialSummary(
    sideToMove: Color,
    captures: List[LineMaterialCapture],
    netCaptureCpForMover: Int,
    maxGainCpForMover: Int,
    maxLossCpForMover: Int,
    hasRecaptureChain: Boolean,
    hasRecoveryWindow: Boolean,
    promotionGainCpForMover: Int,
    materialWindowComplete: Boolean
):
  def hasPromotion: Boolean = promotionGainCpForMover != 0

  def capturesByMover: List[LineMaterialCapture] =
    captures.filter(_.side == sideToMove)

  def capturesByOpponent: List[LineMaterialCapture] =
    captures.filter(_.side != sideToMove)

  def nonPawnCapturesByMover: List[LineMaterialCapture] =
    capturesByMover.filter(capture => proofSignalCapturedRole(capture.capturedRole))

  def nonPawnCapturesByOpponent: List[LineMaterialCapture] =
    capturesByOpponent.filter(capture => proofSignalCapturedRole(capture.capturedRole))

  def pawnCapturesByMover: List[LineMaterialCapture] =
    capturesByMover.filter(capture => pawnCapturedRole(capture.capturedRole))

  def pawnCapturesByOpponent: List[LineMaterialCapture] =
    capturesByOpponent.filter(capture => pawnCapturedRole(capture.capturedRole))

  def hasPromotionGainForMover: Boolean =
    promotionGainCpForMover > 0

  def hasPromotionLossForMover: Boolean =
    promotionGainCpForMover < 0

  def hasResolvedMaterialSequence: Boolean =
    materialWindowComplete && (hasRecaptureChain || hasRecoveryWindow)

  def hasProofSignalMaterialGain: Boolean =
    materialWindowComplete && (nonPawnCapturesByMover.nonEmpty || hasPromotionGainForMover || hasRecoveryWindow)

  def hasProofSignalMaterialLoss: Boolean =
    materialWindowComplete && (nonPawnCapturesByOpponent.nonEmpty || hasPromotionLossForMover)

  def hasUnrecoveredPawnGainForMover: Boolean =
    materialWindowComplete && pawnCapturesByMover.nonEmpty && !hasRecoveryWindow

  def hasUnrecoveredPawnLossForMover: Boolean =
    materialWindowComplete && pawnCapturesByOpponent.nonEmpty && !hasRecoveryWindow

  def hasProofSignalMaterialEvent: Boolean =
    hasProofSignalMaterialGain ||
      hasProofSignalMaterialLoss ||
      hasUnrecoveredPawnGainForMover ||
      hasUnrecoveredPawnLossForMover ||
      hasResolvedMaterialSequence

  def hasSacrificeMaterialEvent: Boolean =
    materialWindowComplete &&
      capturesByOpponent.exists(capture => !capture.recapture) &&
      !hasRecoveryWindow &&
      !hasProofSignalMaterialGain

  private def proofSignalCapturedRole(role: EvidencePieceRole): Boolean =
    val normalized = role.name.trim.toLowerCase
    normalized.nonEmpty && normalized != "pawn" && normalized != "king"

  private def pawnCapturedRole(role: EvidencePieceRole): Boolean =
    role.name.trim.equalsIgnoreCase("pawn")

final case class LineFactEvidence(
    line: LineNodeRef,
    private val firstMove: Option[String],
    private val replyMove: Option[String],
    private val continuationMoves: List[String],
    private val forcedTheme: Option[ForcedLineThemeEvidence] = None,
    private val material: Option[LineMaterialSummary] = None
)(private val replay: List[LineReplayStep] = Nil,
    private val events: List[LineMoveEvent] = Nil,
    private val consequences: List[LineConsequence] = Nil,
    private val endgameHorizons: List[LineEndgameTechniqueHorizon] = Nil
) extends EvidencePayload:
  val layer: EvidenceLayer = EvidenceLayer.Line
  def rootMove: Option[String] =
    firstMove
  def reply: Option[String] =
    replyMove
  def continuation: List[String] =
    continuationMoves
  def forcedThemeId: Option[String] =
    forcedTheme.map(_.id)
  def lineReplaySteps: List[LineReplayStep] =
    replay
  def lineReplayMoves: List[String] =
    replay.map(_.moveUci)
  def lineReplayContinuationMoves: List[String] =
    lineReplayMoves.drop(1)
  def materialCaptures: List[LineMaterialCapture] =
    material.toList.flatMap(_.captures)
  def materialSacrificeCapture(capture: LineMaterialCapture): Boolean =
    !capture.recapture &&
      materialValue(capture.attackerRole) > materialValue(capture.capturedRole) &&
      materialCaptures.exists(reply =>
        reply.recapture &&
          reply.plyOffset > capture.plyOffset &&
          reply.square == capture.square &&
          reply.side != capture.side
      )
  def lineEvents: List[LineMoveEvent] =
    events
  def lineConsequences: List[LineConsequence] =
    consequences
  def endgameTechniqueHorizons: List[LineEndgameTechniqueHorizon] =
    endgameHorizons
  def terminalEndgameTechniqueOverrideKinds: List[LineConsequenceKind] =
    proofSignalConsequenceKinds.filter(LineEndgameTechniqueHorizon.terminalProofOverrides)
  def hasTerminalEndgameTechniqueOverride: Boolean =
    terminalEndgameTechniqueOverrideKinds.nonEmpty
  def maintainedWinningEndgameTechniqueHorizons: List[LineEndgameTechniqueHorizon] =
    endgameHorizons.filter(horizon =>
      LineEndgameTechniqueHorizon.winningPattern(horizon.pattern) &&
        LineEndgameTechniqueHorizon.maintained(horizon.status) &&
        !hasTerminalEndgameTechniqueOverride
    )
  def failedWinningEndgameTechniqueHorizons: List[LineEndgameTechniqueHorizon] =
    endgameHorizons.filter(horizon =>
      LineEndgameTechniqueHorizon.winningPattern(horizon.pattern) &&
        horizon.status == LineEndgameTechniqueHorizonStatus.Failed &&
        !hasTerminalEndgameTechniqueOverride
    )
  def maintainedDefensiveEndgameTechniqueHorizons: List[LineEndgameTechniqueHorizon] =
    endgameHorizons.filter(horizon =>
      LineEndgameTechniqueHorizon.defensivePattern(horizon.pattern) &&
        LineEndgameTechniqueHorizon.maintained(horizon.status) &&
        !hasTerminalEndgameTechniqueOverride
    )
  def rootOwnedEndgameTechniqueHorizons(rootMoveUci: String, kind: RelativeCauseKind): List[LineEndgameTechniqueHorizon] =
    val normalizedRoot = normalizeUci(rootMoveUci)
    val candidates =
      kind match
        case RelativeCauseKind.ConversionSecured =>
          maintainedWinningEndgameTechniqueHorizons
        case RelativeCauseKind.ConversionMiss =>
          failedWinningEndgameTechniqueHorizons ++ maintainedWinningEndgameTechniqueHorizons
        case RelativeCauseKind.DrawResource =>
          maintainedDefensiveEndgameTechniqueHorizons
        case _ =>
          Nil
    candidates.filter(horizon =>
      horizon.triggerMove.exists(move => normalizeUci(move) == normalizedRoot) ||
        (LineEndgameTechniqueHorizon.maintained(horizon.status) && horizon.triggerMove.isEmpty && horizon.entryPlyOffset <= 0)
    )
  def lineReplayCount: Int =
    replay.size
  def hasLineReplay: Boolean =
    replay.nonEmpty
  def lineEventKinds: List[LineEventKind] =
    events.map(_.kind)
  def lineEventsOf(kind: LineEventKind): List[LineMoveEvent] =
    events.filter(_.kind == kind)
  def hasLineEvent(kind: LineEventKind): Boolean =
    lineEventsOf(kind).nonEmpty
  def rootOwnedLineEvents(rootMoveUci: String): List[LineMoveEvent] =
    val normalizedRoot = normalizeUci(rootMoveUci)
    events.filter(event =>
      normalizeUci(event.moveUci) == normalizedRoot || event.plyOffset == 0
    )
  def hasLineEventAt(kind: LineEventKind, plyOffset: Int): Boolean =
    lineEventsOf(kind).exists(_.plyOffset == plyOffset)
  def hasTempoEventAt(plyOffset: Int): Boolean =
    hasLineEventAt(LineEventKind.Tempo, plyOffset)
  def lineEventMoves(kind: LineEventKind): List[String] =
    lineEventsOf(kind).map(_.moveUci)
  def hasRootCaptureEvent(rootMoveUci: String): Boolean =
    val normalizedRoot = normalizeUci(rootMoveUci)
    events.exists(event =>
      (event.kind == LineEventKind.Capture || event.kind == LineEventKind.Recapture) &&
        normalizeUci(event.moveUci) == normalizedRoot
    )
  def lineEventCount: Int =
    events.size
  def hasLineEvents: Boolean =
    events.nonEmpty
  def lineConsequenceCount: Int =
    consequences.size
  def hasLineConsequences: Boolean =
    consequences.nonEmpty
  def hasForcedTheme: Boolean =
    forcedTheme.nonEmpty
  def materialNetCaptureCpForMover: Option[Int] =
    material.map(_.netCaptureCpForMover)
  def materialMaxGainCpForMover: Option[Int] =
    material.map(_.maxGainCpForMover)
  def materialPromotionGainCpForMover: Option[Int] =
    material.map(_.promotionGainCpForMover)
  def hasMaterialRecaptureChain: Boolean =
    material.exists(_.hasRecaptureChain)
  def hasMaterialRecoveryWindow: Boolean =
    material.exists(_.hasRecoveryWindow)
  def hasCompleteMaterialWindow: Boolean =
    material.forall(_.materialWindowComplete)
  def hasProofSignalMaterialEvent: Boolean =
    material.exists(_.hasProofSignalMaterialEvent)
  def hasSacrificeMaterialEvent: Boolean =
    material.exists(_.hasSacrificeMaterialEvent)
  def proofSignalConsequences: List[LineConsequence] =
    consequences.filter(_.proofSignal)
  def proofSignalConsequencesOf(kind: LineConsequenceKind): List[LineConsequence] =
    proofSignalConsequences.filter(_.kind == kind)
  def rootOwnedProofSignalConsequences(rootMoveUci: String): List[LineConsequence] =
    proofSignalConsequences.filter(_.rootMoveMatched(rootMoveUci))
  def hasProofSignalConsequence: Boolean =
    proofSignalConsequences.nonEmpty
  def proofSignalConsequenceKinds: List[LineConsequenceKind] =
    proofSignalConsequences.map(_.kind)
  def hasProofSignalConsequence(kind: LineConsequenceKind): Boolean =
    proofSignalConsequenceKinds.contains(kind)
  def consequenceProfile: LineConsequenceProfile =
    val kinds = proofSignalConsequenceKinds
    LineConsequenceProfile(
      proofSignalKinds = kinds,
      hasConcreteProofSignal = kinds.nonEmpty,
      hasConversionConsequence = kinds.exists {
        case LineConsequenceKind.RecaptureSequence | LineConsequenceKind.RecoveryWindow |
            LineConsequenceKind.MaterialGain | LineConsequenceKind.MaterialLoss |
            LineConsequenceKind.Sacrifice =>
          true
        case _ =>
          false
      },
      hasMaterialResult = kinds.exists {
        case LineConsequenceKind.MaterialGain | LineConsequenceKind.MaterialLoss |
            LineConsequenceKind.Sacrifice | LineConsequenceKind.PromotionRace =>
          true
        case _ =>
          false
      },
      hasRecaptureRecovery = kinds.exists(kind =>
        kind == LineConsequenceKind.RecaptureSequence || kind == LineConsequenceKind.RecoveryWindow
      ),
      hasSacrifice = kinds.contains(LineConsequenceKind.Sacrifice),
      hasPromotionRace = kinds.contains(LineConsequenceKind.PromotionRace),
      hasMate = kinds.contains(LineConsequenceKind.Mate),
      hasDrawResource = kinds.contains(LineConsequenceKind.DrawResource)
    )
  def hasConcreteLineConsequence: Boolean =
    consequenceProfile.hasConcreteProofSignal
  def hasConversionConsequence: Boolean =
    consequenceProfile.hasConversionConsequence
  def hasMaterialConsequence: Boolean =
    consequenceProfile.hasMaterialResult
  def hasRecaptureRecoveryConsequence: Boolean =
    consequenceProfile.hasRecaptureRecovery
  def hasSacrificeConsequence: Boolean =
    consequenceProfile.hasSacrifice
  def tacticalLineConsequenceKinds: List[LineConsequenceKind] =
    consequenceProfile.tacticalDriverKinds
  def hasTacticalLineConsequence: Boolean =
    tacticalLineConsequenceKinds.nonEmpty
  def semanticGroupingAnchors: List[EvidenceSemanticAnchor] =
    Option
      .when(hasLineEvent(LineEventKind.Castling))(
        EvidenceSemanticAnchor.of(EvidenceSemanticAnchorKind.LineEvent, LineEventKind.Castling.toString)
      )
      .toList ++
      proofSignalConsequenceKinds.map(kind =>
        EvidenceSemanticAnchor.of(EvidenceSemanticAnchorKind.LineConsequence, kind.toString)
      ) ++
      endgameTechniqueHorizons.map(horizon =>
        EvidenceSemanticAnchor.of(
          EvidenceSemanticAnchorKind.LineConsequence,
          "EndgameTechniqueHorizon",
          s"pattern:${horizon.pattern}",
          s"rook-pattern:${horizon.rookPattern}",
          s"horizonStatus:${horizon.status}",
          s"technique-side:${horizon.techniqueSideKey}"
        )
      )

  private def normalizeUci(raw: String): String =
    Option(raw).getOrElse("").trim.toLowerCase
  def materialOutcomeProfile: LineMaterialOutcomeProfile =
    val consequenceGainSignals =
      consequences.collect {
        case LineConsequence(LineConsequenceKind.MaterialGain, _, true, _) =>
          LineMaterialOutcomeSignal.MoverCapture
        case LineConsequence(LineConsequenceKind.MaterialGain, _, false, _) =>
          LineMaterialOutcomeSignal.UnrecoveredPawnGain
        case LineConsequence(LineConsequenceKind.RecoveryWindow, _, true, _) =>
          LineMaterialOutcomeSignal.RecoveryWindow
      }.toSet
    val consequenceLossSignals =
      consequences.collect {
        case LineConsequence(LineConsequenceKind.MaterialLoss, _, true, _) =>
          LineMaterialOutcomeSignal.OpponentCapture
        case LineConsequence(LineConsequenceKind.MaterialLoss, _, false, _) =>
          LineMaterialOutcomeSignal.UnrecoveredPawnLoss
      }.toSet
    val materialGainSignals =
      material
        .map(summary =>
          Set(
            Option.when(summary.nonPawnCapturesByMover.nonEmpty)(LineMaterialOutcomeSignal.MoverCapture),
            Option.when(summary.hasPromotionGainForMover)(LineMaterialOutcomeSignal.PromotionGain),
            Option.when(summary.hasUnrecoveredPawnGainForMover)(LineMaterialOutcomeSignal.UnrecoveredPawnGain),
            Option.when(summary.hasRecoveryWindow)(LineMaterialOutcomeSignal.RecoveryWindow)
          ).flatten
        )
        .getOrElse(Set.empty)
    val materialLossSignals =
      material
        .map(summary =>
          Set(
            Option.when(summary.nonPawnCapturesByOpponent.nonEmpty)(LineMaterialOutcomeSignal.OpponentCapture),
            Option.when(summary.hasPromotionLossForMover)(LineMaterialOutcomeSignal.PromotionLoss),
            Option.when(summary.hasUnrecoveredPawnLossForMover)(LineMaterialOutcomeSignal.UnrecoveredPawnLoss)
          ).flatten
        )
        .getOrElse(Set.empty)
    LineMaterialOutcomeProfile(
      gainSignals = consequenceGainSignals ++ materialGainSignals,
      lossSignals = consequenceLossSignals ++ materialLossSignals
    )
  def hasMaterialOutcomeSignals: Boolean =
    val profile = materialOutcomeProfile
    profile.gainSignals.nonEmpty || profile.lossSignals.nonEmpty
  def materialOutcomeConsequenceKinds: List[LineConsequenceKind] =
    val profile = materialOutcomeProfile
    List(
      Option.when(hasMaterialRecaptureChain)(LineConsequenceKind.RecaptureSequence),
      Option.when(hasMaterialRecoveryWindow)(LineConsequenceKind.RecoveryWindow),
      Option.when(profile.gainMagnitude != LineMaterialOutcomeMagnitude.None)(LineConsequenceKind.MaterialGain),
      Option.when(profile.lossMagnitude != LineMaterialOutcomeMagnitude.None)(LineConsequenceKind.MaterialLoss)
    ).flatten.distinct
  def hasMaterialOutcomeConsequence(kind: LineConsequenceKind): Boolean =
    materialOutcomeConsequenceKinds.contains(kind)

  private def materialValue(role: EvidencePieceRole): Int =
    role.name.toLowerCase match
      case "queen"             => 9
      case "rook"              => 5
      case "bishop" | "knight" => 3
      case "pawn"              => 1
      case "king"              => 100
      case _                   => 0

object LineFactEvidence:
  def fromRecords(records: List[EvidenceRecord]): List[LineFactEvidence] =
    records.collect { case EvidenceRecord(_, payload: LineFactEvidence, _) => payload }

  def materialOutcomeProfile(records: List[EvidenceRecord]): LineMaterialOutcomeProfile =
    fromRecords(records).map(_.materialOutcomeProfile).foldLeft(LineMaterialOutcomeProfile.empty)(_.merge(_))

  def maxMaterialNetCaptureCpForMover(records: List[EvidenceRecord]): Int =
    fromRecords(records).flatMap(_.materialNetCaptureCpForMover).maxOption.getOrElse(0)

  def maxMaterialGainCpForMover(records: List[EvidenceRecord]): Int =
    fromRecords(records).flatMap(_.materialMaxGainCpForMover).maxOption.getOrElse(0)

  def maxMaterialPromotionGainCpForMover(records: List[EvidenceRecord]): Int =
    fromRecords(records).flatMap(_.materialPromotionGainCpForMover).maxOption.getOrElse(0)

  def hasMaterialRecaptureChain(records: List[EvidenceRecord]): Boolean =
    fromRecords(records).exists(_.hasMaterialRecaptureChain)

  def hasMaterialRecoveryWindow(records: List[EvidenceRecord]): Boolean =
    fromRecords(records).exists(_.hasMaterialRecoveryWindow)

  def allHaveCompleteMaterialWindow(records: List[EvidenceRecord]): Boolean =
    fromRecords(records).forall(_.hasCompleteMaterialWindow)

  def apply(
      line: LineNodeRef,
      firstMove: Option[String],
      replyMove: Option[String],
      continuationMoves: List[String]
  ): LineFactEvidence =
    new LineFactEvidence(line, firstMove, replyMove, continuationMoves, None, None)()

  def apply(
      line: LineNodeRef,
      firstMove: Option[String],
      replyMove: Option[String],
      continuationMoves: List[String],
      forcedTheme: Option[ForcedLineThemeEvidence]
  ): LineFactEvidence =
    new LineFactEvidence(line, firstMove, replyMove, continuationMoves, forcedTheme, None)()

  def apply(
      line: LineNodeRef,
      firstMove: Option[String],
      replyMove: Option[String],
      continuationMoves: List[String],
      forcedTheme: Option[ForcedLineThemeEvidence],
      material: Option[LineMaterialSummary]
  ): LineFactEvidence =
    new LineFactEvidence(line, firstMove, replyMove, continuationMoves, forcedTheme, material)()

final case class EvalFactEvidence(
    line: LineNodeRef,
    whitePovEvalCp: Int,
    mate: Option[Int],
    depth: Int
) extends EvidencePayload:
  val layer: EvidenceLayer = EvidenceLayer.Eval
  def evalPoint: PerspectiveMath.EvalPoint =
    PerspectiveMath.EvalPoint(whitePovEvalCp, mate)
  def whiteWinPercent: Double =
    PerspectiveMath.winPercentFromWhiteEval(whitePovEvalCp, mate)
  def winPercentFor(mover: Color): Double =
    PerspectiveMath.winPercentForMover(mover, whitePovEvalCp, mate)
  def winPercentAdvantageFor(mover: Color): Double =
    PerspectiveMath.winPercentAdvantageFor(mover, whitePovEvalCp, mate)

final case class MoveMotifProof(
    kind: String,
    category: MotifCategory,
    color: Color,
    subjectSquares: List[EvidenceSquare] = Nil,
    targetSquares: List[EvidenceSquare] = Nil,
    relatedSquares: List[EvidenceSquare] = Nil,
    relatedFiles: List[EvidenceFile] = Nil,
    roles: List[EvidencePieceRole] = Nil
):
  def focusSquares: List[EvidenceSquare] =
    (subjectSquares ++ targetSquares ++ relatedSquares).distinct

final case class MoveMotifEvent(
    rootMove: String,
    eventMove: Option[String],
    plyOffset: Int,
    line: Option[LineNodeRef],
    lineRole: Option[LineNodeRole],
    position: PositionNodeRef,
    scope: EvidenceScope,
    motif: Motif,
    proof: MoveMotifProof
):
  def isRootEvent: Boolean =
    eventMove.exists(move => EvidenceRef.sameMove(move, rootMove)) ||
      (eventMove.isEmpty && plyOffset == 0)
  def kind: String =
    proof.kind
  def category: MotifCategory =
    proof.category

object MoveMotifEvent:
  def fromMotif(
      rootMove: String,
      motif: Motif,
      position: PositionNodeRef,
      line: Option[LineNodeRef],
      scope: EvidenceScope
  ): MoveMotifEvent =
    MoveMotifEvent(
      rootMove = rootMove,
      eventMove = motif.move.map(EvidenceRef.normalizeMove),
      plyOffset = motif.plyIndex,
      line = line,
      lineRole = line.map(_.role),
      position = position,
      scope = scope,
      motif = motif,
      proof = proofFor(motif)
    )

  private def proofFor(motif: Motif): MoveMotifProof =
    val (subjectSquares, targetSquares, relatedSquares, roles) =
      motif match
        case Motif.PawnAdvance(file, fromRank, toRank, _, _, _) =>
          (Nil, Nil, List(squareKey(file, fromRank), squareKey(file, toRank)).flatten, List(Pawn))
        case Motif.PawnBreak(file, targetFile, _, _, _) =>
          (Nil, Nil, Nil, List(Pawn))
        case Motif.PawnPromotion(file, promotedTo, color, _, _) =>
          (Nil, List(squareKey(file, if color.white then 8 else 1)).flatten, Nil, List(Pawn, promotedTo))
        case Motif.PassedPawnPush(file, toRank, _, _, _) =>
          (Nil, Nil, List(squareKey(file, toRank)).flatten, List(Pawn))
        case Motif.RookLift(file, fromRank, toRank, _, _, _) =>
          (Nil, Nil, List(squareKey(file, fromRank), squareKey(file, toRank)).flatten, List(Rook))
        case Motif.Outpost(piece, square, _, _, _) =>
          (List(evidenceSquare(square)), Nil, Nil, List(piece))
        case Motif.Centralization(piece, square, _, _, _) =>
          (List(evidenceSquare(square)), Nil, Nil, List(piece))
        case Motif.Check(piece, targetSquare, _, _, _, _) =>
          (Nil, List(evidenceSquare(targetSquare)), Nil, List(piece, King))
        case Motif.Capture(piece, captured, square, _, _, _, _, _) =>
          (Nil, List(evidenceSquare(square)), Nil, List(piece, captured))
        case Motif.Zwischenzug(_, _, expectedRecaptureSquare, _, _, _) =>
          (Nil, List(evidenceSquare(expectedRecaptureSquare)), Nil, Nil)
        case Motif.Pin(pinningPiece, pinnedPiece, targetBehind, _, _, _, pinningSq, pinnedSq, behindSq) =>
          (
            pinningSq.map(evidenceSquare).toList,
            pinnedSq.map(evidenceSquare).toList,
            behindSq.map(evidenceSquare).toList,
            List(pinningPiece, pinnedPiece, targetBehind)
          )
        case Motif.Fork(attackingPiece, targets, square, targetSquares, _, _, _) =>
          (List(evidenceSquare(square)), targetSquares.map(evidenceSquare), Nil, attackingPiece :: targets)
        case Motif.Domination(dominatingPiece, dominatedPiece, square, _, _, _) =>
          (List(evidenceSquare(square)), Nil, Nil, List(dominatingPiece, dominatedPiece))
        case Motif.Skewer(attackingPiece, frontPiece, backPiece, _, _, _, attackingSq, frontSq, backSq) =>
          (
            attackingSq.map(evidenceSquare).toList,
            frontSq.map(evidenceSquare).toList,
            backSq.map(evidenceSquare).toList,
            List(attackingPiece, frontPiece, backPiece)
          )
        case Motif.DiscoveredAttack(movingPiece, attackingPiece, target, _, _, _, movingSq, attackingSq, targetSq) =>
          (
            movingSq.map(evidenceSquare).toList ++ attackingSq.map(evidenceSquare).toList,
            targetSq.map(evidenceSquare).toList,
            Nil,
            List(movingPiece, attackingPiece, target)
          )
        case Motif.RemovingTheDefender(attacker, victim, protectedTarget, square, _, _, _) =>
          (Nil, List(evidenceSquare(square)), Nil, List(attacker, victim, protectedTarget))
        case Motif.Deflection(piece, fromSquare, _, _, _) =>
          (List(evidenceSquare(fromSquare)), Nil, Nil, List(piece))
        case Motif.Decoy(piece, toSquare, _, _, _) =>
          (Nil, List(evidenceSquare(toSquare)), Nil, List(piece))
        case Motif.XRay(piece, target, square, _, _, _) =>
          (List(evidenceSquare(square)), Nil, Nil, List(piece, target))
        case Motif.Overloading(overloadedPiece, overloadedSquare, duties, _, _, _) =>
          (List(evidenceSquare(overloadedSquare)), duties.map(evidenceSquare), Nil, List(overloadedPiece))
        case Motif.DoubleCheck(movingPiece, revealedPiece, _, _, _) =>
          (Nil, Nil, Nil, List(movingPiece, revealedPiece, King))
        case Motif.BackRankMate(_, attackingPiece, _, _, _) =>
          (Nil, Nil, Nil, List(attackingPiece, King))
        case Motif.TrappedPiece(trappedRole, trappedSquare, _, _, _) =>
          (Nil, List(evidenceSquare(trappedSquare)), Nil, List(trappedRole))
        case Motif.MateNet(kingSquare, attackers, _, _, _) =>
          (Nil, List(evidenceSquare(kingSquare)), Nil, King :: attackers)
        case Motif.Interference(interferingPiece, interferingSquare, blockedPiece1, blockedPiece2, _, _, _) =>
          (List(evidenceSquare(interferingSquare)), Nil, Nil, List(interferingPiece, blockedPiece1, blockedPiece2))
        case Motif.Clearance(clearingPiece, clearingFrom, _, beneficiary, _, _, _) =>
          (List(evidenceSquare(clearingFrom)), Nil, Nil, List(clearingPiece, beneficiary))
        case Motif.DoubledPieces(role, file, _, _, _) =>
          (Nil, Nil, Nil, List(role))
        case Motif.Battery(front, back, _, _, _, _, frontSq, backSq) =>
          (frontSq.map(evidenceSquare).toList, backSq.map(evidenceSquare).toList, Nil, List(front, back))
        case Motif.IsolatedPawn(file, rank, _, _, _) =>
          (Nil, List(squareKey(file, rank)).flatten, Nil, List(Pawn))
        case Motif.BackwardPawn(file, rank, _, _, _) =>
          (Nil, List(squareKey(file, rank)).flatten, Nil, List(Pawn))
        case Motif.PassedPawn(file, rank, _, _, _, _) =>
          (Nil, List(squareKey(file, rank)).flatten, Nil, List(Pawn))
        case Motif.DoubledPawns(file, _, _, _) =>
          (Nil, Nil, Nil, List(Pawn))
        case Motif.PawnChain(baseFile, tipFile, _, _, _) =>
          (Nil, Nil, Nil, List(Pawn))
        case Motif.Opposition(opponentKingSquare, ownKingSquare, _, _, _, _) =>
          (List(evidenceSquare(ownKingSquare)), List(evidenceSquare(opponentKingSquare)), Nil, List(King))
        case Motif.KingStep(_, _, _, move) =>
          val normalized = move.map(_.trim.toLowerCase).getOrElse("")
          val from = Square.fromKey(normalized.take(2)).map(evidenceSquare).toList
          val to = Square.fromKey(normalized.slice(2, 4)).map(evidenceSquare).toList
          (from, to, Nil, List(King))
        case Motif.Castling(side, color, _, _) =>
          val rank = if color.white then "1" else "8"
          val (kingTo, rookFrom, rookTo) =
            if side == Motif.CastlingSide.Kingside then (s"g$rank", s"h$rank", s"f$rank")
            else (s"c$rank", s"a$rank", s"d$rank")
          val subject = List(s"e$rank", rookFrom).flatMap(Square.fromKey).map(evidenceSquare)
          val target = List(kingTo, rookTo).flatMap(Square.fromKey).map(evidenceSquare)
          (subject, target, Nil, List(King, Rook))
        case Motif.OpenFileControl(file, _, _, _) =>
          (Nil, Nil, Nil, Nil)
        case Motif.SemiOpenFileControl(file, _, _, _) =>
          (Nil, Nil, Nil, Nil)
        case Motif.RookBehindPassedPawn(file, _, _, _) =>
          (Nil, Nil, Nil, List(Rook, Pawn))
        case Motif.KingCutOff(_, coordinate, _, _, _) =>
          (Nil, Nil, Nil, List(King))
        case Motif.Blockade(piece, square, pawnSquare, _, _, _) =>
          (List(evidenceSquare(square)), List(evidenceSquare(pawnSquare)), Nil, List(piece, Pawn))
        case Motif.SmotheredMate(_, kingSquare, _, _) =>
          (Nil, List(evidenceSquare(kingSquare)), Nil, List(Knight, King))
        case _ =>
          (Nil, Nil, Nil, Nil)
    MoveMotifProof(
      kind = motif.getClass.getSimpleName.stripSuffix("$"),
      category = motif.category,
      color = motif.color,
      subjectSquares = subjectSquares.distinct,
      targetSquares = targetSquares.distinct,
      relatedSquares = relatedSquares.distinct,
      relatedFiles = filesFor(motif),
      roles = roles.distinct.map(role => EvidencePieceRole(role.toString))
    )

  private def filesFor(motif: Motif): List[EvidenceFile] =
    val files =
      motif match
        case Motif.PawnAdvance(file, _, _, _, _, _)         => List(file)
        case Motif.PawnBreak(file, targetFile, _, _, _)     => List(file, targetFile)
        case Motif.PawnPromotion(file, _, _, _, _)          => List(file)
        case Motif.PassedPawnPush(file, _, _, _, _)         => List(file)
        case Motif.RookLift(file, _, _, _, _, _)            => List(file)
        case Motif.DoubledPieces(_, file, _, _, _)          => List(file)
        case Motif.IsolatedPawn(file, _, _, _, _)           => List(file)
        case Motif.BackwardPawn(file, _, _, _, _)           => List(file)
        case Motif.PassedPawn(file, _, _, _, _, _)          => List(file)
        case Motif.DoubledPawns(file, _, _, _)              => List(file)
        case Motif.PawnChain(baseFile, tipFile, _, _, _)    => List(baseFile, tipFile)
        case Motif.OpenFileControl(file, _, _, _)           => List(file)
        case Motif.SemiOpenFileControl(file, _, _, _)       => List(file)
        case Motif.RookBehindPassedPawn(file, _, _, _)      => List(file)
        case _                                              => Nil
    files.distinct.map(file => EvidenceFile(file.toString.toLowerCase))

  private def evidenceSquare(square: Square): EvidenceSquare =
    EvidenceSquare(square.key)

  private def squareKey(file: chess.File, rank: Int): Option[EvidenceSquare] =
    Square.fromKey(s"${file.toString.toLowerCase}$rank").map(evidenceSquare)

final case class MoveMotifEvidence(
    event: MoveMotifEvent
) extends EvidencePayload:
  val layer: EvidenceLayer = EvidenceLayer.MoveMotif
  def moveUci: String = event.rootMove
  def rootMove: String = event.rootMove
  def motif: Motif = event.motif
  def proof: MoveMotifProof = event.proof
  def eventMove: Option[String] = event.eventMove
  def plyOffset: Int = event.plyOffset
  def line: Option[LineNodeRef] = event.line
  def lineRole: Option[LineNodeRole] = event.lineRole
  def isRootEvent: Boolean = event.isRootEvent
  def recordLineBound(ref: EvidenceRef): Boolean =
    isRootEvent &&
      line.forall(boundLine => ref.line.contains(boundLine)) &&
      ref.line.forall(lineRef => EvidenceRef.sameMove(lineRef.rootMove, moveUci))
  def sameMotifContext(ref: EvidenceRef, otherRef: EvidenceRef, other: MoveMotifEvidence): Boolean =
    EvidenceRef.sameMove(rootMove, other.rootMove) &&
      (
        (ref.line, otherRef.line) match
          case (Some(left), Some(right)) => left == right
          case (None, None)             => ref.position == otherRef.position && ref.scope == otherRef.scope
          case _                        => false
      )

final case class MoveTransitionEvidence(
    moveUci: String,
    from: PositionNodeRef,
    to: PositionNodeRef
) extends EvidencePayload:
  val layer: EvidenceLayer = EvidenceLayer.MoveTransition

enum StructuralSignalPolarity:
  case Gain
  case Loss
  case Neutral

enum StructuralSignalKind:
  case FileOpened
  case SemiOpenFileCreated
  case FileAccessChanged
  case FileOccupied
  case WeakPawnCreated
  case WeakSquareCreated
  case PawnTensionCreated
  case PawnTensionResolved
  case PawnTensionChanged
  case TargetPressureCreated
  case TargetPressureReleased
  case TargetPressureChanged
  case CenterControlChanged
  case DevelopmentChanged
  case DevelopmentChoice
  case MobilityChanged
  case KingSafetyChanged
  case LineUnlocked
  case PassedPawnCreated
  case PassedPawnAdvanced
  case PromotionPressureChanged
  case OutpostCreated
  case OutpostRemoved
  case RookLiftCreated
  case BatteryCreated
  case KingRingPressureChanged

final case class StructuralSignal(
    kind: StructuralSignalKind,
    polarity: StructuralSignalPolarity,
    magnitude: Int,
    subjects: List[String] = Nil
):
  def anchorKey: String =
    s"$kind:$polarity"

enum TransitionConsequenceKind:
  case OpenFileGain
  case SemiOpenFileGain
  case FileOccupationGain
  case WeakPawnTargetCreated
  case WeakSquareTargetCreated
  case PawnTensionGain
  case PawnTensionResolution
  case TargetPressureGain
  case TargetPressureRelease
  case CenterControlGain
  case CenterControlLoss
  case DevelopmentLagReduced
  case DevelopmentLagIncreased
  case DevelopmentPieceActivated
  case DevelopmentPieceRetreated
  case DevelopmentMobilityGain
  case DevelopmentMobilityLoss
  case DevelopmentCenterControlGain
  case DevelopmentCenterControlLoss
  case DevelopmentSafePlacement
  case DevelopmentUnsafePlacement
  case MobilityGain
  case MobilityLoss
  case LineUnlockGain
  case FileAccessGain
  case FileAccessLoss
  case KingSafetyPressure
  case KingSafetyConcession
  case PassedPawnProgress
  case PassedPawnConcession
  case PromotionPressureGain
  case PromotionPressureConcession
  case OutpostGain
  case OutpostConcession
  case RookLiftActivation
  case BatteryPressureGain
  case OpponentMobilityRestriction
  case KingRingPressureGain
  case KingRingPressureConcession

enum TransitionConsequenceCategory:
  case PawnStructure
  case PawnStructureDelta
  case Development
  case PieceActivity
  case TargetPressure
  case CenterControl
  case StructuralAnchor
  case StrategicMove
  case StrategicSupport
  case PlanAnchor
  case OpeningCenterControl
  case OpeningDevelopment

final case class TransitionConsequence(
    kind: TransitionConsequenceKind,
    polarity: StructuralSignalPolarity,
    strength: Int,
    subjects: List[String] = Nil
):
  def positive: Boolean =
    polarity == StructuralSignalPolarity.Gain
  def negative: Boolean =
    polarity == StructuralSignalPolarity.Loss
  def anchorKey: String =
    s"$kind:$polarity"

final case class StructuralDevelopmentChoice(
    role: String,
    from: String,
    to: String
)

final case class StructuralTransitionBinding(
    moveUci: String,
    role: TransitionEdgeRole,
    from: PositionNodeRef,
    to: PositionNodeRef,
    line: Option[LineNodeRef],
    perspective: Color
)

final case class RelationFactEvidence(
    kind: RelationFactKind,
    focusSquares: List[EvidenceSquare],
    targetSquare: Option[EvidenceSquare],
    lineMoves: List[String],
    participants: List[RelationParticipant]
)(val witnessProof: RelationWitnessProof = RelationWitnessProof.empty
) extends EvidencePayload:
  val layer: EvidenceLayer = EvidenceLayer.Relation
  def detail: RelationWitnessDetail =
    witnessProof.detail
  def hasTypedWitness: Boolean =
    witnessProof.hasTypedDetail
  def proofAtoms: List[RelationProofAtom] =
    witnessProof.proofAtoms
  def hasLineProof: Boolean =
    witnessProof.hasLineProof
  def lineProofCount: Int =
    proofAtoms.count(_.role == RelationProofAtomRole.LineMove)
  def hasParticipantProof: Boolean =
    proofAtoms.exists(_.role == RelationProofAtomRole.Participant)
  def hasConcreteRelationProof: Boolean =
    hasTypedWitness && proofAtoms.nonEmpty
  def mentionsLineMove(moveUci: String): Boolean =
    lineMoves.exists(EvidenceRef.sameMove(_, moveUci))

enum TacticalMechanismKind:
  case KingForcing
  case MaterialGain
  case RecaptureChoice
  case Tempo
  case RelationMechanism
  case Conversion
  case Refutation
  case DrawResource
  case PawnPromotion
  case DefensiveResource

object TacticalMechanismKind:
  def fromMotif(motif: Motif): List[TacticalMechanismKind] =
    motif match
      case m: Motif.Check =>
        List(TacticalMechanismKind.KingForcing) ++
          Option.when(m.checkType == Motif.CheckType.Mate || m.checkType == Motif.CheckType.Smothered)(
            TacticalMechanismKind.Refutation
          ).toList
      case _: Motif.DoubleCheck | _: Motif.BackRankMate | _: Motif.MateNet | _: Motif.SmotheredMate =>
        List(TacticalMechanismKind.KingForcing)
      case m: Motif.Capture =>
        m.captureType match
          case Motif.CaptureType.Recapture =>
            List(TacticalMechanismKind.RecaptureChoice)
          case Motif.CaptureType.Exchange | Motif.CaptureType.ExchangeSacrifice =>
            List(TacticalMechanismKind.MaterialGain, TacticalMechanismKind.Conversion)
          case Motif.CaptureType.Winning | Motif.CaptureType.Sacrifice =>
            List(TacticalMechanismKind.MaterialGain)
          case Motif.CaptureType.Normal =>
            Nil
      case _: Motif.Zwischenzug =>
        List(TacticalMechanismKind.Tempo, TacticalMechanismKind.RecaptureChoice)
      case _: Motif.Fork | _: Motif.Pin | _: Motif.Skewer | _: Motif.DiscoveredAttack |
          _: Motif.RemovingTheDefender | _: Motif.Deflection | _: Motif.Decoy | _: Motif.XRay |
          _: Motif.Overloading | _: Motif.Interference | _: Motif.Clearance | _: Motif.Battery =>
        List(TacticalMechanismKind.RelationMechanism)
      case _: Motif.TrappedPiece | _: Motif.Domination =>
        List(TacticalMechanismKind.MaterialGain)
      case _: Motif.PawnPromotion | _: Motif.PassedPawnPush =>
        List(TacticalMechanismKind.PawnPromotion)
      case _: Motif.StalemateThreat =>
        List(TacticalMechanismKind.DrawResource)
      case _ =>
        Nil

  def fromRelation(kind: RelationFactKind): TacticalMechanismKind =
    kind match
      case RelationFactKind.DoubleCheck | RelationFactKind.BackRankMate | RelationFactKind.MateNet | RelationFactKind.GreekGift =>
        TacticalMechanismKind.KingForcing
      case RelationFactKind.DefenderTrade =>
        TacticalMechanismKind.RecaptureChoice
      case RelationFactKind.HangingPiece | RelationFactKind.TrappedPiece | RelationFactKind.Domination =>
        TacticalMechanismKind.MaterialGain
      case RelationFactKind.Zwischenzug =>
        TacticalMechanismKind.Tempo
      case RelationFactKind.BadPieceLiquidation =>
        TacticalMechanismKind.Conversion
      case RelationFactKind.StalemateTrap | RelationFactKind.PerpetualCheck =>
        TacticalMechanismKind.DrawResource
      case _ =>
        TacticalMechanismKind.RelationMechanism

  def fromLineConsequence(kind: LineConsequenceKind): List[TacticalMechanismKind] =
    kind match
      case LineConsequenceKind.MaterialGain | LineConsequenceKind.MaterialLoss =>
        List(TacticalMechanismKind.MaterialGain)
      case LineConsequenceKind.RecaptureSequence | LineConsequenceKind.RecoveryWindow =>
        List(TacticalMechanismKind.RecaptureChoice)
      case LineConsequenceKind.ImmediateReplyCheck =>
        List(TacticalMechanismKind.Tempo)
      case LineConsequenceKind.Mate =>
        List(TacticalMechanismKind.KingForcing)
      case LineConsequenceKind.DrawResource =>
        List(TacticalMechanismKind.DrawResource)
      case LineConsequenceKind.PromotionRace =>
        List(TacticalMechanismKind.PawnPromotion)
      case LineConsequenceKind.ForcedTheme | LineConsequenceKind.Sacrifice =>
        Nil

  def relativeCauseKind(
      kind: TacticalMechanismKind,
      badLoss: Boolean,
      playedCandidate: Boolean
  ): RelativeCauseKind =
    kind match
      case TacticalMechanismKind.KingForcing =>
        RelativeCauseKind.KingForcing
      case TacticalMechanismKind.RecaptureChoice =>
        if badLoss then RelativeCauseKind.WrongRecapturer else RelativeCauseKind.RecaptureRecoveryWindow
      case TacticalMechanismKind.Tempo =>
        RelativeCauseKind.TempoLoss
      case TacticalMechanismKind.Conversion =>
        if badLoss then RelativeCauseKind.ConversionMiss else RelativeCauseKind.ConversionSecured
      case TacticalMechanismKind.DrawResource =>
        RelativeCauseKind.DrawResource
      case TacticalMechanismKind.DefensiveResource =>
        RelativeCauseKind.DefensiveResource
      case TacticalMechanismKind.MaterialGain | TacticalMechanismKind.RelationMechanism |
          TacticalMechanismKind.Refutation | TacticalMechanismKind.PawnPromotion =>
        if badLoss then
          if playedCandidate then RelativeCauseKind.TacticalRefutationOfPlayed
          else RelativeCauseKind.CandidateTacticalLiability
        else RelativeCauseKind.MissedTacticalResource

enum TacticalMechanismSignalKind:
  case Motif
  case Relation
  case LineConsequence
  case MateBranch
  case ThreatEpisode

final case class TacticalMechanismSignal(
    kind: TacticalMechanismSignalKind,
    label: String,
    sourceLayer: EvidenceLayer,
    source: Option[EvidenceRef] = None
)

final case class TacticalMechanismEvidence(
    kind: TacticalMechanismKind,
    moveUci: Option[String],
    line: Option[LineNodeRef],
    signals: List[TacticalMechanismSignal]
) extends EvidencePayload:
  val layer: EvidenceLayer = EvidenceLayer.TacticalMechanism
  def signalKinds: Set[TacticalMechanismSignalKind] =
    signals.map(_.kind).toSet
  def hasLineProof: Boolean =
    signalKinds.exists(kind =>
      kind == TacticalMechanismSignalKind.LineConsequence ||
        kind == TacticalMechanismSignalKind.Relation ||
        kind == TacticalMechanismSignalKind.MateBranch
    )
  def hasThreatProof: Boolean =
    signalKinds.contains(TacticalMechanismSignalKind.ThreatEpisode)
  def hasConcreteProof: Boolean =
    signals.nonEmpty && (hasLineProof || hasThreatProof)
  def hasEngineOrForcingProof: Boolean =
    signalKinds.exists(kind =>
      kind == TacticalMechanismSignalKind.MateBranch ||
        kind == TacticalMechanismSignalKind.LineConsequence ||
        kind == TacticalMechanismSignalKind.ThreatEpisode
    ) ||
      (kind == TacticalMechanismKind.Tempo &&
        signals.exists(signal => signal.kind == TacticalMechanismSignalKind.Relation && signal.label == RelationFactKind.Zwischenzug.toString))
  def tactical: Boolean =
    kind != TacticalMechanismKind.DefensiveResource &&
      kind != TacticalMechanismKind.DrawResource
  def defensive: Boolean =
    kind == TacticalMechanismKind.DefensiveResource ||
      kind == TacticalMechanismKind.DrawResource
  def canAnchorTacticalIdea: Boolean =
    tactical && hasConcreteProof && hasEngineOrForcingProof
  def canAnchorDefensiveIdea: Boolean =
    defensive && (hasThreatProof || hasLineProof)

final case class StructuralDeltaEvidence(
    transition: StructuralTransitionBinding,
    signals: List[StructuralSignal],
    consequences: List[TransitionConsequence],
    developmentChoices: List[StructuralDevelopmentChoice] = Nil
) extends EvidencePayload:
  val layer: EvidenceLayer = EvidenceLayer.StructuralDelta
  import StructuralSignalKind.*
  import TransitionConsequenceKind.*

  def moveUci: String = transition.moveUci
  def role: TransitionEdgeRole = transition.role
  def from: PositionNodeRef = transition.from
  def to: PositionNodeRef = transition.to
  def line: Option[LineNodeRef] = transition.line
  def perspective: Color = transition.perspective
  def hasSignals: Boolean = signals.nonEmpty
  def hasConsequences: Boolean = consequences.nonEmpty
  def hasTypedOutput: Boolean = hasSignals || hasConsequences
  def consequenceKinds: List[TransitionConsequenceKind] = consequences.map(_.kind).distinct
  def signalAnchors: List[String] = signals.map(_.anchorKey).distinct
  def consequenceAnchors: List[String] = consequences.map(_.anchorKey).distinct
  def signalsOf(kind: StructuralSignalKind): List[StructuralSignal] = signals.filter(_.kind == kind)
  def consequencesOf(kind: TransitionConsequenceKind): List[TransitionConsequence] = consequences.filter(_.kind == kind)
  def hasSignal(kind: StructuralSignalKind): Boolean = signals.exists(_.kind == kind)
  def hasConsequence(kind: TransitionConsequenceKind): Boolean = consequences.exists(_.kind == kind)
  def hasAnyConsequence(kinds: Set[TransitionConsequenceKind]): Boolean =
    consequences.exists(consequence => kinds.contains(consequence.kind))
  def hasConsequenceCategory(category: TransitionConsequenceCategory): Boolean =
    consequences.exists(consequence => StructuralDeltaEvidence.hasConsequenceCategory(consequence.kind, category))
  def positiveConsequences: List[TransitionConsequence] =
    consequences.filter(_.positive)
  def negativeConsequences: List[TransitionConsequence] =
    consequences.filter(_.negative)
  def meaningfulConsequences: List[TransitionConsequence] =
    consequences.filter(consequence =>
      consequence.strength > 0 &&
        consequence.polarity != StructuralSignalPolarity.Neutral
    )
  def hasMeaningfulConsequences: Boolean =
    meaningfulConsequences.nonEmpty
  def hasPawnStructureImprovement: Boolean =
    hasConsequenceCategory(TransitionConsequenceCategory.PawnStructure)
  def hasMeaningfulPawnStructureDelta: Boolean =
    hasPawnStructureImprovement
  def hasTargetPressureGain: Boolean =
    hasConsequence(TargetPressureGain)
  def hasTargetPressureRelease: Boolean =
    hasConsequence(TargetPressureRelease)
  def hasCenterControlGain: Boolean =
    hasConsequence(CenterControlGain)
  def hasDevelopmentActivation: Boolean =
    hasConsequenceCategory(TransitionConsequenceCategory.Development)
  def hasPieceActivityGain: Boolean =
    hasConsequenceCategory(TransitionConsequenceCategory.PieceActivity)
  def hasKingSafetyPressure: Boolean =
    hasConsequence(KingSafetyPressure)
  def hasPassedPawnProgress: Boolean =
    hasConsequence(PassedPawnProgress)
  def hasOutpostGain: Boolean =
    hasConsequence(OutpostGain)
  def hasRookLiftActivation: Boolean =
    hasConsequence(RookLiftActivation)
  def hasBatteryPressureGain: Boolean =
    hasConsequence(BatteryPressureGain)
  def hasOpponentMobilityRestriction: Boolean =
    consequencesOf(OpponentMobilityRestriction).exists(consequence =>
      consequence.subjects.exists(StructuralDeltaEvidence.validOpponentMobilityRestrictionSubject)
    )
  def hasKingRingPressureGain: Boolean =
    hasConsequence(KingRingPressureGain)
  def hasStrategicConcession: Boolean =
    strategicConcessions.nonEmpty
  def strategicConcessions: List[TransitionConsequence] =
    negativeConsequences.filter(consequence =>
      StructuralDeltaEvidence.hasConsequenceCategory(consequence.kind, TransitionConsequenceCategory.StrategicSupport)
    )
  def hasPawnStructureDelta: Boolean =
    hasConsequenceCategory(TransitionConsequenceCategory.PawnStructureDelta)
  def hasPawnTensionDelta: Boolean =
    hasAnyConsequence(Set(PawnTensionGain, PawnTensionResolution))
  def hasStructuralAnchor: Boolean =
    hasConsequenceCategory(TransitionConsequenceCategory.StructuralAnchor)
  def hasStrategicMoveDelta: Boolean =
    hasConsequenceCategory(TransitionConsequenceCategory.StrategicMove)
  def hasStrategicSupport: Boolean =
    hasConsequenceCategory(TransitionConsequenceCategory.StrategicSupport)
  def hasPositivePlanAnchor: Boolean =
    positiveConsequences.exists(consequence =>
      StructuralDeltaEvidence.hasConsequenceCategory(consequence.kind, TransitionConsequenceCategory.PlanAnchor) ||
        consequence.kind == PassedPawnProgress
    )
  def structuralImprovementScore: Int =
    positiveConsequences
      .filterNot(consequence => consequence.kind == KingSafetyPressure)
      .map(_.strength)
      .sum
  def structuralImprovementConsequenceKinds: List[TransitionConsequenceKind] =
    positiveConsequences
      .map(_.kind)
      .filter(StructuralDeltaEvidence.isStructuralAnchorConsequence)
      .distinct
  def createdTargetPressureSubjects: List[String] =
    subjectsOfSignal(TargetPressureCreated)
  def releasedTargetPressureSubjects: List[String] =
    subjectsOfSignal(TargetPressureReleased)

  private def subjectsOfSignal(kind: StructuralSignalKind): List[String] =
    signalsOf(kind).flatMap(_.subjects).distinct

object StructuralDeltaEvidence:
  import TransitionConsequenceKind.*
  import TransitionConsequenceCategory.*

  val pawnStructureImprovementConsequences: Set[TransitionConsequenceKind] =
    consequenceKindsFor(PawnStructure)

  val pawnStructureDeltaConsequences: Set[TransitionConsequenceKind] =
    consequenceKindsFor(PawnStructureDelta)

  val developmentActivationConsequences: Set[TransitionConsequenceKind] =
    consequenceKindsFor(Development)

  val pieceActivityGainConsequences: Set[TransitionConsequenceKind] =
    consequenceKindsFor(PieceActivity)

  def structuralImprovementConsequenceKinds(records: Iterable[EvidenceRecord]): List[TransitionConsequenceKind] =
    records.collect { case EvidenceRecord(_, payload: StructuralDeltaEvidence, _) =>
      payload.structuralImprovementConsequenceKinds
    }.flatten.toList.distinct.sortBy(_.toString)

  def hasConsequenceCategory(kind: TransitionConsequenceKind, category: TransitionConsequenceCategory): Boolean =
    consequenceCategories.getOrElse(kind, Set.empty).contains(category)

  def isStructuralAnchorConsequence(kind: TransitionConsequenceKind): Boolean =
    hasConsequenceCategory(kind, StructuralAnchor)

  def isStrategicSupportConsequence(kind: TransitionConsequenceKind): Boolean =
    hasConsequenceCategory(kind, StrategicSupport)

  private[chessjudgment] def validOpponentMobilityRestrictionSubject(subject: String): Boolean =
    val normalized = Option(subject).getOrElse("").trim.toLowerCase
    normalized match
      case opponentMobilityRestrictionSubject(bishopSquare, blockerSquare, before, after) =>
        fianchettoBishopSquare(bishopSquare) &&
          centralDiagonalBlockerSquare(blockerSquare) &&
          after.toIntOption.exists(afterValue => before.toIntOption.exists(beforeValue => afterValue < beforeValue))
      case _ =>
        false

  private def fianchettoBishopSquare(square: String): Boolean =
    Set("g7", "b7", "g2", "b2").contains(square.toLowerCase)

  private def centralDiagonalBlockerSquare(square: String): Boolean =
    square.toLowerCase.matches("[c-f][45]")

  private def consequenceKindsFor(category: TransitionConsequenceCategory): Set[TransitionConsequenceKind] =
    consequenceCategories.collect { case (kind, categories) if categories.contains(category) => kind }.toSet

  private val opponentMobilityRestrictionSubject =
    raw"bishop:([a-h][1-8]):diagonal-denial:blocked-by:([a-h][1-8]):locked-center:mobility-([0-9]+)-to-([0-9]+)".r

  private lazy val consequenceCategories: Map[TransitionConsequenceKind, Set[TransitionConsequenceCategory]] =
    Map(
      OpenFileGain -> Set(PawnStructure, PawnStructureDelta, StructuralAnchor, StrategicMove, StrategicSupport),
      SemiOpenFileGain -> Set(PawnStructure, PawnStructureDelta, StructuralAnchor, StrategicMove, StrategicSupport),
      FileOccupationGain -> Set(PawnStructure, PawnStructureDelta, PieceActivity, StructuralAnchor, StrategicMove, StrategicSupport),
      WeakPawnTargetCreated -> Set(PawnStructure, PawnStructureDelta, StructuralAnchor, StrategicMove, StrategicSupport),
      WeakSquareTargetCreated -> Set(PawnStructure, PawnStructureDelta, StructuralAnchor, StrategicMove, StrategicSupport),
      PawnTensionGain -> Set(PawnStructure, PawnStructureDelta, StructuralAnchor, StrategicMove, StrategicSupport),
      PawnTensionResolution -> Set(PawnStructureDelta),
      TargetPressureGain -> Set(TargetPressure, StructuralAnchor, StrategicMove, StrategicSupport, PlanAnchor),
      TargetPressureRelease -> Set(TargetPressure, StrategicSupport),
      CenterControlGain -> Set(CenterControl, OpeningCenterControl, StructuralAnchor, StrategicMove, StrategicSupport, PlanAnchor),
      CenterControlLoss -> Set(CenterControl, StrategicSupport),
      DevelopmentLagReduced -> Set(Development, StructuralAnchor, StrategicMove, StrategicSupport, PlanAnchor, OpeningDevelopment),
      DevelopmentPieceActivated -> Set(Development, StructuralAnchor, StrategicMove, StrategicSupport, PlanAnchor, OpeningDevelopment),
      DevelopmentMobilityGain -> Set(Development, StructuralAnchor, StrategicMove, StrategicSupport, PlanAnchor, OpeningDevelopment),
      DevelopmentCenterControlGain -> Set(Development, CenterControl, OpeningCenterControl, StructuralAnchor, StrategicMove, StrategicSupport, PlanAnchor),
      DevelopmentSafePlacement -> Set(Development, StructuralAnchor, StrategicMove, StrategicSupport, PlanAnchor, OpeningDevelopment),
      DevelopmentLagIncreased -> Set(Development, StrategicSupport),
      DevelopmentPieceRetreated -> Set(Development, StrategicSupport),
      DevelopmentMobilityLoss -> Set(Development, StrategicSupport),
      DevelopmentCenterControlLoss -> Set(Development, CenterControl, StrategicSupport),
      DevelopmentUnsafePlacement -> Set(Development, StrategicSupport),
      MobilityGain -> Set(PieceActivity, StructuralAnchor, StrategicMove, StrategicSupport, PlanAnchor, OpeningDevelopment),
      MobilityLoss -> Set(PieceActivity, StrategicSupport),
      LineUnlockGain -> Set(PieceActivity, StructuralAnchor, StrategicMove, StrategicSupport, PlanAnchor),
      FileAccessGain -> Set(StructuralAnchor, StrategicMove, StrategicSupport, PlanAnchor),
      FileAccessLoss -> Set(StrategicSupport),
      KingSafetyPressure -> Set(StrategicMove, StrategicSupport, PlanAnchor),
      KingSafetyConcession -> Set(StrategicSupport),
      PassedPawnProgress -> Set(StructuralAnchor, StrategicMove, StrategicSupport),
      PassedPawnConcession -> Set(StrategicSupport),
      PromotionPressureGain -> Set(StructuralAnchor, StrategicMove, StrategicSupport),
      PromotionPressureConcession -> Set(StrategicSupport),
      OutpostGain -> Set(StructuralAnchor, StrategicMove, StrategicSupport),
      OutpostConcession -> Set(StrategicSupport),
      RookLiftActivation -> Set(StructuralAnchor, StrategicMove, StrategicSupport),
      BatteryPressureGain -> Set(PieceActivity, StructuralAnchor, StrategicMove, StrategicSupport),
      OpponentMobilityRestriction -> Set(StrategicMove, StrategicSupport, PlanAnchor),
      KingRingPressureGain -> Set(StructuralAnchor, StrategicMove, StrategicSupport),
      KingRingPressureConcession -> Set(StrategicSupport)
    )

final case class PlanTransitionEvidence(
    transition: PlanSequenceSummary
) extends EvidencePayload:
  val layer: EvidenceLayer = EvidenceLayer.PlanTransition

final case class PlanPressureEvidence(
    scoring: PlanScoringResult,
    activePlans: ActivePlans
) extends EvidencePayload:
  val layer: EvidenceLayer = EvidenceLayer.PlanPressure

final case class CandidateComparisonEvidence(
    comparison: CandidateComparisonFact
) extends EvidencePayload:
  val layer: EvidenceLayer = EvidenceLayer.CandidateComparison

final case class CounterfactualFactEvidence(
    referenceLine: LineNodeRef,
    candidateLine: LineNodeRef,
    comparison: EvalComparison
) extends EvidencePayload:
  val layer: EvidenceLayer = EvidenceLayer.Counterfactual

final case class RelativeAssessmentEvidence(
    assessment: RelativeMoveAssessment
) extends EvidencePayload:
  val layer: EvidenceLayer = EvidenceLayer.RelativeAssessment

final case class RelativeCauseFactEvidence(
    cause: RelativeCauseFact
) extends EvidencePayload:
  val layer: EvidenceLayer = EvidenceLayer.RelativeCause

final case class MoveVerdictCertificationEvidence(
    certification: MoveVerdictCertification
) extends EvidencePayload:
  val layer: EvidenceLayer = EvidenceLayer.MoveVerdictCertification

final case class ChessIdeaEvidence(
    idea: ChessIdeaRef
) extends EvidencePayload:
  val layer: EvidenceLayer = EvidenceLayer.ChessIdea

final case class ClaimEvidence(
    claimId: String
) extends EvidencePayload:
  val layer: EvidenceLayer = EvidenceLayer.Claim

final case class EvidenceRecord(
    ref: EvidenceRef,
    payload: EvidencePayload,
    parents: List[EvidenceRef] = Nil
):
  def payloadLineRefs: List[LineNodeRef] =
    payload match
      case lineFact: LineFactEvidence =>
        List(lineFact.line)
      case EvalFactEvidence(payloadLine, _, _, _) =>
        List(payloadLine)
      case CandidateComparisonEvidence(fact) =>
        List(fact.referenceLine, fact.candidateLine)
      case CounterfactualFactEvidence(referenceLine, candidateLine, _) =>
        List(referenceLine, candidateLine)
      case RelativeAssessmentEvidence(assessment) =>
        List(assessment.reference.ref, assessment.candidate.ref)
      case RelativeCauseFactEvidence(cause) =>
        List(cause.eventLine)
      case payload: TacticalMechanismEvidence =>
        payload.line.toList
      case _: StrategicMechanismEvidence =>
        ref.line.toList
      case payload: StrategicMechanismContrastEvidence =>
        List(payload.referenceLine, payload.candidateLine)
      case MoveVerdictCertificationEvidence(certification) =>
        certification.causes.map(_.eventLine).distinct
      case _ =>
        Nil
  def referencesLine(line: LineNodeRef): Boolean =
    ref.line.contains(line) || payloadLineRefs.contains(line)
  def carriesLinePayload(line: LineNodeRef, layer: EvidenceLayer): Boolean =
    ref.layer == layer && ref.line.contains(line) && payloadLineRefs.contains(line)
  def hasConcreteLineSignal: Boolean =
    payload match
      case payload: LineFactEvidence =>
        payload.hasConcreteLineConsequence
      case EvalFactEvidence(_, _, mate, _) =>
        mate.nonEmpty
      case payload: RelationFactEvidence =>
        payload.hasLineProof
      case payload: TacticalMechanismEvidence =>
        payload.hasLineProof
      case _ =>
        false
  def hasRootCaptureEvent(rootMove: String): Boolean =
    payload match
      case payload: LineFactEvidence =>
        payload.hasRootCaptureEvent(rootMove)
      case _ =>
        false

object EvidenceRecord:
  def hasConcreteLineSignal(records: List[EvidenceRecord]): Boolean =
    records.exists(_.hasConcreteLineSignal)

  def rootCaptureRecords(records: List[EvidenceRecord], rootMove: String): List[EvidenceRecord] =
    records.filter(_.hasRootCaptureEvent(rootMove))

  def hasRootCaptureEvent(records: List[EvidenceRecord], rootMove: String): Boolean =
    rootCaptureRecords(records, rootMove).nonEmpty

final case class TypedEvidenceGraph(
    records: List[EvidenceRecord]
):
  lazy val byId: Map[String, EvidenceRecord] =
    records.map(record => record.ref.id -> record).toMap

  def refs(layer: EvidenceLayer): List[EvidenceRef] =
    records.collect { case record if record.payload.layer == layer => record.ref }

  def recordsFor(position: PositionNodeRef): List[EvidenceRecord] =
    records.filter(_.ref.position == position)

  def recordsFor(line: LineNodeRef): List[EvidenceRecord] =
    records.filter(_.ref.line.contains(line))

  def add(record: EvidenceRecord): TypedEvidenceGraph =
    copy(records = records.filterNot(_.ref.id == record.ref.id) :+ record)

object TypedEvidenceGraph:
  val empty: TypedEvidenceGraph = TypedEvidenceGraph(Nil)
