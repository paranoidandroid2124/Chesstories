package lila.chessjudgment.model.judgment

import chess.*
import lila.chessjudgment.model.evaluation.{ JudgmentThresholds, PerspectiveMath }
import lila.chessjudgment.model.line.PrincipalVariationEvidence
import lila.chessjudgment.model.position.{ PawnTopology, PositionFeatures }
import lila.chessjudgment.model.{ ActivePlans, BranchReplyProbeBinding, Fact, Motif, MotifCategory, PlanEventIdentity, PlanId, PlanMatch, PlanScoringResult, PlanSequenceSummary, TransitionType }
import lila.chessjudgment.model.structure.{ PlanAlignment, StructureId, StructureProfile }
import lila.chessjudgment.model.strategic.PlanContinuity
import lila.chessjudgment.model.strategic.PlanTaxonomy.{ PlanKind, PlanTheme }

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
  case PlanCausalEvent
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
  def hasConcreteObject: Boolean =
    target.nonEmpty ||
      (actor.nonEmpty && (mechanism.nonEmpty || consequence.nonEmpty))
  def specificTargetMechanismReady: Boolean =
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
  private val ConcretePieceRoleKeys = Set("pawn", "knight", "bishop", "rook", "queen", "king")


  def fromClaim(claim: JudgmentClaim, graph: TypedEvidenceGraph): List[EvidenceObjectBinding] =
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

  private[chessjudgment] def specificTargetMechanismReadySignatures(signatures: Iterable[String]): Boolean =
    signatures.exists(specificTargetMechanismReadySignature)

  private def specificTargetMechanismReadySignature(signature: String): Boolean =
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
      normalized.startsWith("target=piece:")

  private[chessjudgment] def specificSurfaceTargetObject(obj: ConcreteChessObject): Boolean =
    obj.kind match
      case EvidenceObjectKind.Square | EvidenceObjectKind.File | EvidenceObjectKind.Pawn | EvidenceObjectKind.Piece =>
        true
      case _ =>
        false

  private[chessjudgment] def goalTargetObjectGroups(
      consequence: TransitionConsequence
  ): List[Set[ConcreteChessObject]] =
    consequence.goalSubjects.flatMap { subject =>
      val group = subjectObject(subject)
        .filter(obj => specificSurfaceTargetObject(obj) && obj.kind != EvidenceObjectKind.PlanSubject)
        .toSet
      val pieceRolesValid = group.collect {
        case obj if obj.kind == EvidenceObjectKind.Piece => obj.key.toLowerCase
      }.forall(ConcretePieceRoleKeys)
      Option.when(group.nonEmpty && pieceRolesValid)(group)
    }.distinct

  private[chessjudgment] def goalTargetObjects(
      consequence: TransitionConsequence
  ): Set[ConcreteChessObject] =
    goalTargetObjectGroups(consequence).flatten.toSet

  def specificTargetMechanismReady(bindings: List[EvidenceObjectBinding]): Boolean =
    bindings.exists(_.specificTargetMechanismReady)

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
                target = objectOf(EvidenceObjectKind.PlanSubject, plan.id),
                mechanism = objectOf(EvidenceObjectKind.Mechanism, payload.kind.toString),
                consequence = objectOf(EvidenceObjectKind.Consequence, payload.kind.toString),
                witness = objectOf(EvidenceObjectKind.PlanSubject, plan.id),
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
        case payload: PlanPressureEvidence =>
          fromPlanPressure(record.ref, payload)
        case payload: PlanCausalEventEvidence =>
          fromPlanCausalEvent(record.ref, payload)
        case PlanTransitionEvidence(transition) =>
          transition.currentEvent.toList.map(current =>
            EvidenceObjectBinding(
              source = record.ref,
              actor = transition.previousEvent.toList.flatMap(previous =>
                moveObjects(previous.rootMove) ++
                  previous.actorRole.toList.flatMap(objectOf(EvidenceObjectKind.Piece, _))
              ),
              target = (
                objectOf(EvidenceObjectKind.PlanSubject, current.goalKey) ++
                  current.targets.flatMap(subjectObject)
              ).distinctBy(_.signaturePart),
              mechanism = objectOf(EvidenceObjectKind.Mechanism, "plan-transition"),
              consequence = objectOf(EvidenceObjectKind.Consequence, transition.transitionType.toString),
              witness = (
                transition.continuity.toList.flatMap(_.supportingMoves).flatMap(move => objectOf(EvidenceObjectKind.Move, move)) ++
                  objectOf(EvidenceObjectKind.Move, current.rootMove)
              ).distinctBy(_.signaturePart),
              line = record.ref.line,
              horizon = transition.continuity.map(continuity => s"${continuity.consecutivePlies}-ply")
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
      case Fact.KingActivity(color, _, _, _, _) =>
        objectOf(EvidenceObjectKind.Side, colorKey(color))
      case Fact.Opposition(color, _, _, _, _, _, _) =>
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
        val sacrificeCaptureTargets =
          Option
            .when(
              consequence.kind == LineConsequenceKind.Sacrifice &&
                consequence.rootMove.exists(root => payload.rootMove.exists(EvidenceRef.sameMove(_, root)))
            )(consequence.stationarySacrificeCaptures.flatMap(capture =>
              squareObject(Some(capture.square)) ++
                roleObject(Some(capture.capturedRole)) ++
                objectOf(EvidenceObjectKind.PlanSubject, s"material-sacrifice:${capture.square.key}")
            ))
            .toList
            .flatten
        val rootMoveSacrificeTargets =
          Option
            .when(
              consequence.kind == LineConsequenceKind.Sacrifice &&
                consequence.rootMove.exists(root => payload.rootMove.exists(EvidenceRef.sameMove(_, root)))
            )(eventMove.toList.flatMap(move => moveTargetSquare(move).flatMap(target =>
              objectOf(EvidenceObjectKind.PlanSubject, s"material-sacrifice:${target.key}")
            )))
            .toList
            .flatten
        val lineMoveWitness =
          consequenceMoves.flatMap(move => objectOf(EvidenceObjectKind.Move, move) ++ moveTargetSquare(move))
        val lineEventWitness =
          payload.lineEvents
            .filter(event => consequenceMoves.contains(normalize(event.moveUci)))
            .flatMap(event => squareObject(event.square) ++ roleObject(event.pieceRole) ++ roleObject(event.targetRole))
        EvidenceObjectBinding(
          source = ref,
          actor = eventMove.toList.flatMap(moveObjects),
          target = eventMove.toList.flatMap(moveTargetSquare) ++ sacrificeCaptureTargets ++ rootMoveSacrificeTargets,
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
        val rootMoveCapture = capture.plyOffset == 0 || rootMove.contains(move)
        val prefix = if capture.recapture then "material-recapture" else "material-capture"
        val materialIdentityTargets =
          if rootMoveCapture then
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
    val geometry = payload.geometry
    EvidenceObjectBinding(
      source = ref,
      actor = moveObjects(payload.eventMove.getOrElse(payload.rootMove)) ++
        geometry.subjectSquares.flatMap(square => objectOf(EvidenceObjectKind.Square, square.key)) ++
        geometry.roles.map(role => ConcreteChessObject(EvidenceObjectKind.Piece, normalize(role.name))),
      target = geometry.targetSquares.flatMap(square => objectOf(EvidenceObjectKind.Square, square.key)) ++
        geometry.relatedSquares.flatMap(square => objectOf(EvidenceObjectKind.Square, square.key)) ++
        geometry.relatedFiles.flatMap(file => objectOf(EvidenceObjectKind.File, file.key)),
      mechanism = objectOf(EvidenceObjectKind.Motif, payload.kind) ++ objectOf(EvidenceObjectKind.Mechanism, payload.category.toString),
      consequence = objectOf(EvidenceObjectKind.Consequence, payload.category.toString),
      witness = ref.line.toList.flatMap(lineObject) ++ objectOf(EvidenceObjectKind.Move, payload.rootMove),
      line = ref.line,
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
      actor =
        objectOf(EvidenceObjectKind.Side, colorKey(episode.threatActor)) ++
          attackSquareObjects,
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
    }.distinctBy(_.signature)

  private def fromPlanPressure(ref: EvidenceRef, payload: PlanPressureEvidence): List[EvidenceObjectBinding] =
    val alignmentBindings =
      payload.alignment.toList.flatMap { alignment =>
        alignment.matchedPlanIds.map(planId =>
          EvidenceObjectBinding(
            source = ref,
            actor = Nil,
            target = objectOf(EvidenceObjectKind.PlanSubject, planId),
            mechanism = objectOf(EvidenceObjectKind.Mechanism, "structure-plan-alignment"),
            consequence = objectOf(EvidenceObjectKind.Consequence, alignment.band.toString),
            witness = objectOf(EvidenceObjectKind.PlanSubject, planId),
            line = ref.line
          )
        )
      }
    val planBindings = payload.rootBackedPlans(ref.line.map(_.rootMove)).map { plan =>
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
        target = objectOf(EvidenceObjectKind.PlanSubject, plan.plan.kind.id),
        mechanism = objectOf(EvidenceObjectKind.Mechanism, "plan-pressure"),
        consequence = objectOf(EvidenceObjectKind.Consequence, plan.plan.kind.id),
        witness = evidenceMoveWitnessObjects,
        line = ref.line
      )
    }
    (planBindings ++ alignmentBindings).distinctBy(_.signature)

  private def fromPlanCausalEvent(ref: EvidenceRef, payload: PlanCausalEventEvidence): List[EvidenceObjectBinding] =
    val planTarget = objectOf(EvidenceObjectKind.PlanSubject, payload.planId.id)
    val rootActor =
      moveObjects(payload.rootMove) ++
        payload.identity.actorRole.toList.flatMap(objectOf(EvidenceObjectKind.Piece, _)) ++
        objectOf(EvidenceObjectKind.Side, colorKey(payload.perspective))
    val rootDestination =
      moveTargetSquare(payload.rootMove) ++
        Option(normalize(payload.rootMove).slice(2, 3)).filter(_.matches("[a-h]")).toList.flatMap(
          objectOf(EvidenceObjectKind.File, _)
        )
    val rootWitness = objectOf(EvidenceObjectKind.Move, payload.rootMove) ++ lineObject(payload.rootLine)
    val directBindings = payload.directGoalConsequences.map { consequence =>
      val explicitTargets = consequence.targetSubjects.nonEmpty
      EvidenceObjectBinding(
        source = ref,
        actor = rootActor,
        target = (
          planTarget ++
            Option.unless(explicitTargets)(rootDestination).getOrElse(Nil) ++
            consequence.goalSubjects.flatMap(subjectObject)
        ).distinctBy(_.signaturePart),
        mechanism = objectOf(EvidenceObjectKind.Mechanism, consequence.kind.toString),
        consequence = objectOf(EvidenceObjectKind.Consequence, consequence.anchorKey),
        witness = (rootWitness ++ consequence.witnessSubjects.flatMap(subjectObject)).distinctBy(_.signaturePart),
        line = Some(payload.rootLine)
      )
    }
    val developmentBindings = payload.developmentChoices.map { choice =>
      EvidenceObjectBinding(
        source = ref,
        actor = (
          rootActor ++
            objectOf(EvidenceObjectKind.Piece, choice.role) ++
            objectOf(EvidenceObjectKind.Square, choice.from)
        ).distinctBy(_.signaturePart),
        target = (planTarget ++ objectOf(EvidenceObjectKind.Square, choice.to)).distinctBy(_.signaturePart),
        mechanism = objectOf(EvidenceObjectKind.Mechanism, StructuralSignalKind.DevelopmentChoice.toString),
        consequence = objectOf(EvidenceObjectKind.Consequence, TransitionConsequenceKind.DevelopmentPieceActivated.toString),
        witness = rootWitness,
        line = Some(payload.rootLine)
      )
    }
    val responseBindings = payload.planVerifiedResponseGoalResults.map { case (response, consequence) =>
      val resultTargets = (
        consequence.goalSubjects.flatMap(subjectObject) ++
          PlanCausalEpisode
            .consequenceTargetSquares(payload.identity, consequence)
            .flatMap(square => objectOf(EvidenceObjectKind.Square, square.key))
      ).distinctBy(_.signaturePart)
      EvidenceObjectBinding(
        source = ref,
        actor = rootActor,
        target = (planTarget ++ resultTargets).distinctBy(_.signaturePart),
        mechanism = objectOf(EvidenceObjectKind.Mechanism, consequence.kind.toString),
        consequence = objectOf(EvidenceObjectKind.Consequence, consequence.anchorKey),
        witness = (
          rootWitness ++
            objectOf(EvidenceObjectKind.Move, response.step.moveUci) ++
            moveObjects(response.step.moveUci) ++
            resultTargets ++
            consequence.witnessSubjects.flatMap(subjectObject)
        ).distinctBy(_.signaturePart),
        line = Some(payload.rootLine),
        horizon = Some(s"ply:${payload.responseStepDistanceFromPlanStart(response)}")
      )
    }
    val conditionalContinuationBindings = payload.conditionalResponseContinuationResults.map {
      case (response, sourceEvent, consequence) =>
        val resultTargets = (
          consequence.goalSubjects.flatMap(subjectObject) ++
            PlanCausalEpisode
              .consequenceTargetSquares(sourceEvent.identity, consequence)
              .flatMap(square => objectOf(EvidenceObjectKind.Square, square.key))
        ).distinctBy(_.signaturePart)
        EvidenceObjectBinding(
          source = ref,
          actor = (
            rootActor ++ sourceEvent.identity.actorRole.toList.flatMap(objectOf(EvidenceObjectKind.Piece, _))
          ).distinctBy(_.signaturePart),
          target = (planTarget ++ resultTargets).distinctBy(_.signaturePart),
          mechanism = (
            objectOf(EvidenceObjectKind.Mechanism, PlanCausalDependencyKind.ResponseContinuationPrecondition.toString) ++
              objectOf(EvidenceObjectKind.Mechanism, consequence.kind.toString)
          ).distinctBy(_.signaturePart),
          consequence = objectOf(EvidenceObjectKind.Consequence, consequence.anchorKey),
          witness = (
            rootWitness ++
              objectOf(EvidenceObjectKind.Move, response.step.moveUci) ++
              objectOf(EvidenceObjectKind.Move, sourceEvent.moveUci) ++
              resultTargets ++
              consequence.witnessSubjects.flatMap(subjectObject)
          ).distinctBy(_.signaturePart),
          line = Some(payload.rootLine),
          horizon = Some(s"ply:${payload.episode.fold(0)(episode => sourceEvent.step.ply - episode.root.step.ply)}")
        )
    }
    val rootDependencyBindings = payload.rootEnablingDependencies.map { dependency =>
      val dependencySquares = dependency.proofSquares.flatMap(square => objectOf(EvidenceObjectKind.Square, square.key))
      val dependencyPieces = dependency.proofPieceRoles.flatMap(role => objectOf(EvidenceObjectKind.Piece, role.name))
      EvidenceObjectBinding(
        source = ref,
        actor = rootActor,
        target = (planTarget ++ dependencySquares).distinctBy(_.signaturePart),
        mechanism = objectOf(EvidenceObjectKind.Mechanism, dependency.kind.toString),
        consequence = objectOf(EvidenceObjectKind.Consequence, dependency.kind.toString),
        witness = (
          rootWitness ++
            dependencySquares ++
            dependencyPieces
        ).distinctBy(_.signaturePart),
        line = Some(payload.rootLine)
      )
    }
    val historyDependencyBindings = payload.historyEnablingDependencies.map { dependency =>
      val dependencySquares = dependency.proofSquares.flatMap(square => objectOf(EvidenceObjectKind.Square, square.key))
      val dependencyPieces = dependency.proofPieceRoles.flatMap(role => objectOf(EvidenceObjectKind.Piece, role.name))
      EvidenceObjectBinding(
        source = ref,
        actor = (
          rootActor ++ dependency.from.identity.actorRole.toList.flatMap(objectOf(EvidenceObjectKind.Piece, _))
        ).distinctBy(_.signaturePart),
        target = (planTarget ++ dependencySquares).distinctBy(_.signaturePart),
        mechanism = objectOf(EvidenceObjectKind.Mechanism, dependency.kind.toString),
        consequence = objectOf(EvidenceObjectKind.Consequence, dependency.kind.toString),
        witness = (
          rootWitness ++
            objectOf(EvidenceObjectKind.Move, dependency.from.moveUci) ++
            dependencySquares ++
            dependencyPieces
        ).distinctBy(_.signaturePart),
        line = Some(payload.rootLine),
        horizon = Some(s"ply:${dependency.plyOffset}")
      )
    }
    val resultBindings = payload.episode.toList.flatMap { episode =>
      payload.positiveGoalResultAssessments.map { assessment =>
        val sourceEvent = assessment.sourceEvent
        val resultTargets = (
          assessment.consequence.goalSubjects.flatMap(subjectObject) ++
            PlanCausalEpisode
              .consequenceTargetSquares(sourceEvent.identity, assessment.consequence)
              .flatMap(square =>
                objectOf(EvidenceObjectKind.Square, square.key) ++ objectOf(EvidenceObjectKind.File, square.key.take(1))
              )
        ).distinctBy(_.signaturePart)
        val enablingDependencies = episode.enablingDependenciesTo(sourceEvent)
        val dependencyWitnesses = (
          enablingDependencies.flatMap(_.proofSquares).flatMap(square =>
            objectOf(EvidenceObjectKind.Square, square.key)
          ) ++
            enablingDependencies.flatMap(_.proofPieceRoles).flatMap(role =>
              objectOf(EvidenceObjectKind.Piece, role.name)
            )
        ).distinctBy(_.signaturePart)
        EvidenceObjectBinding(
          source = ref,
          actor = (
            rootActor ++
              sourceEvent.identity.actorRole.toList.flatMap(objectOf(EvidenceObjectKind.Piece, _))
          ).distinctBy(_.signaturePart),
          target = (planTarget ++ resultTargets).distinctBy(_.signaturePart),
          mechanism = (
            enablingDependencies.flatMap(dependency => objectOf(EvidenceObjectKind.Mechanism, dependency.kind.toString)) ++
              objectOf(EvidenceObjectKind.Mechanism, assessment.consequence.kind.toString)
          ).distinctBy(_.signaturePart),
          consequence = (
            objectOf(EvidenceObjectKind.Consequence, assessment.consequence.anchorKey) ++
              objectOf(EvidenceObjectKind.Consequence, assessment.robustness.toString)
          ).distinctBy(_.signaturePart),
          witness = (
            rootWitness ++
              objectOf(EvidenceObjectKind.Move, sourceEvent.moveUci) ++
              assessment.realizedObservations.flatMap(observation => lineObject(observation.line)) ++
              assessment.realizedObservations.flatMap(_.realizationMove).flatMap(objectOf(EvidenceObjectKind.Move, _)) ++
              resultTargets ++
              assessment.consequence.witnessSubjects.flatMap(subjectObject) ++
              dependencyWitnesses
          ).distinctBy(_.signaturePart),
          line = Some(payload.rootLine),
          horizon = Some(s"ply:${assessment.sourcePlyOffset}")
        )
      }
    }
    (directBindings ++ responseBindings ++ conditionalContinuationBindings ++ developmentBindings ++
      rootDependencyBindings ++ historyDependencyBindings ++ resultBindings)
      .distinctBy(_.signature)

  private[judgment] def fromPlanMaterialTradeoffs(
      ref: EvidenceRef,
      payload: PlanCausalEventEvidence
  ): List[EvidenceObjectBinding] =
    val planTarget = objectOf(EvidenceObjectKind.PlanSubject, payload.planId.id)
    val rootActor =
      moveObjects(payload.rootMove) ++
        payload.identity.actorRole.toList.flatMap(objectOf(EvidenceObjectKind.Piece, _)) ++
        objectOf(EvidenceObjectKind.Side, colorKey(payload.perspective))
    val rootWitness = objectOf(EvidenceObjectKind.Move, payload.rootMove) ++ lineObject(payload.rootLine)
    payload.materialTradeoffs.map { case (cost, sourceEvent, consequence) =>
      val sacrificeTargets = (
        objectOf(EvidenceObjectKind.PlanSubject, s"material-sacrifice:${cost.capture.square.key}") ++
          objectOf(EvidenceObjectKind.Square, cost.capture.square.key) ++
          objectOf(EvidenceObjectKind.Piece, cost.capture.capturedRole.name)
      ).distinctBy(_.signaturePart)
      val resultTargets = (
        consequence.goalSubjects.flatMap(subjectObject) ++
          PlanCausalEpisode
            .consequenceTargetSquares(sourceEvent.identity, consequence)
            .flatMap(square => objectOf(EvidenceObjectKind.Square, square.key))
      ).distinctBy(_.signaturePart)
      EvidenceObjectBinding(
        source = ref,
        actor = rootActor,
        target = (planTarget ++ sacrificeTargets ++ resultTargets).distinctBy(_.signaturePart),
        mechanism = (
          objectOf(EvidenceObjectKind.Mechanism, LineConsequenceKind.Sacrifice.toString) ++
            objectOf(EvidenceObjectKind.Mechanism, consequence.kind.toString)
        ).distinctBy(_.signaturePart),
        consequence = objectOf(EvidenceObjectKind.Consequence, consequence.anchorKey),
        witness = (
          rootWitness ++
            objectOf(EvidenceObjectKind.Move, cost.responseStep.moveUci) ++
            objectOf(EvidenceObjectKind.Move, sourceEvent.moveUci) ++
            sacrificeTargets ++
            resultTargets ++
            consequence.witnessSubjects.flatMap(subjectObject)
        ).distinctBy(_.signaturePart),
        line = Some(payload.rootLine),
        horizon = Some(s"ply:${sourceEvent.step.ply - payload.episode.fold(sourceEvent.step.ply)(_.root.step.ply)}")
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
      moveObjects(payload.moveUci) ++
        structuralMoveActorRole(ref.position.fen, payload.moveUci).toList.flatMap(objectOf(EvidenceObjectKind.Piece, _)) ++
        objectOf(EvidenceObjectKind.Side, colorKey(payload.perspective))
    val signalBindings =
      payload.signals.map { signal =>
        EvidenceObjectBinding(
          source = ref,
          actor = actor.distinctBy(_.signaturePart),
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
          actor = actor.distinctBy(_.signaturePart),
          target = (
            consequence.goalSubjects.flatMap(subjectObject) ++
              structuralPressureTargetPieceObjects(payload, consequence) ++
              structuralOpponentRestrictionTargetObjects(consequence)
          ).distinctBy(_.signaturePart),
          mechanism = objectOf(EvidenceObjectKind.Mechanism, consequence.kind.toString),
          consequence = objectOf(EvidenceObjectKind.Consequence, consequence.anchorKey),
          witness = (
            objectOf(EvidenceObjectKind.Move, payload.moveUci) ++
              payload.line.toList.flatMap(lineObject) ++
              consequence.witnessSubjects.flatMap(subjectObject)
          ).distinctBy(_.signaturePart),
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

  private def structuralMoveActorRole(fen: String, moveUci: String): Option[String] =
    for
      position <- _root_.chess.format.Fen.read(
        _root_.chess.variant.Standard,
        _root_.chess.format.Fen.Full(fen)
      )
      from <- Square.fromKey(normalize(moveUci).take(2))
      piece <- position.board.pieceAt(from)
    yield piece.role.name

  private def structuralPressureTargetPieceObjects(
      payload: StructuralDeltaEvidence,
      consequence: TransitionConsequence
  ): List[ConcreteChessObject] =
    val targetPosition = consequence.kind match
      case TransitionConsequenceKind.TargetPressureGain    => Some(payload.to)
      case TransitionConsequenceKind.TargetPressureRelease => Some(payload.from)
      case _                                                => None
    targetPosition.toList.flatMap { positionRef =>
      _root_.chess.format.Fen
        .read(_root_.chess.variant.Standard, _root_.chess.format.Fen.Full(positionRef.fen))
        .toList
        .flatMap { position =>
          consequence.goalSubjects.flatMap { subject =>
            Square
              .fromKey(StructuralPurposeSubject.carrierToken(subject))
              .flatMap(position.board.pieceAt)
              .filter(_.color != payload.perspective)
              .toList
              .flatMap(piece => objectOf(EvidenceObjectKind.Piece, piece.role.name))
          }
        }
        .distinctBy(_.signaturePart)
    }

  private def structuralOpponentRestrictionTargetObjects(
      consequence: TransitionConsequence
  ): List[ConcreteChessObject] =
    Option
      .when(consequence.kind == TransitionConsequenceKind.OpponentMobilityRestriction)(
        consequence.subjects
          .filter(StructuralDeltaEvidence.validOpponentMobilityRestrictionSubject)
          .flatMap(StructuralDeltaEvidence.restrictedOpponentEntry)
          .flatMap { case (piece, from, to) =>
            objectOf(EvidenceObjectKind.Piece, piece) ++
              objectOf(EvidenceObjectKind.Square, from) ++
              objectOf(EvidenceObjectKind.Square, to)
          }
          .distinctBy(_.signaturePart)
      )
      .getOrElse(Nil)

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
      case None if StructuralPurposeSubject.restrictedEntry(cleaned).nonEmpty =>
        val route = StructuralPurposeSubject.restrictedEntryRoute(cleaned).getOrElse(
          StructuralPurposeSubject.restrictedEntry(cleaned).toList.flatMap { case (_, from, to) => List(from, to) }
        )
        val routeMoves = route.sliding(2).collect { case List(from, to) => s"$from$to" }.toList
        val routeObjects = route.flatMap(square => objectOf(EvidenceObjectKind.Square, square)) ++
          routeMoves.flatMap(move => objectOf(EvidenceObjectKind.Move, move))
        val conditionObjects =
          StructuralPurposeSubject.restrictedEntryGoal(cleaned).toList.flatMap(objectOf(EvidenceObjectKind.Square, _)) ++
            StructuralPurposeSubject.restrictedEntryClearance(cleaned).toList.flatMap(move =>
              objectOf(EvidenceObjectKind.Move, move)
            )
        identityObject ++ StructuralPurposeSubject.restrictedEntry(cleaned).toList.flatMap { case (piece, _, _) =>
          objectOf(EvidenceObjectKind.Piece, piece)
        } ++ routeObjects ++ conditionObjects
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
      case value
          if value.contains(":advance-restricted") || value.contains(":entry-restricted:") ||
            value.contains(":color-complex-safe") =>
        None
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
        value.matches("pawn:[a-h][1-8]-[a-h][1-8]:advance-restricted.*") ||
        value.startsWith("passed-pawn-")
    )(value)

  def restrictedEntry(raw: String): Option[(String, String, String)] =
    val normalized = normalize(raw).stripPrefix("subject:")
    normalized match
      case restrictedEntryPattern(piece, from, to, _, gain)
          if gain.toIntOption.exists(value =>
            value > 0 || value == 0 && restrictedEntryRoutePattern.findFirstIn(normalized).nonEmpty
          ) =>
        Some((piece, from, to))
      case _ =>
        None

  def restrictedEntryRoute(raw: String): Option[List[String]] =
    for
      (_, from, to) <- restrictedEntry(raw)
      routeMatch <- restrictedEntryRoutePattern.findFirstMatchIn(normalize(raw).stripPrefix("subject:"))
      route = routeMatch.group(1).split("-").toList
      if route.size >= 3 && route.headOption.contains(from) && route.lastOption.contains(to)
    yield route

  def restrictedEntryGoal(raw: String): Option[String] =
    restrictedEntry(raw).flatMap(_ =>
      restrictedEntryGoalPattern
        .findFirstMatchIn(normalize(raw).stripPrefix("subject:"))
        .map(_.group(1))
    )

  def restrictedEntryClearance(raw: String): Option[String] =
    restrictedEntry(raw).flatMap(_ =>
      restrictedEntryClearancePattern
        .findFirstMatchIn(normalize(raw).stripPrefix("subject:"))
        .map(_.group(1))
    )

  def restrictedEntryRouteNeedsLine(raw: String): Boolean =
    restrictedEntry(raw).nonEmpty && normalize(raw).contains(":via-contested")

  private val restrictedEntryPattern =
    raw"(pawn|knight|bishop|rook|queen):([a-h][1-8])-([a-h][1-8]):entry-restricted:by:([a-h][1-8][a-h][1-8]):exchange-gain:([0-9]+)(?::.*)?".r
  private val restrictedEntryRoutePattern = raw"route:([a-h][1-8](?:-[a-h][1-8])+)".r
  private val restrictedEntryGoalPattern = raw"goal:([a-h][1-8])".r
  private val restrictedEntryClearancePattern = raw"clearance:([a-h][1-8][a-h][1-8])".r

  def pawnAdvanceUnlockedBy(raw: String, moveUci: String): Option[(String, String)] =
    val value = normalize(raw)
    val marker = s":line-unlock:by:${EvidenceRef.normalizeMove(moveUci)}"
    Option.when(value.contains(marker))(parse(value)).flatten.collect {
      case PieceRoute("pawn", from, to)
          if from.headOption == to.headOption &&
            (for
              fromRank <- from.drop(1).toIntOption
              toRank <- to.drop(1).toIntOption
            yield fromRank != toRank && (fromRank - toRank).abs <= 2).contains(true) =>
        from -> to
    }

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
  case StalemateTrap(stalematedKingSquare: EvidenceSquare, resourceSquare: EvidenceSquare, entryMove: String, terminalMove: String)
  case PerpetualCheck(
      checkedKingSquare: EvidenceSquare,
      checkerSquares: List[EvidenceSquare],
      checkingSide: String,
      entryMove: String,
      cycleStartMove: String,
      cycleReturnMove: String,
      repeatedPositionKey: String
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
      case StalemateTrap(stalematedKingSquare, resourceSquare, entryMove, terminalMove) =>
        detail(
          "StalemateTrap",
          s"king=${sq(stalematedKingSquare)}",
          s"resource=${sq(resourceSquare)}",
          s"entry_move=$entryMove",
          s"terminal_move=$terminalMove"
        )
      case PerpetualCheck(checkedKingSquare, checkerSquares, checkingSide, entryMove, cycleStartMove, cycleReturnMove, _) =>
        detail(
          "PerpetualCheck",
          s"king=${sq(checkedKingSquare)}",
          s"checkers=${squares(checkerSquares)}",
          s"side=$checkingSide",
          s"entry_move=$entryMove",
          s"cycle=$cycleStartMove-$cycleReturnMove"
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

object RelationWitnessDetail:
  def factKind(detail: RelationWitnessDetail): Option[RelationFactKind] =
    detail match
      case Empty                  => None
      case _: DefenderTrade       => Some(RelationFactKind.DefenderTrade)
      case _: BadPieceLiquidation => Some(RelationFactKind.BadPieceLiquidation)
      case _: Overload            => Some(RelationFactKind.Overload)
      case _: Deflection          => Some(RelationFactKind.Deflection)
      case _: DiscoveredAttack    => Some(RelationFactKind.DiscoveredAttack)
      case _: DoubleCheck         => Some(RelationFactKind.DoubleCheck)
      case MatePattern(relationKind, _, _, _, _) =>
        RelationFactKind
          .fromId(relationKind)
          .filter(kind => kind == RelationFactKind.BackRankMate || kind == RelationFactKind.MateNet)
      case _: GreekGift       => Some(RelationFactKind.GreekGift)
      case _: Fork            => Some(RelationFactKind.Fork)
      case _: HangingPiece    => Some(RelationFactKind.HangingPiece)
      case _: TrappedPiece    => Some(RelationFactKind.TrappedPiece)
      case _: Domination      => Some(RelationFactKind.Domination)
      case _: Zwischenzug     => Some(RelationFactKind.Zwischenzug)
      case _: Decoy           => Some(RelationFactKind.Decoy)
      case _: XRay            => Some(RelationFactKind.XRay)
      case _: Clearance       => Some(RelationFactKind.Clearance)
      case _: Battery         => Some(RelationFactKind.Battery)
      case _: Interference    => Some(RelationFactKind.Interference)
      case _: Pin             => Some(RelationFactKind.Pin)
      case _: Skewer          => Some(RelationFactKind.Skewer)
      case _: StalemateTrap   => Some(RelationFactKind.StalemateTrap)
      case _: PerpetualCheck  => Some(RelationFactKind.PerpetualCheck)

  def focusSquares(detail: RelationWitnessDetail): List[EvidenceSquare] =
    val squares =
      detail match
        case Empty =>
          Nil
        case DefenderTrade(_, exchangeSquare, targetSquare) =>
          List(targetSquare, exchangeSquare)
        case BadPieceLiquidation(badPieceSquare, exchangeSquare) =>
          List(badPieceSquare, exchangeSquare)
        case Overload(defenderSquare, targetSquares, _) =>
          defenderSquare :: targetSquares
        case Deflection(defenderSquare, targetSquare, attackerSquare) =>
          List(targetSquare, defenderSquare, attackerSquare)
        case DiscoveredAttack(attackerSquare, clearedSquare, targetSquare, _) =>
          List(attackerSquare, clearedSquare, targetSquare)
        case DoubleCheck(kingSquare, checkerSquares, _, _) =>
          kingSquare :: checkerSquares
        case MatePattern(_, kingSquare, checkerSquares, _, _) =>
          kingSquare :: checkerSquares
        case GreekGift(bishopSquare, targetSquare, _, _) =>
          List(bishopSquare, targetSquare)
        case Fork(attackerSquare, _, targets) =>
          attackerSquare :: targets.map(_.square)
        case HangingPiece(attackerSquare, targetSquare, _, _) =>
          List(attackerSquare, targetSquare)
        case TrappedPiece(attackerSquare, targetSquare, _, _) =>
          List(attackerSquare, targetSquare)
        case Domination(attackerSquare, targetSquare, _, _, controlledEscapeSquares) =>
          attackerSquare :: targetSquare :: controlledEscapeSquares
        case Zwischenzug(_, expectedRecaptureSquare, checkingPieceSquare, _, checkedKingSquare, _) =>
          List(checkingPieceSquare, expectedRecaptureSquare, checkedKingSquare)
        case Decoy(baitFromSquare, baitSquare, luredFromSquare, _, _, _, _) =>
          List(baitFromSquare, baitSquare, luredFromSquare)
        case XRay(attackerSquare, blockerSquare, targetSquare, _, _, _) =>
          List(attackerSquare, blockerSquare, targetSquare)
        case Clearance(beneficiarySquare, clearedSquare, targetSquare, _, _) =>
          List(beneficiarySquare, clearedSquare, targetSquare)
        case Battery(frontSquare, backSquare, targetSquare, _, _, _) =>
          List(frontSquare, backSquare, targetSquare)
        case Interference(blockerSquare, defenderSquare, targetSquare, _, _, _) =>
          List(blockerSquare, defenderSquare, targetSquare)
        case Pin(attackerSquare, pinnedSquare, behindSquare, _, _, _, _, _) =>
          List(attackerSquare, pinnedSquare, behindSquare)
        case Skewer(attackerSquare, frontSquare, backSquare, _, _, _, _) =>
          List(attackerSquare, frontSquare, backSquare)
        case StalemateTrap(stalematedKingSquare, resourceSquare, _, _) =>
          List(stalematedKingSquare, resourceSquare)
        case PerpetualCheck(checkedKingSquare, checkerSquares, _, _, _, _, _) =>
          checkedKingSquare :: checkerSquares
    squares.distinct

  def targetSquare(detail: RelationWitnessDetail): Option[EvidenceSquare] =
    detail match
      case Empty =>
        None
      case DefenderTrade(_, _, targetSquare) =>
        Some(targetSquare)
      case BadPieceLiquidation(_, exchangeSquare) =>
        Some(exchangeSquare)
      case Overload(_, targetSquares, _) =>
        targetSquares.headOption
      case Deflection(_, targetSquare, _) =>
        Some(targetSquare)
      case DiscoveredAttack(_, _, targetSquare, _) =>
        Some(targetSquare)
      case DoubleCheck(kingSquare, _, _, _) =>
        Some(kingSquare)
      case MatePattern(_, kingSquare, _, _, _) =>
        Some(kingSquare)
      case GreekGift(_, targetSquare, _, _) =>
        Some(targetSquare)
      case Fork(_, _, targets) =>
        targets.headOption.map(_.square)
      case HangingPiece(_, targetSquare, _, _) =>
        Some(targetSquare)
      case TrappedPiece(_, targetSquare, _, _) =>
        Some(targetSquare)
      case Domination(_, targetSquare, _, _, _) =>
        Some(targetSquare)
      case Zwischenzug(_, expectedRecaptureSquare, _, _, _, _) =>
        Some(expectedRecaptureSquare)
      case Decoy(_, baitSquare, _, _, _, _, _) =>
        Some(baitSquare)
      case XRay(_, _, targetSquare, _, _, _) =>
        Some(targetSquare)
      case Clearance(_, _, targetSquare, _, _) =>
        Some(targetSquare)
      case Battery(_, _, targetSquare, _, _, _) =>
        Some(targetSquare)
      case Interference(_, _, targetSquare, _, _, _) =>
        Some(targetSquare)
      case Pin(_, _, _, targetSquare, _, _, _, _) =>
        Some(targetSquare)
      case Skewer(_, _, _, targetSquare, _, _, _) =>
        Some(targetSquare)
      case StalemateTrap(stalematedKingSquare, _, _, _) =>
        Some(stalematedKingSquare)
      case PerpetualCheck(checkedKingSquare, _, _, _, _, _, _) =>
        Some(checkedKingSquare)

  def participants(detail: RelationWitnessDetail): List[RelationParticipant] =
    val values =
      detail match
        case Empty =>
          Nil
        case DefenderTrade(defenderSquare, exchangeSquare, targetSquare) =>
          List(
            part(defenderSquare, RelationParticipantRole.Defender),
            part(exchangeSquare, RelationParticipantRole.Mover),
            part(targetSquare, RelationParticipantRole.Target)
          )
        case BadPieceLiquidation(badPieceSquare, exchangeSquare) =>
          List(
            part(badPieceSquare, RelationParticipantRole.Target),
            part(exchangeSquare, RelationParticipantRole.Mover)
          )
        case Overload(defenderSquare, targetSquares, attackerSquare) =>
          part(defenderSquare, RelationParticipantRole.Defender) ::
            part(attackerSquare, RelationParticipantRole.Attacker) ::
            targetSquares.map(part(_, RelationParticipantRole.Target))
        case Deflection(defenderSquare, targetSquare, attackerSquare) =>
          List(
            part(defenderSquare, RelationParticipantRole.Defender),
            part(targetSquare, RelationParticipantRole.Target),
            part(attackerSquare, RelationParticipantRole.Attacker)
          )
        case DiscoveredAttack(attackerSquare, clearedSquare, targetSquare, attackerRole) =>
          List(
            part(attackerSquare, RelationParticipantRole.Attacker, Some(attackerRole)),
            part(clearedSquare, RelationParticipantRole.Mover),
            part(targetSquare, RelationParticipantRole.Target)
          )
        case DoubleCheck(kingSquare, checkerSquares, moverSquare, moverRole) =>
          part(kingSquare, RelationParticipantRole.King) ::
            part(moverSquare, RelationParticipantRole.Mover, Some(moverRole)) ::
            checkerSquares.map(part(_, RelationParticipantRole.Attacker))
        case MatePattern(_, kingSquare, checkerSquares, matingMove, _) =>
          part(kingSquare, RelationParticipantRole.King) ::
            uciDestination(matingMove).map(part(_, RelationParticipantRole.Mover)).toList :::
            checkerSquares.map(part(_, RelationParticipantRole.Attacker))
        case GreekGift(bishopSquare, targetSquare, _, _) =>
          List(
            part(bishopSquare, RelationParticipantRole.Attacker, Some(EvidencePieceRole("bishop"))),
            part(targetSquare, RelationParticipantRole.Target)
          )
        case Fork(attackerSquare, attackerRole, targets) =>
          part(attackerSquare, RelationParticipantRole.Attacker, Some(attackerRole)) ::
            targets.map(target => part(target.square, RelationParticipantRole.Target, Some(target.role)))
        case HangingPiece(attackerSquare, targetSquare, attackerRole, targetRole) =>
          List(
            part(attackerSquare, RelationParticipantRole.Attacker, Some(attackerRole)),
            part(targetSquare, RelationParticipantRole.Target, Some(targetRole))
          )
        case TrappedPiece(attackerSquare, targetSquare, attackerRole, targetRole) =>
          List(
            part(attackerSquare, RelationParticipantRole.Attacker, Some(attackerRole)),
            part(targetSquare, RelationParticipantRole.Target, Some(targetRole))
          )
        case Domination(attackerSquare, targetSquare, attackerRole, targetRole, controlledEscapeSquares) =>
          List(
            part(attackerSquare, RelationParticipantRole.Attacker, Some(attackerRole)),
            part(targetSquare, RelationParticipantRole.Target, Some(targetRole))
          ) ++ controlledEscapeSquares.map(part(_, RelationParticipantRole.Other))
        case Zwischenzug(intermediateMove, expectedRecaptureSquare, checkingPieceSquare, checkingPieceRole, checkedKingSquare, _) =>
          uciDestination(intermediateMove).map(part(_, RelationParticipantRole.Mover)).toList ++ List(
            part(expectedRecaptureSquare, RelationParticipantRole.Target),
            part(checkingPieceSquare, RelationParticipantRole.Attacker, Some(checkingPieceRole)),
            part(checkedKingSquare, RelationParticipantRole.King)
          )
        case Decoy(baitFromSquare, baitSquare, luredFromSquare, executionFromSquare, executionToSquare, baitRole, luredRole) =>
          List(
            part(baitFromSquare, RelationParticipantRole.Bait, Some(baitRole)),
            part(baitSquare, RelationParticipantRole.Bait, Some(baitRole)),
            part(luredFromSquare, RelationParticipantRole.Lured, Some(luredRole)),
            part(executionFromSquare, RelationParticipantRole.Attacker),
            part(executionToSquare, RelationParticipantRole.Target)
          )
        case XRay(attackerSquare, blockerSquare, targetSquare, attackerRole, blockerRole, targetRole) =>
          List(
            part(attackerSquare, RelationParticipantRole.Attacker, Some(attackerRole)),
            part(blockerSquare, RelationParticipantRole.Blocker, Some(blockerRole)),
            part(targetSquare, RelationParticipantRole.Target, Some(targetRole))
          )
        case Clearance(beneficiarySquare, clearedSquare, targetSquare, beneficiaryRole, clearingTo) =>
          List(
            part(beneficiarySquare, RelationParticipantRole.Beneficiary, Some(beneficiaryRole)),
            part(clearedSquare, RelationParticipantRole.Mover),
            part(clearingTo, RelationParticipantRole.Mover),
            part(targetSquare, RelationParticipantRole.Target)
          )
        case Battery(frontSquare, backSquare, targetSquare, frontRole, backRole, _) =>
          List(
            part(frontSquare, RelationParticipantRole.Attacker, Some(frontRole)),
            part(backSquare, RelationParticipantRole.Attacker, Some(backRole)),
            part(targetSquare, RelationParticipantRole.Target)
          )
        case Interference(blockerSquare, defenderSquare, targetSquare, blockerRole, defenderRole, targetRole) =>
          List(
            part(blockerSquare, RelationParticipantRole.Blocker, Some(blockerRole)),
            part(defenderSquare, RelationParticipantRole.Defender, Some(defenderRole)),
            part(targetSquare, RelationParticipantRole.Target, Some(targetRole))
          )
        case Pin(attackerSquare, pinnedSquare, behindSquare, targetSquare, attackerRole, pinnedRole, behindRole, _) =>
          List(
            part(attackerSquare, RelationParticipantRole.Attacker, Some(attackerRole)),
            part(pinnedSquare, RelationParticipantRole.Blocker, Some(pinnedRole)),
            part(behindSquare, RelationParticipantRole.Target, Some(behindRole)),
            part(targetSquare, RelationParticipantRole.Target)
          )
        case Skewer(attackerSquare, frontSquare, backSquare, targetSquare, attackerRole, frontRole, backRole) =>
          List(
            part(attackerSquare, RelationParticipantRole.Attacker, Some(attackerRole)),
            part(frontSquare, RelationParticipantRole.Target, Some(frontRole)),
            part(backSquare, RelationParticipantRole.Target, Some(backRole)),
            part(targetSquare, RelationParticipantRole.Target)
          )
        case StalemateTrap(stalematedKingSquare, resourceSquare, _, _) =>
          List(
            part(stalematedKingSquare, RelationParticipantRole.King),
            part(resourceSquare, RelationParticipantRole.Other)
          )
        case PerpetualCheck(checkedKingSquare, checkerSquares, _, _, _, _, _) =>
          part(checkedKingSquare, RelationParticipantRole.King) ::
            checkerSquares.map(part(_, RelationParticipantRole.Attacker))
    values.distinct

  def proofAtoms(detail: RelationWitnessDetail, lineMoves: List[String]): List[RelationProofAtom] =
    val participantAtoms =
      participants(detail).map { participant =>
        RelationProofAtom(
          role = RelationProofAtomRole.Participant,
          square = Some(participant.square),
          participantRole = Some(participant.participantRole),
          pieceRole = participant.role
        )
      }
    val label = factKind(detail).map(RelationFactKind.id).getOrElse("unknown")
    val lineMoveAtoms =
      lineMoves.zipWithIndex.map { case (move, index) =>
        RelationProofAtom(
          role = RelationProofAtomRole.LineMove,
          moveUci = Some(move),
          label = Some(s"$label:line:${index + 1}")
        )
      }
    val focusAtoms =
      focusSquares(detail).map(square => RelationProofAtom(role = RelationProofAtomRole.Focus, square = Some(square)))
    val targetAtoms =
      targetSquare(detail).toList.map(square => RelationProofAtom(role = RelationProofAtomRole.Target, square = Some(square)))
    (participantAtoms ++ lineMoveAtoms ++ focusAtoms ++ targetAtoms).distinct

  private def part(
      square: EvidenceSquare,
      participantRole: RelationParticipantRole,
      role: Option[EvidencePieceRole] = None
  ): RelationParticipant =
    RelationParticipant(square = square, role = role, participantRole = participantRole)

  private def uciDestination(move: String): Option[EvidenceSquare] =
    val normalized = Option(move).getOrElse("").trim.toLowerCase
    val destination = normalized.drop(2).take(2)
    Option.when(destination.matches("[a-h][1-8]"))(EvidenceSquare(destination))

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
  private val ids: Map[RelationFactKind, String] =
    byId.map((id, kind) => kind -> id)

  def fromId(raw: String): Option[RelationFactKind] =
    byId.get(Option(raw).getOrElse("").trim.toLowerCase)
  def id(kind: RelationFactKind): String =
    ids.getOrElse(kind, kind.toString)

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

sealed trait EvidencePayload

final case class BoardFactEvidence(
    private val facts: List[Fact],
    private val features: Option[PositionFeatures],
    private val anchors: List[BoardAnchor] = Nil,
    private val attackDefense: List[BoardAttackDefenseEntry] = Nil
) extends EvidencePayload:
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
  def positionFeatures: Option[PositionFeatures] =
    features
  def factCount: Int =
    facts.size
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

final case class PawnStructureFactEvidence(
    profile: StructureProfile,
    pawnPlay: Option[PawnPlayAnalysis]
) extends EvidencePayload

final case class StrategicFactEvidence(
    kind: StrategicFactKind,
    facts: List[Fact],
    relatedPlans: List[lila.chessjudgment.model.PlanId],
    confidence: Double,
    boardAnchors: List[BoardAnchor] = Nil
) extends EvidencePayload:
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
  def signalKinds: Set[StrategicMechanismSignalKind] =
    signals.map(_.kind).toSet
  def hasSignals: Boolean =
    signals.nonEmpty
  def directStrength: Int =
    signals.map(_.strength).sum
  def hasCompositeSupport: Boolean =
    hasResolvedPlanEvent || signals.size >= 2 || signalKinds.exists(kind =>
      kind == StrategicMechanismSignalKind.StructuralDelta ||
        kind == StrategicMechanismSignalKind.PawnStructure ||
        kind == StrategicMechanismSignalKind.PlanTransition ||
        kind == StrategicMechanismSignalKind.OpeningAnchor ||
        kind == StrategicMechanismSignalKind.EndgamePosition
    )
  def hasResolvedPlanEvent: Boolean =
    signals.exists(signal =>
      signal.kind == StrategicMechanismSignalKind.PlanPressure &&
        signal.sourceLayer == EvidenceLayer.PlanCausalEvent &&
        signal.source.confidence != EvidenceConfidence.Heuristic &&
        signal.axis.exists(_.kind == StrategicAxisKind.PlanCoherence)
    )
  def canAnchorStrategicClaim: Boolean =
    hasCompositeSupport &&
      kind != StrategicMechanismKind.OpeningAlignment &&
      (kind match
        case StrategicMechanismKind.StructuralImprovement | StrategicMechanismKind.StrategicConcession =>
          hasStrategicAxis
        case StrategicMechanismKind.PlanPressure =>
          hasResolvedPlanEvent
        case _ =>
          true
      )
  def canAnchorPawnStructureClaim: Boolean =
    kind == StrategicMechanismKind.PawnStructure && hasSignals
  def canAnchorOpeningClaim: Boolean =
    kind == StrategicMechanismKind.OpeningAlignment &&
      signalKinds.contains(StrategicMechanismSignalKind.OpeningApplicability)
  def canAnchorPlanClaim: Boolean =
    kind == StrategicMechanismKind.PlanPressure &&
      hasResolvedPlanEvent
  def canSupportCompensation: Boolean =
    kind == StrategicMechanismKind.Compensation && hasSignals
  def canSupportStrategicCause: Boolean =
    canAnchorStrategicClaim || canAnchorPawnStructureClaim || canSupportCompensation
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
  def hasActivityGainAxis: Boolean =
    kind == StrategicMechanismKind.Activity && canSupportStrategicCause &&
      hasAxis(StrategicAxisKind.Activity, Set(StrategicAxisPolarity.Gain))
  def hasStrategicConcessionAxis: Boolean =
    kind == StrategicMechanismKind.StrategicConcession && hasStrategicAxis
  def hasPassedPawnResourceSignal: Boolean =
    kind == StrategicMechanismKind.PawnStructure &&
      canAnchorPawnStructureClaim &&
      hasAnySignalLabel(Set("passed-pawn-progress", "promotion-pressure-gain"))
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
  def actionableComparisons: List[StrategicAxisComparison] =
    axisComparisons.filter(_.hasContrast)
  def hasActionableContrast: Boolean =
    actionableComparisons.nonEmpty || planComparison.exists(_.hasPlanDelta)
  def sourceRefs: List[EvidenceRef] =
    support.all
  def axisKeys: List[String] =
    axisComparisons.map(_.axisKey).distinct.sorted

object StrategicMechanismContrastEvidence:
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
        currentMoveBreakCarrierRecords(candidateLine, records) ++
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

  private[chessjudgment] def currentMoveBreakCarrier(
      candidateLine: LineNodeRef,
      records: List[EvidenceRecord]
  ): Boolean =
    currentMoveBreakCarrierRecords(candidateLine, records).nonEmpty

  private[chessjudgment] def currentMoveCounterplayRestraintCarrier(
      candidateLine: LineNodeRef,
      records: List[EvidenceRecord]
  ): Boolean =
    currentMoveBreakCarrier(candidateLine, records) ||
      currentMoveConcreteActivityCarrierRecords(candidateLine, records).nonEmpty

  private[chessjudgment] def currentMoveBreakCarrierRecords(
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
          EvidenceLayer.PlanPressure | EvidenceLayer.PlanCausalEvent | EvidenceLayer.PlanTransition | EvidenceLayer.FeatureAnchor |
          EvidenceLayer.ApplicabilityAssessment | EvidenceLayer.OpeningContext =>
        true
      case _ =>
        false

  def openingClaimSupported(records: List[EvidenceRecord]): Boolean =
    val mechanisms = records.collect { case EvidenceRecord(_, payload: StrategicMechanismEvidence, _) => payload }
    mechanisms.exists(_.canAnchorOpeningClaim)

  def sourceMechanisms(record: EvidenceRecord): List[(StrategicMechanismKind, StrategicMechanismSignal)] =
    record.payload match
      case payload @ StrategicFactEvidence(kind, _, _, confidence, _) if confidence >= 0.35 && payload.hasTypedSupport =>
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
        val axis = payload.pawnPlay.flatMap(pawnPlayAxis)
        List(StrategicMechanismKind.PawnStructure -> signal(StrategicMechanismSignalKind.PawnStructure, label, record.ref, 2, concreteAxis(record, axis)))
      case payload: PlanPressureEvidence if payload.alignment.exists(_.matchedPlanIds.nonEmpty) =>
        payload.alignment.toList.map { alignment =>
          val label = alignment.matchedPlanIds.sorted.mkString(",")
          StrategicMechanismKind.PlanPressure ->
            signal(
              StrategicMechanismSignalKind.PlanPressure,
              alignment.band.toString,
              record.ref,
              math.round(alignment.score.toDouble / 25.0).toInt.max(1),
              concreteAxis(
                record,
                Some(
                  StrategicAxisDetail(
                    StrategicAxisKind.PlanCoherence,
                    StrategicAxisPolarity.Support,
                    label
                  )
                )
              )
            )
        }
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
          Option.when(payload.hasReplyIndependentOpponentMobilityRestriction)(
            StrategicMechanismKind.Activity -> signal(
              StrategicMechanismSignalKind.StructuralDelta,
              "opponent-mobility-restriction",
              record.ref,
              3,
              concreteAxis(
                record,
                structuralDeltaAxis(StrategicAxisKind.Counterplay, StrategicAxisPolarity.Restrain, "opponent-mobility-restriction")
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
      case payload: PlanCausalEventEvidence if record.ref.confidence != EvidenceConfidence.Heuristic =>
        val axes = planCausalAxes(payload).flatMap(axis => concreteAxis(record, Some(axis))).distinct
        val admittedAxes = if axes.nonEmpty then axes.map(Some(_)) else List(None)
        admittedAxes.map(axis =>
          StrategicMechanismKind.PlanPressure ->
            signal(
              StrategicMechanismSignalKind.PlanPressure,
              payload.planId.id,
              record.ref,
              axis.fold(1)(axis => if axis.polarity == StrategicAxisPolarity.Support then 2 else 3),
              axis
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
      case payload: BoardFactEvidence =>
        List(
          Option.when(payload.positionFeatures.exists(_.materialPhase.phase == "endgame"))(
            StrategicMechanismKind.Endgame ->
              signal(StrategicMechanismSignalKind.EndgamePosition, "endgame-position", record.ref, 2)
          ),
          Option.when(payload.endgameTechniqueAnchors.nonEmpty)(
            StrategicMechanismKind.Endgame ->
              signal(StrategicMechanismSignalKind.EndgamePosition, "endgame-technique", record.ref, 2)
          )
        ).flatten
      case _ =>
        Nil

  private def planCausalAxes(event: PlanCausalEventEvidence): List[StrategicAxisDetail] =
    val directProof =
      event.structuralConsequences.nonEmpty ||
        event.planVerifiedResponseGoalResults.nonEmpty ||
        event.conditionalResponseContinuationResults.nonEmpty ||
        event.developmentChoices.nonEmpty ||
        event.rootEnablingDependencies.nonEmpty
    val futurePolarity = event.episode match
      case None => None
      case Some(_) if event.positiveCausalResultAssessments.nonEmpty =>
        if event.allCausalResultsRobust then Some(StrategicAxisPolarity.Gain)
        else Some(StrategicAxisPolarity.Support)
      case Some(_) if event.allCausalResultsRefuted =>
        Some(StrategicAxisPolarity.Concede)
      case Some(_) => None
    (
      Option.when(
        directProof && futurePolarity.forall(_ == StrategicAxisPolarity.Concede)
      )(StrategicAxisPolarity.Support).toList ++
        futurePolarity.toList
    ).distinct.map(StrategicAxisDetail(StrategicAxisKind.PlanCoherence, _, event.planId.id))

  def sourceSemanticAnchors(record: EvidenceRecord): List[EvidenceSemanticAnchor] =
    record.payload match
      case payload @ StrategicFactEvidence(kind, _, relatedPlans, confidence, _)
          if confidence >= 0.35 && payload.hasTypedSupport =>
        EvidenceSemanticAnchor.of(EvidenceSemanticAnchorKind.StrategicKind, kind.toString) ::
          relatedPlans.map(plan => EvidenceSemanticAnchor.of(EvidenceSemanticAnchorKind.Plan, plan.id)) ++
          payload.semanticGroupingAnchors
      case PawnStructureFactEvidence(profile, pawnPlay) =>
        (
          Option.when(profile.primary != StructureId.Unknown)(
            EvidenceSemanticAnchor.of(EvidenceSemanticAnchorKind.PawnStructure, profile.primary.toString)
          ).toList ++
            pawnStructureGenericAnchors(profile.primary) ++
            pawnPlay.toList.flatMap(pawnPlaySemanticAnchors)
        ).distinct
      case FeatureAnchorEvidence(anchor) =>
        List(EvidenceSemanticAnchor.of(EvidenceSemanticAnchorKind.OpeningAnchor, anchor.theme.toString, anchor.signal.toString))
      case ApplicabilityAssessmentEvidence(assessment) =>
        assessment.supportedThemes.map(theme => EvidenceSemanticAnchor.of(EvidenceSemanticAnchorKind.OpeningSupported, theme.toString)) ++
          assessment.observedThemes.map(theme => EvidenceSemanticAnchor.of(EvidenceSemanticAnchorKind.OpeningObserved, theme.toString))
      case payload: PlanPressureEvidence =>
        (
          payload.evidenceBackedPlans.map(plan =>
            EvidenceSemanticAnchor.of(EvidenceSemanticAnchorKind.PlanPressure, plan.plan.kind.id)
          ) ++
            payload.alignment.toList.map(alignment =>
              EvidenceSemanticAnchor.of(
                EvidenceSemanticAnchorKind.StructurePlan,
                alignment.band.toString,
                alignment.matchedPlanIds.sorted.mkString(",")
              )
            )
        ).distinct
      case payload: PlanCausalEventEvidence =>
        payload.semanticGroupingAnchors
      case PlanTransitionEvidence(transition) =>
        transition.currentEvent.map(current =>
          EvidenceSemanticAnchor.of(
            EvidenceSemanticAnchorKind.PlanTransition,
            (transition.previousEvent.toList.map(_.goalKey) ++ List(current.goalKey) ++
              transition.continuity.toList.map(continuity => s"${continuity.consecutivePlies}-ply"))*
          )
        ).toList
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

  def planTransitionCanSupportPlan(transition: PlanSequenceSummary): Boolean =
    transition.currentEvent.nonEmpty &&
      (if transition.transitionType == TransitionType.Opening then
         transition.previousEvent.isEmpty &&
           transition.previousPlanId.isEmpty &&
           transition.continuity.exists(continuity =>
             transition.currentEvent.exists(current =>
               continuity.startingEvent.contains(current) &&
                 continuity.supportingEvents.nonEmpty &&
                 continuity.supportingMoves == continuity.supportingEvents.map(_.rootMove) &&
                 !continuity.supportingEvents.contains(current) &&
                 continuity.consecutivePlies >= continuity.supportingEvents.size + 1 &&
                 continuity.startingPly >= 0
             )
           )
       else
         transition.previousPlanId == transition.primaryPlanId &&
           transition.previousEvent.exists(previous =>
             transition.continuity.exists(continuity =>
               continuity.episodeTransitionType == transition.transitionType &&
                 continuity.startingEvent == continuity.supportingEvents.headOption &&
                 continuity.supportingEvents.lastOption.contains(previous) &&
                 continuity.supportingMoves == continuity.supportingEvents.map(_.rootMove) &&
                 continuity.supportingEvents.nonEmpty &&
                 continuity.consecutivePlies >= continuity.supportingEvents.size + 1 &&
                 continuity.startingPly >= 0 &&
                 !continuity.supportingEvents.contains(transition.currentEvent.get)
             )
           ))

  def pawnStructureCanAnchorPlan(payload: PawnStructureFactEvidence): Boolean =
    payload.profile.primary != StructureId.Unknown && payload.profile.confidence >= 0.65 ||
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
        payload.pawnPlay.exists(pawnPlay =>
            pawnPlay.breakFile.exists(_.trim.nonEmpty) ||
              pawnPlay.tensionSquares.exists(_.trim.nonEmpty) ||
              pawnPlay.tensionEdges.exists(_.trim.nonEmpty) ||
              pawnPlay.counterBreakFiles.exists(_.trim.nonEmpty) ||
              pawnPlay.blockadeSquare.nonEmpty
          )
      case payload: PlanPressureEvidence =>
        payload.alignment.exists(_.matchedPlanIds.nonEmpty) ||
          payload.evidenceBackedPlans.nonEmpty
      case payload: StructuralDeltaEvidence =>
        payload.signals.exists(_.subjects.exists(_.trim.nonEmpty)) ||
          payload.consequences.exists(_.subjects.exists(_.trim.nonEmpty)) ||
          payload.developmentChoices.nonEmpty
      case payload: PlanCausalEventEvidence =>
        payload.identity.actorRole.nonEmpty &&
          (
            payload.identity.actorFrom.zip(payload.identity.actorTo).exists { case (from, to) =>
              EvidenceRef.sameMove(s"$from$to", payload.rootMove)
            } ||
            payload.identity.targets.nonEmpty ||
              payload.structuralConsequences.exists(_.subjects.exists(_.trim.nonEmpty)) ||
              payload.developmentChoices.nonEmpty ||
              payload.episode.exists(_.planSequenceProven)
          )
      case PlanTransitionEvidence(transition) =>
        transition.currentEvent.exists(event => event.targets.nonEmpty || event.actorRole.nonEmpty)
      case payload: BoardFactEvidence =>
        payload.targetHintSquares.nonEmpty ||
          payload.anchorFocusSquares.nonEmpty ||
          payload.lowLevelFacts.exists(factHasAxisSubject)
      case FeatureAnchorEvidence(_) | ApplicabilityAssessmentEvidence(_) =>
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
) extends EvidencePayload

final case class FeatureAnchorEvidence(
    anchor: FeatureAnchor
) extends EvidencePayload

final case class ApplicabilityAssessmentEvidence(
    assessment: ApplicabilityAssessment
) extends EvidencePayload

final case class ThreatEpisode(
    episodeId: String,
    sourceThreatIndex: Int,
    threat: Threat
):
  def threatActor: Color = threat.threatActor
  def sideUnderPressure: Color = threat.sideUnderPressure
  def kind: ThreatKind = threat.kind
  def severity: ThreatSeverity = threat.severity
  def driver: ThreatDriver = ThreatEpisode.driverFor(threat)
  def turnsToImpact: Int = threat.turnsToImpact
  def attackSquares: List[EvidenceSquare] =
    threat.attackSquares.distinct.map(EvidenceSquare(_))
  def targetPieces: List[EvidencePieceRole] =
    threat.targetPieces.distinct.map(EvidencePieceRole(_))
  def motifs: List[Motif] = threat.motifs
  def bestDefense: Option[String] = threat.bestDefense.map(EvidenceRef.normalizeMove)
  def defenseCount: Int = threat.defenseCount
  def immediate: Boolean =
    turnsToImpact <= 2
  def strategic: Boolean =
    turnsToImpact >= 3
  def defenseRequired: Boolean =
    severity != ThreatSeverity.Low
  def hasMotifProof: Boolean =
    motifs.nonEmpty
  def hasConcreteThreatProof: Boolean =
    hasMotifProof
  def motifKinds: List[String] =
    motifs.map(_.getClass.getSimpleName.stripSuffix("$")).distinct

object ThreatEpisode:
  def fromThreat(threat: Threat, index: Int): ThreatEpisode =
    ThreatEpisode(
      episodeId =
        s"${threat.threatActor.name}->${threat.sideUnderPressure.name}:threat:$index:${threat.kind}:${threat.turnsToImpact}",
      sourceThreatIndex = index,
      threat = threat
    )

  def fromThreats(threats: List[Threat]): List[ThreatEpisode] =
    threats.zipWithIndex.map { case (threat, index) =>
      fromThreat(threat, index)
    }

  private[judgment] def driverFor(threat: Threat): ThreatDriver =
    threat.kind match
      case ThreatKind.Mate       => ThreatDriver.MateThreat
      case ThreatKind.Material   => ThreatDriver.MaterialThreat
      case ThreatKind.Positional => ThreatDriver.PositionalThreat

final case class ThreatEpisodeEvidence(
    episode: ThreatEpisode
) extends EvidencePayload:
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
  def isProofSignalDefensivePressure: Boolean =
    !insufficientData &&
      (
        defenseRequired ||
          prophylaxisNeeded ||
          episode.severity != ThreatSeverity.Low
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

final case class LineObjectTrajectory(
    rootStep: LineReplayStep,
    futureStep: LineReplayStep,
    pieceRole: EvidencePieceRole,
    color: Color,
    rootFrom: EvidenceSquare,
    rootTo: EvidenceSquare,
    futureFrom: EvidenceSquare,
    futureTo: EvidenceSquare,
    plyOffset: Int
)

object LineObjectTrajectory:
  def remainsAtRootDestination(
      trajectory: LineObjectTrajectory,
      continuation: List[LineReplayStep]
  ): Boolean =
    def matchingPiece(fen: String): Boolean =
      Square.fromKey(trajectory.rootTo.key).exists(square =>
        _root_.chess.format.Fen
          .read(_root_.chess.variant.Standard, _root_.chess.format.Fen.Full(fen))
          .flatMap(_.board.pieceAt(square))
          .exists(piece =>
            piece.color == trajectory.color && piece.role.toString.equalsIgnoreCase(trajectory.pieceRole.name)
          )
      )
    matchingPiece(trajectory.rootStep.fenAfter) &&
      continuation.forall(step => matchingPiece(step.fenBefore) && matchingPiece(step.fenAfter))

  def provesObjectStatePrecondition(trajectory: LineObjectTrajectory): Boolean =
    val rootMove = EvidenceRef.normalizeMove(trajectory.rootStep.moveUci)
    val futureMove = EvidenceRef.normalizeMove(trajectory.futureStep.moveUci)
    def pieceAt(fen: String, square: EvidenceSquare): Option[chess.Piece] =
      Square.fromKey(square.key).flatMap(square =>
        _root_.chess.format.Fen
          .read(_root_.chess.variant.Standard, _root_.chess.format.Fen.Full(fen))
          .flatMap(_.board.pieceAt(square))
      )
    val before = pieceAt(trajectory.rootStep.fenBefore, trajectory.rootFrom)
    val afterRoot = pieceAt(trajectory.rootStep.fenAfter, trajectory.rootTo)
    val beforeFuture = pieceAt(trajectory.futureStep.fenBefore, trajectory.futureFrom)
    val afterFuture = pieceAt(trajectory.futureStep.fenAfter, trajectory.futureTo)
    before.exists(piece =>
      rootMove.take(2) == trajectory.rootFrom.key &&
        rootMove.slice(2, 4) == trajectory.rootTo.key &&
        futureMove.take(2) == trajectory.futureFrom.key &&
        futureMove.slice(2, 4) == trajectory.futureTo.key &&
        trajectory.rootTo == trajectory.futureFrom &&
        trajectory.plyOffset > 0 &&
        piece.color == trajectory.color &&
        piece.role.toString.equalsIgnoreCase(trajectory.pieceRole.name) &&
        afterRoot.contains(piece) &&
        beforeFuture.contains(piece) &&
        afterFuture.exists(candidate =>
          candidate.color == piece.color &&
            (candidate.role == piece.role || futureMove.length == 5)
        )
    )

  def find(
      rootStep: LineReplayStep,
      continuation: List[LineReplayStep],
      maxPlyOffset: Int = Int.MaxValue
  ): Option[LineObjectTrajectory] =
    val rootMove = EvidenceRef.normalizeMove(rootStep.moveUci)
    for
      rootFrom <- Square.fromKey(rootMove.take(2))
      rootTo <- Square.fromKey(rootMove.slice(2, 4))
      before <- _root_.chess.format.Fen.read(
        _root_.chess.variant.Standard,
        _root_.chess.format.Fen.Full(rootStep.fenBefore)
      )
      piece <- before.board.pieceAt(rootFrom)
      if _root_.chess.format.Fen
        .read(_root_.chess.variant.Standard, _root_.chess.format.Fen.Full(rootStep.fenAfter))
        .exists(_.board.pieceAt(rootTo).contains(piece))
      (futureStep, index) <- continuation.zipWithIndex
        .take(maxPlyOffset.max(0))
        .takeWhile { case (step, _) =>
          _root_.chess.format.Fen
            .read(_root_.chess.variant.Standard, _root_.chess.format.Fen.Full(step.fenBefore))
            .exists(_.board.pieceAt(rootTo).contains(piece))
        }
        .find { case (step, _) =>
          Square.fromKey(EvidenceRef.normalizeMove(step.moveUci).take(2)).contains(rootTo)
        }
      futureTo <- Square.fromKey(EvidenceRef.normalizeMove(futureStep.moveUci).slice(2, 4))
      if futureTo != rootFrom
      if _root_.chess.format.Fen
        .read(_root_.chess.variant.Standard, _root_.chess.format.Fen.Full(futureStep.fenAfter))
        .exists(position =>
          position.board.pieceAt(futureTo).exists(afterPiece =>
            afterPiece.color == piece.color &&
              (afterPiece.role == piece.role || rootMove.length == 5 || EvidenceRef.normalizeMove(futureStep.moveUci).length == 5)
          )
        )
    yield LineObjectTrajectory(
      rootStep = rootStep,
      futureStep = futureStep,
      pieceRole = EvidencePieceRole(piece.role.toString),
      color = piece.color,
      rootFrom = EvidenceSquare(rootFrom.key),
      rootTo = EvidenceSquare(rootTo.key),
      futureFrom = EvidenceSquare(rootTo.key),
      futureTo = EvidenceSquare(futureTo.key),
      plyOffset = index + 1
    )

final case class LineAccessTrajectory(
    enablingStep: LineReplayStep,
    enabledStep: LineReplayStep,
    interveningSteps: List[LineReplayStep],
    enabledPieceRole: EvidencePieceRole,
    color: Color,
    vacatedSquare: EvidenceSquare,
    enabledFrom: EvidenceSquare,
    enabledTo: EvidenceSquare,
    plyOffset: Int
):
  def placesPieceBeforeClearance: Boolean =
    EvidenceRef.sameMove(EvidenceRef.normalizeMove(enablingStep.moveUci).slice(2, 4), enabledFrom.key) &&
      EvidenceRef.sameMove(EvidenceRef.normalizeMove(enabledStep.moveUci).take(2), vacatedSquare.key)

final case class PawnAdvanceSupportTrajectory(
    supportingStep: LineReplayStep,
    pawnAdvanceStep: LineReplayStep,
    interveningSteps: List[LineReplayStep],
    supporterRole: EvidencePieceRole,
    color: Color,
    supporterSquare: EvidenceSquare,
    pawnFrom: EvidenceSquare,
    pawnTo: EvidenceSquare,
    plyOffset: Int
)

object PawnAdvanceSupportTrajectory:
  def find(
      supportingStep: LineReplayStep,
      pawnAdvanceStep: LineReplayStep,
      interveningSteps: List[LineReplayStep]
  ): Option[PawnAdvanceSupportTrajectory] =
    val supportingMove = EvidenceRef.normalizeMove(supportingStep.moveUci)
    val advanceMove = EvidenceRef.normalizeMove(pawnAdvanceStep.moveUci)
    for
      supporterFrom <- Square.fromKey(supportingMove.take(2))
      supporterTo <- Square.fromKey(supportingMove.slice(2, 4))
      pawnFrom <- Square.fromKey(advanceMove.take(2))
      pawnTo <- Square.fromKey(advanceMove.slice(2, 4))
      beforeSupporting <- position(supportingStep.fenBefore)
      afterSupporting <- position(supportingStep.fenAfter)
      supporter <- beforeSupporting.board.pieceAt(supporterFrom)
      if afterSupporting.board.pieceAt(supporterTo).contains(supporter)
      pawn <- afterSupporting.board.pieceAt(pawnFrom)
      if pawn.role == Pawn && pawn.color == supporter.color
      if straightForwardAdvance(pawn.color, pawnFrom.key, pawnTo.key)
      rearSupport =
        (supporter.role == Rook || supporter.role == Queen) &&
          !supportsPawn(beforeSupporting.board, supporterFrom.key, pawnFrom.key, pawn.color) &&
          supportsPawn(afterSupporting.board, supporterTo.key, pawnFrom.key, pawn.color)
      destinationSupport =
        !supportsSquare(beforeSupporting.board, supporterFrom, pawnTo, pawn.color) &&
          supportsSquare(afterSupporting.board, supporterTo, pawnTo, pawn.color)
      if rearSupport || destinationSupport
      if interveningSteps.forall(step =>
        position(step.fenBefore).exists(position =>
          position.board.pieceAt(supporterTo).contains(supporter) &&
            position.board.pieceAt(pawnFrom).contains(pawn) &&
            (if rearSupport then supportsPawn(position.board, supporterTo.key, pawnFrom.key, pawn.color)
             else supportsSquare(position.board, supporterTo, pawnTo, pawn.color))
        )
      )
      beforeAdvance <- position(pawnAdvanceStep.fenBefore)
      if beforeAdvance.board.pieceAt(supporterTo).contains(supporter)
      if beforeAdvance.board.pieceAt(pawnFrom).contains(pawn)
      if
        if rearSupport then supportsPawn(beforeAdvance.board, supporterTo.key, pawnFrom.key, pawn.color)
        else supportsSquare(beforeAdvance.board, supporterTo, pawnTo, pawn.color)
      afterAdvance <- position(pawnAdvanceStep.fenAfter)
      if afterAdvance.board.pieceAt(supporterTo).contains(supporter)
      if afterAdvance.board.pieceAt(pawnTo).contains(pawn)
      if
        supportsPawn(afterAdvance.board, supporterTo.key, pawnTo.key, pawn.color) ||
          supportsSquare(afterAdvance.board, supporterTo, pawnTo, pawn.color)
    yield PawnAdvanceSupportTrajectory(
      supportingStep = supportingStep,
      pawnAdvanceStep = pawnAdvanceStep,
      interveningSteps = interveningSteps,
      supporterRole = EvidencePieceRole(supporter.role.toString),
      color = supporter.color,
      supporterSquare = EvidenceSquare(supporterTo.key),
      pawnFrom = EvidenceSquare(pawnFrom.key),
      pawnTo = EvidenceSquare(pawnTo.key),
      plyOffset = interveningSteps.size + 1
    )

  def proves(trajectory: PawnAdvanceSupportTrajectory): Boolean =
    find(trajectory.supportingStep, trajectory.pawnAdvanceStep, trajectory.interveningSteps).contains(trajectory)

  private def position(fen: String): Option[chess.Position] =
    _root_.chess.format.Fen.read(_root_.chess.variant.Standard, _root_.chess.format.Fen.Full(fen))

  private def straightForwardAdvance(color: Color, from: String, to: String): Boolean =
    from.matches("[a-h][1-8]") &&
      to.matches("[a-h][1-8]") &&
      from.head == to.head &&
      {
        val direction = if color.white then 1 else -1
        val distance = to.last.asDigit - from.last.asDigit
        distance == direction ||
          (distance == direction * 2 && from.last.asDigit == (if color.white then 2 else 7))
      }

  private def supportsSquare(board: Board, supporter: Square, target: Square, color: Color): Boolean =
    board.pieceAt(supporter).exists(_.color == color) && board.attackers(target, color).exists(_ == supporter)

  private def supportsPawn(board: Board, supporter: String, pawn: String, color: Color): Boolean =
    if !supporter.matches("[a-h][1-8]") || !pawn.matches("[a-h][1-8]") || supporter.head != pawn.head then false
    else
      val supporterRank = supporter.last.asDigit
      val pawnRank = pawn.last.asDigit
      val behind = if color.white then supporterRank < pawnRank else supporterRank > pawnRank
      behind && (math.min(supporterRank, pawnRank) + 1 until math.max(supporterRank, pawnRank)).forall(rank =>
        Square.fromKey(s"${supporter.head}$rank").forall(board.pieceAt(_).isEmpty)
      )

object LineAccessTrajectory:
  def find(
      enablingStep: LineReplayStep,
      enabledStep: LineReplayStep,
      interveningSteps: List[LineReplayStep]
  ): Option[LineAccessTrajectory] =
    clearanceBeforeUse(enablingStep, enabledStep, interveningSteps)
      .orElse(placementBeforeClearance(enablingStep, enabledStep, interveningSteps))

  private def clearanceBeforeUse(
      enablingStep: LineReplayStep,
      enabledStep: LineReplayStep,
      interveningSteps: List[LineReplayStep]
  ): Option[LineAccessTrajectory] =
    val enablingMove = EvidenceRef.normalizeMove(enablingStep.moveUci)
    val enabledMove = EvidenceRef.normalizeMove(enabledStep.moveUci)
    for
      vacated <- Square.fromKey(enablingMove.take(2))
      enabledFrom <- Square.fromKey(enabledMove.take(2))
      enabledTo <- Square.fromKey(enabledMove.slice(2, 4))
      beforeEnabling <- position(enablingStep.fenBefore)
      afterEnabling <- position(enablingStep.fenAfter)
      enablingPiece <- beforeEnabling.board.pieceAt(vacated)
      enabledPiece <- beforeEnabling.board.pieceAt(enabledFrom)
      if enablingPiece.color == enabledPiece.color
      if afterEnabling.board.pieceAt(vacated).isEmpty
      if afterEnabling.board.pieceAt(enabledFrom).contains(enabledPiece)
      enabledPath = movementPath(enabledPiece, enabledFrom.key, enabledTo.key)
      if enabledTo == vacated || enabledPath.contains(vacated.key)
      if pathRemainsClear((enabledPath :+ vacated.key).distinct, enablingStep, enabledStep, interveningSteps)
      if interveningSteps.forall(step =>
        pieceAt(step.fenBefore, enabledFrom.key).contains(enabledPiece) &&
          pieceAt(step.fenAfter, enabledFrom.key).contains(enabledPiece)
      )
      if pieceAt(enabledStep.fenBefore, enabledFrom.key).contains(enabledPiece)
      if pieceAt(enabledStep.fenAfter, enabledTo.key).exists(piece =>
        piece.color == enabledPiece.color &&
          (piece.role == enabledPiece.role || enabledMove.length == 5)
      )
    yield LineAccessTrajectory(
      enablingStep = enablingStep,
      enabledStep = enabledStep,
      interveningSteps = interveningSteps,
      enabledPieceRole = EvidencePieceRole(enabledPiece.role.toString),
      color = enabledPiece.color,
      vacatedSquare = EvidenceSquare(vacated.key),
      enabledFrom = EvidenceSquare(enabledFrom.key),
      enabledTo = EvidenceSquare(enabledTo.key),
      plyOffset = interveningSteps.size + 1
    )

  private def placementBeforeClearance(
      enablingStep: LineReplayStep,
      enabledStep: LineReplayStep,
      interveningSteps: List[LineReplayStep]
  ): Option[LineAccessTrajectory] =
    val enablingMove = EvidenceRef.normalizeMove(enablingStep.moveUci)
    val clearanceMove = EvidenceRef.normalizeMove(enabledStep.moveUci)
    for
      placedFrom <- Square.fromKey(enablingMove.take(2))
      placedAt <- Square.fromKey(enablingMove.slice(2, 4))
      vacated <- Square.fromKey(clearanceMove.take(2))
      blockerTo <- Square.fromKey(clearanceMove.slice(2, 4))
      beforePlacement <- position(enablingStep.fenBefore)
      afterPlacement <- position(enablingStep.fenAfter)
      beforeClearance <- position(enabledStep.fenBefore)
      afterClearance <- position(enabledStep.fenAfter)
      placedPiece <- beforePlacement.board.pieceAt(placedFrom)
      if Set(Rook, Bishop, Queen)(placedPiece.role)
      if afterPlacement.board.pieceAt(placedAt).contains(placedPiece)
      blocker <- afterPlacement.board.pieceAt(vacated)
      if blocker.color == placedPiece.color
      if beforeClearance.board.pieceAt(placedAt).contains(placedPiece)
      if beforeClearance.board.pieceAt(vacated).contains(blocker)
      if afterClearance.board.pieceAt(placedAt).contains(placedPiece)
      if afterClearance.board.pieceAt(vacated).isEmpty
      if afterClearance.board.pieceAt(blockerTo).contains(blocker)
      direction <- lineDirection(placedPiece, placedAt, vacated)
      path = movementPath(placedPiece, placedAt.key, vacated.key)
      if pathRemainsClear(path, enablingStep, enabledStep, interveningSteps)
      if interveningSteps.forall(step =>
        pieceAt(step.fenBefore, placedAt.key).contains(placedPiece) &&
          pieceAt(step.fenAfter, placedAt.key).contains(placedPiece) &&
          pieceAt(step.fenBefore, vacated.key).contains(blocker) &&
          pieceAt(step.fenAfter, vacated.key).contains(blocker)
      )
      reachable = reachableRay(afterClearance.board, placedAt, direction)
      vacatedIndex = reachable.indexOf(vacated)
      if vacatedIndex >= 0 && reachable.size > vacatedIndex + 1
      enabledTo <- reachable.lastOption
    yield LineAccessTrajectory(
      enablingStep = enablingStep,
      enabledStep = enabledStep,
      interveningSteps = interveningSteps,
      enabledPieceRole = EvidencePieceRole(placedPiece.role.toString),
      color = placedPiece.color,
      vacatedSquare = EvidenceSquare(vacated.key),
      enabledFrom = EvidenceSquare(placedAt.key),
      enabledTo = EvidenceSquare(enabledTo.key),
      plyOffset = interveningSteps.size + 1
    )

  def proves(trajectory: LineAccessTrajectory): Boolean =
    find(trajectory.enablingStep, trajectory.enabledStep, trajectory.interveningSteps).contains(trajectory)

  private def position(fen: String): Option[chess.Position] =
    _root_.chess.format.Fen.read(_root_.chess.variant.Standard, _root_.chess.format.Fen.Full(fen))

  private def pieceAt(fen: String, square: String): Option[chess.Piece] =
    for
      position <- position(fen)
      square <- Square.fromKey(square)
      piece <- position.board.pieceAt(square)
    yield piece

  private def pathRemainsClear(
      path: List[String],
      enablingStep: LineReplayStep,
      enabledStep: LineReplayStep,
      interveningSteps: List[LineReplayStep]
  ): Boolean =
    val observedFens =
      enablingStep.fenAfter ::
        (interveningSteps.flatMap(step => List(step.fenBefore, step.fenAfter)) :+ enabledStep.fenBefore)
    observedFens.forall(fen => path.forall(square => pieceAt(fen, square).isEmpty))

  private def movementPath(piece: chess.Piece, from: String, to: String): List[String] =
    coordinates(from).zip(coordinates(to)).toList.flatMap { case ((fromFile, fromRank), (toFile, toRank)) =>
      val fileDelta = toFile - fromFile
      val rankDelta = toRank - fromRank
      val diagonal = fileDelta.abs == rankDelta.abs && fileDelta != 0
      val straight = (fileDelta == 0) != (rankDelta == 0)
      val roleSupportsPath =
        piece.role == Queen ||
          piece.role == Bishop && diagonal ||
          piece.role == Rook && straight
      val pawnDoubleStep =
        piece.role == Pawn &&
          fileDelta == 0 &&
          rankDelta == (if piece.color.white then 2 else -2)
      if pawnDoubleStep then
        List(s"${from.head}${fromRank + (if piece.color.white then 1 else -1) + 1}")
      else if !roleSupportsPath || (!diagonal && !straight) then Nil
      else
        val fileStep = Integer.signum(fileDelta)
        val rankStep = Integer.signum(rankDelta)
        (1 until math.max(fileDelta.abs, rankDelta.abs)).map(offset =>
          s"${('a' + fromFile + fileStep * offset).toChar}${fromRank + rankStep * offset + 1}"
        ).toList
    }

  private def lineDirection(piece: chess.Piece, from: Square, through: Square): Option[(Int, Int)] =
    for
      (fromFile, fromRank) <- coordinates(from.key)
      (throughFile, throughRank) <- coordinates(through.key)
      fileDelta = throughFile - fromFile
      rankDelta = throughRank - fromRank
      diagonal = fileDelta.abs == rankDelta.abs && fileDelta != 0
      straight = (fileDelta == 0) != (rankDelta == 0)
      if piece.role == Queen || piece.role == Bishop && diagonal || piece.role == Rook && straight
      if diagonal || straight
    yield Integer.signum(fileDelta) -> Integer.signum(rankDelta)

  private def reachableRay(board: Board, from: Square, direction: (Int, Int)): List[Square] =
    val (fileStep, rankStep) = direction
    coordinates(from.key).toList.flatMap { case (fromFile, fromRank) =>
      val ray = (1 to 7).flatMap { offset =>
        val file = fromFile + fileStep * offset
        val rank = fromRank + rankStep * offset
        Option
          .when(file >= 0 && file < 8 && rank >= 0 && rank < 8)(s"${('a' + file).toChar}${rank + 1}")
          .flatMap(Square.fromKey)
      }.toList
      val firstOccupied = ray.indexWhere(board.pieceAt(_).nonEmpty)
      if firstOccupied >= 0 then ray.take(firstOccupied + 1) else ray
    }

  private def coordinates(square: String): Option[(Int, Int)] =
    Option.when(square.matches("[a-h][1-8]"))((square.head - 'a', square.last.asDigit - 1))

final case class RetreatControlTrajectory(
    supportingStep: LineReplayStep,
    pressuringStep: LineReplayStep,
    interveningSteps: List[LineReplayStep],
    supporterRole: EvidencePieceRole,
    pressuredRole: EvidencePieceRole,
    color: Color,
    supporterSquare: EvidenceSquare,
    pressuredSquare: EvidenceSquare,
    controlledRetreatSquare: EvidenceSquare,
    plyOffset: Int
)

object RetreatControlTrajectory:
  def find(
      supportingStep: LineReplayStep,
      pressuringStep: LineReplayStep,
      interveningSteps: List[LineReplayStep]
  ): Option[RetreatControlTrajectory] =
    val supportingMove = EvidenceRef.normalizeMove(supportingStep.moveUci)
    val pressuringMove = EvidenceRef.normalizeMove(pressuringStep.moveUci)
    for
      supporterFrom <- Square.fromKey(supportingMove.take(2))
      supporterSquare <- Square.fromKey(supportingMove.slice(2, 4))
      pressureFrom <- Square.fromKey(pressuringMove.take(2))
      pressureTo <- Square.fromKey(pressuringMove.slice(2, 4))
      beforeSupport <- position(supportingStep.fenBefore)
      afterSupport <- position(supportingStep.fenAfter)
      beforePressure <- position(pressuringStep.fenBefore)
      afterPressure <- position(pressuringStep.fenAfter)
      supporter <- beforeSupport.board.pieceAt(supporterFrom)
      if supporter.role != Pawn
      if afterSupport.board.pieceAt(supporterSquare).contains(supporter)
      if interveningSteps.forall(step =>
        pieceAt(step.fenBefore, supporterSquare).contains(supporter) &&
          pieceAt(step.fenAfter, supporterSquare).contains(supporter)
      )
      if beforePressure.board.pieceAt(supporterSquare).contains(supporter)
      if afterPressure.board.pieceAt(supporterSquare).contains(supporter)
      pressuringPiece <- beforePressure.board.pieceAt(pressureFrom)
      if pressuringPiece.color == supporter.color
      if pressuringPiece.role == Pawn
      if pressureFrom.file == pressureTo.file
      if pressureTo.rank.value - pressureFrom.rank.value == (if pressuringPiece.color.white then 1 else -1)
      if beforePressure.board.pieceAt(pressureTo).isEmpty
      if afterPressure.board.pieceAt(pressureTo).contains(pressuringPiece)
      pressuredSquare <- attacks(afterPressure.board, pressureTo, pressuringPiece)
        .squares
        .filter(square => beforePressure.board.pieceAt(square).exists(_.color != supporter.color))
        .filter(square => afterPressure.board.pieceAt(square) == beforePressure.board.pieceAt(square))
        .toList
        .sortBy(_.key)
        .headOption
      pressuredPiece <- beforePressure.board.pieceAt(pressuredSquare)
      if pressuredPiece.role != Pawn
      newlyControlled =
        attacks(afterSupport.board, supporterSquare, supporter) &
          ~attacks(beforeSupport.board, supporterFrom, supporter)
      controlledAtPressure = attacks(afterPressure.board, supporterSquare, supporter)
      retreatSquare <- (attacks(beforePressure.board, pressuredSquare, pressuredPiece) &
        ~beforePressure.board.byColor(pressuredPiece.color) &
        newlyControlled &
        controlledAtPressure)
        .squares
        .filter(square => beforePressure.board.pieceAt(square).isEmpty)
        .toList
        .sortBy(_.key)
        .headOption
    yield RetreatControlTrajectory(
      supportingStep = supportingStep,
      pressuringStep = pressuringStep,
      interveningSteps = interveningSteps,
      supporterRole = EvidencePieceRole(supporter.role.toString),
      pressuredRole = EvidencePieceRole(pressuredPiece.role.toString),
      color = supporter.color,
      supporterSquare = EvidenceSquare(supporterSquare.key),
      pressuredSquare = EvidenceSquare(pressuredSquare.key),
      controlledRetreatSquare = EvidenceSquare(retreatSquare.key),
      plyOffset = interveningSteps.size + 1
    )

  def proves(trajectory: RetreatControlTrajectory): Boolean =
    find(trajectory.supportingStep, trajectory.pressuringStep, trajectory.interveningSteps).contains(trajectory)

  private def position(fen: String): Option[chess.Position] =
    _root_.chess.format.Fen.read(
      _root_.chess.variant.Standard,
      _root_.chess.format.Fen.Full(fen)
    )

  private def pieceAt(fen: String, square: Square): Option[chess.Piece] =
    position(fen).flatMap(_.board.pieceAt(square))

  private def attacks(board: chess.Board, square: Square, piece: chess.Piece): Bitboard =
    piece.role match
      case Pawn   => square.pawnAttacks(piece.color)
      case Knight => square.knightAttacks
      case Bishop => square.bishopAttacks(board.occupied)
      case Rook   => square.rookAttacks(board.occupied)
      case Queen  => square.queenAttacks(board.occupied)
      case King   => square.kingAttacks

enum PawnBreakFollowUpKind:
  case NextPawnLever
  case ReleasedPassedPawn

sealed trait PlanResponseContinuationTrajectory:
  def triggerStep: LineReplayStep
  def replyStep: LineReplayStep
  def followUpStep: LineReplayStep
  def interveningSteps: List[LineReplayStep]
  def replyFrom: EvidenceSquare
  def replyTo: EvidenceSquare
  def followUpFrom: EvidenceSquare
  def followUpTo: EvidenceSquare
  def plyOffset: Int
  def involvedRoles: List[EvidencePieceRole]

final case class PawnBreakFollowUpTrajectory(
    breakStep: LineReplayStep,
    replyStep: LineReplayStep,
    followUpStep: LineReplayStep,
    interveningSteps: List[LineReplayStep],
    kind: PawnBreakFollowUpKind,
    color: Color,
    replyFrom: EvidenceSquare,
    replyTo: EvidenceSquare,
    followUpFrom: EvidenceSquare,
    followUpTo: EvidenceSquare,
    releasedPassedPawn: Option[EvidenceSquare],
    plyOffset: Int
) extends PlanResponseContinuationTrajectory:
  def triggerStep: LineReplayStep = breakStep
  def involvedRoles: List[EvidencePieceRole] = List(EvidencePieceRole(Pawn.toString))

object PawnBreakFollowUpTrajectory:
  def find(
      breakStep: LineReplayStep,
      replyStep: LineReplayStep,
      followUpStep: LineReplayStep,
      interveningSteps: List[LineReplayStep]
  ): Option[PawnBreakFollowUpTrajectory] =
    val breakMove = EvidenceRef.normalizeMove(breakStep.moveUci)
    val replyMove = EvidenceRef.normalizeMove(replyStep.moveUci)
    val followUpMove = EvidenceRef.normalizeMove(followUpStep.moveUci)
    for
      breakFrom <- Square.fromKey(breakMove.take(2))
      breakTo <- Square.fromKey(breakMove.slice(2, 4))
      replyFrom <- Square.fromKey(replyMove.take(2))
      replyTo <- Square.fromKey(replyMove.slice(2, 4))
      followUpFrom <- Square.fromKey(followUpMove.take(2))
      followUpTo <- Square.fromKey(followUpMove.slice(2, 4))
      beforeBreak <- position(breakStep.fenBefore)
      afterBreak <- position(breakStep.fenAfter)
      beforeReply <- position(replyStep.fenBefore)
      afterReply <- position(replyStep.fenAfter)
      beforeFollowUp <- position(followUpStep.fenBefore)
      afterFollowUp <- position(followUpStep.fenAfter)
      breakPawn <- beforeBreak.board.pieceAt(breakFrom)
      if breakPawn.role == Pawn
      if afterBreak.board.pieceAt(breakTo).contains(breakPawn)
      if beforeReply.board.pieceAt(breakTo).contains(breakPawn)
      replyPawn <- beforeReply.board.pieceAt(replyFrom)
      if replyPawn.role == Pawn && replyPawn.color == !breakPawn.color
      if replyTo == breakTo
      if afterReply.board.pieceAt(replyTo).contains(replyPawn)
      followUpPawn <- beforeFollowUp.board.pieceAt(followUpFrom)
      if followUpPawn.role == Pawn && followUpPawn.color == breakPawn.color
      if afterFollowUp.board.pieceAt(followUpTo).exists(piece =>
        piece.color == breakPawn.color && (piece.role == Pawn || followUpMove.length == 5)
      )
      if interveningSteps.headOption.contains(replyStep)
      if followUpStep.ply - breakStep.ply == interveningSteps.size + 1
      passedBeforeReply = passedPawns(afterBreak, breakPawn.color)
      passedAfterReply = passedPawns(afterReply, breakPawn.color)
      released = passedAfterReply.diff(passedBeforeReply)
      releasedAdvance = released.find(square =>
        square == followUpFrom.key &&
          passedPawns(beforeFollowUp, breakPawn.color)(square) &&
          interveningSteps.drop(1).forall(step => pieceAt(step.fenBefore, square).contains(followUpPawn))
      )
      immediateLocalLever =
        interveningSteps == List(replyStep) &&
          (followUpFrom.file.value - breakFrom.file.value).abs <= 1
      kind <- releasedAdvance
        .map(_ => PawnBreakFollowUpKind.ReleasedPassedPawn)
        .orElse(Option.when(immediateLocalLever)(PawnBreakFollowUpKind.NextPawnLever))
    yield PawnBreakFollowUpTrajectory(
      breakStep = breakStep,
      replyStep = replyStep,
      followUpStep = followUpStep,
      interveningSteps = interveningSteps,
      kind = kind,
      color = breakPawn.color,
      replyFrom = EvidenceSquare(replyFrom.key),
      replyTo = EvidenceSquare(replyTo.key),
      followUpFrom = EvidenceSquare(followUpFrom.key),
      followUpTo = EvidenceSquare(followUpTo.key),
      releasedPassedPawn = releasedAdvance.map(EvidenceSquare(_)),
      plyOffset = followUpStep.ply - breakStep.ply
    )

  def proves(trajectory: PawnBreakFollowUpTrajectory): Boolean =
    find(
      trajectory.breakStep,
      trajectory.replyStep,
      trajectory.followUpStep,
      trajectory.interveningSteps
    ).contains(trajectory)

  private def position(fen: String): Option[chess.Position] =
    _root_.chess.format.Fen.read(
      _root_.chess.variant.Standard,
      _root_.chess.format.Fen.Full(fen)
    )

  private def pieceAt(fen: String, square: String): Option[chess.Piece] =
    for
      position <- position(fen)
      square <- Square.fromKey(square)
      piece <- position.board.pieceAt(square)
    yield piece

  private def passedPawns(position: chess.Position, color: Color): Set[String] =
    PawnTopology
      .passedPawns(color, position.board.byPiece(color, Pawn), position.board.byPiece(!color, Pawn))
      .map(_.key)
      .toSet

final case class CaptureResponseFollowUpTrajectory(
    triggerStep: LineReplayStep,
    replyStep: LineReplayStep,
    followUpStep: LineReplayStep,
    interveningSteps: List[LineReplayStep],
    triggerRole: EvidencePieceRole,
    responderRole: EvidencePieceRole,
    followUpRole: EvidencePieceRole,
    replyFrom: EvidenceSquare,
    replyTo: EvidenceSquare,
    followUpFrom: EvidenceSquare,
    followUpTo: EvidenceSquare,
    plyOffset: Int
) extends PlanResponseContinuationTrajectory:
  def involvedRoles: List[EvidencePieceRole] = List(triggerRole, responderRole, followUpRole).distinct

object CaptureResponseFollowUpTrajectory:
  def find(
      triggerStep: LineReplayStep,
      replyStep: LineReplayStep,
      followUpStep: LineReplayStep,
      interveningSteps: List[LineReplayStep]
  ): Option[CaptureResponseFollowUpTrajectory] =
    val triggerMove = EvidenceRef.normalizeMove(triggerStep.moveUci)
    val replyMove = EvidenceRef.normalizeMove(replyStep.moveUci)
    val followUpMove = EvidenceRef.normalizeMove(followUpStep.moveUci)
    for
      triggerFrom <- Square.fromKey(triggerMove.take(2))
      triggerTo <- Square.fromKey(triggerMove.slice(2, 4))
      replyFrom <- Square.fromKey(replyMove.take(2))
      replyTo <- Square.fromKey(replyMove.slice(2, 4))
      followUpFrom <- Square.fromKey(followUpMove.take(2))
      followUpTo <- Square.fromKey(followUpMove.slice(2, 4))
      beforeTrigger <- position(triggerStep.fenBefore)
      afterTrigger <- position(triggerStep.fenAfter)
      beforeReply <- position(replyStep.fenBefore)
      afterReply <- position(replyStep.fenAfter)
      beforeFollowUp <- position(followUpStep.fenBefore)
      afterFollowUp <- position(followUpStep.fenAfter)
      triggerActor <- beforeTrigger.board.pieceAt(triggerFrom)
      if afterTrigger.board.pieceAt(triggerTo).contains(triggerActor)
      if beforeReply.board.pieceAt(triggerTo).contains(triggerActor)
      responder <- beforeReply.board.pieceAt(replyFrom)
      if responder.color == !triggerActor.color && replyTo == triggerTo
      if afterReply.board.pieceAt(replyTo).contains(responder)
      follower <- beforeFollowUp.board.pieceAt(followUpFrom)
      if follower.color == triggerActor.color && followUpTo == replyTo
      if beforeFollowUp.board.pieceAt(replyTo).contains(responder)
      if afterFollowUp.board.pieceAt(followUpTo).exists(_.color == triggerActor.color)
      if interveningSteps == List(replyStep)
      if followUpStep.ply - triggerStep.ply == 2
    yield CaptureResponseFollowUpTrajectory(
      triggerStep = triggerStep,
      replyStep = replyStep,
      followUpStep = followUpStep,
      interveningSteps = interveningSteps,
      triggerRole = EvidencePieceRole(triggerActor.role.toString),
      responderRole = EvidencePieceRole(responder.role.toString),
      followUpRole = EvidencePieceRole(follower.role.toString),
      replyFrom = EvidenceSquare(replyFrom.key),
      replyTo = EvidenceSquare(replyTo.key),
      followUpFrom = EvidenceSquare(followUpFrom.key),
      followUpTo = EvidenceSquare(followUpTo.key),
      plyOffset = 2
    )

  def proves(trajectory: CaptureResponseFollowUpTrajectory): Boolean =
    find(
      trajectory.triggerStep,
      trajectory.replyStep,
      trajectory.followUpStep,
      trajectory.interveningSteps
    ).contains(trajectory)

  private def position(fen: String): Option[chess.Position] =
    _root_.chess.format.Fen.read(
      _root_.chess.variant.Standard,
      _root_.chess.format.Fen.Full(fen)
    )

final case class CheckResponseFollowUpTrajectory(
    triggerStep: LineReplayStep,
    replyStep: LineReplayStep,
    followUpStep: LineReplayStep,
    interveningSteps: List[LineReplayStep],
    triggerRole: EvidencePieceRole,
    responderRole: EvidencePieceRole,
    followUpRole: EvidencePieceRole,
    replyFrom: EvidenceSquare,
    replyTo: EvidenceSquare,
    followUpFrom: EvidenceSquare,
    followUpTo: EvidenceSquare,
    plyOffset: Int
) extends PlanResponseContinuationTrajectory:
  def involvedRoles: List[EvidencePieceRole] = List(triggerRole, responderRole, followUpRole).distinct

object CheckResponseFollowUpTrajectory:
  def find(
      triggerStep: LineReplayStep,
      replyStep: LineReplayStep,
      followUpStep: LineReplayStep,
      interveningSteps: List[LineReplayStep]
  ): Option[CheckResponseFollowUpTrajectory] =
    val triggerMove = EvidenceRef.normalizeMove(triggerStep.moveUci)
    val replyMove = EvidenceRef.normalizeMove(replyStep.moveUci)
    val followUpMove = EvidenceRef.normalizeMove(followUpStep.moveUci)
    for
      triggerFrom <- Square.fromKey(triggerMove.take(2))
      triggerTo <- Square.fromKey(triggerMove.slice(2, 4))
      replyFrom <- Square.fromKey(replyMove.take(2))
      replyTo <- Square.fromKey(replyMove.slice(2, 4))
      followUpFrom <- Square.fromKey(followUpMove.take(2))
      followUpTo <- Square.fromKey(followUpMove.slice(2, 4))
      beforeTrigger <- position(triggerStep.fenBefore)
      afterTrigger <- position(triggerStep.fenAfter)
      beforeReply <- position(replyStep.fenBefore)
      afterReply <- position(replyStep.fenAfter)
      beforeFollowUp <- position(followUpStep.fenBefore)
      afterFollowUp <- position(followUpStep.fenAfter)
      triggerActor <- beforeTrigger.board.pieceAt(triggerFrom)
      if afterTrigger.board.pieceAt(triggerTo).contains(triggerActor)
      if afterTrigger.check.yes && beforeReply.check.yes
      responder <- beforeReply.board.pieceAt(replyFrom)
      if responder.color == !triggerActor.color
      if afterReply.board.pieceAt(replyTo).contains(responder)
      if !afterReply.check.yes
      follower <- beforeFollowUp.board.pieceAt(followUpFrom)
      if follower.color == triggerActor.color
      if afterFollowUp.board.pieceAt(followUpTo).exists(_.color == triggerActor.color)
      if interveningSteps == List(replyStep)
      if followUpStep.ply - triggerStep.ply == 2
    yield CheckResponseFollowUpTrajectory(
      triggerStep = triggerStep,
      replyStep = replyStep,
      followUpStep = followUpStep,
      interveningSteps = interveningSteps,
      triggerRole = EvidencePieceRole(triggerActor.role.toString),
      responderRole = EvidencePieceRole(responder.role.toString),
      followUpRole = EvidencePieceRole(follower.role.toString),
      replyFrom = EvidenceSquare(replyFrom.key),
      replyTo = EvidenceSquare(replyTo.key),
      followUpFrom = EvidenceSquare(followUpFrom.key),
      followUpTo = EvidenceSquare(followUpTo.key),
      plyOffset = 2
    )

  def proves(trajectory: CheckResponseFollowUpTrajectory): Boolean =
    find(
      trajectory.triggerStep,
      trajectory.replyStep,
      trajectory.followUpStep,
      trajectory.interveningSteps
    ).contains(trajectory)

  private def position(fen: String): Option[chess.Position] =
    _root_.chess.format.Fen.read(
      _root_.chess.variant.Standard,
      _root_.chess.format.Fen.Full(fen)
    )

final case class ExchangeConversionTrajectory(
    planRootStep: LineReplayStep,
    triggerStep: LineReplayStep,
    replyStep: LineReplayStep,
    followUpStep: LineReplayStep,
    interveningSteps: List[LineReplayStep],
    exchangeCapture: LineMaterialCapture,
    exchangeRecapture: LineMaterialCapture,
    checkPrefix: Option[CheckResponseFollowUpTrajectory],
    exchangeActorRoute: Option[LineObjectTrajectory],
    convertingPawnAtPhaseBoundary: EvidenceSquare,
    materialSummary: LineMaterialSummary
) extends PlanResponseContinuationTrajectory:
  private def moveSquares(step: LineReplayStep): (EvidenceSquare, EvidenceSquare) =
    val move = EvidenceRef.normalizeMove(step.moveUci)
    EvidenceSquare(move.take(2)) -> EvidenceSquare(move.slice(2, 4))
  def replyFrom: EvidenceSquare = moveSquares(replyStep)._1
  def replyTo: EvidenceSquare = moveSquares(replyStep)._2
  def followUpFrom: EvidenceSquare = moveSquares(followUpStep)._1
  def followUpTo: EvidenceSquare = moveSquares(followUpStep)._2
  def plyOffset: Int = followUpStep.ply - triggerStep.ply
  def involvedRoles: List[EvidencePieceRole] =
    List(exchangeCapture.attackerRole, EvidencePieceRole(Pawn.toString)).distinct

object ExchangeConversionTrajectory:
  private val MaterialRoles = List(Knight, Bishop, Rook, Queen)

  def find(
      planRootStep: LineReplayStep,
      exchangeStep: LineReplayStep,
      replyStep: LineReplayStep,
      promotionStep: LineReplayStep,
      interveningSteps: List[LineReplayStep],
      planPrefix: List[LineReplayStep],
      materialSummary: LineMaterialSummary
  ): Option[ExchangeConversionTrajectory] =
    val exchangeOffset = exchangeStep.ply - planRootStep.ply
    val replyOffset = replyStep.ply - planRootStep.ply
    val rootIsExchange = exchangeStep == planRootStep
    val prefixProof =
      if rootIsExchange && planPrefix.isEmpty then Some(None -> None)
      else if exchangeOffset == 2 && planPrefix.size == 1 then
        for
          check <- CheckResponseFollowUpTrajectory
            .find(planRootStep, planPrefix.head, exchangeStep, planPrefix)
          route <- LineObjectTrajectory
            .find(planRootStep, planPrefix :+ exchangeStep, maxPlyOffset = 2)
            .filter(_.futureStep == exchangeStep)
        yield Some(check) -> Some(route)
      else None
    for
      (checkPrefix, actorRoute) <- prefixProof
      if materialSummary.materialWindowComplete
      pair <- materialSummary.lastPieceExchangePairAt(exchangeOffset, replyOffset)
      (exchangeCapture, exchangeRecapture) = pair
      if replyOffset == exchangeOffset + 1
      if interveningSteps.headOption.contains(replyStep)
      if promotionStep.ply - exchangeStep.ply == interveningSteps.size + 1
      if actualLastPieceExchange(exchangeStep, replyStep, exchangeCapture, exchangeRecapture)
      phaseBoundaryPawn <- convertingPawn(
        replyStep,
        promotionStep,
        interveningSteps.drop(1),
        exchangeCapture.side
      )
      chain =
        (if rootIsExchange then List(planRootStep)
         else planRootStep :: (planPrefix :+ exchangeStep)) ++
          interveningSteps :+ promotionStep
      if continuous(chain)
    yield ExchangeConversionTrajectory(
      planRootStep = planRootStep,
      triggerStep = exchangeStep,
      replyStep = replyStep,
      followUpStep = promotionStep,
      interveningSteps = interveningSteps,
      exchangeCapture = exchangeCapture,
      exchangeRecapture = exchangeRecapture,
      checkPrefix = checkPrefix,
      exchangeActorRoute = actorRoute,
      convertingPawnAtPhaseBoundary = EvidenceSquare(phaseBoundaryPawn.key),
      materialSummary = materialSummary
    )

  def proves(trajectory: ExchangeConversionTrajectory): Boolean =
    find(
      trajectory.planRootStep,
      trajectory.triggerStep,
      trajectory.replyStep,
      trajectory.followUpStep,
      trajectory.interveningSteps,
      trajectory.checkPrefix.map(_.replyStep).toList,
      trajectory.materialSummary
    ).contains(trajectory)

  private def actualLastPieceExchange(
      exchangeStep: LineReplayStep,
      replyStep: LineReplayStep,
      exchangeCapture: LineMaterialCapture,
      exchangeRecapture: LineMaterialCapture
  ): Boolean =
    val exchangeMove = EvidenceRef.normalizeMove(exchangeStep.moveUci)
    val replyMove = EvidenceRef.normalizeMove(replyStep.moveUci)
    (for
      exchangeFrom <- Square.fromKey(exchangeMove.take(2))
      exchangeTo <- Square.fromKey(exchangeMove.slice(2, 4))
      replyFrom <- Square.fromKey(replyMove.take(2))
      replyTo <- Square.fromKey(replyMove.slice(2, 4))
      beforeExchange <- position(exchangeStep.fenBefore)
      afterExchange <- position(exchangeStep.fenAfter)
      beforeReply <- position(replyStep.fenBefore)
      afterReply <- position(replyStep.fenAfter)
      actor <- beforeExchange.board.pieceAt(exchangeFrom)
      peer <- beforeExchange.board.pieceAt(exchangeTo)
      responder <- beforeReply.board.pieceAt(replyFrom)
    yield
      actor.color == exchangeCapture.side &&
        actor.role == peer.role &&
        !Set(Pawn, King)(actor.role) &&
        peer.color == !actor.color &&
        afterExchange.board.pieceAt(exchangeTo).contains(actor) &&
        PrincipalVariationEvidence.sameBoardState(exchangeStep.fenAfter, replyStep.fenBefore) &&
        replyTo == exchangeTo &&
        responder.color == peer.color &&
        afterReply.board.pieceAt(replyTo).contains(responder) &&
        nonPawnMaterialCount(beforeExchange.board) == 2 &&
        beforeExchange.board.byPiece(actor.color, actor.role).count == 1 &&
        beforeExchange.board.byPiece(peer.color, peer.role).count == 1 &&
        nonPawnMaterialCount(afterReply.board) == 0 &&
        captureMatches(exchangeCapture, exchangeStep, actor, peer, exchangeTo) &&
        captureMatches(exchangeRecapture, replyStep, responder, actor, replyTo)
    ).contains(true)

  private def convertingPawn(
      replyStep: LineReplayStep,
      promotionStep: LineReplayStep,
      betweenReplyAndPromotion: List[LineReplayStep],
      color: Color
  ): Option[Square] =
    val promotionMove = EvidenceRef.normalizeMove(promotionStep.moveUci)
    for
      promotionFrom <- Square.fromKey(promotionMove.take(2))
      promotionTo <- Square.fromKey(promotionMove.slice(2, 4))
      if promotionMove.length == 5
      beforePromotion <- position(promotionStep.fenBefore)
      afterPromotion <- position(promotionStep.fenAfter)
      if beforePromotion.board.pieceAt(promotionFrom).contains(Piece(color, Pawn))
      promoted <- afterPromotion.board.pieceAt(promotionTo)
      if promoted.color == color && promoted.role != Pawn && promoted.role != King
      boundarySquare <- betweenReplyAndPromotion.reverse.foldLeft(Option(promotionFrom)) {
        case (None, _) => None
        case (Some(current), step) =>
          val move = EvidenceRef.normalizeMove(step.moveUci)
          for
            from <- Square.fromKey(move.take(2))
            to <- Square.fromKey(move.slice(2, 4))
            before <- position(step.fenBefore)
            after <- position(step.fenAfter)
            previous <-
              if to == current &&
                  before.board.pieceAt(from).contains(Piece(color, Pawn)) &&
                  after.board.pieceAt(to).contains(Piece(color, Pawn))
              then Some(from)
              else if
                before.board.pieceAt(current).contains(Piece(color, Pawn)) &&
                  after.board.pieceAt(current).contains(Piece(color, Pawn))
              then Some(current)
              else None
          yield previous
      }
      boundary <- position(replyStep.fenAfter)
      if boundary.board.pieceAt(boundarySquare).contains(Piece(color, Pawn))
    yield boundarySquare

  private def captureMatches(
      capture: LineMaterialCapture,
      step: LineReplayStep,
      attacker: Piece,
      captured: Piece,
      square: Square
  ): Boolean =
    EvidenceRef.sameMove(capture.moveUci, step.moveUci) &&
      capture.side == attacker.color &&
      capture.attackerRole.name.equalsIgnoreCase(attacker.role.toString) &&
      capture.capturedRole.name.equalsIgnoreCase(captured.role.toString) &&
      capture.square.key.equalsIgnoreCase(square.key)

  private def nonPawnMaterialCount(board: Board): Int =
    List(Color.White, Color.Black).map(color => MaterialRoles.map(role => board.byPiece(color, role).count).sum).sum

  private def continuous(steps: List[LineReplayStep]): Boolean =
    steps.zip(steps.drop(1)).forall((left, right) =>
      PrincipalVariationEvidence.sameBoardState(left.fenAfter, right.fenBefore)
    )

  private def position(fen: String): Option[chess.Position] =
    _root_.chess.format.Fen.read(
      _root_.chess.variant.Standard,
      _root_.chess.format.Fen.Full(fen)
    )

object PlanResponseContinuationTrajectory:
  def proves(trajectory: PlanResponseContinuationTrajectory): Boolean =
    trajectory match
      case pawn: PawnBreakFollowUpTrajectory          => PawnBreakFollowUpTrajectory.proves(pawn)
      case capture: CaptureResponseFollowUpTrajectory => CaptureResponseFollowUpTrajectory.proves(capture)
      case check: CheckResponseFollowUpTrajectory     => CheckResponseFollowUpTrajectory.proves(check)
      case exchange: ExchangeConversionTrajectory     => ExchangeConversionTrajectory.proves(exchange)

enum LineEndgameTechniqueHorizonStatus:
  case Active
  case Transitioned
  case Completed
  case Failed
  case SupersededByTactic
  case ContradictedByTerminalProof

final case class EndgameZugzwangReplyProof(
    moveUci: String,
    resultingDtm: Int
)

final case class EndgameZugzwangProof private (
    constrainedSide: Color,
    terminalFen: String,
    comparisonFen: String,
    terminalDtm: Int,
    comparisonDtm: Int,
    legalReplies: List[EndgameZugzwangReplyProof]
):
  def constrainedSideKey: String =
    if constrainedSide.white then "white" else "black"

  private[chessjudgment] def validFor(
      horizon: LineEndgameTechniqueHorizon,
      replay: List[LineReplayStep]
  ): Boolean =
    val normalizedReplies =
      legalReplies.map(reply => reply.copy(moveUci = EvidenceRef.normalizeMove(reply.moveUci)))
    (
      for
        trigger <- horizon.triggerMove
        root <- replay.headOption
        entry <- replay.lift(horizon.entryPlyOffset)
        terminal <- replay.lift(horizon.terminalPlyOffset)
        terminalPosition <- EndgameZugzwangProof.position(terminalFen)
        comparisonPosition <- EndgameZugzwangProof.position(comparisonFen)
        replyDtms <- Option.when(
          normalizedReplies.nonEmpty &&
            normalizedReplies.map(_.moveUci).distinct.size == normalizedReplies.size &&
            normalizedReplies.forall(_.resultingDtm > 0)
        )(normalizedReplies.map(_.resultingDtm))
        maxReplyDtm <- replyDtms.maxOption
      yield
        val expectedMoves =
          terminalPosition.legalMoves.map(_.toUci.uci).map(EvidenceRef.normalizeMove).distinct.sorted
        horizon.pattern == "Triangulation" &&
          horizon.status == LineEndgameTechniqueHorizonStatus.Completed &&
          horizon.entryPlyOffset == 0 &&
          horizon.terminalPlyOffset == 4 &&
          EvidenceRef.sameMove(trigger, root.moveUci) &&
          EndgameZugzwangProof.normalizeFen(terminalFen) == EndgameZugzwangProof.normalizeFen(terminal.fenAfter) &&
          EndgameZugzwangProof.normalizeFen(comparisonFen) == EndgameZugzwangProof.normalizeFen(entry.fenBefore) &&
          EndgameZugzwangProof.sameBoardWithoutTurn(terminalFen, comparisonFen) &&
          constrainedSide == terminalPosition.color &&
          constrainedSide == !horizon.techniqueSide &&
          comparisonPosition.color == horizon.techniqueSide &&
          normalizedReplies.map(_.moveUci).sorted == expectedMoves &&
          terminalDtm < 0 &&
          terminalDtm.toLong.abs == maxReplyDtm.toLong + 1L &&
          comparisonDtm > maxReplyDtm
    ).contains(true)

object EndgameZugzwangProof:
  private[chessjudgment] def verified(
      constrainedSide: Color,
      terminalFen: String,
      comparisonFen: String,
      terminalDtm: Int,
      comparisonDtm: Int,
      legalReplies: List[EndgameZugzwangReplyProof],
      horizon: LineEndgameTechniqueHorizon,
      replay: List[LineReplayStep]
  ): Option[EndgameZugzwangProof] =
    val proof =
      new EndgameZugzwangProof(
        constrainedSide,
        terminalFen,
        comparisonFen,
        terminalDtm,
        comparisonDtm,
        legalReplies
      )
    Option.when(proof.validFor(horizon, replay))(proof)

  private def position(fen: String): Option[chess.Position] =
    _root_.chess.format.Fen.read(
      _root_.chess.variant.Standard,
      _root_.chess.format.Fen.Full(fen)
    )

  private def normalizeFen(fen: String): String =
    Option(fen).getOrElse("").trim.split("\\s+").filter(_.nonEmpty).mkString(" ")

  private def sameBoardWithoutTurn(left: String, right: String): Boolean =
    def state(fen: String): List[String] =
      normalizeFen(fen).split("\\s+").toList.zipWithIndex.collect {
        case (field, index) if index == 0 || index == 2 || index == 3 => field
      }
    state(left) == state(right)

final case class LineEndgameTechniqueHorizon(
    pattern: String,
    rookPattern: Option[String],
    techniqueSide: Color,
    entryPlyOffset: Int,
    terminalPlyOffset: Int,
    status: LineEndgameTechniqueHorizonStatus,
    triggerMove: Option[String] = None,
    requiredSquares: List[String] = Nil,
    maintainedSquares: List[String] = Nil,
    brokenSquares: List[String] = Nil,
    terminalConsequenceKinds: List[LineConsequenceKind] = Nil,
    failureReason: Option[String] = None,
    zugzwangProof: Option[EndgameZugzwangProof] = None
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
          LineConsequenceKind.Promotion | LineConsequenceKind.PromotionRace =>
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
  case Promotion
  case PromotionRace

object LineConsequenceKind:
  def terminalResultProof(kind: LineConsequenceKind): Boolean =
    kind match
      case LineConsequenceKind.Mate | LineConsequenceKind.MaterialGain | LineConsequenceKind.MaterialLoss |
          LineConsequenceKind.Promotion | LineConsequenceKind.PromotionRace | LineConsequenceKind.DrawResource =>
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
          LineConsequenceKind.DrawResource | LineConsequenceKind.Promotion | LineConsequenceKind.PromotionRace =>
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
    eventMove: Option[String] = None,
    rootMove: Option[String] = None,
    rootSide: Option[Color] = None,
    beneficiary: Option[Color] = None,
    stationarySacrificeCaptures: List[LineMaterialCapture] = Nil
):
  def rootMoveMatched(rootMove: String): Boolean =
    this.rootMove.exists(move => EvidenceRef.sameMove(move, rootMove))

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
    materialWindowComplete && netCaptureCpForMover > 0 && (nonPawnCapturesByMover.nonEmpty || hasPromotionGainForMover)

  def hasProofSignalMaterialLoss: Boolean =
    materialWindowComplete && netCaptureCpForMover < 0 && (nonPawnCapturesByOpponent.nonEmpty || hasPromotionLossForMover)

  def hasUnrecoveredPawnGainForMover: Boolean =
    materialWindowComplete && netCaptureCpForMover > 0 && pawnCapturesByMover.nonEmpty

  def hasUnrecoveredPawnLossForMover: Boolean =
    materialWindowComplete && netCaptureCpForMover < 0 && pawnCapturesByOpponent.nonEmpty

  def hasProofSignalMaterialEvent: Boolean =
    hasProofSignalMaterialGain ||
      hasProofSignalMaterialLoss ||
      hasUnrecoveredPawnGainForMover ||
      hasUnrecoveredPawnLossForMover ||
      hasResolvedMaterialSequence

  def hasSacrificeMaterialEvent: Boolean =
    materialWindowComplete &&
      capturesByOpponent.exists(capture => !capture.recapture) &&
      netCaptureCpForMover < 0

  def captureForMove(moveUci: String): Option[LineMaterialCapture] =
    captures.find(capture => EvidenceRef.sameMove(capture.moveUci, moveUci))

  def sacrificeResponseFor(capture: LineMaterialCapture): Option[LineMaterialCapture] =
    captures.find(response => LineMaterialSummary.materialSacrificePair(capture, response))

  def lastPieceExchangePairAt(
      exchangePlyOffset: Int,
      recapturePlyOffset: Int
  ): Option[(LineMaterialCapture, LineMaterialCapture)] =
    for
      exchange <- captures.find(_.plyOffset == exchangePlyOffset)
      recapture <- captures.find(_.plyOffset == recapturePlyOffset)
      if recapturePlyOffset == exchangePlyOffset + 1
      if exchange.side == sideToMove
      if exchange.attackerRole == exchange.capturedRole
      if !Set("pawn", "king")(exchange.attackerRole.name.trim.toLowerCase)
      if recapture.recapture
      if recapture.side != exchange.side
      if recapture.square == exchange.square
      if recapture.capturedRole == exchange.attackerRole
    yield exchange -> recapture

  def hasCaptureSacrifice(moveUci: String): Boolean =
    captureForMove(moveUci).exists(capture => sacrificeResponseFor(capture).nonEmpty)

  def hasSacrificeMaterialEventFor(rootMove: Option[String]): Boolean =
    hasSacrificeMaterialEvent ||
      rootMove.exists(hasCaptureSacrifice)

  private def proofSignalCapturedRole(role: EvidencePieceRole): Boolean =
    val normalized = role.name.trim.toLowerCase
    normalized.nonEmpty && normalized != "pawn" && normalized != "king"

  private def pawnCapturedRole(role: EvidencePieceRole): Boolean =
    role.name.trim.equalsIgnoreCase("pawn")

object LineMaterialSummary:
  private[chessjudgment] def materialSacrificePair(
      offer: LineMaterialCapture,
      response: LineMaterialCapture
  ): Boolean =
    !offer.recapture &&
      response.recapture &&
      response.plyOffset > offer.plyOffset &&
      response.square == offer.square &&
      response.side != offer.side &&
      response.capturedRole == offer.attackerRole &&
      materialValue(offer.attackerRole) > materialValue(offer.capturedRole)

  private def materialValue(role: EvidencePieceRole): Int =
    role.name.toLowerCase match
      case "queen"             => 9
      case "rook"              => 5
      case "bishop" | "knight" => 3
      case "pawn"              => 1
      case "king"              => 100
      case _                    => 0

final case class LineFactEvidence private[chessjudgment] (
    line: LineNodeRef,
    private val forcedTheme: Option[ForcedLineThemeEvidence] = None,
    private val material: Option[LineMaterialSummary] = None,
    private val replay: List[LineReplayStep] = Nil,
    private val events: List[LineMoveEvent] = Nil,
    private val consequences: List[LineConsequence] = Nil,
    private val endgameHorizons: List[LineEndgameTechniqueHorizon] = Nil
) extends EvidencePayload:
  def rootMove: Option[String] =
    replay.headOption.map(_.moveUci)
  def reply: Option[String] =
    replay.lift(1).map(_.moveUci)
  def continuation: List[String] =
    replay.drop(2).map(_.moveUci)
  def forcedThemeId: Option[String] =
    forcedTheme.map(_.id)
  def lineReplaySteps: List[LineReplayStep] =
    replay
  def lineReplayMoves: List[String] =
    replay.map(_.moveUci)
  def lineReplayContinuationMoves: List[String] =
    lineReplayMoves.drop(1)
  def rootPreparesFuturePawnAdvance(rootMoveUci: String): Boolean =
    replay.headOption
      .filter(step => EvidenceRef.sameMove(step.moveUci, rootMoveUci))
      .exists { rootStep =>
        val continuation = replay.drop(1)
        continuation.zipWithIndex.exists { case (futureStep, index) =>
          PawnAdvanceSupportTrajectory.find(rootStep, futureStep, continuation.take(index)).nonEmpty
        }
      }
  private def rootActorSurvivesThrough(steps: List[LineReplayStep]): Option[Boolean] =
    steps.headOption.flatMap { rootStep =>
      val rootMoveUci = EvidenceRef.normalizeMove(rootStep.moveUci)
      for
        rootFrom <- Square.fromKey(rootMoveUci.take(2))
        rootTo <- Square.fromKey(rootMoveUci.slice(2, 4))
        before <- _root_.chess.format.Fen.read(
          _root_.chess.variant.Standard,
          _root_.chess.format.Fen.Full(rootStep.fenBefore)
        )
        rootPiece <- before.board.pieceAt(rootFrom)
      yield
        val afterRootOwnsDestination =
          _root_.chess.format.Fen
            .read(_root_.chess.variant.Standard, _root_.chess.format.Fen.Full(rootStep.fenAfter))
            .flatMap(_.board.pieceAt(rootTo))
            .exists(_.color == rootPiece.color)
        val finalSquare =
          if !afterRootOwnsDestination then None
          else
            steps.drop(1).foldLeft(Option(rootTo)) { (currentSquare, step) =>
              currentSquare.flatMap { current =>
                val move = EvidenceRef.normalizeMove(step.moveUci)
                val nextSquare =
                  if Square.fromKey(move.take(2)).contains(current) then Square.fromKey(move.slice(2, 4))
                  else Some(current)
                nextSquare.filter { square =>
                  _root_.chess.format.Fen
                    .read(_root_.chess.variant.Standard, _root_.chess.format.Fen.Full(step.fenAfter))
                    .flatMap(_.board.pieceAt(square))
                    .exists(_.color == rootPiece.color)
                }
              }
            }
        finalSquare.nonEmpty
    }
  def rootActorSurvivesReply: Option[Boolean] =
    rootActorSurvivesThrough(replay.take(2))
  def rootActorSurvivesLine: Option[Boolean] =
    rootActorSurvivesThrough(replay)
  def materialCaptures: List[LineMaterialCapture] =
    material.toList.flatMap(_.captures)
  def rootMaterialCapture(rootMoveUci: String): Option[LineMaterialCapture] =
    val normalizedRoot = normalizeUci(rootMoveUci)
    materialCaptures.find(capture =>
      capture.plyOffset == 0 && normalizeUci(capture.moveUci) == normalizedRoot
    )
  def rootIsRecapture(rootMoveUci: String): Boolean =
    rootMaterialCapture(rootMoveUci).exists(_.recapture)
  def rootIsCaptureSacrifice(rootMoveUci: String): Boolean =
    rootMaterialCapture(rootMoveUci).exists(materialSacrificeCapture)
  def rootCaptureSacrificeResponse(rootMoveUci: String): Option[LineMaterialCapture] =
    for
      summary <- material
      capture <- rootMaterialCapture(rootMoveUci)
      response <- summary.sacrificeResponseFor(capture)
    yield response
  def materialGainCapturesByMover: List[LineMaterialCapture] =
    material.toList.filter(_.netCaptureCpForMover > 0).flatMap { summary =>
      val pawnGains =
        if summary.hasUnrecoveredPawnGainForMover then summary.pawnCapturesByMover.filterNot(_.recapture) else Nil
      (summary.nonPawnCapturesByMover.filterNot(_.recapture) ++ pawnGains).distinct
    }
  def materialGainCapturesFor(side: Color): List[LineMaterialCapture] =
    val lastingGainMoves = consequences.collect {
      case consequence
          if Set(LineConsequenceKind.MaterialGain, LineConsequenceKind.MaterialLoss)(consequence.kind) &&
            consequence.beneficiary.contains(side) =>
        consequence.eventMove
    }.flatten
    materialCaptures.filter(capture =>
      capture.side == side && lastingGainMoves.exists(EvidenceRef.sameMove(_, capture.moveUci))
    ).distinct
  def materialSacrificeCapture(capture: LineMaterialCapture): Boolean =
    material.exists(_.sacrificeResponseFor(capture).nonEmpty)
  def lineEvents: List[LineMoveEvent] =
    events
  def lineConsequences: List[LineConsequence] =
    consequences
  def endgameTechniqueHorizons: List[LineEndgameTechniqueHorizon] =
    endgameHorizons
  def withEndgameZugzwangProof(
      entryPlyOffset: Int,
      terminalPlyOffset: Int,
      proof: EndgameZugzwangProof
  ): LineFactEvidence =
    copy(
      endgameHorizons = endgameHorizons.map { horizon =>
        if
          horizon.pattern == "Triangulation" &&
            horizon.entryPlyOffset == entryPlyOffset &&
            horizon.terminalPlyOffset == terminalPlyOffset
        then horizon.copy(zugzwangProof = Some(proof))
        else horizon
      }
    )
  def terminalEndgameTechniqueOverrideKinds: List[LineConsequenceKind] =
    proofSignalConsequenceKinds.filter(LineEndgameTechniqueHorizon.terminalProofOverrides)
  def hasTerminalEndgameTechniqueOverride: Boolean =
    terminalEndgameTechniqueOverrideKinds.nonEmpty
  def maintainedWinningEndgameTechniqueHorizons: List[LineEndgameTechniqueHorizon] =
    endgameHorizons.filter(horizon =>
      LineEndgameTechniqueHorizon.winningPattern(horizon.pattern) &&
        LineEndgameTechniqueHorizon.maintained(horizon.status)
    )
  def failedWinningEndgameTechniqueHorizons: List[LineEndgameTechniqueHorizon] =
    endgameHorizons.filter(horizon =>
      LineEndgameTechniqueHorizon.winningPattern(horizon.pattern) &&
        horizon.status == LineEndgameTechniqueHorizonStatus.Failed
    )
  def maintainedDefensiveEndgameTechniqueHorizons: List[LineEndgameTechniqueHorizon] =
    endgameHorizons.filter(horizon =>
      LineEndgameTechniqueHorizon.defensivePattern(horizon.pattern) &&
        LineEndgameTechniqueHorizon.maintained(horizon.status)
    )
  def endgameTechniquesTriggeredByRootMove(rootMoveUci: String, kind: RelativeCauseKind): List[LineEndgameTechniqueHorizon] =
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
      horizon.triggerMove.exists(move => normalizeUci(move) == normalizedRoot)
    )
  def lineReplayCount: Int =
    replay.size
  def futureRootObjectMove(maxPlyOffset: Int = Int.MaxValue): Option[LineObjectTrajectory] =
    if maxPlyOffset >= replay.size then completeFutureRootObjectMove
    else findFutureRootObjectMove(maxPlyOffset)

  private lazy val completeFutureRootObjectMove: Option[LineObjectTrajectory] =
    findFutureRootObjectMove(replay.size)

  private def findFutureRootObjectMove(maxPlyOffset: Int): Option[LineObjectTrajectory] =
    for
      rootStep <- replay.headOption
      if rootMove.exists(EvidenceRef.sameMove(_, rootStep.moveUci))
      trajectory <- LineObjectTrajectory.find(rootStep, replay.drop(1), maxPlyOffset)
    yield trajectory
  def hasLineReplay: Boolean =
    replay.nonEmpty
  def lineEventsOf(kind: LineEventKind): List[LineMoveEvent] =
    events.filter(_.kind == kind)
  def hasLineEvent(kind: LineEventKind): Boolean =
    lineEventsOf(kind).nonEmpty
  def eventsForRootMove(rootMoveUci: String): List[LineMoveEvent] =
    val normalizedRoot = normalizeUci(rootMoveUci)
    events.filter(event =>
      event.plyOffset == 0 && normalizeUci(event.moveUci) == normalizedRoot
    )
  def hasRootCaptureEvent(rootMoveUci: String): Boolean =
    eventsForRootMove(rootMoveUci).exists(event =>
      event.kind == LineEventKind.Capture || event.kind == LineEventKind.Recapture
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
    material.exists(_.hasSacrificeMaterialEventFor(rootMove))
  def proofSignalConsequences: List[LineConsequence] =
    consequences.filter(_.proofSignal)
  def proofSignalConsequencesOf(kind: LineConsequenceKind): List[LineConsequence] =
    proofSignalConsequences.filter(_.kind == kind)
  def consequencesForRootMove(rootMoveUci: String): List[LineConsequence] =
    proofSignalConsequences.filter(_.rootMoveMatched(rootMoveUci))
  def sacrificeSquaresForRootMove(rootMoveUci: String): List[EvidenceSquare] =
    consequencesForRootMove(rootMoveUci)
      .filter(_.kind == LineConsequenceKind.Sacrifice)
      .flatMap(consequence =>
        consequence.stationarySacrificeCaptures.map(_.square) ++
          consequence.eventMove
            .map(EvidenceRef.normalizeMove)
            .flatMap(move => Option.when(move.length >= 4)(EvidenceSquare(move.slice(2, 4))))
            .toList
      )
      .distinct
  def stationarySacrificeCapturesForRootMove(rootMoveUci: String): List[LineMaterialCapture] =
    consequencesForRootMove(rootMoveUci)
      .filter(_.kind == LineConsequenceKind.Sacrifice)
      .flatMap(_.stationarySacrificeCaptures)
      .distinct
  def principalStationarySacrificeSequenceForRootMove(rootMoveUci: String): List[LineMaterialCapture] =
    stationarySacrificeCapturesForRootMove(rootMoveUci).sortBy(_.plyOffset) match
      case first :: remaining =>
        remaining.foldLeft(List(first) -> true) { case ((sequence, collecting), capture) =>
          val previous = sequence.last
          val collectorSquareUntouched = lineReplaySteps
            .slice(previous.plyOffset + 1, capture.plyOffset)
            .forall { step =>
              val move = EvidenceRef.normalizeMove(step.moveUci)
              move.take(2) != previous.square.key.toLowerCase &&
                move.slice(2, 4) != previous.square.key.toLowerCase
            }
          val sameCollector =
            capture.attackerRole == previous.attackerRole &&
              EvidenceRef.normalizeMove(capture.moveUci).take(2) == previous.square.key.toLowerCase &&
              collectorSquareUntouched
          if collecting && sameCollector then (sequence :+ capture) -> true
          else sequence -> false
        }._1
      case Nil => Nil
  def principalSacrificeCostSequenceForRootMove(rootMoveUci: String): List[LineMaterialCapture] =
    rootCaptureSacrificeResponse(rootMoveUci).toList match
      case Nil       => principalStationarySacrificeSequenceForRootMove(rootMoveUci)
      case responses => responses
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
            LineConsequenceKind.Sacrifice | LineConsequenceKind.Promotion | LineConsequenceKind.PromotionRace =>
          true
        case _ =>
          false
      },
      hasRecaptureRecovery = kinds.exists(kind =>
        kind == LineConsequenceKind.RecaptureSequence || kind == LineConsequenceKind.RecoveryWindow
      ),
      hasSacrifice = kinds.contains(LineConsequenceKind.Sacrifice),
      hasPromotionRace = kinds.exists(kind =>
        kind == LineConsequenceKind.Promotion || kind == LineConsequenceKind.PromotionRace
      ),
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
          (
            List(
              "EndgameTechniqueHorizon",
              s"pattern:${horizon.pattern}",
              s"horizonStatus:${horizon.status}",
              s"technique-side:${horizon.techniqueSideKey}"
            ) ++ horizon.rookPattern.map(pattern => s"rook-pattern:$pattern")
          )*
        )
      )
  def rootCaptureIsRecaptured(rootMoveUci: String): Boolean =
    materialCaptures.exists(rootCapture =>
      rootCapture.plyOffset == 0 &&
        EvidenceRef.sameMove(rootCapture.moveUci, rootMoveUci) &&
        materialCaptures.exists(reply =>
          reply.recapture &&
            reply.plyOffset > rootCapture.plyOffset &&
            reply.side != rootCapture.side &&
            reply.square == rootCapture.square
        )
    )

  private def normalizeUci(raw: String): String =
    Option(raw).getOrElse("").trim.toLowerCase

  def materialOutcomeProfile: LineMaterialOutcomeProfile =
    val consequenceGainSignals =
      consequences.collect {
        case LineConsequence(LineConsequenceKind.MaterialGain, _, true, _, _, _, _, _) =>
          LineMaterialOutcomeSignal.MoverCapture
        case LineConsequence(LineConsequenceKind.MaterialGain, _, false, _, _, _, _, _) =>
          LineMaterialOutcomeSignal.UnrecoveredPawnGain
        case LineConsequence(LineConsequenceKind.RecoveryWindow, _, true, _, _, _, _, _) =>
          LineMaterialOutcomeSignal.RecoveryWindow
      }.toSet
    val consequenceLossSignals =
      consequences.collect {
        case LineConsequence(LineConsequenceKind.MaterialLoss, _, true, _, _, _, _, _) =>
          LineMaterialOutcomeSignal.OpponentCapture
        case LineConsequence(LineConsequenceKind.MaterialLoss, _, false, _, _, _, _, _) =>
          LineMaterialOutcomeSignal.UnrecoveredPawnLoss
      }.toSet
    val materialGainSignals =
      material
        .map(summary =>
          Set(
            Option.when(summary.netCaptureCpForMover > 0 && summary.nonPawnCapturesByMover.nonEmpty)(LineMaterialOutcomeSignal.MoverCapture),
            Option.when(summary.netCaptureCpForMover > 0 && summary.hasPromotionGainForMover)(LineMaterialOutcomeSignal.PromotionGain),
            Option.when(summary.hasUnrecoveredPawnGainForMover)(LineMaterialOutcomeSignal.UnrecoveredPawnGain),
            Option.when(summary.hasRecoveryWindow)(LineMaterialOutcomeSignal.RecoveryWindow)
          ).flatten
        )
        .getOrElse(Set.empty)
    val materialLossSignals =
      material
        .map(summary =>
          Set(
            Option.when(summary.netCaptureCpForMover < 0 && summary.nonPawnCapturesByOpponent.nonEmpty)(LineMaterialOutcomeSignal.OpponentCapture),
            Option.when(summary.netCaptureCpForMover < 0 && summary.hasPromotionLossForMover)(LineMaterialOutcomeSignal.PromotionLoss),
            Option.when(summary.hasUnrecoveredPawnLossForMover)(LineMaterialOutcomeSignal.UnrecoveredPawnLoss)
          ).flatten
        )
        .getOrElse(Set.empty)
    LineMaterialOutcomeProfile(
      gainSignals = consequenceGainSignals ++ materialGainSignals,
      lossSignals = consequenceLossSignals ++ materialLossSignals
    )

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

final case class EvalFactEvidence(
    line: LineNodeRef,
    whitePovEvalCp: Int,
    mate: Option[Int],
    depth: Int
) extends EvidencePayload:
  def whiteWinPercent: Double =
    PerspectiveMath.winPercentFromWhiteEval(whitePovEvalCp, mate)
  def winPercentFor(mover: Color): Double =
    PerspectiveMath.winPercentForMover(mover, whitePovEvalCp, mate)
  def winPercentAdvantageFor(mover: Color): Double =
    PerspectiveMath.winPercentAdvantageFor(mover, whitePovEvalCp, mate)

final case class MoveMotifGeometry private[judgment] (
    subjectSquares: List[EvidenceSquare] = Nil,
    targetSquares: List[EvidenceSquare] = Nil,
    relatedSquares: List[EvidenceSquare] = Nil,
    relatedFiles: List[EvidenceFile] = Nil,
    roles: List[EvidencePieceRole] = Nil
):
  def focusSquares: List[EvidenceSquare] =
    (subjectSquares ++ targetSquares ++ relatedSquares).distinct

final case class MoveMotifEvent private[judgment] (
    rootMove: String,
    motif: Motif
):
  def eventMove: Option[String] =
    motif.move.map(EvidenceRef.normalizeMove)
  def plyOffset: Int =
    motif.plyIndex
  def geometry: MoveMotifGeometry =
    MoveMotifEvent.geometryFor(motif)
  def isRootEvent: Boolean =
    eventMove.exists(move => EvidenceRef.sameMove(move, rootMove)) ||
      (eventMove.isEmpty && plyOffset == 0)
  def kind: String =
    motif.getClass.getSimpleName.stripSuffix("$")
  def category: MotifCategory =
    motif.category

object MoveMotifEvent:
  def fromMotif(
      rootMove: String,
      motif: Motif
  ): MoveMotifEvent =
    MoveMotifEvent(
      rootMove = rootMove,
      motif = motif
    )

  private def geometryFor(motif: Motif): MoveMotifGeometry =
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
        case Motif.Maneuver(piece, _, _, _, move) =>
          val normalized = move.map(_.trim.toLowerCase).getOrElse("")
          val from = Square.fromKey(normalized.take(2)).map(evidenceSquare).toList
          val to = Square.fromKey(normalized.slice(2, 4)).map(evidenceSquare).toList
          (from, to, Nil, List(piece))
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
        case Motif.Battery(front, back, _, _, _, _, frontSq, backSq, targetSq, _) =>
          (
            (frontSq ++ backSq).map(evidenceSquare).toList,
            targetSq.map(evidenceSquare).toList,
            Nil,
            List(front, back).distinct
          )
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
    MoveMotifGeometry(
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
  def moveUci: String = event.rootMove
  def rootMove: String = event.rootMove
  def motif: Motif = event.motif
  def geometry: MoveMotifGeometry = event.geometry
  def kind: String = event.kind
  def category: MotifCategory = event.category
  def eventMove: Option[String] = event.eventMove
  def plyOffset: Int = event.plyOffset
  def isRootEvent: Boolean = event.isRootEvent
  def recordLineBound(ref: EvidenceRef): Boolean =
    isRootEvent &&
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
) extends EvidencePayload

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
  case SpaceChanged
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
  case SpaceGain
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
  case PieceExchangeAvailable
  case PieceExchangeCompleted
  case OpponentMobilityRestriction
  case KingRingPressureGain
  case KingRingPressureConcession

object TransitionConsequenceKind:
  private val RootActorBound = Set(
    MobilityGain,
    FileOccupationGain,
    OutpostGain,
    RookLiftActivation,
    BatteryPressureGain,
    DevelopmentLagReduced,
    DevelopmentPieceActivated,
    DevelopmentMobilityGain,
    DevelopmentCenterControlGain,
    DevelopmentSafePlacement,
    PassedPawnProgress,
    PromotionPressureGain
  )

  private val ConcreteGoalResults = Set(
    OpenFileGain,
    SemiOpenFileGain,
    FileOccupationGain,
    WeakPawnTargetCreated,
    WeakSquareTargetCreated,
    PawnTensionGain,
    PawnTensionResolution,
    SpaceGain,
    PassedPawnProgress,
    PromotionPressureGain,
    OutpostGain,
    PieceExchangeAvailable,
    PieceExchangeCompleted
  )

  def requiresRootActorSurvival(kind: TransitionConsequenceKind): Boolean =
    RootActorBound(kind)

  private[judgment] def isConcreteGoalResult(kind: TransitionConsequenceKind): Boolean =
    ConcreteGoalResults(kind)

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
    subjects: List[String] = Nil,
    targetSubjects: List[String] = Nil
):
  def positive: Boolean =
    polarity == StructuralSignalPolarity.Gain
  def negative: Boolean =
    polarity == StructuralSignalPolarity.Loss
  def anchorKey: String =
    s"$kind:$polarity"
  def goalSubjects: List[String] =
    if targetSubjects.nonEmpty then targetSubjects else subjects
  def witnessSubjects: List[String] =
    if targetSubjects.isEmpty then Nil else subjects.filterNot(targetSubjects.toSet)
  def subjectsAt(square: String): List[String] =
    subjects.filter(subject =>
      StructuralPurposeSubject.weakPawnSquare(subject).exists(_.equalsIgnoreCase(square)) ||
        StructuralPurposeSubject.carrierToken(subject).equalsIgnoreCase(square)
    )
  def subjectsForPieceAt(pieceRole: String, square: String): List[String] =
    subjects.filter(subject =>
      StructuralPurposeSubject.parse(subject).contains(
        StructuralPurposeSubject.PieceSquare(pieceRole.toLowerCase, square.toLowerCase)
      )
    )

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

final class RelationFactEvidence private (
    val detail: RelationWitnessDetail,
    val lineMoves: List[String]
) extends EvidencePayload:
  def kind: RelationFactKind =
    RelationWitnessDetail
      .factKind(detail)
      .getOrElse(throw IllegalStateException("relation evidence requires a canonical typed detail"))
  def focusSquares: List[EvidenceSquare] =
    RelationWitnessDetail.focusSquares(detail)
  def targetSquare: Option[EvidenceSquare] =
    RelationWitnessDetail.targetSquare(detail)
  def participants: List[RelationParticipant] =
    RelationWitnessDetail.participants(detail)
  def hasTypedWitness: Boolean =
    RelationWitnessDetail.factKind(detail).nonEmpty
  def proofAtoms: List[RelationProofAtom] =
    RelationWitnessDetail.proofAtoms(detail, lineMoves)
  def hasLineProof: Boolean =
    lineMoves.nonEmpty
  def lineProofCount: Int =
    lineMoves.size
  def hasParticipantProof: Boolean =
    participants.nonEmpty
  def hasConcreteRelationProof: Boolean =
    hasTypedWitness && proofAtoms.nonEmpty
  def mentionsLineMove(moveUci: String): Boolean =
    lineMoves.exists(EvidenceRef.sameMove(_, moveUci))
  override def equals(other: Any): Boolean =
    other match
      case that: RelationFactEvidence =>
        detail == that.detail && lineMoves == that.lineMoves
      case _ =>
        false
  override def hashCode: Int =
    31 * detail.hashCode + lineMoves.hashCode

object RelationFactEvidence:
  def from(
      detail: RelationWitnessDetail,
      lineMoves: List[String]
  ): Option[RelationFactEvidence] =
    RelationWitnessDetail
      .factKind(detail)
      .map(_ => new RelationFactEvidence(detail = detail, lineMoves = lineMoves))

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
            List(TacticalMechanismKind.Conversion)
          case Motif.CaptureType.Winning =>
            List(TacticalMechanismKind.MaterialGain)
          case Motif.CaptureType.Normal | Motif.CaptureType.Sacrifice =>
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
      case RelationFactKind.StalemateTrap =>
        TacticalMechanismKind.DrawResource
      case RelationFactKind.PerpetualCheck =>
        TacticalMechanismKind.RelationMechanism
      case _ =>
        TacticalMechanismKind.RelationMechanism

  def fromLineConsequence(kind: LineConsequenceKind, rootRecapture: Boolean): List[TacticalMechanismKind] =
    kind match
      case LineConsequenceKind.MaterialGain =>
        List(TacticalMechanismKind.MaterialGain)
      case LineConsequenceKind.MaterialLoss =>
        Nil
      case LineConsequenceKind.RecaptureSequence | LineConsequenceKind.RecoveryWindow =>
        Option.when(rootRecapture)(TacticalMechanismKind.RecaptureChoice).toList
      case LineConsequenceKind.ImmediateReplyCheck =>
        List(TacticalMechanismKind.Tempo)
      case LineConsequenceKind.Mate =>
        List(TacticalMechanismKind.KingForcing)
      case LineConsequenceKind.DrawResource =>
        List(TacticalMechanismKind.DrawResource)
      case LineConsequenceKind.Promotion | LineConsequenceKind.PromotionRace =>
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
  case LineEvent
  case MateBranch
  case ThreatEpisode

final case class TacticalMechanismSignal(
    kind: TacticalMechanismSignalKind,
    label: String,
    sourceLayer: EvidenceLayer,
    source: Option[EvidenceRef] = None,
    relationKind: Option[RelationFactKind] = None
)

final case class TacticalMechanismEvidence(
    kind: TacticalMechanismKind,
    moveUci: Option[String],
    line: Option[LineNodeRef],
    signals: List[TacticalMechanismSignal]
) extends EvidencePayload:
  def signalKinds: Set[TacticalMechanismSignalKind] =
    signals.map(_.kind).toSet
  def hasLineProof: Boolean =
    signalKinds.exists(kind =>
      kind == TacticalMechanismSignalKind.LineConsequence ||
        kind == TacticalMechanismSignalKind.LineEvent ||
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
        signals.exists(signal => signal.kind == TacticalMechanismSignalKind.Relation && signal.relationKind.contains(RelationFactKind.Zwischenzug)))
  def tactical: Boolean =
    kind != TacticalMechanismKind.DefensiveResource &&
      kind != TacticalMechanismKind.DrawResource
  def defensive: Boolean =
    kind == TacticalMechanismKind.DefensiveResource ||
      kind == TacticalMechanismKind.DrawResource
  def canAnchorTacticalClaim: Boolean =
    tactical && hasConcreteProof && hasEngineOrForcingProof
  def canAnchorDefensiveClaim: Boolean =
    defensive && (hasThreatProof || hasLineProof)

final case class StructuralDeltaEvidence(
    transition: StructuralTransitionBinding,
    signals: List[StructuralSignal],
    consequences: List[TransitionConsequence],
    developmentChoices: List[StructuralDevelopmentChoice] = Nil
) extends EvidencePayload:
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
  def hasTargetPressureGain: Boolean =
    hasConsequence(TargetPressureGain)
  def hasTargetPressureRelease: Boolean =
    hasConsequence(TargetPressureRelease)
  def hasCenterControlGain: Boolean =
    hasConsequence(CenterControlGain)
  def hasPieceActivityGain: Boolean =
    hasConsequenceCategory(TransitionConsequenceCategory.PieceActivity)
  def hasKingSafetyPressure: Boolean =
    hasConsequence(KingSafetyPressure)
  def hasPassedPawnProgress: Boolean =
    hasConsequence(PassedPawnProgress)
  def hasOutpostGain: Boolean =
    hasConsequence(OutpostGain)
  def hasBatteryPressureGain: Boolean =
    hasConsequence(BatteryPressureGain)
  def hasOpponentMobilityRestriction: Boolean =
    consequencesOf(OpponentMobilityRestriction).exists(consequence =>
      consequence.subjects.exists(StructuralDeltaEvidence.validOpponentMobilityRestrictionSubject)
    )
  def hasReplyIndependentOpponentMobilityRestriction: Boolean =
    consequencesOf(OpponentMobilityRestriction).exists(consequence =>
      consequence.subjects.exists(subject =>
        StructuralDeltaEvidence.validOpponentMobilityRestrictionSubject(subject) &&
          !StructuralDeltaEvidence.directlyBlockedPawnAdvance(subject)
      )
    )
  def hasStrategicConcession: Boolean =
    strategicConcessions.nonEmpty
  def strategicConcessions: List[TransitionConsequence] =
    negativeConsequences.filter(consequence =>
      StructuralDeltaEvidence.hasConsequenceCategory(consequence.kind, TransitionConsequenceCategory.StrategicSupport)
    )
  def hasStructuralAnchor: Boolean =
    hasConsequenceCategory(TransitionConsequenceCategory.StructuralAnchor)
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
      case colorComplexSafeSubject(_) =>
        true
      case restrictedPawnAdvanceSubject(_, _) =>
        true
      case _ =>
        StructuralPurposeSubject.restrictedEntry(normalized).nonEmpty

  private[chessjudgment] def restrictedOpponentEntry(subject: String): Option[(String, String, String)] =
    StructuralPurposeSubject.restrictedEntry(subject).orElse:
      Option(subject).getOrElse("").trim.toLowerCase match
        case restrictedPawnAdvanceSubject(from, to) => Some(("pawn", from, to))
        case _                                       => None

  private[chessjudgment] def restrictedOpponentRoute(subject: String): Option[List[String]] =
    StructuralPurposeSubject.restrictedEntryRoute(subject)

  private[chessjudgment] def restrictedOpponentRouteNeedsLine(subject: String): Boolean =
    StructuralPurposeSubject.restrictedEntryRouteNeedsLine(subject)

  private[chessjudgment] def directlyRestrictedOpponentSubjects(
      consequence: TransitionConsequence
  ): List[String] =
    consequence.subjects.filter(subject =>
      validOpponentMobilityRestrictionSubject(subject) &&
        restrictedOpponentRoute(subject).isEmpty
    )

  private[chessjudgment] def movedPieceRoute(subject: String): Option[(String, String, String)] =
    StructuralPurposeSubject.parse(subject).collect {
      case StructuralPurposeSubject.PieceRoute(piece, from, to) => (piece, from, to)
    }

  private[chessjudgment] def directlyBlockedPawnAdvance(subject: String): Boolean =
    Option(subject).getOrElse("").trim.toLowerCase match
      case restrictedPawnAdvanceSubject(_, _) => true
      case _                                   => false

  private def fianchettoBishopSquare(square: String): Boolean =
    Set("g7", "b7", "g2", "b2").contains(square.toLowerCase)

  private def centralDiagonalBlockerSquare(square: String): Boolean =
    square.toLowerCase.matches("[c-f][45]")

  private val opponentMobilityRestrictionSubject =
    raw"bishop:([a-h][1-8]):diagonal-denial:blocked-by:([a-h][1-8]):locked-center:mobility-([0-9]+)-to-([0-9]+)".r
  private val colorComplexSafeSubject =
    raw"(?:bishop|pawn):([a-h][1-8](?:-[a-h][1-8])?):color-complex-safe".r
  private val restrictedPawnAdvanceSubject =
    raw"pawn:([a-h][1-8])-([a-h][1-8]):advance-restricted".r
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
      SpaceGain -> Set(PawnStructure, PawnStructureDelta, StructuralAnchor, StrategicMove, StrategicSupport, PlanAnchor),
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
      PieceExchangeAvailable -> Set(PieceActivity, StructuralAnchor, StrategicMove, StrategicSupport, PlanAnchor),
      PieceExchangeCompleted -> Set(PieceActivity, StructuralAnchor, StrategicMove, StrategicSupport, PlanAnchor),
      OpponentMobilityRestriction -> Set(StrategicMove, StrategicSupport, PlanAnchor),
      KingRingPressureGain -> Set(StructuralAnchor, StrategicMove, StrategicSupport),
      KingRingPressureConcession -> Set(StrategicSupport)
    )

final case class PlanTransitionEvidence(
    transition: PlanSequenceSummary
) extends EvidencePayload

enum PlanMoveRole:
  case Preparation
  case Execution
  case Prevention
  case Pivot

enum PlanCausalDependencyKind:
  case ObjectStatePrecondition
  case LineAccessPrecondition
  case PawnAdvanceSupport
  case RetreatControlPrecondition
  case ResponseContinuationPrecondition
  case SharedTargetCoordination
  case FlankAdvanceCoordination

enum PlanCausalDependencyProof:
  case ObjectState(trajectory: LineObjectTrajectory)
  case LineAccess(trajectory: LineAccessTrajectory)
  case PawnAdvanceSupport(trajectory: PawnAdvanceSupportTrajectory)
  case RetreatControl(trajectory: RetreatControlTrajectory)
  case ResponseContinuation(trajectory: PlanResponseContinuationTrajectory)
  case SharedTarget(targets: List[EvidenceSquare])
  case FlankAdvance(kingSquare: EvidenceSquare, targets: List[EvidenceSquare])

final case class PlanCausalEventNode(
    identity: PlanEventIdentity,
    step: LineReplayStep,
    perspective: Color,
    structuralConsequences: List[TransitionConsequence],
    developmentChoices: List[StructuralDevelopmentChoice]
):
  def moveUci: String = EvidenceRef.normalizeMove(step.moveUci)
  private val moveOrigin = moveUci.take(2)
  private val moveDestination = moveUci.slice(2, 4)
  require(
    moveUci.length >= 4 &&
      EvidenceRef.sameMove(identity.rootMove, moveUci) &&
      identity.actorFrom.contains(moveOrigin) &&
      identity.actorTo.contains(moveDestination),
    "plan-causal event identity must be derived from the event replay step"
  )

final case class PlanCausalEventDependency(
    from: PlanCausalEventNode,
    to: PlanCausalEventNode,
    kind: PlanCausalDependencyKind,
    proof: PlanCausalDependencyProof,
    plyOffset: Int
):
  def planConnectionProven: Boolean =
    from.step.ply < to.step.ply &&
      plyOffset == to.step.ply - from.step.ply &&
      ((kind, proof) match
        case (PlanCausalDependencyKind.ObjectStatePrecondition, PlanCausalDependencyProof.ObjectState(trajectory)) =>
          trajectory.rootStep == from.step &&
            trajectory.futureStep == to.step &&
            trajectory.plyOffset == plyOffset &&
            LineObjectTrajectory.provesObjectStatePrecondition(trajectory)
        case (PlanCausalDependencyKind.LineAccessPrecondition, PlanCausalDependencyProof.LineAccess(trajectory)) =>
          trajectory.enablingStep == from.step &&
            trajectory.enabledStep == to.step &&
            trajectory.plyOffset == plyOffset &&
            LineAccessTrajectory.proves(trajectory)
        case (PlanCausalDependencyKind.PawnAdvanceSupport, PlanCausalDependencyProof.PawnAdvanceSupport(trajectory)) =>
          trajectory.supportingStep == from.step &&
            trajectory.pawnAdvanceStep == to.step &&
            trajectory.plyOffset == plyOffset &&
            PawnAdvanceSupportTrajectory.proves(trajectory)
        case (PlanCausalDependencyKind.RetreatControlPrecondition, PlanCausalDependencyProof.RetreatControl(trajectory)) =>
          trajectory.supportingStep == from.step &&
            trajectory.pressuringStep == to.step &&
            trajectory.plyOffset == plyOffset &&
            RetreatControlTrajectory.proves(trajectory)
        case (
              PlanCausalDependencyKind.ResponseContinuationPrecondition,
              PlanCausalDependencyProof.ResponseContinuation(trajectory)
            ) =>
          trajectory.triggerStep == from.step &&
            trajectory.followUpStep == to.step &&
            trajectory.plyOffset == plyOffset &&
            PlanResponseContinuationTrajectory.proves(trajectory)
        case (PlanCausalDependencyKind.SharedTargetCoordination, PlanCausalDependencyProof.SharedTarget(targets)) =>
          val shared = targets.map(_.key.toLowerCase).filter(_.matches("[a-h][1-8]")).toSet
          shared.nonEmpty &&
            shared.subsetOf(PlanCausalEpisode.pressureTargetSquares(from)) &&
            shared.subsetOf(PlanCausalEpisode.pressureTargetSquares(to))
        case (PlanCausalDependencyKind.FlankAdvanceCoordination, PlanCausalDependencyProof.FlankAdvance(king, targets)) =>
          PlanCausalEpisode.flankAdvanceProof(from, to, king, targets)
        case _ =>
          false
      )
  def enablesContinuation: Boolean =
    planConnectionProven &&
      (kind == PlanCausalDependencyKind.ObjectStatePrecondition ||
        kind == PlanCausalDependencyKind.LineAccessPrecondition ||
        kind == PlanCausalDependencyKind.PawnAdvanceSupport ||
        kind == PlanCausalDependencyKind.RetreatControlPrecondition ||
        kind == PlanCausalDependencyKind.ResponseContinuationPrecondition)
  def coordinatedResponseKing(response: PlanCausalResponse): Option[EvidenceSquare] =
    (kind, proof) match
      case (
            PlanCausalDependencyKind.FlankAdvanceCoordination,
            PlanCausalDependencyProof.FlankAdvance(king, _)
          )
          if planConnectionProven &&
            response.trigger == to &&
            response.proven &&
            response.attacksPlanPiece &&
            response.weakenedKingSquare.exists(_.key.equalsIgnoreCase(king.key)) =>
        Some(king)
      case _ => None
  def proofSquares: List[EvidenceSquare] =
    proof match
      case PlanCausalDependencyProof.ObjectState(trajectory) =>
        List(trajectory.rootTo)
      case PlanCausalDependencyProof.LineAccess(trajectory) =>
        List(trajectory.vacatedSquare)
      case PlanCausalDependencyProof.PawnAdvanceSupport(trajectory) =>
        List(trajectory.supporterSquare, trajectory.pawnFrom, trajectory.pawnTo)
      case PlanCausalDependencyProof.RetreatControl(trajectory) =>
        List(trajectory.pressuredSquare, trajectory.controlledRetreatSquare)
      case PlanCausalDependencyProof.ResponseContinuation(trajectory) =>
        List(
          trajectory.replyFrom,
          trajectory.replyTo,
          trajectory.followUpFrom,
          trajectory.followUpTo
        ) ++ (trajectory match
          case pawn: PawnBreakFollowUpTrajectory => pawn.releasedPassedPawn.toList
          case exchange: ExchangeConversionTrajectory =>
            List(exchange.convertingPawnAtPhaseBoundary)
          case _ => Nil)
      case PlanCausalDependencyProof.SharedTarget(targets) =>
        targets
      case PlanCausalDependencyProof.FlankAdvance(king, targets) =>
        king :: targets
  def proofPieceRoles: List[EvidencePieceRole] =
    proof match
      case PlanCausalDependencyProof.ObjectState(trajectory) =>
        List(trajectory.pieceRole)
      case PlanCausalDependencyProof.LineAccess(trajectory) =>
        List(trajectory.enabledPieceRole)
      case PlanCausalDependencyProof.PawnAdvanceSupport(trajectory) =>
        List(trajectory.supporterRole, EvidencePieceRole(Pawn.toString))
      case PlanCausalDependencyProof.RetreatControl(trajectory) =>
        List(trajectory.supporterRole, trajectory.pressuredRole)
      case PlanCausalDependencyProof.ResponseContinuation(trajectory) =>
        trajectory.involvedRoles
      case _ =>
        Nil
  def preparedPawnAdvanceFile: Option[String] =
    val pawnAdvanceSquares = proof match
      case PlanCausalDependencyProof.LineAccess(trajectory)
          if trajectory.enabledPieceRole.name.equalsIgnoreCase(Pawn.toString) =>
        Some(trajectory.enabledFrom -> trajectory.enabledTo)
      case PlanCausalDependencyProof.PawnAdvanceSupport(trajectory) =>
        Some(trajectory.pawnFrom -> trajectory.pawnTo)
      case _ => None
    pawnAdvanceSquares.collect {
      case (from, to) if from.key.headOption == to.key.headOption => from.key.take(1)
    }

final case class PlanCausalResponse(
    trigger: PlanCausalEventNode,
    step: LineReplayStep,
    plyOffset: Int,
    structuralConsequences: List[TransitionConsequence] = Nil
):
  def capturesPlanPiece: Boolean =
    PlanCausalEpisode.responseCapturesPlanPiece(trigger, step)
  def attacksPlanPiece: Boolean =
    PlanCausalEpisode.responseAttacksPlanPiece(trigger, step)
  def weakenedKingSquare: Option[EvidenceSquare] =
    PlanCausalEpisode.responseWeakenedKingSquare(trigger, step)
  def weakensKingShelter: Boolean =
    weakenedKingSquare.nonEmpty
  def movesPressuredPiece: Boolean =
    PlanCausalEpisode.responseOriginSquares(trigger).contains(EvidenceRef.normalizeMove(step.moveUci).take(2))
  def answersCheck: Boolean =
    PlanCausalEpisode.responseAnswersCheck(trigger, step)
  def proven: Boolean =
    step.ply > trigger.step.ply &&
      plyOffset == step.ply - trigger.step.ply &&
      PlanCausalEpisode.opponentCanAnswerPlanPiece(trigger, step.fenBefore) &&
      (movesPressuredPiece || capturesPlanPiece || attacksPlanPiece || answersCheck)

final case class ObservedPlanCost(
    capture: LineMaterialCapture,
    responseStep: LineReplayStep,
    offerCapture: Option[LineMaterialCapture] = None
):
  def plyOffset: Int = capture.plyOffset
  def proven(root: PlanCausalEventNode): Boolean =
    val move = EvidenceRef.normalizeMove(responseStep.moveUci)
    val square = capture.square
    val rootCaptureSacrifice = offerCapture.exists(offer =>
      offer.plyOffset == 0 &&
        EvidenceRef.sameMove(offer.moveUci, root.moveUci) &&
        LineMaterialSummary.materialSacrificePair(offer, capture)
    )
    move.length >= 4 &&
      (!capture.recapture || rootCaptureSacrifice) &&
      EvidenceRef.sameMove(move, capture.moveUci) &&
      capture.plyOffset == responseStep.ply - root.step.ply &&
      capture.side == !root.perspective &&
      move.slice(2, 4) == square.key.toLowerCase &&
      (for
        boardSquare <- _root_.chess.Square.fromKey(square.key)
        before <- _root_.chess.format.Fen.read(
          _root_.chess.variant.Standard,
          _root_.chess.format.Fen.Full(responseStep.fenBefore)
        )
        after <- _root_.chess.format.Fen.read(
          _root_.chess.variant.Standard,
          _root_.chess.format.Fen.Full(responseStep.fenAfter)
        )
        captured <- before.board.pieceAt(boardSquare)
        collector <- after.board.pieceAt(boardSquare)
      yield
        captured.color == root.perspective &&
          capture.capturedRole.name.equalsIgnoreCase(captured.role.toString) &&
          collector.color == capture.side &&
          capture.attackerRole.name.equalsIgnoreCase(collector.role.toString)
      ).contains(true)

final case class PlanCausalEpisode(
    root: PlanCausalEventNode,
    continuations: List[PlanCausalEventNode],
    dependencies: List[PlanCausalEventDependency],
    responses: List[PlanCausalResponse],
    antecedents: List[PlanCausalEventNode] = Nil
):
  def withRootIdentity(identity: PlanEventIdentity): PlanCausalEpisode =
    if root.identity == identity then this
    else
      val previousRoot = root
      val resolvedRoot = root.copy(identity = identity)
      def resolve(event: PlanCausalEventNode): PlanCausalEventNode =
        if event == previousRoot then resolvedRoot else event
      copy(
        root = resolvedRoot,
        continuations = continuations.map(resolve),
        dependencies = dependencies.map(dependency =>
          dependency.copy(from = resolve(dependency.from), to = resolve(dependency.to))
        ),
        responses = responses.map(response => response.copy(trigger = resolve(response.trigger))),
        antecedents = antecedents.map(resolve)
      )
  lazy val antecedentSteps: List[PlanCausalEventNode] =
    antecedents.distinct.filter(_.step.ply < root.step.ply).sortBy(event => (event.step.ply, event.moveUci))
  lazy val planSteps: List[PlanCausalEventNode] =
    (root :: continuations).distinct.sortBy(event => (event.step.ply, event.moveUci))
  lazy val chronologicalSteps: List[PlanCausalEventNode] =
    (antecedentSteps ++ planSteps).distinct.sortBy(event => (event.step.ply, event.moveUci))
  def events: List[PlanCausalEventNode] = planSteps
  def spanPlies: Int =
    chronologicalSteps match
      case Nil => 1
      case steps => (steps.last.step.ply - steps.head.step.ply + 1).max(1)
  private lazy val futureDependencies: List[PlanCausalEventDependency] =
    dependencies.filter(dependency => planSteps.contains(dependency.from) && planSteps.contains(dependency.to))
  lazy val historyDependencies: List[PlanCausalEventDependency] =
    dependencies.filter(dependency =>
      chronologicalSteps.contains(dependency.from) &&
        chronologicalSteps.contains(dependency.to) &&
        dependency.from.step.ply < root.step.ply &&
        dependency.to.step.ply <= root.step.ply
    )
  def planSequenceProven: Boolean =
    continuations.nonEmpty &&
      futureDependencies.nonEmpty &&
      futureDependencies.forall(_.planConnectionProven) &&
      responses.forall(response => planSteps.contains(response.trigger) && response.proven) &&
      futureConnectedToRoot
  lazy val antecedentsEnablingRoot: List[PlanCausalEventNode] =
    @annotation.tailrec
    def expand(enabled: Set[PlanCausalEventNode]): Set[PlanCausalEventNode] =
      val next = enabled ++ historyDependencies.collect {
        case dependency if enabled(dependency.to) && dependency.enablesContinuation => dependency.from
      }
      if next == enabled then enabled else expand(next)
    antecedentSteps.filter(expand(Set(root)))
  def historyEnablesRoot: Boolean = antecedentsEnablingRoot.nonEmpty
  def historySequenceProven: Boolean =
    antecedentSteps.nonEmpty &&
      historyDependencies.nonEmpty &&
      historyDependencies.forall(_.planConnectionProven) &&
      antecedentsEnablingRoot.size == antecedentSteps.size
  def historicalSequence: List[PlanCausalEventNode] =
    (antecedentsEnablingRoot :+ root).distinct.sortBy(event => (event.step.ply, event.moveUci))
  def historicalCompletionProven: Boolean =
    historySequenceProven && PlanCausalEpisode.resultConsequences(root).exists(consequence =>
      consequence.positive && consequence.strength > 0
    )
  def futureEvents: List[PlanCausalEventNode] = planSteps.filterNot(_ == root)
  lazy val rootEnabledSteps: List[PlanCausalEventNode] =
    @annotation.tailrec
    def expand(enabled: Set[PlanCausalEventNode]): Set[PlanCausalEventNode] =
      val next = enabled ++ dependencies.collect {
        case dependency if enabled(dependency.from) && dependency.enablesContinuation => dependency.to
      }
      if next == enabled then enabled else expand(next)
    planSteps.filter(expand(Set(root)))
  def rootEnablesContinuation: Boolean = rootEnabledSteps.exists(_ != root)
  def continuationsEnabledByRoot: List[PlanCausalEventNode] = rootEnabledSteps.filterNot(_ == root)
  def responseResults: List[(PlanCausalResponse, TransitionConsequence)] =
    responses.flatMap(response => response.structuralConsequences.map(response -> _))
  def responseResultProven: Boolean =
    responseResults.exists((response, consequence) =>
      response.proven && consequence.positive && consequence.strength > 0 && consequence.subjects.nonEmpty
    )
  def causalEpisodeProven: Boolean = planSequenceProven || historySequenceProven || responseResultProven
  def requiredPlyOffset: Int =
    representativeResult.map(_._1.step.ply - root.step.ply).getOrElse(0).max(0)
  private lazy val rankedResults: List[(PlanCausalEventNode, TransitionConsequence)] =
    continuationsEnabledByRoot
      .flatMap(event => PlanCausalEpisode.resultConsequences(event).map(event -> _))
      .filter((_, consequence) => consequence.positive && consequence.strength > 0)
      .sortBy((event, consequence) =>
        (
          enablingPathPriority(event),
          -PlanCausalEpisode.resultSalience(consequence.kind),
          -consequence.strength,
          event.step.ply
        )
      )
  def representativeResult: Option[(PlanCausalEventNode, TransitionConsequence)] =
    rankedResults.headOption
  def representativeResultMatching(
      ownsResult: (PlanCausalEventNode, TransitionConsequence) => Boolean
  ): Option[(PlanCausalEventNode, TransitionConsequence)] =
    rankedResults.find(ownsResult.tupled)
  def futureEvent: Option[PlanCausalEventNode] =
    representativeResult.map(_._1).orElse(continuationsEnabledByRoot.lastOption)
  def futureMove: Option[String] = futureEvent.map(_.moveUci)
  def futureTarget: Option[EvidenceSquare] =
    representativeResult
      .filterNot((_, consequence) => PlanCausalEpisode.meansOnlyResultKind(consequence.kind))
      .flatMap((event, consequence) =>
        PlanCausalEpisode.consequenceTargetSquares(event.identity, consequence) match
          case target :: Nil => Some(target)
          case _             => None
      )
  def completionProven: Boolean =
    representativeResult.exists(_._1 != root)
  def representativeSequence: List[PlanCausalEventNode] =
    representativeResult
      .flatMap((event, _) => enablingPathTo(event))
      .getOrElse(List(root))
  def enablingPathTo(destination: PlanCausalEventNode): Option[List[PlanCausalEventNode]] =
    selectedEnablingRouteTo(destination).map(_._1)
  def enablingDependenciesTo(destination: PlanCausalEventNode): List[PlanCausalEventDependency] =
    selectedEnablingRouteTo(destination).map(_._2).getOrElse(Nil)
  def vacatedSquareLineAccessTo(destination: PlanCausalEventNode): List[LineAccessTrajectory] =
    enablingDependenciesTo(destination).collect {
      case PlanCausalEventDependency(
            from,
            to,
            PlanCausalDependencyKind.LineAccessPrecondition,
            PlanCausalDependencyProof.LineAccess(trajectory),
            _
          )
          if from == root &&
            to == destination &&
            trajectory.enabledTo == trajectory.vacatedSquare &&
            !trajectory.placesPieceBeforeClearance =>
        trajectory
    }

  private def futureConnectedToRoot: Boolean =
    @annotation.tailrec
    def expand(connected: Set[PlanCausalEventNode]): Set[PlanCausalEventNode] =
      val next = connected ++ futureDependencies.flatMap { dependency =>
        if connected(dependency.from) || connected(dependency.to) then List(dependency.from, dependency.to)
        else Nil
      }
      if next == connected then connected else expand(next)
    planSteps.toSet.subsetOf(expand(Set(root)))

  private def enablingPathPriority(event: PlanCausalEventNode): Int =
    def pathPriority(node: PlanCausalEventNode): Option[Int] =
      if node == root then Some(0)
      else
        dependencies
          .filter(dependency => dependency.to == node && dependency.enablesContinuation)
          .flatMap(dependency =>
            pathPriority(dependency.from).map(previous =>
              previous.max(PlanCausalEpisode.enablingDependencyPriority(dependency.kind))
            )
          )
          .minOption
    pathPriority(event).getOrElse(Int.MaxValue)

  private def selectedEnablingRouteTo(
      destination: PlanCausalEventNode
  ): Option[(List[PlanCausalEventNode], List[PlanCausalEventDependency])] =
    def routesTo(
        node: PlanCausalEventNode
    ): List[(List[PlanCausalEventNode], List[PlanCausalEventDependency])] =
      if node == root then List(List(root) -> Nil)
      else
        dependencies
          .filter(dependency => dependency.to == node && dependency.enablesContinuation)
          .flatMap(dependency =>
            routesTo(dependency.from).map { case (nodes, routeDependencies) =>
              (nodes :+ node) -> (routeDependencies :+ dependency)
            }
          )
    routesTo(destination).sortBy { case (nodes, routeDependencies) =>
      (
        routeDependencies.map(dependency => PlanCausalEpisode.enablingDependencyPriority(dependency.kind)).maxOption
          .getOrElse(0),
        routeDependencies.map(dependency => PlanCausalEpisode.enablingDependencyPriority(dependency.kind)).sum,
        routeDependencies.size,
        nodes.map(_.moveUci).mkString(":")
      )
    }.headOption

final case class OpponentResourceComparison(
    rootMove: String,
    sourceProbeId: String,
    resourceLine: LineNodeRef
)

final case class OpponentResourceDeterrenceProof private[chessjudgment] (
    sourceProbeId: String,
    resourceLine: LineNodeRef,
    comparisons: List[OpponentResourceComparison],
    materialGainPlyOffset: Int
):
  require(
    materialGainPlyOffset >= 1,
    "opponent-resource proof material result must follow the resource move"
  )

  private def canonicalLine(
      ref: LineNodeRef,
      lines: List[CandidateLineNode]
  ): Option[CandidateLineNode] =
    lines.filter(_.ref == ref) match
      case line :: Nil => Some(line)
      case _           => None

  private def canonicalEvaluatedLine(
      ref: LineNodeRef,
      lines: List[CandidateLineNode],
      graph: TypedEvidenceGraph
  ): Option[CandidateLineNode] =
    for
      line <- canonicalLine(ref, lines)
      evalRecord <- graph.records.collect {
        case record @ EvidenceRecord(_, payload: EvalFactEvidence, _) if payload.line == ref =>
          record
      } match
        case record :: Nil => Some(record)
        case _             => None
      eval <- evalRecord.payload match
        case payload: EvalFactEvidence => Some(payload)
        case _                         => None
      if evalRecord.ref.producer == EvidenceProducer.EngineEvalProducer
      if evalRecord.ref.layer == EvidenceLayer.Eval
      if evalRecord.ref.position == line.evidence.position
      if evalRecord.ref.line.contains(ref)
      if evalRecord.ref.scope == line.role.scope
      if evalRecord.parents == List(line.evidence)
      if eval.whitePovEvalCp == line.whitePovEvalCp
      if eval.mate == line.mate
      if eval.depth == line.depth
    yield line

  private def canonicalResourceEvidence(
      lines: List[CandidateLineNode],
    graph: TypedEvidenceGraph
  ): Option[(CandidateLineNode, LineFactEvidence)] =
    for
      line <- canonicalEvaluatedLine(resourceLine, lines, graph)
      record <- graph.records.collect {
        case record @ EvidenceRecord(_, payload: LineFactEvidence, _) if payload.line == resourceLine =>
          record
      } match
        case record :: Nil => Some(record)
        case _             => None
      facts <- record.payload match
        case payload: LineFactEvidence => Some(payload)
        case _                         => None
      if record.ref == line.evidence
      if record.ref.producer == EvidenceProducer.LegalLineProducer
      if record.ref.layer == EvidenceLayer.Line
      if record.ref.position == line.evidence.position
      if record.ref.line.contains(resourceLine)
      if record.ref.scope == line.role.scope
      if facts.line == resourceLine
      if facts.lineReplayMoves.map(EvidenceRef.normalizeMove) ==
        line.line.moves.map(EvidenceRef.normalizeMove)
    yield line -> facts

  private def canonicalWitness(
      perspective: Color,
      lines: List[CandidateLineNode],
      graph: TypedEvidenceGraph
  ): Option[(List[LineReplayStep], LineMaterialCapture)] =
    for
      (_, facts) <- canonicalResourceEvidence(lines, graph)
      materialGain <- facts
        .materialGainCapturesFor(perspective)
        .filter(_.plyOffset == materialGainPlyOffset) match
        case gain :: Nil => Some(gain)
        case _           => None
      resourceSequence = facts.lineReplaySteps.take(materialGainPlyOffset + 1)
      if resourceSequence.size == materialGainPlyOffset + 1
      if resourceSequence.size >= 2
    yield resourceSequence -> materialGain

  def resourceSequence(
      perspective: Color,
      lines: List[CandidateLineNode],
      graph: TypedEvidenceGraph
  ): Option[List[LineReplayStep]] =
    canonicalWitness(perspective, lines, graph).map(_._1)

  def materialGain(
      perspective: Color,
      lines: List[CandidateLineNode],
      graph: TypedEvidenceGraph
  ): Option[LineMaterialCapture] =
    canonicalWitness(perspective, lines, graph).map(_._2)

  def resourceStep(
      perspective: Color,
      lines: List[CandidateLineNode],
      graph: TypedEvidenceGraph
  ): Option[LineReplayStep] =
    resourceSequence(perspective, lines, graph).flatMap(_.headOption)

  def responseStep(
      perspective: Color,
      lines: List[CandidateLineNode],
      graph: TypedEvidenceGraph
  ): Option[LineReplayStep] =
    resourceSequence(perspective, lines, graph).flatMap(_.lift(1))

  def materialResultStep(
      perspective: Color,
      lines: List[CandidateLineNode],
      graph: TypedEvidenceGraph
  ): Option[LineReplayStep] =
    resourceSequence(perspective, lines, graph).flatMap(_.lastOption)

  def resourceMove(
      perspective: Color,
      lines: List[CandidateLineNode],
      graph: TypedEvidenceGraph
  ): Option[String] =
    resourceStep(perspective, lines, graph).map(_.moveUci)

  def consequence(
      perspective: Color,
      lines: List[CandidateLineNode],
      graph: TypedEvidenceGraph
  ): Option[TransitionConsequence] =
    resourceMove(perspective, lines, graph).map(OpponentResourceDeterrenceProof.consequence)

  def rootResourceImprovementWinPercent(
      perspective: Color,
      rootLine: LineNodeRef,
      lines: List[CandidateLineNode]
  ): Option[Double] =
    for
      baseline <- canonicalLine(rootLine, lines)
      resource <- canonicalLine(resourceLine, lines)
    yield
      PerspectiveMath.winPercentForMover(perspective, resource.whitePovEvalCp, resource.mate) -
        PerspectiveMath.winPercentForMover(perspective, baseline.whitePovEvalCp, baseline.mate)

  def bestComparisonContrastWinPercent(
      perspective: Color,
      lines: List[CandidateLineNode]
  ): Option[Double] =
    val resolvedComparisons = comparisons.map(comparison => canonicalLine(comparison.resourceLine, lines))
    for
      resource <- canonicalLine(resourceLine, lines)
      comparisonLines <- Option.when(resolvedComparisons.forall(_.nonEmpty))(resolvedComparisons.flatten)
      if comparisonLines.nonEmpty
    yield
      val rootOutcome =
        PerspectiveMath.winPercentForMover(perspective, resource.whitePovEvalCp, resource.mate)
      comparisonLines.map(line =>
        rootOutcome - PerspectiveMath.winPercentForMover(
          perspective,
          line.whitePovEvalCp,
          line.mate
        )
      ).max

  private def canonicalEvaluationProven(
      rootLine: LineNodeRef,
      perspective: Color,
      lines: List[CandidateLineNode],
      graph: TypedEvidenceGraph,
      threshold: Double
  ): Boolean =
    val resolvedComparisons =
      comparisons.map(comparison => canonicalEvaluatedLine(comparison.resourceLine, lines, graph))
    (for
      baseline <- canonicalEvaluatedLine(rootLine, lines, graph)
      resource <- canonicalEvaluatedLine(resourceLine, lines, graph)
      comparisonLines <- Option.when(resolvedComparisons.forall(_.nonEmpty))(resolvedComparisons.flatten)
      if comparisonLines.nonEmpty
    yield
      val resourceOutcome =
        PerspectiveMath.winPercentForMover(perspective, resource.whitePovEvalCp, resource.mate)
      val rootImprovement =
        resourceOutcome - PerspectiveMath.winPercentForMover(
          perspective,
          baseline.whitePovEvalCp,
          baseline.mate
        )
      val bestComparisonContrast = comparisonLines.map(line =>
        resourceOutcome - PerspectiveMath.winPercentForMover(
          perspective,
          line.whitePovEvalCp,
          line.mate
        )
      ).max
      rootImprovement >= threshold && bestComparisonContrast >= threshold
    ).contains(true)

  private def structurallyProven(
      rootLine: LineNodeRef,
      rootTransition: StructuralTransitionBinding,
      episode: PlanCausalEpisode,
      lines: List[CandidateLineNode],
      graph: TypedEvidenceGraph
  ): Boolean =
    canonicalWitness(rootTransition.perspective, lines, graph).exists { case (resourceSequence, materialGain) =>
      val resourceStep = resourceSequence.head
      val responseStep = resourceSequence(1)
      val materialResultStep = resourceSequence.last
      val resource = EvidenceRef.normalizeMove(resourceStep.moveUci)
      val exactResourcePawn =
        resource.length >= 4 &&
          (for
            from <- Square.fromKey(resource.take(2))
            to <- Square.fromKey(resource.slice(2, 4))
            before <- _root_.chess.format.Fen.read(
              _root_.chess.variant.Standard,
              _root_.chess.format.Fen.Full(resourceStep.fenBefore)
            )
            after <- _root_.chess.format.Fen.read(
              _root_.chess.variant.Standard,
              _root_.chess.format.Fen.Full(resourceStep.fenAfter)
            )
            pawn <- before.board.pieceAt(from)
            advanced <- after.board.pieceAt(to)
          yield
            pawn.color == !rootTransition.perspective && pawn.role == Pawn &&
              advanced.color == pawn.color && advanced.role == Pawn &&
              from.file == to.file
          ).contains(true)
      val exactMaterialResult =
        EvidenceRef.sameMove(materialGain.moveUci, materialResultStep.moveUci) &&
          materialGain.side == rootTransition.perspective &&
          materialGain.square.key.equalsIgnoreCase(EvidenceRef.normalizeMove(materialResultStep.moveUci).slice(2, 4))
      val captureEnabledByRoot = episode.continuationsEnabledByRoot.exists(event =>
        EvidenceRef.sameMove(event.moveUci, materialGain.moveUci)
      )
      val exactObservedSequence =
        resourceSequence.sliding(2).forall {
          case List(previous, next) =>
            previous.ply + 1 == next.ply &&
              PrincipalVariationEvidence.sameBoardState(previous.fenAfter, next.fenBefore)
          case _ => true
        }
      rootTransition.line.contains(rootLine) &&
        resourceLine.role == LineNodeRole.Threat &&
        sourceProbeId.trim.nonEmpty &&
        comparisons.exists(comparison =>
          comparison.sourceProbeId.trim.nonEmpty &&
            comparison.resourceLine.role == LineNodeRole.Threat &&
            canonicalLine(comparison.resourceLine, lines).nonEmpty &&
            !EvidenceRef.sameMove(comparison.rootMove, rootLine.rootMove)
        ) &&
        PrincipalVariationEvidence.sameBoardState(resourceStep.fenBefore, rootTransition.to.fen) &&
        responseStep.ply == resourceStep.ply + 1 &&
        materialResultStep.ply >= responseStep.ply &&
        exactResourcePawn &&
        exactMaterialResult &&
        exactObservedSequence &&
        captureEnabledByRoot &&
        episode.rootEnablesContinuation
    }

  def proven(
      rootLine: LineNodeRef,
      rootTransition: StructuralTransitionBinding,
      episode: PlanCausalEpisode,
      lines: List[CandidateLineNode],
      graph: TypedEvidenceGraph
  ): Boolean =
    structurallyProven(rootLine, rootTransition, episode, lines, graph) &&
      canonicalEvaluationProven(
        rootLine,
        rootTransition.perspective,
        lines,
        graph,
        JudgmentThresholds.SIGNIFICANT_THREAT_WP
      )

  def materialCounterplayPreventionProven(
      rootLine: LineNodeRef,
      rootTransition: StructuralTransitionBinding,
      episode: PlanCausalEpisode,
      lines: List[CandidateLineNode],
      graph: TypedEvidenceGraph
  ): Boolean =
    structurallyProven(rootLine, rootTransition, episode, lines, graph) &&
      responseStep(rootTransition.perspective, lines, graph) ==
        materialResultStep(rootTransition.perspective, lines, graph) &&
      canonicalEvaluationProven(
        rootLine,
        rootTransition.perspective,
        lines,
        graph,
        JudgmentThresholds.MATERIAL_THREAT_WP
      )

object OpponentResourceDeterrenceProof:
  def consequence(resourceMove: String): TransitionConsequence =
    val normalized = EvidenceRef.normalizeMove(resourceMove)
    TransitionConsequence(
      kind = TransitionConsequenceKind.OpponentMobilityRestriction,
      polarity = StructuralSignalPolarity.Gain,
      strength = 1,
      subjects = List(s"pawn:${normalized.take(2)}-${normalized.slice(2, 4)}:advance-restricted")
    )

object PlanCausalEpisode:
  private val RouteResultKinds = Set(
    TransitionConsequenceKind.MobilityGain,
    TransitionConsequenceKind.DevelopmentLagReduced,
    TransitionConsequenceKind.DevelopmentPieceActivated,
    TransitionConsequenceKind.DevelopmentMobilityGain,
    TransitionConsequenceKind.DevelopmentSafePlacement,
    TransitionConsequenceKind.RookLiftActivation
  )

  private val PressureKinds = Set(
    TransitionConsequenceKind.WeakPawnTargetCreated,
    TransitionConsequenceKind.WeakSquareTargetCreated,
    TransitionConsequenceKind.TargetPressureGain,
    TransitionConsequenceKind.KingSafetyPressure,
    TransitionConsequenceKind.KingRingPressureGain,
    TransitionConsequenceKind.BatteryPressureGain,
    TransitionConsequenceKind.OpponentMobilityRestriction
  )

  private val DestinationResultKinds = Set(
    TransitionConsequenceKind.SpaceGain,
    TransitionConsequenceKind.FileOccupationGain,
    TransitionConsequenceKind.OutpostGain,
    TransitionConsequenceKind.PassedPawnProgress,
    TransitionConsequenceKind.PromotionPressureGain
  )

  private val MeansOnlyResultKinds = RouteResultKinds ++ Set(
    TransitionConsequenceKind.DevelopmentCenterControlGain,
    TransitionConsequenceKind.LineUnlockGain,
    TransitionConsequenceKind.FileAccessGain
  )

  private[judgment] def enablingDependencyPriority(kind: PlanCausalDependencyKind): Int =
    kind match
      case PlanCausalDependencyKind.PawnAdvanceSupport               => 0
      case PlanCausalDependencyKind.ResponseContinuationPrecondition => 0
      case PlanCausalDependencyKind.ObjectStatePrecondition          => 1
      case PlanCausalDependencyKind.RetreatControlPrecondition       => 2
      case PlanCausalDependencyKind.LineAccessPrecondition           => 3
      case _                                                         => Int.MaxValue

  def resultConsequences(event: PlanCausalEventNode): List[TransitionConsequence] =
    (
      event.structuralConsequences ++
        event.developmentChoices.map(choice =>
          TransitionConsequence(
            kind = TransitionConsequenceKind.DevelopmentPieceActivated,
            polarity = StructuralSignalPolarity.Gain,
            strength = 1,
            subjects = List(s"${choice.role}:${choice.from}-${choice.to}")
          )
        )
    )
      .filter(_.subjects.exists(_.trim.nonEmpty))
      .distinctBy(consequence => (consequence.kind, consequence.polarity, consequence.subjects.sorted))

  def consequenceSquares(consequence: TransitionConsequence): List[EvidenceSquare] =
    consequence.subjects
      .flatMap(subject => "[a-h][1-8]".r.findAllIn(subject.toLowerCase).map(EvidenceSquare(_)))
      .distinct

  def goalTargetSubjects(consequence: TransitionConsequence): List[String] =
    consequence.goalSubjects

  def consequenceTargetSquares(
      identity: PlanEventIdentity,
      consequence: TransitionConsequence
  ): List[EvidenceSquare] =
    val routeSquares = (identity.actorFrom.toList ++ identity.actorTo.toList).map(_.toLowerCase).toSet
    consequence.goalSubjects.flatMap { subject =>
      val normalized = subject.trim.toLowerCase
      val explicitSquare = normalized.stripPrefix("square:").matches("[a-h][1-8]")
      "[a-h][1-8]".r
        .findAllIn(normalized)
        .map(EvidenceSquare(_))
        .filter(square => explicitSquare || DestinationResultKinds(consequence.kind) || !routeSquares(square.key))
    }.distinct

  private[judgment] def routeResultKind(kind: TransitionConsequenceKind): Boolean =
    RouteResultKinds(kind)

  private[judgment] def pressureResultKind(kind: TransitionConsequenceKind): Boolean =
    PressureKinds(kind)

  private[judgment] def destinationResultKind(kind: TransitionConsequenceKind): Boolean =
    DestinationResultKinds(kind)

  private[chessjudgment] def meansOnlyResultKind(kind: TransitionConsequenceKind): Boolean =
    MeansOnlyResultKinds(kind)

  def pressureTargetSquares(event: PlanCausalEventNode): Set[String] =
    event.structuralConsequences
      .filter(consequence => PressureKinds(consequence.kind))
      .flatMap(consequenceSquares)
      .map(_.key.toLowerCase)
      .toSet

  def responseOriginSquares(event: PlanCausalEventNode): Set[String] =
    val tensionPawnSquares = _root_.chess.format.Fen
      .read(_root_.chess.variant.Standard, _root_.chess.format.Fen.Full(event.step.fenAfter))
      .toList
      .flatMap { position =>
        event.structuralConsequences
          .filter(_.kind == TransitionConsequenceKind.PawnTensionGain)
          .flatMap(consequenceSquares)
          .filter(square =>
            Square.fromKey(square.key).flatMap(position.board.pieceAt).exists(piece =>
              piece.color == !event.perspective && piece.role == Pawn
            )
          )
      }
      .map(_.key.toLowerCase)
      .toSet
    pressureTargetSquares(event) ++ tensionPawnSquares

  def planPiecePresent(trigger: PlanCausalEventNode, fen: String): Boolean =
    positionWithPlanPiece(trigger, fen).nonEmpty

  def opponentCanAnswerPlanPiece(trigger: PlanCausalEventNode, fen: String): Boolean =
    positionWithPlanPiece(trigger, fen).exists(_.color == !trigger.perspective)

  private def positionWithPlanPiece(trigger: PlanCausalEventNode, fen: String): Option[Position] =
    (for
      (planSquare, planPiece) <- planPieceAfterMove(trigger)
      position <- standardPosition(fen)
      if position.board.pieceAt(planSquare).contains(planPiece)
    yield position)

  def responseAttacksPlanPiece(trigger: PlanCausalEventNode, response: LineReplayStep): Boolean =
    val responseMove = EvidenceRef.normalizeMove(response.moveUci)
    (for
      (planSquare, planPiece) <- planPieceAfterMove(trigger)
      responseFrom <- Square.fromKey(responseMove.take(2))
      responseTo <- Square.fromKey(responseMove.slice(2, 4))
      beforeResponse <- standardPosition(response.fenBefore)
      afterResponse <- standardPosition(response.fenAfter)
      if planPiece.color == trigger.perspective
      if beforeResponse.board.pieceAt(planSquare).contains(planPiece)
      responder <- beforeResponse.board.pieceAt(responseFrom)
      if responder.color == !trigger.perspective
      if afterResponse.board.pieceAt(responseTo).contains(responder)
      if afterResponse.board.pieceAt(planSquare).contains(planPiece)
      if afterResponse.board.attackers(planSquare, responder.color).squares.contains(responseTo)
      if !beforeResponse.board.attackers(planSquare, responder.color).squares.contains(responseFrom)
    yield true).contains(true)

  def responseAnswersCheck(trigger: PlanCausalEventNode, response: LineReplayStep): Boolean =
    val responseMove = EvidenceRef.normalizeMove(response.moveUci)
    response.ply == trigger.step.ply + 1 &&
      PrincipalVariationEvidence.sameBoardState(trigger.step.fenAfter, response.fenBefore) &&
      (for
        responseFrom <- Square.fromKey(responseMove.take(2))
        responseTo <- Square.fromKey(responseMove.slice(2, 4))
        beforeResponse <- standardPosition(response.fenBefore)
        afterResponse <- standardPosition(response.fenAfter)
        responder <- beforeResponse.board.pieceAt(responseFrom)
        if beforeResponse.check.yes
        if responder.color == !trigger.perspective && responder.role == King
        if afterResponse.board.pieceAt(responseTo).contains(responder)
        if !afterResponse.check.yes
      yield true).contains(true)

  def responseWeakenedKingSquare(
      trigger: PlanCausalEventNode,
      response: LineReplayStep
  ): Option[EvidenceSquare] =
    val responseMove = EvidenceRef.normalizeMove(response.moveUci)
    for
      responseFrom <- Square.fromKey(responseMove.take(2))
      responseTo <- Square.fromKey(responseMove.slice(2, 4))
      beforeResponse <- standardPosition(response.fenBefore)
      afterResponse <- standardPosition(response.fenAfter)
      responder <- beforeResponse.board.pieceAt(responseFrom)
      kingSquare <- beforeResponse.board.kingPosOf(responder.color)
      if responder.color == !trigger.perspective
      if responder.role == Pawn
      if responseFrom.file == responseTo.file
      if
        if responder.color.white then responseTo.rank.value > responseFrom.rank.value
        else responseTo.rank.value < responseFrom.rank.value
      if kingOnWing(kingSquare)
      if kingSquare.kingAttacks.squares.contains(responseFrom)
      if afterResponse.board.kingPosOf(responder.color).contains(kingSquare)
      if afterResponse.board.pieceAt(responseTo).contains(responder)
    yield EvidenceSquare(kingSquare.key.toLowerCase)

  private def planPieceAfterMove(trigger: PlanCausalEventNode): Option[(Square, Piece)] =
    for
      square <- Square.fromKey(EvidenceRef.normalizeMove(trigger.step.moveUci).slice(2, 4))
      position <- standardPosition(trigger.step.fenAfter)
      actor <- position.board.pieceAt(square)
    yield square -> actor

  private def standardPosition(fen: String): Option[Position] =
    _root_.chess.format.Fen.read(
      _root_.chess.variant.Standard,
      _root_.chess.format.Fen.Full(fen)
    )

  private def kingOnWing(square: Square): Boolean =
    square.file.value <= 2 || square.file.value >= 5

  def responseCapturesPlanPiece(trigger: PlanCausalEventNode, response: LineReplayStep): Boolean =
    val triggerMove = EvidenceRef.normalizeMove(trigger.step.moveUci)
    val responseMove = EvidenceRef.normalizeMove(response.moveUci)
    val triggerTo = triggerMove.slice(2, 4)
    responseMove.slice(2, 4) == triggerTo &&
      (for
        triggerSquare <- Square.fromKey(triggerTo)
        responseFrom <- Square.fromKey(responseMove.take(2))
        afterTrigger <- _root_.chess.format.Fen.read(
          _root_.chess.variant.Standard,
          _root_.chess.format.Fen.Full(trigger.step.fenAfter)
        )
        beforeResponse <- _root_.chess.format.Fen.read(
          _root_.chess.variant.Standard,
          _root_.chess.format.Fen.Full(response.fenBefore)
        )
        afterResponse <- _root_.chess.format.Fen.read(
          _root_.chess.variant.Standard,
          _root_.chess.format.Fen.Full(response.fenAfter)
        )
        triggerActor <- afterTrigger.board.pieceAt(triggerSquare)
        if triggerActor.color == trigger.perspective
        if beforeResponse.board.pieceAt(triggerSquare).contains(triggerActor)
        responder <- beforeResponse.board.pieceAt(responseFrom)
        if responder.color == !trigger.perspective
        if afterResponse.board.pieceAt(triggerSquare).contains(responder)
      yield true).contains(true)

  def triggerMoveCapturesPiece(trigger: PlanCausalEventNode): Boolean =
    val move = EvidenceRef.normalizeMove(trigger.step.moveUci)
    (for
      destination <- Square.fromKey(move.slice(2, 4))
      before <- _root_.chess.format.Fen.read(
        _root_.chess.variant.Standard,
        _root_.chess.format.Fen.Full(trigger.step.fenBefore)
      )
      after <- _root_.chess.format.Fen.read(
        _root_.chess.variant.Standard,
        _root_.chess.format.Fen.Full(trigger.step.fenAfter)
      )
      captured <- before.board.pieceAt(destination)
      mover <- after.board.pieceAt(destination)
      if captured.color != mover.color && mover.color == trigger.perspective
    yield true).contains(true)

  def flankAdvanceProof(
      from: PlanCausalEventNode,
      to: PlanCausalEventNode,
      king: EvidenceSquare,
      targets: List[EvidenceSquare]
  ): Boolean =
    val move = from.moveUci
    val fromSquare = move.take(2)
    val toSquare = move.slice(2, 4)
    val targetKeys = targets.map(_.key.toLowerCase).filter(_.matches("[a-h][1-8]")).toSet
    val actualKing = _root_.chess.format.Fen
      .read(_root_.chess.variant.Standard, _root_.chess.format.Fen.Full(from.step.fenBefore))
      .flatMap(_.board.kingPosOf(!from.perspective))
      .map(_.key)
    from.identity.actorRole.exists(_.equalsIgnoreCase("pawn")) &&
      fromSquare.matches("[a-h][1-8]") &&
      toSquare.matches("[a-h][1-8]") &&
      fromSquare.head == toSquare.head &&
      Set('a', 'b', 'g', 'h')(fromSquare.head) &&
      to.identity.actorRole.exists(role => !role.equalsIgnoreCase("pawn")) &&
      actualKing.contains(king.key.toLowerCase) &&
      (king.key.head - fromSquare.head).abs <= 2 &&
      targetKeys.nonEmpty &&
      targetKeys.subsetOf(pressureTargetSquares(to)) &&
      targetKeys.exists(square => kingDistance(square, king.key) <= 2)

  private[judgment] def resultSalience(kind: TransitionConsequenceKind): Int =
    kind match
      case TransitionConsequenceKind.WeakPawnTargetCreated | TransitionConsequenceKind.WeakSquareTargetCreated |
          TransitionConsequenceKind.TargetPressureGain | TransitionConsequenceKind.KingRingPressureGain |
          TransitionConsequenceKind.PassedPawnProgress | TransitionConsequenceKind.PromotionPressureGain |
          TransitionConsequenceKind.OutpostGain | TransitionConsequenceKind.BatteryPressureGain |
          TransitionConsequenceKind.PieceExchangeAvailable | TransitionConsequenceKind.PieceExchangeCompleted =>
        3
      case TransitionConsequenceKind.FileOccupationGain | TransitionConsequenceKind.OpenFileGain |
          TransitionConsequenceKind.SemiOpenFileGain | TransitionConsequenceKind.OpponentMobilityRestriction |
          TransitionConsequenceKind.KingSafetyPressure | TransitionConsequenceKind.SpaceGain =>
        2
      case _ =>
        1

  private def kingDistance(square: String, king: String): Int =
    if !square.matches("[a-h][1-8]") || !king.matches("[a-h][1-8]") then Int.MaxValue
    else math.max((square.head - king.head).abs, (square.last.asDigit - king.last.asDigit).abs)

enum PlanCausalBranchOutcome:
  case Realized
  case Deferred
  case Diverted
  case Refuted

enum PlanCausalRealizationMatch:
  case ExactMove
  case EquivalentFunction

enum PlanCausalTerminalOutcome:
  case Victory
  case Defeat
  case Draw

enum PlanCausalRobustness:
  case Untested
  case Deferred
  case Superseded
  case Refuted
  case Conditional
  case Robust

final case class PlanCausalBranchWitness(
    sourceProbeId: String,
    line: LineNodeRef,
    outcome: PlanCausalBranchOutcome,
    observedEpisode: Option[PlanCausalEpisode],
    observedConsequences: List[TransitionConsequence],
    realizationMatch: Option[PlanCausalRealizationMatch],
    realizationMove: Option[String],
    requiredPlyOffset: Int,
    certifiedHorizonPlyOffset: Int,
    observedPlyOffset: Int,
    observedThroughPlyOffset: Int,
    terminalOutcome: Option[PlanCausalTerminalOutcome],
    terminalPlyOffset: Option[Int],
    terminalStep: Option[LineReplayStep]
)

object PlanCausalFunctionalMatch:
  def functionallyEquivalent(
      expected: List[TransitionConsequence],
      observed: List[TransitionConsequence]
  ): Boolean =
    val expectedPositive = expected.filter(consequence => consequence.positive && consequence.strength > 0)
    val observedPositive = observed.filter(consequence => consequence.positive && consequence.strength > 0)
    expectedPositive.exists(left =>
      observedPositive.exists(right =>
        left.kind == right.kind && targetObjectsCompatible(left, right)
      )
    )

  def causallyEquivalent(
      expectedEpisode: PlanCausalEpisode,
      expectedEvent: PlanCausalEventNode,
      expected: List[TransitionConsequence],
      observedEpisode: PlanCausalEpisode,
      observedEvent: PlanCausalEventNode,
      observed: List[TransitionConsequence]
  ): Boolean =
    dependencyPathsCompatible(
      expectedEpisode.enablingDependenciesTo(expectedEvent),
      observedEpisode.enablingDependenciesTo(observedEvent)
    ) && functionallyEquivalent(expected, observed)

  private def targetObjectsCompatible(
      expected: TransitionConsequence,
      observed: TransitionConsequence
  ): Boolean =
    val left = EvidenceObjectBinding.goalTargetObjectGroups(expected).toSet
    val right = EvidenceObjectBinding.goalTargetObjectGroups(observed).toSet
    left.nonEmpty && right.nonEmpty && left.intersect(right).nonEmpty

  private def dependencyPathsCompatible(
      expected: List[PlanCausalEventDependency],
      observed: List[PlanCausalEventDependency]
  ): Boolean =
    def signature(dependency: PlanCausalEventDependency): (PlanCausalDependencyKind, List[String]) =
      dependency.kind -> dependency.proofPieceRoles.map(_.name.toLowerCase).distinct.sorted

    expected.nonEmpty &&
      observed.nonEmpty &&
      expected.forall(_.planConnectionProven) &&
      observed.forall(_.planConnectionProven) &&
      expected.map(signature) == observed.map(signature)

final case class PlanCausalResultObservation(
    line: LineNodeRef,
    replyMove: String,
    outcome: PlanCausalBranchOutcome,
    realizationEvent: Option[PlanCausalEventNode],
    realizationMatch: Option[PlanCausalRealizationMatch],
    observedThroughPlyOffset: Int,
    terminalOutcome: Option[PlanCausalTerminalOutcome],
    terminalPlyOffset: Option[Int],
    terminalStep: Option[LineReplayStep]
):
  def realizationMove: Option[String] = realizationEvent.map(_.moveUci)
  def realizationPlyOffset(root: PlanCausalEventNode): Option[Int] =
    realizationEvent.map(_.step.ply - root.step.ply)

final case class PlanCausalResultAssessment(
    sourceEvent: PlanCausalEventNode,
    consequence: TransitionConsequence,
    sourcePlyOffset: Int,
    observations: List[PlanCausalResultObservation],
    robustness: PlanCausalRobustness
):
  def positiveProofReady: Boolean =
    robustness == PlanCausalRobustness.Robust || robustness == PlanCausalRobustness.Conditional
  def realizedObservations: List[PlanCausalResultObservation] =
    observations.filter(_.outcome == PlanCausalBranchOutcome.Realized)

object PlanCausalResultAssessment:
  def from(
      episode: PlanCausalEpisode,
      sourceEvent: PlanCausalEventNode,
      consequence: TransitionConsequence,
      witnesses: List[PlanCausalBranchWitness],
      branchSetComplete: Boolean
  ): PlanCausalResultAssessment =
    val sourcePlyOffset = sourceEvent.step.ply - episode.root.step.ply
    val observations = witnesses.map(witness => observation(episode, sourceEvent, consequence, sourcePlyOffset, witness))
    val robustness =
      if observations.isEmpty then PlanCausalRobustness.Untested
      else if !branchSetComplete || observations.exists(_.outcome == PlanCausalBranchOutcome.Deferred) then
        PlanCausalRobustness.Deferred
      else if observations.forall(_.outcome == PlanCausalBranchOutcome.Realized) then PlanCausalRobustness.Robust
      else if observations.exists(_.outcome == PlanCausalBranchOutcome.Realized) then PlanCausalRobustness.Conditional
      else if observations.forall(observation =>
        observation.outcome == PlanCausalBranchOutcome.Diverted &&
          observation.terminalOutcome.exists(_ != PlanCausalTerminalOutcome.Defeat)
      ) then PlanCausalRobustness.Superseded
      else PlanCausalRobustness.Refuted
    PlanCausalResultAssessment(sourceEvent, consequence, sourcePlyOffset, observations, robustness)

  private def observation(
      episode: PlanCausalEpisode,
      sourceEvent: PlanCausalEventNode,
      consequence: TransitionConsequence,
      sourcePlyOffset: Int,
      witness: PlanCausalBranchWitness
  ): PlanCausalResultObservation =
    val candidates = witness.observedEpisode.toList.flatMap { observedEpisode =>
      observedEpisode.continuationsEnabledByRoot.filter { candidate =>
        val offset = candidate.step.ply - episode.root.step.ply
        val observedResults = PlanCausalEpisode.resultConsequences(candidate)
        offset <= witness.observedThroughPlyOffset &&
          PlanCausalFunctionalMatch.causallyEquivalent(
            episode,
            sourceEvent,
            List(consequence),
            observedEpisode,
            candidate,
            observedResults
          )
      }
    }
    val selected = candidates.sortBy { candidate =>
      val offset = candidate.step.ply - episode.root.step.ply
      (if EvidenceRef.sameMove(sourceEvent.moveUci, candidate.moveUci) && offset == sourcePlyOffset then 0 else 1, offset)
    }.headOption
    val realizationMatch = selected.map { candidate =>
      val offset = candidate.step.ply - episode.root.step.ply
      if EvidenceRef.sameMove(sourceEvent.moveUci, candidate.moveUci) && offset == sourcePlyOffset then
        PlanCausalRealizationMatch.ExactMove
      else PlanCausalRealizationMatch.EquivalentFunction
    }
    val terminalBeforeDeadline =
      witness.terminalOutcome.filter(_ => witness.terminalPlyOffset.exists(_ <= witness.observedThroughPlyOffset))
    val outcome =
      if selected.nonEmpty then PlanCausalBranchOutcome.Realized
      else
        terminalBeforeDeadline match
          case Some(PlanCausalTerminalOutcome.Defeat) => PlanCausalBranchOutcome.Refuted
          case Some(_)                                => PlanCausalBranchOutcome.Diverted
          case None if witness.observedThroughPlyOffset < sourcePlyOffset => PlanCausalBranchOutcome.Deferred
          case None if witness.observedEpisode.exists(_.continuationsEnabledByRoot.exists(event =>
              event.step.ply - episode.root.step.ply <= witness.observedThroughPlyOffset
            )) =>
            PlanCausalBranchOutcome.Diverted
          case None => PlanCausalBranchOutcome.Refuted
    PlanCausalResultObservation(
      line = witness.line,
      replyMove = witness.line.rootMove,
      outcome = outcome,
      realizationEvent = selected,
      realizationMatch = realizationMatch,
      observedThroughPlyOffset = witness.observedThroughPlyOffset,
      terminalOutcome = terminalBeforeDeadline,
      terminalPlyOffset = witness.terminalPlyOffset.filter(_ <= witness.observedThroughPlyOffset),
      terminalStep = Option.when(terminalBeforeDeadline.nonEmpty)(witness.terminalStep).flatten
    )

final case class PlanCausalEventEvidence(
    rootTransition: StructuralTransitionBinding,
    causalEpisode: PlanCausalEpisode,
    branchWitnesses: List[PlanCausalBranchWitness],
    observedMaterialCosts: List[ObservedPlanCost] = Nil,
    opponentResourceDeterrence: Option[OpponentResourceDeterrenceProof] = None,
    continuationSourceLine: Option[LineNodeRef] = None
) extends EvidencePayload:
  require(rootTransition.line.nonEmpty, "plan-causal root transition must reference its canonical line")
  require(
    EvidenceRef.sameMove(causalEpisode.root.moveUci, rootTransition.moveUci) &&
      causalEpisode.root.step.ply == rootTransition.to.ply &&
      PrincipalVariationEvidence.sameBoardState(causalEpisode.root.step.fenBefore, rootTransition.from.fen) &&
      PrincipalVariationEvidence.sameBoardState(causalEpisode.root.step.fenAfter, rootTransition.to.fen) &&
      causalEpisode.root.perspective == rootTransition.perspective,
    "plan-causal episode root must match its authoritative root transition"
  )

  def episode: Option[PlanCausalEpisode] =
    Option.when(causalEpisode.causalEpisodeProven)(causalEpisode)
  def identity: PlanEventIdentity = causalEpisode.root.identity
  def planId: PlanKind = identity.kind
  def rootLine: LineNodeRef = rootTransition.line.get
  def structuralConsequences: List[TransitionConsequence] = causalEpisode.root.structuralConsequences
  def developmentChoices: List[StructuralDevelopmentChoice] = causalEpisode.root.developmentChoices
  def rootMove: String = rootTransition.moveUci
  def perspective: Color = rootTransition.perspective
  private def representativeGoalResult: Option[(PlanCausalEventNode, TransitionConsequence)] =
    episode.flatMap { causalEpisode =>
      val directRookTransferOccupation =
        if identity.goalKind.contains(PlanKind.RookFileTransfer) then
          causalEpisode.representativeResultMatching((sourceEvent, consequence) =>
            rookTransferOccupiesFreedSquare(causalEpisode, sourceEvent, consequence)
          )
        else None
      val rootEnabledPieceRoute =
        if identity.goalTheme == PlanTheme.PieceRedeployment then
          directRookTransferOccupation.orElse(
            causalEpisode.representativeResultMatching((sourceEvent, consequence) =>
              rootOpenedLineProducesResult(causalEpisode, sourceEvent, consequence)
            )
          )
        else None
      val establishedResult =
        causalEpisode.representativeResultMatching((sourceEvent, consequence) =>
          resultAdvancesEstablishedGoal(sourceEvent, consequence)
        )
      val prioritizedResult =
        if identity.goalKind.exists(Set(PlanKind.WorstPieceImprovement, PlanKind.RookFileTransfer)) then
          rootEnabledPieceRoute.orElse(establishedResult)
        else establishedResult.orElse(rootEnabledPieceRoute)
      prioritizedResult
        .orElse(
          causalEpisode.representativeResultMatching((sourceEvent, consequence) =>
            resultAdvancesGoal(sourceEvent, consequence)
          )
        )
    }
  def representativeDirectGoalConsequence: Option[TransitionConsequence] =
    val establishedResults = directGoalConsequences.filter(consequence =>
      PlanCausalGoalProof.proves(identity, rootTransition, consequence)
    )
    establishedResults
      .sortBy(consequence =>
        val describesMovedPieceAtDestination =
          (
            PlanCausalEpisode.routeResultKind(consequence.kind) ||
              PlanCausalEpisode.destinationResultKind(consequence.kind)
          ) &&
            identity.actorTo.exists(destination =>
              PlanCausalEpisode.consequenceSquares(consequence).exists(_.key.equalsIgnoreCase(destination))
            )
        (
          -principalResultSalience(consequence),
          if describesMovedPieceAtDestination then 0 else 1,
          -consequence.strength,
          consequence.kind.toString
        )
      )
      .headOption
  private def representativeEventResult: Option[(PlanCausalEventNode, TransitionConsequence)] =
    representativeGoalResult.orElse(episode.flatMap(_.representativeResult))
  def representativeResult: Option[(PlanCausalEventNode, TransitionConsequence)] = representativeEventResult
  def futureMove: Option[String] = representativeEventResult.map(_._1.moveUci)
  def futureTarget: Option[EvidenceSquare] =
    representativeEventResult
      .filterNot((_, consequence) => PlanCausalEpisode.meansOnlyResultKind(consequence.kind))
      .flatMap((sourceEvent, consequence) =>
        PlanCausalEpisode.consequenceTargetSquares(sourceEvent.identity, consequence) match
          case target :: Nil => Some(target)
          case _             => None
      )
  def opponentResourceDeterrenceProofReady(
      lines: List[CandidateLineNode],
      graph: TypedEvidenceGraph
  ): Boolean =
    opponentResourceDeterrence.exists(proof =>
      episode.exists(proof.proven(rootLine, rootTransition, _, lines, graph))
    )
  def materialCounterplayPreventionProofReady(
      lines: List[CandidateLineNode],
      graph: TypedEvidenceGraph
  ): Boolean =
    opponentResourceDeterrence.exists(proof =>
      episode.exists(proof.materialCounterplayPreventionProven(rootLine, rootTransition, _, lines, graph))
    )
  def ownedConditionalResponseProofReady(
      lines: List[CandidateLineNode],
      graph: TypedEvidenceGraph
  ): Boolean =
    opponentResourceDeterrenceProofReady(lines, graph)
  def resultAdvancesGoal(consequence: TransitionConsequence): Boolean =
    PlanCausalGoalProof.proves(identity, rootTransition, consequence) ||
      PlanCausalGoalProof.ownsDirectPiecePressureAlongsideRoute(
        identity.goalTheme,
        identity.goalKind,
        rootTransition,
        structuralConsequences.filterNot(_ == consequence),
        consequence
      ) ||
      PlanCausalGoalProof.lineAccessAdvancesExchangeGoal(
        identity.goalTheme,
        structuralConsequences.filterNot(_ == consequence),
        consequence
      )
  private def transitionOf(sourceEvent: PlanCausalEventNode): StructuralTransitionBinding =
    rootTransition.copy(
      moveUci = sourceEvent.moveUci,
      from = PositionNodeRef(sourceEvent.step.fenBefore, sourceEvent.step.ply - 1, Some(sourceEvent.perspective)),
      to = PositionNodeRef(sourceEvent.step.fenAfter, sourceEvent.step.ply, Some(!sourceEvent.perspective)),
      perspective = sourceEvent.perspective
    )
  def resultAdvancesGoal(sourceEvent: PlanCausalEventNode, consequence: TransitionConsequence): Boolean =
    resultAdvancesEstablishedGoal(sourceEvent, consequence) ||
      samePieceRouteProducesResult(sourceEvent, consequence) ||
      episode.exists(causalEpisode =>
        rootOpenedLineProducesResult(causalEpisode, sourceEvent, consequence)
      )

  private def resultAdvancesEstablishedGoal(
      sourceEvent: PlanCausalEventNode,
      consequence: TransitionConsequence
  ): Boolean =
    val inducedCapturePath = episode.exists(causalEpisode =>
      causalEpisode.enablingDependenciesTo(sourceEvent).exists {
        case PlanCausalEventDependency(
              _,
              to,
              PlanCausalDependencyKind.ResponseContinuationPrecondition,
              PlanCausalDependencyProof.ResponseContinuation(_: CaptureResponseFollowUpTrajectory),
              _
            ) => to == sourceEvent
        case _ => false
      }
    )
    val enabledRookTransfer =
      identity.goalKind.contains(PlanKind.RookFileTransfer) &&
        consequence.subjects.nonEmpty &&
        Set(TransitionConsequenceKind.MobilityGain, TransitionConsequenceKind.TargetPressureGain)(consequence.kind) &&
        episode.exists(causalEpisode =>
          causalEpisode.enablingDependenciesTo(sourceEvent).exists {
            case PlanCausalEventDependency(
                  _,
                  to,
                  PlanCausalDependencyKind.LineAccessPrecondition,
                  PlanCausalDependencyProof.LineAccess(trajectory),
                  _
                ) =>
              to == sourceEvent &&
                trajectory.enabledPieceRole.name.equalsIgnoreCase(_root_.chess.Rook.toString)
            case _ => false
          }
        )
    if inducedCapturePath then
      PlanCausalGoalProof.provesAfterInducedResponse(identity, rootTransition, consequence)
    else if enabledRookTransfer then true
    else if
      identity.goalTheme == PlanTheme.PieceRedeployment &&
        episode.exists(_.root != sourceEvent)
    then false
    else PlanCausalGoalProof.proves(identity, transitionOf(sourceEvent), consequence)

  private def samePieceRouteProducesResult(
      sourceEvent: PlanCausalEventNode,
      consequence: TransitionConsequence
  ): Boolean =
    identity.goalTheme == PlanTheme.PieceRedeployment &&
      identity.actorRole.exists(rootRole =>
        !rootRole.equalsIgnoreCase(_root_.chess.Pawn.toString) &&
          sourceEvent.identity.actorRole.exists(_.equalsIgnoreCase(rootRole))
      ) &&
      !PlanCausalEpisode.triggerMoveCapturesPiece(sourceEvent) &&
      PlanCausalGoalProof.movedPieceCreatesRouteResult(sourceEvent, consequence) &&
      episode.exists { causalEpisode =>
        sourceEvent != causalEpisode.root &&
          causalEpisode.enablingDependenciesTo(sourceEvent).exists(dependency =>
            dependency.enablesContinuation && dependencyPieceMatchesSource(dependency, sourceEvent)
          )
      }

  private def rootOpenedLineProducesResult(
      causalEpisode: PlanCausalEpisode,
      sourceEvent: PlanCausalEventNode,
      consequence: TransitionConsequence
  ): Boolean =
    identity.goalTheme == PlanTheme.PieceRedeployment &&
      !PlanCausalEpisode.triggerMoveCapturesPiece(sourceEvent) &&
      PlanCausalGoalProof.movedPieceCreatesRouteResult(sourceEvent, consequence) &&
      causalEpisode.enablingDependenciesTo(sourceEvent).exists {
        case PlanCausalEventDependency(
              from,
              _,
              PlanCausalDependencyKind.LineAccessPrecondition,
              PlanCausalDependencyProof.LineAccess(trajectory),
              _
            ) =>
          from == causalEpisode.root &&
            sourceEvent.identity.actorRole.exists(_.equalsIgnoreCase(trajectory.enabledPieceRole.name))
        case _ => false
      }

  private def rookTransferOccupiesFreedSquare(
      causalEpisode: PlanCausalEpisode,
      sourceEvent: PlanCausalEventNode,
      consequence: TransitionConsequence
    ): Boolean =
      consequence.kind == TransitionConsequenceKind.FileOccupationGain &&
        rootOpenedLineProducesResult(causalEpisode, sourceEvent, consequence) &&
      causalEpisode.enablingDependenciesTo(sourceEvent).exists {
        case PlanCausalEventDependency(
              from,
              to,
              PlanCausalDependencyKind.LineAccessPrecondition,
              PlanCausalDependencyProof.LineAccess(trajectory),
              _
            ) =>
          from == causalEpisode.root &&
            to == sourceEvent &&
            trajectory.enabledTo == trajectory.vacatedSquare
        case _ => false
      }

  private def dependencyPieceMatchesSource(
      dependency: PlanCausalEventDependency,
      sourceEvent: PlanCausalEventNode
  ): Boolean =
    dependency.proof match
      case PlanCausalDependencyProof.ObjectState(trajectory) =>
        dependency.to == sourceEvent &&
          sourceEvent.identity.actorRole.exists(_.equalsIgnoreCase(trajectory.pieceRole.name)) &&
          EvidenceRef.sameMove(sourceEvent.moveUci, trajectory.futureStep.moveUci)
      case PlanCausalDependencyProof.LineAccess(trajectory) =>
        dependency.to == sourceEvent &&
          sourceEvent.identity.actorRole.exists(_.equalsIgnoreCase(trajectory.enabledPieceRole.name)) &&
          EvidenceRef.sameMove(sourceEvent.moveUci, trajectory.enabledStep.moveUci)
      case _ => false
  def directGoalConsequences: List[TransitionConsequence] =
    structuralConsequences.filter(resultAdvancesGoal)
  def directPreparedPawnAdvances: List[(String, String)] =
    directGoalConsequences
      .flatMap(_.subjects)
      .flatMap(StructuralPurposeSubject.pawnAdvanceUnlockedBy(_, rootMove))
      .filterNot((from, to) => EvidenceRef.sameMove(s"${from}${to}", rootMove))
      .distinct
  def preparedPawnAdvanceFiles: List[String] =
    Option
      .when(identity.goalTheme == PlanTheme.PawnBreakPreparation)(
        (
          rootEnablingDependencies.flatMap(_.preparedPawnAdvanceFile) ++
            directPreparedPawnAdvances.map(_._1.take(1))
        ).distinct.sorted
      )
      .getOrElse(Nil)
  private def goalResponseResults(
      causalEpisode: PlanCausalEpisode
  ): List[(PlanCausalResponse, TransitionConsequence)] =
    val enabledTriggers = causalEpisode.rootEnabledSteps.toSet
    causalEpisode.responseResults
      .filter((response, consequence) =>
        enabledTriggers(response.trigger) ||
          coordinatedResponseOwnedByRoot(causalEpisode, response, consequence)
      )
      .filter((response, consequence) =>
        PlanCausalGoalProof.provesInducedResponse(identity, transitionOf(response.trigger), response, consequence)
      )

  private def coordinatedResponseOwnedByRoot(
      causalEpisode: PlanCausalEpisode,
      response: PlanCausalResponse,
      consequence: TransitionConsequence
  ): Boolean =
    identity.goalTheme == PlanTheme.WingPlay &&
      causalEpisode.dependencies.exists(dependency =>
        dependency.from == causalEpisode.root &&
          dependency.coordinatedResponseKing(response).exists(king =>
            directGoalConsequences.exists(kingSafetyAt(_, king)) &&
              kingSafetyAt(consequence, king)
          )
      )

  private def kingSafetyAt(
      consequence: TransitionConsequence,
      king: EvidenceSquare
  ): Boolean =
    consequence.positive &&
      consequence.kind == TransitionConsequenceKind.KingSafetyPressure &&
      consequence.goalSubjects.exists(subject =>
        StructuralPurposeSubject.parse(subject).exists {
          case StructuralPurposeSubject.PieceSquare(piece, square) =>
            piece.equalsIgnoreCase("king") && square.equalsIgnoreCase(king.key)
          case _ => false
        }
      )
  def responseGoalResults: List[(PlanCausalResponse, TransitionConsequence)] =
    (
      episode.toList ++
        realizedBranchWitnesses.flatMap(_.observedEpisode)
    ).flatMap(goalResponseResults).distinct
  def planVerifiedResponseGoalResults: List[(PlanCausalResponse, TransitionConsequence)] =
    if episodePublicProofReady then
      responseGoalResults.filter((response, consequence) =>
        PlanCausalGoalProof.provesOwnedInducedResponse(
          identity.goalTheme,
          transitionOf(response.trigger),
          response,
          consequence
        )
      )
    else
      episode.toList.flatMap(causalEpisode =>
        goalResponseResults(causalEpisode).filter((response, consequence) =>
          PlanCausalGoalProof.provesOwnedInducedResponse(
            identity.goalTheme,
            transitionOf(response.trigger),
            response,
            consequence
          ) &&
            (
              response.trigger == causalEpisode.root && response.plyOffset == 1 ||
                coordinatedResponseOwnedByRoot(causalEpisode, response, consequence)
            )
        )
      ).distinct
  def responseStepDistanceFromPlanStart(response: PlanCausalResponse): Int =
    episode
      .map(causalEpisode => response.step.ply - causalEpisode.root.step.ply)
      .filter(_ >= 0)
      .getOrElse(response.plyOffset)
  def conditionalResponseContinuationResults
      : List[(PlanCausalResponse, PlanCausalEventNode, TransitionConsequence)] =
    episode.toList.flatMap { causalEpisode =>
      causalEpisode.dependencies.flatMap {
        case dependency @ PlanCausalEventDependency(
              from,
              to,
              PlanCausalDependencyKind.ResponseContinuationPrecondition,
              PlanCausalDependencyProof.ResponseContinuation(trajectory: CaptureResponseFollowUpTrajectory),
              _
            ) if from == causalEpisode.root && dependency.planConnectionProven =>
          causalEpisode.responses
            .filter(response => response.trigger == from && response.step == trajectory.replyStep && response.proven)
            .flatMap(response =>
              PlanCausalEpisode
                .resultConsequences(to)
                .filter(consequence =>
            consequence.positive && consequence.strength > 0 && resultAdvancesGoal(to, consequence)
                )
                .map(consequence => (response, to, consequence))
            )
        case _ => Nil
      }
    }.distinct
  private lazy val extendingGoalResults: List[(PlanCausalEventNode, TransitionConsequence)] =
    episode.toList.flatMap { causalEpisode =>
      representativeEventResult.toList.flatMap { case (representativeEvent, _) =>
        causalEpisode.enablingPathTo(representativeEvent).toList.flatMap { representativePath =>
          causalEpisode.continuationsEnabledByRoot.flatMap { sourceEvent =>
            causalEpisode.enablingPathTo(sourceEvent).toList
              .filter(path =>
                path.size > representativePath.size &&
                  path.take(representativePath.size) == representativePath
              )
              .flatMap(_ =>
                PlanCausalEpisode.resultConsequences(sourceEvent)
                  .filter(consequence =>
                    consequence.positive &&
                      consequence.strength > 0 &&
                      resultAdvancesGoal(sourceEvent, consequence)
                  )
                  .map(sourceEvent -> _)
              )
          }
        }
      }
    }.distinct
  def publicTailExpectedResult: Option[(PlanCausalEventNode, TransitionConsequence)] =
    extendingGoalResults
      .sortBy { case (sourceEvent, consequence) =>
        (
          -PlanCausalEpisode.resultSalience(consequence.kind),
          sourceEvent.step.ply,
          -consequence.strength,
          consequence.kind.toString,
          consequence.subjects.map(_.trim.toLowerCase).sorted.mkString(":")
        )
      }
      .headOption
      .orElse(representativeEventResult)
  private lazy val publicTailAssessmentResults: List[(PlanCausalEventNode, TransitionConsequence)] =
    val publicPath = for
      causalEpisode <- episode.toList
      (tailEvent, _) <- publicTailExpectedResult.toList
      path <- causalEpisode.enablingPathTo(tailEvent).toList
    yield path.toSet
    def onPublicPath(sourceEvent: PlanCausalEventNode): Boolean =
      publicPath.exists(_.contains(sourceEvent))
    (
      representativeEventResult.toList.flatMap { case (sourceEvent, _) =>
        PlanCausalEpisode.resultConsequences(sourceEvent)
          .filter(consequence => consequence.positive && consequence.strength > 0)
          .map(sourceEvent -> _)
      } ++ extendingGoalResults.filter((sourceEvent, _) => onPublicPath(sourceEvent))
    ).distinct
  def observedGoalResults: List[(PlanCausalEventNode, TransitionConsequence)] =
    publicTailAssessmentResults.filter((sourceEvent, consequence) =>
      resultAdvancesGoal(sourceEvent, consequence)
    )
  private[chessjudgment] def sourceOwnedMaterialCosts: List[ObservedPlanCost] =
    Option
      .when(continuationSourceLine.isEmpty && opponentResourceDeterrence.isEmpty)(observedMaterialCosts)
      .getOrElse(Nil)
  def materialTradeoffs: List[(ObservedPlanCost, PlanCausalEventNode, TransitionConsequence)] =
    episode.toList.flatMap { causalEpisode =>
      val directResults = directGoalConsequences.map(causalEpisode.root -> _)
      sourceOwnedMaterialCosts.flatMap { cost =>
        val laterResults = observedGoalResults.filter { case (sourceEvent, _) =>
          sourceEvent.step.ply - causalEpisode.root.step.ply > cost.plyOffset
        }
        (directResults ++ laterResults).map { case (sourceEvent, consequence) =>
          (cost, sourceEvent, consequence)
        }
      }
    }.distinct
  def ownedGoalConsequences: List[TransitionConsequence] =
    (
      directGoalConsequences ++
        planVerifiedResponseGoalResults.map(_._2) ++
        conditionalResponseContinuationResults.map(_._3)
    ).distinct
  def rootEnablingDependencies: List[PlanCausalEventDependency] =
    episode.toList.flatMap(causalEpisode =>
      causalEpisode.dependencies.filter(dependency => dependency.from == causalEpisode.root && dependency.enablesContinuation)
    )
  def historyEnablingDependencies: List[PlanCausalEventDependency] =
    episode.toList.flatMap(_.historyDependencies.filter(_.enablesContinuation))
  def counterfactualContinuationProven: Boolean = episode.exists(_.rootEnablesContinuation)
  def requiredHorizonPlyOffset: Int =
    (for
      causalEpisode <- episode
      (sourceEvent, _) <- publicTailExpectedResult
    yield sourceEvent.step.ply - causalEpisode.root.step.ply).getOrElse(0).max(0)
  def expectedReplyCount: Int = BranchReplyProbeBinding.requiredReplyCount(rootTransition.to.fen)
  lazy val provenForwardSequence: Option[List[PlanCausalEventNode]] =
    episode.flatMap { causalEpisode =>
      val replyTestedResult =
        Option
          .when(branchWitnesses.nonEmpty && episodePublicProofReady)(
            canonicalPublicTailAssessment
              .map(_.sourceEvent)
          )
          .flatten
      val inducedResponseResult =
        Option
          .when(causalEpisode.dependencies.exists(_.kind == PlanCausalDependencyKind.ResponseContinuationPrecondition))(
            conditionalResponseContinuationResults
              .sortBy { case (_, sourceEvent, consequence) =>
                (sourceEvent.step.ply, -PlanCausalEpisode.resultSalience(consequence.kind), -consequence.strength)
              }
              .headOption
              .map(_._2)
          )
          .flatten
      replyTestedResult
        .orElse(inducedResponseResult)
        .flatMap(causalEpisode.enablingPathTo)
        .filter(_.size >= 2)
    }
  lazy val planSequenceSummary: Option[PlanSequenceSummary] =
    episode
      .filter(causalEpisode =>
        causalEpisode.historySequenceProven &&
          (
            !causalEpisode.rootEnablesContinuation ||
              directGoalConsequences.exists(consequence => !PlanCausalEpisode.meansOnlyResultKind(consequence.kind))
          )
      )
      .flatMap(causalEpisode =>
        val antecedents = causalEpisode.historicalSequence.dropRight(1)
        PlanContinuity
          .fromAntecedents(
            antecedents.map(event => event.identity -> event.step.ply),
            currentPly = causalEpisode.root.step.ply,
            completionProven = causalEpisode.historicalCompletionProven
          )
          .map(continuity => causalEpisode -> continuity)
      ) match
      case Some((causalEpisode, continuity)) =>
        Some(PlanSequenceSummary(
          transitionType = continuity.episodeTransitionType,
          primaryPlanId = Some(planId),
          previousPlanId = Some(planId),
          continuity = Some(continuity),
          previousEvent = causalEpisode.historicalSequence.dropRight(1).lastOption.map(_.identity),
          currentEvent = Some(identity)
        ))
      case None =>
        for
          causalEpisode <- episode
          sequence <- provenForwardSequence
          continuity <- PlanContinuity.fromEvents(
            sequence.map(event => event.identity -> event.step.ply),
            completionProven = causalEpisode.completionProven
          )
        yield PlanSequenceSummary(
          transitionType = TransitionType.Opening,
          primaryPlanId = Some(planId),
          previousPlanId = None,
          continuity = Some(continuity),
          previousEvent = None,
          currentEvent = Some(identity)
        )
  def realizedBranchWitnesses: List[PlanCausalBranchWitness] =
    branchWitnesses.filter(_.outcome == PlanCausalBranchOutcome.Realized)
  def exactBranchWitnesses: List[PlanCausalBranchWitness] =
    realizedBranchWitnesses.filter(_.realizationMatch.contains(PlanCausalRealizationMatch.ExactMove))
  def equivalentBranchWitnesses: List[PlanCausalBranchWitness] =
    realizedBranchWitnesses.filter(_.realizationMatch.contains(PlanCausalRealizationMatch.EquivalentFunction))
  lazy val causalResultAssessments: List[PlanCausalResultAssessment] =
    episode.toList.flatMap { causalEpisode =>
      publicTailAssessmentResults.map { case (sourceEvent, consequence) =>
        PlanCausalResultAssessment.from(
          causalEpisode,
          sourceEvent,
          consequence,
          branchWitnesses,
          branchSetComplete
        )
      }
    }.distinctBy(assessment => (
      EvidenceRef.normalizeMove(assessment.sourceEvent.moveUci),
      assessment.sourcePlyOffset,
      assessment.consequence.kind,
      assessment.consequence.subjects.map(_.trim.toLowerCase).sorted
    ))
  def positiveCausalResultAssessments: List[PlanCausalResultAssessment] =
    causalResultAssessments.filter(_.positiveProofReady)
  def positiveGoalResultAssessments: List[PlanCausalResultAssessment] =
    positiveCausalResultAssessments.filter(assessment =>
      resultAdvancesGoal(assessment.sourceEvent, assessment.consequence)
    )
  def resolvedCausalResultAssessments: List[PlanCausalResultAssessment] =
    causalResultAssessments.filterNot(assessment =>
      assessment.robustness == PlanCausalRobustness.Untested ||
        assessment.robustness == PlanCausalRobustness.Deferred
    )
  def resolvedGoalResultAssessments: List[PlanCausalResultAssessment] =
    resolvedCausalResultAssessments.filter(assessment =>
      resultAdvancesGoal(assessment.sourceEvent, assessment.consequence)
    )
  def goalDependencyProofReady: Boolean =
    identity.goalTheme == PlanTheme.PawnBreakPreparation &&
      rootEnablingDependencies.exists(_.preparedPawnAdvanceFile.nonEmpty) &&
      (continuationSourceLine.isEmpty || branchCoverageComplete)
  def publicGoalProofReady(
      lines: List[CandidateLineNode],
      graph: TypedEvidenceGraph
  ): Boolean =
    opponentResourceDeterrence.forall(_ => opponentResourceDeterrenceProofReady(lines, graph)) &&
      (
        directGoalConsequences.nonEmpty ||
          planVerifiedResponseGoalResults.nonEmpty ||
          conditionalResponseContinuationResults.nonEmpty ||
          resolvedGoalResultAssessments.nonEmpty ||
          goalDependencyProofReady ||
          developmentChoices.nonEmpty && PlanCausalGoalProof.developmentProves(identity.goalTheme)
      )
  def principalExplanationSortKey: Option[(Int, Int, Int, Int)] =
    principalExplanationSortKeyFor(None)
  def principalExplanationSortKey(
      lines: List[CandidateLineNode],
      graph: TypedEvidenceGraph
  ): Option[(Int, Int, Int, Int)] =
    val deterrenceConsequence =
      opponentResourceDeterrence
        .filter(_ => opponentResourceDeterrenceProofReady(lines, graph))
        .flatMap(_.consequence(perspective, lines, graph))
    principalExplanationSortKeyFor(deterrenceConsequence)
  private def principalExplanationSortKeyFor(
      deterrenceConsequence: Option[TransitionConsequence]
  ): Option[(Int, Int, Int, Int)] =
    val directResults = directGoalConsequences.filter(consequence => consequence.positive && consequence.strength > 0)
    val concreteDirectResults = directResults.filter(consequence =>
      TransitionConsequenceKind.isConcreteGoalResult(consequence.kind)
    )
    val materialTradeoffWitnesses = materialTradeoffs.map { case (cost, _, consequence) =>
      consequence -> cost.plyOffset
    }.filter((consequence, _) => consequence.positive && consequence.strength > 0)
    val materialTradeoffResults = materialTradeoffWitnesses.map(_._1).distinct
    val materialTradeoffDepth = materialTradeoffWitnesses.map(_._2).maxOption.getOrElse(0)
    val futureResultWitnesses = (
      positiveGoalResultAssessments.map(assessment => assessment.consequence -> assessment.sourcePlyOffset) ++
        planVerifiedResponseGoalResults.map((response, consequence) =>
          consequence -> responseStepDistanceFromPlanStart(response)
        ) ++
        conditionalResponseContinuationResults.map((_, sourceEvent, consequence) =>
          consequence -> episode.map(causalEpisode => sourceEvent.step.ply - causalEpisode.root.step.ply).getOrElse(0)
        )
    ).filter((consequence, _) =>
      consequence.positive &&
        consequence.strength > 0
    ).distinct
    val futureResults = futureResultWitnesses.map(_._1).distinct
    val futureCausalDepth = futureResultWitnesses.map(_._2).maxOption.getOrElse(0)
    val developmentProven =
      developmentChoices.nonEmpty && PlanCausalGoalProof.developmentProves(identity.goalTheme)
    val (causalSpecificity, rankedResults, causalDepth) =
      if deterrenceConsequence.nonEmpty then
        (6, deterrenceConsequence.toList, 0)
      else if materialTradeoffResults.nonEmpty then
        (5, materialTradeoffResults, materialTradeoffDepth)
      else if observedVacatedSquareRouteProofReady then
        (6, futureResults, futureCausalDepth)
      else if identity.goalTheme == PlanTheme.PawnBreakPreparation && directPreparedPawnAdvances.size == 1 then
        (5, directResults, 0)
      else if concreteDirectResults.nonEmpty then (5, directResults, 0)
      else if identity.goalTheme == PlanTheme.PawnBreakPreparation && preparedPawnAdvanceFiles.nonEmpty then
        (4, directResults, 0)
      else if
        futureResults.nonEmpty &&
          (
            episodePublicProofReady ||
              planVerifiedResponseGoalResults.nonEmpty ||
              conditionalResponseContinuationResults.nonEmpty
          )
      then
        (4, futureResults, futureCausalDepth)
      else if directResults.nonEmpty then (3, directResults, 0)
      else if developmentProven then (3, Nil, 0)
      else (0, Nil, 0)
    Option.when(causalSpecificity > 0)(
      (
        causalSpecificity,
        rankedResults.map(_.strength).maxOption.getOrElse(0),
        causalDepth,
        rankedResults.map(principalResultSalience).maxOption.getOrElse(0)
      )
    )
  private def principalResultSalience(result: TransitionConsequence): Int =
    if
      result.kind == TransitionConsequenceKind.OpponentMobilityRestriction &&
        identity.goalTheme != PlanTheme.RestrictionProphylaxis
    then 1
    else PlanCausalEpisode.resultSalience(result.kind)
  private def borrowsPreparedPawnClearance: Boolean =
    identity.goalTheme == PlanTheme.PieceRedeployment &&
      directGoalConsequences.nonEmpty &&
      directGoalConsequences.forall(consequence =>
        consequence.kind == TransitionConsequenceKind.LineUnlockGain &&
          consequence.subjects.exists(subject =>
            StructuralPurposeSubject.pawnAdvanceUnlockedBy(subject, rootMove).nonEmpty
          )
      ) &&
      positiveGoalResultAssessments.isEmpty
  private[chessjudgment] def principalCurrentMoveFunctionProofReady: Boolean =
    val directRootResult =
      directGoalConsequences.exists(consequence => consequence.positive && consequence.strength > 0) &&
        !borrowsPreparedPawnClearance
    directRootResult ||
      developmentChoices.nonEmpty && PlanCausalGoalProof.developmentProves(identity.goalTheme) ||
      goalDependencyProofReady ||
      rootActorGoalProofReady ||
      observedVacatedSquareRouteProofReady ||
      materialTradeoffs.exists(_._1.offerCapture.nonEmpty)
  def directPieceRoleProofReady: Boolean =
    identity.goalTheme == PlanTheme.PieceRedeployment &&
      identity.actorRole.exists(role => !role.equalsIgnoreCase(_root_.chess.Pawn.toString)) &&
      !borrowsPreparedPawnClearance &&
      directGoalConsequences.exists(consequence =>
        consequence.positive &&
          consequence.strength > 0 &&
          (
            TransitionConsequenceKind.requiresRootActorSurvival(consequence.kind) ||
              consequence.kind == TransitionConsequenceKind.PieceExchangeAvailable
          )
      )
  private[chessjudgment] def directPawnRoleProofReady: Boolean =
    identity.actorRole.exists(_.equalsIgnoreCase(_root_.chess.Pawn.toString)) &&
      directGoalConsequences.exists(consequence =>
        consequence.positive &&
          consequence.strength > 0 &&
          Set(
            TransitionConsequenceKind.PawnTensionGain,
            TransitionConsequenceKind.PawnTensionResolution,
            TransitionConsequenceKind.SpaceGain,
            TransitionConsequenceKind.PassedPawnProgress,
            TransitionConsequenceKind.PromotionPressureGain
          )(consequence.kind)
      )
  def rootActorGoalProofReady: Boolean =
    directPawnRoleProofReady ||
      representativeGoalResultAssessment.exists(assessment =>
        assessment.positiveProofReady &&
          samePieceRouteProducesResult(assessment.sourceEvent, assessment.consequence)
      )
  def principalOwnedResultKey: Option[(Int, Int, Int)] =
    principalExplanationSortKey.map { case (_, resultStrength, causalDepth, resultSalience) =>
      if borrowsPreparedPawnClearance then (0, 0, 0)
      else (resultStrength, resultSalience, causalDepth)
    }
  def allCausalResultsRobust: Boolean =
    causalResultAssessments.nonEmpty && causalResultAssessments.forall(_.robustness == PlanCausalRobustness.Robust)
  def allCausalResultsRefuted: Boolean =
    causalResultAssessments.nonEmpty && causalResultAssessments.forall(_.robustness == PlanCausalRobustness.Refuted)
  def representativeResultAssessment: Option[PlanCausalResultAssessment] =
    representativeEventResult.flatMap { case (sourceEvent, consequence) =>
      causalResultAssessments.find(assessment => assessment.sourceEvent == sourceEvent && assessment.consequence == consequence)
    }
  def representativeGoalResultAssessment: Option[PlanCausalResultAssessment] =
    representativeResultAssessment.filter(assessment =>
      resultAdvancesGoal(assessment.sourceEvent, assessment.consequence)
    )
  def publicTailExpectedResultAssessment: Option[PlanCausalResultAssessment] =
    publicTailExpectedResult.flatMap { case (sourceEvent, consequence) =>
      causalResultAssessments.find(assessment =>
        assessment.sourceEvent == sourceEvent && assessment.consequence == consequence
      )
    }
  def canonicalPublicTailAssessment: Option[PlanCausalResultAssessment] =
    val representative = representativeGoalResultAssessment.filter(_.positiveProofReady)
    publicTailExpectedResultAssessment
      .filter(_.positiveProofReady)
      .orElse(representative)
      .orElse(positiveGoalResultAssessments.headOption)
  def branchSetComplete: Boolean =
    expectedReplyCount > 0 &&
      branchWitnesses.size == expectedReplyCount &&
      branchWitnesses.map(_.line).distinct.size == branchWitnesses.size &&
      branchWitnesses.map(_.line.rootMove).distinct.size == branchWitnesses.size &&
      branchWitnesses.map(_.sourceProbeId).distinct.size == 1 &&
      branchWitnesses.map(_.certifiedHorizonPlyOffset).distinct.size == 1
  def publicTailCoverageComplete: Boolean =
    branchSetComplete &&
      publicTailExpectedResultAssessment.exists(assessment =>
        assessment.robustness != PlanCausalRobustness.Untested &&
          assessment.robustness != PlanCausalRobustness.Deferred
      )
  def branchCoverageComplete: Boolean =
    publicTailCoverageComplete &&
      causalResultAssessments.nonEmpty &&
      causalResultAssessments.forall(assessment =>
        assessment.robustness != PlanCausalRobustness.Untested &&
          assessment.robustness != PlanCausalRobustness.Deferred
      )
  def observedVacatedSquareRoute: Boolean =
    identity.goalTheme == PlanTheme.PieceRedeployment &&
      representativeGoalResultAssessment.exists(assessment =>
        assessment.consequence.kind == TransitionConsequenceKind.MobilityGain &&
          assessment.consequence.subjects.nonEmpty &&
          episode.exists(causalEpisode =>
            causalEpisode.vacatedSquareLineAccessTo(assessment.sourceEvent).nonEmpty
          )
      )
  def observedVacatedSquareRouteProofReady: Boolean =
    observedVacatedSquareRoute &&
      branchSetComplete &&
      expectedReplyCount == BranchReplyProbeBinding.ReplyMultiPv &&
      representativeGoalResultAssessment.exists(assessment =>
        assessment.robustness == PlanCausalRobustness.Robust &&
          assessment.realizedObservations.size == expectedReplyCount
      )
  def robustness: PlanCausalRobustness =
    representativeResultAssessment.map(_.robustness).getOrElse {
      if branchWitnesses.isEmpty then PlanCausalRobustness.Untested
      else if !branchCoverageComplete then PlanCausalRobustness.Deferred
      else if realizedBranchWitnesses.size == branchWitnesses.size then PlanCausalRobustness.Robust
      else if realizedBranchWitnesses.nonEmpty then PlanCausalRobustness.Conditional
      else if branchWitnesses.forall(witness =>
        witness.outcome == PlanCausalBranchOutcome.Diverted &&
          witness.terminalOutcome.exists(_ != PlanCausalTerminalOutcome.Defeat)
      ) then PlanCausalRobustness.Superseded
      else PlanCausalRobustness.Refuted
    }
  def episodePublicProofReady: Boolean =
    counterfactualContinuationProven &&
      branchCoverageComplete &&
      canonicalPublicTailAssessment.nonEmpty
  def moveRole(
      transitionType: Option[TransitionType],
      lines: List[CandidateLineNode],
      graph: TypedEvidenceGraph
  ): Option[PlanMoveRole] =
    val preventsCounterplay =
      opponentResourceDeterrence match
        case Some(_) => opponentResourceDeterrenceProofReady(lines, graph)
        case None =>
          structuralConsequences.exists(_.kind == TransitionConsequenceKind.OpponentMobilityRestriction)
    val realizesDirectGoal =
      developmentChoices.nonEmpty || directGoalConsequences.exists(_.positive) ||
        planVerifiedResponseGoalResults.nonEmpty || conditionalResponseContinuationResults.nonEmpty
    val directlyPreparesContinuation =
      counterfactualContinuationProven &&
        rootEnablingDependencies.nonEmpty &&
        (
          directGoalConsequences.isEmpty ||
            directGoalConsequences.forall(consequence => PlanCausalEpisode.meansOnlyResultKind(consequence.kind))
        )
    val inducesResponseContinuation =
      episodePublicProofReady &&
        rootEnablingDependencies.exists(_.kind == PlanCausalDependencyKind.ResponseContinuationPrecondition)
    transitionType match
      case Some(TransitionType.ForcedPivot) => Some(PlanMoveRole.Pivot)
      case _ if preventsCounterplay => Some(PlanMoveRole.Prevention)
      case _ if inducesResponseContinuation => Some(PlanMoveRole.Execution)
      case _ if episodePublicProofReady => Some(PlanMoveRole.Preparation)
      case _ if directlyPreparesContinuation => Some(PlanMoveRole.Preparation)
      case Some(TransitionType.Completion) | Some(TransitionType.Opportunistic) => Some(PlanMoveRole.Execution)
      case _ if realizesDirectGoal => Some(PlanMoveRole.Execution)
      case _ => None
  def semanticGroupingAnchors: List[EvidenceSemanticAnchor] =
    List(
      EvidenceSemanticAnchor.of(EvidenceSemanticAnchorKind.PlanPressure, planId.id),
      EvidenceSemanticAnchor.of(
        EvidenceSemanticAnchorKind.PlanCausalEvent,
        identity.goalKey,
        s"root:$rootMove",
        s"actor:${identity.actorRole.getOrElse("unknown")}",
        s"targets:${identity.targets.mkString(",")}",
        s"results:${identity.results.mkString(",")}",
        s"future:${futureMove.getOrElse("none")}"
      )
    )

object PlanCausalGoalProof:
  private val QueensideWingFiles = Set('a', 'b', 'c')
  private val KingsideWingFiles = Set('f', 'g', 'h')

  private def boardWing(file: Char): Option[Set[Char]] =
    if QueensideWingFiles(file) then Some(QueensideWingFiles)
    else if KingsideWingFiles(file) then Some(KingsideWingFiles)
    else None

  private[chessjudgment] def sameBoardWing(firstFile: Char, secondFile: Char): Boolean =
    boardWing(firstFile).exists(_(secondFile))

  private def namedAttackWing(planId: PlanId): Option[Set[Char]] =
    planId match
      case PlanId.QueensideAttack => Some(QueensideWingFiles)
      case PlanId.KingsideAttack  => Some(KingsideWingFiles)
      case _                      => None

  private def subjectWingFiles(subject: String): Set[Char] =
    val carrier = StructuralPurposeSubject.carrierToken(subject)
    (
      Option.when(carrier.matches("[a-h]"))(carrier.head).toList ++
        "[a-h][1-8]".r.findAllIn(carrier).flatMap(_.headOption)
    ).filter(file => boardWing(file).nonEmpty).toSet

  private[chessjudgment] def consequenceOnNamedAttackWing(
      planId: PlanId,
      consequence: TransitionConsequence
  ): Option[TransitionConsequence] =
    namedAttackWing(planId) match
      case None => Some(consequence)
      case Some(expectedFiles) =>
        val matchingSubjects = consequence.goalSubjects.filter { subject =>
          val files = subjectWingFiles(subject)
          files.nonEmpty && files.subsetOf(expectedFiles)
        }
        Option.when(matchingSubjects.nonEmpty)(
          if consequence.targetSubjects.nonEmpty then consequence.copy(targetSubjects = matchingSubjects)
          else consequence.copy(subjects = matchingSubjects)
        )

  def developmentProves(goalTheme: PlanTheme): Boolean =
    goalTheme == PlanTheme.OpeningPrinciples || goalTheme == PlanTheme.PieceRedeployment

  def proves(
      identity: PlanEventIdentity,
      transition: StructuralTransitionBinding,
      consequence: TransitionConsequence
  ): Boolean =
    proves(identity.goalTheme, identity.goalKind, transition, consequence)

  def provesInducedResponse(
      identity: PlanEventIdentity,
      transition: StructuralTransitionBinding,
      response: PlanCausalResponse,
      consequence: TransitionConsequence
  ): Boolean =
    provesInducedResponse(identity.goalTheme, transition, response, consequence)

  def provesInducedResponse(
      goalTheme: PlanTheme,
      transition: StructuralTransitionBinding,
      response: PlanCausalResponse,
      consequence: TransitionConsequence
  ): Boolean =
    val pawnChaseWeakensKing = pawnChaseKingPressure(response, consequence)
    pawnChaseWeakensKing ||
      response.proven && response.capturesPlanPiece &&
        PlanCausalEpisode.triggerMoveCapturesPiece(response.trigger) &&
        (goalTheme match
          case PlanTheme.WeaknessFixation =>
            Set(
              TransitionConsequenceKind.WeakPawnTargetCreated,
              TransitionConsequenceKind.WeakSquareTargetCreated
            )(consequence.kind)
          case PlanTheme.FavorableExchange =>
            Set(
              TransitionConsequenceKind.CenterControlGain,
              TransitionConsequenceKind.PieceExchangeCompleted
            )(consequence.kind)
          case _ =>
            provesAfterInducedResponse(goalTheme, transition, consequence))

  def provesOwnedInducedResponse(
      goalTheme: PlanTheme,
      transition: StructuralTransitionBinding,
      response: PlanCausalResponse,
      consequence: TransitionConsequence
  ): Boolean =
    provesInducedResponse(goalTheme, transition, response, consequence) &&
      (
        !pawnChaseKingPressure(response, consequence) ||
          goalTheme == PlanTheme.WingPlay && wingResultMatchesStartingMove(transition, consequence)
      )

  private def pawnChaseKingPressure(
      response: PlanCausalResponse,
      consequence: TransitionConsequence
  ): Boolean =
    response.proven &&
      response.weakensKingShelter &&
      response.attacksPlanPiece &&
      consequence.kind == TransitionConsequenceKind.KingSafetyPressure

  def provesAfterInducedResponse(
      identity: PlanEventIdentity,
      transition: StructuralTransitionBinding,
      consequence: TransitionConsequence
  ): Boolean =
    provesAfterInducedResponse(identity.goalTheme, transition, consequence)

  private def provesAfterInducedResponse(
      goalTheme: PlanTheme,
      transition: StructuralTransitionBinding,
      consequence: TransitionConsequence
  ): Boolean =
    import TransitionConsequenceKind.*
    goalTheme match
      case PlanTheme.WingPlay =>
        Set(
          OpenFileGain,
          SemiOpenFileGain,
          FileAccessGain,
          LineUnlockGain,
          TargetPressureGain,
          KingRingPressureGain,
          KingSafetyPressure,
          BatteryPressureGain
        )(consequence.kind) && wingResultMatchesStartingMove(transition, consequence)
      case PlanTheme.FavorableExchange =>
        consequence.kind == PieceExchangeCompleted
      case _ =>
        false

  def proves(
      goalTheme: PlanTheme,
      goalKind: Option[PlanKind],
      transition: StructuralTransitionBinding,
      consequence: TransitionConsequence
  ): Boolean =
    import TransitionConsequenceKind.*
    goalTheme match
      case PlanTheme.OpeningPrinciples =>
        Set(
          CenterControlGain,
          DevelopmentLagReduced,
          DevelopmentPieceActivated,
          DevelopmentMobilityGain,
          DevelopmentCenterControlGain,
          DevelopmentSafePlacement
        )(consequence.kind)
      case PlanTheme.RestrictionProphylaxis =>
        consequence.kind == OpponentMobilityRestriction
      case PlanTheme.PieceRedeployment =>
        goalKind match
          case Some(PlanKind.OutpostEntrenchment) =>
            consequence.kind == OutpostGain
          case Some(PlanKind.RookFileTransfer) =>
            Set(FileOccupationGain, RookLiftActivation, BatteryPressureGain)(consequence.kind)
          case Some(PlanKind.WorstPieceImprovement) =>
            consequence.kind match
              case MobilityGain   => consequence.subjects.nonEmpty || consequence.strength > 1
              case LineUnlockGain => consequence.strength > 1
              case resultKind     => Set(BatteryPressureGain, PieceExchangeAvailable)(resultKind)
          case _ =>
            consequence.kind match
              case MobilityGain => consequence.subjects.nonEmpty || consequence.strength > 1
              case resultKind   =>
                Set(FileOccupationGain, OutpostGain, RookLiftActivation, BatteryPressureGain, PieceExchangeAvailable)(resultKind)
      case PlanTheme.WeaknessFixation =>
        Set(WeakPawnTargetCreated, WeakSquareTargetCreated)(consequence.kind) ||
          consequence.kind == TargetPressureGain && consequence.targetSubjects.nonEmpty
      case PlanTheme.PawnBreakPreparation =>
        consequence.kind == PawnTensionGain ||
          consequence.kind == LineUnlockGain && consequence.subjects.exists(
            StructuralPurposeSubject.pawnAdvanceUnlockedBy(_, transition.moveUci).nonEmpty
          )
      case PlanTheme.SpaceClamp =>
        Set(SpaceGain, CenterControlGain, OpponentMobilityRestriction)(consequence.kind)
      case PlanTheme.WingPlay =>
        val actor = transitionActorRole(transition)
        val kindMatchesResult = goalKind match
          case Some(PlanKind.WingExpansion) =>
            actor.contains(Pawn) &&
              Set(SpaceGain, PawnTensionGain, TargetPressureGain, KingRingPressureGain, KingSafetyPressure)(consequence.kind)
          case Some(PlanKind.HookCreation) =>
            actor.contains(Pawn) &&
              Set(PawnTensionGain, TargetPressureGain, KingRingPressureGain, KingSafetyPressure)(consequence.kind)
          case Some(PlanKind.RookLiftScaffold) =>
            actor.contains(Rook) && Set(TargetPressureGain, KingRingPressureGain, RookLiftActivation, BatteryPressureGain)(
              consequence.kind
            )
          case _ =>
            (
              actor.exists(role => role == Pawn || role == Rook) &&
                Set(SpaceGain, PawnTensionGain, TargetPressureGain, KingRingPressureGain, RookLiftActivation, BatteryPressureGain)(
                  consequence.kind
                )
            ) || actor.contains(Pawn) && consequence.kind == KingSafetyPressure
        kindMatchesResult && wingResultMatchesStartingMove(transition, consequence)
      case PlanTheme.AdvantageTransformation =>
        Set(PassedPawnProgress, PromotionPressureGain, FileOccupationGain)(consequence.kind)
      case PlanTheme.FavorableExchange =>
        Set(PieceExchangeAvailable, PieceExchangeCompleted)(consequence.kind)
      case PlanTheme.Unknown =>
        false

  def movedPieceCreatesRouteResult(
      sourceEvent: PlanCausalEventNode,
      consequence: TransitionConsequence
  ): Boolean =
    import TransitionConsequenceKind.*
    val move = EvidenceRef.normalizeMove(sourceEvent.moveUci)
    val destination = move.slice(2, 4)
    val actorRole = sourceEvent.identity.actorRole.map(_.toLowerCase)
    val endpointImproved =
      sourceEvent.structuralConsequences
        .filter(_.kind == MobilityGain)
        .flatMap(_.subjects.flatMap(StructuralDeltaEvidence.movedPieceRoute))
        .exists { case (piece, from, to) =>
          actorRole.contains(piece.toLowerCase) &&
            from.equalsIgnoreCase(move.take(2)) &&
            to.equalsIgnoreCase(destination)
        }
    val movedPieceCreatedBattery =
      consequence.subjects.exists(subject =>
        StructuralPurposeSubject.parse(subject).exists {
          case StructuralPurposeSubject.Battery(_, from, to, roles) =>
            actorRole.exists(role => roles.exists(_.equalsIgnoreCase(role))) &&
              List(from, to).exists(_.equalsIgnoreCase(destination))
          case _ =>
            false
        }
      )
    actorRole.exists(role => !role.equalsIgnoreCase(_root_.chess.Pawn.toString)) &&
      consequence.positive &&
      consequence.strength > 0 &&
      (consequence.kind match
        case MobilityGain =>
          endpointImproved
        case OpponentMobilityRestriction =>
          endpointImproved &&
            StructuralDeltaEvidence.directlyRestrictedOpponentSubjects(consequence).nonEmpty
        case BatteryPressureGain =>
          movedPieceCreatedBattery
        case resultKind =>
          Set(
            TargetPressureGain,
            FileOccupationGain,
            OutpostGain,
            RookLiftActivation,
            PieceExchangeAvailable
          )(resultKind) && consequence.goalSubjects.nonEmpty)

  private def hasPieceRouteResultShape(consequence: TransitionConsequence): Boolean =
    import TransitionConsequenceKind.*
    consequence.positive &&
      consequence.strength > 0 &&
      (consequence.kind match
        case MobilityGain => consequence.subjects.nonEmpty || consequence.strength > 1
        case resultKind =>
          Set(
            TargetPressureGain,
            FileOccupationGain,
            OutpostGain,
            RookLiftActivation,
            BatteryPressureGain,
            PieceExchangeAvailable
          )(resultKind) && consequence.goalSubjects.nonEmpty)

  def ownsDirectPiecePressureAlongsideRoute(
      goalTheme: PlanTheme,
      goalKind: Option[PlanKind],
      transition: StructuralTransitionBinding,
      establishedResults: List[TransitionConsequence],
      candidate: TransitionConsequence
  ): Boolean =
    val establishedRouteResults = establishedResults.filter(result =>
      proves(goalTheme, goalKind, transition, result) && hasPieceRouteResultShape(result)
    )
    goalTheme == PlanTheme.PieceRedeployment &&
      candidate.kind == TransitionConsequenceKind.TargetPressureGain &&
      hasPieceRouteResultShape(candidate) &&
      establishedRouteResults.nonEmpty &&
      candidate.strength >= establishedRouteResults.map(_.strength).max

  def lineAccessAdvancesExchangeGoal(
      goalTheme: PlanTheme,
      establishedResults: List[TransitionConsequence],
      candidate: TransitionConsequence
  ): Boolean =
    goalTheme == PlanTheme.FavorableExchange &&
      candidate.kind == TransitionConsequenceKind.LineUnlockGain &&
      candidate.positive &&
      candidate.strength > 0 &&
      candidate.subjects.nonEmpty &&
      establishedResults.exists(result =>
        result.positive &&
          result.strength > 0 &&
          Set(
            TransitionConsequenceKind.PieceExchangeAvailable,
            TransitionConsequenceKind.PieceExchangeCompleted
          )(result.kind)
      )

  private def wingResultMatchesStartingMove(
      transition: StructuralTransitionBinding,
      consequence: TransitionConsequence
  ): Boolean =
    val move = EvidenceRef.normalizeMove(transition.moveUci)
    val startingWingFiles = List(move.slice(2, 3), move.take(1))
      .flatMap(_.headOption.flatMap(boardWing).toList.flatten)
      .toSet
    val resultWingFiles = consequence.goalSubjects.flatMap(subjectWingFiles).toSet
    resultWingFiles.nonEmpty &&
      startingWingFiles.nonEmpty &&
      startingWingFiles.intersect(resultWingFiles).nonEmpty

  private def transitionActorRole(transition: StructuralTransitionBinding): Option[Role] =
    val origin = EvidenceRef.normalizeMove(transition.moveUci).take(2)
    _root_.chess.format.Fen
      .read(_root_.chess.variant.Standard, _root_.chess.format.Fen.Full(transition.from.fen))
      .flatMap(position => _root_.chess.Square.fromKey(origin).flatMap(position.board.roleAt))

final case class ChessTurn(
    moveNumber: Int,
    side: String,
    notation: String
)

object ChessTurn:
  def fromCompletedPly(completedPly: Int): ChessTurn =
    val halfMove = (completedPly - 1).max(0)
    val moveNumber = halfMove / 2 + 1
    val white = halfMove % 2 == 0
    ChessTurn(moveNumber, if white then "white" else "black", if white then s"$moveNumber." else s"$moveNumber...")

  def fromFenAndOffset(rootFen: String, plyOffset: Int): Option[ChessTurn] =
    for
      position <- _root_.chess.format.Fen.read(
        _root_.chess.variant.Standard,
        _root_.chess.format.Fen.Full(rootFen)
      )
      fullMove <- rootFen.trim.split("\\s+").lift(5).flatMap(_.toIntOption)
    yield
      val rootHalfMove = (fullMove.max(1) - 1) * 2 + (if position.color == Color.White then 0 else 1)
      val halfMove = (rootHalfMove + plyOffset).max(0)
      val moveNumber = halfMove / 2 + 1
      val white = halfMove % 2 == 0
      ChessTurn(moveNumber, if white then "white" else "black", if white then s"$moveNumber." else s"$moveNumber...")

final case class NumberedChessMove(
    uci: String,
    san: String,
    turn: ChessTurn,
    notation: String
)

object NumberedChessMove:
  def fromHistoricalStep(step: LineReplayStep): Option[NumberedChessMove] =
    fromFenWithTurn(step.fenBefore, step.moveUci, Some(ChessTurn.fromCompletedPly(step.ply)))

  def fromStepAtOffset(step: LineReplayStep, rootFen: String, plyOffset: Int): Option[NumberedChessMove] =
    fromFenAtOffset(step.fenBefore, step.moveUci, rootFen, plyOffset)

  def fromFen(fenBefore: String, moveUci: String): Option[NumberedChessMove] =
    fromFenWithTurn(fenBefore, moveUci, ChessTurn.fromFenAndOffset(fenBefore, 0))

  def fromFenAtOffset(
      fenBefore: String,
      moveUci: String,
      rootFen: String,
      plyOffset: Int
  ): Option[NumberedChessMove] =
    fromFenWithTurn(fenBefore, moveUci, ChessTurn.fromFenAndOffset(rootFen, plyOffset))

  private def fromFenWithTurn(
      fenBefore: String,
      moveUci: String,
      publicTurn: Option[ChessTurn]
  ): Option[NumberedChessMove] =
    for
      step <- PrincipalVariationEvidence
        .legalMoveReplay(fenBefore, List(moveUci), startPly = 0)
        .flatMap(_.headOption)
      turn <- publicTurn
    yield
      val san = _root_.chess.format.pgn.Dumper(step.before, step.move, step.after).toString
      NumberedChessMove(EvidenceRef.normalizeMove(moveUci), san, turn, s"${turn.notation}$san")

final case class PlanResult(
    stage: String,
    kind: TransitionConsequenceKind,
    polarity: StructuralSignalPolarity,
    strength: Int,
    subjects: List[String],
    source: Option[NumberedChessMove] = None,
    robustness: Option[PlanCausalRobustness] = None,
    conditions: List[PlanReplyTest] = Nil,
    refutations: List[PlanReplyTest] = Nil,
    supersessions: List[PlanReplyTest] = Nil,
    sourcePlyOffset: Option[Int] = None,
    materialCounterplayPreventionProofReady: Boolean = false
)

object PlanResult:
  def from(
      stage: String,
      consequence: TransitionConsequence,
      source: Option[NumberedChessMove] = None,
      robustness: Option[PlanCausalRobustness] = None,
      conditions: List[PlanReplyTest] = Nil,
      refutations: List[PlanReplyTest] = Nil,
      supersessions: List[PlanReplyTest] = Nil,
      sourcePlyOffset: Option[Int] = None,
      materialCounterplayPreventionProofReady: Boolean = false
  ): PlanResult =
    PlanResult(
      stage = stage,
      kind = consequence.kind,
      polarity = consequence.polarity,
      strength = consequence.strength,
      subjects = consequence.goalSubjects,
      source = source,
      robustness = robustness,
      conditions = conditions,
      refutations = refutations,
      supersessions = supersessions,
      sourcePlyOffset = sourcePlyOffset,
      materialCounterplayPreventionProofReady = materialCounterplayPreventionProofReady
    )

final case class PlanSequenceMove(
    move: String,
    moveReference: Option[NumberedChessMove],
    actorRole: Option[String],
    actorFrom: Option[String],
    actorTo: Option[String],
    dependencyKinds: List[PlanCausalDependencyKind],
    keySquares: List[String] = Nil,
    involvedPieces: List[String] = Nil
)

final case class TestedPlanContinuation(
    dependencyKind: PlanCausalDependencyKind,
    futureMove: String,
    targetSquare: Option[String],
    plyOffset: Int,
    robustness: PlanCausalRobustness,
    realizedReplies: Int,
    exactReplies: Int,
    equivalentReplies: Int,
    testedReplies: Int,
    expectedReplies: Int,
    sequence: List[PlanSequenceMove],
    representativeResult: Option[PlanResult]
)

final case class PlanReplyTest(
    move: String,
    moveReference: Option[NumberedChessMove],
    outcome: PlanCausalBranchOutcome,
    realizationMove: Option[String],
    realizationReference: Option[NumberedChessMove],
    realizationMatch: Option[PlanCausalRealizationMatch],
    observedThrough: Option[ChessTurn],
    terminalOutcome: Option[PlanCausalTerminalOutcome],
    terminalReference: Option[NumberedChessMove],
    realizationPlyOffset: Option[Int] = None,
    observedThroughPlyOffset: Int = 0,
    terminalPlyOffset: Option[Int] = None,
    line: List[NumberedChessMove] = Nil
)

final case class ResolvedPlanEvent(
    goalTheme: String,
    goalKind: Option[String],
    moveRole: Option[PlanMoveRole],
    transitionType: Option[TransitionType],
    rootMove: String,
    actorRole: Option[String],
    actorFrom: Option[String],
    actorTo: Option[String],
    targets: List[String],
    developmentChoices: List[StructuralDevelopmentChoice],
    results: List[PlanResult],
    responses: List[PlanReplyTest],
    testedContinuation: Option[TestedPlanContinuation],
    observedMainLine: List[PlanSequenceMove] = Nil,
    historySequence: List[PlanSequenceMove] = Nil,
    rootMoveReference: Option[NumberedChessMove] = None,
    rootDependencyKinds: List[PlanCausalDependencyKind] = Nil,
    preparedPawnAdvanceFiles: List[String] = Nil,
    representativeResult: Option[PlanResult] = None,
    materialCostSquares: List[String] = Nil,
    observedVacatedSquareRoute: Boolean = false
)

object ResolvedPlanEvent:
  private def logicalPlanResultKey(result: PlanResult) =
    (
      result.stage,
      result.kind,
      result.polarity,
      result.subjects.sorted,
      result.source.map(source => (source.uci, source.turn.moveNumber, source.turn.side))
    )

  private[judgment] def mergePlanResults(results: List[PlanResult]): List[PlanResult] =
    results.foldLeft(List.empty[PlanResult]) { (merged, result) =>
      val index = merged.indexWhere(existing => logicalPlanResultKey(existing) == logicalPlanResultKey(result))
      if index < 0 then merged :+ result
      else
        val existing = merged(index)
        merged.updated(index, existing.copy(
          conditions = (existing.conditions ++ result.conditions).distinct,
          refutations = (existing.refutations ++ result.refutations).distinct,
          supersessions = (existing.supersessions ++ result.supersessions).distinct
        ))
    }

  private def consequenceTargetTokens(
      identity: PlanEventIdentity,
      consequence: TransitionConsequence
  ): Set[String] =
    if consequence.kind == TransitionConsequenceKind.FileOccupationGain then
      consequence.subjects
        .flatMap(StructuralPurposeSubject.fileSquareTarget)
        .map((file, _) => s"file:$file")
        .toSet
    else
      (
        PlanCausalEpisode.goalTargetSubjects(consequence).map(_.trim.toLowerCase) ++
          PlanCausalEpisode.consequenceTargetSquares(identity, consequence).map(square => s"square:${square.key.toLowerCase}")
      ).filter(_.nonEmpty).toSet

  def from(event: PlanCausalEventEvidence): ResolvedPlanEvent =
    from(event, Nil, TypedEvidenceGraph.empty)

  def from(
      event: PlanCausalEventEvidence,
      lines: List[CandidateLineNode]
  ): ResolvedPlanEvent =
    from(event, lines, TypedEvidenceGraph.empty)

  def from(
      event: PlanCausalEventEvidence,
      lines: List[CandidateLineNode],
      graph: TypedEvidenceGraph
  ): ResolvedPlanEvent =
    val canonicalDeterrenceProofReady =
      event.opponentResourceDeterrenceProofReady(lines, graph)
    val canonicalDeterrence = event.opponentResourceDeterrence
      .filter(_ => canonicalDeterrenceProofReady)
      .flatMap(proof =>
        proof.consequence(event.perspective, lines, graph).map(consequence => proof -> consequence)
      )
    val responseResults = event.planVerifiedResponseGoalResults.sortBy { case (response, consequence) =>
      (
        event.responseStepDistanceFromPlanStart(response),
        -PlanCausalEpisode.resultSalience(consequence.kind),
        -consequence.strength
      )
    }
    val conditionalContinuationResults = Option
      .unless(event.episodePublicProofReady)(event.conditionalResponseContinuationResults)
      .getOrElse(Nil)
      .sortBy { case (_, sourceEvent, consequence) =>
        (sourceEvent.step.ply, -PlanCausalEpisode.resultSalience(consequence.kind), -consequence.strength)
      }
    val testedContinuation = TestedPlanContinuation.from(event, lines, graph)
    val observedMainLine = observedMainLineFrom(event)
    val historySequence = historicalSequence(event)
    val directSource = event.episode
      .flatMap(episode => NumberedChessMove.fromStepAtOffset(episode.root.step, event.rootTransition.from.fen, 0))
      .orElse(NumberedChessMove.fromFen(event.rootTransition.from.fen, event.rootMove))
    val canonicalFuture = event.canonicalPublicTailAssessment.toList
    val branchResolvedFuture = Option
      .when(event.branchCoverageComplete)(event.resolvedGoalResultAssessments)
      .getOrElse(Nil)
    val resolvedFuture = (
      canonicalFuture ++
        branchResolvedFuture
    ).distinct
      .sortBy(assessment =>
        (assessment.sourcePlyOffset, -PlanCausalEpisode.resultSalience(assessment.consequence.kind), -assessment.consequence.strength)
      )
    val positiveFuture = Option.when(event.episodePublicProofReady)(event.positiveGoalResultAssessments).getOrElse(Nil)
    val completedExchangeObserved =
      responseResults.exists(_._2.kind == TransitionConsequenceKind.PieceExchangeCompleted) ||
        conditionalContinuationResults.exists(_._3.kind == TransitionConsequenceKind.PieceExchangeCompleted) ||
        resolvedFuture.exists(_.consequence.kind == TransitionConsequenceKind.PieceExchangeCompleted) ||
        positiveFuture.exists(_.consequence.kind == TransitionConsequenceKind.PieceExchangeCompleted)
    val directConsequences = event.directGoalConsequences
      .filter(consequence =>
        event.opponentResourceDeterrence.isEmpty ||
          consequence.kind != TransitionConsequenceKind.OpponentMobilityRestriction ||
          canonicalDeterrence.exists(_._2 == consequence)
      )
      .filterNot(consequence =>
        completedExchangeObserved && consequence.kind == TransitionConsequenceKind.PieceExchangeAvailable
      )
      .sortBy(consequence =>
        (-PlanCausalEpisode.resultSalience(consequence.kind), -consequence.strength)
      )
    val responseConditions = responseResults.map { case (response, _) =>
      observedResponseFrom(event, response, None)
    }
    val continuationConditions = conditionalContinuationResults.map { case (response, sourceEvent, _) =>
      observedResponseFrom(event, response, Some(sourceEvent))
    }
    val deterrenceConditions = canonicalDeterrence.toList.flatMap { case (proof, _) =>
      deterrenceResponseFrom(event, proof, lines, graph)
    }
    val publicResults = mergePlanResults(
      directConsequences.map(consequence =>
        PlanResult.from(
          "direct",
          consequence,
          source = directSource,
          robustness = Option.when(canonicalDeterrence.exists(_._2 == consequence))(
            PlanCausalRobustness.Conditional
          ),
          conditions = deterrenceConditions.filter(_ => canonicalDeterrence.exists(_._2 == consequence)),
          sourcePlyOffset = Some(0),
          materialCounterplayPreventionProofReady =
            event.materialCounterplayPreventionProofReady(lines, graph) &&
            canonicalDeterrence.exists(_._2 == consequence)
        )
      ) ++
        responseResults.map { case (response, consequence) =>
          val condition = observedResponseFrom(event, response, None)
          val responseStepDistance = event.responseStepDistanceFromPlanStart(response)
          PlanResult.from(
            "response",
            consequence,
            source = NumberedChessMove.fromStepAtOffset(
              response.step,
              event.rootTransition.from.fen,
              responseStepDistance
            ),
            robustness = Some(PlanCausalRobustness.Conditional),
            conditions = List(condition),
            sourcePlyOffset = Some(responseStepDistance)
          )
        } ++
        conditionalContinuationResults.map { case (response, sourceEvent, consequence) =>
          val sourcePlyOffset = event.episode.fold(0)(episode => sourceEvent.step.ply - episode.root.step.ply)
          val condition = observedResponseFrom(event, response, Some(sourceEvent))
          PlanResult.from(
            "future",
            consequence,
            source = NumberedChessMove.fromStepAtOffset(
              sourceEvent.step,
              event.rootTransition.from.fen,
              sourcePlyOffset
            ),
            robustness = Some(PlanCausalRobustness.Conditional),
            conditions = List(condition),
            sourcePlyOffset = Some(sourcePlyOffset)
          )
        } ++
        resolvedFuture.map(assessment => resultFromAssessment(event, assessment))
    )
    def publicResultFor(sourceEvent: PlanCausalEventNode, consequence: TransitionConsequence): Option[PlanResult] =
      val sourcePlyOffset = event.episode.map(episode => sourceEvent.step.ply - episode.root.step.ply).getOrElse(0)
      publicResults.find(result =>
        result.kind == consequence.kind &&
          result.polarity == consequence.polarity &&
          result.subjects.sorted == consequence.goalSubjects.sorted &&
          result.sourcePlyOffset.contains(sourcePlyOffset)
      )
    val representativeResult = event.canonicalPublicTailAssessment
      .flatMap(assessment => publicResultFor(assessment.sourceEvent, assessment.consequence))
      .orElse(
        event.representativeDirectGoalConsequence.flatMap(consequence =>
          publicResults.find(result =>
            result.stage == "direct" &&
              result.kind == consequence.kind &&
              result.polarity == consequence.polarity &&
              result.subjects.sorted == consequence.goalSubjects.sorted
          )
        )
      )
    val targets = (
      directConsequences
        .filterNot(consequence => PlanCausalEpisode.meansOnlyResultKind(consequence.kind))
        .flatMap(consequence => consequenceTargetTokens(event.identity, consequence)) ++
        responseResults
          .map(_._2)
          .filterNot(consequence => PlanCausalEpisode.meansOnlyResultKind(consequence.kind))
          .flatMap(consequence => consequenceTargetTokens(event.identity, consequence)) ++
        conditionalContinuationResults
          .filterNot((_, _, consequence) => PlanCausalEpisode.meansOnlyResultKind(consequence.kind))
          .flatMap((_, sourceEvent, consequence) => consequenceTargetTokens(sourceEvent.identity, consequence)) ++
        positiveFuture
          .filterNot(assessment => PlanCausalEpisode.meansOnlyResultKind(assessment.consequence.kind))
          .flatMap(assessment => consequenceTargetTokens(assessment.sourceEvent.identity, assessment.consequence))
    )
      .distinct
      .sorted
    val representativeAssessment = Option
      .when(event.episodePublicProofReady)(event.canonicalPublicTailAssessment)
      .flatten
      .orElse(positiveFuture.headOption)
      .orElse(branchResolvedFuture.headOption)
    val pathResponses = representativeAssessment.toList.flatMap { assessment =>
      event.episode.toList.flatMap { causalEpisode =>
        causalEpisode.enablingDependenciesTo(assessment.sourceEvent).flatMap {
          case dependency @ PlanCausalEventDependency(
                from,
                to,
                PlanCausalDependencyKind.ResponseContinuationPrecondition,
                PlanCausalDependencyProof.ResponseContinuation(trajectory),
                _
              ) if from != causalEpisode.root && dependency.planConnectionProven =>
            causalEpisode.responses
              .filter(response => response.trigger == from && response.step == trajectory.replyStep && response.proven)
              .map(response => observedResponseFrom(event, response, Some(to)))
          case _ => Nil
        }
      }
    }
    val transitionType = event.planSequenceSummary.map(_.transitionType)
    ResolvedPlanEvent(
      goalTheme = event.identity.goalTheme.id,
      goalKind = event.identity.goalKind.map(_.id),
      moveRole = event.moveRole(transitionType, lines, graph),
      transitionType = transitionType,
      rootMove = event.rootMove,
      actorRole = event.identity.actorRole,
      actorFrom = event.identity.actorFrom,
      actorTo = event.identity.actorTo,
      targets = targets,
      developmentChoices = event.developmentChoices,
      results = publicResults,
      responses = (
        responseConditions ++ continuationConditions ++ deterrenceConditions ++ pathResponses ++
          representativeAssessment.toList.flatMap(_.observations.map(responseFrom(event, _)))
      ).distinct,
      testedContinuation = testedContinuation,
      observedMainLine = observedMainLine,
      historySequence = historySequence,
      rootMoveReference = directSource,
      rootDependencyKinds = (
        event.rootEnablingDependencies ++ event.episode.toList.flatMap(causalEpisode =>
          event.historyEnablingDependencies.filter(_.to == causalEpisode.root)
        )
      ).map(_.kind).distinct,
      preparedPawnAdvanceFiles = event.preparedPawnAdvanceFiles,
      representativeResult = representativeResult,
      materialCostSquares = event.sourceOwnedMaterialCosts.map(_.capture.square.key).distinct,
      observedVacatedSquareRoute = event.observedVacatedSquareRoute
    )

  private def observedMainLineFrom(event: PlanCausalEventEvidence): List[PlanSequenceMove] =
    if !event.goalDependencyProofReady || event.continuationSourceLine.nonEmpty then Nil
    else
      event.episode.toList.flatMap { causalEpisode =>
        event.rootEnablingDependencies
          .filter(_.preparedPawnAdvanceFile.nonEmpty)
          .map(_.to)
          .distinct match
          case destination :: Nil =>
            causalEpisode.enablingPathTo(destination).toList.flatMap(path =>
              planSequence(
                event,
                causalEpisode.root.step.ply,
                path,
                causalEpisode.enablingDependenciesTo(destination)
              )
            )
          case _ => Nil
      }

  private[judgment] def fromMaterialTradeoffs(
      event: PlanCausalEventEvidence,
      tradeoffs: List[(ObservedPlanCost, PlanCausalEventNode, TransitionConsequence)]
  ): ResolvedPlanEvent =
    val base = from(event)
    val futureTradeoffs = tradeoffs.filter { case (cost, resultEvent, _) =>
      event.episode.exists(episode => resultEvent.step.ply - episode.root.step.ply > cost.plyOffset)
    }
    val responses = futureTradeoffs
      .map(_._1)
      .distinct
      .sortBy(cost => (cost.plyOffset, cost.capture.square.key, cost.responseStep.moveUci))
      .map { cost =>
        val futureResult = futureTradeoffs
          .collect { case (`cost`, resultEvent, _) => resultEvent }
          .sortBy(_.step.ply)
          .headOption
        observedMaterialCostResponseFrom(event, cost, futureResult)
      }
    val futureResults = futureTradeoffs
      .groupBy { case (_, resultEvent, consequence) => (resultEvent, consequence) }
      .toList
      .sortBy { case ((resultEvent, consequence), _) =>
        (resultEvent.step.ply, consequence.kind.toString, consequence.anchorKey)
      }
      .map { case ((resultEvent, consequence), relatedTradeoffs) =>
        val sourcePlyOffset = event.episode.fold(0)(episode => resultEvent.step.ply - episode.root.step.ply)
        val conditions = relatedTradeoffs
          .map { case (cost, _, _) => observedMaterialCostResponseFrom(event, cost, Some(resultEvent)) }
          .distinct
          .sortBy(response => (response.moveReference.map(_.turn.moveNumber).getOrElse(Int.MaxValue), response.move))
        PlanResult.from(
          stage = "response",
          consequence = consequence,
          source = NumberedChessMove.fromStepAtOffset(
            resultEvent.step,
            event.rootTransition.from.fen,
            sourcePlyOffset
          ),
          robustness = Some(PlanCausalRobustness.Conditional),
          conditions = conditions,
          sourcePlyOffset = Some(sourcePlyOffset)
        )
      }
    val futureTargets = futureTradeoffs
      .filterNot((_, _, consequence) => PlanCausalEpisode.meansOnlyResultKind(consequence.kind))
      .flatMap((_, resultEvent, consequence) => consequenceTargetTokens(resultEvent.identity, consequence))
    base.copy(
      targets = (base.targets ++ futureTargets).distinct.sorted,
      results = mergePlanResults(base.results ++ futureResults),
      responses = (base.responses ++ responses).distinct
    )

  private def historicalSequence(event: PlanCausalEventEvidence): List[PlanSequenceMove] =
    event.episode
      .filter(_.historySequenceProven)
      .toList
      .flatMap { causalEpisode =>
        planSequence(
          event,
          causalEpisode.root.step.ply,
          causalEpisode.historicalSequence,
          causalEpisode.historyDependencies,
          useHistoricalTurns = true
        )
      }

  private[judgment] def planSequence(
      event: PlanCausalEventEvidence,
      rootPly: Int,
      path: List[PlanCausalEventNode],
      dependencies: List[PlanCausalEventDependency],
      useHistoricalTurns: Boolean = false
  ): List[PlanSequenceMove] =
    path.map { node =>
      val incoming = dependencies.filter(_.to == node)
      PlanSequenceMove(
        move = node.moveUci,
        moveReference =
          if useHistoricalTurns && node.step.ply < rootPly then NumberedChessMove.fromHistoricalStep(node.step)
          else
            NumberedChessMove.fromStepAtOffset(
              node.step,
              event.rootTransition.from.fen,
              node.step.ply - rootPly
            ),
        actorRole = node.identity.actorRole,
        actorFrom = node.identity.actorFrom,
        actorTo = node.identity.actorTo,
        dependencyKinds = incoming.map(_.kind).distinct,
        keySquares = incoming.flatMap(_.proofSquares).map(_.key.toLowerCase).distinct,
        involvedPieces = incoming.flatMap(_.proofPieceRoles).map(_.name.toLowerCase).distinct
      )
    }

  private def observedResponseFrom(
      event: PlanCausalEventEvidence,
      response: PlanCausalResponse,
      realization: Option[PlanCausalEventNode]
  ): PlanReplyTest =
    val realizationPlyOffset = for
      sourceEvent <- realization
      episode <- event.episode
    yield sourceEvent.step.ply - episode.root.step.ply
    val responseStepDistance = event.responseStepDistanceFromPlanStart(response)
    val observedThroughPlyOffset = realizationPlyOffset.getOrElse(responseStepDistance)
    val responseReference = NumberedChessMove.fromStepAtOffset(
      response.step,
      event.rootTransition.from.fen,
      responseStepDistance
    )
    val responseTriggerLine = event.episode.toList
      .filter(_.root != response.trigger)
      .flatMap(causalEpisode =>
        List(
          NumberedChessMove.fromStepAtOffset(
            response.trigger.step,
            event.rootTransition.from.fen,
            response.trigger.step.ply - causalEpisode.root.step.ply
          ),
          responseReference
        ).flatten
      )
    PlanReplyTest(
      move = response.step.moveUci,
      moveReference = responseReference,
      outcome = PlanCausalBranchOutcome.Realized,
      realizationMove = realization.map(_.moveUci),
      realizationReference = for
        sourceEvent <- realization
        offset <- realizationPlyOffset
        reference <- NumberedChessMove.fromStepAtOffset(sourceEvent.step, event.rootTransition.from.fen, offset)
      yield reference,
      realizationMatch = realization.map(_ => PlanCausalRealizationMatch.ExactMove),
      observedThrough = ChessTurn.fromFenAndOffset(event.rootTransition.from.fen, observedThroughPlyOffset),
      terminalOutcome = None,
      terminalReference = None,
      realizationPlyOffset = realizationPlyOffset,
      observedThroughPlyOffset = observedThroughPlyOffset,
      line = responseTriggerLine
    )

  private[judgment] def deterrenceResponseFrom(
      event: PlanCausalEventEvidence,
      proof: OpponentResourceDeterrenceProof,
      lines: List[CandidateLineNode],
      graph: TypedEvidenceGraph
  ): Option[PlanReplyTest] =
    for
      resourceSequence <- proof.resourceSequence(event.perspective, lines, graph)
      resourceStep <- resourceSequence.headOption
      responseStep <- resourceSequence.lift(1)
      materialResultStep <- resourceSequence.lastOption
    yield
      val realizationPlyOffset = responseStep.ply - event.rootTransition.from.ply
      val observedThroughPlyOffset = materialResultStep.ply - event.rootTransition.from.ply
      PlanReplyTest(
        move = resourceStep.moveUci,
        moveReference = NumberedChessMove.fromStepAtOffset(
          resourceStep,
          event.rootTransition.from.fen,
          resourceStep.ply - event.rootTransition.from.ply
        ),
        outcome = PlanCausalBranchOutcome.Realized,
        realizationMove = Some(responseStep.moveUci),
        realizationReference = NumberedChessMove.fromStepAtOffset(
          responseStep,
          event.rootTransition.from.fen,
          realizationPlyOffset
        ),
        realizationMatch = Some(PlanCausalRealizationMatch.ExactMove),
        observedThrough = ChessTurn.fromFenAndOffset(event.rootTransition.from.fen, observedThroughPlyOffset),
        terminalOutcome = None,
        terminalReference = None,
        realizationPlyOffset = Some(realizationPlyOffset),
        observedThroughPlyOffset = observedThroughPlyOffset,
        line = resourceSequence.flatMap(step =>
          NumberedChessMove.fromStepAtOffset(
            step,
            event.rootTransition.from.fen,
            step.ply - event.rootTransition.from.ply
          )
        )
      )

  private def observedMaterialCostResponseFrom(
      event: PlanCausalEventEvidence,
      cost: ObservedPlanCost,
      realization: Option[PlanCausalEventNode]
  ): PlanReplyTest =
    val realizationPlyOffset = for
      resultEvent <- realization
      episode <- event.episode
    yield resultEvent.step.ply - episode.root.step.ply
    PlanReplyTest(
      move = cost.responseStep.moveUci,
      moveReference = NumberedChessMove.fromStepAtOffset(
        cost.responseStep,
        event.rootTransition.from.fen,
        cost.plyOffset
      ),
      outcome = PlanCausalBranchOutcome.Realized,
      realizationMove = realization.map(_.moveUci),
      realizationReference = for
        resultEvent <- realization
        offset <- realizationPlyOffset
        reference <- NumberedChessMove.fromStepAtOffset(resultEvent.step, event.rootTransition.from.fen, offset)
      yield reference,
      realizationMatch = realization.map(_ => PlanCausalRealizationMatch.ExactMove),
      observedThrough = realizationPlyOffset.flatMap(offset =>
        ChessTurn.fromFenAndOffset(event.rootTransition.from.fen, offset)
      ),
      terminalOutcome = None,
      terminalReference = None,
      realizationPlyOffset = realizationPlyOffset,
      observedThroughPlyOffset = realizationPlyOffset.getOrElse(cost.plyOffset)
    )

  private[judgment] def resultFromAssessment(
      event: PlanCausalEventEvidence,
      assessment: PlanCausalResultAssessment
  ): PlanResult =
    val responses = assessment.observations.map(responseFrom(event, _))
    PlanResult.from(
      stage = "future",
      consequence = assessment.consequence,
      source = NumberedChessMove.fromStepAtOffset(
        assessment.sourceEvent.step,
        event.rootTransition.from.fen,
        assessment.sourcePlyOffset
      ),
      robustness = Some(assessment.robustness),
      conditions = responses.filter(_.outcome == PlanCausalBranchOutcome.Realized),
      refutations = responses.filter(response =>
        response.outcome == PlanCausalBranchOutcome.Refuted ||
          response.outcome == PlanCausalBranchOutcome.Diverted && response.terminalOutcome.isEmpty
      ),
      supersessions = responses.filter(response =>
        response.outcome == PlanCausalBranchOutcome.Diverted && response.terminalOutcome.nonEmpty
      ),
      sourcePlyOffset = Some(assessment.sourcePlyOffset)
    )

  private def responseFrom(
      event: PlanCausalEventEvidence,
      observation: PlanCausalResultObservation
  ): PlanReplyTest =
    val realizationPlyOffset = event.episode.flatMap(episode => observation.realizationPlyOffset(episode.root))
    PlanReplyTest(
      move = observation.replyMove,
      moveReference = NumberedChessMove.fromFenAtOffset(
        event.rootTransition.to.fen,
        observation.replyMove,
        event.rootTransition.from.fen,
        1
      ),
      outcome = observation.outcome,
      realizationMove = observation.realizationMove,
      realizationReference = for
        node <- observation.realizationEvent
        offset <- realizationPlyOffset
        reference <- NumberedChessMove.fromStepAtOffset(node.step, event.rootTransition.from.fen, offset)
      yield reference,
      realizationMatch = observation.realizationMatch,
      observedThrough = ChessTurn.fromFenAndOffset(event.rootTransition.from.fen, observation.observedThroughPlyOffset),
      terminalOutcome = observation.terminalOutcome,
      terminalReference = for
        step <- observation.terminalStep
        offset <- observation.terminalPlyOffset
        reference <- NumberedChessMove.fromStepAtOffset(step, event.rootTransition.from.fen, offset)
      yield reference,
      realizationPlyOffset = realizationPlyOffset,
      observedThroughPlyOffset = observation.observedThroughPlyOffset,
      terminalPlyOffset = observation.terminalPlyOffset
    )

object TestedPlanContinuation:
  def from(event: PlanCausalEventEvidence): Option[TestedPlanContinuation] =
    from(event, Nil, TypedEvidenceGraph.empty)

  def from(
      event: PlanCausalEventEvidence,
      lines: List[CandidateLineNode]
  ): Option[TestedPlanContinuation] =
    from(event, lines, TypedEvidenceGraph.empty)

  def from(
      event: PlanCausalEventEvidence,
      lines: List[CandidateLineNode],
      graph: TypedEvidenceGraph
  ): Option[TestedPlanContinuation] =
    (for
      _ <- Option.when(event.episodePublicProofReady)(())
      episode <- event.episode
      assessment <- event.canonicalPublicTailAssessment
      resultEvent = assessment.sourceEvent
      path <- event.provenForwardSequence.filter(_.lastOption.contains(resultEvent))
      nextEvent <- path.drop(1).headOption
      pathDependencies = episode.enablingDependenciesTo(resultEvent)
      dependency <- pathDependencies.headOption
    yield TestedPlanContinuation(
      dependencyKind = dependency.kind,
      futureMove = nextEvent.moveUci,
      targetSquare = PlanCausalEpisode
        .consequenceTargetSquares(resultEvent.identity, assessment.consequence) match
          case target :: Nil => Some(target.key)
          case _             => None,
      plyOffset = nextEvent.step.ply - episode.root.step.ply,
      robustness = assessment.robustness,
      realizedReplies = assessment.realizedObservations.size,
      exactReplies = assessment.realizedObservations.count(_.realizationMatch.contains(PlanCausalRealizationMatch.ExactMove)),
      equivalentReplies = assessment.realizedObservations.count(
        _.realizationMatch.contains(PlanCausalRealizationMatch.EquivalentFunction)
      ),
      testedReplies = assessment.observations.size,
      expectedReplies = event.expectedReplyCount,
      sequence = ResolvedPlanEvent.planSequence(event, episode.root.step.ply, path, pathDependencies),
      representativeResult = Some(ResolvedPlanEvent.resultFromAssessment(event, assessment))
    )).orElse(fromOpponentResourceDeterrence(event, lines, graph))

  private def fromOpponentResourceDeterrence(
      event: PlanCausalEventEvidence,
      lines: List[CandidateLineNode],
      graph: TypedEvidenceGraph
  ): Option[TestedPlanContinuation] =
    for
      proof <- event.opponentResourceDeterrence
      if event.opponentResourceDeterrenceProofReady(lines, graph)
      episode <- event.episode
      materialGain <- proof.materialGain(event.perspective, lines, graph)
      resultEvent <- episode.continuationsEnabledByRoot.find(node =>
        EvidenceRef.sameMove(node.moveUci, materialGain.moveUci)
      )
      path <- episode.enablingPathTo(resultEvent).filter(_.size >= 2)
      nextEvent <- path.drop(1).headOption
      pathDependencies = episode.enablingDependenciesTo(resultEvent)
      dependency <- pathDependencies.headOption
      consequence <- proof.consequence(event.perspective, lines, graph)
      condition <- ResolvedPlanEvent.deterrenceResponseFrom(event, proof, lines, graph)
    yield TestedPlanContinuation(
      dependencyKind = dependency.kind,
      futureMove = nextEvent.moveUci,
      targetSquare = Some(materialGain.square.key.toLowerCase),
      plyOffset = nextEvent.step.ply - episode.root.step.ply,
      robustness = PlanCausalRobustness.Conditional,
      realizedReplies = 1,
      exactReplies = 1,
      equivalentReplies = 0,
      testedReplies = 1,
      expectedReplies = 1,
      sequence = ResolvedPlanEvent.planSequence(event, episode.root.step.ply, path, pathDependencies),
      representativeResult = Some(
        PlanResult.from(
          stage = "direct",
          consequence = consequence,
          source = NumberedChessMove.fromStepAtOffset(episode.root.step, event.rootTransition.from.fen, 0),
          robustness = Some(PlanCausalRobustness.Conditional),
          conditions = List(condition),
          sourcePlyOffset = Some(0)
        )
      )
    )

final case class PlanPressureEvidence(
    activePlans: ActivePlans,
    alignment: Option[PlanAlignment]
) extends EvidencePayload:
  def evidenceBackedPlans: List[PlanMatch] =
    activePlans.allPlans.filter(_.evidence.nonEmpty).distinctBy(_.plan.id)

  def rootBackedPlans(rootMove: Option[String]): List[PlanMatch] =
    rootMove.toList.flatMap(move =>
      evidenceBackedPlans.filter(
        _.evidence.exists(atom => atom.motif.move.exists(EvidenceRef.sameMove(_, move)))
      )
    )

  def activePlanIds(rootMove: Option[String]): List[PlanId] =
    rootBackedPlans(rootMove).map(_.plan.id)

final case class CandidateComparisonEvidence(
    comparison: CandidateComparisonFact
) extends EvidencePayload

final case class RelativeAssessmentEvidence(
    assessment: RelativeMoveAssessment
) extends EvidencePayload

final case class RelativeCauseFactEvidence(
    cause: RelativeCauseFact
) extends EvidencePayload

final case class EvidenceRecord(
    ref: EvidenceRef,
    payload: EvidencePayload,
    parents: List[EvidenceRef] = Nil
):
  def payloadLineRefs: List[LineNodeRef] =
    payload match
      case lineFact: LineFactEvidence =>
        List(lineFact.line)
      case payload: PlanCausalEventEvidence =>
        payload.rootLine :: (
          payload.branchWitnesses.map(_.line) ++
            payload.opponentResourceDeterrence.toList.flatMap(proof =>
              proof.resourceLine :: proof.comparisons.map(_.resourceLine)
            )
        )
      case EvalFactEvidence(payloadLine, _, _, _) =>
        List(payloadLine)
      case CandidateComparisonEvidence(fact) =>
        List(fact.referenceLine, fact.candidateLine)
      case RelativeAssessmentEvidence(assessment) =>
        List(assessment.reference.ref, assessment.candidate.ref)
      case RelativeCauseFactEvidence(_) =>
        ref.line.toList
      case payload: TacticalMechanismEvidence =>
        payload.line.toList
      case _: StrategicMechanismEvidence =>
        ref.line.toList
      case payload: StrategicMechanismContrastEvidence =>
        List(payload.referenceLine, payload.candidateLine)
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

final class TypedEvidenceGraph private (
    val records: List[EvidenceRecord]
):
  lazy val byId: Map[String, EvidenceRecord] =
    records.map(record => record.ref.id -> record).toMap

  def record(ref: EvidenceRef): Option[EvidenceRecord] =
    byId.get(ref.id).filter(_.ref == ref)

  def recordsFor(position: PositionNodeRef): List[EvidenceRecord] =
    records.filter(_.ref.position == position)

  def recordsFor(line: LineNodeRef): List[EvidenceRecord] =
    records.filter(_.ref.line.contains(line))

  def relativeCauseProofRecords(section: RelativeCauseProofSection): List[EvidenceRecord] =
    section.sourceRefs.flatMap(ref => record(ref)).distinctBy(_.ref.id)

  def relativeCauseBoardAnchorKinds(section: RelativeCauseProofSection): List[(EvidenceRef, BoardAnchorKind)] =
    relativeCauseProofRecords(section).flatMap {
      case EvidenceRecord(ref, payload: BoardFactEvidence, _) =>
        payload.proofSignalAnchorKinds.map(kind => ref -> kind)
      case _ =>
        Nil
    }.distinct

  def relativeCauseLineEvents(section: RelativeCauseProofSection): List[(EvidenceRef, LineMoveEvent)] =
    relativeCauseProofRecords(section).flatMap {
      case EvidenceRecord(ref, payload: LineFactEvidence, _) =>
        payload.lineEvents.map(event => ref -> event)
      case _ =>
        Nil
    }.distinct

  def relativeCauseLineConsequences(
      kind: RelativeCauseKind,
      section: RelativeCauseProofSection
  ): List[(EvidenceRef, LineConsequence)] =
    relativeCauseProofRecords(section).flatMap {
      case EvidenceRecord(ref, payload: LineFactEvidence, _) =>
        payload.proofSignalConsequences
          .filter(consequence => RelativeCauseKind.acceptsLineConsequence(kind, consequence.kind))
          .map(consequence => ref -> consequence)
      case _ =>
        Nil
    }.distinct

  def relativeCauseRelations(section: RelativeCauseProofSection): List[(EvidenceRef, RelationFactEvidence)] =
    relativeCauseProofRecords(section).collect {
      case EvidenceRecord(ref, payload: RelationFactEvidence, _) if payload.hasConcreteRelationProof =>
        ref -> payload
    }.distinct

  def relativeCauseTacticalMechanisms(
      section: RelativeCauseProofSection
  ): List[(EvidenceRef, TacticalMechanismEvidence)] =
    relativeCauseProofRecords(section).collect {
      case EvidenceRecord(ref, payload: TacticalMechanismEvidence, _) if payload.hasConcreteProof =>
        ref -> payload
    }.distinct

  def relativeCauseStrategicMechanisms(
      section: RelativeCauseProofSection
  ): List[(EvidenceRef, StrategicMechanismEvidence)] =
    relativeCauseProofRecords(section).collect {
      case EvidenceRecord(ref, payload: StrategicMechanismEvidence, _) if payload.canSupportStrategicCause =>
        ref -> payload
    }.distinct

  def relativeCauseStrategicContrasts(
      section: RelativeCauseProofSection
  ): List[(EvidenceRef, StrategicMechanismContrastEvidence)] =
    relativeCauseProofRecords(section).collect {
      case EvidenceRecord(ref, payload: StrategicMechanismContrastEvidence, _)
          if payload.axisComparisons.exists(_.hasContrast) =>
        ref -> payload
    }.distinct

  def relativeCauseThreatEpisodes(
      section: RelativeCauseProofSection
  ): List[(EvidenceRef, ThreatEpisodeEvidence)] =
    relativeCauseProofRecords(section).collect {
      case EvidenceRecord(ref, payload: ThreatEpisodeEvidence, _) if payload.isProofSignalDefensivePressure =>
        ref -> payload
    }.distinct

  def relativeCauseTransitionConsequences(
      kind: RelativeCauseKind,
      section: RelativeCauseProofSection
  ): List[(EvidenceRef, StructuralTransitionBinding, TransitionConsequence)] =
    relativeCauseProofRecords(section).flatMap {
      case EvidenceRecord(ref, payload: StructuralDeltaEvidence, _) =>
        RelativeCauseKind
          .structuralConsequences(kind, payload)
          .map(consequence => (ref, payload.transition, consequence))
      case _ =>
        Nil
    }.distinct

  def relativeCauseProofSectionHasConcreteProof(
      kind: RelativeCauseKind,
      section: RelativeCauseProofSection
  ): Boolean =
    relativeCauseBoardAnchorKinds(section).nonEmpty ||
      relativeCauseLineConsequences(kind, section).nonEmpty ||
      relativeCauseRelations(section).nonEmpty ||
      relativeCauseTacticalMechanisms(section).nonEmpty ||
      relativeCauseStrategicMechanisms(section).nonEmpty ||
      relativeCauseStrategicContrasts(section).nonEmpty ||
      relativeCauseThreatEpisodes(section).nonEmpty ||
      relativeCauseTransitionConsequences(kind, section).nonEmpty

  def relativeCauseProofHasRawTypedDepth(kind: RelativeCauseKind, proof: RelativeCauseProof): Boolean =
    relativeCauseProofHasRawDirectProof(kind, proof) ||
      relativeCauseProofSectionHasConcreteProof(kind, proof.contrastProof)

  def relativeCauseProofHasRawDirectProof(kind: RelativeCauseKind, proof: RelativeCauseProof): Boolean =
    relativeCauseProofSectionHasConcreteProof(kind, proof.directProof)

  def relativeCauseProofHasRawContrastProof(kind: RelativeCauseKind, proof: RelativeCauseProof): Boolean =
    relativeCauseProofSectionHasConcreteProof(kind, proof.contrastProof)

  def relativeCauseProofHasRawContextSupport(kind: RelativeCauseKind, proof: RelativeCauseProof): Boolean =
    relativeCauseProofRecords(proof.contextSupport).nonEmpty

  def relativeCauseProofHasLineDriver(kind: RelativeCauseKind, proof: RelativeCauseProof): Boolean =
    (
      relativeCauseLineConsequences(kind, proof.directProof) ++
        relativeCauseLineConsequences(kind, proof.contrastProof)
    ).exists { case (_, consequence) => LineConsequenceKind.tacticalDriver(consequence.kind) }

  def relativeCauseProofHasTransitionDriver(kind: RelativeCauseKind, proof: RelativeCauseProof): Boolean =
    relativeCauseTransitionConsequences(kind, proof.directProof).nonEmpty ||
      relativeCauseTransitionConsequences(kind, proof.contrastProof).nonEmpty

  def relativeCauseHasRawTypedDepth(cause: RelativeCauseFact): Boolean =
    cause.proof.exists(proof => relativeCauseProofHasRawTypedDepth(cause.kind, proof))

  def relativeCauseHasOwnedTypedDepth(cause: RelativeCauseFact): Boolean =
    cause.attribution.directProofEligible &&
      cause.proof.exists(proof =>
        relativeCauseProofSectionHasConcreteProof(cause.kind, proof.directProof) ||
          relativeCauseHasOwnedRootDefenderMoveProof(cause, proof)
      )

  def relativeCauseHasStrategicContrastDepth(cause: RelativeCauseFact): Boolean =
    cause.strategicCauseKind &&
      cause.proof.exists(proof =>
        relativeCauseStrategicContrasts(proof.directProof).nonEmpty ||
          relativeCauseStrategicContrasts(proof.contrastProof).nonEmpty
      )

  def relativeCauseHasOwnedAdmissibleLongTermProof(cause: RelativeCauseFact): Boolean =
    cause.attribution.directProofEligible &&
      cause.strategicCauseKind &&
      cause.proof.exists(proof =>
        relativeCauseStrategicContrasts(proof.directProof).nonEmpty ||
          relativeCauseTransitionConsequences(cause.kind, proof.directProof).nonEmpty
      )

  def relativeCauseHasOwnedTacticalProof(cause: RelativeCauseFact): Boolean =
    cause.attribution.directProofEligible &&
      cause.proof.exists(proof =>
        relativeCauseTacticalMechanisms(proof.directProof).nonEmpty ||
          relativeCauseRelations(proof.directProof).exists { case (_, payload) =>
            payload.hasConcreteRelationProof && payload.hasLineProof
          } ||
          relativeCauseLineConsequences(cause.kind, proof.directProof)
            .exists { case (_, consequence) => LineConsequenceKind.tacticalDriver(consequence.kind) }
      )

  private def relativeCauseHasOwnedRootDefenderMoveProof(
      cause: RelativeCauseFact,
      proof: RelativeCauseProof
  ): Boolean =
    cause.kind == RelativeCauseKind.ConversionSecured &&
      relativeCauseBinding(cause).exists(binding =>
        relativeCauseLineEvents(proof.directProof).exists { case (_, event) =>
          event.kind == LineEventKind.DefenderMove &&
            (EvidenceRef.sameMove(event.moveUci, binding.eventLine.rootMove) || event.plyOffset == 0)
        }
      )

  def relativeCauseStrategicProofIdentity(
      cause: RelativeCauseFact
  ): RelativeCauseStrategicProofIdentity =
    cause.proof match
      case None =>
        RelativeCauseStrategicProofIdentity.empty
      case Some(proof) =>
        val mechanisms =
          proof.sections
            .flatMap(relativeCauseStrategicMechanisms)
            .distinctBy(_._1.id)
        val contrasts =
          proof.sections
            .flatMap(relativeCauseStrategicContrasts)
            .distinctBy(_._1.id)
        RelativeCauseStrategicProofIdentity(
          axisKeys = (
            mechanisms.flatMap(_._2.signals.flatMap(_.axisKey)) ++
              contrasts.flatMap(_._2.axisComparisons.map(_.axisKey))
          ).distinct.sorted,
          mechanismKinds = mechanisms.map(_._2.kind).distinct.sortBy(_.toString),
          mechanismSourceIds = (mechanisms.map(_._1.id) ++ contrasts.map(_._1.id)).distinct.sorted,
          signalSourceIds = (
            mechanisms.flatMap(_._2.signals.map(_.source.id)) ++
              contrasts.flatMap(_._2.axisComparisons.flatMap(_.sources.map(_.id)))
          ).distinct.sorted
        )

  def relativeCauseProofKindLabels(
      cause: RelativeCauseFact,
      section: RelativeCauseProofSection
  ): List[String] =
    (
      relativeCauseBoardAnchorKinds(section).map { case (_, kind) => s"BoardAnchor:$kind" } ++
        relativeCauseLineEvents(section).map { case (_, event) => s"LineEvent:${event.kind}" } ++
        relativeCauseLineConsequences(cause.kind, section)
          .map { case (_, consequence) => s"LineConsequence:${consequence.kind}" } ++
        relativeCauseRelations(section)
          .map { case (_, payload) => s"Relation:${payload.kind}:${payload.detail.detailName}" } ++
        relativeCauseTacticalMechanisms(section)
          .map { case (_, payload) => s"TacticalMechanism:${payload.kind}" } ++
        relativeCauseStrategicMechanisms(section).flatMap { case (_, payload) =>
          s"StrategicMechanism:${payload.kind}" ::
            payload.signals.flatMap(_.axisKey.map(axis => s"StrategicAxis:$axis"))
        } ++
        relativeCauseStrategicContrasts(section).flatMap { case (_, payload) =>
          s"StrategicMechanismContrast:${payload.comparisonKind}" ::
            payload.axisComparisons.map(axis => s"StrategicAxisContrast:${axis.axisKey}:${axis.outcome}")
        } ++
        relativeCauseThreatEpisodes(section).map(_ => "ThreatEpisode") ++
        relativeCauseTransitionConsequences(cause.kind, section)
          .map { case (_, _, consequence) => s"TransitionConsequence:${consequence.anchorKey}" } ++
        Option
          .when(section.role == RelativeCauseProofRole.ContextSupport)(
            relativeCauseProofRecords(section).map(record => s"ContextLayer:${record.ref.layer}")
          )
          .toList
          .flatten
    ).distinct

  def candidateComparisonRecord(ref: EvidenceRef): Option[EvidenceRecord] =
    record(ref)
      .filter(_.payload.isInstanceOf[CandidateComparisonEvidence])

  def candidateComparison(ref: EvidenceRef): Option[CandidateComparisonFact] =
    candidateComparisonRecord(ref).collect {
      case EvidenceRecord(_, CandidateComparisonEvidence(fact), _) => fact
    }

  def comparisonFor(assessment: RelativeMoveAssessment): Option[CandidateComparisonFact] =
    candidateComparison(assessment.primaryComparisonEvidence)

  def comparisonFor(cause: RelativeCauseFact): Option[CandidateComparisonFact] =
    candidateComparison(cause.comparisonEvidence)

  def relativeCauseBinding(cause: RelativeCauseFact): Option[RelativeCauseBinding] =
    comparisonFor(cause).map(comparison => RelativeCauseFact.binding(cause, comparison))

  def requiredRelativeCauseBinding(cause: RelativeCauseFact): RelativeCauseBinding =
    relativeCauseBinding(cause).getOrElse(
      throw IllegalArgumentException(
        s"relative cause '${cause.comparisonEvidence.id}' is not bound to a candidate comparison"
      )
    )

  def candidateSetFor(fact: CandidateComparisonFact): Option[CandidateSetDescriptor] =
    fact.candidateSet.orElse {
      if fact.kind != CandidateComparisonKind.PlayedVsBest then None
      else
        val requesterParents =
          records.collect {
            case EvidenceRecord(_, CandidateComparisonEvidence(candidateFact), parents)
                if candidateFact == fact =>
              parents.map(_.id)
          }.flatten.toSet
        val owners =
          records.collect {
            case EvidenceRecord(ref, CandidateComparisonEvidence(candidateFact), _)
                if requesterParents(ref.id) && candidateFact.candidateSet.nonEmpty =>
              candidateFact.candidateSet.get
          }
        owners match
          case descriptor :: Nil => Some(descriptor)
          case Nil                => None
          case _ =>
            throw IllegalStateException(
              s"multiple candidate-set owners for comparison '${fact.referenceLine.id}->${fact.candidateLine.id}'"
            )
    }

  def candidateSetFor(assessment: RelativeMoveAssessment): Option[CandidateSetDescriptor] =
    comparisonFor(assessment).flatMap(candidateSetFor)

  def parentClosure(record: EvidenceRecord): List[EvidenceRecord] =
    def loop(refs: List[EvidenceRef], seen: Set[String]): List[EvidenceRecord] =
      refs.flatMap { ref =>
        if seen.contains(ref.id) then Nil
        else byId.get(ref.id).toList.flatMap(parent => parent :: loop(parent.parents, seen + ref.id))
      }
    loop(record.parents, Set.empty).distinctBy(_.ref.id)

  def add(record: EvidenceRecord): TypedEvidenceGraph =
    records.filter(_.ref.id == record.ref.id) match
      case Nil =>
        new TypedEvidenceGraph(records :+ record)
      case existing :: Nil if existing == record =>
        this
      case _ =>
        throw IllegalArgumentException(
          s"evidence id collision for '${record.ref.id}': existing record differs from the attempted addition"
        )

object TypedEvidenceGraph:
  val empty: TypedEvidenceGraph = new TypedEvidenceGraph(Nil)
