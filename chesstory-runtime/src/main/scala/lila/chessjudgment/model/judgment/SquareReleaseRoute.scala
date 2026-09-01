package lila.chessjudgment.model.judgment

import lila.chessjudgment.analysis.position.PositionRelationExtractor
import lila.chessjudgment.model.line.PrincipalVariationEvidence

private[chessjudgment] enum SquareReleaseRoutePremiseRole extends CausalPremiseRole:
  case ReferenceReleaseMove
  case ReferenceRouteMove(routeIndex: Int)
  case ReferenceTerminalResource
  case ReferenceTerminalReply

  def stableKey: String =
    this match
      case ReferenceReleaseMove           => "reference-release-move"
      case ReferenceRouteMove(routeIndex) => s"reference-route-move-$routeIndex"
      case ReferenceTerminalResource      => "reference-terminal-resource"
      case ReferenceTerminalReply         => "reference-terminal-reply"

private[chessjudgment] enum SquareReleaseRouteAbsenceRole extends CausalAbsenceRole:
  case PlayedFirstRouteLegAbsent

  def stableKey: String = "played-first-route-leg-absent"

private[chessjudgment] enum SquareReleaseRouteStateRole extends CausalStateRole:
  case ReferenceVacancy
  case ReferenceRoutePiece(routeIndex: Int)
  case ReferenceRoutePersistence(routeIndex: Int)
  case PlayedBlockerPersistence
  case PlayedRouteOriginPersistence

  def stableKey: String =
    this match
      case ReferenceVacancy                     => "reference-vacancy"
      case ReferenceRoutePiece(routeIndex)       => s"reference-route-piece-$routeIndex"
      case ReferenceRoutePersistence(routeIndex) => s"reference-route-persistence-$routeIndex"
      case PlayedBlockerPersistence              => "played-blocker-persistence"
      case PlayedRouteOriginPersistence          => "played-route-origin-persistence"

/** Derivation-free terminal assertion. Exact L1 result ids stay in proof
  * paths, so independent derivations of one assertion are never collapsed.
  */
private[chessjudgment] enum SquareReleaseRouteTerminal:
  case Occupation
  case Capture(
      assertionId: String,
      mover: RelationMoveTransitionWitness,
      captured: RelationColoredPieceWitness,
      geometricRecapturers: List[RelationPieceWitness],
      legalRecaptures: List[RelationLegalMoveResourceWitness],
      restrictedRecaptures: List[RelationRestrictedResourceWitness]
  )
  case CreatedCheck(
      assertionId: String,
      mover: RelationMoveTransitionWitness,
      checkedSide: chess.Color,
      kingSquare: EvidenceSquare,
      checkers: List[RelationPieceWitness],
      responses: List[RelationCheckResponseWitness],
      controlledKingDestinations: List[RelationControlledKingDestinationWitness],
      terminal: RelationCheckTerminalState
  )

  def stableKey: String =
    this match
      case Occupation => "occupation"
      case Capture(assertionId, _, _, _, _, _) => s"capture:$assertionId"
      case CreatedCheck(assertionId, _, _, _, _, _, _, _) => s"created-check:$assertionId"

  def terminalMover: Option[RelationMoveTransitionWitness] =
    this match
      case Occupation => None
      case Capture(_, mover, _, _, _, _) => Some(mover)
      case CreatedCheck(_, mover, _, _, _, _, _, _) => Some(mover)

  def contract: Option[VerticalRelationContractKind] =
    this match
      case Occupation     => None
      case _: Capture     => Some(VerticalRelationContractKind.CaptureRecaptureInventory)
      case _: CreatedCheck => Some(VerticalRelationContractKind.CreatedCheckResponseInventory)

  def needsReply: Boolean =
    this match
      case Occupation => false
      case _: Capture => true
      case CreatedCheck(_, _, _, _, _, _, _, terminal) =>
        terminal == RelationCheckTerminalState.Ongoing

private[chessjudgment] object SquareReleaseRouteTerminal:
  def from(authority: RecordBoundVerticalRelationOccurrence): Option[SquareReleaseRouteTerminal] =
    val assertionId = authority.occurrence.relation.assertionId
    authority.occurrence.relation.detail match
      case RelationWitnessDetail.CaptureRecaptureInventory(
            mover,
            captured,
            geometricRecapturers,
            legalRecaptures,
            restrictedRecaptures,
            _
          ) =>
        Some(Capture(
          assertionId,
          mover,
          captured,
          geometricRecapturers,
          legalRecaptures,
          restrictedRecaptures
        ))
      case RelationWitnessDetail.CreatedCheckResponseInventory(
            mover,
            checkedSide,
            kingSquare,
            checkers,
            responses,
            controlledKingDestinations,
            terminal,
            _
          ) =>
        Some(CreatedCheck(
          assertionId,
          mover,
          checkedSide,
          kingSquare,
          checkers,
          responses,
          controlledKingDestinations,
          terminal
        ))
      case _ => None

private[chessjudgment] object SquareReleaseRouteCausalAuthority:
  private final case class Descriptor(
      rootFen: String,
      release: RelationMoveTransitionWitness,
      blocker: RelationColoredPieceWitness,
      route: List[RelationMoveTransitionWitness],
      routePiece: RelationColoredPieceWitness,
      terminal: SquareReleaseRouteTerminal
  ) extends CausalSemanticDescriptor:
    require(route.nonEmpty, "a square-release route needs one exact route leg")
    require(
      blocker == RelationColoredPieceWitness(release.from, release.beforeRole, release.side),
      "a square-release route needs the exact released blocker"
    )
    require(
      routePiece == RelationColoredPieceWitness(route.head.from, route.head.beforeRole, route.head.side) &&
        route.head.side == release.side && route.head.to == release.from,
      "a square-release route needs its exact first same-side occupation leg"
    )
    require(
      route.zip(route.drop(1)).forall { case (before, after) =>
        before.side == after.side && before.to == after.from && before.afterRole == after.beforeRole
      },
      "a square-release route must retain one ordered piece path"
    )
    require(
      terminal match
        case SquareReleaseRouteTerminal.Occupation => route.size == 1
        case exact => route.size >= 2 && exact.terminalMover.contains(route.last),
      "a multi-leg route needs an exact terminal resource on its final leg"
    )

    val contractKind = BoundedCausalContractKind.SquareReleaseRoute
    val semanticParts: List[String] = List(
      release.stableKey,
      BoundedCausalIdentity.coloredPieceKey(blocker),
      route.map(_.stableKey).mkString("[", ",", "]"),
      BoundedCausalIdentity.coloredPieceKey(routePiece),
      terminal.stableKey
    )

  def proposition(
      rootFen: String,
      release: RelationMoveTransitionWitness,
      blocker: RelationColoredPieceWitness,
      route: List[RelationMoveTransitionWitness],
      routePiece: RelationColoredPieceWitness,
      terminal: SquareReleaseRouteTerminal
  ): CausalPropositionIdentity =
    CausalPropositionIdentity.from(Descriptor(rootFen, release, blocker, route, routePiece, terminal))

private[chessjudgment] sealed trait SquareReleaseRouteManifest extends BoundedCausalContractManifest:
  def releaseMove: CausalLegalMovePremiseUse
  def routeMoves: List[CausalLegalMovePremiseUse]
  def terminalUse: Option[CausalVerticalRelationPremiseUse]
  def terminalReply: Option[CausalLegalMovePremiseUse]
  def playedFirstLegAbsent: CausalClosedAbsenceBinding
  def referenceVacancies: List[CausalClosedStateBinding]
  def referenceRoutePieces: List[CausalClosedStateBinding]
  def referencePersistence: List[CausalClosedStateBinding]
  def playedBlockerPersistence: List[CausalClosedStateBinding]
  def playedRouteOriginPersistence: List[CausalClosedStateBinding]

  final val contractKind = BoundedCausalContractKind.SquareReleaseRoute
  final def premiseUses: List[CausalVerticalRelationPremiseUse] = terminalUse.toList
  final def absenceBindings = List(playedFirstLegAbsent)
  final override def stateBindings =
    referenceVacancies ++ referenceRoutePieces ++ referencePersistence ++
      playedBlockerPersistence ++ playedRouteOriginPersistence
  final override def supplementalPremiseUses: List[CausalSupplementalPremiseUse] =
    releaseMove :: (routeMoves ++ terminalReply.toList)
  final def stableKey: String =
    List(
      contractKind.toString.toLowerCase,
      premiseUses.map(_.stableKey).mkString("[", ",", "]"),
      supplementalPremiseUses.map(_.stableKey).mkString("[", ",", "]"),
      absenceBindings.map(_.stableKey).mkString("[", ",", "]"),
      stateBindings.map(_.stableKey).mkString("[", ",", "]")
    ).mkString("|")

private[chessjudgment] object SquareReleaseRouteManifest:
  private final case class Exact(
      releaseMove: CausalLegalMovePremiseUse,
      routeMoves: List[CausalLegalMovePremiseUse],
      terminalUse: Option[CausalVerticalRelationPremiseUse],
      terminalReply: Option[CausalLegalMovePremiseUse],
      playedFirstLegAbsent: CausalClosedAbsenceBinding,
      referenceVacancies: List[CausalClosedStateBinding],
      referenceRoutePieces: List[CausalClosedStateBinding],
      referencePersistence: List[CausalClosedStateBinding],
      playedBlockerPersistence: List[CausalClosedStateBinding],
      playedRouteOriginPersistence: List[CausalClosedStateBinding]
  ) extends SquareReleaseRouteManifest

  def exact(
      semantic: SquareReleaseRouteSemanticProof,
      releaseMove: CausalLegalMovePremiseUse,
      routeMoves: List[CausalLegalMovePremiseUse],
      terminalUse: Option[CausalVerticalRelationPremiseUse],
      terminalReply: Option[CausalLegalMovePremiseUse],
      playedFirstLegAbsent: CausalClosedAbsenceBinding,
      referenceVacancies: List[CausalClosedStateBinding],
      referenceRoutePieces: List[CausalClosedStateBinding],
      referencePersistence: List[CausalClosedStateBinding],
      playedBlockerPersistence: List[CausalClosedStateBinding],
      playedRouteOriginPersistence: List[CausalClosedStateBinding]
  ): SquareReleaseRouteManifest =
    val referenceRole = ComparedLineBranchRole.CounterfactualReference
    val playedRole = ComparedLineBranchRole.PlayedRootAnalysisContinuation
    val routeIndices = routeMoves.map(_.stepIndex)
    val firstIndex = routeIndices.headOption.getOrElse(
      throw IllegalArgumentException("a square-release route needs one route movement premise")
    )
    require(
      releaseMove.role == SquareReleaseRoutePremiseRole.ReferenceReleaseMove &&
        releaseMove.branchRole == referenceRole && releaseMove.stepIndex == 0 &&
        releaseMove.movement == semantic.release,
      "a square-release route needs the exact reference root release movement"
    )
    require(
      routeMoves.zipWithIndex.forall { case (use, routeIndex) =>
        use.role == SquareReleaseRoutePremiseRole.ReferenceRouteMove(routeIndex) &&
          use.branchRole == referenceRole && use.branchId == releaseMove.branchId &&
          use.movement == semantic.route(routeIndex)
      } && routeIndices == routeIndices.distinct.sorted && firstIndex >= 2 &&
        routeMoves.head.capture.isEmpty && routeMoves.head.movement.to == releaseMove.movement.from,
      "a square-release route needs every ordered replay-owned route movement"
    )
    require(
      semantic.terminal match
        case SquareReleaseRouteTerminal.Occupation =>
          routeMoves.size == 1 && terminalUse.isEmpty && terminalReply.isEmpty
        case exact =>
          routeMoves.size >= 2 && terminalUse.exists(use =>
            use.role == SquareReleaseRoutePremiseRole.ReferenceTerminalResource &&
              use.branchRole == referenceRole && use.branchId == releaseMove.branchId &&
              use.stepIndex == routeIndices.last && exact.contract.contains(use.contract)
          ) && (terminalReply.isDefined == exact.needsReply) && terminalReply.forall(reply =>
            reply.role == SquareReleaseRoutePremiseRole.ReferenceTerminalReply &&
              reply.branchRole == referenceRole && reply.branchId == releaseMove.branchId &&
              reply.stepIndex == routeIndices.last + 1
          ),
      "a multi-leg route needs its exact terminal L1 resource and actual reply occurrence"
    )
    require(
      playedFirstLegAbsent.role == SquareReleaseRouteAbsenceRole.PlayedFirstRouteLegAbsent &&
        playedFirstLegAbsent.branchRole == playedRole &&
        playedFirstLegAbsent.afterStepIndex == firstIndex - 1,
      "a square-release route needs the sibling's exact first-leg absence"
    )
    require(
      referenceVacancies.size == firstIndex &&
        referenceVacancies.zipWithIndex.forall { case (binding, stepIndex) =>
          binding.role == SquareReleaseRouteStateRole.ReferenceVacancy &&
            binding.branchRole == referenceRole && binding.branchId == releaseMove.branchId &&
            binding.afterStepIndex == stepIndex &&
            binding.query == PositionRelationExtractor.ClosedPositionStateQuery.Vacant(
              releaseMove.movement.from
            )
        },
      "the released square must remain exactly vacant until the first route leg"
    )
    require(
      referenceRoutePieces.zip(routeMoves).zipWithIndex.forall {
        case ((binding, routeMove), routeIndex) =>
          binding.role == SquareReleaseRouteStateRole.ReferenceRoutePiece(routeIndex) &&
            binding.branchRole == referenceRole && binding.branchId == releaseMove.branchId &&
            binding.afterStepIndex == routeMove.stepIndex &&
            binding.query == PositionRelationExtractor.ClosedPositionStateQuery.OccupiedBy(
              RelationColoredPieceWitness(
                routeMove.movement.to,
                routeMove.movement.afterRole,
                routeMove.movement.side
              )
            )
      } && referenceRoutePieces.size == routeMoves.size,
      "every route endpoint needs its exact post-move occupant state"
    )
    val expectedPersistence = routeMoves.zip(routeMoves.drop(1)).zipWithIndex.flatMap {
      case ((before, after), routeIndex) =>
        (before.stepIndex + 1 until after.stepIndex).map(stepIndex =>
          (
            SquareReleaseRouteStateRole.ReferenceRoutePersistence(routeIndex),
            stepIndex,
            RelationColoredPieceWitness(
              before.movement.to,
              before.movement.afterRole,
              before.movement.side
            )
          )
        )
    }
    require(
      referencePersistence.zip(expectedPersistence).forall {
        case (binding, (role, stepIndex, piece)) =>
          binding.role == role && binding.branchRole == referenceRole &&
            binding.branchId == releaseMove.branchId && binding.afterStepIndex == stepIndex &&
            binding.query == PositionRelationExtractor.ClosedPositionStateQuery.OccupiedBy(piece)
      } && referencePersistence.size == expectedPersistence.size,
      "same-object route continuity needs every intervening occupied state"
    )
    val firstRoute = routeMoves.head
    val blocker = RelationColoredPieceWitness(
      releaseMove.movement.from,
      releaseMove.movement.beforeRole,
      releaseMove.movement.side
    )
    val routeOrigin = RelationColoredPieceWitness(
      firstRoute.movement.from,
      firstRoute.movement.beforeRole,
      firstRoute.movement.side
    )
    require(
      playedBlockerPersistence.size == firstIndex &&
        playedBlockerPersistence.zipWithIndex.forall { case (binding, stepIndex) =>
          binding.role == SquareReleaseRouteStateRole.PlayedBlockerPersistence &&
            binding.branchRole == playedRole && binding.branchId == playedFirstLegAbsent.branchId &&
            binding.afterStepIndex == stepIndex &&
            binding.query == PositionRelationExtractor.ClosedPositionStateQuery.OccupiedBy(blocker)
        } && playedRouteOriginPersistence.size == firstIndex &&
        playedRouteOriginPersistence.zipWithIndex.forall { case (binding, stepIndex) =>
          binding.role == SquareReleaseRouteStateRole.PlayedRouteOriginPersistence &&
            binding.branchRole == playedRole && binding.branchId == playedFirstLegAbsent.branchId &&
            binding.afterStepIndex == stepIndex &&
            binding.query == PositionRelationExtractor.ClosedPositionStateQuery.OccupiedBy(routeOrigin)
        },
      "the sibling must continuously retain the exact blocker and first-route origin piece"
    )
    Exact(
      releaseMove,
      routeMoves,
      terminalUse,
      terminalReply,
      playedFirstLegAbsent,
      referenceVacancies,
      referenceRoutePieces,
      referencePersistence,
      playedBlockerPersistence,
      playedRouteOriginPersistence
    )

private[chessjudgment] final case class SquareReleaseRouteSemanticProof private[chessjudgment] (
    identity: CausalPropositionIdentity,
    release: RelationMoveTransitionWitness,
    blocker: RelationColoredPieceWitness,
    route: List[RelationMoveTransitionWitness],
    routePiece: RelationColoredPieceWitness,
    terminal: SquareReleaseRouteTerminal
):
  require(identity.contractKind == BoundedCausalContractKind.SquareReleaseRoute)
  require(
    SquareReleaseRouteCausalAuthority.proposition(
      identity.rootPositionIdentity.value,
      release,
      blocker,
      route,
      routePiece,
      terminal
    ) == identity,
    "a square-release semantic proof must retain its exact causal assertion"
  )

  def semanticId: String = identity.semanticId
  def rootBoardState: String = identity.rootPositionIdentity.value

private[chessjudgment] final case class SquareReleaseRouteOccurrence private[chessjudgment] (
    proofSet: BoundedCausalProofSet,
    routeStepIndices: List[Int],
    terminalReplyStepIndex: Option[Int]
):
  require(proofSet.proposition.contractKind == BoundedCausalContractKind.SquareReleaseRoute)
  require(
    proofSet.occurrence.branches.size == 2 &&
      proofSet.occurrence.branch(ComparedLineBranchRole.CounterfactualReference).exists(
        _.line.role == LineNodeRole.BestReference
      ) && proofSet.occurrence.branch(ComparedLineBranchRole.PlayedRootAnalysisContinuation).exists(
        _.line.role == LineNodeRole.Played
      ),
    "a square-release route occurrence needs one reference and one Played sibling"
  )
  require(
    routeStepIndices.nonEmpty && routeStepIndices == routeStepIndices.distinct.sorted &&
      routeStepIndices.head >= 2 && playedSteps.size == routeStepIndices.head &&
      referenceSteps.size == terminalReplyStepIndex.map(_ + 1).getOrElse(routeStepIndices.last + 1) &&
      terminalReplyStepIndex.forall(_ == routeStepIndices.last + 1),
    "a square-release route occurrence must retain its ordered route and exact branch bounds"
  )

  private def exactBranch(role: ComparedLineBranchRole): CausalBranchOccurrence =
    proofSet.occurrence.branch(role).getOrElse(
      throw IllegalStateException(s"a square-release route occurrence lost its $role branch")
    )

  def semanticId: String = proofSet.proposition.semanticId
  def occurrenceId: String = proofSet.occurrence.occurrenceId
  def referenceBranch: CausalBranchOccurrence =
    exactBranch(ComparedLineBranchRole.CounterfactualReference)
  def playedBranch: CausalBranchOccurrence =
    exactBranch(ComparedLineBranchRole.PlayedRootAnalysisContinuation)
  def referenceLine: LineNodeRef = referenceBranch.line
  def playedLine: LineNodeRef = playedBranch.line
  def referenceSteps: List[LineReplayStep] = referenceBranch.replaySteps
  def playedSteps: List[LineReplayStep] = playedBranch.replaySteps
  def releaseStep: LineReplayStep = referenceSteps.head
  def routeSteps: List[LineReplayStep] = routeStepIndices.map(referenceSteps)
  def firstRouteStep: LineReplayStep = routeSteps.head
  def terminalStep: LineReplayStep = routeSteps.last
  def firstRouteStepIndex: Int = routeStepIndices.head
  def terminalStepIndex: Int = routeStepIndices.last
  def terminalReplyStep: Option[LineReplayStep] = terminalReplyStepIndex.map(referenceSteps)
  def proofPaths: List[CausalProofPathOccurrence] = proofSet.paths

private[chessjudgment] final case class SquareReleaseRouteDependencyManifest private[chessjudgment] (
    referenceLineRecord: EvidenceRecord,
    playedLineRecord: EvidenceRecord,
    proofSet: BoundedCausalProofSet,
    trajectoryKeys: List[String]
) extends BoundedCausalDependencyManifest:
  val contractKind = BoundedCausalContractKind.SquareReleaseRoute
  require(
    proofSet.proposition.contractKind == contractKind &&
      proofSet.paths.forall(_.manifest.contractKind == contractKind),
    "a square-release dependency may contain only its exact typed proof paths"
  )
  require(
    trajectoryKeys.distinct.size == trajectoryKeys.size,
    "a square-release dependency must retain every trajectory edge once"
  )

  val stableKey: String =
    List(
      BoundedCausalIdentity.evidenceRecordKey(referenceLineRecord),
      BoundedCausalIdentity.evidenceRecordKey(playedLineRecord),
      contractKind.toString.toLowerCase,
      proofSet.proposition.semanticId,
      proofSet.occurrence.occurrenceId,
      proofSet.paths.map(_.pathOccurrenceId).mkString("[", ",", "]"),
      trajectoryKeys.mkString("[", ",", "]")
    ).mkString("|")

  def consumes(reference: EvidenceRecord, played: EvidenceRecord): Boolean =
    referenceLineRecord == reference && playedLineRecord == played

private[chessjudgment] final case class SquareReleaseRoutePathAuthority(
    pathOccurrenceId: String,
    terminalAuthority: Option[RecordBoundVerticalRelationOccurrence],
    replyAuthority: Option[RecordBoundLegalMoveOccurrence],
    absenceAuthority: ClosedRelationAbsenceAuthority,
    stateAuthorities: List[ClosedPositionStateAuthority]
)

private[chessjudgment] final class CertifiedSquareReleaseRoute private[chessjudgment] (
    val semantic: SquareReleaseRouteSemanticProof,
    val occurrence: SquareReleaseRouteOccurrence,
    val dependency: BoundedCausalDependencyFingerprint,
    private val dependencyManifest: SquareReleaseRouteDependencyManifest,
    private val referenceLineRecord: EvidenceRecord,
    private val playedLineRecord: EvidenceRecord,
    private val referenceReplay: CanonicalLineReplay,
    private val playedReplay: CanonicalLineReplay,
    private val releaseAuthority: RecordBoundLegalMoveOccurrence,
    private val routeAuthorities: List[RecordBoundLegalMoveOccurrence],
    private val trajectories: List[RecordBoundObjectTrajectory],
    private val vacancyClosure: VacancySiblingClosure,
    private val pathAuthorities: List[SquareReleaseRoutePathAuthority]
):
  require(
    semantic.identity == occurrence.proofSet.proposition &&
      dependencyManifest.proofSet == occurrence.proofSet &&
      dependencyManifest.trajectoryKeys == trajectories.map(_.stableKey) &&
      BoundedCausalDependencyFingerprint.from(dependencyManifest) == dependency,
    "a certified square-release route needs one proposition and complete dependency manifest"
  )
  require(
    pathAuthorities.map(_.pathOccurrenceId).sorted == occurrence.proofPaths.map(_.pathOccurrenceId) &&
      pathAuthorities.map(_.pathOccurrenceId).distinct.size == pathAuthorities.size,
    "every square-release proof path needs one exact authority bundle"
  )

  def parentSources: List[EvidenceRef] =
    List(referenceLineRecord.ref, playedLineRecord.ref).sortBy(_.id)

  private[chessjudgment] def lowerIssuerRecords: List[EvidenceRecord] =
    List(referenceLineRecord, playedLineRecord)

  def consumesDependencies(reference: EvidenceRecord, played: EvidenceRecord): Boolean =
    dependencyManifest.consumes(reference, played)

  def proves(record: EvidenceRecord, payload: SquareReleaseRouteEvidence): Boolean =
    record.ref.producer == EvidenceProducer.CausalProofProducer &&
      record.ref.layer == EvidenceLayer.CausalProof &&
      record.ref.confidence == EvidenceConfidence.LegalReplayVerified &&
      record.ref.position == referenceLineRecord.ref.position &&
      record.ref.line.contains(occurrence.referenceLine) &&
      record.ref.scope == occurrence.referenceLine.role.scope &&
      record.parents == parentSources && payload.semantic == semantic &&
      payload.occurrence == occurrence && payload.dependencyFingerprint == dependency.value &&
      payload.occurrenceProof.contains(this) && remainsCertified

  def remainsCertified: Boolean =
    CertifiedComparedLineAuthority.exactRecord(
      referenceLineRecord,
      occurrence.referenceLine,
      referenceReplay
    ) && CertifiedComparedLineAuthority.exactRecord(
      playedLineRecord,
      occurrence.playedLine,
      playedReplay
    ) && referenceLineRecord.ref.position == playedLineRecord.ref.position &&
      referenceReplay.replaySteps.take(occurrence.referenceSteps.size) == occurrence.referenceSteps &&
      playedReplay.replaySteps.take(occurrence.playedSteps.size) == occurrence.playedSteps &&
      semanticRemainsCertified && movesRemainCertified && trajectoriesRemainCertified &&
      pathsRemainCertified && dependencyManifest.consumes(referenceLineRecord, playedLineRecord)

  private def semanticRemainsCertified: Boolean =
    SquareReleaseRouteCausalAuthority.proposition(
      semantic.rootBoardState,
      semantic.release,
      semantic.blocker,
      semantic.route,
      semantic.routePiece,
      semantic.terminal
    ) == semantic.identity

  private def movesRemainCertified: Boolean =
    referenceReplay.legalMoveOccurrence(occurrence.releaseStep)
      .flatMap(RecordBoundLegalMoveOccurrence.certified(referenceLineRecord, _))
      .contains(releaseAuthority) &&
      occurrence.routeSteps.zip(routeAuthorities).forall { case (step, authority) =>
        referenceReplay.legalMoveOccurrence(step)
          .flatMap(RecordBoundLegalMoveOccurrence.certified(referenceLineRecord, _))
          .contains(authority)
      } && routeAuthorities.size == occurrence.routeSteps.size

  private def trajectoriesRemainCertified: Boolean =
    trajectories.size == routeAuthorities.size - 1 &&
      trajectories.zip(routeAuthorities.zip(routeAuthorities.drop(1))).forall {
        case (trajectory, (before, after)) =>
          trajectory.rootMovement == before && trajectory.futureMovement == after &&
            trajectory.remainsCertified
      }

  private def pathsRemainCertified: Boolean =
    occurrence.proofPaths.forall { path =>
      pathAuthorities.find(_.pathOccurrenceId == path.pathOccurrenceId).exists(authorities =>
        path.manifest match
          case manifest: SquareReleaseRouteManifest =>
            val expectedRelease = CausalLegalMovePremiseUse.from(
              SquareReleaseRoutePremiseRole.ReferenceReleaseMove,
              releaseAuthority,
              occurrence.referenceBranch,
              0
            )
            val expectedRoute = routeAuthorities.zipWithIndex.map { case (authority, routeIndex) =>
              CausalLegalMovePremiseUse.from(
                SquareReleaseRoutePremiseRole.ReferenceRouteMove(routeIndex),
                authority,
                occurrence.referenceBranch,
                occurrence.routeStepIndices(routeIndex)
              )
            }
            val expectedTerminal = authorities.terminalAuthority.map(authority =>
              CausalVerticalRelationPremiseUse.from(
                SquareReleaseRoutePremiseRole.ReferenceTerminalResource,
                authority,
                occurrence.referenceBranch,
                occurrence.terminalStepIndex
              )
            )
            val expectedReply = authorities.replyAuthority.map(authority =>
              CausalLegalMovePremiseUse.from(
                SquareReleaseRoutePremiseRole.ReferenceTerminalReply,
                authority,
                occurrence.referenceBranch,
                occurrence.terminalReplyStepIndex.getOrElse(
                  throw IllegalStateException("a retained terminal reply lost its branch index")
                )
              )
            )
            manifest.releaseMove == expectedRelease && manifest.routeMoves == expectedRoute &&
              manifest.terminalUse == expectedTerminal && manifest.terminalReply == expectedReply &&
              path.closedAbsenceUses.map(_.binding.authority) == List(authorities.absenceAuthority) &&
              path.closedStateUses.map(_.binding.authority) == authorities.stateAuthorities &&
              closuresRemainCertified(authorities) && terminalRemainsCertified(authorities) &&
              vacancyClosure.matches(
                semantic.release,
                occurrence.releaseStep.moveUci,
                semantic.blocker,
                semantic.route.head,
                occurrence.firstRouteStep.moveUci,
                semantic.routePiece,
                occurrence.referenceBranch,
                occurrence.playedBranch,
                occurrence.firstRouteStepIndex,
                manifest.playedBlockerPersistence,
                manifest.playedRouteOriginPersistence,
                manifest.playedFirstLegAbsent
              )
          case _ => false
      )
    }

  private def closuresRemainCertified(authorities: SquareReleaseRoutePathAuthority): Boolean =
    authorities.absenceAuthority.remainsCertified && authorities.stateAuthorities.forall(authority =>
      ClosedPositionStateAuthority
        .certified(authority.issuerRecord, authority.occurrence, authority.proof)
        .contains(authority)
    )

  private def terminalRemainsCertified(authorities: SquareReleaseRoutePathAuthority): Boolean =
    val terminalCertified = (semantic.terminal, authorities.terminalAuthority) match
      case (SquareReleaseRouteTerminal.Occupation, None) => true
      case (terminal, Some(authority)) =>
        SquareReleaseRouteTerminal.from(authority).contains(terminal) &&
          RecordBoundVerticalRelationOccurrence
            .certified(referenceLineRecord, authority.occurrence)
            .contains(authority)
      case _ => false
    terminalCertified && ((semantic.terminal, authorities.replyAuthority) match
      case (SquareReleaseRouteTerminal.Occupation, None) => true
      case (SquareReleaseRouteTerminal.CreatedCheck(_, _, _, _, _, _, _, RelationCheckTerminalState.Checkmate), None) =>
        true
      case (SquareReleaseRouteTerminal.CreatedCheck(_, _, _, _, _, _, _, RelationCheckTerminalState.Ongoing), Some(reply)) =>
        occurrence.terminalReplyStep.exists(step =>
          referenceReplay.exactCheckResponseOccurrenceMembership(occurrence.terminalStep, step)
            .exists { case (terminalOccurrence, response) =>
              authorities.terminalAuthority.exists(_.occurrence == terminalOccurrence) &&
                response.resource.moveUci == reply.moveUci &&
                response.resource.movement == reply.movement && response.resource.capture == reply.capture
            }
        )
      case (_: SquareReleaseRouteTerminal.Capture, Some(reply)) =>
        occurrence.terminalReplyStep.exists(step =>
          referenceReplay.legalMoveOccurrence(step)
            .flatMap(RecordBoundLegalMoveOccurrence.certified(referenceLineRecord, _))
            .contains(reply)
        )
      case _ => false)

private[chessjudgment] final case class SquareReleaseRouteDemand private[chessjudgment] (
    releaseOccurrence: ReplayLegalMoveOccurrence,
    routeOccurrences: List[ReplayLegalMoveOccurrence],
    routeStepIndices: List[Int],
    terminalOccurrence: Option[ReplayVerticalRelationOccurrence],
    terminalReplyOccurrence: Option[ReplayLegalMoveOccurrence]
):
  require(
    routeOccurrences.nonEmpty && routeOccurrences.size == routeStepIndices.size &&
      routeStepIndices == routeStepIndices.distinct.sorted && routeStepIndices.head >= 2,
    "a square-release demand needs one ordered route occurrence"
  )
  require(
    terminalOccurrence match
      case None => routeOccurrences.size == 1 && terminalReplyOccurrence.isEmpty
      case Some(terminal) =>
        routeOccurrences.size >= 2 && terminal.step == routeOccurrences.last.step &&
          Set(
            VerticalRelationContractKind.CaptureRecaptureInventory,
            VerticalRelationContractKind.CreatedCheckResponseInventory
          )(terminal.contract),
    "a multi-leg square-release demand needs a capture or created-check terminal on its last route leg"
  )

  private[chessjudgment] def stableKey: String =
    List(
      releaseOccurrence.occurrenceId,
      routeOccurrences.map(_.occurrenceId).mkString("[", ",", "]"),
      routeStepIndices.map(index => f"$index%08d").mkString("[", ",", "]"),
      terminalOccurrence.map(_.occurrenceId).getOrElse("occupation"),
      terminalReplyOccurrence.map(_.occurrenceId).getOrElse("no-reply")
    ).mkString("|")

private[chessjudgment] object SquareReleaseRouteDemand:
  def occupation(
      release: ReplayLegalMoveOccurrence,
      firstRouteLeg: ReplayLegalMoveOccurrence,
      firstRouteStepIndex: Int
  ): SquareReleaseRouteDemand =
    SquareReleaseRouteDemand(
      release,
      List(firstRouteLeg),
      List(firstRouteStepIndex),
      None,
      None
    )

  def terminal(
      release: ReplayLegalMoveOccurrence,
      route: List[ReplayLegalMoveOccurrence],
      routeStepIndices: List[Int],
      terminal: ReplayVerticalRelationOccurrence,
      reply: Option[ReplayLegalMoveOccurrence]
  ): SquareReleaseRouteDemand =
    SquareReleaseRouteDemand(release, route, routeStepIndices, Some(terminal), reply)

private[chessjudgment] object SquareReleaseRouteProof:
  private final case class PersistenceState(
      routeIndex: Int,
      stepIndex: Int,
      authority: ClosedPositionStateAuthority
  )

  private final case class ExactInputs(
      rootBoard: String,
      release: RecordBoundLegalMoveOccurrence,
      route: List[RecordBoundLegalMoveOccurrence],
      routeStepIndices: List[Int],
      trajectories: List[RecordBoundObjectTrajectory],
      blocker: RelationColoredPieceWitness,
      routePiece: RelationColoredPieceWitness,
      terminal: SquareReleaseRouteTerminal,
      terminalAuthority: Option[RecordBoundVerticalRelationOccurrence],
      replyAuthority: Option[RecordBoundLegalMoveOccurrence],
      referenceBranch: CausalBranchOccurrence,
      playedBranch: CausalBranchOccurrence,
      playedAbsenceAuthority: ClosedRelationAbsenceAuthority,
      referenceVacancyAuthorities: List[ClosedPositionStateAuthority],
      referenceRoutePieceAuthorities: List[ClosedPositionStateAuthority],
      referencePersistence: List[PersistenceState],
      playedBlockerAuthorities: List[ClosedPositionStateAuthority],
      playedRouteOriginAuthorities: List[ClosedPositionStateAuthority],
      vacancyClosure: Option[VacancySiblingClosure]
  )

  private final case class BoundPath(
      manifest: SquareReleaseRouteManifest,
      authority: SquareReleaseRoutePathAuthority
  )

  private final case class ClosureBindings(
      playedAbsence: CausalClosedAbsenceBinding,
      referenceVacancies: List[CausalClosedStateBinding],
      referenceRoutePieces: List[CausalClosedStateBinding],
      referencePersistence: List[CausalClosedStateBinding],
      playedBlockerPersistence: List[CausalClosedStateBinding],
      playedRouteOriginPersistence: List[CausalClosedStateBinding]
  ):
    def stateAuthorities: List[ClosedPositionStateAuthority] =
      referenceVacancies.map(_.authority) ++ referenceRoutePieces.map(_.authority) ++
        referencePersistence.map(_.authority) ++ playedBlockerPersistence.map(_.authority) ++
        playedRouteOriginPersistence.map(_.authority)

  private[chessjudgment] def certifyDemanded(
      referenceLine: LineNodeRef,
      playedLine: LineNodeRef,
      referenceLineRecord: EvidenceRecord,
      playedLineRecord: EvidenceRecord,
      referenceReplay: CanonicalLineReplay,
      playedReplay: CanonicalLineReplay,
      demands: List[SquareReleaseRouteDemand]
  ): List[CertifiedSquareReleaseRoute] =
    val demandKeys = demands.map(_.stableKey)
    require(
      demandKeys == demandKeys.distinct.sorted,
      "square-release route needs unique canonical exact occurrence demands"
    )
    val admitted =
      CertifiedComparedLineAuthority.exactRecord(referenceLineRecord, referenceLine, referenceReplay) &&
        CertifiedComparedLineAuthority.exactRecord(playedLineRecord, playedLine, playedReplay) &&
        referenceLineRecord.ref.position == playedLineRecord.ref.position &&
        !EvidenceRef.sameMove(referenceLine.rootMove, playedLine.rootMove)
    if !admitted then Nil
    else
      val inputs = demands.flatMap(demand =>
        exactInputs(
          demand,
          referenceLine,
          playedLine,
          referenceLineRecord,
          playedLineRecord,
          referenceReplay,
          playedReplay
        )
      )
      val grouped = inputs.groupBy(input =>
        val proposition = propositionFor(input)
        val occurrence = CausalOccurrenceIdentity.from(
          proposition,
          List(input.referenceBranch, input.playedBranch)
        )
        proposition.semanticId -> occurrence.occurrenceId
      )
      grouped.toList.sortBy(_._1).map { case (_, exact) =>
        certify(
          exact.sortBy(input => input.terminalAuthority.map(_.stableKey).getOrElse("occupation")),
          referenceLineRecord,
          playedLineRecord,
          referenceReplay,
          playedReplay
        )
      }

  private def exactInputs(
      demand: SquareReleaseRouteDemand,
      referenceLine: LineNodeRef,
      playedLine: LineNodeRef,
      referenceLineRecord: EvidenceRecord,
      playedLineRecord: EvidenceRecord,
      referenceReplay: CanonicalLineReplay,
      playedReplay: CanonicalLineReplay
  ): Option[ExactInputs] =
    val referenceSteps = referenceReplay.replaySteps
    val playedSteps = playedReplay.replaySteps
    for
      releaseStep <- referenceSteps.headOption
      rootBoard <- PrincipalVariationEvidence.semanticBoardStateFen(releaseStep.fenBefore)
      playedRoot <- playedSteps.headOption
      if PrincipalVariationEvidence.sameBoardState(releaseStep.fenBefore, playedRoot.fenBefore)
      releaseOccurrence <- referenceReplay.legalMoveOccurrence(releaseStep)
      if releaseOccurrence == demand.releaseOccurrence
      release <- RecordBoundLegalMoveOccurrence.certified(referenceLineRecord, releaseOccurrence)
      routeOccurrences <- exactRouteOccurrences(demand, referenceReplay)
      route <- traverseOptions(routeOccurrences.map(RecordBoundLegalMoveOccurrence.certified(referenceLineRecord, _)))
      if route.head.capture.isEmpty && route.head.movement.side == release.movement.side &&
        route.head.movement.to == release.movement.from
      trajectories <- exactTrajectories(referenceLineRecord, route)
      terminalAuthority <- exactTerminalAuthority(demand, referenceLineRecord)
      terminal <- terminalAuthority.map(SquareReleaseRouteTerminal.from).getOrElse(
        Some(SquareReleaseRouteTerminal.Occupation)
      )
      if terminal.terminalMover.forall(_ == route.last.movement)
      replyAuthority <- exactReplyAuthority(demand, referenceLineRecord)
      if replyAuthority.isDefined == terminal.needsReply
      if terminalReplyIsExact(
        terminal,
        terminalAuthority,
        replyAuthority,
        referenceReplay,
        route.last.step
      )
      firstIndex = demand.routeStepIndices.head
      terminalIndex = demand.routeStepIndices.last
      retainedReferenceCount = replyAuthority.map(_ => terminalIndex + 2).getOrElse(terminalIndex + 1)
      if playedSteps.size >= firstIndex && referenceSteps.size >= retainedReferenceCount
      referenceBranch = CausalBranchOccurrence.certifiedCounterfactual(
        ComparedLineBranchRole.CounterfactualReference,
        referenceLine,
        referenceReplay,
        retainedReferenceCount
      )
      playedBranch = CausalBranchOccurrence.observedRootWithAnalyzedContinuation(
        ComparedLineBranchRole.PlayedRootAnalysisContinuation,
        playedLine,
        playedReplay,
        firstIndex
      )
      blocker = RelationColoredPieceWitness(
        release.movement.from,
        release.movement.beforeRole,
        release.movement.side
      )
      routePiece = RelationColoredPieceWitness(
        route.head.movement.from,
        route.head.movement.beforeRole,
        route.head.movement.side
      )
      vacancyPrefix = continuousVacancyPrefix(
        referenceLineRecord,
        referenceSteps,
        release.movement.from
      )
      if vacancyPrefix.size >= firstIndex
      referenceVacancies = vacancyPrefix.take(firstIndex)
      routePieceAuthorities <- traverseOptions(route.map(authority =>
        occupiedAuthority(
          referenceLineRecord,
          authority.step,
          RelationColoredPieceWitness(
            authority.movement.to,
            authority.movement.afterRole,
            authority.movement.side
          )
        )
      ))
      persistence <- exactPersistence(trajectories, referenceSteps)
      playedPreStep <- playedSteps.lift(firstIndex - 1)
      playedOccurrence <- playedReplay.positionAfter(playedPreStep)
      playedBlockerAuthorities <- traverseOptions(playedSteps.take(firstIndex).map(step =>
        occupiedAuthority(playedLineRecord, step, blocker)
      ))
      playedRouteOriginAuthorities <- traverseOptions(playedSteps.take(firstIndex).map(step =>
        occupiedAuthority(playedLineRecord, step, routePiece)
      ))
      playedAbsenceAuthority <- ClosedRelationAbsenceAuthority.forQuery(
        playedLineRecord,
        playedOccurrence,
        PositionRelationExtractor.ClosedRelationAbsenceQuery.LegalMoveFromTo(
          route.head.movement.side,
          route.head.movement.from,
          route.head.movement.to
        )
      )
      preliminary = ExactInputs(
        rootBoard,
        release,
        route,
        demand.routeStepIndices,
        trajectories,
        blocker,
        routePiece,
        terminal,
        terminalAuthority,
        replyAuthority,
        referenceBranch,
        playedBranch,
        playedAbsenceAuthority,
        referenceVacancies,
        routePieceAuthorities,
        persistence,
        playedBlockerAuthorities,
        playedRouteOriginAuthorities,
        None
      )
      bindings = closureBindings(preliminary)
      vacancyClosure <- VacancySiblingClosure.certified(
        release.movement,
        release.moveUci,
        blocker,
        route.head.movement,
        route.head.moveUci,
        routePiece,
        referenceBranch,
        playedBranch,
        firstIndex,
        bindings.playedBlockerPersistence,
        bindings.playedRouteOriginPersistence,
        bindings.playedAbsence
      )
    yield preliminary.copy(vacancyClosure = Some(vacancyClosure))

  private def certify(
      inputs: List[ExactInputs],
      referenceLineRecord: EvidenceRecord,
      playedLineRecord: EvidenceRecord,
      referenceReplay: CanonicalLineReplay,
      playedReplay: CanonicalLineReplay
  ): CertifiedSquareReleaseRoute =
    val first = inputs.headOption.getOrElse(
      throw IllegalArgumentException("a square-release occurrence needs one exact proof path")
    )
    require(
      inputs.forall(input =>
        input.copy(terminalAuthority = None) == first.copy(terminalAuthority = None)
      ),
      "independent terminal derivations may differ only by their exact L1 path authority"
    )
    val proposition = propositionFor(first)
    val semantic = SquareReleaseRouteSemanticProof(
      proposition,
      first.release.movement,
      first.blocker,
      first.route.map(_.movement),
      first.routePiece,
      first.terminal
    )
    val occurrenceIdentity = CausalOccurrenceIdentity.from(
      proposition,
      List(first.referenceBranch, first.playedBranch)
    )
    val boundPaths = inputs.map(input => boundPath(semantic, input))
    val paths = boundPaths.map(bound => CausalProofPathOccurrence.from(proposition, bound.manifest))
    val proofSet = BoundedCausalProofSet.from(proposition, occurrenceIdentity, paths)
    val occurrence = SquareReleaseRouteOccurrence(
      proofSet,
      first.routeStepIndices,
      first.replyAuthority.map(_ => first.routeStepIndices.last + 1)
    )
    val authoritiesByPath = boundPaths.map(_.authority).map(authority => authority.pathOccurrenceId -> authority).toMap
    val exactPathAuthorities = proofSet.paths.map(path =>
      authoritiesByPath.getOrElse(
        path.pathOccurrenceId,
        throw IllegalStateException("a square-release path lost its exact lower authorities")
      )
    )
    val dependencyManifest = SquareReleaseRouteDependencyManifest(
      referenceLineRecord,
      playedLineRecord,
      proofSet,
      first.trajectories.map(_.stableKey)
    )
    new CertifiedSquareReleaseRoute(
      semantic,
      occurrence,
      BoundedCausalDependencyFingerprint.from(dependencyManifest),
      dependencyManifest,
      referenceLineRecord,
      playedLineRecord,
      referenceReplay,
      playedReplay,
      first.release,
      first.route,
      first.trajectories,
      first.vacancyClosure.getOrElse(
        throw IllegalStateException("a square-release route lost its sibling closure")
      ),
      exactPathAuthorities
    )

  private def boundPath(
      semantic: SquareReleaseRouteSemanticProof,
      input: ExactInputs
  ): BoundPath =
    val bindings = closureBindings(input)
    val releaseUse = CausalLegalMovePremiseUse.from(
      SquareReleaseRoutePremiseRole.ReferenceReleaseMove,
      input.release,
      input.referenceBranch,
      0
    )
    val routeUses = input.route.zipWithIndex.map { case (authority, routeIndex) =>
      CausalLegalMovePremiseUse.from(
        SquareReleaseRoutePremiseRole.ReferenceRouteMove(routeIndex),
        authority,
        input.referenceBranch,
        input.routeStepIndices(routeIndex)
      )
    }
    val terminalUse = input.terminalAuthority.map(authority =>
      CausalVerticalRelationPremiseUse.from(
        SquareReleaseRoutePremiseRole.ReferenceTerminalResource,
        authority,
        input.referenceBranch,
        input.routeStepIndices.last
      )
    )
    val replyUse = input.replyAuthority.map(authority =>
      CausalLegalMovePremiseUse.from(
        SquareReleaseRoutePremiseRole.ReferenceTerminalReply,
        authority,
        input.referenceBranch,
        input.routeStepIndices.last + 1
      )
    )
    val manifest = SquareReleaseRouteManifest.exact(
      semantic,
      releaseUse,
      routeUses,
      terminalUse,
      replyUse,
      bindings.playedAbsence,
      bindings.referenceVacancies,
      bindings.referenceRoutePieces,
      bindings.referencePersistence,
      bindings.playedBlockerPersistence,
      bindings.playedRouteOriginPersistence
    )
    val path = CausalProofPathOccurrence.from(semantic.identity, manifest)
    BoundPath(
      manifest,
      SquareReleaseRoutePathAuthority(
        path.pathOccurrenceId,
        input.terminalAuthority,
        input.replyAuthority,
        input.playedAbsenceAuthority,
        bindings.stateAuthorities
      )
    )

  private def closureBindings(input: ExactInputs): ClosureBindings =
    val firstIndex = input.routeStepIndices.head
    ClosureBindings(
      CausalClosedAbsenceBinding.afterStep(
        SquareReleaseRouteAbsenceRole.PlayedFirstRouteLegAbsent,
        input.playedAbsenceAuthority,
        input.playedBranch,
        firstIndex - 1
      ),
      input.referenceVacancyAuthorities.zipWithIndex.map { case (authority, stepIndex) =>
        CausalClosedStateBinding.afterStep(
          SquareReleaseRouteStateRole.ReferenceVacancy,
          authority,
          input.referenceBranch,
          stepIndex
        )
      },
      input.referenceRoutePieceAuthorities.zipWithIndex.map { case (authority, routeIndex) =>
        CausalClosedStateBinding.afterStep(
          SquareReleaseRouteStateRole.ReferenceRoutePiece(routeIndex),
          authority,
          input.referenceBranch,
          input.routeStepIndices(routeIndex)
        )
      },
      input.referencePersistence.map(state =>
        CausalClosedStateBinding.afterStep(
          SquareReleaseRouteStateRole.ReferenceRoutePersistence(state.routeIndex),
          state.authority,
          input.referenceBranch,
          state.stepIndex
        )
      ),
      input.playedBlockerAuthorities.zipWithIndex.map { case (authority, stepIndex) =>
        CausalClosedStateBinding.afterStep(
          SquareReleaseRouteStateRole.PlayedBlockerPersistence,
          authority,
          input.playedBranch,
          stepIndex
        )
      },
      input.playedRouteOriginAuthorities.zipWithIndex.map { case (authority, stepIndex) =>
        CausalClosedStateBinding.afterStep(
          SquareReleaseRouteStateRole.PlayedRouteOriginPersistence,
          authority,
          input.playedBranch,
          stepIndex
        )
      }
    )

  private def propositionFor(input: ExactInputs): CausalPropositionIdentity =
    SquareReleaseRouteCausalAuthority.proposition(
      input.rootBoard,
      input.release.movement,
      input.blocker,
      input.route.map(_.movement),
      input.routePiece,
      input.terminal
    )

  private def exactRouteOccurrences(
      demand: SquareReleaseRouteDemand,
      replay: CanonicalLineReplay
  ): Option[List[ReplayLegalMoveOccurrence]] =
    val rebound = demand.routeStepIndices.zip(demand.routeOccurrences).flatMap { case (stepIndex, expected) =>
      replay.replaySteps.lift(stepIndex).flatMap(replay.legalMoveOccurrence).filter(_ == expected)
    }
    Option.when(rebound.size == demand.routeOccurrences.size)(rebound)

  private def exactTrajectories(
      record: EvidenceRecord,
      route: List[RecordBoundLegalMoveOccurrence]
  ): Option[List[RecordBoundObjectTrajectory]] =
    val exact = route.zip(route.drop(1)).flatMap { case (before, after) =>
      RecordBoundObjectTrajectory.firstAfter(record, before.step).filter(_.futureMovement == after)
    }
    Option.when(exact.size == route.size - 1)(exact)

  private def exactTerminalAuthority(
      demand: SquareReleaseRouteDemand,
      record: EvidenceRecord
  ): Option[Option[RecordBoundVerticalRelationOccurrence]] =
    demand.terminalOccurrence match
      case None => Some(None)
      case Some(occurrence) =>
        RecordBoundVerticalRelationOccurrence.certified(record, occurrence).map(Some(_))

  private def exactReplyAuthority(
      demand: SquareReleaseRouteDemand,
      record: EvidenceRecord
  ): Option[Option[RecordBoundLegalMoveOccurrence]] =
    demand.terminalReplyOccurrence match
      case None => Some(None)
      case Some(occurrence) => RecordBoundLegalMoveOccurrence.certified(record, occurrence).map(Some(_))

  private def terminalReplyIsExact(
      terminal: SquareReleaseRouteTerminal,
      terminalAuthority: Option[RecordBoundVerticalRelationOccurrence],
      replyAuthority: Option[RecordBoundLegalMoveOccurrence],
      replay: CanonicalLineReplay,
      terminalStep: LineReplayStep
  ): Boolean =
    (terminal, replyAuthority) match
      case (SquareReleaseRouteTerminal.Occupation, None) => terminalAuthority.isEmpty
      case (SquareReleaseRouteTerminal.CreatedCheck(_, _, _, _, _, _, _, RelationCheckTerminalState.Checkmate), None) =>
        terminalAuthority.nonEmpty
      case (SquareReleaseRouteTerminal.CreatedCheck(_, _, _, _, _, _, _, RelationCheckTerminalState.Ongoing), Some(reply)) =>
        replay.exactCheckResponseOccurrenceMembership(terminalStep, reply.step).exists {
          case (occurrence, response) =>
            terminalAuthority.exists(_.occurrence == occurrence) &&
              response.resource.moveUci == reply.moveUci &&
              response.resource.movement == reply.movement && response.resource.capture == reply.capture
        }
      case (_: SquareReleaseRouteTerminal.Capture, Some(reply)) =>
        replay.replaySteps.indexOf(reply.step) == replay.replaySteps.indexOf(terminalStep) + 1 &&
          PrincipalVariationEvidence.sameBoardState(terminalStep.fenAfter, reply.step.fenBefore)
      case _ => false

  private def exactPersistence(
      trajectories: List[RecordBoundObjectTrajectory],
      steps: List[LineReplayStep]
  ): Option[List[PersistenceState]] =
    val exact = trajectories.zipWithIndex.flatMap { case (trajectory, routeIndex) =>
      trajectory.persistenceStates.flatMap(authority =>
        val stepIndex = steps.indexOf(authority.step)
        Option.when(stepIndex >= 0)(PersistenceState(routeIndex, stepIndex, authority))
      )
    }
    val expectedCount = trajectories.map(_.persistenceStates.size).sum
    Option.when(exact.size == expectedCount)(exact)

  private def occupiedAuthority(
      issuerRecord: EvidenceRecord,
      step: LineReplayStep,
      piece: RelationColoredPieceWitness
  ): Option[ClosedPositionStateAuthority] =
    ClosedPositionStateAuthority.atStep(issuerRecord, step)((occurrence, scope) =>
      occurrence.existingOccupantState(piece, scope)
    )

  private def continuousVacancyPrefix(
      issuerRecord: EvidenceRecord,
      steps: List[LineReplayStep],
      releasedSquare: EvidenceSquare
  ): List[ClosedPositionStateAuthority] =
    steps
      .foldLeft(List.empty[ClosedPositionStateAuthority] -> true) {
        case ((reversePrefix, true), step) =>
          ClosedPositionStateAuthority.atStep(issuerRecord, step)((occurrence, scope) =>
            occurrence.existingVacancyState(releasedSquare, scope)
          ) match
            case Some(authority) => (authority :: reversePrefix) -> true
            case None            => reversePrefix -> false
        case (inactive, _) => inactive
      }
      ._1
      .reverse

  private def traverseOptions[A](values: List[Option[A]]): Option[List[A]] =
    val exact = values.flatten
    Option.when(exact.size == values.size)(exact)
