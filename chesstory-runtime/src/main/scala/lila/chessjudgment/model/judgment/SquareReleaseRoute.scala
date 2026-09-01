package lila.chessjudgment.model.judgment

import lila.chessjudgment.analysis.position.PositionRelationExtractor
import lila.chessjudgment.model.line.PrincipalVariationEvidence

private[chessjudgment] enum SquareReleaseRouteBranchRole extends CausalBranchRole:
  case ReleasedSquareRoute
  case RetainedBlocker

  def stableKey: String =
    this match
      case ReleasedSquareRoute => "released-square-route"
      case RetainedBlocker     => "retained-blocker"

private[chessjudgment] enum SquareReleaseRoutePremiseRole extends CausalPremiseRole:
  case ReleaseMove
  case RouteMove(routeIndex: Int)
  case TerminalResource
  case TerminalReply

  def stableKey: String =
    this match
      case ReleaseMove           => "release-move"
      case RouteMove(routeIndex) => s"route-move-$routeIndex"
      case TerminalResource      => "terminal-resource"
      case TerminalReply         => "terminal-reply"

private[chessjudgment] enum SquareReleaseRouteAbsenceRole extends CausalAbsenceRole:
  case RetainedBlockerFirstRouteLegAbsent

  def stableKey: String = "retained-blocker-first-route-leg-absent"

private[chessjudgment] enum SquareReleaseRouteStateRole extends CausalStateRole:
  case ReleasedSquareVacancy
  case RoutePiece(routeIndex: Int)
  case RoutePersistence(routeIndex: Int)
  case RetainedBlockerPersistence
  case RetainedRouteOriginPersistence

  def stableKey: String =
    this match
      case ReleasedSquareVacancy      => "released-square-vacancy"
      case RoutePiece(routeIndex)     => s"route-piece-$routeIndex"
      case RoutePersistence(routeIndex) => s"route-persistence-$routeIndex"
      case RetainedBlockerPersistence => "retained-blocker-persistence"
      case RetainedRouteOriginPersistence => "retained-route-origin-persistence"

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
  def retainedFirstLegAbsent: CausalClosedAbsenceBinding
  def releasedVacancies: List[CausalClosedStateBinding]
  def releasedRoutePieces: List[CausalClosedStateBinding]
  def releasedPersistence: List[CausalClosedStateBinding]
  def retainedBlockerPersistence: List[CausalClosedStateBinding]
  def retainedRouteOriginPersistence: List[CausalClosedStateBinding]

  final val contractKind = BoundedCausalContractKind.SquareReleaseRoute
  final def premiseUses: List[CausalVerticalRelationPremiseUse] = terminalUse.toList
  final def absenceBindings = List(retainedFirstLegAbsent)
  final override def stateBindings =
    releasedVacancies ++ releasedRoutePieces ++ releasedPersistence ++
      retainedBlockerPersistence ++ retainedRouteOriginPersistence
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
      retainedFirstLegAbsent: CausalClosedAbsenceBinding,
      releasedVacancies: List[CausalClosedStateBinding],
      releasedRoutePieces: List[CausalClosedStateBinding],
      releasedPersistence: List[CausalClosedStateBinding],
      retainedBlockerPersistence: List[CausalClosedStateBinding],
      retainedRouteOriginPersistence: List[CausalClosedStateBinding]
  ) extends SquareReleaseRouteManifest

  def exact(
      semantic: SquareReleaseRouteSemanticProof,
      releaseMove: CausalLegalMovePremiseUse,
      routeMoves: List[CausalLegalMovePremiseUse],
      terminalUse: Option[CausalVerticalRelationPremiseUse],
      terminalReply: Option[CausalLegalMovePremiseUse],
      retainedFirstLegAbsent: CausalClosedAbsenceBinding,
      releasedVacancies: List[CausalClosedStateBinding],
      releasedRoutePieces: List[CausalClosedStateBinding],
      releasedPersistence: List[CausalClosedStateBinding],
      retainedBlockerPersistence: List[CausalClosedStateBinding],
      retainedRouteOriginPersistence: List[CausalClosedStateBinding]
  ): SquareReleaseRouteManifest =
    val releasedRole = SquareReleaseRouteBranchRole.ReleasedSquareRoute
    val retainedRole = SquareReleaseRouteBranchRole.RetainedBlocker
    val routeIndices = routeMoves.map(_.stepIndex)
    val firstIndex = routeIndices.headOption.getOrElse(
      throw IllegalArgumentException("a square-release route needs one route movement premise")
    )
    require(
      releaseMove.role == SquareReleaseRoutePremiseRole.ReleaseMove &&
        releaseMove.branchRole == releasedRole && releaseMove.stepIndex == 0 &&
        releaseMove.movement == semantic.release,
      "a square-release route needs the exact released-route root movement"
    )
    require(
      routeMoves.zipWithIndex.forall { case (use, routeIndex) =>
        use.role == SquareReleaseRoutePremiseRole.RouteMove(routeIndex) &&
          use.branchRole == releasedRole && use.branchId == releaseMove.branchId &&
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
            use.role == SquareReleaseRoutePremiseRole.TerminalResource &&
              use.branchRole == releasedRole && use.branchId == releaseMove.branchId &&
              use.stepIndex == routeIndices.last && exact.contract.contains(use.contract)
          ) && (terminalReply.isDefined == exact.needsReply) && terminalReply.forall(reply =>
            reply.role == SquareReleaseRoutePremiseRole.TerminalReply &&
              reply.branchRole == releasedRole && reply.branchId == releaseMove.branchId &&
              reply.stepIndex == routeIndices.last + 1
          ),
      "a multi-leg route needs its exact terminal L1 resource and actual reply occurrence"
    )
    require(
      retainedFirstLegAbsent.role == SquareReleaseRouteAbsenceRole.RetainedBlockerFirstRouteLegAbsent &&
        retainedFirstLegAbsent.branchRole == retainedRole &&
        retainedFirstLegAbsent.afterStepIndex == firstIndex - 1,
      "a square-release route needs the sibling's exact first-leg absence"
    )
    require(
      releasedVacancies.size == firstIndex &&
        releasedVacancies.zipWithIndex.forall { case (binding, stepIndex) =>
          binding.role == SquareReleaseRouteStateRole.ReleasedSquareVacancy &&
            binding.branchRole == releasedRole && binding.branchId == releaseMove.branchId &&
            binding.afterStepIndex == stepIndex &&
            binding.query == PositionRelationExtractor.ClosedPositionStateQuery.Vacant(
              releaseMove.movement.from
            )
        },
      "the released square must remain exactly vacant until the first route leg"
    )
    require(
      releasedRoutePieces.zip(routeMoves).zipWithIndex.forall {
        case ((binding, routeMove), routeIndex) =>
          binding.role == SquareReleaseRouteStateRole.RoutePiece(routeIndex) &&
            binding.branchRole == releasedRole && binding.branchId == releaseMove.branchId &&
            binding.afterStepIndex == routeMove.stepIndex &&
            binding.query == PositionRelationExtractor.ClosedPositionStateQuery.OccupiedBy(
              RelationColoredPieceWitness(
                routeMove.movement.to,
                routeMove.movement.afterRole,
                routeMove.movement.side
              )
            )
      } && releasedRoutePieces.size == routeMoves.size,
      "every route endpoint needs its exact post-move occupant state"
    )
    val expectedPersistence = routeMoves.zip(routeMoves.drop(1)).zipWithIndex.flatMap {
      case ((before, after), routeIndex) =>
        (before.stepIndex + 1 until after.stepIndex).map(stepIndex =>
          (
            SquareReleaseRouteStateRole.RoutePersistence(routeIndex),
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
      releasedPersistence.zip(expectedPersistence).forall {
        case (binding, (role, stepIndex, piece)) =>
          binding.role == role && binding.branchRole == releasedRole &&
            binding.branchId == releaseMove.branchId && binding.afterStepIndex == stepIndex &&
            binding.query == PositionRelationExtractor.ClosedPositionStateQuery.OccupiedBy(piece)
      } && releasedPersistence.size == expectedPersistence.size,
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
      retainedBlockerPersistence.size == firstIndex &&
        retainedBlockerPersistence.zipWithIndex.forall { case (binding, stepIndex) =>
          binding.role == SquareReleaseRouteStateRole.RetainedBlockerPersistence &&
            binding.branchRole == retainedRole && binding.branchId == retainedFirstLegAbsent.branchId &&
            binding.afterStepIndex == stepIndex &&
            binding.query == PositionRelationExtractor.ClosedPositionStateQuery.OccupiedBy(blocker)
        } && retainedRouteOriginPersistence.size == firstIndex &&
        retainedRouteOriginPersistence.zipWithIndex.forall { case (binding, stepIndex) =>
          binding.role == SquareReleaseRouteStateRole.RetainedRouteOriginPersistence &&
            binding.branchRole == retainedRole && binding.branchId == retainedFirstLegAbsent.branchId &&
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
      retainedFirstLegAbsent,
      releasedVacancies,
      releasedRoutePieces,
      releasedPersistence,
      retainedBlockerPersistence,
      retainedRouteOriginPersistence
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
    subjectOccurrence: ExplanationSubjectOccurrence,
    routeStepIndices: List[Int],
    terminalReplyStepIndex: Option[Int]
):
  require(proofSet.proposition.contractKind == BoundedCausalContractKind.SquareReleaseRoute)
  require(
    proofSet.occurrence.branches.size == 2 &&
      proofSet.occurrence.branch(SquareReleaseRouteBranchRole.ReleasedSquareRoute).nonEmpty &&
      proofSet.occurrence.branch(SquareReleaseRouteBranchRole.RetainedBlocker).nonEmpty,
    "a square-release route occurrence needs its released-route and retained-blocker branches"
  )
  require(
    routeStepIndices.nonEmpty && routeStepIndices == routeStepIndices.distinct.sorted &&
      routeStepIndices.head >= 2 && retainedBlockerSteps.size == routeStepIndices.head &&
      releasedRouteSteps.size == terminalReplyStepIndex.map(_ + 1).getOrElse(routeStepIndices.last + 1) &&
      terminalReplyStepIndex.forall(_ == routeStepIndices.last + 1),
    "a square-release route occurrence must retain its ordered route and exact branch bounds"
  )
  require(ownsSubject, "the proof occurrence must retain its exact requested root occurrence")

  private def exactBranch(role: SquareReleaseRouteBranchRole): CausalBranchOccurrence =
    proofSet.occurrence.branch(role).getOrElse(
      throw IllegalStateException(s"a square-release route occurrence lost its $role branch")
    )

  def semanticId: String = proofSet.proposition.semanticId
  def occurrenceId: String = proofSet.occurrence.occurrenceId
  def releasedRouteBranch: CausalBranchOccurrence = exactBranch(SquareReleaseRouteBranchRole.ReleasedSquareRoute)
  def retainedBlockerBranch: CausalBranchOccurrence = exactBranch(SquareReleaseRouteBranchRole.RetainedBlocker)
  def releasedRouteLine: LineNodeRef = releasedRouteBranch.line
  def retainedBlockerLine: LineNodeRef = retainedBlockerBranch.line
  def releasedRouteSteps: List[LineReplayStep] = releasedRouteBranch.replaySteps
  def retainedBlockerSteps: List[LineReplayStep] = retainedBlockerBranch.replaySteps
  def releaseStep: LineReplayStep = releasedRouteSteps.head
  def routeSteps: List[LineReplayStep] = routeStepIndices.map(releasedRouteSteps)
  def firstRouteStep: LineReplayStep = routeSteps.head
  def terminalStep: LineReplayStep = routeSteps.last
  def firstRouteStepIndex: Int = routeStepIndices.head
  def terminalStepIndex: Int = routeStepIndices.last
  def terminalReplyStep: Option[LineReplayStep] = terminalReplyStepIndex.map(releasedRouteSteps)
  def proofPaths: List[CausalProofPathOccurrence] = proofSet.paths

  private def ownsSubject: Boolean =
    List(releasedRouteBranch, retainedBlockerBranch).exists { branch =>
      branch.line.id == subjectOccurrence.line.id &&
      branch.lineOwnerEvidenceId == subjectOccurrence.lineOwnerEvidenceId &&
      branch.rootTransitionEvidenceId == subjectOccurrence.transitionEvidenceId &&
      branch.rootProvenance == subjectOccurrence.rootProvenance &&
      branch.steps.headOption.exists(root =>
        EvidenceRef.sameMove(root.step.moveUci, subjectOccurrence.moveUci) &&
          root.step.ply == subjectOccurrence.destination.ply &&
          PrincipalVariationEvidence.sameBoardState(root.step.fenBefore, subjectOccurrence.start.fen) &&
          PrincipalVariationEvidence.sameBoardState(root.step.fenAfter, subjectOccurrence.destination.fen)
      )
    }

private[chessjudgment] final case class SquareReleaseRouteDependencyManifest private[chessjudgment] (
    subject: CertifiedRootOccurrence,
    releasedRoute: CertifiedRootOccurrence,
    retainedBlocker: CertifiedRootOccurrence,
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
      "subject-root",
      BoundedCausalIdentity.evidenceRecordKey(subject.lineOwner),
      BoundedCausalIdentity.evidenceRecordKey(subject.transitionOwner),
      "released-route-root",
      BoundedCausalIdentity.evidenceRecordKey(releasedRoute.lineOwner),
      BoundedCausalIdentity.evidenceRecordKey(releasedRoute.transitionOwner),
      "retained-blocker-root",
      BoundedCausalIdentity.evidenceRecordKey(retainedBlocker.lineOwner),
      BoundedCausalIdentity.evidenceRecordKey(retainedBlocker.transitionOwner),
      contractKind.toString.toLowerCase,
      proofSet.proposition.semanticId,
      proofSet.occurrence.occurrenceId,
      proofSet.paths.map(_.pathOccurrenceId).mkString("[", ",", "]"),
      trajectoryKeys.mkString("[", ",", "]")
    ).mkString("|")

  def consumes(
      exactSubject: CertifiedRootOccurrence,
      exactReleasedRoute: CertifiedRootOccurrence,
      exactRetainedBlocker: CertifiedRootOccurrence
  ): Boolean =
    subject == exactSubject && releasedRoute == exactReleasedRoute && retainedBlocker == exactRetainedBlocker

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
    val subject: CertifiedRootOccurrence,
    private val releasedRoute: CertifiedRootOccurrence,
    private val retainedBlocker: CertifiedRootOccurrence,
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
    val sources = List(
      releasedRoute.lineOwner.ref,
      releasedRoute.transitionOwner.ref,
      retainedBlocker.lineOwner.ref,
      retainedBlocker.transitionOwner.ref
    ).sortBy(_.id)
    require(sources.map(_.id).distinct.size == sources.size, "the two roots need distinct line and transition owners")
    sources

  private[chessjudgment] def lowerIssuerRecords: List[EvidenceRecord] =
    List(releasedRoute.lineOwner, releasedRoute.transitionOwner, retainedBlocker.lineOwner, retainedBlocker.transitionOwner)

  def consumesDependencies(
      exactSubject: CertifiedRootOccurrence,
      exactReleasedRoute: CertifiedRootOccurrence,
      exactRetainedBlocker: CertifiedRootOccurrence
  ): Boolean = dependencyManifest.consumes(exactSubject, exactReleasedRoute, exactRetainedBlocker)

  def proves(record: EvidenceRecord, payload: SquareReleaseRouteEvidence): Boolean =
    record.ref.producer == EvidenceProducer.CausalProofProducer &&
      record.ref.layer == EvidenceLayer.CausalProof &&
      record.ref.confidence == EvidenceConfidence.LegalReplayVerified &&
      record.ref.position == subject.transition.from &&
      record.ref.line.contains(subject.line) &&
      record.ref.scope == subject.transitionOwner.ref.scope &&
      record.parents == parentSources && payload.semantic == semantic &&
      payload.occurrence == occurrence && payload.dependencyFingerprint == dependency.value &&
      payload.subjectOccurrence == subject.publicOccurrence &&
      payload.occurrenceProof == this && remainsCertified

  def remainsCertified: Boolean =
    subject.remainsCertified && releasedRoute.remainsCertified && retainedBlocker.remainsCertified &&
      (subject == releasedRoute || subject == retainedBlocker) &&
      occurrence.subjectOccurrence == subject.publicOccurrence &&
      occurrence.releasedRouteLine == releasedRoute.line && occurrence.retainedBlockerLine == retainedBlocker.line &&
      releasedRoute.replay.replaySteps.take(occurrence.releasedRouteSteps.size) == occurrence.releasedRouteSteps &&
      retainedBlocker.replay.replaySteps.take(occurrence.retainedBlockerSteps.size) == occurrence.retainedBlockerSteps &&
      semanticRemainsCertified && movesRemainCertified && trajectoriesRemainCertified &&
      pathsRemainCertified && dependencyManifest.consumes(subject, releasedRoute, retainedBlocker)

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
    releasedRoute.replay.legalMoveOccurrence(occurrence.releaseStep)
      .flatMap(RecordBoundLegalMoveOccurrence.certified(releasedRoute.lineOwner, _))
      .contains(releaseAuthority) &&
      occurrence.routeSteps.zip(routeAuthorities).forall { case (step, authority) =>
        releasedRoute.replay.legalMoveOccurrence(step)
          .flatMap(RecordBoundLegalMoveOccurrence.certified(releasedRoute.lineOwner, _))
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
              SquareReleaseRoutePremiseRole.ReleaseMove,
              releaseAuthority,
              occurrence.releasedRouteBranch,
              0
            )
            val expectedRoute = routeAuthorities.zipWithIndex.map { case (authority, routeIndex) =>
              CausalLegalMovePremiseUse.from(
                SquareReleaseRoutePremiseRole.RouteMove(routeIndex),
                authority,
                occurrence.releasedRouteBranch,
                occurrence.routeStepIndices(routeIndex)
              )
            }
            val expectedTerminal = authorities.terminalAuthority.map(authority =>
              CausalVerticalRelationPremiseUse.from(
                SquareReleaseRoutePremiseRole.TerminalResource,
                authority,
                occurrence.releasedRouteBranch,
                occurrence.terminalStepIndex
              )
            )
            val expectedReply = authorities.replyAuthority.map(authority =>
              CausalLegalMovePremiseUse.from(
                SquareReleaseRoutePremiseRole.TerminalReply,
                authority,
                occurrence.releasedRouteBranch,
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
                occurrence.releasedRouteBranch,
                occurrence.retainedBlockerBranch,
                occurrence.firstRouteStepIndex,
                manifest.retainedBlockerPersistence,
                manifest.retainedRouteOriginPersistence,
                manifest.retainedFirstLegAbsent
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
            .certified(releasedRoute.lineOwner, authority.occurrence)
            .contains(authority)
      case _ => false
    terminalCertified && ((semantic.terminal, authorities.replyAuthority) match
      case (SquareReleaseRouteTerminal.Occupation, None) => true
      case (SquareReleaseRouteTerminal.CreatedCheck(_, _, _, _, _, _, _, RelationCheckTerminalState.Checkmate), None) =>
        true
      case (SquareReleaseRouteTerminal.CreatedCheck(_, _, _, _, _, _, _, RelationCheckTerminalState.Ongoing), Some(reply)) =>
        occurrence.terminalReplyStep.exists(step =>
          releasedRoute.replay.exactCheckResponseOccurrenceMembership(occurrence.terminalStep, step)
            .exists { case (terminalOccurrence, response) =>
              authorities.terminalAuthority.exists(_.occurrence == terminalOccurrence) &&
                response.resource.moveUci == reply.moveUci &&
                response.resource.movement == reply.movement && response.resource.capture == reply.capture
            }
        )
      case (_: SquareReleaseRouteTerminal.Capture, Some(reply)) =>
        occurrence.terminalReplyStep.exists(step =>
          releasedRoute.replay.legalMoveOccurrence(step)
            .flatMap(RecordBoundLegalMoveOccurrence.certified(releasedRoute.lineOwner, _))
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
      releasedRouteBranch: CausalBranchOccurrence,
      retainedBlockerBranch: CausalBranchOccurrence,
      retainedFirstLegAbsenceAuthority: ClosedRelationAbsenceAuthority,
      releasedVacancyAuthorities: List[ClosedPositionStateAuthority],
      releasedRoutePieceAuthorities: List[ClosedPositionStateAuthority],
      releasedPersistence: List[PersistenceState],
      retainedBlockerAuthorities: List[ClosedPositionStateAuthority],
      retainedRouteOriginAuthorities: List[ClosedPositionStateAuthority],
      vacancyClosure: Option[VacancySiblingClosure]
  )

  private final case class BoundPath(
      manifest: SquareReleaseRouteManifest,
      authority: SquareReleaseRoutePathAuthority
  )

  private final case class ClosureBindings(
      retainedFirstLegAbsence: CausalClosedAbsenceBinding,
      releasedVacancies: List[CausalClosedStateBinding],
      releasedRoutePieces: List[CausalClosedStateBinding],
      releasedPersistence: List[CausalClosedStateBinding],
      retainedBlockerPersistence: List[CausalClosedStateBinding],
      retainedRouteOriginPersistence: List[CausalClosedStateBinding]
  ):
    def stateAuthorities: List[ClosedPositionStateAuthority] =
      releasedVacancies.map(_.authority) ++ releasedRoutePieces.map(_.authority) ++
        releasedPersistence.map(_.authority) ++ retainedBlockerPersistence.map(_.authority) ++
        retainedRouteOriginPersistence.map(_.authority)

  private[chessjudgment] def certifyDemanded(
      subject: CertifiedRootOccurrence,
      releasedRoute: CertifiedRootOccurrence,
      retainedBlocker: CertifiedRootOccurrence,
      demands: List[SquareReleaseRouteDemand]
  ): List[CertifiedSquareReleaseRoute] =
    val demandKeys = demands.map(_.stableKey)
    require(
      demandKeys == demandKeys.distinct.sorted,
      "square-release route needs unique canonical exact occurrence demands"
    )
    val admitted =
      (subject == releasedRoute || subject == retainedBlocker) &&
        releasedRoute.line != retainedBlocker.line &&
        releasedRoute.transition.from.ply == retainedBlocker.transition.from.ply &&
        PrincipalVariationEvidence.sameBoardState(
          releasedRoute.transition.from.fen,
          retainedBlocker.transition.from.fen
        )
    if !admitted then Nil
    else
      val inputs = demands.flatMap(demand =>
        exactInputs(
          demand,
          releasedRoute,
          retainedBlocker
        )
      )
      val grouped = inputs.groupBy(input =>
        val proposition = propositionFor(input)
        val occurrence = CausalOccurrenceIdentity.from(
          proposition,
          List(input.releasedRouteBranch, input.retainedBlockerBranch)
        )
        proposition.semanticId -> occurrence.occurrenceId
      )
      grouped.toList.sortBy(_._1).map { case (_, exact) =>
        certify(
          exact.sortBy(input => input.terminalAuthority.map(_.stableKey).getOrElse("occupation")),
          subject,
          releasedRoute,
          retainedBlocker
        )
      }

  private def exactInputs(
      demand: SquareReleaseRouteDemand,
      releasedRoute: CertifiedRootOccurrence,
      retainedBlocker: CertifiedRootOccurrence
  ): Option[ExactInputs] =
    val releasedSteps = releasedRoute.replay.replaySteps
    val retainedSteps = retainedBlocker.replay.replaySteps
    for
      releaseStep <- releasedSteps.headOption
      rootBoard <- PrincipalVariationEvidence.semanticBoardStateFen(releaseStep.fenBefore)
      retainedRoot <- retainedSteps.headOption
      if PrincipalVariationEvidence.sameBoardState(releaseStep.fenBefore, retainedRoot.fenBefore)
      releaseOccurrence <- releasedRoute.replay.legalMoveOccurrence(releaseStep)
      if releaseOccurrence == demand.releaseOccurrence
      release <- RecordBoundLegalMoveOccurrence.certified(releasedRoute.lineOwner, releaseOccurrence)
      routeOccurrences <- exactRouteOccurrences(demand, releasedRoute.replay)
      route <- traverseOptions(routeOccurrences.map(RecordBoundLegalMoveOccurrence.certified(releasedRoute.lineOwner, _)))
      if route.head.capture.isEmpty && route.head.movement.side == release.movement.side &&
        route.head.movement.to == release.movement.from
      trajectories <- exactTrajectories(releasedRoute.lineOwner, route)
      terminalAuthority <- exactTerminalAuthority(demand, releasedRoute.lineOwner)
      terminal <- terminalAuthority.map(SquareReleaseRouteTerminal.from).getOrElse(
        Some(SquareReleaseRouteTerminal.Occupation)
      )
      if terminal.terminalMover.forall(_ == route.last.movement)
      replyAuthority <- exactReplyAuthority(demand, releasedRoute.lineOwner)
      if replyAuthority.isDefined == terminal.needsReply
      if terminalReplyIsExact(
        terminal,
        terminalAuthority,
        replyAuthority,
        releasedRoute.replay,
        route.last.step
      )
      firstIndex = demand.routeStepIndices.head
      terminalIndex = demand.routeStepIndices.last
      retainedReferenceCount = replyAuthority.map(_ => terminalIndex + 2).getOrElse(terminalIndex + 1)
      if retainedSteps.size >= firstIndex && releasedSteps.size >= retainedReferenceCount
      releasedRouteBranch = CausalBranchOccurrence.fromRootOccurrence(
        SquareReleaseRouteBranchRole.ReleasedSquareRoute,
        releasedRoute,
        retainedReferenceCount
      )
      retainedBlockerBranch = CausalBranchOccurrence.fromRootOccurrence(
        SquareReleaseRouteBranchRole.RetainedBlocker,
        retainedBlocker,
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
        releasedRoute.lineOwner,
        releasedSteps,
        release.movement.from
      )
      if vacancyPrefix.size >= firstIndex
      releasedVacancies = vacancyPrefix.take(firstIndex)
      releasedRoutePieceAuthorities <- traverseOptions(route.map(authority =>
        occupiedAuthority(
          releasedRoute.lineOwner,
          authority.step,
          RelationColoredPieceWitness(
            authority.movement.to,
            authority.movement.afterRole,
            authority.movement.side
          )
        )
      ))
      persistence <- exactPersistence(trajectories, releasedSteps)
      retainedPreStep <- retainedSteps.lift(firstIndex - 1)
      retainedOccurrence <- retainedBlocker.replay.positionAfter(retainedPreStep)
      retainedBlockerAuthorities <- traverseOptions(retainedSteps.take(firstIndex).map(step =>
        occupiedAuthority(retainedBlocker.lineOwner, step, blocker)
      ))
      retainedRouteOriginAuthorities <- traverseOptions(retainedSteps.take(firstIndex).map(step =>
        occupiedAuthority(retainedBlocker.lineOwner, step, routePiece)
      ))
      retainedFirstLegAbsenceAuthority <- ClosedRelationAbsenceAuthority.forQuery(
        retainedBlocker.lineOwner,
        retainedOccurrence,
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
        releasedRouteBranch,
        retainedBlockerBranch,
        retainedFirstLegAbsenceAuthority,
        releasedVacancies,
        releasedRoutePieceAuthorities,
        persistence,
        retainedBlockerAuthorities,
        retainedRouteOriginAuthorities,
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
        releasedRouteBranch,
        retainedBlockerBranch,
        firstIndex,
        bindings.retainedBlockerPersistence,
        bindings.retainedRouteOriginPersistence,
        bindings.retainedFirstLegAbsence
      )
    yield preliminary.copy(vacancyClosure = Some(vacancyClosure))

  private def certify(
      inputs: List[ExactInputs],
      subject: CertifiedRootOccurrence,
      releasedRoute: CertifiedRootOccurrence,
      retainedBlocker: CertifiedRootOccurrence
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
      List(first.releasedRouteBranch, first.retainedBlockerBranch)
    )
    val boundPaths = inputs.map(input => boundPath(semantic, input))
    val paths = boundPaths.map(bound => CausalProofPathOccurrence.from(proposition, bound.manifest))
    val proofSet = BoundedCausalProofSet.from(proposition, occurrenceIdentity, paths)
    val occurrence = SquareReleaseRouteOccurrence(
      proofSet,
      subject.publicOccurrence,
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
      subject,
      releasedRoute,
      retainedBlocker,
      proofSet,
      first.trajectories.map(_.stableKey)
    )
    new CertifiedSquareReleaseRoute(
      semantic,
      occurrence,
      BoundedCausalDependencyFingerprint.from(dependencyManifest),
      dependencyManifest,
      subject,
      releasedRoute,
      retainedBlocker,
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
      SquareReleaseRoutePremiseRole.ReleaseMove,
      input.release,
      input.releasedRouteBranch,
      0
    )
    val routeUses = input.route.zipWithIndex.map { case (authority, routeIndex) =>
      CausalLegalMovePremiseUse.from(
        SquareReleaseRoutePremiseRole.RouteMove(routeIndex),
        authority,
        input.releasedRouteBranch,
        input.routeStepIndices(routeIndex)
      )
    }
    val terminalUse = input.terminalAuthority.map(authority =>
      CausalVerticalRelationPremiseUse.from(
        SquareReleaseRoutePremiseRole.TerminalResource,
        authority,
        input.releasedRouteBranch,
        input.routeStepIndices.last
      )
    )
    val replyUse = input.replyAuthority.map(authority =>
      CausalLegalMovePremiseUse.from(
        SquareReleaseRoutePremiseRole.TerminalReply,
        authority,
        input.releasedRouteBranch,
        input.routeStepIndices.last + 1
      )
    )
    val manifest = SquareReleaseRouteManifest.exact(
      semantic,
      releaseUse,
      routeUses,
      terminalUse,
      replyUse,
      bindings.retainedFirstLegAbsence,
      bindings.releasedVacancies,
      bindings.releasedRoutePieces,
      bindings.releasedPersistence,
      bindings.retainedBlockerPersistence,
      bindings.retainedRouteOriginPersistence
    )
    val path = CausalProofPathOccurrence.from(semantic.identity, manifest)
    BoundPath(
      manifest,
      SquareReleaseRoutePathAuthority(
        path.pathOccurrenceId,
        input.terminalAuthority,
        input.replyAuthority,
        input.retainedFirstLegAbsenceAuthority,
        bindings.stateAuthorities
      )
    )

  private def closureBindings(input: ExactInputs): ClosureBindings =
    val firstIndex = input.routeStepIndices.head
    ClosureBindings(
      CausalClosedAbsenceBinding.afterStep(
        SquareReleaseRouteAbsenceRole.RetainedBlockerFirstRouteLegAbsent,
        input.retainedFirstLegAbsenceAuthority,
        input.retainedBlockerBranch,
        firstIndex - 1
      ),
      input.releasedVacancyAuthorities.zipWithIndex.map { case (authority, stepIndex) =>
        CausalClosedStateBinding.afterStep(
          SquareReleaseRouteStateRole.ReleasedSquareVacancy,
          authority,
          input.releasedRouteBranch,
          stepIndex
        )
      },
      input.releasedRoutePieceAuthorities.zipWithIndex.map { case (authority, routeIndex) =>
        CausalClosedStateBinding.afterStep(
          SquareReleaseRouteStateRole.RoutePiece(routeIndex),
          authority,
          input.releasedRouteBranch,
          input.routeStepIndices(routeIndex)
        )
      },
      input.releasedPersistence.map(state =>
        CausalClosedStateBinding.afterStep(
          SquareReleaseRouteStateRole.RoutePersistence(state.routeIndex),
          state.authority,
          input.releasedRouteBranch,
          state.stepIndex
        )
      ),
      input.retainedBlockerAuthorities.zipWithIndex.map { case (authority, stepIndex) =>
        CausalClosedStateBinding.afterStep(
          SquareReleaseRouteStateRole.RetainedBlockerPersistence,
          authority,
          input.retainedBlockerBranch,
          stepIndex
        )
      },
      input.retainedRouteOriginAuthorities.zipWithIndex.map { case (authority, stepIndex) =>
        CausalClosedStateBinding.afterStep(
          SquareReleaseRouteStateRole.RetainedRouteOriginPersistence,
          authority,
          input.retainedBlockerBranch,
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
