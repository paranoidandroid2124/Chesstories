package lila.chessjudgment.model.judgment

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import lila.chessjudgment.analysis.position.PositionRelationExtractor
import lila.chessjudgment.model.line.PrincipalVariationEvidence

private[chessjudgment] enum BoundedCausalContractKind:
  case ImmediateForcedReplyResourceDifferential
  case PassedPawnResultUnderClosedReplies

private[chessjudgment] final case class SemanticPositionIdentity private (value: String)

private[chessjudgment] object SemanticPositionIdentity:
  def fromFen(fen: String): SemanticPositionIdentity =
    SemanticPositionIdentity(
      PrincipalVariationEvidence
        .semanticBoardStateFen(fen)
        .getOrElse(throw IllegalArgumentException("a causal proposition needs a valid root board"))
    )

/** Shared transposition identity. Concrete proof paths and line occurrences
  * are deliberately excluded so independently reached proofs retain one
  * proposition without losing their routes.
  */
private[chessjudgment] final case class CausalPropositionIdentity private (
    semanticId: String,
    contractKind: BoundedCausalContractKind,
    rootPositionIdentity: SemanticPositionIdentity
):
  require(semanticId.matches("[0-9a-f]{64}"), "a causal proposition needs a canonical id")

private[chessjudgment] object CausalPropositionIdentity:
  def forcedReplyResourceDifferential(
      rootFen: String,
      mechanismKey: String,
      trigger: RelationMoveTransitionWitness,
      forcedReply: RelationLegalMoveResourceWitness,
      realizer: RelationMoveTransitionWitness,
      capturedTarget: RelationColoredPieceWitness,
      playedDefense: RelationLegalMoveResourceWitness,
      disabledDefender: RelationColoredPieceWitness
  ): CausalPropositionIdentity =
    require(mechanismKey.nonEmpty, "a causal proposition needs its exact mechanism")
    val rootPositionIdentity = SemanticPositionIdentity.fromFen(rootFen)
    val semanticId = BoundedCausalIdentity.digest(
      List(
        "causal-proposition:immediate-forced-reply-resource-differential:v2",
        rootPositionIdentity.value,
        mechanismKey,
        trigger.stableKey,
        forcedReply.stableKey,
        realizer.stableKey,
        BoundedCausalIdentity.coloredPieceKey(capturedTarget),
        playedDefense.stableKey,
        BoundedCausalIdentity.coloredPieceKey(disabledDefender)
      )
    )
    CausalPropositionIdentity(
      semanticId,
      BoundedCausalContractKind.ImmediateForcedReplyResourceDifferential,
      rootPositionIdentity
    )

  def passedPawnResult(
      event: PassedPawnResultEventEvidence,
      assessment: PassedPawnResultReplyAssessment
  ): CausalPropositionIdentity =
    require(
      event.exactRobustPublicResultAssessments.contains(assessment) &&
        assessment.consequence.kind == TransitionConsequenceKind.PassedPawnProgress,
      "a passed-pawn causal proposition needs an exact robust passed-pawn result"
    )
    val rootPositionIdentity = SemanticPositionIdentity.fromFen(event.causalEpisode.root.step.fenBefore)
    val semanticId = BoundedCausalIdentity.digest(
      List(
        "causal-proposition:passed-pawn-result-under-closed-replies:v1",
        rootPositionIdentity.value,
        event.causalEpisode.root.identity.stableKey,
        assessment.sourceEvent.identity.stableKey,
        assessment.consequence.stableKey,
        assessment.resultProof.functionIdentity(event.causalEpisode.root).stableKey
      )
    )
    CausalPropositionIdentity(
      semanticId,
      BoundedCausalContractKind.PassedPawnResultUnderClosedReplies,
      rootPositionIdentity
    )

private[chessjudgment] trait CausalBranchRole:
  def stableKey: String

private[chessjudgment] enum CausalRootProvenance:
  case CounterfactualAnalyzedRoot
  case ObservedGameRoot

private[chessjudgment] enum CausalStepProvenance:
  case ObservedGameMove
  case CertifiedAnalysisMove

private[chessjudgment] enum CausalOccurrenceLinkKind:
  case AdjacentLegalReplay
  case CertifiedCausalDependency

/** Exact authority for the jump from one retained occurrence to the next.
  * Adjacent replay is proved by board continuity. A non-adjacent jump is
  * admitted only through an already certified causal dependency; the bounded
  * layer never guesses over omitted plies.
  */
private[chessjudgment] final case class CausalOccurrenceLink private (
    kind: CausalOccurrenceLinkKind,
    fromStepKey: String,
    toStepKey: String,
    lowerProofKey: String
):
  require(fromStepKey.nonEmpty && toStepKey.nonEmpty, "a causal occurrence link needs exact endpoints")
  require(lowerProofKey.nonEmpty, "a causal occurrence link needs its lower authority")

  def stableKey: String =
    List(
      kind.toString.toLowerCase,
      fromStepKey,
      toStepKey,
      lowerProofKey
    ).mkString("|")

  def links(before: LineReplayStep, after: LineReplayStep): Boolean =
    fromStepKey == BoundedCausalIdentity.stepKey(before) &&
      toStepKey == BoundedCausalIdentity.stepKey(after) &&
      (kind match
        case CausalOccurrenceLinkKind.AdjacentLegalReplay =>
          after.ply == before.ply + 1 &&
            PrincipalVariationEvidence.sameBoardState(before.fenAfter, after.fenBefore)
        case CausalOccurrenceLinkKind.CertifiedCausalDependency =>
          after.ply > before.ply)

private[chessjudgment] object CausalOccurrenceLink:
  def adjacent(before: LineReplayStep, after: LineReplayStep): CausalOccurrenceLink =
    require(
      after.ply == before.ply + 1 &&
        PrincipalVariationEvidence.sameBoardState(before.fenAfter, after.fenBefore),
      "an adjacent causal occurrence link needs one continuous legal replay"
    )
    CausalOccurrenceLink(
      CausalOccurrenceLinkKind.AdjacentLegalReplay,
      BoundedCausalIdentity.stepKey(before),
      BoundedCausalIdentity.stepKey(after),
      BoundedCausalIdentity.digest(
        List(
          "adjacent-legal-replay:v1",
          BoundedCausalIdentity.stepKey(before),
          BoundedCausalIdentity.stepKey(after)
        )
      )
    )

  def passedPawnResultDependency(
      dependency: PassedPawnResultDependency
  ): CausalOccurrenceLink =
    require(dependency.enablesContinuation, "a causal occurrence jump needs a certified passed-pawn-result dependency")
    CausalOccurrenceLink(
      CausalOccurrenceLinkKind.CertifiedCausalDependency,
      BoundedCausalIdentity.stepKey(dependency.from.step),
      BoundedCausalIdentity.stepKey(dependency.to.step),
      dependency.stableKey
    )

private[chessjudgment] final case class CausalStepOccurrence private[judgment] (
    index: Int,
    step: LineReplayStep,
    line: LineNodeRef,
    provenance: CausalStepProvenance,
    linkFromPrevious: Option[CausalOccurrenceLink]
):
  require(index >= 0, "a causal step needs a non-negative branch index")
  require(
    (index == 0) == linkFromPrevious.isEmpty,
    "only the root causal occurrence may omit its incoming link"
  )

/** Exact ordered path identity. A played branch records only its root move as
  * observed; every continuation move remains certified analysis.
  */
private[chessjudgment] final case class CausalBranchOccurrence private (
    branchId: String,
    role: CausalBranchRole,
    rootProvenance: CausalRootProvenance,
    line: LineNodeRef,
    steps: List[CausalStepOccurrence],
    rootPositionIdentity: SemanticPositionIdentity
):
  require(branchId.matches("[0-9a-f]{64}"), "a causal branch needs a canonical id")
  require(steps.nonEmpty, "a causal branch needs an exact root step")
  require(steps.map(_.index) == steps.indices.toList, "causal steps must preserve their exact order")
  require(
    steps.head.line == line && EvidenceRef.sameMove(line.rootMove, steps.head.step.moveUci),
    "a causal branch must start at its registered line root"
  )
  require(
    steps.zip(steps.drop(1)).forall { case (before, after) =>
      after.linkFromPrevious.exists(_.links(before.step, after.step))
    },
    "every retained causal occurrence needs an exact adjacent replay or certified dependency"
  )
  require(
    (rootProvenance, steps.map(_.provenance)) match
      case (
            CausalRootProvenance.CounterfactualAnalyzedRoot,
            provenances
          ) => provenances.forall(_ == CausalStepProvenance.CertifiedAnalysisMove)
      case (
            CausalRootProvenance.ObservedGameRoot,
            CausalStepProvenance.ObservedGameMove :: tail
          ) => tail.forall(_ == CausalStepProvenance.CertifiedAnalysisMove)
      case _ => false,
    "branch provenance must distinguish the observed root from analyzed continuation"
  )

  def replaySteps: List[LineReplayStep] = steps.map(_.step)
  def stepAt(index: Int): Option[CausalStepOccurrence] = steps.lift(index)

private[chessjudgment] object CausalBranchOccurrence:
  def certifiedCounterfactual(
      role: CausalBranchRole,
      line: LineNodeRef,
      replay: CanonicalLineReplay,
      retainedStepCount: Int
  ): CausalBranchOccurrence =
    val steps = retainedSteps(replay, retainedStepCount)
    build(
      role,
      CausalRootProvenance.CounterfactualAnalyzedRoot,
      line,
      steps.zipWithIndex.map { case (step, index) =>
        new CausalStepOccurrence(
          index,
          step,
          line,
          CausalStepProvenance.CertifiedAnalysisMove,
          incomingAdjacentLink(steps, index)
        )
      }
    )

  def observedRootWithAnalyzedContinuation(
      role: CausalBranchRole,
      line: LineNodeRef,
      replay: CanonicalLineReplay,
      retainedStepCount: Int
  ): CausalBranchOccurrence =
    val steps = retainedSteps(replay, retainedStepCount)
    build(
      role,
      CausalRootProvenance.ObservedGameRoot,
      line,
      steps.zipWithIndex.map { case (step, index) =>
        new CausalStepOccurrence(
          index,
          step,
          line,
          if index == 0 then CausalStepProvenance.ObservedGameMove
          else CausalStepProvenance.CertifiedAnalysisMove,
          incomingAdjacentLink(steps, index)
        )
      }
    )

  def certifiedPassedPawnResultRoute(
      role: CausalBranchRole,
      event: PassedPawnResultEventEvidence,
      assessment: PassedPawnResultReplyAssessment
  ): CausalBranchOccurrence =
    require(
      event.exactRobustPublicResultAssessments.contains(assessment),
      "a bounded passed-pawn result route needs an exact robust result owned by its event"
    )
    val dependencies = assessment.causalPath
    require(
      dependencies.nonEmpty && dependencies.head.from == event.causalEpisode.root &&
        dependencies.last.to == assessment.sourceEvent &&
        dependencies.zip(dependencies.drop(1)).forall { case (left, right) => left.to == right.from },
      "a bounded passed-pawn result route needs one connected root-owned dependency path"
    )
    val nodes = event.causalEpisode.root :: dependencies.map(_.to)
    require(nodes.forall(_.certifiedLegalStep.nonEmpty), "every retained passed-pawn result occurrence needs legal certification")
    val continuationLine = event.continuationSourceLine.getOrElse(event.rootLine)
    val rootProvenance = provenanceFor(event.rootLine)
    build(
      role,
      rootProvenance,
      event.rootLine,
      nodes.zipWithIndex.map { case (node, index) =>
        new CausalStepOccurrence(
          index,
          node.step,
          if index == 0 then event.rootLine else continuationLine,
          if index == 0 && rootProvenance == CausalRootProvenance.ObservedGameRoot then
            CausalStepProvenance.ObservedGameMove
          else CausalStepProvenance.CertifiedAnalysisMove,
          Option.when(index > 0)(CausalOccurrenceLink.passedPawnResultDependency(dependencies(index - 1)))
        )
      }
    )

  def certifiedPassedPawnReply(
      role: CausalBranchRole,
      event: PassedPawnResultEventEvidence,
      witness: PassedPawnReplyBranchWitness
  ): CausalBranchOccurrence =
    require(event.branchWitnesses.contains(witness), "a bounded reply must belong to the exact passed-pawn result event")
    val replay = witness.canonicalReplay.getOrElse(
      throw IllegalArgumentException("a bounded reply needs its admitted canonical replay")
    )
    val replySteps = replay.replaySteps
    require(
      replySteps.nonEmpty && replySteps.size == witness.observedThroughPlyOffset &&
        replySteps.forall(replay.legalStep(_).nonEmpty) &&
        EvidenceRef.sameMove(replySteps.head.moveUci, witness.line.rootMove),
      "a bounded reply must retain its complete certified observation horizon"
    )
    val allSteps = event.causalEpisode.root.step :: replySteps
    val rootProvenance = provenanceFor(event.rootLine)
    build(
      role,
      rootProvenance,
      event.rootLine,
      allSteps.zipWithIndex.map { case (step, index) =>
        new CausalStepOccurrence(
          index,
          step,
          if index == 0 then event.rootLine else witness.line,
          if index == 0 && rootProvenance == CausalRootProvenance.ObservedGameRoot then
            CausalStepProvenance.ObservedGameMove
          else CausalStepProvenance.CertifiedAnalysisMove,
          incomingAdjacentLink(allSteps, index)
        )
      }
    )

  private def build(
      role: CausalBranchRole,
      rootProvenance: CausalRootProvenance,
      line: LineNodeRef,
      steps: List[CausalStepOccurrence]
  ): CausalBranchOccurrence =
    val rootPositionIdentity = SemanticPositionIdentity.fromFen(steps.headOption.map(_.step.fenBefore).getOrElse(""))
    val branchId = BoundedCausalIdentity.digest(
      List(
        "causal-branch-occurrence:v1",
        role.stableKey,
        rootProvenance.toString.toLowerCase,
        BoundedCausalIdentity.lineKey(line),
        steps
          .map(step =>
            List(
              step.provenance.toString.toLowerCase,
              BoundedCausalIdentity.lineKey(step.line),
              BoundedCausalIdentity.stepKey(step.step),
              step.linkFromPrevious.map(_.stableKey).getOrElse("root")
            ).mkString(":")
          )
          .mkString("[", ",", "]")
      )
    )
    CausalBranchOccurrence(branchId, role, rootProvenance, line, steps, rootPositionIdentity)

  private def retainedSteps(
      replay: CanonicalLineReplay,
      retainedStepCount: Int
  ): List[LineReplayStep] =
    require(retainedStepCount > 0, "a causal branch must retain its root step")
    val steps = replay.replaySteps.take(retainedStepCount)
    require(
      steps.size == retainedStepCount && steps.forall(replay.legalStep(_).nonEmpty),
      "a causal branch may retain only exact steps from its certified replay"
    )
    steps

  private def incomingAdjacentLink(
      steps: List[LineReplayStep],
      index: Int
  ): Option[CausalOccurrenceLink] =
    Option.when(index > 0)(CausalOccurrenceLink.adjacent(steps(index - 1), steps(index)))

  private def provenanceFor(line: LineNodeRef): CausalRootProvenance =
    line.role match
      case LineNodeRole.Played => CausalRootProvenance.ObservedGameRoot
      case _                   => CausalRootProvenance.CounterfactualAnalyzedRoot

private[chessjudgment] trait CausalPremiseRole:
  def stableKey: String

private[chessjudgment] trait CausalSupplementalPremiseUse:
  def branchIds: Set[String]
  def stableKey: String

/** Exact typed lower premise that is not itself an L1 relation occurrence.
  * Construction stays inside typed factories; callers cannot mint a proof by
  * supplying only a label or a guessed identifier.
  */
private[chessjudgment] final case class CausalTypedPremiseUse private (
    role: CausalPremiseRole,
    lowerKind: String,
    lowerSemanticKey: String,
    sourcePremiseIds: List[String],
    branchId: String,
    branchRole: CausalBranchRole,
    relatedBranchIds: List[String],
    fromStepIndex: Int,
    toStepIndex: Int
) extends CausalSupplementalPremiseUse:
  require(lowerKind.nonEmpty && lowerSemanticKey.nonEmpty, "a typed causal premise needs exact lower meaning")
  require(sourcePremiseIds.nonEmpty, "a typed causal premise needs exact lower evidence owners")
  require(
    sourcePremiseIds == sourcePremiseIds.sorted && sourcePremiseIds.distinct.size == sourcePremiseIds.size,
    "typed causal premise owners must be unique and canonical"
  )
  require(fromStepIndex >= 0 && toStepIndex >= fromStepIndex, "a typed premise needs ordered occurrences")
  require(
    relatedBranchIds == relatedBranchIds.sorted && relatedBranchIds.distinct.size == relatedBranchIds.size &&
      !relatedBranchIds.contains(branchId),
    "related causal branches must be distinct and canonical"
  )

  val branchIds: Set[String] = relatedBranchIds.toSet + branchId

  def stableKey: String =
    List(
      role.stableKey,
      lowerKind,
      lowerSemanticKey,
      sourcePremiseIds.mkString("[", ",", "]"),
      branchId,
      branchRole.stableKey,
      relatedBranchIds.mkString("[", ",", "]"),
      fromStepIndex.toString,
      toStepIndex.toString
    ).mkString("|")

private[chessjudgment] object CausalTypedPremiseUse:
  private def lineOccurrenceOwnerStepBinding(
      ownerId: String,
      occurrenceId: String,
      step: LineReplayStep
  ): String =
    BoundedCausalIdentity.digest(List(
      "passed-pawn-result-line-occurrence-owner-step:v1",
      ownerId,
      BoundedCausalIdentity.stepKey(step),
      occurrenceId
    ))

  private def structuralPremiseIds(event: PassedPawnResultEventNode): List[String] =
    (
      List(
        event.lineOccurrenceOwner.id,
        event.structuralOccurrence.occurrenceId,
        lineOccurrenceOwnerStepBinding(
          event.lineOccurrenceOwner.id,
          event.structuralOccurrence.occurrenceId,
          event.step
        )
      ) ++
        event.structuralOccurrence.sourcePremiseKeys
    ).distinct.sorted

  private def resultPremiseIds(proof: PassedPawnResultTransitionProof): List[String] =
    (
      List(
        proof.sourceLineOccurrenceOwner.id,
        proof.sourceOccurrenceId,
        lineOccurrenceOwnerStepBinding(
          proof.sourceLineOccurrenceOwner.id,
          proof.sourceOccurrenceId,
          LineReplayStep(
            proof.sourceTransition.to.ply,
            proof.sourceTransition.moveUci,
            proof.sourceTransition.from.fen,
            proof.sourceTransition.to.fen
          )
        )
      ) ++ proof.sourcePremiseKeys
    ).distinct.sorted

  private def relationOccurrencePremiseIds(
      dependency: PassedPawnResultDependency
  ): List[String] =
    val retainedSteps = dependency.proof match
      case PassedPawnResultDependencyProof.ResponseContinuation(trajectory) =>
        dependency.from.step :: (trajectory.interveningSteps :+ dependency.to.step)
      case _ => List(dependency.from.step, dependency.to.step)
    val bindings = dependency.relationOccurrenceBindings
    require(
      bindings.forall(binding => retainedSteps.contains(binding.step)),
      "a typed passed-pawn result dependency must retain every exact L1 occurrence on its certified route"
    )
    bindings.flatMap(binding =>
      binding.occurrenceId :: binding.certifiedSourcePremiseIds
    ).distinct.sorted

  def passedPawnResultDependency(
      role: CausalPremiseRole,
      dependency: PassedPawnResultDependency,
      sourceRecord: EvidenceRecord,
      branch: CausalBranchOccurrence,
      fromStepIndex: Int,
      toStepIndex: Int
  ): CausalTypedPremiseUse =
    val event = sourceRecord.payload match
      case exact: PassedPawnResultEventEvidence => exact
      case _ => throw IllegalArgumentException("a passed-pawn result dependency premise needs its owning event record")
    require(
      event.causalEpisode.dependencies.contains(dependency) && dependency.enablesContinuation,
      "a typed passed-pawn result premise must belong to its event's certified dependency inventory"
    )
    require(
      branch.stepAt(fromStepIndex).exists(_.step == dependency.from.step) &&
        branch.stepAt(toStepIndex).exists(_.step == dependency.to.step),
      "a typed passed-pawn result premise must bind its exact ordered occurrence endpoints"
    )
    val lineOccurrenceOwners =
      List(dependency.from.lineOccurrenceOwner.id, dependency.to.lineOccurrenceOwner.id).distinct
    require(
      lineOccurrenceOwners.forall(id => sourceRecord.parents.exists(_.id == id)),
      "a typed passed-pawn result dependency must retain both exact line occurrence owners"
    )
    CausalTypedPremiseUse(
      role,
      "passed_pawn_result_dependency",
      dependency.stableKey,
      (
        sourceRecord.ref.id ::
          (
            structuralPremiseIds(dependency.from) ++
              structuralPremiseIds(dependency.to) ++
              relationOccurrencePremiseIds(dependency)
          )
      ).distinct.sorted,
      branch.branchId,
      branch.role,
      Nil,
      fromStepIndex,
      toStepIndex
    )

  def passedPawnResult(
      role: CausalPremiseRole,
      assessment: PassedPawnResultReplyAssessment,
      sourceRecord: EvidenceRecord,
      branch: CausalBranchOccurrence,
      stepIndex: Int
  ): CausalTypedPremiseUse =
    val event = sourceRecord.payload match
      case exact: PassedPawnResultEventEvidence => exact
      case _ => throw IllegalArgumentException("a passed-pawn-result premise needs its owning event record")
    require(
      event.exactRobustPublicResultAssessments.contains(assessment) &&
        assessment.resultProof.binds(assessment.sourceEvent, assessment.consequence, assessment.causalPath),
      "a typed passed-pawn-result premise needs an exact robust result route"
    )
    require(
      branch.stepAt(stepIndex).exists(_.step == assessment.sourceEvent.step),
      "a passed-pawn-result premise must bind its exact realizing occurrence"
    )
    require(
      sourceRecord.parents.exists(_.id == assessment.resultProof.sourceLineOccurrenceOwner.id),
      "a passed-pawn-result premise must retain its exact line occurrence owner"
    )
    CausalTypedPremiseUse(
      role,
      "passed_pawn_result",
      assessment.resultProof.stableKey,
      (sourceRecord.ref.id :: resultPremiseIds(assessment.resultProof)).distinct.sorted,
      branch.branchId,
      branch.role,
      Nil,
      stepIndex,
      stepIndex
    )

  def observedPassedPawnResultDependency(
      role: CausalPremiseRole,
      dependency: PassedPawnResultDependency,
      sourceRecord: EvidenceRecord,
      witness: PassedPawnReplyBranchWitness,
      branch: CausalBranchOccurrence,
      fromStepIndex: Int,
      toStepIndex: Int
  ): CausalTypedPremiseUse =
    val event = sourceRecord.payload match
      case exact: PassedPawnResultEventEvidence => exact
      case _ => throw IllegalArgumentException("an observed passed-pawn result dependency needs its owning event record")
    val observedEpisode = witness.observedEpisode.getOrElse(
      throw IllegalArgumentException("an observed passed-pawn result dependency needs its exact reply episode")
    )
    require(
      event.branchWitnesses.contains(witness) && observedEpisode.dependencies.contains(dependency) &&
        dependency.enablesContinuation,
      "an observed passed-pawn result premise must belong to its exact branch episode"
    )
    require(
      branch.stepAt(fromStepIndex).exists(_.step == dependency.from.step) &&
        branch.stepAt(toStepIndex).exists(_.step == dependency.to.step),
      "an observed passed-pawn result premise must bind its exact ordered reply occurrences"
    )
    val branchLineOwners = sourceRecord.parents
      .filter(_.line.contains(witness.line))
      .map(_.id)
    require(branchLineOwners.nonEmpty, "an observed passed-pawn result premise needs its exact branch-line owner")
    val lineOccurrenceOwners =
      List(dependency.from.lineOccurrenceOwner.id, dependency.to.lineOccurrenceOwner.id).distinct
    require(
      lineOccurrenceOwners.forall(id => sourceRecord.parents.exists(_.id == id)),
      "an observed passed-pawn result dependency must retain both exact line occurrence owners"
    )
    CausalTypedPremiseUse(
      role,
      "observed_passed_pawn_result_dependency",
      dependency.stableKey,
      (
        sourceRecord.ref.id ::
          (
            branchLineOwners ++
              structuralPremiseIds(dependency.from) ++
              structuralPremiseIds(dependency.to) ++
              relationOccurrencePremiseIds(dependency)
          )
      ).distinct.sorted,
      branch.branchId,
      branch.role,
      Nil,
      fromStepIndex,
      toStepIndex
    )

  def observedPassedPawnResult(
      role: CausalPremiseRole,
      realization: PassedPawnResultRealization,
      sourceRecord: EvidenceRecord,
      witness: PassedPawnReplyBranchWitness,
      branch: CausalBranchOccurrence,
      stepIndex: Int
  ): CausalTypedPremiseUse =
    val event = sourceRecord.payload match
      case exact: PassedPawnResultEventEvidence => exact
      case _ => throw IllegalArgumentException("an observed passed-pawn result needs its owning event record")
    val observedEpisode = witness.observedEpisode.getOrElse(
      throw IllegalArgumentException("an observed passed-pawn result needs its exact reply episode")
    )
    require(
      event.branchWitnesses.contains(witness) && realization.observedRoot == observedEpisode.root &&
        observedEpisode.resultRoutes.contains(realization.resultRoute) &&
        realization.resultRoute.resultProof.binds(
          realization.resultRoute.sourceEvent,
          realization.resultRoute.consequence,
          realization.resultRoute.causalPath
        ),
      "an observed passed-pawn result must be an exact certified result route in its branch"
    )
    require(
      branch.stepAt(stepIndex).exists(_.step == realization.event.step),
      "an observed passed-pawn result must bind its exact realizing occurrence"
    )
    val branchLineOwners = sourceRecord.parents
      .filter(_.line.contains(witness.line))
      .map(_.id)
    require(branchLineOwners.nonEmpty, "an observed passed-pawn result needs its exact branch-line owner")
    require(
      sourceRecord.parents.exists(_.id == realization.resultRoute.resultProof.sourceLineOccurrenceOwner.id),
      "an observed passed-pawn result must retain its exact line occurrence owner"
    )
    CausalTypedPremiseUse(
      role,
      "observed_passed_pawn_result",
      realization.resultRoute.resultProof.stableKey,
      (
        sourceRecord.ref.id ::
          (branchLineOwners ++ resultPremiseIds(realization.resultRoute.resultProof))
      ).distinct.sorted,
      branch.branchId,
      branch.role,
      Nil,
      stepIndex,
      stepIndex
    )

  def functionalMatch(
      role: CausalPremiseRole,
      expected: PassedPawnResultReplyAssessment,
      realization: PassedPawnResultRealization,
      sourceRecord: EvidenceRecord,
      expectedBranch: CausalBranchOccurrence,
      observedBranch: CausalBranchOccurrence,
      expectedStepIndex: Int,
      observedStepIndex: Int
  ): CausalTypedPremiseUse =
    val event = sourceRecord.payload match
      case exact: PassedPawnResultEventEvidence => exact
      case _ => throw IllegalArgumentException("a passed-pawn result functional match needs its owning event record")
    require(
      event.exactRobustPublicResultAssessments.contains(expected) &&
        PassedPawnResultFunctionalMatch.causallyEquivalent(
          event.causalEpisode.root,
          expected.resultRoute,
          realization.observedRoot,
          realization.resultRoute
        ),
      "a passed-pawn result functional-match premise needs two exact causally equivalent routes"
    )
    require(
      expectedBranch.stepAt(expectedStepIndex).exists(_.step == expected.sourceEvent.step) &&
        observedBranch.stepAt(observedStepIndex).exists(_.step == realization.event.step),
      "a passed-pawn result functional match must bind both exact result occurrences"
    )
    val lineOccurrenceOwners =
      List(
        expected.resultProof.sourceLineOccurrenceOwner.id,
        realization.resultRoute.resultProof.sourceLineOccurrenceOwner.id
      ).distinct
    require(
      lineOccurrenceOwners.forall(id => sourceRecord.parents.exists(_.id == id)),
      "a passed-pawn result functional match must retain both exact line occurrence owners"
    )
    CausalTypedPremiseUse(
      role,
      "passed_pawn_result_functional_match",
      BoundedCausalIdentity.digest(
        List(
          expected.resultRoute.stableKey,
          realization.resultRoute.stableKey,
          realization.matchKind.toString.toLowerCase
        )
      ),
      (
        sourceRecord.ref.id ::
          (resultPremiseIds(expected.resultProof) ++ resultPremiseIds(realization.resultRoute.resultProof))
      ).distinct.sorted,
      observedBranch.branchId,
      observedBranch.role,
      List(expectedBranch.branchId).sorted,
      observedStepIndex,
      observedStepIndex
    )

  def comparisonDemand(
      role: CausalPremiseRole,
      comparisonRecord: EvidenceRecord,
      branch: CausalBranchOccurrence
  ): CausalTypedPremiseUse =
    val comparison = comparisonRecord.payload match
      case CandidateComparisonEvidence(exact) => exact
      case _ => throw IllegalArgumentException("a causal demand premise needs a typed comparison record")
    require(
      comparison.kind == CandidateComparisonKind.PlayedVsBest &&
        Set(comparison.referenceLine, comparison.candidateLine)(branch.line),
      "a passed-pawn result demand must be the exact PlayedVsBest endpoint comparison"
    )
    CausalTypedPremiseUse(
      role,
      "played_vs_best_demand",
      CandidateComparisonSemanticKey.from(comparison).stableKey,
      (comparisonRecord.ref.id :: comparisonRecord.parents.map(_.id)).distinct.sorted,
      branch.branchId,
      branch.role,
      Nil,
      0,
      0
    )

private[chessjudgment] final case class CausalRelationPremiseUse private (
    role: CausalPremiseRole,
    contract: VerticalRelationContractKind,
    result: DerivedRelationResultKey,
    sourcePremiseIds: List[String],
    branchId: String,
    branchRole: CausalBranchRole,
    stepIndex: Int
):
  require(sourcePremiseIds.nonEmpty, "a causal relation premise needs its exact lower sources")
  require(
    sourcePremiseIds == sourcePremiseIds.sorted && sourcePremiseIds.distinct.size == sourcePremiseIds.size,
    "lower premise ids must be unique and canonical within one named use"
  )
  require(stepIndex >= 0, "a relation premise needs an exact branch step")

  def stableKey: String =
    List(
      role.stableKey,
      contract.toString.toLowerCase,
      result.stableKey,
      sourcePremiseIds.mkString("[", ",", "]"),
      branchId,
      branchRole.toString.toLowerCase,
      stepIndex.toString
    ).mkString("|")

private[chessjudgment] object CausalRelationPremiseUse:
  def from(
      role: CausalPremiseRole,
      occurrence: ReplayVerticalRelationOccurrence,
      branch: CausalBranchOccurrence,
      stepIndex: Int
  ): CausalRelationPremiseUse =
    require(
      branch.stepAt(stepIndex).exists(_.step == occurrence.step),
      "a relation premise must bind its exact certified replay occurrence"
    )
    val sourcePremiseIds = occurrence.certifiedSourcePremiseIds
    CausalRelationPremiseUse(
      role,
      occurrence.contract,
      DerivedRelationResultKey.from(occurrence.relation),
      sourcePremiseIds,
      branch.branchId,
      branch.role,
      stepIndex
    )

private[chessjudgment] trait CausalAbsenceRole:
  def stableKey: String

private[chessjudgment] trait CausalSupplementalClosureBinding:
  def branchIds: Set[String]
  def stableKey: String

private[chessjudgment] final case class CausalClosedReplyInventoryBinding private (
    issuerEvidenceId: String,
    coverageEvidenceId: String,
    rootAfter: PositionNodeRef,
    scope: EvidenceScope,
    legalReplyMoves: List[String],
    branchByReply: List[(String, String)],
    certifiedHorizonPlyOffset: Int
) extends CausalSupplementalClosureBinding:
  require(issuerEvidenceId.nonEmpty, "a closed reply inventory needs its exact issuer")
  require(coverageEvidenceId.nonEmpty, "a closed reply inventory needs its exact branch-coverage issuer")
  require(legalReplyMoves.nonEmpty, "a closed reply inventory needs at least one legal reply")
  require(
    legalReplyMoves == legalReplyMoves.sorted && legalReplyMoves.distinct.size == legalReplyMoves.size,
    "closed legal replies must be unique and canonical"
  )
  require(
    branchByReply.map(_._1) == legalReplyMoves && branchByReply.map(_._2).distinct.size == branchByReply.size,
    "a closed reply inventory needs one occurrence branch for every legal reply"
  )
  require(certifiedHorizonPlyOffset > 0, "a closed reply inventory needs an exact positive horizon")

  val branchIds: Set[String] = branchByReply.map(_._2).toSet

  def stableKey: String =
    List(
      issuerEvidenceId,
      coverageEvidenceId,
      PrincipalVariationEvidence.normalizeFen(rootAfter.fen),
      rootAfter.ply.toString,
      scope.toString.toLowerCase,
      branchByReply.map { case (move, branch) => s"$move@$branch" }.mkString("[", ",", "]"),
      certifiedHorizonPlyOffset.toString
    ).mkString("|")

private[chessjudgment] object CausalClosedReplyInventoryBinding:
  def from(
      inventoryRecord: EvidenceRecord,
      sourceRecord: EvidenceRecord,
      event: PassedPawnResultEventEvidence,
      replyBranches: List[(PassedPawnReplyBranchWitness, CausalBranchOccurrence)]
  ): CausalClosedReplyInventoryBinding =
    val inventory = inventoryRecord.payload match
      case exact: StructuralDeltaEvidence => exact
      case _ => throw IllegalArgumentException("a closed reply inventory needs a StructuralDelta authority")
    require(
      sourceRecord.payload == event && event.branchSetComplete &&
        sourceRecord.parents.contains(inventoryRecord.ref),
      "a reply closure must retain its exact lower inventory and branch-coverage authorities"
    )
    require(
      inventoryRecord.ref.producer == EvidenceProducer.StructuralDeltaProducer &&
        inventoryRecord.ref.layer == EvidenceLayer.StructuralDelta &&
        inventoryRecord.ref.confidence == EvidenceConfidence.BoardDerived &&
        inventoryRecord.ref.position == event.rootTransition.from &&
        inventoryRecord.ref.line.contains(event.rootLine) &&
        inventoryRecord.ref.scope == event.rootTransition.role.scope &&
        inventory.transition == event.rootTransition &&
        inventory.transitionIsCertified && inventory.exactOutputInventoryCertified &&
        inventory.canonicalTransitionProof == event.canonicalRootTransitionProof,
      "a reply closure must consume the exact graph-owned root transition inventory"
    )
    val legalReplies = inventory.certifiedRootResponseMoves
      .map(_.map(EvidenceRef.normalizeMove).sorted)
      .getOrElse(throw IllegalArgumentException("a closed passed-pawn result needs the root legal-reply inventory"))
    val byMove = replyBranches.map { case (witness, branch) =>
      require(event.branchWitnesses.contains(witness), "a closed reply branch must belong to its event")
      EvidenceRef.normalizeMove(witness.line.rootMove) -> branch.branchId
    }.sortBy(_._1)
    require(
      byMove.map(_._1) == legalReplies,
      "a closed passed-pawn result must preserve every and only root legal reply"
    )
    val horizons = event.branchWitnesses.map(_.certifiedHorizonPlyOffset).distinct
    CausalClosedReplyInventoryBinding(
      inventoryRecord.ref.id,
      sourceRecord.ref.id,
      event.rootTransition.to,
      inventoryRecord.ref.scope,
      legalReplies,
      byMove,
      horizons match
        case exact :: Nil => exact
        case _ => throw IllegalArgumentException("a closed passed-pawn result needs one exact branch horizon")
    )

/** Semantic absence id plus one exact branch use. The same semantic absence
  * may occur in several paths or steps; uniqueness never collapses by proof id.
  */
private[chessjudgment] final case class CausalClosedAbsenceBinding private (
    role: CausalAbsenceRole,
    semanticProofId: String,
    issuerEvidenceId: String,
    issuerOccurrenceId: String,
    queryKey: String,
    position: PositionNodeRef,
    scope: EvidenceScope,
    branchId: String,
    branchRole: CausalBranchRole,
    afterStepIndex: Int
):
  require(semanticProofId.matches("[0-9a-f]{64}"), "a closed absence needs its semantic id")
  require(issuerEvidenceId.nonEmpty, "a closed absence needs its exact LegalLine issuer")
  require(issuerOccurrenceId.matches("[0-9a-f]{64}"), "a closed absence needs its exact replay occurrence")
  require(queryKey.nonEmpty, "a closed absence needs its exact inventory query")

  def stableKey: String =
    List(
      role.stableKey,
      semanticProofId,
      issuerEvidenceId,
      issuerOccurrenceId,
      queryKey,
      PrincipalVariationEvidence.normalizeFen(position.fen),
      position.ply.toString,
      scope.toString.toLowerCase,
      branchId,
      branchRole.toString.toLowerCase,
      s"after:$afterStepIndex"
    ).mkString("|")

private[chessjudgment] object CausalClosedAbsenceBinding:
  def afterStep(
      role: CausalAbsenceRole,
      proof: PositionRelationExtractor.ClosedRelationAbsenceProof,
      branch: CausalBranchOccurrence,
      stepIndex: Int,
      issuerRecord: EvidenceRecord,
      issuerOccurrence: ReplayVerticalRelationOccurrence
  ): CausalClosedAbsenceBinding =
    val step = branch.stepAt(stepIndex).map(_.step)
      .getOrElse(throw IllegalArgumentException("a closed absence must bind a retained branch step"))
    require(
      step.ply == proof.position.ply &&
        PrincipalVariationEvidence.sameBoardState(step.fenAfter, proof.position.fen),
      "a closed absence position must be the exact after occurrence of its branch step"
    )
    require(
      proof.scope == branch.line.role.scope,
      "a closed absence must retain its exact branch inventory scope"
    )
    val issuerCertified = issuerRecord match
      case EvidenceRecord(ref, line: LineFactEvidence, _) =>
        ref.producer == EvidenceProducer.LegalLineProducer &&
          ref.layer == EvidenceLayer.Line &&
          ref.confidence == EvidenceConfidence.LegalReplayVerified &&
          ref.line.contains(branch.line) && ref.scope == branch.line.role.scope &&
          line.line == branch.line && line.certifiedReplay.exists(replay =>
            replay.verticalRelationOccurrences(step, List(issuerOccurrence.contract)).exists(
              _.occurrenceId == issuerOccurrence.occurrenceId
            )
          )
      case _ => false
    require(
      issuerCertified && issuerOccurrence.step == step,
      "a closed absence must name its exact graph-owned LegalLine relation occurrence"
    )
    val semanticBoard = PrincipalVariationEvidence
      .semanticBoardStateFen(proof.position.fen)
      .getOrElse(throw IllegalArgumentException("a closed absence needs a semantic board state"))
    val semanticProofId = BoundedCausalIdentity.digest(
      List("closed-relation-absence:v1", semanticBoard, proof.query.stableKey)
    )
    CausalClosedAbsenceBinding(
      role,
      semanticProofId,
      issuerRecord.ref.id,
      issuerOccurrence.occurrenceId,
      proof.query.stableKey,
      proof.position,
      proof.scope,
      branch.branchId,
      branch.role,
      stepIndex
    )

private[chessjudgment] trait BoundedCausalContractManifest:
  def contractKind: BoundedCausalContractKind
  def premiseUses: List[CausalRelationPremiseUse]
  def absenceBindings: List[CausalClosedAbsenceBinding]
  def supplementalPremiseUses: List[CausalSupplementalPremiseUse] = Nil
  def supplementalClosureBindings: List[CausalSupplementalClosureBinding] = Nil
  def stableKey: String

private[chessjudgment] final case class CausalClosedAbsenceUse private[judgment] (
    useId: String,
    pathOccurrenceId: String,
    binding: CausalClosedAbsenceBinding
):
  require(useId.matches("[0-9a-f]{64}"), "a closed absence use needs a canonical id")

private[chessjudgment] final case class CausalSupplementalClosureUse private[judgment] (
    useId: String,
    pathOccurrenceId: String,
    binding: CausalSupplementalClosureBinding
):
  require(useId.matches("[0-9a-f]{64}"), "a causal closure use needs a canonical id")

private[chessjudgment] final case class CausalProofPathOccurrence private (
    pathOccurrenceId: String,
    propositionId: String,
    manifest: BoundedCausalContractManifest,
    closedAbsenceUses: List[CausalClosedAbsenceUse],
    supplementalClosureUses: List[CausalSupplementalClosureUse]
):
  require(pathOccurrenceId.matches("[0-9a-f]{64}"), "a causal proof path needs a canonical id")
  require(
    closedAbsenceUses.forall(_.pathOccurrenceId == pathOccurrenceId) &&
      closedAbsenceUses.map(_.useId).distinct.size == closedAbsenceUses.size,
    "closed absence uses must retain their exact proof-path ownership"
  )
  require(
    supplementalClosureUses.forall(_.pathOccurrenceId == pathOccurrenceId) &&
      supplementalClosureUses.map(_.useId).distinct.size == supplementalClosureUses.size,
    "causal closure uses must retain their exact proof-path ownership"
  )

  def premiseUses: List[CausalRelationPremiseUse] = manifest.premiseUses

private[chessjudgment] object CausalProofPathOccurrence:
  def from(
      proposition: CausalPropositionIdentity,
      manifest: BoundedCausalContractManifest
  ): CausalProofPathOccurrence =
    require(
      proposition.contractKind == manifest.contractKind,
      "a causal proof path must use the proposition's exact contract"
    )
    val pathId = BoundedCausalIdentity.digest(
      List("causal-proof-path-occurrence:v1", proposition.semanticId, manifest.stableKey)
    )
    val uses = manifest.absenceBindings.map { binding =>
      new CausalClosedAbsenceUse(
        useId = BoundedCausalIdentity.digest(
          List("causal-closed-absence-use:v1", pathId, binding.stableKey)
        ),
        pathOccurrenceId = pathId,
        binding = binding
      )
    }
    val supplementalUses = manifest.supplementalClosureBindings.map { binding =>
      new CausalSupplementalClosureUse(
        useId = BoundedCausalIdentity.digest(
          List("causal-supplemental-closure-use:v1", pathId, binding.stableKey)
        ),
        pathOccurrenceId = pathId,
        binding = binding
      )
    }
    CausalProofPathOccurrence(pathId, proposition.semanticId, manifest, uses, supplementalUses)

private[chessjudgment] final case class CausalOccurrenceIdentity private (
    occurrenceId: String,
    propositionId: String,
    branches: List[CausalBranchOccurrence]
):
  require(occurrenceId.matches("[0-9a-f]{64}"), "a causal occurrence needs a canonical id")

  def branch(role: CausalBranchRole): Option[CausalBranchOccurrence] =
    branches.filter(_.role == role) match
      case exact :: Nil => Some(exact)
      case _            => None

private[chessjudgment] object CausalOccurrenceIdentity:
  def from(
      proposition: CausalPropositionIdentity,
      branches: List[CausalBranchOccurrence]
  ): CausalOccurrenceIdentity =
    require(
      branches.nonEmpty && branches.map(_.branchId).distinct.size == branches.size,
      "a causal occurrence needs non-empty uniquely owned branches"
    )
    require(
      branches.forall(_.rootPositionIdentity == proposition.rootPositionIdentity),
      "all causal branches must share the proposition's semantic root board"
    )
    val canonical = branches.sortBy(_.branchId)
    val occurrenceId = BoundedCausalIdentity.digest(
      List(
        "causal-occurrence:v1",
        proposition.semanticId,
        canonical.map(_.branchId).mkString("[", ",", "]")
      )
    )
    CausalOccurrenceIdentity(occurrenceId, proposition.semanticId, canonical)

/** One occurrence can retain several independent proof paths. Exact duplicate
  * path ids are rejected; no defensive winner selection is performed.
  */
private[chessjudgment] final case class BoundedCausalProofSet private (
    proposition: CausalPropositionIdentity,
    occurrence: CausalOccurrenceIdentity,
    paths: List[CausalProofPathOccurrence]
):
  require(paths.nonEmpty, "a causal occurrence needs at least one proof path")
  require(
    paths == paths.sortBy(_.pathOccurrenceId) &&
      paths.map(_.pathOccurrenceId).distinct.size == paths.size,
    "independent causal proof paths must be preserved once in canonical order"
  )

private[chessjudgment] object BoundedCausalProofSet:
  def from(
      proposition: CausalPropositionIdentity,
      occurrence: CausalOccurrenceIdentity,
      paths: List[CausalProofPathOccurrence]
  ): BoundedCausalProofSet =
    require(occurrence.propositionId == proposition.semanticId)
    require(paths.forall(_.propositionId == proposition.semanticId))
    require(paths.map(_.pathOccurrenceId).distinct.size == paths.size)
    val branchIds = occurrence.branches.map(_.branchId).toSet
    require(
      paths.flatMap(_.premiseUses).forall(use => branchIds(use.branchId)) &&
        paths.flatMap(_.manifest.supplementalPremiseUses).forall(use => use.branchIds.subsetOf(branchIds)) &&
        paths.flatMap(_.closedAbsenceUses).forall(use => branchIds(use.binding.branchId)) &&
        paths.flatMap(_.supplementalClosureUses).forall(use => use.binding.branchIds.subsetOf(branchIds)),
      "every premise and absence use must belong to a retained branch"
    )
    BoundedCausalProofSet(proposition, occurrence, paths.sortBy(_.pathOccurrenceId))

private[chessjudgment] trait BoundedCausalDependencyManifest:
  def contractKind: BoundedCausalContractKind
  def stableKey: String

private[chessjudgment] final case class BoundedCausalDependencyFingerprint private (
    value: String
):
  require(value.matches("[0-9a-f]{64}"), "a causal proof needs a complete dependency fingerprint")

private[chessjudgment] object BoundedCausalDependencyFingerprint:
  def from(manifest: BoundedCausalDependencyManifest): BoundedCausalDependencyFingerprint =
    BoundedCausalDependencyFingerprint(
      BoundedCausalIdentity.digest(
        List(
          "bounded-causal-dependency:v1",
          manifest.contractKind.toString.toLowerCase,
          manifest.stableKey
        )
      )
    )

private[chessjudgment] object BoundedCausalIdentity:
  def digest(parts: Iterable[String]): String =
    val exact = parts.iterator.map(value => s"${value.length}:$value").mkString("|")
    MessageDigest
      .getInstance("SHA-256")
      .digest(exact.getBytes(StandardCharsets.UTF_8))
      .iterator
      .map(byte => f"${byte & 0xff}%02x")
      .mkString

  def lineKey(line: LineNodeRef): String =
    List(
      line.id,
      line.role.toString,
      line.rank.toString,
      EvidenceRef.normalizeMove(line.rootMove)
    ).mkString(":")

  def stepKey(step: LineReplayStep): String =
    List(
      step.ply.toString,
      EvidenceRef.normalizeMove(step.moveUci),
      PrincipalVariationEvidence.normalizeFen(step.fenBefore),
      PrincipalVariationEvidence.normalizeFen(step.fenAfter)
    ).mkString(":")

  def coloredPieceKey(piece: RelationColoredPieceWitness): String =
    s"${piece.side.toString.toLowerCase}:${piece.role.name.toLowerCase}@${piece.square.key.toLowerCase}"

  def evidenceRecordKey(record: EvidenceRecord): String =
    val ref = record.ref
    List(
      ref.id,
      ref.producer.toString,
      ref.layer.toString,
      PrincipalVariationEvidence.normalizeFen(ref.position.fen),
      ref.position.ply.toString,
      ref.line.map(lineKey).getOrElse(""),
      ref.scope.toString,
      ref.confidence.toString,
      record.parents.map(_.id).sorted.mkString("[", ",", "]")
    ).mkString("|")
