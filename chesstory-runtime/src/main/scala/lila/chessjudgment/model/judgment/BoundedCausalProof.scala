package lila.chessjudgment.model.judgment

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import lila.chessjudgment.analysis.position.PositionRelationExtractor
import lila.chessjudgment.model.line.PrincipalVariationEvidence

private[chessjudgment] enum BoundedCausalContractKind:
  case ImmediateForcedReplyResourceDifferential
  case DefenseObligationChange
  case PassedPawnResultUnderClosedReplies

  def semanticNamespace: String =
    this match
      case ImmediateForcedReplyResourceDifferential =>
        "causal-proposition:immediate-forced-reply-resource-differential:v2"
      case DefenseObligationChange =>
        "causal-proposition:defense-obligation-change:v1"
      case PassedPawnResultUnderClosedReplies =>
        "causal-proposition:passed-pawn-result-under-closed-replies:v1"

private[chessjudgment] final case class SemanticPositionIdentity private (value: String)

private[chessjudgment] object SemanticPositionIdentity:
  def fromFen(fen: String): SemanticPositionIdentity =
    SemanticPositionIdentity(
      PrincipalVariationEvidence
        .semanticBoardStateFen(fen)
        .getOrElse(throw IllegalArgumentException("a causal proposition needs a valid root board"))
    )

/** Package-internal family extension capability. It carries semantic parts
  * only after a private family constructor has validated the exact lower
  * witnesses; the descriptor itself is not chess-proof authority.
  */
private[chessjudgment] trait CausalSemanticDescriptor:
  def contractKind: BoundedCausalContractKind
  def rootFen: String
  def semanticParts: List[String]

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
  private[judgment] def from(descriptor: CausalSemanticDescriptor): CausalPropositionIdentity =
    require(descriptor.semanticParts.nonEmpty, "a causal proposition needs exact semantic parts")
    val rootPositionIdentity = SemanticPositionIdentity.fromFen(descriptor.rootFen)
    val semanticId = BoundedCausalIdentity.digest(
      descriptor.contractKind.semanticNamespace :: rootPositionIdentity.value :: descriptor.semanticParts
    )
    CausalPropositionIdentity(semanticId, descriptor.contractKind, rootPositionIdentity)

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

/** Package-internal family extension capability for one certified
  * non-adjacent occurrence jump. Private family tokens validate the lower
  * dependency; this projection itself does not certify chess truth.
  */
private[chessjudgment] trait CausalCertifiedDependencyAuthority:
  def before: LineReplayStep
  def after: LineReplayStep
  def lowerProofKey: String

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

  private[judgment] def fromCertifiedDependency(
      authority: CausalCertifiedDependencyAuthority
  ): CausalOccurrenceLink =
    require(authority.after.ply > authority.before.ply, "a certified dependency must move to a later occurrence")
    require(authority.lowerProofKey.nonEmpty, "a certified dependency needs its exact lower authority")
    CausalOccurrenceLink(
      CausalOccurrenceLinkKind.CertifiedCausalDependency,
      BoundedCausalIdentity.stepKey(authority.before),
      BoundedCausalIdentity.stepKey(authority.after),
      authority.lowerProofKey
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
    fromCertifiedOccurrences(
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
    fromCertifiedOccurrences(
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

  private[judgment] def fromCertifiedOccurrences(
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

  private[judgment] def rootProvenanceFor(line: LineNodeRef): CausalRootProvenance =
    line.role match
      case LineNodeRole.Played => CausalRootProvenance.ObservedGameRoot
      case _                   => CausalRootProvenance.CounterfactualAnalyzedRoot

private[chessjudgment] trait CausalPremiseRole:
  def stableKey: String

private[chessjudgment] trait CausalSupplementalPremiseUse:
  def branchIds: Set[String]
  def stableKey: String

private[chessjudgment] final case class CausalVerticalRelationPremiseUse private (
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

private[chessjudgment] object CausalVerticalRelationPremiseUse:
  def from(
      role: CausalPremiseRole,
      occurrence: ReplayVerticalRelationOccurrence,
      branch: CausalBranchOccurrence,
      stepIndex: Int
  ): CausalVerticalRelationPremiseUse =
    require(
      branch.stepAt(stepIndex).exists(_.step == occurrence.step),
      "a relation premise must bind its exact certified replay occurrence"
    )
    val sourcePremiseIds = occurrence.certifiedSourcePremiseIds
    CausalVerticalRelationPremiseUse(
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

private[chessjudgment] trait CausalStateRole:
  def stableKey: String

private[chessjudgment] trait CausalSupplementalClosureBinding:
  def branchIds: Set[String]
  def stableKey: String

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
      issuerOccurrence: ReplayPositionOccurrence
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
            replay.positionAfter(step).exists(exactOccurrence =>
              exactOccurrence.occurrenceId == issuerOccurrence.occurrenceId &&
                exactOccurrence.sameOwner(issuerOccurrence) &&
                exactOccurrence.certifies(proof, branch.line.role.scope)
            )
          )
      case _ => false
    require(
      issuerCertified && issuerOccurrence.step == step &&
        issuerOccurrence.certifies(proof, branch.line.role.scope),
      "a closed absence must name its exact graph-owned LegalLine position occurrence"
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

/** Semantic positive-state id plus one exact branch use. The proof remains
  * owned by the replay position inventory that certified the full typed
  * query; this binding only adds LegalLine and causal-path coordinates.
  */
private[chessjudgment] final case class CausalClosedStateBinding private (
    role: CausalStateRole,
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
  require(semanticProofId.matches("[0-9a-f]{64}"), "a closed state needs its semantic id")
  require(issuerEvidenceId.nonEmpty, "a closed state needs its exact LegalLine issuer")
  require(issuerOccurrenceId.matches("[0-9a-f]{64}"), "a closed state needs its exact replay occurrence")
  require(queryKey.nonEmpty, "a closed state needs its exact inventory query")

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

private[chessjudgment] object CausalClosedStateBinding:
  def afterStep(
      role: CausalStateRole,
      proof: PositionRelationExtractor.ClosedPositionStateProof,
      branch: CausalBranchOccurrence,
      stepIndex: Int,
      issuerRecord: EvidenceRecord,
      issuerOccurrence: ReplayPositionOccurrence
  ): CausalClosedStateBinding =
    val step = branch.stepAt(stepIndex).map(_.step)
      .getOrElse(throw IllegalArgumentException("a closed state must bind a retained branch step"))
    require(
      step.ply == proof.position.ply &&
        PrincipalVariationEvidence.sameBoardState(step.fenAfter, proof.position.fen),
      "a closed state position must be the exact after occurrence of its branch step"
    )
    require(
      proof.scope == branch.line.role.scope,
      "a closed state must retain its exact branch inventory scope"
    )
    val issuerCertified = issuerRecord match
      case EvidenceRecord(ref, line: LineFactEvidence, _) =>
        ref.producer == EvidenceProducer.LegalLineProducer &&
          ref.layer == EvidenceLayer.Line &&
          ref.confidence == EvidenceConfidence.LegalReplayVerified &&
          ref.line.contains(branch.line) && ref.scope == branch.line.role.scope &&
          line.line == branch.line && line.certifiedReplay.exists(replay =>
            replay.positionAfter(step).exists(exactOccurrence =>
              exactOccurrence.occurrenceId == issuerOccurrence.occurrenceId &&
                exactOccurrence.sameOwner(issuerOccurrence) &&
                exactOccurrence.certifies(proof, branch.line.role.scope)
            )
          )
      case _ => false
    require(
      issuerCertified && issuerOccurrence.step == step &&
        issuerOccurrence.certifies(proof, branch.line.role.scope),
      "a closed state must name its exact graph-owned LegalLine position occurrence"
    )
    val semanticBoard = PrincipalVariationEvidence
      .semanticBoardStateFen(proof.position.fen)
      .getOrElse(throw IllegalArgumentException("a closed state needs a semantic board state"))
    val semanticProofId = BoundedCausalIdentity.digest(
      List("closed-position-state:v1", semanticBoard, proof.query.stableKey)
    )
    CausalClosedStateBinding(
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
  def premiseUses: List[CausalVerticalRelationPremiseUse]
  def absenceBindings: List[CausalClosedAbsenceBinding]
  def stateBindings: List[CausalClosedStateBinding] = Nil
  def supplementalPremiseUses: List[CausalSupplementalPremiseUse] = Nil
  def supplementalClosureBindings: List[CausalSupplementalClosureBinding] = Nil
  def stableKey: String

private[chessjudgment] final case class CausalClosedAbsenceUse private[judgment] (
    useId: String,
    pathOccurrenceId: String,
    binding: CausalClosedAbsenceBinding
):
  require(useId.matches("[0-9a-f]{64}"), "a closed absence use needs a canonical id")

private[chessjudgment] final case class CausalClosedStateUse private[judgment] (
    useId: String,
    pathOccurrenceId: String,
    binding: CausalClosedStateBinding
):
  require(useId.matches("[0-9a-f]{64}"), "a closed state use needs a canonical id")

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
    closedStateUses: List[CausalClosedStateUse],
    supplementalClosureUses: List[CausalSupplementalClosureUse]
):
  require(pathOccurrenceId.matches("[0-9a-f]{64}"), "a causal proof path needs a canonical id")
  require(
    closedAbsenceUses.forall(_.pathOccurrenceId == pathOccurrenceId) &&
      closedAbsenceUses.map(_.useId).distinct.size == closedAbsenceUses.size,
    "closed absence uses must retain their exact proof-path ownership"
  )
  require(
    closedStateUses.forall(_.pathOccurrenceId == pathOccurrenceId) &&
      closedStateUses.map(_.useId).distinct.size == closedStateUses.size,
    "closed state uses must retain their exact proof-path ownership"
  )
  require(
    supplementalClosureUses.forall(_.pathOccurrenceId == pathOccurrenceId) &&
      supplementalClosureUses.map(_.useId).distinct.size == supplementalClosureUses.size,
    "causal closure uses must retain their exact proof-path ownership"
  )

  def premiseUses: List[CausalVerticalRelationPremiseUse] = manifest.premiseUses

private[chessjudgment] object CausalProofPathOccurrence:
  def from(
      proposition: CausalPropositionIdentity,
      manifest: BoundedCausalContractManifest
  ): CausalProofPathOccurrence =
    require(
      proposition.contractKind == manifest.contractKind,
      "a causal proof path must use the proposition's exact contract"
    )
    val completeManifestKey = List(
      manifest.stableKey,
      manifest.premiseUses.map(_.stableKey).mkString("premises[", ",", "]"),
      manifest.absenceBindings.map(_.stableKey).mkString("absences[", ",", "]"),
      manifest.stateBindings.map(_.stableKey).mkString("states[", ",", "]"),
      manifest.supplementalPremiseUses.map(_.stableKey).mkString("supplemental-premises[", ",", "]"),
      manifest.supplementalClosureBindings.map(_.stableKey).mkString("supplemental-closures[", ",", "]")
    ).mkString("|")
    val pathId = BoundedCausalIdentity.digest(
      List("causal-proof-path-occurrence:v2", proposition.semanticId, completeManifestKey)
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
    val stateUses = manifest.stateBindings.map { binding =>
      new CausalClosedStateUse(
        useId = BoundedCausalIdentity.digest(
          List("causal-closed-state-use:v1", pathId, binding.stableKey)
        ),
        pathOccurrenceId = pathId,
        binding = binding
      )
    }
    CausalProofPathOccurrence(pathId, proposition.semanticId, manifest, uses, stateUses, supplementalUses)

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
        paths.flatMap(_.closedStateUses).forall(use => branchIds(use.binding.branchId)) &&
        paths.flatMap(_.supplementalClosureUses).forall(use => use.binding.branchIds.subsetOf(branchIds)),
      "every premise and absence use must belong to a retained branch"
    )
    BoundedCausalProofSet(proposition, occurrence, paths.sortBy(_.pathOccurrenceId))

/** Read-only wire projection for common branch, vertical-relation premise,
  * and closed-relation-absence coordinates. Family-specific supplemental
  * premises stay in their own typed projection and may never be dropped here.
  */
final case class BoundedCausalPublicStep private[chessjudgment] (
    index: Int,
    provenance: String,
    ply: Int,
    moveUci: String,
    fenBefore: String,
    fenAfter: String
)

final case class BoundedCausalPublicBranch private[chessjudgment] (
    branchId: String,
    line: LineNodeRef,
    role: String,
    rootProvenance: String,
    steps: List[BoundedCausalPublicStep]
)

final case class BoundedCausalPublicPremiseUse private[chessjudgment] (
    role: String,
    contract: String,
    resultId: String,
    sourcePremiseIds: List[String],
    branchId: String,
    branchRole: String,
    stepIndex: Int
)

final case class BoundedCausalPublicClosedAbsenceUse private[chessjudgment] (
    useId: String,
    role: String,
    semanticProofId: String,
    issuerEvidenceId: String,
    issuerOccurrenceId: String,
    query: String,
    branchId: String,
    branchRole: String,
    afterStepIndex: Int,
    position: PositionNodeRef,
    scope: EvidenceScope
)

final case class BoundedCausalPublicProofPath private[chessjudgment] (
    pathOccurrenceId: String,
    premises: List[BoundedCausalPublicPremiseUse],
    closedAbsenceUses: List[BoundedCausalPublicClosedAbsenceUse]
)

private[chessjudgment] object BoundedCausalPublicProjection:
  def branch(branch: CausalBranchOccurrence): BoundedCausalPublicBranch =
    BoundedCausalPublicBranch(
      branch.branchId,
      branch.line,
      branch.role.stableKey,
      branch.rootProvenance.toString,
      branch.steps.map(step =>
        BoundedCausalPublicStep(
          step.index,
          step.provenance.toString,
          step.step.ply,
          step.step.moveUci,
          step.step.fenBefore,
          step.step.fenAfter
        )
      )
    )

  def paths(paths: List[CausalProofPathOccurrence]): List[BoundedCausalPublicProofPath] =
    require(
      paths.forall(path =>
        path.closedStateUses.isEmpty && path.manifest.supplementalPremiseUses.isEmpty &&
          path.supplementalClosureUses.isEmpty
      ),
      "the common public causal projection cannot omit closed-state or family-specific proof uses"
    )
    paths.map(path =>
      BoundedCausalPublicProofPath(
        path.pathOccurrenceId,
        path.premiseUses.map(premise =>
          BoundedCausalPublicPremiseUse(
            premise.role.stableKey,
            premise.contract.toString,
            premise.result.stableKey,
            premise.sourcePremiseIds,
            premise.branchId,
            premise.branchRole.stableKey,
            premise.stepIndex
          )
        ),
        path.closedAbsenceUses.map { use =>
          val binding = use.binding
          BoundedCausalPublicClosedAbsenceUse(
            use.useId,
            binding.role.stableKey,
            binding.semanticProofId,
            binding.issuerEvidenceId,
            binding.issuerOccurrenceId,
            binding.queryKey,
            binding.branchId,
            binding.branchRole.stableKey,
            binding.afterStepIndex,
            binding.position,
            binding.scope
          )
        }
      )
    )

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
