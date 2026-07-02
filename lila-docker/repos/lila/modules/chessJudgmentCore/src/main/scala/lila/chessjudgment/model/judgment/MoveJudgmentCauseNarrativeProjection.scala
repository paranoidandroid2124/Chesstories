package lila.chessjudgment.model.judgment

object MoveJudgmentCauseNarrativeProjection:
  def defaultNarrativeRole(role: MoveJudgmentCauseFrameRole): MoveJudgmentCauseNarrativeRole =
    role match
      case MoveJudgmentCauseFrameRole.PrimaryCause   => MoveJudgmentCauseNarrativeRole.RootCause
      case MoveJudgmentCauseFrameRole.SecondaryCause => MoveJudgmentCauseNarrativeRole.SupportingCause
      case MoveJudgmentCauseFrameRole.ContextCause   => MoveJudgmentCauseNarrativeRole.ContextCause

  def withNarrativeRoles(graph: TypedEvidenceGraph, frames: List[MoveJudgmentCauseFrame]): List[MoveJudgmentCauseFrame] =
    val selectedRootsByComparison =
      selectedRootFramesByComparison(graph, frames)
    val selectedRootIds =
      selectedRootsByComparison.values.flatten.map(causeFrameIdentity).toSet
    val selectedLongTermRootsByComparison =
      selectedRootsByComparison.view
        .mapValues(_.filter(longTermRootFrame))
        .toMap
    frames.map { frame =>
      val arbitrationTier = rootArbitrationProfile(graph, frame).tier
      val frameWithTier = frame.copy(rootArbitrationTier = arbitrationTier)
      val matchingLongTermRoots = selectedLongTermRootsByComparison.getOrElse(comparisonFrameKey(frame), Nil)
      if selectedRootIds.contains(causeFrameIdentity(frame)) then
        if longTermRootFrame(frame) then
          val witnesses =
            frames
              .filter(candidate => sameComparison(candidate, frame) && tacticalWitnessNarrativeFrame(graph, candidate))
              .filterNot(candidate => causeFrameIdentity(candidate) == causeFrameIdentity(frame))
          val witnessBindings = witnesses.map(witness => witness -> witnessBinding(frame, witness))
          val punishmentWitnesses =
            witnessBindings.collect { case (witness, binding) if binding.level == MoveJudgmentCauseWitnessBindingLevel.Punishment =>
              witness
            }
          val contextualWitnesses =
            witnessBindings.collect {
              case (witness, binding)
                  if binding.level != MoveJudgmentCauseWitnessBindingLevel.Punishment &&
                    binding.level != MoveJudgmentCauseWitnessBindingLevel.SameComparisonOnly =>
                witness
            }
          val boundWitnesses = (punishmentWitnesses ++ contextualWitnesses).distinctBy(causeFrameIdentity)
          frameWithTier.copy(
            narrativeRole = MoveJudgmentCauseNarrativeRole.RootCause,
            tacticalWitnessCauseEvidenceIds = boundWitnesses.flatMap(_.causeEvidenceIds).distinct.sorted,
            tacticalWitnessCauseKinds = boundWitnesses.map(_.causeKind).distinct,
            punishmentWitnessCauseEvidenceIds = punishmentWitnesses.flatMap(_.causeEvidenceIds).distinct.sorted,
            punishmentWitnessCauseKinds = punishmentWitnesses.map(_.causeKind).distinct,
            contextualTacticalWitnessCauseEvidenceIds = contextualWitnesses.flatMap(_.causeEvidenceIds).distinct.sorted,
            contextualTacticalWitnessCauseKinds = contextualWitnesses.map(_.causeKind).distinct
          )
        else frameWithTier.copy(narrativeRole = MoveJudgmentCauseNarrativeRole.RootCause)
      else if tacticalWitnessNarrativeFrame(graph, frame) && matchingLongTermRoots.nonEmpty then
        val binding = strongestWitnessBinding(frame, matchingLongTermRoots)
        if binding.level == MoveJudgmentCauseWitnessBindingLevel.SameComparisonOnly then
          frameWithTier.copy(
            narrativeRole = MoveJudgmentCauseNarrativeRole.ContextCause,
            witnessBindingLevel = binding.level,
            witnessBindingSignals = binding.signals,
            witnessBindingRootCauseEvidenceIds = binding.rootCauseEvidenceIds
          )
        else
          frameWithTier.copy(
            narrativeRole = MoveJudgmentCauseNarrativeRole.TacticalWitness,
            witnessBindingLevel = binding.level,
            witnessBindingSignals = binding.signals,
            witnessBindingRootCauseEvidenceIds = binding.rootCauseEvidenceIds
          )
      else if arbitrationManagedPrimaryFrame(frame) &&
          (selectedRootsByComparison.contains(comparisonFrameKey(frame)) ||
            arbitrationTier == MoveJudgmentCauseRootArbitrationTier.ContextOnly ||
            (arbitrationTier == MoveJudgmentCauseRootArbitrationTier.BroadOwnedRoot &&
              broadObjectSensitiveRootKind(frame.causeKind)))
      then
        frameWithTier.copy(narrativeRole = MoveJudgmentCauseNarrativeRole.SupportingCause)
      else
        frameWithTier.copy(narrativeRole = defaultNarrativeRole(frame.role))
    }

  private final case class RootArbitrationProfile(
      tier: MoveJudgmentCauseRootArbitrationTier,
      fallbackKind: Boolean,
      eventKind: Boolean
  )

  private def selectedRootFramesByComparison(
      graph: TypedEvidenceGraph,
      frames: List[MoveJudgmentCauseFrame]
  ): Map[(CandidateComparisonKind, LineNodeRef, LineNodeRef), List[MoveJudgmentCauseFrame]] =
    frames
      .filter(arbitrationManagedPrimaryFrame)
      .groupBy(comparisonFrameKey)
      .view
      .mapValues(selectedRootFrames(graph, _))
      .filter { case (_, selected) => selected.nonEmpty }
      .toMap

  private def selectedRootFrames(graph: TypedEvidenceGraph, frames: List[MoveJudgmentCauseFrame]): List[MoveJudgmentCauseFrame] =
    val profiled = frames.map(frame => frame -> rootArbitrationProfile(graph, frame))
    val qualifiedLongTermRoots =
      profiled.filter { case (frame, profile) =>
        longTermRootFrame(frame) &&
          !profile.fallbackKind &&
          (
            profile.tier == MoveJudgmentCauseRootArbitrationTier.ExactOwnedRoot ||
              profile.tier == MoveJudgmentCauseRootArbitrationTier.ConcreteOwnedRoot ||
              (profile.tier == MoveJudgmentCauseRootArbitrationTier.BroadOwnedRoot && !broadObjectSensitiveRootKind(frame.causeKind))
          )
      }
    val eventRoots = profiled.filter { case (_, profile) =>
      profile.eventKind &&
        (profile.tier == MoveJudgmentCauseRootArbitrationTier.ConcreteOwnedRoot || profile.tier == MoveJudgmentCauseRootArbitrationTier.ExactOwnedRoot)
    }
    val terminalEventRoots = eventRoots.filter { case (frame, _) => terminalProofEventRoot(frame) }
    val fallbackRoots = profiled.filter { case (_, profile) => profile.tier == MoveJudgmentCauseRootArbitrationTier.FallbackRoot }
    val selected =
      if terminalEventRoots.nonEmpty then selectedEventRootFrames(graph, terminalEventRoots.map(_._1))
      else if qualifiedLongTermRoots.nonEmpty then qualifiedLongTermRoots.map(_._1)
      else if eventRoots.nonEmpty then selectedEventRootFrames(graph, eventRoots.map(_._1))
      else selectedFallbackRootFrames(fallbackRoots.map(_._1))
    selected.distinctBy(causeFrameIdentity)

  private def terminalProofEventRoot(frame: MoveJudgmentCauseFrame): Boolean =
    frame.proofLineConsequences.exists(LineConsequenceKind.terminalResultProof)

  private def selectedEventRootFrames(graph: TypedEvidenceGraph, frames: List[MoveJudgmentCauseFrame]): List[MoveJudgmentCauseFrame] =
    frames.sortBy(eventRootSortKey(graph, _)).lastOption.toList

  private def selectedFallbackRootFrames(frames: List[MoveJudgmentCauseFrame]): List[MoveJudgmentCauseFrame] =
    frames.sortBy(fallbackRootSortKey).lastOption.toList

  private def eventRootSortKey(graph: TypedEvidenceGraph, frame: MoveJudgmentCauseFrame): (Int, Int, Int, Int, Int, Int, String) =
    (
      eventRootKindRank(graph, frame),
      boolRank(rootArbitrationObjectReady(frame)),
      boolRank(frame.attributionRootMoveMatched),
      boolRank(frame.attributionDirectProofEligible),
      frame.proofDirectSourceIds.distinct.size,
      sourceSideRank(frame.causeSourceSide),
      frame.causeKind.toString
    )

  private def fallbackRootSortKey(frame: MoveJudgmentCauseFrame): (Int, Int, Int, Int, String) =
    (
      fallbackRootKindRank(frame.causeKind),
      boolRank(rootArbitrationObjectReady(frame)),
      frame.proofDirectSourceIds.distinct.size,
      sourceSideRank(frame.causeSourceSide),
      frame.causeKind.toString
    )

  private def eventRootKindRank(graph: TypedEvidenceGraph, frame: MoveJudgmentCauseFrame): Int =
    frame.causeKind match
      case RelativeCauseKind.WrongRecapturer if eventRootCaptureBound(graph, frame) =>
        110
      case RelativeCauseKind.WrongMoveOrder | RelativeCauseKind.TempoLoss =>
        100
      case RelativeCauseKind.TacticalRefutationOfPlayed | RelativeCauseKind.CandidateTacticalLiability =>
        95
      case RelativeCauseKind.MissedTacticalResource | RelativeCauseKind.KingForcing =>
        90
      case RelativeCauseKind.ConversionMiss | RelativeCauseKind.ConversionSecured =>
        70
      case RelativeCauseKind.RecaptureRecoveryWindow | RelativeCauseKind.MaterialSwing =>
        65
      case RelativeCauseKind.OnlyMoveNecessity | RelativeCauseKind.OnlyDefenseNecessity =>
        55
      case RelativeCauseKind.DefensiveResource | RelativeCauseKind.DrawResource =>
        45
      case RelativeCauseKind.SacrificeCompensation =>
        40
      case _ =>
        0

  private def fallbackRootKindRank(kind: RelativeCauseKind): Int =
    kind match
      case RelativeCauseKind.PlanContradiction => 2
      case RelativeCauseKind.PlanImprovement   => 1
      case _                                   => 0

  private def sourceSideRank(side: RelativeCauseSourceSide): Int =
    side match
      case RelativeCauseSourceSide.Candidate | RelativeCauseSourceSide.Reference => 1
      case RelativeCauseSourceSide.Mixed | RelativeCauseSourceSide.Shared        => 0

  private def eventRootSelectable(graph: TypedEvidenceGraph, frame: MoveJudgmentCauseFrame): Boolean =
    frame.causeKind != RelativeCauseKind.WrongRecapturer || eventRootCaptureBound(graph, frame)

  private def eventRootCaptureBound(graph: TypedEvidenceGraph, frame: MoveJudgmentCauseFrame): Boolean =
    EvidenceRecord.hasRootCaptureEvent(frameEvidenceRecords(graph, frame), frame.eventRootMove)

  private def frameEvidenceRecords(graph: TypedEvidenceGraph, frame: MoveJudgmentCauseFrame): List[EvidenceRecord] =
    (
      frame.evidenceIds ++
        frame.causeEvidenceIds ++
        frame.proofDirectSourceIds ++
        frame.proofContrastSourceIds ++
        frame.proofContextSupportSourceIds ++
        frame.proofStrategicMechanismSourceIds ++
        frame.proofStrategicMechanismSignalSourceIds ++
        frame.supportEvidenceSourceIds
    ).distinct.flatMap(id => graph.byId.get(id))

  private def boolRank(value: Boolean): Int =
    if value then 1 else 0

  private def rootArbitrationProfile(graph: TypedEvidenceGraph, frame: MoveJudgmentCauseFrame): RootArbitrationProfile =
    val lineOwned = frame.attributionDirectProofEligible && frame.attributionRootMoveMatched
    val fallbackKind = fallbackRootKind(frame.causeKind)
    val arbitrationObjectReady =
      if broadObjectSensitiveRootKind(frame.causeKind) then directProofSpecificTargetReady(frame)
      else rootArbitrationObjectReady(frame)
    val eventKind = tacticalWitnessFrame(frame) && eventRootSelectable(graph, frame)
    val tier =
      if !lineOwned then MoveJudgmentCauseRootArbitrationTier.ContextOnly
      else if fallbackKind && fallbackRootProofReady(frame) then MoveJudgmentCauseRootArbitrationTier.FallbackRoot
      else if fallbackKind then MoveJudgmentCauseRootArbitrationTier.ContextOnly
      else if longTermRootFrame(frame) && arbitrationObjectReady then MoveJudgmentCauseRootArbitrationTier.ExactOwnedRoot
      else if eventKind then MoveJudgmentCauseRootArbitrationTier.ConcreteOwnedRoot
      else if arbitrationObjectReady then MoveJudgmentCauseRootArbitrationTier.ConcreteOwnedRoot
      else if longTermRootFrame(frame) then MoveJudgmentCauseRootArbitrationTier.BroadOwnedRoot
      else MoveJudgmentCauseRootArbitrationTier.ContextOnly
    RootArbitrationProfile(
      tier = tier,
      fallbackKind = fallbackKind,
      eventKind = eventKind
    )

  private def rootArbitrationObjectReady(frame: MoveJudgmentCauseFrame): Boolean =
    frame.concreteObjectReady || directProofSpecificTargetReady(frame)

  private def directProofSpecificTargetReady(frame: MoveJudgmentCauseFrame): Boolean =
    EvidenceObjectBinding.directProofSpecificTargetReady(frame.objectBindingSignatures)

  private def arbitrationManagedPrimaryFrame(frame: MoveJudgmentCauseFrame): Boolean =
    frame.role == MoveJudgmentCauseFrameRole.PrimaryCause &&
      frame.comparisonKind == CandidateComparisonKind.PlayedVsBest &&
      frame.causeRole == RelativeCauseRole.PrimaryPlayedCause &&
      frame.causeImportance == RelativeCauseImportance.Primary

  private def fallbackRootKind(kind: RelativeCauseKind): Boolean =
    kind == RelativeCauseKind.PlanContradiction ||
      kind == RelativeCauseKind.PlanImprovement

  private def fallbackRootProofReady(frame: MoveJudgmentCauseFrame): Boolean =
    frame.causeKind match
      case RelativeCauseKind.PlanContradiction | RelativeCauseKind.PlanImprovement =>
        directPlanCoherenceProofReady(frame)
      case _ =>
        true

  private def directPlanCoherenceProofReady(frame: MoveJudgmentCauseFrame): Boolean =
    val directSourceIds = frame.proofDirectSourceIds.toSet
    frame.proofStrategicAxisLineage.exists(lineage =>
      lineage.axisKind == StrategicAxisKind.PlanCoherence &&
        directSourceIds.contains(lineage.mechanismEvidenceId)
    )

  private def broadObjectSensitiveRootKind(kind: RelativeCauseKind): Boolean =
    kind == RelativeCauseKind.ActivityGain ||
      kind == RelativeCauseKind.ActivityLoss ||
      kind == RelativeCauseKind.TargetPressureGain ||
      kind == RelativeCauseKind.TargetPressureRelease ||
      kind == RelativeCauseKind.PawnBreakOpportunity ||
      kind == RelativeCauseKind.PawnWeaknessTarget ||
      kind == RelativeCauseKind.CenterControlGain ||
      kind == RelativeCauseKind.StructuralImprovement ||
      kind == RelativeCauseKind.OpponentRestriction ||
      kind == RelativeCauseKind.KingSafetyConcession ||
      fallbackRootKind(kind)

  private def causeFrameIdentity(frame: MoveJudgmentCauseFrame): (Option[String], List[String], RelativeCauseKind, CandidateComparisonKind, RelativeCauseRole, RelativeCauseSourceSide, RelativeCauseImportance, LineNodeRef) =
    (
      frame.clusterId,
      frame.causeEvidenceIds.distinct.sorted,
      frame.causeKind,
      frame.comparisonKind,
      frame.causeRole,
      frame.causeSourceSide,
      frame.causeImportance,
      frame.eventLine
    )

  private final case class TacticalWitnessBinding(
      level: MoveJudgmentCauseWitnessBindingLevel,
      signals: List[MoveJudgmentCauseWitnessBindingSignal],
      rootCauseEvidenceIds: List[String]
  )

  private def strongestWitnessBinding(
      witness: MoveJudgmentCauseFrame,
      roots: List[MoveJudgmentCauseFrame]
  ): TacticalWitnessBinding =
    roots
      .filter(root => sameComparison(root, witness))
      .map(root => witnessBinding(root, witness))
      .sortBy(binding => witnessBindingRank(binding.level))
      .lastOption
      .getOrElse(
        TacticalWitnessBinding(
          MoveJudgmentCauseWitnessBindingLevel.SameComparisonOnly,
          List(MoveJudgmentCauseWitnessBindingSignal.SameComparison),
          Nil
        )
      )

  private def witnessBinding(
      root: MoveJudgmentCauseFrame,
      witness: MoveJudgmentCauseFrame
  ): TacticalWitnessBinding =
    val rootSignatures = root.objectBindingSignatures.toSet
    val witnessSignatures = witness.objectBindingSignatures.toSet
    val rootDirectSignatures = objectSignatures(root, Some(RelativeCauseProofRole.DirectProof))
    val witnessDirectSignatures = objectSignatures(witness, Some(RelativeCauseProofRole.DirectProof))
    val sharedExactSignatures = rootSignatures.intersect(witnessSignatures)
    val sharedDirectExactSignatures = rootDirectSignatures.intersect(witnessDirectSignatures)
    val sharedExact = sharedExactSignatures.nonEmpty
    val sharedSurfaceExact = EvidenceObjectBinding.playerFacingReadySignatures(sharedExactSignatures)
    val sharedActor = sharedSpecificActorToken(root, witness)
    val sharedTarget = sharedConcreteTargetToken(root, witness)
    val sharedMechanism = sharedObjectToken(root, witness, "mechanism")
    val sharedConsequence = sharedObjectToken(root, witness, "consequence")
    val sharedWitness = sharedObjectToken(root, witness, "witness")
    val sharedDirectExact = sharedDirectExactSignatures.nonEmpty
    val sharedDirectSurfaceExact = EvidenceObjectBinding.directProofSpecificTargetReady(sharedDirectExactSignatures)
    val sharedDirectActor = sharedSpecificActorToken(root, witness, Some(RelativeCauseProofRole.DirectProof))
    val sharedDirectTarget = sharedConcreteTargetToken(root, witness, Some(RelativeCauseProofRole.DirectProof))
    val sharedDirectMechanism = sharedObjectToken(root, witness, "mechanism", Some(RelativeCauseProofRole.DirectProof))
    val sharedDirectConsequence = sharedObjectToken(root, witness, "consequence", Some(RelativeCauseProofRole.DirectProof))
    val sameEvent = root.eventLine == witness.eventLine
    val directProofOverlap = root.proofDirectSourceIds.toSet.intersect(witness.proofDirectSourceIds.toSet).nonEmpty
    val graphBound = sameEvent || directProofOverlap
    val signals =
      List(
        Some(MoveJudgmentCauseWitnessBindingSignal.SameComparison),
        Option.when(sharedExact)(MoveJudgmentCauseWitnessBindingSignal.SharedExactObjectSignature),
        Option.when(sharedActor)(MoveJudgmentCauseWitnessBindingSignal.SharedActor),
        Option.when(sharedTarget)(MoveJudgmentCauseWitnessBindingSignal.SharedTarget),
        Option.when(sharedMechanism)(MoveJudgmentCauseWitnessBindingSignal.SharedMechanism),
        Option.when(sharedConsequence)(MoveJudgmentCauseWitnessBindingSignal.SharedConsequence),
        Option.when(sharedWitness)(MoveJudgmentCauseWitnessBindingSignal.SharedWitness),
        Option.when(sharedDirectExact)(MoveJudgmentCauseWitnessBindingSignal.SharedDirectObjectSignature),
        Option.when(sharedDirectActor)(MoveJudgmentCauseWitnessBindingSignal.SharedDirectActor),
        Option.when(sharedDirectTarget)(MoveJudgmentCauseWitnessBindingSignal.SharedDirectTarget),
        Option.when(sharedDirectMechanism)(MoveJudgmentCauseWitnessBindingSignal.SharedDirectMechanism),
        Option.when(sharedDirectConsequence)(MoveJudgmentCauseWitnessBindingSignal.SharedDirectConsequence),
        Option.when(sameEvent)(MoveJudgmentCauseWitnessBindingSignal.SameEventLine),
        Option.when(directProofOverlap)(MoveJudgmentCauseWitnessBindingSignal.DirectProofSourceOverlap)
      ).flatten.distinct.sortBy(_.toString)
    TacticalWitnessBinding(
      level = witnessBindingLevel(
        objectBound = graphBound && (sharedSurfaceExact || sharedActor || sharedTarget),
        directObjectBound = graphBound && (sharedDirectSurfaceExact || sharedDirectActor || sharedDirectTarget),
        mechanismBound = graphBound && sharedMechanism,
        consequenceBound = graphBound && sharedConsequence,
        directMechanismBound = graphBound && sharedDirectMechanism,
        directConsequenceBound = graphBound && sharedDirectConsequence,
        lineOrEvalBound = graphBound,
        eventOrEvalBound = graphBound,
        ownedTacticalProof = witness.hasOwnedTacticalProof
      ),
      signals = signals,
      rootCauseEvidenceIds = root.causeEvidenceIds.distinct.sorted
    )

  private def witnessBindingLevel(
      objectBound: Boolean,
      directObjectBound: Boolean,
      mechanismBound: Boolean,
      consequenceBound: Boolean,
      directMechanismBound: Boolean,
      directConsequenceBound: Boolean,
      lineOrEvalBound: Boolean,
      eventOrEvalBound: Boolean,
      ownedTacticalProof: Boolean
  ): MoveJudgmentCauseWitnessBindingLevel =
    if ownedTacticalProof && directObjectBound && directMechanismBound && directConsequenceBound && eventOrEvalBound then
      MoveJudgmentCauseWitnessBindingLevel.Punishment
    else if objectBound && (mechanismBound || consequenceBound) then
      MoveJudgmentCauseWitnessBindingLevel.ObjectContext
    else if lineOrEvalBound then MoveJudgmentCauseWitnessBindingLevel.LineContext
    else MoveJudgmentCauseWitnessBindingLevel.SameComparisonOnly

  private def sharedObjectToken(
      left: MoveJudgmentCauseFrame,
      right: MoveJudgmentCauseFrame,
      role: String,
      proofRole: Option[RelativeCauseProofRole] = None
  ): Boolean =
    objectTokens(left, role, proofRole).intersect(objectTokens(right, role, proofRole)).nonEmpty

  private def sharedSpecificActorToken(
      left: MoveJudgmentCauseFrame,
      right: MoveJudgmentCauseFrame,
      proofRole: Option[RelativeCauseProofRole] = None
  ): Boolean =
    filteredObjectTokens(left, "actor", proofRole, EvidenceObjectBinding.specificActorToken)
      .intersect(filteredObjectTokens(right, "actor", proofRole, EvidenceObjectBinding.specificActorToken))
      .nonEmpty

  private def sharedConcreteTargetToken(
      left: MoveJudgmentCauseFrame,
      right: MoveJudgmentCauseFrame,
      proofRole: Option[RelativeCauseProofRole] = None
  ): Boolean =
    filteredObjectTokens(left, "target", proofRole, EvidenceObjectBinding.concreteTargetToken)
      .intersect(filteredObjectTokens(right, "target", proofRole, EvidenceObjectBinding.concreteTargetToken))
      .nonEmpty

  private def filteredObjectTokens(
      frame: MoveJudgmentCauseFrame,
      role: String,
      proofRole: Option[RelativeCauseProofRole],
      keep: String => Boolean
  ): Set[String] =
    val prefix = s"$role="
    EvidenceObjectBinding
      .signatureTokens(EvidenceObjectBinding.signaturesForProofRole(frame.objectBindingSignatures, proofRole).toList, prefix)
      .filter(keep)

  private def objectTokens(
      frame: MoveJudgmentCauseFrame,
      role: String,
      proofRole: Option[RelativeCauseProofRole]
  ): Set[String] =
    EvidenceObjectBinding.objectTokens(frame.objectBindingSignatures, role, proofRole)

  private def objectSignatures(
      frame: MoveJudgmentCauseFrame,
      proofRole: Option[RelativeCauseProofRole]
  ): Set[String] =
    EvidenceObjectBinding.signaturesForProofRole(frame.objectBindingSignatures, proofRole)

  private def witnessBindingRank(level: MoveJudgmentCauseWitnessBindingLevel): Int =
    level match
      case MoveJudgmentCauseWitnessBindingLevel.NotWitness          => 0
      case MoveJudgmentCauseWitnessBindingLevel.SameComparisonOnly => 1
      case MoveJudgmentCauseWitnessBindingLevel.LineContext        => 2
      case MoveJudgmentCauseWitnessBindingLevel.ObjectContext      => 3
      case MoveJudgmentCauseWitnessBindingLevel.Punishment         => 4

  private def longTermRootFrame(frame: MoveJudgmentCauseFrame): Boolean =
    frame.role == MoveJudgmentCauseFrameRole.PrimaryCause &&
      frame.hasOwnedAdmissibleLongTermProof &&
      ClaimEventCluster.kindForCause(frame.causeKind).isEmpty &&
      frame.comparisonKind == CandidateComparisonKind.PlayedVsBest &&
      frame.causeRole == RelativeCauseRole.PrimaryPlayedCause &&
      frame.causeImportance == RelativeCauseImportance.Primary &&
      frame.attributionDirectProofEligible

  private def tacticalWitnessFrame(frame: MoveJudgmentCauseFrame): Boolean =
    frame.role == MoveJudgmentCauseFrameRole.PrimaryCause &&
      ClaimEventCluster.kindForCause(frame.causeKind).exists(kind =>
        kind == ClaimEventClusterKind.TacticalEvent ||
          kind == ClaimEventClusterKind.DefensiveEvent ||
          kind == ClaimEventClusterKind.ConversionEvent ||
          kind == ClaimEventClusterKind.MaterialEvent
      )

  private def tacticalWitnessNarrativeFrame(graph: TypedEvidenceGraph, frame: MoveJudgmentCauseFrame): Boolean =
    tacticalWitnessFrame(frame) && eventRootSelectable(graph, frame)

  private def sameComparison(left: MoveJudgmentCauseFrame, right: MoveJudgmentCauseFrame): Boolean =
    comparisonFrameKey(left) == comparisonFrameKey(right)

  private def comparisonFrameKey(frame: MoveJudgmentCauseFrame): (CandidateComparisonKind, LineNodeRef, LineNodeRef) =
    (frame.comparisonKind, frame.referenceLine, frame.candidateLine)
