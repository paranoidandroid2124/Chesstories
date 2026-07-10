package lila.chessjudgment.analysis.assembly

import _root_.chess.format.Fen
import _root_.chess.variant.Standard
import lila.chessjudgment.model.{ ProbePurpose, ProbeRequest }
import lila.chessjudgment.model.judgment.*

enum JudgmentPacketValidationIssueKind:
  case MissingRootPosition
  case EmptyEvidenceGraph
  case MissingAttachedEvidence
  case MissingIdeaEvidence
  case MissingClaimEvidence
  case MissingClaimIdea
  case MissingClaimClusterEvidence
  case MissingClaimClusterClaim
  case MissingClaimClusterIdea
  case EmptyClaimClusterAnchor
  case NonLongTermClaimClusterAnchor
  case MissingClaimEventClusterEvidence
  case MissingClaimEventClusterClaim
  case MissingClaimEventClusterIdea
  case EmptyClaimEventClusterMember
  case EmptyClaimEventClusterCause
  case DuplicateClaimEventClusterId
  case NonConcreteClaimEventClusterMember
  case LongTermClaimEventClusterEvidence
  case MismatchedClaimEventClusterKind
  case UnbackedClaimEventClusterProof
  case UnownedClaimEventCauseProof
  case MissingRelativeEvidence
  case MismatchedLineEvidenceRef
  case MismatchedEvalEvidenceRef
  case MismatchedEvalWhitePov
  case MismatchedThreatPressureSideToMove
  case MismatchedStructuralDeltaTransitionBinding
  case MissingStructuralDeltaTransitionParent
  case MissingTacticalMechanismParent
  case UnbackedTacticalMechanismSignal
  case MismatchedRelativeCauseEventLine
  case MismatchedRelativeCauseRole
  case MismatchedRelativeCauseImportance
  case MismatchedRelativeCauseEvidenceLines
  case MismatchedRelativeCauseSourceSide
  case MismatchedRelativeCauseSupportRef
  case MissingRelativeCauseSupportParent
  case MissingRelativeCauseComparisonParent
  case UnbackedRelativeCauseProof
  case ContextOnlyRelativeCause
  case UnattributedRelativeCause
  case RootMismatchedRelativeCause
  case SupportPromotedToDirectRelativeCause
  case MissingComparisonReferenceParent
  case MissingComparisonCandidateParent
  case MissingCounterfactualReferenceParent
  case MissingCounterfactualCandidateParent
  case MissingVerdictCertificationReferenceParent
  case MissingVerdictCertificationPrimaryParent
  case MismatchedClaimSubjectBinding
  case MissingMoveJudgmentViewClaim
  case MissingMoveJudgmentViewIdea
  case MissingMoveJudgmentViewEvidence
  case MissingMoveJudgmentViewCluster
  case InvalidProbeRequest

final case class JudgmentPacketValidationIssue(
    kind: JudgmentPacketValidationIssueKind,
    subjectId: String,
    evidence: Option[EvidenceRef] = None
)

final case class JudgmentPacketValidationResult(
    issues: List[JudgmentPacketValidationIssue]
):
  def isValid: Boolean = issues.isEmpty

object JudgmentPacketValidationResult:
  val valid: JudgmentPacketValidationResult = JudgmentPacketValidationResult(Nil)

object JudgmentPacketValidator:

  def validate(packet: EvidenceBackedJudgmentPacket): JudgmentPacketValidationResult =
    val graphIds = packet.evidenceGraph.records.map(_.ref.id).toSet
    val ideaIds = packet.ideas.map(_.ref).toSet
    val ideaIdStrings = ideaIds.map(_.id)
    val claimIds = packet.claims.map(_.id).toSet
    val claimsById = packet.claims.map(claim => claim.id -> claim).toMap
    val supportClusterIds = packet.claimSupportClusters.map(_.id).toSet
    val eventClusterIds = packet.claimEventClusters.map(_.id).toSet
    val issues =
      List.concat(
        rootIssues(packet),
        emptyGraphIssues(packet),
        missingAttachedEvidence(packet, graphIds),
        missingIdeaEvidence(packet, graphIds),
        missingClaimEvidence(packet, graphIds),
        missingClaimIdeas(packet, ideaIds),
        missingClaimClusterEvidence(packet, graphIds),
        missingClaimClusterClaims(packet, claimIds),
        missingClaimClusterIdeas(packet, ideaIds),
        emptyClaimClusterAnchors(packet),
        nonLongTermClusterAnchors(packet, claimsById),
        missingClaimEventClusterEvidence(packet, graphIds),
        missingClaimEventClusterClaims(packet, claimIds),
        missingClaimEventClusterIdeas(packet, ideaIds),
        emptyClaimEventClusterMembers(packet),
        emptyClaimEventClusterCauses(packet),
        duplicateClaimEventClusterIds(packet),
        nonConcreteClaimEventClusterMembers(packet, claimsById),
        longTermClaimEventClusterEvidence(packet),
        mismatchedClaimEventClusterKinds(packet),
        unbackedClaimEventClusterProof(packet),
        unownedClaimEventCauseProof(packet, claimsById),
        missingRelativeEvidence(packet, graphIds),
        graphBindingInvariants(packet),
        claimSubjectBindingInvariants(packet),
        missingMoveJudgmentViewClaims(packet, claimIds),
        missingMoveJudgmentViewIdeas(packet, ideaIdStrings),
        missingMoveJudgmentViewEvidence(packet, graphIds),
        missingMoveJudgmentViewClusters(packet, supportClusterIds, eventClusterIds),
        invalidProbeRequests(packet)
      )
    JudgmentPacketValidationResult(issues.distinct)

  private def rootIssues(packet: EvidenceBackedJudgmentPacket): List[JudgmentPacketValidationIssue] =
    Option
      .when(!packet.positions.exists(_.ref == packet.root))(
        JudgmentPacketValidationIssue(JudgmentPacketValidationIssueKind.MissingRootPosition, "root-position")
      )
      .toList

  private def emptyGraphIssues(packet: EvidenceBackedJudgmentPacket): List[JudgmentPacketValidationIssue] =
    Option
      .when(packet.evidenceGraph.records.isEmpty)(
        JudgmentPacketValidationIssue(JudgmentPacketValidationIssueKind.EmptyEvidenceGraph, "evidence-graph")
      )
      .toList

  private def missingAttachedEvidence(
      packet: EvidenceBackedJudgmentPacket,
      graphIds: Set[String]
  ): List[JudgmentPacketValidationIssue] =
    val attached =
      packet.candidateLines.map(_.evidence) ++
        packet.transitions.map(_.evidence)
    missing(attached, graphIds, JudgmentPacketValidationIssueKind.MissingAttachedEvidence)

  private def missingIdeaEvidence(
      packet: EvidenceBackedJudgmentPacket,
      graphIds: Set[String]
  ): List[JudgmentPacketValidationIssue] =
    missing(
      packet.ideas.flatMap(_.evidence),
      graphIds,
      JudgmentPacketValidationIssueKind.MissingIdeaEvidence
    )

  private def missingClaimEvidence(
      packet: EvidenceBackedJudgmentPacket,
      graphIds: Set[String]
  ): List[JudgmentPacketValidationIssue] =
    missing(
      packet.claims.flatMap(_.evidence),
      graphIds,
      JudgmentPacketValidationIssueKind.MissingClaimEvidence
    )

  private def missingRelativeEvidence(
      packet: EvidenceBackedJudgmentPacket,
      graphIds: Set[String]
  ): List[JudgmentPacketValidationIssue] =
    val refs =
      packet.relativeAssessments.flatMap(assessment =>
        assessment.evidence ::
          (assessment.counterfactualEvidence ++
            assessment.candidateComparisonEvidence ++
            assessment.relativeCauseEvidence ++
            assessment.verdictCertificationEvidence.toList)
      )
    missing(refs, graphIds, JudgmentPacketValidationIssueKind.MissingRelativeEvidence)

  private def missingMoveJudgmentViewClaims(
      packet: EvidenceBackedJudgmentPacket,
      claimIds: Set[String]
  ): List[JudgmentPacketValidationIssue] =
    packet.moveJudgmentView.toList.flatMap { view =>
      val frameClaims =
        view.verdictCarriers.map(_.claimId) ++
          moveJudgmentCauseFrames(view).flatMap(frame =>
            frame.causeClaimIds ++ frame.evaluationClaimIds ++ frame.witnessClaimIds ++ frame.finalClaimIds
          ) ++
          view.positionPlanTechniqueFrames.flatMap(_.claimIds) ++
          view.overriddenLocalIdeas.flatMap(_.claimIds) ++
          view.preservedLocalIdeas.flatMap(_.claimIds)
      frameClaims.distinct.filterNot(claimIds.contains).map { claimId =>
        JudgmentPacketValidationIssue(
          kind = JudgmentPacketValidationIssueKind.MissingMoveJudgmentViewClaim,
          subjectId = claimId
        )
      }
    }

  private def missingMoveJudgmentViewIdeas(
      packet: EvidenceBackedJudgmentPacket,
      ideaIds: Set[String]
  ): List[JudgmentPacketValidationIssue] =
    packet.moveJudgmentView.toList.flatMap { view =>
      val frameIdeas =
        view.verdictCarriers.flatMap(_.ideaIds) ++
          moveJudgmentCauseFrames(view).flatMap(frame => frame.ideaIds ++ frame.supportIdeaIds) ++
          view.positionPlanTechniqueFrames.flatMap(_.ideaIds) ++
          view.overriddenLocalIdeas.map(_.ideaId) ++
          view.preservedLocalIdeas.map(_.ideaId)
      frameIdeas.distinct.filterNot(ideaIds.contains).map { ideaId =>
        JudgmentPacketValidationIssue(
          kind = JudgmentPacketValidationIssueKind.MissingMoveJudgmentViewIdea,
          subjectId = ideaId
        )
      }
    }

  private def missingMoveJudgmentViewEvidence(
      packet: EvidenceBackedJudgmentPacket,
      graphIds: Set[String]
  ): List[JudgmentPacketValidationIssue] =
    packet.moveJudgmentView.toList.flatMap { view =>
      val verdictEvidence =
        view.verdict.toList.flatMap(frame =>
          frame.relativeAssessmentEvidenceId :: frame.verdictCertificationEvidenceId.toList
        )
      val frameEvidence =
        view.verdictCarriers.flatMap(_.evidenceIds) ++
          moveJudgmentCauseFrames(view).flatMap(frame =>
            frame.causeEvidenceIds ++
              frame.evidenceIds ++
              frame.proofDirectSourceIds ++
              frame.proofContrastSourceIds ++
              frame.proofContextSupportSourceIds ++
              frame.proofStrategicMechanismSourceIds ++
              frame.proofStrategicMechanismSignalSourceIds ++
              frame.supportEvidenceSourceIds
          ) ++
          view.positionPlanTechniqueFrames.flatMap(positionPlanTechniqueFrameEvidenceIds) ++
          view.moveMeaningClaims.flatMap(moveMeaningClaimEvidenceIds) ++
          view.overriddenLocalIdeas.flatMap(_.evidenceIds) ++
          view.preservedLocalIdeas.flatMap(_.evidenceIds)
      (verdictEvidence ++ frameEvidence).distinct.filterNot(graphIds.contains).map { evidenceId =>
        JudgmentPacketValidationIssue(
          kind = JudgmentPacketValidationIssueKind.MissingMoveJudgmentViewEvidence,
          subjectId = evidenceId
        )
      }
    }

  private def positionPlanTechniqueFrameEvidenceIds(frame: PositionPlanTechniqueFrame): List[String] =
    (
      frame.evidenceIds ++
        frame.mechanismEvidenceIds ++
        frame.sourceEvidenceIds ++
        frame.relativeCauseEvidenceIds ++
        frame.semanticDetails.flatMap(detail =>
          detail.referenceEvidenceIds ++
            detail.candidateEvidenceIds ++
            detail.sourceEvidenceIds ++
            detail.causeEvidenceIds
        ) ++
        frame.objectBindings.map(_.sourceEvidenceId)
    ).distinct.sorted

  private def moveMeaningClaimEvidenceIds(claim: MoveMeaningClaim): List[String] =
    (
      claim.causeEvidenceIds ++
        claim.sourceEvidenceIds
    ).distinct.sorted

  private def missingMoveJudgmentViewClusters(
      packet: EvidenceBackedJudgmentPacket,
      supportClusterIds: Set[String],
      eventClusterIds: Set[String]
  ): List[JudgmentPacketValidationIssue] =
    packet.moveJudgmentView.toList.flatMap { view =>
      val missingEventClusters =
        moveJudgmentCauseFrames(view).flatMap(_.clusterId).distinct.filterNot(eventClusterIds.contains).map { clusterId =>
          JudgmentPacketValidationIssue(
            kind = JudgmentPacketValidationIssueKind.MissingMoveJudgmentViewCluster,
            subjectId = clusterId
          )
        }
      val missingSupportClusters =
        (view.supportContextClusterIds ++ moveJudgmentCauseFrames(view).flatMap(_.relatedSupportClusterIds))
          .distinct
          .filterNot(supportClusterIds.contains)
          .map { clusterId =>
            JudgmentPacketValidationIssue(
              kind = JudgmentPacketValidationIssueKind.MissingMoveJudgmentViewCluster,
              subjectId = clusterId
            )
          }
      missingEventClusters ++ missingSupportClusters
    }

  private def moveJudgmentCauseFrames(view: MoveJudgmentView): List[MoveJudgmentCauseFrame] =
    view.causeAudit.all

  private def missingClaimIdeas(
      packet: EvidenceBackedJudgmentPacket,
      ideaIds: Set[ChessIdeaRef]
  ): List[JudgmentPacketValidationIssue] =
    packet.claims.flatMap { claim =>
      claim.ideaRefs.filterNot(ideaIds.contains).map { idea =>
        JudgmentPacketValidationIssue(
          kind = JudgmentPacketValidationIssueKind.MissingClaimIdea,
          subjectId = idea.id
        )
      }
    }

  private def missingClaimClusterEvidence(
      packet: EvidenceBackedJudgmentPacket,
      graphIds: Set[String]
  ): List[JudgmentPacketValidationIssue] =
    missing(
      packet.claimSupportClusters.flatMap(_.evidence),
      graphIds,
      JudgmentPacketValidationIssueKind.MissingClaimClusterEvidence
    )

  private def missingClaimClusterClaims(
      packet: EvidenceBackedJudgmentPacket,
      claimIds: Set[String]
  ): List[JudgmentPacketValidationIssue] =
    packet.claimSupportClusters.flatMap { cluster =>
      val ids = cluster.anchorClaimIds ++ cluster.supportingClaimIds ++ cluster.constrainingClaimIds
      ids.distinct.filterNot(claimIds.contains).map { claimId =>
        JudgmentPacketValidationIssue(
          kind = JudgmentPacketValidationIssueKind.MissingClaimClusterClaim,
          subjectId = claimId
        )
      }
    }

  private def missingClaimClusterIdeas(
      packet: EvidenceBackedJudgmentPacket,
      ideaIds: Set[ChessIdeaRef]
  ): List[JudgmentPacketValidationIssue] =
    packet.claimSupportClusters.flatMap { cluster =>
      cluster.ideas.filterNot(ideaIds.contains).map { idea =>
        JudgmentPacketValidationIssue(
          kind = JudgmentPacketValidationIssueKind.MissingClaimClusterIdea,
          subjectId = idea.id
        )
      }
    }

  private def emptyClaimClusterAnchors(
      packet: EvidenceBackedJudgmentPacket
  ): List[JudgmentPacketValidationIssue] =
    packet.claimSupportClusters.filter(_.anchorClaimIds.isEmpty).map { cluster =>
      JudgmentPacketValidationIssue(
        kind = JudgmentPacketValidationIssueKind.EmptyClaimClusterAnchor,
        subjectId = cluster.id
      )
    }

  private def nonLongTermClusterAnchors(
      packet: EvidenceBackedJudgmentPacket,
      claimsById: Map[String, ClaimSeed]
  ): List[JudgmentPacketValidationIssue] =
    packet.claimSupportClusters.flatMap { cluster =>
      cluster.anchorClaimIds.flatMap(claimsById.get).filterNot(_.family.isLongTerm).map { claim =>
        JudgmentPacketValidationIssue(
          kind = JudgmentPacketValidationIssueKind.NonLongTermClaimClusterAnchor,
          subjectId = claim.id
        )
      }
    }

  private def missingClaimEventClusterEvidence(
      packet: EvidenceBackedJudgmentPacket,
      graphIds: Set[String]
  ): List[JudgmentPacketValidationIssue] =
    missing(
      packet.claimEventClusters.flatMap(cluster =>
        cluster.evidence ++
          cluster.interactions.flatMap(_.evidence) ++
          cluster.proofTransitionConsequences.map(_.source) ++
          cluster.causeProofs.flatMap(proof =>
            proof.proofDirectSourceIds ++
              proof.proofContrastSourceIds ++
              proof.proofContextSupportSourceIds ++
              proof.proofStrategicMechanismSourceIds ++
              proof.proofStrategicMechanismSignalSourceIds
          ).distinct.flatMap(id => packet.evidenceGraph.byId.get(id).map(_.ref))
      ),
      graphIds,
      JudgmentPacketValidationIssueKind.MissingClaimEventClusterEvidence
    )

  private def missingClaimEventClusterClaims(
      packet: EvidenceBackedJudgmentPacket,
      claimIds: Set[String]
  ): List[JudgmentPacketValidationIssue] =
    packet.claimEventClusters.flatMap { cluster =>
      val ids =
          cluster.memberClaimIds ++
          cluster.causeClaimIds ++
          cluster.evaluationClaimIds ++
          cluster.witnessClaimIds ++
          cluster.interactions.flatMap(interaction => List(interaction.sourceClaimId, interaction.targetClaimId))
      ids.distinct.filterNot(claimIds.contains).map { claimId =>
        JudgmentPacketValidationIssue(
          kind = JudgmentPacketValidationIssueKind.MissingClaimEventClusterClaim,
          subjectId = claimId
        )
      }
    }

  private def missingClaimEventClusterIdeas(
      packet: EvidenceBackedJudgmentPacket,
      ideaIds: Set[ChessIdeaRef]
  ): List[JudgmentPacketValidationIssue] =
    packet.claimEventClusters.flatMap { cluster =>
      cluster.ideas.filterNot(ideaIds.contains).map { idea =>
        JudgmentPacketValidationIssue(
          kind = JudgmentPacketValidationIssueKind.MissingClaimEventClusterIdea,
          subjectId = idea.id
        )
      }
    }

  private def emptyClaimEventClusterMembers(
      packet: EvidenceBackedJudgmentPacket
  ): List[JudgmentPacketValidationIssue] =
    packet.claimEventClusters.filter(_.memberClaimIds.isEmpty).map { cluster =>
      JudgmentPacketValidationIssue(
        kind = JudgmentPacketValidationIssueKind.EmptyClaimEventClusterMember,
        subjectId = cluster.id
      )
    }

  private def emptyClaimEventClusterCauses(
      packet: EvidenceBackedJudgmentPacket
  ): List[JudgmentPacketValidationIssue] =
    packet.claimEventClusters.filter(_.causeClaimIds.isEmpty).map { cluster =>
      JudgmentPacketValidationIssue(
        kind = JudgmentPacketValidationIssueKind.EmptyClaimEventClusterCause,
        subjectId = cluster.id
      )
    }

  private def duplicateClaimEventClusterIds(
      packet: EvidenceBackedJudgmentPacket
  ): List[JudgmentPacketValidationIssue] =
    packet.claimEventClusters
      .groupBy(_.id)
      .collect { case (id, clusters) if clusters.size > 1 =>
        JudgmentPacketValidationIssue(
          kind = JudgmentPacketValidationIssueKind.DuplicateClaimEventClusterId,
          subjectId = id
        )
      }
      .toList

  private def nonConcreteClaimEventClusterMembers(
      packet: EvidenceBackedJudgmentPacket,
      claimsById: Map[String, ClaimSeed]
  ): List[JudgmentPacketValidationIssue] =
    packet.claimEventClusters.flatMap { cluster =>
      cluster.memberClaimIds.flatMap(claimsById.get).filterNot(_.family.isEvent).map { claim =>
        JudgmentPacketValidationIssue(
          kind = JudgmentPacketValidationIssueKind.NonConcreteClaimEventClusterMember,
          subjectId = claim.id
        )
      }
    }

  private def longTermClaimEventClusterEvidence(
      packet: EvidenceBackedJudgmentPacket
  ): List[JudgmentPacketValidationIssue] =
    val longTermLayers =
      Set(
        EvidenceLayer.PawnStructure,
        EvidenceLayer.Strategic,
        EvidenceLayer.StrategicMechanism,
        EvidenceLayer.OpeningContext,
        EvidenceLayer.FeatureAnchor,
        EvidenceLayer.ApplicabilityAssessment,
        EvidenceLayer.PlanPressure,
        EvidenceLayer.PlanTransition
      )
    packet.claimEventClusters.flatMap { cluster =>
      cluster.evidence.filter(ref => longTermLayers.contains(ref.layer)).map { ref =>
        JudgmentPacketValidationIssue(
          kind = JudgmentPacketValidationIssueKind.LongTermClaimEventClusterEvidence,
          subjectId = cluster.id,
          evidence = Some(ref)
        )
      }
    }

  private def mismatchedClaimEventClusterKinds(
      packet: EvidenceBackedJudgmentPacket
  ): List[JudgmentPacketValidationIssue] =
    packet.claimEventClusters.flatMap { cluster =>
      ClaimEventCluster.kindForCause(cluster.causeKind).filterNot(_ == cluster.kind).map { _ =>
        JudgmentPacketValidationIssue(
          kind = JudgmentPacketValidationIssueKind.MismatchedClaimEventClusterKind,
          subjectId = cluster.id
        )
      }
    }

  private def unbackedClaimEventClusterProof(
      packet: EvidenceBackedJudgmentPacket
  ): List[JudgmentPacketValidationIssue] =
    packet.claimEventClusters.flatMap { cluster =>
      cluster.proofTransitionConsequences.filterNot(transitionConsequenceBacked(packet.evidenceGraph, _)).map { proof =>
        JudgmentPacketValidationIssue(
          kind = JudgmentPacketValidationIssueKind.UnbackedClaimEventClusterProof,
          subjectId = cluster.id,
          evidence = Some(proof.source)
        )
      }
    }

  private def unownedClaimEventCauseProof(
      packet: EvidenceBackedJudgmentPacket,
      claimsById: Map[String, ClaimSeed]
  ): List[JudgmentPacketValidationIssue] =
    packet.claimEventClusters.flatMap { cluster =>
      val unownedProofs = cluster.causeProofs.filterNot(causeProofOwnedByClaim(packet.evidenceGraph, claimsById, cluster, _)).map { proof =>
        JudgmentPacketValidationIssue(
          kind = JudgmentPacketValidationIssueKind.UnownedClaimEventCauseProof,
          subjectId = s"${cluster.id}:${proof.claimId}:${proof.causeKind}",
          evidence = firstCauseProofEvidence(packet.evidenceGraph, proof)
        )
      }
      val unownedCauseClaims = cluster.causeClaimIds.filterNot(claimId =>
        claimsById.get(claimId).exists(claimOwnsClusterCause(packet.evidenceGraph, _, cluster))
      ).map { claimId =>
        JudgmentPacketValidationIssue(
          kind = JudgmentPacketValidationIssueKind.UnownedClaimEventCauseProof,
          subjectId = s"${cluster.id}:$claimId:${cluster.causeKind}"
        )
      }
      unownedProofs ++ unownedCauseClaims
    }

  private def causeProofOwnedByClaim(
      graph: TypedEvidenceGraph,
      claimsById: Map[String, ClaimSeed],
      cluster: ClaimEventCluster,
      proof: ClaimEventCauseProof
  ): Boolean =
    clusterCauseProofMatchesCluster(cluster, proof) &&
      clusterCauseProofRoleMatchesClaim(cluster, proof) &&
      claimsById.get(proof.claimId).exists(claimOwnsCauseProof(graph, _, proof))

  private def clusterCauseProofMatchesCluster(
      cluster: ClaimEventCluster,
      proof: ClaimEventCauseProof
  ): Boolean =
    proof.causeKind == cluster.causeKind &&
      proof.comparisonKind == cluster.comparisonKind &&
      proof.causeRole == cluster.causeRole &&
      proof.causeSourceSide == cluster.causeSourceSide &&
      proof.causeImportance == cluster.causeImportance &&
      proof.attributionKind == cluster.attributionKind &&
      proof.attributionRootMoveMatched == cluster.attributionRootMoveMatched &&
      proof.attributionDirectProofEligible == cluster.attributionDirectProofEligible &&
      proof.referenceLine == cluster.referenceLine &&
      proof.candidateLine == cluster.candidateLine &&
      proof.eventLine == cluster.eventLine

  private def clusterCauseProofRoleMatchesClaim(
      cluster: ClaimEventCluster,
      proof: ClaimEventCauseProof
  ): Boolean =
    proof.memberRole match
      case ClaimEventMemberRole.CauseOwner =>
        cluster.causeClaimIds.contains(proof.claimId)
      case ClaimEventMemberRole.VerdictCarrier =>
        cluster.evaluationClaimIds.contains(proof.claimId)
      case ClaimEventMemberRole.Witness =>
        cluster.witnessClaimIds.contains(proof.claimId)

  private def claimOwnsCauseProof(
      graph: TypedEvidenceGraph,
      claim: ClaimSeed,
      proof: ClaimEventCauseProof
  ): Boolean =
    claim.evidence.flatMap(ref => graph.byId.get(ref.id)).exists {
      case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) =>
        relativeCauseOwnsCauseProof(cause, proof)
      case EvidenceRecord(_, MoveVerdictCertificationEvidence(certification), _) =>
        certification.causes.exists(relativeCauseOwnsCauseProof(_, proof))
      case _ =>
        false
    }

  private def claimOwnsClusterCause(
      graph: TypedEvidenceGraph,
      claim: ClaimSeed,
      cluster: ClaimEventCluster
  ): Boolean =
    claim.evidence.flatMap(ref => graph.byId.get(ref.id)).exists {
      case EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) =>
        relativeCauseMatchesCluster(cause, cluster)
      case EvidenceRecord(_, MoveVerdictCertificationEvidence(certification), _) =>
        certification.causes.exists(relativeCauseMatchesCluster(_, cluster))
      case _ =>
        false
    }

  private def relativeCauseMatchesCluster(
      cause: RelativeCauseFact,
      cluster: ClaimEventCluster
  ): Boolean =
    cause.kind == cluster.causeKind &&
      cause.comparisonKind == cluster.comparisonKind &&
      cause.role == cluster.causeRole &&
      cause.sourceSide == cluster.causeSourceSide &&
      cause.importance == cluster.causeImportance &&
      cause.attribution.kind == cluster.attributionKind &&
      cause.attribution.rootMoveMatched == cluster.attributionRootMoveMatched &&
      cause.attribution.directProofEligible == cluster.attributionDirectProofEligible &&
      cause.referenceLine == cluster.referenceLine &&
      cause.candidateLine == cluster.candidateLine &&
      cause.eventLine == cluster.eventLine

  private def relativeCauseOwnsCauseProof(
      cause: RelativeCauseFact,
      proof: ClaimEventCauseProof
  ): Boolean =
    cause.kind == proof.causeKind &&
      cause.comparisonKind == proof.comparisonKind &&
      cause.role == proof.causeRole &&
      cause.sourceSide == proof.causeSourceSide &&
      cause.importance == proof.causeImportance &&
      cause.attribution.kind == proof.attributionKind &&
      cause.attribution.rootMoveMatched == proof.attributionRootMoveMatched &&
      cause.attribution.directProofEligible == proof.attributionDirectProofEligible &&
      cause.referenceLine == proof.referenceLine &&
      cause.candidateLine == proof.candidateLine &&
      cause.eventLine == proof.eventLine &&
      relativeCauseSupportMatchesCauseProof(cause, proof) &&
      cause.proof
        .map(relativeCauseProofMatchesCauseProof(_, proof))
        .getOrElse(relativeCauseProofMatchesCauseProof(RelativeCauseProof(), proof))

  private def relativeCauseSupportMatchesCauseProof(
      cause: RelativeCauseFact,
      proof: ClaimEventCauseProof
  ): Boolean =
    cause.supportEvidence.map(_.id).toSet == proof.supportEvidenceSourceIds.toSet

  private def relativeCauseProofMatchesCauseProof(
      relativeProof: RelativeCauseProof,
      causeProof: ClaimEventCauseProof
  ): Boolean =
    val strategicProof = relativeProof.strategicProofIdentity
    relativeProof.directProof.sourceRefs.map(_.id).toSet == causeProof.proofDirectSourceIds.toSet &&
      relativeProof.contrastProof.sourceRefs.map(_.id).toSet == causeProof.proofContrastSourceIds.toSet &&
      relativeProof.contextSupport.sourceRefs.map(_.id).toSet == causeProof.proofContextSupportSourceIds.toSet &&
      strategicProof.axisKeys == causeProof.proofStrategicAxisKeys &&
      strategicProof.mechanismKinds == causeProof.proofStrategicMechanismKinds &&
      strategicProof.mechanismSourceIds == causeProof.proofStrategicMechanismSourceIds &&
      strategicProof.signalSourceIds == causeProof.proofStrategicMechanismSignalSourceIds &&
      relativeProof.directProof.kindLabels.toSet == causeProof.proofDirectKinds.toSet &&
      relativeProof.contrastProof.kindLabels.toSet == causeProof.proofContrastKinds.toSet &&
      relativeProof.contextSupport.kindLabels.toSet == causeProof.proofContextSupportKinds.toSet

  private def firstCauseProofEvidence(
      graph: TypedEvidenceGraph,
      proof: ClaimEventCauseProof
  ): Option[EvidenceRef] =
    (
      proof.proofDirectSourceIds ++
        proof.proofContrastSourceIds ++
        proof.proofContextSupportSourceIds ++
        proof.proofStrategicMechanismSourceIds ++
        proof.proofStrategicMechanismSignalSourceIds ++
        proof.supportEvidenceSourceIds
    ).flatMap(id => graph.byId.get(id).map(_.ref)).headOption

  private def transitionConsequenceBacked(graph: TypedEvidenceGraph, proof: TransitionConsequenceProof): Boolean =
    graph.byId.get(proof.source.id).exists(parentHasTransitionConsequence(_, proof))

  private def invalidProbeRequests(packet: EvidenceBackedJudgmentPacket): List[JudgmentPacketValidationIssue] =
    packet.probeRequests.filterNot(validProbeRequest).map { request =>
      JudgmentPacketValidationIssue(
        kind = JudgmentPacketValidationIssueKind.InvalidProbeRequest,
        subjectId = Option(request.id).map(_.trim).filter(_.nonEmpty).getOrElse("probe-request")
      )
    }

  private def graphBindingInvariants(packet: EvidenceBackedJudgmentPacket): List[JudgmentPacketValidationIssue] =
    val candidateLinesByRef = packet.candidateLines.map(line => line.ref -> line).toMap
    packet.evidenceGraph.records.flatMap { record =>
      val bindingIssues =
        record match
          case EvidenceRecord(ref, payload: LineFactEvidence, _) =>
            Option
              .when(!ref.line.contains(payload.line))(
                JudgmentPacketValidationIssue(
                  JudgmentPacketValidationIssueKind.MismatchedLineEvidenceRef,
                  ref.id,
                  Some(ref)
                )
              )
              .toList
          case EvidenceRecord(ref, EvalFactEvidence(line, whitePovEvalCp, _, _), _) =>
            List(
              Option.when(!ref.line.contains(line))(
                JudgmentPacketValidationIssue(
                  JudgmentPacketValidationIssueKind.MismatchedEvalEvidenceRef,
                  ref.id,
                  Some(ref)
                )
              ),
              Option.when(candidateLinesByRef.get(line).exists(_.whitePovEvalCp != whitePovEvalCp))(
                JudgmentPacketValidationIssue(
                  JudgmentPacketValidationIssueKind.MismatchedEvalWhitePov,
                  ref.id,
                  Some(ref)
                )
              )
            ).flatten
          case EvidenceRecord(ref, ThreatPressureEvidence(sideUnderPressure, _), _) =>
            Option
              .when(ref.position.sideToMove.exists(_ != sideUnderPressure))(
                JudgmentPacketValidationIssue(
                  JudgmentPacketValidationIssueKind.MismatchedThreatPressureSideToMove,
                  ref.id,
                  Some(ref)
                )
              )
              .toList
          case EvidenceRecord(ref, payload: ThreatEpisodeEvidence, _) =>
            Option
              .when(ref.position.sideToMove.exists(_ != payload.sideUnderPressure))(
                JudgmentPacketValidationIssue(
                  JudgmentPacketValidationIssueKind.MismatchedThreatPressureSideToMove,
                  ref.id,
                  Some(ref)
                )
              )
              .toList
          case record @ EvidenceRecord(ref, payload: StructuralDeltaEvidence, _) =>
            List(
              Option.when(
                ref.position != payload.from ||
                  ref.scope != payload.role.scope ||
                  ref.line != payload.line
              )(
                JudgmentPacketValidationIssue(
                  JudgmentPacketValidationIssueKind.MismatchedStructuralDeltaTransitionBinding,
                  ref.id,
                  Some(ref)
                )
              ),
              Option.when(!hasMatchingTransitionParent(packet.evidenceGraph, record, payload))(
                JudgmentPacketValidationIssue(
                  JudgmentPacketValidationIssueKind.MissingStructuralDeltaTransitionParent,
                  ref.id,
                  Some(ref)
                )
              )
            ).flatten
          case record @ EvidenceRecord(ref, payload: TacticalMechanismEvidence, _) =>
            val parents = record.parents.flatMap(parent => packet.evidenceGraph.byId.get(parent.id))
            List(
              Option.when(parents.isEmpty)(
                JudgmentPacketValidationIssue(
                  JudgmentPacketValidationIssueKind.MissingTacticalMechanismParent,
                  ref.id,
                  Some(ref)
                )
              ),
              Option.when(!payload.signals.forall(signal => tacticalMechanismSignalBacked(parents, signal)))(
                JudgmentPacketValidationIssue(
                  JudgmentPacketValidationIssueKind.UnbackedTacticalMechanismSignal,
                  ref.id,
                  Some(ref)
                )
              )
            ).flatten
          case record @ EvidenceRecord(_, RelativeCauseFactEvidence(cause), _) =>
            relativeCauseValidationIssues(packet.evidenceGraph, record, cause)
          case record @ EvidenceRecord(ref, CandidateComparisonEvidence(fact), _) =>
            List(
              Option.when(!hasParentLineAndEval(packet.evidenceGraph, record, fact.referenceLine))(
                JudgmentPacketValidationIssue(
                  JudgmentPacketValidationIssueKind.MissingComparisonReferenceParent,
                  ref.id,
                  Some(ref)
                )
              ),
              Option.when(!hasParentLineAndEval(packet.evidenceGraph, record, fact.candidateLine))(
                JudgmentPacketValidationIssue(
                  JudgmentPacketValidationIssueKind.MissingComparisonCandidateParent,
                  ref.id,
                  Some(ref)
                )
              )
            ).flatten
          case record @ EvidenceRecord(ref, CounterfactualFactEvidence(referenceLine, candidateLine, _), _) =>
            List(
              Option.when(!hasParentLineAndEval(packet.evidenceGraph, record, referenceLine))(
                JudgmentPacketValidationIssue(
                  JudgmentPacketValidationIssueKind.MissingCounterfactualReferenceParent,
                  ref.id,
                  Some(ref)
                )
              ),
              Option.when(!hasParentLineAndEval(packet.evidenceGraph, record, candidateLine))(
                JudgmentPacketValidationIssue(
                  JudgmentPacketValidationIssueKind.MissingCounterfactualCandidateParent,
                  ref.id,
                  Some(ref)
                )
              )
            ).flatten
          case record @ EvidenceRecord(ref, MoveVerdictCertificationEvidence(certification), _) =>
            List(
              Option.when(!hasParentLineAndEval(packet.evidenceGraph, record, certification.primaryComparison.referenceLine))(
                JudgmentPacketValidationIssue(
                  JudgmentPacketValidationIssueKind.MissingVerdictCertificationReferenceParent,
                  ref.id,
                  Some(ref)
                )
              ),
              Option.when(
                !hasParentLineAndEval(packet.evidenceGraph, record, certification.primaryComparison.candidateLine) ||
                  !hasMatchingPrimaryComparisonParent(packet.evidenceGraph, record, certification.primaryComparison)
              )(
                JudgmentPacketValidationIssue(
                  JudgmentPacketValidationIssueKind.MissingVerdictCertificationPrimaryParent,
                  ref.id,
                  Some(ref)
                )
              )
            ).flatten ++ certification.causes.flatMap(cause =>
              relativeCauseValidationIssues(packet.evidenceGraph, record, cause)
            )
          case _ =>
            Nil
      bindingIssues
    }

  private def relativeCauseValidationIssues(
      graph: TypedEvidenceGraph,
      record: EvidenceRecord,
      cause: RelativeCauseFact
  ): List[JudgmentPacketValidationIssue] =
    val ref = record.ref
    val hasSupportParents =
      relativeCauseSupportEvidenceParents(graph, record, cause)
    val hasCanonicalSupportRefs =
      hasSupportParents && relativeCauseSupportRefsCanonical(graph, record, cause)
    val canonicalSupport =
      canonicalRelativeCauseSupportRefs(graph, record, cause)
    val expectedBinding =
      RelativeCauseFact.binding(
        kind = cause.kind,
        comparisonKind = cause.comparisonKind,
        referenceLine = cause.referenceLine,
        candidateLine = cause.candidateLine,
        supportEvidence = canonicalSupport,
        explicitSourceSide = Some(cause.sourceSide)
      )
    List(
      Option.when(cause.role != expectedBinding.role)(
        JudgmentPacketValidationIssue(
          JudgmentPacketValidationIssueKind.MismatchedRelativeCauseRole,
          ref.id,
          Some(ref)
        )
      ),
      Option.when(cause.eventLine != expectedBinding.eventLine || !relativeCauseRecordLineConsistent(record, cause))(
        JudgmentPacketValidationIssue(
          JudgmentPacketValidationIssueKind.MismatchedRelativeCauseEventLine,
          ref.id,
          Some(ref)
        )
      ),
      Option.when(cause.importance != expectedBinding.importance)(
        JudgmentPacketValidationIssue(
          JudgmentPacketValidationIssueKind.MismatchedRelativeCauseImportance,
          ref.id,
          Some(ref)
        )
      ),
      Option.when(cause.sourceSide != expectedBinding.sourceSide)(
        JudgmentPacketValidationIssue(
          JudgmentPacketValidationIssueKind.MismatchedRelativeCauseSourceSide,
          ref.id,
          Some(ref)
        )
      ),
      Option.when(!relativeCauseEvidenceLinesConsistent(cause, expectedBinding))(
        JudgmentPacketValidationIssue(
          JudgmentPacketValidationIssueKind.MismatchedRelativeCauseEvidenceLines,
          ref.id,
          Some(ref)
        )
      ),
      Option.when(!hasSupportParents)(
        JudgmentPacketValidationIssue(
          JudgmentPacketValidationIssueKind.MissingRelativeCauseSupportParent,
          ref.id,
          Some(ref)
        )
      ),
      Option.when(hasSupportParents && !hasCanonicalSupportRefs)(
        JudgmentPacketValidationIssue(
          JudgmentPacketValidationIssueKind.MismatchedRelativeCauseSupportRef,
          ref.id,
          Some(ref)
        )
      ),
      Option.when(!hasMatchingRelativeCauseComparisonParent(graph, record, cause))(
        JudgmentPacketValidationIssue(
          JudgmentPacketValidationIssueKind.MissingRelativeCauseComparisonParent,
          ref.id,
          Some(ref)
        )
      ),
      Option.when(cause.proof.exists(proof => proof.hasAnyEvidence && !relativeCauseProofBacked(graph, record, cause, proof)))(
        JudgmentPacketValidationIssue(
          JudgmentPacketValidationIssueKind.UnbackedRelativeCauseProof,
          ref.id,
          Some(ref)
        )
      ),
      Option.when(cause.attribution.contextOnly)(
        JudgmentPacketValidationIssue(
          JudgmentPacketValidationIssueKind.ContextOnlyRelativeCause,
          ref.id,
          Some(ref)
        )
      ),
      Option.when(cause.attribution.unattributed)(
        JudgmentPacketValidationIssue(
          JudgmentPacketValidationIssueKind.UnattributedRelativeCause,
          ref.id,
          Some(ref)
        )
      ),
      Option.when(cause.attribution.rootMismatch)(
        JudgmentPacketValidationIssue(
          JudgmentPacketValidationIssueKind.RootMismatchedRelativeCause,
          ref.id,
          Some(ref)
        )
      ),
      Option.when(relativeCauseSupportPromotedToDirect(cause))(
        JudgmentPacketValidationIssue(
          JudgmentPacketValidationIssueKind.SupportPromotedToDirectRelativeCause,
          ref.id,
          Some(ref)
        )
      )
    ).flatten

  private def relativeCauseSupportPromotedToDirect(cause: RelativeCauseFact): Boolean =
    val directIds = cause.proof.toList.flatMap(_.directProof.sourceRefs.map(_.id)).toSet
    directIds.nonEmpty && !cause.attribution.directProofEligible

  private def relativeCauseRecordLineConsistent(record: EvidenceRecord, cause: RelativeCauseFact): Boolean =
    record.ref.line match
      case None =>
        true
      case Some(line) if record.ref.layer == EvidenceLayer.MoveVerdictCertification =>
        line == cause.eventLine || line == cause.referenceLine || line == cause.candidateLine
      case Some(line) =>
        line == cause.eventLine

  private def tacticalMechanismSignalBacked(
      parents: List[EvidenceRecord],
      signal: TacticalMechanismSignal
  ): Boolean =
    def sourceMatches(record: EvidenceRecord): Boolean =
      signal.source.exists(source => source.id == record.ref.id && source.layer == record.ref.layer)
    def sourceShapeMatches(record: EvidenceRecord): Boolean =
      record.payload match
        case _: MoveMotifEvidence =>
          signal.kind == TacticalMechanismSignalKind.Motif
        case payload: RelationFactEvidence =>
          signal.kind == TacticalMechanismSignalKind.Relation &&
            payload.hasConcreteRelationProof
        case payload: LineFactEvidence =>
          (signal.kind == TacticalMechanismSignalKind.LineConsequence && payload.proofSignalConsequenceKinds.nonEmpty) ||
            (signal.kind == TacticalMechanismSignalKind.LineEvent && payload.lineEvents.nonEmpty)
        case EvalFactEvidence(_, _, mate, _) =>
          signal.kind == TacticalMechanismSignalKind.MateBranch &&
            mate.nonEmpty
        case payload: ThreatEpisodeEvidence =>
          signal.kind == TacticalMechanismSignalKind.ThreatEpisode &&
            payload.isProofSignalDefensivePressure
        case _ =>
          false
    def labelMatches(record: EvidenceRecord): Boolean =
      record.payload match
        case payload: MoveMotifEvidence =>
          signal.kind == TacticalMechanismSignalKind.Motif &&
            payload.proof.kind == signal.label
        case payload: RelationFactEvidence =>
          signal.kind == TacticalMechanismSignalKind.Relation &&
            payload.hasConcreteRelationProof &&
            payload.kind.toString == signal.label
        case payload: LineFactEvidence =>
          (
            signal.kind == TacticalMechanismSignalKind.LineConsequence &&
              payload.proofSignalConsequenceKinds.exists(_.toString == signal.label)
          ) ||
            (
              signal.kind == TacticalMechanismSignalKind.LineEvent &&
                payload.lineEvents.exists(_.kind.toString == signal.label)
            )
        case EvalFactEvidence(_, _, mate, _) =>
          signal.kind == TacticalMechanismSignalKind.MateBranch &&
            mate.exists(_.toString == signal.label)
        case payload: ThreatEpisodeEvidence =>
          signal.kind == TacticalMechanismSignalKind.ThreatEpisode &&
            payload.isProofSignalDefensivePressure &&
            payload.episode.episodeId == signal.label
        case _ =>
          false
    parents.exists(record =>
      record.ref.layer == signal.sourceLayer &&
        signal.source.fold(labelMatches(record))(_ => sourceMatches(record) && sourceShapeMatches(record))
    )

  private def hasMatchingTransitionParent(
      graph: TypedEvidenceGraph,
      record: EvidenceRecord,
      payload: StructuralDeltaEvidence
  ): Boolean =
    record.parents.flatMap(parent => graph.byId.get(parent.id)).exists {
      case EvidenceRecord(parentRef, MoveTransitionEvidence(moveUci, from, to), _) =>
        parentRef.scope == payload.role.scope &&
          moveUci == payload.moveUci &&
          from == payload.from &&
          to == payload.to
      case _ =>
        false
    }

  private def claimSubjectBindingInvariants(packet: EvidenceBackedJudgmentPacket): List[JudgmentPacketValidationIssue] =
    val playedMoves = JudgmentSubjectBinding.packetPlayedMoves(packet)
    packet.claims.flatMap { claim =>
      val binding = JudgmentSubjectBinding.claimBinding(packet, claim)
      Option
        .when(
          claim.subject == IdeaSubject.PlayedMove &&
            (binding == SubjectBindingClass.Other || !JudgmentSubjectBinding.directPlayedClaim(claim, playedMoves))
        )(
          JudgmentPacketValidationIssue(
            JudgmentPacketValidationIssueKind.MismatchedClaimSubjectBinding,
            claim.id
          )
        )
        .toList
    }

  private def hasParentLineAndEval(
      graph: TypedEvidenceGraph,
      record: EvidenceRecord,
      line: LineNodeRef
  ): Boolean =
    hasParentPayloadLine(graph, record, line, EvidenceLayer.Line) &&
      hasParentPayloadLine(graph, record, line, EvidenceLayer.Eval)

  private def hasParentPayloadLine(
      graph: TypedEvidenceGraph,
      record: EvidenceRecord,
      line: LineNodeRef,
      layer: EvidenceLayer
  ): Boolean =
    graph.parentClosure(record).exists(parent => parent.carriesLinePayload(line, layer))

  private def relativeCauseEvidenceLinesConsistent(
      cause: RelativeCauseFact,
      expectedBinding: RelativeCauseBinding
  ): Boolean =
    val lines = cause.evidenceLines.toSet
    val allowedLines = Set(cause.referenceLine, cause.candidateLine)
    lines.subsetOf(allowedLines) && lines == expectedBinding.evidenceLines.toSet

  private def relativeCauseSupportEvidenceParents(
      graph: TypedEvidenceGraph,
      record: EvidenceRecord,
      cause: RelativeCauseFact
  ): Boolean =
    val parentIds = (record.parents ++ graph.parentClosure(record).map(_.ref)).map(_.id).toSet
    cause.supportEvidence.forall(ref => parentIds.contains(ref.id))

  private def relativeCauseSupportRefsCanonical(
      graph: TypedEvidenceGraph,
      record: EvidenceRecord,
      cause: RelativeCauseFact
  ): Boolean =
    val parentRefs = (record.parents ++ graph.parentClosure(record).map(_.ref))
      .map(ref => ref.id -> ref)
      .toMap
    cause.supportEvidence.forall(ref =>
      parentRefs.get(ref.id).exists(canonical => sameEvidenceBinding(ref, canonical))
    )

  private def canonicalRelativeCauseSupportRefs(
      graph: TypedEvidenceGraph,
      record: EvidenceRecord,
      cause: RelativeCauseFact
  ): List[EvidenceRef] =
    val parentRefs = (record.parents ++ graph.parentClosure(record).map(_.ref))
      .map(ref => ref.id -> ref)
      .toMap
    cause.supportEvidence.flatMap(ref => parentRefs.get(ref.id)).distinctBy(_.id)

  private def sameEvidenceBinding(left: EvidenceRef, right: EvidenceRef): Boolean =
    left.producer == right.producer &&
      left.layer == right.layer &&
      left.position == right.position &&
      left.line == right.line &&
      left.scope == right.scope

  private def hasMatchingRelativeCauseComparisonParent(
      graph: TypedEvidenceGraph,
      record: EvidenceRecord,
      cause: RelativeCauseFact
  ): Boolean =
    record.parents.flatMap(parent => graph.byId.get(parent.id)).exists {
      case EvidenceRecord(_, CandidateComparisonEvidence(fact), _) =>
        fact.kind == cause.comparisonKind &&
          fact.referenceLine == cause.referenceLine &&
          fact.candidateLine == cause.candidateLine
      case _ =>
        false
    }

  private def relativeCauseProofBacked(
      graph: TypedEvidenceGraph,
      record: EvidenceRecord,
      cause: RelativeCauseFact,
      proof: RelativeCauseProof
  ): Boolean =
    val parents = graph.parentClosure(record)
    val parentIds = parents.map(_.ref.id).toSet
    relativeCauseProofShapeBacked(graph, cause, proof, parentIds) &&
      proof.boardAnchorProofs.forall(proof => parents.exists(parentHasBoardAnchor(_, proof))) &&
      proof.lineEventProofs.forall(proof => parents.exists(parentHasLineEvent(_, proof))) &&
      proof.lineConsequenceProofs.forall(proof => parents.exists(parentHasLineConsequence(_, proof))) &&
      proof.relationProofs.forall(relationProof => parents.exists(parentHasRelationProof(_, relationProof))) &&
      proof.tacticalMechanisms.forall(mechanismProof => parents.exists(parentHasTacticalMechanism(_, mechanismProof))) &&
      proof.strategicMechanisms.forall(mechanismProof => parents.exists(parentHasStrategicMechanism(_, mechanismProof))) &&
      proof.strategicMechanismContrasts.forall(contrastProof =>
        parents.exists(parentHasStrategicMechanismContrast(_, contrastProof))
      ) &&
      proof.threatEpisodes.forall(threatProof => parents.exists(parentHasThreatEpisode(_, threatProof))) &&
      proof.transitionConsequences.forall(proof => parents.exists(parentHasTransitionConsequence(_, proof))) &&
      proof.contextLayers.forall(layer => parents.exists(_.ref.layer == layer))

  private def relativeCauseProofShapeBacked(
      graph: TypedEvidenceGraph,
      cause: RelativeCauseFact,
      proof: RelativeCauseProof,
      parentIds: Set[String]
  ): Boolean =
    val directRole =
      proof.directProof.role == RelativeCauseProofRole.DirectProof &&
        proof.directProof.strength == RelativeCauseProofStrength.Primary
    val contrastRole =
      proof.contrastProof.role == RelativeCauseProofRole.ContrastProof &&
        proof.contrastProof.strength == RelativeCauseProofStrength.Supporting
    val contextRole =
      proof.contextSupport.role == RelativeCauseProofRole.ContextSupport &&
        proof.contextSupport.strength == RelativeCauseProofStrength.WeakHint
    val directIds = proof.directProof.sourceRefs.map(_.id).toSet
    val contrastIds = proof.contrastProof.sourceRefs.map(_.id).toSet
    val contextIds = proof.contextSupport.sourceRefs.map(_.id).toSet
    val ownedIds = cause.attribution.ownedEvidence.map(_.id).toSet
    val allSourceIds = directIds ++ contrastIds ++ contextIds
    directRole &&
      contrastRole &&
      contextRole &&
      allSourceIds.subsetOf(parentIds) &&
      directIds == ownedIds &&
      (directIds.isEmpty || cause.attribution.directProofEligible) &&
      contrastIds.forall(id => graph.byId.get(id).exists(record => contrastProofSource(record, cause))) &&
      contextIds.forall(id => graph.byId.get(id).exists(record => contextSupportSource(record, cause))) &&
      (directIds intersect contrastIds).isEmpty &&
      (contextIds intersect directIds).isEmpty &&
      (contextIds intersect contrastIds).isEmpty

  private def contrastProofSource(record: EvidenceRecord, cause: RelativeCauseFact): Boolean =
    (
      (record.ref.layer == EvidenceLayer.Line || record.ref.layer == EvidenceLayer.Eval) &&
        record.ref.line.exists(line => line == cause.referenceLine || line == cause.candidateLine)
    ) ||
      (record.payload match
        case CandidateComparisonEvidence(fact) =>
          fact.kind == cause.comparisonKind &&
            fact.referenceLine == cause.referenceLine &&
            fact.candidateLine == cause.candidateLine
        case _ =>
          false
      )

  private def contextSupportSource(record: EvidenceRecord, cause: RelativeCauseFact): Boolean =
    val attributedContext = cause.attribution.contextEvidence.exists(_.id == record.ref.id)
    val referencesComparedLine =
      record.referencesLine(cause.referenceLine) || record.referencesLine(cause.candidateLine)
    attributedContext ||
      (!referencesComparedLine &&
      (
        record.ref.scope == EvidenceScope.BeforePosition ||
          record.ref.scope == EvidenceScope.CurrentPosition ||
          record.ref.line.nonEmpty
      ))

  private def parentHasBoardAnchor(record: EvidenceRecord, proof: BoardAnchorProof): Boolean =
    record match
      case EvidenceRecord(ref, payload: BoardFactEvidence, _) =>
        ref.id == proof.source.id && payload.hasProofSignalAnchor(proof.kind)
      case _ =>
        false

  private def parentHasLineEvent(record: EvidenceRecord, proof: LineEventProof): Boolean =
    record match
      case EvidenceRecord(ref, payload: LineFactEvidence, _) =>
        ref.id == proof.source.id &&
          payload.lineEvents.exists(event =>
            event.kind == proof.kind &&
              proof.moveUci.forall(move => normalizeMove(move) == normalizeMove(event.moveUci)) &&
              proof.plyOffset.forall(_ == event.plyOffset) &&
              proof.side.forall(side => event.side.contains(side)) &&
              proof.square.forall(square => event.square.contains(square))
          )
      case _ =>
        false

  private def normalizeMove(raw: String): String =
    Option(raw).getOrElse("").trim.toLowerCase

  private def parentHasLineConsequence(record: EvidenceRecord, proof: LineConsequenceProof): Boolean =
    record match
      case EvidenceRecord(ref, payload: LineFactEvidence, _) =>
        ref.id == proof.source.id &&
          payload.proofSignalConsequences.exists(consequence =>
            consequence.kind == proof.kind &&
              proof.eventMove.forall(move => consequence.eventMove.exists(existing => normalizeMove(existing) == normalizeMove(move))) &&
              proof.lineMoves.forall(move => consequence.lineMoves.exists(existing => normalizeMove(existing) == normalizeMove(move)))
          )
      case _ =>
        false

  private def parentHasTransitionConsequence(record: EvidenceRecord, proof: TransitionConsequenceProof): Boolean =
    record match
      case EvidenceRecord(ref, payload: StructuralDeltaEvidence, _) =>
        ref.id == proof.source.id &&
          payload.transition == proof.transition &&
          payload.consequences.contains(proof.consequence)
      case _ =>
        false

  private def parentHasTacticalMechanism(record: EvidenceRecord, proof: TacticalMechanismProof): Boolean =
    record match
      case EvidenceRecord(ref, payload: TacticalMechanismEvidence, _) =>
        ref.id == proof.source.id &&
          payload.kind == proof.kind &&
          proof.signals.forall(payload.signals.contains)
      case _ =>
        false

  private def parentHasStrategicMechanism(record: EvidenceRecord, proof: StrategicMechanismProof): Boolean =
    record match
      case EvidenceRecord(ref, payload: StrategicMechanismEvidence, _) =>
        ref.id == proof.source.id &&
          payload.kind == proof.kind &&
          proof.signals.forall(signal =>
            payload.signals.contains(signal) &&
              record.parents.exists(parent => parent.id == signal.source.id)
          )
      case _ =>
        false

  private def parentHasStrategicMechanismContrast(
      record: EvidenceRecord,
      proof: StrategicMechanismContrastProof
  ): Boolean =
    record match
      case EvidenceRecord(ref, payload: StrategicMechanismContrastEvidence, _) =>
        ref.id == proof.source.id &&
          payload.comparisonKind == proof.comparisonKind &&
          payload.referenceLine == proof.referenceLine &&
          payload.candidateLine == proof.candidateLine &&
          payload.axisComparisons == proof.axisComparisons &&
          payload.sustainability == proof.sustainability &&
          payload.hasActionableContrast
      case _ =>
        false

  private def parentHasThreatEpisode(record: EvidenceRecord, proof: ThreatEpisodeCauseProof): Boolean =
    record match
      case EvidenceRecord(ref, payload: ThreatEpisodeEvidence, _) =>
        ref.id == proof.source.id &&
          payload.episode.driver == proof.driver &&
          payload.episode.kind == proof.kind &&
          payload.episode.severity == proof.severity &&
          payload.isProofSignalDefensivePressure
      case _ =>
        false

  private def parentHasRelationProof(record: EvidenceRecord, proof: RelationCauseProof): Boolean =
    record match
      case EvidenceRecord(ref, payload: RelationFactEvidence, _) =>
        ref.id == proof.source.id &&
          payload.kind == proof.kind &&
          payload.witnessProof == proof.proof &&
          payload.hasConcreteRelationProof
      case _ =>
        false

  private def hasMatchingPrimaryComparisonParent(
      graph: TypedEvidenceGraph,
      record: EvidenceRecord,
      primary: CandidateComparisonFact
  ): Boolean =
    record.parents.flatMap(parent => graph.byId.get(parent.id)).exists {
      case EvidenceRecord(_, CandidateComparisonEvidence(fact), _) =>
        sameComparisonIdentity(fact, primary)
      case EvidenceRecord(_, CounterfactualFactEvidence(referenceLine, candidateLine, comparison), _) =>
        referenceLine == primary.referenceLine &&
          candidateLine == primary.candidateLine &&
          sameEvalComparison(comparison, primary.comparison)
      case _ =>
        false
    }

  private def sameComparisonIdentity(left: CandidateComparisonFact, right: CandidateComparisonFact): Boolean =
    left.kind == right.kind &&
      left.referenceLine == right.referenceLine &&
      left.candidateLine == right.candidateLine &&
      sameEvalComparison(left.comparison, right.comparison)

  private def sameEvalComparison(left: EvalComparison, right: EvalComparison): Boolean =
    left.referenceLine == right.referenceLine &&
      left.candidateLine == right.candidateLine &&
      left.verdict == right.verdict &&
      left.winPercentLossForMover == right.winPercentLossForMover &&
      left.candidateWinPercentDeltaForMover == right.candidateWinPercentDeltaForMover

  private def validProbeRequest(request: ProbeRequest): Boolean =
    val idValid = Option(request.id).exists(_.trim.nonEmpty)
    val fenValid =
      Option(request.fen)
        .map(_.trim)
        .exists(fen => fen.nonEmpty && Fen.read(Standard, Fen.Full(fen)).isDefined)
    val depthValid = request.depth > 0
    val purposeValid = request.purpose.nonEmpty
    val requiredSignalsValid = request.requiredSignals.exists(_.trim.nonEmpty)
    val movesValid = request.moves.forall(validUciMove)
    val branchValid =
      request.purpose.filter(ProbePurpose.isBranchReply).forall(_ => validBranchReplyProbeRequest(request))
    idValid && fenValid && depthValid && purposeValid && requiredSignalsValid && movesValid && branchValid

  private def validBranchReplyProbeRequest(request: ProbeRequest): Boolean =
    val signals = request.requiredSignals.map(_.trim).filter(_.nonEmpty).toSet
    Set("replyLines", "depth", "purpose", "variationHash").subsetOf(signals) &&
      request.candidateMove.exists(validUciMove) &&
      request.multiPv.exists(_ >= 2) &&
      request.depthFloor.exists(floor => floor > 0 && floor <= request.depth) &&
      request.variationHash.exists(_.trim.nonEmpty)

  private def validUciMove(raw: String): Boolean =
    Option(raw).map(_.trim.toLowerCase).exists(_.matches("""[a-h][1-8][a-h][1-8][nbrq]?"""))

  private def missing(
      refs: List[EvidenceRef],
      graphIds: Set[String],
      kind: JudgmentPacketValidationIssueKind
  ): List[JudgmentPacketValidationIssue] =
    refs.distinctBy(_.id).filterNot(ref => graphIds.contains(ref.id)).map { ref =>
      JudgmentPacketValidationIssue(kind = kind, subjectId = ref.id, evidence = Some(ref))
    }
