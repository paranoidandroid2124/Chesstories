package lila.chessjudgment.model.judgment

enum EvidenceFlowStage:
  case Produced
  case RegisteredInGraph
  case AttachedToNodeOrEdge
  case AttachedToRelativeAssessment
  case PromotedToIdea
  case PromotedToClaim
  case IncludedInPacket

enum EvidenceLossReason:
  case ReferenceNotRegistered
  case EvidenceAvailableWithoutIdea
  case IdeaAvailableWithoutClaim
  case ClaimEvidenceMissing
  case PacketMissingRoot

final case class EvidenceLayerCoverage(
    layer: EvidenceLayer,
    registered: Int,
    promotedToIdea: Int,
    promotedToClaim: Int
)

final case class EvidenceLossDiagnostic(
    stage: EvidenceFlowStage,
    reason: EvidenceLossReason,
    subjectId: String,
    evidence: Option[EvidenceRef],
    layer: Option[EvidenceLayer]
)

final case class EvidenceLossReport(
    coverage: List[EvidenceLayerCoverage],
    diagnostics: List[EvidenceLossDiagnostic]
):
  def isClean: Boolean = diagnostics.isEmpty

object EvidenceLossReport:
  val empty: EvidenceLossReport = EvidenceLossReport(Nil, Nil)

object EvidenceLossDiagnostics:

  def fromAssembly(ctx: JudgmentAssemblyContext): EvidenceLossReport =
    val graphIds = ctx.evidenceGraph.records.map(_.ref.id).toSet
    val ideaEvidenceIds = ctx.ideas.flatMap(_.evidence.map(_.id)).toSet
    val claimEvidenceIds = ctx.claims.flatMap(_.evidence.map(_.id)).toSet
    val attachedRefs =
      ctx.lines.map(_.evidence) ++
        ctx.transitions.map(_.evidence) ++
        ctx.relativeAssessments.flatMap(assessment =>
          assessment.evidence ::
            (assessment.counterfactualEvidence ++
              assessment.candidateComparisonEvidence ++
              assessment.relativeCauseEvidence ++
              assessment.verdictCertificationEvidence.toList)
        ) ++
        ctx.ideas.flatMap(_.evidence) ++
        ctx.claims.flatMap(_.evidence)

    val missingRegisteredRefs =
      attachedRefs
        .distinctBy(_.id)
        .filterNot(ref => graphIds.contains(ref.id))
        .map { ref =>
          EvidenceLossDiagnostic(
            stage = stageFor(ref.layer),
            reason = EvidenceLossReason.ReferenceNotRegistered,
            subjectId = ref.id,
            evidence = Some(ref),
            layer = Some(ref.layer)
          )
        }

    val unpromotedEvidence =
      ctx.evidenceGraph.records
        .filterNot(record => ideaEvidenceIds.contains(record.ref.id) || claimEvidenceIds.contains(record.ref.id))
        .map { record =>
          EvidenceLossDiagnostic(
            stage = EvidenceFlowStage.RegisteredInGraph,
            reason = EvidenceLossReason.EvidenceAvailableWithoutIdea,
            subjectId = record.ref.id,
            evidence = Some(record.ref),
            layer = Some(record.ref.layer)
          )
        }

    val ideasWithoutClaim =
      ctx.ideas
        .filterNot(idea => ctx.claims.exists(_.ideaRefs.contains(idea.ref)))
        .map { idea =>
          EvidenceLossDiagnostic(
            stage = EvidenceFlowStage.PromotedToIdea,
            reason = EvidenceLossReason.IdeaAvailableWithoutClaim,
            subjectId = idea.ref.id,
            evidence = None,
            layer = Some(EvidenceLayer.ChessIdea)
          )
        }

    val missingClaimEvidence =
      ctx.claims
        .flatMap(_.evidence)
        .distinctBy(_.id)
        .filterNot(ref => graphIds.contains(ref.id))
        .map { ref =>
          EvidenceLossDiagnostic(
            stage = EvidenceFlowStage.PromotedToClaim,
            reason = EvidenceLossReason.ClaimEvidenceMissing,
            subjectId = ref.id,
            evidence = Some(ref),
            layer = Some(ref.layer)
          )
        }

    val missingRoot =
      Option.when(ctx.root.isEmpty)(
        EvidenceLossDiagnostic(
          stage = EvidenceFlowStage.IncludedInPacket,
          reason = EvidenceLossReason.PacketMissingRoot,
          subjectId = "root-position",
          evidence = None,
          layer = None
        )
      ).toList

    EvidenceLossReport(
      coverage = coverage(ctx.evidenceGraph, ideaEvidenceIds, claimEvidenceIds),
      diagnostics = missingRegisteredRefs ++ unpromotedEvidence ++ ideasWithoutClaim ++ missingClaimEvidence ++ missingRoot
    )

  private def coverage(
      graph: TypedEvidenceGraph,
      ideaEvidenceIds: Set[String],
      claimEvidenceIds: Set[String]
  ): List[EvidenceLayerCoverage] =
    graph.records
      .groupBy(_.ref.layer)
      .toList
      .sortBy(_._1.ordinal)
      .map { case (layer, records) =>
        EvidenceLayerCoverage(
          layer = layer,
          registered = records.size,
          promotedToIdea = records.count(record => ideaEvidenceIds.contains(record.ref.id)),
          promotedToClaim = records.count(record => claimEvidenceIds.contains(record.ref.id))
        )
      }

  private def stageFor(layer: EvidenceLayer): EvidenceFlowStage =
    layer match
      case EvidenceLayer.RelativeAssessment | EvidenceLayer.Counterfactual | EvidenceLayer.CandidateComparison |
          EvidenceLayer.RelativeCause | EvidenceLayer.MoveVerdictCertification =>
        EvidenceFlowStage.AttachedToRelativeAssessment
      case EvidenceLayer.ChessIdea =>
        EvidenceFlowStage.PromotedToIdea
      case EvidenceLayer.Claim =>
        EvidenceFlowStage.PromotedToClaim
      case _ =>
        EvidenceFlowStage.AttachedToNodeOrEdge
