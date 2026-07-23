package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.model.judgment.*

final case class ClaimSeedAssembly(
    input: NormalizedMoveReviewInput,
    context: JudgmentAssemblyContext
)

object ClaimSeedAssembler:

  def assemble(raw: RawMoveReviewInput): Option[ClaimSeedAssembly] =
    ChessIdeaAssembler.assemble(raw).map(enrich)

  def enrich(assembly: ChessIdeaAssembly): ClaimSeedAssembly =
    val allocator = JudgmentProvenanceAllocator.forInput(assembly.input)
    val context = assembly.context
    val claimCandidates =
      context.ideas.map { idea =>
        val family = claimFamily(idea)
        ClaimComposer.fromIdea(
          id = allocator.evidenceId(s"claim:${allocator.key(idea.ref.family)}:${allocator.key(idea.ref.id)}"),
          family = family,
          idea = idea,
          engineComparison = engineComparison(context, idea, family),
          confidence = idea.confidence
        )
      }
    val claimGraph = ClaimCandidateGraphAssembler.fromClaims(claimCandidates, context.evidenceGraph)
    val arbitration = ClaimArbitrator.rankWithLifecycle(claimGraph, context.relativeAssessments)
    val claims = arbitration.claims
    val claimRecords = claims.map(claim => ClaimComposer.evidenceRecord(s"${claim.id}:evidence", claim))
    val withEvidence = context.withEvidence(claimRecords)
    val withClaims = claims.foldLeft(withEvidence)((ctx, claim) => ctx.withClaim(claim))
    ClaimSeedAssembly(assembly.input, withClaims.withClaimLifecycle(arbitration.lifecycle))

  private def claimFamily(idea: ChessIdea): ClaimFamily =
    if idea.subject == IdeaSubject.Plan then ClaimFamily.Plan
    else idea.ref.family match
      case ChessIdeaFamily.Tactical      => ClaimFamily.Tactical
      case ChessIdeaFamily.Strategic     => ClaimFamily.Strategic
      case ChessIdeaFamily.PawnStructure => ClaimFamily.PawnStructure
      case ChessIdeaFamily.Opening       => ClaimFamily.Opening
      case ChessIdeaFamily.Defensive     => ClaimFamily.Defensive
      case ChessIdeaFamily.Conversion    => ClaimFamily.Conversion
      case ChessIdeaFamily.Material      => ClaimFamily.Material
      case ChessIdeaFamily.Evaluation    => ClaimFamily.Evaluation

  private def engineComparison(
      context: JudgmentAssemblyContext,
      idea: ChessIdea,
      family: ClaimFamily
  ): Option[EvalComparison] =
    Option.when(family == ClaimFamily.Evaluation)(localEngineComparison(context, idea)).flatten

  private def localEngineComparison(
      context: JudgmentAssemblyContext,
      idea: ChessIdea
  ): Option[EvalComparison] =
    val ids = idea.evidence.map(_.id).toSet
    val records = context.evidenceGraph.records.filter(record => ids.contains(record.ref.id))
    records.collectFirst {
      case EvidenceRecord(_, CandidateComparisonEvidence(fact), _) =>
        fact.comparison
      case EvidenceRecord(_, CounterfactualFactEvidence(_, _, comparison), _) =>
        comparison
      case EvidenceRecord(_, RelativeAssessmentEvidence(assessment), _) =>
        assessment.comparison
    }
