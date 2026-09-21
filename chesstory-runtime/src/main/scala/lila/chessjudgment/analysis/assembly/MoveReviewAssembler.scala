package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.model.judgment.{ AdmittedLegalLine, AdmittedMoveReviewInput, JudgmentAssemblyContext }

object MoveReviewAssembler:

  def assemble(preparedInput: AdmittedMoveReviewInput): JudgmentAssemblyContext =
    assemble(preparedInput, None)

  /** Preserves the admitted occurrence assembly independently of public assessment projection. */
  def assemble(
      preparedInput: AdmittedMoveReviewInput,
      retained: Option[JudgmentAssemblyContext]
  ): JudgmentAssemblyContext =
    withAssessment(assembleOccurrences(preparedInput, retained))

  def assembleOccurrences(
      preparedInput: AdmittedMoveReviewInput,
      retained: Option[JudgmentAssemblyContext] = None
  ): JudgmentAssemblyContext =
    val input = retained.fold(preparedInput) { previous =>
      require(previous.input.positionHistory == preparedInput.positionHistory &&
        previous.input.playedMoveUci == preparedInput.playedMoveUci, "retained observations need the same reviewed history")
      // Supplied lines are one indexed document, not a set of chess paths.
      require(previous.input.suppliedLines.isEmpty || preparedInput.suppliedLines.isEmpty ||
        previous.input.suppliedLines == preparedInput.suppliedLines,
        "retained observations must preserve the same indexed input document")
      preparedInput.copy(
        legalLines = AdmittedLegalLine.merge(previous.input.legalLines ++ preparedInput.legalLines),
        evaluationObservations = (previous.input.evaluationObservations ++ preparedInput.evaluationObservations).distinct,
        suppliedLines =
          if previous.input.suppliedLines.nonEmpty then previous.input.suppliedLines
          else preparedInput.suppliedLines
      )
    }
    retained.filter(_.input.legalLines == input.legalLines) match
      case Some(previous) =>
        // A new ranking over the same exact replays changes no physical occurrence.
        // Reuse its admitted evidence, but replace every assessment-dependent record.
        NodeLineTransitionAssembler.withCandidates(previous.copy(input = input, lines = Nil,
          evidenceGraph = previous.evidenceGraph.withoutAssessment))
      case None =>
        val context = NodeLineTransitionAssembler.assemble(input, retained)
        val known = retained.toList.flatMap(_.transitions.map(_.evidence.id)).toSet
        EvidenceFactAssembler.enrichTransitions(context, context.transitions.filterNot(edge => known(edge.evidence.id)))

  def withAssessment(occurrences: JudgmentAssemblyContext): JudgmentAssemblyContext =
    if occurrences.lines.isEmpty then occurrences
    else RelativeAssessmentAssembler.enrichAssessment(
      RelativeAssessmentAssembler.enrichComparisonEvidence(occurrences))
