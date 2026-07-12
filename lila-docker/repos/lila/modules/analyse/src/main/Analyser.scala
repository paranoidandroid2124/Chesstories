package lila.analyse

import lila.tree.Analysis

final class Analyser(
    analysisRepo: AnalysisRepo
)(using Executor)
    extends lila.tree.Analyser:

  def get(game: Game): Fu[Option[Analysis]] =
    analysisRepo.byGame(game)

  def byId(id: Analysis.Id): Fu[Option[Analysis]] = analysisRepo.byId(id)

  def save(analysis: Analysis): Funit = analysisRepo.save(analysis)

  def progress(analysis: Analysis): Funit = funit
