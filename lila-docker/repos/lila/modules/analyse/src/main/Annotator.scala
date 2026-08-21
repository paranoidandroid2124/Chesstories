package lila.analyse

import chess.format.pgn.{ Comment, Glyphs, Move, Pgn, PgnStr, SanStr, Tag }
import chess.{ Status, Tree, Variation }

import lila.core.game.Game
import lila.tree.{ Advice, Analysis, StatusText }

final class Annotator(netDomain: lila.core.config.NetDomain) extends lila.tree.Annotator:

  def apply(p: Pgn, game: Game, analysis: Option[Analysis]): Pgn =
    annotateStatus(game.winnerColor, game.status):
      annotateTurns(
        p,
        analysis.so(_.advices)
      ).copy(
        tags = p.tags + Tag(_.Annotator, netDomain)
      )

  def addEvals(p: Pgn, analysis: Analysis): Pgn =
    analysis.infos.foldLeft(p) { (pgn, info) =>
      pgn
        .updatePly(
          info.ply,
          move => move.copy(comments = info.pgnComment.toList ::: move.comments)
        )
        .getOrElse(pgn)
    }

  def toPgnString(pgn: Pgn): PgnStr = PgnStr:
    s"${pgn.render}\n\n\n".replaceIf("] } { [", "] [")

  private def annotateStatus(winner: Option[Color], status: Status)(p: Pgn) =
    StatusText(status, winner, chess.variant.Standard) match
      case "" => p
      case text => p.updateLastPly(_.copy(result = text.some))

  // add advices into mainline
  private def annotateTurns(p: Pgn, advices: List[Advice]): Pgn =
    advices
      .foldLeft(p): (pgn, advice) =>
        pgn
          .modifyInMainline(
            advice.ply,
            node =>
              node.copy(
                value = node.value.copy(
                  glyphs = Glyphs.fromList(advice.judgment.glyph :: Nil),
                  comments = advice.makeComment(true) :: node.value.comments
                ),
                variations = makeVariation(advice).toList ++ node.variations
              )
          )
          .getOrElse(pgn)

  private def makeVariation(advice: Advice): Option[Variation[Move]] =
    Tree
      .build[SanStr, Move](advice.info.variation.take(20), Move(_))
      .map(_.toVariation)
