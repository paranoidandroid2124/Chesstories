package controllers

import chess.format.Fen
import chess.format.pgn.{ ParsedPgn, PgnNodeData, PgnStr }
import chess.{ Node as PgnNode, Position }
import chess.variant.*
import lila.app.*
import lila.analyse.CondensedJsonView
import lila.core.game.Pov
import lila.tree.{ ParseImport, Root }
import lila.ui.Page
import play.api.libs.json.JsObject

object AnalysePgnPipeline:

  // Shared "analyse PGN" entrypoint used by all controller-side import flows.
  val maxInlinePgnChars = 200000
  // Keep the server's accepted tree within the browser PGN reader's bounded review surface.
  private val maxInlinePgnNodes = 600

  final case class RenderPayload(
      data: JsObject,
      pov: Pov,
      inlinePgn: Option[String]
  )

  def inlinePgnDraft(rawPgn: String): Option[String] =
    Option(rawPgn)
      .map(_.trim)
      .filter(_.nonEmpty)
      .filter(_.length <= maxInlinePgnChars)

  def validatedInlinePgn(rawPgn: String): Option[String] =
    inlinePgnDraft(rawPgn).filter: pgn =>
      ParseImport
        .full(PgnStr(pgn))
        .exists: result =>
          result.replayError.isEmpty &&
            result.parsed.mainline.nonEmpty &&
            browserSupportedSetup(result.parsed) &&
            replayBrowserTree(result.parsed)

  private def browserSupportedSetup(parsed: ParsedPgn): Boolean =
    ParseImport.extractVariant(parsed.toGame, parsed.tags) match
      case variant @ (Standard | Chess960 | FromPosition) =>
        parsed.tags.fen.forall(fen => Fen.readWithMoveNumber(variant, fen).isDefined)
      case _ => false

  private def replayBrowserTree(parsed: ParsedPgn): Boolean =
    parsed.tree.exists: tree =>
      var nodes = 0

      def replay(node: PgnNode[PgnNodeData], position: Position): Boolean =
        nodes += 1
        nodes <= maxInlinePgnNodes &&
        node.value
          .san(position)
          .fold(
            _ => false,
            moveOrDrop =>
              node.variations.forall(variation => replay(variation.toNode, position)) &&
                node.child.forall(replay(_, moveOrDrop.after))
          )

      replay(tree, parsed.toGame.position)

  def buildPayload(
      variant: chess.variant.Variant = chess.variant.Standard,
      fen: Option[chess.format.Fen.Full] = None,
      inlinePgn: Option[String] = None
  )(using ctx: Context): RenderPayload =
    val pov = Pov(lila.core.game.Game.make(variant, fen), chess.Color.White)
    val root = fen.fold(Root.default(variant))(f => Root.default(variant).copy(fen = f))
    val data = CondensedJsonView(pov, root, ctx.pref)
    RenderPayload(
      data = data,
      pov = pov,
      inlinePgn = inlinePgn
    )

  def page(
      variant: chess.variant.Variant = chess.variant.Standard,
      fen: Option[chess.format.Fen.Full] = None,
      inlinePgn: Option[String] = None
  )(using ctx: Context): Page =
    val payload = buildPayload(variant = variant, fen = fen, inlinePgn = inlinePgn)
    views.analyse.ui.userAnalysis(
      payload.data,
      payload.pov,
      chess960PositionNum = Chess960Analysis.positionNumber(variant, fen),
      inlinePgn = payload.inlinePgn
    )
