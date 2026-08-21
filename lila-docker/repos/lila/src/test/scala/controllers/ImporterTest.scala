package controllers

import play.api.libs.json.Json

import java.nio.charset.StandardCharsets
import java.util.Base64

class ImporterTest extends munit.FunSuite:

  test("Chess.com archive responses reject representative authority confusion"):
    val valid = "https://api.chess.com/pub/player/Example-User/games/2025/07"
    assertEquals(Importer.chessComArchiveFetchUrls(Json.stringify(Json.obj("archives" -> List(valid)))), List(valid))
    val confused = "https://api.chess.com@evil.example/pub/player/example/games/2025/07"
    assertEquals(Importer.chessComArchiveFetchUrls(Json.stringify(Json.obj("archives" -> List(confused)))), Nil)

  test("inline PGN requires a complete browser-replayable tree"):
    val validPgn = "1. e4 e5 2. Nf3 Nc6 *"
    val sideBranchA =
      (1 to 75)
        .flatMap(number => List(s"${number * 2 - 1}. Nf3 Nf6", s"${number * 2}. Ng1 Ng8"))
        .mkString(" ")
    val sideBranchB =
      (1 to 75)
        .flatMap(number => List(s"${number * 2 - 1}. Nc3 Nc6", s"${number * 2}. Nb1 Nb8"))
        .mkString(" ")
    assertEquals(AnalysePgnPipeline.validatedInlinePgn(validPgn), Some(validPgn))
    List(
      "oversize" -> ("1" * 200001),
      "malformed" -> "[Event \"broken\"",
      "illegal side variation" -> "1. e4 (1. e5) e5 *",
      "aggregate side tree over budget" -> s"1. e4 ($sideBranchA) ($sideBranchB) e5 *"
    ).foreach: (name, pgn) =>
      assertEquals(AnalysePgnPipeline.validatedInlinePgn(pgn), None, name)

  test("submitted PGN treats form provenance as untrusted"):
    val manualPgn = "1. e4 e5 2. Nf3 Nc6 *"
    val pgn64 = Base64.getUrlEncoder.withoutPadding.encodeToString(manualPgn.getBytes(StandardCharsets.UTF_8))
    val malformedUtf8 = Base64.getUrlEncoder.withoutPadding.encodeToString(Array[Byte](0xc3.toByte, 0x28.toByte))
    val forged = Map(
      "pgn64" -> Seq(pgn64),
      "sourceProvider" -> Seq("chesscom"),
      "sourceUrl" -> Seq("https://evil.example/game")
    )
    assertEquals(Importer.submittedPgn(Map("pgn" -> Seq(manualPgn))), Some(Importer.SubmittedPgn.Inline(manualPgn)))
    assertEquals(Importer.submittedPgn(forged), Some(Importer.SubmittedPgn.Inline(manualPgn)))
    assertEquals(Importer.submittedPgn(Map("pgn64" -> Seq(malformedUtf8))), Some(Importer.SubmittedPgn.InvalidEncoded))

  test("game selection uses only the cached PGN despite forged form fields"):
    val cached = Importer.GameSummary(
      provider = Importer.providerLichess,
      gameId = "cached-game",
      playedAt = "2026-08-04 12:00",
      white = "CachedWhite",
      black = "CachedBlack",
      result = "1-0",
      speed = "blitz",
      sourceUrl = Some("https://lichess.org/cached-game"),
      pgn = "1. e4 e5 2. Nf3 Nc6 *"
    )
    val forged = Map(
      "gameSelection" -> Seq("selection-id"),
      "pgn" -> Seq("1. d4 d5 *"),
      "pgn64" -> Seq("Zm9yZ2Vk"),
      "sourceProvider" -> Seq(Importer.providerChessCom),
      "sourceUsername" -> Seq("forged-user"),
      "sourceGameId" -> Seq("forged-game"),
      "sourceUrl" -> Seq("https://evil.example/game")
    )
    assertEquals(
      Importer.gameSelectionSubmission(forged, _ => Some("normalized-user" -> cached)),
      Importer.GameSelectionSubmission.Selected(cached.pgn)
    )

  test("blank or missing game selections reject manual PGN fallback"):
    val manualPgn = "1. e4 e5 2. Nf3 Nc6 *"
    List(
      Map("gameSelection" -> Seq(""), "pgn" -> Seq(manualPgn)),
      Map("gameSelection" -> Seq("missing-selection"), "pgn" -> Seq(manualPgn))
    ).foreach: form =>
      assertEquals(Importer.gameSelectionSubmission(form, _ => None), Importer.GameSelectionSubmission.Invalid)
