package controllers

import play.api.libs.json.Json

class ImporterTest extends munit.FunSuite:

  test("Chess.com archive responses only schedule approved monthly archive requests"):
    val valid = "https://api.chess.com/pub/player/Example-User/games/2025/07"
    val cases = List(
      valid -> true,
      "https://api.chess.com:443/pub/player/example/games/2025/07" -> true,
      "https://api.chess.com.evil.example/pub/player/example/games/2025/07" -> false,
      "https://127.0.0.1/pub/player/example/games/2025/07" -> false,
      "https://api.chess.com@evil.example/pub/player/example/games/2025/07" -> false,
      "https://api.chess.com:444/pub/player/example/games/2025/07" -> false,
      "https://api.chess.com/pub/player/example/games/2025/07?redirect=https://evil.example" -> false,
      "//api.chess.com/pub/player/example/games/2025/07" -> false,
      "https://api.chess.com/pub/player/example/games/2025/13" -> false
    )

    cases.foreach: (url, allowed) =>
      val archiveList = Json.stringify(Json.obj("archives" -> List(url)))
      val expected = if allowed then List(url) else Nil
      assertEquals(Importer.chessComArchiveFetchUrls(archiveList), expected, url)
