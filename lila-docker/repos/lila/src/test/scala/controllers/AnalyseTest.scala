package controllers

class AnalyseTest extends munit.FunSuite:

  test("uses the v6 position-commentary upstream path"):
    assertEquals(
      Analyse.positionCommentaryJobsPath("https://runtime.example.test"),
      "https://runtime.example.test/v1/position-commentary-jobs",
    )

  test("normalizes only safe runtime origins"):
    List(
      "http://127.0.0.1:8091" -> Some("http://127.0.0.1:8091"),
      "http://[::1]:8091" -> Some("http://[::1]:8091"),
      "https://runtime.example.test/" -> Some("https://runtime.example.test"),
      "http://runtime.example.test:8091" -> None,
      "https://runtime.example.test/path" -> None,
      "https://runtime.example.test?x=1" -> None,
      "https://user@runtime.example.test" -> None,
    ).foreach: (raw, expected) =>
      assertEquals(Analyse.normalizedRuntimeOrigin(raw), expected)
