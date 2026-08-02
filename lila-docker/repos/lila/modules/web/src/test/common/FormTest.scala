package lila.common

import play.api.data.*
import play.api.data.Forms.*
import play.api.data.format.*
import play.api.data.format.Formats.*
import play.api.data.validation.*

import lila.common.Form.*

class FormTest extends munit.FunSuite:

  val date = java.time.LocalDateTime.of(2023, 4, 12, 11, 1, 15, 337_000_000)
  test("format date mappings"):
    List(
      single("t" -> lila.common.Form.ISODateTime.mapping).unbind(date) -> Map(
        "t" -> "2023-04-12T11:01:15.337Z"
      ),
      single("t" -> lila.common.Form.ISODate.mapping).unbind(date.date) -> Map("t" -> "2023-04-12"),
      single("t" -> lila.common.Form.PrettyDateTime.mapping).unbind(date) -> Map("t" -> "2023-04-12 11:01"),
      single("t" -> lila.common.Form.Timestamp.mapping).unbind(date.instant) -> Map("t" -> "1681297275337"),
      single("t" -> lila.common.Form.ISOInstantOrTimestamp.mapping).unbind(date.instant) -> Map(
        "t" -> "2023-04-12T11:01:15.337Z"
      ),
      single("t" -> lila.common.Form.ISODateOrTimestamp.mapping).unbind(date.date) -> Map("t" -> "2023-04-12")
    ).foreach { case (actual, expected) => assertEquals(actual, expected) }

  test("parse date mappings"):
    List(
      single("t" -> lila.common.Form.ISODateTime.mapping)
        .bind(Map("t" -> "2023-04-25T10:00:00.000Z"))
        .isRight,
      single("t" -> lila.common.Form.ISODateTime.mapping).bind(Map("t" -> "2017-01-01T12:34:56Z")).isRight,
      single("t" -> lila.common.Form.ISODateTime.mapping).bind(Map("t" -> "2017-01-01T12:34:56")).isLeft,
      single("t" -> lila.common.Form.ISODateTime.mapping).bind(Map("t" -> "2017-01-01")).isLeft,
      single("t" -> lila.common.Form.ISODate.mapping).bind(Map("t" -> "2017-01-01")).isRight,
      single("t" -> lila.common.Form.ISODate.mapping).bind(Map("t" -> "2023-04-25T10:00:00.000Z")).isLeft,
      single("t" -> lila.common.Form.ISODate.mapping).bind(Map("t" -> "2017-01-01T12:34:56Z")).isLeft,
      single("t" -> lila.common.Form.ISODate.mapping).bind(Map("t" -> "2017-01-01T12:34:56")).isLeft,
      single("t" -> lila.common.Form.PrettyDateTime.mapping).bind(Map("t" -> "2017-01-01 23:11")).isRight,
      single("t" -> lila.common.Form.PrettyDateTime.mapping)
        .bind(Map("t" -> "2023-04-25T10:00:00.000Z"))
        .isLeft,
      single("t" -> lila.common.Form.PrettyDateTime.mapping).bind(Map("t" -> "2017-01-01T12:34:56Z")).isLeft,
      single("t" -> lila.common.Form.PrettyDateTime.mapping).bind(Map("t" -> "2017-01-01")).isLeft,
      single("t" -> lila.common.Form.Timestamp.mapping).bind(Map("t" -> "1483228800000")).isRight,
      single("t" -> lila.common.Form.Timestamp.mapping).bind(Map("t" -> "2017-01-01 23:11")).isLeft,
      single("t" -> lila.common.Form.Timestamp.mapping).bind(Map("t" -> "2023-04-25T10:00:00.000Z")).isLeft,
      single("t" -> lila.common.Form.Timestamp.mapping).bind(Map("t" -> "2017-01-01T12:34:56Z")).isLeft,
      single("t" -> lila.common.Form.Timestamp.mapping).bind(Map("t" -> "2017-01-01")).isLeft,
      single("t" -> lila.common.Form.ISOInstantOrTimestamp.mapping).bind(Map("t" -> "1483228800000")).isRight,
      single("t" -> lila.common.Form.ISOInstantOrTimestamp.mapping)
        .bind(Map("t" -> "2023-04-25T10:00:00.000Z"))
        .isRight,
      single("t" -> lila.common.Form.ISOInstantOrTimestamp.mapping)
        .bind(Map("t" -> "2017-01-01 23:11"))
        .isLeft,
      single("t" -> lila.common.Form.ISOInstantOrTimestamp.mapping)
        .bind(Map("t" -> "2017-01-01T12:34:56Z"))
        .isRight,
      single("t" -> lila.common.Form.ISOInstantOrTimestamp.mapping).bind(Map("t" -> "2017-01-01")).isLeft,
      single("t" -> lila.common.Form.ISODateOrTimestamp.mapping).bind(Map("t" -> "1483228800000")).isRight,
      single("t" -> lila.common.Form.ISODateOrTimestamp.mapping).bind(Map("t" -> "2017-01-01")).isRight,
      single("t" -> lila.common.Form.ISODateOrTimestamp.mapping)
        .bind(Map("t" -> "2023-04-25T10:00:00.000Z"))
        .isLeft,
      single("t" -> lila.common.Form.ISODateOrTimestamp.mapping).bind(Map("t" -> "2017-01-01 23:11")).isLeft,
      single("t" -> lila.common.Form.ISODateOrTimestamp.mapping)
        .bind(Map("t" -> "2017-01-01T12:34:56Z"))
        .isLeft
    ).foreach { value => assert(value) }

  test("trim before validation"):

    assert(
      FieldMapping("t", List(Constraints.minLength(1)))
        .bind(Map("t" -> " "))
        .isRight
    )

    assert(
      FieldMapping("t", List(Constraints.minLength(1)))
        .as(cleanTextFormatter)
        .bind(Map("t" -> " "))
        .isLeft
    )

    assert(
      single("t" -> cleanText(minLength = 3))
        .bind(Map("t" -> "     "))
        .isLeft
    )

    assert(
      single("t" -> cleanText(minLength = 3))
        .bind(Map("t" -> "aa "))
        .isLeft
    )

    assert(
      single("t" -> cleanText(minLength = 3))
        .bind(Map("t" -> "aaa"))
        .isRight
    )

    assert(
      single("t" -> text)
        .bind(Map("t" -> ""))
        .isRight
    )
    assert(
      single("t" -> cleanText)
        .bind(Map("t" -> ""))
        .isRight
    )
    assert(
      single("t" -> cleanText)
        .bind(Map("t" -> "   "))
        .isRight
    )

  test("invisible chars are removed before validation"):
    val invisibleChars = List('\u200e', '\u200f', '\u202e', '\u1160')
    val invisibleStr = invisibleChars.mkString("")
    assertEquals(single("t" -> cleanText).bind(Map("t" -> invisibleStr)), Right(""))
    assertEquals(single("t" -> cleanText).bind(Map("t" -> s"  $invisibleStr  ")), Right(""))
    assertEquals(single("t" -> cleanTextWithSymbols).bind(Map("t" -> s"  $invisibleStr  ")), Right(""))
    assert(single("t" -> cleanText(minLength = 1)).bind(Map("t" -> invisibleStr)).isLeft)
    assert(single("t" -> cleanText(minLength = 1)).bind(Map("t" -> s"  $invisibleStr  ")).isLeft)
    // braille space
    assert(single("t" -> cleanText(minLength = 1)).bind(Map("t" -> "⠀")).isLeft)
  test("other garbage chars are also removed before validation, unless allowed"):
    val garbageStr = "꧁ ۩۞"
    assertEquals(single("t" -> cleanText).bind(Map("t" -> garbageStr)), Right(""))
    assertEquals(single("t" -> cleanTextWithSymbols).bind(Map("t" -> garbageStr)), Right(garbageStr))
  test("emojis are removed before validation, unless allowed"):
    val emojiStr = "🌈🌚"
    assertEquals(single("t" -> cleanText).bind(Map("t" -> emojiStr)), Right(""))
    assertEquals(single("t" -> cleanTextWithSymbols).bind(Map("t" -> emojiStr)), Right(emojiStr))

  test("special chars"):
    val half = '½'
    assertEquals(single("t" -> cleanTextWithSymbols).bind(Map("t" -> half.toString)), Right(half.toString))
    assertEquals(single("t" -> cleanText).bind(Map("t" -> half.toString)), Right(half.toString))
