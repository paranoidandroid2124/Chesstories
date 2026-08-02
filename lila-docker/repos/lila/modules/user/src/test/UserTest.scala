package lila.user

class UserTest extends munit.FunSuite:

  given Conversion[String, UserStr] = UserStr(_)
  given Conversion[String, UserId] = UserId(_)

  import UserStr.couldBeUsername

  test("username regex bad prefix: can login"):
    assert(couldBeUsername("000"))
    assert(couldBeUsername("0foo"))
    assert(couldBeUsername("_foo"))
    assert(couldBeUsername("__foo"))
    assert(couldBeUsername("-foo"))

  test("username regex bad suffix: can login"):
    assert(couldBeUsername("a_"))
    assert(couldBeUsername("a-"))

  test("username regex bad length: cannot login"):
    assert(!couldBeUsername(""))
    assert(!couldBeUsername("a"))
    assert(!couldBeUsername("A123456789012345678901234567890"))
    assert(!couldBeUsername("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"))

  test("username regex too many consecutive non-letter chars"):
    assert(couldBeUsername("a--a"))

  test("username regex ok names: can login"):
    assert(couldBeUsername("g-foo"))
    assert(couldBeUsername("G_FOo"))
    assert(couldBeUsername("g-foO"))
    assert(couldBeUsername("FOOO"))
    assert(couldBeUsername("AB"))
    assert(couldBeUsername("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"))
    assert(couldBeUsername("A12345678901234567890123456789"))
