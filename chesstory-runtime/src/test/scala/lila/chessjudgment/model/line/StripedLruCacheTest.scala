package lila.chessjudgment.model.line

import java.util.concurrent.{ Callable, CountDownLatch, Executors, TimeUnit }
import java.util.concurrent.atomic.AtomicInteger

class StripedLruCacheTest extends munit.FunSuite:

  test("occurrence keys keep exact identity while live and remain removable after collection"):
    val first = chess.format.Fen.read(chess.variant.Standard, chess.variant.Standard.initialFen).get
    val same = PositionOccurrenceKey.from(first)
    val key = PositionOccurrenceKey.from(first)
    val other = PositionOccurrenceKey.from(
      chess.format.Fen.read(chess.variant.Standard, chess.variant.Standard.initialFen).get
    )
    assertEquals(key, same)
    assertNotEquals(key, other)
    val retained = new java.util.HashMap[PositionOccurrenceKey, String]()
    retained.put(key, "retained")
    val hash = key.hashCode
    // Deterministically model cleared weak references; no timing-dependent GC assertion.
    key.clear()
    other.clear()
    assertEquals(key.hashCode, hash)
    assertNotEquals(key, same)
    assertNotEquals(key, other)
    assertEquals(key, key)
    assertEquals(retained.remove(key), "retained")

  test("a concurrent cache miss has one calculation owner"):
    val cache = new StripedLruCache[String, String](maxEntries = 8, requestedStripes = 4)
    val calculations = new AtomicInteger(0)
    val start = new CountDownLatch(1)
    val executor = Executors.newFixedThreadPool(8)
    try
      val results = (1 to 32).map { _ =>
        executor.submit(new Callable[String]:
          override def call(): String =
            start.await()
            cache.getOrCompute("shared") {
              calculations.incrementAndGet()
              "value"
            }
        )
      }
      start.countDown()

      assertEquals(results.map(_.get(5, TimeUnit.SECONDS)).toList, List.fill(32)("value"))
      assertEquals(calculations.get(), 1)
    finally
      executor.shutdownNow()

  test("the bounded cache evicts the least recently used entry"):
    val cache = new StripedLruCache[String, String](maxEntries = 2, requestedStripes = 1)
    val calculations = new AtomicInteger(0)
    def value(key: String): String =
      cache.getOrCompute(key) {
        calculations.incrementAndGet()
        key
      }

    value("a")
    value("b")
    value("a")
    value("c")
    value("b")

    assertEquals(calculations.get(), 4)
