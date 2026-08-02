package lila.memo

import java.util.concurrent.{ CountDownLatch, TimeUnit }
import java.util.concurrent.atomic.AtomicInteger

class RateLimitTest extends munit.FunSuite:

  test("atomically reserves a key's credits across concurrent callers"):
    given Executor = scala.concurrent.ExecutionContext.global
    given lila.core.config.RateLimit = lila.core.config.RateLimit(true)

    val limiter = RateLimit[String](credits = 6, duration = 1.minute, key = "rate-limit-test", log = false)
    val ready = CountDownLatch(32)
    val start = CountDownLatch(1)
    val done = CountDownLatch(32)
    val accepted = AtomicInteger()

    val callers = List.fill(32):
      Thread: () =>
        ready.countDown()
        start.await()
        try if limiter("same-key", false, cost = 2)(true) then accepted.incrementAndGet()
        finally done.countDown()

    callers.foreach(_.start())
    assert(ready.await(5, TimeUnit.SECONDS))
    start.countDown()
    assert(done.await(5, TimeUnit.SECONDS))
    assertEquals(accepted.get(), 3)
