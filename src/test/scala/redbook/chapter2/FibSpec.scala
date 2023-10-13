package redbook.chapter2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class FibSpec extends AnyFlatSpec with should.Matchers {

  "fib func" should "work" in {
    Fib.fib(1) shouldBe 0
    Fib.fib(2) shouldBe 1
    Fib.fib(3) shouldBe 1
    Fib.fib(6) shouldBe 5
  }

}
