package redbook.chapter2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class CurrySpec extends AnyFlatSpec with should.Matchers {

  "curry func" should "work" in {
    val curriedSumFunc = Curry.curry((a: Int, b: Int) => a + b)
    curriedSumFunc(2)(2) shouldBe 4
  }
}
