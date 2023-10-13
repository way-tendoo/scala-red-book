package redbook.chapter2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class UncurrySpec extends AnyFlatSpec with should.Matchers {

  "uncurry func" should "work" in {
    val uncurriedSumFunc = Uncurry.uncurry((a: Int) => (b: Int) => a + b)
    uncurriedSumFunc(2, 3) shouldBe 5
  }
}
