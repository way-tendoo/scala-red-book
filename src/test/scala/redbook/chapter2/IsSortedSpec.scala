package redbook.chapter2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class IsSortedSpec extends AnyFlatSpec with should.Matchers {

  "isSorted func" should "work" in {
    IsSorted.isSorted(Array(1, 2, 3, 4, 5), (a: Int, b: Int) => a < b) shouldBe true
    IsSorted.isSorted(Array(5, 1, 3, 4, 5), (a: Int, b: Int) => a < b) shouldBe false
    IsSorted.isSorted(Array(5, 4, 3, 2, 1), (a: Int, b: Int) => a > b) shouldBe true
    IsSorted.isSorted(Array(5, 5, 3, 3, 1), (a: Int, b: Int) => a >= b) shouldBe true
  }
}
