package redbook.chapter5

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class StreamSpec extends AnyFlatSpec with should.Matchers {

  "toList func" should "work" in {
    Stream(1, 2, 3, 4).toList shouldBe List(1, 2, 3, 4)
    Empty.toList shouldBe List()
  }

  "drop func" should "work" in {
    Stream(1, 2, 3, 4).drop(2).toList shouldBe List(3, 4)
    Stream(1, 2, 3, 4).drop(4).toList shouldBe List()
    Stream(1, 2, 3, 4).drop(45).toList shouldBe List()
    Stream(1, 2, 3, 4).drop(0).toList shouldBe List(1, 2, 3, 4)
  }
}
