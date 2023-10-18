package redbook.chapter3

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class TreeSpec extends AnyFlatSpec with should.Matchers {

  "size func" should "work" in {
    Leaf(5).size shouldBe 1
    Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(3), Leaf(4)))).size shouldBe 7
  }

  "depth func" should "work" in {
    Leaf(5).depth shouldBe 1
    Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(3), Leaf(4)))).depth shouldBe 4
    Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(3), Branch(Leaf(3), Leaf(4))))).depth shouldBe 5
  }

  "map func" should "work" in {
    Leaf(5).map(_ + 1) shouldBe Leaf(6)
    Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(3), Leaf(4)))).map(_ + 1) shouldBe Branch(
      Leaf(2),
      Branch(Leaf(3), Branch(Leaf(4), Leaf(5)))
    )
  }

  "maximum func" should "work" in {
    Leaf(5).maximum(Math.max) shouldBe 5
    Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(3), Leaf(4)))).maximum(Math.max) shouldBe 4
  }

  "fold func" should "work" in {
    Leaf(5).fold(identity)(Math.max) shouldBe 5
    Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(3), Leaf(4)))).fold(identity)(Math.max) shouldBe 4
  }
}
