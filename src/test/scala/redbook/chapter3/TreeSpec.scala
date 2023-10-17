package redbook.chapter3

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class TreeSpec extends AnyFlatSpec with should.Matchers {

  "size func" should "work" in {
    Tree.size(Leaf(5)) shouldBe 1
    Tree.size(Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(3), Leaf(4))))) shouldBe 7
  }

  "maximum func" should "work" in {
    Tree.maximum(Leaf(5))(Math.max) shouldBe 5
    Tree.maximum(Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(3), Leaf(4)))))(Math.max) shouldBe 4
  }

  "depth func" should "work" in {
    Tree.depth(Leaf(5)) shouldBe 1
    Tree.depth(Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(3), Leaf(4))))) shouldBe 4
    Tree.depth(Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(3), Branch(Leaf(3), Leaf(4)))))) shouldBe 5
  }

  "map func" should "work" in {
    Tree.map(Leaf(5))(_ + 1) shouldBe Leaf(6)
    Tree.map(Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(3), Leaf(4)))))(_ + 1) shouldBe Branch(
      Leaf(2),
      Branch(Leaf(3), Branch(Leaf(4), Leaf(5)))
    )
  }

  "fold func" should "work" in {
    Tree.fold(Leaf(5))(identity)(Math.max) shouldBe 5
    Tree.fold(Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(3), Leaf(4)))))(identity)(Math.max) shouldBe 4
  }
}
