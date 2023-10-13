package redbook.chapter3

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class ListSpec extends AnyFlatSpec with should.Matchers {

  "tail func" should "work" in {
    List.tail(List(1, 2, 3, 4, 5)) shouldBe List(2, 3, 4, 5)
    List.tail(List(1)) shouldBe Nil
    List.tail(Nil) shouldBe Nil
  }

  "setHead func" should "work" in {
    List.setHead(List(1, 2, 3), 5) shouldBe List(5, 2, 3)
    List.setHead(List(1), 5) shouldBe List(5)
    List.setHead(Nil, 5) shouldBe Nil
  }

  "dropWhile func" should "work" in {
    List.dropWhile(List(1, 2, 3), (a: Int) => a < 4) shouldBe Nil
    List.dropWhile(List(1, 2, 3), (a: Int) => a > 4) shouldBe List(1, 2, 3)
    List.dropWhile(Nil, (a: Int) => a > 4) shouldBe Nil
    List.dropWhile(List(1, 2, 3), (a: Int) => a < 3) shouldBe List(3)
  }

  "drop func" should "work" in {
    List.drop(List(1, 2, 3), 3) shouldBe Nil
    List.drop(List(1, 2, 3), 123) shouldBe Nil
    List.drop(List(1, 2, 3), 0) shouldBe List(1, 2, 3)
    List.drop(Nil, 123) shouldBe Nil
    List.drop(List(1, 2, 3), 2) shouldBe List(3)
  }

  "length func" should "work" in {
    List.length(List(1, 2, 3)) shouldBe 3
    List.length(Nil) shouldBe 0
    List.length(List(1)) shouldBe 1
  }

  "init func" should "work" in {
    List.init(List(1, 2, 3)) shouldBe List(1, 2)
    List.init(Nil) shouldBe Nil
    List.init(List(1)) shouldBe Nil
  }

  "foldRight func" should "work" in {
    List.foldRight(List(1, 2, 3), 0)((a, b) => a + b) shouldBe 6
    List.foldRight(Nil, 0)((a: Int, b: Int) => a + b) shouldBe 0
    List.foldRight(List("a", "b", "c"), "")((a, b) => a + b) shouldBe "abc"
  }
}
