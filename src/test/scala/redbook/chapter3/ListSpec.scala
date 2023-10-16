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

  "foldRightNotOptimal func" should "work" in {
    List.foldRightNotOptimal(List(1, 2, 3), 0)((a, b) => a + b) shouldBe 6
    List.foldRightNotOptimal(Nil, 0)((a: Int, b: Int) => a + b) shouldBe 0
    List.foldRightNotOptimal(List(1, 2, 3), 0)((a: Int, b: Int) => a - b) shouldBe 2
    List.foldRightNotOptimal(List("a", "b", "c"), "")((a, b) => a + b) shouldBe "abc"
    try {
      List.foldRightNotOptimal(List((1 to 1000000).toVector), 0L)((a, b) => a + b)
    } catch {
      case _: StackOverflowError =>
        println("foldRight throw StackOverflowError")
    }
  }

  "foldRight func" should "work" in {
    List.foldRight(List(1, 2, 3), 0)((a, b) => a + b) shouldBe 6
    List.foldRight(Nil, 0)((a: Int, b: Int) => a + b) shouldBe 0
    List.foldRight(List("a", "b", "c"), "")((a, b) => a + b) shouldBe "abc"
    List.foldRight(List(1, 2, 3), 0)((a: Int, b: Int) => a - b) shouldBe 2
  }

  "foldLeft func" should "work" in {
    List.foldLeft(List(1, 2, 3), 0)((a, b) => a + b) shouldBe 6
    List.foldLeft(Nil, 0)((a: Int, b: Int) => a + b) shouldBe 0
    List.foldLeft(List("a", "b", "c"), "")((a, b) => a + b) shouldBe "abc"
    List.foldLeft(List(1, 2, 3), 0)((a, b) => a - b) shouldBe -6
    List.foldLeft(List((1 to 1000000).toVector), 0L)((a, b) => a + b) shouldBe 500000500000L
  }

  "reverse func" should "work" in {
    List.reverse(List(1, 2, 3)) shouldBe List(3, 2, 1)
    List.reverse(List(1)) shouldBe List(1)
    List.reverse(Nil) shouldBe Nil
  }

  "append func" should "work" in {
    List.append(List(1, 2, 3), List(4, 5, 6)) shouldBe List(1, 2, 3, 4, 5, 6)
    List.append(List(1, 2, 3), Nil) shouldBe List(1, 2, 3)
    List.append(Nil, List(1, 2, 3)) shouldBe List(1, 2, 3)
    List.append(Nil, Nil) shouldBe Nil
  }

  "flatten func" should "work" in {
    List.flatten(List(List(1, 2), List(3, 4), List(5, 6))) shouldBe List(1, 2, 3, 4, 5, 6)
    List.flatten(List(List(1, 2), Nil, List(5, 6))) shouldBe List(1, 2, 5, 6)
    List.flatten(List(List(1, 2), Nil, Nil)) shouldBe List(1, 2)
    List.flatten(List(Nil, Nil, Nil)) shouldBe Nil
  }

  "map func" should "work" in {
    List.map(List(1, 2, 3))(_ + 1) shouldBe List(2, 3, 4)
    List.map(List(1))(_ + 1) shouldBe List(2)
    List.map(Nil: List[Int])(_ + 1) shouldBe Nil
  }

  "flatMap func" should "work" in {
    List.flatMap(List(1, 2, 3))(i => List(i, i)) shouldBe List(1, 1, 2, 2, 3, 3)
    List.flatMap(List(1))(i => List(i, i)) shouldBe List(1, 1)
    List.flatMap(Nil: List[Int])(i => List(i, i)) shouldBe Nil
  }

  "filter func" should "work" in {
    List.filter(List(1, 2, 3))(_ % 2 == 0) shouldBe List(2)
    List.filter(List(1, 2, 3))(_ == 0) shouldBe Nil
    List.filter(Nil: List[Int])(_ % 2 == 0) shouldBe Nil
  }

  "zipWith func" should "work" in {
    List.zipWith(List(1, 2, 3), List(4, 5, 6))(_ + _) shouldBe List(5, 7, 9)
    List.zipWith(List(1, 2, 3), Nil)(_ + _) shouldBe List(1, 2, 3)
    List.zipWith(Nil, List(1, 2, 3))(_ + _) shouldBe List(1, 2, 3)
    List.zipWith(List(1, 2, 3), List(4, 5))(_ + _) shouldBe List(5, 7, 3)
    List.zipWith(List(1, 2, 3), List(4))(_ + _) shouldBe List(5, 2, 3)
    List.zipWith(List(4, 5), List(1, 2, 3))(_ + _) shouldBe List(5, 7, 3)
    List.zipWith(List(4), List(1, 2, 3))(_ + _) shouldBe List(5, 2, 3)
  }

  "toString func" should "work" in {
    List.toString(List(1, 2, 3)) shouldBe "123"
    List.toString(Nil) shouldBe ""
  }

  "hasSubsequence func" should "work" in {
    List.hasSubsequence(List(1, 2, 3), Nil) shouldBe false
    List.hasSubsequence(Nil, List(1, 2, 3)) shouldBe false
    List.hasSubsequence(List(1, 2, 3), List(2, 3)) shouldBe true
    List.hasSubsequence(List(1, 2, 2, 3), List(2, 3)) shouldBe true
  }
}
