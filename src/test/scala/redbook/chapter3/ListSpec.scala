package redbook.chapter3

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class ListSpec extends AnyFlatSpec with should.Matchers {

  "tail func" should "work" in {
    List(1, 2, 3, 4, 5).getTail shouldBe List(2, 3, 4, 5)
    List(1).getTail shouldBe Nil
    Nil.getTail shouldBe Nil
  }

  "setHead func" should "work" in {
    List(1, 2, 3).setHead(5) shouldBe List(5, 2, 3)
    List(1).setHead(5) shouldBe List(5)
    Nil.setHead(5) shouldBe Nil
  }

  "foldLeft func" should "work" in {
    List(1, 2, 3).foldLeft(0)((a, b) => a + b) shouldBe 6
    Nil.foldLeft(0)((a: Int, b: Int) => a + b) shouldBe 0
    List("a", "b", "c").foldLeft("")((a, b) => a + b) shouldBe "abc"
    List(1, 2, 3).foldLeft(0)((a, b) => a - b) shouldBe -6
    List((1 to 1000000).toVector).foldLeft(0L)((a, b) => a + b) shouldBe 500000500000L
  }

  "reverse func" should "work" in {
    List(1, 2, 3).reverse shouldBe List(3, 2, 1)
    List(1).reverse shouldBe List(1)
    Nil.reverse shouldBe Nil
  }

  "foldRight func" should "work" in {
    List(1, 2, 3).foldRight(0)((a, b) => a + b) shouldBe 6
    Nil.foldRight(0)((a: Int, b: Int) => a + b) shouldBe 0
    List("a", "b", "c").foldRight("")((a, b) => a + b) shouldBe "abc"
    List(1, 2, 3).foldRight(0)((a: Int, b: Int) => a - b) shouldBe 2
  }

  "foldRightNotOptimal func" should "work" in {
    List(1, 2, 3).foldRightNotOptimal(0)((a, b) => a + b) shouldBe 6
    Nil.foldRightNotOptimal(0)((a: Int, b: Int) => a + b) shouldBe 0
    List(1, 2, 3).foldRightNotOptimal(0)((a: Int, b: Int) => a - b) shouldBe 2
    List("a", "b", "c").foldRightNotOptimal("")((a, b) => a + b) shouldBe "abc"
    try {
      List((1 to 1000000).toVector).foldRightNotOptimal(0L)((a, b) => a + b)
    } catch {
      case _: StackOverflowError =>
        println("foldRight throw StackOverflowError")
    }
  }

  "dropWhile func" should "work" in {
    List(1, 2, 3).dropWhile((a: Int) => a < 4) shouldBe Nil
    List(1, 2, 3).dropWhile((a: Int) => a > 4) shouldBe List(1, 2, 3)
    Nil.dropWhile((a: Int) => a > 4) shouldBe Nil
    List(1, 2, 3).dropWhile((a: Int) => a < 3) shouldBe List(3)
  }

  "drop func" should "work" in {
    List(1, 2, 3).drop(3) shouldBe Nil
    List(1, 2, 3).drop(123) shouldBe Nil
    List(1, 2, 3).drop(0) shouldBe List(1, 2, 3)
    Nil.drop(123) shouldBe Nil
    List(1, 2, 3).drop(2) shouldBe List(3)
  }

  "append func" should "work" in {
    List(1, 2, 3).append(List(4, 5, 6)) shouldBe List(1, 2, 3, 4, 5, 6)
    List(1, 2, 3).append(Nil) shouldBe List(1, 2, 3)
    Nil.append(List(1, 2, 3)) shouldBe List(1, 2, 3)
    Nil.append(Nil) shouldBe Nil
  }

  "init func" should "work" in {
    List(1, 2, 3).init shouldBe List(1, 2)
    Nil.init shouldBe Nil
    List(1).init shouldBe Nil
  }

  "length func" should "work" in {
    List(1, 2, 3).length shouldBe 3
    Nil.length shouldBe 0
    List(1).length shouldBe 1
  }

  "flatten func" should "work" in {
    List(List(1, 2), List(3, 4), List(5, 6)).flatten shouldBe List(1, 2, 3, 4, 5, 6)
    List(List(1, 2), Nil, List(5, 6)).flatten shouldBe List(1, 2, 5, 6)
    List(List(1, 2), Nil, Nil).flatten shouldBe List(1, 2)
    List(Nil, Nil, Nil).flatten shouldBe Nil
  }

  "forall func" should "work" in {
    List(1, 2, 3).forall(_ > 0) shouldBe true
    (List(): List[Int]).forall(_ > 0) shouldBe true
    List(1, 2, 3).forall(_ % 2 == 0) shouldBe false
  }

  "map func" should "work" in {
    List(1, 2, 3).map(_ + 1) shouldBe List(2, 3, 4)
    List(1).map(_ + 1) shouldBe List(2)
    (Nil: List[Int]).map(_ + 1) shouldBe Nil
  }

  "flatMap func" should "work" in {
    List(1, 2, 3).flatMap(i => List(i, i)) shouldBe List(1, 1, 2, 2, 3, 3)
    List(1).flatMap(i => List(i, i)) shouldBe List(1, 1)
    (Nil: List[Int]).flatMap(i => List(i, i)) shouldBe Nil
  }

  "filter func" should "work" in {
    List(1, 2, 3).filter(_ % 2 == 0) shouldBe List(2)
    List(1, 2, 3).filter(_ == 0) shouldBe Nil
    (Nil: List[Int]).filter(_ % 2 == 0) shouldBe Nil
  }

  "zipWith func" should "work" in {
    List(1, 2, 3).zipWith(List(4, 5, 6))(_ + _) shouldBe List(5, 7, 9)
    List(1, 2, 3).zipWith(Nil)(_ + _) shouldBe List(1, 2, 3)
    Nil.zipWith(List(1, 2, 3))(_ + _) shouldBe List(1, 2, 3)
    List(1, 2, 3).zipWith(List(4, 5))(_ + _) shouldBe List(5, 7, 3)
    List(1, 2, 3).zipWith(List(4))(_ + _) shouldBe List(5, 2, 3)
    List(4, 5).zipWith(List(1, 2, 3))(_ + _) shouldBe List(5, 7, 3)
    List(4).zipWith(List(1, 2, 3))(_ + _) shouldBe List(5, 2, 3)
  }

  "toString func" should "work" in {
    List(1, 2, 3).toString shouldBe "123"
    Nil.toString shouldBe ""
  }

  "hasSubsequence func" should "work" in {
    List(1, 2, 3).hasSubsequence(Nil) shouldBe false
    Nil.hasSubsequence(List(1, 2, 3)) shouldBe false
    List(1, 2, 3).hasSubsequence(List(2, 3)) shouldBe true
    List(1, 2, 2, 3).hasSubsequence(List(2, 3)) shouldBe true
  }
}
