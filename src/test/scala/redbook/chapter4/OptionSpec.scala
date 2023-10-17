package redbook.chapter4

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class OptionSpec extends AnyFlatSpec with should.Matchers {

  "map func" should "work" in {
    Some(5).map(_ + 1) shouldBe Some(6)
    None.map(_.asInstanceOf[Int] + 1) shouldBe None
  }

  def plusOneIfEven(num: Int): Option[Int] = {
    if (num % 2 == 0) Some(num + 1)
    else None
  }

  "flatMap func" should "work" in {
    Some(4).flatMap(plusOneIfEven) shouldBe Some(5)
    Some(3).flatMap(plusOneIfEven) shouldBe None
    None.flatMap(i => plusOneIfEven(i.asInstanceOf[Int])) shouldBe None
  }

  "getOrElse func" should "work" in {
    Some(4).getOrElse(3) shouldBe 4
    None.getOrElse(3) shouldBe 3
  }

  "orElse func" should "work" in {
    Some(4).orElse(Some(3)) shouldBe Some(4)
    Some(4).orElse(None) shouldBe Some(4)
    None.orElse(Some(3)) shouldBe Some(3)
    None.orElse(None) shouldBe None
  }

  "filter func" should "work" in {
    Some(4).filter(_ == 4) shouldBe Some(4)
    Some(4).filter(_ != 4) shouldBe None
    None.filter(_ != 4) shouldBe None
  }

  "map2 func" should "work" in {
    Option.map2(Some(3), Some(4))((a, b) => a + b) shouldBe Some(7)
    Option.map2(Some(3), None: Option[Int])((a, b) => a + b) shouldBe None
    Option.map2(None: Option[Int], Some(3))((a, b) => a + b) shouldBe None
    Option.map2(None: Option[Int], None: Option[Int])((a, b) => a + b) shouldBe None
  }

  "sequence func" should "work" in {
    Option.sequence(List(Some(4), Some(5), Some(6))) shouldBe Some(List(4, 5, 6))
    Option.sequence(List(Some(4), None: Option[Int], Some(6))) shouldBe None
    Option.sequence(List()) shouldBe None
  }

  "traverse func" should "work" in {
    Option.traverse(List(1, 2, 3)) { i =>
      if (i > 0) Some(i)
      else None
    } shouldBe Some(List(1, 2, 3))
    Option.traverse(List(1, 2, 3)) { i =>
      if (i % 2 == 0) Some(i)
      else None
    } shouldBe None
    Option.traverse(List(): List[Int]) { i =>
      if (i % 2 == 0) Some(i)
      else None
    } shouldBe None
  }
}
