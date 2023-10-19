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

  "append func" should "work" in {
    Stream(1, 2, 3, 4).append(Stream(5, 6)).toList shouldBe List(1, 2, 3, 4, 5, 6)
    Stream(1, 2, 3, 4).append(Stream.empty).toList shouldBe List(1, 2, 3, 4)
    Stream.empty.append(Stream(1, 2, 3, 4)).toList shouldBe List(1, 2, 3, 4)
    Stream.empty.append(Stream.empty).toList shouldBe List.empty
  }

  "take func" should "work" in {
    Stream(1, 2, 3, 4).take(2).toList shouldBe List(1, 2)
    Stream(1, 2, 3, 4).append(Stream.empty).toList shouldBe List(1, 2, 3, 4)
    Stream.empty.append(Stream(1, 2, 3, 4)).toList shouldBe List(1, 2, 3, 4)
    Stream.empty.append(Stream.empty).toList shouldBe List.empty
  }

  "takeWhile func" should "work" in {
    Stream(1, 2, 3, 4).takeWhile(_ <= 2).toList shouldBe List(1, 2)
    Stream(1, 2, 3, 4).takeWhile(_ > 0).toList shouldBe List(1, 2, 3, 4)
    Stream(1, 2, 3, 4).takeWhile(_ <= 0).toList shouldBe List.empty
    Stream.empty[Int].takeWhile(_ <= 0).toList shouldBe List.empty
  }

  "exists func" should "work" in {
    Stream(1, 2, 3, 4).exists(_ == 2) shouldBe true
    Stream(1, 2, 3, 4).exists(_ == 13) shouldBe false
    Stream.empty[Int].exists(_ == 13) shouldBe false
  }

  "forall func" should "work" in {
    Stream(1, 2, 3, 4).forall(_ <= 2) shouldBe false
    Stream(1, 2, 3, 4).forall(_ > 0) shouldBe true
    Stream(1, 2, 3, 4).forall(_ <= 0) shouldBe false
    Stream.empty[Int].forall(_ <= 0) shouldBe true
  }

  "headOption func" should "work" in {
    Stream(1, 2, 3).headOption shouldBe Some(1)
    Stream(1).headOption shouldBe Some(1)
    Stream.empty[Int].headOption shouldBe None
  }

  "map func" should "work" in {
    Stream(1, 2, 3).map(_ + 1).toList shouldBe List(2, 3, 4)
    Stream.empty[Int].map(_ + 1).toList shouldBe List.empty
  }

  "flatMap func" should "work" in {
    Stream(1, 2, 3).flatMap(a => Stream(a + 1)).toList shouldBe List(2, 3, 4)
    Stream.empty[Int].flatMap(a => Stream(a + 1)).toList shouldBe List.empty
  }

  "filter func" should "work" in {
    Stream(1, 2, 3).filter(_   % 2 == 0).toList shouldBe List(2)
    Stream.empty[Int].filter(_ % 2 == 0).toList shouldBe List.empty
  }

  "unfold func" should "work" in {
    Stream.unfold(1)(a => Some((a, a))).take(5).toList shouldBe List(1, 1, 1, 1, 1)
  }
}
