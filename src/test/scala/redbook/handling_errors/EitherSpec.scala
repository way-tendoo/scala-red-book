package redbook.handling_errors

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class EitherSpec extends AnyFlatSpec with should.Matchers {

  "map func" should "work" in {
    Right(5).map(_ + 1) shouldBe Right(6)
    val left: Either[String, Int] = Left("error")
    left.map(_ + 1) shouldBe Left("error")
  }

  "flatMap func" should "work" in {
    Right(5).flatMap(a => Right(a + 1)) shouldBe Right(6)
    val left: Either[String, Int] = Left("error")
    left.flatMap(a => Right(a + 1)) shouldBe Left("error")
    Right(5).flatMap(_ => Left("error")) shouldBe Left("error")
  }

  "orElse func" should "work" in {
    Right(5).orElse(Right(4)) shouldBe Right(5)
    Left("error").orElse(Right(4)) shouldBe Right(4)
    Right(5).orElse(Left("error")) shouldBe Right(5)
    Left("error").orElse(Left("other_error")) shouldBe Left("other_error")
  }

  "map2 func" should "work" in {
    Right(5).map2(Right(4))(_ + _) shouldBe Right(9)
    Right(5).map2(Left("error"))(_ + _) shouldBe Left("error")
    (Left("error"): Either[String, Int]).map2(Right(4))(_ + _) shouldBe Left("error")
  }

  "sequence func" should "work" in {
    List(Right(5), Right(6), Right(7)).sequence shouldBe Right(List(5, 6, 7))
    List(Right(5), Left("error"), Left("error2")).sequence shouldBe Left("error")
  }

  "traverse func" should "work" in {
    import Either._
    List(5, 6, 7).traverse { i =>
      if (i > 0) Right(i)
      else Left(s"error_$i")
    } shouldBe Right(List(5, 6, 7))
    List(5, 6, 7).traverse { i =>
      if (i % 2 == 0) Right(i)
      else Left(s"error_$i")
    } shouldBe Left(s"error_5")
  }
}
