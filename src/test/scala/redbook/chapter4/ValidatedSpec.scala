package redbook.chapter4

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import Validated._

class ValidatedSpec extends AnyFlatSpec with should.Matchers {

  case class TestUser(name: String, surname: String, age: Int)

  object TestUser {

    def validateName(name: String): Either[String, String] =
      if (name.isEmpty) Left("Field `name` is empty")
      else Right(name)

    def validateSurname(surname: String): Either[String, String] =
      if (surname.isEmpty) Left("Field `surname` is empty")
      else Right(surname)

    def validateAge(age: Int): Either[String, Int] =
      if (age < 0) Left("Field `age` is negative")
      else Right(age)
  }

  "valid user" should "work" in {
    val either = (
      TestUser.validateName("Ivan").toValidated,
      TestUser.validateSurname("Ivanov").toValidated,
      TestUser.validateAge(31).toValidated
    ).mapN { (name, surname, age) =>
      TestUser(name, surname, age)
    }.toEither
    either.isRight shouldBe true
  }

  "invalid user" should "work" in {
    val either = (
      TestUser.validateName("").toValidated,
      TestUser.validateSurname("").toValidated,
      TestUser.validateAge(31).toValidated
    ).mapN { (name, surname, age) =>
      TestUser(name, surname, age)
    }.toEither
    either.isLeft shouldBe true
  }

}
