package redbook.chapter4

case class Validated[+E, +A](value: Option[A], errors: List[E])

object Validated {

  implicit class Tuple3Ops[E, A1, A2, A3](t: (Validated[E, A1], Validated[E, A2], Validated[E, A3])) {
    def mapN[R](func: (A1, A2, A3) => R): Validated[E, R] = {
      val (first, second, third) = t
      val validated = for {
        f <- first.value
        s <- second.value
        t <- third.value
      } yield {
        Validated(Some(func(f, s, t)), List.empty[E])
      }
      validated.getOrElse {
        Validated(None, first.errors ++ second.errors ++ third.errors)
      }
    }
  }

  implicit class ValidatedOps[E, A](v: Validated[E, A]) {
    def toEither: Either[List[E], A] = v match {
      case Validated(value, errors) =>
        value match {
          case Some(value) => Right(value)
          case None        => Left(errors)
        }
    }
  }

  implicit class EitherOps[E, A](val e: Either[E, A]) {
    def toValidated: Validated[E, A] = e match {
      case Left(error)  => Validated(None, List(error))
      case Right(value) => Validated(Some(value), List.empty)
    }
  }
}
