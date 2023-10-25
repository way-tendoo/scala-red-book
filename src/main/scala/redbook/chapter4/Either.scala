package redbook.chapter4

sealed trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] = this match {
    case Right(value) => Right(f(value))
    case Left(value)  => Left(value)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(value) => f(value)
    case Left(value)  => Left(value)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(value) => Right(value)
    case Left(_)      => b
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      a <- this
      b <- b
    } yield f(a, b)

  def isLeft: Boolean = this match {
    case Left(_)  => true
    case Right(_) => false
  }

  def right: Right[A] = this match {
    case Left(_)      => throw new UnsupportedOperationException("Left.right")
    case Right(value) => Right(value)
  }

  def left: Left[E] = this match {
    case Left(value) => Left(value)
    case Right(_)    => throw new UnsupportedOperationException("Right.left")
  }
}

case class Left[+E](value: E) extends Either[E, Nothing] {
  def get: E = value
}
case class Right[+A](value: A) extends Either[Nothing, A] {
  def get: A = value
}

object Either {

  implicit class ListEitherOps[E, A](val l: List[Either[E, A]]) {
    def sequence: Either[E, List[A]] = {
      val (errors, values) = l.partition(_.isLeft)
      if (errors.nonEmpty) Left(errors.head.left.get)
      else Right(values.map(_.right.get))
    }
  }

  implicit class ListOps[A](val l: List[A]) {
    def traverse[E, B](f: A => Either[E, B]): Either[E, List[B]] = l.map(f).sequence
  }

}
