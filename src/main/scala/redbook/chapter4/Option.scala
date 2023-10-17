package redbook.chapter4

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = flatMap(a => Some(f(a)))

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(value) => f(value)
    case None        => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(value) => value
    case None        => default
  }

  def isDefined: Boolean = this match {
    case Some(_) => true
    case None    => false
  }

  def get: A = this match {
    case Some(value) => value
    case None        => throw new UnsupportedOperationException(s"None.get")
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case Some(value) => Some(value)
    case None        => ob
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(value) if f(value) => Some(value)
    case _                       => None
  }
}

case class Some[+A](value: A) extends Option[A]
case object None              extends Option[Nothing]

object Option {

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      a <- a
      b <- b
    } yield f(a, b)

  def sequence[A](list: List[Option[A]]): Option[List[A]] = {
    if (list.isEmpty) return None
    val allSuccess = list.forall(_.isDefined)
    if (allSuccess) Some(list.map(_.get))
    else None
  }

  def traverse[A, B](list: List[A])(f: A => Option[B]): Option[List[B]] = sequence(list.map(f))

}
