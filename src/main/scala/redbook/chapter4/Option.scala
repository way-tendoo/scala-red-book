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

  def map2[B, C](b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      a <- this
      b <- b
    } yield f(a, b)
}

case class Some[+A](value: A) extends Option[A]
case object None              extends Option[Nothing]

object Option {

  implicit class ListOptOps[A](val l: List[Option[A]]) {
    def sequence: Option[List[A]] = {
      if (l.isEmpty) return None
      val allSuccess = l.forall(_.isDefined)
      if (allSuccess) Some(l.map(_.get))
      else None
    }
  }

  implicit class ListOps[A](val l: List[A]) {
    def traverse[B](f: A => Option[B]): Option[List[B]] = l.map(f).sequence
  }

}
