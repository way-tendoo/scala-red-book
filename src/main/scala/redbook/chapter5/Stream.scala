package redbook.chapter5

import redbook.chapter5.Stream.cons

import scala.annotation.tailrec

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty         => None
    case Cons(head, _) => Some(head())
  }

  def toList: List[A] = this match {
    case Empty            => Nil
    case Cons(head, tail) => head() :: tail().toList
  }

  def drop(n: Int): Stream[A] = {
    @tailrec
    def loop(s: Stream[A])(n: Int): Stream[A] = s match {
      case Empty                      => Empty
      case Cons(head, tail) if n == 0 => cons(head(), tail())
      case Cons(_, tail)              => loop(tail())(n - 1)
    }
    loop(this)(n)
  }

  def take(n: Int): Stream[A] = {
    @tailrec
    def loop(s: Stream[A])(buffer: Stream[A])(n: Int): Stream[A] = s match {
      case Empty | Cons(_, _) if n == 0 => buffer
      case Cons(_, tail)                => loop(tail())(buffer)(n - 1)
    }
    loop(this)(Empty)(n)
  }
}
case object Empty                                   extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty
}
