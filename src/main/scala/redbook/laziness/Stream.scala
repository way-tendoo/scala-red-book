package redbook.laziness

import redbook.laziness.Stream.{ cons, empty, unfold }

import scala.annotation.tailrec

sealed trait Stream[+A] {

  def headOption: Option[A] = foldRight(None: Option[A])((head, _) => Some(head))

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

  def append[AA >: A](other: Stream[AA]): Stream[AA] = foldRight(other)((a, acc) => cons(a, acc))

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(head, tail) => f(head(), tail().foldRight(z)(f))
    case _                => z
  }

  def take(n: Int): Stream[A] = unfold((this, n)) {
    case (_, n) if n == 0      => None
    case (Empty, _)            => None
    case (Cons(head, tail), n) => Some(head(), (tail(), n - 1))
  }

  def takeWhile(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(head, tail) if p(head()) => Some(head(), tail())
    case _                             => None
  }

  def exists(p: A => Boolean): Boolean = foldRight(false)((a, acc) => p(a) || acc)

  def forall(p: A => Boolean): Boolean = foldRight(true)((a, acc) => p(a) && acc)

  def map[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(head, tail) => Some((f(head()), tail()))
    case Empty            => None
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Stream.empty[B])((a, acc) => f(a).append(acc))

  def filter(p: A => Boolean): Stream[A] = flatMap(a => if (p(a)) Stream(a) else Stream.empty)

  def zipWith[AA >: A](other: Stream[AA])(zip: (AA, AA) => AA): Stream[AA] = unfold((this, other)) {
    case (Empty, Empty)            => None
    case (Empty, rhs @ Cons(_, _)) => Some((rhs.head(), (Empty, rhs.tail())))
    case (lhs @ Cons(_, _), Empty) => Some((lhs.head(), (lhs.tail(), Empty)))
    case (lhs @ Cons(_, _), rhs @ Cons(_, _)) =>
      val zipped = zip(lhs.head(), rhs.head())
      Some((zipped, (lhs.tail(), rhs.tail())))
  }

  def zipAll[B](other: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, other)) {
    case (Empty, Empty)                       => None
    case (Empty, rhs @ Cons(_, _))            => Some(((None, Some(rhs.head())), (Empty, rhs.tail())))
    case (lhs @ Cons(_, _), Empty)            => Some(((Some(lhs.head()), None), (lhs.tail(), Empty)))
    case (lhs @ Cons(_, _), rhs @ Cons(_, _)) => Some(((Some(lhs.head()), Some(rhs.head())), (lhs.tail(), rhs.tail())))
  }

  def startsWith[AA >: A](prefix: Stream[AA]): Boolean = {
    if (prefix == Empty) return false
    @tailrec
    def loop(source: Stream[AA], prefix: Stream[AA]): Boolean = (source, prefix) match {
      case (_, Empty)                                               => true
      case (s @ Cons(_, _), p @ Cons(_, _)) if s.head() == p.head() => loop(s.tail(), p.tail())
      case _                                                        => false
    }
    loop(this, prefix)
  }

  def tails: Stream[Stream[A]] = scanRight(empty[A]) ((a, acc) => Stream(a).append(acc))

  def scanRight[B](init: => B)(f: (A, => B) => B): Stream[B] = this match {
    case Empty          => Stream(init)
    case s @ Cons(_, _) => cons(s.foldRight(init)(f), s.tail().scanRight(init)(f))
  }

  def hasSubsequence[AA >: A](other: Stream[AA]): Boolean = tails.exists(_ startsWith other)

}
case object Empty                                         extends Stream[Nothing]
case class Cons[+A](head: () => A, tail: () => Stream[A]) extends Stream[A]

object Stream {

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def unfold[A, S](init: S)(f: S => Option[(A, S)]): Stream[A] = f(init) match {
    case Some(value) =>
      val (a, s) = value
      cons(a, unfold(s)(f))
    case None => Empty
  }

}
