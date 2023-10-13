package redbook.chapter3

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil                             extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](list: List[A]): List[A] = list match {
    case Cons(_, tail) => tail
    case _             => Nil
  }

  def setHead[A](list: List[A], newHead: A): List[A] = list match {
    case Cons(_, tail) => Cons(newHead, tail)
    case _             => Nil
  }

  @tailrec
  def dropWhile[A](list: List[A], predicate: A => Boolean): List[A] = list match {
    case Nil                                 => Nil
    case Cons(head, tail) if predicate(head) => dropWhile(tail, predicate)
    case list                                => list
  }

  @tailrec
  def drop[A](list: List[A], n: Int): List[A] = list match {
    case Nil            => Nil
    case list if n == 0 => list
    case Cons(_, tail)  => drop(tail, n - 1)
  }

  def append[A](target: List[A], source: List[A]): List[A] = target match {
    case Nil              => source
    case Cons(head, tail) => Cons(head, append(tail, source))
  }

  def init[A](list: List[A]): List[A] = {
    @tailrec
    def loop(source: List[A], acc: List[A]): List[A] = source match {
      case Nil              => Nil
      case Cons(_, Nil)     => acc
      case Cons(head, tail) => loop(tail, append(acc, Cons(head, Nil)))
    }
    loop(list, Nil)
  }

  def foldRight[A, B](list: List[A], z: B)(f: (A, B) => B): B = list match {
    case Nil         => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def length[A](list: List[A]): Int = foldRight(list, 0)((_, acc: Int) => acc + 1)

}
