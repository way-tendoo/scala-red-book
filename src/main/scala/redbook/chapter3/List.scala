package redbook.chapter3

import redbook.chapter2.{ Curry, Uncurry }

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil                             extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  /**
   * The following implementation is not optimal
   */
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def apply[A](elems: Vector[A]): List[A] = {
    @tailrec
    def loop(elems: Vector[A])(acc: List[A], idx: Int): List[A] = elems match {
      case _ if idx == -1 => acc
      case _              => loop(elems)(Cons(elems(idx), acc), idx - 1)
    }
    loop(elems)(Nil, elems.length - 1)
  }

  def tail[A](list: List[A]): List[A] = list match {
    case Cons(_, tail) => tail
    case _             => Nil
  }

  def setHead[A](list: List[A], newHead: A): List[A] = list match {
    case Cons(_, tail) => Cons(newHead, tail)
    case _             => Nil
  }

  def dropWhile[A](list: List[A], predicate: A => Boolean): List[A] = {
    @tailrec
    def loop(list: List[A]): List[A] = list match {
      case Nil                                 => Nil
      case Cons(head, tail) if predicate(head) => loop(tail)
      case list                                => list
    }
    loop(list)
  }

  def drop[A](list: List[A], n: Int): List[A] = {
    @tailrec
    def loop(list: List[A])(n: Int): List[A] = list match {
      case Nil            => Nil
      case list if n == 0 => list
      case Cons(_, tail)  => loop(tail)(n - 1)
    }
    loop(list)(n)
  }

  def append[A](lhs: List[A], rhs: List[A]): List[A] = {
    foldRight(lhs, rhs)((a, acc) => Cons(a, acc))
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

  /*
   * Non tail-recursive impl, maybe throw StackOverflowException
   */
  def foldRightNotOptimal[A, B](list: List[A], init: B)(f: (A, B) => B): B = list match {
    case Nil              => init
    case Cons(head, tail) => f(head, foldRightNotOptimal(tail, init)(f))
  }

  def foldRight[A, B](list: List[A], init: B)(f: (A, B) => B): B = {
    foldLeft(reverse(list), init)((acc, a) => f(a, acc))
  }

  def foldLeft[A, B](list: List[A], init: B)(f: (B, A) => B): B = {
    @tailrec
    def loop(list: List[A])(acc: B): B = list match {
      case Nil              => acc
      case Cons(head, tail) => loop(tail)(f(acc, head))
    }
    loop(list)(init)
  }

  def length[A](list: List[A]): Int = {
    foldLeft(list, 0)((acc: Int, _) => acc + 1)
  }

  def reverse[A](list: List[A]): List[A] = {
    foldLeft(list, Nil: List[A])((acc, a) => Cons(a, acc))
  }

  def flatten[A](lists: List[List[A]]): List[A] = {
    foldRight(lists, Nil: List[A])((a, acc) => append(a, acc))
  }

  def map[A, B](list: List[A])(f: A => B): List[B] = {
    foldRight(list, Nil: List[B])((a, acc) => Cons(f(a), acc))
  }

  def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] = {
    List.flatten(List.map(list)(f))
  }

  def filter[A](list: List[A])(p: A => Boolean): List[A] = {
    flatMap(list)(i => if (p(i)) List(i) else Nil)
  }

  def foreach[A](list: List[A])(a: A => Unit): Unit = map(list)(a)

  def zipWith[A](lhs: List[A], rhs: List[A])(zip: (A, A) => A): List[A] = {
    val lists = if (length(lhs) <= length(rhs)) (lhs, rhs) else (rhs, lhs)
    @tailrec
    def loop(lists: (List[A], List[A]))(acc: List[A]): List[A] = lists match {
      case (Nil, maxList) => append(acc, maxList)
      case (Cons(lhsHead, lhsTail), Cons(rhsHead, rhsTail)) =>
        loop(lhsTail, rhsTail)(append(acc, List(zip(lhsHead, rhsHead))))
    }
    loop(lists)(Nil)
  }

  def toString[A](list: List[A]): String = foldLeft(list, "")((acc, a) => acc + a.toString)

  def hasSubsequence[A](list: List[A], sub: List[A]): Boolean = {
    val listStr = toString(list)
    val subStr  = toString(sub)
    if (listStr.nonEmpty && subStr.isEmpty) return false
    listStr.contains(subStr)
  }
}
