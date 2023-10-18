package redbook.chapter3

import scala.annotation.tailrec

sealed trait List[+A] {

  def getTail: List[A] = this match {
    case Cons(_, tail) => tail
    case _             => Nil
  }

  def setHead[AA >: A](newHead: AA): List[AA] = this match {
    case Cons(_, tail) => Cons(newHead, tail)
    case _             => Nil
  }

  def foldLeft[B](init: B)(f: (B, A) => B): B = {
    @tailrec
    def loop(list: List[A])(buffer: B): B = list match {
      case Nil              => buffer
      case Cons(head, tail) => loop(tail)(f(buffer, head))
    }
    loop(this)(init)
  }

  def reverse: List[A] = this.foldLeft(Nil: List[A])((acc, a) => Cons(a, acc))

  def dropWhile(predicate: A => Boolean): List[A] = {
    @tailrec
    def loop(list: List[A]): List[A] = list match {
      case Cons(head, tail) if predicate(head) => loop(tail)
      case list                                => list
    }
    loop(this)
  }

  def foldRight[B](init: B)(f: (A, B) => B): B = this.reverse.foldLeft(init)((acc, a) => f(a, acc))

  /*
   * Non tail-recursive impl, can throw StackOverflowException
   */
  def foldRightNotOptimal[B](init: B)(f: (A, B) => B): B = this match {
    case Nil              => init
    case Cons(head, tail) => f(head, tail.foldRightNotOptimal(init)(f))
  }

  def drop(n: Int): List[A] = {
    @tailrec
    def loop(list: List[A])(n: Int): List[A] = list match {
      case Nil            => Nil
      case list if n == 0 => list
      case Cons(_, tail)  => loop(tail)(n - 1)
    }
    loop(this)(n)
  }

  def append[AA >: A](other: List[AA]): List[AA] = this.foldRight(other)((a, acc) => Cons(a, acc))

  def init: List[A] = {
    @tailrec
    def loop(source: List[A], buffer: List[A]): List[A] = source match {
      case Nil              => Nil
      case Cons(_, Nil)     => buffer
      case Cons(head, tail) => loop(tail, buffer.append(Cons(head, Nil)))
    }
    loop(this, Nil)
  }

  def length: Int = this.foldLeft(0)((acc: Int, _) => acc + 1)

  def forall(p: A => Boolean): Boolean = {
    @tailrec
    def loop(list: List[A]): Boolean = list match {
      case Nil                         => true
      case Cons(head, _) if !p(head)   => false
      case Cons(head, tail) if p(head) => loop(tail)
    }
    loop(this)
  }

  def map[B](f: A => B): List[B] = this.foldRight(Nil: List[B])((a, acc) => Cons(f(a), acc))

  def flatMap[B](f: A => List[B]): List[B] = this.map(f).flatten

  def zipWith[AA >: A](other: List[AA])(zip: (AA, AA) => AA): List[AA] = {
    val (minLengthList, maxLengthList) = if (this.length <= other.length) (this, other) else (other, this)

    @tailrec
    def loop(minLengthList: List[AA], maxLengthList: List[AA])(buffer: List[AA]): List[AA] =
      (minLengthList, maxLengthList) match {
        case (Nil, maxLengthList) => buffer.append(maxLengthList)
        case (Cons(minLengthListHead, minLengthListTail), Cons(maxLengthListHead, maxLengthListTail)) =>
          val zipped = zip(minLengthListHead, maxLengthListHead)
          loop(minLengthListTail, maxLengthListTail)(buffer.append(List(zipped)))
      }

    loop(minLengthList, maxLengthList)(Nil)
  }

  def filter(p: A => Boolean): List[A] = this.flatMap(i => if (p(i)) List(i) else Nil)

  def foreach(f: A => Unit): Unit = this.map(f)

  override def toString: String = this.foldLeft("")((acc, a) => acc + a.toString)

  def hasSubsequence[AA >: A](sub: List[AA]): Boolean = {
    val thisStr = this.toString
    val subStr  = sub.toString
    if (thisStr.nonEmpty && subStr.isEmpty) return false
    thisStr.contains(subStr)
  }

}
case object Nil                             extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  /**
   * The following implementation is not optimal, can throw StackOverflowException
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

  implicit class ListsOps[A](val lists: List[List[A]]) {
    def flatten: List[A] = lists.foldRight(Nil: List[A])((a, acc) => a.append(acc))
  }
}
