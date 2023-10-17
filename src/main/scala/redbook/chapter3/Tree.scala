package redbook.chapter3

sealed trait Tree[+A]
case class Leaf[A](value: A)                        extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](t: Tree[A]): Int =
    fold(t)(_ => 1)((lhs, rhs) => 1 + lhs + rhs)

  def maximum[A](t: Tree[A])(max: (A, A) => A): A =
    fold(t)(identity)(max)

  def depth[A](t: Tree[A]): Int =
    fold(t)(_ => 1)((lhs, rhs) => 1 + Math.max(lhs, rhs))

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))

  def fold[A, B](t: Tree[A])(f: A => B)(combine: (B, B) => B): B = t match {
    case Leaf(value)         => f(value)
    case Branch(left, right) => combine(fold(left)(f)(combine), fold(right)(f)(combine))
  }

}
