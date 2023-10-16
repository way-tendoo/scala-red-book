package redbook.chapter3

sealed trait Tree[+A]
case class Leaf[A](value: A)                        extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_)             => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  def maximum[A](t: Tree[A])(max: (A, A) => A): A = t match {
    case Leaf(value)         => value
    case Branch(left, right) => max(maximum(left)(max), maximum(right)(max))
  }

  def depth[A](t: Tree[A]): Int = {
    def loop(t: Tree[A])(depth: Int): Int = t match {
      case Leaf(_)             => depth
      case Branch(left, right) => Math.max(loop(left)(depth + 1), loop(right)(depth + 1))
    }
    loop(t)(1)
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(value)         => Leaf(f(value))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

}
