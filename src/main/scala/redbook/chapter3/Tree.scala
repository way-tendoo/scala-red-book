package redbook.chapter3

sealed trait Tree[+A] {

  def size: Int = this.fold(_ => 1)((lhs, rhs) => 1 + lhs + rhs)

  def depth: Int = this.fold(_ => 1)((lhs, rhs) => 1 + Math.max(lhs, rhs))

  def maximum[AA >: A](max: (AA, AA) => AA): AA = this.fold(_.asInstanceOf[AA])(max)

  def map[B](f: A => B): Tree[B] = this.fold(a => Leaf(f(a)): Tree[B])(Branch(_, _))

  def fold[B](f: A => B)(combine: (B, B) => B): B = this match {
    case Leaf(value)         => f(value)
    case Branch(left, right) => combine(left.fold(f)(combine), right.fold(f)(combine))
  }
}

case class Leaf[A](value: A)                        extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

