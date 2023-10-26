package redbook.data_structures

sealed trait Tree[+A] {

  def size: Int = fold(_ => 1) {
    case (lhs, rhs) =>
      lhs + rhs + 1
  }

  def depth: Int = fold(_ => 1) {
    case (lhs, rhs) =>
      Math.max(lhs, rhs) + 1
  }

  def maximum[AA >: A](max: (AA, AA) => AA): AA = fold(_.asInstanceOf[AA])(max)

  def map[B](f: A => B): Tree[B] = fold(a => Leaf(f(a)): Tree[B])(Branch(_, _))

  def fold[B](f: A => B)(combine: (B, B) => B): B = this match {
    case Leaf(value)         => f(value)
    case Branch(left, right) => combine(left.fold(f)(combine), right.fold(f)(combine))
  }

}
case class Leaf[A](value: A)                        extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
