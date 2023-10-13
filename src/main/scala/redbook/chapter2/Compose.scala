package redbook.chapter2

object Compose {

  def compose[A, B, C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))
}
