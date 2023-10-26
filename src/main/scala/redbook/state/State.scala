package redbook.state

case class State[S, +A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] = flatMap(a => State(s => (f(a), s)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State { s =>
    val (a, newState) = run(s)
    f(a).run(newState)
  }

  def get: State[S, S] = State(s => (s, s))

  def set(s: S): State[S, Unit] = State(_ => ((), s))

  def modify(f: S => S): State[S, Unit] =
    for {
      s <- get
      _ <- set(f(s))
    } yield ()

  def map2[B, C](b: State[S, B])(f: (A, B) => C): State[S, C] =
    for {
      a <- this
      b <- b
    } yield f(a, b)
}

object State {

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  implicit class ListStateOps[S, A](val l: List[State[S, A]]) {
    def sequence: State[S, List[A]] = l.foldRight(unit[S, List[A]](List.empty[A]))((acc, s) => acc.map2(s)(_ :: _))
  }
}
