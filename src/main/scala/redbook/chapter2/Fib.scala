package redbook.chapter2

import scala.annotation.tailrec

object Fib {

  def fib(n: Int): Int = {
    if (n == 1) return 0
    if (n == 2) return 1
    @tailrec
    def loop(prev: Int, current: Int)(counter: Int): Int = {
      if (counter == n)
        current
      else
        loop(current, prev + current)(counter + 1)
    }
    loop(0, 1)(2)
  }
}
