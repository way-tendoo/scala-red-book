package redbook.introduction

import scala.annotation.tailrec

object IsSorted {

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    if (as.length <= 1) return true

    @tailrec
    def loop(prev: A)(idx: Int): Boolean = {
      if (idx == as.length - 1) return true
      if (ordered(prev, as(idx)))
        loop(as(idx))(idx + 1)
      else false
    }

    loop(as(0))(1)
  }
}
