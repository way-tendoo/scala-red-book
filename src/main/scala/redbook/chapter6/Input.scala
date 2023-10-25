package redbook.chapter6

sealed trait Input
case object Coin extends Input
case object Turn extends Input