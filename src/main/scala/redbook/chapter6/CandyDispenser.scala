package redbook.chapter6

case class Machine(locked: Boolean, candies: Int, coins: Int)

object CandyDispenser {

  private def insertCoin(): State[Machine, (Int, Int)] = State[Machine, (Int, Int)] {
    case m @ Machine(locked, candies, coins) if candies == 0 | !locked => ((candies, coins), m)
    case Machine(_, candies, coins) =>
      ((candies, coins + 1), Machine(locked = false, candies = candies, coins = coins + 1))
  }

  private def turnKnob(): State[Machine, (Int, Int)] = State[Machine, (Int, Int)] {
    case m @ Machine(locked, candies, coins) if candies == 0 | locked => ((candies, coins), m)
    case Machine(_, candies, coins) =>
      ((candies - 1, coins), Machine(locked = true, candies = candies - 1, coins = coins))
  }

  def simulate(inputs: List[Input]): State[Machine, (Int, Int)] = {
    inputs.map {
      case Coin => insertCoin()
      case Turn => turnKnob()
    }.sequence.map(_.reverse.headOption.getOrElse((0, 0)))
  }
}
