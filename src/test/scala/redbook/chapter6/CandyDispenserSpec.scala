package redbook.chapter6

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class CandyDispenserSpec extends AnyFlatSpec with should.Matchers {

  "simulate func" should "work" in {
    CandyDispenser
      .simulate(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn))
      .run(Machine(locked = true, 5, 10))
      ._1 shouldBe (1, 14)

    CandyDispenser
      .simulate(List(Turn))
      .run(Machine(locked = true, 5, 10))
      ._1 shouldBe (5, 10)

    CandyDispenser
      .simulate(List(Coin, Turn, Coin, Turn))
      .run(Machine(locked = true, 0, 10))
      ._1 shouldBe(0, 10)

    CandyDispenser
      .simulate(List(Coin, Turn, Coin, Turn, Coin, Turn))
      .run(Machine(locked = true, 2, 10))
      ._1 shouldBe(0, 12)

    CandyDispenser
      .simulate(List(Turn, Coin, Turn, Coin, Turn, Coin, Turn))
      .run(Machine(locked = true, 2, 10))
      ._1 shouldBe(0, 12)
  }

}
