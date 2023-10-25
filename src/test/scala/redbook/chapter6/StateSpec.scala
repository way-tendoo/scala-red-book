package redbook.chapter6

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class StateSpec extends AnyFlatSpec with should.Matchers {

  "map func" should "work" in {
    State
      .unit[Machine, (Int, Int)]((0, 0))
      .map { case (a, b) => (a + 1, b + 1) }
      .run(Machine(locked = true, 10, 5))
      ._1 shouldBe (1, 1)
  }

  "flatMap func" should "work" in {
    val s = for {
      a <- State.unit[Machine, (Int, Int)]((1, 1))
      b <- State.unit[Machine, (Int, Int)]((2, 2))
    } yield {
      (a._1 + b._1, a._2 + b._2)
    }
    s.run(Machine(locked = true, 10, 5))._1 shouldBe (3, 3)
  }

  "unit func" should "work" in {
    State.unit[Machine, (Int, Int)]((0, 0)).run(Machine(locked = true, 10, 5))._1 shouldBe (0, 0)
    State.unit[Machine, (Int, Int)]((0, 0)).run(Machine(locked = true, 10, 5))._2 shouldBe Machine(locked = true, 10, 5)
  }

  "map2 func" should "work" in {
    State
      .unit[Machine, (Int, Int)]((1, 1))
      .map2(State.unit[Machine, (Int, Int)]((2, 2)))((a, b) => (a._1 + b._1, a._2 + b._2))
      .run(Machine(locked = true, 10, 5))
      ._1 shouldBe (3, 3)
  }

  "modify func" should "work" in {
    State
      .unit[Machine, (Int, Int)]((1, 1))
      .modify(_.copy(locked = false))
      .run(Machine(locked = true, 10, 5))
      ._2
      .locked shouldBe false
  }

}
