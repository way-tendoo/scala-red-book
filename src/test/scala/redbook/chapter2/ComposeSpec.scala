package redbook.chapter2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class ComposeSpec extends AnyFlatSpec with should.Matchers {

  "compose func" should "work" in {
    val strToIntAdd2 = Compose.compose((b: Int) => b + 2, (a: String) => Integer.parseInt(a))

    strToIntAdd2("2") shouldBe 4
  }
}
