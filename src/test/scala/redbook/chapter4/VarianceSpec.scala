package redbook.chapter4

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class VarianceSpec extends AnyFlatSpec with should.Matchers {

  "variance func" should "work" in {
    Variance.variance(Seq(5.0, 2.0, 3.0, 5.0, 4.0, 5.0)) shouldBe Some(1.3333333333333333)
    Variance.variance(Seq()) shouldBe None
  }
}
