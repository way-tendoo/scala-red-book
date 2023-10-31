package redbook.parallelism

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import java.util.concurrent.{ExecutorService, Executors}

class ParSpec extends AnyFlatSpec with should.Matchers {
  import Par._

  private val es: ExecutorService = Executors.newFixedThreadPool(3)

  "unit func" should "work" in {
    Par.unit(5).run(es) shouldBe 5
  }

  "map func" should "work" in {
    Par.unit(5).map(_ + 1).run(es) shouldBe 6
  }

  "flatMap func" should "work" in {
    Par.unit(5).flatMap(a => Par.unit(a + 1)).run(es) shouldBe 6
  }

  "join func" should "work" in {
    Par.join(Par.unit(5).map(a => Par.unit(a + 1))).run(es) shouldBe 6
  }

  "asyncF func" should "work" in {
    Par.unit(5).flatMap(Par.asyncF(_ + 1)).run(es) shouldBe 6
  }

  "lazyUnit func" should "work" in {
    Par.lazyUnit(5).run(es) shouldBe 5
  }

  "fork func" should "work" in {
    Par.fork(Par.unit(5)).run(es) shouldBe 5
  }

}
