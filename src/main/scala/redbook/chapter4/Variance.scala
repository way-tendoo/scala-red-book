package redbook.chapter4

object Variance {

  def variance(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else {
      val avg = xs.sum / xs.length
      Some(xs.map(x => Math.pow(x - avg, 2)).sum / xs.length)
    }
  }
}
