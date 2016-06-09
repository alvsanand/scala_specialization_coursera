package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
                   c: Signal[Double]): Signal[Double] = {

    Var({
      val aVal = a()
      val bVal = b()
      val cVal = c()

      bVal * bVal - 4.0 * aVal * cVal
    })
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Var({
      val aVal = a()
      val bVal = b()
      val deltaVal = delta()

      deltaVal match {
        case d if d < 0 => Set.empty[Double]
        case _ => Set((0.0 - bVal + Math.sqrt(deltaVal)) / (2.0 * aVal), (0.0 - bVal - Math.sqrt(deltaVal)) / (2.0 *
          aVal))
      }
    })
  }
}
