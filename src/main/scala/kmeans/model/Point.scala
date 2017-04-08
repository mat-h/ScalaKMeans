package kmeans.model

import scala.util.Random

class Point(val coord: List[Double]) {
  override def toString = coord.map(_.toString).mkString(",")

  def +(other: Point) = new Point(
    coord.zip(other.coord)
      .map(p => p._1 + p._2))

  def *(multiplier: Double) = new Point(coord.map(_ * multiplier))

  def /(divisor: Double) = new Point(coord.map(_ / divisor))

  /* KMeans needs cluster, responsibilities, contributions */

  var cluster: Seq[Point] = Nil

  var responsibilities: Seq[Double] = Nil
  def updateResponsibilities = {
    def distance(other: Point): Double = Math.sqrt(
      coord.zip(other.coord)
        .map(p => p._1 - p._2)
        .map(Math.pow(_, 2))
        .sum)

    def exponential(x: Double): Double = Math.exp((-1.0) * 100 * x)

    responsibilities = cluster.map(distance).map(exponential)
  }

  def contributionVector: Seq[Point] = {
    val s = responsibilities.sum
    responsibilities
      .map(_ / s)
      .map(normalizedResp => coord.map(_ * normalizedResp))
      .map(new Point(_))
  }
}

object Point {
  private val rand = new Random(System.currentTimeMillis())

  def randomPoint: Point = new Point(List(rand.nextDouble(), rand.nextDouble()))
}