package kmeans.model

import scala.util.Random

class Point(val coord: List[Double]) {
  var cluster: Seq[Point] = Nil
  var responsibilities: Seq[Double] = Nil
  def updateResponsibilities = {
    responsibilities = cluster.map(distance).map(exponential)
  }
  override def toString = coord.map(_.toString).mkString(",")
  
  def distance(other: Point): Double = Math.sqrt(
      coord.zip(other.coord)
        .map(p => p._1 - p._2)
        .map(Math.pow(_, 2))
        .sum
  )
  
  def exponential(x: Double):Double = Math.exp((-1.0) * 100 * x)
}

object Point {
  private val rand = new Random(System.currentTimeMillis())
  
  def randomPoint: Point = new Point(List(rand.nextDouble(), rand.nextDouble()))
}