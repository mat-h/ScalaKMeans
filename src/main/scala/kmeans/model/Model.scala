package kmeans.model

/* 実装完了 */
class Model(data: Seq[List[Double]], MAXTIME: Int, verbose: Boolean) {
  val points: Seq[Point] = data.map(new Point(_))
  val dimension = points(0).coord.size

  private var clusters: Seq[Point] =
    for (i <- 1 to dimension) yield Point.randomPoint

  points.foreach(_.cluster = clusters)

  private var time: Int = 0
  def iterate = for (t <- 0 to MAXTIME) { time = t; update }

  private def update = {
    points.foreach(_.updateResponsibilities)
    val totalResp = for (i <- 0 to dimension - 1)
      yield points.map(_.responsibilities(i)).sum
    clusters = points
      .map(_.contributionVector.reduce(_ + _))
      .zip(totalResp)
      .map(p => p._1 / p._2)

    if (verbose) dump
  }

  def dump = {
    println("clusters state (time=" + time + "): ");
    (0 to dimension - 1).foreach(i => {
      println("cluster " + i + "'s total weight is " +
        points.map(_.responsibilities(i)).sum
        + ". center coordinates are " + clusters(i))
    })
  }
}