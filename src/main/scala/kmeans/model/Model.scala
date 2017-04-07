package kmeans.model

class Model(data: Seq[List[Double]], MAXTIME: Int, verbose: Boolean) {
  val points: Seq[Point] = data.map(new Point(_))

  private var time: Int = 0

  val dimension = points(0).coord.size

  private val clusters: Seq[Point] =
    for (i <- 1 to dimension) yield Point.randomPoint

  points.foreach(_.cluster = clusters)

  def iterate = {
    (0 to MAXTIME).foreach(i => {
      update
      if (verbose) dump
    })
  }

  def update = {
    points.foreach(_.updateResponsibilities)
    val totalResp = for (i <- 0 to dimension-1)
      yield points.map(_.responsibilities(i)).sum
    println(totalResp.map(_.toString).mkString(","))
  }

  def dump = {}
}