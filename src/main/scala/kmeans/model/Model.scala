package kmeans.model

/* 実装完了 */
class Model(data: Seq[List[Double]], MAXTIME: Int, verbose: Boolean) {
  private val points: Seq[Point] = data.map(new Point(_))
  private val dimension = points(0).coord.size
  private val dim_index = 0 to dimension - 1

  private var clusters: Seq[Point] =
    for (i <- dim_index) yield Point.randomPoint
  points.foreach(_.cluster = clusters)

  private def update = {
    points.foreach(_.updateResponsibilities)
    
    val totalResp = this.totalResp

    val totalCont = this.totalCont

    clusters = totalCont.zip(totalResp).map(p => p._1 / p._2)

    points.foreach(_.cluster = clusters)

    if (verbose) dump
  }

  type Matrix[T] = Seq[Seq[T]]
  def takeRow[T](matrix: Matrix[T], i: Int) = matrix(i)
  def takeColumn[T](matrix: Matrix[T], i: Int) = matrix.map(_(i))

  private def totalResp = {
    val responsibillitiesMatrix = points.map(_.responsibilities)
    
    for (i <- dim_index)
      yield takeColumn(responsibillitiesMatrix, i).sum
  }

  private def totalCont = {
    val contributionMatrix = points.map(_.contributionVectors)
    
    for (i <- dim_index)
      yield takeColumn(contributionMatrix, i).reduce(_ + _)
  }

  def dump = {
    println("clusters state (time=" + time + "): ")
    println("center coordinates are " + clusters)
    dim_index.foreach(i => {
      println("cluster " + i + "'s total weight is " + totalResp(i))
    })
  }

  private var time: Int = 0
  def iterate = for (t <- 0 to MAXTIME) { time = t; update }
}