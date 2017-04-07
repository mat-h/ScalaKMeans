package kmeans.model

object ModelTest extends App {

  val data = Seq(List(0.5, 0.5), List(0.2, 0.2))
  val model = new Model(data, 1, true)

  println(model.dimension)
  println(model.points)
  model.update

}