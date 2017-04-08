package kmeans.model

import scala.io.Source

object ModelTest extends App {
  
  val data = for(line <- Source.fromFile(args(0)).getLines()) 
    yield (line split(",") toList).map(_.toDouble)

  // instead you can pass data like this: val data = Seq(List(0.5, 0.5), List(0.2, 0.2))

  new Model(data.toSeq, 10, true) iterate
}