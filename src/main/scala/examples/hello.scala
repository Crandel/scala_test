package examples

//import scala.collection.mutable.{ArrayBuffer, Map => MMap}

object HelloWorld {
  def apply(): Unit = {
    val tup: (Int, String, Double) = (1, "Gomer", 33.6)
    println(f"${tup._2} should pay ${tup._3}%.2f dollars for beer")
    tup.productIterator.foreach{ i => println(i) }
    println(tup.toString)
  }
}
