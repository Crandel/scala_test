package simple

import scala.collection.mutable.{ArrayBuffer, Map => MMap}

object TupleExample {
  def run(args: Array[String]) {
    var tup = (1, "Gomer", 33.6)
    println(f"${tup._2} should pay ${tup._3}%.2f dollars for beer")
    tup.productIterator.foreach{ i => println(i) }
    println(tup.toString)
  }
}
