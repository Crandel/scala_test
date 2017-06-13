package simple

import scala.collection.mutable.ArrayBuffer

object ArrayExamples {
  def run() {
    var mutList = ArrayBuffer[Int]()
    mutList.insert(0, 2)
    println(mutList)
    mutList += 4
    println(mutList)
    mutList ++= Array(455, 677)
    println(mutList)
    mutList.insert(1, 5555)
    println(mutList)
    mutList(3) = 88888888
    println(mutList)
    println(s"Sum ${mutList.sum}")
    println(s"Min ${mutList.min}")
    println(s"Max ${mutList.max}")
    val sortList = mutList.sortWith(_<_)
    println(s"sortList ${sortList}")
  }
}
