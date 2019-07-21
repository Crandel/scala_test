package structures

import scala.collection.mutable.ArrayBuffer

object ArrayExamples {
  def apply(): Unit = {
    var INSERT_QUERY = "INSERT INTO $database.$table VALUES "
    var valueList = ArrayBuffer(
      ArrayBuffer("First1", "Second1", "Third1"),
      ArrayBuffer("First2", "Second2", "Third2"),
      ArrayBuffer("First3", "Second3", "Third3"))
    var c = 0
    val stop = valueList.length
    for (v <- valueList){
      c += 1
      if (c != stop){
        INSERT_QUERY += v.mkString("(\"", "\", \"", "\"),")
      } else {
        INSERT_QUERY += v.mkString("(\"", "\", \"", "\")")
      }
    }
    println(INSERT_QUERY)
    intArrays
  }

  def intArrays() {
    var mutList = ArrayBuffer[Int]()
    mutList.insert(0, 2)
    println(mutList)
    mutList += 4
    println(mutList)
    mutList ++= ArrayBuffer(455, 677)
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
