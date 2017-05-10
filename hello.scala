import scala.collection.mutable.ArrayBuffer
object HelloWorld {
  def main(args: Array[String]) {
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
    println(f"Sum ${mutList.sum}")
    println(f"Min ${mutList.min}")
    println(f"Max ${mutList.max}")
    val sortList = mutList.sortWith(_<_)
    println(f"sortList ${sortList}")
  }
}
