package simple

import scala.io.StdIn.{readLine}
import java.io.PrintWriter
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object StdINOUTExample {
  def apply() {
    var nmb = 0
    var filename = "test.txt"
    val numArray = ArrayBuffer.empty[Int]
    do {
      println("Guess number, please")
      val lne = readLine
      if (lne.forall(_.isDigit)){
        nmb = lne.toInt
        numArray += nmb
      } else {
        filename = lne
      }
    } while(nmb != 15)
    println(f"Congrats the number is $nmb")
    println(numArray.toString)
    println()
    println(s"filename is $filename")
    val full_filename = "files/" + filename
    val writer = new PrintWriter(full_filename)
    for(m <- numArray){
      writer.write(m.toString + "\n")
    }
    writer.close()
    val text = Source.fromFile(full_filename, "UTF-8")
    text.foreach(println)
    text.close()
  }
}
