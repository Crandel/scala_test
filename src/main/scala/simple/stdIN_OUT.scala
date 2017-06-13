package simple

import scala.io.StdIn.{readLine}
import java.io.PrintWriter
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object StdINOUTExample {
  def run(args: Array[String]) {
    var nmb = 0
    val numArray = ArrayBuffer.empty[Int]
    do {
      println("Guess number, please")
      nmb = readLine.toInt
      numArray += nmb
    } while(nmb != 15)
    println(f"Congrats the number is $nmb")
    println(numArray.toString)
    println()
    val writer = new PrintWriter("test.txt")
    for(m <- numArray){
      writer.write(m.toString + "\n")
    }
    writer.close()
    val text = Source.fromFile("test.txt", "UTF-8")
    text.foreach(println)
    text.close()
  }
}
