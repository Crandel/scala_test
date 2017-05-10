import scala.io.StdIn.{readLine}
object StdINOUTExample {
  def main(args: Array[String]) {
    var nmb = 0
    do {
      println("Guess number, please")
      nmb = readLine.toInt
    } while(nmb != 15)
    println(f"Congrats the number is $nmb")
  }
}
