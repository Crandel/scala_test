import scala.io.StdIn.{readLine}
object HelloWorld {
  def main(args: Array[String]) {
    var nmb = 0
    do {
      println("Guess number")
      nmb = readLine.toInt
    } while(nmb != 15)
    printf("Congrats the number is %d\n", nmb)
  }
}
