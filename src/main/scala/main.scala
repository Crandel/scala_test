import functional.Currying

object StartApp {
  def main(args: Array[String]): Unit = {
    println("*" * 10)
    val cur = Currying.curry((a: Int, b: Int) => a * b)
    println(cur(5)(4))
  }
}
