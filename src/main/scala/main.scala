import functional.{Currying, Functions}

object StartApp {
  def main(args: Array[String]): Unit = {
    println("Currying example" * 3)
    Currying()
    println()
    println("Funcs example" * 3)
    Functions()
  }
}
