import functional.{Currying, Functions}
import primitives.ListExample

object StartApp {
  def main(args: Array[String]): Unit = {
    println("Currying example" * 3)
    Currying()
    println()
    println("Funcs example" * 3)
    Functions()
    println("Lists")
    ListExample()
  }
}
