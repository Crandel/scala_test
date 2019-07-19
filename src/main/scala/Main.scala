import primitives.FuncList
import functional.{Currying, HighOrderFunctions}

object Main {
  def main(args: Array[String]): Unit = {
    FuncList.test()
    Currying()
    HighOrderFunctions()
  }
}
