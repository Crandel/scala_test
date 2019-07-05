import simple._

object SimpleApp {
  def main(args: Array[String]): Unit = {
    println("*" * 10)
    FunctionalPR()
    val as = Array("fib", "abs", "factorial", "loop")
    val compare = (p: String) => p == "abs"
    println(FunctionalPR.first_elem(as, compare))
    // ArrayExamples()
    // println("-" * 10)
    // CSVCleaner()
    // println("/" * 10)
    // StdINOUTExample()
    // println("." * 10)
    // ClassE()
    // println("->" * 10)
    // TraitsObject()
  }
}
