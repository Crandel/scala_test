import simple._

object SimpleApp {
  def main(args: Array[String]): Unit = {
    println("*" * 10)
    val as = Array("fib", "abs", "factorial", "loop")
    val first_comp = (p: String) => p == "abs"
    println(FunctionalPR.first_elem(as, first_comp))
    val str_comp = (x: String, y: String) => x.length > y.length
    println((FunctionalPR.isSorted(as, str_comp)))
    val as_sort = Array("fib", "abs", "factorial", "loopingssss")
    println("=" * 10)
    println((FunctionalPR.isSorted(as_sort, str_comp)))
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
