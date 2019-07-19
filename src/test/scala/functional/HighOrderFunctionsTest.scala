package functional

import org.scalatest._
import functional.HighOrderFunctions._

class HighOrderFunctionsTest extends FunSuite {
  val as = Array("fib", "abs", "factorial", "loop")

  test("abs function") {
    assertResult(3){
      abs(3)
    }

    assertResult(3){
      abs(-3)
    }
  }

  test("fibonacci function") {
    assertResult(610) {
      fibonacci(15)
    }
  }

  test("factorial function") {
    assertResult(2004310016) {
      factorial(15)
    }
  }

  test("firstElem function") {
    assertResult(1) {
      val first_comp: String => Boolean = (p: String) => p == "abs"
      firstElem(as, first_comp)
    }
  }

  test("isSorted function") {
    val str_comp: (String, String) => Boolean = (x: String, y: String) => x.length > y.length
    assert(!isSorted(as, str_comp))
    val as_sort: Array[String] = Array("fib", "abs", "factorial", "loopingssss")
    assert(isSorted(as_sort, str_comp))
  }
}
