package functional

import org.scalatest._
import functional.HighOrderFunctions._

class HighOrderFunctionsTest extends FunSuite {
  val as = Array("fib", "abs", "factorial", "loop")

  test("Test abs function") {
    assertResult(3){
      abs(3)
    }

    assertResult(3){
      abs(-3)
    }
  }

  test("Test fibonacci function") {
    assertResult(610) {
      fibonacci(15)
    }
  }

  test("Test factorial function") {
    assertResult(2004310016) {
      factorial(15)
    }
  }

  test("Test firstElem function") {
    assertResult(1) {
      val first_comp = (p: String) => p == "abs"
      firstElem(as, first_comp)
    }
  }

  test("Test isSorted function") {
    val str_comp = (x: String, y: String) => x.length > y.length
    assert(!isSorted(as, str_comp))
    val as_sort = Array("fib", "abs", "factorial", "loopingssss")
    assert(isSorted(as_sort, str_comp))
  }
}
