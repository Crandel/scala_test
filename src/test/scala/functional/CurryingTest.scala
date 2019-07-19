package functional

import functional.Currying.{add, addPart, average, compose_test, curry, uncurry}
import org.scalatest._


class CurryingTest extends FunSuite {

  test("Test add function"){
    val add5 = add(5)

    assertResult(11) {
      add(5)(6)
    }

    assertResult(0) { add(0)(0)
    }

    assertResult(9) {
      add5(4)
    }

    assertResult(5) {
      add5(0)
    }
  }

  test("Test addPart function") {
    assertResult(10) {
      val partOne = addPart(5) _
      partOne(5)
    }

    assertResult(12) {
      val partTwo = addPart(5)(7)
      partTwo
    }
  }

  test("Test average function") {
    assertResult(67.5) { average(90, 100, 110)(30, 40, 70)(10, 40, 85)
    }

    assertResult(0.0) { average(0)(0)(0)
    }
  }

  test("Test curry function") {
    assertResult(20) {
      val cur_ex = curry((a: Int, b: Int) => a * b)
      cur_ex(5)(4)
    }
  }

  test("Test uncurry function") {
    assertResult(21) {
      val uncurry_func = (x: Int) => (y: Int) => x * y
      val uncurry_res = uncurry(uncurry_func)
      uncurry_res(3, 7)
    }
  }

  test("Test compose function") {
    assertResult(15) {
      val comp_f = (a: Int) => a + 7
      val comp_g = (b: Int) => b + 3
      compose_test(comp_f, comp_g)(5)
    }
  }
}
