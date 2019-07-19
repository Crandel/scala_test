import functional.Currying
import org.scalatest._


class CurryingTest extends FunSuite {

  test("Test add function"){
    val add5 = Currying.add(5)

    assertResult(11) {
      Currying.add(5)(6)
    }

    assertResult(0) {
      Currying.add(0)(0)
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
      val partOne = Currying.addPart(5) _
      partOne(5)
    }

    assertResult(12) {
      val partTwo = Currying.addPart(5)(7)
      partTwo
    }
  }

  test("Test average") {
    assertResult(67.5) {
      Currying.average(90, 100, 110)(30, 40, 70)(10, 40, 85)
    }
  }
}
