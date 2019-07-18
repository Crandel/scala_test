import org.scalatest._
import functional.Currying


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
}
