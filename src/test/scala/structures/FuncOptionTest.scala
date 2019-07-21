package structures

import org.scalatest._

class FuncOptionTest extends FunSuite{
  val test_int_option: FuncOption[Int] = Some(3)
  test("map method") {
    assertResult(Some("3")) {
      test_int_option.map(_.toString)
    }
  }

  test("flatMap method") {
    assertResult(Some("3")) {
      test_int_option.flatMap(a => Some(a.toString))
    }
  }
}
