package structures

import org.scalatest._

class FuncOptionTest extends FunSuite {
  val test_int_option: FuncOption[Int] = Some(3)
  val test_none: FuncOption[Int] = None

  test("flatMap method"){
    assertResult(Some("3")){
      test_int_option.flatMap(a => Some(a.toString))
    }
  }

  test("getOrElse method"){
    assertResult(3) {
      test_int_option.getOrElse(5)
    }

    assertResult(5) {
      test_none.getOrElse(5)
    }
  }

  test("map method"){
    assertResult(Some("3")){
      test_int_option.map(_.toString)
    }
  }

  test("orElse method"){
    val test_else: FuncOption[Int] = Some(5)
    assertResult(Some(3)) {
      test_int_option.orElse(test_else)
    }

    assertResult(test_else) {
      test_none.orElse(test_else)
    }
  }
}
