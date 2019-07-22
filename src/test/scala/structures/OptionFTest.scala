package structures

import org.scalatest._

class OptionFTest extends FunSuite {
  val test_int_option: OptionF[Int] = SomeF(3)
  val test_none: OptionF[Int] = NoneF

  test("flatMap method"){
    assertResult(SomeF("3")){
      test_int_option.flatMap(a => SomeF(a.toString))
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
    assertResult(SomeF("3")){
      test_int_option.map(_.toString)
    }
  }

  test("orElse method"){
    val test_else: OptionF[Int] = SomeF(5)
    assertResult(SomeF(3)) {
      test_int_option.orElse(test_else)
    }

    assertResult(test_else) {
      test_none.orElse(test_else)
    }
  }
}
