package examples

import org.scalatest._
import examples.WCExample._

class WCTest extends FunSuite {
  val test_str: String = "abc bca blabla gggg rrrrr eeee"
  val test_2spc_str: String = " abc bca blabla gggg rrrrr eeee "
  val test_1spc_str: String = " abc bca blabla gggg rrrrr eeee"
  val test_spc1_str: String = "abc bca blabla gggg rrrrr eeee "
  val test_empty_str: String = ""

  test("countWC function"){
    assertResult(6){
      countWC(test_str)
    }

    assertResult(6){
      countWC(test_2spc_str)
    }

    assertResult(6){
      countWC(test_1spc_str)
    }

    assertResult(6){
      countWC(test_spc1_str)
    }

    assertResult(1){
      countWC(test_empty_str)
    }
  }
}
