package structures

import org.scalatest._
import structures.FuncTree._

class FuncTreeTest extends FunSuite {
  val test_int_tree: FuncTree[Int] = Branch(Branch(Leaf(3), Leaf(5)), Branch(Leaf(4), Leaf(6)))
  val test_str_tree: FuncTree[String] = Branch(Branch(Leaf("abc"), Leaf("defg")), Leaf("ffffff"))
  test("sizeT function"){
    assertResult(7){
      sizeT(test_int_tree)
    }

    assertResult(5){
      sizeT(test_str_tree)
    }
  }

  test("maximumT function"){
    assertResult(6){
      maximumT(test_int_tree)
    }
  }

  test("depthT function"){
    assertResult(3){
      depthT(test_int_tree)
    }

    assertResult(3){
      depthT(test_str_tree)
    }
  }

  test("mapT function"){
    assertResult(Branch(Branch(Leaf("abc111"), Leaf("defg111")), Leaf("ffffff111"))){
      mapT(test_str_tree)(x => x + "111")
    }
  }
}
