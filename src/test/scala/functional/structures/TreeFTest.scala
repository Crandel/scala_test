package functional.structures

import org.scalatest._

import functional.structures.TreeF._

class TreeFTest extends FunSuite {
  val test_int_tree: TreeF[Int] = BranchFT(BranchFT(LeafFT(3), LeafFT(5)), BranchFT(LeafFT(4), LeafFT(6)))
  val test_str_tree: TreeF[String] = BranchFT(BranchFT(LeafFT("abc"), LeafFT("defg")), LeafFT("ffffff"))
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
    assertResult(BranchFT(BranchFT(LeafFT("abc111"), LeafFT("defg111")), LeafFT("ffffff111"))){
      mapT(test_str_tree)(x => x + "111")
    }
  }
}
