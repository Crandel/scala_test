package structures

import org.scalatest._
import structures.FuncTree._

class FuncTreeTest extends FunSuite {
  val test_int_tree = Branch(Branch(Leaf(3), Leaf(5)), Branch(Leaf(4), Leaf(6)))
  val test_str_tree = Branch(Branch(Leaf("abc"), Leaf("defg")), Branch(Leaf("hijklm"), Leaf("nopqrstuvw")))
  test("sizeT function") {
    assertResult(7) {
      sizeT(test_int_tree)
    }

    assertResult(7) {
      sizeT(test_str_tree)
    }
  }
}
