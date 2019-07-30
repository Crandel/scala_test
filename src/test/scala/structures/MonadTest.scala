package structures

import org.scalatest.FunSuite
import structures.MonadsExample.listFunctor

class MonadTest extends FunSuite {
  val test_int_list1 = List(1, 2, 3, 4, 5, 6)
  val test_int_list2 = List(3, 4, 6, 8, 12, 31)

  test("fmap Functor method"){
    assertResult(List("1", "2", "3", "4", "5", "6")){
      listFunctor.fmap(test_int_list1)(_.toString)
    }
  }
}
