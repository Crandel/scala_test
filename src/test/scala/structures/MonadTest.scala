package structures

import org.scalatest.FunSuite
import structures.MonadsExample.{listFunctor, listMonad}

class MonadTest extends FunSuite {
  val test_int_list1 = List(1, 2, 3)
  val test_int_list2 = List(3, 4, 6)
  val test_str_list1 = List("1", "2", "3")

  test("fmap Functor method") {
    assertResult(test_str_list1){
      listFunctor.fmap(test_int_list1)(_.toString)
    }
  }

  test("lift Monad method") {
    assertResult(List("11", "12", "13", "21", "22", "23", "31", "32", "33")) {
      listMonad.lift(test_str_list1, test_str_list1)((a, b) => a.toString + b)
    }
  }
}
