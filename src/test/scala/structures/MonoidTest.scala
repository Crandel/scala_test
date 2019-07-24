package structures

import org.scalatest._
import structures.monoids._

class MonoidTest extends FunSuite {
  val test_str_list: List[String] = List("aaa", "bbb", "ccc")
  val test_int_list: List[Int] = List(3, 4, 6, 8)
  val test_empty_list: List[Nothing] = List()

  test("concatenate function"){
    assertResult(576){
      concatenate(test_int_list, IntMultMonoid)
    }

    assertResult(1){
      concatenate(test_empty_list, IntMultMonoid)
    }
  }

  test("IntAddMonoid"){
    assertResult(21){
      test_int_list.foldLeft(IntAddMonoid.zero)(IntAddMonoid.op)
    }

    assertResult(0){
      test_empty_list.foldLeft(IntAddMonoid.zero)(IntAddMonoid.op)
    }
  }

  test("IntMultMonoid"){
    assertResult(576){
      test_int_list.foldLeft(IntMultMonoid.zero)(IntMultMonoid.op)
    }

    assertResult(1){
      test_empty_list.foldLeft(IntMultMonoid.zero)(IntMultMonoid.op)
    }
  }

  test("StringMonoid"){
    val test_str1: String = "Hello "
    val test_str2: String = "World"

    assertResult("Hello World"){
      StringMonoid.op(test_str1, test_str2)
    }

    assertResult("aaabbbccc"){
      test_str_list.foldLeft(StringMonoid.zero)(StringMonoid.op)
    }
  }
}
