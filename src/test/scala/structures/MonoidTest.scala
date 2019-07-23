package structures

import org.scalatest._
import structures.monoids._

class MonoidTest extends FunSuite {
  val test_str1: String = "Hello "
  val test_str2: String = "World"
  val test_str_list: List[String] = List("aaa", "bbb", "ccc")

  test("StringMonoid"){
    assertResult("Hello World"){
      StringMonoid.op(test_str1, test_str2)
    }

    assertResult("aaabbbccc"){
      test_str_list.foldLeft(StringMonoid.zero)(StringMonoid.op)
    }
  }
}
