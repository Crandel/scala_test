package structures

import org.scalatest._
import structures.monoids._

class MonoidTest extends FunSuite {
  val test_str_list: List[String] = List("aaa", "bbb", "ccc")
  val test_int_list: List[Int] = List(3, 4, 6, 8)
  val test_empty_list: List[Nothing] = List()
  val test_int_seq: IndexedSeq[Int] = IndexedSeq(1, 3, 5, 7, 9)
  val test_empty_seq: IndexedSeq[Nothing] = IndexedSeq()


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

  test("concatenate function"){
    assertResult(576){
      concatenate(test_int_list, IntMultMonoid)
    }

    assertResult(1){
      concatenate(test_empty_list, IntMultMonoid)
    }
  }

  test("foldMap function"){
    assertResult("3468"){
      foldMap(test_int_list, StringMonoid)(_.toString)
    }

    assertResult(""){
      foldMap(test_empty_list: List[Int], StringMonoid)(_.toString)
    }
  }

  test("foldMapV function"){
    assertResult("13579"){
      foldMapV(test_int_seq, StringMonoid)(_.toString)
    }

    assertResult(""){
      foldMapV(test_empty_seq: IndexedSeq[Int], StringMonoid)(_.toString)
    }
  }
}
