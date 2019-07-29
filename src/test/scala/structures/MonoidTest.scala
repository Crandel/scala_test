package structures

import org.scalatest._
import structures.Monoids._

class MonoidTest extends FunSuite {
  val test_str_list: List[String] = List("aaa", "bbb", "ccc")
  val test_int_list: List[Int] = List(3, 4, 6, 8)
  val test_empty_list: List[Nothing] = List()
  val test_int_seq: IndexedSeq[Int] = IndexedSeq(1, 3, 5, 7, 9)
  val test_empty_seq: IndexedSeq[Nothing] = IndexedSeq()


  test("intAddMonoid"){
    assertResult(21){
      test_int_list.foldLeft(intAddMonoid.zero)(intAddMonoid.op)
    }

    assertResult(0){
      test_empty_list.foldLeft(intAddMonoid.zero)(intAddMonoid.op)
    }
  }

  test("intMultMonoid"){
    assertResult(576){
      test_int_list.foldLeft(intMultMonoid.zero)(intMultMonoid.op)
    }

    assertResult(1){
      test_empty_list.foldLeft(intMultMonoid.zero)(intMultMonoid.op)
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
      concatenate(test_int_list, intMultMonoid)
    }

    assertResult(1){
      concatenate(test_empty_list, intMultMonoid)
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

  test("foldLeft function"){
    assertResult(21){
      foldLeft(test_int_list)(0)(_ + _)
    }

    assertResult(0){
      foldLeft(test_empty_list)(0)((a: Int, b: Int) => a + b)
    }
  }

  test("foldRight function"){
    assertResult(21){
      foldRight(test_int_list)(0)(_ + _)
    }

    assertResult(0){
      foldRight(test_empty_list)(0)((a: Int, b: Int) => a + b)
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
