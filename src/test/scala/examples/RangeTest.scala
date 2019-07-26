package examples

import org.scalatest._
import examples.OrderingExample._

class RangeTest extends FunSuite {
  val test_int_ord_seq: IndexedSeq[Int] = IndexedSeq(1, 3, 5, 6, 7, 9, 11)
  val test_int_unord_seq: IndexedSeq[Int] = IndexedSeq(1, 8, 3, 5, 4, 7, 9, 11)
  val test_empty_seq: IndexedSeq[Nothing] = IndexedSeq()

  test("isOrdered"){
    assertResult(true){
      isOrdered(test_int_ord_seq)
    }

    assertResult(false){
      isOrdered(test_int_unord_seq)
    }

    assertResult(true){
      isOrdered(test_empty_seq)
    }
  }
}
