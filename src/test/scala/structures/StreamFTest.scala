package structures

import org.scalatest._

class StreamFTest extends FunSuite {
  val test_int_stream: StreamF[Int] = StreamF(5, 4, 3, 2, 1)
  val test_empty_stream: StreamF[Nothing] = StreamF()

  test("headOption method") {
    assertResult(Some(5)) {
      test_int_stream.headOption
    }

    assertResult(None) {
      test_empty_stream.headOption
    }
  }

  test("toList method") {
    assertResult(5 :: 4:: 3 :: 2 :: 1 :: Nil) {
      test_int_stream.toList
    }

    assertResult(Nil) {
      test_empty_stream.toList
    }
  }
}
