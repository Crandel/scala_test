package structures

import org.scalatest._

class StreamFTest extends FunSuite {
  val test_int_stream = StreamF(5, 4, 3, 2, 1)
  test("headOption method") {
    assertResult(Some(5)) {
      test_int_stream.headOption
    }
  }
}
