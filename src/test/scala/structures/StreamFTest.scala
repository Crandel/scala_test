package structures

import org.scalatest._

class StreamFTest extends FunSuite {
  val test_int_stream: StreamF[Int] = StreamF(5, 4, 3, 2, 1)
  val test_empty_stream: StreamF[Nothing] = StreamF()

  test("drop method"){
    assertResult(3 :: 2 :: 1 :: Nil){
      test_int_stream.drop(2).toList
    }

    assertResult(EmptyFS){
      test_empty_stream.drop(5)
    }
  }

  test("exists method"){
    assert(test_int_stream.exists(_ == 5))

    assert(!test_empty_stream.exists(_ == 5))
  }

  test("existsRight method"){
    assert(test_int_stream.existsRight(_ == 5))

    assert(!test_empty_stream.existsRight(_ == 5))
  }

  test("foldRight method"){
    assert(test_int_stream.foldRight(false)((a, b) => a == 5 || b))

    assert(!test_empty_stream.foldRight(false)((a, b) => a == 5 || b))
  }

  test("headOption method"){
    assertResult(Some(5)){
      test_int_stream.headOption
    }

    assertResult(None){
      test_empty_stream.headOption
    }
  }

  test("take method"){
    assertResult(5 :: 4 :: 3 :: Nil){
      test_int_stream.take(3).toList
    }

    assertResult(EmptyFS){
      test_empty_stream.take(5)
    }
  }

  test("takeWhile method"){
    assertResult(5 :: 4 :: 3 :: Nil){
      test_int_stream.takeWhile(a => a >= 3).toList
    }

    assertResult(EmptyFS){
      test_empty_stream.takeWhile(a => a)
    }
  }

  test("toList method"){
    assertResult(5 :: 4 :: 3 :: 2 :: 1 :: Nil){
      test_int_stream.toList
    }

    assertResult(Nil){
      test_empty_stream.toList
    }
  }
}
