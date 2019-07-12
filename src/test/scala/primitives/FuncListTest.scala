import org.scalatest._
import primitives.FuncList

class FuncListTest extends FunSuite {
  val test_str_list = FuncList("uk", "usa", "canada", "ukraine", "germany", "netherlands")
  val test_int_list = FuncList(1, 2, 3, 4, 5, 6)
  val test_nil_list = FuncList()

  test("Test lenght of str list"){
    assertResult(6) {
      FuncList.length(test_str_list)
    }
  }

  test("Test lenght of nil list"){
    assertResult(0) {
      FuncList.length(test_nil_list)
    }
  }

  test("Test original sum of int list"){
    assertResult(21){
      FuncList.sum(test_int_list)
    }
  }

  test("Test original sum of nil list"){
    assertResult(0){
      FuncList.sum(test_nil_list)
    }
  }

  test("Test sumLeft of int list"){
    assertResult(21){
      FuncList.sumLeft(test_int_list)
    }
  }

  test("Test sumLeft of nil list"){
    assertResult(0){
      FuncList.sumLeft(test_nil_list)
    }
  }

  test("Test sumRight of int list"){
    assertResult(21){
      FuncList.sumRight(test_int_list)
    }
  }

  test("Test sumRight of nil list"){
    assertResult(0){
      FuncList.sumRight(test_nil_list)
    }
  }
}
