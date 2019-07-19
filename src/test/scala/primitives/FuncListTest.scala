package primitives

import org.scalatest._

class FuncListTest extends FunSuite {
  val test_str_list = FuncList("uk", "usa", "canada", "ukraine", "germany", "netherlands")
  val test_int_list = FuncList(1, 2, 3, 4, 5, 6)
  val test_dbl_list = FuncList(1.4, 2.5, 3.2, 4.6, 6.5, 8.6)
  val test_nil_list = FuncList()

  test("Test original lenght of str list"){
    assertResult(6) {
      FuncList.length(test_str_list)
    }
  }

  test("Test original lenght of nil list"){
    assertResult(0) {
      FuncList.length(test_nil_list)
    }
  }

  test("Test lenghtLeft of str list"){
    assertResult(6) {
      FuncList.lengthLeft(test_str_list)
    }
  }

  test("Test lenghtLeft of nil list"){
    assertResult(0) {
      FuncList.lengthLeft(test_nil_list)
    }
  }

  test("Test lenghtRight of str list"){
    assertResult(6) {
      FuncList.lengthRight(test_str_list)
    }
  }

  test("Test lenghtRight of nil list"){
    assertResult(0) {
      FuncList.lengthRight(test_nil_list)
    }
  }

  test("Test lenghtLeftRight of str list"){
    assertResult(6) {
      FuncList.lengthLeftRight(test_str_list)
    }
  }

  test("Test lenghtLeftRight of nil list"){
    assertResult(0) {
      FuncList.lengthLeftRight(test_nil_list)
    }
  }

  test("Test lenghtRightLeft of str list"){
    assertResult(6) {
      FuncList.lengthRightLeft(test_str_list)
    }
  }

  test("Test lenghtRightLeft of nil list"){
    assertResult(0) {
      FuncList.lengthRightLeft(test_nil_list)
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

  test("Test original product of double list"){
    assertResult(2879.968){
      FuncList.product(test_dbl_list)
    }
  }

  test("Test original product of nil list"){
    assertResult(1.0){
      FuncList.product(test_nil_list)
    }
  }

  test("Test productLeft of double list"){
    assertResult(2879.968){
      FuncList.productLeft(test_dbl_list)
    }
  }

  test("Test productLeft of nil list"){
    assertResult(1.0){
      FuncList.productLeft(test_nil_list)
    }
  }

  test("Test productRight of double list"){
    assertResult(2879.968){
      FuncList.productRight(test_dbl_list)
    }
  }

  test("Test productRight of nil list"){
    assertResult(1.0){
      FuncList.productRight(test_nil_list)
    }
  }

  test("Test tail function on int list") {
    assertResult(Some(Cons(2,Cons(3,Cons(4,Cons(5,Cons(6,Nil))))) )){
      FuncList.tail(test_int_list)
    }
  }

  test("Test tail function on nil list") {
    assertResult(None){
      FuncList.tail(test_nil_list)
    }
  }

  test("Test setHead function on int list") {
    assertResult(Cons(43, Cons(2,Cons(3,Cons(4,Cons(5,Cons(6,Nil))))))){
      FuncList.setHead(43, test_int_list)
    }
  }

  test("Test setHead function on nil list") {
    assertResult(Cons(3, Nil)){
      FuncList.setHead(3, test_nil_list)
    }
  }

  test("Test drop function on int list") {
    assertResult(Some(Cons(4,Cons(5,Cons(6,Nil))))){
      FuncList.drop(test_int_list, 2)
    }
  }

  test("Test drop function on nil list") {
    assertResult(None){
      FuncList.drop(test_nil_list, 2)
    }
  }

  test("Test dropWhile function on int list with valid func") {
    assertResult(Some(Cons(4, Cons(5, Cons(6, Nil))))){
      FuncList.dropWhile(test_int_list)(x => x <= 3)
    }
  }

  test("Test dropWhile function on int list with invalid func") {
    assertResult(None){
      FuncList.dropWhile(test_int_list)(x => x <= 300)
    }
  }

  test("Test dropWhile function on nil list") {
    assertResult(None){
      FuncList.dropWhile(test_nil_list: FuncList[Int])(x => x <= 300)
    }
  }

  test("Test append function on int list") {
    assertResult(Cons(1, Cons(2,Cons(3,Cons(4,Cons(5,Cons(6, Cons(7, Cons(8, Cons(9, Nil)))))))))) {
      FuncList.append(test_int_list, FuncList(7, 8, 9))
    }
  }

  test("Test append function on nil list") {
    assertResult(Cons(7, Cons(8, Cons(9, Nil)))){
      FuncList.append(test_nil_list, FuncList(7, 8, 9))
    }
  }

  test("Test init function on int list") {
    assertResult(Cons(1, Cons(2,Cons(3,Cons(4,Cons(5, Nil)))))) {
      FuncList.init(test_int_list)
    }
  }

  test("Test init function on nil list") {
    assertResult(Nil){
      FuncList.init(test_nil_list)
    }
  }
}
