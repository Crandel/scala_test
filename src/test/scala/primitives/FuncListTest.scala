package primitives

import primitives.FuncList._
import org.scalatest._

class FuncListTest extends FunSuite {
  val test_str_list = FuncList("uk", "usa", "canada", "ukraine", "germany", "netherlands")
  val test_int_list = FuncList(1, 2, 3, 4, 5, 6)
  val test_dbl_list = FuncList(1.4, 2.5, 3.2, 4.6, 6.5, 8.6)
  val test_nil_list = FuncList()

  test("append function on int list") {
    assertResult(Cons(1, Cons(2,Cons(3,Cons(4,Cons(5,Cons(6, Cons(7, Cons(8, Cons(9, Nil)))))))))) {
      append(test_int_list, FuncList(7, 8, 9))
    }
  }

  test("append function on nil list") {
    assertResult(Cons(7, Cons(8, Cons(9, Nil)))){
      append(test_nil_list, FuncList(7, 8, 9))
    }
  }

  test("appendLeft function on int list") {
    assertResult(Cons(1, Cons(2,Cons(3,Cons(4,Cons(5,Cons(6, Cons(7, Cons(8, Cons(9, Nil)))))))))) {
      appendLeft(test_int_list, FuncList(7, 8, 9))
    }
  }

  test("appendLeft function on nil list") {
    assertResult(Cons(7, Cons(8, Cons(9, Nil)))){
      appendLeft(test_nil_list, FuncList(7, 8, 9))
    }
  }

  test("drop function on int list") {
    assertResult(Cons(3, Cons(4,Cons(5,Cons(6,Nil))))){
      drop(test_int_list, 2)
    }
  }

  test("drop function on nil list") {
    assertResult(Nil){
      drop(test_nil_list, 2)
    }
  }

  test("dropWhile function on int list with valid func") {
    assertResult(Cons(4, Cons(5, Cons(6, Nil)))){
      dropWhile(test_int_list)(x => x <= 3)
    }
  }

  test("dropWhile function on int list with invalid func") {
    assertResult(Nil){
      dropWhile(test_int_list)(x => x <= 300)
    }
  }

  test("dropWhile function on nil list") {
    assertResult(Nil){
      dropWhile(test_nil_list: FuncList[Int])(x => x <= 300)
    }
  }

  test("flatten function on list") {
    assertResult(Cons(1, Cons(2, Cons(3, Nil)))) {
      val test_list = FuncList(FuncList(1, 2, 3))
      flatten(test_list)
    }

    assertResult(Nil) {
      val test_list = FuncList(FuncList())
      flatten(test_list)
    }
  }
  test("init function") {
    assertResult(Cons(1, Cons(2,Cons(3,Cons(4,Cons(5, Nil)))))) {
      init(test_int_list)
    }

    assertResult(Nil){
      init(test_nil_list)
    }
  }

  test("length of list"){
    assertResult(6) {
      length(test_str_list)
    }

    assertResult(0) {
      length(test_nil_list)
    }
  }

  test("lengthLeft of str list"){
    assertResult(6) {
      lengthLeft(test_str_list)
    }
  }

  test("lengthLeft of nil list"){
    assertResult(0) {
      lengthLeft(test_nil_list)
    }
  }

  test("lengthRight of str list"){
    assertResult(6) {
      lengthRight(test_str_list)
    }
  }

  test("lengthRight of nil list"){
    assertResult(0) {
      lengthRight(test_nil_list)
    }
  }

  test("lengthLeftRight of str list"){
    assertResult(6) {
      lengthLeftRight(test_str_list)
    }
  }

  test("lengthLeftRight of nil list"){
    assertResult(0) {
      lengthLeftRight(test_nil_list)
    }
  }

  test("lengthRightLeft of str list"){
    assertResult(6) {
      lengthRightLeft(test_str_list)
    }
  }

  test("lengthRightLeft of nil list"){
    assertResult(0) {
      lengthRightLeft(test_nil_list)
    }
  }

  test("product of double list"){
    assertResult(2879.968){
      product(test_dbl_list)
    }
  }

  test("product of nil list"){
    assertResult(1.0){
      product(test_nil_list)
    }
  }

  test("productLeft of double list"){
    assertResult(2879.968){
      productLeft(test_dbl_list)
    }
  }

  test("productLeft of nil list"){
    assertResult(1.0){
      productLeft(test_nil_list)
    }
  }

  test("productRight of double list"){
    assertResult(2879.968){
      productRight(test_dbl_list)
    }
  }

  test("productRight of nil list"){
    assertResult(1.0){
      productRight(test_nil_list)
    }
  }

  test("productLeftRight of double list"){
    assertResult(2879.968){
      productLeftRight(test_dbl_list)
    }
  }

  test("productLeftRight of nil list"){
    assertResult(1.0){
      productLeftRight(test_nil_list)
    }
  }

  test("productRightLeft of double list"){
    assertResult(2879.968){
      productRightLeft(test_dbl_list)
    }
  }

  test("productRightLeft of nil list"){
    assertResult(1.0){
      productRightLeft(test_nil_list)
    }
  }

  test("reverse function on int list") {
    assertResult(Cons(6, Cons(5,Cons(4,Cons(3,Cons(2,Cons(1, Nil))))))) {
      reverse(test_int_list)
    }
  }

  test("reverse function on nil list") {
    assertResult(Nil){
      reverse(test_nil_list)
    }
  }

  test("setHead function on int list") {
    assertResult(Cons(43, Cons(2,Cons(3,Cons(4,Cons(5,Cons(6,Nil))))))){
      setHead(43, test_int_list)
    }
  }

  test("setHead function on nil list") {
    assertResult(Cons(3, Nil)){
      setHead(3, test_nil_list)
    }
  }

  test("sum of int list"){
    assertResult(21){
      sum(test_int_list)
    }
  }

  test("sum of nil list"){
    assertResult(0){
      sum(test_nil_list)
    }
  }

  test("sumLeft of int list"){
    assertResult(21){
      sumLeft(test_int_list)
    }
  }

  test("sumLeft of nil list"){
    assertResult(0){
      sumLeft(test_nil_list)
    }
  }

  test("sumRight of int list"){
    assertResult(21){
      sumRight(test_int_list)
    }
  }

  test("sumRight of nil list"){
    assertResult(0){
      sumRight(test_nil_list)
    }
  }

  test("tail function on int list") {
    assertResult(Cons(2,Cons(3,Cons(4,Cons(5,Cons(6,Nil))))) ){
      tail(test_int_list)
    }
  }

  test("tail function on nil list") {
    assertResult(Nil){
      tail(test_nil_list)
    }
  }
}
