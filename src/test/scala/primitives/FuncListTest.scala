package primitives

import primitives.FuncList._
import org.scalatest._

class FuncListTest extends FunSuite {
  val test_str_list: FuncList[String] = FuncList("uk", "usa", "canada", "ukraine", "germany", "netherlands")
  val test_int_list: FuncList[Int] = FuncList(1, 2, 3, 4, 5, 6)
  val test_dbl_list: FuncList[Double] = FuncList(1.4, 2.5, 3.2, 4.6, 6.5, 8.6)
  val test_nil_list: FuncList[Nothing] = FuncList()

  test("add1 function on list") {
    assertResult(Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Cons(7, Nil))))))) {
      add1(test_int_list)
    }

    assertResult(Nil) {
      add1(test_nil_list)
    }
  }

  test("append function on list") {
    assertResult(Cons(1, Cons(2,Cons(3,Cons(4,Cons(5,Cons(6, Cons(7, Cons(8, Cons(9, Nil)))))))))) {
      append(test_int_list, FuncList(7, 8, 9))
    }

    assertResult(Cons(7, Cons(8, Cons(9, Nil)))){
      append(test_nil_list, FuncList(7, 8, 9))
    }
  }

  test("appendLeft function on list") {
    assertResult(Cons(1, Cons(2,Cons(3,Cons(4,Cons(5,Cons(6, Cons(7, Cons(8, Cons(9, Nil)))))))))) {
      appendLeft(test_int_list, FuncList(7, 8, 9))
    }

    assertResult(Cons(7, Cons(8, Cons(9, Nil)))){
      appendLeft(test_nil_list, FuncList(7, 8, 9))
    }
  }

  test("drop function on list") {
    assertResult(Cons(3, Cons(4,Cons(5,Cons(6,Nil))))){
      drop(test_int_list, 2)
    }

    assertResult(Nil){
      drop(test_nil_list, 2)
    }
  }

  test("dropWhile function on list with valid func") {
    assertResult(Cons(4, Cons(5, Cons(6, Nil)))){
      dropWhile(test_int_list)(x => x <= 3)
    }

    assertResult(Nil){
      dropWhile(test_int_list)(x => x <= 300)
    }

    assertResult(Nil){
      dropWhile(test_nil_list: FuncList[Int])(x => x <= 300)
    }
  }

  test("flatten function on list") {
    assertResult(Cons(1, Cons(2, Cons(3, Nil)))) {
      val test_list: FuncList[FuncList[Int]] = FuncList(FuncList(1, 2, 3))
      flatten(test_list)
    }

    assertResult(Nil) {
      val test_list: FuncList[FuncList[Nothing]] = FuncList(FuncList())
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

  test("lengthLeft of list"){
    assertResult(6) {
      lengthLeft(test_str_list)
    }

    assertResult(0) {
      lengthLeft(test_nil_list)
    }
  }

  test("lengthRight of list"){
    assertResult(6) {
      lengthRight(test_str_list)
    }

    assertResult(0) {
      lengthRight(test_nil_list)
    }
  }

  test("lengthLeftRight of list"){
    assertResult(6) {
      lengthLeftRight(test_str_list)
    }

    assertResult(0) {
      lengthLeftRight(test_nil_list)
    }
  }

  test("lengthRightLeft of list"){
    assertResult(6) {
      lengthRightLeft(test_str_list)
    }

    assertResult(0) {
      lengthRightLeft(test_nil_list)
    }
  }

  test("mapF") {
    assertResult(Cons("1.4", Cons("2.5", Cons("3.2", Cons("4.6", Cons("6.5", Cons("8.6", Nil))))))) {
      mapF(test_dbl_list)(x => x.toString)
    }

    assertResult(Nil) {
      mapF(test_nil_list)(x => x.toString)
    }
  }

  test("product of list"){
    assertResult(2879.968){
      product(test_dbl_list)
    }

    assertResult(1.0){
      product(test_nil_list)
    }
  }

  test("productLeft of list"){
    assertResult(2879.968){
      productLeft(test_dbl_list)
    }

    assertResult(1.0){
      productLeft(test_nil_list)
    }
  }

  test("productRight of list"){
    assertResult(2879.968){
      productRight(test_dbl_list)
    }

    assertResult(1.0){
      productRight(test_nil_list)
    }
  }

  test("productLeftRight of list"){
    assertResult(2879.968){
      productLeftRight(test_dbl_list)
    }

    assertResult(1.0){
      productLeftRight(test_nil_list)
    }
  }

  test("productRightLeft of list"){
    assertResult(2879.968){
      productRightLeft(test_dbl_list)
    }

    assertResult(1.0){
      productRightLeft(test_nil_list)
    }
  }

  test("reverse function on list") {
    assertResult(Cons(6, Cons(5,Cons(4,Cons(3,Cons(2,Cons(1, Nil))))))) {
      reverse(test_int_list)
    }

    assertResult(Nil){
      reverse(test_nil_list)
    }
  }

  test("setHead function on list") {
    assertResult(Cons(43, Cons(2,Cons(3,Cons(4,Cons(5,Cons(6,Nil))))))){
      setHead(43, test_int_list)
    }

    assertResult(Cons(3, Nil)){
      setHead(3, test_nil_list)
    }
  }

  test("stringify function on double list") {
    assertResult(Cons("1.4", Cons("2.5", Cons("3.2", Cons("4.6", Cons("6.5", Cons("8.6", Nil))))))) {
      stringify(test_dbl_list)
    }

    assertResult(Nil) {
      stringify(test_nil_list)
    }
  }

  test("sum of list"){
    assertResult(21){
      sum(test_int_list)
    }

    assertResult(0){
      sum(test_nil_list)
    }
  }

  test("sumLeft of list"){
    assertResult(21){
      sumLeft(test_int_list)
    }

    assertResult(0){
      sumLeft(test_nil_list)
    }
  }

  test("sumRight of list"){
    assertResult(21){
      sumRight(test_int_list)
    }

    assertResult(0){
      sumRight(test_nil_list)
    }
  }

  test("tail function on list") {
    assertResult(Cons(2,Cons(3,Cons(4,Cons(5,Cons(6,Nil))))) ){
      tail(test_int_list)
    }

    assertResult(Nil){
      tail(test_nil_list)
    }
  }
}
