package functional.structures

import functional.structures.ListF._
import org.scalatest.FunSuite

class ListFTest extends FunSuite {
  val test_str_list: ListF[String] = ListF("uk", "usa", "canada", "ukraine", "germany", "netherlands")
  val test_int_list: ListF[Int] = ListF(1, 2, 3, 4, 5, 6)
  val test_dbl_list: ListF[Double] = ListF(1.4, 2.5, 3.2, 4.6, 6.5, 8.6)
  val test_nil_list: ListF[Nothing] = ListF()

  test("add1 function on list") {
    assertResult(ConFL(2, ConFL(3, ConFL(4, ConFL(5, ConFL(6, ConFL(7, NilFL))))))) {
      add1(test_int_list)
    }

    assertResult(NilFL) {
      add1(test_nil_list)
    }
  }

  test("append function on list") {
    assertResult(ConFL(1, ConFL(2,ConFL(3,ConFL(4,ConFL(5,ConFL(6, ConFL(7, ConFL(8, ConFL(9, NilFL)))))))))) {
      append(test_int_list, ListF(7, 8, 9))
    }

    assertResult(ConFL(7, ConFL(8, ConFL(9, NilFL)))){
      append(test_nil_list, ListF(7, 8, 9))
    }
  }

  test("appendRight function on list") {
    assertResult(ConFL(1, ConFL(2,ConFL(3,ConFL(4,ConFL(5,ConFL(6, ConFL(7, ConFL(8, ConFL(9, NilFL)))))))))) {
      appendRight(test_int_list, ListF(7, 8, 9))
    }

    assertResult(ConFL(7, ConFL(8, ConFL(9, NilFL)))){
      appendRight(test_nil_list, ListF(7, 8, 9))
    }
  }

  test("drop function on list") {
    assertResult(ConFL(3, ConFL(4,ConFL(5,ConFL(6,NilFL))))){
      drop(test_int_list, 2)
    }

    assertResult(NilFL){
      drop(test_nil_list, 2)
    }
  }

  test("dropWhile function on list with valid func") {
    assertResult(ConFL(4, ConFL(5, ConFL(6, NilFL)))){
      dropWhile(test_int_list)(x => x <= 3)
    }

    assertResult(NilFL){
      dropWhile(test_int_list)(x => x <= 300)
    }

    assertResult(NilFL){
      dropWhile(test_nil_list: ListF[Int])(x => x <= 300)
    }
  }

  test("filter function") {
    val test_f: Int => Boolean = (x: Int) => x % 2 == 0
    assertResult(ConFL(2, ConFL(4, ConFL(6, NilFL)))) {
      filter(test_int_list)(test_f)
    }

    assertResult(NilFL) {
      filter(test_nil_list: ListF[Int])(test_f)
    }
  }

  test("filterLeft function") {
    val test_f: Int => Boolean = (x: Int) => x % 2 == 0
    assertResult(ConFL(2, ConFL(4, ConFL(6, NilFL)))) {
      filterRight(test_int_list)(test_f)
    }

    assertResult(NilFL) {
      filterRight(test_nil_list: ListF[Int])(test_f)
    }
  }

  test("filterMap function") {
    val test_f: Int => Boolean = (x: Int) => x % 2 == 0
    assertResult(ConFL(2, ConFL(4, ConFL(6, NilFL)))) {
      filterMap(test_int_list)(test_f)
    }

    assertResult(NilFL) {
      filterMap(test_nil_list: ListF[Int])(test_f)
    }
  }

  test("flatMap function") {
    val test_f: Int => ListF[Int] = (a: Int) => ListF(a, a, a)

    assertResult(
      ConFL(1, ConFL(1, ConFL(1,
          ConFL(2, ConFL(2, ConFL(2,
              ConFL(3, ConFL(3, ConFL(3,
                  ConFL(4, ConFL(4, ConFL(4,
                       ConFL(5, ConFL(5, ConFL(5,
                           ConFL(6, ConFL(6, ConFL(6, NilFL))))))))))))))))))) {
      flatMap(test_int_list)(test_f)
    }

    assertResult(NilFL) {
      flatMap(test_nil_list)(test_f)
    }
  }

  test("flatten function on list") {
    assertResult(ConFL(1, ConFL(2, ConFL(3, NilFL)))) {
      val test_list: ListF[ListF[Int]] = ListF(ListF(1, 2, 3))
      flatten(test_list)
    }

    assertResult(NilFL) {
      val test_list: ListF[ListF[Nothing]] = ListF(ListF())
      flatten(test_list)
    }
  }

  test("init function") {
    assertResult(ConFL(1, ConFL(2,ConFL(3,ConFL(4,ConFL(5, NilFL)))))) {
      init(test_int_list)
    }

    assertResult(NilFL){
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
    assertResult(ConFL("1.4", ConFL("2.5", ConFL("3.2", ConFL("4.6", ConFL("6.5", ConFL("8.6", NilFL))))))) {
      mapF(test_dbl_list)(x => x.toString)
    }

    assertResult(NilFL) {
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
    assertResult(ConFL(6, ConFL(5,ConFL(4,ConFL(3,ConFL(2,ConFL(1, NilFL))))))) {
      reverse(test_int_list)
    }

    assertResult(NilFL){
      reverse(test_nil_list)
    }
  }

  test("setHead function on list") {
    assertResult(ConFL(43, ConFL(2,ConFL(3,ConFL(4,ConFL(5,ConFL(6,NilFL))))))){
      setHead(43, test_int_list)
    }

    assertResult(ConFL(3, NilFL)){
      setHead(3, test_nil_list)
    }
  }

  test("stringify function on double list") {
    assertResult(ConFL("1.4",
                       ConFL("2.5",
                             ConFL("3.2",
                                   ConFL("4.6",
                                         ConFL("6.5",
                                               ConFL("8.6", NilFL))))))) {
      stringify(test_dbl_list)
    }

    assertResult(NilFL) {
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
    assertResult(ConFL(2,ConFL(3,ConFL(4,ConFL(5,ConFL(6,NilFL))))) ){
      tail(test_int_list)
    }

    assertResult(NilFL){
      tail(test_nil_list)
    }
  }

  test("zipWith function") {
    assertResult(ConFL(3, ConFL(5, ConFL(7, ConFL(9, ConFL(11, ConFL(13, NilFL))))))) {
      val new_l: ListF[Int] = add1(test_int_list)
      zipWith(test_int_list, new_l)((x, y) => x + y)
    }
    assertResult(NilFL) {
      zipWith(test_nil_list: ListF[Int], test_nil_list: ListF[Int])((x, y) => x + y)
    }
  }
}
