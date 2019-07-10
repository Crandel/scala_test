package primitives

sealed trait TestList[+A]
case object Nil extends TestList[Nothing]
case class Cons[+A] (head: A, tail:TestList[A]) extends TestList[A]

object TestList {
  def sum(ints: TestList[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: TestList[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): TestList[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def test() = {
    val x = TestList(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }
    println(s"Result of pattern match is ${x}")
    val empty_l = TestList()
    val empty_tail = tail(empty_l)
    println(s"Test of empty list tail ${empty_tail.toString}")
    val test_l = TestList(1, 3, 6)
    val test_tail = tail(test_l)
    println(s"Test of tail ${test_tail}")
  }

  def tail[A](al: TestList[A]): Option[TestList[A]] = al match {
    case Nil => None
    case Cons(x, xs) => Some(xs)
  }
}
