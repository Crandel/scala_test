package primitives

sealed trait TestList[+A]
case object Nil extends TestList[Nothing]
case class Cons[+A] (head: A, tail:TestList[A]) extends TestList[A]

object TestList {
  def length[A](l: TestList[A]): Int =
    l match {
      case Nil => 0
      case Cons(x, xs) => 1 + length(xs)
    }

  def sum(ints: TestList[Int]): Int =
    ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

  def product(ds: TestList[Double]): Double =
    ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }

  def apply[A](as: A*): TestList[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def tail[A](al: TestList[A]): Option[TestList[A]] =
    al match {
      case Nil => None
      case Cons(x, xs) => Some(xs)
    }

  def setHead[A](h: A, al: TestList[A]): TestList[A] =
    al match {
      case Nil => TestList(h)
      case Cons(x, xs) => Cons(h, xs)
    }

  def drop[A](l: TestList[A], n: Int): Option[TestList[A]] =
    l match {
      case Nil => None
      case Cons(x, xs) => {
        if (n >= length(l) - 1)
          None
        else if (n == 0)
          Some(xs)
        else
          drop(xs, n - 1)
      }
    }

  def dropWhile[A](l: TestList[A], f: A => Boolean): Option[TestList[A]] =
    l match {
      case Nil => None
      case Cons(x, xs) => {
        if (f(x))
          dropWhile(xs, f)
        else
          Some(l)
      }
    }

  def append[A](a1: TestList[A], a2: TestList[A]): TestList[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def init[A](l: TestList[A]): TestList[A] =
    l match {
      case Nil => Nil
      case Cons(x, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
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
    val test_l = TestList(1, 3, 6, 7, 89, 65)
    val test_tail = tail(test_l)
    println(s"Test of tail ${test_tail}")
    val new_h = 5
    val new_empty_l = setHead(new_h, empty_l)
    val new_l = setHead(new_h, test_l)
    println(s"Test setHead function empty list: ${new_empty_l}")
    println(s"Test setHead function: ${new_l}")

    val drop_l = drop(test_l, 2)
    val drop_none = drop(test_l, 10)
    println(s"Test drop 2 first elements: ${drop_l}")
    println(s"Test drop 10 first elements: ${drop_none}")

    val f1 = (x: Int) => x <= 7
    val f2 = (x: Int) => x >= 700
    val drop_w_l = dropWhile(test_l, f1)
    val drop_w_n = dropWhile(test_l, f2)
    println(s"Drop till elem <= 7: ${drop_w_l}")
    println(s"Drop till elem >= 700: ${drop_w_n}")
    val init_l = init(test_l)
    println(s"Init func is ${init_l}")
  }
}
