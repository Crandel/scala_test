package primitives

sealed trait FuncList[+A]
case object Nil extends FuncList[Nothing]
case class Cons[+A] (head: A, tail:FuncList[A]) extends FuncList[A]

object FuncList {
  def length[A](l: FuncList[A]): Int =
    l match {
      case Nil => 0
      case Cons(x, xs) => 1 + length(xs)
    }

  def lengthLeft[A](l: FuncList[A]): Int =
    foldLeft(l, 0)((x, y) => 1 + x)

  def lengthRight[A](l: FuncList[A]): Int =
    foldRight(l, 0)((x, y) => 1 + y)

  def foldRight[A, B](as: FuncList[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def foldLeft[A,B](as: FuncList[A], z: B)(f: (B, A) => B): B = {
    @annotation.tailrec
    def iter(as: FuncList[A], acc: B): B =
      as match {
        case Nil => acc
        case Cons(x, xs) => iter(xs, f(acc, x))
      }
    iter(as, z)
  }

  def sum(ints: FuncList[Int]): Int =
    ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

  def sumLeft(ints: FuncList[Int]): Int =
    foldLeft(ints, 0)((x, y) => x + y)

  def sumRight(ints: FuncList[Int]): Int =
    foldRight(ints, 0)((x, y) => x + y)

  def product(ds: FuncList[Double]): Double =
    ds match {
      case Nil => 1.0
      case Cons(x, xs) => x * product(xs)
    }

  def productLeft(ds: FuncList[Double]): Double =
    foldLeft(ds, 1.0)(_ * _)

  def productRight(ds: FuncList[Double]): Double =
    foldRight(ds, 1.0)(_ * _)

  def apply[A](as: A*): FuncList[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def tail[A](al: FuncList[A]): Option[FuncList[A]] =
    al match {
      case Nil => None
      case Cons(x, xs) => Some(xs)
    }

  def setHead[A](h: A, al: FuncList[A]): FuncList[A] =
    al match {
      case Nil => FuncList(h)
      case Cons(x, xs) => Cons(h, xs)
    }

  def drop[A](l: FuncList[A], n: Int): Option[FuncList[A]] =
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

  def dropWhile[A](l: FuncList[A])(f: A => Boolean): Option[FuncList[A]] =
    l match {
      case Nil => None
      case Cons(x, xs) => {
        if (f(x))
          dropWhile(xs)(f)
        else
          Some(l)
      }
    }

  def append[A](a1: FuncList[A], a2: FuncList[A]): FuncList[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def init[A](l: FuncList[A]): FuncList[A] =
    l match {
      case Nil => Nil
      case Cons(x, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }

  def test() = {
    val x = FuncList(1,2,3,4,5)
    println(s"length of list x is ${length(x)}")
    val y = FuncList(1.0, 2.4, 3.5, 4.1, 5.5)
    val m_x = x match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }
    println(s"Result of pattern match is ${m_x}")
    val sum_l = sum(x)
    val product_l = product(y)
    println(s"Sum is ${sum_l} and product is ${product_l}")
    val empty_l = FuncList()
    val empty_tail = tail(empty_l)
    println(s"Test of empty list tail ${empty_tail.toString}")
    val test_l = FuncList(1, 3, 6, 7, 89, 65)
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

    val drop_w_l = dropWhile(test_l)(x => x <= 7)
    val drop_w_n = dropWhile(test_l)(x => x >= 700)
    println(s"Drop till elem <= 7: ${drop_w_l}")
    println(s"Drop till elem >= 700: ${drop_w_n}")
    val init_e_l = init(empty_l)
    val init_l = init(test_l)
    println(s"Init func is ${init_e_l}")
    println(s"Init func is ${init_l}")
  }
}
