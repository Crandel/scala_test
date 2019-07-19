package primitives

sealed trait FuncList[+A]
case object Nil extends FuncList[Nothing]
case class Cons[+A] (head: A, tail:FuncList[A]) extends FuncList[A]

object FuncList {
  def add1(xs: FuncList[Int]): FuncList[Int] =
    xs match {
      case Nil => Nil
      case Cons(x, y) => Cons(x + 1, add1(y))
  }

  def append[A](a1: FuncList[A], a2: FuncList[A]): FuncList[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def appendRight[A](a1: FuncList[A], a2: FuncList[A]): FuncList[A] = {
    foldRight(a1, a2)(Cons(_, _))
  }

  def apply[A](as: A*): FuncList[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  @annotation.tailrec
  def drop[A](l: FuncList[A], n: Int): FuncList[A] =
    n match {
      case 0 => l
      case _ => drop(tail(l), n -1)
    }

  @annotation.tailrec
  def dropWhile[A](l: FuncList[A])(f: A => Boolean): FuncList[A] =
    l match {
      case Cons(x, xs) if f(x) => dropWhile(xs)(f)
      case _ => l
    }

  def filter[A](l: FuncList[A])(f: A => Boolean): FuncList[A] =
    l match {
      case Nil => Nil
      case Cons(x, xs) =>
        if (f(x))
          Cons(x, filter(xs)(f))
        else
          filter(xs)(f)
    }

  def filterRight[A](l: FuncList[A])(f: A => Boolean): FuncList[A] =
    foldRight(l, FuncList(): FuncList[A])((a, acc) => if (f(a)) Cons(a, acc) else acc)

  def filterMap[A](l: FuncList[A])(f: A => Boolean): FuncList[A] =
    flatMap(l)(xs => if (f(xs)) FuncList(xs) else Nil)

  def flatMap[A, B](l: FuncList[A])(f: A=> FuncList[B]): FuncList[B] =
    l match {
      case Nil => Nil
      case Cons(h, t) => append(f(h), flatMap(t)(f))
    }

  def flatten[A](xs: FuncList[FuncList[A]]): FuncList[A] = foldLeft(xs, FuncList(): FuncList[A])(append)

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

  def foldLeftRight[A, B](as: FuncList[A], z: B)(f: (B, A) => B): B = {
    val left_to_right: (A, B) => B = (a: A, b: B) => f(b, a)
    foldRight(as, z)(left_to_right)
  }

  def foldRightLeft[A, B](as: FuncList[A], z: B)(f: (A, B) => B): B = {
    val right_left: (B, A) => B = (b: B, a: A) => f(a, b)
    foldLeft(as, z)(right_left)
  }

  def init[A](l: FuncList[A]): FuncList[A] =
    l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }

  def length[A](l: FuncList[A]): Int =
    l match {
      case Nil => 0
      case Cons(_, xs) => 1 + length(xs)
    }

  def lengthLeft[A](l: FuncList[A]): Int =
    foldLeft(l, 0)((x, _) => 1 + x)

  def lengthRight[A](l: FuncList[A]): Int =
    foldRight(l, 0)((_, y) => 1 + y)

  def lengthLeftRight[A](l: FuncList[A]): Int =
    foldLeftRight(l, 0)((x, _) => 1 + x)

  def lengthRightLeft[A](l: FuncList[A]): Int =
    foldRightLeft(l, 0)((_, y) => 1 + y)

  def mapF[A,B](as: FuncList[A])(f: A => B): FuncList[B] =
    as match {
      case Nil => Nil
      case Cons(h, t) => Cons(f(h), mapF(t)(f))
    }

  def product(ds: FuncList[Double]): Double =
    ds match {
      case Nil => 1.0
      case Cons(x, xs) => x * product(xs)
    }

  def productLeft(ds: FuncList[Double]): Double =
    foldLeft(ds, 1.0)(_ * _)

  def productLeftRight(ds: FuncList[Double]): Double =
    foldLeftRight(ds, 1.0)(_ * _)

  def productRight(ds: FuncList[Double]): Double =
    foldRight(ds, 1.0)(_ * _)

  def productRightLeft(ds: FuncList[Double]): Double =
    foldRightLeft(ds, 1.0)(_ * _)

  def reverse[A](l: FuncList[A]): FuncList[A] =
    foldLeft(l, Nil: FuncList[A])((x, y) => Cons(y, x))

  def setHead[A](h: A, al: FuncList[A]): FuncList[A] =
    al match {
      case Nil => FuncList(h)
      case Cons(_, xs) => Cons(h, xs)
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


  def tail[A](al: FuncList[A]): FuncList[A] =
    al match {
      case Nil => Nil
      case Cons(_, xs) => xs
    }

  def test(): Unit = {
    val x: FuncList[Int] = FuncList(1,2,3,4,5)
    println(s"FuncList x is ${x.toString}")
  }

  def stringify(as: FuncList[Double]): FuncList[String] =
    as match {
      case Nil => Nil
      case Cons(h, t) => Cons(h.toString, stringify(t))
    }
}
