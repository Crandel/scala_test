package structures

sealed trait ListF[+A]
case object NilFL extends ListF[Nothing]
case class ConFL[+A](head: A, tail:ListF[A]) extends ListF[A]

object ListF {
  def add1(xs: ListF[Int]): ListF[Int] =
    xs match {
      case NilFL => NilFL
      case ConFL(x, y) => ConFL(x + 1, add1(y))
  }

  def append[A](a1: ListF[A], a2: ListF[A]): ListF[A] =
    a1 match {
      case NilFL => a2
      case ConFL(h, t) => ConFL(h, append(t, a2))
    }

  def appendRight[A](a1: ListF[A], a2: ListF[A]): ListF[A] = {
    foldRight(a1, a2)(ConFL(_, _))
  }

  def apply[A](as: A*): ListF[A] = {
    if (as.isEmpty) NilFL
    else ConFL(as.head, apply(as.tail: _*))
  }

  @annotation.tailrec
  def drop[A](l: ListF[A], n: Int): ListF[A] =
    n match {
      case 0 => l
      case _ => drop(tail(l), n - 1)
    }

  @annotation.tailrec
  def dropWhile[A](l: ListF[A])(f: A => Boolean): ListF[A] =
    l match {
      case ConFL(x, xs) if f(x) => dropWhile(xs)(f)
      case _ => l
    }

  def filter[A](l: ListF[A])(f: A => Boolean): ListF[A] =
    l match {
      case NilFL => NilFL
      case ConFL(x, xs) =>
        if (f(x))
          ConFL(x, filter(xs)(f))
        else
          filter(xs)(f)
    }

  def filterRight[A](l: ListF[A])(f: A => Boolean): ListF[A] =
    foldRight(l, ListF(): ListF[A])((a, acc) => if (f(a)) ConFL(a, acc) else acc)

  def filterMap[A](l: ListF[A])(f: A => Boolean): ListF[A] =
    flatMap(l)(xs => if (f(xs)) ListF(xs) else NilFL)

  def flatMap[A, B](l: ListF[A])(f: A=> ListF[B]): ListF[B] =
    l match {
      case NilFL => NilFL
      case ConFL(h, t) => append(f(h), flatMap(t)(f))
    }

  def flatten[A](xs: ListF[ListF[A]]): ListF[A] = foldLeft(xs, ListF(): ListF[A])(append)

  def foldRight[A, B](as: ListF[A], z: B)(f: (A, B) => B): B =
    as match {
      case NilFL => z
      case ConFL(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def foldLeft[A,B](as: ListF[A], z: B)(f: (B, A) => B): B = {
    @annotation.tailrec
    def iter(as: ListF[A], acc: B): B =
      as match {
        case NilFL => acc
        case ConFL(x, xs) => iter(xs, f(acc, x))
      }
    iter(as, z)
  }

  def foldLeftRight[A, B](as: ListF[A], z: B)(f: (B, A) => B): B = {
    val left_to_right: (A, B) => B = (a: A, b: B) => f(b, a)
    foldRight(as, z)(left_to_right)
  }

  def foldRightLeft[A, B](as: ListF[A], z: B)(f: (A, B) => B): B = {
    val right_left: (B, A) => B = (b: B, a: A) => f(a, b)
    foldLeft(as, z)(right_left)
  }

  def init[A](l: ListF[A]): ListF[A] =
    l match {
      case NilFL => NilFL
      case ConFL(_, NilFL) => NilFL
      case ConFL(x, xs) => ConFL(x, init(xs))
    }

  def length[A](l: ListF[A]): Int =
    l match {
      case NilFL => 0
      case ConFL(_, xs) => 1 + length(xs)
    }

  def lengthLeft[A](l: ListF[A]): Int =
    foldLeft(l, 0)((x, _) => 1 + x)

  def lengthRight[A](l: ListF[A]): Int =
    foldRight(l, 0)((_, y) => 1 + y)

  def lengthLeftRight[A](l: ListF[A]): Int =
    foldLeftRight(l, 0)((x, _) => 1 + x)

  def lengthRightLeft[A](l: ListF[A]): Int =
    foldRightLeft(l, 0)((_, y) => 1 + y)

  def mapF[A,B](as: ListF[A])(f: A => B): ListF[B] =
    as match {
      case NilFL => NilFL
      case ConFL(h, t) => ConFL(f(h), mapF(t)(f))
    }

  def product(ds: ListF[Double]): Double =
    ds match {
      case NilFL => 1.0
      case ConFL(x, xs) => x * product(xs)
    }

  def productLeft(ds: ListF[Double]): Double =
    foldLeft(ds, 1.0)(_ * _)

  def productLeftRight(ds: ListF[Double]): Double =
    foldLeftRight(ds, 1.0)(_ * _)

  def productRight(ds: ListF[Double]): Double =
    foldRight(ds, 1.0)(_ * _)

  def productRightLeft(ds: ListF[Double]): Double =
    foldRightLeft(ds, 1.0)(_ * _)

  def reverse[A](l: ListF[A]): ListF[A] =
    foldLeft(l, NilFL: ListF[A])((x, y) => ConFL(y, x))

  def setHead[A](h: A, al: ListF[A]): ListF[A] =
    al match {
      case NilFL => ListF(h)
      case ConFL(_, xs) => ConFL(h, xs)
    }

  def sum(ints: ListF[Int]): Int =
    ints match {
      case NilFL => 0
      case ConFL(x, xs) => x + sum(xs)
    }

  def sumLeft(ints: ListF[Int]): Int =
    foldLeft(ints, 0)((x, y) => x + y)

  def sumRight(ints: ListF[Int]): Int =
    foldRight(ints, 0)((x, y) => x + y)


  def stringify(as: ListF[Double]): ListF[String] =
    as match {
      case NilFL => NilFL
      case ConFL(h, t) => ConFL(h.toString, stringify(t))
    }

  def tail[A](al: ListF[A]): ListF[A] =
    al match {
      case NilFL => NilFL
      case ConFL(_, xs) => xs
    }

  def test(): Unit = {
    val x: ListF[Int] = ListF(1,2,3,4,5)
    println(s"FuncList x is ${x.toString}")
  }

  def zipWith[A, B, C](ls: ListF[A], rs: ListF[B])(f: (A, B) => C): ListF[C] =
    (ls, rs) match {
      case (NilFL, _) => NilFL
      case (_, NilFL) => NilFL
      case (ConFL(lh, lt), ConFL(rh, rt)) => ConFL(f(lh, rh), zipWith(lt, rt)(f))
    }
}
