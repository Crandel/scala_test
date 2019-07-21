package structures

sealed trait FuncTree[+A]
case class Leaf[A](value: A) extends FuncTree[A]
case class Branch[A](left: FuncTree[A], right: FuncTree[A]) extends FuncTree[A]

object FuncTree {
  def sizeT(at: FuncTree[_]): Int =
    at match {
      case Leaf(_) => 1
      case Branch(l, r) => sizeT(l) + sizeT(r) + 1
    }

  def maximumT(at: FuncTree[Int]): Int =
    at match {
      case Leaf(n) => n
      case Branch(l, r) => maximumT(l) max maximumT(r)
    }

  def depthT[A](at: FuncTree[A]): Int =
    at match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + depthT(l) max depthT(r)
    }

  def mapT[A, B](at: FuncTree[A])(f: (A) => B): FuncTree[B] =
    at match {
      case Leaf(n) => Leaf(f(n))
      case Branch(l, r) => Branch(mapT(l)(f), mapT(r)(f))
    }
}
