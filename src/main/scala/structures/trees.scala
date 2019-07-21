package structures

sealed trait FuncTree[+A]
case class Leaf[A](value: A) extends FuncTree[A]
case class Branch[A](left: FuncTree[A], right: FuncTree[A]) extends FuncTree[A]

object FuncTree {
  def sizeT(at: FuncTree[_]): Int = at match {
    case Leaf(_) => 1
    case Branch(l, r) => sizeT(l) + sizeT(r) + 1
  }
}
