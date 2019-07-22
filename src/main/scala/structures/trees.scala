package structures

sealed trait TreeF[+A]
case class LeafFT[A](value: A) extends TreeF[A]
case class BranchFT[A](left: TreeF[A], right: TreeF[A]) extends TreeF[A]

object TreeF {
  def sizeT(at: TreeF[_]): Int =
    at match {
      case LeafFT(_) => 1
      case BranchFT(l, r) => sizeT(l) + sizeT(r) + 1
    }

  def maximumT(at: TreeF[Int]): Int =
    at match {
      case LeafFT(n) => n
      case BranchFT(l, r) => maximumT(l) max maximumT(r)
    }

  def depthT[A](at: TreeF[A]): Int =
    at match {
      case LeafFT(_) => 1
      case BranchFT(l, r) => 1 + depthT(l) max depthT(r)
    }

  def mapT[A, B](at: TreeF[A])(f: (A) => B): TreeF[B] =
    at match {
      case LeafFT(n) => LeafFT(f(n))
      case BranchFT(l, r) => BranchFT(mapT(l)(f), mapT(r)(f))
    }
}
