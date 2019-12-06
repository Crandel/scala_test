package functional.structures

sealed trait StreamF[+A] {
  def drop(n: Int): StreamF[A] = n match {
    case 0 => this
    case _ => this match {
      case ConFS(_, t) if n >= 0 => t().drop(n - 1)
      case _ => EmptyFS
    }
  }

  def exists(p: A => Boolean): Boolean = this match {
    case ConFS(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def existsRight(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case ConFS(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def headOption: Option[A] = this match {
    case EmptyFS => None
    case ConFS(h, _) => Some(h())
  }

  def take(n: Int): StreamF[A] = n match {
    case 0 => EmptyFS
    case _ => this match {
      case EmptyFS => EmptyFS
      case ConFS(h, t) => ConFS(() => h(), () => t().take(n - 1))
    }
  }

  def takeWhile(f: A => Boolean): StreamF[A] = this match {
    case ConFS(h, t) if f(h()) => ConFS(() => h(), () => t().takeWhile(f))
    case _ => EmptyFS
  }

  def toList: List[A] = this match {
    case EmptyFS => Nil
    case ConFS(h, t) => h() :: t().toList
  }

}
case object EmptyFS extends StreamF[Nothing]
case class ConFS[+A](h: () => A, t: () => StreamF[A]) extends StreamF[A]

object StreamF {
  def cons[A](hd: => A, ht: => StreamF[A]): StreamF[A] = {
    lazy val head: A = hd
    lazy val tail: StreamF[A] = ht
    ConFS(() => head, () => tail)
  }

  def empty[A](): StreamF[A] = EmptyFS

  def apply[A](as: A*): StreamF[A] =
    if (as.isEmpty) empty() else cons(as.head, apply(as.tail: _*))
}
