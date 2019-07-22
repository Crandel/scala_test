package structures


sealed trait StreamF[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case SCon(h, _) => Some(h())
  }

  def toList: List[A] = this match {
    case Empty => Nil
    case SCon(h, t) => h() :: t().toList
  }
}
case object Empty extends StreamF[Nothing]
case class SCon[+A](h: () => A, t: () => StreamF[A]) extends StreamF[A]

object StreamF {
  def cons[A](hd: => A, ht: => StreamF[A]): StreamF[A] = {
    lazy val head: A = hd
    lazy val tail: StreamF[A] = ht
    SCon(() => head, () => tail)
  }

  def empty[A](): StreamF[A] = Empty

  def apply[A](as: A*): StreamF[A] =
    if (as.isEmpty) empty() else cons(as.head, apply(as.tail: _*))
}
