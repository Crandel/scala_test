package structures

sealed trait FuncOption[+A] {
  def map[B](f: A=> B): FuncOption[B] =
    this match {
      case None => None
      case Some(a) => Some(f(a))
    }
  def flatMap[B](f: A => FuncOption[B]): FuncOption[B] =
    this match {
      case None => None
      case Some(a) => f(a)
    }
}
case class Some[+A](get: A) extends FuncOption[A]
case object None extends FuncOption[Nothing]
