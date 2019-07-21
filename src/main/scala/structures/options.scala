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

  def getOrElse[B >: A](default: => B): B =
    this match {
      case None => default
      case Some(a) => a
    }
  def orElse[B >: A](default: => FuncOption[B]): FuncOption[B] =
    this match {
      case None => default
      case Some(a) => Some(a)
    }
}
case class Some[+A](get: A) extends FuncOption[A]
case object None extends FuncOption[Nothing]
