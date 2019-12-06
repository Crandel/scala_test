package functional.structures

sealed trait OptionF[+A] {
  def map[B](f: A=> B): OptionF[B] =
    this match {
      case NoneF => NoneF
      case SomeF(a) => SomeF(f(a))
    }

  def flatMap[B](f: A => OptionF[B]): OptionF[B] =
    this match {
      case NoneF => NoneF
      case SomeF(a) => f(a)
    }

  def getOrElse[B >: A](default: => B): B =
    this match {
      case NoneF => default
      case SomeF(a) => a
    }
  def orElse[B >: A](default: => OptionF[B]): OptionF[B] =
    this match {
      case NoneF => default
      case SomeF(a) => SomeF(a)
    }
}
case class SomeF[+A](get: A) extends OptionF[A]
case object NoneF extends OptionF[Nothing]
