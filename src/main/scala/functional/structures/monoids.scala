package functional.structures

trait Monoid[A]{
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoids {
  def boolAndMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    def zero: Boolean = true
  }

  def boolOrMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    def zero: Boolean = false
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(a1: A => A, a2: A => A): A => A = a1 compose a2
    def zero: A => A = identity[A]
  }

  def intAddMonoid: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 + a2
    def zero: Int = 0
  }

  def intMultMonoid: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 * a2
    def zero: Int = 1
  }

  def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
    def zero: List[A] = Nil
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
    def zero: Option[A] = None
  }

  val StringMonoid: Monoid[String] = new Monoid[String] {
    def op(a1: String, a2: String): String = a1 + a2
    def zero: String = ""
  }

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = {
    foldMap(as, endoMonoid[B])({a: A => (b: B) => f(b, a)})(z)
  }

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = {
    foldMap(as, endoMonoid[() => B])({ a: A => (b: () => B) => () => f(a, b())})(() => z)()
  }

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (as.isEmpty) m.zero
    else if (as.length == 1) m.op(m.zero, f(as(0)))
    else {
      val (left, right) = as.splitAt(as.length / 2)
      m.op(foldMapV(left, m)(f), foldMapV(right, m)(f))
    }
  }
}
