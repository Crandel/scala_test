package examples

import structures.Monoid


sealed trait WC{
  def +(a2: WC): WC = this match {
    case Stub(a) => Stub(a)
  }
}
case class Stub(chars: String) extends WC
case class Part(lstub: String, words: Int, rstub: String) extends WC

object WCExample{
  val WCMonoid: Monoid[WC] = new Monoid[WC] {
    override def op(a1: WC, a2: WC): WC = a1 + a2

    override def zero: WC = Stub("")
  }
}

