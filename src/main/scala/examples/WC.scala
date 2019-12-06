package examples

import functional.structures.Monoid


sealed trait WC
case class Stub(chars: String) extends WC
case class Part(lstub: String, words: Int, rstub: String) extends WC

object WCExample{
  val WCMonoid: Monoid[WC] = new Monoid[WC] {
    override def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Stub(ls), Stub(rs)) => Stub(ls + rs)
      case (Stub(ls), Part(lstub, words, rstub)) => Part(ls + lstub, words, rstub)
      case (Part(lstub, words, rstub), Stub(rs)) => Part(lstub, words, rs + rstub)
      case (Part(llstub, wordsl, lrstub), Part(rlstub, wordsr, rrstub)) =>
        Part(llstub, wordsl + wordsr + (if (lrstub + rlstub == "") 0 else 1), rrstub)
    }

    override def zero: WC = Stub("")
  }

  def stringToWC(str: String): WC = {
    val wordList: Array[String] = str.split("\\s+")

    lazy val lLength: Int = wordList.length
    lazy val startSpace: Boolean = str.startsWith(" ")
    lazy val endSpace: Boolean = str.endsWith(" ")
    if (wordList.isEmpty)
      Stub(str)
    else {
      if (lLength == 1)
        Stub(wordList.head)
      else if (startSpace && endSpace)
        Part("", lLength - 1, "")
      else if (startSpace)
        // -2 is because if you split string and have a startSpace first element will be ""
        Part("", lLength - 2, wordList.last)
      else
        Part(wordList.head, lLength - 1, "")
    }
  }

  def splitString(s: String, threshold: Int = 50): WC =
    if (s.length > 50) {
      val (left, right) = s.splitAt(s.length / 2)
      WCMonoid.op(splitString(left, threshold), splitString(right, threshold))
    } else
      stringToWC(s)

  def countWC(str: String): Int = {
    val wc: WC = splitString(str)
    wc match {
      case Stub(_) => 1
      case Part(lstub, words, rstub) =>
        words + (if (lstub + rstub == "") 0 else 1)
    }
  }
}

