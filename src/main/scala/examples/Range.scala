package examples

import functional.structures.Monoid
import functional.structures.Monoids.foldMapV

sealed trait Range
case object ZeroRange extends Range
case object UnorderedRange extends Range
case class OrderedRange(min: Int, max: Int) extends Range

object OrderingExample {
  val RangeMonoid: Monoid[Range] = new Monoid[Range] {
    override def zero: ZeroRange.type = ZeroRange
    override def op(a1: Range, a2: Range): Range = (a1, a2) match {
      case (ZeroRange, a) => a
      case (a, ZeroRange) => a
      case (UnorderedRange, _) => UnorderedRange
      case (_, UnorderedRange) => UnorderedRange
      case (OrderedRange(minL, maxL), OrderedRange(minR, maxR)) =>
        if (maxL > maxR) {
          UnorderedRange
        } else {
          OrderedRange(minL, maxR)
        }
    }
  }

  def isOrdered(xs: IndexedSeq[Int]): Boolean = foldMapV(xs, RangeMonoid)(x => OrderedRange(x, x)) match {
    case UnorderedRange => false
    case _ => true
  }
}
