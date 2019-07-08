package simple

object Currying {
  def apply() = {
    val add5 = add(5)
    println(s"add with 5 and 6 arg: ${add(5)(6)}")
    println(s"add5 with 6 arg: ${add5(6)}")
    println(s"add5 class is ${add5.getClass}")
    val partOne = addPart(5)_
    val partTwo = addPart(5)(7)
    println(s"addPart(5)_ with '5' arg is ${partOne(5)}")
    println(s"addPart(5)(7) with who def args is ${partTwo}")
    println(s"Average of (90, 100, 110)(30, 40, 70)(10, 40, 85) is ${average(90, 100, 110)(30, 40, 70)(10, 40, 85)}")
  }

  def add(x: Int): Int => Int = y => x + y

  def addPart(x: Int)(y: Int): Int = x + y

  def average(a: Int*)(b: Int*)(c: Int*) = {
    0.4 * a.sum/a.length + 0.3 * b.sum/b.length + 0.3 * c.sum/c.length
  }
}
