package functional

object Currying {
  def apply(): Unit = {
    val add5: Int => Int = add(5)
    println(s"add with 5 and 6 arg: ${add(5)(6)}")
    println(s"add5 with 6 arg: ${add5(6)}")
    println(s"add5 class is ${add5.toString}")
    val partOne: Int => Int = addPart(5)_
    val partTwo: Int = addPart(5)(7)
    println(s"addPart(5)_ with '5' arg is ${partOne(5)}")
    println(s"addPart(5)(7) with who def args is $partTwo")
    println(s"Average of (90, 100, 110)(30, 40, 70)(10, 40, 85) is ${average(90, 100, 110)(30, 40, 70)(10, 40, 85)} \n\n")

    println(s"Currying funcs")
    val cur: Int => Int => Int = curry((a: Int, b: Int) => a * b)
    println(s"Currying func $cur example ${cur(5)(4)}\n")
    val uncur: (Int, Int) => Int = uncurry(cur)
    println(s"Uncurrying func $uncur example $uncur(3, 7)\n")

    val comp_f: Int => Int = (a: Int) => a + 7
    val comp_g: Int => Int = (b: Int) => b + 3
    val comp_res: Int = compose_test(comp_f, comp_g)(5)
    println(s"Compose example $comp_res")
  }

  def add(x: Int): Int => Int = y => x + y

  def addPart(x: Int)(y: Int): Int = x + y

  def average(a: Int*)(b: Int*)(c: Int*): Double = {
    0.4 * a.sum/a.length + 0.3 * b.sum/b.length + 0.3 * c.sum/c.length
  }

  def curry[A, B, C](f:(A, B) => C): A => B => C = {
    a => b => f(a, b)
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  def compose_test[A, B, C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }
}
