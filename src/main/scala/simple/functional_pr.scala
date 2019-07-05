package simple

import scala.math.{log10}

object FunctionalPR {
  def apply() = {
    val test_num = 15
    println("*" * 10)
    formatResult("fibonacci", test_num, fibonacci(test_num))
    formatResult("factorial", test_num, factorial(test_num))
  }

  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is % d"
    msg.format(name, n, f(n))
  }

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n-1, n*acc)
    go(n, 1)
  }

  def fibonacci(n: Int): Int = {
    @annotation.tailrec
    def fib_tail(n: Int, a: Int, b : Int): Int = n match {
      case 0 => a
      case _ => fib_tail(n-1, b, a+b)
    }
    fib_tail(n, 0, 1)
  }
}
