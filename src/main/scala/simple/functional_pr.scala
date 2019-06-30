package simple

import scala.math.{log10}

object FunctionalPR {
  def apply() = {
    println("Calculate factorial of 8")
    val fr = factorial(8)
    println(fr)
    println("*" * 10)
    println("Calculate fibonacci of 8")
    val fib = fibonacci(8)
    println(fib)
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
