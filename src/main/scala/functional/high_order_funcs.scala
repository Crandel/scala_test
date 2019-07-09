package functional

import scala.math.{log10}

object Functions {
  def apply() = {
    val test_num = 15
    println(formatResult("factorial", test_num, factorial))
    println("-" * 10)
    println(formatResult("fibonacci", test_num, fibonacci))
    println("*" * 10)
    println(formatResult("Abs", -test_num, abs))
    println("+" * 10)
  }

  def abs(n: Int): Int = {
    if (n >= 0) n
    else -n
  }

  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d"
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

  def first_elem[A] (as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int = {
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)
    }
    loop(0)
  }

  def isSorted[T](as: Array[T], p: (T, T) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean = {
      if (n >= as.length - 1) true
      else if (p(as(n), as(n + 1))) false
      else loop(n + 1)
    }

    loop(0)
  }
}
