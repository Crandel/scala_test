package parallel

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

object Multitred{
  def fib(n: Int): Int = if (n < 2) 1 else fib(n - 2) + fib( n - 1)

  def tryFutures(n: Int): Unit = {
    val f = Future {
      for (i <- 1 to n) yield i * 2
    }

    f.onComplete{
      case Success(vi) => println(s"Successfully finish with $vi")
      case Failure(ex) => println(s"Failed with exception $ex")
    }
    println("After future")
  }
  def apply(n: Int): Unit = {
    tryFutures(n)
    Thread.sleep(3000)
    println("Done")
  }
}
