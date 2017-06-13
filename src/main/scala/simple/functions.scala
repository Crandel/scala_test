package simple

import scala.math.{log10}

object FunctionsExample {
  def run() {
    def listLoop(){
      val prList = (1 to 11).toList
      for( i <- prList){
        if(i == 9){
          return
        }
        println(i)
      }
    }
    listLoop

    // first class func
    val log10f = log10 _
    println(log10f(12))
    println()
    List(1333.6, 5444.5, 77.3).map(log10f).foreach(println)
    println()
    // lambda
    List(63, 54, 7).map((x: Int) => x * 5 ).foreach(println)
    println()
    List(63, 54, 7).filter( _ % 2 == 0 ).foreach(println)

    def multF(func: (Double) => Double, n: Double) = {
      func(n)
    }
    println()
    println(s"log10 of 60.0 is ${multF(log10f, 60.0)}")

    // Closure
    val div = 5
    def divider = (n: Double) => n/div

    println(s"66 / 5 is ${divider(66.0)}")
  }
}
