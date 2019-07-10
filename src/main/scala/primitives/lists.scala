package primitives

object ListExample {
  def apply() = {
    println("Test pattern matching in List")
    val x = List(1,2,3,4,5) match {
      case x :: 2 :: 4 :: _ => x
      case Nil => 42
      case x :: y :: 3 :: 4 :: _ => x + y
      case h :: t => h + t.sum
      case _ => 101
    }
    println(x)
  }
}
