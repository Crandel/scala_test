object HelloWorld {
  def main(args: Array[String]) {
    println("before")
    val age = 29
    val name = "Vitalii"
    val sin = new Singleton(name)
    println(s"the name is $name and age is $age and sin")
    println(s"the name is $name and age is $age and sin is $sin")
    println(f"sin is ${sin.pr}")
  }
}
