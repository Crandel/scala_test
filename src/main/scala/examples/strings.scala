package examples

object StringExamples {
  def apply() = {
    println("Start with new line")
    print("Start without new line ")
    print("continue lines")
    print("\n")
    val age = 29
    val flt = 44.67
    val name = "Vitalii"
    val mpList = Map(2 -> "Some str", 4 -> "Another str")
    // s Interpolation if we need to print variables
    println(s"the name is $name and age is $age")
    // f Interpolation if we need some transformation using %s or %f styles
    println(f"Map is ${mpList.toString}%s and float is $flt%.4f")
    // same with printf
    printf("Map is %s and float is %.4f", mpList.toString, flt)
  }
}
