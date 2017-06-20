package simple

import scala.collection.mutable.{Map => MMap}

object MapsExample {
  def apply() = {
    val empl = Map("Manager" -> "Bob", "Secretary" -> "Jack")

    if (empl.contains("Manager"))
      println(s"Manager: ${empl("Manager")}")

    val custs = MMap(1 -> "Paul", 3 -> "Tom", 5 -> "Elena")
    println(s"custs -> ${custs.toString}")
    custs(1) = "Margaret"
    custs(44) = "Anton"
    for ((k, v) <- custs)
      println(s"$k -> $v")
  }
}
