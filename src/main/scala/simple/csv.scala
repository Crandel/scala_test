package simple

import java.io.PrintWriter
import scala.collection.mutable.ArrayBuffer

import scala.io.Source

object CSVCleaner {
  def apply() {
    val filename = "test.csv"
    val content = ArrayBuffer.empty[Array[String]]
    var row_number = 0
    val num = """\d+""".r
    val text = Source.fromURL(getClass.getResource("/" + filename)).getLines
    text.foreach(x => {
                   if (!x.startsWith("!")){
                     var row = x.split("|")
                     
                     content += row
                   } else if (!text.hasNext) {
                     row_number = num.findAllIn(x).mkString.toInt
                   }
                 })
    print(s"$content with row number = $row_number")
  }
}
