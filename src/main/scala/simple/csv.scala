package simple

import java.io.PrintWriter
import java.text.SimpleDateFormat
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object CSVCleaner {
  def apply() {
    val filename = "test.csv"
    val content = ArrayBuffer.empty[String]
    var row_number = 0
    val num = """\d+""".r
    val text = Source.fromURL(getClass.getResource("/" + filename)).getLines
    text.foreach(x => {
                   if (!x.startsWith("!")){
                     var row = x.split('|')
                     val outputFormat = new SimpleDateFormat("yyyy-MM-dd")
                     val inputFormat1 = new SimpleDateFormat("dd-MMM-yyyy")
                     val inputFormat2 = new SimpleDateFormat("MM/dd/yyyy")
                     val first = inputFormat1.parse(row(2))
                     println(first)
                     row(2) = outputFormat.format(first)
                     println("*" * 15)
                     println(row(2))
                     content += row.mkString("|")
                   } else if (!text.hasNext) {
                     // get Int from String
                     row_number = num.findAllIn(x).mkString.toInt
                   }
                 })
    print(s"$content with row number = $row_number")
  }
}
