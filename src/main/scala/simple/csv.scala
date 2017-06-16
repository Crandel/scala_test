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
                     val new_x = x.replace(',', '.')
                     var row = new_x.split('|')
                     val outputFormat = new SimpleDateFormat("yyyy-MM-dd")
                     val inputFormat1 = new SimpleDateFormat("yy-MMM-dd")
                     val inputFormat2 = new SimpleDateFormat("MM/dd/yyyy")
                     val first = inputFormat1.parse(row(2))
                     val second = inputFormat2.parse(row(3))
                     row(2) = outputFormat.format(inputFormat1.parse(row(2)))
                     row(3) = outputFormat.format(inputFormat2.parse(row(3)))

                     content += row.mkString("|")
                   } else if (!text.hasNext) {
                     // get Int from String
                     row_number = num.findAllIn(x).mkString.toInt
                   }
                 })
    if (row_number > 0 && row_number == content.length) {
      val full_filename = "files/clean_test.csv"
      val writer = new PrintWriter(full_filename)
      for(m <- content){
        writer.write(m.toString + "\n")
      }
      writer.close()
    }
    print(f"${content foreach println} \n Row number = $row_number")
  }
}
