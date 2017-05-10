object FunctionsExample {
  def main(args: Array[String]) {
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
  }
}
