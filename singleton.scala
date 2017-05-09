class Singleton(name: String) {
  def pr(): String = {
    return "method with name " + name
  }
  override def toString(): String = s"Singleton with name $name"
}
