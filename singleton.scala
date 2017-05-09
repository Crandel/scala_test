class Singleton(name: String) {
  def pr(): String = {
    return name
  }
  override def toString(): String = s"Singleton with name $name"
}
