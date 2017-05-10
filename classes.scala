class ClassExample(name: String) {
  def pr(): String = {
    return "method with name " + name
  }
  override def toString(): String = s"Class with name $name"
}
