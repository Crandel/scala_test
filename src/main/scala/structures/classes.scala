package structures

object ClassE {
  def apply():Unit = {

    // test different constructors
    val cls = new ClassExample("One param")
    println(s"Class with name='One param' -> $cls")
    println(cls.pr)
    println()
    cls.change
    println(cls)
    // error: reassignment to val - protected attr
    // cls.name = "New name"
    println(cls.name)
    cls.numb = 33
    println(s"After cls.numb = 33 -> ${cls.numb}")
    cls.numb = 3
    // setter setNumb works only on initiation
    println(s"After cls.numb = 3 -> ${cls.numb}")

    val cls2 = new ClassExample("Two params", 5)
    println(s"Class with name='Two params' and numb=5 -> $cls2")
    println(s"old prName -> ${cls2.prName}")
    cls2.prName = "name"
    println(s"new prName -> ${cls2.prName}")
    cls2.prName = "New prName"
    println(s"last new prName -> ${cls2.prName}")

    val cls3 = new ClassExample(50)
    println(s"Class with numb=50 -> $cls3")
    val child = new ChildExample(numb=50)
    println(s"Child with numb=50 -> $child")
    child.change
    println(s"Child with numb=50 -> $child")
  }

  abstract class AbsExample(numb: Int){
    def change(): Unit
  }
  class ClassExample(val name: String, var numb: Int) extends AbsExample(numb) {
    this.setNumb(numb)

    val id = ClassExample.newId

    protected var _prName: String = "Some protected name"

    def setNumb(numb: Int): Unit = {
      if (numb > 10)
        this.numb = numb
      else
        this.numb = 10
    }

    def prName = this._prName

    // prName_= - mandatory format for setter
    def prName_=(name: String) = {
      if (name.length > 5){
        this._prName = name
      }
    }

    // Another Constructor if class will be created without parameters
    def this(){
      this("No name", 5)
    }

    // Another Constructor if class will be created with numb parameter
    def this(numb: Int){
      this("No name", 5)
      this.setNumb(numb)
    }

    // Another Constructor if class will be created with name parameter
    def this(name: String){
      this(name, 10)
    }

    def pr(): String = {
      return s"Method with name $name and numb $numb"
    }

    def change(): Unit = {
      // error: reassignment to val - protected attr
      // name = "Change name"
      numb = 123
    }

    override def toString(): String = "Class toString with name %s, numb %d, id %d and prName %s".format(
      this.name, this.numb, this.id, this.prName)
  }

  object ClassExample {
    private var id = 0
    def newId(): Int = {
      id = id + 1
      return id
    }
  }

  class ChildExample(name: String, numb: Int, val price: Double = 0.0) extends ClassExample(name, numb){
    def this(numb: Int){
      this("No name", 0)
      this.setNumb(numb)
    }

    override def change() = {
      this.setNumb(78)
      this.prName = "fffff name"
    }
  }
}
