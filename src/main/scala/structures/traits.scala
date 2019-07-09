package structures

object TraitsObject {
  def apply(): Unit = {
    val superman = new SuperHero("Superman")
    println(superman.fly)
    println(superman.hit)
    println(superman.ricochet(5.8))
  }

  abstract class Writer {
    def write(str: String): Unit
  }

  trait Upper extends Writer {
    abstract override def write(str: String) = super.write(str.toUpperCase())
  }

  trait Flyable {
    def fly: String
  }

  trait BulletProof{
    def hit: String

    def ricochet(startSpeed: Double): String = {
      f"Bullet ricochet at speed ${startSpeed * .75} m/s"
    }
  }

  class SuperHero(val name: String) extends Flyable with BulletProof{
    def fly = s"$name flys in the sky"

    def hit = s"$name is a bulletproof"
  }

  class ConsoleWriter extends Writer {
    def write(str: String) = println(str)
  }

  val console = new ConsoleWriter with Upper
  console.write("some downcase string")
}
