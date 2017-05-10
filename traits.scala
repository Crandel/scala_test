object TraitsObject {
  def main(args : Array[String]){
    val superman = new SuperHero("Superman")
    println(superman.fly)
    println(superman.hit)
    println(superman.ricochet(5.8))
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
}
