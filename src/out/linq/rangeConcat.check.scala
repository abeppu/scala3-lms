/*****************************************
Emitting Generated Code
*******************************************/
import lms.core.examples.TutorialLinqSchema
class Snippet extends ((Unit)=>(collection.immutable.List)) {
  def apply(x2:Unit): collection.immutable.List = {
    val x0 = TutorialLinqSchema.db
    val x3 = x0.people
    val x24 = TutorialLinqSchema.db.people.flatMap { x13 =>
      val x14 = x13.age
      val x15 = 30 <= x14
      val x16 = x14 < 34
      val x17 = x15 && x16
      val x23 = if (x17) {
        val x20 = x13.name
        val x21 = new TutorialLinqSchema.Record { val name = x20; val age = x14 }
        val x22 = List(x21)
        x22
      } else {
        val x10 = List()
        x10
      }
      x23
    }
    val x44 = TutorialLinqSchema.db.people.flatMap { x33 =>
      val x34 = x33.age
      val x35 = 55 <= x34
      val x36 = x34 < 61
      val x37 = x35 && x36
      val x43 = if (x37) {
        val x40 = x33.name
        val x41 = new TutorialLinqSchema.Record { val name = x40; val age = x34 }
        val x42 = List(x41)
        x42
      } else {
        val x10 = List()
        x10
      }
      x43
    }
    val x45 = x24 ::: x44
    x45
  }
}
/*****************************************
End of Generated Code
*******************************************/
