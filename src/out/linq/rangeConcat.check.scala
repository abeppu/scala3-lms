/*****************************************
Emitting Generated Code
*******************************************/
import lms.core.examples.TutorialLinqSchema
class Snippet extends ((Unit)=>(collection.immutable.List)) {
  def apply(x1:Unit): collection.immutable.List = {
    val x0 = TutorialLinqSchema.db
    val x2 = x0.people
    val x23 = TutorialLinqSchema.db.people.flatMap { x12 =>
      val x13 = x12.age
      val x14 = 30 <= x13
      val x15 = x13 < 34
      val x16 = x14 && x15
      val x22 = if (x16) {
        val x19 = x12.name
        val x20 = new TutorialLinqSchema.Record { val name = x19; val age = x13 }
        val x21 = List(x20)
        x21
      } else {
        val x9 = List()
        x9
      }
      x22
    }
    val x43 = TutorialLinqSchema.db.people.flatMap { x32 =>
      val x33 = x32.age
      val x34 = 55 <= x33
      val x35 = x33 < 61
      val x36 = x34 && x35
      val x42 = if (x36) {
        val x39 = x32.name
        val x40 = new TutorialLinqSchema.Record { val name = x39; val age = x33 }
        val x41 = List(x40)
        x41
      } else {
        val x9 = List()
        x9
      }
      x42
    }
    val x44 = x23 ::: x43
    x44
  }
}
/*****************************************
End of Generated Code
*******************************************/
