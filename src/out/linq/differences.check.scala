/*****************************************
Emitting Generated Code
*******************************************/
import lms.core.examples.TutorialLinqSchema
class Snippet extends ((Unit)=>(collection.immutable.List)) {
  def apply(x2:Unit): collection.immutable.List = {
    val x0 = TutorialLinqSchema.db
    val x3 = x0.couples
    val x38 = TutorialLinqSchema.db.couples.flatMap { x4 =>
      val x5 = x0.people
      val x37 = TutorialLinqSchema.db.people.flatMap { x6 =>
        val x36 = TutorialLinqSchema.db.people.flatMap { x23 =>
          val x8 = x4.her
          val x9 = x6.name
          val x10 = x8 == x9
          val x11 = x4.him
          val x24 = x23.name
          val x25 = x11 == x24
          val x26 = x10 && x25
          val x15 = x6.age
          val x27 = x23.age
          val x28 = x15 > x27
          val x29 = x26 && x28
          val x35 = if (x29) {
            val x32 = x15 - x27
            val x33 = new TutorialLinqSchema.Record { val name = x9; val diff = x32 }
            val x34 = List(x33)
            x34
          } else {
            val x20 = List()
            x20
          }
          x35
        }
        x36
      }
      x37
    }
    x38
  }
}
/*****************************************
End of Generated Code
*******************************************/
