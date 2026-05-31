/*****************************************
Emitting Generated Code
*******************************************/
import lms.core.examples.TutorialLinqSchema
class Snippet extends ((Unit)=>(collection.immutable.List)) {
  def apply(x1:Unit): collection.immutable.List = {
    val x0 = TutorialLinqSchema.db
    val x2 = x0.couples
    val x37 = TutorialLinqSchema.db.couples.flatMap { x3 =>
      val x4 = x0.people
      val x36 = TutorialLinqSchema.db.people.flatMap { x5 =>
        val x35 = TutorialLinqSchema.db.people.flatMap { x22 =>
          val x7 = x3.her
          val x8 = x5.name
          val x9 = x7 == x8
          val x10 = x3.him
          val x23 = x22.name
          val x24 = x10 == x23
          val x25 = x9 && x24
          val x14 = x5.age
          val x26 = x22.age
          val x27 = x14 > x26
          val x28 = x25 && x27
          val x34 = if (x28) {
            val x31 = x14 - x26
            val x32 = new TutorialLinqSchema.Record { val name = x8; val diff = x31 }
            val x33 = List(x32)
            x33
          } else {
            val x19 = List()
            x19
          }
          x34
        }
        x35
      }
      x36
    }
    x37
  }
}
/*****************************************
End of Generated Code
*******************************************/
