/*****************************************
Emitting Generated Code
*******************************************/
import lms.core.examples.TutorialLinqSchema
class Snippet extends ((Unit)=>(collection.immutable.List)) {
  def apply(x2:Unit): collection.immutable.List = {
    val x0 = TutorialLinqSchema.db
    val x3 = x0.people
    val x27 = TutorialLinqSchema.db.people.flatMap { x14 =>
      val x15 = x14.age
      val x16 = x15 < 30
      val x17 = 40 <= x15
      val x18 = x16 || x17
      val x21 = if (x18) {
        val x10 = List()
        x10
      } else {
        val x20 = List(x14)
        x20
      }
      val x26 = x21.flatMap { x22 =>
        val x23 = x22.name
        val x24 = new TutorialLinqSchema.Record { val name = x23 }
        val x25 = List(x24)
        x25
      }
      x26
    }
    x27
  }
}
/*****************************************
End of Generated Code
*******************************************/
