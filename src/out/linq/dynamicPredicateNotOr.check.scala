/*****************************************
Emitting Generated Code
*******************************************/
import lms.core.examples.TutorialLinqSchema
class Snippet extends ((Unit)=>(collection.immutable.List)) {
  def apply(x1:Unit): collection.immutable.List = {
    val x0 = TutorialLinqSchema.db
    val x2 = x0.people
    val x26 = TutorialLinqSchema.db.people.flatMap { x13 =>
      val x14 = x13.age
      val x15 = x14 < 30
      val x16 = 40 <= x14
      val x17 = x15 || x16
      val x20 = if (x17) {
        val x9 = List()
        x9
      } else {
        val x19 = List(x13)
        x19
      }
      val x25 = x20.flatMap { x21 =>
        val x22 = x21.name
        val x23 = new TutorialLinqSchema.Record { val name = x22 }
        val x24 = List(x23)
        x24
      }
      x25
    }
    x26
  }
}
/*****************************************
End of Generated Code
*******************************************/
