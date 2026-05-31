/*****************************************
Emitting Generated Code
*******************************************/
import lms.core.examples.TutorialLinqSchema
class Snippet extends ((Unit)=>(collection.immutable.List)) {
  def apply(x1:Unit): collection.immutable.List = {
    val x0 = TutorialLinqSchema.db
    val x2 = x0.people
    val x19 = TutorialLinqSchema.db.people.flatMap { x10 =>
      val x11 = x10.age
      val x12 = x11 < 40
      val x18 = if (x12) {
        val x15 = x10.name
        val x16 = new TutorialLinqSchema.Record { val name = x15 }
        val x17 = List(x16)
        x17
      } else {
        val x7 = List()
        x7
      }
      x18
    }
    x19
  }
}
/*****************************************
End of Generated Code
*******************************************/
