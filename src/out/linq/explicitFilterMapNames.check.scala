/*****************************************
Emitting Generated Code
*******************************************/
import lms.core.examples.TutorialLinqSchema
class Snippet extends ((Unit)=>(collection.immutable.List)) {
  def apply(x2:Unit): collection.immutable.List = {
    val x0 = TutorialLinqSchema.db
    val x3 = x0.people
    val x20 = TutorialLinqSchema.db.people.flatMap { x11 =>
      val x12 = x11.age
      val x13 = x12 < 40
      val x19 = if (x13) {
        val x16 = x11.name
        val x17 = new TutorialLinqSchema.Record { val name = x16 }
        val x18 = List(x17)
        x18
      } else {
        val x8 = List()
        x8
      }
      x19
    }
    x20
  }
}
/*****************************************
End of Generated Code
*******************************************/
