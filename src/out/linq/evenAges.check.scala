/*****************************************
Emitting Generated Code
*******************************************/
import lms.core.examples.TutorialLinqSchema
class Snippet extends ((Unit)=>(collection.immutable.List)) {
  def apply(x1:Unit): collection.immutable.List = {
    val x0 = TutorialLinqSchema.db
    val x2 = x0.people
    val x21 = TutorialLinqSchema.db.people.flatMap { x11 =>
      val x12 = x11.age
      val x13 = x12 % 2
      val x14 = x13 == 0
      val x20 = if (x14) {
        val x17 = x11.name
        val x18 = new TutorialLinqSchema.Record { val name = x17 }
        val x19 = List(x18)
        x19
      } else {
        val x8 = List()
        x8
      }
      x20
    }
    x21
  }
}
/*****************************************
End of Generated Code
*******************************************/
