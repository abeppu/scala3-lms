/*****************************************
Emitting Generated Code
*******************************************/
import lms.core.examples.TutorialLinqSchema
class Snippet extends ((Unit)=>(collection.immutable.List)) {
  def apply(x2:Unit): collection.immutable.List = {
    val x0 = TutorialLinqSchema.db
    val x3 = x0.people
    val x22 = TutorialLinqSchema.db.people.flatMap { x12 =>
      val x13 = x12.age
      val x14 = x13 % 2
      val x15 = x14 == 0
      val x21 = if (x15) {
        val x18 = x12.name
        val x19 = new TutorialLinqSchema.Record { val name = x18 }
        val x20 = List(x19)
        x20
      } else {
        val x9 = List()
        x9
      }
      x21
    }
    x22
  }
}
/*****************************************
End of Generated Code
*******************************************/
