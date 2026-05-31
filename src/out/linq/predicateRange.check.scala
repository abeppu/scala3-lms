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
      val x15 = x13 < 40
      val x16 = x14 && x15
      val x22 = if (x16) {
        val x19 = x12.name
        val x20 = new TutorialLinqSchema.Record { val name = x19 }
        val x21 = List(x20)
        x21
      } else {
        val x9 = List()
        x9
      }
      x22
    }
    x23
  }
}
/*****************************************
End of Generated Code
*******************************************/
