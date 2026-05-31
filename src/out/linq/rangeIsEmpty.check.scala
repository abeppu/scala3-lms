/*****************************************
Emitting Generated Code
*******************************************/
import lms.core.examples.TutorialLinqSchema
class Snippet extends ((Unit)=>(Boolean)) {
  def apply(x2:Unit): Boolean = {
    val x0 = TutorialLinqSchema.db
    val x3 = x0.people
    val x24 = TutorialLinqSchema.db.people.flatMap { x13 =>
      val x14 = x13.age
      val x15 = 90 <= x14
      val x16 = x14 < 100
      val x17 = x15 && x16
      val x23 = if (x17) {
        val x20 = x13.name
        val x21 = new TutorialLinqSchema.Record { val name = x20; val age = x14 }
        val x22 = List(x21)
        x22
      } else {
        val x10 = List()
        x10
      }
      x23
    }
    val x25 = x24.isEmpty
    x25
  }
}
/*****************************************
End of Generated Code
*******************************************/
