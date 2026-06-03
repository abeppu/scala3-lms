/*****************************************
Emitting Generated Code
*******************************************/
import lms.core.examples.TutorialLinqSchema
class Snippet extends ((Unit)=>(collection.immutable.List)) {
  def apply(x2:Unit): collection.immutable.List = {
    val x0 = TutorialLinqSchema.db
    val x3 = x0.people
    val x8 = TutorialLinqSchema.db.people.flatMap { x4 =>
      val x5 = x4.name
      val x6 = new TutorialLinqSchema.Record { val name = x5 }
      val x7 = List(x6)
      x7
    }
    x8
  }
}
/*****************************************
End of Generated Code
*******************************************/
