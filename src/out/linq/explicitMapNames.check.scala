/*****************************************
Emitting Generated Code
*******************************************/
import lms.core.examples.TutorialLinqSchema
class Snippet extends ((Unit)=>(collection.immutable.List)) {
  def apply(x1:Unit): collection.immutable.List = {
    val x0 = TutorialLinqSchema.db
    val x2 = x0.people
    val x7 = TutorialLinqSchema.db.people.flatMap { x3 =>
      val x4 = x3.name
      val x5 = new TutorialLinqSchema.Record { val name = x4 }
      val x6 = List(x5)
      x6
    }
    x7
  }
}
/*****************************************
End of Generated Code
*******************************************/
