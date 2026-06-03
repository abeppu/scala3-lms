/*****************************************
Emitting Generated Code
*******************************************/
import lms.core.examples.TutorialLinqSchema
class Snippet extends ((Unit)=>(collection.immutable.List)) {
  def apply(x2:Unit): collection.immutable.List = {
    val x1 = TutorialLinqSchema.org
    val x3 = x1.departments
    val x42 = TutorialLinqSchema.org.departments.flatMap { x4 =>
      val x7 = x4.dpt
      val x5 = x1.employees
      val x39 = TutorialLinqSchema.org.employees.flatMap { x14 =>
        val x15 = x14.dpt
        val x16 = x7 == x15
        val x38 = if (x16) {
          val x21 = x14.emp
          val x19 = x1.tasks
          val x35 = TutorialLinqSchema.org.tasks.flatMap { x27 =>
            val x28 = x27.emp
            val x29 = x21 == x28
            val x34 = if (x29) {
              val x32 = x27.tsk
              val x33 = List(x32)
              x33
            } else {
              val x11 = List()
              x11
            }
            x34
          }
          val x36 = new TutorialLinqSchema.Record { val emp = x21; val tasks = x35 }
          val x37 = List(x36)
          x37
        } else {
          val x11 = List()
          x11
        }
        x38
      }
      val x40 = new TutorialLinqSchema.Record { val dpt = x7; val employees = x39 }
      val x41 = List(x40)
      x41
    }
    x42
  }
}
/*****************************************
End of Generated Code
*******************************************/
