/*****************************************
Emitting Generated Code
*******************************************/
import lms.core.examples.TutorialLinqSchema
class Snippet extends ((Unit)=>(collection.immutable.List)) {
  def apply(x2:Unit): collection.immutable.List = {
    val x1 = TutorialLinqSchema.org
    val x3 = x1.departments
    val x143 = TutorialLinqSchema.org.departments.flatMap { x75 =>
      val x5 = x1.employees
      val x135 = TutorialLinqSchema.org.employees.flatMap { x106 =>
        val x77 = x75.dpt
        val x107 = x106.dpt
        val x108 = x77 == x107
        val x10 = x1.tasks
        val x128 = TutorialLinqSchema.org.tasks.flatMap { x119 =>
          val x110 = x106.emp
          val x120 = x119.emp
          val x121 = x110 == x120
          val x122 = x119.tsk
          val x123 = x122 == "abstract"
          val x124 = x121 && x123
          val x127 = if (x124) {
            val x30 = new TutorialLinqSchema.Record {  }
            val x31 = List(x30)
            x31
          } else {
            val x19 = List()
            x19
          }
          x127
        }
        val x129 = x128.isEmpty
        val x131 = x108 && x129
        val x134 = if (x131) {
          val x30 = new TutorialLinqSchema.Record {  }
          val x31 = List(x30)
          x31
        } else {
          val x19 = List()
          x19
        }
        x134
      }
      val x136 = x135.isEmpty
      val x142 = if (x136) {
        val x77 = x75.dpt
        val x140 = new TutorialLinqSchema.Record { val dpt = x77 }
        val x141 = List(x140)
        x141
      } else {
        val x19 = List()
        x19
      }
      x142
    }
    x143
  }
}
/*****************************************
End of Generated Code
*******************************************/
