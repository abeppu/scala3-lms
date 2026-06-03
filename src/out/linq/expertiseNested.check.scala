/*****************************************
Emitting Generated Code
*******************************************/
import lms.core.examples.TutorialLinqSchema
class Snippet extends ((Unit)=>(collection.immutable.List)) {
  def apply(x2:Unit): collection.immutable.List = {
    val x1 = TutorialLinqSchema.org
    val x3 = x1.departments
    val x160 = TutorialLinqSchema.org.departments.flatMap { x101 =>
      val x11 = List()
      val x87 = new TutorialLinqSchema.Record {  }
      val x88 = List(x87)
      val x103 = x101.dpt
      val x5 = x1.employees
      val x133 = TutorialLinqSchema.org.employees.flatMap { x109 =>
        val x110 = x109.dpt
        val x111 = x103 == x110
        val x132 = if (x111) {
          val x115 = x109.emp
          val x19 = x1.tasks
          val x129 = TutorialLinqSchema.org.tasks.flatMap { x121 =>
            val x122 = x121.emp
            val x123 = x115 == x122
            val x128 = if (x123) {
              val x126 = x121.tsk
              val x127 = List(x126)
              x127
            } else {
              x11
            }
            x128
          }
          val x130 = new TutorialLinqSchema.Record { val emp = x115; val tasks = x129 }
          val x131 = List(x130)
          x131
        } else {
          x11
        }
        x132
      }
      val x134 = new TutorialLinqSchema.Record { val dpt = x103; val employees = x133 }
      val x136 = x134.employees
      val x150 = x136.flatMap { x137 =>
        val x138 = x137.tasks
        val x143 = x138.flatMap { x139 =>
          val x140 = x139 == "abstract"
          val x142 = if (x140) {
            val x141 = List(x139)
            x141
          } else {
            x11
          }
          x142
        }
        val x145 = x143.flatMap { x144 =>
          x88
        }
        val x146 = x145.isEmpty
        val x149 = if (x146) {
          val x148 = List(x137)
          x148
        } else {
          x11
        }
        x149
      }
      val x152 = x150.flatMap { x151 =>
        x88
      }
      val x153 = x152.isEmpty
      val x159 = if (x153) {
        val x156 = x134.dpt
        val x157 = new TutorialLinqSchema.Record { val dpt = x156 }
        val x158 = List(x157)
        x158
      } else {
        x11
      }
      x159
    }
    x160
  }
}
/*****************************************
End of Generated Code
*******************************************/
