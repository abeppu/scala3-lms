/*****************************************
Emitting Generated Code
*******************************************/
import lms.core.examples.TutorialLinqSchema
class Snippet extends ((Unit)=>(collection.immutable.List)) {
  def apply(x2:Unit): collection.immutable.List = {
    val x0 = TutorialLinqSchema.db
    val x3 = x0.people
    val x86 = TutorialLinqSchema.db.people.flatMap { x20 =>
      val x21 = x20.name
      val x22 = x21 == "Edna"
      val x85 = if (x22) {
        val x84 = TutorialLinqSchema.db.people.flatMap { x43 =>
          val x44 = x43.name
          val x45 = x44 == "Bert"
          val x83 = if (x45) {
            val x82 = TutorialLinqSchema.db.people.flatMap { x71 =>
              val x25 = x20.age
              val x72 = x71.age
              val x73 = x25 <= x72
              val x48 = x43.age
              val x74 = x72 < x48
              val x75 = x73 && x74
              val x81 = if (x75) {
                val x78 = x71.name
                val x79 = new TutorialLinqSchema.Record { val name = x78 }
                val x80 = List(x79)
                x80
              } else {
                val x8 = List()
                x8
              }
              x81
            }
            x82
          } else {
            val x8 = List()
            x8
          }
          x83
        }
        x84
      } else {
        val x8 = List()
        x8
      }
      x85
    }
    x86
  }
}
/*****************************************
End of Generated Code
*******************************************/
