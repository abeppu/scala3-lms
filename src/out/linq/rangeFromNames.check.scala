/*****************************************
Emitting Generated Code
*******************************************/
import lms.core.examples.TutorialLinqSchema
class Snippet extends ((Unit)=>(collection.immutable.List)) {
  def apply(x1:Unit): collection.immutable.List = {
    val x0 = TutorialLinqSchema.db
    val x2 = x0.people
    val x85 = TutorialLinqSchema.db.people.flatMap { x19 =>
      val x20 = x19.name
      val x21 = x20 == "Edna"
      val x84 = if (x21) {
        val x83 = TutorialLinqSchema.db.people.flatMap { x42 =>
          val x43 = x42.name
          val x44 = x43 == "Bert"
          val x82 = if (x44) {
            val x81 = TutorialLinqSchema.db.people.flatMap { x70 =>
              val x24 = x19.age
              val x71 = x70.age
              val x72 = x24 <= x71
              val x47 = x42.age
              val x73 = x71 < x47
              val x74 = x72 && x73
              val x80 = if (x74) {
                val x77 = x70.name
                val x78 = TutorialLinqSchema.Name(x77)
                val x79 = List(x78)
                x79
              } else {
                val x7 = List()
                x7
              }
              x80
            }
            x81
          } else {
            val x7 = List()
            x7
          }
          x82
        }
        x83
      } else {
        val x7 = List()
        x7
      }
      x84
    }
    x85
  }
}
/*****************************************
End of Generated Code
*******************************************/
