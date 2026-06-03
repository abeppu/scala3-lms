/*****************************************
Emitting Generated Code
*******************************************/
class Snippet extends ((Int)=>(Int)) {
  def apply(x0:Int): Int = {
    val x1 = x0 < 5
    val x6 = if (x1) {
      1
    } else {
      val x2 = x0 >= 5
      val x3 = x0 < 10
      val x4 = x2 && x3
      val x5 = if (x4) {
        2
      } else {
        3
      }
      x5
    }
    x6
  }
}
/*****************************************
End of Generated Code
*******************************************/
