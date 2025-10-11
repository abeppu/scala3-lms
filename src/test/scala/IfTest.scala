package scala.lms.tests

import scala.lms.tests.TutorialFunSuite
import scala.lms.virtualize

class IfTest extends TutorialFunSuite {
  
  val under = "virtualize/"

  test("if-and-true") {
    @virtualize
    object Snippet extends DslDriver[Boolean, Int] with Dsl {
      def snippet(x: Rep[Boolean]): Rep[Int] = {
        if (x && true) { // I think this should just be 1?
            1
        } else {
            2
        }
      }
    }
    check("if-and-true", Snippet.code)
  }

    test("if-and-xy") {
    @virtualize
    object Snippet extends DslDriver2[Boolean, Boolean, Int] with Dsl {
      def snippet(x: Rep[Boolean],y: Rep[Boolean]): Rep[Int] = {
        if (x && y) { // I think this should just be 1?
            1
        } else {
            2
        }
      }
    }
    check("if-and-xy", Snippet.code)
  }
}
