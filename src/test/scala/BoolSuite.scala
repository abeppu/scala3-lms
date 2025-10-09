package scala.lms
package tests

import scala.lms.tests.TutorialFunSuite
import scala.lms.virtualize

@virtualize
class BoolSuite extends TutorialFunSuite {
  val under = "virtualize/"

  test("boolean-and-handwritten") {
    object Snippet extends DslDriver[Boolean, Boolean] with Dsl {
      def snippet(x: Rep[Boolean]): Rep[Boolean] = {
        boolToBoolRep(boolean_and(x, x)) //unit(false))
      }

      check("boolean-and-handwritten", Snippet.code)
    }
  }

  test("boolean-and-rewrite") {
     object Snippet extends DslDriver[Boolean, Boolean] with Dsl {
       def snippet(x: Rep[Boolean]): Rep[Boolean] = {
         x && x
       }

       check("boolean-and-rewrite", Snippet.code)
     }
  }
}
