package scala.lms
package tests

import scala.lms.tests.TutorialFunSuite
import scala.lms.virtualize


class BoolTest extends TutorialFunSuite {
  val under = "virtualize/"

  test("boolean-or-rewrite") {
    @virtualize
    object Snippet extends DslDriver[Boolean, Boolean] with Dsl {
      def snippet(x: Rep[Boolean]): Rep[Boolean] = {
        x || false
      }
    }
    check("boolean-or-false", Snippet.code)
  }

  test("boolean-or-handwritten") {
    @virtualize
    object Snippet extends DslDriver[Boolean, Boolean] with Dsl {
      def snippet(x: Rep[Boolean]): Rep[Boolean] = {
        boolean_or(x, unit(false))
      }
    }
    check("boolean-or-false", Snippet.code)
  }

    test("boolean-or-rewrite2") {
    @virtualize
    object Snippet extends DslDriver[Boolean, Boolean] with Dsl {
      def snippet(x: Rep[Boolean]): Rep[Boolean] = {
        x || x
      }
    }
    check("boolean-or-self", Snippet.code)
  }

    test("boolean-or-handwritten2") {
    @virtualize
    object Snippet extends DslDriver[Boolean, Boolean] with Dsl {
      def snippet(x: Rep[Boolean]): Rep[Boolean] = {
        boolean_or(x, x)
      }
    }
    check("boolean-or-self", Snippet.code)
  }

  test("boolean-or-consts") {
    @virtualize
    object Snippet extends DslDriver[Boolean, Boolean] with Dsl {
      def snippet(x: Rep[Boolean]): Rep[Boolean] = {
        boolean_or(false, true)
      }
    }
    check("boolean-or-consts", Snippet.code)
  }

  test("boolean-and-handwritten1") {
    @virtualize
    object Snippet extends DslDriver[Boolean, Boolean] with Dsl {
      def snippet(x: Rep[Boolean]): Rep[Boolean] = {
        boolean_and(x, x) //unit(false))
      }
    }
    check("boolean-and-self", Snippet.code)
  }

  test("boolean-and-handwritten2") {
    @virtualize
    object Snippet extends DslDriver[Boolean, Boolean] with Dsl {
      def snippet(x: Rep[Boolean]): Rep[Boolean] = {
        boolean_and(x, unit(false))
      }
    }
    check("boolean-and-false", Snippet.code)
  }

  test("boolean-and-rewrite") {
    @virtualize
     object Snippet extends DslDriver[Boolean, Boolean] with Dsl {
       def snippet(x: Rep[Boolean]): Rep[Boolean] = {
         x && x
       }
     }
     check("boolean-and-self", Snippet.code)
  }

  test("boolean-and-rewrite2") {
    @virtualize
    object Snippet extends DslDriver[Boolean, Boolean] with Dsl {
      def snippet(y: Rep[Boolean]): Rep[Boolean] = {
        y && false //
      }
    }
    check("boolean-and-false", Snippet.code)
  }

}
