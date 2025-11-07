package lms.core

import lms.core.virt

import lms.core.TutorialFunSuite


class BoolTest extends TutorialFunSuite {
  val under = "virtualize/"
  test("boolean-not") {
    @virt
    object Snippet extends DslDriver[Boolean, Boolean] with Dsl {
      def snippet(x: Rep[Boolean]): Rep[Boolean] = {
        !x
      }
    }
    check("boolean-not", Snippet.code)
  }

  test("boolean-double-not") {
    @virt
    object Snippet extends DslDriver[Boolean, Boolean] with Dsl {
      def snippet(x: Rep[Boolean]): Rep[Boolean] = {
        !(!x)
      }
    }
    check("boolean-double-not", Snippet.code)
  }


  test("const-not") {
    @virt
    object Snippet extends DslDriver[Boolean, Boolean] with Dsl {
      def snippet(x: Rep[Boolean]): Rep[Boolean] = {
        !true
      }
    }
    check("const-not", Snippet.code)
  }

  test("const-double-not") {
    @virt
    object Snippet extends DslDriver[Boolean, Boolean] with Dsl {
      def snippet(x: Rep[Boolean]): Rep[Boolean] = {
        !(!true)
      }
    }
    check("const-double-not", Snippet.code)
  }


  test("boolean-or-rewrite") {
    @virt
    object Snippet extends DslDriver[Boolean, Boolean] with Dsl {
      def snippet(x: Rep[Boolean]): Rep[Boolean] = {
        x || false
      }
    }
    check("boolean-or-false", Snippet.code)
  }

  test("boolean-or-handwritten") {
    @virt
    object Snippet extends DslDriver[Boolean, Boolean] with Dsl {
      def snippet(x: Rep[Boolean]): Rep[Boolean] = {
        boolean_or(x, unit(false))
      }
    }
    check("boolean-or-false", Snippet.code)
  }

    test("boolean-or-rewrite2") {
    @virt
    object Snippet extends DslDriver[Boolean, Boolean] with Dsl {
      def snippet(x: Rep[Boolean]): Rep[Boolean] = {
        x || x
      }
    }
    check("boolean-or-self", Snippet.code)
  }

    test("boolean-or-handwritten2") {
    @virt
    object Snippet extends DslDriver[Boolean, Boolean] with Dsl {
      def snippet(x: Rep[Boolean]): Rep[Boolean] = {
        boolean_or(x, x)
      }
    }
    check("boolean-or-self", Snippet.code)
  }

  test("boolean-or-consts") {
    @virt
    object Snippet extends DslDriver[Boolean, Boolean] with Dsl {
      def snippet(x: Rep[Boolean]): Rep[Boolean] = {
        boolean_or(false, true)
      }
    }
    check("boolean-or-consts", Snippet.code)
  }

  test("boolean-and-handwritten1") {
    @virt
    object Snippet extends DslDriver[Boolean, Boolean] with Dsl {
      def snippet(x: Rep[Boolean]): Rep[Boolean] = {
        boolean_and(x, x) //unit(false))
      }
    }
    check("boolean-and-self", Snippet.code)
  }

  test("boolean-and-handwritten2") {
    @virt
    object Snippet extends DslDriver[Boolean, Boolean] with Dsl {
      def snippet(x: Rep[Boolean]): Rep[Boolean] = {
        boolean_and(x, unit(false))
      }
    }
    check("boolean-and-false", Snippet.code)
  }

  test("boolean-and-rewrite") {
    @virt
     object Snippet extends DslDriver[Boolean, Boolean] with Dsl {
       def snippet(x: Rep[Boolean]): Rep[Boolean] = {
         x && x
       }
     }
     check("boolean-and-self", Snippet.code)
  }

  test("boolean-and-rewrite2") {
    @virt
    object Snippet extends DslDriver[Boolean, Boolean] with Dsl {
      def snippet(y: Rep[Boolean]): Rep[Boolean] = {
        y && false //
      }
    }
    check("boolean-and-false", Snippet.code)
  }

}
