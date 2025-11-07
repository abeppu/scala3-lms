package lms.core

import lms.core.virtualize

import lms.core.TutorialFunSuite

class IfTest extends TutorialFunSuite {
  
  val under = "virtualize/"

  test("if-and-true") {
    @virt
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

  test("if-elseif-else") {
    @virtualize
    object Snippet extends DslDriver[Int, Int] with Dsl {
      def snippet(x: Rep[Int]): Rep[Int] = {
        if (x < 5) { // I think this should just be 1?
            1
        } else if (x >=5 && x < 10) {
            2
        } else {
            3
        }
      }
    }
    check("if-elseif-else", Snippet.code)
  }

  test("if-const") {
    @virtualize
    object Snippet extends DslDriver[Int, Int] with Dsl {
      def snippet(x: Rep[Int]): Rep[Int] = {
        if (true) { // I think this should just be 1?
          x
        } else {
          x - 1
        }
      }
    }
    check("if-const", Snippet.code)
  }

  test("if-var") {
    @virtualize
    object Snippet extends DslDriver[Boolean, Int] with Dsl {
      def snippet(x: Rep[Boolean]): Rep[Int] = {
        if (x) { // I think this should just be 1?
          1
        } else {
          2
        }
      }
    }
    check("if-var", Snippet.code)
  }


}
