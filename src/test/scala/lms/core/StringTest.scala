package lms.core

import lms.core.virtualize

import lms.core.{Dsl, DslDriver, TutorialFunSuite}

class StringTest extends TutorialFunSuite {

  val under = "virtualize/"

  test("const-string-len") {
    @virt
    object Snippet extends DslDriver[Boolean, Int] with Dsl {
      def snippet(x: Rep[Boolean]): Rep[Int] = {
        val s = "abc"
        s.length
      }
    }
    check("str-const-len", Snippet.code)
  }
  test("string-len") {
    @virt
    object Snippet extends DslDriver[String, Int] with Dsl {
      def snippet(x: Rep[String]): Rep[Int] = {
        x.length
      }
    }
    check("str-len", Snippet.code)
  }

  test("string-char-at-1") {
    @virt
    object Snippet extends DslDriver2[String, Int, Char] with Dsl {
      def snippet(x: Rep[String], i: Rep[Int]): Rep[Char] = {
        x(i)
      }
    }
    check("str-char-at-1", Snippet.code)
  }

  test("string-char-at-2") {
    @virt
    object Snippet extends DslDriver[String, Char] with Dsl {
      def snippet(x: Rep[String]): Rep[Char] = {
        x(1)
      }
    }
    check("str-char-at-2", Snippet.code)
  }

}
