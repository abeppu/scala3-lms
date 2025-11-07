package lms.core

import lms.core.virtualize
import lms.legacy.common.*


class ManifestTest extends TutorialFunSuite {
  val under = "manifest/"

  test("nested arrays") {
    @virt
    object Snippet extends DslDriver[Array[Array[Int]], Int] with Dsl {
      def snippet(x: Rep[Array[Array[Int]]]): Rep[Int] = {
        1
      }
    }
    check("nested-array-formatting", Snippet.code)
  }
}
