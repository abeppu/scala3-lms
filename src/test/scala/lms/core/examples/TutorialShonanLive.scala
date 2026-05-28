package lms.core.examples

import lms.core.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.language.implicitConversions

class TutorialShonanLiveTest extends AnyFunSuite with Matchers {
  private val matrix = Array(
    Array(1, 1, 1, 1, 1),
    Array(0, 0, 0, 0, 0),
    Array(0, 0, 1, 0, 0),
    Array(0, 0, 0, 0, 0),
    Array(0, 0, 1, 0, 1)
  )

  private def hostMatrixVectorProd(a: Array[Array[Int]], v: Array[Int]): Array[Int] = {
    val n = a.length
    val out = new Array[Int](n)
    for (i <- 0 until n)
      for (j <- 0 until n)
        out(i) = out(i) + a(i)(j) * v(j)
    out
  }

  test("shonan-live host baseline matches expected vector") {
    hostMatrixVectorProd(matrix, Array(1, 1, 1, 1, 1)).toSeq shouldBe Seq(5, 0, 1, 0, 2)
  }

  test("shonan-live style staged kernel emits mixed static and dynamic loop structure") {
    @virt
    object Snippet extends DslDriver[Array[Int], Array[Int]] with Dsl {
      def matrixVectorProd(a0: Array[Array[Int]], v: Rep[Array[Int]]): Rep[Array[Int]] = {
        val n = a0.length
        val a = staticData(a0)
        val out = NewArray[Int](n)

        for (i <- (0.until(n)): Range) {
          val sparse = a0(i).count(_ != 0) < 3
          if (sparse) {
            for (j <- (0.until(n)): Range) {
              out(i) = out(i) + a(i)(j) * v(j)
            }
          } else {
            var j: Var[Int] = 0
            while (j < n) {
              val idx = j
              out(i) = out(i) + a(i)(idx) * v(idx)
              j = idx + 1
            }
          }
        }
        out
      }

      def snippet(v: Rep[Array[Int]]): Rep[Array[Int]] =
        matrixVectorProd(matrix, v)
    }

    Snippet.code should include("while (")
    Snippet.code should include("new Array[Int](5)")
    Snippet.code should include("x0")
  }
}
