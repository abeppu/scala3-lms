package lms.core.examples

import lms.core.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.language.implicitConversions

class TutorialShonanTest extends AnyFunSuite with Matchers {
  private val matrix = Array(
    Array(1, 1, 1, 1, 1),
    Array(0, 0, 0, 0, 0),
    Array(0, 0, 1, 0, 0),
    Array(0, 0, 0, 0, 0),
    Array(0, 0, 1, 0, 1)
  )

  test("baseline matrix-vector product on static data") {
    def matrixVectorProd(a: Array[Array[Int]], v: Array[Int]): Array[Int] = {
      val n = a.length
      val out = new Array[Int](n)
      for (i <- 0 until n)
        for (j <- 0 until n)
          out(i) = out(i) + a(i)(j) * v(j)
      out
    }

    matrixVectorProd(matrix, Array.fill(5)(1)).toSeq shouldBe Seq(5, 0, 1, 0, 2)
  }

  test("static conditional disappears from generated code") {
    @virt
    object Snippet extends DslDriver[Array[Int], Array[Int]] with Dsl {
      def snippet(v: Rep[Array[Int]]): Rep[Array[Int]] = {
        val x = 100 - 5
        if (x > 10) {
          println("hello")
        }
        v
      }
    }

    Snippet.code should not include "if ("
  }

  test("dynamic conditional remains in generated code") {
    @virt
    object Snippet extends DslDriver[Array[Int], Array[Int]] with Dsl {
      def snippet(v: Rep[Array[Int]]): Rep[Array[Int]] = {
        val x = 100 - v.length
        if (x > 10) {
          println("hello")
        }
        v
      }
    }

    Snippet.code should include("if (")
  }

  test("mixed static and dynamic loop choices generate code") {
    @virt
    object Snippet extends DslDriver[Array[Int], Array[Int]] with Dsl {
      def snippet(v: Rep[Array[Int]]): Rep[Array[Int]] = {
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
              for (j <- (0.until(n)): Rep[Range]) {
                out(i) = out(i) + a(i)(j) * v(j)
              }
            }
          }
          out
        }

        matrixVectorProd(matrix, v)
      }
    }

    Snippet.code should include("while (")
  }
}
