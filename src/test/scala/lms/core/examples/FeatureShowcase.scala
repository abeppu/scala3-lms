package lms.core.examples

import lms.core.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

@virt
trait FeatureShowcaseSnippets extends Dsl {
  def scoreLine(text: Rep[String]): Rep[Int] = {
    var i: Var[Int] = 0
    var digits: Var[Int] = 0
    var vowels: Var[Int] = 0
    var other: Var[Int] = 0

    val score: Rep[Int] =
      if (text.length == 0) -1
      else if (text(0) == '!') 9000
      else {
        while (readVar(i) < text.length) {
          val idx = readVar(i)
          val ch = text(idx)
          ch match {
            case '0' | '1' =>
              digits = readVar(digits) + 1
            case 'a' | 'e' =>
              vowels = readVar(vowels) + 1
            case ',' =>
              ()
            case _ =>
              other = readVar(other) + 1
          }
          i = idx + 1
        }

        val counters = (readVar(digits), readVar(vowels))
        val (digitCount, vowelCount) = counters
        digitCount * 100 + vowelCount * 10 + readVar(other)
      }

    score
  }
}

class FeatureShowcaseTest extends AnyFunSuite with Matchers {
  private val compiler = new FeatureShowcaseSnippets with DslCompile

  private def hostScoreLine(text: String): Int = {
    var i = 0
    var digits = 0
    var vowels = 0
    var other = 0

    val score =
      if (text.isEmpty) -1
      else if (text(0) == '!') 9000
      else {
        while (i < text.length) {
          val idx = i
          val ch = text(idx)
          ch match {
            case '0' | '1' =>
              digits += 1
            case 'a' | 'e' =>
              vowels += 1
            case ',' =>
              ()
            case _ =>
              other += 1
          }
          i = idx + 1
        }

        val counters = (digits, vowels)
        val (digitCount, vowelCount) = counters
        digitCount * 100 + vowelCount * 10 + other
      }

    score
  }

  test("extended feature showcase compiles and preserves host behavior") {
    val f: compiler.Exp[String] => compiler.Exp[Int] = compiler.scoreLine(_)
    val staged = compiler.compile(f)

    List("", "ace101", "scala,lms", "rhythm", "!abort").foreach { input =>
      staged(input).shouldBe(hostScoreLine(input))
    }
  }
}
