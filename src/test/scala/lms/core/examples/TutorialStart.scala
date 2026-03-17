package lms.core.examples

import lms.core.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.language.implicitConversions

@virt
trait TutorialStartSnippets extends Dsl {
  def hostConditional(x: Rep[Int]): Rep[Int] = {
    def compute(b: Boolean): Rep[Int] =
      if (b) 1 else x
    compute(true) + compute(1 == 1)
  }

  def stagedConditional(x: Rep[Int]): Rep[Int] = {
    def compute(b: Rep[Boolean]): Rep[Int] =
      if (b) 1 else x
    compute(x == 1)
  }

  def powerWithFunctionSquare(b: Rep[Int]): Rep[Int] = {
    def square: Rep[Int => Int] = doLambda[Int, Int] { x => x * x }

    def power(base: Rep[Int], n: Int): Rep[Int] =
      if (n == 0) 1
      else if (n % 2 == 0) square(power(base, n / 2))
      else base * power(base, n - 1)

    power(b, 7)
  }
}

class TutorialStartTest extends AnyFunSuite with Matchers {
  private val compiler = new TutorialStartSnippets with DslCompile

  test("host conditional stays in the first stage") {
    val f: compiler.Exp[Int] => compiler.Exp[Int] = compiler.hostConditional(_)
    val staged = compiler.compile(f)

    staged(0) shouldBe 2
    staged(7) shouldBe 2
  }

  test("staged conditional remains dynamic") {
    val f: compiler.Exp[Int] => compiler.Exp[Int] = compiler.stagedConditional(_)
    val staged = compiler.compile(f)

    staged(1) shouldBe 1
    staged(2) shouldBe 2
  }

  test("generated code contains a dynamic if for staged conditionals") {
    @virt
    object Snippet extends DslDriver[Int, Int] with Dsl {
      def snippet(x: Rep[Int]): Rep[Int] = {
        def compute(b: Rep[Boolean]): Rep[Int] =
          if (b) 1 else x
        compute(x == 1)
      }
    }

    Snippet.code should include("if (")
  }

  test("staged function values work in power example") {
    @virt
    object Snippet extends DslDriver[Int, Int] with Dsl with TutorialStartSnippets {
      def snippet(x: Rep[Int]): Rep[Int] = powerWithFunctionSquare(x)
    }

    Snippet.code should include("=>")
  }
}
