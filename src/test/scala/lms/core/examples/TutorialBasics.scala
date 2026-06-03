package lms.core.examples

import lms.core.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.language.implicitConversions

@virt
trait TutorialBasicsSnippets extends Dsl {
  def stagedPower(base: Rep[Double], n: Int): Rep[Double] =
    if (n == 0) 1.0
    else base * stagedPower(base, n - 1)

  def stagedPowerWithSharedBase(x: Rep[Double]): Rep[Double] = {
    // Keep the dynamic base in a local val so generated code can reuse one binding.
    val base = x + 1.0
    stagedPower(base, 4)
  }
}

class TutorialBasicsTest extends AnyFunSuite with Matchers {
  private val compiler = new TutorialBasicsSnippets with DslCompile

  test("basics power example computes expected staged result") {
    val f: compiler.Exp[Double] => compiler.Exp[Double] = compiler.stagedPowerWithSharedBase(_)
    val staged = compiler.compile(f)

    List(-2.0, 0.0, 1.5).foreach { x =>
      staged(x).shouldBe(math.pow(x + 1.0, 4))
    }
  }

  test("basics power example emits multiplication chain") {
    @virt
    object Snippet extends DslDriver[Double, Double] with Dsl with TutorialBasicsSnippets {
      def snippet(x: Rep[Double]): Rep[Double] = stagedPowerWithSharedBase(x)
    }

    Snippet.code should include("*")
  }
}
