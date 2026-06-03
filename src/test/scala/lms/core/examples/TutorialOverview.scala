package lms.core.examples

import lms.core.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.language.implicitConversions

@virt
trait TutorialOverviewSnippets extends Dsl {
  def hostVsStaged(x: Rep[Int]): Rep[Int] = {
    val hostChosen =
      if (1 + 1 == 2) 40
      else 0

    val stagedChosen =
      if (x < 0) x * 2
      else x + 2

    hostChosen + stagedChosen
  }
}

class TutorialOverviewTest extends AnyFunSuite with Matchers {
  private val compiler = new TutorialOverviewSnippets with DslCompile

  test("overview example preserves host choices and stages dynamic ones") {
    val f: compiler.Exp[Int] => compiler.Exp[Int] = compiler.hostVsStaged(_)
    val staged = compiler.compile(f)

    staged(-3) shouldBe 34
    staged(5) shouldBe 47
  }

  test("overview example emits dynamic if in generated code") {
    @virt
    object Snippet extends DslDriver[Int, Int] with Dsl with TutorialOverviewSnippets {
      def snippet(x: Rep[Int]): Rep[Int] = hostVsStaged(x)
    }

    Snippet.code should include("if (")
  }
}
