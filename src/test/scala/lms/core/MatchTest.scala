package lms.core

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

@virt
trait MatchSnippets extends Dsl {
  def literalMatch(x: Rep[Int]): Rep[Int] =
    x match {
      case 0 => 10
      case 1 => 20
      case _ => x + 30
    }

  def guardedAlternativeMatch(x: Rep[Int]): Rep[Int] =
    x match {
      case 0 if x < 0 => 99
      case 0 => 11
      case 1 | 2 => 12
      case _ => x
    }
}

class MatchTest extends AnyFunSuite with Matchers {
  private val compiler = new MatchSnippets with DslCompile

  test("virtualized literal match on staged scrutinee") {
    val f: compiler.Exp[Int] => compiler.Exp[Int] = compiler.literalMatch(_)
    val staged = compiler.compile(f)

    staged(0) shouldBe 10
    staged(1) shouldBe 20
    staged(7) shouldBe 37
  }

  test("virtualized match supports guards and alternatives") {
    val f: compiler.Exp[Int] => compiler.Exp[Int] = compiler.guardedAlternativeMatch(_)
    val staged = compiler.compile(f)

    staged(0) shouldBe 11
    staged(1) shouldBe 12
    staged(2) shouldBe 12
    staged(7) shouldBe 7
  }
}
