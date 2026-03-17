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

  def dispatchExample(opcode: Rep[Int]): Rep[Int] = {
    val normalized: Rep[Int] =
      opcode match {
        case 0 => 100
        case 1 | 2 => opcode + 10
        case 3 if opcode < 5 => 30
        case 4 | 5 if opcode == 4 => 40
        case _ => opcode * 2
      }

    val routed: Rep[Int] = (normalized - 10) match {
      case 20 => normalized + 200
      case 30 | 90 => normalized + 300
      case _ => normalized
    }
    routed
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

  test("virtualized match can drive a staged dispatch pipeline") {
    val f: compiler.Exp[Int] => compiler.Exp[Int] = compiler.dispatchExample(_)
    val staged = compiler.compile(f)

    staged(0) shouldBe 400
    staged(1) shouldBe 11
    staged(2) shouldBe 12
    staged(3) shouldBe 230
    staged(4) shouldBe 340
    staged(7) shouldBe 14
  }
}
