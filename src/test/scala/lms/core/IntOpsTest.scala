package lms.core

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

@virt
trait IntOpSnippets extends Dsl {
  def bitPipeline(x: Rep[Int]): Rep[Int] = {
    val masked = (x & 15) | 2
    val shifted = (masked << 1) ^ (x >> 1)
    val rotated = shifted >>> 1
    (~rotated) % 17
  }
}

class IntOpsTest extends AnyFunSuite with Matchers {
  private val compiler = new IntOpSnippets with DslCompile

  private def hostBitPipeline(x: Int): Int = {
    val masked = (x & 15) | 2
    val shifted = (masked << 1) ^ (x >> 1)
    val rotated = shifted >>> 1
    (~rotated) % 17
  }

  test("virtualized staged int operators preserve host behavior") {
    val f: compiler.Exp[Int] => compiler.Exp[Int] = compiler.bitPipeline(_)
    val staged = compiler.compile(f)

    List(-7, -1, 0, 1, 5, 42).foreach { x =>
      staged(x) shouldBe hostBitPipeline(x)
    }
  }
}
