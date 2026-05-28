package lms.core

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

@virt
trait PrimitiveNumericSnippets extends Dsl {
  def doublePipeline(x: Rep[Double]): Rep[Double] = {
    val shifted = (x + 1.5) * 2.0
    (shifted - x) / 2.0
  }

  def floatPipeline(x: Rep[Float]): Rep[Float] = {
    val shifted = (x + 1.5f) * 2.0f
    (shifted - x) / 2.0f
  }

  def longPipeline(x: Rep[Long]): Rep[Long] = {
    val shifted = (x + 3L) * 2L
    (shifted - x) / 2L
  }

  def longBitwisePipeline(x: Rep[Long]): Rep[Long] = {
    val y = 0x55L
    val mixed = (x & y) ^ (x | y)
    (mixed << 1) >> 1
  }

  def floatToDoublePipeline(x: Rep[Float]): Rep[Double] = {
    val widened: Rep[Double] = x.toDouble
    widened + 1.25
  }
}

class PrimitiveNumericOpsTest extends AnyFunSuite with Matchers {
  private val compiler = new PrimitiveNumericSnippets with DslCompile

  test("virtualized staged double arithmetic preserves host behavior") {
    val f: compiler.Exp[Double] => compiler.Exp[Double] = compiler.doublePipeline(_)
    val staged = compiler.compile(f)

    List(-3.0, 0.0, 2.5).foreach { x =>
      staged(x).shouldBe(((x + 1.5) * 2.0 - x) / 2.0)
    }
  }

  test("virtualized staged float arithmetic preserves host behavior") {
    val f: compiler.Exp[Float] => compiler.Exp[Float] = compiler.floatPipeline(_)
    val staged = compiler.compile(f)

    List(-3.0f, 0.0f, 2.5f).foreach { x =>
      staged(x).shouldBe(((x + 1.5f) * 2.0f - x) / 2.0f)
    }
  }

  test("virtualized staged long arithmetic preserves host behavior") {
    val f: compiler.Exp[Long] => compiler.Exp[Long] = compiler.longPipeline(_)
    val staged = compiler.compile(f)

    List(-3L, 0L, 25L).foreach { x =>
      staged(x).shouldBe(((x + 3L) * 2L - x) / 2L)
    }
  }

  test("virtualized staged long bitwise and shift operators preserve host behavior") {
    val f: compiler.Exp[Long] => compiler.Exp[Long] = compiler.longBitwisePipeline(_)
    val staged = compiler.compile(f)

    val inputs = List(0x0FL, 0x1234L, -7L)
    inputs.foreach { x =>
      staged(x).shouldBe((((x & 0x55L) ^ (x | 0x55L)) << 1 >> 1))
    }
  }

  test("virtualized staged float to double promotion uses toDouble path") {
    val f: compiler.Exp[Float] => compiler.Exp[Double] = compiler.floatToDoublePipeline(_)
    val staged = compiler.compile[Float, Double](f)

    List(-3.0f, 0.0f, 2.5f).foreach { x =>
      staged(x).shouldBe(x.toDouble + 1.25)
    }
  }
}
