package lms.core

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

@virt
trait TryCatchSnippets extends Dsl {
  def catchFatal(flag: Rep[Boolean]): Rep[Int] =
    try {
      if (flag) fatal("boom")
      10
    } catch {
      case _: Exception => 20
    }

  def catchPreservesWrites(flag: Rep[Boolean]): Rep[Int] = {
    var acc = 0
    try {
      acc = 5
      if (flag) fatal("boom")
      acc = 7
      readVar(acc)
    } catch {
      case _: Exception => readVar(acc) + 100
    }
  }

  def multiCatch(flag: Rep[Int]): Rep[Int] =
    try {
      fatal("boom")
      flag + 1
    } catch {
      case _: IllegalArgumentException => 40
      case _: Exception => 50
    }

  def throwSyntaxException(flag: Rep[Boolean]): Rep[Int] =
    try {
      if (flag) throw new Exception("boom")
      10
    } catch {
      case _: Exception => 20
    }

  def throwSyntaxIllegalArgument(flag: Rep[Boolean]): Rep[Int] =
    try {
      if (flag) throw new IllegalArgumentException("bad")
      10
    } catch {
      case _: IllegalArgumentException => 30
      case _: Exception => 40
    }
}

class TryCatchTest extends AnyFunSuite with Matchers {
  private val compiler = new TryCatchSnippets with DslCompile

  test("virtualized try/catch catches staged fatal exceptions") {
    val f: compiler.Exp[Boolean] => compiler.Exp[Int] = compiler.catchFatal(_)
    val staged = compiler.compile(f)

    staged(false) shouldBe 10
    staged(true) shouldBe 20
  }

  test("virtualized try/catch preserves writes that happen before the throw") {
    val f: compiler.Exp[Boolean] => compiler.Exp[Int] = compiler.catchPreservesWrites(_)
    val staged = compiler.compile(f)

    staged(false) shouldBe 7
    staged(true) shouldBe 105
  }

  test("virtualized try/catch supports multiple typed catch clauses") {
    val f: compiler.Exp[Int] => compiler.Exp[Int] = compiler.multiCatch(_)
    val staged = compiler.compile(f)

    staged(0) shouldBe 50
    staged(-1) shouldBe 50
    staged(3) shouldBe 50
  }

  test("virtualized throw syntax supports new Exception(msg)") {
    val f: compiler.Exp[Boolean] => compiler.Exp[Int] = compiler.throwSyntaxException(_)
    val staged = compiler.compile(f)

    staged(false) shouldBe 10
    staged(true) shouldBe 20
  }

  test("virtualized throw syntax preserves thrown subclass types") {
    val f: compiler.Exp[Boolean] => compiler.Exp[Int] = compiler.throwSyntaxIllegalArgument(_)
    val staged = compiler.compile(f)

    staged(false) shouldBe 10
    staged(true) shouldBe 30
  }
}
