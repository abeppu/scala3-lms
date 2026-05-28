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

  def guardedCatch(x: Rep[Int]): Rep[Int] =
    try {
      if (x < 0) throw new Exception("boom")
      10
    } catch {
      case err: Exception if x == -1 => 20
      case _: Exception => 30
    }

  def throwSyntaxRuntimeException(flag: Rep[Boolean]): Rep[Int] =
    try {
      if (flag) throw new RuntimeException("bad")
      10
    } catch {
      case _: RuntimeException => 60
      case _: Exception => 70
    }

  def throwNoArgException(flag: Rep[Boolean]): Rep[Int] =
    try {
      if (flag) throw new Exception()
      10
    } catch {
      case _: Exception => 80
    }

  def throwNoArgRuntimeException(flag: Rep[Boolean]): Rep[Int] =
    try {
      if (flag) throw new RuntimeException()
      10
    } catch {
      case _: RuntimeException => 90
      case _: Exception => 100
    }

  def throwSyntaxWithCause(flag: Rep[Boolean]): Rep[Int] =
    try {
      if (flag) throw new RuntimeException("outer", new IllegalArgumentException("inner"))
      10
    } catch {
      case _: RuntimeException => 110
      case _: Exception => 120
    }

  def throwSyntaxWithNoArgCause(flag: Rep[Boolean]): Rep[Int] =
    try {
      if (flag) throw new RuntimeException("outer", new IllegalArgumentException())
      10
    } catch {
      case _: RuntimeException => 130
      case _: Exception => 140
    }

  def throwSyntaxCauseOnly(flag: Rep[Boolean]): Rep[Int] =
    try {
      if (flag) throw new RuntimeException(new IllegalArgumentException("inner"))
      10
    } catch {
      case _: RuntimeException => 150
      case _: Exception => 160
    }

  def throwSyntaxCauseOnlyNoArg(flag: Rep[Boolean]): Rep[Int] =
    try {
      if (flag) throw new RuntimeException(new IllegalArgumentException())
      10
    } catch {
      case _: RuntimeException => 170
      case _: Exception => 180
    }

  def finallyOnNormalPath(x: Rep[Int]): Rep[Int] = {
    var side = 0
    try {
      side = 3
    } finally {
      side = side + 10
    }
    readVar(side)
  }

  def finallyOnThrowPath(flag: Rep[Boolean]): Rep[Int] = {
    var side = 0
    try {
      side = 10
      if (flag) throw new Exception("boom")
      side = 11
    } catch {
      case _: Exception => side = 20
    } finally {
      side = side + 100
    }
    readVar(side)
  }

  def stagedReturn(x: Rep[Int]): Rep[Int] = {
    if (x < 0) return 7
    x + 1
  }

  def finallyValueFeedsArithmetic(x: Rep[Int]): Rep[Int] = {
    var side = 0
    (try {
      side = 1
      x + 1
    } finally {
      side = side + 10
    }) * 100 + readVar(side)
  }

  def finallyValueViaLocal(x: Rep[Int]): Rep[Int] = {
    var side = 0
    val result =
      try {
        side = 1
        x + 1
      } finally {
        side = side + 10
      }
    result * 100 + readVar(side)
  }

  def tryCatchFinallyValueFeedsArithmetic(flag: Rep[Boolean]): Rep[Int] = {
    var side = 0
    (try {
      side = 1
      if (flag) throw new Exception("boom")
      10
    } catch {
      case _: Exception => 20
    } finally {
      side = side + 10
    }) * 100 + readVar(side)
  }

  def tryCatchFinallyValueViaLocal(flag: Rep[Boolean]): Rep[Int] = {
    var side = 0
    val result =
      try {
        side = 1
        if (flag) throw new Exception("boom")
        10
      } catch {
        case _: Exception => 20
      } finally {
        side = side + 10
      }
    result * 100 + readVar(side)
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

  test("virtualized try/catch supports staged catch guards and unused binders") {
    val f: compiler.Exp[Int] => compiler.Exp[Int] = compiler.guardedCatch(_)
    val staged = compiler.compile(f)

    staged(3) shouldBe 10
    staged(-1) shouldBe 20
    staged(-2) shouldBe 30
  }

  test("virtualized throw syntax supports arbitrary Throwable subclasses with String constructors") {
    val f: compiler.Exp[Boolean] => compiler.Exp[Int] = compiler.throwSyntaxRuntimeException(_)
    val staged = compiler.compile(f)

    staged(false) shouldBe 10
    staged(true) shouldBe 60
  }

  test("virtualized throw syntax supports no-argument Exception constructors") {
    val f: compiler.Exp[Boolean] => compiler.Exp[Int] = compiler.throwNoArgException(_)
    val staged = compiler.compile(f)

    staged(false) shouldBe 10
    staged(true) shouldBe 80
  }

  test("virtualized throw syntax preserves no-argument thrown subclass types") {
    val f: compiler.Exp[Boolean] => compiler.Exp[Int] = compiler.throwNoArgRuntimeException(_)
    val staged = compiler.compile(f)

    staged(false) shouldBe 10
    staged(true) shouldBe 90
  }

  test("virtualized throw syntax supports (String, Throwable) constructors with message causes") {
    val f: compiler.Exp[Boolean] => compiler.Exp[Int] = compiler.throwSyntaxWithCause(_)
    val staged = compiler.compile(f)

    staged(false) shouldBe 10
    staged(true) shouldBe 110
  }

  test("virtualized throw syntax supports (String, Throwable) constructors with no-arg causes") {
    val f: compiler.Exp[Boolean] => compiler.Exp[Int] = compiler.throwSyntaxWithNoArgCause(_)
    val staged = compiler.compile(f)

    staged(false) shouldBe 10
    staged(true) shouldBe 130
  }

  test("virtualized throw syntax supports Throwable-only constructors with message causes") {
    val f: compiler.Exp[Boolean] => compiler.Exp[Int] = compiler.throwSyntaxCauseOnly(_)
    val staged = compiler.compile(f)

    staged(false) shouldBe 10
    staged(true) shouldBe 150
  }

  test("virtualized throw syntax supports Throwable-only constructors with no-arg causes") {
    val f: compiler.Exp[Boolean] => compiler.Exp[Int] = compiler.throwSyntaxCauseOnlyNoArg(_)
    val staged = compiler.compile(f)

    staged(false) shouldBe 10
    staged(true) shouldBe 170
  }

  test("virtualized finally runs on the normal path") {
    val f: compiler.Exp[Int] => compiler.Exp[Int] = compiler.finallyOnNormalPath(_)
    val staged = compiler.compile(f)

    staged(3) shouldBe 13
  }

  test("virtualized finally runs after catches on the throw path") {
    val f: compiler.Exp[Boolean] => compiler.Exp[Int] = compiler.finallyOnThrowPath(_)
    val staged = compiler.compile(f)

    staged(false) shouldBe 111
    staged(true) shouldBe 120
  }

  test("virtualized return supports staged early exits") {
    val f: compiler.Exp[Int] => compiler.Exp[Int] = compiler.stagedReturn(_)
    val staged = compiler.compile(f)

    staged(-1) shouldBe 7
    staged(3) shouldBe 4
  }

  test("virtualized finally values compose directly in later arithmetic") {
    val f: compiler.Exp[Int] => compiler.Exp[Int] = compiler.finallyValueFeedsArithmetic(_)
    val staged = compiler.compile(f)

    staged(3) shouldBe 411
  }

  test("virtualized finally values survive a local val before later arithmetic") {
    val f: compiler.Exp[Int] => compiler.Exp[Int] = compiler.finallyValueViaLocal(_)
    val staged = compiler.compile(f)

    staged(3) shouldBe 411
  }

  test("virtualized try/catch/finally values compose directly in later arithmetic") {
    val f: compiler.Exp[Boolean] => compiler.Exp[Int] = compiler.tryCatchFinallyValueFeedsArithmetic(_)
    val staged = compiler.compile(f)

    staged(false) shouldBe 1011
    staged(true) shouldBe 2011
  }

  test("virtualized try/catch/finally values survive a local val before later arithmetic") {
    val f: compiler.Exp[Boolean] => compiler.Exp[Int] = compiler.tryCatchFinallyValueViaLocal(_)
    val staged = compiler.compile(f)

    staged(false) shouldBe 1011
    staged(true) shouldBe 2011
  }
}
