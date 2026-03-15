package lms.core.examples

import lms.core.*
import lms.legacy.compat.SourceContext.given
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.language.implicitConversions

@virt
trait StagingCompileVarSnippets extends Dsl {
  def assignSnapshot(x: Rep[Int]): Rep[Int] = {
    var current = x
    val next = current + 1
    current = current + 10
    next
  }

  def guardedCharAt(text: Rep[String]): Rep[Boolean] = {
    var i: Var[Int] = -1
    i = readVar(i) + 1
    val idx = readVar(i)
    idx < text.length && text(idx) == 'a'
  }

  def countTo(limit: Rep[Int]): Rep[Int] = {
    var i = 0
    while (i < limit) {
      i = i + 1
    }
    readVar(i)
  }

  def branchSnapshot(x: Rep[Int]): Rep[Int] = {
    var current = x
    if (x > 0) {
      val next = current + 1
      current = current + 10
      next
    } else {
      val next = current - 1
      current = current - 10
      next
    }
  }

  def findAB(text: Rep[String]): Rep[Boolean] = {
    var start: Var[Int] = -1
    var found: Var[Boolean] = false
    while (!readVar(found) && readVar(start) < text.length) {
      start = readVar(start) + 1
      val startIdx = readVar(start)
      var cursor: Var[Int] = startIdx + 1
      var matched: Var[Boolean] = false
      while (!readVar(matched) && readVar(cursor) < text.length) {
        val cursorIdx = readVar(cursor)
        matched = text(startIdx) == 'a' && text(cursorIdx) == 'b'
        cursor = cursorIdx + 1
      }
      found = readVar(matched)
    }
    readVar(found)
  }

  def crossUpdate(seed: Rep[Int]): Rep[Int] = {
    var a: Var[Int] = seed
    var b: Var[Int] = seed + 1
    val next = readVar(a) + readVar(b)
    a = b
    b = next
    readVar(a) * 10 + readVar(b)
  }
}

class StagingCompileVarTest extends AnyFunSuite with Matchers {
  private val compiler = new StagingCompileVarSnippets with DslCompile

  test("assignment rhs uses pre-update snapshot") {
    val f: compiler.Exp[Int] => compiler.Exp[Int] = compiler.assignSnapshot(_)
    val staged = compiler.compile(f)

    staged(0) shouldBe 1
    staged(7) shouldBe 8
  }

  test("guarded string access preserves short-circuiting") {
    val f: compiler.Exp[String] => compiler.Exp[Boolean] = compiler.guardedCharAt(_)
    val staged = compiler.compile(f)

    staged("") shouldBe false
    staged("abc") shouldBe true
    staged("xbc") shouldBe false
  }

  test("while condition rereads vars on every iteration") {
    val f: compiler.Exp[Int] => compiler.Exp[Int] = compiler.countTo(_)
    val staged = compiler.compile(f)

    staged(0) shouldBe 0
    staged(3) shouldBe 3
    staged(5) shouldBe 5
  }

  test("branch-local pure values are snapshotted before mutation") {
    val f: compiler.Exp[Int] => compiler.Exp[Int] = compiler.branchSnapshot(_)
    val staged = compiler.compile(f)

    staged(4) shouldBe 5
    staged(-4) shouldBe -5
  }

  test("nested while loops keep outer and inner var snapshots distinct") {
    val f: compiler.Exp[String] => compiler.Exp[Boolean] = compiler.findAB(_)
    val staged = compiler.compile(f)

    staged("accb") shouldBe true
    staged("cccc") shouldBe false
    staged("ba") shouldBe false
  }

  test("cross-variable updates preserve previous values") {
    val f: compiler.Exp[Int] => compiler.Exp[Int] = compiler.crossUpdate(_)
    val staged = compiler.compile(f)

    staged(1) shouldBe 23
    staged(2) shouldBe 35
  }
}
