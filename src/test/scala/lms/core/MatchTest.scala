package lms.core

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

@virt
trait MatchSnippets extends Dsl {
  object HostEven {
    def unapply(x: Int): Boolean = x % 2 == 0
  }

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

  def binderMatch(x: Rep[Int]): Rep[Int] =
    x match {
      case n if n < 0 => 0 - n
      case n => n + 1
    }

  def aliasLiteralMatch(x: Rep[Int]): Rep[Int] =
    x match {
      case n @ 0 => n + 100
      case n @ 1 => n + 200
      case 2 => 202
      case n => n * 3
    }

  def typedMatch(x: Rep[Any]): Rep[Int] =
    x match {
      case _: Int => 101
      case _: String => 200
      case _ => 300
    }

  def typedBinderMatch(x: Rep[Any]): Rep[Int] =
    x match {
      case n: Int => n + 1
      case s: String => 200
      case _ => 300
    }

  def typedAliasMatch(x: Rep[Any]): Rep[Int] =
    x match {
      case n @ (_: Int) if n > 0 => n * 2
      case s @ (_: String) => 200
      case _ => 300
    }

  def typedBinderArithmeticMatch(x: Rep[Any]): Rep[Int] =
    x match {
      case n: Int if n < 0 => 0 - n
      case n: Int => n + 10
      case _ => 300
    }

  def combinatorValueMatch(x: Rep[Int]): Rep[Int] =
    stagedMatch[Int, Int](x)(
      stagedValueCase[Int, Int](unit(0))(_ => 100),
      stagedCase[Int, Int](n => n < 0)(n => 0 - n)
    )(n => n + 10)

  def combinatorTypeMatch(x: Rep[Any]): Rep[Int] =
    stagedMatch[Any, Int](x)(
      stagedTypeCase[Any, Int, Int](n => n + 1),
      stagedTypeCase[Any, String, Int](s => s.length)
    )(_ => 300)

  def hostExtractorMatch(x: Int): Int =
    x match {
      case HostEven() => x + 100
      case _ => x + 1
    }

  def hostOptionExtractorMatch(x: Option[Int]): Int =
    x match {
      case Some(n) if n > 0 => n + 10
      case Some(n) => n - 10
      case None => 0
    }

}

class MatchTest extends AnyFunSuite with Matchers {
  private val compiler = new MatchSnippets with DslCompile
  private val hostSnippets = new MatchSnippets with DslCompile

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

  test("virtualized match supports binder patterns in guards and rhs") {
    val f: compiler.Exp[Int] => compiler.Exp[Int] = compiler.binderMatch(_)
    val staged = compiler.compile(f)

    staged(-3) shouldBe 3
    staged(0) shouldBe 1
    staged(7) shouldBe 8
  }

  test("virtualized match supports simple alias patterns") {
    val f: compiler.Exp[Int] => compiler.Exp[Int] = compiler.aliasLiteralMatch(_)
    val staged = compiler.compile(f)

    staged(0) shouldBe 100
    staged(1) shouldBe 201
    staged(2) shouldBe 202
    staged(7) shouldBe 21
  }

  test("virtualized match supports typed patterns on staged Any scrutinees") {
    val f: compiler.Exp[Any] => compiler.Exp[Int] = compiler.typedMatch(_)
    val staged = compiler.compile[Any, Int](f)

    staged(7) shouldBe 101
    staged("zzz") shouldBe 200
    staged(true) shouldBe 300
  }

  test("virtualized match supports typed binders on staged Any scrutinees") {
    val f: compiler.Exp[Any] => compiler.Exp[Int] = compiler.typedBinderMatch(_)
    val staged = compiler.compile[Any, Int](f)

    staged(7) shouldBe 8
    staged("zzz") shouldBe 200
    staged(true) shouldBe 300
  }

  test("virtualized match supports typed alias binders on staged Any scrutinees") {
    val f: compiler.Exp[Any] => compiler.Exp[Int] = compiler.typedAliasMatch(_)
    val staged = compiler.compile[Any, Int](f)

    staged(7) shouldBe 14
    staged(-2) shouldBe 300
    staged("zzz") shouldBe 200
    staged(true) shouldBe 300
  }

  test("virtualized match keeps typed binder arithmetic staged on both branches") {
    val f: compiler.Exp[Any] => compiler.Exp[Int] = compiler.typedBinderArithmeticMatch(_)
    val staged = compiler.compile[Any, Int](f)

    staged(-7) shouldBe 7
    staged(5) shouldBe 15
    staged("zzz") shouldBe 300
  }

  test("staged match combinator supports value and predicate cases") {
    val f: compiler.Exp[Int] => compiler.Exp[Int] = compiler.combinatorValueMatch(_)
    val staged = compiler.compile(f)

    staged(0) shouldBe 100
    staged(-7) shouldBe 7
    staged(5) shouldBe 15
  }

  test("staged match combinator supports typed cases without pattern syntax") {
    val f: compiler.Exp[Any] => compiler.Exp[Int] = compiler.combinatorTypeMatch(_)
    val staged = compiler.compile[Any, Int](f)

    staged(7) shouldBe 8
    staged("abcd") shouldBe 4
    staged(true) shouldBe 300
  }

  test("host extractor matches are preserved inside @virt code when the scrutinee is bare") {
    hostSnippets.hostExtractorMatch(4) shouldBe 104
    hostSnippets.hostExtractorMatch(5) shouldBe 6
  }

  test("host option extractor matches are preserved inside @virt code when the scrutinee is bare") {
    hostSnippets.hostOptionExtractorMatch(Some(7)) shouldBe 17
    hostSnippets.hostOptionExtractorMatch(Some(-3)) shouldBe -13
    hostSnippets.hostOptionExtractorMatch(None) shouldBe 0
  }

}
