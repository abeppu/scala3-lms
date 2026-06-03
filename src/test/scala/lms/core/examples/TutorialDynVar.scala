package lms.core.examples

import lms.core.*
import lms.legacy.compat.SourceContext
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.language.implicitConversions

trait TrackConditionals extends Dsl {
  private var seenTrue: Set[Rep[Boolean]] = Set.empty
  private var seenFalse: Set[Rep[Boolean]] = Set.empty

  private def pushTrue[T](cond: Rep[Boolean], thenp: => Rep[T]): Rep[T] = {
    val saved = seenTrue
    seenTrue += cond
    try thenp
    finally seenTrue = saved
  }

  private def pushFalse[T](cond: Rep[Boolean], elsep: => Rep[T]): Rep[T] = {
    val saved = seenFalse
    seenFalse += cond
    try elsep
    finally seenFalse = saved
  }

  abstract override def __ifThenElse[T: Typ](cond: Rep[Boolean], thenp: => Rep[T], elsep: => Rep[T])(using pos: SourceContext): Rep[T] =
    if (seenTrue.contains(cond)) thenp
    else if (seenFalse.contains(cond)) elsep
    else super.__ifThenElse(cond, pushTrue(cond, thenp), pushFalse(cond, elsep))
}

@virt
trait DynVarWarmup extends Dsl {
  def snippet(n: Rep[Int]): Rep[Int] =
    if (n == 0) {
      if (n == 0) 0 else 2
    } else 1
}

class TutorialDynVarTest extends AnyFunSuite with Matchers {
  private def countIfs(code: String): Int =
    code.sliding(4).count(_ == "if (")

  test("tracking repeated conditionals simplifies generated code") {
    @virt
    object Warmup0 extends DslDriver[Int, Int] with Dsl with DynVarWarmup

    @virt
    object Warmup1 extends DslDriver[Int, Int] with Dsl with DynVarWarmup with TrackConditionals

    countIfs(Warmup0.code) should be > countIfs(Warmup1.code)
    countIfs(Warmup1.code) shouldBe 1
  }
}
