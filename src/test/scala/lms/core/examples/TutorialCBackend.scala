package lms.core.examples

import lms.core.*
import lms.legacy.common.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.io.StringWriter
import scala.reflect.ClassTag

trait TutorialDslGenC
    extends CGenPrimitiveOps
    with CGenBooleanOps
    with CGenEqual
    with CGenOrderingOps
    with CGenIfThenElse
    with CGenVariables
    with CGenWhile
    with CGenArrayOps
    with CGenStringOps
    with CGenMiscOps {
  val IR: DslExp
}

trait TutorialDslDriverC[A: ClassTag, B: ClassTag] extends DslSnippet[A, B] with DslExp { self =>
  val codegen = new TutorialDslGenC {
    val IR: self.type = self
  }

  lazy val cSource: String = {
    val source = new StringWriter()
    codegen.emitSource(snippet, "snippet", new java.io.PrintWriter(source))(using manifestTyp[A], manifestTyp[B])
    source.toString
  }
}

@virt
object TutorialCArithmeticSnippet extends TutorialDslDriverC[Int, Int] {
  def snippet(n: Rep[Int]): Rep[Int] = {
    if n == 0 then int_plus(n, unit(1)) else int_times(n, unit(2))
  }
}

class TutorialCBackendTest extends AnyFunSuite with Matchers {
  test("C backend emits source for arithmetic and if") {
    val code = TutorialCArithmeticSnippet.cSource
    code should include("#include <stdio.h>")
    code should include("int32_t snippet(int32_t")
    code should include("if")
    code should include("return")
  }
}
