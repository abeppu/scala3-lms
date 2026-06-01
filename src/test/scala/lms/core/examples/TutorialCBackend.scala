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
  import IR.*

  private def cString(value: String): String =
    "\"" + value.flatMap {
      case '\\' => "\\\\"
      case '"' => "\\\""
      case '\n' => "\\n"
      case '\t' => "\\t"
      case ch => ch.toString
    } + "\""

  override def quoteRawString(s: Exp[Any]): String = s match {
    case Const(value: String) => cString(value)
    case _ => super.quoteRawString(s)
  }
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

object TutorialCWhileVarSnippet extends TutorialDslDriverC[Int, Int] {
  def snippet(n: Rep[Int]): Rep[Int] = {
    val acc = var_new(unit(0))
    val i = var_new(unit(0))
    __whileDo(ordering_lt(ReadVar(i), n), {
      var_assign(acc, int_plus(ReadVar(acc), ReadVar(i)))
      var_assign(i, int_plus(ReadVar(i), unit(1)))
    })
    ReadVar(acc)
  }
}

object TutorialCPrintSnippet extends TutorialDslDriverC[Int, Unit] {
  def snippet(n: Rep[Int]): Rep[Unit] =
    printf("value=%d\n", n)
}

class TutorialCBackendTest extends AnyFunSuite with Matchers {
  test("C backend emits source for arithmetic and if") {
    val code = TutorialCArithmeticSnippet.cSource
    code should include("#include <stdio.h>")
    code should include("int32_t snippet(int32_t")
    code should include("if")
    code should include("return")
  }

  test("C backend emits source for while loops and mutable vars") {
    val code = TutorialCWhileVarSnippet.cSource
    code should include("for (;;)")
    code should include("break")
    code should include("int32_t")
    code should include(" = ")
    code should include("return")
  }

  test("C backend emits source for printf") {
    val code = TutorialCPrintSnippet.cSource
    code should include("printf")
    code should include("value=%d")
  }
}
