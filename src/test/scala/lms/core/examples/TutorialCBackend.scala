package lms.core.examples

import lms.core.*
import lms.legacy.common.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.io.StringWriter
import java.nio.file.Files
import scala.sys.process.*
import scala.reflect.ClassTag

object TutorialCBackendSupport {
  def compileAndRun(code: String, main: String, prefix: String = ""): String = {
    val dir = Files.createTempDirectory("lms-c-backend")
    val source = dir.resolve("snippet.c")
    val binary = dir.resolve("snippet")
    Files.writeString(source, prefix + code + "\n" + main)
    try {
      Seq("gcc", source.toString, "-o", binary.toString).!!
      Seq(binary.toString).!!.trim
    } finally {
      Files.deleteIfExists(binary)
      Files.deleteIfExists(source)
      Files.deleteIfExists(dir)
    }
  }
}

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

  override def remap[A](m: Typ[A]): String =
    if m.runtimeClass.isArray && m.typeArguments.nonEmpty then remap(m.typeArguments.head)
    else if m.toString == "String" then "char *"
    else if m.toString == "Char" then "char"
    else super.remap(m)

  override def remapWithRef[A](m: Typ[A]): String =
    if m.runtimeClass.isArray && m.typeArguments.nonEmpty then s"${remap(m.typeArguments.head)} *"
    else if m.toString == "String" then "char *"
    else super.remapWithRef(m)

  override def isPrimitiveType(tpe: String): Boolean =
    tpe == "char" || super.isPrimitiveType(tpe)

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case a @ ArrayNew(n) =>
      val elementType = remap(a.m)
      emitValDef(sym, s"($elementType*)calloc(${quote(n)}, sizeof($elementType))")
    case ArrayApply(array, index) =>
      emitValDef(sym, s"${quote(array)}[${quote(index)}]")
    case ArrayUpdate(array, index, value) =>
      stream.println(s"${quote(array)}[${quote(index)}] = ${quote(value)};")
    case _ =>
      super.emitNode(sym, rhs)
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

object TutorialCArraySnippet extends TutorialDslDriverC[Int, Int] {
  def snippet(n: Rep[Int]): Rep[Int] = {
    val values = NewArray[Int](unit(2))
    values(unit(0)) = n
    values(unit(1)) = int_plus(n, unit(1))
    int_plus(values(unit(0)), values(unit(1)))
  }
}

class TutorialCBackendTest extends AnyFunSuite with Matchers {
  import TutorialCBackendSupport.compileAndRun

  test("C backend emits source for arithmetic and if") {
    val code = TutorialCArithmeticSnippet.cSource
    code should include("#include <stdio.h>")
    code should include("int32_t snippet(int32_t")
    code should include("if")
    code should include("return")
  }

  test("C backend compiles and runs arithmetic and if") {
    val output = compileAndRun(
      TutorialCArithmeticSnippet.cSource,
      """int main() {
        |  printf("%d\n", snippet(4));
        |  printf("%d\n", snippet(0));
        |  return 0;
        |}
        |""".stripMargin
    )
    output shouldBe "8\n1"
  }

  test("C backend emits source for while loops and mutable vars") {
    val code = TutorialCWhileVarSnippet.cSource
    code should include("for (;;)")
    code should include("break")
    code should include("int32_t")
    code should include(" = ")
    code should include("return")
  }

  test("C backend compiles and runs while loops and mutable vars") {
    val output = compileAndRun(
      TutorialCWhileVarSnippet.cSource,
      """int main() {
        |  printf("%d\n", snippet(5));
        |  return 0;
        |}
        |""".stripMargin
    )
    output shouldBe "10"
  }

  test("C backend emits source for printf") {
    val code = TutorialCPrintSnippet.cSource
    code should include("printf")
    code should include("value=%d")
  }

  test("C backend compiles and runs printf") {
    val output = compileAndRun(
      TutorialCPrintSnippet.cSource,
      """int main() {
        |  snippet(7);
        |  return 0;
        |}
        |""".stripMargin
    )
    output shouldBe "value=7"
  }

  test("C backend emits source for arrays") {
    val code = TutorialCArraySnippet.cSource
    code should include("int32_t *")
    code should include("calloc")
    code should include("[0]")
    code should include("[1]")
  }

  test("C backend compiles and runs arrays") {
    val output = compileAndRun(
      TutorialCArraySnippet.cSource,
      """int main() {
        |  printf("%d\n", snippet(4));
        |  return 0;
        |}
        |""".stripMargin
    )
    output shouldBe "9"
  }
}
