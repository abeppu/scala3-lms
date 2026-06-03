package lms.core.examples

import lms.core.*
import lms.legacy.common.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.io.StringWriter
import scala.reflect.ClassTag

trait TutorialDslGenCuda
    extends CudaGenPrimitiveOps
    with CudaGenBooleanOps
    with CudaGenEqual
    with CudaGenOrderingOps
    with CudaGenIfThenElse
    with CudaGenVariables
    with CudaGenWhile
    with CudaGenMiscOps {
  val IR: DslExp
}

trait TutorialDslDriverCuda[A: ClassTag, B: ClassTag] extends DslSnippet[A, B] with DslExp { self =>
  val codegen = new TutorialDslGenCuda {
    val IR: self.type = self
  }

  lazy val cudaSource: String = {
    val source = new StringWriter()
    codegen.emitSource(snippet, "snippet", new java.io.PrintWriter(source))(using manifestTyp[A], manifestTyp[B])
    source.toString
  }
}

@virt
object TutorialCudaArithmeticSnippet extends TutorialDslDriverCuda[Int, Int] {
  def snippet(n: Rep[Int]): Rep[Int] = {
    if n == 0 then int_plus(n, unit(1)) else int_times(n, unit(2))
  }
}

object TutorialCudaWhileVarSnippet extends TutorialDslDriverCuda[Int, Int] {
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

class TutorialCudaBackendTest extends AnyFunSuite with Matchers {
  test("CUDA backend emits source without requiring nvcc") {
    val code = TutorialCudaArithmeticSnippet.cudaSource
    code should include("Emitting Cuda Generated Code")
    code should include("#include <stdio.h>")
    code should include("#include <stdlib.h>")
    code should include("int main(int argc, char** argv)")
    code should include("if")
  }

  test("CUDA backend emits source for while loops and mutable vars") {
    val code = TutorialCudaWhileVarSnippet.cudaSource
    code should include("Emitting Cuda Generated Code")
    code should include("for (;;)")
    code should include("break")
    code should include("int32_t")
  }
}
