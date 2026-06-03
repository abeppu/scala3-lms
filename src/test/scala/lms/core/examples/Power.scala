package lms.core.examples

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.*
import lms.gen.StagingCompile
import lms.legacy.common.*
import lms.legacy.compat.SourceContext

import scala.language.implicitConversions

trait Power extends PrimitiveOpsExp with BooleanOpsExp {
  def power(b: Exp[Double],n: Int): Exp[Double] = {
    if (n == 0) {
      unit(1.0)
    } else if (n == 1) {
      b
    } else if (n % 2 == 0) {
      val y = power(b, n/2)
      y * y
    } else {
      b * power(b, n - 1)
    }
  }
}

class PowerSpec extends AnyFlatSpec with Matchers {

  "simple double compilation" should "produce normal value" in {
    val powerCompiler = new Power with StagingCompile with BaseExp with PrimitiveOpsExpOpt with LiftPrimitives with VariablesExp with PrimitiveOpsGen {
      type API = BaseExp

      override implicit def readVar[T: Typ](v: Variable[T])(using pos: SourceContext): Exp[T] = ???
    }
    val f: powerCompiler.Exp[Double] => powerCompiler.Exp[Double] = powerCompiler.power(_, 10)
    val pDouble10: Double => Double = powerCompiler.compile(f)

    pDouble10(2.0) should be (1024.0)
  }
}
