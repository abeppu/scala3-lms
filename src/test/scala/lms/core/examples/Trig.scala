package lms.core.examples

import lms.gen.{Gen, StagingCompile}
import lms.legacy.common.{Base, BaseExp, PrimitiveOpsExp}

import scala.quoted.*

trait Trig extends Base {

  //todo removed
  //implicit def unit(x: Double): Rep[Double]

  def sin(x: Rep[Double]): Rep[Double]
  def cos(x: Rep[Double]): Rep[Double]

}

trait TrigExp extends Trig with PrimitiveOpsExp {
  //implicit def doubleTyp: Typ[Double]
  trait TrigOp extends Def[Double] {
    def x: Exp[Double]
  }
  case class Sin(x: Exp[Double]) extends TrigOp
  case class Cos(x: Exp[Double]) extends TrigOp

  def sin(x: Exp[Double]) = Sin(x)
  def cos(x: Exp[Double]) = Cos(x)
}

trait TrigExpOpt extends TrigExp {

  override def sin(x: Exp[Double]) = x match {
    case Const(x) => {
      val xs = math.sin(x) 
      unit(xs)
    }
    case _ => super.sin(x)
  }
  
  override def cos(x: Exp[Double]) = x match {
    case Const(x) => {
      val cx = math.cos(x)
      unit(cx)
    }
    case _ => super.cos(x)
  }

}

trait TrigGen extends Gen with TrigExp {
  this: StagingCompile =>

  override def constantTerm[T](c: Const[T])(using q: Quotes): q.reflect.Term = {
    super.constantTerm(c)
  }

  override def interpretDefWithEnv[A](d: Def[A])(using q: Quotes, env:Map[Sym[?], q.reflect.Symbol]): q.reflect.Term = {
    import q.reflect.*

    if (!d.isInstanceOf[TrigOp]) {
      super.interpretDefWithEnv(d)
    } else {

      val mathModule = Symbol.requiredModule("java.lang.Math")
      val mathTerm = Ref(mathModule)
      val trigOp = d.asInstanceOf[TrigOp]
      val xTerm: Term = interpretExpWithEnv(trigOp.x)(using q, env)
      trigOp match {
        case Sin(e) => {
          val sinSym = mathModule.methodMember("sin").head
          val symSelect = Select(mathTerm, sinSym)
          Apply(symSelect, List(xTerm))
        }
        case Cos(e) => {
          val cosSym = mathModule.methodMember("cos").head
          val cosSelect = Select(mathTerm, cosSym)
          Apply(cosSelect, List(xTerm))
        }
        case _ => super.interpretDefWithEnv(d)
      }
    }
  }
}