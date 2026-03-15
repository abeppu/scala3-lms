package lms.legacy.common

import lms.gen.{Gen, StagingCompile}

import java.io.PrintWriter
import lms.legacy.compat.SourceContext

import scala.compiletime.deferred

trait LiftBoolean {
  this: Base =>

  given boolTyp: Typ[Boolean] = deferred
  implicit def boolToBoolRep(b: Boolean): Rep[Boolean] = unit(b)
}

trait BooleanOps extends Variables {
  given boolTyp: Typ[Boolean] = deferred

  def infix_unary_!(x: Rep[Boolean])(using pos: SourceContext) = boolean_negate(x)
  def infix_&&(lhs: Rep[Boolean], rhs: =>Rep[Boolean])(using pos: SourceContext) = boolean_and(lhs,rhs)
  def infix_||(lhs: Rep[Boolean], rhs: =>Rep[Boolean])(using pos: SourceContext) = boolean_or(lhs,rhs)

  // TODO: short-circuit by default

  def boolean_negate(lhs: Rep[Boolean])(using pos: SourceContext): Rep[Boolean]
  def boolean_and(lhs: Rep[Boolean], rhs: Rep[Boolean])(using pos: SourceContext): Rep[Boolean]
  def boolean_or(lhs: Rep[Boolean], rhs: Rep[Boolean])(using pos: SourceContext): Rep[Boolean]

  extension (b: Rep[Boolean])
    def unary_! = boolean_negate(b)

}

trait BooleanOpsExp extends BooleanOps with EffectExp {
  override given boolTyp: Typ[Boolean] = manifestTyp

  case class BooleanNegate(lhs: Exp[Boolean]) extends Def[Boolean]
  case class BooleanAnd(lhs: Exp[Boolean], rhs: Exp[Boolean]) extends Def[Boolean]
  case class BooleanOr(lhs: Exp[Boolean], rhs: Exp[Boolean]) extends Def[Boolean]

  def boolean_negate(lhs: Exp[Boolean])(using pos: SourceContext) : Exp[Boolean] = BooleanNegate(lhs)
  def boolean_and(lhs: Exp[Boolean], rhs: Exp[Boolean])(using pos: SourceContext) : Exp[Boolean] = BooleanAnd(lhs,rhs)
  def boolean_or(lhs: Exp[Boolean], rhs: Exp[Boolean])(using pos: SourceContext) : Exp[Boolean] = BooleanOr(lhs,rhs)

  override def mirror[A:Typ](e: Def[A], f: Transformer)(using pos: SourceContext): Exp[A] = (e match {
    case BooleanNegate(x) => boolean_negate(f(x))
    case BooleanAnd(x,y) => boolean_and(f(x),f(y))
    case BooleanOr(x,y) => boolean_or(f(x),f(y))

    case Reflect(BooleanNegate(x), u, es) => reflectMirrored(Reflect(BooleanNegate(f(x)), mapOver(f,u), f(es)))(using mtyp1[A], pos)
    case Reflect(BooleanAnd(x,y), u, es) => reflectMirrored(Reflect(BooleanAnd(f(x),f(y)), mapOver(f,u), f(es)))(using mtyp1[A], pos)
    case Reflect(BooleanOr(x,y), u, es) => reflectMirrored(Reflect(BooleanOr(f(x),f(y)), mapOver(f,u), f(es)))(using mtyp1[A], pos)
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]] // why??
}

import scala.quoted.*
trait BooleanOpsGen extends Gen with BooleanOpsExp {
  this: StagingCompile => 
  
  override def constantTerm[T](c: Const[T])(using q: Quotes): q.reflect.Term = {
    import q.reflect.*
    c match {
      case Const(x: Boolean) => Literal(BooleanConstant(x))
      // TODO others
      case _ => super.constantTerm(c)
    }
  }
  
  override def interpretDefWithEnv[A](d: Def[A])(using q: Quotes, env: Map[Sym[?], q.reflect.Symbol]): q.reflect.Term = {
    import q.reflect.*
    
    d match {
      case BooleanNegate(x) => {
        val xTerm = interpretExpWithEnv(x)
        val xExpr = xTerm.asExprOf[Boolean]
        '{!$xExpr}.asTerm
      }
      case BooleanAnd(x, y) => {
        val xExpr = interpretExpWithEnv(x).asExprOf[Boolean]
        val yExpr = interpretExpWithEnv(y).asExprOf[Boolean]
        '{$xExpr && $yExpr}.asTerm
      }
      case BooleanOr(x, y) => {
        val xExpr = interpretExpWithEnv(x).asExprOf[Boolean]
        val yExpr = interpretExpWithEnv(y).asExprOf[Boolean]
        '{$xExpr || $yExpr}.asTerm
      }
      case _ => super.interpretDefWithEnv(d)
    }
  }
}

/**
 * @author  Alen Stojanov (astojanov@inf.ethz.ch)
 */
trait BooleanOpsExpOpt extends BooleanOpsExp {

  override def boolean_negate(lhs: Exp[Boolean])(using pos: SourceContext) = lhs match {
    case Def(BooleanNegate(x)) => x
    case Const(a) => Const(!a)
    case _ => super.boolean_negate(lhs)
  }

  override def boolean_and(lhs: Exp[Boolean], rhs: Exp[Boolean])(using pos: SourceContext) : Exp[Boolean] = {
    (lhs, rhs) match {
      case (Const(false), _) => Const(false)
      case (_, Const(false)) => Const(false)
      case (Const(true), x) => x
      case (x, Const(true)) => x
      case _ => super.boolean_and(lhs, rhs)
    }
  }

  override def boolean_or(lhs: Exp[Boolean], rhs: Exp[Boolean])(using pos: SourceContext) : Exp[Boolean] = {
    (lhs, rhs) match {
      case (Const(false), x) => x
      case (x, Const(false)) => x
      case (Const(true), _) => Const(true)
      case (_, Const(true)) => Const(true)
      case _ => super.boolean_or(lhs, rhs)
    }
  }
}

trait ScalaGenBooleanOps extends ScalaGenBase {
  val IR: BooleanOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case BooleanNegate(b) => emitValDef(sym, src"!$b")
    case BooleanAnd(lhs,rhs) => emitValDef(sym, src"$lhs && $rhs")
    case BooleanOr(lhs,rhs) => emitValDef(sym, src"$lhs || $rhs")
    case _ => super.emitNode(sym,rhs)
  }
}

trait CLikeGenBooleanOps extends CLikeGenBase {
  val IR: BooleanOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case BooleanNegate(b) => emitValDef(sym, src"!$b")
    case BooleanAnd(lhs,rhs) => emitValDef(sym, src"$lhs && $rhs")
    case BooleanOr(lhs,rhs) => emitValDef(sym, src"$lhs || $rhs")
    case _ => super.emitNode(sym,rhs)
  }
}

trait CudaGenBooleanOps extends CudaGenBase with CLikeGenBooleanOps
trait OpenCLGenBooleanOps extends OpenCLGenBase with CLikeGenBooleanOps
trait CGenBooleanOps extends CGenBase with CLikeGenBooleanOps
