package lms.legacy.common

import scala.language.implicitConversions

import java.io.PrintWriter
import lms.legacy.util.OverloadHack
import lms.legacy.compat.SourceContext

import scala.compiletime.deferred

trait LiftVariables extends Base {
  this: Variables =>

  def __newVar[T:Typ](init: T)(using pos: SourceContext) = var_new(unit(init))
  def __newVar[T](init: Rep[T])(using o: Overloaded1, mT: Typ[T], pos: SourceContext) = var_new(init)
  def __newVar[T](init: Var[T])(using o: Overloaded2, mT: Typ[T], pos: SourceContext) = var_new(init)
}

// ReadVar is factored out so that it does not have higher priority than VariableImplicits when mixed in
// (which result in ambiguous conversions)
trait ReadVarImplicit {
  this: Variables =>

  implicit def readVar[T:Typ](v: Var[T])(using pos: SourceContext) : Rep[T]
}

trait ReadVarImplicitExp extends EffectExp {
  this: VariablesExp =>

  given readVar[T:Typ](using pos: SourceContext): Conversion[Var[T], Exp[T]] with { 
    def apply (v: Var[T]) = ReadVar(v) 
  }
}

trait LowPriorityVariableImplicits extends ImplicitOps {
  this: Variables =>

  given intTyp: Typ[Int] = deferred
  given floatTyp: Typ[Float] = deferred
  given doubleTyp: Typ[Double] = deferred

  given varIntToRepDouble(using pos: SourceContext): Conversion[Var[Int],Rep[Double]] with {
    def apply(x: Var[Int]): Rep[Double] = implicit_convert[Int,Double](readVar(x))
  }
  given varIntToRepFloat(using pos: SourceContext): Conversion[Var[Int],Rep[Float]] with {
    def apply(x: Var[Int]): Rep[Float] = implicit_convert[Int, Float](readVar(x))(using _.toFloat, intTyp, floatTyp, pos)
  }
  given varFloatToRepDouble(using pos: SourceContext): Conversion[Var[Float],Rep[Double]] with {
    def apply(x: Var[Float]): Rep[Double] = implicit_convert[Float,Double](readVar(x))
  }
}

trait VariableImplicits extends LowPriorityVariableImplicits {
  this: Variables =>

  // Cam: Scala 3 changed how implicit search works, and now these cause an "ambiguous implicit" error.
  // we always want to prioritize a direct conversion if any Rep will do
  //given varIntToRepInt(using pos: SourceContext): Conversion[Var[Int],Rep[Int] ] with {
  //  def apply(v): Var[Int] = readVar(v)
  // }
  //given varFloatToRepFloat(using pos: SourceContext): Conversion[Var[Float],Rep[Float] ] with {
  //def apply(v): Var[Float] = readVar(v)
  //}
}

trait Variables extends Base with OverloadHack with VariableImplicits with ReadVarImplicit {
  type Var[+T] //FIXME: should be invariant

  given __virtualizedBareVarConvInternal[T]: Conversion[T, Var[T]] with
    def apply(x: T): Var[T] = throw new RuntimeException("attempted to call __virtualizedBareVarConvInternal (did you forget to virtualize?)");

  given __virtualizedRepVarConvInternal[T]: Conversion[Rep[T], Var[T]] with
    def apply(x: Rep[T]): Var[T] = throw new RuntimeException("attempted to call __virtualizedRepVarConvInternal (did you forget to virtualize?)");

  //implicit def chainReadVar[T,U](x: Var[T])(using f: Rep[T] => U): U = f(readVar(x))
  def var_new[T:Typ](init: Rep[T])(using pos: SourceContext): Var[T]
  def var_assign[T:Typ](lhs: Var[T], rhs: Rep[T])(using pos: SourceContext): Rep[Unit]
  def var_plusequals[T:Typ](lhs: Var[T], rhs: Rep[T])(using pos: SourceContext): Rep[Unit]
  def var_minusequals[T:Typ](lhs: Var[T], rhs: Rep[T])(using pos: SourceContext): Rep[Unit]
  def var_timesequals[T:Typ](lhs: Var[T], rhs: Rep[T])(using pos: SourceContext): Rep[Unit]
  def var_divideequals[T:Typ](lhs: Var[T], rhs: Rep[T])(using pos: SourceContext): Rep[Unit]
  
  def __assign[T:Typ](lhs: Var[T], rhs: T)(using pos: SourceContext) = var_assign(lhs, unit(rhs))
  def __assign[T](lhs: Var[T], rhs: Rep[T])(using o: Overloaded1, mT: Typ[T], pos: SourceContext) = var_assign(lhs, rhs)
  def __assign[T](lhs: Var[T], rhs: Var[T])(using o: Overloaded2, mT: Typ[T], pos: SourceContext) = var_assign(lhs, readVar(rhs))
/*
  def __assign[T,U](lhs: Var[T], rhs: Rep[U])(using o: Overloaded2, mT: Typ[T], mU: Typ[U], conv: Rep[U]=>Rep[T]) = var_assign(lhs, conv(rhs))
*/

  // TODO: why doesn't this implicit kick in automatically? <--- do they belong here? maybe better move to NumericOps
  // we really need to refactor this. +=/-= shouldn't be here or in Arith, but in some other type class, which includes Numeric variables
  def infix_+=[T](lhs: Var[T], rhs: T)(using o: Overloaded1, mT: Typ[T], pos: SourceContext) = var_plusequals(lhs, unit(rhs))
  def infix_+=[T](lhs: Var[T], rhs: Rep[T])(using o: Overloaded2, mT: Typ[T], pos: SourceContext) = var_plusequals(lhs,rhs)
  def infix_+=[T](lhs: Var[T], rhs: Var[T])(using o: Overloaded3, mT: Typ[T], pos: SourceContext) = var_plusequals(lhs,readVar(rhs))
  def infix_-=[T](lhs: Var[T], rhs: T)(using o: Overloaded1, mT: Typ[T], pos: SourceContext) = var_minusequals(lhs, unit(rhs))
  def infix_-=[T](lhs: Var[T], rhs: Rep[T])(using o: Overloaded2, mT: Typ[T], pos: SourceContext) = var_minusequals(lhs,rhs)
  def infix_-=[T](lhs: Var[T], rhs: Var[T])(using o: Overloaded3, mT: Typ[T], pos: SourceContext) = var_minusequals(lhs,readVar(rhs))
  def infix_*=[T](lhs: Var[T], rhs: T)(using o: Overloaded1, mT: Typ[T], pos: SourceContext) = var_timesequals(lhs, unit(rhs))
  def infix_*=[T](lhs: Var[T], rhs: Rep[T])(using o: Overloaded2, mT: Typ[T], pos: SourceContext) = var_timesequals(lhs,rhs)
  def infix_*=[T](lhs: Var[T], rhs: Var[T])(using o: Overloaded3, mT: Typ[T], pos: SourceContext) = var_timesequals(lhs,readVar(rhs))
  def infix_/=[T](lhs: Var[T], rhs: T)(using o: Overloaded1, mT: Typ[T], pos: SourceContext) = var_divideequals(lhs, unit(rhs))
  def infix_/=[T](lhs: Var[T], rhs: Rep[T])(using o: Overloaded2, mT: Typ[T], pos: SourceContext) = var_divideequals(lhs,rhs)
  def infix_/=[T](lhs: Var[T], rhs: Var[T])(using o: Overloaded3, mT: Typ[T], pos: SourceContext) = var_divideequals(lhs,readVar(rhs))
}

trait VariablesExp extends Variables with PrimitiveOps with ImplicitOpsExp with VariableImplicits with ReadVarImplicitExp {
  // REMARK:
  // defining Var[T] as Sym[T] is dangerous. If someone forgets to define a more-specific implicit conversion from
  // Var[T] to Ops, e.g. implicit def varToRepStrOps(s: Var[String]) = new RepStrOpsCls(varToRep(s))
  // then the existing implicit from Rep to Ops will be used, and the ReadVar operation will be lost.
  // Defining Vars as separate from Exps will always cause a compile-time error if the implicit is missing.

  //case class Variable[+T](val e: Exp[Variable[T]]) // FIXME: in Expressions because used by codegen...
  type Var[+T] = Variable[T] //FIXME: should be invariant

  implicit def varTyp[T](using ttyp: Typ[T]): Typ[Var[T]] = {
    VariableTyp(ttyp)
  }

  case class ReadVar[T:Typ](v: Var[T]) extends Def[T] {
    def m = (typ[T]: @unchecked)
  }
  case class NewVar[T:Typ](init: Exp[T]) extends Def[Variable[T]] {
    def m = (typ[T]: @unchecked)
  }
  case class Assign[T:Typ](lhs: Var[T], rhs: Exp[T]) extends Def[Unit] {
    def m = (typ[T]: @unchecked)
  }
  case class VarPlusEquals[T:Typ](lhs: Var[T], rhs: Exp[T]) extends Def[Unit] {
    def m = (typ[T]: @unchecked)
  }
  case class VarMinusEquals[T:Typ](lhs: Var[T], rhs: Exp[T]) extends Def[Unit] {
    def m = (typ[T]: @unchecked)
  }
  case class VarTimesEquals[T:Typ](lhs: Var[T], rhs: Exp[T]) extends Def[Unit] {
    def m = (typ[T]: @unchecked)
  }
  case class VarDivideEquals[T:Typ](lhs: Var[T], rhs: Exp[T]) extends Def[Unit] {
    def m = (typ[T]: @unchecked)
  }

  def var_new[T:Typ](init: Exp[T])(using pos: SourceContext): Var[T] = {
    //reflectEffect(NewVar(init)).asInstanceOf[Var[T]]
    Variable(reflectMutable(NewVar(init)))
  }

  def var_assign[T:Typ](lhs: Var[T], rhs: Exp[T])(using pos: SourceContext): Exp[Unit] = {
    reflectWrite(lhs.e)(Assign(lhs, rhs))
    Const(())
  }

  def var_plusequals[T:Typ](lhs: Var[T], rhs: Exp[T])(using pos: SourceContext): Exp[Unit] = {
    reflectWrite(lhs.e)(VarPlusEquals(lhs, rhs))
    Const(())
  }

  def var_minusequals[T:Typ](lhs: Var[T], rhs: Exp[T])(using pos: SourceContext): Exp[Unit] = {
    reflectWrite(lhs.e)(VarMinusEquals(lhs, rhs))
    Const(())
  }
  
  def var_timesequals[T:Typ](lhs: Var[T], rhs: Exp[T])(using pos: SourceContext): Exp[Unit] = {
    reflectWrite(lhs.e)(VarTimesEquals(lhs, rhs))
    Const(())
  }
  
  def var_divideequals[T:Typ](lhs: Var[T], rhs: Exp[T])(using pos: SourceContext): Exp[Unit] = {
    reflectWrite(lhs.e)(VarDivideEquals(lhs, rhs))
    Const(())
  }

  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case NewVar(a) => Nil
    case ReadVar(Variable(a)) => Nil
    case Assign(Variable(a),b) => Nil
    case VarPlusEquals(Variable(a),b) => Nil
    case VarMinusEquals(Variable(a),b) => Nil
    case VarTimesEquals(Variable(a),b) => Nil
    case VarDivideEquals(Variable(a),b) => Nil
    case _ => super.aliasSyms(e)
  }

  override def containSyms(e: Any): List[Sym[Any]] = e match {
    case NewVar(a) => syms(a)
    case ReadVar(Variable(a)) => Nil
    case Assign(Variable(a),b) => syms(b)
    case VarPlusEquals(Variable(a),b) => syms(b)
    case VarMinusEquals(Variable(a),b) => syms(b)
    case VarTimesEquals(Variable(a),b) => syms(b)
    case VarDivideEquals(Variable(a),b) => syms(b)
    case _ => super.containSyms(e)
  }

  override def extractSyms(e: Any): List[Sym[Any]] = e match {
    case NewVar(a) => Nil
    case ReadVar(Variable(a)) => syms(a)
    case Assign(Variable(a),b) => Nil
    case VarPlusEquals(Variable(a),b) => syms(a)
    case VarMinusEquals(Variable(a),b) => syms(a)
    case VarTimesEquals(Variable(a),b) => syms(a)
    case VarDivideEquals(Variable(a),b) => syms(a)
    case _ => super.extractSyms(e)
  }

  override def copySyms(e: Any): List[Sym[Any]] = e match {
    case NewVar(a) => Nil
    case ReadVar(Variable(a)) => Nil
    case Assign(Variable(a),b) => Nil
    case VarPlusEquals(Variable(a),b) => Nil
    case VarMinusEquals(Variable(a),b) => Nil
    case VarTimesEquals(Variable(a),b) => Nil
    case VarDivideEquals(Variable(a),b) => Nil
    case _ => super.copySyms(e)
  }



  override def mirror[A:Typ](e: Def[A], f: Transformer)(using pos: SourceContext): Exp[A] = (e match {
    case ReadVar(Variable(a)) => readVar[A](Variable(f(a)))
    case Reflect(e@NewVar(a), u, es) => reflectMirrored(Reflect(NewVar(f(a))(using e.m), mapOver(f,u), f(es)))(using mtyp1[A], pos)
    case Reflect(ReadVar(Variable(a)), u, es) => reflectMirrored(Reflect(ReadVar[A](Variable(f(a))), mapOver(f,u), f(es)))(using mtyp1[A], pos)
    case Reflect(e@Assign(Variable(a),b), u, es) => reflectMirrored(Reflect(Assign(Variable(f(a)), f(b))(using e.m), mapOver(f,u), f(es)))(using mtyp1[A], pos)
    case Reflect(e@VarPlusEquals(Variable(a),b), u, es) => reflectMirrored(Reflect(VarPlusEquals(Variable(f(a)), f(b))(using e.m), mapOver(f,u), f(es)))(using mtyp1[A], pos)
    case Reflect(e@VarMinusEquals(Variable(a),b), u, es) => reflectMirrored(Reflect(VarMinusEquals(Variable(f(a)), f(b))(using e.m), mapOver(f,u), f(es)))(using mtyp1[A], pos)
    case Reflect(e@VarTimesEquals(Variable(a),b), u, es) => reflectMirrored(Reflect(VarTimesEquals(Variable(f(a)), f(b))(using e.m), mapOver(f,u), f(es)))(using mtyp1[A], pos)
    case Reflect(e@VarDivideEquals(Variable(a),b), u, es) => reflectMirrored(Reflect(VarDivideEquals(Variable(f(a)), f(b))(using e.m), mapOver(f,u), f(es)))(using mtyp1[A], pos)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]

}


trait VariablesExpOpt extends VariablesExp {

  override implicit def readVar[T:Typ](v: Var[T])(using pos: SourceContext) : Exp[T] = {
    if (context ne null) {
      // find the last modification of variable v
      // if it is an assigment, just return the last value assigned 
      val vs = v.e.asInstanceOf[Sym[Variable[T]]]
      //TODO: could use calculateDependencies(Read(v))
      
      val rhs = context.reverse.collectFirst { 
        case w @ Def(Reflect(NewVar(rhs: Exp[?]), _, _)) if w == vs => Some(rhs.asInstanceOf[Exp[T]])
        case Def(Reflect(Assign(`v`, rhs: Exp[?]), _, _)) => Some(rhs.asInstanceOf[Exp[T]])
        case Def(Reflect(_, u, _)) if mayWrite(u, List(vs)) => None // not a simple assignment
      }
      rhs.flatten.getOrElse(super.readVar[T](v))
    } else {
      super.readVar[T](v)
    }
  }
  
  // eliminate (some) redundant stores
  // TODO: strong updates. overwriting a var makes previous stores unnecessary

  override implicit def var_assign[T:Typ](v: Var[T], e: Exp[T])(using pos: SourceContext) : Exp[Unit] = {
    if (context ne null) {
      val vs = v.e.asInstanceOf[Sym[Variable[T]]]
      val skip = context.reverse.collectFirst {
        case w @ Def(Reflect(NewVar(rhs: Exp[?]), _, _)) if w == vs && rhs == e => true
        case Def(Reflect(Assign(`v`, rhs: Exp[?]), _, _)) if rhs == e => true
        case Def(Reflect(_, u, _)) if mayWrite(u, List(vs)) => false
      }
      if (skip.contains(true)) {
        Const(())
      } else {
        super.var_assign(v,e)
      }
    } else {
      super.var_assign(v,e)
    }
  }



}

trait ScalaGenVariables extends ScalaGenEffect {
  val IR: VariablesExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ReadVar(Variable(a)) => emitValDef(sym, quote(a))
    case NewVar(init) => emitVarDef(sym.asInstanceOf[Sym[Variable[Any]]], quote(init))
    case Assign(Variable(a), b) => emitAssignment(a.asInstanceOf[Sym[Variable[Any]]],quote(b))
    case VarPlusEquals(Variable(a), b) => emitValDef(sym, quote(a) + " += " + quote(b))
    case VarMinusEquals(Variable(a), b) => emitValDef(sym, quote(a) + " -= " + quote(b))
    case VarTimesEquals(Variable(a), b) => emitValDef(sym, quote(a) + " *= " + quote(b))
    case VarDivideEquals(Variable(a), b) => emitValDef(sym, quote(a) + " /= " + quote(b))
    case _ => super.emitNode(sym, rhs)
  }

/*
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ReadVar(Variable(a)) => emitValDef(sym, quote(a))
    case NewVar(init) => emitVarDef(sym.asInstanceOf[Sym[Variable[Any]]], quote(init))
    case Assign(Variable(a), b) => emitValDef(sym, quote(a) + " = " + quote(b))
    case VarPlusEquals(Variable(a), b) => emitValDef(sym, quote(a) + " += " + quote(b))
    case VarMinusEquals(Variable(a), b) => emitValDef(sym, quote(a) + " -= " + quote(b))
    case VarTimesEquals(Variable(a), b) => emitValDef(sym, quote(a) + " *= " + quote(b))
    case VarDivideEquals(Variable(a), b) => emitValDef(sym, quote(a) + " /= " + quote(b))
    case _ => super.emitNode(sym, rhs)
  }
*/  
}

trait CLikeGenVariables extends CLikeGenBase {
  val IR: VariablesExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ReadVar(Variable(a)) => emitValDef(sym, quote(a))
    case NewVar(init) => emitVarDef(sym.asInstanceOf[Sym[Variable[Any]]], quote(init))
    case Assign(Variable(a), b) => stream.println(quote(a) + " = " + quote(b) + ";")
    case VarPlusEquals(Variable(a), b) => stream.println(quote(a) + " += " + quote(b) + ";")
    case VarMinusEquals(Variable(a), b) =>stream.println(quote(a) + " -= " + quote(b) + ";")
    case VarTimesEquals(Variable(a), b) => stream.println(quote(a) + " *= " + quote(b) + ";")
    case VarDivideEquals(Variable(a), b) => stream.println(quote(a) + " /= " + quote(b) + ";")
    case _ => super.emitNode(sym, rhs)
  }
}

import lms.gen.{Gen, StagingCompile}
import scala.quoted.*

trait VariablesGen extends Gen with VariablesExp {
  this: StagingCompile =>

  override def interpretDefWithEnv[A](d: Def[A])(using q: Quotes, env: Map[Sym[?], q.reflect.Symbol]): q.reflect.Term = {
    import q.reflect.{Term, asTerm}
    import scala.math.{Fractional, Integral, Numeric}

    def asObjectRefExpr[t: Type](value: Exp[?]): Expr[scala.runtime.ObjectRef[t]] = {
      val cellTerm = interpretExpWithEnv(value)
      q.reflect.Typed(cellTerm, q.reflect.TypeTree.of[scala.runtime.ObjectRef[t]]).asExprOf[scala.runtime.ObjectRef[t]]
    }

    def handle(node: Def[?]): Option[Term] = node match {
      case newVar: NewVar[?] =>
        newVar.m.asTypeRepr.asType match
          case '[t] =>
            val initExpr = interpretExpWithEnv(newVar.init).asExprOf[t]
            Some('{ scala.runtime.ObjectRef.create[t]($initExpr) }.asTerm)
      case read: ReadVar[?] =>
        read.m.asTypeRepr.asType match
          case '[t] =>
            val cell = asObjectRefExpr[t](read.v.e)
            Some('{ $cell.elem }.asTerm)
      case assign: Assign[?] =>
        assign.m.asTypeRepr.asType match
          case '[t] =>
            val cell = asObjectRefExpr[t](assign.lhs.e)
            val rhsExpr = interpretExpWithEnv(assign.rhs).asExprOf[t]
            Some('{ $cell.elem = $rhsExpr; () }.asTerm)
      case plusEq: VarPlusEquals[?] =>
        plusEq.m.asTypeRepr.asType match
          case '[t] =>
            val cell = asObjectRefExpr[t](plusEq.lhs.e)
            val rhsExpr = interpretExpWithEnv(plusEq.rhs).asExprOf[t]
            Expr.summon[Numeric[t]] match
              case Some(numExpr) =>
                val numericExpr = numExpr.asExprOf[Numeric[t]]
                Some('{
                  val ref = $cell
                  ref.elem = $numericExpr.plus(ref.elem, $rhsExpr)
                  ()
                }.asTerm)
              case None =>
                None
      case minusEq: VarMinusEquals[?] =>
        minusEq.m.asTypeRepr.asType match
          case '[t] =>
            val cell = asObjectRefExpr[t](minusEq.lhs.e)
            val rhsExpr = interpretExpWithEnv(minusEq.rhs).asExprOf[t]
            Expr.summon[Numeric[t]] match
              case Some(numExpr) =>
                val numericExpr = numExpr.asExprOf[Numeric[t]]
                Some('{
                  val ref = $cell
                  ref.elem = $numericExpr.minus(ref.elem, $rhsExpr)
                  ()
                }.asTerm)
              case None =>
                None
      case timesEq: VarTimesEquals[?] =>
        timesEq.m.asTypeRepr.asType match
          case '[t] =>
            val cell = asObjectRefExpr[t](timesEq.lhs.e)
            val rhsExpr = interpretExpWithEnv(timesEq.rhs).asExprOf[t]
            Expr.summon[Numeric[t]] match
              case Some(numExpr) =>
                val numericExpr = numExpr.asExprOf[Numeric[t]]
                Some('{
                  val ref = $cell
                  ref.elem = $numericExpr.times(ref.elem, $rhsExpr)
                  ()
                }.asTerm)
              case None =>
                None
      case divEq: VarDivideEquals[?] =>
        divEq.m.asTypeRepr.asType match
          case '[t] =>
            val cell = asObjectRefExpr[t](divEq.lhs.e)
            val rhsExpr = interpretExpWithEnv(divEq.rhs).asExprOf[t]
            val updateExpr =
              Expr.summon[Fractional[t]]
                .map(_.asExprOf[Fractional[t]])
                .map { fracExpr =>
                  '{ 
                    val ref = $cell
                    ref.elem = $fracExpr.div(ref.elem, $rhsExpr)
                    ()
                  }
                }
                .orElse(
                  Expr.summon[Integral[t]]
                    .map(_.asExprOf[Integral[t]])
                    .map { integralExpr =>
                      '{
                        val ref = $cell
                        ref.elem = $integralExpr.quot(ref.elem, $rhsExpr)
                        ()
                      }
                    }
                )
            updateExpr.map(_.asTerm)
      case _ => None
    }

    d match {
      case Reflect(node, _, _) =>
        handle(node).getOrElse(super.interpretDefWithEnv(d))
      case _ =>
        handle(d).getOrElse(super.interpretDefWithEnv(d))
    }
  }
}

trait CudaGenVariables extends CudaGenEffect with CLikeGenVariables
trait OpenCLGenVariables extends OpenCLGenEffect with CLikeGenVariables
trait CGenVariables extends CGenEffect with CLikeGenVariables
