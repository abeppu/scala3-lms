package lms.legacy.common

import scala.language.implicitConversions

import lms.gen.{Gen, StagingCompile}

import java.io.PrintWriter
import lms.legacy.util.OverloadHack
import lms.legacy.compat.SourceContext

import scala.quoted.*
import scala.math.Ordering.Implicits.given

trait OrderingOps extends Base with Variables with BooleanOps with PrimitiveOps with OverloadHack {
  // workaround for infix not working with implicits in PrimitiveOps
  given orderingToOrderingOps[T:Ordering:Typ]: Conversion[T, OrderingOpsCls[T]] with {
    def apply(n: T): OrderingOpsCls[T] = new OrderingOpsCls(unit(n))
  }
  given repOrderingToOrderingOps[T:Ordering:Typ]: Conversion[Rep[T], OrderingOpsCls[T]] with {
    def apply(n: Rep[T]): OrderingOpsCls[T] = new OrderingOpsCls(n)
  }
  given varOrderingToOrderingOps[T:Ordering:Typ]: Conversion[Var[T], OrderingOpsCls[T]] with {
    def apply(n: Var[T]): OrderingOpsCls[T] = new OrderingOpsCls(readVar(n))
  }

  class OrderingOpsCls[T:Ordering:Typ](lhs: Rep[T]){
    def <       (rhs: Rep[T])(using pos: SourceContext) = ordering_lt(lhs, rhs)
    def <=      (rhs: Rep[T])(using pos: SourceContext) = ordering_lteq(lhs, rhs)
    def >       (rhs: Rep[T])(using pos: SourceContext) = ordering_gt(lhs, rhs)
    def >=      (rhs: Rep[T])(using pos: SourceContext) = ordering_gteq(lhs, rhs)
    def equiv   (rhs: Rep[T])(using pos: SourceContext) = ordering_equiv(lhs, rhs)
    def max     (rhs: Rep[T])(using pos: SourceContext) = ordering_max(lhs, rhs)
    def min     (rhs: Rep[T])(using pos: SourceContext) = ordering_min(lhs, rhs)
    def compare (rhs: Rep[T])(using pos: SourceContext) = ordering_compare(lhs, rhs)

    def <       [B](rhs: B)(using c: B => Rep[T], pos: SourceContext) = ordering_lt(lhs, c(rhs))
    def <=      [B](rhs: B)(using c: B => Rep[T], pos: SourceContext) = ordering_lteq(lhs, c(rhs))
    def >       [B](rhs: B)(using c: B => Rep[T], pos: SourceContext) = ordering_gt(lhs, c(rhs))
    def >=      [B](rhs: B)(using c: B => Rep[T], pos: SourceContext) = ordering_gteq(lhs, c(rhs))
    def equiv   [B](rhs: B)(using c: B => Rep[T], pos: SourceContext) = ordering_equiv(lhs, c(rhs))
    def max     [B](rhs: B)(using c: B => Rep[T], pos: SourceContext) = ordering_max(lhs, c(rhs))
    def min     [B](rhs: B)(using c: B => Rep[T], pos: SourceContext) = ordering_min(lhs, c(rhs))
    def compare [B](rhs: B)(using c: B => Rep[T], pos: SourceContext) = ordering_compare(lhs, c(rhs))
  }

  def ordering_lt      [T:Ordering:Typ](lhs: Rep[T], rhs: Rep[T])(using pos: SourceContext): Rep[Boolean]
  def ordering_lteq    [T:Ordering:Typ](lhs: Rep[T], rhs: Rep[T])(using pos: SourceContext): Rep[Boolean]
  def ordering_gt      [T:Ordering:Typ](lhs: Rep[T], rhs: Rep[T])(using pos: SourceContext): Rep[Boolean]
  def ordering_gteq    [T:Ordering:Typ](lhs: Rep[T], rhs: Rep[T])(using pos: SourceContext): Rep[Boolean]
  def ordering_equiv   [T:Ordering:Typ](lhs: Rep[T], rhs: Rep[T])(using pos: SourceContext): Rep[Boolean]
  def ordering_max     [T:Ordering:Typ](lhs: Rep[T], rhs: Rep[T])(using pos: SourceContext): Rep[T]
  def ordering_min     [T:Ordering:Typ](lhs: Rep[T], rhs: Rep[T])(using pos: SourceContext): Rep[T]
  def ordering_compare [T:Ordering:Typ](lhs: Rep[T], rhs: Rep[T])(using pos: SourceContext): Rep[Int]
}


trait OrderingOpsExp extends OrderingOps with VariablesExp {
  abstract class OrderingDefMN[T:Ordering:Typ,A] extends Def[A] {
    def mev = (typ[T]: @unchecked)
    def aev = implicitly[Ordering[T]]
  }
  case class OrderingLT      [T:Ordering:Typ](lhs: Exp[T], rhs: Exp[T]) extends OrderingDefMN[T,Boolean]
  case class OrderingLTEQ    [T:Ordering:Typ](lhs: Exp[T], rhs: Exp[T]) extends OrderingDefMN[T,Boolean]
  case class OrderingGT      [T:Ordering:Typ](lhs: Exp[T], rhs: Exp[T]) extends OrderingDefMN[T,Boolean]
  case class OrderingGTEQ    [T:Ordering:Typ](lhs: Exp[T], rhs: Exp[T]) extends OrderingDefMN[T,Boolean]
  case class OrderingEquiv   [T:Ordering:Typ](lhs: Exp[T], rhs: Exp[T]) extends OrderingDefMN[T,Boolean]
  case class OrderingMax     [T:Ordering:Typ](lhs: Exp[T], rhs: Exp[T]) extends OrderingDefMN[T,T]
  case class OrderingMin     [T:Ordering:Typ](lhs: Exp[T], rhs: Exp[T]) extends OrderingDefMN[T,T]
  case class OrderingCompare [T:Ordering:Typ](lhs: Exp[T], rhs: Exp[T]) extends OrderingDefMN[T,Int]

  def ordering_lt     [T:Ordering:Typ](lhs: Exp[T], rhs: Exp[T])(using pos: SourceContext): Rep[Boolean] = OrderingLT(lhs,rhs)
  def ordering_lteq   [T:Ordering:Typ](lhs: Exp[T], rhs: Exp[T])(using pos: SourceContext): Rep[Boolean] = OrderingLTEQ(lhs,rhs)
  def ordering_gt     [T:Ordering:Typ](lhs: Exp[T], rhs: Exp[T])(using pos: SourceContext): Rep[Boolean] = OrderingGT(lhs,rhs)
  def ordering_gteq   [T:Ordering:Typ](lhs: Exp[T], rhs: Exp[T])(using pos: SourceContext): Rep[Boolean] = OrderingGTEQ(lhs,rhs)
  def ordering_equiv  [T:Ordering:Typ](lhs: Exp[T], rhs: Exp[T])(using pos: SourceContext): Rep[Boolean] = OrderingEquiv(lhs,rhs)
  def ordering_max    [T:Ordering:Typ](lhs: Exp[T], rhs: Exp[T])(using pos: SourceContext): Rep[T]       = OrderingMax(lhs,rhs)
  def ordering_min    [T:Ordering:Typ](lhs: Exp[T], rhs: Exp[T])(using pos: SourceContext): Rep[T]       = OrderingMin(lhs,rhs)
  def ordering_compare[T:Ordering:Typ](lhs: Exp[T], rhs: Exp[T])(using pos: SourceContext): Rep[Int]     = OrderingCompare(lhs,rhs)

  override def mirror[A:Typ](e: Def[A], f: Transformer)(using pos: SourceContext): Exp[A] = (e match {
    case e@OrderingLT(a,b)                      => ordering_lt(f(a),f(b))(using e.aev,e.mev,pos)
    case e@OrderingLTEQ(a,b)                    => ordering_lteq(f(a),f(b))(using e.aev,e.mev,pos)
    case e@OrderingGT(a,b)                      => ordering_gt(f(a),f(b))(using e.aev,e.mev,pos)
    case e@OrderingGTEQ(a,b)                    => ordering_gteq(f(a),f(b))(using e.aev,e.mev,pos)
    case e@OrderingEquiv(a,b)                   => ordering_equiv(f(a),f(b))(using e.aev,e.mev,pos)
    case e@OrderingMax(a,b)                     => ordering_max(f(a),f(b))(using e.aev.asInstanceOf[Ordering[A]],mtype(e.mev),pos)
    case e@OrderingMin(a,b)                     => ordering_min(f(a),f(b))(using e.aev.asInstanceOf[Ordering[A]],mtype(e.mev),pos)
    case e@OrderingCompare(a,b)                 => ordering_compare(f(a),f(b))(using e.aev,e.mev,pos)
    case Reflect(e@OrderingLT(a,b), u, es)      => reflectMirrored(Reflect(OrderingLT(f(a),f(b))(using e.aev,e.mev), mapOver(f,u), f(es)))(using mtyp1[A], pos)
    case Reflect(e@OrderingLTEQ(a,b), u, es)    => reflectMirrored(Reflect(OrderingLTEQ(f(a),f(b))(using e.aev,e.mev), mapOver(f,u), f(es)))(using mtyp1[A], pos)
    case Reflect(e@OrderingGT(a,b), u, es)      => reflectMirrored(Reflect(OrderingGT(f(a),f(b))(using e.aev,e.mev), mapOver(f,u), f(es)))(using mtyp1[A], pos)
    case Reflect(e@OrderingGTEQ(a,b), u, es)    => reflectMirrored(Reflect(OrderingGTEQ(f(a),f(b))(using e.aev,e.mev), mapOver(f,u), f(es)))(using mtyp1[A], pos)
    case Reflect(e@OrderingEquiv(a,b), u, es)   => reflectMirrored(Reflect(OrderingEquiv(f(a),f(b))(using e.aev,e.mev), mapOver(f,u), f(es)))(using mtyp1[A], pos)
    case Reflect(e@OrderingMax(a,b), u, es)     => reflectMirrored(Reflect(OrderingMax(f(a),f(b))(using e.aev.asInstanceOf[Ordering[A]],mtype(e.mev)), mapOver(f,u), f(es)))(using mtyp1[A], pos)
    case Reflect(e@OrderingMin(a,b), u, es)     => reflectMirrored(Reflect(OrderingMin(f(a),f(b))(using e.aev.asInstanceOf[Ordering[A]],mtype(e.mev)), mapOver(f,u), f(es)))(using mtyp1[A], pos)
    case Reflect(e@OrderingCompare(a,b), u, es) => reflectMirrored(Reflect(OrderingCompare(f(a),f(b))(using e.aev,e.mev), mapOver(f,u), f(es)))(using mtyp1[A], pos)
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]]
}

/**
 * @author  Alen Stojanov (astojanov@inf.ethz.ch)
 */
trait OrderingOpsExpOpt extends OrderingOpsExp {

  override def ordering_lt[T:Ordering:Typ](lhs: Exp[T], rhs: Exp[T])(using pos: SourceContext): Rep[Boolean] = (lhs, rhs) match {
    case (Const(a), Const(b)) => Const(implicitly[Ordering[T]].lt(a, b))
    case (a, b) if a.equals(b) => Const(false)
    case _ => super.ordering_lt(lhs, rhs)
  }

  override def ordering_lteq[T:Ordering:Typ](lhs: Exp[T], rhs: Exp[T])(using pos: SourceContext): Rep[Boolean] = (lhs, rhs) match {
    case (Const(a), Const(b)) => Const(implicitly[Ordering[T]].lteq(a, b))
    case (a, b) if a.equals(b) => Const(true)
    case _ => super.ordering_lteq(lhs, rhs)
  }

  override def ordering_gt[T:Ordering:Typ](lhs: Exp[T], rhs: Exp[T])(using pos: SourceContext): Rep[Boolean] = (lhs, rhs) match {
    case (Const(a), Const(b)) => Const(implicitly[Ordering[T]].gt(a, b))
    case (a, b) if a.equals(b) => Const(false)
    case _ => super.ordering_gt(lhs, rhs)
  }

  override def ordering_gteq[T:Ordering:Typ](lhs: Exp[T], rhs: Exp[T])(using pos: SourceContext): Rep[Boolean] = (lhs, rhs) match {
    case (Const(a), Const(b)) => Const(implicitly[Ordering[T]].gteq(a, b))
    case (a, b) if a.equals(b) => Const(true)
    case _ => super.ordering_gteq(lhs, rhs)
  }

  override def ordering_equiv[T:Ordering:Typ](lhs: Exp[T], rhs: Exp[T])(using pos: SourceContext): Rep[Boolean] = (lhs, rhs) match {
    case (Const(a), Const(b)) => Const(implicitly[Ordering[T]].equiv(a, b))
    case (a, b) if a.equals(b) => Const(true)
    case _ => super.ordering_equiv(lhs, rhs)
  }

  override def ordering_max[T:Ordering:Typ](lhs: Exp[T], rhs: Exp[T])(using pos: SourceContext): Rep[T] = (lhs, rhs) match {
    case (Const(a), Const(b)) => Const(implicitly[Ordering[T]].max(a, b))
    case (a, b) if a.equals(b) => a
    case _ => super.ordering_max(lhs, rhs)
  }

  override def ordering_min[T:Ordering:Typ](lhs: Exp[T], rhs: Exp[T])(using pos: SourceContext): Rep[T] = (lhs, rhs) match {
    case (Const(a), Const(b)) => Const(implicitly[Ordering[T]].min(a, b))
    case (a, b) if a.equals(b) => a
    case _ => super.ordering_min(lhs, rhs)
  }

  override def ordering_compare[T:Ordering:Typ](lhs: Exp[T], rhs: Exp[T])(using pos: SourceContext): Rep[Int] = (lhs, rhs) match {
    case (Const(a), Const(b)) => Const(implicitly[Ordering[T]].compare(a, b))
    case (a, b) if a.equals(b) => Const[Int](0)
    case _ => super.ordering_compare(lhs, rhs)
  }

}


trait OrderingOpsGen extends Gen with OrderingOpsExp { this: StagingCompile =>

  override def interpretDefWithEnv[A](d: Def[A])(using q: Quotes, env: Map[Sym[?], q.reflect.Symbol]): q.reflect.Term = {
    import q.reflect.*

    def orderingMethodTerm(lhs: Exp[?], rhs: Exp[?], methodName: String): Term = {
      val lhsTerm = interpretExpWithEnv(lhs)
      val rhsTerm = interpretExpWithEnv(rhs)
      lhs.tp.asTypeRepr.asType match {
        case '[Int] =>
          val lhsExpr = lhsTerm.asExprOf[Int]
          val rhsExpr = rhsTerm.asExprOf[Int]
          methodName match {
            case "lt" => '{ $lhsExpr < $rhsExpr }.asTerm
            case "lteq" => '{ $lhsExpr <= $rhsExpr }.asTerm
            case "gt" => '{ $lhsExpr > $rhsExpr }.asTerm
            case "gteq" => '{ $lhsExpr >= $rhsExpr }.asTerm
            case "equiv" => '{ $lhsExpr == $rhsExpr }.asTerm
            case "max" => '{ scala.math.Ordering.Int.max($lhsExpr, $rhsExpr) }.asTerm
            case "min" => '{ scala.math.Ordering.Int.min($lhsExpr, $rhsExpr) }.asTerm
            case "compare" => '{ scala.math.Ordering.Int.compare($lhsExpr, $rhsExpr) }.asTerm
            case other =>
              report.errorAndAbort(s"Unsupported ordering method $other for Int")
          }
        case '[t] =>
          val ordTerm = Expr.summon[Ordering[t]]
            .map(_.asTerm)
            .orElse {
              Type.of[t] match
                case '[Int] => Some('{ scala.math.Ordering.Int }.asTerm)
                case '[Long] => Some('{ scala.math.Ordering.Long }.asTerm)
                case '[Short] => Some('{ scala.math.Ordering.Short }.asTerm)
                case '[Byte] => Some('{ scala.math.Ordering.Byte }.asTerm)
                case '[Char] => Some('{ scala.math.Ordering.Char }.asTerm)
                case '[Double] => Some('{ scala.math.Ordering.Double }.asTerm)
                case '[Float] => Some('{ scala.math.Ordering.Float }.asTerm)
                case '[Boolean] => Some('{ scala.math.Ordering.Boolean }.asTerm)
                case _ => None
            }
            .getOrElse(report.errorAndAbort(s"Missing implicit Ordering for ${lhs.tp} (${lhs.tp.asTypeRepr.show})"))
          val methodSymbol = ordTerm.tpe.classSymbol
            .flatMap(_.methodMember(methodName).headOption)
            .getOrElse(report.errorAndAbort(s"Method $methodName not found on Ordering[${lhs.tp}]"))
          Apply(Select(ordTerm, methodSymbol), List(lhsTerm, rhsTerm))
      }
    }

    d match {
      case OrderingLT(lhs, rhs) =>
        orderingMethodTerm(lhs, rhs, "lt")
      case OrderingLTEQ(lhs, rhs) =>
        orderingMethodTerm(lhs, rhs, "lteq")
      case OrderingGT(lhs, rhs) =>
        orderingMethodTerm(lhs, rhs, "gt")
      case OrderingGTEQ(lhs, rhs) =>
        orderingMethodTerm(lhs, rhs, "gteq")
      case OrderingEquiv(lhs, rhs) =>
        orderingMethodTerm(lhs, rhs, "equiv")
      case OrderingMax(lhs, rhs) =>
        orderingMethodTerm(lhs, rhs, "max")
      case OrderingMin(lhs, rhs) =>
        orderingMethodTerm(lhs, rhs, "min")
      case OrderingCompare(lhs, rhs) =>
        orderingMethodTerm(lhs, rhs, "compare")
      case _ =>
        super.interpretDefWithEnv(d)
    }
  }
}


trait ScalaGenOrderingOps extends ScalaGenBase {
  val IR: OrderingOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case OrderingLT(a,b) => emitValDef(sym, src"$a < $b")
    case OrderingLTEQ(a,b) => emitValDef(sym, src"$a <= $b")
    case OrderingGT(a,b) => emitValDef(sym, src"$a > $b")
    case OrderingGTEQ(a,b) => emitValDef(sym, src"$a >= $b")
    case OrderingEquiv(a,b) => emitValDef(sym, src"$a equiv $b")
    // "$a max $b" is wrong for Strings because it tries to use `StringLike.max(Ordering)`
    // can't compare with typ[String] without extending StringOps
    case c@OrderingMax(a,b) =>
      val rhs = if (c.mev.runtimeClass == classOf[String])
        src"scala.math.Ordering.String.max($a, $b)"
      else
        src"$a max $b"
      emitValDef(sym, rhs)
    case c@OrderingMin(a,b) =>
      val rhs = if (c.mev.runtimeClass == classOf[String])
        src"scala.math.Ordering.String.min($a, $b)"
      else
        src"$a min $b"
      emitValDef(sym, rhs)
    case c@OrderingCompare(a,b) => c.mev match {
      case m if m == (typ[Int]: @unchecked) => emitValDef(sym, "java.lang.Integer.compare("+quote(a)+","+quote(b)+")")
      case m if m == (typ[Long]: @unchecked) => emitValDef(sym, "java.lang.Long.compare("+quote(a)+","+quote(b)+")")
      case m if m == (typ[Double]: @unchecked) => emitValDef(sym, "java.lang.Double.compare("+quote(a)+","+quote(b)+")")
      case m if m == (typ[Float]: @unchecked) => emitValDef(sym, "java.lang.Float.compare("+quote(a)+","+quote(b)+")")
      case m if m == (typ[Boolean]: @unchecked) => emitValDef(sym, "java.lang.Boolean.compare("+quote(a)+","+quote(b)+")")
      case m if m == (typ[Byte]: @unchecked) => emitValDef(sym, "java.lang.Byte.compare("+quote(a)+","+quote(b)+")")
      case m if m == (typ[Char]: @unchecked) => emitValDef(sym, "java.lang.Character.compare("+quote(a)+","+quote(b)+")")
      case m if m == (typ[Short]: @unchecked) => emitValDef(sym, "java.lang.Short.compare("+quote(a)+","+quote(b)+")")
      case _ => emitValDef(sym, quote(a) + " compare " + quote(b))
    }
    case _ => super.emitNode(sym, rhs)
  }
}

trait CLikeGenOrderingOps extends CLikeGenBase {
  val IR: OrderingOpsExp
  import IR._
  
  // TODO: Add MIN/MAX macro needs to C-like header file
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
      rhs match {
        case OrderingLT(a,b) =>
          emitValDef(sym, src"$a < $b")
        case OrderingLTEQ(a,b) =>
          emitValDef(sym, src"$a <= $b")
        case OrderingGT(a,b) =>
          emitValDef(sym, src"$a > $b")
        case OrderingGTEQ(a,b) =>
          emitValDef(sym, src"$a >= $b")
        case OrderingEquiv(a,b) =>
          emitValDef(sym, src"$a == $b")
        case OrderingMax(a,b) =>
          //emitValDef(sym, quote(a) + ">" + quote(b) + "?" + quote(a) + ":" + quote(b))
          emitValDef(sym, src"MAX($a, $b)")
        case OrderingMin(a,b) =>
          //emitValDef(sym, quote(a) + "<" + quote(b) + "?" + quote(a) + ":" + quote(b))
          emitValDef(sym, src"MIN($a, $b)")
        case OrderingCompare(a,b) =>
          emitValDef(sym, src"($a < $b) ? -1 : ($a == $b) ? 0 : 1")
        case _ => super.emitNode(sym, rhs)
      }
    }
}

trait CudaGenOrderingOps extends CudaGenBase with CLikeGenOrderingOps
trait OpenCLGenOrderingOps extends OpenCLGenBase with CLikeGenOrderingOps
trait CGenOrderingOps extends CGenBase with CLikeGenOrderingOps
