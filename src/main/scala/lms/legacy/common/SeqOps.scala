package lms.legacy.common

import scala.language.implicitConversions

import java.io.PrintWriter
import lms.legacy.internal.*
import lms.legacy.compat.{Manifest, SourceContext}

import scala.compiletime.deferred

trait SeqOps extends Variables {

  given seqTyp[T:Typ]: Typ[Seq[T]] = deferred
  object Seq {
    def apply[A:Typ](xs: Rep[A]*)(using pos: SourceContext) = seq_new(xs)
  }
  
  given varToSeqOps[A:Typ]: Conversion[Var[Seq[A]], SeqOpsCls[A]] with {
  def apply(x: Var[Seq[A]]): SeqOpsCls[A] = new SeqOpsCls(readVar(x))
}
  given repSeqToSeqOps[T:Typ]: Conversion[Rep[Seq[T]], SeqOpsCls[T]] with {
  def apply(a: Rep[Seq[T]]): SeqOpsCls[T] = new SeqOpsCls(a)
}
  given seqToSeqOps[T:Typ]: Conversion[Seq[T], SeqOpsCls[T]] with {
  def apply(a: Seq[T]): SeqOpsCls[T] = new SeqOpsCls(unit(a))
}

  class SeqOpsCls[T:Typ](a: Rep[Seq[T]]){
    def apply(n: Rep[Int])(using pos: SourceContext) = seq_apply(a,n)
    def length(using pos: SourceContext) = seq_length(a)
  }

  def seq_new[A:Typ](xs: Seq[Rep[A]])(using pos: SourceContext): Rep[Seq[A]]
  def seq_apply[T:Typ](x: Rep[Seq[T]], n: Rep[Int])(using pos: SourceContext): Rep[T]
  def seq_length[T:Typ](x: Rep[Seq[T]])(using pos: SourceContext): Rep[Int]
}

trait SeqOpsExp extends SeqOps with PrimitiveOps with EffectExp {
  override given seqTyp[T:Typ]: Typ[Seq[T]] = {
    implicit val m: Manifest[T] = manifestOf[T]
    manifestTyp
  }

  case class SeqNew[A:Typ](xs: List[Rep[A]]) extends Def[Seq[A]] {
    def mA = (typ[A]: @unchecked)
  }
  case class SeqLength[T:Typ](a: Exp[Seq[T]]) extends Def[Int]
  case class SeqApply[T:Typ](x: Exp[Seq[T]], n: Exp[Int]) extends Def[T]
  
  def seq_new[A:Typ](xs: Seq[Rep[A]])(using pos: SourceContext) = SeqNew(xs.toList)
  def seq_apply[T:Typ](x: Exp[Seq[T]], n: Exp[Int])(using pos: SourceContext): Exp[T] = SeqApply(x, n)
  def seq_length[T:Typ](a: Exp[Seq[T]])(using pos: SourceContext): Exp[Int] = SeqLength(a)

  override def mirror[A:Typ](e: Def[A], f: Transformer)(using pos: SourceContext): Exp[A] = (e match {
    case e@SeqNew(xs) => seq_new(f(xs))(using e.mA,pos)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]

  // TODO: need override? (missing data dependency in delite kernel without it...)
  override def syms(e: Any): List[Sym[Any]] = e match {
    case SeqNew(xs) => (xs flatMap { syms }).toList
    case _ => super.syms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case SeqNew(xs) => (xs flatMap { freqNormal }).toList
    case _ => super.symsFreq(e)
  }
}

trait BaseGenSeqOps extends GenericNestedCodegen {
  val IR: SeqOpsExp
  import IR._

}

trait ScalaGenSeqOps extends BaseGenSeqOps with ScalaGenEffect {
  val IR: SeqOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case SeqNew(xs) => emitValDef(sym, src"Seq($xs)")
    case SeqLength(x) => emitValDef(sym, src"$x.length")
    case SeqApply(x,n) => emitValDef(sym, src"$x($n)")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CLikeGenSeqOps extends BaseGenSeqOps with CLikeGenBase  {
  val IR: SeqOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case _ => super.emitNode(sym, rhs)
    }
  }
}

trait CudaGenSeqOps extends CudaGenEffect with CLikeGenSeqOps
trait OpenCLGenSeqOps extends OpenCLGenEffect with CLikeGenSeqOps
trait CGenSeqOps extends CGenEffect with CLikeGenSeqOps
