package lms.legacy.common

import java.io.PrintWriter
import lms.legacy.internal.GenericNestedCodegen
import lms.legacy.compat.SourceContext

import scala.compiletime.deferred
trait ListOps extends Variables {

  given listTyp[T:Typ]: Typ[List[T]] = deferred
  object List {
    def apply[A:Typ](xs: Rep[A]*)(using pos: SourceContext) = list_new(xs)
  }

  given varToListOps[T:Typ]: Conversion[Var[List[T]], ListOpsCls[T]] with {
  def apply(x: Var[List[T]]): ListOpsCls[T] = new ListOpsCls(readVar(x)) // FIXME: dep on var is not nice
}
  given repToListOps[T:Typ]: Conversion[Rep[List[T]], ListOpsCls[T]] with {
  def apply(a: Rep[List[T]]): ListOpsCls[T] = new ListOpsCls(a)
}
  given listToListOps[T:Typ]: Conversion[List[T], ListOpsCls[T]] with {
  def apply(a: List[T]): ListOpsCls[T] = new ListOpsCls(unit(a))
}
  
  class ListOpsCls[A:Typ](l: Rep[List[A]]) {
    def map[B:Typ](f: Rep[A] => Rep[B]) = list_map(l,f)
    def flatMap[B : Typ](f: Rep[A] => Rep[List[B]]) = list_flatMap(l,f)
    def filter(f: Rep[A] => Rep[Boolean]) = list_filter(l, f)
    def sortBy[B:Typ:Ordering](f: Rep[A] => Rep[B]) = list_sortby(l,f)
    def ::(e: Rep[A]) = list_prepend(l,e)
    def ++ (l2: Rep[List[A]]) = list_concat(l, l2)
    def mkString = list_mkString(l)
    def mkString(s:Rep[String]) = list_mkString2(l,s)
    def head = list_head(l)
    def tail = list_tail(l)
    def isEmpty = list_isEmpty(l)
    def toArray = list_toarray(l)
    def toSeq = list_toseq(l)
  }
  
  def list_new[A:Typ](xs: Seq[Rep[A]])(using pos: SourceContext): Rep[List[A]]
  def list_fromseq[A:Typ](xs: Rep[Seq[A]])(using pos: SourceContext): Rep[List[A]]
  def list_map[A:Typ,B:Typ](l: Rep[List[A]], f: Rep[A] => Rep[B])(using pos: SourceContext): Rep[List[B]]
  def list_flatMap[A : Typ, B : Typ](xs: Rep[List[A]], f: Rep[A] => Rep[List[B]])(using pos: SourceContext): Rep[List[B]]
  def list_filter[A : Typ](l: Rep[List[A]], f: Rep[A] => Rep[Boolean])(using pos: SourceContext): Rep[List[A]]
  def list_sortby[A:Typ,B:Typ:Ordering](l: Rep[List[A]], f: Rep[A] => Rep[B])(using pos: SourceContext): Rep[List[A]]
  def list_prepend[A:Typ](l: Rep[List[A]], e: Rep[A])(using pos: SourceContext): Rep[List[A]]
  def list_toarray[A:Typ](l: Rep[List[A]])(using pos: SourceContext): Rep[Array[A]]
  def list_toseq[A:Typ](l: Rep[List[A]])(using pos: SourceContext): Rep[Seq[A]]
  def list_concat[A:Typ](xs: Rep[List[A]], ys: Rep[List[A]])(using pos: SourceContext): Rep[List[A]]
  def list_cons[A:Typ](x: Rep[A], xs: Rep[List[A]])(using pos: SourceContext): Rep[List[A]] // FIXME remove?
  def list_mkString[A : Typ](xs: Rep[List[A]])(using pos: SourceContext): Rep[String]
  def list_mkString2[A : Typ](xs: Rep[List[A]], sep:Rep[String])(using pos: SourceContext): Rep[String]
  def list_head[A:Typ](xs: Rep[List[A]])(using pos: SourceContext): Rep[A]
  def list_tail[A:Typ](xs: Rep[List[A]])(using pos: SourceContext): Rep[List[A]]
  def list_isEmpty[A:Typ](xs: Rep[List[A]])(using pos: SourceContext): Rep[Boolean]
}

trait ListOpsExp extends ListOps with EffectExp with VariablesExp with BooleanOpsExp with ArrayOpsExp with StringOpsExp {
  override given listTyp[T:Typ]: Typ[List[T]] = manifestTyp
  case class ListNew[A:Typ](xs: Seq[Rep[A]]) extends Def[List[A]] {
    def mA = (typ[A]: @unchecked)
  }
  case class ListFromSeq[A:Typ](xs: Rep[Seq[A]]) extends Def[List[A]]
  case class ListMap[A:Typ,B:Typ](l: Exp[List[A]], x: Sym[A], block: Block[B]) extends Def[List[B]]
  case class ListFlatMap[A:Typ, B:Typ](l: Exp[List[A]], x: Sym[A], block: Block[List[B]]) extends Def[List[B]]
  case class ListFilter[A : Typ](l: Exp[List[A]], x: Sym[A], block: Block[Boolean]) extends Def[List[A]]
  case class ListSortBy[A:Typ,B:Typ:Ordering](l: Exp[List[A]], x: Sym[A], block: Block[B]) extends Def[List[A]]
  case class ListPrepend[A:Typ](x: Exp[List[A]], e: Exp[A]) extends Def[List[A]]
  case class ListToArray[A:Typ](x: Exp[List[A]]) extends Def[Array[A]]
  case class ListToSeq[A:Typ](x: Exp[List[A]]) extends Def[Seq[A]]
  case class ListConcat[A:Typ](xs: Rep[List[A]], ys: Rep[List[A]]) extends Def[List[A]]
  case class ListCons[A:Typ](x: Rep[A], xs: Rep[List[A]]) extends Def[List[A]]
  case class ListMkString[A:Typ](l: Exp[List[A]]) extends Def[String]
  case class ListMkString2[A:Typ](l: Exp[List[A]], s: Exp[String]) extends Def[String]
  case class ListHead[A:Typ](xs: Rep[List[A]]) extends Def[A]
  case class ListTail[A:Typ](xs: Rep[List[A]]) extends Def[List[A]]
  case class ListIsEmpty[A:Typ](xs: Rep[List[A]]) extends Def[Boolean]
  
  def list_new[A:Typ](xs: Seq[Rep[A]])(using pos: SourceContext) = ListNew(xs)
  def list_fromseq[A:Typ](xs: Rep[Seq[A]])(using pos: SourceContext) = ListFromSeq(xs)
  def list_map[A:Typ,B:Typ](l: Exp[List[A]], f: Exp[A] => Exp[B])(using pos: SourceContext) = {
    val a = fresh[A]
    val b = reifyEffects(f(a))
    reflectEffect(ListMap(l, a, b), infix_star(summarizeEffects(b)))
  }
  def list_flatMap[A:Typ, B:Typ](l: Exp[List[A]], f: Exp[A] => Exp[List[B]])(using pos: SourceContext) = {
    val a = fresh[A]
    val b = reifyEffects(f(a))
    reflectEffect(ListFlatMap(l, a, b), infix_star(summarizeEffects(b)))
  }
  def list_filter[A : Typ](l: Exp[List[A]], f: Exp[A] => Exp[Boolean])(using pos: SourceContext) = {
    val a = fresh[A]
    val b = reifyEffects(f(a))
    reflectEffect(ListFilter(l, a, b), infix_star(summarizeEffects(b)))
  }
  def list_sortby[A:Typ,B:Typ:Ordering](l: Exp[List[A]], f: Exp[A] => Exp[B])(using pos: SourceContext) = {
    val a = fresh[A]
    val b = reifyEffects(f(a))
    reflectEffect(ListSortBy(l, a, b), infix_star(summarizeEffects(b)))
  }
  def list_toarray[A:Typ](l: Exp[List[A]])(using pos: SourceContext) = ListToArray(l)
  def list_toseq[A:Typ](l: Exp[List[A]])(using pos: SourceContext) = ListToSeq(l)
  def list_prepend[A:Typ](l: Exp[List[A]], e: Exp[A])(using pos: SourceContext) = ListPrepend(l,e)
  def list_concat[A:Typ](xs: Rep[List[A]], ys: Rep[List[A]])(using pos: SourceContext) = ListConcat(xs,ys)
  def list_cons[A:Typ](x: Rep[A], xs: Rep[List[A]])(using pos: SourceContext) = ListCons(x,xs)
  def list_mkString[A:Typ](l: Exp[List[A]])(using pos: SourceContext) = ListMkString(l)
  def list_mkString2[A:Typ](l: Rep[List[A]], sep:Rep[String])(using pos: SourceContext) = ListMkString2(l,sep)
  def list_head[A:Typ](xs: Rep[List[A]])(using pos: SourceContext) = ListHead(xs)
  def list_tail[A:Typ](xs: Rep[List[A]])(using pos: SourceContext) = ListTail(xs)
  def list_isEmpty[A:Typ](xs: Rep[List[A]])(using pos: SourceContext) = ListIsEmpty(xs)
  
  override def mirror[A:Typ](e: Def[A], f: Transformer)(using pos: SourceContext): Exp[A] = (e match {
    case e@ListNew(xs) => list_new(f(xs))(using e.mA,pos)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]] // why??
  
  override def syms(e: Any): List[Sym[Any]] = e match {
    case ListMap(a, x, body) => syms(a):::syms(body)
    case ListFlatMap(a, _, body) => syms(a) ::: syms(body)
    case ListFilter(a, _, body) => syms(a) ::: syms(body)
    case ListSortBy(a, x, body) => syms(a):::syms(body)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case ListMap(a, x, body) => x :: effectSyms(body)
    case ListFlatMap(_, x, body) => x :: effectSyms(body)
    case ListFilter(_, x, body) => x :: effectSyms(body)
    case ListSortBy(a, x, body) => x :: effectSyms(body)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case ListMap(a, x, body) => freqNormal(a):::freqHot(body)
    case ListFlatMap(a, _, body) => freqNormal(a) ::: freqHot(body)
    case ListFilter(a, _, body) => freqNormal(a) ::: freqHot(body)
    case ListSortBy(a, x, body) => freqNormal(a):::freqHot(body)
    case _ => super.symsFreq(e)
  }  
}

trait ListOpsExpOpt extends ListOpsExp {
  override def list_concat[A : Typ](xs1: Exp[List[A]], xs2: Exp[List[A]])(using pos: SourceContext): Exp[List[A]] = (xs1, xs2) match {
    case (Def(ListNew(xs1)), Def(ListNew(xs2))) => ListNew(xs1 ++ xs2)
    case (Def(ListNew(Seq())), xs2) => xs2
    case (xs1, Def(ListNew(Seq()))) => xs1
    case _ => super.list_concat(xs1, xs2)
  }
}

trait BaseGenListOps extends GenericNestedCodegen {
  val IR: ListOpsExp
  import IR._

}

trait ScalaGenListOps extends BaseGenListOps with ScalaGenEffect {
  val IR: ListOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ListNew(xs) => emitValDef(sym, src"List(${(xs map {quote}).mkString(",")})")
    case ListConcat(xs,ys) => emitValDef(sym, src"$xs ::: $ys")
    case ListCons(x, xs) => emitValDef(sym, src"$x :: $xs")
    case ListHead(xs) => emitValDef(sym, src"$xs.head")
    case ListTail(xs) => emitValDef(sym, src"$xs.tail")
    case ListIsEmpty(xs) => emitValDef(sym, src"$xs.isEmpty")
    case ListFromSeq(xs) => emitValDef(sym, src"List($xs*)")
    case ListMkString(xs) => emitValDef(sym, src"$xs.mkString")
    case ListMkString2(xs,s) => emitValDef(sym, src"$xs.mkString($s)")
    case ListMap(l,x,blk) => 
      gen"""val $sym = $l.map { $x => 
           |${nestedBlock(blk)}
           |$blk
           |}"""
    case ListFlatMap(l, x, b) =>
      gen"""val $sym = $l.flatMap { $x => 
           |${nestedBlock(b)}
           |$b
           |}"""
    case ListFilter(l, x, b) =>
      gen"""val $sym = $l.filter { $x => 
           |${nestedBlock(b)}
           |$b
           |}"""
    case ListSortBy(l,x,blk) =>
      gen"""val $sym = $l.sortBy { $x => 
           |${nestedBlock(blk)}
           |$blk
           |}"""
    case ListPrepend(l,e) => emitValDef(sym, src"$e :: $l")    
    case ListToArray(l) => emitValDef(sym, src"$l.toArray")
    case ListToSeq(l) => emitValDef(sym, src"$l.toSeq")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CLikeGenListOps extends BaseGenListOps with CLikeGenBase {
  val IR: ListOpsExp
  import IR._

/*
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
      rhs match {
        case _ => super.emitNode(sym, rhs)
      }
    }
*/    
}

trait CudaGenListOps extends CudaGenEffect with CLikeGenListOps
trait OpenCLGenListOps extends OpenCLGenEffect with CLikeGenListOps
trait CGenListOps extends CGenEffect with CLikeGenListOps
