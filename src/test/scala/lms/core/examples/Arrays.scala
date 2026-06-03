package lms.core.examples

import scala.language.implicitConversions

import lms.gen.{Gen, StagingCompile}
import lms.legacy.common.{Base, BaseExp, ScalaGenBase}

import scala.quoted.*
import scala.reflect.*

trait Arrays extends Base {

  class ArrayOps[T:Typ](x: Rep[Array[T]]) {
    def apply(i: Int) = arrayApply(x, i)
  }
  given array2arrayOps[T:Typ]: Conversion[Rep[Array[T]], ArrayOps[T]] with {
    def apply(x: Rep[Array[T]]): ArrayOps[T] = new ArrayOps(x)
  }

  def arrayApply[T:Typ](x: Rep[Array[T]], i:Int): Rep[T]
  //def arrayUpdate(x: Rep[Double]): Rep[Unit]
  def makeArray[T:Typ](x: List[Rep[T]]): Rep[Array[T]]
}

trait ArraysExp extends Arrays with BaseExp {
  implicit def arrayTyp[T:Typ]: Typ[Array[T]] = (typ[T]: @unchecked).arrayTyp

  trait ArrayDef[T] extends Def[Array[T]]
  trait ElemDef[T] extends Def[T]
  case class ArrayApply[T:Typ](x:Rep[Array[T]], i:Int) extends ElemDef[T]
  //case class ArrayUpdate[T](x:Rep[Array[T]], i:Int) extends Def[T]
  case class MakeArray[T:Typ](x:List[Rep[T]]) extends ArrayDef[T] {
    def m = (typ[T]: @unchecked)
  }

  def arrayApply[T:Typ](x: Rep[Array[T]], i:Int) = ArrayApply(x, i)
  //def arrayUpdate(x: Rep[Double]) = ArrayUpdate(x)
  def makeArray[T:Typ](x: List[Rep[T]]) = MakeArray(x)
}

trait ScalaGenArrays extends ScalaGenBase {
  val IR: ArraysExp
  import IR.*
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ArrayApply(x,i) => emitValDef(sym, src"$x.apply(${i.toString})")
    case a @ MakeArray(x) => emitValDef(sym, src"Array[${a.m}]($x)")
    case _ => super.emitNode(sym, rhs)
  }
}

trait ArraysGen extends Gen with ArraysExp {
  this: StagingCompile =>
  // TODO

  override def interpretDefWithEnv[A](d: Def[A])(using q: Quotes, env: Map[Sym[?], q.reflect.Symbol]): q.reflect.Term = {
    import q.reflect.*
    d match {
      case arrayApply: ArrayApply[?] =>
        val arrayTerm = interpretExpWithEnv(arrayApply.x)(using q, env)
        val indexTerm = Literal(IntConstant(arrayApply.i))
        Apply(Select.unique(arrayTerm, "apply"), List(indexTerm))
      case makeArray: MakeArray[?] =>
        val elems = makeArray.x
        elems.head.tp.asTypeRepr.asType match {
          case '[t] =>
            val elemTerms: List[Term] = elems.map(interpretExpWithEnv(_)(using q, env))
            val elemExprs: List[Expr[t]] = elemTerms.map(_.asExprOf[t])
            val classTag: ClassTag[t] = ClassTag(elems.head.tp.runtimeClass)
            val classTagExpr: Expr[ClassTag[t]] = Expr(classTag)
            '{
              Array.from[t](${Expr.ofList(elemExprs)})(using $classTagExpr)
            }.asTerm
        }
      case _ =>
        super.interpretDefWithEnv(d)
    }
  }
}
