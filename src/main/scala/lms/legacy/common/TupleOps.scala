package lms.legacy.common

import lms.legacy.internal.GenericCodegen

import java.io.PrintWriter
import lms.legacy.compat.SourceContext

import scala.compiletime.deferred
trait TupleOps extends Base {
  given tuple2_typ[A:Typ,B:Typ]: Typ[(A,B)] = deferred
  given tuple3_typ[A:Typ,B:Typ,C:Typ]: Typ[(A,B,C)] = deferred
  given tuple4_typ[A:Typ,B:Typ,C:Typ,D:Typ]: Typ[(A,B,C,D)] = deferred
  given tuple5_typ[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ]: Typ[(A,B,C,D,E)] = deferred
  implicit def make_tuple2[A:Typ,B:Typ](t: (Rep[A], Rep[B]))(using pos: SourceContext) : Rep[(A,B)]
  implicit def make_tuple3[A:Typ,B:Typ,C:Typ](t: (Rep[A], Rep[B], Rep[C]))(using pos: SourceContext) : Rep[(A,B,C)]
  implicit def make_tuple4[A:Typ,B:Typ,C:Typ,D:Typ](t: (Rep[A], Rep[B], Rep[C], Rep[D]))(using pos: SourceContext) : Rep[(A,B,C,D)]
  implicit def make_tuple5[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ](t: (Rep[A], Rep[B], Rep[C], Rep[D], Rep[E]))(using pos: SourceContext) : Rep[(A,B,C,D,E)]

  implicit def t2[A:Typ,B:Typ](t: Rep[(A,B)])(using pos: SourceContext): (Rep[A], Rep[B]) =
    ((tuple2_get1(t),tuple2_get2(t)))
  implicit def t3[A:Typ,B:Typ,C:Typ](t: Rep[(A,B,C)])(using pos: SourceContext): (Rep[A], Rep[B], Rep[C]) =
    ((tuple3_get1(t),tuple3_get2(t),tuple3_get3(t)))
  implicit def t4[A:Typ,B:Typ,C:Typ,D:Typ](t: Rep[(A,B,C,D)])(using pos: SourceContext): (Rep[A], Rep[B], Rep[C], Rep[D]) =
    ((tuple4_get1(t),tuple4_get2(t),tuple4_get3(t),tuple4_get4(t)))
  implicit def t5[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ](t: Rep[(A,B,C,D,E)])(using pos: SourceContext): (Rep[A], Rep[B], Rep[C], Rep[D], Rep[E])=
    ((tuple5_get1(t),tuple5_get2(t),tuple5_get3(t),tuple5_get4(t),tuple5_get5(t)))

  def tuple2_get1[A:Typ](t: Rep[(A,?)])(using pos: SourceContext) : Rep[A]
  def tuple2_get2[B:Typ](t: Rep[(?,B)])(using pos: SourceContext) : Rep[B]

  def tuple3_get1[A:Typ](t: Rep[(A,?,?)])(using pos: SourceContext) : Rep[A]
  def tuple3_get2[B:Typ](t: Rep[(?,B,?)])(using pos: SourceContext) : Rep[B]
  def tuple3_get3[C:Typ](t: Rep[(?,?,C)])(using pos: SourceContext) : Rep[C]

  def tuple4_get1[A:Typ](t: Rep[(A,?,?,?)])(using pos: SourceContext) : Rep[A]
  def tuple4_get2[B:Typ](t: Rep[(?,B,?,?)])(using pos: SourceContext) : Rep[B]
  def tuple4_get3[C:Typ](t: Rep[(?,?,C,?)])(using pos: SourceContext) : Rep[C]
  def tuple4_get4[D:Typ](t: Rep[(?,?,?,D)])(using pos: SourceContext) : Rep[D]

  def tuple5_get1[A:Typ](t: Rep[(A,?,?,?,?)])(using pos: SourceContext) : Rep[A]
  def tuple5_get2[B:Typ](t: Rep[(?,B,?,?,?)])(using pos: SourceContext) : Rep[B]
  def tuple5_get3[C:Typ](t: Rep[(?,?,C,?,?)])(using pos: SourceContext) : Rep[C]
  def tuple5_get4[D:Typ](t: Rep[(?,?,?,D,?)])(using pos: SourceContext) : Rep[D]
  def tuple5_get5[E:Typ](t: Rep[(?,?,?,?,E)])(using pos: SourceContext) : Rep[E]
}

trait TupleOpsExp extends TupleOps with StructExpOpt {
  override given tuple2_typ[A:Typ,B:Typ]: Typ[(A,B)] = {
    implicit val ManifestTyp(mA: Manifest[A]) = typ[A]
    implicit val ManifestTyp(mB: Manifest[B]) = typ[B]
    manifestTyp
  }
  override given tuple3_typ[A:Typ,B:Typ,C:Typ]: Typ[(A,B,C)] = {
    implicit val ManifestTyp(mA: Manifest[A]) = typ[A]
    implicit val ManifestTyp(mB: Manifest[B]) = typ[B]
    implicit val ManifestTyp(mC: Manifest[C]) = typ[C]
    manifestTyp
  }
  override given tuple4_typ[A:Typ,B:Typ,C:Typ,D:Typ]: Typ[(A,B,C,D)] = {
    implicit val ManifestTyp(mA: Manifest[A]) = typ[A]
    implicit val ManifestTyp(mB: Manifest[B]) = typ[B]
    implicit val ManifestTyp(mC: Manifest[C]) = typ[C]
    implicit val ManifestTyp(mD: Manifest[D]) = typ[D]
    manifestTyp
  }
  override given tuple5_typ[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ]: Typ[(A,B,C,D,E)] = {
    implicit val ManifestTyp(mA: Manifest[A]) = typ[A]
    implicit val ManifestTyp(mB: Manifest[B]) = typ[B]
    implicit val ManifestTyp(mC: Manifest[C]) = typ[C]
    implicit val ManifestTyp(mD: Manifest[D]) = typ[D]
    implicit val ManifestTyp(mE: Manifest[E]) = typ[E]
    manifestTyp
  }

  implicit def make_tuple2[A:Typ,B:Typ](t: (Exp[A],Exp[B]))(using pos: SourceContext) : Exp[(A,B)] = struct(classTag[(A,B)], "_1" -> t._1, "_2" -> t._2)
  implicit def make_tuple3[A:Typ,B:Typ,C:Typ](t: (Exp[A],Exp[B],Exp[C]))(using pos: SourceContext) : Exp[(A,B,C)] = struct(classTag[(A,B,C)], "_1" -> t._1, "_2" -> t._2, "_3" -> t._3)
  implicit def make_tuple4[A:Typ,B:Typ,C:Typ,D:Typ](t: (Exp[A],Exp[B],Exp[C],Exp[D]))(using pos: SourceContext) : Exp[(A,B,C,D)] = struct(classTag[(A,B,C,D)], "_1" -> t._1, "_2" -> t._2, "_3" -> t._3, "_4" -> t._4)
  implicit def make_tuple5[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ](t: (Exp[A],Exp[B],Exp[C],Exp[D],Exp[E]))(using pos: SourceContext) : Exp[(A,B,C,D,E)] = struct(classTag[(A,B,C,D,E)], "_1" -> t._1, "_2" -> t._2, "_3" -> t._3, "_4" -> t._4, "_5" -> t._5)

  def tuple2_get1[A:Typ](t: Exp[(A,_)])(using pos: SourceContext) = field[A](t, "_1")
  def tuple2_get2[B:Typ](t: Exp[(_,B)])(using pos: SourceContext) = field[B](t, "_2")

  def tuple3_get1[A:Typ](t: Exp[(A,_,_)])(using pos: SourceContext) = field[A](t, "_1")
  def tuple3_get2[B:Typ](t: Exp[(_,B,_)])(using pos: SourceContext) = field[B](t, "_2")
  def tuple3_get3[C:Typ](t: Exp[(_,_,C)])(using pos: SourceContext) = field[C](t, "_3")

  def tuple4_get1[A:Typ](t: Exp[(A,_,_,_)])(using pos: SourceContext) = field[A](t, "_1")
  def tuple4_get2[B:Typ](t: Exp[(_,B,_,_)])(using pos: SourceContext) = field[B](t, "_2")
  def tuple4_get3[C:Typ](t: Exp[(_,_,C,_)])(using pos: SourceContext) = field[C](t, "_3")
  def tuple4_get4[D:Typ](t: Exp[(_,_,_,D)])(using pos: SourceContext) = field[D](t, "_4")

  def tuple5_get1[A:Typ](t: Exp[(A,_,_,_,_)])(using pos: SourceContext) = field[A](t, "_1")
  def tuple5_get2[B:Typ](t: Exp[(_,B,_,_,_)])(using pos: SourceContext) = field[B](t, "_2")
  def tuple5_get3[C:Typ](t: Exp[(_,_,C,_,_)])(using pos: SourceContext) = field[C](t, "_3")
  def tuple5_get4[D:Typ](t: Exp[(_,_,_,D,_)])(using pos: SourceContext) = field[D](t, "_4")
  def tuple5_get5[E:Typ](t: Exp[(_,_,_,_,E)])(using pos: SourceContext) = field[E](t, "_5")

  object Both { def unapply[T](x:T):Some[(T,T)] = Some((x,x)) }
}

trait TupleGenBase extends GenericCodegen with BaseGenStruct { 
  val IR: TupleOpsExp
  import IR._

  override def remap[A](m: Typ[A]) = m.runtimeClass.getSimpleName match {
    case "Tuple2" => IR.structName(m)
    case "Tuple3" => IR.structName(m)
    case "Tuple4" => IR.structName(m)
    case "Tuple5" => IR.structName(m)
    case _ => super.remap(m)
  }
}

trait ScalaGenTupleOps extends ScalaGenBase with TupleGenBase with ScalaGenStruct { val IR: TupleOpsExp }
trait CGenTupleOps extends CGenBase with TupleGenBase with CGenStruct
trait CudaGenTupleOps extends CudaGenBase with TupleGenBase with CudaGenStruct
trait OpenCLGenTupleOps extends OpenCLGenBase with TupleGenBase with OpenCLGenStruct
