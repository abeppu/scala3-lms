package lms.legacy.common

import scala.language.implicitConversions

import java.io.PrintWriter
import lms.gen.{Gen, StagingCompile}
import lms.legacy.internal._
import lms.legacy.compat.SourceContext
import scala.quoted.*

trait MiscOps extends Base with PrimitiveOps with StringOps {
  /**
   * Other things that need to get lifted like exit, there should be
   * a better way to do this
   */

  def print(x: Rep[Any])(using pos: SourceContext): Rep[Unit]
  def println(x: Rep[Any])(using pos: SourceContext): Rep[Unit]
  def printf(f: String, x: Rep[Any]*)(using pos: SourceContext): Rep[Unit]

  // TODO: there is no way to override this behavior
  def exit(status: Int)(using pos: SourceContext): Rep[Unit] = exit(unit(status))
  def exit()(using pos: SourceContext): Rep[Unit] = exit(0)
  def exit(status: Rep[Int])(using pos: SourceContext): Rep[Unit]
  def error(s: Rep[String])(using pos: SourceContext): Rep[Unit]
  def returnL[T:Typ](x: Rep[T])(using pos: SourceContext): Rep[Nothing]
}



trait MiscOpsExp extends MiscOps with EffectExp with PrimitiveOpsExp with StringOpsExp {
  given nothingTyp: Typ[Nothing] with
    def typeArguments: List[Typ[?]] = Nil
    def arrayTyp: Typ[Array[Nothing]] =
      ManifestTyp(lms.legacy.compat.Manifest.of[Array[Any]]).asInstanceOf[Typ[Array[Nothing]]]
    def runtimeClass: java.lang.Class[?] = classOf[Null]
    def <:<(that: Typ[?]): Boolean = true
    override def asTypeRepr(using q: Quotes): q.reflect.TypeRepr = q.reflect.TypeRepr.of[Nothing]
    override def toString: String = "Nothing"

  case class Print(x: Exp[Any]) extends Def[Unit]
  case class PrintLn(x: Exp[Any]) extends Def[Unit]
  case class PrintF(f: String, x: List[Exp[Any]]) extends Def[Unit]
  case class Exit(s: Exp[Int]) extends Def[Unit]
  case class Error(s: Exp[String]) extends Def[Unit]
  case class Return[T:Typ](x: Exp[T]) extends Def[Nothing] {
    def m = (typ[T]: @unchecked)
  }

  def print(x: Exp[Any])(using pos: SourceContext) = reflectEffect(Print(x)) // TODO: simple effect
  def println(x: Exp[Any])(using pos: SourceContext) = reflectEffect(PrintLn(x)) // TODO: simple effect
  def printf(f: String, x: Rep[Any]*)(using pos: SourceContext): Rep[Unit] = reflectEffect(PrintF(f, x.toList))
  def exit(s: Exp[Int])(using pos: SourceContext) = reflectEffect(Exit(s))
  def error(s: Exp[String])(using pos: SourceContext) = reflectEffect(Error(s))
  def returnL[T:Typ](x: Exp[T])(using pos: SourceContext): Exp[Nothing] = {
    printlog("warning: staged return statements are unlikely to work because the surrounding source method does not exist in the generated code.")
    printsrc(raw"in ${quotePos(x)}")
    reflectEffect(Return(x), infix_andAlso(Global(), Control()))
  }
  
  override def mirror[A:Typ](e: Def[A], f: Transformer)(using pos: SourceContext): Exp[A] = (e match {
    case Reflect(Error(x), u, es) => reflectMirrored(Reflect(Error(f(x)), mapOver(f,u), f(es)))(using mtyp1[A], pos)
    case Reflect(Print(x), u, es) => reflectMirrored(Reflect(Print(f(x)), mapOver(f,u), f(es)))(using mtyp1[A], pos)
    case Reflect(PrintLn(x), u, es) => reflectMirrored(Reflect(PrintLn(f(x)), mapOver(f,u), f(es)))(using mtyp1[A], pos)
    case Reflect(PrintF(fm,x), u, es) => reflectMirrored(Reflect(PrintF(fm,f(x)), mapOver(f,u), f(es)))(using mtyp1[A], pos)
    case Reflect(Exit(x), u, es) => reflectMirrored(Reflect(Exit(f(x)), mapOver(f,u), f(es)))(using mtyp1[A], pos)
    case Reflect(r @ Return(x), u, es) => reflectMirrored(Reflect(Return(f(x))(using r.m), mapOver(f,u), f(es)))(using mtyp1[A], pos)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]
}

trait ScalaGenMiscOps extends ScalaGenEffect {
  val IR: MiscOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case PrintF(f,xs) => emitValDef(sym, src"printf(${f::xs})")
    case PrintLn(s) => emitValDef(sym, src"println($s)")
    case Print(s) => emitValDef(sym, src"print($s)")
    case Exit(a) => emitValDef(sym, src"exit($a)")
    case Return(x) => stream.println(src"return $x")
    case Error(s) => emitValDef(sym, src"error($s)")
    case _ => super.emitNode(sym, rhs)
  }
}


trait CGenMiscOps extends CGenEffect {
  val IR: MiscOpsExp
  import IR._

  def format(s: Exp[Any]): String = {
    remap(s.tp) match {
      case "uint16_t" => "%c"
      case "bool" | "int8_t" | "int16_t" | "int32_t" => "%d"
      case "int64_t" => "%ld"
      case "float" | "double" => "%f"
      case "string" => "%s" 
      case _ => throw new GenerationFailedException("CGenMiscOps: cannot print type " + remap(s.tp))
    }
  }

  def quoteRawString(s: Exp[Any]): String = {
    remap(s.tp) match {
      case "string" => quote(s) + ".c_str()"
      case _ => quote(s)
    }
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case PrintF(f,x) => stream.println("printf(" + ((Const(f:String)::x).map(quoteRawString)).mkString(",") + ");")
    case PrintLn(s) => stream.println("printf(\"" + format(s) + "\\n\"," + quoteRawString(s) + ");")
    case Print(s) => stream.println("printf(\"" + format(s) + "\"," + quoteRawString(s) + ");")
    case Exit(a) => stream.println("exit(" + quote(a) + ");")
    case Return(x) => stream.println("return " + quote(x) + ";")
    case Error(s) => stream.println("error(-1,0,\"%s\"," + quote(s) + ");")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenMiscOps extends CudaGenEffect {
  val IR: MiscOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}


trait OpenCLGenMiscOps extends OpenCLGenEffect {
  val IR: MiscOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}

trait MiscOpsGen extends Gen with MiscOpsExp {
  this: StagingCompile =>

  override def interpretDefWithEnv[A](d: Def[A])(using q: Quotes, env: Map[Sym[?], q.reflect.Symbol]): q.reflect.Term = {
    import q.reflect.*

    def interpretReturn(ret: this.Return[?]): Term =
      ret.m.asTypeRepr.asType match {
        case '[t] =>
          val value = interpretExpWithEnv(ret.x.asInstanceOf[Exp[t]])(using q, env)
          q.reflect.Return(value, runtimeReturnTarget)
      }

    d match {
      case Reflect(ret: this.Return[?], _, _) =>
        interpretReturn(ret)
      case ret: this.Return[?] =>
        interpretReturn(ret)
      case _ =>
        super.interpretDefWithEnv(d)
    }
  }
}
