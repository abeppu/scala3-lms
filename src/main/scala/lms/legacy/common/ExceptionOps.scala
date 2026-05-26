package lms.legacy.common

import scala.language.implicitConversions

import java.io.PrintWriter
import lms.gen.{Gen, StagingCompile}
import lms.legacy.internal._
import lms.legacy.compat.SourceContext
import scala.quoted.*

trait ExceptionOps extends Variables {
  case class VirtualCatchCase[T](exceptionClassName: String, handler: () => Rep[T])

  def __catchCase[T](exceptionClassName: String, handler: => Rep[T]): VirtualCatchCase[T] =
    VirtualCatchCase(exceptionClassName, () => handler)

  def __tryCatch[T:Typ](body: => Rep[T], catches: VirtualCatchCase[T]*)(using pos: SourceContext): Rep[T]
  
  def fatal(m: Rep[String]) = throw_exception(m)
  
  def throw_exception(m: Rep[String]): Rep[Unit] = throw_exception_class("java.lang.Exception", m)
  def throw_exception_class(exceptionClassName: String, m: Rep[String]): Rep[Unit]
}

trait ExceptionOpsExp extends ExceptionOps with EffectExp with StringOpsExp {
  case class TryCatch[T:Typ](body: Block[T], catches: List[(String, Block[T])]) extends Def[T]
  case class ThrowException(exceptionClassName: String, m: Rep[String]) extends Def[Unit]
  
  private def blockEffectSyms(block: Block[?]): List[Sym[Any]] = block.res match {
    case Def(Reify(_, _, effects)) =>
      effects.asInstanceOf[List[Sym[Any]]]
    case sym: Sym[?] =>
      findDefinition(sym.asInstanceOf[Sym[Any]]) match {
        case Some(TP(_, reify: Reify[?])) =>
          reify.effects.asInstanceOf[List[Sym[Any]]]
        case _ =>
          effectSyms(block.res)
      }
    case _ =>
      effectSyms(block.res)
  }

  def __tryCatch[T:Typ](body: => Rep[T], catches: VirtualCatchCase[T]*)(using pos: SourceContext): Rep[T] = {
    val bodyBlock = reifyEffects(body)
    val catchBlocks = catches.toList.map(c => c.exceptionClassName -> reifyEffects(c.handler()))
    val bodyEffects = summarizeEffects(bodyBlock)
    val catchEffects = catchBlocks.map(_._2).map(summarizeEffects).foldLeft(Pure())(infix_orElse)
    reflectEffectInternal(TryCatch(bodyBlock, catchBlocks), infix_andThen(bodyEffects, catchEffects))
  }

  def throw_exception_class(exceptionClassName: String, m: Exp[String]) = reflectEffect(ThrowException(exceptionClassName, m), Global())    
  
  override def mirrorDef[A:Typ](e: Def[A], f: Transformer)(using pos: SourceContext): Def[A] = e match {
    case TryCatch(body, catches) =>
      TryCatch[A](f(body), catches.map { case (exceptionClassName, handler) => exceptionClassName -> f(handler) })
    case _ =>
      super.mirrorDef(e, f)
  }

  override def mirror[A:Typ](e: Def[A], f: Transformer)(using pos: SourceContext): Exp[A] = (e match {
    case Reflect(TryCatch(body, catches), u, es) =>
      if (f.hasContext) {
        val mirroredCatches: List[VirtualCatchCase[A]] =
          catches.map { case (exceptionClassName, handler) =>
            VirtualCatchCase[A](exceptionClassName, () => f.reflectBlock(handler).asInstanceOf[Exp[A]])
          }
        __tryCatch[A](f.reflectBlock(body), mirroredCatches*)
      } else {
        reflectMirrored(Reflect(TryCatch[A](f(body), catches.map { case (exceptionClassName, handler) => exceptionClassName -> f(handler) }), mapOver(f, u), f(es)))(using mtyp1[A], pos)
      }
    case Reflect(ThrowException(exceptionClassName, s), u, es) =>
      reflectMirrored(Reflect(ThrowException(exceptionClassName, f(s)), mapOver(f,u), f(es)))(using mtyp1[A], pos)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]  

  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case TryCatch(body, catches) => syms(body) ::: catches.flatMap { case (_, handler) => syms(handler) }
    case _ => super.aliasSyms(e)
  }

  override def containSyms(e: Any): List[Sym[Any]] = e match {
    case TryCatch(_, _) => Nil
    case _ => super.containSyms(e)
  }

  override def extractSyms(e: Any): List[Sym[Any]] = e match {
    case TryCatch(_, _) => Nil
    case _ => super.extractSyms(e)
  }

  override def copySyms(e: Any): List[Sym[Any]] = e match {
    case TryCatch(_, _) => Nil
    case _ => super.copySyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case TryCatch(body, catches) =>
      freqHot(body) ++ catches.flatMap { case (_, handler) => freqCold(handler) }
    case _ =>
      super.symsFreq(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case TryCatch(body, catches) =>
      blockEffectSyms(body) ::: catches.flatMap { case (_, handler) => blockEffectSyms(handler) }
    case _ =>
      super.boundSyms(e)
  }
}

trait ScalaGenExceptionOps extends ScalaGenBase {
  val IR: ExceptionOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case TryCatch(body, catches) =>
      val bodyAny = body.asInstanceOf[Block[Any]]
      stream.println("val " + quote(sym) + " = try {")
      emitBlock(bodyAny)
      stream.println(quote(getBlockResult(bodyAny)))
      stream.println("} catch {")
      catches.foreach { case (exceptionClassName, handler) =>
        val handlerAny = handler.asInstanceOf[Block[Any]]
        stream.println(s"case _: $exceptionClassName =>")
        emitBlock(handlerAny)
        stream.println(quote(getBlockResult(handlerAny)))
      }
      stream.println("}")
    case ThrowException(exceptionClassName, m) =>
      emitValDef(sym, s"throw new $exceptionClassName(${quote(m)})")
    case _ => super.emitNode(sym, rhs)
  }
}

trait ExceptionOpsGen extends Gen with ExceptionOpsExp {
  this: StagingCompile =>

  override def interpretDefWithEnv[A](d: Def[A])(using q: Quotes, env: Map[Sym[?], q.reflect.Symbol]): q.reflect.Term = {
    import q.reflect.*

    def interpretTryCatch[T](body: this.Block[T], catches: List[(String, this.Block[T])]): Term = {
      val bodyTerm = interpretBlockWithVars(body)(using q, env)
      val valueType = body.res.tp.asTypeRepr
      valueType.asType match {
        case '[t] =>
          def buildCases(rest: List[(String, this.Block[T])]): List[CaseDef] =
            rest.map { case (exceptionClassName, handler) =>
              val exceptionType = Symbol.requiredClass(exceptionClassName).typeRef
              val handlerTerm = interpretBlockWithVars(handler)(using q, env).asExprOf[t]
              CaseDef(Typed(Wildcard(), TypeTree.of(using exceptionType.asType)), None, handlerTerm.asTerm)
            }
          Try(bodyTerm, buildCases(catches), None)
      }
    }

    d match {
      case Reflect(TryCatch(body, catches), _, _) =>
        interpretTryCatch(body, catches)
      case TryCatch(body, catches) =>
        interpretTryCatch(body, catches)
      case Reflect(ThrowException(exceptionClassName, m), _, _) =>
        val message = interpretExpWithEnv(m).asExprOf[String]
        '{ throw java.lang.Class.forName(${Expr(exceptionClassName)}).getConstructor(classOf[String]).newInstance($message).asInstanceOf[Throwable] }.asTerm
      case ThrowException(exceptionClassName, m) =>
        val message = interpretExpWithEnv(m).asExprOf[String]
        '{ throw java.lang.Class.forName(${Expr(exceptionClassName)}).getConstructor(classOf[String]).newInstance($message).asInstanceOf[Throwable] }.asTerm
      case _ =>
        super.interpretDefWithEnv(d)
    }
  }
}

trait CLikeGenExceptionOps extends CLikeGenBase {
  val IR: ExceptionOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ThrowException(_, m) => 
      stream.println("printf(" + quote(m) + ".c_str());")
      stream.println("assert(false);")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CGenExceptionOps extends CGenBase with CLikeGenExceptionOps
trait CudaGenExceptionOps extends CudaGenBase with CLikeGenExceptionOps {
  val IR: ExceptionOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ThrowException(_, m) =>
      stream.println("printf(" + quote(m) + ");")
      stream.println("assert(false);")
    case _ => super.emitNode(sym, rhs)
  }
}
//OpenCL does not support printf within a kernel
//trait OpenCLGenExceptionOps extends OpenCLGenBase with CLikeGenExceptionOps
