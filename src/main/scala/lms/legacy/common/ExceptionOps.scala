package lms.legacy.common

import scala.language.implicitConversions

import java.io.PrintWriter
import lms.gen.{Gen, StagingCompile}
import lms.legacy.internal._
import lms.legacy.compat.SourceContext
import scala.quoted.*

trait ExceptionOps extends Variables {
  case class VirtualCatchCase[T](exceptionClassName: String, guard: Option[() => Rep[Boolean]], handler: () => Rep[T])

  def __catchCase[T](exceptionClassName: String, handler: => Rep[T]): VirtualCatchCase[T] =
    VirtualCatchCase(exceptionClassName, None, () => handler)

  def __guardedCatchCase[T](exceptionClassName: String, guard: => Rep[Boolean], handler: => Rep[T]): VirtualCatchCase[T] =
    VirtualCatchCase(exceptionClassName, Some(() => guard), () => handler)

  def __tryCatch[T:Typ](body: => Rep[T], catches: VirtualCatchCase[T]*)(using pos: SourceContext): Rep[T]
  def __tryCatchFinally[T:Typ](body: => Rep[T], catches: VirtualCatchCase[T]*)(finalizer: => Rep[Unit])(using pos: SourceContext): Rep[T]
  
  def fatal(m: Rep[String]) = throw_exception(m)
  
  def throw_exception(m: Rep[String]): Rep[Unit] = throw_exception_class("java.lang.Exception", m)
  def throw_exception_class(exceptionClassName: String, m: Rep[String]): Rep[Unit]
}

trait ExceptionOpsExp extends ExceptionOps with EffectExp with StringOpsExp {
  case class TryCatch[T:Typ](body: Block[T], catches: List[(String, Option[Block[Boolean]], Block[T])], finalizer: Option[Block[Unit]]) extends Def[T]
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
    __tryCatchFinally(body, catches*)(Const(()))
  }

  def __tryCatchFinally[T:Typ](body: => Rep[T], catches: VirtualCatchCase[T]*)(finalizer: => Rep[Unit])(using pos: SourceContext): Rep[T] = {
    val bodyBlock = reifyEffects(body)
    val catchBlocks = catches.toList.map { c =>
      (c.exceptionClassName, c.guard.map(g => reifyEffects(g())), reifyEffects(c.handler()))
    }
    val finalizerBlock = reifyEffects(finalizer)
    val bodyEffects = summarizeEffects(bodyBlock)
    val catchEffects = catchBlocks.foldLeft(Pure()) { (acc, c) =>
      val guardEffects = c._2.map(summarizeEffects).getOrElse(Pure())
      val handlerEffects = summarizeEffects(c._3)
      infix_orElse(acc, infix_andThen(guardEffects, handlerEffects))
    }
    val finalizerEffects = summarizeEffects(finalizerBlock)
    val finalSummary =
      if finalizerEffects == Pure() then None
      else Some(finalizerBlock)
    reflectEffectInternal(TryCatch(bodyBlock, catchBlocks, finalSummary), infix_andThen(infix_andThen(bodyEffects, catchEffects), finalizerEffects))
  }

  def throw_exception_class(exceptionClassName: String, m: Exp[String]) = reflectEffect(ThrowException(exceptionClassName, m), Global())    
  
  override def mirrorDef[A:Typ](e: Def[A], f: Transformer)(using pos: SourceContext): Def[A] = e match {
    case TryCatch(body, catches, finalizer) =>
      TryCatch[A](f(body), catches.map { case (exceptionClassName, guard, handler) => (exceptionClassName, guard.map(f(_)), f(handler)) }, finalizer.map(f(_)))
    case _ =>
      super.mirrorDef(e, f)
  }

  override def mirror[A:Typ](e: Def[A], f: Transformer)(using pos: SourceContext): Exp[A] = (e match {
    case Reflect(TryCatch(body, catches, finalizer), u, es) =>
      if (f.hasContext) {
        val mirroredCatches: List[VirtualCatchCase[A]] =
          catches.map { case (exceptionClassName, guard, handler) =>
            VirtualCatchCase[A](
              exceptionClassName,
              guard.map(g => () => f.reflectBlock(g).asInstanceOf[Exp[Boolean]]),
              () => f.reflectBlock(handler).asInstanceOf[Exp[A]]
            )
          }
        finalizer match {
          case Some(fin) =>
            __tryCatchFinally[A](f.reflectBlock(body), mirroredCatches*)(f.reflectBlock(fin))
          case None =>
            __tryCatch[A](f.reflectBlock(body), mirroredCatches*)
        }
      } else {
        reflectMirrored(Reflect(TryCatch[A](f(body), catches.map { case (exceptionClassName, guard, handler) => (exceptionClassName, guard.map(f(_)), f(handler)) }, finalizer.map(f(_))), mapOver(f, u), f(es)))(using mtyp1[A], pos)
      }
    case Reflect(ThrowException(exceptionClassName, s), u, es) =>
      reflectMirrored(Reflect(ThrowException(exceptionClassName, f(s)), mapOver(f,u), f(es)))(using mtyp1[A], pos)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]  

  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case TryCatch(body, catches, finalizer) => syms(body) ::: catches.flatMap { case (_, guard, handler) => guard.toList.flatMap(syms) ::: syms(handler) } ::: finalizer.toList.flatMap(syms)
    case _ => super.aliasSyms(e)
  }

  override def containSyms(e: Any): List[Sym[Any]] = e match {
    case TryCatch(_, _, _) => Nil
    case _ => super.containSyms(e)
  }

  override def extractSyms(e: Any): List[Sym[Any]] = e match {
    case TryCatch(_, _, _) => Nil
    case _ => super.extractSyms(e)
  }

  override def copySyms(e: Any): List[Sym[Any]] = e match {
    case TryCatch(_, _, _) => Nil
    case _ => super.copySyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case TryCatch(body, catches, finalizer) =>
      freqHot(body) ++ catches.flatMap { case (_, guard, handler) => guard.toList.flatMap(freqCold) ++ freqCold(handler) } ++ finalizer.toList.flatMap(freqCold)
    case _ =>
      super.symsFreq(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case TryCatch(body, catches, finalizer) =>
      blockEffectSyms(body) ::: catches.flatMap { case (_, guard, handler) => guard.toList.flatMap(blockEffectSyms) ::: blockEffectSyms(handler) } ::: finalizer.toList.flatMap(blockEffectSyms)
    case _ =>
      super.boundSyms(e)
  }
}

trait ScalaGenExceptionOps extends ScalaGenBase {
  val IR: ExceptionOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case TryCatch(body, catches, finalizer) =>
      val bodyAny = body.asInstanceOf[Block[Any]]
      stream.println("val " + quote(sym) + " = try {")
      emitBlock(bodyAny)
      stream.println(quote(getBlockResult(bodyAny)))
      stream.println("} catch {")
      catches.foreach { case (exceptionClassName, guard, handler) =>
        val handlerAny = handler.asInstanceOf[Block[Any]]
        stream.println(s"case _: $exceptionClassName" + guard.map(_ => " if {").getOrElse(" =>"))
        guard.foreach { guardBlock =>
          val guardAny = guardBlock.asInstanceOf[Block[Any]]
          emitBlock(guardAny)
          stream.println(quote(getBlockResult(guardAny)) + " } =>")
        }
        if guard.isEmpty then stream.println()
        emitBlock(handlerAny)
        stream.println(quote(getBlockResult(handlerAny)))
      }
      finalizer.foreach { fin =>
        val finAny = fin.asInstanceOf[Block[Any]]
        stream.println("} finally {")
        emitBlock(finAny)
        stream.println(quote(getBlockResult(finAny)))
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

    def interpretTryCatch[T](body: this.Block[T], catches: List[(String, Option[this.Block[Boolean]], this.Block[T])], finalizer: Option[this.Block[Unit]]): Term = {
      val bodyTerm = interpretBlockWithEffectOrder(body)(using q, env)
      val valueType = body.res.tp.asTypeRepr
      valueType.asType match {
        case '[t] =>
          def buildCases(rest: List[(String, Option[this.Block[Boolean]], this.Block[T])]): List[CaseDef] =
            rest.map { case (exceptionClassName, guard, handler) =>
              val exceptionType = Symbol.requiredClass(exceptionClassName).typeRef
              val guardTerm = guard.map(g => interpretBlockWithEffectOrder(g)(using q, env))
              val handlerTerm = interpretBlockWithEffectOrder(handler)(using q, env).asExprOf[t]
              CaseDef(Typed(Wildcard(), TypeTree.of(using exceptionType.asType)), guardTerm, handlerTerm.asTerm)
            }
          val finalizerTerm = finalizer.map(fin => interpretBlockWithEffectOrder(fin)(using q, env))
          Try(bodyTerm, buildCases(catches), finalizerTerm)
      }
    }

    d match {
      case Reflect(TryCatch(body, catches, finalizer), _, _) =>
        interpretTryCatch(body, catches, finalizer)
      case TryCatch(body, catches, finalizer) =>
        interpretTryCatch(body, catches, finalizer)
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
