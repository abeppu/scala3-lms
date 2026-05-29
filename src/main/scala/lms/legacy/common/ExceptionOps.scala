package lms.legacy.common

import scala.language.implicitConversions

import java.io.PrintWriter
import lms.gen.{Gen, StagingCompile}
import lms.legacy.internal._
import lms.legacy.compat.SourceContext
import scala.quoted.*

trait ExceptionOps extends Variables {
  case class VirtualCatchCase[T](
    exceptionClassName: String,
    message: Option[Rep[String]],
    guard: Option[() => Rep[Boolean]],
    handler: () => Rep[T]
  )

  def __catchCase[T](exceptionClassName: String, handler: => Rep[T]): VirtualCatchCase[T] =
    VirtualCatchCase(exceptionClassName, None, None, () => handler)

  def __guardedCatchCase[T](exceptionClassName: String, guard: => Rep[Boolean], handler: => Rep[T]): VirtualCatchCase[T] =
    VirtualCatchCase(exceptionClassName, None, Some(() => guard), () => handler)

  def __catchCaseWithMessage[T](exceptionClassName: String, message: Rep[String], handler: => Rep[T]): VirtualCatchCase[T] =
    VirtualCatchCase(exceptionClassName, Some(message), None, () => handler)

  def __guardedCatchCaseWithMessage[T](exceptionClassName: String, message: Rep[String], guard: => Rep[Boolean], handler: => Rep[T]): VirtualCatchCase[T] =
    VirtualCatchCase(exceptionClassName, Some(message), Some(() => guard), () => handler)

  def __catchMessage: Rep[String]

  def __tryCatch[T:Typ](body: => Rep[T], catches: VirtualCatchCase[T]*)(using pos: SourceContext): Rep[T]
  def __tryCatchFinally[T:Typ](body: => Rep[T], catches: VirtualCatchCase[T]*)(finalizer: => Rep[Unit])(using pos: SourceContext): Rep[T]
  
  def fatal(m: Rep[String]) = throw_exception(m)
  
  def throw_exception(m: Rep[String]): Rep[Unit] = throw_exception_class("java.lang.Exception", m)
  def throw_exception_class(exceptionClassName: String, m: Rep[String]): Rep[Unit]
  def throw_exception_class_with_cause(exceptionClassName: String, m: Rep[String], causeClassName: String, causeMessage: Rep[String]): Rep[Unit]
}

trait ExceptionOpsExp extends ExceptionOps with EffectExp with StringOpsExp {
  case class ReifiedCatch[T](exceptionClassName: String, message: Option[Exp[String]], guard: Option[Block[Boolean]], handler: Block[T])
  case class TryCatch[T:Typ](body: Block[T], catches: List[ReifiedCatch[T]], finalizer: Option[Block[Unit]]) extends Def[T]
  case class ThrowException(exceptionClassName: String, m: Rep[String]) extends Def[Unit]
  case class ThrowExceptionWithCause(exceptionClassName: String, m: Rep[String], causeClassName: String, causeMessage: Rep[String]) extends Def[Unit]

  def __catchMessage: Rep[String] =
    fresh[String]
  
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
      ReifiedCatch(c.exceptionClassName, c.message, c.guard.map(g => reifyEffects(g())), reifyEffects(c.handler()))
    }
    val finalizerBlock = reifyEffects(finalizer)
    val bodyEffects = summarizeEffects(bodyBlock)
    val catchEffects = catchBlocks.foldLeft(Pure()) { (acc, c) =>
      val guardEffects = c.guard.map(summarizeEffects).getOrElse(Pure())
      val handlerEffects = summarizeEffects(c.handler)
      infix_orElse(acc, infix_andThen(guardEffects, handlerEffects))
    }
    val finalizerEffects = summarizeEffects(finalizerBlock)
    val finalSummary =
      if finalizerEffects == Pure() then None
      else Some(finalizerBlock)
    reflectEffectInternal(TryCatch(bodyBlock, catchBlocks, finalSummary), infix_andThen(infix_andThen(bodyEffects, catchEffects), finalizerEffects))
  }

  def throw_exception_class(exceptionClassName: String, m: Exp[String]) = reflectEffect(ThrowException(exceptionClassName, m), Global())    
  def throw_exception_class_with_cause(exceptionClassName: String, m: Exp[String], causeClassName: String, causeMessage: Exp[String]) =
    reflectEffect(ThrowExceptionWithCause(exceptionClassName, m, causeClassName, causeMessage), Global())
  
  override def mirrorDef[A:Typ](e: Def[A], f: Transformer)(using pos: SourceContext): Def[A] = e match {
    case TryCatch(body, catches, finalizer) =>
      TryCatch[A](f(body), catches.map(c => ReifiedCatch(c.exceptionClassName, c.message.map(f(_)), c.guard.map(f(_)), f(c.handler))), finalizer.map(f(_)))
    case ThrowExceptionWithCause(exceptionClassName, m, causeClassName, causeMessage) =>
      ThrowExceptionWithCause(exceptionClassName, f(m), causeClassName, f(causeMessage))
    case _ =>
      super.mirrorDef(e, f)
  }

  override def mirror[A:Typ](e: Def[A], f: Transformer)(using pos: SourceContext): Exp[A] = (e match {
    case Reflect(TryCatch(body, catches, finalizer), u, es) =>
      if (f.hasContext) {
        val mirroredCatches: List[VirtualCatchCase[A]] =
          catches.map { c =>
            VirtualCatchCase[A](
              c.exceptionClassName,
              c.message.map(f(_).asInstanceOf[Exp[String]]),
              c.guard.map(g => () => f.reflectBlock(g).asInstanceOf[Exp[Boolean]]),
              () => f.reflectBlock(c.handler).asInstanceOf[Exp[A]]
            )
          }
        finalizer match {
          case Some(fin) =>
            __tryCatchFinally[A](f.reflectBlock(body), mirroredCatches*)(f.reflectBlock(fin))
          case None =>
            __tryCatch[A](f.reflectBlock(body), mirroredCatches*)
        }
      } else {
        reflectMirrored(Reflect(TryCatch[A](f(body), catches.map(c => ReifiedCatch(c.exceptionClassName, c.message.map(f(_).asInstanceOf[Exp[String]]), c.guard.map(f(_)), f(c.handler))), finalizer.map(f(_))), mapOver(f, u), f(es)))(using mtyp1[A], pos)
      }
    case Reflect(ThrowException(exceptionClassName, s), u, es) =>
      reflectMirrored(Reflect(ThrowException(exceptionClassName, f(s)), mapOver(f,u), f(es)))(using mtyp1[A], pos)
    case Reflect(ThrowExceptionWithCause(exceptionClassName, s, causeClassName, causeMessage), u, es) =>
      reflectMirrored(Reflect(ThrowExceptionWithCause(exceptionClassName, f(s), causeClassName, f(causeMessage)), mapOver(f, u), f(es)))(using mtyp1[A], pos)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]  

  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case TryCatch(body, _, finalizer) => syms(body) ::: finalizer.toList.flatMap(syms)
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
    case TryCatch(body, _, finalizer) =>
      freqHot(body) ++ finalizer.toList.flatMap(freqCold)
    case _ =>
      super.symsFreq(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case TryCatch(body, catches, finalizer) =>
      blockEffectSyms(body) :::
        catches.flatMap(c =>
          c.message.collect { case s: Sym[?] => s.asInstanceOf[Sym[Any]] }.toList :::
            c.guard.toList.flatMap(syms) :::
            syms(c.handler) :::
            c.guard.toList.flatMap(blockEffectSyms) :::
            blockEffectSyms(c.handler)
        ) :::
        finalizer.toList.flatMap(blockEffectSyms)
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
      catches.zipWithIndex.foreach { case (c, index) =>
        val handlerAny = c.handler.asInstanceOf[Block[Any]]
        val binderName = c.message.map(_ => s"e$index")
        val pattern = binderName.map(name => s"$name: ${c.exceptionClassName}").getOrElse(s"_: ${c.exceptionClassName}")
        stream.println(s"case $pattern" + c.guard.map(_ => " if {").getOrElse(" =>"))
        c.guard.foreach { guardBlock =>
          val guardAny = guardBlock.asInstanceOf[Block[Any]]
          c.message.foreach(message => stream.println("val " + quote(message) + " = " + binderName.get + ".getMessage"))
          emitBlock(guardAny)
          stream.println(quote(getBlockResult(guardAny)) + " } =>")
        }
        if c.guard.isEmpty then stream.println()
        c.message.foreach(message => stream.println("val " + quote(message) + " = " + binderName.get + ".getMessage"))
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
    case ThrowExceptionWithCause(exceptionClassName, m, causeClassName, causeMessage) =>
      emitValDef(sym, s"throw new $exceptionClassName(${quote(m)}, new $causeClassName(${quote(causeMessage)}))")
    case _ => super.emitNode(sym, rhs)
  }
}

trait ExceptionOpsGen extends Gen with ExceptionOpsExp {
  this: StagingCompile =>

  override def interpretDefWithEnv[A](d: Def[A])(using q: Quotes, env: Map[Sym[?], q.reflect.Symbol]): q.reflect.Term = {
    import q.reflect.*

    def interpretTryCatch[T](body: this.Block[T], catches: List[this.ReifiedCatch[T]], finalizer: Option[this.Block[Unit]]): Term = {
      val bodyTerm = interpretBlockWithEffectOrder(body)(using q, env)
      val valueType = body.res.tp.asTypeRepr
      valueType.asType match {
        case '[t] =>
          def withCatchMessageEnv(message: Option[this.Exp[String]], exceptionSymbol: Symbol)(build: Map[Sym[?], Symbol] => Term): Term =
            message match {
              case Some(messageSym: Sym[?]) =>
                val messageSymbol = Symbol.newVal(Symbol.spliceOwner, s"x${messageSym.id}", TypeRepr.of[String], Flags.EmptyFlags, Symbol.noSymbol)
                val messageVal = ValDef(messageSymbol, Some(Select.unique(Ref(exceptionSymbol), "getMessage")))
                q.reflect.Block(List(messageVal), build(env + (messageSym -> messageSymbol)))
              case None =>
                build(env)
              case Some(other) =>
                throw new Exception(s"catch message binding must be a fresh symbol, got: $other")
            }

          def buildCases(rest: List[this.ReifiedCatch[T]]): List[CaseDef] =
            rest.map { c =>
              val exceptionType = Symbol.requiredClass(c.exceptionClassName).typeRef
              val exceptionSymbol = Symbol.newBind(Symbol.spliceOwner, "e", Flags.EmptyFlags, exceptionType)
              val pattern = c.message match {
                case Some(_) => Bind(exceptionSymbol, Typed(Wildcard(), TypeTree.of(using exceptionType.asType)))
                case None => Typed(Wildcard(), TypeTree.of(using exceptionType.asType))
              }
              val guardTerm = c.guard.map(g =>
                withCatchMessageEnv(c.message, exceptionSymbol) { messageEnv =>
                  interpretBlockWithEffectOrder(g)(using q, messageEnv)
                }
              )
              val handlerTerm =
                withCatchMessageEnv(c.message, exceptionSymbol) { messageEnv =>
                  interpretBlockWithEffectOrder(c.handler)(using q, messageEnv)
                }.asExprOf[t]
              CaseDef(pattern, guardTerm, handlerTerm.asTerm)
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
      case Reflect(ThrowExceptionWithCause(exceptionClassName, m, causeClassName, causeMessage), _, _) =>
        val message = interpretExpWithEnv(m).asExprOf[String]
        val cause = interpretExpWithEnv(causeMessage).asExprOf[String]
        '{
          val causeThrowable = java.lang.Class.forName(${Expr(causeClassName)}).getConstructor(classOf[String]).newInstance($cause).asInstanceOf[Throwable]
          throw java.lang.Class.forName(${Expr(exceptionClassName)}).getConstructor(classOf[String], classOf[Throwable]).newInstance($message, causeThrowable).asInstanceOf[Throwable]
        }.asTerm
      case ThrowExceptionWithCause(exceptionClassName, m, causeClassName, causeMessage) =>
        val message = interpretExpWithEnv(m).asExprOf[String]
        val cause = interpretExpWithEnv(causeMessage).asExprOf[String]
        '{
          val causeThrowable = java.lang.Class.forName(${Expr(causeClassName)}).getConstructor(classOf[String]).newInstance($cause).asInstanceOf[Throwable]
          throw java.lang.Class.forName(${Expr(exceptionClassName)}).getConstructor(classOf[String], classOf[Throwable]).newInstance($message, causeThrowable).asInstanceOf[Throwable]
        }.asTerm
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
    case ThrowExceptionWithCause(_, m, _, causeMessage) =>
      stream.println("printf(" + quote(m) + ".c_str());")
      stream.println("printf(" + quote(causeMessage) + ".c_str());")
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
    case ThrowExceptionWithCause(_, m, _, causeMessage) =>
      stream.println("printf(" + quote(m) + ");")
      stream.println("printf(" + quote(causeMessage) + ");")
      stream.println("assert(false);")
    case _ => super.emitNode(sym, rhs)
  }
}
//OpenCL does not support printf within a kernel
//trait OpenCLGenExceptionOps extends OpenCLGenBase with CLikeGenExceptionOps
