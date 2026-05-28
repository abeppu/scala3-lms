package lms.gen

import lms.legacy.common.{BaseExp, EffectExp, VariablesExp}
import lms.legacy.internal.CodeMotion
import scala.quoted.*

trait StagingCompile extends QuotedGen with CodeMotion {
  this: EffectExp => 

  val IR: this.type = this

  private var compileDefs: List[Stm] = Nil
  private var currentRuntimeReturnTarget: Any = null

  protected def withRuntimeReturnTarget[A](target: Any)(body: => A): A = {
    val saved = currentRuntimeReturnTarget
    currentRuntimeReturnTarget = target
    try body
    finally currentRuntimeReturnTarget = saved
  }

  protected def runtimeReturnTarget(using q: Quotes): q.reflect.Symbol =
    currentRuntimeReturnTarget match {
      case target if target != null =>
        target.asInstanceOf[q.reflect.Symbol]
      case _ =>
        throw new Exception("staged return requires an active runtime return target")
    }

  protected def findCompileDefinition(sym: Sym[?]): Option[Stm] =
    compileDefs.find(infix_lhs(_) contains sym)

  protected def buildScheduleForResult(result: Any, scope: List[Stm], sort: Boolean = true): List[Stm] =
    getSchedule(scope)(result, sort)

  protected def buildExactScopeForResult(result: Exp[?], scope: List[Stm]): List[Stm] = {
    val deepScope = buildScheduleForResult(result, scope)
    getExactScope(deepScope)(List(result.asInstanceOf[Exp[Any]]))
  }

  protected def blockReify(block: Block[?]): Option[Reify[?]] = block.res match {
    case Def(reify: Reify[?]) =>
      Some(reify)
    case sym: Sym[?] =>
      findCompileDefinition(sym.asInstanceOf[Sym[Any]]) match {
        case Some(TP(_, reify: Reify[?])) => Some(reify)
        case Some(TP(_, Reflect(reify: Reify[?], _, _))) => Some(reify)
        case _ => None
      }
    case _ =>
      None
  }

  protected def statementForEffect(exp: Exp[Any], scope: List[Stm]): List[Stm] = exp match {
    case sym: Sym[?] =>
      scope.collectFirst { case stm @ TP(lhs, _) if lhs == sym => stm }.toList
    case other =>
      buildExactScopeForResult(other, scope)
  }

  protected def scheduleForBlock(block: Block[?], scope: List[Stm]): List[Stm] =
    blockReify(block) match {
      case Some(reify) =>
        val effectTargets = reify.effects.asInstanceOf[List[Exp[Any]]].distinct
        val effectScope = effectTargets.flatMap(statementForEffect(_, scope))
        val resultExp = reify.x.asInstanceOf[Exp[Any]]
        val resultScope =
          if effectTargets.contains(resultExp) then Nil
          else buildExactScopeForResult(resultExp, scope)
        val wanted = (effectScope ++ resultScope).toSet
        scope.filter(wanted)
      case None =>
        buildExactScopeForResult(block.res, scope)
    }

  override def interpretExpWithEnv[A](e: Exp[A])(using q: Quotes, env: Map[Sym[?], q.reflect.Symbol]): q.reflect.Term = {
    import q.reflect.*

    def resolveSym(sym: Sym[?]): Option[Term] =
      env.get(sym)
        .map(Ref(_))
        .orElse {
          findCompileDefinition(sym).map {
            case TP(_, rhs) =>
              rhs match {
                case reify: Reify[?] @unchecked =>
                  interpretExpWithEnv(reify.x.asInstanceOf[Exp[Any]])(using q, env)
                case Reflect(inner, _, _) =>
                  interpretDefWithEnv(inner.asInstanceOf[Def[Any]])(using q, env)
                case _ =>
                  interpretDefWithEnv(rhs.asInstanceOf[Def[Any]])(using q, env)
              }
          }
        }

    e match {
      case c @ Const(_) =>
        constantTerm(c)
      case sym @ Sym(_) =>
        resolveSym(sym)
          .getOrElse(throw new Exception(s"Symbol $sym not found in environment: $env"))
      case _ =>
        throw new Exception(s"Unsupported expression: $e")
    }
  }
  
  def compile[A:Typ, B:Typ](f: Exp[A] => Exp[B]): A => B = {
    given staging.Compiler = staging.Compiler.make(getClass.getClassLoader)

    staging.run((q: Quotes) ?=> {
      //println(s"starting run with qp = ${qp}")
      import q.reflect.*

      // 0. ensure type info is available
      val typA: Typ[A] = summon[Typ[A]]
      val typB: Typ[B] = summon[Typ[B]]
      val typeARepr = typA.asTypeRepr
      val typeBRepr = typB.asTypeRepr
      given typeA: Type[A] = typeARepr.asType.asInstanceOf[Type[A]]
      given typeB: Type[B] = typeBRepr.asType.asInstanceOf[Type[B]]

      // TODO for arity > 1, we need multiple input symbols
      // 1. Create a fresh symbol for the input
      val inputSym: Sym[A] = fresh[A](using typA)

      // 2. Reify the function body
      val savedContext = this.context
      this.context = Nil
      val (body: Exp[B], defs: List[Stm]) =
        try reifySubGraph {
          f(inputSym)
        }
        finally
          this.context = savedContext
      compileDefs = defs

      val schedule = compileDefs
      // 3. Roll a lambda term
      // TODO: handle multiple parameters
      val methodType = MethodType(List("a"))(
        _ => List(typeARepr),
        _ => typeBRepr
      )
      val lambdaTerm = Lambda(Symbol.spliceOwner, methodType, (owner, params) => {
        // Map inputSym to the parameter symbol in the environment
        val paramSym = params.head match {
          case v: ValDef => v.symbol
          case ident: Ident => ident.symbol // I
          case _ => throw new Exception(s"Could not find symbol for parameter, got: ${params.head}")
        }

        val envWithParam: Map[Sym[?], q.reflect.Symbol] = Map(inputSym -> paramSym)
        withRuntimeReturnTarget(owner) {
          interpretScheduleWithVars[B]((body, schedule))(using q, envWithParam).changeOwner(owner)
        }
        // You may need to update interpretSchedule to accept the environment
      })
      // 4. Convert the lambda term to an Expr[A => B]
      val stagedF: Expr[A => B] = lambdaTerm.asExprOf[A => B]
      stagedF
    })
  }

  protected def interpretBlockWithVars[A](block: Block[A])(using q: Quotes, env: Map[Sym[?], q.reflect.Symbol]): q.reflect.Term = {
    interpretBlockWithVarsImpl(block, includeAllStatements = false)
  }

  protected def interpretBlockWithEffectOrder[A](block: Block[A])(using q: Quotes, env: Map[Sym[?], q.reflect.Symbol]): q.reflect.Term = {
    interpretBlockWithVarsImpl(block, includeAllStatements = true)
  }

  private def interpretBlockWithVarsImpl[A](block: Block[A], includeAllStatements: Boolean)(using q: Quotes, env: Map[Sym[?], q.reflect.Symbol]): q.reflect.Term = {
    def reusesOuterSymbol(sym: Sym[?]): Boolean =
      findCompileDefinition(sym).exists {
        case TP(_, rhs) =>
          rhs match {
            case Reflect(inner, _, _) => reusesOuterDef(inner)
            case inner => reusesOuterDef(inner)
          }
      }

    def reusesOuterDef(defn: Def[?]): Boolean = defn match {
      case _: VariablesExp#NewVar[?] => true
      case _ => false
    }

    val schedule =
      blockReify(block) match {
        case Some(reify) =>
          val effectTargets = reify.effects.asInstanceOf[List[Exp[Any]]].distinct
          val effectScope = effectTargets.flatMap(exp => buildExactScopeForResult(exp, compileDefs))
          val resultScope = buildExactScopeForResult(reify.x.asInstanceOf[Exp[Any]], compileDefs)
          val wanted = (effectScope ++ resultScope).toSet
          compileDefs.filter(wanted)
        case None =>
          buildExactScopeForResult(block.res, compileDefs)
      }
    val filtered = schedule.filterNot(stm => infix_lhs(stm).exists(sym => env.contains(sym) && reusesOuterSymbol(sym)))
    interpretScheduleWithVars((block.res, filtered), includeAllStatements)
  }

  protected def interpretScheduleWithVars[A](graph: (Exp[A], List[Stm]), includeAllStatements: Boolean = false)(using q: Quotes, env0: Map[Sym[?], q.reflect.Symbol]): q.reflect.Term = {
    import q.reflect.*
    var env = env0
    var varSyms = Set.empty[Sym[?]]

    def varInit(defn: Def[?]): Option[Exp[?]] = defn match {
      case Reflect(inner, _, _) => varInit(inner)
      case nv: VariablesExp#NewVar[?] @unchecked => Some(nv.init.asInstanceOf[Exp[?]])
      case _ => None
    }

    def reifiedResult(defn: Def[?]): Option[Exp[?]] = defn match {
      case Reflect(inner, _, _) => reifiedResult(inner)
      case reify: Reify[?] @unchecked => Some(reify.x.asInstanceOf[Exp[?]])
      case _ => None
    }

    def unwrapDef(defn: Def[?]): Def[?] = defn match {
      case Reflect(inner, _, _) => unwrapDef(inner)
      case _ => defn
    }

    def isReadVar(defn: Def[?]): Boolean = defn match {
      case Reflect(_: VariablesExp#ReadVar[?], _, _) => true
      case _: VariablesExp#ReadVar[?] => true
      case _ => false
    }

    def assignmentValue(defn: Def[?]): Option[Exp[?]] = unwrapDef(defn) match {
      case assign: VariablesExp#Assign[?] @unchecked => Some(assign.rhs.asInstanceOf[Exp[?]])
      case plusEq: VariablesExp#VarPlusEquals[?] @unchecked => Some(plusEq.rhs.asInstanceOf[Exp[?]])
      case minusEq: VariablesExp#VarMinusEquals[?] @unchecked => Some(minusEq.rhs.asInstanceOf[Exp[?]])
      case timesEq: VariablesExp#VarTimesEquals[?] @unchecked => Some(timesEq.rhs.asInstanceOf[Exp[?]])
      case divEq: VariablesExp#VarDivideEquals[?] @unchecked => Some(divEq.rhs.asInstanceOf[Exp[?]])
      case _ => None
    }

    def resolveResult(exp: Exp[?]): Term = exp match {
      case c @ Const(_) => constantTerm(c)
      case s @ Sym(_) =>
        env.get(s)
          .map { symbol =>
            val ref = Ref(symbol)
            if (varSyms.contains(s)) Select.unique(ref, "elem") else ref
          }
          .getOrElse(interpretExpWithEnv(exp.asInstanceOf[Exp[Any]])(using q, env))
      case _ =>
        throw new Exception(s"unsupported block result $exp")
    }

    val nestedBlockSymbols: Set[Sym[Any]] =
      graph._2
        .flatMap { case TP(_, rhs) =>
          blocks(rhs).flatMap { block =>
            buildExactScopeForResult(block.res, compileDefs)
              .filter {
                case TP(_, blockRhs) => varInit(blockRhs).isEmpty
              }
              .flatMap(infix_lhs)
              .map(_.asInstanceOf[Sym[Any]])
          }
        }
        .toSet

    def isReifyNode(defn: Def[?]): Boolean = defn match {
      case _: Reify[?] => true
      case Reflect(_: Reify[?], _, _) => true
      case _ => false
    }

    def shouldMaterialize(defn: Def[?]): Boolean = defn match {
      case Reflect(_, summary, _) =>
        summary.control || summary.resAlloc || summary.mayGlobal || summary.mstGlobal || summary.mayWrite.nonEmpty || summary.mstWrite.nonEmpty
      case _ =>
        false
    }

    def isMaterializationRoot(sym: Sym[?], rhs: Def[?]): Boolean =
      varInit(rhs).nonEmpty ||
      (isReadVar(rhs) && !nestedBlockSymbols(sym.asInstanceOf[Sym[Any]])) ||
      isReifyNode(rhs) ||
      (shouldMaterialize(rhs) && !nestedBlockSymbols(sym.asInstanceOf[Sym[Any]]))

    val directDependencySymbols =
      graph._2.iterator
        .collect { case TP(sym, rhs) if isMaterializationRoot(sym, rhs) => syms(unwrapDef(rhs)) }
        .flatten
        .toSet

    val candidateStatements = graph._2.filter {
      case TP(sym, rhs) =>
        isMaterializationRoot(sym, rhs) ||
        (directDependencySymbols(sym.asInstanceOf[Sym[Any]]) && !nestedBlockSymbols(sym.asInstanceOf[Sym[Any]]))
    }

    val statements =
      if includeAllStatements then graph._2
      else candidateStatements

    def materializePureValue(exp: Exp[?]): List[ValDef] = exp match {
      case s @ Sym(_) if !env.contains(s) =>
        findCompileDefinition(s).toList.flatMap {
          case TP(_, symRhs) =>
            val inner = unwrapDef(symRhs)
            if varInit(symRhs).nonEmpty || reifiedResult(symRhs).nonEmpty || shouldMaterialize(symRhs) then
              Nil
            else
              val prereqs = syms(inner).flatMap(materializePureValue)
              val rhsTerm = interpretDefWithEnv(symRhs)(using q, env)
              val lhsSymbol = Symbol.newVal(Symbol.spliceOwner, s"x${s.id}", s.tp.asTypeRepr, Flags.EmptyFlags, Symbol.noSymbol)
              env += (s -> lhsSymbol)
              prereqs :+ ValDef(lhsSymbol, Some(rhsTerm))
        }
      case _ =>
        Nil
    }

    val valDefs = statements.flatMap { case TP(sym, rhs) =>
      varInit(rhs) match {
        case Some(initExp) =>
          val initTerm = interpretExpWithEnv(initExp)(using q, env)
          initTerm.tpe.asType match {
            case '[t] =>
              val rhsTerm = '{ scala.runtime.ObjectRef.create[t](${initTerm.asExprOf[t]}) }.asTerm
              val symbolName = s"x${sym.id}"
              val lhsSymbol = Symbol.newVal(Symbol.spliceOwner, symbolName, rhsTerm.tpe, Flags.EmptyFlags, Symbol.noSymbol)
              env += (sym -> lhsSymbol)
              varSyms += sym
              List(ValDef(lhsSymbol, Some(rhsTerm)))
          }
        case None =>
          reifiedResult(rhs) match {
            case Some(_) =>
              Nil
            case None =>
              val prereqs = assignmentValue(rhs).toList.flatMap(materializePureValue)
              val rhsTerm = interpretDefWithEnv(rhs)(using q, env)
              val symbolName = s"x${sym.id}"
              val lhsSymbol = Symbol.newVal(Symbol.spliceOwner, symbolName, sym.tp.asTypeRepr, Flags.EmptyFlags, Symbol.noSymbol)
              env += (sym -> lhsSymbol)
              prereqs :+ ValDef(lhsSymbol, Some(rhsTerm))
          }
      }
    }

    val resultTerm: Term = resolveResult(graph._1)

    q.reflect.Block(valDefs, resultTerm)
  }
}
