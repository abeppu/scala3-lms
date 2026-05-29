package lms.core

import lms.legacy.compat.SourceContext

import scala.Conversion
import scala.annotation.*
import scala.math.Ordering
import scala.quoted.*

/**
 * Local virtualization for Scala 3 LMS.
 *
 * The original Scala 2 LMS relied on a compiler plugin to reinterpret control flow and selected
 * operators in terms of the staged DSL. This annotation performs the same job locally by rewriting
 * the annotated definitions after typer, routing `if`/`while`, boolean ops, arithmetic, equality,
 * ordering, string indexing, and mutable locals through the LMS surface when staged values are
 * involved.
 *
 * The central discipline in this file is to classify every term as one of:
 * - `RepW(T)`: already staged as `Rep[T]`
 * - `VarW(T)`: staged mutable cell `Var[T]`
 * - `Bare(T)`: ordinary host Scala value
 *
 * Rewrites preserve plain host semantics when both sides stay `Bare`, and switch to LMS methods as
 * soon as any staged value participates.
 *
 * Control-flow coverage is still intentionally selective. `match` and `try/catch` are lowered only
 * for subsets that can be represented by the existing LMS IR.
 */
@experimental
class virt extends MacroAnnotation {

  def transform(using q: Quotes)(definition: q.reflect.Definition, companion: Option[q.reflect.Definition]): List[q.reflect.Definition] = {
    
    import q.reflect.*

    // Internal classification used by the rewriter to decide whether a tree should stay as ordinary
    // host Scala or be redirected through the LMS interface.
    sealed trait RepOrVar {
      def t: TypeRepr
    }
    case class RepW(t: TypeRepr) extends RepOrVar
    case class VarW(t: TypeRepr) extends RepOrVar
    case class Bare(t: TypeRepr) extends RepOrVar

    object RepLike {
      def unapply(rv: RepOrVar): Option[TypeRepr] =
        rv match {
          case RepW(_) => None
          case VarW(t) => Some(t)
          case Bare(t) => Some(t)
        }
    }
    
    def findMethods(owner: Symbol, name: String): List[Symbol] =
      if (owner.isNoSymbol) Nil
      else {
        owner.methodMember(name) ++ owner.companionClass.methodMember(name) match {
          case Nil => findMethods(owner.maybeOwner, name)
          case results => results
        }
      }
    
    def fetchEnclosingClass(s: Symbol): Symbol =
      if (s.isClassDef) s
      else if (s.isNoSymbol) Symbol.noSymbol
      else fetchEnclosingClass(s.maybeOwner)

    def repOrVar(t: TypeRepr): RepOrVar = {
      val normalized = t.dealias.widenTermRefByName.widen
      normalized match {
        case AppliedType(f, List(arg)) =>
          val sym = f.dealias.typeSymbol
          val symName = sym.name
          def isRepLike: Boolean =
            symName == "Exp" || symName == "Rep" || sym.fullName.endsWith(".Exp") || sym.fullName.endsWith(".Rep")
          def isVarLike: Boolean =
            symName == "Var" || symName == "Variable" || sym.fullName.endsWith(".Var") || sym.fullName.endsWith(".Variable")
          if (isRepLike) {
            RepW(arg)
          } else if (isVarLike) {
            VarW(arg)
          } else {
            Bare(arg)
          }
        case other => Bare(other)
      }
    }

    // Local definitions whose rewritten type no longer matches the original typed tree are rebound
    // to fresh symbols. Later references still mention the pre-rewrite symbol, so we redirect them
    // through this alias table during the recursive walk.
    val reboundAliases = collection.mutable.Map.empty[Symbol, Symbol]
    val reboundPatternNames = collection.mutable.Map.empty[String, Symbol]
    val activeTypedPatternNames = collection.mutable.Set.empty[String]
    var forceThrowVirtualizationDepth = 0
    var forceReturnVirtualizationDepth = 0

    // Mutable Scala locals become freshly rebound immutable vals of LMS type Var[T]. We additionally
    // track the element type of those Var symbols so reads and writes can be recognized quickly.
    val mutableVars = collection.mutable.Map.empty[Symbol, TypeRepr]

    def resolveReboundSymbol(sym: Symbol): Symbol =
      reboundAliases.getOrElse(sym, sym)

    def mutableTermSymbol(term: Term): Symbol = term match {
      case Inlined(_, _, body) => mutableTermSymbol(body)
      case Typed(expr, _) => mutableTermSymbol(expr)
      case sel: Select => resolveReboundSymbol(sel.symbol)
      case ident: Ident => resolveReboundSymbol(ident.symbol)
      case _ => Symbol.noSymbol
    }

    def directClassifyTerm(term: Term): RepOrVar =
      mutableVars.get(mutableTermSymbol(term)).map(VarW(_)).getOrElse(repOrVar(term.tpe))

    def hasStagedChild(term: Term): Boolean = {
      def staged(term: Term): Boolean = directClassifyTerm(term) match {
        case RepW(_) | VarW(_) => true
        case _ => hasStagedChild(term)
      }

      term match {
        case Inlined(_, bindings, body) =>
          bindings.exists {
            case vd: ValDef => vd.rhs.exists(staged)
            case dd: DefDef => dd.rhs.exists(staged)
            case td: TypeDef => false
            case cd: ClassDef => false
            case _ => false
          } || staged(body)
        case Block(stats, expr) =>
          stats.exists {
            case t: Term => staged(t)
            case vd: ValDef => vd.rhs.exists(staged)
            case dd: DefDef => dd.rhs.exists(staged)
            case _ => false
          } || staged(expr)
        case Apply(fun, args) =>
          staged(fun) || args.exists(staged)
        case TypeApply(fun, _) =>
          staged(fun)
        case Select(qualifier, _) =>
          staged(qualifier)
        case Typed(expr, _) =>
          staged(expr)
        case NamedArg(_, arg) =>
          staged(arg)
        case Assign(lhs, rhs) =>
          staged(lhs) || staged(rhs)
        case If(cond, thenp, elsep) =>
          staged(cond) || staged(thenp) || staged(elsep)
        case While(cond, body) =>
          staged(cond) || staged(body)
        case Repeated(elems, _) =>
          elems.exists(staged)
        case _ =>
          false
      }
    }

    // Classify a term using both its surface type and its contents. This is the key heuristic that
    // lets us recognize host-typed expressions such as `x + 1` that already contain staged pieces
    // and therefore need virtualization even though their outer tree is not yet `Rep[...]`.
    def classifyTerm(term: Term): RepOrVar = {
      directClassifyTerm(term) match {
        case bare @ Bare(_) =>
          term match {
            case ident: Ident =>
              ident.symbol.tree match {
                case v: ValDef =>
                  v.rhs match {
                    case Some(rhsTerm) =>
                      classifyTerm(rhsTerm)
                    case None =>
                      bare
                  }
                case _ => bare
              }
            case _ if hasStagedChild(term) =>
              RepW(term.tpe.dealias.widenTermRefByName.widen)
            case _ => bare
          }
        case other => other
      }
    }

    // Frequently reused pieces of context for one rewrite site.
    case class MacroCtx(thist: Term, srcGen: Term, owner: Symbol, unitf: Select)

    // Lift host booleans into Rep form so DSL boolean ops can consume them.
    def wrapBareBoolean(term: Term, ctx: MacroCtx): Term = classifyTerm(term) match {
      case VarW(elemType) =>
        readVarValue(ctx, term, elemType)
      case Bare(_) =>
        Select.overloaded(ctx.thist, "boolToBoolRep", Nil, List(term))
      case _ => term
    }

    // Generic lift helper for arithmetic/ordering operands.
    def wrapBareTerm(term: Term, ctx: MacroCtx): Term = classifyTerm(term) match {
      case VarW(elemType) =>
        readVarValue(ctx, term, elemType)
      case Bare(tpe) =>
        val tTree = TypeTree.of(using tpe.asType)
        val typWitness = findTypW(ctx.thist, tpe)
        Apply(Apply(TypeApply(ctx.unitf, List(tTree)), List(term)), List(typWitness))
      case _ => term
    }

    def readVarValue(ctx: MacroCtx, term: Term, elemType: TypeRepr): Term = {
      val coerced = ensureVarTerm(ctx, term, elemType)
      val readSym = findMethods(ctx.owner, "readVar").find(sym => !sym.flags.is(Flags.Given))
        .getOrElse(report.errorAndAbort("failed to virtualize: no readVar def with value parameter"))
      val reader = Select(ctx.thist, readSym)
      val typedReader = reader.appliedToTypes(List(elemType))
      val afterValue = typedReader.appliedToArgs(List(coerced))
      val typEvidence = findTypW(ctx.thist, elemType)
      applyImplicitArgs(afterValue, List(typEvidence, ctx.srcGen))
    }

    def normalizeRepTerm(term: Term, ctx: MacroCtx): (Term, RepOrVar) = {
      classifyTerm(term) match {
        case VarW(elemType) =>
          val read = readVarValue(ctx, term, elemType)
          (read, RepW(elemType))
        case other => (term, other)
      }
    }

    // Reuse LMS unit to wrap a literal Unit into Rep[Unit].
    def makeUnit(ctx: MacroCtx, value: Term): Term = {
      val unitTree = TypeTree.of[Unit]
      val unitTyp = Applied(TypeSelect(ctx.thist, "Typ"), List(unitTree))
      val unitWitness = Implicits.search(unitTyp.tpe) match {
        case success: ImplicitSearchSuccess => success.tree
        case _ =>
          // The DSL core exposes unitTyp as a member, which is still available
          // even when the concrete implementation is deferred in a trait.
          selectThisMember(ctx, "unitTyp", "missing Typ[Unit] evidence for virtualized unit")
      }
      Apply(Apply(TypeApply(ctx.unitf, List(unitTree)), List(value)), List(unitWitness))
    }

    // Flatten nested blocks so we can replace the trailing expression safely.
    def flattenBlockT(term: Statement): (List[Statement], Term) = term match {
      case Block(stats, expr) =>
        val flattened = stats.flatMap { stm =>
          val (nested, value) = flattenBlockT(stm)
          nested :+ value
        }
        val (tailStats, tailExpr) = flattenBlockT(expr)
        (flattened ++ tailStats, tailExpr)
      case t: Term => (Nil, t)
      case other => (List(other), Literal(UnitConstant()))
    }

    def flattenBlock(term: Term): Term = {
      val (stats, expr) = flattenBlockT(term)
      Block(stats, expr)
    }

    // Guarantee the last value in a block is a Rep by inserting unit conversions when needed.
    def ensureTrailingRep(term: Term, ctx: MacroCtx): Term = {
      val (stats, value) = flattenBlockT(term)
      classifyTerm(value) match {
        case RepW(_) => Block(stats, value)
        case VarW(elemType) =>
          val read = readVarValue(ctx, value, elemType)
          Block(stats, read)
        case RepLike(tpe) =>
          val tTree = TypeTree.of(using tpe.asType)
          val typWitness = findTypW(ctx.thist, tpe)
          val lifted = Apply(Apply(TypeApply(ctx.unitf, List(tTree)), List(value)), List(typWitness))
          Block(stats, lifted)
        case _ =>
          report.errorAndAbort(s"expected Rep or liftable term, found ${value.show}")
      }
    }

    def repResultType(term: Term): TypeRepr =
      repOrVar(term.tpe.widen).t

    def selectRepResultType(terms: List[Term]): TypeRepr = {
      val candidates = terms.map(repResultType)
      candidates.find(_ != TypeRepr.of[Nothing]).getOrElse(TypeRepr.of[Nothing])
    }

    // Normalize while bodies to Rep[Unit] so they match __whileDo's contract.
    def dropTrailingUnitInWhileBody(body: Term, ctx: MacroCtx): Term =
      flattenBlock(body) match {
        case Block(Nil, Literal(_)) =>
          makeUnit(ctx, Literal(UnitConstant()))
        case Block(stats, Literal(_)) =>
          Block(stats, makeUnit(ctx, Literal(UnitConstant())))
        case Block(stats, value) =>
          Block(stats :+ value, makeUnit(ctx, Literal(UnitConstant())))
        case other =>
          Block(List(other), makeUnit(ctx, Literal(UnitConstant())))
      }

    def findOverload(thist: Term, n: Int): Term = {
      val overloadType = TypeSelect(thist, s"Overloaded$n")
      Implicits.search(overloadType.tpe) match {
        case success: ImplicitSearchSuccess => success.tree
        case _ =>
          try Select.unique(thist, s"overloaded$n")
          catch
            case _: Throwable => report.errorAndAbort(s"missing overload evidence Overloaded$n")
      }
    }

    def isVarConversion(fun: Term): Boolean = fun match {
      case TypeApply(inner, _) => isVarConversion(inner)
      case Select(inner, "apply") => isVarConversion(inner)
      case Select(_, "__virtualizedBareVarConvInternal") => true
      case Select(_, "__virtualizedRepVarConvInternal") => true
      case _ => false
    }

    def stripVarConversion(term: Term): Term = term match {
      case Inlined(_, _, body) => stripVarConversion(body)
      case Typed(expr, _) => stripVarConversion(expr)
      case Block(Nil, expr) => stripVarConversion(expr)
      case Apply(fun, List(arg)) if isVarConversion(fun) => stripVarConversion(arg)
      case other => other
    }

    def makeVarType(ctx: MacroCtx, elemType: TypeRepr): TypeTree = {
      val elemTree = TypeTree.of(using elemType.asType)
      Applied(TypeSelect(ctx.thist, "Var"), List(elemTree))
    }

    def ensureVarTerm(ctx: MacroCtx, term: Term, elemType: TypeRepr): Term =
      Typed(term, makeVarType(ctx, elemType))

    // Materialize a Scala `var` as the correct overloaded LMS `__newVar` call, preserving whether
    // the initializer came from a host value, a Rep, or another Var.
    def newVarInit(ctx: MacroCtx, elemType: TypeRepr, init: Term, initKind: RepOrVar): Term = {
      val typEvidence = findTypW(ctx.thist, elemType)
      val arg = initKind match {
        case VarW(_) => ensureVarTerm(ctx, init, elemType)
        case _ => init
      }
      val baseCall = Select.overloaded(ctx.thist, "__newVar", List(elemType), List(arg))
      initKind match {
        case Bare(_) =>
          applyImplicitArgs(baseCall, List(typEvidence, ctx.srcGen))
        case RepW(_) =>
          val overload = findOverload(ctx.thist, 1)
          applyImplicitArgs(baseCall, List(overload, typEvidence, ctx.srcGen))
        case VarW(_) =>
          val overload = findOverload(ctx.thist, 2)
          applyImplicitArgs(baseCall, List(overload, typEvidence, ctx.srcGen))
      }
    }

    // Resolve `lhs = rhs` against the right LMS `__assign` overload. We search concrete methods
    // instead of relying on a single overloaded select because Scala 3 macro trees are brittle once
    // the lhs/rhs mix host, Rep, and Var values.
    def assignCall(ctx: MacroCtx, elemType: TypeRepr, lhs: Term, rhs: Term, rhsKind: RepOrVar): Term = {
      val typEvidence = findTypW(ctx.thist, elemType)
      val lhsVar = ensureVarTerm(ctx, lhs, elemType)
      val rhsArg = rhsKind match {
        case VarW(_) => ensureVarTerm(ctx, rhs, elemType)
        case _ => rhs
      }
      def firstImplicitParamType(sym: Symbol): Option[TypeRepr] = {
        def loop(tpe: TypeRepr): Option[TypeRepr] =
          tpe.dealias.widenTermRefByName match {
            case pt: PolyType =>
              loop(pt.resType)
            case mt: MethodType =>
              if mt.isImplicit then mt.paramTypes.headOption
              else loop(mt.resType)
            case _ =>
              None
          }
        loop(sym.termRef.widen)
      }
      def matchesOverload(sym: Symbol): Boolean = {
        val overloadIdx = firstImplicitParamType(sym).flatMap { tpe =>
          val name = tpe.typeSymbol.name
          if name == "Overloaded1" then Some(1)
          else if name == "Overloaded2" then Some(2)
          else None
        }
        rhsKind match {
          case Bare(_) => overloadIdx.isEmpty
          case RepW(_) => overloadIdx.contains(1)
          case VarW(_) => overloadIdx.contains(2)
        }
      }
      def resolveAssign(implicitArgs: List[Term], error: String): Term =
        findMethods(ctx.owner, "__assign")
          .iterator
          .filter(sym => !sym.flags.is(Flags.Given))
          .filter(matchesOverload)
          .flatMap { sym =>
            try {
              val base = Select(ctx.thist, sym).appliedToTypes(List(elemType)).appliedToArgs(List(lhsVar, rhsArg))
              Some(applyImplicitArgs(base, implicitArgs))
            } catch {
              case _: Throwable => None
            }
          }
          .nextOption()
          .getOrElse(report.errorAndAbort(error))
      rhsKind match {
        case Bare(_) =>
          resolveAssign(List(typEvidence, ctx.srcGen), "failed to resolve bare __assign overload")
        case RepW(_) =>
          val overload = findOverload(ctx.thist, 1)
          resolveAssign(List(overload, typEvidence, ctx.srcGen), "failed to resolve Rep __assign overload")
        case VarW(_) =>
          val overload = findOverload(ctx.thist, 2)
          resolveAssign(List(overload, typEvidence, ctx.srcGen), "failed to resolve Var __assign overload")
      }
    }
    
    def makeThis(owner: Symbol): Term = This(fetchEnclosingClass(owner))

    // Recover Typ[T] evidence for synthesized trees. Raw implicit search is not always enough for
    // members inherited from the LMS stack, so we fall back to the conventional field names used by
    // the DSL (`intTyp`, `boolTyp`, ...).
    def findTypW(thist: Term, trep: TypeRepr): Term = {
      val t = TypeTree.of(using trep.asType)
      val ttyp = Applied(TypeSelect(thist, "Typ"), List(t))
      val typW = Implicits.search(ttyp.tpe) match {
        // Failure should be impossible, else we wouldn't have been
        // able to form the type Rep[T]
        case success: ImplicitSearchSuccess => success.tree
        case failure: ImplicitSearchFailure =>
          val sym = trep.typeSymbol
          val baseCandidates: List[String] =
            if sym.isNoSymbol then Nil
            else
              val fullname = sym.fullName
              val computed =
                if sym.name.forall(_.isLower) then sym.name + "Typ"
                else if sym.name.nonEmpty then s"${sym.name.head.toLower}${sym.name.tail}Typ"
                else "Typ"
              val special = fullname match {
                case "scala.Boolean" => List("boolTyp")
                case _ => Nil
              }
              special :+ computed
          baseCandidates.collectFirst { cname =>
            try Some(Select.unique(thist, cname))
            catch case _: Throwable => None
          }.flatten.getOrElse {
            report.errorAndAbort(s"failed to synthesize Typ for ${trep.show}: ${failure.explanation}")
          }
      }
      typW
    }

    def findOrderingW(trep: TypeRepr): Term = {
      val orderingTpe = TypeRepr.of[Ordering].appliedTo(trep)
      Implicits.search(orderingTpe) match {
        case success: ImplicitSearchSuccess => success.tree
        case failure: ImplicitSearchFailure =>
          report.errorAndAbort(s"failed to find Ordering evidence for ${trep.show}")
      }
    }

    def applyImplicitArgs(term: Term, args: List[Term]): Term = {
      @annotation.tailrec
      def loop(current: Term, remaining: List[Term]): Term =
        if remaining.isEmpty then current
        else
          current.tpe.widenTermRefByName.dealias match {
            case mt: MethodType if mt.isImplicit =>
              val paramCount = mt.paramTypes.size
              if remaining.length < paramCount then
                report.errorAndAbort(s"insufficient implicit arguments for ${current.show}")
              val (now, rest) = remaining.splitAt(paramCount)
              loop(Apply(current, now), rest)
            case _ =>
              report.errorAndAbort(s"no implicit parameter list available on ${current.show}")
          }
      loop(term, args)
    }

    def applySearchedImplicitArgs(term: Term): Option[Term] = {
      def loop(current: Term): Option[Term] =
        current.tpe.widenTermRefByName.dealias match {
          case mt: MethodType if mt.isImplicit =>
            val searched = mt.paramTypes.map { paramType =>
              Implicits.search(paramType) match {
                case success: ImplicitSearchSuccess => Some(success.tree)
                case _ => None
              }
            }
            if searched.forall(_.nonEmpty) then loop(Apply(current, searched.flatten))
            else None
          case _ =>
            Some(current)
        }
      loop(term)
    }

    def invokeOverloadedWithSearch(ctx: MacroCtx, name: String, args: List[Term]): Option[Term] =
      findMethods(ctx.owner, name)
        .iterator
        .filter(sym => !sym.flags.is(Flags.Given))
        .flatMap { sym =>
          try {
            applySearchedImplicitArgs(Select(ctx.thist, sym).appliedToArgs(args))
          } catch {
            case _: Throwable => None
          }
        }
        .nextOption()


    def selectThisMember(ctx: MacroCtx, name: String, error: => String): Term =
      try Select.unique(ctx.thist, name)
      catch
        case _: Throwable => report.errorAndAbort(error)

    def withForcedThrowVirtualization(body: => Term): Term = {
      forceThrowVirtualizationDepth += 1
      try body
      finally forceThrowVirtualizationDepth -= 1
    }

    def shouldForceThrowVirtualization: Boolean =
      forceThrowVirtualizationDepth > 0

    def withForcedReturnVirtualization(body: => Term): Term = {
      forceReturnVirtualizationDepth += 1
      try body
      finally forceReturnVirtualizationDepth -= 1
    }

    def shouldForceReturnVirtualization: Boolean =
      forceReturnVirtualizationDepth > 0

    def withForcedEscapeVirtualization(body: => Term): Term =
      withForcedThrowVirtualization {
        withForcedReturnVirtualization(body)
      }

    // Main tree transformer. The strategy is:
    // 1. rewrite local mutable definitions into LMS Vars
    // 2. normalize terms so Vars become readable Reps when needed
    // 3. preserve host operations when everything is Bare
    // 4. otherwise reroute through the staged LMS combinators
    object Virtualizer extends TreeMap {

      private def isVirtualizedBoolConv(fun: Term): Boolean = fun match {
        case Select(Select(_, "__virtualizedBoolConvInternal"), "apply") => true
        case _ => false
      }

      private def stripBoolConv(term: Term): Term = term match {
        case Apply(fun, List(arg)) if isVirtualizedBoolConv(fun) => stripBoolConv(arg)
        case other => other
      }

      private def sameElementRepLift(term: Apply, arg: Term): Boolean = {
        repOrVar(term.tpe) match {
          case RepW(resultElem) =>
            classifyTerm(arg) match {
              case RepW(argElem) => argElem =:= resultElem
              case VarW(argElem) => argElem =:= resultElem
              case _ => false
            }
          case _ =>
            false
          }
      }

      private def isRepLiftConversion(fun: Term): Boolean = fun match {
        case Select(conv, "apply") =>
          conv.tpe.baseClasses.exists(_.fullName == "scala.Conversion")
        case _ =>
          false
      }

      private def makeCtx(owner: Symbol): MacroCtx = {
        val thist = makeThis(owner)
        val srcGen = '{ SourceContext.generate }.asTerm
        val unitf: Select = findMethods(owner, "unit") match {
          case Nil =>
            report.errorAndAbort("LMS-internal error: no [unit] found for self")
          case x :: _ => thist.select(x)
        }
        MacroCtx(thist, srcGen, owner, unitf)
      }

      private def isReadVarBase(fun: Term): Boolean = fun match {
        case TypeApply(inner, _) => isReadVarBase(inner)
        case Select(_, "readVar") => true
        case _ => false
      }

      private def isUnitLift(fun: Term): Boolean = fun match {
        case TypeApply(inner, _) => isUnitLift(inner)
        case Select(_, "unit") => true
        case _ => false
      }

      private def rebuildBinary(applyTerm: Apply, sel: Select, lhs: Term, rhs: Term): Term =
        Apply.copy(applyTerm)(Select.copy(sel)(lhs, sel.name), List(rhs))

      private def stripNumericOpsReceiver(term: Term): Term = term match {
        case Apply(Select(conv, "apply"), List(arg)) if isLmsNumericConversion(conv) =>
          stripNumericOpsReceiver(arg)
        case _ => term
      }

      private def isLmsNumericConversion(term: Term): Boolean = term match {
        case Apply(TypeApply(sel: Select, _), _) =>
          val name = sel.symbol.name
          name == "numericToNumericOps" || name == "repNumericToNumericOps" || name == "varNumericToNumericOps"
        case Apply(sel: Select, _) =>
          val name = sel.symbol.name
          name == "numericToNumericOps" || name == "repNumericToNumericOps" || name == "varNumericToNumericOps"
        case _ => false
      }

      private def mentionsTypedPattern(term: Term, owner: Symbol): Boolean = {
        var found = false
        object Finder extends TreeTraverser {
          override def traverseTree(tree: Tree)(owner: Symbol): Unit = {
            tree match {
              case id: Ident if activeTypedPatternNames.contains(id.name) =>
                found = true
              case _ if !found =>
                super.traverseTree(tree)(owner)
              case _ =>
            }
          }
        }
        Finder.traverseTree(term)(owner)
        found
      }

      private def rewriteResidualArithmetic(ctx: MacroCtx, term: Term): Term = term match {
        case inlined @ Inlined(call, bindings, body) =>
          Inlined.copy(inlined)(call, bindings, rewriteResidualArithmetic(ctx, body))
        case typed @ Typed(expr, tpt) =>
          Typed.copy(typed)(rewriteResidualArithmetic(ctx, expr), tpt)
        case block @ Block(stats, expr) =>
          Block.copy(block)(stats, rewriteResidualArithmetic(ctx, expr))
        case applyTerm @ Apply(inner @ Apply(sel @ Select(lhsOuter, op @ ("+" | "-" | "*" | "/")), List(rhs)), implicitArgs)
            if implicitArgs.nonEmpty =>
          rewriteArithmeticBinaryWithImplicit(ctx, applyTerm, inner, sel, lhsOuter, rhs, op, implicitArgs)
        case applyTerm @ Apply(inner @ Apply(TypeApply(sel @ Select(lhsOuter, op @ ("+" | "-" | "*" | "/")), _), List(rhs)), implicitArgs)
            if implicitArgs.nonEmpty =>
          rewriteArithmeticBinaryWithImplicit(ctx, applyTerm, inner, sel, lhsOuter, rhs, op, implicitArgs)
        case applyTerm @ Apply(tapply @ TypeApply(sel @ Select(lhs, op @ ("+" | "-" | "*" | "/")), _), List(rhs)) =>
          rewriteArithmeticBinaryTypeApply(ctx, applyTerm, tapply, sel, lhs, rhs, op)
        case applyTerm @ Apply(sel @ Select(lhs, op @ ("+" | "-" | "*" | "/")), List(rhs)) =>
          rewriteArithmeticBinary(ctx, applyTerm, sel, lhs, rhs, op)
        case other =>
          other
      }

      private def rewriteArithmeticBinary(ctx: MacroCtx, applyTerm: Apply, sel: Select, lhsTree: Term, rhsTree: Term, op: String): Term = {
        val lhs = transformTerm(lhsTree)(ctx.owner)
        val rhs = transformTerm(rhsTree)(ctx.owner)
        val lhsRawKind = classifyTerm(lhs)
        val rhsRawKind = classifyTerm(rhs)
        val (lhsNorm, lhsKind) = normalizeRepTerm(lhs, ctx)
        val (rhsNorm, rhsKind) = normalizeRepTerm(rhs, ctx)
        val intVarInvolved =
          lhsRawKind match {
            case VarW(elemType) if elemType =:= TypeRepr.of[Int] => true
            case _ =>
              rhsRawKind match {
                case VarW(elemType) if elemType =:= TypeRepr.of[Int] => true
                case _ => false
              }
          }
        val typedPatternArithmetic =
          activeTypedPatternNames.nonEmpty &&
            (mentionsTypedPattern(lhsTree, ctx.owner) || mentionsTypedPattern(rhsTree, ctx.owner)) &&
            isIntKind(lhsKind) && isIntKind(rhsKind) &&
            !(lhsKind.isInstanceOf[Bare] && rhsKind.isInstanceOf[Bare])
        val stagedIntArithmetic =
          isIntKind(lhsKind) && isIntKind(rhsKind) &&
            !(lhsKind.isInstanceOf[Bare] && rhsKind.isInstanceOf[Bare])
        if stagedIntArithmetic || intVarInvolved || typedPatternArithmetic then
          val method = op match {
            case "+" => "int_plus"
            case "-" => "int_minus"
            case "*" => "int_times"
            case "/" => "int_divide"
          }
          val call = Select.overloaded(ctx.thist, method, Nil, List(wrapBareTerm(lhsNorm, ctx), wrapBareTerm(rhsNorm, ctx)))
          Apply(call, List(ctx.srcGen))
        else
          rebuildBinary(applyTerm, sel, lhsNorm, rhsNorm)
      }

      private def rewriteArithmeticBinaryWithImplicit(ctx: MacroCtx, outer: Apply, inner: Apply, sel: Select, lhsTree: Term, rhsTree: Term, op: String, implicitArgs: List[Term]): Term = {
        val lhs = transformTerm(lhsTree)(ctx.owner)
        val rhs = transformTerm(rhsTree)(ctx.owner)
        val lhsDirect = rewriteResidualArithmetic(ctx, transformTerm(stripNumericOpsReceiver(lhsTree))(ctx.owner))
        val rhsDirect = rewriteResidualArithmetic(ctx, transformTerm(stripNumericOpsReceiver(rhsTree))(ctx.owner))
        val (lhsNorm, lhsKind) = normalizeRepTerm(lhsDirect, ctx)
        val (rhsNorm, rhsKind) = normalizeRepTerm(rhsDirect, ctx)
        val stagedIntArithmetic =
          isIntKind(lhsKind) && isIntKind(rhsKind) &&
            !(lhsKind.isInstanceOf[Bare] && rhsKind.isInstanceOf[Bare])
        if stagedIntArithmetic then
          val method = op match {
            case "+" => "int_plus"
            case "-" => "int_minus"
            case "*" => "int_times"
            case "/" => "int_divide"
          }
          val call = Select.overloaded(ctx.thist, method, Nil, List(wrapBareTerm(lhsNorm, ctx), wrapBareTerm(rhsNorm, ctx)))
          Apply(call, List(ctx.srcGen))
        else {
          val rebuiltInner = Apply.copy(inner)(Select.copy(sel)(lhs, sel.name), List(rhs))
          Apply.copy(outer)(rebuiltInner, implicitArgs.map(transformTerm(_)(ctx.owner)))
        }
      }

      private def rewriteArithmeticBinaryTypeApply(ctx: MacroCtx, applyTerm: Apply, tapply: TypeApply, sel: Select, lhsTree: Term, rhsTree: Term, op: String): Term = {
        val lhs = rewriteResidualArithmetic(ctx, transformTerm(stripNumericOpsReceiver(lhsTree))(ctx.owner))
        val rhs = rewriteResidualArithmetic(ctx, transformTerm(stripNumericOpsReceiver(rhsTree))(ctx.owner))
        val (lhsNorm, lhsKind) = normalizeRepTerm(lhs, ctx)
        val (rhsNorm, rhsKind) = normalizeRepTerm(rhs, ctx)
        val stagedIntArithmetic =
          isIntKind(lhsKind) && isIntKind(rhsKind) &&
            !(lhsKind.isInstanceOf[Bare] && rhsKind.isInstanceOf[Bare])
        if stagedIntArithmetic then
          val method = op match {
            case "+" => "int_plus"
            case "-" => "int_minus"
            case "*" => "int_times"
            case "/" => "int_divide"
          }
          val call = Select.overloaded(ctx.thist, method, Nil, List(wrapBareTerm(lhsNorm, ctx), wrapBareTerm(rhsNorm, ctx)))
          Apply(call, List(ctx.srcGen))
        else {
          val rebuiltTypeApply = TypeApply.copy(tapply)(Select.copy(sel)(lhsNorm, sel.name), tapply.args)
          Apply.copy(applyTerm)(rebuiltTypeApply, List(rhsNorm))
        }
      }

      private def isIntKind(kind: RepOrVar): Boolean = kind match {
        case RepW(t) => t =:= TypeRepr.of[Int]
        case VarW(t) => t =:= TypeRepr.of[Int]
        case Bare(t) => t =:= TypeRepr.of[Int]
      }

      private def rewriteIntBinary(ctx: MacroCtx, applyTerm: Apply, sel: Select, lhsTree: Term, rhsTree: Term, method: String): Term = {
        val lhs = transformTerm(lhsTree)(ctx.owner)
        val rhs = transformTerm(rhsTree)(ctx.owner)
        val (lhsNorm, lhsKind) = normalizeRepTerm(lhs, ctx)
        val (rhsNorm, rhsKind) = normalizeRepTerm(rhs, ctx)
        if lhsKind.isInstanceOf[Bare] && rhsKind.isInstanceOf[Bare] then
          rebuildBinary(applyTerm, sel, lhsNorm, rhsNorm)
        else if isIntKind(lhsKind) && isIntKind(rhsKind) then
          val call = Select.overloaded(ctx.thist, method, Nil, List(wrapBareTerm(lhsNorm, ctx), wrapBareTerm(rhsNorm, ctx)))
          Apply(call, List(ctx.srcGen))
        else
          rebuildBinary(applyTerm, sel, lhsNorm, rhsNorm)
      }

      private def rewriteIntUnary(ctx: MacroCtx, sel: Select, expr: Term, method: String): Term = {
        val raw = transformTerm(expr)(ctx.owner)
        val (value, kind) = normalizeRepTerm(raw, ctx)
        kind match {
          case Bare(t) if t =:= TypeRepr.of[Int] =>
            Select.copy(sel)(value, sel.name)
          case _ if isIntKind(kind) =>
            val call = Select.overloaded(ctx.thist, method, Nil, List(wrapBareTerm(value, ctx)))
            Apply(call, List(ctx.srcGen))
          case _ =>
            Select.copy(sel)(value, sel.name)
        }
      }

      private def adaptApplyArgs(ctx: MacroCtx, fun: Term, args: List[Term]): List[Term] =
        fun.tpe.widenTermRefByName.dealias match {
          case mt: MethodType =>
            args.zip(mt.paramTypes).map { (arg, paramType) =>
              classifyTerm(arg) match {
                case VarW(elemType) =>
                  repOrVar(paramType) match {
                    case RepW(_) => readVarValue(ctx, arg, elemType)
                    case _ => arg
                  }
                case _ => arg
              }
            }
          case _ =>
            args
        }

      // Virtualize branch by routing condition and bodies through __ifThenElse.
      private def rewriteIf(ctx: MacroCtx, ifTerm: If): Term = {
        val guardTree = stripBoolConv(ifTerm.cond)
        val guard = transformTerm(guardTree)(ctx.owner)
        val (normalizedGuard, guardKind) = normalizeRepTerm(guard, ctx)
        guardKind match {
          case Bare(_) =>
            val thenp = transformTerm(ifTerm.thenp)(ctx.owner)
            val elsep = transformTerm(ifTerm.elsep)(ctx.owner)
            If.copy(ifTerm)(normalizedGuard, thenp, elsep)
          case _ =>
            val thenp = withForcedEscapeVirtualization {
              ensureTrailingRep(transformTerm(ifTerm.thenp)(ctx.owner), ctx)
            }
            val elsep = withForcedEscapeVirtualization {
              ensureTrailingRep(transformTerm(ifTerm.elsep)(ctx.owner), ctx)
            }
            val valueType = selectRepResultType(List(thenp, elsep))
            val typW = findTypW(ctx.thist, valueType)
            Apply(
              Select.overloaded(ctx.thist, "__ifThenElse", List(valueType), List(normalizedGuard, thenp, elsep)),
              List(typW, ctx.srcGen))
        }
      }

      private sealed trait SupportedThrownException
      private case class ThrownMessageOnly(exceptionClassName: String, message: Option[Term]) extends SupportedThrownException
      private case class ThrownWithCause(exceptionClassName: String, message: Option[Term], causeClassName: String, causeMessage: Option[Term]) extends SupportedThrownException

      private def supportedThrownException(term: Term): Option[SupportedThrownException] = {
        def normalize(tree: Term): Term = tree match {
          case Inlined(_, _, inner) => normalize(inner)
          case Typed(expr, _) => normalize(expr)
          case Block(Nil, expr) => normalize(expr)
          case other => other
        }

        def throwableClassName(tpt: TypeTree): String = {
          val normalized = tpt.tpe.dealias.widenTermRefByName.widen
          normalized.classSymbol.getOrElse(normalized.typeSymbol).fullName
        }

        normalize(term) match {
          case Apply(Select(New(tpt), ctor), List(msg)) if ctor == "<init>" && tpt.tpe <:< TypeRepr.of[Throwable] =>
            normalize(msg) match {
              case Apply(Select(New(causeTpt), causeCtorName), causeArgs)
                  if causeCtorName == "<init>" && causeTpt.tpe <:< TypeRepr.of[Throwable] =>
                causeArgs match {
                  case List(causeMsg) =>
                    Some(ThrownWithCause(throwableClassName(tpt), None, throwableClassName(causeTpt), Some(causeMsg)))
                  case Nil =>
                    Some(ThrownWithCause(throwableClassName(tpt), None, throwableClassName(causeTpt), None))
                  case _ =>
                    None
                }
              case _ =>
                Some(ThrownMessageOnly(throwableClassName(tpt), Some(msg)))
            }
          case Apply(Select(New(tpt), ctor), List(msg, causeCtor @ Apply(Select(New(causeTpt), causeCtorName), causeArgs)))
              if ctor == "<init>" && tpt.tpe <:< TypeRepr.of[Throwable] &&
                 causeCtorName == "<init>" && causeTpt.tpe <:< TypeRepr.of[Throwable] =>
            val causeClassName = throwableClassName(causeTpt)
            causeArgs match {
              case List(causeMsg) =>
                Some(ThrownWithCause(throwableClassName(tpt), Some(msg), causeClassName, Some(causeMsg)))
              case Nil =>
                Some(ThrownWithCause(throwableClassName(tpt), Some(msg), causeClassName, None))
              case _ =>
                None
            }
          case Apply(Select(New(tpt), ctor), Nil) if ctor == "<init>" && tpt.tpe <:< TypeRepr.of[Throwable] =>
            Some(ThrownMessageOnly(throwableClassName(tpt), None))
          case _ =>
            None
        }
      }

      private def rewriteThrow(ctx: MacroCtx, throwApply: Apply, throwExpr: Term, owner: Symbol): Term = {
        supportedThrownException(throwExpr) match {
          case Some(ThrownMessageOnly(exceptionClassName, msgTree)) =>
            val msg = msgTree.map(transformTerm(_)(owner)).getOrElse(Literal(StringConstant("")))
            val msgKind = classifyTerm(msg)
            if shouldForceThrowVirtualization || !msgKind.isInstanceOf[Bare] then
              invokeOverloadedWithSearch(ctx, "throw_exception_class", List(Literal(StringConstant(exceptionClassName)), wrapBareTerm(msg, ctx)))
                .getOrElse(report.errorAndAbort("failed to virtualize throw_exception_class"))
            else
              Apply.copy(throwApply)(throwApply.fun, List(transformTerm(throwExpr)(owner)))
          case Some(ThrownWithCause(exceptionClassName, msgTree, causeClassName, causeMsgTree)) =>
            val msg = msgTree.map(transformTerm(_)(owner)).getOrElse(Literal(StringConstant("")))
            val causeMsg = causeMsgTree.map(transformTerm(_)(owner)).getOrElse(Literal(StringConstant("")))
            val shouldVirtualizeThisThrow =
              shouldForceThrowVirtualization || !classifyTerm(msg).isInstanceOf[Bare] || !classifyTerm(causeMsg).isInstanceOf[Bare]
            if shouldVirtualizeThisThrow then
              invokeOverloadedWithSearch(
                ctx,
                "throw_exception_class_with_cause",
                List(
                  Literal(StringConstant(exceptionClassName)),
                  wrapBareTerm(msg, ctx),
                  Literal(StringConstant(causeClassName)),
                  wrapBareTerm(causeMsg, ctx)
                )).getOrElse(report.errorAndAbort("failed to virtualize throw_exception_class_with_cause"))
            else
              Apply.copy(throwApply)(throwApply.fun, List(transformTerm(throwExpr)(owner)))
          case None =>
            if shouldForceThrowVirtualization then
              report.errorAndAbort("virtualized throw currently supports Throwable subclasses with zero args, single-String constructors, and (String, Throwable) where the cause is a constructor-form Throwable")
            else
              Apply.copy(throwApply)(throwApply.fun, List(transformTerm(throwExpr)(owner)))
        }
      }

      private def rewriteReturn(ctx: MacroCtx, ret: Return): Term = {
        val value = transformTerm(ret.expr)(ctx.owner)
        val (normalized, kind) = normalizeRepTerm(value, ctx)
        if shouldForceReturnVirtualization || !kind.isInstanceOf[Bare] then {
          val elemType = kind match {
            case RepW(t) => t
            case Bare(t) => t
            case VarW(_) => report.errorAndAbort("unexpected Var in normalized return expression")
          }
          val method = findMethods(ctx.owner, "returnL").find(sym => !sym.flags.is(Flags.Given))
            .getOrElse(report.errorAndAbort("failed to virtualize: no returnL in scope"))
          val typedMethod = Select(ctx.thist, method).appliedToTypes(List(elemType))
          val arg = kind match {
            case Bare(_) => wrapBareTerm(normalized, ctx)
            case _ => normalized
          }
          val typW = findTypW(ctx.thist, elemType)
          applyImplicitArgs(typedMethod.appliedToArgs(List(arg)), List(typW, ctx.srcGen))
        } else {
          Return.copy(ret)(normalized, ret.from)
        }
      }

      // Handle && / || by deferring to LMS boolean combinators only when reps are involved.
      private def rewriteBooleanBinary(ctx: MacroCtx, applyTerm: Apply, sel: Select, lhsTree: Term, rhsTree: Term, method: String): Term = {
        val lhsRaw = transformTerm(stripBoolConv(lhsTree))(ctx.owner)
        val rhsRaw = transformTerm(stripBoolConv(rhsTree))(ctx.owner)
        val (lhs, lhsKind) = normalizeRepTerm(lhsRaw, ctx)
        val (rhs, rhsKind) = normalizeRepTerm(rhsRaw, ctx)
        (lhsKind, rhsKind) match {
          case (Bare(_), Bare(_)) =>
            rebuildBinary(applyTerm, sel, lhs, rhs)
          case _ =>
            val call = Select.overloaded(ctx.thist, method, Nil, List(wrapBareBoolean(lhs, ctx), wrapBareBoolean(rhs, ctx)))
            Apply(call, List(ctx.srcGen))
        }
      }

      // Short helper for the unary ! select shape.
      private def rewriteBooleanNegateSelect(ctx: MacroCtx, sel: Select, expr: Term): Term = {
        val raw = transformTerm(expr)(ctx.owner)
        val (value, kind) = normalizeRepTerm(raw, ctx)
        kind match {
          case Bare(_) =>
            Select.copy(sel)(value, sel.name)
          case _ =>
            val call = Select.overloaded(ctx.thist, "boolean_negate", Nil, List(wrapBareBoolean(value, ctx)))
            Apply(call, List(ctx.srcGen))
        }
      }

      private def emitBooleanBinary(ctx: MacroCtx, lhs: Term, rhs: Term, method: String): Term = {
        val call = Select.overloaded(ctx.thist, method, Nil, List(wrapBareBoolean(lhs, ctx), wrapBareBoolean(rhs, ctx)))
        Apply(call, List(ctx.srcGen))
      }

      private def emitRepIsInstanceOf(ctx: MacroCtx, scrutinee: Term, scrutineeType: TypeRepr, targetType: TypeRepr): Term = {
        val srcTyp = findTypW(ctx.thist, scrutineeType)
        val dstTyp = findTypW(ctx.thist, targetType)
        val method = findMethods(ctx.owner, "rep_isinstanceof").find(sym => !sym.flags.is(Flags.Given))
          .getOrElse(report.errorAndAbort("failed to virtualize: no rep_isinstanceof in scope"))
        val typedMethod = Select(ctx.thist, method).appliedToTypes(List(scrutineeType, targetType))
        Apply(typedMethod.appliedToArgs(List(scrutinee, srcTyp, dstTyp)), List(ctx.srcGen))
      }

      private def emitRepAsInstanceOf(ctx: MacroCtx, scrutinee: Term, scrutineeType: TypeRepr, targetType: TypeRepr): Term = {
        val srcTyp = findTypW(ctx.thist, scrutineeType)
        val dstTyp = findTypW(ctx.thist, targetType)
        val method = findMethods(ctx.owner, "rep_asinstanceof").find(sym => !sym.flags.is(Flags.Given))
          .getOrElse(report.errorAndAbort("failed to virtualize: no rep_asinstanceof in scope"))
        val typedMethod = Select(ctx.thist, method).appliedToTypes(List(scrutineeType, targetType))
        applyImplicitArgs(typedMethod.appliedToArgs(List(scrutinee, srcTyp, dstTyp)), List(dstTyp, ctx.srcGen))
      }

      private def emitHostTypeTest(term: Term, targetType: TypeRepr, method: String): Term = {
        TypeApply(Select.unique(term, method), List(TypeTree.of(using targetType.asType)))
      }

      private def emitEquality(ctx: MacroCtx, lhs: Term, rhs: Term): Term = {
        val combo = (classifyTerm(lhs), classifyTerm(rhs))
        val overload = combo match {
          case (RepW(l), RepW(r)) => Some((l, r, 1))
          case (RepW(l), VarW(r)) => Some((l, r, 2))
          case (VarW(l), RepW(r)) => Some((l, r, 3))
          case (RepW(l), Bare(r)) => Some((l, r, 4))
          case (Bare(l), RepW(r)) => Some((l, r, 5))
          case (VarW(l), Bare(r)) => Some((l, r, 6))
          case (Bare(l), VarW(r)) => Some((l, r, 7))
          case (VarW(l), VarW(r)) => Some((l, r, 8))
          case (Bare(_), Bare(_)) => None
        }
        overload match {
          case None =>
            Apply(Select.unique(lhs, "=="), List(rhs))
          case Some((lty, rty, idx)) =>
            val overloadEv = findOverload(ctx.thist, idx)
            val lTyp = findTypW(ctx.thist, lty)
            val rTyp = findTypW(ctx.thist, rty)
            Apply(
              Select.overloaded(ctx.thist, "__equal", List(lty, rty), List(lhs, rhs)),
              List(overloadEv, lTyp, rTyp, ctx.srcGen))
        }
      }

      private def emitVirtualIf(ctx: MacroCtx, guard: Term, thenp: Term, elsep: Term): Term = {
        val normalizedGuard = wrapBareBoolean(guard, ctx)
        val thenRep = ensureTrailingRep(thenp, ctx)
        val elseRep = ensureTrailingRep(elsep, ctx)
        val valueType = selectRepResultType(List(thenRep, elseRep))
        val typW = findTypW(ctx.thist, valueType)
        Apply(
          Select.overloaded(ctx.thist, "__ifThenElse", List(valueType), List(normalizedGuard, thenRep, elseRep)),
          List(typW, ctx.srcGen))
      }

      // Mirror Scala equality onto LMS __equal overloads, falling back when both sides are plain.
      private def rewriteEquality(ctx: MacroCtx, applyTerm: Apply, sel: Select, lhsTree: Term, rhsTree: Term, negate: Boolean): Term = {
        val lhs = transformTerm(lhsTree)(ctx.owner)
        val rhs = transformTerm(rhsTree)(ctx.owner)
        val combo = (classifyTerm(lhs), classifyTerm(rhs))
        val overload = combo match {
          case (RepW(l), RepW(r)) => Some((l, r, 1))
          case (RepW(l), VarW(r)) => Some((l, r, 2))
          case (VarW(l), RepW(r)) => Some((l, r, 3))
          case (RepW(l), Bare(r)) => Some((l, r, 4))
          case (Bare(l), RepW(r)) => Some((l, r, 5))
          case (VarW(l), Bare(r)) => Some((l, r, 6))
          case (Bare(l), VarW(r)) => Some((l, r, 7))
          case (VarW(l), VarW(r)) => Some((l, r, 8))
          case (Bare(_), Bare(_)) => None
        }
        overload match {
          case None =>
            rebuildBinary(applyTerm, sel, lhs, rhs)
          case Some((lty, rty, idx)) =>
            val overloadEv = findOverload(ctx.thist, idx)
            val lTyp = findTypW(ctx.thist, lty)
            val rTyp = findTypW(ctx.thist, rty)
            val equalTerm = Apply(
              Select.overloaded(ctx.thist, "__equal", List(lty, rty), List(lhs, rhs)),
              List(overloadEv, lTyp, rTyp, ctx.srcGen))
            if (negate) {
              val negateCall = Select.overloaded(ctx.thist, "boolean_negate", Nil, List(equalTerm))
              Apply(negateCall, List(ctx.srcGen))
            } else equalTerm
        }
      }

      private def stripPattern(pattern: Tree): Tree = pattern match {
        case Inlined(_, _, inner) => stripPattern(inner)
        case _ => pattern
      }

      private def isWildcardPattern(pattern: Tree): Boolean = stripPattern(pattern) match {
        case Ident(name) if name == "_" => true
        case Bind(name, inner) if name == "_" => isWildcardPattern(inner)
        case _ => false
      }

      private def patternCondition(ctx: MacroCtx, scrutinee: Term, pattern: Tree): Option[Term] = {
        def normalizeBoolLike(cond: Term): Term =
          classifyTerm(cond) match {
            case RepW(_) =>
              cond
            case VarW(elemType) =>
              readVarValue(ctx, cond, elemType)
            case Bare(_) =>
              wrapBareBoolean(cond, ctx)
          }

        def extractorCondAndValue(fun: Term): (Term, Term) = {
          val unapplyResult = Apply(fun, List(scrutinee))
          val cond = normalizeBoolLike(Select.unique(unapplyResult, "isDefined"))
          val value = Select.unique(unapplyResult, "get")
          (cond, value)
        }

        def tupleProjection(tupleTerm: Term, idx: Int): Term =
          Select.unique(tupleTerm, s"_$idx")

        def typeTest(targetType: TypeRepr): Option[Term] =
          classifyTerm(scrutinee) match {
            case RepW(scrutineeType) =>
              if scrutineeType <:< targetType then None
              else Some(emitRepIsInstanceOf(ctx, scrutinee, scrutineeType, targetType))
            case Bare(scrutineeType) =>
              if scrutineeType <:< targetType then None
              else Some(emitHostTypeTest(scrutinee, targetType, "isInstanceOf"))
            case VarW(_) =>
              report.errorAndAbort("virtualized match scrutinee should have been normalized before typed-pattern handling")
          }

        def loop(tree: Tree): Option[Term] = stripPattern(tree) match {
          case pattern if isWildcardPattern(pattern) =>
            None
          case TypedOrTest(inner, _: Inferred) =>
            loop(inner)
          case TypedOrTest(inner, tpt) =>
            val typedCond = typeTest(tpt.tpe)
            val innerCond = loop(inner)
            (typedCond, innerCond) match {
              case (None, None) => None
              case (Some(cond), None) => Some(cond)
              case (None, Some(cond)) => Some(cond)
              case (Some(lhs), Some(rhs)) => Some(emitBooleanBinary(ctx, lhs, rhs, "boolean_and"))
            }
          case Typed(inner, tpt) =>
            val typedCond = typeTest(tpt.tpe)
            val innerCond = loop(inner)
            (typedCond, innerCond) match {
              case (None, None) => None
              case (Some(cond), None) => Some(cond)
              case (None, Some(cond)) => Some(cond)
              case (Some(lhs), Some(rhs)) => Some(emitBooleanBinary(ctx, lhs, rhs, "boolean_and"))
            }
          case lit: Literal =>
            Some(emitEquality(ctx, scrutinee, lit.asExpr.asTerm))
          case Alternatives(patterns) =>
            patterns.flatMap(loop).reduceOption((lhs, rhs) => emitBooleanBinary(ctx, lhs, rhs, "boolean_or"))
          case ref: Term if ref.symbol.flags.is(Flags.StableRealizable) || ref.symbol.flags.is(Flags.Module) =>
            Some(emitEquality(ctx, scrutinee, transformTerm(ref)(ctx.owner)))
          case Bind(_, inner) =>
            loop(inner)
          case Unapply(fun, _, patterns) =>
            // Scala 3 encodes extractor patterns as `Unapply(fun, ..., patterns)`.
            // Supported forms:
            // - boolean nullary extractor: `case Extractor()`
            // - value extractors: `case Extractor(x)` and fixed-arity tuples from `unapply(...).get`
            val unapplyFun = transformTerm(fun)(ctx.owner)
            if patterns.isEmpty then
              val cond = Apply(unapplyFun, List(scrutinee))
              Some(normalizeBoolLike(cond))
            else
              val (definedCond, extracted) = extractorCondAndValue(unapplyFun)
              val nestedConds = patterns.zipWithIndex.flatMap { case (p, idx) =>
                val target =
                  if patterns.length == 1 then extracted
                  else tupleProjection(extracted, idx + 1)
                patternCondition(ctx, target, p)
              }
              nestedConds.foldLeft(Option(definedCond)) { (acc, next) =>
                acc.map(lhs => emitBooleanBinary(ctx, lhs, next, "boolean_and"))
              }
          case Apply(extractor, args) =>
            // Support boolean extractor patterns such as `case Even() => ...` on staged
            // scrutinees by lowering to `Even.unapply(scrutinee)`.
            // For now we only handle nullary extractor patterns, which correspond to
            // `unapply: T => Boolean` matches.
            if args.nonEmpty then
              report.errorAndAbort(s"unsupported virtualized extractor arity in pattern: ${tree.show}")
            val extractorTerm = transformTerm(extractor)(ctx.owner)
            val cond = Select.overloaded(extractorTerm, "unapply", Nil, List(scrutinee))
            classifyTerm(cond) match {
              case RepW(_) =>
                Some(cond)
              case VarW(elemType) =>
                Some(readVarValue(ctx, cond, elemType))
              case Bare(_) =>
                Some(wrapBareBoolean(cond, ctx))
            }
          case _ =>
            report.errorAndAbort(s"unsupported virtualized match pattern: ${tree.show}")
        }
        loop(pattern)
      }

      private def patternBindings(ctx: MacroCtx, scrutinee: Term, pattern: Tree): List[(Symbol, Term)] = {
        def tupleProjection(tupleTerm: Term, idx: Int): Term =
          Select.unique(tupleTerm, s"_$idx")

        def castedScrutinee(targetType: TypeRepr): Term =
          classifyTerm(scrutinee) match {
            case RepW(scrutineeType) =>
              if scrutineeType <:< targetType then scrutinee
              else emitRepAsInstanceOf(ctx, scrutinee, scrutineeType, targetType)
            case Bare(scrutineeType) =>
              if scrutineeType <:< targetType then scrutinee
              else emitHostTypeTest(scrutinee, targetType, "asInstanceOf")
            case VarW(_) =>
              report.errorAndAbort("virtualized match scrutinee should have been normalized before typed-pattern binding")
          }

        def boundValue(tree: Tree): Term = stripPattern(tree) match {
          case Bind(_, inner) =>
            boundValue(inner)
          case TypedOrTest(_, _: Inferred) =>
            scrutinee
          case TypedOrTest(_, tpt) =>
            castedScrutinee(tpt.tpe)
          case Typed(_, tpt) =>
            castedScrutinee(tpt.tpe)
          case _ =>
            scrutinee
        }

        def loop(tree: Tree): List[(Symbol, Term)] = stripPattern(tree) match {
          case Bind(name, inner) if name != "_" =>
            (tree.symbol, boundValue(tree)) :: loop(inner)
          case TypedOrTest(inner, _: Inferred) =>
            loop(inner)
          case TypedOrTest(id: Ident, tpt) if id.name != "_" =>
            (id.symbol, castedScrutinee(tpt.tpe)) :: Nil
          case TypedOrTest(bind @ Bind(name, inner), tpt) if name != "_" =>
            (bind.symbol, castedScrutinee(tpt.tpe)) :: loop(inner)
          case Typed(id: Ident, tpt) if id.name != "_" =>
            (id.symbol, castedScrutinee(tpt.tpe)) :: Nil
          case Typed(bind @ Bind(name, inner), tpt) if name != "_" =>
            (bind.symbol, castedScrutinee(tpt.tpe)) :: loop(inner)
          case Typed(inner, _) =>
            loop(inner)
          case Unapply(fun, _, patterns) if patterns.nonEmpty =>
            val unapplyFun = transformTerm(fun)(ctx.owner)
            val extracted = Select.unique(Apply(unapplyFun, List(scrutinee)), "get")
            patterns.zipWithIndex.flatMap { case (pat, idx) =>
              val target =
                if patterns.length == 1 then extracted
                else tupleProjection(extracted, idx + 1)
              patternBindings(ctx, target, pat)
            }
          case _ =>
            Nil
        }
        loop(pattern)
      }

      private def patternTypedBindingNames(pattern: Tree): Set[String] = {
        def loop(tree: Tree): Set[String] = stripPattern(tree) match {
          case TypedOrTest(id: Ident, _) if id.name != "_" =>
            Set(id.name)
          case TypedOrTest(Bind(name, inner), _) if name != "_" =>
            Set(name) ++ loop(inner)
          case Typed(id: Ident, _) if id.name != "_" =>
            Set(id.name)
          case Typed(Bind(name, inner), _) if name != "_" =>
            Set(name) ++ loop(inner)
          case Bind(name, inner: Typed) if name != "_" =>
            Set(name) ++ loop(inner)
          case Bind(name, inner) if name != "_" =>
            val nested = loop(inner)
            if nested.nonEmpty then Set(name) ++ nested else nested
          case _ =>
            Set.empty
        }
        loop(pattern)
      }

      private def withPatternBindings(bindings: List[(Symbol, Term)], typedNames: Set[String], owner: Symbol)(body: => Term): Term = {
        if bindings.isEmpty then body
        else {
          val savedAliases = bindings.map { case (sym, _) => sym -> reboundAliases.get(sym) }
          val savedNames = bindings.map { case (sym, _) => sym.name -> reboundPatternNames.get(sym.name) }
          val savedTypedNames = activeTypedPatternNames.toSet
          val defs = bindings.map { case (sym, value) =>
            val reboundSym = Symbol.newVal(owner, sym.name, value.tpe.widenTermRefByName, Flags.EmptyFlags, Symbol.noSymbol)
            reboundAliases.update(sym, reboundSym)
            reboundPatternNames.update(sym.name, reboundSym)
            ValDef(reboundSym, Some(value))
          }
          activeTypedPatternNames.clear()
          activeTypedPatternNames ++= savedTypedNames
          activeTypedPatternNames ++= typedNames
          val transformed =
            try body
            finally
              savedAliases.foreach {
                case (sym, Some(prev)) => reboundAliases.update(sym, prev)
                case (sym, None) => reboundAliases.remove(sym)
              }
              savedNames.foreach {
                case (name, Some(prev)) => reboundPatternNames.update(name, prev)
                case (name, None) => reboundPatternNames.remove(name)
              }
              activeTypedPatternNames.clear()
              activeTypedPatternNames ++= savedTypedNames
          Block(defs, transformed)
        }
      }

      private def caseCondition(ctx: MacroCtx, scrutinee: Term, cdef: CaseDef): Option[Term] = {
        val bindings = patternBindings(ctx, scrutinee, cdef.pattern)
        val typedNames = patternTypedBindingNames(cdef.pattern)
        val patCond = patternCondition(ctx, scrutinee, cdef.pattern)
        val guardCond = cdef.guard.map { guard =>
          withPatternBindings(bindings, typedNames, ctx.owner) {
            transformTerm(guard)(ctx.owner)
          }
        }
        (patCond, guardCond) match {
          case (None, None) => None
          case (Some(p), None) => Some(p)
          case (None, Some(g)) => Some(g)
          case (Some(p), Some(g)) => Some(emitBooleanBinary(ctx, p, g, "boolean_and"))
        }
      }

      // Lower a supported staged `match` into a chain of staged conditionals.
      // Supported staged patterns currently include literals, stable ids, alternatives, wildcard
      // fallback, simple binders/aliases, and typed patterns.
      private def rewriteMatch(ctx: MacroCtx, matchTerm: Match): Term = {
        val scrutineeRaw = transformTerm(matchTerm.scrutinee)(ctx.owner)
        val (scrutinee, scrutineeKind) = directClassifyTerm(scrutineeRaw) match {
          case VarW(elemType) =>
            val read = readVarValue(ctx, scrutineeRaw, elemType)
            (read, RepW(elemType))
          case other =>
            (scrutineeRaw, other)
        }
        scrutineeKind match {
          case Bare(_) =>
            super.transformTerm(matchTerm)(ctx.owner)
          case _ =>
            def build(cases: List[CaseDef]): Term = cases match {
              case Nil =>
                report.errorAndAbort("virtualized match requires a wildcard/default case")
              case cdef :: rest =>
                val bindings = patternBindings(ctx, scrutinee, cdef.pattern)
                val typedNames = patternTypedBindingNames(cdef.pattern)
                val rhs = withPatternBindings(bindings, typedNames, ctx.owner) {
                  withForcedEscapeVirtualization {
                    transformTerm(cdef.rhs)(ctx.owner)
                  }
                }
                caseCondition(ctx, scrutinee, cdef) match {
                  case None => rhs
                  case Some(cond) =>
                    emitVirtualIf(ctx, cond, rhs, build(rest))
                }
            }
            build(matchTerm.cases)
        }
      }

      private def isEmptyFinally(term: Option[Term]): Boolean = term match {
        case None => true
        case Some(Literal(UnitConstant())) => true
        case Some(Inlined(_, _, inner)) => isEmptyFinally(Some(inner))
        case Some(Block(Nil, inner)) => isEmptyFinally(Some(inner))
        case _ => false
      }

      private def tryCatchCaseExceptionName(pattern: Tree): Option[String] = {
        def loop(tree: Tree): Option[String] = stripPattern(tree) match {
          case pattern if isWildcardPattern(pattern) =>
            Some("java.lang.Throwable")
          case Typed(inner, tpt) if tpt.tpe <:< TypeRepr.of[Throwable] =>
            val normalized = tpt.tpe.dealias.widenTermRefByName.widen
            Some(normalized.classSymbol.getOrElse(normalized.typeSymbol).fullName)
          case Bind(name, inner) if name == "_" =>
            loop(inner)
          case Bind(_, inner) =>
            loop(inner)
          case _ =>
            None
        }
        loop(pattern)
      }

      private def tryCatchPatternBinders(pattern: Tree): List[Symbol] = {
        def loop(tree: Tree): List[Symbol] = stripPattern(tree) match {
          case Bind(name, inner) if name != "_" =>
            tree.symbol :: loop(inner)
          case Typed(Bind(name, inner), _) if name != "_" =>
            tree.symbol :: loop(inner)
          case _ =>
            Nil
        }
        loop(pattern)
      }

      private def mentionsSymbols(term: Tree, targets: Set[Symbol]): Boolean = {
        var found = false
        val traverser = new TreeTraverser {
          override def traverseTree(tree: Tree)(owner: Symbol): Unit =
            if !found then
              tree match {
                case ident: Ident if targets.contains(ident.symbol) =>
                  found = true
                case _ =>
                  super.traverseTree(tree)(owner)
              }
        }
        traverser.traverseTree(term)(Symbol.spliceOwner)
        found
      }

      private def transformCaseDefHost(cdef: CaseDef, owner: Symbol): CaseDef =
        CaseDef.copy(cdef)(cdef.pattern, cdef.guard.map(transformTermRec(_, owner, expectVar = false)), transformTermRec(cdef.rhs, owner, expectVar = false))

      private def rewriteTryCatch(ctx: MacroCtx, tryTerm: Try): Term = {
        val body = transformTerm(tryTerm.body)(ctx.owner)
        val transformedCases = tryTerm.cases.map(transformCaseDefHost(_, ctx.owner))

        def caseHandler(cdef: CaseDef): Option[(String, Option[Term], Term)] = {
          val binders = tryCatchPatternBinders(cdef.pattern).toSet
          if binders.nonEmpty && (cdef.guard.exists(mentionsSymbols(_, binders)) || mentionsSymbols(cdef.rhs, binders)) then
            report.errorAndAbort("virtualized try/catch does not yet support using catch binder values inside guards or handlers")
          tryCatchCaseExceptionName(cdef.pattern).map { exceptionClassName =>
            val guard = cdef.guard.map { g =>
              withForcedEscapeVirtualization {
                ensureTrailingRep(transformTerm(g)(ctx.owner), ctx)
              }
            }
            val handler = withForcedEscapeVirtualization {
              ensureTrailingRep(transformTerm(cdef.rhs)(ctx.owner), ctx)
            }
            (exceptionClassName, guard, handler)
          }
        }

        val handlers = tryTerm.cases.map(caseHandler)
        val hasStagedBody = !classifyTerm(body).isInstanceOf[Bare]
        val hasStagedHandler = handlers.flatten.exists { case (_, _, rhs) => !classifyTerm(rhs).isInstanceOf[Bare] }
        val hasStagedGuard = handlers.flatten.exists { case (_, guard, _) => guard.exists(g => !classifyTerm(g).isInstanceOf[Bare]) }
        val finalizerTerm = tryTerm.finalizer.map(transformTermRec(_, ctx.owner, expectVar = false))
        val hasStagedFinalizer = finalizerTerm.exists(t => !classifyTerm(t).isInstanceOf[Bare])
        val shouldVirtualize = hasStagedBody || hasStagedHandler || hasStagedGuard || hasStagedFinalizer

        if !shouldVirtualize then
          Try.copy(tryTerm)(body, transformedCases, finalizerTerm)
        else if handlers.contains(None) then
          report.errorAndAbort("virtualized try/catch currently supports only wildcard or Throwable-typed catch cases")
        else {
          val bodyRep = withForcedEscapeVirtualization {
            ensureTrailingRep(transformTerm(tryTerm.body)(ctx.owner), ctx)
          }
          val valueType = selectRepResultType(bodyRep :: handlers.flatten.map(_._3))
          val materializedCatchTerms = handlers.flatten.map { case (exceptionClassName, guard, handler) =>
            guard match {
              case Some(cond) =>
                Select.overloaded(ctx.thist, "__guardedCatchCase", List(valueType), List(Literal(StringConstant(exceptionClassName)), cond, handler))
              case None =>
                Select.overloaded(ctx.thist, "__catchCase", List(valueType), List(Literal(StringConstant(exceptionClassName)), handler))
            }
          }
          val finalizerRep = finalizerTerm.filterNot(t => isEmptyFinally(Some(t))).map { fin =>
            withForcedEscapeVirtualization {
              dropTrailingUnitInWhileBody(fin, ctx)
            }
          }
          finalizerRep match {
            case Some(fin) =>
              val call = applyImplicitArgs(
                Apply(
                  Select.overloaded(ctx.thist, "__tryCatchFinally", List(valueType), bodyRep :: materializedCatchTerms),
                  List(fin)),
                List(findTypW(ctx.thist, valueType), ctx.srcGen))
              call
            case None =>
              Apply(
                Select.overloaded(ctx.thist, "__tryCatch", List(valueType), bodyRep :: materializedCatchTerms),
                List(findTypW(ctx.thist, valueType), ctx.srcGen))
          }
        }
      }

      private def stripOrderingOpsReceiver(term: Term): Term = term match {
        case Apply(Select(conv, "apply"), List(arg)) if isLmsOrderingConversion(conv) =>
          stripOrderingOpsReceiver(arg)
        case _ => term
      }

      private def isLmsOrderingConversion(term: Term): Boolean = term match {
        case Apply(TypeApply(sel: Select, _), _) =>
          val name = sel.symbol.name
          name == "orderingToOrderingOps" || name == "repOrderingToOrderingOps" || name == "varOrderingToOrderingOps"
        case Apply(sel: Select, _) =>
          val name = sel.symbol.name
          name == "orderingToOrderingOps" || name == "repOrderingToOrderingOps" || name == "varOrderingToOrderingOps"
        case _ => false
      }

      // Forward ordering comparisons to the DSL once reps participate.
      private def rewriteOrdering(ctx: MacroCtx, applyTerm: Apply, sel: Select, lhsTree: Term, rhsTree: Term, method: String): Term = {
        val strippedLhs = stripOrderingOpsReceiver(lhsTree)
        val strippedRhs = stripOrderingOpsReceiver(rhsTree)
        val lhs = transformTerm(strippedLhs)(ctx.owner)
        val rhs = transformTerm(strippedRhs)(ctx.owner)
        val lhsKind = classifyTerm(lhs)
        val rhsKind = classifyTerm(rhs)
        if lhsKind.isInstanceOf[Bare] && rhsKind.isInstanceOf[Bare] then
          return Apply(Select.copy(sel)(lhs, sel.name), List(rhs))
        val elemType = lhsKind match {
          case RepW(t) => t
          case VarW(t) => t
          case Bare(t) =>
            rhsKind match {
              case RepW(t2) => t2
              case VarW(t2) => t2
              case Bare(_) => t
            }
        }
        val args = List(wrapBareTerm(lhs, ctx), wrapBareTerm(rhs, ctx))
        val methodSym = findMethods(ctx.owner, method) match {
          case sym :: _ => sym
          case Nil => report.errorAndAbort(s"failed to virtualize: no $method in scope")
        }
        val methodRef = Select(ctx.thist, methodSym)
        val typedMethod = methodRef.appliedToTypes(List(elemType))
        val applied = typedMethod.appliedToArgs(args)
        val orderingEvidence = findOrderingW(elemType)
        val typEvidence = findTypW(ctx.thist, elemType)
        val ret = applied.appliedToArgs(List(orderingEvidence, typEvidence, ctx.srcGen))
        ret
      }

      private def isStringOpsApply(symbol: Symbol): Boolean = {
        val owner = symbol.owner
        symbol.name == "apply" && owner.fullName == "lms.legacy.common.StringOps"
      }

      private def liftIndexToRepInt(value: Term, ctx: MacroCtx): Term = {
        val intType = TypeRepr.of[Int]
        val (normalized, kind) = normalizeRepTerm(value, ctx)
        kind match {
          case RepW(t) if t =:= intType =>
            normalized
          case Bare(t) if t =:= intType =>
            wrapBareTerm(value, ctx)
          case _ =>
            report.errorAndAbort(s"string index must be an Int or Rep[Int], found ${value.tpe.show}")
        }
      }

      private def rewriteRepStringApply(ctx: MacroCtx, receiverTree: Term, idxTree: Term, owner: Symbol): Term = {
        val receiverValue = transformTerm(receiverTree)(owner)
        val (receiverRep, receiverKind) = normalizeRepTerm(receiverValue, ctx)
        receiverKind match {
          case RepW(strType) if strType =:= TypeRepr.of[String] =>
            val idxValue = transformTerm(idxTree)(owner)
            val idxRep = liftIndexToRepInt(idxValue, ctx)
            val call = Select.overloaded(ctx.thist, "string_charAt", Nil, List(receiverRep, idxRep))
            Apply(call, List(ctx.srcGen))
          case _ =>
            report.errorAndAbort(s"expected Rep[String] receiver for virtualized string apply, found ${receiverValue.tpe.show}")
        }
      }

      // Replace while loops with __whileDo(cond, body).
      private def rewriteWhile(ctx: MacroCtx, guard: Term, body: Term): Term = {
        val normalizedBody = withForcedEscapeVirtualization {
          dropTrailingUnitInWhileBody(body, ctx)
        }
        val method = findMethods(ctx.owner, "__whileDo") match {
          case _ :: symb :: _ => symb
          case symb :: Nil => symb
          case Nil => report.errorAndAbort("failed to virtualize: no __whileDo in scope")
        }
        val whileCall = Apply(ctx.thist.select(method), List(guard, normalizedBody))
        Apply(whileCall, List(ctx.srcGen))
      }
      
      // Rewrite local `var` definitions before we recurse into later terms, so subsequent reads and
      // assignments can recognize the symbol as a staged mutable cell.
      private def transformLocalDefinition(defn: Definition, owner: Symbol): Definition = defn match {
        case dd: DefDef =>
          val newRhs = dd.rhs.map(transformTerm(_)(dd.symbol))
          DefDef.copy(dd)(name = dd.name, paramss = dd.paramss, tpt = dd.tpt, rhs = newRhs)
        case vd: ValDef if vd.rhs.nonEmpty =>
          val isMutable = vd.symbol.flags.is(Flags.Mutable)
          val rhsTree = transformTerm(vd.rhs.get)(vd.symbol)
          if isMutable then
            val ctx = makeCtx(owner)
            val strippedRhs = stripVarConversion(rhsTree)
            val rhsKind = classifyTerm(strippedRhs)
            val elemType = {
              val declaredKind = repOrVar(vd.symbol.termRef.widen)
              declaredKind match {
                case Bare(t) => t
                case RepW(t) => t
                case VarW(t) => t
              }
            }
            val varType = makeVarType(ctx, elemType)
            val init = newVarInit(ctx, elemType, strippedRhs, rhsKind)
            val reboundSym = Symbol.newVal(owner, vd.name, varType.tpe, Flags.EmptyFlags, Symbol.noSymbol)
            reboundAliases.update(vd.symbol, reboundSym)
            mutableVars.update(reboundSym, elemType)
            ValDef(reboundSym, Some(init))
          else
            classifyTerm(rhsTree) match {
              case Bare(_) =>
                ValDef.copy(vd)(name = vd.name, tpt = vd.tpt, rhs = Some(rhsTree))
              case _ =>
                val reboundSym = Symbol.newVal(owner, vd.name, rhsTree.tpe.widenTermRefByName, Flags.EmptyFlags, Symbol.noSymbol)
                reboundAliases.update(vd.symbol, reboundSym)
                ValDef(reboundSym, Some(rhsTree))
            }
        case other => other
      }

      override def transformTerm(term: Term)(owner: Symbol): Term =
        transformTermRec(term, owner, expectVar = false)

      // Recursive tree walk. `expectVar` is carried through a few call sites where we want to keep a
      // Var-valued expression as a Var instead of eagerly rewriting it to `readVar(...)`.
      private def transformTermRec(term: Term, owner: Symbol, expectVar: Boolean): Term = {
        val ctx = makeCtx(owner)
        val rewritten = term match {
          case inlined @ Inlined(call, bindings, body) =>
            val newBindings = bindings.map(b => transformLocalDefinition(b, owner))
            Inlined.copy(inlined)(call, newBindings, transformTermRec(body, owner, expectVar))
          case block @ Block(stats, expr) =>
            val newStats = stats.map(transformStatement(_)(owner))
            Block.copy(block)(newStats, transformTermRec(expr, owner, expectVar))
          case ident: Ident if reboundAliases.contains(ident.symbol) =>
            Ref(reboundAliases(ident.symbol))
          case ident: Ident if activeTypedPatternNames.contains(ident.name) && reboundPatternNames.contains(ident.name) =>
            Ref(reboundPatternNames(ident.name))
          case ident: Ident if mutableVars.contains(ident.symbol) =>
            ident
          case sel @ Select(receiver, _) if reboundAliases.contains(sel.symbol) =>
            Ref(reboundAliases(sel.symbol))
          case sel @ Select(receiver, _) if mutableVars.contains(sel.symbol) =>
            val newReceiver = transformTermRec(receiver, owner, expectVar = false)
            val updatedSel =
              if newReceiver eq receiver then sel
              else Select.copy(sel)(newReceiver, sel.name)
            updatedSel
          case assign: Assign =>
            transformStatement(assign)(owner) match {
              case term: Term => term
              case other =>
                report.errorAndAbort(s"expected assignment rewrite to yield Term, found ${other}")
            }
          case Apply(sel @ Select(th, "boolToBoolRep"), List(arg)) =>
            val source = arg match {
              case ident: Ident =>
                ident.symbol.tree match {
                  case v: ValDef if v.rhs.nonEmpty =>
                    v.rhs
                  case _ => Some(arg)
                }
              case other => Some(other)
            }
            val value = source match {
              case Some(rhs) => transformTermRec(rhs, owner, expectVar = false)
              case None => transformTermRec(arg, owner, expectVar = false)
            }
            classifyTerm(value) match {
              case Bare(_) =>
                val receiver = transformTermRec(th, owner, expectVar = false)
                Apply.copy(term)(Select.copy(sel)(receiver, sel.name), List(value))
              case _ =>
                value
            }
          case applyTerm @ Apply(inner @ Apply(base, List(arg)), implicitArgs) if isReadVarBase(base) =>
            val newBase = transformTermRec(base, owner, expectVar = false)
            val newArg = stripVarConversion(transformTermRec(arg, owner, expectVar = true))
            val newInner = Apply.copy(inner)(newBase, List(newArg))
            Apply.copy(applyTerm)(newInner, implicitArgs.map(transformTermRec(_, owner, expectVar = false)))
          case applyTerm @ Apply(base, List(arg)) if isReadVarBase(base) =>
            val newBase = transformTermRec(base, owner, expectVar = false)
            val newArg = stripVarConversion(transformTermRec(arg, owner, expectVar = true))
            Apply.copy(applyTerm)(newBase, List(newArg))
          case applyTerm @ Apply(inner @ Apply(base, List(arg)), implicitArgs) if isUnitLift(base) =>
            val value = transformTermRec(arg, owner, expectVar = false)
            if sameElementRepLift(applyTerm, value) then value
            else {
              val newBase = transformTermRec(base, owner, expectVar = false)
              val newInner = Apply.copy(inner)(newBase, List(value))
              Apply.copy(applyTerm)(newInner, implicitArgs.map(transformTermRec(_, owner, expectVar = false)))
            }
          case Apply(fun, List(arg)) if isVarConversion(fun) =>
            val value = transformTermRec(arg, owner, expectVar = true)
            classifyTerm(value) match {
              case VarW(_) =>
                value
              case _ =>
                val newFun = transformTermRec(fun, owner, expectVar = false)
                Apply.copy(term)(newFun, List(value))
            }
          case Apply(fun, List(arg)) if isVirtualizedBoolConv(fun) =>
            val value = transformTermRec(arg, owner, expectVar = false)
            classifyTerm(value) match {
              case Bare(_) =>
                val newFun = transformTermRec(fun, owner, expectVar = false)
                Apply.copy(term)(newFun, List(value))
              case _ =>
                value
            }
          case applyTerm @ Apply(fun, List(arg)) if isRepLiftConversion(fun) =>
            val value = transformTermRec(arg, owner, expectVar = false)
            if sameElementRepLift(applyTerm, value) then value
            else {
              val newFun = transformTermRec(fun, owner, expectVar = false)
              Apply.copy(applyTerm)(newFun, List(value))
            }
          case ifTerm: If =>
            rewriteIf(ctx, ifTerm)
          case ret: Return =>
            rewriteReturn(ctx, ret)
          case throwApply @ Apply(Ident("throw"), List(throwExpr)) =>
            rewriteThrow(ctx, throwApply, throwExpr, owner)
          case tryTerm: Try =>
            rewriteTryCatch(ctx, tryTerm)
          case matchTerm: Match =>
            rewriteMatch(ctx, matchTerm)
          case applyTerm @ Apply(sel @ Select(lhs, op @ ("&&" | "||")), List(rhs)) =>
            val method = if (op == "&&") "boolean_and" else "boolean_or"
            rewriteBooleanBinary(ctx, applyTerm, sel, lhs, rhs, method)
          case sel @ Select(expr, "unary_!") =>
            rewriteBooleanNegateSelect(ctx, sel, expr)
          case sel @ Select(expr, "unary_~") =>
            rewriteIntUnary(ctx, sel, expr, "int_bitwise_not")
          case applyTerm @ Apply(sel @ Select(_, "unary_!"), List(arg)) =>
            val raw = transformTermRec(arg, owner, expectVar = false)
            val (value, kind) = normalizeRepTerm(raw, ctx)
            kind match {
              case Bare(_) =>
                super.transformTerm(applyTerm)(owner)
              case _ =>
                val call = Select.overloaded(ctx.thist, "boolean_negate", Nil, List(wrapBareBoolean(value, ctx)))
                Apply(call, List(ctx.srcGen))
            }
          case applyTerm @ Apply(sel @ Select(lhs, "=="), List(rhs)) =>
            rewriteEquality(ctx, applyTerm, sel, lhs, rhs, negate = false)
          case applyTerm @ Apply(sel @ Select(lhs, "!="), List(rhs)) =>
            rewriteEquality(ctx, applyTerm, sel, lhs, rhs, negate = true)
          case applyTerm @ Apply(sel @ Select(lhs, "%"), List(rhs)) =>
            rewriteIntBinary(ctx, applyTerm, sel, lhs, rhs, "int_mod")
          case applyTerm @ Apply(sel @ Select(lhs, "&"), List(rhs)) =>
            rewriteIntBinary(ctx, applyTerm, sel, lhs, rhs, "int_binaryand")
          case applyTerm @ Apply(sel @ Select(lhs, "|"), List(rhs)) =>
            rewriteIntBinary(ctx, applyTerm, sel, lhs, rhs, "int_binaryor")
          case applyTerm @ Apply(sel @ Select(lhs, "^"), List(rhs)) =>
            rewriteIntBinary(ctx, applyTerm, sel, lhs, rhs, "int_binaryxor")
          case applyTerm @ Apply(sel @ Select(lhs, "<<"), List(rhs)) =>
            rewriteIntBinary(ctx, applyTerm, sel, lhs, rhs, "int_leftshift")
          case applyTerm @ Apply(sel @ Select(lhs, ">>"), List(rhs)) =>
            rewriteIntBinary(ctx, applyTerm, sel, lhs, rhs, "int_rightshiftarith")
          case applyTerm @ Apply(sel @ Select(lhs, ">>>"), List(rhs)) =>
            rewriteIntBinary(ctx, applyTerm, sel, lhs, rhs, "int_rightshiftlogical")
          case applyTerm @ Apply(inner @ Apply(sel @ Select(lhsOuter, op @ ("+" | "-" | "*" | "/")), List(rhs)), implicitArgs)
              if implicitArgs.nonEmpty =>
            rewriteArithmeticBinaryWithImplicit(ctx, applyTerm, inner, sel, lhsOuter, rhs, op, implicitArgs)
          case applyTerm @ Apply(inner @ Apply(TypeApply(sel @ Select(lhsOuter, op @ ("+" | "-" | "*" | "/")), _), List(rhs)), implicitArgs)
              if implicitArgs.nonEmpty =>
            rewriteArithmeticBinaryWithImplicit(ctx, applyTerm, inner, sel, lhsOuter, rhs, op, implicitArgs)
          case applyTerm @ Apply(tapply @ TypeApply(sel @ Select(lhs, op @ ("+" | "-" | "*" | "/")), _), List(rhs)) =>
            rewriteArithmeticBinaryTypeApply(ctx, applyTerm, tapply, sel, lhs, rhs, op)
          case applyTerm @ Apply(sel @ Select(lhs, op @ ("+" | "-" | "*" | "/")), List(rhs)) =>
            rewriteArithmeticBinary(ctx, applyTerm, sel, lhs, rhs, op)
          case applyTerm @ Apply(inner @ Apply(sel @ Select(lhsOuter, "<"), List(rhs)), implicitArgs)
              if implicitArgs.nonEmpty =>
            rewriteOrdering(ctx, applyTerm, sel, lhsOuter, rhs, "ordering_lt")
          case applyTerm @ Apply(sel @ Select(lhs, "<"), List(rhs)) =>
            rewriteOrdering(ctx, applyTerm, sel, lhs, rhs, "ordering_lt")
          case applyTerm @ Apply(inner @ Apply(sel @ Select(lhsOuter, "<="), List(rhs)), implicitArgs)
              if implicitArgs.nonEmpty =>
            rewriteOrdering(ctx, applyTerm, sel, lhsOuter, rhs, "ordering_lteq")
          case applyTerm @ Apply(sel @ Select(lhs, "<="), List(rhs)) =>
            rewriteOrdering(ctx, applyTerm, sel, lhs, rhs, "ordering_lteq")
          case applyTerm @ Apply(inner @ Apply(sel @ Select(lhsOuter, ">"), List(rhs)), implicitArgs)
              if implicitArgs.nonEmpty =>
            rewriteOrdering(ctx, applyTerm, sel, lhsOuter, rhs, "ordering_gt")
          case applyTerm @ Apply(sel @ Select(lhs, ">"), List(rhs)) =>
            rewriteOrdering(ctx, applyTerm, sel, lhs, rhs, "ordering_gt")
          case applyTerm @ Apply(inner @ Apply(sel @ Select(lhsOuter, ">="), List(rhs)), implicitArgs)
              if implicitArgs.nonEmpty =>
            rewriteOrdering(ctx, applyTerm, sel, lhsOuter, rhs, "ordering_gteq")
          case applyTerm @ Apply(sel @ Select(lhs, ">="), List(rhs)) =>
            rewriteOrdering(ctx, applyTerm, sel, lhs, rhs, "ordering_gteq")
          case applyTerm @ Apply(Apply(sel @ Select(th, "apply"), List(receiver, idx)), implicitArgs)
              if isStringOpsApply(sel.symbol) =>
            rewriteRepStringApply(ctx, receiver, idx, ctx.owner)
          case whileTerm @ While(condTree, bodyTree) =>
            val guardRaw = transformTermRec(stripBoolConv(condTree), owner, expectVar = false)
            val (guard, guardKind) = normalizeRepTerm(guardRaw, ctx)
            val body = transformTermRec(bodyTree, owner, expectVar = false)
            guardKind match {
              case Bare(_) =>
                While.copy(whileTerm)(guard, body)
              case _ =>
                rewriteWhile(ctx, guard, body)
            }
          case applyTerm @ Apply(fun, args) =>
            val newFun = transformTermRec(fun, owner, expectVar = false)
            val newArgs = args.map(arg => stripVarConversion(transformTermRec(arg, owner, expectVar = false)))
            Apply.copy(applyTerm)(newFun, adaptApplyArgs(ctx, newFun, newArgs))
          case _ => super.transformTerm(term)(owner)
        }
        rewriteResidualArithmetic(ctx, rewritten)
      }

      override def transformStatement(statement: Statement)(owner: Symbol): Statement = statement match {
        case assign @ Assign(lhsTree, rhsTree) =>
          val lhsTerm = transformTermRec(lhsTree, owner, expectVar = true)
          val rhsTerm = transformTermRec(rhsTree, owner, expectVar = false)
          mutableVars.get(mutableTermSymbol(lhsTerm)) match {
            case Some(elemType) =>
              val strippedRhs = stripVarConversion(rhsTerm)
              val rhsKind = classifyTerm(strippedRhs)
              val ctx = makeCtx(owner)
              assignCall(ctx, elemType, lhsTerm, strippedRhs, rhsKind)
            case None =>
              Assign.copy(assign)(lhsTerm, rhsTerm)
          }
        case whileTerm: While =>
          transformTerm(whileTerm)(owner)
        case d: Definition =>
          transformLocalDefinition(d, owner)
        case term: Term =>
          transformTerm(term)(owner)
        case other =>
          super.transformStatement(other)(owner)
      }
    }

    def rewriteTerm(tree: Term, owner: Symbol): Term =
      Virtualizer.transformTerm(tree)(owner)
    
    def rewriteStatement(tree: Statement): Statement = tree match {
      case d: Definition => rewriteDefinition(d)
      case other => other
    }

    def rewriteDefinition(defn: Definition): Definition = defn match {
      case dd @ DefDef(name, paramss, tpt, rhs) =>
        val newRhs = rhs.map(rewriteTerm(_, dd.symbol))
        DefDef.copy(dd)(name = name, paramss = paramss, tpt = tpt, rhs = newRhs)
      case vd @ ValDef(name, tpt, rhs) if rhs.nonEmpty =>
        ValDef.copy(vd)(name = name, tpt = tpt, rhs = rhs.map(rewriteTerm(_, vd.symbol)))
      case cd @ ClassDef(name, constr, parents, selfOpt, body) =>
        val newBody = body.map(rewriteStatement)
        ClassDef.copy(cd)(name = name, constr = constr, parents = parents, selfOpt = selfOpt, body = newBody)
      case other => other
    }
    
    List(rewriteDefinition(definition))
  }
  
  
}
