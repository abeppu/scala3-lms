package lms.core

import lms.legacy.compat.SourceContext

import scala.Conversion
import scala.annotation.*
import scala.math.Ordering
import scala.quoted.*

@experimental
class virt extends MacroAnnotation {

  def transform(using q: Quotes)(definition: q.reflect.Definition, companion: Option[q.reflect.Definition]): List[q.reflect.Definition] = {
    
    import q.reflect.*

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
          if (f.show.endsWith("Exp") || f.show.endsWith("Rep")) {
            RepW(arg)
          } else if (f.show.endsWith("Var") || f.show.endsWith("Variable")) {
            VarW(arg)
          } else {
            Bare(arg)
          }
        case other => Bare(other)
      }
    }

    def classifyTerm(term: Term): RepOrVar = {
      repOrVar(term.tpe) match {
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
            case _ => bare
          }
        case other => other
      }
    }

    case class MacroCtx(thist: Term, srcGen: Term, owner: Symbol, unitf: Select)

    // Lift host booleans into Rep form so DSL boolean ops can consume them.
    def wrapBareBoolean(term: Term, thist: Term): Term = classifyTerm(term) match {
      case Bare(_) =>
        Select.overloaded(thist, "boolToBoolRep", Nil, List(term))
      case _ => term
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
        case RepLike(tpe) =>
          val tTree = TypeTree.of(using tpe.asType)
          val typTpe = Applied(TypeSelect(ctx.thist, "Typ"), List(tTree))
          val typWitness = Implicits.search(typTpe.tpe) match {
            case success: ImplicitSearchSuccess => success.tree
            case _ => report.errorAndAbort(s"could not synthesize Typ for ${tpe.show}")
          }
          val lifted = Apply(Apply(TypeApply(ctx.unitf, List(tTree)), List(value)), List(typWitness))
          Block(stats, lifted)
        case _ =>
          report.errorAndAbort(s"expected Rep or liftable term, found ${value.show}")
      }
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
    
    def makeThis(owner: Symbol): Term = This(fetchEnclosingClass(owner))

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
                else if sym.name.nonEmpty then sym.name.head.toLower + sym.name.tail + "Typ"
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

    def selectThisMember(ctx: MacroCtx, name: String, error: => String): Term =
      try Select.unique(ctx.thist, name)
      catch
        case _: Throwable => report.errorAndAbort(error)
    object Virtualizer extends TreeMap {

      private def isVirtualizedBoolConv(fun: Term): Boolean = fun match {
        case Select(Select(_, "__virtualizedBoolConvInternal"), "apply") => true
        case _ => false
      }

      private def stripBoolConv(term: Term): Term = term match {
        case Apply(fun, List(arg)) if isVirtualizedBoolConv(fun) => stripBoolConv(arg)
        case other => other
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

      private def rebuildBinary(applyTerm: Apply, sel: Select, lhs: Term, rhs: Term): Term =
        Apply.copy(applyTerm)(Select.copy(sel)(lhs, sel.name), List(rhs))

      // Generic lift helper for arithmetic/ordering operands.
      private def wrapBareTerm(term: Term, ctx: MacroCtx): Term = classifyTerm(term) match {
        case Bare(tpe) =>
          val tTree = TypeTree.of(using tpe.asType)
          val typTpe = Applied(TypeSelect(ctx.thist, "Typ"), List(tTree))
          val typWitness = Implicits.search(typTpe.tpe) match {
            case success: ImplicitSearchSuccess => success.tree
            case _ => report.errorAndAbort(s"missing Typ evidence for ${tpe.show}")
          }
          Apply(Apply(TypeApply(ctx.unitf, List(tTree)), List(term)), List(typWitness))
        case _ => term
      }

      // Virtualize branch by routing condition and bodies through __ifThenElse.
      private def rewriteIf(ctx: MacroCtx, ifTerm: If): Term = {
        val guardTree = stripBoolConv(ifTerm.cond)
        val guard = transformTerm(guardTree)(ctx.owner)
        val guardKind = classifyTerm(guard)
        report.info(s"if guard kind: $guardKind", ifTerm.cond.pos)
        guardKind match {
          case Bare(_) =>
            val thenp = transformTerm(ifTerm.thenp)(ctx.owner)
            val elsep = transformTerm(ifTerm.elsep)(ctx.owner)
            If.copy(ifTerm)(guard, thenp, elsep)
          case _ =>
            val thenp = ensureTrailingRep(transformTerm(ifTerm.thenp)(ctx.owner), ctx)
            val elsep = ensureTrailingRep(transformTerm(ifTerm.elsep)(ctx.owner), ctx)
            val valueType = repOrVar(thenp.tpe.widen).t
            val typW = findTypW(ctx.thist, valueType)
            Apply(
              Select.overloaded(ctx.thist, "__ifThenElse", List(valueType), List(guard, thenp, elsep)),
              List(typW, ctx.srcGen))
        }
      }

      // Handle && / || by deferring to LMS boolean combinators only when reps are involved.
      private def rewriteBooleanBinary(ctx: MacroCtx, applyTerm: Apply, sel: Select, lhsTree: Term, rhsTree: Term, method: String): Term = {
        val lhs = transformTerm(stripBoolConv(lhsTree))(ctx.owner)
        val rhs = transformTerm(stripBoolConv(rhsTree))(ctx.owner)
        report.info(s"rewriting $method", sel.pos)
        (classifyTerm(lhs), classifyTerm(rhs)) match {
          case (Bare(_), Bare(_)) =>
            rebuildBinary(applyTerm, sel, lhs, rhs)
          case _ =>
            val call = Select.overloaded(ctx.thist, method, Nil, List(wrapBareBoolean(lhs, ctx.thist), wrapBareBoolean(rhs, ctx.thist)))
            Apply(call, List(ctx.srcGen))
        }
      }

      // Short helper for the unary ! select shape.
      private def rewriteBooleanNegateSelect(ctx: MacroCtx, sel: Select, expr: Term): Term = {
        val value = transformTerm(expr)(ctx.owner)
        classifyTerm(value) match {
          case Bare(_) =>
            Select.copy(sel)(value, sel.name)
          case _ =>
            val call = Select.overloaded(ctx.thist, "boolean_negate", Nil, List(wrapBareBoolean(value, ctx.thist)))
            Apply(call, List(ctx.srcGen))
        }
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

      // Forward ordering comparisons to the DSL once reps participate.
      private def rewriteOrdering(ctx: MacroCtx, applyTerm: Apply, sel: Select, lhsTree: Term, rhsTree: Term, method: String): Term = {
        val lhs = transformTerm(lhsTree)(ctx.owner)
        val rhs = transformTerm(rhsTree)(ctx.owner)
        println(s"< lhs BEFORE ${lhsTree.show(using Printer.TreeCode)}")
        println(s"< lhs AFTER ${lhs.show(using Printer.TreeCode)}")
        println(s"< rhs: ${rhs.show(using Printer.TreeCode)}")
        val lhsKind = classifyTerm(lhs)
        val rhsKind = classifyTerm(rhs)
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
        println(s"elemType = ${elemType.show(using Printer.TypeReprCode)}")
        val args = List(wrapBareTerm(lhs, ctx), wrapBareTerm(rhs, ctx))
        println(s"args = [${args.map(_.show)}]")
        val methodSym = findMethods(ctx.owner, method) match {
          case sym :: _ => sym
          case Nil => report.errorAndAbort(s"failed to virtualize: no $method in scope")
        }
        val methodRef = Select(ctx.thist, methodSym)
        val typedMethod = methodRef.appliedToTypes(List(elemType))
        val applied = typedMethod.appliedToArgs(args)
        val orderingEvidence = findOrderingW(elemType)
        println(s"orderingEvidence : ${orderingEvidence.show}")
        val typEvidence = findTypW(ctx.thist, elemType)
        val ret = applied.appliedToArgs(List(orderingEvidence, typEvidence, ctx.srcGen))
        println(s"< rewrite: ${ret.show(using Printer.TreeAnsiCode)}")
        println(s"< rewrite: ${ret.show(using Printer.TreeCode)}")
        println(s"< rewrite: ${ret.show(using Printer.TreeStructure)}")
        ret
      }

      // Replace while loops with __whileDo(cond, body).
      private def rewriteWhile(ctx: MacroCtx, guard: Term, body: Term): Term = {
        val normalizedBody = dropTrailingUnitInWhileBody(body, ctx)
        val method = findMethods(ctx.owner, "__whileDo") match {
          case _ :: symb :: _ => symb
          case symb :: Nil => symb
          case Nil => report.errorAndAbort("failed to virtualize: no __whileDo in scope")
        }
        val whileCall = Apply(ctx.thist.select(method), List(guard, normalizedBody))
        Apply(whileCall, List(ctx.srcGen))
      }
      
      private def transformLocalDefinition(defn: Definition, owner: Symbol): Definition = defn match {
        case dd: DefDef =>
          val newRhs = dd.rhs.map(transformTerm(_)(dd.symbol))
          DefDef.copy(dd)(name = dd.name, paramss = dd.paramss, tpt = dd.tpt, rhs = newRhs)
        case vd: ValDef if vd.rhs.nonEmpty =>
          val newRhs = vd.rhs.map(transformTerm(_)(vd.symbol))
          ValDef.copy(vd)(name = vd.name, tpt = vd.tpt, rhs = newRhs)
        case other => other
      }

      override def transformTerm(term: Term)(owner: Symbol): Term = {
        val ctx = makeCtx(owner)
        term match {
          case inlined @ Inlined(call, bindings, body) =>
            val newBindings = bindings.map(b => transformLocalDefinition(b, owner))
            Inlined.copy(inlined)(call, newBindings, transformTerm(body)(owner))
          case block @ Block(stats, expr) =>
            val newStats = stats.map(transformStatement(_)(owner))
            Block.copy(block)(newStats, transformTerm(expr)(owner))
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
              case Some(rhs) => transformTerm(rhs)(owner)
              case None => transformTerm(arg)(owner)
            }
            classifyTerm(value) match {
              case Bare(_) =>
                val receiver = transformTerm(th)(owner)
                Apply.copy(term)(Select.copy(sel)(receiver, sel.name), List(value))
              case _ =>
                value
            }
          case ifTerm: If =>
            rewriteIf(ctx, ifTerm)
          case applyTerm @ Apply(sel @ Select(lhs, op @ ("&&" | "||")), List(rhs)) =>
            val method = if (op == "&&") "boolean_and" else "boolean_or"
            rewriteBooleanBinary(ctx, applyTerm, sel, lhs, rhs, method)
          case sel @ Select(expr, "unary_!") =>
            rewriteBooleanNegateSelect(ctx, sel, expr)
          case applyTerm @ Apply(sel @ Select(_, "unary_!"), List(arg)) =>
            val value = transformTerm(arg)(owner)
            classifyTerm(value) match {
              case Bare(_) =>
                super.transformTerm(applyTerm)(owner)
              case _ =>
                val call = Select.overloaded(ctx.thist, "boolean_negate", Nil, List(wrapBareBoolean(value, ctx.thist)))
                Apply(call, List(ctx.srcGen))
            }
          case applyTerm @ Apply(sel @ Select(lhs, "=="), List(rhs)) =>
            rewriteEquality(ctx, applyTerm, sel, lhs, rhs, negate = false)
          case applyTerm @ Apply(sel @ Select(lhs, "!="), List(rhs)) =>
            rewriteEquality(ctx, applyTerm, sel, lhs, rhs, negate = true)
            /*
          case applyTerm @ Apply(sel @ Select(lhs, "<"), List(rhs)) =>
            rewriteOrdering(ctx, applyTerm, sel, lhs, rhs, "ordering_lt")
          case applyTerm @ Apply(sel @ Select(lhs, "<="), List(rhs)) =>
            rewriteOrdering(ctx, applyTerm, sel, lhs, rhs, "ordering_lteq")
          case applyTerm @ Apply(sel @ Select(lhs, ">"), List(rhs)) =>
            rewriteOrdering(ctx, applyTerm, sel, lhs, rhs, "ordering_gt")
          case applyTerm @ Apply(sel @ Select(lhs, ">="), List(rhs)) =>
            rewriteOrdering(ctx, applyTerm, sel, lhs, rhs, "ordering_gteq") */
          case whileTerm @ While(condTree, bodyTree) =>
            val guard = transformTerm(stripBoolConv(condTree))(ctx.owner)
            val body = transformTerm(bodyTree)(ctx.owner)
            val guardKind = classifyTerm(guard)
            report.info(s"while guard kind: $guardKind", condTree.pos)
            guardKind match {
              case Bare(_) =>
                While.copy(whileTerm)(guard, body)
              case _ =>
                rewriteWhile(ctx, guard, body)
            }
          case _ => super.transformTerm(term)(owner)
        }
      }

      override def transformStatement(statement: Statement)(owner: Symbol): Statement = statement match {
        case whileTerm: While =>
          transformTerm(whileTerm)(owner)
        case d: Definition => transformLocalDefinition(d, owner)
        case term: Term => transformTerm(term)(owner)
        case other => super.transformStatement(other)(owner)
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
