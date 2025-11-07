package lms.core

import lms.legacy.compat.SourceContext

import scala.Conversion
import scala.annotation.*
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

    def repOrVar(t: TypeRepr): RepOrVar = t.widen match {
      case AppliedType(f, List(arg)) =>
        // XXX - do something better
        if (f.show.endsWith("Exp") || f.show.endsWith("Rep")) {
          RepW(arg)
        }
        else if (f.show.endsWith("Var") || f.show.endsWith("Variable")) {
          VarW(arg)
        }
        else {
          Bare(arg)
        }
      case t => Bare(t)
    }      

    def wrapBareBoolean(term: Term, thist: Term): Term = repOrVar(term.tpe) match {
      case Bare(_) =>
        Select.overloaded(thist, "boolToBoolRep", Nil, List(term))
      case _ => term
    }
    
    def makeThis(owner: Symbol): Term = This(fetchEnclosingClass(owner))

    def findTypW(thist: Term, trep: TypeRepr): Term = {
      val t = TypeTree.of(using trep.asType)
      val ttyp = Applied(TypeSelect(thist, "Typ"), List(t))
      val typW = Implicits.search(ttyp.tpe) match {
        // Failure should be impossible, else we wouldn't have been
        // able to form the type Rep[T]
        case success: ImplicitSearchSuccess => success.tree
      }
      typW
    }
    object Virtualizer extends TreeMap {

      case class Ctx(thist: Term, srcGen: Term, owner: Symbol, unitf: Select)

      private def isVirtualizedBoolConv(fun: Term): Boolean = fun match {
        case Select(Select(_, "__virtualizedBoolConvInternal"), "apply") => true
        case _ => false
      }

      def rewriteIf(ctx: Ctx, ifTerm: If)(using Quotes): Term = {
        ifTerm match {
          case If(cond, thenp, elsep) => {
            val c = transformTerm(cond)(ctx.owner)
            val t1: Term = transformTerm(thenp)(ctx.owner)
            val e1 = transformTerm(elsep)(ctx.owner)
            val ttype: TypeRepr = t1.tpe.widen
            val innerT = repOrVar(ttype).t
            val thisClass = fetchEnclosingClass(ctx.owner)
            val typW = findTypW(ctx.thist, innerT)

            Apply(Select.overloaded(ctx.thist, "__ifThenElse", List(innerT), List(c, t1, e1)), List(typW, ctx.srcGen))
          }
        }
      }
      
      override def transformTerm(term: Term)(owner: Symbol): Term = {
        val thist = makeThis(owner)
        val srcGen = '{ SourceContext.generate }.asTerm
        val unitf: Select = findMethods(owner, "unit") match {
          case Nil =>
            report.errorAndAbort("LMS-internal error: no [unit] found for self")
          case x :: _ => thist.select(x)
        }
        val ctx = Ctx(thist, srcGen, owner, unitf)
        term match {
          case Apply(fun, List(arg)) if isVirtualizedBoolConv(fun) =>
            transformTerm(arg)(owner)
          case applyTerm @ Apply(sel @ Select(lhsTree, "&&"), List(rhsTree)) =>
            val lhs = transformTerm(lhsTree)(owner)
            val rhs = transformTerm(rhsTree)(owner)
            (repOrVar(lhs.tpe), repOrVar(rhs.tpe)) match {
              case (Bare(_), Bare(_)) =>
                val copiedSelect = Select.copy(sel)(lhs, sel.name)
                Apply.copy(applyTerm)(copiedSelect, List(rhs))
              case _ =>
                val andCall = Select.overloaded(ctx.thist, "boolean_and", Nil, List(wrapBareBoolean(lhs, ctx.thist), wrapBareBoolean(rhs, ctx.thist)))
                Apply(andCall, List(ctx.srcGen))
            }
          case ifTerm @ If(cond, thenp, elsep) => rewriteIf(ctx, ifTerm)
          case _ => super.transformTerm(term)(owner)
        }
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
