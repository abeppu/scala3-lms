package lms.gen

import lms.legacy.common.{Base, BaseExp}
import lms.legacy.internal.{Blocks, Expressions}

import scala.quoted.{Expr, Quotes, Type}

trait QuotedGen extends Gen { // this is the interface for the code generator
  

  // import scala.quoted.*

  def find[T: Type](e: Exp[T])(using q: Quotes, lookup: Map[Sym[?], q.reflect.Symbol]): Expr[T] = {
    println(s"find: e = ${e}, lookup = ${lookup}")
    import q.reflect.*
    e match {
      case s@Sym(_) =>
        lookup.get(s) match {
          case Some(symbol) => Ref(symbol).asExprOf[T] // Regenerate the `Expr` reference
          case None => throw new Exception(s"Symbol $s not found in lookup map")
        }
      case c@Const(x: T) => constantTerm(c).asExprOf[T] // constantExpr(c)
      case _ => throw new Exception(s"Unsupported expression: $e")
    }
  }

  def interpretSchedule[A](reifiedSubgraph: (Exp[A], List[Stm]))
                          (using q: Quotes, initialEnv: Map[Sym[?], q.reflect.Symbol]): q.reflect.Term = {
    var env: Map[Sym[?], q.reflect.Symbol] = initialEnv // Start with the initial environment
    println(s"interpretSchedule: reifiedSubgraph = ${reifiedSubgraph}")
    import q.reflect.{Symbol, Flags, TypeRepr, ValDef, Ref, Statement, Block}
    val (result, statements) = reifiedSubgraph
    // Map from Exp to variable name

    println("env = " + env)
    // Generate val bindings
    val dfs = statements.map { case TP(sym, d: Def[?]) => d }
    val valDefs: List[Statement] = statements.map { case TP(sym, d: Def[?]) =>
      val rhs = interpretDefWithEnv(d)(using q, env)
      println(s"RHS = $rhs")
      val symbolName = "x" + sym.id // Generate a unique name for the symbol
      // this only works if every statement defines a variable of the same type as the result

      val symTypeRepr: TypeRepr = sym.tp.asTypeRepr

      val lhsSymbol = Symbol.newVal(Symbol.spliceOwner, symbolName, symTypeRepr, Flags.EmptyFlags, Symbol.noSymbol)
      env += (sym -> lhsSymbol)
      ValDef(
        lhsSymbol,
        Some(rhs) // The RHS is the interpreted definition
      )
    }

    // Final result, using env
    val resultTerm = result match {
      case c@Const(x) => constantTerm(c) // TODO support constant in block result position
      case s@Sym(_) => {
        Ref(env(s)) // Convert Sym to Expr using the environment
      }
      case _ => throw new Exception(s"unsupported block result ${result}")
    }
    // Combine all into a block
    Block(valDefs, resultTerm)
  }

  // Helper: interpret Exp, using env for variable references
  def interpretExpWithEnv[A](e: Exp[A])(using q: Quotes, env: Map[Sym[?], q.reflect.Symbol]): q.reflect.Term = {
    println(s"interpretExpWithEnv: e = ${e}, env = ${env}")
    import q.reflect.*
    e match {
      case c@Const(_) => constantTerm(c) //  constantExpr(c).asTerm
      case sym@Sym(_) if env.contains(sym) => Ref(env(sym))
      // what if sym isn't in env?
      case sym@Sym(_) => throw new Exception(s"Symbol $sym not found in environment: $env")
      // case Def(d) => interpretDefWithEnv(d)(using q, env) // can this happen?
    }
  }
  /*
  def interpretBlockWithEnv[A](b: Block[A])(using q: Quotes, env: Map[Sym[?], q.reflect.Symbol], lang: Expressions): q.reflect.Term = {
    println(s"interpretBlockWithEnv: b = ${b}, env = ${env}")
    import q.reflect.*
    val (result, statements) = reifySubGraph(b.res)
    interpretSchedule((result, statements))(using q, env, lang)
  }*/

}
