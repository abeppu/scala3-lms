package lms.gen


import scala.lms.internal.Expressions
import scala.quoted.Quotes

trait Gen extends Expressions { // meant to be the code gen interface for a module that introduces specific value types or defs
  // Can we describe an obligation to generate stuff for a specific family?
  // How do we deal with programs that combine different families?
  def constantTerm[T](c: Const[T])(using q: Quotes): q.reflect.Term =
    throw new Exception(s"Unsupported constant: $c")

  def interpretDefWithEnv[A](d: Def[A])(using q: Quotes, env: Map[Sym[?], q.reflect.Symbol]): q.reflect.Term =
    throw new Exception(s"Unsupported definition: $d")
}
