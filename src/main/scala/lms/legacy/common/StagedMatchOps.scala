package lms.legacy.common

import lms.legacy.compat.SourceContext

trait StagedMatchOps extends IfThenElse with CastingOps with Equal {
  case class StagedMatchCase[S, T](test: Rep[S] => Rep[Boolean], body: Rep[S] => Rep[T])

  def stagedCase[S, T](test: Rep[S] => Rep[Boolean])(body: Rep[S] => Rep[T]): StagedMatchCase[S, T] =
    StagedMatchCase(test, body)

  def stagedValueCase[S: Typ, T](value: Rep[S])(body: Rep[S] => Rep[T])(using SourceContext): StagedMatchCase[S, T] =
    stagedCase[S, T](scrutinee => equals(scrutinee, value))(body)

  def stagedTypeCase[S: Typ, A: Typ, T](body: Rep[A] => Rep[T])(using SourceContext): StagedMatchCase[S, T] =
    stagedCase[S, T](scrutinee => rep_isinstanceof[S, A](scrutinee, typ[S], typ[A])) { scrutinee =>
      body(rep_asinstanceof[S, A](scrutinee, typ[S], typ[A]))
    }

  def stagedMatch[S, T: Typ](scrutinee: Rep[S])(cases: StagedMatchCase[S, T]*)(default: Rep[S] => Rep[T])(using SourceContext): Rep[T] =
    cases.toList.foldRight(default(scrutinee)) { (c, otherwise) =>
      __ifThenElse(c.test(scrutinee), c.body(scrutinee), otherwise)
    }
}

trait StagedMatchOpsExp extends StagedMatchOps
