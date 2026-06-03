package lms.core.examples

import lms.core.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.language.implicitConversions

sealed trait EvalTerm
case class ConstI(n: Int) extends EvalTerm
case object ArgI extends EvalTerm
case class AddI(lhs: EvalTerm, rhs: EvalTerm) extends EvalTerm
case class MulI(lhs: EvalTerm, rhs: EvalTerm) extends EvalTerm
case class IfLtI(condLhs: EvalTerm, condRhs: EvalTerm, thenp: EvalTerm, elsep: EvalTerm) extends EvalTerm

@virt
trait TutorialEvalSnippets extends Dsl {
  def evalStaged(term: EvalTerm, arg: Rep[Int]): Rep[Int] = term match {
    case ConstI(n) => n
    case ArgI => arg
    case AddI(l, r) => evalStaged(l, arg) + evalStaged(r, arg)
    case MulI(l, r) => evalStaged(l, arg) * evalStaged(r, arg)
    case IfLtI(cl, cr, t, e) =>
      if (evalStaged(cl, arg) < evalStaged(cr, arg)) evalStaged(t, arg)
      else evalStaged(e, arg)
  }
}

class TutorialEvalTest extends AnyFunSuite with Matchers {
  private val compiler = new TutorialEvalSnippets with DslCompile

  private def evalHost(term: EvalTerm, arg: Int): Int = term match {
    case ConstI(n) => n
    case ArgI => arg
    case AddI(l, r) => evalHost(l, arg) + evalHost(r, arg)
    case MulI(l, r) => evalHost(l, arg) * evalHost(r, arg)
    case IfLtI(cl, cr, t, e) =>
      if (evalHost(cl, arg) < evalHost(cr, arg)) evalHost(t, arg)
      else evalHost(e, arg)
  }

  test("eval tutorial specializer compiles a static expression AST into code") {
    val term: EvalTerm =
      IfLtI(
        ArgI,
        ConstI(5),
        AddI(MulI(ArgI, ArgI), ConstI(1)),
        AddI(ArgI, ConstI(10))
      )

    val f: compiler.Exp[Int] => compiler.Exp[Int] = x => compiler.evalStaged(term, x)
    val staged = compiler.compile(f)

    List(-2, 0, 3, 5, 9).foreach { x =>
      staged(x) shouldBe evalHost(term, x)
    }
  }

  test("eval tutorial generated code keeps dynamic branch from staged input") {
    @virt
    object Snippet extends DslDriver[Int, Int] with Dsl with TutorialEvalSnippets {
      private val term: EvalTerm =
        IfLtI(ArgI, ConstI(5), AddI(ArgI, ConstI(1)), AddI(ArgI, ConstI(2)))
      def snippet(x: Rep[Int]): Rep[Int] = evalStaged(term, x)
    }

    Snippet.code should include("if (")
  }
}
