package lms.core.examples

import lms.core.*
import lms.gen.{Gen, StagingCompile}
import lms.legacy.compat.SourceContext
import lms.legacy.common.{Base, EffectExp}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.language.implicitConversions
import scala.quoted.*

trait TutorialDslApiBase extends Base { this: Dsl =>
  def hashCodeRep(s: Rep[String])(using pos: SourceContext): Rep[Int]
}

trait TutorialDslApiExp extends DslExp with TutorialDslApiBase with EffectExp {
  case class StringHashCode(s: Exp[String]) extends Def[Int]

  def hashCodeRep(s: Rep[String])(using pos: SourceContext): Rep[Int] =
    StringHashCode(s)
}

trait TutorialDslApiGen extends Gen with TutorialDslApiExp { this: StagingCompile =>
  override def interpretDefWithEnv[A](d: Def[A])(using q: Quotes, env: Map[Sym[?], q.reflect.Symbol]): q.reflect.Term = {
    import q.reflect.*
    d match {
      case StringHashCode(s) =>
        val value = interpretExpWithEnv(s).asExprOf[String]
        '{ $value.## }.asTerm
      case _ =>
        super.interpretDefWithEnv(d)
    }
  }
}

trait TutorialScalaGenDslApi extends DslGen {
  val IR: TutorialDslApiExp
  import IR.*

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case StringHashCode(s) =>
      emitValDef(sym, s"${quote(s)}.##")
    case _ =>
      super.emitNode(sym, rhs)
  }
}

@virt
trait TutorialDslApiSnippets extends Dsl with TutorialDslApiBase {
  def score(s: Rep[String]): Rep[Int] = {
    val h = hashCodeRep(s)
    val len = s.length
    h + len
  }
}

@virt
object TutorialDslApiSnippet extends DslDriver[String, Int] with Dsl with TutorialDslApiSnippets with TutorialDslApiExp { self =>
  override val codegen = new TutorialScalaGenDslApi {
    val IR: self.type = self
  }

  def snippet(x: Rep[String]): Rep[Int] = score(x)
}

class TutorialDslApiTest extends AnyFunSuite with Matchers {
  test("dslapi style custom op can run through runtime compilation") {
    val compiler = new TutorialDslApiSnippets with DslCompile with TutorialDslApiExp with TutorialDslApiGen
    val f: compiler.Exp[String] => compiler.Exp[Int] = compiler.score(_)
    val staged = compiler.compile(f)

    List("", "lms", "scala3").foreach { s =>
      staged(s) shouldBe (s.## + s.length)
    }
  }

  test("dslapi style custom op emits dedicated Scala codegen node") {
    TutorialDslApiSnippet.code should include(".##")
  }
}
