package lms.core.examples

import lms.core.*
import lms.legacy.common.ForwardTransformer
import lms.legacy.compat.SourceContext
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.language.implicitConversions

trait Sliding extends Dsl {
  final class SlidingSizeOps(n: Rep[Int]) {
    def sliding[T: Typ](f: Rep[Int] => Rep[T]): Rep[Array[T]] = {
      val out = NewArray[T](n)
      Sliding.this.sliding(unit(0), n) { i => out(i) = f(i) }
      out
    }
  }

  final class SlidingRangeOps(r: Rep[Range]) {
    def sliding: SlidingRangeForeachOps = new SlidingRangeForeachOps(r)
  }

  final class SlidingRangeForeachOps(r: Rep[Range]) {
    def foreach(f: Rep[Int] => Rep[Unit]): Rep[Unit] =
      Sliding.this.sliding(infix_start(r), infix_end(r))(f)
  }

  given Conversion[Rep[Int], SlidingSizeOps] with
    def apply(n: Rep[Int]): SlidingSizeOps = new SlidingSizeOps(n)

  given Conversion[Rep[Range], SlidingRangeOps] with
    def apply(r: Rep[Range]): SlidingRangeOps = new SlidingRangeOps(r)

  def sliding(start: Rep[Int], end: Rep[Int])(f: Rep[Int] => Rep[Unit]): Rep[Unit]
}

trait NoSlidingExp extends DslExp with Sliding {
  def sliding(start: Rep[Int], end: Rep[Int])(f: Rep[Int] => Rep[Unit]): Rep[Unit] =
    range_foreach(range_until(start, end), f)
}

trait SlidingExp extends DslExp with Sliding {
  object trans extends ForwardTransformer {
    val IR: SlidingExp.this.type = SlidingExp.this
  }

  type Subst = scala.collection.immutable.Map[Exp[Any], Exp[Any]]
  type Reified = (Rep[Unit], List[Stm])
  type Shifted = (Rep[Unit], List[Stm], Subst)

  override def int_plus(lhs: Exp[Int], rhs: Exp[Int])(using pos: SourceContext): Exp[Int] =
    ((lhs, rhs) match {
      case (Def(IntPlus(x: Exp[Int], Const(y: Int))), Const(z: Int)) => int_plus(x, unit(y + z))
      case (Def(IntMinus(x: Exp[Int], Const(y: Int))), Const(z: Int)) => int_minus(x, unit(y - z))
      case (x: Exp[Int], Const(z: Int)) if z < 0                     => int_minus(x, unit(-z))
      case _                                                         => super.int_plus(lhs, rhs)
    }).asInstanceOf[Exp[Int]]

  override def int_minus(lhs: Exp[Int], rhs: Exp[Int])(using pos: SourceContext): Exp[Int] =
    ((lhs, rhs) match {
      case (Def(IntMinus(x: Exp[Int], Const(y: Int))), Const(z: Int)) => int_minus(x, unit(y + z))
      case (Def(IntPlus(x: Exp[Int], Const(y: Int))), Const(z: Int))  => int_plus(x, unit(y - z))
      case (x: Exp[Int], Const(z: Int)) if z < 0                      => int_plus(x, unit(-z))
      case _                                                          => super.int_minus(lhs, rhs)
    }).asInstanceOf[Exp[Int]]

  def findOverlap(i: Sym[Int], f: Rep[Int] => Rep[Unit]): (List[Sym[Any]], Reified, Shifted) = {
    val saved = context
    val (r0, stms0) = reifySubGraph(f(i))
    val (((r1, stms1, subst1), (r2, stms2, _)), _) = reifySubGraph {
      reflectSubGraph(stms0)
      context = saved
      val ((r1, subst1), stms1) = reifySubGraph(trans.withSubstScope(i -> (i + 1)) {
        stms0.foreach(trans.traverseStm)
        (trans(r0), trans.subst)
      })
      val ((r2, stms2, subst2), _) = reifySubGraph {
        reflectSubGraph(stms1)
        context = saved
        val ((r2, subst2), stms2) = reifySubGraph(trans.withSubstScope(i -> (i + 2)) {
          stms0.foreach(trans.traverseStm)
          (trans(r0), trans.subst)
        })
        (r2, stms2, subst2)
      }
      ((r1, stms1, subst1), (r2, stms2, subst2))
    }
    context = saved

    val defs = stms0.flatMap {
      case TP(sym, _) => List(sym)
    }
    val overlap01 = stms1.flatMap { case TP(_, d) => syms(d).filter(defs.contains) }.distinct
    val overlap02 = stms2.flatMap { case TP(_, d) => syms(d).filter(defs.contains) }.distinct
    val overlap0 = (overlap01 ++ overlap02).distinct
    (overlap0, (r0, stms0), (r1, stms1, subst1))
  }

  def sliding(start: Rep[Int], end: Rep[Int])(f: Rep[Int] => Rep[Unit]): Rep[Unit] =
    __ifThenElse(
      end > start,
      {
        val i = fresh[Int]
        val (overlap0, (r0, stms0), (r1, stms1, subst1)) = findOverlap(i, f)
        val overlap1 = overlap0.map(subst1)
        val substX = trans.withSubstScope(i -> start) {
          stms0.foreach(trans.traverseStm)
          trans(r0)
          trans.subst
        }
        val vars = overlap0.map { x => var_new(substX(x))(using x.tp, x.pos.head) }
        for (j <- (start + 1).until(end)) {
          generate_comment("variable reads")
          val reads = (overlap0 zip vars).map { case (sym, cell) =>
            (sym, readVar(cell)(using sym.tp, sym.pos.head))
          }
          generate_comment("computation")
          val (_, shifted: Subst) = trans.withSubstScope((reads :+ (i -> (j - 1)))* ) {
            stms1.foreach(trans.traverseStm)
            (trans(r1), trans.subst)
          }
          generate_comment("variable writes")
          overlap1.zip(vars).foreach { case (sym, cell) =>
            var_assign(cell, shifted(sym))(using sym.tp, sym.pos.head)
          }
        }
      },
      unit(())
    )
}

trait SlidingMultiExp extends SlidingExp with DslExp with Sliding {
  override def findOverlap(i: Sym[Int], f: Rep[Int] => Rep[Unit]): (List[Sym[Any]], Reified, Shifted) = {
    val saved = context
    val (r0, stms0) = reifySubGraph(f(i))
    val defs = stms0.flatMap {
      case TP(sym, _) => List(sym)
    }

    def step(n: Int, last: List[Stm], acc: List[Shifted], overlap: List[Sym[Any]]): (Shifted, List[Sym[Any]]) = {
      val (result, _) = reifySubGraph {
        reflectSubGraph(last)
        context = saved
        val ((ri, substi), stmsi) = reifySubGraph(trans.withSubstScope(i -> (i + n)) {
          stms0.foreach(trans.traverseStm)
          (trans(r0), trans.subst)
        })
        val overlapi = stmsi.flatMap { case TP(_, d) => syms(d).filter(defs.contains) }.distinct
        if (overlapi.nonEmpty) step(n + 1, stmsi, (ri, stmsi, substi) :: acc, (overlap ++ overlapi).distinct)
        else ((ri, stmsi, substi), overlap)
      }
      result
    }

    val ((r1, stms1, subst1), overlap0) = step(1, stms0, Nil, Nil)
    context = saved
    (overlap0, (r0, stms0), (r1, stms1, subst1))
  }
}

trait SlidingWarmup extends Sliding {
  def snippet(n: Rep[Int]): Rep[Array[Int]] = {
    def compute(i: Rep[Int]): Rep[Int] = 2 * i + 3
    n.sliding { i => compute(i) + compute(i + 1) }
  }
}

trait Stencil extends Sliding {
  def snippet(v: Rep[Array[Double]]): Rep[Array[Double]] = {
    val n = v.length
    val input = v
    val output = NewArray[Double](n)

    def a(j: Rep[Int]): Rep[Double] = input(j)
    def w1(j: Rep[Int]): Rep[Double] = a(j) * a(j + 1)
    def wm(j: Rep[Int]): Rep[Double] = a(j) - w1(j) + w1(j - 1)
    def w2(j: Rep[Int]): Rep[Double] = wm(j) * wm(j + 1)
    def b(j: Rep[Int]): Rep[Double] = wm(j) - w2(j) + w2(j - 1)

    for (i <- infix_until(2, n - 2).sliding) {
      output(i) = b(i)
    }
    output
  }
}

class TutorialStencilTest extends AnyFunSuite with Matchers {
  private def countSubstring(code: String, needle: String): Int =
    code.sliding(needle.length).count(_ == needle)

  test("sliding warmup reuses loop-carried values in generated code") {
    object Baseline extends DslDriver[Int, Array[Int]] with SlidingWarmup with NoSlidingExp
    object Sliding1 extends DslDriver[Int, Array[Int]] with SlidingWarmup with SlidingExp

    val baselineReads = countSubstring(Baseline.code, ".apply(")
    val slidingReads = countSubstring(Sliding1.code, ".apply(")

    Baseline.code should not include("variable reads")
    slidingReads should be <= baselineReads
    Sliding1.code should include("variable reads")
    Sliding1.code should include("variable writes")
  }

  test("multi sliding stays on the same optimization path for warmup") {
    object Sliding1 extends DslDriver[Int, Array[Int]] with SlidingWarmup with SlidingExp
    object SlidingN extends DslDriver[Int, Array[Int]] with SlidingWarmup with SlidingMultiExp

    SlidingN.code should include("variable reads")
    SlidingN.code should include("variable writes")
    countSubstring(SlidingN.code, ".apply(") shouldBe countSubstring(Sliding1.code, ".apply(")
  }

  test("stencil example still emits loop-based staged code") {
    object Baseline extends DslDriver[Array[Double], Array[Double]] with Stencil with NoSlidingExp
    object Sliding1 extends DslDriver[Array[Double], Array[Double]] with Stencil with SlidingExp
    object SlidingN extends DslDriver[Array[Double], Array[Double]] with Stencil with SlidingMultiExp

    Baseline.code should include("while (")
    Sliding1.code should include("while (")
    SlidingN.code should include("while (")
    Sliding1.code should include("variable reads")
    SlidingN.code should include("variable writes")
  }
}
