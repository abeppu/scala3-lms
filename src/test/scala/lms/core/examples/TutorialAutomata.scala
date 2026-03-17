package lms.core.examples

import lms.core.*
import lms.legacy.util.ClosureCompare
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.language.implicitConversions

case class Automaton[I, O](out: O, next: I => Automaton[I, O])

trait TutorialNFAOps extends ClosureCompare {
  type NIO = List[NTrans]

  def guard(cond: CharSet, found: => Boolean = false)(next: => NIO): NIO =
    List(NTrans(cond, () => found, () => next))

  def stop(): NIO = Nil

  case class NTrans(c: CharSet, found: () => Boolean, next: () => NIO) extends Ordered[NTrans] {
    override def compare(that: NTrans): Int = {
      val c0 = c.compare(that.c)
      if (c0 != 0) c0
      else {
        val c1 = found().compare(that.found())
        if (c1 != 0) c1
        else 0
      }
    }
  }

  sealed abstract class CharSet extends Ordered[CharSet] {
    override def compare(that: CharSet): Int = (this, that) match {
      case (Wildcard, Wildcard) => 0
      case (Wildcard, _) => 1
      case (_, Wildcard) => -1
      case (Single(a), Single(b)) => a.compare(b)
    }
  }

  case class Single(c: Char) extends CharSet
  case object Wildcard extends CharSet
}

trait TutorialDFAOps extends Dsl {
  type DfaState = Automaton[Char, Boolean]
  type DIO = Rep[DfaState]

  given dfaTyp: Typ[DfaState]

  def dfaTrans(found: Boolean = false)(next: Rep[Char] => DIO): DIO
}

trait TutorialNFAtoDFA extends TutorialNFAOps with TutorialDFAOps {
  def convertNFAtoDFA(in: (NIO, Boolean)): DIO = {
    def iterate(found: Boolean, state: NIO): DIO =
      dfaTrans(found) { c =>
        exploreNFA(canonicalizeState(state), c) { iterate }
      }
    iterate(in._2, in._1)
  }

  def canonicalizeState(state: NIO): NIO =
    if (state.isEmpty) state
    else {
      val sorted = state.sorted
      sorted.head :: (sorted.zip(sorted.tail).collect { case (a, b) if a.compare(b) != 0 => b })
    }

  def exploreNFA[A: Typ](xs: NIO, cin: Rep[Char])(k: (Boolean, NIO) => Rep[A]): Rep[A] = xs match {
    case Nil =>
      k(false, Nil)
    case NTrans(Wildcard, found, next) :: rest =>
      val (exact, wildcards) = xs.partition(_.c != Wildcard)
      exploreNFA(exact, cin) { (flag, acc) =>
        k(flag || wildcards.exists(_.found()), acc ++ wildcards.flatMap(_.next()))
      }
    case NTrans(cset, found, next) :: rest =>
      val keptIfTaken = for {
        NTrans(otherSet, otherFound, otherNext) <- rest
        narrowed <- knowing(otherSet, cset)
      } yield NTrans(narrowed, otherFound, otherNext)
      val keptIfRejected = for {
        NTrans(otherSet, otherFound, otherNext) <- rest
        narrowed <- knowingNot(otherSet, cset)
      } yield NTrans(narrowed, otherFound, otherNext)
      __ifThenElse(contains(cset, cin),
        exploreNFA(keptIfTaken, cin) { (flag, acc) =>
          k(flag || found(), acc ++ next())
        },
        exploreNFA(keptIfRejected, cin)(k))
  }

  def contains(s: CharSet, c: Rep[Char]): Rep[Boolean] = s match {
    case Single(ch) => c == ch
    case Wildcard => unit(true)
  }

  def knowing(s1: CharSet, s2: CharSet): Option[CharSet] = (s1, s2) match {
    case (Wildcard, _) => Some(Wildcard)
    case (Single(a), Single(b)) if a == b => Some(Wildcard)
    case _ => None
  }

  def knowingNot(s1: CharSet, s2: CharSet): Option[CharSet] = (s1, s2) match {
    case (Single(a), Single(b)) if a == b => None
    case _ => Some(s1)
  }
}

trait TutorialDFAOpsExp extends DslExp with TutorialDFAOps {
  given dfaTyp: Typ[DfaState] = manifestTyp

  case class DfaStateNode(found: Boolean, nextFn: Rep[Char => DfaState]) extends Def[DfaState]

  def dfaTrans(found: Boolean = false)(next: Rep[Char] => DIO): DIO =
    DfaStateNode(found, doLambda[Char, DfaState](next))
}

trait TutorialScalaGenDFAOps extends DslGen {
  val IR: TutorialDFAOpsExp
  import IR.*

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case DfaStateNode(found, nextFn) =>
      emitValDef(sym, s"lms.core.examples.Automaton(${quote(Const(found))}, ${quote(nextFn)})")
    case _ =>
      super.emitNode(sym, rhs)
  }
}

@virt
object TutorialAutomataSnippet extends DslDriver[Unit, Automaton[Char, Boolean]]
    with Dsl
    with TutorialDFAOpsExp
    with TutorialNFAtoDFA { self =>
  override val codegen = new TutorialScalaGenDFAOps {
    val IR: self.type = self
  }

  def snippet(x: Rep[Unit]): Rep[Automaton[Char, Boolean]] = {
    def findAAB(): NIO =
      guard(Single('A')) {
        guard(Single('A')) {
          guard(Single('B'), found = true) {
            stop()
          }
        }
      } ++
      guard(Wildcard) {
        findAAB()
      }

    convertNFAtoDFA((findAAB(), false))
  }
}

class TutorialAutomataTest extends AnyFunSuite with Matchers {
  test("nfa-to-dfa example emits automaton code") {
    TutorialAutomataSnippet.code should include("Automaton(")
    TutorialAutomataSnippet.code should include("Function1")
  }
}
