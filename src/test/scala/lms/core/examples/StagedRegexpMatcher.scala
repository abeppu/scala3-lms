package lms.core.examples

import lms.core.*
import lms.gen.StagingCompile
import lms.legacy.common.{BaseExp, LiftPrimitives, OrderingOpsGen, PrimitiveOpsExpOpt, PrimitiveOpsGen, StringOpsGen, VariablesExp}
import lms.legacy.compat.SourceContext
import lms.legacy.compat.SourceContext.given

import scala.language.implicitConversions


@virt
trait RegexpMatcher extends DslImpl {

  /* search for regexp anywhere in text */
  def matchsearch(regexp: String, text: Rep[String]): Rep[Boolean] = {
    if (regexp.charAt(0) == '^')
      matchhere(regexp, 1, text, 0)
    else {
      def loop(idx: Rep[Int]): Rep[Boolean] = {
        if (ordering_lt(idx, text.length)) {
          val found = matchhere(regexp, 0, text, idx)
          if (found) true else loop(idx + 1)
        } else false
      }
      loop(0)
    }
  }

  /* search for restart of regexp at start of text */
  def matchhere(regexp: String, restart: Int, text: Rep[String], start: Rep[Int]): Rep[Boolean] = {
    if (restart==regexp.length)
      true
    else if (regexp.charAt(restart)=='$' && restart+1==regexp.length)
      start==text.length
    else if (restart+1 < regexp.length && regexp.charAt(restart+1)=='*')
      matchstar(regexp.charAt(restart), regexp, restart+2, text, start)
    else if (ordering_lt(start, text.length) && matchchar(regexp.charAt(restart), text(start)))
      matchhere(regexp, restart+1, text, start+1)
    else false
  }

  /* search for c* followed by restart of regexp at start of text */
  def matchstar(c: Char, regexp: String, restart: Int, text: Rep[String], start: Rep[Int]): Rep[Boolean] = {
    def loop(pos: Rep[Int]): Rep[Boolean] = {
      val found = matchhere(regexp, restart, text, pos)
      if (found) true
      else if (ordering_lt(pos, text.length) && matchchar(c, text(pos))) loop(pos + 1)
      else false
    }
    loop(start)
  }

  def matchchar(c: Char, t: Rep[Char]): Rep[Boolean] = {
    c == '.' || c == t
  }
}


class RegexpMatcherTest extends TutorialFunSuite {

  val under="regex/"

  test("regex1") {
    object Snippet extends DslDriver[String, Boolean] with RegexpMatcher  {
      def snippet(text: Rep[String]): Rep[Boolean] = {
        matchsearch("^hello$", text)
      }
    }
    check("regex1", Snippet.code)
  }

  val regexpMatcherCompiler = new RegexpMatcher with StagingCompile with BaseExp with PrimitiveOpsExpOpt with LiftPrimitives with VariablesExp with PrimitiveOpsGen with StringOpsGen with OrderingOpsGen {
    type API = BaseExp
  }

  private def matchsearchHost(regexp: String, text: String): Boolean = {
    val f: regexpMatcherCompiler.Exp[String] => regexpMatcherCompiler.Exp[Boolean] = regexpMatcherCompiler.matchsearch(regexp, _)
    val searchFn: String => Boolean = regexpMatcherCompiler.compile(f)
    searchFn(text)
  }

  test("matchsearch exact anchors") {
    assert(matchsearchHost("^hello$", "hello"))
    assert(!matchsearchHost("^hello$", "oh hello"))
    assert(!matchsearchHost("^hello$", "hello there"))
  }

  test("matchsearch substring search") {
    assert(matchsearchHost("world", "hello world!"))
    assert(matchsearchHost("lo w", "hello world"))
    assert(!matchsearchHost("world", "WORD"))
  }

  test("matchsearch wildcard star") {
    assert(matchsearchHost("^h.*o$", "hello"))
    assert(matchsearchHost("a.*b", "accb"))
    assert(!matchsearchHost("^h.*o$", "hey there"))
  }

}
