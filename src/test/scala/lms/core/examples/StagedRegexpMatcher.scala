package lms.core.examples

import lms.core.*
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
      var start = -1
      var found = false
      while (!found && start < text.length) {
        start += 1
        found = matchhere(regexp, 0, text, start)
      }
      found
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
    else if (start < text.length && matchchar(regexp.charAt(restart), text(start)))
      matchhere(regexp, restart+1, text, start+1)
    else false
  }

  /* search for c* followed by restart of regexp at start of text */
  def matchstar(c: Char, regexp: String, restart: Int, text: Rep[String], start: Rep[Int]): Rep[Boolean] = {
    var sstart = start
    var found = matchhere(regexp, restart, text, sstart)
    var failed = false
    while (!failed && !found && sstart < text.length) {
      failed = !matchchar(c, text(sstart))
      sstart += 1
      found = matchhere(regexp, restart, text, sstart)
    }
    !failed && found
  }

  def matchchar(c: Char, t: Rep[Char]): Rep[Boolean] = {
    c == '.' || c == t
  }
}

object RegexpMatcher extends RegexpMatcher


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

  private def matchsearchHost(regexp: String, text: String): Boolean = {
    def matchchar(c: Char, t: Char): Boolean =
      c == '.' || c == t

    def matchstar(c: Char, restart: Int, start: Int): Boolean = {
      var sstart = start
      var found = matchhere(restart, sstart)
      var failed = false
      while (!failed && !found && sstart < text.length) {
        failed = !matchchar(c, text.charAt(sstart))
        sstart += 1
        if (!failed) found = matchhere(restart, sstart)
      }
      !failed && found
    }

    def matchhere(restart: Int, start: Int): Boolean = {
      if (restart == regexp.length) true
      else if (regexp.charAt(restart) == '$' && restart + 1 == regexp.length)
        start == text.length
      else if (restart + 1 < regexp.length && regexp.charAt(restart + 1) == '*')
        matchstar(regexp.charAt(restart), restart + 2, start)
      else if (start < text.length && matchchar(regexp.charAt(restart), text.charAt(start)))
        matchhere(restart + 1, start + 1)
      else false
    }

    if (regexp.nonEmpty && regexp.head == '^')
      matchhere(1, 0)
    else {
      var idx = 0
      var found = false
      while (!found && idx <= text.length) {
        found = matchhere(0, idx)
        idx += 1
      }
      found
    }
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
