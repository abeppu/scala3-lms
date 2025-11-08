package lms.core.examples

import lms.core.*
import lms.legacy.compat.SourceContext
import lms.legacy.compat.SourceContext.given

import scala.language.implicitConversions


@virt
trait RegexpMatcher extends DslImpl {

  /* search for regexp anywhere in text */
  def matchsearch(regexp: String, text: Rep[String]): Rep[Boolean] = {
    if (regexp(0) == '^')
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
    else if (regexp(restart)=='$' && restart+1==regexp.length)
      start==text.length
    else if (restart+1 < regexp.length && regexp(restart+1)=='*')
      matchstar(regexp(restart), regexp, restart+2, text, start)
    else if (start < text.length && matchchar(regexp(restart), text(start)))
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

}
