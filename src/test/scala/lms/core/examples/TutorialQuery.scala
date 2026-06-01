package lms.core.examples

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

object TutorialQuery {
  final case class Chapter(step: String, file: String, description: String)

  val chapters: Vector[Chapter] = Vector(
    Chapter("AST and interpreter", "TutorialQueryUnstaged.scala", "plain SQL-like relational algebra parser and interpreter"),
    Chapter("Scala staging", "TutorialQueryStaged0.scala", "Scala source generation for scans, filters, projections, and joins"),
    Chapter("C staging", "TutorialQueryOptC.scala", "C source generation and executable smoke tests over scanner lowering")
  )
}

class TutorialQueryTest extends AnyFunSuite with Matchers {
  test("query tutorial catalog tracks the Scala 3 query chapter split") {
    TutorialQuery.chapters.map(_.file) shouldBe Vector(
      "TutorialQueryUnstaged.scala",
      "TutorialQueryStaged0.scala",
      "TutorialQueryOptC.scala"
    )
    TutorialQuery.chapters.map(_.step) should contain allOf ("AST and interpreter", "Scala staging", "C staging")
  }
}
