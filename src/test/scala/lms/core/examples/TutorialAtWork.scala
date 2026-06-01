package lms.core.examples

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

object TutorialAtWork {
  final case class Example(name: String, file: String)

  val examples: Vector[Example] = Vector(
    Example("query compiler", "TutorialQuery.scala"),
    Example("LINQ normalization", "TutorialLinq.scala"),
    Example("stencil optimization", "TutorialStencil.scala"),
    Example("FFT specialization", "FFT.scala"),
    Example("matrix-vector staging", "TutorialShonan.scala")
  )
}

class TutorialAtWorkTest extends AnyFunSuite with Matchers {
  test("at-work chapter catalog points at active application-scale examples") {
    TutorialAtWork.examples.map(_.name) should contain allOf (
      "query compiler",
      "LINQ normalization",
      "stencil optimization",
      "FFT specialization",
      "matrix-vector staging"
    )
  }
}
