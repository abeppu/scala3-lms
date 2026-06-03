package lms.core.examples

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

object TutorialCompiler {
  final case class Topic(name: String, representativeTest: String)

  val topics: Vector[Topic] = Vector(
    Topic("IR-backed staging", "TutorialDslApiTest"),
    Topic("effect scheduling and mutable state", "StagingCompileVarTest"),
    Topic("generated Scala code", "TutorialQueryStaged0Test"),
    Topic("generated C code", "TutorialCBackendTest")
  )
}

class TutorialCompilerTest extends AnyFunSuite with Matchers {
  test("compiler chapter catalog points at active Scala 3 IR and codegen coverage") {
    TutorialCompiler.topics.map(_.name) should contain allOf (
      "IR-backed staging",
      "effect scheduling and mutable state",
      "generated Scala code",
      "generated C code"
    )
  }
}
