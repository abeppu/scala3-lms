package lms.core.examples

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Paths}

class TutorialIndexTest extends AnyFunSuite with Matchers {
  private val tutorialFiles = List(
    "TutorialStart.scala",
    "TutorialOverview.scala",
    "TutorialBasics.scala",
    "TutorialAckermann.scala",
    "TutorialAutomata.scala",
    "TutorialDynVar.scala",
    "TutorialLinq.scala",
    "TutorialShonan.scala",
    "TutorialStencil.scala",
    "TutorialScanner.scala",
    "StagedRegexpMatcher.scala"
  )

  test("index lists Scala 3 tutorial chapters currently available in-tree") {
    tutorialFiles.foreach { name =>
      val path = Paths.get("src/test/scala/lms/core/examples", name)
      Files.exists(path) shouldBe true
    }
  }
}
