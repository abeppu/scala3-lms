package lms.core.examples

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.nio.file.Files

object TutorialQueryLive {
  def runDemo(path: String): Vector[Vector[String]] =
    TutorialQueryLiveSteps.parsedQuery(path)
}

class TutorialQueryLiveTest extends AnyFunSuite with Matchers {
  test("query live demo runs through the current parsed query engine") {
    val file = Files.createTempFile("lms-query-live", ".csv")
    Files.writeString(file, "title,time,room\nIntro,09:00 AM,R1\nBreak,10:00 AM,Lobby\n")
    try {
      TutorialQueryLive.runDemo(file.toString) shouldBe Vector(Vector("R1", "Intro"))
    } finally Files.deleteIfExists(file)
  }
}
