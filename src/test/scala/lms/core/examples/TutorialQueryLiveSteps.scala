package lms.core.examples

import lms.core.examples.TutorialQueryUnstaged.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.nio.file.Files

object TutorialQueryLiveSteps {
  def handWrittenFilter(path: String): Vector[Vector[String]] = {
    val out = Vector.newBuilder[Vector[String]]
    val scanner = new TutorialScanner(path)
    try {
      scanner.next('\n')
      while scanner.hasNext do {
        val title = scanner.next(',')
        val time = scanner.next(',')
        val room = scanner.next('\n')
        if time == "09:00 AM" then out += Vector(room, title)
      }
    } finally scanner.close()
    out.result()
  }

  def schemaDrivenFilter(path: String): Vector[Vector[String]] = {
    val out = Vector.newBuilder[Vector[String]]
    val scanner = new TutorialScanner(path)
    try {
      val schema = scanner.next('\n').split(',').toVector
      while scanner.hasNext do {
        val fields = schema.map(field => scanner.next(if field == schema.last then '\n' else ','))
        val record = schema.zip(fields).toMap
        if record("time") == "09:00 AM" then out += Vector(record("room"), record("title"))
      }
    } finally scanner.close()
    out.result()
  }

  def parsedQuery(path: String): Vector[Vector[String]] = {
    val engine = Engine(identity)
    engine.runSql(s"select room,title from $path where time='09:00 AM'")
  }
}

class TutorialQueryLiveStepsTest extends AnyFunSuite with Matchers {
  private def withTalks[A](body: String => A): A = {
    val file = Files.createTempFile("lms-query-live-talks", ".csv")
    Files.writeString(file, "title,time,room\nIntro,09:00 AM,R1\nBreak,10:00 AM,Lobby\nStaging,09:00 AM,R2\n")
    try body(file.toString)
    finally Files.deleteIfExists(file)
  }

  test("query live steps evolve from hand scanner to schema-driven processor") {
    withTalks { path =>
      TutorialQueryLiveSteps.handWrittenFilter(path) shouldBe Vector(Vector("R1", "Intro"), Vector("R2", "Staging"))
      TutorialQueryLiveSteps.schemaDrivenFilter(path) shouldBe Vector(Vector("R1", "Intro"), Vector("R2", "Staging"))
    }
  }

  test("query live steps reach the parsed query engine") {
    withTalks { path =>
      TutorialQueryLiveSteps.parsedQuery(path) shouldBe Vector(Vector("R1", "Intro"), Vector("R2", "Staging"))
    }
  }
}
