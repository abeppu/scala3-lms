package lms.core.examples

import lms.core.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.nio.file.Files

import scala.language.implicitConversions

@virt
object TutorialScannerLibSnippet extends DslDriver[String, String] with Dsl with TutorialScannerExp { self =>
  override val codegen = new TutorialScalaGenScanner {
    val IR: self.type = self
  }

  def snippet(path: Rep[String]): Rep[String] = {
    val scanner = newScanner(path)
    val firstLine = scanner.next('\n')
    val _ = scanner.hasNext
    scanner.next(',')
    val secondFieldFromNextLine = scanner.next(',')
    scanner.close()
    val withSep = string_plus(firstLine, "|")
    string_plus(withSep, secondFieldFromNextLine)
  }
}

class TutorialScannerLibTest extends AnyFunSuite with Matchers {
  private def withCsv(content: String)(body: String => Unit): Unit = {
    val file = Files.createTempFile("lms-tutorial-scannerlib", ".csv")
    Files.writeString(file, content)
    try body(file.toString)
    finally Files.deleteIfExists(file)
  }

  test("scannerlib host scanner supports hasNext/next over multiple lines") {
    withCsv("A,B,C\nx,7,true\n") { path =>
      val scanner = new TutorialScanner(path)
      try {
        scanner.hasNext shouldBe true
        scanner.next('\n') shouldBe "A,B,C"
        scanner.hasNext shouldBe true
        scanner.next(',') shouldBe "x"
        scanner.next(',') shouldBe "7"
        scanner.next('\n') shouldBe "true"
        scanner.hasNext shouldBe false
      } finally scanner.close()
    }
  }

  test("scannerlib host scanner reads whole-line records repeatedly") {
    withCsv("first\nsecond\nthird\n") { path =>
      val scanner = new TutorialScanner(path)
      try {
        scanner.next('\n') shouldBe "first"
        scanner.next('\n') shouldBe "second"
        scanner.next('\n') shouldBe "third"
        scanner.hasNext shouldBe false
      } finally scanner.close()
    }
  }

  test("scannerlib staged snippet emits scanner construction and repeated field reads") {
    val code = TutorialScannerLibSnippet.code
    code should include(".next('\\n')")
    code should include(".next(',')")
    code should include(".close()")
  }
}
