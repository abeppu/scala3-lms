package lms.core

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}

class TutorialUtilsParityTest extends AnyFunSuite with Matchers {
  test("tutorial utility parity helpers support data path, checkOut, and exec") {
    val tmpDir = Files.createTempDirectory("lms-utils-parity")
    try {
      object Suite extends TutorialFunSuite {
        override val under: String = "utils-"
        override val prefix: String = tmpDir.toString + "/"
      }

      Suite.dataFilePath("x.csv") shouldBe "src/data/x.csv"

      val expectedPath = tmpDir.resolve("utils-capture.check.txt")
      Files.writeString(expectedPath, "hello\n")
      Suite.checkOut("capture", "txt", {
        print("hello")
      })
      Files.exists(tmpDir.resolve("utils-capture.actual.txt")) shouldBe false

      Suite.exec("emit", "object X {\nval y = 1\n}\n", "scala")
      val emitted = Files.readString(tmpDir.resolve("utils-emit.actual.scala"))
      emitted should include("object X")
      emitted should include("val y = 1")
    } finally {
      def deleteRec(path: Path): Unit = {
        if (Files.isDirectory(path)) {
          val it = Files.list(path)
          try it.forEach(p => deleteRec(p))
          finally it.close()
        }
        Files.deleteIfExists(path)
      }
      deleteRec(tmpDir)
    }
  }
}
