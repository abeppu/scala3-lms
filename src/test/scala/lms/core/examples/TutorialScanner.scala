package lms.core.examples

import lms.core.*
import lms.legacy.compat.SourceContext
import lms.legacy.common.{Base, EffectExp}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.io.BufferedReader
import java.io.FileReader
import java.nio.file.Files

import scala.language.implicitConversions

final class TutorialScanner(filename: String) {
  private val reader = new BufferedReader(new FileReader(filename))
  private var pending: String | Null = reader.readLine()

  def next(delim: Char): String = {
    val current = pending.nn
    if (delim == '\n') {
      pending = reader.readLine()
      current
    } else {
      val idx = current.indexOf(delim)
      val field = current.substring(0, idx)
      pending = current.substring(idx + 1)
      field
    }
  }

  def hasNext: Boolean = pending != null

  def close(): Unit = reader.close()
}

trait TutorialScannerBase extends Base { this: Dsl =>
  given scannerTyp: Typ[TutorialScanner]

  final class RepScannerOps(s: Rep[TutorialScanner]) {
    def next(d: Char)(using pos: SourceContext): Rep[String] = scannerNext(s, d)
    def hasNext(using pos: SourceContext): Rep[Boolean] = scannerHasNext(s)
    def close()(using pos: SourceContext): Rep[Unit] = scannerClose(s)
  }

  given Conversion[Rep[TutorialScanner], RepScannerOps] with
    def apply(s: Rep[TutorialScanner]): RepScannerOps = new RepScannerOps(s)

  def newScanner(path: Rep[String])(using pos: SourceContext): Rep[TutorialScanner]
  def scannerNext(s: Rep[TutorialScanner], delim: Char)(using pos: SourceContext): Rep[String]
  def scannerHasNext(s: Rep[TutorialScanner])(using pos: SourceContext): Rep[Boolean]
  def scannerClose(s: Rep[TutorialScanner])(using pos: SourceContext): Rep[Unit]
}

trait TutorialScannerExp extends DslExp with TutorialScannerBase with EffectExp {
  given scannerTyp: Typ[TutorialScanner] = manifestTyp

  case class ScannerNew(path: Exp[String]) extends Def[TutorialScanner]
  case class ScannerNext(scanner: Exp[TutorialScanner], delim: Exp[Char]) extends Def[String]
  case class ScannerHasNext(scanner: Exp[TutorialScanner]) extends Def[Boolean]
  case class ScannerClose(scanner: Exp[TutorialScanner]) extends Def[Unit]

  def newScanner(path: Rep[String])(using pos: SourceContext): Rep[TutorialScanner] =
    reflectMutable(ScannerNew(path))

  def scannerNext(s: Rep[TutorialScanner], delim: Char)(using pos: SourceContext): Rep[String] =
    reflectWrite(s)(ScannerNext(s, unit(delim)))

  def scannerHasNext(s: Rep[TutorialScanner])(using pos: SourceContext): Rep[Boolean] =
    reflectWrite(s)(ScannerHasNext(s))

  def scannerClose(s: Rep[TutorialScanner])(using pos: SourceContext): Rep[Unit] =
    reflectWrite(s)(ScannerClose(s))
}

trait TutorialScalaGenScanner extends DslGen {
  val IR: TutorialScannerExp
  import IR.*

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case ScannerNew(path) =>
      emitValDef(sym, s"new lms.core.examples.TutorialScanner(${quote(path)})")
    case ScannerNext(scanner, delim) =>
      emitValDef(sym, s"${quote(scanner)}.next(${quote(delim)})")
    case ScannerHasNext(scanner) =>
      emitValDef(sym, s"${quote(scanner)}.hasNext")
    case ScannerClose(scanner) =>
      emitValDef(sym, s"${quote(scanner)}.close()")
    case _ =>
      super.emitNode(sym, rhs)
  }
}

@virt
object TutorialScannerSnippet extends DslDriver[String, String] with Dsl with TutorialScannerExp { self =>
  override val codegen = new TutorialScalaGenScanner {
    val IR: self.type = self
  }

  def snippet(path: Rep[String]): Rep[String] = {
    val scanner = newScanner(path)
    val header = scanner.next(',')
    scanner.close()
    header
  }
}

class TutorialScannerTest extends AnyFunSuite with Matchers {
  private def withCsv[A](body: String => A): A = {
    val file = Files.createTempFile("lms-tutorial-scanner", ".csv")
    Files.writeString(file, "Name,Value,Flag\nAlice,1,true\n")
    try body(file.toString)
    finally Files.deleteIfExists(file)
  }

  test("low-level scanner returns the first field") {
    withCsv { path =>
      val scanner = new TutorialScanner(path)
      try scanner.next(',') shouldBe "Name"
      finally scanner.close()
    }
  }

  test("low-level scanner returns the first two fields") {
    withCsv { path =>
      val scanner = new TutorialScanner(path)
      try {
        scanner.next(',') shouldBe "Name"
        scanner.next(',') shouldBe "Value"
      } finally scanner.close()
    }
  }

  test("low-level scanner can read a whole line") {
    withCsv { path =>
      val scanner = new TutorialScanner(path)
      try scanner.next('\n') shouldBe "Name,Value,Flag"
      finally scanner.close()
    }
  }

  test("staged scanner example emits scanner construction and reads") {
    TutorialScannerSnippet.code should include("new lms.core.examples.TutorialScanner(")
    TutorialScannerSnippet.code should include(".next(',')")
    TutorialScannerSnippet.code should include(".close()")
  }
}
