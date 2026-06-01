package lms.core.examples

import lms.core.*
import lms.core.examples.TutorialQueryUnstaged.*
import lms.legacy.compat.SourceContext
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.nio.file.Files

trait TutorialQueryOptCCompiler extends Dsl with TutorialScannerLowerBase {
  sealed trait CField {
    def printField()(using SourceContext): Rep[Unit]
    def compare(other: CField)(using SourceContext): Rep[Boolean]
  }

  final case class CStringField(data: Rep[String], len: Rep[Int]) extends CField {
    def printField()(using SourceContext): Rep[Unit] = {
      prints(data)
      ()
    }

    def compare(other: CField)(using SourceContext): Rep[Boolean] = other match {
      case CStringField(otherData, otherLen) =>
        uncheckedPure[Boolean]("strncmp(", data, ", ", otherData, ", ", len, ") == 0 && ", len, " == ", otherLen)
      case CIntField(_) =>
        unit(false)
    }
  }

  final case class CIntField(value: Rep[Int]) extends CField {
    def printField()(using SourceContext): Rep[Unit] =
      printf("%d", value)

    def compare(other: CField)(using SourceContext): Rep[Boolean] = other match {
      case CIntField(otherValue) =>
        __equal(value, otherValue)
      case CStringField(_, _) =>
        unit(false)
    }
  }

  type CFields = Vector[CField]

  final case class CRecord(fields: CFields, schema: Schema) {
    def apply(key: String): CField = fields(schema.indexOf(key))
    def apply(keys: Schema): CFields = keys.map(apply)
  }

  final class CScanner(path: Rep[String]) {
    private val fd = open(path)
    private val len = filelen(fd)
    private val data = mmap[Char](fd, len)
    private val pos = var_new(unit(0))

    def next(delim: Char)(using SourceContext): CStringField = {
      val start = readVar(pos)
      __whileDo(notequals(data(readVar(pos)), unit(delim)), {
        var_assign(pos, int_plus(readVar(pos), unit(1)))
      })
      val fieldLen = int_minus(readVar(pos), start)
      var_assign(pos, int_plus(readVar(pos), unit(1)))
      CStringField(stringFromCharArray(data, start, fieldLen), fieldLen)
    }

    def nextInt(delim: Char)(using SourceContext): CIntField = {
      val value = var_new(unit(0))
      __whileDo(notequals(data(readVar(pos)), unit(delim)), {
        val digit = uncheckedPure[Int](data(readVar(pos)), " - '0'")
        var_assign(value, int_plus(int_times(readVar(value), unit(10)), digit))
        var_assign(pos, int_plus(readVar(pos), unit(1)))
      })
      var_assign(pos, int_plus(readVar(pos), unit(1)))
      CIntField(readVar(value))
    }

    def hasNext(using SourceContext): Rep[Boolean] =
      ordering_lt(readVar(pos), len)

    def done()(using SourceContext): Rep[Unit] =
      closeFd(fd)
  }

  def tablePath(name: String, dynamicPath: Rep[String]): Rep[String] =
    if name == "?" then dynamicPath else name

  def isNumericCol(name: String): Boolean =
    name.startsWith("#")

  def emitIf(cond: Rep[Boolean])(body: => Rep[Unit])(using SourceContext): Rep[Unit] = {
    unchecked[Unit]("if (", cond, ") {")
    body
    unchecked[Unit]("}")
  }

  def processCSV(filename: Rep[String], schema: Schema, fieldDelimiter: Char, externalSchema: Boolean)(yld: CRecord => Rep[Unit])(using SourceContext): Rep[Unit] = {
    val scanner = CScanner(filename)
    val last = schema.last
    def nextRecord: CRecord =
      CRecord(schema.map { field =>
        val delim = if field == last then '\n' else fieldDelimiter
        if isNumericCol(field) then scanner.nextInt(delim) else scanner.next(delim)
      }, schema)
    if !externalSchema then {
      val _ = nextRecord
    }
    __whileDo(scanner.hasNext, {
      val record = nextRecord
      yld(record)
    })
    scanner.done()
  }

  def printFields(fields: CFields)(using SourceContext): Rep[Unit] = {
    if fields.nonEmpty then {
      fields.head.printField()
      fields.tail.foreach { field =>
        printf(",")
        field.printField()
      }
    }
    printf("\n")
  }

  def evalPred(pred: Predicate)(record: CRecord)(using SourceContext): Rep[Boolean] = pred match {
    case Eq(lhs, rhs) => evalRef(lhs)(record).compare(evalRef(rhs)(record))
  }

  def evalRef(ref: Ref)(record: CRecord)(using SourceContext): CField = ref match {
    case Field(name) => record(name)
    case Value(value: Int) => CIntField(unit(value))
    case Value(value) =>
      val text = value.toString
      CStringField(unit(text), unit(text.length))
  }

  def fieldsEqual(lhs: CFields, rhs: CFields)(using SourceContext): Rep[Boolean] =
    lhs.zip(rhs).foldLeft(unit(true)) { case (acc, (left, right)) =>
      boolean_and(acc, left.compare(right))
    }

  def resultSchema(op: Operator): Schema = op match {
    case Scan(_, schema, _, _) => schema
    case Project(schema, _, _) => schema
    case Filter(_, parent) => resultSchema(parent)
    case Join(left, right) => resultSchema(left) ++ resultSchema(right)
    case HashJoin(left, right) => resultSchema(left) ++ resultSchema(right)
    case Group(keys, agg, _) => keys ++ agg
  }

  def execOp(op: Operator, dynamicPath: Rep[String])(yld: CRecord => Rep[Unit])(using SourceContext): Rep[Unit] = op match {
    case Scan(name, schema, fieldDelimiter, externalSchema) =>
      processCSV(tablePath(name, dynamicPath), schema, fieldDelimiter, externalSchema)(yld)
    case Project(outSchema, inSchema, parent) =>
      execOp(parent, dynamicPath) { record =>
        yld(CRecord(record(inSchema), outSchema))
      }
    case Filter(pred, parent) =>
      execOp(parent, dynamicPath) { record =>
        emitIf(evalPred(pred)(record)) {
          yld(record)
        }
      }
    case Join(left, right) =>
      execNestedJoin(left, right, dynamicPath)(yld)
    case HashJoin(left, right) =>
      execNestedJoin(left, right, dynamicPath)(yld)
    case Group(keys, agg, parent) =>
      execSingleStringKeyNumericGroup(keys, agg, parent, dynamicPath)(yld)
  }

  private def execNestedJoin(left: Operator, right: Operator, dynamicPath: Rep[String])(yld: CRecord => Rep[Unit])(using SourceContext): Rep[Unit] =
    execOp(left, dynamicPath) { leftRecord =>
      execOp(right, dynamicPath) { rightRecord =>
        val keys = leftRecord.schema.intersect(rightRecord.schema)
        emitIf(fieldsEqual(leftRecord(keys), rightRecord(keys))) {
          yld(CRecord(leftRecord.fields ++ rightRecord.fields, leftRecord.schema ++ rightRecord.schema))
        }
      }
    }

  private def execSingleStringKeyNumericGroup(keys: Schema, agg: Schema, parent: Operator, dynamicPath: Rep[String])(yld: CRecord => Rep[Unit])(using SourceContext): Rep[Unit] = {
    if keys.size != 1 || agg.size != 1 then
      throw new UnsupportedOperationException("query_optc group fallback currently supports one key and one aggregate")

    val capacity = unit(256)
    val keyData = NewArray[String](capacity)
    val keyLen = NewArray[Int](capacity)
    val sums = NewArray[Int](capacity)
    val count = var_new(unit(0))

    execOp(parent, dynamicPath) { record =>
      val key = record(keys.head).asInstanceOf[CStringField]
      val value = record(agg.head).asInstanceOf[CIntField]
      val found = var_new(unit(-1))
      val i = var_new(unit(0))
      __whileDo(boolean_and(ordering_lt(readVar(i), readVar(count)), __equal(readVar(found), unit(-1))), {
        emitIf(CStringField(keyData(readVar(i)), keyLen(readVar(i))).compare(key)) {
          var_assign(found, readVar(i))
        }
        var_assign(i, int_plus(readVar(i), unit(1)))
      })
      emitIf(__equal(readVar(found), unit(-1))) {
        val slot = readVar(count)
        keyData(slot) = key.data
        keyLen(slot) = key.len
        sums(slot) = value.value
        var_assign(count, int_plus(readVar(count), unit(1)))
      }
      emitIf(notequals(readVar(found), unit(-1))) {
        val slot = readVar(found)
        sums(slot) = int_plus(sums(slot), value.value)
      }
    }

    val out = var_new(unit(0))
    __whileDo(ordering_lt(readVar(out), readVar(count)), {
      val slot = readVar(out)
      yld(CRecord(Vector(CStringField(keyData(slot), keyLen(slot)), CIntField(sums(slot))), keys ++ agg))
      var_assign(out, int_plus(readVar(out), unit(1)))
    })
  }

  def execQuery(op: Operator, dynamicPath: Rep[String])(using SourceContext): Rep[Unit] =
    execOp(op, dynamicPath)(record => printFields(record.fields))
}

object TutorialQueryOptCSnippet extends TutorialDslDriverC[String, Unit] with Dsl with TutorialScannerLowerExp with TutorialQueryOptCCompiler { self =>
  override val codegen = new TutorialDslGenC with TutorialCGenScannerLower {
    val IR: self.type = self
  }

  private val query =
    Engine(identity).parseSql("select Name from ? schema Name, Value, Flag")

  def snippet(path: Rep[String]): Rep[Unit] =
    execQuery(query, path)
}

object TutorialQueryOptCNumericSnippet extends TutorialDslDriverC[String, Unit] with Dsl with TutorialScannerLowerExp with TutorialQueryOptCCompiler { self =>
  override val codegen = new TutorialDslGenC with TutorialCGenScannerLower {
    val IR: self.type = self
  }

  private val query =
    Project(
      Vector("Name", "#Value"),
      Vector("Name", "#Value"),
      Scan("?", Vector("Name", "#Value", "Flag"), ',', externalSchema = true)
    )

  def snippet(path: Rep[String]): Rep[Unit] =
    execQuery(query, path)
}

object TutorialQueryOptCStringFilterSnippet extends TutorialDslDriverC[String, Unit] with Dsl with TutorialScannerLowerExp with TutorialQueryOptCCompiler { self =>
  override val codegen = new TutorialDslGenC with TutorialCGenScannerLower {
    val IR: self.type = self
  }

  private val query =
    Project(
      Vector("Name"),
      Vector("Name"),
      Filter(
        Eq(Field("Flag"), Value("yes")),
        Scan("?", Vector("Name", "Value", "Flag"), ',', externalSchema = true)
      )
    )

  def snippet(path: Rep[String]): Rep[Unit] =
    execQuery(query, path)
}

object TutorialQueryOptCNumericFilterSnippet extends TutorialDslDriverC[String, Unit] with Dsl with TutorialScannerLowerExp with TutorialQueryOptCCompiler { self =>
  override val codegen = new TutorialDslGenC with TutorialCGenScannerLower {
    val IR: self.type = self
  }

  private val query =
    Project(
      Vector("Name", "#Value"),
      Vector("Name", "#Value"),
      Filter(
        Eq(Field("#Value"), Value(2)),
        Scan("?", Vector("Name", "#Value", "Flag"), ',', externalSchema = true)
      )
    )

  def snippet(path: Rep[String]): Rep[Unit] =
    execQuery(query, path)
}

object TutorialQueryOptCJoinSnippet extends TutorialDslDriverC[String, Unit] with Dsl with TutorialScannerLowerExp with TutorialQueryOptCCompiler { self =>
  override val codegen = new TutorialDslGenC with TutorialCGenScannerLower {
    val IR: self.type = self
  }

  private val schema = Vector("Name", "#Value", "Flag")
  private val query =
    Join(
      Scan("?", schema, ',', externalSchema = true),
      Project(Vector("Name"), Vector("Name"), Scan("?", schema, ',', externalSchema = true))
    )

  def snippet(path: Rep[String]): Rep[Unit] =
    execQuery(query, path)
}

object TutorialQueryOptCHashJoinFallbackSnippet extends TutorialDslDriverC[String, Unit] with Dsl with TutorialScannerLowerExp with TutorialQueryOptCCompiler { self =>
  override val codegen = new TutorialDslGenC with TutorialCGenScannerLower {
    val IR: self.type = self
  }

  private val schema = Vector("Name", "#Value", "Flag")
  private val query =
    HashJoin(
      Scan("?", schema, ',', externalSchema = true),
      Project(Vector("Name"), Vector("Name"), Scan("?", schema, ',', externalSchema = true))
    )

  def snippet(path: Rep[String]): Rep[Unit] =
    execQuery(query, path)
}

object TutorialQueryOptCGroupSnippet extends TutorialDslDriverC[String, Unit] with Dsl with TutorialScannerLowerExp with TutorialQueryOptCCompiler { self =>
  override val codegen = new TutorialDslGenC with TutorialCGenScannerLower {
    val IR: self.type = self
  }

  private val query =
    Group(
      Vector("Name"),
      Vector("#Value"),
      Scan("?", Vector("Name", "#Value", "Flag"), ',', externalSchema = true)
    )

  def snippet(path: Rep[String]): Rep[Unit] =
    execQuery(query, path)
}

class TutorialQueryOptCTest extends AnyFunSuite with Matchers {
  import TutorialCBackendSupport.compileAndRun

  private val runtimePrefix =
    """#include <fcntl.h>
      |#include <stdint.h>
      |#include <stdio.h>
      |#include <stdlib.h>
      |#include <string.h>
      |#include <sys/mman.h>
      |#include <sys/stat.h>
      |#include <unistd.h>
      |#ifndef MAP_FILE
      |#define MAP_FILE 0
      |#endif
      |static int fsize(int fd) {
      |  struct stat st;
      |  fstat(fd, &st);
      |  return (int)st.st_size;
      |}
      |static char *slice_string(char *data, int pos, int len) {
      |  char *out = (char *)malloc((size_t)len + 1);
      |  memcpy(out, data + pos, (size_t)len);
      |  out[len] = '\0';
      |  return out;
      |}
      |static int printll(char *s) {
      |  return printf("%s", s);
      |}
      |""".stripMargin

  private def cString(value: String): String =
    "\"" + value.flatMap {
      case '\\' => "\\\\"
      case '"' => "\\\""
      case '\n' => "\\n"
      case '\t' => "\\t"
      case ch => ch.toString
    } + "\""

  private def withCsv[A](contents: String)(body: String => A): A = {
    val file = Files.createTempFile("lms-query-optc", ".csv")
    Files.writeString(file, contents)
    try body(file.toString)
    finally Files.deleteIfExists(file)
  }

  test("query_optc initial slice emits C source over scanner lowering") {
    val code = TutorialQueryOptCSnippet.cSource
    code should include("open(")
    code should include("fsize(")
    code should include("mmap(0,")
    code should include("for (;;)")
    code should include("!=")
    code should include("printll(")
    code should include("close(")
  }

  test("query_optc initial slice compiles and runs string projection") {
    withCsv("Alice,1,yes\nBob,2,no\n") { path =>
      val output = compileAndRun(
        TutorialQueryOptCSnippet.cSource,
        s"""int main() {
           |  snippet(${cString(path)});
           |  return 0;
           |}
           |""".stripMargin,
        runtimePrefix
      )
      output shouldBe "Alice\nBob"
    }
  }

  test("query_optc initial slice emits C source for numeric fields") {
    val code = TutorialQueryOptCNumericSnippet.cSource
    code should include(" - '0'")
    code should include(" * 10")
    code should include("!=")
    code should include("printll(")
    code should include("printf(\"%d\"")
  }

  test("query_optc initial slice compiles and runs numeric projection") {
    withCsv("Alice,1,yes\nBob,2,no\n") { path =>
      val output = compileAndRun(
        TutorialQueryOptCNumericSnippet.cSource,
        s"""int main() {
           |  snippet(${cString(path)});
           |  return 0;
           |}
           |""".stripMargin,
        runtimePrefix
      )
      output shouldBe "Alice,1\nBob,2"
    }
  }

  test("query_optc initial slice emits C source for string filters") {
    val code = TutorialQueryOptCStringFilterSnippet.cSource
    code should include("strncmp(")
    code should include("\"yes\"")
    code should include("if (")
    code should include("printll(")
  }

  test("query_optc initial slice compiles and runs string filters") {
    withCsv("Alice,1,yes\nBob,2,no\nEve,3,yes\n") { path =>
      val output = compileAndRun(
        TutorialQueryOptCStringFilterSnippet.cSource,
        s"""int main() {
           |  snippet(${cString(path)});
           |  return 0;
           |}
           |""".stripMargin,
        runtimePrefix
      )
      output shouldBe "Alice\nEve"
    }
  }

  test("query_optc initial slice emits C source for numeric filters") {
    val code = TutorialQueryOptCNumericFilterSnippet.cSource
    code should include("== 2")
    code should include("if (")
    code should include("printf(\"%d\"")
  }

  test("query_optc initial slice compiles and runs numeric filters") {
    withCsv("Alice,1,yes\nBob,2,no\nEve,2,yes\n") { path =>
      val output = compileAndRun(
        TutorialQueryOptCNumericFilterSnippet.cSource,
        s"""int main() {
           |  snippet(${cString(path)});
           |  return 0;
           |}
           |""".stripMargin,
        runtimePrefix
      )
      output shouldBe "Bob,2\nEve,2"
    }
  }

  test("query_optc initial slice emits C source for nested-loop joins") {
    val code = TutorialQueryOptCJoinSnippet.cSource
    code should include("open(")
    code should include("strncmp(")
    code should include("if (")
  }

  test("query_optc initial slice compiles and runs nested-loop joins") {
    withCsv("Alice,1,yes\nBob,2,no\n") { path =>
      val output = compileAndRun(
        TutorialQueryOptCJoinSnippet.cSource,
        s"""int main() {
           |  snippet(${cString(path)});
           |  return 0;
           |}
           |""".stripMargin,
        runtimePrefix
      )
      output shouldBe "Alice,1,yes,Alice\nBob,2,no,Bob"
    }
  }

  test("query_optc accepts hash joins through nested-loop fallback") {
    withCsv("Alice,1,yes\nBob,2,no\n") { path =>
      val output = compileAndRun(
        TutorialQueryOptCHashJoinFallbackSnippet.cSource,
        s"""int main() {
           |  snippet(${cString(path)});
           |  return 0;
           |}
           |""".stripMargin,
        runtimePrefix
      )
      output shouldBe "Alice,1,yes,Alice\nBob,2,no,Bob"
    }
  }

  test("query_optc group fallback emits C source for one string key and numeric sum") {
    val code = TutorialQueryOptCGroupSnippet.cSource
    code should include("calloc")
    code should include("strncmp(")
    code should include("printf(\"%d\"")
  }

  test("query_optc group fallback compiles and runs one string key and numeric sum") {
    withCsv("Alice,1,yes\nBob,2,no\nAlice,3,yes\n") { path =>
      val output = compileAndRun(
        TutorialQueryOptCGroupSnippet.cSource,
        s"""int main() {
           |  snippet(${cString(path)});
           |  return 0;
           |}
           |""".stripMargin,
        runtimePrefix
      )
      output shouldBe "Alice,4\nBob,2"
    }
  }
}
