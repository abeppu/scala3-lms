package lms.core.examples

import lms.core.*
import lms.core.examples.TutorialQueryUnstaged.*
import lms.legacy.compat.SourceContext
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

trait TutorialQueryOptCCompiler extends Dsl with TutorialScannerLowerBase {
  sealed trait CField {
    def printField()(using SourceContext): Rep[Unit]
  }

  final case class CStringField(data: Rep[String], len: Rep[Int]) extends CField {
    def printField()(using SourceContext): Rep[Unit] = {
      prints(data)
      ()
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
      __whileDo(data(readVar(pos)) != unit(delim), {
        var_assign(pos, int_plus(readVar(pos), unit(1)))
      })
      val fieldLen = int_minus(readVar(pos), start)
      var_assign(pos, int_plus(readVar(pos), unit(1)))
      CStringField(stringFromCharArray(data, start, fieldLen), fieldLen)
    }

    def hasNext(using SourceContext): Rep[Boolean] =
      ordering_lt(readVar(pos), len)

    def done()(using SourceContext): Rep[Unit] =
      closeFd(fd)
  }

  def tablePath(name: String, dynamicPath: Rep[String]): Rep[String] =
    if name == "?" then dynamicPath else name

  def processCSV(filename: Rep[String], schema: Schema, fieldDelimiter: Char, externalSchema: Boolean)(yld: CRecord => Rep[Unit])(using SourceContext): Rep[Unit] = {
    val scanner = CScanner(filename)
    val last = schema.last
    def nextRecord: CRecord =
      CRecord(schema.map(field => scanner.next(if field == last then '\n' else fieldDelimiter)), schema)
    if !externalSchema then {
      val _ = nextRecord
    }
    __whileDo(scanner.hasNext, {
      yld(nextRecord)
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
    case other =>
      throw new UnsupportedOperationException(s"query_optc initial C slice does not yet support $other")
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

class TutorialQueryOptCTest extends AnyFunSuite with Matchers {
  test("query_optc initial slice emits C source over scanner lowering") {
    val code = TutorialQueryOptCSnippet.cSource
    code should include("open(")
    code should include("fsize(")
    code should include("mmap(0,")
    code should include("for (;;)")
    code should include("printll(")
    code should include("close(")
  }
}
