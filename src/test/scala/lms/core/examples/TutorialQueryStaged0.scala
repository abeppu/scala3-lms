package lms.core.examples

import lms.core.*
import lms.core.examples.TutorialQueryUnstaged.*
import lms.legacy.compat.SourceContext
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

@virt
trait TutorialQueryStaged0Compiler extends Dsl with TutorialScannerBase {
  type StagedFields = Vector[Rep[String]]

  final case class StagedRecord(fields: StagedFields, schema: Schema) {
    def apply(key: String): Rep[String] = fields(schema.indexOf(key))
    def apply(keys: Schema): StagedFields = keys.map(apply)
  }

  def tablePath(name: String, dynamicPath: Rep[String]): Rep[String] =
    if name == "?" then dynamicPath else name

  def processCSV(filename: Rep[String], schema: Schema, fieldDelimiter: Char, externalSchema: Boolean)(yld: StagedRecord => Rep[Unit])(using SourceContext): Rep[Unit] = {
    val scanner = newScanner(filename)
    val last = schema.last
    def nextRecord: StagedRecord =
      StagedRecord(schema.map(field => scanner.next(if field == last then '\n' else fieldDelimiter)), schema)
    if !externalSchema then {
      val _ = nextRecord
    }
    while scanner.hasNext do yld(nextRecord)
    scanner.close()
  }

  def fieldsEqual(lhs: StagedFields, rhs: StagedFields)(using SourceContext): Rep[Boolean] =
    lhs.zip(rhs).foldLeft(true: Rep[Boolean]) { case (acc, (l, r)) => acc && l == r }

  def printSchema(schema: Schema)(using SourceContext): Rep[Unit] =
    printf(schema.mkString(",") + "\n")

  def printFields(fields: StagedFields)(using SourceContext): Rep[Unit] =
    printf(fields.map(_ => "%s").mkString("", ",", "\n"), fields*)

  def evalPred(pred: Predicate)(record: StagedRecord)(using SourceContext): Rep[Boolean] = pred match {
    case Eq(lhs, rhs) => evalRef(lhs)(record) == evalRef(rhs)(record)
  }

  def evalRef(ref: Ref)(record: StagedRecord): Rep[String] = ref match {
    case Field(name) => record(name)
    case Value(value) => value.toString
  }

  def resultSchema(op: Operator): Schema = op match {
    case Scan(_, schema, _, _) => schema
    case Project(schema, _, _) => schema
    case Filter(_, parent) => resultSchema(parent)
    case Join(left, right) => resultSchema(left) ++ resultSchema(right)
    case HashJoin(left, right) => resultSchema(left) ++ resultSchema(right)
    case Group(keys, agg, _) => keys ++ agg
  }

  def execOp(op: Operator, dynamicPath: Rep[String])(yld: StagedRecord => Rep[Unit])(using SourceContext): Rep[Unit] = op match {
    case Scan(name, schema, fieldDelimiter, externalSchema) =>
      processCSV(tablePath(name, dynamicPath), schema, fieldDelimiter, externalSchema)(yld)
    case Filter(pred, parent) =>
      execOp(parent, dynamicPath) { record =>
        if evalPred(pred)(record) then yld(record) else ()
      }
    case Project(outSchema, inSchema, parent) =>
      execOp(parent, dynamicPath) { record =>
        yld(StagedRecord(record(inSchema), outSchema))
      }
    case Join(left, right) =>
      execOp(left, dynamicPath) { leftRecord =>
        execOp(right, dynamicPath) { rightRecord =>
          val keys = leftRecord.schema.intersect(rightRecord.schema)
          if fieldsEqual(leftRecord(keys), rightRecord(keys)) then
            yld(StagedRecord(leftRecord.fields ++ rightRecord.fields, leftRecord.schema ++ rightRecord.schema))
          else ()
        }
      }
    case HashJoin(_, _) =>
      throw new UnsupportedOperationException("query_staged0 keeps hash joins for the next query compiler phase")
    case Group(_, _, _) =>
      throw new UnsupportedOperationException("query_staged0 keeps grouping for the next query compiler phase")
  }

  def execQuery(op: Operator, dynamicPath: Rep[String])(using SourceContext): Rep[Unit] = {
    printSchema(resultSchema(op))
    execOp(op, dynamicPath)(record => printFields(record.fields))
  }
}

@virt
object TutorialQueryStaged0Snippet extends DslDriver[String, Unit] with Dsl with TutorialScannerExp with TutorialQueryStaged0Compiler { self =>
  override val codegen = new TutorialScalaGenScanner {
    val IR: self.type = self
  }

  private val query =
    Engine(identity).parseSql("select Name from ? schema Name, Value, Flag where Flag='yes'")

  def snippet(path: Rep[String]): Rep[Unit] =
    execQuery(query, path)
}

@virt
object TutorialQueryStaged0JoinSnippet extends DslDriver[String, Unit] with Dsl with TutorialScannerExp with TutorialQueryStaged0Compiler { self =>
  override val codegen = new TutorialScalaGenScanner {
    val IR: self.type = self
  }

  private val query =
    Engine(identity).parseSql("select * from nestedloops ? schema Name, Value, Flag join (select Name as Name1 from ? schema Name, Value, Flag)")

  def snippet(path: Rep[String]): Rep[Unit] =
    execQuery(query, path)
}

class TutorialQueryStaged0Test extends AnyFunSuite with Matchers {
  test("query_staged0 emits scanner-driven Scala for filtered projection") {
    val code = TutorialQueryStaged0Snippet.code
    code should include("new lms.core.examples.TutorialScanner")
    code should include("while")
    code should include(".hasNext")
    code should include(".next(',')")
    code should include("== \"yes\"")
    code should include("printf(\"%s\\n\"")
  }

  test("query_staged0 emits nested loops for tutorial nested-loop joins") {
    val code = TutorialQueryStaged0JoinSnippet.code
    code should include("while")
    code.sliding("while".length).count(_ == "while") should be >= 2
    code should include("Name1")
  }
}
