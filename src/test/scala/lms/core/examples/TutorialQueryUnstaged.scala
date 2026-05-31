package lms.core.examples

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.nio.file.Files
import scala.collection.mutable

object TutorialQueryUnstaged {
  type Schema = Vector[String]
  type Fields = Vector[String]

  sealed trait Operator
  final case class Scan(name: String, schema: Schema, delim: Char, externalSchema: Boolean) extends Operator
  final case class Project(outSchema: Schema, inSchema: Schema, parent: Operator) extends Operator
  final case class Filter(pred: Predicate, parent: Operator) extends Operator
  final case class Join(left: Operator, right: Operator) extends Operator
  final case class HashJoin(left: Operator, right: Operator) extends Operator
  final case class Group(keys: Schema, agg: Schema, parent: Operator) extends Operator

  sealed trait Predicate
  final case class Eq(lhs: Ref, rhs: Ref) extends Predicate

  sealed trait Ref
  final case class Field(name: String) extends Ref
  final case class Value(value: Any) extends Ref

  final case class Record(fields: Fields, schema: Schema) {
    def apply(key: String): String = fields(schema.indexOf(key))
    def apply(keys: Schema): Fields = keys.map(apply)
  }

  final class Engine(resolveTable: String => String) {
    val defaultFieldDelimiter = ','

    def parseSql(input: String): Operator =
      new Parser(tokenize(input)).parseQuery()

    def scan(table: String, schema: Option[Schema] = None, delim: Option[Char] = None): Scan = {
      val path = resolveTable(table)
      val (actualSchema, externalSchema) = schema match {
        case Some(value) => (value, true)
        case None => (loadSchema(path), false)
      }
      Scan(path, actualSchema, delim.getOrElse(defaultFieldDelimiter), externalSchema)
    }

    def runSql(input: String): Vector[Fields] =
      run(parseSql(input))

    def run(op: Operator): Vector[Fields] = {
      val out = Vector.newBuilder[Fields]
      execOp(op)(rec => out += rec.fields)
      out.result()
    }

    def resultSchema(op: Operator): Schema = op match {
      case Scan(_, schema, _, _) => schema
      case Project(schema, _, _) => schema
      case Filter(_, parent) => resultSchema(parent)
      case Join(left, right) => resultSchema(left) ++ resultSchema(right)
      case HashJoin(left, right) => resultSchema(left) ++ resultSchema(right)
      case Group(keys, agg, _) => keys ++ agg
    }

    private def loadSchema(path: String): Schema = {
      val scanner = new TutorialScanner(path)
      try scanner.next('\n').split(defaultFieldDelimiter).toVector
      finally scanner.close()
    }

    private def processCSV(path: String, schema: Schema, fieldDelimiter: Char, externalSchema: Boolean)(yld: Record => Unit): Unit = {
      val scanner = new TutorialScanner(path)
      val last = schema.last
      def nextRecord: Record =
        Record(schema.map(field => scanner.next(if field == last then '\n' else fieldDelimiter)), schema)
      try {
        if !externalSchema then nextRecord
        while scanner.hasNext do yld(nextRecord)
      } finally scanner.close()
    }

    private def evalPred(pred: Predicate)(record: Record): Boolean = pred match {
      case Eq(lhs, rhs) => evalRef(lhs)(record) == evalRef(rhs)(record)
    }

    private def evalRef(ref: Ref)(record: Record): String = ref match {
      case Field(name) => record(name)
      case Value(value) => value.toString
    }

    private def execOp(op: Operator)(yld: Record => Unit): Unit = op match {
      case Scan(path, schema, fieldDelimiter, externalSchema) =>
        processCSV(path, schema, fieldDelimiter, externalSchema)(yld)
      case Filter(pred, parent) =>
        execOp(parent)(record => if evalPred(pred)(record) then yld(record))
      case Project(outSchema, inSchema, parent) =>
        execOp(parent)(record => yld(Record(record(inSchema), outSchema)))
      case Join(left, right) =>
        execOp(left) { leftRecord =>
          execOp(right) { rightRecord =>
            val keys = leftRecord.schema.intersect(rightRecord.schema)
            if leftRecord(keys) == rightRecord(keys) then
              yld(Record(leftRecord.fields ++ rightRecord.fields, leftRecord.schema ++ rightRecord.schema))
          }
        }
      case HashJoin(left, right) =>
        val keys = resultSchema(left).intersect(resultSchema(right))
        val index = mutable.HashMap.empty[Fields, mutable.ArrayBuffer[Record]]
        execOp(left) { leftRecord =>
          index.getOrElseUpdate(leftRecord(keys), mutable.ArrayBuffer.empty) += leftRecord
        }
        execOp(right) { rightRecord =>
          index.get(rightRecord(keys)).foreach(_.foreach { leftRecord =>
            yld(Record(leftRecord.fields ++ rightRecord.fields, leftRecord.schema ++ rightRecord.schema))
          })
        }
      case Group(keys, agg, parent) =>
        val sumsByKey = mutable.HashMap.empty[Fields, Seq[Int]]
        execOp(parent) { record =>
          val keyValues = record(keys)
          val previous = sumsByKey.getOrElseUpdate(keyValues, agg.map(_ => 0))
          sumsByKey(keyValues) = previous.zip(record(agg).map(_.toInt)).map(_ + _)
        }
        sumsByKey.foreach { case (keyValues, sums) =>
          yld(Record(keyValues ++ sums.map(_.toString), keys ++ agg))
        }
    }

    private final class Parser(tokens: Vector[String]) {
      private var index = 0

      def parseQuery(): Operator = {
        val op = parseSelect()
        expectEnd()
        op
      }

      private def parseSelect(): Operator = {
        expect("select")
        val project = parseSelectClause()
        expect("from")
        val from = parseJoinClause()
        val filtered =
          if accept("where") then Filter(parsePredicate(), from)
          else from
        val grouped =
          if accept("group") then {
            expect("by")
            val keys = parseFieldList()
            expect("sum")
            Group(keys, parseFieldList(), filtered)
          } else filtered
        project(grouped)
      }

      private def parseSelectClause(): Operator => Operator =
        if accept("*") then identity
        else {
          val (out, in) = parseAliasedFieldList()
          parent => Project(out, in, parent)
        }

      private def parseJoinClause(): Operator = {
        val nestedLoops = accept("nestedloops")
        val first = parseTableClause()
        val tables = mutable.ArrayBuffer(first)
        while accept("join") do tables += parseTableClause()
        if nestedLoops then tables.reduceLeft(Join(_, _))
        else tables.reduceLeft(HashJoin(_, _))
      }

      private def parseTableClause(): Operator =
        if accept("(") then {
          val nested = parseSelect()
          expect(")")
          nested
        } else {
          val table = nextToken()
          val schema = if accept("schema") then Some(parseFieldList()) else None
          val delim = if accept("delim") then Some(parseDelimiter(nextToken())) else None
          scan(table, schema, delim)
        }

      private def parsePredicate(): Predicate = {
        val lhs = parseRef()
        expect("=")
        Eq(lhs, parseRef())
      }

      private def parseRef(): Ref = {
        val token = nextToken()
        if token.startsWith("'") && token.endsWith("'") then Value(token.drop(1).dropRight(1))
        else if token.forall(_.isDigit) then Value(token.toInt)
        else Field(token)
      }

      private def parseFieldList(): Schema =
        parseCommaSeparated(nextToken()).toVector

      private def parseAliasedFieldList(): (Schema, Schema) = {
        val out = Vector.newBuilder[String]
        val in = Vector.newBuilder[String]
        var continue = true
        while continue do {
          val source = nextToken()
          val target = if accept("as") then nextToken() else source
          out += target
          in += source
          continue = accept(",")
        }
        (out.result(), in.result())
      }

      private def parseCommaSeparated(first: String): List[String] = {
        val values = mutable.ListBuffer(first)
        while accept(",") do values += nextToken()
        values.toList
      }

      private def parseDelimiter(token: String): Char =
        if token == "\\t" then '\t' else token.head

      private def accept(value: String): Boolean =
        if index < tokens.length && tokens(index) == value then {
          index += 1
          true
        } else false

      private def expect(value: String): Unit =
        if !accept(value) then
          throw new IllegalArgumentException(s"expected '$value' at token $index in ${tokens.mkString(" ")}")

      private def nextToken(): String = {
        if index >= tokens.length then throw new IllegalArgumentException("unexpected end of query")
        val token = tokens(index)
        index += 1
        token
      }

      private def expectEnd(): Unit =
        if index != tokens.length then
          throw new IllegalArgumentException(s"unexpected token '${tokens(index)}' at $index")
    }
  }

  private def tokenize(input: String): Vector[String] = {
    val out = Vector.newBuilder[String]
    val current = new StringBuilder
    var inString = false

    def flush(): Unit =
      if current.nonEmpty then {
        out += current.toString
        current.clear()
      }

    input.foreach {
      case '\'' =>
        current += '\''
        inString = !inString
      case ch if inString =>
        current += ch
      case ch @ (',' | '(' | ')' | '=' | '*') =>
        flush()
        out += ch.toString
      case ch if ch.isWhitespace =>
        flush()
      case ch =>
        current += ch
    }
    flush()
    out.result()
  }
}

class TutorialQueryUnstagedTest extends AnyFunSuite with Matchers {
  import TutorialQueryUnstaged.*

  private def withTables[A](body: Map[String, String] => A): A = {
    val t = Files.createTempFile("lms-query-t", ".csv")
    val words = Files.createTempFile("lms-query-words", ".csv")
    val tsv = Files.createTempFile("lms-query-1gram", ".tsv")
    Files.writeString(t, "Name,Value,Flag\nAlice,1,yes\nBob,2,no\nAlice,3,yes\n")
    Files.writeString(words, "Word\nAuswanderung\nScala\n")
    Files.writeString(tsv, "Auswanderung\t1990\t5\t1\nScala\t2000\t7\t2\nAuswanderung\t1991\t11\t3\n")
    try body(Map("t.csv" -> t.toString, "words.csv" -> words.toString, "?" -> tsv.toString))
    finally {
      Files.deleteIfExists(t)
      Files.deleteIfExists(words)
      Files.deleteIfExists(tsv)
    }
  }

  private def engine(paths: Map[String, String]): Engine =
    new Engine(table => paths.getOrElse(table, table))

  test("query_unstaged parses tutorial AST shapes") {
    withTables { paths =>
      val q = engine(paths)
      q.parseSql("select Name from t.csv where Flag='yes'") shouldBe
        Project(Vector("Name"), Vector("Name"),
          Filter(Eq(Field("Flag"), Value("yes")),
            q.scan("t.csv")))
      q.parseSql("select * from nestedloops t.csv join (select Name as Name1 from t.csv)") shouldBe
        Join(q.scan("t.csv"), Project(Vector("Name1"), Vector("Name"), q.scan("t.csv")))
    }
  }

  test("query_unstaged interprets scan, project, filter, and group") {
    withTables { paths =>
      val q = engine(paths)
      q.runSql("select * from t.csv") shouldBe Vector(
        Vector("Alice", "1", "yes"),
        Vector("Bob", "2", "no"),
        Vector("Alice", "3", "yes")
      )
      q.runSql("select Name from t.csv where Flag='yes'") shouldBe Vector(Vector("Alice"), Vector("Alice"))
      q.runSql("select * from t.csv group by Name sum Value").toSet shouldBe Set(
        Vector("Alice", "4"),
        Vector("Bob", "2")
      )
    }
  }

  test("query_unstaged interprets nested-loop and hash joins") {
    withTables { paths =>
      val q = engine(paths)
      q.runSql("select * from nestedloops t.csv join (select Name as Name1 from t.csv)") should have size 9
      q.runSql("select * from t.csv join (select Name from t.csv)") shouldBe Vector(
        Vector("Alice", "1", "yes", "Alice"),
        Vector("Alice", "3", "yes", "Alice"),
        Vector("Bob", "2", "no", "Bob"),
        Vector("Alice", "1", "yes", "Alice"),
        Vector("Alice", "3", "yes", "Alice")
      )
    }
  }

  test("query_unstaged supports external schema and tab delimiters") {
    withTables { paths =>
      val q = engine(paths)
      val table = "? schema Phrase, Year, MatchCount, VolumeCount delim \\t"
      q.runSql(s"select * from $table where Phrase='Auswanderung'") shouldBe Vector(
        Vector("Auswanderung", "1990", "5", "1"),
        Vector("Auswanderung", "1991", "11", "3")
      )
    }
  }
}
