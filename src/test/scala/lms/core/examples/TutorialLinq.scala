package lms.core.examples

import lms.core.*
import lms.legacy.common.*
import lms.legacy.compat.SourceContext
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.annotation.targetName

object TutorialLinqSchema {
  case class Person(name: String, age: Int)
  case class PeopleDB(people: List[Person])

  abstract class Record extends Product {
    lazy val elems: List[(String, Any)] = {
      val fields = getClass.getDeclaredFields.toList
      for (field <- fields if !field.getName.contains("$")) yield {
        field.setAccessible(true)
        (field.getName, field.get(this))
      }
    }

    def canEqual(that: Any): Boolean = true
    def productElement(n: Int): Any = elems(n)._2
    def productArity: Int = elems.length
    override def productIterator: Iterator[Any] = elems.map(_._2).iterator
    override def toString: String = elems.map { case (name, value) => s"$name:$value" }.mkString("{", ",", "}")
  }

  val db: PeopleDB = PeopleDB(
    people = List(
      Person("Alex", 60),
      Person("Bert", 55),
      Person("Cora", 33),
      Person("Drew", 31),
      Person("Edna", 21),
      Person("Fred", 60)
    )
  )
}

trait TutorialLinqDsl extends Dsl with ListOps {
  import TutorialLinqSchema.*

  given personTyp: Typ[Person]
  given recordTyp: Typ[Record]
  given peopleDbTyp: Typ[PeopleDB]

  extension (person: Rep[Person])
    def name(using SourceContext): Rep[String]
    def age(using SourceContext): Rep[Int]

  extension (record: Rep[Record])
    @targetName("recordName")
    def name(using SourceContext): Rep[String]

  extension (db: Rep[PeopleDB])
    def people(using SourceContext): Rep[List[Person]]

  def database(name: String)(using SourceContext): Rep[PeopleDB]
  def record(fields: (String, Rep[Any])*)(using SourceContext): Rep[Record]
}

trait TutorialLinqExp extends TutorialLinqDsl with DslExp with ListOpsExpOpt {
  import TutorialLinqSchema.*

  override given personTyp: Typ[Person] = manifestTyp
  override given recordTyp: Typ[Record] = manifestTyp
  override given peopleDbTyp: Typ[PeopleDB] = manifestTyp

  case class Database(name: String) extends Def[PeopleDB]
  case class People(db: Exp[PeopleDB]) extends Def[List[Person]]
  case class PersonName(person: Exp[Person]) extends Def[String]
  case class PersonAge(person: Exp[Person]) extends Def[Int]
  case class RecordNew(fields: Seq[(String, Exp[Any])]) extends Def[Record]
  case class RecordField(record: Exp[Record], field: String) extends Def[String]

  case class DBFor[A: Typ, B: Typ](
    source: Exp[List[A]],
    f: Exp[A] => Exp[List[B]],
    db: String,
    table: String,
    fun: Fun[A, List[B]]
  ) extends Def[List[B]]

  case class Fun[A, B](arg: Sym[A], body: Block[B])

  object Empty {
    def apply[A: Typ]()(using SourceContext): Exp[List[A]] = List[A]()
    def unapply[A](x: Exp[List[A]]): Boolean = x match {
      case Def(ListNew(xs)) if xs.isEmpty => true
      case _ => false
    }
  }

  object Yield {
    def apply[A: Typ](x: Exp[A])(using SourceContext): Exp[List[A]] = List[A](x)
    def unapply[A](x: Exp[List[A]]): Option[Exp[A]] = x match {
      case Def(ListNew(xs)) if xs.length == 1 => Some(xs.head)
      case _ => None
    }
  }

  object IfThen {
    def unapply[A](x: Exp[List[A]]): Option[(Exp[Boolean], Exp[List[A]])] = x match {
      case Def(IfThenElse(c, Block(a), Block(Empty()))) => Some((c, a))
      case _ => None
    }
  }

  object For {
    def unapply[B](x: Exp[List[B]]): Option[(Exp[List[Any]], Exp[Any] => Exp[List[B]])] = x match {
      case Def(DBFor(source, f, _, _, _)) => Some((source.asInstanceOf[Exp[List[Any]]], f.asInstanceOf[Exp[Any] => Exp[List[B]]]))
      case _ => None
    }
  }

  object Concat {
    def unapply[A](x: Exp[List[A]]): Option[(Exp[List[A]], Exp[List[A]])] = x match {
      case Def(ListConcat(a, b)) => Some((a, b))
      case _ => None
    }
  }

  extension (person: Rep[Person])
    def name(using SourceContext): Rep[String] = PersonName(person)
    def age(using SourceContext): Rep[Int] = PersonAge(person)

  extension (record: Rep[Record])
    @targetName("recordName")
    def name(using SourceContext): Rep[String] = RecordField(record, "name")

  extension (db: Rep[PeopleDB])
    def people(using SourceContext): Rep[List[Person]] = People(db)

  def database(name: String)(using SourceContext): Rep[PeopleDB] = Database(name)
  def record(fields: (String, Rep[Any])*)(using SourceContext): Rep[Record] =
    RecordNew(fields.map { case (name, value) => name -> value })

  def reifyFun[A: Typ, B: Typ](f: Rep[A] => Rep[B]): Fun[A, B] = {
    val arg = fresh[A]
    Fun(arg, reifyEffects(f(arg)))
  }

  def dbfor[A: Typ, B: Typ](source: Exp[List[A]], f: Exp[A] => Exp[List[B]])(using SourceContext): Exp[List[B]] =
    source match {
      case Empty() =>
        List[B]()
      case Yield(a) =>
        f(a)
      case IfThen(c, nested) =>
        __ifThenElse[List[B]](c, nested.flatMap(f), List[B]())
      case For(nested, nestedF) =>
        nested.flatMap(x => nestedF(x).flatMap(y => f(y.asInstanceOf[Exp[A]])))
      case Concat(a, b) =>
        a.flatMap(f) ++ b.flatMap(f)
      case Def(People(Def(Database(db)))) =>
        val fun = reifyFun(f)
        reflectEffect(DBFor(source, f, db, "people", fun), infix_star(summarizeEffects(fun.body)))
      case _ =>
        super.list_flatMap(source, f)
    }

  override def list_flatMap[A: Typ, B: Typ](source: Exp[List[A]], f: Exp[A] => Exp[List[B]])(using SourceContext): Exp[List[B]] =
    dbfor(source, f)

  override def list_map[A: Typ, B: Typ](source: Exp[List[A]], f: Exp[A] => Exp[B])(using SourceContext): Exp[List[B]] =
    list_flatMap(source, x => Yield(f(x)))

  override def list_filter[A: Typ](source: Exp[List[A]], f: Exp[A] => Exp[Boolean])(using SourceContext): Exp[List[A]] =
    list_flatMap(source, x => __ifThenElse[List[A]](f(x), Yield(x), Empty()))

  override def syms(e: Any): List[Sym[Any]] = e match {
    case DBFor(source, _, _, _, Fun(_, body)) => syms(source) ::: syms(body)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case DBFor(_, _, _, _, Fun(arg, body)) => arg :: effectSyms(body)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case DBFor(source, _, _, _, Fun(_, body)) => freqNormal(source) ::: freqCold(body)
    case _ => super.symsFreq(e)
  }
}

trait TutorialLinqGen extends DslGen with ScalaGenListOps {
  val IR: TutorialLinqExp
  import IR.*

  override def emitFileHeader(): Unit = {
    super.emitFileHeader()
    stream.println("import lms.core.examples.TutorialLinqSchema")
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case Database(name) =>
      emitValDef(sym, s"TutorialLinqSchema.$name")
    case People(db) =>
      emitValDef(sym, s"${quote(db)}.people")
    case PersonName(person) =>
      emitValDef(sym, s"${quote(person)}.name")
    case PersonAge(person) =>
      emitValDef(sym, s"${quote(person)}.age")
    case RecordNew(fields) =>
      val fieldDefs = fields.map { case (name, value) => s"val $name = ${quote(value)}" }.mkString("; ")
      emitValDef(sym, s"new TutorialLinqSchema.Record { $fieldDefs }")
    case RecordField(record, field) =>
      emitValDef(sym, s"${quote(record)}.$field")
    case DBFor(_, _, db, table, Fun(arg, body)) =>
      stream.println(s"val ${quote(sym)} = TutorialLinqSchema.$db.$table.flatMap { ${quote(arg)} =>")
      emitBlock(body)
      stream.println(quote(getBlockResult(body)))
      stream.println("}")
    case _ =>
      super.emitNode(sym, rhs)
  }
}

@virt
trait TutorialLinqProgram extends TutorialLinqDsl {
  import TutorialLinqSchema.*

  val db: Rep[PeopleDB] = database("db")

  type Names = List[Record]

  def range(a: Rep[Int], b: Rep[Int]): Rep[Names] =
    for {
      person <- db.people
      if a <= person.age && person.age < b
    } yield record("name" -> person.name)

  def ageFromName(name: Rep[String]): Rep[List[Int]] =
    for {
      person <- db.people
      if person.name == name
    } yield person.age

  def rangeFromNames(start: Rep[String], end: Rep[String]): Rep[Names] =
    list_flatMap[Int, Record](ageFromName(start), (a: Rep[Int]) =>
      list_flatMap[Int, Record](ageFromName(end), (b: Rep[Int]) =>
        list_map[Record, Record](range(a, b), (record: Rep[Record]) => record)
      )
    )
}

@virt
object TutorialLinqSnippet extends DslDriver[Unit, List[TutorialLinqSchema.Record]] with TutorialLinqProgram with TutorialLinqExp { self =>
  override val codegen = new TutorialLinqGen {
    val IR: self.type = self
  }

  def snippet(unit: Rep[Unit]): Rep[List[TutorialLinqSchema.Record]] =
    rangeFromNames("Edna", "Bert")
}

class TutorialLinqTest extends TutorialFunSuite with Matchers {
  val under = "linq/"

  test("linq rangeFromNames host result matches legacy tutorial") {
    val db = TutorialLinqSchema.db
    def ageFromName(name: String): List[Int] =
      for {
        person <- db.people
        if person.name == name
      } yield person.age
    def range(start: Int, end: Int): List[String] =
      for {
        person <- db.people
        if start <= person.age && person.age < end
      } yield person.name
    val result =
      for {
        start <- ageFromName("Edna")
        end <- ageFromName("Bert")
        name <- range(start, end)
      } yield name

    result shouldBe List("Cora", "Drew", "Edna")
  }

  test("linq rangeFromNames emits normalized staged list traversal") {
    check("rangeFromNames", TutorialLinqSnippet.code)
    TutorialLinqSnippet.code should include("TutorialLinqSchema.db.people.flatMap")
    TutorialLinqSnippet.code should include("new TutorialLinqSchema.Record")
    TutorialLinqSnippet.code should include("val name =")
  }
}
