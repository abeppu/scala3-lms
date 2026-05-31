package lms.core.examples

import lms.core.*
import lms.legacy.common.*
import lms.legacy.compat.SourceContext
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.annotation.targetName

object TutorialLinqSchema {
  case class Person(name: String, age: Int)
  case class Couple(her: String, him: String)
  case class PeopleDB(people: List[Person], couples: List[Couple])
  case class Department(dpt: String)
  case class Employee(dpt: String, emp: String)
  case class Task(emp: String, tsk: String)
  case class OrgDB(departments: List[Department], employees: List[Employee], tasks: List[Task])

  sealed trait AgePredicate
  case class Above(x: Int) extends AgePredicate
  case class Below(x: Int) extends AgePredicate
  case class And(x: AgePredicate, y: AgePredicate) extends AgePredicate
  case class Or(x: AgePredicate, y: AgePredicate) extends AgePredicate
  case class Not(x: AgePredicate) extends AgePredicate

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
    ),
    couples = List(
      Couple("Alex", "Bert"),
      Couple("Cora", "Drew")
    )
  )

  val org: OrgDB = OrgDB(
    departments = List(
      Department("Product"),
      Department("Quality"),
      Department("Research"),
      Department("Sales")
    ),
    employees = List(
      Employee("Product", "Alex"),
      Employee("Product", "Bert"),
      Employee("Research", "Cora"),
      Employee("Research", "Drew"),
      Employee("Research", "Edna"),
      Employee("Sales", "Fred")
    ),
    tasks = List(
      Task("Alex", "build"),
      Task("Bert", "build"),
      Task("Cora", "abstract"),
      Task("Cora", "build"),
      Task("Cora", "design"),
      Task("Drew", "abstract"),
      Task("Drew", "design"),
      Task("Edna", "abstract"),
      Task("Edna", "call"),
      Task("Edna", "design"),
      Task("Fred", "call")
    )
  )
}

trait TutorialLinqDsl extends Dsl with ListOps {
  import TutorialLinqSchema.*

  given personTyp: Typ[Person]
  given coupleTyp: Typ[Couple]
  given departmentTyp: Typ[Department]
  given employeeTyp: Typ[Employee]
  given taskTyp: Typ[Task]
  given recordTyp: Typ[Record]
  given peopleDbTyp: Typ[PeopleDB]
  given orgDbTyp: Typ[OrgDB]

  extension (person: Rep[Person])
    def name(using SourceContext): Rep[String]
    def age(using SourceContext): Rep[Int]

  extension (couple: Rep[Couple])
    def her(using SourceContext): Rep[String]
    def him(using SourceContext): Rep[String]

  extension (department: Rep[Department])
    @targetName("departmentDpt")
    def dpt(using SourceContext): Rep[String]

  extension (employee: Rep[Employee])
    @targetName("employeeDpt")
    def dpt(using SourceContext): Rep[String]
    @targetName("employeeEmp")
    def emp(using SourceContext): Rep[String]

  extension (task: Rep[Task])
    @targetName("taskEmp")
    def emp(using SourceContext): Rep[String]
    @targetName("taskTsk")
    def tsk(using SourceContext): Rep[String]

  extension (record: Rep[Record])
    @targetName("recordName")
    def name(using SourceContext): Rep[String]
    @targetName("recordAge")
    def age(using SourceContext): Rep[Int]
    @targetName("recordDiff")
    def diff(using SourceContext): Rep[Int]
    @targetName("recordDpt")
    def dpt(using SourceContext): Rep[String]

  extension (db: Rep[PeopleDB])
    def people(using SourceContext): Rep[List[Person]]
    def couples(using SourceContext): Rep[List[Couple]]

  extension (db: Rep[OrgDB])
    def departments(using SourceContext): Rep[List[Department]]
    def employees(using SourceContext): Rep[List[Employee]]
    def tasks(using SourceContext): Rep[List[Task]]

  def database(name: String)(using SourceContext): Rep[PeopleDB]
  def orgDatabase(name: String)(using SourceContext): Rep[OrgDB]
  def record(fields: (String, Rep[Any])*)(using SourceContext): Rep[Record]
}

trait TutorialLinqExp extends TutorialLinqDsl with DslExp with ListOpsExpOpt {
  import TutorialLinqSchema.*

  override given personTyp: Typ[Person] = manifestTyp
  override given coupleTyp: Typ[Couple] = manifestTyp
  override given departmentTyp: Typ[Department] = manifestTyp
  override given employeeTyp: Typ[Employee] = manifestTyp
  override given taskTyp: Typ[Task] = manifestTyp
  override given recordTyp: Typ[Record] = manifestTyp
  override given peopleDbTyp: Typ[PeopleDB] = manifestTyp
  override given orgDbTyp: Typ[OrgDB] = manifestTyp

  case class Database(name: String) extends Def[PeopleDB]
  case class OrgDatabase(name: String) extends Def[OrgDB]
  case class Table[A: Typ](db: Exp[PeopleDB], table: String) extends Def[List[A]]
  case class OrgTable[A: Typ](db: Exp[OrgDB], table: String) extends Def[List[A]]
  case class PersonName(person: Exp[Person]) extends Def[String]
  case class PersonAge(person: Exp[Person]) extends Def[Int]
  case class CoupleHer(couple: Exp[Couple]) extends Def[String]
  case class CoupleHim(couple: Exp[Couple]) extends Def[String]
  case class DepartmentDpt(department: Exp[Department]) extends Def[String]
  case class EmployeeDpt(employee: Exp[Employee]) extends Def[String]
  case class EmployeeEmp(employee: Exp[Employee]) extends Def[String]
  case class TaskEmp(task: Exp[Task]) extends Def[String]
  case class TaskTsk(task: Exp[Task]) extends Def[String]
  case class RecordNew(fields: Seq[(String, Exp[Any])]) extends Def[Record]
  case class RecordField[A: Typ](record: Exp[Record], field: String) extends Def[A]

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

  extension (couple: Rep[Couple])
    def her(using SourceContext): Rep[String] = CoupleHer(couple)
    def him(using SourceContext): Rep[String] = CoupleHim(couple)

  extension (department: Rep[Department])
    @targetName("departmentDpt")
    def dpt(using SourceContext): Rep[String] = DepartmentDpt(department)

  extension (employee: Rep[Employee])
    @targetName("employeeDpt")
    def dpt(using SourceContext): Rep[String] = EmployeeDpt(employee)
    @targetName("employeeEmp")
    def emp(using SourceContext): Rep[String] = EmployeeEmp(employee)

  extension (task: Rep[Task])
    @targetName("taskEmp")
    def emp(using SourceContext): Rep[String] = TaskEmp(task)
    @targetName("taskTsk")
    def tsk(using SourceContext): Rep[String] = TaskTsk(task)

  extension (record: Rep[Record])
    @targetName("recordName")
    def name(using SourceContext): Rep[String] = RecordField[String](record, "name")
    @targetName("recordAge")
    def age(using SourceContext): Rep[Int] = RecordField[Int](record, "age")
    @targetName("recordDiff")
    def diff(using SourceContext): Rep[Int] = RecordField[Int](record, "diff")
    @targetName("recordDpt")
    def dpt(using SourceContext): Rep[String] = RecordField[String](record, "dpt")

  extension (db: Rep[PeopleDB])
    def people(using SourceContext): Rep[List[Person]] = Table[Person](db, "people")
    def couples(using SourceContext): Rep[List[Couple]] = Table[Couple](db, "couples")

  extension (db: Rep[OrgDB])
    def departments(using SourceContext): Rep[List[Department]] = OrgTable[Department](db, "departments")
    def employees(using SourceContext): Rep[List[Employee]] = OrgTable[Employee](db, "employees")
    def tasks(using SourceContext): Rep[List[Task]] = OrgTable[Task](db, "tasks")

  def database(name: String)(using SourceContext): Rep[PeopleDB] = Database(name)
  def orgDatabase(name: String)(using SourceContext): Rep[OrgDB] = OrgDatabase(name)
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
      case Def(Table(Def(Database(db)), table)) =>
        val fun = reifyFun(f)
        reflectEffect(DBFor(source, f, db, table, fun), infix_star(summarizeEffects(fun.body)))
      case Def(OrgTable(Def(OrgDatabase(db)), table)) =>
        val fun = reifyFun(f)
        reflectEffect(DBFor(source, f, db, table, fun), infix_star(summarizeEffects(fun.body)))
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
    case OrgDatabase(name) =>
      emitValDef(sym, s"TutorialLinqSchema.$name")
    case Table(db, table) =>
      emitValDef(sym, s"${quote(db)}.$table")
    case OrgTable(db, table) =>
      emitValDef(sym, s"${quote(db)}.$table")
    case PersonName(person) =>
      emitValDef(sym, s"${quote(person)}.name")
    case PersonAge(person) =>
      emitValDef(sym, s"${quote(person)}.age")
    case CoupleHer(couple) =>
      emitValDef(sym, s"${quote(couple)}.her")
    case CoupleHim(couple) =>
      emitValDef(sym, s"${quote(couple)}.him")
    case DepartmentDpt(department) =>
      emitValDef(sym, s"${quote(department)}.dpt")
    case EmployeeDpt(employee) =>
      emitValDef(sym, s"${quote(employee)}.dpt")
    case EmployeeEmp(employee) =>
      emitValDef(sym, s"${quote(employee)}.emp")
    case TaskEmp(task) =>
      emitValDef(sym, s"${quote(task)}.emp")
    case TaskTsk(task) =>
      emitValDef(sym, s"${quote(task)}.tsk")
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
  val org: Rep[OrgDB] = orgDatabase("org")

  type Names = List[Record]

  def ageDifferences: Rep[Names] =
    for {
      couple <- db.couples
      her <- db.people
      him <- db.people
      if couple.her == her.name && couple.him == him.name && her.age > him.age
    } yield record("name" -> her.name, "diff" -> (her.age - him.age))

  def range(a: Rep[Int], b: Rep[Int]): Rep[Names] =
    for {
      person <- db.people
      if a <= person.age && person.age < b
    } yield record("name" -> person.name)

  def rangeWithAge(a: Rep[Int], b: Rep[Int]): Rep[Names] =
    for {
      person <- db.people
      if a <= person.age && person.age < b
    } yield record("name" -> person.name, "age" -> person.age)

  def satisfies(p: Rep[Int] => Rep[Boolean]): Rep[Names] =
    for {
      person <- db.people
      if p(person.age)
    } yield record("name" -> person.name)

  def predicate(predicate: AgePredicate)(age: Rep[Int]): Rep[Boolean] =
    predicate match {
      case Above(x) => x <= age
      case Below(x) => age < x
      case And(x, y) => this.predicate(x)(age) && this.predicate(y)(age)
      case Or(x, y) => this.predicate(x)(age) || this.predicate(y)(age)
      case Not(x) => !this.predicate(x)(age)
    }

  def thirtySomethingsByPredicate: Rep[Names] =
    satisfies(x => 30 <= x && x < 40)

  def evenAges: Rep[Names] =
    satisfies(_ % 2 == 0)

  def dynamicPredicateRange: Rep[Names] =
    satisfies(predicate(And(Above(30), Below(40))))

  def dynamicPredicateNotOr: Rep[Names] =
    satisfies(predicate(Not(Or(Below(30), Above(40)))))

  def ageFromName(name: Rep[String]): Rep[List[Int]] =
    for {
      person <- db.people
      if person.name == name
    } yield person.age

  def rangeFromNames(start: Rep[String], end: Rep[String]): Rep[Names] =
    for {
      a <- ageFromName(start)
      b <- ageFromName(end)
      record <- range(a, b)
    } yield record

  def concatenatedRanges: Rep[Names] =
    rangeWithAge(30, 34) ++ rangeWithAge(55, 61)

  def emptyRangeCheck: Rep[Boolean] =
    rangeWithAge(90, 100).isEmpty

  def explicitMapNames: Rep[Names] =
    db.people.map(person => record("name" -> person.name))

  def explicitFilterMapNames: Rep[Names] =
    db.people.filter(person => person.age < 40).map(person => record("name" -> person.name))

  def literalRecords: Rep[Names] =
    List(record("name" -> "literal", "age" -> 1))

  def exists(xs: Rep[List[Record]]): Rep[Boolean] =
    !xs.isEmpty

  def expertise(taskName: Rep[String]): Rep[Names] =
    for {
      department <- org.departments
      if !exists(
        for {
          employee <- org.employees
          if department.dpt == employee.dpt && !exists(
            for {
              task <- org.tasks
              if employee.emp == task.emp && task.tsk == taskName
            } yield record()
          )
        } yield record()
      )
    } yield record("dpt" -> department.dpt)
}

@virt
object TutorialLinqSnippet extends DslDriver[Unit, List[TutorialLinqSchema.Record]] with TutorialLinqProgram with TutorialLinqExp { self =>
  override val codegen = new TutorialLinqGen {
    val IR: self.type = self
  }

  def snippet(unit: Rep[Unit]): Rep[List[TutorialLinqSchema.Record]] =
    rangeFromNames("Edna", "Bert")
}

@virt
object TutorialLinqAgeSnippet extends DslDriver[Unit, List[TutorialLinqSchema.Record]] with TutorialLinqProgram with TutorialLinqExp { self =>
  override val codegen = new TutorialLinqGen {
    val IR: self.type = self
  }

  def snippet(unit: Rep[Unit]): Rep[List[TutorialLinqSchema.Record]] =
    rangeWithAge(30, 40)
}

@virt
object TutorialLinqPredicateSnippet extends DslDriver[Unit, List[TutorialLinqSchema.Record]] with TutorialLinqProgram with TutorialLinqExp { self =>
  override val codegen = new TutorialLinqGen {
    val IR: self.type = self
  }

  def snippet(unit: Rep[Unit]): Rep[List[TutorialLinqSchema.Record]] =
    thirtySomethingsByPredicate
}

@virt
object TutorialLinqEvenAgeSnippet extends DslDriver[Unit, List[TutorialLinqSchema.Record]] with TutorialLinqProgram with TutorialLinqExp { self =>
  override val codegen = new TutorialLinqGen {
    val IR: self.type = self
  }

  def snippet(unit: Rep[Unit]): Rep[List[TutorialLinqSchema.Record]] =
    evenAges
}

@virt
object TutorialLinqDynamicPredicateSnippet extends DslDriver[Unit, List[TutorialLinqSchema.Record]] with TutorialLinqProgram with TutorialLinqExp { self =>
  override val codegen = new TutorialLinqGen {
    val IR: self.type = self
  }

  def snippet(unit: Rep[Unit]): Rep[List[TutorialLinqSchema.Record]] =
    dynamicPredicateRange
}

@virt
object TutorialLinqDynamicPredicateNotOrSnippet extends DslDriver[Unit, List[TutorialLinqSchema.Record]] with TutorialLinqProgram with TutorialLinqExp { self =>
  override val codegen = new TutorialLinqGen {
    val IR: self.type = self
  }

  def snippet(unit: Rep[Unit]): Rep[List[TutorialLinqSchema.Record]] =
    dynamicPredicateNotOr
}

@virt
object TutorialLinqConcatSnippet extends DslDriver[Unit, List[TutorialLinqSchema.Record]] with TutorialLinqProgram with TutorialLinqExp { self =>
  override val codegen = new TutorialLinqGen {
    val IR: self.type = self
  }

  def snippet(unit: Rep[Unit]): Rep[List[TutorialLinqSchema.Record]] =
    concatenatedRanges
}

@virt
object TutorialLinqIsEmptySnippet extends DslDriver[Unit, Boolean] with TutorialLinqProgram with TutorialLinqExp { self =>
  override val codegen = new TutorialLinqGen {
    val IR: self.type = self
  }

  def snippet(unit: Rep[Unit]): Rep[Boolean] =
    emptyRangeCheck
}

@virt
object TutorialLinqDifferencesSnippet extends DslDriver[Unit, List[TutorialLinqSchema.Record]] with TutorialLinqProgram with TutorialLinqExp { self =>
  override val codegen = new TutorialLinqGen {
    val IR: self.type = self
  }

  def snippet(unit: Rep[Unit]): Rep[List[TutorialLinqSchema.Record]] =
    ageDifferences
}

@virt
object TutorialLinqExplicitMapSnippet extends DslDriver[Unit, List[TutorialLinqSchema.Record]] with TutorialLinqProgram with TutorialLinqExp { self =>
  override val codegen = new TutorialLinqGen {
    val IR: self.type = self
  }

  def snippet(unit: Rep[Unit]): Rep[List[TutorialLinqSchema.Record]] =
    explicitMapNames
}

@virt
object TutorialLinqExplicitFilterMapSnippet extends DslDriver[Unit, List[TutorialLinqSchema.Record]] with TutorialLinqProgram with TutorialLinqExp { self =>
  override val codegen = new TutorialLinqGen {
    val IR: self.type = self
  }

  def snippet(unit: Rep[Unit]): Rep[List[TutorialLinqSchema.Record]] =
    explicitFilterMapNames
}

@virt
object TutorialLinqLiteralListSnippet extends DslDriver[Unit, List[TutorialLinqSchema.Record]] with TutorialLinqProgram with TutorialLinqExp { self =>
  override val codegen = new TutorialLinqGen {
    val IR: self.type = self
  }

  def snippet(unit: Rep[Unit]): Rep[List[TutorialLinqSchema.Record]] =
    literalRecords
}

@virt
object TutorialLinqExpertiseSnippet extends DslDriver[Unit, List[TutorialLinqSchema.Record]] with TutorialLinqProgram with TutorialLinqExp { self =>
  override val codegen = new TutorialLinqGen {
    val IR: self.type = self
  }

  def snippet(unit: Rep[Unit]): Rep[List[TutorialLinqSchema.Record]] =
    expertise("abstract")
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

  test("linq rangeWithAge host result covers multi-field records") {
    val db = TutorialLinqSchema.db
    val result =
      for {
        person <- db.people
        if 30 <= person.age && person.age < 40
      } yield (person.name, person.age)

    result shouldBe List("Cora" -> 33, "Drew" -> 31)
  }

  test("linq rangeWithAge emits multi-field staged records") {
    check("rangeWithAge", TutorialLinqAgeSnippet.code)
    TutorialLinqAgeSnippet.code should include("new TutorialLinqSchema.Record")
    TutorialLinqAgeSnippet.code should include("val name =")
    TutorialLinqAgeSnippet.code should include("val age =")
  }

  test("linq satisfies host result covers higher-order predicates") {
    val db = TutorialLinqSchema.db
    val result =
      for {
        person <- db.people
        if 30 <= person.age && person.age < 40
      } yield person.name

    result shouldBe List("Cora", "Drew")
  }

  test("linq satisfies emits normalized higher-order predicate query") {
    check("predicateRange", TutorialLinqPredicateSnippet.code)
    TutorialLinqPredicateSnippet.code should include("TutorialLinqSchema.db.people.flatMap")
    TutorialLinqPredicateSnippet.code should include("30 <= ")
    TutorialLinqPredicateSnippet.code should include(" < 40")
  }

  test("linq satisfies supports staged modulo predicates") {
    check("evenAges", TutorialLinqEvenAgeSnippet.code)
    TutorialLinqEvenAgeSnippet.code should include(" % 2")
    TutorialLinqEvenAgeSnippet.code should include(" == 0")
  }

  test("linq dynamic predicate AST host result matches direct predicate") {
    val db = TutorialLinqSchema.db
    def p(predicate: TutorialLinqSchema.AgePredicate)(age: Int): Boolean =
      predicate match {
        case TutorialLinqSchema.Above(x) => x <= age
        case TutorialLinqSchema.Below(x) => age < x
        case TutorialLinqSchema.And(x, y) => p(x)(age) && p(y)(age)
        case TutorialLinqSchema.Or(x, y) => p(x)(age) || p(y)(age)
        case TutorialLinqSchema.Not(x) => !p(x)(age)
      }

    db.people.filter(person => p(TutorialLinqSchema.And(TutorialLinqSchema.Above(30), TutorialLinqSchema.Below(40)))(person.age)).map(_.name) shouldBe List("Cora", "Drew")
    db.people.filter(person => p(TutorialLinqSchema.Not(TutorialLinqSchema.Or(TutorialLinqSchema.Below(30), TutorialLinqSchema.Above(40))))(person.age)).map(_.name) shouldBe List("Cora", "Drew")
  }

  test("linq dynamic predicate AST emits host-match-selected staged query") {
    check("dynamicPredicateRange", TutorialLinqDynamicPredicateSnippet.code)
    TutorialLinqDynamicPredicateSnippet.code should include("30 <= ")
    TutorialLinqDynamicPredicateSnippet.code should include(" < 40")
  }

  test("linq dynamic predicate AST supports host-side not/or composition") {
    check("dynamicPredicateNotOr", TutorialLinqDynamicPredicateNotOrSnippet.code)
    TutorialLinqDynamicPredicateNotOrSnippet.code should include("||")
    TutorialLinqDynamicPredicateNotOrSnippet.code should include("} else {")
  }

  test("linq list concat host result combines query results") {
    val db = TutorialLinqSchema.db
    def range(start: Int, end: Int): List[(String, Int)] =
      for {
        person <- db.people
        if start <= person.age && person.age < end
      } yield person.name -> person.age

    range(30, 34) ++ range(55, 61) shouldBe List(
      "Cora" -> 33,
      "Drew" -> 31,
      "Alex" -> 60,
      "Bert" -> 55,
      "Fred" -> 60
    )
  }

  test("linq list concat emits staged ListConcat") {
    check("rangeConcat", TutorialLinqConcatSnippet.code)
    TutorialLinqConcatSnippet.code should include(" ::: ")
    TutorialLinqConcatSnippet.code should include("new TutorialLinqSchema.Record")
  }

  test("linq list isEmpty host result detects empty query") {
    val db = TutorialLinqSchema.db
    db.people.filter(person => 90 <= person.age && person.age < 100).isEmpty shouldBe true
  }

  test("linq list isEmpty emits staged ListIsEmpty") {
    check("rangeIsEmpty", TutorialLinqIsEmptySnippet.code)
    TutorialLinqIsEmptySnippet.code should include(".isEmpty")
    TutorialLinqIsEmptySnippet.code should include("TutorialLinqSchema.db.people.flatMap")
  }

  test("linq differences host result covers multiple database tables") {
    val db = TutorialLinqSchema.db
    val result =
      for {
        couple <- db.couples
        her <- db.people
        him <- db.people
        if couple.her == her.name && couple.him == him.name && her.age > him.age
      } yield couple.her -> (her.age - him.age)

    result shouldBe List("Alex" -> 5, "Cora" -> 2)
  }

  test("linq differences emits normalized multi-table DBFor traversal") {
    check("differences", TutorialLinqDifferencesSnippet.code)
    TutorialLinqDifferencesSnippet.code should include("TutorialLinqSchema.db.couples.flatMap")
    TutorialLinqDifferencesSnippet.code should include("TutorialLinqSchema.db.people.flatMap")
    TutorialLinqDifferencesSnippet.code should include("val diff =")
  }

  test("linq explicit map emits normalized staged list map") {
    check("explicitMapNames", TutorialLinqExplicitMapSnippet.code)
    TutorialLinqExplicitMapSnippet.code should include("TutorialLinqSchema.db.people.flatMap")
    TutorialLinqExplicitMapSnippet.code should include("new TutorialLinqSchema.Record")
  }

  test("linq explicit filter/map emits normalized staged list filter") {
    check("explicitFilterMapNames", TutorialLinqExplicitFilterMapSnippet.code)
    TutorialLinqExplicitFilterMapSnippet.code should include(" < 40")
    TutorialLinqExplicitFilterMapSnippet.code should include("if (")
  }

  test("linq literal List emits staged ListNew") {
    check("literalRecords", TutorialLinqLiteralListSnippet.code)
    TutorialLinqLiteralListSnippet.code should include("List(")
    TutorialLinqLiteralListSnippet.code should include("literal")
  }

  test("linq expertise host result covers nested existence") {
    val org = TutorialLinqSchema.org
    def exists[A](xs: List[A]): Boolean = xs.nonEmpty
    val result =
      for {
        department <- org.departments
        if !exists(
          for {
            employee <- org.employees
            if department.dpt == employee.dpt && !exists(
              for {
                task <- org.tasks
                if employee.emp == task.emp && task.tsk == "abstract"
              } yield ()
            )
          } yield ()
        )
      } yield department.dpt

    result shouldBe List("Quality", "Research")
  }

  test("linq expertise emits nested DBFor and staged emptiness checks") {
    check("expertise", TutorialLinqExpertiseSnippet.code)
    TutorialLinqExpertiseSnippet.code should include("TutorialLinqSchema.org.departments.flatMap")
    TutorialLinqExpertiseSnippet.code should include("TutorialLinqSchema.org.employees.flatMap")
    TutorialLinqExpertiseSnippet.code should include("TutorialLinqSchema.org.tasks.flatMap")
    TutorialLinqExpertiseSnippet.code should include(".isEmpty")
    TutorialLinqExpertiseSnippet.code should include("val dpt =")
  }
}
