package lms.core

import lms.legacy.compat.Manifest
import lms.legacy.internal.Expressions
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.quoted.*

class TypeReificationTest extends AnyFunSuite with Matchers {
  private object probe extends Expressions

  private def typeReprShow(typ: probe.Typ[?]): String = {
    given staging.Compiler = staging.Compiler.make(getClass.getClassLoader)
    staging.run {
      (q: Quotes) ?=>
        import q.reflect.*
        Expr(typ.asTypeRepr.show)
    }
  }

  test("manifest type reification preserves applied type arguments") {
    val typ = probe.ManifestTyp(Manifest.of[List[Int]])

    typeReprShow(typ).shouldBe("scala.collection.immutable.List[scala.Int]")
  }

  test("manifest type reification preserves nested applied type arguments") {
    val typ = probe.ManifestTyp(Manifest.of[Map[String, List[Int]]])

    typeReprShow(typ).shouldBe(
      "scala.collection.immutable.Map[java.lang.String, scala.collection.immutable.List[scala.Int]]"
    )
  }

  test("manifest type reification preserves array element types") {
    val typ = probe.ManifestTyp(Manifest.of[Array[List[Int]]])

    typeReprShow(typ).shouldBe("scala.Array[scala.collection.immutable.List[scala.Int]]")
  }

  test("manifest type reification preserves primitive array element types") {
    val typ = probe.ManifestTyp(Manifest.of[Array[Int]])

    typeReprShow(typ).shouldBe("scala.Array[scala.Int]")
  }

  test("variable type reification preserves inner element type") {
    val typ = probe.VariableTyp(probe.ManifestTyp(Manifest.of[Int]))

    typeReprShow(typ).shouldBe("lms.legacy.internal.Expressions#Variable[scala.Int]")
  }

  test("variable type reification preserves nested applied inner types") {
    val typ = probe.VariableTyp(probe.ManifestTyp(Manifest.of[List[String]]))

    typeReprShow(typ).shouldBe("lms.legacy.internal.Expressions#Variable[scala.collection.immutable.List[java.lang.String]]")
  }

  test("variable array type reification preserves inner element type") {
    val typ = probe.VariableTyp(probe.ManifestTyp(Manifest.of[Int])).arrayTyp

    typeReprShow(typ).shouldBe("scala.Array[lms.legacy.internal.Expressions#Variable[scala.Int]]")
  }

  test("nested variable array type reification preserves inner element type") {
    val typ = probe.VariableTyp(probe.ManifestTyp(Manifest.of[Int])).arrayTyp.arrayTyp

    typeReprShow(typ).shouldBe("scala.Array[scala.Array[lms.legacy.internal.Expressions#Variable[scala.Int]]]")
  }

}
