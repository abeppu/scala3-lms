package lms.core.examples

import lms.core.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.language.implicitConversions

@virt
trait AckermannTutorial extends Dsl {
  def ack(m: Int): Rep[Int => Int] = doLambda[Int, Int] { (n: Rep[Int]) =>
    generate_comment(s"ack_$m")
    if (m == 0) n + 1
    else if (n == 0) ack(m - 1)(1)
    else ack(m - 1)(ack(m)(n - 1))
  }
}

class TutorialAckermannTest extends AnyFunSuite with Matchers {
  test("ackermann example emits staged recursive function code") {
    @virt
    object Ack2 extends DslDriver[Int, Int] with Dsl with AckermannTutorial {
      def snippet(n: Rep[Int]): Rep[Int] = ack(2)(n)
    }

    Ack2.code should include("ack_2")
    Ack2.code should include("Function1")
  }
}
