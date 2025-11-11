package lms.gen


import lms.legacy.common.{BaseExp, EffectExp}
import scala.quoted.*

trait StagingCompile extends QuotedGen {
  this: EffectExp => 
  
  def compile[A:Typ, B:Typ](f: Exp[A] => Exp[B]): A => B = {
    println("starting compile")

    given staging.Compiler = staging.Compiler.make(getClass.getClassLoader)

    println("set up compiler")

    staging.run((q: Quotes) ?=> {
      //println(s"starting run with qp = ${qp}")
      import q.reflect.*

      // 0. ensure type info is available
      val typA: Typ[A] = summon[Typ[A]]
      val typB: Typ[B] = summon[Typ[B]]
      val typeARepr = typA.asTypeRepr
      val typeBRepr = typB.asTypeRepr
      given typeA: Type[A] = typeARepr.asType.asInstanceOf[Type[A]]
      given typeB: Type[B] = typeBRepr.asType.asInstanceOf[Type[B]]

      // TODO for arity > 1, we need multiple input symbols
      // 1. Create a fresh symbol for the input
      val inputSym: Sym[A] = fresh[A](using typA)
      println(s"Input symbol: ${inputSym}")

      // 2. Reify the function body
      val savedContext = this.context
      this.context = Nil
      val (body: Exp[B], schedule: List[Stm]) =
        try reifySubGraph {
          f(inputSym)
        }
        finally
          this.context = savedContext

      println(s"Reified body: ${body}, schedule: ${schedule}")
      // 3. Roll a lambda term
      // TODO: handle multiple parameters
      val methodType = MethodType(List("a"))(
        _ => List(typeARepr),
        _ => typeBRepr
      )
      println(s"Method type: ${methodType}")
      val lambdaTerm = Lambda(Symbol.spliceOwner, methodType, (owner, params) => {
        // Map inputSym to the parameter symbol in the environment
        val paramSym = params.head match {
          case v: ValDef => v.symbol
          case ident: Ident => ident.symbol // I
          case _ => throw new Exception(s"Could not find symbol for parameter, got: ${params.head}")
        }

        val envWithParam: Map[Sym[?], q.reflect.Symbol] = Map(inputSym -> paramSym)
        interpretSchedule[B]((body, schedule))(using q, envWithParam).changeOwner(owner) // Change the owner to the current lambda term owner
        // You may need to update interpretSchedule to accept the environment
      })
      println(s"Lambda term: ${lambdaTerm.show}")

      // 4. Convert the lambda term to an Expr[A => B]
      val stagedF: Expr[A => B] = lambdaTerm.asExprOf[A => B]
      println(s"Staged function: ${stagedF.show}")
      stagedF
    })
  }
}
