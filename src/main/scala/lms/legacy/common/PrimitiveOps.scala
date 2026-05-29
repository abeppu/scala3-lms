package lms.legacy.common

import scala.language.implicitConversions

import lms.gen.{Gen, StagingCompile}

import java.io.PrintWriter
import lms.legacy.util.OverloadHack
import lms.legacy.compat.SourceContext

import scala.compiletime.deferred

trait LiftPrimitives {
  this: PrimitiveOps =>

  implicit def intToRepInt(x: Int): Rep[Int] = unit(x)  
  implicit def floatToRepFloat(x: Float): Rep[Float] = unit(x)
  implicit def doubleToRepDouble(x: Double): Rep[Double] = unit(x)
  
  // precision-widening promotions
  given chainIntToRepFloat[A:Typ](using c: A => Rep[Int]): Conversion[A,Rep[Float]] with {
    def apply(x: A): Rep[Float] = repIntToRepFloat(c(x))
  }
  given chainFloatToRepDouble[A:Typ](using c: A => Rep[Float]): Conversion[A, Rep[Double]] with {
    def apply (x: A) = repFloatToRepDouble(c(x))
  }
}

/**
 * This file is extremely boilerplate and redundant and does not take advantage of any of
 * Scala's type hierarchy to reduce the amount of IR nodes or code generation require.
 * It is in semi-desperate need of a refactor.
 */
trait PrimitiveOps extends Variables with OverloadHack { 
  this: ImplicitOps =>

  given byteTyp   : Typ[Byte] = deferred
  given charTyp   : Typ[Char] = deferred
  given shortTyp  : Typ[Short] = deferred
  given intTyp    : Typ[Int] = deferred
  given longTyp   : Typ[Long] = deferred
  given floatTyp  : Typ[Float] = deferred
  given doubleTyp : Typ[Double] = deferred

  /**
   * Primitive conversions
   */
  implicit def repIntToRepDouble   (x: Rep[Int])  : Rep[Double] = x.toDouble
  implicit def repIntToRepFloat    (x: Rep[Int])  : Rep[Float]  = x.toFloat
  implicit def repFloatToRepDouble (x: Rep[Float]): Rep[Double] = x.toDouble

  /**
   * Enumerate all combinations of primitive math.
   * Avoids certain fragile behavior, including compiler crashes and some erroneous or inaccessible type errors.
   */
  def infix_-(a: Int        , b: Rep[Int]   )(using                  c: SourceContext): Rep[Int]    = int_minus(unit(a), b)
  def infix_-(a: Int        , b: Rep[Float] )(using o: Overloaded1 , c: SourceContext): Rep[Float]  = float_minus(unit(a.toFloat), b)
  def infix_-(a: Int        , b: Rep[Double])(using o: Overloaded2 , c: SourceContext): Rep[Double] = double_minus(unit(a), b)
  def infix_-(a: Float      , b: Rep[Int]   )(using                  c: SourceContext): Rep[Float]  = float_minus(unit(a),b)
  def infix_-(a: Float      , b: Rep[Float] )(using o: Overloaded1 , c: SourceContext): Rep[Float]  = float_minus(unit(a), b)
  def infix_-(a: Float      , b: Rep[Double])(using o: Overloaded2 , c: SourceContext): Rep[Double] = double_minus(unit(a), b)
  def infix_-(a: Double     , b: Rep[Int]   )(using                  c: SourceContext): Rep[Double] = double_minus(unit(a),b)
  def infix_-(a: Double     , b: Rep[Float] )(using o: Overloaded1 , c: SourceContext): Rep[Double] = double_minus(unit(a),b)
  def infix_-(a: Double     , b: Rep[Double])(using o: Overloaded2 , c: SourceContext): Rep[Double] = double_minus(unit(a),b)
  def infix_-(a: Rep[Int]   , b: Int        )(using                  c: SourceContext): Rep[Int]    = int_minus(a, unit(b))
  def infix_-(a: Rep[Int]   , b: Double     )(using                  c: SourceContext): Rep[Double] = double_minus(a, unit(b))
  def infix_-(a: Rep[Int]   , b: Float      )(using                  c: SourceContext): Rep[Float]  = float_minus(a, unit(b))
  def infix_-(a: Rep[Float] , b: Int        )(using o: Overloaded1 , c: SourceContext): Rep[Float]  = float_minus(a, unit(b.toFloat))
  def infix_-(a: Rep[Float] , b: Float      )(using o: Overloaded2 , c: SourceContext): Rep[Float]  = float_minus(a, unit(b))
  def infix_-(a: Rep[Float] , b: Double     )(using o: Overloaded3 , c: SourceContext): Rep[Double] = double_minus(a, unit(b))
  def infix_-(a: Rep[Double], b: Int        )(using o: Overloaded4 , c: SourceContext): Rep[Double] = double_minus(a, unit(b))
  def infix_-(a: Rep[Double], b: Float      )(using o: Overloaded5 , c: SourceContext): Rep[Double] = double_minus(a, unit(b))
  def infix_-(a: Rep[Double], b: Double     )(using o: Overloaded6 , c: SourceContext): Rep[Double] = double_minus(a, unit(b))
  def infix_-(a: Rep[Int]   , b: Rep[Int]   )(using o: Overloaded1 , c: SourceContext): Rep[Int]    = int_minus(a, b)
  def infix_-(a: Rep[Int]   , b: Rep[Float] )(using o: Overloaded2 , c: SourceContext): Rep[Float]  = float_minus(repIntToRepFloat(a), b)
  def infix_-(a: Rep[Int]   , b: Rep[Double])(using o: Overloaded3 , c: SourceContext): Rep[Double] = double_minus(repIntToRepDouble(a), b)
  def infix_-(a: Rep[Float] , b: Rep[Int]   )(using o: Overloaded4 , c: SourceContext): Rep[Float]  = float_minus(a,repIntToRepFloat(b))
  def infix_-(a: Rep[Float] , b: Rep[Float] )(using o: Overloaded5 , c: SourceContext): Rep[Float]  = float_minus(a, b)
  def infix_-(a: Rep[Float] , b: Rep[Double])(using o: Overloaded6 , c: SourceContext): Rep[Double] = double_minus(repFloatToRepDouble(a), b)
  def infix_-(a: Rep[Double], b: Rep[Int]   )(using o: Overloaded7 , c: SourceContext): Rep[Double] = double_minus(a,repIntToRepDouble(b))
  def infix_-(a: Rep[Double], b: Rep[Float] )(using o: Overloaded8 , c: SourceContext): Rep[Double] = double_minus(a,repFloatToRepDouble(b))
  def infix_-(a: Rep[Double], b: Rep[Double])(using o: Overloaded9 , c: SourceContext): Rep[Double] = double_minus(a,b)

  def infix_+(a: Int        , b: Rep[Int]   )(using                  c: SourceContext): Rep[Int]    = int_plus(unit(a), b)
  def infix_+(a: Int        , b: Rep[Float] )(using o: Overloaded1 , c: SourceContext): Rep[Float]  = float_plus(unit(a.toFloat), b)
  def infix_+(a: Int        , b: Rep[Double])(using o: Overloaded2 , c: SourceContext): Rep[Double] = double_plus(unit(a), b)
  def infix_+(a: Float      , b: Rep[Int]   )(using                  c: SourceContext): Rep[Float]  = float_plus(unit(a),b)
  def infix_+(a: Float      , b: Rep[Float] )(using o: Overloaded1 , c: SourceContext): Rep[Float]  = float_plus(unit(a), b)
  def infix_+(a: Float      , b: Rep[Double])(using o: Overloaded2 , c: SourceContext): Rep[Double] = double_plus(unit(a), b)
  def infix_+(a: Double     , b: Rep[Int]   )(using                  c: SourceContext): Rep[Double] = double_plus(unit(a),b)
  def infix_+(a: Double     , b: Rep[Float] )(using o: Overloaded1 , c: SourceContext): Rep[Double] = double_plus(unit(a),b)
  def infix_+(a: Double     , b: Rep[Double])(using o: Overloaded2 , c: SourceContext): Rep[Double] = double_plus(unit(a),b)
  def infix_+(a: Rep[Int]   , b: Int        )(using                  c: SourceContext): Rep[Int]    = int_plus(a, unit(b))
  def infix_+(a: Rep[Int]   , b: Double     )(using                  c: SourceContext): Rep[Double] = double_plus(a, unit(b))
  def infix_+(a: Rep[Int]   , b: Float      )(using                  c: SourceContext): Rep[Float]  = float_plus(a, unit(b))
  def infix_+(a: Rep[Float] , b: Int        )(using o: Overloaded1 , c: SourceContext): Rep[Float]  = float_plus(a, unit(b.toFloat))
  def infix_+(a: Rep[Float] , b: Float      )(using o: Overloaded2 , c: SourceContext): Rep[Float]  = float_plus(a, unit(b))
  def infix_+(a: Rep[Float] , b: Double     )(using o: Overloaded3 , c: SourceContext): Rep[Double] = double_plus(a, unit(b))
  def infix_+(a: Rep[Double], b: Int        )(using o: Overloaded4 , c: SourceContext): Rep[Double] = double_plus(a, unit(b))
  def infix_+(a: Rep[Double], b: Float      )(using o: Overloaded5 , c: SourceContext): Rep[Double] = double_plus(a, unit(b))
  def infix_+(a: Rep[Double], b: Double     )(using o: Overloaded6 , c: SourceContext): Rep[Double] = double_plus(a, unit(b))
  def infix_+(a: Rep[Int]   , b: Rep[Int]   )(using o: Overloaded15, c: SourceContext): Rep[Int]    = int_plus(a, b)
  def infix_+(a: Rep[Int]   , b: Rep[Float] )(using o: Overloaded16, c: SourceContext): Rep[Float]  = float_plus(repIntToRepFloat(a), b)
  def infix_+(a: Rep[Int]   , b: Rep[Double])(using o: Overloaded17, c: SourceContext): Rep[Double] = double_plus(repIntToRepDouble(a), b)
  def infix_+(a: Rep[Float] , b: Rep[Int]   )(using o: Overloaded18, c: SourceContext): Rep[Float]  = float_plus(a,repIntToRepFloat(b))
  def infix_+(a: Rep[Float] , b: Rep[Float] )(using o: Overloaded19, c: SourceContext): Rep[Float]  = float_plus(a, b)
  def infix_+(a: Rep[Float] , b: Rep[Double])(using o: Overloaded20, c: SourceContext): Rep[Double] = double_plus(repFloatToRepDouble(a), b)
  def infix_+(a: Rep[Double], b: Rep[Int]   )(using o: Overloaded21, c: SourceContext): Rep[Double] = double_plus(a,repIntToRepDouble(b))
  def infix_+(a: Rep[Double], b: Rep[Float] )(using o: Overloaded22, c: SourceContext): Rep[Double] = double_plus(a,repFloatToRepDouble(b))
  def infix_+(a: Rep[Double], b: Rep[Double])(using o: Overloaded23, c: SourceContext): Rep[Double] = double_plus(a,b)

  def infix_*(a: Int        , b: Rep[Int]   )(using                  c: SourceContext): Rep[Int]    = int_times(unit(a), b)
  def infix_*(a: Int        , b: Rep[Float] )(using o: Overloaded1 , c: SourceContext): Rep[Float]  = float_times(unit(a.toFloat), b)
  def infix_*(a: Int        , b: Rep[Double])(using o: Overloaded2 , c: SourceContext): Rep[Double] = double_times(unit(a), b)
  def infix_*(a: Float      , b: Rep[Int]   )(using                  c: SourceContext): Rep[Float]  = float_times(unit(a),b)
  def infix_*(a: Float      , b: Rep[Float] )(using o: Overloaded1 , c: SourceContext): Rep[Float]  = float_times(unit(a), b)
  def infix_*(a: Float      , b: Rep[Double])(using o: Overloaded2 , c: SourceContext): Rep[Double] = double_times(unit(a), b)
  def infix_*(a: Double     , b: Rep[Int]   )(using                  c: SourceContext): Rep[Double] = double_times(unit(a),b)
  def infix_*(a: Double     , b: Rep[Float] )(using o: Overloaded1 , c: SourceContext): Rep[Double] = double_times(unit(a),b)
  def infix_*(a: Double     , b: Rep[Double])(using o: Overloaded2 , c: SourceContext): Rep[Double] = double_times(unit(a),b)
  def infix_*(a: Rep[Int]   , b: Int        )(using                  c: SourceContext): Rep[Int]    = int_times(a, unit(b))
  def infix_*(a: Rep[Int]   , b: Double     )(using                  c: SourceContext): Rep[Double] = double_times(a, unit(b))
  def infix_*(a: Rep[Int]   , b: Float      )(using                  c: SourceContext): Rep[Float]  = float_times(a, unit(b))
  def infix_*(a: Rep[Float] , b: Int        )(using o: Overloaded1 , c: SourceContext): Rep[Float]  = float_times(a, unit(b.toFloat))
  def infix_*(a: Rep[Float] , b: Float      )(using o: Overloaded2 , c: SourceContext): Rep[Float]  = float_times(a, unit(b))
  def infix_*(a: Rep[Float] , b: Double     )(using o: Overloaded3 , c: SourceContext): Rep[Double] = double_times(a, unit(b))
  def infix_*(a: Rep[Double], b: Int        )(using o: Overloaded4 , c: SourceContext): Rep[Double] = double_times(a, unit(b))
  def infix_*(a: Rep[Double], b: Float      )(using o: Overloaded5 , c: SourceContext): Rep[Double] = double_times(a, unit(b))
  def infix_*(a: Rep[Double], b: Double     )(using o: Overloaded6 , c: SourceContext): Rep[Double] = double_times(a, unit(b))
  def infix_*(a: Rep[Int]   , b: Rep[Int]   )(using o: Overloaded1 , c: SourceContext): Rep[Int]    = int_times(a, b)
  def infix_*(a: Rep[Int]   , b: Rep[Float] )(using o: Overloaded2 , c: SourceContext): Rep[Float]  = float_times(repIntToRepFloat(a), b)
  def infix_*(a: Rep[Int]   , b: Rep[Double])(using o: Overloaded3 , c: SourceContext): Rep[Double] = double_times(repIntToRepDouble(a), b)
  def infix_*(a: Rep[Float] , b: Rep[Int]   )(using o: Overloaded4 , c: SourceContext): Rep[Float]  = float_times(a,repIntToRepFloat(b))
  def infix_*(a: Rep[Float] , b: Rep[Float] )(using o: Overloaded5 , c: SourceContext): Rep[Float]  = float_times(a, b)
  def infix_*(a: Rep[Float] , b: Rep[Double])(using o: Overloaded6 , c: SourceContext): Rep[Double] = double_times(repFloatToRepDouble(a), b)
  def infix_*(a: Rep[Double], b: Rep[Int]   )(using o: Overloaded7 , c: SourceContext): Rep[Double] = double_times(a,repIntToRepDouble(b))
  def infix_*(a: Rep[Double], b: Rep[Float] )(using o: Overloaded8 , c: SourceContext): Rep[Double] = double_times(a,repFloatToRepDouble(b))
  def infix_*(a: Rep[Double], b: Rep[Double])(using o: Overloaded9 , c: SourceContext): Rep[Double] = double_times(a,b)

  def infix_/(a: Int        , b: Rep[Int]   )(using                  c: SourceContext): Rep[Int]    = int_divide(unit(a), b)
  def infix_/(a: Int        , b: Rep[Float] )(using o: Overloaded1 , c: SourceContext): Rep[Float]  = float_divide(unit(a.toFloat), b)
  def infix_/(a: Int        , b: Rep[Double])(using o: Overloaded2 , c: SourceContext): Rep[Double] = double_divide(unit(a), b)
  def infix_/(a: Float      , b: Rep[Int]   )(using                  c: SourceContext): Rep[Float]  = float_divide(unit(a),b)
  def infix_/(a: Float      , b: Rep[Float] )(using o: Overloaded1 , c: SourceContext): Rep[Float]  = float_divide(unit(a), b)
  def infix_/(a: Float      , b: Rep[Double])(using o: Overloaded2 , c: SourceContext): Rep[Double] = double_divide(unit(a), b)
  def infix_/(a: Double     , b: Rep[Int]   )(using                  c: SourceContext): Rep[Double] = double_divide(unit(a),b)
  def infix_/(a: Double     , b: Rep[Float] )(using o: Overloaded1 , c: SourceContext): Rep[Double] = double_divide(unit(a),b)
  def infix_/(a: Double     , b: Rep[Double])(using o: Overloaded2 , c: SourceContext): Rep[Double] = double_divide(unit(a),b)
  def infix_/(a: Rep[Int]   , b: Int        )(using                  c: SourceContext): Rep[Int]    = int_divide(a, unit(b))
  def infix_/(a: Rep[Int]   , b: Double     )(using                  c: SourceContext): Rep[Double] = double_divide(a, unit(b))
  def infix_/(a: Rep[Int]   , b: Float      )(using                  c: SourceContext): Rep[Float]  = float_divide(a, unit(b))
  def infix_/(a: Rep[Float] , b: Int        )(using o: Overloaded1 , c: SourceContext): Rep[Float]  = float_divide(a, unit(b.toFloat))
  def infix_/(a: Rep[Float] , b: Float      )(using o: Overloaded2 , c: SourceContext): Rep[Float]  = float_divide(a, unit(b))
  def infix_/(a: Rep[Float] , b: Double     )(using o: Overloaded3 , c: SourceContext): Rep[Double] = double_divide(a, unit(b))
  def infix_/(a: Rep[Double], b: Int        )(using o: Overloaded4 , c: SourceContext): Rep[Double] = double_divide(a, unit(b))
  def infix_/(a: Rep[Double], b: Float      )(using o: Overloaded5 , c: SourceContext): Rep[Double] = double_divide(a, unit(b))
  def infix_/(a: Rep[Double], b: Double     )(using o: Overloaded6 , c: SourceContext): Rep[Double] = double_divide(a, unit(b))
  def infix_/(a: Rep[Int]   , b: Rep[Int]   )(using o: Overloaded1 , c: SourceContext): Rep[Int]    = int_divide(a, b)
  def infix_/(a: Rep[Int]   , b: Rep[Float] )(using o: Overloaded2 , c: SourceContext): Rep[Float]  = float_divide(repIntToRepFloat(a), b)
  def infix_/(a: Rep[Int]   , b: Rep[Double])(using o: Overloaded3 , c: SourceContext): Rep[Double] = double_divide(repIntToRepDouble(a), b)
  def infix_/(a: Rep[Float] , b: Rep[Int]   )(using o: Overloaded4 , c: SourceContext): Rep[Float]  = float_divide(a,repIntToRepFloat(b))
  def infix_/(a: Rep[Float] , b: Rep[Float] )(using o: Overloaded5 , c: SourceContext): Rep[Float]  = float_divide(a, b)
  def infix_/(a: Rep[Float] , b: Rep[Double])(using o: Overloaded6 , c: SourceContext): Rep[Double] = double_divide(repFloatToRepDouble(a), b)
  def infix_/(a: Rep[Double], b: Rep[Int]   )(using o: Overloaded7 , c: SourceContext): Rep[Double] = double_divide(a,repIntToRepDouble(b))
  def infix_/(a: Rep[Double], b: Rep[Float] )(using o: Overloaded8 , c: SourceContext): Rep[Double] = double_divide(a,repFloatToRepDouble(b))
  def infix_/(a: Rep[Double], b: Rep[Double])(using o: Overloaded9 , c: SourceContext): Rep[Double] = double_divide(a,b)

  /**
   *  Double
   */
  given doubleToDoubleOps: Conversion[Double, DoubleOpsCls] =   (n: Double) => new DoubleOpsCls(unit(n))
  given repDoubleToDoubleOps: Conversion[Rep[Double], DoubleOpsCls] = (n: Rep[Double]) => new DoubleOpsCls(n)
  implicit def varDoubleToDoubleOps (n: Var[Double]): DoubleOpsCls = new DoubleOpsCls(readVar(n))
  
  object Double {
    def parseDouble(s: Rep[String])(using pos: SourceContext): Rep[Double] = obj_double_parse_double(s)
    def PositiveInfinity           (using pos: SourceContext): Rep[Double] = obj_double_positive_infinity
    def NegativeInfinity           (using pos: SourceContext): Rep[Double] = obj_double_negative_infinity
    def MinValue                   (using pos: SourceContext): Rep[Double] = obj_double_min_value
    def MaxValue                   (using pos: SourceContext): Rep[Double] = obj_double_max_value
  }

  class DoubleOpsCls(lhs: Rep[Double]){
    def floatValue()(using pos: SourceContext): Rep[Float] = double_float_value(lhs)
    def toInt       (using pos: SourceContext): Rep[Int]   = double_to_int(lhs)
    def toFloat     (using pos: SourceContext): Rep[Float] = double_to_float(lhs)
    
    def +(rhs: Rep[Double])(using o1: Overloaded1): Rep[Double] = infix_+(lhs, rhs)
    def -(rhs: Rep[Double])(using o1: Overloaded1): Rep[Double] = infix_-(lhs, rhs)
    def *(rhs: Rep[Double])(using o1: Overloaded1): Rep[Double] = infix_*(lhs, rhs)
    def /(rhs: Rep[Double])(using o1: Overloaded1): Rep[Double] = infix_/(lhs, rhs)
  }

  def obj_double_parse_double(s: Rep[String])(using pos: SourceContext): Rep[Double]
  def obj_double_positive_infinity(using pos: SourceContext): Rep[Double]
  def obj_double_negative_infinity(using pos: SourceContext): Rep[Double]
  def obj_double_min_value        (using pos: SourceContext): Rep[Double]
  def obj_double_max_value        (using pos: SourceContext): Rep[Double]
  def double_float_value(lhs: Rep[Double])(using pos: SourceContext): Rep[Float]
  def double_to_int     (lhs: Rep[Double])(using pos: SourceContext): Rep[Int]
  def double_to_float   (lhs: Rep[Double])(using pos: SourceContext): Rep[Float]
  def double_plus     (lhs: Rep[Double], rhs: Rep[Double])(using pos: SourceContext): Rep[Double]
  def double_minus    (lhs: Rep[Double], rhs: Rep[Double])(using pos: SourceContext): Rep[Double]
  def double_times    (lhs: Rep[Double], rhs: Rep[Double])(using pos: SourceContext): Rep[Double]
  def double_divide   (lhs: Rep[Double], rhs: Rep[Double])(using pos: SourceContext): Rep[Double]


  /**
   * Float
   */
  object Float {
    def parseFloat(s: Rep[String])(using pos: SourceContext): Rep[Float] = obj_float_parse_float(s)
  }

  given floatToFloatOps: Conversion[Float, FloatOpsCls] = (n: Float) => new FloatOpsCls(unit(n))
  given repFloatToFloatOps: Conversion[Rep[Float], FloatOpsCls] = (n: Rep[Float]) => new FloatOpsCls(n)
  implicit def varFloatToFloatOps(n: Var[Float]): FloatOpsCls = new FloatOpsCls(readVar(n))

  class FloatOpsCls(lhs: Rep[Float]) {
    def toInt(using pos: SourceContext): Rep[Int] = float_to_int(lhs)
    def toDouble(using pos: SourceContext): Rep[Double] = float_to_double(lhs)

    def +(rhs: Rep[Float])(using o1: Overloaded1): Rep[Float] = infix_+(lhs, rhs)
    def -(rhs: Rep[Float])(using o1: Overloaded1): Rep[Float] = infix_-(lhs, rhs)
    def *(rhs: Rep[Float])(using o1: Overloaded1): Rep[Float] = infix_*(lhs, rhs)
    def /(rhs: Rep[Float])(using o1: Overloaded1): Rep[Float] = infix_/(lhs, rhs)
  }

  def infix_toInt(lhs: Rep[Float])(using o: Overloaded1, pos: SourceContext): Rep[Int] = float_to_int(lhs)
  def infix_toDouble(lhs: Rep[Float])(using o: Overloaded1, pos: SourceContext): Rep[Double] = float_to_double(lhs)
  
  def obj_float_parse_float(s: Rep[String])(using pos: SourceContext): Rep[Float]
  def float_to_int    (lhs: Rep[Float])(using pos: SourceContext): Rep[Int]
  def float_to_double (lhs: Rep[Float])(using pos: SourceContext): Rep[Double]
  def float_plus      (lhs: Rep[Float], rhs: Rep[Float])(using pos: SourceContext): Rep[Float]
  def float_minus     (lhs: Rep[Float], rhs: Rep[Float])(using pos: SourceContext): Rep[Float]
  def float_times     (lhs: Rep[Float], rhs: Rep[Float])(using pos: SourceContext): Rep[Float]
  def float_divide    (lhs: Rep[Float], rhs: Rep[Float])(using pos: SourceContext): Rep[Float]

  /**
   * Int
   */
  object Integer {
    def parseInt(s: Rep[String])(using pos: SourceContext): Rep[Int] = obj_integer_parse_int(s)
  }

  object Int {
    def MaxValue(using pos: SourceContext): Rep[Int] = obj_int_max_value
    def MinValue(using pos: SourceContext): Rep[Int] = obj_int_min_value
  }

  implicit def intToIntOps    (n: Int)     : IntOpsCls = new IntOpsCls(unit(n))
  implicit def repIntToIntOps (n: Rep[Int]): IntOpsCls = new IntOpsCls(n)
  implicit def varIntToIntOps (n: Var[Int]): IntOpsCls = new IntOpsCls(readVar(n))
    
  class IntOpsCls(lhs: Rep[Int]){
    // TODO (tiark): either of these cause scalac to crash        
    //def /[A](rhs: Rep[A])(using mA: Typ[A], f: Fractional[A], o: Overloaded1) = int_divide_frac(lhs, rhs)
    //def /(rhs: Rep[Int]) = int_divide(lhs, rhs)
    // TODO Something is wrong if we just use floatValue. implicits get confused
    def floatValueL ()(using pos: SourceContext): Rep[Float]  = int_float_value(lhs)
    def doubleValue ()(using pos: SourceContext): Rep[Double] = int_double_value(lhs)
    def unary_~     (using pos: SourceContext): Rep[Int]    = int_bitwise_not(lhs)
    def toLong        (using pos: SourceContext): Rep[Long]   = int_tolong(lhs)
    def toDouble      (using pos: SourceContext): Rep[Double] = int_to_double(lhs)
    def toFloat       (using pos: SourceContext): Rep[Float]  = int_to_float(lhs)
    def %(rhs: Rep[Int])(using o: Overloaded1, pos: SourceContext): Rep[Int] = int_mod(lhs, rhs)
    def &(rhs: Rep[Int])(using o: Overloaded1, pos: SourceContext): Rep[Int] = int_binaryand(lhs, rhs)
    def |(rhs: Rep[Int])(using o: Overloaded1, pos: SourceContext): Rep[Int] = int_binaryor(lhs, rhs)
    def ^(rhs: Rep[Int])(using o: Overloaded1, pos: SourceContext): Rep[Int] = int_binaryxor(lhs, rhs)
    def <<(rhs: Rep[Int])(using o: Overloaded1, pos: SourceContext): Rep[Int] = int_leftshift(lhs, rhs)
    def >>(rhs: Rep[Int])(using o: Overloaded1, pos: SourceContext): Rep[Int] = int_rightshiftarith(lhs, rhs)
    def >>>(rhs: Rep[Int])(using o: Overloaded1, pos: SourceContext): Rep[Int] = int_rightshiftlogical(lhs, rhs)
  }

  def infix_%  (lhs: Rep[Int], rhs: Rep[Int])(using o: Overloaded1, pos: SourceContext): Rep[Int] = int_mod(lhs, rhs)
  def infix_&  (lhs: Rep[Int], rhs: Rep[Int])(using o: Overloaded1, pos: SourceContext): Rep[Int] = int_binaryand(lhs, rhs)
  def infix_|  (lhs: Rep[Int], rhs: Rep[Int])(using o: Overloaded1, pos: SourceContext): Rep[Int] = int_binaryor(lhs, rhs)
  def infix_^  (lhs: Rep[Int], rhs: Rep[Int])(using o: Overloaded1, pos: SourceContext): Rep[Int] = int_binaryxor(lhs, rhs)
  def infix_<< (lhs: Rep[Int], rhs: Rep[Int])(using o: Overloaded1, pos: SourceContext): Rep[Int] = int_leftshift(lhs, rhs)
  def infix_>> (lhs: Rep[Int], rhs: Rep[Int])(using o: Overloaded1, pos: SourceContext): Rep[Int] = int_rightshiftarith(lhs, rhs)
  def infix_>>>(lhs: Rep[Int], rhs: Rep[Int])(using o: Overloaded1, pos: SourceContext): Rep[Int] = int_rightshiftlogical(lhs, rhs)

  def obj_integer_parse_int(s: Rep[String])(using pos: SourceContext): Rep[Int]
  def obj_int_max_value(using pos: SourceContext): Rep[Int]
  def obj_int_min_value(using pos: SourceContext): Rep[Int]

  def int_float_value       (lhs: Rep[Int])(using pos: SourceContext): Rep[Float]
  def int_double_value      (lhs: Rep[Int])(using pos: SourceContext): Rep[Double]
  def int_bitwise_not       (lhs: Rep[Int])(using pos: SourceContext): Rep[Int]
  def int_tolong            (lhs: Rep[Int])(using pos: SourceContext): Rep[Long]
  def int_to_float          (lhs: Rep[Int])(using pos: SourceContext): Rep[Float]
  def int_to_double         (lhs: Rep[Int])(using pos: SourceContext): Rep[Double]

  def int_plus              (lhs: Rep[Int], rhs: Rep[Int])(using pos: SourceContext): Rep[Int]
  def int_minus             (lhs: Rep[Int], rhs: Rep[Int])(using pos: SourceContext): Rep[Int]
  def int_times             (lhs: Rep[Int], rhs: Rep[Int])(using pos: SourceContext): Rep[Int]
  def int_divide            (lhs: Rep[Int], rhs: Rep[Int])(using pos: SourceContext): Rep[Int]
  def int_mod               (lhs: Rep[Int], rhs: Rep[Int])(using pos: SourceContext): Rep[Int]
  def int_binaryor          (lhs: Rep[Int], rhs: Rep[Int])(using pos: SourceContext): Rep[Int]
  def int_binaryand         (lhs: Rep[Int], rhs: Rep[Int])(using pos: SourceContext): Rep[Int]
  def int_binaryxor         (lhs: Rep[Int], rhs: Rep[Int])(using pos: SourceContext): Rep[Int]
  def int_leftshift         (lhs: Rep[Int], rhs: Rep[Int])(using pos: SourceContext): Rep[Int]
  def int_rightshiftarith   (lhs: Rep[Int], rhs: Rep[Int])(using pos: SourceContext): Rep[Int]
  def int_rightshiftlogical (lhs: Rep[Int], rhs: Rep[Int])(using pos: SourceContext): Rep[Int]
  // def int_divide_frac[A:Typ:Fractional](lhs: Rep[Int], rhs: Rep[A])(using pos: SourceContext): Rep[A]

  /**
   * Long
   */
  object Long {
    def parseLong(s: Rep[String])(using pos: SourceContext) = obj_long_parse_long(s)
  }

  given longToLongOps: Conversion[Long, LongOpsCls] = (n: Long) => new LongOpsCls(unit(n))
  given repLongToLongOps: Conversion[Rep[Long], LongOpsCls] = (n: Rep[Long]) => new LongOpsCls(n)
  implicit def varLongToLongOps(n: Var[Long]): LongOpsCls = new LongOpsCls(readVar(n))

  class LongOpsCls(lhs: Rep[Long]) {
    def toInt(using pos: SourceContext): Rep[Int] = long_toint(lhs)
    def unary_~(using pos: SourceContext): Rep[Long] = long_bitwise_not(lhs)

    def +(rhs: Rep[Long])(using o1: Overloaded1): Rep[Long] = long_plus(lhs, rhs)
    def -(rhs: Rep[Long])(using o1: Overloaded1): Rep[Long] = long_minus(lhs, rhs)
    def *(rhs: Rep[Long])(using o1: Overloaded1): Rep[Long] = long_times(lhs, rhs)
    def /(rhs: Rep[Long])(using o1: Overloaded1): Rep[Long] = long_divide(lhs, rhs)
    def %(rhs: Rep[Long])(using o1: Overloaded1): Rep[Long] = long_mod(lhs, rhs)
    def &(rhs: Rep[Long])(using o1: Overloaded1): Rep[Long] = long_binaryand(lhs, rhs)
    def |(rhs: Rep[Long])(using o1: Overloaded1): Rep[Long] = long_binaryor(lhs, rhs)
    def ^(rhs: Rep[Long])(using o1: Overloaded1): Rep[Long] = long_binaryxor(lhs, rhs)
    def <<(rhs: Rep[Int])(using o1: Overloaded1): Rep[Long] = long_shiftleft(lhs, rhs)
    def >>(rhs: Rep[Int])(using o1: Overloaded1): Rep[Long] = long_shiftright_arith(lhs, rhs)
    def >>>(rhs: Rep[Int])(using o1: Overloaded1): Rep[Long] = long_shiftright_unsigned(lhs, rhs)
  }

  def infix_toInt(lhs: Rep[Long])(using o: Overloaded2, pos: SourceContext): Rep[Int] = long_toint(lhs)

  def infix_%   (lhs: Rep[Long], rhs: Rep[Long])(using o: Overloaded2, pos: SourceContext): Rep[Long] = long_mod(lhs, rhs)
  def infix_&   (lhs: Rep[Long], rhs: Rep[Long])(using o: Overloaded2, pos: SourceContext): Rep[Long] = long_binaryand(lhs, rhs)
  def infix_|   (lhs: Rep[Long], rhs: Rep[Long])(using o: Overloaded2, pos: SourceContext): Rep[Long] = long_binaryor(lhs, rhs)
  def infix_^   (lhs: Rep[Long], rhs: Rep[Long])(using o: Overloaded2, pos: SourceContext): Rep[Long] = long_binaryxor(lhs, rhs)
  def infix_<<  (lhs: Rep[Long], rhs: Rep[Int] )(using o: Overloaded2, pos: SourceContext): Rep[Long] = long_shiftleft(lhs, rhs)
  def infix_>>  (lhs: Rep[Long], rhs: Rep[Int] )(using o: Overloaded2, pos: SourceContext): Rep[Long] = long_shiftright_arith(lhs, rhs)
  def infix_>>> (lhs: Rep[Long], rhs: Rep[Int] )(using o: Overloaded2, pos: SourceContext): Rep[Long] = long_shiftright_unsigned(lhs, rhs)

  def obj_long_parse_long(s: Rep[String])(using pos: SourceContext): Rep[Long]
  def long_toint         (lhs: Rep[Long])(using pos: SourceContext): Rep[Int]

  def long_plus               (lhs: Rep[Long], rhs: Rep[Long])(using pos: SourceContext): Rep[Long]
  def long_minus              (lhs: Rep[Long], rhs: Rep[Long])(using pos: SourceContext): Rep[Long]
  def long_times              (lhs: Rep[Long], rhs: Rep[Long])(using pos: SourceContext): Rep[Long]
  def long_divide             (lhs: Rep[Long], rhs: Rep[Long])(using pos: SourceContext): Rep[Long]
  def long_mod                (lhs: Rep[Long], rhs: Rep[Long])(using pos: SourceContext): Rep[Long]
  def long_bitwise_not        (lhs: Rep[Long])(using pos: SourceContext): Rep[Long]
  def long_binaryand          (lhs: Rep[Long], rhs: Rep[Long])(using pos: SourceContext): Rep[Long]
  def long_binaryor           (lhs: Rep[Long], rhs: Rep[Long])(using pos: SourceContext): Rep[Long]
  def long_binaryxor          (lhs: Rep[Long], rhs: Rep[Long])(using pos: SourceContext): Rep[Long]
  def long_shiftleft          (lhs: Rep[Long], rhs: Rep[Int] )(using pos: SourceContext): Rep[Long]
  def long_shiftright_arith   (lhs: Rep[Long], rhs: Rep[Int] )(using pos: SourceContext): Rep[Long]
  def long_shiftright_unsigned(lhs: Rep[Long], rhs: Rep[Int] )(using pos: SourceContext): Rep[Long]
}

trait PrimitiveOpsExp extends PrimitiveOps with EffectExp {
  this: ImplicitOps =>
  
  override given byteTyp   : Typ[Byte]   = manifestTyp
  override given charTyp   : Typ[Char]   = manifestTyp
  override given shortTyp  : Typ[Short]  = manifestTyp
  override given intTyp    : Typ[Int]    = manifestTyp
  override given longTyp   : Typ[Long]   = manifestTyp
  override given floatTyp  : Typ[Float]  = manifestTyp
  override given doubleTyp : Typ[Double] = manifestTyp

  /**
   * Double
   */
  case class ObjDoubleParseDouble(s: Exp[String]) extends Def[Double]
  case class ObjDoublePositiveInfinity() extends Def[Double]
  case class ObjDoubleNegativeInfinity() extends Def[Double]
  case class ObjDoubleMinValue() extends Def[Double]
  case class ObjDoubleMaxValue() extends Def[Double]

  case class DoubleFloatValue(lhs: Exp[Double]) extends Def[Float]
  case class DoubleToInt     (lhs: Exp[Double]) extends Def[Int]
  case class DoubleToFloat   (lhs: Exp[Double]) extends Def[Float]
  
  trait ArithOp[T] extends Def[T] {
    val lhs: Exp[T]
    val rhs: Exp[T]
  }
  
  case class DoublePlus   (lhs: Exp[Double], rhs: Exp[Double]) extends ArithOp[Double]
  case class DoubleMinus  (lhs: Exp[Double], rhs: Exp[Double]) extends ArithOp[Double]
  case class DoubleTimes  (lhs: Exp[Double], rhs: Exp[Double]) extends ArithOp[Double]
  case class DoubleDivide (lhs: Exp[Double], rhs: Exp[Double]) extends ArithOp[Double]

  def obj_double_parse_double(s: Exp[String])(using pos: SourceContext): Exp[Double] = ObjDoubleParseDouble(s)
  def obj_double_positive_infinity(using pos: SourceContext) = ObjDoublePositiveInfinity()
  def obj_double_negative_infinity(using pos: SourceContext) = ObjDoubleNegativeInfinity()
  def obj_double_min_value        (using pos: SourceContext) = ObjDoubleMinValue()
  def obj_double_max_value        (using pos: SourceContext) = ObjDoubleMaxValue()
  def double_float_value  (lhs: Exp[Double])(using pos: SourceContext): Exp[Float] = DoubleFloatValue(lhs)
  def double_to_int       (lhs: Exp[Double])(using pos: SourceContext): Exp[Int]   = DoubleToInt(lhs)
  def double_to_float     (lhs: Exp[Double])(using pos: SourceContext): Exp[Float] = DoubleToFloat(lhs)
  def double_plus   (lhs: Exp[Double], rhs: Exp[Double])(using pos: SourceContext): Exp[Double] = DoublePlus(lhs,rhs)
  def double_minus  (lhs: Exp[Double], rhs: Exp[Double])(using pos: SourceContext): Exp[Double] = DoubleMinus(lhs,rhs)
  def double_times  (lhs: Exp[Double], rhs: Exp[Double])(using pos: SourceContext): Exp[Double] = DoubleTimes(lhs,rhs)
  def double_divide (lhs: Exp[Double], rhs: Exp[Double])(using pos: SourceContext): Exp[Double] = DoubleDivide(lhs,rhs)

  /**
   * Float
   */  
  case class ObjFloatParseFloat(str: Exp[String]) extends Def[Float]
  case class FloatToInt        (lhs: Exp[Float] ) extends Def[Int]
  case class FloatToDouble     (lhs: Exp[Float] ) extends Def[Double]

  case class FloatPlus  (lhs: Exp[Float], rhs: Exp[Float]) extends ArithOp[Float]
  case class FloatMinus (lhs: Exp[Float], rhs: Exp[Float]) extends ArithOp[Float]
  case class FloatTimes (lhs: Exp[Float], rhs: Exp[Float]) extends ArithOp[Float]
  case class FloatDivide(lhs: Exp[Float], rhs: Exp[Float]) extends ArithOp[Float]
  
  def obj_float_parse_float(s: Exp[String])(using pos: SourceContext): Exp[Float]  = ObjFloatParseFloat(s)
  def float_to_int        (lhs: Exp[Float])(using pos: SourceContext): Exp[Int]    = FloatToInt(lhs)
  def float_to_double     (lhs: Exp[Float])(using pos: SourceContext): Exp[Double] = FloatToDouble(lhs)

  def float_plus  (lhs: Exp[Float], rhs: Exp[Float])(using pos: SourceContext): Exp[Float] = FloatPlus(lhs,rhs)
  def float_minus (lhs: Exp[Float], rhs: Exp[Float])(using pos: SourceContext): Exp[Float] = FloatMinus(lhs,rhs)
  def float_times (lhs: Exp[Float], rhs: Exp[Float])(using pos: SourceContext): Exp[Float] = FloatTimes(lhs,rhs)
  def float_divide(lhs: Exp[Float], rhs: Exp[Float])(using pos: SourceContext): Exp[Float] = FloatDivide(lhs,rhs)

  /**
   * Int
   */
  case class ObjIntegerParseInt(s: Exp[String]) extends Def[Int]
  case class ObjIntMaxValue() extends Def[Int]
  case class ObjIntMinValue() extends Def[Int]

  case class IntFloatValue (lhs: Exp[Int]) extends Def[Float]
  case class IntBitwiseNot (lhs: Exp[Int]) extends Def[Int]
  case class IntToLong     (lhs: Exp[Int]) extends Def[Long]
  case class IntToFloat    (lhs: Exp[Int]) extends Def[Float]
  case class IntToDouble   (lhs: Exp[Int]) extends Def[Double]
  case class IntDoubleValue(lhs: Exp[Int]) extends Def[Double]

  case class IntPlus              (lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]
  case class IntMinus             (lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]
  case class IntTimes             (lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]
  case class IntDivide            (lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]
  case class IntMod               (lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]
  case class IntBinaryOr          (lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]
  case class IntBinaryAnd         (lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]
  case class IntBinaryXor         (lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]
  case class IntShiftLeft         (lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]
  case class IntShiftRightArith   (lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]
  case class IntShiftRightLogical (lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]

  def obj_integer_parse_int(s: Rep[String])(using pos: SourceContext): Exp[Int] = ObjIntegerParseInt(s)
  def obj_int_max_value(using pos: SourceContext) = ObjIntMaxValue()
  def obj_int_min_value(using pos: SourceContext) = ObjIntMinValue()

  def int_double_value (lhs: Exp[Int])(using pos: SourceContext): Exp[Double] = IntDoubleValue(lhs)
  def int_float_value  (lhs: Exp[Int])(using pos: SourceContext): Exp[Float]  = IntFloatValue(lhs)
  def int_bitwise_not  (lhs: Exp[Int])(using pos: SourceContext): Exp[Int]    = IntBitwiseNot(lhs)
  def int_tolong       (lhs: Exp[Int])(using pos: SourceContext): Exp[Long]   = IntToLong(lhs)
  def int_to_float     (lhs: Exp[Int])(using pos: SourceContext): Exp[Float]  = IntToFloat(lhs)
  def int_to_double    (lhs: Exp[Int])(using pos: SourceContext): Exp[Double] = IntToDouble(lhs)

  def int_plus              (lhs: Exp[Int], rhs: Exp[Int])(using pos: SourceContext): Exp[Int] = IntPlus(lhs,rhs)
  def int_minus             (lhs: Exp[Int], rhs: Exp[Int])(using pos: SourceContext): Exp[Int] = IntMinus(lhs, rhs)
  def int_times             (lhs: Exp[Int], rhs: Exp[Int])(using pos: SourceContext): Exp[Int] = IntTimes(lhs, rhs)
  def int_divide            (lhs: Exp[Int], rhs: Exp[Int])(using pos: SourceContext): Exp[Int] = IntDivide(lhs, rhs)
  def int_mod               (lhs: Exp[Int], rhs: Exp[Int])(using pos: SourceContext): Exp[Int] = IntMod(lhs, rhs)
  def int_binaryor          (lhs: Exp[Int], rhs: Exp[Int])(using pos: SourceContext): Exp[Int] = IntBinaryOr(lhs, rhs)
  def int_binaryand         (lhs: Exp[Int], rhs: Exp[Int])(using pos: SourceContext): Exp[Int] = IntBinaryAnd(lhs, rhs)
  def int_binaryxor         (lhs: Exp[Int], rhs: Exp[Int])(using pos: SourceContext): Exp[Int] = IntBinaryXor(lhs, rhs)
  def int_leftshift         (lhs: Exp[Int], rhs: Exp[Int])(using pos: SourceContext): Exp[Int] = IntShiftLeft(lhs, rhs)
  def int_rightshiftarith   (lhs: Exp[Int], rhs: Exp[Int])(using pos: SourceContext): Exp[Int] = IntShiftRightArith(lhs, rhs)
  def int_rightshiftlogical (lhs: Exp[Int], rhs: Exp[Int])(using pos: SourceContext): Exp[Int] = IntShiftRightLogical(lhs, rhs)

  /**
   * Long
   */
  case class ObjLongParseLong(s: Exp[String]) extends Def[Long]
  case class LongPlus(lhs: Exp[Long], rhs: Exp[Long]) extends ArithOp[Long]
  case class LongMinus(lhs: Exp[Long], rhs: Exp[Long]) extends ArithOp[Long]
  case class LongTimes(lhs: Exp[Long], rhs: Exp[Long]) extends ArithOp[Long]
  case class LongDivide(lhs: Exp[Long], rhs: Exp[Long]) extends ArithOp[Long]
  case class LongBinaryOr(lhs: Exp[Long], rhs: Exp[Long]) extends Def[Long]
  case class LongBinaryAnd(lhs: Exp[Long], rhs: Exp[Long]) extends Def[Long]
  case class LongBinaryXor(lhs: Exp[Long], rhs: Exp[Long]) extends Def[Long]
  case class LongBitwiseNot(lhs: Exp[Long]) extends Def[Long]
  case class LongShiftLeft(lhs: Exp[Long], rhs: Exp[Int]) extends Def[Long]
  case class LongShiftRightArith(lhs: Exp[Long], rhs: Exp[Int]) extends Def[Long]
  case class LongShiftRightUnsigned(lhs: Exp[Long], rhs: Exp[Int]) extends Def[Long]
  case class LongToInt(lhs: Exp[Long]) extends Def[Int]
  case class LongMod(lhs: Exp[Long], rhs: Exp[Long]) extends Def[Long]

  def obj_long_parse_long(s: Exp[String])(using pos: SourceContext) = ObjLongParseLong(s)
  def long_plus(lhs: Exp[Long], rhs: Exp[Long])(using pos: SourceContext) = LongPlus(lhs,rhs)
  def long_minus(lhs: Exp[Long], rhs: Exp[Long])(using pos: SourceContext) = LongMinus(lhs,rhs)
  def long_times(lhs: Exp[Long], rhs: Exp[Long])(using pos: SourceContext) = LongTimes(lhs,rhs)
  def long_divide(lhs: Exp[Long], rhs: Exp[Long])(using pos: SourceContext) = LongDivide(lhs,rhs)
  def long_binaryor(lhs: Exp[Long], rhs: Exp[Long])(using pos: SourceContext) = LongBinaryOr(lhs,rhs)
  def long_binaryand(lhs: Exp[Long], rhs: Exp[Long])(using pos: SourceContext) = LongBinaryAnd(lhs,rhs)
  def long_binaryxor(lhs: Exp[Long], rhs: Exp[Long])(using pos: SourceContext) = LongBinaryXor(lhs,rhs)
  def long_bitwise_not(lhs: Exp[Long])(using pos: SourceContext) = LongBitwiseNot(lhs)
  def long_shiftleft(lhs: Exp[Long], rhs: Exp[Int])(using pos: SourceContext) = LongShiftLeft(lhs,rhs)
  def long_shiftright_arith(lhs: Exp[Long], rhs: Exp[Int])(using pos: SourceContext) = LongShiftRightArith(lhs,rhs)
  def long_shiftright_unsigned(lhs: Exp[Long], rhs: Exp[Int])(using pos: SourceContext) = LongShiftRightUnsigned(lhs,rhs)
  def long_toint(lhs: Exp[Long])(using pos: SourceContext) = LongToInt(lhs)
  def long_mod    (lhs: Exp[Long], rhs: Exp[Long])(using pos: SourceContext) = LongMod(lhs, rhs)
    
  override def mirror[A:Typ](e: Def[A], f: Transformer)(using pos: SourceContext): Exp[A] = {
    implicit var a: Numeric[A] = null // hack!! need to store it in Def instances??
    val mirror = e match {
      case ObjDoubleParseDouble(x)      => obj_double_parse_double(f(x))
      case ObjDoublePositiveInfinity()  => obj_double_positive_infinity
      case ObjDoubleNegativeInfinity()  => obj_double_negative_infinity
      case ObjDoubleMinValue()          => obj_double_min_value
      case ObjDoubleMaxValue()          => obj_double_max_value
      case DoubleFloatValue(x)          => double_float_value(f(x))
      case DoubleToInt(x)               => double_to_int(f(x))
      case DoubleToFloat(x)             => double_to_float(f(x))
      case DoublePlus(x,y)              => double_plus(f(x),f(y))
      case DoubleMinus(x,y)             => double_minus(f(x),f(y))
      case DoubleTimes(x,y)             => double_times(f(x),f(y))
      case DoubleDivide(x,y)            => double_divide(f(x),f(y))
      case ObjFloatParseFloat(x)        => obj_float_parse_float(f(x))
      case FloatToInt(x)                => float_to_int(f(x))
      case FloatToDouble(x)             => float_to_double(f(x))
      case FloatPlus(x,y)               => float_plus(f(x),f(y))
      case FloatMinus(x,y)              => float_minus(f(x),f(y))
      case FloatTimes(x,y)              => float_times(f(x),f(y))
      case FloatDivide(x,y)             => float_divide(f(x),f(y))
      case ObjIntegerParseInt(x)        => obj_integer_parse_int(f(x))
      case ObjIntMaxValue()             => obj_int_max_value
      case ObjIntMinValue()             => obj_int_min_value
      case IntDoubleValue(x)            => int_double_value(f(x))
      case IntFloatValue(x)             => int_float_value(f(x))
      case IntBitwiseNot(x)             => int_bitwise_not(f(x))
      case IntPlus(x,y)                 => int_plus(f(x),f(y))
      case IntMinus(x,y)                => int_minus(f(x),f(y))
      case IntTimes(x,y)                => int_times(f(x),f(y))
      case IntDivide(x,y)               => int_divide(f(x),f(y))
      case IntMod(x,y)                  => int_mod(f(x),f(y))
      case IntBinaryOr(x,y)             => int_binaryor(f(x),f(y))
      case IntBinaryAnd(x,y)            => int_binaryand(f(x),f(y))
      case IntBinaryXor(x,y)            => int_binaryxor(f(x),f(y))
      case IntToLong(x)                 => int_tolong(f(x))
      case IntToFloat(x)                => int_to_float(f(x))
      case IntToDouble(x)               => int_to_double(f(x))
      case IntShiftLeft(x,y)            => int_leftshift(f(x),f(y))
      case IntShiftRightLogical(x,y)    => int_rightshiftlogical(f(x),f(y))
      case IntShiftRightArith(x,y)      => int_rightshiftarith(f(x),f(y))
      case ObjLongParseLong(x)          => obj_long_parse_long(f(x))
      case LongPlus(x,y)                => long_plus(f(x),f(y))
      case LongMinus(x,y)               => long_minus(f(x),f(y))
      case LongTimes(x,y)               => long_times(f(x),f(y))
      case LongDivide(x,y)              => long_divide(f(x),f(y))
      case LongMod(x,y)                 => long_mod(f(x),f(y))
      case LongShiftLeft(x,y)           => long_shiftleft(f(x),f(y))
      case LongShiftRightArith(x,y)     => long_shiftright_arith(f(x),f(y))
      case LongBinaryOr(x,y)            => long_binaryor(f(x),f(y))
      case LongBinaryAnd(x,y)           => long_binaryand(f(x),f(y))
      case LongBinaryXor(x,y)           => long_binaryxor(f(x),f(y))
      case LongBitwiseNot(x)            => long_bitwise_not(f(x))
      case LongToInt(x)                 => long_toint(f(x))
      case LongShiftRightUnsigned(x,y)  => long_shiftright_unsigned(f(x),f(y))

      case Reflect(ObjDoubleParseDouble(x)    , u, es) => reflectMirrored(Reflect(ObjDoubleParseDouble(f(x))        , mapOver(f,u), f(es)))(using mtyp1[A], pos)
      case Reflect(ObjDoublePositiveInfinity(), u, es) => reflectMirrored(Reflect(ObjDoublePositiveInfinity()       , mapOver(f,u), f(es)))(using mtyp1[A], pos)
      case Reflect(ObjDoubleNegativeInfinity(), u, es) => reflectMirrored(Reflect(ObjDoubleNegativeInfinity()       , mapOver(f,u), f(es)))(using mtyp1[A], pos)
      case Reflect(ObjDoubleMinValue()        , u, es) => reflectMirrored(Reflect(ObjDoubleMinValue()               , mapOver(f,u), f(es)))(using mtyp1[A], pos)
      case Reflect(ObjDoubleMaxValue()        , u, es) => reflectMirrored(Reflect(ObjDoubleMaxValue()               , mapOver(f,u), f(es)))(using mtyp1[A], pos)
      case Reflect(DoubleFloatValue(x)        , u, es) => reflectMirrored(Reflect(DoubleFloatValue(f(x))            , mapOver(f,u), f(es)))(using mtyp1[A], pos)
      case Reflect(DoubleToInt(x)             , u, es) => reflectMirrored(Reflect(DoubleToInt(f(x))                 , mapOver(f,u), f(es)))(using mtyp1[A], pos)
      case Reflect(DoubleToFloat(x)           , u, es) => reflectMirrored(Reflect(DoubleToFloat(f(x))               , mapOver(f,u), f(es)))(using mtyp1[A], pos)
      case Reflect(DoublePlus(x,y)            , u, es) => reflectMirrored(Reflect(DoublePlus(f(x),f(y))             , mapOver(f,u), f(es)))(using mtyp1[A], pos)
      case Reflect(DoubleMinus(x,y)           , u, es) => reflectMirrored(Reflect(DoubleMinus(f(x),f(y))            , mapOver(f,u), f(es)))(using mtyp1[A], pos)
      case Reflect(DoubleTimes(x,y)           , u, es) => reflectMirrored(Reflect(DoubleTimes(f(x),f(y))            , mapOver(f,u), f(es)))(using mtyp1[A], pos)
      case Reflect(DoubleDivide(x,y)          , u, es) => reflectMirrored(Reflect(DoubleDivide(f(x),f(y))           , mapOver(f,u), f(es)))(using mtyp1[A], pos)
      case Reflect(FloatToInt(x)              , u, es) => reflectMirrored(Reflect(FloatToInt(f(x))                  , mapOver(f,u), f(es)))(using mtyp1[A], pos)
      case Reflect(FloatToDouble(x)           , u, es) => reflectMirrored(Reflect(FloatToDouble(f(x))               , mapOver(f,u), f(es)))(using mtyp1[A], pos)
      case Reflect(FloatPlus(x,y)             , u, es) => reflectMirrored(Reflect(FloatPlus(f(x),f(y))              , mapOver(f,u), f(es)))(using mtyp1[A], pos)
      case Reflect(FloatMinus(x,y)            , u, es) => reflectMirrored(Reflect(FloatMinus(f(x),f(y))             , mapOver(f,u), f(es)))(using mtyp1[A], pos)
      case Reflect(FloatTimes(x,y)            , u, es) => reflectMirrored(Reflect(FloatTimes(f(x),f(y))             , mapOver(f,u), f(es)))(using mtyp1[A], pos)
      case Reflect(FloatDivide(x,y)           , u, es) => reflectMirrored(Reflect(FloatDivide(f(x),f(y))            , mapOver(f,u), f(es)))(using mtyp1[A], pos)
      case Reflect(ObjIntegerParseInt(x)      , u, es) => reflectMirrored(Reflect(ObjIntegerParseInt(f(x))          , mapOver(f,u), f(es)))(using mtyp1[A], pos)
      case Reflect(ObjIntMinValue()           , u, es) => reflectMirrored(Reflect(ObjIntMinValue()                  , mapOver(f,u), f(es)))(using mtyp1[A], pos)
      case Reflect(ObjIntMaxValue()           , u, es) => reflectMirrored(Reflect(ObjIntMaxValue()                  , mapOver(f,u), f(es)))(using mtyp1[A], pos)
      case Reflect(IntDoubleValue(x)          , u, es) => reflectMirrored(Reflect(IntDoubleValue(f(x))              , mapOver(f,u), f(es)))(using mtyp1[A], pos)
      case Reflect(IntFloatValue(x)           , u, es) => reflectMirrored(Reflect(IntFloatValue(f(x))               , mapOver(f,u), f(es)))(using mtyp1[A], pos)
      case Reflect(IntBitwiseNot(x)           , u, es) => reflectMirrored(Reflect(IntBitwiseNot(f(x))               , mapOver(f,u), f(es)))(using mtyp1[A], pos)
      case Reflect(IntPlus(x,y)               , u, es) => reflectMirrored(Reflect(IntPlus(f(x),f(y))                , mapOver(f,u), f(es)))(using mtyp1[A], pos)
      case Reflect(IntMinus(x,y)              , u, es) => reflectMirrored(Reflect(IntMinus(f(x),f(y))               , mapOver(f,u), f(es)))(using mtyp1[A], pos)
      case Reflect(IntTimes(x,y)              , u, es) => reflectMirrored(Reflect(IntTimes(f(x),f(y))               , mapOver(f,u), f(es)))(using mtyp1[A], pos)
      case Reflect(IntDivide(x,y)             , u, es) => reflectMirrored(Reflect(IntDivide(f(x),f(y))              , mapOver(f,u), f(es)))(using mtyp1[A], pos)
      case Reflect(IntMod(x,y)                , u, es) => reflectMirrored(Reflect(IntMod(f(x),f(y))                 , mapOver(f,u), f(es)))(using mtyp1[A], pos)
      case Reflect(IntBinaryOr(x,y)           , u, es) => reflectMirrored(Reflect(IntBinaryOr(f(x),f(y))            , mapOver(f,u), f(es)))(using mtyp1[A], pos)
      case Reflect(IntBinaryAnd(x,y)          , u, es) => reflectMirrored(Reflect(IntBinaryAnd(f(x),f(y))           , mapOver(f,u), f(es)))(using mtyp1[A], pos)
      case Reflect(IntBinaryXor(x,y)          , u, es) => reflectMirrored(Reflect(IntBinaryXor(f(x),f(y))           , mapOver(f,u), f(es)))(using mtyp1[A], pos)
      case Reflect(IntToLong(x)               , u, es) => reflectMirrored(Reflect(IntToLong(f(x))                   , mapOver(f,u), f(es)))(using mtyp1[A], pos)
      case Reflect(IntToFloat(x)              , u, es) => reflectMirrored(Reflect(IntToFloat(f(x))                  , mapOver(f,u), f(es)))(using mtyp1[A], pos)
      case Reflect(IntToDouble(x)             , u, es) => reflectMirrored(Reflect(IntToDouble(f(x))                 , mapOver(f,u), f(es)))(using mtyp1[A], pos)
      case Reflect(IntShiftLeft(x,y)          , u, es) => reflectMirrored(Reflect(IntShiftLeft(f(x),f(y))           , mapOver(f,u), f(es)))(using mtyp1[A], pos)
      case Reflect(IntShiftRightLogical(x,y)  , u, es) => reflectMirrored(Reflect(IntShiftRightLogical(f(x),f(y))   , mapOver(f,u), f(es)))(using mtyp1[A], pos)
      case Reflect(IntShiftRightArith(x,y)    , u, es) => reflectMirrored(Reflect(IntShiftRightArith(f(x),f(y))     , mapOver(f,u), f(es)))(using mtyp1[A], pos)
      case Reflect(LongPlus(x,y)              , u, es) => reflectMirrored(Reflect(LongPlus(f(x),f(y))               , mapOver(f,u), f(es)))(using mtyp1[A], pos)
      case Reflect(LongMinus(x,y)             , u, es) => reflectMirrored(Reflect(LongMinus(f(x),f(y))              , mapOver(f,u), f(es)))(using mtyp1[A], pos)
      case Reflect(LongTimes(x,y)             , u, es) => reflectMirrored(Reflect(LongTimes(f(x),f(y))              , mapOver(f,u), f(es)))(using mtyp1[A], pos)
      case Reflect(LongDivide(x,y)            , u, es) => reflectMirrored(Reflect(LongDivide(f(x),f(y))             , mapOver(f,u), f(es)))(using mtyp1[A], pos)
      case Reflect(LongMod(x,y)               , u, es) => reflectMirrored(Reflect(LongMod(f(x),f(y))                , mapOver(f,u), f(es)))(using mtyp1[A], pos)
      case Reflect(LongShiftLeft(x,y)         , u, es) => reflectMirrored(Reflect(LongShiftLeft(f(x),f(y))          , mapOver(f,u), f(es)))(using mtyp1[A], pos)
      case Reflect(LongShiftRightArith(x,y)   , u, es) => reflectMirrored(Reflect(LongShiftRightArith(f(x),f(y))    , mapOver(f,u), f(es)))(using mtyp1[A], pos)
      case Reflect(LongShiftRightUnsigned(x,y), u, es) => reflectMirrored(Reflect(LongShiftRightUnsigned(f(x),f(y)) , mapOver(f,u), f(es)))(using mtyp1[A], pos)
      case Reflect(LongBinaryOr(x,y)          , u, es) => reflectMirrored(Reflect(LongBinaryOr(f(x),f(y))           , mapOver(f,u), f(es)))(using mtyp1[A], pos)
      case Reflect(LongBinaryAnd(x,y)         , u, es) => reflectMirrored(Reflect(LongBinaryAnd(f(x),f(y))          , mapOver(f,u), f(es)))(using mtyp1[A], pos)
      case Reflect(LongBinaryXor(x,y)         , u, es) => reflectMirrored(Reflect(LongBinaryXor(f(x),f(y))          , mapOver(f,u), f(es)))(using mtyp1[A], pos)
      case Reflect(LongBitwiseNot(x)          , u, es) => reflectMirrored(Reflect(LongBitwiseNot(f(x))              , mapOver(f,u), f(es)))(using mtyp1[A], pos)
      case Reflect(LongToInt(x)               , u, es) => reflectMirrored(Reflect(LongToInt(f(x))                   , mapOver(f,u), f(es)))(using mtyp1[A], pos)
      case _ => super.mirror(e,f)
    }
    mirror.asInstanceOf[Exp[A]]
  }
}

trait PrimitiveOpsExpOpt extends PrimitiveOpsExp {


  /**
    * PrimitiveOps optimizations: Doubles
    * @author  Alen Stojanov (astojanov@inf.ethz.ch)
    */
  override def obj_double_parse_double(exp: Exp[String])(using pos: SourceContext): Exp[Double] = exp match {
    case Const(str) => unit(str.toDouble)
    case _ => super.obj_double_parse_double(exp)
  }
  override def double_float_value(exp: Exp[Double])(using pos: SourceContext): Exp[Float] = exp match {
    case Const(d) => unit(d.toFloat)
    case _ => super.double_float_value(exp)
  }
  override def double_to_int(lhs: Exp[Double])(using pos: SourceContext): Exp[Int] = lhs match {
    case Const(d) => unit(d.toInt)
    case Def(IntToDouble(x)) => x
    case _ => super.double_to_int(lhs)
  }
  override def double_to_float(lhs: Exp[Double])(using pos: SourceContext): Exp[Float] = lhs match {
    case Const(d) => unit(d.toFloat)
    case _ => super.double_to_float(lhs)
  }

  override def double_plus(lhs: Exp[Double], rhs: Exp[Double])(using pos: SourceContext): Exp[Double] = (lhs, rhs) match {
    case (Const(a), Const(b)) => unit(a + b)
    case (Const(0.0d), b) => b
    case (a, Const(0.0d)) => a
    case _ => super.double_plus(lhs, rhs)
  }
  override def double_minus(lhs: Exp[Double], rhs: Exp[Double])(using pos: SourceContext): Exp[Double] = (lhs,rhs) match {
    case (Const(a), Const(b)) => unit(a-b)
    case (a, Const(0.0d)) => a
    case (a, b) if a == b => Const(0.0d)
    case _ => super.double_minus(lhs,rhs)
  }
  override def double_times(lhs: Exp[Double], rhs: Exp[Double])(using pos: SourceContext): Exp[Double] = (lhs, rhs) match {
    case (Const(a), Const(b)) => unit(a*b)
    case (Const(0.0d), _) => Const(0.0d)
    case (Const(1.0d), b) => b
    case (_, Const(0.0d)) => Const(0.0d)
    case (a, Const(1.0d)) => a
    case _ => super.double_times(lhs, rhs)
  }
  override def double_divide (lhs: Exp[Double], rhs: Exp[Double])(using pos: SourceContext): Exp[Double] = (lhs, rhs) match {
    case (Const(a), Const(b)) => Const(a / b)
    case (a, Const(1.0d)) => a
    case (a, b) if a == b => Const(1.0d)
    case _ => super.double_divide(lhs, rhs)
  }

  /**
    * PrimitiveOps optimizations: Floats
    * @author  Alen Stojanov (astojanov@inf.ethz.ch)
    */
  override def obj_float_parse_float(lhs: Exp[String])(using pos: SourceContext): Exp[Float] = lhs match {
    case Const(str) => Const(str.toFloat)
    case _ => super.obj_float_parse_float(lhs)
  }
  override def float_to_int(lhs: Exp[Float])(using pos: SourceContext): Exp[Int] = lhs match {
    case Const(f) => Const(f.toInt)
    case _ => super.float_to_int(lhs)
  }
  override def float_to_double(lhs: Exp[Float])(using pos: SourceContext): Exp[Double] = lhs match {
    case Const(f) => Const(f.toDouble)
    case Def(IntToFloat(x)) => int_to_double(x)
    case _ => super.float_to_double(lhs)
  }

  override def float_plus(lhs: Exp[Float], rhs: Exp[Float])(using pos: SourceContext): Exp[Float] = (lhs, rhs) match {
    case (Const(a), Const(b)) => unit(a + b)
    case (Const(0.0f), b) => b
    case (a, Const(0.0f)) => a
    case _ => super.float_plus(lhs, rhs)
  }
  override def float_minus(lhs: Exp[Float], rhs: Exp[Float])(using pos: SourceContext): Exp[Float] = (lhs,rhs) match {
    case (Const(a), Const(b)) => unit(a - b)
    case (a, Const(0.0f)) => a
    case (a, b) if a == b => Const(0.0f)
    case _ => super.float_minus(lhs,rhs)
  }
  override def float_times(lhs: Exp[Float], rhs: Exp[Float])(using pos: SourceContext): Exp[Float] = (lhs, rhs) match {
    case (Const(a), Const(b)) => unit(a * b)
    case (Const(0.0f), _) => Const(0.0f)
    case (Const(1.0f), b) => b
    case (_, Const(0.0f)) => Const(0.0f)
    case (a, Const(1.0f)) => a
    case _ => super.float_times(lhs, rhs)
  }
  override def float_divide (lhs: Exp[Float], rhs: Exp[Float])(using pos: SourceContext): Exp[Float] = (lhs, rhs) match {
    case (Const(a), Const(b)) => Const(a / b)
    case (a, Const(1.0f)) => a
    case (a, b) if a == b => Const(1.0f)
    case _ => super.float_divide(lhs, rhs)
  }

  /**
    * PrimitiveOps optimizations: Ints
    * @author  Alen Stojanov (astojanov@inf.ethz.ch)
    */
  override def obj_integer_parse_int(lhs: Rep[String])(using pos: SourceContext): Exp[Int] = lhs match {
    case Const(str) => Const(str.toInt)
    case _ => super.obj_integer_parse_int(lhs)
  }
  override def int_to_float(lhs: Rep[Int])(using pos: SourceContext): Exp[Float] = lhs match {
    case Const(x) => Const(x.toFloat)
    case _ => super.int_to_float(lhs)
  }

  override def int_to_double(lhs: Rep[Int])(using pos: SourceContext): Exp[Double] = lhs match {
    case Const(x) => Const(x.toDouble)
    case _ => super.int_to_double(lhs)
  }

  override def int_double_value (lhs: Exp[Int])(using pos: SourceContext): Exp[Double] = lhs match {
    case Const(v) => Const(v.toDouble)
    case _ => super.int_double_value(lhs)
  }

  override def int_float_value (lhs: Exp[Int])(using pos: SourceContext): Exp[Float] = lhs match {
    case Const(v) => Const(v.toFloat)
    case _ => super.int_float_value(lhs)
  }

  override def int_tolong(lhs: Rep[Int])(using pos: SourceContext): Exp[Long] = lhs match {
    case Const(x) => Const(x.toLong)
    case _ => super.int_tolong(lhs)
  }

  override def int_plus(lhs: Exp[Int], rhs: Exp[Int])(using pos: SourceContext) : Exp[Int] = (lhs, rhs) match {
    case (Const(a), Const(b)) => unit(a+b)
    case (Const(0), b) => b
    case (a, Const(0)) => a
    case _ => super.int_plus(lhs,rhs)
  }
  override def int_minus(lhs: Exp[Int], rhs: Exp[Int])(using pos: SourceContext) : Exp[Int] = (lhs, rhs) match {
    case (Const(a), Const(b)) => unit(a-b)
    case (a,Const(0)) => a
    case (Def(IntPlus(llhs,lrhs)), rhs) if lrhs.equals(rhs) => llhs
    case (a, b) if a == b => Const(0)
    case _ => super.int_minus(lhs,rhs)
  }
  override def int_times(lhs: Exp[Int], rhs: Exp[Int])(using pos: SourceContext) : Exp[Int] = (lhs, rhs) match {
    case (Const(a), Const(b)) => unit(a*b)
    case (Const(0), _) => Const(0)
    case (Const(1), b) => b
    case (_, Const(0)) => Const(0)
    case (a, Const(1)) => a
    case _ => super.int_times(lhs,rhs)    
  }
  override def int_divide (lhs: Exp[Int], rhs: Exp[Int])(using pos: SourceContext): Exp[Int] = (lhs, rhs) match {
    case (Const(a), Const(b)) => Const(a / b)
    case (a, Const(1)) => a
    case (a, b) if a == b => Const(1)
    case _ => super.int_divide(lhs, rhs)
  }
  override def int_mod (lhs: Exp[Int], rhs: Exp[Int])(using pos: SourceContext): Exp[Int] = (lhs, rhs) match {
    case (Const(a), Const(b)) => Const(a % b)
    case (_, Const(1)) => Const(0)
    case _ => super.int_mod(lhs, rhs)
  }

}


import scala.quoted.*

trait PrimitiveOpsGen extends Gen with PrimitiveOpsExp {
  this: StagingCompile =>

  override def constantTerm[T](c: Const[T])(using q: Quotes): q.reflect.Term = {
    import q.reflect.*
    c match {
      case Const(x: Double) => Literal(DoubleConstant(x))
      case Const(x: Float) => Literal(FloatConstant(x))
      case Const(x: Long) => Literal(LongConstant(x))
      case Const(x: Int) => Literal(IntConstant(x))
      case Const(()) => Literal(UnitConstant())
      // TODO others
      case _ =>  super.constantTerm(c)
    }
  }

  // TODO interpretDefWithEnv
  override def interpretDefWithEnv[A](d: Def[A])(using q: Quotes, env:Map[Sym[?], q.reflect.Symbol]): q.reflect.Term = {
    import q.reflect.*

    def interpretBinary(lhsExp: Exp[?], rhsExp: Exp[?], methodName: String): Term = {
      val lhs = interpretExpWithEnv(lhsExp)
      val rhs = interpretExpWithEnv(rhsExp)
      val method = lhs.tpe.classSymbol.get.methodMember(methodName).head
      Apply(Select(lhs, method), List(rhs))
    }

    def interpretIntBinary(lhsExp: Exp[Int], rhsExp: Exp[Int], op: String): Term = {
      val lhs = interpretExpWithEnv(lhsExp).asExprOf[Int]
      val rhs = interpretExpWithEnv(rhsExp).asExprOf[Int]
      op match {
        case "+" => '{ $lhs + $rhs }.asTerm
        case "-" => '{ $lhs - $rhs }.asTerm
        case "*" => '{ $lhs * $rhs }.asTerm
        case "/" => '{ $lhs / $rhs }.asTerm
        case "%" => '{ $lhs % $rhs }.asTerm
        case "&" => '{ $lhs & $rhs }.asTerm
        case "|" => '{ $lhs | $rhs }.asTerm
        case "^" => '{ $lhs ^ $rhs }.asTerm
        case "<<" => '{ $lhs << $rhs }.asTerm
        case ">>" => '{ $lhs >> $rhs }.asTerm
        case ">>>" => '{ $lhs >>> $rhs }.asTerm
      }
    }

    def interpretIntUnary(argExp: Exp[Int], op: String): Term = {
      val arg = interpretExpWithEnv(argExp).asExprOf[Int]
      op match {
        case "~" => '{ ~$arg }.asTerm
      }
    }

    def interpretLongBinary(lhsExp: Exp[Long], rhsExp: Exp[Long], op: String): Term = {
      val lhs = interpretExpWithEnv(lhsExp).asExprOf[Long]
      val rhs = interpretExpWithEnv(rhsExp).asExprOf[Long]
      op match {
        case "%" => '{ $lhs % $rhs }.asTerm
        case "&" => '{ $lhs & $rhs }.asTerm
        case "|" => '{ $lhs | $rhs }.asTerm
        case "^" => '{ $lhs ^ $rhs }.asTerm
      }
    }

    def interpretLongShift(lhsExp: Exp[Long], rhsExp: Exp[Int], op: String): Term = {
      val lhs = interpretExpWithEnv(lhsExp).asExprOf[Long]
      val rhs = interpretExpWithEnv(rhsExp).asExprOf[Int]
      op match {
        case "<<" => '{ $lhs << $rhs }.asTerm
        case ">>" => '{ $lhs >> $rhs }.asTerm
        case ">>>" => '{ $lhs >>> $rhs }.asTerm
      }
    }

    def interpretLongUnary(argExp: Exp[Long], op: String): Term = {
      val arg = interpretExpWithEnv(argExp).asExprOf[Long]
      op match {
        case "~" => '{ ~$arg }.asTerm
      }
    }

    d match {
      case IntPlus(lhs, rhs) =>
        interpretIntBinary(lhs, rhs, "+")
      case IntMinus(lhs, rhs) =>
        interpretIntBinary(lhs, rhs, "-")
      case IntTimes(lhs, rhs) =>
        interpretIntBinary(lhs, rhs, "*")
      case IntDivide(lhs, rhs) =>
        interpretIntBinary(lhs, rhs, "/")
      case IntMod(lhs, rhs) =>
        interpretIntBinary(lhs, rhs, "%")
      case IntBinaryAnd(lhs, rhs) =>
        interpretIntBinary(lhs, rhs, "&")
      case IntBinaryOr(lhs, rhs) =>
        interpretIntBinary(lhs, rhs, "|")
      case IntBinaryXor(lhs, rhs) =>
        interpretIntBinary(lhs, rhs, "^")
      case IntShiftLeft(lhs, rhs) =>
        interpretIntBinary(lhs, rhs, "<<")
      case IntShiftRightArith(lhs, rhs) =>
        interpretIntBinary(lhs, rhs, ">>")
      case IntShiftRightLogical(lhs, rhs) =>
        interpretIntBinary(lhs, rhs, ">>>")
      case IntBitwiseNot(arg) =>
        interpretIntUnary(arg, "~")
      case LongMod(lhs, rhs) =>
        interpretLongBinary(lhs, rhs, "%")
      case LongBinaryAnd(lhs, rhs) =>
        interpretLongBinary(lhs, rhs, "&")
      case LongBinaryOr(lhs, rhs) =>
        interpretLongBinary(lhs, rhs, "|")
      case LongBinaryXor(lhs, rhs) =>
        interpretLongBinary(lhs, rhs, "^")
      case LongBitwiseNot(arg) =>
        interpretLongUnary(arg, "~")
      case LongShiftLeft(lhs, rhs) =>
        interpretLongShift(lhs, rhs, "<<")
      case LongShiftRightArith(lhs, rhs) =>
        interpretLongShift(lhs, rhs, ">>")
      case LongShiftRightUnsigned(lhs, rhs) =>
        interpretLongShift(lhs, rhs, ">>>")
      case LongToInt(lhs) =>
        val value = interpretExpWithEnv(lhs).asExprOf[Long]
        '{ $value.toInt }.asTerm
      case FloatToDouble(lhs) =>
        val value = interpretExpWithEnv(lhs).asExprOf[Float]
        '{ $value.toDouble }.asTerm
      case FloatToInt(lhs) =>
        val value = interpretExpWithEnv(lhs).asExprOf[Float]
        '{ $value.toInt }.asTerm
      case IntToDouble(lhs) =>
        val value = interpretExpWithEnv(lhs).asExprOf[Int]
        '{ $value.toDouble }.asTerm
      case IntToFloat(lhs) =>
        val value = interpretExpWithEnv(lhs).asExprOf[Int]
        '{ $value.toFloat }.asTerm
      case op: ArithOp[?] =>
        val methodName =
          if (d.isInstanceOf[DoublePlus] || d.isInstanceOf[FloatPlus] || d.isInstanceOf[LongPlus]) {
            "+"
          } else if (d.isInstanceOf[DoubleMinus] || d.isInstanceOf[FloatMinus] || d.isInstanceOf[LongMinus]) {
            "-"
          } else if (d.isInstanceOf[DoubleTimes] || d.isInstanceOf[FloatTimes] || d.isInstanceOf[LongTimes]) {
            "*"
          } else if (d.isInstanceOf[DoubleDivide] || d.isInstanceOf[FloatDivide] || d.isInstanceOf[LongDivide]) {
            "/"
          } else {
            throw new Exception(s"Unsupported Def type: ${d.getClass}")
          }
        interpretBinary(op.lhs, op.rhs, methodName)
      case _ =>
        super.interpretDefWithEnv(d)
    }
  }
}

trait ScalaGenPrimitiveOps extends ScalaGenBase {
  val IR: PrimitiveOpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ObjDoubleParseDouble(s) => emitValDef(sym, src"java.lang.Double.parseDouble($s)")
    case ObjDoublePositiveInfinity() => emitValDef(sym, "scala.Double.PositiveInfinity")
    case ObjDoubleNegativeInfinity() => emitValDef(sym, "scala.Double.NegativeInfinity")
    case ObjDoubleMinValue() => emitValDef(sym, "scala.Double.MinValue")
    case ObjDoubleMaxValue() => emitValDef(sym, "scala.Double.MaxValue")
    case DoubleFloatValue(lhs) => emitValDef(sym, quote(lhs) + ".floatValue()")
    case DoublePlus(lhs,rhs) => emitValDef(sym, quote(lhs) + " + " + quote(rhs))
    case DoubleMinus(lhs,rhs) => emitValDef(sym, quote(lhs) + " - " + quote(rhs))
    case DoubleTimes(lhs,rhs) => emitValDef(sym, quote(lhs) + " * " + quote(rhs))
    case DoubleDivide(lhs,rhs) => emitValDef(sym, quote(lhs) + " / " + quote(rhs))    
    case DoubleToInt(lhs) => emitValDef(sym, quote(lhs) + ".toInt")
    case DoubleToFloat(lhs) => emitValDef(sym, quote(lhs) + ".toFloat")    
    case ObjFloatParseFloat(s) => emitValDef(sym, "java.lang.Float.parseFloat(" + quote(s) + ")")
    case FloatToInt(lhs) => emitValDef(sym, quote(lhs) + ".toInt")
    case FloatToDouble(lhs) => emitValDef(sym, quote(lhs) + ".toDouble")        
    case FloatPlus(lhs,rhs) => emitValDef(sym, quote(lhs) + " + " + quote(rhs))
    case FloatMinus(lhs,rhs) => emitValDef(sym, quote(lhs) + " - " + quote(rhs))
    case FloatTimes(lhs,rhs) => emitValDef(sym, quote(lhs) + " * " + quote(rhs))
    case FloatDivide(lhs,rhs) => emitValDef(sym, quote(lhs) + " / " + quote(rhs))    
    case ObjIntegerParseInt(s) => emitValDef(sym, "java.lang.Integer.parseInt(" + quote(s) + ")")
    case ObjIntMaxValue() => emitValDef(sym, "scala.Int.MaxValue")
    case ObjIntMinValue() => emitValDef(sym, "scala.Int.MinValue")    
    case IntPlus(lhs,rhs) => emitValDef(sym, quote(lhs) + " + " + quote(rhs))
    case IntMinus(lhs,rhs) => emitValDef(sym, quote(lhs) + " - " + quote(rhs))
    case IntTimes(lhs,rhs) => emitValDef(sym, quote(lhs) + " * " + quote(rhs))
    // case IntDivideFrac(lhs,rhs) => emitValDef(sym, quote(lhs) + " / " + quote(rhs))
    case IntDivide(lhs,rhs) => emitValDef(sym, quote(lhs) + " / " + quote(rhs))
    case IntMod(lhs,rhs) => emitValDef(sym, quote(lhs) + " % " + quote(rhs))
    case IntBinaryOr(lhs,rhs) => emitValDef(sym, quote(lhs) + " | " + quote(rhs))
    case IntBinaryAnd(lhs,rhs) => emitValDef(sym, quote(lhs) + " & " + quote(rhs))
    case IntBinaryXor(lhs,rhs) => emitValDef(sym, quote(lhs) + " ^ " + quote(rhs))
    case IntShiftLeft(lhs,rhs) => emitValDef(sym, quote(lhs) + " << " + quote(rhs))
    case IntShiftRightArith(lhs, rhs) => emitValDef(sym, quote(lhs) + " >> " + quote(rhs))
    case IntShiftRightLogical(lhs, rhs) => emitValDef(sym, quote(lhs) + " >>> " + quote(rhs))
    case IntDoubleValue(lhs) => emitValDef(sym, quote(lhs) + ".doubleValue()")
    case IntFloatValue(lhs) => emitValDef(sym, quote(lhs) + ".floatValue()")
    case IntBitwiseNot(lhs) => emitValDef(sym, "~" + quote(lhs))
    case IntToLong(lhs) => emitValDef(sym, quote(lhs) + ".toLong")
    case IntToFloat(lhs) => emitValDef(sym, quote(lhs) + ".toFloat")
    case IntToDouble(lhs) => emitValDef(sym, quote(lhs) + ".toDouble")
    case ObjLongParseLong(s) => emitValDef(sym, "java.lang.Long.parseLong(" + quote(s) + ")")
    case LongPlus(lhs,rhs) => emitValDef(sym, quote(lhs) + " + " + quote(rhs))
    case LongMinus(lhs,rhs) => emitValDef(sym, quote(lhs) + " - " + quote(rhs))
    case LongTimes(lhs,rhs) => emitValDef(sym, quote(lhs) + " * " + quote(rhs))
    case LongDivide(lhs,rhs) => emitValDef(sym, quote(lhs) + " / " + quote(rhs))
    case LongMod(lhs,rhs) => emitValDef(sym, quote(lhs) + " % " + quote(rhs))
    case LongBinaryOr(lhs,rhs) => emitValDef(sym, quote(lhs) + " | " + quote(rhs))
    case LongBinaryAnd(lhs,rhs) => emitValDef(sym, quote(lhs) + " & " + quote(rhs))
    case LongBinaryXor(lhs,rhs) => emitValDef(sym, quote(lhs) + " ^ " + quote(rhs))
    case LongBitwiseNot(lhs) => emitValDef(sym, "~" + quote(lhs))
    case LongShiftLeft(lhs,rhs) => emitValDef(sym, quote(lhs) + " << " + quote(rhs))
    case LongShiftRightArith(lhs,rhs) => emitValDef(sym, quote(lhs) + " >> " + quote(rhs))
    case LongShiftRightUnsigned(lhs,rhs) => emitValDef(sym, quote(lhs) + " >>> " + quote(rhs))
    case LongToInt(lhs) => emitValDef(sym, quote(lhs) + ".toInt")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CLikeGenPrimitiveOps extends CLikeGenBase {
  val IR: PrimitiveOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case ObjDoubleParseDouble(s) => emitValDef(sym, "strtod(" + quote(s) + ",NULL)")
      case ObjDoubleMinValue() => emitValDef(sym, "DBL_MIN")
      case ObjDoubleMaxValue() => emitValDef(sym, "DBL_MAX")
      case DoubleFloatValue(lhs) => emitValDef(sym, "(float)"+quote(lhs))
      case DoublePlus(lhs,rhs) => emitValDef(sym, quote(lhs) + " + " + quote(rhs))
      case DoubleMinus(lhs,rhs) => emitValDef(sym, quote(lhs) + " - " + quote(rhs))
      case DoubleTimes(lhs,rhs) => emitValDef(sym, quote(lhs) + " * " + quote(rhs))
      case DoubleDivide(lhs,rhs) => emitValDef(sym, quote(lhs) + " / " + quote(rhs))      
      case DoubleToInt(lhs) => emitValDef(sym, "(int32_t)" + quote(lhs))
      case DoubleToFloat(lhs) => emitValDef(sym, "(float)" + quote(lhs))    
      case ObjFloatParseFloat(s) => emitValDef(sym, "strtof(" + quote(s) + ".c_str(),NULL)")
      case FloatToInt(lhs) => emitValDef(sym, "(int32_t)" + quote(lhs))
      case FloatToDouble(lhs) => emitValDef(sym, "(double)" + quote(lhs))        
      case FloatPlus(lhs,rhs) => emitValDef(sym, quote(lhs) + " + " + quote(rhs))
      case FloatMinus(lhs,rhs) => emitValDef(sym, quote(lhs) + " - " + quote(rhs))
      case FloatTimes(lhs,rhs) => emitValDef(sym, quote(lhs) + " * " + quote(rhs))
      case FloatDivide(lhs,rhs) => emitValDef(sym, quote(lhs) + " / " + quote(rhs))      
      case ObjIntegerParseInt(s) => emitValDef(sym, "atoi(" + quote(s) + ".c_str())")
      case ObjIntMaxValue() => emitValDef(sym, "INT_MAX")
      case ObjIntMinValue() => emitValDef(sym, "INT_MIN")    
      case IntPlus(lhs,rhs) => emitValDef(sym, quote(lhs) + " + " + quote(rhs))
      case IntMinus(lhs,rhs) => emitValDef(sym, quote(lhs) + " - " + quote(rhs))
      case IntTimes(lhs,rhs) => emitValDef(sym, quote(lhs) + " * " + quote(rhs))
      // case IntDivideFrac(lhs,rhs) => emitValDef(sym, quote(lhs) + " / " + quote(rhs))
      case IntDivide(lhs,rhs) => emitValDef(sym, quote(lhs) + " / " + quote(rhs))
      case IntMod(lhs,rhs) => emitValDef(sym, quote(lhs) + " % " + quote(rhs))
      case IntBinaryOr(lhs,rhs) => emitValDef(sym, quote(lhs) + " | " + quote(rhs))
      case IntBinaryAnd(lhs,rhs) => emitValDef(sym, quote(lhs) + " & " + quote(rhs))
      case IntBinaryXor(lhs,rhs) => emitValDef(sym, quote(lhs) + " ^ " + quote(rhs))
      case IntShiftLeft(lhs,rhs) => emitValDef(sym, quote(lhs) + " << " + quote(rhs))
      case IntShiftRightArith(lhs, rhs) => emitValDef(sym, quote(lhs) + " >> " + quote(rhs))
      case IntShiftRightLogical(lhs, rhs) => emitValDef(sym, "(uint32_t)" + quote(lhs) + " >> " + quote(rhs))
      case IntDoubleValue(lhs) => emitValDef(sym, "(double)"+quote(lhs))
      case IntFloatValue(lhs) => emitValDef(sym, "(float)"+quote(lhs))
      case IntBitwiseNot(lhs) => emitValDef(sym, "~" + quote(lhs))
      case IntToLong(lhs) => emitValDef(sym, "(int64_t)"+quote(lhs))
      case IntToFloat(lhs) => emitValDef(sym, "(float)"+quote(lhs))
      case IntToDouble(lhs) => emitValDef(sym, "(double)"+quote(lhs))
      case ObjLongParseLong(s) => emitValDef(sym, "strtod(" + quote(s) + ".c_str(),NULL)")
      case LongPlus(lhs,rhs) => emitValDef(sym, quote(lhs) + " + " + quote(rhs))
      case LongMinus(lhs,rhs) => emitValDef(sym, quote(lhs) + " - " + quote(rhs))
      case LongTimes(lhs,rhs) => emitValDef(sym, quote(lhs) + " * " + quote(rhs))
      case LongDivide(lhs,rhs) => emitValDef(sym, quote(lhs) + " / " + quote(rhs))
      case LongMod(lhs,rhs) => emitValDef(sym, quote(lhs) + " % " + quote(rhs))
      case LongBinaryOr(lhs,rhs) => emitValDef(sym, quote(lhs) + " | " + quote(rhs))
      case LongBinaryAnd(lhs,rhs) => emitValDef(sym, quote(lhs) + " & " + quote(rhs))
      case LongBinaryXor(lhs,rhs) => emitValDef(sym, quote(lhs) + " ^ " + quote(rhs))
      case LongBitwiseNot(lhs) => emitValDef(sym, "~" + quote(lhs))
      case LongShiftLeft(lhs,rhs) => emitValDef(sym, quote(lhs) + " << " + quote(rhs))
      case LongShiftRightArith(lhs,rhs) => emitValDef(sym, quote(lhs) + " >> " + quote(rhs))
      case LongShiftRightUnsigned(lhs,rhs) => emitValDef(sym, "(uint64_t)" + quote(lhs) + " >> " + quote(rhs))
      case LongToInt(lhs) => emitValDef(sym, "(int32_t)"+quote(lhs))
      case _ => super.emitNode(sym, rhs)
    }
  }
}

trait CudaGenPrimitiveOps extends CudaGenBase with CLikeGenPrimitiveOps {
  val IR: PrimitiveOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case ObjDoublePositiveInfinity() => emitValDef(sym, "__longlong_as_double(0x7ff0000000000000ULL)")
      case ObjDoubleNegativeInfinity() => emitValDef(sym, "__longlong_as_double(0xfff0000000000000ULL)")
      case _ => super.emitNode(sym, rhs)
    }
  }
}

trait OpenCLGenPrimitiveOps extends OpenCLGenBase with CLikeGenPrimitiveOps

trait CGenPrimitiveOps extends CGenBase with CLikeGenPrimitiveOps {
  val IR: PrimitiveOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case ObjDoublePositiveInfinity() => emitValDef(sym, "INFINITY")
      case ObjDoubleNegativeInfinity() => emitValDef(sym, "-INFINITY")
      case _ => super.emitNode(sym, rhs)
    }
  }
}
