package lms.core.examples

import lms.gen.StagingCompile
import lms.legacy.common.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.language.implicitConversions


trait FFT extends PrimitiveOpsExp with LiftNumeric with TrigExp with ArraysExp with BooleanOpsExp {

  def omega(k: Int, N: Int): Complex = {
    val kth = -2.0 * k * math.Pi / N
    Complex(cos(kth), sin(kth))  // how do we handle cases like cos(pi/2) == 0
  }

  case class Complex(re: Rep[Double], im: Rep[Double]) {
    def +(that: Complex) = Complex(this.re + that.re, this.im + that.im)

    def -(that: Complex) = Complex(this.re - that.re, this.im - that.im)

    def *(that: Complex) = Complex(this.re * that.re - this.im * that.im, this.re * that.im + this.im * that.re)
  }

  def splitEvenOdd[T](xs: List[T]): (List[T], List[T]) = (xs: @unchecked) match {
    case e :: o :: xt =>
      val (es, os) = splitEvenOdd(xt)
      ((e :: es), (o :: os))
    case Nil => (Nil, Nil)
    // cases?
  }

  def mergeEvenOdd[T](even: List[T], odd: List[T]): List[T] = ((even, odd): @unchecked) match {
    case (Nil, Nil) =>
      Nil
    case ((e :: es), (o :: os)) =>
      e :: (o :: mergeEvenOdd(es, os))
    // cases?
  }

  def fft(xs: List[Complex]): List[Complex] = xs match {
    case (x :: Nil) => xs
    case _ =>
      val N = xs.length // assume it's a power of two
      val (even0, odd0) = splitEvenOdd(xs)
      val (even1, odd1) = (fft(even0), fft(odd0))
      val (even2, odd2) = even1.zip(odd1).zipWithIndex.map({
        case ((x, y), k) => {
          val z = omega(k, N) * y
          (x + z, x - z)
        }
      }).unzip
      even2 ::: odd2
  }

  def fftFromDoubleArray(xs: Rep[Array[Double]], size: Int): Rep[Array[Double]] = {
    val xsComplex: List[Complex] = for(i <- Range(0, size).toList)
      yield Complex(xs.apply(i * 2), xs.apply(i*2 + 1))
    val zsComplex: List[Complex] = fft(xsComplex)
    makeArray(zsComplex.flatMap(z => List(z.re, z.im)))
  }
}

class FFTSpec extends AnyFlatSpec with Matchers {
  
  "basic compile and application" should "run" in {
    val fftCompiler = new FFT with StagingCompile with BaseExp with PrimitiveOpsExpOpt with LiftPrimitives with VariablesExpOpt with PrimitiveOpsGen with TrigExpOpt with TrigGen with ArraysExp with ArraysGen {
      override type API = this.type
    }

    val f: fftCompiler.Exp[Array[Double]] => fftCompiler.Exp[Array[Double]] = fftCompiler.fftFromDoubleArray(_, 4)

    val fft8 = fftCompiler.compile(f)
    val input = Array(1.0, 2.0, 2.0, 1.0, 1.0, 2.0, 2.0, 1.0) //, 1.0, 2.0, 2.0, 1.0, 1.0, 2.0, 2.0, 1.0)
    fft8(input).mkString("Array(", ", ", ")") should be ("Array(6.0, 6.0, 0.0, 0.0, -2.0, 2.0, 0.0, 0.0)")
  }
}
