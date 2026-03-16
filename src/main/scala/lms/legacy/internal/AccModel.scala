package lms.legacy.internal

import scala.language.implicitConversions

trait AbstractHostTransfer {
  this: GenericCodegen =>

  val IR: Expressions
  import IR._

  def emitSend(tp: Typ[?], peer: Targets.Value): (String,String)
  def emitRecv(tp: Typ[?], peer: Targets.Value): (String,String)
  def emitSendView(tp: Typ[?], peer: Targets.Value): (String,String)
  def emitRecvView(tp: Typ[?], peer: Targets.Value): (String,String)
  def emitSendUpdate(tp: Typ[?], peer: Targets.Value): (String,String)
  def emitRecvUpdate(tp: Typ[?], peer: Targets.Value): (String,String)

  def isListType(tp: Typ[?]): Boolean = {
    tp.runtimeClass == classOf[List[?]]
  }
}

trait AbstractDeviceTransfer {
  this: GenericCodegen =>

  val IR: Expressions
  import IR._

  def emitSendSlave(tp: Typ[?]) : (String,String)
  def emitRecvSlave(tp: Typ[?]) : (String,String)
  //def emitSendViewSlave(tp: Typ[?]) : (String,String)
  //def emitRecvViewSlave(tp: Typ[?]) : (String,String)
  def emitSendUpdateSlave(tp: Typ[?]) : (String,String)
  def emitRecvUpdateSlave(tp: Typ[?]) : (String,String)

  //def allocOutput(newSym: Sym[?], sym: Sym[?], reset: Boolean = false) : Unit
}

object Targets extends Enumeration {
  
  //TODO: Get rid of JVM target, or make an hierarchy
  val JVM = Value("jvm")
  val Scala = Value("scala")
  val Cpp = Value("cpp")
  val Cuda = Value("cuda")
  val OpenCL = Value("opencl")
  
  def apply(s: String): Value = s.toLowerCase() match {
    case "jvm" => JVM
    case "scala" => Scala
    case "cpp" => Cpp
    case "cuda" => Cuda
    case "opencl" => OpenCL
    case _ => throw new IllegalArgumentException("unsupported target: " + s)
  }

  def getHostTarget(target: Value): Targets.Value = {
    target match {
      case Targets.Scala => Targets.Scala
      case Targets.Cpp => Targets.Cpp
      case Targets.Cuda => Targets.Cpp
      case Targets.OpenCL => Targets.Cpp
      case _ => throw new IllegalArgumentException("Cannot find a host target for target " + target)
    }
  }

  implicit def targettostring(target: Targets.Value): String = target.toString
}
