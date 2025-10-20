package lms.legacy.internal

import java.io.{PrintWriter, FileOutputStream}

trait GraphVizExport extends GraphTraversal {
  val IR: Expressions
  import IR._

  def quote(x: Any) = "\""+x+"\""
  
  def emitNode(sym: Sym[Any], rhs: Def[Any])(using stream: PrintWriter) = {
    stream.println("label=" + quote("" + sym + " \\n " + rhs))
    stream.println("shape=box")
  }

  def emitDeps(sym: Sym[Any], rhs: Def[Any], deps: List[Sym[Any]])(using stream: PrintWriter) = {
    for (dep <- deps) {
      stream.println("\"" + dep + "\" -> \"" + sym + "\"")
    }
  }

  def emitDepGraph(start: Exp[Any], file: String, landscape: Boolean = false): Unit =
    emitDepGraph(start, new java.io.PrintWriter(new java.io.FileOutputStream(file)), landscape)

  def emitDepGraph(start: Exp[Any], stream: PrintWriter, landscape: Boolean): Unit = {

    stream.println("digraph G {")

    val deflist = buildScheduleForResult(start,false)

    if (landscape)
      stream.println("rankdir=LR")

    for (case TP(sym, rhs) <- deflist) {

      val deps = syms(rhs)

      stream.println(quote(sym) + " [")

      // all

      emitNode(sym, rhs)(using stream)

      stream.println("]")
      
      emitDeps(sym, rhs, deps)(using stream)

    }

    stream.println("}")
    stream.close()
  }
 
  
  
}
