package org.scala_lang.virtualized.scala

import java.io._


import scala.tools.nsc._
import scala.tools.nsc.util._
import scala.tools.nsc.reporters._
import scala.tools.nsc.io._

import scala.tools.nsc.interpreter.AbstractFileClassLoader


object Scalac {

  var compiler: Global = _
  var reporter: ConsoleReporter = _

  setup()

  def setup(): Unit = {
    
    val settings = new Settings()

    settings.classpath.value = this.getClass.getClassLoader match {
      case ctx: java.net.URLClassLoader => ctx.getURLs.map(_.getPath).mkString(":")
      case _ => System.getProperty("java.class.path")
    }
    settings.bootclasspath.value = Predef.getClass.getClassLoader match {
      case ctx: java.net.URLClassLoader => ctx.getURLs.map(_.getPath).mkString(":")
      case _ => System.getProperty("sun.boot.class.path")
    }
    settings.encoding.value = "UTF-8"
    settings.outdir.value = "."
    settings.extdirs.value = ""

    reporter = new ConsoleReporter(settings/*, null, new PrintWriter(System.out)*/)//writer
    compiler = new Global(settings, reporter)
  
  }

  def compile[A](className: String, source: String, args: List[Any]): A = {

    /*val className = "StagedX"

    val source = new StringWriter()
    val pw = new PrintWriter(source)
    //codegen.emitSource(f, className, pw)
    pw.println("class StagedX extends (Int=>Unit) { def apply(x:Int) = { println(x) } }")*/

    val compiler = this.compiler
    val run = new compiler.Run

    val fileSystem = new VirtualDirectory("<vfs>", None)
    compiler.settings.outputDirs.setSingleOutput(fileSystem)

    run.compileSources(List(new BatchSourceFile("<stdin>", source.toString)))
    reporter.printSummary()

    if (!reporter.hasErrors)
      println("compilation: ok")
    else
      println("compilation: had errors")

    reporter.reset

    val parent = this.getClass.getClassLoader
    val loader = new AbstractFileClassLoader(fileSystem, this.getClass.getClassLoader)

    val cls: Class[_] = loader.loadClass(className)
    //cls.newInstance().asInstanceOf[A]
    val cons = cls.getConstructor(args.map(_.getClass):_*)
    
    cons.newInstance(args.asInstanceOf[List[AnyRef]]:_*).asInstanceOf[A]
  }


}
