package org.scala_lang.virtualized
package scala

object Test {

  def time[A](s: String)(block: =>A): A = {
    val t0 = System.currentTimeMillis
    val r = block
    println(s + " took " + ((System.currentTimeMillis-t0) / 1000.0) + "s")
    r
  }

  def main(args: Array[String]) {
    
    val f1 = UnstagedImpl.genericTest
    val f2 = UnstagedImpl.specificTest
    val f3 = StagedImpl.genericTest
    
    time("--- generic") { f1() }
    time("--- generic") { f1() }
    time("--- generic") { f1() }
    time("--- generic") { f1() }
    time("--- generic") { f1() }

    println
    
    time("--- double") { f2() }
    time("--- double") { f2() }
    time("--- double") { f2() }
    time("--- double") { f2() }
    time("--- double") { f2() }

    println
    
    time("--- staged") { f3() }
    time("--- staged") { f3() }
    time("--- staged") { f3() }
    time("--- staged") { f3() }
    time("--- staged") { f3() }

  }
  
}


object UnstagedImpl {
  
  class Matrix[A: Manifest](val rows: Int, val cols: Int) {
    private val arr: Array[A] = new Array[A](rows * cols)

    def apply(i: Int, j: Int): A = {
      arr(i*cols + j)
    }

    def update(i: Int, j: Int, e: A) {
      arr(i*cols + j) = e
    }
    
    def print {
      println(arr(0))
    }
  }
  
  
  def genericTest: () => Unit = { () =>
    val m = randomMatrix(500, 100)
    val n = randomMatrix(100, 500)

    val p = multGeneric(m,n)
    p.print
  }

  def specificTest: () => Unit = { () =>
    val m = randomMatrix(500, 100)
    val n = randomMatrix(100, 500)

    val p = multDouble(m,n)
    p.print
  }

  def randomMatrix(n: Int, m: Int) = {
    val r = new util.Random(10)
    val x = new Matrix[Double](n, m) 
    for (i <- 0 until n; j <- 0 until m)
      x(i, j) = (r.nextInt % 1000).toDouble
    x
  }

  def multGeneric[T:Numeric:Manifest](m: Matrix[T], n: Matrix[T]) = {
    val num = implicitly[Numeric[T]]
    import num._

    val p = new Matrix[T](m.rows, n.cols)
    
    for (i <- 0 until m.rows) {
      for (j <- 0 until n.cols) {
        for (k <- 0 until n.rows)
          p(i, j) += m(i, k) * n(k, j)
      }
    }
    p
  }

  def multDouble(m: Matrix[Double], n: Matrix[Double]) = {
    val p = new Matrix[Double](m.rows, n.cols)

    for (i <- 0 until m.rows) {
      for (j <- 0 until n.cols) {
        for (k <- 0 until n.rows)
          p(i, j) += m(i, k) * n(k, j)
      }
    }
    p
  }

}





object StagedImpl extends ScalaLift with ScalaCompile {
  

  class Matrix[A: Manifest](val rows: Exp[Int], val cols: Exp[Int]) {
    private val arr: Exp[Array[A]] = ArrayNew[A](rows * cols)

    def apply(i: Exp[Int], j: Exp[Int]): Exp[A] = {
      arr(i*cols + j)
    }

    def update(i: Exp[Int], j: Exp[Int], e: Exp[A]) {
      arr(i*cols + j) = e
    }
    
    def print(): Exp[Unit] = Print(arr)
  }
    

  def genericTest: () => Unit = compile {
    val m = randomMatrix(500, 100)
    val n = randomMatrix(100, 500)

    val p = multGeneric(m,n)
    p.print
  }


  def randomMatrix(n: Int, m: Int) = {
    val r = new Random(10)
    val x = new Matrix[Double](n, m) 
    for (i <- 0 until n; j <- 0 until m)
      x(i, j) = (r.nextInt % 1000).toDouble
    x
  }

  def multGeneric[T:Numeric:Manifest](m: Matrix[T], n: Matrix[T]) = {
    val num = implicitly[Numeric[T]]
    import num._

    val p = new Matrix[T](m.rows, n.cols)
    
    for (i <- 0 until m.rows) {
      for (j <- 0 until n.cols) {
        for (k <- 0 until n.rows)
          p(i, j) += m(i, k) * n(k, j)
      }
    }
    p
  }

}






trait ScalaLift extends ScalaDefsExps {
  
  implicit def liftUnit(x:Unit): Exp[Unit] = Const(())

  def infix_*[T:Numeric:Manifest](x: Exp[T], y: Exp[T]): Exp[T] = BinaryOp[T,T](x, "*", y)
  def infix_+[T:Numeric:Manifest](x: Exp[T], y: Exp[T]): Exp[T] = BinaryOp[T,T](x, "+", y)
  def infix_%[T:Numeric:Manifest](x: Exp[T], y: Exp[T]): Exp[T] = BinaryOp[T,T](x, "%", y)
  def infix_toDouble[T:Numeric:Manifest](x: Exp[T]): Exp[Double] = Method[T,Double](x, "toDouble")
  
  def infix_until(x: Exp[Int], y: Exp[Int]) = RangeExp(x,y)

  def ArrayNew[T:Manifest](n: Exp[Int]): Exp[Array[T]] = ArrayNewOp[T](n, manifest[T])
  
  class ArrayOps[T:Manifest](x: Exp[Array[T]]) {
    def apply(i: Exp[Int]): Exp[T] = ArrayApply(x,i)
    def update(i: Exp[Int], y: Exp[T]): Exp[Unit] = ArrayUpdate(x,i,y)
  }
  implicit def arrayOps[T:Manifest](x: Exp[Array[T]]): ArrayOps[T] = new ArrayOps[T](x)

}


trait ScalaDefsExps extends CoreExps with CoreDefs {

  case class BinaryOp[T, U](x: Exp[T], op: String, y: Exp[T]) extends Def[U]
  case class Method[T, U](x: Exp[T], op: String) extends Def[U]
  

  class Random(x: Exp[Int]) {
    val r: Exp[util.Random] = RandomNewOp(x)
    def nextInt: Exp[Int] = RandomNextInt(r)
  }

  case class RangeExp(val start: Exp[Int], val end: Exp[Int]) {
    def foreach(f: Exp[Int] => Exp[Unit]): Exp[Unit] = {
      val x = Sym[Int]()
      val y = reifyBlock { f(x) }
      Foreach(this, x, y)
    }
  }

  case class Foreach(r: RangeExp, x: Sym[Int], y: Block[Unit]) extends Def[Unit]  
  
  case class ArrayNewOp[T](n: Exp[Int], tp: Manifest[T]) extends Def[Array[T]]
  case class ArrayApply[T](x: Exp[Array[T]], i: Exp[Int]) extends Def[T]
  case class ArrayUpdate[T](x: Exp[Array[T]], i: Exp[Int], y: Exp[T]) extends Def[Unit]

  case class RandomNewOp(n: Exp[Int]) extends Def[util.Random]
  case class RandomNextInt(r: Exp[util.Random]) extends Def[Int]

  case class Print[T](x: Exp[T]) extends Def[Unit]

}




trait ScalaCompile extends ScalaCodeGen {

  def compile(block: => Exp[Unit]): () => Unit = {
    val s = captureOutput {
      emitPlain("class StagedA extends (() => Unit) { def apply() = ")
      emitBlock(reifyBlock { block })
      emitPlain("}")
    }
    println(s)    
    Scalac.compile[() => Unit]("StagedA", s, Nil)
  }
  
}

/** 
 * Rudimentary Scala code generation 
 * 
 * render Block, Def, and Expr as Scala code (printing to the console)
 */
trait ScalaCodeGen extends ScalaDefsExps {
  var nesting = 0
  var indent = true

  def emitValDef[T](s: Sym[T], rhs: String, more: Boolean = false) = {
    emitPlain("var " + s + " = " + rhs, more)
  }

  def emitPlain(s: String, more: Boolean = false) = {
    if (indent) print(" " * (nesting * 2))
    if (more) print(s) else println(s)
    indent = !more
  }

  def emitBlock[T](a: Block[T], more: Boolean = false, extra: String = "") = a match {
    case Block(stms, e) =>
      emitPlain("{", false); nesting += 1
        stms foreach { case t: ScopeEntry[t] => emitNode[t](t.sym, t.rhs) }
        emitPlain(e.toString)
        if (extra != "") emitPlain(extra)
      nesting -= 1; emitPlain("}", more)
  }

  def captureOutput(block: =>Unit): String = {
    val bstream = new java.io.ByteArrayOutputStream
    Console.withOut(new java.io.PrintStream(bstream))(block)
    bstream.toString
  }
  
  def emitNode[T](s: Sym[T], d: Def[T]): Unit = d match {
    case Foreach(r,x,y) => 
      emitPlain("var " + x + ": Int = " + r.start)
      emitPlain("while (" + x + " < " + r.end + ") ", true)
      emitBlock(y, false, x + " += 1")
      emitValDef(s, "()")
    case ArrayNewOp(n, tp) =>
      emitValDef(s, "new Array[" + tp + "](" + n + ")")
    case ArrayApply(x: Exp[Array[T]], i: Exp[Int]) =>
      emitValDef(s, x + "("+ i + ")")
    case ArrayUpdate(x: Exp[Array[T]], i: Exp[Int], y: Exp[T]) =>
      emitValDef(s, x + "("+ i + ") = " + y)
    case RandomNewOp(n: Exp[Int]) =>
      emitValDef(s, "new java.util.Random(" + n + ")")
    case RandomNextInt(r) =>
      emitValDef(s, r + ".nextInt")
    case Method(x, m) =>
      emitValDef(s, x + "." + m)
    case Print(x) =>
      emitValDef(s, "println(" + x + "(0))")
    case BinaryOp(x, op, y) =>
      emitValDef(s, x + " " + op + " " + y)
    case _ => emitPlain(d.toString)
  }

}
