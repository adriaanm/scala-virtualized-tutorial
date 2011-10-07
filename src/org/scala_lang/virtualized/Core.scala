package org.scala_lang.virtualized

/** 
 * CoreExps provides the basic types that are used to represent an expression-based language.
 */
trait CoreExps {
  /**
   * Exp[T] is the type of expressions that evaluate to a value of type T
   */
  trait Exp[T] {
    /**
     * Use refStr to generate a String reference to this expression
     * (for convenience during code generation)
     */
    def refStr: String = error("can't refer to "+ this)
  }

  /**
   * A Sym is a symbolic reference used internally to refer to expressions. 
   */
  object Sym { private var currId = 0 }
  case class Sym[T](id: Int = {Sym.currId += 1; Sym.currId}) extends Exp[T] {
    override def refStr: String = "x"+ id
  }

  /**
   * A statically known constant of type T, we'll use String and Int in this tutorial
   */
  case class Const[T](x: T) extends Exp[T] {
    override def refStr = x match { case s: String => "\""+ s +"\"" case _ => x.toString}
  }
  
  /**
   * Automatically lift Scala strings and integers into their DSL representation. 
   */
  implicit def liftString(x: String): Exp[String] = Const(x) 
  implicit def liftInt(x: Int): Exp[Int] = Const(x) 
}
