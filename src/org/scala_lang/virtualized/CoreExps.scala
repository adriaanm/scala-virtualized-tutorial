package org.scala_lang.virtualized

/** 
 * CoreExps provides the basic types that are used to represent an expression-based language.
 */
trait CoreExps {
  /**
   * Exp[T] is the type of expressions that evaluate to a value of type T
   */
  abstract class Exp[T] {
    /**
     * Use refStr to generate a String reference to this expression
     * (for convenience during code generation)
     */
    def refStr: String = toString // TODO: remove
    override def toString = quoteExp(this)
  }

  def quoteExp[T](x: Exp[T]): String = x match {
    case Const(s: String) => "\""+ s +"\""
    case Const(x) => x.toString
    case Sym(id) => "x" + id
    case _ => "<unknown>"
  }

  /**
   * A Sym is a symbolic reference used internally to refer to expressions. 
   */
  object Sym { private var currId = 0 }
  case class Sym[T](id: Int = {Sym.currId += 1; Sym.currId}) extends Exp[T]

  /**
   * A statically known constant of type T, we'll use String and Int in this tutorial
   */
  case class Const[T](x: T) extends Exp[T]
  
  /**
   * Automatically lift Scala strings and integers into their DSL representation. 
   */
  implicit def liftString(x: String): Exp[String] = Const(x) 
  implicit def liftInt(x: Int): Exp[Int] = Const(x) 
}
