package org.scala_lang.virtualized

/**
 * CoreDefs adds support for statements (also called definitions).
 * 
 * There is no special support for reifying statements in virtualized Scala.
 * You can't virtualize the semi-colon, so to speak (in principle you could, but we don't).
 * 
 * Instead, statements in the domain program (values of type Def[T]) are embedded as Scala statements that
 * have a single side-effect (from the host language's point of view): 
 *   register a new definition in the current scope (the scope that represents a part of the domain program)
 *   
 * This side-effect is implemented by toAtom, which largely remains hidden from the user 
 * (both of the DSL framework and the DSL itself) by virtue of being an implicit conversion.
 * Thus, it is driven by type information: a statement is of type Def[T], but the virtualizing
 * methods are set up to return Exp[T]'s.
 */
trait CoreDefs extends CoreExps {
  /**
   * A Definition
   */
  abstract class Def[T] 

  /**
   * Track a definition and the unique symbol that is used to refer to it
   */
  case class ScopeEntry[T](sym: Sym[T], rhs: Def[T])
  
  /**
   * A Scope is simply a list of ScopeEntries
   */
  type Scope = List[ScopeEntry[_]]
  
  /**
   * This is populated by toAtom.
   */
  var scopeDefs: List[Scope] = Nil

  /**
   * A block consists of a sequence of definitions (a scope), 
   * and a result expression 
   */
  case class Block[T](stms: Scope, e: Exp[T])

  /** 
   * Creates a block by collecting the definitions that are entered in scope 
   * during the evaluation of the argument `e`.
   * 
   * This operation increases the nesting level by creating a new nested scope. 
   * Call it when entering a block.
   */
  implicit def reifyBlock[T](e: => Exp[T]): Block[T] = {
    scopeDefs = Nil::scopeDefs // push a new nested scope onto the stack
    val r = e // evaluate e after going to a new nesting level for scopes -- toAtom calls will now populate the current scope
    val stms = scopeDefs.head // save the populated scope
    scopeDefs = scopeDefs.tail // pop it
    Block(stms, r) // wrap it up in a block
  }

  /** 
   * Append a new definition to the current scope.
   * This reifies the sequencing of definitions
   */
  implicit def toAtom[T](d: Def[T]): Exp[T] = {
    val sym = Sym[T]() // make a fresh symbol to refer to the definition-turned-expression
    scopeDefs = (scopeDefs.head :+ ScopeEntry(sym, d)) :: scopeDefs.tail // append it to the current scope
    sym // the expression-representation of the definition
  }
}