package org.scala_lang.virtualized
package js

/**
 * CoreDefs adds statements (also called definitions) to the core support for expressions.
 * 
 */
trait CoreDefs extends CoreExps {
  abstract class Def[T] 

  case class ScopeEntry[T](sym: Sym[T], rhs: Def[T]) { type Tp = T }
  type Scope = List[ScopeEntry[_]]
  var scopeDefs: List[Scope] = Nil

  case class Block[T](stms: Scope, e: Exp[T])

  // enter a new nested scope,
  // populate it with the definitions resulting from reifing `e`, collecting them in a Block
  // leave the scope and return the Block
  implicit def reifyBlock[T](e: => Exp[T]): Block[T] = {
    scopeDefs = Nil::scopeDefs // push a new nested scope onto the stack
    val r = e // evaluate e after going to a new nesting level for scopes -- toAtom calls will now populate the current scope
    val stms = scopeDefs.head // save the populated scope
    scopeDefs = scopeDefs.tail // pop it
    Block(stms, r) // wrap it up in a block
  }

  // append a new definition to the current scope
  // this reifies the sequencing of definitions
  implicit def toAtom[T](d: Def[T]): Exp[T] = {
    val sym = Sym[T]() // make a fresh symbol to refer to the definition-turned-expression
    scopeDefs = (scopeDefs.head :+ ScopeEntry(sym, d)) :: scopeDefs.tail // append it to the current scope
    sym // the expression-representation of the definition
  }
}

trait JSDefsExps extends CoreDefs {
  // definitions, or statements -- their sequencing will be captured when they are converted to Exp's by toAtom
  case class IfThenElse[T](c: Exp[Boolean], a: Block[T], b: Block[T]) extends Def[T]
  case class VarInit[T](x: Exp[T]) extends Def[T]
  case class VarAssign[T](v: Exp[T], x: Exp[T]) extends Def[Unit]

  
  /**
   * The representation of a binary operation: we'll only use >= here 
   */
  case class BinaryOp[T, U](x: Exp[T], op: String, y: Exp[T]) extends Def[U]
  
  /**
   * Any T for which there is an implicit Ordering[T] will be converted into a BinaryOps[T],
   * so that >= appears to be available on these T's -- calling these operations will
   * yield a BinaryOp statement that represents the operation.
   */
  trait BinaryOps[T] { val self: Exp[T]
    def >=(y: Exp[T]) = BinaryOp[T, Boolean](self, ">=", y)
  }
  implicit def orderingOps[T: Ordering](x: Exp[T]) = new BinaryOps[T]{ val self = x }

  // Obj and Select are Exp's: used on their own (as a statement) in a DSL program, 
  // they will not generate any code (since toAtom is not called, and thus they are not registered in the current scope)
  case class Select[T, U](tgt: Exp[U], field: String) extends Exp[T] {
    override def refStr: String = tgt.refStr +"."+ field
  }
  case class Obj[T](fields: Map[String, Exp[_]]) extends Exp[T] 
}

trait EmbedJS extends JSDefsExps {
  // note the return types! toAtom will be used to turn the Def's into Exp's
  def __ifThenElse[T](cond: Exp[Boolean], thenp: Exp[T], elsep: Exp[T]): Exp[T] = IfThenElse(cond, thenp, elsep)
  def __newVar[T](x: Exp[T]): Exp[T] = VarInit(x)
  def __assign[T](lhs: Exp[T], rhs: Exp[T]): Exp[Unit] = VarAssign(lhs, rhs)

  // marker to trigger __new reification
  class JSObj extends Row[Exp]

  def __new[T](args: (String, Exp[T] => Exp[_])*): Exp[T] = new Obj(args map {case (n, rhs) => (n, rhs(null))} toMap)

  implicit def selectOps(self: Exp[_ <: JSObj]) = new {
    def selectDynamic[T](n: String): Exp[T] = Select(self, n)
  }
}

// to run, right-click on "Test" below, and select Run As > Scala Application
object Test extends App  {
  object Example extends EmbedJS with JSCodeGen { def prog = {
    var kim = new JSObj { val name = "kim"; val age = 20 }
    kim.age = 21
    var allowedDrink = if (kim.age >= 21) "beer" else "milk"
    allowedDrink
  }}

  Example.emitBlock(Example.prog)
}

/* emitted code: {
  var x1 = {"name" : "kim","age" : 20}
  var x2 = (x1.age = 21)
  if (x1.age >= 21) {
    var x3 = "beer"
  } else {
    var x3 = "milk"
  }
  var x4 = x3
  x4
} */


// rudimentarily render Block, Def, and Expr as JavaScript code (printing to the console)
trait JSCodeGen extends JSDefsExps {
  var nesting = 0
  var indent = true

  def emitValDef[T](s: Sym[T], rhs: String, more: Boolean = false) = {
    emitPlain("var " + s.refStr + " = " + rhs, more)
  }

  def emitPlain(s: String, more: Boolean = false) = {
    if (indent) print(" " * (nesting * 2))
    if (more) print(s) else println(s)
    indent = !more
  }

  def emitBlock[T](a: Block[T], more: Boolean = false, s: Sym[T] = null) = a match {
    case Block(stms, e) =>
      emitPlain("{", false); nesting += 1
        stms foreach { case t: ScopeEntry[t] => emitNode[t](t.sym, t.rhs) }

        if(s == null) emitPlain(e.refStr)
        else emitValDef(s, e.refStr)
      nesting -= 1; emitPlain("}", more)
  }

  def emitNode[T](s: Sym[T], d: Def[T]): Unit = d match {
    case IfThenElse(c,a,b) => 
      emitPlain("if (", true); emitExpr(c); emitPlain(") ", true)
      emitBlock(a, true, s)
      emitPlain(" else ", true)
      emitBlock(b, false, s)
    case VarInit(x) => 
      emitPlain("var " + s.refStr + " = ", true); emitExpr(x); emitPlain("")
    case VarAssign(v, x) => 
      emitValDef(s, "(" + v.refStr + " = ", true); emitExpr(x); emitPlain(")")
    case BinaryOp(x, op, y) =>
      emitValDef(s, "", true); emitExpr(x); emitPlain(" "+ op +" ", true); emitExpr(y); emitPlain(")")
  }

  def emitExpr[T](expr: Exp[T]): Unit = expr match {
    case s@Sym(_) => emitPlain(s.refStr, true)
    case c@Const(_) => emitPlain(c.refStr, true); 
    case Select(tgt, field) => emitExpr(tgt); emitPlain("."+field, true)
    case Obj(fields) =>
      emitPlain("{", true)
      if(fields nonEmpty) {
        fields.head match { case (n, v) => emitPlain("\""+ n +"\" : ", true); emitExpr(v)}
        fields.tail foreach {case (n, v) => emitPlain(",", true); emitPlain("\""+ n +"\" : ", true); emitExpr(v)}
      }
      emitPlain("}", true)
  }
}