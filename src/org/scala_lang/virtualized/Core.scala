package org.scala_lang.virtualized

trait CoreExps {
  trait Exp[T] {
    def refStr: String = error("can't refer to "+ this)
  }

  object Sym { private var currId = 0 }
  case class Sym[T](id: Int = {Sym.currId += 1; Sym.currId}) extends Exp[T] {
    override def refStr: String = "x"+ id
  }

  case class Const[T](x: T) extends Exp[T] {
    override def refStr = x match { case s: String => "\""+ s +"\"" case _ => x.toString}
  }
  implicit def liftString(x: String): Exp[String] = Const(x) 
  implicit def liftInt(x: Int): Exp[Int] = Const(x) 

  case class BinaryOp[T, U](x: Exp[T], op: String, y: Exp[T]) extends Exp[U]
  trait BinaryOps[T] { val self: Exp[T]
    def >=(y: Exp[T]) = BinaryOp[T, Boolean](self, ">=", y)
    def <>(y: Exp[T]) = BinaryOp[T, Boolean](self, "<>", y)
  }
  implicit def orderingOps[T: Ordering](x: Exp[T]) = new BinaryOps[T]{ val self = x }
}
