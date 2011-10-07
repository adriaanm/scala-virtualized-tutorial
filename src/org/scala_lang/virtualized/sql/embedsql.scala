package org.scala_lang.virtualized
package sql

trait SQLExps extends CoreExps {
  case class Row[T](fields: Map[String, Exp[_]]) extends Exp[T] 
}

trait EmbedSQL extends SQLExps {
  // marker to trigger __new reification
  class Result extends Row[Exp]

  def __new[T](args: (String, Exp[T] => Exp[_])*): Exp[T] = new Row(args map {case (n, rhs) => (n, rhs(null))} toMap)

  implicit def selectOps(self: Exp[_ <: JSObj]) = new {
    def selectDynamic[T](n: String): Exp[T] = Select(self, n)
  }

  trait FilterOps[T] {
    def Where(f: Rep[T] => Rep[Boolean]): List[Rep[T]]
  }
  implicit def filterOps[T](x: List[Rep[T]]) = new FilterOps[T] {
    def Where(f: Rep[T] => Rep[Boolean]): List[Rep[T]] = {x map f; x} // just run the function so we get some output
  }
}

object Test extends App  {
  object Example extends EmbedSQL with SQLCodeGen { def prog = {
    lineItems Select (e => new Result { val customerName = e.customerName }) Where (_.customerName <> "me")
  }}

  Example.emitQuery(Example.prog)
}
