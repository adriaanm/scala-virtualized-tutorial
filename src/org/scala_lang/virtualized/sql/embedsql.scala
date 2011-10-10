package org.scala_lang.virtualized.sql
import org.scala_lang.virtualized.CoreExps

import scala.reflect.SourceLocation

trait SQLExps extends CoreExps {
  // marker to trigger __new reification
  class Result extends Row[Exp]

  type Tuple <: Result
  
  case class ResultRow[T](fields: Map[String, Exp[_]]) extends Exp[T] 

  case class Select[T, U](tgt: Exp[U], field: String)(val loc: SourceLocation) extends Exp[T] {
    override def refStr: String = tgt.refStr +"."+ field
  }

  case class ListSelect[T](t: Exp[List[Tuple]], f: Exp[Tuple] => Exp[T]) extends Exp[List[Tuple]] {
    def projectedNames: Iterable[String] = f(null) match {
      case ResultRow(fields) => fields.keys
    }
  }

  case class Table(name: String) extends Exp[List[Tuple]]
  
  implicit def liftList[T](x: List[T]): Exp[List[T]] = Const(x)
}

trait EmbedSQL extends SQLExps {
  def __new[T](args: (String, Exp[T] => Exp[_])*): Exp[T] =
    new ResultRow(args map {case (n, rhs) => (n, rhs(null))} toMap)

  implicit def selectOps(self: Exp[_ <: Result])(implicit loc: SourceLocation) = new {
    def selectDynamic[T](n: String): Exp[T] = Select(self, n)(loc)
  }

  /*
  trait FilterOps[T] {
    def Where(f: Rep[T] => Rep[Boolean]): List[Rep[T]]
  }
  implicit def filterOps[T](x: List[Rep[T]]) = new FilterOps[T] {
    def Where(f: Rep[T] => Rep[Boolean]): List[Rep[T]] = {x map f; x} // just run the function so we get some output
  }
  */
  
  implicit def listSelectOps(l: Exp[List[Tuple]]) = new {
    def Select[T](f: Exp[Tuple] => Exp[T]): Exp[List[Tuple]] =
      ListSelect(l, f)
  }
}

object Test extends App  {
  object Example extends EmbedSQL with SQLCodeGen {
    type Tuple = Result {
      val itemName: String
      val customerName: String
    }

    def prog = {
      val items = Table("items")
      items.Select(e => new Result { val customerName = e.customerName })
           .Select(e => new Result { val itemName = e.itemName }) /*Where (_.customerName <> "me")*/
    }
  }

  Example.emitQuery(Example.prog)
}

trait SQLCodeGen extends SQLExps {
  def emitPlain(s: String, more: Boolean = false) = {
    if (more) print(s) else println(s)
  }

  def emitExpr[T](expr: Exp[T]): Unit = expr match {
    case Select(_, field) => emitPlain(field, true)
  }
  
  def emitSelector[T, S](f: Exp[T] => Exp[S]): Unit = f(null) match {
    case ResultRow(fields) =>
      var first = true
      for ((name, value) <- fields) {
        if (first) { first = false } else emitPlain(", ", true)
        emitExpr(value)
      }
  }
  
  def emitQuery[T](expr: Exp[T]): Unit = expr match {
    case Table(name) =>
      emitPlain(name, true)
    case ListSelect(table, selector) =>
      // possible problem: `selector` might access a field that does
      // not exist in the tuples in the list that we select from (table)
      // if table is a ListSelect, we can find out which fields are projected out
      table match {
        case sel @ ListSelect(_, _) =>
          selector(null) match {
            case ResultRow(fields) =>
              for ((field, initializer) <- fields) {
                initializer match {
                  case fieldSel @ Select(_, name) =>
                    if (!sel.projectedNames.toList.contains(name)) {
                      val loc = fieldSel.loc // source location of `Select` call
                      error(loc.fileName + ":" + loc.line + ": error: selecting non-existant field " + name)
                    }
                }
              }
          }
        case _ =>
      }
      emitPlain("SELECT ", true)
      emitSelector(selector)
      emitPlain(" FROM ", true)
      emitQuery(table)
      emitPlain("")
  }
}
