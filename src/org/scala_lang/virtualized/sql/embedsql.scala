package org.scala_lang.virtualized.sql
import org.scala_lang.virtualized.CoreExps

import java.io._
import java.sql._
import scala.reflect.SourceLocation

trait SQLExps extends CoreExps {
  // marker to trigger __new reification
  class Record extends Row[Exp]

  case class ResultRow[T](fields: Map[String, Exp[_]]) extends Exp[T] 

  case class Select[T, U](tgt: Exp[U], field: String) extends Exp[T] {
    override def refStr: String = tgt.refStr +"."+ field
  }

  case class ListSelect[Tuple <: Record, T](t: Exp[List[Tuple]], f: Exp[Tuple] => Exp[T]) extends Exp[List[Tuple]] {
    def projectedNames: Iterable[String] = f(null) match {
      case ResultRow(fields) => fields.keys
    }
  }

  case class Table[Tuple <: Record](name: String)(implicit val loc: SourceLocation) extends Exp[List[Tuple]]
  
  implicit def liftList[T](x: List[T]): Exp[List[T]] = Const(x)
}

trait EmbedSQL extends SQLExps {
  def __new[T](args: (String, Exp[T] => Exp[_])*): Exp[T] =
    new ResultRow(args map {case (n, rhs) => (n, rhs(null))} toMap)

  implicit def selectOps(self: Exp[_ <: Record]) = new {
    def selectDynamic[T](n: String): Exp[T] = Select(self, n)
  }

  /*
  trait FilterOps[T] {
    def Where(f: Rep[T] => Rep[Boolean]): List[Rep[T]]
  }
  implicit def filterOps[T](x: List[Rep[T]]) = new FilterOps[T] {
    def Where(f: Rep[T] => Rep[Boolean]): List[Rep[T]] = {x map f; x} // just run the function so we get some output
  }
  */
  
  implicit def listSelectOps[Tuple <: Record](l: Exp[List[Tuple]]) = new {
    def Select[T](f: Exp[Tuple] => Exp[T]): Exp[List[Tuple]] =
      ListSelect(l, f)
  }
}

object Test extends App  {
  object Example extends EmbedSQL with SQLCodeGen {
    type Item = Record {
      val itemName: String
      val customerName: String
      val address: String
    }

    def prog = {
      val items = Table[Tuple]("items")
      items.Select(e => new Record { val customerName = e.customerName; val address = e.address })
      // items.Select(e => new Record { val customerName = e.customerName })
      //      .Select(e => new Record { val itemName = e.itemName }) /*Where (_.customerName <> "me")*/
    }
  }

  // setup DB to run query against
  Class.forName("org.h2.Driver")
  val con = DriverManager.getConnection("jdbc:h2:mem:testdb", "sa", "")
  val createTableStmt = """
CREATE TABLE Items
(
CustomerName varchar(255),
ItemName varchar(255)
)
"""
  val insertStmt = """
INSERT INTO Items
VALUES ('Typesafe', 'Chair')
"""
  val res = con.createStatement().execute(createTableStmt)
  val res2 = con.createStatement().execute(insertStmt)
  Example.runQuery(con, Example.prog)
  con.close()
}

trait SQLCodeGen extends SQLExps {
  def emitPlain(out: PrintWriter, s: String, more: Boolean = false) = {
    if (more) out.print(s) else out.println(s)
  }

  def emitExpr[T](out: PrintWriter, expr: Exp[T]): Unit = expr match {
    case Select(_, field) => emitPlain(out, field, true)
  }
  
  def emitSelector[T, S](out: PrintWriter, f: Exp[T] => Exp[S]): Unit = f(null) match {
    case ResultRow(fields) =>
      var first = true
      for ((name, value) <- fields) {
        if (first) { first = false } else emitPlain(out, ", ", true)
        emitExpr(out, value)
      }
  }
  
  def emitQuery[T](out: PrintWriter, expr: Exp[T]): Unit = expr match {
    case Table(name) =>
      emitPlain(out, name, true)
    case ListSelect(table, selector) =>
      emitPlain(out, "SELECT ", true)
      emitSelector(out, selector)
      emitPlain(out, " FROM ", true)
      emitQuery(out, table)
      emitPlain(out, "")
  }

  def runQuery[T](con: Connection, expr: Exp[T]) {
    val writer = new StringWriter
    val printer = new PrintWriter(writer)
    emitQuery(printer, expr)
    val query = writer.toString()
    val sta = con.createStatement()
    try {
      val res = sta.executeQuery(query)
      res.next()
      val s = res.getString(1)
      println("Result: " + s)
    } catch {
      case e: Exception =>
        expr match {
          case ListSelect(t, _) =>
            t match {
              case table @ Table(name) =>
                println("error executing query on table " + name +
                  " declared at " + table.loc.line + ": ")
                e.printStackTrace
              case _ =>
                e.printStackTrace
            }
        }
    }
  }
}
