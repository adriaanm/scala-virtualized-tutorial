package org.scala_lang.virtualized.patmat

trait MatcherSpec {
  type Exp[+T]
  type M[+T]

  abstract class Matcher {
    def zero: M[Nothing]
    def one[T](x: Exp[T]): M[T]
    def guard[T](cond: Exp[Boolean], then: => Exp[T]): M[T]
    def runOrElse[T, U](in: Exp[T])(matcher: Exp[T] => M[U]): Exp[U]
  }

  abstract class Maybe[+A] {
    def flatMap[B](f: Exp[A] => M[B]): M[B]
    def orElse[B >: A](alternative: => M[B]): M[B]
  }
}

trait Spec extends MatcherSpec {
  val __match: Matcher

  implicit def proxyMaybe[A](m: M[A]): Maybe[A]
  implicit def unit[T](e: T): Exp[T]

  def __unapply[T](kind: String, x: Exp[_]): M[T]
  object XNil {
    def unapply(x: Exp[_]): M[Unit] = __unapply("Nil", x)
  }
  object XCons {
    def unapply(x: Exp[_]): M[(Any, Any)] = __unapply("Cons", x)
  }

  def infix__1[A, B](t: Exp[(A, B)]): Exp[A]
  def infix__2[A, B](t: Exp[(A, B)]): Exp[B]

  def test1 = 7 match { case _ => "bar" }
  // ResultOrMatchError(One(bar))

  def test2 = 7 match { case 5 => "foo"; case _ => "bar" }
  // ResultOrMatchError(OrElse(FlatMap(IfThenElse(7==5, One(7), Zero), x, One(foo)), One(bar)))

  def test3 = List(1, 2, 3) match { case _ => "list" }
  // ResultOrMatchError(One(list))

  def test4 = List(1, 2, 3) match { case XNil() => "nil"; case _ => "other" }
  // ResultOrMatchError(OrElse(FlatMap(Unapply(Nil,List(1, 2, 3)),x2,One(nil)),One(other)))

  def test5 = List(1, 2, 3) match { case XNil() => "nil"; case XCons(_, _) => "other" }
  // ResultOrMatchError(OrElse(FlatMap(Unapply(Nil,List(1, 2, 3)),x3,One(nil)),FlatMap(Unapply(Cons,List(1, 2, 3)),x4,One(other))))

  def test6 = List(1, 2, 3) match { case XNil() => "default"; case XCons(hd, tl) => hd }
  // ResultOrMatchError(OrElse(FlatMap(Unapply(Nil,List(1, 2, 3)),x5,One(default)),FlatMap(Unapply(Cons,List(1, 2, 3)),x6,One(x6._1))))
}

trait Impl extends MatcherSpec {
  abstract class Exp[+T]
  type M[+T] = Exp[Maybe[T]]

  case class Const[T](e: T) extends Exp[T] { override def toString = e.toString }
  case class Sym[T](id: Int) extends Exp[T] { override def toString = "x" + id }
  case object Zero extends M[Nothing]
  case class One[T](x: Exp[T]) extends M[T]
  case class OrElse[T](x: M[T], y: M[T]) extends M[T]
  case class FlatMap[T,U](x: Exp[T], y: Exp[T], z: M[U]) extends M[U]
  case class ResultOrMatchError[T](x: M[T]) extends Exp[T]
  case class IfThenElse[T](x: Exp[Boolean], y: M[T], z: M[T]) extends M[T]
  case class Unapply[T, U](kind: String, x: Exp[T]) extends M[U]
  case class TupleSelect[T, U](field: Int, t: Exp[T]) extends Exp[U] { override def toString = t.toString + "._" + field }

  var id = 0
  def freshSym[T] = { id += 1; Sym(id) }

  object __match_impl extends Matcher {
    def zero: M[Nothing]                                              = Zero
    def one[T](x: Exp[T]): M[T]                                       = One(x)
    def guard[T](cond: Exp[Boolean], then: => Exp[T]): M[T]           = IfThenElse(cond, one(then), zero)
    def runOrElse[T, U](in: Exp[T])(matcher: Exp[T] => M[U]): Exp[U]  = ResultOrMatchError(matcher(in))
  }

  implicit def proxyMaybe[A](self: M[A]): Maybe[A] = new Maybe[A] {
    def flatMap[B](f: Exp[A] => M[B]): M[B]   = { val x = freshSym[A]; FlatMap(self, x, f(x)) }
    //def map[B](f: Exp[A] => Exp[B])           = flatMap(x => One(f(x)))
    def orElse[B >: A](alt: => M[B]): M[B]    = OrElse(self, alt)
  }
  implicit def unit[T](e: T): Exp[T] = Const(e)

  def __unapply[T](kind: String, x: Exp[_]): M[T] = Unapply(kind, x)

  def infix__1[A, B](t: Exp[(A, B)]): Exp[A] = TupleSelect(1, t)
  def infix__2[A, B](t: Exp[(A, B)]): Exp[B] = TupleSelect(2, t)
}

object Test extends App with Spec with Impl {
  override val __match = __match_impl

  println(test1)
  println(test2)
  println(test3)
  println(test4)
  println(test5)
  println(test6)
}
