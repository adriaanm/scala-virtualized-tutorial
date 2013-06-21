package org.scala_lang.virtualized.prob

trait ProbIntf {
  type Prob = Double
  type Rand[+A] <: RandImpl[A]
  trait RandImpl[+A] {
    var name = super.toString
    override def toString = name
    def dbg(n:String): this.type = { name = n; this }
    def flatMap[B](f: A => Rand[B]): Rand[B]
    def map[B](f: A => B): Rand[B] = flatMap(x => always(f(x)))
    def orElse[B >: A](that: Rand[B]): Rand[B]
  }
  def always[A](x: A) = choice(x -> 1.0)
  def never = choice()
  def flip(p: Double): Rand[Boolean] = choice(true -> p, false -> (1-p))
  def uniform[A](xs: A*): Rand[A] = choice(xs.map((_,1.0)):_*)
  def choice[A](xs: (A,Prob)*): Rand[A]
  def collapse[A](r: Rand[A]): List[(A,Prob)]
  def collapse2[A](r: Rand[A], strategy: String, solutions: Int): List[(A,Prob)]
}

trait ProbCore extends ProbIntf {
  type Rand[+A] = RandVar[A]
  case class Choice[+A](rv: Int, v: A, p: Prob)
  type Path = List[Choice[Any]]
  type Dist[+A] = List[Path]

  case class RandVar[+A](dist: Dist[A]) extends RandImpl[A] {
    def flatMap[B](f: A => Rand[B]): Rand[B] =
      RandVar(dist.flatMap(path => f(path.last.v.asInstanceOf[A]).dist.map(post => path ++ post)))
    def orElse[B >: A](that: Rand[B]): Rand[B] =
      RandVar(dist ++ that.dist)
  }

  def factor[A](w: Prob, xs: List[(A,Prob)]): List[(A,Prob)] = {
    xs.map{case (x,p) => (x,p*w)}
  }
  def consolidate[A](xs: List[(A,Prob)]): List[(A,Prob)] = {
    xs.filter(_._2 > 0).groupBy(_._1).map{case (x,ps) => (x, ps.map(_._2).sum)}.toList.sortBy{case (x,p) => (-p,x.toString)}
  }
  def normalize[A](xs: List[(A,Prob)]): List[(A,Prob)] = {
    val weight = xs.map(_._2).sum
    factor(1/weight,xs) // 1/0 ?
  }

  var numChoices = 0
  def freshChoiceId() = { numChoices += 1; numChoices - 1 }

  def choice[A](xs: (A,Prob)*): Rand[A] = {
    val id = freshChoiceId()
    RandVar[A](xs.toList.map{case(x,p) => List(Choice(id,x,p))})
  }

  def collapse[A](r: Rand[A]): List[(A,Prob)] = {
    def prob(path: Path, env: Map[Int,Any] = Map.empty): Prob = path match {
      case Choice(r,x,p)::rest =>
        env.get(r) match {
          case Some(`x`) => prob(rest,env)
          case None => p * prob(rest,env + (r -> x))
          case _ => 0
        }
      case _ => 1.0
    }
    normalize(consolidate(r.dist.map(path => (path.last.v, prob(path))))).asInstanceOf[List[(A,Prob)]]
  }

  def collapse2[A](r: Rand[A], strategy: String, solutions: Int): List[(A,Prob)] = ???
}


trait ProbCoreLazy extends ProbIntf {
  type Rand[+A] = RandVar[A]
  abstract class RandVar[+A] extends RandImpl[A] { self =>
    def flatMap[B](f: A => Rand[B]): Rand[B] =
      RandVarFlatMap(this, f)
    def orElse[B >: A](that: Rand[B]): Rand[B] =
      RandVarOrElse(this, that)
  }

  case class RandVarChoice[+A](id: Int, dist: List[(A,Prob)]) extends RandVar[A]
  case class RandVarFlatMap[A,+B](x: RandVar[A], f: A => Rand[B]) extends RandVar[B]
  case class RandVarOrElse[+A](x: RandVar[A], y: RandVar[A]) extends RandVar[A]



  def factor[A](w: Prob, xs: List[(A,Prob)]): List[(A,Prob)] = {
    xs.map{case (x,p) => (x,p*w)}
  }
  def consolidate[A](xs: List[(A,Prob)]): List[(A,Prob)] = {
    xs.filter(_._2 > 0).groupBy(_._1).map{case (x,ps) => (x, ps.map(_._2).sum)}.toList.sortBy{case (x,p) => (-p,x.toString)}
  }
  def normalize[A](xs: List[(A,Prob)]): List[(A,Prob)] = {
    val weight = xs.map(_._2).sum
    factor(1/weight,xs) // 1/0 ?
  }

  var numChoices: Int = _ //0 <--- BEWARE OF INIT ORDER!!!
  def freshChoiceId() = { numChoices += 1; numChoices - 1 }

  def choice[A](xs: (A,Prob)*): Rand[A] = {
    val id = freshChoiceId()
    new RandVarChoice[A](id, xs.toList)
  }

  def collapse[A](r: Rand[A]): List[(A,Prob)] = {
    type R = List[(A,Prob)]
    def prob[B](path: RandVar[B], p: Prob, env: Map[Int,Any] = Map.empty)(next: (B,Prob,Map[Int,Any]) => R): R = path match {
      case RandVarChoice(id,dist) =>
        env.get(id) match {
          case Some(x) =>
            assert(dist exists (_._1 == x), x+" not in "+dist+" for "+id)
            next(x.asInstanceOf[B],p,env)
          case None =>
            dist flatMap { case (x,q) =>
              next(x, p*q,env + (id -> x))
            }
        }
      case RandVarFlatMap(x,f) =>
        prob(x,p,env) { (y,q,e) => prob(f(y),q,e)(next) }
      case RandVarOrElse(x,y) =>
        prob(x,p,env)(next) ++ prob(y,p,env)(next)
    }
    normalize(consolidate(prob(r,1)((x,p,e)=>List(x->p))))
  }

  def collapse2[A](r: Rand[A], strategy: String, solutions: Int): List[(A,Prob)] = {
    println("searching for min "+solutions+" solutions")
    type R = List[(A,Prob)]
    var more = true
    type Mem = List[(Int, AnyRef, Any, Any)] // memo table: (id hash, function, arg, res)
    type Env = Map[Int,Any]

    def prob[B](path: RandVar[B], p: Prob, mem: Mem, env: Env, budget: Int)(next: (B,Prob,Mem,Env,Int) => R): R =
    if (budget < 0) { more = true; Nil } else path match {
      case RandVarChoice(id,dist) =>
        env.get(id) match {
          case Some(x) =>
            assert(dist exists (_._1 == x), x+" not in "+dist+" for "+id)
            next(x.asInstanceOf[B],p,mem,env,budget)
          case None =>
            val budget1 = if (dist.lengthCompare(1) <= 0) budget else budget-1 // certain choice doesn't count for depth
            dist flatMap { case (x,q) =>
              next(x, p*q, mem, env + (id -> x), budget1)
            }
        }
      case RandVarFlatMap(x,f) =>
        prob(x,p,mem,env,budget) { (y,q,m,e,k) =>
          // we memoize (continuation,value) pairs.
          // thankfully, closures have identity in Scala.
          // we could also attach ids to flatMap objects?
          m.find(el => (el._2 eq f) && (el._3 == y)) match {
            case Some((id,el,arg,res)) => prob(res.asInstanceOf[RandVar[B]],q,m,e,k)(next)
            case None => val res = f(y); prob(res,q,(System.identityHashCode(f),f,y,res)::m,e,k)(next)
          }
        }
      case RandVarOrElse(x,y) =>
        prob(x,p,mem,env,budget)(next) ++ prob(y,p,mem,env,budget)(next)
    }

    var res: R = Nil
    var depth = 1
    while (res.length < solutions && more) {
      println("trying depth "+depth)
      more = false
      res = prob(r,1,Nil,Map.empty,depth)((x,p,m,e,k)=>List(x->p))
      depth += 1
    }
    //println(ids.sorted.mkString("\n"))
    // todo: don't throw away all solutions each time, print them as
    // they are discovered (solutions=5 will never give an answer if
    // there are only 3)
    normalize(consolidate(res))
  }

}




trait ProbPrettyPrint extends ProbIntf {
  def pp[A](r: Rand[A], strategy: String, solutions: Int) =
    (if (solutions > 0) collapse2(r,strategy,solutions) else collapse(r)).map{case (x,p) => x + " : " + p}.mkString("\n")
  def show[A](r: Rand[A], desc: String = "", strategy: String = "", solutions: Int = -1) = {
    println(desc)
    println(pp(r,strategy,solutions))
    println("")
  }
}

trait ProbLang extends EmbeddedControls with ProbIntf {

  def liftOp2[A,B,C](x: Rand[A], y: Rand[B])(f: (A,B) => C): Rand[C] = for (a <- x; b <- y) yield f(a,b)

  def infix_&&(x: Rand[Boolean], y: Rand[Boolean]): Rand[Boolean] = liftOp2(x,y)(_ && _) // short circuit ??
  def infix_===[A](x: Rand[A], y: Rand[A]): Rand[Boolean] =         liftOp2(x,y)(_ == _)
  def infix_+(x: Rand[Int], y: Rand[Int]): Rand[Int] =              liftOp2(x,y)(_ + _)

}
