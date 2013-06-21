package org.scala_lang.virtualized.prob

trait ProbLangExTraffic extends ProbLang {
  val lightsIt = Iterator("Red", "Yellow", "Green")
  val actionIt = Iterator("Stop", "Drive")
  val resultIt = Iterator("Crash", "NoCrash")

  val Red, Yellow, Green = lightsIt.next
  val Stop, Drive        = actionIt.next
  val Crash, NoCrash     = resultIt.next

  type Light = String
  type Action = String
  type Driver = Rand[Light] => Rand[Action]


  val trafficLight = choice(Red -> 0.5, Yellow -> 0.1, Green -> 0.4) dbg "light"

  def otherLight(light: Rand[Light]) = light map {
    case Red => Green
    case Yellow => Red
    case Green => Red
  } dbg "otherLight"
  def cautiousDriver(light: Rand[Light]) = light flatMap {
    case Red => always(Stop)
    case Yellow => choice(Stop -> 0.9, Drive -> 0.1)
    case Green => always(Drive)
  } dbg "cautiousDriver"
  def aggressiveDriver(light: Rand[Light]) = light flatMap {
    case Red => choice(Stop -> 0.9, Drive -> 0.1)
    case Yellow => choice(Stop -> 0.1, Drive -> 0.9)
    case Green => always(Drive)
  } dbg "aggressiveDriver"


  def crash(driver1: Driver, driver2: Driver, light: Rand[Light]) = {
    light flatMap { l =>
      val light = always(l)

      val d1 = driver1(light)
      val d2 = driver2(otherLight(light))
      (driver1(light) === always(Drive) && (driver2(otherLight(light)) === always(Drive))) flatMap {
        case true =>
          choice(Crash -> 0.9, NoCrash -> 0.1)
        case _ =>
          always(NoCrash)
      }
    }
  }

  def crash2(driver1: Driver, driver2: Driver, light: Rand[Light]) = {
    (driver1(light) === always(Drive) && (driver2(otherLight(light)) === always(Drive))) flatMap {
      case true =>
        choice(Crash -> 0.9, NoCrash -> 0.1) dbg "result"
      case _ =>
        always(NoCrash)
    }
  }



  val trafficModel = crash(cautiousDriver, aggressiveDriver, trafficLight)
  val trafficModel2 = crash(aggressiveDriver, aggressiveDriver, trafficLight)

  val trafficModel3 = crash2(cautiousDriver, aggressiveDriver, trafficLight)
  val trafficModel4 = crash2(aggressiveDriver, aggressiveDriver, trafficLight)


  val cond1 = {
    val x = flip(0.5)
    x flatMap {
      case true => always(1)
      case _ => x map  {
        case true => 2
        case _ => 3
      }
    }
  }

  val coinModel1 = {
    val coin = choice(0 -> 0.5, 1 -> 0.5)
    val sum1 = coin + coin
    val sum2 = sum1 + coin
    sum2
  }

  val coinModel2 = {
    val coin = choice(0 -> 0.5, 1 -> 0.5)
    val sum1 = coin + coin
    val sum2 = sum1 + coin
    (sum2 === always(3)) flatMap {
      case true => sum1
      case false => coin
    }
  }

}

trait AppendProg extends ProbLang {

  def randomList(): Rand[List[Boolean]] = flip(0.5).flatMap {
    case false => always(Nil)
    case true  =>
      val x = flip(0.5)
      val tail = randomList()
      x.flatMap(x => tail.map(xs=>x::xs))
  }

  def append[T](x: Rand[List[T]], y: Rand[List[T]]): Rand[List[T]] = x flatMap {
    case Nil => y
    case h::tl => append(always(tl),y).map(xs=>h::xs) // full list as input, not very efficient?
  }

  val t3 = List(true, true, true)
  val f2 = List(false, false)

  val appendModel1 = {
    append(always(t3),always(f2))
  }

  val appendModel2 = {
    append(flip(0.5).map(_::Nil),always(f2))
  }

  def appendModel3 = { // needs lazy strategy
    append(always(t3),randomList())
  }

  def appendModel4 = {
    // query: X:::f2 == t3:::f2 solve for X
    randomList().flatMap{ x =>
      append(always(x),always(f2)).flatMap {
        case res if res == t3:::f2 => always((x,f2,res))
        case _ => never
      }
    }
  }

  def appendModel5 = {
    // query: X:::Y == t3:::f2 solve for X,Y
    randomList().flatMap{ x =>
      randomList().flatMap{ y =>
        append(always(x),always(y)).flatMap {
          case res if res == t3:::f2 => always((x,y))
          case _ => never
    }}}
  }



  // now try lists where the tail itself is a random var

  abstract class CList[+A]
  case object CNil extends CList[Nothing]
  case class CCons[+A](hd: A, tl: Rand[CList[A]]) extends CList[A]

  def asCList[A](x: List[A]): Rand[CList[A]] = x match {
    case Nil => always(CNil)
    case x::xs => always(CCons(x, asCList(xs)))
  }
  def asLists[A](x: Rand[CList[A]]): Rand[List[A]] = x flatMap {
    case CNil => always(Nil)
    case CCons(x, xs) => asLists(xs).map(xs=>x::xs)
  }

  def randomCList(): Rand[CList[Boolean]] = flip(0.5).flatMap {
    case false => always(CNil)
    case true  =>
      val x = flip(0.5)
      val tail = randomCList()
      x.map(x => CCons(x, tail))
  }

  def appendC[T](x: Rand[CList[T]], y: Rand[CList[T]]): Rand[CList[T]] = x flatMap {
    case CNil => y
    case CCons(h,t) => always(CCons(h, appendC(t,y)))
  }

  def listSameC[T](x: Rand[CList[T]], y: Rand[CList[T]]): Rand[Boolean] =
    x.flatMap { u => y.flatMap { v => (u,v) match {
      case (CCons(a,x),CCons(b,y)) if a == b => listSameC(x,y)
      case (CNil,CNil) => always(true)
      case _ => always(false)
    }}}


  val t3c = asCList(t3)
  val f2c = asCList(f2)

  def appendModel3b = {
    asLists(appendC(t3c,randomCList()))
  }

  def appendModel4b = {
    // query: X:::f2 == t3:::f2 solve for X
    val x = randomCList()
    val t3f2 = t3++f2
    listSameC(appendC(x,f2c), asCList(t3f2)).flatMap {
      case true =>
        // here we rely on memoization: otherwise x.tail would be making new choices all the time,
        asLists(x).map(x=>(x,f2,t3f2))
      case _ => never
    }
  }

  def appendModel5b = {
    // query: X:::Y == t3:::f2 solve for X,Y
    val x = randomCList()
    val y = randomCList()
    listSameC(appendC(x,y), asCList(t3++f2)).flatMap {
      case true => for (a <- asLists(x); b <- asLists(y)) yield (a,b)
      case _    => never
    }
  }


}

trait RunTests extends ProbLang with ProbPrettyPrint with ProbLangExTraffic with AppendProg {
  show(cond1, "cond1")

  show(coinModel1, "coinModel1")
  show(coinModel2, "coinModel2")

  show(trafficModel, "trafficModel")
  show(trafficModel2, "trafficModel2")
  show(trafficModel3, "trafficModel3")
  show(trafficModel4, "trafficModel4")
}


object Test extends App {
  new RunTests with ProbCore {}
  new RunTests with ProbCoreLazy {
    // append needs lazyness
    show(appendModel1, "appendModel1")
    show(appendModel2, "appendModel2")
    show(appendModel3, "appendModel3", "", 5)
    show(appendModel4, "appendModel4", "", 1)
    show(appendModel5, "appendModel5", "", 5)

    show(appendModel3b, "appendModel3b", "", 5)
    show(appendModel4b, "appendModel4b", "", 1)
    show(appendModel5b, "appendModel5b", "", 5)
  }
}
