package edu.ist.psu.sagnik.research.linegraphcurveseparation.impl

//import scala.reflect.api.TypeTags.TypeTag
import reflect.runtime.universe._

/**
 * Created by szr163 on 3/11/16.
 */

//adopted from http://aperiodic.net/phil/scala/s-99/p26.scala
class Combination[A] {

  type A=Int

  def flatMapSublists[A,B](ls: List[A])(f: (List[A]) => List[B]): List[B] =
    ls match {
      case Nil => Nil
      case sublist@(_ :: tail) =>  f(sublist) ::: flatMapSublists(tail)(f)
    }


  //def myRejectFunc[A](xs:List[A]):Boolean=false

  //def testReject[A<:Int](x:A):Boolean=x>5
  /*
  def myRejectFunc[A](xs:List[A]):Boolean= A.type match{
    case Int => xs.forall(y=>xs.exists(x=>(y.asInstanceOf[Int]-x.asInstanceOf[Int])==1))
    case _ => false
  }
 */

  def myRejectFunc[A](xs:List[A]):Boolean=
    if(xs.forall(a=>a.isInstanceOf[Int])) xs.forall(y=>xs.exists(x=>(y.asInstanceOf[Int]-x.asInstanceOf[Int])==1))
    else false




  def combination[A](n: Int, ls: List[A]): List[List[A]] =
    if (n == 0) List(Nil)
    else flatMapSublists(ls) { sl =>
      combination(n - 1, sl.tail) map {sl.head :: _}
    }
}

object TestCombination{
  def main(args:Array[String])={
    val l=List(1,2,3,4)
    println(new Combination[Int].combination[Int](2,l))
  }
}
