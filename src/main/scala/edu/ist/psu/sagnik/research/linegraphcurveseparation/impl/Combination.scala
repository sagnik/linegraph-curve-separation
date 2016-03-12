package edu.ist.psu.sagnik.research.linegraphcurveseparation.impl

//import scala.reflect.api.TypeTags.TypeTag
import reflect.runtime.universe._

/**
 * Created by szr163 on 3/11/16.
 */

class Combination[A] {

  def combination[A](r: Int, ls: List[A], rejectFunc:(A,List[A])=>Boolean): List[List[A]] =
    if (r == 1) ls.map(x => List(x))
    else
      combination(
        r - 1,
        ls,
        rejectFunc
      ).flatMap(x => for (l <- ls if !rejectFunc(l,x)) yield x :+ l ).map(x=>x.toSet).distinct.map(y=>y.toList)
}

object TestCombination{

  //reject function will say if an element l should be put into a list ls. If it returns true, we should NOT put l in ls.
  def myrejectFunc[A](l:A, ls:List[A]):Boolean= ls.contains(l) ||
    //custom logic here
    {
      if ( l.isInstanceOf[Int] && ls.forall(a=>a.isInstanceOf[Int])){
        val ils=ls.map(x=>x.asInstanceOf[Int])
        val il=l.asInstanceOf[Int]
        (il::ils).contains(2)
        //!ils.exists(x=>((il-x)==1)||((x-il)==1))
      }
      else false
    }

  def main(args:Array[String])={
    val l=List(1,2,3,4,5)
    println(new Combination[Int].combination[Int](2,l,myrejectFunc))
  }
}
