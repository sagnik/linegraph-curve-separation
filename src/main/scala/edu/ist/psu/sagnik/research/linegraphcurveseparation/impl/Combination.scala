package edu.ist.psu.sagnik.research.linegraphcurveseparation.impl

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
      ).flatMap(x => for (l <- ls if !(x.contains(l) || rejectFunc(l,x))) yield x :+ l ).map(x=>x.toSet).distinct.map(y=>y.toList)

  def combinationTL[A](r: Int, startingR: Int, ls: List[A], rejectFunc:(A,List[A])=>Boolean, combs:List[List[A]]): List[List[A]] =
    if (r == startingR) combs
    else
      combinationTL(
        r,
        startingR+1,
        ls,
        rejectFunc,
       combs.flatMap(x => for (l <- ls if !(x.contains(l) || rejectFunc(l,x))) yield x :+ l ).map(x=>x.toSet).distinct.map(y=>y.toList)
      )
}

object TestCombination{

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns")
    result
  }

  //reject function will say if an element l should be put into a list ls. If it returns true, we should NOT put l in ls.
  def myrejectFunc[A](l:A, ls:List[A]):Boolean= //custom logic here, ex, a 3 combintation of elements with 2 not present.
      if ( l.isInstanceOf[Int] && ls.forall(a=>a.isInstanceOf[Int])){
        val ils=ls.map(x=>x.asInstanceOf[Int])
        val il=l.asInstanceOf[Int]
        (il::ils).contains(2)
        //!ils.exists(x=>((il-x)==1)||((x-il)==1))
      }
      else false

  def myrejectFunc2[A](l:A, ls:List[A]):Boolean= false

  def main(args:Array[String]):Unit={
    val l=(1 to 1000).toList
    val r=2
    //println(l.combinations(2).filterNot(x=>x.contains(2)).toList)
    //println(new Combination[Int].combinationTL[Int](2,1,l,myrejectFunc,l.map(x=>List(x))))

    println("computation started")
    assert(
    time{l.combinations(r).filterNot(x=>x.contains(2)).toList}
      ==
    time{new Combination[Int].combinationTL[Int](r,1,l,myrejectFunc,l.map(x=>List(x)))}
    )
  }
}
