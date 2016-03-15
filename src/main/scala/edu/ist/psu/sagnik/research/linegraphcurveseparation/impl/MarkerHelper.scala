package edu.ist.psu.sagnik.research.linegraphcurveseparation.impl

import edu.ist.psu.sagnik.research.linegraphcurveseparation.model.{Rectangle, SVGPathCurve}

/**
 * Created by szr163 on 3/11/16.
 */
object MarkerHelper {
  def isH(x:SVGPathCurve):Boolean={val bb=x.svgPath.bb.getOrElse(Rectangle(0f,0f,0f,0f)); (bb.y1==bb.y2) && (bb.x1!=0&&bb.y1!=0&&bb.x2!=0&&bb.y2!=0)}

  def isV(x:SVGPathCurve):Boolean={val bb=x.svgPath.bb.getOrElse(Rectangle(0f,0f,0f,0f)); (bb.x1==bb.x2) && (bb.x1!=0&&bb.y1!=0&&bb.x2!=0&&bb.y2!=0)}

  def isHV(x:SVGPathCurve):Boolean=isH(x) || isV(x)

  def hvTouches(p1:SVGPathCurve,p2:SVGPathCurve):Boolean=
    if (p1.equals(p2)) false
    else if (!isHV(p1) || !isHV(p2)) false
    else if (isH(p1) && isH(p2)) false
    else if (isV(p1) && isV(p2)) false
    //at this point we know the path has a bounding box, so getOrElse is not needed.
    else {
      val bbH = if (isH(p1)) p1.svgPath.bb.get else p2.svgPath.bb.get
      val bbV = if (isV(p1)) p1.svgPath.bb.get else p2.svgPath.bb.get
      (bbH.x1 == bbV.x1) || (bbH.x2 == bbV.x1)
    }



  def hvIntersects(p1:SVGPathCurve,p2:SVGPathCurve):Boolean=
    if (p1.equals(p2)) false
    else if (!isHV(p1) || !isHV(p2)) false
    else if (isH(p1) && isH(p2)) false
    else if (isV(p1) && isV(p2)) false
    //at this point we know the path has a bounding box, so getOrElse is not needed.
    else {
      val bbH = if (isH(p1)) p1.svgPath.bb.get else p2.svgPath.bb.get
      val bbV = if (isV(p1)) p1.svgPath.bb.get else p2.svgPath.bb.get
      !((bbH.x1 == bbV.x1) || (bbH.x2 == bbV.x1)) && Rectangle.rectInterSects(bbH,bbV)
    }



  def pathIntersects(p1:SVGPathCurve,p2:SVGPathCurve):Boolean=
    if (p1.equals(p2)) false
    else {
      val bb1 = p1.svgPath.bb.getOrElse(Rectangle(0f,0f,0f,0f))
      val bb2 = p2.svgPath.bb.getOrElse(Rectangle(0f,0f,0f,0f))
      if (Rectangle(0f,0f,0f,0f).equals(bb1) || Rectangle(0f,0f,0f,0f).equals(bb2))
        false
      else
        Rectangle.rectInterSects(bb1,bb2)
    }


  def nonHVTouches(p1:SVGPathCurve,p2:SVGPathCurve):Boolean=
    if (p1.equals(p2)) false
    else if (isHV(p1) || isHV(p2)) false
    else {
      val bb1 = p1.svgPath.bb.getOrElse(Rectangle(0f,0f,0f,0f))
      val bb2 = p2.svgPath.bb.getOrElse(Rectangle(0f,0f,0f,0f))
      if (Rectangle(0f,0f,0f,0f).equals(bb1) || Rectangle(0f,0f,0f,0f).equals(bb2))
        false
      else
        Rectangle.rectInterSects(bb1,bb2)//TODO: check correctness
    }


  //TODO: currently the code treats (left and right) & (top & bottom) carets as the same, change in future?
  def isLeftCaret(b1:Rectangle,b2:Rectangle):Boolean= !(Rectangle(0f,0f,0f,0f).equals(b1)||Rectangle(0f,0f,0f,0f).equals(b2))&&
    ((b1.y2==b2.y1)||(b1.y1==b2.y2))&&(b1.x1==b2.x1)

  def isRightCaret(b1:Rectangle,b2:Rectangle):Boolean= !(Rectangle(0f,0f,0f,0f).equals(b1)||Rectangle(0f,0f,0f,0f).equals(b2))&&
    ((b1.y2==b2.y1)||(b1.y1==b2.y2))&&(b1.x2==b2.x2)

  def isUpCaret(b1:Rectangle,b2:Rectangle):Boolean= !(Rectangle(0f,0f,0f,0f).equals(b1)||Rectangle(0f,0f,0f,0f).equals(b2))&&
    ((b1.x1==b2.x2)||(b1.x2==b2.x1))&&(b1.y1==b2.y1)

  def isDownCaret(b1:Rectangle,b2:Rectangle):Boolean= !(Rectangle(0f,0f,0f,0f).equals(b1)||Rectangle(0f,0f,0f,0f).equals(b2))&&
    ((b1.x1==b2.x2)||(b1.x2==b2.x1))&&(b1.y2==b2.y2)

  def isCaret(cs:List[SVGPathCurve],dir:String):Boolean=
    if (cs.length!=2) false
    else{
      val bbs=cs.map(x=>x.svgPath.bb.getOrElse(Rectangle(0f,0f,0f,0f)))
      if ("left".equals(dir)) isLeftCaret(bbs(0),bbs(1))
      else if ("right".equals(dir)) isRightCaret(bbs(0),bbs(1))
      else if ("up".equals(dir)) isUpCaret(bbs(0),bbs(1))
      else if ("down".equals(dir)) isDownCaret(bbs(0),bbs(1))
      else false
    }

  def isCaret(cs:List[SVGPathCurve]):Boolean=
    if (cs.length!=2) false
    else isCaret(cs,"left") || isCaret(cs,"right") || isCaret(cs,"up") || isCaret(cs,"down")

  def isCaret(cs:Seq[SVGPathCurve]):Boolean=isCaret(cs.toList)

  def isCaret(p1:SVGPathCurve,p2:SVGPathCurve,dir:String):Boolean= isCaret(List(p1,p2),dir)

  def createsCaret(xs:Seq[SVGPathCurve],dir:String): Boolean =
    (xs.map(a=>a.pathStyle).distinct.length==1) &&
      !xs.exists(a=>isHV(a)) &&
      isCaret(xs.toList,dir)

  /************************* actual shapes *****************************************************/
  def createsSquare(xs:Seq[SVGPathCurve]):Boolean=
    xs.length==4 &&
    (xs.map(a=>a.pathStyle).distinct.length==1) && //the paths are drawn with same style
      xs.forall(isHV(_)) && //there's no non HV line
      xs.forall(a=>xs.count(y=>hvTouches(a,y))==2)



  def createsStar(xs:Seq[SVGPathCurve]):Boolean=
    xs.length==4 &&
    (xs.map(a=>a.pathStyle).distinct.length==1) &&
      xs.count(a=>isHV(a))==2 && //two of the paths are non hV and two of them are HV
      xs.forall(x=>xs.count(y=>pathIntersects(x,y))==3) && //each path intersect with every other path
      xs.filter(x=> !isHV(x))(0).svgPath.bb.equals(xs.filter(x=> !isHV(x))(1).svgPath.bb) //non hv paths have the same bb.


  def createsDiamond(xs:Seq[SVGPathCurve]):Boolean=
    xs.length==4 &&
    (xs.map(a=>a.pathStyle).distinct.length==1) &&
      !xs.exists(a=>isHV(a)) && //there's no HV line
      {
        xs.combinations(2).toList.exists(a=>isCaret(a.toList,"up")) &&
          xs.combinations(2).toList.exists(a=>isCaret(a.toList,"left")) &&
          xs.combinations(2).toList.exists(a=>isCaret(a.toList,"right")) &&
          xs.combinations(2).toList.exists(a=>isCaret(a.toList,"down"))
      }


  def createsTriangle(xs:Seq[SVGPathCurve]):Boolean=
    xs.length == 3 &&
      (xs.map(a => a.pathStyle).distinct.length == 1) &&
      (xs.count(isHV(_)) == 1) && //there's exactly one HV line
      isCaret(xs.filter(!isHV(_)))




}
