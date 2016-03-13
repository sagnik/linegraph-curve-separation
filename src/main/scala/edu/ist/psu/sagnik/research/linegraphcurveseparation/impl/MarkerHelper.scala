package edu.ist.psu.sagnik.research.linegraphcurveseparation.impl

import edu.ist.psu.sagnik.research.linegraphcurveseparation.model.{Rectangle, SVGPathCurve}

/**
 * Created by szr163 on 3/11/16.
 */
object MarkerHelper {
  def isH(x:SVGPathCurve):Boolean={val bb=x.svgPath.bb.getOrElse(Rectangle(0f,0f,0f,0f)); (bb.y1==bb.y2) && (bb.x1!=0&&bb.y1!=0&&bb.x2!=0&&bb.y2!=0)}

  def isV(x:SVGPathCurve):Boolean={val bb=x.svgPath.bb.getOrElse(Rectangle(0f,0f,0f,0f)); (bb.x1==bb.x2) && (bb.x1!=0&&bb.y1!=0&&bb.x2!=0&&bb.y2!=0)}

  def isHV(x:SVGPathCurve):Boolean=isH(x) || isV(x)

  def hvTouches(p1:SVGPathCurve,p2:SVGPathCurve):Boolean={
    if (p1.equals(p2)) false
    else if (!isHV(p1) || !isHV(p2)) false
    else if (isH(p1) && isH(p2)) false
    else if (isV(p1) && isV(p2)) false
    //at this point we know the path has a bounding box, so getOrElse is not needed.
    else {
      val bbH = if (isH(p1)) p1.svgPath.bb.get else p2.svgPath.bb.get
      val bbV = if (isV(p1)) p1.svgPath.bb.get else p2.svgPath.bb.get
      //(bbH.y1==(bbV.y1+bbV.y2)/2) && (bbV.x1==(bbH.x1+bbH.x2)/2)
      //!Rectangle.rectTouches(bbH,bbV)
      (bbH.x1 == bbV.x1) || (bbH.x2 == bbV.x1)
    }

  }

  def hvIntersects(p1:SVGPathCurve,p2:SVGPathCurve):Boolean={
    if (p1.equals(p2)) false
    else if (!isHV(p1) || !isHV(p2)) false
    else if (isH(p1) && isH(p2)) false
    else if (isV(p1) && isV(p2)) false
    //at this point we know the path has a bounding box, so getOrElse is not needed.
    else {
      val bbH = if (isH(p1)) p1.svgPath.bb.get else p2.svgPath.bb.get
      val bbV = if (isV(p1)) p1.svgPath.bb.get else p2.svgPath.bb.get
      //(bbH.y1==(bbV.y1+bbV.y2)/2) && (bbV.x1==(bbH.x1+bbH.x2)/2)
      //!Rectangle.rectTouches(bbH,bbV)
      !((bbH.x1 == bbV.x1) || (bbH.x2 == bbV.x1)) && Rectangle.rectInterSects(bbH,bbV)
      //Rectangle.rectInterSects(bbH,bbV)
    }

  }

  def nonHVIntersects(p1:SVGPathCurve,p2:SVGPathCurve):Boolean={
    if (p1.equals(p2)) false
    else if (isHV(p1) || isHV(p2)) false
    else {
      val bb1 = p1.svgPath.bb.getOrElse(Rectangle(0f,0f,0f,0f))
      val bb2 = p2.svgPath.bb.getOrElse(Rectangle(0f,0f,0f,0f))
      if (Rectangle(0f,0f,0f,0f).equals(bb1) || Rectangle(0f,0f,0f,0f).equals(bb2))
        false
      else
        Rectangle.rectInterSects(bb1,bb2)
    }
  }

  def nonHVTouches(p1:SVGPathCurve,p2:SVGPathCurve):Boolean={
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
  }

  def createsSquare(xs:Seq[SVGPathCurve]):Boolean={
    (xs.map(a=>a.pathStyle).distinct.length==1) &&
    (xs.map(a=>isHV(a)).forall(a=>a==true)) && //there's no non HV line
    xs.forall(a=>xs.filter(y=>hvTouches(a,y)).length==2)
  }

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
      dir match{
        case "left" => isLeftCaret(bbs(0),bbs(1))
        case "right" => isRightCaret(bbs(0),bbs(1))
        case "up" => isUpCaret(bbs(0),bbs(1))
        case "down" => isDownCaret(bbs(0),bbs(1))
        case _ => false
      }
    }

  def isCaret(p1:SVGPathCurve,p2:SVGPathCurve,dir:String):Boolean= {
      val bbs=List(p1,p2).map(x=>x.svgPath.bb.getOrElse(Rectangle(0f,0f,0f,0f)))
      dir match{
        case "left" => isLeftCaret(bbs(0),bbs(1))
        case "right" => isRightCaret(bbs(0),bbs(1))
        case "up" => isUpCaret(bbs(0),bbs(1))
        case "down" => isDownCaret(bbs(0),bbs(1))
        case _ => false
      }
  }

  def createsCaret(xs:Seq[SVGPathCurve],dir:String): Boolean =
    (xs.map(a=>a.pathStyle).distinct.length==1) &&
      (xs.map(a=>isHV(a)).forall(a=>a==false)) &&
     isCaret(xs.toList,dir)

  //TODO: incomplete
  def createsStar(xs:Seq[SVGPathCurve]):Boolean={
    (xs.map(a=>a.pathStyle).distinct.length==1) &&
    (xs.map(a=>isHV(a)).filter(a=>a==true).length==2 && xs.map(a=>isHV(a)).filter(a=>a==false).length==2)&& //two of the paths are non hV and two of them are HV
      hvIntersects(xs.groupBy(a=>isHV(a)).get(true).get(0),xs.groupBy(a=>isHV(a)).get(true).get(1)) //two hV paths intersect
  }

  def createsDiamond(xs:Seq[SVGPathCurve]):Boolean={
    (xs.map(a=>a.pathStyle).distinct.length==1) &&
      (xs.map(a=>isHV(a)).forall(a=>a==false)) && //there's no HV line
      {
        xs.combinations(2).toList.exists(a=>isCaret(a.toList,"up")) &&
          xs.combinations(2).toList.exists(a=>isCaret(a.toList,"left")) &&
          xs.combinations(2).toList.exists(a=>isCaret(a.toList,"right")) &&
          xs.combinations(2).toList.exists(a=>isCaret(a.toList,"down"))
      }
  }



}
