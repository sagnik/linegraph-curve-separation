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
    }

  }

  def createsSquare(xs:Seq[SVGPathCurve]):Boolean={
    if ((xs.map(isHV(_)).forall(a=>a))) false //there's at least one non HV line
    xs.forall(a=>xs.exists(y=>hvTouches(a,y)))
  }

}
