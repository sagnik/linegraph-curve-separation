package edu.ist.psu.sagnik.research.linegraphcurveseparation.impl

import edu.ist.psu.sagnik.research.linegraphcurveseparation.model.{SVGCurve, Rectangle, SVGPathCurve}
import edu.ist.psu.sagnik.research.linegraphcurveseparation.pathparser.model.{MovePath, CordPair}
import edu.ist.psu.sagnik.research.linegraphcurveseparation.reader.XMLReader
import edu.ist.psu.sagnik.research.linegraphcurveseparation.writer.SVGWriter

/**
 * Created by szr163 on 3/10/16.
 */
object MarkerDetection {


  def apply(loc:String,createImages:Boolean)={
    val svgPaths=
      if (loc.contains("-sps")) //this SVG has already paths split
        SVGPathExtract(loc, true)
      else
        SVGPathExtract(loc,false).flatMap(
          c=>
            SplitPaths.splitPath(
              c.svgPath.pOps.slice(1,c.svgPath.pOps.length),
              c,
              CordPair(c.svgPath.pOps(0).args(0).asInstanceOf[MovePath].eP.x,c.svgPath.pOps(0).args(0).asInstanceOf[MovePath].eP.y),
              Seq.empty[SVGPathCurve]
            )
        )
    val (fillExists,noFill)=svgPaths.partition(x=> {
      (x.pathStyle.fill match{
        case Some(fill) => true
        case _ => false
      }) && ("none".equals(x.pathStyle.stroke.getOrElse("none")) || "#ffffff".equals(x.pathStyle.stroke.getOrElse("#ffffff")))
    }
    )

    //TODO: possible exceptions
    val height = ((XMLReader(loc) \\ "svg")(0) \@ "height").toFloat
    val width = ((XMLReader(loc) \\ "svg")(0) \@ "width").toFloat
    val (axes,tics,cPaths)=SeparateAxesGridTickPaths(noFill,width,height)
    val curvePaths=cPaths.filterNot(x=>{x.svgPath.bb match {case Some(bb)=> bb.x1==bb.x2 && bb.y1==bb.y2; case _ => false}})

    def isHV(x:SVGPathCurve)=MarkerHelper.isHV(x)
    def isH(x:SVGPathCurve)=MarkerHelper.isH(x)
    def isV(x:SVGPathCurve)=MarkerHelper.isV(x)
    def hvTouches(x:SVGPathCurve,y:SVGPathCurve)=MarkerHelper.hvTouches(x,y)
    def createsSquare(xs:Seq[SVGPathCurve])=MarkerHelper.createsSquare(xs)

    val (testPaths,rest)= curvePaths.partition(x=>curvePaths.exists(y=>hvTouches(x,y))) //creates squares
    //val testPaths= (curvePaths.combinations(4).filter(xs=>createsSquare(xs))).flatten.toIndexedSeq.distinct //this works, but extremely slow due to the combination step
    println(testPaths.length)

    if (createImages) SVGWriter(testPaths,loc,"sq")

  }


  def main(args: Array[String]):Unit= {
    val loc="data/10.1.1.100.3286-Figure-9.svg"
    MarkerDetection(loc,true)

  }

}
