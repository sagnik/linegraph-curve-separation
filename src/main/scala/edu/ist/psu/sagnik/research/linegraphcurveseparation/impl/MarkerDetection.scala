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

    def hvTouches(x:SVGPathCurve,y:SVGPathCurve)=MarkerHelper.hvTouches(x,y)
    def createsSquare(xs:Seq[SVGPathCurve])=MarkerHelper.createsSquare(xs)
    def hvIntersects(x:SVGPathCurve,y:SVGPathCurve)=MarkerHelper.hvIntersects(x,y)

    //val testPaths= (curvePaths.combinations(4).filter(xs=>createsSquare(xs))).flatten.toIndexedSeq.distinct //this works, but extremely slow due to the combination step


    val (sqPaths,nonSqPaths)= curvePaths.partition(x=>curvePaths.filter(y=>hvTouches(x,y)).length>=2) //creates squares and rectangles. Can't distinguish between square and rectangles.
    println(s"number of square paths: ${sqPaths.length}")

    val (plusPaths,rest)=nonSqPaths.partition(x=>curvePaths.filter(y=>hvIntersects(x,y)).length==1)
    println(s"number of plus paths: ${plusPaths.length}")

    if (createImages) {
      if (sqPaths.nonEmpty) SVGWriter(sqPaths,loc,"sq")
      if (plusPaths.nonEmpty) SVGWriter(plusPaths,loc,"plus")

    }

  }


  def main(args: Array[String]):Unit= {
    val loc="data/10.1.1.100.3286-Figure-9.svg"
    //val loc="data/10.1.1.104.3077-Figure-1.svg"
    //val loc="data/10.1.1.105.5053-Figure-2.svg"
    MarkerDetection(loc,true)

  }

}
