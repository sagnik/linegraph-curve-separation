package edu.ist.psu.sagnik.research.linegraphcurveseparation.impl

import edu.ist.psu.sagnik.research.linegraphcurveseparation.model.{Rectangle, SVGPathCurve}
import edu.ist.psu.sagnik.research.linegraphcurveseparation.pathparser.impl.SVGPathfromDString
import edu.ist.psu.sagnik.research.linegraphcurveseparation.pathparser.model.{MovePath, CordPair}
import edu.ist.psu.sagnik.research.linegraphcurveseparation.reader.XMLReader
import edu.ist.psu.sagnik.research.linegraphcurveseparation.writer.SVGWriter

/**
 * Created by sagnik on 3/8/16.
 */
object MarkerDetection {

  val TMTHRESHOLD=10f
  def apply(loc:String):Unit={
    val svgpathCurves= SVGPathExtract(loc)

    //TODO: possible exceptions
    val height = ((XMLReader(loc) \\ "svg")(0) \@ "height").toFloat
    val width = ((XMLReader(loc) \\ "svg")(0) \@ "width").toFloat

    //note that we are allowing paths to be small in ONE dimensions, not in both dimensions.
    //TODO: This will fuck up things when each axis line is drawn by a separate path.

    val (possibleTicsMarkers,possibleAxes) = svgpathCurves.partition(x=>
      x.svgPath.bb match{
        case Some(bb) => (
          bb.x2-bb.x1< (TMTHRESHOLD) ||
            bb.y2-bb.y1<(TMTHRESHOLD)
          )
        case _ => false
      }
    )
    val axes=possibleAxes.filter(x=>isAxes(x,width,height))
    val tics=possibleTicsMarkers.filter(x=>
      CreateCurves.pathIsHV(
        x.svgPath.pOps.slice(1,x.svgPath.pOps.length),
        CordPair(x.svgPath.pOps(0).args(0).asInstanceOf[MovePath].eP.x,x.svgPath.pOps(0).args(0).asInstanceOf[MovePath].eP.y),
        Seq.empty[Boolean]
      ).forall(x=>x)&&
      axes.exists(a=>pathOverlap(x,a))
    )

    //(axes++tics).foreach(x=>println(x.svgPath.id))
    println(s"length: ${(tics).length}")

    SVGWriter(axes++tics,loc,"ats") //axes+tics

  }

  def pathOverlap(a:SVGPathCurve,b:SVGPathCurve):Boolean={
    (a.svgPath.bb,b.svgPath.bb) match{
      case (Some(aBB),Some(bBB))=>Rectangle.rectTouches(Rectangle(aBB.x1-1f,aBB.y1-1f,aBB.x2+1,aBB.y2+1f),bBB)
      //case (Some(aBB),Some(bBB))=>Rectangle.rectInterSects(aBB,bBB)
      case _ => false
    }
  }

  val AXESRATIOTHRESHOLD=0.5f
  def isAxes(x:SVGPathCurve,W:Float,H:Float):Boolean={
    CreateCurves.pathIsHV(
      x.svgPath.pOps.slice(1,x.svgPath.pOps.length),
      CordPair(x.svgPath.pOps(0).args(0).asInstanceOf[MovePath].eP.x,x.svgPath.pOps(0).args(0).asInstanceOf[MovePath].eP.y),
      Seq.empty[Boolean]
    ).forall(x=>x)&&
      (x.svgPath.bb match{
        case Some(bb) => (bb.y2-bb.y1>AXESRATIOTHRESHOLD*H && bb.x2-bb.x1>AXESRATIOTHRESHOLD*W)
        case None => true
      })

  }

  def main(args: Array[String]):Unit= {
    //val loc = "data/10.1.1.108.9317-Figure-4.svg"
    //val loc = "data/10.1.1.105.5053-Figure-6.svg"
    //val loc="src/test/resources/hassan-Figure-2.svg"
    //val loc="data/10.1.1.105.5053-Figure-2.svg"
    val loc="data/10.1.1.112.9247-Figure-4.svg"
    MarkerDetection(loc)
  }
}
