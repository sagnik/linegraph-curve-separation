package edu.ist.psu.sagnik.research.linegraphcurveseparation.impl

import edu.ist.psu.sagnik.research.linegraphcurveseparation.pathparser.impl.SVGPathfromDString
import edu.ist.psu.sagnik.research.linegraphcurveseparation.pathparser.model.{MovePath, CordPair}
import edu.ist.psu.sagnik.research.linegraphcurveseparation.writer.SVGWriter

/**
 * Created by sagnik on 3/8/16.
 */
object MarkerDetection {

  val TMTHRESHOLD=10f
  def apply(loc:String):Unit={
    val svgpathCurves= SVGPathExtract(loc)
//Print statements to test
/*
    svgpathCurves.foreach(x=>
      println(x.svgPath.id, x.svgPath.pOps.length,x.svgPath.pOps.map(x=>x.args.length),
        x.svgPath.bb match{
          case Some(bb)=>
            List(
              bb.x1,///x.svgPath.transformOps(0).matrix(0,0),
              bb.y1,///x.svgPath.transformOps(0).matrix(0,0),
              bb.x2,///x.svgPath.transformOps(0).matrix(0,0),
              bb.y2///x.svgPath.transformOps(0).matrix(0,0)
            )
          case None => "None"
        }
      )
    )
*/

    val possibleTicsMarkers = svgpathCurves.filter(x=>
      x.svgPath.bb match{
        case Some(bb) => (
          bb.x2-bb.x1< (TMTHRESHOLD) &&
            bb.y2-bb.y1<(TMTHRESHOLD)
          )
        case _ => false
      }
    )

    SVGWriter(possibleTicsMarkers,loc)

  }



  def main(args: Array[String]):Unit= {
    //val loc = "data/10.1.1.108.9317-Figure-4.svg"
    //val loc = "data/10.1.1.105.5053-Figure-6.svg"
    //val loc="src/test/resources/hassan-Figure-2.svg"
    val loc="data/10.1.1.105.5053-Figure-2.svg"
    MarkerDetection(loc)
  }
}
