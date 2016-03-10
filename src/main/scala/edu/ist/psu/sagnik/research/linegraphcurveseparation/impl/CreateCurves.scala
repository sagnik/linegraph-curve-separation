package edu.ist.psu.sagnik.research.linegraphcurveseparation.impl

import java.io.File

import edu.ist.psu.sagnik.research.linegraphcurveseparation.model.{SVGCurve, SVGPathCurve}
import edu.ist.psu.sagnik.research.linegraphcurveseparation.pathparser.impl.SVGPathfromDString
import edu.ist.psu.sagnik.research.linegraphcurveseparation.pathparser.model._
import edu.ist.psu.sagnik.research.linegraphcurveseparation.reader.XMLReader
import edu.ist.psu.sagnik.research.linegraphcurveseparation.writer.SVGWriter
import org.apache.commons.io.FileUtils

import scala.language.postfixOps
/**
 * Created by sagnik on 3/5/16.
 */
object CreateCurves {

  def colorBasedSegmentation(curvePaths:Seq[SVGPathCurve]):Seq[SVGCurve]=
    curvePaths.groupBy(x=> x.pathStyle.stroke.getOrElse("none")).toSeq.zipWithIndex.map{case (d,index)=>SVGCurve(index.toString,d._2)}

  def apply(loc:String,createImages:Boolean)={
    val svgPaths=
      if (loc.contains("-sps")) //this SVG has already paths split
        SVGPathExtract(loc, true)
      else
        SVGPathExtract(loc,false).map(
          c=>
            SplitPaths.splitPath(
              c.svgPath.pOps.slice(1,c.svgPath.pOps.length),
              c,
              CordPair(c.svgPath.pOps(0).args(0).asInstanceOf[MovePath].eP.x,c.svgPath.pOps(0).args(0).asInstanceOf[MovePath].eP.y),
              Seq.empty[SVGPathCurve]
            )
        ).flatten

    //TODO: possible exceptions
    val height = ((XMLReader(loc) \\ "svg")(0) \@ "height").toFloat
    val width = ((XMLReader(loc) \\ "svg")(0) \@ "width").toFloat
    val (axes,tics,curvePaths)=SeparateAxesGridTickPaths(svgPaths,width,height)

    val curveGroups=colorBasedSegmentation(curvePaths)

    if (createImages) {
      val curveDir = new File(loc.substring(0, loc.length - 4));
      val dirResult = if (!curveDir.exists) curveDir.mkdir
      else {
        FileUtils.deleteDirectory(curveDir); curveDir.mkdir
      }

      if (dirResult) {
        curveGroups foreach { x => println(s"Creating SVG for curve ${x.id}"); SVGWriter(x.paths, x.id, loc, curveDir.getAbsolutePath) }
      }
      else {
        println("Couldn't create directory to store Curve SVG files, exiting.")
      }
      curveGroups
    }
    else
      curveGroups
  }

  def main(args: Array[String]):Unit= {
    val loc="src/test/resources/hassan-Figure-2.svg"
    CreateCurves(loc,true)

  }

}
