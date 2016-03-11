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

  def featureBasedSegmentation(curvePaths:Seq[SVGPathCurve]):Seq[SVGCurve]=
    curvePaths.groupBy(x=> x.pathStyle).toSeq.zipWithIndex.map{case (d,index)=>SVGCurve(index.toString,d._2)}

  def apply(loc:String,createImages:Boolean,segementationFunction: (Seq[SVGPathCurve])=>Seq[SVGCurve])={
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
    val (axes,tics,curvePaths)=SeparateAxesGridTickPaths(noFill,width,height)

    val curveGroups=segementationFunction(curvePaths)

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

  def apply(loc:String,curvePaths: Seq[SVGPathCurve],createImages:Boolean,segementationFunction: (Seq[SVGPathCurve])=>Seq[SVGCurve])={
    val curveGroups=segementationFunction(curvePaths)
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
    //val loc="src/test/resources/hassan-Figure-2.svg"
    //val loc="data/10.1.1.120.143-Figure-12.svg"
    //val loc="data/10.1.1.77.6281-Figure-8.svg"
    //val loc="data/10.1.1.172.4429-Figure-7.svg"
    //val loc="data/10.1.1.104.5833-Figure-7.svg"
    //val loc="data/10.1.1.106.6209-Figure-10.svg"

    val loc="data/10.1.1.100.3286-Figure-9.svg"
    CreateCurves(loc,true,featureBasedSegmentation)


  }

}
