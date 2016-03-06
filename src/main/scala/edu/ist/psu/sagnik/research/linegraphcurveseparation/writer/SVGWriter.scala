package edu.ist.psu.sagnik.research.linegraphcurveseparation.writer

import edu.ist.psu.sagnik.research.linegraphcurveseparation.model.SVGPathCurve
import edu.ist.psu.sagnik.research.linegraphcurveseparation.reader.XMLReader

import scala.reflect.io.File

/**
 * Created by sagnik on 3/6/16.
 */
object SVGWriter {
  def apply(curvePaths:Seq[SVGPathCurve],curveNo:String,orgSVGLoc:String,curveDir:String):Unit= {
    val curveSVGLoc = curveDir + "/" + orgSVGLoc.substring(0, orgSVGLoc.length - 4).split("/").last + "-Curve-" + curveNo + ".svg"

    //TODO: Possible exception
    val height = (XMLReader(orgSVGLoc) \\ "svg")(0) \@ "height"
    val width = (XMLReader(orgSVGLoc) \\ "svg")(0) \@ "width"

    val svgStart = "<?xml version=\"1.0\" standalone=\"no\"?>\n\n<svg height=\"" +
      height +
      "\" width=\"" +
      width +
      "\" version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\">" +
      "\n"

    val svgString = curvePaths.map(x => x.svgPath.pContent).foldLeft("")((a, b) => a + "\n" + b)

    val svgEnd = "\n</svg>"

    File(curveSVGLoc).writeAll(svgStart + svgString + svgEnd)
  }

}
