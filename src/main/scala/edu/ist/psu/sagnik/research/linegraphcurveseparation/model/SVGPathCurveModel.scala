package edu.ist.psu.sagnik.research.linegraphcurveseparation.model

import scala.xml.NodeSeq


/**
 * Created by sagnik on 3/4/16.
 */

//TODO: we are using string for all style elements, while they can be other data structures such as array.
// See https://www.w3.org/TR/SVG/painting.html#FillProperties and https://www.w3.org/TR/SVG/painting.html#StrokeProperties
case class PathStyle(
                     fill:Option[String],
                     fillRule:Option[String],
                     fillOpacity:Option[String],
                     stroke:Option[String],
                     strokeWidth:Option[String],
                     strokeLinecap:Option[String],
                     strokeLinejoin:Option[String],
                     strokeMiterlimit:Option[String],
                     strokeDasharray:Option[String],
                     strokeDashoffset:Option[String],
                     strokeOpacity:Option[String]
                      )


case class SVGPathXML(svgPath:SVGPath,styleXML:NodeSeq)
//TODO: We need to add a sequence of "(x,y) points" that is painted by this curve.
case class SVGPathCurve(svgPath:SVGPath,pathStyle:PathStyle)

case class SVGCurve(id:String,paths:Seq[SVGPathCurve])