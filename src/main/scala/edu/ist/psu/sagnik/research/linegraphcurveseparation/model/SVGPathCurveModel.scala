package edu.ist.psu.sagnik.research.linegraphcurveseparation.model

import scala.xml.NodeSeq


/**
 * Created by sagnik on 3/4/16.
 */

// see https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Fills_and_Strokes
case class PathStyle(
                     fill:String,
                     stroke:String,
                     strokeWidth:String,
                     strokeLinecap:String,
                     strokeLinejoin:String,
                     strokeMiterlimit:String,
                     strokeDasharray:String, //TODO: this should actually be a Float sequence, but we can handle that later.
                     strokeOpacity:String,
                     fillOpacity:String
                      )


case class SVGPathXML(svgPath:SVGPath,styleXML:NodeSeq)
//TODO: We need to add a sequence of "(x,y) points" that is painted by this curve.
case class SVGPathCurve(svgPath:SVGPath,pathStyle:PathStyle)

case class SVGCurve(id:String,paths:Seq[SVGPathCurve])