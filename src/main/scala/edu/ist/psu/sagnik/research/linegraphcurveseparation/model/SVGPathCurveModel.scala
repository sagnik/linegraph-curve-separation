package edu.ist.psu.sagnik.research.linegraphcurveseparation.model

import edu.ist.psu.sagnik.research.inkscapesvgprocessing.model.Rectangle

/**
 * Created by sagnik on 3/4/16.
 */

// see https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Fills_and_Strokes
case class PathStyle(fill:String,
                     stroke:String,
                     strokeWidth:Float,
                     strokeLinecap:String,
                     strokeLinejoin:String,
                     strokeMiterlimit:String,
                     strokeDasharray:String, //TODO: this should actually be a Float sequence, but we can handle that later.
                     strokeOpacity:Float,
                     fillOpacity:Float)

case class SVGPathCurve(bb:Rectangle,pathDString:String,pathStyle:PathStyle,pathWholeString:String)