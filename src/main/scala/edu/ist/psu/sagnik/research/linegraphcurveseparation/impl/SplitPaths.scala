package edu.ist.psu.sagnik.research.linegraphcurveseparation.impl

import edu.ist.psu.sagnik.research.linegraphcurveseparation.model.{PathStyle, SVGPath, SVGPathCurve}
import edu.ist.psu.sagnik.research.linegraphcurveseparation.pathparser.impl.SVGPathfromDString
import edu.ist.psu.sagnik.research.linegraphcurveseparation.pathparser.model._
import edu.ist.psu.sagnik.research.linegraphcurveseparation.transformparser.model.TransformCommand
import edu.ist.psu.sagnik.research.linegraphcurveseparation.writer.SVGWriter

/**
 * Created by sagnik on 3/8/16.
 */

//we will take a SVG path and produce a sequence of Paths, each with exactly one "absolute" PathCommand
// and a "single" argument.
//For example, "m 2102.1,4133.7 0,23.62 m 0,1267.88 0,-23.63" now produces
// Path(Move(absolute,MovePath), Line(relative,Seq[LinePath]), Line(absolute,LinePath)).
//Instead, we wanto to produce Path(Move(absolute,MovePath)), Path(Line(absolute,LinePath))...Path(Line(absolute,LinePath))

object SplitPaths {

  def createSVGCurvePath(path: SVGPathCurve, mC: Move, lC: Line): SVGPathCurve =
    SVGPathCurve(
      svgPath = SVGPathBB(
        path.svgPath.copy(
          pdContent = "M " +
            mC.args(0).eP.x +
            "," +
            mC.args(0).eP.y +
            " L " +
            lC.args(0).eP.x +
            "," +
            lC.args(0).eP.y,
          pContent = createSVGPathString(path.pathStyle, path.svgPath.transformOps, mC, lC, path.svgPath.id),
          pOps = Seq(mC, lC)
        )
      ),
      pathStyle = path.pathStyle
    )

  def createSVGPathString(s: PathStyle, tOps: Seq[TransformCommand], mC: Move, lC: Line, id: String): String = {

    val styleString = "fill:" +
      s.fill +
      ";stroke:" +
      s.stroke +
      ";stroke-width:" +
      s.strokeWidth +
      ";stroke-linecap:" +
      s.strokeLinecap +
      ";stroke-linejoin:" +
      s.strokeLinejoin +
      ";stroke-miterlimit:" +
      s.strokeMiterlimit +
      ";stroke-opacity:" +
      s.strokeOpacity +
      ";stroke-dasharray:" +
      s.strokeDasharray

    val transformString = "matrix(1.0,0.0,0.0,0.0,1.0,0.0)" //this is equivalent to no transformation, this will be corrected when
    //we map this with SVGPathBB

    val dString = "M " +
      mC.args(0).eP.x +
      "," +
      mC.args(0).eP.y +
      " L " +
      lC.args(0).eP.x +
      "," +
      lC.args(0).eP.y

    "<path d=\"" +
      dString +
      "\" id=\"" +
      id +
      "\" style=\"" +
      styleString +
      "\" transform=\"" +
      transformString +
      "\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:svg=\"http://www.w3.org/2000/svg\"/>"

  }

  //tail-fucking-recursion. Take that, Python. ;)
  def splitPath(pathElems: Seq[PathCommand], path: SVGPathCurve, lep: CordPair, pathSArr: Seq[SVGPathCurve]): Seq[SVGPathCurve] =
    pathElems match {
      case Nil => pathSArr
      case pathElem :: Nil =>
        if (pathElem.isInstanceOf[Line]) {
          if (pathElem.args.isEmpty)
            pathSArr
          else if (pathElem.args.length == 1) {
            val lastEp = pathElem.getEndPoint[Line](lep,pathElem.isAbsolute,pathElem.args)
            val moveCommand = Move(isAbsolute = true, args = Seq(MovePath(lep)))
            val lineCommand = Line(isAbsolute = true, args = Seq(LinePath(lastEp)))
            val newSvgCurvePath = createSVGCurvePath(path, moveCommand, lineCommand)
            pathSArr :+ newSvgCurvePath
          }
          else {
            splitPath(
              pathElem.args.map(x => Line(isAbsolute = pathElem.isAbsolute, args = Seq(LinePath(x.asInstanceOf[LinePath].eP)))),
              path,
              lep,
              pathSArr
            )
          }
        }
        else {
          val lastEp = pathElem.getEndPoint[pathElem.type](lep,pathElem.isAbsolute,pathElem.args)
          splitPath(pathElems, path, lastEp, pathSArr)
        }
      case pathElem :: rest => {
        val lastEndPoint = pathElem.getEndPoint[pathElem.type](lep,pathElem.isAbsolute,pathElem.args)
        if (pathElem.isInstanceOf[Line]) {
          if (pathElem.args.isEmpty)
            pathSArr
          else if (pathElem.args.length == 1) {
            val lastEp = pathElem.getEndPoint[Line](lep,pathElem.isAbsolute,pathElem.args)
            val moveCommand = Move(isAbsolute = true, args = Seq(MovePath(lep)))
            val lineCommand = Line(isAbsolute = true, args = Seq(LinePath(lastEp)))
            val newSvgCurvePath = createSVGCurvePath(path, moveCommand, lineCommand)
            splitPath(
              rest,
              path,
              lastEndPoint,
              pathSArr :+ newSvgCurvePath
            )
          }
          else {
            val splitPaths = pathElem.args.map(x => Line(isAbsolute = pathElem.isAbsolute, args = Seq(LinePath(x.asInstanceOf[LinePath].eP))))
            splitPath(
              splitPaths ++ rest,
              path,
              lep,
              pathSArr
            )
          }
        }
        else
          splitPath(rest, path, lastEndPoint, pathSArr)

      }
    }
}

object TestSplitPaths{
  def main(args: Array[String]):Unit= {
    val loc="src/test/resources/hassan-Figure-2.svg"
    val cS= SVGPathExtract(loc)
    //Seq(svgpathCurves(0)).foreach(x=>println(x.svgPath.id,x.svgPath.pdContent,x.svgPath.pOps))

    val spPath=cS.map(
      c=>
        SplitPaths.splitPath(
          c.svgPath.pOps.slice(1,c.svgPath.pOps.length),
          c,
          CordPair(c.svgPath.pOps(0).args(0).asInstanceOf[MovePath].eP.x,c.svgPath.pOps(0).args(0).asInstanceOf[MovePath].eP.y),
          Seq.empty[SVGPathCurve]
        )
    ).flatten

    SVGWriter(spPath,loc,"sps")

  }

}
