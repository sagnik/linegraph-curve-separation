package edu.ist.psu.sagnik.research.linegraphcurveseparation.impl

import edu.ist.psu.sagnik.research.linegraphcurveseparation.model.{SVGPath, SVGPathCurve}
import edu.ist.psu.sagnik.research.linegraphcurveseparation.pathparser.model._

/**
 * Created by sagnik on 3/8/16.
 */

//we will take a SVG path and produce a sequence of Paths, each with exactly one "absolute" PathCommand
// and a "single" argument.
//For example, "m 2102.1,4133.7 0,23.62 m 0,1267.88 0,-23.63" now produces
// Path(Move(absolute,MovePath), Line(relative,Seq[LinePath]), Line(absolute,LinePath)).
//Instead, we wanto to produce Path(Move(absolute,MovePath)), Path(Line(absolute,LinePath))...Path(Line(absolute,LinePath))
object SplitPaths {

  def createSVGCurvePath(path:SVGPathCurve,mC:Move,lC:Line):SVGPathCurve=
    SVGPathCurve(
      svgPath=path.svgPath.copy(
        pdContent="M " +
          mC.args(0).eP.x +
          "," +
          mC.args(0).eP.y +
          " L " +
          lC.args(0).eP.x +
          "," +
          lC.args(0).eP.y,
        pContent=createSVGPathString(path.pathStyle,path.svgPath.transformOps,mC,lC),
        pOps=Seq(mC,lC)
    ),
    pathStyle = path.pathStyle
    )

  def splitPath(pathElems:Seq[PathCommand],path:SVGPathCurve,lep:CordPair,pathSArr:Seq[SVGPathCurve]):Seq[SVGPathCurve]=
    pathElems match {
      case Nil => pathSArr
      case pathElem :: Nil =>
        if (pathElem.isInstanceOf[Line]) {
          if (pathElem.args.isEmpty)
            pathSArr
          else if (pathElem.args.length==1){
            val lastEp=pathElem.getEndPoint[Line](lep,pathElem.isAbsolute,pathElem.args)
            val moveCommand=Move(isAbsolute = true,args=Seq(MovePath(lep)))
            val lineCommand=Line(isAbsolute = true,args=Seq(LinePath(lastEp)))
            val newSvgCurvePath=createSVGCurvePath(path,moveCommand,lineCommand)
            //println(s"1 ${tpBB}")
            pathSArr:+newSvgCurvePath
          }
          else {
            pathIsSmall(
              pathElem.args.map(x => Line(isAbsolute = pathElem.isAbsolute, args = Seq(LinePath(x.asInstanceOf[LinePath].eP)))),
              lep,
              pathSArr,
              scalingFactor
            )
          }
        }
        else
          pathSArr
      case pathElem :: rest => {
        val lastEndPoint = pathElem.getEndPoint[Line](lep,pathElem.isAbsolute,pathElem.args)
        if (pathElem.isInstanceOf[Line]) {
          if (pathElem.args.isEmpty)
            pathSArr
          else if (pathElem.args.length==1){
            val tpBB=pathElem.getBoundingBox[Line](lep,pathElem.isAbsolute,pathElem.args)
            //println(s"2 ${tpBB}, pathElem: ${pathElem}, lep: ${lep}")
            pathIsSmall(
              rest,
              lastEndPoint,
              pathSArr:+((tpBB.x2-tpBB.x1)<(SMALLPARAM/scalingFactor)||(tpBB.y2-tpBB.y1)<(SMALLPARAM/scalingFactor)),
              scalingFactor
            )
          }
          else {
            val splitPaths=pathElem.args.map(x => Line(isAbsolute = pathElem.isAbsolute, args = Seq(LinePath(x.asInstanceOf[LinePath].eP))))
            pathIsSmall(
              splitPaths++rest,
              lep,
              pathSArr,
              scalingFactor
            )
          }
        }
        else
          pathIsSmall(rest, lastEndPoint, pathSArr, scalingFactor)

      }
    }

  def apply(path:SVGPathCurve):Seq[SVGPathCurve]={

  }

}
