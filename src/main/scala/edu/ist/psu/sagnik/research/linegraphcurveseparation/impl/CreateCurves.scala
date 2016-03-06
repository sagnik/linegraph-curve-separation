package edu.ist.psu.sagnik.research.linegraphcurveseparation.impl

import java.io.File

import edu.ist.psu.sagnik.research.linegraphcurveseparation.model.{SVGPathCurve, SVGCurve}
import edu.ist.psu.sagnik.research.linegraphcurveseparation.pathparser.impl.SVGPathfromDString
import edu.ist.psu.sagnik.research.linegraphcurveseparation.pathparser.model._
import edu.ist.psu.sagnik.research.linegraphcurveseparation.reader.XMLReader
import edu.ist.psu.sagnik.research.linegraphcurveseparation.writer.SVGWriter

import scala.language.postfixOps
/**
 * Created by sagnik on 3/5/16.
 */
object CreateCurves {
  //Remember an SVG p[ath can have multiple subpaths. Usually a path contains multiple path commands. Each path command
  // (lineto, moveto) can paint something. we say a path is horizontal or vertical if all subpaths in this path paints
  // a horizontal or vertical line.
  /*
  TODO:
  A better implementation will return a sequence of subpaths (PathCommand) that are not horizontal or vertical. But then
  we will need code to create a DSTring given a sequence of path commands.
  */
  def pathIsHV(pathElems:Seq[PathCommand],lep:CordPair,pathHVArr:Seq[Boolean]):Seq[Boolean]=
    pathElems match {
      case Nil => pathHVArr
      case pathElem :: Nil =>
        if (pathElem.isInstanceOf[HL])
          pathHVArr:+true
        else if (pathElem.isInstanceOf[VL])
          pathHVArr:+true
        else if (pathElem.isInstanceOf[Line]) {
          //there can be two cases, if the command is absolute, we will convert create a line command
          // for each of the linepaths. if the command is relative, we only need to check if
          // each of the linepaths contain a 0.
          if (!pathElem.isAbsolute)
            pathHVArr++pathElem.args.map(x=>x.asInstanceOf[LinePath].eP.productIterator.toList.contains(0f))
          else {
            if (pathElem.args.isEmpty)
              pathHVArr
            else if (pathElem.args.length==1){
              val tpBB=pathElem.getBoundingBox[pathElem.type](lep,pathElem.isAbsolute,pathElem.args)
              pathHVArr:+((tpBB.x1==tpBB.x2)||(tpBB.y1==tpBB.y2))
            }
            else
              pathIsHV(
                pathElem.args.map(x => Line(isAbsolute = true, args = Seq(LinePath(x.asInstanceOf[LinePath].eP)))),
                lep,
                pathHVArr
              )
          }
        }
        else
          pathHVArr
      case pathElem :: rest => {
        val lastEndPoint = pathElem.getEndPoint[pathElem.type](lep,pathElem.isAbsolute,pathElem.args)
        if (pathElem.isInstanceOf[HL])
          pathIsHV(rest,lastEndPoint,pathHVArr:+true)
        else if (pathElem.isInstanceOf[VL])
          pathIsHV(rest,lastEndPoint,pathHVArr:+true)
        else if (pathElem.isInstanceOf[Line]) {
          //see before
          if (!pathElem.isAbsolute)
            pathIsHV(rest,lastEndPoint,pathHVArr++pathElem.args.map(x=>x.asInstanceOf[LinePath].eP.productIterator.toList.contains(0f)))
          else {
            if (pathElem.args.isEmpty)
              pathHVArr
            else if (pathElem.args.length==1){
              val tpBB=pathElem.getBoundingBox[pathElem.type](lastEndPoint,pathElem.isAbsolute,pathElem.args)
              pathHVArr:+((tpBB.x1==tpBB.x2)||(tpBB.y1==tpBB.y2))
            }
            pathIsHV(
              rest ++ pathElem.args.map(x => Line(isAbsolute = true, args = Seq(LinePath(x.asInstanceOf[LinePath].eP)))),
              lastEndPoint,
              pathHVArr
            )
          }
        }
        else {
          pathIsHV(rest, lastEndPoint, pathHVArr)
        }
      }
    }

  def createCurves(svgPaths:Seq[SVGPathCurve]):Seq[SVGCurve]= Seq.empty[SVGCurve]

  def main(args: Array[String]):Unit= {
    val loc = "src/test/resources/hassan-Figure-2.svg"
    //This is only for testing

//    val dString="m 3334.88,6013.25 1747.95,0 0,1612.45 -1747.95,0 0,-1612.45 z"
//    val dString="m 3439.89,6980.57 17.31,-403.11 17.69,-134.37 17.32,-67 17.31,-40.27 17.69,-27.11 17.31,-19.19 17.69,-14.31 17.32,-11.28 17.69,-8.66 17.31,-7.53 17.31,-6.02 17.7,-5.27 17.31,-4.52 17.69,-3.76 17.32,-3.39 17.68,-3.01 17.32,-2.64 17.69,-2.25 17.31,-1.88 17.32,-2.27 17.69,-1.5 17.31,-1.5 17.69,-1.51 17.31,-1.51 17.7,-1.12 17.31,-1.13 17.69,-1.13 17.31,-1.13 17.32,-0.75 17.69,-1.13 17.31,-0.76 17.69,-0.75 17.32,-0.75 17.69,-0.38 17.31,-0.75 17.31,-0.75 17.69,-0.38 17.32,-0.75 17.69,-0.38 17.31,-0.38 17.69,-0.75 17.31,-0.38 17.7,-0.37 17.31,-0.38 17.31,-0.37 17.7,-0.38 17.31,-0.38 17.69,-0.37 17.31,-0.38 17.69,-0.38 17.32,0 17.69,-0.37 17.31,-0.38 17.32,-0.38 17.68,0 17.32,-0.37 17.69,-0.38 17.31,0 17.69,-0.38 17.32,0 17.31,-0.37 17.69,-0.38 17.31,0 17.7,-0.37 17.31,0 17.69,-0.38 17.31,0 17.69,-0.38 34.63,0 17.69,-0.37 17.32,0 17.68,-0.38 35.01,0 17.31,-0.38 35.01,0 17.31,-0.37 35,0 17.7,-0.38 35,0 17.31,-0.38 35.01,0 17.31,-0.37 52.7,0 17.31,-0.38 35,0"
//    val dString="M 4377.29,6527.96 4401,6504.25"
//    val pathCommands=SVGPathfromDString.getPathCommands(dString)
//
//    pathCommands.foreach(x=>println(x))
//    if (pathCommands.nonEmpty) {
//      println(
//        pathIsHV(
//          pathCommands.slice(1,pathCommands.length),
//          CordPair(pathCommands(0).args(0).asInstanceOf[MovePath].eP.x,pathCommands(0).args(0).asInstanceOf[MovePath].eP.y),
//          Seq.empty[Boolean]
//        )
//      )
//    }
//        val svgpathCurves= SVGPathExtract(loc)
//        svgpathCurves.foreach(x=>println(
//          x.svgPath.id,
//          pathIsHV(
//            x.svgPath.pOps.slice(1,x.svgPath.pOps.length),
//            CordPair(x.svgPath.pOps(0).args(0).asInstanceOf[MovePath].eP.x,x.svgPath.pOps(0).args(0).asInstanceOf[MovePath].eP.y),
//            Seq.empty[Boolean]
//          )
//        ))
        val svgpathCurves= SVGPathExtract(loc)
        val possibleCurvepaths=svgpathCurves.filterNot(
          x=>pathIsHV(
            x.svgPath.pOps.slice(1,x.svgPath.pOps.length),
            CordPair(x.svgPath.pOps(0).args(0).asInstanceOf[MovePath].eP.x,x.svgPath.pOps(0).args(0).asInstanceOf[MovePath].eP.y),
            Seq.empty[Boolean]
          ).forall(x=>x)
        )
       val curveGroups=possibleCurvepaths.groupBy(x=> x.pathStyle).toSeq.zipWithIndex.
         map{case (d,index)=>SVGCurve(index.toString,d._2)}

    val curveDir = new File(loc.substring(0,loc.length-4));
    val dirResult=if (!curveDir.exists) curveDir.mkdir else true

    if (dirResult) {
      curveGroups foreach { x => println(s"Creating SVG for curve ${x.id}"); SVGWriter(x.paths, x.id, loc, curveDir.getAbsolutePath) }
    }
    else{
      println("Couldn't create directory to store Curve SVG files, exiting.")
    }

  }

}
