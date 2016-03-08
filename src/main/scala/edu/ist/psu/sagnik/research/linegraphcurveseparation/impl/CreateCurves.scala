package edu.ist.psu.sagnik.research.linegraphcurveseparation.impl

import java.io.File

import edu.ist.psu.sagnik.research.linegraphcurveseparation.model.{SVGCurve, SVGPathCurve}
import edu.ist.psu.sagnik.research.linegraphcurveseparation.pathparser.impl.SVGPathfromDString
import edu.ist.psu.sagnik.research.linegraphcurveseparation.pathparser.model._
import edu.ist.psu.sagnik.research.linegraphcurveseparation.writer.SVGWriter
import org.apache.commons.io.FileUtils

import scala.language.postfixOps
/**
 * Created by sagnik on 3/5/16.
 */
object CreateCurves {
  //Remember an SVG path can have multiple subpaths. Usually a path contains multiple path commands. Each path command
  // (lineto, moveto) can paint something. we say a path is horizontal or vertical if all subpaths in this path paints
  // a horizontal or vertical line.
  /*
  TODO:
  A better implementation will return a sequence of subpaths (PathCommand) that are not horizontal or vertical. But then
  we will need code to create a DSTring given a sequence of path commands.
  */
  val SMALLPARAM=5f
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
          //println(s"pathelems: ${pathElems}")
          if (!pathElem.isAbsolute)
            pathHVArr++pathElem.args.map(x=>x.asInstanceOf[LinePath].eP.productIterator.toList.contains(0f))
          else {
            if (pathElem.args.isEmpty)
              pathHVArr
            else if (pathElem.args.length==1){
              val tpBB=pathElem.getBoundingBox[Line](lep,pathElem.isAbsolute,pathElem.args)
              pathHVArr:+((tpBB.x1==tpBB.x2)||(tpBB.y1==tpBB.y2))
            }
            else {
              //println("here")
              //println(s"new array: ${pathElem.args.map(x => Line(isAbsolute = true, args = Seq(LinePath(x.asInstanceOf[LinePath].eP))))}")
              pathIsHV(
                pathElem.args.map(x => Line(isAbsolute = true, args = Seq(LinePath(x.asInstanceOf[LinePath].eP)))),
                lep,
                pathHVArr
              )
            }
          }
        }
        else
          pathHVArr
      case pathElem :: rest => {
        val lastEndPoint = pathElem.getEndPoint[Line](lep,pathElem.isAbsolute,pathElem.args)
        //println(s"pathElem: ${pathElem}, rest ${rest}, lastEndPoint: ${lastEndPoint}, prevEndPoint:${lep}")
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
              val tpBB=pathElem.getBoundingBox[Line](lep,pathElem.isAbsolute,pathElem.args)
              pathIsHV(
                rest,
                lastEndPoint,
                pathHVArr:+((tpBB.x1==tpBB.x2)||(tpBB.y1==tpBB.y2))
              )
            }
            else {
              val splitPaths=pathElem.args.map(x => Line(isAbsolute = true, args = Seq(LinePath(x.asInstanceOf[LinePath].eP))))
              pathIsHV(
                splitPaths++rest,
                lep,
                pathHVArr
              )
            }

          }
        }
        else {
          pathIsHV(rest, lastEndPoint, pathHVArr)
        }
      }
    }

  //we say a path has small subpaths if each subpath in this path paints a very small part of a curve. The idea is to remove
  // markers. We are already removing some lines in such marekers in the method pathISHV.
  //TODO: This will probably limit the precision of the process. Are curves separable even if we remove marker elements?
  //TODO: Also, for now we will only remove subpaths painted by "Line" command. In future, other commands will be handled.

  def pathIsSmall(pathElems:Seq[PathCommand],lep:CordPair,pathSArr:Seq[Boolean],scalingFactor:Float):Seq[Boolean]=
    pathElems match {
      case Nil => pathSArr
      case pathElem :: Nil =>
        if (pathElem.isInstanceOf[Line]) {
          if (pathElem.args.isEmpty)
            pathSArr
          else if (pathElem.args.length==1){
            val tpBB=pathElem.getBoundingBox[Line](lep,pathElem.isAbsolute,pathElem.args)
            //println(s"1 ${tpBB}")
            pathSArr:+((tpBB.x2-tpBB.x1)<(SMALLPARAM/scalingFactor)||(tpBB.y2-tpBB.y1)<(SMALLPARAM/scalingFactor))
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

  def pathIsSmall(pathElems:Seq[PathCommand],lep:CordPair,pathSArr:Seq[Boolean]):Seq[Boolean]=pathIsSmall(pathElems,lep,pathSArr,1)

  def createCurves(svgPaths:Seq[SVGPathCurve]):Seq[SVGCurve]= Seq.empty[SVGCurve]

  def createCurveSVGFiles(loc:String)={
    val svgpathCurves= SVGPathExtract(loc)
    val ppcp=svgpathCurves.filterNot(
      x=>
        pathIsHV(
          x.svgPath.pOps.slice(1,x.svgPath.pOps.length),
          CordPair(x.svgPath.pOps(0).args(0).asInstanceOf[MovePath].eP.x,x.svgPath.pOps(0).args(0).asInstanceOf[MovePath].eP.y),
          Seq.empty[Boolean]
        ).forall(x=>x)
    )
    val possibleCurvepaths=ppcp
      .filterNot(
        x=>pathIsSmall(
          x.svgPath.pOps.slice(1,x.svgPath.pOps.length),
          CordPair(x.svgPath.pOps(0).args(0).asInstanceOf[MovePath].eP.x,x.svgPath.pOps(0).args(0).asInstanceOf[MovePath].eP.y),
          Seq.empty[Boolean],
          x.svgPath.transformOps(0).matrix(0,0)
        ).forall(x=>x)
      )

    val finalCurvePaths= if (possibleCurvepaths.isEmpty) ppcp else possibleCurvepaths

    val curveGroups=finalCurvePaths.groupBy(x=> x.pathStyle).toSeq.zipWithIndex.
      map{case (d,index)=>SVGCurve(index.toString,d._2)}

    val curveDir = new File(loc.substring(0,loc.length-4));
    val dirResult=if (!curveDir.exists) curveDir.mkdir else {FileUtils.deleteDirectory(curveDir); curveDir.mkdir}

    if (dirResult) {
      curveGroups foreach { x => println(s"Creating SVG for curve ${x.id}"); SVGWriter(x.paths, x.id, loc, curveDir.getAbsolutePath) }
    }
    else{
      println("Couldn't create directory to store Curve SVG files, exiting.")
    }
  }

  def main(args: Array[String]):Unit= {
    //val loc = "data/10.1.1.108.9317-Figure-4.svg"
    val loc="data/10.1.1.105.5053-Figure-6.svg"
    //val loc="src/test/resources/hassan-Figure-2.svg"

    //This is only for testing

        //val dString="m 3334.88,6013.25 1747.95,0 0,1612.45 -1747.95,0 0,-1612.45 z"
        //val dString="m 3439.89,6980.57 17.31,-403.11 17.69,-134.37 17.32,-67 17.31,-40.27 17.69,-27.11 17.31,-19.19 17.69,-14.31 17.32,-11.28 17.69,-8.66 17.31,-7.53 17.31,-6.02 17.7,-5.27 17.31,-4.52 17.69,-3.76 17.32,-3.39 17.68,-3.01 17.32,-2.64 17.69,-2.25 17.31,-1.88 17.32,-2.27 17.69,-1.5 17.31,-1.5 17.69,-1.51 17.31,-1.51 17.7,-1.12 17.31,-1.13 17.69,-1.13 17.31,-1.13 17.32,-0.75 17.69,-1.13 17.31,-0.76 17.69,-0.75 17.32,-0.75 17.69,-0.38 17.31,-0.75 17.31,-0.75 17.69,-0.38 17.32,-0.75 17.69,-0.38 17.31,-0.38 17.69,-0.75 17.31,-0.38 17.7,-0.37 17.31,-0.38 17.31,-0.37 17.7,-0.38 17.31,-0.38 17.69,-0.37 17.31,-0.38 17.69,-0.38 17.32,0 17.69,-0.37 17.31,-0.38 17.32,-0.38 17.68,0 17.32,-0.37 17.69,-0.38 17.31,0 17.69,-0.38 17.32,0 17.31,-0.37 17.69,-0.38 17.31,0 17.7,-0.37 17.31,0 17.69,-0.38 17.31,0 17.69,-0.38 34.63,0 17.69,-0.37 17.32,0 17.68,-0.38 35.01,0 17.31,-0.38 35.01,0 17.31,-0.37 35,0 17.7,-0.38 35,0 17.31,-0.38 35.01,0 17.31,-0.37 52.7,0 17.31,-0.38 35,0"
        //val dString="M 4377.29,6527.96 4401,6504.25"
        //val dString="M 5,5 L 5,15 L 15,15 L 15,5 z"
        //val dString="M 5,5 L 5,15 L 10,10 L 15,5 z"
        //val dString="M 5,5 5,15 15,15 15,5 z"
        //val dString="m 931.537,3069.41 23.625,-23.62"
        val dString="m 2102.1,4133.7 0,23.62 m 0,1267.88 0,-23.63"
        val pathCommands=SVGPathfromDString.getPathCommands(dString)

        pathCommands.foreach(x=>println(x))
        if (pathCommands.nonEmpty) {
          println(
            pathIsHV(
              pathCommands.slice(1,pathCommands.length),
              CordPair(pathCommands(0).args(0).asInstanceOf[MovePath].eP.x,pathCommands(0).args(0).asInstanceOf[MovePath].eP.y),
              Seq.empty[Boolean]
            )
          )
        }
    createCurveSVGFiles(loc)
  }

}
