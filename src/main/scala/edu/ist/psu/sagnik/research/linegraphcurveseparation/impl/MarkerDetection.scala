package edu.ist.psu.sagnik.research.linegraphcurveseparation.impl

import java.io.File

import edu.ist.psu.sagnik.research.linegraphcurveseparation.model.{SVGCurve, Rectangle, SVGPathCurve}
import edu.ist.psu.sagnik.research.linegraphcurveseparation.pathparser.model.{MovePath, CordPair}
import edu.ist.psu.sagnik.research.linegraphcurveseparation.reader.XMLReader
import edu.ist.psu.sagnik.research.linegraphcurveseparation.writer.SVGWriter
import org.apache.commons.io.FileUtils

/**
 * Created by szr163 on 3/10/16.
 */
object MarkerDetection {

  //For now, we are considering following markers:
  //         _
  // square |_|, diamond: , cross: x, plus: +, triangle: /_\
  //
  
  val MARKERNUMBERTHRESHOLD=5

  def createsSquare(xs:List[SVGPathCurve])=MarkerHelper.createsSquare(xs.toIndexedSeq)
  def createsStar(xs:List[SVGPathCurve])=MarkerHelper.createsStar(xs.toIndexedSeq)
  def createsDiamond(xs:List[SVGPathCurve])=MarkerHelper.createsDiamond(xs.toIndexedSeq)
  def createsTriangle(xs:List[SVGPathCurve])=MarkerHelper.createsTriangle(xs.toIndexedSeq)
  def createsCross(xs:List[SVGPathCurve])=MarkerHelper.createsCross(xs.toIndexedSeq)
  def createsPlus(xs:List[SVGPathCurve])=MarkerHelper.createsPlus(xs.toIndexedSeq)
  def pathIntersects(p1:SVGPathCurve,p2:SVGPathCurve):Boolean=MarkerHelper.pathIntersects(p1,p2)


  def markerThresholdReject(xs:List[List[SVGPathCurve]],markerCreationMethod:(List[SVGPathCurve])=>Boolean):(List[List[SVGPathCurve]],List[List[SVGPathCurve]])=
    if (xs.partition(markerCreationMethod(_))._1.length > MARKERNUMBERTHRESHOLD)
      xs.partition(markerCreationMethod(_))
    else
      (List.empty[List[SVGPathCurve]], xs)

  def curvePathsforMarker(cps:List[List[SVGPathCurve]],marker:List[SVGPathCurve],noMarkerPoints:Int):List[SVGPathCurve]={
    if (cps.isEmpty||marker.isEmpty) List.empty[SVGPathCurve]
    else {
      val possibleCurvePathsforMarker = cps.filter(x => x.count(y => marker.exists(pathIntersects(_, y)))>0.7*noMarkerPoints) //the curve passes through at least 80% of the markers
      if (possibleCurvePathsforMarker.isEmpty) List.empty[SVGPathCurve]
      else possibleCurvePathsforMarker.sortWith(_.count(y => marker.exists(pathIntersects(_, y)))>_.count(y => marker.exists(pathIntersects(_, y)))).head //paths from this style matches maximally
    }
  }


  def apply(curvePaths:Seq[SVGPathCurve])={
    /******* markers that are combination of four paths: squares, stars and diamonds ******/
    val fourPaths=new Combination[SVGPathCurve].combinationTL[SVGPathCurve](
      4,
      1,
      curvePaths.toList,
      RejectFunctions.rectangleNotOverLapReject,
      curvePaths.toList.map(x=>List(x))
    )

    val (sqPaths,nonSqPaths) = markerThresholdReject(fourPaths,createsSquare)
    val (diamondPaths,nonDiamondPaths)= markerThresholdReject(nonSqPaths,createsDiamond) //nonSqPaths.partition(createsDiamond(_))
    val (starPaths,nonStarPaths)= markerThresholdReject(nonDiamondPaths,createsStar)//nonDiamondPaths.partition(createsStar(_))

    //println(curvePaths.length,fourPaths.length,sqPaths.length,nonSqPaths.length,starPaths.length,nonStarPaths.length)

    /******* markers that are combination of three paths: triangles ******/
    val restThreePaths=curvePaths diff (sqPaths.flatten.distinct++diamondPaths.flatten.distinct++starPaths.flatten.distinct)

    val threePaths=new Combination[SVGPathCurve].combinationTL[SVGPathCurve](
      3,
      1,
      restThreePaths.toList,
      RejectFunctions.rectangleNotOverLapReject,
      restThreePaths.toList.map(List(_))
    )

    //There's no way to distinguish between left a right caret or a top or down caret given JUST the bounding box.
    //TODO: a better caret detection algorithm?

    val(trianglePaths,nonTrianglePaths)= markerThresholdReject(threePaths,createsTriangle) //threePaths.partition(createsTriangle(_))

    val restTwoPaths=curvePaths diff (sqPaths.flatten.distinct++diamondPaths.flatten.distinct++starPaths.flatten.distinct++trianglePaths.flatten.distinct)

    val twoPaths=new Combination[SVGPathCurve].combinationTL[SVGPathCurve](
      2,
      1,
      restTwoPaths.toList,
      RejectFunctions.rectangleNotOverLapReject,
      restTwoPaths.toList.map(List(_))
    )

    val (crossPaths,nonCrossPaths)=markerThresholdReject(twoPaths,createsCross) //twoPaths.partition(createsCross(_))
    val (plusPaths,nonPlusPaths)=markerThresholdReject(nonCrossPaths,createsPlus) //nonCrossPaths.partition(createsPlus(_))

    val restPathsforMarkerCurve=curvePaths diff (sqPaths.flatten.distinct++diamondPaths.flatten.distinct++starPaths.flatten.distinct++
      trianglePaths.flatten.distinct++
      crossPaths.flatten.distinct++plusPaths.flatten.distinct)

    val restPathsforMarkerCurveByStyle=restPathsforMarkerCurve.groupBy(_.pathStyle).map(_._2.toList).toList //this is a bit of hack, we know two paths with same id (i.e., they came)
    //from the original SVG path) should have same styles. We are assuming curve paths will be drawn by the same path.

    val markerCurveDictionary=Map(
      "square" -> (sqPaths.flatten.distinct ++ curvePathsforMarker(restPathsforMarkerCurveByStyle,sqPaths.flatten.distinct,sqPaths.length)),
      "diamond" -> (diamondPaths.flatten.distinct ++ curvePathsforMarker(restPathsforMarkerCurveByStyle,diamondPaths.flatten.distinct,diamondPaths.length)),
      "star" -> (starPaths.flatten.distinct ++ curvePathsforMarker(restPathsforMarkerCurveByStyle,starPaths.flatten.distinct,starPaths.length)),
      "triangle" -> (trianglePaths.flatten.distinct ++ curvePathsforMarker(restPathsforMarkerCurveByStyle,trianglePaths.flatten.distinct,trianglePaths.length)),
      "plus" -> (plusPaths.flatten.distinct ++ curvePathsforMarker(restPathsforMarkerCurveByStyle,plusPaths.flatten.distinct,plusPaths.length)),
      "cross" -> (crossPaths.flatten.distinct ++ curvePathsforMarker(restPathsforMarkerCurveByStyle,crossPaths.flatten.distinct,crossPaths.length))
    )

    val markerBasedCurves=markerCurveDictionary.map{case (x,y) => if (y.nonEmpty) Some(SVGCurve(x,y)) else None}.flatten.toSeq
    //println(markerBasedCurves.length)

    val restCurves=CreateCurvesColor.featureBasedSegmentation(curvePaths diff (markerCurveDictionary.getOrElse("square",List.empty[SVGPathCurve]) ++
      markerCurveDictionary.getOrElse("diamond",List.empty[SVGPathCurve])++
      markerCurveDictionary.getOrElse("star",List.empty[SVGPathCurve])++
      markerCurveDictionary.getOrElse("triangle",List.empty[SVGPathCurve])++
      markerCurveDictionary.getOrElse("plus",List.empty[SVGPathCurve])++
      markerCurveDictionary.getOrElse("cross",List.empty[SVGPathCurve])
      ))

    //TODO: following is very heuristic, change as soon as possible
    /*
        val restSqInterSections= rest.filter(x=>sqPaths.flatten.distinct.exists(pathIntersects(_,x))).groupBy(_.pathStyle).map(_._2).toIndexedSeq.sortWith(_.length>_.length)(0)
        val restPlusInterSections= rest.filter(x=>plusPaths.flatten.distinct.exists(pathIntersects(_,x))).groupBy(_.pathStyle).map(_._2).toIndexedSeq.sortWith(_.length>_.length)(0)
        val restStarInterSections= rest.filter(x=>starPaths.flatten.distinct.exists(pathIntersects(_,x))).groupBy(_.pathStyle).map(_._2).toIndexedSeq.sortWith(_.length>_.length)(0)
        val restCrossInterSections=rest.filter(x=>crossPaths.flatten.distinct.exists(pathIntersects(_,x))).groupBy(_.pathStyle).map(_._2).toIndexedSeq.sortWith(_.length>_.length)(0)
    */

    markerBasedCurves++restCurves
  }

  def apply(loc:String,createImages:Boolean):Unit={
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
    val (axes,tics,cPaths)=SeparateAxesGridTickPaths(noFill,width,height)
    val curvePaths=cPaths.filterNot(x=>{x.svgPath.bb match {case Some(bb)=> bb.x1==bb.x2 && bb.y1==bb.y2; case _ => false}})

    val curveGroups=MarkerDetection(curvePaths)
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
    }


  }


  def main(args: Array[String]):Unit= {
    //val loc="data/10.1.1.100.3286-Figure-9.svg"
    //val loc="data/10.1.1.104.3077-Figure-1.svg"
    //val loc="data/10.1.1.105.5053-Figure-2.svg"

    //val loc="src/test/resources/10.1.1.105.5053-Figure-1.svg"
    //val loc="src/test/resources/10.1.1.105.5053-Figure-6.svg"
    //val loc="src/test/resources/10.1.1.108.5575-Figure-16.svg"
    val loc="src/test/resources/10.1.1.113.223-Figure-10.svg"
    //val loc="src/test/resources/10.1.1.100.3286-Figure-9.svg"
    MarkerDetection(loc,true)

  }

}
