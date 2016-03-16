package edu.ist.psu.sagnik.research.linegraphcurveseparation.impl

import edu.ist.psu.sagnik.research.linegraphcurveseparation.model.{SVGCurve, Rectangle, SVGPathCurve}
import edu.ist.psu.sagnik.research.linegraphcurveseparation.pathparser.model.{MovePath, CordPair}
import edu.ist.psu.sagnik.research.linegraphcurveseparation.reader.XMLReader
import edu.ist.psu.sagnik.research.linegraphcurveseparation.writer.SVGWriter

/**
 * Created by szr163 on 3/10/16.
 */
object MarkerDetection {

  //For now, we are considering following markers:
  //         _
  // square |_|, diamond: , cross: x, plus: +, triangle: /_\
  //
  // TODO: star: *

  val MARKERNUMBERTHRESHOLD=5

  def createsSquare(xs:List[SVGPathCurve])=MarkerHelper.createsSquare(xs.toIndexedSeq)
  def createsStar(xs:List[SVGPathCurve])=MarkerHelper.createsStar(xs.toIndexedSeq)
  def createsDiamond(xs:List[SVGPathCurve])=MarkerHelper.createsDiamond(xs.toIndexedSeq)
  def createsTriangle(xs:List[SVGPathCurve])=MarkerHelper.createsTriangle(xs.toIndexedSeq)
  def createsCross(xs:List[SVGPathCurve])=MarkerHelper.createsCross(xs.toIndexedSeq)
  def createsPlus(xs:List[SVGPathCurve])=MarkerHelper.createsPlus(xs.toIndexedSeq)



  def markerThresholdReject(xs:List[List[SVGPathCurve]],markerCreationMethod:(List[SVGPathCurve])=>Boolean):(List[List[SVGPathCurve]],List[List[SVGPathCurve]])=
    if (xs.partition(markerCreationMethod(_))._1.length > MARKERNUMBERTHRESHOLD)
      xs.partition(markerCreationMethod(_))
    else
      (List.empty[List[SVGPathCurve]], xs)

  def apply(loc:String,createImages:Boolean)={
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

    val markerDictionary=Map(
     "square" -> (if (sqPaths.nonEmpty) Some(sqPaths.flatten.distinct) else None),
     "diamond" -> (if (diamondPaths.nonEmpty) Some(diamondPaths.flatten.distinct) else None),
     "star" -> (if (starPaths.nonEmpty) Some(starPaths.flatten.distinct) else None),
     "triangle" -> (if (trianglePaths.nonEmpty) Some(trianglePaths.flatten.distinct) else None),
     "plus" -> (if (plusPaths.nonEmpty) Some(plusPaths.flatten.distinct) else None),
     "cross" -> (if (crossPaths.nonEmpty) Some(crossPaths.flatten.distinct) else None)
    )

    if (markerDictionary.values.toList.flatten.isEmpty)
     {
       println("we got no marker"); sys.exit(1)
     }
    else{
      markerDictionary.foreach{ case (x,y)=>println(x, y.getOrElse(List.empty[SVGPathCurve]).length)}
    }



    val rest=curvePaths diff (sqPaths.flatten.distinct++diamondPaths.flatten.distinct++starPaths.flatten.distinct++
      trianglePaths.flatten.distinct++
      crossPaths.flatten.distinct++plusPaths.flatten.distinct)


    val restStyleDictionary=rest.groupBy(_.pathStyle)

    //TODO: following is very heuristic, change as soon as possible
/*
    val restSqInterSections= rest.filter(x=>sqPaths.flatten.distinct.exists(pathIntersects(_,x))).groupBy(_.pathStyle).map(_._2).toIndexedSeq.sortWith(_.length>_.length)(0)
    val restPlusInterSections= rest.filter(x=>plusPaths.flatten.distinct.exists(pathIntersects(_,x))).groupBy(_.pathStyle).map(_._2).toIndexedSeq.sortWith(_.length>_.length)(0)
    val restStarInterSections= rest.filter(x=>starPaths.flatten.distinct.exists(pathIntersects(_,x))).groupBy(_.pathStyle).map(_._2).toIndexedSeq.sortWith(_.length>_.length)(0)
    val restCrossInterSections=rest.filter(x=>crossPaths.flatten.distinct.exists(pathIntersects(_,x))).groupBy(_.pathStyle).map(_._2).toIndexedSeq.sortWith(_.length>_.length)(0)
*/

    //val twoPaths=new Combination[SVGPathCurve].combinationTL[SVGPathCurve](2,1,curvePaths.toList,RejectFunctions.rectangleNotOverLapReject,curvePaths.toList.map(x=>List(x)))
    //val (caretPaths,nonCaretPaths) = twoPaths.partition(xs=>createsCaret(xs))
    //println(curvePaths.length,twoPaths.length,caretPaths.length,nonCaretPaths.length)

    if (createImages) {
      if (sqPaths.flatten.distinct.nonEmpty) SVGWriter(sqPaths.flatten.distinct,loc,"sq")
      if (diamondPaths.flatten.distinct.nonEmpty) SVGWriter(diamondPaths.flatten.distinct,loc,"dm")
      if (starPaths.flatten.distinct.nonEmpty) SVGWriter(starPaths.flatten.distinct,loc,"star")
      if (trianglePaths.flatten.distinct.nonEmpty) SVGWriter(trianglePaths.flatten.distinct,loc,"tr")
      if (plusPaths.flatten.distinct.nonEmpty) SVGWriter(plusPaths.flatten.distinct,loc,"pl")
      if (crossPaths.flatten.distinct.nonEmpty) SVGWriter(crossPaths.flatten.distinct,loc,"cr")
      //SVGWriter(rest,loc,"rest")
    }

  }


  def main(args: Array[String]):Unit= {
    //val loc="data/10.1.1.100.3286-Figure-9.svg"
    //val loc="data/10.1.1.104.3077-Figure-1.svg"
    //val loc="data/10.1.1.105.5053-Figure-2.svg"

    //val loc="src/test/resources/10.1.1.105.5053-Figure-1.svg"
    val loc="src/test/resources/10.1.1.105.5053-Figure-6.svg"
    //val loc="src/test/resources/10.1.1.108.5575-Figure-16.svg"
    //val loc="src/test/resources/10.1.1.113.223-Figure-10.svg"
    //val loc="src/test/resources/10.1.1.100.3286-Figure-9.svg"
    MarkerDetection(loc,true)

  }

}
