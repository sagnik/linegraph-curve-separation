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

    def createsSquare(xs:List[SVGPathCurve])=MarkerHelper.createsSquare(xs.toIndexedSeq)
    def createsStar(xs:List[SVGPathCurve])=MarkerHelper.createsStar(xs.toIndexedSeq)
    def createsDiamond(xs:List[SVGPathCurve])=MarkerHelper.createsDiamond(xs.toIndexedSeq)
    def createsTriangle(xs:List[SVGPathCurve])=MarkerHelper.createsTriangle(xs.toIndexedSeq)
    def createsCross(xs:List[SVGPathCurve])=MarkerHelper.createsCross(xs.toIndexedSeq)
    def createsPlus(xs:List[SVGPathCurve])=MarkerHelper.createsPlus(xs.toIndexedSeq)


    /******* markers that are combination of four paths: squares, stars and diamonds ******/
    val fourPaths=new Combination[SVGPathCurve].combinationTL[SVGPathCurve](
    4,
    1,
    curvePaths.toList,
    RejectFunctions.rectangleNotOverLapReject,
    curvePaths.toList.map(x=>List(x))
    )

    val (sqPaths,nonSqPaths) = fourPaths.partition(createsSquare(_))
    val (diamondPaths,nonDiamondPaths)=nonSqPaths.partition(createsDiamond(_))
    val (starPaths,nonStarPaths)=nonDiamondPaths.partition(createsStar(_))

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

    val(trianglePaths,nonTrianglePaths)=threePaths.partition(createsTriangle(_))

    val restTwoPaths=curvePaths diff (sqPaths.flatten.distinct++diamondPaths.flatten.distinct++starPaths.flatten.distinct++trianglePaths.flatten.distinct)

    val twoPaths=new Combination[SVGPathCurve].combinationTL[SVGPathCurve](
      2,
      1,
      restTwoPaths.toList,
      RejectFunctions.rectangleNotOverLapReject,
      restTwoPaths.toList.map(List(_))
    )

    val (crossPaths,nonCrossPaths)=twoPaths.partition(createsCross(_))
    val (plusPaths,nonPlusPaths)=nonCrossPaths.partition(createsPlus(_))

    val rest=curvePaths diff (sqPaths.flatten.distinct++diamondPaths.flatten.distinct++starPaths.flatten.distinct++
      trianglePaths.flatten.distinct++
      crossPaths.flatten.distinct++plusPaths.flatten.distinct)

    def pathIntersects(p1:SVGPathCurve,p2:SVGPathCurve)=MarkerHelper.pathIntersects(p1,p2)

    //val restSqInterSections= rest.filter(x=>sqPaths.flatten.distinct.exists(pathIntersects(_,x)))

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

    val loc="src/test/resources/10.1.1.105.5053-Figure-1.svg"
    //val loc="src/test/resources/10.1.1.108.5575-Figure-16.svg"
    //val loc="src/test/resources/10.1.1.113.223-Figure-10.svg"
    //val loc="src/test/resources/10.1.1.100.3286-Figure-9.svg"
    MarkerDetection(loc,true)

  }

}
