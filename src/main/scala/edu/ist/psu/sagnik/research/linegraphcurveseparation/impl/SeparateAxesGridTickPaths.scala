package edu.ist.psu.sagnik.research.linegraphcurveseparation.impl

import edu.ist.psu.sagnik.research.linegraphcurveseparation.model.{Rectangle, SVGPathCurve}
import edu.ist.psu.sagnik.research.linegraphcurveseparation.pathparser.impl.SVGPathfromDString
import edu.ist.psu.sagnik.research.linegraphcurveseparation.pathparser.model.{Move, Line, MovePath, CordPair}
import edu.ist.psu.sagnik.research.linegraphcurveseparation.reader.XMLReader
import edu.ist.psu.sagnik.research.linegraphcurveseparation.writer.SVGWriter

/**
 * Created by sagnik on 3/8/16.
 */
//To separate
object SeparateAxesGridTickPaths {

  val TMTHRESHOLD=5f

  val AXESCOLORS=Seq("#000000","#696969","#2F4F4F","#696969","#708090","#778899","#808080","#A9A9A9","#D3D3D3","#8E9092")

  def apply(svgpathCurves: Seq[SVGPathCurve],width:Float,height:Float):(Seq[SVGPathCurve],Seq[SVGPathCurve],Seq[SVGPathCurve])= {

    val (paGrids, others) = svgpathCurves.partition(x => isAxesOrGrid(x, width, height))

    val possibleAxesAndGrids=paGrids

    val axes = if (possibleAxesAndGrids.length<5)
      possibleAxesAndGrids
    else // we need to find 2 or 4 paths that doesn't have any other to left or
      List(
        getAxis(possibleAxesAndGrids,"left"), //left
        getAxis(possibleAxesAndGrids,"right"),
        getAxis(possibleAxesAndGrids,"top"),
        getAxis(possibleAxesAndGrids,"bottom")
      ).flatten

    val (tics,curvePaths)=others.partition(x=>
      (x.svgPath.bb match{ case Some(bb)=>(bb.x1==bb.x2 ||bb.y1==bb.y2); case _ => false}) &&
        (x.svgPath.bb match{ case Some(bb)=>(bb.x2-bb.x1<TMTHRESHOLD && bb.y2-bb.y1<TMTHRESHOLD); case _ => false}) &&
        axes.exists(a=>pathOverlap(x,a))
    )
    (axes,tics,curvePaths)
  }

  def isVertical(bb:Option[Rectangle]):Boolean=bb match{case Some(bb)=> bb.x1==bb.x2; case _ => false}

  def getAxis(ps:Seq[SVGPathCurve],pos:String):Option[SVGPathCurve]=
    if ("left".equals(pos))
      ps.filter(a=> isVertical(a.svgPath.bb)).sortWith(_.svgPath.bb.getOrElse(Rectangle(0f,0f,0f,0f)).x1<_.svgPath.bb.getOrElse(Rectangle(0f,0f,0f,0f)).x1).headOption
    else if ("right".equals(pos))
      ps.filter(a=> isVertical(a.svgPath.bb)).sortWith(_.svgPath.bb.getOrElse(Rectangle(0f,0f,0f,0f)).x2>_.svgPath.bb.getOrElse(Rectangle(0f,0f,0f,0f)).x2).headOption
    else if ("top".equals(pos))
      ps.filter(a=> !isVertical(a.svgPath.bb)).sortWith(_.svgPath.bb.getOrElse(Rectangle(0f,0f,0f,0f)).y1<_.svgPath.bb.getOrElse(Rectangle(0f,0f,0f,0f)).y1).headOption
    else if ("bottom".equals(pos))
      ps.filter(a=> !isVertical(a.svgPath.bb)).sortWith(_.svgPath.bb.getOrElse(Rectangle(0f,0f,0f,0f)).y2>_.svgPath.bb.getOrElse(Rectangle(0f,0f,0f,0f)).y2).headOption
    else
      ps.headOption // should never reach here



  def pathOverlap(a:SVGPathCurve,b:SVGPathCurve):Boolean={
    (a.svgPath.bb,b.svgPath.bb) match{
      case (Some(aBB),Some(bBB))=>Rectangle.rectTouches(Rectangle(aBB.x1-1f,aBB.y1-1f,aBB.x2+1,aBB.y2+1f),bBB)
      case _ => false
    }
  }

  val AXESRATIOTHRESHOLD=0.1f

  def isAxesOrGrid(x:SVGPathCurve,W:Float,H:Float):Boolean=
    if (x.svgPath.pOps.length<2 || !x.svgPath.pOps(0).isInstanceOf[Move] || !x.svgPath.pOps(1).isInstanceOf[Line])
      false
    else x.svgPath.bb match {
      case Some(bb)=>
        (bb.x1 == bb.x2 || bb.y1 == bb.y2) && //horizontal or vertical
          (bb.y2 - bb.y1 > AXESRATIOTHRESHOLD * H || bb.x2 - bb.x1 > AXESRATIOTHRESHOLD * W) && //sufficiently large
          (AXESCOLORS.contains(x.pathStyle.stroke.getOrElse("#000000")) || AXESCOLORS.contains(x.pathStyle.stroke.getOrElse("#000000").toUpperCase)) //drawn with black or grey
      case _ => false
    }

  def main(args: Array[String]):Unit= {
    //val loc = "data/10.1.1.108.9317-Figure-4.svg"
    //val loc = "data/10.1.1.105.5053-Figure-6.svg"
    //val loc="src/test/resources/hassan-Figure-2.svg"
    //val loc="data/10.1.1.105.5053-Figure-2.svg"
    //val loc="data/10.1.1.112.9247-Figure-4.svg"
    val loc="data/10.1.1.100.3286-Figure-9.svg" //this is a good example. two paths, one drawn by a dashed
    // array and one by a straight line completely overlap, therefore you will get to see the second path as one of the axes lines.

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
    val (axes,tics,_)=SeparateAxesGridTickPaths(noFill,width,height)
    SVGWriter(axes++tics,loc,"ats")
  }
}
