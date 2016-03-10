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

  val AXESCOLORS=Seq("#000000","#696969","#2F4F4F","#696969","#708090","#778899","#808080","#A9A9A9","#D3D3D3")

  def apply(svgpathCurves: Seq[SVGPathCurve],width:Float,height:Float):(Seq[SVGPathCurve],Seq[SVGPathCurve],Seq[SVGPathCurve])= {

    val (possibleAxesAndGrids, others) = svgpathCurves.partition(x => isAxesOrGrid(x, width, height))

    val axes = if (possibleAxesAndGrids.length<5)
      possibleAxesAndGrids
    else
      (0 to possibleAxesAndGrids.length - 1).
        map(index => (distanceFromBoundary(width, height, possibleAxesAndGrids(index).svgPath.bb), index))
        .sortBy(_._1).map(a => a._2)
        .map(b => possibleAxesAndGrids(b)).slice(0, 4)

    val (tics,curvePaths)=others.partition(x=>
      (x.svgPath.bb match{ case Some(bb)=>(bb.x1==bb.x2 ||bb.y1==bb.y2); case _ => false}) &&
        (x.svgPath.bb match{ case Some(bb)=>(bb.x2-bb.x1<TMTHRESHOLD && bb.y2-bb.y1<TMTHRESHOLD); case _ => false}) &&
        axes.exists(a=>pathOverlap(x,a))
    )

    (axes,tics,curvePaths)
  }

  //TODO: more testing
  implicit def rd(r1:Rectangle,r2:Rectangle)=Rectangle.rectDistance(r1,r2)
  def distanceFromBoundary(W:Float,H:Float,bb:Option[Rectangle]):Float= bb match{
    case Some(bb)=>List(
      rd(Rectangle(0f,0f,H,0f),bb),
      rd(Rectangle(H,0f,W,H),bb),
      rd(Rectangle(W,0f,W,H),bb),
      rd(Rectangle(0f,0f,W,0f),bb)
    ).min
    case _ => 0f
  }


  def pathOverlap(a:SVGPathCurve,b:SVGPathCurve):Boolean={
    (a.svgPath.bb,b.svgPath.bb) match{
      case (Some(aBB),Some(bBB))=>Rectangle.rectTouches(Rectangle(aBB.x1-1f,aBB.y1-1f,aBB.x2+1,aBB.y2+1f),bBB)
      case _ => false
    }
  }

  val AXESRATIOTHRESHOLD=0.8f

  def isAxesOrGrid(x:SVGPathCurve,W:Float,H:Float):Boolean=
    if (x.svgPath.pOps.length<2 || !x.svgPath.pOps(0).isInstanceOf[Move] || !x.svgPath.pOps(1).isInstanceOf[Line])
      false
    else x.svgPath.bb match {
      case Some(bb)=>
        (bb.x1==bb.x2 || bb.y1==bb.y2) && //horizontal or vertical
          (bb.y2-bb.y1>AXESRATIOTHRESHOLD*H || bb.x2-bb.x1>AXESRATIOTHRESHOLD*W) && //sufficiently large
          (!AXESCOLORS.contains(x.pathStyle.fill) || !AXESCOLORS.contains(x.pathStyle.fill.toUpperCase))  //drawn with black or grey
      case _ => false
    }

  def main(args: Array[String]):Unit= {
    //val loc = "data/10.1.1.108.9317-Figure-4.svg"
    //val loc = "data/10.1.1.105.5053-Figure-6.svg"
    val loc="src/test/resources/hassan-Figure-2.svg"
    //val loc="data/10.1.1.105.5053-Figure-2.svg"
    //val loc="data/10.1.1.112.9247-Figure-4.svg"

    val svgPaths=
     if (loc.contains("-sps")) //this SVG has already paths split
      SVGPathExtract(loc, true).groupBy(x => x.svgPath.pdContent).map(_._2.head).toIndexedSeq //this step is done to remove duplicate paths that can come from "close" paths
    else
       SVGPathExtract(loc,false).map(
         c=>
           SplitPaths.splitPath(
             c.svgPath.pOps.slice(1,c.svgPath.pOps.length),
             c,
             CordPair(c.svgPath.pOps(0).args(0).asInstanceOf[MovePath].eP.x,c.svgPath.pOps(0).args(0).asInstanceOf[MovePath].eP.y),
             Seq.empty[SVGPathCurve]
           )
       ).flatten.groupBy(x => x.svgPath.pdContent).map(_._2.head).toIndexedSeq

    //TODO: possible exceptions
    val height = ((XMLReader(loc) \\ "svg")(0) \@ "height").toFloat
    val width = ((XMLReader(loc) \\ "svg")(0) \@ "width").toFloat
    val (axes,tics,_)=SeparateAxesGridTickPaths(svgPaths,width,height)
    SVGWriter(axes++tics,loc,"ats")
  }
}
