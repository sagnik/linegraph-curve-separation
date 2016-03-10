package edu.ist.psu.sagnik.research.linegraphcurveseparation.impl

import edu.ist.psu.sagnik.research.linegraphcurveseparation.model._
import edu.ist.psu.sagnik.research.linegraphcurveseparation.pathparser.impl.SVGPathfromDString
import edu.ist.psu.sagnik.research.linegraphcurveseparation.reader.XMLReader
import edu.ist.psu.sagnik.research.linegraphcurveseparation.transformparser.impl.TransformParser

/**
 * Created by szr163 on 11/8/15.
 */


object SVGPathExtract {

  def apply(fileLoc:String,sps:Boolean)=
    if (sps)
      getPaths(XMLReader(fileLoc))
    else
      getPaths(XMLReader(fileLoc),GroupExtract.apply(fileLoc))

  def getPaths(xmlContent:scala.xml.Elem):Seq[SVGPathCurve]= {
    //This is a path splitted file as created by edu.ist.psu.sagnik.research.linegraphcurveseparation.impl.SplitPaths.
    //it doesn't have any group.
    (xmlContent \ "path").map(x=>
      SVGPathXML(
        svgPath=SVGPath(
          id=x \@ "id",
          pdContent = x \@ "d",
          pOps = SVGPathfromDString.getPathCommands(x.attribute("d") match {case Some(con)=>con.text case _ => ""}),
          pContent = x.toString,
          groups=Seq.empty[SVGGroup],
          transformOps=TransformParser(x \@ "transform"),
          bb=None
        ),
        styleXML=x
      )
    ).map(x=>SVGPathXML(svgPath=SVGPathBB(x.svgPath),styleXML = x.styleXML)).map(x=>getPathStyleObject(x))
  }

  def getPaths(xmlContent:scala.xml.Elem, svgGroups:Seq[SVGGroup]):Seq[SVGPathCurve]= {
    //There's exactly one group with a translate operation, but that might not have an ID.
    val gid=if (((xmlContent \ "g")\@ "id").isEmpty) "g1" else ((xmlContent \ "g")\@ "id")
    val gtContent = (xmlContent \ "g")\@ "transform"
    val gContent= (xmlContent \ "g").toString
    val transformOps = TransformParser((xmlContent \ "g") \@ "transform")
    ((xmlContent \ "g") \ "path").map(x=>
      SVGPathXML(
        svgPath=SVGPath(
          id=x \@ "id",
          pdContent = x \@ "d",
          pOps = SVGPathfromDString.getPathCommands(x.attribute("d") match {case Some(con)=>con.text case _ => ""}),
          pContent = x.toString,
          groups=List(SVGGroup(
            id=gid,
            gtContent=gtContent,
            gContent = gContent,
            transformOps = transformOps
          )).toIndexedSeq,
          transformOps=TransformParser(x \@ "transform"),
          bb=None
        ),
        styleXML=x
      )
    ).map(x=>SVGPathXML(svgPath=SVGPathBB(x.svgPath),styleXML = x.styleXML)).map(x=>getPathStyleObject(x))
  }

  //TODO: for paths with not none fill elements, this is incorrect.  
  def getPathStyleObject(x:SVGPathXML):SVGPathCurve={
    SVGPathCurve(
      svgPath=x.svgPath,
      pathStyle=PathStyle(
        fill= returnPattern(x.styleXML\@"style","fill"), //we are deliberately omitting the fill information.
        stroke= returnPattern(x.styleXML\@"style","stroke"),
        strokeWidth = if ("-1".equals(returnPattern(x.styleXML\@"style","stroke-width"))) (-1f).toString else returnPattern(x.styleXML\@"style","stroke-width") ,
        strokeLinecap = returnPattern(x.styleXML\@"style","stroke-linecap"),
        strokeLinejoin = returnPattern(x.styleXML\@"style","stroke-linejoin"),
        strokeMiterlimit = returnPattern(x.styleXML\@"style","stroke-miterlimit"),
        strokeDasharray = returnPattern(x.styleXML\@"style","stroke-dasharray"),
        strokeOpacity = if ("-1".equals(returnPattern(x.styleXML\@"style","stroke-opacity"))) (-1f).toString else returnPattern(x.styleXML\@"style","stroke-opacity"),
        fillOpacity= if ("-1".equals(returnPattern(x.styleXML\@"style","fill-opacity"))) (-1f).toString else returnPattern(x.styleXML\@"style","fill-opacity")
      )
    )
  }
  def returnPattern(pC:String,s:String)=
    if (pC.split(";").filter(x => x.contains(s)).length == 0) "-1"
    else pC.split(";").filter(x => x.contains(s))(0).split(":")(1)
}
