package edu.ist.psu.sagnik.research.linegraphcurveseparation.impl

import edu.ist.psu.sagnik.research.linegraphcurveseparation.model.{PathGroups, SVGGroup, SVGPath}
import edu.ist.psu.sagnik.research.linegraphcurveseparation.pathparser.impl.SVGPathfromDString
import edu.ist.psu.sagnik.research.linegraphcurveseparation.reader.XMLReader
import edu.ist.psu.sagnik.research.linegraphcurveseparation.transformparser.impl.TransformParser

import scala.xml.{Node, NodeSeq}

/**
 * Created by szr163 on 11/8/15.
 */


object SVGPathExtract {

  def apply(fileLoc:String)=getPaths(XMLReader(fileLoc),GroupExtract.apply(fileLoc))

  def getPaths(xmlContent:scala.xml.Elem, svgGroups:Seq[SVGGroup]):Unit= {

    //There's exactly one group with a translate operation, but that might not have an ID.
    val gid=if (((xmlContent \ "g")\@ "id").isEmpty) "g1" else ((xmlContent \ "g")\@ "id")
    println(gid)

    (xmlContent \ "g").foreach(x=>println(x.toString))


/*
    val SVGCurvepaths=
      (xmlContent \ "g").fil
        map(x =>
        SVGPath(
          x.path.attribute("id") match {
            case Some(idExists) => idExists.text
            case _ => "noID"
          },
          pdContent = x.path.attribute("d") match {
            case Some(con) => con.text
            case _ => ""
          },
          pContent = x.path.toString(),
          pOps = SVGPathfromDString.getPathCommands(x.path.attribute("d") match {case Some(con)=>con.text case _ => ""}),
          groups = svgGroups.filter(a=>x.gIds.contains(a.id)),
          transformOps=TransformParser(x.path \@ "transform"),
          bb=None
        )
      )
    (topLevelSVGPaths ++ lowerLevelSVGpaths).map(x=>SVGPathBB.apply(x))
*/
  }

  def iterateOverGroups(tlGs:NodeSeq,pathGIDs:Seq[PathGroups],gIdMap:Map[String,Seq[String]]):Seq[PathGroups]=
    if (tlGs.length==0) pathGIDs
    else{
      val newGId=tlGs.head \@ "id" //TODO: What happens if the group doesn't have an ID?
      val parentGids=gIdMap.get(newGId) match{
          case(Some(a))=> a
          case None => Seq.empty[String]
        }

      val thisTLGpathGIDs=pathGIDs ++ (tlGs.head \ "path").map(x=>PathGroups(x,newGId +: parentGids))
      iterateOverGroups(tlGs.tail ++ tlGs.head \"g",thisTLGpathGIDs, gIdMap)

    }


  def getGroupParents(tlGs: NodeSeq, parentMap:Map[String,Seq[String]]):Map[String,Seq[String]]=
    if (tlGs.isEmpty) parentMap
    else {
      val newGId = tlGs.head \@ "id" //TODO: What happens if the group doesn't have an ID?
      val newtlGs = tlGs.head \ "g"
      val newParentMap = parentMap ++ newtlGs.map(x => (x \@ "id", newGId)).toMap.map {
        case (k, v) => (k,
          parentMap.get(v) match {
            case Some(a) => v +: a
            case _ => List(v)
          }
          )
      }
      getGroupParents(tlGs.tail ++ newtlGs, newParentMap)
    }


}
