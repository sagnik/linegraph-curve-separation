package edu.ist.psu.sagnik.research.linegraphcurveseparation.impl

import edu.ist.psu.sagnik.research.linegraphcurveseparation.model.SVGGroup
import edu.ist.psu.sagnik.research.linegraphcurveseparation.reader.XMLReader
import edu.ist.psu.sagnik.research.linegraphcurveseparation.transformparser.impl.TransformParser

/**
 * Created by sagnik on 11/11/15.
 */
object GroupExtract {
  def apply(fileLoc:String)=getGroups(XMLReader(fileLoc))

  def getGroups(xmlContent:scala.xml.Elem):Seq[SVGGroup]=
    (xmlContent \\ "g").map{
      x=>
        SVGGroup(
          id= x \@ "id",
          gtContent = x \@ "transform",
          gContent= x.toString,
          transformOps = TransformParser(x \@ "transform")
        )
    }

  def main(args: Array[String]):Unit={
    val loc="src/test/resources/10.1.1.108.9317-Figure-4.svg"
    val groups=GroupExtract(loc)
    groups.foreach(a=>println(s"[group id]: ${a.id} [tramsformops]: ${a.transformOps}"))
  }
}
