package edu.ist.psu.sagnik.research.linegraphcurveseparation.test

import edu.ist.psu.sagnik.research.linegraphcurveseparation.impl.{SplitPaths, MarkerDetection}
import org.scalatest.FunSpec

import java.io.File
import scala.language.postfixOps
/**
 * Created by sagnik on 3/7/16.
 */
class BatchCurveCreationTest extends FunSpec {

  describe("testing the SVG output by printing") {
    import edu.ist.psu.sagnik.research.linegraphcurveseparation.test.DataLocation._
    it("Should create curve SVG files at the specific location.") {
      val svgFiles=(DirContent(new File(svgDirLoc), ".svg" r).toIndexedSeq.map(x=>x.getAbsolutePath))
      svgFiles.foreach(x=>{
        println(x);
        try {
          MarkerDetection(x,true)
        }
        catch{
          case a:org.xml.sax.SAXParseException => println(s"ill formed SVG for ${x}")
        }
      })
    }
  }
}
