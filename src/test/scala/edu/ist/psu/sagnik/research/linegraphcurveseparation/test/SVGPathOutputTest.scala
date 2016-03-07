package edu.ist.psu.sagnik.research.linegraphcurveseparation.test

/**
 * Created by sagnik on 12/23/15.
 */

import edu.ist.psu.sagnik.research.linegraphcurveseparation.impl.SVGPathExtract
import org.scalatest.FunSpec

class SVGPathOutputTest extends FunSpec {

  describe("testing the SVG output by printing") {
    import edu.ist.psu.sagnik.research.linegraphcurveseparation.test.DataLocation._
    it("should print the path info.") {
      val results=SVGPathExtract(svgFileLoc)
      //results.foreach(x=>println(x.pContent))


      results.foreach(
        x=>println(s"[path id]: ${x.svgPath.id}, [pStyleconent]: ${x.pathStyle}" +
          s"[pathbb ]: ${
            x.svgPath.bb match{
              case Some(bb) => (bb.x1,bb.y1,bb.x2-bb.x1,bb.y2-bb.y1)
              case None => None
            }
          }" +
          s"")
      )

    }
  }

}
