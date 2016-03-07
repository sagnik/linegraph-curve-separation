package edu.ist.psu.sagnik.research.linegraphcurveseparation.test

/**
 * Created by sagnik on 3/7/16.
 */

import java.io.File

object DirContent {
  import scala.util.matching.Regex
  def apply(f: File, r: Regex): Array[File] = {
    val these = f.listFiles
    val good = these.filter(f => r.findFirstIn(f.getName).isDefined)
    good ++ these.filter(_.isDirectory).flatMap(apply(_,r))
  }
}