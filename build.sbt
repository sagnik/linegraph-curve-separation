organization := "edu.ist.psu.sagnik.research"

name := "linegraphcurveseparation"

version := "0.0.1"

scalaVersion := "2.11.7"

javacOptions += "-Xlint:unchecked"

//change these for black and white curves

//assemblyJarName in assembly := "linegraphcurveseparation-markeronly.jar"

assemblyJarName in assembly := "linegraphcurveseparation-color.jar"

//mainClass in assembly := Some("edu.ist.psu.sagnik.research.linegraphcurveseparation.impl.MarkerDetection")
mainClass in assembly := Some("edu.ist.psu.sagnik.research.linegraphcurveseparation.impl.CreateCurvesColor")

test in assembly := {}

resolvers ++= Seq(
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/",
  "Sonatype Shapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
  "JAI releases" at "http://maven.geotoolkit.org/"
)

libraryDependencies ++= Seq(
   //jackson for json
  "org.json4s" %% "json4s-native" % "3.2.11",
  "org.json4s" %% "json4s-jackson" % "3.2.10",
  // testing
  "org.scalatest"        %% "scalatest"  %  "2.2.4",
  //log4j
  "log4j" % "log4j" % "1.2.15" excludeAll(
    ExclusionRule(organization = "com.sun.jdmk"),
    ExclusionRule(organization = "com.sun.jmx"),
    ExclusionRule(organization = "javax.jms")
    )
  )

libraryDependencies += "commons-collections" % "commons-collections" % "3.2.1"

libraryDependencies += "org.scalanlp" %% "breeze" % "0.11.2"

libraryDependencies += "org.scalanlp" %% "breeze-natives" % "0.11.2"

libraryDependencies += "commons-io" % "commons-io" % "2.4"


javacOptions ++= Seq("-source", "1.8", "-target", "1.8")

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Xfatal-warnings")

fork := true

testOptions += Tests.Argument(TestFrameworks.JUnit, "-v")

testOptions in Test += Tests.Argument("-oF")

fork in Test := false

parallelExecution in Test := false

