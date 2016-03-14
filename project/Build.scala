import sbt._
import Keys._

object FastParsersBuild extends Build {
	import PublishSettings._
	scalaVersion := "2.11.7"

  def commonSettings = Seq(
    version := "0.1-SNAPSHOT",
	  scalaVersion := "2.11.7",
    //scalacOptions := Seq("-optimize"), // <-- slows down compilation drastically
	  libraryDependencies ++=  Seq(
		  "org.scala-lang" % "scala-compiler"  % scalaVersion.value % "provided",
		  "org.scala-lang" % "scala-reflect" % scalaVersion.value
	  )
  ) ++ publishSettings


  lazy val Examples = Project(
	  id = "Examples",
	  base = file("Examples"),
	  dependencies = Seq(FastParsers),
	  settings = commonSettings ++ Seq(
	    // include the macro classes and resources in the main jar
	    mappings in (Compile, packageBin) ++= mappings.in(FastParsers, Compile, packageBin).value,
	    // include the macro sources in the main source jar
	    mappings in (Compile, packageSrc) ++= mappings.in(FastParsers, Compile, packageSrc).value
	  )
  )

  lazy val FastParsers = Project(
    id = "FastParsers",
    base = file("FastParsers"),
    settings = commonSettings ++ publishableSettings ++ Seq(
  	  resolvers ++= Seq(
  		  "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"
  		),
  		//addSbtPlugin("org.scoverage" %% "sbt-scoverage" % "0.98.0"),

  		libraryDependencies ++= Seq(
        "org.scalatest" %% "scalatest" % "3.0.0-M15" % "test",
        "org.scalacheck" %% "scalacheck" % "1.11.5" % "test",
        "com.storm-enroute" %% "scalameter" % "0.7" % "test",
        "com.lihaoyi" %% "pprint" % "0.3.8",
        "com.lihaoyi" %% "fastparse" % "0.3.4"
      ),

  		testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework"),
      logBuffered := false
  	)
  )
}
