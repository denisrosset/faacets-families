name := "FaacetsFamilies"

version := "1.0"

scalaVersion := "2.10.2"

resolvers ++= Seq(
  "Sonatype Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
  "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases",
  "www2.ph.ed.ac.uk-releases" at "http://www2.ph.ed.ac.uk/maven2"
)

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.10.0" % "test",
  "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test",
  "org.spire-math" %% "spire" % "0.6.0"
)

scalacOptions ++= Seq("-unchecked", "-feature", "-deprecation") 
