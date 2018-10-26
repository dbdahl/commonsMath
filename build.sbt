organization := "org.ddahl"
name := "commonsMath"
//version := "1.3"
version := "1.3-SNAPSHOT"

scalaVersion := "2.12.7"
crossScalaVersions := Seq("2.11.12", "2.12.7")
scalacOptions ++= List("-feature","-deprecation", "-unchecked", "-Xlint")

libraryDependencies ++= Seq(
  "org.apache.commons" % "commons-math3" % "3.6.1",
  "org.scalatest"  %% "scalatest"  % "3.0.1"  % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"
)

