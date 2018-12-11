organization := "org.ddahl"
name := "commonsMath"
//version := "1.2.1"
version := "1.2.2-SNAPSHOT"

scalaVersion := "2.12.8"
crossScalaVersions := Seq("2.11.12", "2.12.8")
scalacOptions ++= List("-feature","-deprecation", "-unchecked", "-Xlint")

libraryDependencies ++= Seq(
  "org.apache.commons" % "commons-math3" % "3.6.1",
  "org.scalatest"  %% "scalatest"  % "3.0.1"  % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"
)

publishMavenStyle := true

publishTo := sonatypePublishTo.value

pomExtra := (
  <url>https://github.com/dbdahl/commonsMath/</url>
  <licenses>
    <license>
      <name>Apache License 2.0</name>
      <url>https://www.apache.org/licenses/LICENSE-2.0</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <scm>
    <url>https://github.com/dbdahl/commonsMath/</url>
    <connection>scm:git:https://github.com/dbdahl/rscala.git</connection>
  </scm>
  <developers>
    <developer>
      <id>dbdahl</id>
      <name>David B. Dahl</name>
      <url>https://dahl.byu.edu</url>
    </developer>
  </developers>
)

