name := "suag"

version := "1.0"

scalaVersion := "2.11.2"

fork := true

resolvers ++= Seq(
  "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "org.scalacheck" %% "scalacheck" % "1.11.5" % "test",
  "com.chuusai" %% "shapeless" % "2.0.0"
)
