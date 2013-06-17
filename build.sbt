name := "Scala-Virtualized Tutorials"

version := "2.0"

scalaOrganization := "org.scala-lang.virtualized"

scalaVersion := "2.10.2-RC1"

scalaSource in Compile <<= baseDirectory(_ / "src")

libraryDependencies += "org.scala-lang.virtualized" % "scala-compiler" % "2.10.2-RC1"

