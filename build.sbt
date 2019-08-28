name := "Scala-Virtualized Tutorials"

version := "3.0"

scalaOrganization := "org.scala-lang.virtualized"

scalaVersion := "2.11.2"

scalaSource in Compile := baseDirectory.value / "src"

libraryDependencies += "org.scala-lang.virtualized" % "scala-compiler" % "2.11.2"

scalacOptions += "-Xexperimental"

scalacOptions += "-Yvirtualize"

scalacOptions += "-language:higherKinds"

scalacOptions += "-language:implicitConversions"

scalacOptions += "-language:reflectiveCalls"
