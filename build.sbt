name := "Scala-Virtualized Tutorials"

version := "1.0"

scalaVersion := "2.10.0-virtualized-SNAPSHOT"

resolvers += ScalaToolsSnapshots 

scalaSource in Compile <<= baseDirectory(_ / "src") // default

// scalacOptions += "-Xexperimental" // needed?

//libraryDependencies += "org.scala-tools.testing" %% "scalacheck" % "1.8" % "test"

//mainClass in (Compile, run) := Some("myproject.MyMain")



