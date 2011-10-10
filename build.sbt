name := "Scala-Virtualized Tutorials"

version := "1.0"

scalaVersion := "2.10.0-virtualized-SNAPSHOT"

resolvers += ScalaToolsSnapshots 

scalaSource in Compile <<= baseDirectory(_ / "src") // default

// scalacOptions += "-Xexperimental" // not needed

libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.10.0-virtualized-SNAPSHOT"

//mainClass in (Compile, run) := Some("myproject.MyMain")



