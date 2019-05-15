organization := "org.ucombinator"
name := "Jade"
version := "0.1-SNAPSHOT"

scalaVersion := "2.12.8"

libraryDependencies ++= Seq(
  "info.picocli" % "picocli" % "4.0.0-alpha-3", // Command-line argument parsing
  "org.jgrapht" % "jgrapht-core" % "1.3.0", // Vertex and edge graphs
  "org.jgrapht" % "jgrapht-ext" % "1.3.0", // (See `jgrapht-core`)
  //"org.jgrapht" % "jgrapht-guava" % "1.3.0", // (See `jgrapht-core`)
  "org.jgrapht" % "jgrapht-io" % "1.3.0", // (See `jgrapht-core`)
  "org.jgrapht" % "jgrapht-opt" % "1.3.0", // (See `jgrapht-core`)
  "org.jsoup" % "jsoup" % "1.11.3", // HTML parsing
  "org.ow2.asm" % "asm" % "7.0", // `.class` file parsing
  "org.ow2.asm" % "asm-analysis" % "7.0", // (See `asm`)
  "org.ow2.asm" % "asm-commons" % "7.0", // (See `asm`)
  //"org.ow2.asm" % "asm-test" % "7.0", // (See `asm`)
  "org.ow2.asm" % "asm-tree" % "7.0", // (See `asm`)
  "org.ow2.asm" % "asm-util" % "7.0", // (See `asm`)
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1", // General parsing framework
  "org.scalatest" %% "scalatest" % "3.0.5" % Test, // Testing framework for `src/test/`
)

filterScalaLibrary := false // include scala library in output of sbt-dependency-graph
dependencyAllowPreRelease := true // include pre-releases in dependency updates

// Flags to `scalac`.  Try to get as much error and warning detection as possible.
scalacOptions ++= Seq(
  "-deprecation",  // Emit warning and location for usages of deprecated APIs.
  "-explaintypes", // Explain type errors in more detail.
  "-feature",      // Emit warning and location for usages of features that should be imported explicitly.
  "-opt:l:inline", // Generates faster bytecode by applying optimisations to the program
  "-unchecked",    // Enable additional warnings where generated code depends on assumptions.
  "-Xlint:_")      // Turn on all lint messages

// Flags to `javac`
javacOptions in compile ++= Seq(
  "-Xlint") // Turn on all warnings

assemblyOutputPath in assembly := new File("lib/jade/jade.jar")

// Create merge strategies that do not cause warnings
def quiet(mergeStragegy: sbtassembly.MergeStrategy): sbtassembly.MergeStrategy = new sbtassembly.MergeStrategy {
  val name = "quiet:" + mergeStragegy.name
  def apply(tempDir: File, path: String, files: Seq[File]): Either[String, Seq[(File, String)]] =
    mergeStragegy(tempDir, path, files)

  override def notifyThreshold = 1
  override def detailLogLevel = Level.Info
  override def summaryLogLevel = Level.Info
}

lazy val quietDiscard = quiet(MergeStrategy.discard)
lazy val quietRename = quiet(MergeStrategy.rename)
lazy val quietFilterDistinctLines = quiet(MergeStrategy.filterDistinctLines)

assemblyMergeStrategy in assembly := {
  case PathList(file) if List(
    "module-info.class", // from asm-7.0
  ).contains(file) => quietDiscard

  case PathList("META-INF", file) if List(
    "MANIFEST.MF",
    "NOTICE.txt"
  ).contains(file) => quietRename

  case _ => MergeStrategy.deduplicate
}
