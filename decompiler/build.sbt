organization := "org.ucombinator"
name := "Jade"

scalaVersion := "2.12.8"

libraryDependencies ++= Seq(
  // Abstract Syntax Trees for the Java language
  "com.github.javaparser" % "javaparser-core" % "3.14.1", // Main library
  "com.github.javaparser" % "javaparser-core-serialization" % "3.14.1", // Serialization to/from JSON
  "com.github.javaparser" % "javaparser-symbol-solver-core" % "3.14.1", // Resolving symbols and identifiers
  // Omitting the `javaparser-parent` package as it is just metadata
  // Omitting the "generator" and "metamodel" packages as they are just for building `javaparser`

  // Command-line argument parsing
  "info.picocli" % "picocli" % "4.0.0-alpha-3",

  // Vertex and edge graphs
  "org.jgrapht" % "jgrapht-core" % "1.3.0",
  "org.jgrapht" % "jgrapht-ext" % "1.3.0",
  //"org.jgrapht" % "jgrapht-guava" % "1.3.0",
  "org.jgrapht" % "jgrapht-io" % "1.3.0",
  "org.jgrapht" % "jgrapht-opt" % "1.3.0",

  // HTML parsing
  "org.jsoup" % "jsoup" % "1.11.3",

  // `.class` file parsing and analysis
  "org.ow2.asm" % "asm" % "7.0",
  "org.ow2.asm" % "asm-analysis" % "7.0",
  "org.ow2.asm" % "asm-commons" % "7.0",
  //"org.ow2.asm" % "asm-test" % "7.0",
  "org.ow2.asm" % "asm-tree" % "7.0",
  "org.ow2.asm" % "asm-util" % "7.0",

  // General parsing framework
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1",

  // Testing framework for `src/test/`
  "org.scalatest" %% "scalatest" % "3.0.5" % Test,
)

filterScalaLibrary := false // include scala library in output of sbt-dependency-graph
dependencyAllowPreRelease := true // include pre-releases in dependency updates

// Setup GitVersioning
useJGit // make things work even if `git` is not installed
enablePlugins(GitVersioning)
git.useGitDescribe := true
git.uncommittedSignifier := Some("dirty")

lazy val root = (project in file(".")).
  enablePlugins(BuildInfoPlugin).
  settings(
    buildInfoKeys := Seq[BuildInfoKey](
      name, version, scalaVersion, sbtVersion, libraryDependencies,
      BuildInfoKey.action("username") { System.getProperty("user.name") }),
    buildInfoPackage := "org.ucombinator.jade.main",
    buildInfoOptions += BuildInfoOption.BuildTime)

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

//assemblyShadeRules in assembly := Seq(
//  ShadeRule.keep("org.ucombinator.jade.main.**").inAll,
//  ShadeRule.keep("picocli.**").inAll)
//  See issues:
//    https://github.com/sbt/sbt-assembly/issues/186
//    https://github.com/sbt/sbt-assembly/issues/265
// java -cp /home/adamsmd/.ivy2/cache/org.pantsbuild/jarjar/jars/jarjar-1.7.2.jar:lib/jade/jade.jar org.pantsbuild.jarjar.Main process <(echo 'keep org.ucombinator.jade.**') lib/jade/jade.jar tmp.jar
//   Reduces size from 32MB to 5.4MB
// GraalVM
// ScalaNative
// ScalaMeter

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
