organization := "org.ucombinator"
name := "Jade"

scalaVersion := "2.12.8"

libraryDependencies ++= Seq(
  // Abstract Syntax Trees for the Java language
  "com.github.javaparser" % "javaparser-core" % "3.14.3", // Main library
  "com.github.javaparser" % "javaparser-core-serialization" % "3.14.3", // Serialization to/from JSON
  "com.github.javaparser" % "javaparser-symbol-solver-core" % "3.14.3", // Resolving symbols and identifiers
  // Omitting the `javaparser-parent` package as it is just metadata
  // Omitting the JavaParser "generator" and "metamodel" packages as they are just for building JavaParser

  // Command-line argument parsing
  "info.picocli" % "picocli" % "4.0.0-alpha-3",

  // Vertex and edge graphs
  "org.jgrapht" % "jgrapht-core" % "1.3.0",
  "org.jgrapht" % "jgrapht-ext" % "1.3.0",
  //"org.jgrapht" % "jgrapht-guava" % "1.3.0",
  "org.jgrapht" % "jgrapht-io" % "1.3.0",
  "org.jgrapht" % "jgrapht-opt" % "1.3.0",

  // HTML parsing
  "org.jsoup" % "jsoup" % "1.12.1",

  // `.class` file parsing and analysis
  "org.ow2.asm" % "asm" % "7.1",
  "org.ow2.asm" % "asm-analysis" % "7.1",
  "org.ow2.asm" % "asm-commons" % "7.1",
  //"org.ow2.asm" % "asm-test" % "7.1",
  "org.ow2.asm" % "asm-tree" % "7.1",
  "org.ow2.asm" % "asm-util" % "7.1",

  // General parsing framework
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",

  // Testing framework for `src/test/`
  "org.scalatest" %% "scalatest" % "3.0.7" % Test,
)

filterScalaLibrary := false // include scala library in output of sbt-dependency-graph
dependencyAllowPreRelease := true // include pre-releases in dependency updates

// Setup `GitVersioning`
useJGit // make GitVersioning work even if `git` is not installed
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
  "-Xlint:_",      // Turn on all lint messages
)

// Flags to `javac`
javacOptions in compile ++= Seq(
  "-Xlint") // Turn on all warnings

// Setup `sbt-assembly`
assemblyOutputPath in assembly := new File("lib/jade/jade.jar")

// In theory, this should slim down the jar to only the parts referenced.
// However, it does not work due to the following two issues:
//  - https://github.com/sbt/sbt-assembly/issues/186
//  - https://github.com/sbt/sbt-assembly/issues/265
//assemblyShadeRules in assembly := Seq(
//  ShadeRule.keep("org.ucombinator.jade.main.**").inAll,
//  ShadeRule.keep("picocli.**").inAll)
//
// Alternatively, we can manually run the following command:
//  java -cp /home/adamsmd/.ivy2/cache/org.pantsbuild/jarjar/jars/jarjar-1.7.2.jar:lib/jade/jade.jar org.pantsbuild.jarjar.Main process <(echo 'keep org.ucombinator.jade.**') lib/jade/jade.jar tmp.jar
// Last time this was tested, it reduces size from 32MB to 5.4MB.

// An alternative to sbt-assembly that we are considering is sbt-onejar, however it is not up to date and does not work:
//  - http://one-jar.sourceforge.net/
//  - https://github.com/sbt/sbt-onejar

assemblyMergeStrategy in assembly := {
  case PathList(file) if List(
    "module-info.class",
  ).contains(file) => MergeStrategy.rename

  case PathList("META-INF", file) if List(
    "MANIFEST.MF",
    "NOTICE.txt"
  ).contains(file) => MergeStrategy.rename

  case _ => MergeStrategy.deduplicate
}
