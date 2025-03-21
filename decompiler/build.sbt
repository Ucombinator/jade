organization := "org.ucombinator"
name := "Jade"
description := "Jade: The Java Decompiler"

scalaVersion := "3.0.2"

// TODO: subprojects: jade-lib and jade-cli

// TODO: improve compilation time

libraryDependencies ++= Seq(
  // format: off

  // NOTE: these are sorted alphabetically

  // Logging (see also com.typesafe.scalalogging)
  "ch.qos.logback" % "logback-classic" % "1.2.6", // Basic logging framework

  // Abstract Syntax Trees for the Java language
  "com.github.javaparser" % "javaparser-core" % "3.23.0", // Main library
  "com.github.javaparser" % "javaparser-core-serialization" % "3.23.0", // Serialization to/from JSON
  "com.github.javaparser" % "javaparser-symbol-solver-core" % "3.23.0", // Resolving symbols and identifiers
  // Omitting the JavaParser "parent" package as it is just metadata
  // Omitting the JavaParser "generator" and "metamodel" packages as they are just for building JavaParser

  // Logging (see also ch.qos.logback)
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.4", // Logging via macros so they run only when needed

  // Command-line argument parsing
  "info.picocli" % "picocli" % "4.6.1",
  "info.picocli" % "picocli-codegen" % "4.6.1",

  // Vertex and edge graphs
  "org.jgrapht" % "jgrapht-core" % "1.5.1",
  "org.jgrapht" % "jgrapht-ext" % "1.5.1",
  //"org.jgrapht" % "jgrapht-guava" % "1.5.1",
  "org.jgrapht" % "jgrapht-io" % "1.5.1",
  "org.jgrapht" % "jgrapht-opt" % "1.5.1",

  // `.class` file parsing and analysis
  "org.ow2.asm" % "asm" % "9.2",
  "org.ow2.asm" % "asm-analysis" % "9.2",
  "org.ow2.asm" % "asm-commons" % "9.2",
  //"org.ow2.asm" % "asm-test" % "9.2",
  "org.ow2.asm" % "asm-tree" % "9.2",
  "org.ow2.asm" % "asm-util" % "9.2",

  // Testing framework for `src/test/`
  "org.scalatest" %% "scalatest" % "3.2.10" % Test,

  // format: on
)

// Flags to `scalac`.  Try to get as much error and warning detection as possible.
scalacOptions ++= Seq(
  // "-help", // Show scalac help
  // "-Wconf:help", // Show scalac help for warning options
  "-opt:l:inline", // Generates faster bytecode by applying optimisations to the program
  "-Xlint:_", // Turn on all lint messages (`sbt-tpolecat` doesn't get all of them)
)

// Flags to `scalac` that are turned on by `sbt-tpolecat` but that we want off
scalacOptions --= Seq(
  "-Xfatal-warnings", // Fail the compilation if there are any warnings
)

// Flags to `javac`
compile / javacOptions  ++= Seq(
  "-Xlint", // Turn on all warnings
)

/* ----- Plugin Setup ----- */

// Setup sbt-dependency-graph
filterScalaLibrary := false // include scala library in output of sbt-dependency-graph
dependencyAllowPreRelease := true // include pre-releases in dependency updates

// Setup sbt-scalafmt
// Run `scalafmtCheckAll` and `scalafmtSbtCheck` when compiling
(Compile / compile) := (
  (Compile / compile)
    .dependsOn(
      scalafmtCheckAll.result, // .result ensures these are only warnings
      (Compile / scalafmtSbtCheck).result, // .result ensures these are only warnings
    )
  )
  .value

// Setup sbt-git
useJGit // make GitVersioning work even if `git` is not installed
enablePlugins(GitVersioning)
git.useGitDescribe := true
git.uncommittedSignifier := Some("dirty")

// Setup sbt-buildinfo
lazy val root = (project in file("."))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    buildInfoKeys := Seq[BuildInfoKey](
      name,
      version,
      scalaVersion,
      sbtVersion,
      libraryDependencies,
      BuildInfoKey.action("username") { System.getProperty("user.name") },
    ),
    buildInfoPackage := "org.ucombinator.jade.main",
    buildInfoOptions += BuildInfoOption.BuildTime,
  )

// Setup `sbt-assembly`
assembly / assemblyOutputPath := new File("lib/jade/jade.jar")

// In theory, this should slim down the jar to only the parts referenced.
// However, it does not work due to the following two issues:
//  - https://github.com/sbt/sbt-assembly/issues/186
//  - https://github.com/sbt/sbt-assembly/issues/265
//assembly / assemblyShadeRules := Seq(
//  ShadeRule.keep("org.ucombinator.jade.main.**").inAll,
//  ShadeRule.keep("picocli.**").inAll)
//
// Alternatively, we can manually run the following command:
//  java -cp /home/adamsmd/.ivy2/cache/org.pantsbuild/jarjar/jars/jarjar-1.7.2.jar:lib/jade/jade.jar org.pantsbuild.jarjar.Main process <(echo 'keep org.ucombinator.jade.**') lib/jade/jade.jar tmp.jar
// Last time this was tested, it reduces size from 32MB to 5.4MB.

// An alternative to sbt-assembly that we are considering is sbt-onejar, however it is not up to date and does not work:
//  - http://one-jar.sourceforge.net/
//  - https://github.com/sbt/sbt-onejar

assembly / assemblyMergeStrategy := {
  case PathList(file)
      if List(
        "module-info.class",
      ).contains(file) =>
    MergeStrategy.rename

  case PathList("META-INF", file)
      if List(
        "LICENSE.txt",
        "MANIFEST.MF",
        "NOTICE.txt",
      ).contains(file) =>
    MergeStrategy.rename

  case _ =>
    MergeStrategy.deduplicate
}

/* ----- Custom SBT Code ----- */

// Code generation
import complete.DefaultParsers._

val flagsTableFile = settingKey[File]("Location of the flags table")
flagsTableFile := (Compile / scalaSource).value / "org" / "ucombinator" / "jade" / "classfile" / "Flags.txt"

val flagsSourceFile = settingKey[File]("Location of the generated `Flags.scala` file")
flagsSourceFile := (Compile / sourceManaged).value / "org" / "ucombinator" / "jade" / "classfile" / "Flags.scala"

val javaSpecParser =
  Space ~>
    token((literal("jls") | literal("jvms")) <~ "-") ~
    token(NatBasic <~ "-") ~
    token(NatBasic <~ Space) ~
    fileParser(new java.io.File("."))
val javaSpec = inputKey[File](
  """javaSpec jls-<version>-<chapter> <file>
    |
    |        Download a chapter of the Java Language Specification
    |
    |        Arguments:

    |            <version>   The version number of the specification to download.
    |            <chapter>   The chapter number of the specification to download
    |            <file>      The file in which to save the specification.
    |
    |        Example:
    |
    |            javaSpec jls-12-1 jls-1.html
    |
    |javaSpec jvms-<version>-<chapter> <file>
    |
    |        Download a chapter of the Java Virtual Machine Specification
    |
    |        Arguments:

    |            <version>   The version number of the specification to download.
    |            <chapter>   The chapter number of the specification to download
    |            <file>      The file in which to save the specification.
    |
    |        Example:
    |
    |            javaSpec jvms-12-4 jvms-4.html""".stripMargin
)
javaSpec := {
  val (((spec, version), chapter), file) = javaSpecParser.parsed
  IO.write(file, FlagsGen.javaSpec(spec, version, chapter))
  file
}

val flagsTableParser = // genMod -f in-file out-file ; genMod -f in-file ; genMod -v 9 out-file ; genMod -v 9
  Space ~>
    ((token("-f") ~ Space ~> fileParser(new java.io.File(".")).map(Left(_))) |
      (token("-v") ~ Space ~> token(NatBasic.map(Right(_))))) ~
    (Space ~> fileParser(new java.io.File("."))).?
val flagsTable = inputKey[File](
  """flagsTable -f <jvmsFile> <file>
    |
    |        Generate a flags table based on a copy of the Java Virtual Machine Specification.
    |
    |        Arguments:
    |
    |            <jvmsFile>  The file containing a copy of the specification.
    |            <file>      The file in which to save the flags table.
    |                        Defaults to the flagsTableFile setting if omitted.
    |
    |        Examples:
    |
    |            flagsTable jvms-4.html Flags.txt
    |            flagsTable jvms-4.html
    |
    |flagsTable -v <version> <file>
    |
    |        Generate a flags table based on an online version of the Java Virtual Machine Specification.
    |
    |        Arguments:
    |
    |            <version>   The version number of the Java Virtual Machine Specification
    |            <file>      The file in which to save the flags table.
    |                        Defaults to the flagsTableFile setting if omitted.
    |
    |        Examples:
    |
    |            flagsTable -v 12 Flags.txt
    |            flagsTable -v 12""".stripMargin
)
flagsTable := {
  val (src, dst) = flagsTableParser.parsed
  val html = src match {
    case Left(file) => IO.read(file)
    case Right(version) => FlagsGen.javaSpec("jvms", version, 4)
  }
  val tableString = FlagsGen.table(html)
  val tableFile = dst.getOrElse(flagsTableFile.value)
  IO.write(tableFile, tableString)
  tableFile
}

// TODO: sbt.Tracked.{ inputChanged, outputChanged } etc
Compile / sourceGenerators += Def.task {
  import org.scalafmt.sbt.ScalafmtSbtReporter

  val streamsValue = streams.value
  val sourceFile = flagsSourceFile.value
  val flagsCode = FlagsGen.code(IO.read(flagsTableFile.value))
  val scalafmt = org.scalafmt.interfaces.Scalafmt
    .create(this.getClass.getClassLoader)
    .withReporter(new ScalafmtSbtReporter(streamsValue.log, new java.io.OutputStreamWriter(streamsValue.binary()), true));
  if (flagsCode != scalafmt.format(scalafmtConfig.value.toPath(), sourceFile.toPath(), flagsCode)) {
    streamsValue.log.warn(f"\nGenerated file isn't formatted properly: ${sourceFile}\n\n")
  }
  IO.write(sourceFile, flagsCode)
  Seq(sourceFile)
}.taskValue
