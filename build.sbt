organization := "org.ucombinator"
name := "jade"
version := "0.1-SNAPSHOT"

scalaVersion := "2.12.4"

// For the plugin: "com.artima.supersafe" % "sbtplugin" % "1.1.3"
resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"

libraryDependencies ++= Seq(
  // TODO: Add more command line options and use scallop to parse
  "org.rogach" %% "scallop" % "3.1.1",

  // https://mvnrepository.com/artifact/org.ow2.asm/asm
  "org.ow2.asm" % "asm" % "6.0",

  // ScalaTest
  "org.scalactic" %% "scalactic" % "3.0.4",
  "org.scalatest" %% "scalatest" % "3.0.4" % Test,

  // https://mvnrepository.com/artifact/commons-io/commons-io
  "commons-io" % "commons-io" % "2.6",

  // https://mvnrepository.com/artifact/com.google.guava/guava
  "com.google.guava" % "guava" % "24.0-jre")

// Flags to 'scalac'.  Try to get as much error and warning detection as possible.
scalacOptions ++= Seq(
  // Emit warning and location for usages of deprecated APIs.
  "-deprecation",
  // Explain type errors in more detail.
  "-explaintypes",
  // Emit warning and location for usages of features that should be imported explicitly.
  "-feature",
  // Generates faster bytecode by applying optimisations to the program
  "-opt:l:inline",
  // Enable additional warnings where generated code depends on assumptions.
  "-unchecked",
  "-Xlint:_"
)

javacOptions in compile ++= Seq(
  // Turn on all warnings
  "-Xlint")
