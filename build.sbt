name := "JavaDecompiler"

version := "0.1"

scalaVersion := "2.12.4"

// TODO: Add more command line options and use scallop to parse
// libraryDependencies += "org.rogach" %% "scallop" % "3.1.1"
// For the plugin: "com.artima.supersafe" % "sbtplugin" % "1.1.3"
resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"

// https://mvnrepository.com/artifact/org.ow2.asm/asm
libraryDependencies += "org.ow2.asm" % "asm" % "6.0"

// ScalaTest
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.4"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % Test

// https://mvnrepository.com/artifact/commons-io/commons-io
libraryDependencies += "commons-io" % "commons-io" % "2.6"

// https://mvnrepository.com/artifact/com.google.guava/guava
libraryDependencies += "com.google.guava" % "guava" % "24.0-jre"
