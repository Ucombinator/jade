organization := "org.ucombinator"
name := "jade"
version := "0.1-SNAPSHOT"

scalaVersion := "2.12.4"

// For the plugin: "com.artima.supersafe" % "sbtplugin" % "1.1.3"
resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"

libraryDependencies ++= Seq(
  "org.rogach" %% "scallop" % "3.1.1",
  "org.ow2.asm" % "asm" % "6.0",
  "org.ow2.asm" % "asm-commons" % "6.0",
  "org.ow2.asm" % "asm-tree" % "6.0",
  "org.ow2.asm" % "asm-analysis" % "6.0",
  "org.ow2.asm" % "asm-util" % "6.0",
  // "org.ow2.asm" % "asm-xml" % "6.0",
  "org.scalactic" %% "scalactic" % "3.0.5",
  "org.scalatest" %% "scalatest" % "3.0.5" % Test,
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.0",
  "commons-io" % "commons-io" % "2.6",
  "org.jgrapht" % "jgrapht-core" % "1.1.0",
  "org.jgrapht" % "jgrapht-ext" % "1.1.0",
//  "com.google.guava" % "guava" % "24.0-jre",

  // NOTE: 3.5.2 has self conflicts in its own dependencies
  "org.apache.maven" % "maven-core" % "3.5.2",
  "org.apache.maven" % "maven-compat" % "3.5.2",
//  "org.apache.maven.indexer" % "indexer-core" % "6.0.0",
  "org.apache.maven.indexer" % "indexer-core" % "6.0.0",
  "org.apache.maven.wagon" % "wagon-http-lightweight" % "2.12",
//  "org.eclipse.sisu" % "org.eclipse.sisu.plexus" % "0.3.3"

"org.apache.lucene" % "lucene-core" % "5.5.5",
  "org.apache.lucene" % "lucene-queryparser" % "5.5.5",
  "org.apache.lucene" % "lucene-analyzers-common" % "5.5.5",
  "org.apache.lucene" % "lucene-highlighter" % "5.5.5",
  "org.apache.lucene" % "lucene-backward-codecs" % "5.5.5"
)

// Deal with version conflicts in library dependencies
dependencyOverrides ++= Seq(
  "com.google.guava" % "guava" % "20.0",
  "org.codehaus.plexus" % "plexus-utils" % "3.1.0"
)

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
  "-Xlint:_")

javacOptions in compile ++= Seq(
  // Turn on all warnings
  "-Xlint")

assemblyOutputPath in assembly := new File("lib/jade/jade.jar")

assemblyShadeRules in assembly := Seq(
  // Conflicts with "javax.annotation" % "javax.annotation-api" % "1.2"
  ShadeRule.rename("javax.annotation.**" -> "javax.annotation.jsr250.@1").inLibrary("javax.annotation" % "jsr250-api" % "1.0")
)

// Create merge strategies that do not cause warnings
def quiet(mergeStragegy: sbtassembly.MergeStrategy): sbtassembly.MergeStrategy = new sbtassembly.MergeStrategy {
  val name = "quiet:" + mergeStragegy.name
  def apply(tempDir: File, path: String, files: Seq[File]): Either[String, Seq[(File, String)]] =
    mergeStragegy(tempDir, path, files)

  override def notifyThreshold = 1
  override def detailLogLevel = Level.Info
  override def summaryLogLevel = Level.Info
}

// MergeStrategy for `META-INF/plexus/components.xml` files
val componentsXmlMerge: sbtassembly.MergeStrategy = new sbtassembly.MergeStrategy {
  val name = "componentsXmlMerge"
  import scala.xml._

  def apply(tempDir: File, path: String, files: Seq[File]): Either[String, Seq[(File, String)]] = {
    val components: Seq[Node] =
      files
      .map(XML.loadFile)
      .flatMap(_ \\ "component-set" \ "components" \ "_")
      .flatMap(List(Text("\n    "), _)) ++ Seq(Text("\n  "))
    val componentSet = new Elem(null, "component-set", Null, TopScope, false,
      Text("\n  "),
      new Elem(null, "components", Null, TopScope, false,
        components: _*),
      Text("\n"))

    val file = MergeStrategy.createMergeTarget(tempDir, path)
    XML.save(file.toString, componentSet, enc = "UTF-8", xmlDecl = true)
    IO.append(file, IO.Newline.getBytes(IO.defaultCharset))
    Right(Seq(file -> path))
  }
}

lazy val quietDiscard = quiet(MergeStrategy.discard)
lazy val quietRename = quiet(MergeStrategy.rename)
lazy val quietFilterDistinctLines = quiet(MergeStrategy.filterDistinctLines)
lazy val quietComponentsXmlMerge = quiet(componentsXmlMerge)

assemblyMergeStrategy in assembly := {
  case PathList(file) if List(
    "library.properties", // from scala-library
    "rootdoc.txt", // from scala-library
    "reflect.properties", // from scala-reflect
    "module-info.class", // from asm-6.0
    "about.html" // from org.eclipse.sisu.plexus-0.3.3 and org.eclipse.sisu.inject-0.3.3
  ).contains(file) => quietDiscard

  case PathList("META-INF", file) if List(
    "MANIFEST.MF",
    "DEPENDENCIES",
    "LICENSE",
    "LICENSE.txt",
    "NOTICE",
    "NOTICE.txt"
  ).contains(file) => quietRename

  case PathList("META-INF", "services", xs @ _*) => quietFilterDistinctLines
  case PathList("META-INF", "plexus", "components.xml") => quietComponentsXmlMerge
  case PathList("META-INF", "sisu", "javax.inject.Named") => quietFilterDistinctLines

  case _ => MergeStrategy.deduplicate
}
