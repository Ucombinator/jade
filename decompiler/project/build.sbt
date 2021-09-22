scalaVersion := "2.12.15" // Most of our plugins do not support Scala 3 or Scala 2.13 so we use Scala 2.12

// format: off

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "1.1.0") // Adds `sbt assembly`
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.10.0") // Creates `org.ucombinator.jade.main.BuildInfo`
addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.6.0") // Adds `sbt dependencyUpdates`.
  // Copying the above line to `~/.sbt/1.0/plugins/sbt-updates.sbt`
  // also adds `sbt ";dependencyUpdates; reload plugins; dependencyUpdates; reload return"`
addSbtPlugin("com.typesafe.sbt" % "sbt-git" % "1.0.1") // Base `version` on git tags
addSbtPlugin("com.typesafe.sbt" % "sbt-license-report" % "1.2.0") // Adds `sbt dumpLicenseReport`
addSbtPlugin("io.github.davidgregory084" % "sbt-tpolecat" % "0.1.20") // Enables all warnings
addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.9.2") // Adds `sbt dependencyTree`
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.4.3") // Adds `sbt scalafmtSbt` and `sbt scalafmtAll`
  // Also adds `sbt 'scalafmtOnly <file>'`
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.9.0") // Adds `sbt clean coverage test coverageReport`

// format: on

libraryDependencies ++= Seq(
  // HTML parsing (for `Flags.table()`)
  "org.jsoup" % "jsoup" % "1.14.2",
)

// Flags to `scalac`.  Try to get as much error and warning detection as possible.
scalacOptions ++= Seq(
  //"-Xlint:_",      // Turn on all lint messages (`sbt-tpolecat` doesn't get all of them)
  // Currently disabled due to unused-import warnings
)
// Flags to `scalac` that are turned on by `sbt-tpolecat` but that we want off
scalacOptions --= Seq(
  "-Xfatal-warnings", // Fail the compilation if there are any warnings.
  "-Ywarn-unused:imports", // Warn if an import selector is not referenced.
)
