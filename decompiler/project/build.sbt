scalaVersion := "2.12.12" // Most of our plugins do not support Scala 2.13 so we use Scala 2.12

// format: off
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.15.0") // Adds `sbt assembly`
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.10.0") // Creates `org.ucombinator.jade.main.BuildInfo`
addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.5.1") // Adds `sbt dependencyUpdates`.
  // Copying the above line to `~/.sbt/1.0/plugins/sbt-updates.sbt`
  // also adds `sbt ";dependencyUpdates; reload plugins; dependencyUpdates; reload return"`
addSbtPlugin("com.typesafe.sbt" % "sbt-git" % "1.0.0") // Base `version` on git tags
addSbtPlugin("com.typesafe.sbt" % "sbt-license-report" % "1.2.0") // Adds `sbt dumpLicenseReport`
addSbtPlugin("io.github.davidgregory084" % "sbt-tpolecat" % "0.1.16") // Enables all warnings
addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.9.2") // Adds `sbt dependencyTree`
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.4.2") // Adds `sbt scalafmtSbt` and `sbt scalafmtAll`
  // Also adds `sbt 'scalafmtOnly <file>'`
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.6.1") // Adds `sbt clean coverage test coverageReport`
// format: on

// scalafmt: { trailingCommas = preserve }
libraryDependencies ++= Seq(
  // HTML parsing (for `Flags.table()`)
  "org.jsoup" % "jsoup" % "1.13.1",
)
