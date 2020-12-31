addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.15.0") // Provides `sbt assembly`
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.10.0") // Generates `org.ucombinator.jade.main.BuildInfo`
addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.5.1") // Provides `sbt dependencyUpdates`. Copying this line to `~/.sbt/1.0/plugins/sbt-updates.sbt` also provides `sbt ";dependencyUpdates; reload plugins; dependencyUpdates; reload return"`
addSbtPlugin("com.typesafe.sbt" % "sbt-git" % "1.0.0") // Base `version` on git tags
addSbtPlugin("com.typesafe.sbt" % "sbt-license-report" % "1.2.0") // Provides `sbt dumpLicenseReport`
addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.9.2") // Provides `sbt dependencyTree`
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.6.1") // Provides `sbt clean coverage test coverageReport`

libraryDependencies ++= Seq(
  // HTML parsing (for `Flags.table()`)
  "org.jsoup" % "jsoup" % "1.13.1",
)
