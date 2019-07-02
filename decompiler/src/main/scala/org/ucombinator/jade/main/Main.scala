package org.ucombinator.jade.main

import java.nio.file.Path
import java.util.concurrent.Callable

import ch.qos.logback.classic.Level
import org.ucombinator.jade.util.Logging
import picocli.CommandLine
import picocli.CommandLine.{Command, HelpCommand, ITypeConverter, Option, ParameterException, Parameters, ParentCommand}

import scala.collection.JavaConverters._

// TODO: analysis to ensure using only the canonical constructor (helps with detecting forward version changes) (as a compiler plugin?)

////////////////
// Top-level command

// TODO: description
// TODO: header/footer?
// TODO: aliases, description, defaultValueProvider

object Main {
  val commandLine: CommandLine = new CommandLine(new Main())
  def main(args: Array[String]): Unit = {
    System.exit(commandLine.execute(args:_*))
  }
}

@Command(
  name = "jade",
  subcommands = Array(
    classOf[HelpCommand],
    classOf[BuildInfoCmd],
    classOf[DecompileCmd],
    classOf[DownloadJlsCmd],
    classOf[DownloadJvmsCmd],
    classOf[GenerateModifierCodeCmd],
    classOf[GenerateInsnTypesCmd]))
class Main() extends Cmd[Unit] {
  override def run(): Unit = {
    throw new ParameterException(Main.commandLine, "Missing required parameter: [COMMAND]")
  }
}

////////////////
// Classes for common settings on commands

@Command(
  mixinStandardHelpOptions = true,
  requiredOptionMarker = '*', // TODO: put in documentation string
  showDefaultValues = true,
  versionProvider = classOf[VersionProvider])
abstract class Cmd[T] extends Callable[T] {
  @ParentCommand var mainCommand: Main = _

  @Option(names = Array("--log"), paramLabel = "LEVEL", description = Array("Set the logging level"), split=",", converter = Array(classOf[LevelConverter]))
  var log = new java.util.LinkedList[(String,Level)]()

  @Option(names = Array("--log-caller-depth"), paramLabel = "DEPTH", description = Array("Number of callers to print after log messages"))
  var logCallerDepth: Int = 0

  // TODO: flag for pause on startup
  // TODO: command to list all loggers

  final override def call(): T = {
    Logging.callerDepth = logCallerDepth
    for ((k, v) <- log.asScala) {
      // TODO: Logging.init(k)
      // TODO: check for logger typos (parent class must exist, and if init, then child must exist) (after run?)
      Logging.logger(k).setLevel(v)
    }
    run()
  }
  def run(): T
}

class VersionProvider extends CommandLine.IVersionProvider {
  override def getVersion: Array[String] = {
    import org.ucombinator.jade.main.BuildInfo._
    Array[String](f"$name version $version (https://github.org/ucombinator/jade)")
  }
}

class LevelConverter extends ITypeConverter[(String, Level)] {
  override def convert(value: String): (String, Level) = {
    val (name, level) = value.split("=") match {
      case Array(l) => ("", Level.toLevel(l, null))
      case Array(n, l) => (n, Level.toLevel(l, null))
      case _ => throw new Exception("could not parse log level")
    }
    if (level == null) throw new Exception(f"invalid level: $level")
    (name, level)
  }
}

////////////////
// Sub-commands

@Command(
  name = "build-info",
  description = Array("Display information about how `jade` was built"))
class BuildInfoCmd extends Cmd[Unit] {
  override def run(): Unit = {
    import org.ucombinator.jade.main.BuildInfo._
    println(f"Build tools: Scala $scalaVersion, SBT $sbtVersion")
    println(f"Build time: $builtAtString UTC")
    println(f"Build user: $username")
    println(f"Libraries:")
    for (l <- libraryDependencies.sorted) {
      println("  " + l)
    }
  }
}

@Command(
  name = "decompile",
  description = Array("Decompile class, jar, or jmod files"))
class DecompileCmd extends Cmd[Unit] {
  // --include-file --exclude-file --include-class --exclude-class
  // --include-cxt-file --include-cxt-class

  @Parameters(paramLabel = "<path>", arity = "1..*", description = Array("Files or directories to decompile"))
  var path: java.util.List[Path] = _

  override def run(): Unit = {
    Decompile.main(path.asScala.toList)
  }
}

@Command(
  name = "download-jls",
  description = Array("Download the Java Langauge Specification"))
class DownloadJlsCmd extends Cmd[Unit] {
  @Parameters(paramLabel = "<version>", index = "0")
  var version: Int = _

  @Parameters(paramLabel = "<chapter>", index = "1")
  var chapter: Int = _

  override def run(): Unit = {
    DownloadSpecification.main("jls", version, chapter)
  }
}

@Command(
  name = "download-jvms",
  description = Array("Download the Java Virtual Machine Specification"))
class DownloadJvmsCmd extends Cmd[Unit] {
  @Parameters(paramLabel = "<version>", index = "0")
  var version: Int = _

  @Parameters(paramLabel = "<chapter>", index = "1")
  var chapter: Int = _

  override def run(): Unit = {
    DownloadSpecification.main("jvms", version, chapter)
  }
}

@Command(
  name = "generate-modifier-code",
  description = Array("Generate the code for `Modifier.scala`"))
class GenerateModifierCodeCmd extends Cmd[Unit] {
  override def run(): Unit = {
    GenerateModifierCode.main()
  }
}

@Command(
  name = "generate-insn-types",
  description = Array("Generate the code for `InsnTypes.scala`"))
class GenerateInsnTypesCmd extends Cmd[Unit] {
  override def run(): Unit = {
    GenerateInsnTypes.main()
  }
}
