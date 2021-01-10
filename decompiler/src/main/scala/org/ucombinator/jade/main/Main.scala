package org.ucombinator.jade.main

import java.nio.file.Path
import java.util.concurrent.Callable

import ch.qos.logback.classic.Level
import org.ucombinator.jade.decompile.Decompile
import org.ucombinator.jade.util.Logging
import picocli.AutoComplete.GenerateCompletion
import picocli.codegen.docgen.manpage.ManPageGenerator
import picocli.CommandLine
import picocli.CommandLine.{Command, HelpCommand, ITypeConverter, Option, ParameterException, Parameters, ParentCommand}

import scala.jdk.CollectionConverters._

//  TODO: analysis to ensure using only the canonical constructor (helps with detecting forward version changes) (as a compiler plugin?)

////////////////
// Top-level command

// TODO: description
// TODO: header/footer?
// TODO: aliases, description, defaultValueProvider

// TODO: java -cp lib/jade/jade.jar picocli.AutoComplete -n jade org.ucombinator.jade.main.Main (see https://picocli.info/autocomplete.html)
object Main {
  val commandLine: CommandLine = new CommandLine(new Main())
  commandLine.setAbbreviatedOptionsAllowed(true)
  commandLine.setAbbreviatedSubcommandsAllowed(true)
  commandLine.setOverwrittenOptionsAllowed(true)
  def main(args: Array[String]): Unit = {
    System.exit(commandLine.execute(args: _*))
  }
  def versionString: String = {
    import org.ucombinator.jade.main.BuildInfo._
    f"${name} version ${version} (https://github.org/ucombinator/jade)"
  }
}

@Command(
  name = "jade",
  subcommands = Array(
    classOf[HelpCommand],
    classOf[DecompileCmd],
    classOf[CompileCmd],
    classOf[DiffCmd],
    classOf[BuildInfoCmd],
    classOf[Loggers],
    classOf[ManPageGenerator],
    classOf[GenerateCompletion],
  )
)
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
  showAtFileInUsageHelp = true,
  showDefaultValues = true,
  showEndOfOptionsDelimiterInUsageHelp = true,
  versionProvider = classOf[VersionProvider],
)
abstract class Cmd[T] extends Callable[T] {
  @ParentCommand var mainCommand: Main = _

  @Option(
    names = Array("--log"),
    paramLabel = "LEVEL",
    description =
      Array("Set the logging level", "Logger names are relative to `org.ucombinator.jade` unless prefixed with `.`."),
    split = ",",
    //converter = Array(classOf[LevelConverter]),
  )
  // TODO: check --help
  // TODO: explain "LOGGER=LEVEL"
  var log: java.util.Map[String, Level] = new java.util.HashMap[String, Level]()

  @Option(
    names = Array("--log-caller-depth"),
    paramLabel = "DEPTH",
    description = Array("Number of callers to print after log messages"),
  )
  var logCallerDepth = 0

  @Option(
    names = Array("--wait"),
    negatable = true,
    description = Array("Wait for input from user before running (useful when attaching to the process)"),
  )
  var waitForUser = false

  // TODO: exit code list

  def run(): T

  final override def call(): T = {
    Logging.callerDepth = logCallerDepth

    for ((k, v) <- log.asScala) {
      // TODO: warn if logger exists
      val name =
        if (k.startsWith(".")) { k.substring(1) }
        else if (k == "") { "" }
        else { Logging.prefix + k }
      Logging.getLogger(name).setLevel(v)
    }

    if (waitForUser) {
      Console.out.println("Waiting for user.  Press \"Enter\" to continue.")
      Console.in.readLine()
    }

    val result = run()
    result
  }
}

class VersionProvider extends CommandLine.IVersionProvider {
  override def getVersion: Array[String] = { Array[String](Main.versionString) }
}

class LevelConverter extends ITypeConverter[(String, Level)] {
  override def convert(value: String): (String, Level) = {
    val (name, level) = value.split("=") match {
      case Array(l) => ("", Level.toLevel(l, null))
      case Array(n, l) => (n, Level.toLevel(l, null))
      case _ => throw new Exception("could not parse log level") // TODO: explain notation
    }
    if (level == null) throw new Exception(f"invalid level: ${level}") // TODO: "must be one of ..."
    (name, level)
  }
}

////////////////
// Sub-commands

@Command(name = "build-info", description = Array("Display information about how `jade` was built"))
class BuildInfoCmd extends Cmd[Unit] {
  override def run(): Unit = {
    import org.ucombinator.jade.main.BuildInfo._
    println(f"""${Main.versionString}
               |Build tools: Scala ${scalaVersion}, SBT ${sbtVersion}
               |Build time: ${builtAtString} ${builtAtMillis}ms
               |Libraries:""".stripMargin)
    for (l <- libraryDependencies.sorted) {
      println("  " + l)
    }
  }
}

@Command(name = "decompile", description = Array("Decompile class, jar, or jmod files"))
class DecompileCmd extends Cmd[Unit] {
  // --include-file --exclude-file --include-class --exclude-class
  // --include-cxt-file --include-cxt-class

  @Parameters(paramLabel = "<path>", arity = "1..*", description = Array("Files or directories to decompile"))
  var path: java.util.List[Path] = _

  override def run(): Unit = {
    Decompile.main(path.asScala.toList)
  }
}

@Command(name = "compile", description = Array("Compile a java file"))
class CompileCmd extends Cmd[Unit] {
  override def run(): Unit = {
    // TODO: Use a JavaAgent of a nested compiler to test whether the code compiles
    // TODO: Test whether it compiles under different Java versions
    // TODO: Back-off if compilation fails
    ??? // TODO: implement compile
  }
}

@Command(name = "diff", description = Array("Compare class files"))
class DiffCmd extends Cmd[Unit] { // TODO: exit codes
  override def run(): Unit = {
    ??? // TODO: implement diff
  }
}

@Command(name = "loggers", description = Array("Lists available loggers"))
class Loggers extends Cmd[Unit] {
  override def run(): Unit = { Logging.listLoggers() }
}
