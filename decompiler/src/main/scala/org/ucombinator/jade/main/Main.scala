package org.ucombinator.jade.main

import picocli.CommandLine
import picocli.CommandLine.{Command, HelpCommand, Option, ParameterException, Parameters, ParentCommand}
import java.nio.file.Path
import java.util.concurrent.Callable

import scala.collection.JavaConverters._

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
    classOf[GenerateAsmInstructionTypesCmd]))
class Main() extends Cmd[Unit] {
  override def call(): Unit = {
    throw new ParameterException(Main.commandLine, "Missing required parameter: [COMMAND]")
  }
}

////////////////
// Classes for common settings on commands

@Command(
  mixinStandardHelpOptions = true,
  requiredOptionMarker = '*',
  showDefaultValues = true,
  versionProvider = classOf[VersionProvider])
abstract class Cmd[T] extends Callable[T] {
  @ParentCommand var mainCommand: Main = _
}

class VersionProvider extends CommandLine.IVersionProvider {
  override def getVersion: Array[String] = {
    import org.ucombinator.jade.main.BuildInfo._
    Array[String](f"$name version $version (https://github.org/ucombinator/jade)")
  }
}

////////////////
// Sub-commands

@Command(
  name = "build-info",
  description = Array("Display information about how `jade` was built"))
class BuildInfoCmd extends Cmd[Unit] {
  override def call(): Unit = {
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
  @Option(names = Array("--print-asm"))
  var printAsm = false

  @Option(names = Array("--print-javaparser"))
  var printJavaParser = false

  @Option(names = Array("--print-methods"))
  var printMethods = false

  @Parameters(paramLabel = "<path>", arity = "1..*", description = Array("Files or directories to decompile"))
  var path: java.util.List[Path] = _

  override def call(): Unit = {
    Decompile(printAsm, printJavaParser, printMethods).main(path.asScala.toList)
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

  override def call(): Unit = {
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

  override def call(): Unit = {
    DownloadSpecification.main("jvms", version, chapter)
  }
}

@Command(
  name = "generate-modifier-code",
  description = Array("Generate the code for `Modifier.scala`"))
class GenerateModifierCodeCmd extends Cmd[Unit] {
  override def call(): Unit = {
    GenerateModifierCode.main()
  }
}

@Command(
  name = "generate-asm-instruction-types",
  description = Array("Generate the code for `InstructionTypes.scala`"))
class GenerateAsmInstructionTypesCmd extends Cmd[Unit] {
  override def call(): Unit = {
    GenerateAsmInstructionTypes.main()
  }
}
