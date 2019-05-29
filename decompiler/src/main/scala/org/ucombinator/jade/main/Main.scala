package org.ucombinator.jade.main

import picocli.CommandLine
import picocli.CommandLine.{Command, HelpCommand, Option, ParameterException, Parameters, ParentCommand}

import java.io.File
import java.util.concurrent.Callable

import scala.collection.JavaConverters._

////////////////
// Top-level command

// TODO: java -cp lib/jade/jade.jar picocli.AutoComplete -n jade org.ucombinator.jade.main.Main (see https://picocli.info/autocomplete.html)
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
    classOf[Decompile],
    classOf[DownloadJls],
    classOf[DownloadJvms],
    classOf[GenerateModifierCode],
    classOf[GenerateAsmInstructionTypes]))
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

@Command(name="build-info")
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

@Command(name="decompile")
class Decompile extends Cmd[Unit] {
  @Option(names = Array("--print-asm"))
  var printAsm = false

  @Option(names = Array("--print-javaparser"))
  var printJavaParser = false

  @Option(names = Array("--print-methods"))
  var printMethods = false

  // TODO: java.io.file or java.nio.path?
  @Parameters(paramLabel = "<file>", arity = "1..*", description = Array("The .class file to decompile")) // TODO: FILE?
  var fileNames: java.util.List[File] = _

  override def call(): Unit = {
    decompile.Main.main(printAsm, printJavaParser, printMethods, fileNames.asScala.toList)
  }
}

@Command(name="download-jls")
class DownloadJls extends Cmd[Unit] {
  @Parameters(paramLabel = "<version>", index = "0")
  var version: Int = _

  @Parameters(paramLabel = "<chapter>", index = "1")
  var chapter: Int = _

  override def call(): Unit = {
    downloadSpecification.Main.main("jls", version, chapter)
  }
}

@Command(name="download-jvms")
class DownloadJvms extends Cmd[Unit] {
  @Parameters(paramLabel = "<version>", index = "0")
  var version: Int = _

  @Parameters(paramLabel = "<chapter>", index = "1")
  var chapter: Int = _

  override def call(): Unit = {
    downloadSpecification.Main.main("jvms", version, chapter)
  }
}

@Command(name="generate-modifier-code")
class GenerateModifierCode extends Cmd[Unit] {
  override def call(): Unit = {
    generateModifierCode.Main.main()
  }
}

@Command(name="generate-asm-instruction-types")
class GenerateAsmInstructionTypes extends Cmd[Unit] {
  override def call(): Unit = {
    generateAsmInstructionTypes.Main.main()
  }
}
