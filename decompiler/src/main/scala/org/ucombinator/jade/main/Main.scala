package org.ucombinator.jade.main

import java.io.File
import java.util.concurrent.Callable

import picocli.CommandLine
import picocli.CommandLine.{Command, HelpCommand, Option, ParameterException, Parameters, ParentCommand}

import scala.collection.JavaConverters._

////////////////
// Classes for common settings on commands

@Command(
  mixinStandardHelpOptions = true,
  requiredOptionMarker = '*',
  showDefaultValues = true,
  versionProvider = classOf[ManifestVersionProvider])
abstract class Cmd[T] extends Callable[T] {
  @ParentCommand var mainCommand: Main = _
}

class ManifestVersionProvider extends CommandLine.IVersionProvider {
  override def getVersion: Array[String] = {
    import BuildInfo._
    Array[String](f"$name version $version (https://github.org/ucombinator/jade)")
  }
}

////////////////
// Top-level command

// TODO: java -cp lib/jade/jade.jar picocli.AutoComplete -n jade org.ucombinator.jade.main.Main (see https://picocli.info/autocomplete.html)
// TODO: description
// TODO: header/footer?

object Main {
  val commandLine: CommandLine = new CommandLine(new Main())
  def main(args: Array[String]): Unit = {
    System.exit(commandLine.execute(args:_*))
  }
}

// TODO: aliases, description, defaultValueProvider
@Command(
  name = "jade",
  subcommands = Array(
    classOf[HelpCommand],
    classOf[Decompile],
    classOf[JavaGrammarDownload],
    classOf[JavaGrammarExtract],
    classOf[JavaGrammarTypes],
    classOf[AsmInstructionConstants],
    classOf[AsmOpcodeConstants],
    classOf[BuildInfoCmd]))
class Main() extends Cmd[Unit] {
  override def call(): Unit = {
    throw new ParameterException(Main.commandLine, "Missing required parameter: [COMMAND]")
  }
}

////////////////
// Sub-commands

@Command(name="decompile")
class Decompile extends Cmd[Unit] {
  @Option(names = Array("--print-asm"))
  var printAsm = false

  @Option(names = Array("--print-javaparser"))
  var printJavaparser = false

  @Option(names = Array("--print-methods"))
  var printMethods = false

  // TODO: java.io.file or java.nio.path?
  @Parameters(paramLabel = "<file>", arity = "1..*", description = Array("The .class file to decompile")) // TODO: FILE?
  var fileNames: java.util.List[File] = _

  override def call(): Unit = {
    decompile.Main.main(printAsm, printJavaparser, printMethods, fileNames.asScala.toList)
  }
}

@Command(name="java-grammar:download")
class JavaGrammarDownload extends Cmd[Unit] {
  @Parameters(paramLabel = "<version or url>")
  var versionOrUrl: String = _

  override def call(): Unit = {
    downloadGrammar.Main.main(versionOrUrl)
  }
}

@Command(name="java-grammar:extract")
class JavaGrammarExtract extends Cmd[Unit] {
  override def call(): Unit = {
    extractGrammar.Main.main()
  }
}

@Command(name="java-grammar:types")
class JavaGrammarTypes extends Cmd[Unit] {
  override def call(): Unit = {
    javaFileTypes.Main.main()
  }
}

@Command(name="asm-constants:instruction-types")
class AsmInstructionConstants extends Cmd[Unit] {
  override def call(): Unit = {
    extractAsmTypes.Main.main()
  }
}

@Command(name="asm-constants:opcodes")
class AsmOpcodeConstants extends Cmd[Unit] {
  override def call(): Unit = {
    extractAsmOpcodes.Main.main()
  }
}

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
