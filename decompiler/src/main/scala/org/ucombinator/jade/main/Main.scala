package org.ucombinator.jade.main

import java.util.concurrent.Callable
import java.util.jar.Attributes

import picocli.CommandLine
import picocli.CommandLine.{Command, HelpCommand, Option, ParameterException, Parameters, ParentCommand}

import scala.collection.JavaConverters._

// TODO: java -cp lib/jade/jade.jar picocli.AutoComplete -n jade org.ucombinator.jade.main.Main (see https://picocli.info/autocomplete.html)
// TODO: description
// TODO: header/footer?

object Main {
  val commandLine: CommandLine = new CommandLine(new Main())
  def main(args: Array[String]): Unit = {
    System.exit(commandLine.execute(args:_*))
  }
}

// Class for common settings on commands
// aliases, description, defaultValueProvider
@Command(
  mixinStandardHelpOptions = true,
  requiredOptionMarker = '*',
  showDefaultValues = true,
  versionProvider = classOf[ManifestVersionProvider])
abstract class Cmd[T] extends Callable[T] {
  @ParentCommand var mainCommand: Main = _
}

@Command(
  name = "jade",
  subcommands = Array(
    classOf[HelpCommand],
    classOf[Decompile],
    classOf[JavaGrammarDownload],
    classOf[JavaGrammarExtract],
    classOf[JavaGrammarTypes],
    classOf[AsmInstructionConstants],
    classOf[AsmOpcodeConstants]))
class Main() extends Cmd[Unit] {
  override def call(): Unit = {
    throw new ParameterException(Main.commandLine, "Missing required parameter: [COMMAND]")
  }
}

@Command(name="decompile")
class Decompile extends Cmd[Unit] {
  @Option(names = Array("--print-asm"))
  var printAsm = false

  @Option(names = Array("--print-javaparser"))
  var printJavaparser = false

  @Option(names = Array("--print-methods"))
  var printMethods = false

  // TODO: java.io.file or java.nio.path?
  @Parameters(paramLabel = "<file>", description = Array("The .class file to decompile")) // TODO: FILE?
  var fileName: String = _

  override def call(): Unit = {
    decompile.Main.main(fileName, printAsm, printJavaparser, printMethods)
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

/** [[picocli.CommandLine.IVersionProvider]] implementation that returns version information from the jar file's `/META-INF/MANIFEST.MF` file.
  * Based on [[https://github.com/remkop/picocli/blob/master/picocli-examples/src/main/java/picocli/examples/VersionProviderDemo2.java]]
  */
class ManifestVersionProvider extends CommandLine.IVersionProvider {
  @throws[Exception]
  override def getVersion: Array[String] = {
    val manifestFile = "META-INF/MANIFEST.MF"
    val title = "Jade"
    val resources = classOf[Main].getClassLoader.getResources(manifestFile)
    for (url <- resources.asScala) {
      val manifest = new java.util.jar.Manifest(url.openStream)
      @inline def get(key: String) = manifest.getMainAttributes.get(new Attributes.Name(key))
      // TODO: include versions of packaged jars?
      if (title == get("Implementation-Title")) {
        return Array[String](f"${get("Implementation-Title")} version ${get("Implementation-Version")}")
      }
    }
    throw new Exception(f"Unable to find `$manifestFile` with title '$title' from which to read version")
  }
}
