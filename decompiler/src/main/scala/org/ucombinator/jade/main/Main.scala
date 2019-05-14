package org.ucombinator.jade.main

import java.io.IOException
import java.util.concurrent.Callable
import java.util.jar.Attributes

import picocli.CommandLine
import picocli.CommandLine.{Command, HelpCommand, ParameterException, Parameters, ParentCommand}

import scala.collection.JavaConverters._

object Main extends App {
  val commandLine: CommandLine = new CommandLine(new Main(commandLine))
  System.exit(commandLine.execute(this.args:_*))
}

// TODO: https://picocli.info/autocomplete.html
// TODO: description
// TODO: common super class instead of @ParentClass?
// TODO: header/footer?
@Command(
  name = "jade",
  mixinStandardHelpOptions = true,
  versionProvider = classOf[ManifestVersionProvider],
  subcommands = Array(
    classOf[HelpCommand],
    classOf[Decompile],
    classOf[JavaGrammarDownload],
    classOf[JavaGrammarExtract],
    classOf[JavaGrammarTypes],
    classOf[AsmInstructionConstants],
    classOf[AsmOpcodeConstants]))
class Main(commandLine: => CommandLine) extends Callable[Unit] {
  override def call(): Unit = {
    throw new ParameterException(this.commandLine, "Missing required parameter: [COMMAND]")
  }
}

@Command(name="decompile")
class Decompile extends Callable[Unit] {
  @ParentCommand var main: Main = _

  @Parameters(paramLabel = "<file>", description = Array("The .class file to decompile")) // TODO: FILE?
  var fileName: String = _

  override def call(): Unit = {
    decompile.Main.main(fileName)
  }
}

@Command(name="java-grammar-download")
class JavaGrammarDownload extends Callable[Unit] {
  @ParentCommand var main: Main = _

  @Parameters(paramLabel = "VERSION_OR_URL")
  var versionOrUrl: String = _

  override def call(): Unit = {
    downloadGrammar.Main.main(versionOrUrl)
  }
}

@Command(name="java-grammar-extract")
class JavaGrammarExtract extends Callable[Unit] {
  @ParentCommand var main: Main = _

  override def call(): Unit = {
    extractGrammar.Main.main()
  }
}

@Command(name="java-grammar-types")
class JavaGrammarTypes extends Callable[Unit] {
  @ParentCommand var main: Main = _

  override def call(): Unit = {
    javaFileTypes.Main.main()
  }
}

@Command(name="asm-instruction-constants")
class AsmInstructionConstants extends Callable[Unit] {
  @ParentCommand var main: Main = _

  override def call(): Unit = {
    extractAsmTypes.Main.main()
  }
}

@Command(name="asm-opcode-constants")
class AsmOpcodeConstants extends Callable[Unit] {
  @ParentCommand var main: Main = _

  override def call(): Unit = {
    extractAsmOpcodes.Main.main()
  }
}

/**
  * {@link IVersionProvider} implementation that returns version information from the jar file's {@code /META-INF/MANIFEST.MF} file.
  */
class ManifestVersionProvider extends CommandLine.IVersionProvider {
  @throws[Exception]
  override def getVersion: Array[String] = {
    val resources = classOf[Main].getClassLoader.getResources("META-INF/MANIFEST.MF")
    for (url <- resources.asScala) {
      try {
        val manifest = new java.util.jar.Manifest(url.openStream)
        if (isApplicableManifest(manifest)) {
          val attr = manifest.getMainAttributes
          return Array[String](f"${get(attr, "Implementation-Title")} version ${get(attr, "Implementation-Version")}")
        }
      } catch {
        case ex: IOException =>
          return Array[String]("Unable to read from " + url + ": " + ex)
      }
    }
    Array[String]()
  }

  private def isApplicableManifest(manifest: java.util.jar.Manifest) = {
    val attributes = manifest.getMainAttributes
    "Jade" == get(attributes, "Implementation-Title")
  }

  def get(attributes: Attributes, key: String) = attributes.get(new Attributes.Name(key))
}
