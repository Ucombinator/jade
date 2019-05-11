package org.ucombinator.jade.main

import org.rogach.scallop.ScallopConf
import org.ucombinator.jade.util.{JadeScallopConf, JadeSubcommand}

// TODO: consider some other command line parsing system
object Main extends App {
  val conf: Main = new Main(this.args)

  conf.subcommand match {
    case None => conf.errorMessageHandler("Missing subcommand")
    case Some(m: JadeSubcommand) => m.run()
    case Some(m) => conf.errorMessageHandler("Unknown subcommand: " + m)
  }
}

class Main(args: Seq[String]) extends ScallopConf(args = args) with JadeScallopConf {
  shortSubcommandsHelp(true)

  banner("Usage: jade [subcommand] [options]")
  addSubcommand(Decompile)
  addSubcommand(DownloadGrammar)
  addSubcommand(ExtractGrammar)
  addSubcommand(JavaFileTypes)
  addSubcommand(ExtractAsmTypes)
  addSubcommand(ExtractAsmOpcodes)
  verify()
}

object Decompile extends JadeSubcommand("decompile") {
  val fileName = trailArg[String]()

  override def run(): Unit = {
    decompile.Main.main(fileName())
  }
}

object DownloadGrammar extends JadeSubcommand("download-grammar") {
  val versionOrUrl = trailArg[String]()

  override def run(): Unit = {
    downloadGrammar.Main.main(versionOrUrl())
  }
}

object ExtractGrammar extends JadeSubcommand("extract-grammar") {
  override def run(): Unit = {
    extractGrammar.Main.main()
  }
}

object ExtractAsmTypes extends JadeSubcommand("extract-asm-types") {
  override def run(): Unit = {
    extractAsmTypes.Main.main()
  }
}

object ExtractAsmOpcodes extends JadeSubcommand("extract-asm-opcodes") {
  override def run(): Unit = {
    extractAsmOpcodes.Main.main()
  }
}

object JavaFileTypes extends JadeSubcommand("java-file-types") {
  override def run(): Unit = {
    javaFileTypes.Main.main()
  }
}
