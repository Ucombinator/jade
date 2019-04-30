package org.ucombinator.jade.main

import org.rogach.scallop.ScallopConf
import org.ucombinator.jade.util.{JadeScallopConf, JadeSubcommand}

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
  addSubcommand(ExtractGrammar)
  addSubcommand(GenerateASTTypes)
  verify()
}

object Decompile extends JadeSubcommand("decompile") {
  val fileName = trailArg[String]()

  override def run(): Unit = {
    decompile.Main.main(fileName())
  }
}

object ExtractGrammar extends JadeSubcommand("extract-grammar") {
// TODO: file version url or stdin
  val version = trailArg[String]()
  val versions = Map(
    //"6" -> "https://docs.oracle.com/javase/specs/jls/se6/html/syntax.html",
    //"7" -> "https://docs.oracle.com/javase/specs/jls/se7/html/jls-18.html",
    "8" -> "https://docs.oracle.com/javase/specs/jls/se8/html/jls-19.html",
    "9" -> "https://docs.oracle.com/javase/specs/jls/se9/html/jls-19.html",
    "10" -> "https://docs.oracle.com/javase/specs/jls/se10/html/jls-19.html",
    "11" -> "https://docs.oracle.com/javase/specs/jls/se11/html/jls-19.html",
    "12" -> "https://docs.oracle.com/javase/specs/jls/se12/html/jls-19.html")

  override def run(): Unit = {
    extractGrammar.Main.main(versions.getOrElse(version(), version()))
  }
}

object GenerateASTTypes extends JadeSubcommand("generate-ast-types") {
  val fileName = trailArg[String]()

  override def run(): Unit = {
    generateASTTypes.Main.main(fileName())
  }
}
