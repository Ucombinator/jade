package org.ucombinator.jade.main

import org.rogach.scallop.ScallopConf

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
  addSubcommand(DecompileClass)
  addSubcommand(GenerateASTTypes)
  verify()
}

object DecompileClass extends JadeSubcommand("decompile-class") {
  val className = trailArg[String]()

  override def run(): Unit = {
    org.ucombinator.jade.main.decompileOneClass.Main.main(className())
  }
}

object GenerateASTTypes extends JadeSubcommand("generate-ast-types") {
//  val jarFile = trailArg[String]()
//  val destinationFolder = trailArg[String]()

  override def run(): Unit = {
    org.ucombinator.jade.main.generateASTTypes.Main.main()
    // TODO: accept a directory that includes .class files
  }
}
