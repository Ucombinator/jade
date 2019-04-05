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
  addSubcommand(GenerateASTTypes)
  verify()
}

object Decompile extends JadeSubcommand("decompile") {
  val fileName = trailArg[String]()

  override def run(): Unit = {
    decompile.Main.main(fileName())
  }
}

object GenerateASTTypes extends JadeSubcommand("generate-ast-types") {
  val fileName = trailArg[String]()
  //   val inputStr = trailArg[String]()
  //   //val jarFile = trailArg[String]()
  //  //  val destinationFolder = trailArg[String]()

  override def run(): Unit = {
    generateASTTypes.Main.main(fileName())
    //generateASTTypes.Main.main()
    // TODO: accept a directory that includes .class files
  }
}
