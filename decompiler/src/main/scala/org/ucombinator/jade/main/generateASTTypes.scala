package org.ucombinator.jade.main.generateASTTypes

import scala.collection.mutable
import scala.util.parsing.combinator._

case class Grammar(nonTerminals: List[NonTerminal])

case class NonTerminal(name: String, productions: List[Production])

sealed trait Production
case class NonTerminalProduction(name: String) extends Production
case class CompoundProduction(elements: Element) extends Production

sealed trait Element
case class NonTerminalNameElement(name: String)
case class OptionalElement(e: Option[Element])


object Main {
  def main(): Unit = {
    // read all of stdin
    val input: String = io.Source.stdin.getLines().mkString // TODO: find a better way to do this
    // parse using scala parsers
    Grammar.main(input)
    val extend = mutable.HashMap[String, Set[String]]()
    val grammar: Grammar = ???
    for (nt <- grammar.nonTerminals) {
      nt.productions match {
        case List(CompoundProduction(elements)) =>
          print(f"case class ${nt.name}(")
          // loop over elements
          print(")")
          print(" extends ")
          for (e <- extend(nt.name)) {
            print(f" ${e}, ")
          }
          println()
          /*
        case ps | ps.all(_.isInstanceOf[NonTerminalProduction]) =>
          printnl(f"sealed trait ${nt.name}")
          for (p <- productions) {
            extend += nt.name => extend(nt.name).add(p)
          }
          */
      }
    }
    // print to stdout
  }
}

object Grammar extends RegexParsers {
  override val skipWhitespace = false
  private val eol = sys.props("line.separator")

  def nonTerminalName: Parser[String] = """[A-Z][a-zA-Z]*""".r ^^ { _.toString }
//  def nonTerminal: Parser[NonTerminal] =
//    nonTerminalName ~ (":\n" ~> production.*) ^^ { case name ~ productions => NonTerminal(name, productions) }

  def nonTerminal: Parser[NonTerminal] =
    nonTerminalName ~ ":" ~ eol ^^ { case name ~ _ ~ _ => NonTerminal(name, List()) }

  def production: Parser[Production] = nonTerminalProduction
  def nonTerminalProduction : Parser[Production] = "  " ~> nonTerminalName <~ "\n" ^^ { name => NonTerminalProduction(name) }

  def main(input: String) {
    val result = Grammar.parse(Grammar.nonTerminal, input)
    result match {
      case Success(matched, next) => println("success"); println(matched); println("next:" + next)
      case Failure(msg, _) => println(f"failure ${msg}")
      case Error(msg, _) => println("error")
    }
  }
}