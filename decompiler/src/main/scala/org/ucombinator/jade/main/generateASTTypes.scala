package org.ucombinator.jade.main.generateASTTypes

import scala.collection.mutable
import scala.util.parsing.combinator._


case class Grammar(nonTerminals: List[NonTerminal])

sealed trait Production
trait CompoundProduction extends Production

case class NonTerminal(nonTerminal: String, productions: List[Production]) extends CompoundProduction
case class Terminal(terminal: String) extends CompoundProduction
case class Sequence(elements: List[CompoundProduction]) extends CompoundProduction
case class Repeat(prod: CompoundProduction) extends CompoundProduction
case class Optional(prod: CompoundProduction) extends CompoundProduction
case class OneOf(prod:Production) extends CompoundProduction

object Main {
  def main(fileName: String): Unit = {
    require(fileName != null, "the given class file name is actually `null`!")

    val source = scala.io.Source.fromFile(fileName)
    val input = try source.mkString finally source.close()

    val grammar: Option[List[NonTerminal]] = Grammar.main(input)
    grammar match{
      case Some(nonTerminals) => convertGrammarToAST(nonTerminals)
      case _ => println("Could not parse grammar")
    }
  }

  def convertGrammarToAST(nonTerminals: List[NonTerminal]) : Unit = {
    val extend = mutable.HashMap[String, Set[String]]()

//    for (nonTerminal <- nonTerminals) {
//      nonTerminal.productions match {
//        case List =>
//          print(f"case class ${nt.name}(")
//          // loop over elements
//          print(")")
//          print(" extends ")
//          for (e <- extend(nt.name)) {
//            print(f" ${e}, ")
//          }
//          println()
//        /*
//      case ps | ps.all(_.isInstanceOf[NonTerminalProduction]) =>
//        printnl(f"sealed trait ${nt.name}")
//        for (p <- productions) {
//          extend += nt.name => extend(nt.name).add(p)
//        }
//        */
//      }
//    }
  }

//  def containsOnlyNonTerminalProduction(productions: List[Production]) : Boolean = {
//
//  }
}

object Grammar extends RegexParsers
{
  override val skipWhitespace = false
  private val eol = sys.props("line.separator")

  def nonTerminalName: Parser[String] = """[A-Z][a-zA-Z]*""".r ^^ { _.toString }
  def terminalName: Parser[String] = """[a-z.*;,=][a-zA-Z]*""".r ^^ { _.toString }
  def startTerminalSymbols : Parser[String] = """[<{(]""".r ^^ { _.toString }
  def endTerminalSymbols : Parser[String] = """[>})]""".r ^^ { _.toString }

  def nonTerminal: Parser[NonTerminal] =
    nonTerminalName ~ (":" ~ eol ~> repsep(production, eol)) ^^ { case name ~ productions => NonTerminal(name, productions) }

  def nonTerminalProduction:Parser[NonTerminal] =
    nonTerminalName ^^ { name => NonTerminal(name, List()) }

  def terminalProduction: Parser[Terminal] =
    (terminalName | startTerminalSymbols | endTerminalSymbols) ^^ { name => Terminal(name)}

  def repetitionProduction: Parser[Repeat] =
    "{" ~ repsep(compoundProduction, " ") ~ "}" ^^ {case  _ ~ compoundProductions ~ _ => Repeat(Sequence(compoundProductions)) }

  def optionalProduction: Parser[Optional] =
    "[" ~ repsep(compoundProduction, " ") ~ "]" ^^ {case  _ ~ compoundProductions ~ _ => Optional(Sequence(compoundProductions)) }

  def compoundProduction : Parser[CompoundProduction] =
    (nonTerminalProduction | repetitionProduction | optionalProduction | terminalProduction) ^^ { compoundProduction => compoundProduction}

  def oneOfProduction: Parser[CompoundProduction] =
    "  (one of)" ~ (eol ~> normalProduction) ^^ {case _ ~ compoundProductions => OneOf(compoundProductions)}

  def normalProduction : Parser[Production] =
    "  " ~> repsep(compoundProduction, " ") ^^ { compoundProductions => Sequence(compoundProductions) }

  def production : Parser[Production] = (oneOfProduction | normalProduction) ^^ { production => production}

  def nonTerminalList : Parser[List[NonTerminal]] = repsep(nonTerminal, eol ~ eol) ^^ { nonTerminals => nonTerminals }


  def main(input: String) : Option[List[NonTerminal]] = {
    val result : ParseResult[List[NonTerminal]] = Grammar.parse(Grammar.nonTerminalList, input)
    result match {
      case Success(matched, _) => println("success"); Some(matched)
      case Failure(msg, _) => println(f"failure $msg"); None
      case Error(msg, _) => println(f"error $msg"); None
    }
  }
}